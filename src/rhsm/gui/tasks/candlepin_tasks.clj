(ns rhsm.gui.tasks.candlepin-tasks
  (:use [rhsm.gui.tasks.test-config :only (config
                                           clientcmd)]
        [clojure.string :only (trim
                               blank?
                               split)]
        rhsm.gui.tasks.tools)
  (:require [rhsm.gui.tasks.rest :as rest])
  (:import [com.redhat.qe.tools RemoteFileTasks]
           [rhsm.cli.tasks CandlepinTasks]
           [rhsm.base SubscriptionManagerBaseTestScript]))

(defn is-true-value? [value]
  (let [val (clojure.string/lower-case value)]
    (cond
     (= "1" val) true
     (= "true" val) true
     (= "yes" val) true
     :else false)))

(defn merge-zipmap
  "Returns a map with the keys mapped to the corresponding vals.
   This merges repeated vals for the same key."
  [keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks)
                      (if (get map (first ks))
                        (vec (distinct
                              (flatten
                               (conj (get map (first ks))
                                     (first vs)))))
                        (first vs)))
               (next ks)
               (next vs))
        map)))

(defn server-url
  "Returns the server url as used by the automation. As used by API calls."
  []
  (SubscriptionManagerBaseTestScript/sm_serverUrl))

(defn server-path
  "Returns the full server path as used by the register dialog."
  []
  (let [hostname (SubscriptionManagerBaseTestScript/sm_serverHostname)
        port (SubscriptionManagerBaseTestScript/sm_serverPort)
        prefix(SubscriptionManagerBaseTestScript/sm_serverPrefix)]
    (str hostname (if-not (blank? port) (str ":" port))  prefix)))

(defn get-rhsm-service-version
  "Returns the version and realease of the candlepin server."
  []
  (let [status (rest/get (str (server-url) "/status")
                         (@config :username)
                         (@config :password))]
    (str (:version status) "-" (:release status))))

; Not a candlepin task, but sticking this here.
(defn get-consumer-id
  "Returns the consumer id if registered."
  []
  (let [identity
        (trim
         (:stdout
          (run-command
           "subscription-manager identity | grep identity | cut -f 2 -d :")))]
    (if (= identity "")
      nil
      identity)))

(defn get-consumer-owner-key
  "Looks up the consumer's owner by consumer-id."
  ([consumerid]
     (:key (rest/get
            (str (server-url) "/consumers/" consumerid "/owner")
            (@config :username)
            (@config :password))))
  ([]
     (get-consumer-owner-key (get-consumer-id))))

(defn list-available
  "Gets a list of all available pools for a given owner and consumer."
  ([owner consumerid & {:keys [all?]
                        :or {all? false}}]
     (rest/get (str (server-url)
                    "/owners/" owner
                    "/pools?consumer=" consumerid
                    (if all? "&listall=true"))
               (@config :username)
               (@config :password)))
  ([all?]
     (list-available
      (get-consumer-owner-key)
      (get-consumer-id)
      :all? all?))
  ([]
     (list-available
      (get-consumer-owner-key)
      (get-consumer-id))))

(defn build-product-map
  [& {:keys [all?]
      :or {all? false}}]
  (let [everything (if all?
                     (list-available true)
                     (list-available))
        productlist (atom {})]
    (doseq [s everything]
      (doseq [p (:providedProducts s)]
        (if (nil? (@productlist (:productName p)))
          (reset! productlist
                  (assoc @productlist
                    (:productName p)
                    [(:productName s)]))
          (reset! productlist
                  (assoc @productlist
                    (:productName p)
                    (into (@productlist (:productName p)) [(:productName s)]))))))
    @productlist))

(defn build-subscription-product-map
  [& {:keys [all?]
      :or {all? false}}]
  (let [everything (if all?
                     (list-available true)
                     (list-available))
        raw (map (fn [s] (list (:productName s)
                              (vec (map (fn [p] (:productName p))
                                        (:providedProducts s)))))
                 everything)
        subs (map first raw)
        prods (map second raw)]
    (merge-zipmap subs prods)))

(defn build-contract-map
  "Builds a mapping of subscription names to their contract number"
  [& {:keys [all?]
      :or {all? false}}]
  (let [x (map #(list (:productName %) (:contractNumber %))
               (if all? (list-available true) (list-available false)))
        y (group-by first x)]
    (zipmap (keys y) (map #(map second %) (vals y)))))

(defn build-service-map
  "Builds a mapping of subscriptions to service levels"
  [& {:keys [all?]
      :or {all? false}}]
  (let [fil (fn [pool]
              (into {}
                    (map (fn [i] {(keyword (:name i)) (:value i)})
                         (filter (fn [attr]
                                   (if (or (= "support_type" (:name attr))
                                           (= "support_level" (:name attr)))
                                     true
                                     false))
                                 (:productAttributes pool)))))
        x (map #(list (:productName %) (fil %))
               (if all? (list-available true) (list-available false)))
        y (group-by first x)
        z (zipmap (keys y) (map #(map second %) (vals y)))]
    (zipmap (keys z) (map first (vals z)))))

(defn build-virt-type-map
  "Builds a map of subscriptions to virt type"
  [& {:keys [all?]
      :or {all? false}}]
  (let [virt-pool? (fn [p]
                     (some #(and (= "virt_only" (:name %))
                                 (is-true-value? (:value %)))
                           p))
        virt-type (fn [p] (if (virt-pool? p) "Virtual" "Physical"))
        itemize (fn [p] (list (:productName p) {(:contractNumber p) (virt-type (:attributes p))}))
        x (map itemize (if all? (list-available true) (list-available false)))
        y (group-by first x)
        overall-status (fn [m] (cond
                                (every? #(= "Physical" %) (flatten (map vals m)))
                                (merge m {:overall "Physical"})
                                (every? #(= "Virtual" %) (flatten (map vals m)))
                                (merge m {:overall "Virtual"})
                               :else (merge m {:overall "Both"})))
        extractor (fn [c] (map last (last c)))
        map-vals (map extractor y)
        map-keys (map first y)
        ;reduced-map (map overall-status muhvals)
        z (zipmap map-keys (map overall-status map-vals))]
       ;(zipmap (keys y) (map #(overall-status (reduce merge (map second %))) (vals y)))
       z))

(defn get-owners
  "Given a username and password, this function returns a list
  of owners associated with that user"
  [username password]
  (seq (CandlepinTasks/getOrgsKeyValueForUser username
                                              password
                                              (server-url)
                                              "displayName")))

(defn get-owner-display-name
  "Given a owner key (org key) this returns the owner's display name"
  [username password orgkey]
  (if orgkey
    (CandlepinTasks/getOrgDisplayNameForOrgKey  username
                                                password
                                                (server-url)
                                                orgkey)))

(defn get-pool-id
  "Get the pool ID for a given subscription/contract pair."
  [username password orgkey subscription contract]
  (CandlepinTasks/getPoolIdFromProductNameAndContractNumber username
                                                            password
                                                            (server-url)
                                                            orgkey
                                                            subscription
                                                            contract))
(defn multi-entitlement?
  "Returns true if the subscription can be entitled to multiple times."
  [username password pool]
  (CandlepinTasks/isPoolProductMultiEntitlement username
                                                password
                                                (server-url)
                                                pool))

(defn get-pool-attributes
  "Returns a combined mapping of subscripton and product attributes in a pool."
  [username password pool]
  (let [attr  (rest/get
               (str (server-url) "/pools/" pool)
               (@config :username)
               (@config :password))
        pattr (:productAttributes attr)
        x (map #(list (:name %) (:value %)) pattr)
        y (group-by first x)
        ykeys (map #(keyword %) (keys y))
        yvals (let [v (map #(map second %) (vals y))]
                (map first v))
        z (zipmap ykeys yvals)
        attr (merge attr z)]
    attr))

(defn get-instance-multiplier
  "Returns the instance multiplier on the pool."
  [username password pool & {:keys [string?]
                             :or {string? false}}]
  (let [attr (get-pool-attributes username password pool)
        multiplier (:instance_multiplier attr)
        settype (if string?
                  (fn [x] (str x))
                  (fn [x] (Integer/parseInt x)))]
    (if multiplier
      (settype multiplier)
      (settype "1"))))

(defn get-quantity-available
  "Returns the quantity available on a pool."
  [username password pool]
  (let [attr (get-pool-attributes username password pool)
        quantity (Integer. (:quantity attr))
        consumed (Integer. (:consumed attr))]
    (if (>= quantity 0)
      (- quantity consumed)
      quantity)))

(defn build-stacking-id-map
  "Builds a map of provides product and product attributes"
  [& {:keys [all?]
      :or {all? false}}]
  (let [listall (if all? (list-available true)
                    (list-available))
        provides-prods (fn [i]  (map :productName (:providedProducts i)))
        prod-attrs (fn [i] (:productAttributes i))
        mapify (map (fn [i] {:pp (map :productName (:providedProducts i)) :attrs (:productAttributes i)}) listall)]
    mapify))

(defn build-subscription-attr-type-map
  "Buils a map of subscription name and product attribute :name map"
  [& {:keys [all?]
      :or {all? false}}]
  (let [listall (if all? (list-available true)
                    (list-available))
        product-names (map :productName listall)
        get-type (fn [i] (map :name i))
        name-types (map get-type (map :productAttributes listall))
        mapify (merge-zipmap product-names name-types)]
    mapify))

(defn build-subscription-dates-map
  "Builds a map of suscripton name and start & end dates map"
  [& {:keys [all?]
      :or {all? false}}]
  (let [listall (if all? (list-available true)
                    (list-available))
        product-names (map :productName listall)
        get-dates (fn [i] [(first (split (:startDate i) #"T"))
                          (first (split (:endDate i) #"T"))])
        date-list (map get-dates listall)
        mapify (merge-zipmap product-names date-list)]
    mapify))

(defn build-subscriptions-name-val-map
  "Builds a map of subscriptions name having the same stacking value."
  [& {:keys [all?]
      :or {all? false}}]
  (let [listall (if all? (list-available true)
                    (list-available))
        product-names (map :productName listall)
        product-attr (map :productAttributes listall)
        get-type-value (fn [i] {(:name i) (:value i)})
        type-val-map (map (fn [i] (into {} (map get-type-value i))) product-attr)
        mapify (zipmap product-names type-val-map)]
    mapify))
