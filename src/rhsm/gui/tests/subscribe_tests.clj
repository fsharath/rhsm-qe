(ns rhsm.gui.tests.subscribe_tests
  (:use [test-clj.testng :only (gen-class-testng
                                data-driven)]
        [rhsm.gui.tasks.test-config :only (config
                                           clientcmd)]
        [com.redhat.qe.verify :only (verify)]
        [slingshot.slingshot :only (try+
                                    throw+)]
        [clojure.string :only (split
                               blank?
                               join
                               trim-newline
                               trim)]
        rhsm.gui.tasks.tools
        gnome.ldtp)
  (:require [clojure.tools.logging :as log]
            [rhsm.gui.tasks.tasks :as tasks]
            [rhsm.gui.tests.base :as base]
            [rhsm.gui.tasks.candlepin-tasks :as ctasks]
             rhsm.gui.tasks.ui)
  (:import [org.testng.annotations
            BeforeClass
            BeforeGroups
            AfterGroups
            Test
            DataProvider]
            org.testng.SkipException))

(def productlist (atom {}))
(def servicelist (atom {}))
(def contractlist (atom {}))
(def subs-contractlist (atom {}))
(def prod-dir (atom {}))
(def sys-log "/var/log/rhsm/rhsm.log")
(def ns-log "rhsm.gui.tests.subscribe_tests")

(defn build-subscription-map
  "Builds the product map and updates the productlist atom"
  []
  (reset! productlist (ctasks/build-product-map :all? true))
  @productlist)

(defn build-contract-map
  "Builds the contract/virt-type map and updates the cont"
  []
  (reset! contractlist (ctasks/build-virt-type-map :all? true))
  @contractlist)

(defn allsearch
  ([filter]
     (tasks/search :match-system? false
                   :do-not-overlap? false
                   :contain-text filter))
  ([] (allsearch nil)))

(defn ^{BeforeClass {:groups ["setup"]}}
  register [_]
  (try
    (if (= "RHEL7" (get-release)) (base/startup nil))
    (tasks/unsubscribe_all)
    (tasks/register-with-creds)
    (reset! servicelist (ctasks/build-service-map :all? true))
    (reset! subs-contractlist (ctasks/build-subscription-product-map :all? true))
    (build-contract-map)
    (catch Exception e
      (reset! (skip-groups :subscribe) true)
      (throw e))))

(defn ^{Test {:groups ["subscribe"
                       "acceptance"
                       "tier1"]
              :dataProvider "subscriptions"
              :priority (int 100)}}
  subscribe_each
  "Asserts that each subscripton can be subscribed to sucessfully."
  [_ subscription]
  (try+
   ;; The below condition is to avoid gui-freeze in RHEL7 when generate
   ;; keyevent attemps to update quanity when quantiy is 0
   (if-not (and (= "RHEL7" (get-release))
                (= "0 *" (tasks/ui getcellvalue :all-subscriptions-view
                                   (tasks/skip-dropdown :all-subscriptions-view
                                                        subscription) 3)))
     (tasks/subscribe subscription))
   (catch [:type :item-not-available] _)
   (catch [:type :error-getting-subscription] _)
   (catch [:type :wrong-consumer-type]
       {:keys [log-warning]} (log-warning))))

(defn ^{Test {:groups ["subscribe"
                       "acceptance"
                       "tier1"]
              :dataProvider "subscribed"
              :dependsOnMethods ["subscribe_each"]}}
  unsubscribe_each
  "Asserts that each subscripton can be unsubscribed from sucessfully."
  [_ subscription]
  (tasks/ui selecttab :my-subscriptions)
  (try+ (tasks/unsubscribe subscription)
        (tasks/checkforerror)
        (sleep 4000)
        (verify (= false (tasks/ui rowexist? :my-subscriptions-view subscription)))
        (catch [:type :not-subscribed] _)))

(defn ^{Test {:groups ["subscribe"
                       "tier2"
                       "blockedByBug-918617"]
              :dependsOnMethods ["unsubscribe_each"]}}
  subscribe_check_syslog
  "Asserts that subscribe events are logged in the syslog."
  [_]
  (allsearch)
  (try+
   (let [subscription (rand-nth (tasks/get-table-elements :all-subscriptions-view 0))
         output (get-logging @clientcmd
                             sys-log
                             "subscribe_check_syslog"
                             nil
                             (tasks/subscribe subscription))]
     (verify (not (blank? output))))
   (catch [:type :error-getting-subscription] _))
  (sleep 5000))

(defn ^{Test {:groups ["subscribe"
                       "tier2"
                       "blockedByBug-918617"]
              :dependsOnMethods ["subscribe_check_syslog"]}}
  unsubscribe_check_syslog
  "Asserts that unsubscribe events are logged in the syslog."
  [_]
  (let [subscription (rand-nth (tasks/get-table-elements :my-subscriptions-view 0))
        output (get-logging @clientcmd
                            sys-log
                            "unsubscribe_check_syslog"
                            nil
                            (tasks/unsubscribe subscription))]
    (verify (not (blank? output)))))

(defn ^{Test {:groups ["subscribe"
                       "tier3"
                       "blockedByBug-703920"
                       "blockedByBug-869028"]
              :dataProvider "multi-contract"}}
  check_contract_selection_dates
  "Asserts that the dates in the contract selection dialog are displayed correctly."
  [_ subscription]
  (try+ (tasks/open-contract-selection subscription)
        (try
          (loop [row (- (tasks/ui getrowcount :contract-selection-table) 1)]
            (if (>= row 0)
              (let [startdate (tasks/ui getcellvalue :contract-selection-table row 3)
                    enddate (tasks/ui getcellvalue :contract-selection-table row 4)
                    datex (get-locale-regex)]
                (verify (not (nil? (re-matches datex startdate))))
                (verify (not (nil? (re-matches datex enddate))))
                (recur (dec row)))))
          (finally
            (tasks/ui click :cancel-contract-selection)))
        (catch [:type :item-not-available] _)
        (catch [:type :error-getting-subscription] _)
        (catch [:type :wrong-consumer-type]
            {:keys [log-warning]} (log-warning))))

  ;; https://bugzilla.redhat.com/show_bug.cgi?id=723248#c3
(defn ^{Test {:groups ["subscribe"
                       "tier3"
                       "blockedByBug-766778"
                       "blockedByBug-723248"
                       "blockedByBug-855257"
                       "blockedByBug-962905"]
              :dataProvider "subscriptions"}}
  check_quantity_scroller
  "Tests the quantity scroller assiciated with subscriptions."
  [_ subscription]
  (if (= "RHEL7" (get-release))
    (throw (SkipException.
            (str "Cannot generate keyevents in RHEL7 !!
                  Skipping Test 'check_quantity_scroller'."))))
  (try+
    (tasks/open-contract-selection subscription)
    (loop [row (- (tasks/ui getrowcount :contract-selection-table) 1)]
      (if (>= row 0)
        (let [contract (tasks/ui getcellvalue :contract-selection-table row 0)
              pool (ctasks/get-pool-id (@config :username)
                                       (@config :password)
                                       (@config :owner-key)
                                       subscription
                                       contract)
              multiplier (ctasks/get-instance-multiplier (@config :username)
                                                         (@config :password)
                                                         pool
                                                         :string? false)
              usedmax (tasks/ui getcellvalue :contract-selection-table row 2)
              default (first (split (tasks/ui getcellvalue :contract-selection-table row 5) #"\s"))
              used (first (split usedmax #" / "))
              unused (last (split usedmax #" / "))
              max (if (= "Unlimited" unused) (rand-int 50) unused)
              available (- (Integer. max) (Integer. used))
              enter-quantity (fn [num]
                               (tasks/ui generatekeyevent
                                         (str (repeat-cmd 5 "<right> ")
                                              "<space>"
                                              (when num (str " "
                                                             (- num (mod num multiplier))
                                                             " <enter>")))))
              get-quantity (fn []
                             (first
                              (split (tasks/ui getcellvalue :contract-selection-table row 5) #"\s")))
              get-quantity-int (fn [] (Integer. (get-quantity)))]
          ;verify that the default quantity is sane for BZ: 855257
          (verify (<= (Integer. default) (- (Integer. max) (Integer. used))))
          (if (ctasks/multi-entitlement? (@config :username) (@config :password) pool)
            (do
              ;verify that the quantity can be changed
              (tasks/ui selectrowindex :contract-selection-table row)
              (enter-quantity available)
              (let [quantity (- available (mod available multiplier))]
                (verify (= quantity
                           (get-quantity-int))))
              ;verify that the quantity cannot exceed the max
              (enter-quantity (+ 1 available))
              (verify (>= available
                          (get-quantity-int)))
              ;verify that the quantity cannot exceed the min
              (enter-quantity -1)
              (verify (<= 0 (get-quantity-int)))
              ;verify max and min values for scroller
              (enter-quantity nil)
              (tasks/ui generatekeyevent (str
                                          (repeat-cmd (+ 2 (Integer. max)) "<up> ")
                                          "<enter>"))
              (verify (>= (Integer. max) (get-quantity-int)))
              (enter-quantity nil)
              (tasks/ui generatekeyevent (str
                                          (repeat-cmd (+ 2 (Integer. max)) "<down> ")
                                          "<enter>"))
              (verify (<= 0 (get-quantity-int))))
            (do
              ;verify that the quantity cannot be changed
              (tasks/ui selectrowindex :contract-selection-table row)
              (enter-quantity (Integer. max))
              (verify (= default (get-quantity)))))
          (recur (dec row)))))
    (catch [:type :subscription-not-available] _)
    (catch [:type :error-getting-subscription] _)
    (catch [:type :wrong-consumer-type]
        {:keys [log-warning]} (log-warning))
    (catch [:type :contract-selection-not-available] _)
    (finally (if (tasks/ui showing? :contract-selection-table)
               (tasks/ui click :cancel-contract-selection)))))

;; https://bugzilla.redhat.com/show_bug.cgi?id=723248#c3
(defn ^{Test {:groups ["subscribe"
                       "tier3"
                       "blockedByBug-723248"
                       "blockedByBug-962905"]
              :dataProvider "multi-entitle"
              :priority (int 500)}}
  check_quantity_subscribe
  "Asserts that the selected quantity is given when subscribed to."
  [_ subscription contract]
  (if (= "RHEL7" (get-release))
    (throw (SkipException.
            (str "Cannot generate keyevents in RHEL7 !!
                  Skipping Test 'check_quantity_subscribe'."))))
  (try+
   (run-command "subscription-manager unsubscribe --all")
   (sleep 3000)
   (tasks/search)
   (let [user (@config :username)
         pass (@config :password)
         pool (ctasks/get-pool-id user
                                  pass
                                  (@config :owner-key)
                                  subscription
                                  contract)
         available (ctasks/get-quantity-available user pass pool)
         multiplier (ctasks/get-instance-multiplier user pass pool)
         requested (- available (mod available multiplier))]
     (tasks/subscribe subscription contract requested)
     (tasks/ui selecttab :my-subscriptions)
      (let [row (tasks/ui gettablerowindex :my-subscriptions-view subscription)
            count (Integer. (tasks/ui getcellvalue :my-subscriptions-view row 3))]
        (verify (= count requested))))
    (tasks/unsubscribe subscription)
    (catch [:type :item-not-available] _)
    (catch [:type :wrong-consumer-type]
        {:keys [log-warning]} (log-warning))
    (catch [:type :contract-selection-not-available] _)
    (catch [:type :error-getting-subscription] _)))

(defn ^{Test {:groups ["subscribe"
                       "tier3"
                       "blockedByBug-755861"
                       "blockedByBug-962905"]
              :dataProvider "multi-entitle"
              :dependsOnMethods ["check_quantity_subscribe"]}}
  check_quantity_subscribe_traceback
  "Asserts no traceback is shown when subscribing in quantity."
  [_ subscription contract]
  (if (= "RHEL7" (get-release))
    (throw (SkipException.
            (str "Cannot generate keyevents in RHEL7 !!
                  Skipping Test 'check_quantity_subscribe_traceback'."))))
  (let [ldtpd-log "/var/log/ldtpd/ldtpd.log"
        output (get-logging @clientcmd
                                  ldtpd-log
                                  "multi-subscribe-tracebacks"
                                  nil
                                  (check_quantity_subscribe nil subscription contract))]
    (verify (not (substring? "Traceback" output)))))

  ;; TODO: this is not in rhel6.3 branch, finish when that is released
  ;; https://bugzilla.redhat.com/show_bug.cgi?id=801434

(comment
  ;; TODO:
(defn ^{Test {:groups ["subscribe" "blockedByBug-740831"]}}
  check_subscribe_greyout [_]
  ))

;;https://tcms.engineering.redhat.com/case/77359/?from_plan=2110
(defn ^{Test {:groups ["subscribe"
                       "acceptance"
                       "tier1"
                       "blockedByBug-874624"
                       "blockedByBug-753057"]
              :dataProvider "subscriptions"}}
  check_contracts_and_virt_type
  "Asserts that the contract number and virt type of each subscription is displayed properly"
  [_ subscription]
  (tasks/ui selecttab :all-available-subscriptions)
  (let [row (tasks/skip-dropdown :all-subscriptions-view subscription)
        overall-stat (fn [a] (get a :overall))
        vector-map (vec (get @contractlist subscription))
        cli-stat (some #(if-not (nil? %) %) (map overall-stat vector-map))
        overall-type (tasks/ui getcellvalue :all-subscriptions-view row 1)]
    ;(verify (= overall-type (:overall (get @contractlist subscription))))
    (verify (= overall-type cli-stat)))
  (try+
   ;; The below condition is to avoid gui-freeze in RHEL7 when generate
   ;; keyevent attemps to update quanity when quantiy is 0
   (if-not (and (= "RHEL7" (get-release))
                (= "0 *" (tasks/ui getcellvalue :all-subscriptions-view
                                   (tasks/skip-dropdown :all-subscriptions-view
                                                        subscription) 3)))
     (do
       (tasks/open-contract-selection subscription)
       (tasks/do-to-all-rows-in
        :contract-selection-table
        0
        (fn [contract]
          (verify (not-nil? (some #{contract}
                                  ;(keys (get @contractlist subscription))
                                  (flatten (map #(map key %)
                                                (vec (get @contractlist subscription)))))))
          ;; old verification
          (comment
            (verify (= (tasks/ui getcellvalue :contract-selection-table
                                 (tasks/ui gettablerowindex :contract-selection-table contract)1)
                       (get (get @contractlist subscription) contract))))

          (verify (not-nil? (some #{(tasks/ui getcellvalue :contract-selection-table
                                              (tasks/ui gettablerowindex
                                                        :contract-selection-table contract)1)}
                                  (flatten (map #(get % contract)
                                                (vec (get @contractlist subscription)))))))))))
   (catch [:type :contract-selection-not-available] _)
    (catch [:type :error-getting-subscription] _)
    (finally (if (tasks/ui showing? :contract-selection-table)
               (tasks/ui click :cancel-contract-selection)))))

(defn ^{Test {:groups ["subscribe"
                       "acceptance"
                       "tier1"
                       "blockedByBug-877579"]
              :dataProvider "unlimited-pools"}}
  check_unlimited_quantities
  "Tests that unlimted pools are displayed properly"
  [_ subscription contract]
  (let [row (tasks/skip-dropdown :all-subscriptions-view subscription)
        quantity (tasks/ui getcellvalue :all-subscriptions-view row 2)]
    (verify (= "Unlimited" quantity)))
  (try+
   (tasks/open-contract-selection subscription)
   (let [row (tasks/ui gettablerowindex :contract-selection-table contract)
         total (last (split (tasks/ui getcellvalue :contract-selection-table row 2) #" / "))
         quantity (first (split (tasks/ui getcellvalue :contract-selection-table row 5) #" "))]
     (verify (= "Unlimited" total))
     (verify (<= 0 (Integer. quantity))))
   (catch [:type :contract-selection-not-available] _)
   (finally (if (tasks/ui showing? :contract-selection-table)
              (tasks/ui click :cancel-contract-selection)))))

(defn ^{Test {:group ["subscribe"
                      "tier2"
                      "blockedByBug-951633"]
              :priority (int 300)}}
  product_with_comma_separated_arch
  "This is to assert products with comma seperated products when subscribed are fully subscribed"
  [_]
  (tasks/do-to-all-rows-in
   :installed-view 0
   (fn [subscription]
     (let [index (tasks/skip-dropdown :installed-view subscription)
           sub-arch (tasks/ui gettextvalue :arch)
           machine-arch (trim-newline (:stdout (run-command "uname -m")))]
       (if (and (substring? "," (tasks/ui gettextvalue :arch))
                (or (substring? sub-arch machine-arch) substring? machine-arch sub-arch()))
         (verify ( = "Subscribed" (tasks/ui getcellvalue :installed-view index 2))))))))

(defn ^{Test {:group ["subscribe"
                      "tier2"
                      "blockedByBug-950672"
                      "blockedByBug-988411"]
              :dependsOnMethods ["product_with_comma_separated_arch"]}}
  check_subscription_in_subscribed_products
  "Asserts there is a valid subscription value for all Subscribed products"
  [_]
  (tasks/do-to-all-rows-in
   :installed-view 0
   (fn [subscription]
     (tasks/skip-dropdown :installed-view subscription)
     (if (not (= "Not Subscribed" (tasks/ui gettextvalue :certificate-status)))
       (verify (not (blank? (tasks/ui gettextvalue :providing-subscriptions))))))))

(defn ^{Test {:group ["subscribe"
                      "tier2"
                      "blockedByBug-909467"
                      "blockedByBug-988411"]
              :dependsOnMethods ["check_subscription_in_subscribed_products"]}}
  check_subscription_compliance
  "Checks for status of subscriptions when archs dont match that of the system"
  [_]
  (let [machine-arch (trim-newline (:stdout (run-command "uname -m")))]
    (tasks/do-to-all-rows-in
     :installed-view 0
     (fn [subscription]
       (tasks/skip-dropdown :installed-view subscription)
       (let
           [sub-name (tasks/ui gettextvalue :providing-subscriptions)
            sub-arch (tasks/ui gettextvalue :arch)
            check-arch (or (substring? sub-arch machine-arch)
                           (substring? machine-arch sub-arch))
            arch-all (= "ALL" (tasks/ui gettextvalue :arch))
            status (not (= "Not Subscribed" (tasks/ui gettextvalue :certificate-status)))]
         (if (and (not (or check-arch arch-all)) status)
           (do
             (tasks/ui selecttab :my-subscriptions)
             (tasks/skip-dropdown :my-subscriptions-view sub-name)
             (verify (substring? (str "Supports architecture " sub-arch " but the system is " machine-arch)
                                 (tasks/ui gettextvalue :status-details-text)))
             (tasks/ui selecttab :my-installed-products))))))))

(defn ^{Test {:groups ["subscribe"
                       "tier3"
                       "blockedByBug-962933"]
              :dataProvider "subscriptions"}}
  check_multiplier_logic
  "Assert instance multiplier logic does not apply to Virtual machines"
  [_ subscription]
  (if (= "RHEL7" (get-release))
    (throw (SkipException.
            (str "Cannot generate keyevents in RHEL7 !!
                  Skipping Test 'check_multiplier_logic'."))))
  (let [cli-out (:stdout (run-command "subscription-manager facts --list | grep \"virt.is_guest\""))
        client-type-virt? (= "true" (.toLowerCase (trim (last (split (trim-newline cli-out) #":")))))]
    (if client-type-virt?
      (do
        (try+
         (tasks/skip-dropdown  :all-subscriptions-view subscription)
         (tasks/open-contract-selection subscription)
         (loop [row (- (tasks/ui getrowcount :contract-selection-table) 1)]
           (if (>= row 0)
             (let [contract (tasks/ui getcellvalue :contract-selection-table row 0)
                   pool (ctasks/get-pool-id (@config :username)
                                            (@config :password)
                                            (@config :owner-key)
                                            subscription
                                            contract)
                   multiplier (ctasks/get-instance-multiplier (@config :username)
                                                              (@config :password)
                                                              pool
                                                              :string? false)]
               (if (> multiplier 1)
                 (do
                   (tasks/ui selectrowindex :contract-selection-table row)
                   (let [quantity-before
                         (Integer. (re-find #"\d+"
                                            (tasks/ui getcellvalue :contract-selection-table row 5)))
                         action (tasks/ui generatekeyevent
                                          (str (repeat-cmd 5 "<right> ")
                                               "<space> " "<up> " "<enter>"))
                         quantity-after
                         (Integer. (re-find #"\d+"
                                            (tasks/ui getcellvalue :contract-selection-table row 5)))]
                     (verify (not (= multiplier (- quantity-after quantity-before))))))))))
         (catch [:type :error-getting-subscription] _)
         (catch [:type :contract-selection-not-available] _)
         (finally
           (if (tasks/ui showing? :contract-selection-table)
             (tasks/ui click :cancel-contract-selection))))))))

(defn ^{Test {:groups ["subscribe"
                       "acceptance"
                       "tier1"
                       "blockedByBug-874624"]
              :dataProvider "subscriptions"}}
  check_contract_number
  "Checks if every subsciption has contract numbers displayed"
  [_ subscription]
  (try+
    (allsearch)
    (let [sub-contract-map (ctasks/build-contract-map :all? true)
          contracts (sort (get sub-contract-map subscription))]
      (tasks/skip-dropdown :all-subscriptions-view subscription)
      (tasks/ui click :attach)
      (tasks/checkforerror)
      (tasks/ui waittillwindowexist :contract-selection-dialog 5)
      (if (bool (tasks/ui guiexist :contract-selection-dialog))
        (verify (= contracts
                   (sort (tasks/get-table-elements :contract-selection-table 0))))
        (do
          (tasks/unsubscribe subscription)
          (tasks/ui selecttab :all-available-subscriptions)
          (verify (= 1 (count contracts))))))
    (catch [:type :error-getting-subscription] _)
    (finally
      (if (bool (tasks/ui guiexist :contract-selection-dialog))
        (tasks/ui click :cancel-contract-selection)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA PROVIDERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; you can test data providers in a REPL the following way:
;; (doseq [s (stest/get_subscriptions nil :debug true)]
;;   (stest/subscribe_each nil (peek s)))

(defn ^{DataProvider {:name "multi-entitle"}}
  get_multi_entitle_subscriptions [_ & {:keys [debug]
                                        :or {debug false}}]
  (log/info (str "======= Starting DataProvider: "
                 ns-log "get_multi_entitle_subscriptions()"))
  (try
    (if-not (assert-skip :subscribe)
      (do
        (tasks/restart-app)
        (register nil)
        (tasks/search)
        (let [subs (atom [])
              subscriptions (tasks/get-table-elements
                             :all-subscriptions-view
                             0
                             :skip-dropdowns? true)]
          (doseq [s subscriptions]
            (try+
             (tasks/open-contract-selection s)
             (loop [row (- (tasks/ui getrowcount :contract-selection-table) 1)]
               (if (>= row 0)
                 (let [contract (tasks/ui getcellvalue :contract-selection-table row 0)
                       pool (ctasks/get-pool-id (@config :username)
                                                (@config :password)
                                                (@config :owner-key)
                                                s
                                                contract)]
                   (if (ctasks/multi-entitlement? (@config :username) (@config :password) pool)
                     (swap! subs conj [s contract]))
                   (recur (dec row)))))
             (tasks/ui click :cancel-contract-selection)
             (catch [:type :subscription-not-available] _)
             (catch [:type :item-not-available] _)
             (catch [:type :contract-selection-not-available] _)
             (catch [:type :wrong-consumer-type]
                 {:keys [log-warning]} (log-warning))))
          (if-not debug
            (to-array-2d @subs)
            @subs)))
      (to-array-2d []))
    (catch Exception e
      (if (substring? "NoSuchMethodError" (.getMessage e))
        (throw (SkipException.
                (str "Skipping Test !!!
                      DataProvider 'get_multi_entitle_subscriptions' failed.")))))))

(defn ^{DataProvider {:name "subscriptions"}}
  get_subscriptions [_ & {:keys [debug]
                          :or {debug false}}]
  (log/info (str "======= Starting DataProvider: " ns-log "get_subscriptions()"))
  (if-not (assert-skip :subscribe)
    (do
      (tasks/restart-app)
      (register nil)
      (allsearch)
      (let [subs (into [] (map vector (tasks/get-table-elements
                                       :all-subscriptions-view
                                       0
                                       :skip-dropdowns? true)))]
        (if-not debug
          (to-array-2d subs)
          subs)))
    (to-array-2d [])))

(defn ^{DataProvider {:name "subscribed"}}
  get_subscribed [_ & {:keys [debug]
                       :or {debug false}}]
  (log/info (str "======= Starting DataProvider: " ns-log "get_subscribed()"))
  (if-not (assert-skip :subscribe)
    (do
      (tasks/restart-app)
      (register nil)
      (tasks/ui selecttab :my-subscriptions)
      (tasks/subscribe_all)
      (tasks/ui selecttab :my-subscriptions)
      (let [subs (into [] (map vector (tasks/get-table-elements
                                       :my-subscriptions-view
                                       0
                                       :skip-dropdowns? true)))]
        (if-not debug
          (to-array-2d subs)
          subs)))
    (to-array-2d [])))

(defn ^{DataProvider {:name "multi-contract"}}
  get_multi_contract_subscriptions [_ & {:keys [debug]
                                         :or {debug false}}]
  (log/info (str "======= Starting DataProvider: "
                 ns-log "get_multi_contract_subscriptions()"))
  (try
    (if-not (assert-skip :subscribe)
      (do
        (tasks/restart-app)
        (register nil)
        (tasks/search :do-not-overlap? false)
        (let [subs (atom [])
              allsubs (tasks/get-table-elements :all-subscriptions-view 0 :skip-dropdowns? true)]
          (doseq [s allsubs]
            (try+
             (tasks/open-contract-selection s)
             (tasks/ui click :cancel-contract-selection)
             (swap! subs conj [s])
             (catch [:type :subscription-not-available] _)
             (catch [:type :contract-selection-not-available] _)))
          (if-not debug
            (to-array-2d @subs)
            @subs)))
      (to-array-2d []))
    (catch Exception e
      (if (substring? "NoSuchMethodError" (.getMessage e))
        (throw (SkipException.
                (str "Skipping Test !!!
                      DataProvider 'get_multi_contract_subscriptions' failed.")))))))

(defn ^{DataProvider {:name "installed-products"}}
  get_installed_products [_ & {:keys [debug]
                               :or {debug false}}]
  (log/info (str "======= Starting DataProvider: " ns-log "get_installed_products()"))
  (if-not (assert-skip :subscribe)
    (do
      (tasks/restart-app :reregister? true)
      (let [prods (into [] (map vector (tasks/get-table-elements
                                        :installed-view
                                        0)))]
        (build-subscription-map)
        (tasks/search)
        (if-not debug
          (to-array-2d prods)
          prods)))
    (to-array-2d [])))

(defn ^{DataProvider {:name "unlimited-pools"}}
  get_unlimited_pools [_ & {:keys [debug]
                            :or {debug false}}]
  (log/info (str "======= Starting DataProvider: " ns-log "get_unlimited_pools()"))
  (if-not (assert-skip :subscribe)
    (do
      (run-command "subscription-manager unregister")
      (register nil)
      (let [all (ctasks/list-available true)
            unlimited? (fn [p] (< (:quantity p) 0))
            all-unlimited (filter unlimited? all)
            itemize (fn [p] (vector (:productName p) (:contractNumber p)))
            pools (into [] (map itemize all-unlimited))]
        (allsearch)
        (if-not debug
          (to-array-2d pools)
          pools)))
    (to-array-2d [])))

  ;; TODO: https://bugzilla.redhat.com/show_bug.cgi?id=683550
  ;; TODO: https://bugzilla.redhat.com/show_bug.cgi?id=691784
  ;; TODO: https://bugzilla.redhat.com/show_bug.cgi?id=691788
  ;; TODO: https://bugzilla.redhat.com/show_bug.cgi?id=727631

(gen-class-testng)
