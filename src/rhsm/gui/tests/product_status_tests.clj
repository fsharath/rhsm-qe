(ns rhsm.gui.tests.product_status_tests
  (:use [test-clj.testng :only (gen-class-testng)]
        [rhsm.gui.tasks.test-config :only (config
                                           clientcmd
                                           cli-tasks
                                           candlepin-runner)]
        [com.redhat.qe.verify :only (verify)]
        [clojure.string :only (blank?
                               split-lines
                               split
                               trim)]
        rhsm.gui.tasks.tools
        clojure.pprint
        gnome.ldtp)
  (:require [rhsm.gui.tasks.tasks :as tasks]
            [rhsm.gui.tests.base :as base]
            [rhsm.gui.tasks.candlepin-tasks :as ctasks]
            rhsm.gui.tasks.ui
            [clojure.tools.logging :as log])
  (:import [org.testng.annotations
            BeforeClass
            AfterGroups
            Test
            DataProvider]
           org.testng.SkipException))

(def ns-log "rhsm.gui.tests.product_status_tests")
(def unreg-status "Keep your system up to date by registering.")
(def ldtpd-log "/var/log/ldtpd/ldtpd.log")
(def productstatus (atom nil))

(defn ^{BeforeClass {:groups ["setup"]}}
  before_check_product_status [_]
  (try
    (if (= "RHEL7" (get-release)) (base/startup nil))
    (tasks/restart-app :reregister? true)
    (let [output (:stdout
                  (run-command "subscription-manager attach --auto"))
          not-blank? (fn [s] (not (blank? s)))
          raw-cli-data (filter not-blank? (drop 1 (split-lines output)))
          status (fn [s] (re-find #"^Status.*" s))
          products (fn [s] (re-find #"^Product Name.*" s))
          grab-value (fn [item] (trim (last (split item #":"))))
          not-nil? (fn [s] (not (nil? s)))
          list-status (into [] (map grab-value (filter not-nil? (map status raw-cli-data))))
          list-products (into [] (map grab-value (filter not-nil? (map products raw-cli-data))))
          cli-data (zipmap list-products list-status)]
    (reset! productstatus cli-data))
    (catch Exception e
      (reset! (skip-groups :product_status) true)
      (throw e))))

(defn ^{Test {:groups ["product_status"
                       "tier3"
                       "blockedByBug-964332"]
              :dataProvider "subscribed"}}
  check_product_status_subscribed
  "Asserts that all product statuses match the known statuses in the CLI."
  [_ product row]
  (let [gui-value (tasks/ui getcellvalue :installed-view row 2)
        cli-value (get @productstatus product)]
    (verify (= gui-value cli-value))))

(defn ^{Test {:groups ["product_status"
                       "tier3"]
              :dependsOnMethods ["check_product_status_subscribed"]
              :dataProvider "installed-products"}}
  check_product_status_unsubscribed
  "Checks product status is correct after unsubscribing."
  [_ product]
  (let [gui-status (tasks/ui getcellvalue :installed-view
                             (tasks/skip-dropdown :installed-view product) 2)]
    (verify (= gui-status "Not Subscribed"))))

(defn ^{Test {:groups ["product_status"
                       "tier1"
                       "blockedByBug-923873"]}}
  check_status_when_unregistered
  "To verify that status in MyInstalledProducts icon color and product status
   are appropriately displayed when client is unregistered"
  [_]
  (tasks/restart-app :unregister? true)
  (run-command "subscription-manager clean")
  (verify (= unreg-status (tasks/ui gettextvalue :overall-status)))
  (tasks/do-to-all-rows-in
   :installed-view 2
   (fn [status]
     (verify (= status "Unknown")))))

(defn ^{Test {:groups ["product_status"
                       "tier3"
                       "assert_subscription_field"]
              :dataProvider "installed-products"}}
  assert_subscription_field
  "Tests whether the subscripton field in installed view is populated when the entitlement
   is subscribed"
  [_ product]
  (if (not (= "Not Subscribed"
              (tasks/ui getcellvalue :installed-view
                        (tasks/skip-dropdown :installed-view product) 2)))
    (let [map (ctasks/build-product-map :all? true)
          gui-value (set (clojure.string/split-lines
                          (tasks/ui gettextvalue :providing-subscriptions)))
          cli-value (set (get map product))]
      (verify (< 0 (count (clojure.set/intersection gui-value cli-value)))))))

(defn ^{AfterGroups {:groups ["product_status"
                              "tier3"]
                     :value ["assert_subscription_field"]
                     :alwaysRun true}}
  after_assert_subscription_field
  [_]
  (tasks/unsubscribe_all)
  (tasks/unregister))

(defn ^{Test {:groups ["system"
                       "acceptance"
                       "tier1"
                       "blockedByBug-1051383"]}}
  check_status_column
  "Asserts that the status column of GUI has only 'Subscribed', 'Partially Subscribed'
   and 'Not Subscribed'"
  [_]
  (try
    (if (not (bool (tasks/ui guiexist :main-window)))
      (tasks/start-app))
    (let [output (get-logging @clientcmd
                              ldtpd-log
                              "check_online_documentation"
                              nil
                              (do
                                (tasks/ui click :online-documentation)
                                (sleep 5000)))]
      (verify (bool (tasks/ui appundertest "Firefox")))
      (verify (not (substring? "Traceback" output))))
    (finally
      (run-command "killall -9 firefox"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA PROVIDERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^{DataProvider {:name "subscribed"}}
  subscribed_products [_ & {:keys [debug]
                               :or {debug false}}]
  (log/info (str "======= Starting DataProvider: " ns-log "/subscribed_products()"))
  (if-not (assert-skip :facts)
    (do
      (tasks/restart-app :reregister? true)
      (:stdout (run-command "subscription-manager attach --auto"))
      (let [prods (tasks/get-table-elements :installed-view 0)
            indexes (range 0 (tasks/ui getrowcount :installed-view))
            prodlist (map (fn [item index] [item index]) prods indexes)]
        (if-not debug
          (to-array-2d (vec prodlist))
          prodlist)))
    (to-array-2d [])))

(defn ^{DataProvider {:name "installed-products"}}
  installed_products [_ & {:keys [debug]
                           :or {debug false}}]
  (log/info (str "======= Starting DataProvider: "
                 ns-log "/installed_products()"))
  (if-not (assert-skip :system)
    (do
      (tasks/restart-app)
      (tasks/register-with-creds)
      (tasks/ui selecttab :my-installed-products)
      (let [subs (into [] (map vector (tasks/get-table-elements
                                       :installed-view
                                       0
                                       :skip-dropdowns? true)))]
        (if-not debug
          (to-array-2d subs)
          subs)))
    (to-array-2d [])))

(gen-class-testng)
