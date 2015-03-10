(ns rhsm.gui.tests.subscription_status_tests
  (:use [test-clj.testng :only (gen-class-testng)]
        [rhsm.gui.tasks.test-config :only (config
                                           clientcmd
                                           cli-tasks
                                           candlepin-runner)]
        [com.redhat.qe.verify :only (verify)]
        [clojure.string :only (split
                               blank?)]
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
            AfterClass
            AfterGroups
            Test
            DataProvider]
           org.testng.SkipException))

(def status-before-subscribe (atom {}))
(def contractlist (atom {}))
(def ns-log "rhsm.gui.tests.subscription_status_tests")

(defn build-contract-map
  "Builds the contract/virt-type map and updates the cont"
  []
  (reset! contractlist (ctasks/build-virt-type-map :all? true))
  @contractlist)

(defn ^{BeforeClass {:groups ["setup"]}}
  before_check_status_message
  [_]
  (try
    (if (= "RHEL7" (get-release)) (base/startup nil))
    (tasks/restart-app :reregister? true)
    (catch Exception e
      (reset! (skip-groups :subscription_status) true)
      (throw e))))

(defn ^{AfterClass {:groups ["cleanup"]
                     :alwaysRun true}}
  after_check_status_message
  [_]
  (tasks/unsubscribe_all)
  ;; Below code was used to set the time and dater after testing
  ;; future subscription. Test for future subscriptions has been
  ;; commented becasue changing dat and time could lead to automation
  ;; failure. The risk out weighs the test
  (comment (let
               [time-cmd (if (= "RHEL7" (get-release))
                           (str "systemctl stop ntpd.service;"
                                " ntpdate clock.redhat.com;"
                                " systemctl start ntpd.service")
                           (str "service ntpd stop;"
                                " ntpdate clock.redhat.com;"
                                " service ntpd start"))]
             (:stdout (run-command time-cmd))
             (:stdout (run-command time-cmd :runner @candlepin-runner)))))

(defn ^{Test {:groups ["subscription_status"
                       "tier1"
                       "acceptance"
                       "blockedByBug-1012501"
                       "blockedByBug-1040119"]
              :priority (int 100)}}
  check_status_message_before_attaching
  "Asserts that status message displayed in main-window is right before subscriptions are attached"
  [_]
  (if (tasks/ui showing? :register-system)
    (tasks/register-with-creds))
  (try
    (tasks/unsubscribe_all)
    (sleep 3000)
    (let
      [installed-products (atom nil)]
      (reset! installed-products (tasks/ui getrowcount :installed-view))
      (reset! status-before-subscribe
              (Integer. (re-find #"\d*" (tasks/ui gettextvalue :overall-status))))
      (verify (= @installed-products @status-before-subscribe)))))

(defn ^{Test {:groups ["subscription_status"
                       "tier1"
                       "acceptance"
                       "blockedByBug-1012501"
                       "blockedByBug-1040119"
                       "blockedByBug-1199671"]
              :dependsOnMethods ["check_status_message_before_attaching"]}}
  check_status_message_after_attaching
  "Asserts that status message displayed in main-window is right after attaching subscriptions"
  [_]
  (try
    (let
  	[subscribed-products (atom (int 0))
         after-subscribe (atom (int 0))]
      (tasks/search :match-installed? true)
      (dotimes [n 3]
        (tasks/subscribe (tasks/ui getcellvalue :all-subscriptions-view
                                   (rand-int (tasks/ui getrowcount :all-subscriptions-view)) 0)))
      (reset! subscribed-products (count (filter #(= "Subscribed" %)
                                                 (tasks/get-table-elements :installed-view 2))))
      (reset! after-subscribe (Integer. (re-find #"\d*"
                                                 (tasks/ui gettextvalue :overall-status))))
      (verify (= @after-subscribe (- @status-before-subscribe @subscribed-products))))))

(defn ^{Test {:groups ["subscription_status"
                       "tier1"
                       "blockedByBug-1012501"
                       "blockedByBug-1040119"
                       "blockedByBug-1199671"]
              :dependsOnMethods ["check_status_message_after_attaching"]}}
  check_status_message_future_subscriptions
  "Asserts that status message displayed in main-window is right after attaching future
   subscriptions"
  [_]
  (if (= "RHEL7" (get-release))
    (throw
     (SkipException.
      (str "Cannot generate keyevents in RHEL7 !!
            Skipping Test 'check_status_message_future_subscriptions'."))))
  (try
    (let
        [subscribed-products-date (atom (int 0))
         after-date-products (atom (int 0))
         present-date (do (tasks/ui selecttab :all-available-subscriptions)
                          (tasks/ui gettextvalue :date-entry))
         date-split (split present-date #"-")
         year (first date-split)
         month (second date-split)
         day (last date-split)
         new-year (+ (Integer. (re-find  #"\d+" year)) 1)]
      (tasks/ui enterstring :date-entry (str new-year "-" month "-" day))
      (tasks/search :match-installed? true)
      (dotimes [n 3]
        (tasks/subscribe (tasks/ui getcellvalue :all-subscriptions-view
                                   (rand-int (tasks/ui getrowcount :all-subscriptions-view)) 0)))
      (reset! subscribed-products-date (count
                                        (filter #(= "Subscribed" %)
                                                (tasks/get-table-elements :installed-view 2))))
      (reset! after-date-products (Integer. (re-find #"\d*"
                                                     (tasks/ui gettextvalue :overall-status))))
      (verify (= @after-date-products (- @status-before-subscribe @subscribed-products-date))))))

;; Commenting out this test as changing dates on the server can have drastic effects
(comment
  (defn ^{Test {:groups ["subscription_status"
                         "tier1"
                         "blockedByBug-1012501"
                         "blockedByBug-1040119"]
                :dependsOnMethods ["check_status_message_future_subscriptions"]}}
    check_status_message_expired_subscriptions
    "Asserts that status message displayed in main-window is right after expiring
   attached subscriptions"
    [_]
    (if (nil?  @candlepin-runner)
      (throw (SkipException.
              (str "This test will not run on stage as a candelpin-runner cannot be created")))
      (do
        (try
          (let
              [subscribed-products-future (atom {})
               after-future-subscribe (atom {})]
            (run-command "date -s \"+1 year\"")
            (run-command "date -s \"+1 year\"" :runner @candlepin-runner)
            (tasks/restart-app)
            (reset! subscribed-products-future
                    (count (filter #(= "Subscribed" %)
                                   (tasks/get-table-elements :installed-view 2))))
            (reset! after-future-subscribe
                    (Integer. (re-find #"\d*"
                                       (tasks/ui gettextvalue :overall-status))))
            (verify (= @after-future-subscribe
                       (- @status-before-subscribe @subscribed-products-future))))
          (finally
            (run-command "date -s \"-1 year\"" :runner @candlepin-runner)
            (run-command "date -s \"-1 year\"")))))))

 (defn ^{Test {:groups ["subscription_status"
                        "tier3"
                        "check_subscription_type_my_subs"]
              :dataProvider "my-subscriptions"}}
  check_subscription_type_my_subscriptions
  "Checks for subscription type in my subscriptions"
  [_ product]
  (tasks/ui selecttab :my-subscriptions)
  (tasks/skip-dropdown :my-subscriptions-view product)
  (verify (not (blank? (tasks/ui gettextvalue :subscription-type)))))

(defn ^{AfterGroups {:groups ["subscription_status"
                              "tier3"]
                     :value ["check_subscription_type_my_subs"]
                     :alwaysRun true}}
  after_check_subscription_type_my_subscription
  [_]
  (tasks/unsubscribe_all)
  (tasks/unregister))

(defn ^{Test {:groups ["subscription_status"
                       "tier1"
                       "blockedByBug-924766"]
              :dataProvider "subscribed"}}
  check_subscribed_virt_type
  "Asserts that the virt type is displayed properly for all of 'My Subscriptions'"
  [_ subscription]
  (tasks/ui selecttab :my-subscriptions)
  (tasks/skip-dropdown :my-subscriptions-view subscription)
  (let [contract (tasks/ui gettextvalue :contract-number)
        type (tasks/ui gettextvalue :support-type)
        ;reference (get (get @contractlist subscription) contract)
        vector-map (vec (get @contractlist subscription))
        map-search (fn [a] (get a contract))
        reference (filter #(if (not-nil? %) %)
                          (map map-search vector-map))]
    ;(verify (= type reference))
    (verify (not-nil? (some #{type} reference)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      DATA PROVIDERS      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^{DataProvider {:name "my-subscriptions"}}
  my_subscriptions [_ & {:keys [debug]
                         :or {debug false}}]
  (log/info (str "======= Starting DataProvider: " ns-log "my_subscriptions"))
  (if-not (assert-skip :system)
    (do
      (tasks/restart-app :reregister? true)
      (tasks/subscribe_all)
      (sleep 3000)
      (tasks/ui selecttab :my-subscriptions)
      (let [subs (into [] (map vector (tasks/get-table-elements
                                       :my-subscriptions-view
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
      (tasks/register-with-creds)
      (build-contract-map)
      (tasks/ui selecttab :my-subscriptions)
      (tasks/subscribe_all)
      (sleep 3000)
      (tasks/ui selecttab :my-subscriptions)
      (let [subs (into [] (map vector (tasks/get-table-elements
                                       :my-subscriptions-view
                                       0
                                       :skip-dropdowns? true)))]
        (if-not debug
          (to-array-2d subs)
          subs)))
    (to-array-2d [])))

(gen-class-testng)
