(ns rhsm.gui.tests.repo_tests
  (:use [test-clj.testng :only (gen-class-testng
                                data-driven)]
        [rhsm.gui.tasks.test-config :only (config
                                           clientcmd)]
        [com.redhat.qe.verify :only (verify)]
        [slingshot.slingshot :only (try+
                                    throw+)]
        [clojure.string :only (split
                               split-lines
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
           org.testng.SkipException
           [com.redhat.qe.auto.bugzilla BzChecker]))

(def random_row_num (atom nil)) ;; Used to dynamically select a random row number
(def list_row (atom []))        ;; Used to hold probable row numbers
(def counter (atom 0))           ;; Used as counter for recursive calls
(def no_repos_message "No repositories are available without an attached subscription.")
(def ns-log "rhsm.gui.tests.repo_tests")

(defn ^{BeforeClass {:groups ["setup"]}}
  setup [_]
  (try+
    (if (= "RHEL7" (get-release)) (base/startup nil))
    (tasks/restart-app)
    (tasks/unregister)
    (catch [:type :not-registered] _)
    (catch Exception e
      (reset! (skip-groups :repo) true)
      (throw e))))

(defn assert-and-open-repo-dialog
  "Asserts if repo-dialog is open if not it opens it"
  []
  (sleep 3000)
  (if (not (bool (tasks/ui guiexist :repositories-dialog)))
    (do (tasks/ui click :repositories)
        (tasks/ui waittillwindowexist :repositories-dialog 10)
        (sleep 4000))
    (do
      (tasks/ui click :close-repo-dialog)
      (tasks/ui waittillwindownotexist :repositories-dialog 10)
      (assert-and-open-repo-dialog))))

(defn assert-and-subscribe-all
  "Asserts if the system is already subscribed before subscribe_all"
  []
  (let [row-count (tasks/ui getrowcount :installed-view)
        status (tasks/ui gettextvalue :overall-status)]
    (if (substring? (str row-count) status)
      (tasks/subscribe_all)
      (sleep 2000))))

(defn assert-and-remove-all-override
  "Asserts if remove all override button functionality"
  [& {:keys [repo]
      :or {repo nil}}]
  (if (tasks/has-state? :repo-remove-override "enabled")
    (do
      (tasks/ui click :repo-remove-override)
      (tasks/ui waittillwindowexist :question-dialog 10)
      (if-not (nil? repo)
        (verify (substring?
                 repo (tasks/ui gettextvalue
                                :question-dialog "Are you sure*"))))
      (tasks/ui click :yes)
      (tasks/checkforerror)
      (verify (bash-bool (tasks/ui guiexist :question-dialog))))))

(defn- exception-handler
  "This is to handle exceptions thrown while checking and unchecking
   checkbox in repo-dialog"
  [row-num col-num & {:keys [uncheck]
                      :or [uncheck false]}]
  (try
    (if (true? uncheck)
      (tasks/ui uncheckrow :repo-table row-num col-num)
      (tasks/ui checkrow :repo-table row-num col-num))
       (catch Exception e
         (if (substring? "Failed to grab focus" (.getMessage e))
           (sleep 5000)
           (throw e)))))

(defn select-random-repo
  "This is a helper function to select random repo which does
   does not have both overrides enabled"
  []
  (sleep 3000)
   (if (= 0 (tasks/ui getrowcount :repo-table))
      (throw (Exception. "Repositories table is not populated")))
   (let [row-count (tasks/ui getrowcount :repo-table)
         list-row (into [] (range row-count))
         row-num (nth list-row (rand (count list-row)))
         repo (tasks/ui getcellvalue :repo-table row-num 2)]
     (tasks/ui selectrow :repo-table repo)
     ;;(sleep 2000)
     (if (check-bz-open? "1155954")
       (do
         (log/info (str "======= Work Around in select-random-repo
                         as Bug 1155954 is not resolved"))
         ;; Work around catches exception and ignores it.
         ;; Why Workaround ?? At the moment 'checkrow' fails because
         ;;                   there is a performance issue in the repo
         ;;                   dialog were the checkbox takes a while
         ;;                   before it can be accessed/asserted.
         (exception-handler row-num 0)
         (exception-handler row-num 1))
         ;; No work around. If the performance improvement is as expected
         ;; the sleep command embedded inbetween can be removed.
       (do
         (tasks/ui checkrow :repo-table row-num 1)
         (sleep 2000)
         (tasks/ui checkrow :repo-table row-num 0)
         (sleep 2000)))
     (if-not (and (tasks/has-state? :repo-remove-override "sensitive")
                  (tasks/has-state? :repo-remove-override "enabled")
                  (< @counter 10))
       (do
         (reset! counter (inc @counter))
         (assert-and-remove-all-override :repo repo)
         (select-random-repo)))
     (if-not (= @counter 10)
       (do
         (assert-and-remove-all-override :repo repo)
         (reset! random_row_num row-num))))
   (reset! counter 0))

(defn ^{Test {:groups ["repo"
                       "tier1"
                       "acceptance"]}}
  check_repo_visible
  "This test checks whether repository option exists
   when system is unregistered"
  [_]
  (tasks/restart-app :unregister? true)
  (tasks/ui click :main-window "System")
  (verify (not (tasks/visible? :repositories))))

(defn ^{Test {:groups ["repo"
                       "tier1"
                       "acceptance"]}}
  check_repo_system_menu
  "This tests for repository option in the system menu"
  [_]
  (if (tasks/ui showing? :register-system)
    (tasks/register-with-creds))
  (try
    (assert-and-open-repo-dialog)
    (verify (bool (tasks/ui guiexist :repositories-dialog)))
    (finally
      (sleep 2000)
      (tasks/ui click :close-repo-dialog))))

(defn ^{Test {:groups ["repo"
                       "tier1"
                       "acceptance"]}}
  check_repo_message_unsubscribed
  "This tests for default static message in repository dialog when unsubscribed"
  [_]
  (try
    (if (tasks/ui showing? :register-system)
      (tasks/register-with-creds))
    (assert-and-open-repo-dialog)
    (verify (bool (tasks/ui guiexist :repositories-dialog)))
    (verify (= no_repos_message (tasks/ui gettextvalue :repo-message)))
    (finally
     (tasks/ui click :close-repo-dialog))))

(defn ^{Test {:groups ["repo"
                       "tier1"
                       "blockedByBug-1095938"]}}
  check_repo_table_populated
  "This tests if repo-table is populated when subscribed"
  [_]
  (try
    (if (tasks/ui showing? :register-system)
      (tasks/register-with-creds))
    (tasks/subscribe_all)
    (assert-and-open-repo-dialog)
    (verify (bool (tasks/ui guiexist :repositories-dialog)))
    (verify (< 0 (tasks/ui getrowcount :repo-table)))
    (finally
     (tasks/ui click :close-repo-dialog)
     (tasks/unsubscribe_all))))

(defn ^{Test {:groups ["repo"
                       "tier2"
                       "blockedByBug-1095938"]}}
  check_repo_remove_override_button
  "This tests if repo-override button is enabled when a row is checked"
  [_]
  (try
    (if (tasks/ui showing? :register-system)
      (tasks/register-with-creds))
    (tasks/subscribe_all)
    (assert-and-open-repo-dialog)
    (verify (bool (tasks/ui guiexist :repositories-dialog)))
    (if (= 0 (tasks/ui getrowcount :repo-table))
      (throw (Exception. "Repositories table is not populated"))
      (do
        (select-random-repo)
        (if (nil? @random_row_num)
          (throw (SkipException.
                  (str "Repo without overrides not found"))))
        (tasks/ui selectrowindex :repo-table @random_row_num)
        (verify (not (tasks/has-state? :repo-remove-override "enabled")))
        (if (check-bz-open? "1155954")
          (do
            (log/info (str "======= Work Around in check_repo_remove_override_button
                            as Bug 1155954 is not resolved"))
            ;; Workaround: Catches exception and ignores it.
            ;; Why Workaround ?? At the moment 'checkrow' fails because
            ;;                   there is a performance issue in the repo
            ;;                   dialog were the checkbox takes a while
            ;;                   before it can be accessed/asserted.
            (exception-handler @random_row_num 0)
            (exception-handler @random_row_num 1))
          ;; No work around. If the performance improvement is as expected
          ;; the sleep command embedded inbetween can be removed.
          (do
            (tasks/ui checkrow :repo-table @random_row_num 0)
            (sleep 1000)
            (tasks/ui checkrow :repo-table @random_row_num 1)
            (sleep 1000)))
        (sleep 2000)
        (verify (tasks/has-state? :repo-remove-override "enabled"))
        (assert-and-remove-all-override)
        (sleep 2000)
        (verify (not (tasks/has-state? :repo-remove-override "enabled")))))
    (finally
     (tasks/ui click :close-repo-dialog)
     (tasks/unsubscribe_all))))

(defn toggle-checkbox-state
  "This is a helper function to toggle state of checkbox"
  [row-num]
  (if (check-bz-open? "1155954")
    (do
      (log/info (str "======= Work Around in toggle-checkbox-state
                      as Bug 1155954 is not resolved"))
      ;; Work around catches exception and ignores it.
      ;; Why Workaround ?? At the moment 'checkrow' fails because
      ;;                   there is a performance issue in the repo
      ;;                   dialog were the checkbox takes a while
      ;;                   before it can be accessed/asserted.
      (exception-handler row-num 0)
      (exception-handler row-num 1)
      (if-not (and (tasks/has-state? :repo-remove-override "enabled")
               (tasks/has-state? :repo-remove-override "sensitive"))
        (do
          (exception-handler row-num 0 :uncheck true)
          (exception-handler row-num 1 :uncheck true))))
    ;; No work around. If the performance improvement is as expected
    ;; the sleep command embedded inbetween can be removed.
    (do
      (tasks/ui checkrow :repo-table row-num 0)
      (sleep 1000)
      (tasks/ui checkrow :repo-table row-num 1)
      (sleep 1000)
      (if-not (and (tasks/has-state? :repo-remove-override "enabled")
                   (tasks/has-state? :repo-remove-override "sensitive"))
        (do
          (tasks/ui uncheckrow :repo-table row-num 0)
          (sleep 1000)
          (tasks/ui uncheckrow :repo-table row-num 1)
          (sleep 1000))))))

(defn ^{Test {:groups ["repo"
                       "tier3"
                       "blockedByBug-1095938"
                       "assert_remove_all_overrides"]
              :dataProvider "repolist"}}
  enable_repo_remove_all_overrides
  "Enable all repos and click remove all override and check state"
  [_ repo]
  (assert-and-open-repo-dialog)
  (tasks/ui selectrow :repo-table repo)
  (sleep 3000)
  (let [row-num (tasks/ui gettablerowindex :repo-table repo)]
    (toggle-checkbox-state row-num))
  (verify (tasks/has-state? :repo-remove-override "enabled"))
  (assert-and-remove-all-override)
  (verify (not (tasks/has-state? :repo-remove-override "enabled"))))

(defn ^{AfterGroups {:groups ["repo"
                              "tier3"]
                     :value ["assert_remove_all_overrides"]
                     :alwaysRun true}}
  after_enable_repo_remove_all_overrides
  [_]
  (tasks/ui click :close-repo-dialog)
  (tasks/unsubscribe_all))

(comment
;; Comenting the below test group because iteratively enabling and checking for override
;; persistance had multi-point failure. Since we have stopped testing on GUI, decided
;; not to explore more and substitured this iterative test with a simple test

(defn ^{BeforeGroups {:groups ["repo"
                               "tier3"]
                      :value ["assert_override_persistance"]}}
  before_verify_override_persistance
  "Modofies all repos by clicking edit gpg-check"
  [_]
  (log/info (str "======= Starting BeforeGroup: " ns-log
                 " before_verify_override_persistance"))
  (tasks/restart-app :reregister? true)
  (tasks/subscribe_all)
  (assert-and-open-repo-dialog)
  (tasks/do-to-all-rows-in :repo-table 2
                           (fn [repo]
                             (sleep 2000)
                             (tasks/ui selectrow :repo-table repo)
                             (let [row-num (tasks/ui gettablerowindex :repo-table repo)]
                               (tasks/ui checkrow :repo-table row-num 1)
                               (sleep 2000)
                               (tasks/ui checkrow :repo-table row-num 0)
                               (sleep 2000))))
  (tasks/ui click :close-repo-dialog)
  (tasks/unsubscribe_all))

(defn ^{Test {:groups ["repo"
                       "tier3"
                       "blockedByBug-1095938"
                       "assert_override_persistance"]
              :dataProvider "repolist"}}
  verify_override_persistance
  "Checks the persistance of repo override after subscriptions are removed"
  [_ repo]
  (assert-and-open-repo-dialog)
  (tasks/ui selectrow :repo-table repo)
  (sleep 2000)
  (verify (and (tasks/has-state? :repo-remove-override "sensitive")
               (tasks/has-state? :repo-remove-override "enabled"))))

(defn ^{AfterGroups {:groups ["repo"
                              "tier3"]
                     :value ["assert_override_persistance"]
                     :alwaysRun true}}
  after_verify_override_persistance
  [_]
  (log/info (str "======= Starting AfterGroup: " ns-log
                 " after_verify_override_persistance"))
  (assert-and-open-repo-dialog)
  (tasks/do-to-all-rows-in :repo-table 2
                           (fn [repo]
                             (sleep 1000)
                             (tasks/ui selectrow :repo-table repo)
                             (assert-and-remove-all-override)))
  (tasks/ui click :close-repo-dialog)
  (tasks/unsubscribe_all))

);; Comment ends here

(defn ^{Test {:groups ["repo"
                       "tier1"
                       "blockedByBug-1095938"]}}
  verify_override_persistance
  "Checks the persistance of repo override after subscriptions are removed"
  [_]
  (tasks/restart-app :reregister? true)
  (tasks/subscribe_all)
  (assert-and-open-repo-dialog)
  (verify (bool (tasks/ui guiexist :repositories-dialog)))
  (try
    (select-random-repo)
    (if (nil? @random_row_num)
      (throw (SkipException.
              (str "Repo without overrides not found"))))
    ;; overriding random repo
    (tasks/ui selectrowindex :repo-table @random_row_num)
    (sleep 3000)
    (if (check-bz-open? "1155954")
      (do
        (log/info (str "======= Work Around in verify_override_persistance
                      as Bug 1155954 is not resolved"))
        ;; Work around catches exception and ignores it.
        ;; Why Workaround ?? At the moment 'checkrow' fails because
        ;;                   there is a performance issue in the repo
        ;;                   dialog were the checkbox takes a while
        ;;                   before it can be accessed/asserted.
        (exception-handler @random_row_num 0)
        (exception-handler @random_row_num 1))
      ;; No work around. If the performance improvement is as expected
      ;; the sleep command embedded inbetween can be removed.
      (do
        (tasks/ui checkrow :repo-table @random_row_num 1)
        (sleep 2000)
        (tasks/ui checkrow :repo-table @random_row_num 0)
        (sleep 2000)))
    (tasks/ui click :close-repo-dialog)
    (tasks/unsubscribe_all)
    ;; verifying persistnace of override
    (tasks/subscribe_all)
    (assert-and-open-repo-dialog)
    (tasks/ui selectrowindex :repo-table @random_row_num)
    (sleep 2000)
    (verify (and (tasks/has-state? :repo-remove-override "sensitive")
                 (tasks/has-state? :repo-remove-override "enabled")))
    (assert-and-remove-all-override)
    (finally
      (tasks/ui click :close-repo-dialog)
      (tasks/unsubscribe_all))))

(defn ^{Test {:groups ["repo"
                       "tier3"
                       "blockedByBug-1095938"]
              :value ["assert_repo_dialog_fields"]
              :dataProvider "repolist"}}
  check_repo_name_url
  "Checks if name and URL are populated for all repositories"
  [_ repo]
  (assert-and-open-repo-dialog)
  (tasks/ui selectrow :repo-table repo)
  (verify (not (blank? (tasks/ui gettextvalue :base-url))))
  (verify (not (blank? (tasks/ui gettextvalue :repo-name)))))

(defn ^{AfterGroups {:groups ["repo"
                              "tier3"]
                     :value ["assert_repo_dialog_fields"]
                     :alwaysRun true}}
  after_check_repo_name_url
  [_]
  (if (bool (tasks/ui guiexist :repositories-dialog))
    (tasks/ui click :close-repo-dialog))
  (tasks/unsubscribe_all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA PROVIDERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^{DataProvider {:name "repolist"}}
  subscribed_repos [_ & {:keys [debug]
                         :or {debug false}}]
  (log/info (str "======= Starting DataProvider: " ns-log " subscribed_repos()"))
  (if-not (assert-skip :repo)
    (do
      (tasks/restart-app)
      (if (tasks/ui showing? :register-system)
        (tasks/register-with-creds))
      (assert-and-subscribe-all)
      (assert-and-open-repo-dialog)
      (tasks/ui waittillwindowexist :repositories-dialog 10)
      (sleep 3000)
      (let [repos (into [] (map vector (tasks/get-table-elements
                                        :repo-table 2)))]
        (if-not debug
          (to-array-2d repos)
          repos)))
    (to-array-2d [])))

(gen-class-testng)
