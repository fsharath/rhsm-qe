(ns rhsm.gui.tests.rhn_interop_tests
  (:use [test-clj.testng :only (gen-class-testng)]
        [rhsm.gui.tasks.test-config :only (config
                                           clientcmd)]
        [com.redhat.qe.verify :only (verify)]
        rhsm.gui.tasks.tools
        gnome.ldtp)
  (:require [clojure.tools.logging :as log]
            [rhsm.gui.tasks.tasks :as tasks]
            [rhsm.gui.tests.base :as base]
             rhsm.gui.tasks.ui)
  (:import [org.testng.annotations
            BeforeClass
            AfterClass
            BeforeGroups
            Test]))

(def systemid "/etc/sysconfig/rhn/systemid")

(defn systemid-exists? []
  (bash-bool (:exitcode (run-command (str "test -e " systemid)))))

(defn kill-app []
  (run-command "killall -9 subscription-manager-gui")
  (tasks/ui waittillwindownotexist :main-window 30))

(defn ^{BeforeClass {:groups ["setup"]}}
  setup [_]
  (try
    (if (= "RHEL7" (get-release)) (base/startup nil))
    (if (tasks/ui exists? :main-window "*")
      (kill-app))
    (run-command (str "touch " systemid))
    (catch Exception e
      (reset! (skip-groups :interop) true)
      (throw e))))

(defn ^{AfterClass {:groups ["cleanup"]}}
  cleanup [_]
  (if-not (tasks/ui exists? :main-window "*")
    (tasks/start-app))
  (run-command (str "rm -f " systemid)))

(defn ^{Test {:groups ["interop"
                       "tier1"]}}
  check_warning
  "Tests that a warning message is shown when registered to classic and launching the app."
  [_]
  (tasks/start-app)
  (tasks/ui waittillwindowexist :warning-dialog 30)
  (verify (systemid-exists?))
  (verify (tasks/ui exists? :warning-dialog "*"))
  (kill-app))

(defn ^{Test {:groups ["interop"
                       "tier1"
                       "blockedByBug-667991"]
              :dependsOnMethods ["check_warning"] }}
  check_warning_ok
  "Tests that the RHN Classic warning can be cleared and that the main winow is still open."
  [_]
  (tasks/start-app)
  (tasks/ui waittillwindowexist :warning-dialog 30)
  (tasks/ui click :warn-ok)
  (verify (tasks/ui waittillwindownotexist :warning-dialog 30))
  (verify (tasks/ui exists? :main-window "*"))
  (kill-app))

(defn ^{Test {:groups ["interop"
                       "tier1"
                       "blockedByBug-667991"]
              :dependsOnMethods ["check_warning"]}}
  check_warning_cancel
  "Tests the cancel button of the RHN Classic warning and that the main window closes."
  [_]
  (tasks/start-app)
  (tasks/ui waittillwindowexist :warning-dialog 30)
  (tasks/ui click :warn-cancel)
  (verify (bool (tasks/ui waittillwindownotexist :main-window 30)))
  (kill-app))

(defn ^{Test {:groups ["interop"
                       "tier1"
                       "blockedByBug-667991"]
              :dependsOnMethods ["check_warning" "check_warning_ok" "check_warning_cancel"]}}
  check_no_warning
  "Asserts that no warning is shown when the app is started without the presence of a systemid file."
  [_]
  (safe-delete systemid)
  (verify (not (systemid-exists?)))
  (tasks/start-app)
  (verify (not (tasks/ui exists? :warning-dialog "*"))))

(gen-class-testng)
