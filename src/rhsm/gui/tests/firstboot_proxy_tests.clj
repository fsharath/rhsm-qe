(ns rhsm.gui.tests.firstboot_proxy_tests
  (:use [test-clj.testng :only (gen-class-testng
                                data-driven)]
        [rhsm.gui.tasks.test-config :only (config
                                           clientcmd
                                           auth-proxyrunner
                                           noauth-proxyrunner)]
        [com.redhat.qe.verify :only (verify)]
        [slingshot.slingshot :only (try+
                                    throw+)]
        rhsm.gui.tasks.tools
        gnome.ldtp)
  (:require [clojure.tools.logging :as log]
            [rhsm.gui.tasks.tasks :as tasks]
            [rhsm.gui.tests.base :as base]
             rhsm.gui.tasks.ui)
  (:import [org.testng.annotations
            AfterClass
            BeforeClass
            BeforeGroups
            Test]
            org.testng.SkipException
            [com.redhat.qe.auto.bugzilla BzChecker]))

(def firstboot-auth-log "/var/log/squid/access.log")
(def firstboot-noauth-log "/var/log/tinyproxy.log")
(def window-name "Choose Service")
(def proxy-success "Proxy connection succeeded")

(defn start_firstboot []
  (if (= "RHEL7" (get-release))
    (do (if-not (bool (tasks/ui guiexist :main-window))
          (tasks/restart-app))
        (if-not (tasks/ui showing? :register-system)
          (tasks/unregister))
        (tasks/start-firstboot)
        (sleep 1000)
        (verify (tasks/fbshowing? :firstboot-window
                                  "Subscription Management Registration"))
        ;; (tasks/ui click :register-now)    unable to access this radio button
        (tasks/ui click :firstboot-forward))
    (do
      (tasks/start-firstboot)
      (tasks/ui click :firstboot-forward)
      (tasks/ui click :license-yes)
      (tasks/ui click :firstboot-forward)
      ;; RHEL5 has a different firstboot order than RHEL6
      (if (and (= "RHEL5" (get-release))
               (tasks/fbshowing? :firewall))
        (do
          (tasks/ui click :firstboot-forward)
          (tasks/ui click :firstboot-forward)
          (tasks/ui click :firstboot-forward)
          (tasks/ui click :firstboot-forward)
          (sleep 3000) ;; FIXME find a better way than a hard wait...
          (verify (tasks/fbshowing? :software-updates))))
      (tasks/ui click :register-now)
      (tasks/ui click :firstboot-forward)
      (assert (bool (tasks/ui guiexist :firstboot-window window-name))))))

(defn kill_firstboot []
  (run-command "killall -9 firstboot")
  (sleep 5000))

(defn zero-proxy-values []
  (tasks/set-conf-file-value "proxy_hostname" "")
  (tasks/set-conf-file-value "proxy_port" "")
  (tasks/set-conf-file-value "proxy_user" "")
  (tasks/set-conf-file-value "proxy_password" ""))

(defn reset_firstboot []
  (kill_firstboot)
  (if-not (tasks/ui showing? :register-system)
    (do (tasks/unsubscribe_all)
        (tasks/unregister)))
  (run-command "subscription-manager clean")
  (zero-proxy-values)
  (start_firstboot))

(defn ^{BeforeClass {:groups ["setup"]}}
  firstboot_proxy_init [_]
  (try
    (if (= "RHEL7" (get-release)) (base/startup nil))
    (if (= "5.7" (:version (get-release :true)))
      (throw (SkipException.
              (str "Skipping firstboot tests on RHEL 5.7 as the tool is not updated"))))
    (skip-if-bz-open "922806")
    (skip-if-bz-open "1016643" (= "RHEL7" (get-release)))
    ;; new rhsm and classic have to be totally clean for this to run
    (run-command "subscription-manager clean")
    (let [sysidpath "/etc/sysconfig/rhn/systemid"]
      (run-command (str "[ -f " sysidpath " ] && rm " sysidpath )))
    (catch Exception e
      (reset! (skip-groups :firstboot) true)
      (throw e))))

(defn ^{AfterClass {:groups ["cleanup"]
                    :alwaysRun true}}
  firstboot_proxy_cleanup [_]
  (assert-valid-testing-arch)
  (kill_firstboot)
  (run-command "subscription-manager clean")
  (zero-proxy-values))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier1"
                       "acceptance"]
              :priority (int 100)}}
  firstboot_enable_proxy_auth_connect
  "Asserts that the rhsm.conf file is correctly set after setting a proxy with auth."
  [_]
  (try
    (start_firstboot)
    (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
    (tasks/ui click :firstboot-window "proxy_button")
    (tasks/ui waittillguiexist :firstboot-proxy-dialog)
    (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
    (let [hostname (@config :basicauth-proxy-hostname)
         port     (@config :basicauth-proxy-port)
         username (@config :basicauth-proxy-username)
         password (@config :basicauth-proxy-password)]
      (tasks/enableproxy hostname :port port :user username :pass password :firstboot? true)
    (tasks/firstboot-register (@config :username) (@config :password))
    (tasks/ui click :firstboot-forward)
    (sleep 2000)
    (verify (not (bool (tasks/ui guiexist :firstboot-window))))
    (verify (not (tasks/ui showing? :register-system)))
    (tasks/verify-conf-proxies hostname port username password))
    (finally
      (reset_firstboot)
      (tasks/disableproxy true)
      (kill_firstboot))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier1"
                       "acceptance"]
              :priority (int 101)}}
  firstboot_enable_proxy_noauth_connect
  "Asserts that the rhsm.conf file is correctly set after setting a proxy without auth."
  [_]
  (try
    (start_firstboot)
    (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
    (tasks/ui click :firstboot-window "proxy_button")
    (tasks/ui waittillguiexist :firstboot-proxy-dialog)
    (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
    (let [hostname (@config :noauth-proxy-hostname)
          port (@config :noauth-proxy-port)]
      (tasks/enableproxy hostname :port port :firstboot? true)
    (tasks/firstboot-register (@config :username) (@config :password))
    (tasks/ui click :firstboot-forward)
    (sleep 2000)
    (verify (not (bool (tasks/ui guiexist :firstboot-window))))
    (verify (not (tasks/ui showing? :register-system)))
    (tasks/verify-conf-proxies hostname port "" ""))
    (finally
      (reset_firstboot)
      (tasks/disableproxy true)
      (kill_firstboot))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier1"]
              :dependsOnMethods ["firstboot_enable_proxy_auth"
                                 "firstboot_enable_proxy_noauth"]}}
  firstboot_disable_proxy
  "Asserts that the rhsm.conf file is correctly set after diabling proxies."
  [_]
  (reset_firstboot)
  (tasks/disableproxy true)
  (kill_firstboot)
  (tasks/verify-conf-proxies "" "" "" ""))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier1"]
              :dependsOnMethods ["firstboot_enable_proxy_auth"]}}
  firstboot_proxy_auth_connect_logging
  "Asserts that rhsm can connect after setting a proxy with auth."
  [_]
  (try
    (start_firstboot)
    (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
    (tasks/ui click :firstboot-window "proxy_button")
    (tasks/ui waittillguiexist :firstboot-proxy-dialog)
    (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
    (let [hostname (@config :basicauth-proxy-hostname)
          port     (@config :basicauth-proxy-port)
          username (@config :basicauth-proxy-username)
          password (@config :basicauth-proxy-password)
          logoutput (get-logging @auth-proxyrunner
                                 firstboot-auth-log
                                 "firstboot-proxy-auth-connect"
                                 nil
                                 (do
                                   (tasks/enableproxy hostname :port port :user username
                                                      :pass password :firstboot? true)
                                   (tasks/firstboot-register (@config :username) (@config :password))
                                   (tasks/ui click :firstboot-forward)
                                   (sleep 2000)))]
      (verify (not (bool (tasks/ui guiexist :firstboot-window))))
      (verify (not (tasks/ui showing? :register-system)))
      (verify (not  (clojure.string/blank? logoutput))))
    (finally
      (reset_firstboot)
      (tasks/disableproxy true)
      (kill_firstboot))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier1"]
              :dependsOnMethods ["firstboot_enable_proxy_noauth"]}}
  firstboot_proxy_noauth_connect_logging
  "Asserts that rhsm can connect after setting a proxy without auth."
  [_]
  (try
    (start_firstboot)
    (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
    (tasks/ui click :firstboot-window "proxy_button")
    (tasks/ui waittillguiexist :firstboot-proxy-dialog)
    (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
    (let [hostname (@config :noauth-proxy-hostname)
          port     (@config :noauth-proxy-port)
          logoutput (get-logging @noauth-proxyrunner
                                 firstboot-noauth-log
                                 "firstboot-proxy-noauth-connect"
                                 nil
                                 (do
                                   (tasks/enableproxy hostname :port port :firstboot? true)
                                   (tasks/firstboot-register (@config :username) (@config :password))
                                   (tasks/ui click :firstboot-forward)
                                   (sleep 2000)))]
      (verify (not (bool (tasks/ui guiexist :firstboot-window))))
      (verify (not (tasks/ui showing? :register-system)))
      (verify (not  (clojure.string/blank? logoutput))))
    (finally
      (reset_firstboot)
      (tasks/disableproxy true)
      (kill_firstboot))))

(defn test_proxy [expected-message]
  (try
    (if (bool (tasks/ui hasstate :test-connection "SENSITIVE"))
      (do
       (tasks/ui click :test-connection)
       (sleep 2000)
       (let [message (tasks/ui gettextvalue :connection-status)]
         (verify (= expected-message message))))
      (do
        (verify (not (some #(= "sensitive" %)
                           (tasks/ui getallstates :test-connection))))
        (verify (= "" (tasks/ui gettextvalue :connection-status)))))
      (finally (tasks/ui click :close-proxy))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier1"]
              :dependsOnMethods ["firstboot_enable_proxy_auth"]}}
  firstboot_test_auth_proxy
  "Tests the 'test connection' button when using a proxy with auth."
  [_]
  (try
    (start_firstboot)
    (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
    (tasks/ui click :firstboot-window "proxy_button")
    (tasks/ui waittillguiexist :firstboot-proxy-dialog)
    (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
    (let [hostname (@config :basicauth-proxy-hostname)
         port     (@config :basicauth-proxy-port)
         username (@config :basicauth-proxy-username)
         password (@config :basicauth-proxy-password)]
      (tasks/enableproxy hostname :port port :user username :pass password
                         :firstboot? true :close? false))
  (test_proxy "Proxy connection succeeded")
  (finally
    (reset_firstboot)
    (tasks/disableproxy true)
    (kill_firstboot))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier1"]
              :dependsOnMethods ["firstboot_enable_proxy_noauth"]}}
  firstboot_test_noauth_proxy
  "Tests the 'test connection' button when using a proxy without auth."
  [_]
  (try
    (start_firstboot)
    (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
    (tasks/ui click :firstboot-window "proxy_button")
    (tasks/ui waittillguiexist :firstboot-proxy-dialog)
    (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
    (let [hostname (@config :noauth-proxy-hostname)
         port     (@config :noauth-proxy-port)]
      (tasks/enableproxy hostname :port port :firstboot? true :close? false))
  (test_proxy "Proxy connection succeeded")
  (finally
    (reset_firstboot)
    (tasks/disableproxy true)
    (kill_firstboot))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier1"]
              :dependsOnMethods ["firstboot_disable_proxy"]}}
  firstboot_test_disabled_proxy
  "Test that the 'test connection' button is disabled when proxy settings are cleared."
  [_]
  (try
    (start_firstboot)
    (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
    (tasks/ui click :firstboot-window "proxy_button")
    (tasks/ui waittillguiexist :firstboot-proxy-dialog)
    (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
    (test_proxy "")
    (finally
      (tasks/ui click :close-proxy)
      (reset_firstboot)
      (kill_firstboot))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier1"]
              :dependsOnMethods ["firstboot_disable_proxy"]}}
  firstboot_test_proxy_with_blank_proxy
  "Test whether 'Test Connection' returns appropriate message when 'Location Proxy' is empty"
  [_]
  (try
    (start_firstboot)
    (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
    (tasks/ui click :firstboot-window "proxy_button")
    (tasks/ui waittillguiexist :firstboot-proxy-dialog)
    (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
    (tasks/enableproxy "" :close? false :firstboot? true)
    (test_proxy "Proxy connection failed")
    (finally (firstboot_disable_proxy nil))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier1"]
              :dependsOnMethods ["firstboot_disable_proxy"]}}
  firstboot_test_proxy_with_blank_credentials
  "Test whether 'Test Connection' returns appropriate message when User and Password fields are empty"
  [_]
  (try
    (start_firstboot)
    (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
    (tasks/ui click :firstboot-window "proxy_button")
    (tasks/ui waittillguiexist :firstboot-proxy-dialog)
    (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
    (tasks/enableproxy "" :close? false :auth? true :user "" :pass "" :firstboot? true)
    (test_proxy "Proxy connection failed")
    (finally (firstboot_disable_proxy nil))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier2"]}}
  firstboot_test_bad_proxy
  "Tests the 'test connection' button when using a non-existant proxy."
  [_]
  (try+
   (start_firstboot)
   (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
   (tasks/ui click :firstboot-window "proxy_button")
   (tasks/ui waittillguiexist :firstboot-proxy-dialog)
   (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
   (tasks/enableproxy "doesnotexist.redhat.com" :close? false :firstboot? true)
   (test_proxy "Proxy connection failed")
   (finally (firstboot_disable_proxy nil))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier2"]}}
  firstboot_bad_proxy
  "Tests error message when using a non-existant proxy."
  [_]
  (try
    (start_firstboot)
    (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
    (tasks/ui click :firstboot-window "proxy_button")
    (tasks/ui waittillguiexist :firstboot-proxy-dialog)
    (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
    (let [hostname  "blahblah"
          port      "666"]
      (tasks/enableproxy hostname :port port :firstboot? true)
      (tasks/verify-conf-proxies hostname port "" ""))
    (let [thrown-error (try+ (do
                                (tasks/firstboot-register (@config :username) (@config :password))
                                (tasks/ui click :firstboot-forward)
                                (sleep 2000))
                             (catch Object e (:type e)))]
      (verify (= thrown-error :network-error)))
    (finally
     (firstboot_disable_proxy nil))))

(defn ^{Test {:groups ["firstboot_proxy"
                       "tier2"]}}
  firstboot_test_proxy_formatting
  "Tests the auto-formatting feature of the proxy location field."
  [_]
  (try+
   (start_firstboot)
   (verify (tasks/fbshowing? :firstboot-window "proxy_button"))
   (tasks/ui click :firstboot-window "proxy_button")
   (tasks/ui waittillguiexist :firstboot-proxy-dialog)
   (verify (bool (tasks/ui guiexist :firstboot-proxy-dialog)))
   (tasks/enableproxy "http://some.host.name:1337" :firstboot? true)
   ;;(tasks/ui click :configure-proxy)
   (tasks/ui click :firstboot-window "proxy_button")
   (tasks/ui waittillwindowexist :proxy-config-dialog 60)
   (let [location (tasks/ui gettextvalue :proxy-location)]
     (verify (not (substring? "http://" location)))
     (verify (= 2 (count (clojure.string/split location #":")))))
   (finally
     (tasks/ui click :close-proxy)
     (firstboot_disable_proxy nil))))

(gen-class-testng)
