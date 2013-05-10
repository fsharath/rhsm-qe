package rhsm.cli.tests;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.testng.annotations.AfterGroups;
import org.testng.annotations.BeforeGroups;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import com.redhat.qe.Assert;
import com.redhat.qe.auto.bugzilla.BlockedByBzBug;
import com.redhat.qe.auto.testng.TestNGUtils;
import rhsm.base.SubscriptionManagerCLITestScript;
import rhsm.cli.tasks.CandlepinTasks;
import rhsm.data.Org;
import com.redhat.qe.tools.SSHCommandResult;

/**
 * @author jsefler
 *
 *
 */
@Test(groups={"OrgsTests"})
public class OrgsTests extends SubscriptionManagerCLITestScript {

	// Test methods ***********************************************************************
	
	
	@Test(	description="subscription-manager: run the orgs module with valid user credentials and verify the expected organizations are listed",
			groups={"blockedByBug-719739"},
			dataProvider="getCredentialsForOrgsData",
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void OrgsWithCredentials_Test(String username, String password, List<Org> expectedOrgs) {
		log.info("Testing subscription-manager orgs module using username="+username+" password="+password+" and expecting orgs="+expectedOrgs+" ...");
		
		// use subscription-manager to get the organizations for which the user has access
		SSHCommandResult orgsResult = clienttasks.orgs_(username, password, null, null, null, null, null);
		
		// when the expectedOrgs is empty, there is a special message, assert it
		if (expectedOrgs.isEmpty()) {
			//Assert.assertEquals(String.format("%s cannot register to any organizations.", username), orgsResult.getStdout().trim(),"Special message when the expectedOrgs is empty.");	// Bug 903298 - String Update: "Register to" -> "Register with" 
			Assert.assertEquals(String.format("%s cannot register with any organizations.", username), orgsResult.getStdout().trim(),"Special message when the expectedOrgs is empty.");
		}
		
		// parse the actual Orgs from the orgsResult
		List<Org> actualOrgs = Org.parse(orgsResult.getStdout());
		
		// assert that all of the expectedOrgs are included in the actualOrgs
		for (Org expectedOrg : expectedOrgs) {
			Assert.assertTrue(actualOrgs.contains(expectedOrg), "The list of orgs returned by subscription-manager for user '"+username+"' includes expected org: "+expectedOrg);
		}
		Assert.assertEquals(actualOrgs.size(), expectedOrgs.size(),"The number of orgs returned by subscription-manager for user '"+username+"'.");
	}
	
	
	@Test(	description="subscription-manager: run the orgs module with invalid user credentials",
			groups={},
			dataProvider="getInvalidCredentialsForOrgsData",
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void OrgsWithInvalidCredentials_Test(String username, String password) {
		log.info("Testing subscription-manager orgs module using username="+username+" password="+password+" ...");
		
		// use subscription-manager to get the organizations for which the user has access
		SSHCommandResult sshCommandResult = clienttasks.orgs_(username, password, null, null, null, null, null);
		
		// assert the sshCommandResult here
		Assert.assertEquals(sshCommandResult.getExitCode(), Integer.valueOf(255), "The expected exit code from the orgs attempt.");
		Assert.assertEquals(sshCommandResult.getStdout().trim(), "", "The expected stdout result from orgs.");
		Assert.assertContainsMatch(sshCommandResult.getStderr().trim(), servertasks.invalidCredentialsRegexMsg(), "The expected stderr result from orgs.");
	}
	
	
	@Test(	description="subscription-manager: run the orgs module while prompting for user credentials interactively",
			groups={"blockedbyBug-878986"},
			dataProvider = "getInteractiveCredentialsForOrgsData",
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void OrgsWithInteractivePromptingForCredentials_Test(Object bugzilla, String promptedUsername, String promptedPassword, String commandLineUsername, String commandLinePassword, Integer expectedExitCode, String expectedStdoutRegex, String expectedStderrRegex) {

		// call orgs while providing a valid username at the interactive prompt
		String command;
		if (client.runCommandAndWait("rpm -q expect").getExitCode().intValue()==0) {	// is expect installed?
			// assemble an ssh command using expect to simulate an interactive supply of credentials to the orgs command
			String promptedUsernames=""; if (promptedUsername!=null) for (String username : promptedUsername.split("\\n")) {
				promptedUsernames += "expect \\\"*Username:\\\"; send "+username+"\\\r;";
			}
			String promptedPasswords=""; if (promptedPassword!=null) for (String password : promptedPassword.split("\\n")) {
				promptedPasswords += "expect \\\"*Password:\\\"; send "+password+"\\\r;";
			}
			// [root@jsefler-onprem-5server ~]# expect -c "spawn subscription-manager environments orgs; expect \"*Username:\"; send qa@redhat.com\r; expect \"*Password:\"; send CHANGE-ME\r; expect eof; catch wait reason; exit [lindex \$reason 3]"
			command = String.format("expect -c \"spawn %s orgs %s %s; %s %s expect eof; catch wait reason; exit [lindex \\$reason 3]\"",
					clienttasks.command,
					commandLineUsername==null?"":"--username="+commandLineUsername,
					commandLinePassword==null?"":"--password="+commandLinePassword,
					promptedUsernames,
					promptedPasswords);
		} else {
			// assemble an ssh command using echo and pipe to simulate an interactive supply of credentials to the orgs command
			String echoUsername= promptedUsername==null?"":promptedUsername;
			String echoPassword = promptedPassword==null?"":promptedPassword;
			String n = (promptedPassword!=null&&promptedUsername!=null)? "\n":"";
			command = String.format("echo -e \"%s\" | %s orgs %s %s",
					echoUsername+n+echoPassword,
					clienttasks.command,
					commandLineUsername==null?"":"--username="+commandLineUsername,
					commandLinePassword==null?"":"--password="+commandLinePassword);
		}
		// attempt orgs with the interactive credentials
		SSHCommandResult sshCommandResult = client.runCommandAndWait(command);
		
		// assert the sshCommandResult here
		if (expectedExitCode!=null) Assert.assertEquals(sshCommandResult.getExitCode(), expectedExitCode, "The expected exit code from the orgs attempt.");
		if (expectedStdoutRegex!=null) Assert.assertContainsMatch(sshCommandResult.getStdout(), expectedStdoutRegex, "The expected stdout result from orgs while supplying interactive credentials.");
		if (expectedStderrRegex!=null) Assert.assertContainsMatch(sshCommandResult.getStderr(), expectedStderrRegex, "The expected stderr result from orgs while supplying interactive credentials.");
	}
	
	
	@Test(	description="subscription-manager: attempt to get a bogus owner via the candlepin api",
			groups={"blockedByBug-729780","blockedByBug-796468"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void AttemptToGetABogusOwnerViaCandlepinApi_Test() throws Exception {
		String bogusOwner = "bogusOwner";
		JSONObject jsonResponse = new JSONObject(CandlepinTasks.getResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/owners/"+bogusOwner));
		
		//Assert.assertEquals(jsonResponse.getString("displayMessage"), "Owner with id "+bogusOwner+" could not be found", "The JSON returned from the candlepin API call should contain a displayMessage stating that the owner/org could not be found.");	// valid prior to Bug 796468 - Owner with id FOO could not be found
		Assert.assertEquals(jsonResponse.getString("displayMessage"), String.format("Organization with id %s could not be found.",bogusOwner), "The JSON returned from the candlepin API call should contain a displayMessage stating that the owner/org could not be found.");
		
		// TODO could also use the RemoteFileTasks to mark the catalina.out file and assert that it contains a 404 response instead of the 403 response as reported in:
		// https://bugzilla.redhat.com/show_bug.cgi?id=729780
		// https://bugzilla.redhat.com/attachment.cgi?id=517680
//		2011-08-10 18:00:31,106 INFO  [STDOUT] Aug 10 18:00:31 [http-10.7.13.82-8080-1] DEBUG org.fedoraproject.candlepin.guice.I18nProvider - Getting i18n engine for locale en_US
//		2011-08-10 18:00:31,154 INFO  [STDOUT] Aug 10 18:00:31 [http-10.7.13.82-8080-1] DEBUG org.fedoraproject.candlepin.servlet.filter.logging.LoggingFilter -
//		====Response====
//		  Status: 403
//		  Content-type: application/json
//		====Response====
//		2011-08-10 18:00:31,154 INFO  [STDOUT] Aug 10 18:00:31 [http-10.7.13.82-8080-1] DEBUG org.fedoraproject.candlepin.servlet.filter.logging.LoggingFilter - ====ResponseBody====
//		2011-08-10 18:00:31,154 INFO  [STDOUT] Aug 10 18:00:31 [http-10.7.13.82-8080-1] DEBUG org.fedoraproject.candlepin.servlet.filter.logging.LoggingFilter - {"displayMessage":"Insufficient permissions"}
	}
	
		
	protected String server_hostname = null;
	protected String server_port = null;
	protected String server_prefix = null;
	@BeforeGroups(value={"OrgsWithServerurl_Test"}, groups={"setup"})
	public void beforeOrgsWithServerurl_Test() {
		if (clienttasks==null) return;
		server_hostname	= clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "server", "hostname");
		server_port		= clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "server", "port");
		server_prefix	= clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "server", "prefix");
	}
	@Test(	description="subscription-manager: orgs with --serverurl",
			dataProvider="getServerurl_TestData",
			groups={"OrgsWithServerurl_Test"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void OrgsWithServerurl_Test(Object bugzilla, String serverurl, String expectedHostname, String expectedPort, String expectedPrefix, Integer expectedExitCode, String expectedStdoutRegex, String expectedStderrMatch) {
		// get original server at the beginning of this test
		String hostnameBeforeTest	= clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "hostname");
		String portBeforeTest		= clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "port");
		String prefixBeforeTest		= clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "prefix");
		
		// orgs with a serverurl
		SSHCommandResult sshCommandResult = clienttasks.orgs_(sm_clientUsername,sm_clientPassword,serverurl, null, null, null, null);
		
		// assert the sshCommandResult here
		if (expectedExitCode!=null)	Assert.assertEquals(sshCommandResult.getExitCode(), expectedExitCode,"ExitCode after register with --serverurl="+serverurl+" and other options:");
		if (expectedStdoutRegex!=null)	Assert.assertContainsMatch(sshCommandResult.getStdout().trim(), expectedStdoutRegex,"Stdout after register with --serverurl="+serverurl+" and other options:");
		if (expectedStderrMatch!=null)	Assert.assertContainsMatch(sshCommandResult.getStderr().trim(), expectedStderrMatch,"Stderr after register with --serverurl="+serverurl+" and other options:");
		Assert.assertContainsNoMatch(sshCommandResult.getStderr().trim(), "Traceback.*","Stderr after register with --serverurl="+serverurl+" and other options should not contain a Traceback.");
		
		// negative testcase assertions........
		if (expectedExitCode.equals(new Integer(255))) {
			// assert that the current config remains unchanged when the expectedExitCode is 255
			Assert.assertEquals(clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "hostname"), hostnameBeforeTest, "The "+clienttasks.rhsmConfFile+" configuration for [server] hostname should remain unchanged when attempting to register with an invalid serverurl.");
			Assert.assertEquals(clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "port"),	portBeforeTest, "The "+clienttasks.rhsmConfFile+" configuration for [server] port should remain unchanged when attempting to register with an invalid serverurl.");
			Assert.assertEquals(clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "prefix"), prefixBeforeTest, "The "+clienttasks.rhsmConfFile+" configuration for [server] prefix should remain unchanged when attempting to register with an invalid serverurl.");
						
			return;	// nothing more to do after these negative testcase assertions
		}
		
		// positive testcase assertions........
		Assert.assertEquals(clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "hostname"), expectedHostname, "The "+clienttasks.rhsmConfFile+" configuration for [server] hostname has been updated from the specified --serverurl "+serverurl);
		Assert.assertEquals(clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "port"), expectedPort, "The "+clienttasks.rhsmConfFile+" configuration for [server] port has been updated from the specified --serverurl "+serverurl);
		Assert.assertEquals(clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "prefix"), expectedPrefix, "The "+clienttasks.rhsmConfFile+" configuration for [server] prefix has been updated from the specified --serverurl "+serverurl);
	}
	@AfterGroups(value={"OrgsWithServerurl_Test"},groups={"setup"})
	public void afterOrgsWithServerurl_Test() {
		if (server_hostname!=null)	clienttasks.config(null,null,true,new String[]{"server","hostname",server_hostname});
		if (server_port!=null)		clienttasks.config(null,null,true,new String[]{"server","port",server_port});
		if (server_prefix!=null)	clienttasks.config(null,null,true,new String[]{"server","prefix",server_prefix});
	}
	
	
	protected String server_ca_cert_dir = null;
	@BeforeGroups(value={"OrgsWithInsecure_Test"}, groups={"setup"})
	public void beforeOrgsWithInsecure_Test() {
		if (clienttasks==null) return;
		server_ca_cert_dir	= clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "server", "ca_cert_dir");
	}
	@Test(	description="subscription-manager: orgs with --insecure",
			groups={"OrgsWithInsecure_Test","blockedByBug-844411"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void OrgsWithInsecure_Test() {
		SSHCommandResult sshCommandResult;
		
		// calling orgs without insecure should pass
		sshCommandResult = clienttasks.orgs(sm_clientUsername,sm_clientPassword, null, false, null, null, null);
		
		// change the server.ca_cert_dir configuration to simulate a missing candlepin ca cert
		sshCommandResult = clienttasks.config(null, null, true, new String[]{"server","ca_cert_dir",clienttasks.factsDir});
		
		// calling orgs without insecure should now fail (throwing stderr "certificate verify failed")
		sshCommandResult = clienttasks.orgs_(sm_clientUsername,sm_clientPassword, null, false, null, null, null);
		Assert.assertEquals(sshCommandResult.getStderr().trim(), "certificate verify failed", "Stderr from the orgs command when configuration server.ca_cert_dir has been falsified.");
		Assert.assertEquals(sshCommandResult.getStdout().trim(), "", "Stdout from the orgs command when configuration server.ca_cert_dir has been falsified.");
		Assert.assertEquals(sshCommandResult.getExitCode(), Integer.valueOf(255), "Exitcode from the orgs command when configuration server.ca_cert_dir has been falsified.");
		
		// calling orgs with insecure should now pass
		sshCommandResult = clienttasks.orgs(sm_clientUsername,sm_clientPassword, null, true, null, null, null);
		
		// assert that option --insecure did NOT persist to rhsm.conf
		Assert.assertEquals(clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "server", "insecure"), "0", "Expected value of "+clienttasks.rhsmConfFile+" server.insecure configuration.  Use of the --insecure option when calling the orgs module should NOT be persisted to rhsm.conf as true.");
	}
	@AfterGroups(value={"OrgsWithInsecure_Test"},groups={"setup"})
	public void afterOrgsWithInsecure_Test() {
		clienttasks.config(null, null, true, new String[]{"server","ca_cert_dir",server_ca_cert_dir});
	}
	
	
	
	
	
	
		
	// Candidates for an automated Test:
	// TODO Bug 789344 - subscription-manager doesn't support spaces in --org parameter unless --environment parameter is given https://github.com/RedHatQE/rhsm-qe/issues/178
	
	// Configuration methods ***********************************************************************
	
	
	
	// Protected methods ***********************************************************************


	
	// Data Providers ***********************************************************************

	
	@DataProvider(name="getCredentialsForOrgsData")
	public Object[][] getCredentialsForOrgsDataAs2dArray() throws JSONException, Exception {
		return TestNGUtils.convertListOfListsTo2dArray(getCredentialsForOrgsDataAsListOfLists());
	}
	protected List<List<Object>> getCredentialsForOrgsDataAsListOfLists() throws JSONException, Exception {
		List<List<Object>> ll = new ArrayList<List<Object>>(); if (!isSetupBeforeSuiteComplete) return ll;
		// Notes...
		// curl -k -u admin:admin https://jsefler-onprem-62candlepin.usersys.redhat.com:8443/candlepin/users | python -mjson.tool
		// curl -k -u admin:admin https://jsefler-onprem-62candlepin.usersys.redhat.com:8443/candlepin/users/testuser1 | python -mjson.tool
		// curl -k -u admin:admin https://jsefler-onprem-62candlepin.usersys.redhat.com:8443/candlepin/users/testuser1/owners | python -mjson.tool

		// get all of the candlepin users
		// curl -k -u admin:admin https://jsefler-onprem-62candlepin.usersys.redhat.com:8443/candlepin/users | python -mjson.tool
		JSONArray jsonUsers = new JSONArray(CandlepinTasks.getResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword,sm_serverUrl,"/users"));	
		for (int i = 0; i < jsonUsers.length(); i++) {
			JSONObject jsonUser = (JSONObject) jsonUsers.get(i);
			// {
			//   "created": "2011-07-01T06:40:00.951+0000", 
			//   "hashedPassword": "05557a2aaec7cb676df574d2eb080691949a6752", 
			//   "id": "8a90f8c630e46c7e0130e46ce9b70020", 
			//   "superAdmin": false, 
			//   "updated": "2011-07-01T06:40:00.951+0000", 
			//   "username": "minnie"
			// }
			Boolean isSuperAdmin = jsonUser.getBoolean("superAdmin");
			String username = jsonUser.getString("username");
			String password = sm_clientPasswordDefault;
			if (username.equals(sm_serverAdminUsername)) password = sm_serverAdminPassword;
			
			// get the user's owners
			// curl -k -u testuser1:password https://jsefler-onprem-62candlepin.usersys.redhat.com:8443/candlepin/users/testuser1/owners | python -mjson.tool
			JSONArray jsonUserOwners = new JSONArray(CandlepinTasks.getResourceUsingRESTfulAPI(username,password,sm_serverUrl,"/users/"+username+"/owners"));	
			List<Org> orgs = new ArrayList<Org>();
			for (int j = 0; j < jsonUserOwners.length(); j++) {
				JSONObject jsonOwner = (JSONObject) jsonUserOwners.get(j);
				// {
				//    "contentPrefix": null, 
				//    "created": "2011-07-01T06:39:58.740+0000", 
				//    "displayName": "Snow White", 
				//    "href": "/owners/snowwhite", 
				//    "id": "8a90f8c630e46c7e0130e46ce114000a", 
				//    "key": "snowwhite", 
				//    "parentOwner": null, 
				//    "updated": "2011-07-01T06:39:58.740+0000", 
				//    "upstreamUuid": null
				// }
				String orgKey = jsonOwner.getString("key");
				String orgName = jsonOwner.getString("displayName");
				orgs.add(new Org(orgKey,orgName));
			}
			
			// String username, String password, List<Org> orgs
			ll.add(Arrays.asList(new Object[]{username,password,orgs}));
		}
		
		return ll;
	}
	
	
	@DataProvider(name="getInvalidCredentialsForOrgsData")
	public Object[][] getInvalidCredentialsForOrgsDataAs2dArray() {
		return TestNGUtils.convertListOfListsTo2dArray(getInvalidCredentialsForOrgsDataAsListOfLists());
	}
	protected List<List<Object>> getInvalidCredentialsForOrgsDataAsListOfLists() {
		List<List<Object>> ll = new ArrayList<List<Object>>(); if (!isSetupBeforeSuiteComplete) return ll;
		String x = String.valueOf(getRandInt());
		
		// String username, String password
		ll.add(Arrays.asList(new Object[]{	sm_clientUsername+x,	sm_clientPassword}));
		ll.add(Arrays.asList(new Object[]{	sm_clientUsername,		sm_clientPassword+x}));
		ll.add(Arrays.asList(new Object[]{	sm_clientUsername+x,	sm_clientPassword+x}));
		
		return ll;
	}
	
	
	@DataProvider(name="getInteractiveCredentialsForOrgsData")
	public Object[][] getInteractiveCredentialsForOrgsDataAs2dArray() {
		return TestNGUtils.convertListOfListsTo2dArray(getInteractiveCredentialsForOrgsDataAsListOfLists());
	}
	protected List<List<Object>> getInteractiveCredentialsForOrgsDataAsListOfLists() {
		List<List<Object>> ll = new ArrayList<List<Object>>(); if (!isSetupBeforeSuiteComplete) return ll;
		if (servertasks==null) return ll;
		if (clienttasks==null) return ll;
		
		String uErrMsg = servertasks.invalidCredentialsRegexMsg();
		String x = String.valueOf(getRandInt());
		if (client.runCommandAndWait("rpm -q expect").getExitCode().intValue()==0) {	// is expect installed?
			// Object bugzilla, String promptedUsername, String promptedPassword, String commandLineUsername, String commandLinePassword, Integer expectedExitCode, String expectedStdoutRegex, String expectedStderrRegex
			ll.add(Arrays.asList(new Object[] {	null,	sm_clientUsername,			null,						null,				sm_clientPassword,	new Integer(0),		sm_clientUsername+" Organizations",				null}));
			ll.add(Arrays.asList(new Object[] {	null,	sm_clientUsername+x,		null,						null,				sm_clientPassword,	new Integer(255),	uErrMsg,										null}));
			ll.add(Arrays.asList(new Object[] {	null,	null,						sm_clientPassword,			sm_clientUsername,	null,				new Integer(0),		sm_clientUsername+" Organizations",				null}));
			ll.add(Arrays.asList(new Object[] {	null,	null,						sm_clientPassword+x,		sm_clientUsername,	null,				new Integer(255),	uErrMsg,										null}));
			ll.add(Arrays.asList(new Object[] {	null,	sm_clientUsername,			sm_clientPassword,			null,				null,				new Integer(0),		sm_clientUsername+" Organizations",				null}));
			ll.add(Arrays.asList(new Object[] {	null,	sm_clientUsername+x,		sm_clientPassword+x,		null,				null,				new Integer(255),	uErrMsg,										null}));
			ll.add(Arrays.asList(new Object[] {	null,	"\n\n"+sm_clientUsername,	"\n\n"+sm_clientPassword,	null,				null,				new Integer(0),		"(\nUsername: ){3}"+sm_clientUsername+"(\nPassword: ){3}"+"\\n\\+-+\\+\\n"+" *"+sm_clientUsername+" Organizations",	null}));
		} else {
			// Object bugzilla, String promptedUsername, String promptedPassword, String commandLineUsername, String commandLinePassword, Integer expectedExitCode, String expectedStdoutRegex, String expectedStderrRegex
			ll.add(Arrays.asList(new Object[] {	null,	sm_clientUsername,			null,						null,				sm_clientPassword,	new Integer(0),		sm_clientUsername+" Organizations",				null}));
			ll.add(Arrays.asList(new Object[] {	null,	sm_clientUsername+x,		null,						null,				sm_clientPassword,	new Integer(255),	null,																		uErrMsg}));
			ll.add(Arrays.asList(new Object[] {	null,	null,						sm_clientPassword,			sm_clientUsername,	null,				new Integer(0),		sm_clientUsername+" Organizations",				null}));
			ll.add(Arrays.asList(new Object[] {	null,	null,						sm_clientPassword+x,		sm_clientUsername,	null,				new Integer(255),	null,																		uErrMsg}));
			ll.add(Arrays.asList(new Object[] {	null,	sm_clientUsername,			sm_clientPassword,			null,				null,				new Integer(0),		sm_clientUsername+" Organizations",				null}));
			ll.add(Arrays.asList(new Object[] {	null,	sm_clientUsername+x,		sm_clientPassword+x,		null,				null,				new Integer(255),	null,																		uErrMsg}));
			ll.add(Arrays.asList(new Object[] {	null,	"\n\n"+sm_clientUsername,	"\n\n"+sm_clientPassword,	null,				null,				new Integer(0),		"(Username: ){3}.*\\n.*"+sm_clientUsername+" Organizations",	"(Warning: Password input may be echoed.\nPassword: \n){3}"}));
		}
		return ll;
	}

}
