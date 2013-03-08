package rhsm.cli.tests;

import java.io.File;
import java.io.IOException;

import java.math.BigInteger;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.testng.SkipException;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterGroups;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeGroups;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import com.redhat.qe.Assert;
import com.redhat.qe.auto.tcms.ImplementsNitrateTest;
import com.redhat.qe.auto.testng.TestNGUtils;

import rhsm.base.CandlepinType;
import rhsm.base.ConsumerType;
import rhsm.base.SubscriptionManagerCLITestScript;
import rhsm.cli.tasks.CandlepinTasks;
import rhsm.data.ConsumerCert;
import rhsm.data.ContentNamespace;
import rhsm.data.EntitlementCert;
import rhsm.data.InstalledProduct;
import rhsm.data.OrderNamespace;
import rhsm.data.ProductCert;
import rhsm.data.ProductSubscription;
import rhsm.data.Repo;
import rhsm.data.SubscriptionPool;
import rhsm.data.YumRepo;
import com.redhat.qe.tools.RemoteFileTasks;
import com.redhat.qe.tools.SSHCommandResult;
import com.redhat.qe.tools.SSHCommandRunner;

/**
 * @author skallesh
 * 
 * 
 */
@Test(groups = { "BugzillaTests" })
public class BugzillaTests extends SubscriptionManagerCLITestScript {
	protected String ownerKey="";
	protected String randomAvailableProductId;
	protected EntitlementCert expiringCert = null;
	protected String EndingDate;
	protected final String importCertificatesDir = "/tmp/sm-importExpiredCertificatesDir"
			.toLowerCase();
	String factname="system.entitlements_valid";
	protected String RemoteServerError="Remote server error. Please check the connection details, or see /var/log/rhsm/rhsm.log for more information.";
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if CLI lets you set consumer nameto empty string and defaults to username", 
			groups = { "VerifyConsumerNameTest","blockedByBug-669395"}, enabled = true)
		public void VerifyConsumerNameTest() throws JSONException,Exception {
		String consumerName="tester";
		clienttasks.register(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String result=clienttasks.identity(null, null, null, null, null, null, null).getStdout();
		Assert.assertContainsMatch(result, "name: "+clienttasks.hostname);
		clienttasks.register(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, consumerName, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		result=clienttasks.identity(null, null, null, null, null, null, null).getStdout();
		String expected="name: "+consumerName;
		Assert.assertContainsMatch(result, expected);
		String consumerId=clienttasks.getCurrentConsumerId();
		clienttasks.clean(null, null, null);
		consumerName="consumer";
		clienttasks.register(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, consumerName, consumerId, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		result=clienttasks.identity(null, null, null, null, null, null, null).getStdout();
		Assert.assertContainsMatch(result, expected);
		clienttasks.clean(null, null, null);
		result=clienttasks.register_(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, "", consumerId, null, null, null,
				(String) null, null, null, null, true, null, null, null, null).getStdout();
		Assert.assertEquals(result.trim(), "Error: system name can not be empty.");
		result=clienttasks.register_(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, "", null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null).getStdout();
		Assert.assertEquals(result.trim(), "Error: system name can not be empty.");
	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if CLI auto-subscribe tries to re-use basic auth credentials.", 
			groups = { "VerifyAutosubscribeReuseBasicAuthCredntials","blockedByBug-707641"}, enabled = true)
		public void VerifyAutosubscribeReuseBasicAuthCredntials() throws JSONException,Exception {
		String LogMarker = System.currentTimeMillis()+" Testing ***************************************************************";
		RemoteFileTasks.markFile(client, CandlepinTasks.tomcat6LogFile, LogMarker);
		clienttasks.register(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);	
		String logMessage=" Authentication check for /consumers/"+clienttasks.getCurrentConsumerId()+"/entitlements";
		Assert.assertTrue(RemoteFileTasks.getTailFromMarkedFile(client,CandlepinTasks.tomcat6LogFile, LogMarker, logMessage).trim().equals(""));

	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	//To be tested against stage
	@Test(description = "verify if 500 errors in stage on subscribe/unsubscribe", 
			groups = { "Verify500ErrorOnStage","blockedByBug-878994"}, enabled = true)
		public void Verify500ErrorOnStage() throws JSONException,Exception {
		String logMessage = "remote server status code: 500";
		String serverurl="subscription.rhn.stage.redhat.com:443/subscription";
		clienttasks.register(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, serverurl, null, null, true, null, null, null, null).getStdout();	
		String LogMarker = System.currentTimeMillis()+" Testing ***************************************************************";
		RemoteFileTasks.markFile(client, clienttasks.rhsmLogFile, LogMarker);
		String result=clienttasks.listAvailableSubscriptionPools().getStdout();
		Assert.assertTrue(RemoteFileTasks.getTailFromMarkedFile(client, clienttasks.rhsmLogFile, LogMarker, logMessage).trim().equals(""));
		Assert.assertNoMatch(result.trim(), RemoteServerError);
		RemoteFileTasks.markFile(client, clienttasks.rhsmLogFile, LogMarker);
		result=clienttasks.subscribe(true,(String)null,(String)null,(String)null, null, null, null, null, null, null, null).getStdout();
		Assert.assertTrue(RemoteFileTasks.getTailFromMarkedFile(client, clienttasks.rhsmLogFile, LogMarker, logMessage).trim().equals(""));
		Assert.assertNoMatch(result.trim(), RemoteServerError);
		RemoteFileTasks.markFile(client, clienttasks.rhsmLogFile, LogMarker);
		result=clienttasks.unregister(null, null, null).getStdout();
		Assert.assertTrue(RemoteFileTasks.getTailFromMarkedFile(client, clienttasks.rhsmLogFile, LogMarker, logMessage).trim().equals(""));
		Assert.assertNoMatch(result.trim(), RemoteServerError);
	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if redhat repo is created subscription-manager yum plugin when the repo is not present", 
			groups = { "RedhatrepoNotBeingCreated","blockedByBug-886992"}, enabled = true)
		public void RedhatrepoNotBeingCreated() throws JSONException,Exception {
		client.runCommandAndWait("mv /etc/yum.repos.d/redhat.repo /root/").getStdout();
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsm",
				"manage_repos".toLowerCase(), "1" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		clienttasks.register(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, null, null, null, null, null);
		String redhatrepoExists=client.runCommandAndWait("ls /etc/yum.repos.d/redhat.repo").getStdout();
		Assert.assertEquals(redhatrepoExists.trim(), "/etc/yum.repos.d/redhat.repo");
		String result=client.runCommandAndWait("yum repolist all").getStdout();

		for(YumRepo originalRepos :clienttasks.getCurrentlySubscribedYumRepos()){
			System.out.println(originalRepos.id);
			Assert.assertNotNull(originalRepos.id);
			Boolean flag=false;
				Pattern pattern = Pattern.compile(originalRepos.id, Pattern.MULTILINE);
				Matcher matcher = pattern.matcher(result);
				if (matcher.find()) {
					flag=true;
				}
				Assert.assertTrue(flag);
				flag=false;
			}
		
	}
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if  insecure in rhsm.comf getse updated when using --insecure option if command fails", 
			groups = { "InsecureValueInRHSMConfAfterRegistrationFailure","blockedByBug-916369"}, enabled = true)
		public void InsecureValueInRHSMConfAfterRegistrationFailure() throws JSONException,Exception {
		String defaultHostname = "rhel7.com";
		String defaultPort = "8443";
		String defaultPrefix = "/candlepin";
		String org="foo";
		String valueBeforeRegister=clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "insecure");
		clienttasks.register(sm_clientUsername, sm_clientPassword,org, null, null, null, null, null, null, null,(String) null,defaultHostname+":"+defaultPort+"/"+defaultPrefix, null, null, null, null, null, null, null);
		String valueAfterRegister = clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "insecure");
		Assert.assertEquals(valueBeforeRegister, valueAfterRegister);
	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if subscription-manager register fails with consumerid and activationkey specified", 
			groups = { "RegisterActivationKeyAndConsumerID","blockedByBug-749636"}, enabled = true)
		public void RegisterActivationKeyAndConsumerID() throws JSONException,Exception {
		String name = String.format("%s_%s-ActivationKey%s", sm_clientUsername,
				sm_clientOrg, System.currentTimeMillis());
		Map<String, String> mapActivationKeyRequest = new HashMap<String, String>();
		mapActivationKeyRequest.put("name", name);
		JSONObject jsonActivationKeyRequest = new JSONObject(
				mapActivationKeyRequest);
		JSONObject jsonActivationKey = new JSONObject(
				CandlepinTasks.postResourceUsingRESTfulAPI(sm_clientUsername,
						sm_clientPassword, sm_serverUrl, "/owners/"
								+ sm_clientOrg + "/activation_keys",
								jsonActivationKeyRequest.toString()));
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, null, null, null, null, null);
		String consumerID=clienttasks.getCurrentConsumerId();
		clienttasks.unregister(null, null, null);
		String result=clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, consumerID, null, null, null,
				jsonActivationKey.get("name").toString(), null, null, null, null, null, null, null, null).getStdout();
		String expected="Error: Activation keys do not require user credentials.";
		Assert.assertEquals(result.trim(), expected);
		
		
	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if Product id is displayed in intsalled list", 
			groups = { "ProductIdInInstalledList","blockedByBug-803386"}, enabled = true)
		public void ProductIdInInstalledList() throws JSONException,Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, null, null, null, null, null);
		for(InstalledProduct result:clienttasks.getCurrentlyInstalledProducts()){
			Assert.assertNotNull(result.productId);
			
		}
		
	}
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if Entitlement certs are downloaded if subscribed to expired pool", 
			groups = { "ServerURLInRHSMFile","blockedByBug-916353"}, enabled = true)
		public void ServerURLInRHSMFile() throws JSONException,Exception {
		String defaultHostname = "rhel7.com";
		String defaultPort = "8443";
		String defaultPrefix = "/candlepin";
		String org="foo";
		String valueBeforeRegister=clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "hostname");
		clienttasks.register_(sm_clientUsername, sm_clientPassword,org, null, null, null, null, null, null, null,(String) null,defaultHostname+":"+defaultPort+"/"+defaultPrefix, null, null, null, null, null, null, null);
		String valueAfterRegister = clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "hostname");
		Assert.assertEquals(valueBeforeRegister, valueAfterRegister);
		
	}
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if Entitlement certs are downloaded if subscribed to expired pool", 
			groups = { "DipslayServicelevelWhenRegisteredToDiffrentMachine","blockedByBug-916362"}, enabled = true)
		public void DipslayServicelevelWhenRegisteredToDiffrentMachine() throws JSONException,Exception {
		String defaultHostname = "subscription.rhn.redhat.com";
		String defaultPort = "443";
		String defaultPrefix = "/subscription";
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, null, null, null, null, null);
		String result=clienttasks.service_level_(null, null, null, null, null, null, null,defaultHostname+":"+defaultPort+"/"+defaultPrefix , null, null, null, null).getStdout();
		Assert.assertEquals(result.trim(), "You are already registered to a different system");
	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if Entitlement certs are downloaded if subscribed to expired pool", 
			groups = { "ExpirationOfEntitlementCerts","blockedByBug-907638"}, enabled = true)
		public void ExpirationOfEntitlementCerts() throws JSONException,Exception {
		int endingMinutesFromNow = 2;
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"certCheckInterval".toLowerCase(), "1" });
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, null, null, null, null, null);
		String productId=null;
		clienttasks.unsubscribeFromAllOfTheCurrentlyConsumedProductSubscriptions();
		String consumerId = clienttasks.getCurrentConsumerId();
		ownerKey = CandlepinTasks.getOwnerKeyOfConsumerId(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, consumerId);
		String expiringPoolId = createTestPool(-60*24,endingMinutesFromNow);
		for(SubscriptionPool pool:clienttasks.getCurrentlyAvailableSubscriptionPools()){
			if(pool.poolId.equals(expiringPoolId)){
				productId=pool.subscriptionName;
			}
		}
		clienttasks.subscribe(null, null, expiringPoolId, null, null, null, null, null, null, null, null).getStdout();
		sleep(2*60*1000);
		for(Repo originalRepos :clienttasks.getCurrentlySubscribedRepos()){
			Assert.assertNotSame(originalRepos.repoId, randomAvailableProductId);
		}
		clienttasks.unsubscribeFromAllOfTheCurrentlyConsumedProductSubscriptions();
		String result=clienttasks.subscribe_(null, null, expiringPoolId, null, null, null, null, null, null, null, null).getStdout();	
		String expected="Unable to entitle consumer to the pool with id '"+expiringPoolId+"'.: Subscriptions for "+productId+" expired on: "+EndingDate;
		Assert.assertEquals(result.trim(), expected);
}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if Entitlement certs are downloaded if subscribed to expired pool", 
			groups = { "SubscribeToexpiredEntitlement","blockedByBug-907638"}, enabled = true)
		public void SubscribeToexpiredEntitlement() throws JSONException,Exception {
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"certCheckInterval".toLowerCase(), "1" });
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);

		int endingMinutesFromNow = 1;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, null, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		ownerKey = CandlepinTasks.getOwnerKeyOfConsumerId(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, consumerId);
		String expiringPoolId = createTestPool(-60*24,endingMinutesFromNow);
		sleep(1*60*1000);
		clienttasks.subscribe_(null, null, expiringPoolId, null, null, null, null, null, null, null, null);
		Assert.assertTrue(clienttasks.getCurrentlyConsumedProductSubscriptions().isEmpty());
		Assert.assertTrue(clienttasks.getCurrentEntitlementCertFiles().isEmpty());
		}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if imported certs are deleted after registering", 
			groups = { "EntitlementCertShouldNotBeDeleted"}, enabled = true)
		public void EntitlementCertShouldNotBeDeleted() throws JSONException,Exception {
		clienttasks.unregister(null, null, null);
		File expectCertFile = new File(System.getProperty("automation.dir",null) + "/expiredcerts/CertV3.pem");
		RemoteFileTasks.putFile(client.getConnection(),expectCertFile.toString(), "/root/", "0755");
		clienttasks.importCertificate_("/root/CertV3.pem");
		List<File> BeforeRegister=clienttasks.getCurrentEntitlementCertFiles();
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, null, null, null, null, null);
		List<File> AfterRegister=clienttasks.getCurrentEntitlementCertFiles();
		Assert.assertEquals(BeforeRegister, AfterRegister);
	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if no multiple repos are created,if subscribed to a product that share one or more engineering subscriptions", 
			groups = { "NoMultipleReposCreated"}, enabled = true)
		public void NoMultipleReposCreated() throws JSONException,Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		ownerKey = CandlepinTasks.getOwnerKeyOfConsumerId(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, consumerId);
		Calendar now = new GregorianCalendar();
		CandlepinTasks.deleteSubscriptionsAndRefreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, sm_clientOrg,"multi-stackable");
		CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/products/" + "multi-stackable");
		Date onDate = now.getTime();
		now.add(Calendar.YEAR, 1);
		now.add(Calendar.DATE, 1);
		Date onDateToTest = now.getTime();
		Map<String,String> attributes = new HashMap<String,String>();
		attributes.put("sockets", "8");
		attributes.put("arch", "ALL");
		attributes.put("type", "MKT");
		attributes.put("multi-entitlement", "yes");
		attributes.put("stacking_id", "726409");
		List<String> providedProducts = new ArrayList<String>();
		providedProducts.add("100000000000002");
		providedProducts.add("100000000000001");
		CandlepinTasks.createProductUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"Multi-Stackable","multi-stackable", 1,attributes ,null);
		String requestBody = CandlepinTasks.createSubscriptionRequestBody(20, onDate, onDateToTest,"multi-stackable", Integer.valueOf(getRandInt()), Integer.valueOf(getRandInt()), providedProducts).toString();
		CandlepinTasks.postResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/owners/" + ownerKey + "/subscriptions", requestBody);	
		JSONObject jobDetail = CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		CandlepinTasks.waitForJobDetailStateUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword,sm_serverUrl,jobDetail,"FINISHED", 5*1000, 1);
		sleep(3*60*1000);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		for(SubscriptionPool pools:clienttasks.getCurrentlyAvailableSubscriptionPools()){
			if(pools.subscriptionName.equals("Multi-Stackable")){
			clienttasks.subscribe(null, null,pools.poolId, null, null, null, null, null, null, null, null);	
			}
		}
		String productIdOne=null;
		List<Repo> originalRepos =clienttasks.getCurrentlySubscribedRepos();
		for (Repo repo : originalRepos) {
			String productIdTwo=null;
			productIdOne=repo.repoId;
			if(!(productIdTwo==null)){
			Assert.assertNotSame(repo.repoId, productIdOne);
			}
			productIdTwo=productIdOne;
		}
		CandlepinTasks.deleteSubscriptionsAndRefreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, sm_clientOrg,"multi-stackable");
		CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/products/" + "multi-stackable");
			
		}

				
	
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if bind and unbind event is recorded in syslog", 
			groups = { "DeleteContentSourceFromProduct","blockedByBug-687970"}, enabled = true)
	public void DeleteContentSourceFromProduct() throws JSONException,Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		 List<String> modifiedProductIds=null;
		 String contentId= "99999";
		 Map<String,String> attributes = new HashMap<String,String>();
		 attributes.put("sockets", "8");
		 attributes.put("arch", "ALL");
		String requestBody = CandlepinTasks.createContentRequestBody("fooname", contentId, "foolabel", "yum", "Foo Vendor", "/foo/path", "/foo/path/gpg", null, null, modifiedProductIds).toString();
		CandlepinTasks.postResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl,"/content", requestBody);	
		JSONObject jsonActivationKey = new JSONObject(CandlepinTasks.getResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/content/"+contentId));
		Assert.assertContainsNoMatch(jsonActivationKey.toString(), "Content with id "+contentId+" could not be found.");
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/content/"+contentId);
		clienttasks.restart_rhsmcertd(null, null, false, null);
		sleep(2*60*1000);
		jsonActivationKey = new JSONObject(CandlepinTasks.getResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/content/"+contentId));
		Assert.assertEquals(jsonActivationKey.getString("displayMessage"), "Content with id "+contentId+" could not be found.");
		requestBody = CandlepinTasks.createContentRequestBody("fooname", contentId, "foolabel", "yum", "Foo Vendor", "/foo/path", "/foo/path/gpg", null, null, modifiedProductIds).toString();
		CandlepinTasks.postResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl,"/content", requestBody);
		CandlepinTasks.createProductUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"fooname","fooproduct", null,attributes ,null);
		CandlepinTasks.postResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl,"/products/fooproduct/content/"+contentId+"?enabled=false",null);
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/content/"+contentId);
		jsonActivationKey = new JSONObject(CandlepinTasks.getResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/content/"+contentId));
		Assert.assertContainsNoMatch(jsonActivationKey.toString(), "Content with id "+contentId+" could not be found.");
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/products/" + "fooproduct");

	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if bind and unbind event is recorded in syslog", 
			groups = { "VerifyBindAndUnbindInSyslog"}, enabled = true)
	@ImplementsNitrateTest(caseId=68740)
	public void VerifyBindAndUnbindInSyslog() throws JSONException,Exception {
		BigInteger serialnums =null;
		String productId=null;
		String poolId=null;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
	String SyslogMessage="Added subscription for";
	String LogMarker = System.currentTimeMillis()+" Testing ***************************************************************";
	clienttasks.subscribe(true, (String)null, (String)null, (String)null, null, null, null, null, null, null, null);
	RemoteFileTasks.markFile(client, clienttasks.varLogMessagesFile, LogMarker);
	Assert.assertTrue(RemoteFileTasks.getTailFromMarkedFile(client, clienttasks.varLogMessagesFile, LogMarker, SyslogMessage).trim().equals(""));
	SyslogMessage="Removed subscription for '";
	clienttasks.unsubscribeFromTheCurrentlyConsumedProductSubscriptionsCollectively();
	RemoteFileTasks.markFile(client, clienttasks.varLogMessagesFile, LogMarker);
	Assert.assertTrue(RemoteFileTasks.getTailFromMarkedFile(client, clienttasks.varLogMessagesFile, LogMarker, SyslogMessage).trim().equals(""));
	LogMarker = System.currentTimeMillis()+" Testing ***************************************************************";	
	for (SubscriptionPool available : clienttasks.getCurrentlyAllAvailableSubscriptionPools()) {
		poolId=available.poolId;
		productId=available.subscriptionName;
	}
	clienttasks.subscribe(null, null, poolId, null, null, null, null, null, null, null, null);
	SyslogMessage="Added subscription for '"+productId+"'";
	RemoteFileTasks.markFile(client, clienttasks.varLogMessagesFile, LogMarker);
	Assert.assertTrue(RemoteFileTasks.getTailFromMarkedFile(client, clienttasks.varLogMessagesFile, LogMarker, SyslogMessage).trim().equals(""));
	
	for (ProductSubscription consumed : clienttasks.getCurrentlyConsumedProductSubscriptions()) {
		serialnums=consumed.serialNumber;
		productId=consumed.productName;
	}
	clienttasks.unsubscribe(null, serialnums, null, null, null);
	SyslogMessage="Removed subscription for '"+productId+"'";
	RemoteFileTasks.markFile(client, clienttasks.varLogMessagesFile, LogMarker);
	Assert.assertTrue(RemoteFileTasks.getTailFromMarkedFile(client, clienttasks.varLogMessagesFile, LogMarker, SyslogMessage).trim().equals(""));

	


	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if register and unregister event is recorded in syslog", 
			groups = { "VerifyRegisterAndUnregisterInSyslog"}, enabled = true)
	@ImplementsNitrateTest(caseId=68749)
	public void VerifyRegisterAndUnregisterInSyslog() throws JSONException,Exception {
	clienttasks.register(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, null, null, null, null, null,(String) null, null, null, null, true, null, null, null, null);
	String consumerid=clienttasks.getCurrentConsumerId();
	String SyslogMessage="Registered system with identity: "+consumerid;
		RemoteFileTasks.runCommandAndAssert(client,"tail -10 "+clienttasks.varLogMessagesFile, null, SyslogMessage, null);
	clienttasks.unregister(null, null, null);	
	SyslogMessage="Unregistered machine with identity: "+consumerid;
	RemoteFileTasks.runCommandAndAssert(client,"tail -10 "+clienttasks.varLogMessagesFile, null, SyslogMessage, null);

	}
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify that Consumer Account And Contract Id are Present in the consumed list", 
			groups = { "VerifyConsumerAccountAndContractIdPresence"}, enabled = true)
	@ImplementsNitrateTest(caseId=68738)
	public void VerifyConsumerAccountAndContractIdPresence() throws JSONException,Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		for(ProductSubscription consumed:clienttasks.getCurrentlyConsumedProductSubscriptions()){
				Assert.assertNotNull(consumed.accountNumber);
				Assert.assertNotNull(consumed.contractNumber);

		}
	}
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify that system should not be compliant for an expired subscription", 
			groups = { "VerifySubscriptionOfBestProductWithUnattendedRegistration"}, enabled = true)
	@ImplementsNitrateTest(caseId=71208)
	public void VerifySubscriptionOfBestProductWithUnattendedRegistration() throws JSONException,Exception {
		Map<String,String> attributes = new HashMap<String,String>();
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		
		String consumerId = clienttasks.getCurrentConsumerId();
		ownerKey = CandlepinTasks.getOwnerKeyOfConsumerId(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, consumerId);
		Calendar now = new GregorianCalendar();
		Date onDate = now.getTime();
		now.add(Calendar.YEAR, 1);
		now.add(Calendar.DATE, 1);
		Date onDateToTest = now.getTime();
		attributes.put("sockets", "8");
		attributes.put("arch", "ALL");
		attributes.put("type", "MKT");
		attributes.put("multi-entitlement", "yes");
		attributes.put("stacking_id", "726409");
		List<String> providedProducts = new ArrayList<String>();
		providedProducts.add("100000000000002");
		CandlepinTasks.createProductUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"Multi-Stackable for 100000000000002","multi-stackable", 1,attributes ,null);
		String requestBody = CandlepinTasks.createSubscriptionRequestBody(20, onDate, onDateToTest,"multi-stackable", Integer.valueOf(getRandInt()), Integer.valueOf(getRandInt()), providedProducts).toString();
		CandlepinTasks.postResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/owners/" + ownerKey + "/subscriptions", requestBody);	
		JSONObject jobDetail = CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		jobDetail = CandlepinTasks.waitForJobDetailStateUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword,sm_serverUrl,jobDetail,"FINISHED", 5*1000, 1);
		clienttasks.unsubscribeFromAllOfTheCurrentlyConsumedProductSubscriptions();
		sleep(3*60*1000);
		int sockets=16;
		Map<String, String> factsMap = new HashMap<String, String>();
		factsMap.put("lscpu.cpu_socket(s)", String.valueOf(sockets));
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(sockets));
		clienttasks.createFactsFileWithOverridingValues("/custom.facts",factsMap);
		for (InstalledProduct installed : clienttasks.getCurrentlyInstalledProducts()) {
			if(!(installed.productId.equals("100000000000002"))){
				moveProductCertFiles(installed.productId+"_"+ ".pem");
				moveProductCertFiles(installed.productId + ".pem");
			}
		}
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		for (InstalledProduct installed : clienttasks.getCurrentlyInstalledProducts()) {
			if(installed.productId.equals("100000000000002"))
			Assert.assertEquals(installed.status, "Subscribed");

		}
		for(ProductSubscription consumed:clienttasks.getCurrentlyConsumedProductSubscriptions()){
			CandlepinTasks.deleteSubscriptionsAndRefreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, sm_clientOrg,"multi-stackable");
			CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
			CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/products/" + "multi-stackable");
			Assert.assertEquals(consumed.productName, "Multi-Stackable for 100000000000002");
		}
		
		
		
	}
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify that system should not be compliant for an expired subscription", 
			groups = { "VerifySystemCompliantFactWhenAllProductsAreExpired_Test"}, enabled = true)
	public void VerifySystemCompliantFactWhenAllProductsAreExpired_Test() throws JSONException,Exception {
		restoreProductCerts();
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"certCheckInterval".toLowerCase(), "1" });
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		Assert.assertFalse(clienttasks.getCurrentlyInstalledProducts().isEmpty(),
				"Products are currently installed for which the compliance of ALL are covered by future available subscription pools.");
		Assert.assertEquals(clienttasks.getFactValue(factname), "invalid",
				"Before attempting to subscribe to any future subscription, the system should be non-compliant (see value for fact '"+factname+"').");
		
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		File expectCertFile = new File(System.getProperty("automation.dir",
				null) + "/expiredcerts/Expiredcert.pem");
		RemoteFileTasks.putFile(client.getConnection(),
				expectCertFile.toString(), "/root/", "0755");
		clienttasks.importCertificate_("/root/Expiredcert.pem");
		for (InstalledProduct installed : clienttasks.getCurrentlyInstalledProducts()) {
			if(!(installed.status.equals("Expired"))){
				moveProductCertFiles(installed.productId+"_"+ ".pem");
				moveProductCertFiles(installed.productId + ".pem");
			}
		}
		Assert.assertFalse(clienttasks.getCurrentlyInstalledProducts().isEmpty());
		String actual=clienttasks.getFactValue(factname).trim();
		Assert.assertEquals(actual, "invalid");
	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if able to entitle consumer to the pool virt_only,pool_derived,bonus pool ", 
			groups = { "VerifyVirtOnlyPoolsRemoved","blockedByBug-722977"}, enabled = true)
	public void VerifyVirtOnlyPoolsRemoved() throws JSONException,Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				sm_clientUsernames,  (String)null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		ownerKey = CandlepinTasks.getOwnerKeyOfConsumerId(sm_clientUsername, sm_clientPassword, sm_serverUrl, consumerId);
		CandlepinTasks.deleteSubscriptionsAndRefreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, sm_clientOrg,"virtualPool");
		CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/products/" + "virtualPool");

		Calendar now = new GregorianCalendar();
		Date onDate = now.getTime();
		now.add(Calendar.YEAR, 1);
		now.add(Calendar.DATE, 1);
		Date onDateToTest = now.getTime();
		Map<String,String> attributes = new HashMap<String,String>();
		attributes.put("virt_limit", "4");
		attributes.put("arch", "ALL");
		attributes.put("type", "MKT");
		attributes.put("multi-entitlement", "no");
		List<String> providedProducts = new ArrayList<String>();
		providedProducts.add("27060");
		CandlepinTasks.createProductUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"virtual-product","virtualPool", 1,attributes ,null);
		String requestBody = CandlepinTasks.createSubscriptionRequestBody(10, onDate, onDateToTest,"virtualPool", Integer.valueOf(getRandInt()), Integer.valueOf(getRandInt()), providedProducts).toString();
		CandlepinTasks.postResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/owners/" + ownerKey + "/subscriptions", requestBody);	

		JSONObject jobDetail = CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		CandlepinTasks.waitForJobDetailStateUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword,sm_serverUrl,jobDetail,"FINISHED", 5*1000, 1);
		sleep(3*60*1000);
		Boolean flag=false;
		String poolId=null;
		for(SubscriptionPool pools:clienttasks.getCurrentlyAvailableSubscriptionPools()){
			if(pools.subscriptionName.equals("virtual-product")){
				flag=true;
				poolId=pools.poolId;
		}
			}
		Assert.assertTrue(flag, "Pool is created");
		JSONObject jsonSubscriptions = new JSONObject(CandlepinTasks.getResourceUsingRESTfulAPI(sm_clientUsername,sm_clientPassword,sm_serverUrl,"/pools/"+poolId));	
		JSONArray jsonProduct = (JSONArray) jsonSubscriptions.get("productAttributes");
		JSONObject product=(JSONObject) jsonProduct.get(0);
		jsonProduct.remove(0);
		product.remove("value");
		product.accumulate("value", "0");
		jsonProduct.put(0, product);
		jsonSubscriptions.remove("productAttributes");
		jsonSubscriptions.accumulate("productAttributes", jsonProduct);
		String result=CandlepinTasks.putResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword,sm_serverUrl,"/owners/" + ownerKey + "/subscriptions" ,jsonSubscriptions);
		CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);

		for(SubscriptionPool pools:clienttasks.getCurrentlyAvailableSubscriptionPools()){
			flag=false;
			if(pools.subscriptionName.equals("virtual-product")){
				flag=true;
				poolId=pools.poolId;
		}
			}
		Assert.assertTrue(!flag, "Pool is no longer available");
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/products/" + "virtual-pool");
	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if able to entitle consumer to the pool virt_only,pool_derived,bonus pool ", 
			groups = { "consumeVirtOnlyPool","blockedByBug-756628"}, enabled = true)
	public void consumeVirtOnlyPool() throws JSONException,Exception {
		String isPool_derived =null;
		Boolean virtonly=false;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				sm_clientUsernames,  (String)null, null, null, true, null, null, null, null);
		
		for (SubscriptionPool availList : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {
			isPool_derived = CandlepinTasks.getPoolProductAttributeValue(sm_clientUsername,	sm_clientPassword, sm_serverUrl, availList.poolId,"pool_derived");		
			 virtonly= CandlepinTasks.isPoolVirtOnly(sm_clientUsername, sm_clientPassword, availList.poolId, sm_serverUrl);
			 if(!(isPool_derived==null) || virtonly){
				String result= clienttasks.subscribe(null, null, availList.poolId, null, null, null, null,null,null, null,null).getStdout();
				String Expected="Guest's host does not match owner of pool: '"+availList.poolId+"'.";
				 Assert.assertEquals(result.trim(), Expected);
			 }
		}

	}

	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if system.entitlements_valid goes from valid to partial after oversubscribing", 
			groups = { "VerifyRHELWorkstationSubscription","blockedByBug-739790"}, enabled = true)
	public void VerifyRHELWorkstationSubscription() throws JSONException,Exception {
		System.out.println(sm_clientUsername + "  "+ sm_clientPassword + "   "+CandlepinType.hosted);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				sm_clientUsernames,  (String)null, null, null, true, null, null, null, null);
		
		for (SubscriptionPool availList : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {
				clienttasks.subscribe(null, null, availList.poolId, null, null, null, null, null, null, null, null);
		}
		for (InstalledProduct installed : clienttasks
				.getCurrentlyInstalledProducts()) {
			if(installed.productId.contains("Workstation")){
				Assert.assertEquals(installed.status, "subscribed");
			}else throw new SkipException(
					"Installed product to be tested is not available");
		}
	}
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if system.entitlements_valid goes from valid to partial after oversubscribing", 
			groups = { "systemEntitlementsValidityAfterOversubscribing","blockedByBug-845126"}, enabled = true)
	public void systemEntitlementsValidityAfterOversubscribing() throws JSONException,Exception {
		Boolean noMultiEntitlements=true;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				sm_clientUsernames,  (String)null, null, null, true, null, null, null, null);
		for (InstalledProduct installed : clienttasks.getCurrentlyInstalledProducts()) {
			if (installed.status.equals("Not Subscribed") || installed.status.equals("Partially Subscribed"))
				moveProductCertFiles(installed.productId + ".pem");
				moveProductCertFiles(installed.productId+"_" + ".pem");
		}		
		String actual=clienttasks.getFactValue(factname).trim();
		Assert.assertEquals(actual, "valid");
		clienttasks.unsubscribeFromAllOfTheCurrentlyConsumedProductSubscriptions();
		String consumerId = clienttasks.getCurrentConsumerId();
		ownerKey = CandlepinTasks.getOwnerKeyOfConsumerId(sm_clientUsername, sm_clientPassword, sm_serverUrl, consumerId);
		for (SubscriptionPool availList : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {
			boolean isMultiEntitled=CandlepinTasks.isSubscriptionMultiEntitlement(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey , availList.poolId);
			
			if(isMultiEntitled){
				noMultiEntitlements=false;
				clienttasks.subscribe(null, null, availList.poolId, null, null, "4", null, null, null, null, null);
			}
		}
		for (InstalledProduct installed : clienttasks.getCurrentlyInstalledProducts()) {
			if (installed.status.equals("Not Subscribed")
					|| installed.status.equals("Partially Subscribed"))
				moveProductCertFiles(installed.productId + ".pem");
				moveProductCertFiles(installed.productId+"_"+ ".pem");
		}
		
		actual=clienttasks.getFactValue(factname).trim();
		if(!noMultiEntitlements) throw new SkipException("No mutli-entitled subscriptions available for testing");
		Assert.assertEquals(actual, "valid");
	}
	
	
/*	*//**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify OwnerInfo is displayed only for pools that are active right now, for all the stats", 
			groups = { "certificateStacking","blockedByBug-726409"}, enabled = true)
	public void certificateStacking() throws JSONException,Exception {
		Map<String,String> attributes = new HashMap<String,String>();
		clienttasks.register(sm_serverAdminUsername, sm_serverAdminPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				sm_clientUsernames, (String) null, null, null, true, null, null, null,null);
		
		int sockets = 14;
		String poolid = null;
		String validity = null;
		Map<String, String> factsMap = new HashMap<String, String>();
		factsMap.put("lscpu.cpu_socket(s)", String.valueOf(sockets));
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(sockets));
		clienttasks.createFactsFileWithOverridingValues("/custom.facts",factsMap);
		clienttasks.facts(null, true, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		ownerKey = CandlepinTasks.getOwnerKeyOfConsumerId(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, consumerId);
		Calendar now = new GregorianCalendar();
		Date onDate = now.getTime();
		now.add(Calendar.YEAR, 1);
		now.add(Calendar.DATE, 1);
		Date onDateToTest = now.getTime();
		attributes.put("sockets", "2");
		attributes.put("arch", "ALL");
		attributes.put("type", "MKT");
		attributes.put("multi-entitlement", "yes");
		attributes.put("stacking_id", "726409");
		List<String> providedProducts = new ArrayList<String>();
		providedProducts.add("100000000000002");
		CandlepinTasks.createProductUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"Multi-Stackable for 100000000000002","multi-stackable", 1,attributes ,null);
		String requestBody = CandlepinTasks.createSubscriptionRequestBody(20, onDate, onDateToTest,"multi-stackable", Integer.valueOf(getRandInt()), Integer.valueOf(getRandInt()), providedProducts).toString();
		CandlepinTasks.postResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/owners/" + ownerKey + "/subscriptions", requestBody);	
		JSONObject jobDetail = CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		jobDetail = CandlepinTasks.waitForJobDetailStateUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword,sm_serverUrl,jobDetail,"FINISHED", 5*1000, 1);
		attributes.put("sockets", "4");
		attributes.put("multi-entitlement", "no");
		CandlepinTasks.createProductUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl, "Stackable for 100000000000002","stackable", 1,attributes ,null);
		requestBody = CandlepinTasks.createSubscriptionRequestBody(20, onDate, onDateToTest, "stackable", Integer.valueOf(getRandInt()), Integer.valueOf(getRandInt()), providedProducts).toString();
		CandlepinTasks.postResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/owners/" + ownerKey + "/subscriptions", requestBody);		
		jobDetail = CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		jobDetail = CandlepinTasks.waitForJobDetailStateUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword,sm_serverUrl,jobDetail,"FINISHED", 5*1000, 1);
		for (InstalledProduct installed : clienttasks.getCurrentlyInstalledProducts()) {
			if(!(installed.productId.equals("100000000000002"))){
				moveProductCertFiles(installed.productId + "_.pem");
				moveProductCertFiles(installed.productId + ".pem");
			}
		}
		sleep(3*60*1000);		
		for (SubscriptionPool availList : clienttasks.getCurrentlyAllAvailableSubscriptionPools()) {
			if(availList.subscriptionName.equals("Multi-Stackable for 100000000000002")){
				poolid=availList.poolId;
				clienttasks.subscribe(null, null, availList.poolId, null, null, "3", null, null, null, null, null);	
				 validity=clienttasks.getFactValue(factname);
				Assert.assertEquals(validity.trim(), "partial");
			}else if( availList.subscriptionName.equals("Stackable for 100000000000002") ){
				clienttasks.subscribe(null, null, availList.poolId, null, null,null, null, null, null, null, null);	
				 validity=clienttasks.getFactValue(factname);
				Assert.assertEquals(validity.trim(), "partial");

			}
		}
		clienttasks.subscribe(null, null, poolid, null, null, "2", null, null, null, null, null);	
		clienttasks.getCurrentlyConsumedProductSubscriptions();
		validity=clienttasks.getFactValue(factname);
		CandlepinTasks.deleteSubscriptionsAndRefreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, sm_clientOrg,"multi-stackable");
		CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/products/" + "multi-stackable");
		CandlepinTasks.deleteSubscriptionsAndRefreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, sm_clientOrg,"stackable");
		CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/products/" + "stackable");
		Assert.assertEquals(validity.trim(), "valid");
	}
	
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify OwnerInfo is displayed only for pools that are active right now, for all the stats", 
			groups = { "OwnerInfoForActivePools","blockedByBug-710141"}, enabled = true)
	public void OwnerInfoForActivePools() throws JSONException,Exception {
		DateFormat yyyy_MM_dd_DateFormat = new SimpleDateFormat("yyyy-MM-dd");
		Map<String,String> regexes = new HashMap<String,String>();
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				sm_clientUsernames,  (String)null, null, null, true, null, null, null, null);
		Calendar now = new GregorianCalendar();

		String onDate = yyyy_MM_dd_DateFormat.format(now.getTime());
		now.add(Calendar.YEAR, 1);
		now.add(Calendar.DATE, 1);
		String onDateToTest = yyyy_MM_dd_DateFormat.format(now.getTime());
		List<SubscriptionPool> availOnDate = getAvailableFutureSubscriptionsOndate(onDateToTest);
		if(availOnDate.size()==0) throw new SkipException(
				"Sufficient future pools are not available");
		for (SubscriptionPool subscriptions : availOnDate) {
			clienttasks.subscribe(null, null, subscriptions.poolId, null, null,
					null, null, null, null, null, null);
		}
		String jsonActivationKey = CandlepinTasks.getResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl, "/owners/"+sm_clientOrg+"/pools");
		for (String Cert: jsonActivationKey.split(",",0)){
			if(Cert.contains("startDate")){
				String[] items = Cert.split("\":\"");
				items=items[1].split("-");
				Assert.assertEquals(items[0], onDate.split("-")[0]);
	 		}
	      }
		jsonActivationKey = CandlepinTasks.getResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl, "/owners/"+sm_clientOrg+"/pools?activeon="+onDateToTest);
	}
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if refresh Pools w/ Auto-Create Owner Fails", 
			groups = { "EnableAndDisableCertV3"}, enabled = true)
	public void EnableAndDisableCertV3() throws JSONException,Exception {
		String version=null;
		servertasks.updateConfigFileParameter("candlepin.enable_cert_v3", "false");
		servertasks.restartTomcat();
		SubscriptionManagerCLITestScript.sleep( 1*60 * 1000);
		clienttasks.restart_rhsmcertd(null, null, false, null);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
			sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		for(EntitlementCert Cert: clienttasks.getCurrentEntitlementCerts()){
			version=Cert.version;
			if(version.equals("1.0")){
			Assert.assertEquals(version, "1.0");
		}else{
			servertasks.updateConfigFileParameter("candlepin.enable_cert_v3", "true");
			servertasks.restartTomcat();
			Assert.fail();
		}
			
		}
		clienttasks.unsubscribeFromAllOfTheCurrentlyConsumedProductSubscriptions();
		servertasks.updateConfigFileParameter("candlepin.enable_cert_v3", "true");
		servertasks.restartTomcat();
		clienttasks.restart_rhsmcertd(null, null, false, null);
		SubscriptionManagerCLITestScript.sleep( 1*60*1000);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
					(String) null, null, null, null, true, null, null, null, null);
		for(EntitlementCert Cert: clienttasks.getCurrentEntitlementCerts()){
			version=Cert.version;
		Assert.assertEquals(version, "3.1");
	}
	}
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if refresh Pools w/ Auto-Create Owner Fails", 
			groups = { "RefreshPoolsWithAutoCreate","blockedByBug-720487"}, enabled = true)
	public void RefreshPoolsWithAutoCreate() throws JSONException,Exception {
		String org="newowner";
		JSONObject jsonActivationKey = new JSONObject(
				CandlepinTasks.getResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl, "/owners/"+org));
		Assert.assertEquals(jsonActivationKey.getString("displayMessage"), "Organization with id newowner could not be found.");
		new JSONObject(CandlepinTasks.putResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/owners/"+org+"/subscriptions?auto_create_owner=true" ));
		jsonActivationKey = new JSONObject(CandlepinTasks.getResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl, "/owners/"+org));
		Assert.assertNotNull(jsonActivationKey.get("created")); 
		jsonActivationKey= new JSONObject(CandlepinTasks.putResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/owners/AnotherOwner/subscriptions?auto_create_owner=false" ));
		Assert.assertEquals(jsonActivationKey.getString("displayMessage"),"owner with key: AnotherOwner was not found.");
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl, "/owners/"+org);
	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify tracebacks occur running yum repolist after subscribing to a pool", 
			groups = { "VerifyipV4Facts","blockedByBug-874147"}, enabled = true)
	public void VerifyipV4Facts() throws JSONException,Exception {
		Boolean pattern=false;
		Boolean Flag=false;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String result=clienttasks.facts(true, null, null, null, null).getStdout();
		Pattern p = Pattern.compile(result);
		Matcher matcher = p.matcher("Unknown");
		while (matcher.find()) {
			 pattern = matcher.find();
	}
		Assert.assertEquals(pattern, Flag);
	}
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify tracebacks occur running yum repolist after subscribing to a pool", 
			groups = { "VerifyRepoFileExistance","blockedByBug-886604"}, enabled = true)
	public void VerifyRepoFileExistance() throws JSONException,Exception {
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsm","manage_repos", "1" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		List<YumRepo> originalRepos =clienttasks.getCurrentlySubscribedYumRepos();
		String result=client.runCommandAndWait("yum repolist").getStdout();

		for (YumRepo repo : originalRepos) {
			
			Boolean flag=false;
			if(!(repo.id==null)){
				Pattern pattern = Pattern.compile(repo.id, Pattern.MULTILINE);
				Matcher matcher = pattern.matcher(result);
				if (matcher.find()) {
					flag=true;
				}
				Assert.assertTrue(flag);
				flag=false;
			}
			listOfSectionNameValues = new ArrayList<String[]>();
			listOfSectionNameValues.add(new String[] { "rhsm","manage_repos", "0" });
			clienttasks.config(null, null, true, listOfSectionNameValues);
			originalRepos =clienttasks.getCurrentlySubscribedYumRepos();
			 result=client.runCommandAndWait("yum repolist").getStdout();
			 
			 for (YumRepo yumrepo : originalRepos) {
					
					flag=false;
					if(!(yumrepo.id==null)){
						Pattern pattern = Pattern.compile(yumrepo.id, Pattern.MULTILINE);
						Matcher matcher = pattern.matcher(result);
						if (matcher.find()) {
							flag=true;
						}
						Assert.assertFalse(flag);
						flag=false;
					}
	}
}
	}
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify tracebacks occur running yum repolist after subscribing to a pool", 
			groups = { "AddingVirtualPoolToActivationKey","blockedByBug-755677"}, enabled = true)
	public void AddingVirtualPoolToActivationKey() throws JSONException,Exception {
		Integer addQuantity=1;
		int count =0;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String name = String.format("%s_%s-ActivationKey%s", sm_clientUsername,
				sm_clientOrg, System.currentTimeMillis());
		Map<String, String> mapActivationKeyRequest = new HashMap<String, String>();
		mapActivationKeyRequest.put("name", name);
		JSONObject jsonActivationKeyRequest = new JSONObject(
				mapActivationKeyRequest);
		JSONObject jsonActivationKey = new JSONObject(
				CandlepinTasks.postResourceUsingRESTfulAPI(sm_clientUsername,
						sm_clientPassword, sm_serverUrl, "/owners/"
								+ sm_clientOrg + "/activation_keys",
								jsonActivationKeyRequest.toString()));
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool availList : clienttasks
				.getCurrentlyAllAvailableSubscriptionPools()) {
			if(availList.subscriptionName.contains("unlimited")){
				count++;
				JSONObject jsonAddedPool = new JSONObject(CandlepinTasks.postResourceUsingRESTfulAPI(sm_clientUsername, sm_clientPassword, sm_serverUrl, "/activation_keys/" + jsonActivationKey.getString("id") + "/pools/" +availList.poolId+(addQuantity==null?"":"?quantity="+addQuantity), null));
			}
		}
		clienttasks.register(null, null, sm_clientOrg, null, null, null, null, null, null, null, name, null, null, null, true, null, null, null, null);
		List<ProductSubscription> consumed=clienttasks.getCurrentlyConsumedProductSubscriptions();
		Assert.assertEquals(consumed.size(), count);
		JSONObject delete = new JSONObject(CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_clientUsername,
				sm_clientPassword, sm_serverUrl,"/activation_keys/"+name));
	}
	
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify tracebacks occur running yum repolist after subscribing to a pool", 
			groups = { "YumReposListAfterSubscription","blockedByBug-696786" }, enabled = true)
	public void YumReposListAfterSubscription() throws JSONException,Exception {
		Boolean pattern=false;
		Boolean Flag=false;
		String yum_cmd="yum repolist enabled --disableplugin=rhnplugin";
		String result=client.runCommandAndWait(yum_cmd).getStdout();
		clienttasks.register(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		result=client.runCommandAndWait(yum_cmd).getStdout();
		Pattern p = Pattern.compile(result);
		Matcher matcher = p.matcher("Traceback (most recent call last):");
		while (matcher.find()) {
			 pattern = matcher.find();

		}
		Assert.assertEquals(Flag, pattern);
	}
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify rhsm log for Update With No Installed Products", 
			groups = {"UpdateWithNoInstalledProducts","blockedByBug-746241","blockedByBug-907638" }, enabled = true)
	public void UpdateWithNoInstalledProducts() throws JSONException,Exception {
		Boolean actual = false;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		clienttasks.restart_rhsmcertd(null, null, false, null);
		moveProductCertFiles("*.pem");
		String InstalledProducts=clienttasks.listInstalledProducts().getStdout();
		Assert.assertEquals(InstalledProducts.trim(), "No installed products to list");
		String LogMarker = System.currentTimeMillis()+" Testing ***************************************************************";
		RemoteFileTasks.markFile(client, clienttasks.rhsmLogFile, LogMarker);
		clienttasks.restart_rhsmcertd(null, null, false, null);
		SubscriptionManagerCLITestScript.sleep(2 * 60 * 1000);
		Boolean flag = waitForRegexInRhsmLog("Error",RemoteFileTasks.getTailFromMarkedFile(client, clienttasks.rhsmLogFile, LogMarker, null));
		Assert.assertEquals(flag, actual);
		actual=true;
		flag = waitForRegexInRhsmLog("Installed product IDs: \\[\\]",RemoteFileTasks.getTailFromMarkedFile(client, clienttasks.rhsmLogFile, LogMarker, null));
		Assert.assertEquals(flag, actual);
				
	}
		
	
	
	
	
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify Facts Update For Deleted Consumer", 
			groups = { "FactsUpdateForDeletedConsumer","blockedByBug-798788" }, enabled = true)
	@ImplementsNitrateTest(caseId = 148216)
	public void FactsUpdateForDeletedConsumer() throws JSONException,Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,
				sm_serverAdminPassword, sm_serverUrl, "/consumers/"
						+ consumerId);
		String result=clienttasks.facts_(null, true, null, null, null).getStderr();
		String ExpectedMsg="Consumer "+consumerId+" has been deleted";
		Assert.assertEquals(result.trim(), ExpectedMsg);
	}
	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if you can register using consumer id of a deleted owner", 
			groups = { "RegisterWithConsumeridOfDeletedOwner" }, enabled = true)
	@ImplementsNitrateTest(caseId = 148216)
	public void RegisterWithConsumeridOfDeletedOwner() throws JSONException,Exception {
		String orgname="testOwner1";
		servertasks.createOwnerUsingCPC(orgname);
		clienttasks.register_(sm_serverAdminUsername, sm_serverAdminPassword,
				orgname, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String consumerId=clienttasks.getCurrentConsumerId();
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/owners/" + orgname);
		clienttasks.clean_(null, null, null);
		SSHCommandResult result=clienttasks.register_(sm_serverAdminUsername, sm_serverAdminPassword, orgname, null, null, null, consumerId, null, null, null,(String)null, null, null, null, null, null, null, null, null);
		String expected="Consumer "+consumerId+" has been deleted";
		Assert.assertEquals(result.getStderr().trim(), expected);
	}

	

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if register to a deleted owner", 
			groups = { "RegisterToDeletedOwner" }, enabled = true)
	@ImplementsNitrateTest(caseId = 148216)
	public void RegisterToDeletedOwner() throws JSONException,Exception {
		String orgname="testOwner1";
		servertasks.createOwnerUsingCPC(orgname);
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/owners/" + orgname);
		SSHCommandResult result=clienttasks.register_(sm_serverAdminUsername, sm_serverAdminPassword,orgname, null, null, null, null, null, null, null,(String) null, null, null, null, true, null, null, null, null);
		String expected="Organization "+orgname+" does not exist.";
		Assert.assertEquals(result.getStderr().trim(), expected);
	}


	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if Repos List is empty for FutureSubscription", 
			groups = { "EmptyReposListFOrFutureSubscription" }, enabled = true)
	@ImplementsNitrateTest(caseId = 148534)
	public void EmptyReposListForFutureSubscription() throws JSONException,
	Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		Calendar now = new GregorianCalendar();
		DateFormat yyyy_MM_dd_DateFormat = new SimpleDateFormat("yyyy-MM-dd");
		now.add(Calendar.YEAR, 1);
		now.add(Calendar.DATE, 1);
		String onDateToTest = yyyy_MM_dd_DateFormat.format(now.getTime());
		now.add(Calendar.YEAR, 1);
		String endDate=yyyy_MM_dd_DateFormat.format(now.getTime());
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		List<SubscriptionPool> availOnDate = getAvailableFutureSubscriptionsOndate(onDateToTest);
		if(availOnDate.size()==0) throw new SkipException(
				"Sufficient future pools are not available");
		for (SubscriptionPool subscriptions : availOnDate) {
			if(!(subscriptions.endDate.before(endDate))){
				clienttasks.subscribe(null, null, subscriptions.poolId, null, null,null, null, null, null, null, null);
		}
		}
		for (ProductSubscription subscriptions : clienttasks.getCurrentlyConsumedProductSubscriptions()) {
			log.info(subscriptions.endDate.toString());
		}
		List<Repo> repo = clienttasks.getCurrentlySubscribedRepos();
		Assert.assertTrue(repo.isEmpty());

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify Display hierarchy of owners", groups = { "VerifyHierarchyOfOwners" }, enabled = true)
	@ImplementsNitrateTest(caseId = 68737)
	public void VerifyHierarchyOfOwners() throws JSONException, Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		JSONObject jsonActivationKey = new JSONObject(
				CandlepinTasks.getResourceUsingRESTfulAPI(sm_clientUsername,
						sm_clientPassword, sm_serverUrl, "/owners/"));

		System.out.println(jsonActivationKey);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if auto-subscribe and activation-key are mutually exclusive", groups = {
			"VerifyAutoSubscribeAndActivationkeyTogether",
	"blockedByBug-869729" }, enabled = true)
	public void VerifyAutoSubscribeAndActivationkeyTogether()
			throws JSONException, Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String name = String.format("%s_%s-ActivationKey%s", sm_clientUsername,
				sm_clientOrg, System.currentTimeMillis());
		Map<String, String> mapActivationKeyRequest = new HashMap<String, String>();
		mapActivationKeyRequest.put("name", name);
		JSONObject jsonActivationKeyRequest = new JSONObject(
				mapActivationKeyRequest);
		JSONObject jsonActivationKey = new JSONObject(
				CandlepinTasks.postResourceUsingRESTfulAPI(sm_clientUsername,
						sm_clientPassword, sm_serverUrl, "/owners/"
								+ sm_clientOrg + "/activation_keys",
								jsonActivationKeyRequest.toString()));
		SSHCommandResult result = clienttasks.register_(null, null,
				sm_clientOrg, null, null, null, null, true, null, null,
				jsonActivationKey.get("name").toString(), null, null, null,
				true, null, null, null, null);
		String expected_msg = "Error: Activation keys cannot be used with --auto-attach.";
		Assert.assertEquals(result.getStdout().trim(), expected_msg);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	/*
	 * @Test( description=
	 * "verify if entitlement certs are regenerated if certs are manually removed"
	 * , groups={"VerifyDuplicateContentsInReposList"}, enabled=true)
	 * 
	 * @ImplementsNitrateTest(caseId=50229) public void
	 * VerifyDuplicateContentsInReposList() throws JSONException, Exception {
	 * clienttasks
	 * .register(sm_clientUsername,sm_clientPassword,sm_clientOrg,null
	 * ,null,null,null,null,null,null,(String)null,null, null, true,null,null,
	 * null, null); List<String[]> listOfSectionNameValues = new
	 * ArrayList<String[]>(); listOfSectionNameValues.add(new
	 * String[]{"rhsmcertd","healFrequency".toLowerCase(), "1440"});
	 * clienttasks.config(null,null,true,listOfSectionNameValues);
	 * clienttasks.unsubscribe(true, (BigInteger)null, null, null, null);
	 * for(SubscriptionPool pool
	 * :clienttasks.getCurrentlyAllAvailableSubscriptionPools()){ List<String>
	 * providedProducts =
	 * CandlepinTasks.getPoolProvidedProductIds(sm_clientUsername,
	 * sm_clientPassword, sm_serverUrl, pool.poolId);
	 * System.out.println(providedProducts + "  providedProducts"); } }
	 */

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if entitlement certs are regenerated if certs are manually removed", groups = { "VerifyRegenrateEntitlementCert" }, enabled = true)
	@ImplementsNitrateTest(caseId = 64181)
	public void VerifyRegenrateEntitlementCert() throws JSONException,
	Exception {
		String poolId = null;
		int Certfrequeny = 1;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool availList : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {
			poolId = availList.poolId;

		}
		clienttasks.subscribe(null, null, poolId, null, null, null, null,
				null, null, null, null);
		client.runCommandAndWait("rm -rf " + clienttasks.entitlementCertDir
				+ "/*.pem");
		clienttasks.restart_rhsmcertd(Certfrequeny, null, false, null);
		SubscriptionManagerCLITestScript.sleep(Certfrequeny * 60 * 1000);
		List<File> Cert = clienttasks.getCurrentEntitlementCertFiles();
		Assert.assertNotNull(Cert.size());
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if entitlement certs are downloaded if subscribed using bogus poolid", groups = { "VerifySubscribingTobogusPoolID" }, enabled = true)
	@ImplementsNitrateTest(caseId = 50223)
	public void VerifySubscribingTobogusPoolID() throws JSONException,
	Exception {
		String poolId = null;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool availList : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {
			poolId = availList.poolId;

		}
		String pool = randomizeCaseOfCharactersInString(poolId);
		clienttasks.subscribe_(null, null, pool, null, null, null, null, null,
				null, null, null);
		List<File> Cert = clienttasks.getCurrentEntitlementCertFiles();
		Assert.assertEquals(Cert.size(), 0);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify Functionality Access After Unregister", groups = { "VerifyFunctionalityAccessAfterUnregister" }, enabled = true)
	@ImplementsNitrateTest(caseId = 50215)
	public void VerifyFunctionalityAccessAfterUnregister()
			throws JSONException, Exception {
		clienttasks
		.register(sm_clientUsername, sm_clientPassword, sm_clientOrg);
		String availList = clienttasks.listAllAvailableSubscriptionPools()
				.getStdout();
		Assert.assertNotNull(availList);
		clienttasks.unregister(null, null, null);
		availList = clienttasks.list_(true, true, null, null, null, null, null,
				null, null).getStdout();
		String expected = "This system is not yet registered. Try 'subscription-manager register --help' for more information.";
		Assert.assertEquals(availList.trim(), expected);
		ConsumerCert consumercert = clienttasks.getCurrentConsumerCert();
		Assert.assertNull(consumercert);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify only One Cert is downloaded Per One Subscription", groups = {"VerifyOneCertPerOneSubscription"}, enabled = true)
	@ImplementsNitrateTest(caseId = 50215)
	public void VerifyOneCertPerOneSubscription() throws JSONException,
	Exception {
		int expected = 0;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		clienttasks.deleteFactsFileWithOverridingValues("/custom.facts");
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool subscriptionpool : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {

			clienttasks.subscribe(null, null, subscriptionpool.poolId, null,
					null, "1", null, null, null, null, null);
			expected = expected + 1;
			List<File> Cert = clienttasks.getCurrentEntitlementCertFiles();
			Assert.assertEquals(Cert.size(), expected);

		}
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify content set associated with product", groups = { "VerifyUnsubscribingCertV3","blockedByBug-895447"}, enabled = true)
	@ImplementsNitrateTest(caseId = 50215)
	public void VerifyUnsubscribingCertV3() throws JSONException, Exception {

		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		File expectCertFile = new File(System.getProperty("automation.dir",
				null) + "/expiredcerts/CertV3.pem");
		RemoteFileTasks.putFile(client.getConnection(),
				expectCertFile.toString(), "/root/", "0755");
		clienttasks.importCertificate_("/root/CertV3.pem");
		String expected = "This machine has been unsubscribed from 1 subscriptions";
		String result = clienttasks.unsubscribe(true, (BigInteger) null, null,
				null, null).getStdout();
		Assert.assertEquals(result.trim(), expected);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify  rhsmcertd is logging update failed (255)", groups = {
			"VerifyRHSMCertdLogging", "blockedByBug-708512","blockedByBug-907638" }, enabled = true)
	public void VerifyRHSMCertdLogging() throws JSONException, Exception {
		int autoAttachInterval = 1;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String Frequency = clienttasks.getConfFileParameter(
				clienttasks.rhsmConfFile, "autoAttachInterval");
		clienttasks.restart_rhsmcertd(autoAttachInterval, null, false, null);
		clienttasks.waitForRegexInRhsmcertdLog("update failed (255)", 1);
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), Frequency });
		clienttasks.config(null, null, true, listOfSectionNameValues);

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify content set associated with product", groups = { "VerifycertsAfterUnsubscribeAndunregister" }, enabled = true)
	@ImplementsNitrateTest(caseId = 50215)
	public void VerifyCertsAfterUnsubscribeAndunregister()
			throws JSONException, Exception {

		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		clienttasks.subscribe(true, null, (String) null, null, null, null,
				null, null, null, null, null);
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		List<File> certs = clienttasks.getCurrentEntitlementCertFiles();
		Assert.assertTrue(certs.isEmpty());
		certs = clienttasks.getCurrentProductCertFiles();
		Assert.assertFalse(certs.isEmpty());
		clienttasks.unregister(null, null, null);
		ConsumerCert consumerCerts = clienttasks.getCurrentConsumerCert();
		Assert.assertNull(consumerCerts);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify reregister with invalid consumerid", groups = { "VerifyRegisterUsingInavlidConsumerId" }, enabled = true)
	@ImplementsNitrateTest(caseId = 61716)
	public void VerifyregisterUsingInavlidConsumerId() throws JSONException,
	Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		String invalidconsumerId = randomGenerator.nextInt() + consumerId;
		System.out.println(invalidconsumerId + "  " + consumerId);
		SSHCommandResult result = clienttasks.register_(sm_clientUsername,
				sm_clientPassword, sm_clientOrg, null, null, null,
				invalidconsumerId, null, null, null, (String) null, null, null,
				null, true, null, null, null, null);
		Assert.assertEquals(result.getStdout().trim(), "The system with UUID "
				+ consumerId + " has been unregistered");
		Assert.assertEquals(result.getStderr().trim(), "Consumer with id "
				+ invalidconsumerId + " could not be found.");
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if corrupt identity cert displays a trace back for list command", groups = {
			"VerifyCorruptIdentityCert", "blockedByBug-607162" }, enabled = true)
	public void VerifycorruptIdentityCert() throws JSONException, Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		client.runCommandAndWait("cp /etc/pki/consumer/cert.pem /etc/pki/consumer/cert.pem.save");
		RemoteFileTasks.runCommandAndAssert(
				client,
				"openssl x509 -noout -text -in "
						+ clienttasks.consumerCertFile()
						+ " > /tmp/stdout; mv /tmp/stdout -f "
						+ clienttasks.consumerCertFile(), 0);
		String result = clienttasks.list_(null, true, null, null, null, null,
				null, null, null).getStdout();
		Assert.assertEquals(result.trim(),
				clienttasks.msg_ConsumerNotRegistered);
		client.runCommandAndWait("mv -f /etc/pki/consumer/cert.pem.save /etc/pki/consumer/cert.pem");

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "subscription-manager facts --update changes update date after facts update", groups = {
			"VerifyUpdateConsumerFacts", "blockedByBug-700821" }, enabled = true)
	public void VerifyupdateConsumerFacts() throws JSONException, Exception {
		// curl -k -u admin:admin https://10.70.35.91:8443/candlepin/consumers/
		// | python -mjson.tool
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String consumerid = clienttasks.getCurrentConsumerId();
		JSONObject jsonConsumer = new JSONObject(
				CandlepinTasks.getResourceUsingRESTfulAPI(
						sm_serverAdminUsername, sm_serverAdminPassword,
						sm_serverUrl, "/consumers/" + consumerid));
		String createdDateBeforeUpdate = jsonConsumer.getString("created");
		String UpdateDateBeforeUpdate = jsonConsumer.getString("updated");
		clienttasks.facts(null, true, null, null, null).getStderr();
		jsonConsumer = new JSONObject(
				CandlepinTasks.getResourceUsingRESTfulAPI(
						sm_serverAdminUsername, sm_serverAdminPassword,
						sm_serverUrl, "/consumers/" + consumerid));
		String createdDateAfterUpdate = jsonConsumer.getString("created");
		String UpdateDateAfterUpdate = jsonConsumer.getString("updated");
		Assert.assertEquals(createdDateBeforeUpdate, createdDateAfterUpdate,
				"no changed in date value after facts update");
		Assert.assertNoMatch(UpdateDateBeforeUpdate, UpdateDateAfterUpdate,
				"updated date has been changed after facts update");
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify healing of installed products without taking future subscriptions into consideration", groups = { "VerifyHealingForFutureSubscription","blockedByBug-907638"}, enabled = true)
	public void VerifyHealingForFuturesubscription() throws JSONException,
	Exception {
		int autoAttachInterval = 2;
		clienttasks.deleteFactsFileWithOverridingValues();
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		clienttasks.service_level_(null, null, null, true, null, null, null,
				null, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		JSONObject jsonConsumer = CandlepinTasks.setAutohealForConsumer(
				sm_clientUsername, sm_clientPassword, sm_serverUrl, consumerId,
				true);
		Assert.assertTrue(jsonConsumer.getBoolean("autoheal"),
				"A consumer's autoheal attribute value=true.");
		Calendar now = new GregorianCalendar();
		List<String> productId = new ArrayList<String>();
		now.add(Calendar.YEAR, 1);
		DateFormat yyyy_MM_dd_DateFormat = new SimpleDateFormat("yyyy-MM-dd");
		String onDateToTest = yyyy_MM_dd_DateFormat.format(now.getTime());
		clienttasks.subscribe(true, null, (String) null, null, null, null,
				null, null, null, null, null);
		for (InstalledProduct installed : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installed.status.equals("Not Subscribed")
					&& installed.status.equals("Partially Subscribed"))
				moveProductCertFiles(installed.productId + ".pem");
				moveProductCertFiles(installed.productId + "._pem");
		}
		for (SubscriptionPool availOnDate : getAvailableFutureSubscriptionsOndate(onDateToTest)) {
			System.out.println(availOnDate.poolId + " avail on date is");
			clienttasks.subscribe(null, null, availOnDate.poolId, null, null,
					null, null, null, null, null, null);
		}
		for (InstalledProduct installedproduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installedproduct.status.equals("Future Subscription")) {
				productId.add(installedproduct.productId);
			}
		}
		clienttasks.restart_rhsmcertd(null, autoAttachInterval, false, null);
		SubscriptionManagerCLITestScript.sleep(autoAttachInterval * 60 * 1000);

		for (InstalledProduct installedproduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			for (String productid : productId) {
				if (installedproduct.productId.equals(productid)) {
					Assert.assertEquals(installedproduct.status.trim(),
							"Subscribed");

				}
			}
		}
		


	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify unsubscribe from multiple invalid serial numbers", groups = { "UnsubscribeFromInvalidMultipleEntitlements" }, enabled = true)
	@ImplementsNitrateTest(caseId = 50230)
	public void UnsubscribeFromInvalidMultipleEntitlements()
			throws JSONException, Exception {
		List<BigInteger> serialnums = new ArrayList<BigInteger>();
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool pool : clienttasks.getCurrentlyAvailableSubscriptionPools()) {
			clienttasks.subscribe(null, null, pool.poolId, null, null, null, null, null, null, null, null);
		}
		if(clienttasks.getCurrentlyConsumedProductSubscriptions().isEmpty())throw new SkipException(
				"Sufficient pools are not available");
		for (ProductSubscription consumed : clienttasks.getCurrentlyConsumedProductSubscriptions()) {
			serialnums.add(consumed.serialNumber);
		}
		
		int i = randomGenerator.nextInt(serialnums.size());
		int j = randomGenerator.nextInt(serialnums.size());
		if (i == j) {
			j = randomGenerator.nextInt(serialnums.size());

		}
		BigInteger serialOne = serialnums.get(i);
		BigInteger serialTwo = serialnums.get(j);
		String result = unsubscribeFromMultipleEntitlementsUsingSerialNumber(
				serialOne.multiply(serialTwo), serialTwo.multiply(serialOne))
				.getStdout();
		String expected = "Unsuccessfully removed serial numbers:" + "\n"
				+ "   " + serialOne.multiply(serialTwo)
				+ " is not a valid value for serial" + "\n" + "   "
				+ serialTwo.multiply(serialOne)
				+ " is not a valid value for serial";
		Assert.assertEquals(result.trim(), expected);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify unsubscribe from multiple subscriptions", groups = {
			"UnsubscribeFromMultipleEntitlementsTest", "blockedByBug-867766","blockedByBug-906550" }, enabled = true)
	@ImplementsNitrateTest(caseId = 50230)
	public void UnsubscribeFromMultipleEntitlements() throws JSONException,
	Exception {
		List<BigInteger> serialnums = new ArrayList<BigInteger>();
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool pool : clienttasks.getCurrentlyAvailableSubscriptionPools()) {
			clienttasks.subscribe(null, null, pool.poolId, null, null, null, null, null, null, null, null);
		}
		for (ProductSubscription consumed : clienttasks
				.getCurrentlyConsumedProductSubscriptions()) {
			serialnums.add(consumed.serialNumber);
		}
		int i = randomGenerator.nextInt(serialnums.size());
		int j = randomGenerator.nextInt(serialnums.size());
		if (i == j) {
			j = randomGenerator.nextInt(serialnums.size());

		}
		BigInteger serialOne = serialnums.get(i);
		BigInteger serialTwo = serialnums.get(j);
		String result = unsubscribeFromMultipleEntitlementsUsingSerialNumber(
				serialOne, serialTwo).getStdout();
		System.out.println(result);
		String expected = "Successfully removed serial numbers:" + "\n" + "   "
				+ serialOne + "\n" + "   " + serialTwo;
		Assert.assertEquals(result.trim(), expected);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify content set associated with product", groups = { "VerifyRegisterWithConsumerIdForDifferentUser" }, enabled = true)
	@ImplementsNitrateTest(caseId = 61710)
	public void VerifyRegisterWithConsumerIdForDifferentUser()
			throws JSONException, Exception {
		if (sm_client2Username==null) throw new SkipException("This test requires valid credentials for a second user.");
		clienttasks.register(sm_client2Username, sm_client2Password,
				sm_client2Org, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String consumerid = clienttasks.getCurrentConsumerId();
		String result = clienttasks.register_(sm_clientUsername,
				sm_clientPassword, sm_clientOrg, null, null, null, consumerid,
				null, null, null, (String) null, null, null, null, true, null,
				null, null, null).getStderr();
		System.out.println("result  " + result);
		Assert.assertNotNull(result);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify content set associated with product", groups = { "VerifyFactsListByOverridingValues" }, enabled = true)
	@ImplementsNitrateTest(caseId = 56389)
	public void VerifyFactsListByOverridingValues() throws JSONException,
	Exception {

		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String listBeforeUpdate = clienttasks.facts(true, null, null, null,
				null).getStdout();
		Map<String, String> factsMap = new HashMap<String, String>();
		Integer sockets = 4;
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(sockets));
		factsMap.put("uname.machine", "i386");
		clienttasks.createFactsFileWithOverridingValues("/custom.facts",
				factsMap);
		clienttasks.facts(null, true, null, null, null);
		String listAfterUpdate = clienttasks.facts(true, null, null, null,
				null).getStdout();
		Assert.assertNoMatch(listAfterUpdate, listBeforeUpdate);
		clienttasks.deleteFactsFileWithOverridingValues("/custom.facts");
		clienttasks.facts(null, true, null, null, null);

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify content set associated with product", groups = { "VerifyFactsListWithOutrageousValues" }, enabled = true)
	@ImplementsNitrateTest(caseId = 56897)
	public void VerifyFactsListWithOutrageousValues() throws JSONException,
	Exception {

		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String listBeforeUpdate = clienttasks.facts(true, null, null, null,
				null).getStdout();

		client.runCommandAndWait("echo '{fuzzing :testing}' >>/var/lib/rhsm/facts/facts.json");
		clienttasks.facts(null, true, null, null, null);
		String listAfterUpdate = clienttasks.facts(true, null, null, null,
				null).getStdout();
		Assert.assertFalse(listAfterUpdate.contentEquals("fuzzing"));
		Assert.assertEquals(listAfterUpdate, listBeforeUpdate);
		client.runCommandAndWait("cp /var/lib/rhsm/facts/facts.json /var/lib/rhsm/facts/facts.json.save");
		client.runCommandAndWait("sed /'uname.machine: x86_64'/d /var/lib/rhsm/facts/facts.json");
		clienttasks.facts(null, true, null, null, null);
		listAfterUpdate = clienttasks.facts(true, null, null, null, null)
				.getStdout();
		client.runCommandAndWait("mv -f /var/lib/rhsm/facts/facts.json.save /var/lib/rhsm/facts/facts.json");
		Assert.assertEquals(listAfterUpdate, listBeforeUpdate);

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify content set associated with product", groups = { "Verifycontentsetassociatedwithproduct" }, enabled = true)
	@ImplementsNitrateTest(caseId = 61115)
	public void Verifycontentsetassociatedwithproduct() throws JSONException,
	Exception {
		clienttasks.unregister(null, null, null);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe(true, (BigInteger)null, null, null, null);
		List<SubscriptionPool> pools = clienttasks
				.getCurrentlyAvailableSubscriptionPools();
		clienttasks.subscribeToSubscriptionPool(pools.get(randomGenerator
				.nextInt(pools.size())));
		List<File> certs = clienttasks.getCurrentEntitlementCertFiles();
		RemoteFileTasks.runCommandAndAssert(
				client,
				"openssl x509 -noout -text -in "
						+ certs.get(randomGenerator.nextInt(certs.size()))
						+ " > /tmp/stdout; mv /tmp/stdout -f "
						+ certs.get(randomGenerator.nextInt(certs.size())), 0);
		String consumed = clienttasks.list_(null, null, true, null, null, null,
				null, null, null).getStderr();
		Assert.assertEquals(consumed.trim(), "Error loading certificate");

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify if rhsmcertd process refresh the identity certificate after every restart", groups = {
			"VerifyrhsmcertdRefreshIdentityCert", "blockedByBug-827034",
	"blockedByBug-827035" }, enabled = false)
	/*
	 * FIXME disabling this test in need of further development As written, it
	 * is closing the existing SSHCommandRunners and causing all subsequent
	 * tests to stop and leaves the server and client dates set in the future.
	 * 201211150545:58.600 - SEVERE: Test Failed:
	 * VerifyrhsmcertdRefreshIdentityCert
	 * (com.redhat.qe.auto.testng.TestNGListener.onTestFailure)
	 * java.lang.RuntimeException: java.io.IOException: Could not open channel
	 * (The connection is being shutdown) at
	 * com.redhat.qe.tools.SSHCommandRunner.run(SSHCommandRunner.java:155) at
	 * com
	 * .redhat.qe.tools.SSHCommandRunner.runCommand(SSHCommandRunner.java:282)
	 * at
	 * com.redhat.qe.tools.SSHCommandRunner.runCommandAndWait(SSHCommandRunner
	 * .java:319) at
	 * com.redhat.qe.tools.SSHCommandRunner.runCommandAndWait(SSHCommandRunner
	 * .java:286) at
	 * com.redhat.qe.tools.RemoteFileTasks.testExists(RemoteFileTasks.java:217)
	 * at rhsm.cli.tasks.SubscriptionManagerTasks.getCurrentConsumerCert(
	 * SubscriptionManagerTasks.java:1091) at
	 * rhsm.cli.tests.BugzillaTests.VerifyrhsmcertdRefreshIdentityCert
	 * (BugzillaTests.java:611)
	 */
	public void VerifyrhsmcertdRefreshIdentityCert() throws JSONException,
	Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		Calendar StartTimeBeforeRHSM = clienttasks.getCurrentConsumerCert().validityNotBefore;
		Calendar EndTimeBeforeRHSM = clienttasks.getCurrentConsumerCert().validityNotAfter;
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "server", "insecure", "1" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		String existingCertdate = client.runCommandAndWait(
				"ls -lart /etc/pki/consumer/cert.pem | cut -d ' ' -f6,7,8")
				.getStdout();
		setDate(sm_serverHostname, sm_sshUser, sm_sshKeyPrivate,
				sm_sshkeyPassphrase, "date -s '15 year 9 month'");
		log.info("Changed the date of candlepin"
				+ client.runCommandAndWait("hostname"));
		setDate(sm_clientHostname, sm_sshUser, sm_sshKeyPrivate,
				sm_sshkeyPassphrase, "date -s '15 year 9 month'");
		clienttasks.restart_rhsmcertd(null, null, false, null);
		SubscriptionManagerCLITestScript.sleep(3 * 60 * 1000);
		Calendar StartTimeAfterRHSM = clienttasks.getCurrentConsumerCert().validityNotBefore;
		Calendar EndTimeAfterRHSM = clienttasks.getCurrentConsumerCert().validityNotAfter;
		String updatedCertdate = client.runCommandAndWait(
				"ls -lart /etc/pki/consumer/cert.pem | cut -d ' ' -f6,7,8")
				.getStdout();
		setDate(sm_serverHostname, sm_sshUser, sm_sshKeyPrivate,
				sm_sshkeyPassphrase, "date -s '15 year ago 9 month ago'");
		log.info("Changed the date of candlepin"
				+ client.runCommandAndWait("hostname"));
		setDate(sm_clientHostname, sm_sshUser, sm_sshKeyPrivate,
				sm_sshkeyPassphrase, "date -s '15 year ago 9 month ago'");
		listOfSectionNameValues.clear();
		listOfSectionNameValues.add(new String[] { "server", "insecure", "0" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		Assert.assertNotSame(StartTimeBeforeRHSM.getTime(),
				StartTimeAfterRHSM.getTime());
		Assert.assertNotSame(EndTimeBeforeRHSM.getTime(),
				EndTimeAfterRHSM.getTime());
		Assert.assertNotSame(existingCertdate, updatedCertdate);

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "subscription-manager unsubscribe --all on expired subscriptions removes certs from entitlement folder", groups = {
			"VerifyUnsubscribeAllForExpiredSubscription", "blockedByBug-852630","blockedByBug-906550" }, enabled = true)
	public void VerifyUnsubscribeAllForExpiredSubscription()
			throws JSONException, Exception {
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"certCheckInterval".toLowerCase(), "1" });
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String consumed = clienttasks.list_(null, null, true, null, null, null,null, null, null).getStdout();
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		File expectCertFile = new File(System.getProperty("automation.dir",
				null) + "/expiredcerts/Expiredcert.pem");
		RemoteFileTasks.putFile(client.getConnection(),
				expectCertFile.toString(), "/root/", "0755");
		clienttasks.importCertificate("/root/Expiredcert.pem");
		consumed = clienttasks.list_(null, null, true, null, null, null,null, null, null).getStdout();
		Assert.assertFalse((consumed == null));
		SSHCommandResult result = clienttasks.unsubscribe(true,(BigInteger) null, null, null, null);
		List<File> Entitlementcerts = clienttasks
				.getCurrentEntitlementCertFiles();
		String expected = Entitlementcerts.size()
				+ " subscriptions removed from this system.";
		Assert.assertEquals(result.getStdout().trim(), expected);

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify One empty certificate file in /etc/rhsm/ca causes registration failure", groups = {
			"VerifyEmptyCertCauseRegistrationFailure_Test",
	"blockedByBug-806958" }, enabled = true)
	public void VerifyEmptyCertCauseRegistrationFailure_Test()
			throws JSONException, Exception {
		clienttasks.unregister(null, null, null);
		String FilePath = myEmptyCaCertFile;
		String command = "touch " + FilePath;
		client.runCommandAndWait(command);
		String result = clienttasks.register_(sm_clientUsername,
				sm_clientPassword, sm_clientOrg, null, null, null, null, null,
				null, null, (String) null, null, null, null, null, null, null,
				null, null).getStdout();
		String Expected = "Bad CA certificate: " + FilePath;
		Assert.assertEquals(result.trim(), Expected);
		command = "rm -rf " + FilePath;
		client.runCommandAndWait(command);
		result = clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null)
				.getStdout();
		Assert.assertContainsMatch(result.trim(),
				"The system has been registered with id: [a-f,0-9,\\-]{36}");

	}
	
	@AfterGroups(groups = {"setup"}, value = {"VerifyEmptyCertCauseRegistrationFailure_Test"})
	public void removeMyEmptyCaCertFile() {
		client.runCommandAndWait("rm -f "+myEmptyCaCertFile);
	}
	@AfterGroups(groups = {"setup"}, value = {"VerifyEmptyCertCauseRegistrationFailure_Test"})
	public void moveProductCerts() {
		client.runCommandAndWait("rm -f "+myEmptyCaCertFile);
	}
	protected final String myEmptyCaCertFile = "/etc/rhsm/ca/myemptycert.pem";

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify facts update with incorrect proxy url produces traceback.", groups = {
			"VerifyFactsWithIncorrectProxy_Test", "blockedByBug-744504" }, enabled = true)
	public void VerifyFactsWithIncorrectProxy_Test() throws JSONException,
	Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String basicauthproxyUrl = String.format("%s:%s", "testmachine.com",
				sm_basicauthproxyPort);
		basicauthproxyUrl = basicauthproxyUrl.replaceAll(":$", "");
		String facts = clienttasks.facts_(null, true, basicauthproxyUrl, null,
				null).getStderr();
		String Expect = "Error updating system data on the server, see /var/log/rhsm/rhsm.log for more details.";
		Assert.assertEquals(facts.trim(), Expect);

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify Subscription Manager Leaves Broken Yum Repos After Unregister", groups = {
			"ReposListAfterUnregisterTest", "blockedByBug-674652" }, enabled = true)
	public void VerifyRepoAfterUnregisterTest() throws JSONException,
	Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe(true, (BigInteger)null, null, null, null);
		clienttasks.subscribeToTheCurrentlyAvailableSubscriptionPoolsIndividually();
		List<YumRepo> repos = clienttasks.getCurrentlySubscribedYumRepos();
		Assert.assertFalse(repos.isEmpty());
		clienttasks.unregister(null, null, null);
		List<YumRepo> repo = clienttasks.getCurrentlySubscribedYumRepos();
		Assert.assertTrue(repo.isEmpty());
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify if stacking entitlements reports as distinct entries in cli list --installed", groups = {
			"VerifyDistinct", "blockedByBug-733327" }, enabled = true)
	public void VerifyDistinctStackingEntires() throws Exception {
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		List<String> poolId = new ArrayList<String>();
		Map<String, String> factsMap = new HashMap<String, String>();
		clienttasks.register(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, null, null, true, null, null,(String) null, null, null, null, true, null, null, null, null);
		for (InstalledProduct installed : clienttasks.getCurrentlyInstalledProducts()) {
			if (!(installed.productId.equals("100000000000002"))){
				moveProductCertFiles(installed.productId + ".pem");
			}
		}
		int sockets = 4;
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(sockets));
		clienttasks.createFactsFileWithOverridingValues("/custom.facts",factsMap);
		clienttasks.facts(null, true, null, null, null);
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool pool : clienttasks.getCurrentlyAvailableSubscriptionPools()) {
			if (pool.multiEntitlement) {
				String poolProductSocketsAttribute = CandlepinTasks.getPoolProductAttributeValue(sm_clientUsername,	sm_clientPassword, sm_serverUrl, pool.poolId,"sockets");
				if ((!(poolProductSocketsAttribute == null))&& poolProductSocketsAttribute.equals("1")) {
					clienttasks.subscribe(null, null, pool.poolId, null, null,null, null, null, null, null, null);
					poolId.add(pool.poolId);
				}
			}}
		for (InstalledProduct installed : clienttasks.getCurrentlyInstalledProducts()) {
			if(installed.productId.equals("100000000000002")){
			Assert.assertEquals(installed.status, "Partially Subscribed");
			}
			}
		clienttasks.subscribe(null, null, poolId, null, null, null,null, null, null, null, null);
		for (InstalledProduct installedProduct : clienttasks.getCurrentlyInstalledProducts()) {
					if(installedProduct.productId.equals("100000000000002")){
					List<ProductSubscription> consumed = clienttasks.getCurrentlyConsumedProductSubscriptions();
					Assert.assertEquals(consumed.size(), sockets);
					Assert.assertEquals(installedProduct.status, "Subscribed");
		}}
		sockets = 1;
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(sockets));
		clienttasks.createFactsFileWithOverridingValues("/custom.facts",factsMap);
		clienttasks.facts(null, true, null, null, null);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify deletion of subscribed product", groups = {
			"DeleteProductTest", "blockedByBug-684941" }, enabled = true)
	public void VerifyDeletionOfSubscribedProduct_Test() throws JSONException,
	Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(List<String>) null, null, null, null, true, null, null, null, null);
		clienttasks.subscribe(true, null, null, (String) null, null, null,
				null, null, null, null, null);
		for (InstalledProduct installed : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installed.status.equals("Subscribed")) {
				for (SubscriptionPool AvailSub : clienttasks
						.getCurrentlyAvailableSubscriptionPools()) {
					if (installed.productName
							.contains(AvailSub.subscriptionName)) {
						String jsonConsumer = CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/products/" + AvailSub.productId);
						String expect = "{\"displayMessage\""+ ":"+ "\"Product with UUID '"+ AvailSub.productId+ "'"+ " cannot be deleted while subscriptions exist.\"}";
						Assert.assertEquals(expect, jsonConsumer);
					}
				}
			}
		}

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify Force Registration After Consumer is Deleted", groups = {
			"ForceRegAfterDEL", "blockedByBug-853876" }, enabled = true)
	public void VerifyForceRegistrationAfterConsumerDeletion_Test()
			throws JSONException, Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(List<String>) null, null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,
				sm_serverAdminPassword, sm_serverUrl, "/consumers/"
						+ consumerId);
		String result = clienttasks.register(sm_clientUsername,
				sm_clientPassword, sm_clientOrg, null, null, null, null, null,
				null, null, (List<String>) null, null, null, null, true, null,
				null, null, null).getStdout();

		Assert.assertContainsMatch(result.trim(),
				"The system has been registered with id: [a-f,0-9,\\-]{36}");

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify config Server port with blank or incorrect text produces traceback", groups = { "configBlankTest" }, enabled = true)
	// @ImplementsNitrateTest(caseId=)
	public void ConfigSetServerPortValueBlank_Test() {

		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		String section = "server";
		String name = "port";
		String newValue = clienttasks.getConfFileParameter(
				clienttasks.rhsmConfFile, section, name);
		listOfSectionNameValues.add(new String[] { section, name.toLowerCase(),
		"" });
		SSHCommandResult results = clienttasks.config(null, null, true,
				listOfSectionNameValues);
		String value = clienttasks.getConfFileParameter(
				clienttasks.rhsmConfFile, section, name);
		Assert.assertEquals("", results.getStdout().trim());
		listOfSectionNameValues.add(new String[] { section, name.toLowerCase(),
				newValue });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		value = clienttasks.getConfFileParameter(clienttasks.rhsmConfFile,
				section, name);
		Assert.assertEquals(value, newValue);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "subscription-manager: register --name , setting consumer name to blank", groups = {
			"registerwithname", "blockedByBug-627665" }, enabled = true)
	public void registerWithNameBlankTest() throws JSONException, Exception {
		String name = "test";
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, name, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		ConsumerCert consumerCert = clienttasks.getCurrentConsumerCert();
		Assert.assertEquals(consumerCert.name, name);
		name = "";
		SSHCommandResult result = clienttasks.register_(sm_clientUsername,
				sm_clientPassword, sm_clientOrg, null, null, name, null, null,
				null, null, (String) null, null, null, null, true, null, null,
				null, null);
		String expectedMsg = String
				.format("Error: system name can not be empty.");
		Assert.assertEquals(result.getExitCode(), new Integer(255));
		Assert.assertEquals(result.getStdout().trim(), expectedMsg);
		consumerCert = clienttasks.getCurrentConsumerCert();
		Assert.assertNotNull(consumerCert.name);

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "subscription-manager: register --consumerid  using a different user and valid consumerId", groups = {
			"reregister", "blockedByBug-627665" }, enabled = true)
	public void registerWithConsumerid_Test() throws JSONException, Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		clienttasks.unsubscribe(true, (BigInteger)null, null, null, null);
		List<SubscriptionPool> pools = clienttasks
				.getCurrentlyAvailableSubscriptionPools();
		if (pools.isEmpty())
			throw new SkipException(
					"Cannot randomly pick a pool for subscribing when there are no available pools for testing.");
		SubscriptionPool pool = pools
				.get(randomGenerator.nextInt(pools.size()));
		clienttasks.subscribeToSubscriptionPoolUsingPoolId(pool);
		List<ProductSubscription> consumedSubscriptionsBeforeregister = clienttasks
				.getCurrentlyConsumedProductSubscriptions();
		clienttasks.clean_(null, null, null);
		if (sm_client2Username==null) throw new SkipException("This test requires valid credentials for a second user.");
		clienttasks.register_(sm_client2Username, sm_client2Password,
				sm_client2Org, null, null, null, consumerId, null, null, null,
				(String) null, null, null, null, null, null, null, null, null);
		String consumerIdAfter = clienttasks.getCurrentConsumerId();
		Assert.assertEquals(consumerId, consumerIdAfter,
				"The consumer identity  has not changed after registering with consumerid.");
		List<ProductSubscription> consumedscriptionsAfterregister = clienttasks
				.getCurrentlyConsumedProductSubscriptions();
		Assert.assertTrue(
				consumedscriptionsAfterregister
				.containsAll(consumedSubscriptionsBeforeregister)
				&& consumedSubscriptionsBeforeregister.size() == consumedscriptionsAfterregister
				.size(),
				"The list of consumed products after reregistering is identical.");
	}

	/**
	 * @author skallesh
	 */
	@Test(description = "subscription-manager: service-level --org (without --list option)", groups = {
			"ServicelevelTest", "blockedByBug-826856" }, enabled = true)
	public void ServiceLevelWithOrgWithoutList_Test() {

		SSHCommandResult result;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(List<String>) null, null, null, null, true, null, null, null, null);
		result = clienttasks.service_level_(null, false, null, null,
				sm_clientUsername, sm_clientPassword, "MyOrg", null, null,
				null, null, null);
		Assert.assertEquals(result.getStdout().trim(),
				"Error: --org is only supported with the --list option");
	}

	/**
	 * @author skallesh
	 */
	@Test(description = "subscription-manager: facts --update (when registered)", groups = {
			"MyTestFacts", "blockedByBug-707525" }, enabled = true)
	public void FactsUpdateWhenregistered_Test() {

		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(List<String>) null, null, null, null, true, null, null, null, null);
		SSHCommandResult result = clienttasks.facts(null, true, null, null,
				null);
		Assert.assertEquals(result.getStdout().trim(),
				"Successfully updated the system facts.");
	}

	

	/**
	 * @author skallesh
	 */
	@Test(description = "subscription-manager: attempt register to with white space in the user name should fail", groups = {
			"registeredTests", "blockedByBug-719378" }, enabled = true)
	public void AttemptregisterWithWhiteSpacesInUsername_Test() {
		SSHCommandResult result = clienttasks.register_("user name",
				"password", sm_clientOrg, null, null, null, null, null, null,
				null, (String) null, null, null, null, true, null, null, null, null);
		Assert.assertEquals(
				result.getStderr().trim(),
				servertasks.invalidCredentialsMsg(),
				"The expected stdout result when attempting to register with a username containing whitespace.");
	}

	/**
	 * @author skallesh
	 * @throws JSONException
	 * @throws Exception
	 */
	@Test(description = "Auto-heal for partial subscription", groups = {
			"autohealPartial", "blockedByBug-746218","blockedByBug-907638" }, enabled = true)
	public void VerifyAutohealForPartialSubscription() throws Exception {
		Integer healFrequency = 3;
		Integer moreSockets = 0;
		List<String> productId = new ArrayList<String>();
		List<String> poolId = new ArrayList<String>();
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		Map<String, String> factsMap = new HashMap<String, String>();
		for (SubscriptionPool pool : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {
			if (pool.multiEntitlement) {
				String poolProductSocketsAttribute = CandlepinTasks
						.getPoolProductAttributeValue(sm_clientUsername,
								sm_clientPassword, sm_serverUrl, pool.poolId,
								"stacking_id");
				if ((!(poolProductSocketsAttribute == null))
						&& (poolProductSocketsAttribute.equals("1"))) {
					String SocketsCount = CandlepinTasks
							.getPoolProductAttributeValue(sm_clientUsername,
									sm_clientPassword, sm_serverUrl,
									pool.poolId, "sockets");

					poolId.add(pool.poolId);
					moreSockets = Integer.parseInt(SocketsCount) + 3;

				}
			}
		}
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(moreSockets));
		clienttasks.createFactsFileWithOverridingValues("/custom.facts",
				factsMap);
		clienttasks.facts(null, true, null, null, null);
		clienttasks.restart_rhsmcertd(null, healFrequency, false, null);
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);

		clienttasks.subscribe(null, null, poolId, null, null, null, null,
				null, null, null, null);

		for (InstalledProduct installedProduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installedProduct.status.equals("Partially Subscribed")) {
				productId.add(installedProduct.productId);
				Assert.assertEquals(installedProduct.status,
						"Partially Subscribed");

			}

		}
		SubscriptionManagerCLITestScript.sleep(healFrequency * 60 * 1000);

		for (InstalledProduct installedProduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			for (String product : productId) {
				if (product.equals(installedProduct.productId))
					Assert.assertEquals(installedProduct.status, "Subscribed");
			}
		}
		moreSockets = 1;
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(moreSockets));
		clienttasks.createFactsFileWithOverridingValues("/custom.facts",
				factsMap);
		clienttasks.facts(null, true, null, null, null);

	}

	/**
	 * @author skallesh
	 * @throws JSONException
	 * @throws Exception
	 */
	@Test(description = "Auto-heal with SLA", groups = { "AutoHealWithSLA","blockedByBug-907638" }, enabled = true)
	public void VerifyAutohealWithSLA() throws JSONException, Exception {
		Integer autoAttachInterval = 2;
		String filename = null;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		List<String> availableServiceLevelData = clienttasks
				.getCurrentlyAvailableServiceLevels();
		String availableService = availableServiceLevelData.get(randomGenerator
				.nextInt(availableServiceLevelData.size()));
		clienttasks.subscribe(true, availableService, (String) null, null,
				null, null, null, null, null, null, null);
		List<EntitlementCert> certs = clienttasks.getCurrentEntitlementCerts();
		if (certs.isEmpty()) {
			availableService = availableServiceLevelData.get(randomGenerator
					.nextInt(availableServiceLevelData.size()));
			clienttasks.subscribe(true, availableService, (String) null, null,
					null, null, null, null, null, null, null);
		}
		clienttasks.service_level_(null, null, null, null, null,
				availableService, null, null, null, null, null, null);
		clienttasks.restart_rhsmcertd(null, autoAttachInterval, false, null);
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		SubscriptionManagerCLITestScript.sleep(autoAttachInterval * 60 * 1000);
		certs = clienttasks.getCurrentEntitlementCerts();
		Assert.assertTrue(!(certs.isEmpty()),
				"autoheal is succesfull with Service level" + availableService);

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 */
	@Test(description = "verfying Auto-heal when auto-heal parameter is turned off", groups = { "AutohealTurnedOff" }, enabled = true)
	public void AutohealTurnedOff() throws Exception {
		Integer healFrequency = 2;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		JSONObject jsonConsumer = CandlepinTasks.setAutohealForConsumer(
				sm_clientUsername, sm_clientPassword, sm_serverUrl, consumerId,
				false);
		Assert.assertFalse(
				jsonConsumer.getBoolean("autoheal"),
				"A consumer's autoheal attribute value can be toggled off (expected value=false).");
		clienttasks.restart_rhsmcertd(null, healFrequency, false, null);

		SubscriptionManagerCLITestScript.sleep(healFrequency * 60 * 1000);
		List<EntitlementCert> certs = clienttasks.getCurrentEntitlementCerts();
		Assert.assertTrue((certs.isEmpty()), "autoheal is successful");

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */

	@Test(description = "Verify if Subscription manager displays incorrect status for partially subscribed subscription", groups = {
			"VerifyStatusForPartialSubscription", "blockedByBug-743710" }, enabled = true)
	@ImplementsNitrateTest(caseId = 119327)
	public void VerifyStatusForPartialSubscription() throws JSONException,
	Exception {
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		String Flag = "false";
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		List<ProductSubscription> consumed = clienttasks
				.getCurrentlyConsumedProductSubscriptions();
		if (!(consumed.isEmpty())) {
			clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		}
		Map<String, String> factsMap = new HashMap<String, String>();
		Integer moreSockets = 4;
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(moreSockets));
		clienttasks.createFactsFileWithOverridingValues("/socket.facts",factsMap);
		clienttasks.facts(null, true, null, null, null);
		for (SubscriptionPool SubscriptionPool : clienttasks
				.getCurrentlyAllAvailableSubscriptionPools()) {
			if (!(SubscriptionPool.multiEntitlement)) {
				String poolProductSocketsAttribute = CandlepinTasks
						.getPoolProductAttributeValue(sm_clientUsername,
								sm_clientPassword, sm_serverUrl,
								SubscriptionPool.poolId, "sockets");
				if ((!(poolProductSocketsAttribute == null))
						&& (poolProductSocketsAttribute.equals("2"))) {
					clienttasks.subscribeToSubscriptionPool_(SubscriptionPool);
				}
			}
		}
		for (InstalledProduct product : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (product.status.equals("Partially Subscribed")) {
				Flag = "true";
			}
		}
		moreSockets = 1;
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(moreSockets));
		clienttasks.createFactsFileWithOverridingValues("/socket.facts",
				factsMap);
		Assert.assertEquals(Flag, "true");
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Auto-heal for Expired subscription", groups = {
			"AutohealForExpired", "blockedByBug-746088","blockedByBug-907638" }, enabled = true)
	public void VerifyAutohealForExpiredSubscription() throws JSONException,
	Exception {
		int healFrequency = 2;
		List<String> Expiredproductid = new ArrayList<String>();
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		JSONObject jsonConsumer = CandlepinTasks.setAutohealForConsumer(
				sm_clientUsername, sm_clientPassword, sm_serverUrl, consumerId,
				true);
		Assert.assertTrue(jsonConsumer.getBoolean("autoheal"),
				"A consumer's autoheal attribute value=true.");

		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		clienttasks.service_level_(null, null, null, true, null, null, null,
				null, null, null, null, null);
		File expectCertFile = new File(System.getProperty("automation.dir",
				null) + "/expiredcerts/Expiredcert.pem");
		RemoteFileTasks.putFile(client.getConnection(),
				expectCertFile.toString(), "/root/", "0755");
		clienttasks.importCertificate_("/root/Expiredcert.pem");
		for (InstalledProduct product : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (product.status.equals("Expired"))
				Expiredproductid.add(product.productId);
		}
		if ((Expiredproductid.size() == 0)) {
			throw new SkipException(
					"No expired products are available for testing");
		} else {
			clienttasks.restart_rhsmcertd(null, healFrequency, false, null);
			SubscriptionManagerCLITestScript.sleep(healFrequency * 60 * 1000);
			for (InstalledProduct product : clienttasks
					.getCurrentlyInstalledProducts()) {
				for (int i = 0; i < Expiredproductid.size(); i++) {

					if (product.productId.equals(Expiredproductid.get(i)))
						Assert.assertEquals(product.status, "Subscribed");

				}
			}
		}
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Auto-heal for subscription", groups = { "AutoHeal","blockedByBug-907638" }, enabled = true)
	@ImplementsNitrateTest(caseId = 119327)
	public void VerifyAutohealForSubscription() throws JSONException, Exception {
		Integer healFrequency = 2;
		clienttasks.register(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, null, null, null, null, null,(String) null, null, null, null, true, null, null, null, null);
		clienttasks.unsubscribeFromAllOfTheCurrentlyConsumedProductSubscriptions();
		String consumerId = clienttasks.getCurrentConsumerId();
		JSONObject jsonConsumer = CandlepinTasks.setAutohealForConsumer(sm_clientUsername, sm_clientPassword, sm_serverUrl, consumerId,true);
		Assert.assertTrue(jsonConsumer.getBoolean("autoheal"),"A consumer's autoheal attribute value=true.");
		clienttasks.service_level_(null, null, null, true, null, null, null,null, null, null, null, null);
		clienttasks.restart_rhsmcertd(null, healFrequency, false, null);
		SubscriptionManagerCLITestScript.sleep(healFrequency * 60 * 1000);
		List<EntitlementCert> certs = clienttasks.getCurrentEntitlementCerts();
		List<ProductSubscription> consumed = clienttasks.getCurrentlyConsumedProductSubscriptions();
		log.info("Currently the consumed products are" + consumed.size());
		Assert.assertTrue((!(certs.isEmpty())), "autoheal is successful");
	}

	/**
	 * @author skallesh
	 * @throws JSONException
	 * @throws Exception
	 */
	@Test(description = "Auto-heal with SLA", groups = { "AutoHealFailForSLA" }, enabled = true)
	public void VerifyAutohealFailForSLA() throws JSONException, Exception {
		Integer healFrequency = 2;
		String filename = null;
		client.runCommandAndWait("mkdir -p " + "/etc/pki/faketmp");
		client.runCommandAndWait("mv " + clienttasks.productCertDir + "/*_.pem"
				+ " " + "/etc/pki/faketmp/");
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		List<String> availableServiceLevelData = clienttasks
				.getCurrentlyAvailableServiceLevels();
		String availableService = availableServiceLevelData.get(randomGenerator
				.nextInt(availableServiceLevelData.size()));
		clienttasks.service_level_(null, null, null, null, null, null, null,
				null, null, null, null, null);
		clienttasks.subscribe(true, availableService, (String) null, null,
				null, null, null, null, null, null, null);
		for (InstalledProduct installedProduct : clienttasks
				.getCurrentlyInstalledProducts()) {

			if (installedProduct.status.trim().equalsIgnoreCase("Subscribed")
					|| installedProduct.status.trim().equalsIgnoreCase(
							"Partially Subscribed")) {
				filename = installedProduct.productId + ".pem";
				moveProductCertFiles(filename);
			}
		}
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		List<EntitlementCert> certsbeforeRHSMService = clienttasks
				.getCurrentEntitlementCerts();
		log.info("cert contents are " + certsbeforeRHSMService);
		clienttasks.subscribe(true, null, null, (List<String>) null, null,
				null, null, null, null, null, null);
		clienttasks.restart_rhsmcertd(null, healFrequency, false, null);
		SubscriptionManagerCLITestScript.sleep(healFrequency * 60 * 1000);
		List<EntitlementCert> certs = clienttasks.getCurrentEntitlementCerts();
		if (!(certs.isEmpty()))
		client.runCommandAndWait("mv " + "/etc/pki/faketmp/*.pem" + " "
				+ clienttasks.productCertDir);
		client.runCommandAndWait("rm -rf " + "/etc/pki/faketmp");
		Assert.assertTrue((certs.isEmpty()), "autoheal has failed");
	}

	
	/**
	 * @author skallesh
	 */

	@Test(description = "subscription-manager: subscribe multiple pools in incorrect format", groups = {
			"MysubscribeTest", "blockedByBug-772218" }, enabled = true)
	public void VerifyIncorrectSubscriptionFormat() {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		List<String> poolid = new ArrayList<String>();
		for (SubscriptionPool pool : clienttasks
				.getCurrentlyAllAvailableSubscriptionPools()) {
			poolid.add(pool.poolId);
		}
		if (poolid.isEmpty())
			throw new SkipException(
					"Cannot randomly pick a pool for subscribing when there are no available pools for testing.");
		int i = randomGenerator.nextInt(poolid.size());
		int j = randomGenerator.nextInt(poolid.size());
		if (i == j) {
			j = randomGenerator.nextInt(poolid.size());
		}
			SSHCommandResult subscribeResult = subscribeInvalidFormat_(null,
					null, poolid.get(i), poolid.get(j), null, null, null, null,
					null, null, null, null);
			Assert.assertEquals(subscribeResult.getStdout().trim(),
					"cannot parse argument: " + poolid.get(j));
		

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 */
	@Test(description = "Verify that Entitlement Start Dates is the Subscription Start Date ", groups = {
			"VerifyEntitlementStartDateIsSubStartDate_Test",
	"blockedByBug-670831" }, enabled = true)
	public void VerifyEntitlementStartDate_Test() throws JSONException,
	Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe(true, (BigInteger)null, null, null, null);
		for (SubscriptionPool pool : clienttasks.getCurrentlyAvailableSubscriptionPools()) {
			JSONObject jsonPool = new JSONObject(CandlepinTasks.getResourceUsingRESTfulAPI(
					sm_clientUsername, sm_clientPassword, sm_serverUrl,"/pools/" + pool.poolId));
			Calendar subStartDate = parseISO8601DateString(jsonPool.getString("startDate"), "GMT");
			EntitlementCert entitlementCert = clienttasks.getEntitlementCertFromEntitlementCertFile(clienttasks.subscribeToSubscriptionPool_(pool));
			Calendar entStartDate = entitlementCert.validityNotBefore;
			Assert.assertEquals(entStartDate,subStartDate,"The entitlement start date '"
							+ EntitlementCert.formatDateString(entStartDate)
							+ "' granted from pool " + pool.poolId
							+ " should equal its subscription start date '"
							+ OrderNamespace.formatDateString(subStartDate)
							+ "'.");
		}
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 */
	@Test(description = "Verify if architecture for auto-subscribe test", groups = { "VerifyarchitectureForAutobind_Test" }, enabled = true)
	public void VerifyarchitectureForAutobind_Test() throws Exception {

		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		Map<String, String> result = clienttasks.getFacts();
		String arch = result.get("uname.machine");
		List<String> cpu_arch = new ArrayList<String>();
		String input = "x86_64|i686|ia64|ppc|ppc64|s390x|s390";
		String[] values = input.split("\\|");
		Boolean flag = false;
		Boolean expected = true;
		for (int i = 0; i < values.length; i++) {
			cpu_arch.add(values[i]);
		}

		Pattern p = Pattern.compile(arch);
		Matcher matcher = p.matcher(input);
		while (matcher.find()) {
			String pattern_ = matcher.group();
			cpu_arch.remove(pattern_);

		}
		String architecture = cpu_arch.get(randomGenerator.nextInt(cpu_arch
				.size()));
		for (SubscriptionPool pool : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {
			if ((pool.subscriptionName).contains(" " + architecture)) {
				flag = true;
				Assert.assertEquals(flag, expected);
			}

		}

		for (SubscriptionPool pools : clienttasks
				.getCurrentlyAllAvailableSubscriptionPools()) {
			if ((pools.subscriptionName).contains(architecture)) {
				flag = true;
				Assert.assertEquals(flag, expected);
			}

		}
		Map<String, String> factsMap = new HashMap<String, String>();
		factsMap.put("uname.machine", String.valueOf(architecture));
		clienttasks.createFactsFileWithOverridingValues("/socket.facts",
				factsMap);
		clienttasks.facts(null, true, null, null, null);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 */
	@Test(description = "Verify if rhsm not logging subscriptions and products properly ", groups = { "VerifyRhsmLogging_Test","blockedByBug-668032","blockedByBug-907638" }, enabled = true)
	public void VerifyRhsmLoggingTest() throws Exception {
		Boolean actual = true;
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		String LogMarker = System.currentTimeMillis()+" Testing ***************************************************************";
		RemoteFileTasks.markFile(client, clienttasks.rhsmLogFile, LogMarker);
		for (SubscriptionPool pool : clienttasks.getCurrentlyAllAvailableSubscriptionPools()) {
			List<String> providedProducts = CandlepinTasks.getPoolProvidedProductIds(sm_clientUsername,sm_clientPassword, sm_serverUrl, pool.poolId);
			if ((providedProducts.size()) > 2) {
			
				clienttasks.subscribe(null, null, pool.poolId, null, null,
						null, null, null, null, null, null);
			}
	
			Boolean flag = waitForRegexInRhsmLog("@ /etc/pki/entitlement",RemoteFileTasks.getTailFromMarkedFile(client, clienttasks.rhsmLogFile, LogMarker, null));
			Assert.assertEquals(flag, actual);
			

		}

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 */
	@Test(description = "Verify if the status of installed products match when autosubscribed,and when you subscribe all the available products ", groups = {"VerifyFuturesubscription_Test", "blockedByBug-746035" }, enabled = true)
	public void VerifyFuturesubscription_Test() throws Exception {
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		Calendar now = new GregorianCalendar();
		List<String> productId = new ArrayList<String>();
		now.add(Calendar.YEAR, 1);
		DateFormat yyyy_MM_dd_DateFormat = new SimpleDateFormat("yyyy-MM-dd");
		String onDateToTest = yyyy_MM_dd_DateFormat.format(now.getTime());
		clienttasks.subscribe(true, null, (String) null, null, null, null,
				null, null, null, null, null);
		for (InstalledProduct installed : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installed.status.equals("Not Subscribed")
					&& installed.status.equals("Partially Subscribed"))
				moveProductCertFiles(installed.productId + ".pem");
			moveProductCertFiles(installed.productId + "_.pem");
		}
		for (SubscriptionPool availOnDate : getAvailableFutureSubscriptionsOndate(onDateToTest)) {
			clienttasks.subscribe(null, null, availOnDate.poolId, null, null,
					null, null, null, null, null, null);
		}
		for (InstalledProduct installedproduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installedproduct.status.equals("Future Subscription")) {

				productId.add(installedproduct.productId);
			}
		}
		clienttasks.subscribe(true, null, (String) null, null, null, null,
				null, null, null, null, null);
		for (InstalledProduct installedproduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			for (String productid : productId) {
				if (installedproduct.productId.equals(productid)) {
					Assert.assertEquals(installedproduct.status.trim(),
							"Subscribed");

				}
			}
		}

		for (InstalledProduct installedproduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			for (String productid : productId) {
				if (installedproduct.productId.equals(productid)) {
					Assert.assertEquals(installedproduct.status.trim(),
							"Subscribed");

				}
			}
		}

	}

	protected Calendar parseISO8601DateString(String dateString, String timeZone) {
		String iso8601DatePattern = "yyyy-MM-dd'T'HH:mm:ss.SSSZ";
		String datePattern = iso8601DatePattern;
		if (timeZone == null)
			datePattern = datePattern.replaceFirst("Z$", ""); // strip off final
		// timezone
		// offset symbol
		// from
		// iso8601DatePattern
		return parseDateStringUsingDatePattern(dateString, datePattern,
				timeZone);
	}

	protected Calendar parseDateStringUsingDatePattern(String dateString,
			String datePattern, String timeZone) {
		try {
			DateFormat dateFormat = new SimpleDateFormat(datePattern); // format="yyyy-MM-dd'T'HH:mm:ss.SSSZ"
			// will
			// parse
			// dateString="2012-02-08T00:00:00.000+0000"
			if (timeZone != null)
				dateFormat.setTimeZone(TimeZone.getTimeZone(timeZone)); // timeZone="GMT"
			Calendar calendar = new GregorianCalendar();
			calendar.setTimeInMillis(dateFormat.parse(dateString).getTime());
			return calendar;
		} catch (ParseException e) {
			log.warning("Failed to parse " + (timeZone == null ? "" : timeZone)
					+ " date string '" + dateString + "' with format '"
					+ datePattern + "':\n" + e.getMessage());
			return null;
		}
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify if the status of installed products match when autosubscribed,and when you subscribe all the available products ", groups = { "VerifyautosubscribeTest" }, enabled = true)
	public void VerifyautosubscribeTest() throws JSONException, Exception {

		List<String> ProductIdBeforeAuto = new ArrayList<String>();
		List<String> ProductIdAfterAuto = new ArrayList<String>();
		clienttasks.deleteFactsFileWithOverridingValues();
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		clienttasks.subscribeToTheCurrentlyAvailableSubscriptionPoolsIndividually();
		for (InstalledProduct installedProductsBeforeAuto : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installedProductsBeforeAuto.status.equals("Subscribed"))
				ProductIdBeforeAuto.add(installedProductsBeforeAuto.productId);
		}

		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
		clienttasks.subscribe(true, null, (String) null, null, null, null,
				null, null, null, null, null);
		for (InstalledProduct installedProductsAfterAuto : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installedProductsAfterAuto.status.equals("Subscribed"))
				ProductIdAfterAuto.add(installedProductsAfterAuto.productId);
		}
		Assert.assertEquals(ProductIdBeforeAuto.size(),
				ProductIdAfterAuto.size());
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 */
	@Test(description = "Verify if autosubscribe ignores socket count on non multi-entitled subscriptions ", groups = { "VerifyautosubscribeIgnoresSocketCount_Test" }, enabled = true)
	public void VerifyautosubscribeIgnoresSocketCount_Test() throws Exception {
		int socketnum = 0;
		int socketvalue = 0;
		List<String> SubscriptionId = new ArrayList<String>();
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		for (SubscriptionPool SubscriptionPool : clienttasks.getCurrentlyAllAvailableSubscriptionPools()) {
			if (!(SubscriptionPool.multiEntitlement)) {
				SubscriptionId.add(SubscriptionPool.subscriptionName);
				String poolProductSocketsAttribute = CandlepinTasks
						.getPoolProductAttributeValue(sm_clientUsername,
								sm_clientPassword, sm_serverUrl,
								SubscriptionPool.poolId, "sockets");
				if (!(poolProductSocketsAttribute == null)) {
					socketvalue = Integer.parseInt(poolProductSocketsAttribute);
					if (socketvalue > socketnum) {
						socketnum = socketvalue;
					}
				} else {
					socketvalue = 0;
				}
			}
			Map<String, String> factsMap = new HashMap<String, String>();
			factsMap.put("cpu.cpu_socket(s)", String.valueOf(socketnum + 2));
			clienttasks.createFactsFileWithOverridingValues(factsMap);
			clienttasks.facts(null, true, null, null, null);

		}
		clienttasks.subscribe(true, null, (String) null, null, null, null,
				null, null, null, null, null);
		for (InstalledProduct installedProductsAfterAuto : clienttasks
				.getCurrentlyInstalledProducts()) {
			for (String pool : SubscriptionId) {
				if (installedProductsAfterAuto.productName.contains(pool))

					if ((installedProductsAfterAuto.status)
							.equalsIgnoreCase("Subscribed")) {
						Map<String, String> factsMap = new HashMap<String, String>();
						factsMap.put("cpu.cpu_socket(s)", String.valueOf(1));
						clienttasks
						.createFactsFileWithOverridingValues(factsMap);
						clienttasks.facts(null, true, null, null, null);
						Assert.assertEquals("Subscribed",
								(installedProductsAfterAuto.status).trim(),
								"test  has failed");
					}
			}
		}
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "subscription-manager: entitlement key files created with weak permissions", groups = {
			"MykeyTest", "blockedByBug-720360" }, enabled = true)
	public void VerifyKeyFilePermissions() throws JSONException, Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		clienttasks.subscribeToTheCurrentlyAvailableSubscriptionPoolsCollectively();
		String subscribeResult = getEntitlementCertFilesWithPermissions();
		Pattern p = Pattern.compile("[,\\s]+");
		String[] result = p.split(subscribeResult);
		for (int i = 0; i < result.length; i++) {
			Assert.assertEquals(result[i], "-rw-------.",
					"permission for etc/pki/entitlement/<serial>-key.pem is -rw-------");
			i++;
		}
	}

	@BeforeGroups(groups = "setup", value = { "VerifyDistinct",
			"VerifyStatusForPartialSubscription", "AutoHeal",
			"AutoHealFailForSLA", "VerifyautosubscribeTest", "validTest",
			"BugzillaTests", "autohealPartial",
			"VerifyEntitlementStartDate_Test", "reregister" }, enabled = true)
	public void unsubscribeBeforeGroup() {
		clienttasks.unsubscribe(true, (BigInteger) null, null, null, null);
	}

	@BeforeGroups(groups = "setup", value = { "VerifyDistinct", "AutoHeal",
			"autohealPartial", "BugzillaTests" }, enabled = true)
	public void unsetServicelevelBeforeGroup() {
		clienttasks.service_level_(null, null, null, true, null, null, null,
				null, null, null, null, null);
	}
	
	

	@BeforeGroups(groups = "setup", value = { "VerifyDistinct", "AutoHeal",
			"VerifyStatusForPartialSubscription", "autohealPartial",
			"VerifyEntitlementStartDate_Test", "BugzillaTests" }, enabled = true)
	public void setHealFrequencyGroup() {
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"autoAttachInterval".toLowerCase(), "1440" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
		String param = clienttasks.getConfFileParameter(
				clienttasks.rhsmConfFile, "rhsmcertd", "autoAttachInterval");

		Assert.assertEquals(param, "1440");
	}
	
	@AfterGroups(groups = "setup", value = {"VerifyRepoFileExistance"}, enabled = true)
	public void TurnonRepos(){
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsm","manage_repos", "1" });
		clienttasks.config(null, null, true, listOfSectionNameValues);
	}
	@BeforeGroups(groups = "setup", value = { "autohealPartial", "AutoHeal",
			"heal", "BugzillaTests", "AutoHealFailForSLA", "AutohealForExpired" }, enabled = false)
	public void VerifyAutohealAttributeDefaultsToTrueForNewSystemConsumer_Test()
			throws Exception {

		String consumerId = clienttasks
				.getCurrentConsumerId(clienttasks.register(sm_clientUsername,
						sm_clientPassword, sm_clientOrg, null, null, null,
						null, null, null, null, (String) null, null, null,
						null, true, null, null, null, null));
		JSONObject jsonConsumer = CandlepinTasks.setAutohealForConsumer(
				sm_clientUsername, sm_clientPassword, sm_serverUrl, consumerId,
				true);

		Assert.assertTrue(jsonConsumer.getBoolean("autoheal"),
				"A new system consumer's autoheal attribute value defaults to true.");
	}

	protected Integer configuredHealFrequency = null;

	@BeforeClass(groups = "setup")
	public void rememberConfiguredHealFrequency() {
		if (clienttasks == null)
			return;
		configuredHealFrequency = Integer.valueOf(clienttasks
				.getConfFileParameter(clienttasks.rhsmConfFile, "rhsmcertd",
						"autoAttachInterval"));
	}

	@AfterClass(groups = "setup")
	public void restoreConfiguredHealFrequency() {
		if (clienttasks == null)
			return;
		clienttasks.restart_rhsmcertd(null, configuredHealFrequency, false,
				null);
	}
	@AfterGroups(groups = { "setup" }, value = { "validTest","AutoHealWithSLA","AutoHealFailForSLA","VerifyFuturesubscription_Test","VerifySubscriptionOfBestProductWithUnattendedRegistration",
	"VerifySystemCompliantFactWhenAllProductsAreExpired_Test","systemEntitlementsValidityAfterOversubscribing","certificateStacking","UpdateWithNoInstalledProducts","VerifyHealingForFuturesubscription"
	,"VerifyautosubscribeIgnoresSocketCount_Test","VerifyDistinct" })
	@AfterClass(groups = "setup")
	public void restoreProductCerts() {
		if (clienttasks == null)
			return;
		client.runCommandAndWait("mv " + "/etc/pki/tmp1/*.pem" + " "
				+ clienttasks.productCertDir);
		client.runCommandAndWait("rm -rf " + "/etc/pki/tmp1");
	}

	@AfterGroups(groups = { "setup" }, value = { "VerifyautosubscribeTest",
	"VerifyautosubscribeIgnoresSocketCount_Test" })
	@AfterClass(groups = { "setup" })
	// insurance
	public void deleteFactsFileWithOverridingValues() {
		clienttasks.deleteFactsFileWithOverridingValues();
	}

	// Protected methods
	// ***********************************************************************

	protected void setDate(String hostname, String user, String passphrase,
			String privatekey, String datecmd) throws IOException {
		client = new SSHCommandRunner(hostname, user, passphrase, privatekey,
				null);
		client.runCommandAndWait(datecmd);

	}

	protected void moveProductCertFiles(String filename) {
		client.runCommandAndWait("mkdir -p " + "/etc/pki/tmp1");
		client.runCommandAndWait("mv " + clienttasks.productCertDir + "/"
					+ filename + " " + "/etc/pki/tmp1/");
			
	}

	protected String getEntitlementCertFilesWithPermissions() {
		String lsFiles = client.runCommandAndWait(
				"ls -l " + clienttasks.entitlementCertDir + "/*-key.pem"
						+ " | cut -d " + "' '" + " -f1,9").getStdout();
		return lsFiles;
	}

	protected SSHCommandResult unsubscribeFromMultipleEntitlementsUsingSerialNumber(
			BigInteger SerialNumOne, BigInteger SerialNumTwo) {
		String command = clienttasks.command;
		command += " unsubscribe";
		if (SerialNumOne != null && SerialNumTwo != null)
			command += " --serial=" + SerialNumOne + " " + "--serial="
					+ SerialNumTwo;

		// run command without asserting results
		return client.runCommandAndWait(command);
	}

	protected SSHCommandResult subscribeInvalidFormat_(Boolean auto,
			String servicelevel, String poolIdOne, String poolIdTwo,
			List<String> productIds, List<String> regtokens, String quantity,
			String email, String locale, String proxy, String proxyuser,
			String proxypassword) {

		String command = clienttasks.command;
		command += " subscribe";
		if (poolIdOne != null && poolIdTwo != null)
			command += " --pool=" + poolIdOne + " " + poolIdTwo;

		// run command without asserting results
		return client.runCommandAndWait(command);
	}

	public Boolean waitForRegexInRhsmLog(String logRegex, String input) {
		
		Pattern pattern = Pattern.compile(logRegex, Pattern.MULTILINE);
		Matcher matcher = pattern.matcher(input);
		int count = 0;
		Boolean flag = false;
		while (matcher.find()) {
			count++;
		}
		if (count >= 2) {
			flag = true;
		}
		return flag;

	}

	/**
	 * @return list of objects representing the subscription-manager list
	 *         --avail --ondate
	 */
	public List<SubscriptionPool> getAvailableFutureSubscriptionsOndate(
			String onDateToTest) {
		return SubscriptionPool.parse(clienttasks.list_(null, true, null, null,
				null, onDateToTest, null, null, null).getStdout());
	}

	protected List<String> listFutureSubscription_OnDate(Boolean available,
			String ondate) {
		List<String> PoolId = new ArrayList<String>();
		SSHCommandResult result = clienttasks.list_(true, true, null, null,
				null, ondate, null, null, null);
		List<SubscriptionPool> Pool = SubscriptionPool
				.parse(result.getStdout());
		for (SubscriptionPool availablePool : Pool) {
			if (availablePool.multiEntitlement) {
				PoolId.add(availablePool.poolId);
			}
		}

		return PoolId;
	}

	@DataProvider(name="getPackageFromEnabledRepoAndSubscriptionPoolData")
	public Object[][] getPackageFromEnabledRepoAndSubscriptionPoolDataAs2dArray() throws JSONException, Exception {
		return TestNGUtils.convertListOfListsTo2dArray(getPackageFromEnabledRepoAndSubscriptionPoolDataAsListOfLists());
	}
	
	protected List<List<Object>> getPackageFromEnabledRepoAndSubscriptionPoolDataAsListOfLists() throws JSONException, Exception {
		List<List<Object>> ll = new ArrayList<List<Object>>(); if (!isSetupBeforeSuiteComplete) return ll;
		if (clienttasks==null) return ll;
		if (sm_clientUsername==null) return ll;
		if (sm_clientPassword==null) return ll;
		
		// get the currently installed product certs to be used when checking for conditional content tagging
		List<ProductCert> currentProductCerts = clienttasks.getCurrentProductCerts();
		
		// assure we are freshly registered and process all available subscription pools
		clienttasks.register(sm_clientUsername, sm_clientPassword, sm_clientOrg, null, ConsumerType.system, null, null, null, null, null, (String)null, null, null, null, Boolean.TRUE, false, null, null, null);
		for (SubscriptionPool pool : clienttasks.getCurrentlyAvailableSubscriptionPools()) {
			
			File entitlementCertFile = 		clienttasks.subscribeToSubscriptionPoolUsingPoolId(pool);
			Assert.assertNotNull(entitlementCertFile, "Found the entitlement cert file that was granted after subscribing to pool: "+pool);
			EntitlementCert entitlementCert = clienttasks.getEntitlementCertFromEntitlementCertFile(entitlementCertFile);
			for (ContentNamespace contentNamespace : entitlementCert.contentNamespaces) {
				if (!contentNamespace.type.equalsIgnoreCase("yum")) continue;
				if (contentNamespace.enabled && clienttasks.areAllRequiredTagsInContentNamespaceProvidedByProductCerts(contentNamespace, currentProductCerts)) {
					String repoLabel = contentNamespace.label;
					
					// find an available package that is uniquely provided by repo
					String pkg = clienttasks.findUniqueAvailablePackageFromRepo(repoLabel);
					if (pkg==null) {
						log.warning("Could NOT find a unique available package from repo '"+repoLabel+"' after subscribing to SubscriptionPool: "+pool);
					}

					// String availableGroup, String installedGroup, String repoLabel, SubscriptionPool pool
					ll.add(Arrays.asList(new Object[]{pkg, repoLabel, pool}));
				}
			}
			clienttasks.unsubscribeFromSerialNumber(clienttasks.getSerialNumberFromEntitlementCertFile(entitlementCertFile));

			// minimize the number of dataProvided rows (useful during automated testcase development)
			if (Boolean.valueOf(getProperty("sm.debug.dataProviders.minimize","false"))) break;
		}
		
		return ll;
	}
	
	/**
	 * @param startingMinutesFromNow
	 * @param endingMinutesFromNow
	 * @return poolId to the newly available SubscriptionPool
	 * @throws JSONException
	 * @throws Exception
	 */
	protected String createTestPool(int startingMinutesFromNow, int endingMinutesFromNow) throws JSONException, Exception  {
		Calendar endCalendar = new GregorianCalendar();
		endCalendar.add(Calendar.MINUTE, endingMinutesFromNow);
		Date endDate = endCalendar.getTime();
		DateFormat EndDateFormat = new SimpleDateFormat("MM/dd/yy hh:mm a");
		EndingDate=EndDateFormat.format(endDate);
		
		if (true) return CandlepinTasks.createSubscriptionAndRefreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey, 3, startingMinutesFromNow, endingMinutesFromNow, getRandInt(), getRandInt(), randomAvailableProductId, null).getString("id");
// TODO DELETE THE REST OF THIS METHOD'S CODE WHEN WE KNOW THE ABOVE CANDLEPIN TASK IS WORKING 8/12/2011
		
		// set the start and end dates
		
		Calendar startCalendar = new GregorianCalendar();
		startCalendar.add(Calendar.MINUTE, startingMinutesFromNow);
		Date startDate = startCalendar.getTime();
		
		// randomly choose a contract number
		Integer contractNumber = Integer.valueOf(getRandInt());
		
		// randomly choose an account number
		Integer accountNumber = Integer.valueOf(getRandInt());
		
		// choose a product id for the subscription
		//String productId =  "MKT-rhel-server";  // too hard coded
		//JSONArray jsonProducts = new JSONArray(CandlepinTasks.getResourceUsingRESTfulAPI(serverHostname,serverPort,serverPrefix,serverAdminUsername,serverAdminPassword,"/products"));	
		//String productId = null;
		//do {	// pick a random productId (excluding a personal productId) // too random; could pick a product that is not available to this system
		//	productId =  ((JSONObject) jsonProducts.get(randomGenerator.nextInt(jsonProducts.length()))).getString("id");
		//} while (getPersonProductIds().contains(productId));
		String productId = randomAvailableProductId;
		// choose providedProducts for the subscription
		//String[] providedProducts = {"37068", "37069", "37060"};
		//String[] providedProducts = {
		//	((JSONObject) jsonProducts.get(randomGenerator.nextInt(jsonProducts.length()))).getString("id"),
		//	((JSONObject) jsonProducts.get(randomGenerator.nextInt(jsonProducts.length()))).getString("id"),
		//	((JSONObject) jsonProducts.get(randomGenerator.nextInt(jsonProducts.length()))).getString("id")
		//};
		List<String> providedProducts = null;
		
		// create the subscription
		String requestBody = CandlepinTasks.createSubscriptionRequestBody(3, startDate, endDate, productId, contractNumber, accountNumber, providedProducts).toString();
		JSONObject jsonSubscription = new JSONObject(CandlepinTasks.postResourceUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, "/owners/" + ownerKey + "/subscriptions", requestBody));
		
		// refresh the pools
		JSONObject jobDetail = CandlepinTasks.refreshPoolsUsingRESTfulAPI(sm_serverAdminUsername, sm_serverAdminPassword, sm_serverUrl, ownerKey);
		jobDetail = CandlepinTasks.waitForJobDetailStateUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword,sm_serverUrl,jobDetail,"FINISHED", 5*1000, 1);
		
		// assemble an activeon parameter set to the start date so we can pass it on to the REST API call to find the created pool
		DateFormat iso8601DateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");	

		// "2012-02-08T00:00:00.000+0000"
		String iso8601FormatedDateString = iso8601DateFormat.format(startDate);
		iso8601FormatedDateString = iso8601FormatedDateString.replaceFirst("(..$)", ":$1");	
		
		
		// "2012-02-08T00:00:00.000+00:00"	// see https://bugzilla.redhat.com/show_bug.cgi?id=720493 // http://books.xmlschemata.org/relaxng/ch19-77049.html requires a colon in the time zone for xsd:dateTime
		String urlEncodedActiveOnDate = java.net.URLEncoder.encode(iso8601FormatedDateString, "UTF-8");	// "2012-02-08T00%3A00%3A00.000%2B00%3A00"	encode the string to escape the colons and plus signs so it can be passed as a parameter on an http call

		// loop through all pools available to owner and find the newly created poolid corresponding to the new subscription id activeon startDate
		String poolId = null;
		String subscriptionId=null;
		JSONArray jsonPools = new JSONArray(CandlepinTasks.getResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword,sm_serverUrl,"/owners/"+ownerKey+"/pools"+"?activeon="+urlEncodedActiveOnDate));	
		for (int i = 0; i < jsonPools.length(); i++) {
			JSONObject jsonPool = (JSONObject) jsonPools.get(i);
			//if (contractNumber.equals(jsonPool.getInt("contractNumber"))) {
			if (jsonPool.getString("subscriptionId").equals(jsonSubscription.getString("id"))) {
				
				poolId = jsonPool.getString("id");
				break;
			}
		}
		Assert.assertNotNull(poolId,"Found newly created pool corresponding to the newly created subscription with id: "+jsonSubscription.getString("id"));
		log.info("The newly created subscription pool with id '"+poolId+"' will start '"+startingMinutesFromNow+"' minutes from now.");
		log.info("The newly created subscription pool with id '"+poolId+"' will expire '"+endingMinutesFromNow+"' minutes from now.");
		return poolId; // return poolId to the newly available SubscriptionPool
		
	}
	@BeforeClass(groups="setup")
	public void findRandomAvailableProductIdBeforeClass() throws Exception {
		clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, null, true, null, null, null, null);
		// find a randomly available product id
		List<SubscriptionPool> pools = clienttasks.getCurrentlyAvailableSubscriptionPools();
		SubscriptionPool pool = pools.get(randomGenerator.nextInt(pools.size())); // randomly pick a pool
		
		// debugging bugzilla 760162
		//pool = SubscriptionPool.findFirstInstanceWithMatchingFieldFromList("productId", "awesomeos-virt-4", pools);
		
		randomAvailableProductId = pool.productId;
	}

	@AfterClass(groups = "setup")
	protected void moveFakeProductCertFilesFromFakeTmp() {
		client.runCommandAndWait("mv " + "/etc/pki/faketmp/*.pem" + " "
				+ clienttasks.productCertDir);
		client.runCommandAndWait("rm -rf " + "/etc/pki/faketmp");
	}

}
