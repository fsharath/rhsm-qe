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
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
	protected String ownerKey;
	protected String randomAvailableProductId;
	protected EntitlementCert expiringCert = null;
	protected final String importCertificatesDir = "/tmp/sm-importExpiredCertificatesDir"
			.toLowerCase();

	// Bugzilla Healing Test methods
	// ***********************************************************************

	// Healing Candidates for an automated Test:
	// TODO Cases in Bug 710172 - [RFE] Provide automated healing of expiring
	// subscriptions//working on
	// TODO subcase Bug 746088 - autoheal is not super-subscribing on the day
	// the current entitlement cert expires //done
	// TODO subcase Bug 746218 - auto-heal isn't working for partial
	// subscription //done
	// TODO Cases in Bug 726411 - [RFE] Support for certificate healing
	// TODO https://bugzilla.redhat.com/show_bug.cgi?id=627665 //done
	// TODO Bug 669395 - gui defaults to consumer name of the hostname and
	// doesn't let you set it to empty string. cli defaults to username, and
	// does let you set it to empty string
	// TODO https://bugzilla.redhat.com/show_bug.cgi?id=669513//done
	// TODO Bug 803386 - display product ID in product details pane on sm-gui
	// and cli
	// https://bugzilla.redhat.com/show_bug.cgi?id=733327
	// TODO Bug 674652 - Subscription Manager Leaves Broken Yum Repos After
	// Unregister//done
	// TODO Bug 744504 - [ALL LANG] [RHSM CLI] facts module - Run facts update
	// with incorrect proxy url produces traceback.//done
	// TODO Bug 806958 - One empty certificate file in /etc/rhsm/ca causes
	// registration failure//done
	// https://bugzilla.redhat.com/show_bug.cgi?id=700821
	// TODO Bug 827034 - Teach rhsmcertd to refresh the identity
	// certificate//done
	// https://bugzilla.redhat.com/show_bug.cgi?id=607162//done
	// https://tcms.engineering.redhat.com/case/50235/?from_plan=2105
	// https://tcms.engineering.redhat.com/case/50215/?from_plan=2851
	// https://tcms.engineering.redhat.com/case/50238/?from_plan=2851
	// https://tcms.engineering.redhat.com/case/56897/?from_plan=2681
	// https://tcms.engineering.redhat.com/case/61710/?from_plan=2476
	// https://tcms.engineering.redhat.com/case/50230/?from_plan=2477
	// https://tcms.engineering.redhat.com/case/50215/?from_plan=2851
	// https://tcms.engineering.redhat.com/case/50238/?from_plan=2851
	// https://tcms.engineering.redhat.com/case/214136/?from_plan=5846
	// https://tcms.engineering.redhat.com/case/50215/?from_plan=2851
	// https://tcms.engineering.redhat.com/case/50223/?from_plan=2851
	// https://tcms.engineering.redhat.com/case/64181/?from_plan=2105
	// https://tcms.engineering.redhat.com/case/68737/?from_plan=2477
	// https://bugzilla.redhat.com/show_bug.cgi?id=869729
	// TODO Bug 746241 - UEPConnection.updateConsumer will not allow passing [] for facts, installed_products, or guest_uuids
	
	
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, true, null, null, null, null);
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
			groups = {"UpdateWithNoInstalledProducts","blockedByBug-746241" }, enabled = true)
	public void UpdateWithNoInstalledProducts() throws JSONException,Exception {
		Boolean actual = false;
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.restart_rhsmcertd(null, null, false, null);
		moveProductCertFiles("*.pem", true);
		String InstalledProducts=clienttasks.listInstalledProducts().getStdout();
		Assert.assertEquals(InstalledProducts.trim(), "No installed products to list");
		
		int countBefore = Integer
				.parseInt(client
						.runCommandAndWait(
								"wc -l /var/log/rhsm/rhsm.log | cut -d ' ' -f1")
								.getStdout().trim());
		clienttasks.restart_rhsmcertd(null, null, false, null);
		SubscriptionManagerCLITestScript.sleep(2 * 60 * 1000);

	if (countBefore != 0) {
		int countAfter = Integer
				.parseInt(client
						.runCommandAndWait(
								"wc -l /var/log/rhsm/rhsm.log | cut -d ' ' -f1")
								.getStdout().trim());
		moveProductCertFiles(null, false);
		Boolean flag = waitForRegexInRhsmLog("Error",
				countAfter - countBefore);
		Assert.assertEquals(flag, actual);
		actual=true;
		flag = waitForRegexInRhsmLog("Installed product IDs: \\[\\]",
				countAfter - countBefore);
		Assert.assertEquals(flag, actual);
				
	}
		
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
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
				(String) null, null, null, true, null, null, null, null);
		String consumerId=clienttasks.getCurrentConsumerId();
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,sm_serverAdminPassword, sm_serverUrl,"/owners/" + orgname);
		clienttasks.clean_(null, null, null);
		SSHCommandResult result=clienttasks.register_(sm_serverAdminUsername, sm_serverAdminPassword, orgname, null, null, null, consumerId, null, null, null,(String)null, null, null, null, null, null, null, null);
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
		SSHCommandResult result=clienttasks.register_(sm_serverAdminUsername, sm_serverAdminPassword,orgname, null, null, null, null, null, null, null,(String) null, null, null, true, null, null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"healFrequency".toLowerCase(), "1440" });
		clienttasks.config_(null, null, true, listOfSectionNameValues);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		Calendar now = new GregorianCalendar();
		now.add(Calendar.YEAR, 1);
		now.add(Calendar.DATE, 1);
		DateFormat yyyy_MM_dd_DateFormat = new SimpleDateFormat("yyyy-MM-dd");
		String onDateToTest = yyyy_MM_dd_DateFormat.format(now.getTime());
		List<SubscriptionPool> availOnDate = getAvailableFutureSubscriptionsOndate(onDateToTest);
		if(availOnDate.size()==0) throw new SkipException(
				"Sufficient future pools are not available");
		for (SubscriptionPool subscriptions : availOnDate) {
			clienttasks.subscribe_(null, null, subscriptions.poolId, null, null,
					null, null, null, null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
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
				jsonActivationKey.get("name").toString(), null, null, true,
				null, null, null, null);
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
	 * .register_(sm_clientUsername,sm_clientPassword,sm_clientOrg,null
	 * ,null,null,null,null,null,null,(String)null,null, null, true,null,null,
	 * null, null); List<String[]> listOfSectionNameValues = new
	 * ArrayList<String[]>(); listOfSectionNameValues.add(new
	 * String[]{"rhsmcertd","healFrequency".toLowerCase(), "1440"});
	 * clienttasks.config_(null,null,true,listOfSectionNameValues);
	 * clienttasks.unsubscribe_(true, (BigInteger)null, null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"healFrequency".toLowerCase(), "1440" });
		clienttasks.config_(null, null, true, listOfSectionNameValues);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool availList : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {
			poolId = availList.poolId;

		}
		clienttasks.subscribe_(null, null, poolId, null, null, null, null,
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"healFrequency".toLowerCase(), "1440" });
		clienttasks.config_(null, null, true, listOfSectionNameValues);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
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
		clienttasks.unregister_(null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"healFrequency".toLowerCase(), "1440" });
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"certFrequency".toLowerCase(), "1440" });
		clienttasks.config_(null, null, true, listOfSectionNameValues);
		clienttasks.deleteFactsFileWithOverridingValues("/custom.facts");
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool subscriptionpool : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {

			clienttasks.subscribe_(null, null, subscriptionpool.poolId, null,
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
	@Test(description = "verify content set associated with product", groups = { "VerifyUnsubscribingCertV3","blockedByBug-895447" }, enabled = true)
	@ImplementsNitrateTest(caseId = 50215)
	public void VerifyUnsubscribingCertV3() throws JSONException, Exception {

		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		File expectCertFile = new File(System.getProperty("automation.dir",
				null) + "/expiredcerts/certV3.pem");
		RemoteFileTasks.putFile(client.getConnection(),
				expectCertFile.toString(), "/root/", "0755");
		clienttasks.importCertificate_("/root/certV3.pem");
		String expected = "This machine has been unsubscribed from 1 subscriptions";
		String result = clienttasks.unsubscribe_(true, (BigInteger) null, null,
				null, null).getStdout();
		Assert.assertEquals(result.trim(), expected);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "verify  rhsmcertd is logging update failed (255)", groups = {
			"VerifyRHSMCertdLogging", "blockedByBug-708512" }, enabled = true)
	public void VerifyRHSMCertdLogging() throws JSONException, Exception {
		int certFrequency = 1;
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		String Frequency = clienttasks.getConfFileParameter(
				clienttasks.rhsmConfFile, "certFrequency");
		clienttasks.restart_rhsmcertd(certFrequency, null, false, null);
		clienttasks.waitForRegexInRhsmcertdLog("update failed (255)", 1);
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"certFrequency".toLowerCase(), Frequency });
		clienttasks.config_(null, null, true, listOfSectionNameValues);

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

		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.subscribe_(true, null, (String) null, null, null, null,
				null, null, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		List<File> certs = clienttasks.getCurrentEntitlementCertFiles();
		Assert.assertTrue(certs.isEmpty());
		certs = clienttasks.getCurrentProductCertFiles();
		Assert.assertFalse(certs.isEmpty());
		clienttasks.unregister_(null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		String invalidconsumerId = randomGenerator.nextInt() + consumerId;
		System.out.println(invalidconsumerId + "  " + consumerId);
		SSHCommandResult result = clienttasks.register_(sm_clientUsername,
				sm_clientPassword, sm_clientOrg, null, null, null,
				invalidconsumerId, null, null, null, (String) null, null, null,
				true, null, null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		String consumerid = clienttasks.getCurrentConsumerId();
		JSONObject jsonConsumer = new JSONObject(
				CandlepinTasks.getResourceUsingRESTfulAPI(
						sm_serverAdminUsername, sm_serverAdminPassword,
						sm_serverUrl, "/consumers/" + consumerid));
		String createdDateBeforeUpdate = jsonConsumer.getString("created");
		String UpdateDateBeforeUpdate = jsonConsumer.getString("updated");
		clienttasks.facts_(null, true, null, null, null).getStderr();
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
	@Test(description = "verify healing of installed products without taking future subscriptions into consideration", groups = { "VerifyHealingForFutureSubscription" }, enabled = true)
	public void VerifyHealingForFuturesubscription() throws JSONException,
	Exception {
		int healFrequency = 2;
		clienttasks.deleteFactsFileWithOverridingValues();
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.service_level_(null, null, null, true, null, null, null,
				null, null, null, null);
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
		clienttasks.subscribe_(true, null, (String) null, null, null, null,
				null, null, null, null, null);
		for (InstalledProduct installed : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installed.status.equals("Not Subscribed")
					&& installed.status.equals("Partially Subscribed"))
				moveProductCertFiles(installed.productId + ".pem", true);
				moveProductCertFiles(installed.productId + "._pem", true);
		}
		for (SubscriptionPool availOnDate : getAvailableFutureSubscriptionsOndate(onDateToTest)) {
			System.out.println(availOnDate.poolId + " avail on date is");
			clienttasks.subscribe_(null, null, availOnDate.poolId, null, null,
					null, null, null, null, null, null);
		}
		for (InstalledProduct installedproduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installedproduct.status.equals("Future Subscription")) {
				productId.add(installedproduct.productId);
			}
		}
		clienttasks.restart_rhsmcertd(null, healFrequency, false, null);
		SubscriptionManagerCLITestScript.sleep(healFrequency * 60 * 1000);

		for (InstalledProduct installedproduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			for (String productid : productId) {
				if (installedproduct.productId.equals(productid)) {
					Assert.assertEquals(installedproduct.status.trim(),
							"Subscribed");

				}
			}
		}
		
		moveProductCertFiles(null, false);


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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool pool : clienttasks.getCurrentlyAvailableSubscriptionPools()) {
			clienttasks.subscribe_(null, null, pool.poolId, null, null, null, null, null, null, null, null);
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
			"UnsubscribeFromMultipleEntitlementsTest", "blockedByBug-867766" }, enabled = true)
	@ImplementsNitrateTest(caseId = 50230)
	public void UnsubscribeFromMultipleEntitlements() throws JSONException,
	Exception {
		List<BigInteger> serialnums = new ArrayList<BigInteger>();
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool pool : clienttasks.getCurrentlyAvailableSubscriptionPools()) {
			clienttasks.subscribe_(null, null, pool.poolId, null, null, null, null, null, null, null, null);
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

		clienttasks.register_(sm_client2Username, sm_client1Password,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		String consumerid = clienttasks.getCurrentConsumerId();
		String result = clienttasks.register_(sm_clientUsername,
				sm_clientPassword, sm_clientOrg, null, null, null, consumerid,
				null, null, null, (String) null, null, null, true, null, null,
				null, null).getStderr();
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

		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		String listBeforeUpdate = clienttasks.facts_(true, null, null, null,
				null).getStdout();
		Map<String, String> factsMap = new HashMap<String, String>();
		Integer sockets = 4;
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(sockets));
		factsMap.put("uname.machine", "i386");
		clienttasks.createFactsFileWithOverridingValues("/custom.facts",
				factsMap);
		clienttasks.facts_(null, true, null, null, null);
		String listAfterUpdate = clienttasks.facts_(true, null, null, null,
				null).getStdout();
		Assert.assertNoMatch(listAfterUpdate, listBeforeUpdate);
		clienttasks.deleteFactsFileWithOverridingValues("/custom.facts");
		clienttasks.facts_(null, true, null, null, null);

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

		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		String listBeforeUpdate = clienttasks.facts_(true, null, null, null,
				null).getStdout();

		client.runCommandAndWait("echo '{fuzzing :testing}' >>/var/lib/rhsm/facts/facts.json");
		clienttasks.facts_(null, true, null, null, null);
		String listAfterUpdate = clienttasks.facts_(true, null, null, null,
				null).getStdout();
		Assert.assertFalse(listAfterUpdate.contentEquals("fuzzing"));
		Assert.assertEquals(listAfterUpdate, listBeforeUpdate);
		client.runCommandAndWait("cp /var/lib/rhsm/facts/facts.json /var/lib/rhsm/facts/facts.json.save");
		client.runCommandAndWait("sed /'uname.machine: x86_64'/d /var/lib/rhsm/facts/facts.json");
		clienttasks.facts_(null, true, null, null, null);
		listAfterUpdate = clienttasks.facts_(true, null, null, null, null)
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
		clienttasks.unregister_(null, null, null);
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger)null, null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		Calendar StartTimeBeforeRHSM = clienttasks.getCurrentConsumerCert().validityNotBefore;
		Calendar EndTimeBeforeRHSM = clienttasks.getCurrentConsumerCert().validityNotAfter;
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "server", "insecure", "1" });
		clienttasks.config_(null, null, true, listOfSectionNameValues);
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
		clienttasks.config_(null, null, true, listOfSectionNameValues);
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
			"VerifyUnsubscribeAllForExpiredSubscription", "blockedByBug-852630" }, enabled = true)
	public void VerifyUnsubscribeAllForExpiredSubscription()
			throws JSONException, Exception {
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"healFrequency".toLowerCase(), "1440" });
		clienttasks.config_(null, null, true, listOfSectionNameValues);
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		File expectCertFile = new File(System.getProperty("automation.dir",
				null) + "/expiredcerts/Expiredcert.pem");
		RemoteFileTasks.putFile(client.getConnection(),
				expectCertFile.toString(), "/root/", "0755");
		clienttasks.importCertificate_("/root/Expiredcert.pem");
		String consumed = clienttasks.list_(null, null, true, null, null, null,
				null, null, null).getStdout();
		Assert.assertTrue(!(consumed == null));
		SSHCommandResult result = clienttasks.unsubscribe_(true,
				(BigInteger) null, null, null, null);
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
		clienttasks.unregister_(null, null, null);
		String FilePath = myEmptyCaCertFile;
		String command = "touch " + FilePath;
		client.runCommandAndWait(command);
		String result = clienttasks.register_(sm_clientUsername,
				sm_clientPassword, sm_clientOrg, null, null, null, null, true,
				null, null, (String) null, null, null, null, null, null, null,
				null).getStdout();
		String Expected = "Bad CA certificate: " + FilePath;
		Assert.assertEquals(result.trim(), Expected);
		command = "rm -rf " + FilePath;
		client.runCommandAndWait(command);
		result = clienttasks.register(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, null, null, null, null, null)
				.getStdout();
		Assert.assertContainsMatch(result.trim(),
				"The system has been registered with id: [a-f,0-9,\\-]{36}");

	}
	// ensure the empty ca cert is removed after VerifyEmptyCertCauseRegistrationFailure_Test to avoid subsequent test failures due to "Bad CA certificate: /etc/rhsm/ca/myemptycert.pem" 
	@AfterGroups(groups = {"setup"}, value = {"VerifyEmptyCertCauseRegistrationFailure_Test"})
	public void removeMyEmptyCaCertFile() {
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, true, null, null, null, null);
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
	public void VerifyRepoAfterUnregister_Test() throws JSONException,
	Exception {
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger)null, null, null, null);
		clienttasks.subscribeToTheCurrentlyAllAvailableSubscriptionPoolsCollectively();
		List<Repo> repos = clienttasks.getCurrentlySubscribedRepos();
		Assert.assertFalse(repos.isEmpty());
		clienttasks.unregister_(null, null, null);
		List<Repo> repo = clienttasks.getCurrentlySubscribedRepos();
		Assert.assertTrue(repo.isEmpty());
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify if stacking entitlements reports as distinct entries in cli list --installed", groups = {
			"VerifyDistinct", "blockedByBug-733327" }, enabled = false)
	public void VerifyDistinctStackingEntires() throws Exception {
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"healFrequency".toLowerCase(), "1440" });
		clienttasks.config_(null, null, true, listOfSectionNameValues);
		List<String> poolId = new ArrayList<String>();
		String productId = null;
		Map<String, String> factsMap = new HashMap<String, String>();
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		for (SubscriptionPool pool : clienttasks
				.getCurrentlyAvailableSubscriptionPools()) {
			if (pool.multiEntitlement) {
				String poolProductSocketsAttribute = CandlepinTasks
						.getPoolProductAttributeValue(sm_clientUsername,
								sm_clientPassword, sm_serverUrl, pool.poolId,
								"sockets");

				if ((!(poolProductSocketsAttribute == null))
						&& poolProductSocketsAttribute.equals("1")) {
					clienttasks.subscribe_(null, null, pool.poolId, null, null,
							null, null, null, null, null, null).getStdout();

					poolId.add(pool.poolId);

				}
			}
			if (!(pool.quantity.equalsIgnoreCase("Unlimited"))) {
				int quantity = Integer.parseInt(pool.quantity);
				if (quantity < 4)
					throw new SkipException("Sufficient pools are not available");
			
		}
		for (InstalledProduct installed : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installed.status.equals("Not Subscribed"))
				moveProductCertFiles(installed.productId + ".pem", true);
		}
		int sockets = 4;
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(sockets));
		clienttasks.createFactsFileWithOverridingValues("/custom.facts",
				factsMap);
		clienttasks.facts_(null, true, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		clienttasks.subscribe_(null, null, poolId, null, null, null, null,
				null, null, null, null).getStdout();
		for (InstalledProduct installed : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installed.status.equals("Partially Subscribed")) {
				productId = installed.productId;
				clienttasks.subscribe_(null, null, poolId, null, null, null,
						null, null, null, null, null).getStdout();
			}
		}
		for (InstalledProduct installedProduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (productId.equals(installedProduct.productId)) {
				if (!(installedProduct.status.equals("Subscribed")))
					moveProductCertFiles("", false);
				List<ProductSubscription> consumed = clienttasks
						.getCurrentlyConsumedProductSubscriptions();
				Assert.assertEquals(consumed.size(), sockets);
				Assert.assertEquals(installedProduct.status, "Subscribed");
			}
		}
		sockets = 1;
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(sockets));
		clienttasks.createFactsFileWithOverridingValues("/custom.facts",
				factsMap);
		clienttasks.facts_(null, true, null, null, null);

	}}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "Verify deletion of subscribed product", groups = {
			"DeleteProductTest", "blockedByBug-684941" }, enabled = true)
	public void VerifyDeletionOfSubscribedProduct_Test() throws JSONException,
	Exception {
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(List<String>) null, null, null, true, null, null, null, null);
		clienttasks.subscribe_(true, null, null, (String) null, null, null,
				null, null, null, null, null);
		for (InstalledProduct installed : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installed.status.equals("Subscribed")) {
				for (SubscriptionPool AvailSub : clienttasks
						.getCurrentlyAvailableSubscriptionPools()) {
					if (installed.productName
							.contains(AvailSub.subscriptionName)) {
						String jsonConsumer = CandlepinTasks
								.deleteResourceUsingRESTfulAPI(
										sm_serverAdminUsername,
										sm_serverAdminPassword, sm_serverUrl,
										"/products/" + AvailSub.productId);
						String expect = "{\"displayMessage\""
								+ ":"
								+ "\"Product with UUID '"
								+ AvailSub.productId
								+ "'"
								+ " cannot be deleted while subscriptions exist.\"}";
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(List<String>) null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		CandlepinTasks.deleteResourceUsingRESTfulAPI(sm_serverAdminUsername,
				sm_serverAdminPassword, sm_serverUrl, "/consumers/"
						+ consumerId);
		String result = clienttasks.register_(sm_clientUsername,
				sm_clientPassword, sm_clientOrg, null, null, null, null, null,
				null, null, (List<String>) null, null, null, true, null, null,
				null, null).getStdout();

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
		clienttasks.config_(null, null, true, listOfSectionNameValues);
		value = clienttasks.getConfFileParameter(clienttasks.rhsmConfFile,
				section, name);
		Assert.assertEquals(value, newValue);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "subscription-manager: register_ --name , setting consumer name to blank", groups = {
			"register_withname", "blockedByBug-627665" }, enabled = true)
	public void register_WithNameBlankTest() throws JSONException, Exception {
		String name = "test";
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, name, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		ConsumerCert consumerCert = clienttasks.getCurrentConsumerCert();
		Assert.assertEquals(consumerCert.name, name);
		name = "";
		SSHCommandResult result = clienttasks.register_(sm_clientUsername,
				sm_clientPassword, sm_clientOrg, null, null, name, null, null,
				null, null, (String) null, null, null, true, null, null, null,
				null);
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
	@Test(description = "subscription-manager: register_ --consumerid  using a different user and valid consumerId", groups = {
			"reregister", "blockedByBug-627665" }, enabled = true)
	public void register_WithConsumerid_Test() throws JSONException, Exception {
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		clienttasks.unsubscribe_(true, (BigInteger)null, null, null, null);
		List<SubscriptionPool> pools = clienttasks
				.getCurrentlyAvailableSubscriptionPools();
		if (pools.isEmpty())
			throw new SkipException(
					"Cannot randomly pick a pool for subscribing when there are no available pools for testing.");
		SubscriptionPool pool = pools
				.get(randomGenerator.nextInt(pools.size()));
		clienttasks.subscribeToSubscriptionPoolUsingPoolId(pool);
		List<ProductSubscription> consumedSubscriptionsBeforeregister_ = clienttasks
				.getCurrentlyConsumedProductSubscriptions();
		clienttasks.clean_(null, null, null);
		clienttasks.register_(sm_client2Username, sm_clientPassword,
				sm_clientOrg, null, null, null, consumerId, null, null, null,
				(String) null, null, null, null, null, null, null, null);
		String consumerIdAfter = clienttasks.getCurrentConsumerId();
		Assert.assertEquals(consumerId, consumerIdAfter,
				"The consumer identity  has not changed after register_ing with consumerid.");
		List<ProductSubscription> consumedscriptionsAfterregister_ = clienttasks
				.getCurrentlyConsumedProductSubscriptions();
		Assert.assertTrue(
				consumedscriptionsAfterregister_
				.containsAll(consumedSubscriptionsBeforeregister_)
				&& consumedSubscriptionsBeforeregister_.size() == consumedscriptionsAfterregister_
				.size(),
				"The list of consumed products after reregister_ing is identical.");
	}

	/**
	 * @author skallesh
	 */
	@Test(description = "subscription-manager: service-level --org (without --list option)", groups = {
			"ServicelevelTest", "blockedByBug-826856" }, enabled = true)
	public void ServiceLevelWithOrgWithoutList_Test() {

		SSHCommandResult result;
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(List<String>) null, null, null, true, null, null, null, null);
		result = clienttasks.service_level_(null, false, null, null,
				sm_clientUsername, sm_clientPassword, "MyOrg", null, null,
				null, null);
		Assert.assertEquals(result.getStdout().trim(),
				"Error: --org is only supported with the --list option");
	}

	/**
	 * @author skallesh
	 */
	@Test(description = "subscription-manager: facts --update (when register_ed)", groups = {
			"MyTestFacts", "blockedByBug-707525" }, enabled = true)
	public void FactsUpdateWhenregister_ed_Test() {

		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(List<String>) null, null, null, true, null, null, null, null);
		SSHCommandResult result = clienttasks.facts(null, true, null, null,
				null);
		Assert.assertEquals(result.getStdout().trim(),
				"Successfully updated the system facts.");
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 * @throws JSONException
	 */
	@Test(description = "subscription-manager: facts --list,verify system.entitlements_valid ", groups = {
			"validTest", "blockedByBug-669513" }, enabled = true)
	public void VerifyEntilementValidityInFactsList_Test()
			throws JSONException, Exception {
		client.runCommandAndWait("mkdir -p " + "/etc/pki/faketmp");
		client.runCommandAndWait("mv " + clienttasks.productCertDir + "/*_.pem"
				+ " " + "/etc/pki/faketmp/");
		moveProductCertFiles(null, false);
		clienttasks.deleteFactsFileWithOverridingValues();
		List<String> productId = new ArrayList<String>();
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"healFrequency".toLowerCase(), "1440" });
		clienttasks.config_(null, null, true, listOfSectionNameValues);
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(List<String>) null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		clienttasks.facts_(true, null, null, null, null);
		String result = clienttasks.getFactValue("system.entitlements_valid");
		Assert.assertEquals(result.trim(), "invalid");
		clienttasks.subscribe_(true, null, null, (String) null, null, null,
				null, null, null, null, null);
		for (InstalledProduct installed : clienttasks
				.getCurrentlyInstalledProducts()) {
			if ((installed.status.equals("Not Subscribed"))
					|| (installed.status.equals("Partially Subscribed"))) {
				productId.add(installed.productId);

			}
		}
		System.out.println(productId.size());
		if (!(productId.size() == 0)) {
			for (int i = 0; i < productId.size(); i++) {
				moveProductCertFiles(productId.get(i) + ".pem", true);
			}
			result = clienttasks.getFactValue("system.entitlements_valid");
			Assert.assertEquals(result.trim(), "valid");
			moveProductCertFiles(null, false);

		} else {
			result = clienttasks.getFactValue("system.entitlements_valid");
			Assert.assertEquals(result.trim(), "valid");
		}
		client.runCommandAndWait("mv " + "/etc/pki/faketmp/*.pem" + " "
				+ clienttasks.productCertDir);
		client.runCommandAndWait("rm -rf " + "/etc/pki/faketmp");

	}

	/**
	 * @author skallesh
	 */
	@Test(description = "subscription-manager: attempt register_ to with white space in the user name should fail", groups = {
			"register_edTests", "blockedByBug-719378" }, enabled = true)
	public void Attemptregister_WithWhiteSpacesInUsername_Test() {
		SSHCommandResult result = clienttasks.register_("user name",
				"password", sm_clientOrg, null, null, null, null, null, null,
				null, (String) null, null, null, true, null, null, null, null);
		Assert.assertEquals(
				result.getStderr().trim(),
				servertasks.invalidCredentialsMsg(),
				"The expected stdout result when attempting to register_ with a username containing whitespace.");
	}

	/**
	 * @author skallesh
	 * @throws JSONException
	 * @throws Exception
	 */
	@Test(description = "Auto-heal for partial subscription", groups = {
			"autohealPartial", "blockedByBug-746218" }, enabled = true)
	public void VerifyAutohealForPartialSubscription() throws Exception {
		Integer healFrequency = 3;
		Integer moreSockets = 0;
		List<String> productId = new ArrayList<String>();
		List<String> poolId = new ArrayList<String>();
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, true, null, null,
				(String) null, null, null, true, null, null, null, null);
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
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);

		clienttasks.subscribe_(null, null, poolId, null, null, null, null,
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
	@Test(description = "Auto-heal with SLA", groups = { "AutoHealWithSLA" }, enabled = true)
	public void VerifyAutohealWithSLA() throws JSONException, Exception {
		Integer healFrequency = 2;
		String filename = null;
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		List<String> availableServiceLevelData = clienttasks
				.getCurrentlyAvailableServiceLevels();
		String availableService = availableServiceLevelData.get(randomGenerator
				.nextInt(availableServiceLevelData.size()));
		clienttasks.subscribe_(true, availableService, (String) null, null,
				null, null, null, null, null, null, null);
		List<EntitlementCert> certs = clienttasks.getCurrentEntitlementCerts();
		if (certs.isEmpty()) {
			availableService = availableServiceLevelData.get(randomGenerator
					.nextInt(availableServiceLevelData.size()));
			clienttasks.subscribe_(true, availableService, (String) null, null,
					null, null, null, null, null, null, null);
		}
		clienttasks.service_level_(null, null, null, null, null,
				availableService, null, null, null, null, null);
		clienttasks.restart_rhsmcertd(null, healFrequency, false, null);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		SubscriptionManagerCLITestScript.sleep(healFrequency * 60 * 1000);
		certs = clienttasks.getCurrentEntitlementCerts();
		moveProductCertFiles(filename, false);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
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
				"healFrequency".toLowerCase(), "1440" });
		clienttasks.config_(null, null, true, listOfSectionNameValues);
		String Flag = "false";
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		List<ProductSubscription> consumed = clienttasks
				.getCurrentlyConsumedProductSubscriptions();
		if (!(consumed.isEmpty())) {
			clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		}
		Map<String, String> factsMap = new HashMap<String, String>();
		Integer moreSockets = 4;
		factsMap.put("cpu.cpu_socket(s)", String.valueOf(moreSockets));
		clienttasks.createFactsFileWithOverridingValues("/socket.facts",factsMap);
		clienttasks.facts_(null, true, null, null, null);
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
			"AutohealForExpired", "blockedByBug-746088" }, enabled = true)
	public void VerifyAutohealForExpiredSubscription() throws JSONException,
	Exception {
		int healFrequency = 2;
		List<String> Expiredproductid = new ArrayList<String>();
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		String consumerId = clienttasks.getCurrentConsumerId();
		JSONObject jsonConsumer = CandlepinTasks.setAutohealForConsumer(
				sm_clientUsername, sm_clientPassword, sm_serverUrl, consumerId,
				true);
		Assert.assertTrue(jsonConsumer.getBoolean("autoheal"),
				"A consumer's autoheal attribute value=true.");

		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		clienttasks.service_level_(null, null, null, true, null, null, null,
				null, null, null, null);
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
	@Test(description = "Auto-heal for subscription", groups = { "AutoHeal" }, enabled = true)
	@ImplementsNitrateTest(caseId = 119327)
	public void VerifyAutohealForSubscription() throws JSONException, Exception {
		Integer healFrequency = 2;
		clienttasks.register_(sm_clientUsername, sm_clientPassword,sm_clientOrg, null, null, null, null, null, null, null,(String) null, null, null, true, null, null, null, null);
		clienttasks.unsubscribeFromAllOfTheCurrentlyConsumedProductSubscriptions();
		String consumerId = clienttasks.getCurrentConsumerId();
		JSONObject jsonConsumer = CandlepinTasks.setAutohealForConsumer(sm_clientUsername, sm_clientPassword, sm_serverUrl, consumerId,true);
		Assert.assertTrue(jsonConsumer.getBoolean("autoheal"),"A consumer's autoheal attribute value=true.");
		clienttasks.service_level_(null, null, null, true, null, null, null,null, null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		List<String> availableServiceLevelData = clienttasks
				.getCurrentlyAvailableServiceLevels();
		String availableService = availableServiceLevelData.get(randomGenerator
				.nextInt(availableServiceLevelData.size()));
		clienttasks.service_level_(null, null, null, null, null, null, null,
				null, null, null, null);
		clienttasks.subscribe_(true, availableService, (String) null, null,
				null, null, null, null, null, null, null);
		for (InstalledProduct installedProduct : clienttasks
				.getCurrentlyInstalledProducts()) {

			if (installedProduct.status.trim().equalsIgnoreCase("Subscribed")
					|| installedProduct.status.trim().equalsIgnoreCase(
							"Partially Subscribed")) {
				filename = installedProduct.productId + ".pem";
				moveProductCertFiles(filename, true);
			}
		}
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		List<EntitlementCert> certsbeforeRHSMService = clienttasks
				.getCurrentEntitlementCerts();
		log.info("cert contents are " + certsbeforeRHSMService);
		clienttasks.subscribe_(true, null, null, (List<String>) null, null,
				null, null, null, null, null, null);
		clienttasks.restart_rhsmcertd(null, healFrequency, false, null);
		SubscriptionManagerCLITestScript.sleep(healFrequency * 60 * 1000);
		List<EntitlementCert> certs = clienttasks.getCurrentEntitlementCerts();
		if (!(certs.isEmpty()))
			moveProductCertFiles(filename, false);
		client.runCommandAndWait("mv " + "/etc/pki/faketmp/*.pem" + " "
				+ clienttasks.productCertDir);
		client.runCommandAndWait("rm -rf " + "/etc/pki/faketmp");
		Assert.assertTrue((certs.isEmpty()), "autoheal has failed");
		moveProductCertFiles(filename, false);
	}

	// Bugzilla subscribe_ Test methods
	// ***********************************************************************

	// subscribe_ Candidates for an automated Test:
	// TODO Bug 668032 - rhsm not logging subscriptions and products properly
	// //done --shwetha
	// TODO Bug 670831 - Entitlement Start Dates should be the Subscription
	// Start Date //Done --shwetha
	// TODO Bug 664847 - Autobind logic should respect the architecture
	// attribute //working on
	// TODO Bug 676377 - rhsm-compliance-icon's status can be a day out of sync
	// - could use dbus-monitor to assert that the dbus message is sent on the
	// expected compliance changing events
	// TODO Bug 739790 - Product "RHEL Workstation" has a valid stacking_id but
	// its socket_limit is 0
	// TODO Bug 707641 - CLI auto-subscribe_ tries to re-use basic auth
	// credentials.

	// TODO Write an autosubscribe bug... 1. subscribe_ to all avail and note
	// the list of installed products (Subscribed, Partially, Not)
	// 2. Unsubscribe all 3. Autosubscribe and verfy same installed product
	// status (Subscribed, Not)//done --shwetha
	// TODO Bug 746035 - autosubscribe should NOT consider existing future
	// entitlements when determining what pools and quantity should be
	// autosubscribed //working on
	// TODO Bug 747399 - if consumer does not have architecture then we should
	// not check for it
	// TODO Bug 743704 - autosubscribe ignores socket count on non multi-entitle
	// subscriptions //done --shwetha
	// TODO Bug 740788 - Getting error with quantity subscribe_ using
	// subscription-assistance page
	// Write an autosubscribe test that mimics partial subscriptions in
	// https://bugzilla.redhat.com/show_bug.cgi?id=740788#c12
	// TODO Bug 720360 - subscription-manager: entitlement key files created
	// with weak permissions // done --shwetha
	// TODO Bug 772218 - Subscription manager silently rejects pools requested
	// in an incorrect format.//done --shwetha

	/**
	 * @author skallesh
	 */

	@Test(description = "subscription-manager: subscribe_ multiple pools in incorrect format", groups = {
			"MysubscribeTest", "blockedByBug-772218" }, enabled = true)
	public void VerifyIncorrectSubscriptionFormat() {
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
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

			SSHCommandResult subscribeResult = subscribeInvalidFormat_(null,
					null, poolid.get(i), poolid.get(j), null, null, null, null,
					null, null, null, null);
			Assert.assertEquals(subscribeResult.getStdout().trim(),
					"cannot parse argument: " + poolid.get(j));
		}

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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger)null, null, null, null);
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
	@Test(description = "Verify if architecture for auto-subscribe_ test", groups = { "VerifyarchitectureForAutobind_Test" }, enabled = true)
	public void VerifyarchitectureForAutobind_Test() throws Exception {

		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
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
		clienttasks.facts_(null, true, null, null, null);
	}

	/**
	 * @author skallesh
	 * @throws Exception
	 */
	@Test(description = "Verify if rhsm not logging subscriptions and products properly ", groups = { "VerifyRhsmLogging_Test" }, enabled = true)
	public void VerifyRhsmLoggingTest() throws Exception {
		Boolean actual = true;
		int countBefore = 0;
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);

		for (SubscriptionPool pool : clienttasks
				.getCurrentlyAllAvailableSubscriptionPools()) {
			List<String> providedProducts = CandlepinTasks
					.getPoolProvidedProductIds(sm_clientUsername,
							sm_clientPassword, sm_serverUrl, pool.poolId);
			if ((providedProducts.size()) > 2) {
				countBefore = Integer
						.parseInt(client
								.runCommandAndWait(
										"wc -l /var/log/rhsm/rhsm.log | cut -d ' ' -f1")
										.getStdout().trim());
				clienttasks.subscribe_(null, null, pool.poolId, null, null,
						null, null, null, null, null, null);
			}
			if (countBefore != 0) {
				int countAfter = Integer
						.parseInt(client
								.runCommandAndWait(
										"wc -l /var/log/rhsm/rhsm.log | cut -d ' ' -f1")
										.getStdout().trim());

				Boolean flag = waitForRegexInRhsmLog("@ /etc/pki/entitlement",
						countAfter - countBefore);
				Assert.assertEquals(flag, actual);
			}

		}

	}

	/**
	 * @author skallesh
	 * @throws Exception
	 */
	@Test(description = "Verify if the status of installed products match when autosubscribed,and when you subscribe_ all the available products ", groups = {
			"VerifyFuturesubscription_Test", "blockedByBug-746035" }, enabled = true)
	public void VerifyFuturesubscription_Test() throws Exception {
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		Calendar now = new GregorianCalendar();
		List<String> productId = new ArrayList<String>();
		now.add(Calendar.YEAR, 1);
		DateFormat yyyy_MM_dd_DateFormat = new SimpleDateFormat("yyyy-MM-dd");
		String onDateToTest = yyyy_MM_dd_DateFormat.format(now.getTime());
		clienttasks.subscribe_(true, null, (String) null, null, null, null,
				null, null, null, null, null);
		for (InstalledProduct installed : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installed.status.equals("Not Subscribed")
					&& installed.status.equals("Partially Subscribed"))
				moveProductCertFiles(installed.productId + ".pem", true);
			moveProductCertFiles(installed.productId + "_.pem", true);
		}
		for (SubscriptionPool availOnDate : getAvailableFutureSubscriptionsOndate(onDateToTest)) {
			clienttasks.subscribe_(null, null, availOnDate.poolId, null, null,
					null, null, null, null, null, null);
		}
		for (InstalledProduct installedproduct : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installedproduct.status.equals("Future Subscription")) {

				productId.add(installedproduct.productId);
			}
		}
		clienttasks.subscribe_(true, null, (String) null, null, null, null,
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

		moveProductCertFiles(null, false);
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
	@Test(description = "Verify if the status of installed products match when autosubscribed,and when you subscribe_ all the available products ", groups = { "Verifyautosubscribe_Test" }, enabled = true)
	public void Verifyautosubscribe_Test() throws JSONException, Exception {

		List<String> ProductIdBeforeAuto = new ArrayList<String>();
		List<String> ProductIdAfterAuto = new ArrayList<String>();
		clienttasks.deleteFactsFileWithOverridingValues();
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		clienttasks.subscribeToTheCurrentlyAvailableSubscriptionPoolsIndividually();
		for (InstalledProduct installedProductsBeforeAuto : clienttasks
				.getCurrentlyInstalledProducts()) {
			if (installedProductsBeforeAuto.status.equals("Subscribed"))
				ProductIdBeforeAuto.add(installedProductsBeforeAuto.productId);
		}

		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
		clienttasks.subscribe_(true, null, (String) null, null, null, null,
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
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
			clienttasks.facts_(null, true, null, null, null);

		}
		clienttasks.subscribe_(true, null, (String) null, null, null, null,
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
						clienttasks.facts_(null, true, null, null, null);
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
		clienttasks.register_(sm_clientUsername, sm_clientPassword,
				sm_clientOrg, null, null, null, null, null, null, null,
				(String) null, null, null, true, null, null, null, null);
		clienttasks.subscribeToTheCurrentlyAllAvailableSubscriptionPoolsCollectively();
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
			"AutoHealFailForSLA", "Verifyautosubscribe_Test", "validTest",
			"BugzillaTests", "autohealPartial",
			"VerifyEntitlementStartDate_Test", "reregister" }, enabled = true)
	public void unsubscribeBeforeGroup() {
		clienttasks.unsubscribe_(true, (BigInteger) null, null, null, null);
	}

	@BeforeGroups(groups = "setup", value = { "VerifyDistinct", "AutoHeal",
			"autohealPartial", "BugzillaTests" }, enabled = true)
	public void unsetServicelevelBeforeGroup() {
		clienttasks.service_level_(null, null, null, true, null, null, null,
				null, null, null, null);
	}

	@BeforeGroups(groups = "setup", value = { "VerifyDistinct", "AutoHeal",
			"VerifyStatusForPartialSubscription", "autohealPartial",
			"VerifyEntitlementStartDate_Test", "BugzillaTests" }, enabled = true)
	public void setHealFrequencyGroup() {
		List<String[]> listOfSectionNameValues = new ArrayList<String[]>();
		listOfSectionNameValues.add(new String[] { "rhsmcertd",
				"healFrequency".toLowerCase(), "1440" });
		clienttasks.config_(null, null, true, listOfSectionNameValues);
		String param = clienttasks.getConfFileParameter(
				clienttasks.rhsmConfFile, "rhsmcertd", "healFrequency");

		Assert.assertEquals(param, "1440");
	}

	@BeforeGroups(groups = "setup", value = { "autohealPartial", "AutoHeal",
			"heal", "BugzillaTests", "AutoHealFailForSLA", "AutohealForExpired" }, enabled = false)
	public void VerifyAutohealAttributeDefaultsToTrueForNewSystemConsumer_Test()
			throws Exception {

		String consumerId = clienttasks
				.getCurrentConsumerId(clienttasks.register_(sm_clientUsername,
						sm_clientPassword, sm_clientOrg, null, null, null,
						null, null, null, null, (String) null, null, null,
						true, null, null, null, null));
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
						"healFrequency"));
	}

	@AfterClass(groups = "setup")
	public void restoreConfiguredHealFrequency() {
		if (clienttasks == null)
			return;
		clienttasks.restart_rhsmcertd(null, configuredHealFrequency, false,
				null);
	}

	@AfterClass(groups = "setup")
	public void restoreProductCerts() {
		if (clienttasks == null)
			return;
		moveProductCertFiles(null, false);
	}

	@AfterGroups(groups = { "setup" }, value = { "Verifyautosubscribe_Test",
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

	protected void moveProductCertFiles(String filename, Boolean move) {
		client.runCommandAndWait("mkdir -p " + "/etc/pki/tmp1");

		if (move == true) {
			client.runCommandAndWait("mv " + clienttasks.productCertDir + "/"
					+ filename + " " + "/etc/pki/tmp1/");
		} else {
			client.runCommandAndWait("mv " + "/etc/pki/tmp1/*.pem" + " "
					+ clienttasks.productCertDir);
			client.runCommandAndWait("rm -rf " + "/etc/pki/tmp1");
		}
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

	public Boolean waitForRegexInRhsmLog(String logRegex, int linecount) {
		String input = client
				.runCommandAndWait(
						"tail -" + linecount + " " + clienttasks.rhsmLogFile)
						.getStdout().trim();
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
		clienttasks.register(sm_clientUsername, sm_clientPassword, sm_clientOrg, null, ConsumerType.system, null, null, null, null, null, (String)null, null, null, Boolean.TRUE, false, null, null, null);
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
	@BeforeClass(groups = "setup")
	protected void moveFakeProductCertFilesToFakeTmp() {
		client.runCommandAndWait("mkdir -p " + "/etc/pki/faketmp");
		client.runCommandAndWait("mv " + clienttasks.productCertDir + "/*_.pem"
				+ " " + "/etc/pki/faketmp/");
	}

	@AfterClass(groups = "setup")
	protected void moveFakeProductCertFilesFromFakeTmp() {
		client.runCommandAndWait("mv " + "/etc/pki/faketmp/*.pem" + " "
				+ clienttasks.productCertDir);
		client.runCommandAndWait("rm -rf " + "/etc/pki/faketmp");
	}
}
