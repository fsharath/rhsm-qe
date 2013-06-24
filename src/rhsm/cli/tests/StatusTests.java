package rhsm.cli.tests;

import java.io.File;
import java.math.BigInteger;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.xmlrpc.XmlRpcException;
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
import com.redhat.qe.auto.bugzilla.BlockedByBzBug;
import com.redhat.qe.auto.bugzilla.BzChecker;
import com.redhat.qe.auto.tcms.ImplementsNitrateTest;
import com.redhat.qe.auto.testng.TestNGUtils;
import rhsm.base.ConsumerType;
import rhsm.base.SubscriptionManagerCLITestScript;
import rhsm.cli.tasks.CandlepinTasks;
import rhsm.data.EntitlementCert;
import rhsm.data.InstalledProduct;
import rhsm.data.OrderNamespace;
import rhsm.data.ProductCert;
import rhsm.data.ProductNamespace;
import rhsm.data.ProductSubscription;
import rhsm.data.SubscriptionPool;

import com.redhat.qe.tools.RemoteFileTasks;
import com.redhat.qe.tools.SSHCommandResult;

/**
 *  @author jsefler
 *
 */
@Test(groups={"StatusTests"})
public class StatusTests extends SubscriptionManagerCLITestScript{
	
	
	// Test Methods ***********************************************************************
	
	
	@Test(	description="when subscription-manager is run with no args, it should default to the status module",
			groups={},
			enabled=false)	// this test was invalided by Bug 974123 - subscription-manager defaults to status and not to help
			//@ImplementsNitrateTest(caseId=)
	public void StatusIsTheDefault_Test() {
		String overallStatusLabel = "Overall Status:";
		clienttasks.unregister(null,null,null);
		SSHCommandResult defaultResult = RemoteFileTasks.runCommandAndAssert(client,clienttasks.command,Integer.valueOf(0));
		SSHCommandResult statusResult = clienttasks.status(null, null, null);
		Assert.assertTrue(statusResult.getStdout().contains(overallStatusLabel),"Expected status to report '"+overallStatusLabel+"'.");
		Assert.assertTrue(defaultResult.toString().equals(statusResult.toString()), "When not registered, the default output running subscription-manager with no arguments should be identical to output from the status module.");
		clienttasks.register(sm_clientUsername, sm_clientPassword, sm_clientOrg, null, null, null, null, null, null, null, (String)null, null, null, null, true, false, null, null, null);
		defaultResult = RemoteFileTasks.runCommandAndAssert(client,clienttasks.command,Integer.valueOf(0));
		statusResult = clienttasks.status(null, null, null);
		Assert.assertTrue(defaultResult.toString().split(overallStatusLabel)[0].equals(statusResult.toString().split(overallStatusLabel)[0]), "When registered, the default output running subscription-manager with no arguments should default to the status module.");
	}
	
	@Test(	description="run subscription-manager status without being registered; status should be Unknown",
			groups={},
			enabled=true)
			//@ImplementsNitrateTest(caseId=)
	public void StatusWithoutBeingRegistered_Test() {
		SSHCommandResult statusResult;
		clienttasks.unregister(null,null,null);
		statusResult = clienttasks.status(null, null, null);
		
		//	[root@jsefler-5 ~]# subscription-manager status
		//	+-------------------------------------------+
		//	   System Status Details
		//	+-------------------------------------------+
		//	Overall Status: Unknown

		String expectedStatus = "Overall Status: Unknown";
		Assert.assertTrue(statusResult.getStdout().contains(expectedStatus), "Expecting '"+expectedStatus+"' when not registered.");
		Assert.assertEquals(statusResult.getStdout().replaceFirst("\\+-+\\+\\n\\s*System Status Details\\s*\\n\\+-+\\+", "").trim(),expectedStatus, "Expecting ONLY '"+expectedStatus+"' to be reported when not registered.");
	}
	
	
	@Test(	description="run subscription-manager status when registered without entitlements",
			groups={"AcceptanceTests"},
			enabled=true)
			//@ImplementsNitrateTest(caseId=)
	public void StatusWhileRegisteredWithoutEntitlements_Test() {
		int numberOfInstalledProducts = clienttasks.getCurrentProductCertFiles().size();
		SSHCommandResult statusResult;
		clienttasks.register(sm_clientUsername, sm_clientPassword, sm_clientOrg, null, null, null, null, null, null, null, (String)null, null, null, null, true, false, null, null, null);
		List<InstalledProduct> installedProducts = clienttasks.getCurrentlyInstalledProducts();
		statusResult = clienttasks.status(null, null, null);
		
		//	[root@jsefler-5 ~]# subscription-manager status
		//	+-------------------------------------------+
		//	   System Status Details
		//	+-------------------------------------------+
		//	Overall Status: Invalid
		//
		//	Awesome OS Modifier Bits:
		//	- Not covered by a valid subscription.


		// assert the overall status
		String expectedStatus;
 		if (numberOfInstalledProducts==0) {
			expectedStatus = "Overall Status: Current";	// translation for "valid"
		} else {
			expectedStatus = "Overall Status: Invalid";	// translation for "invalid"
		}
		Assert.assertTrue(statusResult.getStdout().contains(expectedStatus), "Expecting '"+expectedStatus+"' when registered without entitlements and '"+numberOfInstalledProducts+"' installed products.");

		// assert the individual installed product status details
		for (InstalledProduct installedProduct : installedProducts) {
			for (String statusDetail : installedProduct.statusDetails) {
				Assert.assertTrue(!getSubstringMatches(statusResult.getStdout(), "^"+installedProduct.productName.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")+":(\\n- .*)*?\\n- "+statusDetail.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")).isEmpty(),
						"Expecting the status detail '"+statusDetail+"' of installed product '"+installedProduct.productName+"' to appear in the list of overall status details.");
				Assert.assertTrue(!doesStringContainMatches(statusResult.getStdout(), statusDetail.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")+"(\\n- .*)*?\\n- "+statusDetail.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")),
						"Status detail '"+statusDetail+"' of installed product '"+installedProduct.productName+"' should not appear in duplicate.");
			}
		}
		if (installedProducts.isEmpty()) {
			Assert.assertTrue(statusResult.getStdout().trim().endsWith(expectedStatus), "There should be no report of installed product details when there are no installed products; only expected '"+expectedStatus+"'.");
		}
	}
	
	
	@Test(	description="run subscription-manager status when registered with entitlements",
			groups={"AcceptanceTests"/*, "blockedByBug-958827"*/,"StatusWhileRegisteredWithEntitlements_Test"},
			enabled=true)
			//@ImplementsNitrateTest(caseId=)
	public void StatusWhileRegisteredWithEntitlements_Test() throws JSONException, Exception {
		SSHCommandResult statusResult;
		// override the system facts setting the attribute count to a value for which all the stackable subscriptions are needed to achieve compliance
		Map<String,String> factsMap = new HashMap<String,String>();
		factsMap.put("memory.memtotal", "75");
		factsMap.put("cpu.cpu_socket(s)", "100");
		factsMap.put("cpu.core(s)_per_socket", "2");
		clienttasks.createFactsFileWithOverridingValues(factsMap);
		clienttasks.register(sm_clientUsername, sm_clientPassword, sm_clientOrg, null, null, null, null, null, null, null, (String)null, null, null, null, true, false, null, null, null);
		clienttasks.subscribeToTheCurrentlyAvailableSubscriptionPoolsCollectively();
		String systemEntitlementsValid = clienttasks.getFactValue("system.entitlements_valid");
		statusResult = clienttasks.status(null, null, null);
		
		//	[root@jsefler-5 ~]# subscription-manager status
		//	+-------------------------------------------+
		//	   System Status Details
		//	+-------------------------------------------+
		//	Overall Status: Invalid
		//
		//	Large File Support Bits:
		//	- Not covered by a valid subscription.
		//
		//	Awesome OS for x86 Bits:
		//	- Not covered by a valid subscription.
		//
		//	Stackable Cores Package (8 cores)/Stackable Cores Package (8 cores):
		//	- Only covers 16 of 200 cores.
		//
		//	Awesome OS for S390X Bits:
		//	- Not covered by a valid subscription.
		//
		//	Awesome OS for S390 Bits:
		//	- Not covered by a valid subscription.
		//
		//	Load Balancing Bits:
		//	- Not covered by a valid subscription.
		//
		//	Awesome OS Workstation Bits:
		//	- Not covered by a valid subscription.
		//
		//	Clustering Bits:
		//	- Not covered by a valid subscription.
		//
		//	Awesome OS for i386 Bits:
		//	- Not covered by a valid subscription.
		//
		//	Stackable with Awesome OS for x86_64/Awesome OS for x86_64/Stackable with Awesome OS for x86_64/Awesome OS for x86_64:
		//	- Only covers 4 of 100 sockets.
		//
		//	Multi-Attribute (non-stackable) (6 cores, 8GB):
		//	- Only covers 6 of 200 cores.
		//
		//	Awesome OS for ia64 Bits:
		//	- Not covered by a valid subscription.
		//
		//	Awesome OS Developer Basic:
		//	- Not covered by a valid subscription.
		//
		//	Multi-Attribute (multi-entitlement only) (8 cores, 4GB):
		//	- Only covers 8 of 200 cores.
		//
		//	Cores Package (26 cores):
		//	- Only covers 26 of 200 cores.
		//
		//	Multiplier Product Bits:
		//	- Not covered by a valid subscription.
		//
		//	RAM/Cores Package (8GB, 4 cores):
		//	- Only covers 4 of 200 cores.
		//
		//	Shared Storage Bits:
		//	- Not covered by a valid subscription.
		//
		//	Awesome OS for ppc64 Bits:
		//	- Not covered by a valid subscription.
		//
		//	Awesome OS for i686 Bits:
		//	- Not covered by a valid subscription.
		//
		//	Awesome OS Premium Architecture Bits:
		//	- Not covered by a valid subscription.
		//
		//	Multi-Attribute Stackable (4 cores)/Multi-Attribute Stackable (2 GB, 2 Cores)/Multi-Attribute Stackable (4 cores)/Multi-Attribute Stackable (2 GB, 2 Cores)/Multi-Attribute Stackable (16
		//	cores, 4 sockets, 8GB RAM)/Multi-Attribute Stackable (2 sockets)/Multi-Attribute Stackable (2 GB)/Multi-Attribute Stackable (2 sockets)/Multi-Attribute Stackable (16 cores, 4 sockets, 8GB
		//	RAM)/Multi-Attribute Stackable (2 GB):
		//	- Only covers 44 of 200 cores.
		//	- Only covers 12 of 100 sockets.
		//
		//	Management Bits:
		//	- Not covered by a valid subscription.
		//
		//	Virt Only Awesome OS for i386 Bits:
		//	- Not covered by a valid subscription.
		//
		//	Awesome OS Developer Bits:
		//	- Not covered by a valid subscription.
		//
		//	Awesome OS for x86_64/i686/ia64/ppc/ppc64/s390x/s390 Bits:
		//	- Not covered by a valid subscription.

		// assert the overall status
		String expectedStatus = null;
 		if (systemEntitlementsValid.equals("valid")) {
			expectedStatus = "Overall Status: Current";	// translation for "valid"
		} else if (systemEntitlementsValid.equals("invalid")){
			expectedStatus = "Overall Status: Invalid";	// translation for "invalid"
		} else if (systemEntitlementsValid.equals("partial")){
			expectedStatus = "Overall Status: Insufficient";	// translation for "partial"	// Bug 959124 - "Compliant status" of the system set to "Insufficient" if any partial subscription is attached to a product (which is not exsiting )
		} else if (systemEntitlementsValid.equals("unknown")){
			expectedStatus = "Overall Status: Unknown";	// translation for "unknown"
		} else {
			Assert.fail("Encountered an unexpected value for systemEntitlementsValid '"+systemEntitlementsValid+"'.");
		}
		Assert.assertTrue(statusResult.getStdout().contains(expectedStatus), "Expecting '"+expectedStatus+"'.");

		// assert the individual installed product status details
		for (InstalledProduct installedProduct : clienttasks.getCurrentlyInstalledProducts()) {
			
			// status details from the individual installed products is only included in the status report when the product is Not Subscribed
			if (installedProduct.status.equals("Not Subscribed")) {
				if (installedProduct.statusDetails.isEmpty()) log.warning("Status Details appears empty.  Is your candlepin server older than 0.8.6?");	// expectedDetails "Not covered by a valid subscription."
				Assert.assertTrue(!getSubstringMatches(statusResult.getStdout(), "^"+installedProduct.productName.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")+":").isEmpty(),
						"Installed product '"+installedProduct.productName+"' should be included in the overall status details report when its own status is Not Subscribed.");
				for (String statusDetail : installedProduct.statusDetails) {
					Assert.assertTrue(!getSubstringMatches(statusResult.getStdout(), "^"+installedProduct.productName.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")+":(\\n- .*)*?\\n- "+statusDetail.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")).isEmpty(),
							"Expecting the status detail '"+statusDetail+"' of installed product '"+installedProduct.productName+"' to appear in the list of overall status details.");
					//Assert.assertTrue(!doesStringContainMatches(listStatusResult.getStdout(), "(\\n^- "+statusDetail+"){2,}"),
					//		"Status detail '"+statusDetail+"' of installed product '"+installedProduct.productName+"' should NOT appear in duplicate.");
					Assert.assertTrue(!doesStringContainMatches(statusResult.getStdout(), statusDetail.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")+"(\\n- .*)*?\\n- "+statusDetail.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")),
							"Status detail '"+statusDetail+"' of installed product '"+installedProduct.productName+"' should not appear in duplicate.");
				}
			} else {
				Assert.assertTrue(getSubstringMatches(statusResult.getStdout(), "^"+installedProduct.productName.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")+":").isEmpty(),
						"Installed product '"+installedProduct.productName+"' should NOT be included in the overall status details report when its own status '"+installedProduct.status+"' is something other than Not Subscribed.");
			}
		}
		
		// assert the individual consumed subscription status details
		for (ProductSubscription productSubscription : clienttasks.getCurrentlyConsumedProductSubscriptions()) {
			
			// asset the list status output
			if (productSubscription.statusDetails.isEmpty()) {
				Assert.assertTrue(getSubstringMatches(statusResult.getStdout(), "^.*"+productSubscription.productName.replaceAll("\\(","\\\\(").replaceAll("\\)","\\\\)")+".*:").isEmpty(),
						"Expecting the empty status details "+productSubscription.statusDetails+" of consumed subscription '"+productSubscription.productName+" to NOT appear in the list of overall status details.");
			} else {
				for (String statusDetail : productSubscription.statusDetails) {
					Assert.assertTrue(!getSubstringMatches(statusResult.getStdout(), "^.*"+productSubscription.productName.replaceAll("\\(","\\\\(").replaceAll("\\)","\\\\)")+".*:(\\n- .*)*?\\n- "+statusDetail.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")).isEmpty(),
							"Expecting the status detail '"+statusDetail+"' of consumed subscription '"+productSubscription.productName+"' to appear in the list of overall status details.");
					//Assert.assertTrue(!doesStringContainMatches(listStatusResult.getStdout(), "(\\n^- "+statusDetail+"){2,}"),
					//		"Status detail '"+statusDetail+"' of consumed subscription '"+productSubscription+"' should NOT appear in duplicate.");
					Assert.assertTrue(!doesStringContainMatches(statusResult.getStdout(), statusDetail.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")+"(\\n- .*)*?\\n- "+statusDetail.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)")),
							"Status detail '"+statusDetail+"' of consumed subscription '"+productSubscription.productName+"' should not appear in duplicate.");
				}
			}
		}
	}
	@AfterGroups(groups={"setup"},value="StatusWhileRegisteredWithEntitlements_Test", alwaysRun=true)
	public void afterAttemptListStatusWhileRegisteredWithEntitlements_Test() {
		if (clienttasks==null) return;
		clienttasks.deleteFactsFileWithOverridingValues();
	}
	
	
	
	
	
	// Candidates for an automated Test:
	
	
	
	
	// Configuration methods ***********************************************************************
	
	
	
	// Protected methods ***********************************************************************
	
	
	
	// Data Providers ***********************************************************************
	
}
