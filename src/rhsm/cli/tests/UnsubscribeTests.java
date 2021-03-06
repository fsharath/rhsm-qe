package rhsm.cli.tests;

import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.json.JSONException;
import org.testng.SkipException;
import org.testng.annotations.Test;

import rhsm.base.SubscriptionManagerCLITestScript;
import rhsm.cli.tasks.CandlepinTasks;
import rhsm.data.EntitlementCert;
import rhsm.data.ProductSubscription;
import rhsm.data.SubscriptionPool;

import com.redhat.qe.Assert;
import com.redhat.qe.auto.tcms.ImplementsNitrateTest;
import com.redhat.qe.jul.TestRecords;
import com.redhat.qe.tools.RemoteFileTasks;
import com.redhat.qe.tools.SSHCommandResult;

/**
 * @author ssalevan
 * @author jsefler
 *
 */
@Test(groups={"UnsubscribeTests","Tier2Tests"})
public class UnsubscribeTests extends SubscriptionManagerCLITestScript{
	
	
	// Test Methods ***********************************************************************

	@Test(	description="subscription-manager-cli: unsubscribe consumer from an entitlement using product ID",
			groups={"blockedByBug-584137", "blockedByBug-602852", "blockedByBug-873791"},
			//dataProvider="getAllConsumedProductSubscriptionsData",	// 06/04/2014 takes too long; rarely reveals a bug
			dataProvider="getRandomSubsetOfConsumedProductSubscriptionsData",
			enabled=true)
	@ImplementsNitrateTest(caseId=41688)
	public void UnsubscribeFromValidProductIDs_Test(ProductSubscription productSubscription){
//		sm.subscribeToEachOfTheCurrentlyAvailableSubscriptionPools();
//		sm.unsubscribeFromEachOfTheCurrentlyConsumedProductSubscriptions();
		clienttasks.unsubscribeFromProductSubscription(productSubscription);
	}
	
	
	@Test(	description="Unsubscribe product entitlement and re-subscribe",
			groups={"blockedByBug-584137", "blockedByBug-602852", "blockedByBug-873791","blockedByBug-979492"},
			//dataProvider="getAllConsumedProductSubscriptionsData",	// 06/04/2014 takes too long; rarely reveals a bug
			dataProvider="getRandomSubsetOfConsumedProductSubscriptionsData",
			enabled=true)
	@ImplementsNitrateTest(caseId=41898)
	public void ResubscribeAfterUnsubscribe_Test(ProductSubscription productSubscription) throws Exception{
//		sm.subscribeToEachOfTheCurrentlyAvailableSubscriptionPools();
//		sm.unsubscribeFromEachOfTheCurrentlyConsumedProductSubscriptions();
//		sm.subscribeToEachOfTheCurrentlyAvailableSubscriptionPools();
		
//		// first make sure we are subscribed to all pools
//		sm.register(username,password,null,null,null,null);
//		sm.subscribeToEachOfTheCurrentlyAvailableSubscriptionPools();
		
		// now loop through each consumed product subscription and unsubscribe/re-subscribe
		SubscriptionPool pool = clienttasks.getSubscriptionPoolFromProductSubscription(productSubscription,sm_clientUsername,sm_clientPassword);
		if (clienttasks.unsubscribeFromProductSubscription(productSubscription)) {
			Assert.assertNotNull(pool, "Successfully determined what SubscriptionPool ProductSubscription '"+productSubscription+"' was consumed from.");
			clienttasks.subscribeToSubscriptionPool/*UsingProductId*/(pool);	// only re-subscribe when unsubscribe was a success
		}
	}
	
	
	@Test(description="Malicious Test - Unsubscribe and then attempt to reuse the revoked entitlement cert.",
			groups={"AcceptanceTests","Tier1Tests","blockedByBug-584137","blockedByBug-602852","blockedByBug-672122","blockedByBug-804227","blockedByBug-871146","blockedByBug-905546","blockedByBug-962520","blockedByBug-822402","blockedByBug-986572","blockedByBug-1000301","blockedByBug-1026435"},
			//dataProvider="getAvailableSubscriptionPoolsData",	// very thorough, but takes too long to execute and rarely finds more bugs
			dataProvider="getRandomSubsetOfAvailableSubscriptionPoolsData",
			enabled=true)
	@ImplementsNitrateTest(caseId=41903)
	public void UnsubscribeAndAttemptToReuseTheRevokedEntitlementCert_Test(SubscriptionPool subscriptionPool) throws JSONException, Exception{
		client.runCommandAndWaitWithoutLogging("killall -9 yum");
		
		// choose a quantity before subscribing to avoid Stdout: Quantity '1' is not a multiple of instance multiplier '2'
		String quantity = null;
		/*if (clienttasks.isPackageVersion("subscription-manager",">=","1.10.3-1"))*/ if (subscriptionPool.suggested!=null) {
			if (subscriptionPool.suggested<1) quantity = CandlepinTasks.getPoolProductAttributeValue(sm_clientUsername, sm_clientPassword, sm_serverUrl, subscriptionPool.poolId, "instance_multiplier"); 	// when the Suggested quantity is 0, let's specify a quantity to avoid Stdout: Quantity '1' is not a multiple of instance multiplier '2'
			if (subscriptionPool.suggested>1 && quantity==null) quantity = subscriptionPool.suggested.toString();
		}

		// subscribe to a pool
		File entitlementCertFile = clienttasks.subscribeToSubscriptionPool(subscriptionPool,quantity,/*sm_serverAdminUsername*/sm_clientUsername,/*sm_serverAdminPassword*/sm_clientPassword,sm_serverUrl);
		EntitlementCert entitlementCert = clienttasks.getEntitlementCertFromEntitlementCertFile(entitlementCertFile);
		List <EntitlementCert> entitlementCerts = new ArrayList<EntitlementCert>();
		entitlementCerts.add(entitlementCert);

		// assert all of the entitlement certs are reported in the "yum repolist all"
		clienttasks.assertEntitlementCertsInYumRepolist(entitlementCerts,true);
 
		// copy entitlement certificate from location /etc/pki/entitlement/product/ to /tmp
		log.info("Now let's copy the valid entitlement cert to the side so we can maliciously try to reuse it after its serial has been unsubscribed.");
		String randDir = "/tmp/sm-certForSubscriptionPool-"+subscriptionPool.poolId;
		client.runCommandAndWait("rm -rf "+randDir+"; mkdir -p "+randDir);
		client.runCommandAndWait("cp "+entitlementCertFile+" "+randDir+"; cp "+clienttasks.getEntitlementCertKeyFileCorrespondingToEntitlementCertFile(entitlementCertFile)+" "+randDir);
		
		// unsubscribe from the pool (Note: should be the only one subscribed too
		clienttasks.unsubscribeFromSerialNumber(clienttasks.getSerialNumberFromEntitlementCertFile(entitlementCertFile));
		
		// assert all of the entitlement certs are no longer reported in the "yum repolist all"
		clienttasks.assertEntitlementCertsInYumRepolist(entitlementCerts,false);

		/* restarting rhsmcertd takes too long and can screw up the certCheckInterval
		// restart the rhsm cert deamon
		// Note: by passing assertCertificatesUpdate=null, we are assuming that the subsequent assertions will execute within 2 min before the next cert update and waitForRegexInRhsmcertdLog 
		int certFrequency = 2; clienttasks.restart_rhsmcertd(certFrequency, null, null);
		*/
		log.info("Assuming that the currently configured value of certCheckInterval='"+clienttasks.getConfParameter("certCheckInterval")+"' will not interfere with this test.");
		
		// move the copied entitlement certificate from /tmp to location /etc/pki/entitlement/product
		// Note: this is malicious activity (user is trying to continue using entitlement certs that have been unsubscribed)
		client.runCommandAndWait("cp -f "+randDir+"/* "+clienttasks.entitlementCertDir);
		
		// assert all of the entitlement certs are reported in the "yum repolist all" again
		clienttasks.assertEntitlementCertsInYumRepolist(entitlementCerts,true);
		
		// assert that the rhsmcertd will clean up the malicious activity
		/* restarting rhsmcertd takes too long; instead we will call run_rhsmcertd_worker(false)
		log.info("Now let's wait for \"Certificates updated\" by the rhsmcertd and assert that the deamon deletes the copied entitlement certificate since it was put on candlepins certificate revocation list during the unsubscribe.");
		String marker = "Testing UnsubscribeAndAttemptToReuseTheRevokedEntitlementCert_Test..."; // https://tcms.engineering.redhat.com/case/41692/
		RemoteFileTasks.runCommandAndAssert(client,"echo \""+marker+"\" >> "+clienttasks.rhsmcertdLogFile,Integer.valueOf(0));
		clienttasks.waitForRegexInRhsmcertdLog("Certificates updated.", certFrequency);	// https://bugzilla.redhat.com/show_bug.cgi?id=672122
		sleep(10000); // plus a little padding for the client to do it's thing
 		*/clienttasks.run_rhsmcertd_worker(false);
		Assert.assertTrue(!RemoteFileTasks.testExists(client, entitlementCertFile.getPath()),"Entitlement certificate '"+entitlementCertFile+"' was deleted by the rhsm certificate deamon.");
		clienttasks.assertEntitlementCertsInYumRepolist(entitlementCerts,false);
		
		// cleanup
		client.runCommandAndWait("rm -rf "+randDir);
	}

	
	@Test(	description="subscription-manager: subscribe and then unsubscribe from a future subscription pool",
			groups={"blockedByBug-727970","blockedByBug-958775"},
			//dataProvider="getAllFutureSystemSubscriptionPoolsData",	// 06/04/2014 takes too long; rarely reveals a bug
			dataProvider="getRandomSubsetOfFutureSystemSubscriptionPoolsData",
			enabled=true)
			//@ImplementsNitrateTest(caseId=)
	public void UnsubscribeAfterSubscribingToFutureSubscriptionPool_Test(SubscriptionPool pool) throws Exception {
//if (!pool.productId.equals("awesomeos-virt-unlmtd-phys")) throw new SkipException("debugTesting pool productId="+pool.productId);
		
		Calendar now = new GregorianCalendar();
		now.setTimeInMillis(System.currentTimeMillis());
		
		// subscribe to the future SubscriptionPool
		SSHCommandResult subscribeResult = clienttasks.subscribe(null,null,pool.poolId,null,null,null,null,null,null,null, null, null);
		// Pool is restricted to virtual guests: '8a90f85734205a010134205ae8d80403'.
		// Pool is restricted to physical systems: '8a9086d3443c043501443c052aec1298'.
		if (subscribeResult.getStdout().startsWith("Pool is restricted")) {
			throw new SkipException("Subscribing to this future subscription is not applicable to this test: "+pool);
		}
		
		// assert that the granted EntitlementCert and its corresponding key exist
		EntitlementCert entitlementCert = clienttasks.getEntitlementCertCorrespondingToSubscribedPool(pool);
		File entitlementCertFile = clienttasks.getEntitlementCertFileFromEntitlementCert(entitlementCert);
		File entitlementCertKeyFile = clienttasks.getEntitlementCertKeyFileFromEntitlementCert(entitlementCert);
		Assert.assertEquals(RemoteFileTasks.testFileExists(client, entitlementCertFile.getPath()), 1,"EntitlementCert file exists after subscribing to future SubscriptionPool.");
		Assert.assertEquals(RemoteFileTasks.testFileExists(client, entitlementCertKeyFile.getPath()), 1,"EntitlementCert key file exists after subscribing to future SubscriptionPool.");

		// find the consumed ProductSubscription from the future SubscriptionPool
		ProductSubscription productSubscription =  ProductSubscription.findFirstInstanceWithMatchingFieldFromList("serialNumber",entitlementCert.serialNumber, clienttasks.getCurrentlyConsumedProductSubscriptions());
		Assert.assertNotNull(productSubscription,"Found the newly consumed ProductSubscription after subscribing to future subscription pool '"+pool.poolId+"'.");
		
		// unsubscribe
		clienttasks.unsubscribeFromProductSubscription(productSubscription);
		
		// assert that the EntitlementCert file and its key are removed.
		/* NOTE: this assertion is already built into the unsubscribeFromProductSubscription task above
		Assert.assertEquals(RemoteFileTasks.testFileExists(client, entitlementCertFile.getPath()), 0,"EntitlementCert file has been removed after unsubscribing to future SubscriptionPool.");
		Assert.assertEquals(RemoteFileTasks.testFileExists(client, entitlementCertKeyFile.getPath()), 0,"EntitlementCert key file has been removed after unsubscribing to future SubscriptionPool.");
		*/
	}
	
	@Test(description="Attempt to unsubscribe when not registered",
			groups={"blockedByBug-735338","blockedByBug-838146","blockedByBug-865590","blockedByBug-873791"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void UnsubscribeFromSerialWhenNotRegistered_Test() {
	
		// first make sure we are subscribed to a pool
		clienttasks.register(sm_clientUsername,sm_clientPassword,sm_clientOrg,null,null,null,null,null,null,null,(List<String>)null,null,null,null, true, false, null, null, null);
		List<SubscriptionPool> pools = clienttasks.getCurrentlyAvailableSubscriptionPools();
		SubscriptionPool pool = pools.get(randomGenerator.nextInt(pools.size()));	// random available pool
		EntitlementCert entitlementCert = clienttasks.getEntitlementCertFromEntitlementCertFile(clienttasks.subscribeToSubscriptionPool(pool,/*sm_serverAdminUsername*/sm_clientUsername,/*sm_serverAdminPassword*/sm_clientPassword,sm_serverUrl));
		
		// now remove the consumer cert to simulate an unregister
		clienttasks.removeAllCerts(true,false, false);
		SSHCommandResult identityResult = clienttasks.identity_(null,null,null,null,null,null,null);
		if (clienttasks.isPackageVersion("subscription-manager",">=","1.13.8-1")) { // post commit 5697e3af094be921ade01e19e1dfe7b548fb7d5b bug 1119688
			Assert.assertEquals(identityResult.getStderr().trim(),clienttasks.msg_ConsumerNotRegistered, "stderr");
		} else {
			Assert.assertEquals(identityResult.getStdout().trim(),clienttasks.msg_ConsumerNotRegistered, "stdout");
		}
		
		// now unsubscribe from the serial number (while not registered)
		Assert.assertTrue(clienttasks.getCurrentlyConsumedProductSubscriptions().size()>0, "We should be consuming an entitlement (even while not registered)");
		//clienttasks.unsubscribeFromSerialNumber(entitlementCert.serialNumber);	// this will assert a different stdout message, instead call unsubscribe manually and assert results
		SSHCommandResult result = clienttasks.unsubscribe(null,entitlementCert.serialNumber,null,null,null);
		Assert.assertEquals(result.getStdout().trim(), "Subscription with serial number "+entitlementCert.serialNumber+" removed from this system", "We should always be able to remove a subscription (even while not registered).");
		Assert.assertEquals(clienttasks.getCurrentlyConsumedProductSubscriptions().size(), 0, "We should not be consuming any entitlements after unsubscribing (while not registered).");
	}
	
	@Test(description="Attempt to unsubscribe when from an invalid serial number",
			groups={"blockedByBug-706889","blockedByBug-867766"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void UnsubscribeFromAnInvalidSerial_Test() {
		SSHCommandResult result;
		
		BigInteger serial = BigInteger.valueOf(-123);
		result = clienttasks.unsubscribe_(null, serial, null, null, null);
		Integer expectedExitCode = new Integer(255);
		if (clienttasks.isPackageVersion("subscription-manager",">=","1.13.8-1")) expectedExitCode = new Integer(64);	// EX_USAGE // post commit 5697e3af094be921ade01e19e1dfe7b548fb7d5b bug 1119688
		Assert.assertEquals(result.getExitCode(), expectedExitCode, "Asserting exit code when attempting to unsubscribe from an invalid serial number.");
		//Assert.assertEquals(result.getStderr().trim(), "Error: '-123' is not a valid serial number");
		//Assert.assertEquals(result.getStdout().trim(), "");
		// stderr moved to stdout by Bug 867766 - [RFE] unsubscribe from multiple entitlement certificates using serial numbers 
		Assert.assertEquals(result.getStdout().trim(), String.format("Error: '%s' is not a valid serial number",serial));
		Assert.assertEquals(result.getStderr().trim(), "");
		
		List<BigInteger> serials = Arrays.asList(new BigInteger[]{BigInteger.valueOf(123),BigInteger.valueOf(-456),BigInteger.valueOf(789)});
		result = clienttasks.unsubscribe_(null, serials, null, null, null);
		Assert.assertEquals(result.getExitCode(), expectedExitCode, "Asserting exit code when attempting to unsubscribe from an invalid serial number.");
		Assert.assertEquals(result.getStdout().trim(), String.format("Error: '%s' is not a valid serial number",serials.get(1)));
		Assert.assertEquals(result.getStderr().trim(), "");
	}
	
	
	@Test(description="Verify the feedback after unsubscribing from all consumed subscriptions using unsubscribe --all",
			groups={"blockedByBug-812388","blockedByBug-844455"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void UnsubscribeFromAll_Test() {
	
		clienttasks.register(sm_clientUsername,sm_clientPassword,sm_clientOrg,null,null,null,null,null,null,null,(List<String>)null,null,null,null, true, null, null, null, null);
		clienttasks.subscribeToTheCurrentlyAllAvailableSubscriptionPoolsCollectively();
		int numberSubscriptionsConsumed = clienttasks.getCurrentEntitlementCertFiles().size();
		
		// unsubscribe from all and assert # subscriptions are unsubscribed
		SSHCommandResult result = clienttasks.unsubscribe(true, (BigInteger)null, null, null, null);
		//Assert.assertEquals(result.getStdout().trim(), String.format("This machine has been unsubscribed from %s subscriptions",pools.size()),"Expected feedback when unsubscribing from all the currently consumed subscriptions.");	// 10/18/2013 NOT SURE WHAT COMMIT/BUG CAUSED THIS CHANGE TO THE FOLLOWING...
		Assert.assertEquals(result.getStdout().trim(), String.format("%s subscriptions removed at the server."+"\n"+"%s local certificates have been deleted.",numberSubscriptionsConsumed,numberSubscriptionsConsumed),"Expected feedback when unsubscribing from all the currently consumed subscriptions.");
		
		// now attempt to unsubscribe from all again and assert 0 subscriptions are unsubscribed
		result = clienttasks.unsubscribe(true, (BigInteger)null, null, null, null);
		//Assert.assertEquals(result.getStdout().trim(), String.format("This machine has been unsubscribed from %s subscriptions",0),"Expected feedback when unsubscribing from all when no subscriptions are currently consumed.");	// 10/18/2013 NOT SURE WHAT COMMIT/BUG CAUSED THIS CHANGE TO THE FOLLOWING...
		Assert.assertEquals(result.getStdout().trim(), String.format("%s subscriptions removed at the server.",0),"Expected feedback when unsubscribing from all when no subscriptions are currently consumed.");
	}
	
	@Test(description="Verify the feedback after unsubscribing from all consumed subscriptions using unsubscribe --serial SERIAL1 --serial SERIAL2 --serial SERIAL3 etc.",
			groups={"blockedByBug-867766"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void UnsubscribeFromAllSerials_Test() throws Exception {
	
		clienttasks.register(sm_clientUsername,sm_clientPassword,sm_clientOrg,null,null,null,null,null,null,null,(List<String>)null,null,null,null, true, false, null, null, null);
		List<SubscriptionPool> pools = clienttasks.subscribeToTheCurrentlyAllAvailableSubscriptionPoolsCollectively();
		if (pools.isEmpty()) throw new SkipException("This test requires multiple available pools.");
		
		// unsubscribe from all serials in one call and assert the feedback
		List<ProductSubscription> productSubscriptions = clienttasks.getCurrentlyConsumedProductSubscriptions();
		String expectedStdoutMsgLabel;
		expectedStdoutMsgLabel = "Successfully unsubscribed serial numbers:";
		expectedStdoutMsgLabel = "Successfully removed serial numbers:";	// changed by bug 874749
		expectedStdoutMsgLabel = "Serial numbers successfully removed at the server:";	// changed by bug 895447 subscription-manager commit 8e10e76fb5951e0b5d6c867c6c7209d8ec80dead
		String expectedStdoutMsg = expectedStdoutMsgLabel;
		for (ProductSubscription productSubscription : productSubscriptions) expectedStdoutMsg+="\n   "+productSubscription.serialNumber;
		SSHCommandResult result = clienttasks.unsubscribeFromTheCurrentlyConsumedProductSubscriptionsCollectively();
		String actualStdoutMsg = result.getStdout().trim();
		actualStdoutMsg = clienttasks.workaroundForBug906550(actualStdoutMsg);
		
		// NOTE: This expectedStdoutMsg makes a huge assumption about the order of the unsubscribed serial numbers printed to stdout
		// NOTE: TIME TO FIX THIS ASSERTION... Assert.assertEquals(result.getStdout().trim(), expectedStdoutMsg, "Stdout feedback when unsubscribing from all the currently consumed subscriptions.");
		List<String> expectedStdoutMsgAsList = new ArrayList<String>(Arrays.asList(expectedStdoutMsg.split("\n"))); expectedStdoutMsgAsList.remove(expectedStdoutMsgLabel);
		List<String> actualStdoutMsgAsList = new ArrayList<String>(Arrays.asList(actualStdoutMsg.split("\n"))); actualStdoutMsgAsList.remove(expectedStdoutMsgLabel);
		Assert.assertTrue(expectedStdoutMsgAsList.containsAll(actualStdoutMsgAsList) && actualStdoutMsgAsList.containsAll(expectedStdoutMsgAsList), "Stdout feedback when unsubscribing from all the currently consumed subscriptions contains all the expected serial numbers:"+expectedStdoutMsg.replace(expectedStdoutMsgLabel, ""));
	}
	
	@Test(description="Verify the feedback after unsubscribing from all consumed subscriptions (including revoked serials) using unsubscribe --serial SERIAL1 --serial SERIAL2 --serial SERIAL3 etc.",
			groups={"blockedByBug-867766"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void UnsubscribeFromAllSerialsIncludingRevokedSerials_Test() throws JSONException, Exception {
	
		clienttasks.register(sm_clientUsername,sm_clientPassword,sm_clientOrg,null,null,null,null,null,null,null,(List<String>)null,null,null,null,true,false,null,null,null);
		List<SubscriptionPool> pools = clienttasks.getCurrentlyAllAvailableSubscriptionPools();
		if (pools.isEmpty()) throw new SkipException("This test requires multiple available pools.");
		
		// which poolIds are modifiers?
		Set<String> modifierPoolIds = new HashSet<String>();
		for (SubscriptionPool subscriptionPool : pools) if (CandlepinTasks.isPoolAModifier(sm_clientUsername,sm_clientPassword, subscriptionPool.poolId, sm_serverUrl)) modifierPoolIds.add(subscriptionPool.poolId);
		
		// subscribe to all of the available pools
		List<String> poolIds = new ArrayList<String>();
		for (SubscriptionPool pool : pools) poolIds.add(pool.poolId);
		clienttasks.subscribe(null, null, poolIds, null, null, null, null, null, null, null, null, null);
		
		// prepare a list of currently consumed serials that we can use to collectively unsubscribe from
		List<BigInteger> serials = new ArrayList<BigInteger>();
		for(ProductSubscription productSubscription : clienttasks.getCurrentlyConsumedProductSubscriptions()) {
			// insert modifiers at the head of the serials list so their removal won't cause a cert regeneration
			if (!productSubscription.poolId.equals("Unknown")/*indicative of an older candlepin*/ && modifierPoolIds.contains(productSubscription.poolId))
				serials.add(0,productSubscription.serialNumber);
			else
				serials.add(productSubscription.serialNumber);
		}
		
		// unsubscribe from all serials in one call and assert the feedback;
		SSHCommandResult result = clienttasks.unsubscribe(null,serials,null,null,null);
		String actualStdoutMsg = result.getStdout().trim();
		actualStdoutMsg = clienttasks.workaroundForBug906550(actualStdoutMsg);
		String expectedStdoutMsg;
		expectedStdoutMsg = "Successfully unsubscribed serial numbers:";	// changed by bug 874749
		expectedStdoutMsg = "Successfully removed serial numbers:";
		expectedStdoutMsg = "Serial numbers successfully removed at the server:";	// changed by bug 895447 subscription-manager commit 8e10e76fb5951e0b5d6c867c6c7209d8ec80dead
		for (BigInteger serial : serials) expectedStdoutMsg+="\n   "+serial;	// NOTE: This expectedStdoutMsg makes a huge assumption about the order of the unsubscribed serial numbers printed to stdout
		Assert.assertEquals(actualStdoutMsg, expectedStdoutMsg, "Stdout feedback when unsubscribing from all the currently consumed subscriptions.");
		
		// remember the unsubscribed serials as revoked serials
		List<BigInteger> revokedSerials = new ArrayList<BigInteger>();
		for (BigInteger serial : serials) revokedSerials.add(serial);
		
		// re-subscribe to all the available pools again
		clienttasks.subscribe(null, null, poolIds, null, null, null, null, null, null, null, null, null);
		
		// re-prepare a list of currently consumed serials that we can use to collectively unsubscribe from
		// include the revokedSerials by interleaving them into the currently consumed serials
		serials.clear();
		List<ProductSubscription> productSubscriptions = clienttasks.getCurrentlyConsumedProductSubscriptions();
		for (int i=0; i<productSubscriptions.size(); i++) {
			ProductSubscription productSubscription = productSubscriptions.get(i);
			// insert modifiers at the head of the serials list so their removal won't cause a cert regeneration
			if (!productSubscription.poolId.equals("Unknown")/*indicative of an older candlepin*/ && modifierPoolIds.contains(productSubscription.poolId))
				serials.add(0,productSubscription.serialNumber);
			else
				serials.add(productSubscription.serialNumber);
			
			// interleave the former revokedSerials amongst the serials
			serials.add(revokedSerials.get(i));
		}
		
		// now attempt to unsubscribe from both the current serials AND the previously consumed serials in one call and assert the feedback
		result = clienttasks.unsubscribe(null,serials,null,null,null);
		actualStdoutMsg = result.getStdout().trim();
		actualStdoutMsg = clienttasks.workaroundForBug906550(actualStdoutMsg);
		expectedStdoutMsg = "Successfully unsubscribed serial numbers:";	// added by bug 867766	// changed by bug 874749
		expectedStdoutMsg = "Successfully removed serial numbers:";
		expectedStdoutMsg = "Serial numbers successfully removed at the server:";	// changed by bug 895447 subscription-manager commit 8e10e76fb5951e0b5d6c867c6c7209d8ec80dead
		for (BigInteger serial : serials) if (!revokedSerials.contains(serial)) expectedStdoutMsg+="\n   "+serial;	// NOTE: This expectedStdoutMsg makes a huge assumption about the order of the unsubscribed serial numbers printed to stdout
		expectedStdoutMsg +="\n";
		//expectedStdoutMsg += "Unsuccessfully unsubscribed serial numbers:";	// added by bug 867766	// changed by bug 874749
		//expectedStdoutMsg += "Unsuccessfully removed serial numbers:";
		expectedStdoutMsg += "Serial numbers unsuccessfully removed at the server:";	// changed by bug 895447 subscription-manager commit 8e10e76fb5951e0b5d6c867c6c7209d8ec80dead
		//for (BigInteger revokedSerial : revokedSerials) expectedStdoutMsg+="\n   "+String.format("Entitlement Certificate with serial number %s could not be found.", revokedSerial);	// NOTE: This expectedStdoutMsg makes a huge assumption about the order of the unsubscribed serial numbers printed to stdout
		for (BigInteger revokedSerial : revokedSerials) expectedStdoutMsg+="\n   "+String.format("Entitlement Certificate with serial number '%s' could not be found.", revokedSerial);	// NOTE: This expectedStdoutMsg makes a huge assumption about the order of the unsubscribed serial numbers printed to stdout
		Assert.assertEquals(actualStdoutMsg, expectedStdoutMsg, "Stdout feedback when unsubscribing from all the currently consumed subscriptions (including revoked serials).");
	}
//TOO MUCH LOGGING FROM TOO MANY ASSERTIONS;  DELETEME IF ABOVE TEST WORKS WELL
//	public void UnsubscribeFromAllSerialsIncludingRevokedSerials_Test() {
//		
//		clienttasks.register(sm_clientUsername,sm_clientPassword,sm_clientOrg,null,null,null,null,null,null,null,(List<String>)null,null,null,true, null, null, null, null);
//		List<SubscriptionPool> pools = clienttasks.subscribeToTheCurrentlyAllAvailableSubscriptionPoolsCollectively();
//		if (pools.isEmpty()) throw new SkipException("This test requires multiple available pools.");
//		
//		// unsubscribe from all serials in one call and assert the feedback
//		List<ProductSubscription> productSubscriptions = clienttasks.getCurrentlyConsumedProductSubscriptions();
//		SSHCommandResult result = clienttasks.unsubscribeFromTheCurrentlyConsumedProductSubscriptionsCollectively();
//		String expectedStdoutMsg = "Successfully unsubscribed serial numbers:";
//		for (ProductSubscription productSubscription : productSubscriptions) expectedStdoutMsg+="\n   "+productSubscription.serialNumber;	// NOTE: This expectedStdoutMsg makes a huge assumption about the order of the unsubscribed serial numbers printed to stdout
//		Assert.assertEquals(result.getStdout().trim(), expectedStdoutMsg, "Stdout feedback when unsubscribing from all the currently consumed subscriptions.");
//
//		List<BigInteger> revokedSerials = new ArrayList<BigInteger>();
//		for (ProductSubscription productSubscription : productSubscriptions) revokedSerials.add(productSubscription.serialNumber);
//		
//		// subscribe to all the available pools again
//		clienttasks.subscribeToTheCurrentlyAllAvailableSubscriptionPoolsCollectively();
//		
//		// now attempt to unsubscribe from both the current serials AND the previously consumed serials in one call and assert the feedback
//		productSubscriptions = clienttasks.getCurrentlyConsumedProductSubscriptions();
//		List<BigInteger> serials = new ArrayList<BigInteger>();
//		for (int i=0; i<productSubscriptions.size(); i++) {
//			serials.add(productSubscriptions.get(i).serialNumber);
//			serials.add(revokedSerials.get(i));
//		}
//		expectedStdoutMsg = "Successfully unsubscribed serial numbers:";
//		for (ProductSubscription productSubscription : productSubscriptions) expectedStdoutMsg+="\n   "+productSubscription.serialNumber;	// NOTE: This expectedStdoutMsg makes a huge assumption about the order of the unsubscribed serial numbers printed to stdout
//		expectedStdoutMsg +="\n";
//		expectedStdoutMsg += "Unsuccessfully unsubscribed serial numbers:";
//		for (BigInteger revokedSerial : revokedSerials) expectedStdoutMsg+="\n   "+String.format("Entitlement Certificate with serial number %s could not be found.", revokedSerial);	// NOTE: This expectedStdoutMsg makes a huge assumption about the order of the unsubscribed serial numbers printed to stdout
//		result = clienttasks.unsubscribe(false,serials,null,null,null);
//		Assert.assertEquals(result.getStdout().trim(), expectedStdoutMsg, "Stdout feedback when unsubscribing from all the currently consumed subscriptions (including revoked serials).");
//	}
	
	
	@Test(	description="subscription-manager: unsubscribe and remove can be used interchangably",
			groups={"blockedByBug-874749"},
			enabled=true)
			//@ImplementsNitrateTest(caseId=)
	public void RemoveDeprecatesUnsubscribe_Test() throws Exception {
		SSHCommandResult result = client.runCommandAndWait(clienttasks.command+" --help");
		Assert.assertContainsMatch(result.getStdout(), "^\\s*unsubscribe\\s+Deprecated, see remove$");
		
		SSHCommandResult unsubscribeResult;
		SSHCommandResult removeResult;
		
		clienttasks.register(sm_clientUsername, sm_clientPassword, sm_clientOrg, null, null, null, null, null, null, null, (String)null, null, null, null, true, false, null, null, null);
		unsubscribeResult = client.runCommandAndWait(clienttasks.command+" unsubscribe --serial=123");
		removeResult = client.runCommandAndWait(clienttasks.command+" remove --serial=123");
		Assert.assertEquals(unsubscribeResult.toString(), removeResult.toString(), "Results from 'unsubscribe' and 'remove' module commands should be identical.");
		unsubscribeResult = client.runCommandAndWait(clienttasks.command+" unsubscribe --all");
		removeResult = client.runCommandAndWait(clienttasks.command+" remove --all");
		Assert.assertEquals(unsubscribeResult.toString(), removeResult.toString(), "Results from 'unsubscribe' and 'remove' module commands should be identical.");

		clienttasks.unregister(null,null,null);
		unsubscribeResult = client.runCommandAndWait(clienttasks.command+" unsubscribe --serial=123");
		removeResult = client.runCommandAndWait(clienttasks.command+" remove --serial=123");
		Assert.assertEquals(unsubscribeResult.toString(), removeResult.toString(), "Results from 'unsubscribe' and 'remove' module commands should be identical.");
		unsubscribeResult = client.runCommandAndWait(clienttasks.command+" unsubscribe --all");
		removeResult = client.runCommandAndWait(clienttasks.command+" remove --all");
		Assert.assertEquals(unsubscribeResult.toString(), removeResult.toString(), "Results from 'unsubscribe' and 'remove' module commands should be identical.");
	}
	
	
	@Test(	description="after attaching many subscriptions (in a different order) to two different consumers, call unsubscribe --all on each consumer",
			groups={"blockedByBug-1095939"})
			//@ImplementsNitrateTest(caseId=)
	public void MultiClientAttemptToDeadLockOnUnsubscribeAll_Test() throws JSONException, Exception {
		if (client2tasks==null) throw new SkipException("This multi-client test requires a second client.");
		
		// register two clients
		String client1ConsumerId = client1tasks.getCurrentConsumerId(client1tasks.register(sm_client1Username, sm_client1Password, sm_client1Org, null, null, null, null, null, null, null, (List<String>)null, null, null, null, true, false, null, null, null));
		String client2ConsumerId = client2tasks.getCurrentConsumerId(client2tasks.register(sm_client2Username, sm_client2Password, sm_client2Org, null, null, null, null, null, null, null, (List<String>)null, null, null, null, true, false, null, null, null));
		String client1OwnerKey = CandlepinTasks.getOwnerKeyOfConsumerId(sm_client1Username, sm_client1Password, sm_serverUrl, client1ConsumerId);
		String client2OwnerKey = CandlepinTasks.getOwnerKeyOfConsumerId(sm_client2Username, sm_client2Password, sm_serverUrl, client2ConsumerId);
		if (!client1OwnerKey.equals(client2OwnerKey)) throw new SkipException("This multi-client test requires that both client registerers belong to the same owner. (client1: username="+sm_client1Username+" ownerkey="+client1OwnerKey+") (client2: username="+sm_client2Username+" ownerkey="+client2OwnerKey+")");
		
		// get a list of all of the available pools for each client
		List<SubscriptionPool> client1pools = client1tasks.getCurrentlyAllAvailableSubscriptionPools();
		List<SubscriptionPool> client2pools = client2tasks.getCurrentlyAllAvailableSubscriptionPools();
		
		// attempt this test more than once
		for (int attempt=1; attempt<5; attempt++) {
			
			// subscribe each client to each of the pools in a different order (this is crucial)
			List<String> client1poolIds = new ArrayList<String>();
			List<String> client2poolIds = new ArrayList<String>();
			for (SubscriptionPool pool : /*getRandomList(*/client1pools/*)*/) client1poolIds.add(pool.poolId);
			for (SubscriptionPool pool : /*getRandomList(*/client2pools/*)*/) client2poolIds.add(0,pool.poolId);
			client1tasks.subscribe_(null, null, client1poolIds, null, null, null, null, null, null, null, null, null);
			client2tasks.subscribe_(null, null, client2poolIds, null, null, null, null, null, null, null, null, null);
			
			// unsubscribe from all subscriptions on each client simultaneously
			log.info("Simultaneously attempting to unsubscribe all on '"+client1tasks.hostname+"' and '"+client2tasks.hostname+"'...");
			client1.runCommand/*AndWait*/(client1tasks.unsubscribeCommand(true, null, null, null, null), TestRecords.action());
			client2.runCommand/*AndWait*/(client2tasks.unsubscribeCommand(true, null, null, null, null), TestRecords.action());
			client1.waitForWithTimeout(new Long(10*60*1000)); // timeout after 10 min
			client2.waitForWithTimeout(new Long(10*60*1000)); // timeout after 10 min
			SSHCommandResult client1Result = client1.getSSHCommandResult();
			SSHCommandResult client2Result = client2.getSSHCommandResult();
			//	201405091632:43.313 - INFO: SSHCommandResult from an attempt to unsubscribe all on 'jsefler-7server.usersys.redhat.com': 
			//		exitCode=255
			//		stdout=''
			//		stderr='Runtime Error ERROR: deadlock detected
			//		  Detail: Process 3247 waits for ShareLock on transaction 45358106; blocked by process 3221.
			//		Process 3221 waits for ShareLock on transaction 45358105; blocked by process 3247.
			//		  Hint: See server log for query details. at org.postgresql.core.v3.QueryExecutorImpl.receiveErrorResponse:2,102'
			
			// assert the results
			log.info("SSHCommandResult from an attempt to unsubscribe all on '"+client1tasks.hostname+"': \n"+client1Result);
			log.info("SSHCommandResult from an attempt to unsubscribe all on '"+client2tasks.hostname+"': \n"+client2Result);
			Assert.assertEquals(client1Result.getExitCode(), Integer.valueOf(0), "The exit code from the unsubscribe all command on '"+client1tasks.hostname+"'.");
			Assert.assertEquals(client1Result.getStderr(), "", "Stderr from the unsubscribe all on '"+client1tasks.hostname+"'.");
			Assert.assertEquals(client2Result.getExitCode(), Integer.valueOf(0), "The exit code from the unsubscribe all command on '"+client2tasks.hostname+"'.");
			Assert.assertEquals(client2Result.getStderr(), "", "Stderr from the unsubscribe all on '"+client2tasks.hostname+"'.");
		}
	}
	
	
	
	
	
	
	// Candidates for an automated Test:
	

	
	// Data Providers ***********************************************************************
	
	
	
	
}
