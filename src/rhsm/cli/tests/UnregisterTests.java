package rhsm.cli.tests;


import java.util.List;

import org.testng.annotations.Test;

import com.redhat.qe.Assert;
import com.redhat.qe.auto.tcms.ImplementsNitrateTest;
import rhsm.base.SubscriptionManagerCLITestScript;
import rhsm.data.SubscriptionPool;

/**
 * @author ssalevan
 *
 */
@Test(groups={"UnregisterTests"})
public class UnregisterTests extends SubscriptionManagerCLITestScript {
	
	
	// Test Methods ***********************************************************************

	@Test(description="unregister the consumer",
			groups={"blockedByBug-589626"},
			enabled=true)
	@ImplementsNitrateTest(caseId=46714)
	public void RegisterSubscribeAndUnregisterTest() {
		clienttasks.unregister(null, null, null);
		clienttasks.register(sm_clientUsername, sm_clientPassword, sm_clientOrg, null, null, null, null, null, null, null, (String)null, null, null, null, null, false, null, null, null);
		List<SubscriptionPool> availPoolsBeforeSubscribingToAllPools = clienttasks.getCurrentlyAvailableSubscriptionPools();
		clienttasks.subscribeToTheCurrentlyAvailableSubscriptionPoolsIndividually();
		clienttasks.unregister(null, null, null);
		clienttasks.register(sm_clientUsername, sm_clientPassword, sm_clientOrg, null, null, null, null, null, null, null, (String)null, null, null, null, null, false, null, null, null);
		for (SubscriptionPool afterPool : clienttasks.getCurrentlyAvailableSubscriptionPools()) {
			SubscriptionPool originalPool = SubscriptionPool.findFirstInstanceWithMatchingFieldFromList("poolId", afterPool.poolId, availPoolsBeforeSubscribingToAllPools);
			Assert.assertEquals(originalPool.quantity, afterPool.quantity,
				"The subscription quantity count for Pool "+originalPool.poolId+" returned to its original count after subscribing to it and then unregistering from the candlepin server.");
		}
	}
	
	// Candidates for an automated Test:
	// TODO Bug 674652 - Subscription Manager Leaves Broken Yum Repos After Unregister
	// TODO Bug 706853 - SM Gui “unregister” button deletes “consumer” folder for non network host.
}
