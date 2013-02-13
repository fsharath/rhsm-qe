package rhsm.cli.tests;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.json.JSONException;
import org.testng.SkipException;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import rhsm.base.SubscriptionManagerCLITestScript;
import rhsm.data.InstalledProduct;
import rhsm.data.ProductCert;
import rhsm.data.SubscriptionPool;

import com.redhat.qe.Assert;
import com.redhat.qe.tools.RemoteFileTasks;
import com.redhat.qe.tools.SSHCommandResult;

/**
 * @author jsefler
 * 
 * Primary scenario comes from https://bugzilla.redhat.com/show_bug.cgi?id=859197
 * 
 * Uses automation properties:
 * 	sm.ha.username = stage_test_2
 *	sm.ha.password = 
 *	sm.ha.org = 
 *	sm.ha.sku = RH1149049
 *	# RHEL64 High Availability Packages http://download.devel.redhat.com/nightly/latest-RHEL6.4/6.4/Server/x86_64/os/HighAvailability/listing
 *	sm.ha.packages = ccs, cluster-cim, cluster-glue, cluster-glue-libs, cluster-glue-libs-devel, cluster-snmp, clusterlib, clusterlib-devel, cman, corosync, corosynclib, corosynclib-devel, fence-virt, fence-virtd-checkpoint, foghorn, libesmtp-devel, libqb, libqb-devel, libtool-ltdl-devel, luci, modcluster, omping, openais, openaislib, openaislib-devel, pacemaker, pacemaker-cli, pacemaker-cluster-libs, pacemaker-cts, pacemaker-doc, pacemaker-libs, pacemaker-libs-devel, pcs, python-repoze-what-plugins-sql, python-repoze-what-quickstart, python-repoze-who-friendlyform, python-repoze-who-plugins-sa, python-tw-forms, resource-agents, rgmanager, ricci
 *	# RHEL63 High Availability Packages http://download.devel.redhat.com/released/RHEL-6/6.3/Server/x86_64/os/HighAvailability/listing
 *	sm.ha.packages = ccs, cluster-cim, cluster-glue, cluster-glue-libs, cluster-glue-libs-devel, cluster-snmp, clusterlib, clusterlib-devel, cman, corosync, corosynclib, corosynclib-devel, fence-virt, fence-virtd-checkpoint, foghorn, libesmtp-devel, libqb, libqb-devel, libtool-ltdl-devel, luci, modcluster, omping, openais, openaislib, openaislib-devel, pacemaker, pacemaker-cli, pacemaker-cluster-libs, pacemaker-libs, pacemaker-libs-devel, python-repoze-what-plugins-sql, python-repoze-what-quickstart, python-repoze-who-friendlyform, python-repoze-who-plugins-sa, python-tw-forms, resource-agents, rgmanager, ricci
 *
 *
 */
@Test(groups={"HighAvailabilityTests","AcceptanceTests"})
public class HighAvailabilityTests extends SubscriptionManagerCLITestScript {

	// Test methods ***********************************************************************
	
	
	@Test(	description="make sure there are no High Availability packages installed",
			groups={"blockedByBug-904193"},
			priority=10,
			dependsOnMethods={},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void VerifyHighAvailabilityIsNotInstalled_Test() {
		
		// yum clean all to ensure the yum database is reset
		clienttasks.yumClean("all");
		
		// verify no High Availability packages are installed
		boolean haPackagesInstalled = false;
		for (String pkg: sm_haPackages) {
			if (clienttasks.isPackageInstalled(pkg)) {
				haPackagesInstalled = true;
				log.warning("Did not expect High Availability package '"+pkg+"' to be instaled.");				
			}
		}
		Assert.assertTrue(!haPackagesInstalled,"There should NOT be any packages from HighAvialability installed on a fresh install of RHEL '"+clienttasks.releasever+"'.");
		
		// get the currently installed products
		List <InstalledProduct> installedProducts = clienttasks.getCurrentlyInstalledProducts();
		
		// verify High Availability product id is not installed
		InstalledProduct haInstalledProduct = InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", haProductId, installedProducts);
		Assert.assertNull(haInstalledProduct, "The High Availability product id '"+haProductId+"' should NOT be installed.");

		// verify RHEL product server id 69 is installed
		InstalledProduct serverInstalledProduct = InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", serverProductId, installedProducts);
		Assert.assertNotNull(serverInstalledProduct, "The RHEL Server product id '"+serverProductId+"' should be installed.");
	}
	
	
	@Test(	description="verify product database and installed products are in sync",
			groups={},
			priority=12,
			dependsOnMethods={},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void VerifyProductDatabaseIsInSyncWithInstalledProducts_Test() throws JSONException {
		
		// get the installed products and product database map
		List<InstalledProduct> installedProducts = clienttasks.getCurrentlyInstalledProducts();
		Map<String,String> productIdRepoMap = clienttasks.getProductIdRepoMap();
		List<ProductCert> installedProductCerts = clienttasks.getCurrentProductCerts();
		
		// assert that product database and installed products are in sync
		int installedProductCertCount=0;
		for (ProductCert installedProductCert: installedProductCerts) {
			// skip productCerts from TESTDATA
			if (installedProductCert.file.getName().endsWith("_.pem")) {
				log.info("Skipping assertion that product cert '"+installedProductCert.file+"' (manually installed from generated candlepin TESTDATA) is accounted for in the product database '"+clienttasks.productIdJsonFile+"'.");
				continue;
			}
			installedProductCertCount++;
			Assert.assertTrue(productIdRepoMap.containsKey(installedProductCert.productId), "Database '"+clienttasks.productIdJsonFile+"' contains installed product id: "+installedProductCert.productId);
			log.info("Database '"+clienttasks.productIdJsonFile+"' maps installed product id '"+installedProductCert.productId+"' to repo '"+productIdRepoMap.get(installedProductCert.productId)+"'.");
		}
		for (String productId : productIdRepoMap.keySet()) {
			Assert.assertNotNull(InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", productId, installedProducts), "Database '"+clienttasks.productIdJsonFile+"' product id '"+productId+"' is among the installed products.");
		}
		Assert.assertEquals(productIdRepoMap.keySet().size(), installedProductCertCount, "The product id database size matches the number of installed products (excluding TESTDATA products).");
	}
	
	
	@Test(	description="register to the stage/prod environment with credentials to access High Availability product subscription",
			groups={},
			priority=14,
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void RegisterToHighAvailabilityAccount_Test() {
		if (sm_haUsername.equals("")) throw new SkipException("Skipping this test when no value was given for the High Availability Username");

		// register the to an account that offers High Availability subscriptions
		clienttasks.register(sm_haUsername,sm_haPassword,sm_haOrg,null,null,null,null,null,null,null,(String)null,null,null, null, true, null, null, null, null);
	}
	
	
	@Test(	description="verify that a local yum install will not delete the product database when repolist is empty",
			groups={"blockedByBug-806457"},
			priority=16,
			dependsOnMethods={"RegisterToHighAvailabilityAccount_Test"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void VerifyLocalYumInstallAndRemoveDoesNotAlterInstalledProducts_Test() throws JSONException {
		List<InstalledProduct> originalInstalledProducts = clienttasks.getCurrentlyInstalledProducts();
		String originalProductIdJSONString = client.runCommandAndWait("cat "+clienttasks.productIdJsonFile).getStdout();
		
		// assert that there are no active repos
		Assert.assertEquals(clienttasks.getYumRepolist("enabled").size(), 0, "Expected number of enabled yum repositories.");
		
		// get an rpm to perform a local yum install
		String localRpmFile =  String.format("/tmp/%s.rpm",haPackage1);
		RemoteFileTasks.runCommandAndAssert(client, String.format("wget -O %s %s",localRpmFile,haPackage1Fetch), new Integer(0));
		
		// do a yum local install and assert
		SSHCommandResult yumInstallResult = client.runCommandAndWait(String.format("yum -q -y localinstall %s",localRpmFile));
		Assert.assertTrue(clienttasks.isPackageInstalled(haPackage1),"Local install of package '"+haPackage1+"' completed successfully.");
		
		// verify that installed products remains unchanged
		List<InstalledProduct> currentlyInstalledProducts = clienttasks.getCurrentlyInstalledProducts();
		Assert.assertTrue(currentlyInstalledProducts.containsAll(originalInstalledProducts) && originalInstalledProducts.containsAll(currentlyInstalledProducts), "The installed products remains unchanged after a yum local install of package '"+haPackage1+"'.");
		
		// verify that the current product id database was unchanged
		String currentProductIdJSONString = client.runCommandAndWait("cat "+clienttasks.productIdJsonFile).getStdout();
		Assert.assertEquals(currentProductIdJSONString, originalProductIdJSONString, "The product id to repos JSON database file remains unchanged after a yum local install of package '"+haPackage1+"'.");

		// finally remove the locally installed rpm
		clienttasks.yumRemovePackage(haPackage1);
		
		// verify that installed products remains unchanged
		currentlyInstalledProducts = clienttasks.getCurrentlyInstalledProducts();
		Assert.assertTrue(currentlyInstalledProducts.containsAll(originalInstalledProducts) && originalInstalledProducts.containsAll(currentlyInstalledProducts), "The installed products remains unchanged after a yum removal of locally installed package '"+haPackage1+"'.");

		// verify that the current product id database was unchanged
		currentProductIdJSONString = client.runCommandAndWait("cat "+clienttasks.productIdJsonFile).getStdout();
		Assert.assertEquals(currentProductIdJSONString, originalProductIdJSONString, "The product id to repos JSON database file remains unchanged after a yum removal of locally installed package '"+haPackage1+"'.");
	}
	
	
	
	@Test(	description="subscribe to the expected High Availability product subscription",
			groups={},
			priority=20,
			//dependsOnMethods={"VerifyHighAvailabilityIsNotInstalled_Test"},
			dependsOnMethods={"VerifyLocalYumInstallAndRemoveDoesNotAlterInstalledProducts_Test"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void SubscribeToHighAvailabilitySKU_Test() {
		
		// assert that the High Availability subscription SKU is found in the all available list
		List<SubscriptionPool> allAvailableSubscriptionPools = clienttasks.getCurrentlyAllAvailableSubscriptionPools();
		Assert.assertNotNull(SubscriptionPool.findFirstInstanceWithMatchingFieldFromList("productId", sm_haSku, allAvailableSubscriptionPools), "High Availability subscription SKU '"+sm_haSku+"' is available for consumption when the client arch is ignored.");
		
		// assert that the High Availability subscription SKU is found in the available list only on x86_64,x86 arches; see https://docspace.corp.redhat.com/docs/DOC-63084
		List<String> supportedArches = Arrays.asList("x86_64","x86","i386","i686");
		List<SubscriptionPool> availableSubscriptionPools = clienttasks.getCurrentlyAvailableSubscriptionPools();
		SubscriptionPool haPool = SubscriptionPool.findFirstInstanceWithMatchingFieldFromList("productId", sm_haSku, availableSubscriptionPools);
		if (!supportedArches.contains(clienttasks.arch)) {
			Assert.assertNull(haPool, "High Availability subscription SKU '"+sm_haSku+"' should NOT be available for consumption on a system whose arch '"+clienttasks.arch+"' is NOT among the supported arches "+supportedArches);
			throw new SkipException("Cannot consume High Availability subscription SKU '"+sm_haSku+"' on a system whose arch '"+clienttasks.arch+"' is NOT among the supported arches "+supportedArches);
		}
		Assert.assertNotNull(haPool, "High Availability subscription SKU '"+sm_haSku+"' is available for consumption on a system whose arch '"+clienttasks.arch+"' is among the supported arches "+supportedArches);
		
		// Subscribe to the High Availability subscription SKU
		haEntitlementCertFile = clienttasks.subscribeToSubscriptionPool(haPool);
	}
	
	
	@Test(	description="verify the expected High Availability packages are availabile for yum install",
			groups={},
			priority=30,
			dependsOnMethods={"SubscribeToHighAvailabilitySKU_Test"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void VerifyHighAvailabilityPackagesAreAvailabile_Test() {
		
		List<String> availablePackages = clienttasks.getYumListAvailable(null);
		boolean foundAllExpectedPkgs = true;
		for (String expectedPkg: sm_haPackages) {
			boolean foundExpectedPkg = false;
			for (String availablePkg: availablePackages) {	// availablePackages are suffixed by their arch like this: ccs.x86_64 libesmtp.i686 python-tw-forms.noarch
				if (availablePkg.startsWith(expectedPkg+".")) {
					Assert.assertTrue(true, "High Availability package '"+expectedPkg+"' is available for yum install as '"+availablePkg+"'.");
					foundExpectedPkg = true;
				}
			}
			if (!foundExpectedPkg) {
				foundAllExpectedPkgs = false;
				log.warning("Expected High Availability package '"+expectedPkg+"' to be available for yum install.");
			}
		}
		Assert.assertTrue(foundAllExpectedPkgs,"All expected High Availability packages are available for yum install.");
	}
	
	
	@Test(	description="yum install a High Availability package ccs and assert installed products",
			groups={"blockedByBug-859197"},
			priority=40,
			//dependsOnMethods={"VerifyHighAvailabilityPackagesAreAvailabile_Test"},
			dependsOnMethods={"SubscribeToHighAvailabilitySKU_Test"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void YumInstallFirstHighAvailabilityPackageAndAssertInstalledProductCerts_Test() {
		clienttasks.yumInstallPackage(haPackage1);
		
		// get the currently installed products
		List <InstalledProduct> installedProducts = clienttasks.getCurrentlyInstalledProducts();

		// verify High Availability product id is now installed and Subscribed
		InstalledProduct haInstalledProduct = InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", haProductId, installedProducts);
		Assert.assertNotNull(haInstalledProduct, "The High Availability product id '"+haProductId+"' should be installed after successful install of High Availability package '"+haPackage1+"'.");
		Assert.assertEquals(haInstalledProduct.status, "Subscribed", "The status of the installed High Availability product cert.");

		// verify RHEL product server id 69 is installed
		InstalledProduct serverInstalledProduct = InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", serverProductId, installedProducts);
		Assert.assertNotNull(serverInstalledProduct, "The RHEL Server product id '"+serverProductId+"' should still be installed after successful install of High Availability package '"+haPackage1+"'.");
	}
	
	
	@Test(	description="yum install a second High Availability package cman and assert installed products",
			groups={"blockedByBug-859197"},
			priority=50,
			//dependsOnMethods={"YumInstallFirstHighAvailabilityPackageAndAssertInstalledProductCerts_Test"},
			dependsOnMethods={"SubscribeToHighAvailabilitySKU_Test"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void YumInstallSecondHighAvailabilityPackageAndAssertInstalledProductCerts_Test() {
		clienttasks.yumInstallPackage(haPackage2);
		
		// get the currently installed products
		List <InstalledProduct> installedProducts = clienttasks.getCurrentlyInstalledProducts();

		// verify High Availability product id is now installed and Subscribed
		InstalledProduct haInstalledProduct = InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", haProductId, installedProducts);
		Assert.assertNotNull(haInstalledProduct, "The High Availability product id '"+haProductId+"' should be installed after successful install of High Availability package '"+haPackage2+"'.");
		Assert.assertEquals(haInstalledProduct.status, "Subscribed", "The status of the installed High Availability product cert.");

		// verify RHEL product server id 69 is installed
		InstalledProduct serverInstalledProduct = InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", serverProductId, installedProducts);
		Assert.assertNotNull(serverInstalledProduct, "The RHEL Server product id '"+serverProductId+"' should still be installed after successful install of High Availability package '"+haPackage2+"'.");
	}
	
	
	@Test(	description="yum remove second High Availability package cman and assert installed products",
			groups={},
			priority=60,
			dependsOnMethods={"YumInstallSecondHighAvailabilityPackageAndAssertInstalledProductCerts_Test"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void YumRemoveSecondHighAvailabilityPackageAndAssertInstalledProductCerts_Test() {
		clienttasks.yumRemovePackage(haPackage2);
		
		// get the currently installed products
		List <InstalledProduct> installedProducts = clienttasks.getCurrentlyInstalledProducts();

		// verify High Availability product id remains installed and Subscribed
		InstalledProduct haInstalledProduct = InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", haProductId, installedProducts);
		Assert.assertNotNull(haInstalledProduct, "The High Availability product id '"+haProductId+"' should remain installed after successful removal of High Availability package '"+haPackage2+"' (because package "+haPackage1+" is still installed).");
		Assert.assertEquals(haInstalledProduct.status, "Subscribed", "The status of the installed High Availability product cert.");

		// verify RHEL product server id 69 is installed
		InstalledProduct serverInstalledProduct = InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", serverProductId, installedProducts);
		Assert.assertNotNull(serverInstalledProduct, "The RHEL Server product id '"+serverProductId+"' should remain installed after successful removal of High Availability package '"+haPackage2+"'.");
	}
	
	
	@Test(	description="yum remove first High Availability package cman and assert installed products",
			groups={"blockedByBug-859197"},
			priority=70,
			dependsOnMethods={"YumInstallFirstHighAvailabilityPackageAndAssertInstalledProductCerts_Test"},
			//dependsOnMethods={"YumRemoveSecondHighAvailabilityPackageAndAssertInstalledProductCerts_Test"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void YumRemoveFirstHighAvailabilityPackageAndAssertInstalledProductCerts_Test() {
		clienttasks.yumRemovePackage(haPackage1);
		
		// get the currently installed products
		List <InstalledProduct> installedProducts = clienttasks.getCurrentlyInstalledProducts();

		// verify High Availability product id is uninstalled
		InstalledProduct haInstalledProduct = InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", haProductId, installedProducts);
		Assert.assertNull(haInstalledProduct, "The High Availability product id '"+haProductId+"' should no longer be installed after successful removal of High Availability package '"+haPackage1+"' (because no High Availability packages should be installed).");

		// verify RHEL product server id 69 is installed
		InstalledProduct serverInstalledProduct = InstalledProduct.findFirstInstanceWithMatchingFieldFromList("productId", serverProductId, installedProducts);
		Assert.assertNotNull(serverInstalledProduct, "The RHEL Server product id '"+serverProductId+"' should remain installed after successful removal of High Availability package '"+haPackage1+"'.");
	}
	
	
	
	
	
	
	
	// Candidates for an automated Test:
	// TODO Bug 654442 - pidplugin and rhsmplugin should add to the yum "run with pkgs. list" https://github.com/RedHatQE/rhsm-qe/issues/156
	// TODO Bug 705068 - product-id plugin displays "duration" https://github.com/RedHatQE/rhsm-qe/issues/157
	// TODO Bug 706265 - product cert is not getting removed after removing all the installed packages from its repo using yum https://github.com/RedHatQE/rhsm-qe/issues/158
	// TODO Bug 740773 - product cert lost after installing a pkg from cdn-internal.rcm-test.redhat.com https://github.com/RedHatQE/rhsm-qe/issues/159
	
	/* 
	------------------------------------------------------------
	Notes from alikins
	summary: if you register, and subscribe to say, openshift
	(but not rhel), and install something from openshift, you
	can have your productid deleted as inactive. This doesn't
	seem to be a new behaviour. If you are subscribe to rhel
	(even with no rhel packages installed), you seem to be okay.
	
	The openshift guys have ran into this with 6.3, and I belive 6.4 betas.
	There scenario was:
	
	install rhel
	register
	subscribe to rhel (I belive...)
	subscribe to openshift
	yum install something from openshift
	*boom* rhel product id goes away
	
	MUST WE BE SUBSCRIBED TO RHEL?
	> Testing this, it seems that if rhel is just subscribe to (no packages
	> installed or updated) it seems to be okay.
	>
	> However, I'm not really sure I understand why...
	> Adrian
	>
	>
	
	Our logic is this:
	
	if you have a subscription, and if you have a product id which is not
	backed by a susbcription repo hen we should remove it. I think we need
	to change this logic. Is there a bug open for this? If not, I will.
	-- bk

	----------------------------------------------------------------------
	A couple of thoughts from dgregor regarding bug 859197:

	* A product cert should only be removed when there are no packages left
	on the system that came from the corresponding repo.  So, in the above
	example, as long as there are packages on the system that came from
	anaconda-RedHatEnterpriseLinux-201211201732.x86_64, the product cert
	should stay regardless of whether
	anaconda-RedHatEnterpriseLinux-201211201732.x86_64 is still a defined
	repository.

	* Do we ever associate a product cert with multiple repos?  Let's say I
	install a package from rhel-ha-for-rhel-6-server-beta-rpms and that
	pulls in product cert 83.  I then install a second package from
	rhel-ha-for-rhel-6-server-rpms, which is also mapped to cert 83.  If I
	remove that first package, we still want cert 83 on disk.

	-- Dennis
	HOW TO DETERMINE WHAT REPO A PACKAGE CAME FROM

	# yum history package-info filesystem
	Transaction ID : 1
	Begin time     : Sat Oct 13 19:38:17 2012
	Package        : filesystem-2.4.30-3.el6.x86_64
	State          : Install
	Size           : 0
	Build host     : x86-004.build.bos.redhat.com
	Build time     : Tue Jun 28 10:13:32 2011
	Packager       : Red Hat, Inc. <http://bugzilla.redhat.com/bugzilla>
	Vendor         : Red Hat, Inc.
	License        : Public Domain
	URL            : https://fedorahosted.org/filesystem
	Source RPM     : filesystem-2.4.30-3.el6.src.rpm
	Commit Time    : Tue Jun 28 08:00:00 2011
	Committer      : Ondrej Vasik <ovasik@redhat.com>
	Reason         : user
	>From repo      : anaconda-RedHatEnterpriseLinux-201206132210.x86_64
	Installed by   : System <unset>
	
	
	*/
	
	// Configuration methods ***********************************************************************
	
	@BeforeClass(groups="setup")
	public void assertRhelServerBeforeClass() {
		if (clienttasks==null) return;
		if (!clienttasks.releasever.contains("Server")) {	// "5Server" or "6Server"
			throw new SkipException("High Availability tests are only executable on a RHEL Server.");
		}
		if (clienttasks.arch.startsWith("ppc")) {		// ppc ppc64
			serverProductId = "74";	// Red Hat Enterprise Linux for IBM POWER
			throw new SkipException("High Availability is not offered on arch '"+clienttasks.arch+"'.");
		}
		else if (clienttasks.arch.startsWith("s390")) {	// s390 s390x
			serverProductId = "72";	// Red Hat Enterprise Linux for IBM System z
			throw new SkipException("High Availability is not offered on arch '"+clienttasks.arch+"'.");
		}
		else { 											// i386 x86_64
			serverProductId = "69";	// Red Hat Enterprise Linux Server
			if (clienttasks.arch.startsWith("i")) {
				haPackage1Fetch = "http://download.devel.redhat.com/released/RHEL-6/6.1/Server/i386/os/Packages/ccs-0.16.2-35.el6.i686.rpm";
			} else {
				haPackage1Fetch = "http://download.devel.redhat.com/released/RHEL-6/6.1/Server/x86_64/os/Packages/ccs-0.16.2-35.el6.x86_64.rpm";
			}
		}
	}

	@BeforeClass(groups="setup",dependsOnMethods={"assertRhelServerBeforeClass"})
	public void configProductCertDirBeforeClass() {
		if (clienttasks==null) return;
		originalProductCertDir = clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "rhsm", "productCertDir");
		Assert.assertNotNull(originalProductCertDir);
		log.info("Initializing a new product cert directory with the currently installed product certs for this test class...");
		RemoteFileTasks.runCommandAndAssert(client,"mkdir -p "+haProductCertDir,Integer.valueOf(0));
		RemoteFileTasks.runCommandAndAssert(client,"rm -f "+haProductCertDir+"/*.pem",Integer.valueOf(0));
		RemoteFileTasks.runCommandAndAssert(client,"cp "+clienttasks.productCertDir+"/*.pem "+haProductCertDir,Integer.valueOf(0));
		clienttasks.updateConfFileParameter(clienttasks.rhsmConfFile, "productCertDir", haProductCertDir);
	}
	
	@BeforeClass(groups="setup",dependsOnMethods={"assertRhelServerBeforeClass"})
	public void backupProductIdJsonFileBeforeClass() {
		if (clienttasks==null) return;
		RemoteFileTasks.runCommandAndAssert(client,"cat "+clienttasks.productIdJsonFile+" > "+backupProductIdJsonFile, Integer.valueOf(0));
	}
	
	@BeforeClass(groups={"setup"},dependsOnMethods={"backupProductIdJsonFileBeforeClass","configProductCertDirBeforeClass"})
	public void disableAllRepos() {
		// by default on Beaker provisioned hardware, there are numerous beaker-* enabled repos that interfere with this test class
		clienttasks.yumDisableAllRepos("--disableplugin=product-id --disableplugin=subscription-manager");	// disabling these plugins as insurance to avoid product-id contamination before the tests run
	}
	
	
	
	@AfterClass(groups="setup")
	public void unregisterAfterClass() {
		if (clienttasks==null) return;
		client.runCommandAndWait("yum remove "+haPackage1+" "+haPackage2+" -y --disableplugin=rhnplugin"); // or remove all sm_haPackages
		clienttasks.unregister_(null, null, null);
	}
	
	@AfterClass(groups="setup", dependsOnMethods={"unregisterAfterClass"})
	public void unconfigProductCertDirAfterClass() {
		if (clienttasks==null) return;
		if (originalProductCertDir==null) return;	
		log.info("Restoring the originally configured product cert directory...");
		clienttasks.updateConfFileParameter(clienttasks.rhsmConfFile, "productCertDir", originalProductCertDir);
	}
	
	@AfterClass(groups="setup", dependsOnMethods={"unregisterAfterClass"})
	public void restoreProductIdJsonFileAfterClass() {
		if (clienttasks==null) return;
		if (!RemoteFileTasks.testExists(client, backupProductIdJsonFile)) return;
		RemoteFileTasks.runCommandAndAssert(client,"cat "+backupProductIdJsonFile+" > "+clienttasks.productIdJsonFile, Integer.valueOf(0));
	}
	
	
	
	
	// Protected methods ***********************************************************************

	protected String originalProductCertDir			= null;
	protected final String haProductCertDir			= "/tmp/sm-haProductCertDir";
	protected final String backupProductIdJsonFile	= "/tmp/sm-productIdJsonFile";
	protected final String haProductId				= "83";	// Red Hat Enterprise Linux High Availability (for RHEL Server)
	protected /*final*/ String serverProductId		= null;	// set in assertRhelServerBeforeClass()
	protected final String haPackage1				= "ccs";
	protected /*final*/ String haPackage1Fetch		= null;	// set in assertRhelServerBeforeClass()	// released RHEL61 package to wget for testing bug 806457
	protected final String haPackage2				= "cluster-glue-libs";
	File haEntitlementCertFile = null;
	
	
	
	
	
	// Data Providers ***********************************************************************

}
