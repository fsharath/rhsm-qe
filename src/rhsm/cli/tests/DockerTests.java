package rhsm.cli.tests;

import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.testng.SkipException;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterGroups;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeGroups;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import rhsm.base.SubscriptionManagerCLITestScript;
import rhsm.data.ContentNamespace;
import rhsm.data.EntitlementCert;
import rhsm.data.ProductCert;
import rhsm.data.YumRepo;

import com.redhat.qe.Assert;
import com.redhat.qe.auto.bugzilla.BlockedByBzBug;
import com.redhat.qe.auto.testng.TestNGUtils;
import com.redhat.qe.tools.RemoteFileTasks;
import com.redhat.qe.tools.SSHCommandResult;

/**
 * @author jsefler
 * 
 * Getting started with Docker in RHEL7
 *   https://access.redhat.com/articles/881893
 *
 * DEV Sprint 76 Demo
 *  Subscription Manager Container Mode (dgoodwin)
 *    Video: https://sas.elluminate.com/p.jnlp?psid=2014-06-11.0638.M.D38450C42DA81F82F8E4981A4E1190.vcr&sid=819
 *  
 * DEV Sprint 82 Demo
 *  Subscription Manager ContainerContentPlugin (dgoodwin)
 *    Video: https://sas.elluminate.com/p.jnlp?psid=2014-10-22.0645.M.AEBE7425F4036682CD070CAC3BC449.vcr&sid=819
 *    
 * Delivery options for docker package on rhel7
 *   https://brewweb.devel.redhat.com/packageinfo?packageID=13865
 *   wget --no-check-certificate -nv -O docker.rpm "http://auto-services.usersys.redhat.com/latestrpm/get_latest_rpm.py?regress=true&arch=x86_64&release=el7&rpmname=docker&version=1.0.0"
 *   http://download.devel.redhat.com/nightly/latest-EXTRAS-7-RHEL-7/compose/Server/x86_64/
 *   
 * Red Hat Docker images
 *   http://docker-registry.usersys.redhat.com:8080/#redhat
 *     docker-registry.usersys.redhat.com/brew/rhel7:latest
 *     docker-registry.usersys.redhat.com/brew/rhel6:latest
 *     
 *     
 */
@Test(groups={"DockerTests","Tier3Tests"})
public class DockerTests extends SubscriptionManagerCLITestScript {

	// Test methods ***********************************************************************
	
	@Test(	description="Verify that when in container mode, attempts to run subscription-manager are blocked",
			groups={"VerifySubscriptionManagementCommandIsDisabledInContainerMode_Test","blockedbyBug-1114126"},
			dataProvider="getSubscriptionManagementCommandData",
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void VerifySubscriptionManagementCommandIsDisabledInContainerMode_Test(Object bugzilla, String helpCommand) {
		
		// CLOSED WONTFIX exceptions
		if (helpCommand.contains("subscription-manager-gui")) throw new SkipException("Disabled use of '"+helpCommand+"' in container mode was CLOSED WONTFIX.  See https://bugzilla.redhat.com/show_bug.cgi?id=1114132#c5");
		if (helpCommand.startsWith("rhn-migrate-classic-to-rhsm")) throw new SkipException("Disabled use of '"+helpCommand+"' in container mode was CLOSED WONTFIX.  See https://bugzilla.redhat.com/show_bug.cgi?id=1114132#c5");
		if (helpCommand.startsWith("rhsmcertd")) throw new SkipException("Disabled use of '"+helpCommand+"' in container mode was CLOSED WONTFIX.  See https://bugzilla.redhat.com/show_bug.cgi?id=1114132#c5");
		
		SSHCommandResult result = client.runCommandAndWait(helpCommand);
		Integer expectedExitCode = new Integer(255);
		if (clienttasks.isPackageVersion("subscription-manager",">=","1.13.8-1")) expectedExitCode = new Integer(78);	// EX_CONFIG	// post commit df95529a5edd0be456b3528b74344be283c4d258 bug 1119688
		Assert.assertEquals(result.getExitCode(), expectedExitCode, "ExitCode from attempting command '"+helpCommand+"' while in container mode.");
		Assert.assertEquals(result.getStderr().trim(), clienttasks.msg_ContainerMode, "Stderr from attempting command '"+helpCommand+"' while in container mode.");	
		Assert.assertEquals(result.getStdout().trim(), "", "Stdout from attempting command '"+helpCommand+"' while in container mode.");	
	}
	
	@AfterClass(groups={"setup"})	// insurance
	@AfterGroups(groups={"setup"}, value={"VerifySubscriptionManagementCommandIsDisabledInContainerMode_Test"})
	public void teardownContainerMode() {
		if (clienttasks!=null) {
			client.runCommandAndWait("rm -rf "+rhsmHostDir);
			client.runCommandAndWait("rm -rf "+entitlementHostDir);	// although it would be okay to leave this behind
		}
	}
	
	@BeforeGroups(groups={"setup"}, value={"VerifySubscriptionManagementCommandIsDisabledInContainerMode_Test"})
	protected void setupContainerMode() {
		if (clienttasks!=null) {
			client.runCommandAndWait("rm -rf "+rhsmHostDir);
			client.runCommandAndWait("mkdir "+rhsmHostDir);
			client.runCommandAndWait("cp -r /etc/rhsm/* "+rhsmHostDir);
			client.runCommandAndWait("rm -rf "+entitlementHostDir);
			client.runCommandAndWait("mkdir "+entitlementHostDir);
			Assert.assertTrue(RemoteFileTasks.testExists(client, rhsmHostDir+"/ca"), "After setting up container mode, directory '"+rhsmHostDir+"/ca"+"' should exist.");
			Assert.assertTrue(RemoteFileTasks.testExists(client, rhsmHostDir+"/rhsm.conf"), "After setting up container mode, file '"+rhsmHostDir+"/rhsm.conf"+"' should exist.");
			Assert.assertTrue(RemoteFileTasks.testExists(client, entitlementHostDir), "After setting up container mode, directory '"+entitlementHostDir+"' should exist.");
		}
	}
	
	
	
	
	
	
	
	
	
	@Test(	description="Verify that when in container mode, redhat.repo is populated from the entitlements in /etc/rhsm/entitlement-host",
			groups={"VerifySubscriptionManagementEntitlementsInContainerMode_Test"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void VerifySubscriptionManagementEntitlementsInContainerMode_Test() {
		
		// start by registering the host with autosubscribe to gain some entitlements...
		log.info("Start fresh by registering the host with autosubscribe and getting the host's yum repolist...");
		consumerId = clienttasks.getCurrentConsumerId(clienttasks.register(sm_clientUsername,sm_clientPassword,sm_clientOrg,null,null,null,null,true,null,null,(String)null,null,null, null, true, false, null, null, null));
		
		// remember the yum repolist and the subscribed YumRepo data on the host
		List<YumRepo> subscribedYumReposOnHost = clienttasks.getCurrentlySubscribedYumRepos();
		if (subscribedYumReposOnHost.isEmpty()) throw new SkipException("Skipping this test when no redhat.repo content is granted.  (Expected autosubscribe to grant some entitlements.)");
		List<String> yumRepolistOnHost = clienttasks.getYumRepolist("all");
		if (yumRepolistOnHost.isEmpty()) throw new SkipException("Skipping this test when yum repolist all is empty.  (Should always pass since the prior assert for some granted entitlements passed.)");
		
		// put the system into container mode
		setupContainerMode();
		
		// verify that no content is available when /etc/pki/entitlement-host is empty
		String entitlementCertDir = clienttasks.getConfFileParameter(clienttasks.rhsmConfFile, "rhsm", "entitlementCertDir");
		client.runCommandAndWait("ls -l "+entitlementCertDir);
		client.runCommandAndWait("ls -l "+entitlementHostDir);
		List<String> yumRepolistOnContainer = clienttasks.getYumRepolist("all");
		Assert.assertTrue(yumRepolistOnContainer.size()<yumRepolistOnHost.size(),"When in container mode (with *no* entitlements in '"+entitlementHostDir+"'), the number of yum repolists available should have dimmished (by the number of redhat.repo repos on the host)");
		List<YumRepo> subscribedYumReposOnContainer = clienttasks.getCurrentlySubscribedYumRepos();
		Assert.assertTrue(subscribedYumReposOnContainer.isEmpty(),"When in container mode (with *no* entitlements in '"+entitlementHostDir+"'), there should be no redhat.repo content available.");

		// put the host's entitlements into /etc/pki/entitlement-host
		client.runCommandAndWait("cp "+entitlementCertDir+"/* "+entitlementHostDir);
		client.runCommandAndWait("ls -l "+entitlementHostDir);
		
		// clean the consumer  TODO: FIXME - cleaning the host consumer will orphan all of his entitlements.  This should be properly deleted in an AfterGroup
		log.info("Deleting the consumer cert (containers don't depend on a consumer)...");
		clienttasks.removeAllCerts(true, true, false);
		
		// verify that the host entitlements are now accessible when in container mode
		yumRepolistOnContainer = clienttasks.getYumRepolist("all");
		Assert.assertTrue(yumRepolistOnContainer.containsAll(yumRepolistOnHost)&&yumRepolistOnHost.containsAll(yumRepolistOnContainer),"When in container mode, the entitlements in '"+entitlementHostDir+"' are reflected in yum repolist all. (yum repolist all in the container matches yum repolist all from the host)");
		subscribedYumReposOnContainer = clienttasks.getCurrentlySubscribedYumRepos();
		// Note: the subscribedYumReposOnContainer should only differ from the subscribedYumReposOnHost by the value of the entitlement cert dir path:
		// sslclientcert = /etc/pki/entitlement-host/2166701319103111701.pem
		// sslclientkey = /etc/pki/entitlement-host/2166701319103111701-key.pem
		// sslcacert = /etc/rhsm-host/ca/redhat-uep.pem
		Assert.assertEquals(subscribedYumReposOnContainer.size(),subscribedYumReposOnHost.size(),"When in container mode, the redhat.repo content in available reflects the same list of redhat.repo content available on the host.  (Size check only.)");
		for (YumRepo subscribedYumRepoOnHost : subscribedYumReposOnHost) {
			YumRepo subscribedYumRepoOnContainer = YumRepo.findFirstInstanceWithMatchingFieldFromList("id", subscribedYumRepoOnHost.id, subscribedYumReposOnContainer);
			Assert.assertEquals(subscribedYumRepoOnContainer.name, subscribedYumRepoOnHost.name,"YumRepo ["+subscribedYumRepoOnHost.id+"] data 'name' compares between host and container entitlements.");
			Assert.assertEquals(subscribedYumRepoOnContainer.baseurl, subscribedYumRepoOnHost.baseurl,"YumRepo ["+subscribedYumRepoOnHost.id+"] data 'baseurl' compares between host and container entitlements.");
			if (subscribedYumRepoOnContainer.gpgkey!=null)			Assert.assertEquals(subscribedYumRepoOnContainer.gpgkey, subscribedYumRepoOnHost.gpgkey,"YumRepo ["+subscribedYumRepoOnHost.id+"] data 'gpgkey' compares between host and container entitlements.");
			if (subscribedYumRepoOnContainer.gpgcheck!=null)		Assert.assertEquals(subscribedYumRepoOnContainer.gpgcheck, subscribedYumRepoOnHost.gpgcheck,"YumRepo ["+subscribedYumRepoOnHost.id+"] data 'gpgcheck' compares between host and container entitlements.");
			if (subscribedYumRepoOnContainer.enabled!=null)			Assert.assertEquals(subscribedYumRepoOnContainer.enabled, subscribedYumRepoOnHost.enabled,"YumRepo ["+subscribedYumRepoOnHost.id+"] data 'enabled' compares between host and container entitlements.");
			if (subscribedYumRepoOnContainer.metadata_expire!=null)	Assert.assertEquals(subscribedYumRepoOnContainer.metadata_expire, subscribedYumRepoOnHost.metadata_expire,"YumRepo ["+subscribedYumRepoOnHost.id+"] data 'metadata_expire' compares between host and container entitlements.");
			// TODO could continue adding more asserts for field equality like these ^
			Assert.assertTrue(subscribedYumRepoOnContainer.sslclientcert.replaceFirst(entitlementHostDir, "").equals(subscribedYumRepoOnHost.sslclientcert.replaceFirst(entitlementCertDir, "")),"YumRepo ["+subscribedYumRepoOnHost.id+"] data 'sslclientcert' between host '"+subscribedYumRepoOnHost.sslclientcert+"' and container '"+subscribedYumRepoOnContainer.sslclientcert+"' entitlements differ only by directory path.");
			Assert.assertTrue(subscribedYumRepoOnContainer.sslclientkey.replaceFirst(entitlementHostDir, "").equals(subscribedYumRepoOnHost.sslclientkey.replaceFirst(entitlementCertDir, "")),"YumRepo ["+subscribedYumRepoOnHost.id+"] data 'sslclientkey' between host '"+subscribedYumRepoOnHost.sslclientkey+"' and container '"+subscribedYumRepoOnContainer.sslclientkey+"' entitlements differ only by directory path.");
			Assert.assertTrue(subscribedYumRepoOnContainer.sslcacert.replaceFirst(rhsmHostDir, "").equals(subscribedYumRepoOnHost.sslcacert.replaceFirst("/etc/rhsm", "")),"YumRepo ["+subscribedYumRepoOnHost.id+"] data 'sslcacert' between host '"+subscribedYumRepoOnHost.sslcacert+"' and container '"+subscribedYumRepoOnContainer.sslcacert+"' entitlements differ only by directory path.");
		}
	}
	@AfterGroups(groups={"setup"},value="VerifySubscriptionManagementEntitlementsInContainerMode_Test")
	public void teardownVerifySubscriptionManagementEntitlementsInContainerMode_Test() {
		teardownContainerMode();
		if (consumerId!=null) {
			clienttasks.register_(sm_clientUsername,sm_clientPassword,sm_clientOrg,null,null,null,consumerId,null,null,null,(String)null,null,null, null, true, null, null, null, null);
			clienttasks.unregister_(null, null, null);
			consumerId=null;
		}
	}
	protected String consumerId=null;
	
	
	@Test(	description="Verify that subscription-manager-container-plugin production needed registry_hostnames and CA certs",
			groups={"AcceptanceTests","blockedbyBug-1184940","blockedbyBug-1186386"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void VerifyExpectedRegistryHostnamesAreConfigured_Test() {
		clienttasks.unregister(null, null, null);
		
		// get the list of registry_hostnames from /etc/rhsm/pluginconf.d/container_content.ContainerContentPlugin.conf
		String registry_hostnames = clienttasks.getConfFileParameter(containerContentPluginFile.getPath(), "registry_hostnames");
		List<String> registryHostnames = Arrays.asList(registry_hostnames.split(" *, *"));
		
		// verify all the expected registry hostnames are configured
		List<String> expectedRegistryHostnames = new ArrayList<String>();
		expectedRegistryHostnames.add("registry.access.redhat.com");
		expectedRegistryHostnames.add("cdn.redhat.com");
		//if (clienttasks.isPackageVersion("subscription-manager-plugin-container", ">", "1.13.17-1"))
		expectedRegistryHostnames.add("access.redhat.com");	// added by bug 1184940
		for (String expectedRegistryHostname : expectedRegistryHostnames) {
			Assert.assertTrue(registryHostnames.contains(expectedRegistryHostname), "Container plugin file '"+containerContentPluginFile+"' configuration for registry_hostnames includes expected '"+expectedRegistryHostname+"'.  Actual='"+registry_hostnames+"'");
		}
		
		// verify the CA cert file is installed in all of the expected registry hostname directories.
		for (String expectedRegistryHostname : expectedRegistryHostnames) {
			verifyCaCertInEtcDockerCertsRegistryHostnameDir(expectedRegistryHostname);
		}
	}
	
	
	
	// RHEL7 Only ==============================================================================================
	
	@Test(	description="install the latest docker package on the host",
			groups={"AcceptanceTests"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void InstallDockerPackageOnHost_Test() {
		// assert that the host system is rhel7+
		if (Integer.valueOf(clienttasks.redhatReleaseX)<7) throw new SkipException("Installation of docker.rpm is only applicable on RHEL7+");
		if (!clienttasks.arch.equals("x86_64")) throw new SkipException("Installation of docker.rpm is only applicable on arch x86_64");
		
		// if provided in the script arguments, install the requested docker packages
		clienttasks.installSubscriptionManagerRPMs(sm_dockerRpmInstallUrls, null, sm_yumInstallOptions);
		
		// assert the docker version is >= 1.0.0-2
		Assert.assertTrue(clienttasks.isPackageVersion("docker", ">=", "1.0.0-2"), "Expecting docker version to be >= 1.0.0-2 (first RHSM compatible version of docker).");
		
		// restart the docker service
		//RemoteFileTasks.runCommandAndAssert(client,"service docker restart",Integer.valueOf(0),"^Starting docker: +\\[  OK  \\]$",null);
		RemoteFileTasks.runCommandAndAssert(client, "systemctl restart docker.service && systemctl is-active docker.service", Integer.valueOf(0), "^active$", null);
	}
	
	@Test(	description="verify the specified docker image downloads and will run subscription-manager >= 1.12.4-1",
			groups={"AcceptanceTests","blockedByBug-1186386"},
			dependsOnMethods={"InstallDockerPackageOnHost_Test"},
			dataProvider="getDockerImageData",
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void PullDockerImage_Test(Object bugzilla, String dockerImage) {
		// TODO: Once registry.access.redhat.com is protected by certificates, this test will require a valid entitlement for containerimages.
		// unregister the host
		clienttasks.unregister(null, null, null);
		
		// pull the docker image
		//	[root@jsefler-7 ~]# docker pull docker-registry.usersys.redhat.com/brew/rhel7:latest
		//	Pulling repository docker-registry.usersys.redhat.com/brew/rhel7
		//	81ed26a5d836: Download complete 
		// Bug 1186386 - Docker unable to pull from CDN due to CA failure
		//	[root@10-16-7-142 ~]# docker pull registry.access.redhat.com/rhel7:latest
		//	FATA[0000] Error: v1 ping attempt failed with error: Get https://registry.access.redhat.com/v1/_ping: x509: certificate signed by unknown authority. If this private registry supports only HTTP or HTTPS with an unknown CA certificate, please add `--insecure-registry registry.access.redhat.com` to the daemon's arguments. In the case of HTTPS, if you have access to the registry's CA certificate, no need for the flag; simply place the CA certificate at /etc/docker/certs.d/registry.access.redhat.com/ca.crt 
		//	[root@10-16-7-142 ~]# echo $?
		//	1
		//RemoteFileTasks.runCommandAndAssert(client, "docker pull "+dockerImage, 0,"Download complete",null);
		String dockerPullCommand = "docker pull "+dockerImage;
		SSHCommandResult dockerPullResult = client.runCommandAndWait(dockerPullCommand);
		Assert.assertEquals(dockerPullResult.getExitCode(), Integer.valueOf(0), "The exit code from an attempt to run '"+dockerPullCommand+"'");
		Assert.assertEquals(dockerPullResult.getStderr(), "", "Stderr from an attempt to run '"+dockerPullCommand+"'");
		
		
		//	[root@jsefler-7 ~]# docker images
		//	REPOSITORY                                      TAG                 IMAGE ID            CREATED             VIRTUAL SIZE
		//	docker-registry.usersys.redhat.com/brew/rhel7   latest              81ed26a5d836        9 days ago          147.1 MB
		//	[root@jsefler-7 ~]# docker rmi 81ed26a5d836
		//	Untagged: docker-registry.usersys.redhat.com/brew/rhel7:latest
		//	Deleted: 81ed26a5d8363e8d0d20c390fb18a5f6d0b5ad9bbc64f678e6ea6334afebbd1b
		
		// verify the image will run subscription-manager >= 1.12.4-1
		//	[root@jsefler-7 ~]# docker run --rm docker-registry.usersys.redhat.com/brew/rhel7:latest rpm -q subscription-manager
		//	subscription-manager-1.12.4-1.el7.x86_64
		SSHCommandResult versionResult = RemoteFileTasks.runCommandAndAssert(client, "docker run --rm "+dockerImage+" rpm -q subscription-manager", 0);
		String subscriptionManagerVersionInDockerImage = versionResult.getStdout().trim().replace("subscription-manager"+"-", "");
		Assert.assertTrue(clienttasks.isVersion(subscriptionManagerVersionInDockerImage, ">=", "1.12.4-1"), "Expecting the version of subscription-manager baked inside image '"+dockerImage+"' to be >= 1.12.4-1 (first docker compatible version of subscription-manager)");
	}
	
	@Test(	description="verify a running container has no yum repolist when the host has no entitlement",
			groups={"AcceptanceTests"},
			dependsOnMethods={"PullDockerImage_Test"},
			dataProvider="getDockerImageData",
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void VerifyYumRepolistIsEmptyOnRunningDockerImageWhenHostIsUnregistered_Test(Object bugzilla, String dockerImage) {
		// unregister the host
		clienttasks.unregister(null, null, null);
		
		// verify the host has no redhat.repo content
		List<YumRepo> yumReposOnHost = clienttasks.getCurrentlySubscribedYumRepos();
		Assert.assertTrue(yumReposOnHost.isEmpty(),"When the host is unregistered, its list of yum repos in '"+clienttasks.redhatRepoFile+"' is empty.");
		
		// verify yum repolist is empty for running docker image
		//	[root@jsefler-7 ~]# docker run --rm docker-registry.usersys.redhat.com/brew/rhel7:latest yum repolist
		//	Loaded plugins: product-id, subscription-manager
		//	repolist: 0
		SSHCommandResult yumRepolistResultOnRunningDockerImage = RemoteFileTasks.runCommandAndAssert(client, "docker run --rm "+dockerImage+" yum repolist", 0, "repolist: 0", null);
	}
	
	@Test(	description="verify a running container has yum repolist access to appropriate content from the host's entitlement",
			groups={"AcceptanceTests"},
			dependsOnMethods={"VerifyYumRepolistIsEmptyOnRunningDockerImageWhenHostIsUnregistered_Test"},
			dataProvider="getDockerImageData",
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void VerifyYumRepolistOnRunningDockerImageConsumedFromHostEntitlements_Test(Object bugzilla, String dockerImage) {
		// register the host and autosubscribe
		clienttasks.register(sm_clientUsername,sm_clientPassword,sm_clientOrg,null,null,null,null,true,null,null,(String)null,null,null, null, true, false, null, null, null);
		
		// get a list of the entitled yum repos on the host
//		List<YumRepo> yumReposOnHost = clienttasks.getCurrentlySubscribedYumRepos();
//		Assert.assertTrue(!yumReposOnHost.isEmpty(),"When the host has registered with autosubscribe, we expect to have been granted access to at least one yum repos in '"+clienttasks.redhatRepoFile+"'.");
		List<String> enabledYumReposOnHost = clienttasks.getYumRepolist("enabled");
		List<EntitlementCert> entitlementCertsOnHost = clienttasks.getCurrentEntitlementCerts();
		Assert.assertTrue(!entitlementCertsOnHost.isEmpty(),"When the host has registered with autosubscribe, we expect to have been granted at least one entitlement.");
		
		// determine what products are installed on the running docker image
		String productCertDir = clienttasks.getConfParameter("productcertdir").replaceFirst("/$", "");	// strip of trailing /
		SSHCommandResult lsResultOnRunningDockerImage = client.runCommandAndWait("docker run --rm "+dockerImage+" ls "+productCertDir);
		//	201407071248:40.755 - FINE: ssh root@jsefler-7.usersys.redhat.com docker run --rm docker-registry.usersys.redhat.com/brew/rhel7:latest ls /etc/pki/product
		//	201407071248:41.023 - FINE: Stdout: 69.pem
		List<ProductCert> productCertsOnRunningDockerImage = new ArrayList<ProductCert>();
		for (String productCertFileOnRunningDockerImage : lsResultOnRunningDockerImage.getStdout().trim().split("\n")) {
			productCertFileOnRunningDockerImage = productCertDir+"/"+productCertFileOnRunningDockerImage;
			SSHCommandResult rctCatCertResultOnRunningDockerImage = RemoteFileTasks.runCommandAndAssert(client, "docker run --rm "+dockerImage+" rct cat-cert "+productCertFileOnRunningDockerImage, 0);
			//	201407071250:40.755 - FINE: ssh root@jsefler-7.usersys.redhat.com docker run --rm docker-registry.usersys.redhat.com/brew/rhel7:latest rct cat-cert /etc/pki/product/69.pem
			//	201407071250:43.954 - FINE: Stdout: 
			//
			//	+-------------------------------------------+
			//		Product Certificate
			//	+-------------------------------------------+
			//
			//	Certificate:
			//		Path: /etc/pki/product/69.pem
			//		Version: 1.0
			//		Serial: 12750047592154746969
			//		Start Date: 2014-01-28 18:37:08+00:00
			//		End Date: 2034-01-23 18:37:08+00:00
			//
			//	Subject:
			//		CN: Red Hat Product ID [eb3b72ca-acb1-4092-9e67-f2915f6444f4]
			//
			//	Issuer:
			//		C: US
			//		CN: Red Hat Entitlement Product Authority
			//		O: Red Hat, Inc.
			//		OU: Red Hat Network
			//		ST: North Carolina
			//		emailAddress: ca-support@redhat.com
			//
			//	Product:
			//		ID: 69
			//		Name: Red Hat Enterprise Linux Server
			//		Version: 7.0
			//		Arch: x86_64
			//		Tags: rhel-7,rhel-7-server
			//		Brand Type: 
			//		Brand Name: 
			productCertsOnRunningDockerImage.add(ProductCert.parse(rctCatCertResultOnRunningDockerImage.getStdout()).get(0));
		}
		
		// get the product tags installed on the running docker image
		Set<String> providedTagsOnRunningDockerImage = new HashSet<String>();
		for (ProductCert productCertOnRunningDockerImage : productCertsOnRunningDockerImage) {
			if (productCertOnRunningDockerImage.productNamespace.providedTags!=null) {
				for (String providedTag : productCertOnRunningDockerImage.productNamespace.providedTags.split("\\s*,\\s*")) {
					providedTagsOnRunningDockerImage.add(providedTag);
				}
			}
		}
		
		// get the arch on the running docker image
		String archOnRunningDockerImage = RemoteFileTasks.runCommandAndAssert(client, "docker run --rm "+dockerImage+" uname --machine", 0).getStdout().trim();
		
		// get the yum repolist of enabled repos on the running docker image
		SSHCommandResult enabledYumRepolistResultOnRunningDockerImage = RemoteFileTasks.runCommandAndAssert(client, "docker run --rm "+dockerImage+" yum repolist enabled", 0, "repolist:", null);
		List<String> enabledYumReposOnRunningDockerImage = clienttasks.getYumRepolistFromSSHCommandResult(enabledYumRepolistResultOnRunningDockerImage);
		// assert that only the appropriate entitled content sets appear in the yum repolist on the running docker image
		for (EntitlementCert entitlementCertOnHost : entitlementCertsOnHost ) {
			for (ContentNamespace contentNamespaceOnHost : entitlementCertOnHost.contentNamespaces) {
				
				// get the content namespace requiredTags
				Set<String> contentNamespaceRequiredTags = new HashSet<String>();
				if (contentNamespaceOnHost.requiredTags!=null) {
					for (String requiredTag : contentNamespaceOnHost.requiredTags.split("\\s*,\\s*")) {
						if (requiredTag.isEmpty()) continue;
						contentNamespaceRequiredTags.add(requiredTag);
					}
				}
				// get the content namespace arches
				Set<String> contentNamespaceArches = new HashSet<String>();
				if (contentNamespaceOnHost.arches!=null) {
					for (String arch : contentNamespaceOnHost.arches.split("\\s*,\\s*")) {
						if (arch.isEmpty()) continue;
						contentNamespaceArches.add(arch);
					}
				}
				if (contentNamespaceArches.contains("x86")) {contentNamespaceArches.addAll(Arrays.asList("i386","i486","i586","i686"));}  // Note: x86 is a general arch to cover all 32-bit intel microprocessors 
				
				// when the content namespace is not of type "yum", it will not appear in either the yum repolist of the host or the running docker image
				if (!contentNamespaceOnHost.type.equals("yum")) {
					Assert.assertTrue(!enabledYumReposOnHost.contains(contentNamespaceOnHost.label), "Entitled content namespace '"+contentNamespaceOnHost.label+"' of type '"+contentNamespaceOnHost.type+"' should never appear on the yum repolist of the host.");
					Assert.assertTrue(!enabledYumReposOnRunningDockerImage.contains(contentNamespaceOnHost.label), "Entitled content namespace '"+contentNamespaceOnHost.label+"' of type '"+contentNamespaceOnHost.type+"' should never appear on the yum repolist of the running docker container.");
					continue;	// go to the next content namespace
				}
				
				// when the content namespace is not enabled, it will not appear in either the yum repolist of the host or the running docker image
				if (!contentNamespaceOnHost.enabled) {
					Assert.assertTrue(!enabledYumReposOnHost.contains(contentNamespaceOnHost.label), "Entitled content namespace '"+contentNamespaceOnHost.label+"' is disabled and should NOT appear on the yum repolist of the host because it is disabled by default.");
					Assert.assertTrue(!enabledYumReposOnRunningDockerImage.contains(contentNamespaceOnHost.label), "Entitled content namespace '"+contentNamespaceOnHost.label+"' is disabled and should NOT appear on the yum repolist of the running docker container because it is disabled by default.");
					continue;	// go to the next content namespace
				}
				
				// when the content namespace is enabled, it's appearance on the yum repolist of the running docker image depends on the installed product certs on the image.
				if ((contentNamespaceArches.isEmpty() || contentNamespaceArches.contains(archOnRunningDockerImage)) &&
					(contentNamespaceRequiredTags.isEmpty() || providedTagsOnRunningDockerImage.containsAll(contentNamespaceRequiredTags))) {
					Assert.assertTrue(enabledYumReposOnRunningDockerImage.contains(contentNamespaceOnHost.label), "Entitled content namespace '"+contentNamespaceOnHost.label+"' on the host should be enabled in the running docker container because both the docker container arch '"+archOnRunningDockerImage+"' is among the supported content set arches "+contentNamespaceArches+" and the docker container providedTags "+providedTagsOnRunningDockerImage+" provides all the content set required tags "+contentNamespaceRequiredTags+".");
				} else {
					Assert.assertTrue(!enabledYumReposOnRunningDockerImage.contains(contentNamespaceOnHost.label), "Entitled content namespace '"+contentNamespaceOnHost.label+"' on the host should NOT be enabled in the running docker container because either the docker container arch '"+archOnRunningDockerImage+"' is not among the supported content set arches "+contentNamespaceArches+" or the docker container providedTags "+providedTagsOnRunningDockerImage+" does not provide all the content set required tags "+contentNamespaceRequiredTags+".");
				}
			}
		}
		
		// let's test installing a simple package (zsh)
		boolean installedPackage = false;
		if (enabledYumReposOnRunningDockerImage.contains("rhel-6-server-rpms") ||
			enabledYumReposOnRunningDockerImage.contains("rhel-7-server-rpms")) {
			RemoteFileTasks.runCommandAndAssert(client, "docker run --rm "+dockerImage+" yum -y install zsh", 0, "Complete!", null);
			//	[root@jsefler-7 ~]# docker run --rm docker-registry.usersys.redhat.com/brew/rhel7:latest yum -y install zsh
			//	Loaded plugins: product-id, subscription-manager
			//	Resolving Dependencies
			//	--> Running transaction check
			//	---> Package zsh.x86_64 0:5.0.2-7.el7 will be installed
			//	--> Finished Dependency Resolution
			//
			//	Dependencies Resolved
			//
			//	================================================================================
			//	 Package    Arch          Version               Repository                 Size
			//	================================================================================
			//	Installing:
			//	 zsh        x86_64        5.0.2-7.el7           rhel-7-server-rpms        2.4 M
			//
			//	Transaction Summary
			//	================================================================================
			//	Install  1 Package
			//
			//	Total download size: 2.4 M
			//	Installed size: 5.6 M
			//	Downloading packages:
			//	warning: /var/cache/yum/x86_64/7Server/rhel-7-server-rpms/packages/zsh-5.0.2-7.el7.x86_64.rpm: Header V3 RSA/SHA256 Signature, key ID fd431d51: NOKEY
			//	Public key for zsh-5.0.2-7.el7.x86_64.rpm is not installed
			//	Retrieving key from file:///etc/pki/rpm-gpg/RPM-GPG-KEY-redhat-release
			//	Importing GPG key 0xFD431D51:
			//	 Userid     : "Red Hat, Inc. (release key 2) <security@redhat.com>"
			//	 Fingerprint: 567e 347a d004 4ade 55ba 8a5f 199e 2f91 fd43 1d51
			//	 Package    : redhat-release-server-7.0-1.el7.x86_64 (@koji-override-0/7.0)
			//	 From       : /etc/pki/rpm-gpg/RPM-GPG-KEY-redhat-release
			//	Importing GPG key 0x2FA658E0:
			//	 Userid     : "Red Hat, Inc. (auxiliary key) <security@redhat.com>"
			//	 Fingerprint: 43a6 e49c 4a38 f4be 9abf 2a53 4568 9c88 2fa6 58e0
			//	 Package    : redhat-release-server-7.0-1.el7.x86_64 (@koji-override-0/7.0)
			//	 From       : /etc/pki/rpm-gpg/RPM-GPG-KEY-redhat-release
			//	Running transaction check
			//	Running transaction test
			//	Transaction test succeeded
			//	Running transaction
			//	  Installing : zsh-5.0.2-7.el7.x86_64                                       1/1 
			//	  Verifying  : zsh-5.0.2-7.el7.x86_64                                       1/1 
			//
			//	Installed:
			//	  zsh.x86_64 0:5.0.2-7.el7                                                      
			//
			//	Complete!
			installedPackage = true;
		}
		if (!installedPackage) log.warning("Skipped attempts to install a package since the rhel-(6|7)-server-rpms repo was not entitled.");
	}
	
	
	@Test(	description="Verify that entitlements providing containerimage content are copied to relevant directoried when attached (as governed by the subscription-manager-plugin-container package)",
			groups={"AcceptanceTests"},
			enabled=true)
	//@ImplementsNitrateTest(caseId=)
	public void VerifyContainerConfigurationsAreSetAfterAutoSubscribingAndUnsubscribing_Test() {
		
		// get a list of the currently installed product Certs
		List<ProductCert> currentProductCerts = clienttasks.getCurrentProductCerts();
		
		// get the list of registry_hostnames from /etc/rhsm/pluginconf.d/container_content.ContainerContentPlugin.conf
		String registry_hostnames = clienttasks.getConfFileParameter(containerContentPluginFile.getPath(), "registry_hostnames");
		List<String> registryHostnames = Arrays.asList(registry_hostnames.split(" *, *"));
		// configure another registry_hostname for functional test purposes
		if (!registryHostnames.contains("rhsm-test.redhat.com")) clienttasks.updateConfFileParameter(containerContentPluginFile.getPath(), "registry_hostnames",registry_hostnames+","+"rhsm-test.redhat.com");	// rhsm-test.redhat.com does NOT appear to come from a redhat.com CDN
		if (!registryHostnames.contains("cdn.rhsm-test.redhat.com")) clienttasks.updateConfFileParameter(containerContentPluginFile.getPath(), "registry_hostnames",registry_hostnames+","+"cdn.rhsm-test.redhat.com");	// cdn.rhsm-test.redhat.com DOES appear to come from a redhat.com CDN because it matches regex ^cdn\.(?:.*\.)?redhat\.com$
		registry_hostnames = clienttasks.getConfFileParameter(containerContentPluginFile.getPath(), "registry_hostnames");
		registryHostnames = Arrays.asList(registry_hostnames.split(" *, *"));
		
		// register the host, autosubscribe, and get the granted entitlements
		clienttasks.register(sm_clientUsername,sm_clientPassword,sm_clientOrg,null,null,null,null,true,null,null,(String)null,null,null, null, true, false, null, null, null);
		List<EntitlementCert> entitlementCerts = clienttasks.getCurrentEntitlementCerts();
		
		// verify that the entitlements which provide containerimage content are copied to registry_hostnames...
		//	[root@jsefler-os7 ~]# ls /etc/docker/certs.d/registry.access.redhat.com/
		//	5109020365795659852.cert  5109020365795659852.key
		//	[root@jsefler-os7 ~]# ls /etc/docker/certs.d/cdn.redhat.com
		//	5109020365795659852.cert  5109020365795659852.key  redhat-uep.crt
		boolean foundContainerImageContent = false;
		for (EntitlementCert entitlementCert : entitlementCerts) {
			List<ContentNamespace> containerImageContentNamespaces = ContentNamespace.findAllInstancesWithCaseInsensitiveMatchingFieldFromList("type", "containerimage", entitlementCert.contentNamespaces);
			if (containerImageContentNamespaces.isEmpty()) {
				// assert that the entitlementCert was NOT copied to the directory of registry_hostnames because it does not contain content of type 'containerimage' (case insensitive).
				for (String registryHostname : registryHostnames) {
					File certFile = getRegistryHostnameCertFileFromEntitlementCert(registryHostname,entitlementCert);
					File keyFile = getRegistryHostnameCertKeyFileFromEntitlementCert(registryHostname,entitlementCert);
					Assert.assertTrue(!RemoteFileTasks.testExists(client, certFile.getPath()),"Entitlement cert '"+entitlementCert.file+"' '"+entitlementCert.orderNamespace.productName+"' was NOT copied to '"+certFile+"' because it does not contain content of type 'containerimage' (case insensitive).");
					Assert.assertTrue(!RemoteFileTasks.testExists(client, keyFile.getPath()),"Corresponding entitlement key '"+clienttasks.getEntitlementCertKeyFileFromEntitlementCert(entitlementCert)+"' was NOT copied to '"+keyFile+"' because it does not contain content of type 'containerimage' (case insensitive).");
				}
			} else {
				foundContainerImageContent = true;
				// assert that the entitlementCert was copied to the directory of registry_hostnames (but only if all of its required_tags are installed)
				for (String registryHostname : registryHostnames) {
					File certFile = getRegistryHostnameCertFileFromEntitlementCert(registryHostname,entitlementCert);
					File keyFile = getRegistryHostnameCertKeyFileFromEntitlementCert(registryHostname,entitlementCert);					
					
					// determine if this entitlement contains at least one container image with required tags that are provided by the installed product certs
					boolean entitlementContainsAtLeastOneContainerImageContentNamespaceWithRequiredTagsThatAreProvidedByInstalledProducts = false;
					for (ContentNamespace containerImageContentNamespace : containerImageContentNamespaces) {
						if (clienttasks.areAllRequiredTagsInContentNamespaceProvidedByProductCerts(containerImageContentNamespace, currentProductCerts)) {
							entitlementContainsAtLeastOneContainerImageContentNamespaceWithRequiredTagsThatAreProvidedByInstalledProducts = true;
							log.info("containerImageContentNamespace '"+containerImageContentNamespace.name+"' has requiredTags '"+containerImageContentNamespace.requiredTags+"' that ARE provided by the currently installed products.");
						} else {
							log.info("containerImageContentNamespace '"+containerImageContentNamespace.name+"' has requiredTags '"+containerImageContentNamespace.requiredTags+"' that are NOT provided by the currently installed products.");			
						}
					}
					if (entitlementContainsAtLeastOneContainerImageContentNamespaceWithRequiredTagsThatAreProvidedByInstalledProducts) {	
						Assert.assertTrue(RemoteFileTasks.testExists(client, certFile.getPath()),"Entitlement cert '"+entitlementCert.file+"' '"+entitlementCert.orderNamespace.productName+"' providing a 'containerimage' (case insensitive) was copied to '"+certFile+"' because at least one contentNamespace of type 'containerimage' from the entitlement has required_tags that are provided by the currently installed product certs.");
						Assert.assertTrue(RemoteFileTasks.testExists(client, keyFile.getPath()),"Corresponding entitlement key '"+clienttasks.getEntitlementCertKeyFileFromEntitlementCert(entitlementCert)+"' providing a 'containerimage' (case insensitive) was copied to '"+keyFile+"' because at least one contentNamespace of type 'containerimage' from the entitlement has required_tags that are provided by the currently installed product certs.");
						// also assert that the ca cert corresponding to registry hostname is copied to the directory as a ca.crt, but only if it appears to be a redhat.com CDN
						verifyCaCertInEtcDockerCertsRegistryHostnameDir(registryHostname);
					} else {
						Assert.assertTrue(!RemoteFileTasks.testExists(client, certFile.getPath()),"Entitlement cert '"+entitlementCert.file+"' '"+entitlementCert.orderNamespace.productName+"' providing a 'containerimage' (case insensitive) was NOT copied to '"+certFile+"' because no contentNamespace of type 'containerimage' from the entitlement has required_tags that are provided by the currently installed product certs.");
						Assert.assertTrue(!RemoteFileTasks.testExists(client, keyFile.getPath()),"Corresponding entitlement key '"+clienttasks.getEntitlementCertKeyFileFromEntitlementCert(entitlementCert)+"' providing a 'containerimage' (case insensitive) was NOT copied to '"+keyFile+"' because no contentNamespace of type 'containerimage' from the entitlement has required_tags that are provided by the currently installed product certs.");
					}
				}
			}
		}
		if (!foundContainerImageContent) throw new SkipException("None of the auto-attached subscriptions for this system provide content of type \"containerimage\".");
		
		// individually unsubscribe from entitlements and assert the entitlement bearing a containerimage is also removed from registry_hostnames
		for (EntitlementCert entitlementCert : entitlementCerts) {
			List<ContentNamespace> containerImageContentNamespaces = ContentNamespace.findAllInstancesWithCaseInsensitiveMatchingFieldFromList("type", "containerimage", entitlementCert.contentNamespaces);
			BigInteger serialNumber = clienttasks.getSerialNumberFromEntitlementCertFile(entitlementCert.file);
			clienttasks.unsubscribeFromSerialNumber(serialNumber);
			if (!containerImageContentNamespaces.isEmpty()) {
				// after unsubscribing, assert that the entitlementCert was removed from the directory of registry_hostnames
				for (String registryHostname : registryHostnames) {
					File certFile = getRegistryHostnameCertFileFromEntitlementCert(registryHostname,entitlementCert);
					File keyFile = getRegistryHostnameCertKeyFileFromEntitlementCert(registryHostname,entitlementCert);
					Assert.assertTrue(!RemoteFileTasks.testExists(client, certFile.getPath()),"Entitlement cert '"+entitlementCert.orderNamespace.productName+"' providing a 'containerimage' (case insensitive) was removed from '"+certFile.getPath()+"' after unsubscribing.");
					Assert.assertTrue(!RemoteFileTasks.testExists(client, keyFile.getPath()),"Corresponding entitlement key providing a 'containerimage' (case insensitive) was removed from '"+keyFile.getPath()+"' after unsubscribing.");
				}
			}
		}
	}
	protected final String etcDockerCertsDir = "/etc/docker/certs.d/";
	protected File getRegistryHostnameCertFileFromEntitlementCert(String registryHostname, EntitlementCert entitlementCert) {
		return (new File(etcDockerCertsDir+registryHostname+"/"+(entitlementCert.file.getName().split("\\.")[0])+".cert"));
	}
	protected File getRegistryHostnameCertKeyFileFromEntitlementCert(String registryHostname, EntitlementCert entitlementCert) {
		return (new File(etcDockerCertsDir+registryHostname+"/"+(entitlementCert.file.getName().split("\\.")[0])+".key"));
	}
	/**
	 * @param registryHostname
	 * @return File path to /etc/docker/certs.d/registryHostname/ca.crt
	 */
	protected File getRegistryHostnameCACert(String registryHostname) {
		/* Bug 1184940 - Subscription Manager Container Plugin Requires Config / CA Cert Update
		 * FailedQA thereby invalidating this solution
		return (new File("/etc/docker/certs.d/"+registryHostname+"/"+"redhat-uep.crt"));	// implemented by commit 6246a41fc1666eafa60b4d4341c8a50bde0df297
		*/
		
		// commit db16ad8abb4c2f2bf4e895384f1246293fc4cba4 from Bug 1186386 - Docker unable to pull from CDN due to CA failure
		// Only registry hostnames that appear to match a redhat.com CDN should get a redhat-entitlement-authority.pem
		//if (clienttasks.isPackageVersion("subscription-manager-plugin-container",">=","1.13.19-1")) {	// Bug 1186386 - Docker unable to pull from CDN due to CA failure	// commit db16ad8abb4c2f2bf4e895384f1246293fc4cba4
			if (registryHostname.matches("cdn\\.(?:.*\\.)?redhat\\.com")) {
				return (new File(etcDockerCertsDir+registryHostname+"/"+"redhat-entitlement-authority.crt"));	// implemented by commit db16ad8abb4c2f2bf4e895384f1246293fc4cba4
			}
		//}
		
		return null;	// the ca cert for this registry is unknown
	}
	
	
	/**
	 * For the given registryHostname (comes from registry_hostnames configured in /etc/rhsm/pluginconf.d/container_content.ContainerContentPlugin.conf),
	 * verify that the redhat-entitlement-authority.pem is copied to the directory ONLY when the registryHostname appears to be a redhat.com CDN.
	 * @param registryHostname
	 */
	protected void verifyCaCertInEtcDockerCertsRegistryHostnameDir(String registryHostname) {
		if (clienttasks.isPackageVersion("subscription-manager-plugin-container","<","1.13.19-1")) {	// Bug 1186386 - Docker unable to pull from CDN due to CA failure	// commit db16ad8abb4c2f2bf4e895384f1246293fc4cba4
			Assert.fail("This version of subscription-manager-plugin-container does not properly place a CA crt in /etc/docker/certs.d/<registry_hostname>/ca.crt");
		}
		
		// also assert that the ca cert corresponding to registry hostname is copied to the directory as a ca.crt, but only if it appears to be a redhat.com CDN
		File caCertFile = getRegistryHostnameCACert(registryHostname);
		if (caCertFile==null) { // assert that there is NO ca.crt located in /etc/docker/certs.d/<registryHostname>
			//	[root@jsefler-os7 ~]# ls /etc/docker/certs.d/registry.access.redhat.com/*.crt
			//	ls: cannot access /etc/docker/certs.d/registry.access.redhat.com/*.crt: No such file or directory
			//	[root@jsefler-os7 ~]# echo $?
			//	2
			String path = etcDockerCertsDir+registryHostname;
			SSHCommandResult result = client.runCommandAndWait("ls "+path+"/*.crt");
			Assert.assertEquals(result.getExitCode(), Integer.valueOf(2), "The exitCode above should indicate that there are no ca.crt files installed in '"+path+"'. ");
		} else {
			//	[root@jsefler-os7 ~]# ls /etc/docker/certs.d/cdn.redhat.com/*.crt
			//	/etc/docker/certs.d/cdn.redhat.com/redhat-entitlement-authority.crt
			//	[root@jsefler-os7 ~]# echo $?
			//	0
			File redhatEntitlementAuthorityPemFile = new File(clienttasks.caCertDir+"/redhat-entitlement-authority.pem");	// redhat-entitlement-authority.pem
			Assert.assertTrue(RemoteFileTasks.testExists(client, caCertFile.getPath()),"CA crt '"+redhatEntitlementAuthorityPemFile+"' was copied to '"+caCertFile+"' because registry hostname '"+registryHostname+"' appears to match a redhat.com CDN.");
			
			SSHCommandResult result = client.runCommandAndWait("cmp "+redhatEntitlementAuthorityPemFile.getPath()+" "+caCertFile.getPath());
			Assert.assertEquals(result.getExitCode(), Integer.valueOf(0), "ExitCode from comparing CA cert files byte by byte for equality.");
			Assert.assertEquals(result.getStderr(), "", "Stderr from comparing CA cert files byte by byte for equality.");
			Assert.assertEquals(result.getStdout(), "", "Stdout from comparing CA cert files byte by byte for equality.");
		}
	}
	
	
	
	
	
	
	
	// Candidates for an automated Test:
	
	
	
	// Configuration methods ***********************************************************************
	@BeforeClass(groups={"setup"})
	public void checkPackageVersionBeforeClass() {
		if (clienttasks!=null) {
			if (clienttasks.isPackageVersion("subscription-manager", "<", "1.12.2-1")) {
				throw new SkipException("Subscription Management compatibility with docker requires subscription-manager-1.12.2-1 or higher.");
			}
			
			// skip test class when subscription-manager-plugin-container is not installed
			String pkg = "subscription-manager-plugin-container";
			if (!clienttasks.isPackageInstalled(pkg)) {
				throw new SkipException("Subscription Management compatibility with docker requires package '"+pkg+"'.");
			}
		}
	}
	
	
	// Protected methods ***********************************************************************
	protected final String rhsmHostDir = "/etc/rhsm-host";
	protected final String entitlementHostDir = "/etc/pki/entitlement-host";
	protected final File containerContentPluginFile = new File("/etc/rhsm/pluginconf.d/container_content.ContainerContentPlugin.conf");
	
	
	
	// Data Providers ***********************************************************************
	@DataProvider(name="getSubscriptionManagementCommandData")
	public Object[][] getSubscriptionManagementCommandDataAs2dArray() {
		return TestNGUtils.convertListOfListsTo2dArray(getSubscriptionManagementCommandDataAsListOfLists());
	}
	protected List<List<Object>> getSubscriptionManagementCommandDataAsListOfLists() {
		List<List<Object>> ll = new ArrayList<List<Object>>(); if (!isSetupBeforeSuiteComplete) return ll;
		Set<String> commands = new HashSet<String>();
		
		for (List<Object> l: HelpTests.getExpectedCommandLineOptionsDataAsListOfLists()) { 
			//Object bugzilla, String helpCommand, Integer exitCode, String stdoutRegex, List<String> expectedOptions
			//BlockedByBzBug blockedByBzBug = (BlockedByBzBug) l.get(0);
			String helpCommand = (String) l.get(1);
			//Integer exitCode = (Integer) l.get(2);
			//String stdoutRegex = (String) l.get(3);
			//List<String> expectedHelpOptions = (List<String>) l.get(4);
			
			// only process the commands with modules for which --help is an option
			if (!helpCommand.contains("--help")) continue;
				
			// remove the --help option
			String command = helpCommand.replace("--help", "");
			
			// collapse white space and trim
			command = command.replaceAll(" +", " ").trim();
			
			// skip command "subscription-manager"
			if (command.equals(clienttasks.command)) continue;
			
			// skip command "rhsm-debug"
			if (command.equals("rhsm-debug")) continue;
			
			// skip command "rct"
			if (command.startsWith("rct")) continue;
			
			// skip command "rhsm-icon"
			if (command.startsWith("rhsm-icon")) continue;
			
			// skip command "usr/libexec/rhsmd"
			if (command.startsWith("/usr/libexec/rhsmd")) continue;
			
			// skip command "usr/libexec/rhsmcertd-worker"
			if (command.startsWith("/usr/libexec/rhsmcertd-worker")) continue;
			
			// skip duplicate commands
			if (commands.contains(command)) continue; else commands.add(command);
			
			Set<String> bugIds = new HashSet<String>();

			// Bug 1114132 - when in container mode, subscription-manager-gui (and some other tools) should also be disabled
			if (command.contains("subscription-manager-gui"))		bugIds.add("1114132");
			if (command.startsWith("rhn-migrate-classic-to-rhsm"))	bugIds.add("1114132");
			if (command.startsWith("rhsmcertd"))					bugIds.add("1114132");

			BlockedByBzBug blockedByBzBug = new BlockedByBzBug(bugIds.toArray(new String[]{}));

			ll.add(Arrays.asList(new Object[]{blockedByBzBug, command}));
		}
		
		return ll;
	}
	
	
	
	@DataProvider(name="getDockerImageData")
	public Object[][] getDockerImageDataAs2dArray() {
		return TestNGUtils.convertListOfListsTo2dArray(getDockerImageDataAsListOfLists());
	}
	protected List<List<Object>> getDockerImageDataAsListOfLists() {
		List<List<Object>> ll = new ArrayList<List<Object>>(); if (!isSetupBeforeSuiteComplete) return ll;

		// get the names of the docker images to be tested from an input parameter
		for (String dockerImage : sm_dockerImages) {
			ll.add(Arrays.asList(new Object[]{null, dockerImage}));
		}
		
		return ll;
	}
}


// STATUS
//On 12/22/2014 05:22 PM, John Sefler wrote:
//	> Devan,
//	> I spent some time today trying to catch up on docker/entitlement
//	> testing....
//	> I re-watched the Sprint 82 demo where you presented comma separated
//	> registry_hostnames=registry.access.redhat.com,cdn.redhat.com and that
//	> the entitlement certs would get copied into corresponding
//	> /etc/docker/certs.d/ subdirectories
//	>
//	> What I don't understand is what to expect when attempting to docker
//	> pull registry.access.redhat.com/rhel7:latest
//	>
//	> This pull seems to work regardless of subscription-manager
//	> entitlements.  I expected to be blocked in some way until I attached
//	> an atomic subscription, but I do not get blocked.  Sub-man seems to be
//	> ignored upon docker pull.  Don't know what to do here.
//
//	Nothing has changed on the appinfra side, the cert protection of docker
//	images has still not been deployed in prod, and we've still never seen
//	it work in stage. They're fighting with it a bit here and there, there
//	are problems with golang and ssl, still no idea what will happen with
//	this. So the docker stuff is just a best guess at what should be
//	required for this to work, but we still don't know.
//
//	>
//	> I'm also unsure how the fields from a containerimage content set are
//	> supposed to be used/tested.
//
//	At this point the only behavior that should trigger that I am aware of
//	is that the cert containing that content should get copied into the
//	relevant directories. After that the cert is presented for auth when
//	docker does it's thing (theoretically) and then something checks that
//	the content path is in the cert.
//
//	Sadly I don't know how much can be done with this just yet, and I'm
//	worried things might need to get redone. I'm also not sure how motivated
//	they are to get this deployed.
//
//	>
//	> [root@jsefler-os7 ~]# subscription-manager list --consumed
//	> No consumed subscription pools to list
//	> [root@jsefler-os7 ~]# docker pull registry.access.redhat.com/rhel7:latest
//	> Pulling repository registry.access.redhat.com/rhel7
//	> bef54b8f8a2f: Download complete
//	> Status: Image is up to date for registry.access.redhat.com/rhel7:latest
//	>
//	> Thoughts?  I'll ping you Tues morning.
//	> John
//	>
//	>
//	> PS.  I'm working with the latest packages...
//	>
//	> [root@jsefler-os7 ~]# rpm -q subscription-manager
//	> subscription-manager-plugin-container docker
//	> subscription-manager-1.13.12-1.el7.x86_64
//	> subscription-manager-plugin-container-1.13.12-1.el7.x86_64
//	> docker-1.4.1-4.el7.x86_64
//	>

