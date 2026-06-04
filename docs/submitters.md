# Submitter Guide On What To Expect 

The submission process to Bioconductor combines github actions and r-universe
build system to automate the process of review as much as possible.  Packages
are not guaranteed acceptance upon submission; besides passing R CMD build, R
CMD check, and BiocCheck, a package will undergo a formal review from a member
of the Bioconductor Review Team for Bioconductor appropriateness and adherence
to [Bioconductor standard policies and guidelines][1].

You can add a GitHub action on your personal GitHub repository to mimic the R-universe
build, check and BiocCheck before pushing to Bioconductor. 
See [Adding Bioconductor R-Universe GitHub Action](https://docs.r-universe.dev/bioconductor/#debugging-the-ci)

## Table of Contents
* [Overview](#overview-of-submission-steps-and-automated-actions)
* [Build Report Breakdown](#build-report-breakdown)
* [Package Submission Status](#package-submission-status)
* [FAQ](#faqs)


## Overview of Submission steps and automated actions

There is also a detailed [Slide
Deck](https://docs.google.com/presentation/d/1EK2wsDoRbtVGECdYC1GU5nGtYkN-h_7R-on-CSUC6CQ/edit?usp=sharing)
of the submission process with screenshots.

1. [Open an Issue][2] using the new_submission_template. <br>Do NOT change the template.  Please
name the issue the name of the package and in the template replace the link with
a link to your github repository for the package.

2. Address any validation issues. <br> Opening an issue will trigger a validation action. The following are
part of the Bioconductor precheck validation:

     - The link provided is a valid, public,  github url
     - DESCRIPTION file and vignettes directory exist
     - DESCRIPTION file checks including:
       
        + Package, Version, and biocViews fields present
		+ Package name matches repository name (case sensitive)
		+ Version number: Incoming packages must be  x.99.y
	    + No remotes. Bioconductor does not allow use of Remotes.

     - Large File Check: Files may not exceed 5MB
     - Git LFS Check: Bioconductor does not allow use of Git LFS
     - Duplicate Submission

	2a. Fails validation: it will close the issue and comment what does not meet current Bioconductor precheck requirements. You may 	open a new issue yourself or request someone from the review team re-open the issue to repeat this validation
	step after appropriate changes have been made to the package. 

  	2b. Successful Validation: A ![precheck-passed](https://img.shields.io/badge/precheck--passed-cfd3d7) label is
	added to the issue and Bioconductor policies are added as a comment. The
	submitter must understand and accept Bioconductor policies to continue. An
	![awaiting policy acceptance](https://img.shields.io/badge/awaiting_policy_acceptance-d93f0b)
	label is also added to the issue

3. Accept Bioconductor Policies.<br>
The submitter must understand and accept Bioconductor
policies to continue the review process. This is achieved by the submitter
commenting exactly `/accept-policies`. This will trigger the next automatic
action. Once policies are accepted,
![policies-accepted](https://img.shields.io/badge/policies--accepted-0e8a16) and
![pre-review](https://img.shields.io/badge/pre--review-cfd3d7) labels are added
to the issue.

4. Wait for the build Report<br>
At this time, the original repository is cloned into an organization
that mimics standards of Bioconductor git repositories. It also adds the package
to a new submission r-universe to start receiving build reports. The system adds
instructions for linking and pushing to the new location. This will be required
to trigger future build reports on the system. It can take some time for new
builds to be picked up in the r-universe system; please be patient. If you do
not receive a build report within 24 hours please contact Bioconductor review
admins by tagging @Bioconductor/packagereviewers for assistance.

5. Fix issues and justify any notes, warnings, or errors to be assigned a reviewer.<br>
Bioconductor will not assign a reviewer to a package until
ERRORs are resolved. In most cases, even if a reviewer is assigned, the package
should be free of ERROR and Warnings before a reviewer will do an in-depth
review. Any Notes, Warnings, or Errors that remain in the package reports should
be justified and a reviewer will consider if an exception should be made or
not. See below section on [Build Report Breakdown](#build-report-breakdown)<br>
Once a reviewer is assigned, a ![review in
progress](https://img.shields.io/badge/review_in_progress-1d76db) label is
added. New build reports will not register unless there is a valid version
bump that is pushed to the new Bioconductor location NOT your individual original github;
a valid version bump is advancing only the z of version x.y.z. Example:
0.99.0 was initial submission, a valid version bump is 0.99.1, 0.99.2...<br>
For every push to trigger a new build, it can take time to be picked up in the r-universe
system. If you do not receive a build report within 24 hours please contact Bioconductor review
admins by tagging @Bioconductor/packagereviewers for assistance. You should check first to
make sure the repository at BiocStaging reflects a pushed commit.

7. Work with reviewer to improve package for acceptance <br>
A review will take place typically within 3 weeks of a clean build and/or
justifications of remaining build report issues. Once the reviewer posts
any concerns or comments, the submitter should alter the package accordingly,
kick off a new build with a valid version bump, and respond point by point to
reviewers comments. This may involve several interations.

8. Package Acceptance, Package Decline, or Inactive.<br>
Once the assigned reviewer feels the review is complete or staled, they may assign the
decision on the package by adjusting the issue label to:

+ ![package accepted](https://img.shields.io/badge/package_accepted-1d76db)
+ ![package declined](https://img.shields.io/badge/package_declined-b60205)
+ ![inactive review](https://img.shields.io/badge/inactive_review-b60205)

See below section on [Package Submission Status](#package-submission-status) for details on what occurs with each of these actions.

## Build Report Breakdown
A Build Report will post and looks something like the following:
```
✅ New build detected for LoriPkgTest2, version 0.99.5.

⚙️ Detailed run: https://github.com/r-universe/tempbioc/actions/runs/24356343947

📦 Bioconductor staging repository: https://github.com/tempbioc/LoriPkgTest2

🌐 R-universe package page: https://tempbioc.r-universe.dev/LoriPkgTest2#checktable


📊 R-universe check results for LoriPkgTest2
```
| Platform | R | Status | URL |
|----------|---|--------|------|
| bioc-checks | 4.5.3 | ❌ ERROR | [run](https://github.com/r-universe/tempbioc/actions/runs/24356343947/job/71124597591) |
| linux-devel-x86_64 | 4.6.0 | ⚠️ WARNING | [run](https://github.com/r-universe/tempbioc/actions/runs/24356343947/job/71124597588) |
| macos-release-arm64 | 4.6.0 | ⚠️ WARNING | [run](https://github.com/r-universe/tempbioc/actions/runs/24356343947/job/71124597603) |
| source | 4.5.3 | ✅ OK | [run](https://github.com/r-universe/tempbioc/actions/runs/24356343947/job/71124216990) |
| windows-release | 4.6.0 | ⚠️ WARNING | [run](https://github.com/r-universe/tempbioc/actions/runs/24356343947/job/71124597648) |


Let us review what information each of these pieces of the report contain. 
```
✅ New build detected for LoriPkgTest2, version 0.99.5.
```
This shows the package name and version that the report was generated on.

```
⚙️ Detailed run: https://github.com/r-universe/tempbioc/actions/runs/24356343947
```
This shows the full r-universe install, build, and check logs of the package across all platforms. 
```
📦 Bioconductor staging repository: https://github.com/tempbioc/LoriPkgTest2
```
This is a link to the current source of the package building in r-universe. 
```
🌐 R-universe package page: https://tempbioc.r-universe.dev/LoriPkgTest2#checktable
```
This is the complete landing page in r-universe for the package. This includes
the built source or binaries for various platforms available for download and
testing locally. It also includes a table of results for all platforms r-universe builds on. 
While all platforms are encouraged to be clean, Bioconductor new submissions will be 
evaluted on the current associated R version for current Bioconductor devel. For this
reason the table provided in the comments is a filtered table of relevant platforms.
<br><br>
The table provides the **Platform**, the version of **R**, **Status** of the Run, and **URL** to detailed 
log of the run. All **Status** should be minimally `ℹ️ NOTE` and ideally `✅ OK`. Warnings and 
Errors should be fixed or justified in a comment to the reviewer. To see the reports, you can 
click on the URL. This will take you to the run overview for that platform. 
<br>To see the detailed reports from the r-universe logs:<br> 
+ "source" expand "Build source package and vignettes" section of the log<br>
+ "bioc-check" expand "Run BiocCheck for packagename" <br>
+ all other platforms expand "R CMD check"
<br>
The labels on the issue will update based on the reports. If any platform results in that level of status.

+  ![Build OK](https://img.shields.io/badge/Build_OK-8aca2b)
+  ![Build Note](https://img.shields.io/badge/Build_Note-c2e0c6)
+  ![Build Warning](https://img.shields.io/badge/Build_Warning-FFB302)
+  ![Build Error](https://img.shields.io/badge/Build_Error-D60409)
+  ![Build Unknown](https://img.shields.io/badge/Build_Unknown-d4c5f9)


## Package Submission Status 

There are three completion actions to a review:

+ ![package accepted](https://img.shields.io/badge/package_accepted-1d76db)
+ ![package declined](https://img.shields.io/badge/package_declined-b60205)
+ ![inactive review](https://img.shields.io/badge/inactive_review-b60205)

### Package Acceptance

When a package is accepted, the issue will be closed and the package will move to the live 
Bioconductor devel location. This includes:

	1. Cloning the github repository to the canonical Bioconductor location. Instructions are given in the issue comment to update remotes.
	2. Add the package to the official Bioconductor manifest for Bioconductor devel
	3. Create a BiocCredential account to manage ssh-keys for push access to the new location
	4. Remove the package from the new submission organization and r-universe registry
	5. Close the issue

### Package Decline

If the original reviewer deemed the package not appropriate for Bioconductor, they will add this label. 
At this time a secondary reviewer is assigned to look at the package to see if they concur. If this occurs, 
we encourage the submitter to make any additional comments or statements regarding the initial review. If
the second reviewer agrees the package should be declined, they will confirm the package decline triggering

	1. Remove the package from the new submission organization and r-universe registry
	2. Close the issue

### Inactive Review

We understand that package development is not always top priority for submitters. However, the review
process should proceed in a timely fashion.  If the review has staled, the reviewer will likely ping
the submitter for an update on progress and intention to continue the review. If the review stays idol
for an extended period of time, the reviewer may add the inactive review tag. This results in 

	1. Remove the package from the new submission organization and r-universe registry
	2. Close the issue

If the submitter wishes to reinstate the review process, they may comment on the issue for the review team
to re-open the issue.  The validation and policy acceptance will need to be re-completed. 

### Accidental Closure

If you accidently close the issue, you will need to request the issue be
re-opened by a member of the package review team. The validation checks will
rerun but you should be able to pick up the process where you left off.

## FAQs

- Related Packages/Circular Dependencies<br>
Related packages should now be submitted as separate issues. The r-universe will
be able to find the related packages on subsequent builds. We may manually
adjust assigned reviewers to have a common reviewer if it is appropriate to
review together. You may also provide a comment on the issue if you would like
to suggest a set of packages should be reviewed together by a single person

- No Commit Message <br>
There is no confirmation that a successful commit is made to the Bioconductor
version of package. You should check to see the expected commit on the package
repository at https://github.com/orgs/BiocStaging/repositories

- Delay to Post Report <br>
There is a delay from when a push happens to when it registers in R-universe.
There is also a delay from when a build/check run completes to when the report
is posted on the issue. It can be a hour or two. If you do not receive a build report
within 24 hours please contact Bioconductor review admins by tagging
@Bioconductor/packagereviewers for assistance.

## Conclusion

We thank you for your interest in Bioconductor. If you have any questions, concerns, or feedback regarding the 
submissions process we encourage you to reach out to the package review team
admins on the Bioconductor [zulip][3] #packages-submission channel. 


[1]: https://contributions.bioconductor.org/develop-overview.html
[2]: https://github.com/Bioconductor/BiocContributions/issues
[3]: https://community-bioc.zulipchat.com
