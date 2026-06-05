# Reviewer Instructions and Navigations Guide

All members of the package review team will be able to moderate issues and
trigger action events.  Please be mindful, considerate, and cautious with this
power and responsibility. We recommend following Bioconductor/BiocContributions
to make sure you are notified when assigned <br>
<br>
All previous administration steps are now automated up to assigning the
reviewer. See [submitters
overview](https://github.com/Bioconductor/BiocContributions/blob/devel/docs/submitters.md#overview-of-submission-steps-and-automated-actions)
for more information on what occurs during the submission process or [Slide Deck
for Submitters](https://docs.google.com/presentation/d/1EK2wsDoRbtVGECdYC1GU5nGtYkN-h_7R-on-CSUC6CQ/edit?usp=sharing).<br>
Some pre-check that admins would formerly do will no longer occur. You
as a reviewer should use your best judgement if a package is appropriate for
Bioconductor and must enforce a package to adhere to Bioconductor standards and
guidelines. This includes interoperability with existing packages and pipelines,
novel concepts not already introduced, and executing live code in vignettes, man
pages, and tests. <br>

## Table of Contents

* [Useful Links](#useful-links)
* [Build Report Breakdown](#build-reports)
* [Reviewing](#reviewing)
* [Accept/Decline/Inactive](#close-accept-decline-inactive)
* [Reassignment/Leave of Absence](#reassignment-and-leave-of-absence)
* [Troubleshooting](#troubleshooting)
  - [No Report](#no-report)
* [FAQ](#faqs)

## Useful links:

- [Bioconductor standard policies and guidelines][1]
- [SlideDeck for Reviewers](https://docs.google.com/presentation/d/1P0MROLo4mhr0Cn6X97ZjrhC0AfZ09u3wAuBPFgrjvuQ/edit?usp=sharing)
- [reviewer template checklist](https://contributions.bioconductor.org/docs/package-review-checklist.md)

<br>


## Build Reports

We tell submitters packages must be free from Errors and Warnings. Any Errors,
Warnings or Notes left in the reports should be justified by the submitter. If
this is not provided please ask for it and then use your best judgement if an
excpetion should be made. If there are any concerns over granting an exception
please ask in the reviewer closed zulip channel or contact one of the package
review admins (currently Lori Kern: lori.shepherd@roswellpark.org). <br>

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
log of the run.All **Status** should be minimally `ℹ️ NOTE` and ideally `✅ OK`. Warnings and 
Errors should be fixed or justified in a comment to the reviewer. To see the reports, you can 
click on the URL. This will take you to the run overview for that platform. 
<br>To see the detailed reports from the r-universe logs:<br> 
+ "source" expand "Build source package and vignettes" section of the log<br>
+ "bioc-check expand "Run BiocCheck for packagename" <br>
+ all other platforms expand "R CMD check"
<br>
The labels on the issue will update based on the reports. If any platform results in that level of status.

+  ![Build OK](https://img.shields.io/badge/Build_OK-8aca2b)
+  ![Build Note](https://img.shields.io/badge/Build_Note-c2e0c6)
+  ![Build Warning](https://img.shields.io/badge/Build_Warning-FFB302)
+  ![Build Error](https://img.shields.io/badge/Build_Error-D60409)
+  ![Build Unknown](https://img.shields.io/badge/Build_Unknown-d4c5f9)

## Reviewing

Reviewers should adhere to the Bioconductor [Code of Conduct][2] and expect that submitters do the same. 
If there is ever an issue please reach out to the Core Team member currently admininstrating the Single
Package Builder Process.

It is recommended to review/clone the [BiocStaging](https://github.com/orgs/BiocStaging/repositories) version of
the package. This would be the standard git location. R-universe's built versions of the package will not
allow comparison of certain hidden or ignored files that even while ignored should not be present in 
publically distributed versions of the package. It also allows validation of things like Authors@R in the 
DESCRIPTION and evaluating the template and code chunk settings of vignettes,for example andamongst other things that 
would not be possible on a built version of the package.

Installing and testing the R-universe package directly through Bioconductor's R-universe is fine and encouraged.
But file evaluation should likely be done from BiocStaging.

Please remember to enforce Bioconductor policies. We encourage you to glance (and contribute to) the
recommended [checklist](https://contributions.bioconductor.org/docs/package-review-checklist.md)

## Close Accept Decline Inactive

Reviewers should NEVER close an issue manually. Please use the designated labels. 
Each label is associated with an action that will perform clean up, adjust labels, 
post a closing comment, and close the issue.  It may take several minutes but it 
should happen automatically.  The labels are:


+ ![package accepted](https://img.shields.io/badge/package_accepted-1d76db)
+ ![package declined](https://img.shields.io/badge/package_declined-b60205)
+ ![inactive review](https://img.shields.io/badge/inactive_review-b60205)

There is also a new process for declining a package. If the assigned reviewer adds
the package declined label, a secondary reviewer from a list of admins will automatically
be added to the issue for an independent look at the package. This ensures that a 
package being declined was verified by at least two separate reviewers. 

## Reopening Issues

Anyone from the package review team may now reopen issues. When reopening the package 
will be re-evaluated by the validation prechecks and the submitter may be asked to
verify Bioconductor policies again. You should not have to change any labels manually. <br>
There is an option in the validation precheck to add a  ![allow large files](https://img.shields.io/badge/allow--large--files-fbca04).
This allows exception for packages containing files over 5 MB. This exception should be used
sparingly and generally should not be allowed but available for certain circumstances.


## Reassignment and Leave of Absence

Anyone from the package reviewer team may reassign a reviewer.  This may be done manually
(hopefully after a discussion with the newly assigned reviewer) or to assign the next
available reviewer in the review queue, you may apply the label ![assign reviewer](https://img.shields.io/badge/assign--reviewer-fbca04).
<br>
If you are taking an extended leave and would temporarily like to be excluded from new assignments, 
please let a package reviewer admin know either on the closed #reviewer zulip
channel or directly (currently Lori Kern: lori.shepherd@roswellpark.org); they can still progamatically exclude through action variables.

## Troubleshooting

#### No Report
For every push to trigger a new build, it can take time to be picked up in the r-universe
system and then also for our system to pick up a completed run for the
package. We loosely say if you do not receive a build report within 24 hours
please contact Bioconductor review team by tagging
@Bioconductor/packagereviewers for assistance. Any reviewer may assist. See
[Slide
Deck](https://docs.google.com/presentation/d/1P0MROLo4mhr0Cn6X97ZjrhC0AfZ09u3wAuBPFgrjvuQ/edit?slide=id.g3e8ac41a748_1_0#slide=id.g3e8ac41a748_1_0)
for images of each of these steps.

* Start by ensuring there was a valid push to the
[Biocstaging](https://github.com/orgs/BiocStaging/repositories) location of the
package repository. It should show that there was a recent commit (with valid
version bump). 

* Next check if the [R-universe](https://biocstaging.r-universe.dev/builds) built
the latest version of the package. 

* If the R-universe has not updated for package versions you can trigger an update
by clicking on the refresh for the R-universe **NOT** the individual package
run. See slide deck for location. Once you hit the refresh, the R-universe
should pick up the changes and once the build completes the BiocContributions
should pick up the package run within 2 hours. 
  
* You can check the latest R-universe runs to check that there was a recent
completed run for the package at [R-universe
workflows](https://github.com/r-universe/biocstaging/actions/workflows/build.yml)
  
* If there was a recent run for the package that has not posted on the Issue, you
can retrigger the BiocContribution action to search for the run. Navigate to the
BiocContributions Actions tab and Select "Track Runiverse Builds". On the right
side, click on the button for "Run workflow". Enter the package name (no version
number, just name) and select "Run Workflow". This will trigger the action and
hopefully you should see a build report once the action completes. 

If these steps did not resolve the issue. Contact the Bioconductor core team for
assistance. 


## FAQ

See [FAQs](https://github.com/Bioconductor/BiocContributions/blob/devel/docs/FAQs.md)


[1]: https://contributions.bioconductor.org/develop-overview.html
[2]: https://bioconductor.org/about/code-of-conduct/
