# Submitter Guide On What To Expect 

The submission process to Bioconductor combines github actions and r-universe
build system to automate the process of review as much as possible.  Packages
are not guaranteed acceptance upon submission; besides passing R CMD build, R
CMD check, and BiocCheck, a package will undergo a formal review from a member
of the Bioconductor Review Team for Bioconductor appropriateness and adherence
to [Bioconductor standard policies and guidelines][1].

## Overview of Submission steps and automated actions

1. [Open an Issue][2] using the issue_template. Do NOT change the template.  Please
name the issue the name of the package and in the template replace the link with
a link to your github repository for the package.

2. Validation step. Opening an issue will trigger a validation action. If the
package fails validation, it will close the issue and comment what does not meet
current Bioconductor precheck requirements. You may open a new issue yourself or request
someone from the review team re-open the issue to repeat this validation
step after appropriate changes have been made to the package. The following are
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

If pre-check passes. A
![precheck-passed](https://img.shields.io/badge/precheck--passed-cfd3d7) label is
added to the issue and Bioconductor policies are added as a comment. The
submitter must understand and accept Bioconductor policies to continue. An
![awaiting policy
acceptance](https://img.shields.io/badge/awaiting_policy_acceptance-d93f0b)
label is also added to the issue

3. Policy Acceptance. The submitter must understand and accept Bioconductor
policies to continue the review process. This is achieved by the submitter
commenting exactly `/accept-policies`. This will trigger the next automatic
action. Once policies are accepted,
![policies-accepted](https://img.shields.io/badge/policies--accepted-0e8a16) and
![pre-review](https://img.shields.io/badge/pre--review-cfd3d7) labels are added
to the issue.

4. Clone and register the package in the new package submission process
r-universe. At this time, the original repository is cloned into an organization
that mimics standards of Bioconductor git repositories. It also adds the package
to a new submission r-universe to start receiving build reports. The system adds
instructions for linking and pushing to the new location. This will be required
to trigger future build reports on the system. It can take some time for new
builds to be picked up in the r-universe system; please be patient. If you do
not receive a build report within 24 hours please contact Bioconductor review
admins for assistance.

5. Build Reports and Reviewer Assignment.
Bioconductor will not assign a reviewer to a package until
ERRORs are resolved. In most cases, even if a reviewer is assigned, the package
should be free of ERROR and Warnings before a reviewer will do an in-depth
review. Any Notes, Warnings, or Errors that remain in the package reports should
be justified and a reviewer will consider if an exception should be made or
not.
Once a reviewer is assigned, a ![review in
progress](https://img.shields.io/badge/review_in_progress-1d76db) label is
added. New build reports will not register unless there is a valid version
bump; a valid version bump is advancing only the z of version x.y.z. Example:
0.99.0 was initial submission, a valid version bump is 0.99.1, 0.99.2...

6. Build Report Breakdown

```
✅ New build detected for LoriTestPkg5, version 0.99.5.
⚙️ Detailed run: https://github.com/r-universe/tempbioc/actions/runs/24356345288
📦 Bioconductor staging repository: https://github.com/tempbioc/LoriTestPkg5
🌐 R-universe package page: https://tempbioc.r-universe.dev/LoriTestPkg5#checktable

📊 R-universe check results for `LoriTestPkg5`

| Platform | R | Status | URL |
|----------|---|--------|------|
| source | 4.5.3 | ✅ OK | [run](https://github.com/r-universe/tempbioc/actions/runs/24356345288/job/71124219924) |
| bioc-checks | 4.5.3 | ℹ️ NOTE | [run](https://github.com/r-universe/tempbioc/actions/runs/24356345288/job/71125390494) |
| linux-devel-x86_64 | 4.6.0 | ✅ OK | [run](https://github.com/r-universe/tempbioc/actions/runs/24356345288/job/71125390503) |
| macos-release-arm64 | 4.6.0 | ✅ OK | [run](https://github.com/r-universe/tempbioc/actions/runs/24356345288/job/71125390585) |
| windows-release | 4.6.0 | ✅ OK | [run](https://github.com/r-universe/tempbioc/actions/runs/24356345288/job/71125390637) |
```






[1]: https://contributions.bioconductor.org/develop-overview.html
[2]: https://github.com/Bioconductor/BiocContributions/issues
