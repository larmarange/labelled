## Test environments
* local Windows R, R 4.0.2
* local CentOS 7 install, R 4.0.2
* ubuntu (on travis-ci)
* win-builder (devel and release)

## Note

All documentation links have been updated using roxygen2 markdown support and 
this PR https://github.com/r-lib/roxygen2/pull/1109 to link to files, not topicsvers

## revdepcheck results

We checked 13 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* questionr
  checking for code/documentation mismatches ... WARNING
  The mismatch is due to an import of a labelled function, 
  the documentation needed to be regenerated with the new version of labelled.
  It will be fixed automatically in next version.
  Developpers of questionr are aware of the changes.
