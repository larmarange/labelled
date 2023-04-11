## Test environments

* local R installation: R 4.2.3
* macos-latest (on github actions): R-release
* windows-latest (on github actions): R-release
* ubuntu-latest (on github actions): R-devel, R-release, R-oldrel-1, R-oldrel-2, R-oldrel-3

cf. https://github.com/larmarange/labelled/actions/workflows/R-CMD-check.yaml

0 errors √ | 0 warnings √ | 0 notes √

## revdepcheck results

We checked 30 reverse dependencies (29 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* ggformula
  checking for code/documentation mismatches ... WARNING
