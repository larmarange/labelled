## Test environments

* local R installation: R 4.5.1
* macos-latest (on github actions): R-release
* windows-latest (on github actions): R-release
* ubuntu-latest (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/labelled/actions/workflows/R-CMD-check.yaml

0 errors √ | 0 warnings √ | 0 notes √

## revdepcheck results

We checked 53 reverse dependencies (51 from CRAN + 2 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* summarySCI (NA)
 
## recheck GitHub action

------- Check results summary ------
Check status summary:
                  ERROR WARNING NOTE OK
  Source packages     0       0    0  1
  Reverse depends     1       1    4 42

------- Check for regressions ------
No changes between old and new version

cf. https://github.com/larmarange/labelled/actions/workflows/recheck.yml
