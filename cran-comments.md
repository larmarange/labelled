## Test environments

* local R installation: R 4.4.1
* macos-latest (on github actions): R-release
* windows-latest (on github actions): R-release
* ubuntu-latest (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/labelled/actions/workflows/R-CMD-check.yaml

0 errors √ | 0 warnings √ | 0 notes √

## revdepcheck results

We checked 43 reverse dependencies (40 from CRAN + 3 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* dm (NA)


## recheck GitHub action

------- Check results summary ------
Check status summary:
                  WARNING NOTE OK
  Source packages       0    1  0
  Reverse depends       1   13 27

Check results summary:
labelled ... NOTE
* checking package dependencies ... NOTE
* checking R code for possible problems ... NOTE
rdepends_GGally ... OK
rdepends_REDCapDM ... NOTE
* checking data for non-ASCII characters ... NOTE
rdepends_REDCapTidieR ... OK
rdepends_Rrepest ... NOTE
* checking data for non-ASCII characters ... NOTE
rdepends_SEERaBomb ... WARNING
* checking whether package ‘SEERaBomb’ can be installed ... WARNING
rdepends_broom.helpers ... OK
rdepends_bulkreadr ... OK
rdepends_cesR ... NOTE
* checking package subdirectories ... NOTE
rdepends_codebook ... NOTE
* checking data for non-ASCII characters ... NOTE
rdepends_crosswalkr ... OK
rdepends_datadictionary ... OK
rdepends_dataset ... OK
rdepends_dbGaPCheckup ... NOTE
* checking Rd files ... NOTE
rdepends_dm ... OK
rdepends_ech ... NOTE
* checking data for non-ASCII characters ... NOTE
rdepends_forestmodel ... NOTE
* checking LazyData ... NOTE
rdepends_ggblanket ... OK
rdepends_ggeasy ... OK
rdepends_ggformula ... NOTE
* checking Rd cross-references ... NOTE
rdepends_ggstats ... OK
rdepends_growthcleanr ... NOTE
* checking installed package size ... NOTE
rdepends_jsmodule ... OK
rdepends_jstable ... OK
rdepends_modelsummary ... OK
rdepends_opalr ... OK
rdepends_plotor ... OK
rdepends_pollster ... OK
rdepends_questionr ... NOTE
* checking data for non-ASCII characters ... NOTE
rdepends_readabs ... OK
rdepends_retroharmonize ... NOTE
* checking installed package size ... NOTE
* checking Rd files ... NOTE
rdepends_robotoolbox ... NOTE
* checking data for non-ASCII characters ... NOTE
rdepends_saros ... OK
rdepends_saros.base ... OK
rdepends_speedycode ... OK
rdepends_stevemisc ... OK
rdepends_surveyPrev ... NOTE
* checking package dependencies ... NOTE
* checking installed package size ... NOTE
rdepends_tableone ... OK
rdepends_testdat ... OK
rdepends_tntpr ... OK
rdepends_xportr ... OK
rdepends_zmisc ... OK

------- Check for regressions ------
No changes between old and new version

cf. https://github.com/larmarange/labelled/actions/workflows/recheck.yml
