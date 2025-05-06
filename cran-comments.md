## Test environments

* local R installation: R 4.5.0
* macos-latest (on github actions): R-release
* windows-latest (on github actions): R-release
* ubuntu-latest (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/labelled/actions/workflows/R-CMD-check.yaml

0 errors √ | 0 warnings √ | 0 notes √

## revdepcheck results

We checked 48 reverse dependencies (44 from CRAN + 4 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* surveyPrev (NA)
  checking running R code from vignettes ...

### Failed to check

* surveyPrev (NA)
 
## recheck GitHub action

------- Check results summary ------
Check status summary:
                  WARNING NOTE OK
  Source packages       0    0  1
  Reverse depends       1    7 38

Check results summary:
labelled ... OK
rdepends_GGally ... OK
rdepends_REDCapDM ... OK
rdepends_REDCapTidieR ... OK
rdepends_Rrepest ... OK
rdepends_SEERaBomb ... WARNING
* checking whether package ‘SEERaBomb’ can be installed ... WARNING
* checking compiled code ... NOTE
rdepends_broom.helpers ... OK
rdepends_bulkreadr ... OK
rdepends_cesR ... NOTE
* checking package subdirectories ... NOTE
rdepends_codebook ... OK
rdepends_crosswalkr ... OK
rdepends_datadictionary ... OK
rdepends_dataset ... OK
rdepends_dbGaPCheckup ... NOTE
* checking Rd files ... NOTE
rdepends_dm ... OK
rdepends_ech ... OK
rdepends_forestmodel ... OK
rdepends_ggblanket ... OK
rdepends_ggeasy ... NOTE
* checking DESCRIPTION meta-information ... NOTE
rdepends_ggformula ... OK
rdepends_ggstats ... OK
rdepends_growthcleanr ... OK
rdepends_guideR ... OK
rdepends_jsmodule ... OK
rdepends_jstable ... OK
rdepends_modelsummary ... OK
rdepends_nettskjemar ... OK
rdepends_opalr ... OK
rdepends_plotor ... OK
rdepends_pollster ... OK
rdepends_questionr ... OK
rdepends_readabs ... OK
rdepends_readstata13 ... NOTE
* checking compiled code ... NOTE
rdepends_retroharmonize ... NOTE
* checking Rd files ... NOTE
rdepends_robotoolbox ... OK
rdepends_saros ... OK
rdepends_saros.base ... OK
rdepends_speedycode ... OK
rdepends_spicy ... OK
rdepends_stevemisc ... OK
rdepends_surveyPrev ... OK
rdepends_tableone ... OK
rdepends_tabxplor ... OK
rdepends_testdat ... OK
rdepends_tntpr ... NOTE
* checking DESCRIPTION meta-information ... NOTE
rdepends_xportr ... OK
rdepends_zmisc ... NOTE
* checking DESCRIPTION meta-information ... NOTE

------- Check for regressions ------
No changes between old and new version

cf. https://github.com/larmarange/labelled/actions/workflows/recheck.yml
