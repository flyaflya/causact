This is a minor release that removes any dependency on the greta package.  Unfortunately, greta's Python integration has an extremely challenging installation process for M1/M2 mac machines which is not even available from CRAN.  This release introduces dag_numpyro() as a drop-in replacement for dag_greta() which is both easier to install and faster running.  This resubmission to CRAN fixes title case issue in title.

## Test environments
* local R installation, R 4.3.1
* ubuntu 16.04 (on travis-ci), R 4.3.1
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## rhub::check_for_cran() 

Runs without error.
