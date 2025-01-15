This is a patch release to fix a bug that is in our numpyro dependency.  `install_causact_deps` now installs the more recent `numpyro==0.16.1` and its associated files.

## Test environments
* local R installation, R 4.4.2
* ubuntu 16.04 (on travis-ci), R 4.4.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## rhub::check_for_cran() 

Runs without error.
