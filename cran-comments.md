## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.

* Examples with CPU (user + system) or elapsed time > 5s
              user system elapsed
  dagp_plot 125.59   5.31   62.75
  dag_greta  80.16   4.01   38.21
  dag_node   22.58   0.54   19.44
  
These examples are long because they showcase operations that use TensorFlow.  They cannot be shortened further without eliminating the demonstration of this important functionality.

## rhub::check_for_cran() fails

This installations fails with the following error:

> Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  namespace 'vctrs' 0.2.4 is being loaded, but >= 0.3.0 is required

The only package I use with a `vctrs` dependency is `dplyr`.  It requires `vctrs (>= 0.3.0)` (see here: <https://github.com/tidyverse/dplyr/blob/master/DESCRIPTION>).  However, `rhub::check_for_cran()` insists on installing `vctrs (= 0.2.4)` even if I try to override it.  Version 0.3.0 is available via CRAN.

## Possibly mis-spelled words in DESCRIPTION:
     DAGs (4:13, 12:72)
     TensorFlow (15:26)
     Workflows (3:39)
     greta (14:37, 14:75)
     workflows (11:44)
All of the above words are spelled correctly.  DAGs is an acronym for directed acyclig graphs.

