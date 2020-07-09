# causact (development version)

# causact 0.3.2

## Minor improvements and fixes

* `dag_render()` displayed weird DAGs when users have `dplyr (>=v1.0)` installed.  Isolated problem to changes in `dplyr::group_indices()` and `dplyr::bind_cols` that caused a jumbling of node id's.
* Updated README.md to display images and give CRAN install instructions.

# causact 0.3.1

* Initial release for CRAN
* Biggest change from previous development versions is that `dag_greta()` now returns a dataframe as opposed to creating variables in your global environment.

