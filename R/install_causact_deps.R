#' Install causact's python dependencies like numpyro, arviz, and xarray.
#'
#' `install_causact_deps()` installs python, the numpyro and arviz packages, and their
#' direct dependencies.
#'
#' @details You may be prompted to download and install miniconda if reticulate
#'   did not find a non-system installation of python. Miniconda is the
#'   only supported installation method for users, as it ensures that the R
#'   python installation is isolated from other python installations. All python
#'   packages will by default be installed into a self-contained conda or venv
#'   environment named "r-causact". Note that "conda" is the only supported method for install.
#'
#'   If you initially declined the miniconda installation prompt, you can later
#'   manually install miniconda by running [`reticulate::install_miniconda()`].
#'
#'   If you manually configure a python environment with the required
#'   dependencies, you can tell R to use it by pointing reticulate at it,
#'   commonly by setting an environment variable:
#'
#'   ``` R
#'   Sys.setenv("RETICULATE_PYTHON" = "~/path/to/python-env/bin/python")
#'   ```
#' @import reticulate
#' @importFrom rstudioapi hasFun restartSession
#'
#' @export
install_causact_deps <-
  function() {

    ## parameters that might become function
    ## arguments in future releases
    ## current assumption is that causact
    ## sets up and uses its own python environment
    ## only conda install are supported right now
    envname = "r-causact"
    ## lock in package versions that are
    ## guaranteed to work together with python 3.11
    packages = c("numpyro[cpu]==0.13.2",
                 "arviz==0.16.1",
                 "pandas==2.1.3")
    python_version = "3.11"
    pip = TRUE
    new_env = TRUE
    method = "conda"
    conda = "auto"
    restart_session = TRUE

    ## below code for future support of virtualenv
    #method = c("auto", "virtualenv", "conda")
    #method <- match.arg(method)

    # verify 64-bit
    if (.Machine$sizeof.pointer != 8) {
      stop("Unable to install numpyro on this platform.",
           "Binary installation is only available for 64-bit platforms.",
           "Alternative is to install and use causact on Posit Cloud.")
    }

    # some special handling for windows
    if (is_windows()) {

      # avoid DLL in use errors
      if (py_available()) {
        stop("You should call install_causact_deps() only in a fresh ",
             "R session that has not yet initialized python (this is ",
             "to avoid DLL in use errors during installation).",
             "If in RSTUDIO, use menu: SESSION -> RESTART R.")
      }
    }

    if (isTRUE(new_env)) {

      if (method %in% c("auto", "virtualenv") &&
          reticulate::virtualenv_exists(envname))
        reticulate::virtualenv_remove(envname = envname, confirm = FALSE)

      if (method %in% c("auto", "conda")) {
        if (!is.null(tryCatch(conda_python(envname, conda = conda),
                              error = function(e) NULL)))
          reticulate::conda_remove(envname, conda = conda)
      }

    }

    message("Installing python, numpyro, and other dependencies. This may take a few minutes.  If requested to install miniconda, please answer 'Y' or yes to the prompts.")

    tryCatch({
      reticulate::conda_binary()
    }, error = function(e) {
      reticulate::install_miniconda()
    })

    reticulate::py_install(
      packages       = packages,
      envname        = envname,
      method         = method,
      conda          = conda,
      python_version = python_version,
      pip            = TRUE
    )

    cat("\nInstallation complete.\n\n")

    if (restart_session &&
        requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::hasFun("restartSession")) {
      rstudioapi::restartSession()
      } else {
        message("Please restart your R session.  If in RStudio, use the menu: SESSION -> RESTART R.")
      }

    invisible(NULL)
  }

# utility function for install
is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}
## The above installation script borrowed heavily from the script used to install the tensorflow package in R.
