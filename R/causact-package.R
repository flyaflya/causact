#' @keywords internal
"_PACKAGE"

#' @importFrom reticulate use_condaenv py_run_string py_capture_output
#'
#' @export

.onLoad <- function(libname, pkgname) {
  if (!check_r_causact_env()) {
    # Set the environment variable to FALSE if the 'r-causact' environment is not set up
    options(causact_env_setup = FALSE)
  } else {
    # Set the environment variable to TRUE if the 'r-causact' environment is set up
    options(causact_env_setup = TRUE)
    options("reticulate.engine.environment" = cacheEnv)
  }
}

.onAttach <- function(libname, pkgname) {
  # temporarily turn off the reticulate autoconfigure functionality
  if (!check_r_causact_env()) {
    packageStartupMessage("WARNING: The 'r-causact' Conda environment does not exist. To use the 'dag_numpyro()' function, you need to set up the 'r-causact' environment. Run install_causact_deps() when ready to set up the 'r-causact' environment.")
  } else {
    # If the environment is set up, switch to it
    tryCatch({
      # Attempt to load the required Python packages
      reticulate::use_condaenv("r-causact", required = TRUE)
      packageStartupMessage("Initializing python, numpyro, and other dependencies. This may take up to 15 seconds...")
      ac_flag <- Sys.getenv("RETICULATE_AUTOCONFIGURE")
      ## disable attempt to upgrade python packages
      on.exit(
        Sys.setenv(
          RETICULATE_AUTOCONFIGURE = ac_flag
        )
      )
      Sys.setenv(RETICULATE_AUTOCONFIGURE = FALSE)
      invisible(reticulate::py_config())
      ## this code is used to suppress GPU warning
      ## on further calls
      temp = reticulate::py_capture_output(reticulate::py_run_string(
        "import jax.numpy as jnp; jnp.arange(10)"
      ))
      packageStartupMessage("Initializing python, numpyro, and other dependencies. This may take up to 15 seconds...COMPLETED!")
      # Above code for if successful , continue
    }, error = function(e) {# Display an error message
      packageStartupMessage("An error occurred:", conditionMessage(e))
      packageStartupMessage(". To make causact's required connection to Python, restart R, then install causact's dependencies with causact::install_causact_deps().\n")
    })
  } # end else
} #end .onAttach function
