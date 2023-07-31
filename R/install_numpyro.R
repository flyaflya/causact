#' Install Python dependencies for using numpyro with causact
#'
#' This is a helper function to install Python dependencies needed. This
#'   includes python (3.11), numpyro-cpu (0.12.1), and arviz(0.15.1). These Python modules will be installed into a
#'   conda environment, named "numPyroFromR".
#'
#' @param conda The path to a `conda` executable. Use `"auto"` to allow
#'   `reticulate` to automatically find an appropriate `conda` binary.
#' @param timeout maximum time in minutes until the installation for each
#'    installation component times out and exits. Default is 8 minutes per
#'    installation component.
#' @param ... Optional arguments, reserved for future expansion.
#'
#' @note This will automatically install Miniconda (a minimal version of the
#'  Anaconda scientific software management system), create a 'conda'
#'  environment for causact named 'numPyroFromR' with required python and python
#'  package versions, and forcibly switch over to using that conda environment.
#'
#'  If you don't want to use conda or the "numPyroFromR" conda environment, you
#'  can install these specific versions of numpyro (version 0.12.1), and
#'  arviz (version 0.15.1), and ensure that the python
#'  environment that is initialised in this R session has these versions
#'  installed. This is not always straightforward, so we recommend installing
#'  the python packages using `install_numpyro()` for most users.
#'
#' @name install_numpyro
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_numpyro()
#' }
#' @importFrom reticulate py_available
#' @importFrom reticulate install_tensorflow
#' @importFrom reticulate conda_create
#' @importFrom reticulate conda_install
#' @importFrom reticulate conda_binary
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_process_start
#' @importFrom cli cli_process_done
#' @importFrom cli cli_ul
#' @importFrom callr r_process_options
#' @importFrom callr r_process
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_ul
install_numpyro <- function(conda = "auto",
                               timeout = 8,
                               ...) {

  # set warning message length
  options(warning.length = 2000)

  # install miniconda if needed
  if (!have_conda()) {
    greta_install_miniconda(timeout)
  }

  if (!have_greta_conda_env()) {
    greta_create_conda_env(timeout)
  }

  greta_install_python_deps(timeout)

  cli_alert_success("Installation of {.pkg greta} dependencies is complete!")
  cli_ul("Restart R, then load {.pkg greta} with: {.code library(greta)}")
}

## utility functions
have_conda <- function() {
  conda_bin <- tryCatch(reticulate::conda_binary("auto"),
                        error = function(e) NULL
  )
  !is.null(conda_bin)
}

install_miniconda <- function(timeout) {

  stdout_file <- create_temp_file("out-miniconda")
  stderr_file <- create_temp_file("err-miniconda")

  callr_install_miniconda <- callr::r_process_options(
    func = function() {
      reticulate::install_miniconda()
    },
    stdout = stdout_file,
    stderr = stderr_file
  )

  # if this function doesn't fail, then this code here can be run?
  install_miniconda_process <- new_install_process(
    callr_process = callr_install_miniconda,
    timeout = timeout,
    stdout_file = stdout_file,
    stderr_file = stderr_file,
    cli_start_msg = "No {.pkg miniconda} detected, installing \\
                      {.pkg miniconda}, this may take a minute.",
    cli_end_msg = "{.pkg miniconda} installed!"
  )

  greta_stash$miniconda_notes <- install_miniconda_process$output_notes
  greta_stash$miniconda_error <- install_miniconda_process$output_error

  cli::cli_ul("To see full installation notes run:")
  cli::cli_ul("{.code greta_notes_install_miniconda_output()}")
  cli::cli_ul("To see any error messages, run:")
  cli::cli_ul("{.code greta_notes_install_miniconda_error()}")
}

## new function
new_install_process <- function(callr_process,
                                timeout,
                                stdout_file = NULL,
                                stderr_file = NULL,
                                cli_start_msg = NULL,
                                cli_end_msg = NULL){
  options(warning.length = 2000)
  cli::cli_process_start(cli_start_msg)
  # convert max timeout from milliseconds into minutes
  timeout_minutes <- timeout * 1000 * 60
  r_callr_process <- callr::r_process$new(callr_process)
  r_callr_process$wait(timeout = timeout_minutes)

  status <- r_callr_process$get_exit_status()
  output_notes <- read_char(stdout_file)
  no_output <- nchar(output_notes) == 0
  output_error <- read_char(stderr_file)

  if (is.null(output_error)) {
    output_error <- "No output detected in stderr"
  }

  if (is.null(status)) {
    cli::cli_process_failed()
    msg_timeout <- timeout_install_msg(timeout, output_error)
    stop(
      msg_timeout,
      call. = FALSE
    )
  } else if (no_output) {
    cli::cli_process_failed()
    msg_other <- other_install_fail_msg(output_error)
    stop(
      msg_other,
      call. = FALSE
    )
  }

  cli_process_done(msg_done = cli_end_msg)

  return(
    list(output_notes = output_notes,
         status = status,
         no_output = no_output,
         output_error = output_error)
  )

}
