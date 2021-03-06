# Setup description -------------------------------------------------------
#
# These functions are used to setup your files so that all options are in one
# location, and references via the function call.
#
# These functions can be accessed using either `library(projectname)` or
# `devtools::load_all()` (Ctrl-Shift-L in RStudio).

#' Set options for all documents and scripts.
#'
#' @export
#'
#' @examples
#'
#' set_options()
#'
set_options <- function() {
    # Set the options here for individual packages

    # For the document (knitr)
    knitr::opts_chunk$set(
        warning = FALSE, message = FALSE, collapse = TRUE, echo = FALSE,
        dev = c('png', 'jpeg'), fig.path = 'fig/'
    )
}

