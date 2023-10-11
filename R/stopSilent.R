#' Helper function to stop script silent
#' @noRd
#' @export
stopSilent <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
