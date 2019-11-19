#' @useDynLib yaps, .registration = TRUE
#' @useDynLib yaps_sync, .registration = TRUE
.onUnload <- function (lib) {
  library.dynam.unload("yaps", lib)
}
#' @importFrom Rcpp sourceCpp

.onAttach <- function(lib, pkg) {
    ver <- utils::packageVersion('yaps')
    packageStartupMessage(paste0('Welcome to yaps, version: ', ver))
 }