.datatable.aware = TRUE

# importFrom goes here to get into NAMESPACE when processed by roxygen

#' @importFrom graphics par plot matplot abline points lines
#' @importFrom grDevices dev.off pdf
#' @importFrom ggplot2 aes facet_grid facet_wrap labs scale_fill_gradientn scale_y_log10 theme xlab ylab
#' @importFrom ggplot2 geom_bar geom_boxplot geom_histogram geom_hline geom_line geom_point geom_ribbon geom_smooth geom_tile geom_violin geom_vline
#' @importFrom stats power rnorm median quantile
#' @importFrom utils tail
#' @importFrom data.table .N .SD := %between% %like% %between% .I 
NULL


# # UGLY hack to avoid notes about data.table or ggplot not recognizing bindings
# # http://stackoverflow.com/questions/9439256/
# # how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# Prevent R CMD check from complaining about the use of standard data.table variables and variables inside data.table calls
if (getRversion() >= "2.15.1"){
	utils::globalVariables(c("."))
    utils::globalVariables(c("E", "E_m", "OFFSET", "SLOPE1", "SLOPE2", "delta", "dist_to_sync_tag", "epo", "epofrac", "epo_roll", "eposync"))
    utils::globalVariables(c("focal_hydro_idx", "frac", "hx", "hy", "hydro_idx", "id", "idx", "idx med_delta", "med_delta"))
    utils::globalVariables(c("next_ping_too_late", "offset_idx", "offset_level", "ping2next", "ping_idx", "q10", "q50"))
    utils::globalVariables(c("q90", "serial", "slope1", "slope2", "ss", "sync_tag"))
    utils::globalVariables(c("sync_tag_idx", "tag", "toa_idx", "ts", "x", "y"))
}


#' @useDynLib yaps
.onUnload <- function (lib) {
  library.dynam.unload("yaps", lib)
}
#' @importFrom Rcpp sourceCpp
.onAttach <- function(lib, pkg) {
	new_version_on_github <- newPkgVersion()
	if(is.null(new_version_on_github)) { new_version_on_github <- FALSE}
    ver <- utils::packageVersion('yaps')
	out_msg <- paste0('Welcome to yaps (v', ver,')')
	if(new_version_on_github) {
		out_msg <- paste0(out_msg, '\n There seems to be a new version of yaps available on github - please consider updating using: \n devtools::install_github("baktoft/yaps")')
	}
	out_msg <- paste0(out_msg, "\n Please let us know if you experience any trouble using yaps. \n Run testYaps() to ensure basic functions (incl. TMB) is working.")
	
    packageStartupMessage(out_msg)
 }