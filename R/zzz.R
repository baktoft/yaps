.datatable.aware = TRUE

# importFrom goes here to get into NAMESPACE when processed by roxygen

#' @importFrom graphics par plot matplot abline points lines
#' @importFrom grDevices dev.off pdf
#' @importFrom ggplot2 aes coord_fixed facet_grid facet_wrap ggtitle labs scale_fill_gradientn scale_y_log10 theme xlab ylab  ylim xlim
#' @importFrom ggplot2 geom_bar geom_boxplot geom_histogram geom_hline geom_line geom_point geom_ribbon geom_smooth geom_tile geom_violin geom_vline
#' @importFrom stats power rnorm median quantile
#' @importFrom utils tail
#' @importFrom data.table .N .SD := %between% %like% %between% .I 


# # UGLY hack to avoid notes about data.table or ggplot not recognizing bindings
# # http://stackoverflow.com/questions/9439256/
# # how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# Prevent R CMD check from complaining about the use of standard data.table variables and variables inside data.table calls
if (getRversion() >= "2.15.1"){
	utils::globalVariables(c("."))
    utils::globalVariables(c("bi", "coord_fixed", "cum_bi", "delta", "delta_eposync", "delta_ping", "dist_to_sync_tag"))
	utils::globalVariables(c("E", "E_m", "epo", "epofrac", "epofrac_lin_corr", "epo_roll", "eposync"))
    utils::globalVariables(c("focal_hydro_idx", "frac", "h_idx", "h1", "h2", "h1_idx", "h2_idx", "hx", "hy", "hydro_idx", "id", "idx", "idx med_delta", "in_seq", "lin_corr_coeffs_offset", "lin_corr_coeffs_slope", "med_delta", "mean_E_m"))
    utils::globalVariables(c("N", "N.x", "N.y", "next_ping_too_late", "OFFSET", "offset_idx", "offset_level", "ping", "ping2next", "ping_idx", "q10", "q50"))
    utils::globalVariables(c("q90", "roll_eposync", "roll_seq_epo"))
    utils::globalVariables(c("seq_epo", "seq_lng", "seq_ping_idx", "serial", "slope1","SLOPE1", "SLOPE2", "slope2", "ss", "ss_idx", "sync_tag", "sync_tag_idx"))
    utils::globalVariables(c("t_delta", "t1", "t2", "tag", "toa_idx", "top", "ts", "x", "x_synced", "y", "y_synced"))
    utils::globalVariables(c("value"))
}

 
#' @useDynLib yaps
#' @importFrom Rcpp sourceCpp

.onUnload <- function (lib) {
  library.dynam.unload("yaps", lib)
}

.onAttach <- function(lib, pkg) {
	new_version_on_github <- newPkgVersion()
	if(is.null(new_version_on_github)) { new_version_on_github <- FALSE}
    ver <- utils::packageVersion('yaps')
	out_msg <- paste0('Welcome to yaps (v', ver,')\n NOTE: THIS IS THE ROAMING HYDROS VERSION\n NOTE: THIS IS THE ROAMING HYDROS VERSION\n NOTE: THIS IS THE ROAMING HYDROS VERSION')
	if(new_version_on_github) {
		out_msg <- paste0(out_msg, '\n There seems to be a new version of yaps available on github - please consider updating using: \n devtools::install_github("baktoft/yaps")')
	}
	out_msg <- paste0(out_msg, "\n Please let us know if you experience any trouble using yaps. \n Run testYaps() to ensure basic functions (incl. TMB) is working.")
	
    packageStartupMessage(out_msg)
 }


NULL