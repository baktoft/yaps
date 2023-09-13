#' Checks input data for sync models...
#' @export
checkInpSyncData <- function(hydros, dat_sync, dat_ss, sync_params){
	# # # data checks...
	Es <- 0
	Ws <- 0
	
	if(is.null(attr(hydros, 'yapsified'))){
		Es <- Es + 1
		stop("ERROR: The hydros data.table is not yapsified. Run e.g. hydros <- yapsifyHydros(hydros) to do so. \n")
	}

	if(!is.null(attr(hydros, 'yapsified')) & attr(hydros, 'yapsified') == FALSE){
		Es <- Es + 1
		stop("ERROR: The hydros data.table is not yapsified. Run e.g. hydros <- yapsifyHydros(hydros) to do so. \n")
	}
	
	if(!"epo" %in% colnames(dat_sync)){
		cat("Note: Adding column epo to dat_sync \n")
		dat_sync[, epo := as.numeric(ts)]
	}
	
	if(!"epofrac" %in% colnames(dat_sync)){
		cat("Note: Adding column epofrac to dat_sync \n")
		dat_sync[, epofrac := epo+frac]
	}
	
	
	if(length(unique(hydros$h_sn)) != nrow(hydros)){
		Es <- Es+1
		stop("ERROR: At least one hydrophone serial number is used more than once in sync_dat$hydros!\n")
	}
	
	if(sync_params$keep_rate <=0 | (sync_params$keep_rate > 1 & sync_params$keep_rate < 10) | (sync_params$keep_rate >= 10 & sync_params$keep_rate %% 1 != 0)){
		Es <- Es+1
		stop("ERROR: Invalid sync_params$keep_rate! Must be either ]0;1] or integer >= 10\n")
	}
	
	if(sum(!hydros$h_sn %in% unique(dat_sync$h_sn)) != 0){
		det_hydros <- unique(dat_sync$h_sn)
		not_dets <- hydros$h_sn[!hydros$h_sn %in% det_hydros]
		cat(paste0("WARNING: These hydro serials were not found in the detection tabel. They cannot be synced. Please fix or remove. \n",paste(not_dets, collapse=", "),"\n"))
		Ws <- Ws + 1
	}
	
	if(sum(!unique(dat_sync$h_sn) %in% hydros$h_sn) != 0){
		det_hydros <- unique(dat_sync$h_sn)
		not_dets <- det_hydros[!hydros$h_sn %in% det_hydros]
		cat(paste0("WARNING: These hydro serials are present in the detection tabel, but not in hydros. Please fix or remove. \n",paste(not_dets, collapse=", "),"\n"))
		Ws <- Ws + 1
	}
	
	if(Es == 0 & Ws == 0){
		# cat("No errors or warnings found in InpSyncData\n")
	}
}