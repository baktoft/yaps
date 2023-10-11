#' Internal function. Extract vector of speed of sounds for each timestamp from supplied data.
#' Input data must be a data.table() containing columns ts (POSIXct timestamp) and ss (numerical speed of sound)
#' @inheritParams getInpSync
#' @noRd
#' @export
getSsVec <- function(inp_toa_list, ss_data){
	roll <- data.table::data.table(ts = as.POSIXct(inp_toa_list$epo_self_vec, origin="1970-01-01", tz="UTC"))
	data.table::setkey(ss_data, ts)
	data.table::setkey(roll, ts)
	ss_data_vec <- ss_data[roll, roll="nearest"]$ss
	return(ss_data_vec)
}
