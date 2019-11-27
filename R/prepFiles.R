#' Prepare detections data.table from raw data - csv-files exported from vendor software
#' @param raw_dat Data file from vendor supplied software
#' @param type Type of the vendor file. Currently only 'vemco_vue' is supported.
#' @export
prepDetections <- function(raw_dat, type){
	detections <- data.table::data.table()
	if (type == "vemco_vue"){
		detections[, ts:=as.POSIXct(raw_dat$'Date and Time (UTC)', tz="UTC")]
		detections[, tag:=as.numeric(sapply(raw_dat$Transmitter, function(x) strsplit(x, "-")[[1]][3]))]
		detections[, epo:=as.numeric(ts)]
		detections[, frac:=as.numeric(sapply(raw_dat$'Date and Time (UTC)', function(x) strsplit(x, "\\.")[[1]][2]))]
		detections[, serial:=as.numeric(sapply(raw_dat$Receiver, function(x) strsplit(x, "-")[[1]][2]))]
	}
	detections[]
	return(detections)
}
	
