#' Experimental! Prepare detections data.table from raw data - csv-files exported from vendor software
#' @param raw_dat Data file from vendor supplied software
#' @param type Type of the vendor file. Currently only 'vemco_vue' is supported.
#' @export
#' @return `data.table` containing detections extracted from manufacturer data file.
#' @examples \dontrun{
#' prepped_detections <- prepDetections("path-to-raw-data-file", type="vemco_vue")
#' }
prepDetections <- function(raw_dat, type){
	detections <- data.table::data.table()
	if (type == "vemco_vue"){
	  detections[, ts := as.POSIXct(raw_dat$'Date and Time (UTC)', tz = "UTC")]
	  detections[, tag := as.numeric(gsub('.*-', '', raw_dat$Transmitter))]
	  detections[, epo := as.numeric(ts)]
	  detections[, frac := as.numeric(gsub('.*\\.', '',
	                                       raw_dat$"Date and Time (UTC)")) / 1000]
	  detections[, serial := as.numeric(gsub('.*-', '', raw_dat$Receiver))]
	}
	detections[]
	return(detections)
}
	
