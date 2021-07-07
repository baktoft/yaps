#' Experimental! Prepare detections data.table from raw data - csv-files exported from vendor software
#' @param raw_dat Data file from vendor supplied software
#' @param type Type of the vendor file. Currently only 'vemco_vue' is supported.
#' @export
#' @return `data.table` containing detections extracted from manufacturer data file.
#' @examples \dontrun{
#' prepped_detections <- prepDetections("path-to-raw-data-file", type="vemco_vue")
#' }
prepDetections <- function(raw_dat, type){
  
  # gsub (and the previous version using strsplit) convert to character before
  #   doing their thing. The default number of millisecond digits for R to
  #   display is 0, so millisecond information is dropped when converted to
  #   character. Temporarily change this to keep milliseconds, then used on.exit
  #   to change the option back to what it was before when the function finishes
  op <- options(digits.secs = 3)
  on.exit(options(op), add = T)
  
	detections <- data.table::copy(raw_dat)
	
	if (type == "vemco_vue"){
	  
	  # Only parse datetime if needed
	  ##  trunc drops milliseconds but converts to POSIXlt in the process, so it needs
	  ##  to be converted back to POSIXct for use elsewhere.
	  
	  if(!inherits(detections$`Date and Time (UTC)`, 'POSIXt')){
	    
	    detections[, ts :=
	                 as.POSIXct(
	                   trunc(
	                     as.POSIXct(`Date and Time (UTC)`, tz = "UTC")
	                   )
	                 )]
	    
	  } else{
	    
	    detections[, ts :=
	                 as.POSIXct(
	                   trunc(`Date and Time (UTC)`)
	                 )]
	    
	  }
	  
	  detections[, ':='(tag = as.numeric(gsub('.*-', '', Transmitter)),
	                    epo = as.numeric(ts),
	                    frac = as.numeric(gsub('.*\\.', '',
	                                            `Date and Time (UTC)`)) / 1000,
	                    serial = as.numeric(gsub('.*-', '', Receiver)))]
	}
	
	detections[, .(ts, tag, epo, frac, serial)]
	
}
	
