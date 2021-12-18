#' Experimental! Prepare detections data.table from raw data - csv-files exported from vendor software
#' @param raw_dat Data file from vendor supplied software
#' @param type Type of the vendor file. Currently only 'vemco_vue' is supported.
#' @export
#' @return `data.table` containing detections extracted from manufacturer data file.
#' @examples \dontrun{
#' prepped_detections <- prepDetections("path-to-raw-data-file", type="vemco_vue")
#' }
prepDetections <- function(raw_dat, type){
  
  detections <- data.table::copy(raw_dat)
  
  if (type == "vemco_vue"){
    
    # Only parse datetime if needed
    if(!inherits(detections$`Date and Time (UTC)`, 'POSIXt')){
      detections[, ts := as.POSIXct(`Date and Time (UTC)`,
                                    format = '%Y-%m-%d %H:%M:%OS',
                                    tz = 'UTC')]
    } else{
      detections[, ts := `Date and Time (UTC)`]
    }
    
    
    detections[, ':='(tag = as.numeric(gsub('.*-', '', Transmitter)),
                      serial = as.numeric(gsub('.*-', '', Receiver)),
                      epo = as.numeric(ts))]
    detections[, frac := round(epo - floor(epo), 3)]
    detections[, epo := floor(epo)]
    detections[, ts := as.POSIXct(epo, 
                                  origin = '1970-01-01',
                                  tz = 'UTC')]
    
  }
  
  detections[, .(ts, tag, epo, frac, serial)]
	
}
	
