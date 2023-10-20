#' Remove multipath detections from detection table
#' @export
removeMultipath <- function(dets, mp_threshold=0.5){
	dets[, epodiff := c(NA, diff(eposync)), by=h_idx]
	mps <- which(dets$epodiff < mp_threshold)
	if(length(mps) > 0){
		dets <- dets[-mps]
	}
	dets[, epodiff := NULL]
	dets[]
	return(dets)
}
