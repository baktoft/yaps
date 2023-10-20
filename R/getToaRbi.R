#' Get TOA matrix for random burst interval transmitters
#' @export
getToaRbi <- function(dets, hydros, rbi_min, rbi_max){
	
	data.table::setorder(dets, eposync)
	
	dets <- merge(dets, hydros[, .(h_sn, h_idx)], all.x=TRUE)
	
	# # # TODO
	# # # Consider to add a better function to remove too short BIs...
	# # # ...current version is blind to e.g. nobs
	
	
	# remove multipath...
	dets <- removeMultipath(dets, mp_threshold=0.5)
	
	# build seq to catch pings
	seq_factor <- ifelse(rbi_min < 10, 0.1, 1.0)
	seq <- matrix(seq(from=floor(min(dets$eposync)), to=ceiling(max(dets$eposync)), by=seq_factor))
	nobs <- apply(seq, 1, function(k) nrow(dets[eposync %between% c(k-.5*seq_factor, k+.5*seq_factor)]))

	# which times along seq have pings
	hits <- data.table(epo = seq[nobs > 0], nobs=nobs[nobs > 0])
	
	# identify lines representing new pings
	# threshold for new ping is dependent on rbi_min. For very fast transmitters, we need a lower threshold
	# threshold could be based on some metric of max possible distance in area...?
	new_ping_thres <- ifelse(rbi_min >= 5, 1, .5) 
	if(rbi_min >= 2){
		hits[, new_ping := c(1, ifelse(diff(epo) > new_ping_thres, 1, 0))]
	} else {
		hits[, new_ping := c(1, ifelse(diff(epo) > new_ping_thres, 1, 0))]
	}

	hits[, ping := cumsum(new_ping)]

	# identify median time for each ping
	ping_times <- hits[, .(t_ping = median(epo), nobs=sum(nobs)), by=ping]

	# prep to roll hits and dets
	ping_times[, roll_ping_times := t_ping]
	dets[, roll_dat := eposync]
	setkey(dets, roll_dat)
	setkey(ping_times, roll_ping_times)

	dets[, ping := ping_times[dets, roll="nearest"]	[, ping]	]
	dets[, roll_dat := NULL]
	
	# pings only detected by one hydro are useless for ping_type = rbi
	# excluded as they might as well be noise
	dets <- dets[!ping %in% dets[, .N, by=ping][N <= 1, ping]]

	# take the first if more than one detection from one hydro per ping has survived so far...
	dets <- dets[, .(eposync=min(eposync)), by=.(h_idx, ping)]
	
	# ...re-adjust ping number to start from 1 and be continous
	dets[, new_ping := ping != shift(ping, type="lag")]
	dets[1, new_ping := TRUE]
	dets[, ping := cumsum(new_ping)]
	dets[, new_ping := NULL]
	

	# identify and remove pings with too short BIs
	ping_times2 <- dets[, .(ping_time=median(eposync), .N), by=ping]
	ping_times2[, ping_diff := c(diff(ping_time), NA)]
	if(rbi_min > 5){
		ping_times2[, next_ping_too_soon := ping_diff < rbi_min-1]
	} else {
		ping_times2[, next_ping_too_soon := ping_diff < rbi_min-.5]
	}
	
	dets <- dets[ping %in% ping_times2[next_ping_too_soon == FALSE, ping]]

	# ...re-adjust ping number to start from 1 and be continous
	dets[, new_ping := ping != shift(ping, type="lag")]
	dets[1, new_ping := TRUE]
	dets[, ping := cumsum(new_ping)]
	dets[, new_ping := NULL]

	
	# build toa matrix
	dets[, h_idx_factor := factor(h_idx, levels=(1:nrow(hydros)))]
	toa <- as.matrix(reshape2::acast(dets, ping~h_idx_factor, value.var="eposync", drop=FALSE))
	dets[, h_idx_factor := NULL]
	

	# remake toa-matrix to include pings missed by all hydros...
	pings <- dets[, .(top=median(eposync)), by=ping]
	pings[, diff := c(diff(top), NA)]
	pings[, ping2next := 1]
	
	if(rbi_max > 10){
		pings[, next_ping_too_late := diff > rbi_max+1] 
	} else {
		pings[, next_ping_too_late := diff > rbi_max+.5]
	}
	
	pings[next_ping_too_late==TRUE, ping2next:=ceiling(diff/(rbi_max-1))] 

	pings[, ping_idx:=cumsum(c(1,ping2next[-.N]))]

	toa_all <- matrix(ncol=ncol(toa), nrow=max(pings$ping_idx))
	toa_all[pings$ping_idx, ] <- toa
	
	toa <- toa_all
	
	diff(rowMeans(toa, na.rm=TRUE))
	
	# print(p1)
	return(toa)
}
