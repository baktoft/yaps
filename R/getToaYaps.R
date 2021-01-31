#' Build TOA matrix from synced data.table - also do some pre-filtering of severe MP, pruning loose ends etc
#' 
#' @param synced_dat `data.table` containing synchronized data formatted as output from/or obtained using `applySync()`
#' @inheritParams getInp
#' @export
#' @example man/examples/example-yaps_ssu1.R
getToaYaps <- function(synced_dat, hydros, ping_type, rbi_min, rbi_max){

	# remove NAs in eposync
	synced_dat <- synced_dat[!is.na(eposync)]
	# remove multipaths...
	data.table::setkey(synced_dat, hydro_idx, eposync)
	diffs <- synced_dat[, c(diff(eposync),NA), by=hydro_idx]$V1
	mps <- which(diffs < .5)+1
	if(length(mps) > 0){
		synced_dat <- synced_dat[-mps]
	}

	# build toa-matrix...
	ts_focal <- splusTimeSeries::signalSeries(pos=floor(synced_dat[hydro_idx==1,eposync*10]), data=synced_dat[hydro_idx==1,eposync*10])
	for(i in 2:nrow(hydros)){
		# print(i)
		xi <- splusTimeSeries::signalSeries(pos=floor(synced_dat[hydro_idx==i,eposync*10]), data=synced_dat[hydro_idx==i,eposync*10])
		ts_focal <- splusTimeSeries::seriesMerge(ts_focal, xi, pos="union", matchtol=10)
	}
	ts_focal <- as.matrix(as.data.frame(ts_focal))
	ts_focal <- ts_focal/10
	toa <- ts_focal

	dimnames(toa) <- NULL

	# remove rows with too short BI...
	top1 <- rowMeans(toa, na.rm=TRUE)
	diffs1 <- c(diff(top1),NA)
	nobs <- apply(toa, 1, function(k) sum(!is.na(k)))
	if(rbi_min > 10){														### USE PING_TYPE INSTEAD!!!!
		rem_idx <- which(diffs1 < rbi_min-1) # THIS NEEDS TO BE SET BASED ON USED SYSTEM - 1 IS TOO HIGH FOR HR-LIKE
	} else {
		rem_idx <- which(diffs1 < rbi_min-0.1) # rbi_min < 10 should always be HR???
	}
	if(length(rem_idx) > 0){
		toa[rem_idx, ] <- NA
	}
	
	# remove empty rows... 
	empty_rows <- which(apply(toa, 1, function(k) sum(!is.na(k))) == 0)
	if(length(empty_rows) > 0){
		toa <- toa[-empty_rows, ]
	}
	
	# trim toa to exclude rows in start and end with very few obs
	nobs <- apply(toa, 1, function(k) sum(!is.na(k)))
	first_ping <- 	which(nobs >= 2)[1]
	last_ping <- 	rev(which(nobs >= 2))[1]
	
	if(is.na(first_ping) | is.na(last_ping) | first_ping == last_ping){
		print("FATAL ERROR: Not enough data to produce toa")
		return(FALSE)
	}
	
	toa <- toa[first_ping:last_ping, ]
	
	# remake toa-matrix to include pings missed by all hydros...
	# top2 <- apply(toa, 1, function(k) median(k, na.rm=TRUE))
	top2 <- rowMeans(toa, na.rm=TRUE) # we use rowMeans instead of apply(... median) - rowMeans is much faster...
	diffs2 <- c(diff(top2),NA)

	pings <- data.table::data.table(top=top2, diff=diffs2)
	pings[, toa_idx:=1:.N]
	pings[, ping2next := 1]
	if(rbi_max > 15){														### USE PING_TYPE INSTEAD!!!!
		pings[, next_ping_too_late := diff > rbi_max+1] # same as for rbi_min above when trying to exclude impossible pings
	} else {
		pings[, next_ping_too_late := diff > rbi_max+.1]
	}
	if(ping_type != 'sbi'){
		pings[next_ping_too_late==TRUE, ping2next:=ping2next+round(diff/rbi_max)] 
	} else {
		pings[next_ping_too_late==TRUE, ping2next:=round(diff/rbi_max)] # the line above puts in an extra pang for pingType = "sbi"
	}
	pings[, ping_idx:=cumsum(c(1,ping2next[-.N]))]
	toa_all <- matrix(ncol=ncol(toa), nrow=max(pings$ping_idx))
	toa_all[pings$ping_idx, ] <- toa
	
	
	# pings[, ping2next:=round(diffs2 / ((rbi_max - rbi_min)/2 + rbi_min))]
	# pings[, min_ping2next:=round(diffs2 / rbi_max)]
	# pings[, max_ping2next:=round(diffs2 / rbi_min)]

	# top3 <- rowMeans(toa_all, na.rm=TRUE) # we use rowMeans instead of apply(... median) - rowMeans is much faster...
	# diffs3 <- c(diff(top3),NA)
	# plot(diffs3)
	# which(diffs3 > 31)
	
	# toa_all[110:115,]
	
	return(toa_all)
	
	
}