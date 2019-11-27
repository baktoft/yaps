#' Build TOA matrix from synced data.table - also do some pre-filtering of severe MP, pruning loose ends etc
#' 
#' @param synced_dat `data.table` containing synchronized data formatted as output from/or obtained using `applySync()`
#' @inheritParams getInp
#' @export
getToaYaps <- function(synced_dat, hydros, rbi_min, rbi_max){

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
	dim(toa)
	dimnames(toa) <- NULL

	# remove rows with too short BI...
	top1 <- rowMeans(toa, na.rm=TRUE)
	diffs1 <- c(diff(top1),NA)
	nobs <- apply(toa, 1, function(k) sum(!is.na(k)))
	rem_idx <- which(diffs1 < rbi_min-1) # THIS NEEDS TO BE SET BASED ON USED SYSTEM - 1 IS TOO HIGH FOR HR-LIKE
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
	toa <- toa[first_ping:last_ping, ]
	
	# remake toa-matrix to include pings missed by all hydros...
	# top2 <- apply(toa, 1, function(k) median(k, na.rm=TRUE))
	top2 <- rowMeans(toa, na.rm=TRUE) # we use rowMeans instead of apply(... median) - rowMeans is much faster...
	diffs2 <- c(diff(top2),NA)

	pings <- data.table::data.table(top=top2, diff=diffs2)
	pings[, toa_idx:=1:.N]
	pings[, ping2next := 1]
	pings[, next_ping_too_late := diff > rbi_max+1]
	pings[next_ping_too_late==TRUE, ping2next:=ping2next+round(diff/rbi_max)]
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