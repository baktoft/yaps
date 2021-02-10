#' Align synced data with known burst sequence
#' 
#' Identifies where in the sequence of known burst intervals the detected data is from. 
#' Add extra columns to data.table containing ping index of the burst sequence (seq_ping_idx) and expected time of ping (seq_epo).
#' Only to be used for 'random' burst interval data when you know the burst sequence.
#' @param synced_dat data.table obtained using applySync() on a detections_table
#' @param burst_seq Vector containing known burst sequence
#' @param seq_lng_min Minimum length of sequence of consecutive pings to use for the alignment. Finds first occurence of sequence of this length in the data and compare to the known burst sequence
#' @param rbi_min,rbi_max Minimum and maximum burst interval of the transmitter. Used to identify sequence of consecutive pings in the data
#' @param plot_diag Logical indicating if visual diagnosis plots should be created.
#' @export
#' @return `data.table` like the input `synced_dat`, but with extra columns seq_ping_idx and seq_epo
#' @example man/examples/example-alignBurstSeq.R
alignBurstSeq <- function(synced_dat, burst_seq, seq_lng_min=10, rbi_min, rbi_max, plot_diag=TRUE){
	burst_seq_dt <- data.table::data.table(bi=burst_seq)
	burst_seq_dt[, seq_ping_idx := 1:.N]
	burst_seq_dt[, cum_bi := c(0, cumsum(bi)[1:(.N-1)] )]

	data.table::setorder(synced_dat, hydro_idx, eposync)
	synced_dat[, delta_eposync := c(diff(eposync), NA), by=c('hydro_idx')]
	synced_dat[, ping_idx := 1:.N, by=c('hydro_idx')]
	
	synced_dat[delta_eposync %between% c(rbi_min-1, rbi_max+1), in_seq:=1]

	synced_dat[in_seq==1, delta_ping := c(diff(ping_idx), NA), by=hydro_idx]
	
	ping_rle <- synced_dat[, rle(delta_ping)]
	
	synced_dat[, seq_lng := rep(ping_rle$lengths, ping_rle$lengths)]
	
	first_seq_info <- synced_dat[delta_ping == 1 & seq_lng >= seq_lng_min][1]
	
	first_seq_bi <- synced_dat[hydro_idx==first_seq_info$hydro_idx & ping_idx %between% c(first_seq_info$ping_idx, first_seq_info$ping_idx + seq_lng_min), diff(eposync)]
		
	seq_diffs <- plyr::aaply(1:(length(burst_seq) - seq_lng_min), 1, function(k) {	sum(abs(burst_seq[k:(k+seq_lng_min-1)] - first_seq_bi))	}	)
	seq_fix_idx <- which.min(seq_diffs)
	
	
	# finding epo for all pings in burst_seq_dt
	seq_fix_epo <- (first_seq_info$eposync)
	seq_fix_cum_bi <- burst_seq_dt[seq_fix_idx, cum_bi]
	burst_seq_dt[, seq_epo := cum_bi - seq_fix_cum_bi + seq_fix_epo]
	
	# adding burst sequence ping info to table synced_dat
	burst_seq_dt[, roll_seq_epo := seq_epo]
	data.table::setkey(burst_seq_dt, roll_seq_epo)
	synced_dat[, roll_eposync := eposync]
	data.table::setkey(synced_dat, roll_eposync)

	synced_dat[, c('seq_epo' ,'seq_ping_idx') := burst_seq_dt[synced_dat,  roll="nearest"][, c('seq_epo', 'seq_ping_idx')] ]
	synced_dat[, roll_eposync := NULL]

	# plot if plot_diag == TRUE
	if(plot_diag){
		oldpar <- par(no.readonly = TRUE) 
		on.exit(par(oldpar))
		
		par(mfrow=c(1,2))
		plot(log(seq_diffs))
		points(log(seq_diffs[seq_fix_idx]) ~ seq_fix_idx, col="red", pch=20, cex=2)

		plot(synced_dat[, eposync - seq_epo] ~ synced_dat$ts, pch=".")
	}
	
	return(synced_dat)
}
