# Align data from a tag with known random burst interval to the burst interval sequence
# using the hald data included in `yapsdata` (see ?yapsdata::hald for info).
synced_dat_1315 <- dat_align$synced_dat_1315
seq_1315 <- dat_align$seq_1315
rbi_min <- 60
rbi_max <- 120
aligned_dat <- alignBurstSeq(synced_dat=synced_dat_1315, burst_seq=seq_1315, 
	rbi_min=rbi_min, rbi_max=rbi_max, plot_diag=TRUE)
