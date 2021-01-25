context("runYaps")
library(yaps)

set.seed(42)
# # # Run alignBurstSeq on hald data
detect_1315 <- hald$detections[tag==1315]
sync_model <- hald$sync_model
seq_1315 <- hald$burst_seqs$seq1315
rbi_min <- hald$fish[tag==1315, rbi_min]
rbi_max <- hald$fish[tag==1315, rbi_max]
hydros <- hald$hydros

synced_data_1315 <- applySync(toa=detect_1315, sync_model=sync_model, hydros=hydros)
aligned_dat <- alignBurstSeq(synced_dat=synced_data_1315, burst_seq=seq_1315, rbi_min=rbi_min, rbi_max=rbi_max, plot_diag=FALSE)

# # # Only run to reset reference
# setwd('tests/testthat')
# aligned_dat_ref <- aligned_dat
# save(aligned_dat_ref, file = "aligned_dat_ref.RData")

test_that("alignBurstSeq works as expected", {
	load("aligned_dat_ref.RData")
	expect_equal(aligned_dat, aligned_dat_ref, tolerance=1E-6)
})
