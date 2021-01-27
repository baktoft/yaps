context("alignBurstSeq")
set.seed(42)
# # # Run alignBurstSeq on hald data
synced_dat_1315 <- dat_align$synced_dat_1315
seq_1315 <- dat_align$seq_1315
rbi_min <- 60
rbi_max <- 120
aligned_dat <- alignBurstSeq(synced_dat=synced_dat_1315, burst_seq=seq_1315, rbi_min=rbi_min, rbi_max=rbi_max, plot_diag=FALSE)

# # # Only run to reset reference
# setwd('tests/testthat')
# aligned_dat_ref <- aligned_dat
# save(aligned_dat_ref, file = "aligned_dat_ref.RData")

test_that("alignBurstSeq works as expected", {
	load("aligned_dat_ref.RData")
	expect_equal(aligned_dat, aligned_dat_ref, tolerance=1E-6)
})
