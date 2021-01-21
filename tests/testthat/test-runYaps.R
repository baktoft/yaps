context("runYaps")
library(yaps)

# # # Estimate a yaps output based on the ssu1 example data
# setwd('tests/testthat')
load("sync_model_ref.RData")
set.seed(42)
detections_synced <- applySync(toa=ssu1$detections, hydros=ssu1$hydros, sync_model_ref)
hydros_yaps <- data.table::data.table(sync_model_ref$pl$TRUE_H)
colnames(hydros_yaps) <- c('hx','hy','hz')
focal_tag <- 15266
rbi_min <- 20
rbi_max <- 40
synced_dat <- detections_synced[tag == focal_tag]
toa <- getToaYaps(synced_dat, hydros_yaps, rbi_min, rbi_max)
inp <- getInp(hydros_yaps, toa, E_dist="Mixture", n_ss=2, pingType="rbi", 
	sdInits=1, rbi_min=rbi_min, rbi_max=rbi_max, ss_data_what="est", ss_data=0)
yaps_out <- runYaps(inp, silent=TRUE) 


# # # Only run to reset reference
# setwd('tests/testthat')
# yaps_out_ref <- yaps_out
# save(yaps_out_ref, file = "yaps_out_ref.RData")

test_that("yaps_out is as expected", {
	load("yaps_out_ref.RData")
	
	# need to run individual tests - somehow order of items in the lists differ when running devtools::check()
	expect_equal(yaps_out$pl$X, yaps_out_ref$pl$X, tolerance=1E-1)
	expect_equal(yaps_out$pl$Y, yaps_out_ref$pl$Y, tolerance=1E-1)
	expect_equal(yaps_out$pl$top, yaps_out_ref$pl$top, tolerance=1E-1)
	expect_equal(yaps_out$pl$ss, yaps_out_ref$pl$ss, tolerance=1E-1)
	expect_equal(yaps_out$pl$logD_xy, yaps_out_ref$pl$logD_xy, tolerance=1E-1)

	expect_equal(yaps_out$pl_sd$X,   yaps_out_ref$pl_sd$X, tolerance=1E-3)
	expect_equal(yaps_out$pl_sd$Y,   yaps_out_ref$pl_sd$Y, tolerance=1E-3)
	expect_equal(yaps_out$pl_sd$top, yaps_out_ref$pl_sd$top, tolerance=1E-3)
	expect_equal(yaps_out$pl_sd$ss,  yaps_out_ref$pl_sd$ss, tolerance=1E-3)
	expect_equal(yaps_out$pl_sd$logD_xy,  yaps_out_ref$pl_sd$logD_xy, tolerance=1E-3)
})


