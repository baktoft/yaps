context("Sync model")
library(yaps)

set.seed(42)
# set sync parameters 
max_epo_diff <- 120
min_hydros <- 2
time_keeper_idx <- 5
fixed_hydros_idx <- c(2:3, 6, 8, 11, 13:17)
n_offset_day <- 2
n_ss_day <- 2
keep_rate <- 0.1

# get input data ready for getSyncModel()
inp_sync <- getInpSync(sync_dat=ssu1, max_epo_diff, min_hydros, time_keeper_idx, 
    fixed_hydros_idx, n_offset_day, n_ss_day, keep_rate=keep_rate, silent_check=TRUE)

# fit the sync model
sync_model <- getSyncModel(inp_sync, silent=TRUE, max_iter=5000)

# # # Only run to reset reference
# setwd('tests/testthat')
# sync_model_ref <- sync_model
# save(sync_model_ref, file = "sync_model_ref.RData")


test_that("sync_model is as expected", {
	load("sync_model_ref.RData")
	expect_equal(sync_model$pl, sync_model_ref$pl, tolerance=1E-3)
	expect_equal(sync_model$report, sync_model_ref$report, tolerance=1E-3)
	expect_equal(sync_model$obj_val, sync_model_ref$obj_val, tolerance=1E-3)
	expect_equal(sync_model$eps_long, sync_model_ref$eps_long, tolerance=1E-3)
	expect_equal(sync_model$inp_synced, sync_model_ref$inp_synced, tolerance=1E-3)
})
