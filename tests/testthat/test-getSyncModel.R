context("getSyncModel")
library(yaps)

set.seed(42)
# set sync parameters 
max_epo_diff <- 120
min_hydros <- 2
time_keeper_idx <- 5
fixed_hydros_idx <- c(2:3, 6, 8, 11, 13:17)
n_offset_day <- 2
n_ss_day <- 2
keep_rate <- 15

# get input data ready for getSyncModel()
inp_sync <- getInpSync(sync_dat=ssu1, max_epo_diff, min_hydros, time_keeper_idx, 
    fixed_hydros_idx, n_offset_day, n_ss_day, keep_rate=keep_rate, silent_check=TRUE, sync_type='top')



sync_model <- sync_model_no_smartsearch <- sync_model_f1 <- sync_model_no_smartsearch_f1 <- NULL

# fit the sync model
sync_model 					<- getSyncModel(inp_sync, silent=TRUE, max_iter=500, tmb_smartsearch = TRUE)
# sync_model_no_smartsearch 	<- getSyncModel(inp_sync, silent=TRUE, max_iter=5000, tmb_smartsearch = FALSE)


sync_model_f1 					<- fineTuneSyncModel(sync_model, eps_threshold=1.5, silent=TRUE)
# sync_model_no_smartsearch_f1 	<- fineTuneSyncModel(sync_model_no_smartsearch, eps_threshold=1.5, silent=TRUE)


# plotSyncModelResids(sync_model, by = "overall")
# plotSyncModelResids(sync_model_f1, by = "overall")
# plotSyncModelResids(sync_model_no_smartsearch, by = "overall")
# plotSyncModelResids(sync_model_no_smartsearch_f1, by = "overall")


# # # Only run to reset reference
# setwd('tests/testthat')
# sync_model_ref <- sync_model
# sync_model_f1_ref <- sync_model_f1
# save(sync_model_ref, file = "sync_model_ref.RData")
# save(sync_model_f1_ref, file = "sync_model_f1_ref.RData")


test_that("sync_model is as expected", {
	load("sync_model_ref.RData")
	testthat::expect_equal(sync_model$pl, 							sync_model_ref$pl, tolerance=1E-3)
	testthat::expect_equal(sync_model$report, 						sync_model_ref$report, tolerance=1E-3)
	testthat::expect_equal(sync_model$obj_val, 						sync_model_ref$obj_val, tolerance=1E-3)
	testthat::expect_equal(sync_model$eps_long, 					sync_model_ref$eps_long, tolerance=1E-3)
	testthat::expect_equal(sync_model$inp_synced, 					sync_model_ref$inp_synced, tolerance=1E-3)
})

test_that("fine tuned sync_model is as expected", {
	load("sync_model_f1_ref.RData")
	testthat::expect_equal(sync_model_f1$pl, 							sync_model_f1_ref$pl, tolerance=1E-3)
	testthat::expect_equal(sync_model_f1$report, 						sync_model_f1_ref$report, tolerance=1E-3)
	testthat::expect_equal(sync_model_f1$obj_val, 						sync_model_f1_ref$obj_val, tolerance=1E-3)
	testthat::expect_equal(sync_model_f1$eps_long, 						sync_model_f1_ref$eps_long, tolerance=1E-3)
	testthat::expect_equal(sync_model_f1$inp_synced, 					sync_model_f1_ref$inp_synced, tolerance=1E-3)
})


# test_that("sync_model_no_smartsearch is as expected", {
	# load("sync_model_ref.RData")
	# testthat::expect_equal(sync_model_no_smartsearch$pl, 			sync_model_ref$pl, tolerance=1E-3)
	# testthat::expect_equal(sync_model_no_smartsearch$report, 		sync_model_ref$report, tolerance=1E-3)
	# testthat::expect_equal(sync_model_no_smartsearch$obj_val, 		sync_model_ref$obj_val, tolerance=1E-3)
	# testthat::expect_equal(sync_model_no_smartsearch$eps_long, 		sync_model_ref$eps_long, tolerance=1E-3)
	# # # # mismatch with tmb_smartsearch
	# # testthat::expect_equal(sync_model_no_smartsearch$inp_synced, 	sync_model_ref$inp_synced, tolerance=1E-3)
# })


# test_that("fine tuned sync_model_no_smartsearch is as expected", {
	# load("sync_model_f1_ref.RData")
	# testthat::expect_equal(sync_model_no_smartsearch_f1$pl, 							sync_model_f1_ref$pl, tolerance=1E-2)
	# testthat::expect_equal(sync_model_no_smartsearch_f1$report, 						sync_model_f1_ref$report, tolerance=1E-1)
	# testthat::expect_equal(sync_model_no_smartsearch_f1$obj_val, 						sync_model_f1_ref$obj_val, tolerance=1E-1)
	# testthat::expect_equal(sync_model_no_smartsearch_f1$eps_long, 						sync_model_f1_ref$eps_long, tolerance=1E-1)
	# # # # mismatch with tmb_smartsearch
 	# # testthat::expect_equal(sync_model_no_smartsearch_f1$inp_synced, 					sync_model_f1_ref$inp_synced, tolerance=1E-2)
# })



