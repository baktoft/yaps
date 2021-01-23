context("runYaps")
library(yaps)

# setwd('tests/testthat')
load("test_out_ref.RData")


tmb_smartsearch <- TRUE
test_out <- list()
test_out[[1]] <- testYaps(pingType='rbi', return_yaps=TRUE, tmb_smartsearch=tmb_smartsearch, est_ss=TRUE)
test_out[[2]] <- testYaps(pingType='rbi', return_yaps=TRUE, tmb_smartsearch=tmb_smartsearch, est_ss=FALSE)
test_out[[3]] <- testYaps(pingType='sbi', return_yaps=TRUE, tmb_smartsearch=tmb_smartsearch, est_ss=TRUE)
test_out[[4]] <- testYaps(pingType='sbi', return_yaps=TRUE, tmb_smartsearch=tmb_smartsearch, est_ss=FALSE)
test_out[[5]] <- testYaps(pingType='pbi', return_yaps=TRUE, tmb_smartsearch=tmb_smartsearch, est_ss=TRUE)
test_out[[6]] <- testYaps(pingType='pbi', return_yaps=TRUE, tmb_smartsearch=tmb_smartsearch, est_ss=FALSE)

# # # Only run to reset reference
# setwd('tests/testthat')
# test_out_ref <- test_out
# save(test_out_ref, file = "test_out_ref.RData")


test_that("test_out from testYaps() is as expected", {
	load("test_out_ref.RData")
	
	for(i in 1:length(test_out)){
		expect_equal(test_out[[i]]$pl$X, 		test_out_ref[[i]]$pl$X, tolerance=1E-3)
		expect_equal(test_out[[i]]$pl$Y, 		test_out_ref[[i]]$pl$Y, tolerance=1E-3)
		expect_equal(test_out[[i]]$pl$top, 		test_out_ref[[i]]$pl$top, tolerance=1E-3)
		expect_equal(test_out[[i]]$pl$logD_xy, 	test_out_ref[[i]]$pl$logD_xy, tolerance=1E-3)

		expect_equal(test_out[[i]]$plsd$X, 			test_out_ref[[i]]$plsd$X, tolerance=1E-3)
		expect_equal(test_out[[i]]$plsd$Y, 			test_out_ref[[i]]$plsd$Y, tolerance=1E-3)
		expect_equal(test_out[[i]]$plsd$top, 		test_out_ref[[i]]$plsd$top, tolerance=1E-3)
		expect_equal(test_out[[i]]$plsd$logD_xy, 	test_out_ref[[i]]$plsd$logD_xy, tolerance=1E-3)
	}
})



