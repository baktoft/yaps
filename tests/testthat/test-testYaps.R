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


# # # testing when opt_fun = 'nloptr'
opt_controls <- list(algorithm="NLOPT_LD_AUGLAG",
		 xtol_abs=1e-12,
		 maxeval=2E+4,
		 print_level = 0,
		 local_opts= list(algorithm="NLOPT_LD_AUGLAG_EQ", xtol_rel=1e-4)
		 # local_opts= list(algorithm="NLOPT_LD_CCSAQ", xtol_rel=1e-4)
)

tmb_smartsearch <- TRUE
test_out_nloptr <- list()
test_out_nloptr[[1]] <- testYaps(pingType='rbi', return_yaps=TRUE, est_ss=TRUE,  opt_fun='nloptr', opt_controls=opt_controls,  tmb_smartsearch=tmb_smartsearch)
test_out_nloptr[[2]] <- testYaps(pingType='rbi', return_yaps=TRUE, est_ss=FALSE, opt_fun='nloptr', opt_controls=opt_controls,  tmb_smartsearch=tmb_smartsearch)
test_out_nloptr[[3]] <- testYaps(pingType='sbi', return_yaps=TRUE, est_ss=TRUE,  opt_fun='nloptr', opt_controls=opt_controls,  tmb_smartsearch=tmb_smartsearch)
test_out_nloptr[[4]] <- testYaps(pingType='sbi', return_yaps=TRUE, est_ss=FALSE, opt_fun='nloptr', opt_controls=opt_controls,  tmb_smartsearch=tmb_smartsearch)
test_out_nloptr[[5]] <- testYaps(pingType='pbi', return_yaps=TRUE, est_ss=TRUE,  opt_fun='nloptr', opt_controls=opt_controls,  tmb_smartsearch=tmb_smartsearch)
test_out_nloptr[[6]] <- testYaps(pingType='pbi', return_yaps=TRUE, est_ss=FALSE, opt_fun='nloptr', opt_controls=opt_controls,  tmb_smartsearch=tmb_smartsearch)




# # # Only run to reset reference
# setwd('tests/testthat')
# test_out_ref <- test_out
# save(test_out_ref, file = "test_out_ref.RData")
# test_out_nloptr_ref <- test_out_nloptr
# save(test_out_nloptr_ref, file = "test_out_nloptr_ref.RData")


test_that("test_out from testYaps() is as expected", {
	load("test_out_ref.RData")
	
	for(i in 1:length(test_out)){
		expect_equal(test_out[[i]]$pl$X, 		test_out_ref[[i]]$pl$X, tolerance=1E-1)
		expect_equal(test_out[[i]]$pl$Y, 		test_out_ref[[i]]$pl$Y, tolerance=1E-1)
		expect_equal(test_out[[i]]$pl$top, 		test_out_ref[[i]]$pl$top, tolerance=1E-1)
		expect_equal(test_out[[i]]$pl$logD_xy, 	test_out_ref[[i]]$pl$logD_xy, tolerance=1E-1)

		# expect_equal(test_out[[i]]$plsd$X, 			test_out_ref[[i]]$plsd$X, tolerance=1E-3)
		# expect_equal(test_out[[i]]$plsd$Y, 			test_out_ref[[i]]$plsd$Y, tolerance=1E-3)
		# expect_equal(test_out[[i]]$plsd$top, 		test_out_ref[[i]]$plsd$top, tolerance=1E-3)
		# expect_equal(test_out[[i]]$plsd$logD_xy, 	test_out_ref[[i]]$plsd$logD_xy, tolerance=1E-3)
	}
})

test_that("test_out_nloptr from testYaps() is as expected", {
	load("test_out_nloptr_ref.RData")
	
	for(i in 1:length(test_out_nloptr)){
		expect_equal(test_out_nloptr[[i]]$pl$X, 		test_out_nloptr_ref[[i]]$pl$X, tolerance=1E-1)
		expect_equal(test_out_nloptr[[i]]$pl$Y, 		test_out_nloptr_ref[[i]]$pl$Y, tolerance=1E-1)
		expect_equal(test_out_nloptr[[i]]$pl$top, 		test_out_nloptr_ref[[i]]$pl$top, tolerance=1E-1)
		expect_equal(test_out_nloptr[[i]]$pl$logD_xy, 	test_out_nloptr_ref[[i]]$pl$logD_xy, tolerance=1E-1)

		# expect_equal(test_out[[i]]$plsd$X, 			test_out_ref[[i]]$plsd$X, tolerance=1E-3)
		# expect_equal(test_out[[i]]$plsd$Y, 			test_out_ref[[i]]$plsd$Y, tolerance=1E-3)
		# expect_equal(test_out[[i]]$plsd$top, 		test_out_ref[[i]]$plsd$top, tolerance=1E-3)
		# expect_equal(test_out[[i]]$plsd$logD_xy, 	test_out_ref[[i]]$plsd$logD_xy, tolerance=1E-3)
	}
})



