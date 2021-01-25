context("SyncModel plotters")

set.seed(42)
# test_that("plotSyncModelHydros is as expected", {
	# sync_model <- ssu1$sync_model
	# p <- plotSyncModelHydros(sync_model)
	# vdiffr::expect_doppelganger("plotSyncModelHydros", p)
# })

test_that("plotSyncModelResids_overall is as expected", {
	sync_model <- ssu1$sync_model
	p <- plotSyncModelResids(sync_model, by = "overall")
	vdiffr::expect_doppelganger("plotSyncModelResids_overall", p)
})

test_that("plotSyncModelResids_quantiles is as expected", {
	sync_model <- ssu1$sync_model
	p <- plotSyncModelResids(sync_model, by = "quantiles")
	vdiffr::expect_doppelganger("plotSyncModelResids_quantiles", p)
})

test_that("plotSyncModelCheck_by_hydro is as expected", {
	sync_model <- ssu1$sync_model
	p <- plotSyncModelCheck(sync_model, by = "hydro")
	vdiffr::expect_doppelganger("plotSyncModelCheck_by_hydro", p)
})

test_that("plotSyncModelCheck_by_sync_tag is as expected", {
	sync_model <- ssu1$sync_model
	p <- plotSyncModelCheck(sync_model, by = "sync_tag")
	vdiffr::expect_doppelganger("plotSyncModelCheck_by_sync_tag", p)
})

