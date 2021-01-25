\dontrun{
sync_model <- ssu1$sync_model

plotSyncModelHydros(sync_model)

plotSyncModelResids(sync_model, by = "overall")
plotSyncModelResids(sync_model, by = "quantiles")
plotSyncModelResids(sync_model, by = "sync_tag")
plotSyncModelResids(sync_model, by = "hydro")
plotSyncModelResids(sync_model, by = "temporal")
plotSyncModelResids(sync_model, by = "temporal_hydro")
plotSyncModelResids(sync_model, by = "temporal_sync_tag")

plotSyncModelCheck(sync_model, by = "hydro")
plotSyncModelCheck(sync_model, by = "sync_tag")
plotSyncModelCheck(sync_model, by = "sync_bin_sync")
plotSyncModelCheck(sync_model, by = "sync_bin_hydro")
# # # if more sync periods are used, these two can be applied
# plotSyncModelCheck(sync_model, by = "sync_bin_sync_smooth")
# plotSyncModelCheck(sync_model, by = "sync_bin_hydro_smooth")

}