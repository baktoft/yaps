# Align data from a tag with known random burst interval to the burst interval sequence
# using the included hald data (see ?hald for info).
\dontrun{
detect_1315 <- hald$detections[tag==1315]
sync_model <- hald$sync_model
seq_1315 <- hald$burst_seqs$seq1315
rbi_min <- hald$fish[tag==1315, rbi_min]
rbi_max <- hald$fish[tag==1315, rbi_max]
hydros <- hald$hydros

synced_data_1315 <- applySync(toa=detect_1315, sync_model=sync_model, hydros=hydros)
alignBurstSeq(synced_dat=synced_data_1315, burst_seq=seq_1315, rbi_min=rbi_min, rbi_max=rbi_max)

}