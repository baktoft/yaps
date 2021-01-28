\dontrun{
hydros <- ssu1$hydros
colnames(hydros) <- c('serial','hx','hy','hz','sync_tag','idx')
bbox <- getBbox(hydros)
plotBbox(hydros, bbox)
}