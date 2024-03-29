% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getInp.R
\name{getInp}
\alias{getInp}
\title{Get prepared inp-object for use in TMB-call}
\usage{
getInp(
  hydros,
  toa,
  E_dist,
  n_ss,
  pingType,
  sdInits = 1,
  rbi_min = 0,
  rbi_max = 0,
  ss_data_what = "est",
  ss_data = 0,
  biTable = NULL,
  z_vec = NULL,
  bbox = NULL
)
}
\arguments{
\item{hydros}{Dataframe from simHydros() or Dataframe with columns hx and hy containing positions of the receivers. Translate the coordinates to get the grid centre close to (0;0).}

\item{toa}{TOA-matrix: matrix with receivers in rows and detections in columns. Make sure that the receivers are in the same order as in hydros, and that the matrix is very regular: one ping per column (inlude empty columns if a ping is not detected).}

\item{E_dist}{Which distribution to use in the model - "Gaus" = Gaussian, "Mixture" = mixture of Gaussian and t or "t" = pure t-distribution}

\item{n_ss}{Number of soundspeed estimates: one estimate per hour is usually enough}

\item{pingType}{Type of transmitter to simulate - either stable burst interval ('sbi'), random burst interval ('rbi') or random burst interval but where the random sequence is known a priori}

\item{sdInits}{If >0 initial values will be randomized around the normally fixed value using rnorm(length(inits), mean=inits, sd=sdInits)}

\item{rbi_min, rbi_max}{Minimum and maximum BI for random burst interval transmitters}

\item{ss_data_what}{What speed of sound (ss) data to be used. Default ss_data_what='est': ss is estimated by the model. Alternatively, if ss_data_what='data': ss_data must be provided and length(ss_data) == ncol(toa)}

\item{ss_data}{Vector of ss-data to be used if ss_data_what = 'est'. Otherwise ss_data <- 0 (default)}

\item{biTable}{Table of known burst intervals. Only used when pingType == "pbi". Default=NULL}

\item{z_vec}{Vector of known depth values (positive real). Default=NULL is which case no 3D is assumed. If z_vec = "est" depth will be estimated.}

\item{bbox}{Spatial constraints in the form of a bounding box. See ?getBbox for details.}
}
\value{
List of input data ready for use in \code{runYaps()}
}
\description{
Wrapper-function to compile a list of input needed to run TMB
}
\examples{
\donttest{
library(yaps)
set.seed(42)

# # # Example using the ssu1 data included in package. See ?ssu1 for info.
# # # Set parameters to use in the sync model - these will differ per study
max_epo_diff <- 120
min_hydros <- 2
time_keeper_idx <- 5
fixed_hydros_idx <- c(2:3, 6, 8, 11, 13:17)
n_offset_day <- 2
n_ss_day <- 2
keep_rate <- 20


# # # Get input data ready for getSyncModel()
inp_sync <- getInpSync(sync_dat=ssu1, max_epo_diff, min_hydros, time_keeper_idx, 
    fixed_hydros_idx, n_offset_day, n_ss_day, keep_rate=keep_rate, silent_check=TRUE)

# # # Check that inp_sync is ok
checkInpSync(inp_sync, silent_check=FALSE)

# # # Also take a look at coverage of the sync data
getSyncCoverage(inp_sync, plot=TRUE)

# # # Fit the sync model
sync_model <- getSyncModel(inp_sync, silent=TRUE, max_iter=200, tmb_smartsearch = TRUE)

# # # On some systems it might work better, if we disbale the smartsearch feature in TMB
# # # To do so, set tmb_smartsearch = FALSE in getSyncModel()

# # # Visualize the resulting sync model
plotSyncModelResids(sync_model, by = "overall")
plotSyncModelResids(sync_model, by = "quantiles")
plotSyncModelResids(sync_model, by = "sync_tag")
plotSyncModelResids(sync_model, by = "hydro")
plotSyncModelResids(sync_model, by = "temporal_hydro")
plotSyncModelResids(sync_model, by = "temporal_sync_tag")

# # # If the above plots show outliers, sync_model can be fine tuned by excluding these.
# # # Use fineTuneSyncModel() for this.
# # # This should typically be done sequentially using eps_thresholds of e.g. 1E4, 1E3, 1E2, 1E2
sync_model <- fineTuneSyncModel(sync_model, eps_threshold=1E3, silent=TRUE)
sync_model <- fineTuneSyncModel(sync_model, eps_threshold=1E2, silent=TRUE)

# # # Apply the sync_model to detections data.
detections_synced <- applySync(toa=ssu1$detections, hydros=ssu1$hydros, sync_model)

# # # Prepare data for running yaps
hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
colnames(hydros_yaps) <- c('hx','hy','hz')
focal_tag <- 15266
rbi_min <- 20
rbi_max <- 40
synced_dat <- detections_synced[tag == focal_tag]
toa <- getToaYaps(synced_dat=synced_dat, hydros=hydros_yaps, pingType='rbi', 
	rbi_min=rbi_min, rbi_max=rbi_max)

bbox <- getBbox(hydros_yaps, buffer=50, pen=1e6)
inp <- getInp(hydros_yaps, toa, E_dist="Mixture", n_ss=5, pingType="rbi", 
	sdInits=1, rbi_min=rbi_min, rbi_max=rbi_max, ss_data_what="est", ss_data=0, bbox=bbox)


# # # Check that inp is ok
checkInp(inp)

# # # Run yaps on the prepared data to estimate track
yaps_out <- runYaps(inp, silent=TRUE, tmb_smartsearch=TRUE, maxIter=5000) 


# # # Plot the results and compare to "the truth" obtained using gps

oldpar <- par(no.readonly = TRUE) 
par(mfrow=c(2,2))
plot(hy~hx, data=hydros_yaps, asp=1, xlab="UTM X", ylab="UTM Y", pch=20, col="green")
lines(utm_y~utm_x, data=ssu1$gps, col="blue", lwd=2)
lines(y~x, data=yaps_out$track, col="red")

plot(utm_x~ts, data=ssu1$gps, col="blue", type="l", lwd=2)
points(x~top, data=yaps_out$track, col="red")
lines(x~top, data=yaps_out$track, col="red")
lines(x-2*x_sd~top, data=yaps_out$track, col="red", lty=2)
lines(x+2*x_sd~top, data=yaps_out$track, col="red", lty=2)

plot(utm_y~ts, data=ssu1$gps, col="blue", type="l", lwd=2)
points(y~top, data=yaps_out$track, col="red")
lines(y~top, data=yaps_out$track, col="red")
lines(y-2*y_sd~top, data=yaps_out$track, col="red", lty=2)
lines(y+2*y_sd~top, data=yaps_out$track, col="red", lty=2)

plot(nobs~top, data=yaps_out$track, type="p", main="#detecting hydros per ping")
lines(caTools::runmean(nobs, k=10)~top, data=yaps_out$track, col="orange", lwd=2)
par(oldpar)
}
}
