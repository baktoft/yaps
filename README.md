
<!-- README_sync.md is generated from README_sync.Rmd. Please edit that file -->

# YAPS - How to use

### A complete walk-through taking you from raw data, through synchronization to final tracks

YAPS (Yet Another Position Solver) was introduced in 2017 as an
alternative to proprietary and vendor specific software for estimating
position and tracks of aquatic animals tagged with acoustic
transmitters.

Some motivations for developing and using YAPS:

  - **Transparency and reproducibility.** These concepts should be
    fundamental in all science and scientific methodology. However,
    available proprietary software for estimating positions lack
    transparency and literally constitute “Black Boxes” making
    reproducibility very challenging, if not impossible.

  - **Open source.** In addition to complete transparency, publishing
    YAPS open-source enables the user community to e.g. implement study
    specific adaptations, tweak parts of the estimation models and
    contribute to the continued development. Oh, and it is available
    free of charge. We urge users to submit improvements, refinements
    etc. to the main YAPS repository on github, thereby helping future
    users.

  - **Vendor agnostic.** The basic input to YAPS is plain matrices of
    time-of-arrivals of transmitter signals detected by hydrophones. The
    brand of hydrophones and transmitters are irrelevant. This enables
    more direct comparisons of tracks and derived behavioural
    quantifications between studies using varying vendors.

  - **Get the most out of your data.** To our knowledge, all vendor
    supplied tracking software process data from individual pings
    independently from other pings. By doing this, these algorithms
    explicitly discard collected data when pings are detected by less
    than three hydrophones. However, these data are valuable for
    estimating tracks if used correctly, and can dramatically increase
    overall data yield from your study (see e.g. Fig.6 in YAPS paper).

  - **Get the best out of your data.** The holistic
    complete-track-oriented approach used by YAPS, have proven to be
    superior in many instances to the isolated single-ping-oriented
    approach used in vendor software. The single-ping approach is prone
    to suffer from numerical challenges, which can lead to varying
    degrees of spurious outliers, multiple position estimates from
    individual pings (sometimes known as “ghost positions”) and overall
    increased track jaggedness and uncertainties. These artefacts often
    necessitate the use of post-processing filters and/or smoothing
    techniques. In contrast, many of these irregularities are in most
    circumstances handled well by YAPS and resulting tracks are often
    ready for use in further analyses. Note, we are not stating that
    YAPS estimated tracks never need or will benefit from
    post-processing – it depends very much on study specifics such as
    the acoustic environment and transmitter burst interval type and
    duration. However, using YAPS estimated tracks as the starting point
    for post-processing have in all use cases we know of been vastly
    superior to using tracks estimated by vendor software.

Although YAPS development was focused on openness, transparency and
flexibility, YAPS is not (nor has it been intended to be) a turn-key
and/or a one-size-fits-all solution. While YAPS itself is open and
transparent, the process from collecting raw data in the field to having
final estimated tracks is long and sometimes challenging. Especially,
the very important process of synchronizing arrays have proven to be a
major obstacle for many potential users.

This how-to guide is intended to provide users with an example work-flow
to start from. The guide walks through the entire workflow from raw data
collected by PPM-based systems (e.g. Vemco VR2 and Thelma TBR700) to
final tracks. It shows how to use new functions added to YAPS enabling
an easy and approachable (but fine tuned) synchronization. The guide
does not cover any field specific steps in the work-flow
(e.g. hydrophone array layout and fish tagging) as this information is
available in other sources. We do advice users to make informed
decisions regarding hydrophone array layout, to obtain precise positions
of all hydrophones using high quality gps units and to reduce the
potential for receiver movement during the study as much as possible.

## Disclaimer

**YAPS obeys the fundamental rule of “garbage in, garbage out”.
Therefore, DO NOT expect YAPS to salvage a poorly designed study, nor to
turn crappy data into gold.**  
We have attempted to make both synchronization process and track
estimation user-friendly. However, it is not trivial to synchronize
hydrophones (let alone automating the process) based on detections in a
variable and often noisy environment. Hydrophones might be
replaced/shifted and if not fixed securely, hydrophones might move/be
moved during a study. Additionally, hydrophone performance and output
format varies considerably between (and within) manufacturers. On top of
that, hydrophones don’t always behave and perform as expected. For
instance, some hydrophone models autonomously initiate reboots causing
perturbation of varying magnitude and/or duration of the internal clock
at apparently random time intervals. Therefore, the functions in YAPS
might perform sub-optimal or even fail miserably when applied to new
data. If/when this happens, please let us know through a direct message
or leave a bug-report. Also note, the to-do list for improvements and
tweaks is long and growing, so stay tuned for updates.

### About synchronization in YAPS

Regardles of which positioning algorithm is used, synchronization of the
hydrophone array prior to track estimation is *extremely* important for
quality of the final tracks and great care should be taken to achieve as
good as possible sync. The sync method included in YAPS has several
benefits. First of all, it *synchronizes all hydrophones in one go*,
avoiding sequential propagating synchronization that might lead to error
accumulation in the outer ends of the array. Additionally, it allows for
*estimation of hydrophone positions*, which can be usefull if the
initial hydrophone positions are uncertain e.g. due to deep water,
strong current or low-accuracy GPS. The internal clock in all
hydrophones drift in different directions and the rate of drift is
affected by water temperature. Therefore, temporally flexible non-linear
correction is needed. The sync method employed by YAPS is based on a
number of hydrophone-specific second-order polynomials. This
whole-array-at-once, have proved to give very good synchronization.
Perhaps most importantly, synchronization in YAPS is aimed at being
user-friendly. To meet this, we have attempted to keep all the
nitty-gritty stuff inside easy-to-use functions and kept the amount of
user decisions at a minimum.

## Installation

Make sure you have the newest version of YAPS installed. For this, you
need `devtools` installed - if not already installed, run
`install.packages('devtools')`.  
YAPS relies heavily on use of Template Model Builder
[TMB](https://github.com/kaskr/adcomp) for fitting the models, so make
sure `TMB` is installed and working by following the simple [TMB
instructions](https://github.com/kaskr/adcomp/wiki/Download).  
Then install the latest version of YAPS with:

``` 
    # install.packages("devtools")
    # install.packages("TMB")
    # TMB::runExample(all=TRUE)
    devtools::install_github("baktoft/yaps")
    library(yaps)
```

## Included example data

Two data sets are included in the package to serve as examples for how
to use YAPS:

1.  **ssu1**: A tiny data set containing short test tracks from Florida
    Bay. 19 hydrophones, \~24 hour data, \~1 hour test track, Vemco VR2
    PPM. This data set is very small and simple, 2D only and perfect for
    testing purposes. See`?ssu1`for further info.

2.  **hald**: A larger data set from the Danish lake Hald Sø containing
    test and fish tracks. 70 hydrophones, 1 week data, \~1 hour test
    track, N fish tracks, Thelma TBR700 PPM. (Will be added).

### Data structure

Each data set is a list containing data.tables `hydros`, `detections`
and `gps`. See e.g. `?ssu1` for further infomation.  
For help to create these tables from raw data, look in `?prepDetections`
or [go here](#prepDetectionsHelp)

``` r
    head(ssu1$hydros, n=3)
#>    serial      x       y   z sync_tag idx
#> 1: 128344 525973 2771312 1.8       NA   1
#> 2: 128355 526136 2771277 1.5       NA   2
#> 3: 128361 526189 2771221 1.1       NA   3
    head(ssu1$detections, n=3)
#>                     ts   tag        epo  frac serial
#> 1: 2019-09-09 16:05:53 59335 1568045153 0.631 128368
#> 2: 2019-09-09 16:07:46 59334 1568045266 0.644 128368
#> 3: 2019-09-09 16:09:21 59337 1568045361 0.932 128368
    head(ssu1$gps, n=3)
#>                     ts    utm_x   utm_y
#> 1: 2019-09-09 18:02:18 526073.3 2771147
#> 2: 2019-09-09 18:02:19 526073.3 2771147
#> 3: 2019-09-09 18:02:25 526073.1 2771145
```

## Workflow from raw to synchronized data

1.  **Prepare data** list containing tables `hydros` and `detections` as
    described [here](#prepDetectionsHelp).

2.  **Define sync parameters**. Each data set is different in terms of
    e.g. array configuration, precision and accuracy of hydrophone
    positioning, acoustic environment and detection probability, sync
    tag configuration, manufacturer etc. A number of parameters are
    available to setup the synchronization process for best results.
    
      - `max_epo_diff` Hydrophones are assumed to be running on at least
        somewhat similar time, so that ping trains from sync tags can be
        aligned correctly across all hydrophones. `max_epo_diff` sets
        the upper threshold for differences in TOA (Time Of Arrival) of
        a sync tag transmission detected at each hydrophone. Best
        parameter value depends on burst rate of sync tags and how far
        apart the internal clocks of the hydrophones are prior to
        synchronization. A bit less than half of minimum sync tag burst
        rate is a good starting choice. A linear time correction applied
        to the detection data prior to synchronization can improve the
        alignment process dramitically.
    
      - `min_hydros` To ensure connectivity throughout the array, pings
        from sync tags need to be detected by multiple hydrophones and
        all hydrophones need to detect sync tags also detected by other
        hydrophones. `min_hydros` sets the lower threshold of how many
        hydrophones need to detect each sync tag ping in order to be
        included in the sync process. Should be as high as possible
        while observing that all hydrosphones are contributing. If too
        low, isolated hydrophones risk falling out completely. Future
        versions will work towards automation of this step.
    
      - `time_keeper_idx` Index of the hydrophone to use as time keeper.
        Could e.g. be the one with smallest overall clock-drift.
    
      - `fixed_hydros_idx` Vector of hydro idx’s for all hydrophones
        where the position is assumed to be known with adequate accuracy
        and precision. Include as many as possible as fixed hydros to
        reduce overall computation time and reduce overall variability.
        As a bare minimum two hydros need to be fixed, but we strongly
        advice to use more than two.
    
      - `n_offset_day` Specifies the number of hydrophone specific
        quadratic polynomials to use per day. For PPM based systems, 1
        or 2 is often adeqaute.
    
      - `n_ss_day` Specifies number of speed of sound to estimate per
        day. Future versions will enable use of logged water temperature
        instead. However, estimating SS gives an extra option for
        sanity-checking the final sync-model.
    
      - `keep_rate` Syncing large data sets can take a long time.
        However, there is typically an excess number of sync tag
        detections and a sub-sample is typically enough for good
        synchronization. This parameter specifies the proportion (0-1)
        of data to keep when sub-sampling.

3.  **Compile input data** for the sync process using `getInpSync()`.

4.  **Run** the sync model using `getSyncModel()`. This can take a long
    time for larger data sets including more hydrophones and/or covering
    longer time periods. It might be a good idea to start with the first
    few days to make sure everything looks ok before syncing the entire
    data set. Also consider the parameter `keep_rate` in `getInpSync()`.

5.  **Check** the sync model to ensure the array is synced well using
    functions `plotSyncModelResids()` and `plotSyncModelCheck()`. These
    plots can be used to diagnose if the overall model fit is good.  
    Function `plotSyncModelResids()` plots sync model residuals (in
    meter) - sync model is ok if these residuals are centered closely
    around 0. If fixed hydrophones and/or sync tags consistently have
    large deviations from zero, it may indicate that position accuracy
    of the hydrophone is sub-optimal. Either obtain a better position
    (typically not possible) or allow th eposition to be non-fixed
    (i.e. remove from `fixed_hydros_idx`).  
    Function `plotSyncModelCheck()` apply the sync model to the sync
    data and compare true distances between hydros and sync tags to
    distances estimated based on the newly synced TOA-data.

6.  **Apply** the sync model to all data using `applySync()`.

The data should now be ready to be processed by YAPS to obtain estimated
tracks.

## Processing example data ssu1

### Synchronizing - ssu1

1.  Already taken care as we are using example data.

2.  set the parameters…

<!-- end list -->

``` r
    max_epo_diff <- 120
    min_hydros <- 2
    time_keeper_idx <- 5
    fixed_hydros_idx <- c(2:3,6, 8,11,13:17)
    n_offset_day <- 2
    n_ss_day <- 2
```

3.  get input data ready for getSyncModel()…

<!-- end list -->

``` r
    inp_sync <- getInpSync(sync_dat=ssu1, max_epo_diff, min_hydros, time_keeper_idx, fixed_hydros_idx, 
        n_offset_day, n_ss_day)
```

4.  fit the sync model…

<!-- end list -->

``` r
    sync_model <- getSyncModel(inp_sync, silent=TRUE)
```

5.  a Plot model residuals (see code block below for explanation)…

<!-- end list -->

``` r
# Overall histogram of sync model residuals (converted to meter). 
#   Vertical red lines indicate 1 %, 5 %, 95 % and 99 % quantiles.
plotSyncModelResids(sync_model, by='overall') 

# Boxplots of sync model residuals (in meter) grouped by sync tags (panel) and hydrophone (x-axis). 
#  Vertical red lines added for every fifth hydro_idx to aid in identifying troublesome hydrophones. 
plotSyncModelResids(sync_model, by='sync_tag')

# Boxplots of sync model residuals (in meter) grouped by sync tags (x-axis) and hydrophone (panels).  
plotSyncModelResids(sync_model, by='hydro')
```

![](man/figures/README_sync-plotSyncModelResids-1.png)![](man/figures/README_sync-plotSyncModelResids-2.png)![](man/figures/README_sync-plotSyncModelResids-3.png)

5.  b …and model check plots. These plots apply `sync_model` to the data
    and compare *true* hydrophones \<-\> sync tags distances to
    *estimated* distances based on the synced data (delta). Closer to
    zero is better. See code block below for further info about each
    plot.

<!-- end list -->

``` r
# Delta grouped by sync tag (panel) and sync period (x-axis)
plotSyncModelCheck(sync_model, by="sync_bin_sync")

# Delta grouped by hydro_idx (panel) and sync period (x-axis)
plotSyncModelCheck(sync_model, by="sync_bin_hydro")

# Delta grouped by sync tag (panel) and hydro_idx (x-axis)
plotSyncModelCheck(sync_model, by="sync_tag")

# Delta grouped by hydro_idx (panel) and sync tag (x-axis)
plotSyncModelCheck(sync_model, by="hydro")
```

![](man/figures/README_sync-plotSyncModelCheck-1.png)![](man/figures/README_sync-plotSyncModelCheck-2.png)![](man/figures/README_sync-plotSyncModelCheck-3.png)![](man/figures/README_sync-plotSyncModelCheck-4.png)

6.  Apply the sync model to all detection data. Synced timestamps are
    found in column eposync

<!-- end list -->

``` r
detections_synced <- applySync(toa=ssu1$detections, hydros=ssu1$hydros, sync_model)
head(detections_synced, n=3)
#>                     ts   tag        epo  frac serial    epofrac hydro_idx
#> 1: 2019-09-09 16:05:53 59335 1568045153 0.631 128368 1568045154         6
#> 2: 2019-09-09 16:07:46 59334 1568045266 0.644 128368 1568045267         6
#> 3: 2019-09-09 16:09:21 59337 1568045361 0.932 128368 1568045362         6
#>       eposync
#> 1: 1568045154
#> 2: 1568045267
#> 3: 1568045362
```

### Running YAPS - ssu1

Finally, run the synchronized data through YAPS - needs a little
compiling beforehand.  
The only tracks to estimate from this data set are test tracks performed
to test feasibility of deploying an array at this location. In the
original data set, several transmitters with different power and burst
rates were used. Here we focus on a single of these: ID 15266, Vemco V9,
high power output, 20-40 second burst interval.

``` r
# We use the estimated positions of the hydros as they are assumed to be better
hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
colnames(hydros_yaps) <- c('hx','hy','hz')

# Specify focal tag and tag specific min and max burst intervals
focal_tag <- 15266
rbi_min <- 20
rbi_max <- 40

# Extract relevant data from the synced data
synced_dat_ssu1 <- detections_synced[tag == focal_tag]

# Compile TOA-matrix to use for YAPS
toa_ssu1 <- getToaYaps(synced_dat_ssu1, hydros_yaps, rbi_min, rbi_max)

# Compile all input data needed for YAPS
inp_ssu1 <- getInp(hydros_yaps, toa_ssu1, E_dist="Mixture", n_ss=2, pingType="rbi", sdInits=1, 
        rbi_min=rbi_min-5, rbi_max=rbi_max+5, ss_data_what="est", ss_data=0)

# Run YAPS to obtain estimated track
yaps_out_ssu1 <- runYaps(inp_ssu1, silent=TRUE) # Default parameter values should work for this example
```

### Plot YAPS results - ssu1

Plot below shows true track (gps) as black hatched line while red line
is YAPS tracking results. Green symbols indicate hydrophone positions.

![](man/figures/README_sync-plotYapsTrack-1.png)<!-- -->

X and Y coordinates of true track (black) and estimated track(red).
Broken red lines indicate estimated track +- standard error of position
estimate.  
![](man/figures/README_sync-plotYapsOutDetails-1.png)<!-- -->

-----

### Caveats, notes and hints

  - Current version assumes all sync tags are co-located with
    hydrophones (which is usually the case). If needed, future versions
    will allow for other options.
  - For hydrophones where improved position is desired/needed (i.e. not
    in the `fixed_hydros_idx` vector), z is assumed to be known with
    much higher precision than x and y.
  - Only use data from periods where entire array is up, stationary and
    running - otherwise, make sure to NA invalid data…
  - Ensuring initial accuracy and precision of hydrophone positions is
    as good as possible is probably the easiest way to improve final
    results. We recommend to use a differential GPS/GNSS unit instead of
    low-accuracy consumer grade devices.

-----

## Appendix

#### How to create tables `hydros` and `detections`

The `detections` tables contain all detections and can be constructed
from csv-files exported from vendor software (millisecond or better time
resolution is necessary; remember to enable this in export options, if
needed). We advise to, if possible, apply a linear time correction on
the detection data based on overall drift of hydrophone internal clock.
Assuming the format follows the example shown below, the function
`prepDetections()` can be used. Data should be truncated to only cover
the period you want to synchronize - i.e. get rid of all detections
before and after the study period. Also consider to exclude data
recorded while the system is only partially in place - i.e. during
installation, retrieval and/or downloading hydrophones. A minimum is to
exclude data from each hydrophone in periods when it was not deployed at
the recording position. Data removal is likely most easily done after
import to R.  
There are many other formats out there - please tell us, if you want a
specific format added to `prepDetections()`. Future versions will allow
extraction of detections directly from the vendor provided SQLite
database.

``` r
    # Extract location of raw data included in yaps
    fn <- system.file("extdata", "VUE_Export_ssu1.csv", package="yaps") 
    vue <- data.table::fread(fn, fill=TRUE)
    head(vue, n=3)
#>        Date and Time (UTC)    Receiver    Transmitter Transmitter Name
#> 1: 2019-09-09 16:04:11.193 VR2W-128355 A69-1602-59335               NA
#> 2: 2019-09-09 16:04:12.574 VR2W-128371 A69-1602-59336               NA
#> 3: 2019-09-09 16:04:43.953 VR2W-128959 A69-1602-59335               NA
#>    Transmitter Serial Sensor Value Sensor Unit Station Name Latitude Longitude
#> 1:                 NA           NA          NA       CESI10       NA        NA
#> 2:                 NA           NA          NA       CESI15       NA        NA
#> 3:                 NA           NA          NA       CESI12       NA        NA
    detections <- prepDetections(raw_dat=vue, type="vemco_vue")
    head(detections, n=3)
#>                     ts   tag        epo frac serial
#> 1: 2019-09-09 16:04:11 59335 1568045051  193 128355
#> 2: 2019-09-09 16:04:12 59336 1568045053  574 128371
#> 3: 2019-09-09 16:04:43 59335 1568045084  953 128959
```

#### Example using YAPS on simulated data

    devtools::install_github("baktoft/yaps")
    rm(list=ls())   
    library(yaps)
    
    # Simulate true track of animal movement of n seconds
    trueTrack <- simTrueTrack(model='crw', n = 15000, deltaTime=1, shape=1, scale=0.5, addDielPattern=TRUE, ss='rw')
    
    # Simulate telemetry observations from true track.
    # Format and parameters depend on type of transmitter burst interval (BI) - stable (sbi) or random (rbi).
    pingType <- 'sbi'
    
    if(pingType == 'sbi') { # stable BI
        sbi_mean <- 30; sbi_sd <- 1e-4;
        teleTrack <- simTelemetryTrack(trueTrack, pingType=pingType, sbi_mean=sbi_mean, sbi_sd=sbi_sd)
    } else if(pingType == 'rbi'){ # random BI
        pingType <- 'rbi'; rbi_min <- 20; rbi_max <- 40;
        teleTrack <- simTelemetryTrack(trueTrack, pingType=pingType, rbi_min=rbi_min, rbi_max=rbi_max)
    }
    
    # Simulate hydrophone array
    hydros <- simHydros(auto=TRUE, trueTrack=trueTrack)
    toa_list <- simToa(teleTrack, hydros, pingType, sigmaToa=1e-4, pNA=0.25, pMP=0.01)
    toa <- toa_list$toa
    
    # Specify whether to use ss_data from measured water temperature (ss_data_what <- 'data') or to estimate ss in the model (ss_data_what <- 'est')
    ss_data_what <- 'data'
    if(ss_data_what == 'data') {ss_data <- teleTrack$ss} else {ss_data <- 0}
    
    
    if(pingType == 'sbi'){
        inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=0, ss_data_what=ss_data_what, ss_data=ss_data)
    } else if(pingType == 'rbi'){
        inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=0, rbi_min=rbi_min, rbi_max=rbi_max, ss_data_what=ss_data_what, ss_data=ss_data)
    } 
    str(inp)
    
    pl <- c()
    maxIter <- ifelse(pingType=="sbi", 500, 5000)
    outTmb <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
    str(outTmb)
    
    # Estimates in pl
    pl <- outTmb$pl
    # Correcting for hydrophone centering
    pl$X <- outTmb$pl$X + inp$inp_params$Hx0
    pl$Y <- outTmb$pl$Y + inp$inp_params$Hy0
    
    
    # Error estimates in plsd
    plsd <- outTmb$plsd
    
    # plot the resulting estimated track
    plot(y~x, data=trueTrack, type="l", xlim=range(hydros$hx), ylim=range(hydros$hy), asp=1)
    lines(y~x, data=teleTrack)
    points(hy~hx, data=hydros, col="green", pch=20, cex=3)
    lines(pl$Y~pl$X, col="red")
