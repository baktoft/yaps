# yaps v.2.0.0.9000

* Work in progress - still in early phase
* Complete rework of all/most/many functions
* Will not be backward compatible
* Will break current code 
* Improve functions to obtain sync models
* Attempt to improve workflow
* Remove dependence on deprecated package splusTimeSeries 


# yaps v.1.2.5.9000

## New stuff
* Add another approach to do array synchronization. Still experimental, but seems to work just as well, but much faster. 
Set sync_type="delta" in getInpSync() to try it out.
* Enable use of lin_corr_coeffs in applySync() when type=="toa_matrix".
* Change from fatal error to warning if observed rbi_min/max is outside specified rbi_min/max. Now also updates input to runYaps to allow for deviations.
* Add check to ensure ss_data does not contain NAs.
* Add check that time_keeper has data in all subsets of sync_model.
* Much better performance of prepDetections() - thanks @mhpob!

## Bug fixes
* Fix bug when downsampling toa for sync.
* Remove dependency of deprecated package tictoc.
* Fix bug in plotSyncModelCheck() when number of offsets is low.
* Various minor bug fixes.

# yaps v1.2.5

## New stuff
* Add support for ping_type = 'sbi_doulbe'. Special case - needs carefull contrsuction of TOA-matrix.
* More robustification of the optimizer.
* More pre-flight checks to catch issues with inp.
* Use separate diffusivity for Z when estimating 3D

## Bug fixes
* Bug fix - constraint hitting very fast transmitters with BI < 1 relaxed
* Implement better start of top sequence when ping_type = sbi_double

# yaps v1.2.4

## New stuff
* Now on CRAN
* More checks in checkInp() to catch typical errors in format of inp.
* EXPERIMENTAL Attempt to robustify runYaps() - use with care.

## Bug fixes
* Fix bug in getToaYaps() re number of empty pings.
* Docs and examples fixed to meet requirements.
* Make getToaYaps() aware of pingType


# yaps v1.2.3

## New stuff
* Moved example data `hald` to an external package with yaps example data `yapsdata`. Available from github using devtools::install_github('baktoft/yapsdata').
* Lots of examples and tests added.


# yaps v1.2.2

## New stuff
* New site added to github pages intended to collect yaps-related resources, how-tos etc. (https://baktoft.github.io/yaps/)
* Add first step-by-step tutorial to yaps pages
* Add function to sequentially fine-tune sync model.
* Add option to use speed of sound based on logged temperatur in the synchronisation process.
* Add function to fine-tune sync_model based on residual threshold
* Add plot to check temporal stability of sync_models. Try plotSyncModelResids(sync_model, by='temporal')
* Add option to impose spatial constraints (BBox only) and plot a visual of the constraint. Mainly used to constrain parameter space and increase speed and convergence.
* Add various checks in checkInpSync()
* Add option to use selective downsampling in getInpSync(). 
* Add function to plot hydros from sync_model - usefull if hydros were re-positioned during getSyncModel().
* Add option to estimate Z-dimension (depth) of tracks.
* Add function to calucate speed of sound from water temperature, salinity and depth - tempToSs(temp, sal, depth).
* Add github actions to hopefully ensure nothing breaks from now on...

## Bug fixes
* Fix nasty bug in likelihood contribution of ToP-estimation when using random burst interval (ping_type = 'rbi')
* Fix bug in getInpSync() - failed if sync_tag was only heard on own hydro
* Eliminate estimation of log_sigma_hydros_xy in sync_model
* Relax priors on SS in both track and sync model - consider to switch to softplus instead
* Return plsd object from getSyncModel
* Fix bug in getToaYaps() that allowed too short/too high BI to pass through when using ping_type='rbi' and very fast transmitters
* Fix bug in getToaYaps() that added to many empty pings when ping_type='sbi'.


# yaps v1.2.0.9111

## New stuff
* Add option to use alternative optimization function (EXPERIMENTAL). Try nloptr() instead of default nlminb(), if you have issues with e.g. false convergence.
* Add option to use linear correction information when running the sync model (EXPERIMENTAL).
* Add option to specify all three ping types in testYaps(). PingType can be 'sbi' (default), 'rbi' or 'pbi'
* Not related to package development, but check out yaps-live https://baktoft.shinyapps.io/yapslive/ to see `yaps` in action.


## Minor new stuff
* Split cpp file into pingType, Edist and ss specifics. Make code more readable and eliminates the need for 'diffuse priors' on parameters.
* Implement testthat functions to ensure future updates doesn't break anything. Work in progress...
* Update readMe.md

## Bug fixes
* Fix bug in prepDetections(). Thanks to Hugh Pederson for reporting it.


# yaps v1.2.0.9110

## New stuff
* Add support for using pressure sensor data as z-values in track estimation
* Add function alignBurstSeq() - aligns detection data with known burst sequence
* Add option to use stationary ref tags as beacons/sync tags without hydro
* Add testYaps() - function to test that everything is running as expected

## Minor new stuff
* Add sync_model and known burst sequences to example data hald
* Add extra plots to plotSyncModelCheck()
* Improve diagnostic plots - now violins instead on boxplots
* Add first version of checkInp(). More tests will be added to catch common mistakes
* Added a `NEWS.md` file to track changes to the package
* Make console output from runYaps() more user friendly - work in progress

## Bug fixes
* Fixed bug in getToaYaps() when is.na(eposync) == TRUE
* Add option to ignore extreme values when plotting plotSyncModelCheck()
* Fix issue with sync_model extrapolating eposync beyond period
* Relax initial ss and put gentle constraint on rest ss
* Fix bug in on Attach()

