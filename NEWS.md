# yaps v1.2.1.9000

## New stuff
* New site added to github pages intended to collect yaps-related resources, how-tos etc. (https://baktoft.github.io/yaps/)
* Add first step-by-step tutorial to yaps pages
* Add function to sequentially fine-tune sync model.
* Add option to use speed of sound based on logged temperatur in the synchronisation process.
* Add function to fine-tune sync_model based on residual threshold
* Add plot to check temporal stability of sync_models. Try plotSyncModelResids(sync_model, by='temporal')
* Add option to impose spatial constraints (BBox only) and plot a visual of the constraint. Mainly used to constrain parameter space and increase speed and convergence.
* Add various checks in checkInpSync()

## Bug fixes
* Fix bug in getInpSync() - failed if sync_tag was only heard on own hydro
* Eliminate estimation of log_sigma_hydros_xy in sync_model
* Relax priors on SS in both track and sync model - consider to switch to softplus instead
* Return plsd object from getSyncModel




# yaps v1.2.0.9111

## New stuff
* Add option to use alternative optimization function (EXPERIMENTAL). Try nloptr() instead of default nlminb(), if you have issues with e.g. false convergence.
* Add option to use linear correction information when running the sync model (EXPERIMENTAL).
* Add option to specify all three ping types in testYaps(). PingType can be 'sbi' (default), 'rbi' or 'pbi'
* Not related to package development, but check out [yaps-live](https://baktoft.shinyapps.io/yapslive/) to see `yaps` in action.


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

