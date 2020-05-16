# yaps v1.2.0.9111

* Add option to use alternative optimization function (EXPERIMENTAL). Try nloptr() instead of default nlminb(), if you have issues with e.g. false convergence.
* Split cpp file into pingType, Edist and ss specifics. Make code more readable and eliminates the need for 'diffuse priors' on parameters.
* Add option to specify pingType in testYaps(). PingType can be 'sbi' (default), 'rbi' or 'pbi'
* Create site to github pages intended to collect yaps-related resources, how-tos etc. (https://baktoft.github.io/yaps/)
* Add first step-by-step tutorial to yaps pages
* Implement testthat functions to ensure we don't break anything. Work in progress...
* Fix bug in prepDetections(). Thanks to Hugh Pederson for reporting it.
* Add option to use linear correction information when running the sync model (EXPERIMENTAL).

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

