## Comments

* This is first submission of this package to CRAN. 

* The package contains compiled c++ code, hence it is not possible to bring down the installed package size.

* win-builder mentions possibly mis-spelled words in DESCRIPTION. These are all author names.

## Test environments
* Local Windows 10, R 4.0.3, R 4.1.0 (2021-01-20 r79850)
* Ubuntu 16.04 (travis), R 4.0.2 
* Ubuntu 20.04 (github action), R release, R devel
* Windows-latest (github action), R 3.6, R release, R devel
* macOS-latest (github action), R 3.6, R release
* win-builder (oldrelease, release, devel)

## R CMD check results

0 errors | 0 warnings | 2 notes

checking CRAN incoming feasibility ... NOTE
Maintainer: 'Henrik Baktoft <hba@aqua.dtu.dk>'
New submission

checking installed package size ... NOTE
installed size is 21.6Mb
sub-directories of 1Mb or more:
libs  19.6Mb

---

On win-builder (oldrelease, release, devel):

Possibly mis-spelled words in DESCRIPTION:
  Baktoft (9:579, 9:729)
  Corujo (9:774)
  Gjelland (9:588, 9:738)
  Rehage (9:756)
  Rodemann (9:764)
  Thygesen (9:607, 9:792)
  Viadero (9:782)
  Økland (9:598, 9:748)
