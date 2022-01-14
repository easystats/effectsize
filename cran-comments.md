All URL issues have been resolved.

## Test environments

* local installation: R 4.1.1 on Windows
* GitHub Actions
    - Windows:        devel, release, oldrel
    - macOS:          devel, release, oldrel
    - ubuntu-16.04:   devel, release, oldrel, 3.6, 3.5, 3.4
* win-builder:        release


## R CMD check results

0 errors | 0 warnings | 0 notes


### Known issues

- Failed handshake with *shinyapps.io* is a false positive.


## revdepcheck results

We checked 16 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
    * `report`: Error is expected. Authors have been updated and will submit updated package.
 * We failed to check 0 packages
