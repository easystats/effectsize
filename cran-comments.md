All URL issues have been resolved.
DOI issues are a false positive.

## Test environments

* local installation: R 4.2.1 on Windows
* GitHub Actions
    - Windows:        devel, release, oldrel
    - macOS:          devel, release, oldrel
    - ubuntu-18.04:   devel, release, oldrel, 4.0, 3.6
* win-builder:        release


## R CMD check results

0 errors | 0 warnings | 0 notes


### Known issues

- Failed handshake with *shinyapps.io* is a false positive.
- Unavailable DOI link are false positives.
- Spelling mistakes are false positives.


## revdepcheck results

We checked 18 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

### New problems

* statsExpressions - 1 new error. Package maintainer has been informed.
