## Known issues

- Failed handshake with *shinyapps.io* is a false positive.
- Unavailable DOI link are false positives.
- Spelling mistakes are false positives.

## Test environments

* local installation: R 4.2.2 on Windows
* GitHub Actions
    - Windows:        release
    - macOS:          release
    - ubuntu-18.04:   release, oldrel, 4.0, 3.6
* win-builder:        release


## R CMD check results

0 errors | 0 warnings | 0 notes


## revdepcheck results

We checked 19 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

