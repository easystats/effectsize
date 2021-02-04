# correlation

<details>

* Version: 0.5.0
* GitHub: https://github.com/easystats/correlation
* Source code: https://github.com/cran/correlation
* Date/Publication: 2020-12-02 07:40:19 UTC
* Number of recursive dependencies: 169

Run `revdep_details(, "correlation")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `expected`:  0
      ── Failure (test-correlation.R:102:7): comparison with other packages ──────────
      max(r - as.matrix(ppcor$estimate)) (`actual`) not equal to 0 (`expected`).
      
        `actual`: NA
      `expected`:  0
      ── Failure (test-correlation.R:144:5): format checks ───────────────────────────
      c(nrow(out), ncol(out)) (`actual`) not equal to c(6, 13) (`expected`).
      
        `actual`: 6 14
      `expected`: 6 13
      
      [ FAIL 6 | WARN 24 | SKIP 1 | PASS 64 ]
      Error: Test failures
      Execution halted
    ```

# statsExpressions

<details>

* Version: 0.7.0
* GitHub: https://github.com/IndrajeetPatil/statsExpressions
* Source code: https://github.com/cran/statsExpressions
* Date/Publication: 2021-01-19 16:10:02 UTC
* Number of recursive dependencies: 153

Run `revdep_details(, "statsExpressions")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       [7] "estimate"    | "estimate"      [7] 
       [8] "conf.level"  - "ci.width"      [8] 
       [9] "conf.low"    | "conf.low"      [9] 
      [10] "conf.high"   | "conf.high"     [10]
      [11] "effectsize"  | "effectsize"    [11]
      
      `actual$conf.level` is a double vector (0.89, 0.99, 0.9, 0.5)
      `expected$conf.level` is absent
      
      `actual$ci.width` is absent
      `expected$ci.width` is a double vector (0.89, 0.99, 0.9, 0.5)
      
      [ FAIL 4 | WARN 0 | SKIP 19 | PASS 65 ]
      Error: Test failures
      Execution halted
    ```

