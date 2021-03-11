# statsExpressions

<details>

* Version: 1.0.0
* GitHub: https://github.com/IndrajeetPatil/statsExpressions
* Source code: https://github.com/cran/statsExpressions
* Date/Publication: 2021-03-11 06:20:02 UTC
* Number of recursive dependencies: 153

Run `revdep_details(, "statsExpressions")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. └─testthat::test_check("statsExpressions")
        2.   └─testthat::test_dir(...)
        3.     └─testthat:::test_files(...)
        4.       └─testthat:::test_files(...)
        5.         ├─testthat::with_reporter(...)
        6.         │ ├─base::withRestarts(...)
        7.         │ │ └─base:::withOneRestart(expr, restarts[[1L]])
        8.         │ │   └─base:::doWithOneRestart(return(expr), restart)
        9.         │ └─base::force(code)
       10.         └─testthat:::parallel_event_loop_chunky(queue, reporters)
       11.           └─queue$poll(Inf)
       12.             └─base::lapply(...)
       13.               └─testthat:::FUN(X[[i]], ...)
       14.                 └─private$handle_error(msg, i)
      Execution halted
    ```

