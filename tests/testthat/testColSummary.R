test_that("Test a few invariants in ColSummary", {
    n <- 5
    m <- 10
    data <- GenerateOptiData(n = n, m = m, p = 2)
    res <- ColSummary(data, cols = "o")
    expect_equal(sum(res[[1]]), n * m)

    res <- ColSummary(data, cols = "o", valS = "UNSAT")
    expect_true(all(is.na(res[[2]]$median)))
    expect_true(all(is.nan(res[[2]]$mean)))
    expect_true(all(is.na(res[[2]]$sd)))
    expect_true(all(is.nan(res[[2]]$gmean)))


    res <- ColSummary(data, cols = c("o", "m1"), valS = list(c("OPT", "SAT", "UNSAT", "UNK")))
    expect_equal(unique(res[[2]]$n), m)
    expect_equal(unique(res[[3]]$n), m)

    res <- ColSummary(data, cols = c("o", "m1"), colG = c("a", "i"),  valS = list("OPT", c("SAT", "OPT")))
    expect_equal(length(res), 4)

})
