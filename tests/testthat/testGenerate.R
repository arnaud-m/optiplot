test_that("Generate optimization data", {
    n  <- 3
    m  <- 10
    p  <- 2
    t <- 10
    o <- 10
    df <- GenerateOptiData(n, m, p, t, o)
    expect_equal(nrow(df), n * m)
    expect_equal(length(unique(df$a)), n)
    expect_equal(length(unique(df$i)), m)
    expect_true(all( is.na(df$o) | (df$o >= 0 & df$o <= o)))
    expect_true(all( df$m1 >= 0 & df$m1 <= t))
    expect_true(all( df$m2 >= 0 & df$m2 <= t))
})

test_that("Generate optimization data (no UNSAT answer)", {
    n  <- 6
    m  <- 2
    p  <- 0
    t <- 15
    o <- 15
    df <- GenerateOptiData(n, m, p, t, o, pSat = 1)
    expect_equal(nrow(df), n * m)
    expect_equal(length(unique(df$a)), n)
    expect_equal(length(unique(df$i)), m)
    expect_true(all( df$s != "UNSAT"))
    expect_true(all( is.na(df$o) | (df$o >= 0 & df$o <= o)))
})

test_that("Generate optimization data (only OPT answers)", {
    n  <- 8
    m  <- 5
    p  <- 0
    t <- 15
    o <- 25
    df <- GenerateOptiData(n, m, p, t, o, pSat = 1, pOpt = 1, pUnknown = 0)
    expect_equal(nrow(df), n * m)
    expect_equal(length(unique(df$a)), n)
    expect_equal(length(unique(df$i)), m)
    expect_true(all( df$s == "OPT"))
    expect_true(all( is.na(df$o) | (df$o >= 0 & df$o <= o)))
})

test_that("Generate optimization data (only UNSAT and UNK answers)", {
    n  <- 11
    m  <- 7
    p  <- 0
    t <- 150
    o <- 50
    df <- GenerateOptiData(n, m, p, t, o, pSat = 0, pOpt = 1, pUnknown = 0.5)
    expect_equal(nrow(df), n * m)
    expect_equal(length(unique(df$a)), n)
    expect_equal(length(unique(df$i)), m)
    expect_true(all( df$s == "UNSAT" | df$s == "UNK"))
    expect_true(all( is.na(df$o) | (df$o >= 0 & df$o <= o)))
})
