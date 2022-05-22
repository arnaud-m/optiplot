test_that("Rank vector", {
    n <- 50
    x <- seq(n)
    expect_equal(x, Rank(x))
    x <- rev(x)
    expect_equal(x, Rank(x))
})

test_that("Rank list and data frame", {
    n <- 50
    m <- 2
    x <- rep(seq(m), each = n)
    y <- seq( m * n)
    expect_equal(y, Rank(list(x, y)))
    expect_equal(y, Rank(data.frame(x, y, y)))

    x <- rev(x)
    y <- rev(y)
    expect_equal(y, Rank(list(x, y)))
    expect_equal(y, Rank(data.frame(x, y, y)))
})

test_that("Rank Random data frames", {
    t <- 5
    for(i in seq(t)) {
        n <- 20
        m <- 1000
        ## Generate random data frame
        df <- data.frame(
            x = sample.int(n, m, replace = TRUE),
            y = sample.int(n, m, replace = TRUE),
            z = sample.int(n, m, replace = TRUE)
        )
        ## map vector order onto integers
        x <- n * (n * df$x + df$y) + df$z
        expect_equal(Rank(x), Rank(df))
    }
})
