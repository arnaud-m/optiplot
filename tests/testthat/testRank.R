test_that("Rank vector", {
    n <- 50
    x <- seq(n)
    y <- rev(x)
    expect_equal(Rank(x), x)
    expect_equal(Rank(x, decreasing = TRUE), y)

    expect_equal(Rank(y), y)
    expect_equal(Rank(y, decreasing = TRUE), x)
})

test_that("Rank list and data frame", {
    n <- 50
    m <- 2
    x <- rep(seq(m), each = n)
    rx <- rev(x)
    y <- seq( m * n)
    ry <- rev(y)
    expect_equal(Rank(list(x, y)), y)
    expect_equal(Rank(list(x, y), decreasing = TRUE), ry)
    expect_equal(Rank(data.frame(x, y, y)), y)
    expect_equal(Rank(data.frame(x, y, y), decreasing = TRUE), ry)

    expect_equal(Rank(list(rx, ry)), ry)
    expect_equal(Rank(list(rx, ry), decreasing = TRUE), y)
    expect_equal(Rank(data.frame(rx, ry, ry)), ry)
    expect_equal(Rank(data.frame(rx, ry, ry), decreasing = TRUE), y)
})

test_that("Rank with missing values", {

    df <- data.frame(x = c(1, 2, NA), y = c(12:1, NA, NA, NA))
    expect_equal(c(4, 9, 14, 3, 8, 13, 2, 7, 12, 1, 6, 11, 5, 10, 15), Rank(df))

    df <- rbind(df, df)
    expect_equal(Rank(df), c(7.5, 17.5, 27, 5.5, 15.5, 25, 3.5, 13.5, 23, 1.5, 11.5, 21,
                   9, 19, 29, 7.5, 17.5, 28, 5.5, 15.5, 26, 3.5, 13.5, 24, 1.5,
                   11.5, 22, 10, 20, 30))
})

test_that("Rank Random data frames", {
    t <- 3
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
        expect_equal(Rank(-x), Rank(df, decreasing = TRUE))
    }
})
