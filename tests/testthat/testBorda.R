test_that("Wikipedia example for Borda count (en)", {
    df <- read.table(header = TRUE, text = "
voter candidate rank
u a 1
u b 2
u c 3
u d 4
v a 1
v b 2
v c 3
v d 4
w a 4
w b 1
w c 2
w d 3
")
    results <- BordaCount(df, "voter", "candidate", "rank")
    expected <- data.frame (candidate = c("b", "a", "c", "d"), count = c(10, 9, 7, 4))
    expect_equal( as.data.frame(results), expected)
})

test_that("Wikipedia example for Borda count (fr)", {

    df <- expand.grid(candidate = head(LETTERS, 4), voter = 1:100)
    df$rank <- c( rep(c(1,2,3,4), 42), rep(c(4,1,2,3), 26), rep(c(4,3,1,2), 15), rep(c(4,3,2,1), 17))

    results <- BordaCount(df, "voter", "candidate", "rank")
    expected <- data.frame (candidate = factor(c("B", "C", "A", "D")), count = c(294, 273, 226, 207))
    expect_equal( as.data.frame(results), expected)

})
