test_that("Simple cross table", {
    df <- expand.grid(a = LETTERS, i = letters, s = c("OPT", "SAT"), t = 1)
    ct <- CrossTable(df, "A", "B")
    expect_true(all(ct == 26))
    ct <- CrossTable(df, "C")
    expect_true(all(ct == 650))

})

test_that("Cross table with dropped levels", {
    s <- factor(c("OPT", "SAT", "UNK"))
    df <- expand.grid(a = LETTERS, i = letters, s = head(s, 2), t = 1)

    ct <- CrossTable(df, "E", "F")
    expect_equal(nrow(ct), 2)
    expect_equal(ncol(ct), 2)
    expect_true(all(ct == 26))

    ct <- CrossTable(df, "H")
    expect_equal(nrow(ct), 2)
    expect_equal(ncol(ct), 2)
    expect_true(all(ct == 650))
})

test_that("Cross table with dropped levels on rows", {
    s <- factor(c("OPT", "SAT", "UNK"))
    df <- expand.grid(a = LETTERS, i = letters, s = s, t = 1)
    df <- subset(df, df$a != "A" | df$s != "UNK")
    ct <- CrossTable(df, "A", "Z")
    expect_equal(nrow(ct), 2)
    expect_equal(ncol(ct), 3)
    expect_true(all(ct == 26))

    ct <- CrossTable(df, "A")
    expect_equal(nrow(ct), 2)
    expect_equal(ncol(ct), 3)
    expect_true(all(ct == 650))
})
