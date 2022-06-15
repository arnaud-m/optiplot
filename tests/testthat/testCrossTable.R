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

test_that("Cross table with a unique row or column", {
    s <- factor(c("OPT", "SAT"))
    df <- data.frame(a = c("A", "A", "B", "B") , i = c("a", "b", "a", "b"), s = s[ c(1,1,1,2)])

    ct <- CrossTable(df, "A", "B", tableGrob = TRUE)
    expect_equal(nrow(ct$table), 1)
    expect_equal(ncol(ct$table), 2)
    expect_true(all(ct$table == 1))

    ct <- CrossTable(df, "B", "A", tableGrob = TRUE)
    expect_equal(nrow(ct$table), 2)
    expect_equal(ncol(ct$table), 1)
    expect_true(all(ct$table == 1))

})


test_that("Cross table with a single value", {
    df <- expand.grid(a =c("A", "B") , i = c("a", "b"), s = factor("OPT"))
    ct <- CrossTable(df, "A", "B", tableGrob = TRUE)
    expect_equal(nrow(ct$table), 1)
    expect_equal(ncol(ct$table), 1)
    expect_true(all(ct$table == 2))

})
