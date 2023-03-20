test_that("VBS over static data", {
    ## Read static data
    data <- structure(list(a = c("A", "A", "A", "A", "A", "A", "A", "A",
                         "B", "B", "B", "B", "B", "B", "B", "B", "C", "C", "C", "C", "C",
                         "C", "C", "C", "D", "D", "D", "D", "D", "D", "D", "D"),
                   i = c("a", "b", "c", "d", "e", "f", "g", "h", "a", "b", "c", "d", "e", "f",
                         "g", "h", "a", "b", "c", "d", "e", "f", "g", "h", "a", "b", "c",
                         "d", "e", "f", "g", "h"),
                   s = structure(c(2L, 2L, 4L, 4L, 2L,
                                   3L, 2L, 1L, 2L, 2L, 3L, 3L, 1L, 4L, 2L, 2L, 2L, 2L, 4L, 3L, 2L,
                                   3L, 1L, 2L, 1L, 2L, 3L, 3L, 1L, 3L, 2L, 2L),
                                 levels = c("OPT", "SAT", "UNSAT", "UNK"), class = "factor"),
                   o = c(44, 72, NA,
                         NA, 75, NA, 36, 0, 29, 25, NA, NA, 0, NA, 41, 22, 1, 62, NA,
                         NA, 25, NA, 0, 97, 0, 83, NA, NA, 0, NA, 28, 25),
                   m1 = c(97L, 19L, 66L, 16L, 2L, 23L, 97L, 51L, 89L, 44L, 35L, 52L, 40L, 34L,
                          9L, 84L, 87L, 65L, 83L, 33L, 90L, 29L, 93L, 15L, 8L, 33L, 62L,
                          82L, 96L, 5L, 100L, 32L)), class = "data.frame", row.names = c(NA,-32L))

    ## Test raw VBS data
    vbs <- VirtualBestSolver(data, withRows = FALSE, withCols = FALSE)
    expectedVBS <- structure(list(
        i = c("a", "b", "c", "d", "e", "f", "g", "h"),
        s = structure(c(1L, 2L, 3L, 3L, 1L, 3L, 1L, 1L), levels = c("OPT", "SAT", "UNSAT", "UNK"), class = "factor"),
        o = c(0, 25, NA, NA, 0, NA, 0, 0)), class = c("grouped_df", "tbl_df", "tbl", "data.frame"), row.names = c(NA, -8L),
        groups = structure(list(
            i = c("a", "b", "c", "d", "e", "f", "g", "h"),
            .rows = structure(list(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), ptype = integer(0), class = c("vctrs_list_of", "vctrs_vctr", "list"))),
            row.names = c(NA, -8L), class = c("tbl_df", "tbl", "data.frame"), .drop = TRUE))
    expect_equal(vbs, expectedVBS)

    ## Test VBS rows
    rdata <- VirtualBestSolver(data, withRows = TRUE, withCols = FALSE)
    expect_equal(nrow(rdata), nrow(data) + nrow(vbs))
    ## Test VBS columns
    rdata <- VirtualBestSolver(data, withRows = FALSE, withCols = TRUE)
    expect_equal(mean(rdata$VBS, na.rm=T), 0.4375)
    ## Test both
    rdata <- VirtualBestSolver(data, withRows = TRUE, withCols = TRUE)
    expect_equal(mean(rdata$VBS, na.rm=T), 0.55)

    ## Test again VBS columns
    rdata <- VirtualBestSolver(data,  cols = c("s", "o", "m1"), withRows = FALSE, withCols = TRUE)
    expect_equal(mean(rdata$VBS, na.rm=T), 0.25)

})
