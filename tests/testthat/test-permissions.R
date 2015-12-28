context("read_permissions")
test_that("it is read correctly", {
    res <- read_permissions("bioconductor.authz.orig")
    expect_equal(length(res), 2265L)

    expect_equal(names(res)[1:5],
        c("groups", "/", "/trunk/bioconductor.org", "/trunk/bioC/R-patches", "/trunk/madman/Rpacks"))

    # check group is read correctly
    expect_equal(names(res$groups)[1:4], c(NA, "bioconductor-readers", NA, "bioconductor-write0"))

    expect_equal(tail(n = 8, names(res$groups)),
        c("normalize450K", "profileScoreDist", "transcriptR", "kimod", "lpsymphony", "splineTCDiffExpr", NA, NA))

    expect_equal(tail(n = 10, names(res)),
        c("/trunk/madman/Rpacks/profileScoreDist", "/branches/RELEASE_3_2/madman/Rpacks/profileScoreDist",
            "/trunk/madman/Rpacks/transcriptR", "/branches/RELEASE_3_2/madman/Rpacks/transcriptR",
            "/trunk/madman/Rpacks/kimod", "/branches/RELEASE_3_2/madman/Rpacks/kimod",
            "/trunk/madman/Rpacks/lpsymphony", "/branches/RELEASE_3_2/madman/Rpacks/lpsymphony",
            "/trunk/madman/Rpacks/splineTCDiffExpr", "/branches/RELEASE_3_2/madman/Rpacks/splineTCDiffExpr"))
    last <- tail(n = 1, res)[[1]]
    t1 <- authz_section(x = list("@splineTCDiffExpr" = "rw", ""),
        name = "/branches/RELEASE_3_2/madman/Rpacks/splineTCDiffExpr")
    names(t1)[[2]] <- NA
    expect_equal(last, t1)
})

test_that("Reading and writing without changes produces the same file", {
    res <- read_permissions("bioconductor.authz.orig")
    tmp <- tempfile()
    on.exit(unlink(tmp))

    write_permissions(res, tmp)

    orig_lines <- readLines("bioconductor.authz.orig")

    new_lines <- readLines(tmp)

    expect_equal(orig_lines, new_lines)
})
