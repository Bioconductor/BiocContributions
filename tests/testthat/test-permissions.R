context("read_permissions")
test_that("it fails if file does not exist", {
   expect_error(read_permissions("unknown"), "retrieving file 'unknown'")
})

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
    expect_equal(last, authz_section(x = list("@splineTCDiffExpr" = "rw", ""),
        name = "/branches/RELEASE_3_2/madman/Rpacks/splineTCDiffExpr"))
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

context("edit_permissions")
test_that("Assigning new biocoductor-users works", {
    res <- read_permissions("bioconductor.authz.orig")

    new_perms <- edit_software_permissions(list(`bioconductor-readers` = "test.user"), data = res)

    # New user added
    expect_equal(new_perms$groups$`bioconductor-readers`, "test.user")

    # Rest of data is equal
    expect_equal(new_perms$groups[-2], res$groups[-2])
    expect_equal(new_perms[-1], res[-1])
})

test_that("Setting a single user to existing package assignments works", {
    res <- read_permissions("bioconductor.authz.orig")

    new_perms <- edit_software_permissions(list(`lpsymphony` = "j.heiss"), data = res)

    # New user set
    expect_equal(new_perms$groups$`lpsymphony`, "j.heiss")

    # Rest of data is equal
    lpsymphony_group <- which(names(new_perms$groups) == "lpsymphony")
    expect_equal(new_perms$groups[-lpsymphony_group], res$groups[-lpsymphony_group])
    expect_equal(new_perms[-1], res[-1])
})

test_that("Setting multiple users to existing package assignments works", {
    res <- read_permissions("bioconductor.authz.orig")

    new_perms <- edit_software_permissions(list(`lpsymphony` = c("j.heiss", "a.karapetyan")), data = res)

    # New user set
    expect_equal(new_perms$groups$`lpsymphony`, c("j.heiss", "a.karapetyan"))

    # Rest of data is equal
    lpsymphony_group <- which(names(new_perms$groups) == "lpsymphony")
    expect_equal(new_perms$groups[-lpsymphony_group], res$groups[-lpsymphony_group])
    expect_equal(new_perms[-1], res[-1])
})

test_that("Setting multiple users to multiple packages package assignments works", {
    res <- read_permissions("bioconductor.authz.orig")

    new_perms <- edit_software_permissions(list(`lpsymphony` = c("j.heiss", "a.karapetyan"),
            `transcriptR` = c("a.bass", "j.sun2")), data = res)

    # New user set
    expect_equal(new_perms$groups$`lpsymphony`, c("j.heiss", "a.karapetyan"))
    expect_equal(new_perms$groups$`transcriptR`, c("a.bass", "j.sun2"))

    # Rest of data is equal
    modified_groups <- which(names(new_perms$groups) %in% c("lpsymphony", "transcriptR"))
    expect_equal(new_perms$groups[-modified_groups], res$groups[-modified_groups])
    expect_equal(new_perms[-1], res[-1])
})

test_that("Users who don't exist in bioconductor-readers are added", {
    res <- read_permissions("bioconductor.authz.orig")

    new_perms <- edit_software_permissions(list(`lpsymphony` = "new.user"), data = res)

    # Added to bioconductor-readers
    expect_equal(tail(n = 1, new_perms$groups$`bioconductor-readers`), "new.user")

    # New user set for package
    expect_equal(new_perms$groups$`lpsymphony`, "new.user")

    # Rest of data is equal
    modified_groups <- which(names(new_perms$groups) %in% c("lpsymphony", "bioconductor-readers"))
    expect_equal(new_perms$groups[-modified_groups], res$groups[-modified_groups])
    expect_equal(new_perms[-1], res[-1])
})

test_that("Multiple users who don't exist in bioconductor-readers are added", {
    res <- read_permissions("bioconductor.authz.orig")

    new_perms <- edit_software_permissions(list(`lpsymphony` = c("new.user", "new.user2")), data = res)

    # Added to bioconductor-readers
    expect_equal(tail(n = 2, new_perms$groups$`bioconductor-readers`), c("new.user", "new.user2"))

    # New user set
    expect_equal(new_perms$groups$`lpsymphony`, c("new.user", "new.user2"))

    # Rest of data is equal
    modified_groups <- which(names(new_perms$groups) %in% c("lpsymphony", "bioconductor-readers"))
    expect_equal(new_perms$groups[-modified_groups], res$groups[-modified_groups])
    expect_equal(new_perms[-1], res[-1])
})

test_that("Setting multiple new users to multiple packages package assignments works", {
    res <- read_permissions("bioconductor.authz.orig")

    new_perms <- edit_software_permissions(list(`lpsymphony` = c("new.user1", "new.user2"),
            `transcriptR` = c("new.user3", "new.user4")), data = res)

    # Added to bioconductor-readers
    expect_equal(tail(n = 4, new_perms$groups$`bioconductor-readers`),
        c("new.user1", "new.user2", "new.user3", "new.user4"))

    # New user set
    expect_equal(new_perms$groups$`lpsymphony`, c("new.user1", "new.user2"))
    expect_equal(new_perms$groups$`transcriptR`, c("new.user3", "new.user4"))

    # Rest of data is equal
    modified_groups <- which(names(new_perms$groups) %in% c("lpsymphony", "transcriptR", "bioconductor-readers"))
    expect_equal(new_perms$groups[-modified_groups], res$groups[-modified_groups])
    expect_equal(new_perms[-1], res[-1])
})

test_that("Adding new packages works", {
    res <- read_permissions("bioconductor.authz.orig")

    new_perms <- edit_software_permissions(list(`new.pkg` = "a.bass"), data = res)

    # New package added
    expect_true("new.pkg" %in% names(new_perms$groups))
    expect_equal(new_perms$groups$`new.pkg`, "a.bass")

    new_paths <- tail(n = 2, new_perms)
    expect_equal(names(new_paths),
        c("/trunk/madman/Rpacks/new.pkg", "/branches/RELEASE_3_2/madman/Rpacks/new.pkg"))

    expect_equal(new_paths,
        list("/trunk/madman/Rpacks/new.pkg" =
            authz_section(list("@new.pkg" = "rw", ""), name = "/trunk/madman/Rpacks/new.pkg"),
        "/branches/RELEASE_3_2/madman/Rpacks/new.pkg" =
            authz_section(list("@new.pkg" = "rw", ""), name = "/branches/RELEASE_3_2/madman/Rpacks/new.pkg")))

    # Rest of data is equal
    modified_groups <- which(names(new_perms$groups) %in% c("new.pkg"))
    expect_equal(new_perms$groups[-modified_groups], c(res$groups[]))
})
