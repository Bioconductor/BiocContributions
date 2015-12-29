context("clean_data_package")
test_that("it splits the package properly", {
  tmp_pkgs <- tempfile()
  dir.create(tmp_pkgs)

  tmp_data_store <- tempfile()
  dir.create(tmp_data_store)

  on.exit(unlink(c(tmp_pkgs, tmp_data_store), recursive = TRUE))

  pkg <- system.file(package="BiocContributions",
                     "testpackages", "hgu95av2.db_2.10.1.tar.gz")

  clean_data_package(pkg,
                     svn_pkgs = tmp_pkgs,
                     svn_data_store = tmp_data_store)

  expect_equal(list.files(tmp_pkgs), "hgu95av2.db")

  expect_equal(list.files(file.path(tmp_pkgs, "hgu95av2.db")),
               c("DESCRIPTION", "external_data_store.txt", "man", "NAMESPACE", "R", "tests"))

  expect_equal(readLines(file.path(tmp_pkgs, "hgu95av2.db", "external_data_store.txt")),
               "inst/extdata")

  expect_equal(list.files(tmp_data_store), "hgu95av2.db")

  expect_equal(list.files(file.path(tmp_data_store, "hgu95av2.db")),
               c("inst"))

  expect_equal(list.files(file.path(tmp_data_store, "hgu95av2.db", "inst")),
               c("extdata"))
})
