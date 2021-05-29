devtools::load_all(".")


test_that("final code print", {
    expect_output(uniset::uniset_getFiles("wegweg"))
    expect_output(uniset::printFinalCodeMessage(".dpe", "stn", "bla"))
    expect_output(uniset::uniset_copyFilesToPackage("test"))

}) # EOT

