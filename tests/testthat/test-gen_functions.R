library(testthat)
#
######## the easy part #######-----
ptp <- "~/Documents/RPS/uniset_R/uniset"
###
test_that("checkCh1", {
    a <- "a"; b <- "a"
    expect_null(checkCh1(a,b))
    a <- c("a", "b"); b <- "a"
    expect_error(checkCh1(a,b))
    a <- 1; b <- "a"
    expect_error(checkCh1(a,b))
}) # EOT

test_that("checkGetTaPaSH", {
    a <- "a"; b <- "a"
    expect_type(checkGetTaPaSH(a,b), "character")
    a <- 1; b <- "a"
    expect_error(checkGetTaPaSH(a,b))
    a <- c("a", "b"); b <- "a"
    expect_error(checkGetTaPaSH(a,b))
}) # EOT

test_that("checkGetTaPaEnv", {
    a <- "a"; b <- "a"
    expect_type(checkGetTaPaEnv(a,b), "character")
    a <- 1; b <- "a"
    expect_error(checkGetTaPaEnv(a,b))
    a <- c("a", "b"); b <- "a"
    expect_error(checkGetTaPaEnv(a,b))
}) # EOT

test_that("checkPath_Package_getName", {
    expect_type(checkPath_Package_getName(ptp), "character")
    expect_error(checkPath_Package_getName("a"))
}) # EOT

test_that("printFinalCodeMessage", {
    expect_output(printFinalCodeMessage("a", "b", "c"))
}) # EOT

taPaName <- "dogPack"
taPaEnv <- ".doe"
taPaObj <- "stn"
taPaSH <- "dogPack_SH"
tmpl <- "_TEMPLATE"
test_that("readInReplaceTxtUnisFiles", {
    expect_type(readInReplaceTxtUnisFiles(taPaName, taPaSH, taPaEnv, taPaObj, tmpl), "list")
}) # EOT

td <- tempdir()
na <- "name"
pa <- paste0(td, "/", na)
tx <- "some text"
test_that("createFilesWriteText", {
    expect_null(createFilesWriteText(na, pa, tx, na, pa, tx, na, pa, tx))
}) # EOT

taPaName <- "dogPack"
taPaEnv <- ".doe"
taPaObj <- "stn"
taPaSH <- "dogPack_SH"
tmpl <- "_TEMPLATE"
where <- tempdir()
test_that("uniset_getFiles", {
    expect_type(uniset_getFiles(taPaName, taPaEnv, taPaObj, where, taPaSH, tmpl), "character")
    expect_output(uniset_getFiles(taPaName, taPaEnv, taPaObj, where, taPaSH, tmpl))
    expect_error(uniset_getFiles(taPaName, taPaEnv, taPaObj, where="~/blabla", taPaSH, tmpl))
    expect_error(uniset_getFiles(taPaName=1, taPaEnv, taPaObj, where, taPaSH, tmpl))
    expect_error(uniset_getFiles(taPaName, taPaEnv=c("a", "b"), taPaObj, where, taPaSH, tmpl))
}) # EOT

td <- tempdir()
pso <- "~/Documents/RPS/uniset_R/dopaem"
ptp <- paste0(td, "/dopaem")
R.utils::copyDirectory(pso, ptp)
test_that("uniset_copyFilesToPackage", {
    expect_null(uniset_copyFilesToPackage(ptp, taPaEnv="def", taPaObj="stn", taPaSH="def", tmpl= "_TEMPLATE"))
    expect_output(uniset_copyFilesToPackage(ptp, taPaEnv="def", taPaObj="stn", taPaSH="def", tmpl= "_TEMPLATE"))
    expect_error(uniset_copyFilesToPackage(ptp="~/blabla", taPaEnv="def", taPaObj="stn", taPaSH="def", tmpl= "_TEMPLATE"))
}) # EOT

########### the tricky part ######## -----

# siumlate the "dogPack"
.GlobalEnv$.dogPack_unisetEnv <- new.env()
.dogPack_unisetEnv$pkgUniset_UserPackageName <- "dogPack"
.dogPack_unisetEnv$pkgUniset_RenvironSettingsHomeName <- "dogPack_SH"
.dogPack_unisetEnv$pkgUniset_EnvironmentName <- ".doe"
.dogPack_unisetEnv$pkgUniset_SettingsObjectName <- "stn"
.dogPack_unisetEnv$pkgUniset_SuffixForTemplate <- "_TEMPLATE"
uev <- uniset_env_name <-  ".dogPack_unisetEnv"
test_that("uniset_test", {
    expect_type(uniset_test(uev), "list")
    expect_output(uniset_test(uev))
    expect_error(uniset_test(uev="blabla"))
}) # EOT

test_that("getUnisEnvirVariables", {
    expect_type(getUnisEnvirVariables(uev), "list")
    expect_error(getUnisEnvirVariables(uev="blabla"))
}) # EOT



test_that("uniset_updateSettings", {
    skip("not yet finished")
    expect_type(uniset_updateSettings(uev), "list")
    expect_output(uniset_updateSettings(uev))
    expect_error(uniset_updateSettings(uev="blabla"))
}) # EOT


