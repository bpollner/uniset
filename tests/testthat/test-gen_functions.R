 library(testthat)
#
######## the easy part #######-----
# ptp <- "~/Documents/RPS/uniset_R/uniset"
ptp <- path.package("uniset")
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


pso <- paste0(ptp, "/testHelpers/dopaem")
if (!file.exists(pso)) {
    pso <- paste0(ptp, "/inst/testHelpers/dopaem") # because the Cmd-check testing is using the **installed** package, while the cmd-shift-T is using the source structure for testing
}
file.copy(pso, td, recursive=TRUE)
ptp <- paste0(td, "/dopaem") # the path to package dopaem
# file.exists(ptp)
test_that("uniset_copyFilesToPackage", {
    expect_null(uniset_copyFilesToPackage(ptp, taPaEnv="def", taPaObj="stn", taPaSH="def", tmpl= "_TEMPLATE"))
    expect_output(uniset_copyFilesToPackage(ptp, taPaEnv="def", taPaObj="stn", taPaSH="def", tmpl= "_TEMPLATE"))
    expect_error(uniset_copyFilesToPackage(ptp="~/blabla", taPaEnv="def", taPaObj="stn", taPaSH="def", tmpl= "_TEMPLATE"))
}) # EOT


# here have an other template settings file (in td/dopaem/inst finally)  for better testing
a <- file.copy(paste0(pso, "/R/dogPack_settings.R"), paste0(ptp, "/inst"), overwrite = TRUE)

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

td <- tempdir()
tempSysHome <- "tempSysHome"
taPaName <- "dogPack"
fn_taPaSH <- "dogPack_SH"
fullPath <- paste0(td, "/", tempSysHome)
dogSH <- paste0(fullPath, "/", fn_taPaSH)
dir.create(fullPath, showWarnings=FALSE) # we create the tempSystemHome
test_that("checkCreateSHfolder", {
    expect_true(checkCreateSHfolder(fullPath, fn_taPaSH)) # now we create it
    expect_true(checkCreateSHfolder(fullPath, fn_taPaSH)) # nothing happening
    unlink(dogSH, TRUE)
    expect_message(checkCreateSHfolder(fullPath, fn_taPaSH)) # create again
    unlink(dogSH, TRUE)
    expect_true(checkCreateSHfolder(fullPath, fn_taPaSH)) # create again
    unlink(dogSH, TRUE)
    expect_false(checkCreateSHfolder("blabla", fn_taPaSH))
    expect_message(checkCreateSHfolder("blabla", fn_taPaSH))
    expect_true(checkCreateSHfolder(fullPath, fn_taPaSH)) # now we create ogain
}) # EOT

systemHome_R <- fullPath
fullRenvPath <- paste0(fullPath, "/.Renviron")
fn_taPaSH <- taPaSH <- "dogPack_SH"
taPaSH_creationMsg <- "creation Message"
addInfo <- "addInfo Message"
test_that("ifNotRenvExists", {
    expect_message(ifNotRenvExists(systemHome_R="blabla", fn_taPaSH, taPaSH, taPaSH_creationMsg, addInfo), "Sorry, the creation of the .Renviron file")
    expect_false(ifNotRenvExists(systemHome_R="blabla", fn_taPaSH, taPaSH, taPaSH_creationMsg, addInfo))
    expect_false(ifNotRenvExists(systemHome_R, fn_taPaSH, taPaSH, taPaSH_creationMsg, addInfo))
    unlink(fullRenvPath, TRUE)
    expect_message(ifNotRenvExists(systemHome_R, fn_taPaSH, taPaSH, taPaSH_creationMsg, addInfo), "The required '.Renviron' file")
}) # EOT

restartMsg <- "restart message"
test_that("taPaSH_System_missing", {
    expect_false(taPaSH_System_missing(systemHome_R, taPaName, taPaSH, fn_taPaSH, taPaSH_creationMsg, restartMsg, addInfo))
    expect_message(taPaSH_System_missing(systemHome_R, taPaName, taPaSH, fn_taPaSH, taPaSH_creationMsg, restartMsg, addInfo), restartMsg)
    # now we have to delete the contents of the .Renviron
    fcon <- file(fullRenvPath, open="w")
    newContent <- ""
    writeLines(newContent, fcon)
    close(fcon)
    expect_message(taPaSH_System_missing(systemHome_R, taPaName, taPaSH, fn_taPaSH, taPaSH_creationMsg, restartMsg, addInfo), taPaSH_creationMsg)
    # we leave with the .Renviron file filled with text
}) # EOT

taPaSH_system <- paste0(td, "/", tempSysHome, "/", taPaSH)
taPaSH_system_wrong <- aa <-  paste0(td, "/", tempSysHome, "/bliblablu")
test_that("taPaSH_System_OK_noDir", {
    expect_message(taPaSH_System_OK_noDir(systemHome_R, taPaSH, taPaSH_system, restartMsg), "Sorry, the path")
    expect_message(taPaSH_System_OK_noDir(systemHome_R, taPaSH, taPaSH_system=aa, restartMsg), restartMsg)
}) # EOT

setFiName <- "dogPack_settings.R"
taPaSettingsPath <- paste0(td, "/dopaem/inst/", setFiName)
taPaSH_system <- paste0(td, "/", tempSysHome, "/", fn_taPaSH)
test_that("pleaseCopyFreshSettings", {
    expect_true(pleaseCopyFreshSettings(taPaSettingsPath, taPaSH_system, setFiName))
    expect_message(pleaseCopyFreshSettings(taPaSettingsPath, taPaSH_system, setFiName), "Sorry, for unknown")
}) # EOT

test_that("pleaaseCopyAsTemplate", {
    expect_true(pleaaseCopyAsTemplate(taPaSettingsPath, taPaSH_system, setFiName))
}) # EOT
print("")
print("------------------------------------------------------------")
print("------------------------------------------------------------")
print("------------------------------------------------------------")
print("------------------------------------------------------------")

pathToPack <- paste0(td, "/dopaem/inst/", setFiName)
folderLocal <- paste0(td, "/", tempSysHome, "/", fn_taPaSH) # same as taPaSH_system above
nameLocal <- setFiName
tmpl <- "_TEMPLATE"

pt1 <-  paste0(td, "/dopaem/R/", "s_mod_plus.R")
msg1 <- "varAdd1, varAdd2, varAdd3"

pt11 <-  paste0(td, "/dopaem/R/", "s_mod_plus_my.R")
msg11 <- "myAdd01, myAdd02, myAdd1, myAddTNH, myAdd2, myAdd3, myAdd4"

pt12 <-  paste0(td, "/dopaem/R/", "s_mod_plus_my2.R")
msg12 <- "peter, paul, mary, newHeaderKey"

pt13 <-  paste0(td, "/dopaem/R/", "s_mod_plus_my3.R")
msg13 <- "changed, John"

pt10 <-  paste0(td, "/dopaem/R/", "s_mod_plus2.R")
msg10 <- "varAdd_0, varAdd1, varAdd2, varAdd3, varAdd4, varAdd5, varAdd6"

pt2 <-  paste0(td, "/dopaem/R/", "s_mod_minus.R")
msg2 <- "willBeDeleted1, willBeDeleted2, willBeDeleted3"

pt3 <-  paste0(td, "/dopaem/R/", "s_mod_plus_minus.R")
msg3 <- "comboAdd1, comboAdd2, comboAdd3"

pt4 <-  paste0(td, "/dopaem/R/", "s_block_new.R")
msg4 <- "newBlock1, newBlock2, newBlock3, below_newBlock4"

pt5 <-  paste0(td, "/dopaem/R/", "s_block_delete.R")
msg5 <- "block2, block2_2, comboDelete1, block2_oneMoreVariable"

pt6 <-  paste0(td, "/dopaem/R/", "s_block_delete2.R")
msg6 <- "block2, block2_2, comboDelete1, block2_oneMoreVariable, block3_giveMeaningfulNames, favouriteColor, willBeDeleted2, comboDelete2, comboDelete3"

pt7 <-  paste0(td, "/dopaem/R/", "s_block_delete3.R")
msg7 <- "block2, block2_2, comboDelete1, block2_oneMoreVariable, block4, blabla, andSoOn, petterson, findus, mouse"

cleanUp <- function(onTest=TRUE){
    unlink(paste0(dogSH, "/", setFiName)); a <- pleaseCopyFreshSettings(pathToPack, folderLocal, nameLocal, onTest) # go back to original
} # EOF
cleanUp()

test_that("checkFileVersionPossiblyModify - original", {
    expect_true(checkFileVersionPossiblyModify(pathToPack, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) ####
    cleanUp()
}) # EOT

test_that("checkFileVersionPossiblyModify - 1", {
    expect_message(checkFileVersionPossiblyModify(pt1, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg1) # change it
   	cleanUp()
}) # EOT

test_that("checkFileVersionPossiblyModify - 11, 12, 13", {
	# 11
    expect_message(checkFileVersionPossiblyModify(pt11, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg11) # change it
    expect_true(checkFileVersionPossiblyModify(pt11, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	# 12
    expect_message(checkFileVersionPossiblyModify(pt12, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg12) # change it
    expect_true(checkFileVersionPossiblyModify(pt12, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	# 13
    expect_message(checkFileVersionPossiblyModify(pt13, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg13) # change it
    expect_true(checkFileVersionPossiblyModify(pt13, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
}) # EOT

test_that("checkFileVersionPossiblyModify - 10, 2, 3", {
	#10
    expect_message(checkFileVersionPossiblyModify(pt10, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg10) # change it
    expect_true(checkFileVersionPossiblyModify(pt10, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
    #2
    expect_message(checkFileVersionPossiblyModify(pt2, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg2) # change it
    expect_true(checkFileVersionPossiblyModify(pt2, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
	#3
    expect_message(checkFileVersionPossiblyModify(pt3, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg3) # change it
    expect_true(checkFileVersionPossiblyModify(pt3, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
}) # EOT


test_that("checkFileVersionPossiblyModify - 4, 5, 6, 7", {
	#4
    expect_message(checkFileVersionPossiblyModify(pt4, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg4) # change it
    expect_true(checkFileVersionPossiblyModify(pt4, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
    #5
    expect_message(checkFileVersionPossiblyModify(pt5, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg5) # change it
    expect_true(checkFileVersionPossiblyModify(pt5, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
    #6
    expect_message(checkFileVersionPossiblyModify(pt6, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg6) # change it
    expect_true(checkFileVersionPossiblyModify(pt6, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
    #7
    expect_message(checkFileVersionPossiblyModify(pt7, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg7) # change it
    expect_true(checkFileVersionPossiblyModify(pt7, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
}) # EOT


######################################
aaa <- getUnisEnvirVariables(uev)
localSettingsPath <- paste0(taPaSH_system, "/", setFiName)
cleanUp()
test_that("checkSettings", {
    expect_true(checkSettings(aaa, onTest=TRUE, taPaSH_system, taPaSettingsPath, localSettingsPath, systemHome_R))
}) # EOT


# now clean up
unlink(paste0(td, "/dopaem"), TRUE)
unlink(paste0(td, "/", tempSysHome), TRUE)
unlink(paste0(td, "/", "UnisetFiles_R-pkg_'dogPack'"), TRUE)

