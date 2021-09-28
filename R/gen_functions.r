checkCh1 <- function(char, argName) {
	if (!all(is.character(char)) | length(char) != 1) {
		stop(paste0("Please provide a character length one to the argument '", argName, "'."), call.=FALSE)
	}
    return(invisible(NULL))
} # EOF

checkGetTaPaSH <- function(taPaSH, taPaName) {
	shSuff <- glob_settingsHomeSuffix # "_SH"
	#
	checkCh1(taPaSH, "taPaSH")
	checkCh1(taPaName, "taPaName")
	#
	if (taPaSH == "def") {
		return(paste0(taPaName, shSuff))
	} else {
		return(taPaSH)
	}
} # EOF

checkGetTaPaEnv <- function(taPaEnv, taPaName) {
	checkCh1(taPaEnv, "taPaEnv")
	if (taPaEnv == "def") {
		return(paste0(".", substr(taPaName, 1,2), "e"))
	} else {
		return(taPaEnv)
	} # end else
} # EOF

readInCharInput <- function(what) {
	maxTries <- 3
	ask <- TRUE
	aa <- 1
	txt <- paste0("Please provide a character length one to the argument '", what, "'.")
	if (what == "taPaName") {
		askIntro <- paste0("Please provide the name of the target-package, i.e. the name of the package that you want to enable to use package 'uniset': ")
	} # end if
	if (what == "taPaEnv") {
		askIntro <- paste0("Please provide a (short) name of the environment that will contain the object holding the settings, preferably starting with a '.' (dot): ")
	} # end if
	if (what == "taPaObj") {
		askIntro <- paste0("Please provide a (short) name of the object holding the settings, e.g. 'stn': ")
	} # end if
	#
	checkChoice <- function(charIn) {
		if (length(charIn) == 0) {
			message(txt)
			return(TRUE)
		} else {
			checkCh1(charIn, what)
		} # end else
		return(FALSE) # returns FALSE if all is ok
	} # EOiF
	#
	message(askIntro)
	while (ask) {
		charIn <- (scan(file = "", n = 1, quiet = TRUE, what="character"))
		ask <- checkChoice(charIn)
		aa <- aa+1
		if (aa > maxTries & ask == TRUE) {
			ask <- FALSE
			stop(paste0("Please try again"), call.=FALSE)
		} # end if
	} # end while ask
	return(charIn)
} # EOF

checkPath_Package_getName <- function(pathToPackage) {
	stdiFn_descr <- "DESCRIPTION"
	stdFin_ns <- "NAMESPACE"
	stdFon_R <- "R"
	stdVec <- c(stdiFn_descr, stdFin_ns, stdFon_R)
	packSkellName <- "anRpackage" # the name given when using the function package.skelleton() without providing a package name
	ptp <- pathToPackage
	#
	if (dir.exists(ptp)) {
		fls <- list.files(ptp)
		if (!all(stdVec %in% fls)) {
			stop(paste0("Sorry, the provided path '", ptp, "' does not seem to lead to a valid R-package structure."), call.=FALSE)
		} # end if
		descrFile <- paste0(ptp, "/",  stdiFn_descr)
		fcon <- file(descrFile, open="r")
		descrTxt <- readLines(fcon) # the full settings.r text from the package
		close(fcon)
		taPaName <- trimws(unlist(strsplit(descrTxt[1], ":"))[2]) # gives back the package name
		if (taPaName == packSkellName) {
			stop(paste0("Please provide your package with an other name than '", packSkellName, "'"), call.=FALSE)
		} # end if
	} else {
		stop(paste0("Sorry, the provided path '", ptp, "' does not lead to a folder."), call.=FALSE)
	} # end else
	return(taPaName)
} # EOF

printFinalCodeMessage <- function(taPaEnv, taPaObj, expSettingsName) {
  	msgChar <- paste0("\n\nYou will be able to access values of the file '", expSettingsName, "' via the code: \n")
  	codechar <- paste0("'", taPaEnv, "$", taPaObj, "$KEY'")
  	txt2 <- paste0("(With 'KEY' being any of the 'key=value' pairs defined in the file '", expSettingsName, "'.)")
	cat(msgChar)
	message(codechar)
	cat(txt2)
	return(invisible(NULL))
} # EOF

###############
readInReplaceTxtUnisFiles <- function(taPaName, taPaSH, taPaEnv, taPaObj, tmpl) {
	#
	unisEnvName <- glob_unisetEnvName  							# XXX_unisetEnv
	taPackName <- glob_targetPackageName 						# XXX_packageName
	taPackSHname <- glob_targetPackageSettingsHomeVarName		# XXX_SH
	taPackEnvName <- glob_targetPackageEnvName					# XXX_targetEnv
	taPackStnName <- glob_targetPackgae_settingsObjectName 		# XXX_obj
	taPackTmplSuff <- glob_targetPackage_templateSuffix 		# XXX_template
	taPackActualSett <- glob_actualSettingsName 				# XXX_actualSettingsName
	#
	unisetEnvSuffix <- glob_unisetEnvSuffix
	filnameSettings <- glob_filnameSettings
	templGlobals <- glob_templGlobals 		# "uniset_globals.R"
	templZZZ <- glob_templZZZ				# "zzz.R"
	templSettinsg <- glob_templSettinsg		# "settings.R"
	fileAdd <- "/templates/"				# the above three files will reside in the folder 'templates' in the installation of the uniset-package

	####### define paths ######
	aa <- path.package("uniset")
	if (dir.exists(paste0(aa, "/inst"))) { # so we are in a local dev mode and the templates are residing in the 'inst' folder of package root
		fileAdd <- "/inst/templates/"
	} # end if
	path_unis_globals <- paste0(aa, fileAdd, templGlobals)
	path_unis_zzz <- paste0(aa, fileAdd, templZZZ)
	path_unis_settingsTemplate <- paste0(aa, fileAdd, templSettinsg)
	#
	###### read in the three template files #######
	fcon <- file(path_unis_globals, open="r")
	globalsTxt <- readLines(fcon) # the full settings.r text from the package
	close(fcon)
	fcon <- file(path_unis_zzz, open="r")
	zzzTxt <- readLines(fcon) # the full settings.r text from the package
	close(fcon)
	fcon <- file(path_unis_settingsTemplate, open="r")
	settingsTxt <- readLines(fcon) # the full settings.r text from the package
	close(fcon)
	#

	####### replace text #########
	expSettingsName <- paste0(taPaName, "_", filnameSettings) # put the package name and 'settings.r' together
	thisUnisetEnvName <-  paste0(".", taPaName, unisetEnvSuffix) # the underscore '_' between is already above     # here we dictate the . in the beginning.
	#
	zzzTxt <-gsub(unisEnvName, thisUnisetEnvName, zzzTxt) # replace the uniset environment name with a package-prefix
	zzzTxt <-gsub(taPackName, taPaName, zzzTxt)	# fill in the provided target package name
	zzzTxt <-gsub(taPackSHname, taPaSH, zzzTxt) # settings home
	zzzTxt <-gsub(taPackEnvName, taPaEnv, zzzTxt) # target package environment name #  the .ap2 pendent
	zzzTxt <-gsub(taPackStnName, taPaObj, zzzTxt) # the object holding the keys. # the stn pendent
	zzzTxt <-gsub(taPackTmplSuff, tmpl, zzzTxt) # the object holding the keys. # the stn pendent
	#
	globalsTxt <- gsub(taPackName, taPaName, globalsTxt) # the package name
	globalsTxt <- gsub(unisEnvName, thisUnisetEnvName, globalsTxt)	# the name of the uniset environment where the package name etc. are defined.
	#
	settingsTxt <- gsub(taPackName, taPaName, settingsTxt) # the name of the target package
	settingsTxt <- gsub(taPackStnName, taPaObj, settingsTxt) # the name of the object holding the settings-list
	settingsTxt <- gsub(taPackEnvName, taPaEnv, settingsTxt) # the name of environment holding the object holding the settings-list
	settingsTxt <- gsub(taPackActualSett, expSettingsName, settingsTxt) # the name of settings file expanded with the name of the package
	#
	return(list(zzzTxt=zzzTxt, globalsTxt=globalsTxt, settingsTxt=settingsTxt))
} # EOF

createFilesWriteText <- function(zzzName, zzzPath, zzzTxt, globalsName, globalsPath, globalsTxt, settingsName, settingsPath, settingsTxt) {
	#
	ok <- file.create(zzzPath)
	if (!ok) {
		stop(paste0("Sorry, the file '", zzzName, "' could not be created."), call.=FALSE)
	} # end if
	ok <- file.create(globalsPath)
	if (!ok) {
		stop(paste0("Sorry, the file '", globalsName, "' could not be created."), call.=FALSE)
	} # end if
	ok <- file.create(settingsPath)
	if (!ok) {
		stop(paste0("Sorry, the file '", settingsName, "' could not be created."), call.=FALSE)
	} # end if
	#
	# write into the files
	fcon <- file(zzzPath, open="w")
	writeLines(zzzTxt, fcon) # write the zzz.r file
	close(fcon)
	#
	fcon <- file(globalsPath, open="w")
	writeLines(globalsTxt, fcon) # write globals file
	close(fcon)
	#
	fcon <- file(settingsPath, open="w")
	writeLines(settingsTxt, fcon) # write the settings file
	close(fcon)
	#
	return(invisible(NULL))
} # EOF

#' @title Get Uniset Files
#' @description Function to generate the three files required in the target
#' package (i.e. the package that should be enabled to use the package 'uniset').
#' If those arguments that default to 'NULL' are left at their default 'NULL',
#' their values are read in interactively.
#' @details The importance of providing rather short characters at the arguments
#' 'taPaEnv' and 'taPaObj' lies in the fact that those two names will be used to
#' get any parameter stored in the settings file of the target package. For example,
#' if you provide '.dpe' at the argument 'taPaEnv' and 'stn' at the argument
#' 'taPaObj', then all the values stored in the settings file can be retrieved by
#' calling \code{.dpe$stn$XXX}, with 'XXX' being any of the keys defined in the
#' settings file.
#' @param taPaName Character length one. The name of the target package.
#' @param taPaEnv Character length one. The name of the environment where the
#' settings for the target package (in a key=value format) will be stored. It is
#' recommended to use a rather short name starting with a '.' (dot). See details.
#' If left at the default 'def', the first two characters of the package name,
#' prepended with a '.' (dot), and appended with an 'e' (for environment) will
#' be used.
#' @param taPaObj Character length one. The name of the object (residing in the
#' environment with the name provided in argument 'taPaEnv') holding the list
#' with all the individual key-value pairs that can be defined to be used in the
#' target package. It is recommended to use a rather short name. See details.
#' Defaults to 'stn'.
#' @param where Character length one. The location where the folder with the
#' resulting three files should be copied to. Defaults to '~/desktop'.
#' @param taPaSH Character length one. The name of the variable to be defined in
#' the '.Renviron' file, leading to the place where the settings.r file for the
#' target package will be stored. If left at the default 'def', \code{taPaName_SH}
#' will be used, with 'taPaName' being the value provided at the argument
#' 'taPaName'.
#' @param tmpl Character length one. the Character string that will be appended
#' to the fresh settings file that is possibly copied (by the target package)
#' to the users settings home directory. Can be left at the default '_TEMPLATE'.
#' @return Creates a folder at the location specified at argument 'where' with
#' three files to be moved into the target package in it. Returns an (invisible)
#' character holding the path of the folder where the three files were written
#' into.
#' @seealso \code{\link{uniset_copyFilesToPackage}}
#' @examples
#' \dontrun{
#' # for an imaginary package called 'dogPack':
#' uniset_getFiles("dogPack", ".dpe", "stn")
#' 	# 'dpe' could be for 'dogPack Environment', 'stn' could be for 'settings'
#' 	# see details.
#' }
#' @export
uniset_getFiles <- function(taPaName=NULL, taPaEnv="def", taPaObj="stn", where="~/desktop", taPaSH="def", tmpl= "_TEMPLATE") {
	#
	folderPrev <- glob_folderPrev
	filenameZZZ <- glob_filenameZZZ
	filenameGlobals <- glob_filenameGlobals
	filnameSettings <- glob_filnameSettings # the '.r' gets appended below
	#

	########### possibly read in values ###########
	if (is.null(taPaName)) {
		taPaName <- readInCharInput("taPaName")
	} # end if
		if (is.null(taPaEnv)) {
		taPaEnv <- readInCharInput("taPaEnv")
	} # end if
	if (is.null(taPaObj)) {
		taPaObj <- readInCharInput("taPaObj")
	} # end if
	#
	checkCh1(taPaName, "taPaName")
	taPaEnv <- checkGetTaPaEnv(taPaEnv, taPaName)
	checkCh1(taPaObj, "taPaObj")
	checkCh1(tmpl, "tmpl")
	taPaSH <- checkGetTaPaSH(taPaSH, taPaName)
	checkCh1(tmpl, "tmpl")
	#

	##### read in and replace text #######
	aaa <- readInReplaceTxtUnisFiles(taPaName, taPaSH, taPaEnv, taPaObj, tmpl)
		zzzTxt <- aaa$zzzTxt
		globalsTxt <- aaa$globalsTxt
		settingsTxt <- aaa$settingsTxt
	#
	expSettingsName <- paste0(taPaName, "_", filnameSettings) # put the package name and 'settings.r' together
	#

	####### write to file ##########
	# check create folder
	folderName <- paste0(folderPrev, taPaName, "'")
	folderPath <- paste0(where, "/", folderName)
	if (dir.exists(folderPath)) {
		fls <- list.files(folderPath)
		if (length(fls) != 0) {
			ok <- file.remove(paste0(folderPath, "/", fls))
			if (!all(ok)) {
				stop(paste0("Sorry, there was a problem when trying to remove files in the folder '", folderPath, "'."), call.=FALSE)
			} # end if !ok
		} # end if
	} else { # so the folder path is not existing
		ok <- dir.create(folderPath, showWarnings=FALSE)
		if (!ok) {
			stop("Sorry, the folder could not be created", call.=FALSE)
		} # end if !ok
	} # end else
	# now we can be sure to have an empty folder
	#

	#### create files and write text into them #######
	zzzPath <- paste0(folderPath, "/", filenameZZZ)
	globalsPath <- paste0(folderPath, "/", filenameGlobals)
	settingsPath <- paste0(folderPath, "/", expSettingsName)
	#
	createFilesWriteText(filenameZZZ, zzzPath, zzzTxt, filenameGlobals, globalsPath, globalsTxt, expSettingsName, settingsPath, settingsTxt)
	##

	allfns <- c(filenameZZZ, filenameGlobals, expSettingsName)
	cat(paste0("Three files called \n'", paste0(allfns, collapse="'\n'"), "\nhave been written to the folder \n'", folderPath, "'"))
 	cat("\n")
 	cat("Please move these three files into their resp. target folders (see ?uniset, or have a look at the content of the three generated files")
  	printFinalCodeMessage(taPaEnv, taPaObj, expSettingsName)
	return(invisible(folderPath))
} # EOF

#' @title Copy Uniset Files into Target Package
#' @description Function to generate the three files required in the target
#' package (i.e. the package that should be enabled to use the package 'uniset').
#' The generated files will be copied directly into their required destination
#' folders in the target package. The name of the target package will be extracted
#' from the description file.
#' @details The importance of providing rather short characters at the arguments
#' 'taPaEnv' and 'taPaObj' lies in the fact that those two names will be used to
#' get any parameter stored in the settings file of the target package. For example,
#' if you provide '.dpe' at the argument 'taPaEnv' and 'stn' at the argument
#' 'taPaObj', then all the values stored in the settings file can be retrieved by
#' calling \code{.dpe$stn$XXX}, with 'XXX' being any of the keys defined in the
#' settings file.
#' @param pathToPackage Character length one. The path to the root of the
#' target package.
#' @inheritParams uniset_getFiles
#' @return Writes the three required files directly into a valid R-package folder
#' structure. Returns (invisible) NULL.
#' @seealso \code{\link{uniset_getFiles}}
#' @examples
#' \dontrun{
#' # for an imaginary package called 'dogPack':
#' aa <- "~/desktop/dogPack"
#' uniset_copyFilesToPackage(aa) # uses the defaults for all arguments
#' uniset_copyFilesToPackage(aa, ".dpe", "sn")
#' 	# see details.
#' }
#' @export
uniset_copyFilesToPackage <- function(pathToPackage, taPaEnv="def", taPaObj="stn", taPaSH="def", tmpl= "_TEMPLATE") {
	#
	filenameZZZ <- glob_filenameZZZ
	filenameGlobals <- glob_filenameGlobals
	filnameSettings <- glob_filnameSettings # the '.r' gets appended below
	zzzAdd <- "_2.R"
	#

	#### check and get names ######
	taPaName <- checkPath_Package_getName(pathToPackage) # stops if path is not good
	taPaEnv <- checkGetTaPaEnv(taPaEnv, taPaName)
	checkCh1(taPaObj, "taPaObj")
	taPaSH <- checkGetTaPaSH(taPaSH, taPaName)
	checkCh1(tmpl, "tmpl")
	#

	##### read in and replace text #######
	aaa <- readInReplaceTxtUnisFiles(taPaName, taPaSH, taPaEnv, taPaObj, tmpl)
		zzzTxt <- aaa$zzzTxt
		globalsTxt <- aaa$globalsTxt
		settingsTxt <- aaa$settingsTxt
	#
	expSettingsName <- paste0(taPaName, "_", filnameSettings) # put the package name and 'settings.R' together
	#

	########## check / remove files #############
	folderName <- basename(pathToPackage)
	folderPath <- pathToPackage
	pathToR <- paste0(folderPath, "/R")
	paToInst <- paste0(folderPath, "/inst")
	if (dir.exists(paToInst)) {
		fls <- list.files(paToInst)
		if (expSettingsName %in% fls) {
			ok <- file.remove(paste0(paToInst, "/", expSettingsName))
			if (!ok) {
				stop(paste0("Sorry, there was a problem when trying to remove the previously generated file '", expSettingsName, "' in the folder 'inst'."), call.=FALSE)
			} # end if
		} # end if
	} else { # so the folder "inst" is not existing
		ok <- dir.create(paToInst, showWarnings=FALSE)
		if (!ok) {
			stop("Sorry, the folder 'inst' could not be created", call.=FALSE)
		} # end if !ok
	} # end else
	# check / remove zzz.r and globals.r
	fls <- list.files(pathToR)
	if (length(fls) != 0) { # check if already there
		if (filenameZZZ %in% fls) { # se we already got a file called zzz.R, we must not overwrite or remove it
			oldZZZname <- filenameZZZ
			filenameZZZ <- paste0(substr(filenameZZZ, 1, nchar(filenameZZZ)-2), zzzAdd)
			msg <- paste0("It seems that there already is a file called '", oldZZZname, "' in the 'R' folder of your package. \nIn case there is already an '.onLoad' function defined, please add the six lines of code from the file '", filenameZZZ, "' to your existing '.onLoad' function.")
			message(msg)
		} # end if zzz already here
		if (filenameZZZ %in% fls) { # now this would be zzz_2.R an other #2 already here -- this one must go
			ok <- file.remove(paste0(pathToR, "/", filenameZZZ))
			if (!ok) {
				stop(paste0("Sorry, there was a problem when trying to remove the previously generated file '", filenameZZZ, "' in the folder 'R'."), call.=FALSE)
			} # end if
		} # end if
		if (filenameGlobals %in% fls) { # remove it
			ok <- file.remove(paste0(pathToR, "/", filenameGlobals))
			if (!ok) {
				stop(paste0("Sorry, there was a problem when trying to remove the previously generated file '", filenameGlobals, "' in the folder 'R'."), call.=FALSE)
			} # end if
		} # end if
	} # end if length(fls) != 0
	# now we are sure to have a) the inst folder present, and b) none of our three files present
	#

	#### create files and write text into them #######
	zzzPath <- paste0(pathToR, "/", filenameZZZ)
	globalsPath <- paste0(pathToR, "/", filenameGlobals)
	settingsPath <- paste0(paToInst, "/", expSettingsName)
	#
	createFilesWriteText(filenameZZZ, zzzPath, zzzTxt, filenameGlobals, globalsPath, globalsTxt, expSettingsName, settingsPath, settingsTxt)
	##

	cat(paste0("A file called '", expSettingsName, "' has been written into the 'inst' folder, \ntwo files called '", filenameZZZ, "' and '", filenameGlobals, "' have been written into the 'R' folder of the package '", folderName, "' at \n'", folderPath, "'."))
  	printFinalCodeMessage(taPaEnv, taPaObj, expSettingsName)
	return(invisible(NULL))
} # EOF

#' @title Simple Test
#' @description Test if your input regarding environment name, package name
#' etc. was correct / successful. This function is meant to be called from the
#' target package. See examples.
#' @param unisetEnv Character length one. The name of the environment holding the uniset
#' definitions for a single package.
#' @return An (invisible) list.
#' @examples
#' \dontrun{
#' targetPackageTest <- function() { # this function is defined in the target package
#'     uniset::uniset_test(get("uev"))
#' } # eof
#' }
#' @export
uniset_test <- function(unisetEnv) {
	env <- get(unisetEnv) # gives back the environment
#	aa <- ls(env); print(aa); print("------------")
	taPaName <- get("pkgUniset_UserPackageName", envir=env)
	taPaEnv <- get("pkgUniset_EnvironmentName", envir=env)
	taPaSH <- get("pkgUniset_RenvironSettingsHomeName", envir=env)
	taPaObj <- get("pkgUniset_SettingsObjectName", envir=env)
	tmpl <- get("pkgUniset_SuffixForTemplate", envir=env)
	#
	out <- list(targetPackageName=taPaName, targetPackageEnvironment=taPaEnv, targetPackageSettingsHomeVariableName=taPaSH, targetPackageSettingsObjectName=taPaObj, suffuxForTemplates=tmpl)
	print(utils::str(out))
	return(invisible(out))
} # EOF
####################################################################
getCheckForDoubleNames <- function(pathToLocal, pathToPack, pmu) {
	#
	checkIt <- function(charVec, what) {
		aa <- length(charVec)
		bb <- length(unique(charVec))
		if (bb < aa) {
			rl <- rle(sort(charVec)) # but they already come in sorted *
			ind <- which(rl$lengths > 1)
			vals <- rl$values[ind]
			stop(paste0("Sorry, the keys '", paste0(vals, collapse="', '"), "' ", "seem to appear more than once in the ", what, "settings.R file."), call.=FALSE)
		} # end if
	} # EOIF
	##
	lenv <- new.env()
	sys.source(pathToLocal, envir=lenv)
	txt <- paste0("sort(names(lenv", pmu, "))") # *
	locNames <- eval(parse(text=txt))
	penv <- new.env()
	sys.source(pathToPack, envir=penv)
	txt <- paste0("sort(names(penv", pmu, "))") # *
	pacNames <- eval(parse(text=txt))
	#
	checkIt(locNames, "local ")
	checkIt(pacNames, "package ")
	#
	return(list(locNames=locNames, pacNames=pacNames))
} # EOF

getStnCenter <- function(pac, taPaObj) {
		fconPack <- file(pac, open="r")
		ftPack <- readLines(fconPack)   # read in the pac file
		close(fconPack)
		indPm <- which(startsWith(trimws(ftPack), taPaObj)) # on which line number is the "stn" object
		indBracket <- which(startsWith(trimws(ftPack), ")"))
		return(ftPack[indPm:indBracket])
} # EOF

getKeysOnlyFromText <- function(ftXX, splitChar, taPaObj) {
	ftXXr <- unlist(lapply(strsplit(trimws(ftXX), splitChar), function(x) trimws(x[1])))
	ftXXr[is.na(ftXXr)] <- "" # replace NAs with empty character. NAs were those that did not have a key = value pair. (--> an "=")
	ftXXr[1] <- taPaObj
	return(ftXXr)
} # EOF

getMissingKVPs <- function(ftPack, ftPackR, missingKeys) {
	out <- character(length(missingKeys))
	for (i in 1: length(missingKeys)) {
		out[i] <- ftPack[which(ftPackR == missingKeys[i])]
	} # end for i
	return(out)
} # EOF

getTxtsBetweenLocal <- function(ftLocal, ftLocalR, indLocHook, indLocNext, ftPack, ftPackR, indKey, maxS) {
# 	indKey is in ftPack/R dimension
#	txtBetweenLocal: tbl
#	txtBetweenLocalUpper: tbl_U
#	txtBetweenLocalLower: tbl_L
	#
	if (indLocHook+1 == indLocNext) { # so there is no between text
		return(list(tbl=NULL, tbl_U=NULL, tbl_L=NULL)) # direct insertion between two adjacent lines
	} # end if
	#
	tbl <- ftLocal[(indLocHook+1):(indLocNext-1)]
	######
	indPacHook <- which(ftPackR == ftLocalR[indLocHook])
	if (indPacHook+1 == indKey) { # there is no space above; the new key is directly below the hook
		return(list(tbl=tbl, tbl_U=NULL, tbl_L=tbl))
	} # end if
	#
	indPacNext <- which(ftPackR == ftLocalR[indLocNext])
	if (indPacNext-1 == indKey) { # there is no space below, the new key is directly above the next
		return(list(tbl=tbl, tbl_U=tbl, tbl_L=NULL))
	} # end if
	#######
	if (all(trimws(tbl) == "")) {
		return(list(tbl=tbl, tbl_U=tbl, tbl_L=NULL)) # classic case for new block
	} # end if
	if (all(trimws(tbl) != "")) {
		return(list(tbl=tbl, tbl_U=rep("", maxS), tbl_L=tbl)) # can be either or (top or bottom, no way to know). But we are "somewhere in the middle", so insert empty lines above
	} # end if
	# by now tbl has to be longer than 1
	# and by now the new key HAS to be "somewhere in the middle"
	indFirstSpace <- which(trimws(tbl) == "")[1] # so the text could belong more to below
	if (indFirstSpace == 1) {
		return(list(tbl=tbl, tbl_U=rep("", maxS), tbl_L=tbl))
	} # end if
	#
	tbl_U <- tbl[1:(indFirstSpace-1)]
	tbl_L <- tbl[indFirstSpace: length(tbl)]
	return(list(tbl=tbl, tbl_U=c(tbl_U, rep("", maxS)), tbl_L=tbl_L)) # because we have to be "in the middle" somewhere
} # EOF

getTextBetweenPac <- function(pacNames, singleMissingKey, ftPackR, ftPack, indKey, ftLocal) {
	pacHookAbove <- pacNames[which(pacNames == singleMissingKey)-1]
	indPacHook <- which(ftPackR == pacHookAbove)
	keyIndFT <- which(ftPackR == singleMissingKey)
	if (indPacHook == (keyIndFT-1)) { # so there is no space between the two keys
		return(NULL)
	} # end if
	#
	txtBetweenPac <- ftPack[(indPacHook+1):(indKey-1)]
	# cut away everything above that is above an empty line
	txtT <- trimws(txtBetweenPac)
	if (all(txtT == "")) {
		return(NULL)
	} # end if
	aa <- max(which(txtT == "")) 	# get the index of the last empty line
	if (aa == length(txtBetweenPac)) { # so if the last line is empty, that means we do not have text directly above the key
		return(NULL)
	} # end if
	out <- txtBetweenPac[(aa+1):(length(txtBetweenPac))]
	ind <- which(out %in% ftLocal)
	if (length(ind) != 0) {
		out <- out[-ind] # make sure that we do not copy anything that is already in the local file
	} # end if
	if (length(out) == 0) {
		out <- NULL # just to be sure
	} # end if
	return(out)
} # EOF

getTxtEmptyBelowPacKey <- function(indKey, pacNames, ftPack, ftPackR, ftLocal) { # indKey is in ftPackR-dimension
	aa <- which(pacNames == ftPackR[indKey])+1
	indPacNext <- which(ftPackR == pacNames[aa])
	if (indKey+1 == indPacNext) { # so there is no space between
		return(NULL)
	} # end if
	txtPackBelowKey <- ftPackR[(indKey+1):(indPacNext-1)]
	txtPackBelowKey_Full <- ftPack[(indKey+1):(indPacNext-1)]

	if (all(txtPackBelowKey == "")) {
		return(txtPackBelowKey)
	} # end if
	if (all(txtPackBelowKey != "")) {
		return(NULL)
	} # end if
	rl <- rle(txtPackBelowKey == "")
	if (rl$values[1]) { # we have some empty lines as first block
		return(rep("", rl$lengths[1]))
	} else {
		txt <- txtPackBelowKey_Full[1:rl$lengths[1]] # first get the characters
		aa <- which(txt %in% ftLocal)
		txt <- txt[-aa]
		empty <- rep("", rl$lengths[2]) # then get the empty lines
		return(c(txt, empty))
	} # end else
 	return("\t# something went wrong...") # we should never get here
} # EOF

getMaxSpace <- function(ftLocal) {
	aa <- trimws(ftLocal)
	rl <- rle(aa=="")
	out <- max(rl$lengths[rl$values])
	return(out)
} # EOF

reduceEmptyLines <- function(ftLocal, maxS, maxSD) { # maxSD is after deleting (is the higher one)
	# find the indices where the too big space maxSD is
	if (maxSD > maxS) { # so we have to cut down on big empty spaces
		ftT <- trimws(ftLocal)
		rl <- rle(ftT == "")
		aboveSIndRL <- which(rl$lengths > maxS & rl$values)
		indDelOut <- NULL
		for (i in 1: length(aboveSIndRL)) {
			ind <- aboveSIndRL[i]
			thisLeng <- rl$lengths[ind]
			cutAwayLeng <- thisLeng - maxS
			maxIndTxt <- sum(rl$lengths[1:ind])
			minIndTxt <- maxIndTxt - cutAwayLeng +1
			indDelOut <- c(indDelOut, (minIndTxt : maxIndTxt)) # cut down delete to the max empty lines of before deletion
		} # end for i
		return(ftLocal[-indDelOut])
	} else {
	return(ftLocal) # nothing to cut away, return the original
	}
} # EOF

tellKeyAddDelete <- function(keys, folderLocal, nameLocal, what="add") {
	if (what == "add") {
		whatTxt <- " added to"
	} else {
		whatTxt <- " deleted from"
	}
	if (length(keys) > 1) {
		plS <- "s"; plC <- " were"
	} else {
		plS <- ""; plC <- " was"
	}
	cat(paste0("The following ", length(keys), " key", plS, plC, whatTxt, " the settings-file '", nameLocal, "' in \n'", folderLocal, "':\n\t", paste0(keys, collapse=", "), "\n\n"))
} # EOF

addMissingKeys <- function(ftLocal, splitChar, taPaObj, pathToPack, folderLocal, nameLocal, pacNames, locNames, maxS) {
	missingKeys <- pacNames[which(!pacNames %in% locNames)]
	if (length(missingKeys != 0)) { # so we do have to add something
		ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar, taPaObj)
		ftPack <- getStnCenter(pathToPack, taPaObj)
		ftPackR <- getKeysOnlyFromText(ftPack, splitChar, taPaObj)
		missingKVPs <- getMissingKVPs(ftPack, ftPackR, missingKeys) # all the missing key-values pairs in one object
		for (i in 1: length(missingKeys)) {
			indKey <- which(ftPackR == missingKeys[i])  # the pac index of a key missing in loc
			if (length(indKey) > 1) {stop("Sorry, it seems that a key name appears twice.", call.=FALSE)}
			newKVP <- missingKVPs[i]

			# now look up for the next hook
			pacHook <- pacNames[which(pacNames == missingKeys[i])-1]
			locHookKeyInd <- which(locNames == pacHook)
			locHook <- locNames[locHookKeyInd] # the name of the key in loc one higher than the missing one

			# get the comments above and empty spaces below the pacHook (if there are any)
			txtBetweenPack <- getTextBetweenPac(pacNames, missingKeys[i],ftPackR, ftPack, indKey, ftLocal)
			txtEmptyBelowPacKey <- getTxtEmptyBelowPacKey(indKey, pacNames, ftPack, ftPackR, ftLocal)

			# get all local lines between hook and next
			indLocHook <- which(ftLocalR == locHook) # next higher hook in local file !!
			locNext <- locNames[which(locNames == locHook)+1] # the name of the next key present in the local file
			indLocNext <- which(ftLocalR == locNext)
			aa <- getTxtsBetweenLocal(ftLocal, ftLocalR, indLocHook, indLocNext, ftPack, ftPackR, indKey, maxS) # here it is decided where in the between text the new KVP is put
				txtBetweenLocal <- aa$tbl
				txtBetweenLocalUpper <- aa$tbl_U
				txtBetweenLocalLower <- aa$tbl_L
			txtUpper <- ftLocal[(1):(indLocHook)] # !!!!! changes this if pacHook == taPaObj.
			txtLower <- ftLocal[(indLocNext):length(ftLocal)]
			#
			# put together the text, add to locNames etc. as well.
			ftLocal <- c(txtUpper, txtBetweenLocalUpper, txtBetweenPack, newKVP, txtEmptyBelowPacKey, txtBetweenLocalLower, txtLower) ## CORE ##
			ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar, taPaObj) # could be removed (is done at the top of the loop already)
			locNames <- c(locNames[1:locHookKeyInd], missingKeys[i], locNames[(locHookKeyInd+1):length(locNames)])
			if (FALSE) {
		#		print(txtUpper);
				print(locHook)
				print(txtBetweenLocalUpper);
				print(txtBetweenPack);
				print(newKVP);
		#		print(txtEmptyBelowPacKey);
		#		print(txtBetweenLocalLower);
				print(locNext);
		#		print(txtLower)
		#		#
		#		print(ftLocal)
				print("---------------------------");
	  			wait()
			} # end if TRUE  # dev helpers, is printing things.
		} # end for i (going through missing keys)
		maxSD <- getMaxSpace(ftLocal) # the max space after adding keys
		ftLocal <- reduceEmptyLines(ftLocal, maxS, maxSD)
		#
		tellKeyAddDelete(missingKeys, folderLocal, nameLocal, what="add")
	} ########### end if length(missingKeys) != 0 # until here, things were added. OR not.
	return(ftLocal)
} # EOF

deleteSurplusKeys <- function(folderLocal, nameLocal, ftLocal, splitChar, taPaObj, locNames, pacNames, maxS) {
	surplusKeys <- locNames[which(!locNames %in% pacNames)]
	if (length(surplusKeys != 0)) { # so we do have to delete something
		ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar, taPaObj) # the incoming ftLocal is "stn" only, and was possibly modified above in the additions
		for (i in 1: length(surplusKeys)) { # we are collecting possible single line comments above a block
			indKey <- which(ftLocalR == surplusKeys[i])
			aa <- trimws(ftLocal) # so that tabs etc go to ""
			ikm <- NULL
			if ( (aa[indKey-1] != "")  & (aa[indKey-2] == "") ) { # that means we have a single line of comment above a key
				ikm <- indKey-1 # ikm: index key minus
				if (ftLocalR[ikm] %in% locNames) { # now this above could be a key, check....
					ikm <- NULL
				} # end if
				if (ftLocalR[indKey+1] %in% locNames) { # means we have an other key directly below the one to be deleted, so we will *not* delete the one comment line above
					ikm <- NULL
				} # end if
			} # end if
			indDel <- c(indKey, ikm)
			ftLocal <- ftLocal[-indDel] # delete here #### ******************
			ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar, taPaObj)
		} # end for i going through surplusKeys
		#
		maxSD <- getMaxSpace(ftLocal) # the max space after deleting keys
		ftLocal <- reduceEmptyLines(ftLocal, maxS, maxSD)
		#
		tellKeyAddDelete(surplusKeys, folderLocal, nameLocal, what="delete")
		ind <- which(locNames %in% surplusKeys)
		locNames <- locNames[-ind]
	} # end if (length(surplusKeys != 0))
	return(list(ftLocal=ftLocal, locNames=locNames))
} # EOF

checkFileVersionPossiblyModify <- function(pathToPack, folderLocal, nameLocal, pm=NULL, tmpl, taPaName=NULL){
	pv_suffixForTemplates <- tmpl
	taPaObj <- pm
	splitChar <- "="
	#
	loc <- pathToLocal <- paste0(folderLocal, "/", nameLocal)
	pac <- pathToPack
	if (is.null(pm)) {
		pmu <- ""
	} else {
		pmu <- paste0("$", taPaObj)
	}
	aa <- getCheckForDoubleNames(pathToLocal, pathToPack, pmu) # is checking for non-unique keys
	if (identical(aa$locNames, aa$pacNames)) {
		return(invisible(TRUE))
	} # end if identical

	# we only continue, if the locNames and pacNames are NOT identical

	######## first we will ADD any possible keys
	# get the name of the keys that are missing in local / added in pathToPack
	lenv <- new.env()
	sys.source(pathToLocal, envir=lenv)
	txt <- paste0("names(lenv", pmu, ")") # NOT sorted
	locNames <- c(taPaObj, eval(parse(text=txt))) # add taPaObj as first in case of a first key is introduced. Need a hook then.
	penv <- new.env()
	sys.source(pathToPack, envir=penv)
	txt <- paste0("names(penv", pmu, ")") # NOT sorted
	pacNames <- c(taPaObj, eval(parse(text=txt)))
	#
	#get parts before and after the list (TaPaPbj)
	fconPack <- file(pathToPack, open="r")
	ftPack <- readLines(fconPack)   # read in the pac file
	close(fconPack)
	fconLocal <- file(pathToLocal, open="r")
	ftLocal <- ftLocalBackup <- readLines(fconLocal)   # read in the local file
	close(fconLocal)
	indPm <- which(startsWith(trimws(ftLocal), taPaObj)) # on which line number is the "stn" object
	indBracket <- which(startsWith(trimws(ftLocal), ")"))
	txtAbove <- ftLocal[1:(indPm-1)] # get the txtAbove and txtBelow from the local file. The user could have written something in there that should stay.
	txtBelow <- ftLocal[(indBracket+1):length(ftLocal)]
	#
	ftLocal <- getStnCenter(pathToLocal, taPaObj) # might need that in the deletions. !!! gets possibly modified in the additions below
	maxS <- getMaxSpace(ftLocal) # get the maximum number of continuous empty lines
	#######
	aa <- deleteSurplusKeys(folderLocal, nameLocal, ftLocal, splitChar, taPaObj, locNames, pacNames, maxS) # ***************
		ftLocal <- aa$ftLocal
		locNames <- aa$locNames # locNames have to updated due a possible deletion of keys
	ftLocal <- addMissingKeys(ftLocal, splitChar, taPaObj, pathToPack, folderLocal, nameLocal, pacNames, locNames, maxS) # **************
	#######
	# now write into local settings file
	fconLocal <- file(loc, open="w")
	writeLines(c(txtAbove, ftLocal, txtBelow), fconLocal) # write the new file to settings.r in pathSH
	close(fconLocal)
	#
	# just to be sure, check again
	aa <- getCheckForDoubleNames(pathToLocal, pathToPack, pmu)
	if (!identical(aa$locNames, aa$pacNames)) { # this should never happen
		message("Sorry, for unknown reasons the keys in the settings file could not be updated.")
		fconLocal <- file(loc, open="w")
		writeLines(ftLocalBackup, fconLocal) # write the backup file to settings.r in pathSH
		close(fconLocal)
		cat(paste0("A template containing the required key-value pairs will be made available.\n"))
		pleaaseCopyAsTemplate(pathToPack, folderLocal, nameLocal)
		return(invisible(FALSE))
	} # end if identical
	#
	return(invisible(TRUE))
} # EOF
####################################################################
checkCreateSHfolder <- function(systemHome, fn_taPaSH) {
	if (!dir.exists(paste0(systemHome, "/", fn_taPaSH))) {
		dirCreaOk <- dir.create(paste0(systemHome, "/", fn_taPaSH), showWarnings=FALSE)
		if (!dirCreaOk) {
			msg <- paste0("Sorry, the required settings-home directory `", fn_taPaSH, "` could not be created in `", systemHome, "`.")
			message(msg)
			return(FALSE)
		} else { # so we created the .Renviron file AND created the taPaSH folder
			msg <- paste0("The folder `", fn_taPaSH, "` as settings-home directory has been created in `", systemHome, "`.")
			message(msg)
			return(TRUE)
		}
	} # end if !dir.exists taPaSH
	return(TRUE)
} # EOF

ifNotRenvExists <- function(systemHome_R, fn_taPaSH, taPaSH, taPaSH_creationMsg, addInfo) {
	 # we have NO .Renviron file, so we simply make one
	fullRenvPath <- paste0(systemHome_R, "/.Renviron")
	createOK <- file.create(fullRenvPath, showWarnings=FALSE)
	if (!createOK) {  #  if .Renviron could not be created
		msg <- paste0("Sorry, the creation of the .Renviron file in `", systemHome_R, "` failed.")
		message(msg)
		return(FALSE)
	} else { # so we could create the .Renviron file
		# if no .Renviron file, then also no settings home diretory --> create one
		ok <- checkCreateSHfolder(systemHome_R, fn_taPaSH)
			if (!ok) {
				return(FALSE)
			}
		# now we have to fill the newly created .Renviron file and point taPaSH to the newly created folder
		defaultFillForRenviron <- paste0("\n\n", taPaSH, " = ", systemHome_R, "/", fn_taPaSH) # here problem in windows !! # really? I do not (in windows)
		fcon <- file(fullRenvPath, open="w")
		writeLines(defaultFillForRenviron, fcon)
		close(fcon)
		creMsg <- paste0("The required '.Renviron' file in '", systemHome_R, "' has been created for you.\n", taPaSH_creationMsg, "\n", addInfo)
		message(creMsg)
		return(FALSE)
	} # end else (where we could create and fill the .Renviron file and create the settings home folder
} # EOF

taPaSH_System_missing <- function(systemHome_R, taPaName, taPaSH, fn_taPaSH, taPaSH_creationMsg, restartMsg, addInfo) {
	# so it is not existing in the system, and we have to check if it exists in the .Renviron file
	fullRenvPath <- paste0(systemHome_R, "/.Renviron")
	fcon <- file(fullRenvPath, open="r")
	content <- readLines(fcon)
	close(fcon)
	#
	if (any(grepl(taPaSH, content))) { # returns TRUE if taPaSH is present in the .Renviron file
		# so not in the system, but on the file --> that means we have to restart R
		message(restartMsg)
		return(FALSE)
	} else { # so not in the system, and not on the file (but the .Renviron was present
		# now we have to ADD the taPaSH to the existing .Renviron file
		# first check for existence / create the settings home folder
		ok <- checkCreateSHfolder(systemHome_R, fn_taPaSH)
		if (!ok) {
			return(FALSE)
		}
		fcon <- file(fullRenvPath, open="r+b")
		content <- readLines(fcon)
		newContent <- c(content, paste0("\n\n## ", taPaName, ":"), paste0(taPaSH, " = ", systemHome_R, "/", fn_taPaSH), "\n") # was c(content, "\n\n## aquap2", paste0("AQUAP2SH = ", systemHome, "/", fn_taPaSH), "\n")
		writeLines(newContent, fcon)
		close(fcon)
		msg <- paste0(taPaSH_creationMsg, "\n", addInfo)
		message(msg)
		return(FALSE)
	} # end else
} # EOF

taPaSH_System_OK_noDir <- function(systemHome_R, taPaSH, taPaSH_system, restartMsg) {
	# first check if the content of taPaSH in the file and in the system are the same
	fullRenvPath <- paste0(systemHome_R, "/.Renviron")
	fcon <- file(fullRenvPath, open="r")
	content <- readLines(fcon)
	close(fcon)
	taPaSH_file <- content[which(grepl(taPaSH, content))] # get only the one string that is the taPaSH
#	print(taPaSH_file)
	fileValue <- trimws(strsplit(taPaSH_file, "=")[[1]][[2]]) # the [[1]] to get out of the list. naja.
#	print(fileValue)
	if (fileValue != taPaSH_system) { # so the content of taPaSH is different in the system and in the file, we have to restart R
		message(restartMsg)
		return(FALSE)
	} # end if
	msg <- paste0("Sorry, the path `", taPaSH_system, "` specified in the `", taPaSH,"` variable is not pointing to a valid directory.\nPlease change the value of `", taPaSH, "` in the .Renviron file (`", fullRenvPath, "`), or create the appropriate file structure.")
	message(msg)
	return(FALSE)
} # EOF

pleaaseCopyAsTemplate <- function(taPaSettingsPath, taPaSH_system, setFiName) {
		tmpl <- "_TEMPLATE.R"
		#
		td <- tempdir()
		ok <- file.copy(taPaSettingsPath, td, overwrite = TRUE)
		nfn <- paste0(td, "/", setFiName)
		nfn_T <- paste0(nfn, tmpl)
		ok <- file.rename(nfn, nfn_T)
		ok <- file.copy(nfn_T, taPaSH_system, overwrite = TRUE)
		unlink(nfn_T)
		if (!ok) {
			message("Sorry, for unknown reasons the required template file could not be copied.\n")
			return(invisible(FALSE))
		} else {
		message(paste0("The '", paste0(setFiName, tmpl), "' file has been copied into `", taPaSH_system, "`."))
		return(invisible(TRUE))
		} # end else
} # EOF

pleaseCopyFreshSettings <- function(taPaSettingsPath, taPaSH_system, setFiName) {
	# please simply copy the settings
	ok <- file.copy(taPaSettingsPath, taPaSH_system)
	if (!ok) {
		message(paste0("Sorry, for unknown reasons it was not possible to copy the `", setFiName, "` file from `", taPaSettingsPath, "` to `", taPaSH_system, "`."))
		return(FALSE)
	} else { # so we could copy the settings.r file
		message(paste0("The '", setFiName, "' file has been copied into `", taPaSH_system, "`."))
		return(TRUE)
	} # end else
} # EOF

checkSettings <- function(taPaList, onTest=FALSE, taPaSH_system=NULL, taPaSettingsPath=NULL, localSettingsPath=NULL) {
	#
	aaa <- taPaList
		taPaName <- aaa$taPaName
		taPaEnv <- aaa$taPaEnv
		taPaSH <- aaa$taPaSH
		taPaObj <- aaa$taPaObj
		tmplName <- aaa$tmplName
		setFiName <- aaa$setFiName
	#########
	systemHome <- Sys.getenv("HOME")
	systemHome_R <- gsub("\\\\", "/", systemHome)
	fullRenvPath <- paste0(systemHome_R, "/.Renviron")
	fn_taPaSH <- taPaSH # makes the name of the variable and the name of the final folder identical
	#
	taPaSH_creationMsg <- paste0("The initial path of `", taPaSH, "` in the .Renviron file (`", fullRenvPath, "`) has been set to `", systemHome_R, "/", fn_taPaSH, "`. \nIf you want, you can open the .Renviron file (e.g. using R-Studio) and modify the variable `", taPaSH, "` (holding the path to the  `settings-home` directory) so that it points to a folder of your liking.")
	addInfo <- "Restart R for the changes to become effective."
	restartMsg <- "Please restart R for the changes in the .Renviron file to become effective."
	#
	# first check for existence of the .Renviron file
	renvExists <- file.exists(fullRenvPath)
	if (!renvExists) {
		return(ifNotRenvExists(systemHome_R, fn_taPaSH, taPaSH, taPaSH_creationMsg, addInfo)) #############
	}  else { # so the .Renviron file is existing
		# check if taPaSH is existing in the system: if yes, check if pointing to a valid directory; if no check if it is existing on the .Renviron file
		if (!onTest) {
			pat <- paste0("Sys.getenv(\"", taPaSH, "\")")
			taPaSH_system <- eval(parse(text=pat))  # returns `""` if not existing in Sys.getenv()
		} # end if !onTest
		if (taPaSH_system == "") {
			return(taPaSH_System_missing(systemHome_R, taPaName, taPaSH, fn_taPaSH, taPaSH_creationMsg, restartMsg, addInfo))	#############
		} else { # (taPaSH_system != "") --> so taPaSH IS existing in the system
			if (!dir.exists(taPaSH_system)) {  # check if pointing to a valid folder
				return(taPaSH_System_OK_noDir(systemHome_R, taPaSH, taPaSH_system, restartMsg))
			} else { # end if !dir.exists
				# so now everything should be good, file and system unisono etc.
				# check if a settings file is here, If no, please copy it.
				if (!onTest) {
					taPaSettingsPath <- paste0(path.package(taPaName), "/",  setFiName)
					localSettingsPath <- paste0(taPaSH_system, "/", setFiName)
				} # end if
				if (!file.exists(localSettingsPath)) {
					return(pleaseCopyFreshSettings(taPaSettingsPath, taPaSH_system, setFiName))
				} else { # so the settings.r file does exist  - we can, finally, go to checking the content of the settings.r file
					return(checkFileVersionPossiblyModify(pathToPack=taPaSettingsPath, folderLocal=taPaSH_system, nameLocal=setFiName, pm=taPaObj, taPaName, tmpl=tmplName))  # returns TRUE or FALSE
				} # end else
			} # end else !dir.exists
		} # end else taPaSH_system == ""
	} # end else if !renvExists
} # EOF

getUnisEnvirVariables <- function(unisetEnv) {
	#
	seFiName <- glob_filnameSettings # "settings.R"
	#
	env <- get(unisetEnv) # gives back the environment
	taPaName <- get("pkgUniset_UserPackageName", envir=env)
	taPaEnv <- get("pkgUniset_EnvironmentName", envir=env)
	taPaSH <- get("pkgUniset_RenvironSettingsHomeName", envir=env)
	taPaObj <- get("pkgUniset_SettingsObjectName", envir=env)
	tmplName <- get("pkgUniset_SuffixForTemplate", envir=env)
	setFiName <- paste0(taPaName, "_", seFiName)
	return(list(taPaName=taPaName, taPaEnv=taPaEnv, taPaSH=taPaSH, taPaObj=taPaObj, tmplName=tmplName, setFiName=setFiName))
} # EOF

#' @title Update settings of target package
#' @description Manually read in the settings-file in the target package settings
#' home directory as specified in the .Renviron file.
#' @section Note: If not present, the required `.Renviron` file will be
#' automatically created. If the variable defined in argument 'taPaSH' in
#' \code{\link{uniset_getFiles}} is not defined in the .Renviron file, it
#' will be automatically added, and its default path is pointing to the
#' (possibly also created) folder having the same name as `taPaSH` in
#' the users home directory, where the `xxx_settings.R` file is automatically
#' copied to if not already present.
#' It is possible to manually provide a different path in the
#' variable as defined in argument `taPaSH` in the .Renviron file, pointing
#' to any folder where then the xxx_settings.R file will reside.
#' XXX Improve this text please.
#' @param unisetEnv Character length one. Hand over the global variable defined
#' in the target package holding the name of the uniset-environment for the
#' specific target package ('uniset_env_name' or 'uev', see examples at
#' \code{\link{uniset}}.)
#' @param silent Logical. If a confirmation should be printed. Defaults
#' to 'FALSE'
#' @return An (invisible) list with the settings resp. a list called as defined
#' in argument 'taPaObj' in the environment called as defined in argument 'taPaEnv'.
#' See \code{\link{uniset}} for examples.
#' @export
uniset_updateSettings <- function(unisetEnv, silent=FALSE) {
	aaa <- getUnisEnvirVariables(unisetEnv)
		taPaName <- aaa$taPaName
		taPaEnv <- aaa$taPaEnv
		taPaSH <- aaa$taPaSH
		taPaObj <- aaa$taPaObj
		tmplName <- aaa$tmplName
		setFiName <- aaa$setFiName
	######
	ok <- checkSettings(taPaList=aaa) # makes sure that we have the latest version of the settings.r file in the settings-home directory defined in .Renviron
	if (ok) {
		pathSettings <- paste0(Sys.getenv(taPaSH), "/", setFiName) # the path to the local settings file as defined in the .Renviron file
		pat <- paste0(".GlobalEnv$", taPaEnv, " <- new.env()") # create a new environment
		eval(parse(text=pat)) # possibly also an other way
		sys.source(pathSettings, envir=get(taPaEnv, pos=".GlobalEnv"))
		#
		if (!silent) {
			cat(paste(taPaName, "settings updated\n"))
		}
		pat <- paste0("return(invisible(", taPaEnv, "$", taPaObj, "))")  #		return(invisible(.ap2$stn)) # was that
		eval(parse(text=pat))
	} else { # so if the settings check was not ok
		return(invisible(NULL))
	}
} # EOF

#' @title Automatically update Settings
#' @description Use this function within your code to automatically update the
#' settings from the users settings file
#' @details If 'autoUpdateSettings' in xxx_settings.r is left at 'TRUE', the
#' settings will be checked resp. updated automatically every time a function in
#' the target package is calling \code{\link{uniset_autoUpS}}.
#' @inheritParams uniset_updateSettings
#' @export
uniset_autoUpS <- function(unisetEnv) { # stops if somethings goes wrong
	aaa <- getUnisEnvirVariables(unisetEnv)
		taPaEnv <- aaa$taPaEnv
		taPaObj <- aaa$taPaObj
	####
	res <- 1
	pat <- paste0("exists(\"", taPaEnv, "$", taPaObj, "\")")
	doesExist <- eval(parse(text=pat))
	if (doesExist) {
		pat <- paste0(taPaEnv, "$", taPaObj, "$autoUpdateSettings")
		autoUpS <- eval(parse(text=pat))
	} else {
		autoUpS <- TRUE
	}
	if (autoUpS) {
		res <- uniset_updateSettings(unisetEnv, silent=TRUE)
	}
	if (is.null(res)) {
		stop(call.=FALSE)
	}
} # EOF
