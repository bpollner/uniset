checkCh1 <- function(char, argName) {
	if (!all(is.character(char)) | length(char) != 1) {
		stop(paste0("Please provide a character length one to the argument '", argName, "'."), call.=FALSE)
	}
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
	stdFon_man <- "man"
	stdVec <- c(stdiFn_descr, stdFin_ns, stdFon_R, stdFon_man)
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
		ok <- dir.create(folderPath)
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
		ok <- dir.create(paToInst)
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

#' @title Test
#' @description Test
#' @param unisetEnv Character length one. The name of the environment holding the uniset 
#' definitions for a single package.
#' @export
uniset_test <- function(unisetEnv) {
	print(unisetEnv)
	env <- get(unisetEnv) # gives back the environment
	aa <- ls(env)
	print(aa)
	print("------------")
	pn <- get("pkgUniset_UserPackageName", envir=env)
	evn <- get("pkgUniset_EnvironmentName", envir=env)
	shn <- get("pkgUniset_RenvironSettingsHomeName", envir=env)
	print(pn); print(evn); print(shn)
	} # EOF


####################################################################
####################################################################
getParamName <- function(rowString) {
	sep <- "="
	if (!grepl(sep, rowString)) {
		if (grepl("<-", rowString)) {
			sep <- "<-"
		} else { # so none of both is present!
		#	stop("Sorry, a problem in updating one of the input files occurred.", call.=FALSE)
			return(NULL)
		}
	}
	if (grepl("=", rowString) & grepl("<-", rowString)) {
	#	stop("There are '=' and '<-' at the same time in one row in one of the input-files.\nThis can cause problems, sorry.", call.=FALSE)
		return(NULL)
	}
	check <- grep(sep, rowString)
	if (length(check) == 0) {
	#	message("Warning no good name (no equal)")
		return(NULL)
	}
	a <- strsplit(rowString, sep)[[1]][1] # get everything before the separator sep
	b <- strsplit(a, "\t")[[1]] # split by eventual tabs
	b <- b[length(b)] # get the last element
	return(trimws(b))
} # EOF
	
getMissingNameIndex <- function(char, txt) {
	ind <- grep(char, txt)
	if (length(ind) == 0) {return(NULL)}
	if (length(ind) == 1) {return(ind)}
	for (i in 1:length(ind)) {
		paramName <- getParamName(txt[ind[i]])			
		if (paramName == char) {
			return(ind[i])
		} else {
			stop("Uiui problem here...")
		}
	} # end for i
} # EOF
	
lookForParamNameInLocalFile <- function(paramName, ftLoc) {
	if (is.null(paramName)) {
		return(NULL)
	}
	ind <- getMissingNameIndex(paramName, ftLoc)
	if (length(ind) == 0) {
		return(NULL)
	} else {
		return(ind)
	}
} # EOF

modifyIndexFrame <- function(indF) {
	lookBack <- NULL # for satisfying the check
	indF <- indF[order(indF[,1]),] # sort the data frame
	rownames(indF) <- 1:nrow(indF)
	vals <- rle(indF[,3])$lengths # count how many values in each "group" there are in the third column
	if (length(vals) > 1) {
		corec <- c(0, (vals[1:(length(vals)-1)])) # start with a zero and cut away the lates value
	} else {
		corec <- 0 # because in only one group / single element the correction is only zero
	}
	corec <- cumsum(corec)
	corecVec <- rep(corec, vals)
	out <- cbind(indF, data.frame(correction=corecVec)) # the correction is due to the frameshift that gets introduced whenever a previous insertion is being made
	minCheck <- plyr::ddply(out, "nextLocalInd", plyr::summarise, minLook=min(lookBack)) # get the minimum value of looking back in each group of next local indices
	if (max(minCheck$minLook) > 1) { # that means we have a parameter that is starting a new block!
		return(NULL)
	}
	return(out)
} # EOF

expandFillInLocalTxt <- function(ftPack, ftLocal, missNames) {	
	if (!is.null(missNames)) {
		mi <- NULL # the missIndex, indexing the whole row
		for (i in 1: length(missNames)) {
			ind <- getMissingNameIndex(missNames[i], ftPack) # the index of the missing item in the package-txt
			mi <- c(mi, ind) # collect them all
		} # end for i	
		# get the anchor, that is the first lower indexed existing name in ftLocal and pair it with ..
		indF <- as.data.frame(matrix(NA, ncol=3, nrow=length(mi))) # for collecting the results
		colnames(indF) <- c("MissInd", "lookBack", "nextLocalInd")
		for (i in 1: length(mi)) {
			found <- FALSE
			lookBack <- 1
			while(!found) {
				si <- mi[i] - lookBack # si: search index
				a <- getParamName(ftPack[si])
				locInd <- lookForParamNameInLocalFile(a, ftLocal)
				if (is.null(locInd)) {
					lookBack <- lookBack+1
				} else {
					indF[i,] <- c(mi[i], lookBack, locInd)
					found <- TRUE
				}
			} # end while
		} # end for i
		indF <- modifyIndexFrame(indF) # sorting and adapting following to previous inserts, i.e. adding the correction vector
		if (is.null(indF)) { # gets returned NULL if one parameter is at the start of a new block !
			return(NULL)
		}
		newTxt <- rep(TRUE, (length(ftLocal)+length(missNames)))
		newValInd <- apply(indF[,-1, drop=FALSE], 1, sum) # gives the position in the expanded file where the new values will be placed
		newTxt[newValInd] <- FALSE
		newTxt[newTxt==TRUE] <- ftLocal  # fill in the users local text in the expanded vector
		newTxt[newValInd] <- ftPack[indF[,1]] # get the rows from the package text
	} else { # so we have nothing to add
		newTxt <- ftLocal
	}
	return(newTxt)
} # EOF

copyFreshTemplate <- function(pathToPack, folderLocal, fileName, tmpl) {
	suff <- tmpl
	#
	toPath <- paste0(folderLocal, "/", fileName, suff, ".r")
	ok <- file.copy(pathToPack, toPath, overwrite=TRUE)
	if (ok) { 
		cat("A fresh template of the ", fileName, " file has been copied from the package.\n")
	} else {
		stop(paste0("Sorry, an error while copying the template for the '", fileName, "' file has occurred."), call.=FALSE)
	}			
} # EOF

checkFileVersionPossiblyModify <- function(pathToPack, pathToLocal, folderLocal, nameLocal, pm=NULL, tmpl) {
	pv_suffixForTemplates <- tmpl
	# 
	loc <- pathToLocal
	pac <- pathToPack
	if (is.null(pm)) {
		pm <- ""
	} else {
		pm <- paste0("$", pm)
	}
	lenv <- new.env()
	sys.source(loc, envir=lenv)
	txt <- paste0("sort(names(lenv", pm, "))")
	locNames <- eval(parse(text=txt))
	penv <- new.env()
	sys.source(pac, envir=penv)
	txt <- paste0("sort(names(penv", pm, "))")
	pacNames <- eval(parse(text=txt))
	if (!identical(locNames, pacNames)) {
		okInd <- which(pacNames %in% locNames)
		miss <- pacNames[-okInd]
		delInd <- which(locNames %in% pacNames)
		del <- locNames[-delInd]
		if (length(miss) == 0) {miss <- NULL}
		if (length(del) == 0) {del <- NULL}
		msgNew <- "The new variables are:"
		msgDel <- "The following variables have been deleted:"
		#
		fconPack <- file(pac, open="r")
		ftPack <- readLines(fconPack) # the full settings.r text from the package
		close(fconPack)
		fconLocal <- file(loc, open="r")
		ftLocal <- readLines(fconLocal) # the full settings.r text fromt the local file
		close(fconLocal)
		## add the missing parameters
		newTxt <- try(expandFillInLocalTxt(ftPack, ftLocal, missNames=miss), silent=TRUE) # returns the unchanged local text if there is nothing to add
		if(class(newTxt) == "try-error") { 
			newTxt <- NULL
		}
		## now delete the obsolete parameters
		if (!is.null(del) & (!is.null(newTxt))) {
			delInd <- NULL
			for (i in 1: length(del)) {
				ind <- getMissingNameIndex(del[i], newTxt) # the index of the items to be deleted
				delInd <- c(delInd, ind) # collect them all
			} # end for i
			newTxt <- newTxt[-delInd]
		}
		#
		if (!is.null(newTxt)) { # so we maybe had to add something (what went well), and we maybe also had to delete some parameters
			fconLocal <- file(loc, open="w")
			writeLines(newTxt, fconLocal) # write the new file to settings.r in pathSH
			close(fconLocal)
			msg <- paste0("Your '", nameLocal, "' file in the folder \n", folderLocal, "\nhas been updated.\n***Everything is ok.***")
			doCopyMove <- FALSE
		} else { # so we could NOT modify the local file
			msg <- paste0("There appears to be a newer version of the '", nameLocal, "' file in the package 'aquap2'.")
			doCopyMove <- TRUE
		}
		message(msg) # actually display here a message !!
		fillLeft <- "   " # to have some space from the left side
		if ((!is.null(miss)) & is.null(del)) {
			message(msgNew) ;   message(paste0(fillLeft, paste(miss, collapse=", "))) # "The new variables are:"
		} else {
			if (is.null(miss) & (!is.null(del)) ) {
				message(msgDel); 	message(paste0(fillLeft, paste(del, collapse=", ")))  #"The following variables have been deleted:"
			} else {
				message(msgNew) ;   message(paste0(fillLeft, paste(miss, collapse=", ")))
				message(msgDel); 	message(paste0(fillLeft, paste(del, collapse=", ")))
			}
		}
		if (doCopyMove) {
			message(paste0("Do you want to copy it now into the folder \n'", folderLocal, "'\n as a template ('", nameLocal, pv_suffixForTemplates, "') for modifying the existing '", nameLocal, "' file?\n( y / n )"))
			a <- readLines(n=1)
			if (a != "y" & a != "Y") {
				message("Please be aware that the package will not work properly if your '", nameLocal, "' file is not up to date.")
				return(FALSE)
			} else {
				copyFreshTemplate(pathToPack, folderLocal, nameLocal, tmpl)
				message(paste0("Please update your '", nameLocal, "' file according to the template."))
				return(FALSE)
			}	
		} else { ## if (doCopyMove) == FALSE
			return(TRUE) # so we successfully copied / deleted the parameter(s) and wrote the new file via writeLines; no copying of the file.
		}
	} else { 	# so the variable names in the two settings files are identical
		return(TRUE)
	} 
	stop(paste0("Sorry, there is an unexpected error in file check (", nameLocal, ")")) # theoretically this should never happen...
} # EOF

checkCreateSHfolder <- function(systemHome, fn_taPaSH) {
	if (!dir.exists(paste0(systemHome, "/", fn_taPaSH))) {
		dirCreaOk <- dir.create(paste0(systemHome, "/", fn_taPaSH))
		if (!dirCreaOk) {
			msg <- paste0("Sorry, the required settings-home directory `", fn_taPaSH, "` could not be created in `", systemHome, "`.")
			message(msg)
			return(FALSE)
		} else { # so we created the .Renviron file AND created the aquap2SH folder
			msg <- paste0("The folder `", fn_taPaSH, "` as settings-home directory has been created in `", systemHome, "`.")
			message(msg)
			return(TRUE)
		}
	} # end if !dir.exists aquap2SH
	return(TRUE) 
} # EOF

checkSettings <- function(taPaList) {
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
	if (!renvExists) { # we have NO .Renviron file, so we simply make one
		createOK <- file.create(fullRenvPath, showWarnings=FALSE)
		if (!createOK) {  #  if .Renviron could not be created
			msg <- paste0("Sorry, for unknown reasons the creation of the .Renviron file in `", systemHome, "` failed.")
			message(msg)
			return(FALSE)
		} else { # so we could create the .Renviron file
			# if no .Renviron file, then also no settings home diretory --> create one
			ok <- checkCreateSHfolder(systemHome, fn_taPaSH)
				if (!ok) {
					return(FALSE)
				}
			# now we have to fill the newly created .Renviron file and point taPaSH to the newly created folder
			defaultFillForRenviron <- paste0("\n\n", taPaSH, " = ", systemHome_R, "/", fn_taPaSH) # here problem in windows !! # really? I do not (in windows)
			fcon <- file(fullRenvPath, open="w")
			writeLines(defaultFillForRenviron, fcon)
			close(fcon)
			creMsg <- paste0("The required '.Renviron' file in '", systemHome, "' has been created for you.\n", taPaSH_creationMsg, "\n", addInfo)
			message(creMsg)
			return(FALSE)
		} # end else (where we could create and fill the .Renviron file and create the settings home folder
	}  else { # so the .Renviron file is existing
		# check if taPaSH is existing in the system: if yes, check if pointing to a valid directory; if no check if it is existing on the .Renviron file
		pat <- paste0("Sys.getenv(\"", taPaSH, "\")")
		taPaSH_system <- eval(parse(text=pat))  # returns `""` if not existing in Sys.getenv() # was 		taPaSH_system <- Sys.getenv("AQUAP2SH") # returns `""` if not existing in Sys.getenv()
		if (taPaSH_system == "") { # so it is not existing in the system, and we have to check if it exists in the .Renviron file
			fcon <- file(fullRenvPath, open="r")
			content <- readLines(fcon)
			close(fcon)
			if (any(grepl(taPaSH, content))) { # returns TRUE if taPaSH is present in the .Renviron file
				# so not in the system, but on the file --> that means we have to restart R
				message(restartMsg)
				return(FALSE)
			} else { # so not in the system, and not on the file (but the .Renviron was present
				# now we have to ADD the taPaSH to the existing .Renviron file
				# first check for existence / create the settings home folder
				ok <- checkCreateSHfolder(systemHome, fn_taPaSH)
				if (!ok) {
					return(FALSE)
				}
				fcon <- file(fullRenvPath, open="r+b")
				content <- readLines(fcon)
				newContent <- c(content, paste0("\n\n## ", taPaName, ":"), paste0(taPaSH, " = ", systemHome, "/", fn_taPaSH), "\n") # was c(content, "\n\n## aquap2", paste0("AQUAP2SH = ", systemHome, "/", fn_taPaSH), "\n")
				writeLines(newContent, fcon)
				close(fcon)
				msg <- paste0(taPaSH_creationMsg, "\n", addInfo)
				message(msg)
				return(FALSE)
			} # end else
		} else { # (taPaSH_system != "") --> so taPaSH IS existing in the system
			# check if pointing to a valid folder
			if (!dir.exists(taPaSH_system)) {
				# first check if the content of taPaSH in the file and in the system are the same
				fcon <- file(fullRenvPath, open="r")
				content <- readLines(fcon)
				close(fcon)
				taPaSH_file <- content[which(grepl(taPaSH, content))] # get only the one string that is the taPaSH
				fileValue <- trimws(strsplit(taPaSH_file, "=")[[1]][[2]]) # the [[1]] to get out of the list. naja.
				if (fileValue != taPaSH_system) { # so the content of taPaSH is different in the system and in the file, we have to restart R
					message(restartMsg)
					return(FALSE)
				} # end if				
				msg <- paste0("Sorry, the path `", taPaSH_system, "` specified in the `", taPaSH,"` variable is not pointing to a valid directory.\nPlease change the value of `", taPaSH, "` in the .Renviron file (`", fullRenvPath, "`), or create the appropriate file structure.")
				message(msg)
				return(FALSE)
			} else { # end if !dir.exists
				# so now everything should be good, file and system unisono etc.
				# check if a settings file is here, If no, please copy it.
				sFile <- setFiName # we get that from the environment -reading things.
				pat <- paste0("Sys.getenv(\"", taPaSH, "\")")	
				pathSH <- eval(parse(text=pat))
				pspath <- paste(path.package(taPaName), sFile, sep="/")
				pathToSettings <- paste(pathSH, sFile, sep="/")
				if (!file.exists(pathToSettings)) {
					# please simply copy the settings
					ok <- file.copy(pspath, pathSH)
					if (!ok) {
						message(paste0("Sorry, for unknown reasons it was not possible to copy the `", sFile, "` file from `", pspath, "` to `", pathSH, "`."))
						return(FALSE)
					} else { # so we could copy the settings.r file
						message(paste0("The '", sFile, "' file has been copied into `", pathSH, "`."))
						return(TRUE)
					} # end else
				} else { # so the settings.r file does exist  - we can, finally, go to checking the content of the settings.r file		
					return(checkFileVersionPossiblyModify(pathToPack=pspath, pathToLocal=pathToSettings, folderLocal=pathSH, nameLocal=sFile, pm=taPaObj, tmpl=tmplName))  # returns TRUE or FALSE
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
