checkCh1 <- function(char, argName){
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

readInCharInput <- function(what) {
	
} # EOF


# function for generating and producing the three files that we need in the target package
# we want three files: 
#	the setttings.r file: here we have to adapt the object name
#	the zzz.r file: here the environment name and the values of the uniset_Variables are set
# 	the uniset_globals.r file: here the one global variable, defining the name of uniset-environment to read data from
# provide via input, if any of them is NULL, this one gets asked interactively. (or all interactively) 

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
#' @param taPaObj Character length one. The name of the object (residing in the 
#' environment with the name provided in argument 'taPaEnv') holding the list 
#' with all the individual key-value pairs that can be defined to be used in the 
#' target package. It is recommended to use a rather short name. See details.
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
#' @return An (invisible) character holding the path of the folder where the 
#' three files were written into. 
#' @examples
#' \dontrun{
#' # for an imaginary package called 'dogPack':
#' uniset_getFiles("dogPack", ".dpe", "stn") 
#' 	# 'dpe' could be for 'dogPack Environment', 'stn' could be for 'settings'
#' 	# see details. 
#' } 
#' @export
uniset_getFiles <- function(taPaName=NULL, taPaEnv=NULL, taPaObj=NULL, where="~/desktop", taPaSH="def", tmpl= "_TEMPLATE") {
	# read in from the globals, give shorter names
	unisEnvName <- glob_unisetEnvName  							# XXX_unisetEnv
	taPackName <- glob_targetPackageName 						# XXX_packageName
	taPackSHname <- glob_targetPackageSettingsHomeVarName		# XXX_SH
	taPackEnvName <- glob_targetPackageEnvName					# XXX_targetEnv
	taPackStnName <- glob_targetPackgae_settingsObjectName 		# XXX_obj
	taPackTmplSuff <- glob_targetPackage_templateSuffix 		# XXX_template
	taPackActualSett <- glob_actualSettingsName 							# XXX_actualSettingsName

	#
	unisetEnvSuffix <- "_unisetEnv"
	folderPrev <- "Files for R-package '"
	filenameZZZ <- "zzz.r"
	filenameGlobals <- "uniset_globals.r"
	filnameSettings <- "settings" # the '.r' gets appended below
	## 	
	####### define paths ###### 
	aa <- path.package("uniset")
	path_unis_globals <- paste0(aa, "/inst/uniset_globals.r")
	path_unis_zzz <- paste0(aa, "/inst/zzz.r")
	path_unis_settingsTemplate <- paste0(aa, "/inst/settings.r")
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
	taPaName <- "dogPack"
	taPaSH <- checkGetTaPaSH(taPaSH, taPaName)
	taPaEnv <- ".dpe"
	taPaObj <- "stn"
	#	
	####### replace text #########
	expSettingsName <- paste0(taPaName, "_", filnameSettings, ".r") # put the package name and 'settings.r' together
	thisUnisetEnvName <-  paste0(taPaName, unisetEnvSuffix) # the underscore '_' between is already above
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
	# create the three files
	ok <- file.create(paste0(folderPath, "/", filenameZZZ)); 		if (!ok) {stop(paste0("Sorry, the file '", filenameZZZ, "' could not be created."), call.=FALSE)}
	ok <- file.create(paste0(folderPath, "/", filenameGlobals)); 	if (!ok) {stop(paste0("Sorry, the file '", filenameGlobals, "' could not be created."), call.=FALSE)}
	ok <- file.create(paste0(folderPath, "/", expSettingsName)); 	if (!ok) {stop(paste0("Sorry, the file '", expSettingsName, "' could not be created."), call.=FALSE)}
	#
	# write into the files
	loc <- paste0(folderPath, "/", filenameZZZ)
	fcon <- file(loc, open="w")
	writeLines(zzzTxt, fcon) # write the zzz.r file
	close(fcon)
	#
	loc <- paste0(folderPath, "/", filenameGlobals)
	fcon <- file(loc, open="w")
	writeLines(globalsTxt, fcon) # write globals file
	close(fcon)
	#
	loc <- paste0(folderPath, "/", expSettingsName)
	fcon <- file(loc, open="w")
	writeLines(settingsTxt, fcon) # write the settings file
	close(fcon)
	##
	allfns <- c(filenameZZZ, filenameGlobals, expSettingsName)
	cat(paste0("Three files called '", paste0(allfns, collapse="', \n'"), "have been written to the folder '", folderPath, "'."))
	return(invisible(folderPath))
} # EOF

#' @title Copy Uniset templates
#' @description Copies the required templates (a file with global variables
#' and the template for the settings.r file) either to the "home" directory,
#' or to any desired folder.
#' @details If the usual '~' for the users home directory can not be expanded, 
#' the files are instead copied to 'user/home'. Existing files will be overwritten.
#' The 'settings.r' file has to be moved into the 'inst' folder (create one if 
#' not already done) of your package, while the 'unisetGlobals.r' file goes into 
#' the 'R' folder of your package.
#' @param target Where the templates should be copied to. Provide either a valid
#' path to a folder, or leave at the default '~' to copy the files to your home 
#' directory.
#' @examples
#' \dontrun{
#' copyUnisetTemplates() # to copy them to your home directory
#' copyUnisetTemplates("foo/moo/bar") # copy the templates to folder 'bar'
#' }
#' @export
copyUnisetTemplates <- function(target="~") {
	if (target == "~") {
		pathTarget <-  "user/home"
		hp <- try(path.expand(target), silent=TRUE)
		if (class(hp) != "try-Error") {
			pathTarget <- hp
		}
	} else {
		if (!dir.exists(target)) {
			stop(paste0("Sorry, the provided directory '", target, "' does not seem to exist"), call.=FALSE)
		} else {
			pathTarget <- target
		}	
	}
	path <- path.package("uniset")
	pathGlobals <- paste0(path, "/", "zzz.r")
	pathSettings <- paste0(path, "/", "settings.r")
	okGlob <- file.copy(pathGlobals, pathTarget, overwrite=TRUE)
	okSet <- file.copy(pathSettings, pathTarget, overwrite=TRUE)
	if (!any(okGlob, okSet)) {
		stop(paste0("Sorry, it appears that both required templates could not be copied to '", pathTarget, "'."), call.=FALSE)
	}
	msg <- paste0("Two r-files, i.e. a template for the settings.r file and the necessary globals to include in your package should have been copied to '", pathTarget, "'.")
	cat(msg)
	return(invisible(NULL))
}









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

copySettingsFile <- function(fromPath, toPath) {
	a <- paste(toPath, "settings.r", sep="/")
	b <- paste(toPath, "settings_OLD.r", sep="/")
	if (file.exists(a)) {
		file.rename(a, b)
	}
	ok <- file.copy(fromPath, toPath, overwrite=TRUE)
	if (ok) { cat("A fresh version of the settings.r file has been copied from the package.\n")}			
} # EOF

copyFreshTemplate <- function(pathToPack, folderLocal, fileName) {
	suff <- pv_suffixForTemplates
	#
	toPath <- paste0(folderLocal, "/", fileName, suff, ".r")
	ok <- file.copy(pathToPack, toPath, overwrite=TRUE)
	if (ok) { 
		cat("A fresh template of the ", fileName, " file has been copied from the package.\n")
	} else {
		stop(paste0("Sorry, an error while copying the template for the '", fileName, "' file has occurred."), call.=FALSE)
	}			
} # EOF

checkFileVersionPossiblyModify <- function(pathToPack, pathToLocal, folderLocal, nameLocal, pm=NULL) {
	loc <- pathToLocal
	pac <- pathToPack
	#
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
			message(paste0("Do you want to copy it now into the folder \n'", folderLocal, "'\n as a template ('", nameLocal, pv_suffixForTemplates, ".r') for modifying the existing '", nameLocal, "' file?\n( y / n )"))
			a <- readLines(n=1)
			if (a != "y" & a != "Y") {
				message("Please be aware that the package will not work properly if your '", nameLocal, "' file is not up to date.")
				return(FALSE)
			} else {
				copyFreshTemplate(pathToPack, folderLocal, nameLocal)
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

checkCreateSHfolder <- function(systemHome, fn_aquap2SH) {
	if (!dir.exists(paste0(systemHome, "/", fn_aquap2SH))) {
		dirCreaOk <- dir.create(paste0(systemHome, "/", fn_aquap2SH))
		if (!dirCreaOk) {
			msg <- paste0("Sorry, the required settings-home directory `", fn_aquap2SH, "` could not be created in `", systemHome, "`.")
			message(msg)
			return(FALSE)
		} else { # so we created the .Renviron file AND created the aquap2SH folder
			msg <- paste0("The folder `", fn_aquap2SH, "` as settings-home directory has been created in `", systemHome, "`.")
			message(msg)
			return(TRUE)
		}
	} # end if !dir.exists aquap2SH
	return(TRUE) 
} # EOF

checkSettings <- function() {
	systemHome <- Sys.getenv("HOME")
	systemHome_R <- gsub("\\\\", "/", systemHome)
#	fullRenvPath <- paste0(systemHome, "/.Renviron") # was this
	fullRenvPath <- paste0(systemHome_R, "/.Renviron")
	fn_aquap2SH <- "aquap2SH"
	AQUAP2SH_creationMsg <- paste0("The initial path of `AQUAP2SH` in the .Renviron file (`", fullRenvPath, "`) has been set to `", systemHome_R, "/", fn_aquap2SH, "`. \nIf you want, you can open the .Renviron file (e.g. using R-Studio) and modify the variable `AQUAP2SH` (holding the path to the  `settings-home` directory) so that it points to a folder of your liking.")
	addInfo <- "Restart R for the changes to become effective. \nSee the help for '?updateSettings' for additional information."
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
			ok <- checkCreateSHfolder(systemHome, fn_aquap2SH)
				if (!ok) {
					return(FALSE)
				}
			# now we have to fill the newly created .Renviron file and point AQUAP2SH to the newly created folder
			defaultFillForRenviron <- paste0("\n\nAQUAP2SH = ", systemHome_R, "/", fn_aquap2SH) # here problem in windows !!
			fcon <- file(fullRenvPath, open="w")
			writeLines(defaultFillForRenviron, fcon)
			close(fcon)
			creMsg <- paste0("The required '.Renviron' file in '", systemHome, "' has been created for you.\n", AQUAP2SH_creationMsg, "\n", addInfo)
			message(creMsg)
			return(FALSE)
		} # end else (where we could create and fill the .Renviron file and create the settings home folder
	}  else { # so the .Renviron file is existing
		# check if AQUAP2SH is existing in the system: if yes, check if pointing to a valid directory; if no check if it is existing on the .Renviron file
		AP2SH_system <- Sys.getenv("AQUAP2SH") # returns `""` if not existing in Sys.getenv()
		if (AP2SH_system == "") { # so it is not existing in the system, and we have to check if it exists in the .Renviron file
			fcon <- file(fullRenvPath, open="r")
			content <- readLines(fcon)
			close(fcon)
			if (any(grepl("AQUAP2SH", content))) { # returns TRUE if AQUAP2SH is present in the .Renviron file
				# so not in the system, but on the file --> that means we have to restart R
				message(restartMsg)
				return(FALSE)
			} else { # so not in the system, and not on the file (but the .Renviron was present
				# now we have to ADD the AQUAP2SH to the existing .Renviron file
				# first check for existence / create the settings home folder
				ok <- checkCreateSHfolder(systemHome, fn_aquap2SH)
				if (!ok) {
					return(FALSE)
				}
				fcon <- file(fullRenvPath, open="r+b")
				content <- readLines(fcon)
				newContent <- c(content, "\n\n## aquap2", paste0("AQUAP2SH = ", systemHome, "/", fn_aquap2SH), "\n")
				writeLines(newContent, fcon)
				close(fcon)
				msg <- paste0(AQUAP2SH_creationMsg, "\n", addInfo)
				message(msg)
				return(FALSE)
			} # end else
		} else { # (AP2SH_system != "") --> so AQUAP2SH IS existing in the system
			# check if pointing to a valid folder
			if (!dir.exists(AP2SH_system)) {
				# first check if the content of AQUAP2SH in the file and in the system are the same
				fcon <- file(fullRenvPath, open="r")
				content <- readLines(fcon)
				close(fcon)
				AP2SH_file <- content[which(grepl("AQUAP2SH", content))] # get only the one string that is the AQUAP2SH
				fileValue <- trimws(strsplit(AP2SH_file, "=")[[1]][[2]])
				if (fileValue != AP2SH_system) { # so the content of AQUAP2SH is different in the system and in the file, we have to restart R
					message(restartMsg)
					return(FALSE)
				}				
				msg <- paste0("Sorry, the path `", AP2SH_system, "` specified in the `AQUAP2SH` variable is not pointing to a valid directory.\nPlease change the value of `AQUAP2SH` in the .Renviron file (`", fullRenvPath, "`), or create the appropriate file structure.")
				message(msg)
				return(FALSE)
			} else { # end if !dir.exists
				# so now everything should be good, file and system unisono etc.
				# check if a settings file is here, If no, please copy it.
				sFile <- "settings.r"
				pathSH <- Sys.getenv("AQUAP2SH")
				pspath <- paste(path.package("aquap2"), sFile, sep="/")
				pathToSettings <- paste(pathSH, sFile, sep="/")
				if (!file.exists(pathToSettings)) {
					# please simply copy the settings
					ok <- file.copy(pspath, pathSH)
					if (!ok) {
						message(paste0("Sorry, for unknown reasons it was not possible to copy the `settings.r` file from `", pspath, "` to `", pathSH, "`."))
						return(FALSE)
					} else { # so we could copy the settings.r file
						message(paste0("The settings.r file has been copied into `", pathSH, "`."))
						return(TRUE)
					} # end else
				} else { # so the settings.r file does exist  - we can, finally, go to checking the content of the settings.r file		
					return(checkFileVersionPossiblyModify(pathToPack=pspath, pathToLocal=pathToSettings, folderLocal=pathSH, nameLocal=sFile, pm="stn"))  # returns TRUE or FALSE
				} # end else
			} # end else !dir.exists
		} # end else AP2SH_system == ""
	} # end else if !renvExists	
} # EOF

#' @title Update aquap2 settings.
#' @description Manually read in the settings-file in the aquap2-settings 
#' home directory as specified in the .Renviron file.
#' @details If you leave 'autoUpdateSettings' in settings.r to 'TRUE', the 
#' settings will be checked resp. updated automatically every time you call any 
#' function from package 'aquap2'.
#' @section Note: If not present, the required `.Renviron` file will be 
#' automatically created. If the variable `AQUAP2SH` is not defined in the 
#' .Renviron file, it will be automatically added, and its default path is 
#' pointing to the (possibly also created) folder `aquap2SH` in the users home 
#' directory, where the `settings.r` file is automatically copied to if not 
#' already present. It is possible to manually provide a different path in the 
#' variable `AQUAP2SH` in the .Renviron file, pointing to any folder where then 
#' the settings.r file and all other relevant general files will reside.
#' @param packageName Character, the name of the package where settings 
#' should be updated. Defaults to "aquap2".
#' @param silent Logical. If a confirmation should be printed. Defaults 
#' to 'FALSE'
#' @return An (invisible) list with the settings resp. a list called 'stn' in 
#' the environment '.ap2'.
#' @family Helper Functions
#' @seealso \code{\link{settings_file}} 
#' @examples
#' \dontrun{
#' updateSettings()
#' str(.ap2$stn)
#' ls(.ap2)
#'}
#' @export
updateSettings <- function(packageName="aquap2", silent=FALSE) { 
	ok <- checkSettings() # makes sure that we have the latest version of the settings.r file in the settings-home directory defined in .Renviron
	if (ok) {
		pathSettings <- paste0(Sys.getenv("AQUAP2SH"), "/settings.r")
		sys.source(pathSettings, envir=.GlobalEnv$.ap2)
	#	if (any(grepl(".ap2", search(), fixed=TRUE))) {
	#		detach(.ap2)
	#	}
	#	attach(.ap2)
		if (!silent) {
			cat(paste(packageName, "settings updated\n"))
		}
		return(invisible(.ap2$stn))
	} else { # so if the settings check was not ok
		return(invisible(NULL))
	}
} # EOF


#' @title Test
#' @export
testingUniset <- function() {
	print(.unisetVars$pkgUniset_UserPackageName)
	print(.unisetVars$pkgUniset_RenvironSettingsHomeName)
	print(.unisetVars$pkgUniset_SettingsObjectName)
	print(.unisetVars$pkgUniset_EnvironmentName)
	print(.unisetVars$pkgUniset_SuffixForTemplate)
} # EOF



#' @title Automatically update Settings
#' @description Use this function within your code to automatically update the 
#' settings from the user´s settings file
#' @export
autoUpS <- function() { # stops if somethings goes wrong
	res <- 1
	if (exists(".ap2$stn")) {
		autoUpS <- .ap2$stn$autoUpdateSettings
	} else {
		autoUpS <- TRUE
	}
	if (autoUpS) {
		if (is.null(.ap2$.devMode)) { 			## to be able to run it locally without loading the package
			res <- updateSettings(packageName="aquap2", silent=TRUE)
		}
	}
	if (is.null(res)) {
		stop(call.=FALSE)
	}
} # EOF
