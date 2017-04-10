#' @title Copy Uniset templates
#' @description Copies the required templates (a file with global variables
#' and the template for the settings.r file) either to the "home" directory,
#' or to any desired folder.
#' @details If the usual '~' for the users home directory can not be expanded, 
#' the files are instead copied to 'user/home'. Existing files will be overwritten.
#' The 'settings.r' file has to be moved into the 'inst' folder of your package, 
#' while the 'unisetGlobals.r' file goes into the 'R' folder of your package.
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
	pathGlobals <- paste0(path, "/", "unisetGlobals.r")
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

########################
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
		newTxt[newTxt==TRUE] <- ftLocal  # fill in the user´s local text in the expanded vector
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

copyFreshTemplate <- function(pathToPack, folderLocal, fileName, suff=pkgUniset_SuffixForTemplate) {
	#
	toPath <- paste0(folderLocal, "/", fileName, suff, ".r")
	ok <- file.copy(pathToPack, toPath, overwrite=TRUE)
	if (ok) { 
		cat("A fresh template of the ", fileName, " file has been copied from the package.\n")
	} else {
		stop(paste0("Sorry, an error while copying the template for the '", fileName, "' file has occurred."), call.=FALSE)
	}			
} # EOF

checkFileVersionPossiblyModify <- function(pathToPack, pathToLocal, folderLocal, nameLocal, pm=NULL, suff=pkgUniset_SuffixForTemplate) {
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
			message(paste0("Do you want to copy it now into the folder \n'", folderLocal, "'\n as a template ('", nameLocal, suff, ".r') for modifying the existing '", nameLocal, "' file?\n( y / n )"))
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

checkSettings <- function() {
	defaultFillForRenviron <- "\n\n# Please provide below a valid path to a folder of your liking, \n# with 'XX' being the folder where finally the 'settings.r' file will reside. \nAQUAP2SH = /Users/Name/Documents/path/to/some/folder/XX"
	addInfo <- "\nRestart R for the changes to become effective. \nSee the help for '?updateSettings' for additional information."
	sFile <- "settings.r"
	#
	pathSH <- Sys.getenv("AQUAP2SH")
	pspath <- paste(path.package("aquap2"), sFile, sep="/")
	if (nchar(pathSH) == 0) { 			## so the variable is *not* defined in .Renviron, or maybe we do not even have an .Renviron file
		homePath <- hoPa <-  "user/home"
		hp <- try(path.expand("~"), silent=TRUE)
		if (class(hp) != "try-Error") {
			homePath <- hoPa <-   hp
			hoPaAdd <- paste0("in '", homePath, "'")
		} else {
			hoPaAdd <- ""
		}
		homePath <- paste(homePath, ".Renviron", sep="/")
		if (!file.exists(homePath)) {
			ok <- file.create(homePath, showWarnings=FALSE)
			if (ok) {
				fcon <- file(homePath, open="w")
				writeLines(defaultFillForRenviron, fcon)
				close(fcon)
				creMsg <- paste0("The required '.Renviron' file in '", hoPa, "' has been created for you. Please open it (e.g. using R-Studio) and modify the variable 'AQUAP2SH' so that it points to a folder of your liking.", addInfo)
			} else { # file creation did NOT work
				creMsg <- paste0("Sorry, it was not possible to create the '.Renviron' file for you. Please do this manually by going to your home-directory and there saving a new plain text file under the name '.Renviron'. Open the file and define the variable 'AQUAP2SH' as the path to a folder of your liking.", addInfo)
			}
			message(creMsg)
			return(FALSE)
		} else { # so the .Renviron file is existing
		msg <- paste0("It appears you did not yet define the path to your aquap2 settings.r home directory in your '.Renviron' file ", hoPaAdd, ". \nPlease do this by going to the .Renviron file in your home directory and there define the variable 'AQUAP2SH' as the path to a folder of your liking.", addInfo)
		}
		message(msg)
		return(FALSE)
	} else { ## so we have something defined under AQUAP2SH
		if (!file.exists(pathSH)) {
			msg <- paste0("The folder \"", pathSH, "\" does not seem to exist. Please check the path defined in the '.Renviron' file.", addInfo)
			message(msg)
			return(FALSE)
		}
		pathToSettings <- paste(pathSH, sFile, sep="/")
		if (!file.exists(pathToSettings)) {
			msg <- paste("The required settings.r file does not seem to exist in the provided directory \n\"", pathSH, "\".\nWould like to copy a factory-fresh version of the settings.r file there now? \n( y / n)", sep="")
			message(msg)
			a <- readLines(n=1)
			if (a != "y" & a != "Y") {
				msg <- paste("Please see to it that a valid settings.r file will be in the directory shown above.")
				message(msg)
				return(FALSE)
			} else {  # so we do want to copy the file
				copySettingsFile(pspath, pathSH)
				return(TRUE)
			}
		} else { ## so the file does exist
			return(checkFileVersionPossiblyModify(pathToPack=pspath, pathToLocal=pathToSettings, folderLocal=pathSH, nameLocal=sFile, pm="stn"))  # returns TRUE or FALSE
		} # end else file exists
	} # end else nchar == 0
} # EOF

#' @title Update aquap2 settings.
#' @description Manually read in the settings-file in the aquap2-settings 
#' home directory as specified in the .Renviron file.
#' @details If you leave 'autoUpdateSettings' in settings.r to 'TRUE', the 
#' settings will be checked resp. updated automatically every time you call any 
#' function from package 'aquap2'.
#' @section Note: You have to set the path to where you want the settings.r file 
#' to be stored once in your .Renviron file by defining 
#' \code{AQUAP2SH = path/to/any/folder/XX} , with XX being any folder where then the 
#' settings.r file will reside in. If you do not have a '.Renviron' file in your 
#' home directory (user/home) you have to create one.
#' @param packageName Character, the name of the package where settings 
#' should be updated. Defaults to "aquap2".
#' @param silent Logical. If a confirmation should be printed. Defaults 
#' to 'FALSE'
#' @return An (invisible) list with the settings resp. a list called 'stn' in 
#' the environment '.ap2'.
#' @family Helper Functions
# @seealso \code{\link{settings_file}} 
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

checkForExperimentFolderStructure <- function() {
	aa <- .ap2$stn$fn_exports
	aaa  <- .ap2$stn$fn_rcode
	bb <- .ap2$stn$fn_rawdata
	bbb  <- .ap2$stn$fn_rdata
	cc <- .ap2$stn$fn_metadata
	ccc <- .ap2$stn$fn_results
	dd  <- .ap2$stn$fn_sampleList
	folderNames <- c(aa, aaa, bb, bbb, cc, ccc, dd)
	#
	if(!all(folderNames %in% list.files()) ) {
		stop(paste0("Sorry, it appears the current working directory is not within the required standard folder structure.\nPlease change the working directory or use 'genFolderStr()' to create an appropriate folder structure in the current working directory."), call.=FALSE)
	}
} # EOF

autoUpS <- function(cfs=.ap2$stn$defCfs) { # stops if somethings goes wrong
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
	if (cfs) {	
		checkForExperimentFolderStructure()
	}
	if (is.null(res)) {
		stop(call.=FALSE)
	}
} # EOF
