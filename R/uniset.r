#' @title Universal Settings file for R
#' @description Package 'uniset' provides the means to outfit your package 
#' (the 'target package') with an easily accessible, user-friendly and easy 
#' to read text file where settings resp. various parameters can be saved.
#' This settings file remains in place and can remain unchanged even when the 
#' target package is updated or re-installed.
#' @details First use \code{\link{uniset_getFiles}}, then move the 'xxx_settings.r' 
#' file ('xxx' for the name of your package) into the 'inst' folder 
#' (create one if not already done) of the package that you want to enable to use 
#' the package 'uniset' (the target package). Move the file 'zzz.r" and the file 
#' 'uniset_globals' to the 'R' folder of the target package. In case that the 
#' '.onLoad' function already is defined, add the six lines of code from the file 
#' 'zzz.R' to your existing '.onLoad' function.
#' Alternatively, use \code{\link{uniset_copyFilesToPackage}} to copy the required 
#' files directly into the target package.
#' Every variable defined in the xxx_settings.r file is accessible in the code of 
#' the target package. See the created 'xxx_settings.r' file for an example. 
#' The target package has to list 'uniset' as an 'import', and then you can 
#' use the function \code{\link{uniset_updateSettings}} or 
#' \code{\link{uniset_autoUpS}} to manually or automatically update the settings, 
#' i.e. to read in the key=value pairs stored in the xxx_settings.r file and have 
#' them accessible in a environment created by the target package. 
#' For an introduction and more detailed information please see the package 
#' vignettes: \code{browseVignettes(package = "uniset")}
#' @section Important: In case that the '.onLoad' function already is defined, add 
#' the six lines of code from the file 'zzz.R' to your existing '.onLoad' function.
#' @section Advantage: The most imminent advantage of the 'uniset' settings-file 
#' system over using simply a csv-file or an excel-file for permanently storing 
#' settings for any package is the fact that the key=value pairs in the xxx_settings.R
#' file get updated (added / deleted) dynamically. So the developer of a package can 
#' delete keys or introduce new ones, and the new key=value pairs will be (with few 
#' exceptions) automatically added to or deleted from the local xxx_settings.R file. 
#' Values changed by the user of the target package will be preserved. So the author 
#' of the target package can add or delete keys from the xxx_settings.R file 
#' without worrying that this will cause any effort or troubles for the user of 
#' the target package. (Few exceptions can apply, see the text in the generated 
#' xxx_settings.R file.)
#' @author Bernhard Pollner, Zoltan Kovacs
#' @section Maintainer: Bernhard Pollner <bernhard.pollner@@mac.com>
#' @section Important Functions: \code{\link{uniset_copyFilesToPackage}}, 
#' \code{\link{uniset_getFiles}}
#' @examples
#' \dontrun{
#' # for an imaginary package called 'dogPack':
#' uniset_getFiles("dogPack", ".dpe", "stn") 
#' 	# 'dpe' could be for 'dogPack Environment', 'stn' could be for 'settings'
#' 	# see ?uniset_getFiles
#' 	# now move the three files into their target folder
#' 	# add and modify the key=value pairs in the file 'xxx_settings.r' 
#' 	# You can access the values in the xxx_settings.r file with (in this example):
#' 	.dpe$stn$XXX  # with 'XXX' being any defined key in the xxx_settings.r file
#' ################
#' # or leave everything at their defaults:
#' uniset_getFiles("dogPack") 
#' # or use:
#' aa <- "~/desktop/dogPack" # the path to root of package 'dogPack'
#' uniset_copyFilesToPackage(aa) # uses the defaults for all arguments, will produce 
#' code that enables you to access the key=value pairs as follows:
#' color <- .doe$stn$darkColor # 'darkColor' being e.g. a defined key
#' ################
#' 	# from 'dogPack', you would call:
#' updateSettings <- function() { # this function is defined in package 'dogPack' !
#' 	  	uniset::uniset_updateSettings(get("uniset_env_name"))
#' 			# you have to hand over the name of the environment where all the 
#' 			# necessary uniset-variables are defined. This name is defined as 
#' 			# global variable in the file 'uniset_globals.R'.
#' } # EOF
#' updateSettings <- function() { # this function is defined in package 'dogPack' !
#' 	  	uniset::uniset_updateSettings(get("uev"))	# same as above, short version
#' } # EOF
#' ################
#' # For using the auto-update function (assuming we left everything at default at 
#' # uniset_copyFilesToPackage(aa) above)
#' someDogPackFunc <- function(m=2) { # this function is defined in package 'dogPack' !
#'		uniset::uniset_autoUpS(get("uev")) 
#' 		myColor <- .doe$stn$darkColor # read in a color from the settings
#' 		if (.doe$stn$anyDecision == TRUE) { # read in a logical
#'			plot(1:10, type="l", col=myColor)	
#' 		} else {
#' 			print(.doe$stn$strength * m) # read in a number
#' 		}
#' } # EOF
#' } # end dontrun 
#' @docType package
#' @name uniset
NULL
