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
#' @author Bernhard Pollner, Zoltan Kovacs
#' @section Maintainer: Bernhard Pollner <bernhard.pollner@@mac.com>
#' @section Important Functions: \code{\link{uniset_getFiles}}
#' @examples
#' \dontrun{
#' # for an imaginary package called 'dogPack':
#' uniset_getFiles("dogPack", ".dpe", "stn") 
#' 	# 'dpe' could be for 'dogPack Environment', 'stn' could be for 'settings'
#' 	# see ?uniset_getFiles
#' 	# now move the three files into their target folder
#' 	# add and modify the key=value pairs in the file 'xxx_settings.r' 
#'  ########
#' 	# from 'dogPack', you would call:
#' uniset::uniset_updateSettings() # or
#' uniset::uniset_autoUpS() # from inside of functions
#' # then you can access the values in the xxx_settings.r file with (in this example):
#' .dpe$stn$XXX  # with 'XXX' being any defined key in the xxx_settings.r file
#' } 
#' @docType package
#' @name uniset
NULL
