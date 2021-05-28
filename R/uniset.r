#' @title Universal Settings file for R
#' @description A universal framework to use an easily accessible, user-friendly 
#' text file as settings-file for any R-package, enabling personalization and 
#' user-specific customization of any parameter to be used within the code of a 
#' package. This settings file remains in place and unchanged even when the 
#' package is updated or re-installed.
#' @details First use \code{\link{copyUnisetTemplates}}, then move the 'settings.r' 
#' file into the 'inst' folder (create one if not already done) of  the package 
#' that you want to enable to use the package 'uniset', and the file 'zzz.r" to 
#' the 'R' folder of the package that you want to enable to use the package 
#' 'uniset'. 
#' Every variable defined in the settings.r file is accessible in your code.
#' The big advantage is that the user of your package can customize and 
#' personalize your package by using .. XXX
#' For an introduction and more detailed information please see the package 
#' vignettes: \code{browseVignettes(package = "uniset")}
#' @author Bernhard Pollner, Zoltan Kovacs
#' @section Maintainer: Bernhard Pollner <bernhard.pollner@@mac.com>
#' @section Important Functions: \code{\link{copyUnisetTemplates}}
#'
#'
#'
#' @docType package
#' @name uniset
NULL
