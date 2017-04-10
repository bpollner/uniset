#' @title Universal Settings file for R
#' @description A universal framework to use an easily accessible, user-friendly 
#' text file as settings-file for any R-package, enabling personalization and 
#' user-specific customization of any parameter to be used within the code of a 
#' package. This settings file remains in place and unchanged even when the 
#' package is updated or re-installed.
#' @details First use \code{\link{copyUnisetTemplates}}, move the 'settings.r' 
#' file into the 'inst' folder (create one if not already done) of your package, 
#' and the 'unisetGlobals.r' file into the 'R' folder of your package.
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
