#' @title dogPack TEst
#' @description Simple test if setup is ok.
#' @return  Prints target package name, environment name, name of list-object,
#' name of folder where the local 'dogPack_settings.R' resides etc.
#' @export
dogPackTest <- function(){
    uniset::uniset_test(get("uev"))
} # EOF

#' @title Update Settings
#' @description Update Settings
#' @export
dogPack_updateSettings <- function() {
    uniset::uniset_updateSettings(get("uev"))
} # EOF


#' @title Test Autoupdate
#' @description AutoUpdate Settings. Is also reading the value of the
#' key 'favouriteColor' from the 'dogPack_settings.R' file.
#' @export
dogPack_autoUpS <- function() {
	uniset::uniset_autoUpS(get("uev"))
	cat("My favourite Color: \n")
	cat(.doe$stn$favouriteColor)
} # EOF
