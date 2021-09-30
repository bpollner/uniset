#' @title dogPack TEst
#' @description Simple test if setup is ok.
#' @return  Prints target package name, environment name, name of list-object,
#' name of folder where the local 'dogPack_settings.R' resides etc.
#' @export
dogPackTest <- function(){

    uniset::uniset_test(get("uev"))
    # this line, called from a function in the target package, in our example the package 'dogPack',
    # is only intended to test if everything was set up correctly.
    
} # EOF



#' @title Update Settings
#' @description Update Settings
#' @export
dogPack_demo_updateSettings <- function() {

    uniset::uniset_updateSettings(get("uev"))
    # use this line in a function defined in the target package, in our example the package
    # 'dogPack', to manually read in and update the values from the settings file. 
    # A possible use could be to have a direct way to check if updating the values from the 
    # settings file works correctly. 

} # EOF



#' @title Test Autoupdate
#' @description AutoUpdate Settings. Is also reading the value of the
#' key 'favouriteColor' from the 'dogPack_settings.R' file.
#' @export
dogPack_demo_autoUpS <- function() {

	.doe <- NULL # that is to avoid a note when running CMD-CHECK saying "no visible binding for global variable '.doe'..."
	
	uniset::uniset_autoUpS(get("uev"))
	# As the developer of the target package, in our example the package 'dogPack', you would 
	# include this line at the top of each and every single function that wants to access 
	# values from the settings file. 
	# this function ('uniset_autoUpS') is the one function that confers the main functionality of package 'uniset'.
	
	favouriteColor <- .doe$stn$favouriteColor
	
	cat("My favourite Color: \n")
	cat(favouriteColor)
	
} # EOF
