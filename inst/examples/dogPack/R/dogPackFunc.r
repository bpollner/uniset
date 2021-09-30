#' @title Test Target Package Parameters
#' @description Simple test if setup is ok.
#' @return  Prints target package name, environment name, name of list-object,
#' name of folder where the local 'dogPack_settings.R' resides etc.
#' @export
dogPack_test_targetPackageParams <- function(){

    uniset::uniset_test(get("uev"))
    # this line, called from a function in the target package, in our example the package 'dogPack',
    # is only intended to test if everything was set up correctly.
    
} # EOF



#' @title Demo Update Settings
#' @description Update Settings
#' @export
dogPack_demo_updateSettings <- function() {

    uniset::uniset_updateSettings(get("uev"))
    # use this line in a function defined in the target package, in our example the package
    # 'dogPack', to manually read in and update the values from the settings file. 
    # A possible use could be to have a direct way to check if updating the values from the 
    # settings file works correctly. 

} # EOF



#' @title Demo Autoupdate
#' @description AutoUpdate Settings. Is also reading the value of the
#' key 'favouriteColor' from the 'dogPack_settings.R' file.
#' param txt Character
#' @export
dogPack_demo_autoUpS <- function(txt="My favourite color is ") {

	.doe <- NULL # that is to avoid a note when running CMD-CHECK saying "no visible binding for global variable '.doe'..."
	
	uniset::uniset_autoUpS(get("uev"))
	# As the developer of the target package, in our example the package 'dogPack', you would 
	# include this line at the top of each and every single function that you want to access values from the settings file. 
	# this function ('uniset_autoUpS') is the one function that confers the main functionality of package 'uniset'.
	# By calling 'uniset_autoUpS(get("uev"))', the local key=value pairs from the settings file get updated according
	# to the template from the target package, and the object ('stn' in our example) containing the list with the key=value pairs is
	# updated/saved in the environment called, in our example, '.doe'.
	
	
	#  Here would be the main body of the function.
	# ...
	# ... 
	# ...
	
	
	# Any key=value pairs of the settings file can be accessed in the body of a function defined 
	# in the target package, in our example the package 'dogPack', as shown below. 
	# '.doe' is the name of the environment, and 'stn' is the name of the object holding the key=value
	# pairs defined in this environment. 
	# Both names can be defined when using 'uniset' to generate the three required files. 
	
	favColor <- .doe$stn$favouriteColor
	
	txtOut <- paste0(txt, favColor, "\n")
	cat(txtOut)	
	return(invisible(NULL))
} # EOF
