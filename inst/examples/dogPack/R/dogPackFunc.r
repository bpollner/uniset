#' @title Test Target Package Parameters
#' @description Simple test if setup is ok.
#' @return  Prints target package name, environment name, name of list-object,
#' name of folder where the local 'dogPack_settings.R' resides etc, 
#' and gives it back as (invisible) list.
#' @export
dogPack_test_targetPackageParams <- function(){

    out <- uniset::uniset_test(get("uev"))
    # this line, called from a function in the target package, in our example the package 'dogPack',
    # is only intended to test if everything was set up correctly.
    return(invisible(out))
} # EOF



#' @title Demo Update Settings
#' @description Update Settings
#' @param silent Logical. If a confirmation should be printed. Defaults to 'FALSE'.
#' @return A (invisible) list containing the key=value pairs from the settings file. 
#' @export
dogPack_demo_updateSettings <- function(silent=FALSE) {

    stn <- uniset::uniset_updateSettings(get("uev"), silent)
    # use this line in a function defined in the target package, in our example the package
    # 'dogPack', to manually read in and update the values from the settings file. 
    # A possible use could be to have a direct way to check if updating the values from the 
    # settings file works correctly. 
	return(invisible(stn))
} # EOF



#' @title Demo Autoupdate
#' @description AutoUpdate Settings. Is also reading the value of the
#' key 'favouriteColor' from the 'dogPack_settings.R' file.
#' @param txt Character
#' @export
dogPack_demo_autoUpS <- function(txt="My favourite color is ") {
	
	uniset::uniset_autoUpS(get("uev"))
	# As the developer of the target package, in our example the package 'dogPack', you would 
	# include this line at the top of each and every single function that you want to trigger the
	# auto-update mechanism, i.e. when the key=value pairs from the local 'dogPack_settings.R' 
	# file should be updated and read in.
	#	
	# This function ('uniset_autoUpS') is the one function that confers the main functionality of package 'uniset'.
	# By calling 'uniset_autoUpS(get("uev"))', the local key=value pairs from the settings file get updated according
	# to the template from the target package, and the object ('stn' in our example) containing the list with the key=value pairs is
	# updated/saved in the environment called, in our example, '.doe'.
	# Both names can be defined when using 'uniset' to generate the three required files. 

	
	stn <- get("stn", envir=get(".doe"))
	# Read in the 'stn' object from the environment '.doe'. Subsequently, cou can access values via
	# 'stn$xxx', with 'xxx' being any key defined in the settings file. 
	# It is also possible to directly access the values of a key via
	# '.doe$stn$xxx', but this throws now a note when running CMD-CHECK ("no visible binding for global variable '.doe'")
	
		
	
	#  Here would be the main body of the function.
	# ...
	# ... 
	# ...
	
	
	# Any key=value pairs of the settings file can be accessed in the body of a function defined 
	# in the target package, in our example this package 'dogPack', as shown below:


	# favColor <- .doe$stn$favouriteColor # it works, but would throw a note when running CMD-CHECK
	favColor <- stn$favouriteColor # we defined 'stn' in the beginning of the function
	
	txtOut <- paste0(trimws(txt), ": \n", favColor, "\n")
	cat(txtOut)	
	
	return(invisible(NULL))
} # EOF



#' @title Demo No Autoupdate
#' @description A function that is \strong{not} auto-updating the values from 
#' the settings file. Is also reading the value of the key 'favouriteColor' 
#' from the 'dogPack_settings.R' file.
#' @param txt Character
#' @export
dogPack_demo_No_autoUpS<- function(txt="My favourite color is ") {
	
	stn <- get("stn", envir=get(".doe"))
	# Read in the 'stn' object from the environment '.doe'. Subsequently, cou can access values via
	# 'stn$xxx', with 'xxx' being any key defined in the settings file. 
	# It is also possible to directly access the values of a key via
	# '.doe$stn$xxx', but this throws now a note when running CMD-CHECK ("no visible binding for global variable '.doe'")
	
		
	
	#  Here would be the main body of the function.
	# ...
	# ... 
	# ...
	
	
	# Any key=value pairs of the settings file can be accessed in the body of a function defined 
	# in the target package, in our example this package 'dogPack', as shown below:


	# favColor <- .doe$stn$favouriteColor # it works, but would throw a note when running CMD-CHECK
	favColor <- stn$favouriteColor # we defined 'stn' in the beginning of the function
	
	txtOut <- paste0(trimws(txt), ": \n", favColor, "\n")
	cat(txtOut)	
	
	return(invisible(NULL))

} # EOF
