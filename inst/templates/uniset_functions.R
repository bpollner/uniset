#################################################################################################################
######################## Custom-tailored functions for package 'XXX_packageName' ################################
#################################################################################################################



# If not already there, move this file called "uniset_functions.R" into the "R" folder of 
# your package 'XXX_packageName'

# The following three functions are custom-tailored by package 'uniset', and intended to be used inside 
# functions defined in the package 'XXX_packageName':



# Can be used inside a function of the package 'XXX_packageName' to manually update the settings.
# If silent=FALSE, upon success a message will be displayed.
updateSettings <- function(silent=FALSE) {
	 stn <- uniset::uniset_updateSettings(get("uniset_env_name"), setupFunc="XXX_setupFuncName", silent)
	return(invisible(stn))
} # EOF



# Can be used inside a function of the package 'XXX_packageName' to automatically update the settings.
# No message will be displayed upon success.
autoUpS <- function() {
	stn <- uniset::uniset_autoUpS(get("uniset_env_name"), setupFunc="XXX_setupFuncName")
	return(invisible(stn))
} # EOF



# Can be used inside a function of the package 'XXX_packageName' to conveniently get the list holding the settings, 
# i.e. the key=value pairs from the file XXX_packageName_settings.R
getstn <- function(){
    stn <-  try(get("XXX_obj", envir=get("XXX_targetEnv", pos="pkg_XXX_packageName_envs")), silent=TRUE)
    if (class(stn) == "try-error") {
    	# if no manual 'updateSettings' has been called yet, this will throw an error. Hence, we have to force the 
    	# manual update here.
        uniset::uniset_updateSettings(get("uniset_env_name"), setupFunc="XXX_setupFuncName", silent=TRUE)
        stn <-  try(get("XXX_obj", envir=get("XXX_targetEnv", pos="pkg_XXX_packageName_envs")), silent=FALSE)
    }
    return(stn)
} # EOF
