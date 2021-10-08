########################################################################################################
##################### .onLoad and .onUnload functions for package 'XXX_packageName' ####################
########################################################################################################


# If not already there, move this file called "zzz.R" into the "R" folder of 
# your package 'XXX_packageName'.

# Or, alternatively, if you already have a file containing the '.onLoad' function in your package 'XXX_packageName', 
# add the eight lines of code below to your '.onLoad' function, and add the one lines of code to your '.onUnload' function. 

######## Do NOT change the names of the variables. It will cripple the functionality of the package "uniset" ############

.onLoad <- function(libname, pkgname) {
	nsp <- "pkg_XXX_packageName_envs"												## is defining the name on the search path
	if (!any(grepl(nsp, search()))) {attach(what=NULL, name=nsp)}					## create a new entry on the search path if not already there
	assign("XXX_unisetEnv", new.env(), pos=nsp)										## create a new environment called "XXX_unisetEnv"
	assign("pkgUniset_UserPackageName","XXX_packageName", envir=XXX_unisetEnv)		## the name of the target package using the uniset system
	assign("pkgUniset_RenvironSettingsHomeName","XXX_SH", envir=XXX_unisetEnv)		## the name of the variable in the .Renviron file that contains the path to the user-defined settings-home
	assign("pkgUniset_EnvironmentName","XXX_targetEnv", envir=XXX_unisetEnv)		## the name of the environment containing the settings for the package using 'uniset. It is recommended to use a rather short name starting with a '.' dot.
	assign("pkgUniset_SettingsObjectName","XXX_obj", envir=XXX_unisetEnv)			## the name of the object (within the environment defined above) that is containing the settings-list. This *MUST* be the same name as the name of the list in the settings.R file 
	assign("pkgUniset_SuffixForTemplate","XXX_template", envir=XXX_unisetEnv)		## the character string that should be appended to the fresh settings file that could be copied to the users settings home directory
} # EOF

.onUnload <- function(libpath) {
	if (any(grepl("pkg_XXX_packageName_envs", search()))) {detach(name="pkg_XXX_packageName_envs")}
} # EOF
