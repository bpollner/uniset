#############################################################################################
######################### On-Load function for package 'XXX_packageName' ####################
#############################################################################################


# Move this file called "zzz.r" into the "R" folder of your package 'XXX_packageName'.

# Or, alternatively, if you already have a file containing the '.onLoad' function in your package 'XXX_packageName', 
# add the 6 lines of code below to your '.onLoad' function

######## !! Do NOT change the names of the variables. It will cripple the functionality of the package "uniset" !! ############

.onLoad <- function(libname, pkgname) {
	.GlobalEnv$XXX_unisetEnv <- new.env()										## creates a new environment called "XXX_unisetEnv"
	XXX_unisetEnv$pkgUniset_UserPackageName <- "XXX_packageName"				## character length one, the name of the package using the uniset system
	XXX_unisetEnv$pkgUniset_RenvironSettingsHomeName <- "XXX_SH"				## character length one, the name of the variable in the .Renviron file that contains the path to the user-defined settings-home
	XXX_unisetEnv$pkgUniset_EnvironmentName <- "XXX_targetEnv"					## character length one, the name of the environment containing the settings for the package using 'uniset. It is recommended to use a rather short name starting with a '.' dot.
	XXX_unisetEnv$pkgUniset_SettingsObjectName <- "XXX_obj"						## character length one, the name of the object (within the environment defined above) that is containing the settings-list. This *MUST* be the same name as the name of the list in the settings.r file 
	XXX_unisetEnv$pkgUniset_SuffixForTemplate <- "XXX_template"					## the character string that should be appended to the fresh settings file that is copied to the users settings home directory
} # EOF
