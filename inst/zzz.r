###########################################################################
######################### From Package Uniset #############################
###########################################################################


# Move this file called "zzz.r" into the "R" folder of the package that you want to enable to use the package "uniset".

# Or, alternatively, if you already have a file containing the ".onLoad" function, add the 6 lines of code below to your ".onLoad" function

# Change the character values (e.g. "MyPackageName", ".mse") to the values of your liking.

######## !! Do NOT change the names of the variables. It will cripple the functionality of the package "uniset" !! ############


.onLoad <- function(libname, pkgname) {

    .GlobalEnv$.unisetVars <- new.env()												## creates a new environment called ".unisetVars"

	.unisetVars$pkgUniset_UserPackageName <- "MyPackageName"						## character length one, the name of the package using the uniset system

    .unisetVars$pkgUniset_RenvironSettingsHomeName <- "MyPackageName_SettingsHome"	## character length one, the name of the variable in the .Renviron file that contains the path to the user-defined settings-home

	.unisetVars$pkgUniset_EnvironmentName <- ".mse"									## character length one, the name of the environment containing the settings. It is recommended to use a rather short name starting with a '.' dot.

	.unisetVars$pkgUniset_SettingsObjectName <- "stn"								## character length one, the name of the object (within the environment defined above) that is containing the settings-list. This *MUST* be the same name as the name of the list in the settings.r file (in the default template, this is 'stn' in line 5)

	.unisetVars$pkgUniset_SuffixForTemplate <- "_TEMPLATE"							## the character string that should be appended to the fresh settings file that is copied to the users settings home directory

} # EOF
