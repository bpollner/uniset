###########################################################################################################
#################### Global variable for package 'XXX_packageName' ################################
##########################################################################################################



# Move this file called "uniset_globals.r" into the "R" folder of your package 'XXX_packageName'

# Or, alternatively, if you already have a file defining global variables, just add the one line defining the 
# global variable 'uniset_env_name' to it. 

# Change neither the object name, nor the value of the object-value pair below. 

uniset_env_name <- "XXX_unisetEnv" 

# Explanation: 
# this is the name of the environment holding the !!uniset!! variables defining the target package name etc. in the '.onLoad' function in file 'zzz.r'
# it is highly recommended that the package name is included in the name of the environment, so that the resulting name of the environment is unique.
# This is necessary so that more than one loaded package can make use of 'uniset'
# These lines are for explanation only, do not change any of the object-value pair above (unless you know exactly what you are doing.... :-) )
