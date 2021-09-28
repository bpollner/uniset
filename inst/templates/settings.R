#######################################################################################################
######################## Settings file for package "XXX_packageName" ##################################
#######################################################################################################


# If not already there, move this file called 'XXX_actualSettingsName' into the "inst" folder of 
# your package 'XXX_packageName'. (create an "inst" folder if it does not exist)

# do NOT change the name of the object holding the list - in this case, 'XXX_obj'

# use the code 'XXX_targetEnv$XXX_obj$KEY' (with 'KEY' being any of the key=value pairs defined below) 
# in the target package to access the values of the object 'XXX_obj'.

XXX_obj <- list(
	# tag = value, # with a comma !!
		
	## general behavior
	gen_autoUpdateSettings = TRUE, 			## Do not delete this variable (but of course you can change its value)
	
	
		
	## block 1 (describe what this collection of variables is about)
	block1 = "foo",							## add a comment to describe what variables are about
	block1_2 = TRUE,	
	willBeDeleted1 = 5,							## you can specify any type of variable
	block1_nameThemWhateverYouWant = "Henry", 
	anyName = "anyValue", 



	## block 2 (describe what this collection of variables is about)
	block2 = "bar",	
	block2_2 = 12345,	
	comboDelete1 = "theo",
	block2_oneMoreVariable = FALSE,



	## block 3 (describe what this collection of variables is about)
	block3_giveMeaningfulNames = TRUE,  
	favouriteColor = "blue", 
	willBeDeleted2 = "darkred", 
	comboDelete2 = TRUE, 
	comboDelete3 = FALSE, 



	block4 = "you get the picture",	
	blabla = TRUE, 
	andSoOn = 999999,
	petterson = "old",
	findus = "cat",
	mouse = "grey", 



	obey = TRUE,
	willBeDeleted3 = FALSE,
	strength = 5000,
	
	
	
	######
	last = 0 # do not add anything below that
	## the last one without comma !!
) # end of list called 'XXX_obj'


# any of these key=value pairs can be accessed in the code of your package 'XXX_packageName' by using simply
# XXX_targetEnv$XXX_obj$favouriteColor, for example. Instead of 'favouriteColor' any other of the keys defined
# in the list above can be used. 
# You can create as many key=value pairs as you want. 

# Key=value pairs have to specified exactly in the format as shown above, with a ',' (comma) after the value!
# (as this is nothing but a simple list, and in a list the key=value pairs have to be separated by a comma.)
# The keys have to be unique (obviously). 

# Do not change the name of the object holding the list - in this case, 'XXX_obj'

# empty lines and comments can be introduced or deleted at the users wish
# it is recommended to place comments, explanations etc. to a specific key directly into the line holding the key.
# comments have to be preceeded by a '#' 
