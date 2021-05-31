#######################################################################################################
######################## Settings file for package "dogPack" ##################################
#######################################################################################################


# If not already there, move this file called 'dogPack_settings.R' into the "inst" folder of 
# your package 'dogPack'. (create an "inst" folder if it does not exist)

# do NOT change the name of the object holding the list - in this case, 'stn'

# use the code '.doe$stn$KEY' (with 'KEY' being any of the key=value pairs defined below) 
# in the target package to access the values of the object 'stn'.

stn <- list(
	# tag = value, # with a comma !!
		
		## general behavior
	gen_autoUpdateSettings = TRUE, 			## Do not delete this variable (but of course you can change its value)
	
	
		
	## block 1 (describe what this collection of variables is about)
	block1 = "foo",							## add a comment to describe what variables are about
	block1_2 = TRUE,	
	willBeDeleted1 = 5,							## you can specify any type of variable
	block1_nameThemWhateverYouWant = "Henry", 
	anyName = "anyValue", 



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
) # end of list called 'stn'


# any of these key=value pairs can be accessed in the code of your package 'dogPack' by using simply
# .doe$stn$favouriteColor, for example. Instead of 'favouriteColor' any other of the keys defined
# in the list above can be used. 
# You can create as many key=value pairs as you want. 

# Key=value pairs have to specified exactly in the format as shown above, with a ',' (comma) after the value!
# (as this is nothing but a simple list, and in a list the key=value pairs have to be separated by a comma.)
# The keys have to be unique (obviously). 

# Do not change the name of the object holding the list - in this case, 'stn'

# If you add a new variable within a block *below the first* variable of the block, 
# this variable gets simply inserted into the settings file of the user. (Only the first variable
# of a block acts as a hook.)
# There can be any number of empty lines between blocks. 


# If you add a new block, or you add a variable *above the first* variable of the block,
# the user´s settings-file can not be updated any more, and the user has to manually insert
# the changes into the settings-file.
# Therefore it is recommended to pre-generate some blocks - depending on the complexity of your package,
#' so that the user of your package will not have to manually insert the new variables into the settings file.
