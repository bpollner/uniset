###########################################################################################
######################## Settings file for package "XXX" ##################################
###########################################################################################

stn <- list(
	# tag = value, # with a comma !!
	
	
	## general behavior
	gen_autoUpdateSettings = TRUE, 								## Do not delete this variable (but of course you can change its value)
	
	
	
	
	
	## block 1 (describe what this collection of variables is about)
	block1_1 = "foo",				## add a comment to describe what this variable is about
	block1_2 = TRUE,					## add a comment to describe what this variable is about
	block1_3 = 5,					## you can specify any type of variable
	block1_nameThemWhateverYouWant = "Henry", 




	## block 2 (describe what this collection of variables is about)
	block2_1 = "bar",				## add a comment to describe what this variable is about
	block2_2 = 12345,				## add a comment to describe what this variable is about
	block2_oneMoreVariable = FALSE,



	## block 3 (describe what this collection of variables is about)
	block3_giveMeaningfulNames = TRUE,




	##
	last = 0
	## the last one without comma !!
) # end of list


# If you add a new variable within a block *below the first* variable of the block, 
# this variable gets simply inserted into the settings file of the user. (Only the first variable
# of a block acts as a hook.)

# If you add a new block, or you add a variable *above the first* variable of the block,
# the user´s settings-file can not be updated any more, and the user has to manually insert
# the changes into the settings-file.
