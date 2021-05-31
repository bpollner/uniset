# uniset
Package 'uniset' provides an easily accessible, user-friendly text file as settings-file for your R-package.

### Description 
Package 'uniset' provides the means to outfit your package (the 'target package') with an accessible, user-friendly and easy to read text file where settings resp. various parameters can be saved. This settings file remains in place and can remain unchanged even when the target package is updated or re-installed.  
In order to enable the target package to make use of the functionality offered by package 'uniset', three files have to be exported by 'uniset' and be placed into the target package.

### Advantage
The most imminent advantage of the 'uniset' settings-file system over using simply a csv-file or an excel-file for permanently storing settings for any package is the fact that the key=value pairs in the xxx_settings.R file get updated (added / deleted) dynamically. So the developer of a package can delete keys or introduce new ones, and the new key=value pairs will be (with few exceptions) automatically added to or deleted from the local xxx_settings.R file. Values changed by the user of the target package will be preserved. So the author of the target package can add or delete keys from the xxx_settings.R file without worrying that this will cause any effort or troubles for the user of the target package. (Few exceptions can apply, see the text in the generated xxx_settings.R file.)

### Two ways to generate the required files
* **Export and Move Files**  Use *uniset_getFiles*, then move the 'xxx_settings.r' file ('xxx' for the name of your package) into the 'inst' folder (create one if not already done) of the target package. Move the file 'zzz.r" and the file 'uniset_globals' to the 'R' folder of the target package.  
In case that the '.onLoad' function already is defined, add the six lines of code from the file 'zzz.R' to your existing '.onLoad' function. 
* **Write files directly to target package**  Alternatively, use *uniset_copyFilesToPackage* to copy the required files directly into the target package.  


### Accessing values from target package
Every variable defined in the xxx_settings.r file is accessible in the code of the target package. See the created 'xxx_settings.r' file for an example.  
The target package has to list 'uniset' as an 'import', and then you can use the functions *uniset_updateSettings* or *uniset_autoUpS* called from a function **defined in the target package** to manually or automatically update the settings, i.e. to read in the key=value pairs stored in the xxx_settings.r file and have them accessible in an environment created by the target package. See the examples at the documentation for *?uniset* and below.
***

## Installation
From Cran XXX
```
xxxxxx
```
Or download from github:
```
library(devtools)
install_github(repo="bpollner/uniset", ref="master")
```
***
## Usage
We assume that we want to enable a package called *dogPack* with the settings-functionality provided by 'uniset'.  
In this example, 'dogPack' is the target package, and we want it to live at '*~/desktop*'. First copy the example folder 'dogPack' to your desktop:
```
library(uniset)
to <- "~/desktop"
from <- paste0(path.package("uniset"), "/examples/dogPack")
file.copy(from, to, recursive = TRUE) 
```
* **Export and Move Files**
With everything left at the defaults, this call to 'uniset_getFiles' creates a folder containing the three required files on the desktop. 
```
uniset_getFiles("dogPack")
```
Move the 'dogPack_settings.r' file into the 'inst' folder (create one if not already done) of 'dogPack'. Move the file 'zzz.r" and the file 'uniset_globals' to the 'R' folder of 'dogPack'.  
In case that the '.onLoad' function already is defined, add the six lines of code from the file 'zzz.R' to your existing '.onLoad' function.  

* **Write files directly to target package**
With everything left at the defaults, this call to *uniset_copyFilesToPackage()* copies the three required files directly into the target package -- called 'dogPack' in our example, living directly on the desktop.
```
path <- "~/desktop/dogPack"
uniset_copyFilesToPackage(path)
```
Now you can define functions **in 'dogPack'** that can call the following functions from 'uniset': 
```
uniset::uniset_test(get("uev"))
uniset::uniset_updateSettings(get("uev))
uniset::uniset_autoUpS(get("uev"))
```
*uev* is a global constant defined in 'dogPack', handing over the name of the environment where necessary variables are stored.
'uniset_test' is merely a testing function to see if the handover of environments etc. is workign properly.  
  
'uniset_updateSettings' and 'uniset_autoUpS' is:
* When called for the **first time**
  * Creating the required environment variable in your .Renviron file, and
  * copying the 'dogPack_settings.R' file to a folder in the users home directory. It is this file ('dogPack_settings.R) that is meant to be seen, read and modified by the **user** of package 'dogPack'.
* When called subsequently, simply updating (adding / deleting) the key=value pairs in the local, user-level 'dogPack_settings.R' file according to a possibly new template in the 'dogPack' installation folder. Thus, whenever the developer of package 'dogPack' is introducing new or deleting obsolete key=value pairs, they will be (usually) automatically added to or deleted from the user´s file. Any values that the user modified will be preserved. Thus, a new update or installation of package 'dogPack' will not force the user of package 'dogPack' to completely re-customize the 'dogPack_settings.R' file. 
  
In package 'dogPack', you could now define functions as follows:
```
dogPackTest <- function(){
    uniset::uniset_test(get("uev"))
} # EOF
#
dogPack_updateSettings <- function() {
    uniset::uniset_updateSettings(get("uev"))
} # EOF
#
dogPack_autoUpS <- function() {
	uniset::uniset_autoUpS(get("uev"))
 	 cat("My favourite Color: \n")
	cat(.doe$stn$favouriteColor)
  # and some more code ... 
} # EOF
```
The latter function is intended to be placed at the beginning of any function of package 'dogPack' to always (if desired) automatically source the local 'dogPack_settings.R' file into the environment called '.doe' (in our example). Thus, any values stored in the local 'dogPack_settings.R' file can be obtained via
```
color <- .doe$stn$favouriteColor
```
In this example we obtain the value from the key 'favouriteColor' from the list called 'stn' in the environment called '.doe'. All these names (environmen name, object name) can of course be customized when using the functions *uniset_getFiles* or *uniset_copyFilesToPackage*.  
To try this out, open the RProject project file in the folder 'dogPack', build and install the package, and then call:
```
dogPackTest() # should give a nice printout
dogPack_updateSettings()
```
You might have to restart R now for the changes in the environment variable in your .Renviron file to become effective.  
Now call again:

```
dogPack_updateSettings()
```
Now everything should be ready and set up.  

Use the auto-update function within your code when you want to automatically source all the values from the local 'dogPack_settings.R' file into the environment '.doe':
```
dogPack_autoUpS
```
Enjoy !
