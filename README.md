<!-- badges: start -->
[![R-CMD-check](https://github.com/bpollner/uniset/workflows/R-CMD-check/badge.svg)](https://github.com/bpollner/uniset/actions)
[![codecov](https://codecov.io/gh/bpollner/uniset/branch/master/graph/badge.svg?token=QK8GPB9XLM)](https://app.codecov.io/gh/bpollner/uniset?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/uniset)](https://cran.r-project.org/package=uniset)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/uniset)](https://cran.r-project.org/package=uniset)
<img src='man/figures/logo.png' align="right" height="139" />
<!-- badges: end -->

# uniset 
Package `uniset` provides an easily accessible, user-friendly text file as settings-file for your R-package.

### Description 
Package `uniset` enables any package (the 'target package') to provide its users an easily accessible, user-friendly and human readable text file where key=value pairs (used by functions defined in the target package) can be saved. This settings file lives in a location defined by the user of the target package, and its user-defined values remain unchanged even when the author of the target package is introducing or deleting keys, or when the target package is updated or re-installed. 
In order to enable the target package to make use of the functionality offered by package `uniset`, four files have to be exported by `uniset` and be placed into the target package.

### Advantage
The most imminent advantage of the `uniset` settings-file system over using any static file for permanently storing settings for any package is the fact that the key=value pairs in the 'settings.R' file get updated (added / deleted) dynamically. So the developer of a package can delete keys or introduce new ones, and the new key=value pairs will be automatically added to or deleted from the local 'settings.R' file. Values changed by the user of the target package will be preserved. So the author of the target package can add or delete keys from the 'settings.R' file without worrying that this will cause any effort or troubles for the user of the target package.

### Two ways to generate the required files
* **1) Export and Move Files**  Use **`uniset_getFiles`**, then move the 'xxx_settings.R' file ('xxx' for the name of the target package) into the 'inst' folder (create one if not already done) of the target package. Move the file 'zzz.R', 'uniset_globals.R' and 'uniset_functions.R' to the 'R' folder of the target package.  
In case that the '.onLoad' function already is defined, add the eight lines of code from the file 'zzz.R' to your existing '.onLoad' function. In case that the '.onUnLoad' function already is defined, add the one line of code from the file 'zzz.R' to your existing '.onUnLoad' function. 


* **2) Write files directly to target package (recommended)**  Alternatively, use **`uniset_copyFilesToPackage`** to copy the required files directly into the target package.  

In both cases the name of a setup-function has to be provided, that is the name of the function defined in the target package that contains the `uniset` function `uniset_setup`.

### Accessing values from within target package
Every variable defined in the 'settings.R' file is accessible in the code of the target package. The target package has to list `uniset` as an import, and then the functions `uniset_updateSettings` or `uniset_autoUpS` called from a function **defined in the target package** can be used to manually or automatically update the settings, i.e. to read in the key=value pairs stored in the 'settings.R' file and have them accessible in an environment created by the target package. See below for a practical example.

## Installation
Install release version from CRAN via
```
install.packages("uniset") 
```
Or download from github:
```
library(devtools)
install_github(repo="bpollner/uniset", ref="master")
```
***
## Usage
### Set up example
We assume that we want to enable a package called `dogPack` to use the dynamic settings file provided by `uniset`. 
In this example, `dogPack` is the target package, and we want it to live at `~/desktop`. First copy the example folder `dogPack` to your desktop:
```
library(uniset)
to <- "~/desktop"
from <- paste0(path.package("uniset"), "/examples/dogPack")
file.copy(from, to, recursive = TRUE) 
```
### Generate Files
There are two ways to set up a target package (in our example the package called `dogPack`) to make use of `uniset`:

* **1) Export and Move Files**
The package name, the name of the setup-function and a location have to be specified, and then a call to `uniset_getFiles` creates a folder containing the four required files:  
```
setupFunc <- "dogPack_demo_setup" # the name of the setup-function in our example
to <- "~/desktop"
uniset_getFiles("dogPack", setupFunc, to)
```
Move the 'dogPack_settings.R' file into the 'inst' folder (create one if not already done) of `dogPack`. Move the file 'zzz.R', 'uniset_globals' and 'uniset_functions.R' into the 'R' folder of `dogPack`.

* **2) Write files directly to target package**
(recommended) A call to `uniset_copyFilesToPackage` copies the four required files directly into the target package -- called `dogPack` in our example, living directly on the desktop. 
```
path <- "~/desktop/dogPack"
setupFunc <- "dogPack_demo_setup" # the name of the setup-function in our example
uniset_copyFilesToPackage(path, setupFunc)
```
### Some more explanations
You can define functions **in the target package** (what is in this example the package 'dogPack') that can call the following four functions from `uniset`: 
* `uniset_setup(where="somePath", get("uev"))`
* `uniset_test(get("uev"))`
* `uniset_updateSettings(get("uev"), "nameOfSetupFunction")`
* `uniset_autoUpS(get("uev"), "nameOfSetupFunction")`  
`uev` is a global constant defined in the target package, handing over the name of the environment where necessary variables are stored.
`uniset_test` is merely a testing function to see if the handover of environments etc. is working properly. 
 `"nameOfSetupFunction"`is the name of the function defined in the target package that is containing the `uniset` setup-function `uniset_setup`.

The function `uniset_setup` is:
* Creating the required environment variable in the .Renviron file, and
* copying the 'dogPack_settings.R' file to a destination specified by the user of 'dogPack'. It is this file ('dogPack_settings.R) that is meant to be seen, read and modified by the **user** of package `dogPack`.
  
The functions `uniset_updateSettings` and `uniset_autoUpS` are updating (adding / deleting) the key=value pairs in the local, user-level 'dogPack_settings.R' file according to a possibly new template in the `dogPack` installation folder. Thus, whenever the developer of package `dogPack` is introducing new or deleting obsolete key=value pairs, they will be automatically added to or deleted from the user´s file. Any values that the user modified will be preserved. Thus, **a new update or installation of package `dogPack` will not force the user of package `dogPack` to completely re-customize the 'dogPack_settings.R' file**. 

Please review the file 'dogPackFunc.R' in the folder 'dogPack/R' that was copied to the desktop previously and look at the code of the practical examples that will be executed below. 

Try accessing a value from the settings file as described in 'dogPackFunc.R/dogPack_demo_autoUpS()'. It will not work yet.

```
color <- .dogPack_settingsEnv$settings$favouriteColor # does not work yet
```
(In this example we try to obtain the value from the key 'favouriteColor' from the list called 'settings' in the environment called '.dogPack_settingsEnv'.)


### The real world test
Assuming you copied or moved the required files as described above, open the RStudio project file in the folder 'dogPack' on your desktop, build and install the package, and then call:
```
library(dogPack)
dogPack_test_targetPackageParams() # gives a printout of the target package parameters
dt <- "~/desktop"
dogPack_demo_setup(where=dt) # will place the settings-home folder on the desktop
```
You probably have to restart R now for the changes in the environment variable in your .Renviron file to become effective.  
Now call:
```
library(dogPack) # if you had to restart R
dogPack_demo_updateSettings()
```
#### Getting Values
By now everything should be ready and set up. You can look at the settings via
```
stn <- dogPack_demo_updateSettings()
str(stn)
```
It is possible to obtain values from 'dogPack_settings.R' directly via: 

```
color <- .dogPack_settingsEnv$settings$favouriteColor
color
```
However, it is preferable to use the function `getstn()` defined in package 'dogPack' to directly obtain the settings list – see the example code in `dogPack_demo_autoUpS()` and `dogPack_demo_tellFavouriteColor()` in the example folder copied previously. (`getstn()` was customized by package `uniset` at the time of creating the four required files and is located in the file 'uniset_functions.R' in 'dogPack/R'.)

```
dogPack_demo_tellFavouriteColor()
```
Now open the file 'dogPack_settings.R' in the folder 'dogPack_SH' that was created during setup, and change the value of the key 'favouriteColor' to "orange". After that, call

```
dogPack_demo_tellFavouriteColor() # should be still "blue"
dogPack_demo_autoUpS()
dogPack_demo_tellFavouriteColor() # should be "orange" now
```
**Cave:** Be aware that a call to the auto-update function (defined in `uniset`) is re-instating the values from the 'dogPack_settings.R' file to the object 'settings' in the environment '.dogPack_settingsEnv', but not when used with the key 'gen_autoUpdateSettings' in the settings.R file previously set to 'FALSE'.

Including the auto-update function `uniset::uniset_autoUpS` as demonstrated in `dogPack_demo_autoUpS` is of course not required – the key=value pairs in the settings file can be accessed anyway. Practically, it makes sense to include the auto-update in every function that the user of the target package can call, and to not include it in all the other functions.

Go to the local settings.R file (in the settings-home folder at `~/desktop`)and change the key 'favouriteColor' back to "blue".
```
dogPack_demo_tellFavouriteColor() # the old value ("orange")
dogPack_demo_No_autoUpS(F)
dogPack_demo_tellFavouriteColor() # still orange
#
dogPack_demo_autoUpS(F) # now update the values
dogPack_demo_tellFavouriteColor() # should be "blue" again
```
#### Adding / deleting keys
If you are the developer of package `dogPack`, at some time after you published `dogPack` you might want to add keys to or delete from the settings file. 
Do that now: Add e.g. a new 'key=value,' pair (do not forget the comma ',') anywhere in the settings-file **in the folder 'inst' of `dogPack`**, and re-install `dogPack`. Or, as an alternative shortcut for this demonstration, go to root of the *installed* `dogPack` package at

```
path.package("dogPack")
```
and there simply modify (add a key) the file 'dogPack_settings.R'.

Now the **user** of `dogPack` calls a function that includes the auto-update function `uniset::uniset_autoUpS`:

```
dogPack_demo_autoUpS(F)
```
The **user** of `dogPack` will then have the new key added to the local file 'dogPack_settings.R' in the folder 'dogPack_SH' in the settings-home directory.  

The same is true for deleting keys: again, open the file 'dogPack_settings.R' in the root of `path.package("dogPack")` and delete any key. 

Now the **user** of `dogPack` calls again a function that includes the auto-update function `uniset::uniset_autoUpS`:

```
dogPack_demo_autoUpS(F)
```
and surplus keys will be deleted from the local file 'dogPack_settings.R'.

Enjoy !
