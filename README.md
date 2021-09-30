<!-- badges: start -->
[![R-CMD-check](https://github.com/bpollner/uniset/workflows/R-CMD-check/badge.svg)](https://github.com/bpollner/uniset/actions)
[![codecov](https://codecov.io/gh/bpollner/uniset/branch/master/graph/badge.svg?token=QK8GPB9XLM)](https://codecov.io/gh/bpollner/uniset)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/uniset)](https://cran.r-project.org/package=uniset)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/uniset)](https://cran.r-project.org/package=uniset)
<img src='man/figures/logo.png' align="right" height="139" />
<!-- badges: end -->

# uniset 
Package `uniset` provides an easily accessible, user-friendly text file as settings-file for your R-package.

### Description 
Package `uniset` provides the means to outfit your package (the 'target package') with an accessible, user-friendly and easy to read text file where settings resp. various parameters can be saved. This settings file remains in place and can remain unchanged even when the target package is updated or re-installed.  
In order to enable the target package to make use of the functionality offered by package `uniset`, three files have to be exported by `uniset` and be placed into the target package.

### Advantage
The most imminent advantage of the `uniset` settings-file system over using simply a csv-file or an excel-file for permanently storing settings for any package is the fact that the key=value pairs in the 'xxx_settings.R' file get updated (added / deleted) dynamically. So the developer of a package can delete keys or introduce new ones, and the new key=value pairs will be automatically added to or deleted from the local 'xxx_settings.R' file. Values changed by the user of the target package will be preserved. So the author of the target package can add or delete keys from the 'xxx_settings.R' file without worrying that this will cause any effort or troubles for the user of the target package.

### Two ways to generate the required files
* **Export and Move Files**  Use **`uniset_getFiles`**, then move the 'xxx_settings.R' file ('xxx' for the name of your package) into the 'inst' folder (create one if not already done) of the target package. Move the file 'zzz.R" and the file 'uniset_globals.R' to the 'R' folder of the target package.  
In case that the '.onLoad' function already is defined, add the six lines of code from the file 'zzz.R' to your existing '.onLoad' function. 

* **Write files directly to target package (recommended)**  Alternatively, use **`uniset_copyFilesToPackage`** to copy the required files directly into the target package.  

### Accessing values from within target package
Every variable defined in the 'xxx_settings.R' file is accessible in the code of the target package. See the created 'xxx_settings.r' file for an example.  
The target package has to list `uniset` as an import, and then you can use the functions `uniset_updateSettings` or `uniset_autoUpS` called from a function **defined in the target package** to manually or automatically update the settings, i.e. to read in the key=value pairs stored in the 'xxx_settings.R' file and have them accessible in an environment created by the target package. See the examples at the documentation for `?uniset` and below.
***

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
We assume that we want to enable a package called `dogPack` with the settings-functionality provided by `uniset`.  
In this example, `dogPack` is the target package, and we want it to live at `*~/desktop*`. First copy the example folder `dogPack` to your desktop:
```
library(uniset)
to <- "~/desktop"
from <- paste0(path.package("uniset"), "/examples/dogPack")
file.copy(from, to, recursive = TRUE) 
```
### Generate Files
There are two ways to set up a target package (in our example the package called `dogPack`) to make use of `uniset`:

* **1) Export and Move Files**
With everything left at the defaults, a call to `uniset_getFiles` creates a folder containing the three required files on the desktop.  

```
uniset_getFiles("dogPack")
```
Move the 'dogPack_settings.r' file into the 'inst' folder (create one if not already done) of `dogPack`. Move the file 'zzz.r" and the file 'uniset_globals' to the 'R' folder of `dogPack`.  
In case that the '.onLoad' function already is defined, add the six lines of code from the file 'zzz.R' to your existing '.onLoad' function.  

* **2) Write files directly to target package**
(recommended) With everything left at the defaults, this call to `uniset_copyFilesToPackage()` copies the three required files directly into the target package -- called `dogPack` in our example, living directly on the desktop. 
```
path <- "~/desktop/dogPack"
uniset_copyFilesToPackage(path)
```
### Some more explanations
You can define functions **in dogPack** (what is in this example the target package) that can call the following three functions from `uniset`: 
* `uniset::uniset_test(get("uev"))`
* `uniset::uniset_updateSettings(get("uev"))`
* `uniset::uniset_autoUpS(get("uev"))`  
`uev` is a global constant defined in `dogPack`, handing over the name of the environment where necessary variables are stored.
`uniset_test` is merely a testing function to see if the handover of environments etc. is working properly.  
  

The functions `uniset_updateSettings` and `uniset_autoUpS` are:
* When called for the **first time**
  * Creating the required environment variable in your .Renviron file, and
  * copying the 'dogPack_settings.R' file to a folder in the users home directory. It is this file ('dogPack_settings.R) that is meant to be seen, read and modified by the **user** of package `dogPack`.
  
* When called subsequently, simply updating (adding / deleting) the key=value pairs in the local, user-level 'dogPack_settings.R' file according to a possibly new template in the `dogPack` installation folder. Thus, whenever the developer of package `dogPack` is introducing new or deleting obsolete key=value pairs, they will be automatically added to or deleted from the user´s file. Any values that the user modified will be preserved. Thus, **a new update or installation of package `dogPack` will not force the user of package `dogPack` to completely re-customize the 'dogPack_settings.R' file**. 
  
**In package `dogPack`**, you could now define functions as follows:
> `dogPack_test_targetPackageParams <- function(){uniset::uniset_test(get("uev"))}`  
> `dogPack_demo_updateSettings <- function(){uniset::uniset_updateSettings(get("uev"))}`  
> `dogPack_demo_autoUpS <- function(){uniset::uniset_autoUpS(get("uev"))}`  

The latter function is intended to be placed at the beginning of any function of package `dogPack` to, if desired, automatically source the local 'dogPack_settings.R' file into the environment called '.doe' (in our example). Thus, any values stored in the local 'dogPack_settings.R' file can be obtained via 
```
color <- .doe$stn$favouriteColor # does not work yet
```
In this example we obtain the value from the key 'favouriteColor' from the list called 'stn' in the environment called '.doe'. All these names (environment name, object name) can of course be customized when using the functions `uniset_getFiles` or `uniset_copyFilesToPackage`.  


### The real world test
Assuming you copied or moved the required files as described above, open the RStudio project file in the folder 'dogPack' on your desktop, build and install the package, and then call:
```
library(dogPack)
dogPack_test_targetPackageParams() # gives a printout of the target package parameters
dogPack_demo_updateSettings()
```
You probably have to restart R now for the changes in the environment variable in your .Renviron file to become effective.  
Now call again:
```
dogPack_demo_updateSettings()
```
#### Getting Values
Now everything should be ready and set up, and it is possible to obtain values from 'dogPack_settings.R' directly via: 
```
color <- .doe$stn$favouriteColor
color
```

Use the auto-update function within the code in the target package (ideally immediately at the beginning of a function) when you want to automatically source all the values from the local 'dogPack_settings.R' file into the environment '.doe':

```
dogPack_demo_autoUpS()
```
Now open the file 'dogPack_settings.R' in the folder 'dogPack_SH' in your home directory (if you left all at the defaults above), and change the value of the key 'favouriteColor'. After that, call

```
color <- .doe$stn$favouriteColor
color # should be the same as before
dogPack_demo_autoUpS()
color <- .doe$stn$favouriteColor
color # should have the new value
```
#### Setting Values
Of course it is also possible to locally set the value of a key in the 'stn' object via
```
.doe$stn$favouriteColor <- "lightyellow"
```
**Cave:** Be aware that a call to the auto-update function (defined in `uniset`) is re-instating the values from the 'dogPack_settings.R' file to the object 'stn' in the environment '.doe', but not when used with the key 'gen_autoUpdateSettings' previously set to 'FALSE'.

```
.doe$stn$favouriteColor # should be "lightyellow"
dogPack_demo_autoUpS(F)
.doe$stn$favouriteColor # should be the value you assigned before
```
Including the auto-update function `uniset::uniset_autoUpS` as demonstrated in `dogPack_demo_autoUpS` is of course not required – the key=value pairs in the settings file can be accessed anyway. Practically, it makes sense to include the auto-update in every function that the user of the target package can call, and to not include it in all the other functions.
```
.doe$stn$favouriteColor # your value
.doe$stn$favouriteColor <- "green"
.doe$stn$favouriteColor # should be green
dogPack_demo_No_autoUpS(F)
.doe$stn$favouriteColor # still green
#
dogPack_demo_autoUpS(F) # now update the values
.doe$stn$favouriteColor # your value again
```
#### Adding / deleting keys
If you are the developer of package `dogPack`, at some time after you published `dogPack` you might want to add keys to or delete from the settings file.  
Do that now: Add e.g. a new 'key=value,' pair (do not forget the comma ',') anywhere in the settings-file **in the folder 'inst' of `dogPack`**, and re-install `dogPack`. Or, as an alternative shortcut for this demonstration, go to root of the *installed* `dogPack` package at

```
path.package("dogPack")
```
and simply modify (add a key) there the file 'dogPack_settings.R'.

Now the **user** of `dogPack` calls a function that includes the auto-update function `uniset::uniset_autoUpS`:

```
dogPack_demo_autoUpS(F)
```
The **user** of `dogPack` will then have the new key added to the local file 'dogPack_settings.R' in the folder 'dogPack_SH' in the home directory (default location).  

The same is true for deleting keys: again, open the file 'dogPack_settings.R' in the root of `path.package("dogPack")` and delete any key. 

Now the **user** of `dogPack` calls again a function that includes the auto-update function `uniset::uniset_autoUpS`:

```
dogPack_demo_autoUpS(F)
```
and surplus keys will be deleted from the local file 'dogPack_settings.R'.

Enjoy !
