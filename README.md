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
The target package has to list 'uniset' as an 'import', and then you can use the functions *uniset_updateSettings* or *uniset_autoUpS* called from a function **defined in the target package** to manually or automatically update the settings, i.e. to read in the key=value pairs stored in the xxx_settings.r file and have them accessible in an environment created by the target package. See the examples at the documentation for *?uniset*.
***

## Installation
From Cran XXX
```
asdfasdf
```
Or download grom github:
```
library(devtools)
devtools_install()
```

