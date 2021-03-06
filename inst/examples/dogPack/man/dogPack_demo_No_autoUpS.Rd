% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dogPackFunc.R
\name{dogPack_demo_No_autoUpS}
\alias{dogPack_demo_No_autoUpS}
\title{Demo No Autoupdate}
\usage{
dogPack_demo_No_autoUpS(tellMe = TRUE, txt = "My favourite color is ")
}
\arguments{
\item{tellMe}{Logical. If the favourite color should be printed.}

\item{txt}{Character}
}
\value{
Does NOT update resp. (re-)sourece the settings file. If 'tellMe' is 
left at the default 'TRUE', the value of the key 'favouriteColor' in the settings 
file is printed. Returns (invisible) NULL.
}
\description{
A function that is \strong{not} auto-updating the values from 
the settings file. Is also reading the value of the key 'favouriteColor' 
from the 'dogPack_settings.R' file.
}
