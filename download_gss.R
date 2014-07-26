#################################################################################################################
# Analyze the 1972-2012 General Social Survey cross-sectional cumulative data (release 2, feb. 2012) file with R #
##################################################################################################################
#This script probably won't run in R-Studio, the data it downloads is >600mb. 


# set your working directory.
# all GSS data files will be stored here
# after downloading and importing.
# use forward slashes instead of back slashes

# uncomment tis line by removing the `#` at the front..
# setwd( "C:/My Directory/GSS/" )
# ..in order to set your current working directory



# set the number of digits shown in all output

options( digits = 8 )


# remove the # in order to run this install.packages line only once
# install.packages( "survey" )


library(foreign) # load foreign package (converts data files into R)
library(survey) # load survey package (analyzes complex design surveys)


# by default, R will crash if a primary sampling unit (psu) has a single observation
# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
# by uncommenting this line:
# options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN



###############################################
# DATA LOADING COMPONENT - ONLY RUN THIS ONCE #
###############################################

# create new character variables containing the full filepath of the file on norc's website
# that needs to be downloaded and imported into r for analysis
GSS.2012.CS.file.location <-
  "http://publicdata.norc.org/GSS/DOCUMENTS/OTHR/GSS_stata.zip"


# create a temporary file and a temporary directory
# for downloading file to the local drive
tf <- tempfile() ; td <- tempdir()


# download the file using the filepath specified
download.file(
  # download the file stored in the location designated above
  GSS.2012.CS.file.location ,
  # save the file as the temporary file assigned above
  tf ,
  # download this as a binary file type
  mode = "wb"
)


# the variable 'tf' now contains the full file path on the local computer to the specified file

# store the file path on the local disk to the extracted file (previously inside the zipped file)
# inside a new character string object 'fn'
fn <-
  unzip(
    # unzip the contents of the temporary file
    tf ,
    # ..into the the temporary directory (also assigned above)
    exdir = td ,
    # overwrite the contents of the temporary directory
    # in case there's anything already in there
    overwrite = T
  )

# print the temporary location of the stata (.dta) file to the screen
print( fn )


# these two steps take a while. but once saved as a .rda, future loading becomes fast forever after #


# convert the stata (.dta) file saved on the local disk (at 'fn') into an r data frame
GSS.2012.CS.df <- read.dta( fn )


# save the cross-sectional cumulative gss r data frame inside an r data file (.rda)
save( GSS.2012.CS.df , file = "GSS.2012.CS.rda" )

# note that this .rda file will be stored in the local directory specified
# with the setwd command at the beginning of the script

##########################################################################
# END OF DATA LOADING COMPONENT - DO NOT RUN DATA LOADING COMMANDS AGAIN #
##########################################################################
gss1990<-GSS.2012.CS.df
rm(GSS.2012.CS.df)

