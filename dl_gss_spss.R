# analyze survey data for free (http://asdfree.com) with the r language
# general social survey
# 1972-2012 cross-sectional cumulative data (release 1, march 2013)
# 
#
# if you have never used the r language before,
# watch this two minute video i made outlining
# how to run this script from start to finish
# http://www.screenr.com/Zpd8

# anthony joseph damico
# ajdamico@gmail.com

# if you use this script for a project, please send me a note
# it's always nice to hear about how people are using this stuff

# for further reading on cross-package comparisons, see:
# http://journal.r-project.org/archive/2009-2/RJournal_2009-2_Damico.pdf


##################################################################################################################
# Analyze the 1972-2012 General Social Survey cross-sectional cumulative data (release 6, July 2014) file with R #
##################################################################################################################
# this is memory intensive so it clears the workspace first save anything you want to keep
rm(list=ls())

# set your working directory.
# all GSS data files will be stored here
# after downloading and importing.
# use forward slashes instead of back slashes

# uncomment this line by removing the `#` at the front..
# setwd( "C:/My Directory/GSS/" )
# ..in order to set your current working directory



# set the number of digits shown in all output

options( digits = 8 )


# remove the # in order to run this install.packages line only once
install.packages("downloader")
install.packages("foreign")
#install.packages( "survey" )

library(downloader)
library(foreign) # load foreign package (converts data files into R)
#library(survey)  # load survey package (analyzes complex design surveys)


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
  "http://publicdata.norc.org/GSS/DOCUMENTS/OTHR/GSS_spss.zip"


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

# print the temporary location of the spss (.sav) file to the screen
print( fn[ grep( "sav$" , fn ) ] )


# these two steps take a while.  but once saved as a .rda, future loading becomes fast forever after #


# convert the spss (.sav) file saved on the local disk (at 'fn') into an r data frame
GSS.2012.CS.df <- 
  read.spss( 
    fn[ grep( "sav$" , fn ) ] , 
    to.data.frame = TRUE , 
    use.value.labels = FALSE 
  )

# copy to a different object
z <- GSS.2012.CS.df

# remove the original from RAM
rm( GSS.2012.CS.df )

# clear up memory
gc()

# repeat
GSS.2012.CS.df <- z

# repeat
rm( z )

# i have no idea why this works.
gc()
# but if you don't do this on a 3gb ram machine
# you will run out of memory.  go figure.



# save the cross-sectional cumulative gss r data frame inside an r data file (.rda)
save( GSS.2012.CS.df , file = "GSS.2012.CS.rda" )

# note that this .rda file will be stored in the local directory specified
# with the setwd command at the beginning of the script

##########################################################################
# END OF DATA LOADING COMPONENT - DO NOT RUN DATA LOADING COMMANDS AGAIN #
##########################################################################

#Create Working Sets
#complete set is GSS.2012.CS.rda, created above
#second set has 2008-2012 and includes all variables is gss2008_12_all.rda
#third set has just religion and demo variables is gss2008_12.rda
unique(GSS.2012.CS.df$year)

#unique(gss2010_12$year)
gss2008_12<-subset(GSS.2012.CS.df, year > 2007)
save(gss2008_12, file="gss2008_12_all.rda")
rm(GSS.2012.CS.df) #clean up memory, this is huge and we won't need it again
#gss2008_12<-GSS.2012.CS.df
names(gss2008_12)
relnames<-grep("reli",names(gss2008_12))
names(gss2008_12[relnames])
names(gss2008_12[grep("premars",names(gss2008_12))])
names(gss2008_12[,4588:4675])

gss2008_12.religionA<-gss2008_12[c("year","region","age","reborn", "attend","attend12","cohort")]
gss2008_12.religionB<-gss2008_12[relnames]
gss2008_12.religionC<-gss2008_12[4588:4675]
gss2008_12.relcombine<-data.frame(gss2008_12.religionA,gss2008_12.religionB, gss2008_12.religionC)
#convert all names to uppercase
names(gss2008_12.relcombine)<-toupper(names(gss2008_12.relcombine))
head(gss2008_12.relcombine)
nrow(gss2008_12.relcombine)
rm(gss2008_12) # clean up memory before last save
save( gss2008_12.relcombine , file = "gss2008_12.rda" )