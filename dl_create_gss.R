########################################################################
# The GSS files used here are from the roper center
# GSS 1972-2012 v 3
# http://0-www.ropercenter.uconn.edu.wncln.wncln.org/CFIDE/cf/action/catalog/abstract.cfm?type=&start=&id=&archno=USNORCGSS1972-2012&abstract=
# The files are downloaded as spss portable file and saved into google docs.
# download link at directlinkupload.com
download_link<- "http://www.directlinkupload.com/uploads/71.81.255.26/gss1972-2012por.zip"
#dropbox link for entire file 1972-2012 in spss .sav
download_link2<-"https://www.dropbox.com/s/1jmv48wm4kj45ly/gss1972-2012.sav?dl=1"
#
# This process takes a large amount of memory. I failed on a 3.5 and 7 gb virtual machine
# In the end it required a m3:xlarge machine with 15gb ram to run successfully
# The AMI was acquired at Louis aslet site
# http://www.louisaslett.com/RStudio_AMI/
#
#download file logic from
## anthony joseph damico
# ajdamico@gmail.com
#
#######################################################################
#update.packages(checkBuilt=TRUE)
install.packages("downloader", dependencies=TRUE)
install.packages("memisc", dependencies=TRUE)
install.packages("devtools", dependencies=TRUE)
install.packages("Hmisc", dependencies=TRUE)
install.packages("foreign", dependencies=TRUE)
library("downloader")
library("memisc")
library("Hmisc")
library("foreign")
library(devtools)
install_github("ROAuth", "duncantl")
require(devtools)
install_github("rDrop", "karthikram")

# you should run these manually, replacing as appropriate
#options(DropboxKey = "Your_App_key")
#options(DropboxSecret = "Your_App_Secret")


#dropbox app intialization
library(rDrop)
# If you have Dropbox keys in your .rprofile, simply run:
dropbox_credentials <- dropbox_auth()
# Otherwise:
#dropbox_credentials <- dropbox_auth("Your_consumer_key", "Your_consumer_secret")
save(dropbox_credentials, file="my_dropbox_credentials.rdata")

# set your working directory.
# all GSS data files will be stored here
# after downloading and importing.
# use forward slashes instead of back slashes

# uncomment this line by removing the `#` at the front..
# setwd( "C:/My Directory/GSS/" )
# ..in order to set your current working directory

# to download portable spss file set dl_flag="por"
# to download spss sav file set dl_flag="sav"
dl_flag="sav"

# set the number of digits shown in all output

options( digits = 8 )

###############################################
# DATA LOADING COMPONENT - ONLY RUN THIS ONCE #
###############################################


# create a temporary file and a temporary directory
# for downloading file to the local drive
tf <- tempfile() ; td <- tempdir()

# create new character variables containing the full filepath of the file on norc's website
# that needs to be downloaded and imported into r for analysis
if (dl_flag=="por") {
  GSS.2012.CS.file.location <- download_link
} else
  GSS.2012.CS.file.location <- download_link2
  
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
#this does not have to be run for the .sav
if (dl_flag=="por") {
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
print( fn[1] )
} 
  
  #get file from dropbox
#  tf<-dropbox_get(dropbox_credentials, 'gss1972-2012.sav', binary=TRUE)
# save(tf, file="gss1972-2012.sav", ascii=FALSE)
#  fn<-"gss1972-2012.sav"
  





# these two steps take a while.  but once saved as a .rda, future loading becomes fast forever after #


# convert the stata (.dta) file saved on the local disk (at 'fn') into an r data frame
if (dl_flag=="por"){
GSS.2012.CS.df <- as.data.set(spss.portable.file( fn[1] )) 
} else
GSS.2012.CS.df <- spss.get(fn, use.value.labels=TRUE)

# save the cross-sectional cumulative gss r data frame inside an r data file (.rda)
save( GSS.2012.CS.df , file = "GSS.2012.CS.rda" )

# note that this .rda file will be stored in the local directory specified
# with the setwd command at the beginning of the script




unique(GSS.2012.CS.df$year)

#unique(gss2010_12$year)
gss2008_12<-subset(GSS.2012.CS.df, year > 2007)
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
save( gss2008_12.relcombine , file = "gss2008_12.rda" )
