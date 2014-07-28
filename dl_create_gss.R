########################################################################
# The GSS files used here are from the roper center
# GSS 1972-2012 v 3
# http://0-www.ropercenter.uconn.edu.wncln.wncln.org/CFIDE/cf/action/catalog/abstract.cfm?type=&start=&id=&archno=USNORCGSS1972-2012&abstract=
# The files are downloaded as spss portable file and saved into google docs.
# download link at directlinkupload.com
download_link<- "http://www.directlinkupload.com/uploads/71.81.255.26/gss1972-2012por.zip"
#
#download file logic from
## anthony joseph damico
# ajdamico@gmail.com
#
#######################################################################
#update.packages(checkBuilt=TRUE)
install.packages("downloader", dependencies=TRUE)
install.packages("memisc", dependencies=TRUE)

library("downloader")
library("memisc")


# set your working directory.
# all GSS data files will be stored here
# after downloading and importing.
# use forward slashes instead of back slashes

# uncomment this line by removing the `#` at the front..
# setwd( "C:/My Directory/GSS/" )
# ..in order to set your current working directory



# set the number of digits shown in all output

options( digits = 8 )

###############################################
# DATA LOADING COMPONENT - ONLY RUN THIS ONCE #
###############################################

# create new character variables containing the full filepath of the file on norc's website
# that needs to be downloaded and imported into r for analysis
GSS.2012.CS.file.location <- download_link


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
print( fn[1] )


# these two steps take a while.  but once saved as a .rda, future loading becomes fast forever after #


# convert the stata (.dta) file saved on the local disk (at 'fn') into an r data frame
GSS.2012.CS.df <- as.data.set(spss.portable.file( fn[1] ))


# save the cross-sectional cumulative gss r data frame inside an r data file (.rda)
save( GSS.2012.CS.df , file = "GSS.2012.CS.rda" )

# note that this .rda file will be stored in the local directory specified
# with the setwd command at the beginning of the script




unique(GSS.2012.CS.df$year)
unique(gss2010_12$year)
names(gss2008_12)
gss2008_12<-subset(GSS.2012.CS.df, year %in% c("2008","2010","2012"))
relnames<-grep("reli",names(gss2008_12))
names(gss2008_12[relnames])
names(gss2008_12[grep("premars",names(gss2008_12))])
names(gss2008_12[,4588:4675])

gss2008_12.religionA<-gss2008_12[c("year","region","age","reborn", "attend","attend12","cohort")]
gss2008_12.religionB<-gss2008_12[relnames]
gss2008_12.religionC<-gss2008_12[4588:4675]
gss2008_12.relcombine<-data.frame(gss2008_12.religionA,gss2008_12.religionB, gss2008_12.religionC)
head(gss2008_12.relcombine)
nrow(gss2008_12.relcombine)
save( gss2008_12.relcombine , file = "gss2008_12.rda" )
