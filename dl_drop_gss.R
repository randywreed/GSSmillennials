#dl_rdrop_gss - try to download spss file from dropbox
# The GSS files used here are from the roper center
# GSS 1972-2012 v 3
# http://0-www.ropercenter.uconn.edu.wncln.wncln.org/CFIDE/cf/action/catalog/abstract.cfm?type=&start=&id=&archno=USNORCGSS1972-2012&abstract=
# The files are downloaded as spss portable file and saved into dropbox.
#
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
#Rdrop 
###################################################################################################33333333
install.packages("devtools", dependencies=TRUE)
library(devtools)
install_github("ROAuth", "duncantl")
require(devtools)
install_github("rDrop", "karthikram")
install.packages("memisc")
library(memisc)
# you should run these manually, replacing as appropriate
options(DropboxKey = "v0supc54ahomzxx")
options(DropboxSecret = "5nfhtz6f1l0nxsy")
install.packages("foreign")
library(foreign)
install.packages("Hmisc")
library(Hmisc)

#dropbox app intialization
library(rDrop)
# If you have Dropbox keys in your .rprofile, simply run:
dropbox_credentials <- dropbox_auth()
# Otherwise:
#dropbox_credentials <- dropbox_auth("Your_consumer_key", "Your_consumer_secret")
save(dropbox_credentials, file="my_dropbox_credentials.rdata")
# create a temporary file and a temporary directory
# for downloading file to the local drive
tf <- tempfile() ; td <- tempdir()

#get file from dropbox
tf<-(dropbox_get(dropbox_credentials, 'gss1972-2012.sav', binary=TRUE))
writeBin(as.raw(tf), "gss1972-2012.sav")
#save(tf, file="gss1972-2012.sav", ascii=FALSE)
fn<-"gss1972-2012.sav"
#GSS.2012.CS.df <- as.data.frame(as.data.set(spss.system.file(fn)))
GSS.2012.CS.df <- spss.get(fn, use.value.labels=TRUE, to.data.frame=TRUE)
#save(GSS.2012.CS.df,file="GSS_All.rda")
dropbox_save(dropbox_credentials, GSS.2012.CS.df, file="GSS_all.rdata")
unique(GSS.2012.CS.df$year)

#unique(gss2010_12$year)
gss2008_12<-subset(GSS.2012.CS.df, year > 2007)
#gss2008_12<-GSS.2012.CS.df
#save(gss2008_12,file="GSS_2008_12_All_Fields.rda")
dropbox_save(dropbox_credentials, gss2008_12, file="GSS_2008_12_All_Fields.rdata")
rm(GSS.2012.CS.df)
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

#Upload to Dropbox
rm(gss2008_12)
dropbox_save(dropbox_credentials, gss2008_12.relcombine, file="gss2008_12.rdata")