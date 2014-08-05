#dl_rdrop_gss - try to download spss file from dropbox
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
GSS.2012.CS.df <- as.data.frame(as.data.set(spss.system.file(fn)))