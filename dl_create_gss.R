# Download the files manually as spss from gss.norc.org/get-the-data
# ########################################################################
# # The GSS files used here are from the roper center
# # GSS 1972-2012 v 3
# # http://0-www.ropercenter.uconn.edu.wncln.wncln.org/CFIDE/cf/action/catalog/abstract.cfm?type=&start=&id=&archno=USNORCGSS1972-2012&abstract=
# # The files are downloaded as spss portable file and saved into google docs.
# # download link at directlinkupload.com
# download_link<- "http://www.directlinkupload.com/uploads/71.81.255.26/gss1972-2012por.zip"
# #dropbox link for entire file 1972-2012 in spss .sav
# download_link2<-"https://www.dropbox.com/s/1jmv48wm4kj45ly/gss1972-2012.sav?dl=1"
# #
# # This process takes a large amount of memory. I failed on a 3.5 and 7 gb virtual machine
# # In the end it required a m3:xlarge machine with 15gb ram to run successfully
# # The AMI was acquired at Louis aslet site
# # http://www.louisaslett.com/RStudio_AMI/
# #
# #download file logic from
# ## anthony joseph damico
# # ajdamico@gmail.com
# #
#######################################################################
#update.packages(checkBuilt=TRUE)
#install.packages("downloader", dependencies=TRUE)
install.packages("memisc", dependencies=TRUE)
install.packages("devtools", dependencies=TRUE)
install.packages("Hmisc", dependencies=TRUE)
install.packages("foreign", dependencies=TRUE)
install.packages("RCurl")
install.packages("httr")

library("downloader")
library("memisc")
library("Hmisc")
library("foreign")
library(devtools)
library("RCurl")
library("httr")
install_github("duncantl/ROAuth")
require(devtools)
install.packages("rdrop2")
install.packages("httpuv")
library("httpuv")
# you should run these manually, replacing as appropriate
#options(DropboxKey = "Your_App_key")
#options(DropboxSecret = "Your_App_Secret")


# #dropbox app intialization
 library(rdrop2)
#can't generate the token from rstudio server. In rstudio standalone do the following
# install.packages("rdrop2")
#token<- drop_auth()
#saveRDS(token, "droptoken.rds")
# then upload droptoken.rds to current directory
token<-readRDS("droptoken.rds")
drop_get("ICPSR_36319.zip", dtoken = token)
unzip("ICPSR_36319.zip")
load("./ICPSR_36319/DS0001/36319-0001-Data.rda")
#rename the rda file df to GSS.current.df
GSS.current.df <- da36319.0001
# 
#
###############################################################
# Isolate Religious Fields
# This section of code helps you determine what fields are related to religion
# The best approach is to search the code book manually and then use a variant of:
# which(names(gss2008_cur)=="PREMARS1") to get the index number
# misc_rel holds all the groups of religion in 1972-2014
# It should be redone with each issuance. The numbers will Not stay the same
#
unique(GSS.current.df$YEAR) # check to make sure what years we have
# isolate years from 2008 on
gss2008_cur<-subset(GSS.current.df, YEAR > 2007)

names(gss2008_cur)
relnames<-grep("RELI",names(gss2008_cur))
names(gss2008_cur[relnames])
names(gss2008_cur[grep("PREMARS",names(gss2008_cur))])
which(names(gss2008_cur)=="PREMARS1")
misc_rel<-c(319:413, 1059:1063, 2100:2123, 3071:3106, 3323, 3834:3852, 4640:4726)
names(gss2008_cur[misc_rel])

gss2008_cur.religionA<-gss2008_cur[toupper(c("year","region","age","reborn", "attend","attend12","cohort"))]
gss2008_cur.religionB<-gss2008_cur[relnames]
gss2008_cur.religionC<-gss2008_cur[misc_rel]
gss2008_cur.relcombine<-data.frame(gss2008_cur.religionA,gss2008_cur.religionB, gss2008_cur.religionC)
#convert all names to uppercase
names(gss2008_cur.relcombine)<-toupper(names(gss2008_cur.relcombine))
head(gss2008_cur.relcombine)
nrow(gss2008_cur.relcombine)
save( gss2008_cur.relcombine , file = "gss2008_cur.rda" )

