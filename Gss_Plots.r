#if r has had a verison change run this
#update.packages(ask=FALSE, dependencies=c('Suggests'))

#packages (Install once)
install.packages("lattice")
install.packages("cluster")
install.packages("Hmisc")
install.packages("tables") #tabular package
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("Rcpp")
install.packages("dplyr")
install.packages("foreign")
install.packages("reshape2")
install.packages("scales")
install.packages("vcd")
install.packages("stringr")
install.packages("tidyr")
install.packages("plyr")

#install dplyr and rcpp from github
install.packages("devtools")
install.packages("hflights") #required for dplyr
install.packages("DBI")
install.packages("RSQLite")
install.packages("RSQLite.extfuns")
install.packages("nlme")
install.packages("Matrix")
install.packages("mgcv")
install.packages("Lahman")
install.packages("knitr")
install.packages("tidyr")
install.packages("circlize")
#these may not be necessary and errored anyway
#devtools::install_github( "romainfrancois/Rcpp11" )
#require( Rcpp11 )
#evalCpp( "2LL" )

devtools::install_github("hadley/dplyr")
devtools::install_github("randywreed/gssReligion", auth_token = Sys.getenv("GITHUB_PAT"))

## @knitr setup
library("Hmisc")
library("ggplot2")
library("tables")
library("gridExtra")
library("dplyr")
library("foreign")
library("reshape2")
library("scales")
library("vcd")
library("gssReligion")
library("stringr")
library("knitr")
library("tidyr")
library("plyr")
library("circlize")
#copy gss 2008-2012 data
#run dl_create_gss.R first to create data file (NOTE: heavy system requirements, please read the comments in file)
# dl_create_gss.R creates gss2008_cur.rda
# gss2008_cur.rda is loaded into gss2012
load("gss2008_cur.rda")
gss2012<-gss2008_cur.relcombine
#upcase names
names(gss2012)<-toupper(names(gss2012))
if (exists("gss2008_cur.relcombine")) rm("gss2008_cur.relcombine")
#Create religid_region
Nreligid_region<-gss2012[,c("YEAR","RELIG","REGION")]
#Nreligid_region$RELIG<- factor(gss2012$RELIG, labels=c("Protestant","Catholic","Jewish","None","Other (specify)","Buddhism","Hinduism","Other Eastern Religion","Muslim/Islam","Orthodox Christian","Christian","Native American","Inter-/non-denominational"))
lookupRegion<-data.frame(regionid=c('2','3','4','5','6','7','8','9','10'), region=c('North',"North","Midwest","Midwest","South","South","South","West","West"))
Nreligid_region$REGIONID<-as.numeric(Nreligid_region$REGION)
Nreligid_region$NEWREGIONID <- lookupRegion$region[match(Nreligid_region$REGIONID, lookupRegion$regionid)]
#Add data of everyone born again
Nreligid_region$AGE<-gss2012$AGE
Nreligid_region$REBORN<-gss2012$REBORN
#millennials<-c("18","19","20","21","22","23","24","25","26","27","28","29")
Nreligid_region$COHORT<-gss2012$COHORT
#Nreligid_region<-within(Nreligid_region, MILLENNIALS <- {ifelse(Nreligid_region$AGE %in% millennials, TRUE, FALSE)})
Nreligid_region<-within(Nreligid_region, MILLENNIALS <- {ifelse(as.numeric(as.character(Nreligid_region$COHORT)) >= 1980, TRUE, FALSE)})
Nreligid_region$MILLENNIALS<-as.factor(Nreligid_region$MILLENNIALS)
levels(Nreligid_region$MILLENNIALS)<-c("Non-Millennials","Millennials")
Nreligid_region$RELIG16<-gss2012$RELIG16
Nreligid_region$ATTEND<-gss2012$ATTEND
#levels(Nreligid_region$RELIG)<-c("iap","Protestant","Catholic","Jewish","None","Other","Buddhism","Hinduism","Other Eastern","Muslim/Islam","Orthodox-Christian","Christian","Native American","Inter-/Nondenomenational","Don't Know","No Answer")
#levels(Nreligid_region$RELIG16)<-c("iap","Protestant","Catholic","Jewish","None","Other","Buddhism","Hinduism","Other Eastern","Muslim/Islam","Orthodox-Christian","Christian","Native American","Inter-/Nondenomenational","Don't Know","No Answer")
#Nreligid_region$RELIG<-factor(Nreligid_region$RELIG,levels(Nreligid_region$RELIG)[c(2:14,1,15:16)])
#Nreligid_region$RELIG16<-factor(Nreligid_region$RELIG16,levels(Nreligid_region$RELIG16)[c(2:14,1,15:16)])
# add variable to scale ATTEND 
attendlookup<-data.frame(attendid=c(1:10), attendnum=c(0,.5,1,6,12,30,46,52,100,-1))
Nreligid_region$NEWATTENDID<-as.numeric(Nreligid_region$ATTEND)
Nreligid_region$NEWATTEND<- attendlookup$attendnum[match(Nreligid_region$NEWATTENDID,attendlookup$attendid)]
Nreligid_region$NEWATTEND<-factor(Nreligid_region$NEWATTEND,labels=c("Never","LT Yearly","Yearly","Several Times a Year","Monthly","2-3x a Month","Nearly Weekly","Weekly","More Than Weekly"))
#Create Subsets
#Millennials only Subset
gss_millennials<-subset(Nreligid_region, Nreligid_region$MILLENNIALS=="Millennials")
#Southern Millennials only
gss_millenials_so<-subset(gss_millennials, gss_millennials$NEWREGIONID=="South")

#percentage of millennials who are born again
gss_millennials$REBORN<-factor(gss_millennials$REBORN)
gss_millennials<-na.omit(gss_millennials)
gss_millennials$REBORN<-factor(gss_millennials$REBORN)
born_again_and_millennials<-melt(round(prop.table(table(gss_millennials$REBORN, gss_millennials$NEWREGIONID),2)*100,3))

#Create Tables for Graph - Millennials religious identificaiton by region
millennials_table_by_revised_region <- round((prop.table(table(gss_millennials$RELIG, gss_millennials$NEWREGIONID),2)*100),3)
millennials_melt<-melt(millennials_table_by_revised_region)
names(millennials_melt)<-c("ReligiousID", "Region", "Percentage")
# All Religious Identification by region
percentage_table_by_revised_region <- round((prop.table(table(Nreligid_region$RELIG, Nreligid_region$NEWREGIONID),2 )*100), 3)
percentage_table_melt<-melt(percentage_table_by_revised_region)
names(percentage_table_melt)<-c("ReligiousID","Region","Percentage")
#comparison table creation
nones_compare<-cbind(millennials_table_by_revised_region[4,1], percentage_table_by_revised_region[4,1],millennials_table_by_revised_region[4,2], percentage_table_by_revised_region[4,2],millennials_table_by_revised_region[4,3], percentage_table_by_revised_region[4,3],millennials_table_by_revised_region[4,4], percentage_table_by_revised_region[4,4])
colnames(nones_compare)<-c("Millennials Midwest","All Midwest", "Millennials North","All North","Millennials South","All South","Millennials West","All West" )
#create a table ggplot of millennials nones v. all nones
millennials_summary_by_region<-subset(melt(millennials_table_by_revised_region), Var1=="(04) NONE" & Var2 !="Not Assigned")
millennials_summary_by_region$MILLENNIALS <- factor(TRUE)
nonmillennials_summary_by_region<-subset(melt(percentage_table_by_revised_region), Var1=="(04) NONE" & Var2 !="Not Assigned")
nonmillennials_summary_by_region$MILLENNIALS <- factor(FALSE)
all_summary_by_region<-merge(millennials_summary_by_region, nonmillennials_summary_by_region, all=TRUE)
levels(all_summary_by_region$MILLENNIALS)<-c("Millennials","Non-Millennials")




## @knitr EvangelicalPresence
#Evangelical presence in the country
Nreligid_region %>% na.omit() %>% filter(YEAR==2014)  %>%
ggplot(aes(x=REBORN, fill=REBORN))+
  xlab("Born Again Experience?")+
  ylab("Percentage")+ggtitle("National percentage of individuals who have had a 'Born Again' Experience")+
  geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge")+
  theme(legend.position="none")+
  scale_y_continuous(labels=percent)+
  annotate("text", label="General Social Survey 2014", x=1.5, y=.01, size=3, color="black")

## @knitr EvangelicalStacked
Nreligid_region$RELIG<-factor(Nreligid_region$RELIG, levels=rev(levels(Nreligid_region$RELIG)))
Nreligid_region %>% na.omit() %>% filter(YEAR==2014)  %>%
  ggplot(aes(x=REBORN, fill=RELIG))+
  xlab("Born Again Experience?")+
  ylab("Percentage")+ggtitle("National percentage of individuals who have had a 'Born Again' Experience")+
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  scale_fill_grey(start = 0.8, end= 0.2)+ theme_classic()+
  #theme(legend.position="none")+
  scale_y_continuous(labels=percent)+
  annotate("text", label="General Social Survey 2014", x=1.5, y=.01, size=3, color="black")

  
## @knitr EvangelicalByRegion
#Evangelical presence in the country by region
ggplot(na.omit(Nreligid_region), aes(x=REBORN, fill=NEWREGIONID))+
  xlab("Born Again?")+
  ylab("Percentage")+ggtitle("Percentage of Evangelicals in Each Region")+
  geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge")+
    scale_fill_grey(name="Been Born Again?", start=0, end=.5)+
    scale_y_continuous(labels=percent)+
    caption="General Social Survey 2008-2014"

## @knitr EvangelicalByRegionCompareYN
#Regional Comparison of Born Again
ggplot(na.omit(Nreligid_region, REBORN), 
       aes(x=NEWREGIONID, fill=REBORN))+
  xlab("Born Again?")+
  ylab("Percentage")+ggtitle("Percentage of Evangelicals in Each Region")+
  geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge")+
  scale_fill_grey(name="Been Born Again?", start=0, end=0.5)+
  scale_y_continuous(labels=percent)+
  labs(caption="General Social Survey 2008-2014")

    

## @knitr MillennialBornAgainByRegion
ggplot(born_again_and_millennials, aes(x=born_again_and_millennials$Var1, y=born_again_and_millennials$value, fill=born_again_and_millennials$Var2))+
  geom_bar(stat="identity", position="dodge")+
  ylab("Percentage")+xlab("Born Again?")+
  ggtitle("Millennial Identification as 'Born Again'")+
  scale_fill_discrete(name="Region")+
  labs(caption="General Social Survey 2008-12")
  #annotate("text",label="General Social Survey 2008-2012", x=1.5, y=.01, size=3, color="black")

#table(gss_millennials$REBORN, gss_millennials$RELIG)
#round((prop.table(table(gss_millennials$REBORN, gss_millennials$RELIG),2)*100),3)
#round((prop.table(table(Nreligid_region$RELIG, Nreligid_region$REBORN, Nreligid_region$MILLENNIALS),1)*100),3)

## @knitr MillennialvNonMillennialBornAgainSetup
#Millennials v. Non-Millennials self-identified Born Again

# round((prop.table(table(Nreligid_region$REBORN, Nreligid_region$MILLENNIALS),2)*100),3)
# #round((prop.table(table(Nreligid_region$REBORN, Nreligid_region$MILLENNIALS),2)*100),3)
# BAtab<-summary(MILLENNIALS ~ REBORN, method="reverse", data=Nreligid_region)
# #BAtab
# #plot(BAtab)


#Protestants born again, millennial v non-millennial
Protestant_Only<-subset(Nreligid_region, Nreligid_region$RELIG=="PROTESTANT")
round((prop.table(table(Protestant_Only$REBORN, Protestant_Only$MILLENNIALS),2)*100),3)

## @knitr nonMillennialsBornAgainSetup
#non-millennials identification as born again
non_millennials<-subset(Nreligid_region, Nreligid_region$MILLENNIALS=="Non-Millennials")
non_millennials$REBORN<-factor(non_millennials$REBORN)
#levels(non_millennials$REBORN)<-c("Yes","No")
#round((prop.table(table(non_millennials$REBORN, non_millennials$RELIG),2)*100),3)
table3<-tabular(Heading("Non-Millennials Identification as 'Born Again'")*REBORN*Percent("col") ~ (Religion=RELIG),data=non_millennials) 
#table3
#table3[1:2,c(1:2,4)]

## @knitr nonmillennialIdentificationByRegion
ggplot(subset(non_millennials, RELIG %in% c("PROTESTANT","CATHOLIC","NONE")), aes(x=REBORN, fill=REBORN))+
  geom_bar(stat="count", position="dodge")+
  facet_wrap(~RELIG, ncol=3)+
  ggtitle("Non-millennials Identification as 'Born Again'")+
  annotate("text",label="General Social Survey 2008-2012", x=1.5, y=.01, size=3, color="black")

## @knitr PercentageOfNonMillennialEVsbyRegion
#non-millennials identification as born again by region
ggplot(na.omit(subset(non_millennials, REBORN %in% c("YES","NO"))), aes(x=NEWREGIONID, fill=REBORN))+
  xlab("Born Again?")+
  ylab("Percentage")+
  ggtitle("Percentage of Non-millennial Evangelicals in Each Region")+
  geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge")+
  scale_fill_discrete(name="Been Born Again?")+
  scale_y_continuous(labels=percent)+
  annotate("text",label="General Social Survey 2008-2012", x=1.5, y=.01, size=3, color="black")

## @knitr CompareBornAgainMillennialsAndNonByRegion
#compare born again identification between millennials and non-millennials by region
Nreligid_region$REBORNFAC<-factor(Nreligid_region$REBORN)
#levels(Nreligid_region$REBORNFAC)<- c("Yes","No")
#table(Born_again_Subset$NEWREGIONID,Born_again_Subset$MILLENNIALS)
#table(Nreligid_region$NEWREGIONID,Nreligid_region$REBORNFAC,Nreligid_region$MILLENNIALS)
#tabular(NEWREGIONID*MILLENNIALS~((REBORNFAC=="YES")+(REBORNFAC=="NO")), data=Nreligid_region)
BornAgainTable<-round((prop.table(table(Nreligid_region$NEWREGIONID,Nreligid_region$REBORNFAC,Nreligid_region$MILLENNIALS),c(3,1))*100),3)
#ftable(BornAgainTable)
#plot(BornAgainTable)
BornAgainDF<-as.data.frame(BornAgainTable)
BornAgainDF<-rename(BornAgainDF, c("Var1"="NEWREGIONID","Var2"="REBORN","Var3"="MILLENNIALS"))

#percentage of all born agains (REBORN=YES) in Millennial and non-millennial
# (just proves that there are a lot more non-millennials - Doesn't actually tell us anything)
# The table should tell us the percent of all millennials who are born again v. percent of all 
# non-millennials who are born again.
# round((prop.table(table(Born_again_Subset$NEWREGIONID, Born_again_Subset$MILLENNIALS),1)*100),3)
#Nreligid_region_plot<-na.omit(Nreligid_region)
# ggplot(Nreligid_region_plot, aes(x=MILLENNIALS, fill=REBORNFAC))+
#   geom_bar(aes(y=(..count..)/sum(..count..)),position="dodge")+
#   facet_wrap(~NEWREGIONID, ncol=2)+
#   ylab("Percentage")+
#   scale_fill_discrete(name="Born Again?")+
#   scale_y_continuous(labels = percent_format())

## @knitr nonMillennialsvMillennialsBornAgain
ggplot(BornAgainDF, aes(x=MILLENNIALS, y=Freq, fill=REBORN ))+
  geom_bar( stat="identity", position="dodge")+
  facet_wrap(~NEWREGIONID, ncol=2)+
  ylab("Percentage")+xlab("Non-Millennials v. Millennials")+
  ggtitle("Born Again Non-Millennials v. Born Again Millennials")+
  scale_fill_grey(name="Born Again?", start=0, end=.5)+
  labs(caption="General Social Survey 2008-2014")


## @knitr TimeSeries
#Time Series - 4yr chart (2008-12)
time_melt<-melt(prop.table(table(as.factor(Nreligid_region$YEAR),Nreligid_region$REBORNFAC)), id.vars=YEAR)
ggplot(time_melt, aes(x=Var1, y=value, color=Var2))+geom_line()+xlab("Year")+ylab("Percentage")+
  scale_y_continuous( labels=percent_format())+
  scale_color_grey(name="Born Again", start=0, end=.5)+
  ggtitle("Percentage of All Born Again Over Time (2008-14)")+
  #scale_fill_grey(name="Born Again?", start=0, end=.5)+
  labs(caption="General Social Survey 2008-2014")

#Time Series - 12 yr chart (2000-12)
#load 2000-2012 religion data, this requires gss database of 2000-2012
# Create data if necessary - This should be run only on large ram comp.
# religionModify assumes lower case names, so change that first
gss2000_cur<-subset(GSS.current.df, YEAR>1999)
names(gss2000_cur)<-tolower(names(gss2000_cur))
gss2000_cur<-religionModify(subset(gss2000_cur, year>1999))
names(gss2000_cur)<-toupper(names(gss2000_cur))
#gss2000_cur<-subset(gss2000_cur,REBORN %in% c("YES","NO"))
#rm(gss2000_2012)
gss2000_cur<-within(gss2000_cur, MILLENNIALS <- {ifelse(as.numeric(as.character(gss2000_cur$COHORT)) >= 1980, TRUE, FALSE)})
gss2000_cur$MILLENNIALS<-as.factor(gss2000_cur$MILLENNIALS)
levels(gss2000_cur$MILLENNIALS)<-c("Non-Millennials","Millennials")
lookupRegion<-data.frame(regionid=c('2','3','4','5','6','7','8','9','10'), region=c('North',"North","Midwest","Midwest","South","South","South","West","West"))
gss2000_cur$REGIONID<-as.numeric(gss2000_cur$REGION)
gss2000_cur$NEWREGIONID <- lookupRegion$region[match(gss2000_cur$REGIONID, lookupRegion$regionid)]
gss2000_cur$REBORN<-factor(gss2000_cur$REBORN)
time_melt<-melt(prop.table(table(as.factor(gss2000_cur$YEAR), gss2000_cur$REBORN)), id.vars=YEAR)
ggplot(time_melt, aes(x=Var1, y=value, color=Var2))+geom_line()+xlab("Year")+ylab("Percentage")+
  scale_y_continuous( labels=percent)+
  scale_color_grey(name="Born Again", start=0, end=.5)+
  ggtitle("Percentage of All Born Again Over Time (2000-2014")+
  #scale_fill_grey(name="Born Again?", start=0, end=.5)+
  labs(caption="General Social Survey 2008-2014")

millennials_subset<-subset(Nreligid_region, Nreligid_region$MILLENNIALS=="Millennials")
Reborn_prop<-table(as.factor(millennials_subset$YEAR), 
                 millennials_subset$REBORNFAC, 
                 millennials_subset$NEWREGIONID)
#prop.table(Reborn_prop,1)*100
Mills_reborn_prop<-prop.table(ftable(
  xtabs(~millennials_subset$NEWREGIONID+millennials_subset$YEAR+
          millennials_subset$REBORNFAC)),1)*100
#Mills_reborn_prop
time_melt<-melt(Mills_reborn_prop, id.vars=YEAR)
ggplot(time_melt, aes(x=value.millennials_subset.YEAR, y=value.Freq, group=value.millennials_subset.REBORNFAC, color=value.millennials_subset.REBORNFAC))+
  geom_line()+
  facet_wrap(~value.millennials_subset.NEWREGIONID, ncol=2)+
  ylab("Percentage")+xlab("Year")+scale_color_discrete(name="Born Again?")+
  ggtitle("Millennial Born Again Percentage Since 2008")

#regional time series 2000-2012
millennials_subset<-subset(gss2000_cur, MILLENNIALS=="Millennials")
Reborn_prop<-table(as.factor(millennials_subset$YEAR), 
                   millennials_subset$REBORN, 
                   millennials_subset$NEWREGIONID)
#prop.table(Reborn_prop,1)*100
Mills_reborn_prop<-prop.table(ftable(
  xtabs(~millennials_subset$NEWREGIONID+millennials_subset$YEAR+
          millennials_subset$REBORN)),1)*100
#Mills_reborn_prop
time_melt<-melt(Mills_reborn_prop, id.vars=YEAR)
ggplot(time_melt, aes(x=value.millennials_subset.YEAR, y=value.Freq, group=value.millennials_subset.REBORN, color=value.millennials_subset.REBORN))+
  geom_line()+
  facet_wrap(~value.millennials_subset.NEWREGIONID, ncol=2)+
  ylab("Percentage")+xlab("Year")+scale_color_discrete(name="Born Again?")+
  ggtitle("Millennial Born Again Percentage Since 2004")

## @knitr MillennialvNonMillennialsSouth
#Born Again Millennials v. Non-Millenials focus on the south

BornAgainSouthDF<-subset(BornAgainDF, BornAgainDF$NEWREGIONID=="South")
ggplot(BornAgainSouthDF, aes(x=MILLENNIALS,y=Freq, fill=REBORN))+
  geom_bar(stat="identity",position="dodge")+
  #facet_wrap(~NEWREGIONID, ncol=1)+
  ylab("Percentage")+xlab("")+
 # scale_y_continuous(labels = percent_format())+
  scale_fill_grey(name="Born Again?", start=0, end=0.5)+
  ggtitle("Millennials v. Non-Millennial Born Again in the South")+
  labs(caption="General Social Survey 2008-2014")

#p = ggplot(mydataf, aes(x = foo)) + 
#  geom_bar(aes(y = (..count..)/sum(..count..))) + 
#  scale_y_continuous(formatter = 'percent')



## @knitr plotReligiousIDbyRegion
ggplot(subset(millennials_melt, Percentage>10), aes(x=Region, y=Percentage, fill=Region))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~ReligiousID, ncol=1)+
  ggtitle("Millennials Religious Identification By Region")
#all religions same plot

## @knitr plotMillennialsIDbyReligiousID
ggplot(subset(millennials_melt, Percentage>10), aes(x=Region, y=Percentage, fill=ReligiousID))+
  geom_bar(stat="identity", position="dodge")+
 # facet_wrap(~ReligiousID, ncol=1)+
  ggtitle("Millennials Religious Identification By Region")


## @knitr AllReligiousIDbyRegionFacet
#different plot for each religion
ggplot(subset(percentage_table_melt, Percentage>10), aes(x=Region, y=Percentage, fill=Region))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~ReligiousID, ncol=1)+
  ggtitle("U.S. Religious Identification by Region")

## @knitr AllReligiousIDbyRegionPlot
#all religions same plot
ggplot(subset(percentage_table_melt, Percentage>10), aes(x=Region, y=Percentage, fill=ReligiousID))+
  geom_bar(stat="identity", position="dodge")+
#  facet_wrap(~ReligiousID, ncol=1)+
  scale_fill_grey(start=0, end=0.5)+
  ggtitle("U.S. Religious Identification by Region")+
  labs(caption="General Social Survey 2008-2014")

## @knitr comparisontable
ggplot(all_summary_by_region, aes(x=Var2, y=value, fill=MILLENNIALS)) + 
  geom_bar(stat="identity", position="dodge")+ylab("Perecentage")+xlab("Region")+
  ggtitle("Millennial v. Non-Millennial Nones by Region")+
  scale_fill_grey(name="Religiously Unaffiliated (Nones)", 
                      labels=c("Millennial Nones","Non-Millennial Nones"), start = 0, end=0.5)+
  labs(caption="General Social Survey 2008-2014")

## @knitr BornAgainNones
# percentage of nones who are born again
#round((prop.table(table(Nreligid_region$RELIG, Nreligid_region$NEWREGIONID, Nreligid_region$REBORN, exclude=c("iap","dk", "na")),2)*100),3)
Born_again_Subset<-subset(Nreligid_region, toupper(Nreligid_region$REBORN)=="YES")
Born_again_table<-round((prop.table(table(Born_again_Subset$RELIG, Born_again_Subset$NEWREGIONID),2)*100),3)
Born_again_subset_wide<-melt(Born_again_table)
Born_again_subset_wide_select<-subset(Born_again_subset_wide, Born_again_subset_wide$Var.1 %in% toupper(c("Protestant","None","Catholic")))
ggplot(Born_again_subset_wide_select, aes(x=Var.2, y=value, fill=Var.1)) + geom_bar(stat="identity", position="dodge")+ylab("Perecentage")+xlab("Region")+ggtitle("Religious Identification of 'Born Again' by Region")

## knitr proportionOfMillennialReligions
#proportion of millenial protestant/none/catholics using plyr
# require(plyr)
# results <- ddply(.data = Nreligid_region, .var = c("NEWREGIONID", "MILLENNIALS"), .fun = function(x) {
#   data.frame(n = nrow(x),
#              protestant.n = nrow(subset(x, RELIG == "(01) PROTESTANT")),
#              protestant.prop = (nrow(subset(x, RELIG ==
#                                               "(01) PROTESTANT")) / nrow(x))*100,
#              none.n = nrow(subset(x, RELIG == "(04) NONE")),
#              none.prop = (nrow(subset(x, RELIG ==
#                                               "(04) NONE")) / nrow(x))*100,
#              catholic.n = nrow(subset(x, RELIG == "(02) CATHOLIC")),
#              catholic.prop = (nrow(subset(x, RELIG ==
#                                               "(02) CATHOLIC")) / nrow(x))*100
#   )
# }
# )
results<-ddply(na.omit(Nreligid_region), .var=c("NEWREGIONID","MILLENNIALS"), .fun=summarize, 
               n=length(RELIG),
               protestant.n=sum(RELIG=="(01) PROTESTANT"),
               protestant.prop=(protestant.n/n)*100,
               none.n=sum(RELIG=="(04) NONE"),
               none.prop=(none.n/n)*100,
               catholic.n=sum(RELIG=="(02) CATHOLIC"),
               catholic.prop=(catholic.n/n)*100)
               
millennials.results <- na.omit(subset(results, MILLENNIALS == "Millennials"))
#millennials.results

#Graph proportionsn of catholic/protestant/none results for the south
millennials.results.prop <- melt(millennials.results, id.vars="NEWREGIONID", measure.vars=c("protestant.prop","none.prop","catholic.prop"))
ggplot(millennials.results.prop, aes(x=variable, y=value, fill=NEWREGIONID))+
  geom_bar(stat="identity", position="dodge")+
  xlab("millennials by affilliation")+ylab("Percentage")+
  ggtitle("Millennials Regional Affiliation by Religious Affiliation")+
  scale_fill_discrete(name="Region",
   breaks=c("Midwest", "North","South","West"),
    labels=c("Midwest", "North", "South", "West"))+
   scale_x_discrete(breaks=c("protestant.prop","none.prop","catholic.prop"),
    labels=c("Protestant","Catholic","None"))
    

  
## @knitr MillennialsAffiliationByRegion  
#ggplot(millennials.results.prop, aes(x=variable, y=value, fill=NEWREGIONID))+geom_bar(stat="identity", position="dodge")+xlab("millennials by affilliation")+ylab("Percentage")
ggplot(millennials.results.prop, aes(x=NEWREGIONID, y=value, fill=variable))+
  geom_bar(stat="identity", position="dodge")+
  xlab("millennials by affilliation")+ylab("Percentage")+
  ggtitle("Millennials Affiliation by Region")+
  scale_fill_discrete(name="Affiliation",
                      breaks=c("protestant.prop","none.prop","catholic.prop"),
                      labels=c("Protestant","None","Catholic"))

#ggplot(millennials.results.prop, aes(x=NEWREGIONID, y=value, fill=variable))+geom_bar(stat="identity", position="dodge")+xlab("millennials by affilliation")+ylab("Percentage")+title("Millennials Affilliation by Region")

## @knitr ConversionRatesSetup
#Conversion rates from/to Catholic/Protestant/None
# Both a proportion and actual numbers
# results16 <- ddply(.data = Nreligid_region, .var = c("NEWREGIONID", "RELIG16", "MILLENNIALS"), .fun = function(x) {
#   data.frame(n = nrow(x),
#              protestant.n = nrow(subset(x, RELIG == "(01) PROTESTANT")),
#              protestant.prop = (nrow(subset(x, RELIG ==
#                                               "(01) PROTESTANT")) / nrow(x))*100,
#              none.n = nrow(subset(x, RELIG == "(04) NONE")),
#              none.prop = (nrow(subset(x, RELIG ==
#                                         "(04) NONE")) / nrow(x))*100,
#              catholic.n = nrow(subset(x, RELIG == " (02) CATHOLIC")),
#              catholic.prop = (nrow(subset(x, RELIG ==
#                                             "(02) CATHOLIC")) / nrow(x))*100
#   )
# }
# )

results16 <- ddply(.data = na.omit(Nreligid_region), .var = c("NEWREGIONID", "RELIG16", "MILLENNIALS"), .fun = summarize,
                   n=length(RELIG),
                   protestant.n=sum(RELIG=="(01) PROTESTANT"),
                   protestant.prop=(protestant.n/n)*100,
                   none.n=sum(RELIG=="(04) NONE"),
                   none.prop=(none.n/n)*100,
                   catholic.n=sum(RELIG=="(02) CATHOLIC"),
                   catholic.prop=(catholic.n/n)*100)


#results16[results16$NEWREGIONID=="South",]
#results16[results16$MILLENNIALS=="Millennials",]
relchange<-subset(results16, MILLENNIALS == "Millennials" & NEWREGIONID =="South" & RELIG16 %in% c("(01) PROTESTANT","(02) CATHOLIC","(04) NONE"))
relchange.melt.n<-melt(relchange, id.vars=c("NEWREGIONID","RELIG16","MILLENNIALS","n"), measure.vars=c("protestant.n","catholic.n","none.n"))
relchange.melt<-melt(relchange, id.vars=c("NEWREGIONID","RELIG16","MILLENNIALS"), measure.vars=c("protestant.prop","catholic.prop","none.prop"))
#relchange.melt.n
#levels(relchange.melt$variable)<-c("Protestant","Catholic","None")
relchange.melt$RELIG16.lab<- levels(relchange.melt$RELIG16) [as.numeric(relchange.melt$RELIG16)]
relchange.melt$variable.lab <- levels(relchange.melt$variable) [as.numeric(relchange.melt$variable)]
relchange.comp.melt<-relchange.melt
#relchange.melt<-subset(relchange.melt, RELIG16.lab!=variable.lab)

## @knitr PlotRelChange
convmil<-ggplot(relchange.melt, aes(x=RELIG16,y=value, fill=variable))+geom_bar(stat="identity")+
  xlab("Original Religious Affilliation (South)")+ylab("Percentage")
#convmil
 convmil+ scale_fill_discrete(name="Current Affilliation",
                      breaks=c("protestant.prop","catholic.prop","none.prop"),
                      labels=c("Protestant", "Catholicism", "None"))+
  ggtitle("Change/Conversion of Southern Millennials")

## @knitr PlotRelChangeCount
ggplot(relchange.melt.n, aes(x=RELIG16, y=value, fill=variable))+geom_bar(stat="identity", position="dodge")+
  scale_fill_discrete(name="Conversion N",
                      breaks=c("protestant.n","catholic.n","none.n"),
                      labels=c("Protestants","Catholics","None"))+
  xlab("Original Religious Identification")+ylab("Number")+
  ggtitle("Religious Change in Southern Millennials (Count)")

## @knitr RelChangeRegionsSetup
#Conversion across all regions
#head(results16)
relchange.all<-na.omit(subset(results16,MILLENNIALS=="Millennials" & RELIG16 %in% c("(01) PROTESTANT","(02) CATHOLIC","(04) NONE")))
relchange.all.melt.n<-melt(relchange.all, id.vars=c("NEWREGIONID","RELIG16","MILLENNIALS","n"), measure.vars=c("protestant.n","catholic.n","none.n"))
relchange.all.melt<-melt(relchange.all, id.vars=c("NEWREGIONID","RELIG16","MILLENNIALS"), measure.vars=c("protestant.prop","catholic.prop","none.prop"))
#relchange.all.melt
#relchange.all.melt.n
levels(relchange.all.melt$variable)<-c("Protestant","Catholic","None")
relchange.all.melt$RELIG16.lab<- levels(relchange.all.melt$RELIG16) [as.numeric(relchange.all.melt$RELIG16)]
relchange.all.melt$variable.lab <- levels(relchange.all.melt$variable) [as.numeric(relchange.all.melt$variable)]
relchange.all.melt.noidentity<-subset(relchange.all.melt, RELIG16.lab!=variable.lab)

## @knitr RegionReligionChangePlot
#separate bar plot all regions include original identity
ggplot(relchange.all.melt.noidentity, aes(x=RELIG16,y=value, fill=variable))+geom_bar(stat="identity", position="dodge")+
  xlab("Original Religious Affilliation")+ylab("Percentage")+
  scale_fill_discrete(name="Current Affiliation",
  #                    breaks=c("protestant.prop", "catholic.prop", "none.prop"),
                      labels=c("Protestant", "Catholicism", "None"))+
  ggtitle("Conversion of All Millennials By Region")+facet_grid(.~NEWREGIONID)

## @knitr RegionReligionChangeFromTo
#separate bar plot all regions exclude original identity
ggplot(subset(relchange.all.melt.noidentity, RELIG16.lab!=toupper(variable.lab)), aes(x=RELIG16,y=value, fill=variable))+geom_bar(stat="identity", position="dodge")+
  xlab("Original Religious Affilliation")+ylab("Percentage")+
  scale_fill_discrete(name="Current Affiliation",
                      #                    breaks=c("protestant.prop", "catholic.prop", "none.prop"),
                      labels=c("Protestant", "Catholicism", "None"))+
  ggtitle("Conversion of All Millennials By Region")+facet_grid(.~NEWREGIONID)

## @knitr ConversionRegionFromToStacked
#stacked bar plot all regions include original identity percentages
ggplot(relchange.all.melt, aes(x=RELIG16,y=value, fill=variable))+geom_bar(stat="identity")+
  xlab("Original Religious Affilliation")+ylab("Percentage")+
  scale_fill_grey(name="Conversion\nType",
                      #                    breaks=c("protestant.prop", "catholic.prop", "none.prop"),
                      labels=c("to Protestant", "to Catholicism", "to None"), start = 0, end=0.5)+
  ggtitle("Conversion of All Millennials By Region")+facet_grid(.~NEWREGIONID)

## @knitr ConverMillennialsRegionStacked
#stacked bar plot all regions exclude original identity percentages
ggplot(subset(relchange.all.melt, RELIG16.lab!=toupper(variable.lab)), aes(x=RELIG16,y=value, fill=variable))+
  geom_bar(stat="identity")+
  xlab("Original Religious Affilliation")+ylab("Percentage")+
  scale_fill_grey(name="Conversion\nType",
                      #                    breaks=c("protestant.prop", "catholic.prop", "none.prop"),
                      labels=c("to Protestant", "to Catholicism", "to None"), start=0, end=0.5)+
  scale_x_discrete(labels=c("Protestant","Catholic","None"))+
  theme(axis.text.x = element_text(vjust=grid::unit(c(0, .3, .7), "points")))+
  facet_grid(.~NEWREGIONID)+
  theme(axis.title.x = element_text(margin = margin(t=-80)))+
  ggtitle("Conversion of All Millennials By Region")+
  labs(caption="General Social Survery 2008-2014")
  

## @knitr ConverMillennialsRegionStackedCount
#stacked bar plot all regions include original identity count
ggplot(relchange.all.melt.n, aes(x=RELIG16, y=value, fill=variable))+geom_bar(stat="identity", position="dodge")+
  scale_fill_discrete(name="Conversion N",
                     # breaks=c("protestant.prop","catholic.prop","none.prop"),
                      labels=c("Protestants","Catholics","None"))+
  xlab("Original Religious Identification")+ylab("Number")+
  ggtitle("Religious Change in Millennials All Regions")+facet_grid(.~NEWREGIONID)

## @knitr ConverMillennialsRegionChord
# pare down datafrome to value, RELIG16.lab, and variable.lab and NEWREGIONID

chordRelchange<-relchange.all.melt %>%
  mutate(RELIG16.lab=substr(RELIG16.lab,6,9)) %>%
  mutate(RELIG16.lab=tolower(paste(NEWREGIONID, RELIG16.lab, sep="-"))) %>%
  select(RELIG16.lab, variable.lab, value)
chordRelchange$RELIG16.lab=str_wrap(chordRelchange$RELIG16.lab, width=6)
chordRelchange$variable.lab<-sub("^", "to ", chordRelchange$variable.lab)
par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0))
#circos.axis(labels.cex=par(3))
circos.clear()
col_fun=colorRamp2(range(chordRelchange$value), c("#333333", "#888888"))
grid.col="grey"
chordDiagram(chordRelchange, directional=1, grid.col="grey", col=col_fun)

## @knitr ConvMillennialsRegionCount
#stacked bar plot all regions exclude original identity count
ggplot(subset(relchange.all.melt.n, RELIG16!=toupper(str_sub(variable,1,-3))), aes(x=RELIG16, y=value, fill=variable))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_discrete(name="Conversion N",
                      # breaks=c("protestant.prop","catholic.prop","none.prop"),
                      labels=c("Protestants","Catholics","None"))+
  xlab("Original Religious Identification")+ylab("Number")+
  ggtitle("Religious Change in Millennials All Regions (count)")+facet_grid(.~NEWREGIONID)+
  scale_x_discrete(labels=c("Prot","Cath","None"))

## @knitr birthrightNonesVallNonesSetup
#birthright nones v. all nones
orig16 <- ddply(.data = na.omit(subset(Nreligid_region, RELIG16 %in% c("(01) PROTESTANT","(02) CATHOLIC","(04) NONE"))), .var = c("NEWREGIONID", "RELIG16", "MILLENNIALS"), .fun = summarize,
  n=length(RELIG),
   none.n=sum(RELIG == "(04) NONE"),
   none.prop = none.n/n*100
  )

#orig16

## @knitr birthrightNonesVAllNonesPlot
#plot birthright nones v. all by original religious ID Millennials
ggplot(subset(orig16, MILLENNIALS=="Millennials"), aes(x=RELIG16, y=none.prop, fill=NEWREGIONID))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Birthright Nones v.s. Converts - Millennials Only")+
  xlab("Nones Religious Idenfication at 16")+ylab("Percentage")

## @knitr birthrightNonesVAllNonesNonMillennialsPlot
#plot birthright nones v. all by original Religous ID Non-Millennials only
ggplot(subset(orig16, MILLENNIALS=="Non-Millennials"), aes(x=RELIG16, y=none.prop, fill=NEWREGIONID))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Birthright Nones v.s. Converts - Non-Millennials Only")+
  xlab("Nones Religious Idenfication at 16")+ylab("Percentage")

## @knitr birthrightNonesVAllNonesMillennialsCount
#Plot birthright nones v. all by Original Religious ID (Count) Millennials only
ggplot(subset(orig16, MILLENNIALS=="Millennials"), aes(x=RELIG16, y=none.n, fill=NEWREGIONID))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Birthright Nones v.s. Converts - Millennials Only")+
  xlab("Nones Religious Idenfication at 16 (count)")+ylab("Count")

## @knitr birthrightNonesVAllNonesNonMillennialsCount
#Plot birthrigh nones v. all by Original Religious ID (Count) Non-Millennials
ggplot(subset(orig16, MILLENNIALS=="Non-Millennials"), aes(x=RELIG16, y=none.n, fill=NEWREGIONID))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Birthright Nones v.s. Converts - Non-Millennials Only")+
  xlab("Nones Religious Idenfication at 16")+ylab("Count")


################### From Here On, Only Experimental Stuff ###########################
## @knitr millennialAttitudeTowardChurch
#millennials attitude toward the church - This was only asked in 08
millennials_subset_noNA<-subset(millennials_subset, CONCHURH!="NA")
ggplot(subset(millennials_subset_noNA, RELIG %in% c("PROTESTANT","CATHOLIC","NONE")), aes(x=CONCHURH, fill=CONCHURH))+
  geom_bar(stat = "bin", position="dodge")+
  scale_x_discrete(labels=c("Complete \n Confidence","Great Deal \n of Confidence","Some \n Confidence","Very Little \n Confidence","No Confidence \n at all","DK"))+
  facet_wrap(~RELIG, ncol=1)

ggplot(subset(millennials_subset_noNA, RELIG=="NONE"), aes(x=CONCHURH, fill=CONCHURH))+geom_bar(stat = "bin", position="dodge")


#table with tables package

Protcathnone<-subset(Nreligid_region,RELIG %in% c("PROTESTANT","CATHOLIC","NONE"))
Protcathnone$RELIG <-factor(Protcathnone$RELIG)
table3<-tabular((Region=NEWREGIONID*MILLENNIALS)*(Percent("row"))
      ~ Heading("Religious Affiliation")*(RELIG), data=Nreligid_region ) 
outtable<-table3[,c(1:2,4)]
outtable
table3
table4<-as.matrix(table3[,c(1:2,4)])
head(Protcathnone)
tabprotcathnone<-Protcathnone[,c(1,5,9,11)]
head(tabprotcathnone)
str(tabprotcathnone)

#abbreviate function for use with labels in the barchart
abbrev <- function(text, width=10, split=" "){
  if (is.list(text)) return(lapply(text, abbrev, width=width,
                                   split=split)) 
  if (length(text) > 1)
    return(as.vector(sapply(text, abbrev, width=width, split=split)))
  words <- strsplit(text, split=split)[[1]]
  words <- ifelse(nchar(words) <= width, words, 
                  abbreviate(words, minlength=width))
  words <- paste(words, collapse=" ")
  paste(strwrap(words, width=width), collapse="\n")
}

table5<-table(tabprotcathnone) 
table5
ftable(table5)
table6<-xtabs(~RELIG+ATTEND, data=Protcathnone)
ftable(table6)
fbba<-c("red","darkgreen","cornflowerblue")
modified.names<-abbrev(colnames(table6))
par(mar=c(3.1, 4.1, 2.1, 1.1)+0.1)
#par(oma=c(0,0,0,0))

barplot(100*prop.table(table6),
        col=fbba,
        names.arg=modified.names,
        #horiz=T,
        las=1,
        cex.names=0.7,
        beside=T,
        legend.text=rownames(table6)
        #xlim=c(0, 1200)
        )
mosaicplot(table6, col=rainbow(ncol(table6)))
colnames(table6)
colnames(table6)<-c("< Yearly","Yearly",">Yearly","Monthly","2/3 Month", "<Weekly","Weekly",">Weekly","NA/DK")
mosaicplot(table6, col=rainbow(ncol(table6)))
mosaicplot(table6, shade=T, main="Residuals for correlation Attendance/Affiliation")

#correllate church attendance with none status
Nreligid_region$NONE<-ifelse(Nreligid_region$RELIG=="NONE",TRUE,FALSE)
cor.test(as.numeric(Nreligid_region$ATTEND),as.numeric(Nreligid_region$NONE))

#Pearson's product-moment correlation
#
#data:  as.numeric(Nreligid_region$ATTEND) and as.numeric(Nreligid_region$NONE)
#t = -35.7602, df = 4818, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#-0.4800082 -0.4353797
#sample estimates:
#cor 
#-0.4579825 
#
#results t(4818)-35.76, p<.001 cor -0.458 
#95 % confidence does not cross zero
#Negative correlation, with high confidence
#
#X2 test 
chisq.test(Nreligid_region$NEWATTEND, Nreligid_region$RELIG)
chisq.test(gss_millenials_so$NEWATTEND, gss_millenials_so$RELIG)
attendvrelig<-table(gss_millenials_so$NEWATTEND, gss_millenials_so$RELIG)
chisq.test(attendvrelig)


describe(Nreligid_region$NEWATTEND)
describe(Nreligid_region$NONE)
describe(gss_millenials_so$NEWATTEND)
hist(as.numeric(Nreligid_region$NEWATTEND))
hist(as.numeric(gss_millennials$NEWATTEND))
hist(as.numeric(gss_millenials_so$NEWATTEND))

#Millennial Church attendance 2008-2012
#create time series for millennials
tab2 = function(x, useNA =FALSE) {
  if(!useNA) if(any(is.na(x))) x = na.omit(x)
  n = length(x)
  out = data.frame(x,1) %>%
    group_by(x) %>%
    dplyr::summarise(
      Freq    = length(X1),
      Percent = (Freq/n)*100
    ) %>%
    dplyr::arrange(x)
  ids = as.character(out$x)
  ids[is.na(ids)] = '<NA>'
  out = select(out, Freq, Percent)
  out$cum = cumsum(out$Percent)
  class(out)="data.frame"
  out = rbind(out,c(n,1,NA))
  rownames(out) = c(ids,'')
  out
}


require('zoo')
require('dplyr')
millennials_at<-data.frame(YEAR=as.numeric(millennials_subset$YEAR), ATTEND=millennials_subset$ATTEND)
millennials_attend_tab<-(tab<-prop.table(table(millennials_at),1)*100)
millennials_attend_df<-data.frame(millennials_attend_tab)
millennials_attend_df$YEAR<-as.numeric(levels(millennials_attend_df$YEAR))
millennials_attendance <- tbl_df(millennials_attend_df %>% 
                                   arrange(YEAR, ATTEND)%>%
                                   #summarize(attendcount = tab2(millennials_at$ATTEND)$Percent) 
                                   filter(ATTEND %in% c("MORE THN ONCE WK","EVERY WEEK","NEARLY EVERY WEEK", "2-3X A MONTH")) %>%
                                   group_by(YEAR)%>%
                                   summarize(yrattendcount=sum(Freq))
)
millennials_attendance$YEAR<-as.POSIXct(strptime(millennials_attendance$YEAR, "%Y"))
millennials_attendance_ts<-read.zoo(millennials_attendance, index.column=millennials_attendance$YEAR, drop=FALSE)

plot(millennials_attendance_ts)

#stackoverflow reproducible
Region<-rep(c('Midwest'),9)
RELIG16<-rep(c('Protestant','Catholic','None'),3)
OutRel<-rep(c('Protestant'), 3) 
OutRel<-append(OutRel, rep(c('Catholic'),3))
OutRel<-append(OutRel, rep(c('None'),3))
value<-rep(c(77.35, 10.25, 18.18),3)
df=data.frame(Region,RELIG16,OutRel,value)
ggplot(df, aes(x=RELIG16,y=value, fill=OutRel))+geom_bar(stat="identity")+
  xlab("Original Religious Affilliation")+ylab("Percentage")+
  scale_fill_discrete(name="Conversion\nType",
                      #                    breaks=c("protestant.prop", "catholic.prop", "none.prop"),
                      labels=c("to Protestant", "to Catholicism", "to None"))+
  ggtitle("Conversion of All Millennials By Region")
