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
install.packages("reshape")
install.packages("scales")
install.packages("vcd")

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

#these may not be necessary and errored anyway
#devtools::install_github( "romainfrancois/Rcpp11" )
#require( Rcpp11 )
#evalCpp( "2LL" )

devtools::install_github("hadley/dplyr")
devtools::install_github("randywreed/gssReligion", auth_user="randywreed", auth_token="1834calvin")

library("Hmisc")
library("ggplot2")
library("tables")
library("gridExtra")
library("dplyr")
library("foreign")
library("reshape")
library("scales")
library("vcd")
library("gssReligion")
#copy gss 2008-2012 data
#run dl_create_gss.R first to create data file (NOTE: heavy system requirements, please read the comments in file)
# dl_create_gss.R creates gss2008_12.rda
# gss2008_12.rda is loaded into gss2012
load("gss2008_12.rda")
gss2012<-gss2008_12.relcombine
#upcase names
names(gss2012)<-toupper(names(gss2012))
rm("gss2008_12.relcombine")

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

head(Nreligid_region, 20)
head(Nreligid_region[8:10],20)

#Create Subsets
#Millennials only Subset
gss_millennials<-subset(Nreligid_region, Nreligid_region$MILLENNIALS=="Millennials")
#Southern Millennials only
gss_millenials_so<-subset(gss_millennials, gss_millennials$NEWREGIONID=="South")

#Evangelical presence in the country
ggplot(subset(Nreligid_region, REBORN %in% c("YES","NO")), aes(x=REBORN, fill=REBORN))+
  xlab("Born Again Experience?")+
  ylab("Percentage")+ggtitle("National percentage of individuals who have had a 'Born Again' Experience")+
  geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge")+
  theme(legend.position="none")+
  scale_y_continuous(labels=percent)

#Evangelical presence in the country by region
ggplot(subset(Nreligid_region, REBORN %in% c("YES","NO")), aes(x=REBORN, fill=NEWREGIONID))+xlab("Born Again?")+
  ylab("Percentage")+ggtitle("Percentage of Evangelicals in Each Region")+
  geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge")+
    scale_fill_discrete(name="Been Born Again?")+
    scale_y_continuous(labels=percent)

#Regional Comparison of Born Again
ggplot(na.omit(subset(Nreligid_region, REBORN %in% c("YES","NO"))), aes(x=NEWREGIONID, fill=REBORN))+xlab("Born Again?")+
  ylab("Percentage")+ggtitle("Percentage of Evangelicals in Each Region")+
  geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge")+
  scale_fill_discrete(name="Been Born Again?")+
  scale_y_continuous(labels=percent)
    

#Create Tables for Graph - Millennials religious identificaiton by region
millennials_table_by_revised_region <- round((prop.table(table(gss_millennials$RELIG, gss_millennials$NEWREGIONID),2)*100),3)
millennials_table_by_revised_region
colSums(millennials_table_by_revised_region)
# all nones
percentage_table_by_revised_region <- round((prop.table(table(Nreligid_region$RELIG, Nreligid_region$NEWREGIONID),2 )*100), 3)
percentage_table_by_revised_region
colSums(percentage_table_by_revised_region)
#comparison table
#par(mar=c(5.1,4.1,4.1,2.1))
nones_compare<-cbind(millennials_table_by_revised_region[4,1], percentage_table_by_revised_region[4,1],millennials_table_by_revised_region[4,2], percentage_table_by_revised_region[4,2],millennials_table_by_revised_region[4,3], percentage_table_by_revised_region[4,3],millennials_table_by_revised_region[4,4], percentage_table_by_revised_region[4,4])
colnames(nones_compare)<-c("Millennials Midwest","All Midwest", "Millennials North","All North","Millennials South","All South","Millennials West","All West" )
compare_nones_chart<-barplot(nones_compare, beside=TRUE, col=c("blue","red"),  axis.lty=1, las=2, main="Millennials Nones v All Nones By Region")
#text(compare_nones_chart, nones_compare, labels=nones_compare, pos=3, cex=0.75)
abline(h=mean(nones_compare))

#create a table ggplot of millennials v. all
#library(reshape)
millennials_summary_by_region<-subset(melt(millennials_table_by_revised_region), Var.1=="NONE" & Var.2 !="Not Assigned")
millennials_summary_by_region$MILLENNIALS <- factor(TRUE)
nonmillennials_summary_by_region<-subset(melt(percentage_table_by_revised_region), Var.1=="NONE" & Var.2 !="Not Assigned")
nonmillennials_summary_by_region$MILLENNIALS <- factor(FALSE)
all_summary_by_region<-merge(millennials_summary_by_region, nonmillennials_summary_by_region, all=TRUE)
levels(all_summary_by_region$MILLENNIALS)<-c("Millennials","Non-Millennials")
#comparison table using ggplot
#library(ggplot2)
ggplot(all_summary_by_region, aes(x=Var.2, y=value, fill=MILLENNIALS)) + 
  geom_bar(stat="identity", position="dodge")+ylab("Perecentage")+xlab("Region")+
  ggtitle("Millennial v. Non-Millennial Nones by Region")+
  scale_fill_discrete(name="Religiously Unaffiliated (Nones)", 
                      labels=c("Millennial Nones","Non-Millennial Nones"))

# percentage of nones who are born again


round((prop.table(table(Nreligid_region$RELIG, Nreligid_region$NEWREGIONID, Nreligid_region$REBORN, exclude=c("iap","dk", "na")),2)*100),3)
Born_again_Subset<-subset(Nreligid_region, toupper(Nreligid_region$REBORN)=="YES")
Born_again_table<-round((prop.table(table(Born_again_Subset$RELIG, Born_again_Subset$NEWREGIONID),2)*100),3)
Born_again_subset_wide<-melt(Born_again_table)
Born_again_subset_wide_select<-subset(Born_again_subset_wide, Born_again_subset_wide$Var.1 %in% toupper(c("Protestant","None","Catholic")))
ggplot(Born_again_subset_wide_select, aes(x=Var.2, y=value, fill=Var.1)) + geom_bar(stat="identity", position="dodge")+ylab("Perecentage")+xlab("Region")+ggtitle("Religious Identification of 'Born Again' by Region")

#percentage of millennials who are born again
gss_millennials$REBORN<-factor(gss_millennials$REBORN)
levels(gss_millennials$REBORN)<-c("Yes","No")
round((prop.table(table(gss_millennials$RELIG, gss_millennials$NEWREGIONID),2)*100),3)
round((prop.table(table(gss_millennials$REBORN, gss_millennials$NEWREGIONID),2)*100),3)
gss_millennials<-subset(gss_millennials, toupper(REBORN) %in% c("YES","NO"))
gss_millennials$REBORN<-factor(gss_millennials$REBORN)
ggplot(gss_millennials, aes(x=gss_millennials$REBORN, fill=gss_millennials$NEWREGIONID))+geom_bar(stat="bin", position="dodge")+xlab("Born Again")+ylab("Number of Adherents")+ggtitle("Count of Millennials Are Born Again")+scale_fill_discrete(name="Region")
born_again_and_millennials <- subset(melt(round((prop.table(table(gss_millennials$REBORN, gss_millennials$NEWREGIONID),2)*100),3)), toupper(Var.1) %in% c("YES","NO"))
ggplot(born_again_and_millennials, aes(x=born_again_and_millennials$Var.1, y=born_again_and_millennials$value, fill=born_again_and_millennials$Var.2))+geom_bar(stat="identity", position="dodge")+ylab("Percentage")+xlab("Born Again?")+ggtitle("Millennial Identification as 'Born Again'")+scale_fill_discrete(name="Region")
table(gss_millennials$REBORN, gss_millennials$RELIG)
round((prop.table(table(gss_millennials$REBORN, gss_millennials$RELIG),2)*100),3)
round((prop.table(table(Nreligid_region$RELIG, Nreligid_region$REBORN, Nreligid_region$MILLENNIALS),1)*100),3)

#Millennials v. Non-Millennials self-identified Born Again
round((prop.table(table(Nreligid_region$REBORN, Nreligid_region$MILLENNIALS),2)*100),3)
BAtab<-summary(MILLENNIALS ~ REBORN, method="reverse", data=Nreligid_region)
BAtab
#plot(BAtab)

#Protestants born again, millennial v non-millennial
Protestant_Only<-subset(Nreligid_region, Nreligid_region$RELIG=="PROTESTANT")
round((prop.table(table(Protestant_Only$REBORN, Protestant_Only$MILLENNIALS),2)*100),3)


#non-millennials identification as born again
non_millennials<-subset(Nreligid_region, Nreligid_region$MILLENNIALS=="Non-Millennials")
non_millennials$REBORN<-factor(non_millennials$REBORN)
#levels(non_millennials$REBORN)<-c("Yes","No")
round((prop.table(table(non_millennials$REBORN, non_millennials$RELIG),2)*100),3)
table3<-tabular(Heading("Non-Millennials Identification as 'Born Again'")*REBORN*Percent("col") ~ (Religion=RELIG),data=non_millennials) 
table3
table3[1:2,c(1:2,4)]
ggplot(subset(non_millennials, RELIG %in% c("PROTESTANT","CATHOLIC","NONE")), aes(x=REBORN, fill=REBORN))+
  geom_bar(stat="bin", position="dodge")+
  facet_wrap(~RELIG, ncol=3)+
  ggtitle("Non-millennials Identification as 'Born Again'")

#compare born again identification between millennials and non-millennials by region
Nreligid_region$REBORNFAC<-factor(Nreligid_region$REBORN)
#levels(Nreligid_region$REBORNFAC)<- c("Yes","No")
table(Born_again_Subset$NEWREGIONID,Born_again_Subset$MILLENNIALS)
table(Nreligid_region$NEWREGIONID,Nreligid_region$REBORNFAC,Nreligid_region$MILLENNIALS)
tabular(NEWREGIONID*MILLENNIALS~((REBORNFAC=="YES")+(REBORNFAC=="NO")), data=Nreligid_region)
BornAgainTable<-round((prop.table(table(Nreligid_region$NEWREGIONID,Nreligid_region$REBORNFAC,Nreligid_region$MILLENNIALS),c(3,1))*100),3)
ftable(BornAgainTable)
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

ggplot(BornAgainDF, aes(x=MILLENNIALS, y=Freq, fill=REBORN ))+
  geom_bar( stat="identity", position="dodge")+
  facet_wrap(~NEWREGIONID, ncol=2)+
  ylab("Percentage")+xlab("Non-Millennials v. Millennials")+
  scale_fill_discrete(name="Born Again?")

#Time Series - 4yr chart (2008-12)

time_melt<-melt(prop.table(table(as.factor(Nreligid_region$YEAR),Nreligid_region$REBORNFAC)), id.vars=YEAR)
ggplot(time_melt, aes(x=Var.1, y=value, color=Var.2))+geom_line()+xlab("Year")+ylab("Percentage")+
  scale_y_continuous( labels=percent_format())+
  scale_color_discrete(name="Born Again")+
  ggtitle("Percentage of All Born Again Over Time (2008-12")

#Time Series - 12 yr chart (2000-12)
#load 2000-2012 religion data
# Create data if necessary - This should be run only on large ram comp.
#gss2000_12<-religionModify(subset(gss1990, year>1999))
gss2000_12<-subset(gss2000_12,REBORN %in% c("yes","no"))
gss2000_12$REBORN<-factor(gss2000_12$REBORN)
time_melt<-melt(prop.table(table(as.factor(gss2000_12$YEAR), gss2000_12$REBORN)), id.vars=YEAR)
ggplot(time_melt, aes(x=Var.1, y=value, color=Var.2))+geom_line()+xlab("Year")+ylab("Percentage")+
  scale_y_continuous( labels=percent)+
  scale_color_discrete(name="Born Again")+
  ggtitle("Percentage of All Born Again Over Time (2000-12")

millennials_subset<-subset(Nreligid_region, Nreligid_region$MILLENNIALS=="Millennials")
Reborn_prop<-table(as.factor(millennials_subset$YEAR), 
                 millennials_subset$REBORNFAC, 
                 millennials_subset$NEWREGIONID)
prop.table(Reborn_prop,1)*100
Mills_reborn_prop<-prop.table(ftable(
  xtabs(~millennials_subset$NEWREGIONID+millennials_subset$YEAR+
          millennials_subset$REBORNFAC)),1)*100
Mills_reborn_prop
time_melt<-melt(Mills_reborn_prop, id.vars=YEAR)
ggplot(time_melt, aes(x=value.millennials_subset.YEAR, y=value.Freq, group=value.millennials_subset.REBORNFAC, color=value.millennials_subset.REBORNFAC))+
  geom_line()+
  facet_wrap(~value.millennials_subset.NEWREGIONID, ncol=2)+
  ylab("Percentage")+xlab("Year")+scale_color_discrete(name="Born Again?")+
  ggtitle("Millennial Born Again Percentage Since 2008")

#focus on the south
BornAgainSouthDF<-subset(BornAgainDF, BornAgainDF$NEWREGIONID=="South")
ggplot(BornAgainSouthDF, aes(x=MILLENNIALS,y=Freq, fill=REBORN))+
  geom_bar(stat="identity",position="dodge")+
  #facet_wrap(~NEWREGIONID, ncol=1)+
  ylab("Percentage")+
 # scale_y_continuous(labels = percent_format())+
  scale_fill_discrete(name="Born Again?")+
  ggtitle("Millennials v. Non-Millennial Born Again in the South")


#p = ggplot(mydataf, aes(x = foo)) + 
#  geom_bar(aes(y = (..count..)/sum(..count..))) + 
#  scale_y_continuous(formatter = 'percent')

#proportion of millenial protestant/none/catholics using plyr
require(plyr)
results <- ddply(.data = Nreligid_region, .var = c("NEWREGIONID", "MILLENNIALS"), .fun = function(x) {
  data.frame(n = nrow(x),
             protestant.n = nrow(subset(x, RELIG == "PROTESTANT")),
             protestant.prop = (nrow(subset(x, RELIG ==
                                              "PROTESTANT")) / nrow(x))*100,
             none.n = nrow(subset(x, RELIG == "NONE")),
             none.prop = (nrow(subset(x, RELIG ==
                                              "NONE")) / nrow(x))*100,
             catholic.n = nrow(subset(x, RELIG == "CATHOLIC")),
             catholic.prop = (nrow(subset(x, RELIG ==
                                              "CATHOLIC")) / nrow(x))*100
  )
}
)
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
    labels=c("Protestant","Catholic","None")
    )

  
  
#ggplot(millennials.results.prop, aes(x=variable, y=value, fill=NEWREGIONID))+geom_bar(stat="identity", position="dodge")+xlab("millennials by affilliation")+ylab("Percentage")
ggplot(millennials.results.prop, aes(x=NEWREGIONID, y=value, fill=variable))+
  geom_bar(stat="identity", position="dodge")+
  xlab("millennials by affilliation")+ylab("Percentage")+
  ggtitle("Millennials Affiliation by Region")+
  scale_fill_discrete(name="Affiliation",
                      breaks=c("protestant.prop","none.prop","catholic.prop"),
                      labels=c("Protestant","None","Catholic"))

#ggplot(millennials.results.prop, aes(x=NEWREGIONID, y=value, fill=variable))+geom_bar(stat="identity", position="dodge")+xlab("millennials by affilliation")+ylab("Percentage")+title("Millennials Affilliation by Region")

#Conversion rates from/to Catholic/Protestant/None
# Both a proportion and actual numbers
results16 <- ddply(.data = Nreligid_region, .var = c("NEWREGIONID", "RELIG16", "MILLENNIALS"), .fun = function(x) {
  data.frame(n = nrow(x),
             protestant.n = nrow(subset(x, RELIG == "PROTESTANT")),
             protestant.prop = (nrow(subset(x, RELIG ==
                                              "PROTESTANT")) / nrow(x))*100,
             none.n = nrow(subset(x, RELIG == "NONE")),
             none.prop = (nrow(subset(x, RELIG ==
                                        "NONE")) / nrow(x))*100,
             catholic.n = nrow(subset(x, RELIG == "CATHOLIC")),
             catholic.prop = (nrow(subset(x, RELIG ==
                                            "CATHOLIC")) / nrow(x))*100
  )
}
)
#results16[results16$NEWREGIONID=="South",]
#results16[results16$MILLENNIALS=="Millennials",]
relchange<-subset(results16, MILLENNIALS == "Millennials" & NEWREGIONID =="South" & RELIG16 %in% c("PROTESTANT","CATHOLIC","NONE"))
relchange.melt.n<-melt(relchange, id.vars=c("NEWREGIONID","RELIG16","MILLENNIALS","n"), measure.vars=c("protestant.n","catholic.n","none.n"))
relchange.melt<-melt(relchange, id.vars=c("NEWREGIONID","RELIG16","MILLENNIALS"), measure.vars=c("protestant.prop","catholic.prop","none.prop"))
relchange.melt.n
levels(relchange.melt$variable)<-c("Protestant","Catholic","None")
relchange.melt$RELIG16.lab<- levels(relchange.melt$RELIG16) [as.numeric(relchange.melt$RELIG16)]
relchange.melt$variable.lab <- levels(relchange.melt$variable) [as.numeric(relchange.melt$variable)]
relchange.comp.melt<-relchange.melt
#relchange.melt<-subset(relchange.melt, RELIG16.lab!=variable.lab)

convmil<-ggplot(relchange.melt, aes(x=RELIG16,y=value, fill=variable))+geom_bar(stat="identity")+
  xlab("Original Religious Affilliation (South)")+ylab("Percentage")
convmil
 convmil+ scale_fill_discrete(name="Current Affilliation",
                      breaks=c("PROTESTANT", "CATHOLIC", "NONE"),
                      labels=c("Protestant", "Catholicism", "None"))+
  ggtitle("Change/Conversion of Southern Millennials")

ggplot(relchange.melt.n, aes(x=RELIG16, y=value, fill=variable))+geom_bar(stat="identity", postition="dodge")+
  scale_fill_discrete(name="Conversion N",
                      breaks=c("protestant.n","catholic.n","none.n"),
                      labels=c("Protestants","Catholics","None"))+
  xlab("Original Religious Identification")+ylab("Number")+ggtitle("Religious Change in Southern Millennials (Count)")

#Conversion across all regions
head(results16)
relchange.all<-na.omit(subset(results16,MILLENNIALS=="Millennials" & RELIG16 %in% c("PROTESTANT","CATHOLIC","NONE")))
relchange.all.melt.n<-melt(relchange.all, id.vars=c("NEWREGIONID","RELIG16","MILLENNIALS","n"), measure.vars=c("protestant.n","catholic.n","none.n"))
relchange.all.melt<-melt(relchange.all, id.vars=c("NEWREGIONID","RELIG16","MILLENNIALS"), measure.vars=c("protestant.prop","catholic.prop","none.prop"))
relchange.all.melt
relchange.all.melt.n
levels(relchange.all.melt$variable)<-c("Protestant","Catholic","None")
relchange.all.melt$RELIG16.lab<- levels(relchange.all.melt$RELIG16) [as.numeric(relchange.all.melt$RELIG16)]
relchange.all.melt$variable.lab <- levels(relchange.all.melt$variable) [as.numeric(relchange.all.melt$variable)]
relchange.all.melt.noidentity<-subset(relchange.all.melt, RELIG16.lab!=variable.lab)
ggplot(relchange.all.melt.noidentity, aes(x=RELIG16,y=value, fill=variable))+geom_bar(stat="identity", position="dodge")+
  xlab("Original Religious Affilliation")+ylab("Percentage")+
  scale_fill_discrete(name="Current Affiliation",
  #                    breaks=c("protestant.prop", "catholic.prop", "none.prop"),
                      labels=c("Protestant", "Catholicism", "None"))+
  ggtitle("Conversion of All Millennials By Region")+facet_grid(.~NEWREGIONID)

ggplot(relchange.all.melt, aes(x=RELIG16,y=value, fill=variable))+geom_bar(stat="identity")+
  xlab("Original Religious Affilliation")+ylab("Percentage")+
  scale_fill_discrete(name="Conversion\nType",
                      #                    breaks=c("protestant.prop", "catholic.prop", "none.prop"),
                      labels=c("to Protestant", "to Catholicism", "to None"))+
  ggtitle("Conversion of All Millennials By Region")+facet_grid(.~NEWREGIONID)

ggplot(relchange.all.melt.n, aes(x=RELIG16, y=value, fill=variable))+geom_bar(stat="identity", postition="dodge")+
  scale_fill_discrete(name="Conversion N",
                     # breaks=c("protestant.prop","catholic.prop","none.prop"),
                      labels=c("Protestants","Catholics","None"))+
  xlab("Original Religious Identification")+ylab("Number")+
  ggtitle("Religious Change in Millennials All Regions")+facet_grid(.~NEWREGIONID)

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




