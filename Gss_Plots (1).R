#packages (Install once)
install.packages("tables") #tabular package
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("foreign")
install.packages("reshape")

library("Hmisc")
library("ggplot2")
library("tables")
library("gridExtra")
library("dplyr")
library("foreign")
library("reshape")

#copy gss 2008-2012 data

gss2012<-gss2008_12.relcombine
names(gss2012)
names(gss2012)<-toupper(names(gss2012))
names(gss2012)
      
#Create religid_region
Nreligid_region<-gss2012[,c("RELIG","REGION")]
Nreligid_region$REGIONID<-as.numeric(gss2012$REGION)
Nreligid_region$RELIGID<-as.numeric(gss2012$RELIG)
lookupRegion<-data.frame(regionid=c('2','3','4','5','6','7','8','9','10'), region=c('North',"North","Midwest","Midwest","South","South","South","West","West"))
Nreligid_region$NEWREGIONID <- lookupRegion$region[match(Nreligid_region$REGIONID, lookupRegion$regionid)]
#Add data of everyone born again
Nreligid_region$AGE<-gss2012$AGE
Nreligid_region$REBORN<-gss2012$REBORN
#millennials<-c("18","19","20","21","22","23","24","25","26","27","28","29")
Nreligid_region$COHORT<-gss2012$COHORT
#Nreligid_region<-within(Nreligid_region, MILLENNIALS <- {ifelse(Nreligid_region$AGE %in% millennials, TRUE, FALSE)})
Nreligid_region<-within(Nreligid_region, MILLENNIALS <- {ifelse(as.numeric(as.character(Nreligid_region$COHORT)) >= 1985, TRUE, FALSE)})
Nreligid_region$MILLENNIALS<-as.factor(Nreligid_region$MILLENNIALS)
levels(Nreligid_region$MILLENNIALS)<-c("Non-Millennials","Millennials")
Nreligid_region$RELIG16<-gss2012$RELIG16
Nreligid_region$ATTEND<-gss2012$ATTEND
head(Nreligid_region, 20)
head(Nreligid_region[8:10],20)

#Create Tables for Graph
gss_millennials<-subset(Nreligid_region, Nreligid_region$MILLENNIALS=="Millennials")
millennials_table_by_revised_region <- round((prop.table(table(gss_millennials$RELIG, gss_millennials$NEWREGIONID),2)*100),3)
millennials_table_by_revised_region
colSums(millennials_table_by_revised_region)
# all nones
percentage_table_by_revised_region <- round(prop.table(table(Nreligid_region$RELIG, Nreligid_region$NEWREGIONID),2 )*100, 3)
percentage_table_by_revised_region
colSums(percentage_table_by_revised_region)
#comparison table
par(mar=c(5.1,4.1,4.1,2.1))
nones_compare<-cbind(millennials_table_by_revised_region[4,1], percentage_table_by_revised_region[4,1],millennials_table_by_revised_region[4,2], percentage_table_by_revised_region[4,2],millennials_table_by_revised_region[4,3], percentage_table_by_revised_region[4,3],millennials_table_by_revised_region[4,4], percentage_table_by_revised_region[4,4])
colnames(nones_compare)<-c("Millennials Midwest","All Midwest", "Millennials North","All North","Millennials South","All South","Millennials West","All West" )
compare_nones_chart<-barplot(nones_compare, beside=TRUE, col=c("blue","red"),  axis.lty=1, las=2, main="Millennials Nones v All Nones By Region")
#text(compare_nones_chart, nones_compare, labels=nones_compare, pos=3, cex=0.75)
abline(h=mean(nones_compare))
#create a table ggplot of millennials v. all
#library(reshape)
millennials_summary_by_region<-subset(melt(millennials_table_by_revised_region), Var.1=="None" & Var.2 !="Not Assigned")
millennials_summary_by_region$MILLENNIALS <- factor(TRUE)
nonmillennials_summary_by_region<-subset(melt(percentage_table_by_revised_region), Var.1=="None" & Var.2 !="Not Assigned")
nonmillennials_summary_by_region$MILLENNIALS <- factor(FALSE)
all_summary_by_region<-merge(millennials_summary_by_region, nonmillennials_summary_by_region, all=TRUE)
levels(all_summary_by_region$MILLENNIALS)<-c("Millennials","Non-Millennials")
#comparison table using ggplot
#library(ggplot2)
ggplot(all_summary_by_region, aes(x=Var.2, y=value, fill=MILLENNIALS)) + geom_bar(stat="identity", position="dodge")+ylab("Perecentage")+xlab("Region")+ggtitle("Millennial v. Non-Millennial Nones by Region")

# percentage of nones who are born again

round((prop.table(table(Nreligid_region$RELIG, Nreligid_region$NEWREGIONID, Nreligid_region$REBORN, exclude=c("Don't know", "No answer")),2)*100),3)
Born_again_Subset<-subset(Nreligid_region, Nreligid_region$REBORN=="Yes")
Born_again_table<-round((prop.table(table(Born_again_Subset$RELIG, Born_again_Subset$NEWREGIONID),2)*100),3)
Born_again_subset_wide<-melt(Born_again_table)
Born_again_subset_wide_select<-subset(Born_again_subset_wide, Born_again_subset_wide$Var.1 %in% c("Protestant","None","Catholic"))
ggplot(Born_again_subset_wide_select, aes(x=Var.2, y=value, fill=Var.1)) + geom_bar(stat="identity", position="dodge")+ylab("Perecentage")+xlab("Region")+ggtitle("Religious Identification of 'Born Again' by Region")

#percentage of millennials who are born again
round((prop.table(table(gss_millennials$RELIG, gss_millennials$NEWREGIONID),2)*100),3)
round((prop.table(table(gss_millennials$REBORN, gss_millennials$NEWREGIONID),2)*100),3)
gss_millennials<-subset(gss_millennials, REBORN %in% c("Yes","No"))
ggplot(gss_millennials, aes(x=gss_millennials$REBORN, fill=gss_millennials$NEWREGIONID))+geom_bar(stat="bin", position="dodge")+xlab("Born Again")+ylab("Number of Adherents")+ggtitle("Count of Millennials Are Born Again")+scale_fill_discrete(name="Region")
born_again_and_millennials <- subset(melt(round((prop.table(table(gss_millennials$REBORN, gss_millennials$NEWREGIONID),2)*100),3)), Var.1==c("Yes","No"))
ggplot(born_again_and_millennials, aes(x=born_again_and_millennials$Var.1, y=born_again_and_millennials$value, fill=born_again_and_millennials$Var.2))+geom_bar(stat="identity", position="dodge")+ylab("Percentage")+xlab("Born Again?")+ggtitle("Millennial Identification as 'Born Again'")+scale_fill_discrete(name="Region")
table(gss_millennials$REBORN, gss_millennials$RELIG)
round((prop.table(table(gss_millennials$REBORN, gss_millennials$RELIG),2)*100),3)
round((prop.table(table(Nreligid_region$RELIG, Nreligid_region$REBORN, Nreligid_region$MILLENNIALS),1)*100),3)

#Millennials v. Non-Millennials self-identified Born Again
round((prop.table(table(Nreligid_region$REBORN, Nreligid_region$MILLENNIALS),2)*100),3)
BAtab<-summary(MILLENNIALS ~ REBORN, method="reverse", data=Nreligid_region)
BAtab
plot(BAtab)

#Protestants born again, millennial v non-millennial
Protestant_Only<-subset(Nreligid_region, Nreligid_region$RELIG=="Protestant")
round((prop.table(table(Protestant_Only$REBORN, Protestant_Only$MILLENNIALS),2)*100),3)


#non-millennials identification as born again
non_millennials<-subset(Nreligid_region, Nreligid_region$MILLENNIALS=="Non-Millennials")
round((prop.table(table(non_millennials$REBORN, non_millennials$RELIG),2)*100),3)
table3<-tabular(Heading("Non-Millennials Identification as 'Born Again'")*REBORN*Percent("col") ~ (Religion=RELIG),data=non_millennials) 
table3[1:2,c(1:2,4)]
ggplot(non_millennials, aes(x=REBORN, fill=REBORN))+
  geom_bar(stat="bin", position="dodge")+
  facet_wrap(~RELIG, ncol=3)+
  ggtitle("Non-millennials Identification as 'Born Again'")


#proportion of millenial protestant/none/catholics using plyr
require(plyr)
results <- ddply(.data = Nreligid_region, .var = c("NEWREGIONID", "MILLENNIALS"), .fun = function(x) {
  data.frame(n = nrow(x),
             protestant.n = nrow(subset(x, RELIG == "Protestant")),
             protestant.prop = (nrow(subset(x, RELIG ==
                                              "Protestant")) / nrow(x))*100,
             none.n = nrow(subset(x, RELIG == "None")),
             none.prop = (nrow(subset(x, RELIG ==
                                              "None")) / nrow(x))*100,
             catholic.n = nrow(subset(x, RELIG == "Catholic")),
             catholic.prop = (nrow(subset(x, RELIG ==
                                              "Catholic")) / nrow(x))*100
  )
}
)
millennials.results <- subset(results, MILLENNIALS == "Millennials")
#millennials.results
#Graph proportionsn of catholic/protestant/none results for the south
millennials.results.prop <- melt(millennials.results, id.vars="NEWREGIONID", measure.vars=c("protestant.prop","none.prop","catholic.prop"))
ggplot(millennials.results.prop, aes(x=variable, y=value, fill=NEWREGIONID))+
  geom_bar(stat="identity", position="dodge")+
  xlab("millennials by affilliation")+ylab("Percentage")+
  ggtitle("Millennials Affilliation by Region")+
  scale_fill_discrete(name="Region",
   breaks=c("Midwest", "North","South","West"),
    labels=c("Midwest", "North", "South", "West"))
  
  
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
             protestant.n = nrow(subset(x, RELIG == "Protestant")),
             protestant.prop = (nrow(subset(x, RELIG ==
                                              "Protestant")) / nrow(x))*100,
             none.n = nrow(subset(x, RELIG == "None")),
             none.prop = (nrow(subset(x, RELIG ==
                                        "None")) / nrow(x))*100,
             catholic.n = nrow(subset(x, RELIG == "Catholic")),
             catholic.prop = (nrow(subset(x, RELIG ==
                                            "Catholic")) / nrow(x))*100
  )
}
)
#results16[results16$NEWREGIONID=="South",]
#results16[results16$MILLENNIALS=="Millennials",]
relchange<-subset(results16, MILLENNIALS == "Millennials" & NEWREGIONID =="South" & RELIG16 %in% c("Protestant","Catholic","None"))
relchange.melt.n<-melt(relchange, id.vars=c("NEWREGIONID","RELIG16","MILLENNIALS","n"), measure.vars=c("protestant.n","catholic.n","none.n"))
relchange.melt<-melt(relchange, id.vars=c("NEWREGIONID","RELIG16","MILLENNIALS"), measure.vars=c("protestant.prop","catholic.prop","none.prop"))
relchange.melt.n
levels(relchange.melt$variable)<-c("Protestant","Catholic","None")
relchange.melt$RELIG16.lab<- levels(relchange.melt$RELIG16) [as.numeric(relchange.melt$RELIG16)]
relchange.melt$variable.lab <- levels(relchange.melt$variable) [as.numeric(relchange.melt$variable)]
relchange.comp.melt<-relchange.melt
#relchange.melt<-subset(relchange.melt, RELIG16.lab!=variable.lab)

convmil<-ggplot(relchange.melt, aes(x=RELIG16,y=value, fill=variable))+geom_bar(stat="identity")+
  xlab("Original Religious Affilliation")+ylab("Percentage")
convmil
 convmil+ scale_fill_discrete(name="Current Affilliation",
                      breaks=c("Protestant", "Catholic", "None"),
                      labels=c("Protestant", "Catholicism", "None"))+
  ggtitle("Change/Conversion of Southern Millennials")

ggplot(relchange.melt.n, aes(x=RELIG16, y=value, fill=variable))+geom_bar(stat="identity", postition="dodge")+
  scale_fill_discrete(name="Conversion N",
                      breaks=c("protestant.n","catholic.n","none.n"),
                      labels=c("Protestants","Catholics","None"))+
  xlab("Original Religious Identification")+ylab("Number")+ggtitle("Religious Change in Southern Millennials")

#Conversion across all regions
head(results16)
relchange.all<-subset(results16,MILLENNIALS=="Millennials" & RELIG16 %in% c("Protestant","Catholic","None"))
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

Protcathnone<-subset(Nreligid_region,RELIG=c("Protestant","Catholic","None"))
table3<-tabular((Region=NEWREGIONID*MILLENNIALS)*(Percent("row"))
      ~ Heading("Religious Affiliation")*(RELIG), data=Nreligid_region ) 
table3[,c(1:2,4)]
table4<-as.matrix(table3[,c(1:2,4)])


#correllate church attendance with none status
Nreligid_region$NONE<-ifelse(Nreligid_region$RELIG=="None",TRUE,FALSE)
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
describe(Nreligid_region$ATTEND)
describe(Nreligid_region$NONE)
scatter<-ggplot(Nreligid_region, aes(as.numeric(ATTEND), as.numeric(RELIG)))+geom_point()+geom_smooth(method="lm")
scatter
plot(Nreligid_region$ATTEND, Nreligid_region$RELIG) #this currently includes all religions, needs to be subsetted



