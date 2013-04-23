#######################################################################################
#PreferenceMturk.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript - to be updated when published

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#command below will install each package. if you run this script from the beginning you need to run every single one again
lapply(c("sem","ggplot2", "psych", "RCurl", "irr","pgirmess", 
         "nortest", "moments","GPArotation","nFactors","Hmisc","beanplot","GGally",
         "gridExtra"), library, character.only=T)
########################################################################################################
#IMPORTING DATA
######################################################################################################

#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the templateData object. replace the word template.data by a name that might easier for you to remember and that represents your data
#pref <- read.csv("/Users/rpietro/Google Drive/R/mturk_preference/consolidada_data.csv")
#pref <- read.csv("/Users/Talitha/Google Drive/mturk_preference/consolidada_data.csv")
pref <- read.csv("~/Google Drive/RoR Duke/Applied Projects/mturk_preference/mturkpreferencedata.csv")
#pref <- read.csv("/Users/Sal/Google Drive/mturk_preference/consolidada_data.csv")

#Data for Demographics Graph
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
data <- getURL("https://docs.google.com/spreadsheet/pub?key=0AjVEotAQPzQsdGtFRXR3QXY5NXFDTEFxLVk3eFZhb2c&single=true&gid=0&output=csv")
sociodemographic<-read.csv(textConnection(data))


#Data for Attrition/Duration Graph
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
data1 <- getURL("https://docs.google.com/spreadsheet/pub?key=0AjVEotAQPzQsdGtFRXR3QXY5NXFDTEFxLVk3eFZhb2c&single=true&gid=1&output=csv
")
feasability<-read.csv(textConnection(data1))

########################################################################################################
#DATA MANAGEMENT
######################################################################################################

#below will view data in a spreadsheet format. notice that in this all subsequent commands you have to replace pref with whatever name you chose for your data object in the previous command
#View(pref)
#below will list variable names, classes (integer, factor, etc), alternative responses
str(pref)
#list variable names so that they can be used later
names(pref)
#below will attach your data so that when you execute a command you don't have to write the name of your data object over and over again
attach(pref)

#function below is used to recode variables. things to notice: replace old.var with the variable you are recoding, replace new.var with the variable you want to create. the whole recoding happens within " ". all character and factor variables will be within '', numbers will be displayed with digits (not inside '') or NA (also without ''). see video at http://goo.gl/aDgo4 for more details
#new.var  <- car::recode(old.var, " 1:2 = 'A'; 3 = 'C'; '' = NA; else = 'B' ")

new.Mturk <-as.factor(car::recode(Mturk, "1 = 'Yes'; NA = 'No'"))
new.Age <-as.factor(car::recode(Age,  "2:4 = '18 to 44'; 5:6 = '45 to 64'; 7:9 = '> 65'"))
tabulate(new.Age)
new.Race <-as.factor(car::recode(Race, "1 = 'White'; 2 = 'Afr. Am.'; 3 = 'Asian'; 4:8 = 'Other'"))
tabulate(new.Race)
new.Education <-as.factor(car::recode(Education, "1:2 = 'High'; 3 = 'College'; 4:5 = 'UnderG'; 6:8 = 'Grad'"))
tabulate(new.Education)
new.Income <-as.factor(car::recode(Income, "1 = '<20'; 2:4 = '20~50'; 5:8 = '50~90'; 9 = '>90'"))
tabulate(new.Income)
new.MaritalStat <-as.factor(car::recode(MaritalStat, "1 = 'Single'; c(2,6) = 'Married'; 3:5 = 'Other'"))
tabulate(new.MaritalStat)
new.Gender <-as.factor(car::recode(Gender, "1 = 'Male'; 2 = 'Female'"))
tabulate(new.Gender)

#Creating dataframe with recoded variables
mt<-data.frame(Mturk,new.Age,new.Race,new.Education,new.Income,
               new.MaritalStat,new.Gender,Attention,EliminDurate,
               WillToLive,Comorbidities,Inconsistent,MturkTTOSlider)

###########################################################################################
#TABLE 1: DEMOGRAPHICS
###########################################################################################
#Ggplot2 function to create a graph for the sociodemographic variables characterization
qplot(data, subclass, data=sociodemographic, fill=data, size=data, colour=data) + 
  facet_grid(class~ . ~ set, scales="free_y", space = "free") +
  scale_area()+  xlab("Frequency (%)") + ylab ("")+ theme(legend.position = "none")
Hmisc::describe(mt)
#######################################################################################
#Figure 2. Feasability.
#######################################################################################
##Creating subset for mturk respondents
mturk <- subset(mt,Mturk == 1)
##Creating subsett and poportions of sample loss per sociodemographic variable
Dur<-factor(mturk$EliminDurate)
prop.table(table(Dur,mturk$new.Age),2)
table(Dur,mturk$new.Age)
prop.table(table(Dur,mturk$new.Race),2)
table(Dur,mturk$new.Race)
prop.table(table(Dur,mturk$new.Education),2)
table(Dur,mturk$new.Education)
prop.table(table(Dur,mturk$new.Income),2)
table(Dur,mturk$new.Income)
prop.table(table(Dur,mturk$new.MaritalStat),2)
table(Dur,mturk$new.MaritalStat)
prop.table(table(Dur,mturk$new.Gender),2)
table(Dur,mturk$new.Gender)
prop.table(table(Dur))
summary(Dur)

##Creating subset for mturk population, excluding those with duration violation
Att<-factor(mturk$Attention)
prop.table(table(Att,mturk$new.Age),2)
table(Att,mturk$new.Age)
prop.table(table(Att,mturk$new.Race),2)
table(Att,mturk$new.Race)
prop.table(table(Att,mturk$new.Education),2)
table(Att,mturk$new.Education)
prop.table(table(Att,mturk$new.Income),2)
table(Att,mturk$new.Income)
prop.table(table(Att,mturk$new.MaritalStat),2)
table(Att,mturk$new.MaritalStat)
prop.table(table(Att,mturk$new.Gender),2)
table(Att,mturk$new.Gender)
prop.table(table(Att))
summary(Att)

##Creating subset for mturk population, excluding those with duration violation
inctto <- subset(mturk,MturkTTOSlider == 2)
inc<-factor(inctto$Inconsistent)
prop.table(table(inc,inctto$new.Age),2)
table(inc,inctto$new.Age)
prop.table(table(inc,inctto$new.Race),2)
table(inc,inctto$new.Race)
prop.table(table(inc,inctto$new.Education),2)
table(inc,inctto$new.Education)
prop.table(table(inc,inctto$new.Income),2)
table(inc,inctto$new.Income)
prop.table(table(inc,inctto$new.MaritalStat),2)
table(inc,inctto$new.MaritalStat)
prop.table(table(inc,inctto$new.Gender),2)
table(inc,inctto$new.Gender)
prop.table(table(inc))
table(inc)

##Creating plot for Elimaniont through Duratoin and Attention for sociodemographicl variables.
##This plot was generated with the dataset found at: https://docs.google.com/spreadsheet/pub?key=0AjVEotAQPzQsdGtFRXR3QXY5NXFDTEFxLVk3eFZhb2c&single=true&gid=1&output=csv
qplot(subclass,data, data=feasability, fill=data, geom="bar") + 
  facet_grid(set~ . ~ class, scales="free_x", space="free") + 
xlab("") + ylab ("% of Sample Loss")+ theme(legend.position = "none") + theme_bw() +
theme(legend.position = "none",
axis.text.x  = element_text(angle=45, hjust=1.2))

##Creating boxplot graphs to compare WilltoLive for the sample without the eliminated
##and the full sample.
#Selecting subsett withtou subjects who missed UDration and Attention
mturkXAtt <- subset(mturk,Attention == 1)
mturkXAttANDDur <- subset(mturkXAtt,EliminDurate == 1)
studysample <- rbind(subset(mturkXAttANDDur,Inconsistent == 1 ),
                 subset(mturkXAttANDDur,MturkTTOSlider == 1 ))

excludedsample <- rbind(subset(mturk,Attention != 1),
                    subset(mturkXAtt,EliminDurate != 1),
                       subset(subset(mturkXAttANDDur,EliminDurate == 1),Inconsistent != 1))

excludedsample$source<-c("Excluded Sample") #Create vector for Excluded sample string
mturk$source<-c("Total Sample") #Create vector for Total sample strin
studysample$source<-c("Study Sample") #Create vector for Study sample string
Totalsample<-rbind(studysample,mturk,excludedsample) #Gather vectors in on vector to specify source of data that will feed the graph function
#call graph to plot descriptives for each sample group
qplot(Totalsample$source,Totalsample$WillToLive, geom=c("boxplot","jitter")) + 
xlab("Source") + ylab("Willingness To Live") + theme_bw()

#Compare groups
Totalsample$source<-as.factor(Totalsample$source)
kruskal.test(Totalsample$WillToLive ~ Totalsample$source, data=Totalsample)
kruskalmc(Totalsample$WillToLive ~ Totalsample$source, data=Totalsample)

#######################################################################################
#Figure 3. Comparison of Willingness to Live (median) indicators between Mturk and 
# traditional samples.
#######################################################################################
#Create subsets with different collection methods (TTOXSlider) in the two plataforms (Mturk or Hangout)
wtlttomturk <- subset(studysample, ttoXslider == "ttomt",
                  select=c(WillToLive))
ad.test(wtlttomturk$WillToLive) #Normality test
wtlhangmturk <- subset(pref, ttoXslider == "ttohang",
                      select=c(WillToLive))
ad.test(wtlhangmturk$WillToLive)#Normality test
wtlslidermturk <- subset(studysample, ttoXslider == "slidermt",
                      select=c(WillToLive))
ad.test(wtlslidermturk$WillToLive)#Normality test
wtlsliderhang <- subset(pref, ttoXslider == "slidehang",
                      select=c(WillToLive))
ad.test(wtlsliderhang$WillToLive)#Normality test

#Compare groups
tapply(WillToLive,ttoXslider,summary) #Also will give some regular descriptives by groups
kruskal.test(WillToLive ~ ttoXslider, data=studysampletotal)
kruskalmc(WillToLive ~ ttoXslider, data=studysampletotal)

#Create beanplot graph to compare groups
beanplot<-rbind(wtlhangmturk,wtlslidermturk,wtlsliderhang,wtlttomturk) #Create vectorwith all the data
beanplot(WillToLive ~ ttoXslider, data = beanplot, 
         overallline = "median", method="overplot", side="b",
         col = list("black", "white"), border = c("black", "black"),
         horizontal=TRUE, axes = TRUE, xlab="Willingness to Live",
         beanlinewd=5, ylab="Method of Estimation", names=c("Slider","TTO"))
#legend(locator(), bty="n",c("Mturk", "Traditional"),
#       fill = c("black", "white"))
#######################################################################################
#Figure 4. Comparison of Willingness to Live (median) indicators between sociodemographic
# variables for the Mturk sample.
#######################################################################################
str(studysample) #Get variabes characteristics
#Calculate Utilities values - Willingness to Live devided by the maximum time trade off option (30 years)
studysample$Preference<-studysample$WillToLive/30
studysample<-na.omit(studysample) #exclude missing values
#Get summary statistics by grouping variable
tapply(studysample$Preference,studysample$new.Age,summary) #Also will give some regular descriptives by groups
kruskal.test(studysample$Preference ~ studysample$new.Age, data=studysample) #Non Parametric Variance analysis
#kruskalmc(WillToLive ~ ttoXslider, data=pref)

tapply(studysample$Preference,studysample$new.Income,summary) #Also will give some regular descriptives by groups
kruskal.test(studysample$Preference ~ new.Income, data=studysample) #Non Parametric Variance analysis
kruskalmc(studysample$Preference ~ new.Income, data=studysample) #Post hoc pairwise comparisons

tapply(studysample$Preference,studysample$new.Education,summary) #Also will give some regular descriptives by groups
kruskal.test(studysample$Preference ~ new.Education, data=studysample) #Non Parametric Variance analysis
#kruskalmc(WillToLive ~ ttoXslider, data=pref)

tapply(studysample$Preference,studysample$new.Race,summary) #Also will give some regular descriptives by groups
kruskal.test(studysample$Preference ~ new.Race, data=studysample)  #Non Parametric Variance analysis
kruskalmc(studysample$Preference ~ new.Race, data=studysample)

tapply(studysample$Preference,studysample$new.MaritalStat,summary) #Also will give some regular descriptives by groups
kruskal.test(studysample$Preference ~ new.MaritalStat, data=studysample)  #Non Parametric Variance analysis
kruskalmc(studysample$Preference ~ new.MaritalStat, data=studysample)

tapply(studysample$Preference,studysample$new.Gender,summary) #Also will give some regular descriptives by groups
kruskal.test(studysample$Preference ~ new.Gender, data=studysample)  #Non Parametric Variance analysis
kruskalmc(studysample$Preference ~ new.Gender, data=studysample)

wilcox.test(studysample$Preference ~ Comorbidities, data=studysample) #Group comparison

#Creating Graphs objects
studysample$Age<-studysample$new.Age
age<-ggally_dotAndBox(studysample, aes(x = Preference, y = Age, col=new.Age), boxPlot=TRUE,
                      outlier.colour="red")  +
                       ylab("Willingness to Live") + theme_bw() + theme(legend.position = "none") +
  xlab("Age")
age

studysample$Education<-studysample$new.Education
edu<-ggally_dotAndBox(studysample, aes(x = Preference, y = Education, col=new.Education), boxPlot=TRUE,
                      outlier.colour="red")  +
                        xlab("Education") + ylab("Willingness to Live") + theme_bw() + 
  theme(legend.position = "none")
edu

studysample$Income<-studysample$new.Income
inc<-ggally_dotAndBox(studysample, aes(x = Preference, y = Income, col=new.Income), boxPlot=TRUE,
                      outlier.colour="red") + 
                    ylab("Willingness to Live") + theme_bw() + theme(legend.position = "none")
inc

studysample$MaritalStatus<-studysample$new.MaritalStat
ms<-ggally_dotAndBox(studysample, aes(x = Preference, y = MaritalStatus, col=new.MaritalStat), 
                     boxPlot=TRUE,
                      outlier.colour="red") + 
                  ylab("Willingness to Live") + theme_bw() + theme(legend.position = "none")
ms

studysample$Race<-studysample$new.Race
race<-ggally_dotAndBox(studysample, aes(x = Preference, y = Race, col=new.Race), boxPlot=TRUE,
                      outlier.colour="red") + 
                ylab("Willingness to Live")+ theme_bw() + theme(legend.position = "none")
race

studysample$Gender<-studysample$new.Gender
gender<-ggally_dotAndBox(studysample, aes(x = Preference, y = Gender, col=new.Gender), boxPlot=TRUE,
                       outlier.colour="red") + 
                ylab("Willingness to Live") + theme_bw() + theme(legend.position = "none")
gender

comorbidity<-ggally_dotAndBox(studysample, aes(x = Preference, y = Comorbidities, col=Comorbidities),
                              boxPlot=TRUE,outlier.colour="red") + 
  ylab("Willingness to Live") + theme_bw() + theme(legend.position = "none")
comorbidity

studysample$KneePreference<-c("Knee Preference")
total<-ggally_dotAndBox(studysample, aes(x = Preference, y = KneePreference, col=KneePreference),
                              boxPlot=TRUE,outlier.colour="red") + 
  ylab("Willingness to Live") + theme_bw() + theme(legend.position = "none")
total

#Arrange graphs in a grid
grid.arrange(age,edu,inc,ms,race,gender,comorbidity,total)

#######################################################################################
#PreferenceMturk.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################

