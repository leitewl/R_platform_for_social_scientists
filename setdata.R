urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
dataWBT=read.csv(urlfile)

#remove URL 
rm(urlfile)

#select the city of KAYSERI
# listwise deletion for gen_att and education variables
dataWBT_Kayseri=na.omit(dataWBT[dataWBT$city=="KAYSERI",c("id","gen_att","higher_ed","gender")])

# Higher education is coded as 0 and 1, change it to non-college, college 
dataWBT_Kayseri$HEF=droplevels(factor(dataWBT_Kayseri$higher_ed, 
                                      levels = c(0,1), 
                                      labels = c("non-college", "college")))

#table(dataWBT_Kayseri$gender)
#table(dataWBT_Kayseri$HEF)

#drop empty levels
dataWBT_Kayseri$gender=droplevels(dataWBT_Kayseri$gender)

head(dataWBT_Kayseri)
dataWBT_Kayseri$higher_ed=NULL

write.csv(dataWBT_Kayseri,"data101.csv")


urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/data101.csv'
data101=read.csv(urlfile)


rm(urlfile)

# from wide to long. Create one single item column based on item1 to item6
# while keeping other values in the data set 
library(tidyr)
data_long = gather(tempdata, item, score, item1:item6, factor_key=TRUE)

#sort data by id
data_long=data_long[order(data_long$id),] 

# from long  to wide.
data_wide = spread(data101, gender, gen_att)



# load csv from an online repository
urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
dataWBT=read.csv(urlfile)

#remove URL 
rm(urlfile)
dataWBT_USAK=dataWBT[dataWBT$city=="USAK",]


# We explained the functions 'factor' and 'droplevels' in section 5.2.4
# here we create a factor, Higher Education Factor (HEF). 
# it is labeled as 'non-college' when the higher_ed variable equals 0, 
# 'college' when equals to 1.
# if you dont use droplevels function, you might have an empty level 
dataWBT_USAK$HEF=droplevels(factor(dataWBT_USAK$higher_ed, 
                                   levels = c(0,1), 
                                   labels = c("non-college", "college")))

head(dataWBT_USAK)
dataWBT_USAK=dataWBT_USAK[,c("id","gen_att","HEF")]
write.csv(dataWBT_USAK,file="dataWBT_USAK.csv")

dataWBT_TRABZON=dataWBT[dataWBT$city=="TRABZON",]
t.test(gen_att~wage01,data=dataWBT_TRABZON,var.equal=T,
       alternative="two.sided",
       conf.level=0.95)
head(dataWBT_TRABZON)
dataWBT_TRABZON=dataWBT_TRABZON[,c("id","gen_att","wage01")]
write.csv(dataWBT_TRABZON,file="dataWBT_TRABZON.csv")

woundsREP=data.frame(ratid=1:22,                  
                     tape=c(7.47,10.60,4.73,6.66,5.37,5.72,7.56,8.64,7.37,6.79,
                            8.14,11.25,5.78,6.45,6.85,6.68,8.57,9.94,8.28,7.85,6.12,5.34), 
                     suture=c(5.34,6.64,5.64,8.89,4.31,3.97,3.32,6.36,6.73,6.33,
                              6.45,7.64,6.88,8.43,5.51,4.76,4.58,7.82,7.78,6.37,5.64,4.43))

library(tidyr)
plotdata=gather(woundsREP, method, strength, tape:suture, factor_key=TRUE)

require(ggplot2)
ggplot(plotdata, aes(x = strength)) +
  geom_histogram(aes(y = ..density..),col="black",alpha=0.7) + 
  geom_density(size=2) +
  theme_bw()+labs(x = "strength")+ facet_wrap(~ method)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))

library(psych)
with(woundsREP, t.test(tape,suture,paired=T,
                    alternative="two.sided",
                    conf.level=0.95))


# load csv from an online repository
urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
dataWBT=read.csv(urlfile)

#remove URL 
rm(urlfile)

#select the city of KOCAELI
# listwise deletion for gen_att and education variables
dataWBT_KOCAELI=na.omit(dataWBT[dataWBT$city=="KOCAELI",
                                c("id","gen_att","education")])


library(car)
dataWBT_KOCAELI$eduNEW <- recode(dataWBT_KOCAELI$education,
                                 "'None'='Primary School (5 years)'")

dataWBT_KOCAELI$eduNEW <- recode(dataWBT_KOCAELI$eduNEW,
                                 "'High School (Lycee)'=
                                 'High School (Lycee) (4 years)'")

dataWBT_KOCAELI$eduNEW <- recode(dataWBT_KOCAELI$eduNEW,
                                 "'Vocational School'=
                                 'Vocational High School (4 years)'")
#table(dataWBT_KOCAELI$eduNEW)

##optional re-order levels (cosmetic)
#levels(dataWBT_KOCAELI$eduNEW)
dataWBT_KOCAELI$eduNEW = factor(dataWBT_KOCAELI$eduNEW,
                         levels(dataWBT_KOCAELI$eduNEW)[c(4,3,1,6,2,5)])


#which(dataWBT_KOCAELI$education=="None")

#drop empty levels
dataWBT_KOCAELI$eduNEW=droplevels(dataWBT_KOCAELI$eduNEW)
dataWBT_KOCAELI$id=as.factor(dataWBT_KOCAELI$id)
dataWBT_KOCAELI=dataWBT_KOCAELI[,c("id","gen_att","eduNEW")]
head(dataWBT_KOCAELI)
write.csv(dataWBT_KOCAELI,file="dataWBT_KOCAELI.csv")


dataWBT_Kayseri=na.omit(dataWBT[dataWBT$city=="KAYSERI",c("id","gen_att","higher_ed","gender")])

# Higher education is coded as 0 and 1, change it to non-college, college 
dataWBT_Kayseri$HEF=droplevels(factor(dataWBT_Kayseri$higher_ed, 
                                      levels = c(0,1), 
                                      labels = c("non-college", "college")))

#table(dataWBT_Kayseri$gender)
#table(dataWBT_Kayseri$HEF)

#drop empty levels
dataWBT_Kayseri$gender=droplevels(dataWBT_Kayseri$gender)
head(dataWBT_Kayseri)
write.csv(dataWBT_Kayseri,file="dataWBT_Kayseri.csv")
urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT_KOCAELI.csv'
dataWBT_sil=read.csv(urlfile)


#oneway between anova exercise data
# load csv from an online repository
urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
dataWBT=read.csv(urlfile)

#remove URL 
rm(urlfile)

#select the city of KOCAELI
# listwise deletion for gen_att and education variables
dataWBT_HATAY=na.omit(dataWBT[dataWBT$city=="HATAY",
                                c("id","gen_att","education")])


library(car)
dataWBT_HATAY$eduNEW <- recode(dataWBT_HATAY$education,
                                 "'None'='Primary School (5 years)'")

dataWBT_HATAY$eduNEW <- recode(dataWBT_HATAY$eduNEW,
                               "'University -Masters degree or more'='University - Undergraduate degree'")

dataWBT_HATAY$eduNEW <- recode(dataWBT_HATAY$eduNEW,
                                 "'High School (Lycee)'=
                                 'High School (Lycee) (4 years)'")

dataWBT_HATAY$eduNEW <- recode(dataWBT_HATAY$eduNEW,
                                 "'Vocational School'=
                                 'Vocational High School (4 years)'")

##optional re-order levels (cosmetic)
#levels(dataWBT_HATAY$eduNEW)
dataWBT_HATAY$eduNEW = factor(dataWBT_HATAY$eduNEW,
                                levels(dataWBT_HATAY$eduNEW)[c(4,3,1,6,2,5)])


#which(dataWBT_KOCAELI$education=="None")

#drop empty levels
dataWBT_HATAY$eduNEW=droplevels(dataWBT_HATAY$eduNEW)
dataWBT_HATAY$id=as.factor(dataWBT_HATAY$id)
dataWBT_HATAY=dataWBT_HATAY[,c("id","gen_att","eduNEW")]
head(dataWBT_HATAY)

library(psych)
desc1BW=data.frame(with(dataWBT_HATAY,
                        describeBy(gen_att, eduNEW,mat=T,digits = 2)),
                   row.names=NULL)

require(ggplot2)
ggplot(dataWBT_HATAY, aes(x = gen_att)) +
  geom_histogram(aes(y = ..density..),col="black",binwidth = 0.2,alpha=0.7) + 
  geom_density(size=1.5) +
  theme_bw()+labs(x = "Gender Attitude by Degree in Hatay")+ facet_wrap(~ eduNEW)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))

require(ggplot2)
ggplot(dataWBT_HATAY, aes(eduNEW,gen_att)) +
  geom_boxplot() + 
  labs(x = "Education",y="Gender Attitude by degree in HATAY")+coord_flip()


dataWBT_HATAY$id=as.factor(dataWBT_HATAY$id)
require(ez)
alternative1 = ezANOVA(data = dataWBT_HATAY,wid=id, dv = gen_att, between = eduNEW,observed=eduNEW)





write.csv(dataWBT_HATAY,file="dataWBT_HATAY.csv")


dataWBT_ESKISEHIR=na.omit(dataWBT[dataWBT$city=="ESKISEHIR",c("id","gen_att","higher_ed","gender")])

# Higher education is coded as 0 and 1, change it to non-college, college 
dataWBT_ESKISEHIR$HEF=droplevels(factor(dataWBT_ESKISEHIR$higher_ed, 
                                        levels = c(0,1), 
                                        labels = c("non-college", "college")))

#table(dataWBT_ESKISEHIR$gender)
#table(dataWBT_ESKISEHIR$HEF)

#drop empty levels
dataWBT_ESKISEHIR$gender=droplevels(dataWBT_ESKISEHIR$gender)
head(dataWBT_ESKISEHIR)

dataWBT_ESKISEHIR$id=as.factor(dataWBT_ESKISEHIR$id)

#alternative 1 the ezANOVA function
alternative1 = ezANOVA(
  data = dataWBT_ESKISEHIR,
  wid=id, dv = gen_att, between = .(HEF,gender),observed=.(HEF,gender),type=2)



write.csv(dataWBT_ESKISEHIR,file="dataWBT_ESKISEHIR.csv")



PSdata=data.frame(id=factor(1:17),
                  wave1=c(20,19,13,10,16,12,16,11,11,14,13,17,16,12,12,16,16),
                  wave2=c(28,27,18,17,29,18,26,21,15,26,28,23,29,18,26,21,22),
                  wave3=c(21,24,14,8,23,15,21,15,12,21,23,17,26,18,14,18,19))

PSdata$wave1=PSdata$wave1+sample(1:5,size=17,replace = T)
PSdata$wave2=PSdata$wave2+sample(1:9,size=17,replace = T)
PSdata$wave3=PSdata$wave3+sample(1:12,size=17,replace = T)
library(tidyr)
data_long = gather(PSdata, wave, PrbSol, wave1:wave3, factor_key=TRUE)

#get descriptives 
library(doBy)
library(moments)
desc1W=as.matrix(summaryBy(PrbSol~wave, data = data_long, 
                           FUN = function(x) { c(n = sum(!is.na(x)),
                                                 mean = mean(x,na.rm=T), sdv = sd(x,na.rm=T),
                                                 skw=moments::skewness(x,na.rm=T),                    
                                                 krt=moments::kurtosis(x,na.rm=T)) } ))

ggplot(data_long, aes(x=wave, y=PrbSol))+
  geom_boxplot()+
  labs(x = "Wave",y="Problem Solving Knowledge scores")

require(ggplot2)
ggplot(data_long, aes(x=wave, y=PrbSol, group=id))+
  geom_line() + labs(x = "Wave",y="Problem Solving Knowledge scores")

alternative1 = ezANOVA(
  data = data_long,
  wid=id, dv = PrbSol, within = wave,
  detailed = T,return_aov=T)

alternative1


#CORR

# load csv from an online repository
urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
dataWBT=read.csv(urlfile)

#remove URL 
rm(urlfile)

#select the city of Bayburt
# listwise deletion for gen_att and education variables
dataWBT_DIYARBAKIR=dataWBT[dataWBT$city=="DIYARBAKIR",]
#hist(dataWBT_DIYARBAKIR$income_per_member)
dataWBT_DIYARBAKIR$binitem1=ifelse(dataWBT_DIYARBAKIR$item1==4,1,0)
dataWBT_DIYARBAKIR=dataWBT_DIYARBAKIR[,c("id","gender","gen_att","income_per_member","binitem1","wage01")]
head(dataWBT_DIYARBAKIR)
dataWBT_DIYARBAKIR$gender=droplevels(dataWBT_DIYARBAKIR$gender)
str(dataWBT_DIYARBAKIR)


require(psych)
with(dataWBT_DIYARBAKIR,biserial(gen_att,binitem1))


with(dataWBT_DIYARBAKIR,cor(gen_att,income_per_member,
                         use = "complete.obs",method="pearson"))
## [1] 0.0664

with(dataWBT_DIYARBAKIR,cor.test(gen_att,income_per_member,
                              alternative = "two.sided",
                              method="pearson",
                              conf.level = 0.95,
                              na.action="na.omit"))   
library(WRS2)
pbcor(dataWBT_DIYARBAKIR$gen_att,dataWBT_DIYARBAKIR$income_per_member,beta=.2)
## Call:

top=quantile(dataWBT_DIYARBAKIR$income_per_member,.95)
dataWBT_DIYARBAKIR$incomeTC=ifelse(dataWBT_DIYARBAKIR$income_per_member>top,
                                   top,dataWBT_DIYARBAKIR$income_per_member)
#transform
dataWBT_DIYARBAKIR$incomeTC=log(dataWBT_DIYARBAKIR$incomeTC+
                                sqrt((dataWBT_DIYARBAKIR$incomeTC^2)+1))

with(dataWBT_DIYARBAKIR,cor.test(gen_att,incomeTC,
                               alternative = "two.sided",
                               method="pearson",
                               conf.level = 0.95,
                               na.action="na.omit"))  

with(dataWBT_DIYARBAKIR,cor.test(gen_att,income_per_member,
                              alternative = "two.sided",
                              method="spearman",
                              conf.level = 0.95,
                              na.action="na.omit",
                              exact=FALSE)) 

with(dataWBT_DIYARBAKIR,cor.test(gen_att,income_per_member,
                              alternative = "two.sided",
                              method="kendall",
                              conf.level = 0.95,
                              na.action="na.omit",
                              exact=FALSE)) 


dataWBT_DIYARBAKIR$genderNUM=ifelse(dataWBT_DIYARBAKIR$gender=="Female",1,0)
with(dataWBT_DIYARBAKIR,cor.test(gen_att,genderNUM,
                              alternative = "two.sided",
                              method="pearson",
                              conf.level = 0.95,
                              na.action="na.omit"))   

table(dataWBT_DIYARBAKIR$gender,dataWBT_DIYARBAKIR$wage01)
##          
##           No Yes
##   Female  52  97
##   Male    49  54
##   Unknown  0   0

genderWAGE=matrix(c(26,22,23,23),ncol=2)
library(psych)
phi(genderWAGE)
write.csv(dataWBT_DIYARBAKIR,file="dataWBT_DIYARBAKIR.csv")
head(dataWBT_DIYARBAKIR)
