1+1
1-1
1 + (2 / 3) - (2 * 6.5)
sin(30) + 4^3 + log(4) + exp(3) + sqrt(7)
a=1 - 1
b=1 + 1
c=1 + (2 / 3) - (2 * 6.5)
d=sin(30) + 4^3 + log(4) + exp(3) + sqrt(7)
a+b+c+d

e=3+2
e
e=e+10

Equation1_output=a
Equation1_output + b + c + d  


sabit5=function(girdi){
  cikti=girdi+5
  return(cikti)
}
sabit5(girdi=50)

sabit5(60)
sabit5(80)


sistematik1=function(input){
  output=input+(input/100)
  return(output)
}

sistematik1(input=50)

sistematik1(100)

sistematik1(120)


eksipuan=function(puan, yanlis){
  cikti=puan - (0.2 * yanlis)
  return(cikti)
}
eksipuan(puan=90,yanlis=6)

eksipuan(90,17)


geridonut=function(dogruyanit, katsayi){
  total=dogruyanit*katsayi
  kalan=(100-total)/katsayi
  cikti=c(paste("Puan:", total," eksik:",kalan))
  return(cikti)
}
geridonut(dogruyanit=20,katsayi=2)
## [1] "Puan: 40  eksik: 30"
geridonut(27,2)
## [1] "Puan: 54  eksik: 23"



sabit5=function(girdi=1000){                     	       
  cikti=girdi+5                     			               
  return(cikti)                     			                  
}									                                               
sabit5()								                                            
## [1] 1005	




geridonut=function(dogruyanit, katsayi){
  total=dogruyanit*katsayi
  kalan=(100-total)/katsayi
  cikti=c(paste("Puan:", total," eksik:",kalan))
  return(cikti)
}

geridonut(dogruyanit=20)

eksipuan=function(puan, yanlis){
  cikti=puan - (0.2 * yanlis)
  return(cikti)
}
eksipuan(puan=50,yanlis=10)



eksipuan2=function(puan, yanlis){
  cikti=puan - (0.2 * yanlis)
  if (cikti<0) 
    warning("Yeni puan 0'dan düşük")  
  return(cikti)
}
eksipuan2(puan=10,yanlis=60)


eksipuan3=function(puan, yanlis){
  
  if ((puan)<(20)) 
    stop("20den düşük puanlar için bu fonksiyon işlemez")
  
  cikti=puan - (0.2 * yanlis)
  return(cikti)
}
eksipuan3(10,9)

help("base")  # 
help(mean)    # aritmetik ortalama fonksiyonu ve argümanları
?mean         # aritmetik ortalama fonksiyonu ve argümanları
??mean        # aritmetik ortalama fonksiyonu ve argümanları
example(mean) # aritmetik ortalama fonksiyonu ve argümanları


notlar=c(40,50,53,65,72,77,79,81,86,90)
notlar



notlar=c(40,50,53,65,72,77,79,81,86,90)
#her nota 10 ekle
notlar+10
##  [1]  50  60  63  75  82  87  89  91  96 100
#her nota yüzde 10 ekle
notlar+(notlar*0.10)
##  [1] 44.0 55.0 58.3 71.5 79.2 84.7 86.9 89.1 94.6 99.0
#kendi ile çarp
notlar*notlar
##  [1] 1600 2500 2809 4225 5184 5929 6241 6561 7396 8100
# yeni notlar
notlar2=c(30,40,46,58,64,66,69,72,74,81)


# notlar ve notlar2 nin ortalamasını al
(notlar+notlar2)/2
##  [1] 35.0 45.0 49.5 61.5 68.0 71.5 74.0 76.5 80.0 85.5

# ilk notların yüzde 40ı ile ikinci notların yüzde 60ını topla
notlar*0.4 + notlar2*0.6
##  [1] 34.0 44.0 48.8 60.8 67.2 70.4 73.0 75.6 78.8 84.6


a=1:12            # a 1 den 12ye tam sayılar
rep(0,12)         # 0 12 kez tekrarlanır
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0
rep(1:5,each=3)   # 1 den 5'e tam sayılar 3er kez tekrarlanır
##  [1] 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5
rep(1:5,times=3)  # 3 kere 1'den 5'e tekrarla
##  [1] 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5
seq(from=1,to=12) # 1'den 12'ye tam sayılar
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12
seq(1,25,by=2)    # 1'den 25'e ikişer atla
##  [1]  1  3  5  7  9 11 13 15 17 19 21 23 25
seq(1,6,by=0.5)   # 1'den 6'ya 0.5 atla
##  [1] 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5 6.0
rnorm(12)        #  ~N(0,1) 12 gözlem
##  [1] -0.7835716 -0.9407123 -0.3829866 -2.1404926  0.1053887  0.5326148
##  [7]  1.5451630  0.2785917 -0.8777137  0.7329613  1.3189034  1.9611107
rnorm(12,mean=10,sd=2) #~ N(10,2) 12 gözlem
##  [1] 12.519010 11.703584  8.690880 12.152435 11.661208 10.200169 13.614309
##  [8]  8.090524 10.948593  7.161299  5.216387  8.637427
runif(12, min = 10, max = 37) 

A=matrix(1:16,ncol=4,nrow=4) #4x4 matris oluştur
A

B=matrix(runif(16,min=20,max=40),ncol=4) #4x4 matris oluştur
A+B
A*B    # çarp
A%*%B  # matris çarp
t(B)   # çevir

adres=c("AAX","BBZ","CBT","DBA","DDC","XZT")
cinsiyet=c("M","F","F","M","F","M")
id=sample(letters,6)
program=rep(c("var","yok"),each=3)
sehir=as.character(1:6)


soru1=ordered(c("zayif","orta","iyi","iyi","zayif","zayif"),
              levels=c("zayif","orta","iyi"))
ses=ordered(c(1,3,2,2,1,3),levels=c("1","2","3"))


notlar=c(52,75,39,62,24,86)
notlar=rnorm(n=6,mean=160,sd=5)

dt=as.Date(c("1994-06-01","1988-10-20","1990-12-01",
             "1978-03-23","1974-08-22","1994-11-04"))

dt

tatil=as.Date(c("01/01/2016","04/23/2016","05/19/2016","08/30/2016","09/29/2016"),format="%m/%d/%y")

tatil

Sys.Date( )
Sys.Date( )-dt



notlar=c(52,75,39,62,24,86)    # notlar
notlar>mean(notlar)            
## [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE
as.numeric(notlar>mean(notlar)) 

kurs=factor(c("muhasebe","garson","temizlik","garson","muhasebe","garson"))
ga1=factor(c(1,1,3,4,2,3),levels = 1:4,
           labels=c("tamamenkatilmiyorum","katilmiyorum","katiliyorum","tamamenkatiliyorum"))
ga2=factor(c(1,3,4,4,2,3),ordered = T)
ga3=gl(n=3,k=2,labels=c("A","B","C"),ordered=F)



renk=factor(c(1,1,1,2,2,3),levels = 1:3,labels=c("mavi","yesil","sari"))
renk
renk2=renk[1:5]  # renk2 değişkeni son degeri almadı
renk2  #fakat hala 3 level mevcut
droplevels(renk2) #kullanılmayan level silindi

gelir=c("maas","maas","destek",NA,NA,"maas")
hanekisi=c(3,2,3,NA,NA,4)

ornek = factor(c('maas','destek', NA, 'NA'," ",-99,"-99"))
is.na(ornek) 
ornek[ornek=='NA' | ornek==" "| ornek== -99 | ornek== "-99"]=NA

is.na(ornek)
ornek=droplevels(ornek)

#parantez içinde yer alan değişkenler yukarıda tanımlanmıştır
basit_data=data.frame(id,program,cinsiyet,soru1,ses,
                      notlar,gelir,dt,kurs)
basit_data
str(basit_data)




###########################
###########################
######  BOLUM 2   ########
###########################
###########################

data1=read.csv("dataismi.csv") 

load("dataismi.Rdata")  

require(foreign)
?read.spss
data=read.spss("dataismi.sav",to.data.frame=TRUE)   
rm(data)


#sanal depodan CSV oku
urldosyasi='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
dataismi=read.csv(urldosyasi)
str(dataismi)


#sanal depodan Rdata oku
urldosyasi2='https://github.com/burakaydin/materyaller/blob/gh-pages/ARPASS/dataWBT.Rdata?raw=true'
load(url(urldosyasi2))
str(dataWBT)



# sanal depodan CSV oku
urldosya='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
veriseti1=read.csv(urldosya)

#URL adresini sil
rm(urldosya)


# 151. satır 16. sütunu değiştir ve 30 yap
veriseti1[151,16]=30

# aynı işlem satır ismi ve sütün ismi verilerek yapılabilir.
# id numarası 67034022 olan satırın yaş değerini 32 yap. 
veriseti1[veriseti1$id==67034022,"age"]=32

# tekrar kodlama
# Veri setinde yer alan treatment değişkeni 0 ve 1 olarak girilmiştir.
# 1leri  "trt" ve 2leri  "cnt" yapmak için 
veriseti1[veriseti1$treatment==1,"treatment"]="trt"
veriseti1[veriseti1$treatment==2,"treatment"]="cnt"

# ifelse fonksiyonu benzer şekilde çalışır
# "wage01" değişkeninde "wage01"  "Yes" ise 0.5, değil ise -0.5 olarak kodlayalım
veriseti1$wage01=ifelse(veriseti1$wage01=="Yes",0.5,-0.5)

# plyr paketi kullanarak
require(plyr)
# pension01yeni değişkeni pension01 değişkeni üzeriden tanımlanmıştır
# eski değerler olan Yes ve No yerine 1 ve 0 kodlayalım
veriseti1$pension01yeni <- mapvalues(veriseti1$pension01, 
                                     from=c("Yes","No"),to=c("1","0"))
#bir değişkene yeni isim verelim
#4. ve 5. değişkenlere yeni isim verelim
colnames(veriseti1)[4]="kurs"
colnames(veriseti1)[5]="bolge"

#isim verme işlemini tek sıra kod ile yapalım
colnames(veriseti1)[c(17,21)]=c("Tgelir","maas1")

#plyr paketini kullanalım gen_att değişkenine toplumsalCinsiyet ismi verelim
veriseti1 <- rename(veriseti1,c('gen_att'='toplumsalCinsiyet'))

#kontrol etmek için head(veriseti) veya summary(veriseti) kullanılabilir

# veriseti1'i çalışma alanından sil
rm(veriseti1)


# CSV yükle 
#urldosya='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
#veriseti1=read.csv(urldosya)

# URL adresi sil 
#rm(urldosya)


veriseti1=read.csv("dataWBT.csv")
# sadece İstanbul'u seç
istDAT=veriseti1[veriseti1$city=="ISTANBUL",]

# sadece İstanbul'dan ilk sekiz katılımcıyı seç
istDAT18=veriseti1[veriseti1$city=="ISTANBUL",1:8]

# sadece İstanbul'dan gen_att puanı 2den yüksek olanları seç
istDATGAT2=veriseti1[veriseti1$city=="ISTANBUL" | veriseti1$gen_att >2 ,]

# subset fonksiyonu
# sadece İstanbul'dan gen_att puanı 2den yüksek olan ilk sekiz katılımcıyı seç
istDATGAT2B=subset(veriseti1, city=="ISTANBUL" | veriseti1$gen_att >2, select=1:8) 

#item 1 değeri 1,2 ve 3 olan katılımcıları seç
item1_123 <- veriseti1[veriseti1$item1 %in% c(1,2,3), ]

#çalışma alanını temizle
rm(list=ls())


# CSV yükle
#urldosya='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
#veriseti1=read.csv(urldosya)

#URL dosyası yükle 
#rm(urldosya)

veriseti1=read.csv("dataWBT.csv")

# item2'den 6'ya kadar olan sütünları topla
veriseti1$itemTOPLAM=with(veriseti1,item2+item3+item4+item5+item6)

# item2'den 6'ya kadar olan sütünların ortalamasını al (na.rm =T önemli)
veriseti1$itemAVE=with(veriseti1,
                       rowMeans(cbind(item2,item3,item4,item5,item6),na.rm=T))

#veya rowMeans fonksiyonu
veriseti1$itemAVE=rowMeans(veriseti1[,10:14],na.rm = T)

# Şehirler için ortalama hesapla
veriseti1$CityAVEscore =with(veriseti1, ave(itemAVE,city,FUN=function(x) mean(x, na.rm=T)))

#veya
veriseti1=merge(veriseti1, aggregate(itemAVE ~ city, data = veriseti1, FUN=mean, na.rm=TRUE), by = "city", suffixes = c("", "citymean"),all=T)

#veya her bir soru için şehir ortalaması hesapla
veriseti1=merge(veriseti1, aggregate(cbind(item2,item3,item4,item5,item6) ~ city, data = veriseti1, FUN=mean, na.rm=TRUE), by = "city", suffixes = c("", "Citymean"),all=T)

# değişkenlerin kategorize edilmesi. Eğer item1AVE 2'den küçük ise 0 aksi halde 1
veriseti1$itemAVE01=ifelse(veriseti1$itemAVE<2,0,1)

# 0 ile 1.8 arasına 1 ver
# 1.8 ve 2.5 arasına 2 ver
# 2.5 ile 5 arasına 3 ver
veriseti1$itemAVE123=with(veriseti1,cut(itemAVE, breaks=c(0,1.8,2.5,5), labels = FALSE))
# cut fonksiyonu içerisinde yer alan right=T argümanına göz gezdirin
# örneğin  right=T  ise değeri tam olarak 1.8 olan değişkenler 1 olur
#          right=F  ise değeri tam olarak 1.8 olan değişkenler 2 olur




# CSV yükle
#urldosya='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
#veriseti1=read.csv(urldosya)
#adresi sil
#rm(urldosya)


veriseti1=read.csv("dataWBT.csv")
#genişten uzuna item 1den 6ya kadar olan sütünları item adı altında birleştir
library(tidyr)
data_long = gather(veriseti1, item, score, item1:item6, factor_key=TRUE)

#id'ye göre diz
data_long=data_long[order(data_long$id),] 
# uzundan genişe.
data_wide = spread(data_long, item, score)
## belirlediğiniz nesneler dışında çalışma alanını temizle
rm(list=setdiff(ls(),c("veriseti1")))


# CSV yükle
#urldosya='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
#veriseti1=read.csv(urldosya,stringsAsFactors = F)
#URL sil
#rm(urldosya)

veriseti1=read.csv("dataWBT.csv",stringsAsFactors = F)
#treatment değişkenini incele
str(veriseti1$treatment)
#sayısal veriyi faktöre çevir
veriseti1$treatmentFactor=factor(veriseti1$treatment,                                                                           					    labels=c("treatment","control"))
#karakter olarak girildiğinde faktörleri sayıya çevirme
veriseti1$iv1=factor(rep(c("1","2","3"),length=nrow(veriseti1)))
veriseti1$iv1numeric=as.numeric(levels(veriseti1$iv1))[veriseti1$iv1]
#veya
veriseti1$iv1numeric=as.numeric(as.character(veriseti1$iv1))

#NAleri -99'a çevir  
veriseti1[is.na(veriseti1)]= (-99)

#çalışma alanını temizle
rm(list=ls())


# CSV yükle
#urldosya='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
#veriseti1=read.csv(urldosya,stringsAsFactors = F)

#URL sil 
#rm(urldosya)
veriseti1=read.csv("dataWBT.csv",stringsAsFactors = F)

#nesne oluştur.
subset1=veriseti1[1:20,1:5]
object2=mean(veriseti1$item1,na.rm = T)


#çalışma klasörünüzü kontrol edin
getwd()

# Rdata olarak sakla
save(subset1,file="subset1Rfile.Rdata")
# adres vererek sakla
# save(object2,file="C:/Users/Desktop/object2Rfile.Rdata")

# csv olarak sakla
write.csv(subset1,file="subset1CSVfile.csv",row.names = F)

#sps dosyası olarak sakla
library(foreign)
write.foreign(subset1, "subset1SPSfile.txt","subset1SPSfile.sps", package="SPSS")

#çalışma alanını temizle
rm(list=ls())

# load csv from an online repository
#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/data101.csv'
#data101=read.csv(urlfile)

data101=read.csv("data101.csv")

###########################
###########################
######  BOLUM 3   ########
###########################
###########################


# load csv from an online repository					 		
#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/data101.csv'	
#data101=read.csv(urlfile)									

#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
#dataWBT=read.csv(urlfile)

data101=read.csv("data101.csv")	
dataWBT=read.csv("dataWBT.csv")


mean(dataWBT$gen_att,na.rm = T)
## [1] 1.940576

# birden fazla değişkenin ortalamasını hesapla
# ?colMeans
colMeans(dataWBT[,c("gen_att","item1")],na.rm = T)
##  gen_att    item1 
## 1.940576 3.451014


median(dataWBT$gen_att,na.rm = T)

var(dataWBT$gen_att,na.rm = T)

sd(dataWBT$gen_att,na.rm = T)
library(moments)
skewness(dataWBT$gen_att,na.rm= T)
#basıklık hesapla
library(moments)
kurtosis(dataWBT$gen_att,na.rm = T)
## [1] 2.903905


library(psych)
desc1=describe(dataWBT[,c("gen_att","age")],trim = 0.05,type=3)
desc1


#write.csv(desc1,file="pscyhbetimsel.csv")

#doBy
# program değişkenine göre betimleyici istatistikler
library(doBy)
library(moments)
desc2=as.matrix(summaryBy(gen_att+age~treatment, data = dataWBT, 
                          FUN = function(x) { c(n = sum(!is.na(x)), nmis=sum(is.na(x)),
                                                m = mean(x,na.rm=T), s = sd(x,na.rm=T),
                                                skw=moments::skewness(x,na.rm=T),                    
                                                krt=moments::kurtosis(x,na.rm=T)) } ))

#yuvarlama
round(desc2,2)


library(apaStyle)
apa.descriptives(data = dataWBT[,c("gen_att","age")], 
                 variables =c("Gender Attitude","Age"), report = c("M","SD"), 
                 title = "APAtableGenderAge", filename = "APAtableGenderAge.docx", 
                 note = NULL, position = "lower", merge = FALSE, 
                 landscape = FALSE, save = TRUE)


library(ggplot2)
ggplot(dataWBT, aes(x = gen_att)) +geom_histogram(binwidth=0.2)+                            		theme_bw()+labs(x="Toplumsal Cinsiyet Algısı ")+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))



dataWBT$HEF=droplevels(factor(dataWBT$higher_ed, 
                              levels = c(0,1), 
                              labels = c("Lise ve altı", "Üniversite")))

ggplot(dataWBT, aes(x = gen_att, fill=HEF,drop=T)) +
  geom_histogram(breaks=seq(1, 4, by =0.2),alpha=.5,col="black")+ 
  theme_bw()+labs(x="Toplumsal Cinsiyet Algısı",fill='Yüksek Öğretim Durumu')+  
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))

dataWBT2=na.omit(dataWBT[,c("gen_att","HEF")])
ggplot(dataWBT2, aes(x = gen_att)) +
  geom_histogram(breaks=seq(1, 4, by =0.2),alpha=.5,col="black")+ 
  theme_bw()+labs(x = "Toplumsal Cinsiyet Algısı")+ facet_wrap(~ HEF)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))


library(ggplot2)
ggplot(dataWBT, aes(x = gen_att)) +
  geom_histogram(binwidth = 0.2)+ theme_bw()+facet_wrap(~city, ncol = 8)

dataWBT2=na.omit(dataWBT[,c("gen_att","HEF","gender")])
ggplot(dataWBT2, aes(x = gen_att,fill=gender)) +labs(fill='Gender')+ 
  geom_histogram(binwidth = 0.2,alpha=.5)+ theme_bw()+
  facet_grid(~HEF)


dataWBT$Condition=droplevels(factor(dataWBT$treatment, 
                                    levels = c(1,2), 
                                    labels = c("treatment", "control")))

dataWBT2=na.omit(dataWBT[,c("gen_att","HEF","gender","Condition")])

ggplot(dataWBT2, aes(x = gen_att,fill=gender)) +labs(fill='Gender')+ 
  geom_histogram(binwidth = 0.2,alpha=.5)+ theme_bw()+
  facet_grid(~HEF+Condition)


# gözlem sayısı, n
GA_n=sum(!is.na(dataWBT$gen_att))

#ortalama
GA_m=mean(dataWBT$gen_att,na.rm = T)
#ss
GA_s=sd(dataWBT$gen_att,na.rm = T)
#95% güven aralığı
alt=GA_m - 1.96 * (GA_s/sqrt(GA_n))
alt
## [1] 1.924339
ust=GA_m + 1.96 * (GA_s/sqrt(GA_n))
ust
## [1] 1.956812
#veya 
GA_m +c(-1,1)*1.96 * (GA_s/sqrt(GA_n))
## [1] 1.924339 1.956812

# z puanı hesapla

GA_m=mean(dataWBT$gen_att,na.rm = T)
GA_s=sd(dataWBT$gen_att,na.rm = T)
z_GA=(dataWBT$gen_att-GA_m)/GA_s

#veya
z_GA=scale(dataWBT$gen_att, center=T, scale=T)


#  n
GA_n=sum(!is.na(dataWBT$gen_att))
#ortalama
GA_m=mean(dataWBT$gen_att,na.rm = T)
#ss
GA_s=sd(dataWBT$gen_att,na.rm = T)
# boş hipotez
mu_hyp=2

# z istatistiği
(GA_m-mu_hyp)/(GA_s/sqrt(GA_n))
## [1] -7.17343

#alpha=0.05 ve yönsüz alternatif için kritik değer
qnorm(1-(0.05/2))
## [1] 1.959964

# boş hipotez
mu_hyp=1.9
# z istatistik
(GA_m-mu_hyp)/(0.75/sqrt(GA_n))
## [1] 3.939368
#yönlü alternatif ve alfa=.01
qnorm(1-(0.01))
## [1] 2.326348

dataWBT_DUZCE=dataWBT[dataWBT$city=="DUZCE",]
#betimleyici
describe(dataWBT_DUZCE[,"gen_att"],type=3)
##    vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## X1    1 47 2.18 0.55      2    2.14 0.59   1 3.8   2.8 0.56     0.28 0.08

#t test
t.test(dataWBT_DUZCE$gen_att,
       alternative="two.sided",
       mu=1.94,
       conf.level = 0.95)
#kritik değer
qt(.975,df=46)


t.test(dataWBT_DUZCE$gen_att,
       alternative="less",
       mu=1.94,
       conf.level = 0.95)
## 
##  One Sample t-test
## 
## data:  dataWBT_DUZCE$gen_att
## t = 2.9391, df = 46, p-value = 0.9974
## alternative hypothesis: true mean is less than 1.94
## 95 percent confidence interval:
##      -Inf 2.310055
## sample estimates:
## mean of x 
##  2.175532

#kritik  değer
qt(.05,df=46)


1-pnorm(1.8)

2*(1-pnorm(1.8))
1-pnorm(qnorm(0.95),mean=2.5)

#power.t.test
power.t.test(delta=.1, sd=.6,sig.level=0.05, power=0.9, 
             type="one.sample", alternative="one.sided")

#ikinci tür bootstrap t metodu
# Düzce katılımcılarını seç
dataWBT_DUZCE=na.omit(dataWBT[dataWBT$city=="DUZCE",c("id","gen_att")])
# normallik varsayımı ve t test kullanarak
# evren ortalamasının 1.94 olup olmadığını sına
t.test(dataWBT_DUZCE$gen_att,mu=1.94,conf.level = 0.95)
## 
##  One Sample t-test
## 
## data:  dataWBT_DUZCE$gen_att
## t = 2.9391, df = 46, p-value = 0.005133
## alternative hypothesis: true mean is not equal to 1.94
## 95 percent confidence interval:
##  2.014224 2.336840
## sample estimates:
## mean of x 
##  2.175532
# bootstrap ile 95% GA  (normallik varsayımı yok)
set.seed(04012017)
B=5000       # bootstrap sayısı
alpha=0.05   # alfa

#x değişken
# xBAR gözlemlenen ortalama
tstar=function(x,xBAR) sqrt(length(x))*abs(mean(x)-xBAR)/sd(x)
output=c()
for (i in 1:B){
  output[i]=tstar(sample(dataWBT_DUZCE$gen_att,
                         replace=T,
                         size=length(dataWBT_DUZCE$gen_att)),
                  xBAR=mean(dataWBT_DUZCE$gen_att))  }
output=sort(output)
Tc=output[as.integer(B*(1-alpha))]

#bootstrap GA
mean(dataWBT_DUZCE$gen_att)+c(-1,1)*(Tc*sd(dataWBT_DUZCE$gen_att)/sqrt(length(dataWBT_DUZCE$gen_att)))
## [1] 2.011540 2.339524


###########################
###########################
######  BOLUM 4   ########
###########################
###########################

#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT_USAK.csv'
#dataWBT_USAK=read.csv(urlfile)
#remove URL 
#rm(urlfile)


#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
#dataWBT=read.csv(urlfile)
#rm(urlfile)

dataWBT_USAK=read.csv("dataWBT_USAK.csv")

dataWBT=read.csv("dataWBT.csv")
dataWBT_USAK=dataWBT[dataWBT$city=="USAK",]

# factor ve droplevels fonksiyonları bölüm 5.2.4 ile verilmiştir.
# yeni oluşturulan HEF (Higher Education Factor)
# katılımcı lise veya altı diplomaya sahipse 0, non-college
# katılımcı lise üstü diplomaya sahip ise 1,  college
dataWBT_USAK$HEF=droplevels(factor(dataWBT_USAK$higher_ed, 
                                   levels = c(0,1), 
                                   labels = c("non-college", "college")))

require(ggplot2)
plotdata=na.omit(dataWBT_USAK[,c("gen_att","HEF")])
ggplot(plotdata, aes(x = gen_att)) +
  geom_histogram(aes(y = ..density..),col="black",binwidth = 0.2,alpha=0.7) + 
  geom_density(size=2) +
  theme_bw()+labs(x = "Uşak ilinde Yüksek Öğretim Durumuna göre TCA puanları")+ facet_wrap(~ HEF)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))


library(psych)
descIDT=with(dataWBT_USAK,describeBy(gen_att, HEF,mat=T,digits = 2))
descIDT
##     item      group1 vars  n mean   sd median trimmed  mad min max range
## X11    1 non-college    1 86 1.83 0.54    1.8    1.80 0.59   1 3.8   2.8
## X12    2     college    1 51 1.64 0.61    1.6    1.54 0.59   1 3.4   2.4
##     skew kurtosis   se
## X11 0.72     0.90 0.06
## X12 1.19     1.09 0.09
# rapor etmek için
#write.csv(descIDT,file="independent_t_test_desc.csv")
#türkçe excel için
# #write.csv2(descIDT,file="independent_t_test_desc.csv")
sp=sqrt((85*.543^2 + 50*.608^2)/(86+51-2))
# t-istatistik
tstatistic=(1.832-1.635)/(sp*sqrt(1/86+1/51))
# alfa=0.05 kritik değer
qt(.975,df=135)
## [1] 1.977692


# dataWBT HEF faktörünü içermez, yukarıda HEF faktörü oluşturulmuştur.
t.test(gen_att~HEF,data=dataWBT_USAK,var.equal=T,alternative="two.sided",
       conf.level=0.95)
##  Two Sample t-test
## 
## data:  gen_att by HEF
## t = 1.9587, df = 135, p-value = 0.05221
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.001903268  0.394880924
## sample estimates:
## mean in group non-college     mean in group college 
##                  1.831783                  1.635294

# büyüktür
t.test(gen_att~HEF,data=dataWBT_USAK,var.equal=T, alternative="greater",
       conf.level=0.95)
##  Two Sample t-test
## 
## data:  gen_att by HEF
## t = 1.9587, df = 135, p-value = 0.0261
## alternative hypothesis: true difference in means is greater than 0
## 95 percent confidence interval:
##  0.03034529        Inf
## sample estimates:
## mean in group non-college     mean in group college 
##                  1.831783                  1.635294


# küçüktür
t.test(gen_att~HEF,data=dataWBT_USAK,var.equal=T, alternative="less",
       conf.level=0.95)
## 
##  Two Sample t-test
## 
## data:  gen_att by HEF
## t = 1.9587, df = 135, p-value = 0.9739
## alternative hypothesis: true difference in means is less than 0
## 95 percent confidence interval:
##       -Inf 0.3626324
## sample estimates:
## mean in group non-college     mean in group college 
##                  1.831783                  1.635294


t.test(gen_att~HEF,data=dataWBT_USAK,var.equal=F, alternative="two.sided",
       conf.level=0.95)
##  Welch Two Sample t-test
## 
## data:  gen_att by HEF
## t = 1.9028, df = 95.885, p-value = 0.06006
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.008484626  0.401462282
## sample estimates:
## mean in group non-college     mean in group college 
##                  1.831783                  1.635294


#bootstrap ile %95 güven aralığı (normallik varsayımı yok)
set.seed(04012017)
B=5000       # bootstrap tekrar sayısı
alpha=0.05   # alfa
# grupları tanımla
GroupCollege=na.omit(dataWBT_USAK[dataWBT_USAK$HEF=="college","gen_att"])
GroupNONcollege=na.omit(dataWBT_USAK[dataWBT_USAK$HEF=="non-college","gen_att"])

output=c()
for (i in 1:B){
  x1=mean(sample(GroupCollege,replace=T,size=length(GroupCollege)))
  x2=mean(sample(GroupNONcollege,replace=T,size=length(GroupNONcollege)))
  output[i]=x2-x1   }
output=sort(output)
## yönsüz
# D yıldız alt
output[as.integer(B*alpha/2)+1]
## [1] -0.01338349

# D yıldız üst
output[B-as.integer(B*alpha/2)]
## [1] 0.3899111

##Yönlü x2>x1
# D yıldız alt
output[as.integer(B*alpha)+1]
## [1] 0.02202462
#hatalı yön x2<x1
# D yıldız üst
output[as.integer(B*(1-alpha))]
## [1] 0.3575695



n1=51
n2=86
tval=1.96

EB=tval/sqrt((n1*n2)/(n1+n2))
EB
## [1] 0.3464033

#veya effsize paketi ile
t.test(gen_att~HEF,data=dataWBT_USAK,var.equal=F,alternative="two.sided",
       conf.level=0.95)
## 
##  Welch Two Sample t-test
## 
## data:  gen_att by HEF
## t = 1.9028, df = 95.885, p-value = 0.06006
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.008484626  0.401462282
## sample estimates:
## mean in group non-college     mean in group college 
##                  1.831783                  1.635294
library(effsize)
cohen.d(gen_att~HEF,data=dataWBT_USAK, paired=F,conf.level=0.95,noncentral=F)
## 
## Cohen's d
## 
## d estimate: 0.346177 (small)
## 95 percent confidence interval:
##          inf          sup 
## -0.005791781  0.698145741
# noncentral=T argümanını araştırabilirsiniz.


#power.t.test
power.t.test(delta=.35, sd=.6,sig.level=0.05, power=0.95, 
             type="two.sample", alternative="two.sided")

gerMUK=data.frame(tavid=1:20,
                  bant=c(6.59,9.84 ,3.97,5.74,4.47,4.79,6.76,7.61,6.47,5.77,
                         7.36,10.45,4.98,5.85,5.65,5.88,7.77,8.84,7.68,6.89),
                  dikis=c(4.52,5.87,4.60,7.87,3.51,2.77,2.34,5.16,5.77,5.13,
                          5.55,6.99,5.78,7.41,4.51,3.96,3.56,6.22,6.72,5.17))

# Grafik verisi
library(tidyr)
plotdata=gather(gerMUK, metot, mukavemet, bant:dikis, factor_key=TRUE)

require(ggplot2)
ggplot(plotdata, aes(x = mukavemet)) +
  geom_histogram(aes(y = ..density..),col="black",alpha=0.7) + 
  geom_density(size=2) +
  theme_bw()+labs(x = "mukavemet")+ facet_wrap(~ metot)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))


library(psych)
descDT=with(gerMUK,describe(cbind(bant,dikis)))
descDT
##       vars  n mean   sd median trimmed  mad  min   max range  skew
## bant     1 20 6.67 1.71   6.53    6.54 1.45 3.97 10.45  6.48  0.55
## dikis    2 20 5.17 1.49   5.17    5.19 1.30 2.34  7.87  5.53 -0.08
##       kurtosis   se
## bant     -0.45 0.38
## dikis    -0.87 0.33

corDT=with(gerMUK,cor(bant,dikis,use="complete.obs"))
corDT
tsh=sqrt(((1.71^2+1.49^2)-(2*1.71*1.49*corDT))/(20))

# t-istatistik
tstatistic=(6.67-5.17)/tsh

#  alfa=0.05 kritik değer
qt(.975,df=19)
## [1] 2.093024


library(psych)
with(gerMUK, t.test(bant,dikis,paired=T,
                    alternative="two.sided",
                    conf.level=0.95))



set.seed(04012017)
B=5000       # bootstrap tekrar sayısı
alpha=0.05   # alfa

gerMUK=data.frame(ratid=1:20,
                  bant=c(6.59,9.84 ,3.97,5.74,4.47,4.79,6.76,7.61,6.47,5.77,
                         7.36,10.45,4.98,5.85,5.65,5.88,7.77,8.84,7.68,6.89),
                  dikis=c(4.52,5.87,4.60,7.87,3.51,2.77,2.34,5.16,5.77,5.13,
                          5.55,6.99,5.78,7.41,4.51,3.96,3.56,6.22,6.72,5.17))

output=c()
for (i in 1:B){
  #satırları örnekle
  bs_rows=sample(gerMUK$ratid,replace=T,size=nrow(gerMUK))
  bs_sample=gerMUK[bs_rows,]
  mean1=mean(bs_sample$bant)
  mean2=mean(bs_sample$dikis)
  output[i]=mean1-mean2
}
output=sort(output)

## yönsüz
# d yıldız alt
output[as.integer(B*alpha/2)+1]
## [1] 0.6865

# d yıldız üst
output[B-as.integer(B*alpha/2)]
## [1] 2.2415

##Yönlü x2>x1
# d yıldız alt
output[as.integer(B*alpha)+1]
## [1] 0.837

#yanlış yön x2<x1
# d yıldız üst
output[as.integer(B*(1-alpha))]
## [1] 2.1445


## dirençli prosedürler farklı sonuç vermediği için
## normallik ve varyans eşitliği varsayımları yapılmıştır.
n=20
tval=3.6678

EB=tval/sqrt(n)
EB
## [1] 0.820145

library(effsize)
cohen.d(gerMUK$bant,gerMUK$dikis, 
        paired=T, conf.level=0.95,noncentral=F)



#power.t.test
power.t.test(delta=.35, sd=.6,sig.level=0.05, power=0.95, 
             type="paired", alternative="two.sided")


# csv yükle 
#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT_TRABZON.csv'
#dataWBT_TRABZON=read.csv(urlfile)

dataWBT_TRABZON=read.csv("dataWBT_TRABZON.csv")

gerMUKtekrar=data.frame(ratid=1:22,                  
                        bant=c(7.47,10.60,4.73,6.66,5.37,5.72,7.56,8.64,7.37,6.79,
                               8.14,11.25,5.78,6.45,6.85,6.68,8.57,9.94,8.28,7.85,6.12,5.34),
                        dikis=c(5.34,6.64,5.64,8.89,4.31,3.97,3.32,6.36,6.73,6.33,
                                6.45,7.64,6.88,8.43,5.51,4.76,4.58,7.82,7.78,6.37,5.64,4.43))




###########################
###########################
######  BOLUM 5   ########
###########################
###########################


# Seçenek 1
# csv yükle
#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT_KOCAELI.csv'
#dataWBT_KOCAELI=read.csv(urlfile)
#remove URL 
#rm(urlfile)

# Seçenek 2
# csv yükle
#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
#dataWBT=read.csv(urlfile)

# URL sil 
#rm(urlfile)

# Seçenek 1
# csv yükle
dataWBT_KOCAELI=read.csv("dataWBT_KOCAELI.csv")
# Seçenek 2
# csv yükle
dataWBT=read.csv("dataWBT.csv")


#Kocaeli'yi seç
# sıralı silme uygula (listwise deletion)
dataWBT_KOCAELI=na.omit(dataWBT[dataWBT$city=="KOCAELI",
                                c("id","gen_att","education")])

#diplomasız katılımcıyı ilkokul alt sınıfına al
library(car)
dataWBT_KOCAELI$eduNEW <- recode(dataWBT_KOCAELI$education,
                                 "'None'='Primary School (5 years)'")

#kozmetik, faktör etiketini kısalt
dataWBT_KOCAELI$eduNEW <- recode(dataWBT_KOCAELI$eduNEW,
                                 "'High School (Lycee)'=
                                 'High School (Lycee) (4 years)'")

dataWBT_KOCAELI$eduNEW <- recode(dataWBT_KOCAELI$eduNEW,
                                 "'Vocational School'=
                                 'Vocational High School (4 years)'")
#kozmetik, alt sınıfları sırala
#levels(dataWBT_KOCAELI$eduNEW)
dataWBT_KOCAELI$eduNEW = factor(dataWBT_KOCAELI$eduNEW,
                                levels(dataWBT_KOCAELI$eduNEW)[c(4,3,1,6,2,5)])

# hangi katılımcı diplomasız
#which(dataWBT_KOCAELI$education=="None")

#boş alt sınıfları kaldır
dataWBT_KOCAELI$eduNEW=droplevels(dataWBT_KOCAELI$eduNEW)


#betimsel 
library(psych)
desc1BW=data.frame(with(dataWBT_KOCAELI,
                        describeBy(gen_att, eduNEW,mat=T,digits = 2)),
                   row.names=NULL)


#istenilenleri seç
# Table 1
desc1BW[,c(2,4,5,6,7,13,14)]
##                                 group1   n mean   sd median  skew kurtosis
## 1             Primary School (5 years)  70 2.11 0.41    2.2 -0.19     0.81
## 2 Junior High/ Middle School (8 years)  94 2.08 0.52    2.1 -0.35    -0.37
## 3        High School (Lycee) (4 years) 158 1.84 0.58    2.0  0.29     0.64
## 4     Vocational High School (4 years)  74 2.04 0.50    2.0 -0.14     0.41
## 5          Higher education of 2 years 112 1.80 0.53    1.8  0.28    -0.36
## 6    University - Undergraduate degree  62 1.78 0.53    1.8  0.06    -0.63


require(ggplot2)
ggplot(dataWBT_KOCAELI, aes(x = gen_att)) +
  geom_histogram(aes(y = ..density..),col="black",binwidth = 0.2,alpha=0.7) + 
  geom_density(size=1.5) +
  theme_bw()+labs(x = "Kocaeli Diploma ve TCA")+ facet_wrap(~ eduNEW)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))


require(ggplot2)
ggplot(dataWBT_KOCAELI, aes(eduNEW,gen_att)) +  geom_boxplot() + 
  labs(x = "Education",y="Kocaeli Diploma ve TCA")+coord_flip()


#ez kütüphanesini aktif hale getir
library(ez)
#katılımcı kimliğini belirten id değişkeni faktör olmazsa uyarı verir
dataWBT_KOCAELI$id=as.factor(dataWBT_KOCAELI$id)

# kozmetik, virgülden sonra kaç rakam gösterilsin?
options(digits = 3)


#birinci yol, ezANOVA fonksiyonu

alternative1 = ezANOVA(
  data = dataWBT_KOCAELI,
  wid=id, dv = gen_att, between = eduNEW,observed=eduNEW)
## Warning: Data is unbalanced (unequal N per group). Make sure you specified
## a well-considered value for the type argument to ezANOVA().

alternative1
## $ANOVA
##   Effect DFn DFd    F        p p<.05    ges
## 1 eduNEW   5 564 7.27 1.31e-06     * 0.0605
## 
## $`Levene's Test for Homogeneity of Variance`
##   DFn DFd  SSn  SSd   F      p p<.05
## 1   5 564 1.35 63.5 2.4 0.0361     *

# kritik F değeri
qf(.95,5,564)
## [1] 2.23

# ikinci yol, lm fonksiyonu
alternative2=lm(gen_att~eduNEW,data=dataWBT_KOCAELI)

#Tablo 2
anova(alternative2)

alternative3=aov(gen_att~eduNEW,data=dataWBT_KOCAELI)
summary(alternative3)

# Tablo 3
with(dataWBT_KOCAELI,pairwise.t.test(gen_att,eduNEW,p.adjust.method ="holm"))
## 


library(WRS2)
#t1way
# 20% kırpılmış
t1way(gen_att~eduNEW,data=dataWBT_KOCAELI,tr=.2,nboot=5000)


t1way(gen_att~eduNEW,data=dataWBT_KOCAELI,tr=.1,nboot=5000)

t1way(gen_att~eduNEW,data=dataWBT_KOCAELI,tr=.05,nboot=5000)

#alt sınıfların sıralanışı
lincon(gen_att~eduNEW,data=dataWBT_KOCAELI,tr=.1)[[2]]
## [1] "Higher education of 2 years"         
## [2] "Junior High/ Middle School (8 years)"
## [3] "University - Undergraduate degree"   
## [4] "Vocational High School (4 years)"    
## [5] "High School (Lycee) (4 years)"       
## [6] "Primary School (5 years)"
#ikili kıyaslamalar
round(lincon(gen_att~eduNEW,data=dataWBT_KOCAELI,tr=.1)[[1]][,c(1,2,6)],3)




# Seçenek 1
# csv yükle
#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT_Kayseri.csv'
#dataWBT_Kayseri=read.csv(urlfile)
#remove URL 
#rm(urlfile)

# Seçenek 2
# CSV yükle
#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
#dataWBT=read.csv(urlfile)
#URL sil 
#rm(urlfile)


# Seçenek 1
# csv yükle
dataWBT_Kayseri=read.csv("dataWBT_Kayseri.csv")

# Seçenek 2
# CSV yükle
dataWBT=read.csv("dataWBT.csv")

#Kayseri ilini seç
# sıralı silme uygula
dataWBT_Kayseri=na.omit(dataWBT[dataWBT$city=="KAYSERI",c("id","gen_att","higher_ed","gender")])

# Yüksek öğretim etiketlerini değiştir
dataWBT_Kayseri$HEF=droplevels(factor(dataWBT_Kayseri$higher_ed, 
                                      levels = c(0,1), 
                                      labels = c("non-college", "college")))

#table(dataWBT_Kayseri$gender)
#table(dataWBT_Kayseri$HEF)
#boş alt sınıfları düşür
dataWBT_Kayseri$gender=droplevels(dataWBT_Kayseri$gender)

with(dataWBT_Kayseri,
     table(gender,HEF))
##         HEF
## gender   non-college college
##   Female          99      50
##   Male            67      36

#  kozmetik, virgülden sonra kaç rakam gösterilsin?
options(digits = 3)

library(doBy)
library(moments)
desc2BW=as.matrix(summaryBy(gen_att~HEF+gender, data = dataWBT_Kayseri, 
                            FUN = function(x) { c(n = sum(!is.na(x)),
                                                  mean = mean(x,na.rm=T), sdv = sd(x,na.rm=T),
                                                  skw=moments::skewness(x,na.rm=T),                    
                                                  krt=moments::kurtosis(x,na.rm=T)) } ))
# Tablo 4
desc2BW


require(ggplot2)
ggplot(dataWBT_Kayseri, aes(x = gen_att)) +
  geom_histogram(aes(y = ..density..),col="black",binwidth = 0.2,alpha=0.7) + 
  geom_density(size=1.5) + theme_bw()+labs(x = "Kayseri Diploma ve TCA")+         facet_wrap(~ HEF+gender)+ theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))


require(ggplot2)
ggplot(dataWBT_Kayseri, aes(x=gender, y=gen_att))+ geom_boxplot()+
  facet_grid(.~HEF)+ labs(x = "Gender",y="Kayseri Diploma ve TCA")


library(ez)
#katılımcı kimliğini belirten id değişkeni faktör olmazsa uyarı verir
dataWBT_Kayseri$id=as.factor(dataWBT_Kayseri$id)
#birinci yol ezANOVA
alternative1 = ezANOVA(
  data = dataWBT_Kayseri,
  wid=id,dv=gen_att,between = .(HEF,gender),observed=.(HEF,gender),type=2)


# kritik F değeri
qf(.95,1,248)

library(WRS2)
#t2way # 20% kırpılmış
t2way(gen_att~HEF*gender,data=dataWBT_Kayseri,tr=.2)
## Call:
## t2way(formula = gen_att ~ HEF * gender, data = dataWBT_Kayseri, 
##     tr = 0.2)
## 
##              value p.value
## HEF         7.1310   0.011
## gender     20.2039   0.001
## HEF:gender  0.0855   0.772
# 10% kırpılmış
t2way(gen_att~HEF*gender,data=dataWBT_Kayseri,tr=.1)
## Call:
## t2way(formula = gen_att ~ HEF * gender, data = dataWBT_Kayseri, 
##     tr = 0.1)
## 
##              value p.value
## HEF         8.4235   0.005
## gender     33.1599   0.001
## HEF:gender  0.0361   0.850
# 5% kırpılmış
t2way(gen_att~HEF*gender,data=dataWBT_Kayseri,tr=.05)
## Call:


owadata =data.frame(id =1:8,
                    Alkolyok=c(1,2,2,2,3,3,3,6),
                    ikioz=c(2,3,3,3,4,4,4,5),
                    dortoz=c(5,5,6,6,6,7,7,8),
                    altioz=c(7,8,8,9,9,10,10,11))
apply(owadata,1, mean)
## [1] 3.2 4.0 4.4 4.8 5.4 6.0 6.2 7.6

#alt sınıfların ortalaması
apply(owadata[,-1],2, mean)
## Alkolyok    ikioz   dortoz   altioz 
##      2.8      3.5      6.2      9.0

#alt sınıfların standart sapması
apply(owadata[,-1],2,sd)



PSdata=data.frame(id=factor(1:17),
                  wave1=c(20,19,13,10,16,12,16,11,11,14,13,17,16,12,12,16,16),
                  wave2=c(28,27,18,17,29,18,26,21,15,26,28,23,29,18,26,21,22),
                  wave3=c(21,24,14,8,23,15,21,15,12,21,23,17,26,18,14,18,19))

options(digits = 3)

#veriyi uzun formata çevir
#head(PSdata)
library(tidyr)
data_long = gather(PSdata, wave, PrbSol, wave1:wave3, factor_key=TRUE)

#betimsel analizler
library(doBy)
library(moments)
desc1W=as.matrix(summaryBy(PrbSol~wave, data = data_long, 
                           FUN = function(x) { c(n = sum(!is.na(x)),
                                                 mean = mean(x,na.rm=T), sdv = sd(x,na.rm=T),
                                                 skw=moments::skewness(x,na.rm=T),                    
                                                 krt=moments::kurtosis(x,na.rm=T)) } ))
# Tablo 6
desc1W


cov(PSdata[,-1])

ggplot(data_long, aes(x=wave, y=PrbSol))+
  geom_boxplot()+
  labs(x = "Wave",y="Problem çözme bilgisi")


require(ggplot2)
ggplot(data_long, aes(x=wave, y=PrbSol, group=id))+
  geom_line() + labs(x = "Wave",y="Problem çözme bilgisi")

library(asbio)
with(data_long,tukey.add.test(PrbSol,wave,id))
## 
## Tukey's one df test for additivity 
## F = 5.943   Denom df = 31    p-value = 0.021


library(ez)
#birinci yol ezANOVA fonksiyonu

alternative1 = ezANOVA(
  data = data_long,
  wid=id, dv = PrbSol, within = wave,
  detailed = T,return_aov=T)

alternative1


PrbSolres=sort(alternative1$aov$id$residuals)
qqnorm(PrbSolres);qqline(PrbSolres)


# ikinci yol aov fonksiyonu
summary(aov(PrbSol ~ wave + Error(id/wave), data=data_long))

library(WRS2)

#rmanova
# 20% kırpılmış
with(data_long,rmanova(PrbSol,wave,id,tr=.20))


#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT_HATAY.csv'
#dataWBT_HATAY=read.csv(urlfile)
# csv yükle 
#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT_ESKISEHIR.csv'
#dataWBT_ESKISEHIR=read.csv(urlfile)

dataWBT_HATAY=read.csv("dataWBT_HATAY.csv")
dataWBT_ESKISEHIR=read.csv('dataWBT_ESKISEHIR.csv')
PSdata2=data.frame(id=factor(1:17),
                   wave1=c(23,22,16,15,21,16,21,13,12,19,14,18,21,16,16,18,18),
                   wave2=c(31,31,26,19,33,26,32,23,20,34,36,25,38,19,31,28,31),
                   wave3=c(28,31,26,20,33,21,31,27,19,30,24,25,28,19,24,29,30))

###########################
###########################
######  BOLUM 6   ########
###########################
###########################
# CSV yükle
#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT.csv'
#dataWBT=read.csv(urlfile)

#URL sil
#rm(urlfile)

dataWBT=read.csv('dataWBT.csv')
#Bayburt ilini seç
#  sıralı silme uygula (listwise deletion)
dataWBT_Bayburt=dataWBT[dataWBT$city=="BAYBURT",]
#hist(dataWBT_Bayburt$income_per_member)


with(dataWBT_Bayburt,cor(gen_att,income_per_member,
                         use = "complete.obs",method="pearson"))
## [1] 0.0664
with(dataWBT_Bayburt,cor.test(gen_att,income_per_member,
                              alternative = "two.sided",
                              method="pearson",
                              conf.level = 0.95,
                              na.action="na.omit"))   


sample_r=0.06641641 
r0=0        #boş hipotez
sample_n=137       # örneklem sayısı
zr=(0.5)*log((1+sample_r)/(1-sample_r))  # z transformasyonu
z0=(0.5)*log((1+r0)/(1-r0))  # z transformasyonu
sigmar=1/(sqrt(sample_n-3))
#z istatistiği
(zr-z0)/sigmar
## [1] 0.77

ll=zr-(qnorm(0.975)*sigmar)  # alt limit
ul=zr+(qnorm(0.975)*sigmar)  # üst limit

(exp(2*ll)-1)/(exp(2*ll)+1)  #ters transform
## [1] -0.102
(exp(2*ul)-1)/(exp(2*ul)+1)  #ters transform
## [1] 0.232

t=sample_r*(sqrt((sample_n-2)/(1-sample_r^2)))        
qt(c(.025, .975), df=(sample_n-2))      
## [1] -1.98  1.98
p.value = 2*pt(-abs(t), df=sample_n-2)  
p.value
## [1] 0.441


dataWBT_Bayburt2=na.omit(dataWBT_Bayburt[,c("gen_att","income_per_member")])
set.seed(31012017)
B=5000       #  bootstraps tekrarı
alpha=0.05   # alfa

#TCA ve gelir
originaldata=dataWBT_Bayburt2
# id ekle
originaldata$id=1:nrow(originaldata)
output=c()
for (i in 1:B){
  #sample rows
  bs_rows=sample(originaldata$id,replace=T,size=nrow(originaldata))
  bs_sample=originaldata[bs_rows,]
  output[i]=cor(bs_sample$gen_att,bs_sample$income_per_member)  }
output=sort(output)

## Yönsüz
# alt limit
output[as.integer(B*alpha/2)]
## [1] -0.138

# d yıldız üst
output[B-as.integer(B*alpha/2)+1]
## [1] 0.252

#  WRS2 paketi
dataWBT_Bayburt2=na.omit(dataWBT_Bayburt[,c("gen_att","income_per_member")])
library(WRS2)
pbcor(dataWBT_Bayburt2$gen_att,dataWBT_Bayburt2$income_per_member,beta=.2)
## Call:
## pbcor(x = dataWBT_Bayburt2$gen_att, y = dataWBT_Bayburt2$income_per_member, 
##     beta = 0.2)
## 
## Robust correlation coefficient: -0.0351
## Test statistic: -0.407
## p-value: 0.684

wincor(dataWBT_Bayburt2$gen_att,dataWBT_Bayburt2$income_per_member,tr=.2)
## Call:
## wincor(x = dataWBT_Bayburt2$gen_att, y = dataWBT_Bayburt2$income_per_member, 
##     tr = 0.2)
## 
## Robust correlation coefficient: -0.0197
## Test statistic: -0.229
## p-value: 0.82


dataWBT_Bayburt2=na.omit(dataWBT_Bayburt[,c("gen_att","income_per_member")])
top=quantile(dataWBT_Bayburt2$income_per_member,.95)
dataWBT_Bayburt2$incomeTC=ifelse(dataWBT_Bayburt2$income_per_member>top,                                               top,dataWBT_Bayburt2$income_per_member)
#transform
dataWBT_Bayburt2$incomeTC=log(dataWBT_Bayburt2$incomeTC+
                                sqrt((dataWBT_Bayburt2$incomeTC^2)+1))
with(dataWBT_Bayburt2,cor.test(gen_att,incomeTC,
                               alternative = "two.sided",
                               method="pearson",
                               conf.level = 0.95,
                               na.action="na.omit"))  


with(dataWBT_Bayburt,cor.test(gen_att,income_per_member,
                              alternative = "two.sided",
                              method="spearman",
                              conf.level = 0.95,
                              na.action="na.omit",
                              exact=FALSE)) 

with(dataWBT_Bayburt,cor.test(gen_att,income_per_member,
                              alternative = "two.sided", method="kendall",
                              conf.level = 0.95,
                              na.action="na.omit",
                              exact=FALSE)) 


dataWBT_Bayburt$binitem1=ifelse(dataWBT_Bayburt$item1==4,1,0)
require(psych)
with(dataWBT_Bayburt,biserial(gen_att,binitem1))

dataWBT_Kayseri=dataWBT[dataWBT$city=="KAYSERI",]
dataWBT_Kayseri$genderNUM=ifelse(dataWBT_Kayseri$gender=="Female",1,0)
with(dataWBT_Kayseri,cor.test(gen_att,genderNUM,
                              alternative = "two.sided",method="pearson", conf.level = 0.95,
                              na.action="na.omit"))   


dataWBT_Kayseri=dataWBT[dataWBT$city=="KAYSERI",]
table(dataWBT_Kayseri$gender,dataWBT_Kayseri$wage01)
##          
##           No Yes
##   Female  52  97
##   Male    49  54
##   Unknown  0   0

genderWAGE=matrix(c(52,49,97,54),ncol=2)
library(psych)
phi(genderWAGE)

#tetrakorik korelasyon
# 3. ve 6. sorular dikatom yapılsın
# Dünya Bankası araştırma grubu tarafından kullanılan yöntem 
# Eğer yanıt 1 (strongly disagree) veya 2 (disagree) ise 1 değilse 0.
dataWBT_Kayseri$binitem3=ifelse(dataWBT_Kayseri$item3==1|dataWBT_Kayseri$item3==2,1,0)
dataWBT_Kayseri$binitem6=ifelse(dataWBT_Kayseri$item6==1|dataWBT_Kayseri$item6==2,1,0)
require(psych)
tetrachoric(as.matrix(dataWBT_Kayseri[,c("binitem3","binitem6")]))
polychoric(as.matrix(dataWBT_Kayseri[,c("item3","item6")]))

#pearson
with(dataWBT_Kayseri,cor(item4,item5,use = "complete.obs"))

dataWBT_DIYARBAKIR=read.csv('dataWBT_DIYARBAKIR.csv')

# csv yükle 
#urlfile='https://raw.githubusercontent.com/burakaydin/materyaller/gh-pages/ARPASS/dataWBT_DIYARBAKIR.csv'
#dataWBT_DIYARBAKIR=read.csv(urlfile)


###########################
###########################
######  BOLUM 7   ########
###########################
###########################




Y=matrix(c(8,4,6,6,5,9,7,-6,-8,-1,0,5),ncol=1)
X=matrix(cbind(rep(1,12),
               c(0,-2,6,-2,5,4,3,-4,-4,-3,-2,-1),
               c(3,1,3,0,0,2,3,-5,-6,0,-2,1)),ncol=3)

solve(t(X)%*%X)%*%t(X)%*%Y


X2omitted=matrix(cbind(rep(1,12),c(0,-2,6,-2,5,4,3,-4,-4,-3,-2,-1)),ncol=2)
solve(t(X2omitted)%*%X2omitted)%*%t(X2omitted)%*%Y

# KT total
n=length(Y)
TotalSS=t(Y)%*%Y-(n*mean(Y)^2)
# KT Model
betahat=solve(t(X)%*%X)%*%t(X)%*%Y
ModelSS=t(betahat)%*%t(X)%*%Y-(n*mean(Y)^2)

ModelSS/TotalSS


Rsquared=ModelSS/TotalSS
#sörneklem
n=12
#bağımsız değişken sayısı
p=2
# sabit (intercept) varsa 1, yoksa 0 
int_inc=1
AdjustedRsquared=1-(1-Rsquared)*((n-int_inc)/(n-int_inc-p))
AdjustedRsquared


Yhat=X%*%betahat
residuals=Y-Yhat
residuals


library(mvtnorm)
sigma <- matrix(c(4,2,2,3), ncol=2)
xx <- rmvnorm(n=500, mean=c(0,0), sigma=sigma)
yy=5+xx[,1]*2+xx[,2]*-3+rnorm(500,0,1.5)
model=lm(yy~xx[,1]+xx[,2])
errors=rstudent(model)
predicted=predict(model)

library(ggplot2)
plotdata=data.frame(errors,predicted)
ggplot(plotdata, aes(x = predicted, y = errors)) +
  geom_point() + geom_hline(yintercept=0)+ ylab("Student artıklar")+
  theme_bw()+stat_smooth()



set.seed(04022017)
library(mvtnorm)
sigma <- matrix(c(4,2,2,3), ncol=2)
xx <- rmvnorm(n=100, mean=c(10,10), sigma=sigma)
yy=(xx[,1]*4)+(xx[,2]*-3)+rnorm(100,0,3)
tempdata=data.frame(yy,xx,id=1:100)
model=lm(yy~X1+X2,data=tempdata)
tempdata$SUTresiduals=rstudent(model)

# kaç tane artık değer kritik değerin üstünde
# alfa=.05
sum(abs(tempdata$SUTresiduals)>qt(c(.975), df=100-3-1))
## [1] 8

#hangi gözlemler?
tempdata[which(abs(tempdata$SUTresiduals)>qt(c(.975), df=100-3-1)),]


summary(influence.measures(model))

#artıklar
s2 <- (t(residuals) %*% residuals)/(nrow(Y)-nrow(betahat))  
Var_betahat <- s2[1,1]*solve(t(X)%*%X)  


#veri oluştur
set.seed(03032017)
library(mvtnorm)
sigma <- matrix(c(1,.7,.7,1), ncol=2)
xx <- rmvnorm(n=100, mean=c(1,1), sigma=sigma)
#heteroscedasticity ekle
hts=function(v1,v2){2+.5*v1+.5*v2}
yy=5+xx[,1]*5+xx[,2]*5+rnorm(100,0,hts(xx[,1],xx[,2]))
model=lm(yy~xx[,1]+xx[,2])
#summary(model)
errors=rstudent(model)
predicted=predict(model)
#student artıklar ve tahmin edilen Y
library(ggplot2)
plotdata=data.frame(errors,predicted)
ggplot(plotdata, aes(x = predicted, y = errors)) +
  geom_point() + geom_hline(yintercept=0)+ylab("Student artık değerler")+
  geom_segment(mapping=aes(xend = predicted, yend = 0)) +  theme_bw()


dfREG=2  #(p=2, bağımsız değişkenler X1 and X2)
dfRES=9  #(n-p', 12-3)
MSreg=ModelSS/dfREG
MSres=(TotalSS-ModelSS)/dfRES
MSreg/MSres
##      [,1]
## [1,] 32.8

#kritik F
qf(.95,dfREG,dfRES)
## [1] 4.26
1-pf(MSreg/MSres,dfREG,dfRES)



# X2 regresyon katsayısı 0'dan farklı mı
Bhyp=0  #boş hipotez değeri

# betahat daha önceden hesaplanmıştır
# X2 için hesaplanan katsayı
bx2=betahat[3]  
# Var_betahat daha önce hesaplanmıştır
# X2 regresyon katsayısına ait standart hata
se_bx2=sqrt(Var_betahat[3,3])
#t istatistiği
(bx2-Bhyp)/se_bx2
## [1] 5.33
# t kritik değeri
qt(.975,9)
## [1] 2.26

#p değeri
2*(pt(-abs((bx2-Bhyp)/se_bx2),9))
## [1] 0.000478


#veri oluştur
set.seed(02082017)
library(mvtnorm)
sigma=matrix(c(5.899559,4.277045,3.906341,
               4.277045,5.817412,3.654419,
               3.906341,3.654419,5.642258),ncol=3)
xx <- rmvnorm(n=200, mean=c(0,0,0), sigma=sigma)
yy=5+xx[,1]+xx[,2]*1.5+xx[,3]*2+rnorm(200,0,3)
simdata=data.frame(yy,xx,id=1:200)

library(leaps)
formula <- formula(paste("yy ~ ", 
                         paste(names(simdata[2:4]), collapse=" + ")))
allpossreg <- regsubsets(formula,nbest=3,data=simdata)
aprout <- summary(allpossreg)

# str(aprout) u inceleyiniz
# bu fonksiyon R2 ve Düzeltilmiş R2 den başka kriterler de hesaplar

APRtable=with(aprout,round(cbind(which,rsq,adjr2),3))
APRtable=data.frame(APRtable,check.rows = F,row.names = NULL)
APRtable$ppri=rowSums(APRtable[,1:4])
APRtable


require(ggplot2)
ggplot(APRtable, aes(x=ppri-1, y=rsq)) +
  geom_point(shape=1,size=3)+   
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)) +
  scale_x_continuous(breaks = seq(0, 3, by = 1))+
  theme_bw()+labs(x = "R-squared")+ theme(axis.text=element_text(size=15),
                                          axis.title=element_text(size=14,face="bold"))

ggplot(APRtable, aes(x=ppri-1, y=adjr2)) +
  geom_point(shape=1,size=3)+   
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)) +
  scale_x_continuous(breaks = seq(0, 3, by = 1))+
  theme_bw()+labs(x = "Adjusted R-squared")+ 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))




cor(simdata[,2:4])
##      X1    X2    X3
## X1 1.00 0.730 0.640
## X2 0.73 1.000 0.666
## X3 0.64 0.666 1.000

#en yüksek korelasyon .73, 
#çoklu doğrusallık problemi beklenmez

library(car)
vif(lm(yy~X1+X2+X3,data=simdata))
##   X1   X2   X3 
## 2.36 2.50 1.98


## Yvar: etkileşim yokken  bağımlı değişken
simdata$Yvar=3+simdata$X1*2+simdata$X2*3+rnorm(nrow(simdata),0,5)

library(visreg)

model=lm(Yvar~X1+X2+X1*X2,data=simdata)
visreg2d(model, "X1", "X2", plot.type="persp")


## Yvar: etkileşim varken  bağımlı değişken
simdata$Yvarint=3+simdata$X1*1+simdata$X2*2+simdata$X1*simdata$X2*1.5+rnorm(nrow(simdata),0,5)

library(visreg)

model2=lm(Yvarint~X1+X2+X1*X2,data=simdata)
visreg2d(model2, "X1", "X2", plot.type="persp")



