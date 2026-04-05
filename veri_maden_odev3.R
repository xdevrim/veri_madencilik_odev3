## Sentetik veri ureterek kesifsel veri analizi
## Veri uretmek icin gerekli olan kutuphaneyi install.packages("MASS")ile yukluyoruz

install.packages("MASS")
library(MASS)

#################################################################
## Veri seti olusturma islemi verileri sabitlemek icin seed() komutu kullanılarak yapilmistir.

set.seed(456)

##Gozlem sayisi belirlenmistir ve 4 tane bagimsiz x degiskeni icin veri uretilmistir

n<-120
x1<-rnorm(n,mean = 50,sd=12)
x2<-rnorm(n,mean = 60,sd=8)
x3<-rnorm(n,mean = 48,sd=15)
x4<-rnorm(n,mean = 75,sd=9)

### Olusturulan veri seti icin bir bagimli degiskene regresyon fonksiyonu olusturulmustur

y=1.2*x1+0.7*x2+0.8*x3+0.2*x4+rnorm(n,mean=5,sd=1.2)

### Veri setini dataframe ve tibble yapmak için tidyverse paketi yüklenmis ve dataframe ve tibble yapilmistir.

install.packages("tidyverse")
install.packages("magrittr", repos = "https://cran.r-project.org")
library(tidyverse)
sentetik_veri<-data.frame(y,x1,x2,x3,x4)
sentetik_veri_tibble<-as.tibble(sentetik_veri)

##Kurulum hata verdigi icin magnittr paketi ayrica yuklendi

################################################################

## Olusturulan veri seti head(),names(),summary() fonksiyonlari ile incelenmistir

head(sentetik_veri)
names(sentetik_veri)
summary(sentetik_veri)

################################################################

## Verilerin tip kontrolü

str(x1)
str(x2)
str(x3)
str(x4)

#############################################################

## Veri setindeki eksik verilerin kontrolü ve doldurulması
## Sentetik veride eksik olması beklenmedigi icin kontrol ve doldurrma adimlari göstermek maksadıyla asagidaki kodlar uygulanmistir
no_nax1<-!anyNA(x1)
no_nax2<-!anyNA(x2)
no_nax3<-!anyNA(x3)
no_nax4<-!anyNA(x4)
##her degisken tek tek bos deger var mı bakılır
x1_na<-is.na(x1)
x2_na<-is.na(x2)
x3_na<-is.na(x3)
x4_na<-is.na(x4)
##her degisken icin bosluklari doldurmak icin medyan (ortalamada olabilir) alındı
medyan_x1<-median(x1,na.rm = TRUE)
medyan_x2<-median(x2,na.rm = TRUE)
medyan_x3<-median(x3,na.rm = TRUE)
medyan_x4<-median(x4,na.rm = TRUE)
## bosluklar degiskenlerin medyani ile dolduruldu
x1_filled<-ifelse(x1_na,medyan_x1,x1)
x2_filled<-ifelse(x2_na,medyan_x2,x2)
x3_filled<-ifelse(x3_na,medyan_x3,x3)
x4_filled<-ifelse(x4_na,medyan_x4,x4)
###############################################################

## Aykırı deger tespiti icin once box plot ile dagilim bakılmis ve IQR ile ceyreklikler hesaplanmistir
boxplot(x1_filled)
boxplot(x2_filled)
boxplot(x3_filled)
boxplot(x4_filled)

## Boxplotlar incelenmis herhangi bir aykırı deger görülmemiştir

## İkinci aykırı deger tespit yöntemi olan z skoru degerlerine bakilmistir
##z skoru degeri 3 ten buyuk olanlari aykiri degiskenine cekiyoruz
aykiri_x1<-abs(scale(x1_filled))>3
aykiri_x2<-abs(scale(x2_filled))>3
aykiri_x3<-abs(scale(x3_filled))>3
aykiri_x4<-abs(scale(x4_filled))>3
## aykiri degerler kontrol edilmis tamaminin FALSE olduğu gorulmustur

##########################################################################

## Kesfedici veri analizi icin görsellestirme islemlerine histogram ile baslanilmistir

hist(x1_filled)
hist(x2_filled)
hist(x3_filled)
hist(x4_filled)

## Keşfedici veri analizi için ikinci olarak QQplot grafige bakıyoruz
qqnorm(x1_filled)
qqline(x1_filled)
qqnorm(x2_filled)
qqline(x2_filled)
qqnorm(x3_filled)
qqline(x3_filled)
qqnorm(x4_filled)
qqline(x4_filled)

## Kantil degerlere bakiyoruz
summary(x1_filled)
summary(x2_filled)
summary(x3_filled)
summary(x4_filled)

#####################################################################

###standardize islemi yapıyoruz
x1_std<-scale(x1_filled)
x2_std<-scale(x2_filled)
x3_std<-scale(x3_filled)
x4_std<-scale(x4_filled)

## Olusturdugumuz y bagimli degiskeni ile bagimsiz degiskenlerin korelasyon degerlerine bakılması

korelasyon_x1<-cor(x1_filled,y)
korelasyon_x2<-cor(x2_filled,y)
korelasyon_x3<-cor(x3_filled,y)
korelasyon_x4<-cor(x4_filled,y)

### Sentetik veri setimiz için çoklu regresyon modeli kuruyoruz ve özetini alıyoruz
model_sentetik<-lm(y~x1_filled+x2_filled+x3_filled+x4_filled)
summary(model_sentetik)

### Bagimsiz degiskenler icin korelasyon matrisi ve görselleştirilmesini yapıyoruz

sentetik_korelasyon_matrix<-cor(data.frame(x1_filled,x2_filled,x3_filled,x4_filled))
print(sentetik_korelasyon_matrix)

### Korelasyon matrisinin görsellestirilmesi
install.packages("corrplot")
library(corrplot)
corrplot(sentetik_korelasyon_matrix,method="color")

### Bagimsiz degiskenlerin bagimli degisken ile iliskisini dagilim grafigi ile görsellestiriyoruz

par(mfrow=c(2,2))
plot(x1_filled,y,xlab="x1",ylab = "y",col="red",pch=16)
plot(x2_filled,y,xlab = "x2",ylab = "y",col="yellow",pch=16)
plot(x3_filled,y,xlab = "x3",ylab="y",col="green",pch=16)
plot(x4_filled,y,xlab="x4",ylab = "y",col="blue",pch=16)

#################################################################
#################################################################
#################################################################

## Veri Cekerek Kesifsel Veri Analizi##
## Odev 2 de kullanılan veri seti kullanilmistir

### Oncelikle veriyi R programına cekiyoruz
ik_kesifveri<-read.csv("https://raw.githubusercontent.com/ryankarlos/Human-Resource-Analytics/refs/heads/master/Original_Kaggle_Dataset/HR_comma_sep.csv")

## veri setini dataframe ve tibble formatına ceviriyoruz
ik_kesif_df<-data.frame(ik_kesifveri)
ik_kesif_tibble<-as.tibble(ik_kesif_df)

###Veri setini tanımak icin head(),names(),summary() kodlari kullanilmistir

head(ik_kesif_tibble)
names(ik_kesif_tibble)
summary(ik_kesif_tibble)

### Değişken tiplerine bakıyoruz
str(ik_kesif_tibble)

#####################################################################

##Eksik veri kontrolu ve eksik verilerin tamamlanması
## satisfaction_level,avarage_mothly_hours,time_spend_company degiskenleri kullanılacaktır

satisfaction_anyNA<-!anyNA(ik_kesif_df$satisfaction_level)
mothly_hours_anyNA<-!anyNA(ik_kesif_df$avarage_montly_hours)
time_spend_anyNA<-!anyNA(ik_kesif_df$time_spend_company)

### Sutunlarda herhangi bir eksik veri bulunmadı ancak kontrol icin is.na() ve fill islemi yapacagiz

satisfaction_na<-is.na(ik_kesif_df$satisfaction_level)
monthly_hours_na<-is.na(ik_kesif_df$average_montly_hours)
time_spend_na<-is.na(ik_kesif_df$time_spend_company)

### Medyanlari Bulma
medyan_satisfaction<-median(ik_kesif_df$satisfaction_level,na.rm=TRUE)
medyan_monthlyhours<-median(ik_kesif_df$average_montly_hours,na.rm = TRUE)
medyan_timespend<-median(ik_kesif_df$time_spend_company,na.rm = TRUE)

### Eksik verileri medyan ile doldurma (LOCF de kullanılabilir)

satisfaction_filled<-ifelse(satisfaction_na,medyan_satisfaction,ik_kesif_df$satisfaction_level)
monthlyhours_filled<-ifelse(monthly_hours_na,medyan_monthlyhours,ik_kesif_df$average_montly_hours)
timespend_filled<-ifelse(time_spend_na,medyan_timespend,ik_kesif_df$time_spend_company)

#####################################################################

##Aykiri deger tespiti
## Boxplot ile kontrol

par(mfrow=c(1,3))
boxplot(satisfaction_filled)
boxplot(monthlyhours_filled)
boxplot(timespend_filled)

### z skorları ile aykırı değer tespitine devam ediyoruz

aykiri_satisfaction<-abs(scale(satisfaction_filled))>3
aykiri_montlyhours<-abs(scale(monthlyhours_filled))>3
aykiri_timespend<-abs(scale(timespend_filled))>3

### IQR ile aykiri deger tespiti

##Satisfaction icin IQR
Q1_satisfaction<-quantile(satisfaction_filled,0.25)
Q3_satisfaction<-quantile(satisfaction_filled,0.75)
IQR_satisfaction<-Q3_satisfaction-Q1_satisfaction
upper_satisfaction<-Q3_satisfaction+1.5*IQR_satisfaction
lower_satisfaction<-Q1_satisfaction-1.5*IQR_satisfaction
aykiri_IQRsatisfaction<-satisfaction_filled<lower_satisfaction|satisfaction_filled>upper_satisfaction
sum(aykiri_IQRsatisfaction)
## Mothly_hours icin IQR
Q1_monthlyhours<-quantile(monthlyhours_filled,0.25)
Q3_monthlyhours<-quantile(monthlyhours_filled,0.75)
IQR_monthlyhours<-Q3_monthlyhours-Q1_monthlyhours
upper_monthlyhours<-Q3_monthlyhours+1.5*IQR_monthlyhours
lower_monthlyhours<-Q1_monthlyhours-1.5*IQR_monthlyhours
aykiri_IQRmonthlyhours<-monthlyhours_filled<lower_monthlyhours|monthlyhours_filled>upper_monthlyhours
sum(aykiri_IQRmonthlyhours)
## Time_spend değişkeni için IQR
Q1_timespend<-quantile(timespend_filled,0.25)
Q3_timespend<-quantile(timespend_filled,0.75)
IQR_timespend<-Q3_timespend-Q1_timespend
upper_timespend<-Q3_timespend+1.5*IQR_timespend
lower_timespend<-Q1_timespend-1.5*IQR_timespend
aykiri_IQRtimespend<-timespend_filled<lower_timespend|timespend_filled>upper_timespend
sum(aykiri_IQRtimespend)

############################################################################

### Kesfedici veri analizi icin histogram grafikleri cizdirilmistir
par(mfrow=c(1,3))
hist(satisfaction_filled)
hist(monthlyhours_filled)
hist(timespend_filled)

###Kesfedici veri analizi icin QQplot grafiklere bakilmistir
par(mfrow=c(1,3))
qqnorm(satisfaction_filled)
qqline(satisfaction_filled)
qqnorm(monthlyhours_filled)
qqline(monthlyhours_filled)
qqnorm(timespend_filled)
qqline(timespend_filled)

### Hem histogramda hemde qqplot ta satisfaction ve monthly_hours degiskeni normale yakin dagilmis
##ancak time_spend degiskeni daginik yapidadir##

### Kantil Degerelerin incelenmesi

summary(satisfaction_filled)
summary(monthlyhours_filled)
summary(timespend_filled)

### satisfaction icin medyan 0.64 ortalama 0.61
### monthly_hours icin medyan 200 ortalama 201.1
###time_spend icin medyan 3 ortalama 3.49 oldugu medyan ve ortalamalar arasi cok fark olmadıgı goruldu

####################################################################

#Veri standardizasyonunun yapilmasi
satisfaction_std<-scale(satisfaction_filled)
monthly_hours_std<-scale(monthlyhours_filled)
time_spend_std<-scale(timespend_filled)
summary(satisfaction_std)
summary(monthly_hours_std)
summary(time_spend_std)
###Veriler standardize edilmis olup ortalamaları 0 degeri aldigi gorulmustur

#####################################################################

### Verilerin korelasyonlarına bakilmistir
## Satisfaction bagimli degeri ile avarage_monthly_hours bagimsiz degiskeni ve time_spend_company bagimsiz degiskeni arasinda korelasyon degerelerine bakilmistir


korelasyon_monthly_hours<-cor(monthlyhours_filled,satisfaction_filled)
korelasyon_time_spend_company<-cor(timespend_filled,satisfaction_filled)

##satisfaction ve avarage_monthly_hours arasında -0.02
##satisfaction ile time_spend_company arasinda -0.1 korelasyon katsayısı ölcüldü
##her iki bagimsiz degisken icinde ters yönlü zayif etki var

#####################################################################

##Modelin kurulması
model_is_tatmini<-lm(satisfaction_std~monthly_hours_std+time_spend_std)
summary(model_is_tatmini)

##Özete göre avarage_monthly_hours yani aylık çalışma saati iş tatmini üzerinde anlamlı bir etkisi yok çünkü p>.05 ancak time-spend_company değişkeni yani şirkette kalma süresi (kıdem) iş tatmini üzerin de etkili p<.05 ve bu etki negatif yöndedir

####################################################################

## Bağımsız değişkenler arası korelasyon matrisi
ikmodel_korelasyon_matrix<-cor(data.frame(monthlyhours_filled,timespend_filled))
print(ikmodel_korelasyon_matrix)

### Korelasyon matrisinin görsellestirilmesi

corrplot(ikmodel_korelasyon_matrix,method="color",)
### Her iki bağımsız değişken arasında 0.12 korelasyon katsayısı vardır budurumda multicollinearity durumu yoktur.

#################################################################
### Satisfaction(iş tatmini) bagimli degisken ile avarage_monthly_hours(aylık çalışma saati) ve time_spend_company(şirkette kaldığı süre veya kıdem) bağımsız değişkenlerinin iliskisini dagilim grafigi ile görsellestiriyoruz

par(mfrow=c(1,3))
plot(monthlyhours_filled,satisfaction_filled,xlab="Aylık Çalışma Saati",ylab = "İş Tatmini",col="red",pch=16)
plot(timespend_filled,satisfaction_filled,xlab = "Şirket Kıdemi",ylab = "İş Tatmini",col="yellow",pch=16)

## İs tatmini uzerinde etkisi olmasi beklenen aylik calisma saati ve is yeri icinde kıdem durumları görsellestirildiginde
##Regresyon modelinde ortaya çıkan aylık çalısma saatinin (avarage_monthly_hours) is tatmini üzerinde etkili olmadigi durumu görselde görülmektedir. Ayrıca is tatmini üzerinde az etkisi olduğu modelde ortaya çıkan is yeri kıdemi(time_spend_company) icinde zayıf dogrusal iliski görselde görülmüstür