2+2
a=2
a^2

vec=c(1,2,3,4)
rownames=c("Row 1","Row 2","Row 3")
colnames=c("Col 1","Col 2","Col 3")
mm=matrix(1:9,nrow=3,ncol=3,dimnames=list(rownames,colnames))

vores_data=data.frame(Col1=c(1,2,3),Col2=c("hi","Hello","world"))

vec[1]
vec[1:2]
vec[c(1,2)]
vec[-(1:2)]


mm[1,1]
mm[1,]

 vores_data$Col2
 
 
TV= seq(2,30,2)
min(TV) 
sd(TV)

minfunktion = function(a,b,c){
  res = (a+b)/c
  
  return(res)
}

minfunktion(3,5,4)

 min_normalisering = function(input){
   res = (input-mean(input))/sd(input)
   
   return(res)
 }
 min_normalisering(TV)
scale(TV)

### Betinget Kode
#   | OR
#   & AND
#   >=
#   <=
#   >
#   <
#   ==
x=-1
if(x>0){
  print("X er positiv")
} else if(x==0){
  print("x er nul")
} else if(x<0){}

TV[TV < 10| TV>20]
###
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
### indlæs data
starwars_data=starwars[,1:10]
head(starwars)
colnames(starwars)
human = starwars[starwars$species=="Human",]
head(human)

## 
starwars %>% filter(species=="Human",homeworld=="Tatooine")%>%select(name,height)%>%
  arrange(-height)

starwars%>%group_by(species)%>%tally()
starwars%>%group_by(species)%>%summarise(height_mean=mean(height,na.rm=T),mass_mean=mean(mass,na.rm=T))
starwars%>%filter(mass==max(mass,na.rm=T))%>%select(name,mass)
###plot med ggplot##

starwars_jabba=starwars%>%filter(name!="Jabba Desilijic Tiure")
ggplot(data=starwars_jabba,aes(height,mass,col=gender)) + geom_point() + theme_tufte()

### histogram ggplot
ggplot(starwars_jabba,aes(mass))+geom_histogram()


###boxplot ggplot
starwars_subset=starwars%>%filter(species=="Droid"|species=="Human"|species=="Gungan")
ggplot(data=starwars_subset,aes(species,mass))+geom_boxplot()

###Regression
lm1=lm(mass~height,data=starwars_jabba)

sum1=summary(lm1)

ggplot(data=starwars_jabba,aes(height,mass,col=gender)) + geom_point() + theme_tufte()+
  geom_abline(intercept=sum1$coefficients[1,1],slope=sum1$coefficients[2,1])
###### Libraries #####
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(visdat)
library(GGally)
library(Hmisc)
library(DMwR2)
library(data.table)
library(mltools)
library(ggfortify)
library(ggiraphExtra)
library(coronavirus)
library(lubridate)
library(plotly)

auto= read.csv("Automobile_dataset-210511-145757.csv")

str(auto)
unique(auto$num.of.doors)
auto$num.of.doors =  ifelse(auto$num.of.doors=="two",2,ifelse(auto$num.of.doors=="four",4,NA))

############## Missing Data ###############
apply(auto,2,function(x) sum(is.na(x)))
missingness = apply(auto,2,function(x) sum(x=="?",na.rm=T))

missing_names = colnames(auto[,missingness>0])
auto[,missing_names] = apply(auto[,missing_names],2,as.numeric)

auto = auto %>% mutate(across(where(is.character),as.factor))

################# Missingness ############################
vis_miss(auto)

missing_counts = as.data.frame(apply(auto,2,function(x) sum(is.na(x))))
missing_counts$names = rownames(missing_counts)
colnames(missing_counts)[1] = "NAs"

ggplot(missing_counts,aes(x=reorder(names,NAs),y=NAs))+geom_bar(stat="identity")+coord_flip()+xlab("Names")
#ggpairs(auto[,c(colnames(auto)[c(1,2,4:10)],"price")])
       
auto = auto %>% mutate(volume=length*width*height)

ggplot(data=auto,aes(volume,price,col=make))+geom_point()+facet_wrap(~make)

auto_omit = na.omit(auto)
auto_no_loss = auto[,-which(names(auto)%in%c("normalized losses"))]
auto_omit = na.omit(auto_no_loss)
#mean / mode inputation
NAconvert = function(x) replace(x,is.na(x),mean(x,na.rm=T))

auto_mean_mode = replace(auto,T,lapply(auto,NAconvert))

view(auto_mean_mode)
Hmisc::impute(auto$normalized.losses,mean)
#MCAR MAR MNAR
auto = auto_mean_mode

auto_numeric = auto %>% select(where(is.numeric))
auto_factor = auto %>% select(where(is.factor))

auto_factor_long = auto_factor %>% gather(Variable,Value)
 
for_plot = auto_factor_long %>% group_by(Variable,Value)%>%tally()
ggplot(for_plot,aes(Variable,n,group=Value,fill=Value))+geom_bar(stat="identity",position="dodge")


### nmominal / odinal (er der rangereing i dataen)

auto_ordinal = auto_factor[,8]
auto_nominal = auto_factor[,-8]

auto_ordinal = factor(auto_ordinal,levels=c("two","three","four","five","six","eight","twelve"))
auto_ordinal_LE = as.numeric(auto_ordinal)
auto_nominal_OHE = one_hot(as.data.table(auto_nominal))

auto_numeric_total = cbind(auto_numeric,auto_ordinal_LE,auto_nominal_OHE)
auto.pca = prcomp(auto_numeric_total,center=T,scale=T)
autoplot(auto.pca)+theme_bw()

data_to_plot = data.frame(Variable=auto$fuel.type,auto.pca$x[,1:2])
ggplot(data=data_to_plot,aes(PC1,PC2,col=Variable))+geom_point()

#################### Linear Models ############################
lm1=lm(price~HP,data=auto)
lm2=lm(log(price)~HP,data=auto)

summary(lm2)
based = ggplot(data=auto,aes(HP,log(price)))+geom_point()+geom_smooth(method = "lm",col="red",se=F)

#Residuals
par(mfrow=c(2,2))
plot(lm2)

auto$predicted = predict(lm2)
auto$residuals = residuals(lm2)

based + geom_segment(aes(xend=HP,yend=predicted))+geom_point(aes(color=residuals))

lm3 = lm(log(price)~HP+make,data=auto)
summary(lm3)

auto$predictedlm3 = predict(lm3)

ggplot(auto,aes(HP,log(price),color=make))+geom_point()+geom_line(aes(y=predictedlm3))

plot(lm3)

##################### Tidsrækker #############################
data("coronavirus")

head(coronavirus)
str(coronavirus)
coronavirus$date = as.character(coronavirus$date) #Hvordan date data ofte ser ud
coronavirus$date = ymd(coronavirus$date)# og hvordan det kan fikses

coronavirus_date = coronavirus%>%filter(type=="confirmed")%>%group_by(date,country)%>%summarise(cases_sum=sum(cases))

ggplot(data=coronavirus_date,aes(date,cases_sum))+geom_bar(stat="identity",width=0.2)+theme_bw()+ylab("Confirmed Cases")

ggplot(data=coronavirus_date%>%filter(country=="Denmark"),aes(date,cases_sum))+
  geom_bar(stat="identity",width=0.2)+theme_bw()+ylab("Confirmed Cases")

coronavirus_date%>%filter(country%in%c("Denmark","Russia"))%>%
  ggplot(aes(date,cases_sum,col=country))+
    geom_line()+theme_bw()+ylab("Confirmed Cases")+scale_x_date(date_breaks="2 months",date_labels = "%b%y")+
    scale_y_continuous(labels=scales::comma)

