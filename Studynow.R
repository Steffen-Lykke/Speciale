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
