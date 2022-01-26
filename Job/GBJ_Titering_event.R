library(plotly)
library(dplyr)
library(stats)
library(lubridate)
library(rio)

setwd("C:/Users/53887/OneDrive - Grundfos/Desktop/Titering/CSV")

columnnames <- c("mL","pH", "sec","dpHdml")

#load af forskellige filer
#alle test er kørt med 0,1M NaOH 
#Test 1 : rå spildevand d. 25/10-21
test_1 <- import("1_pH_titration_NaOH_22_11_21-11_11_51.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
#Test 2 : Udløbsspildevand d. 25/10-21
test_2 <- import("2_igen_pH_titration_NaOH_19_11_21-15_09_44.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
#Test 3 : rå spildevand d. 04/11-21
test_3 <- import("3_pH_titration_NaOH_22_11_21-12_43_20.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
#Test 4 : Udløbsspildevand d. 04/11-21
test_4 <- import("4_pH_titration_NaOH_19_11_21-12_55_35.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
#Test 5 : rå spildevand d. 05/11-21
test_5 <- import("5_pH_titration_NaOH_22_11_21-14_01_59.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
#Test 6 : Udløbsspildevand d. 05/11-21    udløbs turb=35 NTU
test_6 <- import("6_pH_titration_NaOH_19_11_21-14_08_05.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))

#differentiering af data
titrationdata <- test_3
n <- 7
dvdpH <- diff(titrationdata$mL,lag=n)/diff(titrationdata$pH,lag=n) 
length(dvdpH) =nrow(titrationdata)-(n-1)
dpHdv <- diff(titrationdata$pH,lag=n)/diff(titrationdata$mL,lag=n) 
length(dpHdv) = nrow(titrationdata)-(n-1)
diff.cal<- cbind(dvdpH,dpHdv) 
na_df<-data.frame(matrix(NA,nrow = n-1, ncol = 2))
colnames(na_df) <- c('dvdpH','dpHdv')
diff.cal<-rbind(na_df,diff.cal)
titrationdata <-cbind(titrationdata,diff.cal)
write.csv(titrationdata,"C:/Users/53887/OneDrive - Grundfos/Desktop/Titering/Indlet 4-11.csv")

#polynomisk regration
tidata3<-dplyr::filter(titrationdata, dvdpH > -100 &dvdpH < 100  &pH<5.5&pH>3.3)
tidata3$mod<-fitted(lm(tidata3$dvdpH ~ poly(tidata3$pH,2)))
tidata3[which.max(tidata3$mod),]


#plot af data
P2 <- plot_ly(data = titrationdata, x = ~pH) %>%
  add_lines(y = ~pH, name = "pH", color = I("red") ,x = ~mL,line=list(width=4))%>%
  add_lines(y = ~dvdpH, yaxis = "y2", name = "dV/dpH",  color = I("darkgreen"))%>%
  #add_lines(y=tidata3$mod,yaxis = "y2", name = "dV/dpH model",x = tidata3$pH, color = I("green"))%>%
  subplot(nrows = 2)%>%
  plotly::layout(yaxis=list(title="pH"),yaxis2=list(title="dV/dpH"),
                 xaxis=list(showgrid = FALSE,
                            showspikes = TRUE,
                            spikemode = "across",
                            spikesnap = "data",
                            spikecolor = "black",
                            spikethickness = 0.1,
                            spikedash = "solid", 
                            showline = TRUE), hovermode = "x",
                 spikedistance = -1) 
P2


pHplot <- plot_ly(data = titrationdata, x = ~pH) %>%
  add_lines(y = test_1$pH, name = "pH test 1", x = test_1$mL,line=list(width=4))%>%
  add_lines(y = test_2$pH, name = "pH test 2", x = test_2$mL,line=list(width=4))%>%
  add_lines(y = test_3$pH, name = "pH test 3", x = test_3$mL,line=list(width=4))%>%
  add_lines(y = test_4$pH, name = "pH test 4", x = test_4$mL,line=list(width=4))%>%
  add_lines(y = test_5$pH, name = "pH test 5", x = test_5$mL,line=list(width=4))%>%
  add_lines(y = test_6$pH, name = "pH test 6", x = test_6$mL,line=list(width=4))%>%
  plotly::layout(
    yaxis = list(title = "pH", showgrid = FALSE, color = "red"),
    xaxis=list(title="V [mL]"),
    legend = list(x = 0.1, y = 100, orientation = 'h'),
    hovermode = "x", spikedistance = -1 )
pHplot
