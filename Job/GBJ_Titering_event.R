library(plotly)
library(dplyr)
library(stats)
library(lubridate)
library(rio)
library(here)
setwd(here("Job","IWR titrering"))

columnnames <- c("mL","pH", "sec","dpHdml")

#load af forskellige filer
#alle test er k?rt med 0,1M NaOH 
#Test 1 : r? spildevand d. 25/10-21
T2_11 <- import("11_nov_tank2_NaOH_0.1M_50mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T3_11 <- import("11_nov_tank2_NaOH_0.2M_50mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))

T2_15 <- import("15_nov_tank2_NaOH_0.1M_50mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T3_15 <- import("15_nov_tank2_NaOH_0.1M_50mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))

T1_18 <- import("18_nov_tank1_NaOH_0.2M_50mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T2_18 <- import("18_nov_tank2_morgen_NaOH_0.2M_50mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T3_18 <- import("18_nov_tank3_NaOH_0.1M_50mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))

T1_19 <- import("19_nov_tank1_pH_NaOH_0.2M_46mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T2_19 <- import("19_nov_tank2_NaOH_0.5_50mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T3_19 <- import("19_nov_tank3_NaOH_0.2M_49mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))

T1_23 <- import("23_nov_tank1_NaOH_0.5_100mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T2_23 <- import("23_nov_tank2_pH_NaOH_0.2M_100mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T3_23 <- import("23_nov_tank3_pH_NaOH_0.2M_100mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))

T1_25 <- import("25_nov_tank1_pH_NaOH_0.2M_100mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T2_25 <- import("25_nov_tank2_NaOH_0.5M_100mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T3_25 <- import("25_nov_tank3_pH_NaOH_0.2M_100mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))

T1_29 <- import("29_nov_tank1_pH_NaOH_0.2_M_100mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T2_29 <- import("29_nov_tank2_pH_NaOH_0.2M_100mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
T3_29 <- import("29_nov_tank3_pH_NaOH_0.2M_100mL.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))

#differentiering af data
titrationdata <- T3_23
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
#write.csv(titrationdata,here("Job","Indlet 4-11.csv"))

#polynomisk regration
tidata1<-dplyr::filter(titrationdata, dvdpH > -100 &dvdpH < 100  &pH<5.5&pH>3.3)
tidata1$mod<-fitted(lm(tidata1$dvdpH ~ poly(tidata1$pH,2)))
tidata1[which.max(tidata1$mod),]


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
  add_lines(y = T2_11$pH, name = "pH test 1", x = T2_11$mL,line=list(width=4))%>%
  add_lines(y = T3_11$pH, name = "pH test 2", x = T3_11$mL,line=list(width=4))%>%
  plotly::layout(
    yaxis = list(title = "pH", showgrid = FALSE, color = "red"),
    xaxis=list(title="V [mL]"),
    legend = list(x = 0.1, y = 100, orientation = 'h'),
    hovermode = "x", spikedistance = -1 )
pHplot
