library(plotly)
library(dplyr)
library(stats)
library(lubridate)
library(rio)
library(here)
setwd(here("data","titration"))

columnnames <- c("mL","pH", "sec","dpHdml")

#load af forskellige filer
ID1 <- import("CT_vand_langå_0.2MNaOH.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))
ID2 <- import("pH_titration_NaOH_22_03_22-12_43_27.csv",";", skip=1, header=F,stringsAsFactors = FALSE,col.names = columnnames) %>%
  mutate(mL = as.numeric(mL),pH = as.numeric(pH),sec = as.numeric(sec),dpHdml = as.numeric(dpHdml))

#differentiering af data
titrationdata <- ID2
n <- 2
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
P <- plot_ly(data = titrationdata, x = ~pH) %>%
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

P


pHplot <- plot_ly(data = titrationdata, x = ~pH) %>%
  add_lines(y = ID1$pH, name = "pH test 1", x = ID1$mL,line=list(width=4))%>%
  add_lines(y = ID2$pH, name = "pH test 2", x = ID2$mL,line=list(width=4))%>%
  plotly::layout(
    yaxis = list(title = "pH", showgrid = FALSE, color = "red"),
    xaxis=list(title="V [mL]"),
    legend = list(x = 0.1, y = 100, orientation = 'h'),
    hovermode = "x", spikedistance = -1 )
pHplot
