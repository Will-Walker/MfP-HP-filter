install.packages("mFilter")
install.packages("quantmod")
library(mFilter)
data <- new.env()

# Set time series (start and end)
date.start <- "1960-01-01"
date.end <- "2019-12-30"

#"y_obs", "c_obs", "i_obs", "w_obs", "n_obs"
#QUARTERLY

tickers <-  c("GBRRGDPQDSNAQ",    #Real Gross Domestic Product #Y
              "NAEXKP02GBQ189S",  #Real Personal Consumption Expenditures #C
              "NAEXKP04GBQ652S")  #Real Gross Private Domestic Investment #I

# FRED
library("quantmod")
getSymbols( tickers
            , src = "FRED"
            , from = date.start 
            , to = date.end  
            , env = data
)

# Data set
dtx1 <- data$GBRRGDPQDSNAQ
GDP <- dtx1[paste(date.start,date.end,sep="/")]
dtx2 <- data$NAEXKP02GBQ189S
Consumption <- dtx2[paste(date.start,date.end,sep="/")]
dtx3 <- data$NAEXKP04GBQ652S
Investment <- dtx3[paste(date.start,date.end,sep="/")]


# HP filtering (with lambda=1600)
hpf1 <- hpfilter(log(GDP),freq = 1600)
hpf2 <- hpfilter(log(Consumption),freq = 1600)
hpf3 <- hpfilter(log(Investment),freq = 1600)

########## Results ##########

# Results GDP
out_1 <- xts(cbind(hpf1$x, hpf1$trend, hpf1$cycle), index(GDP))
colnames(out_1) <- c("x", "trend", "cycle")
par(mfrow = c(3, 1), mar = c(3, 2, 2, 1))

plot(out_1[,"x"], t= "n", main = paste(hpf1$title, "of", hpf1$xname))
lines(out_1[,"trend"], col = "red")
plot(out_1[,"cycle"], t = "n", main = "Cyclical component (deviations from trend)", col = "steelblue")

# Results Consumption
out_2 <- xts(cbind(hpf2$x, hpf2$trend, hpf2$cycle), index(Consumption))
colnames(out_2) <- c("x", "trend", "cycle")
par(mfrow = c(3, 1), mar = c(3, 2, 2, 1))

plot(out_2[,"x"], t= "n", main = paste(hpf2$title, "of", hpf2$xname))
lines(out_2[,"trend"], col = "red")
plot(out_2[,"cycle"], t = "n", main = "Cyclical component (deviations from trend)", col = "steelblue")


# Results Investment
out_3 <- xts(cbind(hpf3$x, hpf3$trend, hpf3$cycle), index(Investment))
colnames(out_3) <- c("x", "trend", "cycle")
par(mfrow = c(3, 1), mar = c(3, 2, 2, 1))

plot(out_3[,"x"], t= "n", main = paste(hpf3$title, "of", hpf3$xname))
lines(out_3[,"trend"], col = "red")
plot(out_3[,"cycle"], t = "n", main = "Cyclical component (deviations from trend)", col = "steelblue")



