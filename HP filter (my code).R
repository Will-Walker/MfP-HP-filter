# packages and functions --------------------------------------------------

library(dplyr)
library(fredr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(mFilter)
library(gridExtra)

#my FRED API, shouldent really leave it here but oh well
fredr_set_key("c903caa9d57ed28725e00a7a98cd9099")

#function for calling the mapping for a ggplot legend
#useful for messing around with plot legends in grid.arrange() or similar
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# read in data ------------------------------------------------------------

#read in data series from FRED, yes the IDs are meaningless
RGDP <-fredr_series_observations(series_id = "GBRRGDPQDSNAQ") 
Consumption <-fredr_series_observations(series_id = "NAEXKP02GBQ189S") 
Investment <-fredr_series_observations(series_id = "NAEXKP04GBQ652S") 

#merge and rename imported data
All_data <- as.data.frame(rbind(RGDP,
                                Consumption,
                                Investment)) %>% 
  pivot_wider(names_from = "series_id", values_from = "value") %>%
  rename("RGDP" = GBRRGDPQDSNAQ,
         "Consumption" = NAEXKP02GBQ189S,
         "Investment" = NAEXKP04GBQ652S)

# data manipulation -------------------------------------------------------

#take the log of the macro data 
#use HP filter to identify trend RGDP
macro_plot_data <- All_data %>% 
  filter(date > "1995-01-01") %>%
  mutate_at(vars(c(RGDP:Investment)), .funs = list(log = ~ log(.))) %>% 
  mutate(trend_GDP = hpfilter(RGDP_log, freq = 1600, type = "lambda")$trend) %>% 
  select(-c(realtime_start:Investment)) %>% 
  melt(id.vars = "date") 

#take LOG GDP and compute variance from trend as per hp filter
#calculation is separate so that the plot can be separated
Divergance_plot_data <- All_data %>% 
  filter(date > "1995-01-01") %>%
  mutate(RGDP_log = log(RGDP),
         divergance_from_trend = hpfilter(RGDP_log, freq = 1600, type = "lambda")$cycle) %>% 
  select(c(date, divergance_from_trend)) 

# plots -------------------------------------------------------------------

#plot the macro data by type
macro_data_plot <- ggplot(macro_plot_data) + geom_line(mapping = aes(x=date,y=value, colour = variable)) +
  ggtitle("Log macro data US 1995 - 2020") + 
  labs(x = "Year",
       y = "Log value",
       colour = "Variable") +
  theme_bw() +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y (%b)")

#we want to position the legend flexibly, so call as object with special function
legend <- get_legend(macro_data_plot)

#remove legend from source to avoid duplication
macro_data_plot <- macro_data_plot + theme(legend.position = "none") 

#plot divergence
Divergance_plot <- ggplot(Divergance_plot_data) + geom_line(mapping = aes(x=date,y=divergance_from_trend)) +
  ggtitle("Divergance of log RGDP from trend") + 
  labs(x = "Year",
       y = "Divergance") +
  theme_bw() +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y (%b)")

#merge plots and arrange in grid
grid.arrange(macro_data_plot,Divergance_plot, legend, 
             layout_matrix = rbind(c(1,1,3), c(2,2,3)))





