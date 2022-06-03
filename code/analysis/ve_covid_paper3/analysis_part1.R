# This code creates descriptive table and graph describing the data 
# on the Vaccine effectiveness against lung injury associated with COVID-19
# during delta and omicron variant surges

library(data.table)
library(lubridate)
library(stringr)
library(tidyverse)
library(tableone)
library(xtable)
library(lmtest)
options(scipen = 999) 

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

load("data/ve_ct_paper3/trc_dt_all.Rdata")

trc_dt <- trc_dt_all %>% 
  filter(is.na(past_inf)) %>%
  filter(is.na(pcr_in_pr)) %>%
  filter(is.na(pcr_2)) %>%
  filter(pcr==1)

# Table 1
myVars <- c( "age",  
             "age_group", 
             "sex",  
             "vaccine_status",
             "voc",
             "source")

# Vector of categorical variables 
catVars <- c( "age_group", 
              "sex",  
              "vaccine_status",
              "voc",
              "source")

table_1 <- CreateTableOne(vars = myVars, factorVars = catVars, 
                               data = trc_dt, 
                               strata = "ct_score",
                               test = FALSE, 
                               includeNA = TRUE,
                               addOverall=TRUE )

# print table 
# xtable(print(table_1,  showAllLevels = T))

#=========== GRAPH
#============ total (vaccinated and unvaccinated)
try_pcr <- trc_dt

try_pcr$week <- week(try_pcr$date)
try_pcr$ct_abn <- as.integer(try_pcr$ct_simple)-1

try_pcr[which(try_pcr$week<10),]$week <- try_pcr[which(try_pcr$week<10),]$week+52

graph_data <- try_pcr

temp <- subset(graph_data, select=c('date'))

dates <- unique(temp)

dates$shared <- 1
dates$n <- 1
for (d in dates$date) {
  dates[dates$date==d,]$shared <- mean(graph_data[which(graph_data$date==d),]$ct_abn)
  dates[dates$date==d,]$n <- nrow(graph_data[which(graph_data$date==d),])
}

dates <- dates[order(dates$date),]
dates$share7 <- rollmean(dates$shared, 7, na.pad=TRUE)
dates$num7 <- rollmean(dates$n, 7, na.pad=TRUE)

dates$share7 <- dates$share7*100+100
dates$shared <- dates$shared*100

figure2 <- ggplot(dates, aes(date, share7)) + 
  geom_area(aes(x=date, y=num7), fill='gray') +
  geom_line(aes(y = 2.5*share7)) + 
  geom_vline (xintercept = as.Date("2022-01-09"), linetype="dotted") +
  xlab('Date') +
  ylab('Number of triaged patients by day (gray area)') +
  scale_y_continuous(sec.axis = sec_axis(~(. - 0)*(1/2.50), name="Patients with lung injury, % (black line)")
  ) +
  theme( axis.title.y.left = element_text(angle = 90))+
  scale_x_date(date_breaks = "1 month", date_labels = '%b, 1') +
  theme_bw()

# figure2