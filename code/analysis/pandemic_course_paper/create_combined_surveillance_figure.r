# This code creates the fig:combined_surveillance_figure
# chart for COVID-19 pandemic in Saint Petersburg, Russia
# paper
library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(scales)
library(ggthemes)
library(ggsci)
library(ggpubr)
library(dplyr)
library(tidyr)
library(readxl)
library(grid)
library(gridExtra)
library(ggplot2)
library(lattice)
library(zoo)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Define start and end dates for all charts
start_date <- ymd("2020-03-01")
end_date <- ymd("2021-05-31")
start_end_range <- c(start_date, end_date)

#######
# Read in the data

# Monthly excess mortality from https://doi.org/10.1111/1740-9713.01486
excess_deaths <- fread("https://raw.githubusercontent.com/dkobak/excess-mortality/main/russia_excess_deaths.csv", encoding = "UTF-8")[ V1 == "Санкт-Петербург"]
excess_deaths <- melt(excess_deaths, id.vars = "V1", variable.name = "DATE", value.name = "deaths", variable.factor = F)
excess_deaths[, V1 := NULL]
excess_deaths[, DATE := gsub("Январь ", "25.01.", DATE)]
excess_deaths[, DATE := gsub("Февраль ", "25.02.", DATE)]
excess_deaths[, DATE := gsub("Март ", "25.03.", DATE)]
excess_deaths[, DATE := gsub("Апрель ", "25.04.", DATE)]
excess_deaths[, DATE := gsub("Май ", "25.05.", DATE)]
excess_deaths[, DATE := gsub("Июнь ", "25.06.", DATE)]
excess_deaths[, DATE := gsub("Июль ", "25.07.", DATE)]
excess_deaths[, DATE := gsub("Август ", "25.08.", DATE)]
excess_deaths[, DATE := gsub("Сентябрь ", "25.09.", DATE)]
excess_deaths[, DATE := gsub("Октябрь ", "25.10.", DATE)]
excess_deaths[, DATE := gsub("Ноябрь ", "25.11.", DATE)]
excess_deaths[, DATE := gsub("Декабрь ", "25.12.", DATE)]
excess_deaths[, DATE := dmy(DATE)]

# Manually add May, 2021 data as missing
excess_deaths <- rbind(excess_deaths, data.table(DATE = ymd("2021-05-01")), fill = T)
setorderv(excess_deaths, "DATE")
excess_deaths <- excess_deaths[ DATE >= start_date & DATE <= end_date ]

# Interpolate monthly deaths to week
zoo.mo <- zoo(excess_deaths$deaths/4.345, excess_deaths$DATE)

zoo.da <- merge(zoo.mo, zoo(, seq(start(zoo.mo), end(zoo.mo)+15, by="day")))
zoo.da <- na.approx(zoo.da)
zoo.we <- rollapply(zoo.da, 7, mean, by=7)

edeaths <- fortify.zoo(zoo.we) %>%
	rename(DATE=Index,count=zoo.we) %>%
	mutate(source="Excess deaths (interpolated)") %>% select(DATE, source, count) 

# Search trends
search_trends <- fread("https://raw.githubusercontent.com/alexei-kouprianov/COVID-19.SPb/main/data/primary/covid.SPb.yandex.weekly.txt", encoding = "UTF-8")
search_trends <- search_trends[, c("START.DATE.LOSS.01", "COUNT.LOSS.01")]
setnames(search_trends, c("START.DATE.LOSS.01", "COUNT.LOSS.01"), c("DATE", "weekly_count"))
search_trends[, DATE := dmy(DATE)]

search_trends<-search_trends %>% 
	mutate(source="Search trends") %>%
	rename(count=weekly_count) %>%
	select(DATE, source, count)

# SARS-Cov-2 Variants
variants3 <- read_excel("data/variants/variants_count_by_months.xlsx") %>% 
	pivot_longer(-month, names_to = "variant", values_to = "n") %>%
	group_by(month) %>%
	mutate(n0 = sum(n)) %>%
	mutate(proportion = round ((n / n0), 2)) %>%
	ungroup() %>%
	select(month,variant,proportion)

# Seroprevalence by cross-section
# Load and prepare the estimated prevalence across waves
# (see the estimation code in analysis/wave*/estimate_prevalence_by_{model,variable}.r)
waves <- c(1, 1.5, 2, 3)

# Init an object to store the results
# of prevalence computations
perwave_prevalence_by_model_list <- list()

# Load the per-wave estimates and store them in one object
for(w in waves) {

	load(paste0("estimates/wave", w, "/prevalence_by_model.rdata"))

	perwave_prevalence_by_model_list[[ as.character(w) ]] <- prevalence_by_model

	rm(prevalence_by_model)

}

# Aggregate the results
perwave_prevalence_by_model <- rbindlist(perwave_prevalence_by_model_list, idcol = "wave")

# Define a variable for test adjustment
perwave_prevalence_by_model[ sensitivity == 1 & specificity == 1, test_adjustment := "unadjusted" ]
perwave_prevalence_by_model[ sensitivity != 1 & specificity == 1, test_adjustment := "adjusted" ]

# Perwave prevalence data from the univariate model
perwave_prevalence <- perwave_prevalence_by_model[ scenario == "basic" & test_adjustment == "adjusted" & serotest == "IgA_or_G_or_M_testC" & prevalence_type == "univariate" & surveyweight == "no", c("wave", "lowerbound", "pointest", "upperbound") ]

# Repeat the rows a set amount of times
perwave_prevalence <- rbind(perwave_prevalence[1][rep(1:.N, 5), ],
							perwave_prevalence[2][rep(1:.N, 4), ],
							perwave_prevalence[3][rep(1:.N, 8), ],
							perwave_prevalence[4][rep(1:.N, 8), ]
							)
perwave_prevalence[, DATE := c("2020-05-31", "2020-06-07", "2020-06-14", "2020-06-21", "2020-06-28", 
			"2020-07-26", "2020-08-02", "2020-08-09", "2020-08-16", 
			"2020-10-18", "2020-10-25", "2020-11-01","2020-11-08", "2020-11-15", "2020-11-22", "2020-11-29", "2020-12-06",
			"2021-02-14", "2021-02-21", "2021-02-28", "2021-03-07", "2021-03-14", "2021-03-21", "2021-03-28", "2021-04-04")]
perwave_prevalence[, DATE := ymd(DATE)]
perwave_prevalence[, source := "Vaccination uptake and serosurvey prevalence"]
perwave_prevalence[, count := pointest/100]
perwave_prevalence[, lab := NA_character_ ]
perwave_prevalence[DATE == ymd("2020-05-31"), lab := "9.7 (7.7-11.7)" ]
perwave_prevalence[DATE == ymd("2020-07-26"), lab := "13.3 (9.9-16.6)" ]
perwave_prevalence[DATE == ymd("2020-11-01"), lab := "22.9 (20.3-25.5)" ]
perwave_prevalence[DATE == ymd("2021-02-28"), lab := "43.9 (39.7-48.0)" ]

perwave_prevalence$source <- factor(perwave_prevalence$source,levels =c("Officially registered cases",
												"Tests performed", 
												"Hospitalized cases", 
												"COVID-19 deaths", 
												"Excess deaths (interpolated)",
												"Search trends",
												"Urban activity",
												"Vaccination uptake and serosurvey prevalence"))

# Weekly data
weekly <- fread("https://raw.githubusercontent.com/alexei-kouprianov/COVID-19.SPb/main/data/derived/spb.combined.weekly.txt", 
				encoding = "UTF-8") %>% 
	select(TIME, HOSPITALIZED.today, CONFIRMED, DEATHS, PCR.tested, Yandex.ACTIVITY.points, v1.CS) %>% 
	mutate(v1.CS=round((v1.CS/5398064),3)) %>%
	rename("Hospitalized cases"=HOSPITALIZED.today,
			"Officially registered cases"= CONFIRMED,
			"Tests performed"=PCR.tested,
			"COVID-19 deaths"=DEATHS,
			"Urban activity"= Yandex.ACTIVITY.points,
			"Vaccination uptake and serosurvey prevalence"=v1.CS) %>%
	pivot_longer(-TIME,names_to="source",values_to = "count") %>% 
	mutate(TIME=as.Date(TIME)) %>%
	rename(DATE=TIME) %>% 
	filter(DATE >= start_date & DATE <= end_date) %>% 
	rbind(edeaths) %>%
	rbind(search_trends) 

weekly$source <- factor(weekly$source,levels =c("Officially registered cases",
								"Tests performed", 
								"Hospitalized cases", 
								"COVID-19 deaths", 
								"Excess deaths (interpolated)",
								"Search trends",
								"Urban activity",
								"Vaccination uptake and serosurvey prevalence"))

# Plot witn main pandemic data
plot_main <- ggplot(data = weekly, aes(x = DATE, y = count, fill=source, color=source)) +
											geom_col(position = "dodge",width=7) +
											scale_y_continuous(labels=comma_format())+
	geom_col(data=perwave_prevalence, position = "dodge",width=6.7,fill="blue",color=NA,alpha=0.2) +
	geom_text(data=perwave_prevalence, aes(label=lab),size=2.4,fontface="bold", color="black",hjust=0.3, vjust=0.8) +
	scale_x_date(	name = "",
					breaks = date_breaks("2 months"),
					#date_labels = "%m-\n%Y",
					date_labels= c("Mar\n2020", "May",
									"Jul", "Sep",
									"Nov", "Jan\n2021",
									"Mar", "May"),
					expand = c(0, 0),
					limits = start_end_range) +
	theme_minimal() + 
	facet_wrap(~source, ncol=1,scales="free_y")+
	scale_fill_lancet() +
	scale_color_lancet() +
	theme(panel.grid.minor = element_blank(), 
			legend.position = "none")+
	ylab("")+
	ggtitle('A')

# Plot witn SARS-CoV-2 variants
plot_var <- ggplot(data = variants3, aes(x = month, y = proportion)) +
	geom_col(position = "dodge",color=NA,fill="gray") +
	scale_y_continuous(labels = scales::percent_format())+
	scale_x_discrete(name = "",
					breaks = c("01april","03jun","05aug","07oct","09dec",
								"11feb","13april2","15jun2"),
					labels = c("Apr\n2020","Jun",
								"Aug","Oct",
								"Dec", "Feb\n2021",
								"Apr","Jun")) +
	theme_minimal() +	
	facet_wrap(~variant,ncol=1)+
	scale_fill_lancet()+
	scale_color_lancet()+
	theme(panel.grid.minor = element_blank(), 
		legend.position = "none")+
	ylab("")+
	ggtitle('B')

# Combine the two charts under one figure
#pdf("pandemic_course_paper/media/combined_surveillance_figure.pdf", width = 14, height = 12)
#grid.arrange(plot_main, plot_var, ncol = 2, widths = c(2,1))
#dev.off() 
