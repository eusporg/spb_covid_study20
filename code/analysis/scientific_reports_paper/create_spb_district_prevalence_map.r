# This code creates the seroprevalence 
# map by city district for
# Seroprevalence of SARS-CoV-2 antibodies in Saint
# Petersburg, Russia paper (https://doi.org/10.1038/s41598-021-92206-y)
library(data.table)
library(ggplot2)
library(ggrepel)
library(scales)
library(ggthemes)
library(ggspatial)
library(sf)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load the per-group prevalence computed by estimate_prevalence_by_variable.r
load("estimates/wave1/prevalence_by_variable_level.rdata")

# Prevalence lower bounds that cross zero to zero
prevalence_by_variable_level[lowerbound < 0, lowerbound := 0 ]

# Keep only univariate prevalence from Abbott test
prevalence_by_district <- prevalence_by_variable_level[ prevalence_type == "univariate" & serotest == "IgG_testB" & variable == "district" & surveyweight == "no" & sensitivity != 1]
setnames(prevalence_by_district, "variable_level", "district")

# Load the district boundaries shapefile
# from NextGIS-processed OSM data
spb_boundaries <- st_read("data/spb_map/spb_district_boundaries.shp")

# Convert to metric coordinate reference system
# We will use Pulkovo 1995 / Gauss-Kruger zone 6 (https://epsg.io/20006)
spb_boundaries <- st_transform(spb_boundaries, crs = 20006)

# Translate district names
prevalence_by_district[ district == "Admiralteyskiy District", NAME := "Адмиралтейский район"]
prevalence_by_district[ district == "Frunzenskiy District", NAME := "Фрунзенский район"]
prevalence_by_district[ district == "Kalininskiy District", NAME := "Калининский район"]
prevalence_by_district[ district == "Kirovskiy District", NAME := "Кировский район"]
prevalence_by_district[ district == "Krasnogvardeyskiy District", NAME := "Красногвардейский район"]
prevalence_by_district[ district == "Moskovskiy District", NAME := "Московский район"]
prevalence_by_district[ district == "Nevskiy District", NAME := "Невский район"]
prevalence_by_district[ district == "Petrogradskiy District", NAME := "Петроградский район"]
prevalence_by_district[ district == "Primorskiy District", NAME := "Приморский район"]
prevalence_by_district[ district == "Tsentralniy District", NAME := "Центральный район"]
prevalence_by_district[ district == "Vasileostrovskiy District", NAME := "Василеостровский район"]
prevalence_by_district[ district == "Vyborgskiy District", NAME := "Выборгский район"]

# Add prevalence to the data
spb_boundaries <- merge(spb_boundaries, prevalence_by_district, by = "NAME", all.x = T, all.y = F)

# Test site location
test_site <- st_as_sf(data.table(lat = 59.9049557, lon = 30.3179625), coords = c("lon", "lat"), crs = 4326)
test_site <- st_transform(test_site, crs = 20006)

# Initiate the plot
theme_set(theme_bw())

prev_distr_plot <- ggplot(spb_boundaries) +
						geom_sf(aes(fill = pointest)) +
						scale_fill_viridis_c(na.value = "grey90", option = "magma", direction = -1) +
						geom_sf(data = test_site, size = 4, shape = 21, fill = "green") +
						geom_label_repel(data = spb_boundaries[!is.na(spb_boundaries$pointest),], aes(label = paste0(pointest, "\n(", lowerbound, "; ", upperbound, ")"), geometry = geometry), size = 2, stat = "sf_coordinates", min.segment.length = 0, segment.size = 0.25, segment.alpha = 0.8, max.iter = 5000) +
						xlab("") + ylab("") +
					    labs(title = "", caption = "", fill = "Prevalence, %") +
						annotation_scale(location = "bl", width_hint = 0.3) +
						theme(	panel.grid.major = element_blank(),
								panel.background = element_rect(fill = "white"),
								panel.border = element_blank(),
								axis.title.x = element_blank(),
								axis.text.x = element_blank(),
								axis.ticks.x = element_blank(),
								axis.title.y = element_blank(),
								axis.text.y = element_blank(),
								axis.ticks.y = element_blank(),
								legend.position = c(0.07,.2)
							)

#ggsave(prev_distr_plot, file = "media/wave1/prevalence_estimates_by_district.pdf", scale = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        