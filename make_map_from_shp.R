setwd("/home/seth/workspace_Unix/dataIncub/bbp/shape_files")
# http://www.kevjohnson.org/making-maps-in-r/
library(ggplot2)
library(rgdal)
library(scales)
library(ggmap)
library(dplyr)
library(Cairo)
library(maptools)

# http://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

tract <- readOGR(dsn = paste(getwd(),'/CA', sep = ''), layer = "cb_2014_06_tract_500k")
tract <- fortify(tract, region="tract")
tract$tract_num=as.numeric(tract$TRACTCE)

data <- read.csv("/home/seth/workspace_Unix/dataIncub/bbp/census_stateDF1.csv", stringsAsFactors = FALSE)
data$LAND_AREA_NUM=as.numeric(data$LAND_AREA)
data$Tot_Population_ACS_09_13_NUM=as.numeric(data$Tot_Population_ACS_09_13)
data$pop_density=data$Tot_Population_ACS_09_13_NUM/data$LAND_AREA_NUM
data_plot <- data[,c("tract", "pop_density")]
# colnames(data) <- c("id", "percent")
# data$id <- as.character(data$id)
# data$percent <- data$percent/100
# 
# data$id <- paste("1400000US", data$id, sep = "")
plotData <- left_join(tract, data_plot, by.x='TRACTCE')



p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", labels = percent,
                       breaks = pretty_breaks(n = 10), values = c(1,0)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Percentage of Population Without\nHealth Insurance",
       fill = "")
ggsave(p, file = "map6.png", width = 5, height = 4.5, type = "cairo-png")
