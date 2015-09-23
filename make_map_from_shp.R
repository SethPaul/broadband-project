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
tract <- fortify(tract, region="TRACTCE")
# tract$tract_num=as.numeric(tract$data$TRACTCE)

data <- read.csv("/home/seth/workspace_Unix/dataIncub/bbp/census_stateDF1.csv", stringsAsFactors = FALSE)
data$LAND_AREA_NUM=as.numeric(data$LAND_AREA)
data$Tot_Population_ACS_09_13_NUM=as.numeric(data$Tot_Population_ACS_09_13)
data$pop_density=data$Tot_Population_ACS_09_13_NUM/data$LAND_AREA_NUM
data$id <- paste("1400000US", data$id, sep = "")
data_plot <- data[,c("tract", "pop_density")]
# colnames(data) <- c("id", "percent")
# data$id <- as.character(data$id)
# data$percent <- data$percent/100
# 
# data$id <- paste("1400000US", data$id, sep = "")
tract_plot=tract
tract_plot$pop_density=data_plot[match(as.numeric(tract_plot$id),data_plot$tract),'pop_density']

county <- readOGR(dsn = paste(getwd(),'/CA', sep = ''), layer = "cb_2014_06_tract_500k")
county <- fortify(county, region="COUNTYFP")

p <- ggplot() +
  geom_polygon(data = tract_plot, aes(x = long, y = lat, group = group,
                                    fill = pop_density)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "white", size = 0.05) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", labels=comma,name='Persons per square mile',
                       breaks = pretty_breaks(n = 10), values = c(1,0)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Population Density in California\n from Census 2014 estimates",
       fill = "")
ggsave(p, file = "pop_CA.png", width = 5, height = 4.5, type = "cairo-png")



ind_data <- read.csv("/home/seth/workspace_Unix/dataIncub/bbp/CA_industryDF.csv", stringsAsFactors = FALSE)

emp_per_county=aggregate(ind_data$EMP,by=list(ind_data$COUNTY), function(x) sum(as.numeric(x), na.rm =TRUE) )
colnames(emp_per_county)=c('countyNum', 'emp_per_county')
emp_per_county$LAND_AREA_NUM=data[match(emp_per_county$countyNum,data$County),'LAND_AREA_NUM']
emp_per_county$emp_density=emp_per_county$emp_per_county/emp_per_county$LAND_AREA_NUM


county_plot=county
county_plot$emp_density=emp_per_county[match(as.numeric(county_plot$id),emp_per_county$countyNum),'emp_per_county']

p2 <- ggplot() +
  geom_polygon(data = county_plot, aes(x = long, y = lat, group = group,
                                      fill = emp_density)) +
  geom_polygon(data = county_plot, aes(x = long, y = lat, group = group),
               fill = NA, color = "white", size = 0.05) +
  coord_map() +
  scale_fill_distiller(palette = "PuRd", labels=comma,name='Employees per square mile',
                       breaks = pretty_breaks(n = 10), values = c(1,0)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Employee Density in California\n from Econ 2012",
       fill = "")
ggsave(p2, file = "emp_CA.png", width = 5, height = 4.5, type = "cairo-png")


speed_data <- read.csv("/home/seth/workspace_Unix/dataIncub/bbp/speed_by_county.csv", stringsAsFactors = FALSE)
speed_data_CA=speed_data[(speed_data$geographyId>6000 & speed_data$geographyId<7000),]
speed_data_CA$countyNum=speed_data_CA$geographyId%%1000
speed_data_CA_business=speed_data_CA[speed_data_CA$accessingFrom=='Business',]
speed_data_CA_home=speed_data_CA[speed_data_CA$accessingFrom=='Home',]
speed_data_CA_mobile=speed_data_CA[speed_data_CA$accessingFrom=='Mobile',]
county_plot=county
county_plot$business_speed=speed_data_CA_business[match(as.numeric(county_plot$id),speed_data_CA_business$countyNum),'medianDownload']

p3 <- ggplot() +
  geom_polygon(data = county_plot, aes(x = long, y = lat, group = group,
                                       fill = business_speed)) +
  coord_map() +
  scale_fill_distiller(palette = "Blues", labels=comma,name='Download Speeds (Mbps)',
                       breaks = pretty_breaks(n = 10), values = c(1,0)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Median Download Speeds in California\n from BroadbandMap.gov",
       fill = "")
ggsave(p3, file = "speed_CA.png", width = 5, height = 4.5, type = "cairo-png")

tot_emp=sum(ind_data$EMP)
#yolo County
ind_data113=ind_data[ind_data$COUNTY==113,]
tot_emp113=sum(ind_data113$EMP)
ind_group_113=aggregate(ind_data113$EMP,by=list(ind_data113$Industry_Group), function(x) sum(as.numeric(x), na.rm =TRUE) )
colnames(ind_group_113)=c('IndustryGroups', 'emp_per_group') 
ind_group_113$emp_prop=ind_group_113$emp_per_group/tot_emp113

p4=ggplot(data=ind_group_113, aes(x=IndustryGroups, y=emp_prop, fill=IndustryGroups)) +
  theme_classic() + 
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  labs(title = 'Worker Proportions based on Industry Group in Yolo County', x = "Industry Groups", y = "Proportion of all employees in industry group")
  # scale_fill_manual(values=c("#999999", "#E69F00"))

ggsave(p4, file = "emp_prop_Yolo.png", width = 10, height = 7, type = "cairo-png")

#orange County
ind_data59=ind_data[ind_data$COUNTY==59,]
tot_emp59=sum(ind_data59$EMP)
ind_group_59=aggregate(ind_data59$EMP,by=list(ind_data59$Industry_Group), function(x) sum(as.numeric(x), na.rm =TRUE) )
colnames(ind_group_59)=c('IndustryGroups', 'emp_per_group') 
ind_group_59$emp_prop=ind_group_59$emp_per_group/tot_emp59

p5=ggplot(data=ind_group_59, aes(x=IndustryGroups, y=emp_prop, fill=IndustryGroups)) +
  theme_classic() + 
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  labs(title = 'Worker Proportions based on Industry Group in Orange County', x = "Industry Groups", y = "Proportion of all employees in industry group")

ggsave(p5, file = "emp_prop_Orange.png", width = 10, height = 7, type = "cairo-png")

#alemeda County
ind_data1=ind_data[ind_data$COUNTY==1,]
tot_emp1=sum(ind_data1$EMP)
ind_group_1=aggregate(ind_data1$EMP,by=list(ind_data1$Industry_Group), function(x) sum(as.numeric(x), na.rm =TRUE) )
colnames(ind_group_1)=c('IndustryGroups', 'emp_per_group') 
ind_group_1$emp_prop=ind_group_1$emp_per_group/tot_emp1

p6=ggplot(data=ind_group_1, aes(x=IndustryGroups, y=emp_prop, fill=IndustryGroups)) +
  theme_classic() + 
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  labs(title = 'Worker Proportions based on Industry Group in Alemeda County', x = "Industry Groups", y = "Proportion of all employees in industry group")

ggsave(p6, file = "emp_prop_Alemeda.png", width = 10, height = 7, type = "cairo-png")


emp_per_county=aggregate(ind_data$EMP,by=list(ind_data$COUNTY), function(x) sum(as.numeric(x), na.rm =TRUE) )