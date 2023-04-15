###For 2004

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(rgeos)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("C:/Users/milad/OneDrive/Desktop/Datasets/World Map/Afghanistan Plotting")
getwd()

#Creating a new dataframe of ethnic demographics
library(sf)
AFG <- read_sf("AFG_adm1.shp")
View(AFG)
AFG %>% 
  mutate(Pashtuns = Sepal.Length/Petal.Length)


#read in data table

AFG_Violence = read.csv("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2004")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)






AFG_Violence %>%
  group_by(adm_1) %>%
  summarize(casualties=sum(high,na.rm=TRUE)) %>%
  ungroup() -> casProv
casProv



#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")






#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")





###For 2001

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2001")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")






###For 2002

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2002")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2002")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")





###For 2003

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2003")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2003")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")








###For 2004

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2004")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2004")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")




###For 2006

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2006")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2006")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")






###For 2008

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2008")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2008")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")












####For 2012
#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2012")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2012")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")




####For 2010
#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2010")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2010")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")





















####For 2014
#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2014")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2014")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")
view(AFG_Violence)





####For 2016
#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2016")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2016")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")
view(AFG_Violence)













####For 2018
#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2018")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2018")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")
View(AFG_Violence)













####For 2020
#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
AFG_Violence = fread("ged211.csv")
#convert to data table
AFG_Violence = as.data.table(AFG_Violence)
AFG_Violence <- subset(AFG_Violence, year == "2020")
AFG_Violence <- subset(AFG_Violence, country == "Afghanistan")
View(AFG_Violence)
#make data spatial
coordinates(AFG_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(AFG_Violence) = crs.geo1  

plot(AFG_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
AFG = readOGR(dsn = "./AFG_adm", layer = "AFG_adm1") #name of file and object
dev.new()
plot(AFG)
points(AFG_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
qpal = colorBin("Reds", AFG_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="Number of Battles in 2020")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")
view(AFG_Violence)























###For Iraq 2004

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2004")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(IRQ_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")





###For Iraq 2005

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2005")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")











###For Iraq 2006

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2004")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")









###For Iraq 2007

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2007")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")








###For Iraq 2008

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2008")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")











###For Iraq 2009

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2009")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")








###For Iraq 2010

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2010")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")












###For Iraq 2011

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2011")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")









###For Iraq 2012

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2012")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")












###For Iraq 2013

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2013")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")













###For Iraq 2014

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2014")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")








###For Iraq 2015

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2015")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")










###For Iraq 2016

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2016")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")









###For Iraq 2017

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2017")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")










###For Iraq 2018

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2018")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")






###For Iraq 2019

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2019")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")









###For Iraq 2004

#Choropleth Maps Using Leafleft in R
#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)
getwd()
setwd("D:/Datasets/World Map/Afghanistan Plotting")
getwd()
#read in data table
IRQ_Violence = fread("ged211.csv")
#convert to data table
IRQ_Violence = as.data.table(IRQ_Violence)
IRQ_Violence <- subset(IRQ_Violence, year == "2020")
IRQ_Violence <- subset(IRQ_Violence, country == "Iraq")
View(IRQ_Violence)
#make data spatial
coordinates(IRQ_Violence) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(IRQ_Violence) = crs.geo1  

plot(IRQ_Violence, pch = 20, col = "steelblue")
#read in shapefile of Afghanistan
IRQ = readOGR(dsn = "./IRQ_adm", layer = "IRQ_adm1") #name of file and object
dev.new()
plot(IRQ)
points(IRQ_Violence, pch = 20, col = "red")
#leaflet map -- # of incidents
#reproject coordinates
proj4string(IRQ) = crs.geo1
IRQ_AGG = aggregate(x=IRQ_Violence["best"],by=IRQ,FUN=length)
qpal = colorBin("Reds", IRQ_AGG$best, bins=4)
leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1) %>%
  addLegend(values=~best,pal=qpal,title="best")

#Another Way of Plotting
proj4string(AFG) = crs.geo1
AFG_AGG = aggregate(x=AFG_Violence["best"],by=AFG,FUN=length)
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", AFG_AGG$best, bins=bins)
labels <- sprintf(
  "<strong>%s</strong>",
  AFG_AGG$best
) %>% lapply(htmltools::HTML)

leaflet(AFG_AGG) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(best),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~best,pal=qpal,title="Casualties")
