library(zoo)
library(raster)
library(DescTools)
library(rgdal)
library(sf)
library(ggplot2)
library(ggthemes)
library(reshape)

recent <- c(0, 30, 365, 365*2, 365*3, 365*4, 5*365, 365*7, 365*10, 365*15, 365*20)

#############################################################################
######## Tatton #############################################################
#############################################################################

raster_list <- paste("meta_habitat_suit_", "tatton", "_", recent,"_new.tif", sep ="")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
raster_stack <- stack(raster_list)
y <- data.frame( rasterToPoints(raster_stack) )
y <- y[,3:length(y)]

id <- order(recent)

out <- matrix(ncol = length(id), nrow = dim(y)[1], data = NA)
for(i in 3:(length(id))){
print(i)
  for(s in 1:dim(y)[1]){
AUC_value <- DescTools::AUC(recent[1:i], y[s,1:i])
out[s,i] <- AUC_value/recent[i]
}
}

# find max value by row, identify which time interval corresponds to that column
out2 <- as.data.frame(out)
names(out2) <- recent/365
out2 <- out2[,3:11]

head(out2)
out2$diff <- apply(out2, 1, max, na.rm=TRUE) - apply(out2, 1, min, na.rm=TRUE)
out2$Largest_Column <- colnames(out2)[apply(out2,1,which.max)]
out2$Largest_Column[out2$diff < 0.02] <- 0

r <- raster_stack[[1]]
r[!is.na(r)] <- as.numeric(as.character(out2$Largest_Column))
writeRaster(r, "meta_tatton_optimal_fire_frequency.tiff", format="GTiff", overwrite=TRUE)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
tatton <- readOGR(dsn=getwd(), layer="tatton_fire_units")

Mode1 <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#extract raster cell count (sum) within each polygon area (poly)

ex <- raster::extract(r, tatton,  df=TRUE)
names(ex)[2] <- "value"
tatton$fire_freq <- tapply(ex$value, list(ex$ID), Mode1)

plot(tatton, col = tatton$fire_freq)

r <- raster_stack[[1]]
r[!is.na(r)] <- out2$diff

ex <- raster::extract(r, tatton,  df=TRUE)
names(ex)[2] <- "value"
mean1 <- function(x) {
  x <- na.omit(x)
  mean(x)
}

tatton$range <- tapply(ex$value, list(ex$ID), mean1)

plot(tatton, col = tatton$range*100)

st_write(st_as_sf(tatton), paste0(getwd(), "/", "meta_tatton_fire_freq_optimal.shp"), delete_layer = TRUE) 



ex2 <- raster::extract(raster_stack, tatton,  df=TRUE)
names(ex2) <- c("ID", recent/365)
ex2$site <- paste("tatton", ex2$ID, sep="_")

#################################################################################
#### Matagorda ##################################################################
#################################################################################

raster_list <- paste("meta_habitat_suit_","matagorda", "_", recent,"_new.tif", sep ="")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
raster_stack <- stack(raster_list)
y <- data.frame( rasterToPoints(raster_stack) )
y <- y[,3:length(y)]

id <- order(recent)

out <- matrix(ncol = length(id), nrow = dim(y)[1], data = NA)
for(i in 3:(length(id))){
  print(i)
  for(s in 1:dim(y)[1]){
    AUC_value <- DescTools::AUC(recent[1:i], y[s,1:i])
    out[s,i] <- AUC_value/recent[i]
  }
}

# find max value by row, identify which time interval corresponds to that column
out2 <- as.data.frame(out)
names(out2) <- recent/365
out2 <- out2[,3:11]

head(out2)
out2$diff <- apply(out2, 1, max, na.rm=TRUE) - apply(out2, 1, min, na.rm=TRUE)
out2$Largest_Column <- colnames(out2)[apply(out2,1,which.max)]
out2$Largest_Column[out2$diff < 0.02] <- 0

r <- raster_stack[[1]]
r[!is.na(r)] <- as.numeric(as.character(out2$Largest_Column))
writeRaster(r, "meta_matagorda_optimal_fire_frequency.tiff", format="GTiff", overwrite=TRUE)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
matagorda <- readOGR(dsn=getwd(), layer="matagorda_fire_units")

Mode1 <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#extract raster cell count (sum) within each polygon area (poly)

ex <- raster::extract(r, matagorda,  df=TRUE)
names(ex)[2] <- "value"
matagorda$fire_freq <- tapply(ex$value, list(ex$ID), Mode1)

plot(matagorda, col = matagorda$fire_freq*100)

r <- raster_stack[[1]]
r[!is.na(r)] <- out2$diff

ex <- raster::extract(r, matagorda,  df=TRUE)
names(ex)[2] <- "value"
mean1 <- function(x) {
  x <- na.omit(x)
  mean(x)
}

matagorda$range <- tapply(ex$value, list(ex$ID), mean1)

plot(matagorda, col = matagorda$range*100)

st_write(st_as_sf(matagorda), paste0(getwd(), "/", "meta_matagorda_fire_freq_optimal.shp"), delete_layer = TRUE) 

ex3 <- raster::extract(raster_stack, matagorda,  df=TRUE)
names(ex3) <- c("ID", recent/365)
ex3$site <- paste("matagorda", ex3$ID, sep="_")

#################################################################################
#### aransas south ##################################################################
#################################################################################

raster_list <- paste("habitat_suit_aransas_south", "_", recent,"_new.tif", sep ="")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
raster_stack <- stack(raster_list)
y <- data.frame( rasterToPoints(raster_stack) )
y <- y[,3:length(y)]

out <- matrix(ncol = length(id), nrow = dim(y)[1], data = NA)
for(i in 3:(length(id))){
  print(i)
  for(s in 1:dim(y)[1]){
    AUC_value <- DescTools::AUC(recent[1:i], y[s,1:i])
    out[s,i] <- AUC_value/recent[i]
  }
}

# find max value by row, identify which time interval corresponds to that column
out2 <- as.data.frame(out)
names(out2) <- recent/365
out2 <- out2[,3:11]

head(out2)
out2$diff <- apply(out2, 1, max, na.rm=TRUE) - apply(out2, 1, min, na.rm=TRUE)
out2$Largest_Column <- colnames(out2)[apply(out2,1,which.max)]
out2$Largest_Column[out2$diff < 0.02] <- 0

r <- raster_stack[[1]]
r[!is.na(r)] <- out2$Largest_Column
writeRaster(r, "meta_south_aransas_optimal_fire_frequency_new2.tiff", format="GTiff", overwrite=TRUE)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
south_aransas <- readOGR(dsn=getwd(), layer="Aransas_south")

Mode1 <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#extract raster cell count (sum) within each polygon area (poly)

ex <- raster::extract(r, south_aransas,  df=TRUE)
names(ex)[2] <- "value"
south_aransas$fire_freq <- tapply(ex$value, list(ex$ID), Mode1)
south_aransas$fire_freq[is.na(south_aransas$fire_freq)] <- 0

plot(south_aransas, col = south_aransas$fire_freq)

r <- raster_stack[[1]]
r[!is.na(r)] <- out2$diff



ex <- raster::extract(r, south_aransas,  df=TRUE)
names(ex)[2] <- "value"
ex$value[is.na(ex$value)] <- 0

mean1 <- function(x) {
  x <- na.omit(x)
  mean(x)
}

south_aransas$range <- tapply(ex$value, list(ex$ID), mean1)

plot(south_aransas, col = south_aransas$range*100)

st_write(st_as_sf(south_aransas), paste0(getwd(), "/", "meta_south_aransas_fire_freq_optimal_new2.shp"), delete_layer = TRUE) 

ex4 <- raster::extract(raster_stack, south_aransas,  df=TRUE)
names(ex4) <- c("ID", recent/365)
ex4$site <- paste("south_aransas", ex4$ID, sep="_")

#################################################################################
#### aransas north ##################################################################
#################################################################################

raster_list <- paste("meta_habitat_suit_aransas_north", "_", recent,"_new.tif", sep ="")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
raster_stack <- stack(raster_list)
y <- data.frame( rasterToPoints(raster_stack) )
y <- y[,3:length(y)]

out <- matrix(ncol = length(id), nrow = dim(y)[1], data = NA)
for(i in 3:(length(id))){
  print(i)
  for(s in 1:dim(y)[1]){
    AUC_value <- DescTools::AUC(recent[1:i], y[s,1:i])
    out[s,i] <- AUC_value/recent[i]
  }
}

# find max value by row, identify which time interval corresponds to that column
out2 <- as.data.frame(out)
names(out2) <- recent/365
out2 <- out2[,3:11]

head(out2)
out2$diff <- apply(out2, 1, max, na.rm=TRUE) - apply(out2, 1, min, na.rm=TRUE)
out2$Largest_Column <- colnames(out2)[apply(out2,1,which.max)]
out2$Largest_Column[out2$diff < 0.02] <- 0

r <- raster_stack[[1]]
r[!is.na(r)] <- as.numeric(as.character(out2$Largest_Column))
writeRaster(r, "meta_north_aransas_optimal_fire_frequency.tiff", format="GTiff", overwrite=TRUE)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
north_aransas <- readOGR(dsn=getwd(), layer="aransas_north_units")

Mode1 <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#extract raster cell count (sum) within each polygon area (poly)

ex <- raster::extract(r, north_aransas,  df=TRUE)
names(ex)[2] <- "value"
north_aransas$fire_freq <- tapply(ex$value, list(ex$ID), Mode1)

plot(north_aransas, col = north_aransas$fire_freq)


r <- raster_stack[[1]]
r[!is.na(r)] <- out2$diff

ex <- raster::extract(r, north_aransas,  df=TRUE)
names(ex)[2] <- "value"
mean1 <- function(x) {
  x <- na.omit(x)
  mean(x)
}

north_aransas$range <- tapply(ex$value, list(ex$ID), mean1)

plot(north_aransas, col = north_aransas$range*100)

st_write(st_as_sf(north_aransas), paste0(getwd(), "/", "meta_north_aransas_fire_freq_optimal.shp"), delete_layer = TRUE) 

ex5 <- raster::extract(raster_stack, north_aransas,  df=TRUE)
names(ex5) <- c("ID", recent/365)
ex5$site <- paste("north_aransas", ex5$ID, sep="_")

#################################################################################
#### east_units ##################################################################
#################################################################################

raster_list <- paste("meta_habitat_suit_aransas_east", "_", recent,"_new.tif", sep ="")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
raster_stack <- stack(raster_list)
y <- data.frame( rasterToPoints(raster_stack) )
y <- y[,3:length(y)]

out <- matrix(ncol = length(id), nrow = dim(y)[1], data = NA)
for(i in 3:(length(id))){
  print(i)
  for(s in 1:dim(y)[1]){
    AUC_value <- DescTools::AUC(recent[1:i], y[s,1:i])
    out[s,i] <- AUC_value/recent[i]
  }
}

# find max value by row, identify which time interval corresponds to that column
out2 <- as.data.frame(out)
names(out2) <- recent/365
out2 <- out2[,3:11]

head(out2)
out2$diff <- apply(out2, 1, max, na.rm=TRUE) - apply(out2, 1, min, na.rm=TRUE)
out2$Largest_Column <- colnames(out2)[apply(out2,1,which.max)]
out2$Largest_Column[out2$diff < 0.02] <- 0

r <- raster_stack[[1]]
r[!is.na(r)] <- as.numeric(as.character(out2$Largest_Column))
writeRaster(r, "meta_east_units_optimal_fire_frequency.tiff", format="GTiff", overwrite=TRUE)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
east_units <- readOGR(dsn=getwd(), layer="east_units")

Mode1 <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#extract raster cell count (sum) within each polygon area (poly)

ex <- raster::extract(r, east_units,  df=TRUE)
names(ex)[2] <- "value"
east_units$fire_freq <- tapply(ex$value, list(ex$ID), Mode1)

plot(east_units, col = east_units$fire_freq)


r <- raster_stack[[1]]
r[!is.na(r)] <- out2$diff

ex <- raster::extract(r, east_units,  df=TRUE)
names(ex)[2] <- "value"
mean1 <- function(x) {
  x <- na.omit(x)
  mean(x)
}

east_units$range <- tapply(ex$value, list(ex$ID), mean1)

plot(east_units, col = east_units$range*100)

st_write(st_as_sf(east_units), paste0(getwd(), "/", "meta_east_units_fire_freq_optimal.shp"), delete_layer = TRUE) 

ex6 <- raster::extract(raster_stack, east_units,  df=TRUE)
names(ex6) <- c("ID", recent/365)
ex6$site <- paste("east", ex6$ID, sep="_")

#################################################################################
#### leftovers dissolve ##################################################################
#################################################################################

raster_list <- paste("habitat_suit_leftover_units", "_", recent,"_new.tif", sep ="") 
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
raster_stack <- stack(raster_list)
y <- data.frame( rasterToPoints(raster_stack) )
y <- y[,3:length(y)]

out <- matrix(ncol = length(id), nrow = dim(y)[1], data = NA)
for(i in 3:(length(id))){
  print(i)
  for(s in 1:dim(y)[1]){
    AUC_value <- DescTools::AUC(recent[1:i], y[s,1:i])
    out[s,i] <- AUC_value/recent[i]
  }
}

# find max value by row, identify which time interval corresponds to that column
out2 <- as.data.frame(out)
names(out2) <- recent/365
out2 <- out2[,3:11]

head(out2)
out2$diff <- apply(out2, 1, max, na.rm=TRUE) - apply(out2, 1, min, na.rm=TRUE)
out2$Largest_Column <- colnames(out2)[apply(out2,1,which.max)]
out2$Largest_Column[out2$diff < 0.02] <- 0

r <- raster_stack[[1]]
r[!is.na(r)] <- as.numeric(as.character(out2$Largest_Column))
writeRaster(r, "meta_leftover_units_optimal_fire_frequency.tiff", format="GTiff", overwrite=TRUE)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
leftover_units <- readOGR(dsn=getwd(), layer="leftovers")

Mode1 <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#extract raster cell count (sum) within each polygon area (poly)

ex <- raster::extract(r, leftover_units,  df=TRUE)
names(ex)[2] <- "value"
leftover_units$fire_freq <- tapply(ex$value, list(ex$ID), Mode1)

plot(leftover_units, col = leftover_units$fire_freq)


r <- raster_stack[[1]]
r[!is.na(r)] <- out2$diff

ex <- raster::extract(r, leftover_units,  df=TRUE)
names(ex)[2] <- "value"
mean1 <- function(x) {
  x <- na.omit(x)
  mean(x)
}

leftover_units$range <- tapply(ex$value, list(ex$ID), mean1)

plot(leftover_units, col = leftover_units$range*100)

st_write(st_as_sf(leftover_units), paste0(getwd(), "/", "meta_leftover_units_fire_freq_optimal.shp"), delete_layer = TRUE) 

ex7 <- raster::extract(raster_stack, leftover_units,  df=TRUE)
names(ex7) <- c("ID", recent/365)
ex7$site <- paste("leftover", ex7$ID, sep="_")


data_all <- rbind(ex2, ex3, ex4, ex5, ex6, ex7)
head(data_all)

saveRDS(data_all, "meta_all_data.RDS")
data_all <- readRDS("meta_all_data.RDS")

data_all$site <- factor(data_all$site)
huh <- data_all %>% group_by(site) %>% 
  summarise(across(names(data_all)[2:12], .f = list(mean = mean), na.rm = TRUE))
head(huh)

dim(huh)

huh <- as.data.frame(huh)

huh <- na.omit(huh)
head(huh)
names(huh)[2:12] <- paste("what", 1:11, sep = "_")
huh$diff <- huh$what_1 - huh$what_11
huh$class <- NA
huh$class[abs(huh$diff) <= 0.02 ] <- "Low impact"
huh$class[abs(huh$diff) > 0.02  & huh$diff> 0] <- "Short interval"
huh$class[abs(huh$diff) > 0.02  & huh$diff< 0] <- "Long interval"
huh <- subset(huh, select = -c(diff))



names(huh) <- c("Site", recent/365, "class")
melt_data <- melt(huh, id = c("Site", "class")) 
names(melt_data) <- c("Site", "class", "Time", "Mean")
melt_data$Time <- as.numeric(as.character(melt_data$Time))


ggplot(melt_data, aes(x = Time, y = Mean, Color = Site))+
  geom_line() + 
  facet_wrap(~class)+
  theme_few()+
  ylab("")

tapply(melt_data$Mean, list(melt_data$class, melt_data$Time), mean)


huh[,c(2:12)] <-huh[,c(2:12)] - huh[,2]
melt_data2 <- melt(huh, id = c("Site", "class")) 
names(melt_data2) <- c("Site", "class", "Time", "Mean")
melt_data2$Time <- as.numeric(as.character(melt_data2$Time))
melt_data2$class <- factor(melt_data2$class, levels =  c("Low impact", "Short interval", "Long interval"))

p1 <- ggplot(melt_data2, aes(x = Time, y = Mean, color = Site))+
  geom_line(size=1, alpha=0.5) + 
  facet_wrap(~class)+
  theme_few()+
  xlim(c(0, 10))+
  xlab("Years since fire")+
  ylab("Habitat suitability change relative to 0 days after fire")+
  theme(legend.position="none")

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Figures/Figures_for_pub")
jpeg(file="Fire_change_meta_6_20_23.jpeg", quality = 100, width = 2000, height = 1500, pointsize = 12, bg = "white", res = 300)
p1
dev.off()


