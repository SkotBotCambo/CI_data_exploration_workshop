
#SET PARAMETERS
setwd("e:/py/CI_data_exploration_workshop")
tv = read.csv('TV-20140605141210_1231.csv', skip=62)


#FIX DATE
tv$date <- as.Date(tv$Photo.Date, format="%Y-%m-%d")

#ADD SPECIES
animals <- unique(sort(paste0(tv$Genus, " ", tv$Species)))

#ADD A COLOR FOR EACH SPECIES
animals.col <- colorRampPalette(c("blue", 
                                  "red",
                                  "green",
                                  "yellow",
                                  "black",
                                  "white"))(length(animals))


#USE THE MAPS LIBRARY FOR SIMPLE MAPPING
library(maps)
map("world")
map("world", "costa rica")

#READ IN SOME OTHER PACKAGES FOR MORE COMPLEX MAPPING
library(rJava)
library(OpenStreetMap)
library(rgdal)
library(maps)
library(RColorBrewer)

#MAKE A WATERCOLOR MAP
par(mfrow=c(1,1))
wcMap <- openmap(c(12, -86), c(8,-82.2),
                 type="stamen-watercolor")

wcMap <- openproj(wcMap, projection = "+proj=longlat")
plot(wcMap, raster=TRUE)

#MAKE A TONER MAP
bwMap <- openproj(bwMap, projection = "+proj=longlat")

plot(bwMap, raster=TRUE)
names(bwMap)

#MAKE A BASIC OUTLINE MAP #From http://www.gadm.org/download
par(mfrow=c(1,1))
load("crMap0.RData")

#PLOT THE OUTLINE OVER THE WATERCOLOR MAP
gadm0 <- gadm
plot(wcMap, raster=TRUE)

#ADD BORDER
plot(gadm0, add=TRUE, lwd=5, border="black")

#ADD ANIMALS
for (i in 1:length(animals)){
  
  points(tv$Longitude[which(paste0(tv$Genus, " ", tv$Species)== animals[i])],
         tv$Latitude[which(paste0(tv$Genus, " ", tv$Species) == animals[i])],
         pch=16,
         cex=tv$Number.of.Animals^(1/4),
         col=adjustcolor(animals.col[i], alpha.f=0.1))
}
legend(x="bottomleft",
       col=adjustcolor(animals.col, alpha.f=0.3),
       pch=16,
       legend=animals,
       cex=0.7,
       ncol=3, 
       bty="n")



#SIMPLE MAPS FOR EACH SPECIES
load("crMap.RData") #From http://www.gadm.org/download

par(mar=c(0,0,2,0))
par(oma=c(1,1,1,1))
par(mfrow=c(3,3))
for (i in unique(sort(paste0(tv$Genus, " ", tv$Species)))){
  plot(gadm,
       xlim=c(-85.5,-82.8),
       ylim=c(8.0, 11.5),
       border="grey")
  title(main=i, cex.main=1.5)
  points(tv$Longitude[which(paste0(tv$Genus, " ", tv$Species)== i)],
         tv$Latitude[which(paste0(tv$Genus, " ", tv$Species) == i)],
         pch=16,
         cex=tv$Number.of.Animals,
         col=adjustcolor("darkred", alpha.f=0.1))
}

#TIME MAP
par(mfrow=c(1,1))
for (i in unique(sort(tv$date))){
  plot(gadm0,
       xlim=c(-85,-83),
       ylim=c(10, 11))
  points(tv$Longitude[which(tv$date == i)],
         tv$Latitude[which(tv$date == i)],
         pch=16,
         cex=0.1,
         col=adjustcolor("darkred", alpha.f=0.5))
  title(main=format(as.Date(i, origin="1970-01-01"), format="%B %d %Y"))
  Sys.sleep(0.2)
  
}
