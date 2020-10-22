
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Conceptual figure 7

library(raster)
library(viridis)
library(RColorBrewer)
library(scales)
library(sp)

# use the show_col function to display the colours to use
show_col(viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "C")(10),
         cex_label = 0.75)

# create a custom colour pallette
pal <- c("white", "#0D0887FF", "#9C179EFF", "#ED7953FF", "#FDC926FF")

# create simulated raster data
rast <- raster(ncol = 5, nrow = 4, xmn = -150, xmx = -80, ymn = 20, ymx = 60)

# generate a random variable from the uniform distribution
raster_values <- runif(ncell(rast), min = 0.1)

# choose values to make zeros
raster_values[c(1, 5, 11, 16, 17)] <- 0

# add these values to the raster object
values(rast) <- raster_values

# plot this raster
dev.off()
plot(rast, legend = FALSE, axes = FALSE, box = FALSE, col = scales::alpha(pal, alpha = 0.5))
plot(rast, legend.only=TRUE, horizontal = TRUE,
     col = scales::alpha(pal, alpha = 0.5),
     legend.width = 1, legend.shrink = 0.75,
     axis.args=list(labels=NULL, 
                    cex.axis=0, col = "white"),
     legend.args=list(cex=0))


# throw sampling points onto this grid



# create a set of sampling points

longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5,
               -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9,
              36.2, 39, 41.6, 36.9)


sampling_sites <- cbind(longitude, latitude)

library(sp)
pts <- SpatialPoints(sampling_sites)

plot(rast)
plot(pts, cex=2, pch=20, col='red', main='Sampling sites', add = T)


wst <- data.frame(longitude, latitude, name, body_size)
wst