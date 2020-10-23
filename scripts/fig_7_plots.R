
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Conceptual figure 7

# load relevant libraries
library(raster)
library(viridis)
library(RColorBrewer)
library(scales)
library(sp)
library(here)
library(ggplot2)

# where to access functions from
source(here("scripts/function_plotting_theme.R"))

# create a folder for the outputted figures
if(! dir.exists(here("figures/fig_7"))){
  dir.create(here("figures/fig_7"))
}

# use the show_col function to display the colours to use
show_col(viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "C")(10),
         cex_label = 0.75)

# create a custom colour pallette
pal <- c("white", "#0D0887FF", "#9C179EFF", "#ED7953FF", "#FDC926FF")

# create simulated raster data
rast <- raster(ncol = 5, nrow = 4, xmn = -150, xmx = -80, ymn = 20, ymx = 60)
rast

# generate a random variable from the uniform distribution
raster_values <- runif(ncell(rast), min = 0.1)

# choose values to make zeros
raster_values[c(1, 5, 11, 16, 17)] <- 0

# add these values to the raster object
values(rast) <- raster_values

# plot this raster
dev.off()
plot(rast)

dev.off()
# open jpeg file
png(here("figures/fig_7/fig_7_raster.png"), 
    units = "cm", width = 10, height = 7, res = 600)

plot(rast, legend = FALSE, axes = FALSE, box = FALSE, col = scales::alpha(pal, alpha = 0.5))
plot(rast, legend.only=TRUE, horizontal = TRUE,
     col = scales::alpha(pal, alpha = 0.5),
     legend.width = 1, legend.shrink = 0.75,
     axis.args=list(labels=NULL, 
                    cex.axis=0, col = "white"),
     legend.args=list(cex=0))

# close the file
dev.off()


# make bivariate plot outlines

# local species pool terminology
t1 <- expression(paste("probabilistic ", alpha, " species pool diversity", sep = " "))
t2 <- paste("connectivity")
t3 <- paste("total island diversity", sep = " ")

# ecosystem function terminology
eco_func <- expression(paste("ecosystem function  ", (t[n]), sep = " "))


# simulate some data
x <- seq(from = 3, to = 27, by = 1)
set.seed(45)
y1 <- 1 + (1.5*x) + rnorm(n = length(x), mean = 0, sd = 0.05)
range(y1)

d1 <- data.frame(x = x, y = y1)

p1 <- 
  ggplot(data = d1,
         mapping = aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  ylab(eco_func) +
  xlab(t1) +
  scale_y_continuous(limits = c(-3, 48)) +
  scale_x_continuous(limits = c(-3, 33)) +
  theme_meta() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = 0.6, vjust = 2, size = 11, face = "bold"))

p2 <- 
  ggplot(data = d1,
         mapping = aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  ylab(eco_func) +
  xlab(t2) +
  scale_y_continuous(limits = c(-3, 48)) +
  scale_x_continuous(limits = c(-3, 33)) +
  theme_meta() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = 0.6, vjust = 2, size = 11, face = "bold"))

p3 <- 
  ggplot(data = d1,
         mapping = aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  ylab(eco_func) +
  xlab(t3) +
  scale_y_continuous(limits = c(-3, 48)) +
  scale_x_continuous(limits = c(-3, 33)) +
  theme_meta() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = 0.6, vjust = 2, size = 11, face = "bold"))

# save these plots
p1_list <- list(p1, p2, p3)

# save these pies using lapply

for (i in 1:length(p1_list )) {
  
  ggsave(filename = here(paste0("figures/fig_7/fig_7_biv_",i, ".png") ), 
         plot = p1_list[[i]], width = 7.2, height = 6, units = "cm",
         dpi = 600)
  
}











