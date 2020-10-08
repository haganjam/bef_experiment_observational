
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Conceptual figure 1

# load relevant libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)
library(ggpubr)


# where to access functions from
source(here("scripts/function_plotting_theme.R"))

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

if(! dir.exists(here("figures/fig_1"))){
  dir.create(here("figures/fig_1"))
}


# simulate normally distributed niches
rnorm_perfect <- function(n, mean = 0, sd = 1) {
  stats::qnorm(seq(1 / n, 1 - 1 / n, length.out = n), mean, sd)
}

# set the seed
set.seed(555)

# set up species abundances
spp_abun <- c(200000, 25000, 30000, 10000)

# set up species locations (i.e. means)
spp_means <- c(0, 20, 35, -15)

# set up species standard deviations
spp_sd <- c(25, 5, 5, 3)

# run a loop to 
spp <- vector("list", length = length(spp_abun))
names(spp) <- LETTERS[1:length(spp_abun)]

for (i in seq_along(1:length(spp_abun))) {
  
  spp[[i]] <- data.frame(spp = rnorm_perfect(n = spp_abun[i], mean = spp_means[i], sd = spp_sd[i]))
  
}

spp <- 
  bind_rows(spp, .id = "species") %>%
  as_tibble()

# normal distributions in grey scale

f1a <- 
  ggplot(data = spp, 
       mapping = aes(x = spp, y = ..count.., fill = species)) +
  geom_density(alpha = 0.75, colour = "white") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) +
  scale_x_continuous(limits = c(-75, 75)) +
  ylab("competitive ability") +
  xlab("niche") +
  scale_fill_viridis_d(option = "C", end = 0.9, direction = -1) +
  ggtitle("filtered species pool") +
  theme_meta() +
  theme(legend.position = "right",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 12, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 12, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.45, vjust = 2),
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

ggsave(filename = here("figures/fig_1/fig_a.png"), 
       plot = f1a, width = 7, height = 5, units = "cm",
       dpi = 600)


# local species pool terminology
a_term <- expression(paste(alpha, " species pool diversity", sep = " "))
ae_term <- expression(paste("initial ", alpha, " diversity", sep = " "))

# realised diversity terminology
b_term <- expression(paste("realised ", alpha, " diversity ", (t[n]), sep = " "))

# ecosystem function terminology
eco_func <- expression(paste("ecosystem function  ", (t[n]), sep = " "))


# figure 1b
x <- seq(from = 0, to = 30, by = 1)
set.seed(45)
y1 <- 1*(x^0.1) + rnorm(n = length(x), mean = 0, sd = 0.05)

fig_1b_dat <- tibble(x = x, y = y1)

b1 <- 
  ggplot(data = fig_1b_dat,
       mapping = aes(x = x, y = y)) +
  stat_smooth(method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)), se = FALSE,
              size = 0.5, colour = "black") +
  scale_y_continuous(limits = c(0.9, 1.6)) +
  ylab(eco_func) +
  xlab(ae_term) +
  ggtitle("(b) BEF theory and exp.") +
  theme_meta() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = 0.6, vjust = 2, size = 11, face = "bold"))

b2 <- 
  ggplot(data = fig_1b_dat,
       mapping = aes(x = x, y = y)) +
  scale_y_continuous(limits = c(0.9, 1.6)) +
  ylab("") +
  xlab(b_term) +
  annotate(geom = "text", x = 15, y = 1.25, 
           label = "italic(rarely)~~italic(reported)", parse = TRUE, size = 3.5) +
  ggtitle("") +
  theme_meta() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = -0.9, vjust = 2, size = 11))

f1b <- ggarrange(b1, b2)  

# figure 1c
x <- seq(from = 0, to = 30, by = 1)
y1 <- (5 - 0.05*x) + rnorm(n = length(x), mean = 0, sd = 0.05)
y2 <- 2 + rnorm(n = length(x), mean = 0, sd = 0.05)
y3 <- (0 + 0.1*x) + rnorm(n = length(x), mean = 0, sd = 0.05)

fig_c_dat <- tibble(x = x, y1 = y1, y2 = y2, y3 = y3)
fig_c_dat <- 
  fig_c_dat %>%
  pivot_longer(cols = c("y1", "y2", "y3"))

c1 <- 
  ggplot(data = fig_1b_dat,
         mapping = aes(x = x, y = y)) +
  ylab(eco_func) +
  xlab(a_term) +
  theme_meta() +
  annotate(geom = "text", x = 15, y = 1.25, label = "italic(unknown)", parse = TRUE, size = 3.5) +
  ggtitle("(c) BEF field data") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = -0.2, vjust = 2, size = 11, face = "bold"))


c2 <- 
  ggplot(data = fig_c_dat,
         mapping = aes(x = x, y = value, group = name)) +
  geom_smooth(method = "lm", size = 0.5, colour = "black", se = FALSE) +
  ylab("") +
  xlab(b_term) +
  theme_meta() +
  ggtitle("") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = -0.9, vjust = 2, size = 11))

f1c <- ggarrange(c1, c2)


# bind these two figures together
f1bc <- 
  ggarrange(f1b, f1c, nrow = 1, ncol = 2,
            labels = NULL)

ggsave(filename = here("figures/fig_1/fig_bc.png"), 
       plot = f1bc, width = 21, height = 6.5, units = "cm",
       dpi = 600)



# Pie charts

### fig. 1:

# pie 1
df1 <- data.frame(
  group = c("A", "B", "C", "D"),
  value = c(1, 1, 1, 1)
)

p1 <- 
  ggplot(df1, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 1) +
  coord_polar("y", start=0) +
  scale_fill_viridis_d(option = "C", end = 0.9, direction = -1) +
  theme_void() +
  theme(legend.position = "none")

scales::show_col(viridis_pal(option = "C", end = 0.9, direction = -1)(4))

# pie 2
df2 <- 
  data.frame(
  group = c("B", "C", "D"),
  value = c(1, 1, 1)
)

p2 <- 
  ggplot(df2, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#E16462FF", "#900DA4FF", "#0D0887FF" )) +
  theme_void() +
  theme(legend.position = "none")

# pie 3
df3 <- data.frame(
  group = c("A", "C"),
  value = c(1, 1)
)

p3 <- ggplot(df3, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FCCE25FF", "#900DA4FF")) +
  theme_void() +
  theme(legend.position = "none")

# pie 4
df4 <- data.frame(
  group = c("A", "C"),
  value = c(2, 1)
)

p4 <- ggplot(df4, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FCCE25FF", "#900DA4FF")) +
  theme_void() +
  theme(legend.position = "none")

f1a_p <- list(p1, p2, p3, p4)

# save these pies using lapply

for (i in 1:length(f1a_p)) {
  
  ggsave(filename = here(paste0("figures/fig_1/pie_",i, ".png") ), 
         plot = f1a_p[[i]], width = 2.5, height = 2.5, units = "cm",
         dpi = 600)
  
}








