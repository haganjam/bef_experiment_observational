
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Conceptual figure 1

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)
library(RColorBrewer)
library(viridis)
library(here)
library(vegan)
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
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.45, vjust = 2),
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

ggsave(filename = here("figures/fig_1/fig_a.png"), 
       plot = f1a, width = 9, height = 7, units = "cm",
       dpi = 450)


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
  annotate(geom = "text", x = 15, y = 1.25, label = "?", size = 18) +
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
  annotate(geom = "text", x = 15, y = 1.25, label = "?", size = 18) +
  theme_meta() +
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
       plot = f1bc, width = 21, height = 7, units = "cm",
       dpi = 450)



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


### Box 1, Fig. 1

# pie 5

df4 <- data.frame(
  group = c("B", "C"),
  value = c(1, 1)
)

p4 <- ggplot(df4, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#666666", "#CCCCCC")) +
  theme_void() +
  theme(legend.position = "none")


df5 <- data.frame(
  group = c("B", "C", "D"),
  value = c(2, 1, 1)
)

p5 <- ggplot(df5, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#666666", "#CCCCCC", "#000000")) +
  theme_void() +
  theme(legend.position = "none")

ggarrange(p1, p2, p3, p4, p5)

df6 <- data.frame(
  group = c("A", "C"),
  value = c(1, 1)
)

p6 <- ggplot(df6, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FFFFFF", "#CCCCCC")) +
  theme_void() +
  theme(legend.position = "none")

df7 <- data.frame(
  group = c("A","B","D"),
  value = c(1, 1, 1)
)

p7 <- ggplot(df7, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FFFFFF", "#666666", "#000000")) +
  theme_void() +
  theme(legend.position = "none")

df8 <- data.frame(
  group = c("A","B","C","D"),
  value = c(1, 0.05, 0.3, 0.05)
)

p8 <- ggplot(df8, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FFFFFF", "#666666", "#CCCCCC", "#000000")) +
  theme_void() +
  theme(legend.position = "none")

df9 <- data.frame(
  group = c("B","C","D"),
  value = c(0.05, 0.05, 0.05)
)

p9 <- ggplot(df9, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#666666", "#CCCCCC", "#000000")) +
  theme_void() +
  theme(legend.position = "none")

df10 <- data.frame(
  group = c("B","C","D"),
  value = c(0.05, 0.10, 0.05)
)

p10 <- ggplot(df10, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#666666", "#CCCCCC", "#000000")) +
  theme_void() +
  theme(legend.position = "none")

df11 <- data.frame(
  group = c("B","D"),
  value = c(0.05, 0.05)
)

p11 <- ggplot(df11, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#666666", "#000000")) +
  theme_void() +
  theme(legend.position = "none")

df12 <- data.frame(
  group = c("A","B"),
  value = c(0.05, 0.05)
)

p12 <- ggplot(df12, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FFFFFF", "#666666")) +
  theme_void() +
  theme(legend.position = "none")

df13 <- data.frame(
  group = c("A","D"),
  value = c(0.05, 0.05)
)

p13 <- ggplot(df13, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FFFFFF", "#000000")) +
  theme_void() +
  theme(legend.position = "none")

df14 <- data.frame(
  group = c("B", "C"),
  value = c(1, 1.3)
)

p14 <- ggplot(df14, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#666666", "#CCCCCC")) +
  theme_void() +
  theme(legend.position = "none")


# make singular charts
cols <- c("#FFFFFF", "#666666", "#CCCCCC", "#000000")

df_cols <- data.frame(
  group = c("B"),
  value = c(1)
)

df_plots <- vector("list")

for (i in seq_along(1:length(cols))) {
  
  df_plots[[i]] <- 
    ggplot(df_cols, aes(x = "", y = value, fill = group)) +
    geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
    coord_polar("y", start=0) +
    scale_fill_manual(values=cols[i]) +
    theme_void() +
    theme(legend.position = "none")
  
}

pies <- 
  ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14,
          df_plots[[1]], df_plots[[2]], df_plots[[3]], df_plots[[4]])

pies

ggsave(filename = here("figures/fig_1_pies.png"), plot = pies,
       dpi = 500)


# draw case diagrams (Box 2, fig. 1)

# case 1
case_1a <- 
  ggplot() +
  geom_segment(mapping = aes(x = 1, y = 0.5,
                             xend = 2, yend = 1.5),
               size = 0.5, colour = "black") +
  ylab(eco_func) +
  xlab("") +
  scale_x_continuous(limits = c(0.75, 2.25), breaks = c(1, 2)) +
  scale_y_continuous(limits = c(0.25, 1.75)) +
  theme_meta() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour ="black", size = 10, face = "plain", margin=margin(10,0,0,0,"pt")))

case_1b <- ggplot() +
  geom_segment(mapping = aes(x = 1, y = 0.5,
                             xend = 2, yend = 1.5),
               size = 0.5, colour = "black") +
  ylab("") +
  xlab("") +
  scale_x_continuous(limits = c(0.75, 2.25), breaks = c(1, 2)) +
  scale_y_continuous(limits = c(0.25, 1.75)) +
  theme_meta() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour ="black", size = 10, face = "plain", margin=margin(10,0,0,0,"pt")))

case_1 <- ggarrange(case_1a, case_1b)
case_1  

ggsave(filename = here("figures/case_1.png"), plot = case_1,
       dpi = 500, units = "cm", width = 10, height = 5)


# case 2
case_2a <- 
  ggplot() +
  geom_segment(mapping = aes(x = 3, y = 0.5,
                             xend = 4, yend = 1.5),
               size = 0.5, colour = "black") +
  ylab(eco_func) +
  xlab(a_term) +
  scale_x_continuous(limits = c(2.75, 4.25), breaks = c(3, 4)) +
  scale_y_continuous(limits = c(0.25, 1.75)) +
  theme_meta() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour ="black", size = 10, face = "plain", margin=margin(10,0,0,0,"pt")))

case_2b <- 
  ggplot() +
  geom_segment(mapping = aes(x = 2, y = 1.5,
                             xend = 3, yend = 0.5),
               size = 0.5, colour = "black") +
  ylab("") +
  xlab(b_term) +
  scale_x_continuous(limits = c(1.75, 3.25), breaks = c(2, 3)) +
  scale_y_continuous(limits = c(0.25, 1.75)) +
  theme_meta() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour ="black", size = 10, face = "plain", margin=margin(10,0,0,0,"pt")))

case_2 <- ggarrange(case_2a, case_2b)
case_2  

ggsave(filename = here("figures/case_2.png"), plot = case_2,
       dpi = 500, units = "cm", width = 10, height = 5)

# case 3
case_3a <- 
  ggplot() +
  geom_segment(mapping = aes(x = 3, y = 0.5,
                             xend = 4, yend = 1.5),
               size = 0.5, colour = "black") +
  ylab(eco_func) +
  xlab(a_term) +
  scale_x_continuous(limits = c(2.75, 4.25), breaks = c(3, 4)) +
  scale_y_continuous(limits = c(0.25, 1.75)) +
  theme_meta() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour ="black", size = 10, face = "plain", margin=margin(10,0,0,0,"pt")))

case_3b <- 
  ggplot() +
  geom_segment(mapping = aes(x = 3, y = 0.5,
                             xend = 4, yend = 1.5),
               size = 0.5, colour = "black") +
  ylab("") +
  xlab(b_term) +
  scale_x_continuous(limits = c(2.75, 4.25), breaks = c(3, 4)) +
  scale_y_continuous(limits = c(0.25, 1.75)) +
  theme_meta() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour ="black", size = 10, face = "plain", margin=margin(10,0,0,0,"pt")))

case_3 <- ggarrange(case_3a, case_3b)
case_3  

ggsave(filename = here("figures/case_3.png"), plot = case_3,
       dpi = 500, units = "cm", width = 10, height = 5)







