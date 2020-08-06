

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

# create customised plotting theme
theme_meta <- function(base_size = 12, base_family = "") {
  theme(panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill="NA", color="black", size=0.35, linetype="solid"),
        axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2),
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(),
        axis.ticks.length = unit(-0.16, "cm"),
        axis.title.x = element_text(colour ="black", size = 12, face = "plain", margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(colour = "black", size = 12, face = "plain", margin=margin(0,15,0,0,"pt")),
        axis.text.x = element_text(colour = "black", size=10, face = "plain",  margin=margin(10,0,0,0,"pt")),
        axis.text.y = element_text(colour ="black", size=10, face = "plain", margin=margin(0,10,0,0,"pt")),
        axis.ticks.x = element_line(colour = "black", size = 0.4),
        axis.ticks.y = element_line(colour = "black", size = 0.4))
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

fig_1a <- 
  ggplot(data = spp, 
       mapping = aes(x = spp, y = ..count.., fill = species)) +
  geom_density(alpha = 0.6, colour = "black") +
  scale_fill_manual(values=c("#FFFFFF", "#666666", "#CCCCCC", "#000000")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000)) +
  scale_x_continuous(limits = c(-75, 75)) +
  ylab("competitive ability") +
  xlab("niche") +
  theme_meta() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
fig_1a

ggsave(filename = here("figures/fig_1a.png"), plot = fig_1a,
       dpi = 300)


# local species pool terminology
a_term <- expression(paste("species pool diversity  ", (t[0]), sep = " "))

# realised diversity terminology
b_term <- expression(paste("realised diversity  ", (t[1]), sep = " "))

# ecosystem function terminology
eco_func <- expression(paste("ecosystem function  ", (t[1]), sep = " "))


# figure 1b
x <- seq(from = 0, to = 30, by = 1)
set.seed(45)
y1 <- 1*(x^0.1) + rnorm(n = length(x), mean = 0, sd = 0.05)

fig_1b_dat <- tibble(x = x, y = y1)

fig_1b_1 <- 
  ggplot(data = fig_1b_dat,
       mapping = aes(x = x, y = y)) +
  stat_smooth(method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)), se = FALSE,
              size = 0.5, colour = "black") +
  scale_y_continuous(limits = c(0.9, 1.6)) +
  ylab(eco_func) +
  xlab(a_term) +
  theme_meta() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

fig_1b_2 <- 
  ggplot(data = fig_1b_dat,
       mapping = aes(x = x, y = y)) +
  scale_y_continuous(limits = c(0.9, 1.6)) +
  ylab("") +
  xlab(b_term) +
  theme_meta() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

fig_1b <- ggarrange(fig_1b_1, fig_1b_2)  

ggsave(filename = here("figures/fig_1b.png"), plot = fig_1b,
       dpi = 300)


# figure 1c
x <- seq(from = 0, to = 30, by = 1)
y1 <- (5 - 0.05*x) + rnorm(n = length(x), mean = 0, sd = 0.05)
y2 <- 2 + rnorm(n = length(x), mean = 0, sd = 0.05)
y3 <- (0 + 0.1*x) + rnorm(n = length(x), mean = 0, sd = 0.05)

fig_1b2_dat <- tibble(x = x, y1 = y1, y2 = y2, y3 = y3)
fig_1b2_dat <- 
  fig_1b2_dat %>%
  pivot_longer(cols = c("y1", "y2", "y3"))

fig_1c_1 <- 
  ggplot(data = fig_1b_dat,
         mapping = aes(x = x, y = y)) +
  ylab(eco_func) +
  xlab(a_term) +
  theme_meta() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


fig_1c_2 <- 
  ggplot(data = fig_1b2_dat,
         mapping = aes(x = x, y = value, group = name)) +
  geom_smooth(method = "lm", size = 0.5, colour = "black") +
  ylab("") +
  xlab(b_term) +
  theme_meta() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

fig_1c <- ggarrange(fig_1c_1, fig_1c_2)  

ggsave(filename = here("figures/fig_1c.png"), plot = fig_1c,
       dpi = 300)






# make pie charts

df1 <- data.frame(
  group = c("A", "B", "C", "D"),
  value = c(1, 1, 1, 1)
)

p1 <- ggplot(df1, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FFFFFF", "#666666", "#CCCCCC", "#000000")) +
  theme_void() +
  theme(legend.position = "none")

df2 <- data.frame(
  group = c("A", "D"),
  value = c(1.5, 0.5)
)

p2 <- ggplot(df2, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FFFFFF", "#000000")) +
  theme_void() +
  theme(legend.position = "none")

df3 <- data.frame(
  group = c("A", "C"),
  value = c(2.5, 1)
)

p3 <- ggplot(df3, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FFFFFF", "#CCCCCC")) +
  theme_void() +
  theme(legend.position = "none")

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

ggsave(filename = here("figures/fig_1_pies.png"), plot = pies,
       dpi = 300)




