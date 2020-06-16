

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

# simulate normally distributed niches

rnorm_perfect <- function(n, mean = 0, sd = 1) {
  stats::qnorm(seq(1 / n, 1 - 1 / n, length.out = n), mean, sd)
}

# set the seed
set.seed(555)

# set up species abundances
spp_abun <- c(200000, 40000, 30000, 10000)

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

# normal distributions in colour

ggplot(data = spp, 
       mapping = aes(x = spp, y = ..count.., fill = species)) +
  geom_density(alpha = 0.6, colour = NA) +
  theme_classic() +
  scale_fill_viridis_d(option = "C") +
  geom_hline(yintercept=0, colour="white", size=1) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())

# normal distributions in grey scale

fig_1 <- 
  ggplot(data = spp, 
       mapping = aes(x = spp, y = ..count.., fill = species)) +
  geom_density(alpha = 0.6, colour = "black") +
  theme_classic() +
  scale_fill_manual(values=c("#FFFFFF", "#666666", "#CCCCCC", "#000000")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000)) +
  scale_x_continuous(limits = c(-100, 100)) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
fig_1

ggsave(filename = here("figures/fig_1.png"), plot = fig_1,
       dpi = 300)


fig_1a <- 
  ggplot(data = spp %>% filter(species %in% c("A", "D")), 
         mapping = aes(x = spp, y = ..count.., fill = species)) +
  geom_density(alpha = 0.6, colour = "black") +
  theme_classic() +
  scale_fill_manual(values=c("#FFFFFF", "#000000")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000)) +
  scale_x_continuous(limits = c(-53, 60)) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave(filename = here("figures/fig_1a.png"), plot = fig_1a,
       dpi = 300)


fig_1b <- 
  ggplot(data = spp %>% filter(species %in% c("B", "C")), 
         mapping = aes(x = spp, y = ..count.., fill = species)) +
  geom_density(alpha = 0.6, colour = "black") +
  theme_classic() +
  scale_fill_manual(values=c("#666666", "#CCCCCC")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000)) +
  scale_x_continuous(limits = c(-53, 60)) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave(filename = here("figures/fig_1b.png"), plot = fig_1b,
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
  group = c("A", "B", "C"),
  value = c(2, 1, 1)
)

p3 <- ggplot(df3, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity", colour = "black", alpha = 0.6) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#FFFFFF", "#666666", "#CCCCCC")) +
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
  ggarrange(p1, p2, p3, p4, p5,
          df_plots[[1]], df_plots[[2]], df_plots[[3]], df_plots[[4]])

ggsave(filename = here("figures/fig_1_pies.png"), plot = pies,
       dpi = 300)




