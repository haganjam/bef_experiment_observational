
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Re-plotting fig. 2 in van der Plas (2019) and performing the spatial extent comparison

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


# load the species pool data
van_fig_3_raw <- read_csv(here("data/van_der_plas_replot_data.csv"))

van_fig_3 <- 
  van_fig_3_raw %>%
  mutate(stratified = if_else(stratified == "no", "unstratified", "stratified"),
         direction = rep(c("+"," ", "-"), 2)) %>%
  mutate(n = rep(c(282, 44), each = 3)) %>%
  mutate(n_slopes = round(proportion*n, 0) ) %>%
  group_by(slope, direction) %>%
  summarise(n_slopes = sum(n_slopes)) %>%
  ungroup() %>%
  mutate(proportion = n_slopes/sum(n_slopes, na.rm = TRUE))

fig_2a <- 
  ggplot(data = van_fig_3,
       mapping = aes(x = slope, y = proportion, fill = slope)) +
  geom_bar(width = 0.25, stat = "identity", position = position_dodge(width=0.5),
           colour = "black", alpha = 0.8) +
  geom_text(aes(label = direction), position = position_dodge(width = 0.5), vjust = -0.5) +
  #scale_fill_manual(values = c("#CC0000", "#FFFF00", "#00CC00")) +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = c(0, 0.73)) +
  ylab("proportion of slopes") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, colour = "black"))

# ggsave(filename = here("figures/fig_3.png"), plot = fig_3, dpi = 300)


# load the spatial extent data
spat_ex_raw <- read_csv(here("data/van_der_plas_2019_spatial_extent_complete.csv"))

# check the column names
names(spat_ex_raw)

# subset the references to include
spat_ex_raw <- 
  spat_ex_raw %>%
  filter(include == "yes")

View(spat_ex_raw)

spat_ex_raw$bef_relationship %>%
  unique()


# summarise the data

spat_ex <- 
  spat_ex_raw %>%
  group_by(spatial_extent) %>%
  mutate(pos = if_else(bef_relationship == "Positive", 1, 0),
         neu = if_else(bef_relationship == "Neutral", 1, 0),
         neg = if_else(bef_relationship == "Negative", 1, 0)) %>%
  summarise_at(vars(c("pos", "neu", "neg")), ~sum(., na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_n = pos + neu + neg) %>%
  gather(pos, neu, neg, key = "relationship", value = "slope") %>%
  mutate(slope_proportion = slope/total_n)

spat_ex %>%
  group_by(spatial_extent) %>%
  summarise(total_n = min(total_n))


# plot out the data 

# first reorder the factors
spat_ex$spatial_extent <- factor(spat_ex$spatial_extent,
                                 levels = c("landscape", "regional", "continental", "global"))

fig_2b <- 
  ggplot(data = spat_ex %>%
         mutate(direction = rep(c("+"," ", "-"), each = 4)),
       mapping = aes(x = spatial_extent, y = slope_proportion, fill = relationship)) +
  geom_bar(width = 0.4, stat = "identity", position = position_dodge(width=0.5),
           colour = "black", alpha = 0.8) +
  geom_text(aes(label = direction), position = position_dodge(width=0.5), vjust = -0.5) +
  #scale_fill_manual(values = c("#CC0000", "#FFFF00", "#00CC00")) +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = c(0, 0.73)) +
  ylab("") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, colour = "black"))

# ggsave(filename = here("figures/fig_3b.png"), plot = fig_3b, dpi = 300)


fig_2 <- 
  ggarrange(fig_2a, fig_2b, labels = c("(a)", "(b)"),
          font.label = list(size = 10, color = "black", face = "plain", family = NULL),
          widths = c(1, 1.8) )

fig_2

ggsave(filename = here("figures/fig_2a_b.png"), plot = fig_2, dpi = 300,
       width = 18, height = 10, units = "cm")


# what about within vs. between habitat comparisons?

spat_with <- 
  spat_ex_raw %>%
  filter(within_between_habitats == "within") %>%
  group_by(spatial_extent) %>%
  mutate(pos = if_else(bef_relationship == "Positive", 1, 0),
         neu = if_else(bef_relationship == "Neutral", 1, 0),
         neg = if_else(bef_relationship == "Negative", 1, 0)) %>%
  summarise_at(vars(c("pos", "neu", "neg")), ~sum(., na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_n = pos + neu + neg)




