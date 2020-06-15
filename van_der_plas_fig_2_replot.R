
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Re-plotting fig. 2 in van der Plas (2019)

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
van_fig_3 <- read_csv(here("data/van_der_plas_2019_fig_3.csv"))

van_fig_3

fig_3 <- 
  ggplot(data = van_fig_3 %>%
         mutate(stratified = if_else(stratified == "no", "unstratified (n = 282)", "stratified (n = 44)"),
                direction = rep(c("-"," ", "+"), 2)),
       mapping = aes(x = stratified, y = proportion, fill = slope_direction)) +
  geom_bar(width = 0.4, stat = "identity", position = position_dodge(width=0.5),
           colour = "black", alpha = 0.8) +
  geom_text(aes(label = direction), position=position_dodge(width=0.5), vjust = -0.5) +
  scale_fill_manual(values = c("#CC0000", "#FFFF00", "#00CC00")) +
  ylab("proportion of slopes") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, colour = "black"))

ggsave(filename = here("figures/fig_3.png"), plot = fig_3, dpi = 300)




