
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Analyse kelp data and plot Fig. 5

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# where to access functions from
source(here("scripts/function_plotting_theme.R"))

# check if appropriate packages are installed
if(! "ggpubr" %in% installed.packages()[,1]) stop(
  "WARNING!! this script requires the ggpubr package to be installed to combine figures"
)

# load the biomass data
k.dat <- read_csv(here("analysis_data/kelp_data_cleaned.csv"))

# view the data
head(k.dat)

# calculate alpha diversity
k.a <- 
  k.dat %>%
  group_by(SITE, YEAR) %>%
  summarise(a_div = sum(if_else(AFDM > 0, 1, 0)), .groups = "drop")

# test this result with random examples: if TRUE, then calculation is correct
x <- k.a[sample(x = 1:nrow(k.a), size = 1), ]

x$a_div == k.dat %>%
  filter(SITE == x$SITE,
         YEAR == x$YEAR,
         AFDM > 0) %>%
  nrow()

# calculate local ecosystem functioning
k.a.ef <- 
  k.dat %>%
  group_by(SITE, YEAR) %>%
  summarise(a_ef = sum(AFDM), .groups = "drop")

# test this result with random examples: if TRUE, then calculation is correct
x <- k.a.ef[sample(x = 1:nrow(k.a.ef), size = 1), ]

x$a_ef == k.dat %>%
  filter(SITE == x$SITE,
         YEAR == x$YEAR,
         AFDM > 0) %>%
  pull(AFDM) %>%
  sum()

# bind these data.frames
k.a.cal <- 
  full_join(k.a, k.a.ef)

head(k.a.cal)

# plot fig. 5a

# check the variable distributions

# realised diversity
ggplot(data = k.a.cal,
       mapping = aes(x = a_div, colour = YEAR)) +
  geom_histogram() +
  facet_wrap(~YEAR)

# ecosystem functioning: square-root transformation improves normality of distributions
ggplot(data = k.a.cal,
       mapping = aes(x = sqrt(a_ef), colour = YEAR)) +
  geom_histogram() +
  facet_wrap(~YEAR)

f.5a <- 
  k.a.cal %>%
  mutate(year = as.character(YEAR)) %>%
  ggplot(data = ,
       mapping = aes(x = a_div, y = sqrt(a_ef), colour = year)) +
  geom_point(size = 1.5) +
  stat_smooth(geom='line', alpha=0.5, size = 1, se=FALSE, method = "lm") +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  ylab(expression(sqrt(paste("community dry mass (g ",  "m"^"-2", ")") ))) +
  xlab("realised diversity") +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank(),
        legend.key.size = unit(0.5,"line"))


# calculate local species pool diversity
k.lsp <- 
  k.dat %>%
  filter(AFDM > 0) %>%
  group_by(SITE) %>%
  summarise(lsp_div = length(unique(SP_CODE)), .groups = "drop")

# test this result with random examples: if TRUE, then calculation is correct
x <- k.lsp[sample(x = 1:nrow(k.lsp), size = 1), ]

x$lsp_div == k.dat %>%
  filter(SITE == x$SITE,
         AFDM > 0) %>%
  pull(SP_CODE) %>%
  unique(.) %>%
  length(.)


# calculate average ecosystem function and SE through time
# first square-root transform it
k.lsp.ef <- 
  k.dat %>%
  group_by(SITE, YEAR) %>%
  summarise(lsp_ef = sum(AFDM, na.rm = TRUE), .groups = "drop") %>%
  mutate(lsp_ef = sqrt(lsp_ef)) %>%
  group_by(SITE) %>%
  summarise(mean_lsp_ef = mean(lsp_ef, na.rm = TRUE),
            se_lsp_ef = sd(lsp_ef, na.rm = TRUE)/sqrt(n()), .groups = "drop")

# test this result with random examples: if TRUE, then calculation is correct
x <- k.lsp.ef[sample(x = 1:nrow(k.lsp.ef), size = 1), ]

y <- 
  k.dat %>%
  filter(SITE == x$SITE) %>%
  group_by(YEAR) %>%
  summarise(ef = sum(AFDM, na.rm = TRUE), .groups = "drop") %>%
  mutate(ef = sqrt(ef)) %>%
  summarise(ef_mean = mean(ef, na.rm = TRUE),
            ef_se = sd(ef, na.rm = TRUE)/sqrt(n()))

x$mean_lsp_ef == y$ef_mean
x$se_lsp_ef == y$ef_se


# bind these data.frames
k.lsp.cal <- 
  full_join(k.lsp, k.lsp.ef)

head(k.lsp.cal)

# plot fig. 5b

f.5b <- 
  ggplot(data = k.lsp.cal,
         mapping = aes(x = lsp_div, y = mean_lsp_ef)) +
  geom_point(size = 1.5) +
  geom_errorbar(mapping = aes(x = lsp_div, 
                              ymin = mean_lsp_ef - se_lsp_ef, 
                              ymax = mean_lsp_ef + se_lsp_ef),
                width = 0.1) +
  geom_smooth(method = "lm", size = 0.5, colour = "black", alpha = 0.2) +
  ylab(expression(sqrt(paste("community dry mass (g ",  "m"^"-2", ")") ))) +
  xlab("local species pool diversity") +
  theme_meta() +
  theme(plot.margin = unit(c(5.5,5.5,57,5.5), "pt"))


# join these two figures together
f.5 <- ggpubr::ggarrange(f.5a, f.5b, ncol = 2, nrow = 1, 
                         labels = c("a", "b"),
                         font.label = list(size = 9, color = "black", face = "plain", family = NULL))

ggsave(filename = here("figures/fig_5.pdf"), 
       plot = f.5, width = 11, height = 8, units = "cm",
       dpi = 450)

### END