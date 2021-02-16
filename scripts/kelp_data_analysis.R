
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Analyse kelp data and plot Fig. 4

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(vegan)

# where to access functions from
source(here("scripts/function_plotting_theme.R"))

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

# plot fig. 4a



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
k.lsp.ef <- 
  k.dat %>%
  group_by(SITE, YEAR) %>%
  summarise(lsp_ef = sum(AFDM, na.rm = TRUE), .groups = "drop") %>%
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
  summarise(ef_mean = mean(ef, na.rm = TRUE),
            ef_se = sd(ef, na.rm = TRUE)/sqrt(n()))

x$mean_lsp_ef == y$ef_mean
x$se_lsp_ef == y$ef_se


# bind these data.frames
k.lsp.cal <- 
  full_join(k.lsp, k.lsp.ef)

head(k.lsp.cal)

# plot fig. 4b





# work with the summarised data
# examine the relationships between alpha diversity and function
alpha_div <- 
  kelp_ana_sum %>%
  group_by(SITE, YEAR) %>%
  summarise(alpha_diversity = sum(decostand(AFDM, method = "pa"), na.rm = TRUE),
            comm_biomass = sum(AFDM, na.rm = TRUE),
            .groups = "drop")

ggplot(data = alpha_div,
       mapping = aes(x = alpha_diversity, y = sqrt(comm_biomass), colour = as.character(YEAR) )) +
  geom_point(alpha = 0.5, shape = 16, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, size = 0.1) +
  scale_colour_viridis_d() +
  theme_meta() +
  ylab("sqrt( community biomass )") +
  xlab("realised diversity") +
  theme(legend.position = "none")


# get a slope for each year by fitting a simple linear regression
# then plot a distribution of slopes
alpha_slopes <- 
  split(select(alpha_div, -YEAR), alpha_div$YEAR ) %>%
  lapply(., function(x) {
    
    z <- lm(sqrt(comm_biomass) ~ (alpha_diversity), data = x)
    
    coef(z)[2]
    
  }) %>%
  
  bind_rows(., .id = "YEAR")

fig_4a_inset <- 
  ggplot(data = alpha_slopes,
         mapping = aes(x = alpha_diversity)) +
  geom_histogram(bins = 15, alpha = 0.5, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean(alpha_slopes$alpha_diversity), 
             colour = "red") +
  xlab(NULL) +
  ylab(NULL) +
  theme_meta() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

# plot this histogram as an inset graph

fig_4a <- 
  ggplot(data = alpha_div,
         mapping = aes(x = alpha_diversity, y = sqrt(comm_biomass), colour = as.character(YEAR) )) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  theme_meta() +
  ylab(expression(sqrt(paste("community dry mass (g ",  "m"^"-2", ")") ))) +
  xlab(expression(paste("realised ", alpha, " diversity"))) +
  scale_y_continuous(limits = c(0, 53)) +
  annotation_custom(ggplotGrob(fig_4a_inset), xmin = 1, xmax = 12, 
                    ymin = 34, ymax = 57) +
  theme(legend.position = "none")

fig_4a

# examine the relationship between temporal gamma diversity function
kelp_ana_sum %>%
  group_by(SITE) %>%
  summarise(years = length(unique(YEAR)))


# we fit the model with the square-root of community biomass
# do the calculations for biomass with the square-root

# calculate mean biomass
mean_bio <- 
  kelp_ana_sum %>%
  group_by(SITE, YEAR) %>%
  summarise(comm_biomass = sum(AFDM, na.rm = TRUE), .groups = "drop") %>%
  group_by(SITE) %>%
  summarise(comm_biomass_mean = mean(sqrt(comm_biomass), na.rm = TRUE),
            comm_biomass_se = (sd(sqrt(comm_biomass), na.rm = TRUE)/sqrt(n())), .groups = "drop")

# calculate temporal gamma diversity
temporal_gamma <- 
  kelp_ana_sum %>%
  pivot_wider(names_from = SP_CODE, values_from = AFDM) %>%
  select(-YEAR) %>%
  group_by(SITE) %>%
  summarise(across(.cols = everything(), ~sum(.x, na.rm = TRUE)), .groups = "keep") %>%
  mutate(across(.cols = everything(), ~as.numeric(decostand(.x, method = "pa")) )) %>%
  ungroup() %>%
  rowwise(SITE) %>%
  summarise(temporal_gamma = sum(c_across(where(is.numeric)))) %>%
  ungroup()

gamma_div <- full_join(mean_bio, temporal_gamma, by = "SITE") 

# test this with a linear model

# examine the variable distributions
hist(gamma_div$comm_biomass_mean)
hist((gamma_div$temporal_gamma) )

# fit the linear model
lm_1 <- lm(comm_biomass_mean ~ (temporal_gamma), data = gamma_div)
plot(lm_1)
hist(residuals(lm_1))

# check the model output
summary(lm_1)
lm_1_sum <- summary(lm_1)
lm_1_sum

# plot the graph
fig_4b <- 
  ggplot() +
  geom_point(data = gamma_div,
             mapping = aes(x = temporal_gamma, y = comm_biomass_mean), 
             size = 1.5) +
  geom_errorbar(data = gamma_div,
                mapping = aes(x = temporal_gamma, 
                              ymin = comm_biomass_mean - comm_biomass_se, 
                              ymax = comm_biomass_mean + comm_biomass_se),
                width = 0.1) +
  geom_smooth(data = gamma_div,
              mapping = aes(x = temporal_gamma, y = comm_biomass_mean),
              method = "lm", size = 0.5, colour = "black", alpha = 0.3) +
  annotate("text", x = -Inf, y = Inf, 
           label = paste("r^2 == ", round(lm_1_sum$r.squared, 2)), parse = TRUE,
           vjust = 1.1, hjust = -0.5, size = 3) +
  annotate("text", x = -Inf, y = Inf, 
           label = paste("F == ", round(lm_1_sum$fstatistic[1], 2)), parse = TRUE,
           vjust = 3.3, hjust = -0.5, size = 3) +
  annotate("text", x = -Inf, y = Inf, 
           label = paste("P == ", round(lm_1_sum$coefficients[2, 4], 3)), parse = TRUE,
           vjust = 5.1, hjust = -0.45, size = 3) +
  xlab(expression(paste(alpha, " species pool diversity", sep = ""))) +
  ylab(expression(sqrt(paste("community dry mass (g ",  "m"^"-2", ")") ))) +
  theme_meta()

fig_4b

fig_4 <- 
  ggarrange(fig_4a, fig_4b, labels = c("a", "b"),
            font.label = list(size = 12, color = "black", face = "plain", family = NULL),
            widths = c(1, 1))

ggsave(filename = here("figures/fig_4.png"), plot = fig_4, dpi = 500,
       width = 16, height = 7, units = "cm")


