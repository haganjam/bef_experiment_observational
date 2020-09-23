
# pre-submission proposal code

### rough plots for presubmission proposal

# plot species pool diversity and realised diversity at the final time point

# get final time point
tn <- max(unique(pp_fig_s_l_mod$time))

# choose species pool diversities to consider
sp_p_plot <- c(10, 20, 60)

# check range of biomass values
range(pull(filter(pp_fig_s_l_mod, time == tn), community_biomass))
y_ran <- seq(from = 100, to = 170, by = 20)

pp_fig_1b_1 <- 
  pp_fig_s_l_mod %>%
  filter(time == tn) %>%
  ggplot(data = .,
         mapping = aes(x = species_pool, y = community_biomass)) +
  geom_jitter(size = 3, width = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.75) +
  scale_x_continuous(limits = c(5, 65), breaks = unique(pull(filter(pp_fig_s_l_mod, time == tn), species_pool)) ) +
  scale_y_continuous(limits = range(pull(filter(pp_fig_s_l_mod, time == tn), community_biomass)), breaks = y_ran) +
  ylab("community biomass") +
  xlab("local species pool diversity") +
  theme_meta() 

pp_fig_1b_2 <- 
  pp_fig_s_l_mod %>%
  filter(time == tn, species_pool %in% sp_p_plot) %>%
  mutate(`LSP diversity` = as.character(species_pool)) %>%
  ggplot(data = .,
         mapping = aes(x = realised_richness, y = community_biomass, colour = `LSP diversity`)) +
  geom_jitter(size = 3, width = 0.1) +
  geom_smooth(method = "lm", se = FALSE, size = 0.75) +
  ylab("") +
  xlab("local realised diversity") +
  scale_colour_viridis_d(option = "D") +
  scale_y_continuous(limits = range(pull(filter(pp_fig_s_l_mod, time == tn), community_biomass)), breaks = y_ran) +
  theme_meta() +
  theme(legend.position = c(0.75, 0.2),
        legend.key = element_rect(fill = NA))


pp_fig_1b_12 <- ggarrange(pp_fig_1b_1, pp_fig_1b_2, nrow = 1)



### plots for pre-proposal submission i.e. random samples of different years

# choose hypothetical plots to sample at a given time-point
n_samp <- 20

# only consider plots after x generations
x_gen <- 500

# how many time points to sample?
t_p <- 3

sim_samp <- 
  pp_fig_s_l_mod %>%
  filter(time > x_gen) %>%
  filter(time %in% sample(x = c(x_gen:max(unique(pp_fig_s_l_mod$time))), size = t_p) ) %>%
  group_by(time) %>%
  slice_sample(n = n_samp)

# output a set of breaks
range(sim_samp$community_biomass)
y_ran <- seq(from = 100, to = 170, by = 20)

# plot the relationship between realised diversity and biomass at different random time-points
pp_fig_1_e_1 <-
  ggplot(data = sim_samp %>%
           ungroup() %>%
           mutate(time = as.character(rep(c(1:t_p), each = n_samp)) ),
         mapping = aes(x = realised_richness,
                       y = community_biomass,
                       colour = time )) +
  geom_jitter(size = 3, width = 0.1) +
  geom_smooth(method = "lm", se = FALSE, size = 0.75) +
  scale_y_continuous(limits = range(sim_samp$community_biomass), breaks = y_ran) +
  ylab("") +
  xlab("local realised diversity") +
  scale_colour_viridis_d(option = "D") +
  theme(legend.position = c(0.85, 0.2),
        legend.key = element_rect(fill = NA)) +
  theme_meta()

pp_fig_1_e_1


# plot the relationship between species pool diversity and mean ecosystem functioning
pp_fig_1_e_2 <-
  ggplot(data = sim_samp %>%
           ungroup() %>%
           group_by(species_pool) %>%
           summarise(community_biomass_m = mean(community_biomass),
                     community_biomass_se = sd(community_biomass)/sqrt(n()) ),
         mapping = aes(x = species_pool,
                       y = community_biomass_m)) +
  geom_errorbar(mapping = aes(ymin = community_biomass_m-community_biomass_se,
                              ymax = community_biomass_m + community_biomass_se),
                width = 0.1) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.75) +
  scale_y_continuous(limits = range(sim_samp$community_biomass), breaks = y_ran) +
  ylab("community biomass") +
  xlab("local species pool diversity") +
  theme_meta()

pp_fig_e_12 <- ggarrange(pp_fig_1_e_2 , pp_fig_1_e_1, nrow = 1)


