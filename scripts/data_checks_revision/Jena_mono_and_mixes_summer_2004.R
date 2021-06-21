
# load the Jena biomass data
# jena_bio <- read_delim(here("raw_data/Jena_Biomass_02-08.csv"), delim = ",")
jena_bio <- read_delim(url("https://ndownloader.figshare.com/files/5608847"), delim = ",")
head(jena_bio)
names(jena_bio)

# create a vector of species names
sp_names <- names(jena_bio[, 85:144])

# remove the first plots that were not sown with any species
jena_bio <- filter(jena_bio, !(sowndiv %in% c(0)) )

# remove species presence columns
jena_bio <- select(jena_bio, -all_of(paste0("p", sp_names)))

# create a season variable for jena_bio
unique(jena_bio$month)

jena_bio <- 
  jena_bio %>%
  mutate(season = if_else(month %in% c("May", "Jun"), "spring", "summer"))

# subset out the spring data only
jena_bio <- 
  jena_bio %>%
  filter(season == "spring")

# replace the NAs with zeros
jena_bio <- 
  jena_bio %>%
  mutate(across(.cols = all_of(sp_names), ~replace(., is.na(.), 0)))

# remove rows of the data where there are missing values i.e. -9999 values in the sp_names
jena_bio <- 
  jena_bio %>%
  filter_at(all_of(sp_names), all_vars(. >= 0 ) )

# take the first three sub-samples as not all plots have four sub-samples
unique(jena_bio$subsample)

jena_bio <- 
  jena_bio %>%
  filter(subsample %in% c(1, 2, 3))

# select out the relevant columns
jena_bio <- 
  jena_bio %>%
  select(plotcode, season, time, subsample, sowndiv, target.biomass, all_of(sp_names))

# for each species and for target biomass, take the average value
jena_bio <- 
  jena_bio %>%
  group_by(plotcode, time) %>%
  summarise(across(.cols = all_of(c("sowndiv", "target.biomass", sp_names)), ~mean(., na.rm = TRUE) ), .groups = "drop")

# extract site variables
site_bio <- 
  jena_bio %>%
  select(-all_of(sp_names))

# check for NA's in the dataset
lapply(site_bio, function(x) { sum(if_else(is.na(x), 1, 0)) })

# check for any -9999's in the dataset
lapply(site_bio, function(x) { sum(if_else(x < 0, 1, 0)) })

# extract out the individual species' biomass
sp_bio <- 
  jena_bio %>%
  select(all_of(sp_names))

# check for NA's in the dataset
lapply(sp_bio, function(x) { sum(if_else(is.na(x), 1, 0)) })

# check for any -9999's in the dataset
lapply(sp_bio, function(x) { sum(if_else(x < 0, 1, 0), na.rm = TRUE) })

# check if biomass calculated from individual species correlates with target.biomass reported
cor(rowSums(sp_bio), site_bio$target.biomass)

# add species-specific biomass and observed species richness to the site_dat data
site_bio <- 
  site_bio %>%
  mutate(comm_biomass = rowSums(sp_bio),
         observed_sr = rowSums(decostand(sp_bio, method = "pa")) )

# take the final time-point
site_bio <- 
  filter(site_bio, time == max(time))

# jena data from PANGAEA: mixtures in 2004
# link: https://doi.pangaea.de/10.1594/PANGAEA.846319
j.dat <- read_tsv(url("https://doi.pangaea.de/10.1594/PANGAEA.846319?format=textfile"), skip = 109)
View(j.dat)

unique(j.dat$`Date/time start`)[1]

j.cols <- names(j.dat)

# get full species names
sp.names.full <- j.cols[grepl(pattern = "\\[g\\/m\\*\\*2\\]", j.cols) & (!grepl(pattern = "plant|Weeds", j.cols))]

# simplify species names
sp.names <- gsub(pattern = "\\ biom\\ \\[g\\/m\\*\\*2\\]", replacement = "", x = sp.names.full)
sp.names <- gsub(pattern = "\\.\\ ", replacement = "_", x = sp.names)

# subset and pivot longer then select relevant columns
j.sub <- 
  j.dat %>%
  filter(`Date/time start` == unique(j.dat$`Date/time start`)[1]) %>%
  filter(Replicate != "mean") %>%
  select(`Experimental plot`, `Date/time start`, Replicate, `Sown plant biom [g/m**2]`,
         all_of(sp.names.full))

names(j.sub) <- c("plotcode", "date", "replicate", "sown_biomass_g_m2", sp.names)

# convert into the long_format and clean the data
j.sub <- 
  j.sub %>%
  pivot_longer(cols = all_of(sp.names),
               names_to = "species",
               values_to = "biomass_g_m2") %>%
  mutate(biomass_g_m2 = if_else(is.na(biomass_g_m2), 0, biomass_g_m2))

# check for -9999 values i.e. true missing values
lapply(j.sub, function(x) {sum(if_else(x < -100, 1, 0)) } )

sowndiv_treat <- 
  site_bio %>%
  select(plotcode, sowndiv)

# add this sowndiv column into the j.sub data
j.bio.mass <- 
  full_join(j.sub, sowndiv_treat, by = "plotcode") %>%
  select(plotcode, date, sowndiv, replicate, sown_biomass_g_m2, species, biomass_g_m2) %>%
  arrange(date, plotcode, sowndiv, replicate, species)

# check if each replicate has columns for each species
j.bio.mass %>%
  group_by(plotcode, replicate) %>%
  summarise(n = length(unique(species))) %>%
  pull(n) %>%
  range()

# take the mean of each species across all the replicates
j.mix <- 
  j.bio.mass %>%
  group_by(date, plotcode, sowndiv, species) %>%
  summarise(mean_biomass_g_m2 = mean(biomass_g_m2, na.rm = TRUE), .groups = "drop")

nrow(j.mix)
length(unique(j.mix$plotcode))

# load the monoculture data from the smaller plots
# https://doi.pangaea.de/10.1594/PANGAEA.866313
j.dom <- read_tsv(file = url("https://doi.pangaea.de/10.1594/PANGAEA.866313?format=textfile"), skip = 36)

unique(j.dom$`Date/time start`)[1]

j.dom <- 
  j.dom %>%
  filter(`Date/time start` == unique(j.dom$`Date/time start`)[1]) %>%
  filter(Replicate != "mean")

nrow(j.dom)

# get list of plots to extract information for the plots
plot.list <- 
  j.dom %>%
  pull(`Experimental plot`) %>%
  unique()

length(plot.list)

# get information about species in each plot
# https://doi.pangaea.de/10.1594/PANGAEA.866313
dom.plots <- read_tsv(url("https://store.pangaea.de/Publications/Jena_Experiment/PlotInformationSmallMonos.txt"))

dom.plots <- 
  dom.plots %>%
  filter(plotcode %in% plot.list) %>%
  select(plotcode, SpeciesFullName, SpeciesAbbreviation)

names(dom.plots) <- c("plotcode", "species", "species_abbr")

# subset out the relevant columns in the j.dom data
j.dom <- 
  j.dom %>%
  select(`Experimental plot`, `Date/time start`, Replicate, `Sown plant biom [g/m**2]`)

names(j.dom) <- c("plotcode", "date", "replicate", "sown_plant_biomass_g_m2")

j.monos <- 
  full_join(j.dom, dom.plots, by = "plotcode") %>%
  select(plotcode, date, species, species_abbr, replicate, sown_plant_biomass_g_m2)

j.monos.m <- 
  j.monos %>%
  group_by(date, plotcode, species, species_abbr) %>%
  summarise(sown_plant_biomass_g_m2 = mean(sown_plant_biomass_g_m2), .groups = "drop") %>%
  group_by(species, species_abbr) %>%
  summarise(monoculture_biomass_g_m2 = mean(sown_plant_biomass_g_m2), .groups = "drop")

# check these data
summary(j.monos.m)
head(j.monos.m)

j.monos.m <- 
  j.monos.m %>%
  mutate(species = paste(substring(species, 1, 1), gsub(pattern = ".*? ", replacement = "", species), sep = "_" ) )

any(is.na(j.monos.m$monoculture_biomass_g_m2))

j.monos.m %>%
  filter(is.na(monoculture_biomass_g_m2))

# find a way to merge these data
j.mix <- 
  j.mix %>%
  mutate(mono_mix = "mixture") %>%
  select(date, plotcode, sowndiv, mono_mix, species, mean_biomass_g_m2)

# join these datasets
any(!(unique(j.mix$species) %in% j.monos.m$species))

unique(j.mix$species[which(!(unique(j.mix$species) %in% j.monos.m$species))])
sort(unique(j.mix$species))
sort(j.monos.m$species)

cbind(sort(unique(j.mix$species) ), sort(j.monos.m$species) ) %>%
  View()

# fix these name problems
j.monos.m <- 
  j.monos.m %>%
  mutate(species = if_else(species == "G_agg.", "G_mollugo", species),
         species = if_else(species == "L_agg.", "L_vulgare", species),
         species = if_else(species == "T_agg.", "T_officinale", species))

j.mix <- 
  j.mix %>%
  mutate(species = if_else(species == "M_x varia", "M_varia", species))

part_dat <- 
  full_join(j.mix, j.monos.m, by = "species") %>%
  select(date, plotcode, sowndiv, species, species_abbr, monoculture_biomass_g_m2, mean_biomass_g_m2) %>%
  filter(!is.na(sowndiv))

nrow(part_dat) == (82*60)

# plot relationship between realised diversity and functioning in 16 species treatment
part_dat %>%
  filter(sowndiv == 16) %>%
  group_by(plotcode) %>%
  summarise(biomass = sum(mean_biomass_g_m2),
            realised_richness = sum(if_else(mean_biomass_g_m2 > 0, 1, 0)), .groups = "drop") %>%
  ggplot(data = .,
         mapping = aes(x = realised_richness, y = biomass)) +
  geom_point() +
  geom_smooth(method = "lm")

# load the BEF partition scripts
source(here("scripts/functions_BEF_partitions.R"))

# what format should the data be in
f1

# put data in the correct format
part_format <- 
  part_dat %>%
  filter(mean_biomass_g_m2 > 0) %>%
  select(plotcode, sowndiv, species, monoculture_biomass_g_m2, mean_biomass_g_m2)

n_rr <- 
  part_format %>%
  group_by(plotcode, sowndiv) %>%
  summarise(n = length(unique(species)), .groups = "drop") %>%
  pull(n)

part_format <- 
  part_format %>%
  select(-sowndiv)

names(part_format) <- c("sample", "species", "M", "Y")


  
# some monoculture data are missing
# remove plots with those monocultures
rye <- 
  part_dat %>%
  select(plotcode, sowndiv) %>%
  distinct()

rye_test <- 1/rep(rye$sowndiv[2], n_rr[2])

unique(part_format$sample)

df_test <- 
  part_format %>%
  filter(sample == "B1A02")

j.list <- split(part_format, part_format$sample)

part_out <- vector("list", length = length(j.list)) 

for(i in 1:length(j.list)) {
  
  x <- 1/rep(rye$sowndiv[i], n_rr[i])
  
  part_out[[i]] <- fox.2005.pt(adf = j.list[[i]], RY.exp = x)
  
}

part_out[[1]]

df.part <- 
  bind_rows(part_out, .id = "plotcode") %>%
  filter( (!is.na(net.biodiversity.effect))) %>%
  filter(trait.independent.complementarity != Inf) %>%
  select(-plotcode)

row.names(df.part) = NULL

df.part <- 
  df.part %>% 
  rename(plotcode = sample)

df.part

df.mixes <- 
  j.mix %>%
  group_by(sowndiv, plotcode) %>%
  summarise(total_biomass = sum(mean_biomass_g_m2),
            realised_richness = sum(if_else(mean_biomass_g_m2 > 0, 1, 0)), .groups = "drop")

tester <- 
  left_join(df.part, df.mixes, by = "plotcode") %>%
  as_tibble()

mean(tester$net.biodiversity.effect)

head(tester)

tester %>%
  filter(sowndiv > 2) %>%
  pivot_longer(cols = c("trait.dependent.complementarity", "trait.independent.complementarity", "dominance"),
               names_to = "comp_sel",
               values_to = "val") %>%
  ggplot(data = .,
         mapping = aes(x = realised_richness, y = val, colour = comp_sel)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~sowndiv, scales = "free") +
  theme_classic() +
  theme(legend.position = "bottom")


tester %>%
  filter(sowndiv > 2) %>%
  ggplot(data = .,
         mapping = aes(x = realised_richness, y = total_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~sowndiv, scales = "free") +
  theme_classic()

  
