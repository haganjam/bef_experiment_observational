
# Isbell et al. (2018): Quantifying effects of biodiversity on ecosystem functioning across times and places

# functions for estimating various biodiversity effects from mixture/monoculture data

library(dplyr)
library(tibble)

# define examples from Isbell et al. (2018)

# table 1A
t1a <- data.frame(sample = c(1,1,2,2),
                  time = c(1,1,1,1), 
                  place = c(1,1,2,2), 
                  species = c(1,2,1,2), 
                  M = c(200,200,100,100),
                  Y = c(200,200,0,0))

# table 1B
t1b <- data.frame(sample = c(1,1,2,2),
                  time = c(1,1,1,1), 
                  place = c(1,1,2,2), 
                  species = c(1,2,1,2), 
                  M = c(50,350,0.44,1),
                  Y = c(8.15,291.7,0.88,0))

# case 1
cs.1 <- data.frame(sample = rep(c(1:4), each = 2),
                   time=c(1,1,1,1,2,2,2,2), 
                   place=c(1,1,2,2,1,1,2,2), 
                   species=c(1,2,1,2,1,2,1,2),
                   M=c(100,50,100,50,100,50,100,50), 
                   Y=c(100,0,100,0,100,0,100,0))

# case 2
cs.2 <- data.frame(sample = rep(c(1:4), each = 2), 
           time=c(1,1,1,1,2,2,2,2), 
           place=c(1,1,2,2,1,1,2,2), 
           species=c(1,2,1,2,1,2,1,2),
           M=c(100,50,100,50,50,100,50,100), 
           Y=c(100,0,100,0,0,100,0,100))

# case 3
cs.3 <- 
  data.frame(sample = rep(c(1:4), each = 2),
             time=c(1,1,1,1,2,2,2,2), 
             place=c(1,1,2,2,1,1,2,2), 
             species=c(1,2,1,2,1,2,1,2),
             M=c(100,50,50,100,100,50,50,100), 
             Y=c(100,0,0,100,100,0,0,100))

# case 4
cs.4 <- 
  data.frame(sample = rep(c(1:4), each = 2),
             time=c(1,1,1,1,2,2,2,2), 
             place=c(1,1,2,2,1,1,2,2), 
             species=c(1,2,1,2,1,2,1,2),
             M=c(100,50,50,100,50,100,100,50), 
             Y=c(100,0,0,100,0,100,100,0))

# case 5
cs.5 <- data.frame(sample = rep(c(1:4), each = 2),
           time=c(1,1,1,1,2,2,2,2), 
           place=c(1,1,2,2,1,1,2,2), 
           species=c(1,2,1,2,1,2,1,2),
           M=c(75,75,75,75,75,75,75,75), 
           Y=c(50,50,50,50,50,50,50,50))

# case 6
cs.6 <- data.frame(sample = rep(c(1:4), each = 2),
           time=c(1,1,1,1,2,2,2,2), 
           place=c(1,1,2,2,1,1,2,2), 
           species=c(1,2,1,2,1,2,1,2),
           M=c(100,50,100,50,100,50,100,50), 
           Y=c(50,50,50,50,50,50,50,50))


# define examples from Fox (2005)
f1 <- data.frame(sample = rep(c(1, 2, 3, 4, 5, 6), each = 2),
                 species = rep(c(1, 2), 6),
                 M = rep(c(500, 250), 6),
                 Y = c(300, 100, 330, 110, 360, 120, 390, 130, 420, 140, 450, 150))

# terms used:

# dY = deviation from total expected yield in mixture (i.e. Yo - Ye)
# Yo = observed mixture yield
# Ye = expected mixture yield (based on initial proportions and monoculture performance)

# Net Biodiversity Effect = Yo - Ye (i.e. dY)

# for each mixture

# Mi = monoculture of each species
# Yoi = observed yield of each species in mixture
# Yo = observed mixture yield - sum(Yoi)

# RYei = expected relative yield of each species (1/n spp)
# RYoi = observed relative yield (Yoi/Mi) i.e. measures the extent to which species i overyields

# Yei = expected yield of each species in mixture (RYe*Mi)
# Ye = expected yield of mixture - sum(Yei)

# dRY = RYoi - RYei

# N = n spp in mixture

# Poi = observed proportion of each species in mixture (i.e. RYoi/sum(RYoi))


# define function for number of unique elements in a vector
n_unique <- function(x) length(unique(x))

# define function for the raw covariance
raw_cov <- function(x, y) {
  c1 <- x - mean(x)
  c2 <- y - mean(y)
  sum( (c1*c2) )/length(c1)
}

# define the preparation function
B.ef.prep <- function(df, RYe) {
  
  if (n_unique(df$species) != length(RYe) ) {
    stop("error, expected frequencies not defined")
  }
  
  # define expected relative yields
  df$RYe <- rep(RYe, n_unique(df$sample))
  
  # define observed relative yields
  df$RYo <- (df$Y/df$M)
  
  # define the change in relative yield
  df$dRY <- (df$RYo - df$RYe)
  
  # calculate expected yield for each species
  df$Ye <- (df$M*df$RYe)
  
  # calculate observed proportion of each species in mixture (po,ijk, Isbell et al. 2018)
  df$Poi <- unlist(lapply(split(df, df$sample), 
                          function(x) {
                            x$Y/sum(x$Y) }), use.names = FALSE)
  
  # calculate change in observed proportion relative to the expectation (d.po,ijk, Isbell et al. 2018)
  df$d.Poi <- (df$Poi - df$RYe)
  
  # calculate change in observed proportion (dRYo,ijk Isbell et al. 2018)
  df$d.RYoi <- (df$RYo - df$Poi)
  
  if(TRUE %in% grepl(pattern = "time|place", names(df))) {
    
    # define extra data.frames for means at different levels
    
    # species means for each time across places (pij and Mij single bar, Isbell et al. 2018)
    sm_t <- aggregate(df[, c("d.Poi", "M") ], list(df$time, df$species), mean)
    names(sm_t) <- c("time", "species", "d.Poi.t", "M.t")
    df <- merge(df, sm_t, all.x = TRUE)
    
    # species means for each place across times (pik and Mik single bar, Isbell et al. 2018)
    sm_p <- aggregate(df[, c("d.Poi", "M") ], list(df$place, df$species), mean)
    names(sm_p) <- c("place", "species", "d.Poi.p", "M.p")
    df <- merge(df, sm_p, all.x = TRUE)
    
    # overall species mean across all times and places
    sm_s <- aggregate(df[, c("d.Poi", "M") ], list(df$species), mean)
    names(sm_s) <- c("species", "d.Poi.s", "M.s")
    df <- merge(df, sm_s, all.x = TRUE)
    
    # reorder the columns
    df <- df[order(df$time,df$place,df$species), ]
    
    df.p <- list(f = df,
                 s = sm_s,
                 p = sm_p,
                 t = sm_t)
    
    return(df.p)
    
  } else ( return(df) )
  
}

# test the B.ef.prep function

# B.ef.prep(df = cs.6, RYe = c(0.5, 0.5))


# define a function to perform the Isbell et al. (2018) partition
isbell.2018.pt <- function(adf, RY.exp = c(0.5, 0.5)) {
  
  # call B.ef.prep function to prepare the data
  df.p <- B.ef.prep(df = adf, RYe = RY.exp)
  
  # set number of places, times and species
  df <- df.p$f
  
  P <- n_unique(df$place)
  M <- n_unique(df$time)
  S <- n_unique(df$species)
  
  PMS <- P*M*S
  
  # calculate the net biodiversity effect across times and places
  NBE.y <- sum(df$dRY*df$M)
  
  # NBE is partitioned as:
  
  # (1) total complementarity effect
  CE.y <- (PMS)*mean(df$dRY)*mean(df$M)
  
  # (2) total selection effect
  SE.y <- PMS*raw_cov(df$dRY, df$M)
  
  # calculate local-scale complementarity
  y <- aggregate(df[, c("dRY", "M")], list(df$sample), mean)
  CE.a <- sum( (S*(y$dRY)*(y$M))  )
  
  # calculate the local-scale selection effect
  y <- sapply(split(df, df$sample), function(x) { (S*raw_cov(x$dRY,x$M)) })
  SE.a <- sum(y)
  
  
  # partition total selection as:
  
  # (1) non-random overyielding
  NR.oy <- PMS*raw_cov(df$d.RYoi, df$M)
  
  # (2) total insurance
  tot.ins <- sum( (df$d.Poi - mean(df$d.Poi))*(df$M - mean(df$M)) )
  
  # partition total insurance as:
  
  # (2a) average selection effect
  ave.S <- PMS*raw_cov(df.p$s$d.Poi.s, df.p$s$M.s)
  
  # (2b) temporal insurance effect
  z <- merge(df.p$t, df.p$s, by = "species")
  t.ins <- P*sum( (z$d.Poi.t - z$d.Poi.s)*(z$M.t - z$M.s) )
  
  # (2c) spatial insurance effect
  z <- merge(df.p$p, df.p$s, by = "species")
  s.ins <- M*sum( (z$d.Poi.p - z$d.Poi.s)*(z$M.p - z$M.s) )
  
  # (2b) spatio-temporal insurance
  dp.n <- df$d.Poi - df$d.Poi.p - df$d.Poi.t + df$d.Poi.s
  M.n <- df$M - df$M.p - df$M.t + df$M.s + mean(df$M)
  
  st.ins <- PMS*raw_cov(dp.n, M.n)
  
  # make sure insurance effects add up
  # if (tot.ins != sum(c(ave.S, t.ins, s.ins, st.ins)) ) {
    # print("error, insurance effects do not add up")
  # }
  
  # pull all these effects into a data.frame
  be_out <- data.frame(biodiversity_effect = c("net_biodiversity_effect", 
                                               "total_complementarity_effect", "total_selection_effect", 
                                               "local_complementarity_effect", "local_selection_effect",
                                               "non_random_overyielding",
                                               "total_insurance", "average_selection",
                                               "temporal_insurance", "spatial_insurance",
                                               "spatio_temporal_insurance"),
                       effect_size = c(NBE.y, CE.y, SE.y, CE.a, SE.a, NR.oy, tot.ins, ave.S, t.ins, s.ins, st.ins))
  
  return(be_out)
  
}

# test this function

# isbell.2018.pt(adf = cs.6, RY.exp = c(0.5, 0.5))
# isbell.2018.pt(adf = t1a, RY.exp = c(0.5, 0.5))
# isbell.2018.pt(adf = t1b, RY.exp = c(0.5, 0.5))
# isbell.2018.pt(adf = cs.1, RY.exp = c(0.5, 0.5))
# isbell.2018.pt(adf = cs.2, RY.exp = c(0.5, 0.5))
# isbell.2018.pt(adf = cs.3, RY.exp = c(0.5, 0.5))
# isbell.2018.pt(adf = cs.4, RY.exp = c(0.5, 0.5))
# isbell.2018.pt(adf = cs.5, RY.exp = c(0.5, 0.5))
# isbell.2018.pt(adf = cs.6, RY.exp = c(0.5, 0.5))
  

# define a function to perform the Loreau and Hector (2001) partition
hector.loreau.2001.pt <- function(adf, RY.exp) {
  
  # check the there are times or places in the data.frame
  if(TRUE %in% grepl(pattern = "time|place", names(adf))) {
    stop("error, remove time and place columns from the data.frame or perform the whole spatial-temporal partition using isbell.2018.pt() function")
  }
  
  # use the B.ef.prep function to define relevant terms
  df.p <- B.ef.prep(df = adf, RYe = RY.exp)
  
  # define the number of species
  N <- n_unique(df.p$species)
  
  # calculate the net biodiversity effect
  nbe_df <- aggregate(df.p[, c("Y", "Ye") ], list(df.p$sample), sum)
  NBE.a <- (nbe_df$Y - nbe_df$Ye)
  
  # calculate the complementarity effect (trait independent complementarity sensu Fox 2005)
  comp_df <- aggregate(df.p[, c("dRY", "M")], list(df.p$sample), mean)
  CE.a <- N*(comp_df$dRY)*(comp_df$M)
  
  # calculate the selection effect
  SE.a <- sapply(split(df.p, df.p$sample), 
                 function(x) { N*raw_cov(x$dRY,x$M) })
  
  # wrap this into a data.frame
  be_out <- data.frame(sample = unique(df.p$sample),
                       net.biodiversity.effect = NBE.a,
                       complementarity.effect = CE.a,
                       selection.effect = SE.a)
  
  return(be_out)
  
}

# test the function with the Fox (2005) example
# hector.loreau.2001.pt(adf = f1, RY.exp = c(0.6, 0.4))

# test with one of Isbell et al.'s (2018) example
# hector.loreau.2001.pt(adf = cs.1, RY.exp = c(0.5, 0.5))

# remove time and place columns
# hector.loreau.2001.pt(adf = cs.1[, !(names(cs.1) %in% c("time", "place")) ], RY.exp = c(0.5, 0.5))



# define a function to perform the Fox (2005) partition
fox.2005.pt <- function(adf, RY.exp) {
  
  # check the there are times or places in the data.frame
  if(TRUE %in% grepl(pattern = "time|place", names(adf))) {
    stop("error, remove time and place columns from the data.frame or perform the whole spatial-temporal partition using isbell.2018.pt() function")
  }
  
  # use the B.ef.prep function to define relevant terms
  df.p <- B.ef.prep(df = adf, RYe = RY.exp)
  
  # define the number of species
  N <- n_unique(df.p$species)
  
  # calculate the net biodiversity effect
  nbe_df <- aggregate(df.p[, c("Y", "Ye") ], list(df.p$sample), sum)
  NBE.a <- (nbe_df$Y - nbe_df$Ye)
  
  # calculate trait independent complementarity sensu Fox 2005
  comp_df <- aggregate(df.p[, c("dRY", "M")], list(df.p$sample), mean)
  CE.a <- N*(comp_df$dRY)*(comp_df$M)
  
  # calculate trait dependent complementarity
  tdc <- sapply(split(df.p, df.p$sample), 
                function(x) { N*raw_cov(x$M, (x$RYo - (x$RYo/sum(x$RYo)) ) ) })
  
  # calculate the dominance effect
  dom <- sapply(split(df.p, df.p$sample), 
                function(x) { N*raw_cov(x$M, ((x$RYo/sum(x$RYo)) - x$RYe) ) })
  
  # wrap this into a data.frame
  be_out <- data.frame(sample = unique(df.p$sample),
                       net.biodiversity.effect = NBE.a,
                       trait.independent.complementarity = CE.a,
                       trait.dependent.complementarity = tdc,
                       dominance = dom)
  
  return(be_out)

}

# test the function with the Fox (2005) example
# fox.2005.pt(adf = f1, RY.exp = c(0.6, 0.4))

# test with one of Isbell et al.'s (2018) example
# fox.2005.pt(adf = cs.1[, !(names(cs.1) %in% c("time", "place")) ], RY.exp = c(0.5, 0.5))








