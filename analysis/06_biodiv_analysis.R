source(list.files("./R", full.names = T))

ipak(c("tidyverse"))

# Joining datasets -------------------------------------------------------------

all_dat <- read.csv("./data/all_dat.csv", row.names = 1) %>% tibble()
coord_covars <- read.csv("./data/coord_covars.csv", row.names = 1) %>% tibble()
disp_covars <- read.csv("./data/disp_covars.csv", row.names = 1) %>% tibble()
ecoreg_covars <- read.csv("./data/ecoreg_dat.csv", row.names = 1) %>% tibble()

c_dat <- all_dat %>%
  filter(monoculture == "yes" | variables_name %in% c("aboveground_carbon",
                                                      "aboveground_biomass")) %>%
  mutate(variables_name = ifelse(monoculture == "yes", "aboveground_carbon", variables_name)) %>%
  mutate(mean_ha = ifelse(grepl("biomass", variables_name), mean_ha * 0.47, mean_ha)) %>%
  left_join(coord_covars) %>%
  left_join(disp_covars) %>%
  left_join(ecoreg_covars %>%
              dplyr::select(coord_id, BIOME_NAME)) %>%
  mutate(ratio = disp_both_eff_le_sum /disp_both_eff_no_le_sum) %>%
  # The above ratio represents seed dispersal integrity
  filter(stand_age > 0, # Because we're log-transforming, these time = 0 will be removed
         mean_ha > 0) %>%
  mutate(any_intervention = ifelse(irrigated == "y" |
                                     weeded == "y" |
                                     fertilized == "y",
                                   1,
                                   0) %>% ifelse(is.na(.), 0, .))

c_dat <- c_dat %>%
  mutate(fire = ifelse(is.na(fire_2500),
                       fire_10000,
                       fire_2500),
         drought = ifelse(is.na(drought_2500),
                          drought_10000,
                          drought_2500))

# Ensuring the biome is the same across the Bukoski and our own attribution
biome_test <- c_dat %>% filter(!is.na(biome)) %>% dplyr::select(biome, BIOME_NAME)
all(biome_test[,1] == biome_test[,2])

# Remove the duplicate column
c_dat <- c_dat %>%
  dplyr::select(-biome) %>%
  mutate(biome = BIOME_NAME) %>%
  dplyr::select(-BIOME_NAME)
table(is.na(c_dat$biome))

# Filter to tropical biomes
c_dat <- c_dat %>%
  filter(grepl("ropical", biome))



# Get data object together for jags --------------------------------------------

j_dat <- c_dat %>%
  dplyr::select(site_id,
                plot_id,
                mean_ha,
                stand_age,
                ratio,
                monoculture,
                tap_mm,
                mat_C,
                npp_10000_max_average,
                grazers,
                any_intervention,
                fire,
                drought,
                lat_dec) %>%
  filter(complete.cases(.)) %>%
  mutate(DISP = ratio, # This is the dispersal integrity value
         MONO = ifelse(monoculture == "yes", 1, 0),
         NPP = npp_10000_max_average,
         GRAZERS = grazers,
         FIRE = fire,
         DROUGHT = drought,
         LAT = abs(lat_dec)/10,
         AMP = tap_mm,
         AMT = mat_C,
         SITE = as.numeric(factor(site_id)))

n_site <- length(unique(j_dat$SITE))

# It's critical that dataframe is ordered by site
j_dat <- j_dat[order(j_dat$SITE),]

# Here is the data object given to jags
dataList <- list(
  BIOMASS = j_dat$mean_ha,
  AGE = j_dat$stand_age,

  n = length(j_dat$mean_ha),
  n_site = n_site,

  SITE = j_dat$SITE,

  MONO_for_SD = (j_dat$monoculture %>% as.factor() %>% as.numeric()),

  # DISP
  c1 = j_dat[!duplicated(j_dat$SITE),]$DISP,
  # AMP
  c2 = j_dat[!duplicated(j_dat$SITE),]$AMP %>% scale() %>% as.vector(),
  # AMT
  c3 = j_dat[!duplicated(j_dat$SITE),]$AMT %>% scale() %>% as.vector(),
  # NPP
  c4 = j_dat[!duplicated(j_dat$SITE),]$NPP %>% scale() %>% as.vector(),

  # Grazers
  c5 = j_dat[!duplicated(j_dat$SITE),]$GRAZERS %>% scale() %>% as.vector(),
  # Fire
  c6 = j_dat[!duplicated(j_dat$SITE),]$FIRE %>% scale() %>% as.vector(),
  # Drought
  c7 = j_dat[!duplicated(j_dat$SITE),]$DROUGHT %>% scale() %>% as.vector(),

  MONO = j_dat[!duplicated(j_dat$SITE),]$MONO %>% as.vector(),
  MONO_factor = j_dat[!duplicated(j_dat$SITE),]$monoculture %>% as.factor() %>% as.numeric(),

  # Intervention
  INTER = j_dat[!duplicated(j_dat$SITE),]$any_intervention,

  pred_ages = 1:40,
  n_pred_ages = length(1:40),
  pred_mono_levels = c(0, 1),
  pred_disp_levels = c(0, 0.58),

  pred_disp_values = seq(0, 0.58, length.out = 30),
  n_pred_disp_values = 30
)


cov_to_var <- c("DISP", "AMP", "AMT", "NPP", "GRAZERS", "FIRE", "DROUGHT")
cov_to_name <- c("DISP", "AMP", "AMT", "NPP", "GRAZERS", "FIRE", "DROUGHT")
mono_intx <- c(1) # Which of the variables should have interactions with monoculture?

cov_to_sample <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7")

n_cov <- length(cov_to_sample)

a_vars_to_sample <- paste("beta_a_",  cov_to_sample, sep = "")
b_vars_to_sample <- paste("beta_b_",  cov_to_sample, sep = "")

if(length(mono_intx) > 0){
  a_vars_to_sample <- c(a_vars_to_sample, "beta_a_mono", paste0("beta_a_mono_intx_c", mono_intx), "beta_a_inter")
  b_vars_to_sample <- c(b_vars_to_sample, "beta_b_mono", paste0("beta_b_mono_intx_c", mono_intx), "beta_b_inter")
}


# # The chunk below takes hours to run, so will just read in
# # a saved version of model outputs below.
# # Define model
# jags_script_string <- "./analysis/biomass_jags.txt"
# sink(jags_script_string)
# cat(paste0("
#
# model {
#
#   # Likelihood
#
#   for(i in 1:n){
#
#     # Biomass values lognormally distrubuted around a Monod function
#     BIOMASS[i] ~ dlnorm(log(mean[i]), tau_log_bio[MONO_for_SD[i]])
#     mean[i] <- a[SITE[i]] * AGE[i] / (b[SITE[i]] + AGE[i])
#
#   }
#
#   for(i in 1:n_site){
#     a[i] <- exp(beta_a_0 +
#                 beta_a_c2 * c2[i] +
#                 beta_a_c3 * c3[i] +
#                 beta_a_c4 * c4[i] +
#                 beta_a_mono * MONO[i] +
#                 rand_a_s[SITE[i]]
#                 )
#
#     b[i] <- exp(beta_b_0 +
#                 beta_b_c1 * c1[i] +
#                 beta_b_c2 * c2[i] +
#                 beta_b_c3 * c3[i] +
#                 beta_b_c4 * c4[i] +
#                 beta_b_c5 * c5[i] +
#                 beta_b_c6 * c6[i] +
#                 beta_b_c7 * c7[i] +
#                 beta_b_inter * INTER[i] +
#                 beta_b_mono * MONO[i] +
#                 beta_b_mono_intx_c1 * MONO[i] * c1[i] +
#                 rand_b_s[SITE[i]]
#                 )
#
#   }
#
#
#   # Priors
#
#   beta_a_0 ~ dnorm(5, 1) # Roughly reasonable max carbon estimates
#
#   beta_a_c2 ~ dnorm(0, 0.1)
#   beta_a_c3 ~ dnorm(0, 0.1)
#   beta_a_c4 ~ dnorm(0, 0.1)
#   beta_a_mono ~ dnorm(0, 0.1)
#
#   beta_b_0 ~ dnorm(3, 0.1) # Using reasonable times to half saturation
#
#   beta_b_c1 ~ dnorm(0, 0.1)
#   beta_b_c2 ~ dnorm(0, 0.1)
#   beta_b_c3 ~ dnorm(0, 0.1)
#   beta_b_c4 ~ dnorm(0, 0.1)
#   beta_b_c5 ~ dnorm(0, 0.1)
#   beta_b_c6 ~ dnorm(0, 0.1)
#   beta_b_c7 ~ dnorm(0, 0.1)
#
#   beta_b_inter ~ dnorm(0, 0.1)T(,0) # Expect negative
#   beta_b_mono ~ dnorm(0, 0.1)
#   beta_b_mono_intx_c1 ~ dnorm(0, 0.1)
#
#
#   # Random effect for prior use, with sum to zero constraints
#   for(i in 1:(n_site-1)){
#     rand_a_s[i] ~ dnorm(0, tau_a_s)
#     rand_b_s[i] ~ dnorm(0, tau_b_s)
#   }
#   rand_a_s[n_site] <- -sum(rand_a_s[1:(n_site-1)])
#   rand_b_s[n_site] <- -sum(rand_b_s[1:(n_site-1)])
#
#   tau_a_s <- pow(sig_a_s, -2)
#   sig_a_s ~ dunif(0,10)
#   tau_b_s <- pow(sig_b_s, -2)
#   sig_b_s ~ dunif(0,10)
#
#   for(i in 1:2){
#     tau_log_bio[i] <- pow(sig_log_bio[i], -2)
#     sig_log_bio[i] ~ dunif(0, 10)
#   }
#
#
#
#   # Derived quantities
#
#   for(j in 1:2){ # Levels of dispersal to test
#     for(k in 1:2){ # Levels of monculture/non-monoculture
#
#       # Note that we are setting all values for other predictor variables to the
#       # mean value (because centered and scaled, this is zero)
#
#       aa[j,k] <- exp(beta_a_0 +
#           beta_a_mono * pred_mono_levels[k]
#           )
#
#       bb[j,k] <- exp(beta_b_0 +  beta_b_c1 * pred_disp_levels[j] + beta_b_mono * pred_mono_levels[k] + beta_b_mono_intx_c1 * pred_mono_levels[k] * pred_disp_levels[j])
#
#       for(t in 1:n_pred_ages){
#         pred_mean[j,k,t] <- aa[j,k] * pred_ages[t] / (bb[j,k] + pred_ages[t])
#
#         pred_biomass[j,k,t] ~ dlnorm(log(pred_mean[j,k,t]), tau_log_bio[k])
#
#       }
#     }
#   }
#
#   # Estimates at 30 years of growth
#   for(j in 1:n_pred_disp_values){ # Values of dispersal to test
#     for(k in 1:2){ # Levels of monculture/non-monoculture
#
#       # Note that we are setting all values for other predictor variables to the
#       # mean value (because centered and scaled, this is zero)
#
#       aa30[j,k] <- exp(beta_a_0 +
#       beta_a_mono * pred_mono_levels[k]
#       )
#
#       bb30[j,k] <- exp(beta_b_0 +  beta_b_c1 * pred_disp_values[j] + beta_b_mono * pred_mono_levels[k] + beta_b_mono_intx_c1 * pred_mono_levels[k] * pred_disp_values[j])
#
#       pred_mean30[j,k] <- aa30[j,k] * 30 / (bb30[j,k] + 30)
#
#     }
#   }
#
#
# } # End of model
#
# "),fill=TRUE)
# sink()
#
#
# # Will comment this out and pull in the saved output below because it
# # takes a while to run and requires jags installation
# ipak("rjags")
#
# biomass_jags <- jags.model(jags_script_string, data = dataList,
#                            n.adapt = 10000,
#                            n.chains = 3)
#
#
# update(biomass_jags, 10000)
#
# variables_to_sample <- c("beta_a_0",
#                          a_vars_to_sample,
#
#                          "beta_b_0",
#                          b_vars_to_sample,
#
#                          "rand_a_s",
#                          "rand_b_s",
#                          "pred_mean",
#                          "pred_biomass",
#
#                          "pred_mean30",
#                          "sig_log_bio"
# )
#
# biomass_samples <- coda.samples(biomass_jags,
#                                 variable.names = variables_to_sample,
#                                 thin = 500,
#                                 n.iter = 500000)
#
# # Save biomass samples and a few other helper vectors
# # Will include attributes for centering/scaling
# scale_df <- tibble(coef = "amt",
#                    center = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$AMT))[[2]],
#                    scale = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$AMT))[[3]]) %>%
#   bind_rows(tibble(coef = "tap",
#                    center = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$AMP))[[2]],
#                    scale = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$AMP))[[3]])) %>%
#   bind_rows(tibble(coef = "npp",
#                    center = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$NPP))[[2]],
#                    scale = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$NPP))[[3]])) %>%
#   bind_rows(tibble(coef = "grazers",
#                    center = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$grazers))[[2]],
#                    scale = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$grazers))[[3]])) %>%
#   bind_rows(tibble(coef = "fire",
#                    center = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$fire))[[2]],
#                    scale = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$fire))[[3]])) %>%
#   bind_rows(tibble(coef = "drought",
#                    center = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$drought))[[2]],
#                    scale = attributes(scale(j_dat[!duplicated(j_dat$SITE),]$drought))[[3]]))
#
# # Actually will be easier if the rownames are the var_names
# scale_df <- as.data.frame(scale_df)
# rownames(scale_df) <- scale_df$coef
# scale_df <- scale_df[,-1]
#
# save(file = "./outputs/biomass_samples.RData",
#      biomass_samples,
#      variables_to_sample,
#      cov_to_var,
#      cov_to_name,
#      cov_to_sample,
#      n_cov,
#      a_vars_to_sample,
#      b_vars_to_sample,
#      scale_df)
#
#
# # Examine chains
# MCMCvis::MCMCtrace(biomass_samples,
#                    params = c(
#                      "sig_log_bio",
#
#                      "beta_a_0",
#                      a_vars_to_sample,
#
#                      "beta_b_0",
#                      b_vars_to_sample,
#                      "rand_a_s",
#                      "rand_b_s"
#                    ),
#                    ISB = T,
#                    Rhat = T,
#                    n.eff = T,
#                    ind = T,
#                    pdf = T)


# # Conduct a similar analysis but without the dispersal variable in the model.
# # This serves to fit a model of forest growth that uses typical environmental
# # variables.
#
# # Again, will save the model output and comment this out
#
# nd_cov_to_var <- c("AMP", "AMT", "NPP", "GRAZERS", "FIRE", "DROUGHT")
# nd_cov_to_name <- c("AMP", "AMT", "NPP", "GRAZERS", "FIRE", "DROUGHT")
#
# nd_cov_to_sample <- c("c2", "c3", "c4", "c5", "c6", "c7")
#
# n_nd_cov <- length(nd_cov_to_sample)
#
# a_vars_to_sample_nd <- paste("beta_a_",  nd_cov_to_sample, sep = "")
# b_vars_to_sample_nd <- paste("beta_b_",  nd_cov_to_sample, sep = "")
#
# # Define model
# jags_script_string <- "./analysis/biomass_nd_jags.txt"
# sink(jags_script_string)
# cat(paste0("
#
# model {
#
#   # Likelihood
#
#   for(i in 1:n){
#
#     # Biomass values lognormally distrubuted around a Monod function
#     BIOMASS[i] ~ dlnorm(log(mean[i]), tau_log_bio[MONO_for_SD[i]])
#     mean[i] <- a[SITE[i]] * AGE[i] / (b[SITE[i]] + AGE[i])
#
#   }
#
#   for(i in 1:n_site){
#     a[i] <- exp(beta_a_0 +
#                 beta_a_c2 * c2[i] +
#                 beta_a_c3 * c3[i] +
#                 beta_a_c4 * c4[i] +
#                 beta_a_mono * MONO[i] +
#                 rand_a_s[SITE[i]]
#                 )
#
#     b[i] <- exp(beta_b_0 +
#                 beta_b_c2 * c2[i] +
#                 beta_b_c3 * c3[i] +
#                 beta_b_c4 * c4[i] +
#                 beta_b_c5 * c5[i] +
#                 beta_b_c6 * c6[i] +
#                 beta_b_c7 * c7[i] +
#                 beta_b_inter * INTER[i] +
#                 beta_b_mono * MONO[i] +
#                 rand_b_s[SITE[i]]
#                 )
#
#   }
#
#
#   # Priors
#
#   beta_a_0 ~ dnorm(5, 1) # Roughly reasonable max carbon estimates
#
#   beta_a_c2 ~ dnorm(0, 0.1)
#   beta_a_c3 ~ dnorm(0, 0.1)
#   beta_a_c4 ~ dnorm(0, 0.1)
#   beta_a_mono ~ dnorm(0, 0.1)
#
#   beta_b_0 ~ dnorm(3, 0.1) # Using reasonable times to half saturation
#
#   beta_b_c2 ~ dnorm(0, 0.1)
#   beta_b_c3 ~ dnorm(0, 0.1)
#   beta_b_c4 ~ dnorm(0, 0.1)
#   beta_b_c5 ~ dnorm(0, 0.1)
#   beta_b_c6 ~ dnorm(0, 0.1)
#   beta_b_c7 ~ dnorm(0, 0.1)
#
#   beta_b_inter ~ dnorm(0, 0.1)T(,0) # Expect negative
#   beta_b_mono ~ dnorm(0, 0.1)
#
#
#   # Random effect for prior use, with sum to zero constraints
#   for(i in 1:(n_site-1)){
#     rand_a_s[i] ~ dnorm(0, tau_a_s)
#     rand_b_s[i] ~ dnorm(0, tau_b_s)
#   }
#   rand_a_s[n_site] <- -sum(rand_a_s[1:(n_site-1)])
#   rand_b_s[n_site] <- -sum(rand_b_s[1:(n_site-1)])
#
#   tau_a_s <- pow(sig_a_s, -2)
#   sig_a_s ~ dunif(0,10)
#   tau_b_s <- pow(sig_b_s, -2)
#   sig_b_s ~ dunif(0,10)
#
#   for(i in 1:2){
#     tau_log_bio[i] <- pow(sig_log_bio[i], -2)
#     sig_log_bio[i] ~ dunif(0, 10)
#   }
#
#
# } # End of model
#
# "),fill=TRUE)
# sink()
#
#
# # Will comment this out and pull in the saved output below because it
# # takes a while to run and requires jags installation
# ipak("rjags")
#
# biomass_nd_jags <- jags.model(jags_script_string, data = dataList,
#                            n.adapt = 10000,
#                            n.chains = 3)
#
#
# update(biomass_nd_jags, 10000)
#
# variables_to_sample <- c("beta_a_0",
#                          a_vars_to_sample_nd,
#
#                          "beta_b_0",
#                          b_vars_to_sample_nd,
#
#                          "rand_a_s",
#                          "rand_b_s",
#                          "pred_mean",
#                          "pred_biomass",
#
#                          "pred_mean30",
#                          "sig_log_bio"
# )
#
# biomass_nd_samples <- coda.samples(biomass_nd_jags,
#                                 variable.names = variables_to_sample,
#                                 thin = 500,
#                                 n.iter = 500000)
#
# save(file = "./outputs/biomass_nd_samples.RData",
#      biomass_nd_samples,
#      variables_to_sample,
#      nd_cov_to_var,
#      nd_cov_to_name,
#      nd_cov_to_sample,
#      n_nd_cov,
#      a_vars_to_sample_nd,
#      b_vars_to_sample_nd)
#
#
# # Examine chains
# MCMCvis::MCMCtrace(biomass_nd_samples,
#                    params = c(
#                      "sig_log_bio",
#
#                      "beta_a_0",
#                      a_vars_to_sample_nd,
#
#                      "beta_b_0",
#                      b_vars_to_sample_nd,
#                      #"rand_a_s",
#                      "rand_b_s"
#                    ),
#                    ISB = T,
#                    Rhat = T,
#                    n.eff = T,
#                    ind = T,
#                    pdf = T)






# Figures ----------------------------------------------------------------------

# Load posterior samples if not run above
load(file = "./outputs/biomass_samples.RData")

# Get posterior means and credible interval vals
biomass_samples_df <- do.call(rbind, biomass_samples)
bsm <- colMeans(biomass_samples_df)
bs975 <-apply(biomass_samples_df, 2, function(x) quantile(x, 0.975))
bs025 <-apply(biomass_samples_df, 2, function(x) quantile(x, 0.025))

# Get the names of these coefs
beta_vec <- names(bsm)[grepl("beta", names(bsm))]
pred_vec <- names(bsm)[grepl("pred_biomass", names(bsm))]
mean_vec <- names(bsm)[grepl("pred_mean", names(bsm))]




# A couple functions to help plotting the results
sample_posterior <- function(chains){
  cc <- sample(1:length(chains), 1)
  ii <- sample(1:length(chains[[1]][,1]), 1)

  chains[[cc]][ii, beta_vec]
}

# Standardized effect sizes in terms of their effect on annualized carbon accumulation

# Note that the predictor variables were scaled prior to analysis, with values
# centered around zero. The dispersal integrity values weren't, so to put all
# these effect sizes into comparable units, will use a 1 sd unit. Also make this
# negative to make visualizations in terms of seed dispersal disruption.

# Below is the (kinda clunky) way we'll calculate the effect size on
# annualized growth rate.

aa1 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c())
names(aa1) <- c("pNPP", "AMT", "TAP", "Fire", "Grazers", "Drought", "DD (natural)", "DD (monoculture)", "Intervention")
bb1 <- aa1
aa2 <- aa1
bb2 <- aa1
effect_size <- aa1

mean_c1 <- j_dat[!duplicated(j_dat$SITE),]$DISP %>% mean()
val_c1 <- mean_c1 - j_dat[!duplicated(j_dat$SITE),]$DISP %>% sd()
val_c2 <- 1
val_c3 <- 1
val_c4 <- 1
val_c5 <- 1
val_c6 <- 1
val_c7 <- -1
val_mono <- 1
val_inter <- 1

biomass_samples_df <- biomass_samples_df %>% as.data.frame()

# pNPP
aa1[["pNPP"]] <- exp(biomass_samples_df$beta_a_0 +
                       biomass_samples_df$beta_a_c2 * 0 +
                       biomass_samples_df$beta_a_c3 * 0 +
                       biomass_samples_df$beta_a_c4 * 0 +
                       biomass_samples_df$beta_a_mono * 0)

bb1[["pNPP"]] <- exp(biomass_samples_df$beta_b_0 +
                       biomass_samples_df$beta_b_c1 * mean_c1 +
                       biomass_samples_df$beta_b_c2 * 0 +
                       biomass_samples_df$beta_b_c3 * 0 +
                       biomass_samples_df$beta_b_c4 * 0 +
                       biomass_samples_df$beta_b_c5 * 0 +
                       biomass_samples_df$beta_b_c6 * 0 +
                       biomass_samples_df$beta_b_c7 * 0 +
                       biomass_samples_df$beta_b_mono * 0 +
                       biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

aa2[["pNPP"]] <- exp(biomass_samples_df$beta_a_0 +
                       biomass_samples_df$beta_a_c2 * 0 +
                       biomass_samples_df$beta_a_c3 * 0 +
                       biomass_samples_df$beta_a_c4 * val_c4 +
                       biomass_samples_df$beta_a_mono * 0)

bb2[["pNPP"]] <- exp(biomass_samples_df$beta_b_0 +
                       biomass_samples_df$beta_b_c1 * mean_c1 +
                       biomass_samples_df$beta_b_c2 * 0 +
                       biomass_samples_df$beta_b_c3 * 0 +
                       biomass_samples_df$beta_b_c4 * val_c4 +
                       biomass_samples_df$beta_b_c5 * 0 +
                       biomass_samples_df$beta_b_c6 * 0 +
                       biomass_samples_df$beta_b_c7 * 0 +
                       biomass_samples_df$beta_b_mono * 0 +
                       biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

effect_size[["pNPP"]] <- (aa2[["pNPP"]] * 30 / (bb2[["pNPP"]] + 30) / 30) -
  (aa1[["pNPP"]] * 30 / (bb1[["pNPP"]] + 30) / 30)
#hist(effect_size[["pNPP"]])




# AMT
aa1[["AMT"]] <- exp(biomass_samples_df$beta_a_0 +
                      biomass_samples_df$beta_a_c2 * 0 +
                      biomass_samples_df$beta_a_c3 * 0 +
                      biomass_samples_df$beta_a_c4 * 0 +
                      biomass_samples_df$beta_a_mono * 0)

bb1[["AMT"]] <- exp(biomass_samples_df$beta_b_0 +
                      biomass_samples_df$beta_b_c1 * mean_c1 +
                      biomass_samples_df$beta_b_c2 * 0 +
                      biomass_samples_df$beta_b_c3 * 0 +
                      biomass_samples_df$beta_b_c4 * 0 +
                      biomass_samples_df$beta_b_c5 * 0 +
                      biomass_samples_df$beta_b_c6 * 0 +
                      biomass_samples_df$beta_b_c7 * 0 +
                      biomass_samples_df$beta_b_mono * 0 +
                      biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

aa2[["AMT"]] <- exp(biomass_samples_df$beta_a_0 +
                      biomass_samples_df$beta_a_c2 * 0 +
                      biomass_samples_df$beta_a_c3 * val_c3 +
                      biomass_samples_df$beta_a_c4 * 0 +
                      biomass_samples_df$beta_a_mono * 0)

bb2[["AMT"]] <- exp(biomass_samples_df$beta_b_0 +
                      biomass_samples_df$beta_b_c1 * mean_c1 +
                      biomass_samples_df$beta_b_c2 * 0 +
                      biomass_samples_df$beta_b_c3 * val_c3 +
                      biomass_samples_df$beta_b_c4 * 0 +
                      biomass_samples_df$beta_b_c5 * 0 +
                      biomass_samples_df$beta_b_c6 * 0 +
                      biomass_samples_df$beta_b_c7 * 0 +
                      biomass_samples_df$beta_b_mono * 0 +
                      biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

effect_size[["AMT"]] <- (aa2[["AMT"]] * 30 / (bb2[["AMT"]] + 30) / 30) -
  (aa1[["AMT"]] * 30 / (bb1[["AMT"]] + 30) / 30)
#hist(effect_size[["AMT"]])




# TAP
aa1[["TAP"]] <- exp(biomass_samples_df$beta_a_0 +
                      biomass_samples_df$beta_a_c2 * 0 +
                      biomass_samples_df$beta_a_c3 * 0 +
                      biomass_samples_df$beta_a_c4 * 0 +
                      biomass_samples_df$beta_a_mono * 0)

bb1[["TAP"]] <- exp(biomass_samples_df$beta_b_0 +
                      biomass_samples_df$beta_b_c1 * mean_c1 +
                      biomass_samples_df$beta_b_c2 * 0 +
                      biomass_samples_df$beta_b_c3 * 0 +
                      biomass_samples_df$beta_b_c4 * 0 +
                      biomass_samples_df$beta_b_c5 * 0 +
                      biomass_samples_df$beta_b_c6 * 0 +
                      biomass_samples_df$beta_b_c7 * 0 +
                      biomass_samples_df$beta_b_mono * 0 +
                      biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

aa2[["TAP"]] <- exp(biomass_samples_df$beta_a_0 +
                      biomass_samples_df$beta_a_c2 * val_c2 +
                      biomass_samples_df$beta_a_c3 * 0 +
                      biomass_samples_df$beta_a_c4 * 0 +
                      biomass_samples_df$beta_a_mono * 0)

bb2[["TAP"]] <- exp(biomass_samples_df$beta_b_0 +
                      biomass_samples_df$beta_b_c1 * mean_c1 +
                      biomass_samples_df$beta_b_c2 * val_c2 +
                      biomass_samples_df$beta_b_c3 * 0 +
                      biomass_samples_df$beta_b_c4 * 0 +
                      biomass_samples_df$beta_b_c5 * 0 +
                      biomass_samples_df$beta_b_c6 * 0 +
                      biomass_samples_df$beta_b_c7 * 0 +
                      biomass_samples_df$beta_b_mono * 0 +
                      biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

effect_size[["TAP"]] <- (aa2[["TAP"]] * 30 / (bb2[["TAP"]] + 30) / 30) -
  (aa1[["TAP"]] * 30 / (bb1[["TAP"]] + 30) / 30)
#hist(effect_size[["TAP"]])



# Fire
aa1[["Fire"]] <- exp(biomass_samples_df$beta_a_0 +
                       biomass_samples_df$beta_a_c2 * 0 +
                       biomass_samples_df$beta_a_c3 * 0 +
                       biomass_samples_df$beta_a_c4 * 0 +
                       biomass_samples_df$beta_a_mono * 0)

bb1[["Fire"]] <- exp(biomass_samples_df$beta_b_0 +
                       biomass_samples_df$beta_b_c1 * mean_c1 +
                       biomass_samples_df$beta_b_c2 * 0 +
                       biomass_samples_df$beta_b_c3 * 0 +
                       biomass_samples_df$beta_b_c4 * 0 +
                       biomass_samples_df$beta_b_c5 * 0 +
                       biomass_samples_df$beta_b_c6 * 0 +
                       biomass_samples_df$beta_b_c7 * 0 +
                       biomass_samples_df$beta_b_mono * 0 +
                       biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

aa2[["Fire"]] <- exp(biomass_samples_df$beta_a_0 +
                       biomass_samples_df$beta_a_c2 * 0 +
                       biomass_samples_df$beta_a_c3 * 0 +
                       biomass_samples_df$beta_a_c4 * 0 +
                       biomass_samples_df$beta_a_mono * 0)

bb2[["Fire"]] <- exp(biomass_samples_df$beta_b_0 +
                       biomass_samples_df$beta_b_c1 * mean_c1 +
                       biomass_samples_df$beta_b_c2 * 0 +
                       biomass_samples_df$beta_b_c3 * 0 +
                       biomass_samples_df$beta_b_c4 * 0 +
                       biomass_samples_df$beta_b_c5 * val_c5 +
                       biomass_samples_df$beta_b_c6 * 0 +
                       biomass_samples_df$beta_b_c7 * 0 +
                       biomass_samples_df$beta_b_mono * 0 +
                       biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

effect_size[["Fire"]] <- (aa2[["Fire"]] * 30 / (bb2[["Fire"]] + 30) / 30) -
  (aa1[["Fire"]] * 30 / (bb1[["Fire"]] + 30) / 30)
#hist(effect_size[["Fire"]])


# Grazers
aa1[["Grazers"]] <- exp(biomass_samples_df$beta_a_0 +
                          biomass_samples_df$beta_a_c2 * 0 +
                          biomass_samples_df$beta_a_c3 * 0 +
                          biomass_samples_df$beta_a_c4 * 0 +
                          biomass_samples_df$beta_a_mono * 0)

bb1[["Grazers"]] <- exp(biomass_samples_df$beta_b_0 +
                          biomass_samples_df$beta_b_c1 * mean_c1 +
                          biomass_samples_df$beta_b_c2 * 0 +
                          biomass_samples_df$beta_b_c3 * 0 +
                          biomass_samples_df$beta_b_c4 * 0 +
                          biomass_samples_df$beta_b_c5 * 0 +
                          biomass_samples_df$beta_b_c6 * 0 +
                          biomass_samples_df$beta_b_c7 * 0 +
                          biomass_samples_df$beta_b_mono * 0 +
                          biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

aa2[["Grazers"]] <- exp(biomass_samples_df$beta_a_0 +
                          biomass_samples_df$beta_a_c2 * 0 +
                          biomass_samples_df$beta_a_c3 * 0 +
                          biomass_samples_df$beta_a_c4 * 0 +
                          biomass_samples_df$beta_a_mono * 0)

bb2[["Grazers"]] <- exp(biomass_samples_df$beta_b_0 +
                          biomass_samples_df$beta_b_c1 * mean_c1 +
                          biomass_samples_df$beta_b_c2 * 0 +
                          biomass_samples_df$beta_b_c3 * 0 +
                          biomass_samples_df$beta_b_c4 * 0 +
                          biomass_samples_df$beta_b_c5 * 0 +
                          biomass_samples_df$beta_b_c6 * val_c6 +
                          biomass_samples_df$beta_b_c7 * 0 +
                          biomass_samples_df$beta_b_mono * 0 +
                          biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

effect_size[["Grazers"]] <- (aa2[["Grazers"]] * 30 / (bb2[["Grazers"]] + 30) / 30) -
  (aa1[["Grazers"]] * 30 / (bb1[["Grazers"]] + 30) / 30)
#hist(effect_size[["Grazers"]])



# Drought
aa1[["Drought"]] <- exp(biomass_samples_df$beta_a_0 +
                          biomass_samples_df$beta_a_c2 * 0 +
                          biomass_samples_df$beta_a_c3 * 0 +
                          biomass_samples_df$beta_a_c4 * 0 +
                          biomass_samples_df$beta_a_mono * 0)

bb1[["Drought"]] <- exp(biomass_samples_df$beta_b_0 +
                          biomass_samples_df$beta_b_c1 * mean_c1 +
                          biomass_samples_df$beta_b_c2 * 0 +
                          biomass_samples_df$beta_b_c3 * 0 +
                          biomass_samples_df$beta_b_c4 * 0 +
                          biomass_samples_df$beta_b_c5 * 0 +
                          biomass_samples_df$beta_b_c6 * 0 +
                          biomass_samples_df$beta_b_c7 * 0 +
                          biomass_samples_df$beta_b_mono * 0 +
                          biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

aa2[["Drought"]] <- exp(biomass_samples_df$beta_a_0 +
                          biomass_samples_df$beta_a_c2 * 0 +
                          biomass_samples_df$beta_a_c3 * 0 +
                          biomass_samples_df$beta_a_c4 * 0 +
                          biomass_samples_df$beta_a_mono * 0)

bb2[["Drought"]] <- exp(biomass_samples_df$beta_b_0 +
                          biomass_samples_df$beta_b_c1 * mean_c1 +
                          biomass_samples_df$beta_b_c2 * 0 +
                          biomass_samples_df$beta_b_c3 * 0 +
                          biomass_samples_df$beta_b_c4 * 0 +
                          biomass_samples_df$beta_b_c5 * 0 +
                          biomass_samples_df$beta_b_c6 * 0 +
                          biomass_samples_df$beta_b_c7 * val_c7 +
                          biomass_samples_df$beta_b_mono * 0 +
                          biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

effect_size[["Drought"]] <- (aa2[["Drought"]] * 30 / (bb2[["Drought"]] + 30) / 30) -
  (aa1[["Drought"]] * 30 / (bb1[["Drought"]] + 30) / 30)
#hist(effect_size[["Drought"]])



# DD (natural)
aa1[["DD (natural)"]] <- exp(biomass_samples_df$beta_a_0 +
                               biomass_samples_df$beta_a_c2 * 0 +
                               biomass_samples_df$beta_a_c3 * 0 +
                               biomass_samples_df$beta_a_c4 * 0 +
                               biomass_samples_df$beta_a_mono * 0)

bb1[["DD (natural)"]] <- exp(biomass_samples_df$beta_b_0 +
                               biomass_samples_df$beta_b_c1 * mean_c1 +
                               biomass_samples_df$beta_b_c2 * 0 +
                               biomass_samples_df$beta_b_c3 * 0 +
                               biomass_samples_df$beta_b_c4 * 0 +
                               biomass_samples_df$beta_b_c5 * 0 +
                               biomass_samples_df$beta_b_c6 * 0 +
                               biomass_samples_df$beta_b_c7 * 0 +
                               biomass_samples_df$beta_b_mono * 0 +
                               biomass_samples_df$beta_b_mono_intx_c1 * 0 * mean_c1)

aa2[["DD (natural)"]] <- exp(biomass_samples_df$beta_a_0 +
                               biomass_samples_df$beta_a_c2 * 0 +
                               biomass_samples_df$beta_a_c3 * 0 +
                               biomass_samples_df$beta_a_c4 * 0 +
                               biomass_samples_df$beta_a_mono * 0)

bb2[["DD (natural)"]] <- exp(biomass_samples_df$beta_b_0 +
                               biomass_samples_df$beta_b_c1 * val_c1 +
                               biomass_samples_df$beta_b_c2 * 0 +
                               biomass_samples_df$beta_b_c3 * 0 +
                               biomass_samples_df$beta_b_c4 * 0 +
                               biomass_samples_df$beta_b_c5 * 0 +
                               biomass_samples_df$beta_b_c6 * 0 +
                               biomass_samples_df$beta_b_c7 * 0 +
                               biomass_samples_df$beta_b_mono * 0 +
                               biomass_samples_df$beta_b_mono_intx_c1 * 0 * val_c1)

effect_size[["DD (natural)"]] <- (aa2[["DD (natural)"]] * 30 / (bb2[["DD (natural)"]] + 30) / 30) -
  (aa1[["DD (natural)"]] * 30 / (bb1[["DD (natural)"]] + 30) / 30)
#hist(effect_size[["DD (natural)"]])



# DD (monoculture)
aa1[["DD (monoculture)"]] <- exp(biomass_samples_df$beta_a_0 +
                                   biomass_samples_df$beta_a_c2 * 0 +
                                   biomass_samples_df$beta_a_c3 * 0 +
                                   biomass_samples_df$beta_a_c4 * 0 +
                                   biomass_samples_df$beta_a_mono * val_mono)

bb1[["DD (monoculture)"]] <- exp(biomass_samples_df$beta_b_0 +
                                   biomass_samples_df$beta_b_c1 * mean_c1 +
                                   biomass_samples_df$beta_b_c2 * 0 +
                                   biomass_samples_df$beta_b_c3 * 0 +
                                   biomass_samples_df$beta_b_c4 * 0 +
                                   biomass_samples_df$beta_b_c5 * 0 +
                                   biomass_samples_df$beta_b_c6 * 0 +
                                   biomass_samples_df$beta_b_c7 * 0 +
                                   biomass_samples_df$beta_b_mono * 0 +
                                   biomass_samples_df$beta_b_mono_intx_c1 * val_mono * mean_c1)

aa2[["DD (monoculture)"]] <- exp(biomass_samples_df$beta_a_0 +
                                   biomass_samples_df$beta_a_c2 * 0 +
                                   biomass_samples_df$beta_a_c3 * 0 +
                                   biomass_samples_df$beta_a_c4 * 0 +
                                   biomass_samples_df$beta_a_mono * val_mono)

bb2[["DD (monoculture)"]] <- exp(biomass_samples_df$beta_b_0 +
                                   biomass_samples_df$beta_b_c1 * val_c1 +
                                   biomass_samples_df$beta_b_c2 * 0 +
                                   biomass_samples_df$beta_b_c3 * 0 +
                                   biomass_samples_df$beta_b_c4 * 0 +
                                   biomass_samples_df$beta_b_c5 * 0 +
                                   biomass_samples_df$beta_b_c6 * 0 +
                                   biomass_samples_df$beta_b_c7 * 0 +
                                   biomass_samples_df$beta_b_mono * 0 +
                                   biomass_samples_df$beta_b_mono_intx_c1 * val_mono * val_c1)

effect_size[["DD (monoculture)"]] <- (aa2[["DD (monoculture)"]] * 30 / (bb2[["DD (monoculture)"]] + 30) / 30) -
  (aa1[["DD (monoculture)"]] * 30 / (bb1[["DD (monoculture)"]] + 30) / 30)
#hist(effect_size[["DD (monoculture)"]])



# Intervention
aa1[["Intervention"]] <- exp(biomass_samples_df$beta_a_0 +
                               biomass_samples_df$beta_a_c2 * 0 +
                               biomass_samples_df$beta_a_c3 * 0 +
                               biomass_samples_df$beta_a_c4 * 0 +
                               biomass_samples_df$beta_a_mono * val_mono)

bb1[["Intervention"]] <- exp(biomass_samples_df$beta_b_0 +
                               biomass_samples_df$beta_b_c1 * mean_c1 +
                               biomass_samples_df$beta_b_c2 * 0 +
                               biomass_samples_df$beta_b_c3 * 0 +
                               biomass_samples_df$beta_b_c4 * 0 +
                               biomass_samples_df$beta_b_c5 * 0 +
                               biomass_samples_df$beta_b_c6 * 0 +
                               biomass_samples_df$beta_b_c7 * 0 +
                               biomass_samples_df$beta_b_mono * val_mono +
                               biomass_samples_df$beta_b_mono_intx_c1 * val_mono * mean_c1)

aa2[["Intervention"]] <- exp(biomass_samples_df$beta_a_0 +
                               biomass_samples_df$beta_a_c2 * 0 +
                               biomass_samples_df$beta_a_c3 * 0 +
                               biomass_samples_df$beta_a_c4 * 0 +
                               biomass_samples_df$beta_a_mono * val_mono)

bb2[["Intervention"]] <- exp(biomass_samples_df$beta_b_0 +
                               biomass_samples_df$beta_b_c1 * mean_c1 +
                               biomass_samples_df$beta_b_c2 * 0 +
                               biomass_samples_df$beta_b_c3 * 0 +
                               biomass_samples_df$beta_b_c4 * 0 +
                               biomass_samples_df$beta_b_c5 * 0 +
                               biomass_samples_df$beta_b_c6 * 0 +
                               biomass_samples_df$beta_b_c7 * 0 +
                               biomass_samples_df$beta_b_mono * val_mono +
                               biomass_samples_df$beta_b_mono_intx_c1 * val_mono * mean_c1 +
                               biomass_samples_df$beta_b_inter * val_inter)

effect_size[["Intervention"]] <- (aa2[["Intervention"]] * 30 / (bb2[["Intervention"]] + 30) / 30) -
  (aa1[["Intervention"]] * 30 / (bb1[["Intervention"]] + 30) / 30)
#hist(effect_size[["Intervention"]])



# Make a table of standardized effect sizes.
med_vals <- lapply(effect_size, median)

q50_lo <- lapply(effect_size, function(x) quantile(x, 0.25)) %>% unlist()
q50_hi <- lapply(effect_size, function(x) quantile(x, 0.75)) %>% unlist()

q95_lo <- lapply(effect_size, function(x) quantile(x, 0.025)) %>% unlist()
q95_hi <- lapply(effect_size, function(x) quantile(x, 0.975)) %>% unlist()

q99_lo <- lapply(effect_size, function(x) quantile(x, 0.005)) %>% unlist()
q99_hi <- lapply(effect_size, function(x) quantile(x, 0.995)) %>% unlist()


tibble(coef = names(med_vals),
       med = med_vals %>% unlist() %>% signif(digits = 3),
       lo99 = q99_lo %>% signif(digits = 3),
       lo95 = q95_lo %>% signif(digits = 3),
       hi95 = q95_hi %>% signif(digits = 3),
       hi99 = q99_hi %>% signif(digits = 3)) %>% write.csv("~/Downloads/posterior_std_effect_sizes.csv")



# Now set up for a caterpillar plot for the natural regrowth effects
aa1 <- aa1[c("TAP", "AMT", "pNPP", "Grazers", "Fire", "Drought", "DD (natural)")]
bb1 <- bb1[c("TAP", "AMT", "pNPP", "Grazers", "Fire", "Drought", "DD (natural)")]
aa2 <- aa2[c("TAP", "AMT", "pNPP", "Grazers", "Fire", "Drought", "DD (natural)")]
bb2 <- bb2[c("TAP", "AMT", "pNPP", "Grazers", "Fire", "Drought", "DD (natural)")]
effect_size <- effect_size[c("TAP", "AMT", "pNPP", "Grazers", "Fire", "Drought", "DD (natural)")]


png(file = "./outputs/figures/biomass model results.png",
    units = "in",
    width = 7.24,
    height = 2.5,#6,
    res = 440)

par(mfrow=c(1,3))
orig_mar <- c(3.5, 3.9, 0.1, 0.1)
orig_mar <- c(3, 4.7, 0.1, 0.6)
#5.1 4.1 4.1 2.1
par(mar = orig_mar)
set.seed(111)

# Plot aboveground biomass over stand age based on model predictions that
# account for site-level non-independence and error

j_dat$pred_a <- exp(bsm["beta_a_0"] +
                      bsm["beta_a_c2"] * ((j_dat$AMP - scale_df["tap", "center"]) / scale_df["tap", "scale"]) +
                      bsm["beta_a_c3"] * ((j_dat$AMT - scale_df["amt", "center"]) / scale_df["amt", "scale"]) +
                      bsm["beta_a_c4"] * ((j_dat$NPP - scale_df["npp", "center"]) / scale_df["npp", "scale"])
)
j_dat$pred_b <- exp(bsm["beta_b_0"] +
                      bsm["beta_b_c1"] * j_dat$DISP +
                      bsm["beta_b_c2"] * ((j_dat$AMP - scale_df["tap", "center"]) / scale_df["tap", "scale"]) +
                      bsm["beta_b_c3"] * ((j_dat$AMT - scale_df["amt", "center"]) / scale_df["amt", "scale"]) +
                      bsm["beta_b_c4"] * ((j_dat$NPP - scale_df["npp", "center"]) / scale_df["npp", "scale"]) +
                      bsm["beta_b_c5"] * ((j_dat$GRAZERS - scale_df["grazers", "center"]) / scale_df["grazers", "scale"]) +
                      bsm["beta_b_c6"] * ((j_dat$FIRE - scale_df["fire", "center"]) / scale_df["fire", "scale"]) +
                      bsm["beta_b_c7"] * ((j_dat$DROUGHT - scale_df["drought", "center"]) / scale_df["drought", "scale"]))
j_dat$pred_est <- j_dat$pred_a * j_dat$stand_age / (j_dat$pred_b + j_dat$stand_age)


legend_cex <- 1

polygon_alpha <- 0.8

mgp_vals_y <- c(3, 0.5, 0)
mgp_vals_x <- c(3, 0.4, 0)
tck_val <- -0.03

axis_cex <- 1

line_x <- 1.7
line_y <- 2

plot(NA,
     xlim = c(0, 50),
     ylim = c(0, 120),
     las = 1,
     xlab = "",#"Stand age (years)",
     ylab = "",#"Aboveground carbon (MgC/ha)",
     xaxt = "n",
     yaxt = "n",
     frame = F,
     mgp = mgp_vals_y,
     tck = tck_val)
mtext("Stand age", side = 1, line = line_x)
mtext("Aboveground\ncarbon (MgC/ha)", side = 2, line = line_y)

text(-19, 120, "A", xpd = T,
     cex = 1.2,
     font = 2)

axis(1, at = c(0, 25, 50),
     labels = c("0", "25", ">50"),
     mgp = mgp_vals_x,
     tck = tck_val,
     cex.axis = axis_cex)
axis(2, at = c(0, 40, 80, 120),
     labels = c("0", "40", "80", ">120"),
     mgp = mgp_vals_y,
     tck = tck_val,
     las = 1,
     cex.axis = axis_cex)

color_range <- colorRampPalette(c("red", "purple", "blue"))(100)
j_plot <- j_dat %>% #j_dat[sample(1:nrow(j_dat)),] %>%
  arrange(ratio) %>%
  filter(monoculture == "no") %>%
  filter(!duplicated(paste(site_id, stand_age)))
points(x = ifelse(j_plot$stand_age > 50, 50, j_plot$stand_age),
       y = ifelse(j_plot$pred_est > 120, 120, j_plot$pred_est),
       col = alpha(color_range[findInterval(j_plot$ratio, seq(0, 0.58, length.out = 100))], 0.5),
       pch = 16,
       cex = 0.8)

# A super clunky way to show a gradient legend
library("fields")
par(new = TRUE, pty = "m", pin = c(0.2, 1))
image.plot(zlim = c(-1, 1),
           col = alpha(rev(color_range), 0.7),
           smallplot = c(.3, .55, .82, .85),
           horizontal = T,
           legend.only = TRUE,
           legend.width = 0.3,
           legend.shrink = 3,
           axis.args = list(xaxt = "n"),
           legend.mar = 11)
text(-25, 210,
     pos = 1,
     "Dispersal\ndisruption",
     col = "grey30",
     cex = legend_cex,
     font = 3)
text(x = c(-55, 25, 105) - 50,
     y = 155,
     labels = c("0", "0.5", "1"),
     cex = legend_cex,
     col = "grey30")


# B


med_vals <- lapply(effect_size, median)

color_interpolate <- function(values, low_color, mid_color, high_color) {
  # Create color palettes
  below_zero_palette <- colorRampPalette(c(low_color, mid_color))
  above_zero_palette <- colorRampPalette(c(mid_color, high_color))

  # Find values below and above zero
  below_zero <- values[values < 0]
  above_zero <- values[values > 0]
  max_range <- max(abs(values))

  # Normalize values to range between 0 and 1 within their respective segments
  norm_below <- scales::rescale(below_zero, to = c(0, 1), from = c(-max_range, 0))
  norm_below <- norm_below^2
  norm_above <- scales::rescale(above_zero, to = c(0, 1), from = c(0, max_range))
  norm_above <- sqrt(norm_above)

  # Interpolate colors
  below_colors <- below_zero_palette(100)[round(norm_below * 99) + 1]
  above_colors <- above_zero_palette(100)[round(norm_above * 99) + 1]

  # Combine colors and map to original values
  colors <- rep(mid_color, length(values))
  colors[values < 0] <- below_colors
  colors[values > 0] <- above_colors

  return(colors)
}

grad_cols <- color_interpolate(unlist(med_vals), low_color = "#ef8a62",
                               mid_color = "grey90",
                               high_color = "#67a9cf")

par(mar = c(3, 7.9, 0.1, 0.1))
y_vals <- -length(names(aa1)):-1
widths <- c(5,3,1)

plot(NA,
     ylim = c(-0.75, -length(names(aa1)) - 0.25),
     xlim = c(-0.6, 0.6),
     frame = F,
     ylab = "",
     xlab = "",
     yaxt = "n",
     xaxt = "n",
     las = 1)
axis(1, at = c(-0.6, -0.3, 0, 0.3, 0.6),
     mgp = mgp_vals_x,
     tck = tck_val,
     cex.axis = axis_cex)
mtext("Standardized effect size", side = 1, line = line_x)

abline(v = 0,
       lty = 2)
text(y = y_vals, x = -0.55,
     #labels = names(effect_size),
     labels = c("Precipitation", "Temperature", "Potential NPP",
                "Grazing livestock", "Fire prevalence", "Drought edge effects",
                "Dispersal disruption"), #c("TAP", "AMT", "pNPP", "Grazers", "Fire", "Drought", "DD (natural)")
     cex = legend_cex*1.05,
     pos = 2,
     xpd = T)

# q50_lo <- lapply(effect_size, function(x) quantile(x, 0.25)) %>% unlist()
# q50_hi <- lapply(effect_size, function(x) quantile(x, 0.75)) %>% unlist()
# segments(y0 = y_vals,
#          y1 = y_vals,
#          x0 = q50_lo,
#          x1 = q50_hi,
#          lwd = widths[1],
#          lend = "butt")

q95_lo <- lapply(effect_size, function(x) quantile(x, 0.025)) %>% unlist()
q95_hi <- lapply(effect_size, function(x) quantile(x, 0.975)) %>% unlist()
segments(y0 = y_vals,
         y1 = y_vals,
         x0 = q95_lo,
         x1 = q95_hi,
         col = grad_cols,#"grey40",
         lwd = widths[2],
         lend = "butt")


q99_lo <- lapply(effect_size, function(x) quantile(x, 0.005)) %>% unlist()
q99_hi <- lapply(effect_size, function(x) quantile(x, 0.995)) %>% unlist()
segments(y0 = y_vals,
         y1 = y_vals,
         x0 = q99_lo,
         x1 = q99_hi,
         col = grad_cols,#"grey40",
         lwd = widths[3],
         lend = "butt")



points(med_vals, y_vals,
       pch = c(16, 16, 16, 21, 16, 16, 16, 16),
       cex = 1.4,
       col = grad_cols,#"grey30",
       bg = "white")

text(-1.45, -7.25, "B", xpd = T,
     cex = 1.2,
     font = 2)

# C

par(mar = orig_mar)

# We want to put these in terms of dispersal disruption on a relative
# scale rather than dispersal integrity
dispersal_disruption_values <- (0.58 - dataList$pred_disp_values)/0.58

plot(NA,
     xlim = c(0, 1),
     ylim = c(0, 5),
     las = 1,
     xlab = "",#"Dispersal disruption",
     ylab = "",#"Accumulation rate (MgC/ha/yr)",
     frame = F,
     xaxt = "n",
     yaxt = "n",
     mgp = mgp_vals_y,
     tck = tck_val)
axis(1, at = c(0, 0.5, 1),
     labels = c(0, 0.5, 1),
     mgp = mgp_vals_x,
     tck = tck_val,
     cex.axis = axis_cex)
axis(2, at = c(0:5),
     las = 2,
     mgp = mgp_vals_y,
     tck = tck_val,
     cex.axis = axis_cex)

mtext("Seed dispersal disruption", side = 1, line = line_x)

mtext("Accumulation\nrate (MgC/ha/yr)", side = 2, line = line_y/1.57)

mono_col <- rgb(216,179,101, maxColorValue = 255)
nat_col <- rgb(90,180,172, maxColorValue = 255)


polygon(y = (c(0, bs975[grepl("pred_mean30",
                              names(bs975), fixed = T) &
                          grepl(",2]",
                                names(bs975), fixed = T)]) %>%
               c(rev(bs025[grepl("pred_mean30",
                                 names(bs025), fixed = T) &
                             grepl(",2]",
                                   names(bs025), fixed = T)]), 0))/30,
        x = c(1, dispersal_disruption_values) %>%
          c(rev(c(1, dispersal_disruption_values))),
        col = alpha(mono_col, polygon_alpha * 0.6),
        border = NA)


lines(y = c(bsm[grepl("pred_mean30",
                      names(bsm),
                      fixed = T) &
                  grepl(",2]",
                        names(bsm),
                        fixed = T)]) / 30,
      x = c(dispersal_disruption_values),
      col = alpha(mono_col, polygon_alpha * 0.6),
      lwd = 3,
      lty = 1,
      lend = "butt")

polygon(y = (c(0, bs975[grepl("pred_mean30",
                              names(bs975), fixed = T) &
                          grepl(",1]",
                                names(bs975), fixed = T)]) %>%
               c(rev(bs025[grepl("pred_mean30",
                                 names(bs025), fixed = T) &
                             grepl(",1]",
                                   names(bs025), fixed = T)]), 0))/30,
        x = c(1, dispersal_disruption_values) %>%
          c(rev(c(1, dispersal_disruption_values))),
        col = alpha(nat_col, polygon_alpha),
        border = NA)


lines(y = c(bsm[grepl("pred_mean30",
                      names(bsm),
                      fixed = T) &
                  grepl(",1]",
                        names(bsm),
                        fixed = T)]) / 30,
      x = c(dispersal_disruption_values),
      col = nat_col,
      lwd = 3,
      lend = "butt")



text("Monoculture\nplantation",
     x = -0.04,
     y = 1.1,
     col = mono_col,
     font = 3,
     cex = legend_cex*1.05,
     pos = 4)

text("Natural\nregrowth",
     x = 0.1,
     y = 4.25,
     col = nat_col,
     font = 3,
     cex = legend_cex*1.05,
     pos = 4)

text(-0.32, 5, "C", xpd = T,
     cex = 1.2,
     font = 2)


dev.off()









png(file = "./outputs/figures/figure s9.png",
    units = "in",
    width = 7.5,#7.24,
    height = 5,#6,
    res = 440)

site_dat <- j_dat %>% filter(!duplicated(j_dat$site_id)) %>%
  mutate(monoculture = fct_recode(monoculture,
                                  "Mono." = "yes",
                                  "Natural\nregrowth" = "no"))

bw_val <- 10

p1 <- ggplot(site_dat, aes(x = monoculture, y = AMP)) +
  geom_violin(bw = diff(range(site_dat$AMP))/bw_val) +
  theme_classic() +
  labs(x = "", y = "Annual precipitation (mm)")

p2 <- ggplot(site_dat, aes(x = monoculture, y = AMT)) +
  geom_violin(bw = diff(range(site_dat$AMT))/bw_val) +
  theme_classic() +
  labs(x = "", y = "Annual mean temp. (°C)")

p3 <- ggplot(site_dat, aes(x = monoculture, y = NPP)) +
  geom_violin(bw = diff(range(site_dat$NPP))/bw_val) +
  theme_classic() +
  labs(x = "", y = "Potential NPP Index")

p4 <- ggplot(site_dat, aes(x = monoculture, y = lat_dec)) +
  geom_violin(bw = diff(range(site_dat$lat_dec))/bw_val) +
  theme_classic() +
  labs(x = "", y = "Latitude")

p5 <- ggplot(site_dat, aes(x = monoculture, y = fire)) +
  geom_violin(bw = diff(range(site_dat$fire))/bw_val) +
  theme_classic() +
  labs(x = "", y = "Fire")

p6 <- ggplot(site_dat, aes(x = monoculture, y = grazers)) +
  geom_violin(bw = diff(range(site_dat$grazers))/bw_val) +
  theme_classic() +
  labs(x = "", y = "Grazers")

p7 <- ggplot(site_dat, aes(x = monoculture, y = -drought)) +
  geom_violin(bw = diff(range(site_dat$drought))/bw_val) +
  theme_classic() +
  labs(x = "", y = "Drought (-VHI)")

library("gridExtra")
combined_plot <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol = 4)

dev.off()
