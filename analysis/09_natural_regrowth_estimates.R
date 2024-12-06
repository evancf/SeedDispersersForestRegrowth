library("tidyverse")
library("terra")

# Set up working directories
top_wd <- getwd()

data_wd <- paste0(top_wd,"/data/")
outputs_wd <- paste0(top_wd,"/outputs/")
raster_wd <- paste0(top_wd, "/data/spatial data/raster/")
raster_gee_wd <- paste0(top_wd, "/data/spatial data/raster/via_gee/")
raster_outputs_wd <- paste0(top_wd, "/data/spatial data/raster_outputs")
raster_intermediary_wd <- paste0(top_wd, "/data/spatial data/raster_intermediary")

# # This code was executed on a compute cluster. Note that users will need to
# # modify the following working directories accordingly.
# data_wd <- "/nobackup1/efricke/VertNBS_data"
# outputs_wd <- "/nobackup1/efricke/VertNBS_data"
# raster_wd <- "/nobackup1/efricke/VertNBS_data"
# raster_gee_wd <- "/nobackup1/efricke/VertNBS_data"
# raster_outputs_wd <- "/nobackup1/efricke/VertNBS_visuals"
# raster_intermediary_wd <- "/nobackup1/efricke/VertNBS_data/intermediary_rasters"



# Pull in posterior distributions from the Bayesian model -----------------
setwd(outputs_wd)
load("biomass_samples.RData")

# Get these into an easier format
library("coda")
coefs <- summary(biomass_samples)[[2]] %>%
  as.data.frame() %>%
  dplyr::select("50%") %>%
  filter(grepl("beta", rownames(.)))

rm(biomass_samples)

# Make sure we have the center and scale right
tap_mean <- scale_df["tap", "center"]
tap_sd <- scale_df["tap", "scale"]
amt_mean <- scale_df["amt", "center"]
amt_sd <- scale_df["amt", "scale"]
npp_mean <- scale_df["npp", "center"]
npp_sd <- scale_df["npp", "scale"]
grazers_mean <- scale_df["grazers", "center"]
grazers_sd <- scale_df["grazers", "scale"]
fire_mean <- scale_df["fire", "center"]
fire_sd <- scale_df["fire", "scale"]
drought_mean <- scale_df["drought", "center"]
drought_sd <- scale_df["drought", "scale"]



# Load relevant raster data ----------------------------------------------------
setwd(raster_outputs_wd)
di_2000_terra <- rast("dispersal_integrity2000.tif")
setwd(raster_wd)
grazers_terra <- rast("grazers.tif")
setwd(raster_gee_wd)
tap_terra <- rast("tap.tif")
amt_terra <- rast("amt.tif")
npp_terra <- rast("npp_max.tif")
fire_terra <- rast("fire.tif")
drought_terra <- rast("drought.tif")





# Dispersal integrity values under natural scenario ----------------------------

# A value of 0.58 equates to dispersal disruption = 0
disp_nat <- 0.58



# Develop predictions for the ~2000 scenario -----------------------------------

# Predict forest growth up to 30 years by multiplying all these
# rasters by their coefficients

# Remember this is how the variables/coefficients are named
cov_to_name
cov_to_var
cov_to_sample

# The current scenario
library("tictoc")
tic()
gc()

aa <- exp((tap_terra - tap_mean)/tap_sd * coefs["beta_a_c2",] +
            (amt_terra/10 - amt_mean)/amt_sd * coefs["beta_a_c3",] +
            (npp_terra - npp_mean)/npp_sd * coefs["beta_a_c4",] +
            coefs["beta_a_0",])
gc()
setwd(raster_intermediary_wd)
writeRaster(aa, filename =  "aa.tif")
rm(aa)
toc()

tic()
gc()
bb_2000_current <- exp(di_2000_terra * coefs["beta_b_c1",] +
                         (tap_terra - tap_mean)/tap_sd * coefs["beta_b_c2",] +
                         (amt_terra/10 - amt_mean)/amt_sd * coefs["beta_b_c3",] +
                         (npp_terra - npp_mean)/npp_sd * coefs["beta_b_c4",] +
                         (grazers_terra - grazers_mean)/grazers_sd * coefs["beta_b_c5",] +
                         (fire_terra - fire_mean)/fire_sd * coefs["beta_b_c6",] +
                         (drought_terra - drought_mean)/drought_sd * coefs["beta_b_c7",] +
                         coefs["beta_b_0",])
gc()
setwd(raster_intermediary_wd)
writeRaster(bb_2000_current, filename =  "bb_2000_current.tif")
rm(bb_2000_current)
toc()




# Now predictions for average yearly accumulation over 30 years ------------------
setwd(raster_intermediary_wd)
aa <- rast("aa.tif")
bb_2000_current <- rast("bb_2000_current.tif")
gc()
c30_2000_current <- (aa * 30 / (bb_2000_current + 30))/30
gc()
writeRaster(c30_2000_current, filename =  "c30_2000_current.tif")
rm(aa)
rm(bb_2000_current)
rm(c30_2000_current)



# Now, get an estimate of the relative difference in accumulation under a scenario
# of high seed dispersal integrity

# The nat scenario
library("tictoc")
tic()
gc()
bb_2000_nat <- exp((tap_terra - tap_mean)/tap_sd * coefs["beta_b_c2",] +
                     (amt_terra/10 - amt_mean)/amt_sd * coefs["beta_b_c3",] +
                     (npp_terra - npp_mean)/npp_sd * coefs["beta_b_c4",] +
                     (grazers_terra - grazers_mean)/grazers_sd * coefs["beta_b_c5",] +
                     (fire_terra - fire_mean)/fire_sd * coefs["beta_b_c6",] +
                     (drought_terra - drought_mean)/drought_sd * coefs["beta_b_c7",] +
                     coefs["beta_b_0",] + disp_nat * coefs["beta_b_c1",])
gc()
setwd(raster_intermediary_wd)
writeRaster(bb_2000_nat, filename =  "bb_2000_nat.tif")
rm(bb_2000_nat)
toc()


# Now predictions for average yearly accumulation over 30 years ------------------
setwd(raster_intermediary_wd)
aa <- rast("aa.tif")
bb_2000_nat <- rast("bb_2000_nat.tif")
gc()
c30_2000_nat <- (aa * 30 / (bb_2000_nat + 30))/30
gc()
writeRaster(c30_2000_nat, filename =  "c30_2000_nat.tif")
rm(aa)
rm(bb_2000_nat)
rm(c30_2000_nat)


# Finally, can get the difference in these
setwd(raster_intermediary_wd)
c30_2000_nat <- rast("c30_2000_nat.tif")
c30_2000_current <- rast("c30_2000_current.tif")
c30_2000_diff <- c30_2000_nat - c30_2000_current
writeRaster(c30_2000_diff, filename =  "c30_2000_diff.tif")
rm(c30_2000_nat, c30_2000_current, c30_2000_diff)








# Next, develop predictions for the ~2020 scenario -----------------------------

rm(di_2000_terra)
setwd(raster_outputs_wd)
di_2020_terra <- rast("dispersal_integrity2020.tif")

# Predict forest growth up to 3 years by multipling all these
# rasters by their coefficients

# Remember this is how the variables/coefficients are named
cov_to_name
cov_to_var
cov_to_sample

# The current scenario
library("tictoc")
tic()
gc()
bb_2020_current <- exp(di_2020_terra * coefs["beta_b_c1",] +
                         (tap_terra - tap_mean)/tap_sd * coefs["beta_b_c2",] +
                         (amt_terra/10 - amt_mean)/amt_sd * coefs["beta_b_c3",] +
                         (npp_terra - npp_mean)/npp_sd * coefs["beta_b_c4",] +
                         (grazers_terra - grazers_mean)/grazers_sd * coefs["beta_b_c5",] +
                         (fire_terra - fire_mean)/fire_sd * coefs["beta_b_c6",] +
                         (drought_terra - drought_mean)/drought_sd * coefs["beta_b_c7",] +
                         coefs["beta_b_0",])
gc()
setwd(raster_intermediary_wd)
writeRaster(bb_2020_current, filename =  "bb_2020_current.tif")
rm(bb_2020_current)
toc()




# Now predictions for average yearly accumulation over 30 years ------------------
setwd(raster_intermediary_wd)
aa <- rast("aa.tif")
bb_2020_current <- rast("bb_2020_current.tif")
gc()
c30_2020_current <- (aa * 30 / (bb_2020_current + 30))/30
gc()
writeRaster(c30_2020_current, filename =  "c30_2020_current.tif")
rm(aa)
rm(bb_2020_current)
rm(c30_2020_current)



# Now, get an estimate of the relative difference in accumulation under a scenario
# of high seed dispersal integrity

# The nat scenario
library("tictoc")
tic()
gc()
bb_2020_nat <- exp((tap_terra - tap_mean)/tap_sd * coefs["beta_b_c2",] +
                     (amt_terra/10 - amt_mean)/amt_sd * coefs["beta_b_c3",] +
                     (npp_terra - npp_mean)/npp_sd * coefs["beta_b_c4",] +
                     (grazers_terra - grazers_mean)/grazers_sd * coefs["beta_b_c5",] +
                     (fire_terra - fire_mean)/fire_sd * coefs["beta_b_c6",] +
                     (drought_terra - drought_mean)/drought_sd * coefs["beta_b_c7",] +
                     coefs["beta_b_0",] + disp_nat * coefs["beta_b_c1",])
gc()
setwd(raster_intermediary_wd)
writeRaster(bb_2020_nat, filename =  "bb_2020_nat.tif")
rm(bb_2020_nat)
toc()


# Now predictions for average yearly accumulation over 30 years ------------------
setwd(raster_intermediary_wd)
aa <- rast("aa.tif")
bb_2020_nat <- rast("bb_2020_nat.tif")
gc()
c30_2020_nat <- (aa * 30 / (bb_2020_nat + 30))/30
gc()
writeRaster(c30_2020_nat, filename =  "c30_2020_nat.tif")
rm(aa)
rm(bb_2020_nat)
rm(c30_2020_nat)

# Finally, can get the difference in these
setwd(raster_intermediary_wd)
c30_2020_nat <- rast("c30_2020_nat.tif")
c30_2020_current <- rast("c30_2020_current.tif")
c30_2020_diff <- c30_2020_nat - c30_2020_current
writeRaster(c30_2020_diff, filename =  "c30_2020_diff.tif")
rm(c30_2020_nat, c30_2020_current, c30_2020_diff)

rm(di_2020_terra, amt_terra, npp_terra, tap_terra, coefs, scale_df)


# Develop maps by masking to relevant regions ----------------------------------


# Want current, diff, and nat for 2000 and 2020 in these areas
# trop_for_and_sav_biomes
# trop_for_biomes
# trop_ncs_restore_sites

setwd(raster_wd)
trop_for_and_sav_biomes <- rast("trop_for_and_sav_biomes.tif")
trop_for_biomes <- rast("trop_for_biomes.tif")
ncs_restore_sites <- rast("ncs_restore_sites.tif")

setwd(raster_intermediary_wd)
c30_2000_nat <- rast("c30_2000_nat.tif")
c30_2000_current <- rast("c30_2000_current.tif")
c30_2000_diff <- rast("c30_2000_diff.tif")


setwd(raster_outputs_wd)

# Tropical forest and savanna biomes
gc()
c30_2000_nat_trop_for_and_sav_biomes <- c30_2000_nat * trop_for_and_sav_biomes
writeRaster(c30_2000_nat_trop_for_and_sav_biomes,
            filename =  "c30_2000_nat_trop_for_and_sav_biomes.tif")
rm(c30_2000_nat_trop_for_and_sav_biomes)
gc()

c30_2000_current_trop_for_and_sav_biomes <- c30_2000_current * trop_for_and_sav_biomes
writeRaster(c30_2000_current_trop_for_and_sav_biomes,
            filename =  "c30_2000_current_trop_for_and_sav_biomes.tif",
            overwrite = T)
rm(c30_2000_current_trop_for_and_sav_biomes)
gc()

c30_2000_diff_trop_for_and_sav_biomes <- c30_2000_diff * trop_for_and_sav_biomes
writeRaster(c30_2000_diff_trop_for_and_sav_biomes,
            filename =  "c30_2000_diff_trop_for_and_sav_biomes.tif",
            overwrite = T)
rm(c30_2000_diff_trop_for_and_sav_biomes)
gc()



# Tropical forest biomes
gc()
c30_2000_nat_trop_for_biomes <- c30_2000_nat * trop_for_biomes
writeRaster(c30_2000_nat_trop_for_biomes,
            filename =  "c30_2000_nat_trop_for_biomes.tif",
            overwrite = T)
rm(c30_2000_nat_trop_for_biomes)
gc()

c30_2000_current_trop_for_biomes <- c30_2000_current * trop_for_biomes
writeRaster(c30_2000_current_trop_for_biomes,
            filename =  "c30_2000_current_trop_for_biomes.tif",
            overwrite = T)
rm(c30_2000_current_trop_for_biomes)
gc()

c30_2000_diff_trop_for_biomes <- c30_2000_diff * trop_for_biomes
writeRaster(c30_2000_diff_trop_for_biomes,
            filename =  "c30_2000_diff_trop_for_biomes.tif",
            overwrite = T)
rm(c30_2000_diff_trop_for_biomes)
gc()



# Tropical restoration sites
gc()
c30_2000_nat_trop_ncs_restore_sites_biomes <- c30_2000_nat * trop_for_and_sav_biomes * ncs_restore_sites
writeRaster(c30_2000_nat_trop_ncs_restore_sites_biomes,
            filename =  "c30_2000_nat_trop_ncs_restore_sites_biomes.tif",
            overwrite = T)
rm(c30_2000_nat_trop_ncs_restore_sites_biomes)
gc()

c30_2000_current_trop_ncs_restore_sites_biomes <- c30_2000_current * trop_for_and_sav_biomes * ncs_restore_sites
writeRaster(c30_2000_current_trop_ncs_restore_sites_biomes,
            filename =  "c30_2000_current_trop_ncs_restore_sites_biomes.tif",
            overwrite = T)
rm(c30_2000_current_trop_ncs_restore_sites_biomes)
gc()

c30_2000_diff_trop_ncs_restore_sites_biomes <- c30_2000_diff * trop_for_and_sav_biomes * ncs_restore_sites
writeRaster(c30_2000_diff_trop_ncs_restore_sites_biomes,
            filename =  "c30_2000_diff_trop_ncs_restore_sites_biomes.tif",
            overwrite = T)
rm(c30_2000_diff_trop_ncs_restore_sites_biomes)
gc()











# Equivalent for 2020

rm(c30_2000_nat, c30_2000_current, c30_2000_diff)

setwd(raster_intermediary_wd)
c30_2020_nat <- rast("c30_2020_nat.tif")
c30_2020_current <- rast("c30_2020_current.tif")
c30_2020_diff <- rast("c30_2020_diff.tif")


setwd(raster_outputs_wd)

# Tropical forest and savanna biomes
gc()
c30_2020_nat_trop_for_and_sav_biomes <- c30_2020_nat * trop_for_and_sav_biomes
writeRaster(c30_2020_nat_trop_for_and_sav_biomes,
            filename =  "c30_2020_nat_trop_for_and_sav_biomes.tif",
            overwrite = T)
rm(c30_2020_nat_trop_for_and_sav_biomes)
gc()

c30_2020_current_trop_for_and_sav_biomes <- c30_2020_current * trop_for_and_sav_biomes
writeRaster(c30_2020_current_trop_for_and_sav_biomes,
            filename =  "c30_2020_current_trop_for_and_sav_biomes.tif",
            overwrite = T)
rm(c30_2020_current_trop_for_and_sav_biomes)
gc()

c30_2020_diff_trop_for_and_sav_biomes <- c30_2020_diff * trop_for_and_sav_biomes
writeRaster(c30_2020_diff_trop_for_and_sav_biomes,
            filename =  "c30_2020_diff_trop_for_and_sav_biomes.tif",
            overwrite = T)
rm(c30_2020_diff_trop_for_and_sav_biomes)
gc()



# Tropical forest biomes
gc()
c30_2020_nat_trop_for_biomes <- c30_2020_nat * trop_for_biomes
writeRaster(c30_2020_nat_trop_for_biomes,
            filename =  "c30_2020_nat_trop_for_biomes.tif",
            overwrite = T)
rm(c30_2020_nat_trop_for_biomes)
gc()

c30_2020_current_trop_for_biomes <- c30_2020_current * trop_for_biomes
writeRaster(c30_2020_current_trop_for_biomes,
            filename =  "c30_2020_current_trop_for_biomes.tif",
            overwrite = T)
rm(c30_2020_current_trop_for_biomes)
gc()

c30_2020_diff_trop_for_biomes <- c30_2020_diff * trop_for_biomes
writeRaster(c30_2020_diff_trop_for_biomes,
            filename =  "c30_2020_diff_trop_for_biomes.tif",
            overwrite = T)
rm(c30_2020_diff_trop_for_biomes)
gc()



# Tropical restoration sites
gc()
c30_2020_nat_trop_ncs_restore_sites_biomes <- c30_2020_nat * trop_for_and_sav_biomes * ncs_restore_sites
writeRaster(c30_2020_nat_trop_ncs_restore_sites_biomes,
            filename =  "c30_2020_nat_trop_ncs_restore_sites_biomes.tif",
            overwrite = T)
rm(c30_2020_nat_trop_ncs_restore_sites_biomes)
gc()

c30_2020_current_trop_ncs_restore_sites_biomes <- c30_2020_current * trop_for_and_sav_biomes * ncs_restore_sites
writeRaster(c30_2020_current_trop_ncs_restore_sites_biomes,
            filename =  "c30_2020_current_trop_ncs_restore_sites_biomes.tif",
            overwrite = T)
rm(c30_2020_current_trop_ncs_restore_sites_biomes)
gc()

c30_2020_diff_trop_ncs_restore_sites_biomes <- c30_2020_diff * trop_for_and_sav_biomes * ncs_restore_sites
writeRaster(c30_2020_diff_trop_ncs_restore_sites_biomes,
            filename =  "c30_2020_diff_trop_ncs_restore_sites_biomes.tif",
            overwrite = T)
rm(c30_2020_diff_trop_ncs_restore_sites_biomes)
gc()







# Equivalent for the model without dispersal integrity in it...
setwd(outputs_wd)
load("biomass_nd_samples.RData")

nd_coefs <- summary(biomass_nd_samples)[[2]] %>%
  as.data.frame() %>%
  dplyr::select("50%") %>%
  filter(grepl("beta", rownames(.)))

setwd(raster_outputs_wd)
di_2000_terra <- rast("dispersal_integrity2000.tif")
setwd(raster_wd)
grazers_terra <- rast("grazers.tif")
setwd(raster_gee_wd)
tap_terra <- rast("tap.tif")
amt_terra <- rast("amt.tif")
npp_terra <- rast("npp_max.tif")
fire_terra <- rast("fire.tif")
drought_terra <- rast("drought.tif")



# The nd scenario
library("tictoc")
tic()
gc()
aa_2020_nd <- exp(
  (tap_terra - tap_mean)/tap_sd * nd_coefs["beta_a_c2",] +
    (amt_terra/10 - amt_mean)/amt_sd * nd_coefs["beta_a_c3",] +
    (npp_terra - npp_mean)/npp_sd * nd_coefs["beta_a_c4",] +
    nd_coefs["beta_a_0",])
gc()
setwd(raster_intermediary_wd)
writeRaster(aa_2020_nd, filename =  "aa_2020_nd.tif")
rm(aa_2020_nd)
toc()

tic()
gc()
bb_2020_nd <- exp(
  (tap_terra - tap_mean)/tap_sd * nd_coefs["beta_b_c2",] +
    (amt_terra/10 - amt_mean)/amt_sd * nd_coefs["beta_b_c3",] +
    (npp_terra - npp_mean)/npp_sd * nd_coefs["beta_b_c4",] +
    (grazers_terra - grazers_mean)/grazers_sd * nd_coefs["beta_b_c5",] +
    (fire_terra - fire_mean)/fire_sd * nd_coefs["beta_b_c6",] +
    (drought_terra - drought_mean)/drought_sd * nd_coefs["beta_b_c7",] +
    nd_coefs["beta_b_0",])
gc()
setwd(raster_intermediary_wd)
writeRaster(bb_2020_nd, filename =  "bb_2020_nd.tif")
rm(bb_2020_nd)
toc()




# Now predictions for average yearly accumulation over 30 years ------------------
setwd(raster_intermediary_wd)
aa_2020_nd <- rast("aa_2020_nd.tif")
bb_2020_nd <- rast("bb_2020_nd.tif")
gc()
c30_2020_nd <- (aa_2020_nd * 30 / (bb_2020_nd + 30))/30
gc()
writeRaster(c30_2020_nd, filename =  "c30_2020_nd.tif")
rm(aa_2020_nd)
rm(bb_2020_nd)
rm(c30_2020_nd)


# Also the difference between current and ND
setwd(raster_intermediary_wd)
c30_2020_current <- rast("c30_2020_current.tif")
c30_2020_nd <- rast("c30_2020_nd.tif")
c30_2020_diff_nd <- c30_2020_current - c30_2020_nd
writeRaster(c30_2020_diff_nd, filename =  "c30_2020_diff_nd.tif")
rm(c30_2020_nd, c30_2020_current, c30_2020_diff_nd)



setwd(raster_intermediary_wd)
c30_2020_nd <- rast("c30_2020_nd.tif")
c30_2020_diff_nd <- rast("c30_2020_diff_nd.tif")

setwd(raster_wd)
trop_for_and_sav_biomes <- rast("trop_for_and_sav_biomes.tif")

c30_2020_nd_trop_for_and_sav_biomes <- c30_2020_nd * trop_for_and_sav_biomes
writeRaster(c30_2020_nd_trop_for_and_sav_biomes,
            filename =  "c30_2020_nd_trop_for_and_sav_biomes.tif")
rm(c30_2020_nd_trop_for_and_sav_biomes)
gc()

c30_2020_diff_nd_trop_for_and_sav_biomes <- c30_2020_diff_nd * trop_for_and_sav_biomes
writeRaster(c30_2020_diff_nd_trop_for_and_sav_biomes,
            filename =  "c30_2020_diff_nd_trop_for_and_sav_biomes.tif")
rm(c30_2020_diff_nd_trop_for_and_sav_biomes)
gc()






# Some visualization ------------------------------------------------------------------------

library("stars")
setwd(raster_outputs_wd)
tif_names <- list.files(pattern = "c30_20")
tif_names <- tif_names[grepl(".tif", tif_names, fixed = T)]

for(i in tif_names){

  terra_raster <- read_stars(i)
  png_name <- gsub(".tif", ".png", i, fixed = T)

  plot_nbreaks <- 90
  plot_breaks <- "fixed"
  plot_col <- viridis::viridis(plot_nbreaks-1, begin = 0, end = 1)

  png(file = png_name,
      width = 4000, height = 2000)

  plot(terra_raster,
       nbreaks = plot_nbreaks,
       #zlim = c(0, 6)#,
       breaks = "equal",
       col = plot_col
  )

  dev.off()

}




# # Alternate version that uses the stars package, which is faster
# # on a compute cluster with slower read/write
#
# library("tidyverse")
# library("stars")
#
# # Set up working directories
# top_wd <- getwd()
#
# data_wd <- paste0(top_wd,"/data/")
# outputs_wd <- paste0(top_wd,"/outputs/")
# raster_wd <- paste0(top_wd, "/data/spatial data/raster/")
# raster_gee_wd <- paste0(top_wd, "/data/spatial data/raster/via_gee/")
# raster_outputs_wd <- paste0(top_wd, "/data/spatial data/raster_outputs")
# raster_intermediary_wd <- paste0(top_wd, "/data/spatial data/raster_intermediary")
#
# # # This code was executed on a compute cluster. Note that users will need to
# # # modify the following working directories accordingly.
# # data_wd <- "/nobackup1/efricke/VertNBS_data"
# # outputs_wd <- "/nobackup1/efricke/VertNBS_data"
# # raster_wd <- "/nobackup1/efricke/VertNBS_data"
# # raster_gee_wd <- "/nobackup1/efricke/VertNBS_data"
# # raster_outputs_wd <- "/nobackup1/efricke/VertNBS_visuals"
# # raster_intermediary_wd <- "/nobackup1/efricke/VertNBS_data/intermediary_rasters"
#
#
#
# # Pull in posterior distributions from the Bayesian model -----------------
# setwd(outputs_wd)
# load("biomass_samples.RData")
#
# # Get these into an easier format
# library("coda")
# coefs <- summary(biomass_samples)[[2]] %>%
#   as.data.frame() %>%
#   dplyr::select("50%") %>%
#   filter(grepl("beta", rownames(.)))
#
# rm(biomass_samples)
#
# # Make sure we have the center and scale right
# tap_mean <- scale_df["tap", "center"]
# tap_sd <- scale_df["tap", "scale"]
# amt_mean <- scale_df["amt", "center"]
# amt_sd <- scale_df["amt", "scale"]
# npp_mean <- scale_df["npp", "center"]
# npp_sd <- scale_df["npp", "scale"]
# grazers_mean <- scale_df["grazers", "center"]
# grazers_sd <- scale_df["grazers", "scale"]
# fire_mean <- scale_df["fire", "center"]
# fire_sd <- scale_df["fire", "scale"]
# drought_mean <- scale_df["drought", "center"]
# drought_sd <- scale_df["drought", "scale"]
#
#
#
# # Load relevant raster data ----------------------------------------------------
# setwd(raster_outputs_wd)
# di_2000_stars <- read_stars("dispersal_integrity2000.tif")
# setwd(raster_wd)
# grazers_stars <- read_stars("grazers.tif")
# setwd(raster_gee_wd)
# tap_stars <- read_stars("tap.tif")
# amt_stars <- read_stars("amt.tif")
# npp_stars <- read_stars("npp_max.tif")
# fire_stars <- read_stars("fire.tif")
# drought_stars <- read_stars("drought.tif")
#
#
#
#
#
# # Dispersal integrity values under natural scenario ----------------------------
#
# # A value of 0.58 equates to dispersal disruption = 0
# disp_nat <- 0.58
#
#
#
# # Develop predictions for the ~2000 scenario -----------------------------------
#
# # Predict forest growth up to 30 years by multiplying all these
# # rasters by their coefficients
#
# # Remember this is how the variables/coefficients are named
# cov_to_name
# cov_to_var
# cov_to_sample
#
# # The current scenario
# library("tictoc")
# tic()
# gc()
#
# aa <- st_as_stars(c(exp((tap_stars - tap_mean)/tap_sd * coefs["beta_a_c2",] +
#                           (amt_stars/10 - amt_mean)/amt_sd * coefs["beta_a_c3",] +
#                           (npp_stars - npp_mean)/npp_sd * coefs["beta_a_c4",] +
#                           coefs["beta_a_0",])))
# gc()
# setwd(raster_intermediary_wd)
# write_stars(aa, dsn =  "aa.tif")
# rm(aa)
# toc()
#
# tic()
# gc()
# bb_2000_current <- st_as_stars(c(exp(di_2000_stars * coefs["beta_b_c1",] +
#                                        (tap_stars - tap_mean)/tap_sd * coefs["beta_b_c2",] +
#                                        (amt_stars/10 - amt_mean)/amt_sd * coefs["beta_b_c3",] +
#                                        (npp_stars - npp_mean)/npp_sd * coefs["beta_b_c4",] +
#                                        (grazers_stars - grazers_mean)/grazers_sd * coefs["beta_b_c5",] +
#                                        (fire_stars - fire_mean)/fire_sd * coefs["beta_b_c6",] +
#                                        (drought_stars - drought_mean)/drought_sd * coefs["beta_b_c7",] +
#                                        coefs["beta_b_0",])))
# gc()
# setwd(raster_intermediary_wd)
# write_stars(bb_2000_current, dsn =  "bb_2000_current.tif")
# rm(bb_2000_current)
# toc()
#
#
#
#
# # Now predictions for average yearly accumulation over 30 years ------------------
# setwd(raster_intermediary_wd)
# aa <- read_stars("aa.tif")
# bb_2000_current <- read_stars("bb_2000_current.tif")
# gc()
# c30_2000_current <- st_as_stars(c((aa * 30 / (bb_2000_current + 30))/30))
# gc()
# write_stars(c30_2000_current, dsn =  "c30_2000_current.tif")
# rm(aa)
# rm(bb_2000_current)
# rm(c30_2000_current)
#
#
#
# # Now, get an estimate of the relative difference in accumulation under a scenario
# # of high seed dispersal integrity
#
# # The nat scenario
# library("tictoc")
# tic()
# gc()
# bb_2000_nat <- st_as_stars(c(exp((tap_stars - tap_mean)/tap_sd * coefs["beta_b_c2",] +
#                                    (amt_stars/10 - amt_mean)/amt_sd * coefs["beta_b_c3",] +
#                                    (npp_stars - npp_mean)/npp_sd * coefs["beta_b_c4",] +
#                                    (grazers_stars - grazers_mean)/grazers_sd * coefs["beta_b_c5",] +
#                                    (fire_stars - fire_mean)/fire_sd * coefs["beta_b_c6",] +
#                                    (drought_stars - drought_mean)/drought_sd * coefs["beta_b_c7",] +
#                                    coefs["beta_b_0",] + disp_nat * coefs["beta_b_c1",])))
# gc()
# setwd(raster_intermediary_wd)
# write_stars(bb_2000_nat, dsn =  "bb_2000_nat.tif")
# rm(bb_2000_nat)
# toc()
#
#
# # Now predictions for average yearly accumulation over 30 years ------------------
# setwd(raster_intermediary_wd)
# aa <- read_stars("aa.tif")
# bb_2000_nat <- read_stars("bb_2000_nat.tif")
# gc()
# c30_2000_nat <- st_as_stars(c((aa * 30 / (bb_2000_nat + 30))/30))
# gc()
# write_stars(c30_2000_nat, dsn =  "c30_2000_nat.tif")
# rm(aa)
# rm(bb_2000_nat)
# rm(c30_2000_nat)
#
#
# # Finally, can get the difference in these
# setwd(raster_intermediary_wd)
# c30_2000_nat <- read_stars("c30_2000_nat.tif")
# c30_2000_current <- read_stars("c30_2000_current.tif")
# c30_2000_diff <- st_as_stars(c(c30_2000_nat - c30_2000_current))
# write_stars(c30_2000_diff, dsn =  "c30_2000_diff.tif")
# rm(c30_2000_nat, c30_2000_current, c30_2000_diff)
#
#
#
#
#
#
#
#
# # Next, develop predictions for the ~2020 scenario -----------------------------
#
# rm(di_2000_stars)
# setwd(raster_outputs_wd)
# di_2020_stars <- read_stars("dispersal_integrity2020.tif")
#
# # Predict forest growth up to 3 years by multipling all these
# # rasters by their coefficients
#
# # Remember this is how the variables/coefficients are named
# cov_to_name
# cov_to_var
# cov_to_sample
#
# # The current scenario
# library("tictoc")
# tic()
# gc()
# bb_2020_current <- st_as_stars(c(exp(di_2020_stars * coefs["beta_b_c1",] +
#                                        (tap_stars - tap_mean)/tap_sd * coefs["beta_b_c2",] +
#                                        (amt_stars/10 - amt_mean)/amt_sd * coefs["beta_b_c3",] +
#                                        (npp_stars - npp_mean)/npp_sd * coefs["beta_b_c4",] +
#                                        (grazers_stars - grazers_mean)/grazers_sd * coefs["beta_b_c5",] +
#                                        (fire_stars - fire_mean)/fire_sd * coefs["beta_b_c6",] +
#                                        (drought_stars - drought_mean)/drought_sd * coefs["beta_b_c7",] +
#                                        coefs["beta_b_0",])))
# gc()
# setwd(raster_intermediary_wd)
# write_stars(bb_2020_current, dsn =  "bb_2020_current.tif")
# rm(bb_2020_current)
# toc()
#
#
#
#
# # Now predictions for average yearly accumulation over 30 years ------------------
# setwd(raster_intermediary_wd)
# aa <- read_stars("aa.tif")
# bb_2020_current <- read_stars("bb_2020_current.tif")
# gc()
# c30_2020_current <- st_as_stars(c((aa * 30 / (bb_2020_current + 30))/30))
# gc()
# write_stars(c30_2020_current, dsn =  "c30_2020_current.tif")
# rm(aa)
# rm(bb_2020_current)
# rm(c30_2020_current)
#
#
#
# # Now, get an estimate of the relative difference in accumulation under a scenario
# # of high seed dispersal integrity
#
# # The nat scenario
# library("tictoc")
# tic()
# gc()
# bb_2020_nat <- st_as_stars(c(exp((tap_stars - tap_mean)/tap_sd * coefs["beta_b_c2",] +
#                                    (amt_stars/10 - amt_mean)/amt_sd * coefs["beta_b_c3",] +
#                                    (npp_stars - npp_mean)/npp_sd * coefs["beta_b_c4",] +
#                                    (grazers_stars - grazers_mean)/grazers_sd * coefs["beta_b_c5",] +
#                                    (fire_stars - fire_mean)/fire_sd * coefs["beta_b_c6",] +
#                                    (drought_stars - drought_mean)/drought_sd * coefs["beta_b_c7",] +
#                                    coefs["beta_b_0",] + disp_nat * coefs["beta_b_c1",])))
# gc()
# setwd(raster_intermediary_wd)
# write_stars(bb_2020_nat, dsn =  "bb_2020_nat.tif")
# rm(bb_2020_nat)
# toc()
#
#
# # Now predictions for average yearly accumulation over 30 years ------------------
# setwd(raster_intermediary_wd)
# aa <- read_stars("aa.tif")
# bb_2020_nat <- read_stars("bb_2020_nat.tif")
# gc()
# c30_2020_nat <- st_as_stars(c((aa * 30 / (bb_2020_nat + 30))/30))
# gc()
# write_stars(c30_2020_nat, dsn =  "c30_2020_nat.tif")
# rm(aa)
# rm(bb_2020_nat)
# rm(c30_2020_nat)
#
# # Finally, can get the difference in these
# setwd(raster_intermediary_wd)
# c30_2020_nat <- read_stars("c30_2020_nat.tif")
# c30_2020_current <- read_stars("c30_2020_current.tif")
# c30_2020_diff <- c30_2020_nat - c30_2020_current
# write_stars(c30_2020_diff, dsn =  "c30_2020_diff.tif")
# rm(c30_2020_nat, c30_2020_current, c30_2020_diff)
#
# rm(di_2020_stars, amt_stars, npp_stars, tap_stars, coefs, scale_df)
#
#
# # Develop maps by masking to relevant regions ----------------------------------
#
#
# # Want current, diff, and nat for 2000 and 2020 in these areas
# # trop_for_and_sav_biomes
# # trop_for_biomes
# # trop_ncs_restore_sites
#
# setwd(raster_wd)
# trop_for_and_sav_biomes <- read_stars("trop_for_and_sav_biomes.tif")
# trop_for_biomes <- read_stars("trop_for_biomes.tif")
# ncs_restore_sites <- read_stars("ncs_restore_sites.tif")
#
# setwd(raster_intermediary_wd)
# c30_2000_nat <- read_stars("c30_2000_nat.tif")
# c30_2000_current <- read_stars("c30_2000_current.tif")
# c30_2000_diff <- read_stars("c30_2000_diff.tif")
#
#
# setwd(raster_outputs_wd)
#
# # Tropical forest and savanna biomes
# gc()
# c30_2000_nat_trop_for_and_sav_biomes <- st_as_stars(c(c30_2000_nat * trop_for_and_sav_biomes))
# write_stars(c30_2000_nat_trop_for_and_sav_biomes,
#             dsn =  "c30_2000_nat_trop_for_and_sav_biomes.tif")
# rm(c30_2000_nat_trop_for_and_sav_biomes)
# gc()
#
# c30_2000_current_trop_for_and_sav_biomes <- st_as_stars(c(c30_2000_current * trop_for_and_sav_biomes))
# write_stars(c30_2000_current_trop_for_and_sav_biomes,
#             dsn =  "c30_2000_current_trop_for_and_sav_biomes.tif")
# rm(c30_2000_current_trop_for_and_sav_biomes)
# gc()
#
# c30_2000_diff_trop_for_and_sav_biomes <- st_as_stars(c(c30_2000_diff * trop_for_and_sav_biomes))
# write_stars(c30_2000_diff_trop_for_and_sav_biomes,
#             dsn =  "c30_2000_diff_trop_for_and_sav_biomes.tif")
# rm(c30_2000_diff_trop_for_and_sav_biomes)
# gc()
#
#
#
# # Tropical forest biomes
# gc()
# c30_2000_nat_trop_for_biomes <- st_as_stars(c(c30_2000_nat * trop_for_biomes))
# write_stars(c30_2000_nat_trop_for_biomes,
#             dsn =  "c30_2000_nat_trop_for_biomes.tif")
# rm(c30_2000_nat_trop_for_biomes)
# gc()
#
# c30_2000_current_trop_for_biomes <- st_as_stars(c(c30_2000_current * trop_for_biomes))
# write_stars(c30_2000_current_trop_for_biomes,
#             dsn =  "c30_2000_current_trop_for_biomes.tif"
# )
# rm(c30_2000_current_trop_for_biomes)
# gc()
#
# c30_2000_diff_trop_for_biomes <- st_as_stars(c(c30_2000_diff * trop_for_biomes))
# write_stars(c30_2000_diff_trop_for_biomes,
#             dsn =  "c30_2000_diff_trop_for_biomes.tif"
# )
# rm(c30_2000_diff_trop_for_biomes)
# gc()
#
#
#
# # Tropical restoration sites
# gc()
# c30_2000_nat_trop_ncs_restore_sites_biomes <- st_as_stars(c(c30_2000_nat * trop_for_and_sav_biomes * ncs_restore_sites))
# write_stars(c30_2000_nat_trop_ncs_restore_sites_biomes,
#             dsn =  "c30_2000_nat_trop_ncs_restore_sites_biomes.tif"
# )
# rm(c30_2000_nat_trop_ncs_restore_sites_biomes)
# gc()
#
# c30_2000_current_trop_ncs_restore_sites_biomes <- st_as_stars(c(c30_2000_current * trop_for_and_sav_biomes * ncs_restore_sites))
# write_stars(c30_2000_current_trop_ncs_restore_sites_biomes,
#             dsn =  "c30_2000_current_trop_ncs_restore_sites_biomes.tif"
# )
# rm(c30_2000_current_trop_ncs_restore_sites_biomes)
# gc()
#
# c30_2000_diff_trop_ncs_restore_sites_biomes <- st_as_stars(c(c30_2000_diff * trop_for_and_sav_biomes * ncs_restore_sites))
# write_stars(c30_2000_diff_trop_ncs_restore_sites_biomes,
#             dsn =  "c30_2000_diff_trop_ncs_restore_sites_biomes.tif"
# )
# rm(c30_2000_diff_trop_ncs_restore_sites_biomes)
# gc()
#
#
#
#
#
#
#
#
#
#
#
# # Equivalent for 2020
#
# rm(c30_2000_nat, c30_2000_current, c30_2000_diff)
#
# setwd(raster_intermediary_wd)
# c30_2020_nat <- read_stars("c30_2020_nat.tif")
# c30_2020_current <- read_stars("c30_2020_current.tif")
# c30_2020_diff <- read_stars("c30_2020_diff.tif")
#
#
# setwd(raster_outputs_wd)
#
# # Tropical forest and savanna biomes
# gc()
# c30_2020_nat_trop_for_and_sav_biomes <- st_as_stars(c(c30_2020_nat * trop_for_and_sav_biomes))
# write_stars(c30_2020_nat_trop_for_and_sav_biomes,
#             dsn =  "c30_2020_nat_trop_for_and_sav_biomes.tif"
# )
# rm(c30_2020_nat_trop_for_and_sav_biomes)
# gc()
#
# c30_2020_current_trop_for_and_sav_biomes <- st_as_stars(c(c30_2020_current * trop_for_and_sav_biomes))
# write_stars(c30_2020_current_trop_for_and_sav_biomes,
#             dsn =  "c30_2020_current_trop_for_and_sav_biomes.tif"
# )
# rm(c30_2020_current_trop_for_and_sav_biomes)
# gc()
#
# c30_2020_diff_trop_for_and_sav_biomes <- st_as_stars(c(c30_2020_diff * trop_for_and_sav_biomes))
# write_stars(c30_2020_diff_trop_for_and_sav_biomes,
#             dsn =  "c30_2020_diff_trop_for_and_sav_biomes.tif"
# )
# rm(c30_2020_diff_trop_for_and_sav_biomes)
# gc()
#
#
#
# # Tropical forest biomes
# gc()
# c30_2020_nat_trop_for_biomes <- st_as_stars(c(c30_2020_nat * trop_for_biomes))
# write_stars(c30_2020_nat_trop_for_biomes,
#             dsn =  "c30_2020_nat_trop_for_biomes.tif"
# )
# rm(c30_2020_nat_trop_for_biomes)
# gc()
#
# c30_2020_current_trop_for_biomes <- st_as_stars(c(c30_2020_current * trop_for_biomes))
# write_stars(c30_2020_current_trop_for_biomes,
#             dsn =  "c30_2020_current_trop_for_biomes.tif"
# )
# rm(c30_2020_current_trop_for_biomes)
# gc()
#
# c30_2020_diff_trop_for_biomes <- st_as_stars(c(c30_2020_diff * trop_for_biomes))
# write_stars(c30_2020_diff_trop_for_biomes,
#             dsn =  "c30_2020_diff_trop_for_biomes.tif"
# )
# rm(c30_2020_diff_trop_for_biomes)
# gc()
#
#
#
# # Tropical restoration sites
# gc()
# c30_2020_nat_trop_ncs_restore_sites_biomes <- st_as_stars(c(c30_2020_nat * trop_for_and_sav_biomes * ncs_restore_sites))
# write_stars(c30_2020_nat_trop_ncs_restore_sites_biomes,
#             dsn =  "c30_2020_nat_trop_ncs_restore_sites_biomes.tif"
# )
# rm(c30_2020_nat_trop_ncs_restore_sites_biomes)
# gc()
#
# c30_2020_current_trop_ncs_restore_sites_biomes <- st_as_stars(c(c30_2020_current * trop_for_and_sav_biomes * ncs_restore_sites))
# write_stars(c30_2020_current_trop_ncs_restore_sites_biomes,
#             dsn =  "c30_2020_current_trop_ncs_restore_sites_biomes.tif"
# )
# rm(c30_2020_current_trop_ncs_restore_sites_biomes)
# gc()
#
# c30_2020_diff_trop_ncs_restore_sites_biomes <- st_as_stars(c(c30_2020_diff * trop_for_and_sav_biomes * ncs_restore_sites))
# write_stars(c30_2020_diff_trop_ncs_restore_sites_biomes,
#             dsn =  "c30_2020_diff_trop_ncs_restore_sites_biomes.tif"
# )
# rm(c30_2020_diff_trop_ncs_restore_sites_biomes)
# gc()
#
#
#
#
#
#
#
# # Equivalent for the model without dispersal integrity in it...
# setwd(outputs_wd)
# load("biomass_nd_samples.RData")
#
# nd_coefs <- summary(biomass_nd_samples)[[2]] %>%
#   as.data.frame() %>%
#   dplyr::select("50%") %>%
#   filter(grepl("beta", rownames(.)))
#
# setwd(raster_outputs_wd)
# di_2000_stars <- read_stars("dispersal_integrity2000.tif")
# setwd(raster_wd)
# grazers_stars <- read_stars("grazers.tif")
# setwd(raster_gee_wd)
# tap_stars <- read_stars("tap.tif")
# amt_stars <- read_stars("amt.tif")
# npp_stars <- read_stars("npp_max.tif")
# fire_stars <- read_stars("fire.tif")
# drought_stars <- read_stars("drought.tif")
#
#
#
# # The nd scenario
# library("tictoc")
# tic()
# gc()
# aa_2020_nd <- st_as_stars(c(exp(
#   (tap_stars - tap_mean)/tap_sd * nd_coefs["beta_a_c2",] +
#     (amt_stars/10 - amt_mean)/amt_sd * nd_coefs["beta_a_c3",] +
#     (npp_stars - npp_mean)/npp_sd * nd_coefs["beta_a_c4",] +
#     nd_coefs["beta_a_0",])))
# gc()
# setwd(raster_intermediary_wd)
# write_stars(aa_2020_nd, dsn =  "aa_2020_nd.tif")
# rm(aa_2020_nd)
# toc()
#
# tic()
# gc()
# bb_2020_nd <- st_as_stars(c(exp(
#   (tap_stars - tap_mean)/tap_sd * nd_coefs["beta_b_c2",] +
#     (amt_stars/10 - amt_mean)/amt_sd * nd_coefs["beta_b_c3",] +
#     (npp_stars - npp_mean)/npp_sd * nd_coefs["beta_b_c4",] +
#     (grazers_stars - grazers_mean)/grazers_sd * nd_coefs["beta_b_c5",] +
#     (fire_stars - fire_mean)/fire_sd * nd_coefs["beta_b_c6",] +
#     (drought_stars - drought_mean)/drought_sd * nd_coefs["beta_b_c7",] +
#     nd_coefs["beta_b_0",])))
# gc()
# setwd(raster_intermediary_wd)
# write_stars(bb_2020_nd, dsn =  "bb_2020_nd.tif")
# rm(bb_2020_nd)
# toc()
#
#
#
#
# # Now predictions for average yearly accumulation over 30 years ------------------
# setwd(raster_intermediary_wd)
# aa_2020_nd <- read_stars("aa_2020_nd.tif")
# bb_2020_nd <- read_stars("bb_2020_nd.tif")
# gc()
# c30_2020_nd <- st_as_stars(c((aa_2020_nd * 30 / (bb_2020_nd + 30))/30))
# gc()
# write_stars(c30_2020_nd, dsn =  "c30_2020_nd.tif")
# rm(aa_2020_nd)
# rm(bb_2020_nd)
# rm(c30_2020_nd)
#
#
# # Also the difference between current and ND
# setwd(raster_intermediary_wd)
# c30_2020_current <- read_stars("c30_2020_current.tif")
# c30_2020_nd <- read_stars("c30_2020_nd.tif")
# c30_2020_diff_nd <- c30_2020_current - c30_2020_nd
# write_stars(c30_2020_diff_nd, dsn =  "c30_2020_diff_nd.tif")
# rm(c30_2020_nd, c30_2020_current, c30_2020_diff_nd)
#
#
#
# setwd(raster_intermediary_wd)
# c30_2020_nd <- read_stars("c30_2020_nd.tif")
# c30_2020_diff_nd <- read_stars("c30_2020_diff_nd.tif")
#
# setwd(raster_wd)
# trop_for_and_sav_biomes <- read_stars("trop_for_and_sav_biomes.tif")
#
# c30_2020_nd_trop_for_and_sav_biomes <- st_as_stars(c(c30_2020_nd * trop_for_and_sav_biomes))
# write_stars(c30_2020_nd_trop_for_and_sav_biomes,
#             dsn =  "c30_2020_nd_trop_for_and_sav_biomes.tif")
# rm(c30_2020_nd_trop_for_and_sav_biomes)
# gc()
#
# c30_2020_diff_nd_trop_for_and_sav_biomes <- st_as_stars(c(c30_2020_diff_nd * trop_for_and_sav_biomes))
# write_stars(c30_2020_diff_nd_trop_for_and_sav_biomes,
#             dsn =  "c30_2020_diff_nd_trop_for_and_sav_biomes.tif")
# rm(c30_2020_diff_nd_trop_for_and_sav_biomes)
# gc()
#
#
#
#
#
#
# # Some visualization ------------------------------------------------------------------------
#
# library("stars")
# setwd(raster_outputs_wd)
# tif_names <- list.files(pattern = "c30_20")
# tif_names <- tif_names[grepl(".tif", tif_names, fixed = T)]
#
# for(i in tif_names){
#
#   stars_raster <- read_stars(i)
#   png_name <- gsub(".tif", ".png", i, fixed = T)
#
#   plot_nbreaks <- 90
#   plot_breaks <- "fixed"
#   plot_col <- viridis::viridis(plot_nbreaks-1, begin = 0, end = 1)
#
#   png(file = png_name,
#       width = 4000, height = 2000)
#
#   plot(stars_raster,
#        nbreaks = plot_nbreaks,
#        #zlim = c(0, 6)#,
#        breaks = "equal",
#        col = plot_col
#   )
#
#   dev.off()
#
# }
