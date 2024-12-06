# Choose the working directory to put all these scripts. Will just put these in
# a temp location because I'll upload them for execution on our slurm cluster

setwd("~/Downloads/")

for(i in 1:100){

  script_name <- paste0("natural_regrowth_estimates_uncertainty_", i, ".R")

  sink(script_name)
  cat(paste0("

# Create an identifier for the filenames output by the script. This function
# will make a suffix particular to this output script

suffix <- ", i ,"

create_file_suffix <- function(x, id = suffix){
  x <- gsub('.tif', '', x, fixed = T)
  x <- paste0(x, '_samp', id, '.tif')
  x
}

library('tidyverse')
library('terra')

# Set up working directories
# topwd <- getwd()
#
# data_wd <- paste0(topwd,'/data/')
# outputs_wd <- paste0(topwd,'/outputs/')
# raster_wd <- paste0(topwd, '/data/spatial data/raster/')
# raster_gee_wd <- paste0(topwd, '/data/spatial data/raster/via_gee/')
# raster_outputs_wd <- paste0(topwd, '/data/spatial data/raster_outputs')
# raster_intermediary_wd <- paste0(topwd, '/data/spatial data/raster_intermediary')

# Will run this on a compute cluster because it's memory intensive
# Have a different structure of the directories there...
data_wd <- '/nobackup1/efricke/VertNBS_data'
outputs_wd <- '/nobackup1/efricke/VertNBS_data'
raster_wd <- '/nobackup1/efricke/VertNBS_data'
raster_gee_wd <- '/nobackup1/efricke/VertNBS_data'
raster_outputs_wd <- '/nobackup1/efricke/VertNBS_visuals'
raster_intermediary_wd <- '/nobackup1/efricke/VertNBS_data/intermediary_rasters'



# Pull in posterior samples from the Bayesian model -----------------
setwd(outputs_wd)
load('biomass_samples.RData')

# Note that this is a single posterior sample, not the average used for the
# maps in the main text
set.seed(", i ,")
library('coda')
coefs <- biomass_samples[[sample(1:3, 1)]][sample(1:1000, 1),] %>%
  .[(grepl('beta', names(.)))] %>%
  as.data.frame()

# Make sure we have the center and scale right
tap_mean <- scale_df['tap', 'center']
tap_sd <- scale_df['tap', 'scale']
amt_mean <- scale_df['amt', 'center']
amt_sd <- scale_df['amt', 'scale']
npp_mean <- scale_df['npp', 'center']
npp_sd <- scale_df['npp', 'scale']
grazers_mean <- scale_df['grazers', 'center']
grazers_sd <- scale_df['grazers', 'scale']
fire_mean <- scale_df['fire', 'center']
fire_sd <- scale_df['fire', 'scale']
drought_mean <- scale_df['drought', 'center']
drought_sd <- scale_df['drought', 'scale']



# Load relevant raster data ----------------------------------------------------
setwd(raster_outputs_wd)
di_2020_terra <- rast('dispersal_integrity2020.tif')
setwd(raster_wd)
grazers_terra <- rast('grazers.tif')
setwd(raster_gee_wd)
tap_terra <- rast('tap.tif')
amt_terra <- rast('amt.tif')
npp_terra <- rast('npp_max.tif')
fire_terra <- rast('fire.tif')
drought_terra <- rast('drought.tif')



# Dispersal integrity values under natural scenario ----------------------------

# A value of 0.58 equates to dispersal disruption = 0
disp_nat <- 0.58


# Predict forest growth up to 30 years by multipling all these
# rasters by their coefficients

# Remember this is how the variables/coefficients are named
cov_to_name
cov_to_var
cov_to_sample

# The current scenario
library('tictoc')
tic()
gc()
aa <- exp((tap_terra - tap_mean)/tap_sd * coefs['beta_a_c2',] +
                                       (amt_terra/10 - amt_mean)/amt_sd * coefs['beta_a_c3',] +
                                       (npp_terra - npp_mean)/npp_sd * coefs['beta_a_c4',] +
                                       coefs['beta_a_0',])
gc()
setwd(raster_intermediary_wd)
writeRaster(aa, overwrite = T, filename = 'aa.tif' %>% create_file_suffix())
rm(aa)
toc()

tic()
gc()
bb_2020_current <- exp(di_2020_terra * coefs['beta_b_c1',] +
                                       (tap_terra - tap_mean)/tap_sd * coefs['beta_b_c2',] +
                                       (amt_terra/10 - amt_mean)/amt_sd * coefs['beta_b_c3',] +
                                       (npp_terra - npp_mean)/npp_sd * coefs['beta_b_c4',] +
                                       (grazers_terra - grazers_mean)/grazers_sd * coefs['beta_b_c5',] +
                                       (fire_terra - fire_mean)/fire_sd * coefs['beta_b_c6',] +
                                       (drought_terra - drought_mean)/drought_sd * coefs['beta_b_c7',] +
                                       coefs['beta_b_0',])
gc()
setwd(raster_intermediary_wd)
writeRaster(bb_2020_current, overwrite = T, filename = 'bb_2020_current.tif' %>% create_file_suffix())
rm(bb_2020_current)
toc()




# Now predictions for average yearly accumulation over 30 years ------------------
setwd(raster_intermediary_wd)
aa <- rast('aa.tif' %>% create_file_suffix())
bb_2020_current <- rast('bb_2020_current.tif' %>% create_file_suffix())
gc()
c30_2020_current <- (aa * 30 / (bb_2020_current + 30))/30
gc()
writeRaster(c30_2020_current, overwrite = T, filename = 'c30_2020_current.tif' %>% create_file_suffix())
rm(aa)
rm(bb_2020_current)
rm(c30_2020_current)



# Now, get an estimate of the relative difference in accumulation under a scenario
# of high seed dispersal integrity

# The nat scenario
library('tictoc')
tic()
gc()
bb_2020_nat <- exp((tap_terra - tap_mean)/tap_sd * coefs['beta_b_c2',] +
                                   (amt_terra/10 - amt_mean)/amt_sd * coefs['beta_b_c3',] +
                                   (npp_terra - npp_mean)/npp_sd * coefs['beta_b_c4',] +
                                   (grazers_terra - grazers_mean)/grazers_sd * coefs['beta_b_c5',] +
                                   (fire_terra - fire_mean)/fire_sd * coefs['beta_b_c6',] +
                                   (drought_terra - drought_mean)/drought_sd * coefs['beta_b_c7',] +
                                   coefs['beta_b_0',] + disp_nat * coefs['beta_b_c1',])
gc()
setwd(raster_intermediary_wd)
writeRaster(bb_2020_nat, overwrite = T, filename = 'bb_2020_nat.tif' %>% create_file_suffix())
rm(bb_2020_nat)
toc()


# Now predictions for average yearly accumulation over 30 years ------------------
setwd(raster_intermediary_wd)
aa <- rast('aa.tif' %>% create_file_suffix())
bb_2020_nat <- rast('bb_2020_nat.tif' %>% create_file_suffix())
gc()
c30_2020_nat <- (aa * 30 / (bb_2020_nat + 30))/30
gc()
writeRaster(c30_2020_nat, overwrite = T, filename = 'c30_2020_nat.tif' %>% create_file_suffix())
rm(aa)
rm(bb_2020_nat)
rm(c30_2020_nat)

# Finally, can get the difference in these
setwd(raster_intermediary_wd)
c30_2020_nat <- rast('c30_2020_nat.tif' %>% create_file_suffix())
c30_2020_current <- rast('c30_2020_current.tif' %>% create_file_suffix())
c30_2020_diff <- c30_2020_nat - c30_2020_current
writeRaster(c30_2020_diff, overwrite = T, filename = 'c30_2020_diff.tif' %>% create_file_suffix())
rm(c30_2020_nat, c30_2020_current, c30_2020_diff)


rm(di_2020_terra, amt_terra, biomass_samples, npp_terra, tap_terra, coefs, scale_df)





# Develop maps by masking to relevant regions ----------------------------------


# Want current, nat, and diff for 2020 in these areas. For space limitations,
# just keep current and diff
# trop_for_and_sav_biomes

setwd(raster_wd)
trop_for_and_sav_biomes <- rast('trop_for_and_sav_biomes.tif')

setwd(raster_intermediary_wd)
c30_2020_nat <- rast('c30_2020_nat.tif' %>% create_file_suffix())
c30_2020_current <- rast('c30_2020_current.tif' %>% create_file_suffix())
c30_2020_diff <- rast('c30_2020_diff.tif' %>% create_file_suffix())


setwd(raster_outputs_wd)

# Tropical forest and savanna biomes
gc()
c30_2020_nat_trop_for_and_sav_biomes <-
  c30_2020_nat * trop_for_and_sav_biomes
writeRaster(c30_2020_nat_trop_for_and_sav_biomes,
            overwrite = T,
            filename = 'c30_2020_nat_trop_for_and_sav_biomes.tif' %>% create_file_suffix())
rm(c30_2020_nat_trop_for_and_sav_biomes)
gc()

c30_2020_current_trop_for_and_sav_biomes <-
  c30_2020_current * trop_for_and_sav_biomes
writeRaster(c30_2020_current_trop_for_and_sav_biomes,
            overwrite = T,
            filename = 'c30_2020_current_trop_for_and_sav_biomes.tif' %>% create_file_suffix())
rm(c30_2020_current_trop_for_and_sav_biomes)
gc()

c30_2020_diff_trop_for_and_sav_biomes <-
  c30_2020_diff * trop_for_and_sav_biomes
writeRaster(c30_2020_diff_trop_for_and_sav_biomes,
            overwrite = T,
            filename = 'c30_2020_diff_trop_for_and_sav_biomes.tif' %>% create_file_suffix())
rm(c30_2020_diff_trop_for_and_sav_biomes)
gc()

# Finally, delete the extra intermediary files

setwd(raster_intermediary_wd)
unlink(c('aa.tif', 'bb_2020_current.tif',
  'aa.tif', 'bb_2020_nat.tif',
  'c30_2020_current', 'c30_2020_nat', 'c30_2020_diff') %>% create_file_suffix())

setwd(raster_outputs_wd)
unlink('c30_2020_nat_trop_for_and_sav_biomes.tif' %>% create_file_suffix())

"),fill=TRUE)
  sink()

  submit_script_name <- paste0("submit.regrowth.uncertainty", i)
  sink(submit_script_name)
  cat(paste0("#!/bin/bash
#SBATCH -n 16 #request cpus
#SBATCH -N 1 #request nodes
#SBATCH -t 0-02:00 #time
#SBATCH -C centos7 #only centos7 nodes
#SBATCH -p sched_mit_hill #Run on sched_engaging_default partition
#SBATCH --mem=0 #
#SBATCH --exclude=node176 # Exclude problem node
#SBATCH -o output_%j.txt #the job output will be directed to the file output_JOBID.txt
#SBATCH -e error_%j.txt #Errors to this
#SBATCH --mail-type=BEGIN,END #Mail at beginning and end of job
#SBATCH --mail-user=efricke@mit.edu
module load R/4.1.0
module add proj/7.2.1
module add gdal/3.2.1
module add geos/3.8.3
module add udunits2/2.2.26
module add engaging/cmake/3.5.2
module add gsl/2.5
module add gcc/9.3.0
R CMD BATCH /nobackup1/efricke/VertNBS_scripts/", script_name),
      fill=TRUE)
  sink()

}

