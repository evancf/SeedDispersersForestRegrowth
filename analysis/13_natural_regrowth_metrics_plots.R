source(list.files("./R", full.names = T))

ipak(c("tidyverse", "stars"))

# Set up working directories
top_wd <- getwd()

data_wd <- paste0(top_wd,"/data/")
outputs_wd <- paste0(top_wd,"/outputs/")
raster_wd <- paste0(top_wd, "/data/spatial data/raster/")
raster_gee_wd <- paste0(top_wd, "/data/spatial data/raster/via_gee/")
raster_outputs_wd <- paste0(top_wd, "/data/spatial data/raster_outputs")
raster_intermediary_wd <- paste0(top_wd, "/data/spatial data/raster_intermediary")

# data_wd <- "/nobackup1/efricke/VertNBS_data"
# outputs_wd <- "/nobackup1/efricke/VertNBS_data"
# raster_wd <- "/nobackup1/efricke/VertNBS_data"
# raster_gee_wd <- "/nobackup1/efricke/VertNBS_data"
# raster_outputs_wd <- "/nobackup1/efricke/VertNBS_visuals"
# raster_intermediary_wd <- "/nobackup1/efricke/VertNBS_data/intermediary_rasters"


# A few objects to help with maps and metrics ----------------------------------
# # # What is the cell size for this projection
# setwd(raster_outputs_wd)
# proj_crs_cell_size <- st_area(read_stars("c30_2020_current_trop_for_and_sav_biomes.tif"), proxy = F)
# range(proj_crs_cell_size$area, na.rm = T)
# # Units: [m^2]
# # [1] 708264.2 708264.2
proj_crs_cell_size <- 708264.2 / 10000

# First get a continents polygon
# Want to match this stars raster
setwd(raster_outputs_wd)
c30_2020_current_trop_for_and_sav_biomes <- read_stars("c30_2020_current_trop_for_and_sav_biomes.tif")
library("rnaturalearth")
sf_use_s2(FALSE)
world <- ne_countries() %>%
  st_as_sf() %>%
  filter(continent != "Antarctica") %>%
  st_union() %>%
  st_transform(crs = st_crs(c30_2020_current_trop_for_and_sav_biomes))
focal_bbox <- st_bbox(c30_2020_current_trop_for_and_sav_biomes)
sf_use_s2(T)

# A version of theme_void that avoids some other formatting issues
theme_blank <- function(){
  theme(line = element_blank(),
        rect = element_blank(),

        axis.line.x = element_blank(),
        axis.line.y = element_blank(),

        axis.title.x = element_blank(),
        axis.title.y = element_blank(),

        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),

        axis.text.x = element_blank(),
        axis.text.y = element_blank())
}

# Set up for hatch marks in savanna biomes
setwd(top_wd)
setwd("./data/spatial data/vector/")
ecoreg <- read_sf("Ecoregions2017/Ecoregions2017.shp")

# Create some columns
ecoreg <- ecoreg %>%
  mutate(forest = ifelse(grepl("Forest", BIOME_NAME), 1, 0)) %>%
  mutate(savanna = ifelse(grepl("Savanna", BIOME_NAME), 1, 0)) %>%
  mutate(tropical = ifelse(grepl("Tropical", BIOME_NAME), 1, 0)) %>%
  mutate(for_and_sav = savanna + forest) %>%
  mutate(trop_for = forest * tropical) %>%
  mutate(trop_for_and_sav = for_and_sav * tropical) %>%
  mutate(for_and_sav = ifelse(for_and_sav == 1, 1, NA)) %>%
  mutate(trop_for = ifelse(trop_for == 1, 1, NA)) %>%
  mutate(trop_for_and_sav = ifelse(trop_for_and_sav == 1, 1, NA)) %>%
  mutate(trop_sav = ifelse(savanna * tropical == 1, 1, NA))

sf_use_s2(FALSE)
trop_sav_sf <- ecoreg %>%
  filter(trop_sav == 1) %>%
  st_union() %>%
  st_simplify(dTolerance = 0.01)
sf_use_s2(TRUE)

trop_sav_poly <- st_cast(trop_sav_sf, "POLYGON")
hatch_list <- list()
library("HatchedPolygons")
for(i in 1:length(trop_sav_poly)){
  # A little work around to avoid errors for very small polygons
  hatch_list[[i]] <- tryCatch(hatched.SpatialPolygons(trop_sav_poly[[i]], density = 0.5), error=function(e){})
}
hatch <- do.call(rbind, hatch_list)
hatch <- hatch %>% sf::st_set_crs(st_crs(trop_sav_sf))
# Transform to match maps
proj_crs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
hatch <- hatch %>%
  st_transform(proj_crs)
rm(ecoreg, trop_sav_poly, trop_sav_sf, hatch_list)



# Maps -------------------------------------------------------------------------

# Current regrowth potential
setwd(raster_outputs_wd)
c30_2020_current_trop_for_and_sav_biomes <- read_stars("c30_2020_current_trop_for_and_sav_biomes.tif", proxy = F) + 0

setwd(top_wd)
# pdf(file = "./outputs/figures/regrowth potential 2020.pdf",
#     width = 4.76 * 7.24 / 4.76,
#     height = 1.8 * 7.24 / 4.76)
png(file = "./outputs/figures/regrowth potential 2020.png",
    width = 4.76 * 7.24 / 4.76,
    height = 1.8 * 7.24 / 4.76,
    units = "in", res = 440)

library(colorspace)
ggplot() +
  geom_stars(data = c30_2020_current_trop_for_and_sav_biomes,
             downsample = 20,
             aes(x = x, y = y, fill = c30_2020_current_trop_for_and_sav_biomes.tif)) +
  scale_fill_gradientn(values = c(0, 0.4, 1),
                         colours = c("lightyellow", "forestgreen", "darkgreen"),
                         na.value = "white") +
  guides(fill = guide_colorbar(title="Current regrowth\npotential (Mg/ha/yr)",
                               barwidth = 12,
                               title.vjust = 1.7,
                               label.vjust = 1.6,
                               barheight = 0.4)) +
  theme(legend.title=element_text(size=10)) +
  theme(legend.position = c(.48,-0.15),
        legend.direction = "horizontal") +
  theme(plot.margin=grid::unit(c(0,0,5,0), "mm")) +
  ylim(st_bbox(world)[2] * .7, st_bbox(world)[4] * .58) +
  xlim(st_bbox(world)[1] * .9, st_bbox(world)[3] * 1) +
  coord_equal() +
  theme_blank() +
  geom_sf(data = world,
          col = "grey",
          linewidth = 0.1,
          fill = NA) +
  geom_sf(data = hatch, color = "white", linewidth = 0.1)
dev.off()

rm(c30_2020_current_trop_for_and_sav_biomes)





# Gains and losses since 2000
setwd(raster_outputs_wd)
list.files()
c30_2000_current_trop_for_and_sav_biomes <- read_stars("c30_2000_current_trop_for_and_sav_biomes.tif", proxy = F)
c30_2020_current_trop_for_and_sav_biomes <- read_stars("c30_2020_current_trop_for_and_sav_biomes.tif", proxy = F)

# Quick diversion to get a sense for the spatial breadth of gains vs losses
diff_ratio <- (c30_2020_current_trop_for_and_sav_biomes[[1]] - c30_2000_current_trop_for_and_sav_biomes[[1]]) / c30_2000_current_trop_for_and_sav_biomes[[1]]
diff_ratio_density <- hist(diff_ratio, breaks = c(-1,-0.05, 0, 0.05, 3))
diff_ratio_density$density[1] / (diff_ratio_density$density[1] + diff_ratio_density$density[4])
rm(diff_ratio)
gc()

c30_diff_current_trop_for_and_sav_biomes <- c30_2020_current_trop_for_and_sav_biomes - c30_2000_current_trop_for_and_sav_biomes
rm(c30_2000_current_trop_for_and_sav_biomes)
rm(c30_2020_current_trop_for_and_sav_biomes)
gc()

# # Could calculate the losses in these areas
# # Load in the ncs sites
# # Consider tropical forest and savanna sites
# setwd(raster_wd)
# ncs_restore_sites <- read_stars("ncs_restore_sites.tif", proxy = F)
#
# # Will mask to tropical forest and savanna
# ncs_restore_sites[[1]] <- ncs_restore_sites[[1]] * read_stars("trop_for_and_sav_biomes.tif", proxy = F)[[1]]
#
# # Want to convert to NA all the pixels that are not potential restoration sites
# ncs_restore_sites[[1]][ncs_restore_sites[[1]] == 0] <- NA
#
# #
# c30_2020_ncs <- read_stars("c30_2020_current_trop_for_and_sav_biomes.tif", proxy = F)
# c30_2020_ncs[[1]] <- c30_2020_ncs[[1]] * ncs_restore_sites[[1]]
# c30_2000_ncs <- c30_2000_current_trop_for_and_sav_biomes
# c30_2000_ncs[[1]] <- c30_2000_ncs[[1]] * ncs_restore_sites[[1]]
# diff_ratio <- (c30_2020_ncs[[1]] - c30_2000_ncs[[1]]) / c30_2000_ncs[[1]]
# # Let's see how many pixels with large magnitude change (>+/- 5%)
# diff_ratio_density <- hist(diff_ratio, breaks = c(-1,-0.05, 0, 0.05, 1.5))
# gc()
# mean(diff_ratio, na.rm = T)
#
# c30_diff_ncs <- c30_diff_current_trop_for_and_sav_biomes
# c30_diff_ncs[[1]] <- c30_diff_ncs[[1]] * ncs_restore_sites[[1]]
# mean(c30_diff_ncs[[1]], na.rm = T)
# rm(c30_diff_ncs)


setwd(top_wd)
# pdf(file = "./outputs/figures/gained and lost regrowth potential.pdf",
#     width = 4.76 * 7.24 / 4.76,
#     height = 1.8 * 7.24 / 4.76)
png(file = "./outputs/figures/gained and lost regrowth potential.png",
    width = 4.76 * 7.24 / 4.76,
    height = 1.8 * 7.24 / 4.76,
    units = "in", res = 440)

library(colorspace)
ggplot() +
  geom_stars(data = c30_diff_current_trop_for_and_sav_biomes,
             downsample = 20,
             aes(x = x, y = y, fill = c30_2020_current_trop_for_and_sav_biomes.tif)) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   na.value = "white",
                                   p1 = 0.6,
                                   p2 = 0.6,
                                   p3 = 0.6,
                                   p4 = 0.6) +
  guides(fill = guide_colorbar(title="Change in regrowth potential\nsince 2000 (Mg/ha/yr)",
                               barwidth = 12,
                               title.vjust = 1.7,
                               label.vjust = 1.6,
                               barheight = 0.4)) +
  theme(#legend.position="bottom",
    legend.title=element_text(size=10)) +
  theme(legend.position = c(.48,-0.15),
        legend.direction = "horizontal") +
  theme(plot.margin=grid::unit(c(0,0,5,0), "mm")) +
  ylim(st_bbox(world)[2] * .7, st_bbox(world)[4] * .58) +
  xlim(st_bbox(world)[1] * .9, st_bbox(world)[3] * 1) +
  coord_equal() +
  theme_blank() +
  geom_sf(data = world,
          col = "grey",
          linewidth = 0.1,
          fill = NA)
dev.off()

rm(c30_diff_current_trop_for_and_sav_biomes)




# lost potential
setwd(raster_outputs_wd)
#c30_2020_diff_trop_for_and_sav_biomes <- read_stars("c30_2020_diff_trop_for_and_sav_biomes.tif", proxy = F) + 0
c30_2020_diff_trop_for_biomes <- read_stars("c30_2020_diff_trop_for_biomes.tif", proxy = F) * -1 # Just expressing this as a loss (rather than abs different)


setwd(top_wd)
pdf(file = "./outputs/figures/lost potential 2020.pdf",
    width = 4.76 * 7.24 / 4.76,
    height = 1.8 * 7.24 / 4.76)
# png(file = "./outputs/figures/lost potential 2020.png",
#     width = 4.76 * 7.24 / 4.76 * 2/3,
#     height = 1.1 * 7.24 / 4.76,
#     units = "in", res = 440)

library(colorspace)
ggplot() +
  # geom_stars(data = c30_2020_diff_trop_for_and_sav_biomes,
  #            downsample = 20,
  #            aes(x = x, y = y, fill = c30_2020_diff_trop_for_and_sav_biomes.tif)) +
  geom_stars(data = c30_2020_diff_trop_for_biomes,
             downsample = 20,
             aes(x = x, y = y, fill = c30_2020_diff_trop_for_biomes.tif)) +
  scale_fill_viridis_c(option = "plasma",
                       direction = -1,
                       na.value = "white",
                       limits = c(-3.89, 0)) +
  guides(fill = guide_colorbar(title="Lost carbon accumulation potential\ndue to seed dispersal disruption (Mg/ha/yr)",
                               barwidth = 6,
                               title.vjust = 1.7,
                               label.vjust = 1.6,
                               barheight = 0.4)) +
  theme(legend.title=element_text(size=10)) +
  theme(legend.position = c(.48,-0.15),
        legend.direction = "horizontal") +
  theme(plot.margin=grid::unit(c(0,0,5,0), "mm")) +
  ylim(st_bbox(world)[2] * .7, st_bbox(world)[4] * .58) +
  xlim(st_bbox(world)[1] * .9, st_bbox(world)[3] * 1) +
  coord_equal() +
  theme_blank() +
  geom_sf(data = world,
          col = "grey",
          linewidth = 0.1,
          fill = NA) +
  geom_sf(data = hatch, color = "white", linewidth = 0.1)
dev.off()

# # Insets?
#
# ggplot() +
#   geom_stars(data = c30_2020_diff_trop_for_and_sav_biomes,
#              downsample = 2,
#              aes(x = x, y = y, fill = c30_2020_diff_trop_for_and_sav_biomes.tif)) +
#   # geom_stars(data = c30_2020_diff_trop_for_biomes,
#   #            downsample = 2,
#   #            aes(x = x, y = y, fill = c30_2020_diff_trop_for_biomes.tif)) +
#   # scale_fill_gradientn(values = c(0, 0.3, 1),
#   #                      colours = c("white", "lightblue", "darkblue"),
#   #                      na.value = "white") +
#   scale_fill_viridis_c(option = "magma",
#                        na.value = "white") +
#   guides(fill = guide_colorbar(title="Regrowth potential\n(Mg/ha/yr)",
#                                barwidth = 12,
#                                title.vjust = 1.7,
#                                label.vjust = 1.6,
#                                barheight = 0.4)) +
#   theme(legend.title=element_text(size=10)) +
#   theme(legend.position = c(.48,-0.15),
#         legend.direction = "horizontal") +
#   theme(plot.margin=grid::unit(c(0,0,5,0), "mm")) +
#   ylim(st_bbox(world)[2] * .275, st_bbox(world)[2] * 0) +
#   xlim(st_bbox(world)[1] * .9, st_bbox(world)[3] * 1) +
#   coord_equal() +
#   theme_blank()

rm(c30_2020_diff_trop_for_and_sav_biomes)


# Examine how much loss occurs within potential restoration sites
setwd(raster_outputs_wd)
c30_2020_diff_trop_ncs_restore_sites_biomes <- read_stars("c30_2020_diff_trop_ncs_restore_sites_biomes.tif", proxy = F) + 0
# The zero values are true zeros (these aren't NCS sites)
c30_2020_diff_trop_ncs_restore_sites_biomes[[1]][c30_2020_diff_trop_ncs_restore_sites_biomes[[1]] == 0] <- NA
gc()

# Ensure that we mask to tropical forest sites only
setwd(raster_wd)
c30_2020_diff_trop_ncs_restore_sites_biomes <- c30_2020_diff_trop_ncs_restore_sites_biomes * read_stars("trop_for_biomes.tif", proxy = F) + 0
gc()
# Will use these values to make a histogram showing the lost carbon accumulation potential.
hist_vals <- (c30_2020_diff_trop_ncs_restore_sites_biomes[[1]] * -1) %>% as.vector()
hist_vals <- tibble(vals = hist_vals[!is.na(hist_vals)])

# png(file = "./outputs/figures/lost potential 2020 restore sites histogram.png",
#     width = 4.76 * 7.24 / 4.76 * 1/3,
#     height = 1.2 * 7.24 / 4.76,
#     units = "in", res = 440)
pdf(file = "./outputs/figures/lost potential 2020 restore sites histogram.pdf",
    width = 1.4,
    height = 1.1)

ggplot(hist_vals, aes(x=vals))+
  geom_histogram(aes(y = ..density.., fill = ..x..), bins = 45) +
  scale_fill_viridis_c(option = "plasma", direction = -1, guide = "none",
                       limits = c(-3.89, 0)) +
  scale_y_continuous(breaks = c(0,0.5,1)) +
  scale_x_continuous(breaks = c(-4,-2,0)) +
  labs(x = "MgC/ha/yr",
       y = "Density") +
  theme_classic() +
  theme(legend.title=element_text(size=10)) +
  labs(title = NULL)
dev.off()

mean(hist_vals$vals) # -1.796

# Put this in terms of percent loss as well
setwd(raster_outputs_wd)
ncs_perc_loss <- c30_2020_diff_trop_ncs_restore_sites_biomes / read_stars("c30_2020_nat_trop_ncs_restore_sites_biomes.tif", proxy = F)
gc()
ncs_perc_loss[[1]] %>% mean(na.rm = T) # 57.3% loss on average
rm(ncs_perc_loss, c30_2020_diff_trop_ncs_restore_sites_biomes, hist_vals)
gc()

# Mapping uncertainty in terms of the magnitude of 95% credible interval ----------
# Uncertainty: current
setwd(raster_outputs_wd)
current_lo <- read_stars("c30_2020_current_trop_for_and_sav_biomes_lo_fullmap.tif",
                         proxy = F) + 0

current_hi <- read_stars("c30_2020_current_trop_for_and_sav_biomes_hi_fullmap.tif",
                         proxy = F) + 0

current_95 <- current_hi
current_95[[1]] <- current_hi[[1]] - current_lo[[1]]

rm(current_lo, current_hi)
gc()

# Want to plot pixels without vales as white not grey
current_95[[1]][current_95[[1]] == 0] <- NA

setwd(top_wd)
# pdf(file = "./outputs/figures/regrowth potential 2020 uncertainty.pdf",
#     width = 4.76 * 7.24 / 4.76,
#     height = 1.8 * 7.24 / 4.76)
png(file = "./outputs/figures/regrowth potential 2020 uncertainty.png",
    width = 4.76 * 7.24 / 4.76,
    height = 1.8 * 7.24 / 4.76,
    units = "in", res = 440)
library(colorspace)
ggplot() +
  geom_stars(data = current_95,
             downsample = 20,
             aes(x = x, y = y, fill = c30_2020_current_trop_for_and_sav_biomes_hi_fullmap.tif)) +
  scale_fill_gradientn(values = c(0, 1),
                       colours = c("grey95", "brown"),
                       na.value = "white",
                       limits = c(0, 4.2)) +
  guides(fill = guide_colorbar(title="Uncertainty in current regrowth\npotential (95% CI, Mg/ha/yr)",
                               barwidth = 12,
                               title.vjust = 1.7,
                               label.vjust = 1.6,
                               barheight = 0.4)) +
  theme(legend.title=element_text(size=10)) +
  theme(legend.position = c(.48,-0.15),
        legend.direction = "horizontal") +
  theme(plot.margin=grid::unit(c(0,0,5,0), "mm")) +
  ylim(st_bbox(world)[2] * .7, st_bbox(world)[4] * .58) +
  xlim(st_bbox(world)[1] * .9, st_bbox(world)[3] * 1) +
  coord_equal() +
  theme_blank() +
  geom_sf(data = world,
          col = "grey",
          linewidth = 0.1,
          fill = NA)
dev.off()

rm(current_95)
gc()


# Uncertainty: diff
setwd(raster_outputs_wd)
diff_lo <- read_stars("c30_2020_diff_trop_for_and_sav_biomes_lo_fullmap.tif",
                      proxy = F) + 0

diff_hi <- read_stars("c30_2020_diff_trop_for_and_sav_biomes_hi_fullmap.tif",
                      proxy = F) + 0

diff_95 <- diff_hi
diff_95[[1]] <- diff_hi[[1]] - diff_lo[[1]]
rm(diff_lo, diff_hi)
gc()

# Want to plot pixels without vales  as white not grey
diff_95[[1]][diff_95[[1]] == 0] <- NA

# Ensure that we mask to tropical forest sites only
setwd(raster_wd)
diff_95 <- diff_95 * (read_stars("trop_for_biomes.tif", proxy = F) + 0)
gc()


setwd(top_wd)
# pdf(file = "./outputs/figures/lost potential 2020 uncertainty.pdf",
#     width = 4.76 * 7.24 / 4.76,
#     height = 1.8 * 7.24 / 4.76)
png(file = "./outputs/figures/lost potential 2020 uncertainty.png",
    width = 4.76 * 7.24 / 4.76,
    height = 1.8 * 7.24 / 4.76,
    units = "in", res = 440)
library(colorspace)
ggplot() +
  geom_stars(data = diff_95,
             downsample = 20,
             aes(x = x, y = y, fill = c30_2020_diff_trop_for_and_sav_biomes_hi_fullmap.tif)) +
  scale_fill_gradientn(values = c(0, 1),
                       colours = c("grey95", "brown"),
                       na.value = "white",
                       limits = c(0, 4.2)) +
  guides(fill = guide_colorbar(title="Uncertainty in lost carbon accumulation potential due\nto seed dispersal disruption (95% CI, Mg/ha/yr)",
                               barwidth = 12,
                               title.vjust = 1.7,
                               label.vjust = 1.6,
                               barheight = 0.4)) +
  theme(legend.title=element_text(size=10)) +
  theme(legend.position = c(.48,-0.15),
        legend.direction = "horizontal") +
  theme(plot.margin=grid::unit(c(0,0,5,0), "mm")) +
  ylim(st_bbox(world)[2] * .7, st_bbox(world)[4] * .58) +
  xlim(st_bbox(world)[1] * .9, st_bbox(world)[3] * 1) +
  coord_equal() +
  theme_blank() +
  geom_sf(data = world,
          col = "grey",
          linewidth = 0.1,
          fill = NA)
dev.off()

rm(diff_95)
gc()

















# Mapping regrowth potential without considering seed dispersal disruption

setwd(raster_wd)
nd_diff <- read_stars("c30_2020_diff_nd_trop_for_and_sav_biomes.tif")
nd <- read_stars("c30_2020_nd_trop_for_and_sav_biomes.tif")


setwd(top_wd)
# pdf(file = "./outputs/figures/nd regrowth potential 2020.pdf",
#     width = 4.76 * 7.24 / 4.76,
#     height = 1.8 * 7.24 / 4.76)
png(file = "./outputs/figures/nd regrowth potential 2020.png",
    width = 4.76 * 7.24 / 4.76,
    height = 1.8 * 7.24 / 4.76,
    units = "in", res = 440)

library(colorspace)
ggplot() +
  geom_stars(data = nd,
             downsample = 20,
             aes(x = x, y = y, fill = c30_2020_nd_trop_for_and_sav_biomes.tif)) +
  scale_fill_gradientn(values = c(0, 0.4, 1),
                       colours = c("lightyellow", "forestgreen", "darkgreen"),
                       na.value = "white") +
  guides(fill = guide_colorbar(title="Regrowth potential from model without\nseed dispersal disruption (Mg/ha/yr)",
                               barwidth = 12,
                               title.vjust = 1.7,
                               label.vjust = 1.6,
                               barheight = 0.4)) +
  theme(legend.title=element_text(size=10)) +
  theme(legend.position = c(.48,-0.15),
        legend.direction = "horizontal") +
  theme(plot.margin=grid::unit(c(0,0,5,0), "mm")) +
  ylim(st_bbox(world)[2] * .7, st_bbox(world)[4] * .58) +
  xlim(st_bbox(world)[1] * .9, st_bbox(world)[3] * 1) +
  coord_equal() +
  theme_blank() +
  geom_sf(data = world,
          col = "grey",
          linewidth = 0.1,
          fill = NA)
dev.off()





png(file = "./outputs/figures/regrowth potential difference from abiotic only.png",
    width = 4.76 * 7.24 / 4.76,
    height = 1.8 * 7.24 / 4.76,
    units = "in", res = 440)

library(colorspace)
ggplot() +
  geom_stars(data = nd_diff,
             downsample = 20,
             aes(x = x, y = y, fill = c30_2020_diff_nd_trop_for_and_sav_biomes.tif)) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   na.value = "white",
                                   p1 = 0.6,
                                   p2 = 0.6,
                                   p3 = 0.6,
                                   p4 = 0.6) +
  guides(fill = guide_colorbar(title="Difference from dispersal disruption-\ndependent model (Mg/ha/yr)",
                               barwidth = 12,
                               title.vjust = 1.7,
                               label.vjust = 1.6,
                               barheight = 0.4)) +
  theme(#legend.position="bottom",
    legend.title=element_text(size=10)) +
  theme(legend.position = c(.48,-0.15),
        legend.direction = "horizontal") +
  theme(plot.margin=grid::unit(c(0,0,5,0), "mm")) +
  ylim(st_bbox(world)[2] * .7, st_bbox(world)[4] * .58) +
  xlim(st_bbox(world)[1] * .9, st_bbox(world)[3] * 1) +
  coord_equal() +
  theme_blank() +
  geom_sf(data = world,
          col = "grey",
          linewidth = 0.1,
          fill = NA)
dev.off()

