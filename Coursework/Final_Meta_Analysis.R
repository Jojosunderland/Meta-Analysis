# load packages
library('metafor')

# read data

data <- read.csv("~/Documents/WorkingD/Meta-Analysis/Coursework/Meta_data3.csv", header = T)
View(data)

####################### CENTRE & CLEAN DATA  #######################
# Centre moderators onto averages instead of zeros
# As intercept assumes 0 latitude, year and elevation which does not make sense - so need to provide it with average values

data$year_c <- data$year - mean(data$year)
data$lat_c <- data$latitude - mean(data$latitude)
data$elev_c <- data$elevation - mean(data$elevation)
View(data)

# Remove NAs from each start/end column
ss_clean <- data[!is.na(data$spring_start), ] # ss = Spring start
sc_clean <- data[!is.na(data$spring_comp), ] # sc = Spring completion
fs_clean <- data[!is.na(data$fall_start), ] # fs = Fall start
fc_clean <- data[!is.na(data$fall_comp), ] # fc = Fall completion

########################## META ANALYSES ##############################

# Does spring and fall moult start/end days change over time?

## Order data by YEAR (lowest to highest)

ss_ordered_year <- ss_clean[order(ss_clean$year, decreasing = FALSE), ]
sc_ordered_year <- sc_clean[order(sc_clean$year, decreasing = FALSE), ]
fs_ordered_year <- fs_clean[order(fs_clean$year, decreasing = FALSE), ]
fc_ordered_year <- fc_clean[order(fc_clean$year, decreasing = FALSE), ]

# meta analysis accounting for fixed and random mods
# year, species, latitude and elevation fixed. site and study as random
# Centred/ average values used instead 

ss_metayear <- rma.mv(yi = spring_start, V = ss_se^2, mods = ~ year_c + species + lat_c + elev_c, 
                       random = list(~1|site, ~1|study),, data = ss_ordered_year, method = "REML")

sc_metayear <- rma.mv(yi = spring_comp, V = sc_se^2, mods = ~ year_c + species + lat_c + elev_c, 
                       random = list(~1|site, ~1|study), data = sc_ordered_year, method = "REML")

fs_metayear <- rma.mv(yi = fall_start, V = fs_se^2, mods = ~ year_c + species + lat_c + elev_c, 
                       random = list(~1|site, ~1|study), data = fs_ordered_year, method = "REML")

fc_metayear <- rma.mv(yi = fall_comp, V = fc_se^2, mods = ~ year_c + species + lat_c + elev_c, 
                       random = list(~1|site, ~1|study), data = fc_ordered_year, method = "REML")

# random effects models for whole effect size

# Fit a model WITHOUT moderators (random-effects model)

## SPRING ##
# spring start #
random_model_ss <- rma.mv(yi = ss_ordered_year$spring_start, 
                    V = ss_ordered_year$ss_se^2,  
                    random = list(~1|site, ~1|study), 
                    method = "REML", data = ss_ordered_year)
# Extract estimate and standard error from the model
est_ss <- as.numeric(coef(random_model_ss)) # Overall effect estimate
se_ss <- random_model_ss$se    # Standard error

# spring end #
random_model_sc <- rma.mv(yi = sc_ordered_year$spring_comp, 
                          V = sc_ordered_year$sc_se^2,  
                          random = list(~1|site, ~1|study),
                          method = "REML", data = sc_ordered_year)
est_sc <- as.numeric(coef(random_model_sc))     
se_sc <- random_model_sc$se    

## FALL ##
# fall start #
random_model_fs <- rma.mv(yi = fs_ordered_year$fall_start, 
                          V = fs_ordered_year$fs_se^2,  
                          random = list(~1|site, ~1|study),
                          method = "REML", data = fs_ordered_year)
est_fs <- as.numeric(coef(random_model_fs))           
se_fs <- random_model_fs$se   

# fall end #
random_model_fc <- rma.mv(yi = fc_ordered_year$fall_comp, 
                          V = fc_ordered_year$fc_se^2,  
                          random = list(~1|site, ~1|study),
                          method = "REML", data = fc_ordered_year)
est_fc <- as.numeric(coef(random_model_fc))           
se_fc <- random_model_fc$se   


################################# PLOTS #####################################


# Allocate colours to species for plots #

# spring start
species_col_ss <- c("Lepus americanus" = "slateblue3",
                 "Lepus timidus" = "lightblue")
row_colours_ss <- species_col_ss[ss_ordered_year$species]

# spring end 
species_col_sc <- c("Lepus americanus" = "slateblue3",
                    "Lepus timidus" = "lightblue")
row_colours_sc <- species_col_sc[sc_ordered_year$species]

# fall start 
species_col_fs <- c("Lepus americanus" = "slateblue3",
                    "Lepus timidus" = "lightblue")
row_colours_fs <- species_col_fs[fs_ordered_year$species]

# fall end
species_col_fc <- c("Lepus americanus" = "slateblue3",
                    "Lepus timidus" = "lightblue")
row_colours_fc <- species_col_fc[fc_ordered_year$species]

###### forest plots - in year order: ####

# SPRING FOREST PLOT #
quartz()
par(mfrow = c(1,2))
# Spring start
forest(ss_metayear, cex.lab=0.8, cex.axis=0.8,addfit=FALSE,shade="zebra", 
       xlab = "Ordinal Day of Spring Moult Initiation",lwd = 2, pch = 15, cex = 0.8,
       slab = year, border = NA, xlim = c(0,250),
       ylim = c(-2, ss_metayear$k + 3), header = 'Year',
       colout = row_colours_ss)
# Add random effect model 
abline(h = -0, col = "black", lwd = 1.5)
addpoly.default(x = est_ss, sei = se_ss, rows = -1, col = "#5CC8A7", mlab = "Random Effects Model", cex = 0.8)

# Spring end 
forest(sc_metayear, cex.lab=0.8,col = 'red', cex.axis=0.8,addfit=FALSE,shade="zebra", 
       xlab = "Ordinal Day of Spring Moult Completion",lwd = 2, pch = 15, cex = 0.8,
       slab = sc_ordered_year$year, xlim = c(50,300), border = NA,
       ylim = c(-2, ss_metayear$k + 3), header = 'Year',
       colout = row_colours_sc)
# Add random effects model
abline(h = -0, col = "black", lwd = 1.5)
addpoly.default(x = est_sc, sei = se_sc, rows = -1, col = "#5CC8A7", mlab = "Random Effects Model", cex = 0.8)

# Add legend for species colours

usr <- par("usr")  # get plot dimensions: xmin, xmax, ymin, ymax
legend(x = mean(usr[2]-200),         # horizontally centered
       y = usr[4] + 3,             # just above top of plot
       legend = expression(italic("Lepus americanus"), italic("Lepus timidus")),
       fill = species_col_sc,
       horiz = FALSE,               # arrange side by side
       bty = "n",
       cex = 0.9,
       xpd = TRUE)                 # allow drawing outside plot area

# FALL FORESTS PLOTS #

quartz()
par(mfrow = c(1,2))
# Fall start
forest(fs_metayear, cex.lab=0.8, cex.axis=0.8,addfit=FALSE,shade="zebra", 
       xlab = "Ordinal Day of Fall Moult Initiation",lwd = 2, pch = 15, cex = 0.8,
       slab = fs_ordered_year$year, border = NA, xlim = c(200,400),
       ylim = c(-2, fs_metayear$k + 3), header = 'Year',
       colout = row_colours_fs)
# Add random effects model
abline(h = -0, col = "black", lwd = 1.5)
addpoly.default(x = est_fs, sei = se_fs, rows = -1, col = "#5CC8A7", mlab = "Random Effects Model")

# Fall end
forest(fc_metayear, cex.lab=0.8,col = 'red', cex.axis=0.8,addfit=FALSE,shade="zebra", 
       xlab = "Ordinal Day of Fall Moult Completion",lwd = 2, pch = 15, cex = 0.8,
       slab = fc_ordered_year$year, xlim = c(200,500), border = NA,
       ylim = c(-2, fs_metayear$k + 2), header = 'Year',
       colout = row_colours_fc)
# Add random effects model
abline(h = -0, col = "black", lwd = 1.5)
addpoly.default(x = est_fc, sei = se_fc, rows = -1, col = "#5CC8A7", mlab = "Random Effects Model")

############### REGPLOTS: EFFECT SIZE VS YEAR / LATITUDE / ELEVATION #################

# Need the non-centred mods for appropriate axis
ss_metayear <- rma.mv(yi = spring_start, V = ss_se^2, mods = ~ year + species + latitude + elevation, 
                      random = list(~1|site, ~1|study),, data = ss_ordered_year, method = "REML")

sc_metayear <- rma.mv(yi = spring_comp, V = sc_se^2, mods =  ~ year + species + latitude + elevation, 
                      random = list(~1|site, ~1|study), data = sc_ordered_year, method = "REML")

fs_metayear <- rma.mv(yi = fall_start, V = fs_se^2, mods =  ~ year + species + latitude + elevation, 
                      random = list(~1|site, ~1|study), data = fs_ordered_year, method = "REML")

fc_metayear <- rma.mv(yi = fall_comp, V = fc_se^2, mods =  ~ year + species + latitude + elevation, 
                      random = list(~1|site, ~1|study), data = fc_ordered_year, method = "REML")
## Spring and fall start day vs year, lat and elevation
quartz()
par(mfrow=c(2,3))
regplot(ss_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Spring Initiation)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(ss_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Spring Initiation)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(ss_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Spring Initiation)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")

regplot(fs_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Fall Initiation)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fs_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Fall Initiation)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fs_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Spring Initiation)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")

# Spring and fall end day vs year, latitude and eleveation
quartz()
par(mfrow=c(2,3))
regplot(sc_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Spring Completion)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(sc_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Spring Completion)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(sc_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Spring Completion)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")

regplot(fc_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Fall Completion)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fc_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Fall Completion)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fc_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Fall Completion)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")

# Figuring out what is causing the opposing trends in forest vs regplot (spring start vs year)

library(ggplot2)

# Group by Study ID
ss_ordered_year$study <- as.factor(ss_ordered_year$study)

study_names <- c("1" = "Zimova et al. (2014)",
                 "2" = "Zimova et al. (2019)",
                 "3" = "Peltier et al. (2023)",
                 "4" = "Mills et al. (2013)",
                 "5" = "Zimova et al. (2020)",
                 "6" = "Kumar et al. (2020)",
                 "7" = "Stokes et al. (2023)")

levels(ss_ordered_year$study) <- study_names


# assign colours to studies
study_col <- c("Zimova et al. (2014)" = "red", 
               "Zimova et al. (2019)" = "blue", 
               "Peltier et al. (2023)" = "cyan2", 
               "Mills et al. (2013)" = "orchid", 
               "Zimova et al. (2020)" = "honeydew", 
               "Kumar et al. (2020)" = "slateblue", 
               "Stokes et al. (2023)" = "orange")

# plot
plot1 <- ggplot(data = ss_ordered_year, aes(x = year, y = spring_start, fill = study)) +
  geom_point(shape = 21, color = "black", size = 3, alpha = 0.9) +
  labs(x = "Year", y = "Ordinal Day of Spring Moult Initiation", fill = "Study") +
  scale_fill_manual(values = study_col) +
  theme_bw() + scale_x_continuous(breaks = seq(2010, 2022, by = 2))

plot1

# Group by study site

plot2 <- ggplot(data = ss_ordered_year, aes(x = year, y = spring_start, fill = site, shape = study)) +
  geom_point(size = 3, alpha = 0.9) +
  scale_shape_manual(values = study_shapes) +
  labs(x = "Year", y = "Ordinal Day of Spring Moult Initiation", fill = "Study Site", shape = "Study") +
  theme_bw() + scale_x_continuous(breaks = seq(2010, 2022, by = 2))

plot2

# combine both plots together

# Assign shapes (limit to 21–25)
study_shapes <- c(21, 22, 23, 24, 25, 21, 22)[1:length(levels(ss_ordered_year$study))]
site_colours <- c("Canada" = "red", 
                 "Chamberlain" = "blue", 
                 "Colorado" = "green",
                 "Gardiner" = "slateblue", 
                 "Isle Royal" = "orchid", 
                 "Marcum" = "cyan2",
                 "Mellen" = "orange", 
                 "Morrel creek" = "brown", 
                 "New England" = "yellow",
                 "Norway" = "pink", 
                 "Red cliff" = "seagreen", 
                 "Scotland" = "honeydew") 


plot3 <- ggplot(data = ss_ordered_year, 
                aes(x = year, y = spring_start, 
                    fill = site, shape = study)) +
  geom_point(color = "black", size = 3, alpha = 0.9) +
  scale_shape_manual(values = study_shapes) +
  scale_fill_manual(values = site_colours) +
  labs(x = "Year", 
       y = "Ordinal Day of Spring Moult Initiation", 
       fill = "Study Site", 
       shape = "Study ID") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) +
  theme_bw() +
  guides(fill = guide_legend(override.aes = list(shape = 21))) # match legend to site colours

plot3

###################### META-ANALYSIS RESULTS ############################

ss_metayear # Sig intercept, year and elevation
sc_metayear # sig elevation
fs_metayear # sig elevation
fc_metayear # Sig species, latitude and elevation

# ALL SIG ***
random_model_ss 
random_model_sc
random_model_fs
random_model_fc

