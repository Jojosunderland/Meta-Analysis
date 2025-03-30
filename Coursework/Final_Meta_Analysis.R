# load packages
library('metafor')
library('ggplot2')
library('lme4')

# read data

data2 <- read.csv("~/Documents/WorkingD/Meta-Analysis/Coursework/Meta_data3.csv", header = T)
View(data2)

####################### CLEAN DATA AND LINEAR MODELS #######################

ss_clean2 <- data2[!is.na(data2$spring_start), ] # remove NAs from spring start column
sc_clean2 <- data2[!is.na(data2$spring_comp), ] # remove NAs from spring comp column
fs_clean2 <- data2[!is.na(data2$fall_start), ] # remove NAs from spring start column
fc_clean2 <- data2[!is.na(data2$fall_comp), ] # remove NAs from spring start column

########################## META ANALYSES ##############################

# Does spring and fall moult start/end days change over time?

## Order data by YEAR (lowest to highest)

ss_ordered_year <- ss_clean2[order(ss_clean2$year, decreasing = FALSE), ]
sc_ordered_year <- sc_clean2[order(sc_clean2$year, decreasing = FALSE), ]
fs_ordered_year <- fs_clean2[order(fs_clean2$year, decreasing = FALSE), ]
fc_ordered_year <- fc_clean2[order(fc_clean2$year, decreasing = FALSE), ]

# meta analysis accounting for fixed and random mods (species, year and site)
# year, species, latitude and elevation fixed. site and study as random, removed 1950 data

ss_metayear <- rma.mv(yi = spring_start, V = ss_se^2, mods = ~ year + species + latitude + elevation, 
                       random = list(~1|site, ~1|study),, data = ss_ordered_year, method = "REML")
ss_metayear

sc_metayear <- rma.mv(yi = spring_comp, V = sc_se^2, mods = ~ year + species +latitude + elevation, 
                       random = list(~1|site, ~1|study), data = sc_ordered_year, method = "REML")

sc_metayear

fs_metayear <- rma.mv(yi = fall_start, V = fs_se^2, mods = ~ year + species + latitude + elevation, 
                       random = list(~1|site, ~1|study), data = fs_ordered_year, method = "REML")

fs_metayear

fc_metayear <- rma.mv(yi = fall_comp, V = fc_se^2, mods = ~ year + species + latitude + elevation, 
                       random = list(~1|site, ~1|study), data = fc_ordered_year, method = "REML")
fc_metayear

# random effects models for whole effect size

# Fit a model WITHOUT moderators (random-effects model)
random_model_ss <- rma.mv(yi = ss_ordered_year$spring_start, 
                    V = ss_ordered_year$ss_se^2,  
                    random = list(~1|site, ~1|study), # IS THIS CORRECT?
                    method = "REML", data = ss_ordered_year)
# Extract estimate and standard error from the model
est_ss <- as.numeric(coef(random_model_ss))     # Overall effect estimate
se_ss <- random_model_ss$se    # Standard error

random_model_sc <- rma.mv(yi = sc_ordered_year$spring_comp, 
                          V = sc_ordered_year$sc_se^2,  
                          random = list(~1|site, ~1|study),
                          method = "REML", data = sc_ordered_year)
est_sc <- as.numeric(coef(random_model_sc))     # Overall effect estimate
se_sc <- random_model_sc$se    # Standard error


random_model_fs <- rma.mv(yi = fs_ordered_year$fall_start, 
                          V = fs_ordered_year$fs_se^2,  
                          random = list(~1|site, ~1|study),
                          method = "REML", data = fs_ordered_year)
est_fs <- as.numeric(coef(random_model_fs))           # Overall effect estimate
se_fs <- random_model_fs$se    # Standard error

random_model_fc <- rma.mv(yi = fc_ordered_year$fall_comp, 
                          V = fc_ordered_year$fc_se^2,  
                          random = list(~1|site, ~1|study),
                          method = "REML", data = fc_ordered_year)
est_fc <- as.numeric(coef(random_model_fc))           # Overall effect estimate
se_fc <- random_model_fc$se    # Standard error


################################# PLOTS #####################################
# forest plots - in year order:

#allocate colours to species

species_col_ss <- c("Lepus americanus" = "slateblue3",
                 "Lepus timidus" = "lightblue")

row_colours_ss <- species_col_ss[ss_ordered_year$species]

species_col_sc <- c("Lepus americanus" = "slateblue3",
                    "Lepus timidus" = "lightblue")

row_colours_sc <- species_col_sc[sc_ordered_year$species]

species_col_fs <- c("Lepus americanus" = "slateblue3",
                    "Lepus timidus" = "lightblue")

row_colours_fs <- species_col_fs[fs_ordered_year$species]

species_col_fc <- c("Lepus americanus" = "slateblue3",
                    "Lepus timidus" = "lightblue")

row_colours_fc <- species_col_fc[fc_ordered_year$species]

# SPRING 
quartz()
par(mfrow = c(1,2))
forest(ss_metayear, cex.lab=0.8, cex.axis=0.8,addfit=FALSE,shade="zebra", 
       xlab = "Ordinal Day of Spring Moult Start",lwd = 2, pch = 15, cex = 0.8,
       slab = year, border = NA, xlim = c(0,250),
       ylim = c(-2, ss_metayear$k + 3), header = 'Year',
       colout = row_colours_ss)

# Add summary diamond manually at a specified row (e.g., -3)
abline(h = -0, col = "black", lwd = 1.5)
addpoly.default(x = est_ss, sei = se_ss, rows = -1, col = "#5CC8A7", mlab = "Random Effects Model", cex = 0.8)

forest(sc_metayear, cex.lab=0.8,col = 'red', cex.axis=0.8,addfit=FALSE,shade="zebra", 
       xlab = "Ordinal Day of Spring Moult End",lwd = 2, pch = 15, cex = 0.8,
       slab = sc_ordered_year$year, xlim = c(50,300), border = NA,
       ylim = c(-2, ss_metayear$k + 3), header = 'Year',
       colout = row_colours_sc)
# Add summary diamond manually at a specified row (e.g., -3)
abline(h = -0, col = "black", lwd = 1.5)
addpoly.default(x = est_sc, sei = se_sc, rows = -1, col = "#5CC8A7", mlab = "Random Effects Model", cex = 0.8)

# Legend for species colours

usr <- par("usr")  # get plot dimensions: xmin, xmax, ymin, ymax
usr
legend(x = mean(usr[2]-200),         # horizontally centered
       y = usr[4] + 3,             # just above top of plot
       legend = expression(italic("Lepus americanus"), italic("Lepus timidus")),
       fill = species_col,
       horiz = FALSE,               # arrange side by side
       bty = "n",
       cex = 0.9,
       xpd = TRUE)                 # allow drawing outside plot area



quartz()
par(mfrow = c(1,2))
forest(fs_metayear, cex.lab=0.8, cex.axis=0.8,addfit=FALSE,shade="zebra", 
       xlab = "Ordinal Day of Fall Moult Start",lwd = 2, pch = 15, cex = 0.8,
       slab = fs_ordered_year$year, border = NA, xlim = c(200,400),
       ylim = c(-2, fs_metayear$k + 3), header = 'Year',
       colout = row_colours_fs)
# Add summary diamond manually at a specified row (e.g., -3)
abline(h = -0, col = "black", lwd = 1.5)
addpoly.default(x = est_fs, sei = se_fs, rows = -1, col = "#5CC8A7", mlab = "Random Effects Model")


forest(fc_metayear, cex.lab=0.8,col = 'red', cex.axis=0.8,addfit=FALSE,shade="zebra", 
       xlab = "Ordinal Day of Fall Moult End",lwd = 2, pch = 15, cex = 0.8,
       slab = fc_ordered_year$year, xlim = c(200,500), border = NA,
       ylim = c(-2, fs_metayear$k + 2), header = 'Year',
       colout = row_colours_fc)
# Add summary diamond manually at a specified row (e.g., -3)
abline(h = -0, col = "black", lwd = 1.5)
addpoly.default(x = est_fc, sei = se_fc, rows = -1, col = "#5CC8A7", mlab = "Random Effects Model")

############### PLOT: EFFECT SIZE VS YEAR / LATITUDE / ELEVATION #################

#extract p values
pvalss <- summary(ss_metayear)$pval[2]  # [2] corresponds to the moderator term
pvalsc <- summary(sc_metayear)$pval[2]  # [2] corresponds to the moderator term
pvalfs <- summary(fs_metayear)$pval[2]  # [2] corresponds to the moderator term
pvalfc <- summary(fc_metayear)$pval[2]  # [2] corresponds to the moderator term

# Format the p-value nicely
pval_text_ss <- if (pvalss < 0.001) { "p < 0.001"} else {
  paste0("p = ", round(pvalss, 3))}
pval_text_sc <- if (pvalsc < 0.001) { "p < 0.001"} else {
  paste0("p = ", round(pvalsc, 3))}
pval_text_fs <- if (pvalfs < 0.001) { "p < 0.001"} else {
  paste0("p = ", round(pvalfs, 3))}
pval_text_fc <- if (pvalfc < 0.001) { "p < 0.001"} else {
  paste0("p = ", round(pvalfc, 3))}

# effect size vs year 
quartz()
par(mfrow=c(2,2))
regplot(ss_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Spring Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
text(x = min(ss_ordered_year$year), 
     y = max(ss_metayear$yi), 
     labels = pval_text_ss, 
     pos = 4, cex = 0.9, font = 2)

regplot(sc_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Spring End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fs_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Fall Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fc_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Fall End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")


# effect size vs latitude 
quartz()
par(mfrow=c(2,2))
regplot(ss_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Spring Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(sc_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Spring End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fs_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Fall Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fc_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Fall End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
text(x = min(fc_ordered_year$latitude), 
     y = max(fc_metayear$yi), 
     labels = pval_text_ss, 
     pos = 4, cex = 0.9, font = 2)


# effect size vs elevation 
quartz()
par(mfrow=c(2,2))
regplot(ss_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Spring Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
text(x = min(ss_ordered_year$elevation), 
     y = max(ss_metayear$yi), 
     labels = pval_text_ss, 
     pos = 4, cex = 0.9, font = 2)
regplot(sc_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Spring End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
text(x = min(sc_ordered_year$elevation), 
     y = max(sc_metayear$yi), 
     labels = pval_text_ss, 
     pos = 4, cex = 0.9, font = 2)
regplot(fs_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Fall Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fc_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Fall End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
text(x = min(fc_ordered_year$elevation), 
     y = max(fc_metayear$yi), 
     labels = pval_text_ss, 
     pos = 4, cex = 0.9, font = 2)

# effect size vs species
quartz()
par(mfrow=c(2,2))
regplot(ss_metayear, 
        mod = "species", 
        xlab = "Species", 
        ylab = "Effect Size (Ordinal Spring Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue",
        xaxt = "n")
axis(1, at = c(0, 1), labels = expression(italic("L. americanus"), italic("L. timidus")))
regplot(sc_metayear, 
        mod = "species", 
        xlab = "Species", 
        ylab = "Effect Size (Ordinal Spring End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue",
        xaxt = "n")
axis(1, at = c(0, 1), labels = expression(italic("L. americanus"), italic("L. timidus")))
regplot(fs_metayear, 
        mod = "species", 
        xlab = "Species", 
        ylab = "Effect Size (Ordinal Fall Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue",
        xaxt = "n")
axis(1, at = c(0, 1), labels = expression(italic("L. americanus"), italic("L. timidus")))
regplot(fc_metayear, 
        mod = "species", 
        xlab = "Species",  
        ylab = "Effect Size (Ordinal Fall End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue",
        xaxt = "n")
axis(1, at = c(0, 1), labels=expression(italic("L. americanus"), italic("L. timidus")))
text(x = min(fc_ordered_year$species), 
     y = max(fc_metayear$yi), 
     labels = pval_text_ss, 
     pos = 4, cex = 0.9, font = 2)


## Spring and fall start day vs year, lat and elevation
quartz()
par(mfrow=c(2,3))
regplot(ss_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Spring Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(ss_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Spring Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(ss_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Spring Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")

regplot(fs_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Fall Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fs_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Fall Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fs_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Spring Start)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")

# Spring and fall end
quartz()
par(mfrow=c(2,3))
regplot(sc_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Spring End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(sc_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Spring End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(sc_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Spring End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")

regplot(fc_metayear, 
        mod = "year", 
        xlab = "Year", 
        ylab = "Effect Size (Ordinal Fall End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fc_metayear, 
        mod = "latitude", 
        xlab = "Latitude", 
        ylab = "Effect Size (Ordinal Fall End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")
regplot(fc_metayear, 
        mod = "elevation", 
        xlab = "Elevation (m)", 
        ylab = "Effect Size (Ordinal Fall End)", 
        col = "black", 
        ci = TRUE, 
        bg = "lightblue")

###################### META-ANALYSIS RESULTS ############################

ss_metayear # Sig intercept, year and elevation
sc_metayear # sig elevation
fs_metayear
fc_metayear # Sig species, latitude and elevation

# ALL SIG ***
random_model_ss 
random_model_sc
random_model_fs
random_model_fc

summary(ss_metayear)


################# REPEAT BUT WITH CENTRED AVERAGE VARIABLES ####################
# As intercept assumes 0 latitude, year and elevation which does not make sense - so need to provide it with average values?

data2$year_c <- data2$year - mean(data2$year)
data2$lat_c <- data2$latitude - mean(data2$latitude)
data2$elev_c <- data2$elevation - mean(data2$elevation)
View(data2)

ss_clean3 <- data2[!is.na(data2$spring_start), ] # remove NAs from spring start column
sc_clean3 <- data2[!is.na(data2$spring_comp), ] # remove NAs from spring comp column
fs_clean3 <- data2[!is.na(data2$fall_start), ] # remove NAs from spring start column
fc_clean3 <- data2[!is.na(data2$fall_comp), ] # remove NAs from spring start column

View(ss_clean3)
head(ss_clean3$year_c)
########################## META ANALYSES ##############################

# Does spring and fall moult start/end days change over time?

## Order data by YEAR (lowest to highest)

ss_ordered_year3 <- ss_clean3[order(ss_clean3$year, decreasing = FALSE), ]
sc_ordered_year3 <- sc_clean3[order(sc_clean3$year, decreasing = FALSE), ]
fs_ordered_year3 <- fs_clean3[order(fs_clean3$year, decreasing = FALSE), ]
fc_ordered_year3 <- fc_clean3[order(fc_clean3$year, decreasing = FALSE), ]

# meta analysis accounting for fixed and random mods (species, year and site)
# year, species, latitude and elevation fixed. site and study as random, removed 1950 data

ss_metayear3 <- rma.mv(yi = spring_start, V = ss_se^2, mods = ~ year_c + species + lat_c + elev_c, 
                      random = list(~1|site, ~1|study),, data = ss_ordered_year3, method = "REML")

sc_metayear3 <- rma.mv(yi = spring_comp, V = sc_se^2, mods = ~ year_c + species + lat_c + elev_c, 
                      random = list(~1|site, ~1|study), data = sc_ordered_year3, method = "REML")

fs_metayear3 <- rma.mv(yi = fall_start, V = fs_se^2, mods = ~ year_c + species + lat_c + elev_c, 
                      random = list(~1|site, ~1|study), data = fs_ordered_year3, method = "REML")

fc_metayear3 <- rma.mv(yi = fall_comp, V = fc_se^2, mods = ~ year_c + species + lat_c + elev_c, 
                      random = list(~1|site, ~1|study), data = fc_ordered_year3, method = "REML")

ss_metayear3
sc_metayear3
fs_metayear3
fc_metayear3

# random model
random_model_ss3 <- rma.mv(yi = sc_ordered_year3$spring_comp, 
                          V = sc_ordered_year3$sc_se^2,  
                          random = list(~1|site, ~1|study),
                          method = "REML", data = sc_ordered_year3)
est_ss3 <- as.numeric(coef(random_model_ss3))     # Overall effect estimate
se_ss3 <- random_model_ss3$se    # Standard error

# plots

quartz()
par(mfrow = c(1,2))
forest(ss_metayear3, cex.lab=0.8, cex.axis=0.8,addfit=FALSE,shade="zebra", 
       xlab = "Ordinal Day of Spring Moult Start",lwd = 2, pch = 15, cex = 0.8,
       slab = year, border = NA, xlim = c(0,250),
       ylim = c(-2, ss_metayear3$k + 3), header = 'Year')

# Add summary diamond manually at a specified row (e.g., -3)
abline(h = -0, col = "black", lwd = 1.5)
addpoly.default(x = est_ss, sei = se_ss, rows = -1, col = "#5CC8A7", mlab = "Random Effects Model", cex = 0.8)

forest(sc_metayear, cex.lab=0.8,col = 'red', cex.axis=0.8,addfit=FALSE,shade="zebra", 
       xlab = "Ordinal Day of Spring Moult End",lwd = 2, pch = 15, cex = 0.8,
       slab = sc_ordered_year3$year, xlim = c(50,300), border = NA,
       ylim = c(-2, ss_metayear3$k + 3), header = 'Year')
# Add summary diamond manually at a specified row (e.g., -3)
abline(h = -0, col = "black", lwd = 1.5)
addpoly.default(x = est_sc, sei = se_sc, rows = -1, col = "#5CC8A7", mlab = "Random Effects Model", cex = 0.8)
