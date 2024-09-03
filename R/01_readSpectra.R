# Start new script
rm(list = ls())
cat("\014")


# Load libraries
require(prospectr)
library(proximater)
require(tidyverse)

# ----------------------------------------------------------------------
# Read in data using the proximater package function read_spc
# ----------------------------------------------------------------------

# USER: SET YOUR WORKING DIRECTORY
wd <- "C:/Users/mmainka/GitHub/TropiRes2024"
setwd(wd)

dat <- read_spc("data/world_data_3644_samples.txt")

# filter by country
dat <- dat %>% data.frame() %>% filter(Countryname == "Kenya")

# create a new factor variable with two categories: High OC and Low OC (defined as > 1%)
dat$Fert <- ifelse(dat$ORGC > 1, "High OC", "Low OC")


# ----------------------------------------------------------------------
# VISUALIZATION ----------------------------------------
# ----------------------------------------------------------------------

# R: extract from the column names of the spectra sub-element 
# the vector of wavelengths/wavenumbers
wavs <- colnames(dat$spc)

# R: Since the "wavs" vector is a character string vector
# we will need to transform it to a numeric vector
# NOTE that the names of the columns of the spectra 
# must be writen only with numbers (otherwise they will not be
# correctly converted from characters to numbers)
wavs <- as.numeric(wavs)


# R: plot the spectra...
# USER: before plotting provide names of the axes 
# of the plot
xax <- "Wavelength, nm"
yax <- "Reflectance"

# define two colors for the two OM classes
cols <- c(rgb(red = 1, green = 0, blue = 0, alpha = 0.3), # red
          rgb(red = 0, green = 0, blue = 1, alpha = 0.3)) # blue, alpha for transparency

matplot(x = wavs, y = t(dat$spc),
        xlab = xax,
        ylab = yax,
        ylim = c(0, 1),
        type = "l",
        lty = 1,
       col = cols[as.factor(dat$Fert)],
        main = "Kenyan soil NIR spectra")
legend("topright", legend = levels(as.factor(dat$Fert)), fill = cols)


# # ----------------------------------------------------------------------
# # SPLICE CORRECTION (optional) -----------------------------------------
# # ----------------------------------------------------------------------
# 
# # Note the "shifts" in the plot of the spectra at 1000 and 1830 nm. 
# # This is due to artefacts of the spectrometer. 
# # This can be corrected with the spliceCorrection function 
# # of the prospectr package
# 
# # USER: Indicate the exact wavelengths at which the shits are
# # located
# sshifts <- c(1000, 1830)
# 
# # R: Correct the spectral "shifts" using the spliceCorrection function 
# dat$spc_corrected <- spliceCorrection(X = dat$spc, 
#                                         wav = wavs, 
#                                         splice = sshifts)
# 
# 
# # R: plot the corrected spectra...                          
# matplot(x = wavs, y = t(dat$spc_corrected),
#         xlab = xax,
#         ylab = yax,
#         ylim = c(0, 1),
#         type = "l",
#         lty = 1,
#         col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3),
#         main = "Spectra corrected")

