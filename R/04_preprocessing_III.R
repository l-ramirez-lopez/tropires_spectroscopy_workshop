#------------------------------- Info ------------------------------------------
# Description: Scatter corrections using:
#              - Multiplicative Scatter Correction (msc)
#              - Standard Normal Variate (snv)
# 
# Inputs:      br_spectra_1.txt
#              br_spectra_2.txt
#
# Authors:     Leo Ramirez-Lopez & Alexandre Wadoux
#              ramirez-lopez.l@buchi.com; alexandre.wadoux@wur.nl 
#
# Date:        Jun 2017
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Set the language of R to English
Sys.setenv(language = "EN")

# Call the required packages 
require(proximater)
require(prospectr)

# USER: specify working directory
wd <- "C:/Users/mmainka/Github/TROPIRES_SummerSchool_Uganda"

# R: Set the working directory
setwd(wd)

# USER: specify the input files (including the subdirectory 
# that is not specified in the working directy)
inputfile1 <- "data/br_spectra_1.txt"

# R: read the data
sdata <- read_spc(inputfile1, 
                  header = TRUE, 
                  sep ="\t")


# R: extract from the column names of the spectra sub-element 
# the vector of wavelengths/wavenumbers
wavs <- colnames(sdata$spc)

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


matplot(x = wavs, y = t(sdata$spc),
        xlab = xax,
        ylab = yax,
        type = "l",
        lty = 1,
        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3),
        main = "Soil spectra")
grid()

#---- 1. Scatter correction ----
# Here we present two types of scatter correction functions:
# Multiplicative Scatter Correction and Standard Normal Variate 

#---- 1.1 Multiplicative Scatter Correction ----
# The msc function from the prospectr package can be used to apply 
# this type of correction

# R: apply the msc function to the spectra
sdata$spc_msc <- msc(X = sdata$spc)

# Now let's say we collected new spectra and we need to preprocess
# them in the same way (e.g. msc) the old spectra was processed. So we need to use
# the information derived from the old data (average spectrum)
# in order to be able to correct the new data 
# This can be automatically done by applying the predict function 
# on the new data and using the information stored in the sdata$spc_msc sub-element

# First specify the input file where the new spectra is 
# (including the subdirectory  that is not specified in 
# the working directory)
inputfile2 <- "data/br_spectra_2.txt"


# USER: read the new data and organize it
# R: read the data
new_data <- read_spc(inputfile2, 
                     header = TRUE,
                     sep ="\t")

# R: apply the msc correction to the new spectra 
# so that the spectra is corrected using the average spectrum 
# of the old data as the reference for the correction

new_data$spc_msc <- msc(new_data$spc, ref_spectrum = attr(sdata$spc_msc, "Reference spectrum"))

# how does the predict function work?
# it uses a "model" object that it is stored in the sdata$spc_msc and 
# applies it to the new_data$spc
# This is a generic function that is extensively used in prediction from 
# quantitative and qualitative models.
# In this case the predict function is a kind of wrap of the real function which 
# can be accessed by typing in your R console pls:::predict.msc
# for other predict function it works like this most of the time
# [name of the package]::predict.[name of the function]

# R: plot the msc spectra of the old and new data
# USER: before plotting provide the new names of the y axis 
# of the plot
yax_msc <- "msc(Reflectance)"


# USER: define the color of the old spectra
colo <- rgb(red = 0, green = 0, blue = 0, alpha = 0.3)

# USER: define the color of the new spectra
colnw <- rgb(red = 1, green = 0, blue = 0, alpha = 0.3)

# R: create the plots
matplot(x = wavs, y = t(sdata$spc_msc),
        xlab = xax,
        ylab = yax_msc,
        type = "l",
        lty = 1,
        col = colo,
        main = "msc(spectra)")
grid()
matlines(x = wavs, y = t(new_data$spc_msc),
         lty = 1,
         col = colnw)
legend("topright", 
       legend = c("Old", "New"), 
       col = c(colo, colnw), 
       lty = 1, cex = 1, box.lty = 3, ncol = 2,
       box.col = rgb(1,1,1,0), bg = rgb(1,1,1,0))


#---- 1.2 Standard Normal Variate ----
# The nwp_snvt function from the proximater package can 
# be used to apply this type of correction

# R: apply the Standard Normal Variate on the spectra
sdata$spc_snv <- nwp_snvt(X = sdata$spc)

# R: apply the Standard Normal Variate on the new spectra 
# in this case we do not need any information form the old data
# since the transformation for each spectrum is independent from
# any other data (it operates row-wise)
new_data$spc_snv <- nwp_snvt(X = new_data$spc)

# R: plot the snv spectra of the old and new data
# USER: before plotting provide the new names of the y axis 
# of the plot
yax_snv <- "snv(Reflectance)"


# R: create the plots
matplot(x = wavs, y = t(sdata$spc_snv),
        xlab = xax,
        ylab = yax_snv,
        type = "l",
        lty = 1,
        col = colo,
        main = "snv(spectra)")
grid()
matlines(x = wavs, y = t(new_data$spc_snv),
         lty = 1,
         col = colnw)
legend("bottomright", 
       legend = c("Old", "New"), 
       col = c(colo, colnw), 
       lty = 1, cex = 1, box.lty = 3, ncol = 2,
       box.col = rgb(1,1,1,0), bg = rgb(1,1,1,0))

#---- 2. Centering and scaling the spectra ----
# The scale function can be used for applying either 
# centering and scaling or both. When both are applied, 
# centering takes place before scaling

# Example 1
# USER: Define what you need:
# e.g. Only center the snv corrected spectral data
centering <- TRUE
scaling <- FALSE

# R: center the data
sdata$spc_snv_cnt <- scale(sdata$spc_snv, center = centering, scale = scaling)

# R: plot the data

# R: plot the snv spectra of the old and new data
# USER: before plotting provide the new names of the y axis 
# of the plot
yax_snv <- "Centred snv(Reflectance)"

# R: create the plots
matplot(x = wavs, y = t(sdata$spc_snv_cnt),
        xlab = xax,
        ylab = yax_snv,
        type = "l",
        lty = 1,
        col = colo,
        main = "Centred snv(spectra)")
grid()

# Example 2
# USER: Define what you need:
# e.g. Perform both centering and scaling the snv corrected spectral data
centering <- TRUE
scaling <- TRUE

# R: center the data
sdata$spc_snv_cs <- scale(sdata$spc_snv, center = centering, scale = scaling)


# R: plot the data

# R: plot the snv spectra of the old and new data
# USER: before plotting provide the new names of the y axis 
# of the plot
yax_snv <- "Centred and scaled snv(Reflectance)"

# R: create the plots
matplot(x = wavs, y = t(sdata$spc_snv_cs),
        xlab = xax,
        ylab = yax_snv,
        type = "l",
        lty = 1,
        col = colo,
        main = "Centred and scaled snv(spectra)")
grid()







