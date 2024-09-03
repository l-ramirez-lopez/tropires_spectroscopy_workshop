#------------------------------- Info ------------------------------------------
# Description: Spectral transformation from reflectance to absorbance 
#              Spectral de-noising
#              Spectral resampling
#              Spectral differentiation
# 
# Inputs:      noisy_spectra.txt
#
# Authors:     Leo Ramirez-Lopez & Alexandre Wadoux
#              ramirez-lopez.l@buchi.com; alexandre.wadoux@wur.nl 
#
# Date:        Jun 2017
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Set the language of R to English
Sys.setenv(language = "EN")

# Call the required packages 
# prospectr
require(proximater)

# USER: specify working directory
wd <- "C:/Users/mmainka/GitHub/TROPIRES_SummerSchool_Uganda"

# R: Set the working directory
setwd(wd)

# USER: specify the input files (including the subdirectory 
# that is not specified in the working directory)
inputfile1 <- "data/noisy_spectra.txt"

# --- 1. Read the data ----

sdata <- read_spc(inputfile1, 
                  header = TRUE, 
                  sep = "\t")


colnames(sdata$spc)


# --- 2. Noise removal ----

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
yax <- "Absorbance"


matplot(x = wavs, y = t(sdata$spc),
        xlab = xax,
        ylab = yax,
        type = "l",
        lty = 1,
        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3),
        main = "Noisy spectra")
grid()

# Note the random noise along all the spectra


# We present one option to solve this:
# Reduce the noise by applying the Savitzky-Golay filter 
#   (spc_smooth function) 

#   USER: First define the window size (e.g. 11 bands)...
swindow_sg <- 21
#   USER: then, define the order of the polynomial 
#   to fit the  points within the window (e.g. 2 )
poly_sg <- 2

#   R: Apply the spc_smooth function to the spectra
#   Note that the 5 first and last wavelengths are lost in the process
#   (i.e. the vector of wavelengths is shorter now because of the window size
#   of 11 bands)
sdata$spc_sg <- spc_smooth(X = sdata$spc, a = poly_sg, w = swindow_sg) 

#   R: extract the vector of remaining wavelengths
wavs_sg <- colnames(sdata$spc_sg)
wavs_sg <- as.numeric(wavs_sg)


# R: plot the de-noised spectra resulting from applying the Savitzky-Golay filter...                          
matplot(x = wavs_sg, y = t(sdata$spc_sg),
        xlab = xax,
        ylab = yax,
        type = "l",
        lty = 1,
        col = rgb(red = 0.5, green = 0, blue = 0.5, alpha = 0.3),
        main = "De-noised spectra (Savitzky-Golay filtered)")
grid()

