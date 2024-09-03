#------------------------------- Info ------------------------------------------
# Description: Sampling calibration sets
#              How to select an adequate calibration set size?
# 
# Inputs:      br_spectra_4.txt
#
# Authors:     Leo Ramirez-Lopez & Alexandre Wadoux
#              ramirez-lopez.l@buchi.com; alexandre.wadoux@wur.nl 
#
#
# References:  Ramirez-Lopez, L., Schmidt, K., Behrens, T., van Wesemael, B., 
#              Dematt?, J. A., Scholten, T. (2014). Sampling optimal calibration 
#              sets in soil infrared spectroscopy. Geoderma, 226, 140-150.
#
# Date:        Jun 2017
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Set the language of R to English
Sys.setenv(language = "EN")

# Call the required packages 
# prospectr
require(prospectr)
require(resemble)
require(matrixStats)
# clhs
require(clhs)


# There are several sampling algorithms that can be used to select 
# a representative set of samples. Here we describe the following ones:
# - Kennard-Stone Sampling (KSS)
# - K-means Sampling (KMS)
# - conditioned Latin hypercube Sampling (cLHS)

#  The approach demonstrated here to identify an adequate sample set size
#  works by comparing the probability density function (pdf) of the population and the pdf 
#  of the sample set in order to asses the representativeness of the sample set. 
#  See Ramirez-Lopez,  et al. (2014) for more details. 


# USER: specifiy working directory
wd <- "C:/Users/mmainka/Github/TROPIRES_SummerSchool_Uganda"


# R: Set the working directory
setwd(wd)

# USER: specifiy the input files (including the subdirectory 
# that is not specified in the working directory)
inputfile1 <- "data/br_spectra_4.txt"


# R: read the second file containing the soil
# spectra
sdata <- read.table(inputfile1, 
                    header = TRUE, 
                    check.names = FALSE, 
                    sep ="\t")

# USER: indicate here the name of the first wavelength/wavenumber 
# of the spectra  as it appears in the data table
firstw <- 502

# R: extract in one object only the spectra from the "data" table...
spc <- as.matrix(sdata[,which(colnames(sdata) == firstw):ncol(sdata)])

# R: remove from "data" spectra...
sdata <- sdata[,-c(which(colnames(sdata) == firstw):ncol(sdata)), drop = FALSE]

# R: put back the spectra in the "data" object but as a 
# sub-element of "data"
sdata$spc <-spc

# R: remove the spc object since it is already a sub-element of "data"
rm(spc)

# R: extract from the column names of the spectra sub-element 
# the vector of wavelengths/wavenumbers
wavs <- colnames(sdata$spc)

# R: Since the "wavs" vector is a character string vector
# we will need to transform it to a numeric vector
# NOTE that the names of the columns of the spectra 
# must be writen only with numbers (otherwise they will not be
# correctly converted from characters to numbers)
wavs <- as.numeric(wavs)


# R: apply the Standard Normal Variate on the absorbance transformed spectra
# in order to account for scattering effects
sdata$spc_snv <- standardNormalVariate(X = -log(sdata$spc))


# R: plot the spectra...
# USER: before plotting provide names of the axes 
# of the plot
xax <- "Wavelength, nm"
yax <- "snv(Absorbance)"

matplot(x = wavs, y = t(sdata$spc_snv),
        xlab = xax,
        ylab = yax,
        type = "l",
        lty = 1,
        col = rgb(red = 0.1, green = 0.7, blue = 0.1, alpha = 0.1),
        main = "Soil spectra - S?o Paulo")
grid()


# Since the sampling algorithms explained here are are sensitive
# to either high-simensionality or multicollinearity

# The sampling algorithms are usually applied in the principal component (PC)
# space of the spectral variables in order to eliminate the multicollinearity problem 
# and also to reduce the complexity of identifiying an optimal sample configuration that
# covers properly the spectral variability. Therefore we need first to compute the PCs 
# of the spectra


# R: compute the PCs of the spectra
# USER: indicate the maximum amount of cummalative variance explained
# that needs to be retained in the PCs
maxexplvar <- 0.99

pcspectra <- pc_projection(Xr = sdata$spc_snv, 
                          pcSelection = list("cumvar", maxexplvar), 
                          center = TRUE, scaled = FALSE)


pcspectra$scores

summary(pcspectra$scores)

# Scale the PC scores
scaled_pcs <- scale(pcspectra$scores, center = TRUE, scale = TRUE)

colSds(scaled_pcs)
colMeans(scaled_pcs)



# Let's say we want to select 90 samples
n_soil <- 90

# the css function in the the "css_function.R" can be used to help you with the 
# identification of an adequate sample set size. Open the "css_function.R" to get more 
# information on how the approach applied here works.
source("R/css_function.R")

# Get the arguments of the css function
args(css)

# Arguments of the css fucntion
#   S:           A matrix of the socres of the principal components
#   k:           A vector containing the sample set sizes to be evaluated
#   method:      The sampleing algorithm. Options are: 
#                - "kss" (Kennard-Stone Sampling)
#                - "kms" (K-means Sampling). The default
#                - "clhs" (conditioned Latin hypercube Sampling)
#   repetitions: The number of times that the sampling must be carried out for 
#                for each sample size to be evaluated. The results of the
#                final msd is the average of the ones obtained at each iteration.
#                Note that since the "kss" method is deterministic and always return the 
#                same results, there is no need for repetitions.
#   n:           The number of equally spaced points at which the probability densities  
#                are to be estimated (see density function of the package stats).
#   from, to:    A vector of the left and right-most points of the grid at which the 
#                densities are to be estimated. Default is the minimums and maximums 
#                of the variables in S.
#   bw:          A vector containing the the smoothing bandwidth to be use for the 
#                probability densities (see density function of the package stats).
#   ...:         arguments to be passed to the calibration sampling algorithms, i.e.
#                additional aruments to be used for the clhs, kenStone or naes functions
#                which run inside this function


# Let's use the css fucntion to compare the probability density function (pdf) of the 
# population and the pdf of sample sets of different sizes
# USER: define a vector indicating the different sample sizes to be evaluated
# in this example a sequence of samples from 30 samples up to 350 samples in step of 20 samples
sss <- seq(from = 30, to = 300, by = 20)


# ---- 1. Sample set size for Kennard-Stone samples ----

# R: use the css fucntion to evaluate different sample set sizes 
# selected with the Kennard-Stone algorithm 
ks_ss <- css(S = scaled_pcs, 
             k = sss, 
             method = "kss", 
             repetitions = 1)

ks_ss

# R: plot the results
plot(x = ks_ss$css,
     y = ks_ss$msd,
     xlab = "Sample set size",
     ylab = "msd",
     type = "b",
     col = rgb(red = 0, green = 0.4, blue = 0.8, alpha = 0.5),
     main = "Kennard-Stone Sampling")



# ---- 2. Sample set size for k-means samples ----

# R: use the css fucntion to evaluate different sample set sizes 
# selected with the Kennard-Stone algorithm 
km_ss <- css(S = scaled_pcs, 
             k = sss, 
             method = "kms", 
             repetitions = 3)

km_ss

# R: plot the results
plot(x = km_ss$css,
     y = km_ss$msd,
     xlab = "Sample set size",
     ylab = "msd",
     type = "b",
     col = rgb(red = 0, green = 0.4, blue = 0.8, alpha = 0.5),
     main = "K-Means Sampling")




# ---- 3. Sample set size for k-means samples ----

# R: use the css fucntion to evaluate different sample set sizes 
# selected with the Kennard-Stone algorithm 
# pass the iter argument to the clhs function (which is executed intenatlly)
# to set the number of iterations to 1000 (the default of the clhs function is
# 10000 but for time contrains of this course we set it to 1000. 
# We recomend to set this argument to 10000 or more)

lh_ss <- css(S = scaled_pcs, 
             k = sss, 
             method = "clhs", 
             repetitions = 3,
             iter = 1000)

lh_ss

# R: plot the results
plot(x = lh_ss$css,
     y = lh_ss$msd,
     xlab = "Sample set size",
     ylab = "msd",
     type = "b",
     col = rgb(red = 0, green = 0.4, blue = 0.8, alpha = 0.5),
     main = "conditioned Latin hypercube Sampling")


# plot all the three results in one graph
plot(x = ks_ss$css,
     y = ks_ss$msd,
     ylim = c(0, 0.02),
     xlab = "Sample set size",
     ylab = "msd",
     type = "b",
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.9),
     main = "Sampling size results")
points(x = km_ss$css,
       y = km_ss$msd,
       type = "b",
       col = rgb(red = 0, green = 1, blue = 0, alpha = 0.9))
points(x = lh_ss$css,
       y = lh_ss$msd,
       type = "b",
       col = rgb(red = 0, green = 0, blue = 1, alpha = 0.9))
       
       

