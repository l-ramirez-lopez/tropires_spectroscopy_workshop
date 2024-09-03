#------------------------------- Info ------------------------------------------
# Description: Sampling calibration sets
# 
# Inputs:      syntheticCalSampling.txt
#              br_spectra_4.txt
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
require(prospectr)
require(resemble)
# clhs
require(clhs)


# There are several sampling algorithms that can be used to select 
# a representative set of samples. Here we describe the following ones:
# - Kennard-Stone Sampling (KSS)
# - K-means Sampling (KMS)
# - conditioned Latin hypercube Sampling (cLHS)

# The examples given here are based on the following datasets:
# - syntheticCalSampling.txt: This file contains a synthetic
#   two-dimensional grid with one outlier. 
# - br_spectra_1.txt: This file contains spectra of 458 soil samples
#   from a ~500 ha farm in the state of Sao Paulo (Brazil). 



# USER: specifiy working directy
wd <- "C:/Users/mmainka/Github/TROPIRES_SummerSchool_Uganda"


# R: Set the working directory
setwd(wd)

# USER: specifiy the input files (including the subdirectory 
# that is not specified in the working directy)
inputfile1 <- "data/syntheticCalSampling.txt"
inputfile2 <- "data/br_spectra_4.txt"




# ---- 1 Sampling algorithms applied to the synthetic grid ----
# R: read the file containing the synthetic grid
griddata <- read.table(inputfile1, 
                       header = TRUE, 
                       sep ="\t")

# R: get a summary of the data (Compactly Display the Structure of the object)
str(griddata)

# griddata contains a two dimensional synthetic where there is one 
# outlier (the first row in the table)
# R: Plot the grid 
plot(x = griddata$x1, y = griddata$x2,
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5))

# R: Plot the grid without the outlier
plot(griddata$x1[-1], griddata$x2[-1],
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5))

# R: create a copy of the grid without the outlier
griddata_no <- griddata[-1,]


# ---- 1.1 Kennard-Stone Sampling (KSS) ----

# USER: define the number of samples that need to be selected
samplesize <- 36

# R: Use the Kennard-Stone algorithm to select the samples on the grid 
# containing the outlier
kss_grid_outlier <- kenStone(X = griddata[,c("x1", "x2")], 
                             k = samplesize, metric = "euclid",
                             .center = FALSE,.scale = FALSE)

# R: get a summary of the kss_grid object created with the kenStone fucntion 
# (Compactly Display the Structure of the object)
str(kss_grid_outlier)

# model is a vector giving the row indices of the selected samples in the input data
# model is a vector giving the row indices of the remaoining samples in the input data

# R: get just the names of the sub-objects in the kss_grid object 
names(kss_grid_outlier)

# R: Plot the grid together with the samples selected
plot(x = griddata$x1, y = griddata$x2,
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5),
     main = "Kennard-Stone selection, n = 36")
points(x = griddata$x1[kss_grid_outlier$model], 
       y = griddata$x2[kss_grid_outlier$model],
       pch = 16,
       cex = 1.5,
       col = rgb(red = 1, green = 0.2, blue = 0.2, alpha = 1))
       

# R: Use the Kennard-Stone algorithm to select the samples on the grid 
# without the outlier
kss_grid <- kenStone(X = griddata_no[,c("x1", "x2")], 
                     k = samplesize, metric = "euclid",
                     .center = FALSE,.scale = FALSE)

# R: Plot the grid together with the samples selected
plot(x = griddata_no$x1, y = griddata_no$x2,
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5),
     main = "Kennard-Stone selection, n = 36")
points(x = griddata_no$x1[kss_grid$model], 
       y = griddata_no$x2[kss_grid$model],
       pch = 16,
       cex = 1.5,
       col = rgb(red = 1, green = 0.2, blue = 0.2, alpha = 1))

# For kennard-Stone sampling you always need to get rid of outliers
# otherwise some of them will be included in the selection!


# ---- 1.2 K-means Sampling (KMS) ----
# R: Randomly select the samples
# before the selection use a random initialization number (seed). 
# This allows you to reproduce your random selection. For this,
# the set.seed function of R can be used. You just need to provide any 
# number you want. 
# Note that if you want to reproduce the random slection on the same vector,
# you need to use the same random initialization number right after the selection
# in conjunction with the set.seed function
# Since the algorithm starts with a random set of initial cluster centers
# this may lead to different final solutions (i.e. different final culster centers)
# due to the fact that the in some cases the algorithm can converge in local minimas 
# and not at the global minimas
set.seed(241180)

# R: Use the k-means algorithm to select the samples on the grid 
# containing the outlier
kms_grid_outlier <- naes(X = griddata[,c("x1", "x2")], 
                         k = samplesize, 
                         iter.max = 1000,
                         .center = FALSE,.scale = FALSE)

# R: get a summary of the kms_grid object created with the naes fucntion 
# (Compactly Display the Structure of the object)
str(kms_grid_outlier)

# model is a vector giving the row indices of the selected samples in the input data
# model is a vector giving the row indices of the remaoining samples in the input data

# R: get just the names of the sub-objects in the kms_grid object 
names(kms_grid_outlier)

# R: Plot the grid together with the samples selected
plot(x = griddata$x1, y = griddata$x2,
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5),
     main = "K-Means Selection, n = 36")
points(x = griddata$x1[kms_grid_outlier$model], 
       y = griddata$x2[kms_grid_outlier$model],
       pch = 16,
       cex = 1.5,
       col = rgb(red = 1, green = 0.2, blue = 0.2, alpha = 1))

# Set a random initialization number (seed)
set.seed(241180)

# R: Use the k-means algorithm to select the samples on the grid 
# without the outlier
kms_grid <- naes(X = griddata_no[,c("x1", "x2")], 
                 k = samplesize, 
                 iter.max = 1000,
                 .center = FALSE,.scale = FALSE)

# R: Plot the grid together with the samples selected
plot(x = griddata_no$x1, y = griddata_no$x2,
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5),
     main = "Kennard-Stone selection, n = 36")
points(x = griddata_no$x1[kms_grid$model], 
       y = griddata_no$x2[kms_grid$model],
       pch = 16,
       cex = 1.5,
       col = rgb(red = 1, green = 0.2, blue = 0.2, alpha = 1))

# The k-means sampling is less sensitive to outliers than kennard-Stone sampling


# ---- 1.3 conditioned Latin hypercube Sampling (cLHS) ----
# R: Randomly select the samples
# before the selection use a random initialization number (seed). 
# This allows you to reproduce your random selection. For this,
# the set.seed function of R can be used. You just need to provide any 
# number you want. 
# Note that if you want to reproduce the random slection on the same vector,
# you need to use the same random initialization number right after the selection
# in conjunction with the set.seed function
# Since the algorithm is based on a startified radom sampling
# the results are not the same when you execute it (unless you use the same seed)
set.seed(241180)

# R: Use the conditioned Latin hypercube algorithm to select the samples on the grid 
# containing the outlier
clhs_grid_outlier <- clhs(x = griddata[,c("x1", "x2")], 
                          size = samplesize, 
                          iter = 10000)

# R: get a summary of the clhs_grid_outlier object created with the clhs fucntion 
# (Compactly Display the Structure of the object)
# In this case the function juts  returns a vector giving the row indices of the selected 
# samples in the input data
str(clhs_grid_outlier)


# R: Plot the grid together with the samples selected
plot(x = griddata$x1, y = griddata$x2,
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5),
     main = "K-Means Selection, n = 36")
points(x = griddata$x1[clhs_grid_outlier], 
       y = griddata$x2[clhs_grid_outlier],
       pch = 16,
       cex = 1.5,
       col = rgb(red = 1, green = 0.2, blue = 0.2, alpha = 1))

# Set a random initialization number (seed)
set.seed(241180)

# R: Use the Kennard-Stone algorithm to select the samples on the grid 
# without the outlier
clhs_grid <- clhs(x = griddata_no[,c("x1", "x2")], 
                  size = samplesize, 
                  iter = 10000)

# R: Plot the grid together with the samples selected
plot(x = griddata_no$x1, y = griddata_no$x2,
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5),
     main = "Kennard-Stone selection, n = 36")
points(x = griddata_no$x1[clhs_grid], 
       y = griddata_no$x2[clhs_grid],
       pch = 16,
       cex = 1.5,
       col = rgb(red = 1, green = 0.2, blue = 0.2, alpha = 1))

# Among the sampling algorithms explained here, cLHS is the less sensitive to outliers
# However cLHS does not maximize the dissimilarity between samples




# ---- 2. Sampling algorithms applied to the soil spectra dataset ----
# R: read the second file containing the soil
# spectra
sdata <- read.table(inputfile2, 
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

# Let's say we want to select 90 samples
n_soil <- 90

# ---- 2.1 Kennard-Stone Sampling (KSS) ----

# R: Use the Kennard-Stone algorithm to select the samples on the grid 
# containing the outlier
kss_soil <- kenStone(X = pcspectra$scores, 
                     k = n_soil, metric = "mahal",
                     .center = TRUE, .scale = FALSE)

# R: get a summary of the kss_soil object created with the kenStone fucntion 
# (Compactly Display the Structure of the object)
str(kss_soil)

# R: Plot the first two PCs together with the samples selected
plot(pcspectra$scores[,c("pc_1", "pc_2")],
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5),
     main = "Kennard-Stone selection, n = 90")
points(pcspectra$scores[kss_soil$model, c("pc_1", "pc_2")], 
       pch = 16,
       cex = 1.5,
       col = rgb(red = 1, green = 0.2, blue = 0.2, alpha = 1))



# ---- 2.2 K-means Sampling (KMS) ----
# R: Randomly select the samples
# before the selection use a random initialization number (seed). 
# This allows you to reproduce your random selection. For this,
# the set.seed function of R can be used. You just need to provide any 
# number you want. 
# Note that if you want to reproduce the random slection on the same vector,
# you need to use the same random initialization number right after the selection
# in conjunction with the set.seed function
# Since the algorithm starts with a random set of initial cluster centers
# this may lead to different final solutions (i.e. different final culster centers)
# due to the fact that the in some cases the algorithm can converge in local minimas 
# and not at the global minimas
set.seed(241180)

# R: Use the k-means algorithm to select the samples on the grid 
# containing the outlier
kms_soil <- naes(X = pcspectra$scores, 
                 k = n_soil,
                 iter.max = 1000,
                 .center = TRUE, .scale = FALSE)

# R: get a summary of the kms_soil object created with the naes fucntion 
# (Compactly Display the Structure of the object)
str(kms_soil)

# R: Plot the first two PCs together with the samples selected
plot(pcspectra$scores[,c("pc_1", "pc_2")],
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5),
     main = "k-means selection, n = 90")
points(pcspectra$scores[kms_soil$model, c("pc_1", "pc_2")], 
       pch = 16,
       cex = 1.5,
       col = rgb(red = 1, green = 0.2, blue = 0.2, alpha = 1))


# ---- 2.3 conditioned Latin hypercube Sampling (cLHS) ----
# R: Randomly select the samples
# before the selection use a random initialization number (seed). 
# This allows you to reproduce your random selection. For this,
# the set.seed function of R can be used. You just need to provide any 
# number you want. 
# Note that if you want to reproduce the random slection on the same vector,
# you need to use the same random initialization number right after the selection
# in conjunction with the set.seed function
# Since the algorithm is based on a startified radom sampling
# the results are not the same when you execute it (unless you use the same seed)
set.seed(241180)

# R: Use the conditioned Latin hypercube algorithm to select the samples on the grid 
# containing the outlier
# Since the clhs function accpets only "data.frame" objects as input variables, we can
# transform our matrix of scores to "data.frame" using the as.data.frame function
clhs_soil <- clhs(x = as.data.frame(pcspectra$scores), 
                  size = n_soil, 
                  iter = 10000)

# R: get a summary of the clhs_soil object created with the clhs fucntion 
# (Compactly Display the Structure of the object)
# In this case the function juts  returns a vector giving the row indices of the selected samples in the input data
str(clhs_soil)


# R: Plot the grid together with the samples selected
plot(pcspectra$scores[,c("pc_1", "pc_2")],
     pch = 16,
     col = rgb(red = 0, green = 0.2, blue = 0.8, alpha = 0.5),
     main = "cLHS selection, n = 90")
points(pcspectra$scores[clhs_soil,],
       pch = 16,
       cex = 1.5,
       col = rgb(red = 1, green = 0.2, blue = 0.2, alpha = 1))


