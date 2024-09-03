rm(list = ls())

library(prospectr)
library(caret)
library(tidyverse)


laura_data <- readRDS("data/calibration_data/NIRabs_PhDLaura_Uganda.rds")
saaka <- grep(x = laura_data$sample_id, pattern = "saak")
laura_data <- laura_data[-saaka,]

tropsoc_data <- readRDS("data/calibration_data/NIRabs_TropSOC_Uganda.rds")

colnames(laura_data) == colnames(tropsoc_data)

alldata <- rbind(laura_data, tropsoc_data)
saveRDS(alldata, "data/calibration_data/NIRabs_allUganda_noSaaka.rds")


## smoothen data
wavs <- as.numeric(colnames(alldata$abs))
range(wavs)
wavs_pre <- as.numeric(colnames(alldata$abs_pre))

alldata$abs_pre <- alldata$abs |>
  movav(w = 19)
  # detrend(wav = wavs_pre, p = 2)
  # standardNormalVariate() 
  # savitzkyGolay(m = 2, p = 2, w = 17) 

# raw absorbance
matplot(x = as.numeric(colnames(alldata$abs)), y = alldata$abs[1,],
        xlab = expression(paste("Wavenumber ", cm^{-1})),
        ylab = 'Absorbance',
        type = 'l',
        lty = 1, 
        # ylim = c(0.2, 0.8),
        xlim = c(7500, 3900))

# pre-processed absorbance data
matplot(x = as.numeric(colnames(alldata$abs_pre)), y = alldata$abs_pre[1,],
        xlab = expression(paste("Wavenumber ", cm^{-1})),
        ylab = 'Absorbance',
        type = 'l',
        lty = 1, 
        # ylim = c(0.2, 0.8),
        xlim = c(7500, 3900))




# maxiumum number of components
pls_ncomp_max <- 10

## define trainControl
train_control <-trainControl(
  method = "repeatedcv",
  savePredictions = TRUE, selectionFunction = "oneSE"
)

alldata$rowIndex <- c(1:nrow(alldata))


## train a pls regression model
pls_model <- caret::train(x = alldata$abs_pre,
                          y = alldata$TC_gkg,
                          method = "pls",
                          tuneLength = pls_ncomp_max,
                          trControl = train_control,
                          preProcess = c("center", "scale"))


saveRDS(pls_model, "out/pls_model/SOC_PLSmodel.rds")

pls_model
plot(pls_model)
range(alldata$TC_gkg)

source("R/pls_eval.R")

eval <- pls_val_plot(pls_object = pls_model, data = alldata, ID = "sample_id", property = "TC", unit = "g/kg", label = "SOC", val_type = "CV")
eval$p_calval



# independent validation --------------------------------------------------

# kennard-stone sampling for independent validation
kS <- kenStone(X = alldata$abs_pre, k = 100,
               metric = "mahal", pc = 10,
               # group = as.factor(alldata$core_id),
               .center = TRUE, .scale = FALSE)


calset <- alldata[kS$model,]
valset <- alldata[kS$test,]

calset$rowIndex <- c(1:nrow(calset))

## train a pls regression model
pls_model <- caret::train(x = calset$abs_pre,
                          y = calset$TC_gkg,
                          method = "pls",
                          tuneLength = pls_ncomp_max,
                          trControl = train_control,
                          preProcess = c("center", "scale"))

pls_model


ind_validation <- pls_val_plot(pls_object = pls_model, data = calset, ID = "sample_id",
                               property = "TC_gkg", unit = "g/kg", label = "SOC",
                               val_type = "ind_val", valset = valset)

ind_validation$p_calval
