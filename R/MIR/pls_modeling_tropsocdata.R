

library(prospectr)
library(caret)
library(tidyverse)


tropsoc_data <- readRDS("data/calibration_data/NIRabs_TropSOC_Uganda.rds")


str(tropsoc_data)


## smoothen data
wavs <- as.numeric(colnames(tropsoc_data$abs))
range(wavs)
wavs_pre <- as.numeric(colnames(tropsoc_data$abs_pre))

tropsoc_data$abs_pre <- tropsoc_data$abs |>
  movav(w = 19)
  # detrend(wav = wavs_pre, p = 2)
  # standardNormalVariate() 
  # savitzkyGolay(m = 2, p = 2, w = 17) 

# raw absorbance
matplot(x = as.numeric(colnames(tropsoc_data$abs)), y = tropsoc_data$abs[1,],
        xlab = expression(paste("Wavenumber ", cm^{-1})),
        ylab = 'Absorbance',
        type = 'l',
        lty = 1, 
        # ylim = c(0.2, 0.8),
        xlim = c(7500, 3900))

# pre-processed absorbance data
matplot(x = as.numeric(colnames(tropsoc_data$abs_pre)), y = tropsoc_data$abs_pre[1,],
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

tropsoc_data$rowIndex <- c(1:nrow(tropsoc_data))


## train a pls regression model
pls_model <- caret::train(x = tropsoc_data$abs_pre,
                          y = tropsoc_data$TC_gkg,
                          method = "pls",
                          tuneLength = pls_ncomp_max,
                          trControl = train_control,
                          preProcess = c("center", "scale"))


pls_model
plot(pls_model)
range(tropsoc_data$TC_gkg)

source("R/pls_eval.R")

eval <- pls_val_plot(pls_object = pls_model, data = tropsoc_data, ID = "sample_id", property = "TC", unit = "g/kg", label = "SOC", val_type = "CV")
eval$p_calval




# independent validation --------------------------------------------------

# kennard-stone sampling for independent validation
kS <- kenStone(X = tropsoc_data$abs_pre, k = 70,
               metric = "mahal", pc = 10,
               group = as.factor(tropsoc_data$core_id),
               .center = TRUE, .scale = FALSE)


calset <- tropsoc_data[kS$model,]
valset <- tropsoc_data[kS$test,]

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
