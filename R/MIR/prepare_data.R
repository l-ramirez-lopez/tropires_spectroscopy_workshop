


###########################################################################
###########################################################################
###                                                                     ###
###                PREPARE DATA FOR CALIBRATION MODELING                ###
###                                                                     ###
###########################################################################
###########################################################################

# 20240815
# Laura Summerauer


# read prepared data ------------------------------------------------------

# TropSOC data
spc_tropsoc <- readRDS("data/calibration_data/raw/TropSOC_NIRabs_Uganda.rds")

# Laura PhD data
spc_laura <- readRDS("data/calibration_data/raw/NIRspcmean_UgandaPhDSamples.rds")


# rename and adjust column names
colnames(spc_tropsoc)
colnames(spc_laura)

spc_tropsoc$core_id <- spc_tropsoc$sample_location
spc_tropsoc$lat <- spc_tropsoc$gps_lat
spc_tropsoc$long <- spc_tropsoc$gps_long
spc_tropsoc$depth <- spc_tropsoc$sampling_layer
spc_tropsoc$TC_gkg <- spc_tropsoc$TC
spc_tropsoc$abs <- spc_tropsoc$spc_mean

spc_laura$core_id <- spc_laura$point_id
spc_laura$land_use <- "cropland"
spc_laura$abs <- spc_laura$spc_mean


cols_sel <- c("sample_id", "core_id", "country_code", "lat", "long", "land_use", "depth", "TC_gkg", "abs")

spc_tropsoc_renamed <- spc_tropsoc[,cols_sel]
spc_laura_renamed <- spc_laura[,cols_sel]

# save data
saveRDS(spc_tropsoc_renamed, "data/calibration_data/NIRabs_TropSOC_Uganda.rds")
saveRDS(spc_laura_renamed, "data/calibration_data/NIRabs_PhDLaura_Uganda.rds")

