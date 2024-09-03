pls_val_plot <- function(pls_object, data, ID, property, unit, label, val_type, valset = NULL) {
  
  pls_model <- pls_object
  sample_id <- ID
  calset <- data
  label <- label
  
  if(val_type == "CV") {
  
      # final model / Calibration
      predobs <- extractPrediction(models = list(pls_model))
      predobs$sample_id <- calset[[sample_id]]
      
      predobs_sel <- predobs %>% 
        mutate(dataType = "Calibration") %>% 
        select(obs, pred, model, dataType, sample_id)
      
      # get hold out predictions from cv
      predobs_cv <- inner_join(pls_model$pred, pls_model$bestTune, by = "ncomp")
      cal_idx <- calset[,c(sample_id, "rowIndex")]
      
    
      ##################### needs work.. just quickly copied from philipp..
      sem_ci <- function(x) {
        qt(0.975, df = length(na.omit(x)) - 1) *
          sqrt(var(x, na.rm = TRUE) / length(na.omit(x)))
      }
    
    
      predobs_cv_ed <- full_join(cal_idx, predobs_cv, by = "rowIndex", multiple = "all") %>%
        select(-ncomp, -Resample) %>% 
        group_by(sample_id) %>% 
        # Average observed and predicted values
        mutate("obs" = mean(obs),
               "pred_sd" = sd(pred)) %>%
        # Add 95% confidence interval for mean hold-out predictions from
        # repeated k-fold cross-validation
        mutate("pred_sem_ci" = sem_ci(pred)) %>%
        # Add mean hold-out predictions from repeated k-fold cross-validation
        mutate("pred" = mean(pred)) %>%
        # Slice data set to only have one row per sample_rep
        slice(1L) %>% 
        ungroup()
    
    
      predobs_cv_ed$model <- pls_model$method
      predobs_cv_ed$dataType <- factor("Cross-validation")
    
      predobs_cv_ed_sel <-predobs_cv_ed %>%
        select("obs", "pred", "pred_sd", "pred_sem_ci",
               "model", "dataType", "sample_id")
    
    
      predobs_all <- bind_rows(predobs_sel, predobs_cv_ed_sel)
    
      predobs_all
    
      
      ##################################################################
      ##           1. Stats and graph for all combined data           ##
      ##################################################################
    
      predobs_all$dataType <- factor(predobs_all$dataType)
      predobs_all$dataType
    
      summary_stats <- predobs_all %>%
        group_by(dataType) %>%
        dplyr::summarize(n = n(),
                         min_obs = round(range(obs)[1], 2),
                         median_obs = round(median(obs), 2),
                         max_obs = round(range(obs)[2], 2),
                         mean_obs = round(mean(obs), 2),
                         CV = round(sd(obs)/mean(obs)*100, 2),
                         rmse = round(mean((obs - pred)^2, na.rm = TRUE)^.5, 2),
                         me_bias = round(mean(obs - pred, na.rm = TRUE), 2),
                         sde = round(mean(((mean(obs, na.rm = TRUE) - obs)
                                           - (mean(pred, na.rm = TRUE) - pred))^2)^0.5, 2),
                         r2  = round(cor(obs, pred, use = "pairwise.complete.obs")^2, 2),
                         rpd = round(sd(obs, na.rm = TRUE) / mean((obs - pred)^2, na.rm = TRUE)^.5, 2),
                         rpiq = round((quantile(obs, .75, na.rm = TRUE) - quantile(obs, .25, na.rm = TRUE))
                                      / mean((obs - pred)^2, na.rm = TRUE)^.5, 2)) %>% 
        ungroup()
    
      summary_stats
      summary_stats$ncomp <- pls_model$bestTune[1,]
    
      xy_min <- if (min(predobs_all$obs) < min(predobs_all$pred))
      {predobs_all$obs} else {predobs_all$pred}
      xy_max <- if (max(predobs_all$obs) > max(predobs_all$pred))
      {predobs_all$obs} else {predobs_all$pred}
      xy_range <- ifelse(diff(range(xy_min) > diff(range(xy_max))),
                         diff(range(xy_min)), diff(range(xy_max)))
    
    
    
      # text in each plot
      annotation <- mutate(summary_stats,
                           rmse = as.character(as.expression(paste0("RMSE == ",
                                                                    round(rmse, 2)))),
                           r2 = as.character(as.expression(paste0("italic(R)^2 == ",
                                                                  round(r2, 2)))),
                           rpiq = as.character(as.expression(paste("RPIQ == ",
                                                                   round(rpiq, 2)))),
                           n = as.character(as.expression(paste0("italic(n) == ", n))),
                           ncomp = as.character(as.expression(paste0("ncomp == ",
                                                                     ncomp)))
      )
    
      
      p_calval <- ggplot(predobs_all, aes(x = obs, y = pred)) +
        geom_point() +
        geom_errorbar(aes(x = obs, ymin = pred - pred_sem_ci, ymax = pred + pred_sem_ci),
                      width = 0, data = predobs_all, inherit.aes = FALSE) +
        geom_abline() +
        facet_grid(~dataType, scales = "free") +
        ggplot2::geom_text(data = annotation,
                           ggplot2::aes(x = Inf, y = -Inf, label = n), size = 5,
                           hjust = 1.23, vjust = -7.25, parse = TRUE) +
        ggplot2::geom_text(data = annotation,
                           ggplot2::aes(x = Inf, y = -Inf, label = ncomp), size = 5,
                           hjust = 1.15, vjust = -4.5, parse = TRUE) +
        ggplot2::geom_text(data = annotation,
                           ggplot2::aes(x = Inf, y = -Inf, label = r2), size = 5,
                           hjust = 1.15, vjust = -3, parse = TRUE) +
        ggplot2::geom_text(data = annotation,
                           ggplot2::aes(x = Inf, y = -Inf, label = rmse), size = 5,
                           hjust = 1.12, vjust = -2.5, parse = TRUE) +
        ggplot2::geom_text(data = annotation,
                           ggplot2::aes(x = Inf, y = -Inf, label = rpiq), size = 5,
                           hjust = 1.12, vjust = -0.5, parse = TRUE) +
    
        xlim(c(min(xy_min) - 0.05 * xy_range,
               max(xy_max) + 0.05 * xy_range)) +
        ylim(c(min(xy_min) - 0.05 * xy_range,
               max(xy_max) + 0.05 * xy_range)) +
        xlab(paste0("Measured ", label, " (", unit, ")")) +
        ylab(paste0("Predicted ", label, " (", unit, ")")) +
        theme_bw()+
        theme(legend.position ="none")
      
      
      
      predobs_cv_out <- predobs_all %>% filter(dataType == "Cross-validation")
      annotation_cv_out <- annotation %>% filter(dataType == "Cross-validation")
     
       p_val <-  ggplot(predobs_cv_out, aes(x = obs, y = pred)) +
        geom_point() +
        geom_errorbar(aes(x = obs, ymin = pred - pred_sem_ci, ymax = pred + pred_sem_ci),
                      width = 0, data = predobs_all, inherit.aes = FALSE) +
        geom_abline() +
        ggplot2::geom_text(data = annotation_cv_out,
                           ggplot2::aes(x = Inf, y = -Inf, label = n), size = 5,
                           hjust = 1.23, vjust = -7.25, parse = TRUE) +
        ggplot2::geom_text(data = annotation_cv_out,
                           ggplot2::aes(x = Inf, y = -Inf, label = ncomp), size = 5,
                           hjust = 1.15, vjust = -4.5, parse = TRUE) +
        ggplot2::geom_text(data = annotation_cv_out,
                           ggplot2::aes(x = Inf, y = -Inf, label = r2), size = 5,
                           hjust = 1.15, vjust = -3, parse = TRUE) +
        ggplot2::geom_text(data = annotation_cv_out,
                           ggplot2::aes(x = Inf, y = -Inf, label = rmse), size = 5,
                           hjust = 1.12, vjust = -2.5, parse = TRUE) +
        ggplot2::geom_text(data = annotation_cv_out,
                           ggplot2::aes(x = Inf, y = -Inf, label = rpiq), size = 5,
                           hjust = 1.12, vjust = -0.5, parse = TRUE) +
        
        xlim(c(min(xy_min) - 0.05 * xy_range,
               max(xy_max) + 0.05 * xy_range)) +
        ylim(c(min(xy_min) - 0.05 * xy_range,
               max(xy_max) + 0.05 * xy_range)) +
        xlab(paste0("Measured ", label, " (", unit, ")")) +
        ylab(paste0("Predicted ", label, " (", unit, ")")) +
        theme_bw()+
        theme(legend.position ="none")
      
      lst_out <- list(
        predobs = predobs_all,
        stats = summary_stats,
        p_calval = p_calval,
        p_val = p_val)
      
      
  } else if (val_type == "ind_val"){
    
    valset <- valset
    testX <- valset$abs_pre
    testY <- valset[[property]]
    
    predobs <- extractPrediction(models = list("pls_model" = pls_model), testX = testX, testY = testY) %>% 
      mutate(dataType = ifelse(dataType == "Training", "Calibration", "Validation"))
    predobs$sample_id <- c(calset$sample_id, valset$sample_id)
    
    summary_stats <- predobs %>% 
      group_by(dataType, model) %>% 
      summarize(n = n(),
                min_obs = round(range(obs)[1], 2), 
                median_obs = round(median(obs), 2),
                max_obs = round(range(obs)[2], 2),
                mean_obs = round(mean(obs), 2),
                CV = round(sd(obs)/mean(obs)*100, 2),
                rmse = round(mean((obs - pred)^2, na.rm = TRUE)^.5, 2),
                me_bias = round(mean(obs - pred, na.rm = TRUE), 2),
                sde = round(mean(((mean(obs, na.rm = TRUE) - obs)
                                  - (mean(pred, na.rm = TRUE) - pred))^2)^0.5, 2),
                r2  = round(cor(obs, pred, use = "pairwise.complete.obs")^2, 2),
                rpd = round(sd(obs, na.rm = TRUE) / mean((obs - pred)^2, na.rm = TRUE)^.5, 2),
                rpiq = round((quantile(obs, .75, na.rm = TRUE) - quantile(obs, .25, na.rm = TRUE))
                             / mean((obs - pred)^2, na.rm = TRUE)^.5, 2)) %>% 
      ungroup()
    
    ncomp <- data.frame(model = c("pls"), ncomp = c(pls_model$bestTune$ncomp))
    summary_ncomp <- inner_join(summary_stats, ncomp, by = "model")
    
    # text in each plot
    annotation <- mutate(summary_ncomp %>% group_by(dataType, model),
                         rmse = as.character(as.expression(paste0("RMSE == ",
                                                                  round(rmse, 2)))),
                         r2 = as.character(as.expression(paste0("italic(R)^2 == ",
                                                                round(r2, 2)))),
                         rpiq = as.character(as.expression(paste("RPIQ == ",
                                                                 round(rpiq, 2)))),
                         n = as.character(as.expression(paste0("italic(n) == ", n))),
                         ncomp = as.character(as.expression(paste0("ncomp == ",
                                                                   ncomp)))
    )
    
    xy_min <- if (min(predobs$obs) < min(predobs$pred))
    {predobs$obs} else {predobs$pred}
    xy_max <- if (max(predobs$obs) > max(predobs$pred))
    {predobs$obs} else {predobs$pred}
    xy_range <- ifelse(diff(range(xy_min) > diff(range(xy_max))),
                       diff(range(xy_min)), diff(range(xy_max)))

    
    # Plot predicted vs. observed values for clay
    
    p_calval <- ggplot(predobs, aes(x = obs, y = pred)) +
      geom_point() +
      geom_abline() +
      
      facet_wrap(. ~ dataType, ncol = 2) +
      
      ggplot2::geom_text(data = annotation,
                         ggplot2::aes(x = Inf, y = -Inf, label = n), size = 5,
                         hjust = 1.23, vjust = -7.25, parse = TRUE) +
      ggplot2::geom_text(data = annotation,
                         ggplot2::aes(x = Inf, y = -Inf, label = ncomp), size = 5,
                         hjust = 1.15, vjust = -4.5, parse = TRUE) +
      ggplot2::geom_text(data = annotation,
                         ggplot2::aes(x = Inf, y = -Inf, label = r2), size = 5,
                         hjust = 1.15, vjust = -3, parse = TRUE) +
      ggplot2::geom_text(data = annotation,
                         ggplot2::aes(x = Inf, y = -Inf, label = rmse), size = 5,
                         hjust = 1.12, vjust = -2.5, parse = TRUE) +
      ggplot2::geom_text(data = annotation,
                         ggplot2::aes(x = Inf, y = -Inf, label = rpiq), size = 5,
                         hjust = 1.15, vjust = -1.25, parse = TRUE) +
      xlim(c(min(xy_min) - 0.05 * xy_range,
             max(xy_max) + 0.05 * xy_range)) +
      ylim(c(min(xy_min) - 0.05 * xy_range,
             max(xy_max) + 0.05 * xy_range)) +
      xlab(paste0("Measured ", label, " (", unit, ")")) +
      ylab(paste0("Predicted ", label, " (", unit, ")")) +
      theme_bw() 
    
    
    

    
    predobs_iv <- predobs %>% filter(dataType == "Validation")
    annotation_iv_out <- annotation %>% filter(dataType == "Validation")
    
    
    xy_min <- if (min(predobs_iv$obs) < min(predobs_iv$pred))
    {predobs_iv$obs} else {predobs_iv$pred}
    xy_max <- if (max(predobs_iv$obs) > max(predobs_iv$pred))
    {predobs_iv$obs} else {predobs_iv$pred}
    xy_range <- ifelse(diff(range(xy_min) > diff(range(xy_max))),
                       diff(range(xy_min)), diff(range(xy_max)))
    
    
    p_val <-  ggplot(predobs_iv, aes(x = obs, y = pred)) +
      geom_point() +
      geom_abline() +
      ggplot2::geom_text(data = annotation_iv_out,
                         ggplot2::aes(x = Inf, y = -Inf, label = n), size = 5,
                         hjust = 1.23, vjust = -7.25, parse = TRUE) +
      ggplot2::geom_text(data = annotation_iv_out,
                         ggplot2::aes(x = Inf, y = -Inf, label = ncomp), size = 5,
                         hjust = 1.15, vjust = -4.5, parse = TRUE) +
      ggplot2::geom_text(data = annotation_iv_out,
                         ggplot2::aes(x = Inf, y = -Inf, label = r2), size = 5,
                         hjust = 1.15, vjust = -3, parse = TRUE) +
      ggplot2::geom_text(data = annotation_iv_out,
                         ggplot2::aes(x = Inf, y = -Inf, label = rmse), size = 5,
                         hjust = 1.12, vjust = -2.5, parse = TRUE) +
      ggplot2::geom_text(data = annotation_iv_out,
                         ggplot2::aes(x = Inf, y = -Inf, label = rpiq), size = 5,
                         hjust = 1.12, vjust = -0.5, parse = TRUE) +
      
      xlim(c(min(xy_min) - 0.05 * xy_range,
             max(xy_max) + 0.05 * xy_range)) +
      ylim(c(min(xy_min) - 0.05 * xy_range,
             max(xy_max) + 0.05 * xy_range)) +
      xlab(paste0("Measured ", label, " (", unit, ")")) +
      ylab(paste0("Predicted ", label, " (", unit, ")")) +
      theme_bw()+
      theme(legend.position ="none")
    
    lst_out <- list(
      predobs = predobs,
      stats = summary_ncomp,
      p_calval = p_calval,
      p_val = p_val)
  }
  
  
  
  
  return(lst_out)
  
  }