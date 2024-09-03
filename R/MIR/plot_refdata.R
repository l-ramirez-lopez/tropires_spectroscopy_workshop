library(tidyverse)


cal <- read_csv("data/calibration_data/metadata_calibration_samples.csv") %>% 
  select(sample_id, TC_gkg) %>% 
  mutate(set = "calibration")
val <- read_csv("data/validation_samples/validation_samples.csv") %>% 
  select(sample_id, TC_gkg) %>% 
  mutate(set = "validation")

merged <- bind_rows(cal, val)

library(ggridges)

(p_ref <- ggplot(merged, aes(x = TC_gkg, y = set))+
  geom_density_ridges(aes(fill = set, point_fill = set), jittered_points = TRUE, point_shape = 21, point_size = 3, point_alpha = 0.9, alpha = 0.9) +
  scale_fill_manual(values = c("#ff9501", "#e91fa9"))+
  scale_discrete_manual(aesthetics = "point_fill", values = c("#ff9501", "#e91fa9"))+
  ylab("") +
  xlab(expression(paste("SOC (g ", kg^{-1}, ")"))) +
  theme_minimal() +
  theme(legend.position = "none"))


ggsave(p_ref, filename = "figures/refdata_calset_valset.png", width = 6, height = 2.5)



range(val$TC_gkg)
