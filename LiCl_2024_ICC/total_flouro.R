# total and cytoplasm flourescence

normalise_overall_stain <- function(dataset){
  water_means<- dataset %>% group_by(timepoint) %>% filter(sample =="water") %>% 
    summarise(
      mean_overall_stain = mean(overall_stain)
      )
  
  water_T4 <- filter(water_means, timepoint=="T4")
  water_T12 <- filter(water_means, timepoint=="T12")
  
  T4_normalised <- dataset %>%  filter(timepoint =="T4" ) %>% 
    mutate(norm_overall_stain = overall_stain/water_T4$mean_overall_stain)
  
  T12_normalised <- dataset %>% 
    filter(timepoint =="T12" ) %>% 
    mutate(norm_overall_stain = overall_stain/water_T12$mean_overall_stain) 
  
  Combined_norm_data <- rbind(T4_normalised, T12_normalised) 
  return(Combined_norm_data)
}

summarise_norm_overall_stain<- function(normalised_points){
  normalised_points %>% 
    group_by(sample, timepoint) %>% 
    summarise(
      mean_overall_stain = mean(norm_overall_stain),
      sd_overall_stain = sd(norm_overall_stain),
      se_overall_stain = sd_overall_stain / sqrt(n())
    )
}

#vecad. os means overall stain


norm_os_Exp2_ve_cad<- normalise_overall_stain(quant_Exp2_ve_cad)
norm_os_Exp3_ve_cad<- normalise_overall_stain(quant_Exp3_ve_cad)
norm_os_Exp4_ve_cad<- normalise_overall_stain(quant_Exp4_ve_cad)

all_os_VE_norm_points <- rbind(norm_os_Exp2_ve_cad, norm_os_Exp3_ve_cad, norm_os_Exp4_ve_cad)
all_os_VE_norm_points$timepoint <- factor(all_os_VE_norm_points$timepoint, levels=c("T4", "T12"))

all_os_VE_summarised <- summarise_norm_overall_stain (all_os_VE_norm_points) 
all_os_VE_summarised$timepoint <- factor(all_os_VE_summarised$timepoint, levels=c("T4", "T12"))

plot_bar(all_os_VE_summarised, "mean_overall_stain", "se_overall_stain", all_os_VE_norm_points, "norm_overall_stain")

### bcat

norm_os_Exp2_bcat<- normalise_overall_stain(quant_Exp2_bcat)
norm_os_Exp3_bcat<- normalise_overall_stain(quant_Exp3_bcat)
norm_os_Exp4_bcat<- normalise_overall_stain(quant_Exp4_bcat)

all_os_bcat_norm_points <- rbind(norm_os_Exp2_bcat, norm_os_Exp3_bcat, norm_os_Exp4_bcat)
all_os_bcat_norm_points$timepoint <- factor(all_bcat_norm_points$timepoint, levels=c("T4", "T12"))

all_os_bcat_summarised <- summarise_norm_overall_stain (all_os_bcat_norm_points) 
all_os_bcat_summarised$timepoint <- factor(all_os_bcat_summarised$timepoint, levels=c("T4", "T12"))

plot_bar(all_os_bcat_summarised, "mean_overall_stain", "se_overall_stain", all_os_bcat_norm_points, "norm_overall_stain")


### claudin-5
norm_os_Exp2_claudin<- normalise_overall_stain(quant_Exp2_claudin)
norm_os_Exp3_claudin<- normalise_overall_stain(quant_Exp3_claudin)
norm_os_Exp4_claudin<- normalise_overall_stain(quant_Exp4_claudin)

all_os_claudin_norm_points <- rbind(norm_os_Exp2_claudin, norm_os_Exp3_claudin, norm_os_Exp4_claudin)
all_os_claudin_norm_points$timepoint <- factor(all_claudin_norm_points$timepoint, levels=c("T4", "T12"))

all_os_claudin_summarised <- summarise_norm_overall_stain (all_os_claudin_norm_points) 
all_os_claudin_summarised$timepoint <- factor(all_os_claudin_summarised$timepoint, levels=c("T4", "T12"))

plot_bar(all_os_claudin_summarised, "mean_overall_stain", "se_overall_stain", all_os_claudin_norm_points, "norm_overall_stain")+ylim(0,1.75)

## zono
norm_os_Exp2_zono<- normalise_overall_stain(quant_Exp2_zono)
norm_os_Exp3_zono<- normalise_overall_stain(quant_Exp3_zono)
norm_os_Exp4_zono<- normalise_overall_stain(quant_Exp4_zono)

all_os_zono_norm_points <- rbind(norm_os_Exp2_zono, norm_os_Exp3_zono, norm_os_Exp4_zono)
all_os_zono_norm_points$timepoint <- factor(all_zono_norm_points$timepoint, levels=c("T4", "T12"))

all_os_zono_summarised <- summarise_norm_overall_stain (all_os_zono_norm_points) 
all_os_zono_summarised$timepoint <- factor(all_os_zono_summarised$timepoint, levels=c("T4", "T12"))

plot_bar(all_os_zono_summarised, "mean_overall_stain", "se_overall_stain", all_os_zono_norm_points, "norm_overall_stain") +ylim(0, 1.75)

