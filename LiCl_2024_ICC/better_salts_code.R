
Exp2_T4 = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T4_2_9[28151]\\2024-09-02T163134+1200[32251]\\2024-09-02T163134+1200[32251]", 
                          Exp2_wellplate) 
Exp2_T4$experiment = "Exp2"
Exp2_T4$timepoint= "T4"

Exp2_T12 = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T12_2_9[28152]\\2024-09-02T155017+1200[32252]\\2024-09-02T155017+1200[32252]",
                           Exp2_wellplate)
Exp2_T12$experiment= "Exp2"
Exp2_T12$timepoint= "T12"

Exp2_T4_and_T12<- rbind(Exp2_T4, Exp2_T12)

# Exp3 separate and combined times
Exp3_T4 = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T4_11_9[28354]\\2024-09-11T135358+1200[32454]\\2024-09-11T135358+1200[32454]", 
                          Exp3_wellplate)
Exp3_T4$experiment = "Exp3"
Exp3_T4$timepoint= "T4"

Exp3_T12 = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T12_11_9[28352]\\2024-09-11T130008+1200[32452]\\2024-09-11T130008+1200[32452]", 
                           Exp3_wellplate)
Exp3_T12$experiment = "Exp3"
Exp3_T12$timepoint= "T12"
Exp3_T4_and_T12<- rbind(Exp3_T4, Exp3_T12)

Exp4_T4 = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T4_21_9[28553]\\2024-09-21T153009+1200[32653]\\2024-09-21T153009+1200[32653]",
                          Exp4_wellplate)
Exp4_T4$experiment = "Exp4"
Exp4_T4$timepoint= "T4"

Exp4_T12 = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T12_21_9[28552]\\2024-09-21T162642+1200[32652]\\2024-09-21T162642+1200[32652]",
                           Exp4_wellplate)
Exp4_T12$experiment = "Exp4"
Exp4_T12$timepoint= "T12"

Exp4_T4_and_T12<- rbind(Exp4_T4, Exp4_T12)


# salts data
test_all_water_salts<- rbind(Exp2_T4_and_T12, Exp3_T4_and_T12, Exp4_T4_and_T12) %>%  
  filter(grepl(" in water", sample) | sample == "water") %>% 
  filter(grepl('VE-Cadherin|b-catenin', name_antibody)) %>% filter(sample != "Li ace in water")

test_all_water_salts <- test_all_water_salts  %>%
  mutate(sample =recode(sample, 'CaCl in water'= 'Cacl in water'))
  
Exp2_salt_VE_water<- test_all_water_salts %>% filter(name_antibody=="VE-Cadherin") %>% 
  filter(experiment=="Exp2")
quant_salt_Exp2_ve_water = segment_and_quant_p(
  Exp2_salt_VE_water, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.003 ,min_area =  10 , nuclear_area =  40 , nuclear_offset =  0.001 
)
norm_Exp2_ve_water <- normalise_to_water(quant_salt_Exp2_ve_water)

#exp 3
Exp3_salt_VE_water<- test_all_water_salts %>% filter(name_antibody=="VE-Cadherin") %>% 
  filter(experiment=="Exp3")
quant_salt_Exp3_ve_water = segment_and_quant_p(
  Exp3_salt_VE_water, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.0008 , min_area =  12 , nuclear_area =  40 , nuclear_offset =  0.001 
)
norm_Exp3_ve_water <- normalise_to_water(quant_salt_Exp3_ve_water)

# Exp 4
Exp4_salt_VE_water<- test_all_water_salts %>% filter(name_antibody=="VE-Cadherin") %>% 
  filter(experiment=="Exp4")
quant_salt_Exp4_ve_water = segment_and_quant_p(
  Exp4_salt_VE_water, nuclear_disk =  10 ,nuclear_area =  40 , nuclear_offset =  0.001,tophat_area =  15 , tophat_threshold =  0.001 , min_area =  15 
)
norm_Exp4_ve_water <- normalise_to_water(quant_salt_Exp4_ve_water)

# combine all normalised data
all_norm_ve_water_points<- rbind(norm_Exp2_ve_water, norm_Exp3_ve_water, norm_Exp4_ve_water) 
sum_norm_ve_water<- summarise_norm_data(all_norm_ve_water_points) 


# plotting using function
plot_bar(sum_norm_ve_water, "mean_cont_area", "se_cont_area", all_norm_ve_water_points, "norm_cont_area") 
# junctional area graph really nice, junctional fluorescence really messed up
plot_bar(sum_norm_ve_water, "mean_cont_fluoro", "se_cont_fluoro", all_norm_ve_water_points, "norm_cont_fluoro")


# water bcat --------------------------------------------------------------
test_all_water_salts <- test_all_water_salts  %>%
  mutate(sample =recode(sample, 'CaCl in water'= 'Cacl in water'))


Exp2_salt_bcat_water<- test_all_water_salts %>% filter(name_antibody=="b-catenin") %>% 
  filter(experiment=="Exp2")
quant_salt_Exp2_bcat_water = segment_and_quant_p(
  Exp2_salt_bcat_water, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.003 ,min_area =  10 , nuclear_area =  40 , nuclear_offset =  0.001 
)
norm_Exp2_bcat_water <- normalise_to_water(quant_salt_Exp2_bcat_water)

#exp 3
Exp3_salt_bcat_water<- test_all_water_salts %>% filter(name_antibody=="b-catenin") %>% 
  filter(experiment=="Exp3")
quant_salt_Exp3_bcat_water = segment_and_quant_p(
  Exp3_salt_bcat_water, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.0008 , min_area =  12 , nuclear_area =  40 , nuclear_offset =  0.001 
)
norm_Exp3_bcat_water <- normalise_to_water(quant_salt_Exp3_bcat_water)

# Exp 4
Exp4_salt_bcat_water<- test_all_water_salts %>% filter(name_antibody=="b-catenin") %>% 
  filter(experiment=="Exp4")
quant_salt_Exp4_bcat_water = segment_and_quant_p(
  Exp4_salt_bcat_water, nuclear_disk =  10 ,nuclear_area =  40 , nuclear_offset =  0.001,tophat_area =  15 , tophat_threshold =  0.001 , min_area =  15 
)
norm_Exp4_bcat_water <- normalise_to_water(quant_salt_Exp4_bcat_water)

# combine all normalised data
all_norm_bcat_water_points<- rbind(norm_Exp2_bcat_water, norm_Exp3_bcat_water, norm_Exp4_bcat_water)

sum_norm_bcat_water<- summarise_norm_data(all_norm_bcat_water_points)

# plotting using function
plot_bar(sum_norm_bcat_water, "mean_cont_area", "se_cont_area", all_norm_bcat_water_points, "norm_cont_area") 
# junctional area graph really nice, junctional fluorescence really messed up
plot_bar(sum_norm_bcat_water, "mean_cont_fluoro", "se_cont_fluoro", all_norm_bcat_water_points, "norm_cont_fluoro")


# plasmin -----------------------------------------------------------------
normalise_to_pl <- function(dataset){
  pl_means<- dataset %>% group_by(timepoint) %>% filter(sample =="Plasmin") %>% 
    summarise(
      mean_cont_area = mean(contiguous_area),
      mean_cont_fluoro = mean(contiguous_fluorescence))
  
  pl_T4 <- filter(pl_means, timepoint=="T4")
  pl_T12 <- filter(pl_means, timepoint=="T12")
  
  T4_normalised <- dataset %>%  filter(timepoint =="T4" ) %>% 
    mutate(norm_cont_area = contiguous_area/pl_T4$mean_cont_area,
           norm_cont_fluoro = contiguous_fluorescence/pl_T4$mean_cont_fluoro)
  
  T12_normalised <- dataset %>% 
    filter(timepoint =="T12" ) %>% 
    mutate(norm_cont_area = contiguous_area/pl_T12$mean_cont_area,
           norm_cont_fluoro = contiguous_fluorescence/pl_T12$mean_cont_fluoro) 
  
  Combined_norm_data <- rbind(T4_normalised, T12_normalised) 
  return(Combined_norm_data)
}

# ve ----------------------------------------------------------------------

test_all_pl_salts<- rbind(Exp3_T4_and_T12, Exp4_T4_and_T12) %>%  
  filter(grepl(" in plasmin", sample) | sample == "Plasmin") %>% 
  filter(grepl('VE-Cadherin|b-catenin', name_antibody)) %>% filter(sample != "Li ace in water")

test_all_pl_salts <- test_all_pl_salts  %>%
  mutate(sample =recode(sample, 'CaCl in water'= 'Cacl in water'))


#exp 3
Exp3_salt_VE_pl<- test_all_pl_salts %>% filter(name_antibody=="VE-Cadherin") %>% 
  filter(experiment=="Exp3")
quant_salt_Exp3_ve_pl = segment_and_quant_p(
  Exp3_salt_VE_pl, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.0008 , min_area =  12 , nuclear_area =  40 , nuclear_offset =  0.001 
)

norm_Exp3_ve_pl <- normalise_to_pl(quant_salt_Exp3_ve_pl)



# Exp 4
Exp4_salt_VE_pl<- test_all_pl_salts %>% filter(name_antibody=="VE-Cadherin") %>% 
  filter(experiment=="Exp4")
quant_salt_Exp4_ve_pl = segment_and_quant_p(
  Exp4_salt_VE_pl, nuclear_disk =  10 ,nuclear_area =  40 , nuclear_offset =  0.001,tophat_area =  15 , tophat_threshold =  0.001 , min_area =  15 
)
norm_Exp4_ve_pl <- normalise_to_pl(quant_salt_Exp4_ve_pl)

# combine all normalised data
all_norm_ve_pl_points<- rbind(norm_Exp3_ve_pl, norm_Exp4_ve_pl) 
sum_norm_ve_pl<- summarise_norm_data(all_norm_ve_pl_points) 


# plotting using function
plot_bar(sum_norm_ve_pl, "mean_cont_area", "se_cont_area", all_norm_ve_pl_points, "norm_cont_area") 
plot_bar(sum_norm_ve_pl, "mean_cont_fluoro", "se_cont_fluoro", all_norm_ve_pl_points, "norm_cont_fluoro")


# ve ----------------------------------------------------------------------



