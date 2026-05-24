
# Set up ------------------------------------------------------------------

library(tidyverse)
library(BiocManager)
library(EBImage)
library(tools)
library(data.table)
library("shiny")
library("bslib")
library(progressr)
library(doFuture)
library(patchwork)
library(gglm)


#Since honours, I have since put code and data onto uni laptop, under C disk/ gitfolder/ LiCl/ LiCl2024
# functions are in this script
source("vjunctur_functions_v1.R")



# wellmaps ----------------------------------------------------------------

Exp2_wellplate = tribble(~well, ~ch_dapi, ~ch_actin, ~ch_antibody, ~name_antibody, ~sample,
                         "A02", 2, 3, 4, "VE-Cadherin", "Plasmin",
                         "B02", 2, 3, 4, "VE-Cadherin", "Low LiCl + plasmin",
                         "C02", 2, 3, 4, "VE-Cadherin", "High LiCl + plasmin",
                         "D02", 2, 3, 4, "VE-Cadherin", "water",
                         "E02", 2, 3, 4, "VE-Cadherin", "Low LiCl + water",
                         "F02", 2, 3, 4, "VE-Cadherin", "High LiCl + water",
                         
                         "A03", 2, 3, 4, "b-catenin", "Plasmin",
                         "B03", 2, 3, 4, "b-catenin", "Low LiCl + plasmin",
                         "C03", 2, 3, 4, "b-catenin", "High LiCl + plasmin",
                         "D03", 2, 3, 4, "b-catenin", "water",
                         "E03", 2, 3, 4, "b-catenin", "Low LiCl + water",
                         "F03", 2, 3, 4, "b-catenin", "High LiCl + water",
                         
                         "A04", 2, 3, 4, "zono occluden", "Plasmin",
                         "B04", 2, 3, 4, "zono occluden", "Low LiCl + plasmin",
                         "C04", 2, 3, 4, "zono occluden", "High LiCl + plasmin",
                         "D04", 2, 3, 4, "zono occluden", "water",
                         "E04", 2, 3, 4, "zono occluden", "Low LiCl + water",
                         "F04", 2, 3, 4, "zono occluden", "High LiCl + water",
                         
                         "A04", 2, 3, 1, "claudin 5", "Plasmin",
                         "B04", 2, 3, 1, "claudin 5", "Low LiCl + plasmin",
                         "C04", 2, 3, 1, "claudin 5", "High LiCl + plasmin",
                         "D04", 2, 3, 1, "claudin 5", "water",
                         "E04", 2, 3, 1, "claudin 5", "Low LiCl + water",
                         "F04", 2, 3, 1, "claudin 5", "High LiCl + water",
                         
                         "A05", 2, 3, 4, "pecam", "Plasmin",
                         "B05", 2, 3, 4, "pecam", "Low LiCl + plasmin",
                         "C05", 2, 3, 4, "pecam", "High LiCl + plasmin",
                         "D05", 2, 3, 4, "pecam", "water",
                         "E05", 2, 3, 4, "pecam", "Low LiCl + water",
                         "F05", 2, 3, 4, "pecam", "High LiCl + water",
                         
                         "G02", 2, 3, 4, "VE-Cadherin", "Nacl in water",
                         "G03", 2, 3, 4, "b-catenin", "Nacl in water",
                         "G04", 2, 3, 4, "VE-Cadherin", "Kcl in water",
                         "G05", 2, 3, 4, "b-catenin", "Kcl in water",
                         "H02", 2, 3, 4, "VE-Cadherin", "CaCl in water",
                         "G04", 2, 3, 4, "b-catenin", "CaCl in water",
)

Exp3_wellplate = tribble(~well, ~ch_dapi, ~ch_actin, ~ch_antibody, ~name_antibody, ~sample,
                         "A02", 2, 3, 4, "VE-Cadherin", "Plasmin",
                         "B02", 2, 3, 4, "VE-Cadherin", "Low LiCl + plasmin",
                         "C02", 2, 3, 4, "VE-Cadherin", "High LiCl + plasmin",
                         "D02", 2, 3, 4, "VE-Cadherin", "water",
                         "E02", 2, 3, 4, "VE-Cadherin", "Low LiCl + water",
                         "F02", 2, 3, 4, "VE-Cadherin", "High LiCl + water",
                         
                         "A03", 2, 3, 4, "b-catenin", "Plasmin",
                         "B03", 2, 3, 4, "b-catenin", "Low LiCl + plasmin",
                         "C03", 2, 3, 4, "b-catenin", "High LiCl + plasmin",
                         "D03", 2, 3, 4, "b-catenin", "water",
                         "E03", 2, 3, 4, "b-catenin", "Low LiCl + water",
                         "F03", 2, 3, 4, "b-catenin", "High LiCl + water",
                         
                         "A04", 2, 3, 4, "zono occluden", "Plasmin",
                         "B04", 2, 3, 4, "zono occluden", "Low LiCl + plasmin",
                         "C04", 2, 3, 4, "zono occluden", "High LiCl + plasmin",
                         "D04", 2, 3, 4, "zono occluden", "water",
                         "E04", 2, 3, 4, "zono occluden", "Low LiCl + water",
                         "F04", 2, 3, 4, "zono occluden", "High LiCl + water",
                         
                         "A04", 2, 3, 1, "claudin 5", "Plasmin",
                         "B04", 2, 3, 1, "claudin 5", "Low LiCl + plasmin",
                         "C04", 2, 3, 1, "claudin 5", "High LiCl + plasmin",
                         "D04", 2, 3, 1, "claudin 5", "water",
                         "E04", 2, 3, 1, "claudin 5", "Low LiCl + water",
                         "F04", 2, 3, 1, "claudin 5", "High LiCl + water",
                         
                         "A05", 2, 3, 4, "pecam", "Plasmin",
                         "B05", 2, 3, 4, "pecam", "Low LiCl + plasmin",
                         "C05", 2, 3, 4, "pecam", "High LiCl + plasmin",
                         "D05", 2, 3, 4, "pecam", "water",
                         "E05", 2, 3, 4, "pecam", "Low LiCl + water",
                         "F05", 2, 3, 4, "pecam", "High LiCl + water",
                         
                         "A01", 2, 3, 4, "VE-Cadherin", "Nacl in plasmin",
                         "B01", 2, 3, 4, "VE-Cadherin", "Nacl in water",
                         "C01", 2, 3, 4, "b-catenin", "Nacl in plasmin",
                         "D01", 2, 3, 4, "b-catenin", "Nacl in water",
                         "E01", 2, 3, 4, "VE-Cadherin", "Kcl in plasmin",
                         "F01", 2, 3, 4, "VE-Cadherin", "Kcl in water",
                         "G01", 2, 3, 4, "b-catenin", "Kcl in plasmin",
                         "H01", 2, 3, 4, "b-catenin", "Kcl in water",
                         "G02", 2, 3, 4, "VE-Cadherin", "Cacl in plasmin",
                         "G03", 2, 3, 4, "VE-Cadherin", "Cacl in water",
                         "G04", 2, 3, 4, "b-catenin", "Cacl in plasmin",
                         "G05", 2, 3, 4, "b-catenin", "Cacl in water",
                         "H02", 2, 3, 4, "VE-Cadherin", "Li ace in plasmin",
                         "H03", 2, 3, 4, "VE-Cadherin", "Li ace in water",
                         "H04", 2, 3, 4, "b-catenin", "Li ace in plasmin",
                         "H05", 2, 3, 4, "b-catenin", "Li ace in water",)

Exp4_wellplate = tribble(~well, ~ch_dapi, ~ch_actin, ~ch_antibody, ~name_antibody, ~sample,
                         "A09", 2, 3, 4, "VE-Cadherin", "High LiCl + water",
                         "B09", 2, 3, 4, "VE-Cadherin", "Low LiCl + water",
                         "C09", 2, 3, 4, "VE-Cadherin", "water",
                         "D09", 2, 3, 4, "VE-Cadherin", "High LiCl + plasmin",
                         "E09", 2, 3, 4, "VE-Cadherin", "Low LiCl + plasmin",
                         "F09", 2, 3, 4, "VE-Cadherin", "Plasmin",
                         
                         "A10", 2, 3, 4, "b-catenin", "High LiCl + water",
                         "B10", 2, 3, 4, "b-catenin", "Low LiCl + water",
                         "C10", 2, 3, 4, "b-catenin", "water",
                         "D10", 2, 3, 4, "b-catenin", "High LiCl + plasmin",
                         "E10", 2, 3, 4, "b-catenin", "Low LiCl + plasmin",
                         "F10", 2, 3, 4, "b-catenin", "Plasmin",
                         
                         "A11", 2, 3, 4, "zono occluden", "High LiCl + water",
                         "B11", 2, 3, 4, "zono occluden", "Low LiCl + water",
                         "C11", 2, 3, 4, "zono occluden", "water",
                         "D11", 2, 3, 4, "zono occluden", "High LiCl + plasmin",
                         "E11", 2, 3, 4, "zono occluden", "Low LiCl + plasmin",
                         "F11", 2, 3, 4, "zono occluden", "Plasmin",
                         
                         "A11", 2, 3, 1, "claudin 5", "High LiCl + water",
                         "B11", 2, 3, 1, "claudin 5", "Low LiCl + water",
                         "C11", 2, 3, 1, "claudin 5", "water",
                         "D11", 2, 3, 1, "claudin 5", "High LiCl + plasmin",
                         "E11", 2, 3, 1, "claudin 5", "Low LiCl + plasmin",
                         "F11", 2, 3, 1, "claudin 5", "Plasmin",
                         
                         "A12", 2, 3, 4, "pecam", "High LiCl + water",
                         "B12", 2, 3, 4, "pecam", "Low LiCl + water",
                         "C12", 2, 3, 4, "pecam", "water",
                         "D12", 2, 3, 4, "pecam", "High LiCl + plasmin",
                         "E12", 2, 3, 4, "pecam", "Low LiCl + plasmin",
                         "F12", 2, 3, 4, "pecam", "Plasmin",
                         
                         "A08", 2, 3, 4, "VE-Cadherin", "Nacl in water",
                         "B08", 2, 3, 4, "VE-Cadherin", "Nacl in plasmin",
                         "C08", 2, 3, 4, "b-catenin", "Nacl in water",
                         "D08", 2, 3, 4, "b-catenin", "Nacl in plasmin",
                         "E08", 2, 3, 4, "VE-Cadherin", "Kcl in water",
                         "F08", 2, 3, 4, "VE-Cadherin", "Kcl in plasmin",
                         "G08", 2, 3, 4, "b-catenin", "Kcl in water",
                         "H08", 2, 3, 4, "b-catenin", "Kcl in plasmin",
                         "G09", 2, 3, 4, "VE-Cadherin", "Cacl in water",
                         "G10", 2, 3, 4, "VE-Cadherin", "Cacl in plasmin",
                         "G11", 2, 3, 4, "b-catenin", "Cacl in water",
                         "G12", 2, 3, 4, "b-catenin", "Cacl in plasmin",
                         "H09", 2, 3, 4, "VE-Cadherin", "Li ace in water",
                         "H10", 2, 3, 4, "VE-Cadherin", "Li ace in plasmin",
                         "H11", 2, 3, 4, "b-catenin", "Li ace in water",
                         "H12", 2, 3, 4, "b-catenin", "Li ace in plasmin",)


#import operetta takes two arguments - file directory of the images I want, 
# combining with my lookup tribble


# importing data ----------------------------------------------------------
Exp2_T4 = import_operetta("Cynthia[12001]\\T4_2_9[28151]\\2024-09-02T163134+1200[32251]\\2024-09-02T163134+1200[32251]", 
                          Exp2_wellplate)
Exp2_T4$experiment = "Exp2"
Exp2_T4$timepoint = "T4"

Exp2_T12 = import_operetta("Cynthia[12001]\\T12_2_9[28152]\\2024-09-02T155017+1200[32252]\\2024-09-02T155017+1200[32252]",
                           Exp2_wellplate)
Exp2_T12$experiment= "Exp2"
Exp2_T12$timepoint= "T12"

Exp2_T4_and_T12<- rbind(Exp2_T4, Exp2_T12)


# Exp3 separate and combined times
Exp3_T4 = import_operetta("Cynthia[12001]\\T4_11_9[28354]\\2024-09-11T135358+1200[32454]\\2024-09-11T135358+1200[32454]", 
                          Exp3_wellplate)
Exp3_T4$experiment = "Exp3"
Exp3_T4$timepoint= "T4"


Exp3_T12 = import_operetta("Cynthia[12001]\\T12_11_9[28352]\\2024-09-11T130008+1200[32452]\\2024-09-11T130008+1200[32452]", 
                           Exp3_wellplate)
Exp3_T12$experiment = "Exp3"
Exp3_T12$timepoint= "T12"

Exp3_T4_and_T12 <- rbind(Exp3_T4, Exp3_T12)

# Exp 4
Exp4_T4 = import_operetta("Cynthia[12001]\\T4_21_9[28553]\\2024-09-21T153009+1200[32653]\\2024-09-21T153009+1200[32653]",
                          Exp4_wellplate)
Exp4_T4$experiment = "Exp4"
Exp4_T4$timepoint= "T4"

Exp4_T12 = import_operetta("Cynthia[12001]\\T12_21_9[28552]\\2024-09-21T162642+1200[32652]\\2024-09-21T162642+1200[32652]",
                          Exp4_wellplate)
Exp4_T12$experiment = "Exp4"
Exp4_T12$timepoint= "T12"

Exp4_T4_and_T12<- rbind(Exp4_T4, Exp4_T12)

# All_data_Exp2_3 <- rbind(Exp2_T4_and_T12, Exp3_T4_and_T12)


# My functions ------------------------------------------------------------

normalise_to_water <- function(dataset){
  water_means<- dataset %>% group_by(timepoint) %>% filter(sample =="water") %>% 
    summarise(
      mean_cont_area = mean(contiguous_area),
      mean_cont_fluoro = mean(contiguous_fluorescence))
  
  water_T4 <- filter(water_means, timepoint=="T4")
  water_T12 <- filter(water_means, timepoint=="T12")
  
  T4_normalised <- dataset %>%  filter(timepoint =="T4" ) %>% 
    mutate(norm_cont_area = contiguous_area/water_T4$mean_cont_area,
           norm_cont_fluoro = contiguous_fluorescence/water_T4$mean_cont_fluoro)
  
  T12_normalised <- dataset %>% 
    filter(timepoint =="T12" ) %>% 
    mutate(norm_cont_area = contiguous_area/water_T12$mean_cont_area,
           norm_cont_fluoro = contiguous_fluorescence/water_T12$mean_cont_fluoro) 
  
  Combined_norm_data <- rbind(T4_normalised, T12_normalised) 
  return(Combined_norm_data)
}

# second function - gets means and sd
summarise_norm_data<- function(normalised_points){
  normalised_points %>% 
    group_by(sample, timepoint) %>% 
    summarise(
      mean_cont_area = mean(norm_cont_area),
      sd_cont_area = sd(norm_cont_area),
      se_cont_area = sd_cont_area / sqrt(n()),
      mean_cont_fluoro = mean(norm_cont_fluoro),
      sd_cont_fluoro = sd(norm_cont_fluoro),
      se_cont_fluoro = sd_cont_fluoro / sqrt(n()))
}



                             
# third function - plot a bar graph
plot_bar<- function(all_summarised, y_axis, se_y_variable, all_norm_points, norm_y_variable){
  all_norm_points$experiment<- recode(all_norm_points$experiment,
                                     Exp2 = '1', Exp3 = '2', Exp4='3')
  
  all_summarised$sample<- recode(all_summarised$sample,
                                    water = 'Vehicle', `Low LiCl + water` = 'Low LiCl', 
                                    `High LiCl + water` = 'High LiCl', 
                                    `High LiCl + plasmin` = 'High LiCl + Plasmin',
                                    `Low LiCl + plasmin` ='Low LiCl + Plasmin') %>% as.factor() 
  
  all_summarised$sample<- fct_relevel(all_summarised$sample, "Vehicle", "Plasmin",
                                      "Low LiCl", "Low LiCl + Plasmin","High LiCl",
                                       "High LiCl + Plasmin")
  all_norm_points$sample<- recode(all_norm_points$sample,
                                 water = 'Vehicle', `Low LiCl + water` = 'Low LiCl', 
                                 `High LiCl + water` = 'High LiCl', 
                                 `High LiCl + plasmin` = 'High LiCl + Plasmin',
                                 `Low LiCl + plasmin` ='Low LiCl + Plasmin') %>% as.factor() 
  
  all_norm_points$sample<- fct_relevel(all_norm_points$sample, "Vehicle", "Plasmin",
                                       "Low LiCl", "Low LiCl + Plasmin", "High LiCl",
                                       "High LiCl + Plasmin")
  ggplot()+
  geom_bar(
    data = all_summarised, 
    mapping = aes(x=sample, y=.data[[y_axis]]), 
    stat="identity", fill = "azure3", color="azure4")+facet_wrap(~timepoint)+
  geom_errorbar(
    data = all_summarised, 
    aes(x=sample,
        ymin=.data[[y_axis]]-.data[[se_y_variable]], 
        ymax=.data[[y_axis]]+.data[[se_y_variable]]), 
    width=1, color = "black"
  )+
  geom_point(data=all_norm_points %>% filter(experiment=="1"), 
             mapping = aes(x=sample, y=.data[[norm_y_variable]], color=experiment), 
             size = 0.8, position = position_nudge(x = 0.05), alpha=0.5,
  )+
  geom_point(data = all_norm_points %>% filter(experiment=="2"), 
             mapping = aes(x=sample, y=.data[[norm_y_variable]], color=experiment), 
             size = 0.8, position = position_nudge(x = -0.1), alpha=0.5,
  )+
  geom_point(data = all_norm_points %>% filter(experiment=="3"), 
             mapping = aes(x=sample, y=.data[[norm_y_variable]], color=experiment), 
             size = 0.8, position = position_nudge(x = 0.2), alpha=0.5
  )+
  theme_bw()+
    scale_color_manual(values = c("1" = "blue", "2" = "red", "3" = "darkcyan"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=11)
  )+ ylim(0,1.5) + 
  labs(x="", y= "", color="Replicate number")
}



# VE-cadherin -------------------------------------------------------------


# Exp 2
Exp2_ve_cad = Exp2_T4_and_T12 %>%
  filter(name_antibody == "VE-Cadherin") %>% 
  filter(!str_detect(sample, "\\b in \\b"))

# segment_and_quant_i(Exp2_ve_cad) 

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_Exp2_ve_cad = segment_and_quant_p(
  Exp2_ve_cad, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.003 ,min_area =  10 , nuclear_area =  40 , nuclear_offset =  0.001 
)

#exp 3
Exp3_ve_cad<- Exp3_T4_and_T12 %>%
  filter(name_antibody == "VE-Cadherin") %>% 
  filter(!str_detect(sample, "\\b in \\b"))

quant_Exp3_ve_cad = segment_and_quant_p(
  Exp3_ve_cad, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.0008 , min_area =  12 , nuclear_area =  40 , nuclear_offset =  0.001 
)

# Exp 4
Exp4_ve_cad<- Exp4_T4_and_T12 %>%
  filter(name_antibody == "VE-Cadherin") %>% 
  filter(!str_detect(sample, "\\b in \\b"))

# segment_and_quant_i(Exp4_ve_cad)

quant_Exp4_ve_cad = segment_and_quant_p(Exp4_ve_cad, nuclear_disk =  10 ,nuclear_area =  40 , nuclear_offset =  0.001,tophat_area =  15 , tophat_threshold =  0.001 , min_area =  15 
)

#normalise data to water at that time point
norm_Exp2_ve_cad<- normalise_to_water(quant_Exp2_ve_cad)
norm_Exp3_ve_cad<- normalise_to_water(quant_Exp3_ve_cad)
norm_Exp4_ve_cad<- normalise_to_water(quant_Exp4_ve_cad)

all_VE_norm_points <- rbind(norm_Exp2_ve_cad, norm_Exp3_ve_cad, norm_Exp4_ve_cad)
all_VE_norm_points$timepoint <- factor(all_VE_norm_points$timepoint, levels=c("T4", "T12"))

all_VE_summarised <- summarise_norm_data (all_VE_norm_points) 
all_VE_summarised$timepoint <- factor(all_VE_summarised$timepoint, levels=c("T4", "T12"))

plot_bar(all_VE_summarised, "mean_cont_area", "se_cont_area", all_VE_norm_points, "norm_cont_area")

plot_bar(all_VE_summarised, "mean_cont_fluoro", "se_cont_fluoro", all_VE_norm_points, "norm_cont_fluoro")

view(all_VE_summarised)


# b-cat -------------------------------------------------------------------
Exp2_bcat = Exp2_T4_and_T12 %>% # Exp 2
  filter(name_antibody == "b-catenin") %>% 
  filter(!str_detect(sample, "\\b in \\b"))

quant_Exp2_bcat = segment_and_quant_p(
  Exp2_bcat, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area = 5 , 
  tophat_threshold =  0.002 , 
  min_area =  10  
)

Exp3_bcat<- Exp3_T4_and_T12 %>% #exp 3
  filter(name_antibody == "b-catenin") %>% 
  filter(!str_detect(sample, "\\b in \\b"))

quant_Exp3_bcat = segment_and_quant_p(
  Exp3_bcat, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  8 , 
  tophat_threshold =  0.003 , 
  min_area =  10 
)

# Exp 4
Exp4_bcat<- Exp4_T4_and_T12 %>%
  filter(name_antibody == "b-catenin") %>% 
  filter(!str_detect(sample, "\\b in \\b"))

#segment_and_quant_i(Exp4_bcat)

quant_Exp4_bcat = segment_and_quant_p(
  Exp4_bcat, 
  nuclear_disk =  10 ,
  nuclear_area =  40 , 
  nuclear_offset =  0.001,
  tophat_area =  10 , 
  tophat_threshold =  0.003 , 
  min_area =  10 
)

#normalise data to water at that time point
norm_Exp2_bcat<- normalise_to_water(quant_Exp2_bcat)
norm_Exp3_bcat<- normalise_to_water(quant_Exp3_bcat)
norm_Exp4_bcat<- normalise_to_water(quant_Exp4_bcat)

all_bcat_norm_points <- rbind(norm_Exp2_bcat, norm_Exp3_bcat, norm_Exp4_bcat)
all_bcat_norm_points$timepoint <- factor(all_bcat_norm_points$timepoint, levels=c("T4", "T12"))

all_bcat_summarised <- summarise_norm_data (all_bcat_norm_points) 
all_bcat_summarised$timepoint <- factor(all_bcat_summarised$timepoint, levels=c("T4", "T12"))

plot_bar(all_bcat_summarised, "mean_cont_area", "se_cont_area", all_bcat_norm_points, "norm_cont_area")
plot_bar(all_bcat_summarised, "mean_cont_fluoro", "se_cont_fluoro", all_bcat_norm_points, "norm_cont_fluoro")

# nuclear etc

bcat_nuclear_normalise <- function(dataset){
  water_means<- dataset %>% group_by(timepoint) %>% filter(sample =="water") %>% 
    summarise(
      mean_nuclear_fluorescence = mean(nuclear_fluorescence),
      mean_overall_stain = mean(overall_stain))
  
  water_T4 <- filter(water_means, timepoint=="T4")
  water_T12 <- filter(water_means, timepoint=="T12")
  
  T4_normalised <- dataset %>%  filter(timepoint =="T4" ) %>% 
    mutate(norm_nuclear_stain = nuclear_fluorescence/water_T4$mean_nuclear_fluorescence,
           norm_overall_stain = overall_stain/water_T4$mean_overall_stain)
  
  T12_normalised <- dataset %>% 
    filter(timepoint =="T12" ) %>% 
    mutate(norm_nuclear_stain = nuclear_fluorescence/water_T12$mean_nuclear_fluorescence,
           norm_overall_stain = overall_stain/water_T12$mean_overall_stain)
  
  Combined_norm_data <- rbind(T4_normalised, T12_normalised) 
  return(Combined_norm_data)
}

norm_Exp2_bcat_nuc<- bcat_nuclear_normalise(quant_Exp2_bcat)
norm_Exp3_bcat_nuc<- bcat_nuclear_normalise(quant_Exp3_bcat)
norm_Exp4_bcat_nuc<- bcat_nuclear_normalise(quant_Exp4_bcat)

all_bcat_norm_points_nuc <- rbind(norm_Exp2_bcat_nuc, norm_Exp3_bcat_nuc, norm_Exp4_bcat_nuc)
all_bcat_norm_points_nuc$timepoint <- factor(all_bcat_norm_points$timepoint, levels=c("T4", "T12"))

all_bcat_summarised_nuc <- all_bcat_norm_points_nuc %>% 
  group_by(sample, timepoint) %>% 
  summarise(
    mean_nuclear = mean(norm_nuclear_stain),
    sd_nuclear = sd(norm_nuclear_stain),
    se_nuclear = sd_nuclear / sqrt(n()),
    mean_overall = mean(norm_overall_stain),
    sd_overall = sd(norm_overall_stain),
    se_overall = sd_overall/ sqrt(n()))
all_bcat_summarised$timepoint <- factor(all_bcat_summarised$timepoint, levels=c("T4", "T12"))

# Nuclear b-cat
plot_bar(all_bcat_summarised_nuc, "mean_nuclear", "se_nuclear", all_bcat_norm_points_nuc, "norm_nuclear_stain")
# overall fluorescence
plot_bar(all_bcat_summarised_nuc, "mean_overall", "se_overall", all_bcat_norm_points_nuc, "norm_overall_stain")

view(all_bcat_summarised_nuc)

# zono --------------------------------------------------------------------

Exp2_zono = Exp2_T4_and_T12 %>%
  filter(name_antibody == "zono occluden") 

#exp 2
quant_Exp2_zono = segment_and_quant_p(
  Exp2_zono, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  10 , 
  tophat_threshold =  0.002 , 
  min_area =  8  
)

Exp3_zono = Exp3_T4_and_T12 %>%
  filter(name_antibody == "zono occluden") 

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_Exp3_zono = segment_and_quant_p(
  Exp3_zono, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  8 , 
  tophat_threshold =  0.002 , 
  min_area =  5  
)

Exp4_zono = Exp4_T4_and_T12 %>%
  filter(name_antibody == "zono occluden") 

#segment_and_quant_i(Exp4_zono) #opens shiny

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_Exp4_zono = segment_and_quant_p(
  Exp4_zono, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  12 , 
  tophat_threshold =  0.003 , 
  min_area =  8  
)

norm_Exp2_zono<- normalise_to_water(quant_Exp2_zono)
norm_Exp3_zono<- normalise_to_water(quant_Exp3_zono)
norm_Exp4_zono<- normalise_to_water(quant_Exp4_zono)

all_zono_norm_points <- rbind(norm_Exp2_zono, norm_Exp3_zono, norm_Exp4_zono)
all_zono_norm_points$timepoint <- factor(all_zono_norm_points$timepoint, levels=c("T4", "T12"))

all_zono_summarised <- summarise_norm_data (all_zono_norm_points) 
all_zono_summarised$timepoint <- factor(all_zono_summarised$timepoint, levels=c("T4", "T12"))

plot_bar(all_zono_summarised, "mean_cont_area", "se_cont_area", all_zono_norm_points, "norm_cont_area")
plot_bar(all_zono_summarised, "mean_cont_fluoro", "se_cont_fluoro", all_zono_norm_points, "norm_cont_fluoro")

plot_bar_adjusted(all_zono_summarised, "mean_cont_area", "se_cont_area", all_zono_norm_points, "norm_cont_area")
plot_bar_adjusted(all_zono_summarised, "mean_cont_fluoro", "se_cont_fluoro", all_zono_norm_points, "norm_cont_fluoro") +ylim(0, 1.7)



plot_bar(all_zono_summarised, "mean_cont_fluoro", "se_cont_fluoro", all_zono_norm_points, "norm_cont_fluoro") +ylim(0, 1.75)


# claudin -----------------------------------------------------------------

Exp2_claudin = Exp2_T4_and_T12 %>%
  filter(name_antibody == "claudin 5") 

quant_Exp2_claudin = segment_and_quant_p(
  Exp2_claudin, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area = 8 , 
  tophat_threshold =  0.001 , 
  min_area =  20  
)

Exp3_claudin = Exp3_T4_and_T12 %>%
  filter(name_antibody == "claudin 5") 

quant_Exp3_claudin = segment_and_quant_p(
  Exp3_claudin, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  8 , 
  tophat_threshold =  0.002 , 
  min_area =  20 
)

Exp4_claudin = Exp4_T4_and_T12 %>%
  filter(name_antibody == "claudin 5") 

quant_Exp4_claudin = segment_and_quant_p(
  Exp4_claudin, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  10 , 
  tophat_threshold =  0.001 , 
  min_area =  20 
)

norm_Exp2_claudin<- normalise_to_water(quant_Exp2_claudin)
norm_Exp3_claudin<- normalise_to_water(quant_Exp3_claudin)
norm_Exp4_claudin<- normalise_to_water(quant_Exp4_claudin)

all_claudin_norm_points <- rbind(norm_Exp2_claudin, norm_Exp3_claudin, norm_Exp4_claudin)
all_claudin_norm_points$timepoint <- factor(all_claudin_norm_points$timepoint, levels=c("T4", "T12"))

all_claudin_summarised <- summarise_norm_data (all_claudin_norm_points) 
all_claudin_summarised$timepoint <- factor(all_claudin_summarised$timepoint, levels=c("T4", "T12"))

plot_bar(all_claudin_summarised, "mean_cont_area", "se_cont_area", all_claudin_norm_points, "norm_cont_area")
plot_bar(all_claudin_summarised, "mean_cont_fluoro", "se_cont_fluoro", all_claudin_norm_points, "norm_cont_fluoro")+ylim(0,1.75)


# pecam -------------------------------------------------------------------

Exp2_pecam = Exp2_T4_and_T12 %>%
  filter(name_antibody == "pecam") 

quant_Exp2_pecam = segment_and_quant_p(
  Exp2_pecam, nuclear_disk =  10 , nuclear_area =  40 , nuclear_offset =  0.001, tophat_area = 10 , tophat_threshold =  0.002 , min_area =  20  
)

Exp3_pecam = Exp3_T4_and_T12 %>%
  filter(name_antibody == "pecam") 

quant_Exp3_pecam = segment_and_quant_p(
  Exp3_pecam, nuclear_disk =  10 , nuclear_area =  40 , nuclear_offset =  0.001, tophat_area =  8 , tophat_threshold =  0.002 , min_area =  10 
)

Exp4_pecam = Exp4_T4_and_T12 %>%
  filter(name_antibody == "pecam") 

#segment_and_quant_i(Exp4_pecam)

quant_Exp4_pecam = segment_and_quant_p(
  Exp4_pecam, nuclear_disk =  10 ,  nuclear_area =  40 , nuclear_offset =  0.001, tophat_area =  5 , tophat_threshold =  0.002 , min_area =  15 
)

norm_Exp2_pecam<- normalise_to_water(quant_Exp2_pecam)
norm_Exp3_pecam<- normalise_to_water(quant_Exp3_pecam)
norm_Exp4_pecam<- normalise_to_water(quant_Exp4_pecam)

all_pecam_norm_points <- rbind(norm_Exp2_pecam, norm_Exp3_pecam, norm_Exp4_pecam)
all_pecam_norm_points$timepoint <- factor(all_pecam_norm_points$timepoint, levels=c("T4", "T12"))

all_pecam_summarised <- summarise_norm_data (all_pecam_norm_points) 
all_pecam_summarised$timepoint <- factor(all_pecam_summarised$timepoint, levels=c("T4", "T12"))

plot_bar_adjusted_lots(all_pecam_summarised, "mean_cont_area", "se_cont_area", all_pecam_norm_points, "norm_cont_area")
plot_bar(all_pecam_summarised, "mean_cont_fluoro", "se_cont_fluoro", all_pecam_norm_points, "norm_cont_fluoro")+ylim(0,1.8)



