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


setwd("E:/Cynthia")

#functions in other script
source("E:\\Cynthia\\vjunctur_functions_v1.R")

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
                         "H02", 2, 3, 4, "VE-Cadherin", "Cacl in water",
                         "G04", 2, 3, 4, "b-catenin", "Cacl in water",
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
all_salts<- rbind(Exp2_T4_and_T12, Exp3_T4_and_T12, Exp4_T4_and_T12) %>%  
  filter(grepl(" in |High LiCl", sample) | sample == "water") %>% 
  filter(grepl('VE-Cadherin|b-catenin', name_antibody))
  

High_LiCl<- all_salts %>% filter(sample=="High LiCl + water")

# quant ----------------------------------------------------------------

salts_VE_water = all_salts %>% 
  filter(name_antibody == "VE-Cadherin") %>% 
  filter(grepl(" in water", sample) | sample == "water")


segment_and_quant_i(salts_VE_water) #opens shiny

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_salts_VE <- segment_and_quant_p(
  salts_VE_water, 
  nuclear_disk =  10 , 
  tophat_area =  5 , 
  tophat_threshold =  0.001 , 
  min_area =  15 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001 
)

quant_salts_VE$sample<- as.factor(quant_salts_VE$sample)
quant_salts_VE$experiment<- as.factor(quant_salts_VE$experiment)

quant_salts_VE <- quant_salts_VE  %>%
  mutate(sample =recode(sample, 'CaCl in water'= 'Cacl in water'))



# just water --------------------------------------------------------------


#normalised
norm_points_salts_in_water_ve<- quant_salts_VE %>% filter(sample != "Li ace in water") %>% 
  normalise_to_water()

#summarise
summary_normsalts_VE_water<- summarise_norm_data(norm_points_salts_in_water_ve)

summary_normsalts_VE_water$timepoint <- as.factor(summary_normsalts_VE_water$timepoint)
summary_normsalts_VE_water$timepoint <- relevel(summary_normsalts_VE_water$timepoint, ref = "T4")
summary_normsalts_VE_water$sample <- relevel(summary_normsalts_VE_water$sample, ref = "water")

#plot of VE in salts compared to water, normalised to water, for junctional area
ggplot(summary_normsalts_VE_water, aes(x=sample, y=mean_cont_area))+
  geom_bar(stat = "identity", position = "dodge", fill = "azure3", color="azure4")+
  geom_errorbar(
    data = summary_normsalts_VE_water, 
    aes(x=sample,
        ymin=mean_cont_area-se_cont_area, 
        ymax=mean_cont_area+se_cont_area), 
    width=1, color = "black" ) + facet_wrap(~timepoint)+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ylim(0,1.5) 

#normalised VE salts in water for junctional fluoro
ggplot(summary_normsalts_VE_water, aes(x=sample, y=mean_cont_fluoro))+
  geom_bar(stat = "identity", position = "dodge", fill = "azure3", color="azure4")+
  geom_errorbar(
    data = summary_normsalts_VE_water, 
    aes(x=sample,
        ymin=mean_cont_fluoro-se_cont_fluoro, 
        ymax=mean_cont_fluoro+se_cont_fluoro), 
    width=1, color = "black" ) + facet_wrap(~timepoint)+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ylim(0,1.5)
  
  
  geom_point(data=norm_points_salts_in_water_ve %>% filter(experiment=="1"), 
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 0.8, position = position_nudge(x = 0.05),
  )+
  geom_point(data = norm_points_salts_in_water_ve %>% filter(experiment=="2"), 
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 0.8, position = position_nudge(x = -0.1),
  )+
  geom_point(data = norm_points_salts_in_water_ve %>% filter(experiment=="3"), 
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 0.8, position = position_nudge(x = 0.2),
  )+
  theme_bw()+
  scale_color_manual(values = c("1" = "blue", "2" = "red", "3" = "darkcyan"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=11)
  )+ 
  labs(x="", y= "", color="Replicate number")

# not normalised
summarise_notnorm_ve_salts_water<- quant_salts_VE %>% filter(sample != "Li ace in water") %>% group_by(sample, timepoint) %>% 
  summarise(
    mean_cont_area = mean(contiguous_area),
    sd_cont_area = sd(contiguous_area),
    se_cont_area = sd_cont_area / sqrt(n()),
    mean_cont_fluoro = mean(contiguous_fluorescence),
    sd_cont_fluoro = sd(contiguous_fluorescence),
    se_cont_fluoro = sd_cont_fluoro / sqrt(n()))

# not normalised graph
ggplot(summarise_notnorm_ve_salts_water, aes(x=sample, y=mean_cont_area))+
  geom_bar(stat = "identity", position = "dodge", fill = "azure3", color="azure4")+
  geom_errorbar(
    data = summarise_notnorm_ve_salts_water, 
    aes(x=sample,
        ymin=mean_cont_area-se_cont_area, 
        ymax=mean_cont_area+se_cont_area), 
    width=1, color = "black" ) + facet_wrap(~timepoint)+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )



#model

salts_model<- lmer(contiguous_area ~ sample+ timepoint +(1|experiment), quant_salts_VE)
library(emmeans)

em_ve<- emmeans(salts_model, specs = "sample")
pairs(em_ve, adjust = "tukey")






# below is not used ------------------------------------------

salts_VE_water = all_salts %>% 
  filter(name_antibody == "VE-Cadherin") %>% 
  filter(grepl(" in water", sample) | sample == "water")

ve_data<- rbind(salts_VE_water, High_LiCl)
# exp 2
Exp2_salt_VE_water<- ve_data %>% filter(experiment=="Exp2")
quant_salt_Exp2_ve_water = segment_and_quant_p(
  Exp2_salt_VE_water, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.003 ,min_area =  10 , nuclear_area =  40 , nuclear_offset =  0.001 
)

#exp 3
Exp3_salt_VE_water<- ve_data %>% filter(experiment=="Exp3")
quant_salt_Exp3_ve_water = segment_and_quant_p(
  Exp3_salt_VE_water, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.0008 , min_area =  12 , nuclear_area =  40 , nuclear_offset =  0.001 
)

# Exp 4
Exp4_salt_VE_water<- ve_data %>% filter(experiment=="Exp4")
quant_salt_Exp4_ve_water = segment_and_quant_p(
  Exp4_salt_VE_water, nuclear_disk =  10 ,nuclear_area =  40 , nuclear_offset =  0.001,tophat_area =  15 , tophat_threshold =  0.001 , min_area =  15 
)

all_quant_salts<- rbind(quant_salt_Exp2_ve_water, quant_salt_Exp3_ve_water, quant_salt_Exp4_ve_water)
ggplot(all_quant_salts %>% filter(sample != "Li ace in water"), aes(x=sample, y=contiguous_area))+
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~timepoint)+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )

# plasmin VE-----------------------------
all_salts<- rbind(Exp2_T4_and_T12, Exp3_T4_and_T12, Exp4_T4_and_T12) %>%  
  filter(grepl(" in |High LiCl", sample) | sample == "Plasmin") %>% 
  filter(grepl('VE-Cadherin|b-catenin', name_antibody))


High_LiCl_pl<- all_salts %>% filter(sample=="High LiCl + plasmin")

# quant ----------------------------------------------------------------

salts_VE_pl = all_salts %>% 
  filter(name_antibody == "VE-Cadherin") %>% 
  filter(grepl(" in plasmin", sample) | sample == "Plasmin")

ve_data_pl<- rbind(salts_VE_pl, High_LiCl_pl)


#exp 3
Exp3_salt_VE_pl<- ve_data_pl %>% filter(experiment=="Exp3")
quant_salt_Exp3_ve_pl = segment_and_quant_p(
  Exp3_salt_VE_pl, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.0008 , min_area =  12 , nuclear_area =  40 , nuclear_offset =  0.001 
)

# Exp 4
Exp4_salt_VE_pl<- ve_data_pl %>% filter(experiment=="Exp4")
quant_salt_Exp4_ve_pl = segment_and_quant_p(
  Exp4_salt_VE_pl, nuclear_disk =  10 ,nuclear_area =  40 , nuclear_offset =  0.001,tophat_area =  15 , tophat_threshold =  0.001 , min_area =  15 
)

all_quant_salts_pl<- rbind(quant_salt_Exp3_ve_pl, quant_salt_Exp4_ve_pl)
ggplot(all_quant_salts_pl, aes(x=sample, y=contiguous_area))+
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~timepoint)+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )


library(lme4)
model_salts_plasmin<- lmer(contiguous_area ~ sample+ timepoint +(1|experiment), all_quant_salts_pl)
library(emmeans)

em_ve<- emmeans(model_salts_plasmin, specs = "sample")
pairs(em_ve, adjust = "tukey")

model_salts_plasmin<- lmer(contiguous_fluorescence ~ sample+ timepoint +(1|experiment), all_quant_salts_pl)
em_ve_fluoro<- emmeans(model_salts_plasmin, specs = "sample")
pairs(em_ve_fluoro, adjust = "tukey")



# bcat --------------------------------------------------------------------

salts_bcat_water = all_salts %>% 
  filter(name_antibody == "b-catenin") %>% 
  filter(grepl(" in water", sample) | sample == "water")


segment_and_quant_i(salts_bcat_water) #opens shiny

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_salts_bcat <- segment_and_quant_p(
  salts_bcat_water, 
  nuclear_disk =  10 , 
  tophat_area =  10 , 
  tophat_threshold =  0.001 , 
  min_area =  15 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.002 
)

quant_salts_bcat$sample<- as.factor(quant_salts_bcat$sample)
quant_salts_bcat$experiment<- as.factor(quant_salts_bcat$experiment)

quant_salts_bcat <- quant_salts_bcat  %>%
  mutate(sample =recode(sample, 'CaCl in water'= 'Cacl in water'))

unique(quant_salts_bcat$sample)

norm_bcat_saltwater<- quant_salts_bcat %>% normalise_to_water()

sum_bcat_saltwater<- summarise_norm_data(norm_bcat_saltwater)

ggplot(sum_bcat_saltwater, aes(x=sample, y=mean_cont_area))+
  geom_bar(stat = "identity", position = "dodge", fill = "azure3", color="azure4")+
  geom_errorbar(
    data = sum_bcat_saltwater, 
    aes(x=sample,
        ymin=mean_cont_area-se_cont_area, 
        ymax=mean_cont_area+se_cont_area), 
    width=1, color = "black" ) + facet_wrap(~timepoint)+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ylim(0,1.5) +geom_point(norm_bcat_saltwater, aes(x=sample, y=norm_cont_area))

ggplot(norm_bcat_saltwater, aes(x=sample, y=norm_cont_area, color=experiment))+geom_point()
