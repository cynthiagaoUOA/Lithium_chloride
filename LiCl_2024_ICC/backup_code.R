# install.packages(c("tidyverse", "EBImage", "shiny", "bslib", "data.table", "BiocManager", "gglm"))
#data.table

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


# First we create the list of metadata

# Select the file
# file.choose()

# Create a tribble of information - data entry

# sample = treatment
# channel will be 1 or 4 depending on claudin

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


#import operetta takes two arguments - file directory of the images I want, 
# combining with my lookup tribble


# Exp2 separate and combined times
Exp2_T4 = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T4_2_9[28151]\\2024-09-02T163134+1200[32251]\\2024-09-02T163134+1200[32251]", 
          Exp2_wellplate)
Exp2_T4$experiment = "Exp2 T4"

Exp2_T12 = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T12_2_9[28152]\\2024-09-02T155017+1200[32252]\\2024-09-02T155017+1200[32252]",
                           Exp2_wellplate)
Exp2_T12$experiment= "Exp2 T12"

Exp2_T4_and_T12<- rbind(Exp2_T4, Exp2_T12)


# Exp3 separate and combined times
Exp3_T4 = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T4_11_9[28354]\\2024-09-11T135358+1200[32454]\\2024-09-11T135358+1200[32454]", 
                          Exp3_wellplate)
Exp3_T4$experiment = "Exp3 T4"


Exp3_T12 = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T12_11_9[28352]\\2024-09-11T130008+1200[32452]\\2024-09-11T130008+1200[32452]", 
                           Exp3_wellplate)
Exp3_T12$experiment = "Exp3 T12"

Exp3_T4_and_T12 <- rbind(Exp3_T4, Exp3_T12)

All_data_Exp2_3 <- rbind(Exp2_T4_and_T12, Exp3_T4_and_T12)
All_data_Exp2_3 <- separate(All_data_Exp2_3, 
                       col = experiment, 
                       into = c("experiment", "timepoint"), 
                       sep = " ") 
All_data_Exp2_3$timepoint <- factor (All_data_Exp2_3$timepoint, 
                                              levels=c("T4", "T12"))


# Exp2 VE cadherin -----------------------------------------------------

# setting settings for each stain, now VE
# getting data of all Ve-cadherin rows, ignoring salt controls

Exp2_ve_cad = Exp2_T4_and_T12 %>%
  filter(name_antibody == "VE-Cadherin") %>% 
  filter(!str_detect(sample, "\\b in \\b"))

# segment_and_quant_i(Exp2_ve_cad) #opens shiny

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_Exp2_ve_cad = segment_and_quant_p(
  Exp2_ve_cad, 
  nuclear_disk =  10 , 
  tophat_area =  10 , 
  tophat_threshold =  0.003 , 
  min_area =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001 
  )

Exp3_ve_cad<- Exp3_T4_and_T12 %>%
  filter(name_antibody == "VE-Cadherin") %>% 
  filter(!str_detect(sample, "\\b in \\b"))

segment_and_quant_i(Exp3_ve_cad)

quant_Exp3_ve_cad = segment_and_quant_p(
  Exp3_ve_cad, 
  nuclear_disk =  10 , 
  tophat_area =  10 , 
  tophat_threshold =  0.0008 , 
  min_area =  12 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001 
)

Exp2_3_quant_VE_cad <- rbind(quant_Exp2_ve_cad, quant_Exp3_ve_cad)
Exp2_3_quant_VE_cad <- separate(Exp2_3_quant_VE_cad, 
                            col = experiment, 
                            into = c("experiment", "timepoint"), 
                            sep = " ") 
Exp2_3_quant_VE_cad$timepoint <- factor (Exp2_3_quant_VE_cad$timepoint, 
                                     levels=c("T4", "T12"))

#plots
All_VE_junctionarea <- ggplot(
  Exp2_3_quant_VE_cad, 
  aes(x = sample, y =contiguous_area, color = experiment)
  ) +
  geom_boxplot() +
  theme_bw()+facet_wrap(~timepoint)+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )

All_VE_junctionintensity <- ggplot(
  Exp2_3_quant_VE_cad, 
  aes(x = sample, y =contiguous_fluorescence, color= experiment)
  ) +
  geom_boxplot() +
  theme_bw()+facet_wrap(~timepoint)+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )


# plot together
All_VE_junctionarea + All_VE_junctionintensity + plot_annotation(
  title = 'Exp 2+3 VEcadherin')+ plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect')



# geom text to find strange looking data, confirm if outlier or not

# total fluorescence
All_VE_total_fluoro <- ggplot(
  Exp2_3_quant_VE_cad, 
  aes(x = sample, y =overall_stain, color= experiment)
) +
  geom_boxplot() +
  theme_bw()+facet_wrap(~timepoint)+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )
All_VE_total_fluoro

# VE normalised bar graph ---------------------------------------------------------------------
# starting again, normalising first
water_means <- Exp2_3_quant_VE_cad %>% group_by(timepoint, sample) %>% filter(sample=="water") %>% 
  summarise(
    mean_cont_area = mean(contiguous_area),
    mean_cont_fluoro = mean(contiguous_fluorescence))

water_T4 <- filter(water_means, timepoint=="T4")
water_T12 <- filter(water_means, timepoint=="T12")

T4_norm_quant_VE_cad <- Exp2_3_quant_VE_cad %>% 
  filter(timepoint =="T4" ) %>% 
  mutate(norm_cont_area = contiguous_area/water_T4$mean_cont_area,
         norm_cont_fluoro = contiguous_fluorescence/water_T4$mean_cont_fluoro)


T12_norm_quant_VE_cad <- Exp2_3_quant_VE_cad %>% 
  filter(timepoint =="T12" ) %>% 
  mutate(norm_cont_area = contiguous_area/water_T12$mean_cont_area,
         norm_cont_fluoro = contiguous_fluorescence/water_T12$mean_cont_fluoro) 



All_norm_quant_VE_cad<- rbind(T4_norm_quant_VE_cad, T12_norm_quant_VE_cad) %>% 
  group_by(sample, timepoint) %>% 
summarise(
    mean_cont_area = mean(norm_cont_area),
    sd_cont_area = sd(norm_cont_area),
    mean_cont_fluoro = mean(norm_cont_fluoro),
    sd_cont_fluoro = sd(norm_cont_fluoro)
  )


Exp2_3_VE_norm_points<- rbind(T4_norm_quant_VE_cad, T12_norm_quant_VE_cad) 

# bar graph 
bar_cont_area_VE<- ggplot()+
  geom_bar(
    data = All_norm_quant_VE_cad, 
    mapping = aes(x=sample, y=mean_cont_area), 
    stat="identity", fill = "azure3", color="black")+facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_VE_cad, 
    aes(x=sample, y=mean_cont_area,
        ymin=mean_cont_area-sd_cont_area, 
        ymax=mean_cont_area+sd_cont_area), 
    width=0.5
  )+
  geom_point(data=Exp2_3_VE_norm_points %>% filter(experiment=="Exp2"), 
             mapping = aes(x=sample, y=norm_cont_area, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
             )+
  geom_point(data=Exp2_3_VE_norm_points %>% filter(experiment=="Exp3"), 
             mapping = aes(x=sample, y=norm_cont_area, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
             )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")


bar_cont_area_VE

#fluoro
bar_cont_fluoro_VE<- ggplot()+
  geom_bar(
    data = All_norm_quant_VE_cad, 
    mapping = aes(x=sample, y=mean_cont_fluoro), 
    stat="identity", fill = "azure3", color="black")+ facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_VE_cad, 
    aes(x=sample, y=mean_cont_fluoro,
        ymin=mean_cont_fluoro-sd_cont_fluoro, 
        ymax=mean_cont_fluoro+sd_cont_fluoro),
    width=0.5
  )+
  geom_point(data=Exp2_3_VE_norm_points %>% filter(experiment=="Exp2"),
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
  )+
  geom_point(data=Exp2_3_VE_norm_points %>% filter(experiment=="Exp3"),
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
            )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")
bar_cont_fluoro_VE

#zono occludin -----------------------------------------------------------

Exp2_zono = Exp2_T4_and_T12 %>%
  filter(name_antibody == "zono occluden") 

segment_and_quant_i(Exp2_zono) #opens shiny

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

segment_and_quant_i(Exp3_zono) #opens shiny

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

Exp2_3_quant_zono <- rbind(quant_Exp2_zono, quant_Exp3_zono)
Exp2_3_quant_zono <- separate(Exp2_3_quant_zono, 
                                col = experiment, 
                                into = c("experiment", "timepoint"), 
                                sep = " ") 
Exp2_3_quant_zono$timepoint <- factor (Exp2_3_quant_zono$timepoint, 
                                         levels=c("T4", "T12"))


water_means_zono <- Exp2_3_quant_zono %>% group_by(timepoint, sample) %>% filter(sample=="water") %>% 
  summarise(
    mean_cont_area = mean(contiguous_area),
    mean_cont_fluoro = mean(contiguous_fluorescence))

water_T4_zono <- filter(water_means, timepoint=="T4")
water_T12_zono <- filter(water_means, timepoint=="T12")

T4_norm_quant_zono <- Exp2_3_quant_zono %>% 
  filter(timepoint =="T4" ) %>% 
  mutate(norm_cont_area = contiguous_area/water_T4_zono$mean_cont_area,
         norm_cont_fluoro = contiguous_fluorescence/water_T4_zono$mean_cont_fluoro)


T12_norm_quant_zono <- Exp2_3_quant_zono %>% 
  filter(timepoint =="T12" ) %>% 
  mutate(norm_cont_area = contiguous_area/water_T12_zono$mean_cont_area,
         norm_cont_fluoro = contiguous_fluorescence/water_T12_zono$mean_cont_fluoro) 



All_norm_quant_zono<- rbind(T4_norm_quant_zono, T12_norm_quant_zono) %>% 
  group_by(sample, timepoint) %>% 
  summarise(
    mean_cont_area = mean(norm_cont_area),
    sd_cont_area = sd(norm_cont_area),
    mean_cont_fluoro = mean(norm_cont_fluoro),
    sd_cont_fluoro = sd(norm_cont_fluoro)
  )


Exp2_3_zono_norm_points<- rbind(T4_norm_quant_zono, T12_norm_quant_zono) 

# bar graph 
bar_cont_area_zono<- ggplot()+
  geom_bar(
    data = All_norm_quant_zono, 
    mapping = aes(x=sample, y=mean_cont_area), 
    stat="identity", fill = "azure3", color="black")+facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_zono, 
    aes(x=sample, y=mean_cont_area,
        ymin=mean_cont_area-sd_cont_area, 
        ymax=mean_cont_area+sd_cont_area), 
    width=0.5
  )+
  geom_point(data=Exp2_3_zono_norm_points %>% filter(experiment=="Exp2"), 
             mapping = aes(x=sample, y=norm_cont_area, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
  )+
  geom_point(data=Exp2_3_zono_norm_points %>% filter(experiment=="Exp3"), 
             mapping = aes(x=sample, y=norm_cont_area, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
  )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")


bar_cont_area_zono

#fluoro
bar_cont_fluoro_zono<- ggplot()+
  geom_bar(
    data = All_norm_quant_zono, 
    mapping = aes(x=sample, y=mean_cont_fluoro), 
    stat="identity", fill = "azure3", color="black")+ facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_zono, 
    aes(x=sample, y=mean_cont_fluoro,
        ymin=mean_cont_fluoro-sd_cont_fluoro, 
        ymax=mean_cont_fluoro+sd_cont_fluoro),
    width=0.5
  )+
  geom_point(data=Exp2_3_zono_norm_points %>% filter(experiment=="Exp2"),
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
  )+
  geom_point(data=Exp2_3_zono_norm_points %>% filter(experiment=="Exp3"),
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
  )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")
bar_cont_fluoro_zono

# Claudin -----------------------------------------------------------------

All_claudin = All_data_Exp2_3 %>%
  filter(name_antibody == "claudin 5") 

segment_and_quant_i(All_claudin) #opens shiny

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_all_claudin = segment_and_quant_p(
  All_claudin, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  10 , 
  tophat_threshold =  0.004 , 
  min_area =  15  
)

#plots
All_claudin_junctionarea <- ggplot( quant_all_claudin, 
                                 aes(x = sample, y =contiguous_area, color= `timepoint`))+
  geom_boxplot()+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )+ labs(x="Treatment", y="Contiguous Area")+facet_wrap(~experiment)


All_claudin_junctionintensity <- ggplot(
  quant_all_claudin, 
  aes(x = sample, y =contiguous_fluorescence, color= `timepoint`)
) +
  geom_boxplot() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )+ labs(x="Treatment", y="Contiguous Fluorescence")+facet_wrap(~experiment)


# plot together
All_claudin_junctionarea + All_claudin_junctionintensity + plot_annotation(
  title = 'Exp 2 and 3 Claudin 5')+ plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect')


# Bar ---------------------------------------------------------------------

Exp2_claudin = Exp2_T4_and_T12 %>%
  filter(name_antibody == "claudin 5") 

segment_and_quant_i(Exp2_claudin) #opens shiny

#exp 2
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

segment_and_quant_i(Exp3_claudin) #opens shiny

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_Exp3_claudin = segment_and_quant_p(
  Exp3_claudin, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  8 , 
  tophat_threshold =  0.002 , 
  min_area =  20 
)

Exp2_3_quant_claudin <- rbind(quant_Exp2_claudin, quant_Exp3_claudin)
Exp2_3_quant_claudin <- separate(Exp2_3_quant_claudin, 
                              col = experiment, 
                              into = c("experiment", "timepoint"), 
                              sep = " ") 
Exp2_3_quant_claudin$timepoint <- factor (Exp2_3_quant_claudin$timepoint, 
                                       levels=c("T4", "T12"))

# normalising
water_means_claudin <- Exp2_3_quant_claudin %>% group_by(timepoint, sample) %>% filter(sample=="water") %>% 
  summarise(
    mean_cont_area = mean(contiguous_area),
    mean_cont_fluoro = mean(contiguous_fluorescence))

water_T4_claudin <- filter(water_means_claudin, timepoint=="T4")
water_T12_claudin <- filter(water_means_claudin, timepoint=="T12")

T4_norm_quant_claudin <- Exp2_3_quant_claudin %>% 
  filter(timepoint =="T4" ) %>% 
  mutate(norm_cont_area = contiguous_area/water_T4_claudin$mean_cont_area,
         norm_cont_fluoro = contiguous_fluorescence/water_T4_claudin$mean_cont_fluoro)


T12_norm_quant_claudin <- Exp2_3_quant_claudin %>% 
  filter(timepoint =="T12" ) %>% 
  mutate(norm_cont_area = contiguous_area/water_T12_claudin$mean_cont_area,
         norm_cont_fluoro = contiguous_fluorescence/water_T12_claudin$mean_cont_fluoro) 



All_norm_quant_claudin<- rbind(T4_norm_quant_claudin, T12_norm_quant_claudin) %>% 
  group_by(sample, timepoint) %>% 
  summarise(
    mean_cont_area = mean(norm_cont_area),
    sd_cont_area = sd(norm_cont_area),
    mean_cont_fluoro = mean(norm_cont_fluoro),
    sd_cont_fluoro = sd(norm_cont_fluoro)
  )


Exp2_3_claudin_norm_points<- rbind(T4_norm_quant_claudin, T12_norm_quant_claudin) 

# bar graph 
bar_cont_area_claudin<- ggplot()+
  geom_bar(
    data = All_norm_quant_claudin, 
    mapping = aes(x=sample, y=mean_cont_area), 
    stat="identity", fill = "azure3", color="black")+facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_claudin, 
    aes(x=sample, y=mean_cont_area,
        ymin=mean_cont_area-sd_cont_area, 
        ymax=mean_cont_area+sd_cont_area), 
    width=0.5
  )+
  geom_point(data=Exp2_3_claudin_norm_points %>% filter(experiment=="Exp2"), 
             mapping = aes(x=sample, y=norm_cont_area, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
  )+
  geom_point(data=Exp2_3_claudin_norm_points %>% filter(experiment=="Exp3"), 
             mapping = aes(x=sample, y=norm_cont_area, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
  )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")


bar_cont_area_claudin

#fluoro
bar_cont_fluoro_claudin<- ggplot()+
  geom_bar(
    data = All_norm_quant_claudin, 
    mapping = aes(x=sample, y=mean_cont_fluoro), 
    stat="identity", fill = "azure3", color="black")+ facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_claudin, 
    aes(x=sample, y=mean_cont_fluoro,
        ymin=mean_cont_fluoro-sd_cont_fluoro, 
        ymax=mean_cont_fluoro+sd_cont_fluoro),
    width=0.5
  )+
  geom_point(data=Exp2_3_claudin_norm_points %>% filter(experiment=="Exp2"),
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
  )+
  geom_point(data=Exp2_3_claudin_norm_points %>% filter(experiment=="Exp3"),
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
  )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")

bar_cont_fluoro_claudin

# pecam -------------------------------------------------------------------

All_pecam = All_data_Exp2_3 %>%
  filter(name_antibody == "pecam") 

segment_and_quant_i(All_pecam) #opens shiny

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_all_pecam = segment_and_quant_p(
  All_pecam, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  10 , 
  tophat_threshold =  0.003 , 
  min_area =  15  
)

#plots
All_pecam_junctionarea <- ggplot(quant_all_pecam, 
                                    aes(x = sample, y =contiguous_area, color= `timepoint`))+
  geom_boxplot()+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )+ labs(x="Treatment", y="Contiguous Area")+facet_wrap(~experiment)


All_pecam_junctionintensity <- ggplot(
  quant_all_pecam, 
  aes(x = sample, y =contiguous_fluorescence, color= `timepoint`)
) +
  geom_boxplot() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )+ labs(x="Treatment", y="Contiguous Fluorescence")+facet_wrap(~experiment)


# plot together
All_pecam_junctionarea + All_pecam_junctionintensity + plot_annotation(
  title = 'Exp 2 and 3 PE-CAM')+ plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect')



# Bars, pecam -------------------------------------------------------------

Exp2_pecam = Exp2_T4_and_T12 %>%
  filter(name_antibody == "pecam") 

segment_and_quant_i(Exp2_pecam) #opens shiny

#exp 2
quant_Exp2_pecam = segment_and_quant_p(
  Exp2_pecam, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area = 10 , 
  tophat_threshold =  0.002 , 
  min_area =  20  
)

Exp3_pecam = Exp3_T4_and_T12 %>%
  filter(name_antibody == "pecam") 

segment_and_quant_i(Exp3_pecam) #opens shiny

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_Exp3_pecam = segment_and_quant_p(
  Exp3_pecam, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  8 , 
  tophat_threshold =  0.002 , 
  min_area =  10 
)

Exp2_3_quant_pecam <- rbind(quant_Exp2_pecam, quant_Exp3_pecam)
Exp2_3_quant_pecam <- separate(Exp2_3_quant_pecam, 
                                 col = experiment, 
                                 into = c("experiment", "timepoint"), 
                                 sep = " ") 
Exp2_3_quant_pecam$timepoint <- factor (Exp2_3_quant_pecam$timepoint, 
                                          levels=c("T4", "T12"))

# normalising
water_means_pecam <- Exp2_3_quant_pecam %>% group_by(timepoint, sample) %>% filter(sample=="water") %>% 
  summarise(
    mean_cont_area = mean(contiguous_area),
    mean_cont_fluoro = mean(contiguous_fluorescence))

water_T4_pecam <- filter(water_means_pecam, timepoint=="T4")
water_T12_pecam <- filter(water_means_pecam, timepoint=="T12")

T4_norm_quant_pecam <- Exp2_3_quant_pecam %>% 
  filter(timepoint =="T4" ) %>% 
  mutate(norm_cont_area = contiguous_area/water_T4_pecam$mean_cont_area,
         norm_cont_fluoro = contiguous_fluorescence/water_T4_pecam$mean_cont_fluoro)


T12_norm_quant_pecam <- Exp2_3_quant_pecam %>% 
  filter(timepoint =="T12" ) %>% 
  mutate(norm_cont_area = contiguous_area/water_T12_pecam$mean_cont_area,
         norm_cont_fluoro = contiguous_fluorescence/water_T12_pecam$mean_cont_fluoro) 



All_norm_quant_pecam<- rbind(T4_norm_quant_pecam, T12_norm_quant_pecam) %>% 
  group_by(sample, timepoint) %>% 
  summarise(
    mean_cont_area = mean(norm_cont_area),
    sd_cont_area = sd(norm_cont_area),
    mean_cont_fluoro = mean(norm_cont_fluoro),
    sd_cont_fluoro = sd(norm_cont_fluoro)
  )


Exp2_3_pecam_norm_points<- rbind(T4_norm_quant_pecam, T12_norm_quant_pecam) 

# bar graph 
bar_cont_area_pecam<- ggplot()+
  geom_bar(
    data = All_norm_quant_pecam, 
    mapping = aes(x=sample, y=mean_cont_area), 
    stat="identity", fill = "azure3", color="black")+facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_pecam, 
    aes(x=sample, y=mean_cont_area,
        ymin=mean_cont_area-sd_cont_area, 
        ymax=mean_cont_area+sd_cont_area), 
    width=0.5
  )+
  geom_point(data=Exp2_3_pecam_norm_points %>% filter(experiment=="Exp2"), 
             mapping = aes(x=sample, y=norm_cont_area, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
  )+
  geom_point(data=Exp2_3_pecam_norm_points %>% filter(experiment=="Exp3"), 
             mapping = aes(x=sample, y=norm_cont_area, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
  )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")


bar_cont_area_pecam

#fluoro
bar_cont_fluoro_pecam<- ggplot()+
  geom_bar(
    data = All_norm_quant_pecam, 
    mapping = aes(x=sample, y=mean_cont_fluoro), 
    stat="identity", fill = "azure3", color="black")+ facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_pecam, 
    aes(x=sample, y=mean_cont_fluoro,
        ymin=mean_cont_fluoro-sd_cont_fluoro, 
        ymax=mean_cont_fluoro+sd_cont_fluoro),
    width=0.5
  )+
  geom_point(data=Exp2_3_pecam_norm_points %>% filter(experiment=="Exp2"),
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
  )+
  geom_point(data=Exp2_3_pecam_norm_points %>% filter(experiment=="Exp3"),
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
  )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")

bar_cont_fluoro_pecam

# b-catenin ---------------------------------------------------------------

All_bcat = All_data_Exp2_3 %>%
  filter(name_antibody == "b-catenin") %>% 
  filter(!str_detect(sample, "\\b in \\b"))

segment_and_quant_i(All_bcat) #opens shiny

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_all_bcat = segment_and_quant_p(
  All_bcat, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  10 , 
  tophat_threshold =  0.004 , 
  min_area =  10  
)

#plots
All_bcat_junctionarea <- ggplot(quant_all_bcat, 
                                 aes(x = sample, y =contiguous_area, color= `timepoint`))+
  geom_boxplot()+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )+ labs(x="Treatment", y="Contiguous Area")


All_bcat_junctionintensity <- ggplot(
  quant_all_bcat, 
  aes(x = sample, y =contiguous_fluorescence, color= `timepoint`)
) +
  geom_boxplot() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )+ labs(x="Treatment", y="Contiguous Fluorescence")


# plot together
All_bcat_junctionarea + All_bcat_junctionintensity + plot_annotation(
  title = 'Exp 2 and 3 b-catenin')+ plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect')

All_bcat_junctionarea <- ggplot(quant_all_bcat, 
                                aes(x = sample, y =contiguous_area, color= `timepoint`))+
  geom_boxplot()+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )+ labs(x="Treatment", y="Contiguous Area") +facet_wrap(~experiment)


All_bcat_junctionintensity <- ggplot(
  quant_all_bcat, 
  aes(x = sample, y =contiguous_fluorescence, color= `timepoint`)
) +
  geom_boxplot() +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )+ labs(x="Treatment", y="Contiguous Fluorescence")+facet_wrap(~experiment)


# plot together
All_bcat_junctionarea + All_bcat_junctionintensity + plot_annotation(
  title = 'Exp 2 and 3 b-catenin')+ plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect')

# nuclear fluorescence

ggplot(quant_all_bcat, aes(x=sample, y=nuclear_fluorescence, color=timepoint)
       )+ geom_bar(position = "identity")+ theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5)
  )+ labs(x="Treatment", y="Nuclear Fluorescence")



# try 2, bars -------------------------------------------------------------

Exp2_bcat = Exp2_T4_and_T12 %>%
  filter(name_antibody == "b-catenin")  %>% 
  filter(!str_detect(sample, "\\b in \\b"))

segment_and_quant_i(Exp2_bcat) #opens shiny

#exp 2
quant_Exp2_bcat = segment_and_quant_p(
  Exp2_bcat, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area = 5 , 
  tophat_threshold =  0.002 , 
  min_area =  10  
)

Exp3_bcat = Exp3_T4_and_T12 %>%
  filter(name_antibody == "b-catenin")  %>% 
  filter(!str_detect(sample, "\\b in \\b"))

segment_and_quant_i(Exp3_bcat) #opens shiny

#find good parameters for the stain of choosing, then apply across all the images of that stain
quant_Exp3_bcat = segment_and_quant_p(
  Exp3_bcat, 
  nuclear_disk =  10 , 
  nuclear_area =  40 , 
  nuclear_offset =  0.001, 
  tophat_area =  8 , 
  tophat_threshold =  0.003 , 
  min_area =  10 
)

Exp2_3_quant_bcat <- rbind(quant_Exp2_bcat, quant_Exp3_bcat)
Exp2_3_quant_bcat <- separate(Exp2_3_quant_bcat, 
                               col = experiment, 
                               into = c("experiment", "timepoint"), 
                               sep = " ") 
Exp2_3_quant_bcat$timepoint <- factor (Exp2_3_quant_bcat$timepoint, 
                                        levels=c("T4", "T12"))

# normalising
water_means_bcat <- Exp2_3_quant_bcat %>% group_by(timepoint, sample) %>% filter(sample=="water") %>% 
  summarise(
    mean_cont_area = mean(contiguous_area),
    mean_cont_fluoro = mean(contiguous_fluorescence),
    mean_nuclear = mean(nuclear_fluorescence))

water_T4_bcat <- filter(water_means_bcat, timepoint=="T4")
water_T12_bcat <- filter(water_means_bcat, timepoint=="T12")

T4_norm_quant_bcat <- Exp2_3_quant_bcat %>% 
  filter(timepoint =="T4" ) %>% 
  mutate(norm_cont_area = contiguous_area/water_T4_bcat$mean_cont_area,
         norm_cont_fluoro = contiguous_fluorescence/water_T4_bcat$mean_cont_fluoro,
         norm_nuclear = nuclear_fluorescence/water_T4_bcat$mean_nuclear)


T12_norm_quant_bcat <- Exp2_3_quant_bcat %>% 
  filter(timepoint =="T12" ) %>% 
  mutate(norm_cont_area = contiguous_area/water_T12_bcat$mean_cont_area,
         norm_cont_fluoro = contiguous_fluorescence/water_T12_bcat$mean_cont_fluoro,
         norm_nuclear = nuclear_fluorescence/water_T12_bcat$mean_nuclear) 



All_norm_quant_bcat<- rbind(T4_norm_quant_bcat, T12_norm_quant_bcat) %>% 
  group_by(sample, timepoint) %>% 
  summarise(
    mean_cont_area = mean(norm_cont_area),
    sd_cont_area = sd(norm_cont_area),
    mean_cont_fluoro = mean(norm_cont_fluoro),
    sd_cont_fluoro = sd(norm_cont_fluoro),
    mean_nuclear= mean(norm_nuclear),
    sd_nuclear = sd(norm_nuclear)
  )


Exp2_3_bcat_norm_points<- rbind(T4_norm_quant_bcat, T12_norm_quant_bcat) 

# bar graph 
bar_cont_area_bcat<- ggplot()+
  geom_bar(
    data = All_norm_quant_bcat, 
    mapping = aes(x=sample, y=mean_cont_area), 
    stat="identity", fill = "azure3", color="black")+facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_bcat, 
    aes(x=sample, y=mean_cont_area,
        ymin=mean_cont_area-sd_cont_area, 
        ymax=mean_cont_area+sd_cont_area), 
    width=0.5
  )+
  geom_point(data=Exp2_3_bcat_norm_points %>% filter(experiment=="Exp2"), 
             mapping = aes(x=sample, y=norm_cont_area, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
  )+
  geom_point(data=Exp2_3_bcat_norm_points %>% filter(experiment=="Exp3"), 
             mapping = aes(x=sample, y=norm_cont_area, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
  )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")


bar_cont_area_bcat

#fluoro
bar_cont_fluoro_bcat<- ggplot()+
  geom_bar(
    data = All_norm_quant_bcat, 
    mapping = aes(x=sample, y=mean_cont_fluoro), 
    stat="identity", fill = "azure3", color="black")+ facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_bcat, 
    aes(x=sample, y=mean_cont_fluoro,
        ymin=mean_cont_fluoro-sd_cont_fluoro, 
        ymax=mean_cont_fluoro+sd_cont_fluoro),
    width=0.5
  )+
  geom_point(data=Exp2_3_bcat_norm_points %>% filter(experiment=="Exp2"),
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
  )+
  geom_point(data=Exp2_3_bcat_norm_points %>% filter(experiment=="Exp3"),
             mapping = aes(x=sample, y=norm_cont_fluoro, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
  )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")

bar_cont_fluoro_bcat

bar_nuclear_fluoro_bcat<- ggplot()+
  geom_bar(
    data = All_norm_quant_bcat, 
    mapping = aes(x=sample, y=mean_nuclear), 
    stat="identity", fill = "azure3", color="black")+ facet_wrap(~timepoint)+
  geom_errorbar(
    data = All_norm_quant_bcat, 
    aes(x=sample, y=mean_nuclear,
        ymin=mean_nuclear-sd_nuclear, 
        ymax=mean_nuclear+sd_nuclear),
    width=0.5
  )+
  geom_point(data=Exp2_3_bcat_norm_points %>% filter(experiment=="Exp2"),
             mapping = aes(x=sample, y=norm_nuclear, color=experiment), 
             size = 1, position = position_nudge(x = 0.1)
  )+
  geom_point(data=Exp2_3_bcat_norm_points %>% filter(experiment=="Exp3"),
             mapping = aes(x=sample, y=norm_nuclear, color=experiment), 
             size = 1, position = position_nudge(x = -0.1)
  )+
  theme_bw()+
  scale_color_manual(values = c("Exp2" = "blue", "Exp3" = "red"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+ geom_point()+ ylim(0,1.5) + 
  labs(x="", y= "")
bar_nuclear_fluoro_bcat


# testing code ------------------------------------------------------------

all_VE_summarised <- summarise_norm_data (all_VE_norm_points) 
all_VE_summarised$timepoint <- factor(all_VE_summarised$timepoint, levels=c("T4", "T12"))


all_VE_summarised$sample<- recode(all_VE_summarised$sample,
                                  water = 'Vehicle', `Low LiCl + water` = 'Low LiCl', 
                                  `High LiCl + water` = 'High LiCl', 
                                  `High LiCl + plasmin` = 'High LiCl + Plasmin',
                                  `Low LiCl + plasmin` ='Low LiCl + Plasmin') %>% 
  as.factor() 
all_VE_summarised$sample<- fct_relevel(all_VE_summarised$sample, rev)

