
# loading packages --------------------------------------------------------
library(vascr)
library(tidyverse)
library(ggplot2)

# install.packages("remotes")
# remotes::install_github("JamesHucklesby/vascr")


# loading in data ---------------------------------------------------------
exp1<- vascr_import("ECIS", #importing raw data and modeled data
                    raw="ECIS_250310_MFT_1_CG_exp1.abp",
                    model="ECIS_250310_MFT_1_CG_exp1_RbA.csv", experiment="EXP1")
exp1_key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, #add treatment to dataframe by creating a map,
                   1, "B C D", "1", "640 nM Plasmin + 10mM LiCl",
                   2, "B C D", "2", "320 nM Plasmin + 10mM LiCl",
                   3, "B C D", "3", "160 nM Plasmin + 10mM LiCl",
                   
                   4, "B C D", "4", "640 nM Plasmin + 1mM LiCl",
                   5, "B C D", "5", "320 nM Plasmin + 1mM LiCl",
                   6, "B C D", "6", "160 nM Plasmin + 1mM LiCl",
                   
                   7, "B C D", "7", "640 nM Plasmin",
                   8, "B C D", "8", "320 nM Plasmin",
                   9, "B C D", "9", "160 nM Plasmin",
                   
                   10, "B C D", "10", "10mM LiCl", 
                   11, "B C D", "11", "1mM LiCl",
                   
                   12, "B C D", "12", "vehicle")
exp1_labeled<- vascr:::vascr_apply_map(exp1, exp1_key) #then combining map and data using vascr_apply_map

#rep2 
exp2<- vascr_import("ECIS", #importing raw data and modeled data
                    raw="ECIS_250317_MFT_1_CG_exp2_li.abp",
                    model="ECIS_250317_MFT_1_CG_exp2_li_RbA.csv", experiment="EXP2")
exp2_key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, #add treatment to dataframe by creating a map,
                   1, "E F G", "9", "640 nM Plasmin + 10mM LiCl",
                   2, "E F G", "8", "320 nM Plasmin + 10mM LiCl",
                   3, "E F G", "7", "160 nM Plasmin + 10mM LiCl",
                   
                   4, "E F G", "6", "640 nM Plasmin + 1mM LiCl",
                   5, "E F G", "5", "320 nM Plasmin + 1mM LiCl",
                   6, "E F G", "4", "160 nM Plasmin + 1mM LiCl",
                   
                   7, "E F G", "3", "640 nM Plasmin",
                   8, "E F G", "2", "320 nM Plasmin",
                   9, "E F G", "1", "160 nM Plasmin",
                   
                   10, "E F G", "11", "10mM LiCl", 
                   11, "E F G", "12", "1mM LiCl",
                   
                   12, "H", "10 11 12", "vehicle")
exp2_labeled<- vascr:::vascr_apply_map(exp2, exp2_key) #then combining map and data using vascr_apply_map

#rep3 
exp3<- vascr_import("ECIS", #importing raw data and modeled data
                    raw="ECIS_250324_MFT_1_CG_exp3_li.abp",
                    model="ECIS_250324_MFT_1_CG_exp3_li_RbA_REAL.csv", experiment="EXP3")
exp3_key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, #add treatment to dataframe by creating a map,
                   1, "B C D", "3", "640 nM Plasmin + 10mM LiCl",
                   2, "B C D", "4", "320 nM Plasmin + 10mM LiCl",
                   3, "B C D", "5", "160 nM Plasmin + 10mM LiCl",
                   
                   4, "B C D", "6", "640 nM Plasmin + 1mM LiCl",
                   5, "A B", "7", "320 nM Plasmin + 1mM LiCl",
                   5, "A", "6", "320 nM Plasmin + 1mM LiCl",
                   6, "B C D", "8", "160 nM Plasmin + 1mM LiCl",
                   
                   7, "A C D", "9", "640 nM Plasmin",
                   8, "A B D", "10", "320 nM Plasmin",
                   9, "B C D", "11", "160 nM Plasmin",
                   
                   10, "B C D", "1", "10mM LiCl", 
                   11, "A D", "2", "1mM LiCl",
                   11, "A", "3", "1mM LiCl",
                   
                   12, "A B C D", "12", "vehicle")
#well map was a bit of a mess in this rep due to selecting for best wells. 
exp3_labeled<- vascr:::vascr_apply_map(exp3, exp3_key) 

#exp4 
exp4<- vascr_import("ECIS", #importing raw data and modeled data
                    raw="ECIS_250401_MFT_1_CG_exp4_li.abp",
                    model="ECIS_250401_MFT_1_CG_exp4_li_RbA_try2.csv", experiment="EXP4")
exp4_key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, #add treatment to dataframe by creating a map,
                   1, "F G H", "12", "640 nM Plasmin + 10mM LiCl",
                   2, "F G H", "11", "320 nM Plasmin + 10mM LiCl",
                   3, "F G H", "10", "160 nM Plasmin + 10mM LiCl",
                   
                   4, "F G H", "9", "640 nM Plasmin + 1mM LiCl",
                   5, "F G H", "8", "320 nM Plasmin + 1mM LiCl",
                   6, "F G H", "7", "160 nM Plasmin + 1mM LiCl",
                   
                   7, "F G H", "6", "640 nM Plasmin",
                   8, "F G H", "5", "320 nM Plasmin",
                   9, "G", "4", "160 nM Plasmin", #removed a well because behaving weirdly, decided outlier
                   9, "E", "5", "160 nM Plasmin",
                   
                   10, "E F G", "2", "10mM LiCl", 
                   11, "E F G", "1", "1mM LiCl",
                   11, "E", "10", "1mM LiCl",
                   
                   12, "F G", "3", "vehicle", #same with a vehicle well, went really high, likely a well seeding issue as had an issue with a whole half row
                  13, "E", "10 11 12", "aprotinin")
exp4_labeled<- vascr:::vascr_apply_map(exp4, exp4_key)

# combine data and data wrangling ------------------------------------------------------------
# separately setting time zero to treatment time - resume time on ECIS software
plot_data_1 = exp1_labeled %>% vascr_zero_time(63.85) 
plot_data_2 = exp2_labeled %>% vascr_zero_time(63.36)
plot_data_3 = exp3_labeled %>% vascr_zero_time(86.86) 
plot_data_4 = exp4_labeled %>% vascr_zero_time(63.86) 

all_data<- vascr_combine(plot_data_1,plot_data_2, plot_data_3, plot_data_4) #combined raw data

#creating master data for plotting - normalised resampled combined raw data
all_plot_data <- all_data %>% 
  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-5,20)) #limiting the range of hours that is shown on graph

# Rb Plots -------------------------------------------------------------------

#creating master data for plotting - normalised resampled combined raw data
all_plot_data <- all_data %>% 
  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-5,20)) #limiting the range of hours that is shown on graph

#all twelve core treatments shown on one graph
all_plot_data %>% 
  vascr_subset(sampleid = c(1:12)) %>%
  vascr_summarise(level = "summary") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() 

#640pl plasmin w Li
all_plot_data %>% # version with rep1
  vascr_subset(sampleid = c(12,7,4,1)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()  +scale_y_continuous(limits = c(0.5,1.25), expand = c(0, 0)) +facet_wrap(~Experiment)

all_plot_data %>% # version excluding iffy rep1
  vascr_subset(experiment=c(2,3,4),sampleid = c(12,7,4,1)) %>%
  vascr_summarise(level = "summary") %>%
  vascr_plot_line()  +scale_y_continuous(limits = c(0.25,1.25), expand = c(0, 0)) +theme_bw()

#320pl
all_plot_data %>% 
  vascr_subset(sampleid = c(12,8,5,2)) %>% vascr_summarise(level = "summary") %>%
  vascr_plot_line() +scale_y_continuous(limits = c(0.25,1.25), expand = c(0, 0))

#160pl
all_plot_data %>% 
  vascr_subset(sampleid = c(12,9,6,3)) %>% vascr_summarise(level = "summary") %>%
  vascr_plot_line()  +scale_y_continuous(limits = c(0.25,1.25), expand = c(0, 0))

#li alone
all_plot_data %>% 
  vascr_subset(sampleid = c(12,11,10)) %>% vascr_summarise(level = "summary") %>%
  vascr_plot_line() +scale_y_continuous(limits = c(0.25,1.25), expand = c(0, 0))


#plasmin alone
all_plot_data %>% 
  vascr_subset(sampleid = c(12,9,8,7)) %>% vascr_summarise(level = "summary") %>%
  vascr_plot_line() +scale_y_continuous(limits = c(0.25,1.25), expand = c(0, 0))




# extra licl from august --------------------------------------------------
## only 640nM data, paper

exp5<- vascr_import("ECIS", #importing raw data and modeled data
                    raw="ECIS_250818_MFT_1_CG_liclextra.abp",
                    model="ECIS_250818_MFT_1_CG_liclextra_RbA.csv", experiment="EXP5")


exp5_key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, #add treatment to dataframe by creating a map,
                   1, "A", "2 3 4", "640 nM Plasmin + 10mM LiCl",
                   
                   4, "B", "2 3 4", "640 nM Plasmin + 1mM LiCl",
                   
                   7, "D", "2 3 4", "640 nM Plasmin",
                   
                   10, "A B C", "5", "10mM LiCl", 
                   11, "C", "3", "1mM LiCl",
                   11, "D", "5", "1mM LiCl",
                   
                   12, "B C D", "1", "vehicle",)
exp5_labeled<- vascr:::vascr_apply_map(exp5, exp5_key)

plot_data_5 = exp5_labeled %>% vascr_zero_time(71.3) 


# KEY DATASET
datawextra<- vascr_combine(plot_data_1,plot_data_2, plot_data_3, plot_data_4, plot_data_5) #combined raw data


# initial go
# plotextra <- datawextra  %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
#  vascr_resample_time(500) %>% 
#  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
#  vascr_subset(time = c(-5,20), unit= c("Rb", "Alpha", "Cm", "R"))
               






#think R is taking lots of frequnecies, but if putting the vascr subset with the rest it cuts out the other units as well. Needs to be done separately. 
resis<- datawextra %>%  
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-5,20), unit= c("R"), frequency = "4000") %>% 
  mutate(Unit= recode(Unit, R = "Overall Resistance at 4000Hz"))

units<- datawextra %>%  
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-5,20), unit= c("Rb", "Alpha", "Cm")) %>% 
  mutate(Unit= recode(Unit, Rb = "Cell-to-Cell Adhesion (Rb)", Cm= "Membrane Capacitance (Cm)", Alpha="Basolateral Adhesion (alpha)"))

plotextra<- vascr_combine(resis, units)

plotextra %>% 
  vascr_subset(time = c(-2,24), sampleid = c(12, 7, 4, 1)) %>% vascr_summarise(level = "summary") %>%
  vascr_plot_line() +scale_y_continuous(limits = c(0.4,1.25), expand = c(0, 0))+
  scale_color_manual(values = c(
    "vehicle" = "#00A9FF", 
    "640 nM Plasmin + 10mM LiCl" = "#0CB702", 
    "640 nM Plasmin" = "#FF0000", 
    "640 nM Plasmin + 1mM LiCl" = "#E68613"  
  ))+
  scale_fill_manual(values = c(
    "vehicle" = "#00B8E7", 
    "640 nM Plasmin + 10mM LiCl" = "#00BE67", 
    "640 nM Plasmin" = "#FF0000",  
    "640 nM Plasmin + 1mM LiCl" = "#CD9600" 
  ))+theme_bw() +
  geom_vline(xintercept=0, colour="azure4", linetype="dashed")+
  facet_wrap(~Unit) + ylab("Fold change")


