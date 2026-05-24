
# data prep ---------------------------------------------------------------
library(tidyverse)
library(readxl)
rep_1 <- read_excel("rep1.xls") 
rep_2 <- read_excel("rep2redo.xls")
rep_3 <- read_excel("rep3.xls")

#rep 1 25/9
rep1_data<- rep_1 %>% as.data.frame() %>% 
  pivot_longer(cols = -`Cycle(Seconds)/Well`, names_to= "well", values_to= "fluorescence")

rep1_data$well<- as.factor(rep1_data$well)

rep1_data <- rep1_data %>%
  mutate(
    treatment = recode(well,
                       A6 = 'substrate', B6 = 'substrate', C6 = 'substrate',
                       A7 = 'plasmin_only', B7 = 'plasmin_only', C7 = 'plasmin_only',
                       A8 = 'licl_only', B8 = 'licl_only', C8 = 'licl_only',
                       A9 = 'high_licl_plasmin', B9 = 'high_licl_plasmin', C9 = 'high_licl_plasmin',
                       A10 = 'low_licl_plasmin', B10 = 'low_licl_plasmin', C10 = 'low_licl_plasmin', # A11 high plasmin
                       .default = NA_character_
    )
  ) %>%
  filter(!is.na(treatment))


# ggplot(data = rep1_data, aes(x=`Cycle(Seconds)/Well`, y = fluorescence, color= well)) + geom_line()



# rep 2 - 25/9
rep2_data<- rep_2 %>% as.data.frame() %>% 
  pivot_longer(cols = -`Cycle(Seconds)/Well`, names_to= "well", values_to= "fluorescence")

rep2_data$well<- as.factor(rep2_data$well)

rep2_data <- rep2_data %>%
  mutate(
    treatment = recode(well,
                       D6 = 'substrate', D7 = 'substrate', D8 = 'substrate',
                       E7 = 'plasmin_only', E8 = 'plasmin_only',  # E6 excluded
                       F6 = 'licl_only', F7 = 'licl_only', F8 = 'licl_only',
                       G7 = 'high_licl_plasmin', G8 = 'high_licl_plasmin',  # G6 excluded
                       H6 = 'low_licl_plasmin', H7 = 'low_licl_plasmin', H8 = 'low_licl_plasmin',
                       .default = NA_character_
    )
  ) %>%
  filter(!is.na(treatment))
# H5 high plasmin

# mean_rep2<- rep2_data %>% group_by(treatment, `Cycle(Seconds)/Well`) %>% 
#   summarise(mean_fluoro= mean(fluorescence),
#             sd_fluoro= sd(fluorescence),
#             se_fluoro= sd_fluoro / sqrt(n()))

# rep2plot<- ggplot(rep2_data, aes(x=`Cycle(Seconds)/Well`, y = fluorescence, color= well))+
#  geom_line()
# 
# ggplotly(rep2plot)

# rep 3
rep3_data<- rep_3 %>% as.data.frame() %>% 
  pivot_longer(cols = -`Cycle(Seconds)/Well`, names_to= "well", values_to= "fluorescence")

rep3_data$well<- as.factor(rep3_data$well)

rep3_data <- rep3_data %>%
  mutate(
    treatment = recode(well,
                       D10 = 'substrate', D11 = 'substrate', D12 = 'substrate',
                       E10 = 'plasmin_only', E11 = 'plasmin_only', E12 = 'plasmin_only',
                       F10 = 'licl_only', F11 = 'licl_only', F12 = 'licl_only',
                       G10 = 'high_licl_plasmin', G11 = 'high_licl_plasmin', G12 = 'high_licl_plasmin',
                       H10 = 'low_licl_plasmin', H11 = 'low_licl_plasmin', H12 = 'low_licl_plasmin',
                       .default = NA_character_
    )
  ) %>%
  filter(!is.na(treatment))


# rep3plot<- ggplot(rep3_data, aes(x=`Cycle(Seconds)/Well`, y = fluorescence, color= well))+
#   geom_line() 
# 
# ggplotly(rep3plot)


# Combining data ----------------------------------------------------------

# Should normalise treatments at time zero for each experiment





norm1 <- rep1_data %>%
  group_by(well) %>%
  mutate(
    baseline =fluorescence[`Cycle(Seconds)/Well` == 0][1],
    fl_norm = fluorescence - baseline
  ) %>%
  ungroup()

norm2 <- rep2_data %>%
  group_by(well) %>%
  mutate(
    baseline =fluorescence[`Cycle(Seconds)/Well` == 0][1],
    fl_norm = fluorescence - baseline
  ) %>%
  ungroup()

norm3 <- rep3_data %>%
  group_by(well) %>%
  mutate(
    baseline =fluorescence[`Cycle(Seconds)/Well` == 0][1],
    fl_norm = fluorescence - baseline
  ) %>%
  ungroup()


# combine, change x axis to hours
combined<- rbind(norm1, norm2, norm3) %>%   mutate(time_hours = `Cycle(Seconds)/Well` / 3600) %>% 
  mutate(time_hours = round(time_hours, 3))



plot<- combined %>%  group_by(treatment, time_hours) %>% 
  summarise(mean= mean(fl_norm),
    sd_fluoro= sd(fl_norm),
    se_fluoro= sd_fluoro / sqrt(n()))



ggplot(plot, 
       aes(x=time_hours, y = mean, group= treatment, colour= treatment))+ geom_line() + 
  geom_ribbon(
    data= plot, 
    aes(x= time_hours, ymin= mean-se_fluoro, ymax= mean+se_fluoro, fill=treatment), alpha= 0.2, colour= NA) + theme_bw()


aov(data= plot, mean ~ treatment)
TukeyHSD(aov(data= plot, mean ~ treatment))

