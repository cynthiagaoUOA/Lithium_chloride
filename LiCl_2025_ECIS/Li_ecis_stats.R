# Stats

### Stats stuff --------------------------------------------------------------------


#all plot data is subset Rb, resample time, time normalised, and subsetted (between -4 and 20 hours)

stats_data<- all_data %>% 
  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_subset(time = c(-5,20), sampleid=c(1:12)) #


stats_data %>% vascr_plot_anova(unit = "Rb", frequency = "0", time = 4)

?vascr_plot_anova


statsdatatest<- datawextra %>% 
  vascr_subset(unit = "Rb") %>% 
  vascr_resample_time(500) %>% 
  vascr_subset(sampleid=c(1,4,7,12), time = c(-5,20))


statsdatatest %>% vascr_plot_anova(unit = "Rb", frequency = "0", time = 4)
#

#samples and sampleID too many


li_stats_data<- all_data %>% 
  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_subset(sampleid=c(1:12),time=c(-4,30)) 


li_stats_data %>% # vascr_subset(sampleid=c(100, 17)) %>% 
  vascr:::vascr_plot_line_dunnett (unit = "Rb", frequency = "0", time = list(4,12), reference = "vehicle", normtime=-2) +xlim(-4,20)


#stats is on unnorm data but plot shows norm

#do I need to correct for multiple comparisons


stats_data$Experiment <- factor(stats_data$Experiment)
stats_data$Sample   <- factor(stats_data$Sample)


# vascrline dunnet shows normalised lines, but performs the statistical analysis on the unnormlised data. Looks cleaner and is valid
stats_data %>%  vascr_subset(sampleid=c(100, 17)) %>% vascr:::vascr_plot_line_dunnett (unit = "Rb", frequency = "0", time = list(4,24,20), reference = "vehicle", normtime=-2) +
  xlim(-4,40) +ylim(-1,0.5)


dunnett<- stats_data %>%  vascr_subset(sampleid=c(12, 1:10)) %>% vascr_dunnett (unit = "Rb", frequency = "0", time = list(4,12), reference = "vehicle") 


sigdunnet <- dunnett %>% filter(Label!="ns") %>%  filter(Label!="+")

sigdunnet


# No to Dunnets, compare everything to each other
#li_stats_data %>% aov(Value~ Experiment + Sample)

#vascr_plot_anova(all_data, unit = "Rb", frequency= 0, time=list(4,12))


fourhr_li_stats_data <- all_data %>% 
  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_subset(sampleid=c(1:12),time=4)



four <- lmer(Value ~ Sample + (1|Experiment), data = fourhr_li_stats_data)
summary(four)

emfour<- emmeans(four, specs = "Sample")
 pairs(emfour, adjust = "tukey")
 
 #12
 twelvehr_li_stats_data <- all_data %>% 
   vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
   vascr_resample_time(500) %>% 
   vascr_subset(sampleid=c(1:12),time=12)
 
 twelve <- lmer(Value ~ Sample + (1|Experiment), data = twelvehr_li_stats_data)
 summary(twelve)
 
 emtwelve<- emmeans(twelve, specs = "Sample")
 pairs(emtwelve, adjust = "tukey")
 
 #1 hour, dip
one_li_stats_data <- all_data %>% 
   vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
   vascr_resample_time(500) %>% 
   vascr_subset(sampleid=c(1:12),time=1)
 
 one <- lmer(Value ~ Sample + (1|Experiment), data = one_li_stats_data)
 summary(one)
 
 emone<- emmeans(one, specs = "Sample")
 pairs(emone, adjust = "tukey")

library(emmeans)
library(lme4)
#from 738 course guide

# em<- emmeans(model, specs = "sample")
# pairs(em, adjust = "tukey")

# plot(em, comparisons = TRUE) + theme_bw()

# residuals_pearson <- residuals(model, type = "pearson")
# shapiro.test(residuals_pearson)


#at T4, VE
# plasmin sig lower than any of the waters <0.001, 




significance <- function (replicate1, replicate2, replicate3, timepoint, variable){
  combine_data <- rbind(replicate1, replicate2, replicate3) %>% filter(timepoint==!!timepoint)
  combine_data$sample <- as.factor(combine_data$sample)
  
  formula_str <- paste(variable, "~ sample + (1|experiment)") 
  model <- lmer(as.formula(formula_str), data = combine_data)
  em<- emmeans(model, specs = "sample")
  
  return(pairs(em, adjust = "tukey"))