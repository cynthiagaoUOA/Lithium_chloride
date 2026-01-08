# data <- rbind(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad)

T4_VE<- data %>% filter(timepoint=="T4")
T4_VE$sample<- as.factor(T4_VE$sample)

model <- lmer(contiguous_area ~ sample + (1|experiment), data = T4_VE)
summary(model)

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
}



# ve ----------------------------------------------------------------------

significance(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad, "T4", "contiguous_area")
significance(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad, "T12", "contiguous_area")

significance(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad, "T4", "contiguous_fluorescence")
significance(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad, "T12", "contiguous_fluorescence")

# zono
significance(quant_Exp2_zono, quant_Exp3_zono, quant_Exp4_zono, "T4", "contiguous_area")
significance(quant_Exp2_zono, quant_Exp3_zono, quant_Exp4_zono, "T12", "contiguous_area")

significance(quant_Exp2_zono, quant_Exp3_zono, quant_Exp4_zono, "T4", "contiguous_fluorescence")
significance(quant_Exp2_zono, quant_Exp3_zono, quant_Exp4_zono, "T12", "contiguous_fluorescence")

# claudin
significance(quant_Exp2_claudin, quant_Exp3_claudin, quant_Exp4_claudin, "T4", "contiguous_area")
significance(quant_Exp2_claudin, quant_Exp3_claudin, quant_Exp4_claudin, "T12", "contiguous_area")

significance(quant_Exp2_claudin, quant_Exp3_claudin, quant_Exp4_claudin, "T4", "contiguous_fluorescence")
significance(quant_Exp2_claudin, quant_Exp3_claudin, quant_Exp4_claudin, "T12", "contiguous_fluorescence")

# pecam
significance(quant_Exp2_pecam, quant_Exp3_pecam, quant_Exp4_pecam, "T4", "contiguous_area")
significance(quant_Exp2_pecam, quant_Exp3_pecam, quant_Exp4_pecam, "T12", "contiguous_area")

significance(quant_Exp2_pecam, quant_Exp3_pecam, quant_Exp4_pecam, "T4", "contiguous_fluorescence")
significance(quant_Exp2_pecam, quant_Exp3_pecam, quant_Exp4_pecam, "T12", "contiguous_fluorescence")

# bcat
significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T4", "contiguous_area")
significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T12", "contiguous_area")

significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T4", "nuclear_fluorescence")
significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T12", "nuclear_fluorescence")



significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T4", "contiguous_fluorescence")
significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T12", "contiguous_fluorescence")


# overall stain -----------------------------------------------------------

# total fluorescence

significance(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad, "T4", "overall_stain")
significance(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad, "T12", "overall_stain")

significance(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad, "T4", "contiguous_fluorescence")
significance(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad, "T12", "contiguous_fluorescence")

# zono
significance(quant_Exp2_zono, quant_Exp3_zono, quant_Exp4_zono, "T4", "overall_stain")
significance(quant_Exp2_zono, quant_Exp3_zono, quant_Exp4_zono, "T12", "overall_stain")

significance(quant_Exp2_zono, quant_Exp3_zono, quant_Exp4_zono, "T4", "contiguous_fluorescence")
significance(quant_Exp2_zono, quant_Exp3_zono, quant_Exp4_zono, "T12", "contiguous_fluorescence")

# claudin
significance(quant_Exp2_claudin, quant_Exp3_claudin, quant_Exp4_claudin, "T4", "overall_stain")
significance(quant_Exp2_claudin, quant_Exp3_claudin, quant_Exp4_claudin, "T12", "overall_stain")

significance(quant_Exp2_claudin, quant_Exp3_claudin, quant_Exp4_claudin, "T4", "contiguous_fluorescence")
significance(quant_Exp2_claudin, quant_Exp3_claudin, quant_Exp4_claudin, "T12", "contiguous_fluorescence")

# pecam
significance(quant_Exp2_pecam, quant_Exp3_pecam, quant_Exp4_pecam, "T4", "overall_stain")
significance(quant_Exp2_pecam, quant_Exp3_pecam, quant_Exp4_pecam, "T12", "overall_stain")

significance(quant_Exp2_pecam, quant_Exp3_pecam, quant_Exp4_pecam, "T4", "contiguous_fluorescence")
significance(quant_Exp2_pecam, quant_Exp3_pecam, quant_Exp4_pecam, "T12", "contiguous_fluorescence")

# bcat
significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T4", "overall_stain")
significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T12", "overall_stain")

significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T4", "nuclear_fluorescence")
significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T12", "nuclear_fluorescence")



significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T4", "contiguous_fluorescence")
significance(quant_Exp2_bcat, quant_Exp3_bcat, quant_Exp4_bcat, "T12", "contiguous_fluorescence")



# Checking against JH anova  ----------------------------------------------

#In theory both analysis types will produce the same outcomes, but want to check as better to simplify methods for paper

# have to re-break down this code as i don't remember what I did
significance <- function (replicate1, replicate2, replicate3, timepoint, variable){
  combine_data <- rbind(replicate1, replicate2, replicate3) %>% filter(timepoint==!!timepoint)
  combine_data$sample <- as.factor(combine_data$sample)
  
  formula_str <- paste(variable, "~ sample + (1|experiment)") 
  model <- lmer(as.formula(formula_str), data = combine_data)
  em<- emmeans(model, specs = "sample")
  
  return(pairs(em, adjust = "tukey"))
  
  significance(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad, "T4", "contiguous_area")
  
# taking the function and doing it manual so I can see what's going on and replicate
test<-rbind(quant_Exp2_ve_cad, quant_Exp3_ve_cad, quant_Exp4_ve_cad) %>% filter (timepoint=="T4")
test$sample<- as.factor(test$sample)

testmodel<- lmer(contiguous_area ~ sample + (1|experiment), data = test)
testem<- emmeans(testmodel, specs= "sample")

pairs(testem, adjust = "tukey")



# doing linear model, returning tukeys pairs p values

# now the way that JH did for vascr

lmtest<-lm(contiguous_area ~ sample + experiment, data = test)
  
  
# install.packages("rstatix")
library(rstatix)

  tukey_hsd(lmtest)
  
# tested one timepoint and the significance lines up. So now I am pretty confident that my results will be the same as if I had used ANOVA. Happy to use in paper

