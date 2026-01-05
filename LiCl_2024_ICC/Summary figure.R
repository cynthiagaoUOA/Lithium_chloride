all_bcat_summarised$protein<- "b-Catenin"
all_claudin_summarised$protein<- "Claudin-5"
all_VE_summarised$protein<- "VE-cadherin"
all_pecam_summarised$protein<- "PE-CAM"
all_zono_summarised$protein<- "Zono occludin-1"


# test_pecam_cont_area<- all_pecam_summarised %>% select(c(protein, sample, timepoint, mean_cont_area)) %>% 
#   pivot_wider(names_from = sample, values_from = c(mean_cont_area)) %>% 
#   mutate(
#     perc_change_low_licl= (`Low LiCl + plasmin`-`Plasmin`)/`Plasmin`,
#     perc_change_high_licl= (`High LiCl + plasmin`-`Plasmin`)/`Plasmin`) 

total_data<- rbind(all_bcat_summarised, all_claudin_summarised, all_pecam_summarised, all_zono_summarised, all_VE_summarised)

total_data$protein<- fct_relevel(total_data$protein, "PE-CAM", "Zono occludin-1", "Claudin-5","b-Catenin","VE-cadherin" )

#data wrangling for mean_cont_area
summary_data_cont_area<- total_data %>% select(c(protein, sample, timepoint, mean_cont_area)) %>% 
  pivot_wider(names_from = sample, values_from = c(mean_cont_area)) %>% #pivot wider first then revert after math is done
  mutate(
    perc_change_low_licl= (`Low LiCl + plasmin`-`Plasmin`)/`Plasmin`,
    perc_change_high_licl= (`High LiCl + plasmin`-`Plasmin`)/`Plasmin`) %>% 
  pivot_longer(cols=3:8, names_to= "sample", values_to = "mean_cont_area") %>% 
  pivot_longer(cols=3:4, names_to="licl_conc", values_to="perc_rescue") %>% 
  mutate(licl_conc=recode(
    licl_conc, 'perc_change_low_licl'='1mM LiCl', 'perc_change_high_licl'='10mM LiCl'))
  
summary_data_cont_area$licl_conc<- fct_relevel(summary_data_cont_area$licl_conc, 
                                               "1mM LiCl","10mM LiCl")

#plot
ggplot(summary_data_cont_area, aes(x=licl_conc, y=protein, fill=perc_rescue*100))+ facet_wrap(~timepoint) +
  geom_tile(color="black")+guides(fill = guide_colourbar(title = ""))+
  scale_fill_gradient2(
    low = "burlywood", 
    mid = "white", 
    high = "darkslateblue", 
    midpoint = 0, limits = c(-6, 42)
  )+theme_minimal()


  
# junctional flurorescence
summary_data_cont_fluoro<- total_data %>% select(c(protein, sample, timepoint, mean_cont_fluoro)) %>% 
    pivot_wider(names_from = sample, values_from = c(mean_cont_fluoro)) %>% #pivot wider first then revert after math is done
    mutate(
      perc_change_low_licl= (`Low LiCl + plasmin`-`Plasmin`)/`Plasmin`,
      perc_change_high_licl= (`High LiCl + plasmin`-`Plasmin`)/`Plasmin`) %>% 
    pivot_longer(cols=3:8, names_to= "sample", values_to = "mean_cont_fluoro") %>% 
    pivot_longer(cols=3:4, names_to="licl_conc", values_to="perc_rescue") %>% 
    mutate(licl_conc=recode(
      licl_conc, 'perc_change_low_licl'='1mM LiCl', 'perc_change_high_licl'='10mM LiCl'))
  
  summary_data_cont_fluoro$licl_conc<- fct_relevel(summary_data_cont_fluoro$licl_conc, 
                                                 "1mM LiCl","10mM LiCl")
  
ggplot(summary_data_cont_fluoro, aes(x=licl_conc, y=protein, fill=perc_rescue*100))+ facet_wrap(~timepoint) +
    geom_tile(color="black")+guides(fill = guide_colourbar(title = ""))+
    scale_fill_gradient2(
      low = "burlywood", 
      mid = "white", 
      high = "darkslateblue", 
      midpoint = 0, limits = c(-6, 42)
    )+theme_minimal()

# plasmin -----------------------------------------------------------------

summary_data_plasmin<- total_data %>% filter(sample=="Plasmin")

#area
area<- ggplot(summary_data_plasmin, aes(x=timepoint, y=protein, fill=mean_cont_area)) + 
  geom_tile(color="black")+guides(fill = guide_colourbar(title = ""))+
  scale_fill_distiller(palette="YlOrRd", limits=c(0.4,1))+theme_minimal()+labs(x="", y="")

#flouro
fl<- ggplot(summary_data_plasmin, aes(x=timepoint, y=protein, fill=mean_cont_fluoro)) + 
  geom_tile(color="black")+guides(fill = guide_colourbar(title = ""))+
  scale_fill_distiller(palette="YlOrRd",limits=c(0.4,1))+theme_minimal()+labs(x="", y="")


library(patchwork)
area+fl+plot_layout(guides='collect')

fl


# previous version --------------------------------------------------------
#A complete summary

#normalised
ggplot(total_data, aes(sample, factor(protein, levels = c("PE-CAM", "Zono occludin", "Claudin-5","b-Catenin","VE-cadherin" )),
                                      fill=mean_cont_area)) +geom_tile()+
  facet_wrap(~timepoint)+theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+scale_fill_distiller(palette='BuPu')

#scale_fill_viridis_c(option="rocket")

ggplot(total_data, aes(sample, factor(protein, levels = c("PE-CAM", "Zono occludin", "Claudin-5","b-Catenin","VE-cadherin" )), fill=mean_cont_fluoro)) +geom_tile()+
  facet_wrap(~timepoint)+theme_bw()+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=12)
  )+scale_fill_distiller(palette='BuPu')
