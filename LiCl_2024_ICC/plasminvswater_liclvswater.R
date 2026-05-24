plasmin_water_points<- rbind(all_bcat_norm_points, all_VE_norm_points, all_claudin_norm_points, all_zono_norm_points, all_pecam_norm_points) %>% 
  filter(sample=="Plasmin"| sample=="water")

plasmin_water_points$name_antibody<- plasmin_water_points$name_antibody %>%  fct_recode(
  "b-Catenin" = "b-catenin", "Zono Occludin" = "zono occluden", "Claudin-5" = "claudin 5", "PE-CAM" = "pecam") %>% 
  fct_relevel("VE-Cadherin", "b-Catenin","Claudin-5", "Zono Occludin","PE-CAM")
plasmin_water_points$sample<- recode(plasmin_water_points$sample, water="Vehicle")
plasmin_water_points$sample<- fct_relevel(plasmin_water_points$sample, "Vehicle")
plasmin_water_points$experiment<- recode(plasmin_water_points$experiment,
                                    Exp2 = '1', Exp3 = '2', Exp4='3')



# normalised to water
# plasmin_water_summarised_norm<- plasmin_water_points %>% 
#   group_by(sample, timepoint, name_antibody) %>% 
#   summarise(
#     mean_cont_area = mean(norm_cont_area),
#     sd_cont_area = sd(norm_cont_area),
#     se_cont_area = sd_cont_area / sqrt(n()),
#     mean_cont_fluoro = mean(norm_cont_fluoro),
#     sd_cont_fluoro = sd(norm_cont_fluoro),
#     se_cont_fluoro = sd_cont_fluoro / sqrt(n()))
# 
# 
# 
# ggplot(plasmin_water_summarised_norm, aes(y=mean_cont_area, x=name_antibody, fill=sample))+
#   geom_bar(stat = "identity", position="dodge" )+
#   facet_wrap(~timepoint)+
#  geom_errorbar(
#     data = plasmin_water_summarised_norm, 
#     aes(y=mean_cont_area, x=name_antibody, group=sample,
#         ymin=mean_cont_area-se_cont_area, 
#         ymax=mean_cont_area+se_cont_area), 
#     width=0.2, color = "black", position=position_dodge(width=0.9)
#   )+
#   geom_point(data=plasmin_water_points %>% filter(experiment=="1"), 
#              mapping = aes(y=norm_cont_area, x=name_antibody, color=experiment), 
#              size = 0.8, position = position_nudge(x = 0.05))+
#   geom_point(data=plasmin_water_points %>% filter(experiment=="2"), 
#                           mapping = aes(y=norm_cont_area, x=name_antibody, color=experiment), 
#                           size = 0.8, position = position_nudge(x = 0.05))+
#   geom_point(data=plasmin_water_points %>% filter(experiment=="3"), 
#              mapping = aes(y=norm_cont_area, x=name_antibody, color=experiment), 
#              size = 0.8, position = position_nudge(x = 0.05))
             
             
### nonnormalised
plasmin_water_summarised<- plasmin_water_points %>% 
  group_by(sample, timepoint, name_antibody) %>% 
  summarise(
    mean_cont_area = mean(contiguous_area),
    sd_cont_area = sd(contiguous_area),
    se_cont_area = sd_cont_area / sqrt(n()),
    mean_cont_fluoro = mean(contiguous_fluorescence),
    sd_cont_fluoro = sd(contiguous_fluorescence),
    se_cont_fluoro = sd_cont_fluoro / sqrt(n()))



ggplot(plasmin_water_summarised, aes(y=mean_cont_area, x=name_antibody, fill=sample))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~timepoint)+
  geom_errorbar(
    data = plasmin_water_summarised,
    aes(group=sample,
        ymin=mean_cont_area-se_cont_area,
        ymax=mean_cont_area+se_cont_area),
    width=0.2, color = "black", position=position_dodge(width=0.9)
  )+ theme_bw()+
  scale_fill_manual(values = c("Vehicle" = "#CABEE9", "Plasmin" = "#7C7189"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=13)
  ) + ylim(0, 150000)+
  labs(x="", y= "Junctional area of antibody stain (px)", color="Replicate number", fill = "Treatment")


ggplot(plasmin_water_summarised, aes(y=mean_cont_fluoro, x=name_antibody, fill=sample))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~timepoint)+
  geom_errorbar(
    data = plasmin_water_summarised, 
    aes(group=sample,
        ymin=mean_cont_fluoro-se_cont_fluoro, 
        ymax=mean_cont_fluoro+se_cont_fluoro), 
    width=0.2, color = "black", position=position_dodge(width=0.9)
  )+ theme_bw()+
  scale_fill_manual(values = c("Vehicle" = "#CABEE9", "Plasmin" = "#7C7189"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=13)
  ) + ylim(0,8500)+
  labs(x="", y= "Fluorescence intensity of junctional area pixels", color="Replicate number", fill = "Treatment")

# For plasmin paper, overall fluorescence

plasmin_water_summarised<- plasmin_water_points %>% 
  group_by(sample, timepoint, name_antibody) %>% 
  summarise(
    mean_cont_area = mean(contiguous_area),
    sd_cont_area = sd(contiguous_area),
    se_cont_area = sd_cont_area / sqrt(n()),
    mean_cont_fluoro = mean(contiguous_fluorescence),
    sd_cont_fluoro = sd(contiguous_fluorescence),
    se_cont_fluoro = sd_cont_fluoro / sqrt(n()),
    mean_total_fluoro = mean(overall_stain),
    sd_total_fluoro = sd(overall_stain),
    se_total_fluoro = sd_total_fluoro / sqrt(n()))

ggplot(plasmin_water_summarised, aes(y=mean_total_fluoro, x=name_antibody, fill=sample))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~timepoint)+
  geom_errorbar(
    data = plasmin_water_summarised,
    aes(group=sample,
        ymin=mean_total_fluoro-se_total_fluoro,
        ymax=mean_total_fluoro+se_total_fluoro),
    width=0.2, color = "black", position=position_dodge(width=0.9)
  )+ theme_bw()+
  scale_fill_manual(values = c("Vehicle" = "#CABEE9", "Plasmin" = "#7C7189"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=13)
  ) + ylim(0, 70000)+
  labs(x="", y= "overall fluorescence", color="Replicate number", fill = "Treatment")

# LiCL --------------------------------------------------------------------

licl_water_points<- rbind(all_bcat_norm_points, all_VE_norm_points, all_claudin_norm_points, all_zono_norm_points, all_pecam_norm_points) %>% 
  filter(sample=="water"| sample=="High LiCl + water"| sample=="Low LiCl + water")


licl_water_points$name_antibody<- fct_recode(licl_water_points$name_antibody, "b-Catenin" = "b-catenin", "Zono Occludin" = "zono occluden", "Claudin-5" = "claudin 5", "PE-CAM" = "pecam") %>% 
  fct_relevel("VE-Cadherin", "b-Catenin","Claudin-5", "Zono Occludin","PE-CAM")
  
licl_water_points$sample<- recode(licl_water_points$sample, water="Vehicle", `High LiCl + water`="High LiCl", `Low LiCl + water`= "Low LiCl")
licl_water_points$sample<- fct_relevel(licl_water_points$sample, "Vehicle", "Low LiCl")
licl_water_points$experiment<- recode(licl_water_points$experiment,
                                         Exp2 = '1', Exp3 = '2', Exp4='3')

licl_water_points_summarised<- licl_water_points %>% 
  group_by(sample, timepoint, name_antibody) %>% 
  summarise(
    mean_cont_area = mean(contiguous_area),
    sd_cont_area = sd(contiguous_area),
    se_cont_area = sd_cont_area / sqrt(n()),
    mean_cont_fluoro = mean(contiguous_fluorescence),
    sd_cont_fluoro = sd(contiguous_fluorescence),
    se_cont_fluoro = sd_cont_fluoro / sqrt(n()),
    
    mean_total = mean(overall_stain),
    sd_total= sd(overall_stain),
    se_total=sd_total/ sqrt(n()))

ggplot(licl_water_points_summarised, aes(y=mean_cont_area, x=name_antibody, fill=sample))+
  geom_bar(stat = "identity", position="dodge" )+
  facet_wrap(~timepoint)+
  geom_errorbar(
    data = licl_water_points_summarised, 
    aes(group=sample,
        ymin=mean_cont_area-se_cont_area, 
        ymax=mean_cont_area+se_cont_area), 
    width=0.2, color = "black", position=position_dodge(width=1)
  )+ theme_bw()+
  scale_fill_manual(values = c("Vehicle" = "#CABEE9", "Low LiCl"= "#7C7189", "High LiCl" = "#BC8E7D"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=13)
  ) + ylim(0,150000)+
  labs(x="", y= "Junctional area of antibody stain (px)", color="Replicate number", fill = "Treatment")



ggplot(licl_water_points_summarised, aes(y=mean_cont_fluoro, x=name_antibody, fill=sample))+
  geom_bar(stat = "identity", position="dodge" )+
  facet_wrap(~timepoint)+
  geom_errorbar(
    data = licl_water_points_summarised, 
    aes(group=sample,
        ymin=mean_cont_fluoro-se_cont_fluoro, 
        ymax=mean_cont_fluoro+se_cont_fluoro), 
    width=0.4, color = "black", position=position_dodge(width=0.9)
  )+ theme_bw()+
  scale_fill_manual(values = c("Vehicle" = "#85BEDC", "Low LiCl"= "#647588", "High LiCl" = "#CCBBCD"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=13)
  ) + ylim(0, 8500)+  theme(panel.spacing = unit(0, "lines")) +
  labs(x="", y= "Fluorescence intensity of junctional area pixels", color="Replicate number", fill = "Treatment")


ggplot(licl_water_points_summarised, aes(y=mean_total, x=name_antibody, fill=sample))+
  geom_bar(stat = "identity", position="dodge" )+
  facet_wrap(~timepoint)+
  geom_errorbar(
    data = licl_water_points_summarised, 
    aes(group=sample,
        ymin=mean_total-se_total, 
        ymax=mean_total+se_total), 
    width=0.4, color = "black", position=position_dodge(width=1)
  )+ theme_bw()+
  scale_fill_manual(values = c("Vehicle" = "#85BEDC", "Low LiCl"= "#647588", "High LiCl" = "#CCBBCD"))+
  theme(
    axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5, size=13)
  ) +  theme(panel.spacing = unit(0, "lines")) +
  labs(x="", y= "Total fluorescence area of antibody stain (px)", color="Replicate number", fill = "Treatment")

