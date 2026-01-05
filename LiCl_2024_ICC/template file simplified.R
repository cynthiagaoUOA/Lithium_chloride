
# First we create the list of metadata

# Select the file
# file.choose()

# Create a tribble of information

install.packages(c("tidyverse", "EBImage", "shiny", "bslib"))

library(tidyverse)
library(EBImage)
library(tools)
library(data.table)
library("shiny")
library("bslib")

setwd("E:/Cynthia")
source("E:\\Cynthia\\vjunctur_functions_v1.R")


lookup_1 = tribble(~well, ~ch_dapi, ~ch_actin, ~ch_antibody, ~name_antibody, ~sample,
                 "B02", 2, 3, 4, "VE-Cadherin", "1",
                 "C02", 2, 3, 4, "VE-Cadherin", "2",
                 "D02", 2, 3, 4, "VE-Cadherin", "3",
                 "E02", 2, 3, 4, "VE-Cadherin", "4",
                 "F02", 2, 3, 4, "VE-Cadherin", "5",
                 "G02", 2, 3, 4, "VE-Cadherin", "6")

file_directory = import_operetta("E:\\Cynthia\\Cynthia[12001]\\T4_26_8[27803]\\2024-08-26T125718+1200[31903]\\2024-08-26T125718+1200[31903]",
                                 lookup_1)

file_directory$experiment = "T4_26_8"


local_fd = file_directory %>% filter(name_antibody == "VE-Cadherin")
segment_and_quant_i(local_fd)
ve_cad_1 = segment_and_quant_p(local_fd, nuclear_disk =  10 , tophat_area =  10 , tophat_threshold =  0.005 , min_area =  10 , nuclear_area =  40 , nuclear_offset =  0.001 )



ve_cad_1 %>% ggplot() +
  geom_violin(aes(x = sample, y =contiguous_area)) +
  geom_text(aes(x = sample, y = contiguous_area, label = field))





















