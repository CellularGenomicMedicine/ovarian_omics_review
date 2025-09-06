######################################################################################################################

#' Author: Darina Obukhova
#' Lab: Cellular Genomic Medicine, Maastricht University Medical Center (MUMC+)

#' Script purpose: Create Figure #2

#' Input files: Dataframe with the information, extracted from the studies, included in the scoping review. 
#' (Supplementary Files)
#' Output files: 4 separate files to be later combined in the multi-panel Figure #2

########################################################################################################################

library(here)
library(readxl)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggsci)

outputdir <- "~/scoping_review/paper_related/figures/figures_upd/"
data <- read_xlsx("~/projects/scoping_review/paper_related/table_drafts/data_extracted_global_chars_upd.xlsx")

data_sep <- data %>% tidyr::separate_longer_delim(c("Developmental_stage"), delim = ",")

data_sep_stage <- data_sep %>% 
  tidyr::separate_longer_delim(c("Developmental_stage"), delim = ",") %>% 
  mutate(Developmental_stage = stringr::str_trim(Developmental_stage)) %>%
  filter(!is.na(Developmental_stage), Developmental_stage != "") %>%
  group_by(Developmental_stage) %>%
  summarise(stage_count = n(), .groups = "drop")

fill_colors_dev_stage <- c("#42B54099", "#ADB6B699", "#00468B99", "#AD002A99",
                           "#925E9F99", "#FF9999", "#FDAF9199")

data_sep_long_fig <- data_sep_stage %>%
  mutate(
    Developmental_stage = str_trim(Developmental_stage),
    prop = stage_count / 121
  ) %>%
  filter(!is.na(Developmental_stage), Developmental_stage != "") %>%
  ggplot(aes(x = 1, y = prop, fill = Developmental_stage)) +
  geom_col(width = .1) +
  scale_fill_manual(values = fill_colors_dev_stage) +
  coord_flip() +
  theme_void() + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme(aspect.ratio = 1/16, 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "top", 
        legend.spacing.x = unit(0.2, "cm"), 
        legend.title = element_blank(), 
        legend.spacing.y = unit(0.5, "cm"))

ggsave(paste0(outputdir, "Figure_2A_ed.pdf"), plot=data_sep_long_fig, height = 4, width = 6)

data_sep <- data %>%
  tidyr::separate_longer_delim(`Profiled_omic(s)`, delim = ",")

data_sep_omics_all <- data_sep %>% 
  dplyr::mutate(Profiled_omics = stringr::str_trim(`Profiled_omic(s)`)) %>%
  dplyr::filter(!is.na(Profiled_omics), Profiled_omics != "") %>%
  dplyr::group_by(Profiled_omics) %>%
  dplyr::summarise(stage_count = dplyr::n(), .groups = "drop")

fill_colors_ome <- c("#63B8FF", "#4682B4", "#6C7B8B", "#CD96CD", "#00B2EE", "#79CDCD", 
                     "#528D8B", "#1874CD", "#008B8B", "#66CDAA", "#8B5F65", "#7A37B8",
                     "#548B54", "#CD8500")

data_sep_long_fig2b <- data_sep_omics_all %>%
  mutate(
    Profiled_omics  = str_trim(Profiled_omics),
    prop = stage_count / 121
  ) %>%
  filter(!is.na(Profiled_omics), Profiled_omics  != "") %>%
  ggplot(aes(x = 1, y = prop, fill = Profiled_omics)) +
  geom_col(width = .1) +
  scale_fill_manual(values = fill_colors_ome) +
  coord_flip() +
  theme_void() + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme(aspect.ratio = 1/16, 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "top", 
        legend.spacing.x = unit(0.2, "cm"), 
        legend.title = element_blank(), 
        legend.spacing.y = unit(0.5, "cm"))

ggsave(paste0(outputdir, "Figure_2B.pdf"), plot=data_sep_long_fig2b, height = 4, width = 6)

data_sep_mod <- data %>% 
  tidyr::separate_longer_delim(c("Data_resolution"), delim = ",")

data_sep_mod <-  data_sep_mod %>% 
  plyr::mutate(res = stringr::str_trim(`Data_resolution`)) %>%
  dplyr::filter(!is.na(Data_resolution), Data_resolution != "") %>%
  dplyr::group_by(Data_resolution) %>%
  dplyr::summarise(stage_count = dplyr::n(), .groups = "drop")

fill_colors_mod <- c("#CDC9C9", "#8B8989")

data_sep_long_fig2c <- data_sep_mod %>%
  mutate(
    Data_resolution = str_trim(Data_resolution),
    prop = stage_count / 119
  ) %>%
  filter(!is.na(Data_resolution), Data_resolution != "") %>%
  ggplot(aes(x = 1, y = prop, fill = Data_resolution)) +
  geom_col(width = .1) +
  scale_fill_manual(values = fill_colors_mod) +
  coord_flip() +
  theme_void() + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme(aspect.ratio = 1/16, 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "top", 
        legend.spacing.x = unit(0.2, "cm"), 
        legend.title = element_blank(), 
        legend.spacing.y = unit(0.5, "cm"))

ggsave(paste0(outputdir, "Figure_2C.pdf"), plot=data_sep_long_fig2c, height = 4, width = 6)

data_sep_multi <-  data %>% 
  tidyr::separate_longer_delim(c("Multi-omic study"), delim = ",")

data_sep_multiome <-  data_sep_multi %>% 
  plyr::mutate(res = stringr::str_trim(`Multi-omic study`)) %>%
  dplyr::filter(!is.na(`Multi-omic study`), Data_resolution != "") %>%
  dplyr::group_by(`Multi-omic study`) %>%
  dplyr::summarise(stage_count = dplyr::n(), .groups = "drop")

fill_colors_multi <- c("#665FD1", "#e6d7ff")

data_sep_long_fig2d <- data_sep_multiome %>%
  mutate(
    `Multi-omic study` = str_trim(`Multi-omic study`),
    prop = stage_count / 119
  ) %>%
  filter(!is.na(`Multi-omic study`), `Multi-omic study` != "") %>%
  ggplot(aes(x = 1, y = prop, fill = `Multi-omic study`)) +
  geom_col(width = .1) +
  scale_fill_manual(values = fill_colors_multi) +
  coord_flip() +
  theme_void() + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme(aspect.ratio = 1/16, 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "top", 
        legend.spacing.x = unit(0.2, "cm"), 
        legend.title = element_blank(), 
        legend.spacing.y = unit(0.5, "cm"))

ggsave(paste0(outputdtir, "Figure_2D.pdf"), plot=data_sep_long_fig2d, height = 4, width = 6)
