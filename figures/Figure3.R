######################################################################################################################

#' Author: Darina Obukhova
#' Lab: Cellular Genomic Medicine, Maastricht University Medical Center (MUMC+)

#' Script purpose: Create Figure #3 with decribing patients' categories, numbers, 
#' with series of donut charts for representation of different characteristics

#' Input files: Dataframe with the information, extracted from the studies, included in the scoping review. 
#' (Supplementary Files)
#' Output files: 4 separate files to be later combined in the multi-panel Figure #3

########################################################################################################################

library(here)
library(readxl)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggsci)

outputdir <- "~/scoping_review/paper_related/figures/figures_upd/"
data_pat <- read_xlsx("~/projects/scoping_review/paper_related/table_drafts/data_extracted_global_chars_upd.xlsx",
                      sheet=2)

data_sep_pat <- data_pat %>% 
  tidyr::separate_longer_delim(c("Study_subject_category"), delim = ",")

data_sep_pat_stage <- data_sep_pat %>% 
  plyr::mutate(res = stringr::str_trim(`Study_subject_category`)) %>%
  dplyr::filter(!is.na(`Study_subject_category`), Study_subject_category != "") %>%
  dplyr::group_by(`Study_subject_category`) %>%
  dplyr::summarise(stage_count = dplyr::n(), .groups = "drop")

pat_cat_fig3 <- ggplot(data_sep_pat_stage, aes(x = stage_count, y = reorder(Study_subject_category, stage_count))) +
  geom_bar(stat = "identity", fill = "darkorchid4") + 
  labs(x="No.of studies") + 
  theme(aspect.ratio = 1,
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10, family = "Helvetica"),
        axis.line.y = element_blank())

ggsave(paste0(outputdir, "Figure_3A.pdf"), plot=pat_cat_fig3, height = 4, width = 6)

data_sep_pat_hormone <- data_pat %>% 
  tidyr::separate_longer_delim(c("Reproductive_hormones_level"), delim = ",")

data_sep_pat_hormone_st <- data_sep_pat_hormone %>% 
  dplyr::mutate(res = stringr::str_trim(`Reproductive_hormones_level`)) %>%
  dplyr::filter(!is.na(`Reproductive_hormones_level`), Reproductive_hormones_level != "") %>%
  dplyr::group_by(`Reproductive_hormones_level`) %>%
  dplyr::summarise(reported = dplyr::n(), .groups = "drop")

#Fraction
data_sep_pat_hormone_st$fraction = data_sep_pat_hormone_st$reported / sum(data_sep_pat_hormone_st$reported)

data_sep_pat_hormone_st$ymax = cumsum(data_sep_pat_hormone_st$fraction)

data_sep_pat_hormone_st$ymin = c(0, head(data_sep_pat_hormone_st$ymax, n=-1))

fig_3b1 <- ggplot(data_sep_pat_hormone_st, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=as.factor(reported))) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(-1, 4))  +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = fill_colors_multi)

ggsave(paste0(outputdir, "Figure_3B1.pdf"), plot=fig_3b1, height = 3, width = 3)

data_sep_BMI <- data_pat %>% 
  tidyr::separate_longer_delim(c("BMI"), delim = ",")

data_sep_BMI_st <- data_sep_BMI %>% 
  dplyr::mutate(res = stringr::str_trim(`BMI`)) %>%
  dplyr::filter(!is.na(`BMI`), BMI != "") %>%
  dplyr::group_by(`BMI`) %>%
  dplyr::summarise(reported = dplyr::n(), .groups = "drop")

data_sep_BMI_st$fraction = data_sep_BMI_st$reported / sum(data_sep_BMI_st$reported)

data_sep_BMI_st$ymax = cumsum(data_sep_BMI_st$fraction)

data_sep_BMI_st$ymin = c(0, head(data_sep_BMI_st$ymax, n=-1))

fig_3b2 <- ggplot(data_sep_BMI_st, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=as.factor(reported))) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(-1, 4))  +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = fill_colors_multi)

ggsave(paste0(outputdir, "Figure_3B2.pdf"), plot=fig_3b2, height = 3, width = 3)

data_sep_re <- data_pat %>% 
  tidyr::separate_longer_delim(c("Race/ethnicity"), delim = ",")

data_sep_re_st <- data_sep_re %>% 
  dplyr::mutate(res = stringr::str_trim(`Race/ethnicity`)) %>%
  dplyr::filter(!is.na(`Race/ethnicity`), `Race/ethnicity` != "") %>%
  dplyr::group_by(`Race/ethnicity`) %>%
  dplyr::summarise(reported = dplyr::n(), .groups = "drop")

data_sep_re_st$fraction = data_sep_re_st$reported / sum(data_sep_re_st$reported)

data_sep_re_st$ymax = cumsum(data_sep_re_st$fraction)

data_sep_re_st$ymin = c(0, head(data_sep_re_st$ymax, n=-1))

fig_3b3 <- ggplot(data_sep_re_st, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=as.factor(reported))) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(-1, 4))  +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = fill_colors_multi)

ggsave(paste0(outputdir, "Figure_3B3.pdf"), plot=fig_3b3, height = 3, width = 3)

data_sep_st <- data_pat %>% 
  tidyr::separate_longer_delim(c("Stimulated"), delim = ",")

data_sep_s_st <- data_sep_st %>% 
  dplyr::mutate(res = stringr::str_trim(`Stimulated`)) %>%
  dplyr::filter(!is.na(`Stimulated`), `Stimulated` != "") %>%
  dplyr::group_by(`Stimulated`) %>%
  dplyr::summarise(reported = dplyr::n(), .groups = "drop")

data_sep_s_st$fraction = data_sep_s_st$reported / sum(data_sep_s_st$reported)

data_sep_s_st$ymax = cumsum(data_sep_s_st$fraction)

data_sep_s_st$ymin = c(0, head(data_sep_s_st$ymax, n=-1))

fig_3b4 <- ggplot(data_sep_s_st, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=as.factor(reported))) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(-1, 4))  +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = fill_colors_mod)

ggsave(paste0(outputdir, "Figure_3B4.pdf"), plot=fig_3b4, height = 2, width = 2)

fig3c <- ggplot(known_samp_size, aes(x = `#`, y = `Sequenced_participants`)) +
  geom_point(aes(color = Sequenced_participants == 6), size = 3) +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "forestgreen"), guide = "none") +
  labs(
    x = "Study ID (#)",
    y = "No. of sequenced participants"
  ) +
  scale_y_continuous(
    breaks = c(6, seq(0, max(known_samp_size$Sequenced_participants), by = 20))
  ) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.y = element_text(size = 10, family = "Helvetica"),
    axis.line.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

ggsave(paste0(outputdir, "Figure_3C.pdf"), plot = fig3c, height = 4, width = 4)

data_sep <- data %>% 
  tidyr::separate_longer_delim(c(`Profiled sample type`), delim = ",")

data_sep_samples <- data_sep %>% 
  dplyr::group_by(`Profiled sample type`) %>% 
  summarise(stage_count = n())

data_bt <- read_xlsx("/Users/darinaobukhova/projects/scoping_review/paper_related/table_drafts/fig3_tab.xlsx",
                     sheet=1)
data_bt$N_samples <- as.integer(data_bt$N_samples)

fig3d <- ggplot(data_bt, aes(x=`Study_ID`, y=`N_samples`, color = `Stage`)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("fetal" = "#42B54099", "reproductive age" = "#FF9999", "prepubertal" = "mediumpurple", "pubertal" = "green")) +
  scale_y_continuous(
    breaks = c(4, seq(0, max(known_samp_size$Sequenced_participants), by = 20))
  ) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.y = element_text(size = 10, family = "Helvetica"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length.y = unit(0.08, "cm"),
        axis.line.y = element_blank(),
        axis.text.x = element_blank())

ggsave(paste0(outputdir, "Figure_3D.pdf"), plot=fig3d, height = 4, width = 6)

data_bt <- read_xlsx("/Users/darinaobukhova/projects/scoping_review/paper_related/table_drafts/fig3_tab.xlsx",
                     sheet=2)
data_bt$N_samples <- as.integer(data_bt$N_samples)

fig3e <- ggplot(data_bt, aes(x=`Study_ID`, y=`N_samples`, color = `Stage`)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("fetal" = "#42B54099", "prepubertal" = "mediumpurple", "reproductive age" = "#FF9999", "postmenopausal" = "#00468B99")) +
  scale_y_continuous(breaks = seq(4, max(data_bt$N_samples), by = 20)) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.y = element_text(size = 10, family = "Helvetica"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length.y = unit(0.08, "cm"),
        axis.line.y = element_blank(),
        axis.text.x = element_blank())

ggsave(paste0(outputdir, "Figure_3E.pdf"), plot=fig3e, height = 4, width = 6)

data_bt <- read_xlsx("/Users/darinaobukhova/projects/scoping_review/paper_related/table_drafts/fig3_tab.xlsx",
                     sheet=5)
data_bt$N_samples <- as.integer(data_bt$N_samples)

fig3f <- ggplot(data_bt, aes(x=`Study_ID`, y=`N_samples`, color = `Stage`)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("fetal" = "#42B54099", "prepubertal" = "mediumpurple", "reproductive age" = "#FF9999", "postmenopausal" = "#00468B99")) +
  scale_y_continuous(breaks = seq(4, max(data_bt$N_samples), by = 20)) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.y = element_text(size = 10, family = "Helvetica"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length.y = unit(0.08, "cm"),
        axis.line.y = element_blank(),
        axis.text.x = element_blank())

ggsave(paste0(outputdir, "Figure_3F.pdf"), plot=fig3f, height = 4, width = 6)

data_bt <- read_xlsx("/Users/darinaobukhova/projects/scoping_review/paper_related/table_drafts/fig3_tab.xlsx",
                     sheet=6)
data_bt$N_samples <- as.integer(data_bt$N_samples)

fig3g <- ggplot(data_bt, aes(x=`Study_ID`, y=`N_samples`, color = `Stage`)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("fetal" = "#42B54099", "prepubertal" = "mediumpurple", "reproductive age" = "#FF9999", "postmenopausal" = "#00468B99")) +
  scale_y_continuous(breaks = seq(4, max(data_bt$N_samples), by = 20)) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.y = element_text(size = 10, family = "Helvetica"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length.y = unit(0.08, "cm"),
        axis.line.y = element_blank(),
        axis.text.x = element_blank())

ggsave(paste0(outputdir, "Figure_3G.pdf"), plot=fig3g, height = 4, width = 6)
