######################################################################################################################

#' Author: Darina Obukhova
#' Lab: Cellular Genomic Medicine, Maastricht University Medical Center (MUMC+)

#' Script purpose: Create Figure #4 

#' Input files: Dataframe with the information, extracted from the studies, included in the scoping review. 
#' (Supplementary Files)
#' Output files: separate files to be later combined in the multi-panel Figure #4

########################################################################################################################

library(here)
library(readxl)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggsci)

data_bt <- read_xlsx("/Users/darinaobukhova/projects/scoping_review/paper_related/table_drafts/fig3_tab.xlsx",
                     sheet=3)
data_bt$N_samples <- as.integer(data_bt$N_samples)

fig4a1 <- ggplot(data_bt, aes(x=`Study_ID`, y=`N_samples`, color = `Stage`)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("fetal" = "#42B54099", "prepubertal" = "mediumpurple","reproductive age" = "#FF9999", "postmenopausal" = "#00468B99")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x), # Logarithmic breaks
    labels = scales::trans_format("log10", scales::math_format(10^.x)) # Format as 10^x
  ) +
  theme_minimal() +
  labs(
    x = "Study ID",
    y = "N of samples (log10)",  # Updated y-axis label
    color = "Stage"
  ) +
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

ggsave(paste0(outputdir, "Figure_4A1.pdf"), plot=fig4a1, height = 4, width = 6)

data_bt <- read_xlsx("/Users/darinaobukhova/projects/scoping_review/paper_related/table_drafts/fig3_tab.xlsx",
                     sheet=4)
data_bt$N_samples <- as.integer(data_bt$N_samples)

fig4a2 <- ggplot(data_bt, aes(x=`Study_ID`, y=`N_samples`, color = `Stage`, shape = `Ome`)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("fetal" = "#42B54099", "reproductive age" = "#FF9999", "postmenopausal" = "#00468B99")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)) 
  ) +
  scale_shape_manual(values = c("genome" = 15, "chromatin acc" = 18, "methylome" = 17)) +
  theme_minimal() +
  labs(
    x = "Study ID",
    y = "N of samples (log10)",  # Updated y-axis label
    color = "Stage"
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

ggsave(paste0(outputdir, "Figure_4A2.pdf"), plot=fig4a2, height = 4, width = 6)
