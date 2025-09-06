######################################################################################################################

#' Author: Darina Obukhova
#' Lab: Cellular Genomic Medicine, Maastricht University Medical Center (MUMC+)

#' Script purpose: Process Web of Science Core Collection results: make one data frame from 12 separate .xsl files as
#' > 1000 search results can not be exported at once, deduplicate the resultant dataframe and filter out non-ovary | ovarian
#' processes-related publications using regex. 

#' Input files: Tables with exported search results
#' Output files: Deduplicated and filtered table

########################################################################################################################

setwd("~/projects/scoping_review/db_search/web_of_science_search_05082025/")

# Loading libraries

suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(writexl))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(citationchaser))

# Loading all Excel files

wos_excel_files <- list.files(path=getwd(), pattern = ".xls$")

# Reading function

read_excel_file <- function(file) {
  if(is.na(file)) stop("No file exists")
  df <- readxl::read_excel(file, col_names=TRUE)
  df <- df[, colnames(df) %in% c("Authors", "Article Title", "Source Title", "Publication Year")]
}

# Looping to read all Excel files data and creating a list out of them

wos_files_list <- purrr::map(.x=wos_excel_files, .f=read_excel_file)

# Merge all dataframes by row

wos_merged <- purrr::map_dfr(.x = wos_excel_files, read_excel_file)

# Exclude publications earlier than 2008

pub_years <- c(2008:2025)

wos_merged$`Publication Year` <- as.character(wos_merged$`Publication Year`)

wos_merged_sub <- wos_merged[wos_merged$`Publication Year` %in% pub_years,]

wos_merged_sub_dedup <- wos_merged_sub[!duplicated(wos_merged_sub[, c("Article Title")]), ]

wos_merged_sub_dedup <- setNames(wos_merged_sub_dedup, c("Authors", "Article_Title", "Source_Title", "Publication_Year"))

wos_merged_nonovary <- wos_merged_sub_dedup %>%
  dplyr::filter(
    !grepl("(?i)ovary|ovarian|ovaries|oocyte|follicle|germ|oogenesis|folliculogenesis|follicular|gonad|gonadogenesis|gonadal|oogonia|granulosa|cumulus|egg|eggs|
           corpus luteum|gamete|gametes", Article_Title) 
  )

wos_merged_ovary <- wos_merged_sub_dedup[!wos_merged_sub_dedup$Article_Title %in% wos_merged_nonovary$Article_Title, ]

sprintf("%i records were removed because they didn't not include (female) gonad-related terms. Now there are %i records left",
        dim(wos_merged_sub_dedup)[1] - dim(wos_merged_ovary)[1], dim(wos_merged_ovary)[1])

write.csv2(wos_merged_ovary, file = "~/projects/scoping_review/db_search/web_of_science_search_05082025/wos_checked_05082025.csv")