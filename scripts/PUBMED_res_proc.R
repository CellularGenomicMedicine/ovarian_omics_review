#########################################################################################################################
#' Author: Darina Obukhova
#' Lab: Cellular Genomic Medicine, Maastricht University Medical Center (MUMC+)

#' Script purpose: Process PubMed results: make one data frame separate .csv files as
#' all search results can not be exported at once, deduplicate the resultant dataframe and filter out non-ovary | ovarian
#' processes-related publications using regex. 

#' Input files: Tables with exported search results
#' Output files: Deduplicated and filtered table

########################################################################################################################

setwd("~/projects/scoping_review/db_search/pubmed_search_05082025/")

pubmed_files <- list.files(path=pubmed_dir, pattern = ".csv$", full.names = TRUE)

read_pubmed <- function(file) {
  if(is.na(file)) stop("No file exists")
  df <- read.csv(file)
  df <- df[, colnames(df) %in% c("PMID", "Title", "Publication.Year")]
}

pubmed_merged <- purrr::map_dfr(.x = pubmed_files, read_pubmed)

colnames(pubmed_merged) <- c("PMID", "Title", "Year")

# Exclude publications earlier than 2008

pub_years <- c(2008:2025)

pubmed_merged$Year <- as.character(pubmed_merged$Year)

pubmed_merged_sub <- pubmed_merged[pubmed_merged$Year %in% pub_years,]

pubmed_merged_sub_dedup <- pubmed_merged_sub[!duplicated(pubmed_merged_sub[, c("Title")]), ]

pubmed_nonovary <- pubmed_merged_sub_dedup %>%
  dplyr::filter(
    !grepl("(?i)ovary|ovarian|ovaries|oocyte|follicle|germ|oogenesis|folliculogenesis|follicular|gonad|gonadogenesis|gonadal|oogonia|granulosa|cumulus|egg|eggs|
           corpus luteum|gamete|gametes", Title) 
  )

pubmed_ovary <- pubmed_merged_sub_dedup[!pubmed_merged_sub_dedup$Title %in% pubmed_nonovary$Title, ]

sprintf("%i records were removed because they didn't not include (female) gonad-related terms. Now there are %i records left",
        dim(pubmed_merged_sub_dedup)[1] - dim(pubmed_ovary)[1], dim(pubmed_ovary)[1])

write.csv2(pubmed_ovary, file = paste0(getwd(), "/pubmed_checked_05082025.csv"))


