#########################################################################################################################
#' Author: Darina Obukhova
#' Lab: Cellular Genomic Medicine, Maastricht University Medical Center (MUMC+)

#' Script purpose: Process Embase results: make one data frame from separate .xsl files as
#' all search results can not be exported at once, deduplicate the resultant dataframe and filter out non-ovary | ovarian
#' processes-related publications using regex. 

#' Input files: Tables with exported search results
#' Output files: Deduplicated and filtered table

########################################################################################################################

setwd("~/projects/scoping_review/db_search/embase_search_05082025/")

embase_excel_files <- list.files(path=getwd(), pattern = ".xls$")

read_excel_file <- function(file) {
  if(is.na(file)) stop("No file exists")
  df <- readxl::read_excel(file, skip=1, col_names=TRUE)
  df <- df[, colnames(df) %in% c("TI", "SO", "AU")]
}

embase_merged <- purrr::map_dfr(.x = embase_excel_files, read_excel_file)

embase_merged$TI <- gsub("\\.", "", embase_merged$TI)

embase_dedup <- embase_merged[!duplicated(embase_merged[, c("TI")]), ]

embase_nonovary <- embase_dedup %>%
  dplyr::filter(
    !grepl("(?i)ovary|ovarian|ovaries|oocyte|follicle|germ|oogenesis|folliculogenesis|follicular|gonad|gonadogenesis|gonadal|oogonia|granulosa|cumulus|egg|eggs|
           corpus luteum|gamete|gametes", TI) 
  )

embase_ovary <- embase_dedup[!embase_dedup$TI %in% embase_nonovary$TI, ]

sprintf("%i records were removed because they didn't not include (female) gonad-related terms. Now there are %i records left",
        dim(embase_dedup)[1] - dim(embase_ovary)[1], dim(embase_ovary)[1])

write.csv2(embase_ovary, file = "~/projects/scoping_review/db_search/embase_search_05082025/embase_checked_05082025.csv")
