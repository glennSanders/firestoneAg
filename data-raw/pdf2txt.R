library(tidyverse)
library(pdftools)

fsAg_file <- "data-raw/2018 FIRESTONE AG DATA BOOK - FIRESTONE AGRICULTURE.pdf"
# Ultra Large Tractor

fsAg_data <- pdf_text(fsAg_file)

saveRDS(fsAg_data,"data-raw/fsAg_data")
