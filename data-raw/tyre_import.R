
library(stringr)
library(tidyverse)
library(pdftools)

f <- "data-raw/2018 FIRESTONE AG DATA BOOK - FIRESTONE AGRICULTURE.pdf"
# Ultra Large Tractor

fs_data <- pdf_text(f)

get_tyre_name <- function(x) {
  x %>%
    str_extract("(?<=\\r\\n).*?(?=\\r\\n)") %>% # get second match
    str_replace("TRACTOR","") %>% # remove tractor
    str_trim()
}

get_tyre_dimensions <- function(x) {
  x %>%
    # find tyre designator and then the rest of the line
    str_extract_all("([A-Z]{2})?([0-9]{3}\\/)?([0-9]{1,2}[xX])?[0-9.L]{1,5}[RD-][0-9.]{2,4}([A-Z]{1,3})?\\s+[0-9].*?\\r\\n", simplify = TRUE) %>%
    # remove any characters from teh tab sheets and end of line text
    str_replace_all("[A-Z &]*\\r\\n(?<=\\r\\n)","") %>%
    # split by two spaces (to avoid splitting the @)
    str_split("\\s\\s+", simplify = TRUE)
}

pg <- c(7:14,17:30,33:52,55:66)

lapply(fs_data[pg],get_tyre_name)
lapply(fs_data[pg],get_tyre_dimensions)

fs_data[[14]]

ul_page <- c("RADIAL DEEP TREAD 23° — R-1W",
             "RADIAL ALL TRACTION DT — R-1W",
             "RADIAL 9000 — R-1W",
             "PERFORMER 85 — R-1W",
             "PERFORMER 65 — R-1W",
             "RADIAL ALL TRACTION 23° — R-1",
             "RADIAL ALL TRACTION FWD — R-1",
             "RADIAL CHAMPION SPADE GRIP — R-2")
ul_trac <- extract_tables(f, pages = 7:14)

extract_areas(f,8)

# remove blank columns
remove_blank_col <- function(x) {
  x[, colSums(x != "") != 0]
}

# remove rows that don't have digits and the header rows
extract_tyre_rows <- function(x) {
  x[str_detect(x[,1],"[0-9]"),]
}

# some of the page tabs come across
create_regex <- function(word) {
  letters <- str_split(word,"",simplify = TRUE)
  paste0(letters,collapse = ".*?")
}

rm_word <- function(string, word) {
  w_length <- nchar(word)
  reg <- create_regex(word)

  j <- which(str_detect(string,reg))

  if(length(j)==1){
    s_length <- nchar(string[[j]])

    sl <- str_locate(string[[j]],reg)
    se <- str_extract(string[[j]],reg)
      for(i in 1:w_length) {
        se <- str_replace(se,substr(word,i,i),"")
      }
      string[[j]] <- paste0(substr(string[[j]],0,sl[1]-1),se,substr(string[[j]],sl[2]+1,s_length))
  }
  string
}



"378882GENE15R3AL 355542FARM1 6T6IRE INFORMATION" %>%
  rm_word("GENERAL") %>%
  rm_word("FARMS")
rm_word("GENERAL","378882GENE15R3AL 355542FARM1 6T6IRE INFORMATION")


# some columns are joined
split_joined_cols <- function(x) {
  matrix(unlist(str_split(x,"(?<=[0-9])(\\s)(?=[0-9])")),
         nrow = nrow(x))
}

#remove empty characters
empty_char <- function(char_str) {
  char_str[nchar(char_str)>0]
}

# paste together rows

paste(b[[2]][1:23,])


b <- ul_trac %>%
  lapply(remove_blank_col) %>%
  lapply(extract_tyre_rows)

a <- b[[2]] %>%
  t() %>%
  as.vector %>%
  rm_word("INDEX") %>%
  rm_word("GENERAL") %>%
  rm_word("FARM TIRE") %>%
  rm_word("INFORMATION") %>%
  rm_word("LOAD &") %>%
  rm_word("INFLATION") %>%
  rm_word("TABLES") %>%
  rm_word("DISCONTINUED") %>%
  rm_word("FORESTRY") %>%
  rm_word("FRONTS &") %>%
  rm_word("IMPLEMENTS") %>%
  rm_word("IRRIGATION") %>%
  rm_word("SPRAYER") %>%
  rm_word("HARVEST") %>%
  rm_word("TRACTOR") %>%
  rm_word("ULTRA-LARGE") %>%
  str_split("(?<=[0-9])(\\s+)(?=[0-9])") %>%
  unlist %>%
  empty_char() %>%
  matrix(nrow =23)


  str_detect(create_regex("GENERAL"))




  lapply(split_joined_cols)

b[[2]]

split_joined_cols(b[[2]])
