library(tidyverse)
library(stringr)

# define regex
tyre_name_regex <- "( — [A-Z]{1,2}-[0-9])| — NHS| — LS2| — BL|ALL TRACTION"
tyre_size_regex <- "([A-Z]{2})?([0-9]{3}\\/)?([0-9]{1,2}[xX])?[0-9.L]{1,5}[RD-][0-9.]{2,4}([A-Z]{1,3})?\\s+[0-9]"

# get the page indexes from the contents page
tyre_pages <- list(
tractor = c(6:13,16:29,32:51,54:65)+1,
harvest = 68:87+1,
sprayer = 90:99+1,
irrigation = 102:105+1,
fronts_implements = 108:127+1,
forestry = 130:134+1,
discontinued = 137:152+1)

stop_words <- c(
  "TRACTOR",
  "HARVEST",
  "SPRAYER",
  "IRRIGATION\\s\\s",
  "FRONTS &",
  "IMPLEMENTS",
  "FORESTRY\\s\\s",
  "DISCONTINUED",
  "LOAD &",
  "INFLATION",
  "TABLES",
  "GENERAL",
  "FARM TIRE\\s\\s",
  "INFORMATION",
  "INDEX"
)

col_names <- c(
  "TIRE NAME",
  "TIRE SIZE",
  "MATERIAL NUMBER",
  "LOAD INDEX", # 144 (OPTIONAL)
  "PLY RATING", # 4,6 (OPTIONAL)
  "SPEED SYMBOL RATING", # A6
  "WEIGHT", # 12, 16
  "DESIGN RIM WIDTH", #4.00
  "OVERALL WIDTH", # 5.0
  "OVERALL DIA.", # 23.8
  "STATIC LOADED RADIUS", #11.3
  "ROLLING CIRCUM.", # 242 (OPTIONAL)
  "ROLLING CIRCUM. INDEX", # 72 (OPTIONAL)
  "BAR HEIGHT 32NDS", # 32
  "BAR HEIGHT INCHES", # 1.00
  "FLAT PLATE", # 375 (OPTIONAL)
  "MAX LOAD & INFL. (LBS @ PSI)")

# add the tyre name to the lines
add_tyre_name <- function(x,tyre_name_regex,stop_words) {
  i <- str_detect(x,tyre_name_regex)
  tyre_name <- str_trim(str_remove_all(x[i],paste0(stop_words,collapse = "|")))
  x <- paste(c(NA,tyre_name)[cumsum(i)+1],x,sep = "   ")
  return(x)
}

# make the columns up to 17 by checking the order and adding "-" where necessary
add_missing_cols <- function(x) {
  # remove bad columns
  if(!str_detect(x[3],"[0-9]{6}")) { # Check material is correct
    if(str_detect(x[2],"[A-Z]")) { # see farm tire I-1 pg 117
      x <- x[-2]
    }
  }
  if(length(x)<17) {
    if(!str_detect(x[6],"[A-G][1-8]?|-")) { # check if speed rating is in column 6
      if(nchar(x[4])==3) { # check if Load index
        x <- append(x,"-",4) # add after load index
      } else {
        x <- append(x,"-",3) # add after ply
      }
    }
    if(!str_detect(x[15],"[0-9]{1,2}\\.[0-9]{2}")) { # check that the bar height in inches is in column 15
      if(nchar(x[12])>=3) { # check if rolling circum
        x <- append(x,"-",12) # add circum. index
      } else {
        x <- append(x,"-",11) # add rolling circum.
      }
    }
    if(str_detect(x[16],"@")) {
      x <- append(x,"-",15) # add flat plate
    }
  }
  return(x)
}

# extract the lines with tyre sizes and split columns
get_tyre_sizes <- function(x,tyre_name_regex,stop_words,tyre_size_regex) {
  x %>%
    add_tyre_name(tyre_name_regex,stop_words) %>%
    subset(str_detect(.,tyre_size_regex)) %>%
    str_remove_all(paste0(stop_words,collapse = "|")) %>%
    str_trim() %>%
    str_split("\\s{2}\\s+") %>%
    lapply(str_replace_all,"\\s+"," ") %>%
    lapply(add_missing_cols)
}

make_tibble <- function(x) {
  as_tibble(matrix(unlist(x),
                   nrow=length(x),
                   byrow=TRUE,
                   dimnames = list(NULL,col_names))) %>%
    na_if("-")
}

# load raw data
fsAg_data <- readRDS("data-raw/fsAg_data")

# split the data into groups by page number
fsAg_data <- lapply(tyre_pages, function(x,data) data[x],fsAg_data)

# split the data into lines
fsAg_data <- lapply(fsAg_data, function(x) lapply(x, str_split, "\\r\\n", simplify = TRUE))

# remove the page seperations as they are not needed
fsAg_data <- lapply(fsAg_data, unlist, use.names= FALSE )

# extract the tyre size data and format
fsAg_data <- lapply(fsAg_data, get_tyre_sizes,tyre_name_regex,stop_words,tyre_size_regex)

# make a tibble(data.frame)
fsAg_data <- lapply(fsAg_data, make_tibble)

#TODO test the formats of the columns
