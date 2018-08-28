library(tidyverse)
library(stringr)

# Define data formats
tyre_regex <- list(
  "TIRE NAME" = "( — [A-Z]{1,2}-[0-9])| — NHS| — LS2| — BL|ALL TRACTION",
  "TIRE SIZE" = "([A-Z]{2})?([0-9]{3}\\/)?([0-9]{1,2}[xX])?[0-9.L]{1,5}[RD-][0-9.]{2,4}([A-Z]{1,3})?",
  "MATERIAL NUMBER" = "[0-9]{6}",
  "LOAD INDEX" = "[0-9]{3}|-",
  "PLY RATING" = "[0-9]{1,2}|-",
  "SPEED SYMBOL RATING" = "[A-G][1-8]?",
  "WEIGHT" = "[0-9]{1,3}",
  "DESIGN RIM WIDTH" = "[0-9]{1,2}\\.[0-9]{2}",
  "OVERALL WIDTH" = "[0-9]{1,2}\\.[0-9]{1}",
  "OVERALL DIA." = "[0-9]{1,2}\\.[0-9]{1}",
  "STATIC LOADED RADIUS" = "[0-9]{1,2}\\.[0-9]{1}",
  "ROLLING CIRCUM." = "[0-9]{3}|-",
  "ROLLING CIRCUM. INDEX" = "[0-9]{2}|-",
  "BAR HEIGHT 32NDS" = "[0-9]{2}",
  "BAR HEIGHT INCHES" = "[0-9]{1}\\.[0-9]{2}",
  "FLAT PLATE" = "[0-9]{1,2}|-",
  "MAX LOAD & INFL. (LBS @ PSI)" = "[0-9]{3,5} @ [0-9]{2}"
)

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

col_names <- names(tyre_regex)

# add the tyre name to the lines
add_tyre_name <- function(x,tyre_regex,stop_words) {
  i <- str_detect(x,tyre_regex$`TIRE NAME`)
  tyre_name <- str_trim(str_remove_all(x[i],paste0(stop_words,collapse = "|")))
  x <- paste(c(NA,tyre_name)[cumsum(i)+1],x,sep = "   ")
  return(x)
}

# make the columns up to 17 by checking the order and adding "-" where necessary
add_missing_cols <- function(x,tyre_regex) {
  # remove bad columns
  if(!str_detect(x[3],tyre_regex$`MATERIAL NUMBER`)) { # Check material is correct
    if(str_detect(x[2],"[A-Z]")) { # see farm tire I-1 pg 117
      x <- x[-2]
    }
  }
  if(length(x)==17) return(x)
  if(!str_detect(x[6],tyre_regex$`SPEED SYMBOL RATING`)) { # check if speed rating is in column 6
    if(str_detect(x[4],tyre_regex$`LOAD INDEX`)) { # check if Load index
      x <- append(x,"-",4) # add after load index
    } else {
      x <- append(x,"-",3) # add after ply
    }
  }
  if(length(x)==17) return(x)
  if(!str_detect(x[15],tyre_regex$`BAR HEIGHT INCHES`)) { # check that the bar height in inches is in column 15
    if(str_detect(x[12],tyre_regex$`ROLLING CIRCUM.`)) { # check if rolling circum
      x <- append(x,"-",12) # add circum. index
    } else {
      x <- append(x,"-",11) # add rolling circum.
    }
  }
  if(length(x)==17) return(x)
    if(str_detect(x[16],tyre_regex$`MAX LOAD & INFL. (LBS @ PSI)`)) {
      x <- append(x,"-",15) # add flat plate
    }
  if(length(x)!=17) error("Additional Columns Required")
  return(x)
}

# extract the lines with tyre sizes and split columns
get_tyre_sizes <- function(x,tyre_regex,stop_words) {
  x %>%
    add_tyre_name(tyre_regex,stop_words) %>%
    subset(str_detect(.,paste0(tyre_regex$`TIRE SIZE`,"\\s+[0-9]"))) %>% # so it doesn't capture page numbers
    str_remove_all(paste0(stop_words,collapse = "|")) %>%
    str_trim() %>%
    str_split("\\s{2}\\s+") %>%
    lapply(str_replace_all,"\\s+"," ") %>%
    lapply(add_missing_cols, tyre_regex)
}

make_dataframe <- function(x) {
  as_tibble(matrix(unlist(x),
                   nrow=length(x),
                   byrow=TRUE,
                   dimnames = list(NULL,col_names)))
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
fsAg_data <- lapply(fsAg_data, get_tyre_sizes,tyre_regex,stop_words)

# make a tibble(data.frame)
fsAg_data <- lapply(fsAg_data, make_dataframe)

#####
# TEST and ERRORS
fsAg_data$tractor <- fsAg_data$tractor %>%
  mutate(`ROLLING CIRCUM.` = replace(`ROLLING CIRCUM.`,
                                     `MATERIAL NUMBER` == "003779",
                                     "186")) %>%
  mutate(`ROLLING CIRCUM. INDEX` = replace(`ROLLING CIRCUM. INDEX`,
                                     `MATERIAL NUMBER` == "003779",
                                     "-"))

mapply(testthat::expect_match,fsAg_data$tractor,tyre_regex)

fsAg_data$tractor$`ROLLING CIRCUM.`
