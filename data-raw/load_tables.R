library(tidyverse)
library(stringr)

stop_words <- c(
  "TRACTOR",
  "HARVEST",
  "SPRAYER",
  "IRRIGATION",
  "FRONTS &",
  "IMPLEMENTS",
  "FORESTRY",
  "DISCONTINUED",
  "LOAD &",
  "INFLATION",
  "TABLES",
  "GENERAL",
  "FARM TIRE",
  "INFORMATION",
  "INDEX"
)

make_dataframe <- function(x) {
  as_tibble(matrix(unlist(x),
                   nrow=length(x),
                   byrow=TRUE,
                   dimnames = list(NULL,col_names)))
}

# load raw data
fsAg_data <- readRDS("data-raw/fsAg_data")

# Get table contents and the page numbers
table_pages <- fsAg_data[[154]] %>%
  str_split("\\r\\n", simplify = TRUE) %>%
  `[`(str_detect(.,"[A-Z][0-4]?\\s+[0-9]{3}(-[0-9]{3})?\\s")) %>%
  str_remove_all(paste0(stop_words,collapse = "|")) %>%
  str_trim %>%
  str_split("\\s\\s\\s+") %>%
  unlist %>%
  matrix(ncol = 3,
         byrow = TRUE,
         dimnames = list(NULL,c("TABLE","PAGE","DESCRIPTION"))) %>%
  as_tibble() %>%
  mutate(TABLE = paste("TABLE",TABLE)) %>%
  rowwise() %>%
  mutate(PAGE = list(seq(str_extract(PAGE,"^[0-9]{3}"),
                         str_extract(PAGE,"[0-9]{3}$"))+1))

# load raw data
fsAg_data <- readRDS("data-raw/fsAg_data")

# Name the pages the table name
names(table_pages$PAGE) <- table_pages$TABLE

# split the data into groups by page number
fsAg_data <- lapply(table_pages$PAGE, function(x,data) data[x],fsAg_data)

# remove stop words for every page
fsAg_data <- lapply(fsAg_data,str_remove_all,paste0(stop_words,collapse = "|"))

# split the data into lines
fsAg_data <- lapply(fsAg_data, function(x) lapply(x, str_split, "\\r\\n", simplify = TRUE))
# remove empty lines
fsAg_data <- lapply(fsAg_data, function(x) lapply(x, function(y) y[!str_detect(y,"^\\s+$|^$")]))

# make lines the same length
pad_line <- function(x) {
  x <- str_pad(x,max(nchar(x)),"right")
  return(x)
}
fsAg_data <- lapply(fsAg_data, function(x) lapply(x, pad_line))

### adjust spacing of lines

# The white space between LI and symbol rows lines up
x <- fsAg_data$`TABLE A`[[1]]
# for each column find most common start
# left align and right align gaps
# line_subset <- x[str_detect(x,line_regex)]

common_end_locs <- function(x,align_dir = c("left","right")) {
  #TODO: Choose how many columns to change position of
  # align_dir <- c("right",rep("left",length(i_end)-1))
  # i <- str_detect(x,"SINGLES|DUALS|TRIPLES|(psi\\s+[0-9])")
  # x <- x[i]

  align_dir <- "left"
  i <- str_detect(x,"\\bsymbol\\b| LI ")
  x <- x[i]

  i <- str_locate_all(x,"[^\\s]+")
  i_end <- sapply(i, function(x) x[,2])
  n <- max(sapply(i_end, length))
  list <- mapply(function(x, align_dir, n) switch(align_dir,
                                                        left = x[1:n],
                                                        right = c(NA_integer_[0:(n-length(x))],x)),
                 i_end,align_dir,MoreArgs = list(n=n), SIMPLIFY = FALSE)
  col_list <- split(unlist(list),1:n)
  end_posn <- unname(sapply(col_list,function(x) as.integer(tail(names(sort(table(x))),1))))
  i_end <- mapply(function(x, align_dir, n, end_posn) switch(align_dir,
                                                                   left = end_posn[1:length(x)],
                                                                   right = end_posn[(n-length(x)+1):n]),
                  i_end,align_dir,MoreArgs = list(n=n, end_posn=end_posn), SIMPLIFY = FALSE)
  str_parts <- str_extract_all(x,"[^\\s]+")

  mapply(function(i_end,str_parts) {
    # add extra fro non-ascii
    n <- c(i_end[1],diff(i_end)) + str_count(str_parts,"[^[:ascii:]]")*2
    do.call(sprintf, c(list(paste0("%",paste0(n,collapse = "s%"),"s")),
                       str_parts))
  },i_end,str_parts)
}

# find the LI and symbol lines and align them to the left
i <- str_detect(x,"\\bsymbol\\b| LI ")
x[i] <- common_end_locs(x[i],"left")

# the SINGLES, DOUBLES, TRIPLES line are aligned to the left
i <- str_detect(x,"SINGLES|DUALS|TRIPLES")
x[i] <- common_end_locs(x[i],"left")

### TEST
fwf_empty(paste0(y,collapse = "\n"))

View(read_fwf(paste0(y,collapse = "\n"),fwf_empty(paste0(y,collapse = "\n"),n=5L)))
data.table::fread(paste0(y,collapse = "\n"),fill = TRUE)
