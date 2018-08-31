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

x <- fsAg_data$`TABLE A`[[1]]

# find the location of SINGLES DUALS TRIPLES
i <- sapply(str_locate_all(x,"SINGLES|DUALS|TRIPLES"),function(x) x[1])
maximum_i <- max(i,na.rm = TRUE)
diff_i <- maximum_i-i
common_i <- as.integer(names(which.max(table(diff_i))))
diff_i[is.na(diff_i)] <- common_i
y <- paste0(sapply(diff_i, function(x) paste0(rep(" ",x),collapse = "")),x)

n_lines <- which.max(str_detect(y,"psi"))-1

z <- read_fwf(paste0(y,collapse = "\n"),
              col_positions = fwf_empty(paste0(y,collapse = "\n"),skip = n_lines),
              skip = n_lines)
z


# fread puts the LI row together
data.table::fread(paste0(y, collapse = "\n"), skip = "psi",
                  fill = TRUE,
                  strip.white = FALSE,
                  blank.lines.skip = TRUE)



# do own white space function
# remove head of characters
y <- x[-1:(-which.max(str_detect(x,"psi|kPa"))+1)]
# make strings of same length
y <- str_pad(y,max(nchar(y)),side = "right")
# find intersections of white space
i <- str_locate_all(y,"\\s+")
# find the white spaces common to all lines
j <- Reduce(intersect, lapply(i, function(x) unlist(apply(x,1,function(y) seq(y[1],y[2])))))
# find the locations with values
k <- setdiff(1:191,j)
# convert to a start and end value

s <- unname(sapply(split(k,cumsum(c(1,diff(k))>1)),function(x) c(min(x), max(x))))
posn <- list(`begin` = s[1,],
             end = s[2,],
             skip = 0L,
             col_names = paste("X",1:ncol(s)))


fwf_empty(paste0(y,collapse = "\n"))

View(read_fwf(paste0(y,collapse = "\n"),fwf_empty(paste0(y,collapse = "\n"),n=5L)))
data.table::fread(paste0(y,collapse = "\n"),fill = TRUE)

# remove the page seperations as there are multiple tables per page
fsAg_data <- lapply(fsAg_data, unlist, use.names= FALSE )

# Table A

# remove the stop words
data.table::fread(fsAg_data$`TABLE A`[c(19:30)]

# Try tabulizer
library(tabulizer)
x <- extract_tables("data-raw/2018 FIRESTONE AG DATA BOOK - FIRESTONE AGRICULTURE.pdf",
                    method = "lattice",
               pages = table_pages$PAGE[[1]])

extract_areas("data-raw/2018 FIRESTONE AG DATA BOOK - FIRESTONE AGRICULTURE.pdf",
              pages = table_pages$PAGE[[1]][1])

get_page_dims("data-raw/2018 FIRESTONE AG DATA BOOK - FIRESTONE AGRICULTURE.pdf",
              pages = table_pages$PAGE[[1]][1])

column_posns <- list(c(55,106,151,183,210,238,264,291,318,345,372,400,425,453,480,506,535,562))

extract_tables("data-raw/2018 FIRESTONE AG DATA BOOK - FIRESTONE AGRICULTURE.pdf",
              pages = table_pages$PAGE[[1]][1],
              columns = column_posns)


# load raw data
fsAg_data <- readRDS("data-raw/fsAg_data")

# split the data into groups by page number
fsAg_data <- lapply(table_pages$PAGE, function(x,data) data[x],fsAg_data)

# split the data into lines
fsAg_data <- lapply(fsAg_data, function(x) lapply(x, str_split, "\\r\\n", simplify = TRUE))
# remove the page seperations as there are multiple tables per page
fsAg_data <- lapply(fsAg_data, unlist, use.names= FALSE )

# start of table
table_start <- "LOAD LIMITS AT VARIOUS COLD INFLATION PRESSURES"
table_finish <- ""

# remove pages with `METRIC UNITS of MEASURE`
remove_metric_pages <- function(x) {
  x <- x[!str_detect(x,"METRIC UNITS OF MEASURE")]
  return(x)
}
x <- lapply(fsAg_data,remove_metric_pages)
