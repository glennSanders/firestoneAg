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

# replace the stars
fsAg_data <- lapply(fsAg_data, str_replace_all,"[^[:ascii:]]","*")

# split the data into lines
fsAg_data <- lapply(fsAg_data, function(x) lapply(x, str_split, "\\r\\n", simplify = TRUE))
# remove empty lines
fsAg_data <- lapply(fsAg_data, function(x) lapply(x, function(y) y[!str_detect(y,"^\\s+$|^$")]))

### align Columns
align_cols <- function(x,align_dir = c("left","right"),ncol = NULL) {
  i <- str_locate_all(x,"[^\\s]+")
  i_end <- sapply(i, function(x) x[,2])
  n <- max(sapply(i_end, length))
  list <- mapply(function(x, align_dir, n) switch(align_dir,
                                                        left = x[1:n],
                                                        right = c(NA_integer_[0:(n-length(x))],x)),
                 i_end,align_dir,MoreArgs = list(n=n), SIMPLIFY = FALSE)
  col_list <- split(unlist(list),1:n)
  end_posn <- unname(sapply(col_list,function(x) as.integer(tail(names(sort(table(x))),1))))
  i_end_new <- mapply(function(x, align_dir, n, end_posn) switch(align_dir,
                                                                   left = end_posn[1:length(x)],
                                                                   right = end_posn[(n-length(x)+1):n]),
                  i_end,align_dir,MoreArgs = list(n=n, end_posn=end_posn), SIMPLIFY = FALSE)
  str_parts <- str_extract_all(x,"[^\\s]+")
  # take an index and only change the indices required
  if (!is.null(ncol)) {
    i_end <- mapply(function(i_end,i_end_new,align_dir,ncol) {
      l <- length(i_end)
      ncol <- min(l,ncol)
      switch(align_dir,
             right = i_end[(l-ncol+1):l] <- i_end_new[(l-ncol+1):l],
             left = i_end[1:ncol] <- i_end_new[1:ncol])
      return(i_end)
    },i_end,i_end_new, align_dir, ncol, SIMPLIFY = FALSE)
  } else {
    i_end <- i_end_new
  }

  mapply(function(i_end,str_parts) {
    # add extra fro non-ascii
    n <- c(i_end[1],diff(i_end)) + str_count(str_parts,"[^[:ascii:]]")*2
    do.call(sprintf, c(list(paste0("%",paste0(n,collapse = "s%"),"s")),
                       str_parts))
  },i_end,str_parts)
}

### TEST

x <- fsAg_data$`TABLE A`[[1]]

# find the LI and symbol lines and align them to the left
i <- str_detect(x,"\\bsymbol\\b| LI ")
x[i] <- align_cols(x[i],"left")

# the SINGLES, DOUBLES, TRIPLES line are aligned to the left and psi to right
i <- str_detect(x,"SINGLES|DUALS|TRIPLES|(psi\\s+[0-9])")
x[i] <- align_cols(x[i],c("right",rep("left",sum(i)-1)),c(14,rep(15,sum(i)-1)))

# detect table
i <- str_detect(x,"SINGLES|DUALS|TRIPLES|(psi\\s+[0-9])|\\bsymbol\\b| LI ")
y <- paste0(x[i],collapse = "\n")

# create data.frame
tb <- suppressWarnings(read_fwf(y,fwf_empty(y)))
