options(max.print  = 2e5)
options(digits = 3)
options(scipen = 999)

suppressPackageStartupMessages(
 suppressWarnings({
#    library(docopt)
#    library(data.table)
    library(foreach)
    library(iterators)
#    library(magrittr)
 })
)

#library(docopt)
library(data.table)
library(foreach)
library(iterators)
library(magrittr)

load.csv <- function(path)
{
  DT <- fread(path, stringsAsFactor=T, na.strings=c('NA', ''))
  setnames(DT, names(DT), tolower(names(DT)))
  DT
}


printf    <- function(...) cat(sprintf(...))
lprintf   <- function(...) cat(sprintf(...), file=stderr())
exit      <- function(code) quit(save='no', status=code)
'%notin%' <- function(x,y)!('%in%'(x,y))
#`%|%` <- function (x, y) y(x)
twoup <- function() { par(mfrow=c(2,2)) }
oneup <- function() { par(mfrow=c(1,1)) }

# Docopt helpers

usage <- function(u, default)
{
  if (interactive())
    return(docopt(u, args=default))
  else
    return(docopt(u))
}


# data.table helpers

# HeadTail mimics the shell script of the same name
ht <- function(dt, n = 10)
{
  dt[, head(colnames(dt), n), with=F]
}

# Split a string on spaces into a character vector
cc <- function(str)
{
  unlist(strsplit(str, '\\s+'))
}

# return dimensions with the variable name
dimx <- function(thing)
{
  varname <- deparse(substitute(thing))
  return(sprintf("%s: [%d rows x %d cols]", varname, dim(thing)[[1]], dim(thing)[[2]]))
}

# Same but without name
dimy <- function(thing)
{
  varname <- deparse(substitute(thing))
  return(sprintf("[%d rows x %d cols]", dim(thing)[[1]], dim(thing)[[2]]))
}


assert.file.exists <- function(fn)
{
	if (!file.exists(fn))
	{
		printf("*** %s file does not exist\n", fn)
		stopifnot(FALSE)
	}
}

qread <- function(fn, ...)
{
  sink(stderr(), type='output')
  DT <- fread(input=fn, showProgress=T,...)
  sink(NULL, type='output')
  return(DT)
}

lread <- function(filepath, ...)
{
  assert.file.exists(filepath)
  csv <- qread(filepath, ...)
  setnames(csv, names(csv), tolower(names(csv)))
  return(csv)
}


# Common enough to move here
read.csv.lowercase <- function(filepath)
{
  assert.file.exists(filepath)
  csv <- qread(filepath, stringsAsFactors = F)
  setnames(csv, names(csv), tolower(names(csv)))
  return(csv)
}

# Ternary operator from C
# C: (1==1) ? "same" : "different"
# R: (1==1) %?% "same" %:% "different
# R: ifelse(1==1, "same", "different")
# `%?%` <- function(x, y) list(x = x, y = y)
# `%:%` <- function(xy, z) if(xy$x) xy$y else z

# Like a data.frame iterator but:
# 1. goes by row (instead of useless default by column)
# 2. .wraps the argument in unique()
# 3. if it's a single column, return a data.frame instead of vector
iunique <- function(x) {
  col1 <- colnames(x)[1]
  it <- iter(unique(x), by='row')
  nextEl <- function() {
    n <- nextElem(it)

    if (!is.data.frame(n)) {
      n <- data.frame(n)
      names(n) <- col1
    }
    data.table(n)
  }
  obj <- list(nextElem=nextEl)
  class(obj) <- c('iterdt', 'abstractiter', 'iter')
  obj
}

# Custom foreach iterator that auto combines with rbind, and finalizes with data.table
foreach.dt <- function(...) { foreach(..., .combine='rbind', .final=data.table) }
foreach.r  <- function(...) { foreach(..., .combine='rbind') }

roundUp <- function(x) 10^ceiling(log10(x))
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

# Assumes YMD stored is integer like 19970526
# which would be May 26, 1997
as.ymd <- function(day)
{
  y <- day %/% 10000
  m <- (day %% 10000) %/% 100
  d <- day %% 100
  as.IDate(paste(y,m,d,sep='-'))
}

clamp <- function(n, .min, .max)
{
  pmax(pmin(n, .max), .min)
}


## data.table helpers

drop.cols.keep.first <- function(DT, basename)
{
  p <- paste0(basename, '([2-9]|..)')
  drop.cols.by.pattern(DT, p)
}

drop.cols.by.pattern <- function(DT, pattern)
{
  DT[, (grep(pattern, colnames(DT), value=T)) := NULL]
}

drop.cols <- function(DT, cols)
{
  suppressWarnings(DT[, (cols) := NULL])
}

keep.cols <- function(DT, cols)
{
  DT[, (cols), by=F]
}

has.cols <- function(DT, cols)
{
  all(cols %in% colnames(DT))
}

round.cols <- function(DT, cols, .round=4)
{
  DT[, (cols) := round(.SD, .round), .SDcols=cols]
}

put.first <- function(DT, first)
{
  last = setdiff(colnames(DT), first)
  setcolorder(DT, c(first, last))
}

is.empty <- function(DT)
{
  assert_that(data.table::is.data.table(DT))
  nrow(DT) == 0
}

has.rows <- function(DT)
{
  assert_that(data.table::is.data.table(DT))
  nrow(DT) > 0
}

put.last <- function(DT, last)
{
  first = setdiff(colnames(DT), last)
  setcolorder(DT, c(first, last))
}

