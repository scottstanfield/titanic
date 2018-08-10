################
# Global Options
################

options(max.print = 2e5)
options(digits = 5)
options(scipen = 999)

########################
# Basic common libraries
########################

suppressPackageStartupMessages(
  suppressWarnings({
    library(docopt)
    library(data.table)
    library(assertthat)
    library(foreach)
    library(iterators)
    library(magrittr)
  })
)

#########################
# Simple common functions
#########################

printf    <- function(...) cat(sprintf(...))
lprintf   <- function(...) cat(sprintf(...), file=stderr())
exit      <- function(code) quit(save='no', status=code)
'%notin%' <- function(x,y)!('%in%'(x,y))


#################################################
# Docopt library for managing commandline options
#################################################

usage <- function(u, default)
{
  if (interactive())
    return(docopt(u, args=default))
  else
    return(docopt(u))
}

# HeadTail mimics the shell script of the same name
ht <- function(dt, n = 10)
{
  dt[, head(colnames(dt), n), with=F]
}

dimx <- function(thing)
{
  varname <- deparse(substitute(thing))
  return(sprintf("%s: [%d rows x %d cols]", varname, dim(thing)[[1]], dim(thing)[[2]]))
}

dimr <- function(thing)
{
  varname <- deparse(substitute(thing))
  races <- thing[, .N, by=raceid][, .N]
  printf('%s: [%d rows x %d cols] %d races\n', varname,
        dim(thing)[[1]], dim(thing)[[2]], races)
}


assert.file.exists <- function(fn)
{
  if (!file.exists(fn))
  {
    printf("*** %s file does not exist\n", fn)
    stopifnot(FALSE)
  }
}

####################
# Data.table helpers
####################

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
  csv <- qread(filepath, stringsAsFactors = T)
  setnames(csv, names(csv), tolower(names(csv)))
  return(csv)
}

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

clamp <- function(n, .min, .max)
{
  pmax(pmin(n, .max), .min)
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


