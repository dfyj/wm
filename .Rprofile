# options(datatable.WhenJisSymbolThenCallingScope = TRUE)

.First <- function() {
  # Sys.setenv(LANGUAGE = "en")
  setwd("./R")
  options("encoding" = 'UTF-8')
  library(stringr)
  library(logging)
  library(quantmod)
  library(dygraphs)
  library(xts)
  library(WindR)
  library(tidyr)
  library(dplyr)
  library(purrr)
  library(ggplot2)
  # library(plotly)
  library(PerformanceAnalytics)
  library(bizdays)
  logging::basicConfig()

  load_calendar(file.path("~/.R/calendars/SSE.cal"))
  bizdays.options$set(default.calendar='SSE')
  w.start()
}
