con <- DBI::dbConnect(RMySQL::MySQL(),
                      host = '127.0.0.1',
                      port = 3306 ,
                      local_infile = 1,
                      username = "root",
                      password = "wgs491982")
df <- tribble(
  ~x,    ~y,
  1,     "牟合1号",
  2,     "致远稳健3号",
  3,     "锐天量化1号"
)



copy_to(con,)

library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
con <- dbConnect(odbc::odbc(), "Oracle DB")
con <- DBI::dbConnect(odbc::odbc(),
                      dsn = "re",
                      UID = "root",
                      PWD = "wgs491982",
                      database = "tests",
                      encoding = "gbk"
)
tbl(con, "test_encoding_copy01") %>% filter(y == "牟合资产")
