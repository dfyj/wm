localeToCharset()
enc2native()


con <- DBI::dbConnect(RMySQL::MySQL(),
                       host = "127.0.0.1",
                       port = 3306,
                       username = "root",
                       password = "wgs491982",
                       dbname = "TESTS",
                       encoding = "gbk"
                       # DBMSencoding = "gbk"
)
dbExecute(con, "SET NAMES gbk")
dbSendQuery(con, 'set character set "utf8"')

df <- tribble(
  ~x,           ~y,
  1,            "牟合资产",
  2,            "华天锐量1号",
  3,            "致远稳健a"
)

dbWriteTable(con, "test_encoding_dbwrite", df,
             row.names = FALSE,
             overwrite = TRUE,
             temporary = FALSE)

copy_to(con, df, "test_encoding_copy01",
        row.names = FALSE,
        overwrite = TRUE,
        temporary = FALSE)
tbl(con, "test_encoding_copy01")


test <- tbl(con, "test_encoding_copy01", encoding = "CP936") %>%
  collect()
test$y <- test$y %>%
  iconv("UTF-8", "CP936")
test
install.packages("RSQLServer")
