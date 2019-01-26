con <- DBI::dbConnect(RMySQL::MySQL(),
                 host = "127.0.0.1",
                 port = 3306,
                 username = "root",
                 password = "wgs491982",
                 )
dbExecute(con, "CREATE DATABASE TESTS")
dbGetQuery(con, "SHOW DATABASES")

con1 <- DBI::dbConnect(RMySQL::MySQL(),
                      host = "127.0.0.1",
                      port = 3306,
                      username = "root",
                      password = "wgs491982",
                      dbname = "TESTS",
                      encoding = "gbk",
                      local_infile = 1
)


dbGetQuery(con1, "SHOW TABLES")

df <- tribble(
  ~x,           ~y,
  1,            "牟合资产",
  2,            "华天锐量1号",
  3,            "致远稳健a"
)
df_test <- df
df_test$y <- df_test$y %>% iconv("CP936", "UTF8")
dbWriteTable(con1, "test_encoding_dbwrite", df_test,
             row.names = FALSE,
             overwrite = TRUE,
             temporary = FALSE)

copy_to(con1, df, "test_encoding_copy01",
        row.names = FALSE,
        overwrite = TRUE,
        temporary = FALSE,
        eol = "\r\n")
dbSendQuery(con1,'SET NAMES gbk')


test <-
  tbl(con1, "test_encoding_copy01") %>%
  collect()
test$y <- iconv(test$y, "UTF-8", "GBK")
test$y <- iconv(test$y, "UTF-8", "CP936")
s <-  paste("select * from test_encoding_copy01 where y like '%",
         iconv("牟合","CP936","UTF8"),"%'",sep = "")
dbGetQuery(con1, sql(s)) %>% iconv("UTF-8", "GBK")
dbDisconnect(con1)

# add column --------------------------------------------------------------

dbExecute(con1 , "alter table test_encoding_copy01 add column add_column varchar (20)")

UPDATE table1 t1
INNER JOIN Tabel2 t2 ON t1.id = t2.id
SET t1.age = t2.age;

# test odbc connection ----------------------------------------------------


con2 <- DBI::dbConnect(odbc::odbc(),
                       dsn = "re",
                       UID = "root",
                       Pwd = "wgs491982",
                       database = "TESTS",
                       encoding = "gbk"
)
# dbDisconnect(con2)

dbWriteTable(con2, "test_encoding_dbwrite", df,
             overwrite = TRUE,
             row.names = FALSE,
             temporary = TRUE
             # overwrite = TRUE,

             )
             # temporary = TRUE)
dbWriteTable(con, "test_encoding_dbwrite", df,
             overwrite = TRUE, row.names = FALSE,
             temporary = TRUE)
copy_to(con2, df, "test_encoding_copy",
        row.names = FALSE,
        overwrite = TRUE,
        temporary = FALSE)
tbl(con, "test-encoding-dbwrite")
tbl(con2, "test_encoding_dbwrite")

con3 <- RSQLite::dbConnect(RSQLite::SQLite(), "D:/F/OSC/db/sqlite/tests")
copy_to(con3, df, "test_encoding_copy",
        row.names = FALSE,
        overwrite = TRUE,
        temporary = FALSE)
tbl(con3, "test_encoding_copy")
