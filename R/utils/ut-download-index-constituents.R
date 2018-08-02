
# index list and constituent as of dates ----------------------------------------

index_list <- read.table(file = "clipboard", sep = "\t",
                         fileEncoding = "native.enc", colClasses = "character", header=TRUE, stringsAsFactors = FALSE)

index_list <-
  index_list %>%
  as_tibble() %>%
  rename(code = 1) %>%
  mutate(
    code = str_c(code, ".CSI"),
    name = wsd_xc(code, "sec_name")
  )

index_list %>%
  rds_write(rds_path("index/index_list/amac"))

index_list <- rds_read(rds_path("index/index_list/amac"))

date_list <- c(
  seq.Date(as.Date("2015-12-31"), as.Date("2017-12-31"), by = "year"),
  Sys.Date()) %>% as.character()

grid <- crossing(index = index_list$code, date = date_list)


# import constituent data -------------------------------------------------

f <- function(index, date){
  wset_index_const(index, date) %>%
    mutate(date = date, index = index)
}

df <- grid[, ] %>%
  pmap_df(f)

df %>% rds_write(rds_path("index/index_constituents/amac"))

df <- rds_read(rds_path("index/index_constituents/amac"))

# get latest available sector mapping -------------------------------------

lastest_sector_mapping <-
  df %>%
  group_by(code) %>%
  summarise(index_code = last(index))

sec_map <-
lastest_sector_mapping %>%
  mutate(name = wsd_xc(code, "sec_name")) %>%
  left_join(set_names(index_list, c("index_code", "index_name"))) %>%
  select(code, name, index_code, index_name)

sec_map %>%
  set_names(c("股票代码", "股票简称", "指数代码", "指数简称")) %>%
  rio::export("foo.xlsx")
  write.csv(file = "foo.csv", fileEncoding = "native.enc", row.names = FALSE)

sec_map %>%
  filter(code %in% c("002310.SZ", "002252.SZ"))
