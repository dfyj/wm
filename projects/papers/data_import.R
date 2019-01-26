library(readr)
path <- "D:/F/graduate-papers/data2019/ST.csv"
df <- rio::import(path) %>%
  mutate(ANN_DT= lubridate::as_date(as.character(ANN_DT), "%Y%m%d")) %>%
  mutate(ANN_DT = as.character(ANN_DT))

df %>%
  rio::export("D:/F/graduate-papers/data2019/ST-test.csv")


# 申万行业统计 ------------------------------------------------------------------
path <- "D:/F/graduate-papers/data2019/SWIndustriesClass.csv"
df <- path %>% rio::import() %>%
  filter(CUR_SIGN == 1) %>%
  mutate(SW_IND_CODE = as.character(SW_IND_CODE), IND_MATCH = SW_IND_CODE) %>%
  select(SW_IND_CODE, IND_MATCH) %>%
  unique()

path1 <- "D:/F/graduate-papers/data2019/SW_NAME.xlsx"
df_all_code <-
  path1 %>%
  rio::import() %>%
  select(SW_IND_CODE = 三级行业代码, SW_IND_NAME = SW三级行业) %>%
  left_join(df, by = "SW_IND_CODE")

df_all_code %>%
  rio::export("D:/F/graduate-papers/data2019/ind_match.xlsx")


test <- readClipboard()


# attention data import ---------------------------------------------------

path <- "D:/F/graduate-papers/data2019/data-test(2019-01-20)/补充关键词2019-01-21.csv"

df <-
  path %>%
  rio::import()

df %>%
  select(keyword = 关键词, date = 时间, index = 搜索指数) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2011-01-01") &
           (date <= as.Date("2018-12-31"))) %>%
  spread(keyword, index) %>%
  rio::export("D:/F/graduate-papers/data2019/third_level_index_complement.xlsx")


# 申万行业数据 ------------------------------------------------------------------

path <- "D:/F/graduate-papers/data2019/SWIndustriesClass.csv"
SW_ind <-
  path %>%
  rio::import()

SW_ind_current <-
  SW_ind %>%
  filter(CUR_SIGN == 1) %>%
  select(SW_IND_CODE) %>%
  mutate(SW_IND_CODE = as.numeric(SW_IND_CODE)) %>%
  unique()

SW_ind_id <- "D:/F/graduate-papers/data2019/ind-att-data/申银万国行业分类标准(2014版)2019.xlsx" %>%
  rio::import() %>%
  select(wind中行业代码) %>%
  mutate(wind中行业代码 = as.numeric(wind中行业代码)) %>%
  na.omit()

SW_ind_id$wind中行业代码 %>%
  setdiff(SW_ind_current$SW_IND_CODE)

SW_ind_current$SW_IND_CODE %>%
  setdiff(SW_ind_id$wind中行业代码)

SW_ind %>%
  mutate(SW_IND_CODE = as.character(SW_IND_CODE)) %>%
  filter(CUR_SIGN == 1) %>%
  filter(SW_IND_CODE %in% c("6106020400", "6109040100", "6109040200", 6122020100,
           "6120010100", "6105020100", "6106010300", "6107010300",
           "6107010400", "6105020200"))
