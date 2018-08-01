.DATE_FILE_NAME_FORMAT <- '%Y%m%d'
.full_code_pattern <- '^([0-9]{6}).(S[H|Z])$'
.six_digits_code_pattern <- '^([0-9]{6})'
.datetime_col <- 'DATETIME'
.date_col_quo <- rlang::quo(DATETIME)

.wsi_default_time_range <- c(start_time = "9:30", end_time = "15:30")
.md_root <- 'C:/db/rds/md'
.mu_id_list <- c('mu01',
                 'mutest01',
                 'mutest01')


.default_wsd_options <- 'currencyType=;unit=1;Period=M;Days=Alldays'
.descriptor_wsd_config_path <- 'c:/db/rds/descriptors/descriptor_wsd_config.csv'

.bt_root <- 'c:/db/rds/bt'

.frequency_config <- tibble::tribble(
  ~freq,       ~annual_factor,    ~xts_apply_func,
  "daily",     252,               xts::apply.daily,
  "weekly",    52,                xts::apply.weekly,
  "monthly",   12,                xts::apply.monthly,
  "quarterly", 4,                 xts::apply.quarterly,
  "yearly",    1,                 xts::apply.yearly
)

.frequency_to_annual_factor <-
  .frequency_config$annual_factor %>% setNames(.frequency_config$freq)

.frequency_to_xts_apply_func <-
  .frequency_config$xts_apply_func %>% setNames(.frequency_config$freq)

.frequency_list <-
  names(.frequency_to_xts_apply_func)

.significance_config <- list(
  cutpoints = c(0, 0.001, 0.01, 0.05, 1),
  symbols = c("***", "**", "*", " ")
)

# .significance_config <- list(
#   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
#   symbols = c("***", "**", "*", ".", " ")
# )

.ohlc_names <- c("open", "high", "low", "close")
.ohlcv_names <- c(.ohlc_names, "volume")
