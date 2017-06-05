/* log_level : conversion table */
const lookup_info ml_table_log_level[] = {
  { 0, 8 },
  { MLTAG_DEBUG, G_LOG_LEVEL_DEBUG },
  { MLTAG_ERROR, G_LOG_LEVEL_ERROR },
  { MLTAG_FLAG_RECURSION, G_LOG_FLAG_RECURSION },
  { MLTAG_CRITICAL, G_LOG_LEVEL_CRITICAL },
  { MLTAG_WARNING, G_LOG_LEVEL_WARNING },
  { MLTAG_MESSAGE, G_LOG_LEVEL_MESSAGE },
  { MLTAG_INFO, G_LOG_LEVEL_INFO },
  { MLTAG_FLAG_FATAL, G_LOG_FLAG_FATAL },
};

/* io_condition : conversion table */
const lookup_info ml_table_io_condition[] = {
  { 0, 6 },
  { MLTAG_IN, G_IO_IN },
  { MLTAG_ERR, G_IO_ERR },
  { MLTAG_HUP, G_IO_HUP },
  { MLTAG_OUT, G_IO_OUT },
  { MLTAG_PRI, G_IO_PRI },
  { MLTAG_NVAL, G_IO_NVAL },
};

/* normalize_mode : conversion table */
const lookup_info ml_table_normalize_mode[] = {
  { 0, 4 },
  { MLTAG_ALL, G_NORMALIZE_ALL },
  { MLTAG_DEFAULT, G_NORMALIZE_DEFAULT },
  { MLTAG_DEFAULT_COMPOSE, G_NORMALIZE_DEFAULT_COMPOSE },
  { MLTAG_ALL_COMPOSE, G_NORMALIZE_ALL_COMPOSE },
};

/* locale_category : conversion table */
const lookup_info ml_table_locale_category[] = {
  { 0, 7 },
  { MLTAG_CTYPE, LC_CTYPE },
  { MLTAG_COLLATE, LC_COLLATE },
  { MLTAG_ALL, LC_ALL },
  { MLTAG_MESSAGES, LC_MESSAGES },
  { MLTAG_MONETARY, LC_MONETARY },
  { MLTAG_NUMERIC, LC_NUMERIC },
  { MLTAG_TIME, LC_TIME },
};

