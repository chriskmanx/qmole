/* log_level : tags and macros */
#define MLTAG_ERROR	((value)(-250084440*2+1))
#define MLTAG_CRITICAL	((value)(138255519*2+1))
#define MLTAG_WARNING	((value)(161459772*2+1))
#define MLTAG_MESSAGE	((value)(434440615*2+1))
#define MLTAG_INFO	((value)(813432942*2+1))
#define MLTAG_DEBUG	((value)(-720532941*2+1))
#define MLTAG_FLAG_RECURSION	((value)(63386543*2+1))
#define MLTAG_FLAG_FATAL	((value)(930452433*2+1))

extern const lookup_info ml_table_log_level[];
#define Val_log_level(data) ml_lookup_from_c (ml_table_log_level, data)
#define Log_level_val(key) ml_lookup_to_c (ml_table_log_level, key)

/* io_condition : tags and macros */
#define MLTAG_IN	((value)(16357*2+1))
#define MLTAG_OUT	((value)(3947630*2+1))
#define MLTAG_PRI	((value)(3996679*2+1))
#define MLTAG_ERR	((value)(3449669*2+1))
#define MLTAG_HUP	((value)(3599523*2+1))
#define MLTAG_NVAL	((value)(869277491*2+1))

extern const lookup_info ml_table_io_condition[];
#define Val_io_condition(data) ml_lookup_from_c (ml_table_io_condition, data)
#define Io_condition_val(key) ml_lookup_to_c (ml_table_io_condition, key)

/* normalize_mode : tags and macros */
#define MLTAG_DEFAULT	((value)(462924961*2+1))
#define MLTAG_DEFAULT_COMPOSE	((value)(603364372*2+1))
#define MLTAG_ALL	((value)(3249409*2+1))
#define MLTAG_ALL_COMPOSE	((value)(725607540*2+1))

extern const lookup_info ml_table_normalize_mode[];
#define Val_normalize_mode(data) ml_lookup_from_c (ml_table_normalize_mode, data)
#define Normalize_mode_val(key) ml_lookup_to_c (ml_table_normalize_mode, key)

/* locale_category : tags and macros */
#define MLTAG_COLLATE	((value)(-315523258*2+1))
#define MLTAG_CTYPE	((value)(-878536579*2+1))
#define MLTAG_MESSAGES	((value)(243493068*2+1))
#define MLTAG_MONETARY	((value)(729787149*2+1))
#define MLTAG_NUMERIC	((value)(731166381*2+1))
#define MLTAG_TIME	((value)(935171085*2+1))

extern const lookup_info ml_table_locale_category[];
#define Val_locale_category(data) ml_lookup_from_c (ml_table_locale_category, data)
#define Locale_category_val(key) ml_lookup_to_c (ml_table_locale_category, key)

