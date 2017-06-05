/* pango_style : tags and macros */
#define MLTAG_NORMAL	((value)(-487842265*2+1))
#define MLTAG_OBLIQUE	((value)(731898001*2+1))
#define MLTAG_ITALIC	((value)(954653424*2+1))

extern const lookup_info ml_table_pango_style[];
#define Val_pango_style(data) ml_lookup_from_c (ml_table_pango_style, data)
#define Pango_style_val(key) ml_lookup_to_c (ml_table_pango_style, key)

/* pango_weight : tags and macros */
#define MLTAG_ULTRALIGHT	((value)(-17845718*2+1))
#define MLTAG_LIGHT	((value)(-219494218*2+1))
#define MLTAG_BOLD	((value)(735857029*2+1))
#define MLTAG_ULTRABOLD	((value)(207112849*2+1))
#define MLTAG_HEAVY	((value)(581376743*2+1))

extern const lookup_info ml_table_pango_weight[];
#define Val_pango_weight(data) ml_lookup_from_c (ml_table_pango_weight, data)
#define Pango_weight_val(key) ml_lookup_to_c (ml_table_pango_weight, key)

/* pango_variant : tags and macros */
#define MLTAG_SMALL_CAPS	((value)(-342156807*2+1))

extern const lookup_info ml_table_pango_variant[];
#define Val_pango_variant(data) ml_lookup_from_c (ml_table_pango_variant, data)
#define Pango_variant_val(key) ml_lookup_to_c (ml_table_pango_variant, key)

/* pango_stretch : tags and macros */
#define MLTAG_ULTRA_CONDENSED	((value)(-275178668*2+1))
#define MLTAG_EXTRA_CONDENSED	((value)(1073678264*2+1))
#define MLTAG_CONDENSED	((value)(637685127*2+1))
#define MLTAG_SEMI_CONDENSED	((value)(-222743754*2+1))
#define MLTAG_SEMI_EXPANDED	((value)(151930474*2+1))
#define MLTAG_EXPANDED	((value)(-749428423*2+1))
#define MLTAG_EXTRA_EXPANDED	((value)(985921576*2+1))
#define MLTAG_ULTRA_EXPANDED	((value)(546524172*2+1))

extern const lookup_info ml_table_pango_stretch[];
#define Val_pango_stretch(data) ml_lookup_from_c (ml_table_pango_stretch, data)
#define Pango_stretch_val(key) ml_lookup_to_c (ml_table_pango_stretch, key)

/* pango_scale : tags and macros */
#define MLTAG_XX_SMALL	((value)(-302482200*2+1))
#define MLTAG_X_SMALL	((value)(-996064704*2+1))
#define MLTAG_SMALL	((value)(-44488537*2+1))
#define MLTAG_MEDIUM	((value)(826998901*2+1))
#define MLTAG_LARGE	((value)(-307663973*2+1))
#define MLTAG_X_LARGE	((value)(888243508*2+1))
#define MLTAG_XX_LARGE	((value)(-565657636*2+1))
#define MLTAG_CUSTOM	((value)(-233491535*2+1))
/* pango_underline : tags and macros */
#define MLTAG_NONE	((value)(868932280*2+1))
#define MLTAG_SINGLE	((value)(-341568888*2+1))
#define MLTAG_DOUBLE	((value)(-447883503*2+1))
#define MLTAG_LOW	((value)(3797108*2+1))

extern const lookup_info ml_table_pango_underline[];
#define Val_pango_underline(data) ml_lookup_from_c (ml_table_pango_underline, data)
#define Pango_underline_val(key) ml_lookup_to_c (ml_table_pango_underline, key)

/* pango_wrap_mode : tags and macros */
#define MLTAG_WORD	((value)(968739274*2+1))
#define MLTAG_CHAR	((value)(746596054*2+1))
#define MLTAG_WORD_CHAR	((value)(944880811*2+1))

extern const lookup_info ml_table_pango_wrap_mode[];
#define Val_pango_wrap_mode(data) ml_lookup_from_c (ml_table_pango_wrap_mode, data)
#define Pango_wrap_mode_val(key) ml_lookup_to_c (ml_table_pango_wrap_mode, key)

/* pango_ellipsize_mode : tags and macros */
#define MLTAG_START	((value)(33139778*2+1))
#define MLTAG_MIDDLE	((value)(-18776235*2+1))
#define MLTAG_END	((value)(3448763*2+1))

extern const lookup_info ml_table_pango_ellipsize_mode[];
#define Val_pango_ellipsize_mode(data) ml_lookup_from_c (ml_table_pango_ellipsize_mode, data)
#define Pango_ellipsize_mode_val(key) ml_lookup_to_c (ml_table_pango_ellipsize_mode, key)

