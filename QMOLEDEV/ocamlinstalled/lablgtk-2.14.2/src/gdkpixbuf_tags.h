/* colorspace : tags and macros */
#define MLTAG_RGB	((value)(4093677*2+1))

extern const lookup_info ml_table_colorspace[];
#define Val_colorspace(data) ml_lookup_from_c (ml_table_colorspace, data)
#define Colorspace_val(key) ml_lookup_to_c (ml_table_colorspace, key)

/* alpha_mode : tags and macros */
#define MLTAG_BILEVEL	((value)(-655932867*2+1))
#define MLTAG_FULL	((value)(780513679*2+1))

extern const lookup_info ml_table_alpha_mode[];
#define Val_alpha_mode(data) ml_lookup_from_c (ml_table_alpha_mode, data)
#define Alpha_mode_val(key) ml_lookup_to_c (ml_table_alpha_mode, key)

/* interpolation : tags and macros */
#define MLTAG_NEAREST	((value)(-442635202*2+1))
#define MLTAG_TILES	((value)(237188453*2+1))
#define MLTAG_BILINEAR	((value)(969098988*2+1))
#define MLTAG_HYPER	((value)(803910220*2+1))

extern const lookup_info ml_table_interpolation[];
#define Val_interpolation(data) ml_lookup_from_c (ml_table_interpolation, data)
#define Interpolation_val(key) ml_lookup_to_c (ml_table_interpolation, key)

