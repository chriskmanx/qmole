/* colorspace : conversion table */
const lookup_info ml_table_colorspace[] = {
  { 0, 1 },
  { MLTAG_RGB, GDK_COLORSPACE_RGB },
};

/* alpha_mode : conversion table */
const lookup_info ml_table_alpha_mode[] = {
  { 0, 2 },
  { MLTAG_BILEVEL, GDK_PIXBUF_ALPHA_BILEVEL },
  { MLTAG_FULL, GDK_PIXBUF_ALPHA_FULL },
};

/* interpolation : conversion table */
const lookup_info ml_table_interpolation[] = {
  { 0, 4 },
  { MLTAG_NEAREST, GDK_INTERP_NEAREST },
  { MLTAG_TILES, GDK_INTERP_TILES },
  { MLTAG_HYPER, GDK_INTERP_HYPER },
  { MLTAG_BILINEAR, GDK_INTERP_BILINEAR },
};

