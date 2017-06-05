/* pango_style : conversion table */
const lookup_info ml_table_pango_style[] = {
  { 0, 3 },
  { MLTAG_NORMAL, PANGO_STYLE_NORMAL },
  { MLTAG_OBLIQUE, PANGO_STYLE_OBLIQUE },
  { MLTAG_ITALIC, PANGO_STYLE_ITALIC },
};

/* pango_weight : conversion table */
const lookup_info ml_table_pango_weight[] = {
  { 0, 6 },
  { MLTAG_NORMAL, PANGO_WEIGHT_NORMAL },
  { MLTAG_LIGHT, PANGO_WEIGHT_LIGHT },
  { MLTAG_ULTRALIGHT, PANGO_WEIGHT_ULTRALIGHT },
  { MLTAG_ULTRABOLD, PANGO_WEIGHT_ULTRABOLD },
  { MLTAG_HEAVY, PANGO_WEIGHT_HEAVY },
  { MLTAG_BOLD, PANGO_WEIGHT_BOLD },
};

/* pango_variant : conversion table */
const lookup_info ml_table_pango_variant[] = {
  { 0, 2 },
  { MLTAG_NORMAL, PANGO_VARIANT_NORMAL },
  { MLTAG_SMALL_CAPS, PANGO_VARIANT_SMALL_CAPS },
};

/* pango_stretch : conversion table */
const lookup_info ml_table_pango_stretch[] = {
  { 0, 9 },
  { MLTAG_EXPANDED, PANGO_STRETCH_EXPANDED },
  { MLTAG_NORMAL, PANGO_STRETCH_NORMAL },
  { MLTAG_ULTRA_CONDENSED, PANGO_STRETCH_ULTRA_CONDENSED },
  { MLTAG_SEMI_CONDENSED, PANGO_STRETCH_SEMI_CONDENSED },
  { MLTAG_SEMI_EXPANDED, PANGO_STRETCH_SEMI_EXPANDED },
  { MLTAG_ULTRA_EXPANDED, PANGO_STRETCH_ULTRA_EXPANDED },
  { MLTAG_CONDENSED, PANGO_STRETCH_CONDENSED },
  { MLTAG_EXTRA_EXPANDED, PANGO_STRETCH_EXTRA_EXPANDED },
  { MLTAG_EXTRA_CONDENSED, PANGO_STRETCH_EXTRA_CONDENSED },
};

/* pango_underline : conversion table */
const lookup_info ml_table_pango_underline[] = {
  { 0, 4 },
  { MLTAG_DOUBLE, PANGO_UNDERLINE_DOUBLE },
  { MLTAG_SINGLE, PANGO_UNDERLINE_SINGLE },
  { MLTAG_LOW, PANGO_UNDERLINE_LOW },
  { MLTAG_NONE, PANGO_UNDERLINE_NONE },
};

/* pango_wrap_mode : conversion table */
const lookup_info ml_table_pango_wrap_mode[] = {
  { 0, 3 },
  { MLTAG_CHAR, PANGO_WRAP_CHAR },
  { MLTAG_WORD_CHAR, PANGO_WRAP_WORD_CHAR },
  { MLTAG_WORD, PANGO_WRAP_WORD },
};

/* pango_ellipsize_mode : conversion table */
const lookup_info ml_table_pango_ellipsize_mode[] = {
#ifdef HASGTK26
  { 0, 4 },
  { MLTAG_MIDDLE, PANGO_ELLIPSIZE_MIDDLE },
  { MLTAG_END, PANGO_ELLIPSIZE_END },
  { MLTAG_START, PANGO_ELLIPSIZE_START },
  { MLTAG_NONE, PANGO_ELLIPSIZE_NONE },
#else
  {0, 0 }
#endif /* HASGTK26 */
};

CAMLprim value ml_pango_get_tables ()
{
  static const lookup_info *ml_lookup_tables[] = {
    ml_table_pango_style,
    ml_table_pango_weight,
    ml_table_pango_variant,
    ml_table_pango_stretch,
    ml_table_pango_underline,
    ml_table_pango_wrap_mode,
    ml_table_pango_ellipsize_mode,
  };
  return (value)ml_lookup_tables;}
