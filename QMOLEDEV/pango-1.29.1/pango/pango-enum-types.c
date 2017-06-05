


#include <pango.h>

/* enumerations from "pango-attributes.h" */
GType
pango_attr_type_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_ATTR_INVALID, "PANGO_ATTR_INVALID", "invalid" },
      { PANGO_ATTR_LANGUAGE, "PANGO_ATTR_LANGUAGE", "language" },
      { PANGO_ATTR_FAMILY, "PANGO_ATTR_FAMILY", "family" },
      { PANGO_ATTR_STYLE, "PANGO_ATTR_STYLE", "style" },
      { PANGO_ATTR_WEIGHT, "PANGO_ATTR_WEIGHT", "weight" },
      { PANGO_ATTR_VARIANT, "PANGO_ATTR_VARIANT", "variant" },
      { PANGO_ATTR_STRETCH, "PANGO_ATTR_STRETCH", "stretch" },
      { PANGO_ATTR_SIZE, "PANGO_ATTR_SIZE", "size" },
      { PANGO_ATTR_FONT_DESC, "PANGO_ATTR_FONT_DESC", "font-desc" },
      { PANGO_ATTR_FOREGROUND, "PANGO_ATTR_FOREGROUND", "foreground" },
      { PANGO_ATTR_BACKGROUND, "PANGO_ATTR_BACKGROUND", "background" },
      { PANGO_ATTR_UNDERLINE, "PANGO_ATTR_UNDERLINE", "underline" },
      { PANGO_ATTR_STRIKETHROUGH, "PANGO_ATTR_STRIKETHROUGH", "strikethrough" },
      { PANGO_ATTR_RISE, "PANGO_ATTR_RISE", "rise" },
      { PANGO_ATTR_SHAPE, "PANGO_ATTR_SHAPE", "shape" },
      { PANGO_ATTR_SCALE, "PANGO_ATTR_SCALE", "scale" },
      { PANGO_ATTR_FALLBACK, "PANGO_ATTR_FALLBACK", "fallback" },
      { PANGO_ATTR_LETTER_SPACING, "PANGO_ATTR_LETTER_SPACING", "letter-spacing" },
      { PANGO_ATTR_UNDERLINE_COLOR, "PANGO_ATTR_UNDERLINE_COLOR", "underline-color" },
      { PANGO_ATTR_STRIKETHROUGH_COLOR, "PANGO_ATTR_STRIKETHROUGH_COLOR", "strikethrough-color" },
      { PANGO_ATTR_ABSOLUTE_SIZE, "PANGO_ATTR_ABSOLUTE_SIZE", "absolute-size" },
      { PANGO_ATTR_GRAVITY, "PANGO_ATTR_GRAVITY", "gravity" },
      { PANGO_ATTR_GRAVITY_HINT, "PANGO_ATTR_GRAVITY_HINT", "gravity-hint" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoAttrType"), values);
  }
  return etype;
}
GType
pango_underline_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_UNDERLINE_NONE, "PANGO_UNDERLINE_NONE", "none" },
      { PANGO_UNDERLINE_SINGLE, "PANGO_UNDERLINE_SINGLE", "single" },
      { PANGO_UNDERLINE_DOUBLE, "PANGO_UNDERLINE_DOUBLE", "double" },
      { PANGO_UNDERLINE_LOW, "PANGO_UNDERLINE_LOW", "low" },
      { PANGO_UNDERLINE_ERROR, "PANGO_UNDERLINE_ERROR", "error" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoUnderline"), values);
  }
  return etype;
}

/* enumerations from "pango-bidi-type.h" */
GType
pango_bidi_type_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_BIDI_TYPE_L, "PANGO_BIDI_TYPE_L", "l" },
      { PANGO_BIDI_TYPE_LRE, "PANGO_BIDI_TYPE_LRE", "lre" },
      { PANGO_BIDI_TYPE_LRO, "PANGO_BIDI_TYPE_LRO", "lro" },
      { PANGO_BIDI_TYPE_R, "PANGO_BIDI_TYPE_R", "r" },
      { PANGO_BIDI_TYPE_AL, "PANGO_BIDI_TYPE_AL", "al" },
      { PANGO_BIDI_TYPE_RLE, "PANGO_BIDI_TYPE_RLE", "rle" },
      { PANGO_BIDI_TYPE_RLO, "PANGO_BIDI_TYPE_RLO", "rlo" },
      { PANGO_BIDI_TYPE_PDF, "PANGO_BIDI_TYPE_PDF", "pdf" },
      { PANGO_BIDI_TYPE_EN, "PANGO_BIDI_TYPE_EN", "en" },
      { PANGO_BIDI_TYPE_ES, "PANGO_BIDI_TYPE_ES", "es" },
      { PANGO_BIDI_TYPE_ET, "PANGO_BIDI_TYPE_ET", "et" },
      { PANGO_BIDI_TYPE_AN, "PANGO_BIDI_TYPE_AN", "an" },
      { PANGO_BIDI_TYPE_CS, "PANGO_BIDI_TYPE_CS", "cs" },
      { PANGO_BIDI_TYPE_NSM, "PANGO_BIDI_TYPE_NSM", "nsm" },
      { PANGO_BIDI_TYPE_BN, "PANGO_BIDI_TYPE_BN", "bn" },
      { PANGO_BIDI_TYPE_B, "PANGO_BIDI_TYPE_B", "b" },
      { PANGO_BIDI_TYPE_S, "PANGO_BIDI_TYPE_S", "s" },
      { PANGO_BIDI_TYPE_WS, "PANGO_BIDI_TYPE_WS", "ws" },
      { PANGO_BIDI_TYPE_ON, "PANGO_BIDI_TYPE_ON", "on" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoBidiType"), values);
  }
  return etype;
}
GType
pango_direction_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_DIRECTION_LTR, "PANGO_DIRECTION_LTR", "ltr" },
      { PANGO_DIRECTION_RTL, "PANGO_DIRECTION_RTL", "rtl" },
      { PANGO_DIRECTION_TTB_LTR, "PANGO_DIRECTION_TTB_LTR", "ttb-ltr" },
      { PANGO_DIRECTION_TTB_RTL, "PANGO_DIRECTION_TTB_RTL", "ttb-rtl" },
      { PANGO_DIRECTION_WEAK_LTR, "PANGO_DIRECTION_WEAK_LTR", "weak-ltr" },
      { PANGO_DIRECTION_WEAK_RTL, "PANGO_DIRECTION_WEAK_RTL", "weak-rtl" },
      { PANGO_DIRECTION_NEUTRAL, "PANGO_DIRECTION_NEUTRAL", "neutral" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoDirection"), values);
  }
  return etype;
}

/* enumerations from "pango-coverage.h" */
GType
pango_coverage_level_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_COVERAGE_NONE, "PANGO_COVERAGE_NONE", "none" },
      { PANGO_COVERAGE_FALLBACK, "PANGO_COVERAGE_FALLBACK", "fallback" },
      { PANGO_COVERAGE_APPROXIMATE, "PANGO_COVERAGE_APPROXIMATE", "approximate" },
      { PANGO_COVERAGE_EXACT, "PANGO_COVERAGE_EXACT", "exact" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoCoverageLevel"), values);
  }
  return etype;
}

/* enumerations from "pango-font.h" */
GType
pango_style_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_STYLE_NORMAL, "PANGO_STYLE_NORMAL", "normal" },
      { PANGO_STYLE_OBLIQUE, "PANGO_STYLE_OBLIQUE", "oblique" },
      { PANGO_STYLE_ITALIC, "PANGO_STYLE_ITALIC", "italic" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoStyle"), values);
  }
  return etype;
}
GType
pango_variant_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_VARIANT_NORMAL, "PANGO_VARIANT_NORMAL", "normal" },
      { PANGO_VARIANT_SMALL_CAPS, "PANGO_VARIANT_SMALL_CAPS", "small-caps" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoVariant"), values);
  }
  return etype;
}
GType
pango_weight_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_WEIGHT_THIN, "PANGO_WEIGHT_THIN", "thin" },
      { PANGO_WEIGHT_ULTRALIGHT, "PANGO_WEIGHT_ULTRALIGHT", "ultralight" },
      { PANGO_WEIGHT_LIGHT, "PANGO_WEIGHT_LIGHT", "light" },
      { PANGO_WEIGHT_BOOK, "PANGO_WEIGHT_BOOK", "book" },
      { PANGO_WEIGHT_NORMAL, "PANGO_WEIGHT_NORMAL", "normal" },
      { PANGO_WEIGHT_MEDIUM, "PANGO_WEIGHT_MEDIUM", "medium" },
      { PANGO_WEIGHT_SEMIBOLD, "PANGO_WEIGHT_SEMIBOLD", "semibold" },
      { PANGO_WEIGHT_BOLD, "PANGO_WEIGHT_BOLD", "bold" },
      { PANGO_WEIGHT_ULTRABOLD, "PANGO_WEIGHT_ULTRABOLD", "ultrabold" },
      { PANGO_WEIGHT_HEAVY, "PANGO_WEIGHT_HEAVY", "heavy" },
      { PANGO_WEIGHT_ULTRAHEAVY, "PANGO_WEIGHT_ULTRAHEAVY", "ultraheavy" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoWeight"), values);
  }
  return etype;
}
GType
pango_stretch_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_STRETCH_ULTRA_CONDENSED, "PANGO_STRETCH_ULTRA_CONDENSED", "ultra-condensed" },
      { PANGO_STRETCH_EXTRA_CONDENSED, "PANGO_STRETCH_EXTRA_CONDENSED", "extra-condensed" },
      { PANGO_STRETCH_CONDENSED, "PANGO_STRETCH_CONDENSED", "condensed" },
      { PANGO_STRETCH_SEMI_CONDENSED, "PANGO_STRETCH_SEMI_CONDENSED", "semi-condensed" },
      { PANGO_STRETCH_NORMAL, "PANGO_STRETCH_NORMAL", "normal" },
      { PANGO_STRETCH_SEMI_EXPANDED, "PANGO_STRETCH_SEMI_EXPANDED", "semi-expanded" },
      { PANGO_STRETCH_EXPANDED, "PANGO_STRETCH_EXPANDED", "expanded" },
      { PANGO_STRETCH_EXTRA_EXPANDED, "PANGO_STRETCH_EXTRA_EXPANDED", "extra-expanded" },
      { PANGO_STRETCH_ULTRA_EXPANDED, "PANGO_STRETCH_ULTRA_EXPANDED", "ultra-expanded" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoStretch"), values);
  }
  return etype;
}
GType
pango_font_mask_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GFlagsValue values[] = {
      { PANGO_FONT_MASK_FAMILY, "PANGO_FONT_MASK_FAMILY", "family" },
      { PANGO_FONT_MASK_STYLE, "PANGO_FONT_MASK_STYLE", "style" },
      { PANGO_FONT_MASK_VARIANT, "PANGO_FONT_MASK_VARIANT", "variant" },
      { PANGO_FONT_MASK_WEIGHT, "PANGO_FONT_MASK_WEIGHT", "weight" },
      { PANGO_FONT_MASK_STRETCH, "PANGO_FONT_MASK_STRETCH", "stretch" },
      { PANGO_FONT_MASK_SIZE, "PANGO_FONT_MASK_SIZE", "size" },
      { PANGO_FONT_MASK_GRAVITY, "PANGO_FONT_MASK_GRAVITY", "gravity" },
      { 0, NULL, NULL }
    };
    etype = g_flags_register_static (g_intern_static_string ("PangoFontMask"), values);
  }
  return etype;
}

/* enumerations from "pango-gravity.h" */
GType
pango_gravity_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_GRAVITY_SOUTH, "PANGO_GRAVITY_SOUTH", "south" },
      { PANGO_GRAVITY_EAST, "PANGO_GRAVITY_EAST", "east" },
      { PANGO_GRAVITY_NORTH, "PANGO_GRAVITY_NORTH", "north" },
      { PANGO_GRAVITY_WEST, "PANGO_GRAVITY_WEST", "west" },
      { PANGO_GRAVITY_AUTO, "PANGO_GRAVITY_AUTO", "auto" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoGravity"), values);
  }
  return etype;
}
GType
pango_gravity_hint_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_GRAVITY_HINT_NATURAL, "PANGO_GRAVITY_HINT_NATURAL", "natural" },
      { PANGO_GRAVITY_HINT_STRONG, "PANGO_GRAVITY_HINT_STRONG", "strong" },
      { PANGO_GRAVITY_HINT_LINE, "PANGO_GRAVITY_HINT_LINE", "line" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoGravityHint"), values);
  }
  return etype;
}

/* enumerations from "pango-layout.h" */
GType
pango_alignment_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_ALIGN_LEFT, "PANGO_ALIGN_LEFT", "left" },
      { PANGO_ALIGN_CENTER, "PANGO_ALIGN_CENTER", "center" },
      { PANGO_ALIGN_RIGHT, "PANGO_ALIGN_RIGHT", "right" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoAlignment"), values);
  }
  return etype;
}
GType
pango_wrap_mode_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_WRAP_WORD, "PANGO_WRAP_WORD", "word" },
      { PANGO_WRAP_CHAR, "PANGO_WRAP_CHAR", "char" },
      { PANGO_WRAP_WORD_CHAR, "PANGO_WRAP_WORD_CHAR", "word-char" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoWrapMode"), values);
  }
  return etype;
}
GType
pango_ellipsize_mode_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_ELLIPSIZE_NONE, "PANGO_ELLIPSIZE_NONE", "none" },
      { PANGO_ELLIPSIZE_START, "PANGO_ELLIPSIZE_START", "start" },
      { PANGO_ELLIPSIZE_MIDDLE, "PANGO_ELLIPSIZE_MIDDLE", "middle" },
      { PANGO_ELLIPSIZE_END, "PANGO_ELLIPSIZE_END", "end" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoEllipsizeMode"), values);
  }
  return etype;
}

/* enumerations from "pango-renderer.h" */
GType
pango_render_part_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_RENDER_PART_FOREGROUND, "PANGO_RENDER_PART_FOREGROUND", "foreground" },
      { PANGO_RENDER_PART_BACKGROUND, "PANGO_RENDER_PART_BACKGROUND", "background" },
      { PANGO_RENDER_PART_UNDERLINE, "PANGO_RENDER_PART_UNDERLINE", "underline" },
      { PANGO_RENDER_PART_STRIKETHROUGH, "PANGO_RENDER_PART_STRIKETHROUGH", "strikethrough" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoRenderPart"), values);
  }
  return etype;
}

/* enumerations from "pango-script.h" */
GType
pango_script_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_SCRIPT_INVALID_CODE, "PANGO_SCRIPT_INVALID_CODE", "invalid-code" },
      { PANGO_SCRIPT_COMMON, "PANGO_SCRIPT_COMMON", "common" },
      { PANGO_SCRIPT_INHERITED, "PANGO_SCRIPT_INHERITED", "inherited" },
      { PANGO_SCRIPT_ARABIC, "PANGO_SCRIPT_ARABIC", "arabic" },
      { PANGO_SCRIPT_ARMENIAN, "PANGO_SCRIPT_ARMENIAN", "armenian" },
      { PANGO_SCRIPT_BENGALI, "PANGO_SCRIPT_BENGALI", "bengali" },
      { PANGO_SCRIPT_BOPOMOFO, "PANGO_SCRIPT_BOPOMOFO", "bopomofo" },
      { PANGO_SCRIPT_CHEROKEE, "PANGO_SCRIPT_CHEROKEE", "cherokee" },
      { PANGO_SCRIPT_COPTIC, "PANGO_SCRIPT_COPTIC", "coptic" },
      { PANGO_SCRIPT_CYRILLIC, "PANGO_SCRIPT_CYRILLIC", "cyrillic" },
      { PANGO_SCRIPT_DESERET, "PANGO_SCRIPT_DESERET", "deseret" },
      { PANGO_SCRIPT_DEVANAGARI, "PANGO_SCRIPT_DEVANAGARI", "devanagari" },
      { PANGO_SCRIPT_ETHIOPIC, "PANGO_SCRIPT_ETHIOPIC", "ethiopic" },
      { PANGO_SCRIPT_GEORGIAN, "PANGO_SCRIPT_GEORGIAN", "georgian" },
      { PANGO_SCRIPT_GOTHIC, "PANGO_SCRIPT_GOTHIC", "gothic" },
      { PANGO_SCRIPT_GREEK, "PANGO_SCRIPT_GREEK", "greek" },
      { PANGO_SCRIPT_GUJARATI, "PANGO_SCRIPT_GUJARATI", "gujarati" },
      { PANGO_SCRIPT_GURMUKHI, "PANGO_SCRIPT_GURMUKHI", "gurmukhi" },
      { PANGO_SCRIPT_HAN, "PANGO_SCRIPT_HAN", "han" },
      { PANGO_SCRIPT_HANGUL, "PANGO_SCRIPT_HANGUL", "hangul" },
      { PANGO_SCRIPT_HEBREW, "PANGO_SCRIPT_HEBREW", "hebrew" },
      { PANGO_SCRIPT_HIRAGANA, "PANGO_SCRIPT_HIRAGANA", "hiragana" },
      { PANGO_SCRIPT_KANNADA, "PANGO_SCRIPT_KANNADA", "kannada" },
      { PANGO_SCRIPT_KATAKANA, "PANGO_SCRIPT_KATAKANA", "katakana" },
      { PANGO_SCRIPT_KHMER, "PANGO_SCRIPT_KHMER", "khmer" },
      { PANGO_SCRIPT_LAO, "PANGO_SCRIPT_LAO", "lao" },
      { PANGO_SCRIPT_LATIN, "PANGO_SCRIPT_LATIN", "latin" },
      { PANGO_SCRIPT_MALAYALAM, "PANGO_SCRIPT_MALAYALAM", "malayalam" },
      { PANGO_SCRIPT_MONGOLIAN, "PANGO_SCRIPT_MONGOLIAN", "mongolian" },
      { PANGO_SCRIPT_MYANMAR, "PANGO_SCRIPT_MYANMAR", "myanmar" },
      { PANGO_SCRIPT_OGHAM, "PANGO_SCRIPT_OGHAM", "ogham" },
      { PANGO_SCRIPT_OLD_ITALIC, "PANGO_SCRIPT_OLD_ITALIC", "old-italic" },
      { PANGO_SCRIPT_ORIYA, "PANGO_SCRIPT_ORIYA", "oriya" },
      { PANGO_SCRIPT_RUNIC, "PANGO_SCRIPT_RUNIC", "runic" },
      { PANGO_SCRIPT_SINHALA, "PANGO_SCRIPT_SINHALA", "sinhala" },
      { PANGO_SCRIPT_SYRIAC, "PANGO_SCRIPT_SYRIAC", "syriac" },
      { PANGO_SCRIPT_TAMIL, "PANGO_SCRIPT_TAMIL", "tamil" },
      { PANGO_SCRIPT_TELUGU, "PANGO_SCRIPT_TELUGU", "telugu" },
      { PANGO_SCRIPT_THAANA, "PANGO_SCRIPT_THAANA", "thaana" },
      { PANGO_SCRIPT_THAI, "PANGO_SCRIPT_THAI", "thai" },
      { PANGO_SCRIPT_TIBETAN, "PANGO_SCRIPT_TIBETAN", "tibetan" },
      { PANGO_SCRIPT_CANADIAN_ABORIGINAL, "PANGO_SCRIPT_CANADIAN_ABORIGINAL", "canadian-aboriginal" },
      { PANGO_SCRIPT_YI, "PANGO_SCRIPT_YI", "yi" },
      { PANGO_SCRIPT_TAGALOG, "PANGO_SCRIPT_TAGALOG", "tagalog" },
      { PANGO_SCRIPT_HANUNOO, "PANGO_SCRIPT_HANUNOO", "hanunoo" },
      { PANGO_SCRIPT_BUHID, "PANGO_SCRIPT_BUHID", "buhid" },
      { PANGO_SCRIPT_TAGBANWA, "PANGO_SCRIPT_TAGBANWA", "tagbanwa" },
      { PANGO_SCRIPT_BRAILLE, "PANGO_SCRIPT_BRAILLE", "braille" },
      { PANGO_SCRIPT_CYPRIOT, "PANGO_SCRIPT_CYPRIOT", "cypriot" },
      { PANGO_SCRIPT_LIMBU, "PANGO_SCRIPT_LIMBU", "limbu" },
      { PANGO_SCRIPT_OSMANYA, "PANGO_SCRIPT_OSMANYA", "osmanya" },
      { PANGO_SCRIPT_SHAVIAN, "PANGO_SCRIPT_SHAVIAN", "shavian" },
      { PANGO_SCRIPT_LINEAR_B, "PANGO_SCRIPT_LINEAR_B", "linear-b" },
      { PANGO_SCRIPT_TAI_LE, "PANGO_SCRIPT_TAI_LE", "tai-le" },
      { PANGO_SCRIPT_UGARITIC, "PANGO_SCRIPT_UGARITIC", "ugaritic" },
      { PANGO_SCRIPT_NEW_TAI_LUE, "PANGO_SCRIPT_NEW_TAI_LUE", "new-tai-lue" },
      { PANGO_SCRIPT_BUGINESE, "PANGO_SCRIPT_BUGINESE", "buginese" },
      { PANGO_SCRIPT_GLAGOLITIC, "PANGO_SCRIPT_GLAGOLITIC", "glagolitic" },
      { PANGO_SCRIPT_TIFINAGH, "PANGO_SCRIPT_TIFINAGH", "tifinagh" },
      { PANGO_SCRIPT_SYLOTI_NAGRI, "PANGO_SCRIPT_SYLOTI_NAGRI", "syloti-nagri" },
      { PANGO_SCRIPT_OLD_PERSIAN, "PANGO_SCRIPT_OLD_PERSIAN", "old-persian" },
      { PANGO_SCRIPT_KHAROSHTHI, "PANGO_SCRIPT_KHAROSHTHI", "kharoshthi" },
      { PANGO_SCRIPT_UNKNOWN, "PANGO_SCRIPT_UNKNOWN", "unknown" },
      { PANGO_SCRIPT_BALINESE, "PANGO_SCRIPT_BALINESE", "balinese" },
      { PANGO_SCRIPT_CUNEIFORM, "PANGO_SCRIPT_CUNEIFORM", "cuneiform" },
      { PANGO_SCRIPT_PHOENICIAN, "PANGO_SCRIPT_PHOENICIAN", "phoenician" },
      { PANGO_SCRIPT_PHAGS_PA, "PANGO_SCRIPT_PHAGS_PA", "phags-pa" },
      { PANGO_SCRIPT_NKO, "PANGO_SCRIPT_NKO", "nko" },
      { PANGO_SCRIPT_KAYAH_LI, "PANGO_SCRIPT_KAYAH_LI", "kayah-li" },
      { PANGO_SCRIPT_LEPCHA, "PANGO_SCRIPT_LEPCHA", "lepcha" },
      { PANGO_SCRIPT_REJANG, "PANGO_SCRIPT_REJANG", "rejang" },
      { PANGO_SCRIPT_SUNDANESE, "PANGO_SCRIPT_SUNDANESE", "sundanese" },
      { PANGO_SCRIPT_SAURASHTRA, "PANGO_SCRIPT_SAURASHTRA", "saurashtra" },
      { PANGO_SCRIPT_CHAM, "PANGO_SCRIPT_CHAM", "cham" },
      { PANGO_SCRIPT_OL_CHIKI, "PANGO_SCRIPT_OL_CHIKI", "ol-chiki" },
      { PANGO_SCRIPT_VAI, "PANGO_SCRIPT_VAI", "vai" },
      { PANGO_SCRIPT_CARIAN, "PANGO_SCRIPT_CARIAN", "carian" },
      { PANGO_SCRIPT_LYCIAN, "PANGO_SCRIPT_LYCIAN", "lycian" },
      { PANGO_SCRIPT_LYDIAN, "PANGO_SCRIPT_LYDIAN", "lydian" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoScript"), values);
  }
  return etype;
}

/* enumerations from "pango-tabs.h" */
GType
pango_tab_align_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { PANGO_TAB_LEFT, "PANGO_TAB_LEFT", "left" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("PangoTabAlign"), values);
  }
  return etype;
}



