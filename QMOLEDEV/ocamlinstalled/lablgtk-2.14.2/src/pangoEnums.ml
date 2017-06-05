(** pango enums *)

open Gpointer

type style = [ `NORMAL | `OBLIQUE | `ITALIC ]
type weight =
  [ `ULTRALIGHT | `LIGHT | `NORMAL | `BOLD | `ULTRABOLD | `HEAVY ]
type variant = [ `NORMAL | `SMALL_CAPS ]
type stretch =
  [ `ULTRA_CONDENSED | `EXTRA_CONDENSED | `CONDENSED | `SEMI_CONDENSED
  | `NORMAL | `SEMI_EXPANDED | `EXPANDED | `EXTRA_EXPANDED
  | `ULTRA_EXPANDED ]
type underline = [ `NONE | `SINGLE | `DOUBLE | `LOW ]
type wrap_mode = [ `WORD | `CHAR | `WORD_CHAR ]
type ellipsize_mode = [ `NONE | `START | `MIDDLE | `END ]

(**/**)

external _get_tables : unit ->
    style variant_table
  * weight variant_table
  * variant variant_table
  * stretch variant_table
  * underline variant_table
  * wrap_mode variant_table
  * ellipsize_mode variant_table
  = "ml_pango_get_tables"


let style, weight, variant, stretch, underline, wrap_mode,
    ellipsize_mode = _get_tables ()

let style_conv = Gobject.Data.enum style
let weight_conv = Gobject.Data.enum weight
let variant_conv = Gobject.Data.enum variant
let stretch_conv = Gobject.Data.enum stretch
let underline_conv = Gobject.Data.enum underline
let wrap_mode_conv = Gobject.Data.enum wrap_mode
let ellipsize_mode_conv = Gobject.Data.enum ellipsize_mode
