open Gobject
open Data
module Object = GtkObject

open Gtk

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module GtkStatusIcon = struct
  let cast w : gtk_status_icon = try_cast w "GtkStatusIcon"
  module P = struct
    let screen : ([>`gtkstatusicon],_) property =
      {name="screen"; conv=(gobject : Gdk.screen data_conv)}
    let visible : ([>`gtkstatusicon],_) property =
      {name="visible"; conv=boolean}
    let blinking : ([>`gtkstatusicon],_) property =
      {name="blinking"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let activate =
      {name="activate"; classe=`gtkstatusicon; marshaller=marshal_unit}
    let popup_menu =
      {name="popup_menu"; classe=`gtkstatusicon; marshaller=fun f ->
       marshal2 uint uint "GtkStatusIcon::popup_menu" f}
    let size_changed =
      {name="size_changed"; classe=`gtkstatusicon; marshaller=fun f ->
       marshal1 int "GtkStatusIcon::size_changed" f}
  end
  let create pl : gtk_status_icon = Gobject.unsafe_create "GtkStatusIcon" pl
  external set_from_pixbuf :
    [>`gtkstatusicon] obj -> GdkPixbuf.pixbuf -> unit
    = "ml_gtk_status_icon_set_from_pixbuf"
  external set_from_file : [>`gtkstatusicon] obj -> string -> unit
    = "ml_gtk_status_icon_set_from_file"
  external set_from_stock : [>`gtkstatusicon] obj -> string -> unit
    = "ml_gtk_status_icon_set_from_stock"
  external set_from_icon_name : [>`gtkstatusicon] obj -> string -> unit
    = "ml_gtk_status_icon_set_from_icon_name"
  external get_pixbuf : [>`gtkstatusicon] obj -> GdkPixbuf.pixbuf
    = "ml_gtk_status_icon_get_pixbuf"
  external get_stock : [>`gtkstatusicon] obj -> string
    = "ml_gtk_status_icon_get_stock"
  external get_icon_name : [>`gtkstatusicon] obj -> string
    = "ml_gtk_status_icon_get_icon_name"
  external get_size : [>`gtkstatusicon] obj -> int
    = "ml_gtk_status_icon_get_size"
  external set_screen : [>`gtkstatusicon] obj -> Gdk.screen -> unit
    = "ml_gtk_status_icon_set_screen"
  external get_screen : [>`gtkstatusicon] obj -> Gdk.screen
    = "ml_gtk_status_icon_get_screen"
  external set_tooltip : [>`gtkstatusicon] obj -> string -> unit
    = "ml_gtk_status_icon_set_tooltip"
  external set_visible : [>`gtkstatusicon] obj -> bool -> unit
    = "ml_gtk_status_icon_set_visible"
  external get_visible : [>`gtkstatusicon] obj -> bool
    = "ml_gtk_status_icon_get_visible"
  external set_blinking : [>`gtkstatusicon] obj -> bool -> unit
    = "ml_gtk_status_icon_set_blinking"
  external get_blinking : [>`gtkstatusicon] obj -> bool
    = "ml_gtk_status_icon_get_blinking"
  external is_embedded : [>`gtkstatusicon] obj -> bool
    = "ml_gtk_status_icon_is_embedded"
  let make_params ~cont pl ?screen ?visible ?blinking =
    let pl = (
      may_cons P.screen screen (
      may_cons P.visible visible (
      may_cons P.blinking blinking pl))) in
    cont pl
end

module Misc = struct
  let cast w : Gtk.misc obj = try_cast w "GtkMisc"
  module P = struct
    let xalign : ([>`misc],_) property = {name="xalign"; conv=float}
    let yalign : ([>`misc],_) property = {name="yalign"; conv=float}
    let xpad : ([>`misc],_) property = {name="xpad"; conv=int}
    let ypad : ([>`misc],_) property = {name="ypad"; conv=int}
  end
  let make_params ~cont pl ?xalign ?yalign ?xpad ?ypad =
    let pl = (
      may_cons P.xalign xalign (
      may_cons P.yalign yalign (
      may_cons P.xpad xpad (
      may_cons P.ypad ypad pl)))) in
    cont pl
end

module Label = struct
  let cast w : Gtk.label obj = try_cast w "GtkLabel"
  module P = struct
    let label : ([>`label],_) property = {name="label"; conv=string}
    let use_markup : ([>`label],_) property =
      {name="use-markup"; conv=boolean}
    let use_underline : ([>`label],_) property =
      {name="use-underline"; conv=boolean}
    let mnemonic_keyval : ([>`label],_) property =
      {name="mnemonic-keyval"; conv=uint}
    let mnemonic_widget : ([>`label],_) property =
      {name="mnemonic-widget";
       conv=(gobject_option : Gtk.widget obj option data_conv)}
    let justify : ([>`label],_) property =
      {name="justify"; conv=GtkEnums.justification_conv}
    let wrap : ([>`label],_) property = {name="wrap"; conv=boolean}
    let pattern : ([>`label],_) property = {name="pattern"; conv=string}
    let selectable : ([>`label],_) property =
      {name="selectable"; conv=boolean}
    let cursor_position : ([>`label],_) property =
      {name="cursor-position"; conv=int}
    let selection_bound : ([>`label],_) property =
      {name="selection-bound"; conv=int}
    let angle : ([>`label],_) property = {name="angle"; conv=double}
    let ellipsize : ([>`label],_) property =
      {name="ellipsize"; conv=PangoEnums.ellipsize_mode_conv}
    let max_width_chars : ([>`label],_) property =
      {name="max-width-chars"; conv=int}
    let single_line_mode : ([>`label],_) property =
      {name="single-line-mode"; conv=boolean}
    let width_chars : ([>`label],_) property = {name="width-chars"; conv=int}
  end
  module S = struct
    open GtkSignal
    let copy_clipboard =
      {name="copy_clipboard"; classe=`label; marshaller=marshal_unit}
    let move_cursor =
      {name="move_cursor"; classe=`label; marshaller=fun f ->
       marshal3 GtkEnums.movement_step_conv int boolean
         "GtkLabel::move_cursor" f}
    let populate_popup =
      {name="populate_popup"; classe=`label; marshaller=fun f ->
       marshal1 (gobject : Gtk.menu obj data_conv)
         "GtkLabel::populate_popup" f}
  end
  let create pl : Gtk.label obj = Object.make "GtkLabel" pl
  external get_text : [>`label] obj -> string = "ml_gtk_label_get_text"
  external set_text : [>`label] obj -> string -> unit
    = "ml_gtk_label_set_text"
  external select_region : [>`label] obj -> int -> int -> unit
    = "ml_gtk_label_select_region"
  external get_selection_bounds : [>`label] obj -> (int * int) option
    = "ml_gtk_label_get_selection_bounds"
  let make_params ~cont pl ?label ?use_markup ?use_underline ?mnemonic_widget
      ?justify ?line_wrap ?pattern ?selectable ?ellipsize =
    let pl = (
      may_cons P.label label (
      may_cons P.use_markup use_markup (
      may_cons P.use_underline use_underline (
      may_cons_opt P.mnemonic_widget mnemonic_widget (
      may_cons P.justify justify (
      may_cons P.wrap line_wrap (
      may_cons P.pattern pattern (
      may_cons P.selectable selectable (
      may_cons P.ellipsize ellipsize pl))))))))) in
    cont pl
end

module TipsQuery = struct
  let cast w : Gtk.tips_query obj = try_cast w "GtkTipsQuery"
  module P = struct
    let caller : ([>`tipsquery],_) property =
      {name="caller";
       conv=(gobject_option : Gtk.widget obj option data_conv)}
    let emit_always : ([>`tipsquery],_) property =
      {name="emit-always"; conv=boolean}
    let label_inactive : ([>`tipsquery],_) property =
      {name="label-inactive"; conv=string}
    let label_no_tip : ([>`tipsquery],_) property =
      {name="label-no-tip"; conv=string}
  end
  module S = struct
    open GtkSignal
    let start_query =
      {name="start_query"; classe=`tipsquery; marshaller=marshal_unit}
    let stop_query =
      {name="stop_query"; classe=`tipsquery; marshaller=marshal_unit}
    let widget_entered =
      {name="widget_entered"; classe=`tipsquery; marshaller=fun f ->
       marshal3 (gobject_option : Gtk.widget obj option data_conv) string
         string "GtkTipsQuery::widget_entered"
         (fun x1 x2 x3 -> f x1 ~text:x2 ~privat:x3)}
    let widget_selected =
      {name="widget_selected"; classe=`tipsquery; marshaller=fun f ->
       marshal4_ret ~ret:boolean
         (gobject_option : Gtk.widget obj option data_conv) string string
         (unsafe_pointer : GdkEvent.Button.t data_conv)
         "GtkTipsQuery::widget_selected"
         (fun x1 x2 x3 -> f x1 ~text:x2 ~privat:x3)}
  end
  let create pl : Gtk.tips_query obj = Object.make "GtkTipsQuery" pl
  external start_query : [>`tipsquery] obj -> unit
    = "ml_gtk_tips_query_start_query"
  external stop_query : [>`tipsquery] obj -> unit
    = "ml_gtk_tips_query_stop_query"
  let make_params ~cont pl ?caller ?emit_always ?label_inactive
      ?label_no_tip =
    let pl = (
      may_cons_opt P.caller caller (
      may_cons P.emit_always emit_always (
      may_cons P.label_inactive label_inactive (
      may_cons P.label_no_tip label_no_tip pl)))) in
    cont pl
end

module Arrow = struct
  let cast w : Gtk.arrow obj = try_cast w "GtkArrow"
  module P = struct
    let arrow_type : ([>`arrow],_) property =
      {name="arrow-type"; conv=GtkEnums.arrow_type_conv}
    let shadow_type : ([>`arrow],_) property =
      {name="shadow-type"; conv=GtkEnums.shadow_type_conv}
  end
  let create pl : Gtk.arrow obj = Object.make "GtkArrow" pl
  let make_params ~cont pl ?kind ?shadow =
    let pl = (
      may_cons P.arrow_type kind (
      may_cons P.shadow_type shadow pl)) in
    cont pl
end

module Image = struct
  let cast w : Gtk.image obj = try_cast w "GtkImage"
  module P = struct
    let file : ([>`image],_) property = {name="file"; conv=string}
    let image : ([>`image],_) property =
      {name="image"; conv=(gobject : Gdk.image data_conv)}
    let pixbuf : ([>`image],_) property =
      {name="pixbuf"; conv=(gobject : GdkPixbuf.pixbuf data_conv)}
    let pixel_size : ([>`image],_) property = {name="pixel-size"; conv=int}
    let pixmap : ([>`image],_) property =
      {name="pixmap"; conv=(gobject : Gdk.pixmap data_conv)}
    let mask : ([>`image],_) property =
      {name="mask"; conv=(gobject_option : Gdk.bitmap option data_conv)}
    let stock : ([>`image],_) property = {name="stock"; conv=GtkStock.conv}
    let icon_set : ([>`image],_) property =
      {name="icon-set"; conv=(unsafe_pointer : Gtk.icon_set data_conv)}
    let icon_size : ([>`image],_) property =
      {name="icon-size"; conv=GtkEnums.icon_size_conv}
    let storage_type : ([>`image],_) property =
      {name="storage-type"; conv=GtkEnums.image_type_conv}
  end
  let create pl : Gtk.image obj = Object.make "GtkImage" pl
  external clear : [>`image] obj -> unit = "ml_gtk_image_clear"
  let make_params ~cont pl ?file ?image ?pixbuf ?pixel_size ?pixmap ?mask
      ?stock ?icon_set ?icon_size =
    let pl = (
      may_cons P.file file (
      may_cons P.image image (
      may_cons P.pixbuf pixbuf (
      may_cons P.pixel_size pixel_size (
      may_cons P.pixmap pixmap (
      may_cons_opt P.mask mask (
      may_cons P.stock stock (
      may_cons P.icon_set icon_set (
      may_cons P.icon_size icon_size pl))))))))) in
    cont pl
end

module ColorSelection = struct
  let cast w : Gtk.color_selection obj = try_cast w "GtkColorSelection"
  module P = struct
    let current_alpha : ([>`colorselection],_) property =
      {name="current-alpha"; conv=uint}
    let current_color : ([>`colorselection],_) property =
      {name="current-color"; conv=(unsafe_pointer : Gdk.color data_conv)}
    let has_opacity_control : ([>`colorselection],_) property =
      {name="has-opacity-control"; conv=boolean}
    let has_palette : ([>`colorselection],_) property =
      {name="has-palette"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let color_changed =
      {name="color_changed"; classe=`colorselection; marshaller=marshal_unit}
  end
  let create pl : Gtk.color_selection obj =
    Object.make "GtkColorSelection" pl
  let make_params ~cont pl ?alpha ?color ?has_opacity_control ?has_palette =
    let pl = (
      may_cons P.current_alpha alpha (
      may_cons P.current_color color (
      may_cons P.has_opacity_control has_opacity_control (
      may_cons P.has_palette has_palette pl)))) in
    cont pl
end

module FontSelection = struct
  let cast w : Gtk.font_selection obj = try_cast w "GtkFontSelection"
  module P = struct
    let font_name : ([>`fontselection],_) property =
      {name="font-name"; conv=string}
    let preview_text : ([>`fontselection],_) property =
      {name="preview-text"; conv=string}
  end
  let create pl : Gtk.font_selection obj = Object.make "GtkFontSelection" pl
  let make_params ~cont pl ?font_name ?preview_text =
    let pl = (
      may_cons P.font_name font_name (
      may_cons P.preview_text preview_text pl)) in
    cont pl
end

module GammaCurve = struct
  let cast w : Gtk.gamma_curve obj = try_cast w "GtkGammaCurve"
  let create pl : Gtk.gamma_curve obj = Object.make "GtkGammaCurve" pl
  external get_gamma : [>`gammacurve] obj -> float
    = "ml_gtk_gamma_curve_get_gamma"
end

module Statusbar = struct
  let cast w : Gtk.statusbar obj = try_cast w "GtkStatusbar"
  module P = struct
    let has_resize_grip : ([>`statusbar],_) property =
      {name="has-resize-grip"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let text_popped =
      {name="text_popped"; classe=`statusbar; marshaller=fun f ->
       marshal2 uint string "GtkStatusbar::text_popped" f}
    let text_pushed =
      {name="text_pushed"; classe=`statusbar; marshaller=fun f ->
       marshal2 uint string "GtkStatusbar::text_pushed" f}
  end
  let create pl : Gtk.statusbar obj = Object.make "GtkStatusbar" pl
  external get_has_resize_grip : [>`statusbar] obj -> bool
    = "ml_gtk_statusbar_get_has_resize_grip"
  external set_has_resize_grip : [>`statusbar] obj -> bool -> unit
    = "ml_gtk_statusbar_set_has_resize_grip"
  external get_context_id : [>`statusbar] obj -> string -> statusbar_context
    = "ml_gtk_statusbar_get_context_id"
  external push :
    [>`statusbar] obj ->
    statusbar_context -> text:string -> statusbar_message
    = "ml_gtk_statusbar_push"
  external pop : [>`statusbar] obj -> statusbar_context ->  unit
    = "ml_gtk_statusbar_pop"
  external remove :
    [>`statusbar] obj -> statusbar_context -> statusbar_message -> unit
    = "ml_gtk_statusbar_remove"
  let make_params ~cont pl ?has_resize_grip =
    let pl = (may_cons P.has_resize_grip has_resize_grip pl) in
    cont pl
end

module Calendar = struct
  let cast w : Gtk.calendar obj = try_cast w "GtkCalendar"
  module P = struct
    let day : ([>`calendar],_) property = {name="day"; conv=int}
    let month : ([>`calendar],_) property = {name="month"; conv=int}
    let year : ([>`calendar],_) property = {name="year"; conv=int}
  end
  module S = struct
    open GtkSignal
    let day_selected =
      {name="day_selected"; classe=`calendar; marshaller=marshal_unit}
    let day_selected_double_click =
      {name="day_selected_double_click"; classe=`calendar;
       marshaller=marshal_unit}
    let month_changed =
      {name="month_changed"; classe=`calendar; marshaller=marshal_unit}
    let next_month =
      {name="next_month"; classe=`calendar; marshaller=marshal_unit}
    let next_year =
      {name="next_year"; classe=`calendar; marshaller=marshal_unit}
    let prev_month =
      {name="prev_month"; classe=`calendar; marshaller=marshal_unit}
    let prev_year =
      {name="prev_year"; classe=`calendar; marshaller=marshal_unit}
  end
  let create pl : Gtk.calendar obj = Object.make "GtkCalendar" pl
  external select_month : [>`calendar] obj -> month:int -> year:int -> unit
    = "ml_gtk_calendar_select_month"
  external select_day : [>`calendar] obj -> int -> unit
    = "ml_gtk_calendar_select_day"
  external mark_day : [>`calendar] obj -> int -> unit
    = "ml_gtk_calendar_mark_day"
  external unmark_day : [>`calendar] obj -> int -> unit
    = "ml_gtk_calendar_unmark_day"
  external get_num_marked_dates : [>`calendar] obj -> int
    = "ml_gtk_calendar_get_num_marked_dates"
  external is_day_marked : [>`calendar] obj -> int -> bool
    = "ml_gtk_calendar_is_day_marked"
  external clear_marks : [>`calendar] obj -> unit
    = "ml_gtk_calendar_clear_marks"
  external display_options :
    [>`calendar] obj -> Gtk.Tags.calendar_display_options list -> unit
    = "ml_gtk_calendar_display_options"
  external get_date : [>`calendar] obj -> int * int * int
    = "ml_gtk_calendar_get_date"
  external freeze : [>`calendar] obj -> unit = "ml_gtk_calendar_freeze"
  external thaw : [>`calendar] obj -> unit = "ml_gtk_calendar_thaw"
end

module DrawingArea = struct
  let cast w : Gtk.drawing_area obj = try_cast w "GtkDrawingArea"
  let create pl : Gtk.drawing_area obj = Object.make "GtkDrawingArea" pl
  external size : [>`drawingarea] obj -> width:int -> height:int -> unit
    = "ml_gtk_drawing_area_size"
end

module Curve = struct
  let cast w : Gtk.curve obj = try_cast w "GtkCurve"
  module P = struct
    let curve_type : ([>`curve],_) property =
      {name="curve-type"; conv=GtkEnums.curve_type_conv}
    let max_x : ([>`curve],_) property = {name="max-x"; conv=float}
    let max_y : ([>`curve],_) property = {name="max-y"; conv=float}
    let min_x : ([>`curve],_) property = {name="min-x"; conv=float}
    let min_y : ([>`curve],_) property = {name="min-y"; conv=float}
  end
  let create pl : Gtk.curve obj = Object.make "GtkCurve" pl
  external reset : [>`curve] obj -> unit = "ml_gtk_curve_reset"
  external set_gamma : [>`curve] obj -> int -> unit
    = "ml_gtk_curve_set_gamma"
  external set_vector : [>`curve] obj -> float array -> unit
    = "ml_gtk_curve_set_vector"
  external get_vector : [>`curve] obj -> int -> float array
    = "ml_gtk_curve_get_vector"
  let make_params ~cont pl ?curve_type ?max_x ?max_y ?min_x ?min_y =
    let pl = (
      may_cons P.curve_type curve_type (
      may_cons P.max_x max_x (
      may_cons P.max_y max_y (
      may_cons P.min_x min_x (
      may_cons P.min_y min_y pl))))) in
    cont pl
end

module Separator = struct
  let cast w : Gtk.separator obj = try_cast w "GtkSeparator"
  let create (dir : Gtk.Tags.orientation) pl : Gtk.separator obj =
    Object.make
      (if dir = `HORIZONTAL then "GtkHSeparator" else "GtkVSeparator")  pl
end

