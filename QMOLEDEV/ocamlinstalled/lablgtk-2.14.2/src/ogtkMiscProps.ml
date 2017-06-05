open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param
open GtkMiscProps

class virtual gtk_status_icon_props = object
  val virtual obj : _ obj
  method set_screen = set GtkStatusIcon.P.screen obj
  method set_visible = set GtkStatusIcon.P.visible obj
  method set_blinking = set GtkStatusIcon.P.blinking obj
  method screen = get GtkStatusIcon.P.screen obj
  method visible = get GtkStatusIcon.P.visible obj
  method blinking = get GtkStatusIcon.P.blinking obj
end

class virtual gtk_status_icon_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method activate = self#connect GtkStatusIcon.S.activate
  method popup_menu = self#connect GtkStatusIcon.S.popup_menu
  method size_changed = self#connect GtkStatusIcon.S.size_changed
end

class virtual misc_props = object
  val virtual obj : _ obj
  method set_xalign = set Misc.P.xalign obj
  method set_yalign = set Misc.P.yalign obj
  method set_xpad = set Misc.P.xpad obj
  method set_ypad = set Misc.P.ypad obj
  method xalign = get Misc.P.xalign obj
  method yalign = get Misc.P.yalign obj
  method xpad = get Misc.P.xpad obj
  method ypad = get Misc.P.ypad obj
end

class virtual label_props = object
  val virtual obj : _ obj
  method set_label = set Label.P.label obj
  method set_use_markup = set Label.P.use_markup obj
  method set_use_underline = set Label.P.use_underline obj
  method set_mnemonic_widget =
    set {Label.P.mnemonic_widget with conv=GObj.conv_widget_option} obj
  method set_justify = set Label.P.justify obj
  method set_line_wrap = set Label.P.wrap obj
  method set_pattern = set Label.P.pattern obj
  method set_selectable = set Label.P.selectable obj
  method set_angle = set Label.P.angle obj
  method set_ellipsize = set Label.P.ellipsize obj
  method set_max_width_chars = set Label.P.max_width_chars obj
  method set_single_line_mode = set Label.P.single_line_mode obj
  method set_width_chars = set Label.P.width_chars obj
  method label = get Label.P.label obj
  method use_markup = get Label.P.use_markup obj
  method use_underline = get Label.P.use_underline obj
  method mnemonic_keyval = get Label.P.mnemonic_keyval obj
  method mnemonic_widget =
    get {Label.P.mnemonic_widget with conv=GObj.conv_widget_option} obj
  method justify = get Label.P.justify obj
  method line_wrap = get Label.P.wrap obj
  method selectable = get Label.P.selectable obj
  method cursor_position = get Label.P.cursor_position obj
  method selection_bound = get Label.P.selection_bound obj
  method angle = get Label.P.angle obj
  method ellipsize = get Label.P.ellipsize obj
  method max_width_chars = get Label.P.max_width_chars obj
  method single_line_mode = get Label.P.single_line_mode obj
  method width_chars = get Label.P.width_chars obj
end

class virtual tips_query_props = object
  val virtual obj : _ obj
  method set_caller =
    set {TipsQuery.P.caller with conv=GObj.conv_widget_option} obj
  method set_emit_always = set TipsQuery.P.emit_always obj
  method set_label_inactive = set TipsQuery.P.label_inactive obj
  method set_label_no_tip = set TipsQuery.P.label_no_tip obj
  method caller =
    get {TipsQuery.P.caller with conv=GObj.conv_widget_option} obj
  method emit_always = get TipsQuery.P.emit_always obj
  method label_inactive = get TipsQuery.P.label_inactive obj
  method label_no_tip = get TipsQuery.P.label_no_tip obj
end

class virtual tips_query_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method start_query = self#connect TipsQuery.S.start_query
  method stop_query = self#connect TipsQuery.S.stop_query
  method widget_entered = self#connect
    {TipsQuery.S.widget_entered with marshaller = fun f ->
     marshal3 GObj.conv_widget_option string string
       "GtkTipsQuery::widget_entered"
       (fun x1 x2 x3 -> f x1 ~text:x2 ~privat:x3)}
  method widget_selected = self#connect
    {TipsQuery.S.widget_selected with marshaller = fun f ->
     marshal4_ret ~ret:boolean GObj.conv_widget_option string string
       (unsafe_pointer : GdkEvent.Button.t data_conv)
       "GtkTipsQuery::widget_selected"
       (fun x1 x2 x3 -> f x1 ~text:x2 ~privat:x3)}
end

class virtual arrow_props = object
  val virtual obj : _ obj
  method set_kind = set Arrow.P.arrow_type obj
  method set_shadow = set Arrow.P.shadow_type obj
  method kind = get Arrow.P.arrow_type obj
  method shadow = get Arrow.P.shadow_type obj
end

class virtual image_props = object
  val virtual obj : _ obj
  method set_file = set Image.P.file obj
  method set_image = set Image.P.image obj
  method set_pixbuf = set Image.P.pixbuf obj
  method set_pixel_size = set Image.P.pixel_size obj
  method set_mask = set Image.P.mask obj
  method set_stock = set Image.P.stock obj
  method set_icon_set = set Image.P.icon_set obj
  method set_icon_size = set Image.P.icon_size obj
  method image = get Image.P.image obj
  method pixbuf = get Image.P.pixbuf obj
  method pixel_size = get Image.P.pixel_size obj
  method mask = get Image.P.mask obj
  method stock = get Image.P.stock obj
  method icon_set = get Image.P.icon_set obj
  method icon_size = get Image.P.icon_size obj
  method storage_type = get Image.P.storage_type obj
end

class virtual color_selection_props = object
  val virtual obj : _ obj
  method set_alpha = set ColorSelection.P.current_alpha obj
  method set_color = set ColorSelection.P.current_color obj
  method set_has_opacity_control =
    set ColorSelection.P.has_opacity_control obj
  method set_has_palette = set ColorSelection.P.has_palette obj
  method alpha = get ColorSelection.P.current_alpha obj
  method color = get ColorSelection.P.current_color obj
  method has_opacity_control = get ColorSelection.P.has_opacity_control obj
  method has_palette = get ColorSelection.P.has_palette obj
end

class virtual font_selection_props = object
  val virtual obj : _ obj
  method set_font_name = set FontSelection.P.font_name obj
  method set_preview_text = set FontSelection.P.preview_text obj
  method font_name = get FontSelection.P.font_name obj
  method preview_text = get FontSelection.P.preview_text obj
end

class virtual calendar_props = object
  val virtual obj : _ obj
  method set_day = set Calendar.P.day obj
  method set_month = set Calendar.P.month obj
  method set_year = set Calendar.P.year obj
  method day = get Calendar.P.day obj
  method month = get Calendar.P.month obj
  method year = get Calendar.P.year obj
end

class virtual calendar_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method day_selected = self#connect Calendar.S.day_selected
  method day_selected_double_click =
    self#connect Calendar.S.day_selected_double_click
  method month_changed = self#connect Calendar.S.month_changed
  method next_month = self#connect Calendar.S.next_month
  method next_year = self#connect Calendar.S.next_year
  method prev_month = self#connect Calendar.S.prev_month
  method prev_year = self#connect Calendar.S.prev_year
end

class virtual curve_props = object
  val virtual obj : _ obj
  method set_curve_type = set Curve.P.curve_type obj
  method set_max_x = set Curve.P.max_x obj
  method set_max_y = set Curve.P.max_y obj
  method set_min_x = set Curve.P.min_x obj
  method set_min_y = set Curve.P.min_y obj
  method curve_type = get Curve.P.curve_type obj
  method max_x = get Curve.P.max_x obj
  method max_y = get Curve.P.max_y obj
  method min_x = get Curve.P.min_x obj
  method min_y = get Curve.P.min_y obj
end

