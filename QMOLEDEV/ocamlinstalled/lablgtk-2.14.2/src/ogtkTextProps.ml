open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param
open GtkTextProps

class virtual text_view_props = object
  val virtual obj : _ obj
  method set_editable = set TextView.P.editable obj
  method set_cursor_visible = set TextView.P.cursor_visible obj
  method set_indent = set TextView.P.indent obj
  method set_justification = set TextView.P.justification obj
  method set_left_margin = set TextView.P.left_margin obj
  method set_pixels_above_lines = set TextView.P.pixels_above_lines obj
  method set_pixels_below_lines = set TextView.P.pixels_below_lines obj
  method set_pixels_inside_wrap = set TextView.P.pixels_inside_wrap obj
  method set_right_margin = set TextView.P.right_margin obj
  method set_wrap_mode = set TextView.P.wrap_mode obj
  method set_accepts_tab = set TextView.P.accepts_tab obj
  method editable = get TextView.P.editable obj
  method cursor_visible = get TextView.P.cursor_visible obj
  method indent = get TextView.P.indent obj
  method justification = get TextView.P.justification obj
  method left_margin = get TextView.P.left_margin obj
  method pixels_above_lines = get TextView.P.pixels_above_lines obj
  method pixels_below_lines = get TextView.P.pixels_below_lines obj
  method pixels_inside_wrap = get TextView.P.pixels_inside_wrap obj
  method right_margin = get TextView.P.right_margin obj
  method wrap_mode = get TextView.P.wrap_mode obj
  method accepts_tab = get TextView.P.accepts_tab obj
end

class virtual text_view_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method copy_clipboard = self#connect TextView.S.copy_clipboard
  method cut_clipboard = self#connect TextView.S.cut_clipboard
  method delete_from_cursor = self#connect TextView.S.delete_from_cursor
  method insert_at_cursor = self#connect TextView.S.insert_at_cursor
  method move_cursor = self#connect TextView.S.move_cursor
  method move_focus = self#connect TextView.S.move_focus
  method page_horizontally = self#connect TextView.S.page_horizontally
  method paste_clipboard = self#connect TextView.S.paste_clipboard
  method populate_popup = self#connect TextView.S.populate_popup
  method set_anchor = self#connect TextView.S.set_anchor
  method set_scroll_adjustments = self#connect
    {TextView.S.set_scroll_adjustments with marshaller = fun f ->
     marshal2 GData.conv_adjustment_option GData.conv_adjustment_option
       "GtkTextView::set_scroll_adjustments" f}
  method toggle_overwrite = self#connect TextView.S.toggle_overwrite
end

class virtual text_buffer_props = object
  val virtual obj : _ obj
  method tag_table = get TextBuffer.P.tag_table obj
  method cursor_position = get TextBuffer.P.cursor_position obj
  method has_selection = get TextBuffer.P.has_selection obj
end

class virtual text_buffer_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method begin_user_action = self#connect TextBuffer.S.begin_user_action
  method changed = self#connect TextBuffer.S.changed
  method end_user_action = self#connect TextBuffer.S.end_user_action
  method mark_deleted = self#connect TextBuffer.S.mark_deleted
  method modified_changed = self#connect TextBuffer.S.modified_changed
end

let text_tag_param = function
  | `BACKGROUND p -> param TextTag.P.background p
  | `BACKGROUND_FULL_HEIGHT p -> param TextTag.P.background_full_height p
  | `BACKGROUND_FULL_HEIGHT_SET p ->
      param TextTag.P.background_full_height_set p
  | `BACKGROUND_GDK p -> param TextTag.P.background_gdk p
  | `BACKGROUND_SET p -> param TextTag.P.background_set p
  | `BACKGROUND_STIPPLE p -> param TextTag.P.background_stipple p
  | `BACKGROUND_STIPPLE_SET p -> param TextTag.P.background_stipple_set p
  | `DIRECTION p -> param TextTag.P.direction p
  | `EDITABLE p -> param TextTag.P.editable p
  | `EDITABLE_SET p -> param TextTag.P.editable_set p
  | `FAMILY p -> param TextTag.P.family p
  | `FAMILY_SET p -> param TextTag.P.family_set p
  | `FONT p -> param TextTag.P.font p
  | `FONT_DESC p -> param TextTag.P.font_desc p
  | `FOREGROUND p -> param TextTag.P.foreground p
  | `FOREGROUND_GDK p -> param TextTag.P.foreground_gdk p
  | `FOREGROUND_SET p -> param TextTag.P.foreground_set p
  | `FOREGROUND_STIPPLE p -> param TextTag.P.foreground_stipple p
  | `FOREGROUND_STIPPLE_SET p -> param TextTag.P.foreground_stipple_set p
  | `INDENT p -> param TextTag.P.indent p
  | `INDENT_SET p -> param TextTag.P.indent_set p
  | `INVISIBLE p -> param TextTag.P.invisible p
  | `INVISIBLE_SET p -> param TextTag.P.invisible_set p
  | `JUSTIFICATION p -> param TextTag.P.justification p
  | `JUSTIFICATION_SET p -> param TextTag.P.justification_set p
  | `LANGUAGE p -> param TextTag.P.language p
  | `LANGUAGE_SET p -> param TextTag.P.language_set p
  | `LEFT_MARGIN p -> param TextTag.P.left_margin p
  | `LEFT_MARGIN_SET p -> param TextTag.P.left_margin_set p
  | `PIXELS_ABOVE_LINES p -> param TextTag.P.pixels_above_lines p
  | `PIXELS_ABOVE_LINES_SET p -> param TextTag.P.pixels_above_lines_set p
  | `PIXELS_BELOW_LINES p -> param TextTag.P.pixels_below_lines p
  | `PIXELS_BELOW_LINES_SET p -> param TextTag.P.pixels_below_lines_set p
  | `PIXELS_INSIDE_WRAP p -> param TextTag.P.pixels_inside_wrap p
  | `PIXELS_INSIDE_WRAP_SET p -> param TextTag.P.pixels_inside_wrap_set p
  | `RIGHT_MARGIN p -> param TextTag.P.right_margin p
  | `RIGHT_MARGIN_SET p -> param TextTag.P.right_margin_set p
  | `RISE p -> param TextTag.P.rise p
  | `RISE_SET p -> param TextTag.P.rise_set p
  | `SCALE p -> param TextTag.P.scale p
  | `SCALE_SET p -> param TextTag.P.scale_set p
  | `SIZE p -> param TextTag.P.size p
  | `SIZE_POINTS p -> param TextTag.P.size_points p
  | `SIZE_SET p -> param TextTag.P.size_set p
  | `STRETCH p -> param TextTag.P.stretch p
  | `STRETCH_SET p -> param TextTag.P.stretch_set p
  | `STRIKETHROUGH p -> param TextTag.P.strikethrough p
  | `STRIKETHROUGH_SET p -> param TextTag.P.strikethrough_set p
  | `STYLE p -> param TextTag.P.style p
  | `STYLE_SET p -> param TextTag.P.style_set p
  | `TABS_SET p -> param TextTag.P.tabs_set p
  | `UNDERLINE p -> param TextTag.P.underline p
  | `UNDERLINE_SET p -> param TextTag.P.underline_set p
  | `VARIANT p -> param TextTag.P.variant p
  | `VARIANT_SET p -> param TextTag.P.variant_set p
  | `WEIGHT p -> param TextTag.P.weight p
  | `WEIGHT_SET p -> param TextTag.P.weight_set p
  | `WRAP_MODE p -> param TextTag.P.wrap_mode p
  | `WRAP_MODE_SET p -> param TextTag.P.wrap_mode_set p

class virtual text_tag_table_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method tag_added = self#connect TextTagTable.S.tag_added
  method tag_changed = self#connect TextTagTable.S.tag_changed
  method tag_removed = self#connect TextTagTable.S.tag_removed
end

