open Gobject
open Data
module Object = GtkObject

open Gtk

module PrivateProps = struct
  let editable = {name="editable"; conv=boolean}
  let indent = {name="indent"; conv=int}
  let justification =
    {name="justification"; conv=GtkEnums.justification_conv}
  let left_margin = {name="left-margin"; conv=int}
  let pixels_above_lines = {name="pixels-above-lines"; conv=int}
  let pixels_below_lines = {name="pixels-below-lines"; conv=int}
  let pixels_inside_wrap = {name="pixels-inside-wrap"; conv=int}
  let right_margin = {name="right-margin"; conv=int}
  let wrap_mode = {name="wrap-mode"; conv=GtkEnums.wrap_mode_conv}
end

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module TextView = struct
  let cast w : Gtk.text_view obj = try_cast w "GtkTextView"
  module P = struct
    let editable : ([>`textview],_) property = PrivateProps.editable
    let cursor_visible : ([>`textview],_) property =
      {name="cursor-visible"; conv=boolean}
    let indent : ([>`textview],_) property = PrivateProps.indent
    let justification : ([>`textview],_) property = PrivateProps.justification
    let left_margin : ([>`textview],_) property = PrivateProps.left_margin
    let pixels_above_lines : ([>`textview],_) property = PrivateProps.pixels_above_lines
    let pixels_below_lines : ([>`textview],_) property = PrivateProps.pixels_below_lines
    let pixels_inside_wrap : ([>`textview],_) property = PrivateProps.pixels_inside_wrap
    let right_margin : ([>`textview],_) property = PrivateProps.right_margin
    let wrap_mode : ([>`textview],_) property = PrivateProps.wrap_mode
    let accepts_tab : ([>`textview],_) property =
      {name="accepts-tab"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let copy_clipboard =
      {name="copy_clipboard"; classe=`textview; marshaller=marshal_unit}
    let cut_clipboard =
      {name="cut_clipboard"; classe=`textview; marshaller=marshal_unit}
    let delete_from_cursor =
      {name="delete_from_cursor"; classe=`textview; marshaller=fun f ->
       marshal2 GtkEnums.delete_type_conv int
         "GtkTextView::delete_from_cursor" f}
    let insert_at_cursor =
      {name="insert_at_cursor"; classe=`textview; marshaller=fun f ->
       marshal1 string "GtkTextView::insert_at_cursor" f}
    let move_cursor =
      {name="move_cursor"; classe=`textview; marshaller=fun f ->
       marshal3 GtkEnums.movement_step_conv int boolean
         "GtkTextView::move_cursor" (fun x1 x2 x3 -> f x1 x2 ~extend:x3)}
    let move_focus =
      {name="move_focus"; classe=`textview; marshaller=fun f ->
       marshal1 GtkEnums.direction_type_conv "GtkTextView::move_focus" f}
    let page_horizontally =
      {name="page_horizontally"; classe=`textview; marshaller=fun f ->
       marshal2 int boolean "GtkTextView::page_horizontally"
         (fun x1 x2 -> f x1 ~extend:x2)}
    let paste_clipboard =
      {name="paste_clipboard"; classe=`textview; marshaller=marshal_unit}
    let populate_popup =
      {name="populate_popup"; classe=`textview; marshaller=fun f ->
       marshal1 (gobject : Gtk.menu obj data_conv)
         "GtkTextView::populate_popup" f}
    let set_anchor =
      {name="set_anchor"; classe=`textview; marshaller=marshal_unit}
    let set_scroll_adjustments =
      {name="set_scroll_adjustments"; classe=`textview; marshaller=fun f ->
       marshal2 (gobject_option : Gtk.adjustment obj option data_conv)
         (gobject_option : Gtk.adjustment obj option data_conv)
         "GtkTextView::set_scroll_adjustments" f}
    let toggle_overwrite =
      {name="toggle_overwrite"; classe=`textview; marshaller=marshal_unit}
  end
  let create pl : Gtk.text_view obj = Object.make "GtkTextView" pl
  let make_params ~cont pl ?editable ?cursor_visible ?justification
      ?wrap_mode ?accepts_tab =
    let pl = (
      may_cons P.editable editable (
      may_cons P.cursor_visible cursor_visible (
      may_cons P.justification justification (
      may_cons P.wrap_mode wrap_mode (
      may_cons P.accepts_tab accepts_tab pl))))) in
    cont pl
end

module TextBuffer = struct
  let cast w : text_buffer = try_cast w "GtkTextBuffer"
  module P = struct
    let tag_table : ([>`textbuffer],_) property =
      {name="tag-table"; conv=(gobject : text_tag_table data_conv)}
    let cursor_position : ([>`textbuffer],_) property =
      {name="cursor-position"; conv=int}
    let has_selection : ([>`textbuffer],_) property =
      {name="has-selection"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let apply_tag =
      {name="apply_tag"; classe=`textbuffer; marshaller=fun f ->
       marshal3 (gobject : text_tag data_conv)
         (unsafe_pointer : Gtk.text_iter data_conv)
         (unsafe_pointer : Gtk.text_iter data_conv)
         "GtkTextBuffer::apply_tag" f}
    let begin_user_action =
      {name="begin_user_action"; classe=`textbuffer; marshaller=marshal_unit}
    let changed =
      {name="changed"; classe=`textbuffer; marshaller=marshal_unit}
    let delete_range =
      {name="delete_range"; classe=`textbuffer; marshaller=fun f ->
       marshal2 (unsafe_pointer : Gtk.text_iter data_conv)
         (unsafe_pointer : Gtk.text_iter data_conv)
         "GtkTextBuffer::delete_range" f}
    let end_user_action =
      {name="end_user_action"; classe=`textbuffer; marshaller=marshal_unit}
    let insert_child_anchor =
      {name="insert_child_anchor"; classe=`textbuffer; marshaller=fun f ->
       marshal2 (unsafe_pointer : Gtk.text_iter data_conv)
         (gobject : text_child_anchor data_conv)
         "GtkTextBuffer::insert_child_anchor" f}
    let insert_pixbuf =
      {name="insert_pixbuf"; classe=`textbuffer; marshaller=fun f ->
       marshal2 (unsafe_pointer : Gtk.text_iter data_conv)
         (gobject : GdkPixbuf.pixbuf data_conv)
         "GtkTextBuffer::insert_pixbuf" f}
    let insert_text =
      {name="insert_text"; classe=`textbuffer; marshaller=fun f ->
       marshal2 (unsafe_pointer : Gtk.text_iter data_conv) string
         "GtkTextBuffer::insert_text" f}
    let mark_deleted =
      {name="mark_deleted"; classe=`textbuffer; marshaller=fun f ->
       marshal1 (gobject : text_mark data_conv)
         "GtkTextBuffer::mark_deleted" f}
    let mark_set =
      {name="mark_set"; classe=`textbuffer; marshaller=fun f ->
       marshal2 (unsafe_pointer : Gtk.text_iter data_conv)
         (gobject : text_mark data_conv) "GtkTextBuffer::mark_set" f}
    let modified_changed =
      {name="modified_changed"; classe=`textbuffer; marshaller=marshal_unit}
    let remove_tag =
      {name="remove_tag"; classe=`textbuffer; marshaller=fun f ->
       marshal3 (gobject : text_tag data_conv)
         (unsafe_pointer : Gtk.text_iter data_conv)
         (unsafe_pointer : Gtk.text_iter data_conv)
         "GtkTextBuffer::remove_tag" f}
  end
  let create ?tag_table pl : text_buffer =
    let pl = (may_cons P.tag_table tag_table pl) in
    Gobject.unsafe_create "GtkTextBuffer" pl
end

module TextChildAnchor = struct
  let cast w : text_child_anchor = try_cast w "GtkTextChildAnchor"
  let create pl : text_child_anchor =
    Gobject.unsafe_create "GtkTextChildAnchor" pl
  external get_widgets : [>`textchildanchor] obj -> widget obj list
    = "ml_gtk_text_child_anchor_get_widgets"
  external get_deleted : [>`textchildanchor] obj -> bool
    = "ml_gtk_text_child_anchor_get_deleted"
end

module TextMark = struct
  let cast w : text_mark = try_cast w "GtkTextMark"
  let create pl : text_mark = Gobject.unsafe_create "GtkTextMark" pl
  external set_visible : [>`textmark] obj -> bool -> unit
    = "ml_gtk_text_mark_set_visible"
  external get_visible : [>`textmark] obj -> bool
    = "ml_gtk_text_mark_get_visible"
  external get_deleted : [>`textmark] obj -> bool
    = "ml_gtk_text_mark_get_deleted"
  external get_name : [>`textmark] obj -> string option
    = "ml_gtk_text_mark_get_name"
  external get_buffer : [>`textmark] obj -> text_buffer option
    = "ml_gtk_text_mark_get_buffer"
  external get_left_gravity : [>`textmark] obj -> bool
    = "ml_gtk_text_mark_get_left_gravity"
end

module TextTag = struct
  let cast w : text_tag = try_cast w "GtkTextTag"
  module P = struct
    let background : ([>`texttag],_) property =
      {name="background"; conv=string}
    let background_full_height : ([>`texttag],_) property =
      {name="background-full-height"; conv=boolean}
    let background_full_height_set : ([>`texttag],_) property =
      {name="background-full-height-set"; conv=boolean}
    let background_gdk : ([>`texttag],_) property =
      {name="background-gdk"; conv=(unsafe_pointer : Gdk.color data_conv)}
    let background_set : ([>`texttag],_) property =
      {name="background-set"; conv=boolean}
    let background_stipple : ([>`texttag],_) property =
      {name="background-stipple"; conv=(gobject : Gdk.bitmap data_conv)}
    let background_stipple_set : ([>`texttag],_) property =
      {name="background-stipple-set"; conv=boolean}
    let direction : ([>`texttag],_) property =
      {name="direction"; conv=GtkEnums.text_direction_conv}
    let editable : ([>`texttag],_) property = PrivateProps.editable
    let editable_set : ([>`texttag],_) property =
      {name="editable-set"; conv=boolean}
    let family : ([>`texttag],_) property = {name="family"; conv=string}
    let family_set : ([>`texttag],_) property =
      {name="family-set"; conv=boolean}
    let font : ([>`texttag],_) property = {name="font"; conv=string}
    let font_desc : ([>`texttag],_) property =
      {name="font-desc";
       conv=(unsafe_pointer : Pango.font_description data_conv)}
    let foreground : ([>`texttag],_) property =
      {name="foreground"; conv=string}
    let foreground_gdk : ([>`texttag],_) property =
      {name="foreground-gdk"; conv=(unsafe_pointer : Gdk.color data_conv)}
    let foreground_set : ([>`texttag],_) property =
      {name="foreground-set"; conv=boolean}
    let foreground_stipple : ([>`texttag],_) property =
      {name="foreground-stipple"; conv=(gobject : Gdk.bitmap data_conv)}
    let foreground_stipple_set : ([>`texttag],_) property =
      {name="foreground-stipple-set"; conv=boolean}
    let indent : ([>`texttag],_) property = PrivateProps.indent
    let indent_set : ([>`texttag],_) property =
      {name="indent-set"; conv=boolean}
    let invisible : ([>`texttag],_) property =
      {name="invisible"; conv=boolean}
    let invisible_set : ([>`texttag],_) property =
      {name="invisible-set"; conv=boolean}
    let justification : ([>`texttag],_) property = PrivateProps.justification
    let justification_set : ([>`texttag],_) property =
      {name="justification-set"; conv=boolean}
    let language : ([>`texttag],_) property = {name="language"; conv=string}
    let language_set : ([>`texttag],_) property =
      {name="language-set"; conv=boolean}
    let left_margin : ([>`texttag],_) property = PrivateProps.left_margin
    let left_margin_set : ([>`texttag],_) property =
      {name="left-margin-set"; conv=boolean}
    let name : ([>`texttag],_) property = {name="name"; conv=string}
    let pixels_above_lines : ([>`texttag],_) property = PrivateProps.pixels_above_lines
    let pixels_above_lines_set : ([>`texttag],_) property =
      {name="pixels-above-lines-set"; conv=boolean}
    let pixels_below_lines : ([>`texttag],_) property = PrivateProps.pixels_below_lines
    let pixels_below_lines_set : ([>`texttag],_) property =
      {name="pixels-below-lines-set"; conv=boolean}
    let pixels_inside_wrap : ([>`texttag],_) property = PrivateProps.pixels_inside_wrap
    let pixels_inside_wrap_set : ([>`texttag],_) property =
      {name="pixels-inside-wrap-set"; conv=boolean}
    let right_margin : ([>`texttag],_) property = PrivateProps.right_margin
    let right_margin_set : ([>`texttag],_) property =
      {name="right-margin-set"; conv=boolean}
    let rise : ([>`texttag],_) property = {name="rise"; conv=int}
    let rise_set : ([>`texttag],_) property = {name="rise-set"; conv=boolean}
    let scale : ([>`texttag],_) property = {name="scale"; conv=double}
    let scale_set : ([>`texttag],_) property =
      {name="scale-set"; conv=boolean}
    let size : ([>`texttag],_) property = {name="size"; conv=int}
    let size_points : ([>`texttag],_) property =
      {name="size-points"; conv=double}
    let size_set : ([>`texttag],_) property = {name="size-set"; conv=boolean}
    let stretch : ([>`texttag],_) property =
      {name="stretch"; conv=PangoEnums.stretch_conv}
    let stretch_set : ([>`texttag],_) property =
      {name="stretch-set"; conv=boolean}
    let strikethrough : ([>`texttag],_) property =
      {name="strikethrough"; conv=boolean}
    let strikethrough_set : ([>`texttag],_) property =
      {name="strikethrough-set"; conv=boolean}
    let style : ([>`texttag],_) property =
      {name="style"; conv=PangoEnums.style_conv}
    let style_set : ([>`texttag],_) property =
      {name="style-set"; conv=boolean}
    let tabs_set : ([>`texttag],_) property = {name="tabs-set"; conv=boolean}
    let underline : ([>`texttag],_) property =
      {name="underline"; conv=PangoEnums.underline_conv}
    let underline_set : ([>`texttag],_) property =
      {name="underline-set"; conv=boolean}
    let variant : ([>`texttag],_) property =
      {name="variant"; conv=PangoEnums.variant_conv}
    let variant_set : ([>`texttag],_) property =
      {name="variant-set"; conv=boolean}
    let weight : ([>`texttag],_) property = {name="weight"; conv=int}
    let weight_set : ([>`texttag],_) property =
      {name="weight-set"; conv=boolean}
    let wrap_mode : ([>`texttag],_) property = PrivateProps.wrap_mode
    let wrap_mode_set : ([>`texttag],_) property =
      {name="wrap-mode-set"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let event =
      {name="event"; classe=`texttag; marshaller=fun f ->
       marshal3_ret ~ret:boolean (gobject : unit obj data_conv)
         (unsafe_pointer : GdkEvent.any data_conv)
         (unsafe_pointer : Gtk.text_iter data_conv) "GtkTextTag::event"
         (fun x1 -> f ~origin:x1)}
  end
  let create ?name pl : text_tag =
    let pl = (may_cons P.name name pl) in
    Gobject.unsafe_create "GtkTextTag" pl
  external get_priority : [>`texttag] obj -> int
    = "ml_gtk_text_tag_get_priority"
  external set_priority : [>`texttag] obj -> int -> unit
    = "ml_gtk_text_tag_set_priority"
  external event :
    [>`texttag] obj -> 'a obj ->  'b Gdk.event -> text_iter -> bool
    = "ml_gtk_text_tag_event"
end

module TextTagTable = struct
  let cast w : text_tag_table = try_cast w "GtkTextTagTable"
  module S = struct
    open GtkSignal
    let tag_added =
      {name="tag_added"; classe=`texttagtable; marshaller=fun f ->
       marshal1 (gobject : text_tag data_conv) "GtkTextTagTable::tag_added" f}
    let tag_changed =
      {name="tag_changed"; classe=`texttagtable; marshaller=fun f ->
       marshal2 (gobject : text_tag data_conv) boolean
         "GtkTextTagTable::tag_changed" (fun x1 x2 -> f x1 ~size:x2)}
    let tag_removed =
      {name="tag_removed"; classe=`texttagtable; marshaller=fun f ->
       marshal1 (gobject : text_tag data_conv)
         "GtkTextTagTable::tag_removed" f}
  end
  let create pl : text_tag_table = Gobject.unsafe_create "GtkTextTagTable" pl
  external add : [>`texttagtable] obj -> text_tag -> unit
    = "ml_gtk_text_tag_table_add"
  external remove : [>`texttagtable] obj -> text_tag -> unit
    = "ml_gtk_text_tag_table_remove"
  external lookup : [>`texttagtable] obj -> string -> text_tag option
    = "ml_gtk_text_tag_table_lookup"
  external get_size : [>`texttagtable] obj -> int
    = "ml_gtk_text_tag_table_get_size"
end

