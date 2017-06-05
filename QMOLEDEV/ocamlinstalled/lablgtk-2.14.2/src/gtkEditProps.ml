open Gobject
open Data
module Object = GtkObject

open Gtk
module Internal = struct
  let marshal_insert =
    ref (fun (_ : string -> pos:int ref -> unit) ->
           failwith "GtkEditProps.Internal.marshal_insert")
end

module PrivateProps = struct
  let has_frame = {name="has-frame"; conv=boolean}
  let model = {name="model"; conv=(gobject : Gtk.tree_model data_conv)}
end

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module Editable = struct
  let cast w : Gtk.editable obj = try_cast w "GtkEditable"
  module S = struct
    open GtkSignal
    let changed = {name="changed"; classe=`editable; marshaller=marshal_unit}
    let delete_text =
      {name="delete_text"; classe=`editable; marshaller=fun f ->
       marshal2 int int "GtkEditable::delete_text"
         (fun x1 x2 -> f ~start:x1 ~stop:x2)}
    let insert_text =
      {name="insert_text"; classe=`editable;
       marshaller=(fun f -> !Internal.marshal_insert f)}
  end
  external select_region : [>`editable] obj -> start:int -> stop:int -> unit
    = "ml_gtk_editable_select_region"
  external get_selection_bounds : [>`editable] obj -> (int * int) option
    = "ml_gtk_editable_get_selection_bounds"
  external insert_text : [>`editable] obj -> string -> pos:int -> int
    = "ml_gtk_editable_insert_text"
  external delete_text : [>`editable] obj -> start:int -> stop:int -> unit
    = "ml_gtk_editable_delete_text"
  external get_chars : [>`editable] obj -> start:int -> stop:int -> string
    = "ml_gtk_editable_get_chars"
  external cut_clipboard : [>`editable] obj -> unit
    = "ml_gtk_editable_cut_clipboard"
  external copy_clipboard : [>`editable] obj -> unit
    = "ml_gtk_editable_copy_clipboard"
  external paste_clipboard : [>`editable] obj -> unit
    = "ml_gtk_editable_paste_clipboard"
  external delete_selection : [>`editable] obj -> unit
    = "ml_gtk_editable_delete_selection"
  external get_position : [>`editable] obj -> int
    = "ml_gtk_editable_get_position"
  external set_position : [>`editable] obj -> int -> unit
    = "ml_gtk_editable_set_position"
  external get_editable : [>`editable] obj -> bool
    = "ml_gtk_editable_get_editable"
  external set_editable : [>`editable] obj -> bool -> unit
    = "ml_gtk_editable_set_editable"
end

module Entry = struct
  let cast w : Gtk.entry obj = try_cast w "GtkEntry"
  module P = struct
    let text : ([>`entry],_) property = {name="text"; conv=string}
    let visibility : ([>`entry],_) property =
      {name="visibility"; conv=boolean}
    let max_length : ([>`entry],_) property = {name="max-length"; conv=int}
    let activates_default : ([>`entry],_) property =
      {name="activates-default"; conv=boolean}
    let cursor_position : ([>`entry],_) property =
      {name="cursor-position"; conv=int}
    let editable : ([>`entry],_) property = {name="editable"; conv=boolean}
    let has_frame : ([>`entry],_) property = PrivateProps.has_frame
    let invisible_char : ([>`entry],_) property =
      {name="invisible-char"; conv=uint}
    let scroll_offset : ([>`entry],_) property =
      {name="scroll-offset"; conv=int}
    let selection_bound : ([>`entry],_) property =
      {name="selection-bound"; conv=int}
    let width_chars : ([>`entry],_) property = {name="width-chars"; conv=int}
    let xalign : ([>`entry],_) property = {name="xalign"; conv=float}
  end
  module S = struct
    open GtkSignal
    let activate = {name="activate"; classe=`entry; marshaller=marshal_unit}
    let copy_clipboard =
      {name="copy_clipboard"; classe=`entry; marshaller=marshal_unit}
    let cut_clipboard =
      {name="cut_clipboard"; classe=`entry; marshaller=marshal_unit}
    let delete_from_cursor =
      {name="delete_from_cursor"; classe=`entry; marshaller=fun f ->
       marshal2 GtkEnums.delete_type_conv int
         "GtkEntry::delete_from_cursor" f}
    let insert_at_cursor =
      {name="insert_at_cursor"; classe=`entry; marshaller=fun f ->
       marshal1 string "GtkEntry::insert_at_cursor" f}
    let move_cursor =
      {name="move_cursor"; classe=`entry; marshaller=fun f ->
       marshal3 GtkEnums.movement_step_conv int boolean
         "GtkEntry::move_cursor" (fun x1 x2 x3 -> f x1 x2 ~extend:x3)}
    let paste_clipboard =
      {name="paste_clipboard"; classe=`entry; marshaller=marshal_unit}
    let populate_popup =
      {name="populate_popup"; classe=`entry; marshaller=fun f ->
       marshal1 (gobject : Gtk.menu obj data_conv)
         "GtkEntry::populate_popup" f}
    let toggle_overwrite =
      {name="toggle_overwrite"; classe=`entry; marshaller=marshal_unit}
  end
  let create pl : Gtk.entry obj = Object.make "GtkEntry" pl
  external append_text : [>`entry] obj -> string -> unit
    = "ml_gtk_entry_append_text"
  external prepend_text : [>`entry] obj -> string -> unit
    = "ml_gtk_entry_prepend_text"
  external text_length : [>`entry] obj -> int = "ml_gtk_entry_text_length"
  external get_completion : [>`entry] obj -> Gtk.entry_completion option
    = "ml_gtk_entry_get_completion"
  external set_completion : [>`entry] obj -> Gtk.entry_completion -> unit
    = "ml_gtk_entry_set_completion"
  let make_params ~cont pl ?text ?visibility ?max_length ?activates_default
      ?editable ?has_frame ?width_chars ?xalign =
    let pl = (
      may_cons P.text text (
      may_cons P.visibility visibility (
      may_cons P.max_length max_length (
      may_cons P.activates_default activates_default (
      may_cons P.editable editable (
      may_cons P.has_frame has_frame (
      may_cons P.width_chars width_chars (
      may_cons P.xalign xalign pl)))))))) in
    cont pl
end

module SpinButton = struct
  let cast w : Gtk.spin_button obj = try_cast w "GtkSpinButton"
  module P = struct
    let adjustment : ([>`spinbutton],_) property =
      {name="adjustment"; conv=(gobject : Gtk.adjustment obj data_conv)}
    let climb_rate : ([>`spinbutton],_) property =
      {name="climb-rate"; conv=double}
    let digits : ([>`spinbutton],_) property = {name="digits"; conv=uint}
    let numeric : ([>`spinbutton],_) property =
      {name="numeric"; conv=boolean}
    let snap_to_ticks : ([>`spinbutton],_) property =
      {name="snap-to-ticks"; conv=boolean}
    let update_policy : ([>`spinbutton],_) property =
      {name="update-policy"; conv=GtkEnums.spin_button_update_policy_conv}
    let value : ([>`spinbutton],_) property = {name="value"; conv=double}
    let wrap : ([>`spinbutton],_) property = {name="wrap"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let change_value =
      {name="change_value"; classe=`spinbutton; marshaller=fun f ->
       marshal1 GtkEnums.scroll_type_conv "GtkSpinButton::change_value" f}
    let input =
      {name="input"; classe=`spinbutton;
       marshaller=fun f -> marshal0_ret ~ret:int f}
    let output =
      {name="output"; classe=`spinbutton;
       marshaller=fun f -> marshal0_ret ~ret:boolean f}
    let value_changed =
      {name="value_changed"; classe=`spinbutton; marshaller=marshal_unit}
    let wrapped =
      {name="wrapped"; classe=`spinbutton; marshaller=marshal_unit}
  end
  let create pl : Gtk.spin_button obj = Object.make "GtkSpinButton" pl
  external spin : [>`spinbutton] obj -> Tags.spin_type -> unit
    = "ml_gtk_spin_button_spin"
  external update : [>`spinbutton] obj -> unit = "ml_gtk_spin_button_update"
  let make_params ~cont pl ?adjustment ?rate ?digits ?numeric ?snap_to_ticks
      ?update_policy ?value ?wrap =
    let pl = (
      may_cons P.adjustment adjustment (
      may_cons P.climb_rate rate (
      may_cons P.digits digits (
      may_cons P.numeric numeric (
      may_cons P.snap_to_ticks snap_to_ticks (
      may_cons P.update_policy update_policy (
      may_cons P.value value (
      may_cons P.wrap wrap pl)))))))) in
    cont pl
end

module Combo = struct
  let cast w : Gtk.combo obj = try_cast w "GtkCombo"
  module P = struct
    let allow_empty : ([>`combo],_) property =
      {name="allow-empty"; conv=boolean}
    let case_sensitive : ([>`combo],_) property =
      {name="case-sensitive"; conv=boolean}
    let enable_arrow_keys : ([>`combo],_) property =
      {name="enable-arrow-keys"; conv=boolean}
    let enable_arrows_always : ([>`combo],_) property =
      {name="enable-arrows-always"; conv=boolean}
    let value_in_list : ([>`combo],_) property =
      {name="value-in-list"; conv=boolean}
  end
  let create pl : Gtk.combo obj = Object.make "GtkCombo" pl
  let make_params ~cont pl ?allow_empty ?case_sensitive ?enable_arrow_keys
      ?value_in_list =
    let pl = (
      may_cons P.allow_empty allow_empty (
      may_cons P.case_sensitive case_sensitive (
      may_cons P.enable_arrow_keys enable_arrow_keys (
      may_cons P.value_in_list value_in_list pl)))) in
    cont pl
end

module ComboBox = struct
  let cast w : Gtk.combo_box obj = try_cast w "GtkComboBox"
  module P = struct
    let model : ([>`combobox],_) property = PrivateProps.model
    let active : ([>`combobox],_) property = {name="active"; conv=int}
    let add_tearoffs : ([>`combobox],_) property =
      {name="add-tearoffs"; conv=boolean}
    let column_span_column : ([>`combobox],_) property =
      {name="column-span-column"; conv=int}
    let focus_on_click : ([>`combobox],_) property =
      {name="focus-on-click"; conv=boolean}
    let has_frame : ([>`combobox],_) property = PrivateProps.has_frame
    let row_span_column : ([>`combobox],_) property =
      {name="row-span-column"; conv=int}
    let wrap_width : ([>`combobox],_) property =
      {name="wrap-width"; conv=int}
  end
  module S = struct
    open GtkSignal
    let changed = {name="changed"; classe=`combobox; marshaller=marshal_unit}
  end
  let create pl : Gtk.combo_box obj = Object.make "GtkComboBox" pl
  external get_active_iter : [>`combobox] obj -> Gtk.tree_iter option
    = "ml_gtk_combo_box_get_active_iter"
  external set_active_iter : [>`combobox] obj -> Gtk.tree_iter option -> unit
    = "ml_gtk_combo_box_set_active_iter"
  external set_row_separator_func :
    [>`combobox] obj ->
    (Gtk.tree_model -> Gtk.tree_iter -> bool) option -> unit
    = "ml_gtk_combo_box_set_row_separator_func"
  let make_params ~cont pl ?model ?active ?add_tearoffs ?focus_on_click
      ?has_frame ?wrap_width =
    let pl = (
      may_cons P.model model (
      may_cons P.active active (
      may_cons P.add_tearoffs add_tearoffs (
      may_cons P.focus_on_click focus_on_click (
      may_cons P.has_frame has_frame (
      may_cons P.wrap_width wrap_width pl)))))) in
    cont pl
end

module ComboBoxEntry = struct
  let cast w : Gtk.combo_box_entry obj = try_cast w "GtkComboBoxEntry"
  module P = struct
    let text_column : ([>`comboboxentry],_) property =
      {name="text-column"; conv=int}
  end
  let create pl : Gtk.combo_box_entry obj = Object.make "GtkComboBoxEntry" pl
end

module EntryCompletion = struct
  let cast w : entry_completion = try_cast w "GtkEntryCompletion"
  module P = struct
    let minimum_key_length : ([>`entrycompletion],_) property =
      {name="minimum-key-length"; conv=int}
    let model : ([>`entrycompletion],_) property = PrivateProps.model
  end
  module S = struct
    open GtkSignal
    let action_activated =
      {name="action_activated"; classe=`entrycompletion; marshaller=fun f ->
       marshal1 int "GtkEntryCompletion::action_activated" f}
    let match_selected =
      {name="match_selected"; classe=`entrycompletion; marshaller=fun f ->
       marshal2_ret ~ret:boolean (gobject : Gtk.tree_model_filter data_conv)
         (unsafe_pointer : Gtk.tree_iter data_conv)
         "GtkEntryCompletion::match_selected" f}
  end
  let create pl : entry_completion =
    Gobject.unsafe_create "GtkEntryCompletion" pl
  external get_entry : [>`entrycompletion] obj -> Gtk.entry obj option
    = "ml_gtk_entry_completion_get_entry"
  external complete : [>`entrycompletion] obj -> unit
    = "ml_gtk_entry_completion_complete"
  external insert_action_text :
    [>`entrycompletion] obj -> int -> string -> unit
    = "ml_gtk_entry_completion_insert_action_text"
  external insert_action_markup :
    [>`entrycompletion] obj -> int -> string -> unit
    = "ml_gtk_entry_completion_insert_action_markup"
  external delete_action : [>`entrycompletion] obj -> int -> unit
    = "ml_gtk_entry_completion_delete_action"
  external set_text_column : [>`entrycompletion] obj -> int -> unit
    = "ml_gtk_entry_completion_set_text_column"
  external set_match_func :
    [>`entrycompletion] obj -> (string -> Gtk.tree_iter -> bool) -> unit
    = "ml_gtk_entry_completion_set_match_func"
  let make_params ~cont pl ?minimum_key_length ?model =
    let pl = (
      may_cons P.minimum_key_length minimum_key_length (
      may_cons P.model model pl)) in
    cont pl
end

