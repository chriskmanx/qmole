open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param

open GtkEditProps

open GtkEditProps

class virtual editable_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method changed = self#connect Editable.S.changed
  method delete_text = self#connect Editable.S.delete_text
  method insert_text = self#connect Editable.S.insert_text
end

class virtual entry_props = object
  val virtual obj : _ obj
  method set_text = set Entry.P.text obj
  method set_visibility = set Entry.P.visibility obj
  method set_max_length = set Entry.P.max_length obj
  method set_activates_default = set Entry.P.activates_default obj
  method set_has_frame = set Entry.P.has_frame obj
  method set_invisible_char = set Entry.P.invisible_char obj
  method set_width_chars = set Entry.P.width_chars obj
  method set_xalign = set Entry.P.xalign obj
  method text = get Entry.P.text obj
  method visibility = get Entry.P.visibility obj
  method max_length = get Entry.P.max_length obj
  method activates_default = get Entry.P.activates_default obj
  method has_frame = get Entry.P.has_frame obj
  method invisible_char = get Entry.P.invisible_char obj
  method scroll_offset = get Entry.P.scroll_offset obj
  method width_chars = get Entry.P.width_chars obj
  method xalign = get Entry.P.xalign obj
end

class virtual entry_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method activate = self#connect Entry.S.activate
  method copy_clipboard = self#connect Entry.S.copy_clipboard
  method cut_clipboard = self#connect Entry.S.cut_clipboard
  method delete_from_cursor = self#connect Entry.S.delete_from_cursor
  method insert_at_cursor = self#connect Entry.S.insert_at_cursor
  method move_cursor = self#connect Entry.S.move_cursor
  method paste_clipboard = self#connect Entry.S.paste_clipboard
  method toggle_overwrite = self#connect Entry.S.toggle_overwrite
end

class virtual spin_button_props = object
  val virtual obj : _ obj
  method set_adjustment =
    set {SpinButton.P.adjustment with conv=GData.conv_adjustment} obj
  method set_rate = set SpinButton.P.climb_rate obj
  method set_digits = set SpinButton.P.digits obj
  method set_numeric = set SpinButton.P.numeric obj
  method set_snap_to_ticks = set SpinButton.P.snap_to_ticks obj
  method set_update_policy = set SpinButton.P.update_policy obj
  method set_value = set SpinButton.P.value obj
  method set_wrap = set SpinButton.P.wrap obj
  method adjustment =
    get {SpinButton.P.adjustment with conv=GData.conv_adjustment} obj
  method rate = get SpinButton.P.climb_rate obj
  method digits = get SpinButton.P.digits obj
  method numeric = get SpinButton.P.numeric obj
  method snap_to_ticks = get SpinButton.P.snap_to_ticks obj
  method update_policy = get SpinButton.P.update_policy obj
  method value = get SpinButton.P.value obj
  method wrap = get SpinButton.P.wrap obj
end

class virtual spin_button_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method change_value = self#connect SpinButton.S.change_value
  method input = self#connect SpinButton.S.input
  method output = self#connect SpinButton.S.output
  method value_changed = self#connect SpinButton.S.value_changed
  method wrapped = self#connect SpinButton.S.wrapped
end

class virtual combo_props = object
  val virtual obj : _ obj
  method set_allow_empty = set Combo.P.allow_empty obj
  method set_case_sensitive = set Combo.P.case_sensitive obj
  method set_enable_arrow_keys = set Combo.P.enable_arrow_keys obj
  method set_value_in_list = set Combo.P.value_in_list obj
  method allow_empty = get Combo.P.allow_empty obj
  method case_sensitive = get Combo.P.case_sensitive obj
  method enable_arrow_keys = get Combo.P.enable_arrow_keys obj
  method value_in_list = get Combo.P.value_in_list obj
end

class virtual combo_box_props = object
  val virtual obj : _ obj
  method set_active = set ComboBox.P.active obj
  method set_add_tearoffs = set ComboBox.P.add_tearoffs obj
  method set_focus_on_click = set ComboBox.P.focus_on_click obj
  method set_has_frame = set ComboBox.P.has_frame obj
  method set_wrap_width = set ComboBox.P.wrap_width obj
  method active = get ComboBox.P.active obj
  method add_tearoffs = get ComboBox.P.add_tearoffs obj
  method focus_on_click = get ComboBox.P.focus_on_click obj
  method has_frame = get ComboBox.P.has_frame obj
  method wrap_width = get ComboBox.P.wrap_width obj
end

class virtual combo_box_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method changed = self#connect ComboBox.S.changed
end

