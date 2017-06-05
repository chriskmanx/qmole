open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param
open GtkFileProps

class virtual file_chooser_props = object
  val virtual obj : _ obj
  method set_action = set FileChooser.P.action obj
  method set_extra_widget =
    set {FileChooser.P.extra_widget with conv=GObj.conv_widget} obj
  method set_local_only = set FileChooser.P.local_only obj
  method set_preview_widget =
    set {FileChooser.P.preview_widget with conv=GObj.conv_widget} obj
  method set_preview_widget_active =
    set FileChooser.P.preview_widget_active obj
  method set_select_multiple = set FileChooser.P.select_multiple obj
  method set_show_hidden = set FileChooser.P.show_hidden obj
  method set_use_preview_label = set FileChooser.P.use_preview_label obj
  method set_do_overwrite_confirmation =
    set FileChooser.P.do_overwrite_confirmation obj
  method action = get FileChooser.P.action obj
  method extra_widget =
    get {FileChooser.P.extra_widget with conv=GObj.conv_widget} obj
  method local_only = get FileChooser.P.local_only obj
  method preview_widget =
    get {FileChooser.P.preview_widget with conv=GObj.conv_widget} obj
  method preview_widget_active = get FileChooser.P.preview_widget_active obj
  method select_multiple = get FileChooser.P.select_multiple obj
  method show_hidden = get FileChooser.P.show_hidden obj
  method use_preview_label = get FileChooser.P.use_preview_label obj
  method do_overwrite_confirmation =
    get FileChooser.P.do_overwrite_confirmation obj
end

class virtual file_chooser_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method current_folder_changed =
    self#connect FileChooser.S.current_folder_changed
  method file_activated = self#connect FileChooser.S.file_activated
  method selection_changed = self#connect FileChooser.S.selection_changed
  method update_preview = self#connect FileChooser.S.update_preview
  method confirm_overwrite = self#connect FileChooser.S.confirm_overwrite
end

class virtual file_chooser_button_props = object
  val virtual obj : _ obj
  method set_title = set FileChooserButton.P.title obj
  method set_width_chars = set FileChooserButton.P.width_chars obj
  method title = get FileChooserButton.P.title obj
  method width_chars = get FileChooserButton.P.width_chars obj
end

