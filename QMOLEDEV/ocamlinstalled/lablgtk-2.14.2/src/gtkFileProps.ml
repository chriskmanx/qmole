open Gobject
open Data
module Object = GtkObject
let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module FileChooser = struct
  let cast w : Gtk.file_chooser obj = try_cast w "GtkFileChooser"
  module P = struct
    let action : ([>`filechooser],_) property =
      {name="action"; conv=GtkEnums.file_chooser_action_conv}
    let extra_widget : ([>`filechooser],_) property =
      {name="extra-widget"; conv=(gobject : Gtk.widget obj data_conv)}
    let file_system_backend : ([>`filechooser],_) property =
      {name="file-system-backend"; conv=string}
    let filter : ([>`filechooser],_) property =
      {name="filter"; conv=(gobject : Gtk.file_filter  Gtk.obj data_conv)}
    let local_only : ([>`filechooser],_) property =
      {name="local-only"; conv=boolean}
    let preview_widget : ([>`filechooser],_) property =
      {name="preview-widget"; conv=(gobject : Gtk.widget obj data_conv)}
    let preview_widget_active : ([>`filechooser],_) property =
      {name="preview-widget-active"; conv=boolean}
    let select_multiple : ([>`filechooser],_) property =
      {name="select-multiple"; conv=boolean}
    let show_hidden : ([>`filechooser],_) property =
      {name="show-hidden"; conv=boolean}
    let use_preview_label : ([>`filechooser],_) property =
      {name="use-preview-label"; conv=boolean}
    let do_overwrite_confirmation : ([>`filechooser],_) property =
      {name="do-overwrite-confirmation"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let current_folder_changed =
      {name="current_folder_changed"; classe=`filechooser;
       marshaller=marshal_unit}
    let file_activated =
      {name="file_activated"; classe=`filechooser; marshaller=marshal_unit}
    let selection_changed =
      {name="selection_changed"; classe=`filechooser;
       marshaller=marshal_unit}
    let update_preview =
      {name="update_preview"; classe=`filechooser; marshaller=marshal_unit}
    let confirm_overwrite =
      {name="confirm_overwrite"; classe=`filechooser;
       marshaller=fun f -> marshal0_ret ~ret:GtkEnums.file_chooser_confirmation_conv f}
  end
end

module FileChooserButton = struct
  let cast w : Gtk.file_chooser_button obj =
    try_cast w "GtkFileChooserButton"
  module P = struct
    let title : ([>`filechooserbutton],_) property =
      {name="title"; conv=string}
    let width_chars : ([>`filechooserbutton],_) property =
      {name="width-chars"; conv=int}
  end
  let create pl : Gtk.file_chooser_button obj =
    Object.make "GtkFileChooserButton" pl
end

