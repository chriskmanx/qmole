open Gobject
open Data
module Object = GtkObject

open Gtk

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module OptionMenu = struct
  let cast w : Gtk.option_menu obj = try_cast w "GtkOptionMenu"
  module P = struct
    let menu : ([>`optionmenu],_) property =
      {name="menu"; conv=(gobject : Gtk.menu obj data_conv)}
  end
  module S = struct
    open GtkSignal
    let changed =
      {name="changed"; classe=`optionmenu; marshaller=marshal_unit}
  end
  let create pl : Gtk.option_menu obj = Object.make "GtkOptionMenu" pl
  external remove_menu : [>`optionmenu] obj -> unit
    = "ml_gtk_option_menu_remove_menu"
  external set_history : [>`optionmenu] obj -> int -> unit
    = "ml_gtk_option_menu_set_history"
end

module MenuItem = struct
  let cast w : Gtk.menu_item obj = try_cast w "GtkMenuItem"
  module S = struct
    open GtkSignal
    let activate =
      {name="activate"; classe=`menuitem; marshaller=marshal_unit}
    let activate_item =
      {name="activate_item"; classe=`menuitem; marshaller=marshal_unit}
  end
  let create pl : Gtk.menu_item obj = Object.make "GtkMenuItem" pl
  external set_submenu : [>`menuitem] obj -> [>`menu] obj -> unit
    = "ml_gtk_menu_item_set_submenu"
  external get_submenu : [>`menuitem] obj -> widget obj option
    = "ml_gtk_menu_item_get_submenu"
  external remove_submenu : [>`menuitem] obj -> unit
    = "ml_gtk_menu_item_remove_submenu"
  external activate : [>`menuitem] obj -> unit = "ml_gtk_menu_item_activate"
  external select : [>`menuitem] obj -> unit = "ml_gtk_menu_item_select"
  external deselect : [>`menuitem] obj -> unit = "ml_gtk_menu_item_deselect"
  external set_right_justified : [>`menuitem] obj -> bool -> unit
    = "ml_gtk_menu_item_set_right_justified"
  external get_right_justified : [>`menuitem] obj -> bool
    = "ml_gtk_menu_item_get_right_justified"
  external set_accel_path : [>`menuitem] obj -> string -> unit
    = "ml_gtk_menu_item_set_accel_path"
  external toggle_size_request : [>`menuitem] obj -> int -> unit
    = "ml_gtk_menu_item_toggle_size_request"
  external toggle_size_allocate : [>`menuitem] obj -> int -> unit
    = "ml_gtk_menu_item_toggle_size_allocate"
end

module CheckMenuItem = struct
  let cast w : Gtk.check_menu_item obj = try_cast w "GtkCheckMenuItem"
  module P = struct
    let active : ([>`checkmenuitem],_) property =
      {name="active"; conv=boolean}
    let inconsistent : ([>`checkmenuitem],_) property =
      {name="inconsistent"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let toggled =
      {name="toggled"; classe=`checkmenuitem; marshaller=marshal_unit}
  end
  let create pl : Gtk.check_menu_item obj = Object.make "GtkCheckMenuItem" pl
  external set_show_toggle : [>`checkmenuitem] obj -> bool -> unit
    = "ml_gtk_check_menu_item_set_show_toggle"
  external toggled : [>`checkmenuitem] obj -> unit
    = "ml_gtk_check_menu_item_toggled"
end

module RadioMenuItem = struct
  let cast w : Gtk.radio_menu_item obj = try_cast w "GtkRadioMenuItem"
  external set_group : [>`radiomenuitem] obj -> radio_menu_item group -> unit
    = "ml_gtk_radio_menu_item_set_group"
end

module ImageMenuItem = struct
  let cast w : Gtk.image_menu_item obj = try_cast w "GtkImageMenuItem"
  module P = struct
    let image : ([>`imagemenuitem],_) property =
      {name="image"; conv=(gobject : Gtk.widget obj data_conv)}
  end
  let create pl : Gtk.image_menu_item obj = Object.make "GtkImageMenuItem" pl
end

module MenuShell = struct
  let cast w : Gtk.menu_shell obj = try_cast w "GtkMenuShell"
  module S = struct
    open GtkSignal
    let activate_current =
      {name="activate_current"; classe=`menushell; marshaller=fun f ->
       marshal1 boolean "GtkMenuShell::activate_current" f}
    let cancel = {name="cancel"; classe=`menushell; marshaller=marshal_unit}
    let cycle_focus =
      {name="cycle_focus"; classe=`menushell; marshaller=fun f ->
       marshal1 GtkEnums.direction_type_conv "GtkMenuShell::cycle_focus" f}
    let deactivate =
      {name="deactivate"; classe=`menushell; marshaller=marshal_unit}
    let move_current =
      {name="move_current"; classe=`menushell; marshaller=fun f ->
       marshal1 GtkEnums.menu_direction_type_conv
         "GtkMenuShell::move_current" f}
    let selection_done =
      {name="selection_done"; classe=`menushell; marshaller=marshal_unit}
  end
  external append : [>`menushell] obj -> [>`widget] obj -> unit
    = "ml_gtk_menu_shell_append"
  external prepend : [>`menushell] obj -> [>`widget] obj -> unit
    = "ml_gtk_menu_shell_prepend"
  external insert : [>`menushell] obj -> [>`widget] obj -> pos:int -> unit
    = "ml_gtk_menu_shell_insert"
  external deactivate : [>`menushell] obj -> unit
    = "ml_gtk_menu_shell_deactivate"
end

module MenuBar = struct
  let cast w : Gtk.menu_bar obj = try_cast w "GtkMenuBar"
  let create pl : Gtk.menu_bar obj = Object.make "GtkMenuBar" pl
end

module Menu = struct
  let cast w : Gtk.menu obj = try_cast w "GtkMenu"
  module P = struct
    let tearoff_title : ([>`menu],_) property =
      {name="tearoff-title"; conv=string}
  end
  module S = struct
    open GtkSignal
    let move_scroll =
      {name="move_scroll"; classe=`menu; marshaller=fun f ->
       marshal1 GtkEnums.scroll_type_conv "GtkMenu::move_scroll" f}
  end
  let create pl : Gtk.menu obj = Object.make "GtkMenu" pl
  external popup :
    [>`menu] obj ->
    [>`menushell] optobj ->
       [>`menuitem] optobj -> button:int -> time:int32 -> unit
    = "ml_gtk_menu_popup"
  external popup_at :
    [>`menu] obj ->
    ?button:int -> ?time:int32 ->
       (x:int -> y:int -> pushed_in:bool -> int * int * bool) -> unit
    = "ml_gtk_menu_popup_at"
  external popdown : [>`menu] obj -> unit = "ml_gtk_menu_popdown"
  external get_active : [>`menu] obj -> widget obj = "ml_gtk_menu_get_active"
  external set_active : [>`menu] obj -> int -> unit
    = "ml_gtk_menu_set_active"
  external set_accel_group : [>`menu] obj -> accel_group -> unit
    = "ml_gtk_menu_set_accel_group"
  external get_accel_group : [>`menu] obj -> accel_group
    = "ml_gtk_menu_get_accel_group"
  external set_accel_path : [>`menu] obj -> string -> unit
    = "ml_gtk_menu_set_accel_path"
  external attach_to_widget : [>`menu] obj -> [>`widget] obj -> unit
    = "ml_gtk_menu_attach_to_widget"
  external get_attach_widget : [>`menu] obj -> widget obj
    = "ml_gtk_menu_get_attach_widget"
  external detach : [>`menu] obj -> unit = "ml_gtk_menu_detach"
end

