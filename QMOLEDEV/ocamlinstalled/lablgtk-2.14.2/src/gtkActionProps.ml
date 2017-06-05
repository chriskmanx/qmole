open Gobject
open Data
module Object = GtkObject
module PrivateProps = struct
  let name = {name="name"; conv=string}
  let sensitive = {name="sensitive"; conv=boolean}
  let visible = {name="visible"; conv=boolean}
end

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module Action = struct
  let cast w : Gtk.action obj = try_cast w "GtkAction"
  module P = struct
    let hide_if_empty : ([>`action],_) property =
      {name="hide-if-empty"; conv=boolean}
    let is_important : ([>`action],_) property =
      {name="is-important"; conv=boolean}
    let label : ([>`action],_) property = {name="label"; conv=string}
    let icon_name : ([>`action],_) property = {name="icon-name"; conv=string}
    let name : ([>`action],_) property = PrivateProps.name
    let sensitive : ([>`action],_) property = PrivateProps.sensitive
    let short_label : ([>`action],_) property =
      {name="short-label"; conv=string}
    let stock_id : ([>`action],_) property =
      {name="stock-id"; conv=GtkStock.conv}
    let tooltip : ([>`action],_) property = {name="tooltip"; conv=string}
    let visible : ([>`action],_) property = PrivateProps.visible
    let visible_horizontal : ([>`action],_) property =
      {name="visible-horizontal"; conv=boolean}
    let visible_vertical : ([>`action],_) property =
      {name="visible-vertical"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let activate = {name="activate"; classe=`action; marshaller=marshal_unit}
  end
  let create ?name pl : Gtk.action obj =
    let pl = (may_cons P.name name pl) in
    Gobject.unsafe_create "GtkAction" pl
  external is_sensitive : [>`action] obj -> bool
    = "ml_gtk_action_is_sensitive"
  external is_visible : [>`action] obj -> bool = "ml_gtk_action_is_visible"
  external activate : [>`action] obj -> unit = "ml_gtk_action_activate"
  external connect_proxy : [>`action] obj -> Gtk.widget Gtk.obj -> unit
    = "ml_gtk_action_connect_proxy"
  external disconnect_proxy : [>`action] obj -> Gtk.widget Gtk.obj -> unit
    = "ml_gtk_action_disconnect_proxy"
  external get_proxies : [>`action] obj -> Gtk.widget Gtk.obj list
    = "ml_gtk_action_get_proxies"
  external connect_accelerator : [>`action] obj -> unit
    = "ml_gtk_action_connect_accelerator"
  external disconnect_accelerator : [>`action] obj -> unit
    = "ml_gtk_action_disconnect_accelerator"
  external set_accel_path : [>`action] obj -> string -> unit
    = "ml_gtk_action_set_accel_path"
  external set_accel_group : [>`action] obj -> Gtk.accel_group -> unit
    = "ml_gtk_action_set_accel_group"
  external block_activate_from : [>`action] obj -> Gtk.widget Gtk.obj -> unit
    = "ml_gtk_action_block_activate_from"
  external unblock_activate_from :
    [>`action] obj -> Gtk.widget Gtk.obj -> unit
    = "ml_gtk_action_unblock_activate_from"
end

module ToggleAction = struct
  let cast w : Gtk.toggle_action obj = try_cast w "GtkToggleAction"
  module P = struct
    let draw_as_radio : ([>`toggleaction],_) property =
      {name="draw-as-radio"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let toggled =
      {name="toggled"; classe=`toggleaction; marshaller=marshal_unit}
  end
  let create pl : Gtk.toggle_action obj =
    Gobject.unsafe_create "GtkToggleAction" pl
  external toggled : [>`toggleaction] obj -> unit
    = "ml_gtk_toggle_action_toggled"
  external set_active : [>`toggleaction] obj -> bool -> unit
    = "ml_gtk_toggle_action_set_active"
  external get_active : [>`toggleaction] obj -> bool
    = "ml_gtk_toggle_action_get_active"
end

module RadioAction = struct
  let cast w : Gtk.radio_action obj = try_cast w "GtkRadioAction"
  module P = struct
    let group : ([>`radioaction],_) property =
      {name="group";
       conv=(gobject_option : Gtk.radio_action obj option data_conv)}
    let value : ([>`radioaction],_) property = {name="value"; conv=int}
  end
  module S = struct
    open GtkSignal
    let changed =
      {name="changed"; classe=`radioaction; marshaller=fun f ->
       marshal1 (gobject : Gtk.radio_action obj data_conv)
         "GtkRadioAction::changed" f}
  end
  let create pl : Gtk.radio_action obj =
    Gobject.unsafe_create "GtkRadioAction" pl
  external get_current_value : [>`radioaction] obj -> int
    = "ml_gtk_radio_action_get_current_value"
  external set_group :
    [>`radioaction] obj -> Gtk.radio_action Gtk.group -> unit
    = "ml_gtk_radio_action_set_group"
end

module UIManager = struct
  let cast w : Gtk.ui_manager obj = try_cast w "GtkUIManager"
  module P = struct
    let add_tearoffs : ([>`uimanager],_) property =
      {name="add-tearoffs"; conv=boolean}
    let ui : ([>`uimanager],_) property = {name="ui"; conv=string}
  end
  module S = struct
    open GtkSignal
    let actions_changed =
      {name="actions_changed"; classe=`uimanager; marshaller=marshal_unit}
    let add_widget =
      {name="add_widget"; classe=`uimanager; marshaller=fun f ->
       marshal1 (gobject : Gtk.widget obj data_conv)
         "GtkUIManager::add_widget" f}
    let connect_proxy =
      {name="connect_proxy"; classe=`uimanager; marshaller=fun f ->
       marshal2 (gobject : Gtk.action obj data_conv)
         (gobject : Gtk.widget obj data_conv) "GtkUIManager::connect_proxy" f}
    let disconnect_proxy =
      {name="disconnect_proxy"; classe=`uimanager; marshaller=fun f ->
       marshal2 (gobject : Gtk.action obj data_conv)
         (gobject : Gtk.widget obj data_conv)
         "GtkUIManager::disconnect_proxy" f}
    let post_activate =
      {name="post_activate"; classe=`uimanager; marshaller=fun f ->
       marshal1 (gobject : Gtk.action obj data_conv)
         "GtkUIManager::post_activate" f}
    let pre_activate =
      {name="pre_activate"; classe=`uimanager; marshaller=fun f ->
       marshal1 (gobject : Gtk.action obj data_conv)
         "GtkUIManager::pre_activate" f}
  end
  let create pl : Gtk.ui_manager obj =
    Gobject.unsafe_create "GtkUIManager" pl
  external insert_action_group :
    [>`uimanager] obj -> Gtk.action_group obj -> int -> unit
    = "ml_gtk_ui_manager_insert_action_group"
  external remove_action_group :
    [>`uimanager] obj -> Gtk.action_group obj -> unit
    = "ml_gtk_ui_manager_remove_action_group"
  external get_action_groups : [>`uimanager] obj -> Gtk.action_group obj list
    = "ml_gtk_ui_manager_get_action_groups"
  external get_accel_group : [>`uimanager] obj -> Gtk.accel_group
    = "ml_gtk_ui_manager_get_accel_group"
  external get_widget : [>`uimanager] obj -> string -> Gtk.widget Gtk.obj
    = "ml_gtk_ui_manager_get_widget"
  external get_toplevels :
    [>`uimanager] obj ->
    GtkEnums.ui_manager_item_type list -> Gtk.widget Gtk.obj list
    = "ml_gtk_ui_manager_get_toplevels"
  external get_action : [>`uimanager] obj -> string -> Gtk.action obj
    = "ml_gtk_ui_manager_get_action"
  external add_ui_from_string : [>`uimanager] obj -> string -> int
    = "ml_gtk_ui_manager_add_ui_from_string"
  external add_ui_from_file : [>`uimanager] obj -> string -> int
    = "ml_gtk_ui_manager_add_ui_from_file"
  external new_merge_id : [>`uimanager] obj -> int
    = "ml_gtk_ui_manager_new_merge_id"
  external add_ui :
    [>`uimanager] obj ->
    int -> path:string -> name:string -> action:string option -> GtkEnums.ui_manager_item_type -> top:bool -> unit
    = "ml_gtk_ui_manager_add_ui_bc" "ml_gtk_ui_manager_add_ui"
  external remove_ui : [>`uimanager] obj -> int -> unit
    = "ml_gtk_ui_manager_remove_ui"
  external ensure_update : [>`uimanager] obj -> unit
    = "ml_gtk_ui_manager_ensure_update"
end

module ActionGroup = struct
  let cast w : Gtk.action_group obj = try_cast w "GtkActionGroup"
  module P = struct
    let name : ([>`actiongroup],_) property = PrivateProps.name
    let sensitive : ([>`actiongroup],_) property = PrivateProps.sensitive
    let visible : ([>`actiongroup],_) property = PrivateProps.visible
  end
  module S = struct
    open GtkSignal
    let connect_proxy =
      {name="connect_proxy"; classe=`actiongroup; marshaller=fun f ->
       marshal2 (gobject : Gtk.action obj data_conv)
         (gobject : Gtk.widget obj data_conv)
         "GtkActionGroup::connect_proxy" f}
    let disconnect_proxy =
      {name="disconnect_proxy"; classe=`actiongroup; marshaller=fun f ->
       marshal2 (gobject : Gtk.action obj data_conv)
         (gobject : Gtk.widget obj data_conv)
         "GtkActionGroup::disconnect_proxy" f}
    let post_activate =
      {name="post_activate"; classe=`actiongroup; marshaller=fun f ->
       marshal1 (gobject : Gtk.action obj data_conv)
         "GtkActionGroup::post_activate" f}
    let pre_activate =
      {name="pre_activate"; classe=`actiongroup; marshaller=fun f ->
       marshal1 (gobject : Gtk.action obj data_conv)
         "GtkActionGroup::pre_activate" f}
  end
  let create ?name pl : Gtk.action_group obj =
    let pl = (may_cons P.name name pl) in
    Gobject.unsafe_create "GtkActionGroup" pl
  external get_action : [>`actiongroup] obj -> string -> Gtk.action obj
    = "ml_gtk_action_group_get_action"
  external list_actions : [>`actiongroup] obj -> Gtk.action obj list
    = "ml_gtk_action_group_list_actions"
  external add_action : [>`actiongroup] obj -> Gtk.action obj -> unit
    = "ml_gtk_action_group_add_action"
  external add_action_with_accel :
    [>`actiongroup] obj -> Gtk.action obj -> string option -> unit
    = "ml_gtk_action_group_add_action_with_accel"
  external remove_action : [>`actiongroup] obj -> Gtk.action obj -> unit
    = "ml_gtk_action_group_remove_action"
end

