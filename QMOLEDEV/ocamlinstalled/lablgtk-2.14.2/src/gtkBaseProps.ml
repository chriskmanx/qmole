open Gobject
open Data
module Object = GtkObject

open Gtk
module Internal = struct
  external allocation_at_pointer : Gpointer.boxed -> rectangle
    = "ml_Val_GtkAllocation"
  let allocation =
    {kind=`POINTER;inj=(fun _ -> failwith "GtkProps.Internal.allocation");
     proj=(function `POINTER(Some p) -> allocation_at_pointer p
          | _ -> failwith "GtkProps.Internal.allocation")}
end

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module Widget = struct
  let cast w : Gtk.widget obj = try_cast w "GtkWidget"
  module P = struct
    let app_paintable : ([>`widget],_) property =
      {name="app-paintable"; conv=boolean}
    let can_default : ([>`widget],_) property =
      {name="can-default"; conv=boolean}
    let can_focus : ([>`widget],_) property =
      {name="can-focus"; conv=boolean}
    let composite_child : ([>`widget],_) property =
      {name="composite-child"; conv=boolean}
    let events : ([>`widget],_) property =
      {name="events"; conv=GdkEnums.event_mask_conv}
    let extension_events : ([>`widget],_) property =
      {name="extension-events"; conv=GdkEnums.extension_mode_conv}
    let has_default : ([>`widget],_) property =
      {name="has-default"; conv=boolean}
    let has_focus : ([>`widget],_) property =
      {name="has-focus"; conv=boolean}
    let has_tooltip : ([>`widget],_) property =
      {name="has-tooltip"; conv=boolean}
    let height_request : ([>`widget],_) property =
      {name="height-request"; conv=int}
    let is_focus : ([>`widget],_) property = {name="is-focus"; conv=boolean}
    let name : ([>`widget],_) property = {name="name"; conv=string}
    let parent : ([>`widget],_) property =
      {name="parent";
       conv=(gobject_option : Gtk.container obj option data_conv)}
    let receives_default : ([>`widget],_) property =
      {name="receives-default"; conv=boolean}
    let sensitive : ([>`widget],_) property =
      {name="sensitive"; conv=boolean}
    let style : ([>`widget],_) property =
      {name="style"; conv=(gobject : Gtk.style data_conv)}
    let tooltip_markup : ([>`widget],_) property =
      {name="tooltip-markup"; conv=string}
    let tooltip_text : ([>`widget],_) property =
      {name="tooltip-text"; conv=string}
    let visible : ([>`widget],_) property = {name="visible"; conv=boolean}
    let width_request : ([>`widget],_) property =
      {name="width-request"; conv=int}
  end
  module S = struct
    open GtkSignal
    let show = {name="show"; classe=`widget; marshaller=marshal_unit}
    let hide = {name="hide"; classe=`widget; marshaller=marshal_unit}
    let map = {name="map"; classe=`widget; marshaller=marshal_unit}
    let unmap = {name="unmap"; classe=`widget; marshaller=marshal_unit}
    let query_tooltip =
      {name="query_tooltip"; classe=`widget; marshaller=fun f ->
       marshal4_ret ~ret:boolean int int boolean
         (gobject : Gtk.tooltip data_conv) "GtkWidget::query_tooltip"
         (fun x1 x2 x3 -> f ~x:x1 ~y:x2 ~kbd:x3)}
    let realize = {name="realize"; classe=`widget; marshaller=marshal_unit}
    let unrealize =
      {name="unrealize"; classe=`widget; marshaller=marshal_unit}
    let state_changed =
      {name="state_changed"; classe=`widget; marshaller=fun f ->
       marshal1 GtkEnums.state_type_conv "GtkWidget::state_changed" f}
    let parent_set =
      {name="parent_set"; classe=`widget; marshaller=fun f ->
       marshal1 (gobject_option : Gtk.widget obj option data_conv)
         "GtkWidget::parent_set" f}
    let size_allocate =
      {name="size_allocate"; classe=`widget; marshaller=fun f ->
       marshal1 Internal.allocation "GtkWidget::size_allocate" f}
    let style_set =
      {name="style_set"; classe=`widget; marshaller=fun f ->
       marshal1 (gobject_option : Gtk.style option data_conv)
         "GtkWidget::style_set" f}
    let selection_get =
      {name="selection_get"; classe=`widget; marshaller=fun f ->
       marshal3 (unsafe_pointer : Gtk.selection_data data_conv) int int32
         "GtkWidget::selection_get" (fun x1 x2 x3 -> f x1 ~info:x2 ~time:x3)}
    let selection_received =
      {name="selection_received"; classe=`widget; marshaller=fun f ->
       marshal2 (unsafe_pointer : Gtk.selection_data data_conv) int32
         "GtkWidget::selection_received" (fun x1 x2 -> f x1 ~time:x2)}
    let drag_begin =
      {name="drag_begin"; classe=`widget; marshaller=fun f ->
       marshal1 (gobject : Gdk.drag_context data_conv)
         "GtkWidget::drag_begin" f}
    let drag_data_delete =
      {name="drag_data_delete"; classe=`widget; marshaller=fun f ->
       marshal1 (gobject : Gdk.drag_context data_conv)
         "GtkWidget::drag_data_delete" f}
    let drag_data_get =
      {name="drag_data_get"; classe=`widget; marshaller=fun f ->
       marshal4 (gobject : Gdk.drag_context data_conv)
         (unsafe_pointer : Gtk.selection_data data_conv) int int32
         "GtkWidget::drag_data_get"
         (fun x1 x2 x3 x4 -> f x1 x2 ~info:x3 ~time:x4)}
    let drag_data_received =
      {name="drag_data_received"; classe=`widget; marshaller=fun f ->
       marshal6 (gobject : Gdk.drag_context data_conv) int int
         (unsafe_pointer : Gtk.selection_data data_conv) int int32
         "GtkWidget::drag_data_received"
         (fun x1 x2 x3 x4 x5 x6 -> f x1 ~x:x2 ~y:x3 x4 ~info:x5 ~time:x6)}
    let drag_drop =
      {name="drag_drop"; classe=`widget; marshaller=fun f ->
       marshal4_ret ~ret:boolean (gobject : Gdk.drag_context data_conv) int
         int int32 "GtkWidget::drag_drop"
         (fun x1 x2 x3 x4 -> f x1 ~x:x2 ~y:x3 ~time:x4)}
    let drag_end =
      {name="drag_end"; classe=`widget; marshaller=fun f ->
       marshal1 (gobject : Gdk.drag_context data_conv)
         "GtkWidget::drag_end" f}
    let drag_leave =
      {name="drag_leave"; classe=`widget; marshaller=fun f ->
       marshal2 (gobject : Gdk.drag_context data_conv) int32
         "GtkWidget::drag_leave" (fun x1 x2 -> f x1 ~time:x2)}
    let drag_motion =
      {name="drag_motion"; classe=`widget; marshaller=fun f ->
       marshal4_ret ~ret:boolean (gobject : Gdk.drag_context data_conv) int
         int int32 "GtkWidget::drag_motion"
         (fun x1 x2 x3 x4 -> f x1 ~x:x2 ~y:x3 ~time:x4)}
    let event =
      {name="event"; classe=`widget; marshaller=fun f ->
       marshal1_ret ~ret:boolean (unsafe_pointer : GdkEvent.any data_conv)
         "GtkWidget::event" f}
    let event_after =
      {name="event_after"; classe=`widget; marshaller=fun f ->
       marshal1 (unsafe_pointer : GdkEvent.any data_conv)
         "GtkWidget::event_after" f}
  end
  external set_double_buffered : [>`widget] obj -> bool -> unit
    = "ml_gtk_widget_set_double_buffered"
  external style_get_property : [>`widget] obj -> string -> g_value
    = "ml_gtk_widget_style_get_property"
end

module Container = struct
  let cast w : Gtk.container obj = try_cast w "GtkContainer"
  module P = struct
    let border_width : ([>`container],_) property =
      {name="border-width"; conv=uint}
    let child : ([>`container],_) property =
      {name="child"; conv=(gobject : Gtk.widget obj data_conv)}
    let resize_mode : ([>`container],_) property =
      {name="resize-mode"; conv=GtkEnums.resize_mode_conv}
  end
  module S = struct
    open GtkSignal
    let add =
      {name="add"; classe=`container; marshaller=fun f ->
       marshal1 (gobject : Gtk.widget obj data_conv) "GtkContainer::add" f}
    let remove =
      {name="remove"; classe=`container; marshaller=fun f ->
       marshal1 (gobject : Gtk.widget obj data_conv) "GtkContainer::remove" f}
    let check_resize =
      {name="check_resize"; classe=`container; marshaller=marshal_unit}
    let set_focus =
      {name="set_focus"; classe=`container; marshaller=fun f ->
       marshal1 (gobject_option : Gtk.widget obj option data_conv)
         "GtkContainer::set_focus" f}
  end
  external check_resize : [>`container] obj -> unit
    = "ml_gtk_container_check_resize"
  external add : [>`container] obj -> [>`widget] obj -> unit
    = "ml_gtk_container_add"
  external remove : [>`container] obj -> [>`widget] obj -> unit
    = "ml_gtk_container_remove"
  external forall : [>`container] obj -> f:(widget obj -> unit) -> unit
    = "ml_gtk_container_forall"
  external foreach : [>`container] obj -> f:(widget obj -> unit) -> unit
    = "ml_gtk_container_foreach"
  external set_focus_child : [>`container] obj -> [>`widget] optobj -> unit
    = "ml_gtk_container_set_focus_child"
  external set_focus_vadjustment :
    [>`container] obj -> [>`adjustment] optobj -> unit
    = "ml_gtk_container_set_focus_vadjustment"
  external set_focus_hadjustment :
    [>`container] obj -> [>`adjustment] optobj -> unit
    = "ml_gtk_container_set_focus_hadjustment"
end

module Bin = struct
  let cast w : Gtk.bin obj = try_cast w "GtkBin"
  external get_child : [>`bin] obj -> widget obj = "ml_gtk_bin_get_child"
end

module Item = struct
  let cast w : Gtk.item obj = try_cast w "GtkItem"
  module S = struct
    open GtkSignal
    let select = {name="select"; classe=`item; marshaller=marshal_unit}
    let deselect = {name="deselect"; classe=`item; marshaller=marshal_unit}
    let toggle = {name="toggle"; classe=`item; marshaller=marshal_unit}
  end
  external select : [>`item] obj -> unit = "ml_gtk_item_select"
  external deselect : [>`item] obj -> unit = "ml_gtk_item_deselect"
  external toggle : [>`item] obj -> unit = "ml_gtk_item_toggle"
end

module Adjustment = struct
  let cast w : Gtk.adjustment obj = try_cast w "GtkAdjustment"
  module S = struct
    open GtkSignal
    let changed =
      {name="changed"; classe=`adjustment; marshaller=marshal_unit}
    let value_changed =
      {name="value_changed"; classe=`adjustment; marshaller=marshal_unit}
  end
  let create pl : Gtk.adjustment obj = Object.make "GtkAdjustment" pl
end

module Window = struct
  let cast w : Gtk.window obj = try_cast w "GtkWindow"
  module P = struct
    let title : ([>`window],_) property = {name="title"; conv=string}
    let accept_focus : ([>`window],_) property =
      {name="accept-focus"; conv=boolean}
    let allow_grow : ([>`window],_) property =
      {name="allow-grow"; conv=boolean}
    let allow_shrink : ([>`window],_) property =
      {name="allow-shrink"; conv=boolean}
    let decorated : ([>`window],_) property =
      {name="decorated"; conv=boolean}
    let default_height : ([>`window],_) property =
      {name="default-height"; conv=int}
    let default_width : ([>`window],_) property =
      {name="default-width"; conv=int}
    let deletable : ([>`window],_) property =
      {name="deletable"; conv=boolean}
    let destroy_with_parent : ([>`window],_) property =
      {name="destroy-with-parent"; conv=boolean}
    let focus_on_map : ([>`window],_) property =
      {name="focus-on-map"; conv=boolean}
    let gravity : ([>`window],_) property =
      {name="gravity"; conv=GdkEnums.gravity_conv}
    let has_toplevel_focus : ([>`window],_) property =
      {name="has-toplevel-focus"; conv=boolean}
    let icon : ([>`window],_) property =
      {name="icon";
       conv=(gobject_option : GdkPixbuf.pixbuf option data_conv)}
    let icon_name : ([>`window],_) property = {name="icon-name"; conv=string}
    let is_active : ([>`window],_) property =
      {name="is-active"; conv=boolean}
    let modal : ([>`window],_) property = {name="modal"; conv=boolean}
    let window_position : ([>`window],_) property =
      {name="window-position"; conv=GtkEnums.window_position_conv}
    let opacity : ([>`window],_) property = {name="opacity"; conv=double}
    let resizable : ([>`window],_) property =
      {name="resizable"; conv=boolean}
    let role : ([>`window],_) property = {name="role"; conv=string}
    let screen : ([>`window],_) property =
      {name="screen"; conv=(gobject : Gdk.screen data_conv)}
    let skip_pager_hint : ([>`window],_) property =
      {name="skip-pager-hint"; conv=boolean}
    let skip_taskbar_hint : ([>`window],_) property =
      {name="skip-taskbar-hint"; conv=boolean}
    let startup_id : ([>`window],_) property =
      {name="startup-id"; conv=string}
    let transient_for : ([>`window],_) property =
      {name="transient-for";
       conv=(gobject_option : Gtk.window obj option data_conv)}
    let kind : ([>`window],_) property =
      {name="type"; conv=GtkEnums.window_type_conv}
    let type_hint : ([>`window],_) property =
      {name="type-hint"; conv=GdkEnums.window_type_hint_conv}
    let urgency_hint : ([>`window],_) property =
      {name="urgency-hint"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let activate_default =
      {name="activate_default"; classe=`window; marshaller=marshal_unit}
    let activate_focus =
      {name="activate_focus"; classe=`window; marshaller=marshal_unit}
    let frame_event =
      {name="frame_event"; classe=`window; marshaller=fun f ->
       marshal1 (unsafe_pointer : GdkEvent.any data_conv)
         "GtkWindow::frame_event" f}
    let keys_changed =
      {name="keys_changed"; classe=`window; marshaller=marshal_unit}
    let move_focus =
      {name="move_focus"; classe=`window; marshaller=fun f ->
       marshal1 GtkEnums.direction_type_conv "GtkWindow::move_focus" f}
    let set_focus =
      {name="set_focus"; classe=`window; marshaller=fun f ->
       marshal1 (gobject_option : Gtk.widget obj option data_conv)
         "GtkWindow::set_focus" f}
  end
  let create ?kind pl : Gtk.window obj =
    let pl = (may_cons P.kind kind pl) in
    Object.make "GtkWindow" pl
  external present : [>`window] obj -> unit = "ml_gtk_window_present"
  external iconify : [>`window] obj -> unit = "ml_gtk_window_iconify"
  external deiconify : [>`window] obj -> unit = "ml_gtk_window_deiconify"
  external stick : [>`window] obj -> unit = "ml_gtk_window_stick"
  external unstick : [>`window] obj -> unit = "ml_gtk_window_unstick"
  external maximize : [>`window] obj -> unit = "ml_gtk_window_maximize"
  external unmaximize : [>`window] obj -> unit = "ml_gtk_window_unmaximize"
  external fullscreen : [>`window] obj -> unit = "ml_gtk_window_fullscreen"
  external unfullscreen : [>`window] obj -> unit
    = "ml_gtk_window_unfullscreen"
  external set_decorated : [>`window] obj -> bool -> unit
    = "ml_gtk_window_set_decorated"
  external set_mnemonic_modifier :
    [>`window] obj -> Gdk.Tags.modifier list -> unit
    = "ml_gtk_window_set_mnemonic_modifier"
  external move : [>`window] obj -> x:int -> y:int -> unit
    = "ml_gtk_window_move"
  external parse_geometry : [>`window] obj -> string -> bool
    = "ml_gtk_window_parse_geometry"
  external reshow_with_initial_size : [>`window] obj -> unit
    = "ml_gtk_window_reshow_with_initial_size"
  external resize : [>`window] obj -> width:int -> height:int -> unit
    = "ml_gtk_window_resize"
  external set_role : [>`window] obj -> string -> unit
    = "ml_gtk_window_set_role"
  external get_role : [>`window] obj -> string = "ml_gtk_window_get_role"
  let make_params ~cont pl ?title ?allow_grow ?allow_shrink ?decorated
      ?deletable ?focus_on_map ?icon ?icon_name ?modal ?position ?resizable
      ?screen ?type_hint ?urgency_hint =
    let pl = (
      may_cons P.title title (
      may_cons P.allow_grow allow_grow (
      may_cons P.allow_shrink allow_shrink (
      may_cons P.decorated decorated (
      may_cons P.deletable deletable (
      may_cons P.focus_on_map focus_on_map (
      may_cons_opt P.icon icon (
      may_cons P.icon_name icon_name (
      may_cons P.modal modal (
      may_cons P.window_position position (
      may_cons P.resizable resizable (
      may_cons P.screen screen (
      may_cons P.type_hint type_hint (
      may_cons P.urgency_hint urgency_hint pl)))))))))))))) in
    cont pl
end

module Dialog = struct
  let cast w : Gtk.dialog obj = try_cast w "GtkDialog"
  module P = struct
    let has_separator : ([>`dialog],_) property =
      {name="has-separator"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let close = {name="close"; classe=`dialog; marshaller=marshal_unit}
    let response =
      {name="response"; classe=`dialog; marshaller=fun f ->
       marshal1 int "GtkDialog::response" f}
  end
  let create pl : Gtk.dialog obj = Object.make "GtkDialog" pl
end

module FileSelection = struct
  let cast w : Gtk.file_selection obj = try_cast w "GtkFileSelection"
  module P = struct
    let filename : ([>`fileselection],_) property =
      {name="filename"; conv=string}
    let select_multiple : ([>`fileselection],_) property =
      {name="select-multiple"; conv=boolean}
    let show_fileops : ([>`fileselection],_) property =
      {name="show-fileops"; conv=boolean}
  end
  let make_params ~cont pl ?filename ?select_multiple ?show_fileops =
    let pl = (
      may_cons P.filename filename (
      may_cons P.select_multiple select_multiple (
      may_cons P.show_fileops show_fileops pl))) in
    cont pl
end

module ColorSelectionDialog = struct
  let cast w : Gtk.color_selection_dialog obj =
    try_cast w "GtkColorSelectionDialog"
  let create pl : Gtk.color_selection_dialog obj =
    Object.make "GtkColorSelectionDialog" pl
end

module FontSelectionDialog = struct
  let cast w : Gtk.font_selection_dialog obj =
    try_cast w "GtkFontSelectionDialog"
  let create pl : Gtk.font_selection_dialog obj =
    Object.make "GtkFontSelectionDialog" pl
end

module MessageDialog = struct
  let cast w : Gtk.message_dialog obj = try_cast w "GtkMessageDialog"
  module P = struct
    let buttons : ([>`messagedialog],_) property =
      {name="buttons"; conv=GtkEnums.buttons_type_conv}
    let message_type : ([>`messagedialog],_) property =
      {name="message-type"; conv=GtkEnums.message_type_conv}
  end
end

module AboutDialog = struct
  let cast w : Gtk.about_dialog obj = try_cast w "GtkAboutDialog"
  module P = struct
    let program_name : ([>`aboutdialog],_) property =
      {name="program-name"; conv=string}
    let comments : ([>`aboutdialog],_) property =
      {name="comments"; conv=string}
    let copyright : ([>`aboutdialog],_) property =
      {name="copyright"; conv=string}
    let license : ([>`aboutdialog],_) property =
      {name="license"; conv=string}
    let logo : ([>`aboutdialog],_) property =
      {name="logo"; conv=(gobject : GdkPixbuf.pixbuf data_conv)}
    let logo_icon_name : ([>`aboutdialog],_) property =
      {name="logo-icon-name"; conv=string}
    let translator_credits : ([>`aboutdialog],_) property =
      {name="translator-credits"; conv=string}
    let version : ([>`aboutdialog],_) property =
      {name="version"; conv=string}
    let website : ([>`aboutdialog],_) property =
      {name="website"; conv=string}
    let website_label : ([>`aboutdialog],_) property =
      {name="website-label"; conv=string}
    let wrap_license : ([>`aboutdialog],_) property =
      {name="wrap-license"; conv=boolean}
  end
  external set_artists : [>`aboutdialog] obj -> string list -> unit
    = "ml_gtk_about_dialog_set_artists"
  external get_artists : [>`aboutdialog] obj -> string list
    = "ml_gtk_about_dialog_get_artists"
  external set_authors : [>`aboutdialog] obj -> string list -> unit
    = "ml_gtk_about_dialog_set_authors"
  external get_authors : [>`aboutdialog] obj -> string list
    = "ml_gtk_about_dialog_get_authors"
  external set_documenters : [>`aboutdialog] obj -> string list -> unit
    = "ml_gtk_about_dialog_set_documenters"
  external get_documenters : [>`aboutdialog] obj -> string list
    = "ml_gtk_about_dialog_get_documenters"
  let make_params ~cont pl ?comments ?copyright ?license ?logo
      ?logo_icon_name ?translator_credits ?version ?website ?website_label
      ?wrap_license =
    let pl = (
      may_cons P.comments comments (
      may_cons P.copyright copyright (
      may_cons P.license license (
      may_cons P.logo logo (
      may_cons P.logo_icon_name logo_icon_name (
      may_cons P.translator_credits translator_credits (
      may_cons P.version version (
      may_cons P.website website (
      may_cons P.website_label website_label (
      may_cons P.wrap_license wrap_license pl)))))))))) in
    cont pl
end

module Plug = struct
  let cast w : Gtk.plug obj = try_cast w "GtkPlug"
  module S = struct
    open GtkSignal
    let embedded = {name="embedded"; classe=`plug; marshaller=marshal_unit}
  end
end

module Socket = struct
  let cast w : Gtk.socket obj = try_cast w "GtkSocket"
  module S = struct
    open GtkSignal
    let plug_added =
      {name="plug_added"; classe=`socket; marshaller=marshal_unit}
    let plug_removed =
      {name="plug_removed"; classe=`socket; marshaller=marshal_unit}
  end
  let create pl : Gtk.socket obj = Object.make "GtkSocket" pl
  external steal : [>`socket] obj -> Gdk.xid -> unit = "ml_gtk_socket_steal"
end

