open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param

open GtkBaseProps

open GtkBaseProps

class virtual container_props = object
  val virtual obj : _ obj
  method set_border_width = set Container.P.border_width obj
  method set_resize_mode = set Container.P.resize_mode obj
  method border_width = get Container.P.border_width obj
  method resize_mode = get Container.P.resize_mode obj
end

class virtual container_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method add = self#connect
    {Container.S.add with marshaller = fun f ->
     marshal1 GObj.conv_widget "GtkContainer::add" f}
  method remove = self#connect
    {Container.S.remove with marshaller = fun f ->
     marshal1 GObj.conv_widget "GtkContainer::remove" f}
end

class virtual item_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method select = self#connect Item.S.select
  method deselect = self#connect Item.S.deselect
  method toggle = self#connect Item.S.toggle
end

class virtual window_props = object
  val virtual obj : _ obj
  method set_title = set Window.P.title obj
  method set_accept_focus = set Window.P.accept_focus obj
  method set_allow_grow = set Window.P.allow_grow obj
  method set_allow_shrink = set Window.P.allow_shrink obj
  method set_decorated = set Window.P.decorated obj
  method set_default_height = set Window.P.default_height obj
  method set_default_width = set Window.P.default_width obj
  method set_deletable = set Window.P.deletable obj
  method set_destroy_with_parent = set Window.P.destroy_with_parent obj
  method set_focus_on_map = set Window.P.focus_on_map obj
  method set_gravity = set Window.P.gravity obj
  method set_icon = set Window.P.icon obj
  method set_icon_name = set Window.P.icon_name obj
  method set_modal = set Window.P.modal obj
  method set_position = set Window.P.window_position obj
  method set_opacity = set Window.P.opacity obj
  method set_resizable = set Window.P.resizable obj
  method set_role = set Window.P.role obj
  method set_screen = set Window.P.screen obj
  method set_skip_pager_hint = set Window.P.skip_pager_hint obj
  method set_skip_taskbar_hint = set Window.P.skip_taskbar_hint obj
  method set_type_hint = set Window.P.type_hint obj
  method set_urgency_hint = set Window.P.urgency_hint obj
  method title = get Window.P.title obj
  method accept_focus = get Window.P.accept_focus obj
  method allow_grow = get Window.P.allow_grow obj
  method allow_shrink = get Window.P.allow_shrink obj
  method decorated = get Window.P.decorated obj
  method default_height = get Window.P.default_height obj
  method default_width = get Window.P.default_width obj
  method deletable = get Window.P.deletable obj
  method destroy_with_parent = get Window.P.destroy_with_parent obj
  method focus_on_map = get Window.P.focus_on_map obj
  method gravity = get Window.P.gravity obj
  method has_toplevel_focus = get Window.P.has_toplevel_focus obj
  method icon = get Window.P.icon obj
  method icon_name = get Window.P.icon_name obj
  method is_active = get Window.P.is_active obj
  method modal = get Window.P.modal obj
  method position = get Window.P.window_position obj
  method opacity = get Window.P.opacity obj
  method resizable = get Window.P.resizable obj
  method role = get Window.P.role obj
  method screen = get Window.P.screen obj
  method skip_pager_hint = get Window.P.skip_pager_hint obj
  method skip_taskbar_hint = get Window.P.skip_taskbar_hint obj
  method kind = get Window.P.kind obj
  method type_hint = get Window.P.type_hint obj
  method urgency_hint = get Window.P.urgency_hint obj
end

class virtual dialog_props = object
  val virtual obj : _ obj
  method set_has_separator = set Dialog.P.has_separator obj
  method has_separator = get Dialog.P.has_separator obj
end

class virtual file_selection_props = object
  val virtual obj : _ obj
  method set_filename = set FileSelection.P.filename obj
  method set_select_multiple = set FileSelection.P.select_multiple obj
  method set_show_fileops = set FileSelection.P.show_fileops obj
  method filename = get FileSelection.P.filename obj
  method select_multiple = get FileSelection.P.select_multiple obj
  method show_fileops = get FileSelection.P.show_fileops obj
end

class virtual message_dialog_props = object
  val virtual obj : _ obj
  method set_message_type = set MessageDialog.P.message_type obj
  method message_type = get MessageDialog.P.message_type obj
end

class virtual about_dialog_props = object
  val virtual obj : _ obj
  method set_comments = set AboutDialog.P.comments obj
  method set_copyright = set AboutDialog.P.copyright obj
  method set_license = set AboutDialog.P.license obj
  method set_logo = set AboutDialog.P.logo obj
  method set_logo_icon_name = set AboutDialog.P.logo_icon_name obj
  method set_translator_credits = set AboutDialog.P.translator_credits obj
  method set_version = set AboutDialog.P.version obj
  method set_website = set AboutDialog.P.website obj
  method set_website_label = set AboutDialog.P.website_label obj
  method set_wrap_license = set AboutDialog.P.wrap_license obj
  method comments = get AboutDialog.P.comments obj
  method copyright = get AboutDialog.P.copyright obj
  method license = get AboutDialog.P.license obj
  method logo = get AboutDialog.P.logo obj
  method logo_icon_name = get AboutDialog.P.logo_icon_name obj
  method translator_credits = get AboutDialog.P.translator_credits obj
  method version = get AboutDialog.P.version obj
  method website = get AboutDialog.P.website obj
  method website_label = get AboutDialog.P.website_label obj
  method wrap_license = get AboutDialog.P.wrap_license obj
end

class virtual plug_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method embedded = self#connect Plug.S.embedded
end

class virtual socket_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method plug_added = self#connect Socket.S.plug_added
  method plug_removed = self#connect Socket.S.plug_removed
end

