open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param

open GtkButtonProps

open GtkButtonProps

class virtual button_props = object
  val virtual obj : _ obj
  method set_focus_on_click = set Button.P.focus_on_click obj
  method set_image = set {Button.P.image with conv=GObj.conv_widget} obj
  method set_image_position = set Button.P.image_position obj
  method set_label = set Button.P.label obj
  method set_use_stock = set Button.P.use_stock obj
  method set_use_underline = set Button.P.use_underline obj
  method set_relief = set Button.P.relief obj
  method set_xalign = set Button.P.xalign obj
  method set_yalign = set Button.P.yalign obj
  method focus_on_click = get Button.P.focus_on_click obj
  method image = get {Button.P.image with conv=GObj.conv_widget} obj
  method image_position = get Button.P.image_position obj
  method label = get Button.P.label obj
  method use_stock = get Button.P.use_stock obj
  method use_underline = get Button.P.use_underline obj
  method relief = get Button.P.relief obj
  method xalign = get Button.P.xalign obj
  method yalign = get Button.P.yalign obj
end

class virtual button_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method clicked = self#connect Button.S.clicked
  method enter = self#connect Button.S.enter
  method leave = self#connect Button.S.leave
  method pressed = self#connect Button.S.pressed
  method released = self#connect Button.S.released
end

class virtual color_button_props = object
  val virtual obj : _ obj
  method set_alpha = set ColorButton.P.alpha obj
  method set_color = set ColorButton.P.color obj
  method set_title = set ColorButton.P.title obj
  method set_use_alpha = set ColorButton.P.use_alpha obj
  method alpha = get ColorButton.P.alpha obj
  method color = get ColorButton.P.color obj
  method title = get ColorButton.P.title obj
  method use_alpha = get ColorButton.P.use_alpha obj
end

class virtual font_button_props = object
  val virtual obj : _ obj
  method set_font_name = set FontButton.P.font_name obj
  method set_show_size = set FontButton.P.show_size obj
  method set_show_style = set FontButton.P.show_style obj
  method set_title = set FontButton.P.title obj
  method set_use_font = set FontButton.P.use_font obj
  method set_use_size = set FontButton.P.use_size obj
  method font_name = get FontButton.P.font_name obj
  method show_size = get FontButton.P.show_size obj
  method show_style = get FontButton.P.show_style obj
  method title = get FontButton.P.title obj
  method use_font = get FontButton.P.use_font obj
  method use_size = get FontButton.P.use_size obj
end

class virtual tool_item_props = object
  val virtual obj : _ obj
  method set_is_important = set ToolItem.P.is_important obj
  method set_visible_horizontal = set ToolItem.P.visible_horizontal obj
  method set_visible_vertical = set ToolItem.P.visible_vertical obj
  method is_important = get ToolItem.P.is_important obj
  method visible_horizontal = get ToolItem.P.visible_horizontal obj
  method visible_vertical = get ToolItem.P.visible_vertical obj
end

class virtual tool_button_props = object
  val virtual obj : _ obj
  method set_icon_widget =
    set {ToolButton.P.icon_widget with conv=GObj.conv_widget} obj
  method set_label = set ToolButton.P.label obj
  method set_label_widget =
    set {ToolButton.P.label_widget with conv=GObj.conv_widget} obj
  method set_stock_id = set ToolButton.P.stock_id obj
  method set_use_underline = set ToolButton.P.use_underline obj
  method icon_widget =
    get {ToolButton.P.icon_widget with conv=GObj.conv_widget} obj
  method label = get ToolButton.P.label obj
  method label_widget =
    get {ToolButton.P.label_widget with conv=GObj.conv_widget} obj
  method stock_id = get ToolButton.P.stock_id obj
  method use_underline = get ToolButton.P.use_underline obj
end

class virtual toolbar_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method orientation_changed = self#connect Toolbar.S.orientation_changed
  method style_changed = self#connect Toolbar.S.style_changed
  method focus_home_or_end = self#connect Toolbar.S.focus_home_or_end
  method move_focus = self#connect Toolbar.S.move_focus
  method popup_context_menu = self#connect Toolbar.S.popup_context_menu
end

class virtual link_button_props = object
  val virtual obj : _ obj
  method set_uri = set LinkButton.P.uri obj
  method uri = get LinkButton.P.uri obj
end

