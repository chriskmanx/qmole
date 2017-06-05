open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param
open GtkActionProps

class virtual action_props = object
  val virtual obj : _ obj
  method set_hide_if_empty = set Action.P.hide_if_empty obj
  method set_is_important = set Action.P.is_important obj
  method set_label = set Action.P.label obj
  method set_icon_name = set Action.P.icon_name obj
  method set_sensitive = set Action.P.sensitive obj
  method set_short_label = set Action.P.short_label obj
  method set_stock_id = set Action.P.stock_id obj
  method set_tooltip = set Action.P.tooltip obj
  method set_visible = set Action.P.visible obj
  method set_visible_horizontal = set Action.P.visible_horizontal obj
  method set_visible_vertical = set Action.P.visible_vertical obj
  method hide_if_empty = get Action.P.hide_if_empty obj
  method is_important = get Action.P.is_important obj
  method label = get Action.P.label obj
  method icon_name = get Action.P.icon_name obj
  method name = get Action.P.name obj
  method sensitive = get Action.P.sensitive obj
  method short_label = get Action.P.short_label obj
  method stock_id = get Action.P.stock_id obj
  method tooltip = get Action.P.tooltip obj
  method visible = get Action.P.visible obj
  method visible_horizontal = get Action.P.visible_horizontal obj
  method visible_vertical = get Action.P.visible_vertical obj
end

class virtual action_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method activate = self#connect Action.S.activate
end

class virtual toggle_action_props = object
  val virtual obj : _ obj
  method set_draw_as_radio = set ToggleAction.P.draw_as_radio obj
  method draw_as_radio = get ToggleAction.P.draw_as_radio obj
end

class virtual toggle_action_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method toggled = self#connect ToggleAction.S.toggled
end

class virtual radio_action_props = object
  val virtual obj : _ obj
  method set_group = set RadioAction.P.group obj
  method set_value = set RadioAction.P.value obj
  method value = get RadioAction.P.value obj
end

class virtual ui_manager_props = object
  val virtual obj : _ obj
  method set_add_tearoffs = set UIManager.P.add_tearoffs obj
  method add_tearoffs = get UIManager.P.add_tearoffs obj
  method ui = get UIManager.P.ui obj
end

class virtual ui_manager_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method actions_changed = self#connect UIManager.S.actions_changed
  method add_widget = self#connect
    {UIManager.S.add_widget with marshaller = fun f ->
     marshal1 GObj.conv_widget "GtkUIManager::add_widget" f}
end

class virtual action_group_props = object
  val virtual obj : _ obj
  method set_sensitive = set ActionGroup.P.sensitive obj
  method set_visible = set ActionGroup.P.visible obj
  method name = get ActionGroup.P.name obj
  method sensitive = get ActionGroup.P.sensitive obj
  method visible = get ActionGroup.P.visible obj
end

