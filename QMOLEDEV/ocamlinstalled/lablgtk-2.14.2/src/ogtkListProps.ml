open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param
open GtkListProps

class virtual liste_props = object
  val virtual obj : _ obj
  method set_selection_mode = set Liste.P.selection_mode obj
  method selection_mode = get Liste.P.selection_mode obj
end

class virtual clist_props = object
  val virtual obj : _ obj
  method set_sort_type = set Clist.P.sort_type obj
  method set_reorderable = set Clist.P.reorderable obj
  method set_row_height = set Clist.P.row_height obj
  method set_selection_mode = set Clist.P.selection_mode obj
  method set_shadow_type = set Clist.P.shadow_type obj
  method set_titles_active = set Clist.P.titles_active obj
  method set_use_drag_icons = set Clist.P.use_drag_icons obj
  method n_columns = get Clist.P.n_columns obj
  method sort_type = get Clist.P.sort_type obj
  method reorderable = get Clist.P.reorderable obj
  method row_height = get Clist.P.row_height obj
  method selection_mode = get Clist.P.selection_mode obj
  method shadow_type = get Clist.P.shadow_type obj
  method titles_active = get Clist.P.titles_active obj
  method use_drag_icons = get Clist.P.use_drag_icons obj
end

class virtual clist_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method click_column = self#connect Clist.S.click_column
  method resize_column = self#connect Clist.S.resize_column
  method scroll_horizontal = self#connect Clist.S.scroll_horizontal
  method scroll_vertical = self#connect Clist.S.scroll_vertical
  method select_all = self#connect Clist.S.select_all
  method select_row = self#connect Clist.S.select_row
  method unselect_all = self#connect Clist.S.unselect_all
  method unselect_row = self#connect Clist.S.unselect_row
end

