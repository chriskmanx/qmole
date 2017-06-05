open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param

open GtkPackProps

open GtkPackProps

class virtual paned_props = object
  val virtual obj : _ obj
  method set_position = set Paned.P.position obj
  method position = get Paned.P.position obj
  method max_position = get Paned.P.max_position obj
  method min_position = get Paned.P.min_position obj
end

class virtual notebook_props = object
  val virtual obj : _ obj
  method set_enable_popup = set Notebook.P.enable_popup obj
  method set_homogeneous_tabs = set Notebook.P.homogeneous obj
  method set_scrollable = set Notebook.P.scrollable obj
  method set_show_border = set Notebook.P.show_border obj
  method set_show_tabs = set Notebook.P.show_tabs obj
  method set_tab_border = set Notebook.P.tab_border obj
  method set_tab_hborder = set Notebook.P.tab_hborder obj
  method set_tab_pos = set Notebook.P.tab_pos obj
  method set_tab_vborder = set Notebook.P.tab_vborder obj
  method enable_popup = get Notebook.P.enable_popup obj
  method homogeneous_tabs = get Notebook.P.homogeneous obj
  method scrollable = get Notebook.P.scrollable obj
  method show_border = get Notebook.P.show_border obj
  method show_tabs = get Notebook.P.show_tabs obj
  method tab_hborder = get Notebook.P.tab_hborder obj
  method tab_pos = get Notebook.P.tab_pos obj
  method tab_vborder = get Notebook.P.tab_vborder obj
end

class virtual table_props = object
  val virtual obj : _ obj
  method set_columns = set Table.P.n_columns obj
  method set_rows = set Table.P.n_rows obj
  method set_homogeneous = set Table.P.homogeneous obj
  method set_row_spacings = set Table.P.row_spacing obj
  method set_col_spacings = set Table.P.column_spacing obj
  method columns = get Table.P.n_columns obj
  method rows = get Table.P.n_rows obj
  method homogeneous = get Table.P.homogeneous obj
  method row_spacings = get Table.P.row_spacing obj
  method col_spacings = get Table.P.column_spacing obj
end

