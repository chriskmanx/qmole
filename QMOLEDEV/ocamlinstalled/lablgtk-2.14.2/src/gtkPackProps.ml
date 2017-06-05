open Gobject
open Data
module Object = GtkObject

open Gtk

module PrivateProps = struct
  let homogeneous = {name="homogeneous"; conv=boolean}
end

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module Box = struct
  let cast w : Gtk.box obj = try_cast w "GtkBox"
  module P = struct
    let homogeneous : ([>`box],_) property = PrivateProps.homogeneous
    let spacing : ([>`box],_) property = {name="spacing"; conv=int}
  end
  let create (dir : Gtk.Tags.orientation) pl : Gtk.box obj =
    Object.make (if dir = `HORIZONTAL then "GtkHBox" else "GtkVBox")  pl
  external pack_start :
    [>`box] obj ->
    [>`widget] obj -> expand:bool -> fill:bool -> padding:int -> unit
    = "ml_gtk_box_pack_start"
  external pack_end :
    [>`box] obj ->
    [>`widget] obj -> expand:bool -> fill:bool -> padding:int -> unit
    = "ml_gtk_box_pack_end"
  external reorder_child : [>`box] obj -> [>`widget] obj -> pos:int -> unit
    = "ml_gtk_box_reorder_child"
  external query_child_packing : [>`box] obj -> [>`widget] obj -> box_packing
    = "ml_gtk_box_query_child_packing"
  external set_child_packing :
    [>`box] obj ->
    [>`widget] obj -> ?expand:bool -> ?fill:bool ->
       ?padding:int -> ?from:Tags.pack_type -> unit
    = "ml_gtk_box_set_child_packing_bc" "ml_gtk_box_set_child_packing"
  let make_params ~cont pl ?homogeneous ?spacing =
    let pl = (
      may_cons P.homogeneous homogeneous (
      may_cons P.spacing spacing pl)) in
    cont pl
end

module ButtonBox = struct
  let cast w : Gtk.button_box obj = try_cast w "GtkButtonBox"
  module P = struct
    let layout_style : ([>`buttonbox],_) property =
      {name="layout-style"; conv=GtkEnums.button_box_style_conv}
  end
  let create (dir : Gtk.Tags.orientation) pl : Gtk.button_box obj =
    Object.make
      (if dir = `HORIZONTAL then "GtkHButtonBox" else "GtkVButtonBox")  pl
  external get_child_secondary : [>`buttonbox] obj -> [>`widget] obj -> bool
    = "ml_gtk_button_box_get_child_secondary"
  external set_child_secondary :
    [>`buttonbox] obj -> [>`widget] obj -> bool -> unit
    = "ml_gtk_button_box_set_child_secondary"
end

module Fixed = struct
  let cast w : Gtk.fixed obj = try_cast w "GtkFixed"
  let create pl : Gtk.fixed obj = Object.make "GtkFixed" pl
  external put : [>`fixed] obj -> [>`widget] obj -> x:int -> y:int -> unit
    = "ml_gtk_fixed_put"
  external move : [>`fixed] obj -> [>`widget] obj -> x:int -> y:int -> unit
    = "ml_gtk_fixed_move"
  external set_has_window : [>`fixed] obj -> bool -> unit
    = "ml_gtk_fixed_set_has_window"
  external get_has_window : [>`fixed] obj -> bool
    = "ml_gtk_fixed_get_has_window"
end

module Paned = struct
  let cast w : Gtk.paned obj = try_cast w "GtkPaned"
  module P = struct
    let position : ([>`paned],_) property = {name="position"; conv=int}
    let position_set : ([>`paned],_) property =
      {name="position-set"; conv=boolean}
    let max_position : ([>`paned],_) property =
      {name="max-position"; conv=int}
    let min_position : ([>`paned],_) property =
      {name="min-position"; conv=int}
  end
  let create (dir : Gtk.Tags.orientation) pl : Gtk.paned obj =
    Object.make (if dir = `HORIZONTAL then "GtkHPaned" else "GtkVPaned")  pl
  external add1 : [>`paned] obj -> [>`widget] obj -> unit
    = "ml_gtk_paned_add1"
  external add2 : [>`paned] obj -> [>`widget] obj -> unit
    = "ml_gtk_paned_add2"
  external pack1 :
    [>`paned] obj -> [>`widget] obj -> resize:bool -> shrink:bool -> unit
    = "ml_gtk_paned_pack1"
  external pack2 :
    [>`paned] obj -> [>`widget] obj -> resize:bool -> shrink:bool -> unit
    = "ml_gtk_paned_pack2"
  external child1 : [>`paned] obj -> widget obj = "ml_gtk_paned_child1"
  external child2 : [>`paned] obj -> widget obj = "ml_gtk_paned_child2"
end

module Layout = struct
  let cast w : Gtk.layout obj = try_cast w "GtkLayout"
  module P = struct
    let hadjustment : ([>`layout],_) property =
      {name="hadjustment"; conv=(gobject : Gtk.adjustment obj data_conv)}
    let height : ([>`layout],_) property = {name="height"; conv=uint}
    let vadjustment : ([>`layout],_) property =
      {name="vadjustment"; conv=(gobject : Gtk.adjustment obj data_conv)}
    let width : ([>`layout],_) property = {name="width"; conv=uint}
  end
  let create pl : Gtk.layout obj = Object.make "GtkLayout" pl
  external put : [>`layout] obj -> [>`widget] obj -> x:int -> y:int -> unit
    = "ml_gtk_layout_put"
  external move : [>`layout] obj -> [>`widget] obj -> x:int -> y:int -> unit
    = "ml_gtk_layout_move"
  external freeze : [>`layout] obj -> unit = "ml_gtk_layout_freeze"
  external thaw : [>`layout] obj -> unit = "ml_gtk_layout_thaw"
  external bin_window : [>`layout] obj -> Gdk.window
    = "ml_gtk_layout_bin_window"
  let make_params ~cont pl ?hadjustment ?height ?vadjustment ?width =
    let pl = (
      may_cons P.hadjustment hadjustment (
      may_cons P.height height (
      may_cons P.vadjustment vadjustment (
      may_cons P.width width pl)))) in
    cont pl
end

module Notebook = struct
  let cast w : Gtk.notebook obj = try_cast w "GtkNotebook"
  module P = struct
    let enable_popup : ([>`notebook],_) property =
      {name="enable-popup"; conv=boolean}
    let homogeneous : ([>`notebook],_) property = PrivateProps.homogeneous
    let page : ([>`notebook],_) property = {name="page"; conv=int}
    let scrollable : ([>`notebook],_) property =
      {name="scrollable"; conv=boolean}
    let show_border : ([>`notebook],_) property =
      {name="show-border"; conv=boolean}
    let show_tabs : ([>`notebook],_) property =
      {name="show-tabs"; conv=boolean}
    let tab_border : ([>`notebook],_) property =
      {name="tab-border"; conv=uint}
    let tab_hborder : ([>`notebook],_) property =
      {name="tab-hborder"; conv=uint}
    let tab_pos : ([>`notebook],_) property =
      {name="tab-pos"; conv=GtkEnums.position_type_conv}
    let tab_vborder : ([>`notebook],_) property =
      {name="tab-vborder"; conv=uint}
  end
  module S = struct
    open GtkSignal
    let switch_page =
      {name="switch_page"; classe=`notebook; marshaller=fun f ->
       marshal2 pointer int "GtkNotebook::switch_page" f}
  end
  let create pl : Gtk.notebook obj = Object.make "GtkNotebook" pl
  external insert_page_menu :
    [>`notebook] obj ->
    [>`widget] obj -> tab_label:[>`widget] optobj ->
      menu_label:[>`widget] optobj -> ?pos:int -> int
    = "ml_gtk_notebook_insert_page_menu"
  external remove_page : [>`notebook] obj -> int -> unit
    = "ml_gtk_notebook_remove_page"
  external get_current_page : [>`notebook] obj -> int
    = "ml_gtk_notebook_get_current_page"
  external get_nth_page : [>`notebook] obj -> int -> widget obj
    = "ml_gtk_notebook_get_nth_page"
  external page_num : [>`notebook] obj -> [>`widget] obj -> int
    = "ml_gtk_notebook_page_num"
  external next_page : [>`notebook] obj -> unit = "ml_gtk_notebook_next_page"
  external prev_page : [>`notebook] obj -> unit = "ml_gtk_notebook_prev_page"
  external get_tab_label : [>`notebook] obj -> [>`widget] obj -> widget obj
    = "ml_gtk_notebook_get_tab_label"
  external set_tab_label :
    [>`notebook] obj -> [>`widget] obj -> [>`widget] obj -> unit
    = "ml_gtk_notebook_set_tab_label"
  external get_menu_label : [>`notebook] obj -> [>`widget] obj -> widget obj
    = "ml_gtk_notebook_get_menu_label"
  external set_menu_label :
    [>`notebook] obj -> [>`widget] obj -> [>`widget] obj -> unit
    = "ml_gtk_notebook_set_menu_label"
  external reorder_child : [>`notebook] obj -> [>`widget] obj -> int -> unit
    = "ml_gtk_notebook_reorder_child"
  let make_params ~cont pl ?enable_popup ?homogeneous_tabs ?scrollable
      ?show_border ?show_tabs ?tab_border ?tab_pos =
    let pl = (
      may_cons P.enable_popup enable_popup (
      may_cons P.homogeneous homogeneous_tabs (
      may_cons P.scrollable scrollable (
      may_cons P.show_border show_border (
      may_cons P.show_tabs show_tabs (
      may_cons P.tab_border tab_border (
      may_cons P.tab_pos tab_pos pl))))))) in
    cont pl
end

module Table = struct
  let cast w : Gtk.table obj = try_cast w "GtkTable"
  module P = struct
    let n_columns : ([>`table],_) property = {name="n-columns"; conv=uint}
    let n_rows : ([>`table],_) property = {name="n-rows"; conv=uint}
    let homogeneous : ([>`table],_) property = PrivateProps.homogeneous
    let row_spacing : ([>`table],_) property =
      {name="row-spacing"; conv=uint}
    let column_spacing : ([>`table],_) property =
      {name="column-spacing"; conv=uint}
  end
  let create pl : Gtk.table obj = Object.make "GtkTable" pl
  external attach :
    [>`table] obj ->
    [>`widget] obj -> left:int -> right:int -> top:int -> bottom:int ->
     xoptions:Tags.attach_options list -> yoptions:Tags.attach_options list ->
     xpadding:int -> ypadding:int -> unit
    = "ml_gtk_table_attach_bc" "ml_gtk_table_attach"
  external set_row_spacing : [>`table] obj -> int -> int -> unit
    = "ml_gtk_table_set_row_spacing"
  external set_col_spacing : [>`table] obj -> int -> int -> unit
    = "ml_gtk_table_set_col_spacing"
  let make_params ~cont pl ?columns ?rows ?homogeneous ?row_spacings
      ?col_spacings =
    let pl = (
      may_cons P.n_columns columns (
      may_cons P.n_rows rows (
      may_cons P.homogeneous homogeneous (
      may_cons P.row_spacing row_spacings (
      may_cons P.column_spacing col_spacings pl))))) in
    cont pl
end

module SizeGroup = struct
  let cast w : size_group = try_cast w "GtkSizeGroup"
  module P = struct
    let mode : ([>`sizegroup],_) property =
      {name="mode"; conv=GtkEnums.size_group_mode_conv}
  end
  let create pl : size_group = Gobject.unsafe_create "GtkSizeGroup" pl
  external add_widget : [>`sizegroup] obj -> [>`widget] obj -> unit
    = "ml_gtk_size_group_add_widget"
  external remove_widget : [>`sizegroup] obj -> [>`widget] obj -> unit
    = "ml_gtk_size_group_remove_widget"
end

