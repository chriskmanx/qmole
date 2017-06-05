open Gobject
open Data
module Object = GtkObject

open Gtk

module PrivateProps = struct
  let selection_mode =
    {name="selection-mode"; conv=GtkEnums.selection_mode_conv}
end

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module ListItem = struct
  let cast w : Gtk.list_item obj = try_cast w "GtkListItem"
  let create pl : Gtk.list_item obj = Object.make "GtkListItem" pl
end

module Liste = struct
  let cast w : Gtk.liste obj = try_cast w "GtkList"
  module P = struct
    let selection_mode : ([>`list],_) property = PrivateProps.selection_mode
  end
  module S = struct
    open GtkSignal
    let select_child =
      {name="select_child"; classe=`list; marshaller=fun f ->
       marshal1 (gobject : Gtk.widget obj data_conv)
         "GtkList::select_child" f}
    let selection_changed =
      {name="selection_changed"; classe=`list; marshaller=marshal_unit}
    let unselect_child =
      {name="unselect_child"; classe=`list; marshaller=fun f ->
       marshal1 (gobject : Gtk.widget obj data_conv)
         "GtkList::unselect_child" f}
  end
  let create pl : Gtk.liste obj = Object.make "GtkList" pl
  let make_params ~cont pl ?selection_mode =
    let pl = (may_cons P.selection_mode selection_mode pl) in
    cont pl
end

module Clist = struct
  let cast w : Gtk.clist obj = try_cast w "GtkCList"
  module P = struct
    let n_columns : ([>`clist],_) property = {name="n-columns"; conv=uint}
    let sort_type : ([>`clist],_) property =
      {name="sort-type"; conv=GtkEnums.sort_type_conv}
    let reorderable : ([>`clist],_) property =
      {name="reorderable"; conv=boolean}
    let row_height : ([>`clist],_) property = {name="row-height"; conv=uint}
    let selection_mode : ([>`clist],_) property = PrivateProps.selection_mode
    let shadow_type : ([>`clist],_) property =
      {name="shadow-type"; conv=GtkEnums.shadow_type_conv}
    let titles_active : ([>`clist],_) property =
      {name="titles-active"; conv=boolean}
    let use_drag_icons : ([>`clist],_) property =
      {name="use-drag-icons"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let click_column =
      {name="click_column"; classe=`clist; marshaller=fun f ->
       marshal1 int "GtkCList::click_column" f}
    let resize_column =
      {name="resize_column"; classe=`clist; marshaller=fun f ->
       marshal2 int int "GtkCList::resize_column" f}
    let scroll_horizontal =
      {name="scroll_horizontal"; classe=`clist; marshaller=fun f ->
       marshal2 GtkEnums.scroll_type_conv float "GtkCList::scroll_horizontal"
         (fun x1 x2 -> f x1 ~pos:x2)}
    let scroll_vertical =
      {name="scroll_vertical"; classe=`clist; marshaller=fun f ->
       marshal2 GtkEnums.scroll_type_conv float "GtkCList::scroll_vertical"
         (fun x1 x2 -> f x1 ~pos:x2)}
    let select_all =
      {name="select_all"; classe=`clist; marshaller=marshal_unit}
    let select_row =
      {name="select_row"; classe=`clist; marshaller=fun f ->
       marshal3 int int
         (unsafe_pointer_option : GdkEvent.Button.t option data_conv)
         "GtkCList::select_row"
         (fun x1 x2 x3 -> f ~row:x1 ~column:x2 ~event:x3)}
    let unselect_all =
      {name="unselect_all"; classe=`clist; marshaller=marshal_unit}
    let unselect_row =
      {name="unselect_row"; classe=`clist; marshaller=fun f ->
       marshal3 int int
         (unsafe_pointer_option : GdkEvent.Button.t option data_conv)
         "GtkCList::unselect_row"
         (fun x1 x2 x3 -> f ~row:x1 ~column:x2 ~event:x3)}
  end
  let make_params ~cont pl ?sort_type ?reorderable ?row_height
      ?selection_mode ?shadow_type ?titles_active ?use_drag_icons =
    let pl = (
      may_cons P.sort_type sort_type (
      may_cons P.reorderable reorderable (
      may_cons P.row_height row_height (
      may_cons P.selection_mode selection_mode (
      may_cons P.shadow_type shadow_type (
      may_cons P.titles_active titles_active (
      may_cons P.use_drag_icons use_drag_icons pl))))))) in
    cont pl
end

