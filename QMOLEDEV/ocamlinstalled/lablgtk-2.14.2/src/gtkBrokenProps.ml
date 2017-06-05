open Gobject
open Data
module Object = GtkObject

open Gtk

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module TreeItem = struct
  let cast w : Gtk.tree_item obj = try_cast w "GtkTreeItem"
  module S = struct
    open GtkSignal
    let collapse =
      {name="collapse"; classe=`treeitem; marshaller=marshal_unit}
    let expand = {name="expand"; classe=`treeitem; marshaller=marshal_unit}
  end
  let create pl : Gtk.tree_item obj = Object.make "GtkTreeItem" pl
  external set_subtree : [>`treeitem] obj -> [>`widget] obj -> unit
    = "ml_gtk_tree_item_set_subtree"
  external remove_subtree : [>`treeitem] obj -> unit
    = "ml_gtk_tree_item_remove_subtree"
  external collapse : [>`treeitem] obj -> unit = "ml_gtk_tree_item_collapse"
  external expand : [>`treeitem] obj -> unit = "ml_gtk_tree_item_expand"
end

module Tree = struct
  let cast w : Gtk.tree obj = try_cast w "GtkTree"
  module S = struct
    open GtkSignal
    let select_child =
      {name="select_child"; classe=`tree; marshaller=fun f ->
       marshal1 (gobject : Gtk.widget obj data_conv)
         "GtkTree::select_child" f}
    let selection_changed =
      {name="selection_changed"; classe=`tree; marshaller=marshal_unit}
    let unselect_child =
      {name="unselect_child"; classe=`tree; marshaller=fun f ->
       marshal1 (gobject : Gtk.widget obj data_conv)
         "GtkTree::unselect_child" f}
  end
  let create pl : Gtk.tree obj = Object.make "GtkTree" pl
end

module OldEditable = struct
  let cast w : Gtk.old_editable obj = try_cast w "GtkOldEditable"
  module P = struct
    let text_position : ([>`oldeditable],_) property =
      {name="text-position"; conv=int}
  end
  module S = struct
    open GtkSignal
    let activate =
      {name="activate"; classe=`oldeditable; marshaller=marshal_unit}
    let copy_clipboard =
      {name="copy_clipboard"; classe=`oldeditable; marshaller=marshal_unit}
    let cut_clipboard =
      {name="cut_clipboard"; classe=`oldeditable; marshaller=marshal_unit}
    let paste_clipboard =
      {name="paste_clipboard"; classe=`oldeditable; marshaller=marshal_unit}
    let move_cursor =
      {name="move_cursor"; classe=`oldeditable; marshaller=fun f ->
       marshal2 int int "GtkOldEditable::move_cursor" f}
    let move_word =
      {name="move_word"; classe=`oldeditable; marshaller=fun f ->
       marshal1 int "GtkOldEditable::move_word" f}
    let move_page =
      {name="move_page"; classe=`oldeditable; marshaller=fun f ->
       marshal1 int "GtkOldEditable::move_page" f}
    let move_to_row =
      {name="move_to_row"; classe=`oldeditable; marshaller=fun f ->
       marshal1 int "GtkOldEditable::move_to_row" f}
    let move_to_column =
      {name="move_to_column"; classe=`oldeditable; marshaller=fun f ->
       marshal1 int "GtkOldEditable::move_to_column" f}
  end
  external claim_selection :
    [>`oldeditable] obj -> claim:bool -> time:int -> unit
    = "ml_gtk_old_editable_claim_selection"
  external changed : [>`oldeditable] obj -> unit
    = "ml_gtk_old_editable_changed"
end

module Text = struct
  let cast w : Gtk.text obj = try_cast w "GtkText"
  module P = struct
    let hadjustment : ([>`text],_) property =
      {name="hadjustment"; conv=(gobject : Gtk.adjustment obj data_conv)}
    let vadjustment : ([>`text],_) property =
      {name="vadjustment"; conv=(gobject : Gtk.adjustment obj data_conv)}
    let editable : ([>`text],_) property = {name="editable"; conv=boolean}
    let line_wrap : ([>`text],_) property = {name="line-wrap"; conv=boolean}
    let word_wrap : ([>`text],_) property = {name="word_wrap"; conv=boolean}
  end
  let create pl : Gtk.text obj = Object.make "GtkText" pl
  let make_params ~cont pl ?hadjustment ?vadjustment ?editable ?line_wrap
      ?word_wrap =
    let pl = (
      may_cons P.hadjustment hadjustment (
      may_cons P.vadjustment vadjustment (
      may_cons P.editable editable (
      may_cons P.line_wrap line_wrap (
      may_cons P.word_wrap word_wrap pl))))) in
    cont pl
end

