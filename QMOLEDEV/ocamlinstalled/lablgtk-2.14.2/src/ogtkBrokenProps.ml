open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param
open GtkBrokenProps

class virtual tree_item_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method collapse = self#connect TreeItem.S.collapse
  method expand = self#connect TreeItem.S.expand
end

class virtual old_editable_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method activate = self#connect OldEditable.S.activate
  method copy_clipboard = self#connect OldEditable.S.copy_clipboard
  method cut_clipboard = self#connect OldEditable.S.cut_clipboard
  method paste_clipboard = self#connect OldEditable.S.paste_clipboard
  method move_cursor = self#connect OldEditable.S.move_cursor
  method move_word = self#connect OldEditable.S.move_word
  method move_page = self#connect OldEditable.S.move_page
  method move_to_row = self#connect OldEditable.S.move_to_row
  method move_to_column = self#connect OldEditable.S.move_to_column
end

class virtual text_props = object
  val virtual obj : _ obj
  method set_hadjustment =
    set {Text.P.hadjustment with conv=GData.conv_adjustment} obj
  method set_vadjustment =
    set {Text.P.vadjustment with conv=GData.conv_adjustment} obj
  method set_line_wrap = set Text.P.line_wrap obj
  method set_word_wrap = set Text.P.word_wrap obj
  method hadjustment =
    get {Text.P.hadjustment with conv=GData.conv_adjustment} obj
  method vadjustment =
    get {Text.P.vadjustment with conv=GData.conv_adjustment} obj
  method line_wrap = get Text.P.line_wrap obj
  method word_wrap = get Text.P.word_wrap obj
end

