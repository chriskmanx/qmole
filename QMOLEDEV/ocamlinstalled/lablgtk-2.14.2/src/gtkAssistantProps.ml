open Gobject
open Data
module Object = GtkObject

open Gtk

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module Assistant = struct
  let cast w : Gtk.assistant obj = try_cast w "GtkAssistant"
  module S = struct
    open GtkSignal
    let apply = {name="apply"; classe=`assistant; marshaller=marshal_unit}
    let cancel = {name="cancel"; classe=`assistant; marshaller=marshal_unit}
    let close = {name="close"; classe=`assistant; marshaller=marshal_unit}
    let leave = {name="leave"; classe=`assistant; marshaller=marshal_unit}
    let prepare =
      {name="prepare"; classe=`assistant; marshaller=marshal_unit}
  end
  let create pl : Gtk.assistant obj = Object.make "GtkAssistant" pl
  external get_current_page : [>`assistant] obj -> int
    = "ml_gtk_assistant_get_current_page"
  external set_current_page : [>`assistant] obj -> int->unit
    = "ml_gtk_assistant_set_current_page"
  external get_n_pages : [>`assistant] obj -> int
    = "ml_gtk_assistant_get_n_pages"
  external get_nth_page : [>`assistant] obj -> int -> widget obj
    = "ml_gtk_assistant_get_nth_page"
  external insert_page : [>`assistant] obj -> widget obj -> int -> int
    = "ml_gtk_assistant_insert_page"
  external set_page_type :
    [>`assistant] obj -> widget obj -> GtkEnums.assistant_page_type -> unit
    = "ml_gtk_assistant_set_page_type"
  external get_page_type :
    [>`assistant] obj -> widget obj -> GtkEnums.assistant_page_type
    = "ml_gtk_assistant_get_page_type"
  external set_page_title : [>`assistant] obj -> widget obj -> string -> unit
    = "ml_gtk_assistant_set_page_title"
  external get_page_title : [>`assistant] obj -> widget obj -> string
    = "ml_gtk_assistant_get_page_title"
  external set_page_header_image :
    [>`assistant] obj -> widget obj -> GdkPixbuf.pixbuf -> unit
    = "ml_gtk_assistant_set_page_header_image"
  external get_page_header_image :
    [>`assistant] obj -> widget obj -> GdkPixbuf.pixbuf
    = "ml_gtk_assistant_get_page_header_image"
  external set_page_side_image :
    [>`assistant] obj -> widget obj -> GdkPixbuf.pixbuf -> unit
    = "ml_gtk_assistant_set_page_side_image"
  external get_page_side_image :
    [>`assistant] obj -> widget obj -> GdkPixbuf.pixbuf
    = "ml_gtk_assistant_get_page_side_image"
  external set_page_complete :
    [>`assistant] obj -> widget obj -> bool -> unit
    = "ml_gtk_assistant_set_page_complete"
  external get_page_complete : [>`assistant] obj -> widget obj -> bool
    = "ml_gtk_assistant_get_page_complete"
  external add_action_widget : [>`assistant] obj -> widget obj -> unit
    = "ml_gtk_assistant_add_action_widget"
  external remove_action_widget : [>`assistant] obj -> widget obj -> unit
    = "ml_gtk_assistant_remove_action_widget"
  external update_buttons_state : [>`assistant] obj -> unit
    = "ml_gtk_assistant_update_buttons_state"
end

