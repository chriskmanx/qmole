open Gobject
open Data
module Object = GtkObject

open Gtk

module PrivateProps = struct
  let adjustment =
    {name="adjustment"; conv=(gobject : Gtk.adjustment obj data_conv)}
end

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module Ruler = struct
  let cast w : Gtk.ruler obj = try_cast w "GtkRuler"
  module P = struct
    let lower : ([>`ruler],_) property = {name="lower"; conv=double}
    let upper : ([>`ruler],_) property = {name="upper"; conv=double}
    let max_size : ([>`ruler],_) property = {name="max-size"; conv=double}
    let position : ([>`ruler],_) property = {name="position"; conv=double}
  end
  let create (dir : Gtk.Tags.orientation) pl : Gtk.ruler obj =
    Object.make (if dir = `HORIZONTAL then "GtkHRuler" else "GtkVRuler")  pl
  external set_metric : [>`ruler] obj -> Tags.metric_type -> unit
    = "ml_gtk_ruler_set_metric"
  let make_params ~cont pl ?lower ?upper ?max_size ?position =
    let pl = (
      may_cons P.lower lower (
      may_cons P.upper upper (
      may_cons P.max_size max_size (
      may_cons P.position position pl)))) in
    cont pl
end

module Range = struct
  let cast w : Gtk.range obj = try_cast w "GtkRange"
  module P = struct
    let adjustment : ([>`range],_) property = PrivateProps.adjustment
    let inverted : ([>`range],_) property = {name="inverted"; conv=boolean}
    let update_policy : ([>`range],_) property =
      {name="update-policy"; conv=GtkEnums.update_type_conv}
  end
  module S = struct
    open GtkSignal
    let adjust_bounds =
      {name="adjust_bounds"; classe=`range; marshaller=fun f ->
       marshal1 double "GtkRange::adjust_bounds" f}
    let move_slider =
      {name="move_slider"; classe=`range; marshaller=fun f ->
       marshal1 GtkEnums.scroll_type_conv "GtkRange::move_slider" f}
    let value_changed =
      {name="value_changed"; classe=`range; marshaller=marshal_unit}
  end
  let make_params ~cont pl ?adjustment ?inverted ?update_policy =
    let pl = (
      may_cons P.adjustment adjustment (
      may_cons P.inverted inverted (
      may_cons P.update_policy update_policy pl))) in
    cont pl
end

module Scale = struct
  let cast w : Gtk.scale obj = try_cast w "GtkScale"
  module P = struct
    let digits : ([>`scale],_) property = {name="digits"; conv=int}
    let draw_value : ([>`scale],_) property =
      {name="draw-value"; conv=boolean}
    let value_pos : ([>`scale],_) property =
      {name="value-pos"; conv=GtkEnums.position_type_conv}
  end
  module S = struct
    open GtkSignal
    let format_value =
      {name="format_value"; classe=`scale; marshaller=fun f ->
       marshal1_ret ~ret:string double "GtkScale::format_value" f}
  end
  let create (dir : Gtk.Tags.orientation) pl : Gtk.scale obj =
    Object.make (if dir = `HORIZONTAL then "GtkHScale" else "GtkVScale")  pl
  let make_params ~cont pl ?digits ?draw_value ?value_pos =
    let pl = (
      may_cons P.digits digits (
      may_cons P.draw_value draw_value (
      may_cons P.value_pos value_pos pl))) in
    cont pl
end

module Scrollbar = struct
  let cast w : Gtk.scrollbar obj = try_cast w "GtkScrollbar"
  let create (dir : Gtk.Tags.orientation) pl : Gtk.scrollbar obj =
    Object.make
      (if dir = `HORIZONTAL then "GtkHScrollbar" else "GtkVScrollbar")  pl
end

module ProgressBar = struct
  let cast w : Gtk.progress_bar obj = try_cast w "GtkProgressBar"
  module P = struct
    let adjustment : ([>`progressbar],_) property = PrivateProps.adjustment
    let orientation : ([>`progressbar],_) property =
      {name="orientation"; conv=GtkEnums.progress_bar_orientation_conv}
    let activity_blocks : ([>`progressbar],_) property =
      {name="activity-blocks"; conv=uint}
    let activity_step : ([>`progressbar],_) property =
      {name="activity-step"; conv=uint}
    let bar_style : ([>`progressbar],_) property =
      {name="bar-style"; conv=GtkEnums.progress_bar_style_conv}
    let discrete_blocks : ([>`progressbar],_) property =
      {name="discrete-blocks"; conv=uint}
    let fraction : ([>`progressbar],_) property =
      {name="fraction"; conv=double}
    let pulse_step : ([>`progressbar],_) property =
      {name="pulse-step"; conv=double}
    let text : ([>`progressbar],_) property = {name="text"; conv=string}
    let ellipsize : ([>`progressbar],_) property =
      {name="ellipsize"; conv=PangoEnums.ellipsize_mode_conv}
  end
  let create pl : Gtk.progress_bar obj = Object.make "GtkProgressBar" pl
  external pulse : [>`progressbar] obj -> unit = "ml_gtk_progress_bar_pulse"
  let make_params ~cont pl ?orientation ?pulse_step =
    let pl = (
      may_cons P.orientation orientation (
      may_cons P.pulse_step pulse_step pl)) in
    cont pl
end

