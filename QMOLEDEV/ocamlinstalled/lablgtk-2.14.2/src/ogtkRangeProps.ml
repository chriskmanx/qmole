open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param
open GtkRangeProps

class virtual ruler_props = object
  val virtual obj : _ obj
  method set_lower = set Ruler.P.lower obj
  method set_upper = set Ruler.P.upper obj
  method set_max_size = set Ruler.P.max_size obj
  method set_position = set Ruler.P.position obj
  method lower = get Ruler.P.lower obj
  method upper = get Ruler.P.upper obj
  method max_size = get Ruler.P.max_size obj
  method position = get Ruler.P.position obj
end

class virtual range_props = object
  val virtual obj : _ obj
  method set_adjustment =
    set {Range.P.adjustment with conv=GData.conv_adjustment} obj
  method set_inverted = set Range.P.inverted obj
  method set_update_policy = set Range.P.update_policy obj
  method adjustment =
    get {Range.P.adjustment with conv=GData.conv_adjustment} obj
  method inverted = get Range.P.inverted obj
  method update_policy = get Range.P.update_policy obj
end

class virtual range_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method adjust_bounds = self#connect Range.S.adjust_bounds
  method move_slider = self#connect Range.S.move_slider
  method value_changed = self#connect Range.S.value_changed
end

class virtual scale_props = object
  val virtual obj : _ obj
  method set_digits = set Scale.P.digits obj
  method set_draw_value = set Scale.P.draw_value obj
  method set_value_pos = set Scale.P.value_pos obj
  method digits = get Scale.P.digits obj
  method draw_value = get Scale.P.draw_value obj
  method value_pos = get Scale.P.value_pos obj
end

class virtual progress_bar_props = object
  val virtual obj : _ obj
  method set_adjustment =
    set {ProgressBar.P.adjustment with conv=GData.conv_adjustment} obj
  method set_orientation = set ProgressBar.P.orientation obj
  method set_fraction = set ProgressBar.P.fraction obj
  method set_pulse_step = set ProgressBar.P.pulse_step obj
  method set_text = set ProgressBar.P.text obj
  method set_ellipsize = set ProgressBar.P.ellipsize obj
  method adjustment =
    get {ProgressBar.P.adjustment with conv=GData.conv_adjustment} obj
  method orientation = get ProgressBar.P.orientation obj
  method fraction = get ProgressBar.P.fraction obj
  method pulse_step = get ProgressBar.P.pulse_step obj
  method text = get ProgressBar.P.text obj
  method ellipsize = get ProgressBar.P.ellipsize obj
end

