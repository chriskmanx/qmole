open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param

open GtkPackProps

open GtkBinProps

class virtual alignment_props = object
  val virtual obj : _ obj
  method set_xalign = set Alignment.P.xalign obj
  method set_yalign = set Alignment.P.yalign obj
  method set_xscale = set Alignment.P.xscale obj
  method set_yscale = set Alignment.P.yscale obj
  method set_bottom_padding = set Alignment.P.bottom_padding obj
  method set_left_padding = set Alignment.P.left_padding obj
  method set_right_padding = set Alignment.P.right_padding obj
  method set_top_padding = set Alignment.P.top_padding obj
  method xalign = get Alignment.P.xalign obj
  method yalign = get Alignment.P.yalign obj
  method xscale = get Alignment.P.xscale obj
  method yscale = get Alignment.P.yscale obj
  method bottom_padding = get Alignment.P.bottom_padding obj
  method left_padding = get Alignment.P.left_padding obj
  method right_padding = get Alignment.P.right_padding obj
  method top_padding = get Alignment.P.top_padding obj
end

class virtual frame_props = object
  val virtual obj : _ obj
  method set_label = set Frame.P.label obj
  method set_label_widget =
    set {Frame.P.label_widget with conv=GObj.conv_widget_option} obj
  method set_label_xalign = set Frame.P.label_xalign obj
  method set_label_yalign = set Frame.P.label_yalign obj
  method set_shadow_type = set Frame.P.shadow_type obj
  method label = get Frame.P.label obj
  method label_widget =
    get {Frame.P.label_widget with conv=GObj.conv_widget_option} obj
  method label_xalign = get Frame.P.label_xalign obj
  method label_yalign = get Frame.P.label_yalign obj
  method shadow_type = get Frame.P.shadow_type obj
end

class virtual aspect_frame_props = object
  val virtual obj : _ obj
  method set_obey_child = set AspectFrame.P.obey_child obj
  method set_ratio = set AspectFrame.P.ratio obj
  method set_xalign = set AspectFrame.P.xalign obj
  method set_yalign = set AspectFrame.P.yalign obj
  method obey_child = get AspectFrame.P.obey_child obj
  method ratio = get AspectFrame.P.ratio obj
  method xalign = get AspectFrame.P.xalign obj
  method yalign = get AspectFrame.P.yalign obj
end

class virtual handle_box_props = object
  val virtual obj : _ obj
  method set_handle_position = set HandleBox.P.handle_position obj
  method set_snap_edge = set HandleBox.P.snap_edge obj
  method set_shadow_type = set HandleBox.P.shadow_type obj
  method handle_position = get HandleBox.P.handle_position obj
  method snap_edge = get HandleBox.P.snap_edge obj
  method shadow_type = get HandleBox.P.shadow_type obj
end

class virtual handle_box_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method child_attached = self#connect
    {HandleBox.S.child_attached with marshaller = fun f ->
     marshal1 GObj.conv_widget "GtkHandleBox::child_attached" f}
  method child_detached = self#connect
    {HandleBox.S.child_detached with marshaller = fun f ->
     marshal1 GObj.conv_widget "GtkHandleBox::child_detached" f}
end

class virtual scrolled_window_props = object
  val virtual obj : _ obj
  method set_hadjustment =
    set {ScrolledWindow.P.hadjustment with conv=GData.conv_adjustment} obj
  method set_vadjustment =
    set {ScrolledWindow.P.vadjustment with conv=GData.conv_adjustment} obj
  method set_hpolicy = set ScrolledWindow.P.hscrollbar_policy obj
  method set_vpolicy = set ScrolledWindow.P.vscrollbar_policy obj
  method set_placement = set ScrolledWindow.P.window_placement obj
  method set_shadow_type = set ScrolledWindow.P.shadow_type obj
  method hadjustment =
    get {ScrolledWindow.P.hadjustment with conv=GData.conv_adjustment} obj
  method vadjustment =
    get {ScrolledWindow.P.vadjustment with conv=GData.conv_adjustment} obj
  method hpolicy = get ScrolledWindow.P.hscrollbar_policy obj
  method vpolicy = get ScrolledWindow.P.vscrollbar_policy obj
  method placement = get ScrolledWindow.P.window_placement obj
  method shadow_type = get ScrolledWindow.P.shadow_type obj
end

class virtual viewport_props = object
  val virtual obj : _ obj
  method set_hadjustment =
    set {Viewport.P.hadjustment with conv=GData.conv_adjustment} obj
  method set_vadjustment =
    set {Viewport.P.vadjustment with conv=GData.conv_adjustment} obj
  method set_shadow_type = set Viewport.P.shadow_type obj
  method hadjustment =
    get {Viewport.P.hadjustment with conv=GData.conv_adjustment} obj
  method vadjustment =
    get {Viewport.P.vadjustment with conv=GData.conv_adjustment} obj
  method shadow_type = get Viewport.P.shadow_type obj
end

class virtual expander_props = object
  val virtual obj : _ obj
  method set_expanded = set Expander.P.expanded obj
  method set_label = set Expander.P.label obj
  method set_label_widget =
    set {Expander.P.label_widget with conv=GObj.conv_widget} obj
  method set_spacing = set Expander.P.spacing obj
  method set_use_underline = set Expander.P.use_underline obj
  method expanded = get Expander.P.expanded obj
  method label = get Expander.P.label obj
  method label_widget =
    get {Expander.P.label_widget with conv=GObj.conv_widget} obj
  method spacing = get Expander.P.spacing obj
  method use_underline = get Expander.P.use_underline obj
end

class virtual expander_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method activate = self#connect Expander.S.activate
end

