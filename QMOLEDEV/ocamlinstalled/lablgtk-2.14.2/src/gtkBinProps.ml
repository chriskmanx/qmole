open Gobject
open Data
module Object = GtkObject

open Gtk

module PrivateProps = struct
  let hadjustment =
    {name="hadjustment"; conv=(gobject : Gtk.adjustment obj data_conv)}
  let shadow_type = {name="shadow-type"; conv=GtkEnums.shadow_type_conv}
  let vadjustment =
    {name="vadjustment"; conv=(gobject : Gtk.adjustment obj data_conv)}
  let xalign = {name="xalign"; conv=float}
  let yalign = {name="yalign"; conv=float}
end

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module Bin = struct let cast w : Gtk.bin obj = try_cast w "GtkBin"
end

module Alignment = struct
  let cast w : Gtk.alignment obj = try_cast w "GtkAlignment"
  module P = struct
    let xalign : ([>`alignment],_) property = PrivateProps.xalign
    let yalign : ([>`alignment],_) property = PrivateProps.yalign
    let xscale : ([>`alignment],_) property = {name="xscale"; conv=float}
    let yscale : ([>`alignment],_) property = {name="yscale"; conv=float}
    let bottom_padding : ([>`alignment],_) property =
      {name="bottom-padding"; conv=uint}
    let left_padding : ([>`alignment],_) property =
      {name="left-padding"; conv=uint}
    let right_padding : ([>`alignment],_) property =
      {name="right-padding"; conv=uint}
    let top_padding : ([>`alignment],_) property =
      {name="top-padding"; conv=uint}
  end
  let create pl : Gtk.alignment obj = Object.make "GtkAlignment" pl
  let make_params ~cont pl ?xalign ?yalign ?xscale ?yscale =
    let pl = (
      may_cons P.xalign xalign (
      may_cons P.yalign yalign (
      may_cons P.xscale xscale (
      may_cons P.yscale yscale pl)))) in
    cont pl
end

module Frame = struct
  let cast w : Gtk.frame obj = try_cast w "GtkFrame"
  module P = struct
    let label : ([>`frame],_) property = {name="label"; conv=string_option}
    let label_widget : ([>`frame],_) property =
      {name="label-widget";
       conv=(gobject_option : Gtk.widget obj option data_conv)}
    let label_xalign : ([>`frame],_) property =
      {name="label-xalign"; conv=float}
    let label_yalign : ([>`frame],_) property =
      {name="label-yalign"; conv=float}
    let shadow_type : ([>`frame],_) property = PrivateProps.shadow_type
  end
  let create pl : Gtk.frame obj = Object.make "GtkFrame" pl
  let make_params ~cont pl ?label ?label_xalign ?label_yalign ?shadow_type =
    let pl = (
      may_cons_opt P.label label (
      may_cons P.label_xalign label_xalign (
      may_cons P.label_yalign label_yalign (
      may_cons P.shadow_type shadow_type pl)))) in
    cont pl
end

module AspectFrame = struct
  let cast w : Gtk.aspect_frame obj = try_cast w "GtkAspectFrame"
  module P = struct
    let obey_child : ([>`aspectframe],_) property =
      {name="obey-child"; conv=boolean}
    let ratio : ([>`aspectframe],_) property = {name="ratio"; conv=float}
    let xalign : ([>`aspectframe],_) property = PrivateProps.xalign
    let yalign : ([>`aspectframe],_) property = PrivateProps.yalign
  end
  let create pl : Gtk.aspect_frame obj = Object.make "GtkAspectFrame" pl
  let make_params ~cont pl ?obey_child ?ratio ?xalign ?yalign =
    let pl = (
      may_cons P.obey_child obey_child (
      may_cons P.ratio ratio (
      may_cons P.xalign xalign (
      may_cons P.yalign yalign pl)))) in
    cont pl
end

module EventBox = struct
  let cast w : Gtk.event_box obj = try_cast w "GtkEventBox"
  let create pl : Gtk.event_box obj = Object.make "GtkEventBox" pl
end

module Invisible = struct
  let cast w : Gtk.invisible obj = try_cast w "GtkInvisible"
  module P = struct
    let screen : ([>`invisible],_) property =
      {name="screen"; conv=(gobject : Gdk.screen data_conv)}
  end
  let create pl : Gtk.invisible obj = Object.make "GtkInvisible" pl
end

module HandleBox = struct
  let cast w : Gtk.handle_box obj = try_cast w "GtkHandleBox"
  module P = struct
    let handle_position : ([>`handlebox],_) property =
      {name="handle-position"; conv=GtkEnums.position_type_conv}
    let snap_edge : ([>`handlebox],_) property =
      {name="snap-edge"; conv=GtkEnums.position_type_conv}
    let shadow_type : ([>`handlebox],_) property =
      {name="shadow_type"; conv=GtkEnums.shadow_type_conv}
    let snap_edge_set : ([>`handlebox],_) property =
      {name="snap-edge-set"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let child_attached =
      {name="child_attached"; classe=`handlebox; marshaller=fun f ->
       marshal1 (gobject : Gtk.widget obj data_conv)
         "GtkHandleBox::child_attached" f}
    let child_detached =
      {name="child_detached"; classe=`handlebox; marshaller=fun f ->
       marshal1 (gobject : Gtk.widget obj data_conv)
         "GtkHandleBox::child_detached" f}
  end
  let create pl : Gtk.handle_box obj = Object.make "GtkHandleBox" pl
  let make_params ~cont pl ?handle_position ?snap_edge ?shadow_type =
    let pl = (
      may_cons P.handle_position handle_position (
      may_cons P.snap_edge snap_edge (
      may_cons P.shadow_type shadow_type pl))) in
    cont pl
end

module ScrolledWindow = struct
  let cast w : Gtk.scrolled_window obj = try_cast w "GtkScrolledWindow"
  module P = struct
    let hadjustment : ([>`scrolledwindow],_) property = PrivateProps.hadjustment
    let vadjustment : ([>`scrolledwindow],_) property = PrivateProps.vadjustment
    let hscrollbar_policy : ([>`scrolledwindow],_) property =
      {name="hscrollbar-policy"; conv=GtkEnums.policy_type_conv}
    let vscrollbar_policy : ([>`scrolledwindow],_) property =
      {name="vscrollbar-policy"; conv=GtkEnums.policy_type_conv}
    let window_placement : ([>`scrolledwindow],_) property =
      {name="window-placement"; conv=GtkEnums.corner_type_conv}
    let shadow_type : ([>`scrolledwindow],_) property = PrivateProps.shadow_type
  end
  module S = struct
    open GtkSignal
    let move_focus_out =
      {name="move_focus_out"; classe=`scrolledwindow; marshaller=fun f ->
       marshal1 GtkEnums.direction_type_conv
         "GtkScrolledWindow::move_focus_out" f}
    let scroll_child =
      {name="scroll_child"; classe=`scrolledwindow; marshaller=fun f ->
       marshal2 GtkEnums.scroll_type_conv boolean
         "GtkScrolledWindow::scroll_child" f}
  end
  let create pl : Gtk.scrolled_window obj =
    Object.make "GtkScrolledWindow" pl
  external add_with_viewport :
    [>`scrolledwindow] obj -> [>`widget] obj -> unit
    = "ml_gtk_scrolled_window_add_with_viewport"
  let make_params ~cont pl ?hadjustment ?vadjustment ?hpolicy ?vpolicy
      ?placement ?shadow_type =
    let pl = (
      may_cons P.hadjustment hadjustment (
      may_cons P.vadjustment vadjustment (
      may_cons P.hscrollbar_policy hpolicy (
      may_cons P.vscrollbar_policy vpolicy (
      may_cons P.window_placement placement (
      may_cons P.shadow_type shadow_type pl)))))) in
    cont pl
end

module Viewport = struct
  let cast w : Gtk.viewport obj = try_cast w "GtkViewport"
  module P = struct
    let hadjustment : ([>`viewport],_) property = PrivateProps.hadjustment
    let vadjustment : ([>`viewport],_) property = PrivateProps.vadjustment
    let shadow_type : ([>`viewport],_) property = PrivateProps.shadow_type
  end
  module S = struct
    open GtkSignal
    let set_scroll_adjustments =
      {name="set_scroll_adjustments"; classe=`viewport; marshaller=fun f ->
       marshal2 (gobject_option : Gtk.adjustment obj option data_conv)
         (gobject_option : Gtk.adjustment obj option data_conv)
         "GtkViewport::set_scroll_adjustments" f}
  end
  let create pl : Gtk.viewport obj = Object.make "GtkViewport" pl
  let make_params ~cont pl ?hadjustment ?vadjustment ?shadow_type =
    let pl = (
      may_cons P.hadjustment hadjustment (
      may_cons P.vadjustment vadjustment (
      may_cons P.shadow_type shadow_type pl))) in
    cont pl
end

module Expander = struct
  let cast w : Gtk.expander obj = try_cast w "GtkExpander"
  module P = struct
    let expanded : ([>`expander],_) property =
      {name="expanded"; conv=boolean}
    let label : ([>`expander],_) property = {name="label"; conv=string}
    let label_widget : ([>`expander],_) property =
      {name="label-widget"; conv=(gobject : Gtk.widget obj data_conv)}
    let spacing : ([>`expander],_) property = {name="spacing"; conv=int}
    let use_underline : ([>`expander],_) property =
      {name="use-underline"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let activate =
      {name="activate"; classe=`expander; marshaller=marshal_unit}
  end
  let create pl : Gtk.expander obj = Object.make "GtkExpander" pl
  let make_params ~cont pl ?expanded ?label ?spacing ?use_underline =
    let pl = (
      may_cons P.expanded expanded (
      may_cons P.label label (
      may_cons P.spacing spacing (
      may_cons P.use_underline use_underline pl)))) in
    cont pl
end

