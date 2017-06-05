open Gobject
open Data
module Object = GtkObject

open Gtk
module Internal = struct
  let tree_path_string = ref (unsafe_pointer : tree_path data_conv)
  let tree_path_copy = ref (unsafe_pointer : tree_path data_conv)
end

module PrivateProps = struct
  let model =
    {name="model"; conv=(gobject_option : tree_model option data_conv)}
  let reorderable = {name="reorderable"; conv=boolean}
  let visible = {name="visible"; conv=boolean}
  let width = {name="width"; conv=int}
end

let may_cons = Property.may_cons
let may_cons_opt = Property.may_cons_opt

module TreeView = struct
  let cast w : Gtk.tree_view obj = try_cast w "GtkTreeView"
  module P = struct
    let enable_search : ([>`treeview],_) property =
      {name="enable-search"; conv=boolean}
    let expander_column : ([>`treeview],_) property =
      {name="expander-column";
       conv=(gobject_option : Gtk.tree_view_column obj option data_conv)}
    let fixed_height_mode : ([>`treeview],_) property =
      {name="fixed-height-mode"; conv=boolean}
    let hadjustment : ([>`treeview],_) property =
      {name="hadjustment"; conv=(gobject : Gtk.adjustment obj data_conv)}
    let headers_clickable : ([>`treeview],_) property =
      {name="headers-clickable"; conv=boolean}
    let headers_visible : ([>`treeview],_) property =
      {name="headers-visible"; conv=boolean}
    let model : ([>`treeview],_) property = PrivateProps.model
    let reorderable : ([>`treeview],_) property = PrivateProps.reorderable
    let rules_hint : ([>`treeview],_) property =
      {name="rules-hint"; conv=boolean}
    let search_column : ([>`treeview],_) property =
      {name="search-column"; conv=int}
    let vadjustment : ([>`treeview],_) property =
      {name="vadjustment"; conv=(gobject : Gtk.adjustment obj data_conv)}
    let hover_expand : ([>`treeview],_) property =
      {name="hover-expand"; conv=boolean}
    let hover_selection : ([>`treeview],_) property =
      {name="hover-selection"; conv=boolean}
    let tooltip_column : ([>`treeview],_) property =
      {name="tooltip-column"; conv=int}
  end
  module S = struct
    open GtkSignal
    let columns_changed =
      {name="columns_changed"; classe=`treeview; marshaller=marshal_unit}
    let cursor_changed =
      {name="cursor_changed"; classe=`treeview; marshaller=marshal_unit}
    let expand_collapse_cursor_row =
      {name="expand_collapse_cursor_row"; classe=`treeview;
       marshaller=fun f ->
       marshal3_ret ~ret:boolean boolean boolean boolean
         "GtkTreeView::expand_collapse_cursor_row"
         (fun x1 x2 x3 -> f ~logical:x1 ~expand:x2 ~all:x3)}
    let move_cursor =
      {name="move_cursor"; classe=`treeview; marshaller=fun f ->
       marshal2_ret ~ret:boolean GtkEnums.movement_step_conv int
         "GtkTreeView::move_cursor" f}
    let row_activated =
      {name="row_activated"; classe=`treeview; marshaller=fun f ->
       marshal2 !Internal.tree_path_copy
         (gobject : Gtk.tree_view_column obj data_conv)
         "GtkTreeView::row_activated" f}
    let row_collapsed =
      {name="row_collapsed"; classe=`treeview; marshaller=fun f ->
       marshal2 (unsafe_pointer : Gtk.tree_iter data_conv)
         !Internal.tree_path_copy "GtkTreeView::row_collapsed" f}
    let row_expanded =
      {name="row_expanded"; classe=`treeview; marshaller=fun f ->
       marshal2 (unsafe_pointer : Gtk.tree_iter data_conv)
         !Internal.tree_path_copy "GtkTreeView::row_expanded" f}
    let select_all =
      {name="select_all"; classe=`treeview;
       marshaller=fun f -> marshal0_ret ~ret:boolean f}
    let select_cursor_parent =
      {name="select_cursor_parent"; classe=`treeview;
       marshaller=fun f -> marshal0_ret ~ret:boolean f}
    let select_cursor_row =
      {name="select_cursor_row"; classe=`treeview; marshaller=fun f ->
       marshal1_ret ~ret:boolean boolean "GtkTreeView::select_cursor_row"
         (fun x1 -> f ~start_editing:x1)}
    let set_scroll_adjustments =
      {name="set_scroll_adjustments"; classe=`treeview; marshaller=fun f ->
       marshal2 (gobject_option : Gtk.adjustment obj option data_conv)
         (gobject_option : Gtk.adjustment obj option data_conv)
         "GtkTreeView::set_scroll_adjustments" f}
    let start_interactive_search =
      {name="start_interactive_search"; classe=`treeview;
       marshaller=fun f -> marshal0_ret ~ret:boolean f}
    let test_collapse_row =
      {name="test_collapse_row"; classe=`treeview; marshaller=fun f ->
       marshal2_ret ~ret:boolean (unsafe_pointer : Gtk.tree_iter data_conv)
         !Internal.tree_path_copy "GtkTreeView::test_collapse_row" f}
    let test_expand_row =
      {name="test_expand_row"; classe=`treeview; marshaller=fun f ->
       marshal2_ret ~ret:boolean (unsafe_pointer : Gtk.tree_iter data_conv)
         !Internal.tree_path_copy "GtkTreeView::test_expand_row" f}
    let toggle_cursor_row =
      {name="toggle_cursor_row"; classe=`treeview;
       marshaller=fun f -> marshal0_ret ~ret:boolean f}
    let unselect_all =
      {name="unselect_all"; classe=`treeview;
       marshaller=fun f -> marshal0_ret ~ret:boolean f}
  end
  let create pl : Gtk.tree_view obj = Object.make "GtkTreeView" pl
  external get_visible_range :
    [>`treeview] obj -> (tree_path * tree_path) option
    = "ml_gtk_tree_view_get_visible_range"
  let make_params ~cont pl ?enable_search ?fixed_height_mode ?hadjustment
      ?headers_clickable ?headers_visible ?model ?reorderable ?rules_hint
      ?search_column ?vadjustment ?tooltip_column =
    let pl = (
      may_cons P.enable_search enable_search (
      may_cons P.fixed_height_mode fixed_height_mode (
      may_cons P.hadjustment hadjustment (
      may_cons P.headers_clickable headers_clickable (
      may_cons P.headers_visible headers_visible (
      may_cons_opt P.model model (
      may_cons P.reorderable reorderable (
      may_cons P.rules_hint rules_hint (
      may_cons P.search_column search_column (
      may_cons P.vadjustment vadjustment (
      may_cons P.tooltip_column tooltip_column pl))))))))))) in
    cont pl
end

module CellRenderer = struct
  let cast w : Gtk.cell_renderer obj = try_cast w "GtkCellRenderer"
  module P = struct
    let cell_background : ([>`cellrenderer],_) property =
      {name="cell-background"; conv=string}
    let cell_background_gdk : ([>`cellrenderer],_) property =
      {name="cell-background-gdk";
       conv=(unsafe_pointer : Gdk.color data_conv)}
    let cell_background_set : ([>`cellrenderer],_) property =
      {name="cell-background-set"; conv=boolean}
    let height : ([>`cellrenderer],_) property = {name="height"; conv=int}
    let is_expanded : ([>`cellrenderer],_) property =
      {name="is-expanded"; conv=boolean}
    let is_expander : ([>`cellrenderer],_) property =
      {name="is-expander"; conv=boolean}
    let mode : ([>`cellrenderer],_) property =
      {name="mode"; conv=GtkEnums.cell_renderer_mode_conv}
    let visible : ([>`cellrenderer],_) property = PrivateProps.visible
    let width : ([>`cellrenderer],_) property = PrivateProps.width
    let xalign : ([>`cellrenderer],_) property = {name="xalign"; conv=float}
    let xpad : ([>`cellrenderer],_) property = {name="xpad"; conv=uint}
    let yalign : ([>`cellrenderer],_) property = {name="yalign"; conv=float}
    let ypad : ([>`cellrenderer],_) property = {name="ypad"; conv=uint}
  end
  let create pl : Gtk.cell_renderer obj = Object.make "GtkCellRenderer" pl
end

module CellRendererPixbuf = struct
  let cast w : Gtk.cell_renderer_pixbuf obj =
    try_cast w "GtkCellRendererPixbuf"
  module P = struct
    let pixbuf : ([>`cellrendererpixbuf],_) property =
      {name="pixbuf"; conv=(gobject : GdkPixbuf.pixbuf data_conv)}
    let pixbuf_expander_closed : ([>`cellrendererpixbuf],_) property =
      {name="pixbuf-expander-closed";
       conv=(gobject : GdkPixbuf.pixbuf data_conv)}
    let pixbuf_expander_open : ([>`cellrendererpixbuf],_) property =
      {name="pixbuf-expander-open";
       conv=(gobject : GdkPixbuf.pixbuf data_conv)}
    let stock_detail : ([>`cellrendererpixbuf],_) property =
      {name="stock-detail"; conv=string}
    let stock_id : ([>`cellrendererpixbuf],_) property =
      {name="stock-id"; conv=string}
    let stock_size : ([>`cellrendererpixbuf],_) property =
      {name="stock-size"; conv=GtkEnums.icon_size_conv}
  end
  let create pl : Gtk.cell_renderer_pixbuf obj =
    Object.make "GtkCellRendererPixbuf" pl
end

module CellRendererText = struct
  let cast w : Gtk.cell_renderer_text obj = try_cast w "GtkCellRendererText"
  module P = struct
    let background : ([>`cellrenderertext],_) property =
      {name="background"; conv=string}
    let background_gdk : ([>`cellrenderertext],_) property =
      {name="background-gdk"; conv=(unsafe_pointer : Gdk.color data_conv)}
    let background_set : ([>`cellrenderertext],_) property =
      {name="background-set"; conv=boolean}
    let editable : ([>`cellrenderertext],_) property =
      {name="editable"; conv=boolean}
    let editable_set : ([>`cellrenderertext],_) property =
      {name="editable-set"; conv=boolean}
    let family : ([>`cellrenderertext],_) property =
      {name="family"; conv=string}
    let family_set : ([>`cellrenderertext],_) property =
      {name="family-set"; conv=boolean}
    let font : ([>`cellrenderertext],_) property = {name="font"; conv=string}
    let font_desc : ([>`cellrenderertext],_) property =
      {name="font-desc";
       conv=(unsafe_pointer : Pango.font_description data_conv)}
    let foreground : ([>`cellrenderertext],_) property =
      {name="foreground"; conv=string}
    let foreground_gdk : ([>`cellrenderertext],_) property =
      {name="foreground-gdk"; conv=(unsafe_pointer : Gdk.color data_conv)}
    let foreground_set : ([>`cellrenderertext],_) property =
      {name="foreground-set"; conv=boolean}
    let markup : ([>`cellrenderertext],_) property =
      {name="markup"; conv=string}
    let rise : ([>`cellrenderertext],_) property = {name="rise"; conv=int}
    let rise_set : ([>`cellrenderertext],_) property =
      {name="rise-set"; conv=boolean}
    let scale : ([>`cellrenderertext],_) property =
      {name="scale"; conv=double}
    let scale_set : ([>`cellrenderertext],_) property =
      {name="scale-set"; conv=boolean}
    let single_paragraph_mode : ([>`cellrenderertext],_) property =
      {name="single-paragraph-mode"; conv=boolean}
    let size : ([>`cellrenderertext],_) property = {name="size"; conv=int}
    let size_points : ([>`cellrenderertext],_) property =
      {name="size-points"; conv=double}
    let size_set : ([>`cellrenderertext],_) property =
      {name="size-set"; conv=boolean}
    let stretch : ([>`cellrenderertext],_) property =
      {name="stretch"; conv=PangoEnums.stretch_conv}
    let stretch_set : ([>`cellrenderertext],_) property =
      {name="stretch-set"; conv=boolean}
    let strikethrough : ([>`cellrenderertext],_) property =
      {name="strikethrough"; conv=boolean}
    let strikethrough_set : ([>`cellrenderertext],_) property =
      {name="strikethrough-set"; conv=boolean}
    let style : ([>`cellrenderertext],_) property =
      {name="style"; conv=PangoEnums.style_conv}
    let style_set : ([>`cellrenderertext],_) property =
      {name="style-set"; conv=boolean}
    let text : ([>`cellrenderertext],_) property = {name="text"; conv=string}
    let underline : ([>`cellrenderertext],_) property =
      {name="underline"; conv=PangoEnums.underline_conv}
    let underline_set : ([>`cellrenderertext],_) property =
      {name="underline-set"; conv=boolean}
    let variant : ([>`cellrenderertext],_) property =
      {name="variant"; conv=PangoEnums.variant_conv}
    let variant_set : ([>`cellrenderertext],_) property =
      {name="variant-set"; conv=boolean}
    let weight : ([>`cellrenderertext],_) property =
      {name="weight"; conv=int}
    let weight_set : ([>`cellrenderertext],_) property =
      {name="weight-set"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let edited =
      {name="edited"; classe=`cellrenderertext; marshaller=fun f ->
       marshal2 !Internal.tree_path_string string
         "GtkCellRendererText::edited" f}
  end
  let create pl : Gtk.cell_renderer_text obj =
    Object.make "GtkCellRendererText" pl
  external set_fixed_height_from_font :
    [>`cellrenderertext] obj -> int -> unit
    = "ml_gtk_cell_renderer_text_set_fixed_height_from_font"
end

module CellRendererToggle = struct
  let cast w : Gtk.cell_renderer_toggle obj =
    try_cast w "GtkCellRendererToggle"
  module P = struct
    let activatable : ([>`cellrenderertoggle],_) property =
      {name="activatable"; conv=boolean}
    let active : ([>`cellrenderertoggle],_) property =
      {name="active"; conv=boolean}
    let inconsistent : ([>`cellrenderertoggle],_) property =
      {name="inconsistent"; conv=boolean}
    let radio : ([>`cellrenderertoggle],_) property =
      {name="radio"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let toggled =
      {name="toggled"; classe=`cellrenderertoggle; marshaller=fun f ->
       marshal1 !Internal.tree_path_string "GtkCellRendererToggle::toggled" f}
  end
  let create pl : Gtk.cell_renderer_toggle obj =
    Object.make "GtkCellRendererToggle" pl
end

module CellRendererProgress = struct
  let cast w : Gtk.cell_renderer_progress obj =
    try_cast w "GtkCellRendererProgress"
  module P = struct
    let value : ([>`cellrendererprogress],_) property =
      {name="value"; conv=int}
    let text : ([>`cellrendererprogress],_) property =
      {name="text"; conv=string_option}
  end
  let create pl : Gtk.cell_renderer_progress obj =
    Object.make "GtkCellRendererProgress" pl
end

module CellRendererCombo = struct
  let cast w : Gtk.cell_renderer_combo obj =
    try_cast w "GtkCellRendererCombo"
  module P = struct
    let model : ([>`cellrenderercombo],_) property = PrivateProps.model
    let text_column : ([>`cellrenderercombo],_) property =
      {name="text_column"; conv=int}
    let has_entry : ([>`cellrenderercombo],_) property =
      {name="has_entry"; conv=boolean}
  end
  module S = struct
    open GtkSignal
    let changed =
      {name="changed"; classe=`cellrenderercombo; marshaller=fun f ->
       marshal2 !Internal.tree_path_string
         (unsafe_pointer : Gtk.tree_iter data_conv)
         "GtkCellRendererCombo::changed" f}
  end
  let create pl : Gtk.cell_renderer_combo obj =
    Object.make "GtkCellRendererCombo" pl
end

module CellRendererAccel = struct
  let cast w : Gtk.cell_renderer_accel obj =
    try_cast w "GtkCellRendererAccel"
  module P = struct
    let accel_key : ([>`cellrendereraccel],_) property =
      {name="accel-key"; conv=int}
    let accel_mode : ([>`cellrendereraccel],_) property =
      {name="accel-mode"; conv=GtkEnums.cell_renderer_accel_mode_conv}
    let accel_mods : ([>`cellrendereraccel],_) property =
      {name="accel-mods"; conv=int}
    let keycode : ([>`cellrendereraccel],_) property =
      {name="keycode"; conv=int}
  end
  module S = struct
    open GtkSignal
    let accel_cleared =
      {name="accel_cleared"; classe=`cellrendereraccel; marshaller=fun f ->
       marshal1 !Internal.tree_path_string
         "GtkCellRendererAccel::accel_cleared" f}
    let accel_edited =
      {name="accel_edited"; classe=`cellrendereraccel; marshaller=fun f ->
       marshal4 !Internal.tree_path_string int int int
         "GtkCellRendererAccel::accel_edited"
         (fun x1 x2 x3 x4 -> f x1 ~accel_key:x2 ~accel_mods:x3
           ~hardware_keycode:x4)}
  end
  let create pl : Gtk.cell_renderer_accel obj =
    Object.make "GtkCellRendererAccel" pl
end

module CellLayout = struct
  let cast w : Gtk.cell_layout obj = try_cast w "GtkCellLayout"
  external pack_start :
    [>`celllayout] obj -> Gtk.cell_renderer Gtk.obj -> expand:bool -> unit
    = "ml_gtk_cell_layout_pack_start"
  external pack_end :
    [>`celllayout] obj -> Gtk.cell_renderer Gtk.obj -> expand:bool -> unit
    = "ml_gtk_cell_layout_pack_end"
  external reorder :
    [>`celllayout] obj -> Gtk.cell_renderer Gtk.obj -> int -> unit
    = "ml_gtk_cell_layout_reorder"
  external clear : [>`celllayout] obj -> unit = "ml_gtk_cell_layout_clear"
  external add_attribute :
    [>`celllayout] obj -> Gtk.cell_renderer Gtk.obj -> string -> int -> unit
    = "ml_gtk_cell_layout_add_attribute"
  external clear_attributes :
    [>`celllayout] obj -> Gtk.cell_renderer Gtk.obj -> unit
    = "ml_gtk_cell_layout_clear_attributes"
  external set_cell_data_func :
    [>`celllayout] obj ->
    Gtk.cell_renderer Gtk.obj -> ([`treemodel] obj -> tree_iter -> unit) option -> unit
    = "ml_gtk_cell_layout_set_cell_data_func"
end

module TreeViewColumn = struct
  let cast w : Gtk.tree_view_column obj = try_cast w "GtkTreeViewColumn"
  module P = struct
    let alignment : ([>`treeviewcolumn],_) property =
      {name="alignment"; conv=float}
    let clickable : ([>`treeviewcolumn],_) property =
      {name="clickable"; conv=boolean}
    let fixed_width : ([>`treeviewcolumn],_) property =
      {name="fixed-width"; conv=int}
    let max_width : ([>`treeviewcolumn],_) property =
      {name="max-width"; conv=int}
    let min_width : ([>`treeviewcolumn],_) property =
      {name="min-width"; conv=int}
    let reorderable : ([>`treeviewcolumn],_) property = PrivateProps.reorderable
    let resizable : ([>`treeviewcolumn],_) property =
      {name="resizable"; conv=boolean}
    let sizing : ([>`treeviewcolumn],_) property =
      {name="sizing"; conv=GtkEnums.tree_view_column_sizing_conv}
    let sort_indicator : ([>`treeviewcolumn],_) property =
      {name="sort-indicator"; conv=boolean}
    let sort_order : ([>`treeviewcolumn],_) property =
      {name="sort-order"; conv=GtkEnums.sort_type_conv}
    let title : ([>`treeviewcolumn],_) property = {name="title"; conv=string}
    let visible : ([>`treeviewcolumn],_) property = PrivateProps.visible
    let widget : ([>`treeviewcolumn],_) property =
      {name="widget";
       conv=(gobject_option : Gtk.widget obj option data_conv)}
    let width : ([>`treeviewcolumn],_) property = PrivateProps.width
  end
  module S = struct
    open GtkSignal
    let clicked =
      {name="clicked"; classe=`treeviewcolumn; marshaller=marshal_unit}
  end
  let create pl : Gtk.tree_view_column obj =
    Object.make "GtkTreeViewColumn" pl
end

module TreeSelection = struct
  let cast w : tree_selection = try_cast w "GtkTreeSelection"
  module S = struct
    open GtkSignal
    let changed =
      {name="changed"; classe=`treeselection; marshaller=marshal_unit}
  end
end

module TreeModel = struct
  let cast w : tree_model = try_cast w "GtkTreeModel"
  module S = struct
    open GtkSignal
    let row_changed =
      {name="row_changed"; classe=`treemodel; marshaller=fun f ->
       marshal2 !Internal.tree_path_copy
         (unsafe_pointer : Gtk.tree_iter data_conv)
         "GtkTreeModel::row_changed" f}
    let row_deleted =
      {name="row_deleted"; classe=`treemodel; marshaller=fun f ->
       marshal1 !Internal.tree_path_copy "GtkTreeModel::row_deleted" f}
    let row_has_child_toggled =
      {name="row_has_child_toggled"; classe=`treemodel; marshaller=fun f ->
       marshal2 !Internal.tree_path_copy
         (unsafe_pointer : Gtk.tree_iter data_conv)
         "GtkTreeModel::row_has_child_toggled" f}
    let row_inserted =
      {name="row_inserted"; classe=`treemodel; marshaller=fun f ->
       marshal2 !Internal.tree_path_copy
         (unsafe_pointer : Gtk.tree_iter data_conv)
         "GtkTreeModel::row_inserted" f}
    let rows_reordered =
      {name="rows_reordered"; classe=`treemodel; marshaller=fun f ->
       marshal2 !Internal.tree_path_copy
         (unsafe_pointer : Gtk.tree_iter data_conv)
         "GtkTreeModel::rows_reordered" f}
  end
end

module TreeModelSort = struct
  let cast w : tree_model_sort = try_cast w "GtkTreeModelSort"
  module P = struct
    let model : ([>`treemodelsort],_) property =
      {name="model"; conv=(gobject : tree_model data_conv)}
  end
  let create ?model pl : tree_model_sort =
    let pl = (may_cons P.model model pl) in
    Gobject.unsafe_create "GtkTreeModelSort" pl
  external convert_child_path_to_path :
    [>`treemodelsort] obj -> Gtk.tree_path -> Gtk.tree_path
    = "ml_gtk_tree_model_sort_convert_child_path_to_path"
  external convert_child_iter_to_iter :
    [>`treemodelsort] obj -> Gtk.tree_iter -> Gtk.tree_iter
    = "ml_gtk_tree_model_sort_convert_child_iter_to_iter"
  external convert_path_to_child_path :
    [>`treemodelsort] obj -> Gtk.tree_path -> Gtk.tree_path
    = "ml_gtk_tree_model_sort_convert_path_to_child_path"
  external convert_iter_to_child_iter :
    [>`treemodelsort] obj -> Gtk.tree_iter -> Gtk.tree_iter
    = "ml_gtk_tree_model_sort_convert_iter_to_child_iter"
  external reset_default_sort_func : [>`treemodelsort] obj -> unit
    = "ml_gtk_tree_model_sort_reset_default_sort_func"
  external iter_is_valid : [>`treemodelsort] obj -> Gtk.tree_iter -> bool
    = "ml_gtk_tree_model_sort_iter_is_valid"
end

module TreeSortable = struct
  let cast w : tree_sortable = try_cast w "GtkTreeSortable"
  module S = struct
    open GtkSignal
    let sort_column_changed =
      {name="sort_column_changed"; classe=`treesortable;
       marshaller=marshal_unit}
  end
  external sort_column_changed : [>`treesortable] obj -> unit
    = "ml_gtk_tree_sortable_sort_column_changed"
  external get_sort_column_id :
    [>`treesortable] obj -> (int * Gtk.Tags.sort_type) option
    = "ml_gtk_tree_sortable_get_sort_column_id"
  external set_sort_column_id :
    [>`treesortable] obj -> int -> Gtk.Tags.sort_type -> unit
    = "ml_gtk_tree_sortable_set_sort_column_id"
  external set_sort_func :
    [>`treesortable] obj ->
    int -> ([`treemodel] Gobject.obj -> Gtk.tree_iter -> Gtk.tree_iter -> int) -> unit
    = "ml_gtk_tree_sortable_set_sort_func"
  external set_default_sort_func :
    [>`treesortable] obj ->
    ([`treemodel] Gobject.obj -> Gtk.tree_iter -> Gtk.tree_iter -> int) -> unit
    = "ml_gtk_tree_sortable_set_default_sort_func"
  external has_default_sort_func : [>`treesortable] obj -> bool
    = "ml_gtk_tree_sortable_has_default_sort_func"
end

module TreeModelFilter = struct
  let cast w : tree_model_filter = try_cast w "GtkTreeModelFilter"
  module P = struct
    let child_model : ([>`treemodelfilter],_) property =
      {name="child-model"; conv=(gobject : tree_model data_conv)}
    let virtual_root : ([>`treemodelfilter],_) property =
      {name="virtual-root"; conv=(unsafe_pointer : Gtk.tree_path data_conv)}
  end
  let create ?child_model ?virtual_root pl : tree_model_filter =
    let pl = (
      may_cons P.child_model child_model (
      may_cons P.virtual_root virtual_root pl)) in
    Gobject.unsafe_create "GtkTreeModelFilter" pl
  external set_visible_func :
    [>`treemodelfilter] obj ->
    ([`treemodel] Gobject.obj -> Gtk.tree_iter -> bool) -> unit
    = "ml_gtk_tree_model_filter_set_visible_func"
  external set_visible_column : [>`treemodelfilter] obj -> int -> unit
    = "ml_gtk_tree_model_filter_set_visible_column"
  external convert_child_path_to_path :
    [>`treemodelfilter] obj -> Gtk.tree_path -> Gtk.tree_path
    = "ml_gtk_tree_model_filter_convert_child_path_to_path"
  external convert_child_iter_to_iter :
    [>`treemodelfilter] obj -> Gtk.tree_iter -> Gtk.tree_iter
    = "ml_gtk_tree_model_filter_convert_child_iter_to_iter"
  external convert_path_to_child_path :
    [>`treemodelfilter] obj -> Gtk.tree_path -> Gtk.tree_path
    = "ml_gtk_tree_model_filter_convert_path_to_child_path"
  external convert_iter_to_child_iter :
    [>`treemodelfilter] obj -> Gtk.tree_iter -> Gtk.tree_iter
    = "ml_gtk_tree_model_filter_convert_iter_to_child_iter"
  external refilter : [>`treemodelfilter] obj -> unit
    = "ml_gtk_tree_model_filter_refilter"
end

module IconView = struct
  let cast w : Gtk.icon_view obj = try_cast w "GtkIconView"
  module P = struct
    let column_spacing : ([>`iconview],_) property =
      {name="column-spacing"; conv=int}
    let columns : ([>`iconview],_) property = {name="columns"; conv=int}
    let item_width : ([>`iconview],_) property =
      {name="item-width"; conv=int}
    let margin : ([>`iconview],_) property = {name="margin"; conv=int}
    let markup_column : ([>`iconview],_) property =
      {name="markup-column"; conv=int}
    let model : ([>`iconview],_) property = PrivateProps.model
    let orientation : ([>`iconview],_) property =
      {name="orientation"; conv=GtkEnums.orientation_conv}
    let pixbuf_column : ([>`iconview],_) property =
      {name="pixbuf-column"; conv=int}
    let row_spacing : ([>`iconview],_) property =
      {name="row-spacing"; conv=int}
    let selection_mode : ([>`iconview],_) property =
      {name="selection-mode"; conv=GtkEnums.selection_mode_conv}
    let spacing : ([>`iconview],_) property = {name="spacing"; conv=int}
    let text_column : ([>`iconview],_) property =
      {name="text-column"; conv=int}
  end
  module S = struct
    open GtkSignal
    let item_activated =
      {name="item_activated"; classe=`iconview; marshaller=fun f ->
       marshal1 (unsafe_pointer : Gtk.tree_path data_conv)
         "GtkIconView::item_activated" f}
    let selection_changed =
      {name="selection_changed"; classe=`iconview; marshaller=marshal_unit}
  end
  let create pl : Gtk.icon_view obj = Object.make "GtkIconView" pl
  external get_path_at_pos : [>`iconview] obj -> int -> int -> Gtk.tree_path
    = "ml_gtk_icon_view_get_path_at_pos"
  external selected_foreach :
    [>`iconview] obj -> (Gtk.tree_path -> unit) -> unit
    = "ml_gtk_icon_view_selected_foreach"
  external select_path : [>`iconview] obj -> Gtk.tree_path -> unit
    = "ml_gtk_icon_view_select_path"
  external unselect_path : [>`iconview] obj -> Gtk.tree_path -> unit
    = "ml_gtk_icon_view_unselect_path"
  external path_is_selected : [>`iconview] obj -> Gtk.tree_path -> bool
    = "ml_gtk_icon_view_path_is_selected"
  external get_selected_items : [>`iconview] obj -> Gtk.tree_path list
    = "ml_gtk_icon_view_get_selected_items"
  external select_all : [>`iconview] obj -> unit
    = "ml_gtk_icon_view_select_all"
  external unselect_all : [>`iconview] obj -> unit
    = "ml_gtk_icon_view_unselect_all"
  external item_activated : [>`iconview] obj -> Gtk.tree_path -> unit
    = "ml_gtk_icon_view_item_activated"
  let make_params ~cont pl ?columns ?model ?orientation ?selection_mode =
    let pl = (
      may_cons P.columns columns (
      may_cons_opt P.model model (
      may_cons P.orientation orientation (
      may_cons P.selection_mode selection_mode pl)))) in
    cont pl
end

