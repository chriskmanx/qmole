(** gtk enums *)

open Gpointer

type anchor_type =
  [ `CENTER | `NORTH | `NW | `NE | `SOUTH | `SW | `SE | `WEST | `EAST ]
type arrow_type = [ `UP | `DOWN | `LEFT | `RIGHT ]
type attach_options = [ `EXPAND | `SHRINK | `FILL ]
type button_box_style = [ `DEFAULT_STYLE | `SPREAD | `EDGE | `START | `END ]
type curve_type = [ `LINEAR | `SPLINE | `FREE ]
type delete_type =
  [ `CHARS | `WORD_ENDS | `WORDS | `DISPLAY_LINES | `DISPLAY_LINE_ENDS
  | `PARAGRAPH_ENDS | `PARAGRAPHS | `WHITESPACE ]
type direction_type =
  [ `TAB_FORWARD | `TAB_BACKWARD | `UP | `DOWN | `LEFT | `RIGHT ]
type expander_style =
  [ `COLLAPSED | `SEMI_COLLAPSED | `SEMI_EXPANDED | `EXPANDED ]
type icon_size =
  [ `INVALID | `MENU | `SMALL_TOOLBAR | `LARGE_TOOLBAR | `BUTTON | `DND
  | `DIALOG ]
type side_type = [ `TOP | `BOTTOM | `LEFT | `RIGHT ]
type text_direction = [ `NONE | `LTR | `RTL ]
type justification = [ `LEFT | `RIGHT | `CENTER | `FILL ]
type match_type = [ `ALL | `ALL_TAIL | `HEAD | `TAIL | `EXACT | `LAST ]
type menu_direction_type = [ `PARENT | `CHILD | `NEXT | `PREV ]
type metric_type = [ `PIXELS | `INCHES | `CENTIMETERS ]
type movement_step =
  [ `LOGICAL_POSITIONS | `VISUAL_POSITIONS | `WORDS | `DISPLAY_LINES
  | `DISPLAY_LINE_ENDS | `PARAGRAPH_ENDS | `PARAGRAPHS | `PAGES
  | `BUFFER_ENDS ]
type orientation = [ `HORIZONTAL | `VERTICAL ]
type corner_type = [ `TOP_LEFT | `BOTTOM_LEFT | `TOP_RIGHT | `BOTTOM_RIGHT ]
type pack_type = [ `START | `END ]
type path_priority =
  [ `LOWEST | `GTK | `APPLICATION | `THEME | `RC | `HIGHEST ]
type path_type = [ `WIDGET | `WIDGET_CLASS | `CLASS ]
type policy_type = [ `ALWAYS | `AUTOMATIC | `NEVER ]
type position_type = [ `LEFT | `RIGHT | `TOP | `BOTTOM ]
type preview_type = [ `COLOR | `GRAYSCALE ]
type relief_style = [ `NORMAL | `HALF | `NONE ]
type resize_mode = [ `PARENT | `QUEUE | `IMMEDIATE ]
type signal_run_type =
  [ `FIRST | `LAST | `BOTH | `NO_RECURSE | `ACTION | `NO_HOOKS ]
type scroll_type =
  [ `NONE | `JUMP | `STEP_FORWARD | `STEP_BACKWARD | `PAGE_BACKWARD
  | `PAGE_FORWARD | `STEP_UP | `STEP_DOWN | `PAGE_UP | `PAGE_DOWN
  | `STEP_LEFT | `STEP_RIGHT | `PAGE_LEFT | `PAGE_RIGHT | `START | `END ]
type selection_mode = [ `NONE | `SINGLE | `BROWSE | `MULTIPLE ]
type shadow_type = [ `NONE | `IN | `OUT | `ETCHED_IN | `ETCHED_OUT ]
type state_type =
  [ `NORMAL | `ACTIVE | `PRELIGHT | `SELECTED | `INSENSITIVE ]
type submenu_direction = [ `LEFT | `RIGHT ]
type submenu_placement = [ `TOP_BOTTOM | `LEFT_RIGHT ]
type toolbar_style = [ `ICONS | `TEXT | `BOTH | `BOTH_HORIZ ]
type update_type = [ `CONTINUOUS | `DISCONTINUOUS | `DELAYED ]
type visibility = [ `NONE | `PARTIAL | `FULL ]
type window_position =
  [ `NONE | `CENTER | `MOUSE | `CENTER_ALWAYS | `CENTER_ON_PARENT ]
type window_type = [ `TOPLEVEL | `POPUP ]
type wrap_mode = [ `NONE | `CHAR | `WORD ]
type sort_type = [ `ASCENDING | `DESCENDING ]
type cell_type = [ `EMPTY | `TEXT | `PIXMAP | `PIXTEXT | `WIDGET ]
type toolbar_child =
  [ `SPACE | `BUTTON | `TOGGLEBUTTON | `RADIOBUTTON | `WIDGET ]
type toolbar_space_style = [ `EMPTY | `LINE ]
type spin_type =
  [ `STEP_FORWARD | `STEP_BACKWARD | `PAGE_FORWARD | `PAGE_BACKWARD | `HOME
  | `END | `USER_DEFINED ]
type accel_flag = [ `VISIBLE | `LOCKED ]
type button_action = [ `SELECTS | `DRAGS | `EXPANDS ]
type calendar_display_options =
  [ `SHOW_HEADING | `SHOW_DAY_NAMES | `NO_MONTH_CHANGE | `SHOW_WEEK_NUMBERS
  | `WEEK_START_MONDAY ]
type progress_bar_style = [ `CONTINUOUS | `DISCRETE ]
type progress_bar_orientation =
  [ `LEFT_TO_RIGHT | `RIGHT_TO_LEFT | `BOTTOM_TO_TOP | `TOP_TO_BOTTOM ]
type dest_defaults = [ `MOTION | `HIGHLIGHT | `DROP | `ALL ]
type target_flags = [ `SAME_APP | `SAME_WIDGET ]
type spin_button_update_policy = [ `ALWAYS | `IF_VALID ]
type text_window_type =
  [ `PRIVATE | `WIDGET | `TEXT | `LEFT | `RIGHT | `TOP | `BOTTOM ]
type text_search_flag = [ `VISIBLE_ONLY | `TEXT_ONLY ]
type tree_view_column_sizing = [ `GROW_ONLY | `AUTOSIZE | `FIXED ]
type cell_renderer_mode = [ `INERT | `ACTIVATABLE | `EDITABLE ]
type message_type = [ `INFO | `WARNING | `QUESTION | `ERROR ]
type buttons_type = [ `NONE | `OK | `CLOSE | `CANCEL | `YES_NO | `OK_CANCEL ]
type response =
  [ `NONE | `REJECT | `ACCEPT | `DELETE_EVENT | `OK | `CANCEL | `CLOSE | `YES
  | `NO | `APPLY | `HELP ]
type widget_flags =
  [ `IN_DESTRUCTION | `FLOATING | `TOPLEVEL | `NO_WINDOW | `REALIZED
  | `MAPPED | `VISIBLE | `SENSITIVE | `PARENT_SENSITIVE | `CAN_FOCUS
  | `HAS_FOCUS | `CAN_DEFAULT | `HAS_DEFAULT | `HAS_GRAB | `RC_STYLE
  | `COMPOSITE_CHILD | `NO_REPARENT | `APP_PAINTABLE | `RECEIVES_DEFAULT
  | `DOUBLE_BUFFERED ]
type image_type =
  [ `EMPTY | `PIXMAP | `IMAGE | `PIXBUF | `STOCK | `ICON_SET | `ANIMATION ]
type size_group_mode = [ `NONE | `HORIZONTAL | `VERTICAL | `BOTH ]
type file_chooser_action =
  [ `OPEN | `SAVE | `SELECT_FOLDER | `CREATE_FOLDER ]
type tree_model_flags = [ `ITERS_PERSIST | `LIST_ONLY ]
type tree_view_drop_position =
  [ `BEFORE | `AFTER | `INTO_OR_BEFORE | `INTO_OR_AFTER ]
type file_filter_flags = [ `FILENAME | `URI | `DISPLAY_NAME | `MIME_TYPE ]
type ui_manager_item_type =
  [ `AUTO | `MENUBAR | `MENU | `TOOLBAR | `PLACEHOLDER | `POPUP | `MENUITEM
  | `TOOLITEM | `SEPARATOR | `ACCELERATOR ]
type assistant_page_type =
  [ `CONTENT | `INTRO | `CONFIRM | `SUMMARY | `PROGRESS ]
type cell_renderer_accel_mode = [ `GTK | `OTHER ]
type file_chooser_confirmation =
  [ `CONFIRM | `ACCEPT_FILENAME | `SELECT_AGAIN ]

(**/**)

external _get_tables : unit ->
    anchor_type variant_table
  * arrow_type variant_table
  * attach_options variant_table
  * button_box_style variant_table
  * curve_type variant_table
  * delete_type variant_table
  * direction_type variant_table
  * expander_style variant_table
  * icon_size variant_table
  * side_type variant_table
  * text_direction variant_table
  * justification variant_table
  * match_type variant_table
  * menu_direction_type variant_table
  * metric_type variant_table
  * movement_step variant_table
  * orientation variant_table
  * corner_type variant_table
  * pack_type variant_table
  * path_priority variant_table
  * path_type variant_table
  * policy_type variant_table
  * position_type variant_table
  * preview_type variant_table
  * relief_style variant_table
  * resize_mode variant_table
  * signal_run_type variant_table
  * scroll_type variant_table
  * selection_mode variant_table
  * shadow_type variant_table
  * state_type variant_table
  * submenu_direction variant_table
  * submenu_placement variant_table
  * toolbar_style variant_table
  * update_type variant_table
  * visibility variant_table
  * window_position variant_table
  * window_type variant_table
  * wrap_mode variant_table
  * sort_type variant_table
  * cell_type variant_table
  * toolbar_child variant_table
  * toolbar_space_style variant_table
  * spin_type variant_table
  * accel_flag variant_table
  * button_action variant_table
  * calendar_display_options variant_table
  * progress_bar_style variant_table
  * progress_bar_orientation variant_table
  * dest_defaults variant_table
  * target_flags variant_table
  * spin_button_update_policy variant_table
  * text_window_type variant_table
  * text_search_flag variant_table
  * tree_view_column_sizing variant_table
  * cell_renderer_mode variant_table
  * message_type variant_table
  * buttons_type variant_table
  * response variant_table
  * widget_flags variant_table
  * image_type variant_table
  * size_group_mode variant_table
  * file_chooser_action variant_table
  * tree_model_flags variant_table
  * tree_view_drop_position variant_table
  * file_filter_flags variant_table
  * ui_manager_item_type variant_table
  * assistant_page_type variant_table
  * cell_renderer_accel_mode variant_table
  * file_chooser_confirmation variant_table
  = "ml_gtk_get_tables"


let anchor_type, arrow_type, attach_options, button_box_style, curve_type,
    delete_type, direction_type, expander_style, icon_size, side_type,
    text_direction, justification, match_type, menu_direction_type,
    metric_type, movement_step, orientation, corner_type, pack_type,
    path_priority, path_type, policy_type, position_type, preview_type,
    relief_style, resize_mode, signal_run_type, scroll_type, selection_mode,
    shadow_type, state_type, submenu_direction, submenu_placement,
    toolbar_style, update_type, visibility, window_position, window_type,
    wrap_mode, sort_type, cell_type, toolbar_child, toolbar_space_style,
    spin_type, accel_flag, button_action, calendar_display_options,
    progress_bar_style, progress_bar_orientation, dest_defaults,
    target_flags, spin_button_update_policy, text_window_type,
    text_search_flag, tree_view_column_sizing, cell_renderer_mode,
    message_type, buttons_type, response, widget_flags, image_type,
    size_group_mode, file_chooser_action, tree_model_flags,
    tree_view_drop_position, file_filter_flags, ui_manager_item_type,
    assistant_page_type, cell_renderer_accel_mode,
    file_chooser_confirmation = _get_tables ()

let _make_enum = Gobject.Data.enum
let anchor_type_conv = _make_enum anchor_type
let arrow_type_conv = _make_enum arrow_type
let attach_options_conv = _make_enum attach_options
let button_box_style_conv = _make_enum button_box_style
let curve_type_conv = _make_enum curve_type
let delete_type_conv = _make_enum delete_type
let direction_type_conv = _make_enum direction_type
let expander_style_conv = _make_enum expander_style
let icon_size_conv = _make_enum icon_size
let side_type_conv = _make_enum side_type
let text_direction_conv = _make_enum text_direction
let justification_conv = _make_enum justification
let match_type_conv = _make_enum match_type
let menu_direction_type_conv = _make_enum menu_direction_type
let metric_type_conv = _make_enum metric_type
let movement_step_conv = _make_enum movement_step
let orientation_conv = _make_enum orientation
let corner_type_conv = _make_enum corner_type
let pack_type_conv = _make_enum pack_type
let path_priority_conv = _make_enum path_priority
let path_type_conv = _make_enum path_type
let policy_type_conv = _make_enum policy_type
let position_type_conv = _make_enum position_type
let preview_type_conv = _make_enum preview_type
let relief_style_conv = _make_enum relief_style
let resize_mode_conv = _make_enum resize_mode
let signal_run_type_conv = _make_enum signal_run_type
let scroll_type_conv = _make_enum scroll_type
let selection_mode_conv = _make_enum selection_mode
let shadow_type_conv = _make_enum shadow_type
let state_type_conv = _make_enum state_type
let submenu_direction_conv = _make_enum submenu_direction
let submenu_placement_conv = _make_enum submenu_placement
let toolbar_style_conv = _make_enum toolbar_style
let update_type_conv = _make_enum update_type
let visibility_conv = _make_enum visibility
let window_position_conv = _make_enum window_position
let window_type_conv = _make_enum window_type
let wrap_mode_conv = _make_enum wrap_mode
let sort_type_conv = _make_enum sort_type
let cell_type_conv = _make_enum cell_type
let toolbar_child_conv = _make_enum toolbar_child
let toolbar_space_style_conv = _make_enum toolbar_space_style
let spin_type_conv = _make_enum spin_type
let accel_flag_conv = _make_enum accel_flag
let button_action_conv = _make_enum button_action
let calendar_display_options_conv = _make_enum calendar_display_options
let progress_bar_style_conv = _make_enum progress_bar_style
let progress_bar_orientation_conv = _make_enum progress_bar_orientation
let dest_defaults_conv = _make_enum dest_defaults
let target_flags_conv = _make_enum target_flags
let spin_button_update_policy_conv = _make_enum spin_button_update_policy
let text_window_type_conv = _make_enum text_window_type
let text_search_flag_conv = _make_enum text_search_flag
let tree_view_column_sizing_conv = _make_enum tree_view_column_sizing
let cell_renderer_mode_conv = _make_enum cell_renderer_mode
let message_type_conv = _make_enum message_type
let buttons_type_conv = _make_enum buttons_type
let response_conv = _make_enum response
let widget_flags_conv = _make_enum widget_flags
let image_type_conv = _make_enum image_type
let size_group_mode_conv = _make_enum size_group_mode
let file_chooser_action_conv = _make_enum file_chooser_action
let tree_model_flags_conv = _make_enum tree_model_flags
let tree_view_drop_position_conv = _make_enum tree_view_drop_position
let file_filter_flags_conv = _make_enum file_filter_flags
let ui_manager_item_type_conv = _make_enum ui_manager_item_type
let assistant_page_type_conv = _make_enum assistant_page_type
let cell_renderer_accel_mode_conv = _make_enum cell_renderer_accel_mode
let file_chooser_confirmation_conv = _make_enum file_chooser_confirmation
