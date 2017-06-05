/* anchor_type : conversion table */
const lookup_info ml_table_anchor_type[] = {
  { 0, 9 },
  { MLTAG_SOUTH, GTK_ANCHOR_SOUTH },
  { MLTAG_NE, GTK_ANCHOR_NE },
  { MLTAG_NW, GTK_ANCHOR_NW },
  { MLTAG_SE, GTK_ANCHOR_SE },
  { MLTAG_SW, GTK_ANCHOR_SW },
  { MLTAG_NORTH, GTK_ANCHOR_NORTH },
  { MLTAG_EAST, GTK_ANCHOR_EAST },
  { MLTAG_CENTER, GTK_ANCHOR_CENTER },
  { MLTAG_WEST, GTK_ANCHOR_WEST },
};

/* arrow_type : conversion table */
const lookup_info ml_table_arrow_type[] = {
  { 0, 4 },
  { MLTAG_RIGHT, GTK_ARROW_RIGHT },
  { MLTAG_UP, GTK_ARROW_UP },
  { MLTAG_DOWN, GTK_ARROW_DOWN },
  { MLTAG_LEFT, GTK_ARROW_LEFT },
};

/* attach_options : conversion table */
const lookup_info ml_table_attach_options[] = {
  { 0, 3 },
  { MLTAG_SHRINK, GTK_SHRINK },
  { MLTAG_EXPAND, GTK_EXPAND },
  { MLTAG_FILL, GTK_FILL },
};

/* button_box_style : conversion table */
const lookup_info ml_table_button_box_style[] = {
  { 0, 5 },
  { MLTAG_SPREAD, GTK_BUTTONBOX_SPREAD },
  { MLTAG_DEFAULT_STYLE, GTK_BUTTONBOX_DEFAULT_STYLE },
  { MLTAG_END, GTK_BUTTONBOX_END },
  { MLTAG_START, GTK_BUTTONBOX_START },
  { MLTAG_EDGE, GTK_BUTTONBOX_EDGE },
};

/* curve_type : conversion table */
const lookup_info ml_table_curve_type[] = {
  { 0, 3 },
  { MLTAG_SPLINE, GTK_CURVE_TYPE_SPLINE },
  { MLTAG_LINEAR, GTK_CURVE_TYPE_LINEAR },
  { MLTAG_FREE, GTK_CURVE_TYPE_FREE },
};

/* delete_type : conversion table */
const lookup_info ml_table_delete_type[] = {
  { 0, 8 },
  { MLTAG_CHARS, GTK_DELETE_CHARS },
  { MLTAG_WORDS, GTK_DELETE_WORDS },
  { MLTAG_PARAGRAPHS, GTK_DELETE_PARAGRAPHS },
  { MLTAG_DISPLAY_LINE_ENDS, GTK_DELETE_DISPLAY_LINE_ENDS },
  { MLTAG_PARAGRAPH_ENDS, GTK_DELETE_PARAGRAPH_ENDS },
  { MLTAG_DISPLAY_LINES, GTK_DELETE_DISPLAY_LINES },
  { MLTAG_WHITESPACE, GTK_DELETE_WHITESPACE },
  { MLTAG_WORD_ENDS, GTK_DELETE_WORD_ENDS },
};

/* direction_type : conversion table */
const lookup_info ml_table_direction_type[] = {
  { 0, 6 },
  { MLTAG_RIGHT, GTK_DIR_RIGHT },
  { MLTAG_UP, GTK_DIR_UP },
  { MLTAG_TAB_FORWARD, GTK_DIR_TAB_FORWARD },
  { MLTAG_TAB_BACKWARD, GTK_DIR_TAB_BACKWARD },
  { MLTAG_DOWN, GTK_DIR_DOWN },
  { MLTAG_LEFT, GTK_DIR_LEFT },
};

/* expander_style : conversion table */
const lookup_info ml_table_expander_style[] = {
  { 0, 4 },
  { MLTAG_EXPANDED, GTK_EXPANDER_EXPANDED },
  { MLTAG_SEMI_COLLAPSED, GTK_EXPANDER_SEMI_COLLAPSED },
  { MLTAG_SEMI_EXPANDED, GTK_EXPANDER_SEMI_EXPANDED },
  { MLTAG_COLLAPSED, GTK_EXPANDER_COLLAPSED },
};

/* icon_size : conversion table */
const lookup_info ml_table_icon_size[] = {
  { 0, 7 },
  { MLTAG_DIALOG, GTK_ICON_SIZE_DIALOG },
  { MLTAG_SMALL_TOOLBAR, GTK_ICON_SIZE_SMALL_TOOLBAR },
  { MLTAG_DND, GTK_ICON_SIZE_DND },
  { MLTAG_LARGE_TOOLBAR, GTK_ICON_SIZE_LARGE_TOOLBAR },
  { MLTAG_BUTTON, GTK_ICON_SIZE_BUTTON },
  { MLTAG_MENU, GTK_ICON_SIZE_MENU },
  { MLTAG_INVALID, GTK_ICON_SIZE_INVALID },
};

/* side_type : conversion table */
const lookup_info ml_table_side_type[] = {
  { 0, 4 },
  { MLTAG_RIGHT, GTK_SIDE_RIGHT },
  { MLTAG_TOP, GTK_SIDE_TOP },
  { MLTAG_BOTTOM, GTK_SIDE_BOTTOM },
  { MLTAG_LEFT, GTK_SIDE_LEFT },
};

/* text_direction : conversion table */
const lookup_info ml_table_text_direction[] = {
  { 0, 3 },
  { MLTAG_LTR, GTK_TEXT_DIR_LTR },
  { MLTAG_RTL, GTK_TEXT_DIR_RTL },
  { MLTAG_NONE, GTK_TEXT_DIR_NONE },
};

/* justification : conversion table */
const lookup_info ml_table_justification[] = {
  { 0, 4 },
  { MLTAG_RIGHT, GTK_JUSTIFY_RIGHT },
  { MLTAG_FILL, GTK_JUSTIFY_FILL },
  { MLTAG_LEFT, GTK_JUSTIFY_LEFT },
  { MLTAG_CENTER, GTK_JUSTIFY_CENTER },
};

/* match_type : conversion table */
const lookup_info ml_table_match_type[] = {
  { 0, 6 },
  { MLTAG_EXACT, GTK_MATCH_EXACT },
  { MLTAG_ALL, GTK_MATCH_ALL },
  { MLTAG_ALL_TAIL, GTK_MATCH_ALL_TAIL },
  { MLTAG_HEAD, GTK_MATCH_HEAD },
  { MLTAG_LAST, GTK_MATCH_LAST },
  { MLTAG_TAIL, GTK_MATCH_TAIL },
};

/* menu_direction_type : conversion table */
const lookup_info ml_table_menu_direction_type[] = {
  { 0, 4 },
  { MLTAG_CHILD, GTK_MENU_DIR_CHILD },
  { MLTAG_PARENT, GTK_MENU_DIR_PARENT },
  { MLTAG_NEXT, GTK_MENU_DIR_NEXT },
  { MLTAG_PREV, GTK_MENU_DIR_PREV },
};

/* metric_type : conversion table */
const lookup_info ml_table_metric_type[] = {
  { 0, 3 },
  { MLTAG_INCHES, GTK_INCHES },
  { MLTAG_CENTIMETERS, GTK_CENTIMETERS },
  { MLTAG_PIXELS, GTK_PIXELS },
};

/* movement_step : conversion table */
const lookup_info ml_table_movement_step[] = {
  { 0, 9 },
  { MLTAG_BUFFER_ENDS, GTK_MOVEMENT_BUFFER_ENDS },
  { MLTAG_VISUAL_POSITIONS, GTK_MOVEMENT_VISUAL_POSITIONS },
  { MLTAG_WORDS, GTK_MOVEMENT_WORDS },
  { MLTAG_PARAGRAPHS, GTK_MOVEMENT_PARAGRAPHS },
  { MLTAG_DISPLAY_LINE_ENDS, GTK_MOVEMENT_DISPLAY_LINE_ENDS },
  { MLTAG_PARAGRAPH_ENDS, GTK_MOVEMENT_PARAGRAPH_ENDS },
  { MLTAG_LOGICAL_POSITIONS, GTK_MOVEMENT_LOGICAL_POSITIONS },
  { MLTAG_DISPLAY_LINES, GTK_MOVEMENT_DISPLAY_LINES },
  { MLTAG_PAGES, GTK_MOVEMENT_PAGES },
};

/* orientation : conversion table */
const lookup_info ml_table_orientation[] = {
  { 0, 2 },
  { MLTAG_VERTICAL, GTK_ORIENTATION_VERTICAL },
  { MLTAG_HORIZONTAL, GTK_ORIENTATION_HORIZONTAL },
};

/* corner_type : conversion table */
const lookup_info ml_table_corner_type[] = {
  { 0, 4 },
  { MLTAG_TOP_LEFT, GTK_CORNER_TOP_LEFT },
  { MLTAG_BOTTOM_LEFT, GTK_CORNER_BOTTOM_LEFT },
  { MLTAG_BOTTOM_RIGHT, GTK_CORNER_BOTTOM_RIGHT },
  { MLTAG_TOP_RIGHT, GTK_CORNER_TOP_RIGHT },
};

/* pack_type : conversion table */
const lookup_info ml_table_pack_type[] = {
  { 0, 2 },
  { MLTAG_END, GTK_PACK_END },
  { MLTAG_START, GTK_PACK_START },
};

/* path_priority : conversion table */
const lookup_info ml_table_path_priority[] = {
  { 0, 6 },
  { MLTAG_HIGHEST, GTK_PATH_PRIO_HIGHEST },
  { MLTAG_RC, GTK_PATH_PRIO_RC },
  { MLTAG_GTK, GTK_PATH_PRIO_GTK },
  { MLTAG_APPLICATION, GTK_PATH_PRIO_APPLICATION },
  { MLTAG_THEME, GTK_PATH_PRIO_THEME },
  { MLTAG_LOWEST, GTK_PATH_PRIO_LOWEST },
};

/* path_type : conversion table */
const lookup_info ml_table_path_type[] = {
  { 0, 3 },
  { MLTAG_CLASS, GTK_PATH_CLASS },
  { MLTAG_WIDGET_CLASS, GTK_PATH_WIDGET_CLASS },
  { MLTAG_WIDGET, GTK_PATH_WIDGET },
};

/* policy_type : conversion table */
const lookup_info ml_table_policy_type[] = {
  { 0, 3 },
  { MLTAG_AUTOMATIC, GTK_POLICY_AUTOMATIC },
  { MLTAG_ALWAYS, GTK_POLICY_ALWAYS },
  { MLTAG_NEVER, GTK_POLICY_NEVER },
};

/* position_type : conversion table */
const lookup_info ml_table_position_type[] = {
  { 0, 4 },
  { MLTAG_RIGHT, GTK_POS_RIGHT },
  { MLTAG_TOP, GTK_POS_TOP },
  { MLTAG_BOTTOM, GTK_POS_BOTTOM },
  { MLTAG_LEFT, GTK_POS_LEFT },
};

/* preview_type : conversion table */
const lookup_info ml_table_preview_type[] = {
  { 0, 2 },
  { MLTAG_COLOR, GTK_PREVIEW_COLOR },
  { MLTAG_GRAYSCALE, GTK_PREVIEW_GRAYSCALE },
};

/* relief_style : conversion table */
const lookup_info ml_table_relief_style[] = {
  { 0, 3 },
  { MLTAG_NORMAL, GTK_RELIEF_NORMAL },
  { MLTAG_HALF, GTK_RELIEF_HALF },
  { MLTAG_NONE, GTK_RELIEF_NONE },
};

/* resize_mode : conversion table */
const lookup_info ml_table_resize_mode[] = {
  { 0, 3 },
  { MLTAG_IMMEDIATE, GTK_RESIZE_IMMEDIATE },
  { MLTAG_QUEUE, GTK_RESIZE_QUEUE },
  { MLTAG_PARENT, GTK_RESIZE_PARENT },
};

/* signal_run_type : conversion table */
const lookup_info ml_table_signal_run_type[] = {
  { 0, 6 },
  { MLTAG_ACTION, GTK_RUN_ACTION },
  { MLTAG_NO_HOOKS, GTK_RUN_NO_HOOKS },
  { MLTAG_FIRST, GTK_RUN_FIRST },
  { MLTAG_NO_RECURSE, GTK_RUN_NO_RECURSE },
  { MLTAG_BOTH, GTK_RUN_BOTH },
  { MLTAG_LAST, GTK_RUN_LAST },
};

/* scroll_type : conversion table */
const lookup_info ml_table_scroll_type[] = {
  { 0, 16 },
  { MLTAG_STEP_LEFT, GTK_SCROLL_STEP_LEFT },
  { MLTAG_STEP_BACKWARD, GTK_SCROLL_STEP_BACKWARD },
  { MLTAG_PAGE_BACKWARD, GTK_SCROLL_PAGE_BACKWARD },
  { MLTAG_STEP_RIGHT, GTK_SCROLL_STEP_RIGHT },
  { MLTAG_PAGE_DOWN, GTK_SCROLL_PAGE_DOWN },
  { MLTAG_PAGE_LEFT, GTK_SCROLL_PAGE_LEFT },
  { MLTAG_STEP_FORWARD, GTK_SCROLL_STEP_FORWARD },
  { MLTAG_END, GTK_SCROLL_END },
  { MLTAG_STEP_UP, GTK_SCROLL_STEP_UP },
  { MLTAG_START, GTK_SCROLL_START },
  { MLTAG_PAGE_UP, GTK_SCROLL_PAGE_UP },
  { MLTAG_PAGE_FORWARD, GTK_SCROLL_PAGE_FORWARD },
  { MLTAG_PAGE_RIGHT, GTK_SCROLL_PAGE_RIGHT },
  { MLTAG_JUMP, GTK_SCROLL_JUMP },
  { MLTAG_NONE, GTK_SCROLL_NONE },
  { MLTAG_STEP_DOWN, GTK_SCROLL_STEP_DOWN },
};

/* selection_mode : conversion table */
const lookup_info ml_table_selection_mode[] = {
  { 0, 4 },
  { MLTAG_BROWSE, GTK_SELECTION_BROWSE },
  { MLTAG_SINGLE, GTK_SELECTION_SINGLE },
  { MLTAG_MULTIPLE, GTK_SELECTION_MULTIPLE },
  { MLTAG_NONE, GTK_SELECTION_NONE },
};

/* shadow_type : conversion table */
const lookup_info ml_table_shadow_type[] = {
  { 0, 5 },
  { MLTAG_IN, GTK_SHADOW_IN },
  { MLTAG_OUT, GTK_SHADOW_OUT },
  { MLTAG_ETCHED_OUT, GTK_SHADOW_ETCHED_OUT },
  { MLTAG_ETCHED_IN, GTK_SHADOW_ETCHED_IN },
  { MLTAG_NONE, GTK_SHADOW_NONE },
};

/* state_type : conversion table */
const lookup_info ml_table_state_type[] = {
  { 0, 5 },
  { MLTAG_INSENSITIVE, GTK_STATE_INSENSITIVE },
  { MLTAG_ACTIVE, GTK_STATE_ACTIVE },
  { MLTAG_NORMAL, GTK_STATE_NORMAL },
  { MLTAG_SELECTED, GTK_STATE_SELECTED },
  { MLTAG_PRELIGHT, GTK_STATE_PRELIGHT },
};

/* submenu_direction : conversion table */
const lookup_info ml_table_submenu_direction[] = {
  { 0, 2 },
  { MLTAG_RIGHT, GTK_DIRECTION_RIGHT },
  { MLTAG_LEFT, GTK_DIRECTION_LEFT },
};

/* submenu_placement : conversion table */
const lookup_info ml_table_submenu_placement[] = {
  { 0, 2 },
  { MLTAG_TOP_BOTTOM, GTK_TOP_BOTTOM },
  { MLTAG_LEFT_RIGHT, GTK_LEFT_RIGHT },
};

/* toolbar_style : conversion table */
const lookup_info ml_table_toolbar_style[] = {
  { 0, 4 },
  { MLTAG_BOTH_HORIZ, GTK_TOOLBAR_BOTH_HORIZ },
  { MLTAG_BOTH, GTK_TOOLBAR_BOTH },
  { MLTAG_ICONS, GTK_TOOLBAR_ICONS },
  { MLTAG_TEXT, GTK_TOOLBAR_TEXT },
};

/* update_type : conversion table */
const lookup_info ml_table_update_type[] = {
  { 0, 3 },
  { MLTAG_CONTINUOUS, GTK_UPDATE_CONTINUOUS },
  { MLTAG_DISCONTINUOUS, GTK_UPDATE_DISCONTINUOUS },
  { MLTAG_DELAYED, GTK_UPDATE_DELAYED },
};

/* visibility : conversion table */
const lookup_info ml_table_visibility[] = {
  { 0, 3 },
  { MLTAG_PARTIAL, GTK_VISIBILITY_PARTIAL },
  { MLTAG_FULL, GTK_VISIBILITY_FULL },
  { MLTAG_NONE, GTK_VISIBILITY_NONE },
};

/* window_position : conversion table */
const lookup_info ml_table_window_position[] = {
  { 0, 5 },
  { MLTAG_CENTER_ALWAYS, GTK_WIN_POS_CENTER_ALWAYS },
  { MLTAG_CENTER_ON_PARENT, GTK_WIN_POS_CENTER_ON_PARENT },
  { MLTAG_MOUSE, GTK_WIN_POS_MOUSE },
  { MLTAG_NONE, GTK_WIN_POS_NONE },
  { MLTAG_CENTER, GTK_WIN_POS_CENTER },
};

/* window_type : conversion table */
const lookup_info ml_table_window_type[] = {
  { 0, 2 },
  { MLTAG_POPUP, GTK_WINDOW_POPUP },
  { MLTAG_TOPLEVEL, GTK_WINDOW_TOPLEVEL },
};

/* wrap_mode : conversion table */
const lookup_info ml_table_wrap_mode[] = {
  { 0, 3 },
  { MLTAG_CHAR, GTK_WRAP_CHAR },
  { MLTAG_NONE, GTK_WRAP_NONE },
  { MLTAG_WORD, GTK_WRAP_WORD },
};

/* sort_type : conversion table */
const lookup_info ml_table_sort_type[] = {
  { 0, 2 },
  { MLTAG_DESCENDING, GTK_SORT_DESCENDING },
  { MLTAG_ASCENDING, GTK_SORT_ASCENDING },
};

/* cell_type : conversion table */
const lookup_info ml_table_cell_type[] = {
  { 0, 5 },
  { MLTAG_EMPTY, GTK_CELL_EMPTY },
  { MLTAG_WIDGET, GTK_CELL_WIDGET },
  { MLTAG_PIXTEXT, GTK_CELL_PIXTEXT },
  { MLTAG_TEXT, GTK_CELL_TEXT },
  { MLTAG_PIXMAP, GTK_CELL_PIXMAP },
};

/* toolbar_child : conversion table */
const lookup_info ml_table_toolbar_child[] = {
  { 0, 5 },
  { MLTAG_RADIOBUTTON, GTK_TOOLBAR_CHILD_RADIOBUTTON },
  { MLTAG_TOGGLEBUTTON, GTK_TOOLBAR_CHILD_TOGGLEBUTTON },
  { MLTAG_WIDGET, GTK_TOOLBAR_CHILD_WIDGET },
  { MLTAG_SPACE, GTK_TOOLBAR_CHILD_SPACE },
  { MLTAG_BUTTON, GTK_TOOLBAR_CHILD_BUTTON },
};

/* toolbar_space_style : conversion table */
const lookup_info ml_table_toolbar_space_style[] = {
  { 0, 2 },
  { MLTAG_EMPTY, GTK_TOOLBAR_SPACE_EMPTY },
  { MLTAG_LINE, GTK_TOOLBAR_SPACE_LINE },
};

/* spin_type : conversion table */
const lookup_info ml_table_spin_type[] = {
  { 0, 7 },
  { MLTAG_STEP_BACKWARD, GTK_SPIN_STEP_BACKWARD },
  { MLTAG_PAGE_BACKWARD, GTK_SPIN_PAGE_BACKWARD },
  { MLTAG_USER_DEFINED, GTK_SPIN_USER_DEFINED },
  { MLTAG_STEP_FORWARD, GTK_SPIN_STEP_FORWARD },
  { MLTAG_END, GTK_SPIN_END },
  { MLTAG_PAGE_FORWARD, GTK_SPIN_PAGE_FORWARD },
  { MLTAG_HOME, GTK_SPIN_HOME },
};

/* accel_flag : conversion table */
const lookup_info ml_table_accel_flag[] = {
  { 0, 2 },
  { MLTAG_LOCKED, GTK_ACCEL_LOCKED },
  { MLTAG_VISIBLE, GTK_ACCEL_VISIBLE },
};

/* button_action : conversion table */
const lookup_info ml_table_button_action[] = {
  { 0, 3 },
  { MLTAG_DRAGS, GTK_BUTTON_DRAGS },
  { MLTAG_SELECTS, GTK_BUTTON_SELECTS },
  { MLTAG_EXPANDS, GTK_BUTTON_EXPANDS },
};

/* calendar_display_options : conversion table */
const lookup_info ml_table_calendar_display_options[] = {
  { 0, 5 },
  { MLTAG_SHOW_DAY_NAMES, GTK_CALENDAR_SHOW_DAY_NAMES },
  { MLTAG_WEEK_START_MONDAY, GTK_CALENDAR_WEEK_START_MONDAY },
  { MLTAG_SHOW_WEEK_NUMBERS, GTK_CALENDAR_SHOW_WEEK_NUMBERS },
  { MLTAG_NO_MONTH_CHANGE, GTK_CALENDAR_NO_MONTH_CHANGE },
  { MLTAG_SHOW_HEADING, GTK_CALENDAR_SHOW_HEADING },
};

/* progress_bar_style : conversion table */
const lookup_info ml_table_progress_bar_style[] = {
  { 0, 2 },
  { MLTAG_CONTINUOUS, GTK_PROGRESS_CONTINUOUS },
  { MLTAG_DISCRETE, GTK_PROGRESS_DISCRETE },
};

/* progress_bar_orientation : conversion table */
const lookup_info ml_table_progress_bar_orientation[] = {
  { 0, 4 },
  { MLTAG_RIGHT_TO_LEFT, GTK_PROGRESS_RIGHT_TO_LEFT },
  { MLTAG_LEFT_TO_RIGHT, GTK_PROGRESS_LEFT_TO_RIGHT },
  { MLTAG_TOP_TO_BOTTOM, GTK_PROGRESS_TOP_TO_BOTTOM },
  { MLTAG_BOTTOM_TO_TOP, GTK_PROGRESS_BOTTOM_TO_TOP },
};

/* dest_defaults : conversion table */
const lookup_info ml_table_dest_defaults[] = {
  { 0, 4 },
  { MLTAG_HIGHLIGHT, GTK_DEST_DEFAULT_HIGHLIGHT },
  { MLTAG_MOTION, GTK_DEST_DEFAULT_MOTION },
  { MLTAG_ALL, GTK_DEST_DEFAULT_ALL },
  { MLTAG_DROP, GTK_DEST_DEFAULT_DROP },
};

/* target_flags : conversion table */
const lookup_info ml_table_target_flags[] = {
  { 0, 2 },
  { MLTAG_SAME_APP, GTK_TARGET_SAME_APP },
  { MLTAG_SAME_WIDGET, GTK_TARGET_SAME_WIDGET },
};

/* spin_button_update_policy : conversion table */
const lookup_info ml_table_spin_button_update_policy[] = {
  { 0, 2 },
  { MLTAG_IF_VALID, GTK_UPDATE_IF_VALID },
  { MLTAG_ALWAYS, GTK_UPDATE_ALWAYS },
};

/* text_window_type : conversion table */
const lookup_info ml_table_text_window_type[] = {
  { 0, 7 },
  { MLTAG_RIGHT, GTK_TEXT_WINDOW_RIGHT },
  { MLTAG_WIDGET, GTK_TEXT_WINDOW_WIDGET },
  { MLTAG_TOP, GTK_TEXT_WINDOW_TOP },
  { MLTAG_PRIVATE, GTK_TEXT_WINDOW_PRIVATE },
  { MLTAG_BOTTOM, GTK_TEXT_WINDOW_BOTTOM },
  { MLTAG_LEFT, GTK_TEXT_WINDOW_LEFT },
  { MLTAG_TEXT, GTK_TEXT_WINDOW_TEXT },
};

/* text_search_flag : conversion table */
const lookup_info ml_table_text_search_flag[] = {
  { 0, 2 },
  { MLTAG_VISIBLE_ONLY, GTK_TEXT_SEARCH_VISIBLE_ONLY },
  { MLTAG_TEXT_ONLY, GTK_TEXT_SEARCH_TEXT_ONLY },
};

/* tree_view_column_sizing : conversion table */
const lookup_info ml_table_tree_view_column_sizing[] = {
  { 0, 3 },
  { MLTAG_GROW_ONLY, GTK_TREE_VIEW_COLUMN_GROW_ONLY },
  { MLTAG_FIXED, GTK_TREE_VIEW_COLUMN_FIXED },
  { MLTAG_AUTOSIZE, GTK_TREE_VIEW_COLUMN_AUTOSIZE },
};

/* cell_renderer_mode : conversion table */
const lookup_info ml_table_cell_renderer_mode[] = {
  { 0, 3 },
  { MLTAG_ACTIVATABLE, GTK_CELL_RENDERER_MODE_ACTIVATABLE },
  { MLTAG_EDITABLE, GTK_CELL_RENDERER_MODE_EDITABLE },
  { MLTAG_INERT, GTK_CELL_RENDERER_MODE_INERT },
};

/* message_type : conversion table */
const lookup_info ml_table_message_type[] = {
  { 0, 4 },
  { MLTAG_ERROR, GTK_MESSAGE_ERROR },
  { MLTAG_QUESTION, GTK_MESSAGE_QUESTION },
  { MLTAG_WARNING, GTK_MESSAGE_WARNING },
  { MLTAG_INFO, GTK_MESSAGE_INFO },
};

/* buttons_type : conversion table */
const lookup_info ml_table_buttons_type[] = {
  { 0, 6 },
  { MLTAG_CLOSE, GTK_BUTTONS_CLOSE },
  { MLTAG_CANCEL, GTK_BUTTONS_CANCEL },
  { MLTAG_OK, GTK_BUTTONS_OK },
  { MLTAG_YES_NO, GTK_BUTTONS_YES_NO },
  { MLTAG_OK_CANCEL, GTK_BUTTONS_OK_CANCEL },
  { MLTAG_NONE, GTK_BUTTONS_NONE },
};

/* response : conversion table */
const lookup_info ml_table_response[] = {
  { 0, 11 },
  { MLTAG_CLOSE, GTK_RESPONSE_CLOSE },
  { MLTAG_CANCEL, GTK_RESPONSE_CANCEL },
  { MLTAG_NO, GTK_RESPONSE_NO },
  { MLTAG_OK, GTK_RESPONSE_OK },
  { MLTAG_YES, GTK_RESPONSE_YES },
  { MLTAG_DELETE_EVENT, GTK_RESPONSE_DELETE_EVENT },
  { MLTAG_APPLY, GTK_RESPONSE_APPLY },
  { MLTAG_HELP, GTK_RESPONSE_HELP },
  { MLTAG_NONE, GTK_RESPONSE_NONE },
  { MLTAG_REJECT, GTK_RESPONSE_REJECT },
  { MLTAG_ACCEPT, GTK_RESPONSE_ACCEPT },
};

/* widget_flags : conversion table */
const lookup_info ml_table_widget_flags[] = {
  { 0, 20 },
  { MLTAG_HAS_DEFAULT, GTK_HAS_DEFAULT },
  { MLTAG_RECEIVES_DEFAULT, GTK_RECEIVES_DEFAULT },
  { MLTAG_PARENT_SENSITIVE, GTK_PARENT_SENSITIVE },
  { MLTAG_HAS_FOCUS, GTK_HAS_FOCUS },
  { MLTAG_NO_WINDOW, GTK_NO_WINDOW },
  { MLTAG_CAN_FOCUS, GTK_CAN_FOCUS },
  { MLTAG_APP_PAINTABLE, GTK_APP_PAINTABLE },
  { MLTAG_SENSITIVE, GTK_SENSITIVE },
  { MLTAG_MAPPED, GTK_MAPPED },
  { MLTAG_REALIZED, GTK_REALIZED },
  { MLTAG_IN_DESTRUCTION, GTK_IN_DESTRUCTION },
  { MLTAG_TOPLEVEL, GTK_TOPLEVEL },
  { MLTAG_NO_REPARENT, GTK_NO_REPARENT },
  { MLTAG_COMPOSITE_CHILD, GTK_COMPOSITE_CHILD },
  { MLTAG_VISIBLE, GTK_VISIBLE },
  { MLTAG_DOUBLE_BUFFERED, GTK_DOUBLE_BUFFERED },
  { MLTAG_CAN_DEFAULT, GTK_CAN_DEFAULT },
  { MLTAG_RC_STYLE, GTK_RC_STYLE },
  { MLTAG_HAS_GRAB, GTK_HAS_GRAB },
  { MLTAG_FLOATING, GTK_FLOATING },
};

/* image_type : conversion table */
const lookup_info ml_table_image_type[] = {
  { 0, 7 },
  { MLTAG_ANIMATION, GTK_IMAGE_ANIMATION },
  { MLTAG_EMPTY, GTK_IMAGE_EMPTY },
  { MLTAG_ICON_SET, GTK_IMAGE_ICON_SET },
  { MLTAG_STOCK, GTK_IMAGE_STOCK },
  { MLTAG_IMAGE, GTK_IMAGE_IMAGE },
  { MLTAG_PIXBUF, GTK_IMAGE_PIXBUF },
  { MLTAG_PIXMAP, GTK_IMAGE_PIXMAP },
};

/* size_group_mode : conversion table */
const lookup_info ml_table_size_group_mode[] = {
  { 0, 4 },
  { MLTAG_VERTICAL, GTK_SIZE_GROUP_VERTICAL },
  { MLTAG_HORIZONTAL, GTK_SIZE_GROUP_HORIZONTAL },
  { MLTAG_BOTH, GTK_SIZE_GROUP_BOTH },
  { MLTAG_NONE, GTK_SIZE_GROUP_NONE },
};

/* file_chooser_action : conversion table */
const lookup_info ml_table_file_chooser_action[] = {
#ifdef HASGTK24
  { 0, 4 },
  { MLTAG_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER },
  { MLTAG_CREATE_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER },
  { MLTAG_OPEN, GTK_FILE_CHOOSER_ACTION_OPEN },
  { MLTAG_SAVE, GTK_FILE_CHOOSER_ACTION_SAVE },
#else
  {0, 0 }
#endif /* HASGTK24 */
};

/* tree_model_flags : conversion table */
const lookup_info ml_table_tree_model_flags[] = {
  { 0, 2 },
  { MLTAG_LIST_ONLY, GTK_TREE_MODEL_LIST_ONLY },
  { MLTAG_ITERS_PERSIST, GTK_TREE_MODEL_ITERS_PERSIST },
};

/* tree_view_drop_position : conversion table */
const lookup_info ml_table_tree_view_drop_position[] = {
  { 0, 4 },
  { MLTAG_BEFORE, GTK_TREE_VIEW_DROP_BEFORE },
  { MLTAG_INTO_OR_AFTER, GTK_TREE_VIEW_DROP_INTO_OR_AFTER },
  { MLTAG_AFTER, GTK_TREE_VIEW_DROP_AFTER },
  { MLTAG_INTO_OR_BEFORE, GTK_TREE_VIEW_DROP_INTO_OR_BEFORE },
};

/* file_filter_flags : conversion table */
const lookup_info ml_table_file_filter_flags[] = {
#ifdef HASGTK24
  { 0, 4 },
  { MLTAG_MIME_TYPE, GTK_FILE_FILTER_MIME_TYPE },
  { MLTAG_FILENAME, GTK_FILE_FILTER_FILENAME },
  { MLTAG_URI, GTK_FILE_FILTER_URI },
  { MLTAG_DISPLAY_NAME, GTK_FILE_FILTER_DISPLAY_NAME },
#else
  {0, 0 }
#endif /* HASGTK24 */
};

/* ui_manager_item_type : conversion table */
const lookup_info ml_table_ui_manager_item_type[] = {
#ifdef HASGTK24
  { 0, 10 },
  { MLTAG_POPUP, GTK_UI_MANAGER_POPUP },
  { MLTAG_TOOLBAR, GTK_UI_MANAGER_TOOLBAR },
  { MLTAG_ACCELERATOR, GTK_UI_MANAGER_ACCELERATOR },
  { MLTAG_PLACEHOLDER, GTK_UI_MANAGER_PLACEHOLDER },
  { MLTAG_TOOLITEM, GTK_UI_MANAGER_TOOLITEM },
  { MLTAG_AUTO, GTK_UI_MANAGER_AUTO },
  { MLTAG_SEPARATOR, GTK_UI_MANAGER_SEPARATOR },
  { MLTAG_MENU, GTK_UI_MANAGER_MENU },
  { MLTAG_MENUBAR, GTK_UI_MANAGER_MENUBAR },
  { MLTAG_MENUITEM, GTK_UI_MANAGER_MENUITEM },
#else
  {0, 0 }
#endif /* HASGTK24 */
};

/* assistant_page_type : conversion table */
const lookup_info ml_table_assistant_page_type[] = {
#ifdef HASGTK210
  { 0, 5 },
  { MLTAG_PROGRESS, GTK_ASSISTANT_PAGE_PROGRESS },
  { MLTAG_SUMMARY, GTK_ASSISTANT_PAGE_SUMMARY },
  { MLTAG_CONFIRM, GTK_ASSISTANT_PAGE_CONFIRM },
  { MLTAG_CONTENT, GTK_ASSISTANT_PAGE_CONTENT },
  { MLTAG_INTRO, GTK_ASSISTANT_PAGE_INTRO },
#else
  {0, 0 }
#endif /* HASGTK210 */
};

/* cell_renderer_accel_mode : conversion table */
const lookup_info ml_table_cell_renderer_accel_mode[] = {
#ifdef HASGTK210
  { 0, 2 },
  { MLTAG_GTK, GTK_CELL_RENDERER_ACCEL_MODE_GTK },
  { MLTAG_OTHER, GTK_CELL_RENDERER_ACCEL_MODE_OTHER },
#else
  {0, 0 }
#endif /* HASGTK210 */
};

/* file_chooser_confirmation : conversion table */
const lookup_info ml_table_file_chooser_confirmation[] = {
#ifdef HASGTK28
  { 0, 3 },
  { MLTAG_CONFIRM, GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM },
  { MLTAG_SELECT_AGAIN, GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN },
  { MLTAG_ACCEPT_FILENAME, GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME },
#else
  {0, 0 }
#endif /* HASGTK28 */
};

CAMLprim value ml_gtk_get_tables ()
{
  static const lookup_info *ml_lookup_tables[] = {
    ml_table_anchor_type,
    ml_table_arrow_type,
    ml_table_attach_options,
    ml_table_button_box_style,
    ml_table_curve_type,
    ml_table_delete_type,
    ml_table_direction_type,
    ml_table_expander_style,
    ml_table_icon_size,
    ml_table_side_type,
    ml_table_text_direction,
    ml_table_justification,
    ml_table_match_type,
    ml_table_menu_direction_type,
    ml_table_metric_type,
    ml_table_movement_step,
    ml_table_orientation,
    ml_table_corner_type,
    ml_table_pack_type,
    ml_table_path_priority,
    ml_table_path_type,
    ml_table_policy_type,
    ml_table_position_type,
    ml_table_preview_type,
    ml_table_relief_style,
    ml_table_resize_mode,
    ml_table_signal_run_type,
    ml_table_scroll_type,
    ml_table_selection_mode,
    ml_table_shadow_type,
    ml_table_state_type,
    ml_table_submenu_direction,
    ml_table_submenu_placement,
    ml_table_toolbar_style,
    ml_table_update_type,
    ml_table_visibility,
    ml_table_window_position,
    ml_table_window_type,
    ml_table_wrap_mode,
    ml_table_sort_type,
    ml_table_cell_type,
    ml_table_toolbar_child,
    ml_table_toolbar_space_style,
    ml_table_spin_type,
    ml_table_accel_flag,
    ml_table_button_action,
    ml_table_calendar_display_options,
    ml_table_progress_bar_style,
    ml_table_progress_bar_orientation,
    ml_table_dest_defaults,
    ml_table_target_flags,
    ml_table_spin_button_update_policy,
    ml_table_text_window_type,
    ml_table_text_search_flag,
    ml_table_tree_view_column_sizing,
    ml_table_cell_renderer_mode,
    ml_table_message_type,
    ml_table_buttons_type,
    ml_table_response,
    ml_table_widget_flags,
    ml_table_image_type,
    ml_table_size_group_mode,
    ml_table_file_chooser_action,
    ml_table_tree_model_flags,
    ml_table_tree_view_drop_position,
    ml_table_file_filter_flags,
    ml_table_ui_manager_item_type,
    ml_table_assistant_page_type,
    ml_table_cell_renderer_accel_mode,
    ml_table_file_chooser_confirmation,
  };
  return (value)ml_lookup_tables;}
