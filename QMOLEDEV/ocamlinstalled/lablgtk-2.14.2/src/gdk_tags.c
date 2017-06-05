/* event_type : conversion table */
const lookup_info ml_table_event_type[] = {
  { 0, 35 },
  { MLTAG_SELECTION_CLEAR, GDK_SELECTION_CLEAR },
  { MLTAG_PROXIMITY_IN, GDK_PROXIMITY_IN },
  { MLTAG_DROP_START, GDK_DROP_START },
  { MLTAG_THREE_BUTTON_PRESS, GDK_3BUTTON_PRESS },
  { MLTAG_NOTHING, GDK_NOTHING },
  { MLTAG_NO_EXPOSE, GDK_NO_EXPOSE },
  { MLTAG_VISIBILITY_NOTIFY, GDK_VISIBILITY_NOTIFY },
  { MLTAG_BUTTON_RELEASE, GDK_BUTTON_RELEASE },
  { MLTAG_DRAG_STATUS, GDK_DRAG_STATUS },
  { MLTAG_KEY_PRESS, GDK_KEY_PRESS },
  { MLTAG_EXPOSE, GDK_EXPOSE },
  { MLTAG_SCROLL, GDK_SCROLL },
  { MLTAG_WINDOW_STATE, GDK_WINDOW_STATE },
  { MLTAG_KEY_RELEASE, GDK_KEY_RELEASE },
  { MLTAG_MAP, GDK_MAP },
  { MLTAG_FOCUS_CHANGE, GDK_FOCUS_CHANGE },
  { MLTAG_SELECTION_NOTIFY, GDK_SELECTION_NOTIFY },
  { MLTAG_PROXIMITY_OUT, GDK_PROXIMITY_OUT },
  { MLTAG_SELECTION_REQUEST, GDK_SELECTION_REQUEST },
  { MLTAG_TWO_BUTTON_PRESS, GDK_2BUTTON_PRESS },
  { MLTAG_ENTER_NOTIFY, GDK_ENTER_NOTIFY },
  { MLTAG_DELETE, GDK_DELETE },
  { MLTAG_DRAG_ENTER, GDK_DRAG_ENTER },
  { MLTAG_DRAG_LEAVE, GDK_DRAG_LEAVE },
  { MLTAG_BUTTON_PRESS, GDK_BUTTON_PRESS },
  { MLTAG_DESTROY, GDK_DESTROY },
  { MLTAG_UNMAP, GDK_UNMAP },
  { MLTAG_SETTING, GDK_SETTING },
  { MLTAG_DROP_FINISHED, GDK_DROP_FINISHED },
  { MLTAG_CLIENT_EVENT, GDK_CLIENT_EVENT },
  { MLTAG_DRAG_MOTION, GDK_DRAG_MOTION },
  { MLTAG_LEAVE_NOTIFY, GDK_LEAVE_NOTIFY },
  { MLTAG_CONFIGURE, GDK_CONFIGURE },
  { MLTAG_MOTION_NOTIFY, GDK_MOTION_NOTIFY },
  { MLTAG_PROPERTY_NOTIFY, GDK_PROPERTY_NOTIFY },
};

/* event_mask : conversion table */
const lookup_info ml_table_event_mask[] = {
  { 0, 22 },
  { MLTAG_PROPERTY_CHANGE, GDK_PROPERTY_CHANGE_MASK },
  { MLTAG_STRUCTURE, GDK_STRUCTURE_MASK },
  { MLTAG_PROXIMITY_IN, GDK_PROXIMITY_IN_MASK },
  { MLTAG_VISIBILITY_NOTIFY, GDK_VISIBILITY_NOTIFY_MASK },
  { MLTAG_BUTTON_RELEASE, GDK_BUTTON_RELEASE_MASK },
  { MLTAG_EXPOSURE, GDK_EXPOSURE_MASK },
  { MLTAG_BUTTON2_MOTION, GDK_BUTTON2_MOTION_MASK },
  { MLTAG_POINTER_MOTION_HINT, GDK_POINTER_MOTION_HINT_MASK },
  { MLTAG_KEY_PRESS, GDK_KEY_PRESS_MASK },
  { MLTAG_SUBSTRUCTURE, GDK_SUBSTRUCTURE_MASK },
  { MLTAG_SCROLL, GDK_SCROLL_MASK },
  { MLTAG_KEY_RELEASE, GDK_KEY_RELEASE_MASK },
  { MLTAG_FOCUS_CHANGE, GDK_FOCUS_CHANGE_MASK },
  { MLTAG_PROXIMITY_OUT, GDK_PROXIMITY_OUT_MASK },
  { MLTAG_POINTER_MOTION, GDK_POINTER_MOTION_MASK },
  { MLTAG_ENTER_NOTIFY, GDK_ENTER_NOTIFY_MASK },
  { MLTAG_BUTTON_PRESS, GDK_BUTTON_PRESS_MASK },
  { MLTAG_BUTTON3_MOTION, GDK_BUTTON3_MOTION_MASK },
  { MLTAG_BUTTON_MOTION, GDK_BUTTON_MOTION_MASK },
  { MLTAG_ALL_EVENTS, GDK_ALL_EVENTS_MASK },
  { MLTAG_LEAVE_NOTIFY, GDK_LEAVE_NOTIFY_MASK },
  { MLTAG_BUTTON1_MOTION, GDK_BUTTON1_MOTION_MASK },
};

/* extension_mode : conversion table */
const lookup_info ml_table_extension_mode[] = {
  { 0, 3 },
  { MLTAG_CURSOR, GDK_EXTENSION_EVENTS_CURSOR },
  { MLTAG_ALL, GDK_EXTENSION_EVENTS_ALL },
  { MLTAG_NONE, GDK_EXTENSION_EVENTS_NONE },
};

/* gdkVisibilityState : conversion table */
const lookup_info ml_table_gdkVisibilityState[] = {
  { 0, 3 },
  { MLTAG_PARTIAL, GDK_VISIBILITY_PARTIAL },
  { MLTAG_UNOBSCURED, GDK_VISIBILITY_UNOBSCURED },
  { MLTAG_FULLY_OBSCURED, GDK_VISIBILITY_FULLY_OBSCURED },
};

/* gdkInputSource : conversion table */
const lookup_info ml_table_gdkInputSource[] = {
  { 0, 4 },
  { MLTAG_CURSOR, GDK_SOURCE_CURSOR },
  { MLTAG_ERASER, GDK_SOURCE_ERASER },
  { MLTAG_PEN, GDK_SOURCE_PEN },
  { MLTAG_MOUSE, GDK_SOURCE_MOUSE },
};

/* gdkScrollDirection : conversion table */
const lookup_info ml_table_gdkScrollDirection[] = {
  { 0, 4 },
  { MLTAG_RIGHT, GDK_SCROLL_RIGHT },
  { MLTAG_UP, GDK_SCROLL_UP },
  { MLTAG_DOWN, GDK_SCROLL_DOWN },
  { MLTAG_LEFT, GDK_SCROLL_LEFT },
};

/* gdkCrossingMode : conversion table */
const lookup_info ml_table_gdkCrossingMode[] = {
  { 0, 3 },
  { MLTAG_NORMAL, GDK_CROSSING_NORMAL },
  { MLTAG_UNGRAB, GDK_CROSSING_UNGRAB },
  { MLTAG_GRAB, GDK_CROSSING_GRAB },
};

/* gdkNotifyType : conversion table */
const lookup_info ml_table_gdkNotifyType[] = {
  { 0, 6 },
  { MLTAG_UNKNOWN, GDK_NOTIFY_UNKNOWN },
  { MLTAG_INFERIOR, GDK_NOTIFY_INFERIOR },
  { MLTAG_NONLINEAR_VIRTUAL, GDK_NOTIFY_NONLINEAR_VIRTUAL },
  { MLTAG_ANCESTOR, GDK_NOTIFY_ANCESTOR },
  { MLTAG_VIRTUAL, GDK_NOTIFY_VIRTUAL },
  { MLTAG_NONLINEAR, GDK_NOTIFY_NONLINEAR },
};

/* gdkSettingAction : conversion table */
const lookup_info ml_table_gdkSettingAction[] = {
  { 0, 3 },
  { MLTAG_CHANGED, GDK_SETTING_ACTION_CHANGED },
  { MLTAG_NEW, GDK_SETTING_ACTION_NEW },
  { MLTAG_DELETED, GDK_SETTING_ACTION_DELETED },
};

/* gdkWindowState : conversion table */
const lookup_info ml_table_gdkWindowState[] = {
  { 0, 5 },
  { MLTAG_WITHDRAWN, GDK_WINDOW_STATE_WITHDRAWN },
  { MLTAG_ICONIFIED, GDK_WINDOW_STATE_ICONIFIED },
  { MLTAG_FULLSCREEN, GDK_WINDOW_STATE_FULLSCREEN },
  { MLTAG_MAXIMIZED, GDK_WINDOW_STATE_MAXIMIZED },
  { MLTAG_STICKY, GDK_WINDOW_STATE_STICKY },
};

/* fill_rule : conversion table */
const lookup_info ml_table_fill_rule[] = {
  { 0, 2 },
  { MLTAG_EVEN_ODD_RULE, GDK_EVEN_ODD_RULE },
  { MLTAG_WINDING_RULE, GDK_WINDING_RULE },
};

/* overlap_type : conversion table */
const lookup_info ml_table_overlap_type[] = {
  { 0, 3 },
  { MLTAG_IN, GDK_OVERLAP_RECTANGLE_IN },
  { MLTAG_OUT, GDK_OVERLAP_RECTANGLE_OUT },
  { MLTAG_PART, GDK_OVERLAP_RECTANGLE_PART },
};

/* function_type : conversion table */
const lookup_info ml_table_function_type[] = {
  { 0, 3 },
  { MLTAG_INVERT, GDK_INVERT },
  { MLTAG_XOR, GDK_XOR },
  { MLTAG_COPY, GDK_COPY },
};

/* fill : conversion table */
const lookup_info ml_table_fill[] = {
  { 0, 4 },
  { MLTAG_OPAQUE_STIPPLED, GDK_OPAQUE_STIPPLED },
  { MLTAG_SOLID, GDK_SOLID },
  { MLTAG_TILED, GDK_TILED },
  { MLTAG_STIPPLED, GDK_STIPPLED },
};

/* subwindow_mode : conversion table */
const lookup_info ml_table_subwindow_mode[] = {
  { 0, 2 },
  { MLTAG_INCLUDE_INFERIORS, GDK_INCLUDE_INFERIORS },
  { MLTAG_CLIP_BY_CHILDREN, GDK_CLIP_BY_CHILDREN },
};

/* line_style : conversion table */
const lookup_info ml_table_line_style[] = {
  { 0, 3 },
  { MLTAG_SOLID, GDK_LINE_SOLID },
  { MLTAG_ON_OFF_DASH, GDK_LINE_ON_OFF_DASH },
  { MLTAG_DOUBLE_DASH, GDK_LINE_DOUBLE_DASH },
};

/* cap_style : conversion table */
const lookup_info ml_table_cap_style[] = {
  { 0, 4 },
  { MLTAG_ROUND, GDK_CAP_ROUND },
  { MLTAG_BUTT, GDK_CAP_BUTT },
  { MLTAG_PROJECTING, GDK_CAP_PROJECTING },
  { MLTAG_NOT_LAST, GDK_CAP_NOT_LAST },
};

/* join_style : conversion table */
const lookup_info ml_table_join_style[] = {
  { 0, 3 },
  { MLTAG_ROUND, GDK_JOIN_ROUND },
  { MLTAG_MITER, GDK_JOIN_MITER },
  { MLTAG_BEVEL, GDK_JOIN_BEVEL },
};

/* gdkModifier : conversion table */
const lookup_info ml_table_gdkModifier[] = {
  { 0, 13 },
  { MLTAG_BUTTON1, GDK_BUTTON1_MASK },
  { MLTAG_BUTTON2, GDK_BUTTON2_MASK },
  { MLTAG_BUTTON3, GDK_BUTTON3_MASK },
  { MLTAG_BUTTON4, GDK_BUTTON4_MASK },
  { MLTAG_BUTTON5, GDK_BUTTON5_MASK },
  { MLTAG_SHIFT, GDK_SHIFT_MASK },
  { MLTAG_CONTROL, GDK_CONTROL_MASK },
  { MLTAG_LOCK, GDK_LOCK_MASK },
  { MLTAG_MOD1, GDK_MOD1_MASK },
  { MLTAG_MOD2, GDK_MOD2_MASK },
  { MLTAG_MOD3, GDK_MOD3_MASK },
  { MLTAG_MOD4, GDK_MOD4_MASK },
  { MLTAG_MOD5, GDK_MOD5_MASK },
};

/* gdkImageType : conversion table */
const lookup_info ml_table_gdkImageType[] = {
  { 0, 3 },
  { MLTAG_SHARED, GDK_IMAGE_SHARED },
  { MLTAG_FASTEST, GDK_IMAGE_FASTEST },
  { MLTAG_NORMAL, GDK_IMAGE_NORMAL },
};

/* gdkVisualType : conversion table */
const lookup_info ml_table_gdkVisualType[] = {
  { 0, 6 },
  { MLTAG_GRAYSCALE, GDK_VISUAL_GRAYSCALE },
  { MLTAG_PSEUDO_COLOR, GDK_VISUAL_PSEUDO_COLOR },
  { MLTAG_DIRECT_COLOR, GDK_VISUAL_DIRECT_COLOR },
  { MLTAG_STATIC_COLOR, GDK_VISUAL_STATIC_COLOR },
  { MLTAG_TRUE_COLOR, GDK_VISUAL_TRUE_COLOR },
  { MLTAG_STATIC_GRAY, GDK_VISUAL_STATIC_GRAY },
};

/* font_type : conversion table */
const lookup_info ml_table_font_type[] = {
  { 0, 2 },
  { MLTAG_FONTSET, GDK_FONT_FONTSET },
  { MLTAG_FONT, GDK_FONT_FONT },
};

/* gdkDragAction : conversion table */
const lookup_info ml_table_gdkDragAction[] = {
  { 0, 6 },
  { MLTAG_ASK, GDK_ACTION_ASK },
  { MLTAG_PRIVATE, GDK_ACTION_PRIVATE },
  { MLTAG_DEFAULT, GDK_ACTION_DEFAULT },
  { MLTAG_COPY, GDK_ACTION_COPY },
  { MLTAG_LINK, GDK_ACTION_LINK },
  { MLTAG_MOVE, GDK_ACTION_MOVE },
};

/* gdkRgbDither : conversion table */
const lookup_info ml_table_gdkRgbDither[] = {
  { 0, 3 },
  { MLTAG_NORMAL, GDK_RGB_DITHER_NORMAL },
  { MLTAG_MAX, GDK_RGB_DITHER_MAX },
  { MLTAG_NONE, GDK_RGB_DITHER_NONE },
};

/* xdata : conversion table */
const lookup_info ml_table_xdata[] = {
  { 0, 4 },
  { MLTAG_INT32S, 32 },
  { MLTAG_SHORTS, 16 },
  { MLTAG_NONE, 0 },
  { MLTAG_BYTES, 8 },
};

/* property_state : conversion table */
const lookup_info ml_table_property_state[] = {
  { 0, 2 },
  { MLTAG_NEW_VALUE, GDK_PROPERTY_NEW_VALUE },
  { MLTAG_DELETE, GDK_PROPERTY_DELETE },
};

/* property_mode : conversion table */
const lookup_info ml_table_property_mode[] = {
  { 0, 3 },
  { MLTAG_APPEND, GDK_PROP_MODE_APPEND },
  { MLTAG_REPLACE, GDK_PROP_MODE_REPLACE },
  { MLTAG_PREPEND, GDK_PROP_MODE_PREPEND },
};

/* gravity : conversion table */
const lookup_info ml_table_gravity[] = {
  { 0, 10 },
  { MLTAG_SOUTH_WEST, GDK_GRAVITY_SOUTH_WEST },
  { MLTAG_NORTH_EAST, GDK_GRAVITY_NORTH_EAST },
  { MLTAG_NORTH_WEST, GDK_GRAVITY_NORTH_WEST },
  { MLTAG_SOUTH, GDK_GRAVITY_SOUTH },
  { MLTAG_NORTH, GDK_GRAVITY_NORTH },
  { MLTAG_EAST, GDK_GRAVITY_EAST },
  { MLTAG_CENTER, GDK_GRAVITY_CENTER },
  { MLTAG_STATIC, GDK_GRAVITY_STATIC },
  { MLTAG_WEST, GDK_GRAVITY_WEST },
  { MLTAG_SOUTH_EAST, GDK_GRAVITY_SOUTH_EAST },
};

/* window_type_hint : conversion table */
const lookup_info ml_table_window_type_hint[] = {
  { 0, 8 },
  { MLTAG_NORMAL, GDK_WINDOW_TYPE_HINT_NORMAL },
  { MLTAG_DIALOG, GDK_WINDOW_TYPE_HINT_DIALOG },
  { MLTAG_TOOLBAR, GDK_WINDOW_TYPE_HINT_TOOLBAR },
  { MLTAG_SPLASHSCREEN, GDK_WINDOW_TYPE_HINT_SPLASHSCREEN },
  { MLTAG_DESKTOP, GDK_WINDOW_TYPE_HINT_DESKTOP },
  { MLTAG_UTILITY, GDK_WINDOW_TYPE_HINT_UTILITY },
  { MLTAG_DOCK, GDK_WINDOW_TYPE_HINT_DOCK },
  { MLTAG_MENU, GDK_WINDOW_TYPE_HINT_MENU },
};

/* gdkCursorType : conversion table */
const lookup_info ml_table_gdkCursorType[] = {
  { 0, 77 },
  { MLTAG_BOTTOM_TEE, GDK_BOTTOM_TEE },
  { MLTAG_WATCH, GDK_WATCH },
  { MLTAG_DOUBLE_ARROW, GDK_DOUBLE_ARROW },
  { MLTAG_CLOCK, GDK_CLOCK },
  { MLTAG_LEFTBUTTON, GDK_LEFTBUTTON },
  { MLTAG_UMBRELLA, GDK_UMBRELLA },
  { MLTAG_LEFT_PTR, GDK_LEFT_PTR },
  { MLTAG_LEFT_TEE, GDK_LEFT_TEE },
  { MLTAG_CROSS, GDK_CROSS },
  { MLTAG_SB_DOWN_ARROW, GDK_SB_DOWN_ARROW },
  { MLTAG_GOBBLER, GDK_GOBBLER },
  { MLTAG_CENTER_PTR, GDK_CENTER_PTR },
  { MLTAG_RIGHT_SIDE, GDK_RIGHT_SIDE },
  { MLTAG_BOTTOM_LEFT_CORNER, GDK_BOTTOM_LEFT_CORNER },
  { MLTAG_RIGHT_PTR, GDK_RIGHT_PTR },
  { MLTAG_RIGHT_TEE, GDK_RIGHT_TEE },
  { MLTAG_MIDDLEBUTTON, GDK_MIDDLEBUTTON },
  { MLTAG_BOTTOM_RIGHT_CORNER, GDK_BOTTOM_RIGHT_CORNER },
  { MLTAG_TCROSS, GDK_TCROSS },
  { MLTAG_XTERM, GDK_XTERM },
  { MLTAG_DOTBOX, GDK_DOTBOX },
  { MLTAG_RIGHTBUTTON, GDK_RIGHTBUTTON },
  { MLTAG_LEFT_SIDE, GDK_LEFT_SIDE },
  { MLTAG_LR_ANGLE, GDK_LR_ANGLE },
  { MLTAG_PENCIL, GDK_PENCIL },
  { MLTAG_UR_ANGLE, GDK_UR_ANGLE },
  { MLTAG_SHUTTLE, GDK_SHUTTLE },
  { MLTAG_LL_ANGLE, GDK_LL_ANGLE },
  { MLTAG_SPIDER, GDK_SPIDER },
  { MLTAG_UL_ANGLE, GDK_UL_ANGLE },
  { MLTAG_QUESTION_ARROW, GDK_QUESTION_ARROW },
  { MLTAG_SIZING, GDK_SIZING },
  { MLTAG_SB_RIGHT_ARROW, GDK_SB_RIGHT_ARROW },
  { MLTAG_SPRAYCAN, GDK_SPRAYCAN },
  { MLTAG_IRON_CROSS, GDK_IRON_CROSS },
  { MLTAG_COFFEE_MUG, GDK_COFFEE_MUG },
  { MLTAG_DOT, GDK_DOT },
  { MLTAG_MAN, GDK_MAN },
  { MLTAG_TOP_SIDE, GDK_TOP_SIDE },
  { MLTAG_FLEUR, GDK_FLEUR },
  { MLTAG_SB_H_DOUBLE_ARROW, GDK_SB_H_DOUBLE_ARROW },
  { MLTAG_CROSSHAIR, GDK_CROSSHAIR },
  { MLTAG_CIRCLE, GDK_CIRCLE },
  { MLTAG_X_CURSOR, GDK_X_CURSOR },
  { MLTAG_MOUSE, GDK_MOUSE },
  { MLTAG_TOP_TEE, GDK_TOP_TEE },
  { MLTAG_DRAFT_LARGE, GDK_DRAFT_LARGE },
  { MLTAG_BOTTOM_SIDE, GDK_BOTTOM_SIDE },
  { MLTAG_BASED_ARROW_DOWN, GDK_BASED_ARROW_DOWN },
  { MLTAG_GUMBY, GDK_GUMBY },
  { MLTAG_TOP_LEFT_CORNER, GDK_TOP_LEFT_CORNER },
  { MLTAG_TOP_LEFT_ARROW, GDK_TOP_LEFT_ARROW },
  { MLTAG_EXCHANGE, GDK_EXCHANGE },
  { MLTAG_HAND1, GDK_HAND1 },
  { MLTAG_HAND2, GDK_HAND2 },
  { MLTAG_HEART, GDK_HEART },
  { MLTAG_ARROW, GDK_ARROW },
  { MLTAG_DRAFT_SMALL, GDK_DRAFT_SMALL },
  { MLTAG_CROSS_REVERSE, GDK_CROSS_REVERSE },
  { MLTAG_BASED_ARROW_UP, GDK_BASED_ARROW_UP },
  { MLTAG_DRAPED_BOX, GDK_DRAPED_BOX },
  { MLTAG_SB_V_DOUBLE_ARROW, GDK_SB_V_DOUBLE_ARROW },
  { MLTAG_BOGOSITY, GDK_BOGOSITY },
  { MLTAG_BOAT, GDK_BOAT },
  { MLTAG_DIAMOND_CROSS, GDK_DIAMOND_CROSS },
  { MLTAG_ICON, GDK_ICON },
  { MLTAG_BOX_SPIRAL, GDK_BOX_SPIRAL },
  { MLTAG_PLUS, GDK_PLUS },
  { MLTAG_TOP_RIGHT_CORNER, GDK_TOP_RIGHT_CORNER },
  { MLTAG_STAR, GDK_STAR },
  { MLTAG_TREK, GDK_TREK },
  { MLTAG_SAILBOAT, GDK_SAILBOAT },
  { MLTAG_TARGET, GDK_TARGET },
  { MLTAG_PIRATE, GDK_PIRATE },
  { MLTAG_SB_LEFT_ARROW, GDK_SB_LEFT_ARROW },
  { MLTAG_SB_UP_ARROW, GDK_SB_UP_ARROW },
  { MLTAG_RTL_LOGO, GDK_RTL_LOGO },
};

CAMLprim value ml_gdk_get_tables ()
{
  static const lookup_info *ml_lookup_tables[] = {
    ml_table_event_type,
    ml_table_event_mask,
    ml_table_extension_mode,
    ml_table_gdkVisibilityState,
    ml_table_gdkInputSource,
    ml_table_gdkScrollDirection,
    ml_table_gdkCrossingMode,
    ml_table_gdkNotifyType,
    ml_table_gdkSettingAction,
    ml_table_gdkWindowState,
    ml_table_fill_rule,
    ml_table_overlap_type,
    ml_table_function_type,
    ml_table_fill,
    ml_table_subwindow_mode,
    ml_table_line_style,
    ml_table_cap_style,
    ml_table_join_style,
    ml_table_gdkModifier,
    ml_table_gdkImageType,
    ml_table_gdkVisualType,
    ml_table_font_type,
    ml_table_gdkDragAction,
    ml_table_gdkRgbDither,
    ml_table_xdata,
    ml_table_property_state,
    ml_table_property_mode,
    ml_table_gravity,
    ml_table_window_type_hint,
    ml_table_gdkCursorType,
  };
  return (value)ml_lookup_tables;}
