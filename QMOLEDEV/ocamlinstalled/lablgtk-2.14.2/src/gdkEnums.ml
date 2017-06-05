(** gdk enums *)

open Gpointer

type event_type =
  [ `NOTHING | `DELETE | `DESTROY | `EXPOSE | `MOTION_NOTIFY | `BUTTON_PRESS
  | `TWO_BUTTON_PRESS | `THREE_BUTTON_PRESS | `BUTTON_RELEASE | `KEY_PRESS
  | `KEY_RELEASE | `ENTER_NOTIFY | `LEAVE_NOTIFY | `FOCUS_CHANGE | `CONFIGURE
  | `MAP | `UNMAP | `PROPERTY_NOTIFY | `SELECTION_CLEAR | `SELECTION_REQUEST
  | `SELECTION_NOTIFY | `PROXIMITY_IN | `PROXIMITY_OUT | `DRAG_ENTER
  | `DRAG_LEAVE | `DRAG_MOTION | `DRAG_STATUS | `DROP_START | `DROP_FINISHED
  | `CLIENT_EVENT | `VISIBILITY_NOTIFY | `NO_EXPOSE | `SCROLL | `WINDOW_STATE
  | `SETTING ]
type event_mask =
  [ `EXPOSURE | `POINTER_MOTION | `POINTER_MOTION_HINT | `BUTTON_MOTION
  | `BUTTON1_MOTION | `BUTTON2_MOTION | `BUTTON3_MOTION | `BUTTON_PRESS
  | `BUTTON_RELEASE | `KEY_PRESS | `KEY_RELEASE | `ENTER_NOTIFY
  | `LEAVE_NOTIFY | `FOCUS_CHANGE | `STRUCTURE | `PROPERTY_CHANGE
  | `VISIBILITY_NOTIFY | `PROXIMITY_IN | `PROXIMITY_OUT | `SUBSTRUCTURE
  | `SCROLL | `ALL_EVENTS ]
type extension_mode = [ `NONE | `ALL | `CURSOR ]
type visibility_state = [ `UNOBSCURED | `PARTIAL | `FULLY_OBSCURED ]
type input_source = [ `MOUSE | `PEN | `ERASER | `CURSOR ]
type scroll_direction = [ `UP | `DOWN | `LEFT | `RIGHT ]
type crossing_mode = [ `NORMAL | `GRAB | `UNGRAB ]
type notify_type =
  [ `ANCESTOR | `VIRTUAL | `INFERIOR | `NONLINEAR | `NONLINEAR_VIRTUAL
  | `UNKNOWN ]
type setting_action = [ `NEW | `CHANGED | `DELETED ]
type window_state =
  [ `WITHDRAWN | `ICONIFIED | `MAXIMIZED | `STICKY | `FULLSCREEN ]
type fill_rule = [ `EVEN_ODD_RULE | `WINDING_RULE ]
type overlap_type = [ `IN | `OUT | `PART ]
type function_type = [ `COPY | `INVERT | `XOR ]
type fill = [ `SOLID | `TILED | `STIPPLED | `OPAQUE_STIPPLED ]
type subwindow_mode = [ `CLIP_BY_CHILDREN | `INCLUDE_INFERIORS ]
type line_style = [ `SOLID | `ON_OFF_DASH | `DOUBLE_DASH ]
type cap_style = [ `NOT_LAST | `BUTT | `ROUND | `PROJECTING ]
type join_style = [ `MITER | `ROUND | `BEVEL ]
type modifier =
  [ `SHIFT | `LOCK | `CONTROL | `MOD1 | `MOD2 | `MOD3 | `MOD4 | `MOD5
  | `BUTTON1 | `BUTTON2 | `BUTTON3 | `BUTTON4 | `BUTTON5 ]
type image_type = [ `NORMAL | `SHARED | `FASTEST ]
type visual_type =
  [ `STATIC_GRAY | `GRAYSCALE | `STATIC_COLOR | `PSEUDO_COLOR | `TRUE_COLOR
  | `DIRECT_COLOR ]
type font_type = [ `FONT | `FONTSET ]
type drag_action = [ `DEFAULT | `COPY | `MOVE | `LINK | `PRIVATE | `ASK ]
type rgb_dither = [ `NONE | `NORMAL | `MAX ]
type xdata = [ `BYTES | `SHORTS | `INT32S | `NONE ]
type property_state = [ `NEW_VALUE | `DELETE ]
type property_mode = [ `REPLACE | `PREPEND | `APPEND ]
type gravity =
  [ `NORTH_WEST | `NORTH | `NORTH_EAST | `WEST | `CENTER | `EAST
  | `SOUTH_WEST | `SOUTH | `SOUTH_EAST | `STATIC ]
type window_type_hint =
  [ `NORMAL | `DIALOG | `MENU | `TOOLBAR | `SPLASHSCREEN | `UTILITY | `DOCK
  | `DESKTOP ]
type cursor_type =
  [ `X_CURSOR | `ARROW | `BASED_ARROW_DOWN | `BASED_ARROW_UP | `BOAT
  | `BOGOSITY | `BOTTOM_LEFT_CORNER | `BOTTOM_RIGHT_CORNER | `BOTTOM_SIDE
  | `BOTTOM_TEE | `BOX_SPIRAL | `CENTER_PTR | `CIRCLE | `CLOCK | `COFFEE_MUG
  | `CROSS | `CROSS_REVERSE | `CROSSHAIR | `DIAMOND_CROSS | `DOT | `DOTBOX
  | `DOUBLE_ARROW | `DRAFT_LARGE | `DRAFT_SMALL | `DRAPED_BOX | `EXCHANGE
  | `FLEUR | `GOBBLER | `GUMBY | `HAND1 | `HAND2 | `HEART | `ICON
  | `IRON_CROSS | `LEFT_PTR | `LEFT_SIDE | `LEFT_TEE | `LEFTBUTTON
  | `LL_ANGLE | `LR_ANGLE | `MAN | `MIDDLEBUTTON | `MOUSE | `PENCIL | `PIRATE
  | `PLUS | `QUESTION_ARROW | `RIGHT_PTR | `RIGHT_SIDE | `RIGHT_TEE
  | `RIGHTBUTTON | `RTL_LOGO | `SAILBOAT | `SB_DOWN_ARROW
  | `SB_H_DOUBLE_ARROW | `SB_LEFT_ARROW | `SB_RIGHT_ARROW | `SB_UP_ARROW
  | `SB_V_DOUBLE_ARROW | `SHUTTLE | `SIZING | `SPIDER | `SPRAYCAN | `STAR
  | `TARGET | `TCROSS | `TOP_LEFT_ARROW | `TOP_LEFT_CORNER
  | `TOP_RIGHT_CORNER | `TOP_SIDE | `TOP_TEE | `TREK | `UL_ANGLE | `UMBRELLA
  | `UR_ANGLE | `WATCH | `XTERM ]

(**/**)

external _get_tables : unit ->
    event_type variant_table
  * event_mask variant_table
  * extension_mode variant_table
  * visibility_state variant_table
  * input_source variant_table
  * scroll_direction variant_table
  * crossing_mode variant_table
  * notify_type variant_table
  * setting_action variant_table
  * window_state variant_table
  * fill_rule variant_table
  * overlap_type variant_table
  * function_type variant_table
  * fill variant_table
  * subwindow_mode variant_table
  * line_style variant_table
  * cap_style variant_table
  * join_style variant_table
  * modifier variant_table
  * image_type variant_table
  * visual_type variant_table
  * font_type variant_table
  * drag_action variant_table
  * rgb_dither variant_table
  * xdata variant_table
  * property_state variant_table
  * property_mode variant_table
  * gravity variant_table
  * window_type_hint variant_table
  * cursor_type variant_table
  = "ml_gdk_get_tables"


let event_type, event_mask, extension_mode, visibility_state, input_source,
    scroll_direction, crossing_mode, notify_type, setting_action,
    window_state, fill_rule, overlap_type, function_type, fill,
    subwindow_mode, line_style, cap_style, join_style, modifier, image_type,
    visual_type, font_type, drag_action, rgb_dither, xdata, property_state,
    property_mode, gravity, window_type_hint, cursor_type = _get_tables ()

let _make_enum = Gobject.Data.enum
let event_type_conv = _make_enum event_type
let event_mask_conv = Gobject.Data.flags event_mask
let extension_mode_conv = _make_enum extension_mode
let visibility_state_conv = _make_enum visibility_state
let input_source_conv = _make_enum input_source
let scroll_direction_conv = _make_enum scroll_direction
let crossing_mode_conv = _make_enum crossing_mode
let notify_type_conv = _make_enum notify_type
let setting_action_conv = _make_enum setting_action
let window_state_conv = _make_enum window_state
let fill_rule_conv = _make_enum fill_rule
let overlap_type_conv = _make_enum overlap_type
let function_type_conv = _make_enum function_type
let fill_conv = _make_enum fill
let subwindow_mode_conv = _make_enum subwindow_mode
let line_style_conv = _make_enum line_style
let cap_style_conv = _make_enum cap_style
let join_style_conv = _make_enum join_style
let modifier_conv = _make_enum modifier
let image_type_conv = _make_enum image_type
let visual_type_conv = _make_enum visual_type
let font_type_conv = _make_enum font_type
let drag_action_conv = _make_enum drag_action
let rgb_dither_conv = _make_enum rgb_dither
let xdata_conv = _make_enum xdata
let property_state_conv = _make_enum property_state
let property_mode_conv = _make_enum property_mode
let gravity_conv = _make_enum gravity
let window_type_hint_conv = _make_enum window_type_hint
let cursor_type_conv = _make_enum cursor_type
