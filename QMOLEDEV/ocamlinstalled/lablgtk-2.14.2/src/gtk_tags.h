/* anchor_type : tags and macros */
#define MLTAG_CENTER	((value)(945672661*2+1))
#define MLTAG_NORTH	((value)(498572453*2+1))
#define MLTAG_NW	((value)(17481*2+1))
#define MLTAG_NE	((value)(17463*2+1))
#define MLTAG_SOUTH	((value)(-21313043*2+1))
#define MLTAG_SW	((value)(18596*2+1))
#define MLTAG_SE	((value)(18578*2+1))
#define MLTAG_WEST	((value)(968242223*2+1))
#define MLTAG_EAST	((value)(768431101*2+1))

extern const lookup_info ml_table_anchor_type[];
#define Val_anchor_type(data) ml_lookup_from_c (ml_table_anchor_type, data)
#define Anchor_type_val(key) ml_lookup_to_c (ml_table_anchor_type, key)

/* arrow_type : tags and macros */
#define MLTAG_UP	((value)(19035*2+1))
#define MLTAG_DOWN	((value)(758038626*2+1))
#define MLTAG_LEFT	((value)(846254087*2+1))
#define MLTAG_RIGHT	((value)(-414039108*2+1))

extern const lookup_info ml_table_arrow_type[];
#define Val_arrow_type(data) ml_lookup_from_c (ml_table_arrow_type, data)
#define Arrow_type_val(key) ml_lookup_to_c (ml_table_arrow_type, key)

/* attach_options : tags and macros */
#define MLTAG_EXPAND	((value)(-151676326*2+1))
#define MLTAG_SHRINK	((value)(-622600503*2+1))
#define MLTAG_FILL	((value)(779916931*2+1))

extern const lookup_info ml_table_attach_options[];
#define Val_attach_options(data) ml_lookup_from_c (ml_table_attach_options, data)
#define Attach_options_val(key) ml_lookup_to_c (ml_table_attach_options, key)

/* button_box_style : tags and macros */
#define MLTAG_DEFAULT_STYLE	((value)(-141328461*2+1))
#define MLTAG_SPREAD	((value)(-166367629*2+1))
#define MLTAG_EDGE	((value)(768577597*2+1))
#define MLTAG_START	((value)(33139778*2+1))
#define MLTAG_END	((value)(3448763*2+1))

extern const lookup_info ml_table_button_box_style[];
#define Val_button_box_style(data) ml_lookup_from_c (ml_table_button_box_style, data)
#define Button_box_style_val(key) ml_lookup_to_c (ml_table_button_box_style, key)

/* curve_type : tags and macros */
#define MLTAG_LINEAR	((value)(522386917*2+1))
#define MLTAG_SPLINE	((value)(-232703215*2+1))
#define MLTAG_FREE	((value)(780362924*2+1))

extern const lookup_info ml_table_curve_type[];
#define Val_curve_type(data) ml_lookup_from_c (ml_table_curve_type, data)
#define Curve_type_val(key) ml_lookup_to_c (ml_table_curve_type, key)

/* delete_type : tags and macros */
#define MLTAG_CHARS	((value)(-1012804419*2+1))
#define MLTAG_WORD_ENDS	((value)(967358989*2+1))
#define MLTAG_WORDS	((value)(-866990263*2+1))
#define MLTAG_DISPLAY_LINES	((value)(794927298*2+1))
#define MLTAG_DISPLAY_LINE_ENDS	((value)(-619428858*2+1))
#define MLTAG_PARAGRAPH_ENDS	((value)(-387090935*2+1))
#define MLTAG_PARAGRAPHS	((value)(-854403771*2+1))
#define MLTAG_WHITESPACE	((value)(932606333*2+1))

extern const lookup_info ml_table_delete_type[];
#define Val_delete_type(data) ml_lookup_from_c (ml_table_delete_type, data)
#define Delete_type_val(key) ml_lookup_to_c (ml_table_delete_type, key)

/* direction_type : tags and macros */
#define MLTAG_TAB_FORWARD	((value)(494387803*2+1))
#define MLTAG_TAB_BACKWARD	((value)(649100909*2+1))

extern const lookup_info ml_table_direction_type[];
#define Val_direction_type(data) ml_lookup_from_c (ml_table_direction_type, data)
#define Direction_type_val(key) ml_lookup_to_c (ml_table_direction_type, key)

/* expander_style : tags and macros */
#define MLTAG_COLLAPSED	((value)(963272247*2+1))
#define MLTAG_SEMI_COLLAPSED	((value)(102843366*2+1))
#define MLTAG_SEMI_EXPANDED	((value)(151930474*2+1))
#define MLTAG_EXPANDED	((value)(-749428423*2+1))

extern const lookup_info ml_table_expander_style[];
#define Val_expander_style(data) ml_lookup_from_c (ml_table_expander_style, data)
#define Expander_style_val(key) ml_lookup_to_c (ml_table_expander_style, key)

/* icon_size : tags and macros */
#define MLTAG_INVALID	((value)(991669975*2+1))
#define MLTAG_MENU	((value)(857345439*2+1))
#define MLTAG_SMALL_TOOLBAR	((value)(-163065213*2+1))
#define MLTAG_LARGE_TOOLBAR	((value)(29501815*2+1))
#define MLTAG_BUTTON	((value)(207818226*2+1))
#define MLTAG_DND	((value)(3399034*2+1))
#define MLTAG_DIALOG	((value)(-474631992*2+1))

extern const lookup_info ml_table_icon_size[];
#define Val_icon_size(data) ml_lookup_from_c (ml_table_icon_size, data)
#define Icon_size_val(key) ml_lookup_to_c (ml_table_icon_size, key)

/* side_type : tags and macros */
#define MLTAG_TOP	((value)(4194933*2+1))
#define MLTAG_BOTTOM	((value)(402363115*2+1))

extern const lookup_info ml_table_side_type[];
#define Val_side_type(data) ml_lookup_from_c (ml_table_side_type, data)
#define Side_type_val(key) ml_lookup_to_c (ml_table_side_type, key)

/* text_direction : tags and macros */
#define MLTAG_NONE	((value)(868932280*2+1))
#define MLTAG_LTR	((value)(3798218*2+1))
#define MLTAG_RTL	((value)(4096586*2+1))

extern const lookup_info ml_table_text_direction[];
#define Val_text_direction(data) ml_lookup_from_c (ml_table_text_direction, data)
#define Text_direction_val(key) ml_lookup_to_c (ml_table_text_direction, key)

extern const lookup_info ml_table_justification[];
#define Val_justification(data) ml_lookup_from_c (ml_table_justification, data)
#define Justification_val(key) ml_lookup_to_c (ml_table_justification, key)

/* match_type : tags and macros */
#define MLTAG_ALL	((value)(3249409*2+1))
#define MLTAG_ALL_TAIL	((value)(739656142*2+1))
#define MLTAG_HEAD	((value)(801894688*2+1))
#define MLTAG_TAIL	((value)(934772368*2+1))
#define MLTAG_EXACT	((value)(-184395105*2+1))
#define MLTAG_LAST	((value)(846058070*2+1))

extern const lookup_info ml_table_match_type[];
#define Val_match_type(data) ml_lookup_from_c (ml_table_match_type, data)
#define Match_type_val(key) ml_lookup_to_c (ml_table_match_type, key)

/* menu_direction_type : tags and macros */
#define MLTAG_PARENT	((value)(536916266*2+1))
#define MLTAG_CHILD	((value)(-1012407940*2+1))
#define MLTAG_NEXT	((value)(868437235*2+1))
#define MLTAG_PREV	((value)(891258611*2+1))

extern const lookup_info ml_table_menu_direction_type[];
#define Val_menu_direction_type(data) ml_lookup_from_c (ml_table_menu_direction_type, data)
#define Menu_direction_type_val(key) ml_lookup_to_c (ml_table_menu_direction_type, key)

/* metric_type : tags and macros */
#define MLTAG_PIXELS	((value)(1059887917*2+1))
#define MLTAG_INCHES	((value)(-976305992*2+1))
#define MLTAG_CENTIMETERS	((value)(803081579*2+1))

extern const lookup_info ml_table_metric_type[];
#define Val_metric_type(data) ml_lookup_from_c (ml_table_metric_type, data)
#define Metric_type_val(key) ml_lookup_to_c (ml_table_metric_type, key)

/* movement_step : tags and macros */
#define MLTAG_LOGICAL_POSITIONS	((value)(163938868*2+1))
#define MLTAG_VISUAL_POSITIONS	((value)(-939942389*2+1))
#define MLTAG_PAGES	((value)(993747748*2+1))
#define MLTAG_BUFFER_ENDS	((value)(-1017891049*2+1))

extern const lookup_info ml_table_movement_step[];
#define Val_movement_step(data) ml_lookup_from_c (ml_table_movement_step, data)
#define Movement_step_val(key) ml_lookup_to_c (ml_table_movement_step, key)

/* orientation : tags and macros */
#define MLTAG_HORIZONTAL	((value)(130904292*2+1))
#define MLTAG_VERTICAL	((value)(-1013232522*2+1))

extern const lookup_info ml_table_orientation[];
#define Val_orientation(data) ml_lookup_from_c (ml_table_orientation, data)
#define Orientation_val(key) ml_lookup_to_c (ml_table_orientation, key)

/* corner_type : tags and macros */
#define MLTAG_TOP_LEFT	((value)(-71573167*2+1))
#define MLTAG_BOTTOM_LEFT	((value)(271770907*2+1))
#define MLTAG_TOP_RIGHT	((value)(1068913458*2+1))
#define MLTAG_BOTTOM_RIGHT	((value)(325230632*2+1))

extern const lookup_info ml_table_corner_type[];
#define Val_corner_type(data) ml_lookup_from_c (ml_table_corner_type, data)
#define Corner_type_val(key) ml_lookup_to_c (ml_table_corner_type, key)

extern const lookup_info ml_table_pack_type[];
#define Val_pack_type(data) ml_lookup_from_c (ml_table_pack_type, data)
#define Pack_type_val(key) ml_lookup_to_c (ml_table_pack_type, key)

/* path_priority : tags and macros */
#define MLTAG_LOWEST	((value)(427652146*2+1))
#define MLTAG_GTK	((value)(3549566*2+1))
#define MLTAG_APPLICATION	((value)(8042288*2+1))
#define MLTAG_THEME	((value)(225752553*2+1))
#define MLTAG_RC	((value)(18353*2+1))
#define MLTAG_HIGHEST	((value)(-212472316*2+1))

extern const lookup_info ml_table_path_priority[];
#define Val_path_priority(data) ml_lookup_from_c (ml_table_path_priority, data)
#define Path_priority_val(key) ml_lookup_to_c (ml_table_path_priority, key)

/* path_type : tags and macros */
#define MLTAG_WIDGET	((value)(-25863228*2+1))
#define MLTAG_WIDGET_CLASS	((value)(-32997859*2+1))
#define MLTAG_CLASS	((value)(-968445928*2+1))

extern const lookup_info ml_table_path_type[];
#define Val_path_type(data) ml_lookup_from_c (ml_table_path_type, data)
#define Path_type_val(key) ml_lookup_to_c (ml_table_path_type, key)

/* policy_type : tags and macros */
#define MLTAG_ALWAYS	((value)(-111559985*2+1))
#define MLTAG_AUTOMATIC	((value)(-916878197*2+1))
#define MLTAG_NEVER	((value)(387872364*2+1))

extern const lookup_info ml_table_policy_type[];
#define Val_policy_type(data) ml_lookup_from_c (ml_table_policy_type, data)
#define Policy_type_val(key) ml_lookup_to_c (ml_table_policy_type, key)

extern const lookup_info ml_table_position_type[];
#define Val_position_type(data) ml_lookup_from_c (ml_table_position_type, data)
#define Position_type_val(key) ml_lookup_to_c (ml_table_position_type, key)

/* preview_type : tags and macros */
#define MLTAG_COLOR	((value)(-934631101*2+1))
#define MLTAG_GRAYSCALE	((value)(-292554841*2+1))

extern const lookup_info ml_table_preview_type[];
#define Val_preview_type(data) ml_lookup_from_c (ml_table_preview_type, data)
#define Preview_type_val(key) ml_lookup_to_c (ml_table_preview_type, key)

/* relief_style : tags and macros */
#define MLTAG_NORMAL	((value)(-487842265*2+1))
#define MLTAG_HALF	((value)(801698227*2+1))

extern const lookup_info ml_table_relief_style[];
#define Val_relief_style(data) ml_lookup_from_c (ml_table_relief_style, data)
#define Relief_style_val(key) ml_lookup_to_c (ml_table_relief_style, key)

/* resize_mode : tags and macros */
#define MLTAG_QUEUE	((value)(-606550671*2+1))
#define MLTAG_IMMEDIATE	((value)(-884374575*2+1))

extern const lookup_info ml_table_resize_mode[];
#define Val_resize_mode(data) ml_lookup_from_c (ml_table_resize_mode, data)
#define Resize_mode_val(key) ml_lookup_to_c (ml_table_resize_mode, key)

/* signal_run_type : tags and macros */
#define MLTAG_FIRST	((value)(-24399856*2+1))
#define MLTAG_BOTH	((value)(735858817*2+1))
#define MLTAG_NO_RECURSE	((value)(497989665*2+1))
#define MLTAG_ACTION	((value)(-926357578*2+1))
#define MLTAG_NO_HOOKS	((value)(-407665326*2+1))

extern const lookup_info ml_table_signal_run_type[];
#define Val_signal_run_type(data) ml_lookup_from_c (ml_table_signal_run_type, data)
#define Signal_run_type_val(key) ml_lookup_to_c (ml_table_signal_run_type, key)

/* scroll_type : tags and macros */
#define MLTAG_JUMP	((value)(824872174*2+1))
#define MLTAG_STEP_FORWARD	((value)(-119149966*2+1))
#define MLTAG_STEP_BACKWARD	((value)(-878351754*2+1))
#define MLTAG_PAGE_BACKWARD	((value)(-675774701*2+1))
#define MLTAG_PAGE_FORWARD	((value)(324737141*2+1))
#define MLTAG_STEP_UP	((value)(20924590*2+1))
#define MLTAG_STEP_DOWN	((value)(988297589*2+1))
#define MLTAG_PAGE_UP	((value)(188668299*2+1))
#define MLTAG_PAGE_DOWN	((value)(-258770030*2+1))
#define MLTAG_STEP_LEFT	((value)(-1070970598*2+1))
#define MLTAG_STEP_RIGHT	((value)(-605897911*2+1))
#define MLTAG_PAGE_LEFT	((value)(-170554569*2+1))
#define MLTAG_PAGE_RIGHT	((value)(470897292*2+1))

extern const lookup_info ml_table_scroll_type[];
#define Val_scroll_type(data) ml_lookup_from_c (ml_table_scroll_type, data)
#define Scroll_type_val(key) ml_lookup_to_c (ml_table_scroll_type, key)

/* selection_mode : tags and macros */
#define MLTAG_SINGLE	((value)(-341568888*2+1))
#define MLTAG_BROWSE	((value)(-823948918*2+1))
#define MLTAG_MULTIPLE	((value)(-200117744*2+1))

extern const lookup_info ml_table_selection_mode[];
#define Val_selection_mode(data) ml_lookup_from_c (ml_table_selection_mode, data)
#define Selection_mode_val(key) ml_lookup_to_c (ml_table_selection_mode, key)

/* shadow_type : tags and macros */
#define MLTAG_IN	((value)(16357*2+1))
#define MLTAG_OUT	((value)(3947630*2+1))
#define MLTAG_ETCHED_IN	((value)(860859825*2+1))
#define MLTAG_ETCHED_OUT	((value)(845996322*2+1))

extern const lookup_info ml_table_shadow_type[];
#define Val_shadow_type(data) ml_lookup_from_c (ml_table_shadow_type, data)
#define Shadow_type_val(key) ml_lookup_to_c (ml_table_shadow_type, key)

/* state_type : tags and macros */
#define MLTAG_ACTIVE	((value)(-926356026*2+1))
#define MLTAG_PRELIGHT	((value)(992895443*2+1))
#define MLTAG_SELECTED	((value)(183679579*2+1))
#define MLTAG_INSENSITIVE	((value)(-1071513743*2+1))

extern const lookup_info ml_table_state_type[];
#define Val_state_type(data) ml_lookup_from_c (ml_table_state_type, data)
#define State_type_val(key) ml_lookup_to_c (ml_table_state_type, key)

extern const lookup_info ml_table_submenu_direction[];
#define Val_submenu_direction(data) ml_lookup_from_c (ml_table_submenu_direction, data)
#define Submenu_direction_val(key) ml_lookup_to_c (ml_table_submenu_direction, key)

/* submenu_placement : tags and macros */
#define MLTAG_TOP_BOTTOM	((value)(388303541*2+1))
#define MLTAG_LEFT_RIGHT	((value)(490510916*2+1))

extern const lookup_info ml_table_submenu_placement[];
#define Val_submenu_placement(data) ml_lookup_from_c (ml_table_submenu_placement, data)
#define Submenu_placement_val(key) ml_lookup_to_c (ml_table_submenu_placement, key)

/* toolbar_style : tags and macros */
#define MLTAG_ICONS	((value)(885381818*2+1))
#define MLTAG_TEXT	((value)(934974637*2+1))
#define MLTAG_BOTH_HORIZ	((value)(-947878242*2+1))

extern const lookup_info ml_table_toolbar_style[];
#define Val_toolbar_style(data) ml_lookup_from_c (ml_table_toolbar_style, data)
#define Toolbar_style_val(key) ml_lookup_to_c (ml_table_toolbar_style, key)

/* update_type : tags and macros */
#define MLTAG_CONTINUOUS	((value)(-803178225*2+1))
#define MLTAG_DISCONTINUOUS	((value)(-554379331*2+1))
#define MLTAG_DELAYED	((value)(268577410*2+1))

extern const lookup_info ml_table_update_type[];
#define Val_update_type(data) ml_lookup_from_c (ml_table_update_type, data)
#define Update_type_val(key) ml_lookup_to_c (ml_table_update_type, key)

/* visibility : tags and macros */
#define MLTAG_PARTIAL	((value)(-360666271*2+1))
#define MLTAG_FULL	((value)(780513679*2+1))

extern const lookup_info ml_table_visibility[];
#define Val_visibility(data) ml_lookup_from_c (ml_table_visibility, data)
#define Visibility_val(key) ml_lookup_to_c (ml_table_visibility, key)

/* window_position : tags and macros */
#define MLTAG_MOUSE	((value)(173231621*2+1))
#define MLTAG_CENTER_ALWAYS	((value)(-348647623*2+1))
#define MLTAG_CENTER_ON_PARENT	((value)(-315311776*2+1))

extern const lookup_info ml_table_window_position[];
#define Val_window_position(data) ml_lookup_from_c (ml_table_window_position, data)
#define Window_position_val(key) ml_lookup_to_c (ml_table_window_position, key)

/* window_type : tags and macros */
#define MLTAG_TOPLEVEL	((value)(109789903*2+1))
#define MLTAG_POPUP	((value)(-998030836*2+1))

extern const lookup_info ml_table_window_type[];
#define Val_window_type(data) ml_lookup_from_c (ml_table_window_type, data)
#define Window_type_val(key) ml_lookup_to_c (ml_table_window_type, key)

/* wrap_mode : tags and macros */
#define MLTAG_CHAR	((value)(746596054*2+1))
#define MLTAG_WORD	((value)(968739274*2+1))

extern const lookup_info ml_table_wrap_mode[];
#define Val_wrap_mode(data) ml_lookup_from_c (ml_table_wrap_mode, data)
#define Wrap_mode_val(key) ml_lookup_to_c (ml_table_wrap_mode, key)

/* sort_type : tags and macros */
#define MLTAG_ASCENDING	((value)(701500856*2+1))
#define MLTAG_DESCENDING	((value)(157124856*2+1))

extern const lookup_info ml_table_sort_type[];
#define Val_sort_type(data) ml_lookup_from_c (ml_table_sort_type, data)
#define Sort_type_val(key) ml_lookup_to_c (ml_table_sort_type, key)

/* cell_type : tags and macros */
#define MLTAG_EMPTY	((value)(-305630611*2+1))
#define MLTAG_PIXMAP	((value)(1060283293*2+1))
#define MLTAG_PIXTEXT	((value)(297800812*2+1))

extern const lookup_info ml_table_cell_type[];
#define Val_cell_type(data) ml_lookup_from_c (ml_table_cell_type, data)
#define Cell_type_val(key) ml_lookup_to_c (ml_table_cell_type, key)

/* toolbar_child : tags and macros */
#define MLTAG_SPACE	((value)(-11221850*2+1))
#define MLTAG_TOGGLEBUTTON	((value)(-374054778*2+1))
#define MLTAG_RADIOBUTTON	((value)(-752095987*2+1))

extern const lookup_info ml_table_toolbar_child[];
#define Val_toolbar_child(data) ml_lookup_from_c (ml_table_toolbar_child, data)
#define Toolbar_child_val(key) ml_lookup_to_c (ml_table_toolbar_child, key)

/* toolbar_space_style : tags and macros */
#define MLTAG_LINE	((value)(846454772*2+1))

extern const lookup_info ml_table_toolbar_space_style[];
#define Val_toolbar_space_style(data) ml_lookup_from_c (ml_table_toolbar_space_style, data)
#define Toolbar_space_style_val(key) ml_lookup_to_c (ml_table_toolbar_space_style, key)

/* spin_type : tags and macros */
#define MLTAG_HOME	((value)(802394655*2+1))
#define MLTAG_USER_DEFINED	((value)(-593067403*2+1))

extern const lookup_info ml_table_spin_type[];
#define Val_spin_type(data) ml_lookup_from_c (ml_table_spin_type, data)
#define Spin_type_val(key) ml_lookup_to_c (ml_table_spin_type, key)

/* accel_flag : tags and macros */
#define MLTAG_VISIBLE	((value)(586697810*2+1))
#define MLTAG_LOCKED	((value)(206156042*2+1))

extern const lookup_info ml_table_accel_flag[];
#define Val_accel_flag(data) ml_lookup_from_c (ml_table_accel_flag, data)
#define Accel_flag_val(key) ml_lookup_to_c (ml_table_accel_flag, key)

/* button_action : tags and macros */
#define MLTAG_SELECTS	((value)(39343575*2+1))
#define MLTAG_DRAGS	((value)(-576421409*2+1))
#define MLTAG_EXPANDS	((value)(535917753*2+1))

extern const lookup_info ml_table_button_action[];
#define Val_button_action(data) ml_lookup_from_c (ml_table_button_action, data)
#define Button_action_val(key) ml_lookup_to_c (ml_table_button_action, key)

/* calendar_display_options : tags and macros */
#define MLTAG_SHOW_HEADING	((value)(859563808*2+1))
#define MLTAG_SHOW_DAY_NAMES	((value)(-1059042013*2+1))
#define MLTAG_NO_MONTH_CHANGE	((value)(575696365*2+1))
#define MLTAG_SHOW_WEEK_NUMBERS	((value)(396996257*2+1))
#define MLTAG_WEEK_START_MONDAY	((value)(-284104936*2+1))

extern const lookup_info ml_table_calendar_display_options[];
#define Val_calendar_display_options(data) ml_lookup_from_c (ml_table_calendar_display_options, data)
#define Calendar_display_options_val(key) ml_lookup_to_c (ml_table_calendar_display_options, key)

/* progress_bar_style : tags and macros */
#define MLTAG_DISCRETE	((value)(115679673*2+1))

extern const lookup_info ml_table_progress_bar_style[];
#define Val_progress_bar_style(data) ml_lookup_from_c (ml_table_progress_bar_style, data)
#define Progress_bar_style_val(key) ml_lookup_to_c (ml_table_progress_bar_style, key)

/* progress_bar_orientation : tags and macros */
#define MLTAG_LEFT_TO_RIGHT	((value)(125505840*2+1))
#define MLTAG_RIGHT_TO_LEFT	((value)(-74637880*2+1))
#define MLTAG_BOTTOM_TO_TOP	((value)(871230597*2+1))
#define MLTAG_TOP_TO_BOTTOM	((value)(848974565*2+1))

extern const lookup_info ml_table_progress_bar_orientation[];
#define Val_progress_bar_orientation(data) ml_lookup_from_c (ml_table_progress_bar_orientation, data)
#define Progress_bar_orientation_val(key) ml_lookup_to_c (ml_table_progress_bar_orientation, key)

/* dest_defaults : tags and macros */
#define MLTAG_MOTION	((value)(-35638730*2+1))
#define MLTAG_HIGHLIGHT	((value)(-396835308*2+1))
#define MLTAG_DROP	((value)(758186031*2+1))

extern const lookup_info ml_table_dest_defaults[];
#define Val_dest_defaults(data) ml_lookup_from_c (ml_table_dest_defaults, data)
#define Dest_defaults_val(key) ml_lookup_to_c (ml_table_dest_defaults, key)

/* target_flags : tags and macros */
#define MLTAG_SAME_APP	((value)(-580135960*2+1))
#define MLTAG_SAME_WIDGET	((value)(-450430275*2+1))

extern const lookup_info ml_table_target_flags[];
#define Val_target_flags(data) ml_lookup_from_c (ml_table_target_flags, data)
#define Target_flags_val(key) ml_lookup_to_c (ml_table_target_flags, key)

/* spin_button_update_policy : tags and macros */
#define MLTAG_IF_VALID	((value)(-640747590*2+1))

extern const lookup_info ml_table_spin_button_update_policy[];
#define Val_spin_button_update_policy(data) ml_lookup_from_c (ml_table_spin_button_update_policy, data)
#define Spin_button_update_policy_val(key) ml_lookup_to_c (ml_table_spin_button_update_policy, key)

/* tree_view_mode : tags and macros */
#define MLTAG_ITEM	((value)(813731091*2+1))
/* text_window_type : tags and macros */
#define MLTAG_PRIVATE	((value)(155386083*2+1))

extern const lookup_info ml_table_text_window_type[];
#define Val_text_window_type(data) ml_lookup_from_c (ml_table_text_window_type, data)
#define Text_window_type_val(key) ml_lookup_to_c (ml_table_text_window_type, key)

/* text_search_flag : tags and macros */
#define MLTAG_VISIBLE_ONLY	((value)(-365153351*2+1))
#define MLTAG_TEXT_ONLY	((value)(-357645954*2+1))

extern const lookup_info ml_table_text_search_flag[];
#define Val_text_search_flag(data) ml_lookup_from_c (ml_table_text_search_flag, data)
#define Text_search_flag_val(key) ml_lookup_to_c (ml_table_text_search_flag, key)

/* tree_view_column_sizing : tags and macros */
#define MLTAG_GROW_ONLY	((value)(-657645480*2+1))
#define MLTAG_AUTOSIZE	((value)(505803696*2+1))
#define MLTAG_FIXED	((value)(-24104620*2+1))

extern const lookup_info ml_table_tree_view_column_sizing[];
#define Val_tree_view_column_sizing(data) ml_lookup_from_c (ml_table_tree_view_column_sizing, data)
#define Tree_view_column_sizing_val(key) ml_lookup_to_c (ml_table_tree_view_column_sizing, key)

/* cell_renderer_mode : tags and macros */
#define MLTAG_INERT	((value)(1006870658*2+1))
#define MLTAG_ACTIVATABLE	((value)(281666284*2+1))
#define MLTAG_EDITABLE	((value)(791385252*2+1))

extern const lookup_info ml_table_cell_renderer_mode[];
#define Val_cell_renderer_mode(data) ml_lookup_from_c (ml_table_cell_renderer_mode, data)
#define Cell_renderer_mode_val(key) ml_lookup_to_c (ml_table_cell_renderer_mode, key)

/* message_type : tags and macros */
#define MLTAG_INFO	((value)(813432942*2+1))
#define MLTAG_WARNING	((value)(161459772*2+1))
#define MLTAG_QUESTION	((value)(3549990*2+1))
#define MLTAG_ERROR	((value)(-250084440*2+1))

extern const lookup_info ml_table_message_type[];
#define Val_message_type(data) ml_lookup_from_c (ml_table_message_type, data)
#define Message_type_val(key) ml_lookup_to_c (ml_table_message_type, key)

/* buttons_type : tags and macros */
#define MLTAG_OK	((value)(17692*2+1))
#define MLTAG_CLOSE	((value)(-967749736*2+1))
#define MLTAG_CANCEL	((value)(-357131910*2+1))
#define MLTAG_YES_NO	((value)(126759865*2+1))
#define MLTAG_OK_CANCEL	((value)(863273341*2+1))

extern const lookup_info ml_table_buttons_type[];
#define Val_buttons_type(data) ml_lookup_from_c (ml_table_buttons_type, data)
#define Buttons_type_val(key) ml_lookup_to_c (ml_table_buttons_type, key)

/* response : tags and macros */
#define MLTAG_REJECT	((value)(889716063*2+1))
#define MLTAG_ACCEPT	((value)(1032404744*2+1))
#define MLTAG_DELETE_EVENT	((value)(225027494*2+1))
#define MLTAG_YES	((value)(4441351*2+1))
#define MLTAG_NO	((value)(17473*2+1))
#define MLTAG_APPLY	((value)(573160782*2+1))
#define MLTAG_HELP	((value)(801897153*2+1))

extern const lookup_info ml_table_response[];
#define Val_response(data) ml_lookup_from_c (ml_table_response, data)
#define Response_val(key) ml_lookup_to_c (ml_table_response, key)

/* widget_flags : tags and macros */
#define MLTAG_IN_DESTRUCTION	((value)(-69065672*2+1))
#define MLTAG_FLOATING	((value)(925188294*2+1))
#define MLTAG_NO_WINDOW	((value)(-542800018*2+1))
#define MLTAG_REALIZED	((value)(-140692242*2+1))
#define MLTAG_MAPPED	((value)(-341540941*2+1))
#define MLTAG_SENSITIVE	((value)(-368778250*2+1))
#define MLTAG_PARENT_SENSITIVE	((value)(-766711455*2+1))
#define MLTAG_CAN_FOCUS	((value)(-454655255*2+1))
#define MLTAG_HAS_FOCUS	((value)(-746004045*2+1))
#define MLTAG_CAN_DEFAULT	((value)(670860818*2+1))
#define MLTAG_HAS_DEFAULT	((value)(-888427684*2+1))
#define MLTAG_HAS_GRAB	((value)(922740273*2+1))
#define MLTAG_RC_STYLE	((value)(842647235*2+1))
#define MLTAG_COMPOSITE_CHILD	((value)(353636324*2+1))
#define MLTAG_NO_REPARENT	((value)(164095003*2+1))
#define MLTAG_APP_PAINTABLE	((value)(-396567654*2+1))
#define MLTAG_RECEIVES_DEFAULT	((value)(-848065710*2+1))
#define MLTAG_DOUBLE_BUFFERED	((value)(650845069*2+1))

extern const lookup_info ml_table_widget_flags[];
#define Val_widget_flags(data) ml_lookup_from_c (ml_table_widget_flags, data)
#define Widget_flags_val(key) ml_lookup_to_c (ml_table_widget_flags, key)

/* image_type : tags and macros */
#define MLTAG_IMAGE	((value)(995579707*2+1))
#define MLTAG_PIXBUF	((value)(1059740724*2+1))
#define MLTAG_STOCK	((value)(33832630*2+1))
#define MLTAG_ICON_SET	((value)(-268575620*2+1))
#define MLTAG_ANIMATION	((value)(-963813660*2+1))

extern const lookup_info ml_table_image_type[];
#define Val_image_type(data) ml_lookup_from_c (ml_table_image_type, data)
#define Image_type_val(key) ml_lookup_to_c (ml_table_image_type, key)

extern const lookup_info ml_table_size_group_mode[];
#define Val_size_group_mode(data) ml_lookup_from_c (ml_table_size_group_mode, data)
#define Size_group_mode_val(key) ml_lookup_to_c (ml_table_size_group_mode, key)

/* file_chooser_action : tags and macros */
#define MLTAG_OPEN	((value)(880069578*2+1))
#define MLTAG_SAVE	((value)(923685693*2+1))
#define MLTAG_SELECT_FOLDER	((value)(6390993*2+1))
#define MLTAG_CREATE_FOLDER	((value)(468105425*2+1))

extern const lookup_info ml_table_file_chooser_action[];
#define Val_file_chooser_action(data) ml_lookup_from_c (ml_table_file_chooser_action, data)
#define File_chooser_action_val(key) ml_lookup_to_c (ml_table_file_chooser_action, key)

/* tree_model_flags : tags and macros */
#define MLTAG_ITERS_PERSIST	((value)(-339514704*2+1))
#define MLTAG_LIST_ONLY	((value)(-402526163*2+1))

extern const lookup_info ml_table_tree_model_flags[];
#define Val_tree_model_flags(data) ml_lookup_from_c (ml_table_tree_model_flags, data)
#define Tree_model_flags_val(key) ml_lookup_to_c (ml_table_tree_model_flags, key)

/* tree_view_drop_position : tags and macros */
#define MLTAG_BEFORE	((value)(-860553089*2+1))
#define MLTAG_AFTER	((value)(462462460*2+1))
#define MLTAG_INTO_OR_BEFORE	((value)(486315964*2+1))
#define MLTAG_INTO_OR_AFTER	((value)(-475234977*2+1))

extern const lookup_info ml_table_tree_view_drop_position[];
#define Val_tree_view_drop_position(data) ml_lookup_from_c (ml_table_tree_view_drop_position, data)
#define Tree_view_drop_position_val(key) ml_lookup_to_c (ml_table_tree_view_drop_position, key)

/* file_filter_flags : tags and macros */
#define MLTAG_FILENAME	((value)(-789594425*2+1))
#define MLTAG_URI	((value)(4245324*2+1))
#define MLTAG_DISPLAY_NAME	((value)(6085832*2+1))
#define MLTAG_MIME_TYPE	((value)(-982608795*2+1))

extern const lookup_info ml_table_file_filter_flags[];
#define Val_file_filter_flags(data) ml_lookup_from_c (ml_table_file_filter_flags, data)
#define File_filter_flags_val(key) ml_lookup_to_c (ml_table_file_filter_flags, key)

/* ui_manager_item_type : tags and macros */
#define MLTAG_AUTO	((value)(725067631*2+1))
#define MLTAG_MENUBAR	((value)(976702836*2+1))
#define MLTAG_TOOLBAR	((value)(-363671205*2+1))
#define MLTAG_PLACEHOLDER	((value)(288325459*2+1))
#define MLTAG_MENUITEM	((value)(987452978*2+1))
#define MLTAG_TOOLITEM	((value)(584268907*2+1))
#define MLTAG_SEPARATOR	((value)(752341061*2+1))
#define MLTAG_ACCELERATOR	((value)(-53513845*2+1))

extern const lookup_info ml_table_ui_manager_item_type[];
#define Val_ui_manager_item_type(data) ml_lookup_from_c (ml_table_ui_manager_item_type, data)
#define Ui_manager_item_type_val(key) ml_lookup_to_c (ml_table_ui_manager_item_type, key)

/* assistant_page_type : tags and macros */
#define MLTAG_CONTENT	((value)(424370457*2+1))
#define MLTAG_INTRO	((value)(1007616588*2+1))
#define MLTAG_CONFIRM	((value)(269316320*2+1))
#define MLTAG_SUMMARY	((value)(12377862*2+1))
#define MLTAG_PROGRESS	((value)(-542079059*2+1))

extern const lookup_info ml_table_assistant_page_type[];
#define Val_assistant_page_type(data) ml_lookup_from_c (ml_table_assistant_page_type, data)
#define Assistant_page_type_val(key) ml_lookup_to_c (ml_table_assistant_page_type, key)

/* cell_renderer_accel_mode : tags and macros */
#define MLTAG_OTHER	((value)(879009456*2+1))

extern const lookup_info ml_table_cell_renderer_accel_mode[];
#define Val_cell_renderer_accel_mode(data) ml_lookup_from_c (ml_table_cell_renderer_accel_mode, data)
#define Cell_renderer_accel_mode_val(key) ml_lookup_to_c (ml_table_cell_renderer_accel_mode, key)

/* file_chooser_confirmation : tags and macros */
#define MLTAG_ACCEPT_FILENAME	((value)(934303262*2+1))
#define MLTAG_SELECT_AGAIN	((value)(864149629*2+1))

extern const lookup_info ml_table_file_chooser_confirmation[];
#define Val_file_chooser_confirmation(data) ml_lookup_from_c (ml_table_file_chooser_confirmation, data)
#define File_chooser_confirmation_val(key) ml_lookup_to_c (ml_table_file_chooser_confirmation, key)

