/* platform : tags and macros */
#define MLTAG_X11	((value)(4387128*2+1))
#define MLTAG_WIN32	((value)(-933730405*2+1))
#define MLTAG_QUARTZ	((value)(-13833829*2+1))
/* event_type : tags and macros */
#define MLTAG_NOTHING	((value)(-818712595*2+1))
#define MLTAG_DELETE	((value)(492530731*2+1))
#define MLTAG_DESTROY	((value)(609878234*2+1))
#define MLTAG_EXPOSE	((value)(-150979004*2+1))
#define MLTAG_MOTION_NOTIFY	((value)(1002486002*2+1))
#define MLTAG_BUTTON_PRESS	((value)(559079958*2+1))
#define MLTAG_TWO_BUTTON_PRESS	((value)(344913929*2+1))
#define MLTAG_THREE_BUTTON_PRESS	((value)(-820667913*2+1))
#define MLTAG_BUTTON_RELEASE	((value)(-463011046*2+1))
#define MLTAG_KEY_PRESS	((value)(-291069597*2+1))
#define MLTAG_KEY_RELEASE	((value)(-39653465*2+1))
#define MLTAG_ENTER_NOTIFY	((value)(347669104*2+1))
#define MLTAG_LEAVE_NOTIFY	((value)(773096561*2+1))
#define MLTAG_FOCUS_CHANGE	((value)(6790743*2+1))
#define MLTAG_CONFIGURE	((value)(1001679302*2+1))
#define MLTAG_MAP	((value)(3843708*2+1))
#define MLTAG_UNMAP	((value)(618174915*2+1))
#define MLTAG_PROPERTY_NOTIFY	((value)(1017095347*2+1))
#define MLTAG_SELECTION_CLEAR	((value)(-1043152422*2+1))
#define MLTAG_SELECTION_REQUEST	((value)(111837404*2+1))
#define MLTAG_SELECTION_NOTIFY	((value)(10997180*2+1))
#define MLTAG_PROXIMITY_IN	((value)(-953066939*2+1))
#define MLTAG_PROXIMITY_OUT	((value)(67253774*2+1))
#define MLTAG_DRAG_ENTER	((value)(498749229*2+1))
#define MLTAG_DRAG_LEAVE	((value)(528946956*2+1))
#define MLTAG_DRAG_MOTION	((value)(730822241*2+1))
#define MLTAG_DRAG_STATUS	((value)(-433203363*2+1))
#define MLTAG_DROP_START	((value)(-935149326*2+1))
#define MLTAG_DROP_FINISHED	((value)(679812290*2+1))
#define MLTAG_CLIENT_EVENT	((value)(695404486*2+1))
#define MLTAG_VISIBILITY_NOTIFY	((value)(-627886890*2+1))
#define MLTAG_NO_EXPOSE	((value)(-778664510*2+1))
#define MLTAG_SCROLL	((value)(-102267891*2+1))
#define MLTAG_WINDOW_STATE	((value)(-58144478*2+1))
#define MLTAG_SETTING	((value)(662418800*2+1))

extern const lookup_info ml_table_event_type[];
#define Val_event_type(data) ml_lookup_from_c (ml_table_event_type, data)
#define Event_type_val(key) ml_lookup_to_c (ml_table_event_type, key)

/* event_mask : tags and macros */
#define MLTAG_EXPOSURE	((value)(-431242489*2+1))
#define MLTAG_POINTER_MOTION	((value)(300690648*2+1))
#define MLTAG_POINTER_MOTION_HINT	((value)(-362125938*2+1))
#define MLTAG_BUTTON_MOTION	((value)(600815651*2+1))
#define MLTAG_BUTTON1_MOTION	((value)(820800438*2+1))
#define MLTAG_BUTTON2_MOTION	((value)(-367075883*2+1))
#define MLTAG_BUTTON3_MOTION	((value)(592531444*2+1))
#define MLTAG_STRUCTURE	((value)(-1029192685*2+1))
#define MLTAG_PROPERTY_CHANGE	((value)(-1034331302*2+1))
#define MLTAG_SUBSTRUCTURE	((value)(-255536461*2+1))
#define MLTAG_ALL_EVENTS	((value)(619809239*2+1))

extern const lookup_info ml_table_event_mask[];
#define Val_event_mask(data) ml_lookup_from_c (ml_table_event_mask, data)
#define Event_mask_val(key) ml_lookup_to_c (ml_table_event_mask, key)

/* extension_mode : tags and macros */
#define MLTAG_NONE	((value)(868932280*2+1))
#define MLTAG_ALL	((value)(3249409*2+1))
#define MLTAG_CURSOR	((value)(-244630826*2+1))

extern const lookup_info ml_table_extension_mode[];
#define Val_extension_mode(data) ml_lookup_from_c (ml_table_extension_mode, data)
#define Extension_mode_val(key) ml_lookup_to_c (ml_table_extension_mode, key)

/* gdkVisibilityState : tags and macros */
#define MLTAG_UNOBSCURED	((value)(-112785096*2+1))
#define MLTAG_PARTIAL	((value)(-360666271*2+1))
#define MLTAG_FULLY_OBSCURED	((value)(665041940*2+1))

extern const lookup_info ml_table_gdkVisibilityState[];
#define Val_gdkVisibilityState(data) ml_lookup_from_c (ml_table_gdkVisibilityState, data)
#define GdkVisibilityState_val(key) ml_lookup_to_c (ml_table_gdkVisibilityState, key)

/* gdkInputSource : tags and macros */
#define MLTAG_MOUSE	((value)(173231621*2+1))
#define MLTAG_PEN	((value)(3993785*2+1))
#define MLTAG_ERASER	((value)(-122581812*2+1))

extern const lookup_info ml_table_gdkInputSource[];
#define Val_gdkInputSource(data) ml_lookup_from_c (ml_table_gdkInputSource, data)
#define GdkInputSource_val(key) ml_lookup_to_c (ml_table_gdkInputSource, key)

/* gdkScrollDirection : tags and macros */
#define MLTAG_UP	((value)(19035*2+1))
#define MLTAG_DOWN	((value)(758038626*2+1))
#define MLTAG_LEFT	((value)(846254087*2+1))
#define MLTAG_RIGHT	((value)(-414039108*2+1))

extern const lookup_info ml_table_gdkScrollDirection[];
#define Val_gdkScrollDirection(data) ml_lookup_from_c (ml_table_gdkScrollDirection, data)
#define GdkScrollDirection_val(key) ml_lookup_to_c (ml_table_gdkScrollDirection, key)

/* gdkCrossingMode : tags and macros */
#define MLTAG_NORMAL	((value)(-487842265*2+1))
#define MLTAG_GRAB	((value)(791451596*2+1))
#define MLTAG_UNGRAB	((value)(348357285*2+1))

extern const lookup_info ml_table_gdkCrossingMode[];
#define Val_gdkCrossingMode(data) ml_lookup_from_c (ml_table_gdkCrossingMode, data)
#define GdkCrossingMode_val(key) ml_lookup_to_c (ml_table_gdkCrossingMode, key)

/* gdkNotifyType : tags and macros */
#define MLTAG_ANCESTOR	((value)(-318434221*2+1))
#define MLTAG_VIRTUAL	((value)(384135659*2+1))
#define MLTAG_INFERIOR	((value)(-386323714*2+1))
#define MLTAG_NONLINEAR	((value)(919364242*2+1))
#define MLTAG_NONLINEAR_VIRTUAL	((value)(-347467778*2+1))
#define MLTAG_UNKNOWN	((value)(-514918550*2+1))

extern const lookup_info ml_table_gdkNotifyType[];
#define Val_gdkNotifyType(data) ml_lookup_from_c (ml_table_gdkNotifyType, data)
#define GdkNotifyType_val(key) ml_lookup_to_c (ml_table_gdkNotifyType, key)

/* gdkSettingAction : tags and macros */
#define MLTAG_NEW	((value)(3894336*2+1))
#define MLTAG_CHANGED	((value)(-861895468*2+1))
#define MLTAG_DELETED	((value)(312687033*2+1))

extern const lookup_info ml_table_gdkSettingAction[];
#define Val_gdkSettingAction(data) ml_lookup_from_c (ml_table_gdkSettingAction, data)
#define GdkSettingAction_val(key) ml_lookup_to_c (ml_table_gdkSettingAction, key)

/* gdkWindowState : tags and macros */
#define MLTAG_WITHDRAWN	((value)(-875643900*2+1))
#define MLTAG_ICONIFIED	((value)(-625114350*2+1))
#define MLTAG_MAXIMIZED	((value)(-96895496*2+1))
#define MLTAG_STICKY	((value)(1035688233*2+1))
#define MLTAG_FULLSCREEN	((value)(-339890629*2+1))

extern const lookup_info ml_table_gdkWindowState[];
#define Val_gdkWindowState(data) ml_lookup_from_c (ml_table_gdkWindowState, data)
#define GdkWindowState_val(key) ml_lookup_to_c (ml_table_gdkWindowState, key)

/* fill_rule : tags and macros */
#define MLTAG_EVEN_ODD_RULE	((value)(-914835983*2+1))
#define MLTAG_WINDING_RULE	((value)(-32246367*2+1))

extern const lookup_info ml_table_fill_rule[];
#define Val_fill_rule(data) ml_lookup_from_c (ml_table_fill_rule, data)
#define Fill_rule_val(key) ml_lookup_to_c (ml_table_fill_rule, key)

/* overlap_type : tags and macros */
#define MLTAG_IN	((value)(16357*2+1))
#define MLTAG_OUT	((value)(3947630*2+1))
#define MLTAG_PART	((value)(890416115*2+1))

extern const lookup_info ml_table_overlap_type[];
#define Val_overlap_type(data) ml_lookup_from_c (ml_table_overlap_type, data)
#define Overlap_type_val(key) ml_lookup_to_c (ml_table_overlap_type, key)

/* function_type : tags and macros */
#define MLTAG_COPY	((value)(746947509*2+1))
#define MLTAG_INVERT	((value)(-765750506*2+1))
#define MLTAG_XOR	((value)(4393851*2+1))

extern const lookup_info ml_table_function_type[];
#define Val_function_type(data) ml_lookup_from_c (ml_table_function_type, data)
#define Function_type_val(key) ml_lookup_to_c (ml_table_function_type, key)

/* fill : tags and macros */
#define MLTAG_SOLID	((value)(-21763061*2+1))
#define MLTAG_TILED	((value)(237188438*2+1))
#define MLTAG_STIPPLED	((value)(631025699*2+1))
#define MLTAG_OPAQUE_STIPPLED	((value)(-564102527*2+1))

extern const lookup_info ml_table_fill[];
#define Val_fill(data) ml_lookup_from_c (ml_table_fill, data)
#define Fill_val(key) ml_lookup_to_c (ml_table_fill, key)

/* subwindow_mode : tags and macros */
#define MLTAG_CLIP_BY_CHILDREN	((value)(917295288*2+1))
#define MLTAG_INCLUDE_INFERIORS	((value)(-102261922*2+1))

extern const lookup_info ml_table_subwindow_mode[];
#define Val_subwindow_mode(data) ml_lookup_from_c (ml_table_subwindow_mode, data)
#define Subwindow_mode_val(key) ml_lookup_to_c (ml_table_subwindow_mode, key)

/* line_style : tags and macros */
#define MLTAG_ON_OFF_DASH	((value)(92914146*2+1))
#define MLTAG_DOUBLE_DASH	((value)(711795840*2+1))

extern const lookup_info ml_table_line_style[];
#define Val_line_style(data) ml_lookup_from_c (ml_table_line_style, data)
#define Line_style_val(key) ml_lookup_to_c (ml_table_line_style, key)

/* cap_style : tags and macros */
#define MLTAG_NOT_LAST	((value)(1039640674*2+1))
#define MLTAG_BUTT	((value)(736157203*2+1))
#define MLTAG_ROUND	((value)(-346804178*2+1))
#define MLTAG_PROJECTING	((value)(915733417*2+1))

extern const lookup_info ml_table_cap_style[];
#define Val_cap_style(data) ml_lookup_from_c (ml_table_cap_style, data)
#define Cap_style_val(key) ml_lookup_to_c (ml_table_cap_style, key)

/* join_style : tags and macros */
#define MLTAG_MITER	((value)(106641381*2+1))
#define MLTAG_BEVEL	((value)(776962138*2+1))

extern const lookup_info ml_table_join_style[];
#define Val_join_style(data) ml_lookup_from_c (ml_table_join_style, data)
#define Join_style_val(key) ml_lookup_to_c (ml_table_join_style, key)

/* gdkModifier : tags and macros */
#define MLTAG_SHIFT	((value)(-99539870*2+1))
#define MLTAG_LOCK	((value)(846750699*2+1))
#define MLTAG_CONTROL	((value)(425017149*2+1))
#define MLTAG_MOD1	((value)(857840463*2+1))
#define MLTAG_MOD2	((value)(857840464*2+1))
#define MLTAG_MOD3	((value)(857840465*2+1))
#define MLTAG_MOD4	((value)(857840466*2+1))
#define MLTAG_MOD5	((value)(857840467*2+1))
#define MLTAG_BUTTON1	((value)(-901175809*2+1))
#define MLTAG_BUTTON2	((value)(-901175808*2+1))
#define MLTAG_BUTTON3	((value)(-901175807*2+1))
#define MLTAG_BUTTON4	((value)(-901175806*2+1))
#define MLTAG_BUTTON5	((value)(-901175805*2+1))

extern const lookup_info ml_table_gdkModifier[];
#define Val_gdkModifier(data) ml_lookup_from_c (ml_table_gdkModifier, data)
#define GdkModifier_val(key) ml_lookup_to_c (ml_table_gdkModifier, key)

/* gdkImageType : tags and macros */
#define MLTAG_SHARED	((value)(-810677595*2+1))
#define MLTAG_FASTEST	((value)(-569531638*2+1))

extern const lookup_info ml_table_gdkImageType[];
#define Val_gdkImageType(data) ml_lookup_from_c (ml_table_gdkImageType, data)
#define GdkImageType_val(key) ml_lookup_to_c (ml_table_gdkImageType, key)

/* gdkVisualType : tags and macros */
#define MLTAG_STATIC_GRAY	((value)(1009448020*2+1))
#define MLTAG_GRAYSCALE	((value)(-292554841*2+1))
#define MLTAG_STATIC_COLOR	((value)(433926066*2+1))
#define MLTAG_PSEUDO_COLOR	((value)(-52356478*2+1))
#define MLTAG_TRUE_COLOR	((value)(553504338*2+1))
#define MLTAG_DIRECT_COLOR	((value)(178400365*2+1))

extern const lookup_info ml_table_gdkVisualType[];
#define Val_gdkVisualType(data) ml_lookup_from_c (ml_table_gdkVisualType, data)
#define GdkVisualType_val(key) ml_lookup_to_c (ml_table_gdkVisualType, key)

/* font_type : tags and macros */
#define MLTAG_FONT	((value)(780215759*2+1))
#define MLTAG_FONTSET	((value)(370564371*2+1))

extern const lookup_info ml_table_font_type[];
#define Val_font_type(data) ml_lookup_from_c (ml_table_font_type, data)
#define Font_type_val(key) ml_lookup_to_c (ml_table_font_type, key)

/* gdkDragAction : tags and macros */
#define MLTAG_DEFAULT	((value)(462924961*2+1))
#define MLTAG_MOVE	((value)(857844497*2+1))
#define MLTAG_LINK	((value)(846454778*2+1))
#define MLTAG_PRIVATE	((value)(155386083*2+1))
#define MLTAG_ASK	((value)(3250969*2+1))

extern const lookup_info ml_table_gdkDragAction[];
#define Val_gdkDragAction(data) ml_lookup_from_c (ml_table_gdkDragAction, data)
#define GdkDragAction_val(key) ml_lookup_to_c (ml_table_gdkDragAction, key)

/* gdkRgbDither : tags and macros */
#define MLTAG_MAX	((value)(3843716*2+1))

extern const lookup_info ml_table_gdkRgbDither[];
#define Val_gdkRgbDither(data) ml_lookup_from_c (ml_table_gdkRgbDither, data)
#define GdkRgbDither_val(key) ml_lookup_to_c (ml_table_gdkRgbDither, key)

/* xdata : tags and macros */
#define MLTAG_BYTES	((value)(998654027*2+1))
#define MLTAG_SHORTS	((value)(-655420297*2+1))
#define MLTAG_INT32S	((value)(-788831899*2+1))

extern const lookup_info ml_table_xdata[];
#define Val_xdata(data) ml_lookup_from_c (ml_table_xdata, data)
#define Xdata_val(key) ml_lookup_to_c (ml_table_xdata, key)

/* property_state : tags and macros */
#define MLTAG_NEW_VALUE	((value)(-41936174*2+1))

extern const lookup_info ml_table_property_state[];
#define Val_property_state(data) ml_lookup_from_c (ml_table_property_state, data)
#define Property_state_val(key) ml_lookup_to_c (ml_table_property_state, key)

/* property_mode : tags and macros */
#define MLTAG_REPLACE	((value)(721165332*2+1))
#define MLTAG_PREPEND	((value)(934570734*2+1))
#define MLTAG_APPEND	((value)(-1034514982*2+1))

extern const lookup_info ml_table_property_mode[];
#define Val_property_mode(data) ml_lookup_from_c (ml_table_property_mode, data)
#define Property_mode_val(key) ml_lookup_to_c (ml_table_property_mode, key)

/* gravity : tags and macros */
#define MLTAG_NORTH_WEST	((value)(-589513399*2+1))
#define MLTAG_NORTH	((value)(498572453*2+1))
#define MLTAG_NORTH_EAST	((value)(-789324521*2+1))
#define MLTAG_WEST	((value)(968242223*2+1))
#define MLTAG_CENTER	((value)(945672661*2+1))
#define MLTAG_EAST	((value)(768431101*2+1))
#define MLTAG_SOUTH_WEST	((value)(-907515135*2+1))
#define MLTAG_SOUTH	((value)(-21313043*2+1))
#define MLTAG_SOUTH_EAST	((value)(1040157391*2+1))
#define MLTAG_STATIC	((value)(947816622*2+1))

extern const lookup_info ml_table_gravity[];
#define Val_gravity(data) ml_lookup_from_c (ml_table_gravity, data)
#define Gravity_val(key) ml_lookup_to_c (ml_table_gravity, key)

/* window_type_hint : tags and macros */
#define MLTAG_DIALOG	((value)(-474631992*2+1))
#define MLTAG_MENU	((value)(857345439*2+1))
#define MLTAG_TOOLBAR	((value)(-363671205*2+1))
#define MLTAG_SPLASHSCREEN	((value)(-214066157*2+1))
#define MLTAG_UTILITY	((value)(525269836*2+1))
#define MLTAG_DOCK	((value)(758034163*2+1))
#define MLTAG_DESKTOP	((value)(510171580*2+1))

extern const lookup_info ml_table_window_type_hint[];
#define Val_window_type_hint(data) ml_lookup_from_c (ml_table_window_type_hint, data)
#define Window_type_hint_val(key) ml_lookup_to_c (ml_table_window_type_hint, key)

/* gdkCursorType : tags and macros */
#define MLTAG_X_CURSOR	((value)(154765085*2+1))
#define MLTAG_ARROW	((value)(595440041*2+1))
#define MLTAG_BASED_ARROW_DOWN	((value)(395671268*2+1))
#define MLTAG_BASED_ARROW_UP	((value)(643103837*2+1))
#define MLTAG_BOAT	((value)(735854592*2+1))
#define MLTAG_BOGOSITY	((value)(724821840*2+1))
#define MLTAG_BOTTOM_LEFT_CORNER	((value)(-840121927*2+1))
#define MLTAG_BOTTOM_RIGHT_CORNER	((value)(-544591284*2+1))
#define MLTAG_BOTTOM_SIDE	((value)(349596331*2+1))
#define MLTAG_BOTTOM_TEE	((value)(-1067310304*2+1))
#define MLTAG_BOX_SPIRAL	((value)(866974533*2+1))
#define MLTAG_CENTER_PTR	((value)(-847502556*2+1))
#define MLTAG_CIRCLE	((value)(143662608*2+1))
#define MLTAG_CLOCK	((value)(-967753298*2+1))
#define MLTAG_COFFEE_MUG	((value)(-17670868*2+1))
#define MLTAG_CROSS	((value)(-901212320*2+1))
#define MLTAG_CROSS_REVERSE	((value)(642211299*2+1))
#define MLTAG_CROSSHAIR	((value)(102807170*2+1))
#define MLTAG_DIAMOND_CROSS	((value)(760772821*2+1))
#define MLTAG_DOT	((value)(3399273*2+1))
#define MLTAG_DOTBOX	((value)(-458972382*2+1))
#define MLTAG_DOUBLE_ARROW	((value)(-971312453*2+1))
#define MLTAG_DRAFT_LARGE	((value)(346122909*2+1))
#define MLTAG_DRAFT_SMALL	((value)(609298345*2+1))
#define MLTAG_DRAPED_BOX	((value)(645695112*2+1))
#define MLTAG_EXCHANGE	((value)(535745059*2+1))
#define MLTAG_FLEUR	((value)(8222812*2+1))
#define MLTAG_GOBBLER	((value)(-890128687*2+1))
#define MLTAG_GUMBY	((value)(433912310*2+1))
#define MLTAG_HAND1	((value)(537660898*2+1))
#define MLTAG_HAND2	((value)(537660899*2+1))
#define MLTAG_HEART	((value)(581375846*2+1))
#define MLTAG_ICON	((value)(812887929*2+1))
#define MLTAG_IRON_CROSS	((value)(-19196439*2+1))
#define MLTAG_LEFT_PTR	((value)(-907074986*2+1))
#define MLTAG_LEFT_SIDE	((value)(-381540337*2+1))
#define MLTAG_LEFT_TEE	((value)(-906879428*2+1))
#define MLTAG_LEFTBUTTON	((value)(-966386375*2+1))
#define MLTAG_LL_ANGLE	((value)(-267328140*2+1))
#define MLTAG_LR_ANGLE	((value)(-376328710*2+1))
#define MLTAG_MAN	((value)(3843706*2+1))
#define MLTAG_MIDDLEBUTTON	((value)(-561104121*2+1))
#define MLTAG_PENCIL	((value)(-353067059*2+1))
#define MLTAG_PIRATE	((value)(993153369*2+1))
#define MLTAG_PLUS	((value)(890963802*2+1))
#define MLTAG_QUESTION_ARROW	((value)(-214104944*2+1))
#define MLTAG_RIGHT_PTR	((value)(-812837749*2+1))
#define MLTAG_RIGHT_SIDE	((value)(-841472966*2+1))
#define MLTAG_RIGHT_TEE	((value)(-812642191*2+1))
#define MLTAG_RIGHTBUTTON	((value)(-452147538*2+1))
#define MLTAG_RTL_LOGO	((value)(1071412416*2+1))
#define MLTAG_SAILBOAT	((value)(950785137*2+1))
#define MLTAG_SB_DOWN_ARROW	((value)(-894963780*2+1))
#define MLTAG_SB_H_DOUBLE_ARROW	((value)(35713346*2+1))
#define MLTAG_SB_LEFT_ARROW	((value)(1018480929*2+1))
#define MLTAG_SB_RIGHT_ARROW	((value)(-135609674*2+1))
#define MLTAG_SB_UP_ARROW	((value)(1052745333*2+1))
#define MLTAG_SB_V_DOUBLE_ARROW	((value)(664502388*2+1))
#define MLTAG_SHUTTLE	((value)(-302205415*2+1))
#define MLTAG_SIZING	((value)(-208394178*2+1))
#define MLTAG_SPIDER	((value)(-266222555*2+1))
#define MLTAG_SPRAYCAN	((value)(-77171517*2+1))
#define MLTAG_STAR	((value)(924625874*2+1))
#define MLTAG_TARGET	((value)(963616593*2+1))
#define MLTAG_TCROSS	((value)(-532486516*2+1))
#define MLTAG_TOP_LEFT_ARROW	((value)(520760571*2+1))
#define MLTAG_TOP_LEFT_CORNER	((value)(476010563*2+1))
#define MLTAG_TOP_RIGHT_CORNER	((value)(895177858*2+1))
#define MLTAG_TOP_SIDE	((value)(6252257*2+1))
#define MLTAG_TOP_TEE	((value)(173416362*2+1))
#define MLTAG_TREK	((value)(935616868*2+1))
#define MLTAG_UL_ANGLE	((value)(-220796789*2+1))
#define MLTAG_UMBRELLA	((value)(-940333884*2+1))
#define MLTAG_UR_ANGLE	((value)(-329797359*2+1))
#define MLTAG_WATCH	((value)(-1022144977*2+1))
#define MLTAG_XTERM	((value)(-486695996*2+1))

extern const lookup_info ml_table_gdkCursorType[];
#define Val_gdkCursorType(data) ml_lookup_from_c (ml_table_gdkCursorType, data)
#define GdkCursorType_val(key) ml_lookup_to_c (ml_table_gdkCursorType, key)

