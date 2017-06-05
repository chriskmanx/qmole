(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: Summer, 2008
//
(* ****** ****** *)

%{#
#include "contrib/GL/CATS/glut.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload "contrib/GL/SATS/gl.sats"

(* ****** ****** *)

(*
** GLUT API macro definitions -- the special key codes:
*)

macdef GLUT_KEY_F1 = $extval (GLenum, "GLUT_KEY_F1")
macdef GLUT_KEY_F2 = $extval (GLenum, "GLUT_KEY_F2")
macdef GLUT_KEY_F3 = $extval (GLenum, "GLUT_KEY_F3")
macdef GLUT_KEY_F4 = $extval (GLenum, "GLUT_KEY_F4")
macdef GLUT_KEY_F5 = $extval (GLenum, "GLUT_KEY_F5")
macdef GLUT_KEY_F6 = $extval (GLenum, "GLUT_KEY_F6")
macdef GLUT_KEY_F7 = $extval (GLenum, "GLUT_KEY_F7")
macdef GLUT_KEY_F8 = $extval (GLenum, "GLUT_KEY_F8")
macdef GLUT_KEY_F9 = $extval (GLenum, "GLUT_KEY_F9")
macdef GLUT_KEY_F10 = $extval (GLenum, "GLUT_KEY_F10")
macdef GLUT_KEY_F11 = $extval (GLenum, "GLUT_KEY_F11")
macdef GLUT_KEY_F12 = $extval (GLenum, "GLUT_KEY_F12")
macdef GLUT_KEY_LEFT = $extval (GLenum, "GLUT_KEY_LEFT")
macdef GLUT_KEY_RIGHT = $extval (GLenum, "GLUT_KEY_RIGHT")
macdef GLUT_KEY_UP = $extval (GLenum, "GLUT_KEY_UP")
macdef GLUT_KEY_DOWN = $extval (GLenum, "GLUT_KEY_DOWN")
macdef GLUT_KEY_PAGE_UP = $extval (GLenum, "GLUT_KEY_PAGE_UP")
macdef GLUT_KEY_PAGE_DOWN = $extval (GLenum, "GLUT_KEY_PAGE_DOWN")
macdef GLUT_KEY_HOME = $extval (GLenum, "GLUT_KEY_HOME")
macdef GLUT_KEY_END = $extval (GLenum, "GLUT_KEY_END")
macdef GLUT_KEY_INSERT = $extval (GLenum, "GLUT_KEY_INSERT")

(* ****** ****** *)

(*
** GLUT API macro definitions -- mouse state definitions
*)
macdef GLUT_LEFT_BUTTON = $extval (GLenum, "GLUT_LEFT_BUTTON")
macdef GLUT_MIDDLE_BUTTON = $extval (GLenum, "GLUT_MIDDLE_BUTTON")
macdef GLUT_RIGHT_BUTTON = $extval (GLenum, "GLUT_RIGHT_BUTTON")

macdef GLUT_UP = $extval (GLenum, "GLUT_UP")
macdef GLUT_DOWN = $extval (GLenum, "GLUT_DOWN")
macdef GLUT_LEFT = $extval (GLenum, "GLUT_LEFT")
macdef GLUT_ENTERED = $extval (GLenum, "GLUT_ENTERED")

(* ****** ****** *)

(*
** GLUT API macro definitions -- the display mode definitions
*)
macdef GLUT_RGB = $extval (GLenum, "GLUT_RGB")
macdef GLUT_RGBA = $extval (GLenum, "GLUT_RGBA")
macdef GLUT_INDEX = $extval (GLenum, "GLUT_INDEX")
macdef GLUT_SINGLE = $extval (GLenum, "GLUT_SINGLE")
macdef GLUT_DOUBLE = $extval (GLenum, "GLUT_DOUBLE")
macdef GLUT_ACCUM = $extval (GLenum, "GLUT_ACCUM")
macdef GLUT_ALPHA = $extval (GLenum, "GLUT_ALPHA")
macdef GLUT_DEPTH = $extval (GLenum, "GLUT_DEPTH")
macdef GLUT_STENCIL = $extval (GLenum, "GLUT_STENCIL")
macdef GLUT_MULTISAMPLE = $extval (GLenum, "GLUT_MULTISAMPLE")
macdef GLUT_STEREO = $extval (GLenum, "GLUT_STEREO")
macdef GLUT_LUMINANCE = $extval (GLenum, "GLUT_LUMINANCE")

(* ****** ****** *)

(*
** GLUT API macro definitions -- windows and menu related definitions
*)
macdef GLUT_MENU_IN_USE = $extval (GLenum, "GLUT_MENU_IN_USE")
macdef GLUT_MENU_NOT_IN_USE = $extval (GLenum, "GLUT_MENU_NOT_IN_USE")
macdef GLUT_VISIBLE = $extval (GLenum, "GLUT_VISIBLE")
macdef GLUT_NOT_VISIBLE = $extval (GLenum, "GLUT_NOT_VISIBLE")
macdef GLUT_HIDDEN = $extval (GLenum, "GLUT_HIDDEN")
macdef GLUT_FULLY_RETAINED = $extval (GLenum, "GLUT_FULLY_RETAINED")
macdef GLUT_PARTIALLY_RETAINED = $extval (GLenum, "GLUT_PARTIALLY_RETAINED")
macdef GLUT_FULLY_COVERED = $extval (GLenum, "GLUT_FULLY_COVERED")

(* ****** ****** *)

(*
** GLUT API macro definitions -- the glutGet parameters
*)
macdef GLUT_WINDOW_X = $extval (GLenum, "GLUT_WINDOW_X")
macdef GLUT_WINDOW_Y = $extval (GLenum, "GLUT_WINDOW_Y")
macdef GLUT_WINDOW_WIDTH = $extval (GLenum, "GLUT_WINDOW_WIDTH")
macdef GLUT_WINDOW_HEIGHT = $extval (GLenum, "GLUT_WINDOW_HEIGHT")
macdef GLUT_WINDOW_BUFFER_SIZE = $extval (GLenum, "GLUT_WINDOW_BUFFER_SIZE")
macdef GLUT_WINDOW_STENCIL_SIZE = $extval (GLenum, "GLUT_WINDOW_STENCIL_SIZE")
macdef GLUT_WINDOW_DEPTH_SIZE = $extval (GLenum, "GLUT_WINDOW_DEPTH_SIZE")
macdef GLUT_WINDOW_RED_SIZE = $extval (GLenum, "GLUT_WINDOW_RED_SIZE")
macdef GLUT_WINDOW_GREEN_SIZE = $extval (GLenum, "GLUT_WINDOW_GREEN_SIZE")
macdef GLUT_WINDOW_BLUE_SIZE = $extval (GLenum, "GLUT_WINDOW_BLUE_SIZE")
macdef GLUT_WINDOW_ALPHA_SIZE = $extval (GLenum, "GLUT_WINDOW_ALPHA_SIZE")
macdef GLUT_WINDOW_ACCUM_RED_SIZE = $extval (GLenum, "GLUT_WINDOW_ACCUM_RED_SIZE")
macdef GLUT_WINDOW_ACCUM_GREEN_SIZE = $extval (GLenum, "GLUT_WINDOW_ACCUM_GREEN_SIZE")
macdef GLUT_WINDOW_ACCUM_BLUE_SIZE = $extval (GLenum, "GLUT_WINDOW_ACCUM_BLUE_SIZE")
macdef GLUT_WINDOW_ACCUM_ALPHA_SIZE = $extval (GLenum, "GLUT_WINDOW_ACCUM_ALPHA_SIZE")
macdef GLUT_WINDOW_DOUBLEBUFFER = $extval (GLenum, "GLUT_WINDOW_DOUBLEBUFFER")
macdef GLUT_WINDOW_RGBA = $extval (GLenum, "GLUT_WINDOW_RGBA")
macdef GLUT_WINDOW_PARENT = $extval (GLenum, "GLUT_WINDOW_PARENT")
macdef GLUT_WINDOW_NUM_CHILDREN = $extval (GLenum, "GLUT_WINDOW_NUM_CHILDREN")
macdef GLUT_WINDOW_COLORMAP_SIZE = $extval (GLenum, "GLUT_WINDOW_COLORMAP_SIZE")
macdef GLUT_WINDOW_NUM_SAMPLES = $extval (GLenum, "GLUT_WINDOW_NUM_SAMPLES")
macdef GLUT_WINDOW_STEREO = $extval (GLenum, "GLUT_WINDOW_STEREO")
macdef GLUT_WINDOW_CURSOR = $extval (GLenum, "GLUT_WINDOW_CURSOR")

macdef GLUT_SCREEN_WIDTH = $extval (GLenum, "GLUT_SCREEN_WIDTH")
macdef GLUT_SCREEN_HEIGHT = $extval (GLenum, "GLUT_SCREEN_HEIGHT")
macdef GLUT_SCREEN_WIDTH_MM = $extval (GLenum, "GLUT_SCREEN_WIDTH_MM")
macdef GLUT_SCREEN_HEIGHT_MM = $extval (GLenum, "GLUT_SCREEN_HEIGHT_MM")
macdef GLUT_MENU_NUM_ITEMS = $extval (GLenum, "GLUT_MENU_NUM_ITEMS")
macdef GLUT_DISPLAY_MODE_POSSIBLE = $extval (GLenum, "GLUT_DISPLAY_MODE_POSSIBLE")
macdef GLUT_INIT_WINDOW_X = $extval (GLenum, "GLUT_INIT_WINDOW_X")
macdef GLUT_INIT_WINDOW_Y = $extval (GLenum, "GLUT_INIT_WINDOW_Y")
macdef GLUT_INIT_WINDOW_WIDTH = $extval (GLenum, "GLUT_INIT_WINDOW_WIDTH")
macdef GLUT_INIT_WINDOW_HEIGHT = $extval (GLenum, "GLUT_INIT_WINDOW_HEIGHT")
macdef GLUT_INIT_DISPLAY_MODE = $extval (GLenum, "GLUT_INIT_DISPLAY_MODE")
macdef GLUT_ELAPSED_TIME = $extval (GLenum, "GLUT_ELAPSED_TIME")
macdef GLUT_WINDOW_FORMAT_ID = $extval (GLenum, "GLUT_WINDOW_FORMAT_ID")
macdef GLUT_INIT_STATE = $extval (GLenum, "GLUT_INIT_STATE")

(* ****** ****** *)

(*
** GLUT API macro definitions -- the glutDeviceGet parameters
*)
macdef GLUT_HAS_KEYBOARD = $extval (GLenum, "GLUT_HAS_KEYBOARD")
macdef GLUT_HAS_MOUSE = $extval (GLenum, "GLUT_HAS_MOUSE")
macdef GLUT_HAS_SPACEBALL = $extval (GLenum, "GLUT_HAS_SPACEBALL")
macdef GLUT_HAS_DIAL_AND_BUTTON_BOX = $extval (GLenum, "GLUT_HAS_DIAL_AND_BUTTON_BOX")
macdef GLUT_HAS_TABLET = $extval (GLenum, "GLUT_HAS_TABLET")
macdef GLUT_NUM_MOUSE_BUTTONS = $extval (GLenum, "GLUT_NUM_MOUSE_BUTTONS")
macdef GLUT_NUM_SPACEBALL_BUTTONS = $extval (GLenum, "GLUT_NUM_SPACEBALL_BUTTONS")
macdef GLUT_NUM_BUTTON_BOX_BUTTONS = $extval (GLenum, "GLUT_NUM_BUTTON_BOX_BUTTONS")
macdef GLUT_NUM_DIALS = $extval (GLenum, "GLUT_NUM_DIALS")
macdef GLUT_NUM_TABLET_BUTTONS = $extval (GLenum, "GLUT_NUM_TABLET_BUTTONS")
macdef GLUT_DEVICE_IGNORE_KEY_REPEAT = $extval (GLenum, "GLUT_DEVICE_IGNORE_KEY_REPEAT")
macdef GLUT_DEVICE_KEY_REPEAT = $extval (GLenum, "GLUT_DEVICE_KEY_REPEAT")
macdef GLUT_HAS_JOYSTICK = $extval (GLenum, "GLUT_HAS_JOYSTICK")
macdef GLUT_OWNS_JOYSTICK = $extval (GLenum, "GLUT_OWNS_JOYSTICK")
macdef GLUT_JOYSTICK_BUTTONS = $extval (GLenum, "GLUT_JOYSTICK_BUTTONS")
macdef GLUT_JOYSTICK_AXES = $extval (GLenum, "GLUT_JOYSTICK_AXES")
macdef GLUT_JOYSTICK_POLL_RATE = $extval (GLenum, "GLUT_JOYSTICK_POLL_RATE")

(* ****** ****** *)

(*
** GLUT API macro definitions -- the glutLayerGet parameters
*)
macdef GLUT_OVERLAY_POSSIBLE = $extval (GLenum, "GLUT_OVERLAY_POSSIBLE")
macdef GLUT_LAYER_IN_USE = $extval (GLenum, "GLUT_LAYER_IN")
macdef GLUT_HAS_OVERLAY = $extval (GLenum, "GLUT_HAS_OVERLAY")
macdef GLUT_TRANSPARENT_INDEX = $extval (GLenum, "GLUT_TRANSPARENT_INDEX")
macdef GLUT_NORMAL_DAMAGED = $extval (GLenum, "GLUT_NORMAL_DAMAGED")
macdef GLUT_OVERLAY_DAMAGED = $extval (GLenum, "GLUT_OVERLAY_DAMAGED")

(* ****** ****** *)

(*
** GLUT API macro definitions -- the glutVideoResizeGet parameters
*)
macdef GLUT_VIDEO_RESIZE_POSSIBLE = $extval (GLenum, "GLUT_VIDEO_RESIZE_POSSIBLE")
macdef GLUT_VIDEO_RESIZE_IN_USE = $extval (GLenum, "GLUT_VIDEO_RESIZE_IN_USE")
macdef GLUT_VIDEO_RESIZE_X_DELTA = $extval (GLenum, "GLUT_VIDEO_RESIZE_X_DELTA")
macdef GLUT_VIDEO_RESIZE_Y_DELTA = $extval (GLenum, "GLUT_VIDEO_RESIZE_Y_DELTA")
macdef GLUT_VIDEO_RESIZE_WIDTH_DELTA = $extval (GLenum, "GLUT_VIDEO_RESIZE_WIDTH_DELTA")
macdef GLUT_VIDEO_RESIZE_HEIGHT_DELTA = $extval (GLenum, "GLUT_VIDEO_RESIZE_HEIGHT_DELTA")
macdef GLUT_VIDEO_RESIZE_X = $extval (GLenum, "GLUT_VIDEO_RESIZE_X")
macdef GLUT_VIDEO_RESIZE_Y = $extval (GLenum, "GLUT_VIDEO_RESIZE_Y")
macdef GLUT_VIDEO_RESIZE_WIDTH = $extval (GLenum, "GLUT_VIDEO_RESIZE_WIDTH")
macdef GLUT_VIDEO_RESIZE_HEIGHT = $extval (GLenum, "GLUT_VIDEO_RESIZE_HEIGHT")

(* ****** ****** *)

(*
** GLUT API macro definitions -- the glutUseLayer parameters
*)
macdef GLUT_NORMAL = $extval (GLenum, "GLUT_NORMAL")
macdef GLUT_OVERLAY = $extval (GLenum, "GLUT_OVERLAY")

(* ****** ****** *)

(*
** GLUT API macro definitions -- the glutGetModifiers parameters
*)
macdef GLUT_ACTIVE_SHIFT = $extval (GLenum, "GLUT_ACTIVE_SHIFT")
macdef GLUT_ACTIVE_CTRL = $extval (GLenum, "GLUT_ACTIVE_CTRL")
macdef GLUT_ACTIVE_ALT = $extval (GLenum, "GLUT_ACTIVE_ALT")

(* ****** ****** *)

(*
** GLUT API macro definitions -- the glutSetCursor parameters
*)
macdef GLUT_CURSOR_RIGHT_ARROW = $extval (GLenum, "GLUT_CURSOR_RIGHT_ARROW")
macdef GLUT_CURSOR_LEFT_ARROW = $extval (GLenum, "GLUT_CURSOR_LEFT_ARROW")
macdef GLUT_CURSOR_INFO = $extval (GLenum, "GLUT_CURSOR_INFO")
macdef GLUT_CURSOR_DESTROY = $extval (GLenum, "GLUT_CURSOR_DESTROY")
macdef GLUT_CURSOR_HELP = $extval (GLenum, "GLUT_CURSOR_HELP")
macdef GLUT_CURSOR_CYCLE = $extval (GLenum, "GLUT_CURSOR_CYCLE")
macdef GLUT_CURSOR_SPRAY = $extval (GLenum, "GLUT_CURSOR_SPRAY")
macdef GLUT_CURSOR_WAIT = $extval (GLenum, "GLUT_CURSOR_WAIT")
macdef GLUT_CURSOR_TEXT = $extval (GLenum, "GLUT_CURSOR_TEXT")
macdef GLUT_CURSOR_CROSSHAIR = $extval (GLenum, "GLUT_CURSOR_CROSSHAIR")
macdef GLUT_CURSOR_UP_DOWN = $extval (GLenum, "GLUT_CURSOR_UP_DOWN")
macdef GLUT_CURSOR_LEFT_RIGHT = $extval (GLenum, "GLUT_CURSOR_LEFT_RIGHT")
macdef GLUT_CURSOR_TOP_SIDE = $extval (GLenum, "GLUT_CURSOR_TOP_SIDE")
macdef GLUT_CURSOR_BOTTOM_SIDE = $extval (GLenum, "GLUT_CURSOR_BOTTOM_SIDE")
macdef GLUT_CURSOR_LEFT_SIDE = $extval (GLenum, "GLUT_CURSOR_LEFT_SIDE")
macdef GLUT_CURSOR_RIGHT_SIDE = $extval (GLenum, "GLUT_CURSOR_RIGHT_SIDE")
macdef GLUT_CURSOR_TOP_LEFT_CORNER = $extval (GLenum, "GLUT_CURSOR_TOP_LEFT_CORNER")
macdef GLUT_CURSOR_TOP_RIGHT_CORNER = $extval (GLenum, "GLUT_CURSOR_TOP_RIGHT_CORNER")
macdef GLUT_CURSOR_BOTTOM_LEFT_CORNER = $extval (GLenum, "GLUT_CURSOR_BOTTOM_LEFT_CORNER")
macdef GLUT_CURSOR_BOTTOM_RIGHT_CORNER = $extval (GLenum, "GLUT_CURSOR_BOTTOM_RIGHT_CORNER")
macdef GLUT_CURSOR_INHERIT = $extval (GLenum, "GLUT_CURSOR_INHERIT")
macdef GLUT_CURSOR_NONE = $extval (GLenum, "GLUT_CURSOR_NONE")
macdef GLUT_CURSOR_FULL_CROSSHAIR = $extval (GLenum, "GLUT_CURSOR_FULL_CROSSHAIR")

(* ****** ****** *)

(*
** GLUT API macro definitions -- RGB color component specification definitions
*)
macdef GLUT_RED = $extval (GLenum, "GLUT_RED")
macdef GLUT_GREEN = $extval (GLenum, "GLUT_GREEN")
macdef GLUT_BLUE = $extval (GLenum, "GLUT_BLUE")

(* ****** ****** *)

(*
** GLUT API macro definitions -- additional keyboard and joystick definitions
*)
macdef GLUT_KEY_REPEAT_OFF = $extval (GLenum, "GLUT_KEY_REPEAT_OFF")
macdef GLUT_KEY_REPEAT_ON = $extval (GLenum, "GLUT_KEY_REPEAT_ON")
macdef GLUT_KEY_REPEAT_DEFAULT = $extval (GLenum, "GLUT_KEY_REPEAT_DEFAULT")

macdef GLUT_JOYSTICK_BUTTON_A = $extval (GLenum, "GLUT_JOYSTICK_BUTTON_A")
macdef GLUT_JOYSTICK_BUTTON_B = $extval (GLenum, "GLUT_JOYSTICK_BUTTON_B")
macdef GLUT_JOYSTICK_BUTTON_C = $extval (GLenum, "GLUT_JOYSTICK_BUTTON_C")
macdef GLUT_JOYSTICK_BUTTON_D = $extval (GLenum, "GLUT_JOYSTICK_BUTTON_D")

(* ****** ****** *)

(*
** GLUT API macro definitions -- game mode definitions
*)
macdef GLUT_GAME_MODE_ACTIVE = $extval (GLenum, "GLUT_GAME_MODE_ACTIVE")
macdef GLUT_GAME_MODE_POSSIBLE = $extval (GLenum, "GLUT_GAME_MODE_POSSIBLE")
macdef GLUT_GAME_MODE_WIDTH = $extval (GLenum, "GLUT_GAME_MODE_WIDTH")
macdef GLUT_GAME_MODE_HEIGHT = $extval (GLenum, "GLUT_GAME_MODE_HEIGHT")
macdef GLUT_GAME_MODE_PIXEL_DEPTH = $extval (GLenum, "GLUT_GAME_MODE_PIXEL_DEPTH")
macdef GLUT_GAME_MODE_REFRESH_RATE = $extval (GLenum, "GLUT_GAME_MODE_REFRESH_RATE")
macdef GLUT_GAME_MODE_DISPLAY_CHANGED = $extval (GLenum, "GLUT_GAME_MODE_DISPLAY_CHANGED")

(* ****** ****** *)

(*
** Initialization functions, see fglut_init.c
*)

(*
extern void glutInit( int* pargc, char** argv );
*)

fun glutInitWindowPosition (
  x: int, y: int
) : void
  = "mac#atsctrb_glutInitWindowPosition"
// end of [fun]

fun glutInitWindowSize
  (width: int, height: int): void = "mac#atsctrb_glutInitWindowSize"
// end of [fun]

fun glutInitDisplayMode
  (display: uint): void = "mac#atsctrb_glutInitDisplayMode"
// end of [fun]

fun glutInitDisplayString
  (display: string): void = "mac#atsctrb_glutInitDisplayString"
// end of [fun]

(* ****** ****** *)
//
// HX: Process loop function, see freeglut_main.c
//
fun glutMainLoop (): void = "mac#atsctrb_glutMainLoop"

(* ****** ****** *)

(*
** Window management functions, see freeglut_window.c
*)
fun glutCreateWindow
  (title: string): int = "mac#atsctrb_glutCreateWindow"

fun glutCreateSubWindow (
  window: int, x: int, y: int, width: int, height: int
) : int
  = "mac#atsctrb_glutCreateSubWindow"
// end of [fun]

fun glutDestroyWindow
  (window: int): void = "mac#atsctrb_glutDestroyWindow"

fun glutGetWindow
  (): int = "mac#atsctrb_glutGetWindow"
fun glutSetWindow
  (window: int): void = "mac#atsctrb_glutSetWindow"

fun glutSetWindowTitle
  (title: string): void = "mac#atsctrb_glutSetWindowTitle"

fun glutSetIconTitle
  (title: string): void = "mac#atsctrb_glutSetIconTitle"
  
fun glutReshapeWindow (
  width: int, height: int
) : void
  = "mac#atsctrb_glutReshapeWindow"
// end of [fun]

fun glutPositionWindow (
  x: int, y: int
) : void
  = "mac#atsctrb_glutPositionWindow"
// end of [fun]

fun glutShowWindow (): void = "mac#atsctrb_glutShowWindow"
fun glutHideWindow (): void = "mac#atsctrb_glutHideWindow"
fun glutIconifyWindow (): void = "mac#atsctrb_glutIconifyWindow"
fun glutPushWindow (): void = "mac#atsctrb_glutPushWindow"
fun glutPopWindow (): void = "mac#atsctrb_glutPopWindow"
fun glutFullScreen (): void = "mac#atsctrb_glutFullScreen"

(* ****** ****** *)

(*
** Display-connected functions, see freeglut_display.c
*)

fun
glutPostWindowRedisplay (
  window: int
) : void
  = "mac#atsctrb_glutPostWindowRedisplay"
// end of [fun]

fun
glutPostRedisplay
  (): void = "mac#atsctrb_glutPostRedisplay"
// end of [fun]

fun
glutSwapBuffers
  (): void = "mac#atsctrb_glutSwapBuffers"
// end of [fun]

(* ****** ****** *)

(*
** Global callback functions, see freeglut_callbacks.c
*)
fun glutTimerFunc (
  time: uint, callback: (int) -> void, value: int
) : void
  = "mac#atsctrb_glutTimerFunc"

fun glutIdleFunc (
  callback: () -> void
) : void
  = "mac#atsctrb_glutIdleFunc"

fun glutIdleFunc_null
  (): void = "atsctrb_glutIdleFunc_null" // this is a function!
// end of [glutIdleFunc_null]

(* ****** ****** *)

(*
** Window-specific callback functions, see freeglut_callbacks.c
*)
fun glutKeyboardFunc (
  callback: (uchar, int, int) -> void
) : void
  = "mac#atsctrb_glutKeyboardFunc"

fun glutMouseFunc (
  callback: (int, int, int, int) -> void
) : void
  = "mac#atsctrb_glutMouseFunc"

fun glutSpecialFunc (
  callback: (int, int, int) -> void
 ) : void
  = "mac#atsctrb_glutSpecialFunc"

fun glutReshapeFunc (
  callback: (int, int) -> void
) : void
  = "mac#atsctrb_glutReshapeFunc"

fun glutVisibilityFunc (
  callback: (int) -> void
) : void
  = "mac#atsctrb_glutVisibilityFunc"

fun glutDisplayFunc (
  callback: () -> void
) : void
  = "mac#atsctrb_glutDisplayFunc"

fun glutMotionFunc (
  callback: (int, int) -> void
) : void
  = "mac#atsctrb_glutMotionFunc"

fun glutPassiveMotionFunc (
  callback: (int, int) -> void
) : void
  = "mac#atsctrb_glutPassiveMotionFunc"

fun glutEntryFunc (
  callback: (int) -> void
) : void
  = "mac#atsctrb_glutEntryFunc"

(* ****** ****** *)

(*
** State setting and retrieval functions, see freeglut_state.c
*)
fun glutGet (query: GLenum): int = "mac#atsctrb_glutGet"
fun glutDeviceGet (query: GLenum): int = "mac#atsctrb_glutDeviceGet"
fun glutGetModifiers (): int = "mac#atsctrb_glutGetModifiers"
fun glutLayerGet (query: GLenum): int = "mac#atsctrb_glutLayerGet"

(* ****** ****** *)

abstype GLUTfontref_stroke // void*

macdef GLUT_STROKE_ROMAN =
  $extval (GLUTfontref_stroke, "GLUT_STROKE_ROMAN")

macdef GLUT_STROKE_MONO_ROMAN =
  $extval (GLUTfontref_stroke, "GLUT_STROKE_MONO_ROMAN")

fun glutStrokeCharacter (
  font: GLUTfontref_stroke, c: char
) : void
  = "mac#atsctrb_glutStrokeCharacter"
fun glutStrokeWidth (
  font: GLUTfontref_stroke, c: char
) : int
  = "mac#atsctrb_glutStrokeWidth"
fun glutStrokeLength (
  font: GLUTfontref_stroke, txt: string
 ) : int
  = "mac#atsctrb_glutStrokeLength"

//
// HX: implemented in [glut.dats]
//
fun glutStrokeString
  (font: GLUTfontref_stroke, s: string): void
// end of [glutStrokeString]

(* ****** ****** *)

abstype GLUTfontref_bitmap // void*

macdef GLUT_BITMAP_8_BY_13 =
  $extval (GLUTfontref_bitmap, "GLUT_BITMAP_8_BY_13")

macdef GLUT_BITMAP_9_BY_15 =
  $extval (GLUTfontref_bitmap, "GLUT_BITMAP_9_BY_15")

macdef GLUT_BITMAP_TIMES_ROMAN_10 =
  $extval (GLUTfontref_bitmap, "GLUT_BITMAP_TIMES_ROMAN_10")

macdef GLUT_BITMAP_TIMES_ROMAN_24 =
  $extval (GLUTfontref_bitmap, "GLUT_BITMAP_TIMES_ROMAN_24")

macdef GLUT_BITMAP_HELVETICA_10 =
  $extval (GLUTfontref_bitmap, "GLUT_BITMAP_HELVETICA_10")
macdef GLUT_BITMAP_HELVETICA_12 =
  $extval (GLUTfontref_bitmap, "GLUT_BITMAP_HELVETICA_12")
macdef GLUT_BITMAP_HELVETICA_18 =
  $extval (GLUTfontref_bitmap, "GLUT_BITMAP_HELVETICA_18")

fun glutBitmapCharacter (font: GLUTfontref_bitmap, c: char): void
  = "mac#atsctrb_glutBitmapCharacter"
fun glutBitmapWidth (font: GLUTfontref_bitmap, c: char): int
  = "mac#atsctrb_glutBitmapWidth"
fun glutBitmapLength (font: GLUTfontref_bitmap, txt: string): int
  = "mac#atsctrb_glutBitmapLength"
//
// HX: implemented in [glut.dats]
//
fun glutBitmapString (font: GLUTfontref_bitmap, s: string): void

(* ****** ****** *)

fun glutWireCube
  (size: GLdouble): void = "mac#atsctrb_glutWireCube"
fun glutSolidCube
  (size: GLdouble): void = "mac#atsctrb_glutSolidCube"

fun glutWireSphere
  (radius: GLdouble, slices: GLint, stacks: GLint): void
  = "mac#atsctrb_glutWireSphere"

fun glutSolidSphere
  (radius: GLdouble, slices: GLint, stacks: GLint): void
  = "mac#atsctrb_glutSolidSphere"

(* ****** ****** *)

fun glutWireCone (
  base: GLdouble, height: GLdouble, slices: GLint, stacks: GLint
) : void
  = "mac#atsctrb_glutWireCone"
// end of [fun]

fun glutSolidCone (
  base: GLdouble, height: GLdouble, slices: GLint, stacks: GLint
) : void
  = "mac#atsctrb_glutSolidCone"
// end of [fun]

(* ****** ****** *)

fun glutWireTorus (
  innerRadius: GLdouble
, outerRadius: GLdouble
, sides: GLint
, rings: GLint
) : void
  = "mac#atsctrb_glutWireTorus"
// end of [fun]

fun glutSolidTorus (
  innerRadius: GLdouble
, outerRadius: GLdouble
, sides: GLint
, rings: GLint
) : void
  = "mac#atsctrb_glutSolidTorus"
// end of [fun]

(* ****** ****** *)

fun glutWireTeapot
  (size: GLdouble): void = "mac#atsctrb_glutWireTeapot"
// end of [glutWireTeapot]

fun glutSolidTeapot
  (size: GLdouble): void = "mac#atsctrb_glutSolidTeapot"
// end of [glutSolidTeapot]

(* ****** ****** *)

fun glutWireDodecahedron
  (): void = "mac#atsctrb_glutWireDodecahedron"
fun glutSolidDodecahedron
  (): void = "mac#atsctrb_glutSolidDodecahedron"

fun glutWireOctahedron
  (): void = "mac#atsctrb_glutWireOctahedron"
fun glutSolidOctahedron
  (): void = "mac#atsctrb_glutSolidOctahedron"

fun glutWireTetrahedron
  (): void = "mac#atsctrb_glutWireTetrahedron"
fun glutSolidTetrahedron
  (): void = "mac#atsctrb_glutSolidTetrahedron"

fun glutWireIcosahedron
  (): void = "mac#atsctrb_glutWireIcosahedron"
fun glutSolidIcosahedron
  (): void = "mac#atsctrb_glutSolidIcosahedron"

(* ****** ****** *)

(* end of [glut.sats] *)
