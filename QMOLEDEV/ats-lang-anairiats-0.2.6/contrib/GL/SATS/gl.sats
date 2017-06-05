(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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
// Restarting time: December, 2009
//
(* ****** ****** *)

%{#
#include "contrib/GL/CATS/gl.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

typedef GLvoid = void

// typedef GLenum = uint
abst@ype GLenum = $extype"ats_GLenum_type"
fun int_of_GLenum (x: GLenum): int = "atsctrb_int_of_GLenum"
overload int_of with int_of_GLenum

fun eq_GLenum_GLenum
  (x1: GLenum, x2: GLenum):<> bool = "atsctrb_eq_GLenum_GLenum"
overload = with eq_GLenum_GLenum

fun neq_GLenum_GLenum
  (x1: GLenum, x2: GLenum):<> bool = "atsctrb_neq_GLenum_GLenum"
overload <> with neq_GLenum_GLenum

abst@ype GLenum_type (a:t@ype) = GLenum
abst@ype GLenum_format (n:int) = GLenum

(* ****** ****** *)

// typedef GLbitfield = int
abst@ype
GLbitfield = $extype"ats_GLbitfield_type"

(* ****** ****** *)

// typedef GLboolean = uchar
abst@ype
GLboolean = $extype"ats_GLboolean_type"

(* ****** ****** *)

// typedef GLbyte = char // 1-byte signed
abst@ype GLbyte = $extype"ats_GLbyte_type"
castfn GLbyte_of_byte (x: byte):<> GLbyte

// typedef GLubyte = uchar // 1-byte unsigned
abst@ype GLubyte = $extype"ats_GLubyte_type"
castfn GLubyte_of_byte (x: byte):<> GLubyte

(* ****** ****** *)
//
// typedef GLshort = short // 2-byte signed
//
abst@ype GLshort = $extype"ats_GLshort_type"

//
// typedef GLushort = usint // 2-byte unsigned
//
abst@ype GLushort = $extype"ats_GLushort_type"

(* ****** ****** *)

abst@ype
GLint (n:int) = $extype"ats_GLint_type" // 4-byte signed
typedef GLint = [n:int] GLint (n)
castfn GLint_of_int (x: int):<> GLint
castfn int_of_GLint (x: GLint):<> int
castfn GLint_of_int1 {n:int} (x: int n):<> GLint n
castfn int1_of_GLint {n:int} (x: GLint n):<> int n
castfn GLint_of_GLenum (x: GLenum):<> GLint

abst@ype
GLuint (n:int) = $extype"ats_GLuint_type" // 4-byte unsigned
typedef GLuint = [n:nat] GLuint (n)
castfn GLuint_of_uint (x: uint):<> GLuint
castfn uint_of_GLuint (x: GLuint):<> uint
castfn GLuint_of_uint1 {n:int} (x: uint n):<> GLuint n
castfn uint1_of_GLuint {n:int} (x: GLuint n):<> uint n

(* ****** ****** *)
//
// typedef GLsizei = int // 4-byte signed
//
abst@ype
GLsizei (i: int) = $extype"ats_GLsizei_type"
typedef GLsizei = [i:int] GLsizei (i)

(* ****** ****** *)

abst@ype
GLdouble = $extype"ats_GLdouble_type" // double precision
castfn GLdouble_of_double (x: double):<> GLdouble
castfn double_of_GLdouble (x: GLdouble):<> double
overload double_of with double_of_GLdouble

//
// typedef GLclampd = double // double precision float in [0,1]
//
abst@ype
GLclampd = $extype"ats_GLclampd_type"
castfn GLclampd_of_double (x: double):<> GLclampd

(* ****** ****** *)

abst@ype
GLfloat = $extype"ats_GLfloat_type" // single precision
castfn GLfloat_of_float (x: float):<> GLfloat
castfn float_of_GLfloat (x: GLfloat):<> float
overload float_of with float_of_GLfloat

//
// typedef GLclampf = float // single precision float in [0,1]
//
abst@ype
GLclampf = $extype"ats_GLclampf_type"
castfn GLclampf_of_float (x: float):<> GLclampf

(* ****** ****** *)

fun GLbyte_of_int (x: int):<> GLbyte = "atsctrb_GLbyte_of_int"
fun GLubyte_of_int (x: int):<> GLubyte = "atsctrb_GLubyte_of_int"
fun GLubyte_of_uint (x: uint):<> GLubyte = "atsctrb_GLubyte_of_uint"

fun GLshort_of_int (x: int):<> GLshort = "atsctrb_GLshort_of_int"
fun GLushort_of_int (x: int):<> GLushort = "atsctrb_GLushort_of_int"
fun GLushort_of_uint (x: uint):<> GLushort = "atsctrb_GLushort_of_uint"

fun GLsizei_of_int1 {i:int} (x: int i): GLsizei i = "atsctrb_GLsizei_of_int"

fun GLdouble_of_int (x: int):<> GLdouble = "atsctrb_GLdouble_of_int"

fun GLfloat_of_int (x: int):<> GLfloat = "atsctrb_GLfloat_of_int"
fun GLfloat_of_double (x: double):<> GLfloat = "atsctrb_GLfloat_of_double"
fun GLclampf_of_double (x: double):<> GLclampf = "atsctrb_GLclampf_of_double"

(* ****** ****** *)

symintr GLbyte GLubyte
overload GLbyte with GLbyte_of_int
overload GLubyte with GLubyte_of_int
overload GLubyte with GLubyte_of_uint

symintr GLshort GLushort
overload GLshort with GLshort_of_int
overload GLushort with GLushort_of_int
overload GLushort with GLushort_of_uint

symintr GLint GLuint
overload GLint with GLint_of_int // castfn
overload GLint with GLint_of_int1 // castfn
overload GLint with GLint_of_GLenum // castfn
overload GLuint with GLuint_of_uint // castfn
overload GLuint with GLuint_of_uint1 // castfn

symintr GLsizei
overload GLsizei with GLsizei_of_int1

symintr GLdouble GLclampd
overload GLdouble with GLdouble_of_int
overload GLdouble with GLdouble_of_double // castfn
overload GLclampd with GLclampd_of_double // castfn

symintr GLfloat GLclampf
overload GLfloat with GLfloat_of_int
overload GLfloat with GLfloat_of_float
overload GLfloat with GLfloat_of_double
overload GLclampf with GLclampf_of_double

(* ****** ****** *)

fun lor_GLbitfield_GLbitfield
  (b1: GLbitfield, b2: GLbitfield): GLbitfield
  = "atsctrb_lor_GLbitfield_GLbitfield"
overload lor with lor_GLbitfield_GLbitfield

(* ****** ****** *)

abst@ype GLarray2 (a:t@ype, w:int, n:int) // for 2-dimension arrays
abst@ype GLarray3 (a:t@ype, w:int, h:int, n:int) // for 3-dimension arrays

(* ****** ****** *)

// Datatypes

macdef GL_BYTE = $extval (GLenum_type GLbyte, "GL_BYTE")
macdef GL_UNSIGNED_BYTE = $extval (GLenum_type GLubyte, "GL_UNSIGNED_BYTE")
macdef GL_SHORT = $extval (GLenum_type GLshort, "GL_SHORT")
macdef GL_UNSIGNED_SHORT = $extval (GLenum_type GLushort, "GL_UNSIGNED_SHORT")
macdef GL_INT = $extval (GLenum_type GLint, "GL_INT")
macdef GL_UNSIGNED_INT = $extval (GLenum_type GLuint, "GL_UNSIGNED_INT")

macdef GL_FLOAT = $extval (GLenum_type GLfloat, "GL_FLOAT")
macdef GL_DOUBLE = $extval (GLenum_type GLdouble, "GL_DOUBLE")

macdef GL_2_BYTES = $extval (GLenum, "GL_2_BYTES")
macdef GL_3_BYTES = $extval (GLenum, "GL_3_BYTES")
macdef GL_4_BYTES = $extval (GLenum, "GL_4_BYTES")

// Boolean values
macdef GL_TRUE = $extval (GLboolean, "GL_TRUE")
macdef GL_FALSE = $extval (GLboolean, "GL_FALSE")

(* ****** ****** *)

// Primitives

macdef GL_POINTS = $extval (GLenum, "GL_POINTS")
macdef GL_LINES = $extval (GLenum, "GL_LINES")
macdef GL_LINE_LOOP = $extval (GLenum, "GL_LINE_LOOP")
macdef GL_LINE_STRIP = $extval (GLenum, "GL_LINE_STRIP")
macdef GL_TRIANGLES = $extval (GLenum, "GL_TRIANGLES")
macdef GL_TRIANGLE_STRIP = $extval (GLenum, "GL_TRIANGLE_STRIP")
macdef GL_TRIANGLE_FAN = $extval (GLenum, "GL_TRIANGLE_FAN")
macdef GL_QUADS = $extval (GLenum, "GL_QUADS")
macdef GL_QUAD_STRIP = $extval (GLenum, "GL_QUAD_STRIP")
macdef GL_POLYGON = $extval (GLenum, "GL_POLYGON")

// Vertex Arrays

macdef GL_VERTEX_ARRAY = $extval (GLenum, "GL_VERTEX_ARRAY")
macdef GL_NORMAL_ARRAY = $extval (GLenum, "GL_NORMAL_ARRAY")
macdef GL_COLOR_ARRAY = $extval (GLenum, "GL_COLOR_ARRAY")
macdef GL_INDEX_ARRAY = $extval (GLenum, "GL_INDEX_ARRAY")
macdef GL_TEXTURE_COORD_ARRAY = $extval (GLenum, "GL_TEXTURE_COORD_ARRAY")
macdef GL_EDGE_FLAG_ARRAY = $extval (GLenum, "GL_EDGE_FLAG_ARRAY")
macdef GL_VERTEX_ARRAY_SIZE = $extval (GLenum, "GL_VERTEX_ARRAY_SIZE")
macdef GL_VERTEX_ARRAY_TYPE = $extval (GLenum, "GL_VERTEX_ARRAY_TYPE")
macdef GL_VERTEX_ARRAY_STRIDE = $extval (GLenum, "GL_VERTEX_ARRAY_STRIDE")
macdef GL_NORMAL_ARRAY_TYPE = $extval (GLenum, "GL_NORMAL_ARRAY_TYPE")
macdef GL_NORMAL_ARRAY_STRIDE = $extval (GLenum, "GL_NORMAL_ARRAY_STRIDE")
macdef GL_COLOR_ARRAY_SIZE = $extval (GLenum, "GL_COLOR_ARRAY_SIZE")
macdef GL_COLOR_ARRAY_TYPE = $extval (GLenum, "GL_COLOR_ARRAY_TYPE")
macdef GL_COLOR_ARRAY_STRIDE = $extval (GLenum, "GL_COLOR_ARRAY_STRIDE")
macdef GL_INDEX_ARRAY_TYPE = $extval (GLenum, "GL_INDEX_ARRAY_TYPE")
macdef GL_INDEX_ARRAY_STRIDE = $extval (GLenum, "GL_INDEX_ARRAY_STRIDE")
macdef GL_TEXTURE_COORD_ARRAY_SIZE = $extval (GLenum, "GL_TEXTURE_COORD_ARRAY_SIZE")
macdef GL_TEXTURE_COORD_ARRAY_TYPE = $extval (GLenum, "GL_TEXTURE_COORD_ARRAY_TYPE")
macdef GL_TEXTURE_COORD_ARRAY_STRIDE = $extval (GLenum, "GL_TEXTURE_COORD_ARRAY_STRIDE")
macdef GL_EDGE_FLAG_ARRAY_STRIDE = $extval (GLenum, "GL_EDGE_FLAG_ARRAY_STRIDE")
macdef GL_VERTEX_ARRAY_POINTER = $extval (GLenum, "GL_VERTEX_ARRAY_POINTER")
macdef GL_NORMAL_ARRAY_POINTER = $extval (GLenum, "GL_NORMAL_ARRAY_POINTER")
macdef GL_COLOR_ARRAY_POINTER = $extval (GLenum, "GL_COLOR_ARRAY_POINTER")
macdef GL_INDEX_ARRAY_POINTER = $extval (GLenum, "GL_INDEX_ARRAY_POINTER")
macdef GL_TEXTURE_COORD_ARRAY_POINTER = $extval (GLenum, "GL_TEXTURE_COORD_ARRAY_POINTER")
macdef GL_EDGE_FLAG_ARRAY_POINTER = $extval (GLenum, "GL_EDGE_FLAG_ARRAY_POINTER")
macdef GL_V2F = $extval (GLenum, "GL_V2F")
macdef GL_V3F = $extval (GLenum, "GL_V3F")
macdef GL_C4UB_V2F = $extval (GLenum, "GL_C4UB_V2F")
macdef GL_C4UB_V3F = $extval (GLenum, "GL_C4UB_V3F")
macdef GL_C3F_V3F = $extval (GLenum, "GL_C3F_V3F")
macdef GL_N3F_V3F = $extval (GLenum, "GL_N3F_V3F")
macdef GL_C4F_N3F_V3F = $extval (GLenum, "GL_C4F_N3F_V3F")
macdef GL_T2F_V3F = $extval (GLenum, "GL_T2F_V3F")
macdef GL_T4F_V4F = $extval (GLenum, "GL_T4F_V4F")
macdef GL_T2F_C4UB_V3F = $extval (GLenum, "GL_T2F_C4UB_V3F")
macdef GL_T2F_C3F_V3F = $extval (GLenum, "GL_T2F_C3F_V3F")
macdef GL_T2F_N3F_V3F = $extval (GLenum, "GL_T2F_N3F_V3F")
macdef GL_T2F_C4F_N3F_V3F = $extval (GLenum, "GL_T2F_C4F_N3F_V3F")
macdef GL_T4F_C4F_N3F_V4F = $extval (GLenum, "GL_T4F_C4F_N3F_V4F")

// Matrix Mode
macdef GL_MATRIX_MODE = $extval (GLenum, "GL_MATRIX_MODE")
macdef GL_MODELVIEW = $extval (GLenum, "GL_MODELVIEW")
macdef GL_PROJECTION = $extval (GLenum, "GL_PROJECTION")
macdef GL_TEXTURE = $extval (GLenum, "GL_TEXTURE")

// Points
macdef GL_POINT_SMOOTH = $extval (GLenum, "GL_POINT_SMOOTH")
macdef GL_POINT_SIZE = $extval (GLenum, "GL_POINT_SIZE")
macdef GL_POINT_SIZE_GRANULARITY = $extval (GLenum, "GL_POINT_SIZE_GRANULARITY")
macdef GL_POINT_SIZE_RANGE = $extval (GLenum, "GL_POINT_SIZE_RANGE")

// Lines
macdef GL_LINE_SMOOTH = $extval (GLenum, "GL_LINE_SMOOTH")
macdef GL_LINE_STIPPLE = $extval (GLenum, "GL_LINE_STIPPLE")
macdef GL_LINE_STIPPLE_PATTERN = $extval (GLenum, "GL_LINE_STIPPLE_PATTERN")
macdef GL_LINE_STIPPLE_REPEAT = $extval (GLenum, "GL_LINE_STIPPLE_REPEAT")
macdef GL_LINE_WIDTH = $extval (GLenum, "GL_LINE_WIDTH")
macdef GL_LINE_WIDTH_GRANULARITY = $extval (GLenum, "GL_LINE_WIDTH_GRANULARITY")
macdef GL_LINE_WIDTH_RANGE = $extval (GLenum, "GL_LINE_WIDTH_RANGE")

// Polygons
macdef GL_POINT = $extval (GLenum, "GL_POINT")
macdef GL_LINE = $extval (GLenum, "GL_LINE")
macdef GL_FILL = $extval (GLenum, "GL_FILL")
macdef GL_CW = $extval (GLenum, "GL_CW")
macdef GL_CCW = $extval (GLenum, "GL_CCW")
macdef GL_FRONT = $extval (GLenum, "GL_FRONT")
macdef GL_BACK = $extval (GLenum, "GL_BACK")
macdef GL_POLYGON_MODE = $extval (GLenum, "GL_POLYGON_MODE")
macdef GL_POLYGON_SMOOTH = $extval (GLenum, "GL_POLYGON_SMOOTH")
macdef GL_POLYGON_STIPPLE = $extval (GLenum, "GL_POLYGON_STIPPLE")
macdef GL_EDGE_FLAG = $extval (GLenum, "GL_EDGE_FLAG")
macdef GL_CULL_FACE = $extval (GLenum, "GL_CULL_FACE")
macdef GL_CULL_FACE_MODE = $extval (GLenum, "GL_CULL_FACE_MODE")
macdef GL_FRONT_FACE = $extval (GLenum, "GL_FRONT_FACE")
macdef GL_POLYGON_OFFSET_FACTOR = $extval (GLenum, "GL_POLYGON_OFFSET_FACTOR")
macdef GL_POLYGON_OFFSET_UNITS = $extval (GLenum, "GL_POLYGON_OFFSET_UNITS")
macdef GL_POLYGON_OFFSET_POINT = $extval (GLenum, "GL_POLYGON_OFFSET_POINT")
macdef GL_POLYGON_OFFSET_LINE = $extval (GLenum, "GL_POLYGON_OFFSET_LINE")
macdef GL_POLYGON_OFFSET_FILL = $extval (GLenum, "GL_POLYGON_OFFSET_FILL")

// Display Lists
macdef GL_COMPILE = $extval (GLenum, "GL_COMPILE")
macdef GL_COMPILE_AND_EXECUTE = $extval (GLenum, "GL_COMPILE_AND_EXECUTE")
macdef GL_LIST_BASE = $extval (GLenum, "GL_LIST_BASE")
macdef GL_LIST_INDEX = $extval (GLenum, "GL_LIST_INDEX")
macdef GL_LIST_MODE = $extval (GLenum, "GL_LIST_MODE")

// Depth buffer
macdef GL_NEVER = $extval (GLenum, "GL_NEVER")
macdef GL_LESS = $extval (GLenum, "GL_LESS")
macdef GL_EQUAL = $extval (GLenum, "GL_EQUAL")
macdef GL_LEQUAL = $extval (GLenum, "GL_LEQUAL")
macdef GL_GREATER = $extval (GLenum, "GL_GREATER")
macdef GL_NOTEQUAL = $extval (GLenum, "GL_NOTEQUAL")
macdef GL_GEQUAL = $extval (GLenum, "GL_GEQUAL")
macdef GL_ALWAYS = $extval (GLenum, "GL_ALWAYS")
macdef GL_DEPTH_TEST = $extval (GLenum, "GL_DEPTH_TEST")
macdef GL_DEPTH_BITS = $extval (GLenum, "GL_DEPTH_BITS")
macdef GL_DEPTH_CLEAR_VALUE = $extval (GLenum, "GL_DEPTH_CLEAR_VALUE")
macdef GL_DEPTH_FUNC = $extval (GLenum, "GL_DEPTH_FUNC")
macdef GL_DEPTH_RANGE = $extval (GLenum, "GL_DEPTH_RANGE")
macdef GL_DEPTH_WRITEMASK = $extval (GLenum, "GL_DEPTH_WRITEMASK")
macdef GL_DEPTH_COMPONENT = $extval (GLenum, "GL_DEPTH_COMPONENT")

// Lighting
macdef GL_LIGHTING = $extval (GLenum, "GL_LIGHTING")
macdef GL_LIGHT0 = $extval (GLenum, "GL_LIGHT0")
macdef GL_LIGHT1 = $extval (GLenum, "GL_LIGHT1")
macdef GL_LIGHT2 = $extval (GLenum, "GL_LIGHT2")
macdef GL_LIGHT3 = $extval (GLenum, "GL_LIGHT3")
macdef GL_LIGHT4 = $extval (GLenum, "GL_LIGHT4")
macdef GL_LIGHT5 = $extval (GLenum, "GL_LIGHT5")
macdef GL_LIGHT6 = $extval (GLenum, "GL_LIGHT6")
macdef GL_LIGHT7 = $extval (GLenum, "GL_LIGHT7")
macdef GL_SPOT_EXPONENT = $extval (GLenum, "GL_SPOT_EXPONENT")
macdef GL_SPOT_CUTOFF = $extval (GLenum, "GL_SPOT_CUTOFF")
macdef GL_CONSTANT_ATTENUATION = $extval (GLenum, "GL_CONSTANT_ATTENUATION")
macdef GL_LINEAR_ATTENUATION = $extval (GLenum, "GL_LINEAR_ATTENUATION")
macdef GL_QUADRATIC_ATTENUATION = $extval (GLenum, "GL_QUADRATIC_ATTENUATION")
macdef GL_AMBIENT = $extval (GLenum, "GL_AMBIENT")
macdef GL_DIFFUSE = $extval (GLenum, "GL_DIFFUSE")
macdef GL_SPECULAR = $extval (GLenum, "GL_SPECULAR")
macdef GL_SHININESS = $extval (GLenum, "GL_SHININESS")
macdef GL_EMISSION = $extval (GLenum, "GL_EMISSION")
macdef GL_POSITION = $extval (GLenum, "GL_POSITION")
macdef GL_SPOT_DIRECTION = $extval (GLenum, "GL_SPOT_DIRECTION")
macdef GL_AMBIENT_AND_DIFFUSE = $extval (GLenum, "GL_AMBIENT_AND_DIFFUSE")
macdef GL_COLOR_INDEXES = $extval (GLenum, "GL_COLOR_INDEXES")
macdef GL_LIGHT_MODEL_TWO_SIDE = $extval (GLenum, "GL_LIGHT_MODEL_TWO_SIDE")
macdef GL_LIGHT_MODEL_LOCAL_VIEWER = $extval (GLenum, "GL_LIGHT_MODEL_LOCAL_VIEWER")
macdef GL_LIGHT_MODEL_AMBIENT = $extval (GLenum, "GL_LIGHT_MODEL_AMBIENT")
macdef GL_FRONT_AND_BACK = $extval (GLenum, "GL_FRONT_AND_BACK")
macdef GL_SHADE_MODEL = $extval (GLenum, "GL_SHADE_MODEL")
macdef GL_FLAT = $extval (GLenum, "GL_FLAT")
macdef GL_SMOOTH = $extval (GLenum, "GL_SMOOTH")
macdef GL_COLOR_MATERIAL = $extval (GLenum, "GL_COLOR_MATERIAL")
macdef GL_COLOR_MATERIAL_FACE = $extval (GLenum, "GL_COLOR_MATERIAL_FACE")
macdef GL_COLOR_MATERIAL_PARAMETER = $extval (GLenum, "GL_COLOR_MATERIAL_PARAMETER")
macdef GL_NORMALIZE = $extval (GLenum, "GL_NORMALIZE")

// User clipping planes
macdef GL_CLIP_PLANE0 = $extval (GLenum, "GL_CLIP_PLANE0")
macdef GL_CLIP_PLANE1 = $extval (GLenum, "GL_CLIP_PLANE1")
macdef GL_CLIP_PLANE2 = $extval (GLenum, "GL_CLIP_PLANE2")
macdef GL_CLIP_PLANE3 = $extval (GLenum, "GL_CLIP_PLANE3")
macdef GL_CLIP_PLANE4 = $extval (GLenum, "GL_CLIP_PLANE4")
macdef GL_CLIP_PLANE5 = $extval (GLenum, "GL_CLIP_PLANE5")

// Accumulation buffer
macdef GL_ACCUM_RED_BITS = $extval (GLenum, "GL_ACCUM_RED_BITS")
macdef GL_ACCUM_GREEN_BITS = $extval (GLenum, "GL_ACCUM_GREEN_BITS")
macdef GL_ACCUM_BLUE_BITS = $extval (GLenum, "GL_ACCUM_BLUE_BITS")
macdef GL_ACCUM_ALPHA_BITS = $extval (GLenum, "GL_ACCUM_ALPHA_BITS")
macdef GL_ACCUM_CLEAR_VALUE = $extval (GLenum, "GL_ACCUM_CLEAR_VALUE")
macdef GL_ACCUM = $extval (GLenum, "GL_ACCUM")
macdef GL_ADD = $extval (GLenum, "GL_ADD")
macdef GL_LOAD = $extval (GLenum, "GL_LOAD")
macdef GL_MULT = $extval (GLenum, "GL_MULT")
macdef GL_RETURN = $extval (GLenum, "GL_RETURN")

// Alpha testing
macdef GL_ALPHA_TEST = $extval (GLenum, "GL_ALPHA_TEST")
macdef GL_ALPHA_TEST_REF = $extval (GLenum, "GL_ALPHA_TEST_REF")
macdef GL_ALPHA_TEST_FUNC = $extval (GLenum, "GL_ALPHA_TEST_FUNC")

// Blending
macdef GL_BLEND = $extval (GLenum, "GL_BLEND")
macdef GL_BLEND_SRC = $extval (GLenum, "GL_BLEND_SRC")
macdef GL_BLEND_DST = $extval (GLenum, "GL_BLEND_DST")
macdef GL_ZERO = $extval (GLenum, "GL_ZERO")
macdef GL_ONE = $extval (GLenum, "GL_ONE")
macdef GL_SRC_COLOR = $extval (GLenum, "GL_SRC_COLOR")
macdef GL_ONE_MINUS_SRC_COLOR = $extval (GLenum, "GL_ONE_MINUS_SRC_COLOR")
macdef GL_SRC_ALPHA = $extval (GLenum, "GL_SRC_ALPHA")
macdef GL_ONE_MINUS_SRC_ALPHA = $extval (GLenum, "GL_ONE_MINUS_SRC_ALPHA")
macdef GL_DST_ALPHA = $extval (GLenum, "GL_DST_ALPHA")
macdef GL_ONE_MINUS_DST_ALPHA = $extval (GLenum, "GL_ONE_MINUS_DST_ALPHA")
macdef GL_DST_COLOR = $extval (GLenum, "GL_DST_COLOR")
macdef GL_ONE_MINUS_DST_COLOR = $extval (GLenum, "GL_ONE_MINUS_DST_COLOR")
macdef GL_SRC_ALPHA_SATURATE = $extval (GLenum, "GL_SRC_ALPHA_SATURATE")

// Render mode
macdef GL_FEEDBACK = $extval (GLenum, "GL_FEEDBACK")
macdef GL_RENDER = $extval (GLenum, "GL_RENDER")
macdef GL_SELECT = $extval (GLenum, "GL_SELECT")

// Feedback
macdef GL_2D = $extval (GLenum, "GL_2D")
macdef GL_3D = $extval (GLenum, "GL_3D")
macdef GL_3D_COLOR = $extval (GLenum, "GL_3D_COLOR")
macdef GL_3D_COLOR_TEXTURE = $extval (GLenum, "GL_3D_COLOR_TEXTURE")
macdef GL_4D_COLOR_TEXTURE = $extval (GLenum, "GL_4D_COLOR_TEXTURE")
macdef GL_POINT_TOKEN = $extval (GLenum, "GL_POINT_TOKEN")
macdef GL_LINE_TOKEN = $extval (GLenum, "GL_LINE_TOKEN")
macdef GL_LINE_RESET_TOKEN = $extval (GLenum, "GL_LINE_RESET_TOKEN")
macdef GL_POLYGON_TOKEN = $extval (GLenum, "GL_POLYGON_TOKEN")
macdef GL_BITMAP_TOKEN = $extval (GLenum, "GL_BITMAP_TOKEN")
macdef GL_DRAW_PIXEL_TOKEN = $extval (GLenum, "GL_DRAW_PIXEL_TOKEN")
macdef GL_COPY_PIXEL_TOKEN = $extval (GLenum, "GL_COPY_PIXEL_TOKEN")
macdef GL_PASS_THROUGH_TOKEN = $extval (GLenum, "GL_PASS_THROUGH_TOKEN")
macdef GL_FEEDBACK_BUFFER_POINTER = $extval (GLenum, "GL_FEEDBACK_BUFFER_POINTER")
macdef GL_FEEDBACK_BUFFER_SIZE = $extval (GLenum, "GL_FEEDBACK_BUFFER_SIZE")
macdef GL_FEEDBACK_BUFFER_TYPE = $extval (GLenum, "GL_FEEDBACK_BUFFER_TYPE")

// Selection
macdef GL_SELECTION_BUFFER_POINTER = $extval (GLenum, "GL_SELECTION_BUFFER_POINTER")
macdef GL_SELECTION_BUFFER_SIZE = $extval (GLenum, "GL_SELECTION_BUFFER_SIZE")

// Fog
macdef GL_FOG = $extval (GLenum, "GL_FOG")
macdef GL_FOG_MODE = $extval (GLenum, "GL_FOG_MODE")
macdef GL_FOG_DENSITY = $extval (GLenum, "GL_FOG_DENSITY")
macdef GL_FOG_COLOR = $extval (GLenum, "GL_FOG_COLOR")
macdef GL_FOG_INDEX = $extval (GLenum, "GL_FOG_INDEX")
macdef GL_FOG_START = $extval (GLenum, "GL_FOG_START")
macdef GL_FOG_END = $extval (GLenum, "GL_FOG_END")
macdef GL_LINEAR = $extval (GLenum, "GL_LINEAR")
macdef GL_EXP = $extval (GLenum, "GL_EXP")
macdef GL_EXP2 = $extval (GLenum, "GL_EXP2")

// Logic Ops
macdef GL_LOGIC_OP = $extval (GLenum, "GL_LOGIC_OP")
macdef GL_INDEX_LOGIC_OP = $extval (GLenum, "GL_INDEX_LOGIC_OP")
macdef GL_COLOR_LOGIC_OP = $extval (GLenum, "GL_COLOR_LOGIC_OP")
macdef GL_LOGIC_OP_MODE = $extval (GLenum, "GL_LOGIC_OP_MODE")
macdef GL_CLEAR = $extval (GLenum, "GL_CLEAR")
macdef GL_SET = $extval (GLenum, "GL_SET")
macdef GL_COPY = $extval (GLenum, "GL_COPY")
macdef GL_COPY_INVERTED = $extval (GLenum, "GL_COPY_INVERTED")
macdef GL_NOOP = $extval (GLenum, "GL_NOOP")
macdef GL_INVERT = $extval (GLenum, "GL_INVERT")
macdef GL_AND = $extval (GLenum, "GL_AND")
macdef GL_NAND = $extval (GLenum, "GL_NAND")
macdef GL_OR = $extval (GLenum, "GL_OR")
macdef GL_NOR = $extval (GLenum, "GL_NOR")
macdef GL_XOR = $extval (GLenum, "GL_XOR")
macdef GL_EQUIV = $extval (GLenum, "GL_EQUIV")
macdef GL_AND_REVERSE = $extval (GLenum, "GL_AND_REVERSE")
macdef GL_AND_INVERTED = $extval (GLenum, "GL_AND_INVERTED")
macdef GL_OR_REVERSE = $extval (GLenum, "GL_OR_REVERSE")
macdef GL_OR_INVERTED = $extval (GLenum, "GL_OR_INVERTED")

// Stencil
macdef GL_STENCIL_BITS = $extval (GLenum, "GL_STENCIL_BITS")
macdef GL_STENCIL_TEST = $extval (GLenum, "GL_STENCIL_TEST")
macdef GL_STENCIL_CLEAR_VALUE = $extval (GLenum, "GL_STENCIL_CLEAR_VALUE")
macdef GL_STENCIL_FUNC = $extval (GLenum, "GL_STENCIL_FUNC")
macdef GL_STENCIL_VALUE_MASK = $extval (GLenum, "GL_STENCIL_VALUE_MASK")
macdef GL_STENCIL_FAIL = $extval (GLenum, "GL_STENCIL_FAIL")
macdef GL_STENCIL_PASS_DEPTH_FAIL = $extval (GLenum, "GL_STENCIL_PASS_DEPTH_FAIL")
macdef GL_STENCIL_PASS_DEPTH_PASS = $extval (GLenum, "GL_STENCIL_PASS_DEPTH_PASS")
macdef GL_STENCIL_REF = $extval (GLenum, "GL_STENCIL_REF")
macdef GL_STENCIL_WRITEMASK = $extval (GLenum, "GL_STENCIL_WRITEMASK")
macdef GL_STENCIL_INDEX = $extval (GLenum, "GL_STENCIL_INDEX")
macdef GL_KEEP = $extval (GLenum, "GL_KEEP")
macdef GL_REPLACE = $extval (GLenum, "GL_REPLACE")
macdef GL_INCR = $extval (GLenum, "GL_INCR")
macdef GL_DECR = $extval (GLenum, "GL_DECR")

// Buffers, Pixel Drawing/Reading
macdef GL_NONE = $extval (GLenum, "GL_NONE")
macdef GL_LEFT = $extval (GLenum, "GL_LEFT")
macdef GL_RIGHT = $extval (GLenum, "GL_RIGHT")
macdef GL_FRONT_LEFT = $extval (GLenum, "GL_FRONT_LEFT")
macdef GL_FRONT_RIGHT = $extval (GLenum, "GL_FRONT_RIGHT")
macdef GL_BACK_LEFT = $extval (GLenum, "GL_BACK_LEFT")
macdef GL_BACK_RIGHT = $extval (GLenum, "GL_BACK_RIGHT")
macdef GL_AUX0 = $extval (GLenum, "GL_AUX0")
macdef GL_AUX1 = $extval (GLenum, "GL_AUX1")
macdef GL_AUX2 = $extval (GLenum, "GL_AUX2")
macdef GL_AUX3 = $extval (GLenum, "GL_AUX3")
//
macdef GL_COLOR_INDEX = $extval (GLenum, "GL_COLOR_INDEX")
macdef GL_COLOR_INDEX_format = $extval (GLenum_format 1, "GL_COLOR_INDEX")
macdef GL_RED = $extval (GLenum, "GL_RED")
macdef GL_RED_format = $extval (GLenum_format 1, "GL_RED")
macdef GL_GREEN = $extval (GLenum, "GL_GREEN")
macdef GL_GREEN_format = $extval (GLenum_format 1, "GL_GREEN")
macdef GL_BLUE = $extval (GLenum, "GL_BLUE")
macdef GL_BLUE_format = $extval (GLenum_format 1, "GL_BLUE")
macdef GL_ALPHA = $extval (GLenum, "GL_ALPHA")
macdef GL_ALPHA_format = $extval (GLenum_format 1, "GL_ALPHA")
macdef GL_LUMINANCE = $extval (GLenum, "GL_LUMINANCE")
macdef GL_LUMINANCE_format = $extval (GLenum_format 1, "GL_LUMINANCE")
macdef GL_LUMINANCE_ALPHA = $extval (GLenum, "GL_LUMINANCE_ALPHA")
macdef GL_LUMINANCE_ALPHA_format = $extval (GLenum_format 2, "GL_LUMINANCE_ALPHA")
//
macdef GL_ALPHA_BITS = $extval (GLenum, "GL_ALPHA_BITS")
macdef GL_RED_BITS = $extval (GLenum, "GL_RED_BITS")
macdef GL_GREEN_BITS = $extval (GLenum, "GL_GREEN_BITS")
macdef GL_BLUE_BITS = $extval (GLenum, "GL_BLUE_BITS")
macdef GL_INDEX_BITS = $extval (GLenum, "GL_INDEX_BITS")
macdef GL_SUBPIXEL_BITS = $extval (GLenum, "GL_SUBPIXEL_BITS")
macdef GL_AUX_BUFFERS = $extval (GLenum, "GL_AUX_BUFFERS")
macdef GL_READ_BUFFER = $extval (GLenum, "GL_READ_BUFFER")
macdef GL_DRAW_BUFFER = $extval (GLenum, "GL_DRAW_BUFFER")
macdef GL_DOUBLEBUFFER = $extval (GLenum, "GL_DOUBLEBUFFER")
macdef GL_STEREO = $extval (GLenum, "GL_STEREO")
macdef GL_BITMAP = $extval (GLenum, "GL_BITMAP")
macdef GL_COLOR = $extval (GLenum, "GL_COLOR")
macdef GL_DEPTH = $extval (GLenum, "GL_DEPTH")
macdef GL_STENCIL = $extval (GLenum, "GL_STENCIL")
macdef GL_DITHER = $extval (GLenum, "GL_DITHER")
//
macdef GL_RGB = $extval (GLenum, "GL_RGB")
macdef GL_RGB_format = $extval (GLenum_format 3, "GL_RGB")
macdef GL_RGBA = $extval (GLenum, "GL_RGBA")
macdef GL_RGBA_format = $extval (GLenum_format 4, "GL_RGBA")
//

//
// Implementation Limits
//
macdef GL_MAX_LIST_NESTING = $extval (GLenum, "GL_MAX_LIST_NESTING")
macdef GL_MAX_EVAL_ORDER = $extval (GLenum, "GL_MAX_EVAL_ORDER")
macdef GL_MAX_LIGHTS = $extval (GLenum, "GL_MAX_LIGHTS")
macdef GL_MAX_CLIP_PLANES = $extval (GLenum, "GL_MAX_CLIP_PLANES")
macdef GL_MAX_TEXTURE_SIZE = $extval (GLenum, "GL_MAX_TEXTURE_SIZE")
macdef GL_MAX_PIXEL_MAP_TABLE = $extval (GLenum, "GL_MAX_PIXEL_MAP_TABLE")
macdef GL_MAX_ATTRIB_STACK_DEPTH = $extval (GLenum, "GL_MAX_ATTRIB_STACK_DEPTH")
macdef GL_MAX_MODELVIEW_STACK_DEPTH = $extval (GLenum, "GL_MAX_MODELVIEW_STACK_DEPTH")
macdef GL_MAX_NAME_STACK_DEPTH = $extval (GLenum, "GL_MAX_NAME_STACK_DEPTH")
macdef GL_MAX_PROJECTION_STACK_DEPTH = $extval (GLenum, "GL_MAX_PROJECTION_STACK_DEPTH")
macdef GL_MAX_TEXTURE_STACK_DEPTH = $extval (GLenum, "GL_MAX_TEXTURE_STACK_DEPTH")
macdef GL_MAX_VIEWPORT_DIMS = $extval (GLenum, "GL_MAX_VIEWPORT_DIMS")
macdef GL_MAX_CLIENT_ATTRIB_STACK_DEPTH = $extval (GLenum, "GL_MAX_CLIENT_ATTRIB_STACK_DEPTH")

// Gets
macdef GL_ATTRIB_STACK_DEPTH = $extval (GLenum, "GL_ATTRIB_STACK_DEPTH")
macdef GL_CLIENT_ATTRIB_STACK_DEPTH = $extval (GLenum, "GL_CLIENT_ATTRIB_STACK_DEPTH")
macdef GL_COLOR_CLEAR_VALUE = $extval (GLenum, "GL_COLOR_CLEAR_VALUE")
macdef GL_COLOR_WRITEMASK = $extval (GLenum, "GL_COLOR_WRITEMASK")
macdef GL_CURRENT_INDEX = $extval (GLenum, "GL_CURRENT_INDEX")
macdef GL_CURRENT_COLOR = $extval (GLenum, "GL_CURRENT_COLOR")
macdef GL_CURRENT_NORMAL = $extval (GLenum, "GL_CURRENT_NORMAL")
macdef GL_CURRENT_RASTER_COLOR = $extval (GLenum, "GL_CURRENT_RASTER_COLOR")
macdef GL_CURRENT_RASTER_DISTANCE = $extval (GLenum, "GL_CURRENT_RASTER_DISTANCE")
macdef GL_CURRENT_RASTER_INDEX = $extval (GLenum, "GL_CURRENT_RASTER_INDEX")
macdef GL_CURRENT_RASTER_POSITION = $extval (GLenum, "GL_CURRENT_RASTER_POSITION")
macdef GL_CURRENT_RASTER_TEXTURE_COORDS = $extval (GLenum, "GL_CURRENT_RASTER_TEXTURE_COORDS")
macdef GL_CURRENT_RASTER_POSITION_VALID = $extval (GLenum, "GL_CURRENT_RASTER_POSITION_VALID")
macdef GL_CURRENT_TEXTURE_COORDS = $extval (GLenum, "GL_CURRENT_TEXTURE_COORDS")
macdef GL_INDEX_CLEAR_VALUE = $extval (GLenum, "GL_INDEX_CLEAR_VALUE")
macdef GL_INDEX_MODE = $extval (GLenum, "GL_INDEX_MODE")
macdef GL_INDEX_WRITEMASK = $extval (GLenum, "GL_INDEX_WRITEMASK")
macdef GL_MODELVIEW_MATRIX = $extval (GLenum, "GL_MODELVIEW_MATRIX")
macdef GL_MODELVIEW_STACK_DEPTH = $extval (GLenum, "GL_MODELVIEW_STACK_DEPTH")
macdef GL_NAME_STACK_DEPTH = $extval (GLenum, "GL_NAME_STACK_DEPTH")
macdef GL_PROJECTION_MATRIX = $extval (GLenum, "GL_PROJECTION_MATRIX")
macdef GL_PROJECTION_STACK_DEPTH = $extval (GLenum, "GL_PROJECTION_STACK_DEPTH")
macdef GL_RENDER_MODE = $extval (GLenum, "GL_RENDER_MODE")
macdef GL_RGBA_MODE = $extval (GLenum, "GL_RGBA_MODE")
macdef GL_TEXTURE_MATRIX = $extval (GLenum, "GL_TEXTURE_MATRIX")
macdef GL_TEXTURE_STACK_DEPTH = $extval (GLenum, "GL_TEXTURE_STACK_DEPTH")
macdef GL_VIEWPORT = $extval (GLenum, "GL_VIEWPORT")

// Evaluators
macdef GL_AUTO_NORMAL = $extval (GLenum, "GL_AUTO_NORMAL")
macdef GL_MAP1_COLOR_4 = $extval (GLenum, "GL_MAP1_COLOR_4")
macdef GL_MAP1_INDEX = $extval (GLenum, "GL_MAP1_INDEX")
macdef GL_MAP1_NORMAL = $extval (GLenum, "GL_MAP1_NORMAL")
macdef GL_MAP1_TEXTURE_COORD_1 = $extval (GLenum, "GL_MAP1_TEXTURE_COORD_1")
macdef GL_MAP1_TEXTURE_COORD_2 = $extval (GLenum, "GL_MAP1_TEXTURE_COORD_2")
macdef GL_MAP1_TEXTURE_COORD_3 = $extval (GLenum, "GL_MAP1_TEXTURE_COORD_3")
macdef GL_MAP1_TEXTURE_COORD_4 = $extval (GLenum, "GL_MAP1_TEXTURE_COORD_4")
macdef GL_MAP1_VERTEX_3 = $extval (GLenum, "GL_MAP1_VERTEX_3")
macdef GL_MAP1_VERTEX_4 = $extval (GLenum, "GL_MAP1_VERTEX_4")
macdef GL_MAP2_COLOR_4 = $extval (GLenum, "GL_MAP2_COLOR_4")
macdef GL_MAP2_INDEX = $extval (GLenum, "GL_MAP2_INDEX")
macdef GL_MAP2_NORMAL = $extval (GLenum, "GL_MAP2_NORMAL")
macdef GL_MAP2_TEXTURE_COORD_1 = $extval (GLenum, "GL_MAP2_TEXTURE_COORD_1")
macdef GL_MAP2_TEXTURE_COORD_2 = $extval (GLenum, "GL_MAP2_TEXTURE_COORD_2")
macdef GL_MAP2_TEXTURE_COORD_3 = $extval (GLenum, "GL_MAP2_TEXTURE_COORD_3")
macdef GL_MAP2_TEXTURE_COORD_4 = $extval (GLenum, "GL_MAP2_TEXTURE_COORD_4")
macdef GL_MAP2_VERTEX_3 = $extval (GLenum, "GL_MAP2_VERTEX_3")
macdef GL_MAP2_VERTEX_4 = $extval (GLenum, "GL_MAP2_VERTEX_4")
macdef GL_MAP1_GRID_DOMAIN = $extval (GLenum, "GL_MAP1_GRID_DOMAIN")
macdef GL_MAP1_GRID_SEGMENTS = $extval (GLenum, "GL_MAP1_GRID_SEGMENTS")
macdef GL_MAP2_GRID_DOMAIN = $extval (GLenum, "GL_MAP2_GRID_DOMAIN")
macdef GL_MAP2_GRID_SEGMENTS = $extval (GLenum, "GL_MAP2_GRID_SEGMENTS")
macdef GL_COEFF = $extval (GLenum, "GL_COEFF")
macdef GL_ORDER = $extval (GLenum, "GL_ORDER")
macdef GL_DOMAIN = $extval (GLenum, "GL_DOMAIN")

// Hints
macdef GL_PERSPECTIVE_CORRECTION_HINT = $extval (GLenum, "GL_PERSPECTIVE_CORRECTION_HINT")
macdef GL_POINT_SMOOTH_HINT = $extval (GLenum, "GL_POINT_SMOOTH_HINT")
macdef GL_LINE_SMOOTH_HINT = $extval (GLenum, "GL_LINE_SMOOTH_HINT")
macdef GL_POLYGON_SMOOTH_HINT = $extval (GLenum, "GL_POLYGON_SMOOTH_HINT")
macdef GL_FOG_HINT = $extval (GLenum, "GL_FOG_HINT")
macdef GL_DONT_CARE = $extval (GLenum, "GL_DONT_CARE")
macdef GL_FASTEST = $extval (GLenum, "GL_FASTEST")
macdef GL_NICEST = $extval (GLenum, "GL_NICEST")

// Scissor box
macdef GL_SCISSOR_BOX = $extval (GLenum, "GL_SCISSOR_BOX")
macdef GL_SCISSOR_TEST = $extval (GLenum, "GL_SCISSOR_TEST")

// Pixel Mode / Transfer
macdef GL_MAP_COLOR = $extval (GLenum, "GL_MAP_COLOR")
macdef GL_MAP_STENCIL = $extval (GLenum, "GL_MAP_STENCIL")
macdef GL_INDEX_SHIFT = $extval (GLenum, "GL_INDEX_SHIFT")
macdef GL_INDEX_OFFSET = $extval (GLenum, "GL_INDEX_OFFSET")
macdef GL_RED_SCALE = $extval (GLenum, "GL_RED_SCALE")
macdef GL_RED_BIAS = $extval (GLenum, "GL_RED_BIAS")
macdef GL_GREEN_SCALE = $extval (GLenum, "GL_GREEN_SCALE")
macdef GL_GREEN_BIAS = $extval (GLenum, "GL_GREEN_BIAS")
macdef GL_BLUE_SCALE = $extval (GLenum, "GL_BLUE_SCALE")
macdef GL_BLUE_BIAS = $extval (GLenum, "GL_BLUE_BIAS")
macdef GL_ALPHA_SCALE = $extval (GLenum, "GL_ALPHA_SCALE")
macdef GL_ALPHA_BIAS = $extval (GLenum, "GL_ALPHA_BIAS")
macdef GL_DEPTH_SCALE = $extval (GLenum, "GL_DEPTH_SCALE")
macdef GL_DEPTH_BIAS = $extval (GLenum, "GL_DEPTH_BIAS")
macdef GL_PIXEL_MAP_S_TO_S_SIZE = $extval (GLenum, "GL_PIXEL_MAP_S_TO_S_SIZE")
macdef GL_PIXEL_MAP_I_TO_I_SIZE = $extval (GLenum, "GL_PIXEL_MAP_I_TO_I_SIZE")
macdef GL_PIXEL_MAP_I_TO_R_SIZE = $extval (GLenum, "GL_PIXEL_MAP_I_TO_R_SIZE")
macdef GL_PIXEL_MAP_I_TO_G_SIZE = $extval (GLenum, "GL_PIXEL_MAP_I_TO_G_SIZE")
macdef GL_PIXEL_MAP_I_TO_B_SIZE = $extval (GLenum, "GL_PIXEL_MAP_I_TO_B_SIZE")
macdef GL_PIXEL_MAP_I_TO_A_SIZE = $extval (GLenum, "GL_PIXEL_MAP_I_TO_A_SIZE")
macdef GL_PIXEL_MAP_R_TO_R_SIZE = $extval (GLenum, "GL_PIXEL_MAP_R_TO_R_SIZE")
macdef GL_PIXEL_MAP_G_TO_G_SIZE = $extval (GLenum, "GL_PIXEL_MAP_G_TO_G_SIZE")
macdef GL_PIXEL_MAP_B_TO_B_SIZE = $extval (GLenum, "GL_PIXEL_MAP_B_TO_B_SIZE")
macdef GL_PIXEL_MAP_A_TO_A_SIZE = $extval (GLenum, "GL_PIXEL_MAP_A_TO_A_SIZE")
macdef GL_PIXEL_MAP_S_TO_S = $extval (GLenum, "GL_PIXEL_MAP_S_TO_S")
macdef GL_PIXEL_MAP_I_TO_I = $extval (GLenum, "GL_PIXEL_MAP_I_TO_I")
macdef GL_PIXEL_MAP_I_TO_R = $extval (GLenum, "GL_PIXEL_MAP_I_TO_R")
macdef GL_PIXEL_MAP_I_TO_G = $extval (GLenum, "GL_PIXEL_MAP_I_TO_G")
macdef GL_PIXEL_MAP_I_TO_B = $extval (GLenum, "GL_PIXEL_MAP_I_TO_B")
macdef GL_PIXEL_MAP_I_TO_A = $extval (GLenum, "GL_PIXEL_MAP_I_TO_A")
macdef GL_PIXEL_MAP_R_TO_R = $extval (GLenum, "GL_PIXEL_MAP_R_TO_R")
macdef GL_PIXEL_MAP_G_TO_G = $extval (GLenum, "GL_PIXEL_MAP_G_TO_G")
macdef GL_PIXEL_MAP_B_TO_B = $extval (GLenum, "GL_PIXEL_MAP_B_TO_B")
macdef GL_PIXEL_MAP_A_TO_A = $extval (GLenum, "GL_PIXEL_MAP_A_TO_A")
macdef GL_PACK_ALIGNMENT = $extval (GLenum, "GL_PACK_ALIGNMENT")
macdef GL_PACK_LSB_FIRST = $extval (GLenum, "GL_PACK_LSB_FIRST")
macdef GL_PACK_ROW_LENGTH = $extval (GLenum, "GL_PACK_ROW_LENGTH")
macdef GL_PACK_SKIP_PIXELS = $extval (GLenum, "GL_PACK_SKIP_PIXELS")
macdef GL_PACK_SKIP_ROWS = $extval (GLenum, "GL_PACK_SKIP_ROWS")
macdef GL_PACK_SWAP_BYTES = $extval (GLenum, "GL_PACK_SWAP_BYTES")
macdef GL_UNPACK_ALIGNMENT = $extval (GLenum, "GL_UNPACK_ALIGNMENT")
macdef GL_UNPACK_LSB_FIRST = $extval (GLenum, "GL_UNPACK_LSB_FIRST")
macdef GL_UNPACK_ROW_LENGTH = $extval (GLenum, "GL_UNPACK_ROW_LENGTH")
macdef GL_UNPACK_SKIP_PIXELS = $extval (GLenum, "GL_UNPACK_SKIP_PIXELS")
macdef GL_UNPACK_SKIP_ROWS = $extval (GLenum, "GL_UNPACK_SKIP_ROWS")
macdef GL_UNPACK_SWAP_BYTES = $extval (GLenum, "GL_UNPACK_SWAP_BYTES")
macdef GL_ZOOM_X = $extval (GLenum, "GL_ZOOM_X")
macdef GL_ZOOM_Y = $extval (GLenum, "GL_ZOOM_Y")

// Texture mapping
macdef GL_TEXTURE_ENV = $extval (GLenum, "GL_TEXTURE_ENV")
macdef GL_TEXTURE_ENV_MODE = $extval (GLenum, "GL_TEXTURE_ENV_MODE")
macdef GL_TEXTURE_1D = $extval (GLenum, "GL_TEXTURE_1D")
macdef GL_TEXTURE_2D = $extval (GLenum, "GL_TEXTURE_2D")
macdef GL_TEXTURE_WRAP_S = $extval (GLenum, "GL_TEXTURE_WRAP_S")
macdef GL_TEXTURE_WRAP_T = $extval (GLenum, "GL_TEXTURE_WRAP_T")
macdef GL_TEXTURE_MAG_FILTER = $extval (GLenum, "GL_TEXTURE_MAG_FILTER")
macdef GL_TEXTURE_MIN_FILTER = $extval (GLenum, "GL_TEXTURE_MIN_FILTER")
macdef GL_TEXTURE_ENV_COLOR = $extval (GLenum, "GL_TEXTURE_ENV_COLOR")
macdef GL_TEXTURE_GEN_S = $extval (GLenum, "GL_TEXTURE_GEN_S")
macdef GL_TEXTURE_GEN_T = $extval (GLenum, "GL_TEXTURE_GEN_T")
macdef GL_TEXTURE_GEN_MODE = $extval (GLenum, "GL_TEXTURE_GEN_MODE")
macdef GL_TEXTURE_BORDER_COLOR = $extval (GLenum, "GL_TEXTURE_BORDER_COLOR")
macdef GL_TEXTURE_WIDTH = $extval (GLenum, "GL_TEXTURE_WIDTH")
macdef GL_TEXTURE_HEIGHT = $extval (GLenum, "GL_TEXTURE_HEIGHT")
macdef GL_TEXTURE_BORDER = $extval (GLenum, "GL_TEXTURE_BORDER")
macdef GL_TEXTURE_COMPONENTS = $extval (GLenum, "GL_TEXTURE_COMPONENTS")
macdef GL_TEXTURE_RED_SIZE = $extval (GLenum, "GL_TEXTURE_RED_SIZE")
macdef GL_TEXTURE_GREEN_SIZE = $extval (GLenum, "GL_TEXTURE_GREEN_SIZE")
macdef GL_TEXTURE_BLUE_SIZE = $extval (GLenum, "GL_TEXTURE_BLUE_SIZE")
macdef GL_TEXTURE_ALPHA_SIZE = $extval (GLenum, "GL_TEXTURE_ALPHA_SIZE")
macdef GL_TEXTURE_LUMINANCE_SIZE = $extval (GLenum, "GL_TEXTURE_LUMINANCE_SIZE")
macdef GL_TEXTURE_INTENSITY_SIZE = $extval (GLenum, "GL_TEXTURE_INTENSITY_SIZE")
macdef GL_NEAREST_MIPMAP_NEAREST = $extval (GLenum, "GL_NEAREST_MIPMAP_NEAREST")
macdef GL_NEAREST_MIPMAP_LINEAR = $extval (GLenum, "GL_NEAREST_MIPMAP_LINEAR")
macdef GL_LINEAR_MIPMAP_NEAREST = $extval (GLenum, "GL_LINEAR_MIPMAP_NEAREST")
macdef GL_LINEAR_MIPMAP_LINEAR = $extval (GLenum, "GL_LINEAR_MIPMAP_LINEAR")
macdef GL_OBJECT_LINEAR = $extval (GLenum, "GL_OBJECT_LINEAR")
macdef GL_OBJECT_PLANE = $extval (GLenum, "GL_OBJECT_PLANE")
macdef GL_EYE_LINEAR = $extval (GLenum, "GL_EYE_LINEAR")
macdef GL_EYE_PLANE = $extval (GLenum, "GL_EYE_PLANE")
macdef GL_SPHERE_MAP = $extval (GLenum, "GL_SPHERE_MAP")
macdef GL_DECAL = $extval (GLenum, "GL_DECAL")
macdef GL_MODULATE = $extval (GLenum, "GL_MODULATE")
macdef GL_NEAREST = $extval (GLenum, "GL_NEAREST")
macdef GL_REPEAT = $extval (GLenum, "GL_REPEAT")
macdef GL_CLAMP = $extval (GLenum, "GL_CLAMP")
macdef GL_S = $extval (GLenum, "GL_S")
macdef GL_T = $extval (GLenum, "GL_T")
macdef GL_R = $extval (GLenum, "GL_R")
macdef GL_Q = $extval (GLenum, "GL_Q")
macdef GL_TEXTURE_GEN_R = $extval (GLenum, "GL_TEXTURE_GEN_R")
macdef GL_TEXTURE_GEN_Q = $extval (GLenum, "GL_TEXTURE_GEN_Q")

// Utility
macdef GL_VENDOR = $extval (GLenum, "GL_VENDOR")
macdef GL_RENDERER = $extval (GLenum, "GL_RENDERER")
macdef GL_VERSION = $extval (GLenum, "GL_VERSION")
macdef GL_EXTENSIONS = $extval (GLenum, "GL_EXTENSIONS")

// Errors
macdef GL_NO_ERROR = $extval (GLenum, "GL_NO_ERROR")
macdef GL_INVALID_ENUM = $extval (GLenum, "GL_INVALID_ENUM")
macdef GL_INVALID_VALUE = $extval (GLenum, "GL_INVALID_VALUE")
macdef GL_INVALID_OPERATION = $extval (GLenum, "GL_INVALID_OPERATION")
macdef GL_STACK_OVERFLOW = $extval (GLenum, "GL_STACK_OVERFLOW")
macdef GL_STACK_UNDERFLOW = $extval (GLenum, "GL_STACK_UNDERFLOW")
macdef GL_OUT_OF_MEMORY = $extval (GLenum, "GL_OUT_OF_MEMORY")

// glPush/PopAttrib bits
macdef GL_CURRENT_BIT = $extval (GLbitfield, "GL_CURRENT_BIT")
macdef GL_POINT_BIT = $extval (GLbitfield, "GL_POINT_BIT")
macdef GL_LINE_BIT = $extval (GLbitfield, "GL_LINE_BIT")
macdef GL_POLYGON_BIT = $extval (GLbitfield, "GL_POLYGON_BIT")
macdef GL_POLYGON_STIPPLE_BIT = $extval (GLbitfield, "GL_POLYGON_STIPPLE_BIT")
macdef GL_PIXEL_MODE_BIT = $extval (GLbitfield, "GL_PIXEL_MODE_BIT")
macdef GL_LIGHTING_BIT = $extval (GLbitfield, "GL_LIGHTING_BIT")
macdef GL_FOG_BIT = $extval (GLbitfield, "GL_FOG_BIT")
macdef GL_DEPTH_BUFFER_BIT = $extval (GLbitfield, "GL_DEPTH_BUFFER_BIT")
macdef GL_ACCUM_BUFFER_BIT = $extval (GLbitfield, "GL_ACCUM_BUFFER_BIT")
macdef GL_STENCIL_BUFFER_BIT = $extval (GLbitfield, "GL_STENCIL_BUFFER_BIT")
macdef GL_VIEWPORT_BIT = $extval (GLbitfield, "GL_VIEWPORT_BIT")
macdef GL_TRANSFORM_BIT = $extval (GLbitfield, "GL_TRANSFORM_BIT")
macdef GL_ENABLE_BIT = $extval (GLbitfield, "GL_ENABLE_BIT")
macdef GL_COLOR_BUFFER_BIT = $extval (GLbitfield, "GL_COLOR_BUFFER_BIT")
macdef GL_HINT_BIT = $extval (GLbitfield, "GL_HINT_BIT")
macdef GL_EVAL_BIT = $extval (GLbitfield, "GL_EVAL_BIT")
macdef GL_LIST_BIT = $extval (GLbitfield, "GL_LIST_BIT")
macdef GL_TEXTURE_BIT = $extval (GLbitfield, "GL_TEXTURE_BIT")
macdef GL_SCISSOR_BIT = $extval (GLbitfield, "GL_SCISSOR_BIT")
macdef GL_ALL_ATTRIB_BITS = $extval (GLbitfield, "GL_ALL_ATTRIB_BITS")

// OpenGL 1.1
macdef GL_PROXY_TEXTURE_1D = $extval (GLenum, "GL_PROXY_TEXTURE_1D")
macdef GL_PROXY_TEXTURE_2D = $extval (GLenum, "GL_PROXY_TEXTURE_2D")
macdef GL_TEXTURE_PRIORITY = $extval (GLenum, "GL_TEXTURE_PRIORITY")
macdef GL_TEXTURE_RESIDENT = $extval (GLenum, "GL_TEXTURE_RESIDENT")
macdef GL_TEXTURE_BINDING_1D = $extval (GLenum, "GL_TEXTURE_BINDING_1D")
macdef GL_TEXTURE_BINDING_2D = $extval (GLenum, "GL_TEXTURE_BINDING_2D")
macdef GL_TEXTURE_INTERNAL_FORMAT = $extval (GLenum, "GL_TEXTURE_INTERNAL_FORMAT")
macdef GL_ALPHA4 = $extval (GLenum, "GL_ALPHA4")
macdef GL_ALPHA8 = $extval (GLenum, "GL_ALPHA8")
macdef GL_ALPHA12 = $extval (GLenum, "GL_ALPHA12")
macdef GL_ALPHA16 = $extval (GLenum, "GL_ALPHA16")
macdef GL_LUMINANCE4 = $extval (GLenum, "GL_LUMINANCE4")
macdef GL_LUMINANCE8 = $extval (GLenum, "GL_LUMINANCE8")
macdef GL_LUMINANCE12 = $extval (GLenum, "GL_LUMINANCE12")
macdef GL_LUMINANCE16 = $extval (GLenum, "GL_LUMINANCE16")
macdef GL_LUMINANCE4_ALPHA4 = $extval (GLenum, "GL_LUMINANCE4_ALPHA4")
macdef GL_LUMINANCE6_ALPHA2 = $extval (GLenum, "GL_LUMINANCE6_ALPHA2")
macdef GL_LUMINANCE8_ALPHA8 = $extval (GLenum, "GL_LUMINANCE8_ALPHA8")
macdef GL_LUMINANCE12_ALPHA4 = $extval (GLenum, "GL_LUMINANCE12_ALPHA4")
macdef GL_LUMINANCE12_ALPHA12 = $extval (GLenum, "GL_LUMINANCE12_ALPHA12")
macdef GL_LUMINANCE16_ALPHA16 = $extval (GLenum, "GL_LUMINANCE16_ALPHA16")
macdef GL_INTENSITY = $extval (GLenum, "GL_INTENSITY")
macdef GL_INTENSITY4 = $extval (GLenum, "GL_INTENSITY4")
macdef GL_INTENSITY8 = $extval (GLenum, "GL_INTENSITY8")
macdef GL_INTENSITY12 = $extval (GLenum, "GL_INTENSITY12")
macdef GL_INTENSITY16 = $extval (GLenum, "GL_INTENSITY16")
macdef GL_R3_G3_B2 = $extval (GLenum, "GL_R3_G3_B2")
macdef GL_RGB4 = $extval (GLenum, "GL_RGB4")
macdef GL_RGB5 = $extval (GLenum, "GL_RGB5")
macdef GL_RGB8 = $extval (GLenum, "GL_RGB8")
macdef GL_RGB10 = $extval (GLenum, "GL_RGB10")
macdef GL_RGB12 = $extval (GLenum, "GL_RGB12")
macdef GL_RGB16 = $extval (GLenum, "GL_RGB16")
macdef GL_RGBA2 = $extval (GLenum, "GL_RGBA2")
macdef GL_RGBA4 = $extval (GLenum, "GL_RGBA4")
macdef GL_RGB5_A1 = $extval (GLenum, "GL_RGB5_A1")
macdef GL_RGBA8 = $extval (GLenum, "GL_RGBA8")
macdef GL_RGB10_A2 = $extval (GLenum, "GL_RGB10_A2")
macdef GL_RGBA12 = $extval (GLenum, "GL_RGBA12")
macdef GL_RGBA16 = $extval (GLenum, "GL_RGBA16")
macdef GL_CLIENT_PIXEL_STORE_BIT = $extval (GLenum, "GL_CLIENT_PIXEL_STORE_BIT")
macdef GL_CLIENT_VERTEX_ARRAY_BIT = $extval (GLenum, "GL_CLIENT_VERTEX_ARRAY_BIT")
macdef GL_ALL_CLIENT_ATTRIB_BITS = $extval (GLenum, "GL_ALL_CLIENT_ATTRIB_BITS")
macdef GL_CLIENT_ALL_ATTRIB_BITS = $extval (GLenum, "GL_CLIENT_ALL_ATTRIB_BITS")

(* ****** ****** *)

//
// Miscellaneous
//

(* ****** ****** *)

fun glClearIndex
  (c: GLfloat): void = "mac#atsctrb_glClearIndex"
// end of [glClearIndex]

//

typedef glClearColor_type (a:t@ype) =
  (a(*red*), a(*green*), a(*blue*), a(*alpha*)) -<fun1> void
// end of [glClearColor_type]

symintr glClearColor

fun glClearColor_double
  : glClearColor_type (double) = "mac#atsctrb_glClearColor"
overload glClearColor with glClearColor_double

fun glClearColor_GLclampf
  : glClearColor_type (GLclampf) = "mac#atsctrb_glClearColor"
overload glClearColor with glClearColor_GLclampf

//

fun glClear (mask: GLbitfield): void = "mac#atsctrb_glClear"

fun glIndexMask (mask: GLuint): void = "mac#atsctrb_glIndexMask"

fun glColorMask (
  red: GLboolean, green: GLboolean, blue: GLboolean, alpha: GLboolean
) : void
  = "mac#atsctrb_glColorMask"

fun glAlphaFunc
  (func: GLenum, ref: GLclampf): void = "mac#atsctrb_glAlphaFunc"

fun glBlendFunc
  (sfactor: GLenum, dfactor: GLenum): void = "mac#atsctrb_glBlendFunc"

fun glLogicOp (opcode: GLenum): void = "mac#atsctrb_glLogicOp"

(* ****** ****** *)

fun glCullFace (mode: GLenum): void = "mac#atsctrb_glCullFace"
fun glFrontFace (mode: GLenum): void  = "mac#atsctrb_glFrontFace"

(* ****** ****** *)

symintr glPointSize

fun glPointSize_double
  (size: double): void = "mac#atsctrb_glPointSize"
overload glPointSize with glPointSize_double

fun glPointSize_GLfloat
  (size: GLfloat): void = "mac#atsctrb_glPointSize"
overload glPointSize with glPointSize_GLfloat

(* ****** ****** *)

symintr glLineWidth

fun glLineWidth_double
  (width: double): void = "mac#atsctrb_glLineWidth"
overload glLineWidth with glLineWidth_double

fun glLineWidth_GLfloat
  (width: GLfloat): void = "mac#atsctrb_glLineWidth"
overload glLineWidth with glLineWidth_GLfloat

(* ****** ****** *)

typedef glLineStipple_type (a:t@ype) =
  {n:int | 1 <= n; n <= 256} (int n(*factor*), a(*pattern*)) -<fun1> void

fun glLineStipple
  : glLineStipple_type (GLushort) = "mac#atsctrb_glLineStipple"
// end of [glLineStipple]

(* ****** ****** *)

fun glPolygonMode
  (face: GLenum, mode: GLenum): void
  = "mac#atsctrb_glPolygonMode"

fun glPolygonOffset
  (factor: GLfloat, units: GLfloat): void
  = "mac#atsctrb_glPolygonOffset"

fun glPolygonStipple
  (mask: &GLarray2 (GLubyte, 32, 32)): void
  = "mac#atsctrb_glPolygonStipple"

fun glGetPolygonStipple
  (mask: &GLarray2 (GLubyte, 32, 32)): void
  = "mac#atsctrb_glGetPolygonStipple"

fun glEdgeFlag (flag: GLboolean): void = "mac#atsctrb_glEdgeFlag"
fun glEdgeFlagv (flag: &GLboolean): void = "mac#atsctrb_glEdgeFlagv"

fun glScissor
  (x: GLint, y: GLint, width: GLsizei, height: GLsizei): void
  = "mac#atsctrb_glScissor"

(* ****** ****** *)

fun glClipPlane
  (plane: GLenum, eqn: &(@[GLdouble][4])): void
  = "mac#atsctrb_glClipPlane"

fun glGetClipPlane
  (plane: GLenum, eqn: &(@[GLdouble][4]) >> @[GLdouble][4]): void
  = "mac#atsctrb_glGetClipPlane"

(* ****** ****** *)

fun glDrawBuffer (mode: GLenum): void = "mac#atsctrb_glDrawBuffer"
fun glReadBuffer (mode: GLenum): void = "mac#atsctrb_glReadBuffer"

(* ****** ****** *)

fun glEnable (cap: GLenum): void = "mac#atsctrb_glEnable"
fun glDisable (cap: GLenum): void = "mac#atsctrb_glDisable"
fun glIsEnabled (cap: GLenum): GLboolean = "mac#atsctrb_glIsEnabled"

//

(*
// OpenGL 1.1
fun glEnableClientState
  (cap: GLenum): void = "mac#atsctrb_glEnableClientState"
// end of [glEnableClientState]

// version 1.1
fun glDisableClientState
  (cap: GLenum): void = "mac#atsctrb_glDisableClientState"
// end of [glDisableClientState]
*)

(* ****** ****** *)

//
// these are really unsafe functions!!!
//
typedef glGetParams_type
  (a:t@ype, n:int) = (GLenum, &(@[a?][n]) >> @[a][n]) -<fun> void
// end of [glGetParams]

fun glGetBooleanv
  : {n:nat} glGetParams_type (GLboolean, n)
  = "mac#atsctrb_glGetBooleanv"

fun glGetDoublev : {n:nat} glGetParams_type (GLdouble, n)
  = "mac#atsctrb_glGetDoublev"

fun glGetFloatv : {n:nat} glGetParams_type (GLfloat, n)
  = "mac#atsctrb_glGetFloatv"

fun glGetIntegerv : {n:nat} glGetParams_type (GLint, n)
  = "mac#atsctrb_glGetIntegerv"

(* ****** ****** *)

absview glPushAttrib_v
fun glPushAttrib
  (mask: GLbitfield): (glPushAttrib_v | void) = "mac#atsctrb_glPushAttrib"
// end of [glPushAttrib]
fun glPopAttrib (pf: glPushAttrib_v | (*none*)): void = "mac#atsctrb_glPopAttrib"

(*
// OpenGL 1.1
fun glPushClientAttrib (mask: GLbitfield): void = "mac#atsctrb_glPushClientAttrib"
fun glPopClientAttrib (): void = "mac#atsctrb_glPopClientAttrib"
*)

(* ****** ****** *)

fun glRenderMode (mode: GLenum): GLint = "mac#atsctrb_glRenderMode"

fun glGetError (): GLenum = "mac#atsctrb_glGetError"

fun glGetString (name: GLenum): string = "mac#atsctrb_glGetString"

fun glFinish (): void = "mac#atsctrb_glFinish"

fun glFlush (): void = "mac#atsctrb_glFlush"

fun glHint (target: GLenum, mode: GLenum): void = "mac#atsctrb_glHint"

(* ****** ****** *)

// Depth Buffer

fun glClearDepth
  (depth: GLclampd): void = "mac#atsctrb_glClearDepth"
// end of [glClearDepth]

fun glDepthFunc
  (func: GLenum): void = "mac#atsctrb_glDepthFunc"
fun glDepthMask
  (flag: GLboolean): void = "mac#atsctrb_glDepthMask"

fun glDepthRange (
  near_val: GLclampd, far_val: GLclampd
) : void
  = "mac#atsctrb_glDepthRange"
// end of [glDepthRange]

(* ****** ****** *)

// Accumulation Buffer

fun glClearAccum (
  red: GLfloat, green: GLfloat, blue: GLfloat, alpha: GLfloat
) : void
  = "mac#atsctrb_glClearAccum"

fun glAccum
  (opr: GLenum, value: GLfloat): void = "mac#atsctrb_glAccum"
// end of [glAccum]

(* ****** ****** *)

//
// Transformation
//

fun glMatrixMode
  (mode: GLenum): void = "mac#atsctrb_glMatrixMode"

(* ****** ****** *)

typedef
glOrtho_type (a:t@ype) = (
  a (*lft*), a (*rgt*), a (*bot*), a (*top*), a (*near_val*), a (*far_val*)
) -<fun1> void // end of [glOrtho_type]

symintr glOrtho

fun glOrtho_double
  : glOrtho_type (double) = "mac#atsctrb_glOrtho"
overload glOrtho with glOrtho_double

fun glOrtho_GLdouble
  : glOrtho_type (GLdouble) = "mac#atsctrb_glOrtho"
overload glOrtho with glOrtho_GLdouble

(* ****** ****** *)

typedef
glFrustum_type (a:t@ype) = (
  a (*lft*), a (*rgh*), a (*bot*), a (*top*), a (*near_val*), a (*far_val*)
) -<fun1> void // end of [glFrustum_type]

symintr glFrustum

fun glFrustum_double
  : glFrustum_type (double) = "mac#atsctrb_glFrustum"
overload glFrustum with glFrustum_double

fun glFrustum_GLdouble
  : glFrustum_type (GLdouble) = "mac#atsctrb_glFrustum"
overload glFrustum with glFrustum_GLdouble

(* ****** ****** *)

symintr glViewport

fun glViewport_type
  (x: int, y: int, width: int, height: int): void
  = "mac#atsctrb_glViewport"
overload glViewport with glViewport_type

fun glViewport_GLtype
  (x: GLint, y: GLint, width: GLsizei, height: GLsizei): void
  = "mac#atsctrb_glViewport"
overload glViewport with glViewport_GLtype

(* ****** ****** *)

absview glPushMatrix_v
fun glPushMatrix (): (glPushMatrix_v | void)
  = "mac#atsctrb_glPushMatrix"
fun glPopMatrix (pf: glPushMatrix_v | (*none*)): void
  = "mac#atsctrb_glPopMatrix"

(* ****** ****** *)

fun glLoadIdentity (): void = "mac#atsctrb_glLoadIdentity"

(* ****** ****** *)

typedef glLoadMatrix_type (a:t@ype) = (&(@[a][16])) -<fun1> void

fun glLoadMatrixd : glLoadMatrix_type (GLdouble)
  = "mac#atsctrb_glLoadMatrixd"
fun glLoadMatrixf : glLoadMatrix_type (GLfloat)
  = "mac#atsctrb_glLoadMatrixf"

(* ****** ****** *)

typedef glMultMatrix_type (a:t@ype) = (&(@[a][16])) -<fun1> void

fun glMultMatrixd : glMultMatrix_type (GLdouble)
  = "mac#atsctrb_glMultMatrixd"
fun glMultMatrixf : glMultMatrix_type (GLfloat)
  = "mac#atsctrb_glMultMatrixf"

(* ****** ****** *)

typedef glRotate_type (a:t@ype) =
  (a(*angle*), a(*x*), a(*y*), a(*z*)) -<fun1> void

symintr glRotated

fun glRotated_double : glRotate_type (double)
  = "mac#atsctrb_glRotated"
overload glRotated with glRotated_double

fun glRotated_GLdouble : glRotate_type (GLdouble)
  = "mac#atsctrb_glRotated"
overload glRotated with glRotated_GLdouble

fun glRotatef : glRotate_type (GLfloat) = "mac#atsctrb_glRotatef"

(* ****** ****** *)

typedef glScale_type (a:t@ype) =
  (a(*x*), a(*y*), a(*z*)) -<fun1> void

symintr glScaled

fun glScaled_double : glScale_type (double)
  = "mac#atsctrb_glScaled"
overload glScaled with glScaled_double

fun glScaled_GLdouble : glScale_type (GLdouble)
  = "mac#atsctrb_glScaled"
overload glScaled with glScaled_GLdouble

fun glScalef : glScale_type (GLfloat) = "mac#atsctrb_glScalef"

(* ****** ****** *)

typedef glTranslate_type (a:t@ype) =
  (a(*x*), a(*y*), a(*z*)) -<fun1> void

symintr glTranslated

fun glTranslated_double : glTranslate_type (double)
  = "mac#atsctrb_glTranslated"
overload glTranslated with glTranslated_double 

fun glTranslated_GLdouble : glTranslate_type (GLdouble)
  = "mac#atsctrb_glTranslated"
overload glTranslated with glTranslated_GLdouble 

fun glTranslatef
  : glTranslate_type (GLfloat) = "mac#atsctrb_glTranslatef"
// end of [glTranslatef]

(* ****** ****** *)

//
// Display Lists
//

//
// GLnewlist_v(n) indicates that [n] is a unique name
//
absview GLnewlist_v (n:int)
prfun GLnewlst_v_elim_null (pf: GLnewlist_v 0): void
absview GLlist_v (n:int) // a display list of the name [n] exists
absviewt@ype GLlist (n:int) = GLuint
castfn GLlist_encode {n:int} (pf: GLlist_v n | n: GLuint n):<> GLlist n
castfn GLlist_decode {n:int} (lst: GLlist n):<> @(GLlist_v n | GLuint n)

//
// HX-2010-06-15:
// if glIsList(lst) returns true, then we can expect a split as follows:
// (GLlist_v (n) -<lin,prf> void, GLlist_v (n))
//
fun glIsList {n:nat} (lst: GLuint n): GLboolean = "mac#atsctrb_glIsList"

fun glDeleteList {n:int} (lst: GLlist n): void = "atsctrb_glDeleteList" // function!

(*
// HX: this one is difficult to handle
fun glDeleteLists (lst: GLuint, range: GLsizei): void = "atsctrb_glDeleteLists"
*)

fun glGenList ()
  : [n:nat] @(GLnewlist_v n | GLuint n) = "atsctrb_glGenList" // function!
// end of [glGenList]
fun glGenList_exn ()
  : [n:pos] @(GLnewlist_v n | GLuint n) = "atsctrb_glGenList_exn" // function!
// end of [glGenList_exn]

(*
// HX: this one is difficult to handle
fun glGenLists (range: GLsizei): GLuint = "atsctrb_glGenLists"
*)

absview glNewList_v

symintr glNewList
fun glNewList_new {n:pos} (
    pf: GLnewlist_v n | lst: GLuint n, mode: GLenum
  ) : @(glNewList_v | GLlist n) = "atsctrb_glNewList_new"
overload glNewList with glNewList_new
fun glNewList_clear {n:int} (lst: !GLlist n, mode: GLenum): @(glNewList_v | void)
  = "atsctrb_glNewList_clear"
overload glNewList with glNewList_clear

fun glEndList
  (pf: glNewList_v | (*none*)): void = "mac#atsctrb_glEndList"

fun glCallList {n:int} (lst: !GLlist n): void = "mac#atsctrb_glCallList"

fun glListBase (base: GLuint): void = "mac#atsctrb_glListBase"
(*
// HX: this one is difficult to handle
fun glCallLists (n: GLsizei, typ: GLenum, lst: GLvoid* ): void
*)

(* ****** ****** *)

/*
** Drawing Functions
*/

absview glBegin_v

fun glBegin
  (mode: GLenum): @(glBegin_v | void) = "mac#atsctrb_glBegin"
fun glEnd (pf: glBegin_v | (*none*)): void = "mac#atsctrb_glEnd"

(* ****** ****** *)

typedef glVertex2_type
  (a:t@ype) = (a(*x*), a(*y*)) -<fun1> void
// end of [glVertex2_type]

//

symintr glVertex2d

fun glVertex2d_double
  : glVertex2_type (double) = "mac#atsctrb_glVertex2d"
overload glVertex2d with glVertex2d_double

fun glVertex2d_GLdouble
  : glVertex2_type (GLdouble) = "mac#atsctrb_glVertex2d"
overload glVertex2d with glVertex2d_GLdouble

fun glVertex2f : glVertex2_type (GLfloat) = "mac#atsctrb_glVertex2f"
fun glVertex2i : glVertex2_type (GLint) = "mac#atsctrb_glVertex2i"
fun glVertex2s : glVertex2_type (GLshort) = "mac#atsctrb_glVertex2s"

typedef glVertex2v (a:t@ype) = (&(@[a][2])) -<fun1> void

fun glVertex2dv : glVertex2v (GLdouble) = "mac#atsctrb_glVertex2dv"
fun glVertex2fv : glVertex2v (GLfloat) = "mac#atsctrb_glVertex2fv"
fun glVertex2iv : glVertex2v (GLint) = "mac#atsctrb_glVertex2iv"
fun glVertex2sv : glVertex2v (GLshort) = "mac#atsctrb_glVertex2sv"

(* ****** ****** *)

typedef glVertex3_type
  (a:t@ype) = (a(*x*), a(*y*), a(*z*)) -<fun1> void
// end of [glVertex3_type]

symintr glVertex3d

fun glVertex3d_double
  : glVertex3_type (double) = "mac#atsctrb_glVertex3d"
overload glVertex3d with glVertex3d_double

fun glVertex3d_GLdouble
  : glVertex3_type (GLdouble) = "mac#atsctrb_glVertex3d"
overload glVertex3d with glVertex3d_GLdouble

fun glVertex3f : glVertex3_type (GLfloat) = "mac#atsctrb_glVertex3f"
fun glVertex3i : glVertex3_type (GLint) = "mac#atsctrb_glVertex3i"
fun glVertex3s : glVertex3_type (GLshort) = "mac#atsctrb_glVertex3s"

typedef glVertex3v (a:t@ype) = (&(@[a][3])) -<fun1> void

fun glVertex3dv : glVertex3v (GLdouble) = "mac#atsctrb_glVertex3dv"
fun glVertex3fv : glVertex3v (GLfloat) = "mac#atsctrb_glVertex3fv"
fun glVertex3iv : glVertex3v (GLint) = "mac#atsctrb_glVertex3iv"
fun glVertex3sv : glVertex3v (GLshort) = "mac#atsctrb_glVertex3sv"

(* ****** ****** *)

typedef glVertex4_type (a:t@ype) =
  (a(*x*), a(*y*), a(*z*), a(*w*)) -<fun1> void

symintr glVertex4d

fun glVertex4d_double
  : glVertex4_type (double) = "mac#atsctrb_glVertex4d"
overload glVertex4d with glVertex4d_double

fun glVertex4d_GLdouble
  : glVertex4_type (GLdouble) = "mac#atsctrb_glVertex4d"
overload glVertex4d with glVertex4d_GLdouble

fun glVertex4f : glVertex4_type (GLfloat) = "mac#atsctrb_glVertex4f"
fun glVertex4i : glVertex4_type (GLint) = "mac#atsctrb_glVertex4i"
fun glVertex4s : glVertex4_type (GLshort) = "mac#atsctrb_glVertex4s"

typedef glVertex4v (a:t@ype) = (&(@[a][4])) -<fun1> void

fun glVertex4dv : glVertex4v (GLdouble) = "mac#atsctrb_glVertex4dv"
fun glVertex4fv : glVertex4v (GLfloat) = "mac#atsctrb_glVertex4fv"
fun glVertex4iv : glVertex4v (GLint) = "mac#atsctrb_glVertex4iv"
fun glVertex4sv : glVertex4v (GLshort) = "mac#atsctrb_glVertex4sv"

(* ****** ****** *)

typedef glNormal3_type (a:t@ype) =
  (a(*nx*), a(*ny*), a(*nz*)) -<fun1> void

fun glNormal3b : glNormal3_type (GLbyte) = "mac#atsctrb_glNormal3b"
fun glNormal3d : glNormal3_type (GLdouble) = "mac#atsctrb_glNormal3d"
fun glNormal3f : glNormal3_type (GLfloat) = "mac#atsctrb_glNormal3f"
fun glNormal3i : glNormal3_type (GLint) = "mac#atsctrb_glNormal3i"
fun glNormal3s : glNormal3_type (GLshort) = "mac#atsctrb_glNormal3s"

typedef glNormal3v_type (a:t@ype) = (&(@[a][3])) -<fun1> void

fun glNormal3bv : glNormal3v_type (GLbyte) = "mac#atsctrb_glNormal3bv"
fun glNormal3dv : glNormal3v_type (GLdouble) = "mac#atsctrb_glNormal3dv"
fun glNormal3fv : glNormal3v_type (GLfloat) = "mac#atsctrb_glNormal3fv"
fun glNormal3iv : glNormal3v_type (GLint) = "mac#atsctrb_glNormal3iv"
fun glNormal3sv : glNormal3v_type (GLshort) = "mac#atsctrb_glNormal3sv"

(* ****** ****** *)

fun glIndexd (c: GLdouble): void = "mac#atsctrb_glIndexd"
fun glIndexf (c: GLfloat): void = "mac#atsctrb_glIndexf"
fun glIndexi (c: GLint): void = "mac#atsctrb_glIndexi"
fun glIndexs (c: GLshort): void = "mac#atsctrb_glIndexs"
fun glIndexub (c: GLubyte): void = "mac#atsctrb_glIndexub" // OpenGL 1.1

(*
GLAPI void GLAPIENTRY glIndexdv( const GLdouble *c );
GLAPI void GLAPIENTRY glIndexfv( const GLfloat *c );
GLAPI void GLAPIENTRY glIndexiv( const GLint *c );
GLAPI void GLAPIENTRY glIndexsv( const GLshort *c );
GLAPI void GLAPIENTRY glIndexubv( const GLubyte *c );  /* 1.1 */
*)

(* ****** ****** *)

typedef glColor3_type (a:t@ype) =
  (a(*red*), a(*green*), a(*blue*)) -<fun1> void
// end of [glColor3_type]

fun glColor3b : glColor3_type (GLbyte) = "mac#atsctrb_glColor3b"

//

symintr glColor3d

fun glColor3d_double
  : glColor3_type (double) = "mac#atsctrb_glColor3d"
overload glColor3d with glColor3d_double

fun glColor3d_GLdouble
  : glColor3_type (GLdouble) = "mac#atsctrb_glColor3d"
overload glColor3d with glColor3d_GLdouble

//

fun glColor3f
  : glColor3_type (GLfloat) = "mac#atsctrb_glColor3f"
fun glColor3i
  : glColor3_type (GLint) = "mac#atsctrb_glColor3i"
fun glColor3s
  : glColor3_type (GLshort) = "mac#atsctrb_glColor3s"
fun glColor3ub
  : glColor3_type (GLubyte) = "mac#atsctrb_glColor3ub"
fun glColor3ui
  : glColor3_type (GLuint) = "mac#atsctrb_glColor3ui"
fun glColor3us
  : glColor3_type (GLushort) = "mac#atsctrb_glColor3us"

typedef glColor3v_type (a:t@ype) = (&(@[a][3])) -<fun1> void

fun glColor3bv
  : glColor3v_type (GLbyte) = "mac#atsctrb_glColor3bv"
fun glColor3dv
  : glColor3v_type (GLdouble) = "mac#atsctrb_glColor3dv"
fun glColor3fv
  : glColor3v_type (GLfloat) = "mac#atsctrb_glColor3fv"
fun glColor3iv
  : glColor3v_type (GLint) = "mac#atsctrb_glColor3iv"
fun glColor3iv
  : glColor3v_type (GLshort) = "mac#atsctrb_glColor3sv"
fun glColor3ubv
  : glColor3v_type (GLubyte) = "mac#atsctrb_glColor3ubv"
fun glColor3uiv
  : glColor3v_type (GLuint) = "mac#atsctrb_glColor3uiv"
fun glColor3uiv
  : glColor3v_type (GLushort) = "mac#atsctrb_glColor3usv"

(* ****** ****** *)

typedef glColor4_type (a:t@ype) =
  (a(*red*), a(*green*), a(*blue*), a(*alpha*)) -<fun1> void
// end of [glColor4_type]

//

fun glColor4b : glColor4_type (GLbyte) = "mac#atsctrb_glColor4b"

//

symintr glColor4d

fun glColor4d_double
  : glColor4_type (double) = "mac#atsctrb_glColor4d"
overload glColor4d with glColor4d_double
fun glColor4d_GLdouble
  : glColor4_type (GLdouble) = "mac#atsctrb_glColor4d"
overload glColor4d with glColor4d_GLdouble

//

fun glColor4f
  : glColor4_type (GLfloat) = "mac#atsctrb_glColor4f"
fun glColor4i
  : glColor4_type (GLint) = "mac#atsctrb_glColor4i"
fun glColor4s
  : glColor4_type (GLshort) = "mac#atsctrb_glColor4s"
fun glColor4ub
  : glColor4_type (GLubyte) = "mac#atsctrb_glColor4ub"
fun glColor4ui
  : glColor4_type (GLuint) = "mac#atsctrb_glColor4ui"
fun glColor4us
  : glColor4_type (GLushort) = "mac#atsctrb_glColor4us"

(* ****** ****** *)

typedef glColor4v_type (a:t@ype) = (&(@[a][4])) -<fun1> void
fun glColor4bv
  : glColor4v_type (GLbyte) = "mac#atsctrb_glColor4bv"
fun glColor4dv
  : glColor4v_type (GLdouble) = "mac#atsctrb_glColor4dv"
fun glColor4fv
  : glColor4v_type (GLfloat) = "mac#atsctrb_glColor4fv"
fun glColor4iv
  : glColor4v_type (GLint) = "mac#atsctrb_glColor4iv"
fun glColor4iv
  : glColor4v_type (GLshort) = "mac#atsctrb_glColor4sv"
fun glColor4ubv
  : glColor4v_type (GLubyte) = "mac#atsctrb_glColor4ubv"
fun glColor4uiv
  : glColor4v_type (GLuint) = "mac#atsctrb_glColor4uiv"
fun glColor4uiv
  : glColor4v_type (GLushort) = "mac#atsctrb_glColor4usv"

(* ****** ****** *)

typedef glTexCoord1_type (a:t@ype) = (a(*s*)) -<fun1> void

//

symintr glTexCoord1d

fun glTexCoord1d_double
  : glTexCoord1_type (double) = "mac#atsctrb_glTexCoord1d"
overload glTexCoord1d with glTexCoord1d_double

fun glTexCoord1d_GLdouble
  : glTexCoord1_type (GLdouble) = "mac#atsctrb_glTexCoord1d"
overload glTexCoord1d with glTexCoord1d_GLdouble

//

fun glTexCoord1f
  : glTexCoord1_type (GLfloat) = "mac#atsctrb_glTexCoord1f"
fun glTexCoord1i
  : glTexCoord1_type (GLint) = "mac#atsctrb_glTexCoord1i"
fun glTexCoord1s
  : glTexCoord1_type (GLshort) = "mac#atsctrb_glTexCoord1s"

(* ****** ****** *)

typedef glTexCoord2_type
  (a:t@ype) = (a(*s*), a(*t*)) -<fun1> void
// end of [glTexCoord2_type]

//

symintr glTexCoord2d

fun glTexCoord2d_double
  : glTexCoord2_type (double) = "mac#atsctrb_glTexCoord2d"
overload glTexCoord2d with glTexCoord2d_double

fun glTexCoord2d_GLdouble
  : glTexCoord2_type (GLdouble) = "mac#atsctrb_glTexCoord2d"
overload glTexCoord2d with glTexCoord2d_GLdouble

//

fun glTexCoord2f
  : glTexCoord2_type (GLfloat) = "mac#atsctrb_glTexCoord2f"
fun glTexCoord2i
  : glTexCoord2_type (GLint) = "mac#atsctrb_glTexCoord2i"
fun glTexCoord2s
  : glTexCoord2_type (GLshort) = "mac#atsctrb_glTexCoord2s"

(* ****** ****** *)

typedef glTexCoord3_type
  (a:t@ype) = (a(*s*), a(*t*), a(*r*)) -<fun1> void
// end of [glTexCoord3_type]

//

symintr glTexCoord3d

fun glTexCoord3d_double
  : glTexCoord3_type (double) = "mac#atsctrb_glTexCoord3d"
overload glTexCoord3d with glTexCoord3d_double

fun glTexCoord3d_GLdouble
  : glTexCoord3_type (GLdouble) = "mac#atsctrb_glTexCoord3d"
overload glTexCoord3d with glTexCoord3d_GLdouble

//

fun glTexCoord3f
  : glTexCoord3_type (GLfloat) = "mac#atsctrb_glTexCoord3f"
fun glTexCoord3i
  : glTexCoord3_type (GLint) = "mac#atsctrb_glTexCoord3i"
fun glTexCoord3s
  : glTexCoord3_type (GLshort) = "mac#atsctrb_glTexCoord3s"

(* ****** ****** *)

typedef glTexCoord4_type
  (a:t@ype) = (a(*s*), a(*t*), a(*r*), a(*q*)) -<fun1> void
// end of [glTexCoord4_type]

fun glTexCoord4d
  : glTexCoord4_type (GLdouble) = "mac#atsctrb_glTexCoord4d"
fun glTexCoord4f
  : glTexCoord4_type (GLfloat) = "mac#atsctrb_glTexCoord4f"
fun glTexCoord4i
  : glTexCoord4_type (GLint) = "mac#atsctrb_glTexCoord4i"
fun glTexCoord4s
  : glTexCoord4_type (GLshort) = "mac#atsctrb_glTexCoord4s"

(* ****** ****** *)

typedef glTexCoord1v_type (a:t@ype) = (&(@[a][1])) -<fun1> void
fun glTexCoord1dv
  : glTexCoord1v_type (GLdouble) = "mac#atsctrb_glTexCoord1dv"
fun glTexCoord1fv
  : glTexCoord1v_type (GLfloat) = "mac#atsctrb_glTexCoord1fv"
fun glTexCoord1iv
  : glTexCoord1v_type (GLint) = "mac#atsctrb_glTexCoord1iv"
fun glTexCoord1sv
  : glTexCoord1v_type (GLshort) = "mac#atsctrb_glTexCoord1sv"

typedef glTexCoord2v_type (a:t@ype) = (&(@[a][2])) -<fun1> void
fun glTexCoord2dv
  : glTexCoord2v_type (GLdouble) = "mac#atsctrb_glTexCoord2dv"
fun glTexCoord2fv
  : glTexCoord2v_type (GLfloat) = "mac#atsctrb_glTexCoord2fv"
fun glTexCoord2iv
  : glTexCoord2v_type (GLint) = "mac#atsctrb_glTexCoord2iv"
fun glTexCoord2sv
  : glTexCoord2v_type (GLshort) = "mac#atsctrb_glTexCoord2sv"

typedef glTexCoord3v_type (a:t@ype) = (&(@[a][3])) -<fun1> void
fun glTexCoord3dv
  : glTexCoord3v_type (GLdouble) = "mac#atsctrb_glTexCoord3dv"
fun glTexCoord3fv
  : glTexCoord3v_type (GLfloat) = "mac#atsctrb_glTexCoord3fv"
fun glTexCoord3iv
  : glTexCoord3v_type (GLint) = "mac#atsctrb_glTexCoord3iv"
fun glTexCoord3sv
  : glTexCoord3v_type (GLshort) = "mac#atsctrb_glTexCoord3sv"

typedef glTexCoord4v_type (a:t@ype) = (&(@[a][4])) -<fun1> void
fun glTexCoord4dv
  : glTexCoord4v_type (GLdouble) = "mac#atsctrb_glTexCoord4dv"
fun glTexCoord4fv
  : glTexCoord4v_type (GLfloat) = "mac#atsctrb_glTexCoord4fv"
fun glTexCoord4iv
  : glTexCoord4v_type (GLint) = "mac#atsctrb_glTexCoord4iv"
fun glTexCoord4sv
  : glTexCoord4v_type (GLshort) = "mac#atsctrb_glTexCoord4sv"

(* ****** ****** *)

typedef glRasterPos2_type
  (a:t@ype) = (a(*x*), a(*y*)) -<fun1> void
// end of [glRasterPos2_type]

symintr glRasterPos2d
fun glRasterPos2d_double
  : glRasterPos2_type (double) = "mac#atsctrb_glRasterPos2d"
overload glRasterPos2d with glRasterPos2d_double
fun glRasterPos2d_GLdouble
  : glRasterPos2_type (GLdouble) = "mac#atsctrb_glRasterPos2d"
overload glRasterPos2d with glRasterPos2d_GLdouble

fun glRasterPos2f
  : glRasterPos2_type (GLfloat) = "mac#atsctrb_glRasterPos2f"
fun glRasterPos2i
  : glRasterPos2_type (GLint) = "mac#atsctrb_glRasterPos2i"
fun glRasterPos2s
  : glRasterPos2_type (GLshort) = "mac#atsctrb_glRasterPos2s"

(* ****** ****** *)

typedef glRasterPos3_type
  (a:t@ype) = (a(*x*), a(*y*), a(*z*)) -<fun1> void
// end of [glRasterPos3_type]

fun glRasterPos3d
  : glRasterPos3_type (GLdouble) = "mac#atsctrb_glRasterPos3d"
fun glRasterPos3f
  : glRasterPos3_type (GLfloat)  = "mac#atsctrb_glRasterPos3f"
fun glRasterPos3i
  : glRasterPos3_type (GLint) = "mac#atsctrb_glRasterPos3i"
fun glRasterPos3s
  : glRasterPos3_type (GLshort) = "mac#atsctrb_glRasterPos3s"

(* ****** ****** *)

typedef glRasterPos4_type
  (a:t@ype) = (a(*x*), a(*y*), a(*z*), a(*w*)) -<fun1> void
// end of [glRasterPos4_type]

fun glRasterPos4d
  : glRasterPos4_type (GLdouble) = "mac#atsctrb_glRasterPos4d"
fun glRasterPos4f
  : glRasterPos4_type (GLfloat)  = "mac#atsctrb_glRasterPos4f"
fun glRasterPos4i
  : glRasterPos4_type (GLint) = "mac#atsctrb_glRasterPos4i"
fun glRasterPos4s
  : glRasterPos4_type (GLshort) = "mac#atsctrb_glRasterPos4s"

(* ****** ****** *)

typedef glRasterPos2v_type (a:t@ype) = (&(@[a][2])) -<fun1> void
fun glRasterPos2dv
  : glRasterPos2v_type (GLdouble) = "mac#atsctrb_glRasterPos2dv"
fun glRasterPos2fv
  : glRasterPos2v_type (GLfloat) = "mac#atsctrb_glRasterPos2fv"
fun glRasterPos2iv
  : glRasterPos2v_type (GLint) = "mac#atsctrb_glRasterPos2iv"
fun glRasterPos2sv
  : glRasterPos2v_type (GLshort) = "mac#atsctrb_glRasterPos2sv"

typedef glRasterPos3v_type (a:t@ype) = (&(@[a][3])) -<fun1> void

fun glRasterPos3dv
  : glRasterPos3v_type (GLdouble) = "mac#atsctrb_glRasterPos3dv"
fun glRasterPos3fv
  : glRasterPos3v_type (GLfloat) = "mac#atsctrb_glRasterPos3fv"
fun glRasterPos3iv
  : glRasterPos3v_type (GLint) = "mac#atsctrb_glRasterPos3iv"
fun glRasterPos3sv
  : glRasterPos3v_type (GLshort) = "mac#atsctrb_glRasterPos3sv"

typedef glRasterPos4v_type (a:t@ype) = (&(@[a][4])) -<fun1> void

fun glRasterPos4dv
  : glRasterPos4v_type (GLdouble) = "mac#atsctrb_glRasterPos4dv"
fun glRasterPos4fv
  : glRasterPos4v_type (GLfloat) = "mac#atsctrb_glRasterPos4fv"
fun glRasterPos4iv
  : glRasterPos4v_type (GLint) = "mac#atsctrb_glRasterPos4iv"
fun glRasterPos4sv
  : glRasterPos4v_type (GLshort) = "mac#atsctrb_glRasterPos4sv"

(* ****** ****** *)

typedef glRect_type (a:t@ype) =
 (a(*x1*), a(*y1*), a(*x2*), a(*y2*)) -<fun1> void

//

symintr glRectd

fun glRectd_double
  : glRect_type (double) = "mac#atsctrb_glRectd"
overload glRectd with glRectd_double

fun glRectd_GLdouble
  : glRect_type (GLdouble) = "mac#atsctrb_glRectd"
overload glRectd with glRectd_GLdouble

//

fun glRectf
  : glRect_type (GLfloat) = "mac#atsctrb_glRectf"
fun glRecti
  : glRect_type (GLint) = "mac#atsctrb_glRecti"
fun glRects
  : glRect_type (GLshort) = "mac#atsctrb_glRects"

(* ****** ****** *)

typedef glRectv_type
  (a:t@ype) = (&(@[a][2]), &(@[a][2])) -<fun1> void
// end of [glRectv_type]

fun glRectdv
  : glRectv_type (GLdouble) = "mac#atsctrb_glRectdv"
fun glRectfv
  : glRectv_type (GLfloat) = "mac#atsctrb_glRectdf"
fun glRectiv
  : glRectv_type (GLint) = "mac#atsctrb_glRectdi"
fun glRectsv
  : glRectv_type (GLshort) = "mac#atsctrb_glRectds"

(* ****** ****** *)

//
// Lighting
//

(* ****** ****** *)

fun glShadeModel
  (mode: GLenum): void = "mac#atsctrb_glShadeModel"
// end of [glShadeModel]

(* ****** ****** *)

typedef glLight_type (a:t@ype) =
  (GLenum(*light*), GLenum(*pname*), a(*param*)) -<fun1> void

fun glLightf
  : glLight_type (GLfloat) = "mac#atsctrb_glLightf"
fun glLighti
  : glLight_type (GLint) = "mac#atsctrb_glLighti"

(* ****** ****** *)

//
// these are really unsafe functions!!!
//

typedef
glLightv_type (a:t@ype, n:int) = (
  GLenum(*light*), GLenum(*pname*), &(@[a][n])(*param*)
) -<fun1> void // end of [glLightv_type]

fun glLightfv
  : {n:nat} glLightv_type (GLfloat, n) = "mac#atsctrb_glLightfv"
fun glLightiv
  : {n:nat} glLightv_type (GLint, n) = "mac#atsctrb_glLightiv"

typedef
glGetLightv_type (a:t@ype, n:int) = (
  GLenum(*light*), GLenum(*pname*), &(@[a?][n]) >> @[a][n](*param*)
) -<fun1> void
// end of [glGetLightv_type]

fun glGetLightfv
  : {n:nat} glGetLightv_type (GLfloat, n) = "mac#atsctrb_glGetLightfv"
fun glGetLightiv
  : {n:nat} glGetLightv_type (GLint, n) = "mac#atsctrb_glGetLightiv"

(* ****** ****** *)

typedef glLightModel_type
  (a:t@ype) = (GLenum(*pname*), a(*param*)) -<fun1> void
// end of [glLightModel_type]

fun glLightModelf
  : glLightModel_type (GLfloat) = "mac#atsctrb_glLightModelf"
fun glLightModeli
  : glLightModel_type (GLint) = "mac#atsctrb_glLightModeli"

(* ****** ****** *)

typedef glLightModelv_type (a:t@ype, n:int) =
  (GLenum(*pname*), &(@[a][n])(*params*)) -<fun1> void
// end of [glLightModelv_type]

fun glLightModelfv
  : {n:nat} glLightModelv_type (GLfloat, n)
  = "mac#atsctrb_glLightModelfv"

fun glLightModeliv
  : {n:nat} glLightModelv_type (GLint, n)
  = "mac#atsctrb_glLightModeliv"

(* ****** ****** *)

typedef glMaterial_type (a:t@ype) =
  (GLenum(*face*), GLenum(*pname*), a(*param*)) -<fun1> void
// end of [glMaterial_type]

fun glMaterialf
  : glMaterial_type (GLfloat)= "mac#atsctrb_glMaterialf"
fun glMateriali
  : glMaterial_type (GLint) = "mac#atsctrb_glMateriali"

(* ****** ****** *)

typedef
glMaterialv_type (a:t@ype, n:int) =
  (GLenum(*face*), GLenum(*pname*), &(@[a][n])) -<fun1> void
// end of [glMaterialv_type]

fun glMaterialfv
  : {n:nat} glMaterialv_type (GLfloat, n)
  = "mac#atsctrb_glMaterialfv"

fun glMaterialiv
  : {n:nat} glMaterialv_type (GLint, n)
  = "mac#atsctrb_glMaterialiv"

(* ****** ****** *)

typedef
glGetMaterialv_type (a:t@ype, n:int) = (
  GLenum(*face*), GLenum(*pname*), &(@[a?][n]) >> @[a][n]
) -<fun1> void // end of [glGetMaterialv_type]

fun glGetMaterialfv
  : {n:nat} glGetMaterialv_type (GLfloat, n)
  = "mac#atsctrb_glGetMaterialfv"
fun glGetMaterialiv
  : {n:nat} glGetMaterialv_type (GLint, n)
  = "mac#atsctrb_glGetMaterialiv"

(* ****** ****** *)

fun glColorMaterial
  (face: GLenum, mode: GLenum): void = "mac#atsctrb_glColorMaterial"
// end of [glColorMaterial]

(* ****** ****** *)

//
// Raster functions
//

(* ****** ****** *)

fun glPixelZoom
  (xfactor: GLfloat, yfactor: GLfloat): void = "mac#atsctrb_glPixelZoom"
// end of [glPixelZoom]

(* ****** ****** *)

typedef glPixelStore_type (a:t@ype) =
  (GLenum(*pname*), a(*param*)) -<fun1> void
// end of [glPixelStore_type]

fun glPixelStoref
  : glPixelStore_type (GLfloat) = "mac#atsctrb_glPixelStoref"
fun glPixelStorei
  : glPixelStore_type (GLint) = "mac#atsctrb_glPixelStorei"

(* ****** ****** *)

fun glBitmap {w8:nat}
  {w,h:nat | w <= 8*w8} {n:nat} {p:int | p <= n} (
  pf: MUL (w8, h, p)
| width: GLsizei w, height: GLsizei h
, xorig: GLfloat, yorig: GLfloat, xmove: GLfloat, ymove: GLfloat
, bitmap: &(@[GLubyte][n])
) : void
  = "mac#atsctrb_glBitmap"
// end of [glBitmap]

(* ****** ****** *)

fun glReadPixels
  {a:t@ype} {w,h:nat} {n:nat} (
  x: GLint, y: GLint
, w: GLsizei w, h: GLsizei h
, fmt: GLenum_format n
, type: GLenum_type a
, pixels: &GLarray3 (a, w, h, n)
) : void
  = "mac#atsctrb_glReadPixels"
// end of [glReadPixels]

fun glDrawPixels
  {a:t@ype} {w,h:nat} {n:nat} (
  w: GLsizei w, h: GLsizei h
, fmt: GLenum_format n
, type: GLenum_type a
, pixels: &GLarray3 (a, w, h, n)
) : void
  = "mac#atsctrb_glDrawPixels"
// end of [glDrawPixels]

fun glCopyPixels {w,h:nat} (
  x: GLint, y: GLint, w: GLsizei w, h: GLsizei h, type: GLenum
) : void
  = "mac#atsctrb_glCopyPixels"
// end of [glCopyPixels]

(* ****** ****** *)

//
// Texture mapping
//

typedef glTexParameter_type (a:t@ype) =
  (GLenum(*target*), GLenum(*pname*), a(*param*)) -<fun1> void
// end of [glTexParameter_type]

fun glTexParameterf
  : glTexParameter_type (GLfloat) = "mac#atsctrb_glTexParameterf"
fun glTexParameteri
  : glTexParameter_type (GLint) = "mac#atsctrb_glTexParameteri"

typedef glTexEnv_type (a:t@ype) =
  (GLenum(*target*), GLenum(*pname*), a(*param*)) -<fun1> void
// end of [glTexEnv]

fun glTexEnvf : glTexEnv_type (GLfloat) = "mac#atsctrb_glTexEnvf"
fun glTexEnvi : glTexEnv_type (GLint) = "mac#atsctrb_glTexEnvi"

(* ****** ****** *)

fun glTexImage1D
  {a:t@ype} {w:nat} {n:int} (
  target: GLenum
, level: GLint
, interalFormat: GLint
, width: GLsizei w // height = 1
, border: natLt(2)
, format: GLenum_format n
, type: GLenum_type (a)
, texels: &GLarray2 (a, w, n)
) : void
  = "mac#atsctrb_glTexImage1D"
// end of [fun]

fun glTexImage2D
  {a:t@ype} {w,h:int} {n:int} (
  target: GLenum
, level: GLint
, interalFormat: GLint
, width: GLsizei w
, height: GLsizei h
, border: natLt(2)
, format: GLenum_format n
, type: GLenum_type (a)
, texels: &GLarray3 (a, w, h, n)
) : void
  = "mac#atsctrb_glTexImage2D"
// end of [fun]

(* ****** ****** *)

//
// OpenGL 1.1
//

absviewt@ype GLtexture (int) = GLuint
viewtypedef GLtexture = [n:int] GLtexture (n)

fun glGenTexture
  (texture: &GLtexture? >> GLtexture): void
  = "atsctrb_glGenTexture" // this is a function!
// end of [glGenTexture]

fun glGenTextures
  {n:pos} (
  n: GLsizei n
, textures: &(@[GLtexture?][n]) >> @[GLtexture][n]
) : void
  = "mac#atsctrb_glGenTextures"
// end of [glGenTextures]

fun glDeleteTexture
  (texture: GLtexture): void = "atsctrb_glDeleteTexture" // this is a function!
// end of [glDeleteTexture]

fun glDeleteTextures {n:pos} (
  n: GLsizei n, textures: &(@[GLtexture][n])
) : void
  = "mac#atsctrb_glDeleteTextures"
// end of [glDeleteTextures]

fun glBindTexture
  {i:int} (
  target: GLenum, texture: !GLtexture i
) : void
  = "mac#atsctrb_glBindTexture"
// end of [glBindTexture]

(* ****** ****** *)

typedef glFog_type
  (a:t@ype) = (GLenum(*pname*), a(*param*)) -<fun1> void
typedef glFogv_type
  (a:t@ype) = {n:nat} (GLenum(*pname*), &(@[a][n])(*params*)) -<fun1> void
// end of [glFogv_type]

fun glFogf : glFog_type (GLfloat) = "mac#atsctrb_glFogf"
fun glFogi : glFog_type (GLint) = "mac#atsctrb_glFogi"
fun glFogfv : glFogv_type (GLfloat) = "mac#atsctrb_glFogfv"
fun glFogiv : glFogv_type (GLint) = "mac#atsctrb_glFogiv"

(* ****** ****** *)

//
// OpenGL 1.2
//

(* ****** ****** *)

macdef GL_RESCALE_NORMAL = $extval (GLenum, "GL_RESCALE_NORMAL")
macdef GL_CLAMP_TO_EDGE = $extval (GLenum, "GL_CLAMP_TO_EDGE")
macdef GL_MAX_ELEMENTS_VERTICES = $extval (GLenum, "GL_MAX_ELEMENTS_VERTICES")
macdef GL_MAX_ELEMENTS_INDICES = $extval (GLenum, "GL_MAX_ELEMENTS_INDICES")
//
macdef GL_BGR = $extval (GLenum, "GL_BGR")
macdef GL_BGR_format = $extval (GLenum_format 3, "GL_BGR")
macdef GL_BGRA = $extval (GLenum, "GL_BGRA")
macdef GL_BGRA_format = $extval (GLenum_format 4, "GL_BGRA")
//
macdef GL_UNSIGNED_BYTE_3_3_2 = $extval (GLenum, "GL_UNSIGNED_BYTE_3_3_2")
macdef GL_UNSIGNED_BYTE_2_3_3_REV = $extval (GLenum, "GL_UNSIGNED_BYTE_2_3_3_REV")
macdef GL_UNSIGNED_SHORT_5_6_5 = $extval (GLenum, "GL_UNSIGNED_SHORT_5_6_5")
macdef GL_UNSIGNED_SHORT_5_6_5_REV = $extval (GLenum, "GL_UNSIGNED_SHORT_5_6_5_REV")
macdef GL_UNSIGNED_SHORT_4_4_4_4 = $extval (GLenum, "GL_UNSIGNED_SHORT_4_4_4_4")
macdef GL_UNSIGNED_SHORT_4_4_4_4_REV = $extval (GLenum, "GL_UNSIGNED_SHORT_4_4_4_4_REV")
macdef GL_UNSIGNED_SHORT_5_5_5_1 = $extval (GLenum, "GL_UNSIGNED_SHORT_5_5_5_1")
macdef GL_UNSIGNED_SHORT_1_5_5_5_REV = $extval (GLenum, "GL_UNSIGNED_SHORT_1_5_5_5_REV")
macdef GL_UNSIGNED_INT_8_8_8_8 = $extval (GLenum, "GL_UNSIGNED_INT_8_8_8_8")
macdef GL_UNSIGNED_INT_8_8_8_8_REV = $extval (GLenum, "GL_UNSIGNED_INT_8_8_8_8_REV")
macdef GL_UNSIGNED_INT_10_10_10_2 = $extval (GLenum, "GL_UNSIGNED_INT_10_10_10_2")
macdef GL_UNSIGNED_INT_2_10_10_10_REV = $extval (GLenum, "GL_UNSIGNED_INT_2_10_10_10_REV")
macdef GL_LIGHT_MODEL_COLOR_CONTROL = $extval (GLenum, "GL_LIGHT_MODEL_COLOR_CONTROL")
macdef GL_SINGLE_COLOR = $extval (GLenum, "GL_SINGLE_COLOR")
macdef GL_SEPARATE_SPECULAR_COLOR = $extval (GLenum, "GL_SEPARATE_SPECULAR_COLOR")
macdef GL_TEXTURE_MIN_LOD = $extval (GLenum, "GL_TEXTURE_MIN_LOD")
macdef GL_TEXTURE_MAX_LOD = $extval (GLenum, "GL_TEXTURE_MAX_LOD")
macdef GL_TEXTURE_BASE_LEVEL = $extval (GLenum, "GL_TEXTURE_BASE_LEVEL")
macdef GL_TEXTURE_MAX_LEVEL = $extval (GLenum, "GL_TEXTURE_MAX_LEVEL")
macdef GL_SMOOTH_POINT_SIZE_RANGE = $extval (GLenum, "GL_SMOOTH_POINT_SIZE_RANGE")
macdef GL_SMOOTH_POINT_SIZE_GRANULARITY = $extval (GLenum, "GL_SMOOTH_POINT_SIZE_GRANULARITY")
macdef GL_SMOOTH_LINE_WIDTH_RANGE = $extval (GLenum, "GL_SMOOTH_LINE_WIDTH_RANGE")
macdef GL_SMOOTH_LINE_WIDTH_GRANULARITY = $extval (GLenum, "GL_SMOOTH_LINE_WIDTH_GRANULARITY")
macdef GL_ALIASED_POINT_SIZE_RANGE = $extval (GLenum, "GL_ALIASED_POINT_SIZE_RANGE")
macdef GL_ALIASED_LINE_WIDTH_RANGE = $extval (GLenum, "GL_ALIASED_LINE_WIDTH_RANGE")
macdef GL_PACK_SKIP_IMAGES = $extval (GLenum, "GL_PACK_SKIP_IMAGES")
macdef GL_PACK_IMAGE_HEIGHT = $extval (GLenum, "GL_PACK_IMAGE_HEIGHT")
macdef GL_UNPACK_SKIP_IMAGES = $extval (GLenum, "GL_UNPACK_SKIP_IMAGES")
macdef GL_UNPACK_IMAGE_HEIGHT = $extval (GLenum, "GL_UNPACK_IMAGE_HEIGHT")
macdef GL_TEXTURE_3D = $extval (GLenum, "GL_TEXTURE_3D")
macdef GL_PROXY_TEXTURE_3D = $extval (GLenum, "GL_PROXY_TEXTURE_3D")
macdef GL_TEXTURE_DEPTH = $extval (GLenum, "GL_TEXTURE_DEPTH")
macdef GL_TEXTURE_WRAP_R = $extval (GLenum, "GL_TEXTURE_WRAP_R")
macdef GL_MAX_3D_TEXTURE_SIZE = $extval (GLenum, "GL_MAX_3D_TEXTURE_SIZE")
macdef GL_TEXTURE_BINDING_3D = $extval (GLenum, "GL_TEXTURE_BINDING_3D")

(* ****** ****** *)

// GL_ARB_imaging

(* ****** ****** *)

macdef GL_CONSTANT_COLOR = $extval (GLenum, "GL_CONSTANT_COLOR")
macdef GL_ONE_MINUS_CONSTANT_COLOR = $extval (GLenum, "GL_ONE_MINUS_CONSTANT_COLOR")
macdef GL_CONSTANT_ALPHA = $extval (GLenum, "GL_CONSTANT_ALPHA")
macdef GL_ONE_MINUS_CONSTANT_ALPHA = $extval (GLenum, "GL_ONE_MINUS_CONSTANT_ALPHA")
macdef GL_COLOR_TABLE = $extval (GLenum, "GL_COLOR_TABLE")
macdef GL_POST_CONVOLUTION_COLOR_TABLE = $extval (GLenum, "GL_POST_CONVOLUTION_COLOR_TABLE")
macdef GL_POST_COLOR_MATRIX_COLOR_TABLE = $extval (GLenum, "GL_POST_COLOR_MATRIX_COLOR_TABLE")
macdef GL_PROXY_COLOR_TABLE = $extval (GLenum, "GL_PROXY_COLOR_TABLE")
macdef GL_PROXY_POST_CONVOLUTION_COLOR_TABLE = $extval (GLenum, "GL_PROXY_POST_CONVOLUTION_COLOR_TABLE")
macdef GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE = $extval (GLenum, "GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE")
macdef GL_COLOR_TABLE_SCALE = $extval (GLenum, "GL_COLOR_TABLE_SCALE")
macdef GL_COLOR_TABLE_BIAS = $extval (GLenum, "GL_COLOR_TABLE_BIAS")
macdef GL_COLOR_TABLE_FORMAT = $extval (GLenum, "GL_COLOR_TABLE_FORMAT")
macdef GL_COLOR_TABLE_WIDTH = $extval (GLenum, "GL_COLOR_TABLE_WIDTH")
macdef GL_COLOR_TABLE_RED_SIZE = $extval (GLenum, "GL_COLOR_TABLE_RED_SIZE")
macdef GL_COLOR_TABLE_GREEN_SIZE = $extval (GLenum, "GL_COLOR_TABLE_GREEN_SIZE")
macdef GL_COLOR_TABLE_BLUE_SIZE = $extval (GLenum, "GL_COLOR_TABLE_BLUE_SIZE")
macdef GL_COLOR_TABLE_ALPHA_SIZE = $extval (GLenum, "GL_COLOR_TABLE_ALPHA_SIZE")
macdef GL_COLOR_TABLE_LUMINANCE_SIZE = $extval (GLenum, "GL_COLOR_TABLE_LUMINANCE_SIZE")
macdef GL_COLOR_TABLE_INTENSITY_SIZE = $extval (GLenum, "GL_COLOR_TABLE_INTENSITY_SIZE")
macdef GL_CONVOLUTION_1D = $extval (GLenum, "GL_CONVOLUTION_1D")
macdef GL_CONVOLUTION_2D = $extval (GLenum, "GL_CONVOLUTION_2D")
macdef GL_SEPARABLE_2D = $extval (GLenum, "GL_SEPARABLE_2D")
macdef GL_CONVOLUTION_BORDER_MODE = $extval (GLenum, "GL_CONVOLUTION_BORDER_MODE")
macdef GL_CONVOLUTION_FILTER_SCALE = $extval (GLenum, "GL_CONVOLUTION_FILTER_SCALE")
macdef GL_CONVOLUTION_FILTER_BIAS = $extval (GLenum, "GL_CONVOLUTION_FILTER_BIAS")
macdef GL_REDUCE = $extval (GLenum, "GL_REDUCE")
macdef GL_CONVOLUTION_FORMAT = $extval (GLenum, "GL_CONVOLUTION_FORMAT")
macdef GL_CONVOLUTION_WIDTH = $extval (GLenum, "GL_CONVOLUTION_WIDTH")
macdef GL_CONVOLUTION_HEIGHT = $extval (GLenum, "GL_CONVOLUTION_HEIGHT")
macdef GL_MAX_CONVOLUTION_WIDTH = $extval (GLenum, "GL_MAX_CONVOLUTION_WIDTH")
macdef GL_MAX_CONVOLUTION_HEIGHT = $extval (GLenum, "GL_MAX_CONVOLUTION_HEIGHT")
macdef GL_POST_CONVOLUTION_RED_SCALE = $extval (GLenum, "GL_POST_CONVOLUTION_RED_SCALE")
macdef GL_POST_CONVOLUTION_GREEN_SCALE = $extval (GLenum, "GL_POST_CONVOLUTION_GREEN_SCALE")
macdef GL_POST_CONVOLUTION_BLUE_SCALE = $extval (GLenum, "GL_POST_CONVOLUTION_BLUE_SCALE")
macdef GL_POST_CONVOLUTION_ALPHA_SCALE = $extval (GLenum, "GL_POST_CONVOLUTION_ALPHA_SCALE")
macdef GL_POST_CONVOLUTION_RED_BIAS = $extval (GLenum, "GL_POST_CONVOLUTION_RED_BIAS")
macdef GL_POST_CONVOLUTION_GREEN_BIAS = $extval (GLenum, "GL_POST_CONVOLUTION_GREEN_BIAS")
macdef GL_POST_CONVOLUTION_BLUE_BIAS = $extval (GLenum, "GL_POST_CONVOLUTION_BLUE_BIAS")
macdef GL_POST_CONVOLUTION_ALPHA_BIAS = $extval (GLenum, "GL_POST_CONVOLUTION_ALPHA_BIAS")
macdef GL_CONSTANT_BORDER = $extval (GLenum, "GL_CONSTANT_BORDER")
macdef GL_REPLICATE_BORDER = $extval (GLenum, "GL_REPLICATE_BORDER")
macdef GL_CONVOLUTION_BORDER_COLOR = $extval (GLenum, "GL_CONVOLUTION_BORDER_COLOR")
macdef GL_COLOR_MATRIX = $extval (GLenum, "GL_COLOR_MATRIX")
macdef GL_COLOR_MATRIX_STACK_DEPTH = $extval (GLenum, "GL_COLOR_MATRIX_STACK_DEPTH")
macdef GL_MAX_COLOR_MATRIX_STACK_DEPTH = $extval (GLenum, "GL_MAX_COLOR_MATRIX_STACK_DEPTH")
macdef GL_POST_COLOR_MATRIX_RED_SCALE = $extval (GLenum, "GL_POST_COLOR_MATRIX_RED_SCALE")
macdef GL_POST_COLOR_MATRIX_GREEN_SCALE = $extval (GLenum, "GL_POST_COLOR_MATRIX_GREEN_SCALE")
macdef GL_POST_COLOR_MATRIX_BLUE_SCALE = $extval (GLenum, "GL_POST_COLOR_MATRIX_BLUE_SCALE")
macdef GL_POST_COLOR_MATRIX_ALPHA_SCALE = $extval (GLenum, "GL_POST_COLOR_MATRIX_ALPHA_SCALE")
macdef GL_POST_COLOR_MATRIX_RED_BIAS = $extval (GLenum, "GL_POST_COLOR_MATRIX_RED_BIAS")
macdef GL_POST_COLOR_MATRIX_GREEN_BIAS = $extval (GLenum, "GL_POST_COLOR_MATRIX_GREEN_BIAS")
macdef GL_POST_COLOR_MATRIX_BLUE_BIAS = $extval (GLenum, "GL_POST_COLOR_MATRIX_BLUE_BIAS")
macdef GL_POST_COLOR_MATRIX_ALPHA_BIAS = $extval (GLenum, "GL_POST_COLOR_MATRIX_ALPHA_BIAS")
macdef GL_HISTOGRAM = $extval (GLenum, "GL_HISTOGRAM")
macdef GL_PROXY_HISTOGRAM = $extval (GLenum, "GL_PROXY_HISTOGRAM")
macdef GL_HISTOGRAM_WIDTH = $extval (GLenum, "GL_HISTOGRAM_WIDTH")
macdef GL_HISTOGRAM_FORMAT = $extval (GLenum, "GL_HISTOGRAM_FORMAT")
macdef GL_HISTOGRAM_RED_SIZE = $extval (GLenum, "GL_HISTOGRAM_RED_SIZE")
macdef GL_HISTOGRAM_GREEN_SIZE = $extval (GLenum, "GL_HISTOGRAM_GREEN_SIZE")
macdef GL_HISTOGRAM_BLUE_SIZE = $extval (GLenum, "GL_HISTOGRAM_BLUE_SIZE")
macdef GL_HISTOGRAM_ALPHA_SIZE = $extval (GLenum, "GL_HISTOGRAM_ALPHA_SIZE")
macdef GL_HISTOGRAM_LUMINANCE_SIZE = $extval (GLenum, "GL_HISTOGRAM_LUMINANCE_SIZE")
macdef GL_HISTOGRAM_SINK = $extval (GLenum, "GL_HISTOGRAM_SINK")
macdef GL_MINMAX = $extval (GLenum, "GL_MINMAX")
macdef GL_MINMAX_FORMAT = $extval (GLenum, "GL_MINMAX_FORMAT")
macdef GL_MINMAX_SINK = $extval (GLenum, "GL_MINMAX_SINK")
macdef GL_TABLE_TOO_LARGE = $extval (GLenum, "GL_TABLE_TOO_LARGE")
macdef GL_BLEND_EQUATION = $extval (GLenum, "GL_BLEND_EQUATION")
macdef GL_MIN = $extval (GLenum, "GL_MIN")
macdef GL_MAX = $extval (GLenum, "GL_MAX")
macdef GL_FUNC_ADD = $extval (GLenum, "GL_FUNC_ADD")
macdef GL_FUNC_SUBTRACT = $extval (GLenum, "GL_FUNC_SUBTRACT")
macdef GL_FUNC_REVERSE_SUBTRACT = $extval (GLenum, "GL_FUNC_REVERSE_SUBTRACT")
macdef GL_BLEND_COLOR = $extval (GLenum, "GL_BLEND_COLOR")

(* ****** ****** *)

// OpenGL 1.3

(* ****** ****** *)

// multitexture
macdef GL_TEXTURE0 = $extval (GLenum, "GL_TEXTURE0")
macdef GL_TEXTURE1 = $extval (GLenum, "GL_TEXTURE1")
macdef GL_TEXTURE2 = $extval (GLenum, "GL_TEXTURE2")
macdef GL_TEXTURE3 = $extval (GLenum, "GL_TEXTURE3")
macdef GL_TEXTURE4 = $extval (GLenum, "GL_TEXTURE4")
macdef GL_TEXTURE5 = $extval (GLenum, "GL_TEXTURE5")
macdef GL_TEXTURE6 = $extval (GLenum, "GL_TEXTURE6")
macdef GL_TEXTURE7 = $extval (GLenum, "GL_TEXTURE7")
macdef GL_TEXTURE8 = $extval (GLenum, "GL_TEXTURE8")
macdef GL_TEXTURE9 = $extval (GLenum, "GL_TEXTURE9")
macdef GL_TEXTURE10 = $extval (GLenum, "GL_TEXTURE10")
macdef GL_TEXTURE11 = $extval (GLenum, "GL_TEXTURE11")
macdef GL_TEXTURE12 = $extval (GLenum, "GL_TEXTURE12")
macdef GL_TEXTURE13 = $extval (GLenum, "GL_TEXTURE13")
macdef GL_TEXTURE14 = $extval (GLenum, "GL_TEXTURE14")
macdef GL_TEXTURE15 = $extval (GLenum, "GL_TEXTURE15")
macdef GL_TEXTURE16 = $extval (GLenum, "GL_TEXTURE16")
macdef GL_TEXTURE17 = $extval (GLenum, "GL_TEXTURE17")
macdef GL_TEXTURE18 = $extval (GLenum, "GL_TEXTURE18")
macdef GL_TEXTURE19 = $extval (GLenum, "GL_TEXTURE19")
macdef GL_TEXTURE20 = $extval (GLenum, "GL_TEXTURE20")
macdef GL_TEXTURE21 = $extval (GLenum, "GL_TEXTURE21")
macdef GL_TEXTURE22 = $extval (GLenum, "GL_TEXTURE22")
macdef GL_TEXTURE23 = $extval (GLenum, "GL_TEXTURE23")
macdef GL_TEXTURE24 = $extval (GLenum, "GL_TEXTURE24")
macdef GL_TEXTURE25 = $extval (GLenum, "GL_TEXTURE25")
macdef GL_TEXTURE26 = $extval (GLenum, "GL_TEXTURE26")
macdef GL_TEXTURE27 = $extval (GLenum, "GL_TEXTURE27")
macdef GL_TEXTURE28 = $extval (GLenum, "GL_TEXTURE28")
macdef GL_TEXTURE29 = $extval (GLenum, "GL_TEXTURE29")
macdef GL_TEXTURE30 = $extval (GLenum, "GL_TEXTURE30")
macdef GL_TEXTURE31 = $extval (GLenum, "GL_TEXTURE31")
macdef GL_ACTIVE_TEXTURE = $extval (GLenum, "GL_ACTIVE_TEXTURE")
macdef GL_CLIENT_ACTIVE_TEXTURE = $extval (GLenum, "GL_CLIENT_ACTIVE_TEXTURE")
macdef GL_MAX_TEXTURE_UNITS = $extval (GLenum, "GL_MAX_TEXTURE_UNITS")

// texture_cube_map
macdef GL_NORMAL_MAP = $extval (GLenum, "GL_NORMAL_MAP")
macdef GL_REFLECTION_MAP = $extval (GLenum, "GL_REFLECTION_MAP")
macdef GL_TEXTURE_CUBE_MAP = $extval (GLenum, "GL_TEXTURE_CUBE_MAP")
macdef GL_TEXTURE_BINDING_CUBE_MAP = $extval (GLenum, "GL_TEXTURE_BINDING_CUBE_MAP")
macdef GL_TEXTURE_CUBE_MAP_POSITIVE_X = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_POSITIVE_X")
macdef GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_NEGATIVE_X")
macdef GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_POSITIVE_Y")
macdef GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_NEGATIVE_Y")
macdef GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_POSITIVE_Z")
macdef GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_NEGATIVE_Z")
macdef GL_PROXY_TEXTURE_CUBE_MAP = $extval (GLenum, "GL_PROXY_TEXTURE_CUBE_MAP")
macdef GL_MAX_CUBE_MAP_TEXTURE_SIZE = $extval (GLenum, "GL_MAX_CUBE_MAP_TEXTURE_SIZE")

// texture_compression
macdef GL_COMPRESSED_ALPHA = $extval (GLenum, "GL_COMPRESSED_ALPHA")
macdef GL_COMPRESSED_LUMINANCE = $extval (GLenum, "GL_COMPRESSED_LUMINANCE")
macdef GL_COMPRESSED_LUMINANCE_ALPHA = $extval (GLenum, "GL_COMPRESSED_LUMINANCE_ALPHA")
macdef GL_COMPRESSED_INTENSITY = $extval (GLenum, "GL_COMPRESSED_INTENSITY")
macdef GL_COMPRESSED_RGB = $extval (GLenum, "GL_COMPRESSED_RGB")
macdef GL_COMPRESSED_RGBA = $extval (GLenum, "GL_COMPRESSED_RGBA")
macdef GL_TEXTURE_COMPRESSION_HINT = $extval (GLenum, "GL_TEXTURE_COMPRESSION_HINT")
macdef GL_TEXTURE_COMPRESSED_IMAGE_SIZE = $extval (GLenum, "GL_TEXTURE_COMPRESSED_IMAGE_SIZE")
macdef GL_TEXTURE_COMPRESSED = $extval (GLenum, "GL_TEXTURE_COMPRESSED")
macdef GL_NUM_COMPRESSED_TEXTURE_FORMATS = $extval (GLenum, "GL_NUM_COMPRESSED_TEXTURE_FORMATS")
macdef GL_COMPRESSED_TEXTURE_FORMATS = $extval (GLenum, "GL_COMPRESSED_TEXTURE_FORMATS")

// multisample
macdef GL_MULTISAMPLE = $extval (GLenum, "GL_MULTISAMPLE")
macdef GL_SAMPLE_ALPHA_TO_COVERAGE = $extval (GLenum, "GL_SAMPLE_ALPHA_TO_COVERAGE")
macdef GL_SAMPLE_ALPHA_TO_ONE = $extval (GLenum, "GL_SAMPLE_ALPHA_TO_ONE")
macdef GL_SAMPLE_COVERAGE = $extval (GLenum, "GL_SAMPLE_COVERAGE")
macdef GL_SAMPLE_BUFFERS = $extval (GLenum, "GL_SAMPLE_BUFFERS")
macdef GL_SAMPLES = $extval (GLenum, "GL_SAMPLES")
macdef GL_SAMPLE_COVERAGE_VALUE = $extval (GLenum, "GL_SAMPLE_COVERAGE_VALUE")
macdef GL_SAMPLE_COVERAGE_INVERT = $extval (GLenum, "GL_SAMPLE_COVERAGE_INVERT")
macdef GL_MULTISAMPLE_BIT = $extval (GLenum, "GL_MULTISAMPLE_BIT")

// transpose_matrix
macdef GL_TRANSPOSE_MODELVIEW_MATRIX = $extval (GLenum, "GL_TRANSPOSE_MODELVIEW_MATRIX")
macdef GL_TRANSPOSE_PROJECTION_MATRIX = $extval (GLenum, "GL_TRANSPOSE_PROJECTION_MATRIX")
macdef GL_TRANSPOSE_TEXTURE_MATRIX = $extval (GLenum, "GL_TRANSPOSE_TEXTURE_MATRIX")
macdef GL_TRANSPOSE_COLOR_MATRIX = $extval (GLenum, "GL_TRANSPOSE_COLOR_MATRIX")

// texture_env_combine
macdef GL_COMBINE = $extval (GLenum, "GL_COMBINE")
macdef GL_COMBINE_RGB = $extval (GLenum, "GL_COMBINE_RGB")
macdef GL_COMBINE_ALPHA = $extval (GLenum, "GL_COMBINE_ALPHA")
macdef GL_SOURCE0_RGB = $extval (GLenum, "GL_SOURCE0_RGB")
macdef GL_SOURCE1_RGB = $extval (GLenum, "GL_SOURCE1_RGB")
macdef GL_SOURCE2_RGB = $extval (GLenum, "GL_SOURCE2_RGB")
macdef GL_SOURCE0_ALPHA = $extval (GLenum, "GL_SOURCE0_ALPHA")
macdef GL_SOURCE1_ALPHA = $extval (GLenum, "GL_SOURCE1_ALPHA")
macdef GL_SOURCE2_ALPHA = $extval (GLenum, "GL_SOURCE2_ALPHA")
macdef GL_OPERAND0_RGB = $extval (GLenum, "GL_OPERAND0_RGB")
macdef GL_OPERAND1_RGB = $extval (GLenum, "GL_OPERAND1_RGB")
macdef GL_OPERAND2_RGB = $extval (GLenum, "GL_OPERAND2_RGB")
macdef GL_OPERAND0_ALPHA = $extval (GLenum, "GL_OPERAND0_ALPHA")
macdef GL_OPERAND1_ALPHA = $extval (GLenum, "GL_OPERAND1_ALPHA")
macdef GL_OPERAND2_ALPHA = $extval (GLenum, "GL_OPERAND2_ALPHA")
macdef GL_RGB_SCALE = $extval (GLenum, "GL_RGB_SCALE")
macdef GL_ADD_SIGNED = $extval (GLenum, "GL_ADD_SIGNED")
macdef GL_INTERPOLATE = $extval (GLenum, "GL_INTERPOLATE")
macdef GL_SUBTRACT = $extval (GLenum, "GL_SUBTRACT")
macdef GL_CONSTANT = $extval (GLenum, "GL_CONSTANT")
macdef GL_PRIMARY_COLOR = $extval (GLenum, "GL_PRIMARY_COLOR")
macdef GL_PREVIOUS = $extval (GLenum, "GL_PREVIOUS")

// texture_env_dot3
macdef GL_DOT3_RGB = $extval (GLenum, "GL_DOT3_RGB")
macdef GL_DOT3_RGBA = $extval (GLenum, "GL_DOT3_RGBA")

// texture_border_clamp
macdef GL_CLAMP_TO_BORDER = $extval (GLenum, "GL_CLAMP_TO_BORDER")

(* ****** ****** *)

macdef GL_ARB_multitexture = $extval (GLenum, "GL_ARB_multitexture")
macdef GL_TEXTURE0_ARB = $extval (GLenum, "GL_TEXTURE0_ARB")
macdef GL_TEXTURE1_ARB = $extval (GLenum, "GL_TEXTURE1_ARB")
macdef GL_TEXTURE2_ARB = $extval (GLenum, "GL_TEXTURE2_ARB")
macdef GL_TEXTURE3_ARB = $extval (GLenum, "GL_TEXTURE3_ARB")
macdef GL_TEXTURE4_ARB = $extval (GLenum, "GL_TEXTURE4_ARB")
macdef GL_TEXTURE5_ARB = $extval (GLenum, "GL_TEXTURE5_ARB")
macdef GL_TEXTURE6_ARB = $extval (GLenum, "GL_TEXTURE6_ARB")
macdef GL_TEXTURE7_ARB = $extval (GLenum, "GL_TEXTURE7_ARB")
macdef GL_TEXTURE8_ARB = $extval (GLenum, "GL_TEXTURE8_ARB")
macdef GL_TEXTURE9_ARB = $extval (GLenum, "GL_TEXTURE9_ARB")
macdef GL_TEXTURE10_ARB = $extval (GLenum, "GL_TEXTURE10_ARB")
macdef GL_TEXTURE11_ARB = $extval (GLenum, "GL_TEXTURE11_ARB")
macdef GL_TEXTURE12_ARB = $extval (GLenum, "GL_TEXTURE12_ARB")
macdef GL_TEXTURE13_ARB = $extval (GLenum, "GL_TEXTURE13_ARB")
macdef GL_TEXTURE14_ARB = $extval (GLenum, "GL_TEXTURE14_ARB")
macdef GL_TEXTURE15_ARB = $extval (GLenum, "GL_TEXTURE15_ARB")
macdef GL_TEXTURE16_ARB = $extval (GLenum, "GL_TEXTURE16_ARB")
macdef GL_TEXTURE17_ARB = $extval (GLenum, "GL_TEXTURE17_ARB")
macdef GL_TEXTURE18_ARB = $extval (GLenum, "GL_TEXTURE18_ARB")
macdef GL_TEXTURE19_ARB = $extval (GLenum, "GL_TEXTURE19_ARB")
macdef GL_TEXTURE20_ARB = $extval (GLenum, "GL_TEXTURE20_ARB")
macdef GL_TEXTURE21_ARB = $extval (GLenum, "GL_TEXTURE21_ARB")
macdef GL_TEXTURE22_ARB = $extval (GLenum, "GL_TEXTURE22_ARB")
macdef GL_TEXTURE23_ARB = $extval (GLenum, "GL_TEXTURE23_ARB")
macdef GL_TEXTURE24_ARB = $extval (GLenum, "GL_TEXTURE24_ARB")
macdef GL_TEXTURE25_ARB = $extval (GLenum, "GL_TEXTURE25_ARB")
macdef GL_TEXTURE26_ARB = $extval (GLenum, "GL_TEXTURE26_ARB")
macdef GL_TEXTURE27_ARB = $extval (GLenum, "GL_TEXTURE27_ARB")
macdef GL_TEXTURE28_ARB = $extval (GLenum, "GL_TEXTURE28_ARB")
macdef GL_TEXTURE29_ARB = $extval (GLenum, "GL_TEXTURE29_ARB")
macdef GL_TEXTURE30_ARB = $extval (GLenum, "GL_TEXTURE30_ARB")
macdef GL_TEXTURE31_ARB = $extval (GLenum, "GL_TEXTURE31_ARB")
macdef GL_ACTIVE_TEXTURE_ARB = $extval (GLenum, "GL_ACTIVE_TEXTURE_ARB")
macdef GL_CLIENT_ACTIVE_TEXTURE_ARB = $extval (GLenum, "GL_CLIENT_ACTIVE_TEXTURE_ARB")
macdef GL_MAX_TEXTURE_UNITS_ARB = $extval (GLenum, "GL_MAX_TEXTURE_UNITS_ARB")
macdef GL_MESA_shader_debug = $extval (GLenum, "GL_MESA_shader_debug")
macdef GL_DEBUG_OBJECT_MESA = $extval (GLenum, "GL_DEBUG_OBJECT_MESA")
macdef GL_DEBUG_PRINT_MESA = $extval (GLenum, "GL_DEBUG_PRINT_MESA")
macdef GL_DEBUG_ASSERT_MESA = $extval (GLenum, "GL_DEBUG_ASSERT_MESA")
macdef GL_MESA_trace = $extval (GLenum, "GL_MESA_trace")
macdef GL_TRACE_ALL_BITS_MESA = $extval (GLenum, "GL_TRACE_ALL_BITS_MESA")
macdef GL_TRACE_OPERATIONS_BIT_MESA = $extval (GLenum, "GL_TRACE_OPERATIONS_BIT_MESA")
macdef GL_TRACE_PRIMITIVES_BIT_MESA = $extval (GLenum, "GL_TRACE_PRIMITIVES_BIT_MESA")
macdef GL_TRACE_ARRAYS_BIT_MESA = $extval (GLenum, "GL_TRACE_ARRAYS_BIT_MESA")
macdef GL_TRACE_TEXTURES_BIT_MESA = $extval (GLenum, "GL_TRACE_TEXTURES_BIT_MESA")
macdef GL_TRACE_PIXELS_BIT_MESA = $extval (GLenum, "GL_TRACE_PIXELS_BIT_MESA")
macdef GL_TRACE_ERRORS_BIT_MESA = $extval (GLenum, "GL_TRACE_ERRORS_BIT_MESA")
macdef GL_TRACE_MASK_MESA = $extval (GLenum, "GL_TRACE_MASK_MESA")
macdef GL_TRACE_NAME_MESA = $extval (GLenum, "GL_TRACE_NAME_MESA")
macdef GL_MESA_packed_depth_stencil = $extval (GLenum, "GL_MESA_packed_depth_stencil")
macdef GL_DEPTH_STENCIL_MESA = $extval (GLenum, "GL_DEPTH_STENCIL_MESA")
macdef GL_UNSIGNED_INT_24_8_MESA = $extval (GLenum, "GL_UNSIGNED_INT_24_8_MESA")
macdef GL_UNSIGNED_INT_8_24_REV_MESA = $extval (GLenum, "GL_UNSIGNED_INT_8_24_REV_MESA")
macdef GL_UNSIGNED_SHORT_15_1_MESA = $extval (GLenum, "GL_UNSIGNED_SHORT_15_1_MESA")
macdef GL_UNSIGNED_SHORT_1_15_REV_MESA = $extval (GLenum, "GL_UNSIGNED_SHORT_1_15_REV_MESA")
macdef GL_MESA_program_debug = $extval (GLenum, "GL_MESA_program_debug")
macdef GL_FRAGMENT_PROGRAM_POSITION_MESA = $extval (GLenum, "GL_FRAGMENT_PROGRAM_POSITION_MESA")
macdef GL_FRAGMENT_PROGRAM_CALLBACK_MESA = $extval (GLenum, "GL_FRAGMENT_PROGRAM_CALLBACK_MESA")
macdef GL_FRAGMENT_PROGRAM_CALLBACK_FUNC_MESA = $extval (GLenum, "GL_FRAGMENT_PROGRAM_CALLBACK_FUNC_MESA")
macdef GL_FRAGMENT_PROGRAM_CALLBACK_DATA_MESA = $extval (GLenum, "GL_FRAGMENT_PROGRAM_CALLBACK_DATA_MESA")
macdef GL_VERTEX_PROGRAM_POSITION_MESA = $extval (GLenum, "GL_VERTEX_PROGRAM_POSITION_MESA")
macdef GL_VERTEX_PROGRAM_CALLBACK_MESA = $extval (GLenum, "GL_VERTEX_PROGRAM_CALLBACK_MESA")
macdef GL_VERTEX_PROGRAM_CALLBACK_FUNC_MESA = $extval (GLenum, "GL_VERTEX_PROGRAM_CALLBACK_FUNC_MESA")
macdef GL_VERTEX_PROGRAM_CALLBACK_DATA_MESA = $extval (GLenum, "GL_VERTEX_PROGRAM_CALLBACK_DATA_MESA")
macdef GL_ATI_blend_equation_separate = $extval (GLenum, "GL_ATI_blend_equation_separate")
macdef GL_ALPHA_BLEND_EQUATION_ATI = $extval (GLenum, "GL_ALPHA_BLEND_EQUATION_ATI")

(* ****** ****** *)

(* end of [gl.sats] *)
