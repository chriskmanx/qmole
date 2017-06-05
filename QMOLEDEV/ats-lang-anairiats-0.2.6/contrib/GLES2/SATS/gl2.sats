(* ****** ****** *)
//
// Author of the file: Artyom Shalkhakov
// Starting time: May 13, 2011
//
(* ****** ****** *)
//
// License: GNU LESSER GENERAL PUBLIC LICENSE version 2.1
//
(* ****** ****** *)

%{#
#include "prelude/CATS/integer_ptr.cats"
#include "contrib/GLES2/CATS/gl2.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no staloading at run-time

(* ****** ****** *)

typedef GLvoid = void

abst@ype GLenum = $extype"ats_GLenum_type"
fun eq_GLenum_GLenum
  (x1: GLenum, x2: GLenum):<> bool = "atsctrb_eq_GLenum_GLenum"
overload = with eq_GLenum_GLenum

fun neq_GLenum_GLenum
  (x1: GLenum, x2: GLenum):<> bool = "atsctrb_neq_GLenum_GLenum"
overload <> with neq_GLenum_GLenum

abst@ype GLenum_type (a:t@ype) = GLenum
abst@ype GLenum_format (n:int) = GLenum

(* ****** ****** *)

abst@ype
GLboolean = $extype"ats_GLboolean_type"

fun eq_GLboolean_GLboolean (x: GLboolean, y: GLboolean):<> bool
  = "atsctrb_eq_GLboolean_GLboolean" // AS: function!
overload = with eq_GLboolean_GLboolean

fun neq_GLboolean_GLboolean (x: GLboolean, y: GLboolean):<> bool
  = "atsctrb_neq_GLboolean_GLboolean" // AS: function!
overload <> with neq_GLboolean_GLboolean

(* ****** ****** *)

abst@ype
GLbitfield = $extype"ats_GLbitfield_type"

(* ****** ****** *)

abst@ype GLbyte = $extype"ats_GLbyte_type"
castfn GLbyte_of_byte (x: byte):<> GLbyte

abst@ype GLubyte = $extype"ats_GLubyte_type"
castfn GLubyte_of_byte (x: byte):<> GLubyte

(* ****** ****** *)

abst@ype GLshort = $extype"ats_GLshort_type"
abst@ype GLushort = $extype"ats_GLushort_type"

(* ****** ****** *)

abst@ype
GLint (n:int) = $extype"ats_GLint_type"
typedef GLint = [n:int] GLint (n)
castfn GLint_of_int (x: int):<> GLint
castfn int_of_GLint (x: GLint):<> int
castfn GLint_of_int1 {n:int} (x: int n):<> GLint n
castfn int1_of_GLint {n:int} (x: GLint n):<> int n
castfn GLint_of_GLenum (x: GLenum):<> GLint

abst@ype
GLuint (n:int) = $extype"ats_GLuint_type"
typedef GLuint = [n:nat] GLuint (n)
castfn GLuint_of_uint (x: uint):<> GLuint
castfn uint_of_GLuint (x: GLuint):<> uint
castfn GLuint_of_uint1 {n:int} (x: uint n):<> GLuint n
castfn uint1_of_GLuint {n:int} (x: GLuint n):<> uint n

(* ****** ****** *)

abst@ype
GLsizei (i: int) = $extype"ats_GLsizei_type"
typedef GLsizei = [i:int] GLsizei (i)

(* ****** ****** *)

abst@ype
GLfloat = $extype"ats_GLfloat_type" // AS: single precision
castfn GLfloat_of_float (x: float):<> GLfloat
castfn float_of_GLfloat (x: GLfloat):<> float
overload float_of with float_of_GLfloat

abst@ype
GLclampf = $extype"ats_GLclampf_type"
castfn GLclampf_of_float (x: float):<> GLclampf

(* ****** ****** *)

abst@ype
GLfixed = $extype"ats_GLfixed_type" // AS: 32-bit integer
castfn GLfixed_of_uint32 (x: uint32):<> GLfixed
castfn uint32_of_GLfixed (x: GLfixed):<> uint32

(* ****** ****** *)

abst@ype
GLintptr = $extype"ats_GLinptr_type"

(* ****** ****** *)

abst@ype
GLsizeiptr (i: int) = $extype"ats_GLsizeiptr_type"
typedef GLsizeiptr = [i:int] GLsizeiptr (i)

(* ****** ****** *)

fun GLbyte_of_int (x: int):<> GLbyte = "atsctrb_GLbyte_of_int"
fun GLubyte_of_int (x: int):<> GLubyte = "atsctrb_GLubyte_of_int"
fun GLubyte_of_uint (x: uint):<> GLubyte = "atsctrb_GLubyte_of_uint"

fun GLshort_of_int (x: int):<> GLshort = "atsctrb_GLshort_of_int"
fun GLushort_of_int (x: int):<> GLushort = "atsctrb_GLushort_of_int"
fun GLushort_of_uint (x: uint):<> GLushort = "atsctrb_GLushort_of_uint"

fun GLsizei_of_size1 {i:int} (x: size_t i): GLsizei i = "atsctrb_GLsizei_of_size"
fun GLsizei_of_int1 {i:int} (x: int i): GLsizei i = "atsctrb_GLsizei_of_int"

fun GLsizeiptr_of_uintptr1 {i:int} (x: uintptr i): GLsizeiptr i = "atsctrb_GLsizeiptr_of_uintptr"
fun GLsizeiptr_of_int1 {i:nat} (x: int i): GLsizeiptr i = "atsctrb_GLsizeiptr_of_int"

fun GLfloat_of_int (x: int):<> GLfloat = "atsctrb_GLfloat_of_int"
fun GLfloat_of_double (x: double):<> GLfloat = "atsctrb_GLfloat_of_double"

fun GLfixed_of_int (x: int):<> GLfixed = "atsctrb_GLfixed_of_int"

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
overload GLint with GLint_of_int // AS: castfn
overload GLint with GLint_of_int1 // AS: castfn
overload GLint with GLint_of_GLenum // AS: castfn
overload GLuint with GLuint_of_uint // AS: castfn
overload GLuint with GLuint_of_uint1 // AS: castfn

symintr GLsizei
overload GLsizei with GLsizei_of_size1
overload GLsizei with GLsizei_of_int1

symintr GLfloat GLclampf
overload GLfloat with GLfloat_of_int
overload GLfloat with GLfloat_of_float
overload GLfloat with GLfloat_of_double
overload GLclampf with GLclampf_of_float

(* ****** ****** *)

fun lor_GLbitfield_GLbitfield
  (b1: GLbitfield, b2: GLbitfield): GLbitfield
  = "atsctrb_lor_GLbitfield_GLbitfield"
overload lor with lor_GLbitfield_GLbitfield

(* ****** ****** *)

abst@ype GLarray2 (a:t@ype, w:int, n:int) // AS: for two-dimensional arrays
abst@ype GLarray3 (a:t@ype, w:int, h:int, n:int) // AS: for three-dimensional arrays

(* ****** ****** *)

(* ClearBufferMask *)
macdef GL_DEPTH_BUFFER_BIT = $extval (GLbitfield, "GL_DEPTH_BUFFER_BIT")
macdef GL_STENCIL_BUFFER_BIT = $extval (GLbitfield, "GL_STENCIL_BUFFER_BIT")
macdef GL_COLOR_BUFFER_BIT = $extval (GLbitfield, "GL_COLOR_BUFFER_BIT")

(* Boolean *)
macdef GL_TRUE = $extval (GLboolean, "GL_TRUE")
macdef GL_FALSE = $extval (GLboolean, "GL_FALSE")

(* BeginMode *)
macdef GL_POINTS = $extval (GLenum, "GL_POINTS")
macdef GL_LINES = $extval (GLenum, "GL_LINES")
macdef GL_LINE_LOOP = $extval (GLenum, "GL_LINE_LOOP")
macdef GL_LINE_STRIP = $extval (GLenum, "GL_LINE_STRIP")
macdef GL_TRIANGLES = $extval (GLenum, "GL_TRIANGLES")
macdef GL_TRIANGLE_STRIP = $extval (GLenum, "GL_TRIANGLE_STRIP")
macdef GL_TRIANGLE_FAN = $extval (GLenum, "GL_TRIANGLE_FAN")

(* Alpha function (not supported in ES20) *)

(* BlendingFactorDest *)
macdef GL_ZERO = $extval (GLenum, "GL_ZERO")
macdef GL_ONE = $extval (GLenum, "GL_ONE")
macdef GL_SRC_COLOR = $extval (GLenum, "GL_SRC_COLOR")
macdef GL_ONE_MINUS_SRC_COLOR = $extval (GLenum, "GL_ONE_MINUS_SRC_COLOR")
macdef GL_SRC_ALPHA = $extval (GLenum, "GL_SRC_ALPHA")
macdef GL_ONE_MINUS_SRC_ALPHA = $extval (GLenum, "GL_ONE_MINUS_SRC_ALPHA")
macdef GL_DST_ALPHA = $extval (GLenum, "GL_DST_ALPHA")
macdef GL_ONE_MINUS_DST_ALPHA = $extval (GLenum, "GL_ONE_MINUS_DST_ALPHA")

(* BlendingFactorSrc *)
// GL_ONE, GL_ZERO
macdef GL_DST_COLOR = $extval (GLenum, "GL_DST_COLOR")
macdef GL_ONE_MINUS_DST_COLOR = $extval (GLenum, "GL_ONE_MINUS_DST_COLOR")
macdef GL_SRC_ALPHA_SATURATE = $extval (GLenum, "GL_SRC_ALPHA_SATURATE")
// GL_SRC_ALPHA
// GL_ONE_MINUS_SRC_ALPHA
// GL_DST_ALPHA
// GL_ONE_MINUS_DST_ALPHA

(* BlendEquationSeparate *)
macdef GL_FUNC_ADD = $extval (GLenum, "GL_FUNC_ADD")
macdef GL_BLEND_EQUATION = $extval (GLenum, "GL_BLEND_EQUATION")
macdef GL_BLEND_EQUATION_RGB = $extval (GLenum, "GL_BLEND_EQUATION_RGB") // same as GL_BLEND_EQUATION
macdef GL_BLEND_EQUATION_ALPHA = $extval (GLenum, "GL_BLEND_EQUATION_ALPHA")

(* BlendSubtract *)
macdef GL_FUNC_SUBTRACT = $extval (GLenum, "GL_FUNC_SUBTRACT")
macdef GL_FUNC_REVERSE_SUBTRACT = $extval (GLenum, "GL_FUNC_REVERSE_SUBTRACT")

(* Separate Blend Functions *)
macdef GL_BLEND_DST_RGB = $extval (GLenum, "GL_BLEND_DST_RGB")
macdef GL_BLEND_SRC_RGB = $extval (GLenum, "GL_BLEND_SRC_RGB")
macdef GL_BLEND_DST_ALPHA = $extval (GLenum, "GL_BLEND_DST_ALPHA")
macdef GL_BLEND_SRC_ALPHA = $extval (GLenum, "GL_BLEND_SRC_ALPHA")
macdef GL_CONSTANT_COLOR = $extval (GLenum, "GL_CONSTANT_COLOR")
macdef GL_ONE_MINUS_CONSTANT_COLOR = $extval (GLenum, "GL_ONE_MINUS_CONSTANT_COLOR")
macdef GL_CONSTANT_ALPHA = $extval (GLenum, "GL_CONSTANT_ALPHA")
macdef GL_ONE_MINUS_CONSTANT_ALPHA = $extval (GLenum, "GL_ONE_MINUS_CONSTANT_ALPHA")
macdef GL_BLEND_COLOR = $extval (GLenum, "GL_BLEND_COLOR")

(* Buffer Objects *)
macdef GL_ARRAY_BUFFER = $extval (GLenum, "GL_ARRAY_BUFFER")
macdef GL_ELEMENT_ARRAY_BUFFER = $extval (GLenum, "GL_ELEMENT_ARRAY_BUFFER")
macdef GL_ARRAY_BUFFER_BINDING = $extval (GLenum, "GL_ARRAY_BUFFER_BINDING")
macdef GL_ELEMENT_ARRAY_BUFFER_BINDING = $extval (GLenum, "GL_ELEMENT_ARRAY_BUFFER_BINDING")
macdef GL_STREAM_DRAW = $extval (GLenum, "GL_STREAM_DRAW")
macdef GL_STATIC_DRAW = $extval (GLenum, "GL_STATIC_DRAW")
macdef GL_DYNAMIC_DRAW = $extval (GLenum, "GL_DYNAMIC_DRAW")
macdef GL_BUFFER_SIZE = $extval (GLenum, "GL_BUFFER_SIZE")
macdef GL_BUFFER_USAGE = $extval (GLenum, "GL_BUFFER_USAGE")
macdef GL_CURRENT_VERTEX_ATTRIB = $extval (GLenum, "GL_CURRENT_VERTEX_ATTRIB")

(* CullFaceMode *)
macdef GL_FRONT = $extval (GLenum, "GL_FRONT")
macdef GL_BACK = $extval (GLenum, "GL_BACK")
macdef GL_FRONT_AND_BACK = $extval (GLenum, "GL_FRONT_AND_BACK")

(* DepthFunction *)
// GL_NEVER
// GL_LESS
// GL_EQUAL
// GL_LEQUAL
// GL_GREATER
// GL_GEQUAL
// GL_ALWAYS

(* EnableCap *)
macdef GL_TEXTURE_2D = $extval (GLenum, "GL_TEXTURE_2D")
macdef GL_CULL_FACE = $extval (GLenum, "GL_CULL_FACE")
macdef GL_BLEND = $extval (GLenum, "GL_BLEND")
macdef GL_DITHER = $extval (GLenum, "GL_DITHER")
macdef GL_STENCIL_TEST = $extval (GLenum, "GL_STENCIL_TEST")
macdef GL_DEPTH_TEST = $extval (GLenum, "GL_DEPTH_TEST")
macdef GL_SCISSOR_TEST = $extval (GLenum, "GL_SCISSOR_TEST")
macdef GL_POLYGON_OFFSET_FILL = $extval (GLenum, "GL_POLYGON_OFFSET_FILL")
macdef GL_SAMPLE_ALPHA_TO_COVERAGE = $extval (GLenum, "GL_SAMPLE_ALPHA_TO_COVERAGE")
macdef GL_SAMPLE_COVERAGE = $extval (GLenum, "GL_SAMPLE_COVERAGE")

(* ErrorCode *)
macdef GL_NO_ERROR = $extval (GLenum, "GL_NO_ERROR")
macdef GL_INVALID_ENUM = $extval (GLenum, "GL_INVALID_ENUM")
macdef GL_INVALID_VALUE = $extval (GLenum, "GL_INVALID_VALUE")
macdef GL_INVALID_OPERATION = $extval (GLenum, "GL_INVALID_OPERATION")
macdef GL_OUT_OF_MEMORY = $extval (GLenum, "GL_OUT_OF_MEMORY")

(* FrontFaceDirection *)
macdef GL_CW = $extval (GLenum, "GL_CW")
macdef GL_CCW = $extval (GLenum, "GL_CCW")

(* GetPName *)
macdef GL_LINE_WIDTH = $extval (GLenum, "GL_LINE_WIDTH")
macdef GL_ALIASED_POINT_SIZE_RANGE = $extval (GLenum, "GL_ALIASED_POINT_SIZE_RANGE")
macdef GL_ALIASED_LINE_WIDTH_RANGE = $extval (GLenum, "GL_ALIASED_LINE_WIDTH_RANGE")
macdef GL_CULL_FACE_MODE = $extval (GLenum, "GL_CULL_FACE_MODE")
macdef GL_FRONT_FACE = $extval (GLenum, "GL_FRONT_FACE")
macdef GL_DEPTH_RANGE = $extval (GLenum, "GL_DEPTH_RANGE")
macdef GL_DEPTH_WRITEMASK = $extval (GLenum, "GL_DEPTH_WRITEMASK")
macdef GL_DEPTH_CLEAR_VALUE = $extval (GLenum, "GL_DEPTH_CLEAR_VALUE")
macdef GL_DEPTH_FUNC = $extval (GLenum, "GL_DEPTH_FUNC")
macdef GL_STENCIL_CLEAR_VALUE = $extval (GLenum, "GL_STENCIL_CLEAR_VALUE")
macdef GL_STENCIL_FUNC = $extval (GLenum, "GL_STENCIL_FUNC")
macdef GL_STENCIL_FAIL = $extval (GLenum, "GL_STENCIL_FAIL")
macdef GL_STENCIL_PASS_DEPTH_FAIL = $extval (GLenum, "GL_STENCIL_PASS_DEPTH_FAIL")
macdef GL_STENCIL_PASS_DEPTH_PASS = $extval (GLenum, "GL_STENCIL_PASS_DEPTH_PASS")
macdef GL_STENCIL_REF = $extval (GLenum, "GL_STENCIL_REF")
macdef GL_STENCIL_VALUE_MASK = $extval (GLenum, "GL_STENCIL_VALUE_MASK")
macdef GL_STENCIL_WRITEMASK = $extval (GLenum, "GL_STENCIL_WRITEMASK")
macdef GL_STENCIL_BACK_FUNC = $extval (GLenum, "GL_STENCIL_BACK_FUNC")
macdef GL_STENCIL_BACK_FAIL = $extval (GLenum, "GL_STENCIL_BACK_FAIL")
macdef GL_STENCIL_BACK_PASS_DEPTH_FAIL = $extval (GLenum, "GL_STENCIL_BACK_PASS_DEPTH_FAIL")
macdef GL_STENCIL_BACK_PASS_DEPTH_PASS = $extval (GLenum, "GL_STENCIL_BACK_PASS_DEPTH_PASS")
macdef GL_STENCIL_BACK_REF = $extval (GLenum, "GL_STENCIL_BACK_REF")
macdef GL_STENCIL_BACK_VALUE_MASK = $extval (GLenum, "GL_STENCIL_BACK_VALUE_MASK")
macdef GL_STENCIL_BACK_WRITEMASK = $extval (GLenum, "GL_STENCIL_BACK_WRITEMASK")
macdef GL_VIEWPORT = $extval (GLenum, "GL_VIEWPORT")
macdef GL_SCISSOR_BOX = $extval (GLenum, "GL_SCISSOR_BOX")
// GL_SCISSOR_TEST
macdef GL_COLOR_CLEAR_VALUE = $extval (GLenum, "GL_COLOR_CLEAR_VALUE")
macdef GL_COLOR_WRITEMASK = $extval (GLenum, "GL_COLOR_WRITEMASK")
macdef GL_UNPACK_ALIGNMENT = $extval (GLenum, "GL_UNPACK_ALIGNMENT")
macdef GL_PACK_ALIGNMENT = $extval (GLenum, "GL_PACK_ALIGNMENT")
macdef GL_MAX_TEXTURE_SIZE = $extval (GLenum, "GL_MAX_TEXTURE_SIZE")
macdef GL_MAX_VIEWPORT_DIMS = $extval (GLenum, "GL_MAX_VIEWPORT_DIMS")
macdef GL_SUBPIXEL_BITS = $extval (GLenum, "GL_SUBPIXEL_BITS")
macdef GL_RED_BITS = $extval (GLenum, "GL_RED_BITS")
macdef GL_GREEN_BITS = $extval (GLenum, "GL_GREEN_BITS")
macdef GL_BLUE_BITS = $extval (GLenum, "GL_BLUE_BITS")
macdef GL_ALPHA_BITS = $extval (GLenum, "GL_ALPHA_BITS")
macdef GL_DEPTH_BITS = $extval (GLenum, "GL_DEPTH_BITS")
macdef GL_STENCIL_BITS = $extval (GLenum, "GL_STENCIL_BITS")
macdef GL_POLYGON_OFFSET_UNITS = $extval (GLenum, "GL_POLYGON_OFFSET_UNITS")
// GL_POLYGON_OFFSET_FILL
macdef GL_POLYGON_OFFSET_FACTOR = $extval (GLenum, "GL_POLYGON_OFFSET_FACTOR")
macdef GL_TEXTURE_BINDING_2D = $extval (GLenum, "GL_TEXTURE_BINDING_2D")
macdef GL_SAMPLE_BUFFERS = $extval (GLenum, "GL_SAMPLE_BUFFERS")
macdef GL_SAMPLES = $extval (GLenum, "GL_SAMPLES")
macdef GL_SAMPLE_COVERAGE_VALUE = $extval (GLenum, "GL_SAMPLE_COVERAGE_VALUE")
macdef GL_SAMPLE_COVERAGE_INVERT = $extval (GLenum, "GL_SAMPLE_COVERAGE_INVERT")

(* GetTextureParameter *)
(* GL_TEXTURE_MAG_FILTER *)
(* GL_TEXTURE_MIN_FILTER *)
(* GL_TEXTURE_WRAP_S *)
(* GL_TEXTURE_WRAP_T *)

macdef GL_NUM_COMPRESSED_TEXTURE_FORMATS = $extval (GLenum, "GL_NUM_COMPRESSED_TEXTURE_FORMATS")
macdef GL_COMPRESSED_TEXTURE_FORMATS = $extval (GLenum, "GL_COMPRESSED_TEXTURE_FORMATS")

(* HintMode *)
macdef GL_DONT_CARE = $extval (GLenum, "GL_DONT_CARE")
macdef GL_FASTEST = $extval (GLenum, "GL_FASTEST")
macdef GL_NICEST = $extval (GLenum, "GL_NICEST")

(* HintTarget *)
macdef GL_GENERATE_MIPMAP_HINT = $extval (GLenum, "GL_GENERATE_MIPMAP_HINT")

(* DataType *)
macdef GL_BYTE = $extval (GLenum_type GLbyte, "GL_BYTE")
macdef GL_UNSIGNED_BYTE = $extval (GLenum_type GLubyte, "GL_UNSIGNED_BYTE")
macdef GL_SHORT = $extval (GLenum_type GLshort, "GL_SHORT")
macdef GL_UNSIGNED_SHORT = $extval (GLenum_type GLushort, "GL_UNSIGNED_SHORT")
macdef GL_INT = $extval (GLenum_type GLint, "GL_INT")
macdef GL_UNSIGNED_INT = $extval (GLenum_type GLuint, "GL_UNSIGNED_INT")
macdef GL_FLOAT = $extval (GLenum_type GLfloat, "GL_FLOAT")
macdef GL_FIXED = $extval (GLenum_type GLfixed, "GL_FIXED")

(* PixelFormat *)
macdef GL_DEPTH_COMPONENT = $extval (GLenum, "GL_DEPTH_COMPONENT")
macdef GL_ALPHA = $extval (GLenum, "GL_ALPHA")
macdef GL_RGB = $extval (GLenum, "GL_RGB")
macdef GL_RGB_format = $extval (GLenum_format 3, "GL_RGB")
macdef GL_RGBA = $extval (GLenum, "GL_RGBA")
macdef GL_RGBA_format = $extval (GLenum_format 4, "GL_RGBA")
macdef GL_LUMINANCE = $extval (GLenum, "GL_LUMINANCE")
macdef GL_LUMINANCE_ALPHA = $extval (GLenum, "GL_LUMINANCE_ALPHA")

(* PixelType *)
// GL_UNSIGNED_BYTE
macdef GL_UNSIGNED_SHORT_4_4_4_4 = $extval (GLenum, "GL_UNSIGNED_SHORT_4_4_4_4")
macdef GL_UNSIGNED_SHORT_5_5_5_1 = $extval (GLenum, "GL_UNSIGNED_SHORT_5_5_5_1")
macdef GL_UNSIGNED_SHORT_5_6_5 = $extval (GLenum, "GL_UNSIGNED_SHORT_5_6_5")

(* Shaders *)
macdef GL_FRAGMENT_SHADER = $extval (GLenum, "GL_FRAGMENT_SHADER")
macdef GL_VERTEX_SHADER = $extval (GLenum, "GL_VERTEX_SHADER")
macdef GL_MAX_VERTEX_ATTRIBS = $extval (GLenum, "GL_MAX_VERTEX_ATTRIBS")
macdef GL_MAX_VERTEX_UNIFORM_VECTORS = $extval (GLenum, "GL_MAX_VERTEX_UNIFORM_VECTORS")
macdef GL_MAX_VARYING_VECTORS = $extval (GLenum, "GL_MAX_VARYING_VECTORS")
macdef GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $extval (GLenum, "GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS")
macdef GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $extval (GLenum, "GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS")
macdef GL_MAX_TEXTURE_IMAGE_UNITS = $extval (GLenum, "GL_MAX_TEXTURE_IMAGE_UNITS")
macdef GL_MAX_FRAGMENT_UNIFORM_VECTORS = $extval (GLenum, "GL_MAX_FRAGMENT_UNIFORM_VECTORS")
macdef GL_SHADER_TYPE = $extval (GLenum, "GL_SHADER_TYPE")
macdef GL_DELETE_STATUS = $extval (GLenum, "GL_DELETE_STATUS")
macdef GL_LINK_STATUS = $extval (GLenum, "GL_LINK_STATUS")
macdef GL_VALIDATE_STATUS = $extval (GLenum, "GL_VALIDATE_STATUS")
macdef GL_ATTACHED_SHADERS = $extval (GLenum, "GL_ATTACHED_SHADERS")
macdef GL_ACTIVE_UNIFORMS = $extval (GLenum_type GLint, "GL_ACTIVE_UNIFORMS")
macdef GL_ACTIVE_UNIFORM_MAX_LENGTH = $extval (GLenum_type GLint, "GL_ACTIVE_UNIFORM_MAX_LENGTH")
macdef GL_ACTIVE_ATTRIBUTES = $extval (GLenum_type GLint, "GL_ACTIVE_ATTRIBUTES")
macdef GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $extval (GLenum_type GLint, "GL_ACTIVE_ATTRIBUTE_MAX_LENGTH")
macdef GL_SHADING_LANGUAGE_VERSION = $extval (GLenum, "GL_SHADING_LANGUAGE_VERSION")
macdef GL_CURRENT_PROGRAM = $extval (GLenum, "GL_CURRENT_PROGRAM")

(* StencilFunction *)
macdef GL_NEVER = $extval (GLenum, "GL_NEVER")
macdef GL_LESS = $extval (GLenum, "GL_LESS")
macdef GL_EQUAL = $extval (GLenum, "GL_EQUAL")
macdef GL_LEQUAL = $extval (GLenum, "GL_LEQUAL")
macdef GL_GREATER = $extval (GLenum, "GL_GREATER")
macdef GL_NOTEQUAL = $extval (GLenum, "GL_NOTEQUAL")
macdef GL_GEQUAL = $extval (GLenum, "GL_GEQUAL")
macdef GL_ALWAYS = $extval (GLenum, "GL_ALWAYS")

(* StencilOp *)
// GL_ZERO
macdef GL_KEEP = $extval (GLenum, "GL_KEEP")
macdef GL_REPLACE = $extval (GLenum, "GL_REPLACE")
macdef GL_INCR = $extval (GLenum, "GL_INCR")
macdef GL_DECR = $extval (GLenum, "GL_DECR")
macdef GL_INVERT = $extval (GLenum, "GL_INVERT")
macdef GL_INCR_WRAP = $extval (GLenum, "GL_INCR_WRAP")
macdef GL_DECR_WRAP = $extval (GLenum, "GL_DECR_WRAP")

(* StringName *)
macdef GL_VENDOR = $extval (GLenum, "GL_VENDOR")
macdef GL_RENDERER = $extval (GLenum, "GL_RENDERER")
macdef GL_VERSION = $extval (GLenum, "GL_VERSION")
macdef GL_EXTENSIONS = $extval (GLenum, "GL_EXTENSIONS")

(* TextureMagFilter *)
macdef GL_NEAREST = $extval (GLint, "GL_NEAREST")
macdef GL_LINEAR = $extval (GLint, "GL_LINEAR")

(* TextureMinFilter *)
// GL_NEAREST
// GL_LINEAR
macdef GL_NEAREST_MIPMAP_NEAREST = $extval (GLint, "GL_NEAREST_MIPMAP_NEAREST")
macdef GL_LINEAR_MIPMAP_NEAREST = $extval (GLint, "GL_LINEAR_MIPMAP_NEAREST")
macdef GL_NEAREST_MIPMAP_LINEAR = $extval (GLint, "GL_NEAREST_MIPMAP_LINEAR")
macdef GL_LINEAR_MIPMAP_LINEAR = $extval (GLint, "GL_LINEAR_MIPMAP_LINEAR")

(* TextureParameterName *)
macdef GL_TEXTURE_MAG_FILTER = $extval (GLenum, "GL_TEXTURE_MAG_FILTER")
macdef GL_TEXTURE_MIN_FILTER = $extval (GLenum, "GL_TEXTURE_MIN_FILTER")
macdef GL_TEXTURE_WRAP_S = $extval (GLenum, "GL_TEXTURE_WRAP_S")
macdef GL_TEXTURE_WRAP_T = $extval (GLenum, "GL_TEXTURE_WRAP_T")

(* TextureTarget *)
// GL_TEXTURE_2D
macdef GL_TEXTURE = $extval (GLenum, "GL_TEXTURE")
macdef GL_TEXTURE_CUBE_MAP = $extval (GLenum, "GL_TEXTURE_CUBE_MAP")
macdef GL_TEXTURE_BINDING_CUBE_MAP = $extval (GLenum, "GL_TEXTURE_BINDING_CUBE_MAP")
macdef GL_TEXTURE_CUBE_MAP_POSITIVE_X = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_POSITIVE_X")
macdef GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_NEGATIVE_X")
macdef GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_POSITIVE_Y")
macdef GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_NEGATIVE_Y")
macdef GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_POSITIVE_Z")
macdef GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $extval (GLenum, "GL_TEXTURE_CUBE_MAP_NEGATIVE_Z")
macdef GL_MAX_CUBE_MAP_TEXTURE_SIZE = $extval (GLenum, "GL_MAX_CUBE_MAP_TEXTURE_SIZE")

(* TextureUnit *)
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

(* TextureWrapMode *)
macdef GL_REPEAT = $extval (GLint, "GL_REPEAT")
macdef GL_CLAMP_TO_EDGE = $extval (GLint, "GL_CLAMP_TO_EDGE")
macdef GL_MIRRORED_REPEAT = $extval (GLint, "GL_MIRRORED_REPEAT")

(* Uniform Types *)
macdef GL_FLOAT_VEC2 = $extval (GLenum, "GL_FLOAT_VEC2")
macdef GL_FLOAT_VEC3 = $extval (GLenum, "GL_FLOAT_VEC3")
macdef GL_FLOAT_VEC4 = $extval (GLenum, "GL_FLOAT_VEC4")
macdef GL_INT_VEC2 = $extval (GLenum, "GL_INT_VEC2")
macdef GL_INT_VEC3 = $extval (GLenum, "GL_INT_VEC3")
macdef GL_INT_VEC4 = $extval (GLenum, "GL_INT_VEC4")
macdef GL_BOOL = $extval (GLenum, "GL_BOOL")
macdef GL_BOOL_VEC2 = $extval (GLenum, "GL_BOOL_VEC2")
macdef GL_BOOL_VEC3 = $extval (GLenum, "GL_BOOL_VEC3")
macdef GL_BOOL_VEC4 = $extval (GLenum, "GL_BOOL_VEC4")
macdef GL_FLOAT_MAT2 = $extval (GLenum, "GL_FLOAT_MAT2")
macdef GL_FLOAT_MAT3 = $extval (GLenum, "GL_FLOAT_MAT3")
macdef GL_FLOAT_MAT4 = $extval (GLenum, "GL_FLOAT_MAT4")
macdef GL_SAMPLER_2D = $extval (GLenum, "GL_SAMPLER_2D")
macdef GL_SAMPLER_CUBE = $extval (GLenum, "GL_SAMPLER_CUBE")

(* Vertex Arrays *)
macdef GL_VERTEX_ATTRIB_ARRAY_ENABLED = $extval (GLenum, "GL_VERTEX_ATTRIB_ARRAY_ENABLED")
macdef GL_VERTEX_ATTRIB_ARRAY_SIZE = $extval (GLenum, "GL_VERTEX_ATTRIB_ARRAY_SIZE")
macdef GL_VERTEX_ATTRIB_ARRAY_STRIDE = $extval (GLenum, "GL_VERTEX_ATTRIB_ARRAY_STRIDE")
macdef GL_VERTEX_ATTRIB_ARRAY_TYPE = $extval (GLenum, "GL_VERTEX_ATTRIB_ARRAY_TYPE")
macdef GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $extval (GLenum, "GL_VERTEX_ATTRIB_ARRAY_NORMALIZED")
macdef GL_VERTEX_ATTRIB_ARRAY_POINTER = $extval (GLenum, "GL_VERTEX_ATTRIB_ARRAY_POINTER")
macdef GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $extval (GLenum, "GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING")

(* Read Format *)
macdef GL_IMPLEMENTATION_COLOR_READ_TYPE = $extval (GLenum, "GL_IMPLEMENTATION_COLOR_READ_TYPE")
macdef GL_IMPLEMENTATION_COLOR_READ_FORMAT = $extval (GLenum, "GL_IMPLEMENTATION_COLOR_READ_FORMAT")

(* Shader Source *)
macdef GL_COMPILE_STATUS = $extval (GLenum, "GL_COMPILE_STATUS")
macdef GL_INFO_LOG_LENGTH = $extval (GLenum, "GL_INFO_LOG_LENGTH")
macdef GL_SHADER_SOURCE_LENGTH = $extval (GLenum, "GL_SHADER_SOURCE_LENGTH")
macdef GL_SHADER_COMPILER = $extval (GLenum, "GL_SHADER_COMPILER")

(* Shader Binary *)
macdef GL_SHADER_BINARY_FORMATS = $extval (GLenum, "GL_SHADER_BINARY_FORMATS")
macdef GL_NUM_SHADER_BINARY_FORMATS = $extval (GLenum, "GL_NUM_SHADER_BINARY_FORMATS")

(* Shader Precision-Specified Types *)
macdef GL_LOW_FLOAT = $extval (GLenum, "GL_LOW_FLOAT")
macdef GL_MEDIUM_FLOAT = $extval (GLenum, "GL_MEDIUM_FLOAT")
macdef GL_HIGH_FLOAT = $extval (GLenum, "GL_HIGH_FLOAT")
macdef GL_LOW_INT = $extval (GLenum, "GL_LOW_INT")
macdef GL_MEDIUM_INT = $extval (GLenum, "GL_MEDIUM_INT")
macdef GL_HIGH_INT = $extval (GLenum, "GL_HIGH_INT")

(* Framebuffer Object. *)
macdef GL_FRAMEBUFFER = $extval (GLenum, "GL_FRAMEBUFFER")
macdef GL_RENDERBUFFER = $extval (GLenum, "GL_RENDERBUFFER")

macdef GL_RGBA4 = $extval (GLenum, "GL_RGBA4")
macdef GL_RGB5_A1 = $extval (GLenum, "GL_RGB5_A1")
macdef GL_RGB565 = $extval (GLenum, "GL_RGB565")
macdef GL_DEPTH_COMPONENT16 = $extval (GLenum, "GL_DEPTH_COMPONENT16")
macdef GL_STENCIL_INDEX = $extval (GLenum, "GL_STENCIL_INDEX")
macdef GL_STENCIL_INDEX8 = $extval (GLenum, "GL_STENCIL_INDEX8")

macdef GL_RENDERBUFFER_WIDTH = $extval (GLenum, "GL_RENDERBUFFER_WIDTH")
macdef GL_RENDERBUFFER_HEIGHT = $extval (GLenum, "GL_RENDERBUFFER_HEIGHT")
macdef GL_RENDERBUFFER_INTERNAL_FORMAT = $extval (GLenum, "GL_RENDERBUFFER_INTERNAL_FORMAT")
macdef GL_RENDERBUFFER_RED_SIZE = $extval (GLenum, "GL_RENDERBUFFER_RED_SIZE")
macdef GL_RENDERBUFFER_GREEN_SIZE = $extval (GLenum, "GL_RENDERBUFFER_GREEN_SIZE")
macdef GL_RENDERBUFFER_BLUE_SIZE = $extval (GLenum, "GL_RENDERBUFFER_BLUE_SIZE")
macdef GL_RENDERBUFFER_ALPHA_SIZE = $extval (GLenum, "GL_RENDERBUFFER_ALPHA_SIZE")
macdef GL_RENDERBUFFER_DEPTH_SIZE = $extval (GLenum, "GL_RENDERBUFFER_DEPTH_SIZE")
macdef GL_RENDERBUFFER_STENCIL_SIZE = $extval (GLenum, "GL_RENDERBUFFER_STENCIL_SIZE")

macdef GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $extval (GLenum, "GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE")
macdef GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $extval (GLenum, "GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME")
macdef GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $extval (GLenum, "GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL")
macdef GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $extval (GLenum, "GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE")

macdef GL_COLOR_ATTACHMENT0 = $extval (GLenum, "GL_COLOR_ATTACHMENT0")
macdef GL_DEPTH_ATTACHMENT = $extval (GLenum, "GL_DEPTH_ATTACHMENT")
macdef GL_STENCIL_ATTACHMENT = $extval (GLenum, "GL_STENCIL_ATTACHMENT")

macdef GL_NONE = $extval (GLenum, "GL_NONE")

macdef GL_FRAMEBUFFER_COMPLETE = $extval (GLenum, "GL_FRAMEBUFFER_COMPLETE")
macdef GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $extval (GLenum, "GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT")
macdef GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $extval (GLenum, "GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT")
macdef GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $extval (GLenum, "GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS")
macdef GL_FRAMEBUFFER_UNSUPPORTED = $extval (GLenum, "GL_FRAMEBUFFER_UNSUPPORTED")

macdef GL_FRAMEBUFFER_BINDING = $extval (GLenum, "GL_FRAMEBUFFER_BINDING")
macdef GL_RENDERBUFFER_BINDING = $extval (GLenum, "GL_RENDERBUFFER_BINDING")
macdef GL_MAX_RENDERBUFFER_SIZE = $extval (GLenum, "GL_MAX_RENDERBUFFER_SIZE")

macdef GL_INVALID_FRAMEBUFFER_OPERATION = $extval (GLenum, "GL_INVALID_FRAMEBUFFER_OPERATION")

(* ****** ****** *)
(* resource types *)

absviewt@ype GLbuffer (int) = GLuint
viewtypedef GLbuffer = [n:int] GLbuffer (n)

absviewt@ype GLframebuffer (int) = GLuint
viewtypedef GLframebuffer = [n:int] GLframebuffer (n)

absviewt@ype GLrenderbuffer (int) = GLuint
viewtypedef GLrenderbuffer = [n:int] GLrenderbuffer (n)

absviewt@ype GLtexture (int) = GLuint
viewtypedef GLtexture = [n:int] GLtexture (n)

absviewt@ype GLprogram (i:int) = GLuint
viewtypedef GLprogram = [i:int] GLprogram (i)

absviewt@ype GLshader (i:int) = GLuint
viewtypedef GLshader = [i:int] GLshader (i)

(* ****** ****** *)

(*
** GL core functions (listed in mostly alphabetical order)
*)

fun glActiveTexture (texture: GLenum): void = "mac#atsctrb_glActiveTexture"

fun glAttachShader {i,j:int} (
  program: !GLprogram i
, shader: !GLshader j
) : void
  = "mac#atsctrb_glAttachShader"
// end of [glAttachShader]

(* ****** ****** *)

fun glBindAttribLocation {i:int} (
  program: !GLprogram i, index: GLuint, name: string
) : void
  = "mac#atsctrb_glBindAttribLocation"

fun glBindBuffer {i:int} (
  target: GLenum, buffer: !GLbuffer i
) : void
  = "mac#atsctrb_glBindBuffer"
// end of [glBindBuffer]

fun glBindFramebuffer {i:int} (
  target: GLenum, framebuffer: !GLframebuffer i
) : void
  = "mac#atsctrb_glBindFramebuffer"
// end of [glBindFramebuffer]

fun glBindRenderbuffer {i:int} (
  target: GLenum
, renderbuffer: !GLrenderbuffer i
) : void
  = "mac#atsctrb_glBindRenderbuffer"
// end of [glBindRenderbuffer]

fun glBindTexture (
  target: GLenum, texture: !GLtexture
) : void
  = "mac#atsctrb_glBindTexture"
// end of [glBindTexture]

fun glBlendColor (
  red: GLclampf, green: GLclampf, blue: GLclampf, alpha: GLclampf
) : void
  = "mac#atsctrb_glBlendColor"
// end of [glBlendColor]

fun glBlendEquation (mode: GLenum): void
  = "mac#atsctrb_glBlendEquation"
// end of [glBlendEquation]

fun glBlendEquationSeparate (modeRGB: GLenum, modeAlpha: GLenum): void
  = "mac#atsctrb_glBlendEquationSeparate"
// end of [glBlendEquationSeparate]

fun glBlendFunc (sfactor: GLenum, dfactor: GLenum): void = "mac#atsctrb_glBlendFunc"

fun glBlendFuncSeparate (
  srcRGB: GLenum, dstRGB: GLenum, srcAlpha: GLenum, dstAlpha: GLenum
) : void
  = "mac#atsctrb_glBlendFuncSeparate"
// end of [glBlendFuncSeparate]

fun glBufferData {a:t@ype} {n:nat} (
  target: GLenum, size: GLsizeiptr n, data: &(@[a][n]), usage: GLenum
) : void
  = "mac#atsctrb_glBufferData"
// end of [glBufferData]

fun glBufferSubData {a:t@ype} {n:nat} (
  target: GLenum, offset: GLintptr, size: GLsizeiptr n, data: &(@[a][n])
) : void
  = "mac#atsctrb_glBufferSubData"
// end of [glBufferSubData]

(* ****** ****** *)

fun glCheckFramebufferStatus (target: GLenum): GLenum
  = "mac#atsctrb_glCheckFramebufferStatus"
// end of [glCheckFramebufferStatus]

fun glClear (mask: GLbitfield): void = "mac#atsctrb_glClear"

fun glClearColor (
  red: GLclampf, green: GLclampf, blue: GLclampf, alpha: GLclampf
) : void
  = "mac#atsctrb_glClearColor"

fun glClearDepthf (depth: GLclampf): void = "mac#atsctrb_glClearDepthf"

fun glClearStencil (s: GLint): void = "mac#atsctrb_glClearStencil"

fun glColorMask (
  red: GLboolean, green: GLboolean, blue: GLboolean, alpha: GLboolean
) : void
  = "mac#atsctrb_glColorMask"

fun glCompileShader {i:int} (shader: !GLshader i): void = "mac#atsctrb_glCompileShader"

fun glCompressedTexImage2D {w,h,n:nat} (
  target: GLenum, level: GLint, internalformat: GLenum
, width: GLsizei w, height: GLsizei h
, border: GLint, imageSize: GLsizei n, data: &(@[GLubyte][n])
) : void
  = "mac#atsctrb_glCompressedTexImage2D"

fun glCompressedTexSubImage2D {w,h,n:nat} (
  target: GLenum
, level: GLint
, xoffset: GLint
, yoffset: GLint
, width: GLsizei w
, height: GLsizei h
, format: GLenum
, imageSize: GLsizei n
, data: &(@[GLubyte][n])
) : void
  = "mac#atsctrb_glCompressedTexSubImage2D"

fun glCopyTexImage2D {w,h:nat} (
  target: GLenum
, level: GLint
, internalformat: GLenum
, x: GLint
, y: GLint
, width: GLsizei w
, height: GLsizei h
, border: GLint
) : void
  = "mac#atsctrb_glCopyTexImage2D"

fun glCopyTexSubImage2D {w,h:nat} (
  target: GLenum
, level: GLint
, xoffset: GLint
, yoffset: GLint
, x: GLint
, y: GLint
, width: GLsizei w
, height: GLsizei h
) : void
  = "mac#atsctrb_glCopyTexSubImage2D"

fun glCreateProgram (): GLprogram = "mac#atsctrb_glCreateProgram"

fun glCreateShader (t: GLenum): GLshader = "mac#atsctrb_glCreateShader"

fun glCullFace (mode: GLenum): void = "mac#atsctrb_glCullFace"

(* ****** ****** *)

fun glDeleteBuffer
  (buffer: GLbuffer): void = "atsctrb_glDeleteBuffer" // this is a function!
// end of [glDeleteBuffer]

fun glDeleteBuffers {n:pos} (
  n: GLsizei n, buffers: &(@[GLbuffer][n]) >> @[GLbuffer?][n]
) : void
  = "mac#atsctrb_glDeleteBuffers"
// end of [glDeleteBuffers]

fun glDeleteProgram (program: GLprogram): void = "mac#atsctrb_glDeleteProgram"

fun glDeleteRenderbuffer
  (buffer: GLrenderbuffer): void = "atsctrb_glDeleteRenderuffer" // this is a function!
// end of [glDeleteRenderbuffer]

fun glDeleteRenderbuffers {n:pos} (
  n: GLsizei n
, renderbuffers: &(@[GLrenderbuffer][n]) >> @[GLrenderbuffer?][n]
) : void
  = "mac#atsctrb_glDeleteRenderbuffers"
// end of [glDeleteRenderbuffers]

fun glDeleteShader (shader: GLshader): void = "mac#atsctrb_glDeleteShader"

fun glDeleteTexture (texture: GLtexture): void
  = "atsctrb_glDeleteTexture" // this is a function!
// end of [glDeleteTexture]

fun glDeleteTextures {n:pos} (
  n: GLsizei n
, textures: &(@[GLtexture][n]) >> @[GLtexture?][n]
) : void
  = "mac#atsctrb_glDeleteTextures"

fun glDepthFunc (func: GLenum): void = "mac#atsctrb_glDepthFunc"

fun glDepthMask (flag: GLboolean): void = "mac#atsctrb_glDepthMask"

fun glDepthRangef (zNear: GLclampf, zFar: GLclampf): void = "mac#atsctrb_glDepthRangef"

fun glDetachShader (program: !GLprogram, shader: !GLshader): void = "mac#atsctrb_glDetachShader"

fun glDisable (cap: GLenum): void = "mac#atsctrb_glDisable"

fun glDisableVertexAttribArray (index: GLuint): void = "mac#atsctrb_glDisableVertexAttribArray"

fun glDrawArrays (mode: GLenum, first: GLint, count: GLsizei): void = "mac#atsctrb_glDrawArrays"

// type: one of UNSIGNED_BYTE, UNSIGNED_SHORT, UNSIGNED_INT (that is, ubyte, ushort, uint)
fun glDrawElements {a:t@ype} {n:nat} (
  mode: GLenum, count: GLsizei n, type: GLenum_type a, indices: &(@[a][n])
) : void
  = "mac#atsctrb_glDrawElements"
// end of [glDrawElements]

(* ****** ****** *)

fun glEnable (cap: GLenum): void = "mac#atsctrb_glEnable"

fun glEnableVertexAttribArray (index: GLuint): void = "mac#atsctrb_glEnableVertexAttribArray"

(* ****** ****** *)

fun glFinish (): void = "mac#atsctrb_glFinish"

fun glFlush (): void = "mac#atsctrb_glFlush"

fun glFramebufferRenderbuffer (
  target: GLenum
, attachment: GLenum
, renderbuffertarget: GLenum
, renderbuffer: !GLrenderbuffer
) : void
  = "mac#atsctrb_glFramebufferRenderbuffer"
// end of [glFramebufferRenderbuffer]

fun glFramebufferTexture2D (
  target: GLenum
, attachment: GLenum
, textarget: GLenum
, texture: !GLtexture
, level: GLint
) : void
  = "mac#atsctrb_glFramebufferTexture2D"
// end of [glFramebufferTexture2D]

fun glFrontFace (mode: GLenum): void = "mac#atsctrb_glFrontFace"

(* ****** ****** *)

typedef glGen_type (a:viewt@ype) = {n:pos}
  (GLsizei n(*count*), &(@[a?][n]) >> @[a][n]) -<fun1> void

fun glGenBuffer
  (buffer: &GLbuffer? >> GLbuffer): void
  = "atsctrb_glGenBuffer" // this is a function!
// end of [glGenBuffer]

fun glGenBuffers : glGen_type (GLbuffer) = "mac#atsctrb_glGenBuffers"

fun glGenFramebuffer
  (framebuffer: &GLframebuffer? >> GLframebuffer): void
  = "atsctrb_glGenFramebuffer" // this is a function!
// end of [glGenFramebuffer]

fun glGenFramebuffers
  : glGen_type (GLframebuffer)
  = "mac#atsctrb_glGenFramebuffers"
// end of [glGenFramebuffers]

fun glGenRenderbuffer (renderbuffer: &GLrenderbuffer? >> GLrenderbuffer): void
  = "atsctrb_glGenRenderbuffer" // this is a function!
// end of [glGenRenderbuffer]

fun glGenRenderbuffers : glGen_type (GLrenderbuffer)
  = "mac#atsctrb_glGenRenderbuffers"
// end of [glGenRenderbuffers]

fun glGenTexture (texture: &GLtexture? >> GLtexture): void
  = "atsctrb_glGenTexture"
// end of [glGenTexture]

fun glGenTextures : glGen_type (GLtexture)
  = "mac#atsctrb_glGenTextures"
// end of [glGenTextures]

fun glGenerateMipmap (target: GLenum): void = "mac#atsctrb_glGenerateMipmap"

fun glGetActiveAttrib {i:int} (
  program: !GLprogram
, index: GLuint
, bufsize: GLsizei
, length: &GLsizei
, size: &GLint
, type: &GLenum
, name: string
) : void
  = "mac#atsctrb_glGetActiveAttrib"

fun glGetActiveUniform {i:int} (
  program: !GLprogram i
, index: GLuint
, bufsize: GLsizei
, length: &GLsizei
, size: &GLsizei
, type: &GLenum
, name: string
) : void
  = "mac#atsctrb_glGetActiveUniform"

// in: maxcount: GLsizei n, shaders: @[GLshader?][n]
// out: [m<=n] count: GLsizei m, MUL (sizeof GLshader, m, ofs), array_v (GLshader, m, l), array_v (GLshader?, n-m, l+ofs)
fun glGetAttachedShaders {i:int} (
  program: !GLprogram i
, maxcount: GLsizei
, count: &GLsizei
, shaders: ptr
) : void
  = "mac#atsctrb_glGetAttachedShaders"

fun glGetAttribLocation {i:int} (program: !GLprogram i, name: string): GLint = "mac#atsctrb_glGetAttribLocation"

fun glGetBooleanv (pname: GLenum, params: ptr): void = "mac#atsctrb_glGetBooleanv"

fun glGetBufferParameteriv
  (target: GLenum, pname: GLenum, params: ptr): void = "mac#atsctrb_glGetBufferParameteriv"
// end of [glGetBufferParameteriv]

fun glGetError (): GLenum = "mac#atsctrb_glGetError"

fun glGetFloatv (pname: GLenum, params: ptr): void = "mac#atsctrb_glGetFloatv"

fun glGetFramebufferAttachmentParameteriv (
  target: GLenum
, attachment: GLenum
, pname: GLenum
, params: ptr
) : void
  = "mac#atsctrb_glGetFramebufferAttachmentParameteriv"

fun glGetIntegerv (pname: GLenum, params: ptr): void = "mac#atsctrb_glGetIntegerv"

// pname:
// - DELETE_STATUS: TRUE if the program has been flagged for deletion
// - VALIDATE_STATUS: boolean
// - INFO_LOG_LENGTH: length of the info log (including trailing zero)
// - ACTIVE_ATTRIBUTES the number of attributes that are active
// - ACTIVE_UNIFORMS: ...
// - ACTIVE_UNIFORM_MAX_LENGTH: the greatest length of active uniform names (incl. 0-term.)
fun glGetProgramiv {i:int} (
  program: !GLprogram i
, pname: GLenum
, params: &GLint? >> GLint
) : void = "mac#atsctrb_glGetProgramiv"

fun glGetProgramInfoLog {i:int} {m:nat} {l1,l2:addr} (
  pf1: !b0ytes m @ l1 >> strbuf (m, n) @ l1
, pf2: !optvar_v (GLsizei?, l2) >> optvar_v (GLsizei n, l2)
| program: !GLprogram i
, bufsize: GLsizei m
, length: ptr l2
, infolog: ptr l1
) : #[n:nat | n < m] void
  = "mac#atsctrb_glGetProgramInfoLog"
// end of [glGetProgramInfoLog]

fun glGetRenderbufferParameteriv (
  target: GLenum, pname: GLenum, params: ptr
) : void
  = "mac#atsctrb_glGetRenderbufferParameteriv"
// end of [glGetRenderbufferParameteriv]

// pname:
// - SHADER_TYPE will give VERTEX_SHADER or FRAGMENT_SHADER
// - DELETE_STATUS will give TRUE if shader has been flagged for deletion
// - COMPILE_STATUS will give TRUE if shader has been compiled successfully
// - INFO_LOG_LENGTH will return the length of the info log (including zero terminator)
// - SHADER_SOURCE_LENGTH will return the length of the source (including zero terminator)
fun glGetShaderiv {i:int} (
  shader: !GLshader 
, pname: GLenum
, params: &GLint? >> GLint
) : void
  = "mac#atsctrb_glGetShaderiv"
// end of [glGetShaderiv]

fun glGetShaderInfoLog {i:int} {m:nat} {l1,l2:addr} (
  pf1: !b0ytes m @ l1 >> strbuf (m, n) @ l1
, pf2: !optvar_v (GLsizei?, l2) >> optvar_v (GLsizei n, l2)
| shader: !GLshader i
, bufsize: GLsizei
, length: ptr l2
, infolog: ptr l1
) : #[n:nat | n < m] void
  = "mac#atsctrb_glGetShaderInfoLog"
// end of [glGetShaderInfoLog]

fun glGetShaderPrecisionFormat (
  shadertype: GLenum
, precisiontype: GLenum
, range: &GLint
, precision: &GLint
) : void
  = "mac#atsctrb_glGetShaderPrecisionFormat"
// end of [glGetShaderPrecisionFormat]

fun glGetShaderSource {i:int} {m:nat} {l1,l2:addr} (
  pf1: !b0ytes m @ l1 >> strbuf (m, n) @ l1
, pf2: !optvar_v (GLsizei?, l2) >> optvar_v (GLsizei n, l2)
| shader: !GLshader i
, bufsize: GLsizei m
, length: ptr l2
, source: ptr l1
) : #[n:nat | n < m] void
  = "mac#atsctrb_glGetShaderSource"
// end of [glGetShaderSource]

fun glGetString (name: GLenum): string = "mac#atsctrb_glGetString"

fun glGetTexParameterfv (
  target: GLenum, pname: GLenum, params: ptr
) : void
  = "mac#atsctrb_glGetTexParameterfv"
// end of [glGetTexParameterfv]

fun glGetTexParameteriv (
  target: GLenum, pname: GLenum, params: ptr
) : void
  = "mac#atsctrb_glGetTexParameteriv"
// end of [glGetTexParameteriv]

fun glGetUniformfv {i:int} (
  program: !GLprogram i, location: GLint, params: ptr
) : void
  = "mac#atsctrb_glGetUniformfv"
// end of [glGetUniformfv]

fun glGetUniformiv {i:int} (
  program: !GLprogram i, location: GLint, params: ptr
) : void
  = "mac#atsctrb_glGetUniformiv"
// end of [glGetUniformiv]

fun glGetUniformLocation {i:int} (
  program: !GLprogram i, name: string
) : GLint
  = "mac#atsctrb_glGetUniformLocation"
// end of [glGetUniformLocation]

fun glGetVertexAttribfv (
  index: GLuint, pname: GLenum, params: ptr
) : void
  = "mac#atsctrb_glGetVertexAttribfv"
// end of [glGetVertexAttribfv]

fun glGetVertexAttribiv (
  index: GLuint, pname: GLenum, params: ptr
) : void
  = "mac#atsctrb_glGetVertexAttribiv"
// end of [glGetVertexAttribiv]

fun glGetVertexAttribPointerv (
  index: GLuint
, pname: GLenum
, pointer: &ptr
) : void
  = "mac#atsctrb_glGetVertexAttribPointerv"
// end of [glGetVertexAttribPointerv]

(* ****** ****** *)

fun glHint (target: GLenum, mode: GLenum): void = "mac#atsctrb_glHint"

(* ****** ****** *)

fun glIsEnabled (cap: GLenum): GLboolean = "mac#atsctrb_glIsEnabled"

fun glIsBuffer (buffer: !GLbuffer): GLboolean = "mac#atsctrb_glIsBuffer"
fun glIsFramebuffer (framebuffer: !GLframebuffer): GLboolean = "mac#atsctrb_glIsFramebuffer"
fun glIsProgram (program: !GLprogram): GLboolean = "mac#atsctrb_glIsProgram"
fun glIsRenderbuffer (renderbuffer: !GLrenderbuffer): GLboolean = "mac#atsctrb_glIsRenderbuffer"
fun glIsShader (shader: !GLshader): GLboolean = "mac#atsctrb_glIsShader"
fun glIsTexture (texture: !GLtexture): GLboolean = "mac#atsctrb_glIsTexture"

(* ****** ****** *)

fun glLineWidth (width: GLfloat): void = "mac#atsctrb_glLineWidth"

fun glLinkProgram {i:int} (program: !GLprogram i): void = "mac#atsctrb_glLinkProgram"

(* ****** ****** *)

fun glPixelStorei (pname: GLenum, param: GLint): void = "mac#atsctrb_glPixelStorei"

fun glPolygonOffset (factor: GLfloat, units: GLfloat): void = "mac#atsctrb_glPolygonOffset"

(* ****** ****** *)

fun glReadPixels {a:t@ype} {w,h:nat} {n:nat} (
  x: GLint, y: GLint
, width: GLsizei w, height: GLsizei h
, format: GLenum_format n
, type: GLenum_type a
, pixels: &GLarray3 (a, w, h, n)
) : void = "mac#atsctrb_glReadPixels"

fun glReleaseShaderCompiler (): void = "mac#atsctrb_glReleaseShaderCompiler"

fun glRenderbufferStorage (
  target: GLenum, internalformat: GLenum, width: GLsizei, height: GLsizei
) : void = "mac#atsctrb_glRenderbufferStorage"

(* ****** ****** *)

fun glSampleCoverage (value: GLclampf, invert: GLboolean): void = "mac#atsctrb_glSampleCoverage"

fun glScissor (x: GLint, y: GLint, width: GLsizei, height: GLsizei): void = "mac#atsctrb_glScissor"

fun glShaderBinary (
  n: GLsizei
, shaders: ptr
, binaryformat: GLenum
, binary: ptr
, length: GLsizei
) : void
  = "mac#atsctrb_glShaderBinary"
// end of [glShaderBinary]

fun glShaderSource__string {i:int} {l:agz} (
  shader: !GLshader i
, str: !strptr l
) : void
  = "atsctrb_glShaderSource__string" // this is a function!
// end of [glShaderSource__string]

fun glShaderSource__main {i:int} {n:pos} {l:addr} {b:bool} (
  pf_len: !optvar_v (@[GLuint][n], l)
| shader: !GLshader i
, count: GLsizei n
, str: &(@[string][n])
, length: ptr l
) : void
  = "mac#atsctrb_glShaderSource"
// end of [glShaderSource]

fun glStencilFunc (func: GLenum, ref: GLint, mask: GLuint): void
  = "mac#atsctrb_glStencilFunc"
// end of [glStencilFunc]

fun glStencilFuncSeparate (
  face: GLenum, func: GLenum, ref: GLint, mask: GLuint
) : void
  = "mac#atsctrb_glStencilFuncSeparate"
// end of [glStencilFuncSeparate]

fun glStencilMask (mask: GLuint): void = "mac#atsctrb_glStencilMask"
fun glStencilMaskSeparate (face: GLenum, mask: GLuint): void = "mac#atsctrb_glStencilMaskSeparate"

fun glStencilOp (fail: GLenum, zfail: GLenum, zpass: GLenum): void
  = "mac#atsctrb_glStencilMaskSeparate"
// end of [glStencilOp]

fun glStencilOpSeparate (
  face: GLenum, fail: GLenum, zfail: GLenum, zpass: GLenum
): void
  = "mac#atsctrb_glStencilOpSeparate"
// end of [glStencilOpSeparate]

(* ****** ****** *)

fun glTexImage2D
  {a:t@ype} {w,h:int} {n:int} (
  target: GLenum
, level: GLint
, internalFormat: GLint
, width: GLsizei w
, height: GLsizei h
, border: natLt(2)
, format: GLenum_format n
, type: GLenum_type (a)
, texels: &GLarray3 (a, w, h, n)
) : void
  = "mac#atsctrb_glTexImage2D"
// end of [glTexImage2D]

typedef glTexParameter_type (a:t@ype) =
  (GLenum(*target*), GLenum(*pname*), a(*param*)) -<fun1> void
// end of [glTexParameter_type]

fun glTexParameterf
  : glTexParameter_type (GLfloat) = "mac#atsctrb_glTexParameterf"
fun glTexParameteri
  : glTexParameter_type (GLint) = "mac#atsctrb_glTexParameteri"

fun glTexParameterfv (target: GLenum, pname: GLenum, params: ptr): void = "mac#atsctrb_glTexParameterfv"
fun glTexParameteriv (target: GLenum, pname: GLenum, params: ptr): void = "mac#atsctrb_glTexParameteriv"

fun glTexSubImage2D
  {a:t@ype} {w,h:int} {n:int} (
  target: GLenum
, level: GLint
, xoffset: GLint
, yoffset: GLint
, width: GLsizei w
, height: GLsizei h
, format: GLenum_format n
, type: GLenum_type (a)
, pixels: &GLarray3 (a, w, h, n)
) : void
  = "mac#atsctrb_glTexSubImage2D"

(* ****** ****** *)

typedef glUniform_t0ype_int (a:t@ype, m:int) = {n:nat}
  (GLint(*location*), GLsizei n(*count*), &GLarray2 (a(*param*), m, n)) -<fun1> void
// end of [glUniform_t0ype_int]

// Uniform*f{v} commands will load [count] sets of one to four floating-point
// values into a uniform [location] defined as float, a floating-point vector,
// an array of floats, or an array of floating-point vectors
fun glUniform1fv : glUniform_t0ype_int (GLfloat, 1) = "mac#atsctrb_glUniform1fv"
fun glUniform1iv : glUniform_t0ype_int (GLint, 1) = "mac#atsctrb_glUniform1iv"
fun glUniform2fv : glUniform_t0ype_int (GLfloat, 2) = "mac#atsctrb_glUniform2fv"
fun glUniform2iv : glUniform_t0ype_int (GLint, 2) = "mac#atsctrb_glUniform2iv"
fun glUniform3fv : glUniform_t0ype_int (GLfloat, 3) = "mac#atsctrb_glUniform3fv"
fun glUniform3iv : glUniform_t0ype_int (GLint, 3) = "mac#atsctrb_glUniform3iv"
fun glUniform4fv : glUniform_t0ype_int (GLfloat, 4) = "mac#atsctrb_glUniform4fv"
fun glUniform4iv : glUniform_t0ype_int (GLint, 4) = "mac#atsctrb_glUniform4iv"

typedef glUniform_t0ype_1 (a:t@ype) =
  (GLint(*location*), a(*param*)) -<fun1> void
// end of [glUniform_t0ype_1]
fun glUniform1f : glUniform_t0ype_1 (GLfloat) = "mac#atsctrb_glUniform1f"
fun glUniform1i : glUniform_t0ype_1 (GLint) = "mac#atsctrb_glUniform1i"

typedef glUniform_t0ype_2 (a:t@ype) =
  (GLint(*location*), a(*param 1*), a(*param 2*)) -<fun1> void
// end of [glUniform_t0ype_2]
fun glUniform2f : glUniform_t0ype_2 (GLfloat) = "mac#atsctrb_glUniform2f"
fun glUniform2i : glUniform_t0ype_2 (GLint) = "mac#atsctrb_glUniform2i"

typedef glUniform_t0ype_3 (a:t@ype) =
  (GLint(*location*), a(*param 1*), a(*param 2*), a(*param 3*)) -<fun1> void
// end of [glUniform_t0ype_3]
fun glUniform3f : glUniform_t0ype_3 (GLfloat) = "mac#atsctrb_glUniform3f"
fun glUniform3i : glUniform_t0ype_3 (GLint) = "mac#atsctrb_glUniform3i"

typedef glUniform_t0ype_4 (a:t@ype) =
  (GLint(*location*), a(*param 1*), a(*param 2*), a(*param 3*), a(*param 4*)) -<fun1> void
// end of [glUniform_t0ype_4]
fun glUniform4f : glUniform_t0ype_4 (GLfloat) = "mac#atsctrb_glUniform4f"
fun glUniform4i : glUniform_t0ype_4 (GLint) = "mac#atsctrb_glUniform4i"

// UniformMatrix{234}fv commands will load [count] 2x2, 3x3, 4x4
// matrices (corresponding to 2,3,4 in the command name) of floating-point
// values into a uniform location defined as a matrix or an array of matrices
// if [transpose] is false, the matrix is specified in column major order (otherwise row-major)
fun glUniformMatrix2fv (
  location: GLint
, count: GLsizei
, transpose: GLboolean
, value: ptr
) : void
  = "mac#atsctrb_glUniformMatrix2fv"
// end of [glUniformMatrix2fv]

fun glUniformMatrix3fv (
  location: GLint
, count: GLsizei
, transpose: GLboolean
, value: ptr
) : void
  = "mac#atsctrb_glUniformMatrix3fv"
// end of [glUniformMatrix3fv]

fun glUniformMatrix4fv {n:pos} (
  location: GLint
, count: GLsizei n
, transpose: GLboolean
, value: &(@[@[GLfloat][16]][n])
) : void
  = "mac#atsctrb_glUniformMatrix4fv"
// end of [glUniformMatrix4fv]

fun glUseProgram {i:int}
  (program: !GLprogram i): void = "mac#atsctrb_glUseProgram"

(* ****** ****** *)

fun glValidateProgram {i:int}
  (program: !GLprogram i): void = "mac#atsctrb_glValidateProgram"

typedef glVertexAttrib_type (n:int) =
  (GLint(*indx*), &(@[GLfloat][n](*values*))) -<fun1> void
// end of [glVertexAttrib_type]

fun glVertexAttrib1fv : glVertexAttrib_type (1) = "mac#atsctrb_glVertexAttrib1fv"
fun glVertexAttrib2fv : glVertexAttrib_type (2) = "mac#atsctrb_glVertexAttrib2fv"
fun glVertexAttrib3fv : glVertexAttrib_type (3) = "mac#atsctrb_glVertexAttrib3fv"
fun glVertexAttrib4fv : glVertexAttrib_type (4) = "mac#atsctrb_glVertexAttrib4fv"

fun glVertexAttrib1f
  (indx: GLuint, x: GLfloat): void = "mac#atsctrb_glVertexAttrib1f"
fun glVertexAttrib2f
  (indx: GLuint, x: GLfloat, y: GLfloat): void = "mac#atsctrb_glVertexAttrib2f"
fun glVertexAttrib3f
  (indx: GLuint, x: GLfloat, y: GLfloat, z: GLfloat): void = "mac#atsctrb_glVertexAttrib3f"
fun glVertexAttrib4f
  (indx: GLuint, x: GLfloat, y: GLfloat, z: GLfloat, w: GLfloat): void = "mac#atsctrb_glVertexAttrib4f"

fun glVertexAttribPointer
  {a:t@ype}
  {n:pos | n <= 4}
  {m:nat}
  {l:addr} (
  pf: !matrix_v (a, m, n, l)
| indx: GLuint
, size: GLint n
, type: GLenum_type a
, normalized: GLboolean
, stride: GLsizei 0
, p: ptr l
) : void
  = "mac#atsctrb_glVertexAttribPointer"

// it is programmer's responsibility to check that uses of this function
// do not compromise type safety
fun glVertexAttribPointer_unsafe (
  indx: GLuint
, size: GLint
, type: GLenum
, normalized: GLboolean
, stride: GLsizei
, p: ptr
) : void
  = "mac#atsctrb_glVertexAttribPointer"

fun glViewport (
  x: GLint, y: GLint, width: GLsizei, height: GLsizei
) : void
  = "mac#atsctrb_glViewport"

(* ****** ****** *)

(* end of [gl2.sats] *)
