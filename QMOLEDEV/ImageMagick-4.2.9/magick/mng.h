/*
  ImageMagick Image Methods.
*/
#ifndef _MNG_H
#define _MNG_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/*
  For diagnosing PerlMagick problems
*/
#undef MNG_ALWAYS_VERBOSE

/*
  This is temporary until I set up malloc'ed object attributes array.
  Recompile with MNG_MAX_OBJECTS = 65536 to avoid this limit but
  waste more memory.
*/
#define MNG_MAX_OBJECTS 256

/*
  Features under construction.  Define these to work on them.
*/
#undef MNG_OBJECT_BUFFERS
#undef MNG_BASI_SUPPORTED

/*
  Define this to 9600 to get proposed MNG-0.96 capabilities (Draft
  0.96), or to higher numbers to get proposed capabilities.

  As of July 20, 1999, version 0.96 is the latest approved version.

  MNG_LEVEL 9600: MNG-0.96

#define MNG_LEVEL 9600
*/

/*
  If this is not defined, spec is interpreted strictly.  If it is
  defined, an attempt will be made to recover from some errors,
  including
      o global PLTE too short
*/
#undef MNG_LOOSE

/*
  Don't try to define PNG_READ|WRITE_EMPTY_PLTE_SUPPORTED here.  Make sure
  it's defined in libpng/pngconf.h, version 1.0.3a or later.  It won't work
  with earlier versions of libpng.  As of July 20, 1999, libpng-1.0.3a has
  not yet been released by the PNG group.
*/

/*
  Maximum valid unsigned long in PNG/MNG chunks is (2^31)-1
  This macro is only defined in libpng-1.0.3a and later.
*/
#ifndef PNG_MAX_UINT
#define PNG_MAX_UINT (png_uint_32) 0x7fffffffL
#endif

/*
  Constant strings for known chunk types.  If you need to add a chunk,
  add a string holding the name here.   To make the code more
  portable, we use ASCII numbers like this, not characters.
*/

png_byte FARDATA mng_MHDR[5] = { 77,  72,  68,  82, '\0'};
png_byte FARDATA mng_BACK[5] = { 66,  65,  67,  75, '\0'};
png_byte FARDATA mng_BASI[5] = { 66,  65,  83,  73, '\0'};
png_byte FARDATA mng_CLIP[5] = { 67,  76,  73,  80, '\0'};
png_byte FARDATA mng_CLON[5] = { 67,  76,  79,  78, '\0'};
png_byte FARDATA mng_DEFI[5] = { 68,  69,  70,  73, '\0'};
png_byte FARDATA mng_DHDR[5] = { 68,  72,  68,  82, '\0'};
png_byte FARDATA mng_DISC[5] = { 68,  73,  83,  67, '\0'};
png_byte FARDATA mng_ENDL[5] = { 69,  78,  68,  76, '\0'};
png_byte FARDATA mng_FRAM[5] = { 70,  82,  65,  77, '\0'};
png_byte FARDATA mng_IDAT[5] = { 73,  68,  65,  84, '\0'};
png_byte FARDATA mng_IEND[5] = { 73,  69,  78,  68, '\0'};
png_byte FARDATA mng_IHDR[5] = { 73,  72,  68,  82, '\0'};
png_byte FARDATA mng_JDAT[5] = { 74,  68,  65,  84, '\0'};
png_byte FARDATA mng_JHDR[5] = { 74,  72,  68,  82, '\0'};
png_byte FARDATA mng_JSEP[5] = { 74,  83,  69,  80, '\0'};
png_byte FARDATA mng_LOOP[5] = { 76,  79,  79,  80, '\0'};
png_byte FARDATA mng_MEND[5] = { 77,  69,  78,  68, '\0'};
png_byte FARDATA mng_MOVE[5] = { 77,  79,  86,  69, '\0'};
png_byte FARDATA mng_PAST[5] = { 80,  65,  83,  84, '\0'};
png_byte FARDATA mng_PLTE[5] = { 80,  76,  84,  69, '\0'};
png_byte FARDATA mng_SAVE[5] = { 83,  65,  86,  69, '\0'};
png_byte FARDATA mng_SEEK[5] = { 83,  69,  69,  75, '\0'};
png_byte FARDATA mng_SHOW[5] = { 83,  72,  79,  87, '\0'};
png_byte FARDATA mng_TERM[5] = { 84,  69,  82,  77, '\0'};
png_byte FARDATA mng_bKGD[5] = { 98,  75,  71,  68, '\0'};
png_byte FARDATA mng_cHRM[5] = { 99,  72,  82,  77, '\0'};
png_byte FARDATA mng_gAMA[5] = {103,  65,  77,  65, '\0'};
png_byte FARDATA mng_hIST[5] = {104,  73,  83,  84, '\0'};
png_byte FARDATA mng_iCCP[5] = {105,  67,  67,  80, '\0'};
png_byte FARDATA mng_iTXt[5] = {105,  84,  88, 116, '\0'};
png_byte FARDATA mng_oFFs[5] = {111,  70,  70, 115, '\0'};
png_byte FARDATA mng_pHYg[5] = {112,  72,  89, 103, '\0'};
png_byte FARDATA mng_pHYs[5] = {112,  72,  89, 115, '\0'};
png_byte FARDATA mng_sBIT[5] = {115,  66,  73,  84, '\0'};
png_byte FARDATA mng_sPLT[5] = {115,  80,  76,  84, '\0'};
png_byte FARDATA mng_sRGB[5] = {115,  82,  71,  66, '\0'};
png_byte FARDATA mng_tEXt[5] = {116,  69,  88, 116, '\0'};
png_byte FARDATA mng_tIME[5] = {116,  73,  77,  69, '\0'};
png_byte FARDATA mng_tRNS[5] = {116,  82,  78,  83, '\0'};
png_byte FARDATA mng_zTXt[5] = {122,  84,  88, 116, '\0'};

typedef struct _MngBox
{
  long
    left,
    right,
    top,
    bottom;
} MngBox;

typedef struct _MngPair
{
  long
    a,
    b;
} MngPair;

#ifdef MNG_OBJECT_BUFFERS
typedef struct _MngBuffer
{

  unsigned long
    height,
    width;

  Image
    *image;

  png_color
    plte[256];

  int
    reference_count;

  unsigned char
    alpha_sample_depth,
    compression_method,
    color_type,
    concrete,
    filter_method,
    frozen,
    image_type,
    interlace_method,
    pixel_sample_depth,
    plte_length,
    sample_depth,
    viewable;
} MngBuffer;
#endif

typedef struct _Mng
{

#ifdef MNG_OBJECT_BUFFERS
  MngBuffer
    *ob[MNG_MAX_OBJECTS];
#endif

  Image *
    image;

#ifndef PNG_READ_EMPTY_PLTE_SUPPORTED
  png_byte
     read_buffer[8];

  int
     bytes_in_read_buffer,
     found_empty_plte,
     have_saved_bkgd_index,
     saved_bkgd_index;
#endif

  long
    x_off[MNG_MAX_OBJECTS],
    y_off[MNG_MAX_OBJECTS];

  MngBox
    object_clip[MNG_MAX_OBJECTS];

  unsigned char
    /* These flags could be combined into one byte */
    exists[MNG_MAX_OBJECTS],
    frozen[MNG_MAX_OBJECTS],
    visible[MNG_MAX_OBJECTS],
    viewable[MNG_MAX_OBJECTS];

  long
    loop_count[256],
    loop_iteration[256],
    loop_jump[256];

  unsigned char
    loop_active[256];

  png_colorp
    global_plte;

  png_color_8p
    global_sbit;

  png_byte
    global_trns[256];

  float
    global_gamma;

  ChromaticityInfo
    global_chrm;

  RenderingIntent
    global_srgb_intent;

  unsigned long
    global_x_pixels_per_unit,
    global_y_pixels_per_unit;

  unsigned int
    global_phys_unit_type;

  unsigned int
    basi_warning,
    clon_warning,
    dhdr_warning,
    jhdr_warning,
    past_warning,
    phyg_warning,
    phys_warning,
    sbit_warning,
    show_warning,
    verbose;

#ifdef MNG_BASI_SUPPORTED
  unsigned long
    basi_width,
    basi_height;

  unsigned int
    basi_depth,
    basi_color_type,
    basi_compression_method,
    basi_filter_type,
    basi_interlace_method,
    basi_red,
    basi_green,
    basi_blue,
    basi_alpha,
    basi_viewable;
#endif
} Mng;

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
