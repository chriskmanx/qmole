#include "config.h"
#ifdef BUILD_X11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>
#else
# define X_DISPLAY_MISSING
#endif
#include <string.h>
#include <stdarg.h>
#include "common.h"
#include "colormod.h"
#include "image.h"
#include "scale.h"
#include "blend.h"
#include "span.h"
#ifdef BUILD_X11
#include "context.h"
#include "color.h"
#include "grab.h"
#include "rend.h"
#include "rgba.h"
#include "ximage.h"
#include "draw.h"
#endif
#include "file.h"
#include "updates.h"
#include "rgbadraw.h"
#include "Imlib2.h"
#include <ft2build.h>
#include FT_FREETYPE_H
/*#ifdef HAVE_FREETYPE1_FREETYPE_FREETYPE_H
#include <freetype1/freetype/freetype.h>
#elif defined(HAVE_FREETYPE_FREETYPE_H)
#include <freetype/freetype.h>
#else
#include <freetype.h>
#endif
*/
#include "font.h"
#include "grad.h"
#include "rotate.h"
#include "filter.h"
#include "dynamic_filters.h"
#include "script.h"
#include <math.h>
#include "color_helpers.h"

/* convenience macros */
#define   CAST_IMAGE(im, image) (im) = (ImlibImage *)(image)
#define   CHECK_PARAM_POINTER_RETURN(func, sparam, param, ret) \
if (!(param)) \
{ \
  fprintf(stderr, "***** Imlib2 Developer Warning ***** :\n" \
                  "\tThis program is calling the Imlib call:\n\n" \
                  "\t%s();\n\n" \
                  "\tWith the parameter:\n\n" \
                  "\t%s\n\n" \
                  "\tbeing NULL. Please fix your program.\n", func, sparam); \
  return ret; \
}

#define   CHECK_PARAM_POINTER(func, sparam, param) \
if (!(param)) \
{ \
  fprintf(stderr, "***** Imlib2 Developer Warning ***** :\n" \
                  "\tThis program is calling the Imlib call:\n\n" \
                  "\t%s();\n\n" \
                  "\tWith the parameter:\n\n" \
                  "\t%s\n\n" \
                  "\tbeing NULL. Please fix your program.\n", func, sparam); \
  return; \
}

/* internal typedefs for function pointers */
typedef void        (*Imlib_Internal_Progress_Function) (void *, char, int, int,
                                                         int, int);
typedef void        (*Imlib_Internal_Data_Destructor_Function) (void *, void *);

struct _imlibcontext;
typedef struct _imlibcontext ImlibContext;

struct _imlibcontext {
#ifdef BUILD_X11
   Display            *display;
   Visual             *visual;
   Colormap            colormap;
   int                 depth;
   Drawable            drawable;
   Pixmap              mask;
#endif
   char                anti_alias;
   char                dither;
   char                blend;
   Imlib_Color_Modifier color_modifier;
   Imlib_Operation     operation;
   Imlib_Font          font;
   Imlib_Text_Direction direction;
   double              angle;
   Imlib_Color         color;
   Imlib_Color_Range   color_range;
   Imlib_Image         image;
   Imlib_Progress_Function progress_func;
   char                progress_granularity;
   char                dither_mask;
   int                 mask_alpha_threshold;
   Imlib_Filter        filter;
   Imlib_Rectangle     cliprect;
   Imlib_TTF_Encoding  encoding;

   int                 references;
   char                dirty;
};

struct _imlibcontextitem;
typedef struct _imlibcontextitem ImlibContextItem;
struct _imlibcontextitem {
   ImlibContext       *context;
   ImlibContextItem   *below;
};

/* a stack of contexts -- only used by context-handling functions. */
static ImlibContextItem *contexts = NULL;       /* (ImlibContext*) imlib_context_new(); */

/* this is the context all functions use rely on */
static ImlibContext *ctx = NULL;        /* contexts->context; */

/* frees the given context including all its members */
static void
__imlib_free_context(ImlibContext * context)
{
   ImlibContextItem   *next = contexts;

   if (ctx == context)
     {
        next = contexts->below;
        free(contexts);
        contexts = next;
     }

   ctx = context;

   if (ctx->image)
     {
        imlib_free_image();
        ctx->image = NULL;
     }
   if (ctx->font)
     {
        imlib_free_font();
        ctx->font = NULL;
     }
   if (ctx->color_modifier)
     {
        imlib_free_color_modifier();
        ctx->color_modifier = NULL;
     }
   if (ctx->filter)
     {
        imlib_free_filter();
        ctx->filter = NULL;
     }

   free(ctx);
   ctx = next->context;
}

EAPI                Imlib_Context
imlib_context_new(void)
{
   ImlibContext       *context = malloc(sizeof(ImlibContext));

#ifdef BUILD_X11
   context->display = NULL;
   context->visual = NULL;
   context->colormap = 0;
   context->depth = 0;
   context->drawable = 0;
   context->mask = 0;
#endif
   context->anti_alias = 1;
   context->dither = 0;
   context->blend = 1;
   context->color_modifier = NULL;
   context->operation = IMLIB_OP_COPY;
   context->font = NULL;
   context->direction = IMLIB_TEXT_TO_RIGHT;
   context->angle = 0.0;
   context->color.alpha = 255;
   context->color.red = 255;
   context->color.green = 255;
   context->color.blue = 255;
   context->color_range = NULL;
   context->image = NULL;
   context->progress_func = NULL;
   context->progress_granularity = 0;
   context->dither_mask = 0;
   context->mask_alpha_threshold = 128;
   context->filter = NULL;
   context->cliprect.x = 0;
   context->cliprect.y = 0;
   context->cliprect.w = 0;
   context->cliprect.h = 0;
   context->encoding = IMLIB_TTF_ENCODING_ISO_8859_1;

   context->references = 0;
   context->dirty = 0;

   return (Imlib_Context) context;
}

static ImlibContext *
_imlib_context_get(void)
{
   ImlibContext       *context;

   context = imlib_context_new();
   imlib_context_push(context);

   return context;
}

#define CHECK_CONTEXT(_ctx) \
   if (!_ctx) _ctx = _imlib_context_get()

/* frees the given context if it doesn't have any reference anymore. The
   last (default) context can never be freed. 
   If context is the current context, the context below will be made the
   current context.
*/
EAPI void
imlib_context_free(Imlib_Context context)
{
   ImlibContext       *c = (ImlibContext *) context;

   CHECK_PARAM_POINTER("imlib_context_free", "context", context);
   if (c == ctx && !contexts->below)
      return;

   if (c->references == 0)
      __imlib_free_context(c);
   else
      c->dirty = 1;
}

EAPI void
imlib_context_push(Imlib_Context context)
{
   ImlibContextItem   *item;

   CHECK_PARAM_POINTER("imlib_context_push", "context", context);
   ctx = (ImlibContext *) context;

   item = malloc(sizeof(ImlibContextItem));
   item->context = ctx;
   item->below = contexts;
   contexts = item;

   ctx->references++;
}

EAPI void
imlib_context_pop(void)
{
   ImlibContextItem   *item = contexts;
   ImlibContext       *current_ctx = item->context;

   if (!item->below)
      return;

   contexts = item->below;
   ctx = contexts->context;
   current_ctx->references--;
   if (current_ctx->dirty && current_ctx->references <= 0)
      __imlib_free_context(current_ctx);

   free(item);
}

EAPI                Imlib_Context
imlib_context_get(void)
{
   return (Imlib_Context) ctx;
}

/* context setting/getting functions */

/**
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * 
 * Sets the rectangle of the current context.
 **/
EAPI void
imlib_context_set_cliprect(int x, int y, int w, int h)
{
   CHECK_CONTEXT(ctx);
   ctx->cliprect.x = x;
   ctx->cliprect.y = y;
   ctx->cliprect.w = w;
   ctx->cliprect.h = h;
}

EAPI void
imlib_context_get_cliprect(int *x, int *y, int *w, int *h)
{
   CHECK_CONTEXT(ctx);
   *x = ctx->cliprect.x;
   *y = ctx->cliprect.y;
   *w = ctx->cliprect.w;
   *h = ctx->cliprect.h;
}

#ifdef BUILD_X11
/**
 * @param display Current display to be used.
 *
 * Sets the current X display to be used for rendering of images to
 * drawables. You do not need to set this if you do not intend to
 * render an image to an X drawable. If you do you will need to set
 * this. If you change displays just set this to the new display
 * pointer. Do not use a Display pointer if you have closed that
 * display already - also note that if you close a display connection
 * and continue to render using Imlib2 without setting the display
 * pointer to NULL or something new, crashes may occur.
 */
EAPI void
imlib_context_set_display(Display * display)
{
   CHECK_CONTEXT(ctx);
   ctx->display = display;
}

/** 
 * @return The current display.
 *
 * Returns the current display used for Imlib2's display context.
 */
EAPI Display       *
imlib_context_get_display(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->display;
}

/**
 * Tell Imlib2 that the current display connection has been closed.
 *
 * Call when (and only when) you close a display connection but continue
 * using Imlib2 on a different connection.
 */
EAPI void
imlib_context_disconnect_display(void)
{
   CHECK_CONTEXT(ctx);
   if (!ctx->display)
      return;
   __imlib_RenderDisconnect(ctx->display);
   ctx->display = NULL;
}

/**
 * @param visual Current visual to use.
 *
 * Sets the current visual to use when rendering images to
 * drawables or producing pixmaps. You need to set this for anything to
 * render to a drawable or produce any pixmaps (this can be the default
 * visual). 
 */
EAPI void
imlib_context_set_visual(Visual * visual)
{
   CHECK_CONTEXT(ctx);
   ctx->visual = visual;
   ctx->depth = imlib_get_visual_depth(ctx->display, ctx->visual);
}

/** 
 * @return The current visual.
 *
 * Returns the current visual used for Imlib2's context.
 */
EAPI Visual        *
imlib_context_get_visual(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->visual;
}

/**
 * @param colormap Colormap to use.
 * 
 * Sets the colormap to use when rendering to drawables and allocating
 * colors. You must set this to the colormap you are using to render any
 * images or produce any pixmaps (this can be the default colormap). 
 */
EAPI void
imlib_context_set_colormap(Colormap colormap)
{
   CHECK_CONTEXT(ctx);
   ctx->colormap = colormap;
}

/** 
 * @return The current colormap.
 *
 * Returns the current Colormap used for Imlib2's context.
 */
EAPI                Colormap
imlib_context_get_colormap(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->colormap;
}

/**
 * @param drawable An X drawable.
 * 
 * Sets the X drawable to which images will be rendered when you call
 * a render call in Imlib2. This may be either a pixmap or a
 * window. You must set this to render anything. 
 */
EAPI void
imlib_context_set_drawable(Drawable drawable)
{
   CHECK_CONTEXT(ctx);
   ctx->drawable = drawable;
}

/** 
 * @return The current drawable.
 *
 * Returns the current Drawable used for Imlib2's context.
 */
EAPI                Drawable
imlib_context_get_drawable(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->drawable;
}

/**
 * @param mask An 1-bit deep pixmap.
 * 
 * Sets the 1-bit deep pixmap to be drawn to when rendering to generate
 * a mask pixmap. This is only useful if the image you are rendering
 * has alpha. Set this to 0 to not render a pixmap mask. 
 */
EAPI void
imlib_context_set_mask(Pixmap mask)
{
   CHECK_CONTEXT(ctx);
   ctx->mask = mask;
}

/** 
 * @return The current pixmap.
 *
 * Returns the current pixmap destination to be used to render a mask into.
 */
EAPI                Pixmap
imlib_context_get_mask(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->mask;
}
#endif

/**
 * @param dither_mask The dither mask flag.
 * 
 * Selects if, you are rendering to a mask, or producing pixmap masks
 * from images, if the mask is to be dithered or not. passing in 1 for
 * dither_mask means the mask pixmap will be dithered, 0 means it will
 * not be dithered. 
 */
EAPI void
imlib_context_set_dither_mask(char dither_mask)
{
   CHECK_CONTEXT(ctx);
   ctx->dither_mask = dither_mask;
}

/** 
 * @return The current dither mask flag.
 *
 * Returns the current mode for dithering pixmap masks. 1 means
 * dithering is enabled and 0 means it is not. 
 */
EAPI char
imlib_context_get_dither_mask(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->dither_mask;
}

/**
 * @param mask_alpha_threshold The mask alpha threshold.
 * 
 * Selects, if you are rendering to a mask, the alpha threshold above which
 * mask bits are set. The default mask alpha threshold is 128, meaning that
 * a mask bit will be set if the pixel alpha is >= 128.
 */
EAPI void
imlib_context_set_mask_alpha_threshold(int mask_alpha_threshold)
{
   CHECK_CONTEXT(ctx);
   ctx->mask_alpha_threshold = mask_alpha_threshold;
}

/** 
 * @return The current mask mask alpha threshold.
 *
 * Returns the current mask alpha threshold.
 */
EAPI int
imlib_context_get_mask_alpha_threshold(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->mask_alpha_threshold;
}

/**
 * @param anti_alias The anti alias flag.
 * 
 * Toggles "anti-aliased" scaling of images. This
 * isn't quite correct since it's actually super and sub pixel
 * sampling that it turns on and off, but anti-aliasing is used for
 * having "smooth" edges to lines and shapes and this means when
 * images are scaled they will keep their smooth appearance. Passing
 * in 1 turns this on and 0 turns it off. 
 */
EAPI void
imlib_context_set_anti_alias(char anti_alias)
{
   CHECK_CONTEXT(ctx);
   ctx->anti_alias = anti_alias;
}

/** 
 * @return The current anti alias flag.
 *
 * Returns if Imlib2 currently will smoothly scale images. 1 means it
 * will and 0 means it will not.
 */
EAPI char
imlib_context_get_anti_alias(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->anti_alias;
}

/**
 * @param dither The dithering flag.
 * 
 * Sets the dithering flag for rendering to a drawable or when pixmaps
 * are produced. This affects the color image appearance by enabling
 * dithering. Dithering slows down rendering but produces considerably
 * better results. this option has no effect foe rendering in 24 bit
 * and up, but in 16 bit and lower it will dither, producing smooth
 * gradients and much better quality images. setting dither to 1
 * enables it and 0 disables it. 
 */
EAPI void
imlib_context_set_dither(char dither)
{
   CHECK_CONTEXT(ctx);
   ctx->dither = dither;
}

/** 
 * @return The current dithering flag.
 *
 * Returns if image data is rendered with dithering currently. 1 means
 * yes and 0 means no. 
 */
EAPI char
imlib_context_get_dither(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->dither;
}

/**
 * @param blend The blending flag.
 * 
 * When rendering an image to a drawable, Imlib2 is able to blend the
 * image directly onto the drawable during rendering. Setting this to 1
 * will enable this. If the image has no alpha channel this has no
 * effect. Setting it to 0 will disable this. 
 */
EAPI void
imlib_context_set_blend(char blend)
{
   CHECK_CONTEXT(ctx);
   ctx->blend = blend;
}

/** 
 * @return The current blending flag.
 *
 * Returns if Imlib2 will blend images onto a drawable whilst
 * rendering to that drawable. 1 means yes and 0 means no. 
 */
EAPI char
imlib_context_get_blend(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->blend;
}

/**
 * @param color_modifier Current color modifier.
 * 
 * Sets the current color modifier used for rendering pixmaps or
 * images to a drawable or images onto other images. Color modifiers
 * are lookup tables that map the values in the red, green, blue and
 * alpha channels to other values in the same channel when rendering,
 * allowing for fades, color correction etc. to be done whilst
 * rendering. pass in NULL as the color_modifier to disable the color
 * modifier for rendering. 
 */
EAPI void
imlib_context_set_color_modifier(Imlib_Color_Modifier color_modifier)
{
   CHECK_CONTEXT(ctx);
   ctx->color_modifier = color_modifier;
}

/** 
 * @return The current color modifier.
 *
 * Returns the current color modifier being used.
 */
EAPI                Imlib_Color_Modifier
imlib_context_get_color_modifier(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->color_modifier;
}

/**
 * @param operation 
 * 
 * When Imlib2 draws an image onto another or an image onto a drawable
 * it is able to do more than just blend the result on using the given
 * alpha channel of the image. It is also able to do saturating
 * additive, subtractive and a combination of the both (called reshade)
 * rendering. The default mode is IMLIB_OP_COPY. you can also set it to
 * IMLIB_OP_ADD, IMLIB_OP_SUBTRACT or IMLIB_OP_RESHADE. Use this
 * function to set the rendering operation. IMLIB_OP_COPY performs
 * basic alpha blending: DST = (SRC * A) + (DST * (1 -
 * A)). IMLIB_OP_ADD does DST = DST + (SRC * A). IMLIB_OP_SUBTRACT does
 * DST = DST - (SRC * A) and IMLIB_OP_RESHADE does DST = DST + (((SRC -
 * 0.5) / 2) * A). 
 */
EAPI void
imlib_context_set_operation(Imlib_Operation operation)
{
   CHECK_CONTEXT(ctx);
   ctx->operation = operation;
}

/** 
 * @return The current operation mode.
 *
 * Returns the current operation mode.
 */
EAPI                Imlib_Operation
imlib_context_get_operation(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->operation;
}

/**
 * @param font Current font.
 * 
 * Sets the current font to use when rendering text. you should load
 * the font first with imlib_load_font(). 
 */
EAPI void
imlib_context_set_font(Imlib_Font font)
{
   CHECK_CONTEXT(ctx);
   ctx->font = font;
}

/** 
 * @return The current font.
 *
 * Returns the current font.
 */
EAPI                Imlib_Font
imlib_context_get_font(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->font;
}

/**
 * @param direction Text direction.
 * 
 * Sets the direction in which to draw text in terms of simple 90
 * degree orientations or an arbitrary angle. The direction can be one
 * of IMLIB_TEXT_TO_RIGHT, IMLIB_TEXT_TO_LEFT, IMLIB_TEXT_TO_DOWN,
 * IMLIB_TEXT_TO_UP or IMLIB_TEXT_TO_ANGLE. The default is
 * IMLIB_TEXT_TO_RIGHT. If you use IMLIB_TEXT_TO_ANGLE, you will also
 * have to set the angle with imlib_context_set_angle(). 
 */
EAPI void
imlib_context_set_direction(Imlib_Text_Direction direction)
{
   CHECK_CONTEXT(ctx);
   ctx->direction = direction;
}

/**
 * @param angle Angle of the text strings.
 * 
 * Sets the angle at which text strings will be drawn if the text
 * direction has been set to IMLIB_TEXT_TO_ANGLE with
 * imlib_context_set_direction().
 */
EAPI void
imlib_context_set_angle(double angle)
{
   CHECK_CONTEXT(ctx);
   ctx->angle = angle;
}

/** 
 * @return The current angle of the text strings.
 *
 * Returns the current angle used to render text at if the direction
 * is IMLIB_TEXT_TO_ANGLE. 
 */
EAPI double
imlib_context_get_angle(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->angle;
}

/** 
 * @return The current direction of the text.
 *
 * Returns the current direction to render text in.
 */
EAPI                Imlib_Text_Direction
imlib_context_get_direction(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->direction;
}

/**
 * @param red Red channel of the current color.
 * @param green Green channel of the current color.
 * @param blue Blue channel of the current color.
 * @param alpha Alpha channel of the current color.
 * 
 * Sets the color with which text, lines and rectangles are drawn when
 * being rendered onto an image. Values for @p red, @p green, @p blue
 * and @p alpha are between 0 and 255 - any other values have
 * undefined results.
 */
EAPI void
imlib_context_set_color(int red, int green, int blue, int alpha)
{
   CHECK_CONTEXT(ctx);
   ctx->color.red = red;
   ctx->color.green = green;
   ctx->color.blue = blue;
   ctx->color.alpha = alpha;
}

/**
 * @param red Red channel of the current color.
 * @param green Green channel of the current color.
 * @param blue Blue channel of the current color.
 * @param alpha Alpha channel of the current color.
 * 
 * Returns the current color for rendering text, rectangles and lines.
 */
EAPI void
imlib_context_get_color(int *red, int *green, int *blue, int *alpha)
{
   CHECK_CONTEXT(ctx);
   *red = ctx->color.red;
   *green = ctx->color.green;
   *blue = ctx->color.blue;
   *alpha = ctx->color.alpha;
}

/** 
 * @return The current color.
 *
 * Returns the current color as a color struct. Do NOT free this
 * pointer. 
 */
EAPI Imlib_Color   *
imlib_context_get_imlib_color(void)
{
   CHECK_CONTEXT(ctx);
   return &ctx->color;
}

/**
 * @param hue Hue channel of the current color.
 * @param saturation Saturation channel of the current color.
 * @param value Value channel of the current color.
 * @param alpha Alpha channel of the current color.
 * 
 * Sets the color in HSVA space. Values for @p hue are between 0 and 360,
 * values for @p saturation and @p value between 0 and 1, and values for
 * @p alpha are between 0 and 255 - any other values have undefined
 * results.
 */
EAPI void
imlib_context_set_color_hsva(float hue, float saturation, float value,
                             int alpha)
{
   int                 r, g, b;

   __imlib_hsv_to_rgb(hue, saturation, value, &r, &g, &b);
   imlib_context_set_color(r, g, b, alpha);
}

/**
 * @param hue Hue channel of the current color.
 * @param saturation Saturation channel of the current color.
 * @param value Value channel of the current color.
 * @param alpha Alpha channel of the current color.
 * 
 * Returns the current color for rendering text, rectangles and lines
 * in HSVA space. 
 */
EAPI void
imlib_context_get_color_hsva(float *hue, float *saturation, float *value,
                             int *alpha)
{
   int                 r, g, b;

   imlib_context_get_color(&r, &g, &b, alpha);
   __imlib_rgb_to_hsv(r, g, b, hue, saturation, value);
}

/**
 * @param hue Hue channel of the current color.
 * @param lightness Lightness channel of the current color.
 * @param saturation Saturation channel of the current color.
 * @param alpha Alpha channel of the current color.
 * 
 * Sets the color in HLSA space. Values for @p hue are between 0 and 360,
 * values for @p lightness and @p saturation between 0 and 1, and values for
 * @p alpha are between 0 and 255 - any other values have undefined
 * results. 
 */
EAPI void
imlib_context_set_color_hlsa(float hue, float lightness, float saturation,
                             int alpha)
{
   int                 r, g, b;

   __imlib_hls_to_rgb(hue, lightness, saturation, &r, &g, &b);
   imlib_context_set_color(r, g, b, alpha);
}

/**
 * @param hue Hue channel of the current color.
 * @param lightness Lightness channel of the current color.
 * @param saturation Saturation channel of the current color.
 * @param alpha Alpha channel of the current color.
 * 
 * Returns the current color for rendering text, rectangles and lines
 * in HLSA space. 
 */
EAPI void
imlib_context_get_color_hlsa(float *hue, float *lightness, float *saturation,
                             int *alpha)
{
   int                 r, g, b;

   imlib_context_get_color(&r, &g, &b, alpha);
   __imlib_rgb_to_hls(r, g, b, hue, lightness, saturation);
}

/**
 * @param cyan Cyan channel of the current color.
 * @param magenta Magenta channel of the current color.
 * @param yellow Yellow channel of the current color.
 * @param alpha Alpha channel of the current color.
 * 
 * Sets the color in CMYA space. Values for @p cyan, @p magenta, @p yellow and
 * @p alpha are between 0 and 255 - any other values have undefined
 * results. 
 */
EAPI void
imlib_context_set_color_cmya(int cyan, int magenta, int yellow, int alpha)
{
   CHECK_CONTEXT(ctx);
   ctx->color.red = 255 - cyan;
   ctx->color.green = 255 - magenta;
   ctx->color.blue = 255 - yellow;
   ctx->color.alpha = alpha;
}

/**
 * @param cyan Cyan channel of the current color.
 * @param magenta Magenta channel of the current color.
 * @param yellow Yellow channel of the current color.
 * @param alpha Alpha channel of the current color.
 * 
 * Returns the current color for rendering text, rectangles and lines
 * in CMYA space. 
 */
EAPI void
imlib_context_get_color_cmya(int *cyan, int *magenta, int *yellow, int *alpha)
{
   CHECK_CONTEXT(ctx);
   *cyan = 255 - ctx->color.red;
   *magenta = 255 - ctx->color.green;
   *yellow = 255 - ctx->color.blue;
   *alpha = ctx->color.alpha;
}

/**
 * @param color_range Color range.
 * 
 * Sets the current color range to use for rendering gradients.
 */
EAPI void
imlib_context_set_color_range(Imlib_Color_Range color_range)
{
   CHECK_CONTEXT(ctx);
   ctx->color_range = color_range;
}

/**
 * @return The current color range.
 * 
 * Returns the current color range being used for gradients.
 */
EAPI                Imlib_Color_Range
imlib_context_get_color_range(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->color_range;
}

/**
 * @param progress_function A progress function.
 * 
 * Sets the progress function to be called back whilst loading
 * images. Set this to the function to be called, or set it to NULL to
 * disable progress callbacks whilst loading. 
 */
EAPI void
imlib_context_set_progress_function(Imlib_Progress_Function progress_function)
{
   CHECK_CONTEXT(ctx);
   ctx->progress_func = progress_function;
}

/**
 * @return The current progress function.
 * 
 * Returns the current progress function being used.
 */
EAPI                Imlib_Progress_Function
imlib_context_get_progress_function(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->progress_func;
}

/**
 * @param progress_granularity A char.
 * 
 * This hints as to how often to call the progress callback. 0 means
 * as often as possible. 1 means whenever 15 more of the image has been
 * decoded, 10 means every 10% of the image decoding, 50 means every
 * 50% and 100 means only call at the end. Values outside of the range
 * 0-100 are undefined. 
 */
EAPI void
imlib_context_set_progress_granularity(char progress_granularity)
{
   CHECK_CONTEXT(ctx);
   ctx->progress_granularity = progress_granularity;
}

/**
 * @return The current progress granularity
 * 
 * Returns the current progress granularity being used.
 */
EAPI char
imlib_context_get_progress_granularity(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->progress_granularity;
}

/**
 * @param image Current image.
 * 
 * Sets the current image Imlib2 will be using with its function calls.
 */
EAPI void
imlib_context_set_image(Imlib_Image image)
{
   CHECK_CONTEXT(ctx);
   ctx->image = image;
}

/**
 * @return The current image.
 * 
 * Returns the current context image.
 */
EAPI                Imlib_Image
imlib_context_get_image(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->image;
}

EAPI void
imlib_context_set_TTF_encoding(Imlib_TTF_Encoding encoding)
{
   CHECK_CONTEXT(ctx);
   ctx->encoding = encoding;
}

EAPI                Imlib_TTF_Encoding
imlib_context_get_TTF_encoding(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->encoding;
}

/* imlib api */

/**
 * @return The current image size.
 * 
 * Returns the current size of the image cache in bytes. The cache is
 * a unified cache used for image data AND pixmaps. 
 */
EAPI int
imlib_get_cache_size(void)
{
   CHECK_CONTEXT(ctx);
   return __imlib_GetCacheSize();
}

/**
 * @param bytes Cache size.
 * 
 * Sets the cache size. The size is in bytes. Setting the cache size to
 * 0 effectively flushes the cache and keeps the cache size at 0 until
 * set to another value. Whenever you set the cache size Imlib2 will
 * flush as many old images and pixmap from the cache as needed until
 * the current cache usage is less than or equal to the cache size. 
 */
EAPI void
imlib_set_cache_size(int bytes)
{
   CHECK_CONTEXT(ctx);
   __imlib_SetCacheSize(bytes);
}

/**
 * @return The current number of colors.
 * 
 * Gets the number of colors Imlib2 currently at a maximum is allowed
 * to allocate for rendering. The default is 256.
 */
EAPI int
imlib_get_color_usage(void)
{
   CHECK_CONTEXT(ctx);
#ifdef BUILD_X11
   return (int)_max_colors;
#else
   return 256;
#endif
}

/**
 * @param max Maximum number of colors.
 * 
 * Sets the maximum number of colors you would like Imlib2 to allocate
 * for you when rendering. The default is 256. This has no effect in
 * depths greater than 8 bit.
 */
EAPI void
imlib_set_color_usage(int max)
{
   CHECK_CONTEXT(ctx);
#ifdef BUILD_X11
   if (max < 2)
      max = 2;
   else if (max > 256)
      max = 256;
   _max_colors = max;
#endif
}

/**
 * If you want Imlib2 to forcibly flush any cached loaders it has and
 * re-load them from disk (this is useful if the program just
 * installed a new loader and does not want to wait till Imlib2 deems
 * it an optimal time to rescan the loaders) 
 */
EAPI void
imlib_flush_loaders(void)
{
   CHECK_CONTEXT(ctx);
   __imlib_RemoveAllLoaders();
}

#ifdef BUILD_X11
/**
 * @param display The current display
 * @param visual The current visual
 * @return 
 * 
 * Convenience function that returns the depth of a visual for that
 * display. 
 */
EAPI int
imlib_get_visual_depth(Display * display, Visual * visual)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_get_visual_depth", "display", display, 0);
   CHECK_PARAM_POINTER_RETURN("imlib_get_visual_depth", "visual", visual, 0);
   return __imlib_XActualDepth(display, visual);
}

/**
 * @param display The current display
 * @param screen The screen
 * @param depth_return The depth of the returned visual.
 * @return The best visual.
 * 
 * Returns the visual for the display @p display and the screen @p
 * screen that Imlib2 thinks
 * will give you the best quality output. @p depth_return should point to
 * an int that will be filled with the depth of that visual too. 
 */
EAPI Visual        *
imlib_get_best_visual(Display * display, int screen, int *depth_return)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_get_best_visual", "display", display,
                              NULL);
   CHECK_PARAM_POINTER_RETURN("imlib_get_best_visual", "depth_return",
                              depth_return, NULL);
   return __imlib_BestVisual(display, screen, depth_return);
}
#endif

/**
 * @param file Image file.
 * @return An image handle.
 * 
 * Loads an image from disk located at the path specified by
 * @p file. Please see the section \ref loading for more
 * detail. Returns an image handle on success or NULL on failure. 
 */
EAPI                Imlib_Image
imlib_load_image(const char *file)
{
   Imlib_Image         im = NULL;
   Imlib_Image         prev_ctxt_image;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_load_image", "file", file, NULL);
   prev_ctxt_image = ctx->image;
   im = __imlib_LoadImage(file, (ImlibProgressFunction) ctx->progress_func,
                          ctx->progress_granularity, 0, 0, NULL);
   ctx->image = prev_ctxt_image;
   return (Imlib_Image) im;
}

/**
 * @param file Image file.
 * @return An image handle.
 * 
 * Loads an image from disk located at the path specified by
 * @p file. This forces the image data to be decoded at load time too,
 * instead of decoding being deferred until it is needed. Returns an
 * image handle on success or NULL on failure. 
 */
EAPI                Imlib_Image
imlib_load_image_immediately(const char *file)
{
   Imlib_Image         im = NULL;
   Imlib_Image         prev_ctxt_image;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_load_image_immediately", "file", file,
                              NULL);
   prev_ctxt_image = ctx->image;
   im = __imlib_LoadImage(file, (ImlibProgressFunction) ctx->progress_func,
                          ctx->progress_granularity, 1, 0, NULL);
   ctx->image = prev_ctxt_image;
   return (Imlib_Image) im;
}

/**
 * @param file Image file.
 * @return An image handle.
 * 
 * Loads the image without looking in the cache first. Returns an
 * image handle on success or NULL on failure.
 */
EAPI                Imlib_Image
imlib_load_image_without_cache(const char *file)
{
   Imlib_Image         im = NULL;
   Imlib_Image         prev_ctxt_image;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_load_image_without_cache", "file",
                              file, NULL);
   prev_ctxt_image = ctx->image;
   im = __imlib_LoadImage(file, (ImlibProgressFunction) ctx->progress_func,
                          ctx->progress_granularity, 0, 1, NULL);
   ctx->image = prev_ctxt_image;
   return (Imlib_Image) im;
}

/**
 * @param file Image file.
 * @return An image handle.
 * 
 * Loads the image without deferred image data decoding (i.e. it is
 * decoded straight away) and without looking in the cache. Returns an
 * image handle on success or NULL on failure. 
 */
EAPI                Imlib_Image
imlib_load_image_immediately_without_cache(const char *file)
{
   Imlib_Image         im = NULL;
   Imlib_Image         prev_ctxt_image;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_load_image_immediately_without_cache",
                              "file", file, NULL);
   prev_ctxt_image = ctx->image;
   im = __imlib_LoadImage(file, (ImlibProgressFunction) ctx->progress_func,
                          ctx->progress_granularity, 1, 1, NULL);
   ctx->image = prev_ctxt_image;
   return (Imlib_Image) im;
}

/**
 * @param file Image file.
 * @param error_return The returned error.
 * @return An image handle.
 * 
 * Loads an image at the path @p file on disk. If it succeeds it returns
 * a valid image handle, if not NULL is returned and @p error_return
 * is set to the detail of the error. 
 */
EAPI                Imlib_Image
imlib_load_image_with_error_return(const char *file,
                                   Imlib_Load_Error * error_return)
{
   Imlib_Image         im = NULL;
   ImlibLoadError      er;
   Imlib_Image         prev_ctxt_image;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_load_image_with_error_return", "file",
                              file, NULL);
   if (!__imlib_FileExists(file))
     {
        *error_return = IMLIB_LOAD_ERROR_FILE_DOES_NOT_EXIST;
        return NULL;
     }
   if (__imlib_FileIsDir(file))
     {
        *error_return = IMLIB_LOAD_ERROR_FILE_IS_DIRECTORY;
        return NULL;
     }
   if (!__imlib_FileCanRead(file))
     {
        *error_return = IMLIB_LOAD_ERROR_PERMISSION_DENIED_TO_READ;
        return NULL;
     }
   prev_ctxt_image = ctx->image;
   im = (Imlib_Image) __imlib_LoadImage(file,
                                        (ImlibProgressFunction)
                                        ctx->progress_func,
                                        ctx->progress_granularity, 1, 0, &er);
   ctx->image = prev_ctxt_image;
   if (im)
      *error_return = IMLIB_LOAD_ERROR_NONE;
   else
     {
        if (er == IMLIB_LOAD_ERROR_NONE)
           *error_return = IMLIB_LOAD_ERROR_NO_LOADER_FOR_FILE_FORMAT;
        else
           *error_return = (Imlib_Load_Error) er;
     }
   return im;
}

/**
 * Frees the image that is set as the current image in Imlib2's context.
 */
EAPI void
imlib_free_image(void)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_free_image", "image", ctx->image);
   __imlib_FreeImage(ctx->image);
   ctx->image = NULL;
}

/**
 * Frees the current image in Imlib2's context AND removes it from the
 * cache.
 */
EAPI void
imlib_free_image_and_decache(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_free_image_and_decache", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   SET_FLAG(im->flags, F_INVALID);
   __imlib_FreeImage(im);
   ctx->image = NULL;
}

/**
 * Returns the width in pixels of the current image in Imlib2's context.
 */
EAPI int
imlib_image_get_width(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_get_width", "image", ctx->image, 0);
   CAST_IMAGE(im, ctx->image);
   return im->w;
}

/**
 * Returns the height in pixels of the current image in Imlib2's context.
 */
EAPI int
imlib_image_get_height(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_get_height", "image", ctx->image, 0);
   CAST_IMAGE(im, ctx->image);
   return im->h;
}

/**
 * @return The current filename.
 * 
 * Returns the filename for the file that is set as the current
 * context. The pointer returned is only valid as long as no operations
 * cause the filename of the image to change. Saving the file with a
 * different name would cause this. It is suggested you duplicate the
 * string if you wish to continue to use the string for later
 * processing. Do not free the string pointer returned by this
 * function. 
 */
EAPI const char    *
imlib_image_get_filename(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_get_filename", "image", ctx->image,
                              0);
   CAST_IMAGE(im, ctx->image);
   /* strdup() the returned value if you want to alter it! */
   return (const char *)(im->file);
}

/**
 * @return A pointer to the image data.
 * 
 * Returns a pointer to the image data in the image set as the image
 * for the current context. When you get this pointer it is assumed you
 * are planning on writing to the data, thus once you do this the image
 * can no longer be used for caching - in fact all images cached from
 * this one will also be affected when you put the data back. If this
 * matters it is suggested you clone the image first before playing
 * with the image data. The image data is returned in the format of a
 * DATA32 (32 bits) per pixel in a linear array ordered from the top
 * left of the image to the bottom right going from left to right each
 * line. Each pixel has the upper 8 bits as the alpha channel and the
 * lower 8 bits are the blue channel - so a pixel's bits are ARGB (from
 * most to least significant, 8 bits per channel). You must put the
 * data back at some point. 
 */
EAPI DATA32        *
imlib_image_get_data(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_get_data", "image", ctx->image,
                              NULL);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!im->data)
      return NULL;
   __imlib_DirtyImage(im);
   return im->data;
}

/**
 * @return A pointer to the image data.
 * 
 * Functions the same way as imlib_image_get_data(), but returns a
 * pointer expecting the program to NOT write to the data returned (it
 * is for inspection purposes only). Writing to this data has undefined
 * results. The data does not need to be put back. 
 */
EAPI DATA32        *
imlib_image_get_data_for_reading_only(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_get_data_for_reading_only",
                              "image", ctx->image, NULL);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!im->data)
      return NULL;
   return im->data;
}

/**
 * @param data The pointer to the image data.
 * 
 * Puts back @p data when it was obtained by
 * imlib_image_get_data(). @p data must be the same pointer returned
 * by imlib_image_get_data(). This operated on the current context
 * image.  
 */
EAPI void
imlib_image_put_back_data(DATA32 * data)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_put_back_data", "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_image_put_back_data", "data", data);
   CAST_IMAGE(im, ctx->image);
   __imlib_DirtyImage(im);
   data = NULL;
}

/**
 * @return Current alpha channel flag.
 * 
 * Returns 1 if the current context image has an alpha channel, or 0
 * if it does not (the alpha data space is still there and available -
 * just "unused"). 
 */
EAPI char
imlib_image_has_alpha(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_has_alpha", "image", ctx->image, 0);
   CAST_IMAGE(im, ctx->image);
   if (IMAGE_HAS_ALPHA(im))
      return 1;
   return 0;
}

/**
 * By default Imlib2 will not check the timestamp of an image on disk
 * and compare it with the image in its cache - this is to minimize
 * disk activity when using the cache. Call this function and it will
 * flag the current context image as being liable to change on disk
 * and Imlib2 will check the timestamp of the image file on disk and
 * compare it with the cached image when it next needs to use this
 * image in the cache.
 */
EAPI void
imlib_image_set_changes_on_disk(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_set_never_changes_on_disk", "image",
                       ctx->image);
   CAST_IMAGE(im, ctx->image);
   SET_FLAG(im->flags, F_ALWAYS_CHECK_DISK);
}

/**
 * @param border The border of the image.
 * 
 * Fills the Imlib_Border structure to which @p border points to with the
 * values of the border of the current context image. The border is the
 * area at the edge of the image that does not scale with the rest of
 * the image when resized - the borders remain constant in size. This
 * is useful for scaling bevels at the edge of images differently to
 * the image center.
 */
EAPI void
imlib_image_get_border(Imlib_Border * border)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_get_border", "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_image_get_border", "border", border);
   CAST_IMAGE(im, ctx->image);
   border->left = im->border.left;
   border->right = im->border.right;
   border->top = im->border.top;
   border->bottom = im->border.bottom;
}

/**
 * @param border The border of the image.
 * 
 * Sets the border of the current context image to the values contained
 * in the Imlib_Border structure @p border points to. 
 */
EAPI void
imlib_image_set_border(Imlib_Border * border)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_set_border", "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_image_set_border", "border", border);
   CAST_IMAGE(im, ctx->image);
   if ((im->border.left == border->left)
       && (im->border.right == border->right)
       && (im->border.top == border->top)
       && (im->border.bottom == border->bottom))
      return;
   im->border.left = border->left;
   im->border.right = border->right;
   im->border.top = border->top;
   im->border.bottom = border->bottom;
   __imlib_DirtyPixmapsForImage(im);
}

/**
 * @param format Format of the image.
 * 
 * Sets the format of the current image. This is used for when you
 * wish to save an image in a different format that it was loaded in,
 * or if the image currently has no file format associated with it. 
 */
EAPI void
imlib_image_set_format(const char *format)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_set_format", "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_image_set_format", "format", format);
   CAST_IMAGE(im, ctx->image);
   if (im->format)
      free(im->format);
   if (format)
      im->format = strdup(format);
   else
      im->format = NULL;
   if (!(im->flags & F_FORMAT_IRRELEVANT))
     {
        __imlib_DirtyImage(im);
     }
}

/**
 * @param irrelevant Irrelevant format flag.
 * 
 * Sets if the format value of the current image is irrelevant for
 * caching purposes - by default it is. pass irrelevant as 1 to make it
 * irrelevant and 0 to make it relevant for caching. 
 */
EAPI void
imlib_image_set_irrelevant_format(char irrelevant)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_set_irrelevant_format", "image",
                       ctx->image);
   CAST_IMAGE(im, ctx->image);
   if (irrelevant)
     {
        SET_FLAG(im->flags, F_FORMAT_IRRELEVANT);
     }
   else
     {
        UNSET_FLAG(im->flags, F_FORMAT_IRRELEVANT);
     }
}

/**
 * @param irrelevant Irrelevant border flag.
 * 
 * Sets if the border of the current image is irrelevant for caching
 * purposes. By default it is. Set irrelevant to 1 to make it
 * irrelevant, and 0 to make it relevant. 
 */
EAPI void
imlib_image_set_irrelevant_border(char irrelevant)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_set_irrelevant_border", "image",
                       ctx->image);
   CAST_IMAGE(im, ctx->image);
   if (irrelevant)
     {
        SET_FLAG(im->flags, F_BORDER_IRRELEVANT);
     }
   else
     {
        UNSET_FLAG(im->flags, F_BORDER_IRRELEVANT);
     }
}

/**
 * @param irrelevant Irrelevant alpha flag.
 * 
 * Sets if the alpha channel status of the current image (i.e. if
 * there is or is not one) is important for caching purposes. By
 * default it is not. Set irrelevant to 1 to make it irrelevant and 0
 * to make it relevant. 
 */
EAPI void
imlib_image_set_irrelevant_alpha(char irrelevant)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_set_irrelevant_alpha", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if (irrelevant)
     {
        SET_FLAG(im->flags, F_ALPHA_IRRELEVANT);
     }
   else
     {
        UNSET_FLAG(im->flags, F_ALPHA_IRRELEVANT);
     }
}

/**
 * @return Current image format.
 * 
 * Returns the current image's format. Do not free this
 * string. Duplicate it if you need it for later use. 
 */
EAPI char          *
imlib_image_format(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_format", "image", ctx->image, NULL);
   CAST_IMAGE(im, ctx->image);
   return im->format;
}

/**
 * @param has_alpha Alpha flag.
 * 
 * Sets the alpha flag for the current image. Set @p has_alpha to 1 to
 * enable the alpha channel in the current image, or 0 to disable it. 
 */
EAPI void
imlib_image_set_has_alpha(char has_alpha)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_set_has_alpha", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if (has_alpha)
      SET_FLAG(im->flags, F_HAS_ALPHA);
   else
      UNSET_FLAG(im->flags, F_HAS_ALPHA);
}

#ifdef BUILD_X11
/**
 * @param pixmap_return The returned pixmap.
 * @param mask_return The returned mask.
 * 
 * Creates a pixmap of the current image (and a mask if the image has
 * an alpha value) and return the id's of the pixmap and mask to 
 * @p pixmap_return and @p mask_return pixmap id's. You must free these
 * pixmaps using Imlib2's free function imlib_free_pixmap_and_mask(). 
 */
EAPI void
imlib_render_pixmaps_for_whole_image(Pixmap * pixmap_return,
                                     Pixmap * mask_return)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_render_pixmaps_for_whole_image", "image",
                       ctx->image);
   CHECK_PARAM_POINTER("imlib_render_pixmaps_for_whole_image",
                       "pixmap_return", pixmap_return);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_CreatePixmapsForImage(ctx->display, ctx->drawable, ctx->visual,
                                 ctx->depth, ctx->colormap, im, pixmap_return,
                                 mask_return, 0, 0, im->w, im->h, im->w,
                                 im->h, 0, ctx->dither, ctx->dither_mask,
                                 ctx->mask_alpha_threshold,
                                 ctx->color_modifier);
}

/**
 * @param pixmap_return The returned pixmap.
 * @param mask_return The returned mask.
 * @param width Width of the pixmap.
 * @param height Height of the pixmap.
 * 
 * Works just like imlib_render_pixmaps_for_whole_image(), but will
 * scale the output result to the width @p width and height @p height
 * specified. Scaling
 * is done before depth conversion so pixels used for dithering don't
 * grow large. 
 */
EAPI void
imlib_render_pixmaps_for_whole_image_at_size(Pixmap * pixmap_return,
                                             Pixmap * mask_return, int width,
                                             int height)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_render_pixmaps_for_whole_image_at_size",
                       "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_render_pixmaps_for_whole_image_at_size",
                       "pixmap_return", pixmap_return);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);

   if (!(im->data))
      return;
   __imlib_CreatePixmapsForImage(ctx->display, ctx->drawable, ctx->visual,
                                 ctx->depth, ctx->colormap, im, pixmap_return,
                                 mask_return, 0, 0, im->w, im->h, width,
                                 height, ctx->anti_alias, ctx->dither,
                                 ctx->dither_mask, ctx->mask_alpha_threshold,
                                 ctx->color_modifier);
}

/**
 * @param pixmap The pixmap.
 * 
 * Frees @p pixmap (and any mask generated in association with that
 * pixmap). The pixmap will remain cached until the image the pixmap
 * was generated from is dirtied or decached, or the cache is flushed. 
 */
EAPI void
imlib_free_pixmap_and_mask(Pixmap pixmap)
{
   CHECK_CONTEXT(ctx);
   __imlib_FreePixmap(ctx->display, pixmap);
}

/**
 * @param x X coordinate of the pixel.
 * @param y Y coordinate of the pixel.
 * 
 * Renders the current image onto the current drawable at the (@p x, @p y)
 * pixel location specified without scaling. 
 */
EAPI void
imlib_render_image_on_drawable(int x, int y)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_render_image_on_drawable", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_RenderImage(ctx->display, im, ctx->drawable, ctx->mask,
                       ctx->visual, ctx->colormap, ctx->depth, 0, 0, im->w,
                       im->h, x, y, im->w, im->h, 0, ctx->dither, ctx->blend,
                       ctx->dither_mask, ctx->mask_alpha_threshold,
                       ctx->color_modifier, ctx->operation);
}

/**
 * @param x X coordinate of the pixel.
 * @param y Y coordinate of the pixel.
 * @param width Width of the rendered image.
 * @param height Height of the rendered image.
 * 
 * Renders the current image onto the current drawable at the (@p x, @p y)
 * location specified AND scale the image to the width @p width and height
 * @p height.
 */
EAPI void
imlib_render_image_on_drawable_at_size(int x, int y, int width, int height)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_render_image_on_drawable_at_size", "image",
                       ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_RenderImage(ctx->display, im, ctx->drawable, ctx->mask,
                       ctx->visual, ctx->colormap, ctx->depth, 0, 0, im->w,
                       im->h, x, y, width, height, ctx->anti_alias,
                       ctx->dither, ctx->blend, ctx->dither_mask,
                       ctx->mask_alpha_threshold, ctx->color_modifier,
                       ctx->operation);
}

/**
 * @param source_x X coordinate of the source image.
 * @param source_y Y coordinate of the source image.
 * @param source_width Width of the source image.
 * @param source_height Height of the source image.
 * @param x X coordinate of the destination image.
 * @param y Y coordinate of the destination image.
 * @param width Width of the destination image.
 * @param height Height of the destination image.
 * 
 * Renders the source (@p source_x, @p source_y, @p source_width, @p source_height) pixel
 * rectangle from the 
 * current image onto the current drawable at the (@p x, @p y) location scaled
 * to the width @p width and height @p height.
 */
EAPI void
imlib_render_image_part_on_drawable_at_size(int source_x, int source_y,
                                            int source_width,
                                            int source_height, int x, int y,
                                            int width, int height)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_render_image_part_on_drawable_at_size", "image",
                       ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_RenderImage(ctx->display, im, ctx->drawable, 0, ctx->visual,
                       ctx->colormap, ctx->depth, source_x, source_y,
                       source_width, source_height, x, y, width, height,
                       ctx->anti_alias, ctx->dither, ctx->blend, 0,
                       0, ctx->color_modifier, ctx->operation);
}

EAPI                DATA32
imlib_render_get_pixel_color(void)
{
   CHECK_CONTEXT(ctx);
   return __imlib_RenderGetPixel(ctx->display, ctx->drawable, ctx->visual,
                                 ctx->colormap, ctx->depth,
                                 (DATA8) ctx->color.red,
                                 (DATA8) ctx->color.green,
                                 (DATA8) ctx->color.blue);
}

#endif

/**
 * @param source_image The source image.
 * @param merge_alpha Alpha flag.
 * @param source_x X coordinate of the source image.
 * @param source_y Y coordinate of the source image.
 * @param source_width Width of the source image.
 * @param source_height Height of the source image.
 * @param destination_x X coordinate of the destination image.
 * @param destination_y Y coordinate of the destination image.
 * @param destination_width Width of the destination image.
 * @param destination_height Height of the destination image.
 * 
 * Blends the source rectangle (@p source_x, @p source_y, @p
 * source_width, @p source_height) from 
 * @p source_image onto the current image at the destination (@p
 * destination_x, @p destination_y) location
 * scaled to the width @p destination_width and height @p
 * destination_height. If @p merge_alpha is set to 1
 * it will also modify the destination image alpha channel, otherwise
 * the destination alpha channel is left untouched. 
 */
EAPI void
imlib_blend_image_onto_image(Imlib_Image source_image, char merge_alpha,
                             int source_x, int source_y, int source_width,
                             int source_height, int destination_x,
                             int destination_y, int destination_width,
                             int destination_height)
{
   ImlibImage         *im_src, *im_dst;
   int                 aa;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_blend_image_onto_image", "source_image",
                       source_image);
   CHECK_PARAM_POINTER("imlib_blend_image_onto_image", "image", ctx->image);
   CAST_IMAGE(im_src, source_image);
   CAST_IMAGE(im_dst, ctx->image);
   if ((!(im_src->data)) && (im_src->loader) && (im_src->loader->load))
      im_src->loader->load(im_src, NULL, 0, 1);
   if (!(im_src->data))
      return;
   if ((!(im_dst->data)) && (im_dst->loader) && (im_dst->loader->load))
      im_dst->loader->load(im_dst, NULL, 0, 1);
   if (!(im_dst->data))
      return;
   __imlib_DirtyImage(im_dst);
   /* FIXME: hack to get around infinite loops for scaling down too far */
   aa = ctx->anti_alias;
   if ((abs(destination_width) < (source_width >> 7))
       || (abs(destination_height) < (source_height >> 7)))
      aa = 0;
   __imlib_BlendImageToImage(im_src, im_dst, aa, ctx->blend,
                             merge_alpha, source_x, source_y, source_width,
                             source_height, destination_x, destination_y,
                             destination_width, destination_height,
                             ctx->color_modifier, ctx->operation,
                             ctx->cliprect.x, ctx->cliprect.y,
                             ctx->cliprect.w, ctx->cliprect.h);
}

/**
 * @param width The width of the image.
 * @param height The height of the image.
 * @return A new blank image.
 * 
 * Creates a new blank image of size @p width and @p height. The contents of
 * this image at creation time are undefined (they could be garbage
 * memory). You are free to do whatever you like with this image. It
 * is not cached. On success an image handle is returned - on failure
 * NULL is returned.
 **/
EAPI                Imlib_Image
imlib_create_image(int width, int height)
{
   DATA32             *data;

   CHECK_CONTEXT(ctx);
   if ((width <= 0) || (height <= 0))
      return NULL;
   data = malloc(width * height * sizeof(DATA32));
   if (data)
      return (Imlib_Image) __imlib_CreateImage(width, height, data);
   return NULL;
}

/**
 * @param width The width of the image.
 * @param height The height of the image.
 * @param data The data.
 * @return A valid image, otherwise NULL.
 * 
 * Creates an image from the image data specified with the width @p width and
 * the height @p height specified. The image data @p data must be in the same format as
 * imlib_image_get_data() would return. You are responsible for
 * freeing this image data once the image is freed - Imlib2 will not
 * do that for you. This is useful for when you already have static
 * buffers of the same format Imlib2 uses (many video grabbing devices
 * use such a format) and wish to use Imlib2 to render the results
 * onto another image, or X drawable. You should free the image when
 * you are done with it. Imlib2 returns a valid image handle on
 * success or NULL on failure 
 * 
 **/
EAPI                Imlib_Image
imlib_create_image_using_data(int width, int height, DATA32 * data)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_create_image_using_data", "data", data,
                              NULL);
   if ((width <= 0) || (height <= 0))
      return NULL;
   im = __imlib_CreateImage(width, height, data);
   if (im)
      SET_FLAG(im->flags, F_DONT_FREE_DATA);
   return (Imlib_Image) im;
}

/**
 * @param width The width of the image.
 * @param height The height of the image.
 * @param data The data.
 * @return A valid image, otherwise NULL.
 * 
 * Works the same way as imlib_create_image_using_data() but Imlib2
 * copies the image data to the image structure. You may now do
 * whatever you wish with the original data as it will not be needed
 * anymore. Imlib2 returns a valid image handle on success or NULL on
 * failure. 
 * 
 **/
EAPI                Imlib_Image
imlib_create_image_using_copied_data(int width, int height, DATA32 * data)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_create_image_using_copied_data", "data",
                              data, NULL);
   if ((width <= 0) || (height <= 0))
      return NULL;
   im = __imlib_CreateImage(width, height, NULL);
   if (!im)
      return NULL;
   im->data = malloc(width * height * sizeof(DATA32));
   if (data)
     {
        memcpy(im->data, data, width * height * sizeof(DATA32));
        return (Imlib_Image) im;
     }
   else
      __imlib_FreeImage(im);
   return NULL;
}

#ifdef BUILD_X11
/**
 * @param mask A mask.
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * @param need_to_grab_x Grab flag.
 * @return a valid image, otherwise NULL.
 * 
 * Return an image (using the mask @p mask to determine the alpha channel)
 * from the current drawable.
 * If @p mask is 0 it will not create a useful alpha channel in the image.
 * If @p mask is 1 the mask will be set to the shape mask of the drawable.
 * It will create an image from the
 * (@p x, @p y, @p width , @p height) rectangle in the drawable. If @p
 * need_to_grab_x is 1 it will also grab the X Server to avoid possible race
 * conditions in grabbing. If you have not already grabbed the server
 * you MUST set this to 1. Imlib2 returns a valid image handle on
 * success or NULL on failure. 
 * 
 **/
EAPI                Imlib_Image
imlib_create_image_from_drawable(Pixmap mask, int x, int y, int width,
                                 int height, char need_to_grab_x)
{
   ImlibImage         *im;
   char                domask = 0;

   CHECK_CONTEXT(ctx);
   if (mask)
     {
        domask = 1;
        if (mask == (Pixmap) 1)
           mask = None;
     }
   im = __imlib_CreateImage(width, height, NULL);
   im->data = malloc(width * height * sizeof(DATA32));
   if (__imlib_GrabDrawableToRGBA(im->data, 0, 0, width, height, ctx->display,
                                  ctx->drawable, mask, ctx->visual,
                                  ctx->colormap, ctx->depth, x, y, width,
                                  height, &domask, need_to_grab_x))
     {
        if (domask)
           SET_FLAG(im->flags, F_HAS_ALPHA);
        else
           UNSET_FLAG(im->flags, F_HAS_ALPHA);
     }
   else
     {
        __imlib_FreeImage(im);
        im = NULL;
     }

   return (Imlib_Image) im;
}

/**
 * @param image An image.
 * @param mask A mask.
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * @param need_to_grab_x Grab flag.
 * @return a valid image, otherwise NULL.
 * 
 * 
 **/
EAPI                Imlib_Image
imlib_create_image_from_ximage(XImage * image, XImage * mask, int x, int y,
                               int width, int height, char need_to_grab_x)
{
   ImlibImage         *im;
   char                domask = 0;

   CHECK_CONTEXT(ctx);
   if (mask)
      domask = 1;
   im = __imlib_CreateImage(width, height, NULL);
   im->data = malloc(width * height * sizeof(DATA32));
   __imlib_GrabXImageToRGBA(im->data, 0, 0, width, height,
                            ctx->display, image, mask, ctx->visual,
                            ctx->depth, x, y, width, height, need_to_grab_x);
   return (Imlib_Image) im;
}

/**
 * @param mask A mask.
 * @param source_x The top left x coordinate of the rectangle.
 * @param source_y The top left y coordinate of the rectangle.
 * @param source_width The width of the rectangle.
 * @param source_height The height of the rectangle.
 * @param destination_width The width of the returned image.
 * @param destination_height The height of the returned image.
 * @param need_to_grab_x Grab flag.
 * @param get_mask_from_shape A char.
 * @return A valid image, otherwise NULL.
 * 
 * Creates an image from the current drawable (optionally using the
 * @p mask pixmap specified to determine alpha transparency) and scale
 * the grabbed data first before converting to an actual image (to
 * minimize reads from the frame buffer which can be slow). The source
 * (@p source_x, @p source_y, @p source_width, @p source_height) rectangle will be grabbed, scaled to the
 * destination @p destination_width and @p destination_height, then converted to an image. If
 * @p need_to_grab_x is set to 1, X is grabbed (set this to 1 unless you
 * have already grabbed the server) and if @p get_mask_from_shape and the
 * current drawable is a window its shape is used for determining the
 * alpha channel. If successful this function will return a valid
 * image handle, otherwise NULL is returned. 
 * 
 **/
EAPI                Imlib_Image
imlib_create_scaled_image_from_drawable(Pixmap mask, int source_x,
                                        int source_y, int source_width,
                                        int source_height,
                                        int destination_width,
                                        int destination_height,
                                        char need_to_grab_x,
                                        char get_mask_from_shape)
{
   ImlibImage         *im;
   char                domask = 0, tmpmask = 0;
   int                 x, xx;
   XGCValues           gcv;
   GC                  gc = 0, mgc = 0;
   Pixmap              p, m;

   CHECK_CONTEXT(ctx);
   if ((mask) || (get_mask_from_shape))
      domask = 1;
   p = XCreatePixmap(ctx->display, ctx->drawable, destination_width,
                     source_height, ctx->depth);
   gcv.foreground = 0;
   gcv.subwindow_mode = IncludeInferiors;
   gcv.graphics_exposures = False;
   if (domask)
     {
        m = XCreatePixmap(ctx->display, ctx->drawable, destination_width,
                          source_height, 1);
        mgc = XCreateGC(ctx->display, m,
                        GCForeground | GCGraphicsExposures, &gcv);
     }
   else
      m = None;
   gc = XCreateGC(ctx->display, ctx->drawable,
                  GCSubwindowMode | GCGraphicsExposures, &gcv);
   if ((domask) && (!mask))
     {
        XRectangle         *rect;
        int                 rect_num, rect_ord;

        tmpmask = 1;
        mask =
           XCreatePixmap(ctx->display, ctx->drawable, source_width,
                         source_height, 1);
        rect =
           XShapeGetRectangles(ctx->display, ctx->drawable, ShapeBounding,
                               &rect_num, &rect_ord);
        XFillRectangle(ctx->display, mask, mgc, 0, 0, source_width,
                       source_height);
        if (rect)
          {
             XSetForeground(ctx->display, mgc, 1);
             for (x = 0; x < rect_num; x++)
                XFillRectangle(ctx->display, mask, mgc, rect[x].x, rect[x].y,
                               rect[x].width, rect[x].height);
             XFree(rect);
          }
        /* build mask from window shape rects */
     }
   for (x = 0; x < destination_width; x++)
     {
        xx = (source_width * x) / destination_width;
        XCopyArea(ctx->display, ctx->drawable, p, gc, source_x + xx, source_y,
                  1, source_height, x, 0);
        if (m != None)
           XCopyArea(ctx->display, mask, m, mgc, xx, 0, 1, source_height, x, 0);
     }
   for (x = 0; x < destination_height; x++)
     {
        xx = (source_height * x) / destination_height;
        XCopyArea(ctx->display, p, p, gc, 0, xx, destination_width, 1, 0, x);
        if (m != None)
           XCopyArea(ctx->display, m, m, mgc, 0, xx, destination_width, 1, 0,
                     x);
     }
   im = __imlib_CreateImage(destination_width, destination_height, NULL);
   im->data = malloc(destination_width * destination_height * sizeof(DATA32));
   __imlib_GrabDrawableToRGBA(im->data, 0, 0, destination_width,
                              source_height, ctx->display, p, m,
                              ctx->visual, ctx->colormap, ctx->depth, 0, 0,
                              destination_width, destination_height, &domask,
                              need_to_grab_x);
   if (domask)
      SET_FLAG(im->flags, F_HAS_ALPHA);
   else
      UNSET_FLAG(im->flags, F_HAS_ALPHA);
   XFreePixmap(ctx->display, p);
   if (m != None)
     {
        XFreeGC(ctx->display, mgc);
        XFreePixmap(ctx->display, m);
        if (tmpmask)
           XFreePixmap(ctx->display, mask);
     }
   XFreeGC(ctx->display, gc);
   return (Imlib_Image) im;
}

/**
 * @param mask A mask.
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * @param destination_x The x coordinate of the new location.
 * @param destination_y The x coordinate of the new location.
 * @param need_to_grab_x Grab flag.
 * @return A char.
 * 
 * Grabs a section of the current drawable (optionally using the pixmap @p mask
 * provided as a corresponding mask for that drawable - if @p mask is 0
 * this is not used).
 * If @p mask is 1 the mask will be set to the shape mask of the drawable.
 * It grabs the (@p x, @p y, @p width, @p height) rectangle and
 * places it at the destination (@p destination_x, @p destination_y) location in the current image. If
 * @p need_to_grab_x is 1 it will grab and ungrab the server whilst doing
 * this - you need to do this if you have not already grabbed the
 * server. 
 * 
 **/
EAPI char
imlib_copy_drawable_to_image(Pixmap mask, int x, int y, int width, int height,
                             int destination_x, int destination_y,
                             char need_to_grab_x)
{
   ImlibImage         *im;
   char                domask = 0;
   int                 pre_adj;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_copy_drawable_to_image", "image",
                              ctx->image, 0);
   if (mask)
     {
        domask = 1;
        if (mask == (Pixmap) 1)
           mask = None;
     }
   CAST_IMAGE(im, ctx->image);

   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return 0;

   pre_adj = 0;
   if (x < 0)
     {
        width += x;
        pre_adj = x;
        x = 0;
     }
   if (width < 0)
      width = 0;
   if (destination_x < 0)
     {
        width += destination_x;
        x -= destination_x - pre_adj;
        destination_x = 0;
     }
   if ((destination_x + width) >= im->w)
      width = im->w - destination_x;

   pre_adj = 0;
   if (y < 0)
     {
        height += y;
        pre_adj = y;
        y = 0;
     }
   if (height < 0)
      height = 0;
   if (destination_y < 0)
     {
        height += destination_y;
        y -= destination_y - pre_adj;
        destination_y = 0;
     }
   if ((destination_y + height) >= im->h)
      height = im->h - destination_y;

   if ((width <= 0) || (height <= 0))
      return 0;
   __imlib_DirtyImage(im);
   return __imlib_GrabDrawableToRGBA(im->data, destination_x, destination_y,
                                     im->w, im->h, ctx->display,
                                     ctx->drawable, mask, ctx->visual,
                                     ctx->colormap, ctx->depth, x, y, width,
                                     height, &domask, need_to_grab_x);
}
#endif

/**
 * @return A valid image, otherwise NULL.
 * 
 * Creates an exact duplicate of the current image and returns a valid
 * image handle on success, or NULL on failure. 
 * 
 **/
EAPI                Imlib_Image
imlib_clone_image(void)
{
   ImlibImage         *im, *im_old;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_clone_image", "image", ctx->image, NULL);
   CAST_IMAGE(im_old, ctx->image);
   if ((!(im_old->data)) && (im_old->loader) && (im_old->loader->load))
      im_old->loader->load(im_old, NULL, 0, 1);
   if (!(im_old->data))
      return NULL;
   im = __imlib_CreateImage(im_old->w, im_old->h, NULL);
   if (!(im))
      return NULL;
   im->data = malloc(im->w * im->h * sizeof(DATA32));
   if (!(im->data))
     {
        __imlib_FreeImage(im);
        return NULL;
     }
   memcpy(im->data, im_old->data, im->w * im->h * sizeof(DATA32));
   im->flags = im_old->flags;
   SET_FLAG(im->flags, F_UNCACHEABLE);
   im->moddate = im_old->moddate;
   im->border = im_old->border;
   im->loader = im_old->loader;
   if (im_old->format)
     {
        im->format = malloc(strlen(im_old->format) + 1);
        strcpy(im->format, im_old->format);
     }
   if (im_old->file)
     {
        im->file = malloc(strlen(im_old->file) + 1);
        strcpy(im->file, im_old->file);
     }
   return (Imlib_Image) im;
}

/**
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * @return A valid image, otherwise NULL.
 * 
 * Creates a duplicate of a (@p x, @p y, @p width, @p height) rectangle in the
 * current image and returns a valid image handle on success, or NULL
 * on failure. 
 * 
 **/
EAPI                Imlib_Image
imlib_create_cropped_image(int x, int y, int width, int height)
{
   ImlibImage         *im, *im_old;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_create_cropped_image", "image",
                              ctx->image, NULL);
   CAST_IMAGE(im_old, ctx->image);
   if ((!(im_old->data)) && (im_old->loader) && (im_old->loader->load))
      im_old->loader->load(im_old, NULL, 0, 1);
   if (!(im_old->data))
      return NULL;
   im = __imlib_CreateImage(abs(width), abs(height), NULL);
   im->data = malloc(abs(width * height) * sizeof(DATA32));
   if (!(im->data))
     {
        __imlib_FreeImage(im);
        return NULL;
     }
   if (IMAGE_HAS_ALPHA(im_old))
     {
        SET_FLAG(im->flags, F_HAS_ALPHA);
        __imlib_BlendImageToImage(im_old, im, 0, 0, 1, x, y, abs(width),
                                  abs(height), 0, 0, width, height, NULL,
                                  IMLIB_OP_COPY,
                                  ctx->cliprect.x, ctx->cliprect.y,
                                  ctx->cliprect.w, ctx->cliprect.h);
     }
   else
     {
        __imlib_BlendImageToImage(im_old, im, 0, 0, 0, x, y, abs(width),
                                  abs(height), 0, 0, width, height, NULL,
                                  IMLIB_OP_COPY,
                                  ctx->cliprect.x, ctx->cliprect.y,
                                  ctx->cliprect.w, ctx->cliprect.h);
     }
   return (Imlib_Image) im;
}

/**
 * @param source_x The top left x coordinate of the source rectangle.
 * @param source_y The top left y coordinate of the source rectangle.
 * @param source_width The width of the source rectangle.
 * @param source_height The height of the source rectangle.
 * @param destination_width The width of the destination image.
 * @param destination_height The height of the destination image.
 * @return A valid image, otherwise NULL.
 * 
 * Works the same as imlib_create_cropped_image() but will scale the
 * new image to the new destination @p destination_width and
 * @p destination_height whilst cropping.  
 * 
 **/
EAPI                Imlib_Image
imlib_create_cropped_scaled_image(int source_x, int source_y,
                                  int source_width, int source_height,
                                  int destination_width, int destination_height)
{
   ImlibImage         *im, *im_old;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_create_cropped_scaled_image", "image",
                              ctx->image, NULL);
   CAST_IMAGE(im_old, ctx->image);
   if ((!(im_old->data)) && (im_old->loader) && (im_old->loader->load))
      im_old->loader->load(im_old, NULL, 0, 1);
   if (!(im_old->data))
      return NULL;
   im = __imlib_CreateImage(abs(destination_width), abs(destination_height),
                            NULL);
   im->data =
      malloc(abs(destination_width * destination_height) * sizeof(DATA32));
   if (!(im->data))
     {
        __imlib_FreeImage(im);
        return NULL;
     }
   if (IMAGE_HAS_ALPHA(im_old))
     {
        SET_FLAG(im->flags, F_HAS_ALPHA);
        __imlib_BlendImageToImage(im_old, im, ctx->anti_alias, 0, 1, source_x,
                                  source_y, source_width, source_height, 0, 0,
                                  destination_width, destination_height, NULL,
                                  IMLIB_OP_COPY,
                                  ctx->cliprect.x, ctx->cliprect.y,
                                  ctx->cliprect.w, ctx->cliprect.h);
     }
   else
     {
        __imlib_BlendImageToImage(im_old, im, ctx->anti_alias, 0, 0, source_x,
                                  source_y, source_width, source_height, 0, 0,
                                  destination_width, destination_height, NULL,
                                  IMLIB_OP_COPY,
                                  ctx->cliprect.x, ctx->cliprect.y,
                                  ctx->cliprect.w, ctx->cliprect.h);
     }
   return (Imlib_Image) im;
}

/**
 * @param updates An updates list.
 * @return Duplicate of @p updates.
 * 
 * Creates a duplicate of the updates list passed into the function.
 **/
EAPI                Imlib_Updates
imlib_updates_clone(Imlib_Updates updates)
{
   ImlibUpdate        *u;

   CHECK_CONTEXT(ctx);
   u = (ImlibUpdate *) updates;
   return (Imlib_Updates) __imlib_DupUpdates(u);
}

/**
 * @param updates An updates list.
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * @return The updates handle.
 * 
 * Appends an update rectangle to the updates list passed in (if the
 * updates is NULL it will create a new updates list) and returns a
 * handle to the modified updates list (the handle may be modified so
 * only use the new updates handle returned). 
 **/
EAPI                Imlib_Updates
imlib_update_append_rect(Imlib_Updates updates, int x, int y, int w, int h)
{
   ImlibUpdate        *u;

   CHECK_CONTEXT(ctx);
   u = (ImlibUpdate *) updates;
   return (Imlib_Updates) __imlib_AddUpdate(u, x, y, w, h);
}

/**
 * @param updates An updates list.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * @return The updates handle.
 * 
 * Takes an updates list, and modifies it by merging overlapped
 * rectangles and lots of tiny rectangles into larger rectangles to
 * minimize the number of rectangles in the list for optimized
 * redrawing. The new updates handle is now valid and the old one
 * passed in is not. 
 **/
EAPI                Imlib_Updates
imlib_updates_merge(Imlib_Updates updates, int w, int h)
{
   ImlibUpdate        *u;

   CHECK_CONTEXT(ctx);
   u = (ImlibUpdate *) updates;
   return (Imlib_Updates) __imlib_MergeUpdate(u, w, h, 0);
}

/**
 * @param updates An updates list.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * @return The updates handle.
 * 
 * Works almost exactly as imlib_updates_merge() but is more lenient
 * on the spacing between update rectangles - if they are very close it
 * amalgamates 2 smaller rectangles into 1 larger one. 
 **/
EAPI                Imlib_Updates
imlib_updates_merge_for_rendering(Imlib_Updates updates, int w, int h)
{
   ImlibUpdate        *u;

   CHECK_CONTEXT(ctx);
   u = (ImlibUpdate *) updates;
   return (Imlib_Updates) __imlib_MergeUpdate(u, w, h, 3);
}

/**
 * @param updates An updates list.
 * 
 * Frees an updates list.
 **/
EAPI void
imlib_updates_free(Imlib_Updates updates)
{
   ImlibUpdate        *u;

   CHECK_CONTEXT(ctx);
   u = (ImlibUpdate *) updates;
   __imlib_FreeUpdates(u);
}

/**
 * @param updates An updates list.
 * @return The next updates.
 * 
 * Gets the next update in the updates list relative to the one passed
 * in. 
 **/
EAPI                Imlib_Updates
imlib_updates_get_next(Imlib_Updates updates)
{
   ImlibUpdate        *u;

   CHECK_CONTEXT(ctx);
   u = (ImlibUpdate *) updates;
   return (Imlib_Updates) (u->next);
}

/**
 * @param updates An updates list.
 * @param x_return The top left x coordinate of the update.
 * @param y_return The top left y coordinate of the update.
 * @param width_return The width of the update.
 * @param height_return The height of the update.
 * 
 * Returns the coordinates of an update.
 **/
EAPI void
imlib_updates_get_coordinates(Imlib_Updates updates, int *x_return,
                              int *y_return, int *width_return,
                              int *height_return)
{
   ImlibUpdate        *u;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_updates_get_coordinates", "updates", updates);
   u = (ImlibUpdate *) updates;
   if (x_return)
      *x_return = u->x;
   if (y_return)
      *y_return = u->y;
   if (width_return)
      *width_return = u->w;
   if (height_return)
      *height_return = u->h;
}

/**
 * @param updates An updates list.
 * @param x The top left x coordinate of the update.
 * @param y The top left y coordinate of the update.
 * @param width The width of the update.
 * @param height The height of the update.
 * 
 * Modifies the coordinates of an update in @p update.
 **/
EAPI void
imlib_updates_set_coordinates(Imlib_Updates updates, int x, int y, int width,
                              int height)
{
   ImlibUpdate        *u;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_updates_set_coordinates", "updates", updates);
   u = (ImlibUpdate *) updates;
   u->x = x;
   u->y = y;
   u->w = width;
   u->h = height;
}

#ifdef BUILD_X11
/**
 * @param updates An updates list.
 * @param x The top left x coordinate of the update.
 * @param y The top left y coordinate of the update.
 * 
 * Given an updates list (preferable already merged for rendering)
 * this will render the corresponding parts of the image to the current
 * drawable at an offset of @p x, @p y in the drawable. 
 **/
EAPI void
imlib_render_image_updates_on_drawable(Imlib_Updates updates, int x, int y)
{
   ImlibUpdate        *u;
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_render_image_updates_on_drawable", "image",
                       ctx->image);
   CAST_IMAGE(im, ctx->image);
   u = (ImlibUpdate *) updates;
   if (!updates)
      return;
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_SetMaxXImageCount(ctx->display, 10);
   for (; u; u = u->next)
     {
        __imlib_RenderImage(ctx->display, im, ctx->drawable, 0, ctx->visual,
                            ctx->colormap, ctx->depth, u->x, u->y, u->w, u->h,
                            x + u->x, y + u->y, u->w, u->h, 0, ctx->dither, 0,
                            0, 0, ctx->color_modifier, OP_COPY);
     }
   __imlib_SetMaxXImageCount(ctx->display, 0);
}
#endif

/**
 * @return The initialized updates list.
 * 
 * Initializes an updates list before you add any updates to it or
 * merge it for rendering etc. 
 **/
EAPI                Imlib_Updates
imlib_updates_init(void)
{
   CHECK_CONTEXT(ctx);
   return (Imlib_Updates) NULL;
}

/**
 * @param updates An updates list.
 * @param appended_updates The updates list to append.
 * @return The new updates list.
 * 
 * Appends @p appended_updates to the updates list @p updates and
 * returns the new list.  
 **/
EAPI                Imlib_Updates
imlib_updates_append_updates(Imlib_Updates updates,
                             Imlib_Updates appended_updates)
{
   ImlibUpdate        *u, *uu;

   CHECK_CONTEXT(ctx);
   u = (ImlibUpdate *) updates;
   uu = (ImlibUpdate *) appended_updates;
   if (!uu)
      return (Imlib_Updates) u;
   if (!u)
      return (Imlib_Updates) uu;
   while (u)
     {
        if (!(u->next))
          {
             u->next = uu;
             return updates;
          }
        u = u->next;
     }
   return (Imlib_Updates) u;
}

/**
 * Flips/mirrors the current image horizontally.
 **/
EAPI void
imlib_image_flip_horizontal(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_flip_horizontal", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_FlipImageHoriz(im);
}

/**
 * Flips/mirrors the current image vertically.
 **/
EAPI void
imlib_image_flip_vertical(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_flip_vertical", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_FlipImageVert(im);
}

/**
 * Flips/mirrors the current image diagonally (good for quick and dirty
 * 90 degree rotations if used before to after a horizontal or vertical
 * flip). 
 **/
EAPI void
imlib_image_flip_diagonal(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_flip_diagonal", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_FlipImageDiagonal(im, 0);
}

/**
 * @param orientation The orientation.
 * 
 * Performs 90 degree rotations on the current image. Passing in
 * @p orientation does not rotate, 1 rotates clockwise by 90 degree, 2,
 * rotates clockwise by 180 degrees, 3 rotates clockwise by 270
 * degrees. 
 **/
EAPI void
imlib_image_orientate(int orientation)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_orientate", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   switch (orientation)
     {
     default:
     case 0:
        break;
     case 1:
        __imlib_FlipImageDiagonal(im, 1);
        break;
     case 2:
        __imlib_FlipImageBoth(im);
        break;
     case 3:
        __imlib_FlipImageDiagonal(im, 2);
        break;
     case 4:
        __imlib_FlipImageHoriz(im);
        break;
     case 5:
        __imlib_FlipImageDiagonal(im, 3);
        break;
     case 6:
        __imlib_FlipImageVert(im);
        break;
     case 7:
        __imlib_FlipImageDiagonal(im, 0);
        break;
     }
}

/**
 * @param radius The radius.
 * 
 * Blurs the current image. A @p radius value of 0 has no effect, 1 and above
 * determine the blur matrix radius that determine how much to blur the
 * image. 
 **/
EAPI void
imlib_image_blur(int radius)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_blur", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_BlurImage(im, radius);
}

/**
 * @param radius The radius.
 * 
 * Sharpens the current image. The @p radius value affects how much to sharpen
 * by. 
 **/
EAPI void
imlib_image_sharpen(int radius)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CAST_IMAGE(im, ctx->image);
   CHECK_PARAM_POINTER("imlib_image_sharpen", "image", ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_SharpenImage(im, radius);
}

/**
 * Modifies an image so it will tile seamlessly horizontally if used
 * as a tile (i.e. drawn multiple times horizontally).
 **/
EAPI void
imlib_image_tile_horizontal(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_tile_horizontal", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_TileImageHoriz(im);
}

/**
 * Modifies an image so it will tile seamlessly vertically if used as
 * a tile (i.e. drawn multiple times vertically).
 **/
EAPI void
imlib_image_tile_vertical(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_tile_vertical", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_TileImageVert(im);
}

/**
 * Modifies an image so it will tile seamlessly horizontally and
 * vertically if used as a tile (i.e. drawn multiple times horizontally
 * and vertically).
 **/
EAPI void
imlib_image_tile(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_tile", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_TileImageHoriz(im);
   __imlib_TileImageVert(im);
}

/**
 * @param font_name The font name with the size.
 * @return NULL if no font found.
 * 
 * Loads a truetype font from the first directory in the font path that
 * contains that font. The font name @p font_name format is "font_name/size". For
 * example. If there is a font file called blum.ttf somewhere in the
 * font path you might use "blum/20" to load a 20 pixel sized font of
 * blum. If the font cannot be found NULL is returned. 
 * 
 **/
EAPI                Imlib_Font
imlib_load_font(const char *font_name)
{
   return imlib_font_load_joined(font_name);
}

/**
 * Removes the current font from any fallback chain it's in and frees it.
 **/
EAPI void
imlib_free_font(void)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_free_font", "font", ctx->font);
   imlib_remove_font_from_fallback_chain(ctx->font);
   imlib_font_free(ctx->font);
   ctx->font = NULL;
}

/**
 * @param font A previously loaded font.
 * @param fallback_font A previously loaded font to be chained to the given font.
 * @return 0 on success.
 *
 * This arranges for the given fallback font to be used if a glyph does not
 * exist in the given font when text is being rendered.
 * Fonts can be arranged in an aribitrarily long chain and attempts will be
 * made in order on the chain.
 * Cycles in the chain are not possible since the given fallback font is
 * removed from any chain it's already in.
 * A fallback font may be a member of only one chain. Adding it as the
 * fallback font to another font will remove it from it's first fallback chain.
 *
 * @deprecated This function may be removed.
 **/
EAPI int
imlib_insert_font_into_fallback_chain(Imlib_Font font, Imlib_Font fallback_font)
{
   CHECK_PARAM_POINTER_RETURN("imlib_insert_font_into_fallback_chain",
                              "font", font, 1);
   CHECK_PARAM_POINTER_RETURN("imlib_insert_font_into_fallback_chain",
                              "fallback_font", fallback_font, 1);
   return imlib_font_insert_into_fallback_chain_imp(font, fallback_font);
}

/**
 * @param fallback_font A font previously added to a fallback chain.
 * @return 0 on success.
 *
 * This removes the given font from any fallback chain it may be in.
 * Removing this font joins its previous and next font together in the fallback
 * chain.
 *
 * @deprecated This function may be removed.
 **/
EAPI void
imlib_remove_font_from_fallback_chain(Imlib_Font fallback_font)
{
   CHECK_PARAM_POINTER("imlib_remove_font_from_fallback_chain",
                       "fallback_font", fallback_font);
   imlib_font_remove_from_fallback_chain_imp(fallback_font);
}

/**
 * @deprecated This function may be removed.
 **/
EAPI                Imlib_Font
imlib_get_prev_font_in_fallback_chain(Imlib_Font fn)
{
   CHECK_PARAM_POINTER_RETURN("imlib_get_prev_font_in_fallback_chain",
                              "fn", fn, 0);
   return ((ImlibFont *) fn)->fallback_prev;
}

/**
 * @deprecated This function may be removed.
 **/
EAPI                Imlib_Font
imlib_get_next_font_in_fallback_chain(Imlib_Font fn)
{
   CHECK_PARAM_POINTER_RETURN("imlib_get_next_font_in_fallback_chain",
                              "fn", fn, 0);
   return ((ImlibFont *) fn)->fallback_next;
}

/**
 * @param x The x coordinate of the top left  corner.
 * @param y The y coordinate of the top left  corner.
 * @param text A null-byte terminated string.
 * 
 * Draws the null-byte terminated string @p text using the current font on
 * the current image at the (@p x, @p y) location (@p x, @p y denoting the top left
 * corner of the font string) 
 **/
EAPI void
imlib_text_draw(int x, int y, const char *text)
{
   CHECK_CONTEXT(ctx);
   imlib_text_draw_with_return_metrics(x, y, text, NULL, NULL, NULL, NULL);
}

/**
 * @param x The x coordinate of the top left  corner.
 * @param y The y coordinate of the top left  corner.
 * @param text A null-byte terminated string.
 * @param width_return The width of the string.
 * @param height_return The height of the string.
 * @param horizontal_advance_return Horizontal offset.
 * @param vertical_advance_return Vertical offset.
 * 
 * Works just like imlib_text_draw() but also returns the width and
 * height of the string drawn, and @p horizontal_advance_return returns
 * the number of pixels you should advance horizontally to draw another
 * string (useful if you are drawing a line of text word by word) and
 * @p vertical_advance_return does the same for the vertical direction
 * (i.e. drawing text line by line). 
 **/
EAPI void
imlib_text_draw_with_return_metrics(int x, int y, const char *text,
                                    int *width_return, int *height_return,
                                    int *horizontal_advance_return,
                                    int *vertical_advance_return)
{
   ImlibImage         *im;
   ImlibFont          *fn;
   int                 dir;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_text_draw_with_return_metrics", "font",
                       ctx->font);
   CHECK_PARAM_POINTER("imlib_text_draw_with_return_metrics", "image",
                       ctx->image);
   CHECK_PARAM_POINTER("imlib_text_draw_with_return_metrics", "text", text);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   fn = (ImlibFont *) ctx->font;
   __imlib_DirtyImage(im);

   dir = ctx->direction;
   if (ctx->direction == IMLIB_TEXT_TO_ANGLE && ctx->angle == 0.0)
      dir = IMLIB_TEXT_TO_RIGHT;

   imlib_render_str(im, fn, x, y, text, (DATA8) ctx->color.red,
                    (DATA8) ctx->color.green, (DATA8) ctx->color.blue,
                    (DATA8) ctx->color.alpha, (char)dir,
                    ctx->angle, width_return, height_return, 0,
                    horizontal_advance_return, vertical_advance_return,
                    ctx->operation,
                    ctx->cliprect.x, ctx->cliprect.y,
                    ctx->cliprect.w, ctx->cliprect.h);
}

/**
 * @param text A string.
 * @param width_return The width of the text.
 * @param height_return The height of the text.
 * 
 * Gets the width and height in pixels the @p text string would use up
 * if drawn with the current font. 
 **/
EAPI void
imlib_get_text_size(const char *text, int *width_return, int *height_return)
{
   ImlibFont          *fn;
   int                 w, h;
   int                 dir;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_get_text_size", "font", ctx->font);
   CHECK_PARAM_POINTER("imlib_get_text_size", "text", text);
   fn = (ImlibFont *) ctx->font;

   dir = ctx->direction;
   if (ctx->direction == IMLIB_TEXT_TO_ANGLE && ctx->angle == 0.0)
      dir = IMLIB_TEXT_TO_RIGHT;

   imlib_font_query_size(fn, text, &w, &h);

   switch (dir)
     {
     case IMLIB_TEXT_TO_RIGHT:
     case IMLIB_TEXT_TO_LEFT:
        if (width_return)
           *width_return = w;
        if (height_return)
           *height_return = h;
        break;
     case IMLIB_TEXT_TO_DOWN:
     case IMLIB_TEXT_TO_UP:
        if (width_return)
           *width_return = h;
        if (height_return)
           *height_return = w;
        break;
     case IMLIB_TEXT_TO_ANGLE:
        if (width_return || height_return)
          {
             double              sa, ca;

             sa = sin(ctx->angle);
             ca = cos(ctx->angle);

             if (width_return)
               {
                  double              x1, x2, xt;

                  x1 = x2 = 0.0;
                  xt = ca * w;
                  if (xt < x1)
                     x1 = xt;
                  if (xt > x2)
                     x2 = xt;
                  xt = -(sa * h);
                  if (xt < x1)
                     x1 = xt;
                  if (xt > x2)
                     x2 = xt;
                  xt = ca * w - sa * h;
                  if (xt < x1)
                     x1 = xt;
                  if (xt > x2)
                     x2 = xt;
                  *width_return = (int)(x2 - x1);
               }
             if (height_return)
               {
                  double              y1, y2, yt;

                  y1 = y2 = 0.0;
                  yt = sa * w;
                  if (yt < y1)
                     y1 = yt;
                  if (yt > y2)
                     y2 = yt;
                  yt = ca * h;
                  if (yt < y1)
                     y1 = yt;
                  if (yt > y2)
                     y2 = yt;
                  yt = sa * w + ca * h;
                  if (yt < y1)
                     y1 = yt;
                  if (yt > y2)
                     y2 = yt;
                  *height_return = (int)(y2 - y1);
               }
          }
        break;
     default:
        break;
     }
}

/**
 * @param text A string.
 * @param horizontal_advance_return Horizontal offset.
 * @param vertical_advance_return Vertical offset.
 * 
 * Gets the advance horizontally and vertically in pixels the next
 * text string would need to be placed at for the current font. The
 * advances are not adjusted for rotation so you will have to translate
 * the advances (which are calculated as if the text was drawn
 * horizontally from left to right) depending on the text orientation. 
 **/
EAPI void
imlib_get_text_advance(const char *text, int *horizontal_advance_return,
                       int *vertical_advance_return)
{
   ImlibFont          *fn;
   int                 w, h;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_get_text_advance", "font", ctx->font);
   CHECK_PARAM_POINTER("imlib_get_text_advance", "text", text);
   fn = (ImlibFont *) ctx->font;
   imlib_font_query_advance(fn, text, &w, &h);
   if (horizontal_advance_return)
      *horizontal_advance_return = w;
   if (vertical_advance_return)
      *vertical_advance_return = h;
}

/**
 * @param text A string.
 * @return The inset value of @text.
 * 
 * Returns the inset of the first character of @p text
 * in using the current font and returns that value in pixels. 
 * 
 **/
EAPI int
imlib_get_text_inset(const char *text)
{
   ImlibFont          *fn;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_get_text_advance", "font", ctx->font, 0);
   CHECK_PARAM_POINTER_RETURN("imlib_get_text_advance", "text", text, 0);
   fn = (ImlibFont *) ctx->font;
   return imlib_font_query_inset(fn, text);
}

/**
 * @param path A directory path.
 * 
 * Adds the directory @p path to the end of the current list of
 * directories to scan for fonts. 
 **/
EAPI void
imlib_add_path_to_font_path(const char *path)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_add_path_to_font_path", "path", path);
   if (!imlib_font_path_exists(path))
      imlib_font_add_font_path(path);
}

/**
 * @param path A directory path.
 * 
 * Removes all directories in the font path that match @p path.
 **/
EAPI void
imlib_remove_path_from_font_path(const char *path)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_remove_path_from_font_path", "path", path);
   imlib_font_del_font_path(path);
}

/**
 * @param number_return Number of paths in the list.
 * @return A list of strings.
 * 
 * Returns a list of strings that are the directories in the font
 * path. Do not free this list or change it in any way. If you add or
 * delete members of the font path this list will be invalid. If you
 * intend to use this list later duplicate it for your own use. The
 * number of elements in the array of strings is put into
 * @p number_return. 
 * 
 **/
EAPI char         **
imlib_list_font_path(int *number_return)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_list_font_path", "number_return",
                              number_return, NULL);
   return imlib_font_list_font_path(number_return);
}

/**
 * @param text A string.
 * @param x The x offset.
 * @param y The y offset.
 * @param char_x_return The x coordinate of the character.
 * @param char_y_return The x coordinate of the character.
 * @param char_width_return The width of the character.
 * @param char_height_return The height of the character.
 * @return -1 if no character found.
 * 
 * Returns the character number in the string @p text using the current
 * font at the (@p x, @p y) pixel location which is an offset relative to the
 * top left of that string. -1 is returned if there is no character
 * there. If there is a character, @p char_x_return, @p char_y_return,
 * @p char_width_return and @p char_height_return (respectively the
 * character x, y, width and height)  are also filled in. 
 * 
 **/
EAPI int
imlib_text_get_index_and_location(const char *text, int x, int y,
                                  int *char_x_return, int *char_y_return,
                                  int *char_width_return,
                                  int *char_height_return)
{
   ImlibFont          *fn;
   int                 w, h, cx, cy, cw, ch, cp, xx, yy;
   int                 dir;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_text_get_index_and_location", "font",
                              ctx->font, -1);
   CHECK_PARAM_POINTER_RETURN("imlib_text_get_index_and_location", "text",
                              text, -1);
   fn = (ImlibFont *) ctx->font;

   dir = ctx->direction;
   if (ctx->direction == IMLIB_TEXT_TO_ANGLE && ctx->angle == 0.0)
      dir = IMLIB_TEXT_TO_RIGHT;

   imlib_get_text_size(text, &w, &h);

   switch (dir)
     {
     case IMLIB_TEXT_TO_RIGHT:
        xx = x;
        yy = y;
        break;
     case IMLIB_TEXT_TO_LEFT:
        xx = w - x;
        yy = h - y;
        break;
     case IMLIB_TEXT_TO_DOWN:
        xx = y;
        yy = w - x;
        break;
     case IMLIB_TEXT_TO_UP:
        xx = h - y;
        yy = x;
        break;
     default:
        return -1;
     }

   cp = imlib_font_query_text_at_pos(fn, text, xx, yy, &cx, &cy, &cw, &ch);

   switch (dir)
     {
     case IMLIB_TEXT_TO_RIGHT:
        if (char_x_return)
           *char_x_return = cx;
        if (char_y_return)
           *char_y_return = cy;
        if (char_width_return)
           *char_width_return = cw;
        if (char_height_return)
           *char_height_return = ch;
        return cp;
        break;
     case IMLIB_TEXT_TO_LEFT:
        cx = 1 + w - cx - cw;
        if (char_x_return)
           *char_x_return = cx;
        if (char_y_return)
           *char_y_return = cy;
        if (char_width_return)
           *char_width_return = cw;
        if (char_height_return)
           *char_height_return = ch;
        return cp;
        break;
     case IMLIB_TEXT_TO_DOWN:
        if (char_x_return)
           *char_x_return = cy;
        if (char_y_return)
           *char_y_return = cx;
        if (char_width_return)
           *char_width_return = ch;
        if (char_height_return)
           *char_height_return = cw;
        return cp;
        break;
     case IMLIB_TEXT_TO_UP:
        cy = 1 + h - cy - ch;
        if (char_x_return)
           *char_x_return = cy;
        if (char_y_return)
           *char_y_return = cx;
        if (char_width_return)
           *char_width_return = ch;
        if (char_height_return)
           *char_height_return = cw;
        return cp;
        break;
     default:
        return -1;
        break;
     }
   return -1;
}

/**
 * @param text A string.
 * @param index The index of @text.
 * @param char_x_return The x coordinate of the character.
 * @param char_y_return The y coordinate of the character.
 * @param char_width_return The width of the character.
 * @param char_height_return The height of the character.
 * 
 * Gets the geometry of the character at index @p index in the @p text
 * string using the current font. 
 **/
EAPI void
imlib_text_get_location_at_index(const char *text, int index,
                                 int *char_x_return, int *char_y_return,
                                 int *char_width_return,
                                 int *char_height_return)
{
   ImlibFont          *fn;
   int                 cx, cy, cw, ch, w, h;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_text_get_index_and_location", "font", ctx->font);
   CHECK_PARAM_POINTER("imlib_text_get_index_and_location", "text", text);
   fn = (ImlibFont *) ctx->font;

   imlib_font_query_char_coords(fn, text, index, &cx, &cy, &cw, &ch);

   imlib_get_text_size(text, &w, &h);

   switch (ctx->direction)
     {
     case IMLIB_TEXT_TO_RIGHT:
        if (char_x_return)
           *char_x_return = cx;
        if (char_y_return)
           *char_y_return = cy;
        if (char_width_return)
           *char_width_return = cw;
        if (char_height_return)
           *char_height_return = ch;
        return;
        break;
     case IMLIB_TEXT_TO_LEFT:
        cx = 1 + w - cx - cw;
        if (char_x_return)
           *char_x_return = cx;
        if (char_y_return)
           *char_y_return = cy;
        if (char_width_return)
           *char_width_return = cw;
        if (char_height_return)
           *char_height_return = ch;
        return;
        break;
     case IMLIB_TEXT_TO_DOWN:
        if (char_x_return)
           *char_x_return = cy;
        if (char_y_return)
           *char_y_return = cx;
        if (char_width_return)
           *char_width_return = ch;
        if (char_height_return)
           *char_height_return = cw;
        return;
        break;
     case IMLIB_TEXT_TO_UP:
        cy = 1 + h - cy - ch;
        if (char_x_return)
           *char_x_return = cy;
        if (char_y_return)
           *char_y_return = cx;
        if (char_width_return)
           *char_width_return = ch;
        if (char_height_return)
           *char_height_return = cw;
        return;
        break;
     default:
        return;
        break;
     }
}

/**
 * @param number_return Number of fonts in the list.
 * @return A list of fonts.
 * 
 * Returns a list of fonts imlib2 can find in its font path.
 * 
 **/
EAPI char         **
imlib_list_fonts(int *number_return)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_list_fonts", "number_return",
                              number_return, NULL);
   return imlib_font_list_fonts(number_return);
}

/**
 * @param font_list The font list.
 * @param number Number of fonts in the list.
 * 
 * Frees the font list returned by imlib_list_fonts().
 * 
 **/
EAPI void
imlib_free_font_list(char **font_list, int number)
{
   __imlib_FileFreeDirList(font_list, number);
}

/**
 * @return The font cache size.
 *
 * Returns the font cache size in bytes.
 * 
 **/
EAPI int
imlib_get_font_cache_size(void)
{
   CHECK_CONTEXT(ctx);
   return imlib_font_cache_get();
}

/**
 * @param bytes The font cache size.
 * 
 * Sets the font cache in bytes. Whenever you set the font cache size
 * Imlib2 will flush fonts from the cache until the memory used by
 * fonts is less than or equal to the font cache size. Setting the size
 * to 0 effectively frees all speculatively cached fonts. 
 **/
EAPI void
imlib_set_font_cache_size(int bytes)
{
   CHECK_CONTEXT(ctx);
   imlib_font_cache_set(bytes);
}

/**
 * Causes a flush of all speculatively cached fonts from the font
 * cache. 
 **/
EAPI void
imlib_flush_font_cache(void)
{
   CHECK_CONTEXT(ctx);
   imlib_font_flush();
}

/**
 * @return The font's ascent.
 *
 * Returns the current font's ascent value in pixels.
 * 
 **/
EAPI int
imlib_get_font_ascent(void)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_get_font_ascent", "font", ctx->font, 0);
   return imlib_font_ascent_get(ctx->font);
}

/**
 * @return The font's descent.
 * 
 * Returns the current font's descent value in pixels.
 * 
 **/
EAPI int
imlib_get_font_descent(void)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_get_font_ascent", "font", ctx->font, 0);
   return imlib_font_descent_get(ctx->font);
}

/**
 * @return The font's maximum ascent.
 * 
 * Returns the current font's maximum ascent extent.
 * 
 **/
EAPI int
imlib_get_maximum_font_ascent(void)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_get_font_ascent", "font", ctx->font, 0);
   return imlib_font_max_ascent_get(ctx->font);
}

/**
 * @return The font's maximum descent.
 * 
 * Returns the current font's maximum descent extent.
 * 
 **/
EAPI int
imlib_get_maximum_font_descent(void)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_get_font_ascent", "font", ctx->font, 0);
   return imlib_font_max_descent_get(ctx->font);
}

/**
 * @return Valid handle.
 * 
 * Creates a new empty color modifier and returns a
 * valid handle on success. NULL is returned on failure. 
 * 
 **/
EAPI                Imlib_Color_Modifier
imlib_create_color_modifier(void)
{
   CHECK_CONTEXT(ctx);
   return (Imlib_Color_Modifier) __imlib_CreateCmod();
}

/**
 * Frees the current color modifier.
 **/
EAPI void
imlib_free_color_modifier(void)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_free_color_modifier", "color_modifier",
                       ctx->color_modifier);
   __imlib_FreeCmod((ImlibColorModifier *) ctx->color_modifier);
   ctx->color_modifier = NULL;
}

/**
 * @param gamma_value Value of gamma.
 * 
 * Modifies the current color modifier by adjusting the gamma by the
 * value specified @p gamma_value. The color modifier is modified not set, so calling
 * this repeatedly has cumulative effects. A gamma of 1.0 is normal
 * linear, 2.0 brightens and 0.5 darkens etc. Negative values are not
 * allowed. 
 **/
EAPI void
imlib_modify_color_modifier_gamma(double gamma_value)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_modify_color_modifier_gamma", "color_modifier",
                       ctx->color_modifier);
   __imlib_CmodModGamma((ImlibColorModifier *) ctx->color_modifier,
                        gamma_value);
}

/**
 * @param brightness_value Value of brightness.
 * 
 * Modifies the current color modifier by adjusting the brightness by
 * the value @p brightness_value. The color modifier is modified not set, so
 * calling this repeatedly has cumulative effects. brightness values
 * of 0 do not affect anything. -1.0 will make things completely black
 * and 1.0 will make things all white. Values in-between vary
 * brightness linearly.
 **/
EAPI void
imlib_modify_color_modifier_brightness(double brightness_value)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_modify_color_modifier_brightness",
                       "color_modifier", ctx->color_modifier);
   __imlib_CmodModBrightness((ImlibColorModifier *) ctx->color_modifier,
                             brightness_value);
}

/**
 * @param contrast_value Value of contrast.
 * 
 * Modifies the current color modifier by adjusting the contrast by
 * the value @p contrast_value. The color modifier is modified not set, so
 * calling this repeatedly has cumulative effects. Contrast of 1.0 does
 * nothing. 0.0 will merge to gray, 2.0 will double contrast etc. 
 **/
EAPI void
imlib_modify_color_modifier_contrast(double contrast_value)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_modify_color_modifier_contrast",
                       "color_modifier", ctx->color_modifier);
   __imlib_CmodModContrast((ImlibColorModifier *) ctx->color_modifier,
                           contrast_value);
}

/**
 * @param red_table An array of #DATA8.
 * @param green_table An array of #DATA8.
 * @param blue_table An array of #DATA8.
 * @param alpha_table An array of #DATA8.
 * 
 * Explicitly copies the mapping tables from the table pointers passed
 * into this function into those of the current color modifier. Tables
 * are 256 entry arrays of DATA8 which are a mapping of that channel
 * value to a new channel value. A normal mapping would be linear (v[0]
 * = 0, v[10] = 10, v[50] = 50, v[200] = 200, v[255] = 255). 
 **/
EAPI void
imlib_set_color_modifier_tables(DATA8 * red_table, DATA8 * green_table,
                                DATA8 * blue_table, DATA8 * alpha_table)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_set_color_modifier_tables", "color_modifier",
                       ctx->color_modifier);
   __imlib_CmodSetTables((ImlibColorModifier *) ctx->color_modifier,
                         red_table, green_table, blue_table, alpha_table);
}

/**
 * @param red_table: an array of #DATA8.
 * @param green_table: an array of #DATA8.
 * @param blue_table: an array of #DATA8.
 * @param alpha_table: an array of #DATA8.
 * 
 * Copies the table values from the current color modifier into the
 * pointers to mapping tables specified. They must have 256 entries and
 * be DATA8 format. 
 **/
EAPI void
imlib_get_color_modifier_tables(DATA8 * red_table, DATA8 * green_table,
                                DATA8 * blue_table, DATA8 * alpha_table)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_get_color_modifier_tables", "color_modifier",
                       ctx->color_modifier);
   __imlib_CmodGetTables((ImlibColorModifier *) ctx->color_modifier,
                         red_table, green_table, blue_table, alpha_table);
}

/**
 * Resets the current color modifier to have linear mapping tables.
 **/
EAPI void
imlib_reset_color_modifier(void)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_rset_color_modifier", "color_modifier",
                       ctx->color_modifier);
   __imlib_CmodReset((ImlibColorModifier *) ctx->color_modifier);
}

/**
 * Uses the current color modifier and modifies the current image using
 * the mapping tables in the current color modifier. 
 **/
EAPI void
imlib_apply_color_modifier(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_apply_color_modifier", "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_apply_color_modifier", "color_modifier",
                       ctx->color_modifier);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_DataCmodApply(im->data, im->w, im->h, 0, &(im->flags),
                         (ImlibColorModifier *) ctx->color_modifier);
}

/**
 * @param x The x coordinate of the left edge of the rectangle.
 * @param y  The y coordinate of the top edge of the rectangle.
 * @param width  The width of the rectangle.
 * @param height  The height of the rectangle.
 * 
 * Works the same way as imlib_apply_color_modifier() but only modifies
 * a selected rectangle in the current image. 
 **/
EAPI void
imlib_apply_color_modifier_to_rectangle(int x, int y, int width, int height)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_apply_color_modifier_to_rectangle", "image",
                       ctx->image);
   CHECK_PARAM_POINTER("imlib_apply_color_modifier_to_rectangle",
                       "color_modifier", ctx->color_modifier);
   CAST_IMAGE(im, ctx->image);
   if (x < 0)
     {
        width += x;
        x = 0;
     }
   if (width <= 0)
      return;
   if ((x + width) > im->w)
      width = (im->w - x);
   if (width <= 0)
      return;
   if (y < 0)
     {
        height += y;
        y = 0;
     }
   if (height <= 0)
      return;
   if ((y + height) > im->h)
      height = (im->h - y);
   if (height <= 0)
      return;
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_DataCmodApply(im->data + (y * im->w) + x, width, height,
                         im->w - width, &(im->flags),
                         (ImlibColorModifier *) ctx->color_modifier);
}

EAPI                Imlib_Updates
imlib_image_draw_pixel(int x, int y, char make_updates)
{
   ImlibImage         *im;
   DATA32              color;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_draw_pixel", "image", ctx->image,
                              NULL);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return NULL;
   __imlib_DirtyImage(im);
   A_VAL(&color) = (DATA8) ctx->color.alpha;
   R_VAL(&color) = (DATA8) ctx->color.red;
   G_VAL(&color) = (DATA8) ctx->color.green;
   B_VAL(&color) = (DATA8) ctx->color.blue;
   return (Imlib_Updates) __imlib_Point_DrawToImage(x, y, color, im,
                                                    ctx->cliprect.x,
                                                    ctx->cliprect.y,
                                                    ctx->cliprect.w,
                                                    ctx->cliprect.h,
                                                    ctx->operation, ctx->blend,
                                                    make_updates);
}

/**
 * @param x1 The x coordinate of the first point.
 * @param y1 The y coordinate of the first point.
 * @param x2 The x coordinate of the second point.
 * @param y2 The y coordinate of the second point.
 * @param make_updates: a char.
 * @return An updates list.
 * 
 * Draws a line using the current color on the current image from
 * coordinates (@p x1, @p y1) to (@p x2, @p y2). If @p make_updates is 1 it will also
 * return an update you can use for an updates list, otherwise it
 * returns NULL. 
 * 
 **/
EAPI                Imlib_Updates
imlib_image_draw_line(int x1, int y1, int x2, int y2, char make_updates)
{
   ImlibImage         *im;
   DATA32              color;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_draw_line", "image", ctx->image,
                              NULL);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return NULL;
   __imlib_DirtyImage(im);
   A_VAL(&color) = (DATA8) ctx->color.alpha;
   R_VAL(&color) = (DATA8) ctx->color.red;
   G_VAL(&color) = (DATA8) ctx->color.green;
   B_VAL(&color) = (DATA8) ctx->color.blue;
   return (Imlib_Updates) __imlib_Line_DrawToImage(x1, y1, x2, y2, color, im,
                                                   ctx->cliprect.x,
                                                   ctx->cliprect.y,
                                                   ctx->cliprect.w,
                                                   ctx->cliprect.h,
                                                   ctx->operation, ctx->blend,
                                                   ctx->anti_alias,
                                                   make_updates);
}

/**
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * 
 * Draws the outline of a rectangle on the current image at the (@p x,
 * @p y)
 * coordinates with a size of @p width and @p height pixels, using the
 * current color. 
 **/
EAPI void
imlib_image_draw_rectangle(int x, int y, int width, int height)
{
   ImlibImage         *im;
   DATA32              color;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_draw_rectangle", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   A_VAL(&color) = (DATA8) ctx->color.alpha;
   R_VAL(&color) = (DATA8) ctx->color.red;
   G_VAL(&color) = (DATA8) ctx->color.green;
   B_VAL(&color) = (DATA8) ctx->color.blue;
   __imlib_Rectangle_DrawToImage(x, y, width, height, color,
                                 im, ctx->cliprect.x, ctx->cliprect.y,
                                 ctx->cliprect.w, ctx->cliprect.h,
                                 ctx->operation, ctx->blend);
}

/**
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * 
 * Draws a filled rectangle on the current image at the (@p x, @p y)
 * coordinates with a size of @p width and @p height pixels, using the
 * current color. 
 **/
EAPI void
imlib_image_fill_rectangle(int x, int y, int width, int height)
{
   ImlibImage         *im;
   DATA32              color;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_fill_rectangle", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   A_VAL(&color) = (DATA8) ctx->color.alpha;
   R_VAL(&color) = (DATA8) ctx->color.red;
   G_VAL(&color) = (DATA8) ctx->color.green;
   B_VAL(&color) = (DATA8) ctx->color.blue;
   __imlib_Rectangle_FillToImage(x, y, width, height, color,
                                 im, ctx->cliprect.x, ctx->cliprect.y,
                                 ctx->cliprect.w, ctx->cliprect.h,
                                 ctx->operation, ctx->blend);
}

/**
 * @param image_source An image.
 * @param x The x coordinate.
 * @param y The y coordinate.
 * 
 * Copies the alpha channel of the source image @p image_source to the
 * (@p x, @p y) coordinates
 * of the current image, replacing the alpha channel there. 
 **/
EAPI void
imlib_image_copy_alpha_to_image(Imlib_Image image_source, int x, int y)
{
   ImlibImage         *im, *im2;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_copy_alpha_to_image", "image_source",
                       image_source);
   CHECK_PARAM_POINTER("imlib_image_copy_alpha_to_image", "image_destination",
                       ctx->image);
   CAST_IMAGE(im, image_source);
   CAST_IMAGE(im2, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if ((!(im2->data)) && (im2->loader) && (im2->loader->load))
      im2->loader->load(im2, NULL, 0, 1);
   if (!(im->data))
      return;
   if (!(im2->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_copy_alpha_data(im, im2, 0, 0, im->w, im->h, x, y);
}

/**
 * @param image_source An image.
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * @param destination_x The top left x coordinate of the destination rectangle.
 * @param destination_y The top left x coordinate of the destination rectangle.
 * 
 * Copies the source (@p x, @p y, @p width, @p height) rectangle alpha channel from
 * the source image @p image_source and replaces the alpha channel on the destination
 * image at the (@p destination_x, @p destination_y) coordinates. 
 **/
EAPI void
imlib_image_copy_alpha_rectangle_to_image(Imlib_Image image_source, int x,
                                          int y, int width, int height,
                                          int destination_x, int destination_y)
{
   ImlibImage         *im, *im2;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_copy_alpha_rectangle_to_image",
                       "image_source", image_source);
   CHECK_PARAM_POINTER("imlib_image_copy_alpha_rectangle_to_image",
                       "image_destination", ctx->image);
   CAST_IMAGE(im, image_source);
   CAST_IMAGE(im2, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if ((!(im2->data)) && (im2->loader) && (im2->loader->load))
      im2->loader->load(im2, NULL, 0, 1);
   if (!(im->data))
      return;
   if (!(im2->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_copy_alpha_data(im, im2, x, y, width, height, destination_x,
                           destination_y);
}

/**
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * @param delta_x Distance along the x coordinates.
 * @param delta_y Distance along the y coordinates.
 * 
 * Scrolls a rectangle of size @p width, @p height at the (@p x, @p y)
 * location within the current image
 * by the @p delta_x, @p delta_y distance (in pixels). 
 **/
EAPI void
imlib_image_scroll_rect(int x, int y, int width, int height, int delta_x,
                        int delta_y)
{
   ImlibImage         *im;
   int                 xx, yy, w, h, nx, ny;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_scroll_rect", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   if (delta_x > 0)
     {
        xx = x;
        nx = x + delta_x;
        w = width - delta_x;
     }
   else
     {
        xx = x - delta_x;
        nx = x;
        w = width + delta_x;
     }
   if (delta_y > 0)
     {
        yy = y;
        ny = y + delta_y;
        h = height - delta_y;
     }
   else
     {
        yy = y - delta_y;
        ny = y;
        h = height + delta_y;
     }
   __imlib_DirtyImage(im);
   __imlib_copy_image_data(im, xx, yy, w, h, nx, ny);
}

/**
 * @param x The top left x coordinate of the rectangle.
 * @param y The top left y coordinate of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * @param new_x The top left x coordinate of the new location.
 * @param new_y The top left y coordinate of the new location.
 * 
 * Copies a rectangle of size @p width, @p height at the (@p x, @p y) location
 * specified in the current image to a new location (@p new_x, @p new_y) in the same
 * image. 
 **/
EAPI void
imlib_image_copy_rect(int x, int y, int width, int height, int new_x, int new_y)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_copy_rect", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_copy_image_data(im, x, y, width, height, new_x, new_y);
}

/**
 * @return valid handle.
 *
 * Creates a new empty color range and returns a valid handle to that
 * color range.
 **/
EAPI                Imlib_Color_Range
imlib_create_color_range(void)
{
   CHECK_CONTEXT(ctx);
   return (Imlib_Color_Range) __imlib_CreateRange();
}

/**
 * Frees the current color range.
 **/
EAPI void
imlib_free_color_range(void)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_free_color_range", "color_range",
                       ctx->color_range);
   __imlib_FreeRange((ImlibRange *) ctx->color_range);
   ctx->color_range = NULL;
}

/**
 * @param distance_away Distance from the previous color.
 * 
 * Adds the current color to the current color range at a @p distance_away
 * distance from the previous color in the range (if it's the first
 * color in the range this is irrelevant). 
 **/
EAPI void
imlib_add_color_to_color_range(int distance_away)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_add_color_to_color_range", "color_range",
                       ctx->color_range);
   __imlib_AddRangeColor((ImlibRange *) ctx->color_range, ctx->color.red,
                         ctx->color.green, ctx->color.blue, ctx->color.alpha,
                         distance_away);
}

/**
 * @param x The x coordinate of the left edge of the rectangle.
 * @param y The y coordinate of the top edge of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * @param angle Angle of gradient.
 * 
 * Fills a rectangle of width @p width and height @p height at the (@p x, @p y) location
 * specified in the current image with a linear gradient of the
 * current color range at an angle of @p angle degrees with 0 degrees
 * being vertical from top to bottom going clockwise from there. 
 **/
EAPI void
imlib_image_fill_color_range_rectangle(int x, int y, int width, int height,
                                       double angle)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_fill_color_range_rectangle", "image",
                       ctx->image);
   CHECK_PARAM_POINTER("imlib_image_fill_color_range_rectangle",
                       "color_range", ctx->color_range);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_DrawGradient(im, x, y, width, height,
                        (ImlibRange *) ctx->color_range, angle,
                        ctx->operation,
                        ctx->cliprect.x, ctx->cliprect.y,
                        ctx->cliprect.w, ctx->cliprect.h);
}

/**
 * @param x The x coordinate of the left edge of the rectangle.
 * @param y The y coordinate of the top edge of the rectangle.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 * @param angle Angle of gradient.
 * 
 * Fills a rectangle of width @p width and height @p height at the (@p
 * x, @p y) location
 * specified in the current image with a linear gradient in HSVA color
 * space of the current color range at an angle of @p angle degrees with
 * 0 degrees being vertical from top to bottom going clockwise from
 * there. 
 **/
EAPI void
imlib_image_fill_hsva_color_range_rectangle(int x, int y, int width, int height,
                                            double angle)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_fill_color_range_rectangle", "image",
                       ctx->image);
   CHECK_PARAM_POINTER("imlib_image_fill_color_range_rectangle",
                       "color_range", ctx->color_range);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_DrawHsvaGradient(im, x, y, width, height,
                            (ImlibRange *) ctx->color_range, angle,
                            ctx->operation,
                            ctx->cliprect.x, ctx->cliprect.y,
                            ctx->cliprect.w, ctx->cliprect.h);
}

/**
 * @param x The x coordinate of the pixel.
 * @param y The y coordinate of the pixel.
 * @param color_return The returned color.
 * 
 * Fills the @p color_return color structure with the color of the pixel
 * in the current image that is at the (@p x, @p y) location specified. 
 **/
EAPI void
imlib_image_query_pixel(int x, int y, Imlib_Color * color_return)
{
   ImlibImage         *im;
   DATA32             *p;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_query_pixel", "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_image_query_pixel", "color_return", color_return);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   if ((x < 0) || (x >= im->w) || (y < 0) || (y >= im->h))
     {
        color_return->red = 0;
        color_return->green = 0;
        color_return->blue = 0;
        color_return->alpha = 0;
        return;
     }
   p = im->data + (im->w * y) + x;
   color_return->red = ((*p) >> 16) & 0xff;
   color_return->green = ((*p) >> 8) & 0xff;
   color_return->blue = (*p) & 0xff;
   color_return->alpha = ((*p) >> 24) & 0xff;
}

/**
 * @param x The x coordinate of the pixel.
 * @param y The y coordinate of the pixel.
 * @param hue The returned hue channel.
 * @param saturation The returned saturation channel.
 * @param value The returned value channel.
 * @param alpha The returned alpha channel.
 * 
 * Gets the HSVA color of the pixel from the current image that is at
 * the (@p x, @p y) location specified. 
 **/
EAPI void
imlib_image_query_pixel_hsva(int x, int y, float *hue, float *saturation,
                             float *value, int *alpha)
{
   ImlibImage         *im;
   DATA32             *p;
   int                 r, g, b;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_query_pixel", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   if ((x < 0) || (x >= im->w) || (y < 0) || (y >= im->h))
     {
        *hue = 0;
        *saturation = 0;
        *value = 0;
        *alpha = 0;
        return;
     }
   p = im->data + (im->w * y) + x;
   r = ((*p) >> 16) & 0xff;
   g = ((*p) >> 8) & 0xff;
   b = (*p) & 0xff;
   *alpha = ((*p) >> 24) & 0xff;

   __imlib_rgb_to_hsv(r, g, b, hue, saturation, value);
}

/**
 * @param x The x coordinate of the pixel.
 * @param y The y coordinate of the pixel.
 * @param hue The returned hue channel.
 * @param lightness The returned lightness channel.
 * @param saturation The returned saturation channel.
 * @param alpha The returned alpha channel.
 * 
 * Gets the HLSA color of the pixel from the current image that is at
 * the (@p x, @p y) location specified. 
 **/
EAPI void
imlib_image_query_pixel_hlsa(int x, int y, float *hue, float *lightness,
                             float *saturation, int *alpha)
{
   ImlibImage         *im;
   DATA32             *p;
   int                 r, g, b;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_query_pixel", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   if ((x < 0) || (x >= im->w) || (y < 0) || (y >= im->h))
     {
        *hue = 0;
        *lightness = 0;
        *saturation = 0;
        *alpha = 0;
        return;
     }
   p = im->data + (im->w * y) + x;
   r = ((*p) >> 16) & 0xff;
   g = ((*p) >> 8) & 0xff;
   b = (*p) & 0xff;
   *alpha = ((*p) >> 24) & 0xff;

   __imlib_rgb_to_hls(r, g, b, hue, lightness, saturation);
}

/**
 * @param x Tthe x coordinate of the pixel.
 * @param y The y coordinate of the pixel.
 * @param cyan The returned cyan channel.
 * @param magenta The returned magenta channel.
 * @param yellow The returned yellow channel.
 * @param alpha The returned alpha channel.
 * 
 * Gets the CMYA color of the pixel from the current image that is at
 * the (@p x, @p y) location specified. 
 *
 **/
EAPI void
imlib_image_query_pixel_cmya(int x, int y, int *cyan, int *magenta, int *yellow,
                             int *alpha)
{
   ImlibImage         *im;
   DATA32             *p;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_query_pixel", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   if ((x < 0) || (x >= im->w) || (y < 0) || (y >= im->h))
     {
        *cyan = 0;
        *magenta = 0;
        *yellow = 0;
        *alpha = 0;
        return;
     }
   p = im->data + (im->w * y) + x;
   *cyan = 255 - (((*p) >> 16) & 0xff);
   *magenta = 255 - (((*p) >> 8) & 0xff);
   *yellow = 255 - ((*p) & 0xff);
   *alpha = ((*p) >> 24) & 0xff;
}

/**
 * @param key A string.
 * @param data A pointer.
 * @param value A value.
 * @param destructor_function An Imlib internal function.
 * 
 * Attaches data to the current image with the string key of @p key, and
 * the data of @p data and an integer of @p value. The
 * @p destructor_function function, if not NULL is called when this
 * image is freed so the destructor can free @p data, if this is needed. 
 **/
EAPI void
imlib_image_attach_data_value(const char *key, void *data, int value,
                              Imlib_Internal_Data_Destructor_Function
                              destructor_function)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_attach_data_value", "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_image_attach_data_value", "key", key);
   CAST_IMAGE(im, ctx->image);
   __imlib_AttachTag(im, key, value, data,
                     (ImlibDataDestructorFunction) destructor_function);
}

/**
 * @param key A string.
 * @return The attached data as a pointer, or NULL if none.
 * 
 * Gets the data attached to the current image with the key @p key
 * specified. NULL is returned if no data could be found with that key
 * on the current image. 
 * 
 **/
EAPI void          *
imlib_image_get_attached_data(const char *key)
{
   ImlibImageTag      *t;
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_get_attached_data", "image",
                              ctx->image, NULL);
   CHECK_PARAM_POINTER_RETURN("imlib_image_get_attached_data", "key", key,
                              NULL);
   CAST_IMAGE(im, ctx->image);
   t = __imlib_GetTag(im, key);
   if (t)
      return t->data;
   return NULL;
}

/**
 * @param key A string.
 * @return The attached value as an integer, or 0 if none.
 * 
 * Returns the value attached to the current image with the specified
 * key @p key. If none could be found 0 is returned. 
 * 
 **/
EAPI int
imlib_image_get_attached_value(const char *key)
{
   ImlibImageTag      *t;
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_image_get_attached_value", "image",
                              ctx->image, 0);
   CHECK_PARAM_POINTER_RETURN("imlib_image_get_attached_value", "key", key, 0);
   CAST_IMAGE(im, ctx->image);
   t = __imlib_GetTag(im, key);
   if (t)
      return t->val;
   return 0;
}

/**
 * @param key A string.
 * 
 * Detaches the data & value attached with the specified key @p key from the
 * current image. 
 **/
EAPI void
imlib_image_remove_attached_data_value(const char *key)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_remove_attached_data_value", "image",
                       ctx->image);
   CHECK_PARAM_POINTER("imlib_image_remove_attached_data_value", "key", key);
   CAST_IMAGE(im, ctx->image);
   __imlib_RemoveTag(im, key);
}

/**
 * @param key A string.
 * 
 * Removes the data and value attached to the current image with the
 * specified key @p key and also calls the destructor function that was
 * supplied when attaching it (see imlib_image_attach_data_value()).
 **/
EAPI void
imlib_image_remove_and_free_attached_data_value(const char *key)
{
   ImlibImageTag      *t;
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_remove_and_free_attached_data_value",
                       "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_image_remove_and_free_attached_data_value",
                       "key", key);
   CAST_IMAGE(im, ctx->image);
   t = __imlib_RemoveTag(im, key);
   __imlib_FreeTag(im, t);
}

/**
 * @param filename The file name.
 * 
 * Saves the current image in the format specified by the current
 * image's format settings to the filename @p filename. 
 **/
EAPI void
imlib_save_image(const char *filename)
{
   ImlibImage         *im;
   Imlib_Image         prev_ctxt_image;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_save_image", "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_save_image", "filename", filename);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!im->data)
      return;
   prev_ctxt_image = ctx->image;
   __imlib_SaveImage(im, filename, (ImlibProgressFunction) ctx->progress_func,
                     ctx->progress_granularity, NULL);
   ctx->image = prev_ctxt_image;
}

/**
 * @param filename The file name.
 * @param error_return The returned error.
 * 
 * Works the same way imlib_save_image() works, but will set the
 * @p error_return to an error value if the save fails. 
 **/
EAPI void
imlib_save_image_with_error_return(const char *filename,
                                   Imlib_Load_Error * error_return)
{
   ImlibImage         *im;
   Imlib_Image         prev_ctxt_image;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_save_image_with_error_return", "image",
                       ctx->image);
   CHECK_PARAM_POINTER("imlib_save_image_with_error_return", "filename",
                       filename);
   CHECK_PARAM_POINTER("imlib_save_image_with_error_return", "error_return",
                       error_return);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!im->data)
      return;
   prev_ctxt_image = ctx->image;
   __imlib_SaveImage(im, filename, (ImlibProgressFunction) ctx->progress_func,
                     ctx->progress_granularity, error_return);
   ctx->image = prev_ctxt_image;
}

/**
 * @param angle An angle in radians.
 * @return A new image, or NULL.
 * 
 * Creates an new copy of the current image, but rotated by @p angle
 * radians. On success it returns a valid image handle, otherwise
 * NULL. 
 **/
EAPI                Imlib_Image
imlib_create_rotated_image(double angle)
{
   ImlibImage         *im, *im_old;
   int                 x, y, dx, dy, sz;
   double              x1, y1, d;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_create_rotated_image", "image",
                              ctx->image, NULL);
   CAST_IMAGE(im_old, ctx->image);
   if ((!(im_old->data)) && (im_old->loader) && (im_old->loader->load))
      im_old->loader->load(im_old, NULL, 0, 1);
   if (!(im_old->data))
      return NULL;

   d = hypot((double)(im_old->w + 4), (double)(im_old->h + 4)) / sqrt(2.0);

   x1 = (double)(im_old->w) / 2.0 - sin(angle + atan(1.0)) * d;
   y1 = (double)(im_old->h) / 2.0 - cos(angle + atan(1.0)) * d;

   sz = (int)(d * sqrt(2.0));
   x = (int)(x1 * _ROTATE_PREC_MAX);
   y = (int)(y1 * _ROTATE_PREC_MAX);
   dx = (int)(cos(angle) * _ROTATE_PREC_MAX);
   dy = -(int)(sin(angle) * _ROTATE_PREC_MAX);

   im = __imlib_CreateImage(sz, sz, NULL);
   im->data = calloc(sz * sz, sizeof(DATA32));
   if (!(im->data))
     {
        __imlib_FreeImage(im);
        return NULL;
     }

   if (ctx->anti_alias)
     {
#ifdef DO_MMX_ASM
        if (__imlib_get_cpuid() & CPUID_MMX)
           __imlib_mmx_RotateAA(im_old->data, im->data, im_old->w, im_old->w,
                                im_old->h, im->w, sz, sz, x, y, dx, dy, -dy,
                                dx);
        else
#endif
           __imlib_RotateAA(im_old->data, im->data, im_old->w, im_old->w,
                            im_old->h, im->w, sz, sz, x, y, dx, dy, -dy, dx);
     }
   else
     {
        __imlib_RotateSample(im_old->data, im->data, im_old->w, im_old->w,
                             im_old->h, im->w, sz, sz, x, y, dx, dy, -dy, dx);
     }
   SET_FLAG(im->flags, F_HAS_ALPHA);

   return (Imlib_Image) im;
}

void
imlib_rotate_image_from_buffer(double angle, Imlib_Image source_image)
{
   ImlibImage         *im, *im_old;
   int                 x, y, dx, dy, sz;
   double              x1, y1, d;

   CHECK_CONTEXT(ctx);
   // source image (to rotate)
   CHECK_PARAM_POINTER("imlib_rotate_image_from_buffer", "source_image",
                       source_image);
   CAST_IMAGE(im_old, source_image);

   // current context image
   CHECK_PARAM_POINTER("imlib_rotate_image_from_buffer", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);

   if ((!(im_old->data)) && (im_old->loader) && (im_old->loader->load))
      im_old->loader->load(im_old, NULL, 0, 1);

   if (!(im_old->data))
      return;

   d = hypot((double)(im_old->w + 4), (double)(im_old->h + 4)) / sqrt(2.0);

   x1 = (double)(im_old->w) / 2.0 - sin(angle + atan(1.0)) * d;
   y1 = (double)(im_old->h) / 2.0 - cos(angle + atan(1.0)) * d;

   sz = (int)(d * sqrt(2.0));
   x = (int)(x1 * _ROTATE_PREC_MAX);
   y = (int)(y1 * _ROTATE_PREC_MAX);
   dx = (int)(cos(angle) * _ROTATE_PREC_MAX);
   dy = -(int)(sin(angle) * _ROTATE_PREC_MAX);

   if ((im->w != im->h) || ((im->w < sz) && (im->h < sz)))
      return;                   // If size is wrong
   else
      sz = im->w;               // update sz with real width

#if 0                           /* Not neccesary 'cause destination is context */
   im = __imlib_CreateImage(sz, sz, NULL);
   im->data = calloc(sz * sz, sizeof(DATA32));
   if (!(im->data))
     {
        __imlib_FreeImage(im);
        return;
     }
#endif

   if (ctx->anti_alias)
     {
#ifdef DO_MMX_ASM
        if (__imlib_get_cpuid() & CPUID_MMX)
           __imlib_mmx_RotateAA(im_old->data, im->data, im_old->w, im_old->w,
                                im_old->h, im->w, sz, sz, x, y, dx, dy, -dy,
                                dx);
        else
#endif
           __imlib_RotateAA(im_old->data, im->data, im_old->w, im_old->w,
                            im_old->h, im->w, sz, sz, x, y, dx, dy, -dy, dx);
     }
   else
     {
        __imlib_RotateSample(im_old->data, im->data, im_old->w, im_old->w,
                             im_old->h, im->w, sz, sz, x, y, dx, dy, -dy, dx);
     }
   SET_FLAG(im->flags, F_HAS_ALPHA);

   return;
}

/**
 * @param source_image The image source.
 * @param merge_alpha A char.
 * @param source_x The source x coordinate.
 * @param source_y The source y coordinate.
 * @param source_width The source width.
 * @param source_height The source height.
 * @param destination_x The destination x coordinate.
 * @param destination_y The destination y coordinate.
 * @param angle_x An angle.
 * @param angle_y An angle.
 * 
 * Works just like imlib_blend_image_onto_image_skewed() except you
 * cannot skew an image (@p v_angle_x and @p v_angle_y are 0). 
 **/
EAPI void
imlib_blend_image_onto_image_at_angle(Imlib_Image source_image,
                                      char merge_alpha, int source_x,
                                      int source_y, int source_width,
                                      int source_height, int destination_x,
                                      int destination_y, int angle_x,
                                      int angle_y)
{
   ImlibImage         *im_src, *im_dst;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_blend_image_onto_image_at_angle",
                       "source_image", source_image);
   CHECK_PARAM_POINTER("imlib_blend_image_onto_image_at_angle", "image",
                       ctx->image);
   CAST_IMAGE(im_src, source_image);
   CAST_IMAGE(im_dst, ctx->image);
   if ((!(im_src->data)) && (im_src->loader) && (im_src->loader->load))
      im_src->loader->load(im_src, NULL, 0, 1);
   if (!(im_src->data))
      return;
   if ((!(im_dst->data)) && (im_dst->loader) && (im_dst->loader->load))
      im_dst->loader->load(im_dst, NULL, 0, 1);
   if (!(im_dst->data))
      return;
   __imlib_DirtyImage(im_dst);
   __imlib_BlendImageToImageSkewed(im_src, im_dst, ctx->anti_alias,
                                   ctx->blend, merge_alpha, source_x,
                                   source_y, source_width, source_height,
                                   destination_x, destination_y, angle_x,
                                   angle_y, 0, 0, ctx->color_modifier,
                                   ctx->operation,
                                   ctx->cliprect.x, ctx->cliprect.y,
                                   ctx->cliprect.w, ctx->cliprect.h);
}

/**
 * @param source_image The source image.
 * @param merge_alpha A char
 * @param source_x The source x coordinate.
 * @param source_y The source y coordinate.
 * @param source_width The source width.
 * @param source_height The source height.
 * @param destination_x The destination x coordinate.
 * @param destination_y The destination y coordinate.
 * @param h_angle_x An angle.
 * @param h_angle_y An angle.
 * @param v_angle_x An angle.
 * @param v_angle_y An angle.
 * 
 * Blends the source rectangle (@p source_x, @p source_y, @p source_width,
 * @p source_height) from the 
 * @p source_image onto the current image at the destination
 * (@p destination_x, @p destination_y) 
 * location. It will be rotated and scaled so that the upper right
 * corner will be positioned @p h_angle_x pixels to the right (or left,
 * if negative) and @p h_angle_y pixels down (from (@p destination_x,
 * @p destination_y). If 
 * @p v_angle_x and @p v_angle_y are not 0, the image will also be skewed so
 * that the lower left corner will be positioned @p v_angle_x pixels to
 * the right and @p v_angle_y pixels down. The at_angle versions simply
 * have the @p v_angle_x and @p v_angle_y set to 0 so the rotation doesn't
 * get skewed, and the render_..._on_drawable ones seem obvious
 * enough; they do the same on a drawable.
 *
 * Example:
 * @code
 * imlib_blend_image_onto_image_skewed(..., 0, 0, 100, 0, 0, 100);
 * @endcode
 * will simply scale the image to be 100x100. 
 * @code
 * imlib_blend_image_onto_image_skewed(..., 0, 0, 0, 100, 100, 0);
 * @endcode
 * will scale the image to be 100x100, and flip it diagonally. 
 * @code
 * imlib_blend_image_onto_image_skewed(..., 100, 0, 0, 100, -100, 0);
 * @endcode
 * will scale the image and rotate it 90 degrees clockwise.
 * @code
 * imlib_blend_image_onto_image_skewed(..., 50, 0, 50, 50, -50, 50);
 * @endcode
 * will rotate the image 45 degrees clockwise, and will scale it so
 * its corners are at (50,0)-(100,50)-(50,100)-(0,50) i.e. it fits
 * into the 100x100 square, so it's scaled down to 70.7% (sqrt(2)/2).
 * @code
 * imlib_blend_image_onto_image_skewed(..., 50, 50, 100 * cos(a), 100 * sin(a), 0);
 * @endcode
 * will rotate the image `a' degrees, with its upper left corner at (50,50).
 **/
EAPI void
imlib_blend_image_onto_image_skewed(Imlib_Image source_image,
                                    char merge_alpha, int source_x,
                                    int source_y, int source_width,
                                    int source_height, int destination_x,
                                    int destination_y, int h_angle_x,
                                    int h_angle_y, int v_angle_x, int v_angle_y)
{
   ImlibImage         *im_src, *im_dst;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_blend_image_onto_image_skewed", "source_image",
                       source_image);
   CHECK_PARAM_POINTER("imlib_blend_image_onto_image_skewed", "image",
                       ctx->image);
   CAST_IMAGE(im_src, source_image);
   CAST_IMAGE(im_dst, ctx->image);
   if ((!(im_src->data)) && (im_src->loader) && (im_src->loader->load))
      im_src->loader->load(im_src, NULL, 0, 1);
   if (!(im_src->data))
      return;
   if ((!(im_dst->data)) && (im_dst->loader) && (im_dst->loader->load))
      im_dst->loader->load(im_dst, NULL, 0, 1);
   if (!(im_dst->data))
      return;
   __imlib_DirtyImage(im_dst);
   __imlib_BlendImageToImageSkewed(im_src, im_dst, ctx->anti_alias,
                                   ctx->blend, merge_alpha, source_x,
                                   source_y, source_width, source_height,
                                   destination_x, destination_y, h_angle_x,
                                   h_angle_y, v_angle_x, v_angle_y,
                                   ctx->color_modifier, ctx->operation,
                                   ctx->cliprect.x, ctx->cliprect.y,
                                   ctx->cliprect.w, ctx->cliprect.h);
}

#ifdef BUILD_X11
/**
 * @param source_x The source x coordinate.
 * @param source_y The source y coordinate.
 * @param source_width The source width.
 * @param source_height The source height.
 * @param destination_x The destination x coordinate.
 * @param destination_y The destination y coordinate.
 * @param h_angle_x An angle.
 * @param h_angle_y An angle.
 * @param v_angle_x An angle.
 * @param v_angle_y An angle.
 * 
 * 
 * Works just like imlib_blend_image_onto_image_skewed(), except it
 * blends the image onto the current drawable instead of the current
 * image. 
 **/
EAPI void
imlib_render_image_on_drawable_skewed(int source_x, int source_y,
                                      int source_width, int source_height,
                                      int destination_x, int destination_y,
                                      int h_angle_x, int h_angle_y,
                                      int v_angle_x, int v_angle_y)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_render_image_on_drawable_skewed", "image",
                       ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   CAST_IMAGE(im, ctx->image);
   __imlib_RenderImageSkewed(ctx->display, im, ctx->drawable, ctx->mask,
                             ctx->visual, ctx->colormap, ctx->depth, source_x,
                             source_y, source_width, source_height,
                             destination_x, destination_y, h_angle_x,
                             h_angle_y, v_angle_x, v_angle_y, ctx->anti_alias,
                             ctx->dither, ctx->blend, ctx->dither_mask,
                             ctx->mask_alpha_threshold, ctx->color_modifier,
                             ctx->operation);
}

/**
 * @param source_x The source x coordinate.
 * @param source_y The source y coordinate.
 * @param source_width The source width.
 * @param source_height The source height.
 * @param destination_x The destination x coordinate.
 * @param destination_y The destination y coordinate.
 * @param angle_x An angle.
 * @param angle_y An angle.
 * 
 * 
 * Works just like imlib_render_image_on_drawable_skewed() except you
 * cannot skew an image (@p v_angle_x and @p v_angle_y are 0). 
 **/
EAPI void
imlib_render_image_on_drawable_at_angle(int source_x, int source_y,
                                        int source_width, int source_height,
                                        int destination_x, int destination_y,
                                        int angle_x, int angle_y)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_render_image_on_drawable_at_angle", "image",
                       ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   CAST_IMAGE(im, ctx->image);
   __imlib_RenderImageSkewed(ctx->display, im, ctx->drawable, ctx->mask,
                             ctx->visual, ctx->colormap, ctx->depth, source_x,
                             source_y, source_width, source_height,
                             destination_x, destination_y, angle_x, angle_y,
                             0, 0, ctx->anti_alias, ctx->dither, ctx->blend,
                             ctx->dither_mask, ctx->mask_alpha_threshold,
                             ctx->color_modifier, ctx->operation);
}
#endif

EAPI void
imlib_image_filter(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_filter", "image", ctx->image);
   CHECK_PARAM_POINTER("imlib_image_filter", "filter", ctx->filter);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   __imlib_FilterImage(im, (ImlibFilter *) ctx->filter);
}

EAPI                Imlib_Filter
imlib_create_filter(int initsize)
{
   CHECK_CONTEXT(ctx);
   return (Imlib_Filter) __imlib_CreateFilter(initsize);
}

EAPI void
imlib_free_filter(void)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_free_filter", "filter", ctx->filter);
   __imlib_FreeFilter((ImlibFilter *) ctx->filter);
   ctx->filter = NULL;
}

/** 
 * @param filter Current filter.
 *
 * Sets the current filter to be used when applying filters to
 * images. Set this to NULL to disable filters. 
 */
EAPI void
imlib_context_set_filter(Imlib_Filter filter)
{
   CHECK_CONTEXT(ctx);
   ctx->filter = filter;
}

/** 
 * @return
 *
 * Gets the current context image filter.
 */
EAPI                Imlib_Filter
imlib_context_get_filter(void)
{
   CHECK_CONTEXT(ctx);
   return ctx->filter;
}

EAPI void
imlib_filter_set(int xoff, int yoff, int a, int r, int g, int b)
{
   ImlibFilter        *fil;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_filter_set", "filter", ctx->filter);
   fil = (ImlibFilter *) ctx->filter;
   __imlib_FilterSetColor(&fil->alpha, xoff, yoff, a, 0, 0, 0);
   __imlib_FilterSetColor(&fil->red, xoff, yoff, 0, r, 0, 0);
   __imlib_FilterSetColor(&fil->green, xoff, yoff, 0, 0, g, 0);
   __imlib_FilterSetColor(&fil->blue, xoff, yoff, 0, 0, 0, b);
}

EAPI void
imlib_filter_set_alpha(int xoff, int yoff, int a, int r, int g, int b)
{
   ImlibFilter        *fil;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_filter_set_alpha", "filter", ctx->filter);
   fil = (ImlibFilter *) ctx->filter;
   __imlib_FilterSetColor(&fil->alpha, xoff, yoff, a, r, g, b);
}

EAPI void
imlib_filter_set_red(int xoff, int yoff, int a, int r, int g, int b)
{
   ImlibFilter        *fil;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_filter_set_red", "filter", ctx->filter);
   fil = (ImlibFilter *) ctx->filter;
   __imlib_FilterSetColor(&fil->red, xoff, yoff, a, r, g, b);
}

EAPI void
imlib_filter_set_green(int xoff, int yoff, int a, int r, int g, int b)
{
   ImlibFilter        *fil;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_filter_set_green", "filter", ctx->filter);
   fil = (ImlibFilter *) ctx->filter;
   __imlib_FilterSetColor(&fil->green, xoff, yoff, a, r, g, b);
}

EAPI void
imlib_filter_set_blue(int xoff, int yoff, int a, int r, int g, int b)
{
   ImlibFilter        *fil;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_filter_set_blue", "filter", ctx->filter);
   fil = (ImlibFilter *) ctx->filter;
   __imlib_FilterSetColor(&fil->blue, xoff, yoff, a, r, g, b);
}

EAPI void
imlib_filter_constants(int a, int r, int g, int b)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_filter_constants", "filter", ctx->filter);
   __imlib_FilterConstants((ImlibFilter *) ctx->filter, a, r, g, b);
}

EAPI void
imlib_filter_divisors(int a, int r, int g, int b)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_filter_divisors", "filter", ctx->filter);
   __imlib_FilterDivisors((ImlibFilter *) ctx->filter, a, r, g, b);
}

EAPI void
imlib_apply_filter(char *script, ...)
{
   va_list             param_list;
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   __imlib_dynamic_filters_init();
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   va_start(param_list, script);
   __imlib_script_parse(im, script, param_list);
   va_end(param_list);
}

/**
 * Returns a new polygon object with no points set.
 **/
EAPI                ImlibPolygon
imlib_polygon_new(void)
{
   CHECK_CONTEXT(ctx);
   return (ImlibPolygon) __imlib_polygon_new();
}

/**
 * @param poly A polygon
 * @param x The X coordinate.
 * @param y The Y coordinate.
 * 
 * Adds the point (@p x, @p y) to the polygon @p poly. The point will be added
 * to the end of the polygon's internal point list. The points are
 * drawn in order, from the first to the last. 
 **/
EAPI void
imlib_polygon_add_point(ImlibPolygon poly, int x, int y)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_polygon_add_point", "polygon", poly);
   __imlib_polygon_add_point((ImlibPoly) poly, x, y);
}

/**
 * @param poly A polygon.
 * 
 * Frees a polygon object.
 **/
EAPI void
imlib_polygon_free(ImlibPolygon poly)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_polygon_free", "polygon", poly);
   __imlib_polygon_free((ImlibPoly) poly);
}

/**
 * @param poly A polygon.
 * @param closed Closed polygon flag.
 * 
 * Draws the polygon @p poly onto the current context image. Points which have
 * been added to the polygon are drawn in sequence, first to last. The
 * final point will be joined with the first point if @p closed is
 * non-zero. 
 **/
EAPI void
imlib_image_draw_polygon(ImlibPolygon poly, unsigned char closed)
{
   ImlibImage         *im;
   DATA32              color;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_draw_polygon", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   A_VAL(&color) = (DATA8) ctx->color.alpha;
   R_VAL(&color) = (DATA8) ctx->color.red;
   G_VAL(&color) = (DATA8) ctx->color.green;
   B_VAL(&color) = (DATA8) ctx->color.blue;
   __imlib_Polygon_DrawToImage((ImlibPoly) poly, closed, color,
                               im, ctx->cliprect.x, ctx->cliprect.y,
                               ctx->cliprect.w, ctx->cliprect.h,
                               ctx->operation, ctx->blend, ctx->anti_alias);
}

/**
 * @param poly A polygon.
 * 
 * Fills the area defined by the polygon @p polyon the current context image
 * with the current context color. 
 **/
EAPI void
imlib_image_fill_polygon(ImlibPolygon poly)
{
   ImlibImage         *im;
   DATA32              color;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_fill_polygon", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   A_VAL(&color) = (DATA8) ctx->color.alpha;
   R_VAL(&color) = (DATA8) ctx->color.red;
   G_VAL(&color) = (DATA8) ctx->color.green;
   B_VAL(&color) = (DATA8) ctx->color.blue;
   __imlib_Polygon_FillToImage((ImlibPoly) poly, color,
                               im, ctx->cliprect.x, ctx->cliprect.y,
                               ctx->cliprect.w, ctx->cliprect.h,
                               ctx->operation, ctx->blend, ctx->anti_alias);
}

/**
 * @param poly A polygon.
 * @param px1 X coordinate of the upper left corner.
 * @param py1 Y coordinate of the upper left corner.
 * @param px2 X coordinate of the lower right corner.
 * @param py2 Y coordinate of the lower right corner.
 * 
 * Calculates the bounding area of the polygon @p poly. (@p px1, @p py1) defines the
 * upper left corner of the bounding box and (@p px2, @p py2) defines it's
 * lower right corner. 
 **/
EAPI void
imlib_polygon_get_bounds(ImlibPolygon poly, int *px1, int *py1, int *px2,
                         int *py2)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_polygon_get_bounds", "polygon", poly);
   __imlib_polygon_get_bounds((ImlibPoly) poly, px1, py1, px2, py2);
}

/**
 * @param xc X coordinate of the center of the ellipse.
 * @param yc Y coordinate of the center of the ellipse.
 * @param a The horizontal amplitude of the ellipse.
 * @param b The vertical amplitude of the ellipse.
 * 
 * Draws an ellipse on the current context image. The ellipse is
 * defined as (@p x-@p xc)^2/@p a^2 + (@p y-@p yc)^2/@p b^2 = 1. This means that the
 * point (@p xc, @p yc) marks the center of the ellipse, @p a defines the
 * horizontal amplitude of the ellipse, and @p b defines the vertical
 * amplitude. 
 **/
EAPI void
imlib_image_draw_ellipse(int xc, int yc, int a, int b)
{
   ImlibImage         *im;
   DATA32              color;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_draw_ellipse", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   A_VAL(&color) = (DATA8) ctx->color.alpha;
   R_VAL(&color) = (DATA8) ctx->color.red;
   G_VAL(&color) = (DATA8) ctx->color.green;
   B_VAL(&color) = (DATA8) ctx->color.blue;
   __imlib_Ellipse_DrawToImage(xc, yc, a, b, color,
                               im, ctx->cliprect.x, ctx->cliprect.y,
                               ctx->cliprect.w, ctx->cliprect.h,
                               ctx->operation, ctx->blend, ctx->anti_alias);
}

/**
 * @param xc X coordinate of the center of the ellipse.
 * @param yc Y coordinate of the center of the ellipse.
 * @param a The horizontal amplitude of the ellipse.
 * @param b The vertical amplitude of the ellipse.
 * 
 * Fills an ellipse on the current context image using the current
 * context color. The ellipse is 
 * defined as (@p x-@p xc)^2/@p a^2 + (@p y-@p yc)^2/@p b^2 = 1. This means that the
 * point (@p xc, @p yc) marks the center of the ellipse, @p a defines the
 * horizontal amplitude of the ellipse, and @p b defines the vertical
 * amplitude. 
 **/
EAPI void
imlib_image_fill_ellipse(int xc, int yc, int a, int b)
{
   ImlibImage         *im;
   DATA32              color;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_fill_ellipse", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   A_VAL(&color) = (DATA8) ctx->color.alpha;
   R_VAL(&color) = (DATA8) ctx->color.red;
   G_VAL(&color) = (DATA8) ctx->color.green;
   B_VAL(&color) = (DATA8) ctx->color.blue;
   __imlib_Ellipse_FillToImage(xc, yc, a, b, color,
                               im, ctx->cliprect.x, ctx->cliprect.y,
                               ctx->cliprect.w, ctx->cliprect.h,
                               ctx->operation, ctx->blend, ctx->anti_alias);
}

/**
 * @param poly A polygon
 * @param x The X coordinate.
 * @param y The Y coordinate.
 * 
 * Returns non-zero if the point (@p x, @p y) is within the area defined by
 * the polygon @p poly.
 **/
EAPI unsigned char
imlib_polygon_contains_point(ImlibPolygon poly, int x, int y)
{
   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER_RETURN("imlib_polygon_contains_point", "polygon", poly,
                              0);
   return __imlib_polygon_contains_point((ImlibPoly) poly, x, y);
}

EAPI void
imlib_image_clear(void)
{
   ImlibImage         *im;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_clear", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   memset(im->data, 0, im->w * im->h * sizeof(DATA32));
}

EAPI void
imlib_image_clear_color(int r, int g, int b, int a)
{
   ImlibImage         *im;
   int                 i, max;
   DATA32              col;

   CHECK_CONTEXT(ctx);
   CHECK_PARAM_POINTER("imlib_image_clear_color", "image", ctx->image);
   CAST_IMAGE(im, ctx->image);
   if ((!(im->data)) && (im->loader) && (im->loader->load))
      im->loader->load(im, NULL, 0, 1);
   if (!(im->data))
      return;
   __imlib_DirtyImage(im);
   max = im->w * im->h;
   WRITE_RGBA(&col, r, g, b, a);
   for (i = 0; i < max; i++)
      im->data[i] = col;
}
