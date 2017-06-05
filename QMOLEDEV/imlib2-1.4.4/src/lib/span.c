#include "common.h"
#include "colormod.h"
#include "image.h"
#include "blend.h"
#include "span.h"

#define ADD_COPY(r, g, b, dest) \
do { \
                ADD_COLOR(R_VAL(dest), r, R_VAL(dest)); \
                ADD_COLOR(G_VAL(dest), g, G_VAL(dest)); \
                ADD_COLOR(B_VAL(dest), b, B_VAL(dest)); \
} while (0)

#define SUB_COPY(r, g, b, dest) \
do { \
                SUB_COLOR(R_VAL(dest), r, R_VAL(dest)); \
                SUB_COLOR(G_VAL(dest), g, G_VAL(dest)); \
                SUB_COLOR(B_VAL(dest), b, B_VAL(dest)); \
} while (0)

#define RE_COPY(r, g, b, dest) \
do { \
                RESHADE_COLOR(R_VAL(dest), r, R_VAL(dest)); \
                RESHADE_COLOR(G_VAL(dest), g, G_VAL(dest)); \
                RESHADE_COLOR(B_VAL(dest), b, B_VAL(dest)); \
} while (0)

#define MULT(na, a0, a1, tmp) \
do { \
   tmp = ((a0) * (a1)) + 0x80;   \
   na = (tmp + (tmp >> 8)) >> 8; \
} while (0)

extern DATA8        pow_lut[256][256];

/*   point drawing functions  */

/* COPY OPS */

static void
__imlib_CopyToRGBA(DATA32 color, DATA32 * dst)
{
   *dst = color;
}

static void
__imlib_CopyToRGB(DATA32 color, DATA32 * dst)
{
   *dst = (*dst & 0xff000000) | (color & 0x00ffffff);
}

static void
__imlib_BlendToRGB(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;

   BLEND(R_VAL(&color), G_VAL(&color), B_VAL(&color), A_VAL(&color), dst);
}

static void
__imlib_BlendToRGBA(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;
   DATA8               a;

   a = pow_lut[A_VAL(&color)][A_VAL(dst)];
   BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
   BLEND(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
}

/* ADD OPS */

static void
__imlib_AddCopyToRGBA(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;

   A_VAL(dst) = A_VAL(&color);
   ADD_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
}

static void
__imlib_AddCopyToRGB(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;

   ADD_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
}

static void
__imlib_AddBlendToRGB(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;

   BLEND_ADD(R_VAL(&color), G_VAL(&color), B_VAL(&color), A_VAL(&color), dst);
}

static void
__imlib_AddBlendToRGBA(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;
   DATA8               a;

   a = pow_lut[A_VAL(&color)][A_VAL(dst)];
   BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
   BLEND_ADD(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
}

/* SUBTRACT OPS */

static void
__imlib_SubCopyToRGBA(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;

   A_VAL(dst) = A_VAL(&color);
   SUB_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
}

static void
__imlib_SubCopyToRGB(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;

   SUB_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
}

static void
__imlib_SubBlendToRGB(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;

   BLEND_SUB(R_VAL(&color), G_VAL(&color), B_VAL(&color), A_VAL(&color), dst);
}

static void
__imlib_SubBlendToRGBA(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;
   DATA8               a;

   a = pow_lut[A_VAL(&color)][A_VAL(dst)];
   BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
   BLEND_SUB(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
}

/* RESHADE OPS */

static void
__imlib_ReCopyToRGBA(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;

   A_VAL(dst) = A_VAL(&color);
   RE_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
}

static void
__imlib_ReCopyToRGB(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;

   RE_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
}

static void
__imlib_ReBlendToRGB(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;

   BLEND_RE(R_VAL(&color), G_VAL(&color), B_VAL(&color), A_VAL(&color), dst);
}

static void
__imlib_ReBlendToRGBA(DATA32 color, DATA32 * dst)
{
   DATA32              tmp;
   DATA8               a;

   a = pow_lut[A_VAL(&color)][A_VAL(dst)];
   BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
   BLEND_RE(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
}

/*  span drawing functions*  */

/* COPY OPS */

static void
__imlib_CopySpanToRGBA(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        *dst = color;
        dst++;
     }
}

static void
__imlib_CopySpanToRGB(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        *dst = (*dst & 0xff000000) | (color & 0x00ffffff);
        dst++;
     }
}

static void
__imlib_BlendSpanToRGB(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        BLEND(R_VAL(&color), G_VAL(&color), B_VAL(&color), A_VAL(&color), dst);
        dst++;
     }
}

static void
__imlib_BlendSpanToRGBA(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;
        DATA8               a;

        a = pow_lut[A_VAL(&color)][A_VAL(dst)];
        BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
        BLEND(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
        dst++;
     }
}

/* ADD OPS */

static void
__imlib_AddCopySpanToRGBA(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        A_VAL(dst) = A_VAL(&color);
        ADD_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
        dst++;
     }
}

static void
__imlib_AddCopySpanToRGB(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        ADD_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
        dst++;
     }
}

static void
__imlib_AddBlendSpanToRGB(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        BLEND_ADD(R_VAL(&color), G_VAL(&color), B_VAL(&color), A_VAL(&color),
                  dst);
        dst++;
     }
}

static void
__imlib_AddBlendSpanToRGBA(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;
        DATA8               a;

        a = pow_lut[A_VAL(&color)][A_VAL(dst)];
        BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
        BLEND_ADD(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
        dst++;
     }
}

/* SUBTRACT OPS */

static void
__imlib_SubCopySpanToRGBA(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        A_VAL(dst) = A_VAL(&color);
        SUB_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
        dst++;
     }
}

static void
__imlib_SubCopySpanToRGB(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        SUB_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
        dst++;
     }
}

static void
__imlib_SubBlendSpanToRGB(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        BLEND_SUB(R_VAL(&color), G_VAL(&color), B_VAL(&color), A_VAL(&color),
                  dst);
        dst++;
     }
}

static void
__imlib_SubBlendSpanToRGBA(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;
        DATA8               a;

        a = pow_lut[A_VAL(&color)][A_VAL(dst)];
        BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
        BLEND_SUB(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
        dst++;
     }
}

/* RESHADE OPS */

static void
__imlib_ReCopySpanToRGBA(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        A_VAL(dst) = A_VAL(&color);
        RE_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
        dst++;
     }
}

static void
__imlib_ReCopySpanToRGB(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        RE_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
        dst++;
     }
}

static void
__imlib_ReBlendSpanToRGB(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        BLEND_RE(R_VAL(&color), G_VAL(&color), B_VAL(&color), A_VAL(&color),
                 dst);
        dst++;
     }
}

static void
__imlib_ReBlendSpanToRGBA(DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;
        DATA8               a;

        a = pow_lut[A_VAL(&color)][A_VAL(dst)];
        BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
        BLEND_RE(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
        dst++;
     }
}

/*  shaped span drawing functions*  */

/* COPY OPS */

static void
__imlib_CopyShapedSpanToRGBA(DATA8 * src, DATA32 color, DATA32 * dst, int len)
{
   DATA32              col = color;

   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     *dst = color;
                     break;
                  }
               default:
                  {
                     MULT(A_VAL(&col), *src, A_VAL(&color), tmp);
                     *dst = col;
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        switch (*src)
          {
          case 0:
             break;
          case 255:
             {
                *dst = color;
                break;
             }
          default:
             {
                A_VAL(&col) = *src;
                *dst = col;
                break;
             }
          }
        src++;
        dst++;
     }
}

static void
__imlib_CopyShapedSpanToRGB(DATA8 * src, DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        if (*src)
           *dst = (*dst & 0xff000000) | (color & 0x00ffffff);

        src++;
        dst++;
     }
}

static void
__imlib_BlendShapedSpanToRGB(DATA8 * src, DATA32 color, DATA32 * dst, int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;
             DATA8               a;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     BLEND(R_VAL(&color), G_VAL(&color), B_VAL(&color),
                           A_VAL(&color), dst);
                     break;
                  }
               default:
                  {
                     MULT(a, *src, A_VAL(&color), tmp);
                     BLEND(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;

        switch (*src)
          {
          case 0:
             break;
          case 255:
             {
                *dst = (*dst & 0xff000000) | (color & 0x00ffffff);
                break;
             }
          default:
             {
                BLEND(R_VAL(&color), G_VAL(&color), B_VAL(&color), *src, dst);
                break;
             }
          }
        src++;
        dst++;
     }
}

static void
__imlib_BlendShapedSpanToRGBA(DATA8 * src, DATA32 color, DATA32 * dst, int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;
             DATA8               a, aa;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     a = pow_lut[A_VAL(&color)][A_VAL(dst)];
                     BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
                     BLEND(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
                     break;
                  }
               default:
                  {
                     MULT(aa, *src, A_VAL(&color), tmp);
                     a = pow_lut[aa][A_VAL(dst)];
                     BLEND_COLOR(aa, A_VAL(dst), 255, A_VAL(dst));
                     BLEND(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;
        DATA8               a;

        switch (*src)
          {
          case 0:
             break;
          case 255:
             {
                *dst = color;
                break;
             }
          default:
             {
                a = pow_lut[*src][A_VAL(dst)];
                BLEND_COLOR(*src, A_VAL(dst), 255, A_VAL(dst));
                BLEND(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
                break;
             }
          }
        src++;
        dst++;
     }
}

/* ADD OPS */

static void
__imlib_AddCopyShapedSpanToRGBA(DATA8 * src, DATA32 color, DATA32 * dst,
                                int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     A_VAL(dst) = A_VAL(&color);
                     ADD_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                     break;
                  }
               default:
                  {
                     MULT(A_VAL(dst), *src, A_VAL(&color), tmp);
                     ADD_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;

        if (*src)
          {
             A_VAL(dst) = *src;
             ADD_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
          }
        src++;
        dst++;
     }
}

static void
__imlib_AddCopyShapedSpanToRGB(DATA8 * src, DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        if (*src)
          {
             ADD_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
          }

        src++;
        dst++;
     }
}

static void
__imlib_AddBlendShapedSpanToRGB(DATA8 * src, DATA32 color, DATA32 * dst,
                                int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;
             DATA8               a;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     BLEND_ADD(R_VAL(&color), G_VAL(&color), B_VAL(&color),
                               A_VAL(&color), dst);
                     break;
                  }
               default:
                  {
                     MULT(a, *src, A_VAL(&color), tmp);
                     BLEND_ADD(R_VAL(&color), G_VAL(&color), B_VAL(&color), a,
                               dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;

        switch (*src)
          {
          case 0:
             break;
          case 255:
             {
                ADD_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                break;
             }
          default:
             {
                BLEND_ADD(R_VAL(&color), G_VAL(&color), B_VAL(&color), *src,
                          dst);
                break;
             }
          }
        src++;
        dst++;
     }
}

static void
__imlib_AddBlendShapedSpanToRGBA(DATA8 * src, DATA32 color, DATA32 * dst,
                                 int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;
             DATA8               a, aa;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     a = pow_lut[A_VAL(&color)][A_VAL(dst)];
                     BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
                     BLEND_ADD(R_VAL(&color), G_VAL(&color), B_VAL(&color), a,
                               dst);
                     break;
                  }
               default:
                  {
                     MULT(aa, *src, A_VAL(&color), tmp);
                     a = pow_lut[aa][A_VAL(dst)];
                     BLEND_COLOR(aa, A_VAL(dst), 255, A_VAL(dst));
                     BLEND_ADD(R_VAL(&color), G_VAL(&color), B_VAL(&color), a,
                               dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;
        DATA8               a;

        switch (*src)
          {
          case 0:
             break;
          case 255:
             {
                A_VAL(dst) = 255;
                ADD_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                break;
             }
          default:
             {
                a = pow_lut[*src][A_VAL(dst)];
                BLEND_COLOR(*src, A_VAL(dst), 255, A_VAL(dst));
                BLEND_ADD(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
                break;
             }
          }
        src++;
        dst++;
     }
}

/* SUBTRACT OPS */

static void
__imlib_SubCopyShapedSpanToRGBA(DATA8 * src, DATA32 color, DATA32 * dst,
                                int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     A_VAL(dst) = A_VAL(&color);
                     SUB_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                     break;
                  }
               default:
                  {
                     MULT(A_VAL(dst), *src, A_VAL(&color), tmp);
                     SUB_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;

        if (*src)
          {
             A_VAL(dst) = *src;
             SUB_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
          }
        src++;
        dst++;
     }
}

static void
__imlib_SubCopyShapedSpanToRGB(DATA8 * src, DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        if (*src)
          {
             SUB_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
          }

        src++;
        dst++;
     }
}

static void
__imlib_SubBlendShapedSpanToRGB(DATA8 * src, DATA32 color, DATA32 * dst,
                                int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;
             DATA8               a;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     BLEND_SUB(R_VAL(&color), G_VAL(&color), B_VAL(&color),
                               A_VAL(&color), dst);
                     break;
                  }
               default:
                  {
                     MULT(a, *src, A_VAL(&color), tmp);
                     BLEND_SUB(R_VAL(&color), G_VAL(&color), B_VAL(&color), a,
                               dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;

        switch (*src)
          {
          case 0:
             break;
          case 255:
             {
                SUB_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                break;
             }
          default:
             {
                BLEND_SUB(R_VAL(&color), G_VAL(&color), B_VAL(&color), *src,
                          dst);
                break;
             }
          }
        src++;
        dst++;
     }
}

static void
__imlib_SubBlendShapedSpanToRGBA(DATA8 * src, DATA32 color, DATA32 * dst,
                                 int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;
             DATA8               a, aa;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     a = pow_lut[A_VAL(&color)][A_VAL(dst)];
                     BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
                     BLEND_SUB(R_VAL(&color), G_VAL(&color), B_VAL(&color), a,
                               dst);
                     break;
                  }
               default:
                  {
                     MULT(aa, *src, A_VAL(&color), tmp);
                     a = pow_lut[aa][A_VAL(dst)];
                     BLEND_COLOR(aa, A_VAL(dst), 255, A_VAL(dst));
                     BLEND_SUB(R_VAL(&color), G_VAL(&color), B_VAL(&color), a,
                               dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;
        DATA8               a;

        switch (*src)
          {
          case 0:
             break;
          case 255:
             {
                A_VAL(dst) = 255;
                SUB_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                break;
             }
          default:
             {
                a = pow_lut[*src][A_VAL(dst)];
                BLEND_COLOR(*src, A_VAL(dst), 255, A_VAL(dst));
                BLEND_SUB(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
                break;
             }
          }
        src++;
        dst++;
     }
}

/* RESHADE OPS */

static void
__imlib_ReCopyShapedSpanToRGBA(DATA8 * src, DATA32 color, DATA32 * dst, int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     A_VAL(dst) = A_VAL(&color);
                     RE_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                     break;
                  }
               default:
                  {
                     MULT(A_VAL(dst), *src, A_VAL(&color), tmp);
                     RE_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;

        if (*src)
          {
             A_VAL(dst) = *src;
             RE_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
          }
        src++;
        dst++;
     }
}

static void
__imlib_ReCopyShapedSpanToRGB(DATA8 * src, DATA32 color, DATA32 * dst, int len)
{
   while (len--)
     {
        DATA32              tmp;

        if (*src)
          {
             RE_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
          }

        src++;
        dst++;
     }
}

static void
__imlib_ReBlendShapedSpanToRGB(DATA8 * src, DATA32 color, DATA32 * dst, int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;
             DATA8               a;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     BLEND_RE(R_VAL(&color), G_VAL(&color), B_VAL(&color),
                              A_VAL(&color), dst);
                     break;
                  }
               default:
                  {
                     MULT(a, *src, A_VAL(&color), tmp);
                     BLEND_RE(R_VAL(&color), G_VAL(&color), B_VAL(&color), a,
                              dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;

        switch (*src)
          {
          case 0:
             break;
          case 255:
             {
                RE_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                break;
             }
          default:
             {
                BLEND_RE(R_VAL(&color), G_VAL(&color), B_VAL(&color), *src,
                         dst);
                break;
             }
          }
        src++;
        dst++;
     }
}

static void
__imlib_ReBlendShapedSpanToRGBA(DATA8 * src, DATA32 color, DATA32 * dst,
                                int len)
{
   if (A_VAL(&color) < 255)
     {
        while (len--)
          {
             DATA32              tmp;
             DATA8               a, aa;

             switch (*src)
               {
               case 0:
                  break;
               case 255:
                  {
                     a = pow_lut[A_VAL(&color)][A_VAL(dst)];
                     BLEND_COLOR(A_VAL(&color), A_VAL(dst), 255, A_VAL(dst));
                     BLEND_RE(R_VAL(&color), G_VAL(&color), B_VAL(&color), a,
                              dst);
                     break;
                  }
               default:
                  {
                     MULT(aa, *src, A_VAL(&color), tmp);
                     a = pow_lut[aa][A_VAL(dst)];
                     BLEND_COLOR(aa, A_VAL(dst), 255, A_VAL(dst));
                     BLEND_RE(R_VAL(&color), G_VAL(&color), B_VAL(&color), a,
                              dst);
                     break;
                  }
               }
             src++;
             dst++;
          }
        return;
     }

   while (len--)
     {
        DATA32              tmp;
        DATA8               a;

        switch (*src)
          {
          case 0:
             break;
          case 255:
             {
                A_VAL(dst) = 255;
                RE_COPY(R_VAL(&color), G_VAL(&color), B_VAL(&color), dst);
                break;
             }
          default:
             {
                a = pow_lut[*src][A_VAL(dst)];
                BLEND_COLOR(*src, A_VAL(dst), 255, A_VAL(dst));
                BLEND_RE(R_VAL(&color), G_VAL(&color), B_VAL(&color), a, dst);
                break;
             }
          }
        src++;
        dst++;
     }
}

ImlibPointDrawFunction
__imlib_GetPointDrawFunction(ImlibOp op, char dst_alpha, char blend)
{
   /* [ operation ][ dst_alpha ][ blend ]  */
   static ImlibPointDrawFunction ptfuncs[4][2][2] =
      /* OP_COPY */
   { {{__imlib_CopyToRGB, __imlib_BlendToRGB},
      {__imlib_CopyToRGBA, __imlib_BlendToRGBA}},
   /* OP_ADD */
   {{__imlib_AddCopyToRGB, __imlib_AddBlendToRGB},
    {__imlib_AddCopyToRGBA, __imlib_AddBlendToRGBA}},
   /* OP_SUBTRACT */
   {{__imlib_SubCopyToRGB, __imlib_SubBlendToRGB},
    {__imlib_SubCopyToRGBA, __imlib_SubBlendToRGBA}},
   /* OP_RESHADE */
   {{__imlib_ReCopyToRGB, __imlib_ReBlendToRGB},
    {__imlib_ReCopyToRGBA, __imlib_ReBlendToRGBA}},
   };

   int                 opi = (op == OP_COPY) ? 0
      : (op == OP_ADD) ? 1
      : (op == OP_SUBTRACT) ? 2 : (op == OP_RESHADE) ? 3 : -1;

   if (opi == -1)
      return NULL;

   return ptfuncs[opi][!!dst_alpha][!!blend];
}

ImlibSpanDrawFunction
__imlib_GetSpanDrawFunction(ImlibOp op, char dst_alpha, char blend)
{
   static ImlibSpanDrawFunction spanfuncs[4][2][2] =
      /* OP_COPY */
   { {{__imlib_CopySpanToRGB, __imlib_BlendSpanToRGB},
      {__imlib_CopySpanToRGBA, __imlib_BlendSpanToRGBA}},
   /* OP_ADD */
   {{__imlib_AddCopySpanToRGB, __imlib_AddBlendSpanToRGB},
    {__imlib_AddCopySpanToRGBA, __imlib_AddBlendSpanToRGBA}},
   /* OP_SUBTRACT */
   {{__imlib_SubCopySpanToRGB, __imlib_SubBlendSpanToRGB},
    {__imlib_SubCopySpanToRGBA, __imlib_SubBlendSpanToRGBA}},
   /* OP_RESHADE */
   {{__imlib_ReCopySpanToRGB, __imlib_ReBlendSpanToRGB},
    {__imlib_ReCopySpanToRGBA, __imlib_ReBlendSpanToRGBA}},
   };

   int                 opi = (op == OP_COPY) ? 0
      : (op == OP_ADD) ? 1
      : (op == OP_SUBTRACT) ? 2 : (op == OP_RESHADE) ? 3 : -1;

   if (opi == -1)
      return NULL;

   return spanfuncs[opi][!!dst_alpha][!!blend];
}

ImlibShapedSpanDrawFunction
__imlib_GetShapedSpanDrawFunction(ImlibOp op, char dst_alpha, char blend)
{
   static ImlibShapedSpanDrawFunction shapedspanfuncs[4][2][2] =
      /* OP_COPY */
   { {{__imlib_CopyShapedSpanToRGB, __imlib_BlendShapedSpanToRGB},
      {__imlib_CopyShapedSpanToRGBA, __imlib_BlendShapedSpanToRGBA}},
   /* OP_ADD */
   {{__imlib_AddCopyShapedSpanToRGB, __imlib_AddBlendShapedSpanToRGB},
    {__imlib_AddCopyShapedSpanToRGBA, __imlib_AddBlendShapedSpanToRGBA}},
   /* OP_SUBTRACT */
   {{__imlib_SubCopyShapedSpanToRGB, __imlib_SubBlendShapedSpanToRGB},
    {__imlib_SubCopyShapedSpanToRGBA, __imlib_SubBlendShapedSpanToRGBA}},
   /* OP_RESHADE */
   {{__imlib_ReCopyShapedSpanToRGB, __imlib_ReBlendShapedSpanToRGB},
    {__imlib_ReCopyShapedSpanToRGBA, __imlib_ReBlendShapedSpanToRGBA}},
   };

   int                 opi = (op == OP_COPY) ? 0
      : (op == OP_ADD) ? 1
      : (op == OP_SUBTRACT) ? 2 : (op == OP_RESHADE) ? 3 : -1;

   if (opi == -1)
      return NULL;

   return shapedspanfuncs[opi][!!dst_alpha][!!blend];
}
