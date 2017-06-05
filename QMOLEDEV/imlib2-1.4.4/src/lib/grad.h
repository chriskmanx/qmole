#ifndef __GRAD
#define __GRAD 1

typedef struct _imlib_range       ImlibRange;
typedef struct _imlib_range_color ImlibRangeColor;

struct _imlib_range_color
{
   DATA8            red, green, blue, alpha;
   int              distance;
   ImlibRangeColor *next;
};

struct _imlib_range
{
   ImlibRangeColor  *color;
};

__hidden ImlibRange *__imlib_CreateRange(void);
__hidden void        __imlib_FreeRange(ImlibRange *rg);
__hidden void        __imlib_AddRangeColor(ImlibRange *rg, DATA8 r, DATA8 g, DATA8 b, 
				  DATA8 a, int dist);
__hidden DATA32     *__imlib_MapRange(ImlibRange *rg, int len);
__hidden DATA32     *__imlib_MapHsvaRange(ImlibRange *rg, int len);
__hidden void        __imlib_DrawGradient(ImlibImage *im, int x, int y, int w, int h, ImlibRange *rg, double angle, ImlibOp op, int clx, int cly, int clw, int clh);
__hidden void        __imlib_DrawHsvaGradient(ImlibImage *im, int x, int y, int w, int h, ImlibRange *rg, double angle, ImlibOp op, int clx, int cly, int clw, int clh);

#endif
