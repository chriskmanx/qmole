#ifndef __RGBA
#define __RGBA 1

#define DM_BS1 (8 + 3)
#define DM_BS2 (8)
#define DM_X (8)
#define DM_Y (8)

__hidden void       __imlib_RGBASetupContext(Context * ct);
__hidden void       __imlib_RGBA_init(void *rd, void *gd, void *bd, int depth,
                                      DATA8 palette_type);

typedef void        (*ImlibRGBAFunction) (DATA32 *, int, DATA8 *,
                                          int, int, int, int, int);
typedef void        (*ImlibMaskFunction) (DATA32 *, int, DATA8 *,
                                          int, int, int, int, int, int);
__hidden            ImlibRGBAFunction
__imlib_GetRGBAFunction(int depth,
                        unsigned long rm, unsigned long gm, unsigned long bm,
                        char hiq, DATA8 palette_type);
__hidden ImlibMaskFunction __imlib_GetMaskFunction(char hiq);

#ifdef DO_MMX_ASM
void                __imlib_mmx_rgb555_fast(DATA32 *, int, DATA8 *, int, int,
                                            int, int, int);
void                __imlib_mmx_bgr555_fast(DATA32 *, int, DATA8 *, int, int,
                                            int, int, int);
void                __imlib_mmx_rgb565_fast(DATA32 *, int, DATA8 *, int, int,
                                            int, int, int);
void                __imlib_mmx_bgr565_fast(DATA32 *, int, DATA8 *, int, int,
                                            int, int, int);
#endif

#endif
