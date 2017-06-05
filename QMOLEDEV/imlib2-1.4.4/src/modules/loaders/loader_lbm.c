/*------------------------------------------------------------------------------
 * Reads regular Amiga IFF ILBM files.
 *
 * Supports IMLIB2_LBM_NOMASK environment variable. If this is set to "1", then
 * a transparency mask in an image will be ignored. On the Amiga a mask is often
 * applied only when loading a brush rather than a picture, but this loader has
 * no way to tell when the user wants this behaviour from the picture alone.
 *
 * Author:  John Bickers <jbickers@ihug.co.nz>
 * Since:   2004-08-21
 * Version: 2004-08-28
 *------------------------------------------------------------------------------*/

#include "loader_common.h"

#define L2RLONG(a) ((((long)((a)[0]) & 0xff) << 24) + (((long)((a)[1]) & 0xff) << 16) + (((long)((a)[2]) & 0xff) << 8) + ((long)((a)[3]) & 0xff))
#define L2RWORD(a) ((((long)((a)[0]) & 0xff) << 8) + ((long)((a)[1]) & 0xff))

typedef struct CHUNK {
    long    size;
    unsigned char *data;
} CHUNK;

typedef struct ILBM {
    CHUNK   bmhd;
    CHUNK   camg;
    CHUNK   cmap;
    CHUNK   ctbl;
    CHUNK   sham;
    CHUNK   body;

    int     depth;
    int     mask;
    int     ham;
    int     hbrite;

    int     row;

    int     offset;
    int     count;
    int     rle;
} ILBM;


/*------------------------------------------------------------------------------
 * Frees memory allocated as part of an ILBM structure.
 *------------------------------------------------------------------------------*/
static void freeilbm(ILBM *ilbm)
{
    if (ilbm->bmhd.data) free(ilbm->bmhd.data);
    if (ilbm->camg.data) free(ilbm->camg.data);
    if (ilbm->cmap.data) free(ilbm->cmap.data);
    if (ilbm->ctbl.data) free(ilbm->ctbl.data);
    if (ilbm->sham.data) free(ilbm->sham.data);
    if (ilbm->body.data) free(ilbm->body.data);

    memset(ilbm, 0, sizeof(*ilbm));
}


/*------------------------------------------------------------------------------
 * Reads the given chunks out of a file, returns 0 if the file had a problem.
 *
 * Format FORMsizeILBMtag.size....tag.size....tag.size....
 *------------------------------------------------------------------------------*/
static int loadchunks(char *name, ILBM *ilbm, int full)
{
CHUNK   *c;
FILE    *f;
size_t  s;
long    formsize, pos, z;
int     ok, seek;
char    buf[12];

    ok = 0;

    f = fopen(name, "rb");
    if (f) {
        s = fread(buf, 1, 12, f);
        if (s == 12 && !memcmp(buf, "FORM", 4) && !memcmp(buf + 8, "ILBM", 4)) {
            memset(ilbm, 0, sizeof(*ilbm));
            formsize = L2RLONG(buf + 4);

            while (1) {
                pos = ftell(f);
                if (pos < 0 || pos >= formsize + 8) break;  /* Error or FORM data is finished. */
                seek = 1;

                s = fread(buf, 1, 8, f);
                if (s != 8) break;                          /* Error or short file. */

                z = L2RLONG(buf + 4);
                if (z < 0) break;                           /* Corrupt file. */

                c = NULL;
                if (!memcmp(buf, "BMHD", 4)) c = &(ilbm->bmhd);
                else if (full) {
                    if (!memcmp(buf, "CAMG", 4)) c = &(ilbm->camg);
                    else if (!memcmp(buf, "CMAP", 4)) c = &(ilbm->cmap);
                    else if (!memcmp(buf, "CTBL", 4)) c = &(ilbm->ctbl);
                    else if (!memcmp(buf, "SHAM", 4)) c = &(ilbm->sham);
                    else if (!memcmp(buf, "BODY", 4)) c = &(ilbm->body);
                }

                if (c && !c->data) {
                    c->size = z;
                    c->data = malloc(c->size);
                    if (!c->data) break;                    /* Out of memory. */

                    s = fread(c->data, 1, c->size, f);
                    if (s != c->size) break;                /* Error or short file. */

                    seek = 0;
                    if (!full) {                            /* Only BMHD required. */
                        ok = 1;
                        break;
                    }
                }

                if (pos + 8 + z >= formsize + 8) break;     /* This was last chunk. */

                if (seek && fseek(f, z, SEEK_CUR) != 0) break;
            }

          /* File may end strangely, especially if body size is uneven, but it's
           * ok if we have the chunks we want. !full check is already done. */
            if (ilbm->bmhd.data && ilbm->body.data) ok = 1;
            if (!ok) freeilbm(ilbm);
        }
        fclose(f);
    }

    return ok;
}


/*------------------------------------------------------------------------------
 * Unpacks a row of possibly RLE data at a time.
 *
 * RLE compression depends on a count byte, followed by data bytes.
 *
 * 0x80 means skip.
 * 0xff to 0x81 means repeat one data byte (256 - count) + 1 times.
 * 0x00 to 0x7f means copy count + 1 data bytes.
 *
 * In theory RLE compression is not supposed to create runs across scanlines.
 *------------------------------------------------------------------------------*/
static void bodyrow(unsigned char *p, int z, ILBM *ilbm)
{
int     i, x, w;
unsigned char b;

    if (ilbm->offset >= ilbm->body.size) {
        memset(p, 0, z);
        return;
    }

    if (!ilbm->rle) {
        w = ilbm->body.size - ilbm->offset;
        if (w > z) w = z;
        memcpy(p, ilbm->body.data + ilbm->offset, w);
        if (w < z) memset(p + w, 0, z - w);
        ilbm->offset += w;
        return;
    }

    for (i = 0; i < z; ) {
        b = ilbm->body.data[ilbm->offset++];
        while (b == 0x80 && ilbm->offset < ilbm->body.size) b = ilbm->body.data[ilbm->offset++];
        if (ilbm->offset >= ilbm->body.size) break;

        if (b & 0x80) {
            w = (0x100 - b) + 1;
            if (w > z - i) w = z - i;
            
            b = ilbm->body.data[ilbm->offset++];
            memset(p + i, b, w);
            i += w;
        }
        else {
            w = (b & 0x7f) + 1;
            if (w > ilbm->body.size - ilbm->offset) w = ilbm->body.size - ilbm->offset; 
            x = (w <= z - i)? w: z - i;
            memcpy(p + i, ilbm->body.data + ilbm->offset, x);
            i += x;
            ilbm->offset += w;
        }
    }
    
    if (i < z) memset(p, 0, z - i);
}


/*------------------------------------------------------------------------------
 * Shifts a value to produce an 8-bit colour gun, and fills in the lower bits
 * from the high bits of the value so that, for example, 4-bit 0x0f scales to
 * 0xff, or 1-bit 0x01 scales to 0xff.
 *------------------------------------------------------------------------------*/
static unsigned char scalegun(unsigned char v, int sl)
{
int     sr;

    switch (sl) {
        case 1:
        case 2:
        case 3:
            sr = 8 - sl;
            return (v << sl) | (v >> sr);

        case 4:
            return (v << 4) | v;

        case 5:
            return v * 0x24;

        case 6:
            return v * 0x55;

        case 7:
            return v * 0xff;
    }
    return v;
}


/*------------------------------------------------------------------------------
 * Scales the colours in a CMAP chunk if they all look like 4-bit colour, so
 * that they use all 8-bits. This is done by copying the high nybble into the
 * low nybble, so for example 0xf0 becomes 0xff.
 *------------------------------------------------------------------------------*/
static void scalecmap(ILBM *ilbm)
{
int     i;

    if (!ilbm->cmap.data) return;

    for (i = 0; i < ilbm->cmap.size; i++)
        if (ilbm->cmap.data[i] & 0x0f) return;

    for (i = 0; i < ilbm->cmap.size; i++)
        ilbm->cmap.data[i] |= ilbm->cmap.data[i] >> 4;
}


/*------------------------------------------------------------------------------
 * Deplanes and converts an array of bitplanes to a single scanline of DATA32
 * (unsigned int) values. DATA32 is ARGB.
 *------------------------------------------------------------------------------*/
static void deplane(DATA32 *row, int w, ILBM *ilbm, unsigned char *plane[])
{
unsigned long l;
int     i, o, x;
unsigned char bit, r, g, b, a, v, h, *pal;

    pal = NULL;
    if (ilbm->sham.data && ilbm->sham.size >= 2 + (ilbm->row + 1) * 2 * 16)
        pal = ilbm->sham.data + 2 + ilbm->row * 2 * 16;
    if (ilbm->ctbl.data && ilbm->ctbl.size >= (ilbm->row + 1) * 2 * 16)
        pal = ilbm->ctbl.data + ilbm->row * 2 * 16;

    if (ilbm->ham) r = g = b = 0;

    bit = 0x80;
    o = 0;
    for (x = 0; x < w; x++) {
        l = 0;
        for (i = ilbm->depth - 1; i >= 0; i--) {
            l = l << 1;
            if (plane[i][o] & bit) l = l | 1;
        }
        a = (ilbm->mask == 0 || (ilbm->mask == 1 && (plane[ilbm->depth][o] & bit)) || ilbm->mask == 2)? 0xff: 0x00;

        if (ilbm->depth == 32) {
            a = (l >> 24) & 0xff;
            b = (l >> 16) & 0xff;
            g = (l >> 8) & 0xff;
            r = l & 0xff;
        }
        else if (ilbm->depth == 24) {
            b = (l >> 16) & 0xff;
            g = (l >> 8) & 0xff;
            r = l & 0xff;
        }
        else if (ilbm->ham) {
            v = l & ((1 << (ilbm->depth - 2)) - 1);
            h = (l & ~v) >> (ilbm->depth - 2);

            if (h == 0x00) {
                if (!pal) {
                    if ((v + 1) * 3 <= ilbm->cmap.size) {
                        r = ilbm->cmap.data[v * 3];
                        g = ilbm->cmap.data[v * 3 + 1];
                        b = ilbm->cmap.data[v * 3 + 2];
                    }
                    else r = g = b = 0;
                }
                else {
                    r = scalegun(pal[v * 2] & 0x0f, 4);
                    g = scalegun((pal[v * 2 + 1] & 0xf0) >> 4, 4);
                    b = scalegun((pal[v * 2 + 1] & 0x0f), 4);
                }
            }
            else if (h == 0x01) b = scalegun(v, 8 - (ilbm->depth - 2));
            else if (h == 0x02) r = scalegun(v, 8 - (ilbm->depth - 2));
            else g = scalegun(v, 8 - (ilbm->depth - 2));
        }
        else if (ilbm->hbrite) {
            v = l & ((1 << (ilbm->depth - 1)) - 1);
            h = (l & ~v) >> (ilbm->depth - 1);

            if (!pal) {
                if ((v + 1) * 3 <= ilbm->cmap.size) {
                    r = ilbm->cmap.data[v * 3];
                    g = ilbm->cmap.data[v * 3 + 1];
                    b = ilbm->cmap.data[v * 3 + 2];
                }
                else r = g = b = 0;
            }
            else {
                r = scalegun(pal[v * 2] & 0x0f, 4);
                g = scalegun((pal[v * 2 + 1] & 0xf0) >> 4, 4);
                b = scalegun((pal[v * 2 + 1] & 0x0f), 4);
            }

            if (h) {
                r = r >> 1;
                g = g >> 1;
                b = b >> 1;
            }

            if (ilbm->mask == 2 && v == L2RWORD(ilbm->bmhd.data + 12)) a = 0x00;
        }
        else if (ilbm->cmap.size == 0 && !pal) {
            v = l & ((1 << ilbm->depth) - 1);
            r = scalegun(v, ilbm->depth);
            g = r;
            b = r;
        }
        else {
            v = l & 0xff;
            if (!pal) {
                if ((v + 1) * 3 <= ilbm->cmap.size) {
                    r = ilbm->cmap.data[v * 3];
                    g = ilbm->cmap.data[v * 3 + 1];
                    b = ilbm->cmap.data[v * 3 + 2];
                }
                else r = g = b = 0;
            }
            else {
                r = scalegun(pal[v * 2] & 0x0f, 4);
                g = scalegun((pal[v * 2 + 1] & 0xf0) >> 4, 4);
                b = scalegun((pal[v * 2 + 1] & 0x0f), 4);
            }

            if (ilbm->mask == 2 && v == L2RWORD(ilbm->bmhd.data + 12)) a = 0x00;
        }

        row[x] = ((unsigned long)a << 24) | ((unsigned long)r << 16) | ((unsigned long)g << 8) | (unsigned long)b;

        bit = bit >> 1;
        if (bit == 0) {
            o++;
            bit = 0x80;
        }
    }
}


/*------------------------------------------------------------------------------
 * Loads an image. If im->loader is non-zero, or immediate_load is non-zero, or
 * progress is non-zero, then the file is fully loaded, otherwise only the width
 * and height are read.
 *
 * Imlib2 doesn't support reading comment chunks like ANNO.
 *------------------------------------------------------------------------------*/
char    load(ImlibImage *im, ImlibProgressFunction progress, char progress_granularity, char immediate_load)
{
char    *env;
int     cancel, full, i, n, ok, y, z, gran, nexty, prevy;
unsigned char *plane[40];
ILBM    ilbm;

  /*----------
   * Do nothing if the data is already loaded.
   *----------*/
    if (im->data) return 0;

  /*----------
   * Load the chunk(s) we're interested in. If full is not true, then we only
   * want the image size and format.
   *----------*/
    full = (im->loader || immediate_load || progress);
    ok = loadchunks(im->real_file, &ilbm, full);
    if (!ok) return 0;

  /*----------
   * Use and check header.
   *----------*/
    ok = 0;
    if (ilbm.bmhd.size >= 20) {
        ok = 1;

        im->w = L2RWORD(ilbm.bmhd.data);
        im->h = L2RWORD(ilbm.bmhd.data + 2);
	if (!IMAGE_DIMENSIONS_OK(im->w, im->h))
	  {
	     ok = 0;
	  }

        ilbm.depth = ilbm.bmhd.data[8];
        if (ilbm.depth < 1 || (ilbm.depth > 8 && ilbm.depth != 24 && ilbm.depth != 32)) ok = 0; /* Only 1 to 8, 24, or 32 planes. */

        ilbm.rle = ilbm.bmhd.data[10];
        if (ilbm.rle < 0 || ilbm.rle > 1) ok = 0;       /* Only NONE or RLE compression. */

        ilbm.mask = ilbm.bmhd.data[9];

        if (ilbm.mask || ilbm.depth == 32) SET_FLAG(im->flags, F_HAS_ALPHA);
        else UNSET_FLAG(im->flags, F_HAS_ALPHA);

        env = getenv("IMLIB2_LBM_NOMASK");
        if (env && (!strcmp(env, "true") || !strcmp(env, "1") || !strcmp(env, "yes") || !strcmp(env, "on"))) UNSET_FLAG(im->flags, F_HAS_ALPHA);

        if (!im->format) im->format = strdup("lbm");

        ilbm.ham = 0;
        ilbm.hbrite = 0;
        if (ilbm.depth <= 8) {
            if (ilbm.camg.size == 4) {
                if (ilbm.camg.data[2] & 0x08) ilbm.ham = 1;
                if (ilbm.camg.data[3] & 0x80) ilbm.hbrite = 1;
            }
            else {                                      /* Only guess at ham and hbrite if CMAP is present. */
                if (ilbm.depth == 6 && full && ilbm.cmap.size >= 3 * 16) ilbm.ham = 1;
                if (full && !ilbm.ham && ilbm.depth > 1 && ilbm.cmap.size == 3 * (1 << (ilbm.depth - 1))) ilbm.hbrite = 1;
            }
        }
    }
    if (!full || !ok) {
        im->w = im->h = 0;
        freeilbm(&ilbm);
        return ok;
    }

  /*----------
   * The source data is planar. Each plane is an even number of bytes wide. If
   * masking type is 1, there is an extra plane that defines the mask. Scanlines
   * from each plane are interleaved, from top to bottom. The first plane is the
   * 0 bit.
   *----------*/
    ok = 0;
    cancel = 0;
    plane[0] = NULL;

    im->data = malloc(im->w * im->h * sizeof(DATA32));
    n = ilbm.depth;
    if (ilbm.mask == 1) n++;
    plane[0] = malloc(((im->w + 15) / 16) * 2 * n);
    if (im->data && plane[0]) {
        for (i = 1; i < n; i++) plane[i] = plane[i - 1] + ((im->w + 15) / 16) * 2;

        z = ((im->w + 15) / 16) * 2 * n;

        if (progress) {
            prevy = 0;
            if (progress_granularity <= 0) progress_granularity = 1;
            gran = progress_granularity;
            nexty = ((im->h * gran) / 100);
        }

        scalecmap(&ilbm);

        for (y = 0; y < im->h; y++) {
            bodyrow(plane[0], z, &ilbm);

            deplane(im->data + im->w * y, im->w, &ilbm, plane);
            ilbm.row++;

            if (progress && (y >= nexty || y == im->h - 1)) {
                if (!progress(im, (char)((100 * (y + 1)) / im->h), 0, prevy, im->w, y + 1)) {
                    cancel = 1;
                    break;
                }
                prevy = y;
                gran += progress_granularity;
                nexty = ((im->h * gran) / 100);
            }
        }

        ok = !cancel;
    }

  /*----------
   * We either had a successful decode, the user cancelled, or we couldn't get
   * the memory for im->data or plane[0].
   *----------*/
    if (!ok) {
        im->w = im->h = 0;
        if (im->data) free(im->data);
        im->data = NULL;
    }

    if (plane[0]) free(plane[0]);

    freeilbm(&ilbm);

    return (cancel)? 2: ok;
}


/*------------------------------------------------------------------------------
 * Perhaps save only in 32-bit format? The IFF ILBM format has pretty much gone
 * the way of the Amiga, who saves in this format any more?
 *------------------------------------------------------------------------------*/
#if 0
char    save(ImlibImage *im, ImlibProgressFunction progress, char progress_granularity)
{
    return 0;
}
#endif


/*------------------------------------------------------------------------------
 * Identifies the file extensions this loader handles. Standard code from other
 * loaders.
 *------------------------------------------------------------------------------*/
void    formats(ImlibLoader *l)
{
char    *list_formats[] = { "iff", "ilbm", "lbm" };
int     i;

    l->num_formats = sizeof(list_formats) / sizeof(list_formats[0]);
    l->formats = malloc(l->num_formats * sizeof(list_formats[0]));
    for (i = 0; i < l->num_formats; i++) l->formats[i] = strdup(list_formats[i]);
}
