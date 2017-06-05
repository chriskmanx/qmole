/*
 * Geeqie
 * (C) 2006 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define _XOPEN_SOURCE

#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <math.h>

#ifdef HAVE_LCMS
/*** color support enabled ***/

#ifdef HAVE_LCMS_LCMS_H
  #include <lcms/lcms.h>
#else
  #include <lcms.h>
#endif
#endif

#include <glib.h>

#include "intl.h"

#include "main.h"
#include "exif.h"

#include "filedata.h"
#include "filefilter.h"
#include "filecache.h"
#include "format_raw.h"
#include "ui_fileops.h"
#include "cache.h"
#include "jpeg_parser.h"


static gdouble exif_rational_to_double(ExifRational *r, gint sign)
{
	if (!r || r->den == 0.0) return 0.0;

	if (sign) return (gdouble)((gint)r->num) / (gdouble)((gint)r->den);
	return (gdouble)r->num / r->den;
}

static gdouble exif_get_rational_as_double(ExifData *exif, const gchar *key)
{
	ExifRational *r;
	gint sign;

	r = exif_get_rational(exif, key, &sign);
	return exif_rational_to_double(r, sign);
}

static GString *append_comma_text(GString *string, const gchar *text)
{
	string = g_string_append(string, ", ");
	string = g_string_append(string, text);

	return string;
}

static gchar *remove_common_prefix(gchar *s, gchar *t)
{
	gint i;

	if (!s || !t) return t;

	for (i = 0; s[i] && t[i] && s[i] == t[i]; i++)
		;
	if (!i)
		return t;
	if (s[i-1] == ' ' || !s[i])
		{
		while (t[i] == ' ')
			i++;
		return t + i;
		}
	return s;
}

static gdouble get_crop_factor(ExifData *exif)
{
	gdouble res_unit_tbl[] = {0.0, 25.4, 25.4, 10.0, 1.0, 0.001 };
	gdouble xres = exif_get_rational_as_double(exif, "Exif.Photo.FocalPlaneXResolution");
	gdouble yres = exif_get_rational_as_double(exif, "Exif.Photo.FocalPlaneYResolution");
	gint res_unit;
	gint w, h;
	gdouble xsize, ysize, size, ratio;

	if (xres == 0.0 || yres == 0.0) return 0.0;

	if (!exif_get_integer(exif, "Exif.Photo.FocalPlaneResolutionUnit", &res_unit)) return 0.0;
	if (res_unit < 1 || res_unit > 5) return 0.0;

	if (!exif_get_integer(exif, "Exif.Photo.PixelXDimension", &w)) return 0.0;
	if (!exif_get_integer(exif, "Exif.Photo.PixelYDimension", &h)) return 0.0;

	xsize = w * res_unit_tbl[res_unit] / xres;
	ysize = h * res_unit_tbl[res_unit] / yres;

	ratio = xsize / ysize;

	if (ratio < 0.5 || ratio > 2.0) return 0.0; /* reasonable ratio */

	size = sqrt(xsize * xsize + ysize * ysize);

	if (size < 1.0 || size > 100.0) return 0.0; /* reasonable sensor size in mm */

	return sqrt(36*36+24*24) / size;

}

static gboolean remove_suffix(gchar *str, const gchar *suffix, gint suffix_len)
{
	gint str_len = strlen(str);
	
	if (suffix_len < 0) suffix_len = strlen(suffix);
	if (str_len < suffix_len) return FALSE;
	
	if (strcmp(str + str_len - suffix_len, suffix) != 0) return FALSE;
	str[str_len - suffix_len] = '\0';
	
	return TRUE;
}

static gchar *exif_build_formatted_Camera(ExifData *exif)
{
	gchar *text;
	gchar *make = exif_get_data_as_text(exif, "Exif.Image.Make");
	gchar *model = exif_get_data_as_text(exif, "Exif.Image.Model");
	gchar *software = exif_get_data_as_text(exif, "Exif.Image.Software");
	gchar *model2;
	gchar *software2;

	if (make)
		{
		g_strstrip(make);

		if (remove_suffix(make, " CORPORATION", 12)) { /* Nikon */ }
		else if (remove_suffix(make, " Corporation", 12)) { /* Pentax */ }
		else if (remove_suffix(make, " OPTICAL CO.,LTD", 16)) { /* OLYMPUS */ };
		}

	if (model)
		g_strstrip(model);

	if (software)
		{
		gint i, j;

		g_strstrip(software);
		
		/* remove superfluous spaces (pentax K100D) */
		for (i = 0, j = 0; software[i]; i++, j++)
			{
			if (software[i] == ' ' && software[i + 1] == ' ')
				i++;
			if (i != j) software[j] = software[i];
			}
		software[j] = '\0';
		}

	model2 = remove_common_prefix(make, model);
	software2 = remove_common_prefix(model2, software);

	text = g_strdup_printf("%s%s%s%s%s%s", (make) ? make : "", (make && model2) ? " " : "",
					       (model2) ? model2 : "",
					       (software2 && (make || model2)) ? " (" : "",
					       (software2) ? software2 : "",
					       (software2 && (make || model2)) ? ")" : "");

	g_free(make);
	g_free(model);
	g_free(software);
	return text;
}

static gchar *exif_build_formatted_DateTime(ExifData *exif)
{
	gchar *text = exif_get_data_as_text(exif, "Exif.Photo.DateTimeOriginal");
	gchar *subsec = NULL;
	gchar buf[128];
	gchar *tmp;
	gint buflen;
	struct tm tm;
	GError *error = NULL;

	if (text)
		{
		subsec = exif_get_data_as_text(exif, "Exif.Photo.SubSecTimeOriginal");
		}
	else
		{
		text = exif_get_data_as_text(exif, "Exif.Image.DateTime");
		if (text) subsec = exif_get_data_as_text(exif, "Exif.Photo.SubSecTime");
		}

	/* Convert the stuff into a tm struct */
	memset(&tm, 0, sizeof(tm)); /* Uh, strptime could let garbage in tm! */
	if (text && strptime(text, "%Y:%m:%d %H:%M:%S", &tm))
		{
		buflen = strftime(buf, sizeof(buf), "%x %X", &tm);
		if (buflen > 0)
			{
			tmp = g_locale_to_utf8(buf, buflen, NULL, NULL, &error);
			if (error)
				{
				log_printf("Error converting locale strftime to UTF-8: %s\n", error->message);
				g_error_free(error);
				}
			else
				{
				g_free(text);
				text = g_strdup(tmp);
				}
			}
		}

	if (subsec)
		{
		tmp = text;
		text = g_strconcat(tmp, ".", subsec, NULL);
		g_free(tmp);
		g_free(subsec);
		}
	return text;
}

static gchar *exif_build_formatted_ShutterSpeed(ExifData *exif)
{
	ExifRational *r;

	r = exif_get_rational(exif, "Exif.Photo.ExposureTime", NULL);
	if (r && r->num && r->den)
		{
		gdouble n = (gdouble)r->den / (gdouble)r->num;
		return g_strdup_printf("%s%.0fs", n > 1.0 ? "1/" : "",
						  n > 1.0 ? n : 1.0 / n);
		}
	r = exif_get_rational(exif, "Exif.Photo.ShutterSpeedValue", NULL);
	if (r && r->num  && r->den)
		{
		gdouble n = pow(2.0, exif_rational_to_double(r, TRUE));

		/* Correct exposure time to avoid values like 1/91s (seen on Minolta DImage 7) */
		if (n > 1.0 && (gint)n - ((gint)(n/10))*10 == 1) n--;

		return g_strdup_printf("%s%.0fs", n > 1.0 ? "1/" : "",
						  n > 1.0 ? floor(n) : 1.0 / n);
		}
	return NULL;
}

static gchar *exif_build_formatted_Aperture(ExifData *exif)
{
	gdouble n;

	n = exif_get_rational_as_double(exif, "Exif.Photo.FNumber");
	if (n == 0.0) n = exif_get_rational_as_double(exif, "Exif.Photo.ApertureValue");
	if (n == 0.0) return NULL;

	return g_strdup_printf("f/%.1f", n);
}

static gchar *exif_build_formatted_ExposureBias(ExifData *exif)
{
	ExifRational *r;
	gint sign;
	gdouble n;

	r = exif_get_rational(exif, "Exif.Photo.ExposureBiasValue", &sign);
	if (!r) return NULL;

	n = exif_rational_to_double(r, sign);
	return g_strdup_printf("%+.1f", n);
}

static gchar *exif_build_formatted_FocalLength(ExifData *exif)
{
	gdouble n;

	n = exif_get_rational_as_double(exif, "Exif.Photo.FocalLength");
	if (n == 0.0) return NULL;
	return g_strdup_printf("%.0f mm", n);
}

static gchar *exif_build_formatted_FocalLength35mmFilm(ExifData *exif)
{
	gint n;
	gdouble f, c;

	if (exif_get_integer(exif, "Exif.Photo.FocalLengthIn35mmFilm", &n) && n != 0)
		{
		return g_strdup_printf("%d mm", n);
		}

	f = exif_get_rational_as_double(exif, "Exif.Photo.FocalLength");
	if (f == 0.0) return NULL;

	c = get_crop_factor(exif);
	if (c == 0.0) return NULL;

	return g_strdup_printf("%.0f mm", f * c);
}

static gchar *exif_build_formatted_ISOSpeedRating(ExifData *exif)
{
	gchar *text;

	text = exif_get_data_as_text(exif, "Exif.Photo.ISOSpeedRatings");
	/* kodak may set this instead */
	if (!text) text = exif_get_data_as_text(exif, "Exif.Photo.ExposureIndex");
	return text;
}

static gchar *exif_build_formatted_SubjectDistance(ExifData *exif)
{
	ExifRational *r;
	gint sign;
	gdouble n;

	r = exif_get_rational(exif, "Exif.Photo.SubjectDistance", &sign);
	if (!r) return NULL;

	if ((glong)r->num == (glong)0xffffffff) return g_strdup(_("infinity"));
	if ((glong)r->num == 0) return g_strdup(_("unknown"));

	n = exif_rational_to_double(r, sign);
	if (n == 0.0) return _("unknown");
	return g_strdup_printf("%.3f m", n);
}

static gchar *exif_build_formatted_Flash(ExifData *exif)
{
	/* grr, flash is a bitmask... */
	GString *string;
	gchar *text;
	gint n;
	gint v;

	if (!exif_get_integer(exif, "Exif.Photo.Flash", &n)) return NULL;

	/* Exif 2.1 only defines first 3 bits */
	if (n <= 0x07) return exif_get_data_as_text(exif, "Exif.Photo.Flash");

	/* must be Exif 2.2 */
	string = g_string_new("");

	/* flash fired (bit 0) */
	string = g_string_append(string, (n & 0x01) ? _("yes") : _("no"));

	/* flash mode (bits 3, 4) */
	v = (n >> 3) & 0x03;
	if (v) string = append_comma_text(string, _("mode:"));
	switch (v)
		{
		case 1:
			string = g_string_append(string, _("on"));
			break;
		case 2:
			string = g_string_append(string, _("off"));
			break;
		case 3:
			string = g_string_append(string, _("auto"));
			break;
		}

	/* return light (bits 1, 2) */
	v = (n >> 1) & 0x03;
	if (v == 2) string = append_comma_text(string, _("not detected by strobe"));
	if (v == 3) string = append_comma_text(string, _("detected by strobe"));

	/* we ignore flash function (bit 5) */

	/* red-eye (bit 6) */
	if ((n >> 5) & 0x01) string = append_comma_text(string, _("red-eye reduction"));

	text = string->str;
	g_string_free(string, FALSE);
	return text;
}

static gchar *exif_build_formatted_Resolution(ExifData *exif)
{
	ExifRational *rx, *ry;
	gchar *units;
	gchar *text;

	rx = exif_get_rational(exif, "Exif.Image.XResolution", NULL);
	ry = exif_get_rational(exif, "Exif.Image.YResolution", NULL);
	if (!rx || !ry) return NULL;

	units = exif_get_data_as_text(exif, "Exif.Image.ResolutionUnit");
	text = g_strdup_printf("%0.f x %0.f (%s/%s)", rx->den ? (gdouble)rx->num / rx->den : 1.0,
						      ry->den ? (gdouble)ry->num / ry->den : 1.0,
						      _("dot"), (units) ? units : _("unknown"));

	g_free(units);
	return text;
}

static gchar *exif_build_formatted_ColorProfile(ExifData *exif)
{
	const gchar *name = "";
	const gchar *source = "";
	guchar *profile_data;
	guint profile_len;

	profile_data = exif_get_color_profile(exif, &profile_len);
	if (!profile_data)
		{
		gint cs;
		gchar *interop_index;

		/* ColorSpace == 1 specifies sRGB per EXIF 2.2 */
		if (!exif_get_integer(exif, "Exif.Photo.ColorSpace", &cs)) cs = 0;
		interop_index = exif_get_data_as_text(exif, "Exif.Iop.InteroperabilityIndex");

		if (cs == 1)
			{
			name = _("sRGB");
			source = "ColorSpace";
			}
		else if (cs == 2 || (interop_index && !strcmp(interop_index, "R03")))
			{
			name = _("AdobeRGB");
			source = (cs == 2) ? "ColorSpace" : "Iop";
			}

		g_free(interop_index);
		}
	else
		{
		source = _("embedded");
#ifdef HAVE_LCMS

			{
			cmsHPROFILE profile;

			profile = cmsOpenProfileFromMem(profile_data, profile_len);
			if (profile)
				{
				name = cmsTakeProductName(profile);
				cmsCloseProfile(profile);
				}
			g_free(profile_data);
			}
#endif
		}
	if (name[0] == 0 && source[0] == 0) return NULL;
	return g_strdup_printf("%s (%s)", name, source);
}

static gchar *exif_build_formatted_GPSPosition(ExifData *exif)
{
	GString *string;
	gchar *text, *ref;
	ExifRational *value;
	ExifItem *item;
	guint i;
	gdouble p, p3;
	gulong p1, p2;

	string = g_string_new("");

	item = exif_get_item(exif, "Exif.GPSInfo.GPSLatitude");
	ref = exif_get_data_as_text(exif, "Exif.GPSInfo.GPSLatitudeRef");
	if (item && ref)
		{
		p = 0;
		for (i = 0; i < exif_item_get_elements(item); i++)
			{
			value = exif_item_get_rational(item, NULL, i);
			if (value && value->num && value->den)
				p += (gdouble)value->num / (gdouble)value->den / pow(60.0, (gdouble)i);
			}
		p1 = (gint)p;
		p2 = (gint)((p - p1)*60);
		p3 = ((p - p1)*60 - p2)*60;

		g_string_append_printf(string, "%0lu° %0lu' %0.2f\" %.1s", p1, p2, p3, ref);
		} // if (item && ref)

	item = exif_get_item(exif, "Exif.GPSInfo.GPSLongitude");
	ref = exif_get_data_as_text(exif, "Exif.GPSInfo.GPSLongitudeRef");
	if (item && ref)
		{
		p = 0;
		for (i = 0; i < exif_item_get_elements(item); i++)
			{
			value = exif_item_get_rational(item, NULL, i);
			if (value && value->num && value->den)
			p += (gdouble)value->num / (gdouble)value->den / pow(60.0, (gdouble)i);
			}
		p1 = (gint)p;
		p2 = (gint)((p - p1)*60);
		p3 = ((p - p1)*60 - p2)*60;

		g_string_append_printf(string, ", %0lu° %0lu' %0.2f\" %.1s", p1, p2, p3, ref);
		} // if (item && ref)

	text = string->str;
	g_string_free(string, FALSE);

	return text;
} // static gchar *exif_build_forma...

static gchar *exif_build_formatted_GPSAltitude(ExifData *exif)
{
	ExifRational *r;
	ExifItem *item;
	gdouble alt;
	gint ref;

	item = exif_get_item(exif, "Exif.GPSInfo.GPSAltitudeRef");
	r = exif_get_rational(exif, "Exif.GPSInfo.GPSAltitude", NULL);

	if (!r || !item) return NULL;

	alt = exif_rational_to_double(r, 0);
	exif_item_get_integer(item, &ref);

	return g_strdup_printf("%0.f m %s", alt, (ref==0)?_("Above Sea Level"):_("Below Sea Level"));
}


/* List of custom formatted pseudo-exif tags */
#define EXIF_FORMATTED_TAG(name, label) { EXIF_FORMATTED()#name, label, exif_build_formatted##_##name }

ExifFormattedText ExifFormattedList[] = {
	EXIF_FORMATTED_TAG(Camera,		N_("Camera")),
	EXIF_FORMATTED_TAG(DateTime,		N_("Date")),
	EXIF_FORMATTED_TAG(ShutterSpeed,	N_("Shutter speed")),
	EXIF_FORMATTED_TAG(Aperture,		N_("Aperture")),
	EXIF_FORMATTED_TAG(ExposureBias,	N_("Exposure bias")),
	EXIF_FORMATTED_TAG(ISOSpeedRating,	N_("ISO sensitivity")),
	EXIF_FORMATTED_TAG(FocalLength,		N_("Focal length")),
	EXIF_FORMATTED_TAG(FocalLength35mmFilm,	N_("Focal length 35mm")),
	EXIF_FORMATTED_TAG(SubjectDistance,	N_("Subject distance")),
	EXIF_FORMATTED_TAG(Flash,		N_("Flash")),
	EXIF_FORMATTED_TAG(Resolution,		N_("Resolution")),
	EXIF_FORMATTED_TAG(ColorProfile,	N_("Color profile")),
	EXIF_FORMATTED_TAG(GPSPosition,		N_("GPS position")),
	EXIF_FORMATTED_TAG(GPSAltitude,		N_("GPS altitude")),
	{"file.size",				N_("File size"), 	NULL},
	{"file.date",				N_("File date"), 	NULL},
	{"file.mode",				N_("File mode"), 	NULL},
	{ NULL, NULL, NULL }
};

gchar *exif_get_formatted_by_key(ExifData *exif, const gchar *key, gboolean *key_valid)
{
	if (strncmp(key, EXIF_FORMATTED(), EXIF_FORMATTED_LEN) == 0)
		{
		gint i;

		if (key_valid) *key_valid = TRUE;

		key += EXIF_FORMATTED_LEN;
		for (i = 0; ExifFormattedList[i].key; i++)
			if (ExifFormattedList[i].build_func && strcmp(key, ExifFormattedList[i].key + EXIF_FORMATTED_LEN) == 0)
				return ExifFormattedList[i].build_func(exif);
		}

	if (key_valid) *key_valid = FALSE;
	return NULL;
}

gchar *exif_get_description_by_key(const gchar *key)
{
	if (!key) return NULL;

	if (strncmp(key, EXIF_FORMATTED(), EXIF_FORMATTED_LEN) == 0 ||
	    strncmp(key, "file.", 5) == 0)
		{
		gint i;

		for (i = 0; ExifFormattedList[i].key; i++)
			if (strcmp(key, ExifFormattedList[i].key) == 0)
				return g_strdup(_(ExifFormattedList[i].description));
		}

	return exif_get_tag_description_by_key(key);
}

gint exif_get_integer(ExifData *exif, const gchar *key, gint *value)
{
	ExifItem *item;

	item = exif_get_item(exif, key);
	return exif_item_get_integer(item, value);
}

ExifRational *exif_get_rational(ExifData *exif, const gchar *key, gint *sign)
{
	ExifItem *item;

	item = exif_get_item(exif, key);
	return exif_item_get_rational(item, sign, 0);
}

gchar *exif_get_data_as_text(ExifData *exif, const gchar *key)
{
	ExifItem *item;
	gchar *text;
	gboolean key_valid;

	if (!key) return NULL;

	text = exif_get_formatted_by_key(exif, key, &key_valid);
	if (key_valid) return text;

	item = exif_get_item(exif, key);
	if (item) return exif_item_get_data_as_text(item);

	return NULL;
}


static FileCacheData *exif_cache;

void exif_release_cb(FileData *fd)
{
	exif_free(fd->exif);
	fd->exif = NULL;
}

void exif_init_cache(void)
{
	g_assert(!exif_cache);
	exif_cache = file_cache_new(exif_release_cb, 4);
}

ExifData *exif_read_fd(FileData *fd)
{
	gchar *sidecar_path;
	
	if (!exif_cache) exif_init_cache();

	if (!fd) return NULL;
	
	if (file_cache_get(exif_cache, fd)) return fd->exif;
	g_assert(fd->exif == NULL);
	
	/* CACHE_TYPE_XMP_METADATA file should exist only if the metadata are
	 * not writable directly, thus it should contain the most up-to-date version */
	sidecar_path = NULL;

#ifdef HAVE_EXIV2
	/* we are not able to handle XMP sidecars without exiv2 */
	sidecar_path = cache_find_location(CACHE_TYPE_XMP_METADATA, fd->path);

	if (!sidecar_path) sidecar_path = file_data_get_sidecar_path(fd, TRUE);
#endif

	fd->exif = exif_read(fd->path, sidecar_path, fd->modified_xmp);

	g_free(sidecar_path);
	file_cache_put(exif_cache, fd, 1);
	return fd->exif;
}


void exif_free_fd(FileData *fd, ExifData *exif)
{
	if (!fd) return;
	g_assert(fd->exif == exif);
}

/* embedded icc in jpeg */

gboolean exif_jpeg_parse_color(ExifData *exif, guchar *data, guint size)
{
	guint seg_offset = 0;
	guint seg_length = 0;
	guint chunk_offset[255];
	guint chunk_length[255];
	guint chunk_count = 0;

	/* For jpeg/jfif, ICC color profile data can be in more than one segment.
	   the data is in APP2 data segments that start with "ICC_PROFILE\x00\xNN\xTT"
	   NN = segment number for data
	   TT = total number of ICC segments (TT in each ICC segment should match)
	 */

	while (jpeg_segment_find(data + seg_offset + seg_length,
				      size - seg_offset - seg_length,
				      JPEG_MARKER_APP2,
				      "ICC_PROFILE\x00", 12,
				      &seg_offset, &seg_length))
		{
		guchar chunk_num;
		guchar chunk_tot;

		if (seg_length < 14) return FALSE;

		chunk_num = data[seg_offset + 12];
		chunk_tot = data[seg_offset + 13];

		if (chunk_num == 0 || chunk_tot == 0) return FALSE;

		if (chunk_count == 0)
			{
			guint i;

			chunk_count = (guint)chunk_tot;
			for (i = 0; i < chunk_count; i++) chunk_offset[i] = 0;
			for (i = 0; i < chunk_count; i++) chunk_length[i] = 0;
			}

		if (chunk_tot != chunk_count ||
		    chunk_num > chunk_count) return FALSE;

		chunk_num--;
		chunk_offset[chunk_num] = seg_offset + 14;
		chunk_length[chunk_num] = seg_length - 14;
		}

	if (chunk_count > 0)
		{
		guchar *cp_data;
		guint cp_length = 0;
		guint i;

		for (i = 0; i < chunk_count; i++) cp_length += chunk_length[i];
		cp_data = g_malloc(cp_length);

		for (i = 0; i < chunk_count; i++)
			{
			if (chunk_offset[i] == 0)
				{
				/* error, we never saw this chunk */
				g_free(cp_data);
				return FALSE;
				}
			memcpy(cp_data, data + chunk_offset[i], chunk_length[i]);
			}
		DEBUG_1("Found embedded icc profile in jpeg");
		exif_add_jpeg_color_profile(exif, cp_data, cp_length);

		return TRUE;
		}

	return FALSE;
}

/*
 *-------------------------------------------------------------------
 * file info
 * it is here because it shares tag neming infrastructure with exif
 * we should probably not invest too much effort into this because
 * new exiv2 will support the same functionality
 * http://dev.exiv2.org/issues/show/505
 *-------------------------------------------------------------------
 */

static gchar *mode_number(mode_t m)
{
	gint mb, mu, mg, mo;
	gchar pbuf[12];

	mb = mu = mg = mo = 0;

	if (m & S_ISUID) mb |= 4;
	if (m & S_ISGID) mb |= 2;
	if (m & S_ISVTX) mb |= 1;

	if (m & S_IRUSR) mu |= 4;
	if (m & S_IWUSR) mu |= 2;
	if (m & S_IXUSR) mu |= 1;

	if (m & S_IRGRP) mg |= 4;
	if (m & S_IWGRP) mg |= 2;
	if (m & S_IXGRP) mg |= 1;

	if (m & S_IROTH) mo |= 4;
	if (m & S_IWOTH) mo |= 2;
	if (m & S_IXOTH) mo |= 1;

	pbuf[0] = (m & S_IRUSR) ? 'r' : '-';
	pbuf[1] = (m & S_IWUSR) ? 'w' : '-';
	pbuf[2] = (m & S_IXUSR) ? 'x' : '-';
	pbuf[3] = (m & S_IRGRP) ? 'r' : '-';
	pbuf[4] = (m & S_IWGRP) ? 'w' : '-';
	pbuf[5] = (m & S_IXGRP) ? 'x' : '-';
	pbuf[6] = (m & S_IROTH) ? 'r' : '-';
	pbuf[7] = (m & S_IWOTH) ? 'w' : '-';
	pbuf[8] = (m & S_IXOTH) ? 'x' : '-';
	pbuf[9] = '\0';

	return g_strdup_printf("%s (%d%d%d%d)", pbuf, mb, mu, mg, mo);
}

gchar *metadata_file_info(FileData *fd, const gchar *key, MetadataFormat format)
{
	if (strcmp(key, "file.size") == 0)
		{
		return g_strdup_printf("%ld", (long)fd->size);
		}
	if (strcmp(key, "file.date") == 0)
		{
		return g_strdup(text_from_time(fd->date));
		}
	if (strcmp(key, "file.mode") == 0)
		{
		return mode_number(fd->mode);
		}
	return g_strdup("");
}


/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
