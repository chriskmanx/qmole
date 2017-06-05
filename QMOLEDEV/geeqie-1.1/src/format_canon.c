/*
 * Geeqie
 * (C) 2005 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 *
 *
 * Code to add support for Canon CR2 and CRW files, version 0.2
 *
 * Developed by Daniel M. German, dmgerman at uvic.ca
 *
 * you can find the sources for this patch at http://turingmachine.org/~dmg/libdcraw/gqview/
 *
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifndef HAVE_EXIV2

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <glib.h>

#include "intl.h"

#include "main.h"
#include "format_canon.h"
#include "format_raw.h"

#include "exif.h"


/*
 *-----------------------------------------------------------------------------
 * Raw (CR2, CRW) embedded jpeg extraction for Canon
 *-----------------------------------------------------------------------------
 */

static gboolean canon_cr2_tiff_entry(guchar *data, const guint len, guint offset, ExifByteOrder bo,
				     guint *image_offset, gint *jpeg_encoding)
{
	guint tag;
	guint type;
	guint count;
	guint jpeg_start;

	/* the two (tiff compliant) tags we want are:
	 *  0x0103 image compression type (must be type 6 for jpeg)
	 *  0x0111 jpeg start offset
	 * only use the first segment that contains an actual jpeg - as there
	 * is a another that contains the raw data.
	 */
	tag = exif_byte_get_int16(data + offset + EXIF_TIFD_OFFSET_TAG, bo);
	type = exif_byte_get_int16(data + offset + EXIF_TIFD_OFFSET_FORMAT, bo);
	count = exif_byte_get_int32(data + offset + EXIF_TIFD_OFFSET_COUNT, bo);

	/* tag 0x0103 contains the compression type for this segment's image data */
	if (tag == 0x0103)
		{
		if (ExifFormatList[type].size * count == 2 &&
		    exif_byte_get_int16(data + offset + EXIF_TIFD_OFFSET_DATA, bo) == 6)
			{
			*jpeg_encoding = TRUE;
			}
		return FALSE;
		}

	/* find and verify jpeg offset */
	if (tag != 0x0111 ||
	    !jpeg_encoding) return FALSE;

	/* make sure data segment contains 4 bytes */
	if (ExifFormatList[type].size * count != 4) return FALSE;

	jpeg_start = exif_byte_get_int32(data + offset + EXIF_TIFD_OFFSET_DATA, bo);

	/* verify this is jpeg data */
	if (len < jpeg_start + 4 ||
	    memcmp(data + jpeg_start, "\xff\xd8", 2) != 0)
		{
		return FALSE;
		}

	*image_offset = jpeg_start;
	return TRUE;
}

static gint canon_cr2_tiff_table(guchar *data, const guint len, guint offset, ExifByteOrder bo,
				 guint *image_offset)
{
	gboolean jpeg_encoding = FALSE;
	guint count;
	guint i;

	if (len < offset + 2) return 0;

	count = exif_byte_get_int16(data + offset, bo);
	offset += 2;
	if (len < offset + count * EXIF_TIFD_SIZE + 4) return 0;

	for (i = 0; i < count; i++)
		{
		if (canon_cr2_tiff_entry(data, len, offset + i * EXIF_TIFD_SIZE, bo,
					 image_offset, &jpeg_encoding))
			{
			return 0;
			}
		}

	return exif_byte_get_int32(data + offset + count * EXIF_TIFD_SIZE, bo);
}

gboolean format_canon_raw_cr2(guchar *data, const guint len,
			      guint *image_offset, guint *exif_offset)
{
	guint jpeg_offset = 0;
	ExifByteOrder bo;
	guint offset;
	gint level;

	/* cr2 files are tiff files with a few canon specific directory tags
	 * they are (always ?) in little endian format
	 */
	if (!exif_tiff_directory_offset(data, len, &offset, &bo)) return FALSE;

	level = 0;
	while (offset && level < EXIF_TIFF_MAX_LEVELS)
		{
		offset = canon_cr2_tiff_table(data, len, offset, bo, &jpeg_offset);
		level++;

		if (jpeg_offset != 0)
			{
			if (image_offset) *image_offset = jpeg_offset;
			return TRUE;
			}
		}

	return FALSE;
}

#define CRW_BYTE_ORDER		EXIF_BYTE_ORDER_INTEL
#define CRW_HEADER_SIZE		26
#define CRW_DIR_ENTRY_SIZE	10

gboolean format_canon_raw_crw(guchar *data, const guint len,
			      guint *image_offset, guint *exif_offset)
{
	guint block_offset;
	guint data_length;
	guint offset;
	guint count;
	guint i;

	/* CRW header starts with 2 bytes for byte order (always "II", little endian),
	 * 4 bytes for start of root block,
	 * and 8 bytes of magic for file type and format "HEAPCCDR"
	 * (also 4 bytes for file version, and 8 bytes reserved)
	 *
	 * CIFF specification in pdf format is available on some websites,
	 * search for "CIFFspecV1R03.pdf" or "CIFFspecV1R04.pdf"
	 */
	if (len < CRW_HEADER_SIZE ||
	    memcmp(data, "II", 2) != 0 ||
	    memcmp(data + 6, "HEAPCCDR", 8) != 0)
		{
		return FALSE;
		}

	block_offset = exif_byte_get_int32(data + 2, CRW_BYTE_ORDER);

	/* the end of the root block equals end of file,
	 * the last 4 bytes of the root block contain the block's data size
	 */
	offset = len - 4;
	data_length = exif_byte_get_int32(data + offset, CRW_BYTE_ORDER);

	offset = block_offset + data_length;
	if (len < offset + 2) return FALSE;

	/* number of directory entries for this block is in
	 * the next two bytes after the data for this block.
	 */
	count = exif_byte_get_int16(data + offset, CRW_BYTE_ORDER);
	offset += 2;
	if (len < offset + count * CRW_DIR_ENTRY_SIZE + 4) return FALSE;

	/* walk the directory entries looking for type jpeg (tag 0x2007),
	 * for reference, other tags are 0x2005 for raw and 0x300a for photo info:
	 */
	for (i = 0; i < count ; i++)
		{
		guint entry_offset;
		guint record_type;
		guint record_offset;
		guint record_length;

		entry_offset = offset + i * CRW_DIR_ENTRY_SIZE;

		/* entry is 10 bytes (in order):
		 *  2 for type
		 *  4 for length of data
		 *  4 for offset into data segment of this block
		 */
		record_type = exif_byte_get_int16(data + entry_offset, CRW_BYTE_ORDER);
		record_length = exif_byte_get_int32(data + entry_offset + 2, CRW_BYTE_ORDER);
		record_offset = exif_byte_get_int32(data + entry_offset + 6, CRW_BYTE_ORDER);

		/* tag we want for jpeg data */
		if (record_type == 0x2007)
			{
			guint jpeg_offset;

			jpeg_offset = block_offset + record_offset;
			if (len < jpeg_offset + record_length ||
			    record_length < 4 ||
			    memcmp(data + jpeg_offset, "\xff\xd8\xff\xdb", 4) != 0)
				{
				return FALSE;
				}

			/* we now know offset and verified jpeg */
			*image_offset = jpeg_offset;
			return TRUE;
			}
		}

	return FALSE;
}


/*
 *-----------------------------------------------------------------------------
 * EXIF Makernote for Canon
 *-----------------------------------------------------------------------------
 */

static ExifTextList CanonSet1MacroMode[] = {
	{ 1,	"macro" },
	{ 2,	"normal" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1Quality[] = {
	{ 2,	"normal" },
	{ 3,	"fine" },
	{ 4,	"raw" },
	{ 5,	"superfine" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1FlashMode[] = {
	{ 0,	"flash not fired" },
	{ 1,	"auto" },
	{ 2,	"on" },
	{ 3,	"red-eye reduction" },
	{ 4,	"slow sync" },
	{ 5,	"auto + red-eye reduction" },
	{ 6,	"on + red-eye reduction" },
	{ 16,	"external flash" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1DriveMode[] = {
	{ 0,	"single or timer" },
	{ 1,	"continuous" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1FocusMode[] = {
	{ 0,	"one-shot AF" },
	{ 1,	"AI servo AF" },
	{ 2,	"AI focus AF" },
	{ 3,	"manual" },
	{ 4,	"single" },
	{ 5,	"continuous" },
	{ 6,	"manual" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1ImageSize[] = {
	{ 0,	"large" },
	{ 1,	"medium" },
	{ 2,	"small" },
	/* where (or) does Medium 1/2 fit in here ? */
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1ShootingMode[] = {
	{ 0,	"auto" },
	{ 1,	"manual" },
	{ 2,	"landscape" },
	{ 3,	"fast shutter" },
	{ 4,	"slow shutter" },
	{ 5,	"night" },
	{ 6,	"black and white" },
	{ 7,	"sepia" },
	{ 8,	"portrait" },
	{ 9,	"sports" },
	{ 10,	"macro" },
	{ 11,	"pan focus" },
	EXIF_TEXT_LIST_END
};

/* Don't think this is interpreted correctly/completely, A60 at 2.5x Digital sets value of 3 */
static ExifTextList CanonSet1DigitalZoom[] = {
	{ 0,	"none" },
	{ 1,	"2x" },
	{ 2,	"4x" },
	{ 3,	"other" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1ConSatSharp[] = {
	{ 0,	"normal" },
	{ 1,	"high" },
	{ 65535,"low" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1ISOSpeed[] = {
/*	{ 0,	"not set/see EXIF tag" }, */
	{ 15,	"auto" },
	{ 16,	"50" },
	{ 17,	"100" },
	{ 18,	"200" },
	{ 19,	"400" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1MeteringMode[] = {
	{ 0,	"default" },
	{ 1,	"spot" },
	{ 3,	"evaluative" },
	{ 4,	"partial" },
	{ 5,	"center-weighted" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1FocusType[] = {
	{ 0,	"manual" },
	{ 1,	"auto" },
	{ 2,	"auto" },
	{ 3,	"macro" },
	{ 7,	"infinity" },
	{ 8,	"locked" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1AutoFocusPoint[] = {
	{ 0x2005,	"manual AF point selection" },
	{ 0x3000,	"manual focus" },
	{ 0x3001,	"auto" },
	{ 0x3002,	"right" },
	{ 0x3003,	"center" },
	{ 0x3004,	"left" },
	{ 0x4001,	"auto AF point selection" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1ExposureMode[] = {
	{ 0,	"auto" },
	{ 1,	"program" },
	{ 2,	"Tv priority" },
	{ 3,	"Av priority" },
	{ 4,	"manual" },
	{ 5,	"A-DEP" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1FlashFired[] = {
	{ 0,	"no" },
	{ 1,	"yes" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet1FocusCont[] = {
	{ 0,	"no (single)" },
	{ 1,	"yes" },
	EXIF_TEXT_LIST_END
};

static ExifMarker CanonSet1[] = {
/* 0 is length of array in bytes (2 x array size) */
{ 1,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.MacroMode",	"Macro mode",		CanonSet1MacroMode },
{ 2,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.SelfTimer",	"Self timer (10ths of second)", NULL },
{ 3,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.Quality",	"Quality",		CanonSet1Quality },
{ 4,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.FlashMode",	"Flash mode",		CanonSet1FlashMode },
{ 5,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.DriveMode",	"Drive mode",		CanonSet1DriveMode },
{ 7,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.FocusMode",	"Focus mode",		CanonSet1FocusMode },
{ 10,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.ImageSize",	"Image size",		CanonSet1ImageSize },
{ 11,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.ShootingMode","Shooting mode",	CanonSet1ShootingMode },
 { 11,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "ExposureProgram",	"ExposureProgram",	CanonSet1ShootingMode },
{ 12,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.DigitalZoom",	"Digital zoom",		CanonSet1DigitalZoom },
{ 13,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.Contrast",	"Contrast",		CanonSet1ConSatSharp },
{ 14,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.Saturation",	"Saturation",		CanonSet1ConSatSharp },
{ 15,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.Sharpness",	"Sharpness",		CanonSet1ConSatSharp },
{ 16,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.ISOSpeed",	"ISO speed",		CanonSet1ISOSpeed },
 { 16,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "ISOSpeedRatings",	"ISO speed",		CanonSet1ISOSpeed },
{ 17,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.MeteringMode","Metering mode",	CanonSet1MeteringMode },
{ 18,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.FocusType",	"Focus type",		CanonSet1FocusType },
{ 19,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.AutoFocus",	"AutoFocus point",	CanonSet1AutoFocusPoint },
{ 20,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.ExposureMode","Exposure mode",	CanonSet1ExposureMode },
 { 20,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "ExposureMode",		"Exposure mode",	CanonSet1ExposureMode },
{ 23,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.FocalLengthLong","Long focal length", NULL },
{ 24,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.FocalLengthShort","Short focal length", NULL },
{ 25,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.FocalLengthUnits","Focal units per mm", NULL },
{ 28,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.FlashFired",	"Flash fired",		CanonSet1FlashFired },
{ 29,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.FlashDetails","Flash details",	NULL },
{ 32,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.ContinuousFocus","Continuous focus",	CanonSet1FocusCont },
EXIF_MARKER_LIST_END
};

static ExifTextList CanonSet2WhiteBalance[] = {
	{ 0,	"auto" },
	{ 1,	"sunny" },
	{ 2,	"cloudy" },
	{ 3,	"tungsten" },
	{ 4,	"fluorescent" },
	{ 5,	"flash" },
	{ 6,	"custom" },
	{ 7,	"black and white" },
	{ 8,	"shade" },
	{ 9,	"manual" },
	{ 14,	"daylight fluorescent" },
	{ 17,	"underwater" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonSet2FlashBias[] = {
	{ 0x0000,	"0" },
	{ 0x000c,	"0.33" },
	{ 0x0010,	"0.5" },
	{ 0x0014,	"0.67" },
	{ 0x0020,	"1" },
	{ 0x002c,	"1.33" },
	{ 0x0030,	"1.5" },
	{ 0x0034,	"1.67" },
	{ 0x0040,	"2" },
	{ 0xffc0,	"-2" },
	{ 0xffcc,	"-1.67" },
	{ 0xffd0,	"-1.5" },
	{ 0xffd4,	"-1.33" },
	{ 0xffe0,	"-1" },
	{ 0xffec,	"-0.67" },
	{ 0xfff0,	"-0.5" },
	{ 0xfff4,	"-0.33" },
	EXIF_TEXT_LIST_END
};

static ExifMarker CanonSet2[] = {
/* 0 is length of array in bytes (2 x array size) */
{ 7,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.WhiteBalance","White balance",	CanonSet2WhiteBalance },
 { 7,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "LightSource",		"White balance",	CanonSet2WhiteBalance },
{ 9,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.SequenceNumber","Sequence number",	NULL },
{ 15,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.FlashBias",	"Flash bias",		CanonSet2FlashBias },
/* distance needs more than just this (metric) value */
{ 19,	EXIF_FORMAT_SHORT_UNSIGNED, 1, "MkN.Canon.SubjectDistance",	"Subject Distance", NULL },
EXIF_MARKER_LIST_END
};

#if 0

static ExifTextList CanonCustomEnable[] = {
	{ 0,	"off" },
	{ 1,	"on" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonCustomEnableInvert[] = {
	{ 0,	"on" },
	{ 1,	"off" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonCustomExposureLevel[] = {
	{ 0,	"1/2 stop" },
	{ 1,	"1/3 stop" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonCustomAVShutterSpeed[] = {
	{ 0,	"auto" },
	{ 1,	"1/200 (fixed)" },
	EXIF_TEXT_LIST_END
};

static ExifTextList CanonCustomShutterCurtainSync[] = {
	{ 0,	"1st" },
	{ 1,	"2nd" },
	EXIF_TEXT_LIST_END
};

static ExifMarker CanonCustom[] = {
{ 1,	EXIF_FORMAT_SHORT_UNSIGNED, 1,	"MkN.Canon.NoiseReduction", "Noise reduction",	CanonCustomEnable },
/*{ 2,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.BtnFuncShutter",
						"Shutter/Auto exposure button function",CanonCustomBTNShutter }, */
{ 3,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.MirrorLockup", "Mirror lockup",	CanonCustomEnable },
{ 4,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.TvAvExposureLevel",
							"Tv/Av and exposure level",	CanonCustomExposureLevel },
{ 5,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.AFAssistLight", "AF assist light",	CanonCustomEnableInvert },
{ 6,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.AvShutterSpeed",
							"Shutter speed in Av mode",	CanonCustomAVShutterSpeed },
/*{ 7,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.AutoBracket",
				"Auto-Exposure bracketting sequence/auto cancellation",	CanonCustom }, */
{ 8,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.ShutterSync", "Shutter sync",	CanonCustomShutterCurtainSync },
/* { 9,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.BtnFuncAF",	"AF button function",	CanonCustom }, */
{ 10,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.FillFlashReduction",
							"Fill flash auto reduction",	CanonCustomEnableInvert },
/*{ 11,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.BtnFuncMenu",
							"Menu button function",		CanonCustom }, */
/*{ 12,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.BtnFuncSet", "Set button function",	CanonCustom }, */
{ 13,	EXIF_FORMAT_SHORT_UNSIGNED, 1,  "MkN.Canon.SensorCleaning", "Sensor cleaning",	CanonCustomEnable },
EXIF_MARKER_LIST_END
};

#endif

static ExifMarker CanonExifMarkersList[] = {
	{ 1,	EXIF_FORMAT_SHORT_UNSIGNED, -1, "MkN.Canon.Settings1",		NULL, NULL },
	{ 4,	EXIF_FORMAT_SHORT_UNSIGNED, -1, "MkN.Canon.Settings2",		NULL, NULL },
	{ 6,	EXIF_FORMAT_STRING, -1,		"MkN.Canon.ImageType",		"Image type", NULL },
	{ 7,	EXIF_FORMAT_STRING, -1,		"MkN.Canon.FirmwareVersion",	"Firmware version", NULL },
	{ 8,	EXIF_FORMAT_LONG_UNSIGNED, 1,	"MkN.Canon.ImageNumber",	"Image number", NULL },
	{ 9,	EXIF_FORMAT_STRING, -1,		"MkN.Canon.OwnerName",		"Owner name", NULL },
	{ 12,	EXIF_FORMAT_LONG_UNSIGNED, -1,	"MkN.Canon.SerialNumber",	"Camera serial number", NULL },
	{ 15,	EXIF_FORMAT_SHORT_UNSIGNED, -1,	"MkN.Canon.CustomFunctions",	NULL, NULL },
	EXIF_MARKER_LIST_END
};

static void canon_mknote_parse_settings(ExifData *exif,
					guint16 *data, guint32 len, ExifByteOrder bo,
					ExifMarker *list)
{
	gint i;

	i = 0;
	while (list[i].tag != 0)
		{
		if (list[i].tag < len)
			{
			ExifItem *item;

			item = exif_item_new(EXIF_FORMAT_SHORT_UNSIGNED, list[i].tag, 1, &list[i]);
			exif_item_copy_data(item, &data[list[i].tag], 2, EXIF_FORMAT_SHORT_UNSIGNED, bo);
			exif->items = g_list_prepend(exif->items, item);
			}

		i++;
		}
}

#if 0
static void canon_mknote_parse_convert(ExifData *exif)
{
	gint value;
	ExifItem *result;

	/* seems we need more than only this value for distance */
	if (exif_get_integer(exif, "MkN.Canon.SubjectDistance", &value))
		{
		static ExifMarker marker= { 0x9206, EXIF_FORMAT_RATIONAL_UNSIGNED, 1,
					    "SubjectDistance", "Subject distance", NULL };
		ExifItem *item;
		ExifRational *rational;

		item = exif_item_new(marker.format, marker.tag, 1, &marker);
		rational = item->data;
		rational->num = value;
		rational->den = 100;

		exif->items = g_list_prepend(exif->items, item);
		}

	result = exif_get_item(exif, "MkN.Canon.SerialNumber");
	if (result && result->format == EXIF_FORMAT_LONG_UNSIGNED && result->data_len == 4)
		{
		static ExifMarker marker= { 12, EXIF_FORMAT_STRING, -1,
					    "SerialNumber", "Camera serial number", NULL };
		ExifItem *item;
		gchar *text;
		gint l;
		guint32 n;

		n = (guint32)((guint32 *)(result->data))[0];
		text = g_strdup_printf("%04X%05d", n & 0xffff0000 >> 8, n & 0x0000ffff);
		l = strlen(text) + 1;
		item = exif_item_new(marker.format, marker.tag, l, &marker);
		memcpy(item->data, text, l);
		g_free(text);

		exif->items = g_list_prepend(exif->items, item);
		}
}
#endif

gboolean format_canon_makernote(ExifData *exif, guchar *tiff, guint offset,
			        guint size, ExifByteOrder bo)
{
	ExifItem *item;

	if (exif_parse_IFD_table(exif, tiff, offset, size, bo, 0, CanonExifMarkersList) != 0)
		{
		return FALSE;
		}

	item = exif_get_item(exif, "MkN.Canon.Settings1");
	if (item)
		{
		canon_mknote_parse_settings(exif, item->data, item->data_len, bo, CanonSet1);
		}

	item = exif_get_item(exif, "MkN.Canon.Settings2");
	if (item)
		{
		canon_mknote_parse_settings(exif, item->data, item->data_len, bo, CanonSet2);
		}

#if 0
	canon_mknote_parse_convert(exif);
#endif

	return TRUE;
}


#endif
/* not HAVE_EXIV2 */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
