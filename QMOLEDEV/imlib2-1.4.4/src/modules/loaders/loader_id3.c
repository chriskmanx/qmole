#include "loader_common.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <id3tag.h>

#if ! defined (__STDC_VERSION__) || __STDC_VERSION__ < 199901L
# if __GNUC__ >= 2
#  define inline __inline__
# else
#  define inline
# endif
#endif

#ifdef __GNUC__
# define UNLIKELY(exp) __builtin_expect ((exp), 0)
#else
# define UNLIKELY(exp) (exp)
#endif

typedef struct context
{
	int id;
 	char* filename;
	struct id3_tag* tag;
	int refcount;
	struct context* next;
} context;

static context* id3_ctxs = NULL;

static inline struct id3_frame*
id3_tag_get_frame (struct id3_tag* tag, size_t index)
{
	return tag->frames[index];
}

static inline size_t id3_tag_get_numframes (struct id3_tag* tag)
{
	return tag->nframes;
}

static inline char const* id3_frame_id (struct id3_frame* frame)
{
	return frame->id;
}

static context* context_create (const char* filename)
{
	context* node = (context*) malloc (sizeof (context));
	context *ptr, *last;
	int last_id = INT_MAX;
	node->refcount = 1;
	{
		struct id3_file* file;
		struct id3_tag* tag;
		unsigned int i;
		file = id3_file_open (filename, ID3_FILE_MODE_READONLY);
		if (! file) {
			fprintf (stderr, "Unable to open tagged file %s: %s\n",
				 filename, strerror (errno));
			goto fail_free;
		}
		tag = id3_file_tag (file);
		if (! tag) {
			fprintf (stderr,
				 "Unable to find ID3v2 tags in file %s\n",
				 filename);
			id3_file_close (file);
			goto fail_free;
		}
		node->tag = id3_tag_new ();
		for (i = 0; i < id3_tag_get_numframes (tag); i ++)
			if (! strcmp (id3_frame_id
				      (id3_tag_get_frame (tag, i)), "APIC"))
				id3_tag_attachframe (node->tag,
						     id3_tag_get_frame (tag, i));
		id3_file_close (file);
	}
 	node->filename = strdup (filename);
	if (! id3_ctxs) {
		node->id = 1;
		node->next = NULL;
		id3_ctxs = node;
		return node;
	}
	ptr = id3_ctxs;
	last = NULL;
	while (UNLIKELY (ptr && (ptr->id + 1) >= last_id)) {
		last_id = ptr->id;
		last = ptr;
		ptr = ptr->next;
	}
	/* Paranoid! this can occur only if there are INT_MAX contexts :) */
	if (UNLIKELY (ptr == NULL)) {
		fprintf (stderr, "Too many open ID3 contexts\n");
		goto fail_close;
	}
	node->id = ptr->id + 1;
	if (UNLIKELY (last != NULL)) {
		node->next = last->next;
		last->next = node;
	} else {
		node->next = id3_ctxs;
		id3_ctxs = node;
	}
	return node;

fail_close:
	free (node->filename);
	id3_tag_delete (node->tag);
fail_free:
	free (node);
	return NULL;
}

static void context_destroy (context* ctx)
{
	id3_tag_delete (ctx->tag);
	free (ctx->filename);
	free (ctx);
}

static inline void context_addref (context* ctx)
{
	ctx->refcount ++;
}

static context* context_get (int id)
{
	context* ptr = id3_ctxs;
	while (ptr) {
		if (ptr->id == id) {
			context_addref (ptr);
			return ptr;
		}
		ptr = ptr->next;
	}
	fprintf (stderr, "No context by handle %d found\n", id);
	return NULL;
}

static context* context_get_by_name (const char* name)
{
	context* ptr = id3_ctxs;
	while (ptr) {
		if (! strcmp (name, ptr->filename)) {
			context_addref (ptr);
			return ptr;
		}
		ptr = ptr->next;
	}
	return NULL;
}

static void context_delref (context* ctx)
{
	ctx->refcount --;
	if (ctx->refcount <= 0) {
		context *last = NULL, *ptr = id3_ctxs;
		while (ptr) {
			if (ptr == ctx) {
				if (last)
					last->next = ctx->next;
				else
					id3_ctxs = ctx->next;
				context_destroy (ctx);
				return;
			}
			last = ptr;
			ptr = ptr->next;
		}
	}
}

static int str2int (char* str, int old)
{
	long index;
	errno = 0;
	index = strtol (str, NULL, 10);
	return ((errno || index > INT_MAX) ? old : (int)index);
}

static size_t str2uint (char* str, size_t old)
{
	unsigned long index;
	errno = 0;
	index = strtoul (str, NULL, 10);
	return ((errno || index > UINT_MAX) ? old : (size_t)index);
}

static void destructor_data (ImlibImage* im, void* data)
{
	free (data);
}

static void destructor_context (ImlibImage* im, void* data)
{
	context_delref ((context*)data);
}

typedef struct lopt
{
	context* ctx;
	size_t index;
	int traverse;
	char cache_level;
} lopt;

static char get_options (lopt* opt, ImlibImage* im)
{
	size_t handle = 0, index = 0, traverse = 0;
	context* ctx;

	if (im->key) {
		char* key = strdup (im->key);
		char* tok = strtok (key, ",");
		traverse = 0;
		while (tok) {
			char* value = strchr (tok, '=');
			if (! value) {
				value = tok;
				tok = "index";
			} else {
				*value = '\0';
				value ++;
			}
			if (! strcasecmp (tok, "index"))
				index = str2uint (value, index);
			else if (! strcasecmp (tok, "context"))
				handle = str2uint (value, handle);
			else if (! strcasecmp (tok, "traverse"))
				traverse = str2int (value, traverse);
			tok = strtok (NULL, ",");
		}
		free (key);
	} else
		traverse = 1;

	if (! handle) {
		ImlibImageTag* htag = __imlib_GetTag (im, "context");
		if (htag && htag->val)
			handle = htag->val;
	}
	if (handle)
		ctx = context_get (handle);
	else if (! (ctx = context_get_by_name (im->real_file)) &&
		 ! (ctx = context_create (im->real_file)))
		return 0;

	if (! index) {
		ImlibImageTag* htag = __imlib_GetTag (im, "index");
		if (htag && htag->val)
			index = htag->val;
	}
	if (index < 0 || index > id3_tag_get_numframes (ctx->tag) ||
	    (index == 0 && id3_tag_get_numframes (ctx->tag) < 1)) {
		if (index)
			fprintf (stderr, "No picture frame # %d found\n", index);
		context_delref (ctx);
		return 0;
	}
	if (! index)
		index = 1;

	opt->ctx = ctx;
	opt->index = index;
	opt->traverse = traverse;
	opt->cache_level = (id3_tag_get_numframes (ctx->tag) > 1 ? 1 : 0);
	return 1;
}

static int extract_pic (struct id3_frame* frame, int dest)
{
	union id3_field* field;
	unsigned char const * data;
	id3_length_t length;
	int done = 0;

	field = id3_frame_field (frame, 4);
	data = id3_field_getbinarydata (field, &length);
	if (! data) {
		fprintf (stderr, "No image data found for frame\n");
		return 0;
	}
	while (length > 0) {
		ssize_t res;
		if ((res = write (dest, data + done, length)) < 0) {
			if (errno == EINTR)
				continue;
			perror ("Unable to write to file");
			return 0;
		}
		length -= res;
		done += res;
	}
	return 1;
}

#define EXT_LEN 14

static char get_loader (lopt* opt, ImlibLoader** loader)
{
	union id3_field* field;
	char const * data;
	char ext[EXT_LEN + 2];

	ext[EXT_LEN + 1] = '\0';
	ext[0] = '.';

	field = id3_frame_field (id3_tag_get_frame (opt->ctx->tag,
						    opt->index - 1), 1);
	data = (char const*) id3_field_getlatin1 (field);
	if (! data) {
		fprintf (stderr, "No mime type data found for image frame\n");
		return 0;
	}
	if (strncasecmp (data, "image/", 6)) {
		if (! strcmp (data, "-->")) {
			*loader = NULL;
			return 1;
		}
		fprintf (stderr,
			 "Picture frame with unknown mime-type \'%s\' found\n",
			 data);
		return 0;
	}
	strncpy (ext + 1, data + 6, EXT_LEN);
	if (! (*loader = __imlib_FindBestLoaderForFile (ext, 0))) {
		fprintf (stderr, "No loader found for extension %s\n", ext);
		return 0;
	}
	return 1;
}

static char* id3_pic_types [] = {
	/* $00 */  "Other",
	/* $01 */  "32x32 pixels file icon",
	/* $02 */  "Other file icon",
	/* $03 */  "Cover (front)",
	/* $04 */  "Cover (back)",
	/* $05 */  "Leaflet page",
	/* $06 */  "Media",
	/* $07 */  "Lead artist/lead performer/soloist",
	/* $08 */  "Artist/performer",
	/* $09 */  "Conductor",
	/* $0A */  "Band/Orchestra",
	/* $0B */  "Composer",
	/* $0C */  "Lyricist/text writer",
	/* $0D */  "Recording Location",
	/* $0E */  "During recording",
	/* $0F */  "During performance",
	/* $10 */  "Movie/video screen capture",
	/* $11 */  "A bright coloured fish",
	/* $12 */  "Illustration",
	/* $13 */  "Band/artist logotype",
	/* $14 */  "Publisher/Studio logotype"
};

#define NUM_OF_ID3_PIC_TYPES \
    (sizeof(id3_pic_types) / sizeof(id3_pic_types[0]))

static char* id3_text_encodings [] = {
	/* $00 */  "ISO-8859-1",
	/* $01 */  "UTF-16 encoded Unicode with BOM",
	/* $02 */  "UTF-16BE encoded Unicode without BOM",
	/* $03 */  "UTF-8 encoded Unicode"
};

#define NUM_OF_ID3_TEXT_ENCODINGS \
    (sizeof(id3_text_encodings) / sizeof(id3_text_encodings[0]))

static void write_tags (ImlibImage* im, lopt* opt)
{
	struct id3_frame* frame
		= id3_tag_get_frame (opt->ctx->tag, opt->index - 1);
	union id3_field* field;
	int num_data;
	char* data;

	if ((field = id3_frame_field (frame, 1)) &&
	    (data = (char*) id3_field_getlatin1 (field)))
		__imlib_AttachTag (im, "mime-type", 0,
				   strdup (data), destructor_data);
	if ((field = id3_frame_field (frame, 3)) &&
	    (data = (char*) id3_field_getstring (field))) {
		size_t length;
		char* dup;
		id3_ucs4_t* ptr = (id3_ucs4_t*)data;
		while (*ptr)
			ptr ++;
		length = (ptr - (id3_ucs4_t*)data + 1) * sizeof (id3_ucs4_t);
		dup = (char*) malloc (length);
		memcpy (dup, data, length);
		__imlib_AttachTag (im, "id3-description", 0,
				   dup, destructor_data);
	}
	if (field = id3_frame_field (frame, 0))
		__imlib_AttachTag (im, "id3-description-text-encoding",
				   (num_data = (int)
				    id3_field_gettextencoding (field)),
				   num_data < NUM_OF_ID3_TEXT_ENCODINGS ?
				   id3_text_encodings[num_data] : NULL, NULL);
	if (field = id3_frame_field (frame, 2))
		__imlib_AttachTag (im, "id3-picture-type",
				   (num_data = id3_field_getint (field)),
				   num_data < NUM_OF_ID3_PIC_TYPES ?
				   id3_pic_types[num_data] : NULL, NULL);
	__imlib_AttachTag (im, "count", id3_tag_get_numframes (opt->ctx->tag),
			   NULL, NULL);
	if (opt->cache_level) {
		context_addref (opt->ctx);
		__imlib_AttachTag (im, "context", opt->ctx->id,
				   opt->ctx, destructor_context);
	}
	__imlib_AttachTag (im, "index", opt->index, NULL, NULL);
	if (opt->traverse) {
		char* buf = NULL;
		if ((opt->index + opt->traverse)
		    <= id3_tag_get_numframes (opt->ctx->tag)
		    && (opt->index + opt->traverse) > 0) {
			buf = (char*) malloc
				((strlen (im->real_file) + 50) * sizeof (char));
			sprintf (buf, "%s:index=%d,traverse=%d", im->real_file,
				 opt->index + opt->traverse, opt->traverse);
		}
		__imlib_AttachTag (im, "next", 0, buf, destructor_data);
	}
}

char load (ImlibImage *im, ImlibProgressFunction progress,
           char progress_granularity, char immediate_load)
{
	ImlibLoader *loader;
	lopt opt;
	int res;
	struct stat st;

	assert (im);
	if (stat(im->real_file, &st) < 0)
		return 0;
	if (! get_options (&opt, im))
		return 0;

	if (! get_loader (&opt, &loader))
		goto fail_context;

	if (loader) {
		char *ofile, tmp[] = "/tmp/imlib2_loader_id3-XXXXXX";
		int dest;

		if ((dest = mkstemp (tmp)) < 0) {
			fprintf (stderr, "Unable to create a temporary file\n");
			goto fail_context;
		}
		res = extract_pic (id3_tag_get_frame (opt.ctx->tag,
						      opt.index - 1), dest);
		close (dest);

		if (! res) {
			unlink (tmp);
			goto fail_context;
		}

		ofile = im->real_file;
		im->real_file = strdup (tmp);
		res = loader->load (im, progress,
				    progress_granularity, immediate_load);
		free (im->real_file);
		im->real_file = ofile;

		unlink (tmp);
	} else {
		/* The tag actually provides a image url rather than image data.
		 * Practically, dunno if such a tag exists on earth :)
		 * Here's the code anyway...
		 */
		union id3_field* field;
		id3_length_t length;
		char const* data;
		char *url, *file, *ofile;

		field = id3_frame_field
			(id3_tag_get_frame (opt.ctx->tag, opt.index - 1), 4);
		data = (char const*) id3_field_getbinarydata (field, &length);
		if (! data || ! length) {
			fprintf (stderr, "No link image URL present\n");
			goto fail_context;
		}
		url = (char*) malloc ((length + 1) * sizeof (char));
		strncpy (url, data, length);
		url[length] = '\0';
		file = (strncmp (url, "file://", 7) ? url : url + 7);
		if (! (loader = __imlib_FindBestLoaderForFile (file, 0))) {
			fprintf (stderr, "No loader found for file %s\n", file);
			free (url);
			goto fail_context;
		}
		ofile = im->real_file;
		im->real_file = file;
		res = loader->load (im, progress,
				    progress_granularity, immediate_load);
		if (! im->loader)
			__imlib_AttachTag (im, "id3-link-url", 0,
					   url, destructor_data);
		else
			free (url);
		im->real_file = ofile;
	}

	if (! im->loader)
		write_tags (im, &opt);

#ifdef DEBUG
	if (! im->loader) {
		ImlibImageTag* cur = im->tags;
		fprintf (stderr, "Tags for file %s:\n", im->file);
		while (cur) {
			fprintf (stderr, "\t%s: (%d) %s\n", cur->key,
				 cur->val, (char*) cur->data);
			cur = cur->next;
		}
	}
#endif

	context_delref (opt.ctx);
	return res;

fail_context:
	context_delref (opt.ctx);
	return 0;
}

void formats (ImlibLoader *l)
{
	/* this is the only bit you have to change... */
	char *list_formats[] = {"mp3"};
	int i;

	/* don't bother changing any of this - it just reads this in
	 * and sets the struct values and makes copies
	 */
	l->num_formats = sizeof (list_formats) / sizeof (char *);
	l->formats = (char**) malloc (sizeof (char *) * l->num_formats);

	for (i = 0; i < l->num_formats; i++)
		l->formats[i] = strdup (list_formats[i]);
}
