#include "loader_common.h"
#include <zlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>

#define OUTBUF_SIZE 16484

static int uncompress_file (int src, int dest)
{
	gzFile gf;
	DATA8 outbuf[OUTBUF_SIZE];
	int ret = 1, bytes;

	gf = gzdopen (dup (src), "rb");
	if (!gf)
		return 0;

	while (1) {
		bytes = gzread (gf, outbuf, OUTBUF_SIZE);

		if (!bytes)
			break;
		else if (bytes == -1) {
			ret = 0;
			break;
		} else
			write (dest, outbuf, bytes);
	}

	gzclose (gf);

	return ret;
}

char load (ImlibImage *im, ImlibProgressFunction progress,
           char progress_granularity, char immediate_load)
{
	ImlibLoader *loader;
	int src, dest, res;
	char *file, *p, tmp[] = "/tmp/imlib2_loader_zlib-XXXXXX";
	char real_ext[16];
	struct stat st;

	assert (im);

	/* check that this file ends in *.gz and that there's another ext
	 * (e.g. "foo.png.gz"
	 */
	p = strrchr(im->real_file, '.');
	if (p && p != im->real_file) {
		if (strcasecmp(p + 1, "gz"))
			return 0;
	} else
		return 0;

	strncpy (real_ext, p - sizeof (real_ext) + 1, sizeof (real_ext));
	real_ext[sizeof (real_ext) - 1] = '\0';

	/* abort if there's no dot in the "real" filename */
	if (!strrchr (real_ext, '.'))
		return 0;

	if (stat (im->real_file, &st) < 0)
		return 0;

	if ((src = open (im->real_file, O_RDONLY)) < 0) {
		return 0;
	}

	if ((dest = mkstemp (tmp)) < 0) {
		close (src);
		return 0;
	}

	res = uncompress_file (src, dest);
	close (src);
	close (dest);

	if (!res) {
		unlink (tmp);
		return 0;
	}

	if (!(loader = __imlib_FindBestLoaderForFile (real_ext, 0))) {
		unlink (tmp);
		return 0;
	}

	/* remember the original filename */
	file = strdup (im->real_file);

	free (im->real_file);
	im->real_file = strdup (tmp);
	loader->load (im, progress, progress_granularity, immediate_load);

	free (im->real_file);
	im->real_file = file;
	unlink (tmp);

	return 1;
}

void formats (ImlibLoader *l)
{
	/* this is the only bit you have to change... */
	char *list_formats[] = {"gz"};
	int i;

   /* don't bother changing any of this - it just reads this in
	* and sets the struct values and makes copies
	*/
	l->num_formats = sizeof (list_formats) / sizeof (char *);
	l->formats = malloc (sizeof (char *) * l->num_formats);

	for (i = 0; i < l->num_formats; i++)
		l->formats[i] = strdup (list_formats[i]);
}
