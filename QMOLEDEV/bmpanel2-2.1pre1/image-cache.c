#include "gui.h"

#define IMAGES_CACHE_SIZE 128

struct image {
	char *filename;
	cairo_surface_t	*surface;
};

static size_t images_cache_n;
static struct image *images_cache[IMAGES_CACHE_SIZE];

static struct image *load_image_from_file(const char *path)
{
	cairo_surface_t *surface = cairo_image_surface_create_from_png(path);
	if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
		cairo_surface_destroy(surface);
		return 0;
	}

	struct image *img = xmalloc(sizeof(struct image));
	img->filename = xstrdup(path);
	img->surface = surface;
	return img;
}

static struct image *find_image_in_cache(const char *path)
{
	size_t i;
	for (i = 0; i < images_cache_n; ++i) {
		if (strcmp(images_cache[i]->filename, path) == 0)
			return images_cache[i];
	}
	return 0;
}

static void try_add_image_to_cache(struct image *img)
{
	if (images_cache_n == IMAGES_CACHE_SIZE)
		return;

	images_cache[images_cache_n++] = img;	
}

static void free_image(struct image *img, int final)
{
	if (final && cairo_surface_get_reference_count(img->surface) > 1)
		XWARNING("Image: \"%s\" has big ref count", img->filename);
	xfree(img->filename);
	cairo_surface_destroy(img->surface);
	xfree(img);
}

cairo_surface_t *get_image(const char *path)
{
	struct image *img = find_image_in_cache(path);	
	if (img) {
		cairo_surface_reference(img->surface);
		return img->surface;
	}

	img = load_image_from_file(path);
	if (img) {
		try_add_image_to_cache(img);
		cairo_surface_reference(img->surface);
		return img->surface;
	}
	return 0;
}

cairo_surface_t *get_image_part(const char *path, int x, int y, int w, int h)
{
	cairo_surface_t *source = get_image(path);
	if (!source)
		return 0;

	cairo_surface_t *dest = cairo_image_surface_create(
			cairo_image_surface_get_format(source),
			w,h);
	ENSURE(cairo_surface_status(dest) == CAIRO_STATUS_SUCCESS,
	       "Failed to create cairo image surface");

	cairo_t *cr = cairo_create(dest);
	cairo_set_source_surface(cr, source, -x, -y);
	cairo_pattern_set_extend(cairo_get_source(cr), CAIRO_EXTEND_REPEAT);
	cairo_paint(cr);
	cairo_destroy(cr);
	cairo_surface_destroy(source);
	return dest;
}

void clean_image_cache(int final)
{
	size_t i;
	for (i = 0; i < images_cache_n; ++i)
		free_image(images_cache[i], final);
	images_cache_n = 0;
}
