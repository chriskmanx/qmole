/* drivers.c
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL
 */

#include "cfg.h"

#ifdef G

#include "links.h"

int F = 0;

struct graphics_driver *drv = NULL;

#ifdef GRDRV_X
extern struct graphics_driver x_driver;
#endif
#ifdef GRDRV_SVGALIB
extern struct graphics_driver svga_driver;
#endif
#ifdef GRDRV_FB
extern struct graphics_driver fb_driver;
#endif
#ifdef GRDRV_DIRECTFB
extern struct graphics_driver directfb_driver;
#endif
#ifdef GRDRV_PMSHELL
extern struct graphics_driver pmshell_driver;
#endif
#ifdef GRDRV_ATHEOS
extern struct graphics_driver atheos_driver;
#endif
#ifdef GRDRV_SDL
extern struct graphics_driver sdl_driver;
#endif

/*
 * On SPAD you must test first svgalib and then X (because X test is slow).
 * On other systems you must test first X and then svgalib (because svgalib
 *	would work in X too and it's undesirable).
 */

static struct graphics_driver *graphics_drivers[] = {
#ifdef GRDRV_PMSHELL
	&pmshell_driver,
#endif
#ifdef GRDRV_ATHEOS
	&atheos_driver,
#endif
#ifndef SPAD
#ifdef GRDRV_X
	&x_driver,
#endif
#endif
#ifdef GRDRV_FB
	/* use FB before DirectFB because DirectFB has bugs */
	&fb_driver,
#endif
#ifdef GRDRV_DIRECTFB
	&directfb_driver,
#endif
#ifdef GRDRV_SVGALIB
	&svga_driver,
#endif
#ifdef SPAD
#ifdef GRDRV_X
	&x_driver,
#endif
#endif
#ifdef GRDRV_SDL
	&sdl_driver,
#endif
	NULL
};

int dummy_block(struct graphics_device *dev)
{
	return 0;
}

int dummy_unblock(struct graphics_device *dev)
{
	return 0;
}

#if 0
static unsigned char *list_graphics_drivers(void)
{ 
	unsigned char *d = init_str();
	int l = 0;
	struct graphics_driver **gd;
	for (gd = graphics_drivers; *gd; gd++) {
		if (l) add_to_str(&d, &l, " ");
		add_to_str(&d, &l, (*gd)->name);
	}
	return d;
}
#endif

/* Driver je jednorazovy argument, kterej se preda grafickymu driveru, nikde se dal
 * neuklada.  Param se skladuje v default_driver param a uklada se do konfiguraku. Pred
 * ukoncenim grafickeho driveru se nastavi default_driver_param podle
 * drv->get_driver_param.
 */
static unsigned char *init_graphics_driver(struct graphics_driver *gd, unsigned char *param, unsigned char *display)
{
	unsigned char *r;
	unsigned char *p = param;
	struct driver_param *dp=get_driver_param(gd->name);
	if (!param || !*param) p = dp->param;
	gd->codepage=dp->codepage;
	gd->shell=mem_calloc(MAX_STR_LEN);
	if (dp->shell) safe_strncpy(gd->shell,dp->shell,MAX_STR_LEN);
	r = gd->init_driver(p,display);
	if (r) mem_free(gd->shell), gd->shell = NULL;
	return r;
}

unsigned char *init_graphics(unsigned char *driver, unsigned char *param, unsigned char *display)
{
	unsigned char *s = init_str();
	int l = 0;
	struct graphics_driver **gd;
#if defined(GRDRV_PMSHELL) && defined(GRDRV_X)
	if (is_xterm()) {
		static unsigned char swapped = 0;
		if (!swapped) {
			for (gd = graphics_drivers; *gd; gd++) {
				if (*gd == &pmshell_driver) *gd = &x_driver;
				else if (*gd == &x_driver) *gd = &pmshell_driver;
			}
			swapped = 1;
		}
	}
#endif
	for (gd = graphics_drivers; *gd; gd++) {
		if (!driver || !*driver || !strcasecmp((*gd)->name, driver)) {
			unsigned char *r;
			if ((!driver || !*driver) && (*gd)->flags & GD_NOAUTO) continue;
			if (!(r = init_graphics_driver(*gd, param, display))) {
				mem_free(s);
				drv = *gd;
				F = 1;
				return NULL;
			}
			if (!l) {
				if (!driver || !*driver) add_to_str(&s, &l, "Could not initialize any graphics driver. Tried the following drivers:\n");
				else add_to_str(&s, &l, "Could not initialize graphics driver ");
			}
			add_to_str(&s, &l, (*gd)->name);
			add_to_str(&s, &l, ":\n");
			add_to_str(&s, &l, r);
			mem_free(r);
		}
	}
	if (!l) {
		add_to_str(&s, &l, "Unknown graphics driver ");
		if (driver) add_to_str(&s, &l, driver);
		add_to_str(&s, &l, ".\nThe following graphics drivers are supported:\n");
		for (gd = graphics_drivers; *gd; gd++) {
			if (gd != graphics_drivers) add_to_str(&s, &l, ", ");
			add_to_str(&s, &l, (*gd)->name);
		}
		add_to_str(&s, &l, "\n");
	}
	return s;
}

void shutdown_graphics(void)
{
	if (drv)
	{
		if (drv->shell) mem_free(drv->shell);
		drv->shutdown_driver();
	}
}

void update_driver_param(void)
{
	if (drv)
	{
		struct driver_param *dp=get_driver_param(drv->name);
		dp->codepage=drv->codepage;
		if (dp->param) mem_free(dp->param);
		dp->param=stracpy(drv->get_driver_param());
		if (dp->shell) mem_free(dp->shell);
		dp->shell=stracpy(drv->shell);
		dp->nosave = 0;
	}
}

#if defined(GRDRV_SVGALIB)|defined(GRDRV_FB)

static struct graphics_driver *virtual_device_driver;
struct graphics_device **virtual_devices;
int n_virtual_devices = 0;
struct graphics_device *current_virtual_device;

static int virtual_device_timer;

int init_virtual_devices(struct graphics_driver *drv, int n)
{
	if (n_virtual_devices) {
		internal("init_virtual_devices: already initialized");
		return -1;
	}
	if ((unsigned)n > MAXINT / sizeof(struct graphics_device *)) overalloc();
	virtual_devices = mem_calloc(n * sizeof(struct graphics_device *));
	n_virtual_devices = n;
	virtual_device_driver = drv;
	virtual_device_timer = -1;
	current_virtual_device = NULL;
	return 0;
}

struct graphics_device *init_virtual_device(void)
{
	int i;
	for (i = 0; i < n_virtual_devices; i++) if (!virtual_devices[i]) {
		struct graphics_device *dev;
		dev = mem_calloc(sizeof(struct graphics_device));
		dev->size.x2 = virtual_device_driver->x;
		dev->size.y2 = virtual_device_driver->y;
		current_virtual_device = virtual_devices[i] = dev;
		virtual_device_driver->set_clip_area(dev, &dev->size);
		return dev;
	}
	return NULL;
}

static void virtual_device_timer_fn(void *p)
{
	virtual_device_timer = -1;
	if (current_virtual_device && current_virtual_device->redraw_handler) {
		drv->set_clip_area(current_virtual_device, &current_virtual_device->size);
		current_virtual_device->redraw_handler(current_virtual_device, &current_virtual_device->size);
	}
}

void switch_virtual_device(int i)
{
	if (i == VD_NEXT) {
		int j;
		int t = 0;
		for (j = 0; j < n_virtual_devices * 2; j++)
			if (virtual_devices[j % n_virtual_devices] == current_virtual_device) t = 1;
			else if (virtual_devices[j % n_virtual_devices] && t) {
				current_virtual_device = virtual_devices[j % n_virtual_devices];
				goto ok_switch;
			}
		return;
	}
	if (i < 0 || i >= n_virtual_devices || !virtual_devices[i]) return;
	current_virtual_device = virtual_devices[i];
	ok_switch:
	if (virtual_device_timer == -1)
		virtual_device_timer = install_timer(0, virtual_device_timer_fn, NULL);
}

void shutdown_virtual_device(struct graphics_device *dev)
{
	int i;
	for (i = 0; i < n_virtual_devices; i++) if (virtual_devices[i] == dev) {
		virtual_devices[i] = NULL;
		mem_free(dev);
		if (current_virtual_device != dev) return;
		for (; i < n_virtual_devices; i++) if (virtual_devices[i]) {
			switch_virtual_device(i);
			return;
		}
		for (i = 0; i < n_virtual_devices; i++) if (virtual_devices[i]) {
			switch_virtual_device(i);
			return;
		}
		current_virtual_device = NULL;
		return;
	}
	mem_free(dev);
	/*internal("shutdown_virtual_device: device not initialized");*/
}

void shutdown_virtual_devices(void)
{
	int i;
	if (!n_virtual_devices) {
		internal("shutdown_virtual_devices: already shut down");
		return;
	}
	for (i = 0; i < n_virtual_devices; i++) if (virtual_devices[i]) internal("shutdown_virtual_devices: virtual device %d is still active", i);
	mem_free(virtual_devices);
	n_virtual_devices = 0;
	if (virtual_device_timer != -1) kill_timer(virtual_device_timer), virtual_device_timer = -1;
}

#endif

#endif
