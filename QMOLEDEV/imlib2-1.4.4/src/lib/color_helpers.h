#ifndef __COLOR_HELPERS 
#define __COLOR_HELPERS 1

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>

#include "common.h"

__hidden void        __imlib_rgb_to_hsv(int r, int g, int b, float *hue, float *saturation, float *value);
__hidden void        __imlib_hsv_to_rgb(float hue, float saturation, float value, int *r, int *g, int *b);
__hidden void        __imlib_rgb_to_hls(int r, int g, int b, float *hue, float *lightness, float *saturation);
__hidden void        __imlib_hls_to_rgb(float hue, float lightness, float saturation, int *r, int *g, int *b);

#endif
