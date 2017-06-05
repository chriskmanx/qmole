#include "color_helpers.h"
/*
 * Color space conversion helper routines
 * Convert between rgb and hsv adn between rgb and hls
 */

void
__imlib_rgb_to_hsv(int r, int g, int b, float *h, float *s, float *v)
{
   float               min, max, delta;

   min = (((r < g) ? r : g) < b) ? ((r < g) ? r : g) : b;
   max = (((r > g) ? r : g) > b) ? ((r > g) ? r : g) : b;

   *v = max / 255.0;
   delta = (max - min);
   if (delta == 0)
     {
        *h = 0.0;
        *s = 0.0;
        return;
     }
   *s = delta / max;
   if (r == max)
      *h = (g - b) / delta;
   else if (g == max)
      *h = 2.0 + (b - r) / delta;
   else
      *h = 4.0 + (r - g) / delta;
   *h *= 60.0;
   if (*h < 0)
      *h += 360.0;
}

void
__imlib_hsv_to_rgb(float h, float s, float v, int *r, int *g, int *b)
{
   float               f, vf;
   int                 i, p, q, t, vv;

   vf = 255.0 * v;
   vv = (int)round(vf);

   if (s == 0.0)
     {
        *r = *g = *b = vv;
        return;
     }

   h /= 60.0;
   i = floor(h);
   f = h - (float)i;
   p = (int)round(vf * (1.0 - s));
   q = (int)round(vf * (1.0 - (s * f)));
   t = (int)round(vf * (1.0 - s * (1.0 - f)));

   switch (i % 6)
     {
     case 0:
        *r = vv;
        *g = t;
        *b = p;
        break;
     case 1:
        *r = q;
        *g = vv;
        *b = p;
        break;
     case 2:
        *r = p;
        *g = vv;
        *b = t;
        break;
     case 3:
        *r = p;
        *g = q;
        *b = vv;
        break;
     case 4:
        *r = t;
        *g = p;
        *b = vv;
        break;
     case 5:
     default:
        *r = vv;
        *g = p;
        *b = q;
        break;
     }
}

void
__imlib_rgb_to_hls(int r, int g, int b, float *hue, float *lightness,
                   float *saturation)
{
   int                 f;
   float               i, j, k, max, min, d;

   i = ((float)r) / 255.0;
   j = ((float)g) / 255.0;
   k = ((float)b) / 255.0;

   f = 0;
   max = min = i;
   if (j > max)
     {
        max = j;
        f = 1;
     }
   else
      min = j;
   if (k > max)
     {
        max = k;
        f = 2;
     }
   else if (k < min)
      min = k;
   d = max - min;

   *lightness = (max + min) / 2.0;
   if (d == 0)
     {
        *saturation = 0;
        *hue = 0;
     }
   else
     {
        if (*lightness < 0.5)
           *saturation = d / (max + min);
        else
           *saturation = d / (2 - max - min);
        switch (f)
          {
          case 0:
             *hue = (j - k) / d;
             break;
          case 1:
             *hue = 2 + (k - i) / d;
             break;
          case 2:
             *hue = 4 + (i - j) / d;
             break;
          }
        *hue *= 60.0;
        if (*hue < 0)
           *hue += 360.0;
     }
}

void
__imlib_hls_to_rgb(float hue, float lightness, float saturation, int *r, int *g,
                   int *b)
{
   float               m1, m2, m21, h;

   if (saturation == 0)
      *r = *g = *b = (int)(lightness * 255.0);
   else
     {
        if (lightness <= 0.5)
           m2 = lightness * (1 + saturation);
        else
           m2 = lightness + saturation + lightness * saturation;
        m1 = 2 * lightness - m2;
        m21 = m2 - m1;
        h = hue + 120;
        if (h > 360)
           h -= 360;
        else if (h < 0)
           h += 360;
        if (h < 60)
           *r = (int)(255.0 * (m1 + m21 * h / 60.0));
        else if (h < 180)
           *r = (int)(255.0 * m2);
        else if (h < 240)
           *r = (int)(255.0 * (m1 + m21 * (240.0 - h) / 60.0));
        else
           *r = (int)(255.0 * m1);
        h = hue;
        if (h > 360)
           h -= 360;
        else if (h < 0)
           h += 360;
        if (h < 60)
           *g = (int)(255.0 * (m1 + m21 * h / 60.0));
        else if (h < 180)
           *g = (int)(255.0 * m2);
        else if (h < 240)
           *g = (int)(255.0 * (m1 + m21 * (240.0 - h) / 60.0));
        else
           *g = (int)(255.0 * m1);
        h = hue - 120;
        if (h > 360)
           h -= 360;
        else if (h < 0)
           h += 360;
        if (h < 60)
           *b = (int)(255.0 * (m1 + m21 * h / 60.0));
        else if (h < 180)
           *b = (int)(255.0 * m2);
        else if (h < 240)
           *b = (int)(255.0 * (m1 + m21 * (240.0 - h) / 60.0));
        else
           *b = (int)(255.0 * m1);
     }
}
