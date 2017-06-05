/* babl - dynamically extendable universal pixel fish library.
 * Copyright (C) 2005, Øyvind Kolås.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, see
 * <http://www.gnu.org/licenses/>.
 */

#include "config.h"
#include "babl-internal.h"

static Babl *
assert_conversion_find (void *source,
                        void *destination)
{
  Babl *ret = babl_conversion_find (source, destination);

  if (!ret)
    babl_fatal ("failed, aborting");
  return ret;
}

static char *
create_name (const Babl *source,
             const Babl *destination,
             int   is_reference)
{
  static char buf[1024];

  /* fish names are intentionally kept short */
  snprintf (buf, 1024, "%s %p %p",
            is_reference ? "ref "
            : "",
            source, destination);
  return buf;
}


Babl *
babl_fish_reference (const Babl *source,
                     const Babl *destination)
{
  Babl *babl = NULL;
  char *name = create_name (source, destination, 1);

  babl = babl_db_exist_by_name (babl_fish_db (), name);
  if (babl)
    {
      /* There is an instance already registered by the required name,
       * returning the preexistent one instead.
       */
      return babl;
    }

  babl_assert (BABL_IS_BABL (source));
  babl_assert (BABL_IS_BABL (destination));

  babl_assert (source->class_type == BABL_FORMAT);
  babl_assert (destination->class_type == BABL_FORMAT);

  babl = babl_malloc (sizeof (BablFishReference) +
                      strlen (name) + 1);
  babl->class_type    = BABL_FISH_REFERENCE;
  babl->instance.id   = babl_fish_get_id (source, destination);
  babl->instance.name = ((char *) babl) + sizeof (BablFishReference);
  strcpy (babl->instance.name, name);
  babl->fish.source      = source;
  babl->fish.destination = destination;

  babl->fish.processings = 0;
  babl->fish.pixels      = 0;
  babl->fish.error       = 0.0;  /* assuming the provided reference conversions for types
                                    and models are as exact as possible
                                  */

  /* Since there is not an already registered instance by the required
   * name, inserting newly created class into database.
   */
  babl_db_insert (babl_fish_db (), babl);
  return babl;
}


static void
convert_to_double (BablFormat *source_fmt,
                   BablImage  *source,
                   char       *source_buf,
                   char       *source_double_buf,
                   int         n)
{
  int        i;

  BablImage *src_img;
  BablImage *dst_img;

  src_img = (BablImage *) babl_image_new (
    babl_component_from_id (BABL_LUMINANCE), NULL, 1, 0, NULL);
  dst_img = (BablImage *) babl_image_new (
    babl_component_from_id (BABL_LUMINANCE), NULL, 1, 0, NULL);

  dst_img->type[0]  = (BablType *) babl_type_from_id (BABL_DOUBLE);
  dst_img->pitch[0] =
    (dst_img->type[0]->bits / 8) * source_fmt->model->components;
  dst_img->stride[0] = 0;


  src_img->data[0]   = source_buf;
  src_img->type[0]   = (BablType *) babl_type_from_id (BABL_DOUBLE);
  src_img->pitch[0]  = source_fmt->bytes_per_pixel;
  src_img->stride[0] = 0;

  /* i is source position */
  for (i = 0; i < source_fmt->components; i++)
    {
      int j;

      src_img->type[0] = source_fmt->type[i];
      /* j is source position */
      for (j = 0; j < source_fmt->model->components; j++)
        {
          if (source_fmt->component[i] ==
              source_fmt->model->component[j])
            {
              dst_img->data[0] =
                source_double_buf + (dst_img->type[0]->bits / 8) * j;
              break;
            }
        }

      babl_process (
        assert_conversion_find (src_img->type[0], dst_img->type[0]),
        src_img, dst_img,
        n);
      src_img->data[0] += src_img->type[0]->bits / 8;
    }
  babl_free (src_img);
  babl_free (dst_img);
}


static void
convert_from_double (BablFormat *destination_fmt,
                     char       *destination_double_buf,
                     BablImage  *destination,
                     char       *destination_buf,
                     int         n)
{
  int        i;

  BablImage *src_img;
  BablImage *dst_img;

  src_img = (BablImage *) babl_image_new (
    babl_component_from_id (BABL_LUMINANCE), NULL, 1, 0, NULL);
  dst_img = (BablImage *) babl_image_new (
    babl_component_from_id (BABL_LUMINANCE), NULL, 1, 0, NULL);

  src_img->type[0]   = (BablType *) babl_type_from_id (BABL_DOUBLE);
  src_img->pitch[0]  = (src_img->type[0]->bits / 8) * destination_fmt->model->components;
  src_img->stride[0] = 0;

  dst_img->data[0]  = destination_buf;
  dst_img->type[0]  = (BablType *) babl_type_from_id (BABL_DOUBLE);
  dst_img->pitch[0] = destination_fmt->bytes_per_pixel;
  dst_img->stride[0] = 0;

  for (i = 0; i < destination_fmt->components; i++)
    {
      int j;

      dst_img->type[0] = destination_fmt->type[i];

      for (j = 0; j < destination_fmt->model->components; j++)
        {
          if (destination_fmt->component[i] ==
              destination_fmt->model->component[j])
            {
              src_img->data[0] =
                destination_double_buf + (src_img->type[0]->bits / 8) * j;
              break;
            }
        }

      babl_process (
        assert_conversion_find (src_img->type[0], dst_img->type[0]),
        src_img, dst_img,
        n);
      dst_img->data[0] += dst_img->type[0]->bits / 8;
    }
  babl_free (src_img);
  babl_free (dst_img);
}


static void
ncomponent_convert_to_double (BablFormat *source_fmt,
                              BablImage  *source,
                              char       *source_buf,
                              char       *source_double_buf,
                              int         n)
{
  BablImage *src_img;
  BablImage *dst_img;

  src_img = (BablImage *) babl_image_new (
    babl_component_from_id (BABL_LUMINANCE), NULL, 1, 0, NULL);
  dst_img = (BablImage *) babl_image_new (
    babl_component_from_id (BABL_LUMINANCE), NULL, 1, 0, NULL);

  dst_img->type[0]  = (BablType *) babl_type_from_id (BABL_DOUBLE);
  dst_img->pitch[0] = (dst_img->type[0]->bits / 8);
  dst_img->stride[0] = 0;

  src_img->data[0]   = source_buf;
  src_img->type[0] = source_fmt->type[0];
  src_img->pitch[0] = source_fmt->type[0]->bits / 8;
  src_img->stride[0] = 0;

  dst_img->data[0] = source_double_buf;

  babl_process (
    assert_conversion_find (src_img->type[0], dst_img->type[0]),
    src_img, dst_img,
    n * source_fmt->components);
  babl_free (src_img);
  babl_free (dst_img);
}

static void
ncomponent_convert_from_double (BablFormat *destination_fmt,
                                char       *destination_double_buf,
                                BablImage  *destination,
                                char       *destination_buf,
                                int         n)
{
  BablImage *src_img;
  BablImage *dst_img;

  src_img = (BablImage *) babl_image_new (
    babl_component_from_id (BABL_LUMINANCE), NULL, 1, 0, NULL);
  dst_img = (BablImage *) babl_image_new (
    babl_component_from_id (BABL_LUMINANCE), NULL, 1, 0, NULL);

  src_img->type[0]   = (BablType *) babl_type_from_id (BABL_DOUBLE);
  src_img->pitch[0]  = (src_img->type[0]->bits / 8);
  src_img->stride[0] = 0;

  dst_img->data[0]  = destination_buf;
  dst_img->type[0]  = (BablType *) babl_type_from_id (BABL_DOUBLE);
  dst_img->pitch[0] = destination_fmt->type[0]->bits/8;
  dst_img->stride[0] = 0;

  dst_img->type[0] = destination_fmt->type[0];
  src_img->data[0] = destination_double_buf;

  babl_process (
    assert_conversion_find (src_img->type[0], dst_img->type[0]),
    src_img, dst_img,
    n * destination_fmt->components);

  dst_img->data[0] += dst_img->type[0]->bits / 8;
  babl_free (src_img);
  babl_free (dst_img);
}



static int
process_same_model (Babl      *babl,
                    BablImage *source,
                    BablImage *destination,
                    long       n)
{
  void *double_buf;

  if (BABL_IS_BABL (source) ||
      BABL_IS_BABL (destination))
    {
      babl_log ("args=(%p, %p, %p, %li): trying to handle BablImage (unconfirmed code)",
                babl_fish, source, destination, n);
    }

  double_buf = babl_malloc (sizeof (double) * n *
                            BABL (babl->fish.source)->format.components);

  if (
      (BABL (babl->fish.source)->format.components ==
       BABL (babl->fish.destination)->format.components)
      && (BABL (babl->fish.source)->format.model->components !=
          BABL (babl->fish.source)->format.components))
    {
      /* FIXME: should recursively invoke babl and look up an appropriate fish
       * for the conversion and multiply n by the number of components.
       */
      ncomponent_convert_to_double (
        (BablFormat *) BABL (babl->fish.source),
        BABL_IS_BABL (source) ? source : NULL,
        BABL_IS_BABL (source) ? NULL : (char *) source,
        double_buf,
        n
      );

      ncomponent_convert_from_double (
        (BablFormat *) BABL (babl->fish.destination),
        double_buf,
        BABL_IS_BABL (destination) ? destination : NULL,
        BABL_IS_BABL (destination) ? NULL : (char *) destination,
        n
      );
    }
  else
    {
      convert_to_double (
        (BablFormat *) BABL (babl->fish.source),
        BABL_IS_BABL (source) ? source : NULL,
        BABL_IS_BABL (source) ? NULL : (char *) source,
        double_buf,
        n
      );

      convert_from_double (
        (BablFormat *) BABL (babl->fish.destination),
        double_buf,
        BABL_IS_BABL (destination) ? destination : NULL,
        BABL_IS_BABL (destination) ? NULL : (char *) destination,
        n
      );
    }
  babl_free (double_buf);
  return 0;
}

long
babl_fish_reference_process (Babl      *babl,
                             BablImage *source,
                             BablImage *destination,
                             long       n)
{
  void *source_double_buf;
  void *rgba_double_buf;
  void *destination_double_buf;
  Babl *source_image;
  Babl *rgba_image;
  Babl *destination_image;


  if (BABL (babl->fish.source)->format.model ==
      BABL (babl->fish.destination)->format.model)
    return process_same_model (babl, source, destination, n);
#if 0
  if (BABL_IS_BABL (source) ||
      BABL_IS_BABL (destination))
    {
      babl_log ("args=(%p, %p, %p, %li): trying to handle BablImage (unconfirmed code)",
                babl_fish, source, destination, n);
    }
#endif

  source_double_buf = babl_malloc (sizeof (double) * n *
                                   BABL (babl->fish.source)->format.model->components);
  rgba_double_buf        = babl_malloc (sizeof (double) * n * 4);
  destination_double_buf = babl_malloc (sizeof (double) * n *
                                        BABL (babl->fish.destination)->format.model->components);

  source_image = babl_image_from_linear (
    source_double_buf, BABL (BABL ((babl->fish.source))->format.model));
  rgba_image = babl_image_from_linear (
    rgba_double_buf, babl_model_from_id (BABL_RGBA));
  destination_image = babl_image_from_linear (
    destination_double_buf, BABL (BABL ((babl->fish.destination))->format.model));

  convert_to_double (
    (BablFormat *) BABL (babl->fish.source),
    NULL,
    (char *) source,
    source_double_buf,
    n
  );

  {
    Babl *conv =
      assert_conversion_find (
      BABL (babl->fish.source)->format.model,
      babl_model_from_id (BABL_RGBA)
      );
    if (conv->class_type == BABL_CONVERSION_PLANAR)
      {
        babl_process (
          conv,
          source_image, rgba_image,
          n);
      }
    else if (conv->class_type == BABL_CONVERSION_LINEAR)
      {
        babl_process (
          conv,
          source_double_buf, rgba_double_buf,
          n);
      }
    else babl_fatal ("oops");
  }

  {
    Babl *conv =
      assert_conversion_find (
      babl_model_from_id (BABL_RGBA),
      BABL (babl->fish.destination)->format.model);
    if (conv->class_type == BABL_CONVERSION_PLANAR)
      {
        babl_process (
          conv,
          rgba_image, destination_image,
          n);
      }
    else if (conv->class_type == BABL_CONVERSION_LINEAR)
      {
        babl_process (
          conv,
          rgba_double_buf, destination_double_buf,
          n);
      }
    else babl_fatal ("oops");
  }

  convert_from_double (
    (BablFormat *) BABL (babl->fish.destination),
    destination_double_buf,
    NULL,
    (char *) destination,
    n
  );

  babl_free (source_image);
  babl_free (rgba_image);
  babl_free (destination_image);

  babl_free (destination_double_buf);
  babl_free (rgba_double_buf);
  babl_free (source_double_buf);
  return n;
}

