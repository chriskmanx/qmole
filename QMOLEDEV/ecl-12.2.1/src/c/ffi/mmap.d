/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    mmap.d -- Mapping of binary files.
*/
/*
    Copyright (c) 2011, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#ifdef HAVE_SYS_MMAN_H
# include <sys/mman.h>
#endif
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>

@(defun ext::mmap (filename
                   &key
                   (length Cnil)
                   (offset MAKE_FIXNUM(0))
                   (direction @':input')
                   (element_type @'base-char')
                   (if_exists @':new-version')
                   (if_does_not_exist @':error')
                   (external_format @':default'))
@
#ifdef HAVE_SYS_MMAN_H
{
        cl_object output, stream;
        int c_prot, c_flags, fd;
        size_t len;
        void *pa;
        if (direction == @':input')
                c_prot = PROT_READ;
        else if (direction == @':output')
                c_prot = PROT_WRITE;
        else if (direction == @':io')
                c_prot = PROT_READ | PROT_WRITE;
        else
                c_prot = PROT_NONE;
        if (Null(filename)) {
                c_flags = MAP_ANON | MAP_PRIVATE;
                fd = -1;
                len = ecl_to_unsigned_integer(length);
                stream = Cnil;
        } else {
                c_flags = MAP_SHARED;
                stream = cl_open(13, filename,
                                 @':direction', direction,
                                 @':element-type', element_type,
                                 @':if-exists', if_exists,
                                 @':if-does-not-exist', if_does_not_exist,
                                 @':external-format', @':default',
                                 @':cstream', Cnil);
                fd = ecl_to_int(si_file_stream_fd(stream));
                if (Null(length))
                        len = ecl_to_unsigned_integer(ecl_file_length(stream));
        }
        output = si_make_vector(element_type, MAKE_FIXNUM(0), Cnil,
                                Cnil, Cnil, Cnil);
        pa = mmap(0, len, c_prot, c_flags, fd,
                  ecl_integer_to_off_t(offset));
        if (pa == MAP_FAILED) {
                FElibc_error("EXT::MMAP failed.", 0);
        } else {
                output->base_string.self = pa;
                output->base_string.dim =
                        output->base_string.fillp = len;
        }
        @(return CONS(output, stream))
}
#else
{
        cl_object output, vector;
        if (Null(filename)) {
                output = si_make_vector(element_type, length, Cnil,
                                Cnil, Cnil, Cnil);
        } else {
                cl_object stream = cl_open(13, filename,
                                           @':direction', direction,
                                           @':element-type', element_type,
                                           @':if-exists', if_exists,
                                           @':if-does-not-exist', if_does_not_exist,
                                           @':external-format', @':pass-through',
                                           @':cstream', Ct);
                if (Null(length))
                        length = ecl_file_length(stream);
                output = si_make_vector(element_type, length, Cnil,
                                        Cnil, Cnil, Cnil);
                cl_read_sequence(2, output, stream);
                cl_close(1, stream);
        }
        @(return output)
}
#endif
@)

cl_object
si_mmap_array(cl_object map)
{
#ifdef HAVE_SYS_MMAN_H
        @(return cl_car(map));
#else
        @(return map);
#endif
}

cl_object
si_munmap(cl_object map)
{
#ifdef HAVE_SYS_MMAN_H
        cl_object array = cl_car(map);
        cl_object stream = cl_cdr(map);
        int code = munmap(array->base_string.self, array->base_string.dim);
        if (code < 0) {
                FElibc_error("Error when unmapping file.", 0);
        }
        cl_close(1, stream);
#endif
        @(return Cnil)
}
