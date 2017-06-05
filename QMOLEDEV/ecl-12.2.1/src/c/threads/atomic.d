/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    atomic.d -- atomic operations.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/internal.h>

#ifdef ECL_THREADS

cl_object
ecl_atomic_push(cl_object *slot, cl_object c)
{
        cl_object cons = ecl_list1(c);
        cl_object car;
        do {
                car = AO_load(slot);
                ECL_RPLACD(cons, car);
        } while (!AO_compare_and_swap_full(slot, car, cons));
        return cons;
}

bool
ecl_compare_and_swap(cl_object *slot, cl_object old, cl_object new)
{
        return AO_compare_and_swap_full(slot, car, cons);
}


#endif /* ECL_THREADS */
