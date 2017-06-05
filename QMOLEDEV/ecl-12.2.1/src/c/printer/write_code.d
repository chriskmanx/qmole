/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    write_list.d -- ugly printer for bytecodes and functions
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
#include <ecl/bytecodes.h>

void
_ecl_write_bclosure(cl_object x, cl_object stream)
{
        if (ecl_print_readably()) {
                cl_index i;
                cl_object lex = x->bclosure.lex;
                cl_object code_l=Cnil, data_l=Cnil;
                x = x->bclosure.code;
                for ( i=x->bytecodes.code_size-1 ; i<(cl_index)(-1l) ; i-- )
                        code_l = ecl_cons(MAKE_FIXNUM(((cl_opcode*)(x->bytecodes.code))[i]), code_l);
                for ( i=x->bytecodes.data_size-1 ; i<(cl_index)(-1l) ; i-- )
                        data_l = ecl_cons(x->bytecodes.data[i], data_l);
                
                writestr_stream("#Y", stream);
                si_write_ugly_object(cl_list(7, x->bytecodes.name, lex,
                                             Cnil /* x->bytecodes.definition */,
                                             code_l, data_l,
                                             x->bytecodes.file,
                                             x->bytecodes.file_position),
                                     stream);
        } else {
                cl_object name = x->bytecodes.name;
                writestr_stream("#<bytecompiled-closure ", stream);
                if (name != Cnil)
                        si_write_ugly_object(name, stream);
                else
                        _ecl_write_addr(x, stream);
                ecl_write_char('>', stream);
        }
}

void
_ecl_write_bytecodes(cl_object x, cl_object stream)
{
        if (ecl_print_readably()) {
                cl_index i;
                cl_object lex = Cnil;
                cl_object code_l=Cnil, data_l=Cnil;
                for ( i=x->bytecodes.code_size-1 ; i<(cl_index)(-1l) ; i-- )
                        code_l = ecl_cons(MAKE_FIXNUM(((cl_opcode*)(x->bytecodes.code))[i]), code_l);
                for ( i=x->bytecodes.data_size-1 ; i<(cl_index)(-1l) ; i-- )
                        data_l = ecl_cons(x->bytecodes.data[i], data_l);
                writestr_stream("#Y", stream);
                si_write_ugly_object(cl_list(7, x->bytecodes.name, lex,
                                             Cnil /* x->bytecodes.definition */,
                                             code_l, data_l,
                                             x->bytecodes.file,
                                             x->bytecodes.file_position),
                                     stream);
        } else {
                cl_object name = x->bytecodes.name;
                writestr_stream("#<bytecompiled-function ", stream);
                if (name != Cnil)
                        si_write_ugly_object(name, stream);
                else
                        _ecl_write_addr(x, stream);
                ecl_write_char('>', stream);
        }
}
