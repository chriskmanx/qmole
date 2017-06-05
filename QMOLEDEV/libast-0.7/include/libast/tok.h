/*
 * Copyright (C) 1997-2004, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without retokiction, including without limitation the
 * rights to use, copy, modify, merge, publish, ditokibute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef _LIBAST_TOK_H_
#define _LIBAST_TOK_H_

#include <libast/list_if.h>

/* Cast an arbitrary object pointer to a tok. */
#define SPIF_TOK(obj)                (SPIF_CAST(tok) (obj))

/* Check to see if a pointer references a tokenizer object. */
#define SPIF_OBJ_IS_TOK(obj)         (SPIF_OBJ_IS_TYPE(obj, tok))

/* Check to see if a tokenizer object is NULL. */
#define SPIF_TOK_ISNULL(obj)         (SPIF_TOK(obj) == SPIF_NULL_TYPE(tok))

#define SPIF_TOK_NEW(type)           SPIF_TOK((SPIF_CLASS(SPIF_CLASS_VAR(type)))->noo())
#define SPIF_TOK_INIT(obj)           SPIF_OBJ_INIT(obj)
#define SPIF_TOK_DONE(obj)           SPIF_OBJ_DONE(obj)
#define SPIF_TOK_DEL(obj)            SPIF_OBJ_DEL(obj)
#define SPIF_TOK_SHOW(obj, b, i)     SPIF_OBJ_SHOW(obj, b, i)
#define SPIF_TOK_COMP(o1, o2)        SPIF_OBJ_COMP(o1, o2)
#define SPIF_TOK_DUP(obj)            SPIF_OBJ_DUP(obj)
#define SPIF_TOK_TYPE(obj)           SPIF_OBJ_TYPE(obj)

#define SPIF_TOK_LIST(obj)           SPIF_LIST(SPIF_TOK(obj)->tokens)


/* An tok object is a string tokenizer */
SPIF_DECL_OBJ(tok) {
    SPIF_DECL_PARENT_TYPE(obj);
    SPIF_DECL_PROPERTY(str, src);
    SPIF_DECL_PROPERTY(char, quote);
    SPIF_DECL_PROPERTY(char, dquote);
    SPIF_DECL_PROPERTY(char, escape);
    SPIF_DECL_PROPERTY(str, sep);
    SPIF_DECL_PROPERTY(list, tokens);
};

extern spif_class_t SPIF_CLASS_VAR(tok);
extern spif_tok_t spif_tok_new(void);
extern spif_tok_t spif_tok_new_from_ptr(spif_charptr_t);
extern spif_tok_t spif_tok_new_from_fp(FILE *);
extern spif_tok_t spif_tok_new_from_fd(int);
extern spif_bool_t spif_tok_del(spif_tok_t);
extern spif_bool_t spif_tok_init(spif_tok_t);
extern spif_bool_t spif_tok_init_from_ptr(spif_tok_t, spif_charptr_t);
extern spif_bool_t spif_tok_init_from_fp(spif_tok_t, FILE *);
extern spif_bool_t spif_tok_init_from_fd(spif_tok_t, int);
extern spif_bool_t spif_tok_done(spif_tok_t);
extern spif_bool_t spif_tok_eval(spif_tok_t);
extern spif_str_t spif_tok_show(spif_tok_t, spif_charptr_t, spif_str_t, size_t);
extern spif_cmp_t spif_tok_comp(spif_tok_t, spif_tok_t);
extern spif_tok_t spif_tok_dup(spif_tok_t);
extern spif_classname_t spif_tok_type(spif_tok_t);
SPIF_DECL_PROPERTY_FUNC(tok, str, src);
SPIF_DECL_PROPERTY_FUNC(tok, char, quote);
SPIF_DECL_PROPERTY_FUNC(tok, char, dquote);
SPIF_DECL_PROPERTY_FUNC(tok, char, escape);
SPIF_DECL_PROPERTY_FUNC(tok, str, sep);
SPIF_DECL_PROPERTY_FUNC(tok, list, tokens);

#endif /* _LIBAST_TOK_H_ */
