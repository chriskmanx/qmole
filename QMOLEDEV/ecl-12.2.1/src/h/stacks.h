/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    stacks.h -- Bind/Jump/Frame stacks.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2000, Juan Jose Garcia-Ripoll

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifdef __cplusplus
extern "C" {
#endif

/***********
 * C STACK
 ***********/

#ifdef ECL_DOWN_STACK
#define ecl_cs_check(env,var) \
        if (ecl_unlikely((char*)(&var) <= (env)->cs_limit)) ecl_cs_overflow()
#else
#define ecl_cs_check(env,var) \
        if (ecl_unlikely((char*)(&var) >= (env)->cs_limit)) ecl_cs_overflow()
#endif

/**************
 * BIND STACK
 **************/

typedef struct bds_bd {
	cl_object symbol;	/*  symbol  */
	cl_object value;	/*  previous value of the symbol  */
} *bds_ptr;

#define	ecl_bds_check(env) \
	(ecl_unlikely(env->bds_top >= env->bds_limit)? (ecl_bds_overflow(),1) : 0)

#define ECL_MISSING_SPECIAL_BINDING (~((cl_index)0))

extern ECL_API struct bds_bd *ecl_bds_overflow(void);
extern ECL_API void ecl_bds_bind(cl_env_ptr env, cl_object symbol, cl_object v);
extern ECL_API void ecl_bds_push(cl_env_ptr env, cl_object symbol);
extern ECL_API void ecl_bds_unwind1(cl_env_ptr env);
extern ECL_API void ecl_bds_unwind_n(cl_env_ptr env, int n);
#ifdef ECL_THREADS
extern ECL_API cl_object ecl_bds_read(cl_env_ptr env, cl_object s);
extern ECL_API cl_object ecl_bds_set(cl_env_ptr env, cl_object s, cl_object v);
# define ECL_SYM_VAL(env,s) (ecl_bds_read(env,s))
# define ECL_SET(s,v) ((s)->symbol.value=(v))
# define ECL_SETQ(env,s,v) (ecl_bds_set(env,s,v))
#else
# define ECL_SYM_VAL(env,s) ((s)->symbol.value)
# define ECL_SET(s,v) ((s)->symbol.value=(v))
# define ECL_SETQ(env,s,v) ((s)->symbol.value=(v))
#endif

#ifdef __GNUC__
static inline void ecl_bds_bind_inl(cl_env_ptr env, cl_object s, cl_object v)
{
        struct bds_bd *slot;
# ifdef ECL_THREADS
        cl_object *location;
        const cl_index index = s->symbol.binding;
        if (index >= env->thread_local_bindings_size) {
                ecl_bds_bind(env,s,v);
        } else {
                location = env->thread_local_bindings + index;
                slot = ++env->bds_top;
                if (slot >= env->bds_limit) slot = ecl_bds_overflow();
                slot->symbol = s;
                slot->value = *location;
                *location = v;
        }
# else
        slot = ++env->bds_top;
        if (slot >= env->bds_limit) slot = ecl_bds_overflow();
        slot->symbol = s;
	slot->value = s->symbol.value;
	s->symbol.value = v;
# endif /* !ECL_THREADS */
}

static inline void ecl_bds_push_inl(cl_env_ptr env, cl_object s)
{
        struct bds_bd *slot;
# ifdef ECL_THREADS
        cl_object *location;
        const cl_index index = s->symbol.binding;
        if (index >= env->thread_local_bindings_size) {
                ecl_bds_push(env, s);
        } else {
                location = env->thread_local_bindings + index;
                slot = ++env->bds_top;
                if (slot >= env->bds_limit) slot = ecl_bds_overflow();
                slot->symbol = s;
                slot->value = *location;
                if (!(*location)) *location = s->symbol.value;
        }
# else
        slot = ++env->bds_top;
        if (slot >= env->bds_limit) slot = ecl_bds_overflow();
        slot->symbol = s;
	slot->value = s->symbol.value;
# endif /* !ECL_THREADS */
}

static inline void ecl_bds_unwind1_inl(cl_env_ptr env)
{
	struct bds_bd *slot = env->bds_top--;
	cl_object s = slot->symbol;
# ifdef ECL_THREADS
        cl_object *location = env->thread_local_bindings + s->symbol.binding;
        *location = slot->value;
# else
        s->symbol.value = slot->value;
# endif
}

# ifdef ECL_THREADS
static inline cl_object ecl_bds_read_inl(cl_env_ptr env, cl_object s)
{
        cl_index index = s->symbol.binding;
        if (index < env->thread_local_bindings_size) {
                cl_object x = env->thread_local_bindings[index];
                if (x) return x;
        }
        return s->symbol.value;
}
static inline cl_object ecl_bds_set_inl(cl_env_ptr env, cl_object s, cl_object v)
{
        cl_index index = s->symbol.binding;
        if (index < env->thread_local_bindings_size) {
                cl_object *location = env->thread_local_bindings + index;
                if (*location) return *location = v;
        }
        return s->symbol.value = v;
}
#  define ecl_bds_read ecl_bds_read_inl
#  define ecl_bds_set ecl_bds_set_inl
# endif
# define ecl_bds_bind ecl_bds_bind_inl
# define ecl_bds_push ecl_bds_push_inl
# define ecl_bds_unwind1 ecl_bds_unwind1_inl
#else /* !__GNUC__ */
# ifndef ECL_THREADS
#  define ecl_bds_bind(env,sym,val) do {	\
	const cl_env_ptr env_copy = (env);	\
	const cl_object s = (sym);		\
	const cl_object v = (val);		\
	ecl_bds_check(env_copy);		\
	(++(env_copy->bds_top))->symbol = s,	\
	env_copy->bds_top->value = s->symbol.value; \
	s->symbol.value = v; } while (0)
#  define ecl_bds_push(env,sym) do {	\
	const cl_env_ptr env_copy = (env);	\
	const cl_object s = (sym);		\
	const cl_object v = s->symbol.value;	\
	ecl_bds_check(env_copy);		\
	(++(env_copy->bds_top))->symbol = s,	\
	env_copy->bds_top->value = s->symbol.value; } while (0);
#  define ecl_bds_unwind1(env)  do {	\
	const cl_env_ptr env_copy = (env); 		\
	const cl_object s = env_copy->bds_top->symbol;	\
	s->symbol.value = env_copy->bds_top->value;	\
	--(env_copy->bds_top); } while (0)
# endif /* !ECL_THREADS */
#endif /* !__GNUC__ */

/****************************
 * INVOCATION HISTORY STACK
 ****************************/

typedef struct ihs_frame {
	struct ihs_frame *next;
	cl_object function;
	cl_object lex_env;
	cl_index index;
        cl_index bds;
} *ihs_ptr;

#define ecl_ihs_push(env,rec,fun,lisp_env) do { \
	const cl_env_ptr __the_env = (env);	\
	struct ihs_frame * const r = (rec);	\
	r->next=__the_env->ihs_top;		\
	r->function=(fun);			\
	r->lex_env=(lisp_env);			\
	r->index=__the_env->ihs_top->index+1;	\
        r->bds=__the_env->bds_top - __the_env->bds_org; \
	__the_env->ihs_top = r; 		\
} while(0)

#define ecl_ihs_pop(env) do {				\
	const cl_env_ptr __the_env = (env);		\
	struct ihs_frame *r = __the_env->ihs_top;	\
	if (r) __the_env->ihs_top = r->next;		\
} while(0)

extern ECL_API cl_object ihs_top_function_name(void);

/***************
 * FRAME STACK
 ***************/
/* Frames signal points in the code to which we can at any time jump.
 * Frames are established, for instance, by CATCH, BLOCK, TAGBODY,
 * LAMBDA, UNWIND-PROTECT, etc.
 *
 * Frames are established by ecl_frs_push(). For each call to ecl_frs_push()
 * there must be a corresponding ecl_frs_pop(). More precisely, since our
 * frame mechanism relies on the C stack and on the setjmp/longjmp
 * functions, any function that creates a frame must also destroy it
 * with ecl_frs_pop() before returning.
 *
 * Frames are identified by a value frs_val. This can be either a
 * unique identifier, created for each CATCH, BLOCK, etc, or a common
 * one ECL_PROTECT_TAG, used by UNWIND-PROTECT forms. The first type
 * of frames can be target of a search ecl_frs_sch() and thus one can jump
 * to them. The second type of frames are like barriers designed to
 * intercept the jumps to the outer frames and are called
 * automatically by the function unwind() whenever it jumps to a frame
 * which is beyond one of these barriers.
 */

typedef struct ecl_frame {
	jmp_buf		frs_jmpbuf;
	cl_object	frs_val;
	cl_index	frs_bds_top_index;
	ihs_ptr		frs_ihs;
	cl_index	frs_sp;
} *ecl_frame_ptr;

extern ECL_API ecl_frame_ptr _ecl_frs_push(register cl_env_ptr, register cl_object);
#define ecl_frs_push(env,val)  ecl_setjmp(_ecl_frs_push(env,val)->frs_jmpbuf)
#define ecl_frs_pop(env) ((env)->frs_top--)

/*******************
 * ARGUMENTS STACK
 *******************
 * Here we define how we handle the incoming arguments for a
 * function. Our calling conventions specify that at most
 * C_ARGUMENTS_LIMIT ar pushed onto the C stack. If the function
 * receives more than this number of arguments it will keep a copy of
 * _all_ those arguments _plus_ the remaining ones in the lisp
 * stack. The caller is responsible for storing and removing such
 * values.
 *
 * Given this structure, we need our own object for handling variable
 * argument list, cl_va_list. This object joins the C data type for
 * handling vararg lists and a pointer to the lisp stack, in case the
 * arguments were passed there.
 *
 * Note that keeping a direct reference to the lisp stack effectively
 * locks it in memory, preventing the block from being garbage
 * collected if the stack grows -- at least until all references are
 * eliminated --. This is something we have to live with and which
 * is somehow unavoidable, given that function arguments have to be
 * stored somewhere.
 */

#define cl_va_start(a,p,n,k) { \
	a[0].narg = (n)-(k); \
	va_start(a[0].args,p); \
	a[0].sp = ((n) <= C_ARGUMENTS_LIMIT)? 0 : _ecl_va_sp(a[0].narg); }
#define cl_va_arg(a) \
	(a[0].narg--,(a[0].sp? *(a[0].sp++) : va_arg(a[0].args,cl_object)))
#define cl_va_copy(dest,orig) { \
	dest[0].narg = orig[0].narg; \
	dest[0].sp = orig[0].sp; \
	va_copy(dest[0].args,orig[0].args); \
}
#define cl_va_end(a) \
	va_end(a[0].args)
#define	check_arg(n) \
	do { if (ecl_unlikely(narg != (n))) FEwrong_num_arguments_anonym();} while(0)

/***********************
 * RETURN VALUES STACK
 ***********************/

#define NVALUES		cl_env.nvalues
#define VALUES(n)	cl_env.values[n]
#define return0()	return ((NVALUES = 0),Cnil)
#define return1(x)	return ((VALUES(0)=(x)),(NVALUES=1),VALUES(0))
#define returnn(x)	return x
#define ecl_return0(env) \
	do { (env)->nvalues = 0; return Cnil; } while (0)
#define ecl_return1(env,x) \
	do { cl_object __aux = (x); (env)->nvalues = 0; return __aux; } while (0)

/*****************************
 * LEXICAL ENVIRONMENT STACK
 *****************************/
/*
 * A lexical environment is a list of pairs, each one containing either
 * a variable definition, a tagbody or block tag, or a local function
 * definition.
 *
 *	lex_env ---> ( { record }* )
 *	record = variable | function | block_tag | tagbody_tag
 *
 *	variable = ( var_name[symbol] . value )
 *	function = ( function[bytecodes] . fun_name[symbol] )
 *	block_tag = ( tag[fixnum] . block_name[symbol] )
 *	tagbody_tag = ( tag[fixnum] . 0 )
 */

/*************
 * LISP STACK
 *************/

#define ECL_STACK_INDEX(env) ((env)->stack_top - (env)->stack)

#define ECL_STACK_PUSH(the_env,o) do {                                  \
                const cl_env_ptr __env = (the_env);                     \
                cl_object *__new_top = __env->stack_top;                \
                if (ecl_unlikely(__new_top >= __env->stack_limit)) {    \
                        __new_top = ecl_stack_grow(__env);              \
                }                                                       \
                *__new_top = (o);                                       \
                __env->stack_top = __new_top+1; } while (0)

#define ECL_STACK_POP_UNSAFE(env) *(--((env)->stack_top))

#define ECL_STACK_REF(env,n) ((env)->stack_top[n])

#define ECL_STACK_SET_INDEX(the_env,ndx) do {                   \
                const cl_env_ptr __env = (the_env);             \
                cl_object *__new_top = __env->stack + (ndx);    \
                if (ecl_unlikely(__new_top > __env->stack_top)) \
                        FEstack_advance();                      \
                __env->stack_top = __new_top; } while (0)

#define ECL_STACK_POP_N(the_env,n) do {                         \
                const cl_env_ptr __env = (the_env);             \
                cl_object *__new_top = __env->stack_top - (n);  \
                if (ecl_unlikely(__new_top < __env->stack))     \
                        FEstack_underflow();                    \
                __env->stack_top = __new_top; } while (0)

#define ECL_STACK_POP_N_UNSAFE(the_env,n) ((the_env)->stack_top -= (n))

#define ECL_STACK_PUSH_N(the_env,n) do {                                \
                const cl_env_ptr __env = (the_env) ;                    \
                cl_index __aux = (n);                                   \
                cl_object *__new_top = __env->stack_top;                \
                while (ecl_unlikely((__env->stack_limit - __new_top) <= __aux)) { \
                        __new_top = ecl_stack_grow(__env);              \
                }                                                       \
                __env->stack_top = __new_top + __aux; } while (0)

#define ECL_STACK_FRAME_COPY(dest,orig) do {                            \
                cl_object __dest = (dest);                              \
                cl_object __orig = (orig);                              \
                cl_index __size = __orig->frame.size;                   \
                ecl_stack_frame_open(__orig->frame.env, __dest, __size); \
                memcpy(__dest->frame.base, __orig->frame.base, __size * sizeof(cl_object)); \
        } while (0);

#define ECL_STACK_FRAME_SET(f,ndx,o) do { (f)->frame.base[(ndx)] = (o); } while(0)
#define ECL_STACK_FRAME_REF(f,ndx) ((f)->frame.base[(ndx)])

/*********************************
 * HIGH LEVEL CONTROL STRUCTURES *
 *********************************/

#define CL_NEWENV_BEGIN {\
	const cl_env_ptr the_env = ecl_process_env(); \
	cl_index __i = ecl_stack_push_values(the_env); \

#define CL_NEWENV_END \
	ecl_stack_pop_values(the_env,__i); }

#define CL_UNWIND_PROTECT_BEGIN(the_env) do {	   \
	bool __unwinding; ecl_frame_ptr __next_fr; \
	const cl_env_ptr __the_env = (the_env);	   \
	cl_index __nr; \
	if (ecl_frs_push(__the_env,ECL_PROTECT_TAG)) {	\
		__unwinding=1; __next_fr=__the_env->nlj_fr; \
	} else {

#define CL_UNWIND_PROTECT_EXIT \
	__unwinding=0; } \
	ecl_frs_pop(__the_env); \
	__nr = ecl_stack_push_values(__the_env);

#define CL_UNWIND_PROTECT_END \
	ecl_stack_pop_values(__the_env,__nr);	\
	if (__unwinding) ecl_unwind(__the_env,__next_fr); } while(0)

#define ECL_NEW_FRAME_ID(env) MAKE_FIXNUM(env->frame_id++)

#define CL_BLOCK_BEGIN(the_env,id) do {   			\
	const cl_object __id = ECL_NEW_FRAME_ID(the_env);	\
	const cl_env_ptr __the_env = (the_env);			\
	if (ecl_frs_push(__the_env,__id) == 0)

#define CL_BLOCK_END \
	ecl_frs_pop(__the_env); } while(0)

#define CL_CATCH_BEGIN(the_env,tag) do {	\
	const cl_env_ptr __the_env = (the_env);	\
	if (ecl_frs_push(__the_env,tag) == 0) {

#define CL_CATCH_END } \
	ecl_frs_pop(__the_env); } while (0)

#if defined(_MSC_VER)
# define CL_CATCH_ALL_BEGIN(the_env) do {			\
	const cl_env_ptr __the_env = (the_env);			\
	_try {							\
	const cl_env_ptr __the_env = (the_env);			\
	if (ecl_frs_push(__the_env,ECL_PROTECT_TAG) == 0) {
# define CL_CATCH_ALL_IF_CAUGHT } else {
# define CL_CATCH_ALL_END }}						\
	_except(_ecl_w32_exception_filter(GetExceptionInformation())) \
	{ (void)0; }							\
	ecl_frs_pop(__the_env); } while(0)
#else
# define CL_CATCH_ALL_BEGIN(the_env) do {	\
	const cl_env_ptr __the_env = (the_env);	\
	if (ecl_frs_push(__the_env,ECL_PROTECT_TAG) == 0) {
# define CL_CATCH_ALL_IF_CAUGHT } else {
# define CL_CATCH_ALL_END } \
	ecl_frs_pop(__the_env); } while(0)
#endif


#ifdef __cplusplus
}
#endif
