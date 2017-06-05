/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    object.h  -- Data structure definitions.
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

#ifdef __cplusplus
extern "C" {
#endif

/*
	Integer and boolean types (see config.h)
*/

#define	TRUE		1	/*  boolean true value  */
#define	FALSE		0	/*  boolean false value  */

#if !defined(__cplusplus) && !defined(bool)
typedef int bool;
#endif
typedef unsigned char byte;

        /* #define ECL_EXTERNALIZABLE */

/*
	Implementation types.
        Verify that it matches printer/write_ugly.d
*/
typedef enum {
	t_start = 0,
	t_list = 1,
	/* The most specific numeric types come first. Assumed by
	   some routines, like cl_expt */
	t_character = 2,	/* immediate character */
	t_fixnum = 3,		/* immediate fixnum */
	t_bignum = 4,
	t_ratio,
	t_singlefloat,
	t_doublefloat,
#ifdef ECL_LONG_FLOAT
	t_longfloat,
#endif
	t_complex,
	t_symbol,
	t_package,
	t_hashtable,
	t_array,
	t_vector,
#ifdef ECL_UNICODE
	t_string,
#endif
	t_base_string,
	t_bitvector,
	t_stream,
	t_random,
	t_readtable,
	t_pathname,
	t_bytecodes,
	t_bclosure,
	t_cfun,
	t_cfunfixed,
	t_cclosure,
#ifdef CLOS
	t_instance,
#else
	t_structure,
#endif /* CLOS */
#ifdef ECL_THREADS
	t_process,
	t_lock,
	t_rwlock,
	t_condition_variable,
# ifdef ECL_SEMAPHORES
        t_semaphore,
# endif
#endif
	t_codeblock,
	t_foreign,
	t_frame,
	t_weak_pointer,
#ifdef ECL_SSE2
	t_sse_pack,
#endif
	t_end,
	t_other,
	t_contiguous,		/*  contiguous block  */
	FREE = 127		/*  free object  */
} cl_type;


/*
	Definition of the type of LISP objects.
*/
typedef union cl_lispunion *cl_object;
typedef cl_object cl_return;
typedef cl_fixnum cl_narg;
typedef cl_object (*cl_objectfn)(cl_narg narg, ...);
typedef cl_object (*cl_objectfn_fixed)();

/*
	OBJect NULL value.
	It should not coincide with any legal object value.
*/
#define	OBJNULL		((cl_object)NULL)

/*
	Definition of each implementation type.
*/

#define ECL_TAG_BITS		2
#define IMMEDIATE(o)		((cl_fixnum)(o) & 3)
#define IMMEDIATE_TAG		3

#define ecl_to_bool(x) ((x)!=Cnil)
#define ecl_make_bool(x) ((x)? Ct : Cnil)

/* Immediate fixnums:		*/
#define FIXNUM_TAG		t_fixnum
#define MAKE_FIXNUM(n)		((cl_object)(((cl_fixnum)(n) << 2) | FIXNUM_TAG))
#define FIXNUM_MINUSP(n)	((cl_fixnum)(n) < 0)
#define FIXNUM_PLUSP(n)		((cl_fixnum)(n) >= (cl_fixnum)MAKE_FIXNUM(0))
#define	fix(obje)		(((cl_fixnum)(obje)) >> 2)
#define FIXNUMP(o)		(IMMEDIATE(o) == t_fixnum)

#define ECL_FIXNUMP(o)          (IMMEDIATE(o) == t_fixnum)
#define ecl_fixnum_lower(a,b)   ((cl_fixnum)(a) < (cl_fixnum)(b))
#define ecl_fixnum_greater(a,b) ((cl_fixnum)(a) > (cl_fixnum)(b))
#define ecl_fixnum_leq(a,b)     ((cl_fixnum)(a) <= (cl_fixnum)(b))
#define ecl_fixnum_geq(a,b)     ((cl_fixnum)(a) >= (cl_fixnum)(b))
#define ecl_fixnum_plusp(a)     ((cl_fixnum)(a) > (cl_fixnum)MAKE_FIXNUM(0))
#define ecl_fixnum_minusp(a)    ((cl_fixnum)(a) < (cl_fixnum)(0))
#define ecl_fix(a)              (((cl_fixnum)(a)) >> 2)

/* Immediate characters:	*/
#define CHARACTER_TAG		t_character
#define CHARACTERP(o)		(IMMEDIATE(o) == t_character)
#ifdef ECL_UNICODE
#define BASE_CHAR_P(obje)	((((cl_fixnum)(obje)) & 0xFFFFFC03) == CHARACTER_TAG)
#define BASE_CHAR_CODE_P(x)	((x & ~((cl_fixnum)0xFF)) == 0)
#define	CODE_CHAR(c)		((cl_object)(((cl_fixnum)(c << 2)|CHARACTER_TAG)))
#define	CHAR_CODE(obje)		(((cl_fixnum)(obje)) >> 2)
#else
#define	CODE_CHAR(c)		((cl_object)(((cl_fixnum)((c & 0xff) << 2)|CHARACTER_TAG)))
#define	CHAR_CODE(obje)		((((cl_fixnum)(obje)) >> 2) & 0xff)
#endif
#define ECL_CHAR_CODE_RETURN	13
#define ECL_CHAR_CODE_NEWLINE	10
#define ECL_CHAR_CODE_LINEFEED	10

#define ECL_NUMBER_TYPE_P(t)	(t >= t_fixnum && t <= t_complex)
#define REAL_TYPE(t)		(t >= t_fixnum && t < t_complex)
#define ARRAY_TYPE(t)		(t >= t_array && t <= t_bitvector)
#define ECL_ARRAYP(x)		((IMMEDIATE(x) == 0) && (x)->d.t >= t_array && (x)->d.t <= t_bitvector)
#define ECL_VECTORP(x)		((IMMEDIATE(x) == 0) && (x)->d.t >= t_vector && (x)->d.t <= t_bitvector)
#define ECL_BIT_VECTOR_P(x)     ((IMMEDIATE(x) == 0) && ((x)->d.t == t_bitvector))
#ifdef ECL_UNICODE
#define ECL_STRINGP(x)		((IMMEDIATE(x) == 0) && \
                                 ((x)->d.t == t_base_string || (x)->d.t == t_string))
#define ECL_EXTENDED_STRING_P(x) ((IMMEDIATE(x) == 0) && (x)->d.t == t_string)
#else
#define ECL_STRINGP(x)		((IMMEDIATE(x) == 0) && ((x)->d.t == t_base_string))
#define ECL_EXTENDED_STRING_P(x) 0
#endif
#define ECL_BASE_STRING_P(x)	((IMMEDIATE(x) == 0) && ((x)->d.t == t_base_string))
#define ECL_HASH_TABLE_P(x)	((IMMEDIATE(x) == 0) && ((x)->d.t == t_hashtable))
#define ECL_BIGNUMP(x)          ((IMMEDIATE(x) == 0) && ((x)->d.t == t_bignum))
#define ECL_COMPLEXP(x)         ((IMMEDIATE(x) == 0) && ((x)->d.t == t_complex))
#define ECL_RANDOM_STATE_P(x)   ((IMMEDIATE(x) == 0) && ((x)->d.t == t_random))
#define ECL_SINGLE_FLOAT_P(x)   ((IMMEDIATE(x) == 0) && ((x)->d.t == t_singlefloat))
#define ECL_DOUBLE_FLOAT_P(x)   ((IMMEDIATE(x) == 0) && ((x)->d.t == t_doublefloat))
#ifdef ECL_LONG_FLOAT
#define ECL_LONG_FLOAT_P(x)     ((IMMEDIATE(x) == 0) && ((x)->d.t == t_longfloat))
#endif
#define ECL_PACKAGEP(x)         ((IMMEDIATE(x) == 0) && ((x)->d.t == t_package))
#define ECL_PATHNAMEP(x)        ((IMMEDIATE(x) == 0) && ((x)->d.t == t_pathname))
#define ECL_READTABLEP(x)       ((IMMEDIATE(x) == 0) && ((x)->d.t == t_readtable))
#define ECL_FOREIGN_DATA_P(x)   ((IMMEDIATE(x) == 0) && ((x)->d.t == t_foreign))
#ifdef ECL_SSE2
#define ECL_SSE_PACK_P(x)       ((IMMEDIATE(x) == 0) && ((x)->d.t == t_sse_pack))
#endif

#define HEADER			int8_t t, m, padding1, padding2
#define HEADER1(field)		int8_t t, m, field, padding
#define HEADER2(field1,field2)	int8_t t, m, field1, field2

struct ecl_singlefloat {
	HEADER;
	float SFVAL;	/*  singlefloat value  */
};
#define	sf(obje)	(obje)->SF.SFVAL
#define ecl_single_float(o) ((o)->SF.SFVAL)

struct ecl_doublefloat {
	HEADER;
	double DFVAL;	/*  doublefloat value  */
};
#define	df(obje)	(obje)->DF.DFVAL
#define ecl_double_float(o) ((o)->DF.DFVAL)

#ifdef ECL_LONG_FLOAT
struct ecl_long_float {
	HEADER;
	long double value;
};
#define ecl_long_float(o) ((o)->longfloat.value)
#endif

struct ecl_bignum {
	HEADER;
	mpz_t big_num;
};
#define big_dim		big_num->_mp_alloc
#define big_size	big_num->_mp_size
#define big_limbs	big_num->_mp_d

struct ecl_ratio {
	HEADER;
	cl_object den;		/*  denominator, must be an integer  */
	cl_object num;		/*  numerator, must be an integer  */
};

#ifdef _MSC_VER
#undef complex			/* Otherwise we cannot do x->complex.real */
#endif
struct ecl_complex {
	HEADER;
	cl_object real;		/*  real part, must be a number  */
	cl_object imag;		/*  imaginary part, must be a number  */
};

enum ecl_stype {		/*  symbol type  */
	stp_ordinary = 0,
	stp_constant = 1,
        stp_special = 2,
	stp_macro = 4,
	stp_special_form = 8
};

#define	Cnil			((cl_object)t_list)
#define	Cnil_symbol		((cl_object)cl_symbols)
#define	Ct			((cl_object)(cl_symbols+1))
#define ECL_UNBOUND		((cl_object)(cl_symbols+2))
#define ECL_PROTECT_TAG		((cl_object)(cl_symbols+3))

#define ECL_FLAG_MACRO        1
#define ECLF_LAG_SPECIAL_VAR  4

struct ecl_symbol {
	HEADER2(stype, dynamic);/*  symbol type, special-variable-p */
	cl_object value;	/*  global value of the symbol  */
				/*  Coincides with cons.car  */
	cl_object gfdef;	/*  global function definition  */
				/*  For a macro,  */
				/*  its expansion function  */
				/*  is to be stored.  */
				/*  Coincides with cons.cdr  */
	cl_object plist;	/*  property list  */
				/*  This field coincides with cons.car  */
	cl_object name;		/*  print name  */
	cl_object hpack;	/*  home package  */
				/*  Cnil for uninterned symbols  */
#ifdef ECL_THREADS
        cl_index binding;	/*  index into the bindings array  */
#endif
};
#define SYM_FUN(sym)	((sym)->symbol.gfdef)

struct ecl_package {
	HEADER1(locked);
	cl_object name;		/*  package name, a string  */
	cl_object nicknames;	/*  nicknames, list of strings  */
	cl_object shadowings;	/*  shadowing symbol list  */
	cl_object uses;		/*  use-list of packages  */
	cl_object usedby;	/*  used-by-list of packages  */
	cl_object internal;	/*  hashtable for internal symbols  */
	cl_object external;	/*  hashtable for external symbols  */
};

/*
	The values returned by intern and find_symbol.
	File_symbol may return 0.
*/
#define	INTERNAL	1
#define	EXTERNAL	2
#define	INHERITED	3

/*
 * CONSES
 *
 * We implement two variants. The "small cons" type carries the type
 * information in the least significant bits of the pointer. We have
 * to do some pointer arithmetics to find out the CAR / CDR of the
 * cons but the overall result is faster and memory efficient, only
 * using two words per cons.
 *
 * The other scheme stores conses as three-words objects, the first
 * word carrying the type information. This is kept for backward
 * compatibility and also because the oldest garbage collector does
 * not yet support the smaller datatype.
 *
 * To make code portable and independent of the representation, only
 * access the objects using the common macros below (that is all
 * except ECL_CONS_PTR or ECL_PTR_CONS).
 */

#ifdef ECL_SMALL_CONS
#define ECL_LISTP(x)    (IMMEDIATE(x) == t_list)
#define ECL_CONSP(x)    (LISTP(x) && !Null(x))
#define ECL_ATOM(x)     (Null(x) || !LISTP(x))
#define ECL_SYMBOLP(x)  (Null(x) || ((IMMEDIATE(x) == 0) && ((x)->d.t == t_symbol)))

#define LISTP(x)	(IMMEDIATE(x) == t_list)
#define CONSP(x)	(LISTP(x) && !Null(x))
#define ATOM(x)		(Null(x) || !LISTP(x))
#define SYMBOLP(x)	(Null(x) || ((IMMEDIATE(x) == 0) && ((x)->d.t == t_symbol)))

#define ECL_PTR_CONS(x)	(cl_object)((char*)(x) + t_list)
#define ECL_CONS_PTR(x)	((struct ecl_cons *)((char *)(x) - t_list))
#define ECL_CONS_CAR(x)	(*(cl_object*)((char *)(x) - t_list))
#define ECL_CONS_CDR(x)	(*(cl_object*)((char *)(x) + sizeof(cl_object) - t_list))
#define ECL_RPLACA(x,v)	(ECL_CONS_CAR(x)=(v))
#define ECL_RPLACD(x,v)	(ECL_CONS_CDR(x)=(v))

struct ecl_cons {
	cl_object car;		/*  car  */
	cl_object cdr;		/*  cdr  */
};
#else
#define ECL_LISTP(x)    (IMMEDIATE(x)? Null(x) : ((x)->d.t == t_list))
#define ECL_CONSP(x)    ((IMMEDIATE(x) == 0) && ((x)->d.t == t_list))
#define ECL_ATOM(x)     (IMMEDIATE(x) || ((x)->d.t != t_list))
#define ECL_SYMBOLP(x)  (Null(x) || ((IMMEDIATE(x) == 0) && ((x)->d.t == t_symbol)))

#define LISTP(x)	(IMMEDIATE(x)? Null(x) : ((x)->d.t == t_list))
#define CONSP(x)	((IMMEDIATE(x) == 0) && ((x)->d.t == t_list))
#define ATOM(x)		(IMMEDIATE(x) || ((x)->d.t != t_list))
#define SYMBOLP(x)	(Null(x) || ((IMMEDIATE(x) == 0) && ((x)->d.t == t_symbol)))

#define ECL_CONS_CAR(x)	((x)->cons.car)
#define ECL_CONS_CDR(x)	((x)->cons.cdr)
#define ECL_RPLACA(x,v)	(ECL_CONS_CAR(x)=(v))
#define ECL_RPLACD(x,v)	(ECL_CONS_CDR(x)=(v))

struct ecl_cons {
	HEADER;
	cl_object car;		/*  car  */
	cl_object cdr;		/*  cdr  */
};
#endif

enum ecl_httest {		/*  hash table key test function  */
	htt_eq,			/*  eq  */
	htt_eql,		/*  eql  */
	htt_equal,		/*  equal  */
	htt_equalp,		/*  equalp  */
	htt_pack		/*  symbol hash  */
};

enum ecl_htweak {
	htt_not_weak = 0,
	htt_weak_key,
	htt_weak_value,
	htt_weak_key_and_value
};

struct ecl_hashtable_entry {	/*  hash table entry  */
	cl_object key;		/*  key  */
	cl_object value;	/*  value  */
};

struct ecl_hashtable {		/*  hash table header  */
	HEADER2(test,weak);
	struct ecl_hashtable_entry *data; /*  pointer to the hash table  */
	cl_index entries;	/*  number of entries  */
	cl_index size;		/*  hash table size  */
	cl_index limit;		/*  hash table threshold (integer value)  */
	cl_object rehash_size;	/*  rehash size  */
	cl_object threshold;	/*  rehash threshold  */
	double factor;		/*  cached value of threshold  */
	cl_object (*get)(cl_object, cl_object, cl_object);
	cl_object (*set)(cl_object, cl_object, cl_object);
	bool (*rem)(cl_object, cl_object);
};

typedef enum {			/*  array element type  */
	aet_object,		/*  t                */
	aet_sf,			/*  single-float     */
	aet_df,			/*  double-float     */
	aet_bit,		/*  bit              */
	aet_fix,		/*  cl_fixnum        */
	aet_index,		/*  cl_index         */
	/* Below here, list types accepted by streams (i.e. OPEN) */
	aet_b8,			/*  byte8	     */
	aet_i8,			/*  integer8	     */
#ifdef ecl_uint16_t
        aet_b16, aet_i16,
#endif
#ifdef ecl_uint32_t
        aet_b32, aet_i32,
#endif
#ifdef ecl_uint64_t
        aet_b64, aet_i64,
#endif
#ifdef ECL_UNICODE
	aet_ch,			/*  character        */
#endif
	aet_bc,			/*  base-char        */
	aet_last_type = aet_bc
} cl_elttype;

union ecl_array_data {
	cl_object     *t;
        ecl_base_char *bc;
#ifdef ECL_UNICODE
        ecl_character *c;
#endif
	uint8_t       *b8;
	int8_t        *i8;
#ifdef ecl_uint16_t
        ecl_uint16_t  *b16;
        ecl_int16_t   *i16;
#endif
#ifdef ecl_uint32_t
        ecl_uint32_t  *b32;
        ecl_int32_t   *i32;
#endif
#ifdef ecl_uint64_t
        ecl_uint64_t  *b64;
        ecl_int64_t   *i64;
#endif
	float         *sf;
	double        *df;
	cl_fixnum     *fix;
	cl_index      *index;
	byte          *bit;
};

#define ECL_FLAG_HAS_FILL_POINTER 1
#define ECL_FLAG_ADJUSTABLE       2
#define ECL_ADJUSTABLE_ARRAY_P(x) ((x)->array.flags & ECL_FLAG_ADJUSTABLE)
#define ECL_ARRAY_HAS_FILL_POINTER_P(x) ((x)->array.flags & ECL_FLAG_HAS_FILL_POINTER)

struct ecl_array {		/*  array header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
	HEADER2(elttype,flags);	/*  array element type, has fill ptr, adjustable-p */
	cl_object displaced;	/*  displaced  */
	cl_index dim;		/*  dimension  */
	cl_index *dims;		/*  table of dimensions  */
	union ecl_array_data self;	/*  pointer to the array  */
	byte	offset;		/*  bitvector offset  */
        byte    rank;		/*  rank of array = # of dimensions  */
};

struct ecl_vector {		/*  vector header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
	HEADER2(elttype,flags);	/*  array element type, has fill ptr, adjustable-p */
	cl_object displaced;	/*  displaced  */
	cl_index dim;		/*  dimension  */
	cl_index fillp;		/*  fill pointer  */
				/*  For simple vectors,  */
				/*  v_fillp is equal to v_dim.  */
	union ecl_array_data self;	/*  pointer to the vector  */
	byte	offset;
};

struct ecl_base_string {	/*  string header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
	HEADER2(elttype,flags);	/*  array element type, has fill ptr, adjustable-p */
	cl_object displaced;	/*  displaced  */
	cl_index dim;       	/*  dimension  */
				/*  string length  */
	cl_index fillp;		/*  fill pointer  */
				/*  For simple strings,  */
				/*  st_fillp is equal to st_dim-1.  */
	ecl_base_char *self;	/*  pointer to the string  */
};

#ifdef ECL_UNICODE
struct ecl_string {		/*  string header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
	HEADER2(elttype,flags);	/*  array element type, has fill ptr, adjustable-p */
	cl_object displaced;	/*  displaced  */
	cl_index dim;       	/*  dimension  */
				/*  string length  */
	cl_index fillp;		/*  fill pointer  */
				/*  For simple strings,  */
				/*  st_fillp is equal to st_dim-1.  */
	ecl_character *self;	/*  pointer to the string  */
};
#endif

#ifdef CLOS
#define T_STRUCTURE	t_instance
#define STYPE(x)	CLASS_OF(x)
#define SLOTS(x)	(x)->instance.slots
#define SLENGTH(x)	(x)->instance.length
#define SLOT(x,i)	(x)->instance.slots[i]
#define SNAME(x)	CLASS_NAME(CLASS_OF(x))
#else
struct ecl_structure {		/*  structure header  */
	HEADER;
	cl_object name;		/*  structure name  */
	cl_object *self;	/*  structure self  */
	cl_fixnum length;	/*  structure length  */
};

#define T_STRUCTURE	t_structure
#define STYPE(x)	x->str.name
#define SLOTS(x)	(x)->str.self
#define SLENGTH(x)	(x)->str.length
#define SLOT(x,i)	(x)->str.self[i]
#define SNAME(x)	x->str.name
#endif

enum ecl_smmode {		/*  stream mode  */
	smm_input,		/*  input  */
	smm_input_file,		/*  input  */
	smm_output,		/*  output  */
	smm_output_file,	/*  output  */
	smm_io,			/*  input-output  */
	smm_io_file,		/*  input-output  */
	smm_synonym,		/*  synonym  */
	smm_broadcast,		/*  broadcast  */
	smm_concatenated,	/*  concatenated  */
	smm_two_way,		/*  two way  */
	smm_echo,		/*  echo  */
	smm_string_input,	/*  string input  */
	smm_string_output,	/*  string output  */
	smm_probe,		/*  probe (only used in open_stream())  */
#if defined(ECL_WSOCK)
	smm_input_wsock,	/* input socket (Win32) */
	smm_output_wsock,	/* output socket (Win32) */
	smm_io_wsock,		/* input/output socket (Win32) */
#endif
	smm_sequence_input,	/*  sequence input  */
	smm_sequence_output	/*  sequence output  */
};

struct ecl_file_ops {
	cl_index (*write_byte8)(cl_object strm, unsigned char *c, cl_index n);
	cl_index (*read_byte8)(cl_object strm, unsigned char *c, cl_index n);

	void (*write_byte)(cl_object c, cl_object strm);
	cl_object (*read_byte)(cl_object strm);

	int (*read_char)(cl_object strm);
	int (*write_char)(cl_object strm, int c);
	void (*unread_char)(cl_object strm, int c);
	int (*peek_char)(cl_object strm);

	cl_index (*read_vector)(cl_object strm, cl_object data, cl_index start, cl_index end);
	cl_index (*write_vector)(cl_object strm, cl_object data, cl_index start, cl_index end);

	int (*listen)(cl_object strm);
	void (*clear_input)(cl_object strm);
	void (*clear_output)(cl_object strm);
	void (*finish_output)(cl_object strm);
	void (*force_output)(cl_object strm);

	int (*input_p)(cl_object strm);
	int (*output_p)(cl_object strm);
	int (*interactive_p)(cl_object strm);
	cl_object (*element_type)(cl_object strm);

	cl_object (*length)(cl_object strm);
	cl_object (*get_position)(cl_object strm);
	cl_object (*set_position)(cl_object strm, cl_object pos);
	int (*column)(cl_object strm);

	cl_object (*close)(cl_object strm);
};

enum {
	ECL_STREAM_BINARY = 0,
	ECL_STREAM_FORMAT = 0xF,
#ifndef ECL_UNICODE
	ECL_STREAM_DEFAULT_FORMAT = 1,
#else
	ECL_STREAM_DEFAULT_FORMAT = 2,
	ECL_STREAM_ISO_8859_1 = 1,
	ECL_STREAM_LATIN_1 = 1,
	ECL_STREAM_UTF_8 = 2,
	ECL_STREAM_UCS_2 = 3,
	ECL_STREAM_UCS_2LE = 5 + 128,
	ECL_STREAM_UCS_2BE = 5,
	ECL_STREAM_UCS_4 = 6,
	ECL_STREAM_UCS_4LE = 7 + 128,
	ECL_STREAM_UCS_4BE = 7,
	ECL_STREAM_USER_FORMAT = 8,
	ECL_STREAM_US_ASCII = 10,
#endif
	ECL_STREAM_CR = 16,
	ECL_STREAM_LF = 32,
	ECL_STREAM_SIGNED_BYTES = 64,
	ECL_STREAM_LITTLE_ENDIAN = 128,
	ECL_STREAM_C_STREAM = 256,
	ECL_STREAM_MIGHT_SEEK = 512,
	ECL_STREAM_CLOSE_COMPONENTS = 1024
};

typedef int (*cl_eformat_decoder)(cl_object stream);
typedef int (*cl_eformat_encoder)(cl_object stream, unsigned char *buffer, int c);
typedef cl_index (*cl_eformat_read_byte8)(cl_object object, unsigned char *buffer, cl_index n);

#define ECL_ANSI_STREAM_P(o) \
        (IMMEDIATE(o) == 0 && ((o)->d.t == t_stream))
#define ECL_ANSI_STREAM_TYPE_P(o,m) \
        (IMMEDIATE(o) == 0 && ((o)->d.t == t_stream) && ((o)->stream.mode == (m)))

struct ecl_stream {
	HEADER2(mode,closed);	/*  stream mode of enum smmode  */
				/*  closed stream?  */
	struct ecl_file_ops *ops; /*  dispatch table  */
        union {
                FILE *stream;		/* ANSI C streams */
                cl_fixnum descriptor;	/* POSIX files */
        } file;
	cl_object object0;	/*  some object  */
	cl_object object1;	/*  some object */
	cl_object byte_stack;	/*  buffer for unread bytes  */
	cl_fixnum last_char;	/*  last character read  */
	cl_fixnum last_code[2];	/*  actual composition of last character  */
	cl_fixnum int0;		/*  some int  */
	cl_fixnum int1;		/*  some int  */
	cl_index byte_size;	/*  size of byte in binary streams  */
	cl_fixnum last_op;	/*  0: unknown, 1: reading, -1: writing */
	char *buffer;		/*  buffer for FILE  */
	cl_object format;	/*  external format  */
	cl_eformat_encoder encoder;
	cl_eformat_decoder decoder;
	cl_object format_table;
	int flags;		/*  character table, flags, etc  */
};

struct ecl_random {
	HEADER;
	cl_object value;	/*  random state value  */
};

enum ecl_chattrib {		/*  character attribute  */
	cat_whitespace,		/*  whitespace  */
	cat_terminating,	/*  terminating macro  */
	cat_non_terminating,	/*  non-terminating macro  */
	cat_single_escape,	/*  single-escape  */
	cat_multiple_escape,	/*  multiple-escape  */
	cat_constituent		/*  constituent  */
};

struct ecl_readtable_entry {		/*  read table entry  */
	enum ecl_chattrib syntax_type;	/*  character attribute  */
	cl_object dispatch;		/*  a macro, a hash or NIL  */
};

enum ecl_readtable_case {
	ecl_case_upcase,
	ecl_case_downcase,
	ecl_case_invert,
	ecl_case_preserve
};

struct ecl_readtable {		/*  read table  */
	HEADER1(locked);
	enum ecl_readtable_case read_case; /*  readtable-case  */
	struct ecl_readtable_entry *table; /*  read table itself  */
#ifdef ECL_UNICODE
	cl_object hash;		/* hash for values outside base-char range */
#endif
};

struct ecl_pathname {
	HEADER1(logical);	/*  logical pathname?  */
	cl_object host;		/*  host  */
	cl_object device;	/*  device  */
	cl_object directory;	/*  directory  */
	cl_object name;		/*  name  */
	cl_object type;		/*  type  */
	cl_object version;	/*  version  */
};

struct ecl_codeblock {
	HEADER2(self_destruct,locked);	/*  delete DLL after gc */
					/*  do not garbage collect this library */
	void	*handle;		/*  handle returned by dlopen  */
	void	*entry;			/*  entry point  */
 	cl_object *data;		/*  data vector  */
	int	data_size;
	cl_object *temp_data;		/*  data vector for toplevel forms */
	int	temp_data_size;
	const char *data_text;		/*  string with objects to be defined  */
	int	data_text_size;
	cl_object next;			/*  next codeblock within same library */
	cl_object name;
	cl_object links;		/*  list of symbols with linking calls  */
	cl_index cfuns_size;		/*  number of functions defined  */
	const struct ecl_cfun *cfuns;
        cl_object source;		/*  common debug information for this block  */
        cl_object refs;			/*  reference counter for the library  */
};

struct ecl_bytecodes {
	HEADER;
	cl_object name;		/*  function name  */
	cl_object definition;	/*  function definition in list form  */
	cl_objectfn entry;	/*  entry address (must match the position of
                                 *  the equivalent field in cfun) */
	cl_index code_size;	/*  number of bytecodes  */
	cl_index data_size;	/*  number of constants  */
	char *code;		/*  the intermediate language  */
	cl_object *data;	/*  non-inmediate constants used in the code  */
	cl_object file;		/*  file where it was defined...  */
	cl_object file_position;/*  and where it was created  */
};

struct ecl_bclosure {
	HEADER;
	cl_object code;
	cl_object lex;
	cl_objectfn entry;	/*  entry address  */
};

struct ecl_cfun {		/*  compiled function header  */
	HEADER1(narg);
	cl_object name;		/*  compiled function name  */
	cl_object block;	/*  descriptor of C code block for GC  */
	cl_objectfn entry;	/*  entry address  */
	cl_object file;		/*  file where it was defined...  */
	cl_object file_position;/*  and where it was created  */
};

struct ecl_cfunfixed {		/*  compiled function header  */
	HEADER1(narg);
	cl_object name;		/*  compiled function name  */
	cl_object block;	/*  descriptor of C code block for GC  */
	cl_objectfn entry;	/*  entry address (must match the position of
                                 *  the equivalent field in cfun) */
	cl_objectfn_fixed entry_fixed;	/*  entry address  */
	cl_object file;		/*  file where it was defined...  */
	cl_object file_position;/*  and where it was created  */
};

struct ecl_cclosure {		/*  compiled closure header  */
	HEADER;
	cl_object env;		/*  environment  */
	cl_object block;	/*  descriptor of C code block for GC  */
	cl_objectfn entry;	/*  entry address (must match the position of
                                 *  the equivalent field in cfun) */
	cl_object file;		/*  file where it was defined...  */
	cl_object file_position;/*  and where it was created  */
};

#define ECL_FFICALL_LIMIT 256

enum ecl_ffi_tag {
	ECL_FFI_CHAR = 0,
	ECL_FFI_UNSIGNED_CHAR,
	ECL_FFI_BYTE,
	ECL_FFI_UNSIGNED_BYTE,
	ECL_FFI_SHORT,
	ECL_FFI_UNSIGNED_SHORT,
	ECL_FFI_INT,
	ECL_FFI_UNSIGNED_INT,
	ECL_FFI_LONG,
	ECL_FFI_UNSIGNED_LONG,
#ifdef ecl_uint8_t
        ECL_FFI_INT8_T,
        ECL_FFI_UINT8_T,
#endif
#ifdef ecl_uint16_t
        ECL_FFI_INT16_T,
        ECL_FFI_UINT16_T,
#endif
#ifdef ecl_uint32_t
        ECL_FFI_INT32_T,
        ECL_FFI_UINT32_T,
#endif
#ifdef ecl_uint64_t
        ECL_FFI_INT64_T,
        ECL_FFI_UINT64_T,
#endif
#ifdef ecl_long_long_t
        ECL_FFI_LONG_LONG,
        ECL_FFI_UNSIGNED_LONG_LONG,
#endif
	ECL_FFI_POINTER_VOID,
	ECL_FFI_CSTRING,
	ECL_FFI_OBJECT,
	ECL_FFI_FLOAT,
	ECL_FFI_DOUBLE,
	ECL_FFI_VOID
};

union ecl_ffi_values {
	char c;
	unsigned char uc;
	int8_t b;
	uint8_t ub;
	int i;
	unsigned int ui;
	short s;
	unsigned short us;
	long l;
	unsigned long ul;
#ifdef ecl_uint8_t
        ecl_int8_t i8;
        ecl_uint8_t u8;
#endif
#ifdef ecl_uint16_t
        ecl_int16_t i16;
        ecl_uint16_t u16;
#endif
#ifdef ecl_uint32_t
        ecl_int32_t i32;
        ecl_uint32_t u32;
#endif
#ifdef ecl_uint64_t
        ecl_int64_t i64;
        ecl_uint64_t u64;
#endif
#ifdef ecl_long_long_t
        ecl_long_long_t ll;
        ecl_ulong_long_t ull;
        unsigned long l2[2];
#endif
	void *pv;
	char *pc;
	cl_object o;
	float f;
	double d;
};

enum ecl_ffi_calling_convention {
	ECL_FFI_CC_CDECL = 0,
	ECL_FFI_CC_STDCALL
};

struct ecl_foreign {		/*  user defined datatype  */
	HEADER;
	cl_object tag;		/*  a tag identifying the type  */
	cl_index size;		/*  the amount of memory allocated  */
	char *data;		/*  the data itself  */
};

struct ecl_stack_frame {
	HEADER;
	cl_object *stack;	/*  Is this relative to the lisp stack?  */
	cl_object *base;	/*  Start of frame  */
        cl_index size;		/*  Number of arguments  */
	struct cl_env_struct *env;
};

struct ecl_weak_pointer {	/*  weak pointer to value  */
	HEADER;
	cl_object value;
};

/*
	dummy type
*/
struct ecl_dummy {
	HEADER;
};

#ifdef ECL_THREADS
enum {
        ECL_PROCESS_INACTIVE = 0,
        ECL_PROCESS_BOOTING,
        ECL_PROCESS_ACTIVE,
        ECL_PROCESS_EXITING,
        ECL_PROCESS_DEAD
};
struct ecl_process {
	HEADER2(active, phase);
	cl_object name;
	cl_object function;
	cl_object args;
	pthread_t thread;
	struct cl_env_struct *env;
	cl_object interrupt;
	cl_object initial_bindings;
        cl_object parent;
	cl_object exit_lock;
	int trap_fpe_bits;
        cl_object exit_values;
};

struct ecl_lock {
	HEADER1(recursive);
        cl_object name;
        cl_object holder;       /* thread holding the lock or NIL */
	cl_index counter;
        pthread_mutex_t mutex;
};

struct ecl_rwlock {
	HEADER;
        cl_object name;
#ifdef ECL_RWLOCK
        pthread_rwlock_t mutex;
#else
        cl_object mutex;
#endif
};

struct ecl_condition_variable {
        HEADER;
        pthread_cond_t cv;
};

struct ecl_semaphore {
        HEADER;
        void *handle;
};
#endif /* ECL_THREADS */

#ifdef CLOS
#define CLASS_OF(x)		(x)->instance.clas
#define CLASS_NAME(x)		(x)->instance.slots[0]
#define CLASS_SUPERIORS(x)	(x)->instance.slots[1]
#define CLASS_INFERIORS(x)	(x)->instance.slots[2]
#define CLASS_SLOTS(x)		(x)->instance.slots[3]
#define CLASS_CPL(x)		(x)->instance.slots[4]
#define ECL_INSTANCEP(x)	((IMMEDIATE(x)==0) && ((x)->d.t==t_instance))
#define ECL_NOT_FUNCALLABLE	0
#define ECL_STANDARD_DISPATCH	1
#define ECL_READER_DISPATCH	2
#define ECL_WRITER_DISPATCH	3
#define ECL_USER_DISPATCH	4

struct ecl_instance {		/*  instance header  */
	HEADER1(isgf);
	cl_index length;	/*  instance length  */
	cl_object clas;		/*  instance class  */
	cl_objectfn entry;	/*  entry address  */
	cl_object sig;		/*  generation signature  */
	cl_object *slots;	/*  instance slots  */
};
#endif /* CLOS */

#ifdef ECL_SSE2
union ecl_sse_data {
	/* This member must be first in order for
	   ecl_def_ct_sse_pack to work properly. */
	uint8_t       b8[16];
	int8_t        i8[16];

	__m128        vf;
	__m128i       vi;
	__m128d       vd;

#ifdef ecl_uint16_t
	ecl_uint16_t  b16[8];
	ecl_int16_t   i16[8];
#endif
#ifdef ecl_uint32_t
	ecl_uint32_t  b32[4];
	ecl_int32_t   i32[4];
#endif
#ifdef ecl_uint64_t
	ecl_uint64_t  b64[2];
	ecl_int64_t   i64[2];
#endif

	float         sf[4];
	double        df[2];
};

struct ecl_sse_pack {
	HEADER1(elttype);
	union ecl_sse_data data;
};
#endif

/*
	Definition of lispunion.
*/
union cl_lispunion {
#ifndef ECL_SMALL_CONS
	struct ecl_cons		cons;		/*  unoptimized cons  */
#endif
	struct ecl_bignum	big;		/*  bignum  */
	struct ecl_ratio	ratio;		/*  ratio  */
	struct ecl_singlefloat	SF; 		/*  single floating-point number  */
	struct ecl_doublefloat	DF; 		/*  double floating-point number  */
#ifdef ECL_LONG_FLOAT
	struct ecl_long_float	longfloat;	/*  long-float */
#endif
	struct ecl_complex	complex;	/*  complex number  */
	struct ecl_symbol	symbol;		/*  symbol  */
	struct ecl_package	pack;		/*  package  */
	struct ecl_hashtable	hash;		/*  hash table  */
	struct ecl_array	array;		/*  array  */
	struct ecl_vector	vector;		/*  vector  */
	struct ecl_base_string	base_string;	/*  base-string  */
#ifdef ECL_UNICODE
	struct ecl_string	string;		/*  string  */
#endif
	struct ecl_stream	stream;		/*  stream  */
	struct ecl_random	random;		/*  random-states  */
	struct ecl_readtable	readtable; 	/*  read table  */
	struct ecl_pathname	pathname; 	/*  path name  */
	struct ecl_bytecodes	bytecodes; 	/*  bytecompiled function / code */
	struct ecl_bclosure	bclosure; 	/*  bytecompiled closure */
	struct ecl_cfun		cfun;		/*  compiled function  */
	struct ecl_cfunfixed	cfunfixed;	/*  compiled function  */
	struct ecl_cclosure	cclosure; 	/*  compiled closure  */

	struct ecl_dummy	d;		/*  dummy  */
#ifdef CLOS
	struct ecl_instance	instance; 	/*  clos instance */
#else
	struct ecl_structure	str;		/*  structure  */
#endif /* CLOS */
#ifdef ECL_THREADS
	struct ecl_process	process; 	/*  process  */
	struct ecl_lock		lock; 		/*  lock  */
	struct ecl_rwlock	rwlock; 	/*  read/write lock  */
        struct ecl_condition_variable condition_variable; /*  condition-variable */
#endif
#ifdef ECL_SEMAPHORES
        struct ecl_semaphore	semaphore;	/*  semaphore  */
#endif
	struct ecl_codeblock	cblock;		/*  codeblock  */
	struct ecl_foreign	foreign; 	/*  user defined data type */
	struct ecl_stack_frame	frame;		/*  stack frame  */
	struct ecl_weak_pointer weak;		/*  weak pointers  */
#ifdef ECL_SSE2
	struct ecl_sse_pack     sse;
#endif
};

/*
	Type_of.
*/
#if defined(__cplusplus) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))
static inline cl_type type_of(cl_object o) {
	int i = IMMEDIATE(o);
	return (i? (cl_type)i : (cl_type)(o->d.t));
}
#else
#define	type_of(o) \
	((cl_type)(IMMEDIATE(o) ? IMMEDIATE(o) : ((o)->d.t)))
#endif

/*
	This is used to retrieve optional arguments
*/
typedef struct {
	va_list args;
	cl_object *sp;
	int narg;
} cl_va_list[1];

#ifdef __cplusplus
}
#endif
