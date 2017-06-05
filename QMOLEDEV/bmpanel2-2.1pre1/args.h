#pragma once

enum argument_type {
	ARGT_END,

	ARGT_INTEGER, /* --arg=31337 or --arg 31337 */
	ARGT_FLOAT, /* --arg=3.14 or --arg 3.14 */
	ARGT_STRING, /* --arg=John or --arg "John" or --arg John */
	ARGT_BOOLEAN, /* --arg or --no-arg */

	ARGT_SET_BIT, /* --arg */
	ARGT_SET_INT, /* --arg */
	ARGT_SET_PTR, /* --arg */
	ARGT_SET_FLT, /* --arg */

	ARGT_CALLBACK /* --arg=whatever or --arg whatever */
};

typedef void (*argument_callback_t)(const char*);

struct argument {
	enum argument_type type;
	const char *name;
	void *value;
	const char *help;
	union {
		int i;
		const char *s;
		float f;
		void *p;
		unsigned int b;
	} def;
	argument_callback_t callback;
};

#define ARG_STRING(n, v, h, d) \
	{ .type = ARGT_STRING, .name = (n), .value = (v), .help = (h), .def.s = (d), .callback = 0 } 
#define ARG_INTEGER(n, v, h, d) \
	{ .type = ARGT_INTEGER, .name = (n), .value = (v), .help = (h), .def.i = (d), .callback = 0 }
#define ARG_FLOAT(n, v, h, d) \
	{ .type = ARGT_FLOAT, .name = (n), .value = (v), .help = (h), .def.f = (d), .callback = 0 }
#define ARG_BOOLEAN(n, v, h, d) \
	{ .type = ARGT_BOOLEAN, .name = (n), .value = (v), .help = (h), .def.i = (d), .callback = 0 }

#define ARG_SET_BIT(n, v, h, d) \
	{ .type = ARGT_SET_BIT, .name = (n), .value = (v), .help = (h), .def.b = (d), .callback = 0 }
#define ARG_SET_INT(n, v, h, d) \
	{ .type = ARGT_SET_INT, .name = (n), .value = (v), .help = (h), .def.i = (d), .callback = 0 }
#define ARG_SET_PTR(n, v, h, d) \
	{ .type = ARGT_SET_PTR, .name = (n), .value = (v), .help = (h), .def.p = (d), .callback = 0 }
#define ARG_SET_FLT(n, v, h, d) \
	{ .type = ARGT_SET_FLT, .name = (n), .value = (v), .help = (h), .def.f = (d), .callback = 0 }

#define ARG_CALLBACK(n, cb, h) \
	{ .type = ARGT_CALLBACK, .name = (n), .help = (h), .callback = (cb) }

#define ARG_END \
	{ .type = ARGT_END }

void parse_args(struct argument *args, int argc, char **argv, const char *help);
