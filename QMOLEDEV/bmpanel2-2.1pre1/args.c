#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "args.h"

/* TODO: short parameters */

struct opts {
	struct argument *args;
	int argc;
	char **argv;
};

static const char *typemap[] = {
	0,
	"NUMBER",
	"FLOAT",
	"STRING",
	0,
	0,
	0,
	0,
	0
};

static const struct argument helparg = {
	.type = ARGT_END,
	.name = "help",
	.help = "show this message"
};

static struct argument *match_arg(struct opts *opts)
{
	const char *argnodash = &opts->argv[0][2];
	struct argument *args = opts->args;
	
	while (args->type != ARGT_END) {
		int argnlen = strlen(args->name);
		if (!strncmp(args->name, argnodash, argnlen))
			return args;
		if (args->type == ARGT_BOOLEAN && !strncmp(args->name, argnodash+3, argnlen))
			return args;
		args++;
	}
	return 0;
}

static void apply_value_to_arg(struct argument *arg, const char *value)
{
	char *end = "E";

	switch (arg->type) {
	case ARGT_INTEGER:
		if (value) {
			*((int*)arg->value) = strtol(value, &end, 10);
			if (*end == '\0')
				break;
		}
		*((int*)arg->value) = arg->def.i;
		break;
	case ARGT_FLOAT:
		if (value) {
			*((float*)arg->value) = (float)strtod(value, &end);
			if (*end == '\0')
				break;
		}
		*((float*)arg->value) = arg->def.f;
		break;
	case ARGT_STRING:
		if (value)
			*((const char**)arg->value) = value;
		break;
	case ARGT_BOOLEAN:
		*((int*)arg->value) = !strncmp(value, "--no-", 5) ? 0 : 1;
		break;
	case ARGT_SET_BIT:
		*((unsigned int*)arg->value) |= arg->def.b;
		break;
	case ARGT_SET_INT:
		*((int*)arg->value) = arg->def.i;
		break;
	case ARGT_SET_PTR:
		*((void**)arg->value) = arg->def.p;
		break;
	case ARGT_SET_FLT:
		*((float*)arg->value) = arg->def.f;
		break;
	case ARGT_CALLBACK:
		arg->callback(value);	
		break;
	default:
		break;
	}
}

static void set_defaults(struct argument *args)
{
	while (args->type != ARGT_END) {
		switch (args->type) {
		case ARGT_INTEGER:
			*((int*)args->value) = args->def.i;
			break;
		case ARGT_FLOAT:
			*((float*)args->value) = args->def.f;
			break;
		case ARGT_STRING:
			*((const char**)args->value) = args->def.s;
			break;
		case ARGT_BOOLEAN:
			*((int*)args->value) = args->def.i;
			break;
		default:
			break;
		}
		args++;
	}
}

static const char *get_value(struct opts *opts, struct argument *arg)
{
	const char *value = 0;
	int arglen;

	switch (arg->type) {
	case ARGT_INTEGER:
	case ARGT_FLOAT:
	case ARGT_STRING:
	case ARGT_CALLBACK:
		arglen = strlen(arg->name);
		if (opts->argv[0][arglen+2] == '=')
			value = &opts->argv[0][arglen+3];
		else if (opts->argc > 1 && 
			 opts->argv[1][0] != '-' &&
			 opts->argv[1][1] != '-')
		{
			opts->argv++;
			opts->argc--;
			value = opts->argv[0];
		} else {
			value = 0;

		}			
		break;
	case ARGT_BOOLEAN: 
		value = opts->argv[0];
		break;
	default:
		break;
	}

	if (value && value[0] == '\0')
		value = 0;
	return value;
}

static void parse_arg(struct opts *opts, struct argument *match)
{
	const char *value = get_value(opts, match);
	apply_value_to_arg(match, value);
}

static int max_arg_length(struct argument *args)
{
	int maxlen = strlen("-h, help"); /* minimum for help hack */
	while (args->type != ARGT_END) {
		int len = strlen(args->name);
		switch (args->type) {
			case ARGT_STRING:
			case ARGT_INTEGER:
			case ARGT_FLOAT:
				len += strlen(typemap[args->type]) + 1;
				break;
			default:
				break;
		}
		maxlen = (maxlen > len) ? maxlen : len;
		args++;
	}
	return maxlen;
}

static void print_arg_string(const struct argument *arg, int maxlen)
{
	int p;
	/* nasty hack, but.. whatever */
	if (arg == &helparg)
		p = printf("  -h, --help");
	else
		p = printf("  --%s", arg->name);
	switch (arg->type) {
		case ARGT_STRING:
		case ARGT_INTEGER:
		case ARGT_FLOAT:
			p += printf("=%s", typemap[arg->type]);
			break;
		default:
			break;
	}
	for (; p <= maxlen+6; ++p)
		printf(" ");
	printf("%s", arg->help);
	switch (arg->type) {
		case ARGT_STRING: 
			if (arg->def.s) 
				printf(" [\"%s\"]", arg->def.s); 
			break;
		case ARGT_INTEGER: printf(" [%d]", arg->def.i); break;
		case ARGT_FLOAT: printf(" [%f]", arg->def.f); break;
		default:
			break;
	}
	printf("\n");
}

static void show_help_and_quit(struct argument *args, const char *help)
{
	int maxlen = max_arg_length(args);
	if (help)
		printf("%s\n", help);
	print_arg_string(&helparg, maxlen);
	while (args->type != ARGT_END) {
		print_arg_string(args, maxlen);
		args++;
	}
	exit(0);
}

void parse_args(struct argument *args, int argc, char **argv, const char *help)
{
	struct opts opts = {args, argc, argv};
	set_defaults(args);

	for (; opts.argc; opts.argc--, opts.argv++) {
		if (!strcmp(opts.argv[0], "--help") ||
		    !strcmp(opts.argv[0], "-h"))
		{
			show_help_and_quit(args, help);
		}
		if (!strncmp(opts.argv[0], "--", 2)) {
			/* this is our argument */
			struct argument *match = match_arg(&opts);
			if (match)
				parse_arg(&opts, match);
		}
	}
}
