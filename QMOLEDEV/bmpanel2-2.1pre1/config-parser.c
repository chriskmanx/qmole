#include <stdio.h>
#include "config-parser.h"

/* Tiny structure used for tracking current parsing position and probably other
 * parser related data (if any, currently none). 
 */
struct parse_context {
	char *cur;
	size_t line;
};

/* predeclarations */
static int parse_children(struct config_format_entry *te, int indent_level, struct parse_context *ctx);

/* Count indent symbols (tabs and spaces) and advance parsing context to the
 * first non-indent symbol. 
 *
 * RETURNS
 * 	A number of indent symbols (aka indent level).
 */
static int count_and_skip_indent(struct parse_context *ctx)
{
	int indent = 0;
	while (*ctx->cur == ' ' || *ctx->cur == '\t') {
		indent++;
		ctx->cur++;
	}
	return indent;
}

/* Classify the line using its first non-space character. It's intended to do
 * this right after indent skipping. Parser uses this function to know what to
 * do next. If the line is comment or empty, it is being skipped. If the line
 * is entry, it's being parsed as entry. 
 */
static bool line_is_entry(char first_char)
{
	switch (first_char) {
		case '#':
		case '\n':
			return false;
		default:
			return true;
	}
}

/* Counts a number of child entries of the parent entry with indent level
 * equals to 'indent_level'. Also writes an indent level of the first child to
 * a variable at the address 'chld_indent' if it is not zero. This function
 * doesn't advance parse context position.
 *
 * It thinks of the child indent level as an indent level of the first child.
 * Also function skips all bad-formed children (their indent level differs from
 * an indent level of the first child). And of course function stops counting
 * when there is an entry with indent level lower or equals to that the parent
 * has.
 *
 * RETURNS
 * 	A number of children.
 */
static size_t count_children(int indent_level, struct parse_context *ctx, int *chld_indent)
{
	char *pos = ctx->cur; /* remember context position */
	size_t children_count = 0;
	int children_indent = -1;
	while (*ctx->cur) {
		int indent = count_and_skip_indent(ctx);
		if (line_is_entry(*ctx->cur)) {
			if (indent > indent_level) {
				/* if this is the first child, remember its indent level */
				if (children_indent == -1)
					children_indent = indent;
				/* count all children with that indent level (including
				   the first one) */
				if (indent == children_indent)
					children_count++;
			} else
				/* we're done, stop counting */
				break;
		}
		/* skip line */
		while (*ctx->cur != '\n' && *ctx->cur != '\0')
			ctx->cur++;
		switch (*ctx->cur) {
			case '\n':
				ctx->cur++;
				continue;
			case '\0':
				break;
		}
		break;
	}
	ctx->cur = pos; /* restore position */
	if (chld_indent)
		*chld_indent = children_indent;
	return children_count;
}

/* Parse an entry (name with optional value) and recurse to its children
 * parsing. Function writes parsed info to "te", and "te" shouldn't point to
 * zero. Function expects the parse context to be at the first non-indent
 * symbol of the line. And "indent_level" should be the number of indent
 * symbols on that line. This is tricky, but in my opinion it is required for a
 * nice recursion here. Also it is worth to notice that function modifies
 * buffer, because of in-situ parsing.
 * 
 * This function ends after its children, because of recursion. But before
 * calling "parse_children" it ends right after the line it was called on. 
 *
 * RETURNS
 * 	See "parse_children"...
 */
static int parse_format_entry(struct config_format_entry *te, 
			      struct config_format_entry *parent,
			      int indent_level, struct parse_context *ctx)
{
	te->line = ctx->line;
	te->parent = parent;
	/* extract name */
	char *start = ctx->cur;
	while (*ctx->cur != ' ' 
		&& *ctx->cur != '\t' 
		&& *ctx->cur != '\n' 
		&& *ctx->cur != '\0')
	{
		ctx->cur++;
	}

	/* we got the "end" here, but we're not nullifing it yet, since it may
	   cause problems */
	char *end = ctx->cur;
	te->name = start;

	/* skip spaces between name and value */
	if (*ctx->cur == ' ' || *ctx->cur == '\t')
		while (*ctx->cur == ' ' || *ctx->cur == '\t')
			ctx->cur++;

	char *vstart;
	char *vend;
	switch (*ctx->cur) {
		/* these two cases mean we're done here */
		case '\n':
			ctx->cur++; /* next line */
			ctx->line++;
		case '\0':
			break;
		default:
			/* extract value if it exists */
			vstart = ctx->cur;
			while (*ctx->cur != '\n' && *ctx->cur != '\0')
				ctx->cur++;
			vend = ctx->cur;

			te->value = vstart;
			if (*vend) {
				*vend = '\0';
				ctx->cur++; /* next line */
				ctx->line++;
			}
	}
	
	/* delayed nullifing */
	*end = '\0';
	
	/* recurse to our children (function will decide if any) */
	return parse_children(te, indent_level, ctx);
}

/* Parse children entries of the entry "te" which has an indent level equals to
 * the "indent_level". Function takes first children (first entry with indent
 * level more than "indent_level") and then thinks of all next entries with the
 * same indent level as other children. Other entries are skipped. Function
 * stops when entry with indent lower or equals to "indent_level" is found.
 * 
 * RETURNS
 * 	A number of child entries were parsed.
 */
static int parse_children(struct config_format_entry *te, int indent_level, struct parse_context *ctx)
{
	int children_indent = -1;
	size_t children = 0;
	char *pos = ctx->cur;
	te->children_n = count_children(indent_level, ctx, &children_indent);
	/* if there is no children, return immediately */
	if (!te->children_n)
		return 0;

	/* allocate space for child entries */
	te->children = xmallocz(sizeof(struct config_format_entry) * te->children_n);

	/* ok, this is the *main* parse loop actually, since parser starts from
	   virtual root's children. */
	while (*ctx->cur) {
		/* skip indent */
		int indent = count_and_skip_indent(ctx);
		if (indent == children_indent && line_is_entry(*ctx->cur)) {
			/* we're interested in this line (it's a child line) */
			parse_format_entry(&te->children[children], te, indent, ctx);
			/* remember position after line [and its children] */
			pos = ctx->cur; 
			children++; /* we're did one more child */

			/* are we done? */
			if (children == te->children_n)
				break;

			/* check the *end of all world* condition */
			if (*ctx->cur)
				continue;
		} else {
			/* skip line */
			while (*ctx->cur != '\n' && *ctx->cur != '\0')
				ctx->cur++;
			if (*ctx->cur) {
				ctx->line++;
				ctx->cur++;
				continue;
			}
		}
		pos = ctx->cur; /* remember position after skipped line */
		break;
	}
	ctx->cur = pos; /* restore position */

	return children;
}

/* Parse config format tree from a null-terminated string. 
 *
 * RETURNS
 * 	Non-zero on success. 
 * 	Zero on fail.
 */
static int parse_config_format_string(struct config_format_entry *tree, char *str)
{
	struct parse_context ctx = {str, 1};
	CLEAR_STRUCT(tree);
	/* trick the parser with -1 and parse zero-indent entries as children
	   of the root entry */
	return parse_children(tree, -1, &ctx);
}

int load_config_format_tree(struct config_format_tree *tree, const char *path)
{
	long fsize;
	size_t size;
	size_t read;
	char *buf;
	FILE *f;

	f = fopen(path, "rb");
	if (!f)
		return XERROR("Failed to open config file: %s", path);

	if (fseek(f, 0, SEEK_END) == -1) {
		fclose(f);
		return XERROR("Config file fseek failed");
	}
	fsize = ftell(f);
	if (fsize == -1) {
		fclose(f);
		return XERROR("Config file ftell failed");
	}
	size = (size_t)fsize;
	if (fseek(f, 0, SEEK_SET) == -1) {
		fclose(f);
		return XERROR("Config file fseek failed");
	}

	/* read file contents to buffer */
	buf = xmalloc(size+1);
	buf[size] = '\0';
	read = fread(buf, 1, size, f);
	if (read != size) {
		fclose(f);
		xfree(buf);
		return XERROR("Read error in config file: %s", path);
	}

	fclose(f);

	/* use string parsing function to actually parse */
	int children_n = parse_config_format_string(&tree->root, buf);
	if (children_n == 0) {
		xfree(buf);
		return XERROR("Config format file is empty: %s", path);
	}

	/* assign buffer and dir */
	tree->buf = buf;
	tree->dir = xstrdup(path);
	char *slash = strrchr(tree->dir, '/');
	if (slash) {
		*slash = '\0';
	} else {
		tree->dir[0] = '\0';
	}
	return 0;
}

static void free_config_format_entry(struct config_format_entry *e)
{
	size_t i;
	for (i = 0; i < e->children_n; i++)
		free_config_format_entry(&e->children[i]);
	if (e->children)
		xfree(e->children);
}

void free_config_format_tree(struct config_format_tree *tree)
{
	free_config_format_entry(&tree->root);
	xfree(tree->buf);
	xfree(tree->dir);
}

struct config_format_entry *find_config_format_entry(struct config_format_entry *e, 
		const char *name)
{
	size_t i;
	for (i = 0; i < e->children_n; ++i) {
		if (strcmp(e->children[i].name, name) == 0)
			return &e->children[i];
	}
	return 0;
}

const char *find_config_format_entry_value(struct config_format_entry *e, 
		const char *name)
{
	struct config_format_entry *ee = find_config_format_entry(e, name);
	return (ee) ? ee->value : 0;
}

void config_format_entry_path(char *buf, size_t size, struct config_format_entry *e)
{
	if (e->parent)
		config_format_entry_path(buf, size, e->parent);

	if (e->name) {
		size_t buflen = strlen(buf);
		size_t namelen = strlen(e->name);
		if (size > buflen + namelen + 3) {
			if (buflen)
				strcat(buf, "/");
			strcat(buf, e->name);
		}
	}
}
