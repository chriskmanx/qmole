#include "util.h"

/**************************************************************************
  string buffer
**************************************************************************/

void strbuf_assign(struct strbuf *sb, const char *str)
{
	size_t len = strlen(str);
	if (sb->alloc == 0) {
		sb->alloc = len + 1;
		sb->buf = xmalloc(sb->alloc);
		strcpy(sb->buf, str);
	} else {
		if (sb->alloc >= len + 1) {
			strcpy(sb->buf, str);
		} else {
			xfree(sb->buf);
			sb->alloc = len + 1;
			sb->buf = xmalloc(sb->alloc);
			strcpy(sb->buf, str);
		}
	}
}

void strbuf_free(struct strbuf *sb)
{
	if (sb->alloc)
		xfree(sb->buf);
}

