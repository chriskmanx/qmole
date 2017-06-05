/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#include <sys/stat.h>
#include <time.h>

typedef struct _FindCondition FindCondition;
typedef struct _FindInfo FindInfo;
typedef gboolean (*FindTest)(FindCondition *condition, FindInfo *info);
typedef void (*FindFree)(FindCondition *condition);

struct _FindInfo
{
	const guchar	*fullpath;
	const guchar	*leaf;
	struct stat	stats;
	time_t		now;
	gboolean	prune;
};

FindCondition *find_compile(const gchar *string);
gboolean find_test_condition(FindCondition *condition, FindInfo *info);
void find_condition_free(FindCondition *condition);
