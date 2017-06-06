#! /bin/sh

cd $GCONF_SRCDIR || exit 1

glib-mkenums \
	--fhead "#ifndef __GCONF_ENUM_TYPES_H__\n#define __GCONF_ENUM_TYPES_H__\n\n#include <glib-object.h>\n\nG_BEGIN_DECLS\n\n" \
	--fprod "/* enumerations from \"@filename@\" */\n\n" \
	--vhead "GType @enum_name@_get_type (void);\n#define GCONF_TYPE_@ENUMSHORT@ (@enum_name@_get_type())\n\n" \
	--ftail "G_END_DECLS\n\n#endif /* __GCONF_ENUM_TYPES_H__ */" \
	$* > tmp-unfixed-gconf-enum-types.h || exit 1

cat tmp-unfixed-gconf-enum-types.h | sed -e 's/g_conf/gconf/g' -e 's/TYPE_CONF/TYPE/g' > tmp-gconf-enum-types.h || exit 1

rm -f tmp-unfixed-gconf-enum-types.h || exit 1

if cmp -s tmp-gconf-enum-types.h gconf-enum-types.h; then
  echo "gconf-enum-types.h is unchanged"
else
  echo "Replacing gconf-enum-types.h"
  cp tmp-gconf-enum-types.h gconf-enum-types.h || exit 1
fi

rm -f tmp-gconf-enum-types.h || exit 1

exit 0
