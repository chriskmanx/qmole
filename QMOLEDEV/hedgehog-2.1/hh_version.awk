/^#define HEDGEHOG_LISP_VERSION_MAJOR/ { major=$NF }
/^#define HEDGEHOG_LISP_VERSION_MINOR/ { minor=$NF }
/^#define HEDGEHOG_IMPLEMENTATION_VERSION_PATCH/ { patch=$NF }
END { print major "." minor "." patch }
