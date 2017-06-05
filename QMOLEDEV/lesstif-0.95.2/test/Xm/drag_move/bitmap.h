/* Structure used to hold bitmap data */

typedef struct Bitmap_s {
	char           *name;	/* The file name */
	char           *data;	/* bitmap format data */
	int             width;	/* The width */
	int             height;	/* And height */
	Pixmap          bitmap;	/* The data as a bitmap */
	Pixmap          pixmap;	/* The data as a pixmap */
	Boolean         draggable;	/* Is this one draggable */
	Widget          state_icon;	/* Drag state icon */
}               Bitmap_t, *Bitmap_p;
