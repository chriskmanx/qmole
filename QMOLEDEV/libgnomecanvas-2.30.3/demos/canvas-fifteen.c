#include <config.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include "canvas_demo.h"


#define PIECE_SIZE 50


static void
free_stuff (GtkObject *obj, gpointer data)
{
	g_free (data);
}

static void
test_win (GnomeCanvasItem **board)
{
	int i;
#if 0
	GtkWidget *dlg;
#endif

	for (i = 0; i < 15; i++)
		if (!board[i] || (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (board[i]), "piece_num")) != i))
			return;

#if 0
	dlg=gnome_ok_dialog ("You stud, you win!");
	gtk_window_set_modal(GTK_WINDOW(dlg),TRUE);
	gnome_dialog_run (GNOME_DIALOG (dlg));
#endif
}

static char *
get_piece_color (int piece)
{
	static char buf[50];
	int x, y;
	int r, g, b;

	y = piece / 4;
	x = piece % 4;

	r = ((4 - x) * 255) / 4;
	g = ((4 - y) * 255) / 4;
	b = 128;

	sprintf (buf, "#%02x%02x%02x", r, g, b);

	return buf;
}

static gint
piece_event (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
	GnomeCanvas *canvas;
	GnomeCanvasItem **board;
	GnomeCanvasItem *text;
	int pos, newpos;
	int x, y;
	double dx = 0.0, dy = 0.0;
	int move;

	canvas = item->canvas;
	board = g_object_get_data (G_OBJECT (canvas), "board");
	pos = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (item), "piece_pos"));
	text = g_object_get_data (G_OBJECT (item), "text");

	switch (event->type) {
	case GDK_ENTER_NOTIFY:
		gnome_canvas_item_set (text,
				       "fill_color", "white",
				       NULL);
		break;

	case GDK_LEAVE_NOTIFY:
		gnome_canvas_item_set (text,
				       "fill_color", "black",
				       NULL);
		break;

	case GDK_BUTTON_PRESS:
		y = pos / 4;
		x = pos % 4;

		move = TRUE;

		if ((y > 0) && (board[(y - 1) * 4 + x] == NULL)) {
			dx = 0.0;
			dy = -1.0;
			y--;
		} else if ((y < 3) && (board[(y + 1) * 4 + x] == NULL)) {
			dx = 0.0;
			dy = 1.0;
			y++;
		} else if ((x > 0) && (board[y * 4 + x - 1] == NULL)) {
			dx = -1.0;
			dy = 0.0;
			x--;
		} else if ((x < 3) && (board[y * 4 + x + 1] == NULL)) {
			dx = 1.0;
			dy = 0.0;
			x++;
		} else
			move = FALSE;

		if (move) {
			newpos = y * 4 + x;
			board[pos] = NULL;
			board[newpos] = item;
			g_object_set_data (G_OBJECT (item), "piece_pos", GINT_TO_POINTER (newpos));
			gnome_canvas_item_move (item, dx * PIECE_SIZE, dy * PIECE_SIZE);
			test_win (board);
		}

		break;

	default:
		break;
	}

	return FALSE;
}

#define SCRAMBLE_MOVES 256

static void
scramble (GtkObject *object, gpointer data)
{
	GnomeCanvas *canvas;
	GnomeCanvasItem **board;
	int i;
	int pos, oldpos;
	int dir;
	int x, y;

	srand (time (NULL));

	canvas = data;
	board = g_object_get_data (G_OBJECT (object), "board");

	/* First, find the blank spot */

	for (pos = 0; pos < 16; pos++)
		if (board[pos] == NULL)
			break;

	/* "Move the blank spot" around in order to scramble the pieces */

	for (i = 0; i < SCRAMBLE_MOVES; i++) {
retry_scramble:
		dir = rand () % 4;

		x = y = 0;

		if ((dir == 0) && (pos > 3)) /* up */
			y = -1;
		else if ((dir == 1) && (pos < 12)) /* down */
			y = 1;
		else if ((dir == 2) && ((pos % 4) != 0)) /* left */
			x = -1;
		else if ((dir == 3) && ((pos % 4) != 3)) /* right */
			x = 1;
		else
			goto retry_scramble;

		oldpos = pos + y * 4 + x;
		board[pos] = board[oldpos];
		board[oldpos] = NULL;
		g_object_set_data (G_OBJECT (board[pos]), "piece_pos", GINT_TO_POINTER (pos));
		gnome_canvas_item_move (board[pos], -x * PIECE_SIZE, -y * PIECE_SIZE);
		gnome_canvas_update_now (canvas);
		pos = oldpos;
	}
}

GtkWidget *
create_canvas_fifteen (void)
{
	GtkWidget *vbox;
	GtkWidget *alignment;
	GtkWidget *frame;
	GtkWidget *canvas;
	GtkWidget *button;
	GnomeCanvasItem **board;
	GnomeCanvasItem *text;
	int i, x, y;
	char buf[20];

	vbox = gtk_vbox_new (FALSE, 4);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), 4);
	gtk_widget_show (vbox);

	alignment = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
	gtk_box_pack_start (GTK_BOX (vbox), alignment, TRUE, TRUE, 0);
	gtk_widget_show (alignment);

	frame = gtk_frame_new (NULL);
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);
	gtk_container_add (GTK_CONTAINER (alignment), frame);
	gtk_widget_show (frame);

	/* Create the canvas and board */

	canvas = gnome_canvas_new ();
	gtk_widget_set_size_request (canvas, PIECE_SIZE * 4 + 1, PIECE_SIZE * 4 + 1);
	gnome_canvas_set_scroll_region (GNOME_CANVAS (canvas), 0, 0, PIECE_SIZE * 4 + 1, PIECE_SIZE * 4 + 1);
	gtk_container_add (GTK_CONTAINER (frame), canvas);
	gtk_widget_show (canvas);

	board = g_new (GnomeCanvasItem *, 16);
	g_object_set_data (G_OBJECT (canvas), "board", board);
	g_signal_connect (canvas, "destroy",
			  G_CALLBACK (free_stuff),
			  board);

	for (i = 0; i < 15; i++) {
		y = i / 4;
		x = i % 4;

		board[i] = gnome_canvas_item_new (gnome_canvas_root (GNOME_CANVAS (canvas)),
						  gnome_canvas_group_get_type (),
						  "x", (double) (x * PIECE_SIZE),
						  "y", (double) (y * PIECE_SIZE),
						  NULL);

		gnome_canvas_item_new (GNOME_CANVAS_GROUP (board[i]),
				       gnome_canvas_rect_get_type (),
				       "x1", 0.0,
				       "y1", 0.0,
				       "x2", (double) PIECE_SIZE,
				       "y2", (double) PIECE_SIZE,
				       "fill_color", get_piece_color (i),
				       "outline_color", "black",
				       "width_pixels", 0,
				       NULL);

		sprintf (buf, "%d", i + 1);

		text = gnome_canvas_item_new (GNOME_CANVAS_GROUP (board[i]),
					      gnome_canvas_text_get_type (),
					      "text", buf,
					      "x", (double) PIECE_SIZE / 2.0,
					      "y", (double) PIECE_SIZE / 2.0,
					      "font", "Sans bold 24",
					      "anchor", GTK_ANCHOR_CENTER,
					      "fill_color", "black",
					      NULL);

		g_object_set_data (G_OBJECT (board[i]), "piece_num", GINT_TO_POINTER (i));
		g_object_set_data (G_OBJECT (board[i]), "piece_pos", GINT_TO_POINTER (i));
		g_object_set_data (G_OBJECT (board[i]), "text", text);
		g_signal_connect (board[i], "event",
				  G_CALLBACK (piece_event),
				  NULL);
	}

	board[15] = NULL;

	/* Scramble button */

	button = gtk_button_new_with_label ("Scramble");
	gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, FALSE, 0);
	g_object_set_data (G_OBJECT (button), "board", board);
	g_signal_connect (button, "clicked",
			  G_CALLBACK (scramble),
			  canvas);
	gtk_widget_show (button);

	return vbox;
}
