typedef	GtkWidget *(*GDoDemoFunc) (GtkWidget *do_widget);

typedef struct _Demo Demo;

struct _Demo 
{
  gchar *title;
  gchar *filename;
  GDoDemoFunc func;
  Demo *children;
};

GtkWidget *do_appwindow (GtkWidget *do_widget);
GtkWidget *do_assistant (GtkWidget *do_widget);
GtkWidget *do_builder (GtkWidget *do_widget);
GtkWidget *do_button_box (GtkWidget *do_widget);
GtkWidget *do_changedisplay (GtkWidget *do_widget);
GtkWidget *do_clipboard (GtkWidget *do_widget);
GtkWidget *do_colorsel (GtkWidget *do_widget);
GtkWidget *do_combobox (GtkWidget *do_widget);
GtkWidget *do_dialog (GtkWidget *do_widget);
GtkWidget *do_drawingarea (GtkWidget *do_widget);
GtkWidget *do_editable_cells (GtkWidget *do_widget);
GtkWidget *do_entry_buffer (GtkWidget *do_widget);
GtkWidget *do_entry_completion (GtkWidget *do_widget);
GtkWidget *do_expander (GtkWidget *do_widget);
GtkWidget *do_hypertext (GtkWidget *do_widget);
GtkWidget *do_iconview (GtkWidget *do_widget);
GtkWidget *do_iconview_edit (GtkWidget *do_widget);
GtkWidget *do_images (GtkWidget *do_widget);
GtkWidget *do_infobar (GtkWidget *do_widget);
GtkWidget *do_links (GtkWidget *do_widget);
GtkWidget *do_list_store (GtkWidget *do_widget);
GtkWidget *do_menus (GtkWidget *do_widget);
GtkWidget *do_offscreen_window (GtkWidget *do_widget);
GtkWidget *do_offscreen_window2 (GtkWidget *do_widget);
GtkWidget *do_panes (GtkWidget *do_widget);
GtkWidget *do_pickers (GtkWidget *do_widget);
GtkWidget *do_pixbufs (GtkWidget *do_widget);
GtkWidget *do_printing (GtkWidget *do_widget);
GtkWidget *do_rotated_text (GtkWidget *do_widget);
GtkWidget *do_search_entry (GtkWidget *do_widget);
GtkWidget *do_sizegroup (GtkWidget *do_widget);
GtkWidget *do_spinner (GtkWidget *do_widget);
GtkWidget *do_stock_browser (GtkWidget *do_widget);
GtkWidget *do_textview (GtkWidget *do_widget);
GtkWidget *do_textscroll (GtkWidget *do_widget);
GtkWidget *do_toolpalette (GtkWidget *do_widget);
GtkWidget *do_tree_store (GtkWidget *do_widget);
GtkWidget *do_ui_manager (GtkWidget *do_widget);

Demo child0[] = {
  { "Editable Cells", "editable_cells.c", do_editable_cells, NULL },
  { "List Store", "list_store.c", do_list_store, NULL },
  { "Tree Store", "tree_store.c", do_tree_store, NULL },
  { NULL } 
};

Demo child1[] = {
  { "Entry Buffer", "entry_buffer.c", do_entry_buffer, NULL },
  { "Entry Completion", "entry_completion.c", do_entry_completion, NULL },
  { "Search Entry", "search_entry.c", do_search_entry, NULL },
  { NULL } 
};

Demo child2[] = {
  { "Hypertext", "hypertext.c", do_hypertext, NULL },
  { "Multiple Views", "textview.c", do_textview, NULL },
  { "Automatic scrolling", "textscroll.c", do_textscroll, NULL },
  { NULL } 
};

Demo child3[] = {
  { "Icon View Basics", "iconview.c", do_iconview, NULL },
  { "Editing and Drag-and-Drop", "iconview_edit.c", do_iconview_edit, NULL },
  { NULL } 
};

Demo child4[] = {
  { "Rotated button", "offscreen_window.c", do_offscreen_window, NULL },
  { "Effects", "offscreen_window2.c", do_offscreen_window2, NULL },
  { NULL } 
};

Demo testgtk_demos[] = {
  { "Application main window", "appwindow.c", do_appwindow, NULL }, 
  { "Assistant", "assistant.c", do_assistant, NULL }, 
  { "Builder", "builder.c", do_builder, NULL }, 
  { "Button Boxes", "button_box.c", do_button_box, NULL }, 
  { "Change Display", "changedisplay.c", do_changedisplay, NULL }, 
  { "Clipboard", "clipboard.c", do_clipboard, NULL }, 
  { "Color Selector", "colorsel.c", do_colorsel, NULL }, 
  { "Combo boxes", "combobox.c", do_combobox, NULL }, 
  { "Dialog and Message Boxes", "dialog.c", do_dialog, NULL }, 
  { "Drawing Area", "drawingarea.c", do_drawingarea, NULL }, 
  { "Entry", NULL, NULL, child1 }, 
  { "Expander", "expander.c", do_expander, NULL }, 
  { "Icon View", NULL, NULL, child3 }, 
  { "Images", "images.c", do_images, NULL }, 
  { "Info bar", "infobar.c", do_infobar, NULL }, 
  { "Links", "links.c", do_links, NULL }, 
  { "Menus", "menus.c", do_menus, NULL }, 
  { "Offscreen windows", NULL, NULL, child4 }, 
  { "Paned Widgets", "panes.c", do_panes, NULL }, 
  { "Pickers", "pickers.c", do_pickers, NULL }, 
  { "Pixbufs", "pixbufs.c", do_pixbufs, NULL }, 
  { "Printing", "printing.c", do_printing, NULL }, 
  { "Rotated Text", "rotated_text.c", do_rotated_text, NULL }, 
  { "Size Groups", "sizegroup.c", do_sizegroup, NULL }, 
  { "Spinner", "spinner.c", do_spinner, NULL }, 
  { "Stock Item and Icon Browser", "stock_browser.c", do_stock_browser, NULL }, 
  { "Text Widget", NULL, NULL, child2 }, 
  { "Tool Palette", "toolpalette.c", do_toolpalette, NULL }, 
  { "Tree View", NULL, NULL, child0 }, 
  { "UI Manager", "ui_manager.c", do_ui_manager, NULL },
  { NULL } 
};
