#include "gui.h"

static inline int point_in_rect(int px, int py, int x, int y, int w, int h)
{
	return (px >= x &&
		px < x + w &&
		py >= y &&
		py < y + h);
}

void disp_button_press_release(struct panel *p, XButtonEvent *e)
{
	if (e->type == ButtonRelease && !p->dnd.taken_on) {
		p->last_click_widget = 0;
		p->last_click_x = 0;
		p->last_click_y = 0;
	}

	size_t i;
	for (i = 0; i < p->widgets_n; ++i) {
		struct widget *w = &p->widgets[i];
		if (point_in_rect(e->x, e->y, w->x, 0, w->width, p->height)) {
			if (!p->dnd.taken_on) {
				if (e->type == ButtonPress) {
					p->last_click_widget = w;
					p->last_click_x = e->x;
					p->last_click_y = e->y;
				}
				if (w->interface->button_click)
					(*w->interface->button_click)(w, e);
			} else {
				if (e->type == ButtonRelease) {
					p->dnd.dropped_on = w;
					p->dnd.dropped_x = e->x;
					p->dnd.dropped_y = e->y;
					if (w->interface->dnd_drop)
						(*w->interface->dnd_drop)(w, &p->dnd);	
				}
			}
			break;
		}
	}
	if (e->type == ButtonRelease && p->dnd.taken_on) {
		struct widget *w = p->dnd.taken_on;
		if (w->interface->dnd_drop && p->dnd.taken_on != p->dnd.dropped_on)
			(*w->interface->dnd_drop)(w, &p->dnd);

		CLEAR_STRUCT(&p->dnd);
	}
}

void disp_motion_notify(struct panel *p, XMotionEvent *e)
{
	size_t i;

	/* is there any widget under mouse at all? for example if mouse is on
	   top of separator, there is no widget under it */
	int widget_under_mouse = 0;

	/* motion events: enter, leave, motion */
	for (i = 0; i < p->widgets_n; ++i) {
		struct widget *w = &p->widgets[i];
		if (point_in_rect(e->x, e->y, w->x, 0, w->width, p->height)) {
			if (w == p->under_mouse) {
				if (w->interface->mouse_motion)
					(*w->interface->mouse_motion)(w, e);
			} else {
				if (p->under_mouse && 
					p->under_mouse->interface->mouse_leave)
				{
					(*p->under_mouse->interface->
						mouse_leave)(p->under_mouse);
				}
				p->under_mouse = w;
				if (w->interface->mouse_enter)
					(*w->interface->mouse_enter)(w);
			}
			widget_under_mouse = 1;
		}
	}
	if (!widget_under_mouse) {
		if (p->under_mouse && p->under_mouse->interface->mouse_leave)
			(*p->under_mouse->interface->mouse_leave)(p->under_mouse);
		p->under_mouse = 0;
	}

	/* drag'n'drop moving */
	if (p->dnd.taken_on) {
		p->dnd.cur_root_x = e->x_root;
		p->dnd.cur_root_y = e->y_root;
		p->dnd.cur_x = e->x;
		p->dnd.cur_y = e->y;
		struct widget *w = p->dnd.taken_on;
		if (w->interface->dnd_drag)
			(*w->interface->dnd_drag)(w, &p->dnd);
	}

	/* drag'n'drop detection */
	if (p->last_click_widget && (abs(p->last_click_x - e->x) > p->drag_threshold
			|| abs(p->last_click_y - e->y) > p->drag_threshold))
	{
		/* drag detected */
		struct widget *w = p->last_click_widget;
		p->dnd.taken_on = w;
		p->dnd.taken_x = p->last_click_x;
		p->dnd.taken_y = p->last_click_y;
		p->dnd.cur_x = e->x;
		p->dnd.cur_y = e->y;
		p->dnd.cur_root_x = e->x_root;
		p->dnd.cur_root_y = e->y_root;
		if (w->interface->dnd_start)
			(*w->interface->dnd_start)(w, &p->dnd);

		p->last_click_widget = 0;
		p->last_click_x = 0;
		p->last_click_y = 0;
	}
}

void disp_enter_leave_notify(struct panel *p, XCrossingEvent *e)
{
	if (e->type == LeaveNotify) {
		if (p->under_mouse && p->under_mouse->interface->mouse_leave)
			(*p->under_mouse->interface->mouse_leave)(p->under_mouse);
		p->under_mouse = 0;
	}
}

void disp_property_notify(struct panel *p, XPropertyEvent *e)
{
	size_t i;
	for (i = 0; i < p->widgets_n; ++i) {
		struct widget *w = &p->widgets[i];
		if (w->interface->prop_change)
			(*w->interface->prop_change)(w, e);
	}
}

void disp_client_msg(struct panel *p, XClientMessageEvent *e)
{
	size_t i;
	for (i = 0; i < p->widgets_n; ++i) {
		struct widget *w = &p->widgets[i];
		if (w->interface->client_msg)
			(*w->interface->client_msg)(w, e);
	}
}

void disp_win_destroy(struct panel *p, XDestroyWindowEvent *e)
{
	size_t i;
	for (i = 0; i < p->widgets_n; ++i) {
		struct widget *w = &p->widgets[i];
		if (w->interface->win_destroy)
			(*w->interface->win_destroy)(w, e);
	}
}

void disp_configure(struct panel *p, XConfigureEvent *e)
{
	size_t i;
	for (i = 0; i < p->widgets_n; ++i) {
		struct widget *w = &p->widgets[i];
		if (w->interface->configure)
			(*w->interface->configure)(w, e);
	}
}
