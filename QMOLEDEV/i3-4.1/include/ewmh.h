/*
 * vim:ts=4:sw=4:expandtab
 *
 * i3 - an improved dynamic tiling window manager
 * © 2009-2011 Michael Stapelberg and contributors (see also: LICENSE)
 *
 * ewmh.c: Get/set certain EWMH properties easily.
 *
 */
#ifndef _EWMH_C
#define _EWMH_C

/**
 * Updates _NET_CURRENT_DESKTOP with the current desktop number.
 *
 * EWMH: The index of the current desktop. This is always an integer between 0
 * and _NET_NUMBER_OF_DESKTOPS - 1.
 *
 */
void ewmh_update_current_desktop();

/**
 * Updates _NET_ACTIVE_WINDOW with the currently focused window.
 *
 * EWMH: The window ID of the currently active window or None if no window has
 * the focus.
 *
 */
void ewmh_update_active_window(xcb_window_t window);

/**
 * Updates the workarea for each desktop.
 *
 * EWMH: Contains a geometry for each desktop. These geometries specify an area
 * that is completely contained within the viewport. Work area SHOULD be used by
 * desktop applications to place desktop icons appropriately.
 *
 */
void ewmh_update_workarea();

/**
 * Updates the _NET_CLIENT_LIST_STACKING hint. Necessary to move tabs in
 * Chromium correctly.
 *
 * EWMH: These arrays contain all X Windows managed by the Window Manager.
 * _NET_CLIENT_LIST has initial mapping order, starting with the oldest window.
 * _NET_CLIENT_LIST_STACKING has bottom-to-top stacking order. These properties
 * SHOULD be set and updated by the Window Manager.
 *
 */
void ewmh_update_client_list_stacking(xcb_window_t *stack, int num_windows);

#endif
