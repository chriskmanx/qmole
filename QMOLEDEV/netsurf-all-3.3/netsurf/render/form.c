/*
 * Copyright 2004 James Bursa <bursa@users.sourceforge.net>
 * Copyright 2003 Phil Mellor <monkeyson@users.sourceforge.net>
 * Copyright 2004 John Tytgat <joty@netsurf-browser.org>
 * Copyright 2005-9 John-Mark Bell <jmb@netsurf-browser.org>
 * Copyright 2009 Paul Blokus <paul_pl@users.sourceforge.net>
 * Copyright 2010 Michael Drake <tlsa@netsurf-browser.org>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/** \file
 * Form handling functions (implementation).
 */

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <dom/dom.h>

#include "utils/corestrings.h"
#include "utils/log.h"
#include "utils/messages.h"
#include "utils/talloc.h"
#include "utils/url.h"
#include "utils/utf8.h"
#include "utils/utils.h"
#include "content/fetch.h"
#include "content/hlcache.h"
#include "css/css.h"
#include "css/utils.h"
#include "desktop/browser.h"
#include "desktop/mouse.h"
#include "desktop/knockout.h"
#include "desktop/plot_style.h"
#include "desktop/plotters.h"
#include "desktop/scrollbar.h"
#include "desktop/textarea.h"

#include "render/box.h"
#include "render/font.h"
#include "render/form_internal.h"
#include "render/html.h"
#include "render/html_internal.h"
#include "render/layout.h"

#define MAX_SELECT_HEIGHT 210
#define SELECT_LINE_SPACING 0.2
#define SELECT_BORDER_WIDTH 1
#define SELECT_SELECTED_COLOUR 0xDB9370

struct form_select_menu {
	int line_height;
	int width, height;
	struct scrollbar *scrollbar;
	int f_size;
	bool scroll_capture;
	select_menu_redraw_callback callback;
	void *client_data;
	struct content *c;
};

static plot_style_t plot_style_fill_selected = {
	.fill_type = PLOT_OP_TYPE_SOLID,
	.fill_colour = SELECT_SELECTED_COLOUR,
};

static plot_font_style_t plot_fstyle_entry = {
	.family = PLOT_FONT_FAMILY_SANS_SERIF,
	.weight = 400,
	.flags = FONTF_NONE,
	.background = 0xffffff,
	.foreground = 0x000000,
};

static char *form_acceptable_charset(struct form *form);
static char *form_encode_item(const char *item, uint32_t len, const char *charset,
		const char *fallback);
static void form_select_menu_clicked(struct form_control *control,
		int x, int y);
static void form_select_menu_scroll_callback(void *client_data,
		struct scrollbar_msg_data *scrollbar_data);

/* exported interface documented in render/form_internal.h */
struct form *form_new(void *node, const char *action, const char *target, 
		form_method method, const char *charset, 
		const char *doc_charset)
{
	struct form *form;

	form = calloc(1, sizeof *form);
	if (!form)
		return NULL;

	form->action = strdup(action != NULL ? action : "");
	if (form->action == NULL) {
		free(form);
		return NULL;
	}

	form->target = target != NULL ? strdup(target) : NULL;
	if (target != NULL && form->target == NULL) {
		free(form->action);
		free(form);
		return NULL;
	}

	form->method = method;

	form->accept_charsets = charset != NULL ? strdup(charset) : NULL;
	if (charset != NULL && form->accept_charsets == NULL) {
		free(form->target);
		free(form->action);
		free(form);
		return NULL;
	}

	form->document_charset = doc_charset != NULL ? strdup(doc_charset)
						     : NULL;
	if (doc_charset && form->document_charset == NULL) {
		free(form->accept_charsets);
		free(form->target);
		free(form->action);
		free(form);
		return NULL;
	}

	form->node = node;

	return form;
}


/* exported interface documented in render/form_internal.h */
void form_free(struct form *form)
{
	struct form_control *c, *d;

	for (c = form->controls; c != NULL; c = d) {
		d = c->next;

		form_free_control(c);
	}

	free(form->action);
	free(form->target);
	free(form->accept_charsets);
	free(form->document_charset);

	free(form);
}

/* exported interface documented in render/form_internal.h */
struct form_control *form_new_control(void *node, form_control_type type)
{
	struct form_control *control;

	control = calloc(1, sizeof *control);
	if (control == NULL)
		return NULL;

	control->node = node;
	control->type = type;

	return control;
}


/**
 * Add a control to the list of controls in a form.
 *
 * \param form  The form to add the control to
 * \param control  The control to add
 */
void form_add_control(struct form *form, struct form_control *control)
{
	if (form == NULL) {
		return;
	}

	control->form = form;

	if (form->controls != NULL) {
		assert(form->last_control);

		form->last_control->next = control;
		control->prev = form->last_control;
		control->next = NULL;
		form->last_control = control;
	} else {
		form->controls = form->last_control = control;
	}
}


/**
 * Free a struct form_control.
 *
 * \param  control  structure to free
 */
void form_free_control(struct form_control *control)
{
	free(control->name);
	free(control->value);
	free(control->initial_value);

	if (control->type == GADGET_SELECT) {
		struct form_option *option, *next;

		for (option = control->data.select.items; option;
				option = next) {
			next = option->next;
			free(option->text);
			free(option->value);
			free(option);
		}
		if (control->data.select.menu != NULL)
			form_free_select_menu(control);
	}

	if (control->type == GADGET_TEXTAREA ||
			control->type == GADGET_TEXTBOX ||
			control->type == GADGET_PASSWORD) {

		if (control->data.text.initial != NULL)
			dom_string_unref(control->data.text.initial);

		if (control->data.text.ta != NULL)
			textarea_destroy(control->data.text.ta);
	}

	free(control);
}


/**
 * Add an option to a form select control.
 *
 * \param  control   form control of type GADGET_SELECT
 * \param  value     value of option, used directly (not copied)
 * \param  text      text for option, used directly (not copied)
 * \param  selected  this option is selected
 * \param  node      the DOM node this option is associated with
 * \return  true on success, false on memory exhaustion
 */
bool form_add_option(struct form_control *control, char *value, char *text,
		     bool selected, void *node)
{
	struct form_option *option;

	assert(control);
	assert(control->type == GADGET_SELECT);

	option = calloc(1, sizeof *option);
	if (!option)
		return false;

	option->value = value;
	option->text = text;

	/* add to linked list */
	if (control->data.select.items == 0)
		control->data.select.items = option;
	else
		control->data.select.last_item->next = option;
	control->data.select.last_item = option;

	/* set selected */
	if (selected && (control->data.select.num_selected == 0 ||
			control->data.select.multiple)) {
		option->selected = option->initial_selected = true;
		control->data.select.num_selected++;
		control->data.select.current = option;
	}

	control->data.select.num_items++;

	option->node = node;

	return true;
}


/* exported interface documented in render/form_internal.h */
bool form_successful_controls_dom(struct form *_form,
				  struct form_control *_submit_button,
				  struct fetch_multipart_data **successful_controls)
{
	dom_html_form_element *form = _form->node;
	dom_html_element *submit_button = (_submit_button != NULL) ? _submit_button->node : NULL;
	dom_html_collection *form_elements = NULL;
	dom_html_options_collection *options = NULL;
	dom_node *form_element = NULL, *option_element = NULL;
	dom_exception err;
	dom_string *nodename = NULL, *inputname = NULL, *inputvalue = NULL, *inputtype = NULL;
	struct fetch_multipart_data sentinel, *last_success, *success_new;
	bool had_submit = false, element_disabled, checked;
	char *charset, *rawfile_temp = NULL, *basename;
	uint32_t index, element_count;
	struct image_input_coords *coords;

	last_success = &sentinel;
	sentinel.next = NULL;
	
	/** \todo Replace this call with something DOMish */
	charset = form_acceptable_charset(_form);
	if (charset == NULL) {
		LOG(("failed to find charset"));
		return false;
	}

#define ENCODE_ITEM(i) (((i) == NULL) ? (				\
			form_encode_item("", 0, charset, _form->document_charset) \
			):(					\
			form_encode_item(dom_string_data(i), dom_string_byte_length(i), \
					 charset, _form->document_charset) \
			))
	
	err = dom_html_form_element_get_elements(form, &form_elements);
	
	if (err != DOM_NO_ERR) {
		LOG(("Could not get form elements"));
		goto dom_no_memory;
	}
	
	
	err = dom_html_collection_get_length(form_elements, &element_count);
	
	if (err != DOM_NO_ERR) {
		LOG(("Could not get form element count"));
		goto dom_no_memory;
	}
	
	for (index = 0; index < element_count; index++) {
		if (form_element != NULL) {
			dom_node_unref(form_element);
			form_element = NULL;
		}
		if (nodename != NULL) {
			dom_string_unref(nodename);
			nodename = NULL;
		}
		if (inputname != NULL) {
			dom_string_unref(inputname);
			inputname = NULL;
		}
		if (inputvalue != NULL) {
			dom_string_unref(inputvalue);
			inputvalue = NULL;
		}
		if (inputtype != NULL) {
			dom_string_unref(inputtype);
			inputtype = NULL;
		}
		if (options != NULL) {
			dom_html_options_collection_unref(options);
			options = NULL;
		}
		err = dom_html_collection_item(form_elements,
					       index, &form_element);
		if (err != DOM_NO_ERR) {
			LOG(("Could not retrieve form element %d", index));
			goto dom_no_memory;
		}

		/* Form elements are one of:
		 *   HTMLButtonElement
		 *   HTMLInputElement
		 *   HTMLTextAreaElement
		 *   HTMLSelectElement
		 */
		err = dom_node_get_node_name(form_element, &nodename);
		if (err != DOM_NO_ERR) {
			LOG(("Could not get node name"));
			goto dom_no_memory;
		}

		if (dom_string_isequal(nodename, corestring_dom_TEXTAREA)) {
			err = dom_html_text_area_element_get_disabled(
				(dom_html_text_area_element *)form_element,
				&element_disabled);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get text area disabled property"));
				goto dom_no_memory;
			}
			err = dom_html_text_area_element_get_name(
				(dom_html_text_area_element *)form_element,
				&inputname);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get text area name property"));
				goto dom_no_memory;
			}
		} else if (dom_string_isequal(nodename, corestring_dom_SELECT)) {
			err = dom_html_select_element_get_disabled(
				(dom_html_select_element *)form_element,
				&element_disabled);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get select disabled property"));
				goto dom_no_memory;
			}
			err = dom_html_select_element_get_name(
				(dom_html_select_element *)form_element,
				&inputname);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get select name property"));
				goto dom_no_memory;
			}
		} else if (dom_string_isequal(nodename, corestring_dom_INPUT)) {
			err = dom_html_input_element_get_disabled(
				(dom_html_input_element *)form_element,
				&element_disabled);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get input disabled property"));
				goto dom_no_memory;
			}
			err = dom_html_input_element_get_name(
				(dom_html_input_element *)form_element,
				&inputname);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get input name property"));
				goto dom_no_memory;
			}
		} else if (dom_string_isequal(nodename, corestring_dom_BUTTON)) {
			err = dom_html_button_element_get_disabled(
				(dom_html_button_element *)form_element,
				&element_disabled);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get button disabled property"));
				goto dom_no_memory;
			}
			err = dom_html_button_element_get_name(
				(dom_html_button_element *)form_element,
				&inputname);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get button name property"));
				goto dom_no_memory;
			}
		} else {
			/* Unknown element type came through! */
			LOG(("Unknown element type: %*s",
			     dom_string_byte_length(nodename),
			     dom_string_data(nodename)));
			goto dom_no_memory;
		}
		if (element_disabled)
			continue;
		if (inputname == NULL)
			continue;
		
		if (dom_string_isequal(nodename, corestring_dom_TEXTAREA)) {
			err = dom_html_text_area_element_get_value(
				(dom_html_text_area_element *)form_element,
				&inputvalue);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get text area content"));
				goto dom_no_memory;
			}
		} else if (dom_string_isequal(nodename, corestring_dom_SELECT)) {
			uint32_t options_count, option_index;
			err = dom_html_select_element_get_options(
				(dom_html_select_element *)form_element,
				&options);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get select options collection"));
				goto dom_no_memory;
			}
			err = dom_html_options_collection_get_length(
				options, &options_count);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get select options collection length"));
				goto dom_no_memory;
			}
			for(option_index = 0; option_index < options_count;
					++option_index) {
				bool selected;
				if (option_element != NULL) {
					dom_node_unref(option_element);
					option_element = NULL;
				}
				if (inputvalue != NULL) {
					dom_string_unref(inputvalue);
					inputvalue = NULL;
				}
				err = dom_html_options_collection_item(
					options, option_index, &option_element);
				if (err != DOM_NO_ERR) {
					LOG(("Could not get options item %d", option_index));
					goto dom_no_memory;
				}
				err = dom_html_option_element_get_selected(
					(dom_html_option_element *)option_element,
					&selected);
				if (err != DOM_NO_ERR) {
					LOG(("Could not get option selected property"));
					goto dom_no_memory;
				}
				if (!selected)
					continue;
				err = dom_html_option_element_get_value(
					(dom_html_option_element *)option_element,
					&inputvalue);
				if (err != DOM_NO_ERR) {
					LOG(("Could not get option value"));
					goto dom_no_memory;
				}
				
				success_new = calloc(1, sizeof(*success_new));
				if (success_new == NULL) {
					LOG(("Could not allocate data for option"));
					goto dom_no_memory;
				}
		
				last_success->next = success_new;
				last_success = success_new;
		
				success_new->name = ENCODE_ITEM(inputname);
				if (success_new->name == NULL) {
					LOG(("Could not encode name for option"));
					goto dom_no_memory;
				}
				success_new->value = ENCODE_ITEM(inputvalue);
				if (success_new->value == NULL) {
					LOG(("Could not encode value for option"));
					goto dom_no_memory;
				}
			}
			continue;
		} else if (dom_string_isequal(nodename, corestring_dom_BUTTON)) {
			err = dom_html_button_element_get_type(
				(dom_html_button_element *) form_element,
				&inputtype);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get button element type"));
				goto dom_no_memory;
			}
			if (dom_string_caseless_isequal(
				    inputtype, corestring_dom_submit)) {

				if (submit_button == NULL && !had_submit) {
					/* no button used, and first submit
					 * node found, so use it
					 */
					had_submit = true;
				} else if ((dom_node *)submit_button !=
					   (dom_node *)form_element) {
					continue;
				}

				err = dom_html_button_element_get_value(
					(dom_html_button_element *)form_element,
					&inputvalue);
				if (err != DOM_NO_ERR) {
					LOG(("Could not get submit button value"));
					goto dom_no_memory;
				}
				/* Drop through to report successful button */
			} else {
				continue;
			}
		} else if (dom_string_isequal(nodename, corestring_dom_INPUT)) {
			/* Things to consider here */
			/* Buttons -- only if the successful control */
			/* radio and checkbox -- only if selected */
			/* file -- also get the rawfile */
			/* everything else -- just value */
			err = dom_html_input_element_get_type(
				(dom_html_input_element *) form_element,
				&inputtype);
			if (err != DOM_NO_ERR) {
				LOG(("Could not get input element type"));
				goto dom_no_memory;
			}
			if (dom_string_caseless_isequal(
				    inputtype, corestring_dom_submit)) {

				if (submit_button == NULL && !had_submit) {
					/* no button used, and first submit
					 * node found, so use it
					 */
					had_submit = true;
				} else if ((dom_node *)submit_button !=
					   (dom_node *)form_element) {
					continue;
				}

				err = dom_html_input_element_get_value(
					(dom_html_input_element *)form_element,
					&inputvalue);
				if (err != DOM_NO_ERR) {
					LOG(("Could not get submit button value"));
					goto dom_no_memory;
				}
				/* Drop through to report the successful button */
			} else if (dom_string_caseless_isequal(
					   inputtype, corestring_dom_image)) {
				/* We *ONLY* use an image input if it was the
				 * thing which activated us
				 */
				 if ((dom_node *)submit_button !=
				     (dom_node *)form_element)
					 continue;
				 
				 err = dom_node_get_user_data(
					 form_element,
					 corestring_dom___ns_key_image_coords_node_data,
					 &coords);
				 if (err != DOM_NO_ERR) {
					 LOG(("Could not get image XY data"));
					 goto dom_no_memory;
				 }
				 if (coords == NULL) {
					 LOG(("No XY data on the image input"));
					 goto dom_no_memory;
				 }
				 
				 basename = ENCODE_ITEM(inputname);
				 
				 success_new = calloc(1, sizeof(*success_new));
				 if (success_new == NULL) {
					 free(basename);
					 LOG(("Could not allocate data for image.x"));
					 goto dom_no_memory;
				 }
				 
				 last_success->next = success_new;
				 last_success = success_new;
				 
				 success_new->name = malloc(strlen(basename) + 3);
				 if (success_new->name == NULL) {
					 free(basename);
					 LOG(("Could not allocate name for image.x"));
					 goto dom_no_memory;
				 }
				 success_new->value = malloc(20);
				 if (success_new->value == NULL) {
					 free(basename);
					 LOG(("Could not allocate value for image.x"));
					 goto dom_no_memory;
				 }
				 sprintf(success_new->name, "%s.x", basename);
				 sprintf(success_new->value, "%d", coords->x);
				 
				 success_new = calloc(1, sizeof(*success_new));
				 if (success_new == NULL) {
					 free(basename);
					 LOG(("Could not allocate data for image.y"));
					 goto dom_no_memory;
				 }
				 
				 last_success->next = success_new;
				 last_success = success_new;
				 
				 success_new->name = malloc(strlen(basename) + 3);
				 if (success_new->name == NULL) {
					 free(basename);
					 LOG(("Could not allocate name for image.y"));
					 goto dom_no_memory;
				 }
				 success_new->value = malloc(20);
				 if (success_new->value == NULL) {
					 free(basename);
					 LOG(("Could not allocate value for image.y"));
					 goto dom_no_memory;
				 }
				 sprintf(success_new->name, "%s.y", basename);
				 sprintf(success_new->value, "%d", coords->y);
				 free(basename);
				 continue;
			} else if (dom_string_caseless_isequal(
					   inputtype, corestring_dom_radio) ||
				   dom_string_caseless_isequal(
					   inputtype, corestring_dom_checkbox)) {
				err = dom_html_input_element_get_checked(
					(dom_html_input_element *)form_element,
					&checked);
				if (err != DOM_NO_ERR) {
					LOG(("Could not get input element checked"));
					goto dom_no_memory;
				}
				if (!checked)
					continue;
				err = dom_html_input_element_get_value(
					(dom_html_input_element *)form_element,
					&inputvalue);
				if (err != DOM_NO_ERR) {
					LOG(("Could not get input element value"));
					goto dom_no_memory;
				}
				if (inputvalue == NULL) {
					inputvalue = dom_string_ref(
						corestring_dom_on);
				}
				/* Fall through to simple allocation */
			} else if (dom_string_caseless_isequal(
					   inputtype, corestring_dom_file)) {

				err = dom_html_input_element_get_value(
					(dom_html_input_element *)form_element,
					&inputvalue);
				if (err != DOM_NO_ERR) {
					LOG(("Could not get file value"));
					goto dom_no_memory;
				}
				err = dom_node_get_user_data(
					form_element,
					corestring_dom___ns_key_file_name_node_data,
					&rawfile_temp);
				if (err != DOM_NO_ERR) {
					LOG(("Could not get file rawname"));
					goto dom_no_memory;
				}
				rawfile_temp = strdup(rawfile_temp != NULL ?
						      rawfile_temp :
						      "");
				if (rawfile_temp == NULL) {
					LOG(("Could not copy file rawname"));
					goto dom_no_memory;
				}
				/* Fall out to the allocation */
			} else if (dom_string_caseless_isequal(
					   inputtype, corestring_dom_reset) ||
				   dom_string_caseless_isequal(
					   inputtype, corestring_dom_button)) {
				/* Skip these */
				LOG(("Skipping RESET and BUTTON"));
				continue;
			} else {
				/* Everything else is treated as text values */
				err = dom_html_input_element_get_value(
					(dom_html_input_element *)form_element,
					&inputvalue);
				if (err != DOM_NO_ERR) {
					LOG(("Could not get input value"));
					goto dom_no_memory;
				}
				/* Fall out to the allocation */
			}
		}
		
		success_new = calloc(1, sizeof(*success_new));
		if (success_new == NULL) {
			LOG(("Could not allocate data for generic"));
			goto dom_no_memory;
		}
		
		last_success->next = success_new;
		last_success = success_new;
		
		success_new->name = ENCODE_ITEM(inputname);
		if (success_new->name == NULL) {
			LOG(("Could not encode name for generic"));
			goto dom_no_memory;
		}
		success_new->value = ENCODE_ITEM(inputvalue);
		if (success_new->value == NULL) {
			LOG(("Could not encode value for generic"));
			goto dom_no_memory;
		}
		if (rawfile_temp != NULL) {
			success_new->file = true;
			success_new->rawfile = rawfile_temp;
			rawfile_temp = NULL;
		}
	}
	
	free(charset);

	if (form_element != NULL) {
		dom_node_unref(form_element);
	}

	if (form_elements != NULL) {
		dom_html_collection_unref(form_elements);
	}

	if (nodename != NULL) {
		dom_string_unref(nodename);
	}

	if (inputname != NULL) {
		dom_string_unref(inputname);
	}

	if (inputvalue != NULL) {
		dom_string_unref(inputvalue);
	}

	if (options != NULL) {
		dom_html_options_collection_unref(options);
	}

	if (option_element != NULL) {
		dom_node_unref(option_element);
	}

	if (inputtype != NULL) {
		dom_string_unref(inputtype);
	}

	if (rawfile_temp != NULL) {
		free(rawfile_temp);
	}

	*successful_controls = sentinel.next;
	
	return true;
	
dom_no_memory:
	free(charset);
	fetch_multipart_data_destroy(sentinel.next);
	
	if (form_elements != NULL)
		dom_html_collection_unref(form_elements);
	if (form_element != NULL)
		dom_node_unref(form_element);
	if (nodename != NULL)
		dom_string_unref(nodename);
	if (inputname != NULL)
		dom_string_unref(inputname);
	if (inputvalue != NULL)
		dom_string_unref(inputvalue);
	if (options != NULL)
		dom_html_options_collection_unref(options);
	if (option_element != NULL)
		dom_node_unref(option_element);
	if (inputtype != NULL)
		dom_string_unref(inputtype);
	if (rawfile_temp != NULL)
		free(rawfile_temp);
	
	return false;
}
#undef ENCODE_ITEM

/**
 * Encode controls using application/x-www-form-urlencoded.
 *
 * \param  form  form to which successful controls relate
 * \param  control  linked list of fetch_multipart_data
 * \param  query_string  iff true add '?' to the start of returned data
 * \return  URL-encoded form, or 0 on memory exhaustion
 */

static char *form_url_encode(struct form *form,
		struct fetch_multipart_data *control,
		bool query_string)
{
	char *name, *value;
	char *s, *s2;
	unsigned int len, len1, len_init;
	nserror url_err;

	if (query_string)
		s = malloc(2);
	else
		s = malloc(1);

	if (s == NULL)
		return NULL;

	if (query_string) {
		s[0] = '?';
		s[1] = '\0';
		len_init = len = 1;
	} else {
		s[0] = '\0';
		len_init = len = 0;
	}

	for (; control; control = control->next) {
		url_err = url_escape(control->name, 0, true, NULL, &name);
		if (url_err == NSERROR_NOMEM) {
			free(s);
			return NULL;
		}

		assert(url_err == NSERROR_OK);

		url_err = url_escape(control->value, 0, true, NULL, &value);
		if (url_err == NSERROR_NOMEM) {
			free(name);
			free(s);
			return NULL;
		}

		assert(url_err == NSERROR_OK);

		len1 = len + strlen(name) + strlen(value) + 2;
		s2 = realloc(s, len1 + 1);
		if (!s2) {
			free(value);
			free(name);
			free(s);
			return NULL;
		}
		s = s2;
		sprintf(s + len, "%s=%s&", name, value);
		len = len1;
		free(name);
		free(value);
	}

	if (len > len_init) {
		/* Replace trailing '&' */
		s[len - 1] = '\0';
	}
	return s;
}

/**
 * Find an acceptable character set encoding with which to submit the form
 *
 * \param form  The form
 * \return Pointer to charset name (on heap, caller should free) or NULL
 */
char *form_acceptable_charset(struct form *form)
{
	char *temp, *c;

	if (!form)
		return NULL;

	if (!form->accept_charsets) {
		/* no accept-charsets attribute for this form */
		if (form->document_charset)
			/* document charset present, so use it */
			return strdup(form->document_charset);
		else
			/* no document charset, so default to 8859-1 */
			return strdup("ISO-8859-1");
	}

	/* make temporary copy of accept-charsets attribute */
	temp = strdup(form->accept_charsets);
	if (!temp)
		return NULL;

	/* make it upper case */
	for (c = temp; *c; c++)
		*c = toupper(*c);

	/* is UTF-8 specified? */
	c = strstr(temp, "UTF-8");
	if (c) {
		free(temp);
		return strdup("UTF-8");
	}

	/* dispense with temporary copy */
	free(temp);

	/* according to RFC2070, the accept-charsets attribute of the
	 * form element contains a space and/or comma separated list */
	c = form->accept_charsets;

	/* What would be an improvement would be to choose an encoding
	 * acceptable to the server which covers as much of the input
	 * values as possible. Additionally, we need to handle the case
	 * where none of the acceptable encodings cover all the textual
	 * input values.
	 * For now, we just extract the first element of the charset list
	 */
	while (*c && !isspace(*c)) {
		if (*c == ',')
			break;
		c++;
	}

	return strndup(form->accept_charsets, c - form->accept_charsets);
}

/**
 * Convert a string from UTF-8 to the specified charset
 * As a final fallback, this will attempt to convert to ISO-8859-1.
 *
 * \todo Return charset used?
 *
 * \param item String to convert
 * \param len Length of string to convert
 * \param charset Destination charset
 * \param fallback Fallback charset (may be NULL),
 *                 used iff converting to charset fails
 * \return Pointer to converted string (on heap, caller frees), or NULL
 */
char *form_encode_item(const char *item, uint32_t len, const char *charset,
		const char *fallback)
{
	nserror err;
	char *ret = NULL;
	char cset[256];

	if (!item || !charset)
		return NULL;

	snprintf(cset, sizeof cset, "%s//TRANSLIT", charset);

	err = utf8_to_enc(item, cset, 0, &ret);
	if (err == NSERROR_BAD_ENCODING) {
		/* charset not understood, try without transliteration */
		snprintf(cset, sizeof cset, "%s", charset);
		err = utf8_to_enc(item, cset, len, &ret);

		if (err == NSERROR_BAD_ENCODING) {
			/* nope, try fallback charset (if any) */
			if (fallback) {
				snprintf(cset, sizeof cset, 
						"%s//TRANSLIT", fallback);
				err = utf8_to_enc(item, cset, 0, &ret);

				if (err == NSERROR_BAD_ENCODING) {
					/* and without transliteration */
					snprintf(cset, sizeof cset,
							"%s", fallback);
					err = utf8_to_enc(item, cset, 0, &ret);
				}
			}

			if (err == NSERROR_BAD_ENCODING) {
				/* that also failed, use 8859-1 */
				err = utf8_to_enc(item, "ISO-8859-1//TRANSLIT",
						0, &ret);
				if (err == NSERROR_BAD_ENCODING) {
					/* and without transliteration */
					err = utf8_to_enc(item, "ISO-8859-1",
							0, &ret);
				}
			}
		}
	}
	if (err == NSERROR_NOMEM) {
		return NULL;
	}

	return ret;
}

/* exported interface documented in render/form_internal.h */
bool form_open_select_menu(void *client_data,
		struct form_control *control,
		select_menu_redraw_callback callback,
		struct content *c)
{
	int line_height_with_spacing;
	struct box *box;
	plot_font_style_t fstyle;
	int total_height;
	struct form_select_menu *menu;


	/* if the menu is opened for the first time */
	if (control->data.select.menu == NULL) {

		menu = calloc(1, sizeof (struct form_select_menu));
		if (menu == NULL) {
			warn_user("NoMemory", 0);
			return false;
		}

		control->data.select.menu = menu;

		box = control->box;

		menu->width = box->width +
				box->border[RIGHT].width +
				box->border[LEFT].width +
				box->padding[RIGHT] + box->padding[LEFT];

		font_plot_style_from_css(control->box->style,
				&fstyle);
		menu->f_size = fstyle.size;

		menu->line_height = FIXTOINT(FDIV((FMUL(FLTTOFIX(1.2),
				FMUL(nscss_screen_dpi,
				INTTOFIX(fstyle.size / FONT_SIZE_SCALE)))),
				F_72));

		line_height_with_spacing = menu->line_height +
				menu->line_height *
				SELECT_LINE_SPACING;

		total_height = control->data.select.num_items *
				line_height_with_spacing;
		menu->height = total_height;

		if (menu->height > MAX_SELECT_HEIGHT) {

			menu->height = MAX_SELECT_HEIGHT;
		}
		menu->client_data = client_data;
		menu->callback = callback;
		if (!scrollbar_create(false,
				menu->height,
    				total_height,
				menu->height,
				control,
				form_select_menu_scroll_callback,
				&(menu->scrollbar))) {
			free(menu);
			return false;
		}
		menu->c = c;
	}
	else menu = control->data.select.menu;

	menu->callback(client_data, 0, 0, menu->width, menu->height);

	return true;
}


/* exported interface documented in render/form_internal.h */
void form_free_select_menu(struct form_control *control)
{
	if (control->data.select.menu->scrollbar != NULL)
		scrollbar_destroy(control->data.select.menu->scrollbar);
	free(control->data.select.menu);
	control->data.select.menu = NULL;
}


/* exported interface documented in render/form_internal.h */
bool form_redraw_select_menu(struct form_control *control, int x, int y,
		float scale, const struct rect *clip,
		const struct redraw_context *ctx)
{
	const struct plotter_table *plot = ctx->plot;
	struct box *box;
	struct form_select_menu *menu = control->data.select.menu;
	struct form_option *option;
	int line_height, line_height_with_spacing;
	int width, height;
	int x0, y0, x1, scrollbar_x, y1, y2, y3;
	int item_y;
	int text_pos_offset, text_x;
	int scrollbar_width = SCROLLBAR_WIDTH;
	int i;
	int scroll;
	int x_cp, y_cp;
	struct rect r;
	
	box = control->box;
	
	x_cp = x;
	y_cp = y;
	width = menu->width;
	height = menu->height;
	line_height = menu->line_height;
	
	line_height_with_spacing = line_height +
			line_height * SELECT_LINE_SPACING;
	scroll = scrollbar_get_offset(menu->scrollbar);
	
	if (scale != 1.0) {
		x *= scale;
		y *= scale;
		width *= scale;
		height *= scale;
		scrollbar_width *= scale;
		
		i = scroll / line_height_with_spacing;
		scroll -= i * line_height_with_spacing;
		line_height *= scale;
		line_height_with_spacing *= scale;
		scroll *= scale;
		scroll += i * line_height_with_spacing;
	}
	
	
	x0 = x;
	y0 = y;
	x1 = x + width - 1;
	y1 = y + height - 1;
	scrollbar_x = x1 - scrollbar_width;

	r.x0 = x0;
	r.y0 = y0;
	r.x1 = x1 + 1;
	r.y1 = y1 + 1;
	if (!plot->clip(&r))
		return false;
	if (!plot->rectangle(x0, y0, x1, y1 ,plot_style_stroke_darkwbasec))
		return false;
		
	
	x0 = x0 + SELECT_BORDER_WIDTH;
	y0 = y0 + SELECT_BORDER_WIDTH;
	x1 = x1 - SELECT_BORDER_WIDTH;
	y1 = y1 - SELECT_BORDER_WIDTH;
	height = height - 2 * SELECT_BORDER_WIDTH;

	r.x0 = x0;
	r.y0 = y0;
	r.x1 = x1 + 1;
	r.y1 = y1 + 1;
	if (!plot->clip(&r))
		return false;
	if (!plot->rectangle(x0, y0, x1 + 1, y1 + 1,
			plot_style_fill_lightwbasec))
		return false;
	option = control->data.select.items;
	item_y = line_height_with_spacing;
	
	while (item_y < scroll) {
		option = option->next;
		item_y += line_height_with_spacing;
	}
	item_y -= line_height_with_spacing;
	text_pos_offset = y - scroll +
			(int) (line_height * (0.75 + SELECT_LINE_SPACING));
	text_x = x + (box->border[LEFT].width + box->padding[LEFT]) * scale;
	
	plot_fstyle_entry.size = menu->f_size;
	
	while (option && item_y - scroll < height) {
		
 		if (option->selected) {
 			y2 = y + item_y - scroll;
 			y3 = y + item_y + line_height_with_spacing - scroll;
 			if (!plot->rectangle(x0, (y0 > y2 ? y0 : y2),
					scrollbar_x + 1,
     					(y3 < y1 + 1 ? y3 : y1 + 1),
					&plot_style_fill_selected))
				return false;
 		}
		
		y2 = text_pos_offset + item_y;
		if (!plot->text(text_x, y2, option->text,
				strlen(option->text), &plot_fstyle_entry))
			return false;
		
		item_y += line_height_with_spacing;
		option = option->next;
	}
		
	if (!scrollbar_redraw(menu->scrollbar,
			x_cp + menu->width - SCROLLBAR_WIDTH,
      			y_cp,
			clip, scale, ctx))
		return false;
	
	return true;
}

/**
 * Check whether a clipping rectangle is completely contained in the
 * select menu.
 *
 * \param control  the select menu to check the clipping rectangle for
 * \param scale    the current browser window scale
 * \param clip     the clipping rectangle
 * \return true if inside false otherwise
 */
bool form_clip_inside_select_menu(struct form_control *control, float scale,
		const struct rect *clip)
{
	struct form_select_menu *menu = control->data.select.menu;
	int width, height;
	

	width = menu->width;
	height = menu->height;
	
	if (scale != 1.0) {
		width *= scale;
		height *= scale;
	}
	
	if (clip->x0 >= 0 && clip->x1 <= width &&
			clip->y0 >= 0 && clip->y1 <= height)
		return true;

	return false;
}


/**
 * Process a selection from a form select menu.
 *
 * \param  html The html content handle for the form
 * \param  control  form control with menu
 * \param  item	    index of item selected from the menu
 * \return NSERROR_OK or appropriate error code.
 */
static nserror form__select_process_selection(html_content *html,
		struct form_control *control, int item)
{
	struct box *inline_box;
	struct form_option *o;
	int count;
	nserror ret = NSERROR_OK;

	assert(control != NULL);
	assert(html != NULL);

	/** \todo Even though the form code is effectively part of the html
	 *        content handler, poking around inside contents is not good
	 */

	inline_box = control->box->children->children;

	for (count = 0, o = control->data.select.items;
			o != NULL;
			count++, o = o->next) {
		if (!control->data.select.multiple && o->selected) {
			o->selected = false;
			dom_html_option_element_set_selected(o->node, false);
		}

		if (count == item) {
			if (control->data.select.multiple) {
				if (o->selected) {
					o->selected = false;
					dom_html_option_element_set_selected(
							o->node, false);
					control->data.select.num_selected--;
				} else {
					o->selected = true;
					dom_html_option_element_set_selected(
							o->node, true);
					control->data.select.num_selected++;
				}
			} else {
				dom_html_option_element_set_selected(
						o->node, true);
				o->selected = true;
			}
		}

		if (o->selected) {
			control->data.select.current = o;
		}
	}

	talloc_free(inline_box->text);
	inline_box->text = 0;

	if (control->data.select.num_selected == 0) {
		inline_box->text = talloc_strdup(html->bctx,
				messages_get("Form_None"));
	} else if (control->data.select.num_selected == 1) {
		inline_box->text = talloc_strdup(html->bctx,
				control->data.select.current->text);
	} else {
		inline_box->text = talloc_strdup(html->bctx,
				messages_get("Form_Many"));
	}

	if (!inline_box->text) {
		ret = NSERROR_NOMEM;
		inline_box->length = 0;
	} else {
		inline_box->length = strlen(inline_box->text);
	}
	inline_box->width = control->box->width;

	html__redraw_a_box(html, control->box);

	return ret;
}

/* exported interface documented in render/form.h */
nserror form_select_process_selection(struct form_control *control, int item)
{
	assert(control != NULL);

	return form__select_process_selection(control->html, control, item);
}

/* exported interface documented in render/form.h */
struct form_option *
form_select_get_option(struct form_control *control, int item)
{
	struct form_option *opt;

	opt = control->data.select.items;
	while ((opt != NULL) && (item > 0)) {
		opt = opt->next;
		item--;
	}
	return opt;
}

/* exported interface documented in render/form.h */
char *form_control_get_name(struct form_control *control)
{
	return control->name;
}

/* exported interface documented in render/form.h */
nserror form_control_bounding_rect(struct form_control *control, struct rect *r)
{
	box_bounds( control->box, r );
	return NSERROR_OK;
}


/**
 * Handle a click on the area of the currently opened select menu.
 * 
 * \param control	the select menu which received the click
 * \param x		X coordinate of click
 * \param y		Y coordinate of click
 */
void form_select_menu_clicked(struct form_control *control, int x, int y)
{	
	struct form_select_menu *menu = control->data.select.menu;
	struct form_option *option;
	html_content *html = (html_content *)menu->c;
	int line_height, line_height_with_spacing;	
	int item_bottom_y;
	int scroll, i;
	
	scroll = scrollbar_get_offset(menu->scrollbar);
	
	line_height = menu->line_height;
	line_height_with_spacing = line_height +
			line_height * SELECT_LINE_SPACING;
	
	option = control->data.select.items;
	item_bottom_y = line_height_with_spacing;
	i = 0;
	while (option && item_bottom_y < scroll + y) {
		item_bottom_y += line_height_with_spacing;
		option = option->next;
		i++;
	}
	
	if (option != NULL) {
		form__select_process_selection(html, control, i);
	}
	
	menu->callback(menu->client_data, 0, 0, menu->width, menu->height);
}

/**
 * Handle mouse action for the currently opened select menu.
 *
 * \param control	the select menu which received the mouse action
 * \param mouse		current mouse state
 * \param x		X coordinate of click
 * \param y		Y coordinate of click
 * \return		text for the browser status bar or NULL if the menu has
 * 			to be closed
 */
const char *form_select_mouse_action(struct form_control *control,
		browser_mouse_state mouse, int x, int y)
{
	struct form_select_menu *menu = control->data.select.menu;
	int x0, y0, x1, y1, scrollbar_x;
	const char *status = NULL;
	bool multiple = control->data.select.multiple;
	
	x0 = 0;
	y0 = 0;
	x1 = menu->width;
	y1 = menu->height;
	scrollbar_x = x1 - SCROLLBAR_WIDTH;
	
	if (menu->scroll_capture ||
			(x > scrollbar_x && x < x1 && y > y0 && y < y1)) {
		/* The scroll is currently capturing all events or the mouse
		 * event is taking place on the scrollbar widget area
		 */
		x -= scrollbar_x;
		return scrollbar_mouse_status_to_message(
				scrollbar_mouse_action(menu->scrollbar,
						mouse, x, y));
	}
	
	
	if (x > x0 && x < scrollbar_x && y > y0 && y < y1) {
		/* over option area */
		
		if (mouse & (BROWSER_MOUSE_CLICK_1 | BROWSER_MOUSE_CLICK_2))
			/* button 1 or 2 click */
			form_select_menu_clicked(control, x, y);
		
		if (!(mouse & BROWSER_MOUSE_CLICK_1 && !multiple))
			/* anything but a button 1 click over a single select
			   menu */
			status = messages_get(control->data.select.multiple ?
					"SelectMClick" : "SelectClick");
		
	} else if (!(mouse & (BROWSER_MOUSE_CLICK_1 | BROWSER_MOUSE_CLICK_2)))
		/* if not a button 1 or 2 click*/
		status = messages_get("SelectClose");
			
	return status;
}

/**
 * Handle mouse drag end for the currently opened select menu.
 *
 * \param control	the select menu which received the mouse drag end
 * \param mouse		current mouse state
 * \param x		X coordinate of drag end
 * \param y		Y coordinate of drag end
 */
void form_select_mouse_drag_end(struct form_control *control,
		browser_mouse_state mouse, int x, int y)
{
	int x0, y0, x1, y1;
	int box_x, box_y;
	struct box *box;
	struct form_select_menu *menu = control->data.select.menu;

	box = control->box;

	/* Get global coords of scrollbar */
	box_coords(box, &box_x, &box_y);
	box_x -= box->border[LEFT].width;
	box_y += box->height + box->border[BOTTOM].width +
			box->padding[BOTTOM] + box->padding[TOP];

	/* Get drag end coords relative to scrollbar */
	x = x - box_x;
	y = y - box_y;

	if (menu->scroll_capture) {
		x -= menu->width - SCROLLBAR_WIDTH;
		scrollbar_mouse_drag_end(menu->scrollbar, mouse, x, y);
		return;
	}
	
	x0 = 0;
	y0 = 0;
	x1 = menu->width;
	y1 = menu->height;
		
	
	if (x > x0 && x < x1 - SCROLLBAR_WIDTH && y >  y0 && y < y1)
		/* handle drag end above the option area like a regular click */
		form_select_menu_clicked(control, x, y);
}

/**
 * Callback for the select menus scroll
 */
void form_select_menu_scroll_callback(void *client_data,
		struct scrollbar_msg_data *scrollbar_data)
{
	struct form_control *control = client_data;
	struct form_select_menu *menu = control->data.select.menu;
	html_content *html = (html_content *)menu->c;
	
	switch (scrollbar_data->msg) {
		case SCROLLBAR_MSG_MOVED:
			menu->callback(menu->client_data,
				    	0, 0,
					menu->width,
     					menu->height);
			break;
		case SCROLLBAR_MSG_SCROLL_START:
		{
			struct rect rect = {
				.x0 = scrollbar_data->x0,
				.y0 = scrollbar_data->y0,
				.x1 = scrollbar_data->x1,
				.y1 = scrollbar_data->y1
			};

			browser_window_set_drag_type(html->bw,
					DRAGGING_CONTENT_SCROLLBAR, &rect);

			menu->scroll_capture = true;
		}
			break;
		case SCROLLBAR_MSG_SCROLL_FINISHED:
			menu->scroll_capture = false;

			browser_window_set_drag_type(html->bw,
					DRAGGING_NONE, NULL);
			break;
		default:
			break;
	}
}

/**
 * Get the dimensions of a select menu.
 *
 * \param control	the select menu to get the dimensions of
 * \param width		gets updated to menu width
 * \param height	gets updated to menu height
 */
void form_select_get_dimensions(struct form_control *control,
		int *width, int *height)
{
	*width = control->data.select.menu->width;
	*height = control->data.select.menu->height;
}

/**
 * Callback for the core select menu.
 */
void form_select_menu_callback(void *client_data,
		int x, int y, int width, int height)
{
	html_content *html = client_data;
	int menu_x, menu_y;
	struct box *box;
	
	box = html->visible_select_menu->box;
	box_coords(box, &menu_x, &menu_y);
		
	menu_x -= box->border[LEFT].width;
	menu_y += box->height + box->border[BOTTOM].width +
			box->padding[BOTTOM] +
			box->padding[TOP];
	content__request_redraw((struct content *)html, menu_x + x, menu_y + y,
			width, height);
}


/**
 * Set a radio form control and clear the others in the group.
 *
 * \param radio form control of type GADGET_RADIO
 */

void form_radio_set(struct form_control *radio)
{
	struct form_control *control;

	assert(radio);
	if (!radio->form)
		return;

	if (radio->selected)
		return;

	for (control = radio->form->controls; control;
			control = control->next) {
		if (control->type != GADGET_RADIO)
			continue;
		if (control == radio)
			continue;
		if (strcmp(control->name, radio->name) != 0)
			continue;

		if (control->selected) {
			control->selected = false;
			dom_html_input_element_set_checked(control->node, false);
			html__redraw_a_box(radio->html, control->box);
		}
	}

	radio->selected = true;
	dom_html_input_element_set_checked(radio->node, true);
	html__redraw_a_box(radio->html, radio->box);
}


/**
 * Collect controls and submit a form.
 */

void form_submit(nsurl *page_url, struct browser_window *target,
		struct form *form, struct form_control *submit_button)
{
	char *data = NULL;
	struct fetch_multipart_data *success;
	nsurl *action_url;
	nsurl *action_query;
	nserror error;

	assert(form != NULL);

	if (form_successful_controls_dom(form, submit_button, &success) == false) {
		warn_user("NoMemory", 0);
		return;
	}

	/* Decompose action */
	if (nsurl_create(form->action, &action_url) != NSERROR_OK) {
		free(data);
		fetch_multipart_data_destroy(success);
		warn_user("NoMemory", 0);
		return;
	}

	switch (form->method) {
	case method_GET:
		data = form_url_encode(form, success, true);
		if (data == NULL) {
			fetch_multipart_data_destroy(success);
			warn_user("NoMemory", 0);
			return;
		}

		/* Replace query segment */
		error = nsurl_replace_query(action_url, data, &action_query); 
		if (error != NSERROR_OK) {
			nsurl_unref(action_query);
			free(data);
			fetch_multipart_data_destroy(success);
			warn_user(messages_get_errorcode(error), 0);
			return;
		}

		/* Construct submit url */
		browser_window_navigate(target,
					action_query,
					page_url,
					BW_NAVIGATE_HISTORY,
					NULL,
					NULL,
					NULL);

		nsurl_unref(action_query);
		break;

	case method_POST_URLENC:
		data = form_url_encode(form, success, false);
		if (data == NULL) {
			fetch_multipart_data_destroy(success);
			warn_user("NoMemory", 0);
			nsurl_unref(action_url);
			return;
		}

		browser_window_navigate(target,
					action_url,
					page_url,
					BW_NAVIGATE_HISTORY,
					data,
					NULL,
					NULL);
		break;

	case method_POST_MULTIPART:
		browser_window_navigate(target, 
					action_url, 
					page_url,
					BW_NAVIGATE_HISTORY,
					NULL, 
					success, 
					NULL);

		break;
	}

	nsurl_unref(action_url);
	fetch_multipart_data_destroy(success);
	free(data);
}

void form_gadget_update_value(struct form_control *control, char *value)
{
	switch (control->type) {
	case GADGET_HIDDEN:
	case GADGET_TEXTBOX:
	case GADGET_TEXTAREA:
	case GADGET_PASSWORD:
	case GADGET_FILE:
		if (control->value != NULL) {
			free(control->value);
		}
		control->value = value;
		if (control->node != NULL) {
			dom_exception err;
			dom_string *str;
			err = dom_string_create((uint8_t *)value,
						strlen(value), &str);
			if (err == DOM_NO_ERR) {
				if (control->type == GADGET_TEXTAREA)
					err = dom_html_text_area_element_set_value(
						(dom_html_text_area_element *)(control->node),
						str);
				else
					err = dom_html_input_element_set_value(
						(dom_html_input_element *)(control->node),
						str);
				dom_string_unref(str);
			}
		}
		break;
	default:
		/* Do nothing */
		break;
	}
}
