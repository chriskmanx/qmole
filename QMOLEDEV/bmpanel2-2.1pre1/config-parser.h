#pragma once

#include "util.h"

/** 
 * \defgroup config_parser Config format parser 
 *
 * A simple tree-like format parser. 
 *
 * It is used for bmpanel2 config and themes. The config may look like this:
 * @code
 * #---------------------------------------------
 * # panel
 * #---------------------------------------------
 * panel
 * 	position bottom
 * 	separator separator_img.png
 * 	background tile_img.png
 * 
 * #---------------------------------------------
 * # taskbar (this is a comment)
 * #---------------------------------------------
 * taskbar
 * 	idle
 * 		left tb_left_idle_img.png
 * 		center tb_tile_idle_img.png
 * 		right tb_right_idle_img.png
 * 		font DejaVuSans 8
 * 			color 255 255 255
 * 			align left
 * 			offset 5 1
 * 
 * 	pressed
 * 		left tb_left_pressed_img.png
 * 		center tb_tile_pressed_img.png
 * 		right tb_right_pressed_img.png
 * 		font DejaVuSans 8
 * 			color 255 255 255
 * 			align left
 * 			offset 5 1
 * 
 * 	default_icon default_icon.png
 * 		offset 0 1
 * 
 * @endcode
 */
/*@{*/

/**
 * Named config format entry with optional associated value and children. 
 * 
 * * It is capable of building trees of entries.
 */
struct config_format_entry {
	char *name; /**< The name. */
	char *value; /**< The value or 0 if none. */

	struct config_format_entry *parent; /**< Parent entry or 0 if none. */

	size_t children_n; /**< Number of children entries. */
	struct config_format_entry *children; /**< Array of children entries. */

	size_t line; /**< Line in the config file, useful for error messages. */
};

/**
 * Config format tree representation. 
 */
struct config_format_tree {
	/** The directory containing the config file. */
	char *dir; 

	/**
	 * Tree root.
	 *
	 * Name and value of this root always point to zero. Actually, only
	 * \p children_n and \p children values are meaningful.
	 */
	struct config_format_entry root;
	
	/** 
	 * A buffer containing modified config format data (used for in-situ
	 * parsing). 
	 *
	 * Usually it's a pointer to a zero-terminated string, which
	 * represents modified contents of a config file. Normally not used
	 * directly (private data).
	 */
	char *buf;
};


/** 
 * Load a \p tree from a \p file. 
 *
 * After successful loading, the \p tree should be released using
 * free_config_format_tree() function when the data isn't needed anymore.
 *
 * @param[out] tree The tree to load to.
 * @param[in] file The file to load from.
 *
 * @retval 0 Everything is fine.
 * @retval -1 An error occured. Also an error message is printed to the stderr.
 */
int load_config_format_tree(struct config_format_tree *tree, const char *file);

/**
 * Free the loaded tree.
 *
 * @param[in] tree The tree structure to free.
 */
void free_config_format_tree(struct config_format_tree *tree);

/**
 * Look for a child entry by name.
 *
 * @param[in] e The entry where to search.
 * @param[in] name The name of a searched entry.
 *
 * @return The null pointer if not found, a pointer to the entry on success.
 */
struct config_format_entry *find_config_format_entry(struct config_format_entry *e, 
		const char *name);

/**
 * Look for a child entry value by name.
 *
 * @param[in] e The entry where to search.
 * @param[in] name The name of a searched entry.
 *
 * @return The null pointer if not found, a pointer to the value on success.
 */
const char *find_config_format_entry_value(struct config_format_entry *e, 
		const char *name);

/**
 * Write a path of an entry to a buffer using parent information.
 *
 * The written path format looks like this: path/to/an/entry
 *
 * @param[out] buf The buffer to write the path to.
 * @param[in] size The size of the buffer.
 * @param[in] e We're interested in a path of that entry.
 */
void config_format_entry_path(char *buf, size_t size, struct config_format_entry *e);

/*@}*/
