/* This is a -*- C -*- file fragment.  Please don't compile it, however.  */

typedef struct {
	char *file, *section, *key, *def;
	char *path, *opath;
} ParsedPath;

static const char * GNOME_CONFIG_PARSE_ERROR = "__(null)__";

static void
release_path (ParsedPath *p)
{
	if(p->file != GNOME_CONFIG_PARSE_ERROR)
		g_free (p->file);
	g_free (p->opath);
	g_free (p);
}

static ParsedPath *
parse_path (const char *path, gboolean priv)
{
	ParsedPath *p = g_malloc (sizeof (ParsedPath));

	g_assert(path != NULL);
	
	if (*path == '/' || prefix == NULL)
		p->opath = g_strdup (path);
	else
		p->opath = g_strconcat (prefix, path,NULL);

	p->path    = p->opath;
	p->file    = (char *)GNOME_CONFIG_PARSE_ERROR;
	p->section = (char *)GNOME_CONFIG_PARSE_ERROR;
	p->key     = (char *)GNOME_CONFIG_PARSE_ERROR;

	if (*p->path == '='){
		char *token;
		/* If it is an absolute path name */
		p->path++;
		if ((token = strtok (p->path, "="))) {
			if (token[0]=='/')
				p->file = g_strdup (token);
			else {
				char *cwd = g_get_current_dir();
				p->file = config_concat_dir_and_key (cwd,token);
				g_free(cwd);
			}
		}
		if ((token = strtok (NULL, "/=")))
			p->section = token;
		if ((token = strtok (NULL, "=")))
			p->key     = token;
		p->def     = strtok (NULL, "=");
	} else {
		char *end;

		p->file    = p->path;
		p->def     = NULL;
		if ((end = strchr (p->path, '='))) {
			*end = 0;
			p->def = end + 1;
		} else 
			end = p->path + strlen (p->path);

		/* Look backwards for a slash, to split key from the filename/section */
		while (end > p->path){
			end--;
			if (*end == '/'){
				*end = 0;
				p->key = end + 1;
				break;
			}
		}

		/* Look backwards for the next slash, to get the section name */
		while (end > p->path){
			end--;
			if (*end == '/'){
				*end = 0;
				p->section = end + 1;
				break;
			}
		}
		if (*p->file == '/')
			p->file++;

		if (priv){
			p->file = g_strconcat (g_get_home_dir(), "/", GNOME_DOT_GNOME_PRIVATE, (p->file), NULL);
		} else {
			p->file = g_strconcat (g_get_home_dir(), "/", GNOME_DOT_GNOME, (p->file), NULL);
		}
	}
	if (p->file    == GNOME_CONFIG_PARSE_ERROR ||
	    p->section == GNOME_CONFIG_PARSE_ERROR ||
	    p->section == GNOME_CONFIG_PARSE_ERROR) {
		g_warning ("invalid gnome config path \'%s\'\n", path);
	}
	return p;
}
