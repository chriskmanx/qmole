
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "src/cfgparse.y"

/*
 * vim:ts=4:sw=4:expandtab
 *
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>

#include "all.h"

static pid_t configerror_pid = -1;

static Match current_match;
static Barconfig current_bar;
/* The pattern which was specified by the user, for example -misc-fixed-*. We
 * store this in a separate variable because in the i3 config struct we just
 * store the i3Font. */
static char *font_pattern;

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern int yylex(struct context *context);
extern int yyparse(void);
extern int yylex_destroy(void);
extern FILE *yyin;
YY_BUFFER_STATE yy_scan_string(const char *);

static struct bindings_head *current_bindings;
static struct context *context;

/* We don’t need yydebug for now, as we got decent error messages using
 * yyerror(). Should you ever want to extend the parser, it might be handy
 * to just comment it in again, so it stays here. */
//int yydebug = 1;

void yyerror(const char *error_message) {
    context->has_errors = true;

    ELOG("\n");
    ELOG("CONFIG: %s\n", error_message);
    ELOG("CONFIG: in file \"%s\", line %d:\n",
        context->filename, context->line_number);
    ELOG("CONFIG:   %s\n", context->line_copy);
    char buffer[context->last_column+1];
    buffer[context->last_column] = '\0';
    for (int c = 1; c <= context->last_column; c++)
        buffer[c-1] = (c >= context->first_column ? '^' : ' ');
    ELOG("CONFIG:   %s\n", buffer);
    ELOG("\n");
}

int yywrap() {
    return 1;
}

/*
 * Goes through each line of buf (separated by \n) and checks for statements /
 * commands which only occur in i3 v4 configuration files. If it finds any, it
 * returns version 4, otherwise it returns version 3.
 *
 */
static int detect_version(char *buf) {
    char *walk = buf;
    char *line = buf;
    while (*walk != '\0') {
        if (*walk != '\n') {
            walk++;
            continue;
        }

        /* check for some v4-only statements */
        if (strncasecmp(line, "bindcode", strlen("bindcode")) == 0 ||
            strncasecmp(line, "force_focus_wrapping", strlen("force_focus_wrapping")) == 0 ||
            strncasecmp(line, "# i3 config file (v4)", strlen("# i3 config file (v4)")) == 0 ||
            strncasecmp(line, "workspace_layout", strlen("workspace_layout")) == 0) {
            printf("deciding for version 4 due to this line: %.*s\n", (int)(walk-line), line);
            return 4;
        }

        /* if this is a bind statement, we can check the command */
        if (strncasecmp(line, "bind", strlen("bind")) == 0) {
            char *bind = strchr(line, ' ');
            if (bind == NULL)
                goto next;
            while ((*bind == ' ' || *bind == '\t') && *bind != '\0')
                bind++;
            if (*bind == '\0')
                goto next;
            if ((bind = strchr(bind, ' ')) == NULL)
                goto next;
            while ((*bind == ' ' || *bind == '\t') && *bind != '\0')
                bind++;
            if (*bind == '\0')
                goto next;
            if (strncasecmp(bind, "layout", strlen("layout")) == 0 ||
                strncasecmp(bind, "floating", strlen("floating")) == 0 ||
                strncasecmp(bind, "workspace", strlen("workspace")) == 0 ||
                strncasecmp(bind, "focus left", strlen("focus left")) == 0 ||
                strncasecmp(bind, "focus right", strlen("focus right")) == 0 ||
                strncasecmp(bind, "focus up", strlen("focus up")) == 0 ||
                strncasecmp(bind, "focus down", strlen("focus down")) == 0 ||
                strncasecmp(bind, "border normal", strlen("border normal")) == 0 ||
                strncasecmp(bind, "border 1pixel", strlen("border 1pixel")) == 0 ||
                strncasecmp(bind, "border borderless", strlen("border borderless")) == 0) {
                printf("deciding for version 4 due to this line: %.*s\n", (int)(walk-line), line);
                return 4;
            }
        }

next:
        /* advance to the next line */
        walk++;
        line = walk;
    }

    return 3;
}

/*
 * Calls i3-migrate-config-to-v4 to migrate a configuration file (input
 * buffer).
 *
 * Returns the converted config file or NULL if there was an error (for
 * example the script could not be found in $PATH or the i3 executable’s
 * directory).
 *
 */
static char *migrate_config(char *input, off_t size) {
    int writepipe[2];
    int readpipe[2];

    if (pipe(writepipe) != 0 ||
        pipe(readpipe) != 0) {
        warn("migrate_config: Could not create pipes");
        return NULL;
    }

    pid_t pid = fork();
    if (pid == -1) {
        warn("Could not fork()");
        return NULL;
    }

    /* child */
    if (pid == 0) {
        /* close writing end of writepipe, connect reading side to stdin */
        close(writepipe[1]);
        dup2(writepipe[0], 0);

        /* close reading end of readpipe, connect writing side to stdout */
        close(readpipe[0]);
        dup2(readpipe[1], 1);

        static char *argv[] = {
            NULL, /* will be replaced by the executable path */
            NULL
        };
        exec_i3_utility("i3-migrate-config-to-v4", argv);
    }

    /* parent */

    /* close reading end of the writepipe (connected to the script’s stdin) */
    close(writepipe[0]);

    /* write the whole config file to the pipe, the script will read everything
     * immediately */
    int written = 0;
    int ret;
    while (written < size) {
        if ((ret = write(writepipe[1], input + written, size - written)) < 0) {
            warn("Could not write to pipe");
            return NULL;
        }
        written += ret;
    }
    close(writepipe[1]);

    /* close writing end of the readpipe (connected to the script’s stdout) */
    close(readpipe[1]);

    /* read the script’s output */
    int conv_size = 65535;
    char *converted = malloc(conv_size);
    int read_bytes = 0;
    do {
        if (read_bytes == conv_size) {
            conv_size += 65535;
            converted = realloc(converted, conv_size);
        }
        ret = read(readpipe[0], converted + read_bytes, conv_size - read_bytes);
        if (ret == -1) {
            warn("Cannot read from pipe");
            FREE(converted);
            return NULL;
        }
        read_bytes += ret;
    } while (ret > 0);

    /* get the returncode */
    int status;
    wait(&status);
    if (!WIFEXITED(status)) {
        fprintf(stderr, "Child did not terminate normally, using old config file (will lead to broken behaviour)\n");
        return NULL;
    }

    int returncode = WEXITSTATUS(status);
    if (returncode != 0) {
        fprintf(stderr, "Migration process exit code was != 0\n");
        if (returncode == 2) {
            fprintf(stderr, "could not start the migration script\n");
            /* TODO: script was not found. tell the user to fix his system or create a v4 config */
        } else if (returncode == 1) {
            fprintf(stderr, "This already was a v4 config. Please add the following line to your config file:\n");
            fprintf(stderr, "# i3 config file (v4)\n");
            /* TODO: nag the user with a message to include a hint for i3 in his config file */
        }
        return NULL;
    }

    return converted;
}

/*
 * Handler which will be called when we get a SIGCHLD for the nagbar, meaning
 * it exited (or could not be started, depending on the exit code).
 *
 */
static void nagbar_exited(EV_P_ ev_child *watcher, int revents) {
    ev_child_stop(EV_A_ watcher);
    if (!WIFEXITED(watcher->rstatus)) {
        fprintf(stderr, "ERROR: i3-nagbar did not exit normally.\n");
        return;
    }

    int exitcode = WEXITSTATUS(watcher->rstatus);
    printf("i3-nagbar process exited with status %d\n", exitcode);
    if (exitcode == 2) {
        fprintf(stderr, "ERROR: i3-nagbar could not be found. Is it correctly installed on your system?\n");
    }

    configerror_pid = -1;
}

/* We need ev >= 4 for the following code. Since it is not *that* important (it
 * only makes sure that there are no i3-nagbar instances left behind) we still
 * support old systems with libev 3. */
#if EV_VERSION_MAJOR >= 4
/*
 * Cleanup handler. Will be called when i3 exits. Kills i3-nagbar with signal
 * SIGKILL (9) to make sure there are no left-over i3-nagbar processes.
 *
 */
static void nagbar_cleanup(EV_P_ ev_cleanup *watcher, int revent) {
    if (configerror_pid != -1) {
        LOG("Sending SIGKILL (9) to i3-nagbar with PID %d\n", configerror_pid);
        kill(configerror_pid, SIGKILL);
    }
}
#endif

/*
 * Starts an i3-nagbar process which alerts the user that his configuration
 * file contains one or more errors. Also offers two buttons: One to launch an
 * $EDITOR on the config file and another one to launch a $PAGER on the error
 * logfile.
 *
 */
static void start_configerror_nagbar(const char *config_path) {
    if (only_check_config)
        return;

    fprintf(stderr, "Starting i3-nagbar due to configuration errors\n");
    configerror_pid = fork();
    if (configerror_pid == -1) {
        warn("Could not fork()");
        return;
    }

    /* child */
    if (configerror_pid == 0) {
        char *editaction,
             *pageraction;
        sasprintf(&editaction, "i3-sensible-terminal -e sh -c \"i3-sensible-editor \\\"%s\\\" && i3-msg reload\"", config_path);
        sasprintf(&pageraction, "i3-sensible-terminal -e i3-sensible-pager \"%s\"", errorfilename);
        char *argv[] = {
            NULL, /* will be replaced by the executable path */
            "-t",
            (context->has_errors ? "error" : "warning"),
            "-m",
            (context->has_errors ?
             "You have an error in your i3 config file!" :
             "Your config is outdated. Please fix the warnings to make sure everything works."),
            "-b",
            "edit config",
            editaction,
            (errorfilename ? "-b" : NULL),
            (context->has_errors ? "show errors" : "show warnings"),
            pageraction,
            NULL
        };
        exec_i3_utility("i3-nagbar", argv);
    }

    /* parent */
    /* install a child watcher */
    ev_child *child = smalloc(sizeof(ev_child));
    ev_child_init(child, &nagbar_exited, configerror_pid, 0);
    ev_child_start(main_loop, child);

/* We need ev >= 4 for the following code. Since it is not *that* important (it
 * only makes sure that there are no i3-nagbar instances left behind) we still
 * support old systems with libev 3. */
#if EV_VERSION_MAJOR >= 4
    /* install a cleanup watcher (will be called when i3 exits and i3-nagbar is
     * still running) */
    ev_cleanup *cleanup = smalloc(sizeof(ev_cleanup));
    ev_cleanup_init(cleanup, nagbar_cleanup);
    ev_cleanup_start(main_loop, cleanup);
#endif
}

/*
 * Kills the configerror i3-nagbar process, if any.
 *
 * Called when reloading/restarting.
 *
 * If wait_for_it is set (restarting), this function will waitpid(), otherwise,
 * ev is assumed to handle it (reloading).
 *
 */
void kill_configerror_nagbar(bool wait_for_it) {
    if (configerror_pid == -1)
        return;

    if (kill(configerror_pid, SIGTERM) == -1)
        warn("kill(configerror_nagbar) failed");

    if (!wait_for_it)
        return;

    /* When restarting, we don’t enter the ev main loop anymore and after the
     * exec(), our old pid is no longer watched. So, ev won’t handle SIGCHLD
     * for us and we would end up with a <defunct> process. Therefore we
     * waitpid() here. */
    waitpid(configerror_pid, NULL, 0);
}

/*
 * Checks for duplicate key bindings (the same keycode or keysym is configured
 * more than once). If a duplicate binding is found, a message is printed to
 * stderr and the has_errors variable is set to true, which will start
 * i3-nagbar.
 *
 */
static void check_for_duplicate_bindings(struct context *context) {
    Binding *bind, *current;
    TAILQ_FOREACH(current, bindings, bindings) {
        TAILQ_FOREACH(bind, bindings, bindings) {
            /* Abort when we reach the current keybinding, only check the
             * bindings before */
            if (bind == current)
                break;

            /* Check if one is using keysym while the other is using bindsym.
             * If so, skip. */
            /* XXX: It should be checked at a later place (when translating the
             * keysym to keycodes) if there are any duplicates */
            if ((bind->symbol == NULL && current->symbol != NULL) ||
                (bind->symbol != NULL && current->symbol == NULL))
                continue;

            /* If bind is NULL, current has to be NULL, too (see above).
             * If the keycodes differ, it can't be a duplicate. */
            if (bind->symbol != NULL &&
                strcasecmp(bind->symbol, current->symbol) != 0)
                continue;

            /* Check if the keycodes or modifiers are different. If so, they
             * can't be duplicate */
            if (bind->keycode != current->keycode ||
                bind->mods != current->mods)
                continue;
            context->has_errors = true;
            if (current->keycode != 0) {
                ELOG("Duplicate keybinding in config file:\n  modmask %d with keycode %d, command \"%s\"\n",
                     current->mods, current->keycode, current->command);
            } else {
                ELOG("Duplicate keybinding in config file:\n  modmask %d with keysym %s, command \"%s\"\n",
                     current->mods, current->symbol, current->command);
            }
        }
    }
}

static void migrate_i3bar_exec(struct Autostart *exec) {
    ELOG("**********************************************************************\n");
    ELOG("IGNORING exec command: %s\n", exec->command);
    ELOG("It contains \"i3bar\". Since i3 v4.1, i3bar will be automatically started\n");
    ELOG("for each 'bar' configuration block in your i3 config. Please remove the exec\n");
    ELOG("line and add the following to your i3 config:\n");
    ELOG("\n");
    ELOG("    bar {\n");
    ELOG("        status_command i3status\n");
    ELOG("    }\n");
    ELOG("**********************************************************************\n");

    /* Generate a dummy bar configuration */
    Barconfig *bar_config = scalloc(sizeof(Barconfig));
    /* The hard-coded ID is not a problem. It does not conflict with the
     * auto-generated bar IDs and having multiple hard-coded IDs is irrelevant
     * – they all just contain status_command = i3status */
    bar_config->id = sstrdup("migrate-bar");
    bar_config->status_command = sstrdup("i3status");
    TAILQ_INSERT_TAIL(&barconfigs, bar_config, configs);

    /* Trigger an i3-nagbar */
    context->has_warnings = true;
}

void parse_file(const char *f) {
    SLIST_HEAD(variables_head, Variable) variables = SLIST_HEAD_INITIALIZER(&variables);
    int fd, ret, read_bytes = 0;
    struct stat stbuf;
    char *buf;
    FILE *fstr;
    char buffer[1026], key[512], value[512];

    if ((fd = open(f, O_RDONLY)) == -1)
        die("Could not open configuration file: %s\n", strerror(errno));

    if (fstat(fd, &stbuf) == -1)
        die("Could not fstat file: %s\n", strerror(errno));

    buf = scalloc((stbuf.st_size + 1) * sizeof(char));
    while (read_bytes < stbuf.st_size) {
        if ((ret = read(fd, buf + read_bytes, (stbuf.st_size - read_bytes))) < 0)
            die("Could not read(): %s\n", strerror(errno));
        read_bytes += ret;
    }

    if (lseek(fd, 0, SEEK_SET) == (off_t)-1)
        die("Could not lseek: %s\n", strerror(errno));

    if ((fstr = fdopen(fd, "r")) == NULL)
        die("Could not fdopen: %s\n", strerror(errno));

    while (!feof(fstr)) {
        if (fgets(buffer, 1024, fstr) == NULL) {
            if (feof(fstr))
                break;
            die("Could not read configuration file\n");
        }

        /* sscanf implicitly strips whitespace. Also, we skip comments and empty lines. */
        if (sscanf(buffer, "%s %[^\n]", key, value) < 1 ||
            key[0] == '#' || strlen(key) < 3)
            continue;

        if (strcasecmp(key, "set") == 0) {
            if (value[0] != '$') {
                ELOG("Malformed variable assignment, name has to start with $\n");
                continue;
            }

            /* get key/value for this variable */
            char *v_key = value, *v_value;
            if (strstr(value, " ") == NULL && strstr(value, "\t") == NULL) {
                ELOG("Malformed variable assignment, need a value\n");
                continue;
            }

            if (!(v_value = strstr(value, " ")))
                v_value = strstr(value, "\t");

            *(v_value++) = '\0';

            struct Variable *new = scalloc(sizeof(struct Variable));
            new->key = sstrdup(v_key);
            new->value = sstrdup(v_value);
            SLIST_INSERT_HEAD(&variables, new, variables);
            DLOG("Got new variable %s = %s\n", v_key, v_value);
            continue;
        }
    }
    fclose(fstr);

    /* For every custom variable, see how often it occurs in the file and
     * how much extra bytes it requires when replaced. */
    struct Variable *current, *nearest;
    int extra_bytes = 0;
    /* We need to copy the buffer because we need to invalidate the
     * variables (otherwise we will count them twice, which is bad when
     * 'extra' is negative) */
    char *bufcopy = sstrdup(buf);
    SLIST_FOREACH(current, &variables, variables) {
        int extra = (strlen(current->value) - strlen(current->key));
        char *next;
        for (next = bufcopy;
             next < (bufcopy + stbuf.st_size) &&
             (next = strcasestr(next, current->key)) != NULL;
             next += strlen(current->key)) {
            *next = '_';
            extra_bytes += extra;
        }
    }
    FREE(bufcopy);

    /* Then, allocate a new buffer and copy the file over to the new one,
     * but replace occurences of our variables */
    char *walk = buf, *destwalk;
    char *new = smalloc((stbuf.st_size + extra_bytes + 1) * sizeof(char));
    destwalk = new;
    while (walk < (buf + stbuf.st_size)) {
        /* Find the next variable */
        SLIST_FOREACH(current, &variables, variables)
            current->next_match = strcasestr(walk, current->key);
        nearest = NULL;
        int distance = stbuf.st_size;
        SLIST_FOREACH(current, &variables, variables) {
            if (current->next_match == NULL)
                continue;
            if ((current->next_match - walk) < distance) {
                distance = (current->next_match - walk);
                nearest = current;
            }
        }
        if (nearest == NULL) {
            /* If there are no more variables, we just copy the rest */
            strncpy(destwalk, walk, (buf + stbuf.st_size) - walk);
            destwalk += (buf + stbuf.st_size) - walk;
            *destwalk = '\0';
            break;
        } else {
            /* Copy until the next variable, then copy its value */
            strncpy(destwalk, walk, distance);
            strncpy(destwalk + distance, nearest->value, strlen(nearest->value));
            walk += distance + strlen(nearest->key);
            destwalk += distance + strlen(nearest->value);
        }
    }

    /* analyze the string to find out whether this is an old config file (3.x)
     * or a new config file (4.x). If it’s old, we run the converter script. */
    int version = detect_version(buf);
    if (version == 3) {
        /* We need to convert this v3 configuration */
        char *converted = migrate_config(new, stbuf.st_size);
        if (converted != NULL) {
            printf("\n");
            printf("****************************************************************\n");
            printf("NOTE: Automatically converted configuration file from v3 to v4.\n");
            printf("\n");
            printf("Please convert your config file to v4. You can use this command:\n");
            printf("    mv %s %s.O\n", f, f);
            printf("    i3-migrate-config-to-v4 %s.O > %s\n", f, f);
            printf("****************************************************************\n");
            printf("\n");
            free(new);
            new = converted;
        } else {
            printf("\n");
            printf("**********************************************************************\n");
            printf("ERROR: Could not convert config file. Maybe i3-migrate-config-to-v4\n");
            printf("was not correctly installed on your system?\n");
            printf("**********************************************************************\n");
            printf("\n");
        }
    }

    /* now lex/parse it */
    yy_scan_string(new);

    context = scalloc(sizeof(struct context));
    context->filename = f;

    if (yyparse() != 0) {
        fprintf(stderr, "Could not parse configfile\n");
        exit(1);
    }

    check_for_duplicate_bindings(context);

    /* XXX: The following code will be removed in i3 v4.3 (three releases from
     * now, as of 2011-10-22) */
    /* Check for any exec or exec_always lines starting i3bar. We remove these
     * and add a bar block instead. Additionally, a i3-nagbar warning (not an
     * error) will be displayed so that users update their config file. */
    struct Autostart *exec, *next;
    for (exec = TAILQ_FIRST(&autostarts); exec; ) {
        next = TAILQ_NEXT(exec, autostarts);
        if (strstr(exec->command, "i3bar") != NULL) {
            migrate_i3bar_exec(exec);
            TAILQ_REMOVE(&autostarts, exec, autostarts);
        }
        exec = next;
    }

    for (exec = TAILQ_FIRST(&autostarts_always); exec; ) {
        next = TAILQ_NEXT(exec, autostarts_always);
        if (strstr(exec->command, "i3bar") != NULL) {
            migrate_i3bar_exec(exec);
            TAILQ_REMOVE(&autostarts_always, exec, autostarts_always);
        }
        exec = next;
    }

    if (context->has_errors || context->has_warnings) {
        start_configerror_nagbar(f);
    }

    yylex_destroy();
    FREE(context->line_copy);
    free(context);
    FREE(font_pattern);
    free(new);
    free(buf);

    while (!SLIST_EMPTY(&variables)) {
        current = SLIST_FIRST(&variables);
        FREE(current->key);
        FREE(current->value);
        SLIST_REMOVE_HEAD(&variables, variables);
        FREE(current);
    }
}



/* Line 189 of yacc.c  */
#line 706 "src/cfgparse.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     NUMBER = 258,
     WORD = 259,
     STR = 260,
     STR_NG = 261,
     HEXCOLOR = 262,
     OUTPUT = 263,
     TOKBINDCODE = 264,
     TOKTERMINAL = 265,
     TOKCOMMENT = 266,
     TOKFONT = 267,
     TOKBINDSYM = 268,
     MODIFIER = 269,
     TOKCONTROL = 270,
     TOKSHIFT = 271,
     TOKFLOATING_MODIFIER = 272,
     QUOTEDSTRING = 273,
     TOKWORKSPACE = 274,
     TOKOUTPUT = 275,
     TOKASSIGN = 276,
     TOKSET = 277,
     TOKIPCSOCKET = 278,
     TOKRESTARTSTATE = 279,
     TOKEXEC = 280,
     TOKEXEC_ALWAYS = 281,
     TOKSINGLECOLOR = 282,
     TOKCOLOR = 283,
     TOKARROW = 284,
     TOKMODE = 285,
     TOK_BAR = 286,
     TOK_ORIENTATION = 287,
     TOK_HORIZ = 288,
     TOK_VERT = 289,
     TOK_AUTO = 290,
     TOK_WORKSPACE_LAYOUT = 291,
     TOKNEWWINDOW = 292,
     TOKNEWFLOAT = 293,
     TOK_NORMAL = 294,
     TOK_NONE = 295,
     TOK_1PIXEL = 296,
     TOKFOCUSFOLLOWSMOUSE = 297,
     TOK_FORCE_FOCUS_WRAPPING = 298,
     TOK_FORCE_XINERAMA = 299,
     TOK_WORKSPACE_AUTO_BAF = 300,
     TOKWORKSPACEBAR = 301,
     TOK_DEFAULT = 302,
     TOK_STACKING = 303,
     TOK_TABBED = 304,
     TOKSTACKLIMIT = 305,
     TOK_POPUP_DURING_FULLSCREEN = 306,
     TOK_IGNORE = 307,
     TOK_LEAVE_FULLSCREEN = 308,
     TOK_FOR_WINDOW = 309,
     TOK_BAR_OUTPUT = 310,
     TOK_BAR_TRAY_OUTPUT = 311,
     TOK_BAR_SOCKET_PATH = 312,
     TOK_BAR_MODE = 313,
     TOK_BAR_HIDE = 314,
     TOK_BAR_DOCK = 315,
     TOK_BAR_POSITION = 316,
     TOK_BAR_BOTTOM = 317,
     TOK_BAR_TOP = 318,
     TOK_BAR_STATUS_COMMAND = 319,
     TOK_BAR_FONT = 320,
     TOK_BAR_WORKSPACE_BUTTONS = 321,
     TOK_BAR_VERBOSE = 322,
     TOK_BAR_COLORS = 323,
     TOK_BAR_COLOR_BACKGROUND = 324,
     TOK_BAR_COLOR_STATUSLINE = 325,
     TOK_BAR_COLOR_FOCUSED_WORKSPACE = 326,
     TOK_BAR_COLOR_ACTIVE_WORKSPACE = 327,
     TOK_BAR_COLOR_INACTIVE_WORKSPACE = 328,
     TOK_BAR_COLOR_URGENT_WORKSPACE = 329,
     TOK_NO_STARTUP_ID = 330,
     TOK_MARK = 331,
     TOK_CLASS = 332,
     TOK_INSTANCE = 333,
     TOK_WINDOW_ROLE = 334,
     TOK_ID = 335,
     TOK_CON_ID = 336,
     TOK_TITLE = 337
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 636 "src/cfgparse.y"

    int number;
    char *string;
    uint32_t *single_color;
    struct Colortriple *color;
    Match *match;
    struct Binding *binding;



/* Line 214 of yacc.c  */
#line 835 "src/cfgparse.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 847 "src/cfgparse.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   190

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  89
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  74
/* YYNRULES -- Number of rules.  */
#define YYNRULES  154
/* YYNRULES -- Number of states.  */
#define YYNSTATES  234

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   337

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    88,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    85,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    83,     2,    84,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    86,     2,    87,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,    10,    12,    14,    16,    18,
      20,    22,    24,    26,    28,    30,    32,    34,    36,    38,
      40,    42,    44,    46,    48,    50,    52,    54,    56,    58,
      60,    62,    64,    66,    68,    71,    74,    78,    82,    86,
      87,    91,    93,    95,    98,   100,   104,   108,   112,   116,
     120,   124,   128,   130,   132,   134,   136,   142,   143,   146,
     148,   150,   155,   156,   159,   161,   163,   165,   167,   169,
     171,   173,   175,   177,   179,   181,   183,   185,   187,   189,
     191,   193,   196,   199,   202,   205,   207,   209,   212,   214,
     216,   219,   222,   225,   228,   233,   236,   239,   243,   247,
     251,   255,   258,   261,   263,   265,   267,   270,   275,   277,
     279,   281,   284,   287,   289,   291,   293,   295,   297,   300,
     303,   306,   309,   312,   318,   322,   323,   325,   327,   329,
     331,   335,   339,   341,   343,   346,   349,   353,   357,   358,
     360,   363,   366,   369,   374,   376,   377,   379,   383,   386,
     388,   390,   392,   395,   397
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      90,     0,    -1,    -1,    90,     1,    -1,    90,    91,    -1,
      94,    -1,    98,    -1,   106,    -1,   109,    -1,   130,    -1,
     131,    -1,   133,    -1,   135,    -1,   136,    -1,   139,    -1,
     140,    -1,   141,    -1,   142,    -1,   143,    -1,   144,    -1,
     147,    -1,   149,    -1,   150,    -1,   151,    -1,   152,    -1,
     156,    -1,   157,    -1,   154,    -1,   155,    -1,    92,    -1,
     161,    -1,    11,    -1,     5,    -1,    95,    -1,     9,    96,
      -1,    13,    97,    -1,   159,     3,    93,    -1,   159,   105,
      93,    -1,    54,    99,    93,    -1,    -1,   100,   102,   101,
      -1,    83,    -1,    84,    -1,   102,   103,    -1,   103,    -1,
      77,    85,     5,    -1,    78,    85,     5,    -1,    79,    85,
       5,    -1,    81,    85,     5,    -1,    80,    85,     5,    -1,
      76,    85,     5,    -1,    82,    85,     5,    -1,    18,    -1,
       3,    -1,     4,    -1,     3,    -1,    30,    18,    86,   107,
      87,    -1,    -1,   107,   108,    -1,    92,    -1,    95,    -1,
      31,    86,   110,    87,    -1,    -1,   110,   111,    -1,    92,
      -1,   112,    -1,   113,    -1,   114,    -1,   115,    -1,   117,
      -1,   119,    -1,   120,    -1,   121,    -1,   122,    -1,   123,
      -1,   124,    -1,   125,    -1,   126,    -1,   127,    -1,   128,
      -1,   129,    -1,    64,     5,    -1,    55,     5,    -1,    56,
       5,    -1,    61,   116,    -1,    63,    -1,    62,    -1,    58,
     118,    -1,    59,    -1,    60,    -1,    65,     5,    -1,    66,
     138,    -1,    67,   138,    -1,    57,     5,    -1,    68,    86,
     110,    87,    -1,    69,     7,    -1,    70,     7,    -1,    71,
       7,     7,    -1,    72,     7,     7,    -1,    73,     7,     7,
      -1,    74,     7,     7,    -1,    17,   159,    -1,    32,   132,
      -1,    33,    -1,    34,    -1,    35,    -1,    36,   134,    -1,
      36,    50,    50,     3,    -1,    47,    -1,    48,    -1,    49,
      -1,    37,   137,    -1,    38,   137,    -1,    39,    -1,    40,
      -1,    41,    -1,     3,    -1,     4,    -1,    42,   138,    -1,
      43,   138,    -1,    44,   138,    -1,    45,   138,    -1,    46,
     138,    -1,    19,   104,    20,     8,   145,    -1,    19,     3,
     146,    -1,    -1,   146,    -1,    18,    -1,     5,    -1,     4,
      -1,    21,   148,     5,    -1,    21,    99,     5,    -1,    18,
      -1,     6,    -1,    23,     5,    -1,    24,     5,    -1,    25,
     153,     5,    -1,    26,   153,     5,    -1,    -1,    75,    -1,
      10,     5,    -1,    12,     5,    -1,    27,   158,    -1,    28,
     158,   158,   158,    -1,     7,    -1,    -1,   160,    -1,   159,
      88,   160,    -1,   159,    88,    -1,    14,    -1,    15,    -1,
      16,    -1,    51,   162,    -1,    52,    -1,    53,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   753,   753,   754,   755,   759,   760,   761,   762,   763,
     764,   765,   766,   767,   768,   769,   770,   771,   772,   773,
     774,   775,   776,   777,   778,   779,   780,   781,   782,   783,
     784,   788,   792,   796,   803,   804,   808,   822,   836,   851,
     852,   859,   867,   874,   875,   879,   885,   891,   897,   912,
     927,   933,   942,   943,   947,   948,   955,   978,   980,   984,
     985,   997,  1024,  1026,  1030,  1031,  1032,  1033,  1034,  1035,
    1036,  1037,  1038,  1039,  1040,  1041,  1042,  1043,  1044,  1045,
    1046,  1050,  1059,  1070,  1079,  1087,  1088,  1092,  1100,  1101,
    1105,  1114,  1124,  1132,  1141,  1150,  1158,  1166,  1175,  1184,
    1193,  1202,  1210,  1218,  1219,  1220,  1224,  1248,  1269,  1270,
    1271,  1275,  1283,  1291,  1292,  1293,  1297,  1301,  1313,  1321,
    1329,  1337,  1345,  1353,  1387,  1405,  1406,  1410,  1411,  1412,
    1416,  1478,  1494,  1495,  1499,  1506,  1513,  1523,  1533,  1534,
    1538,  1546,  1556,  1564,  1575,  1584,  1585,  1586,  1587,  1591,
    1592,  1593,  1597,  1605,  1606
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "\"<number>\"", "\"<word>\"",
  "\"<string>\"", "\"<string (non-greedy)>\"", "\"#<hex>\"",
  "\"<RandR output>\"", "TOKBINDCODE", "TOKTERMINAL", "\"<comment>\"",
  "\"font\"", "\"bindsym\"", "\"<modifier>\"", "\"control\"", "\"shift\"",
  "\"floating_modifier\"", "\"<quoted string>\"", "\"workspace\"",
  "\"output\"", "\"assign\"", "TOKSET", "\"ipc_socket\"",
  "\"restart_state\"", "\"exec\"", "\"exec_always\"", "TOKSINGLECOLOR",
  "TOKCOLOR", "\"\\342\\206\\222\"", "\"mode\"", "\"bar\"",
  "\"default_orientation\"", "\"horizontal\"", "\"vertical\"", "\"auto\"",
  "\"workspace_layout\"", "\"new_window\"", "\"new_float\"", "\"normal\"",
  "\"none\"", "\"1pixel\"", "\"focus_follows_mouse\"",
  "\"force_focus_wrapping\"", "\"force_xinerama\"",
  "\"workspace_auto_back_and_forth\"", "\"workspace_bar\"", "\"default\"",
  "\"stacking\"", "\"tabbed\"", "\"stack-limit\"",
  "\"popup_during_fullscreen\"", "\"ignore\"", "\"leave_fullscreen\"",
  "\"for_window\"", "\"output (bar)\"", "\"tray_output\"",
  "\"socket_path\"", "\"mode (bar)\"", "\"hide\"", "\"dock\"",
  "\"position\"", "\"bottom\"", "\"top\"", "\"status_command\"",
  "\"font (bar)\"", "\"workspace_buttons\"", "\"verbose\"", "\"colors\"",
  "\"background\"", "\"statusline\"", "\"focused_workspace\"",
  "\"active_workspace\"", "\"inactive_workspace\"", "\"urgent_workspace\"",
  "\"--no-startup-id\"", "\"mark\"", "\"class\"", "\"instance\"",
  "\"window_role\"", "\"id\"", "\"con_id\"", "\"title\"", "'['", "']'",
  "'='", "'{'", "'}'", "'+'", "$accept", "lines", "line", "comment",
  "command", "bindline", "binding", "bindcode", "bindsym", "for_window",
  "match", "matchstart", "matchend", "criteria", "criterion",
  "qstring_or_number", "word_or_number", "mode", "modelines", "modeline",
  "bar", "barlines", "barline", "bar_status_command", "bar_output",
  "bar_tray_output", "bar_position", "bar_position_position", "bar_mode",
  "bar_mode_mode", "bar_font", "bar_workspace_buttons", "bar_verbose",
  "bar_socket_path", "bar_colors", "bar_color_background",
  "bar_color_statusline", "bar_color_focused_workspace",
  "bar_color_active_workspace", "bar_color_inactive_workspace",
  "bar_color_urgent_workspace", "floating_modifier", "orientation",
  "direction", "workspace_layout", "layout_mode", "new_window",
  "new_float", "border_style", "bool", "focus_follows_mouse",
  "force_focus_wrapping", "force_xinerama", "workspace_back_and_forth",
  "workspace_bar", "workspace", "optional_workspace_name",
  "workspace_name", "assign", "window_class", "ipcsocket", "restart_state",
  "exec", "exec_always", "optional_no_startup_id", "terminal", "font",
  "single_color", "color", "colorpixel", "binding_modifiers",
  "binding_modifier", "popup_during_fullscreen", "popup_setting", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,    91,    93,    61,   123,   125,    43
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    89,    90,    90,    90,    91,    91,    91,    91,    91,
      91,    91,    91,    91,    91,    91,    91,    91,    91,    91,
      91,    91,    91,    91,    91,    91,    91,    91,    91,    91,
      91,    92,    93,    94,    95,    95,    96,    97,    98,    99,
      99,   100,   101,   102,   102,   103,   103,   103,   103,   103,
     103,   103,   104,   104,   105,   105,   106,   107,   107,   108,
     108,   109,   110,   110,   111,   111,   111,   111,   111,   111,
     111,   111,   111,   111,   111,   111,   111,   111,   111,   111,
     111,   112,   113,   114,   115,   116,   116,   117,   118,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   132,   132,   133,   133,   134,   134,
     134,   135,   136,   137,   137,   137,   138,   138,   139,   140,
     141,   142,   143,   144,   144,   145,   145,   146,   146,   146,
     147,   147,   148,   148,   149,   150,   151,   152,   153,   153,
     154,   155,   156,   157,   158,   159,   159,   159,   159,   160,
     160,   160,   161,   162,   162
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     2,     3,     3,     3,     0,
       3,     1,     1,     2,     1,     3,     3,     3,     3,     3,
       3,     3,     1,     1,     1,     1,     5,     0,     2,     1,
       1,     4,     0,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     2,     2,     2,     1,     1,     2,     1,     1,
       2,     2,     2,     2,     4,     2,     2,     3,     3,     3,
       3,     2,     2,     1,     1,     1,     2,     4,     1,     1,
       1,     2,     2,     1,     1,     1,     1,     1,     2,     2,
       2,     2,     2,     5,     3,     0,     1,     1,     1,     1,
       3,     3,     1,     1,     2,     2,     3,     3,     0,     1,
       2,     2,     2,     4,     1,     0,     1,     3,     2,     1,
       1,     1,     2,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     1,     3,   145,     0,    31,     0,   145,   145,
       0,    39,     0,     0,   138,   138,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      39,     4,    29,     5,    33,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    27,    28,    25,    26,    30,   149,
     150,   151,    34,     0,   146,   140,   141,    35,     0,   101,
      53,    52,     0,   133,   132,    41,     0,     0,     0,   134,
     135,   139,     0,     0,   144,   142,     0,     0,    62,   103,
     104,   105,   102,   108,   109,   110,     0,   106,   113,   114,
     115,   111,   112,   116,   117,   118,   119,   120,   121,   122,
     153,   154,   152,     0,     0,   148,    55,    54,     0,   129,
     128,   127,   124,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,    44,   130,   136,   137,     0,    57,     0,
       0,    32,    38,    36,   147,    37,   125,     0,     0,     0,
       0,     0,     0,     0,    42,    40,    43,   143,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,    64,    63,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,   107,   123,   126,    50,    45,    46,
      47,    49,    48,    51,    56,    59,    60,    58,    82,    83,
      93,    88,    89,    87,    86,    85,    84,    81,    90,    91,
      92,    62,    95,    96,     0,     0,     0,     0,     0,    97,
      98,    99,   100,    94
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    31,   176,   142,    33,    34,    62,    67,    35,
      76,    77,   155,   132,   133,    72,   118,    36,   158,   207,
      37,   139,   177,   178,   179,   180,   181,   216,   182,   213,
     183,   184,   185,   186,   187,   188,   189,   190,   191,   192,
     193,    38,    39,    92,    40,    97,    41,    42,   101,   105,
      43,    44,    45,    46,    47,    48,   195,   122,    49,    78,
      50,    51,    52,    53,    82,    54,    55,    56,    57,    85,
      63,    64,    58,   112
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -99
static const yytype_int16 yypact[] =
{
     -99,   128,   -99,   -99,    37,    16,   -99,    18,    37,    37,
       7,     6,    31,    44,   -13,   -13,    52,    52,    47,     2,
     -16,    -2,    15,    15,    23,    23,    23,    23,    23,     8,
     -10,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,     5,   -99,   -99,   -99,   -99,     3,    13,
      10,   -99,    66,   -99,   -99,   -99,    85,   -38,    87,   -99,
     -99,   -99,    94,    97,   -99,   -99,    52,    28,   -99,   -99,
     -99,   -99,   -99,   -99,   -99,   -99,    65,   -99,   -99,   -99,
     -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,   111,   111,    37,   -99,   -99,   111,   -99,
     -99,   -99,   -99,   109,   -99,    33,    34,    35,    36,    38,
      42,    45,   -47,   -99,   -99,   -99,   -99,    52,   -99,    11,
     119,   -99,   -99,   -99,   -99,   -99,    10,   126,   127,   129,
     130,   131,   139,   141,   -99,   -99,   -99,   -99,     0,   143,
     145,   156,     4,    -5,   157,   158,    23,    23,    81,   117,
     161,   162,   168,   169,   170,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,   -99,   171,   173,   174,   176,    39,   -99,
     -99,   -99,   -99,   -99
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -99,   -99,   -99,    -1,   -98,   -99,   -25,   -99,   -99,   -99,
     154,   -99,   -99,   -99,    53,   -99,   -99,   -99,   -99,   -99,
     -99,   -35,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,   -99,   -99,   -99,   -99,   -99,   -99,   164,   -24,
     -99,   -99,   -99,   -99,   -99,   -99,   -99,    43,   -99,   -99,
     -99,   -99,   -99,   -99,   175,   -99,   -99,   -99,   -99,   -12,
      62,    73,   -99,   -99
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      32,   106,   107,   108,   109,    86,   116,   117,   114,     4,
      70,     6,    73,     8,   119,   120,   143,    89,    90,    91,
     145,    65,     6,    66,    74,    71,   103,   104,   121,   125,
     126,   127,   128,   129,   130,   131,    79,   154,   125,   126,
     127,   128,   129,   130,   131,    93,    94,    95,    96,    80,
       6,    59,    60,    61,    98,    99,   100,   214,   215,    84,
     110,   111,    81,   211,   212,    87,   159,   160,   161,   162,
      68,    69,   163,    75,   137,   164,   165,   166,   167,   168,
     169,   170,   171,   172,   173,   174,   123,   204,    88,    75,
     124,   115,   134,   115,   159,   160,   161,   162,   175,   135,
     163,   115,   136,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   138,   140,   141,   146,   147,   148,
     149,   150,   194,   151,   222,   157,   233,   152,     2,     3,
     153,   197,   198,   206,   199,   200,   201,     4,     5,     6,
       7,     8,   219,   220,   202,     9,   203,    10,   208,    11,
     209,    12,    13,    14,    15,    16,    17,   205,    18,    19,
      20,   210,   217,   218,    21,    22,    23,   221,   223,   224,
      24,    25,    26,    27,    28,   225,   226,   227,   229,    29,
     230,   231,    30,   232,   113,   156,   228,   102,   144,   196,
      83
};

static const yytype_uint8 yycheck[] =
{
       1,    25,    26,    27,    28,    17,     3,     4,     3,     9,
       3,    11,     6,    13,     4,     5,   114,    33,    34,    35,
     118,     5,    11,     5,    18,    18,     3,     4,    18,    76,
      77,    78,    79,    80,    81,    82,     5,    84,    76,    77,
      78,    79,    80,    81,    82,    47,    48,    49,    50,     5,
      11,    14,    15,    16,    39,    40,    41,    62,    63,     7,
      52,    53,    75,    59,    60,    18,    55,    56,    57,    58,
       8,     9,    61,    83,    86,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    20,    87,    86,    83,
       5,    88,     5,    88,    55,    56,    57,    58,    87,     5,
      61,    88,     5,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    86,    50,     5,     8,    85,    85,
      85,    85,     3,    85,     7,   137,    87,    85,     0,     1,
      85,     5,     5,   158,     5,     5,     5,     9,    10,    11,
      12,    13,   166,   167,     5,    17,     5,    19,     5,    21,
       5,    23,    24,    25,    26,    27,    28,   158,    30,    31,
      32,     5,     5,     5,    36,    37,    38,    86,     7,     7,
      42,    43,    44,    45,    46,     7,     7,     7,     7,    51,
       7,     7,    54,     7,    30,   132,   221,    23,   115,   146,
      15
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    90,     0,     1,     9,    10,    11,    12,    13,    17,
      19,    21,    23,    24,    25,    26,    27,    28,    30,    31,
      32,    36,    37,    38,    42,    43,    44,    45,    46,    51,
      54,    91,    92,    94,    95,    98,   106,   109,   130,   131,
     133,   135,   136,   139,   140,   141,   142,   143,   144,   147,
     149,   150,   151,   152,   154,   155,   156,   157,   161,    14,
      15,    16,    96,   159,   160,     5,     5,    97,   159,   159,
       3,    18,   104,     6,    18,    83,    99,   100,   148,     5,
       5,    75,   153,   153,     7,   158,   158,    18,    86,    33,
      34,    35,   132,    47,    48,    49,    50,   134,    39,    40,
      41,   137,   137,     3,     4,   138,   138,   138,   138,   138,
      52,    53,   162,    99,     3,    88,     3,     4,   105,     4,
       5,    18,   146,    20,     5,    76,    77,    78,    79,    80,
      81,    82,   102,   103,     5,     5,     5,   158,    86,   110,
      50,     5,    93,    93,   160,    93,     8,    85,    85,    85,
      85,    85,    85,    85,    84,   101,   103,   158,   107,    55,
      56,    57,    58,    61,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    87,    92,   111,   112,   113,
     114,   115,   117,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,     3,   145,   146,     5,     5,     5,
       5,     5,     5,     5,    87,    92,    95,   108,     5,     5,
       5,    59,    60,   118,    62,    63,   116,     5,     5,   138,
     138,    86,     7,     7,     7,     7,     7,     7,   110,     7,
       7,     7,     7,    87
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex (context)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 33:

/* Line 1455 of yacc.c  */
#line 797 "src/cfgparse.y"
    {
        TAILQ_INSERT_TAIL(bindings, (yyvsp[(1) - (1)].binding), bindings);
    ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 803 "src/cfgparse.y"
    { (yyval.binding) = (yyvsp[(2) - (2)].binding); ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 804 "src/cfgparse.y"
    { (yyval.binding) = (yyvsp[(2) - (2)].binding); ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 809 "src/cfgparse.y"
    {
        printf("\tFound keycode binding mod%d with key %d and command %s\n", (yyvsp[(1) - (3)].number), (yyvsp[(2) - (3)].number), (yyvsp[(3) - (3)].string));
        Binding *new = scalloc(sizeof(Binding));

        new->keycode = (yyvsp[(2) - (3)].number);
        new->mods = (yyvsp[(1) - (3)].number);
        new->command = (yyvsp[(3) - (3)].string);

        (yyval.binding) = new;
    ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 823 "src/cfgparse.y"
    {
        printf("\tFound keysym binding mod%d with key %s and command %s\n", (yyvsp[(1) - (3)].number), (yyvsp[(2) - (3)].string), (yyvsp[(3) - (3)].string));
        Binding *new = scalloc(sizeof(Binding));

        new->symbol = (yyvsp[(2) - (3)].string);
        new->mods = (yyvsp[(1) - (3)].number);
        new->command = (yyvsp[(3) - (3)].string);

        (yyval.binding) = new;
    ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 837 "src/cfgparse.y"
    {
        if (match_is_empty(&current_match)) {
            ELOG("Match is empty, ignoring this for_window statement\n");
            break;
        }
        printf("\t should execute command %s for the criteria mentioned above\n", (yyvsp[(3) - (3)].string));
        Assignment *assignment = scalloc(sizeof(Assignment));
        assignment->type = A_COMMAND;
        assignment->match = current_match;
        assignment->dest.command = (yyvsp[(3) - (3)].string);
        TAILQ_INSERT_TAIL(&assignments, assignment, assignments);
    ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 853 "src/cfgparse.y"
    {
        printf("match parsed\n");
    ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 860 "src/cfgparse.y"
    {
        printf("start\n");
        match_init(&current_match);
    ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 868 "src/cfgparse.y"
    {
        printf("match specification finished\n");
    ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 880 "src/cfgparse.y"
    {
        printf("criteria: class = %s\n", (yyvsp[(3) - (3)].string));
        current_match.class = regex_new((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 886 "src/cfgparse.y"
    {
        printf("criteria: instance = %s\n", (yyvsp[(3) - (3)].string));
        current_match.instance = regex_new((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 892 "src/cfgparse.y"
    {
        printf("criteria: window_role = %s\n", (yyvsp[(3) - (3)].string));
        current_match.role = regex_new((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 898 "src/cfgparse.y"
    {
        printf("criteria: id = %s\n", (yyvsp[(3) - (3)].string));
        char *end;
        long parsed = strtol((yyvsp[(3) - (3)].string), &end, 10);
        if (parsed == LONG_MIN ||
            parsed == LONG_MAX ||
            parsed < 0 ||
            (end && *end != '\0')) {
            ELOG("Could not parse con id \"%s\"\n", (yyvsp[(3) - (3)].string));
        } else {
            current_match.con_id = (Con*)parsed;
            printf("id as int = %p\n", current_match.con_id);
        }
    ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 913 "src/cfgparse.y"
    {
        printf("criteria: window id = %s\n", (yyvsp[(3) - (3)].string));
        char *end;
        long parsed = strtol((yyvsp[(3) - (3)].string), &end, 10);
        if (parsed == LONG_MIN ||
            parsed == LONG_MAX ||
            parsed < 0 ||
            (end && *end != '\0')) {
            ELOG("Could not parse window id \"%s\"\n", (yyvsp[(3) - (3)].string));
        } else {
            current_match.id = parsed;
            printf("window id as int = %d\n", current_match.id);
        }
    ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 928 "src/cfgparse.y"
    {
        printf("criteria: mark = %s\n", (yyvsp[(3) - (3)].string));
        current_match.mark = regex_new((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 934 "src/cfgparse.y"
    {
        printf("criteria: title = %s\n", (yyvsp[(3) - (3)].string));
        current_match.title = regex_new((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 943 "src/cfgparse.y"
    { sasprintf(&(yyval.string), "%d", (yyvsp[(1) - (1)].number)); ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 949 "src/cfgparse.y"
    {
        sasprintf(&(yyval.string), "%d", (yyvsp[(1) - (1)].number));
    ;}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 956 "src/cfgparse.y"
    {
        if (strcasecmp((yyvsp[(2) - (5)].string), "default") == 0) {
            printf("You cannot use the name \"default\" for your mode\n");
            exit(1);
        }
        printf("\t now in mode %s\n", (yyvsp[(2) - (5)].string));
        printf("\t current bindings = %p\n", current_bindings);
        Binding *binding;
        TAILQ_FOREACH(binding, current_bindings, bindings) {
            printf("got binding on mods %d, keycode %d, symbol %s, command %s\n",
                            binding->mods, binding->keycode, binding->symbol, binding->command);
        }

        struct Mode *mode = scalloc(sizeof(struct Mode));
        mode->name = (yyvsp[(2) - (5)].string);
        mode->bindings = current_bindings;
        current_bindings = NULL;
        SLIST_INSERT_HEAD(&modes, mode, modes);
    ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 986 "src/cfgparse.y"
    {
        if (current_bindings == NULL) {
            current_bindings = scalloc(sizeof(struct bindings_head));
            TAILQ_INIT(current_bindings);
        }

        TAILQ_INSERT_TAIL(current_bindings, (yyvsp[(1) - (1)].binding), bindings);
    ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 998 "src/cfgparse.y"
    {
        printf("\t new bar configuration finished, saving.\n");
        /* Generate a unique ID for this bar */
        current_bar.id = sstrdup("bar-XXXXXX");
        /* This works similar to mktemp in that it replaces the last six X with
         * random letters, but without the restriction that the given buffer
         * has to contain a valid path name. */
        char *x = current_bar.id + strlen("bar-");
        while (*x != '\0') {
            *(x++) = (rand() % 26) + 'a';
        }

        /* If no font was explicitly set, we use the i3 font as default */
        if (!current_bar.font && font_pattern)
            current_bar.font = sstrdup(font_pattern);

        /* Copy the current (static) structure into a dynamically allocated
         * one, then cleanup our static one. */
        Barconfig *bar_config = scalloc(sizeof(Barconfig));
        memcpy(bar_config, &current_bar, sizeof(Barconfig));
        TAILQ_INSERT_TAIL(&barconfigs, bar_config, configs);

        memset(&current_bar, '\0', sizeof(Barconfig));
    ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 1051 "src/cfgparse.y"
    {
        DLOG("should add status command %s\n", (yyvsp[(2) - (2)].string));
        FREE(current_bar.status_command);
        current_bar.status_command = (yyvsp[(2) - (2)].string);
    ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 1060 "src/cfgparse.y"
    {
        DLOG("bar output %s\n", (yyvsp[(2) - (2)].string));
        int new_outputs = current_bar.num_outputs + 1;
        current_bar.outputs = srealloc(current_bar.outputs, sizeof(char*) * new_outputs);
        current_bar.outputs[current_bar.num_outputs] = (yyvsp[(2) - (2)].string);
        current_bar.num_outputs = new_outputs;
    ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 1071 "src/cfgparse.y"
    {
        DLOG("tray %s\n", (yyvsp[(2) - (2)].string));
        FREE(current_bar.tray_output);
        current_bar.tray_output = (yyvsp[(2) - (2)].string);
    ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 1080 "src/cfgparse.y"
    {
        DLOG("position %d\n", (yyvsp[(2) - (2)].number));
        current_bar.position = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 1087 "src/cfgparse.y"
    { (yyval.number) = P_TOP; ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 1088 "src/cfgparse.y"
    { (yyval.number) = P_BOTTOM; ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 1093 "src/cfgparse.y"
    {
        DLOG("mode %d\n", (yyvsp[(2) - (2)].number));
        current_bar.mode = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 1100 "src/cfgparse.y"
    { (yyval.number) = M_HIDE; ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 1101 "src/cfgparse.y"
    { (yyval.number) = M_DOCK; ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 1106 "src/cfgparse.y"
    {
        DLOG("font %s\n", (yyvsp[(2) - (2)].string));
        FREE(current_bar.font);
        current_bar.font = (yyvsp[(2) - (2)].string);
    ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 1115 "src/cfgparse.y"
    {
        DLOG("workspace_buttons = %d\n", (yyvsp[(2) - (2)].number));
        /* We store this inverted to make the default setting right when
         * initializing the struct with zero. */
        current_bar.hide_workspace_buttons = !((yyvsp[(2) - (2)].number));
    ;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 1125 "src/cfgparse.y"
    {
        DLOG("verbose = %d\n", (yyvsp[(2) - (2)].number));
        current_bar.verbose = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 1133 "src/cfgparse.y"
    {
        DLOG("socket_path = %s\n", (yyvsp[(2) - (2)].string));
        FREE(current_bar.socket_path);
        current_bar.socket_path = (yyvsp[(2) - (2)].string);
    ;}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 1142 "src/cfgparse.y"
    {
        /* At the moment, the TOK_BAR_COLORS token is only to make the config
         * friendlier for humans. We might change this in the future if it gets
         * more complex. */
    ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 1151 "src/cfgparse.y"
    {
        DLOG("background = %s\n", (yyvsp[(2) - (2)].string));
        current_bar.colors.background = (yyvsp[(2) - (2)].string);
    ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 1159 "src/cfgparse.y"
    {
        DLOG("statusline = %s\n", (yyvsp[(2) - (2)].string));
        current_bar.colors.statusline = (yyvsp[(2) - (2)].string);
    ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 1167 "src/cfgparse.y"
    {
        DLOG("focused_ws = %s and %s\n", (yyvsp[(2) - (3)].string), (yyvsp[(3) - (3)].string));
        current_bar.colors.focused_workspace_text = (yyvsp[(2) - (3)].string);
        current_bar.colors.focused_workspace_bg = (yyvsp[(3) - (3)].string);
    ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 1176 "src/cfgparse.y"
    {
        DLOG("active_ws = %s and %s\n", (yyvsp[(2) - (3)].string), (yyvsp[(3) - (3)].string));
        current_bar.colors.active_workspace_text = (yyvsp[(2) - (3)].string);
        current_bar.colors.active_workspace_bg = (yyvsp[(3) - (3)].string);
    ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 1185 "src/cfgparse.y"
    {
        DLOG("inactive_ws = %s and %s\n", (yyvsp[(2) - (3)].string), (yyvsp[(3) - (3)].string));
        current_bar.colors.inactive_workspace_text = (yyvsp[(2) - (3)].string);
        current_bar.colors.inactive_workspace_bg = (yyvsp[(3) - (3)].string);
    ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 1194 "src/cfgparse.y"
    {
        DLOG("urgent_ws = %s and %s\n", (yyvsp[(2) - (3)].string), (yyvsp[(3) - (3)].string));
        current_bar.colors.urgent_workspace_text = (yyvsp[(2) - (3)].string);
        current_bar.colors.urgent_workspace_bg = (yyvsp[(3) - (3)].string);
    ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 1203 "src/cfgparse.y"
    {
        DLOG("floating modifier = %d\n", (yyvsp[(2) - (2)].number));
        config.floating_modifier = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 1211 "src/cfgparse.y"
    {
        DLOG("New containers should start with split direction %d\n", (yyvsp[(2) - (2)].number));
        config.default_orientation = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 1218 "src/cfgparse.y"
    { (yyval.number) = HORIZ; ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 1219 "src/cfgparse.y"
    { (yyval.number) = VERT; ;}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 1220 "src/cfgparse.y"
    { (yyval.number) = NO_ORIENTATION; ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 1225 "src/cfgparse.y"
    {
        DLOG("new containers will be in mode %d\n", (yyvsp[(2) - (2)].number));
        config.default_layout = (yyvsp[(2) - (2)].number);

#if 0
        /* We also need to change the layout of the already existing
         * workspaces here. Workspaces may exist at this point because
         * of the other directives which are modifying workspaces
         * (setting the preferred screen or name). While the workspace
         * objects are already created, they have never been used.
         * Thus, the user very likely awaits the default container mode
         * to trigger in this case, regardless of where it is inside
         * his configuration file. */
        Workspace *ws;
        TAILQ_FOREACH(ws, workspaces, workspaces) {
                if (ws->table == NULL)
                        continue;
                switch_layout_mode(global_conn,
                                   ws->table[0][0],
                                   config.container_mode);
        }
#endif
    ;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 1249 "src/cfgparse.y"
    {
        DLOG("stack-limit %d with val %d\n", (yyvsp[(3) - (4)].number), (yyvsp[(4) - (4)].number));
        config.container_stack_limit = (yyvsp[(3) - (4)].number);
        config.container_stack_limit_value = (yyvsp[(4) - (4)].number);

#if 0
        /* See the comment above */
        Workspace *ws;
        TAILQ_FOREACH(ws, workspaces, workspaces) {
                if (ws->table == NULL)
                        continue;
                Container *con = ws->table[0][0];
                con->stack_limit = config.container_stack_limit;
                con->stack_limit_value = config.container_stack_limit_value;
        }
#endif
    ;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 1269 "src/cfgparse.y"
    { (yyval.number) = L_DEFAULT; ;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 1270 "src/cfgparse.y"
    { (yyval.number) = L_STACKED; ;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 1271 "src/cfgparse.y"
    { (yyval.number) = L_TABBED; ;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 1276 "src/cfgparse.y"
    {
        DLOG("new windows should start with border style %d\n", (yyvsp[(2) - (2)].number));
        config.default_border = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 1284 "src/cfgparse.y"
    {
       DLOG("new floating windows should start with border style %d\n", (yyvsp[(2) - (2)].number));
       config.default_floating_border = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 1291 "src/cfgparse.y"
    { (yyval.number) = BS_NORMAL; ;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 1292 "src/cfgparse.y"
    { (yyval.number) = BS_NONE; ;}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 1293 "src/cfgparse.y"
    { (yyval.number) = BS_1PIXEL; ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 1298 "src/cfgparse.y"
    {
        (yyval.number) = ((yyvsp[(1) - (1)].number) == 1);
    ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 1302 "src/cfgparse.y"
    {
        DLOG("checking word \"%s\"\n", (yyvsp[(1) - (1)].string));
        (yyval.number) = (strcasecmp((yyvsp[(1) - (1)].string), "yes") == 0 ||
              strcasecmp((yyvsp[(1) - (1)].string), "true") == 0 ||
              strcasecmp((yyvsp[(1) - (1)].string), "on") == 0 ||
              strcasecmp((yyvsp[(1) - (1)].string), "enable") == 0 ||
              strcasecmp((yyvsp[(1) - (1)].string), "active") == 0);
    ;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 1314 "src/cfgparse.y"
    {
        DLOG("focus follows mouse = %d\n", (yyvsp[(2) - (2)].number));
        config.disable_focus_follows_mouse = !((yyvsp[(2) - (2)].number));
    ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 1322 "src/cfgparse.y"
    {
        DLOG("force focus wrapping = %d\n", (yyvsp[(2) - (2)].number));
        config.force_focus_wrapping = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 1330 "src/cfgparse.y"
    {
        DLOG("force xinerama = %d\n", (yyvsp[(2) - (2)].number));
        config.force_xinerama = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 1338 "src/cfgparse.y"
    {
        DLOG("automatic workspace back-and-forth = %d\n", (yyvsp[(2) - (2)].number));
        config.workspace_auto_back_and_forth = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 1346 "src/cfgparse.y"
    {
        DLOG("workspace bar = %d\n", (yyvsp[(2) - (2)].number));
        config.disable_workspace_bar = !((yyvsp[(2) - (2)].number));
    ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 1354 "src/cfgparse.y"
    {
        char *ws_name = (yyvsp[(2) - (5)].string);

        if ((yyvsp[(5) - (5)].string) != NULL) {
            ELOG("The old (v3) syntax workspace <number> output <output> <name> is deprecated.\n");
            ELOG("Please use the new syntax: workspace \"<workspace>\" output <output>\n");
            ELOG("In your case, the following should work:\n");
            ELOG("    workspace \"%s\" output %s\n", (yyvsp[(5) - (5)].string), (yyvsp[(4) - (5)].string));
            ws_name = (yyvsp[(5) - (5)].string);
            context->has_warnings = true;
        }

        DLOG("Assigning workspace \"%s\" to output \"%s\"\n", ws_name, (yyvsp[(4) - (5)].string));
        /* Check for earlier assignments of the same workspace so that we
         * don’t have assignments of a single workspace to different
         * outputs */
        struct Workspace_Assignment *assignment;
        bool duplicate = false;
        TAILQ_FOREACH(assignment, &ws_assignments, ws_assignments) {
            if (strcasecmp(assignment->name, ws_name) == 0) {
                ELOG("You have a duplicate workspace assignment for workspace \"%s\"\n",
                     ws_name);
                assignment->output = (yyvsp[(4) - (5)].string);
                duplicate = true;
            }
        }
        if (!duplicate) {
            assignment = scalloc(sizeof(struct Workspace_Assignment));
            assignment->name = ws_name;
            assignment->output = (yyvsp[(4) - (5)].string);
            TAILQ_INSERT_TAIL(&ws_assignments, assignment, ws_assignments);
        }
    ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 1388 "src/cfgparse.y"
    {
        int ws_num = (yyvsp[(2) - (3)].number);
        if (ws_num < 1) {
            DLOG("Invalid workspace assignment, workspace number %d out of range\n", ws_num);
        } else {
            DLOG("workspace name to: %s\n", (yyvsp[(3) - (3)].string));
#if 0
            if ((yyvsp[(3) - (3)].string) != NULL) {
                    workspace_set_name(workspace_get(ws_num - 1), (yyvsp[(3) - (3)].string));
                    free((yyvsp[(3) - (3)].string));
            }
#endif
        }
    ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 1405 "src/cfgparse.y"
    { (yyval.string) = NULL; ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 1406 "src/cfgparse.y"
    { (yyval.string) = (yyvsp[(1) - (1)].string); ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 1410 "src/cfgparse.y"
    { (yyval.string) = (yyvsp[(1) - (1)].string); ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 1411 "src/cfgparse.y"
    { (yyval.string) = (yyvsp[(1) - (1)].string); ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 1412 "src/cfgparse.y"
    { (yyval.string) = (yyvsp[(1) - (1)].string); ;}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 1417 "src/cfgparse.y"
    {
        /* This is the old, deprecated form of assignments. It’s provided for
         * compatibility in version (4.1, 4.2, 4.3) and will be removed
         * afterwards. It triggers an i3-nagbar warning starting from 4.1. */
        ELOG("You are using the old assign syntax (without criteria). "
             "Please see the User's Guide for the new syntax and fix "
             "your config file.\n");
        context->has_warnings = true;
        printf("assignment of %s to *%s*\n", (yyvsp[(2) - (3)].string), (yyvsp[(3) - (3)].string));
        char *workspace = (yyvsp[(3) - (3)].string);
        char *criteria = (yyvsp[(2) - (3)].string);

        Assignment *assignment = scalloc(sizeof(Assignment));
        Match *match = &(assignment->match);
        match_init(match);

        char *separator = NULL;
        if ((separator = strchr(criteria, '/')) != NULL) {
            *(separator++) = '\0';
            char *pattern;
            sasprintf(&pattern, "(?i)%s", separator);
            match->title = regex_new(pattern);
            free(pattern);
            printf("  title = %s\n", separator);
        }
        if (*criteria != '\0') {
            char *pattern;
            sasprintf(&pattern, "(?i)%s", criteria);
            match->class = regex_new(pattern);
            free(pattern);
            printf("  class = %s\n", criteria);
        }
        free(criteria);

        /* Compatibility with older versions: If the assignment target starts
         * with ~, we create the equivalent of:
         *
         * for_window [class="foo"] floating enable
         */
        if (*workspace == '~') {
            workspace++;
            if (*workspace == '\0') {
                /* This assignment was *only* for floating */
                assignment->type = A_COMMAND;
                assignment->dest.command = sstrdup("floating enable");
                TAILQ_INSERT_TAIL(&assignments, assignment, assignments);
                break;
            } else {
                /* Create a new assignment and continue afterwards */
                Assignment *floating = scalloc(sizeof(Assignment));
                match_copy(&(floating->match), match);
                floating->type = A_COMMAND;
                floating->dest.command = sstrdup("floating enable");
                TAILQ_INSERT_TAIL(&assignments, floating, assignments);
            }
        }

        assignment->type = A_TO_WORKSPACE;
        assignment->dest.workspace = workspace;
        TAILQ_INSERT_TAIL(&assignments, assignment, assignments);
    ;}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 1479 "src/cfgparse.y"
    {
        if (match_is_empty(&current_match)) {
            ELOG("Match is empty, ignoring this assignment\n");
            break;
        }
        printf("new assignment, using above criteria, to workspace %s\n", (yyvsp[(3) - (3)].string));
        Assignment *assignment = scalloc(sizeof(Assignment));
        assignment->match = current_match;
        assignment->type = A_TO_WORKSPACE;
        assignment->dest.workspace = (yyvsp[(3) - (3)].string);
        TAILQ_INSERT_TAIL(&assignments, assignment, assignments);
    ;}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 1500 "src/cfgparse.y"
    {
        config.ipc_socket_path = (yyvsp[(2) - (2)].string);
    ;}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 1507 "src/cfgparse.y"
    {
        config.restart_state_path = (yyvsp[(2) - (2)].string);
    ;}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 1514 "src/cfgparse.y"
    {
        struct Autostart *new = smalloc(sizeof(struct Autostart));
        new->command = (yyvsp[(3) - (3)].string);
        new->no_startup_id = (yyvsp[(2) - (3)].number);
        TAILQ_INSERT_TAIL(&autostarts, new, autostarts);
    ;}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 1524 "src/cfgparse.y"
    {
        struct Autostart *new = smalloc(sizeof(struct Autostart));
        new->command = (yyvsp[(3) - (3)].string);
        new->no_startup_id = (yyvsp[(2) - (3)].number);
        TAILQ_INSERT_TAIL(&autostarts_always, new, autostarts_always);
    ;}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 1533 "src/cfgparse.y"
    { (yyval.number) = false; ;}
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 1534 "src/cfgparse.y"
    { (yyval.number) = true; ;}
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 1539 "src/cfgparse.y"
    {
        ELOG("The terminal option is DEPRECATED and has no effect. "
            "Please remove it from your configuration file.\n");
    ;}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 1547 "src/cfgparse.y"
    {
        config.font = load_font((yyvsp[(2) - (2)].string), true);
        printf("font %s\n", (yyvsp[(2) - (2)].string));
        FREE(font_pattern);
        font_pattern = (yyvsp[(2) - (2)].string);
    ;}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 1557 "src/cfgparse.y"
    {
        uint32_t *dest = (yyvsp[(1) - (2)].single_color);
        *dest = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 1565 "src/cfgparse.y"
    {
        struct Colortriple *dest = (yyvsp[(1) - (4)].color);

        dest->border = (yyvsp[(2) - (4)].number);
        dest->background = (yyvsp[(3) - (4)].number);
        dest->text = (yyvsp[(4) - (4)].number);
    ;}
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 1576 "src/cfgparse.y"
    {
        (yyval.number) = get_colorpixel((yyvsp[(1) - (1)].string));
        free((yyvsp[(1) - (1)].string));
    ;}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 1584 "src/cfgparse.y"
    { (yyval.number) = 0; ;}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 1586 "src/cfgparse.y"
    { (yyval.number) = (yyvsp[(1) - (3)].number) | (yyvsp[(3) - (3)].number); ;}
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 1587 "src/cfgparse.y"
    { (yyval.number) = (yyvsp[(1) - (2)].number); ;}
    break;

  case 149:

/* Line 1455 of yacc.c  */
#line 1591 "src/cfgparse.y"
    { (yyval.number) = (yyvsp[(1) - (1)].number); ;}
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 1592 "src/cfgparse.y"
    { (yyval.number) = BIND_CONTROL; ;}
    break;

  case 151:

/* Line 1455 of yacc.c  */
#line 1593 "src/cfgparse.y"
    { (yyval.number) = BIND_SHIFT; ;}
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 1598 "src/cfgparse.y"
    {
        DLOG("popup_during_fullscreen setting: %d\n", (yyvsp[(2) - (2)].number));
        config.popup_during_fullscreen = (yyvsp[(2) - (2)].number);
    ;}
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 1605 "src/cfgparse.y"
    { (yyval.number) = PDF_IGNORE; ;}
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 1606 "src/cfgparse.y"
    { (yyval.number) = PDF_LEAVE_FULLSCREEN; ;}
    break;



/* Line 1455 of yacc.c  */
#line 3367 "src/cfgparse.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



