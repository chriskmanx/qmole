/*
 * Copyright (C) 1997-2004, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

static const char cvs_ident[] = "$Id: perf.c,v 1.13 2004/10/26 18:01:54 mej Exp $";

#if defined(HAVE_CONFIG_H) && (HAVE_CONFIG_H != 0)
# include <config.h>
#endif

#include <libast.h>
#include "perf.h"

unsigned long tnum = 0;
struct timeval time1, time2;
double time_diff;
size_t prof_counter;
size_t rep_cnt = 0, rep_mult = 100;

int perf_macros(void);
int perf_strings(void);
int perf_hashes(void);
int perf_options(void);
int perf_obj(void);
int perf_str(void);
int perf_tok(void);
int perf_url(void);
int perf_list(void);

int
perf_macros(void)
{
    char memset_test[] = "abcdefghijklmnopqrstuvwxyz";
    char sc1 = 'X', sc2 = 'K';
    int si1 = 472, si2 = 8786345;
    unsigned long sl1 = 0x98765432, sl2 = 0xffeeddff;

    PERF_SET_REPS(10000);

    PERF_BEGIN("MEMSET() macro");
    PERF_TEST(MEMSET(memset_test, '!', CONST_STRLEN(memset_test)););
    PERF_END();

    PERF_BEGIN("libc memset() function");
    PERF_TEST(memset(memset_test, '!', CONST_STRLEN(memset_test)););
    PERF_END();

    PERF_BEGIN("SWAP() macro");
    PERF_TEST(SWAP(sc1, sc2););
    PERF_TEST(SWAP(si1, si2););
    PERF_TEST(SWAP(sl1, sl2););
    PERF_END();

    PERF_BEGIN("BINSWAP() macro");
    PERF_TEST(BINSWAP(sc1, sc2););
    PERF_TEST(BINSWAP(si1, si2););
    PERF_TEST(BINSWAP(sl1, sl2););
    PERF_END();

    PERF_BEGIN("BEG_STRCASECMP() macro");
    PERF_TEST(BEG_STRCASECMP("this", "this is a test"););
    PERF_TEST(BEG_STRCASECMP("thx", "this is another test"););
    PERF_TEST(BEG_STRCASECMP("this is still another test", "this is"););
    PERF_END();

    PERF_ENDED("macro");
}

int
perf_strings(void)
{
    char *s1, *s2, *s3, *s4;
#if HAVE_REGEX_H
    regex_t *r = NULL;
#endif
    char **slist;

    PERF_SET_REPS(1000);

    PERF_BEGIN("spiftool_substr() function");
    PERF_TEST(s1 = spiftool_substr("pneumonoultramicroscopicsilicovolcanoconiosis", 8, 16); FREE(s1););
    PERF_TEST(s2 = spiftool_substr("abc", 7, 5); FREE(s2););
    PERF_TEST(s3 = spiftool_substr(NULL, 0, 0); FREE(s3););
    PERF_TEST(s4 = spiftool_substr("what the heck", -5, 42); FREE(s4););
    PERF_END();

    PERF_SET_REPS(10);

#if HAVE_REGEX_H
    PERF_BEGIN("spiftool_regexp_match() function");
    PERF_TEST(spiftool_regexp_match("One particular string", "part"););
    PERF_TEST(spiftool_regexp_match("Some other strange string", "^[A-Za-z]+$"););
    PERF_TEST(spiftool_regexp_match("some-rpm-package-1.0.1-4.src.rpm", "^(.*)-([^-]+)-([^-])\\.([a-z0-9]+)\\.rpm$"););
    PERF_TEST(spiftool_regexp_match("/the/path/to/some/odd/file.txt", "/this/should/not/match"););
    PERF_TEST(spiftool_regexp_match("1600x1200", "[[:digit:]]+x[[:digit:]]+"););
    PERF_TEST(spiftool_regexp_match("xxx", NULL););
    PERF_TEST(spiftool_regexp_match_r("AbCdEfGhIjKlMnOpQrStUvWxYz", "[[:upper:]]", &r););
    PERF_TEST(spiftool_regexp_match_r("abcdefjhijklmnopqrstuvwxyz", NULL, &r););
    PERF_TEST(spiftool_regexp_match_r("aaaaa", "[[:lower:]]", &r););
    PERF_END();
#endif

    PERF_BEGIN("split() function");
    PERF_TEST(slist = spiftool_split(" ", "Splitting a string on spaces"); spiftool_free_array(slist, -1););
    PERF_TEST(slist = spiftool_split(NULL, "          a\t \ta        a a a a       a     "); spiftool_free_array(slist, -1););
    PERF_TEST(slist = spiftool_split(NULL, "  first \"just the second\" third \'fourth and \'\"fifth to\"gether last"); spiftool_free_array(slist, -1););
    PERF_TEST(slist = spiftool_split(NULL, "\'don\\\'t\' try this    at home \"\" "); spiftool_free_array(slist, -1););
    PERF_TEST(slist = spiftool_split(":", "A:B:C:D:::E"); spiftool_free_array(slist, -1););
    PERF_END();

    PERF_ENDED("string");
    return 0;
}

int
perf_hashes(void)
{
    spif_uint8_t i;

    for (i = 0; i < 6; i++) {
        spifhash_func_t hash_func;
        spif_uint32_t key_length, hash, j;

        if (i == 0) {
            PERF_NOTICE("*** Profiling Jenkins hash:");
            hash_func = spifhash_jenkins;
        } else if (i == 1) {
            PERF_NOTICE("*** Profiling Jenkins32 hash:");
            hash_func = spifhash_jenkins32;
        } else if (i == 2) {
#if WORDS_BIGENDIAN
            continue;
#else
            PERF_NOTICE("*** Profiling JenkinsLE hash:");
            hash_func = spifhash_jenkinsLE;
#endif
        } else if (i == 3) {
            PERF_NOTICE("*** Profiling rotating hash:");
            hash_func = spifhash_rotating;
        } else if (i == 4) {
            PERF_NOTICE("*** Profiling one-at-a-time hash:");
            hash_func = spifhash_one_at_a_time;
        } else if (i == 5) {
            PERF_NOTICE("*** Profiling FNV hash:");
            hash_func = spifhash_fnv;
        }

        /***
         *** The below tests hash speed for small, medium, and
         *** large randomly-generated keys.
         ***/
        PERF_SET_REPS(1000);

        /*
         * Raw hash speed for fixed-length randomly-generated key.
         */
#define KEYLEN 32
        do {
            spif_uint8_t buff[KEYLEN];

            for (key_length = 0; key_length < KEYLEN; key_length++) {
                buff[key_length] = SPIF_CAST(uint8) (rand() & 0xff);
            }
            PERF_BEGIN("hash speed for 32-byte key");
            if (hash_func == spifhash_jenkins32) {
                PERF_TEST(hash = hash_func(buff, KEYLEN / 4, hash););
            } else {
                PERF_TEST(hash = hash_func(buff, KEYLEN, hash););
            }
            PERF_END();
        } while (0);
#undef KEYLEN

#define KEYLEN 512
        do {
            spif_uint8_t buff[KEYLEN];

            for (key_length = 0; key_length < KEYLEN; key_length++) {
                buff[key_length] = SPIF_CAST(uint8) (rand() & 0xff);
            }
            PERF_BEGIN("hash speed for 512-byte key");
            if (hash_func == spifhash_jenkins32) {
                PERF_TEST(hash = hash_func(buff, KEYLEN / 4, hash););
            } else {
                PERF_TEST(hash = hash_func(buff, KEYLEN, hash););
            }
            PERF_END();
        } while (0);
#undef KEYLEN

#define KEYLEN 8192
        do {
            spif_uint8_t buff[KEYLEN];

            for (key_length = 0; key_length < KEYLEN; key_length++) {
                buff[key_length] = SPIF_CAST(uint8) (rand() & 0xff);
            }
            PERF_BEGIN("hash speed for 8192-byte key");
            if (hash_func == spifhash_jenkins32) {
                PERF_TEST(hash = hash_func(buff, KEYLEN / 4, hash););
            } else {
                PERF_TEST(hash = hash_func(buff, KEYLEN, hash););
            }
            PERF_END();
        } while (0);
#undef KEYLEN

        /***
         *** These tests measure collision rate and distribution
         *** across a varying number of hash buckets for varying
         *** numbers of randomly-generated alphabetical keys.
         ***/

#define KEYLEN      8
#define HASH_BITS   16
        /*
         * Hash distribution for a 16-bit hash and an 8-character random word.
         */
#define KEYCNT      64
        do {
            spif_uint32_t buckets[SPIFHASH_SIZE(HASH_BITS)];
            spif_uint32_t c_total, c_min = SPIF_CAST(uint32) (-1), c_max = 0;
            double c_prob, avg_size;

            MEMSET(buckets, 0, sizeof(buckets));
            printf("Profiling %lu-bucket distribution for %lu-bit hashes of %lu %lu-byte random words...",
                   SPIF_CAST_C(unsigned long) SPIFHASH_SIZE(HASH_BITS),
                   SPIF_CAST_C(unsigned long) HASH_BITS,
                   SPIF_CAST_C(unsigned long) KEYCNT,
                   SPIF_CAST_C(unsigned long) KEYLEN);
            for (j = 0; j < KEYCNT; j++) {
                spif_uint8_t buff[KEYLEN];

                for (key_length = 0; key_length < KEYLEN; key_length++) {
                    buff[key_length] = SPIF_CAST(uint8) (rand() % 26 + 'a');
                }
                if (hash_func == spifhash_jenkins32) {
                    hash = hash_func(buff, KEYLEN / 4, hash);
                } else {
                    hash = hash_func(buff, KEYLEN, hash);
                }
                buckets[((hash >> HASH_BITS) ^ (hash & 0xffff))]++;
            }
            tnum = j;
            for (j = 0, c_total = 0, avg_size = 0; j < SPIFHASH_SIZE(HASH_BITS); j++) {
                spif_uint32_t c;

                c = buckets[j];
                AT_LEAST(c_max, c);
                AT_MOST(c_min, c);

                if (c) {
                    if (--c) {
                        c_total += c;
                    }
                }
            }
            avg_size /= SPIFHASH_SIZE(HASH_BITS);
            c_prob = ((SPIF_CAST_C(double) c_total) / KEYCNT * 100.0);
            printf("%lu collisions (%lu/%lu/%.5le), %.2lf%% probability.\n",
                   SPIF_CAST_C(unsigned long) c_total,
                   SPIF_CAST_C(unsigned long) c_min,
                   SPIF_CAST_C(unsigned long) c_max,
                   avg_size, c_prob);
        } while (0);
#undef KEYCNT

#define KEYCNT      2048
        do {
            spif_uint32_t buckets[SPIFHASH_SIZE(HASH_BITS)];
            spif_uint32_t c_total, c_min = SPIF_CAST(uint32) (-1), c_max = 0;
            double c_prob, avg_size;

            MEMSET(buckets, 0, sizeof(buckets));
            printf("Profiling %lu-bucket distribution for %lu-bit hashes of %lu %lu-byte random words...",
                   SPIF_CAST_C(unsigned long) SPIFHASH_SIZE(HASH_BITS),
                   SPIF_CAST_C(unsigned long) HASH_BITS,
                   SPIF_CAST_C(unsigned long) KEYCNT,
                   SPIF_CAST_C(unsigned long) KEYLEN);
            for (j = 0; j < KEYCNT; j++) {
                spif_uint8_t buff[KEYLEN];

                for (key_length = 0; key_length < KEYLEN; key_length++) {
                    buff[key_length] = SPIF_CAST(uint8) (rand() % 26 + 'a');
                }
                if (hash_func == spifhash_jenkins32) {
                    hash = hash_func(buff, KEYLEN / 4, hash);
                } else {
                    hash = hash_func(buff, KEYLEN, hash);
                }
                buckets[((hash >> HASH_BITS) ^ (hash & 0xffff))]++;
            }
            tnum = j;
            for (j = 0, c_total = 0, avg_size = 0; j < SPIFHASH_SIZE(HASH_BITS); j++) {
                spif_uint32_t c;

                c = buckets[j];
                AT_LEAST(c_max, c);
                AT_MOST(c_min, c);

                if (c) {
                    if (--c) {
                        c_total += c;
                    }
                }
            }
            avg_size /= SPIFHASH_SIZE(HASH_BITS);
            c_prob = ((SPIF_CAST_C(double) c_total) / KEYCNT * 100.0);
            printf("%lu collisions (%lu/%lu/%.5le), %.2lf%% probability.\n",
                   SPIF_CAST_C(unsigned long) c_total,
                   SPIF_CAST_C(unsigned long) c_min,
                   SPIF_CAST_C(unsigned long) c_max,
                   avg_size, c_prob);
        } while (0);
#undef KEYCNT

#define KEYCNT      32768
        do {
            spif_uint32_t buckets[SPIFHASH_SIZE(HASH_BITS)];
            spif_uint32_t c_total, c_min = SPIF_CAST(uint32) (-1), c_max = 0;
            double c_prob, avg_size;

            MEMSET(buckets, 0, sizeof(buckets));
            printf("Profiling %lu-bucket distribution for %lu-bit hashes of %lu %lu-byte random words...",
                   SPIF_CAST_C(unsigned long) SPIFHASH_SIZE(HASH_BITS),
                   SPIF_CAST_C(unsigned long) HASH_BITS,
                   SPIF_CAST_C(unsigned long) KEYCNT,
                   SPIF_CAST_C(unsigned long) KEYLEN);
            for (j = 0; j < KEYCNT; j++) {
                spif_uint8_t buff[KEYLEN];

                for (key_length = 0; key_length < KEYLEN; key_length++) {
                    buff[key_length] = SPIF_CAST(uint8) (rand() % 26 + 'a');
                }
                if (hash_func == spifhash_jenkins32) {
                    hash = hash_func(buff, KEYLEN / 4, hash);
                } else {
                    hash = hash_func(buff, KEYLEN, hash);
                }
                buckets[((hash >> HASH_BITS) ^ (hash & 0xffff))]++;
            }
            tnum = j;
            for (j = 0, c_total = 0, avg_size = 0; j < SPIFHASH_SIZE(HASH_BITS); j++) {
                spif_uint32_t c;

                c = buckets[j];
                AT_LEAST(c_max, c);
                AT_MOST(c_min, c);

                if (c) {
                    if (--c) {
                        c_total += c;
                    }
                }
            }
            avg_size /= SPIFHASH_SIZE(HASH_BITS);
            c_prob = ((SPIF_CAST_C(double) c_total) / KEYCNT * 100.0);
            printf("%lu collisions (%lu/%lu/%.5le), %.2lf%% probability.\n",
                   SPIF_CAST_C(unsigned long) c_total,
                   SPIF_CAST_C(unsigned long) c_min,
                   SPIF_CAST_C(unsigned long) c_max,
                   avg_size, c_prob);
        } while (0);
#undef KEYCNT

#undef HASH_BITS

#undef KEYLEN
    }
    PERF_ENDED("hash function");
    return 0;
}

int
perf_options(void)
{
    spif_uint32_t test_flag_var = 0;
    char *file_var = NULL, **exec_list = NULL;
    long num_var = 0, geom_var = 0;
    char *argv1[] = { "test", "-abvf", "somefile", "-n1", "-g", "3", "-e", "help", "me", "rhonda", NULL };
    int argc1 = 10;
    spifopt_t opts1[] = {
        SPIFOPT_BOOL('a', "agony", "scream in agony", test_flag_var, 0x01),
        SPIFOPT_BOOL('b', "bogus", "mark as bogus", test_flag_var, 0x02),
        SPIFOPT_BOOL('c', "crap", "spew some random crap", test_flag_var, 0x04),
        SPIFOPT_BOOL_LONG("dillhole", "are you a dillhole?", test_flag_var, 0x08),
        SPIFOPT_ARGS('e', "exec", "what to exec", exec_list),
        SPIFOPT_STR('f', "file", "set the filename", file_var),
        SPIFOPT_INT('g', "geom", "geometry", geom_var),
        SPIFOPT_INT('n', "num", "number", num_var),
        SPIFOPT_BOOL_PP('v', "verbose", "be verbose", test_flag_var, 0x10)
    };
    char *display = NULL, *name = NULL, *theme = NULL, **exec = NULL, **foo = NULL;
    long color = 0;
    spif_uint32_t options = 0;
    static void handle_theme(char *val_ptr) {theme = STRDUP(val_ptr);}
    char *argv2[] = { "test", "-rt", "mytheme", "--name", "This is a name", "--exec=ssh foo@bar.com", "--scrollbar",
                      "--buttonbar", "no", "--login=0", "-mvd", "foo:0", "--color", "4", "--foo", "blah", "-d", "eatme", NULL };
    int argc2 = 18;
    spifopt_t opts2[] = {
        SPIFOPT_STR_PP('d', "display", "X display to connect to", display),
        SPIFOPT_ARGS_PP('e', "exec", "command to run", exec),
        SPIFOPT_BOOL_PP('l', "login", "login shell", options, 0x01),
        SPIFOPT_BOOL('m', "map-alert", "raise window on beep", options, 0x02),
        SPIFOPT_STR('n', "name", "name", name),
        SPIFOPT_BOOL('r', "reverse-video", "enable reverse video", options, 0x04),
        SPIFOPT_ABST_PP('t', "theme", "theme to use", handle_theme),
        SPIFOPT_BOOL_PP('v', "visual-bell", "enable visual bell", options, 0x08),
        SPIFOPT_BOOL_LONG("scrollbar", "enable scrollbar", options, 0x10),
        SPIFOPT_BOOL_LONG("buttonbar", "enable buttonbar", options, 0x20),
        SPIFOPT_INT_LONG("color", "pick a color", color),
        SPIFOPT_ARGS_LONG("foo", "foo", foo)
    };

    PERF_SET_REPS(100);

    PERF_BEGIN("spifopt_parse() function");
    SPIFOPT_OPTLIST_SET(opts1);
    SPIFOPT_NUMOPTS_SET(sizeof(opts1) / sizeof(spifopt_t));
    SPIFOPT_ALLOWBAD_SET(0);
    PERF_TEST(
              SPIFOPT_FLAGS_SET(SPIFOPT_SETTING_PREPARSE);
              spifopt_parse(argc1, argv1);
              spifopt_parse(argc1, argv1);
              FREE(file_var);
              spiftool_free_array(exec_list, -1);
              );

    SPIFOPT_OPTLIST_SET(opts2);
    SPIFOPT_NUMOPTS_SET(sizeof(opts2) / sizeof(spifopt_t));
    SPIFOPT_ALLOWBAD_SET(0);
    PERF_TEST(
              SPIFOPT_FLAGS_SET(SPIFOPT_SETTING_PREPARSE);
              spifopt_parse(argc2, argv2);
              spifopt_parse(argc2, argv2);
              FREE(display);
              FREE(name);
              FREE(theme);
              spiftool_free_array(exec, -1);
              spiftool_free_array(foo, -1);
              );

    PERF_END();

    PERF_ENDED("options");
    return 0;
}

int
perf_obj(void)
{
    spif_obj_t testobj;
    spif_class_t cls;

    PERF_SET_REPS(100);

    PERF_BEGIN("spif_obj create/delete");
    PERF_TEST(testobj = spif_obj_new(); spif_obj_del(testobj););
    PERF_END();

    testobj = spif_obj_new();
    PERF_BEGIN("spif_obj_get_classname");
    PERF_TEST(cls = spif_obj_get_class(testobj););
    PERF_END();
    spif_obj_del(testobj);

    PERF_ENDED("spif_obj_t");
    return 0;
}

int
perf_str(void)
{
    spif_str_t teststr, test2str;
    signed char tmp[] = "this is a test";
    signed char buff[4096] = "abcde";
    signed char tmp2[] = "string #1\nstring #2";
    spif_charptr_t foo;

    PERF_SET_REPS(100);

    PERF_BEGIN("spif_str create/delete");
    PERF_TEST(teststr = spif_str_new(); spif_str_del(teststr););
    PERF_END();

    PERF_BEGIN("spif_str_new_from_ptr() function");
    PERF_TEST(teststr = spif_str_new_from_ptr(tmp); spif_str_del(teststr););
    PERF_END();

    PERF_BEGIN("spif_str_new_from_buff() function");
    PERF_TEST(teststr = spif_str_new_from_buff(buff, sizeof(buff)); spif_str_del(teststr););
    PERF_END();

    PERF_BEGIN("spif_str_dup() function");
    PERF_TEST(
              teststr = spif_str_new_from_ptr(tmp);
              test2str = spif_str_dup(teststr);
              spif_str_del(teststr);
              spif_str_del(test2str);
              );
    PERF_END();

    teststr = spif_str_new_from_ptr(tmp2);

    PERF_BEGIN("spif_str_index() function");
    PERF_TEST(spif_str_index(teststr, '#'););
    PERF_END();

    PERF_BEGIN("spif_str_rindex() function");
    PERF_TEST(spif_str_rindex(teststr, '#'););
    PERF_END();

    test2str = spif_str_new_from_ptr(SPIF_CAST(charptr) "ring");
    PERF_BEGIN("spif_str_find() function");
    PERF_TEST(spif_str_find(teststr, test2str););
    PERF_END();
    spif_str_del(test2str);

    PERF_BEGIN("spif_str_find_from_ptr() function");
    PERF_TEST(spif_str_find_from_ptr(teststr, SPIF_CAST(charptr) "in"););
    PERF_END();

    spif_str_del(teststr);

    teststr = spif_str_new_from_ptr(tmp);
    PERF_BEGIN("spif_str_substr() function");
    PERF_TEST(test2str = spif_str_substr(teststr, 2, 5); spif_str_del(test2str););
    PERF_TEST(test2str = spif_str_substr(teststr, -4, 4); spif_str_del(test2str););
    PERF_END();
    spif_str_del(teststr);

    teststr = spif_str_new_from_ptr(tmp);
    PERF_BEGIN("spif_str_substr_to_ptr() function");
    PERF_TEST(foo = spif_str_substr_to_ptr(teststr, 2, 5); FREE(foo););
    PERF_TEST(foo = spif_str_substr_to_ptr(teststr, -4, 4); FREE(foo););
    PERF_END();
    spif_str_del(teststr);

    teststr = spif_str_new_from_ptr(SPIF_CAST(charptr) "11001001");
    PERF_BEGIN("spif_str_to_num() function");
    PERF_TEST(spif_str_to_num(teststr, 10););
    PERF_END();
    spif_str_del(teststr);

    teststr = spif_str_new_from_ptr(SPIF_CAST(charptr) "3.1415");
    PERF_BEGIN("spif_str_to_float() function");
    PERF_TEST(spif_str_to_float(teststr););
    PERF_END();
    spif_str_del(teststr);

    teststr = spif_str_new_from_ptr("copy");
    test2str = spif_str_new_from_ptr("cat");
    PERF_BEGIN("spif_str_append() function");
    PERF_TEST(spif_str_append(teststr, test2str););
    PERF_END();
    spif_str_del(test2str);
    spif_str_del(teststr);

    test2str = spif_str_new_from_ptr("Hello");
    PERF_BEGIN("spif_str_append_char() function");
    PERF_TEST(spif_str_append_char(test2str, '!'););
    PERF_END();
    spif_str_del(test2str);

    teststr = spif_str_new_from_ptr("copy");
    PERF_BEGIN("spif_str_append_from_ptr() function");
    PERF_TEST(spif_str_append_from_ptr(teststr, "crime"););
    PERF_END();
    spif_str_del(teststr);

    teststr = spif_str_new_from_ptr("abcdefg");
    PERF_BEGIN("spif_str_clear() function");
    PERF_TEST(spif_str_clear(teststr, SPIF_CAST_C(char) (rand() & 0xff)););
    PERF_END();
    spif_str_del(teststr);

    PERF_BEGIN("spif_str_trim() function");
    PERF_TEST(
              teststr = spif_str_new_from_ptr(SPIF_CAST(charptr) "  \n \r\f       \t    testing 1 2 3    \v\r \n");
              spif_str_trim(teststr);
              spif_str_del(teststr);
              );
    PERF_END();

    PERF_BEGIN("spif_str_splice() function");
    PERF_TEST(
              teststr = spif_str_new_from_ptr(tmp);
              test2str = spif_str_new_from_ptr("lots of fun");
              spif_str_splice(teststr, 8, 6, test2str);
              spif_str_del(test2str);
              spif_str_del(teststr);
              );
    PERF_END();

    PERF_BEGIN("spif_str_splice_from_ptr() function");
    PERF_TEST(
              teststr = spif_str_new_from_ptr(tmp);
              spif_str_splice_from_ptr(teststr, 8, 0, "not ");
              spif_str_del(teststr);
              );
    PERF_END();

    teststr = spif_str_new_from_buff(buff, sizeof(buff));
    PERF_BEGIN("spif_str_reverse() function");
    PERF_TEST(spif_str_reverse(teststr););
    PERF_END();
    spif_str_del(teststr);

    PERF_ENDED("spif_str_t");
    return 0;
}

int
perf_tok(void)
{
    spif_tok_t testtok;
    spif_str_t teststr;
    signed char tmp[] = "I \"can\'t\" feel my legs!";
    signed char tmp2[] = ":::some:seedy:colon-delimited::data";
    signed char tmp3[] = "\"this is one token\" and this \'over here\' is \"another one\"";
    signed char tmp4[] = "\"there shouldn't be\"\' any problems at\'\"\"\'\'\' \'\"all parsing this\"";

    PERF_SET_REPS(10);

    PERF_BEGIN("spif_tok_new_from_ptr() function");
    PERF_TEST(testtok = spif_tok_new_from_ptr(tmp); spif_tok_del(testtok););
    PERF_END();

    PERF_BEGIN("spif_tok_eval() function");

    testtok = spif_tok_new_from_ptr(tmp);
    PERF_TEST(spif_tok_eval(testtok););
    spif_tok_del(testtok);

    testtok = spif_tok_new_from_ptr(tmp2);
    teststr = spif_str_new_from_ptr(":");
    spif_tok_set_sep(testtok, teststr);
    spif_str_del(teststr);
    PERF_TEST(spif_tok_eval(testtok););
    spif_tok_del(testtok);

    testtok = spif_tok_new_from_ptr(tmp3);
    PERF_TEST(spif_tok_eval(testtok););
    spif_tok_del(testtok);

    testtok = spif_tok_new_from_ptr(tmp4);
    PERF_TEST(spif_tok_eval(testtok););
    spif_tok_del(testtok);

    PERF_END();

    PERF_ENDED("spif_tok_t");
    return 0;
}

int
perf_url(void)
{
    spif_url_t testurl, testurl2, testurl3, testurl4;
    spif_charptr_t tmp1 = "http://www.kainx.org/journal/?view=20020104";
    spif_charptr_t tmp2 = "mailto:foo@bar.com?Subject=Eat Me";
    spif_charptr_t tmp3 = "/path/to/some/file.jpg";
    spif_charptr_t tmp4 = "pop3://dummy:moo@pop.nowhere.com:110";

    PERF_SET_REPS(100);

    PERF_BEGIN("spif_url_new_from_ptr() function");
    PERF_TEST(testurl = spif_url_new_from_ptr(tmp1); spif_url_del(testurl););
    PERF_TEST(testurl2 = spif_url_new_from_ptr(tmp2); spif_url_del(testurl2););
    PERF_TEST(testurl3 = spif_url_new_from_ptr(tmp3); spif_url_del(testurl3););
    PERF_TEST(testurl4 = spif_url_new_from_ptr(tmp4); spif_url_del(testurl4););
    PERF_END();

    PERF_ENDED("spif_url_t");
    return 0;
}

int
perf_list(void)
{
    char buff[2] = "\0";
    unsigned short i;
    spif_list_t testlist;
    spif_str_t s, s2;
    size_t idx, len;

    PERF_SET_REPS(1);

    for (i = 0; i < 3; i++) {
        if (i == 0) {
            PERF_NOTICE("*** Profiling list interface class, linked_list instance:");
            testlist = SPIF_LIST_NEW(linked_list);
        } else if (i == 1) {
            PERF_NOTICE("*** Profiling list interface class, dlinked_list instance:");
            testlist = SPIF_LIST_NEW(dlinked_list);
        } else if (i == 2) {
            PERF_NOTICE("*** Profiling list interface class, array instance:");
            testlist = SPIF_LIST_NEW(array);
        } else if (i == 3) {
        }

        PERF_BEGIN("SPIF_LIST_APPEND() macro");
        buff[0] = SPIF_CAST_C(char) (rand() & 0xff);
        PERF_TEST(SPIF_LIST_APPEND(testlist, spif_str_new_from_ptr(buff)););
        PERF_END();

        PERF_BEGIN("SPIF_LIST_PREPEND() macro");
        buff[0] = SPIF_CAST_C(char) (rand() & 0xff);
        PERF_TEST(SPIF_LIST_PREPEND(testlist, spif_str_new_from_ptr(buff)););
        PERF_END();

        s = spif_str_new();
        PERF_BEGIN("SPIF_LIST_CONTAINS() macro");
        buff[0] = SPIF_CAST_C(char) (rand() & 0xff);
        spif_str_init_from_ptr(s, buff);
        PERF_TEST(SPIF_LIST_CONTAINS(testlist, s));
        spif_str_done(s);
        PERF_END();
        spif_str_del(s);

        PERF_BEGIN("SPIF_LIST_COUNT() macro");
        PERF_TEST(SPIF_LIST_COUNT(testlist));
        PERF_END();

        len = SPIF_LIST_COUNT(testlist);
        PERF_BEGIN("SPIF_LIST_GET() macro");
        idx = SPIF_CAST_C(size_t) (rand() % len);
        PERF_TEST(SPIF_LIST_GET(testlist, idx));
        PERF_END();

        s = spif_str_new();
        PERF_BEGIN("SPIF_LIST_INDEX() macro");
        buff[0] = SPIF_CAST_C(char) (rand() & 0xff);
        spif_str_init_from_ptr(s, buff);
        PERF_TEST(SPIF_LIST_INDEX(testlist, s));
        spif_str_done(s);
        PERF_END();
        spif_str_del(s);

        PERF_BEGIN("SPIF_LIST_INSERT() macro");
        buff[0] = SPIF_CAST_C(char) (rand() & 0xff);
        PERF_TEST(SPIF_LIST_INSERT(testlist, spif_str_new_from_ptr(buff)););
        PERF_END();

        len = SPIF_LIST_COUNT(testlist);
        PERF_BEGIN("SPIF_LIST_INSERT_AT() macro");
        buff[0] = SPIF_CAST_C(char) (rand() & 0xff);
        idx = rand() % len;
        PERF_TEST(SPIF_LIST_INSERT_AT(testlist, spif_str_new_from_ptr(buff), idx););
        PERF_END();

        s = spif_str_new();
        PERF_BEGIN("SPIF_LIST_REMOVE() macro");
        buff[0] = SPIF_CAST_C(char) (rand() & 0xff);
        spif_str_init_from_ptr(s, buff);
        PERF_TEST(s2 = SPIF_CAST(str) SPIF_LIST_REMOVE(testlist, s); if (!SPIF_STR_ISNULL(s2)) {spif_str_del(s2);});
        spif_str_done(s);
        PERF_END();
        spif_str_del(s);

        PERF_BEGIN("SPIF_LIST_REMOVE_AT() macro");
        idx = rand() % SPIF_LIST_COUNT(testlist);
        PERF_TEST(s2 = SPIF_CAST(str) SPIF_LIST_REMOVE_AT(testlist, idx); if (!SPIF_STR_ISNULL(s2)) {spif_str_del(s2);});
        PERF_END();

        /*SPIF_SHOW(testlist, stderr);*/
        SPIF_LIST_DEL(testlist);
    }

    PERF_ENDED("list interface class");
    return 0;
}

int
main(int argc, char *argv[])
{
    int ret = 0;
    struct timeval t1, t2;
    spifopt_t options[] = {
        SPIFOPT_INT_PP('m', "multiplier", "multiplying factor for test runs (default 100)", rep_mult)
    };

    DEBUG_LEVEL = 0;
    SPIFOPT_OPTLIST_SET(options);
    SPIFOPT_NUMOPTS_SET(sizeof(options) / sizeof(spifopt_t));
    SPIFOPT_ALLOWBAD_SET(0);
    spifopt_parse(argc, argv);

    gettimeofday(&t1, NULL);

    if ((ret = perf_macros()) != 0) {
        return ret;
    }
    if ((ret = perf_strings()) != 0) {
        return ret;
    }
    if ((ret = perf_hashes()) != 0) {
        return ret;
    }
    if ((ret = perf_options()) != 0) {
        return ret;
    }
    if ((ret = perf_obj()) != 0) {
        return ret;
    }
    if ((ret = perf_str()) != 0) {
        return ret;
    }
    if ((ret = perf_tok()) != 0) {
        return ret;
    }
    if ((ret = perf_url()) != 0) {
        return ret;
    }
    if ((ret = perf_list()) != 0) {
        return ret;
    }
    /*MALLOC_DUMP();*/

    gettimeofday(&t2, NULL);

    printf("All profiling done, %6.5f seconds total.\n\n", TDIFF(t1, t2));
    return 0;
}
