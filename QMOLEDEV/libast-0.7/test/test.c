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

static const char cvs_ident[] = "$Id: test.c,v 1.47 2004/12/15 00:00:22 mej Exp $";

#if defined(HAVE_CONFIG_H) && (HAVE_CONFIG_H != 0)
# include <config.h>
#endif

#include <libast.h>
#include "test.h"

unsigned short tnum = 0;

int test_macros(void);
int test_mem(void);
int test_strings(void);
int test_hash_functions(void);
int test_snprintf(void);
int test_options(void);
int test_obj(void);
int test_str(void);
int test_tok(void);
int test_url(void);
int test_list(void);
int test_vector(void);
int test_map(void);
int test_socket(void);
int test_regexp(void);

int
test_macros(void)
{
    spif_char_t memset_test[] = "abcdefghijklmnopqrstuvwxyz";
    spif_char_t sc1 = 'X', sc2 = 'K';
    int si1 = 472, si2 = 8786345;
    unsigned long sl1 = 0x98765432, sl2 = 0xffeeddff;
    void *vp1 = &sc1, *vp2 = &sc2;

    TEST_BEGIN("MEMSET() macro");
    MEMSET(memset_test, '!', CONST_STRLEN(memset_test));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(memset_test), "!!!!!!!!!!!!!!!!!!!!!!!!!!"));
    MEMSET(memset_test + 3, '*', 14);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(memset_test), "!!!**************!!!!!!!!!"));
    MEMSET(memset_test, '&', 0 );
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(memset_test), "!!!**************!!!!!!!!!"));
    MEMSET(SPIF_NULL_TYPE(charptr), '_', CONST_STRLEN(memset_test));
    TEST_PASS();

    TEST_BEGIN("SWAP() macro");
    SWAP(sc1, sc2);
    SWAP(si1, si2);
    SWAP(sl1, sl2);
    SWAP(vp1, vp2);
    TEST_FAIL_IF(sc1 != 'K');
    TEST_FAIL_IF(sc2 != 'X');
    TEST_FAIL_IF(si1 != 8786345);
    TEST_FAIL_IF(si2 != 472);
    TEST_FAIL_IF(sl1 != 0xffeeddff);
    TEST_FAIL_IF(sl2 != 0x98765432);
    TEST_FAIL_IF(vp1 != &sc2);
    TEST_FAIL_IF(vp2 != &sc1);
    TEST_PASS();

    TEST_BEGIN("BINSWAP() macro");
    BINSWAP(sc1, sc2);
    BINSWAP(si1, si2);
    BINSWAP(sl1, sl2);
    BINSWAP(vp1, vp2);
    TEST_FAIL_IF(sc1 != 'X');
    TEST_FAIL_IF(sc2 != 'K');
    TEST_FAIL_IF(si1 != 472);
    TEST_FAIL_IF(si2 != 8786345);
    TEST_FAIL_IF(sl1 != 0x98765432);
    TEST_FAIL_IF(sl2 != 0xffeeddff);
    TEST_FAIL_IF(vp1 != &sc1);
    TEST_FAIL_IF(vp2 != &sc2);
    TEST_PASS();

    TEST_BEGIN("BEG_STRCASECMP() macro");
    TEST_FAIL_IF(!BEG_STRCASECMP("this", "this is a test"));
    TEST_FAIL_IF(!BEG_STRCASECMP("thx", "this is another test"));
    TEST_FAIL_IF(BEG_STRCASECMP("this is still another test", "this is"));
    TEST_PASS();

    TEST_PASSED("macro");
}

int
test_mem(void)
{
    spifmem_init();

    return 0;
}

int
test_strings(void)
{
    spif_charptr_t s1, s2, s3, s4;
#if HAVE_REGEX_H
    regex_t *r = NULL;
#endif
    spif_charptr_t *slist;

    TEST_BEGIN("spiftool_safe_strncpy() function");
    s1 = MALLOC(20);
    TEST_FAIL_IF(spiftool_safe_strncpy(s1, SPIF_CHARPTR("pneumonoultramicroscopicsilicovolcanoconiosis"), 20));
    TEST_FAIL_IF(strncmp(SPIF_CHARPTR_C(s1), "pneumonoultramicros", 20));
    TEST_FAIL_IF(!spiftool_safe_strncpy(s1, SPIF_CHARPTR("abc"), 20));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(s1), "abc"));
    TEST_FAIL_IF(!spiftool_safe_strncpy(s1, SPIF_CHARPTR(""), 20));
    TEST_FAIL_IF(*s1);
    TEST_FAIL_IF(!spiftool_safe_strncpy(s1, SPIF_CHARPTR("0123456789012345678"), 20));
    TEST_FAIL_IF(strncmp(SPIF_CHARPTR_C(s1), "0123456789012345678", 20));
    FREE(s1);
    TEST_PASS();

    TEST_BEGIN("spiftool_safe_strncat() function");
    s1 = MALLOC(20);
    *s1 = 0;
    TEST_FAIL_IF(spiftool_safe_strncat(s1, SPIF_CHARPTR("pneumonoultramicroscopicsilicovolcanoconiosis"), 20));
    TEST_FAIL_IF(strncmp(SPIF_CHARPTR_C(s1), "pneumonoultramicros", 20));
    TEST_FAIL_IF(!spiftool_safe_strncpy(s1, SPIF_CHARPTR("abc"), 20));
    TEST_FAIL_IF(!spiftool_safe_strncat(s1, SPIF_CHARPTR("defg"), 20));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(s1), "abcdefg"));
    TEST_FAIL_IF(!spiftool_safe_strncat(s1, SPIF_CHARPTR(""), 20));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(s1), "abcdefg"));
    TEST_FAIL_IF(!spiftool_safe_strncpy(s1, SPIF_CHARPTR("0123456789"), 20));
    TEST_FAIL_IF(!spiftool_safe_strncat(s1, SPIF_CHARPTR("012345678"), 20));
    TEST_FAIL_IF(strncmp(SPIF_CHARPTR_C(s1), "0123456789012345678", 20));
    FREE(s1);
    TEST_PASS();

    TEST_BEGIN("spiftool_substr() function");
    s1 = spiftool_substr(SPIF_CHARPTR("pneumonoultramicroscopicsilicovolcanoconiosis"), 8, 16);
    s2 = spiftool_substr(SPIF_CHARPTR("abc"), 7, 5);
    s3 = spiftool_substr(NULL, 0, 0);
    s4 = spiftool_substr(SPIF_CHARPTR("what the heck"), -5, 42);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(s1), "ultramicroscopic"));
    TEST_FAIL_IF(s2 != NULL);
    TEST_FAIL_IF(s3 != NULL);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(s4), " heck"));
    FREE(s1);
    FREE(s2);
    FREE(s3);
    FREE(s4);
    TEST_PASS();

#if HAVE_REGEX_H
    TEST_BEGIN("spiftool_regexp_match() function");
    TEST_FAIL_IF(!spiftool_regexp_match(SPIF_CHARPTR("One particular string"), SPIF_CHARPTR("part")));
    TEST_FAIL_IF(spiftool_regexp_match(SPIF_CHARPTR("Some other strange string"), SPIF_CHARPTR("^[A-Za-z]+$")));
    TEST_FAIL_IF(!spiftool_regexp_match(SPIF_CHARPTR("some-rpm-package-1.0.1-4.src.rpm"), SPIF_CHARPTR("^(.*)-([^-]+)-([^-])\\.([a-z0-9]+)\\.rpm$")));
    TEST_FAIL_IF(spiftool_regexp_match(SPIF_CHARPTR("/the/path/to/some/odd/file.txt"), SPIF_CHARPTR("/this/should/not/match")));
    TEST_FAIL_IF(!spiftool_regexp_match(SPIF_CHARPTR("1600x1200"), SPIF_CHARPTR("[[:digit:]]+x[[:digit:]]+")));
    TEST_FAIL_IF(spiftool_regexp_match(SPIF_CHARPTR("xxx"), NULL));
    spiftool_regexp_match(NULL, NULL);
    TEST_FAIL_IF(!spiftool_regexp_match_r(SPIF_CHARPTR("AbCdEfGhIjKlMnOpQrStUvWxYz"), SPIF_CHARPTR("[[:upper:]]"), &r));
    TEST_FAIL_IF(spiftool_regexp_match_r(SPIF_CHARPTR("abcdefjhijklmnopqrstuvwxyz"), NULL, &r));
    TEST_FAIL_IF(!spiftool_regexp_match_r(SPIF_CHARPTR("aaaaa"), SPIF_CHARPTR("[[:lower:]]"), &r));
    FREE(r);
    TEST_PASS();
#endif

    TEST_BEGIN("spiftool_split() function");
    slist = spiftool_split(SPIF_CHARPTR(" "), SPIF_CHARPTR("Splitting a string on spaces"));
    TEST_FAIL_IF(!slist);
    TEST_FAIL_IF(!slist[0] || !slist[1] || !slist[2] || !slist[3] || !slist[4] || slist[5]);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[0]), "Splitting"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[1]), "a"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[2]), "string"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[3]), "on"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[4]), "spaces"));
    spiftool_free_array(SPIF_CAST(ptr) slist, 5);

    slist = spiftool_split(NULL, SPIF_CHARPTR("          a\t \ta        a a a a       a     "));
    TEST_FAIL_IF(!slist);
    TEST_FAIL_IF(!slist[0] || !slist[1] || !slist[2] || !slist[3] || !slist[4] || !slist[5] || !slist[6] || slist[7]);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[0]), "a"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[1]), "a"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[2]), "a"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[3]), "a"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[4]), "a"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[5]), "a"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[6]), "a"));
    spiftool_free_array(SPIF_CAST(ptr) slist, 7);

    slist = spiftool_split(NULL, SPIF_CHARPTR("  first \"just the second\" third \'fourth and \'\"fifth to\"gether last"));
    TEST_FAIL_IF(!slist);
    TEST_FAIL_IF(!slist[0] || !slist[1] || !slist[2] || !slist[3] || !slist[4] || slist[5]);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[0]), "first"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[1]), "just the second"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[2]), "third"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[3]), "fourth and fifth together"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[4]), "last"));
    spiftool_free_array(SPIF_CAST(ptr) slist, 5);

    slist = spiftool_split(NULL, SPIF_CHARPTR("\'don\\\'t\' try this    at home \"\" "));
    TEST_FAIL_IF(!slist);
    TEST_FAIL_IF(!slist[0] || !slist[1] || !slist[2] || !slist[3] || !slist[4] || !slist[5] || slist[6]);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[0]), "don\'t"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[1]), "try"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[2]), "this"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[3]), "at"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[4]), "home"));
    TEST_FAIL_IF(slist[5][0]);
    spiftool_free_array(SPIF_CAST(ptr) slist, 6);

    slist = spiftool_split(SPIF_CHARPTR(":"), SPIF_CHARPTR("A:B:C:D:::E"));
    TEST_FAIL_IF(!slist);
    TEST_FAIL_IF(!slist[0] || !slist[1] || !slist[2] || !slist[3] || !slist[4] || slist[5]);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[0]), "A"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[1]), "B"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[2]), "C"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[3]), "D"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(slist[4]), "E"));
    spiftool_free_array(SPIF_CAST(ptr) slist, 5);
    TEST_PASS();

    TEST_BEGIN("spiftool_version_compare() function");
    TEST_FAIL_IF(!SPIF_CMP_IS_LESS(spiftool_version_compare(SPIF_CHARPTR("1.0"), SPIF_CHARPTR("1.0.1"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_LESS(spiftool_version_compare(SPIF_CHARPTR("2.9.99"), SPIF_CHARPTR("3.0"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_LESS(spiftool_version_compare(SPIF_CHARPTR("3.0"), SPIF_CHARPTR("29.9.9"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_LESS(spiftool_version_compare(SPIF_CHARPTR("1.0pre2"), SPIF_CHARPTR("1.0"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_LESS(spiftool_version_compare(SPIF_CHARPTR("9.9"), SPIF_CHARPTR("9.9rc1"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_GREATER(spiftool_version_compare(SPIF_CHARPTR("0.5.3"), SPIF_CHARPTR("0.5.3snap4"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_GREATER(spiftool_version_compare(SPIF_CHARPTR("2.2.4"), SPIF_CHARPTR("2.2.4beta3"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_GREATER(spiftool_version_compare(SPIF_CHARPTR("2.2.4beta3"), SPIF_CHARPTR("2.2.4alpha7"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_GREATER(spiftool_version_compare(SPIF_CHARPTR("1.27.3"), SPIF_CHARPTR("1.13.1"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_GREATER(spiftool_version_compare(SPIF_CHARPTR("0.10"), SPIF_CHARPTR("0.9.2"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_GREATER(spiftool_version_compare(SPIF_CHARPTR("2.3.2a"), SPIF_CHARPTR("2.3.2"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_GREATER(spiftool_version_compare(SPIF_CHARPTR("4.0p1"), SPIF_CHARPTR("4.0"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spiftool_version_compare(SPIF_CHARPTR("3.4.5"), SPIF_CHARPTR("3.4.5"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spiftool_version_compare(SPIF_CHARPTR("1.2.0b3"), SPIF_CHARPTR("1.2.0b3"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spiftool_version_compare(SPIF_CHARPTR("2.0alpha"), SPIF_CHARPTR("2.0alpha"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spiftool_version_compare(SPIF_CHARPTR("5.4pre1"), SPIF_CHARPTR("5.4pre1"))));
    TEST_PASS();

    TEST_PASSED("string");
    return 0;
}

#define MAXPAIR 80
#define MAXLEN 80
int
test_hash_functions(void)
{
    spif_uint8_t i;

    for (i = 0; i < 6; i++) {
        spifhash_func_t hash_func;
        spif_uint32_t key_length, input_byte, trials, successes = 0, hash, ref_hash;
        spif_uint8_t buff[MAXLEN + 20], *pbuff;
        spif_uint8_t align1[] = "This has got the amazing aroma of bovine fecal matter...";
        spif_uint8_t align2[] = "xThis has got the amazing aroma of bovine fecal matter...";
        spif_uint8_t align3[] = "xxThis has got the amazing aroma of bovine fecal matter...";
        spif_uint8_t align4[] = "xxxThis has got the amazing aroma of bovine fecal matter...";
        spif_uint32_t j;

        if (i == 0) {
            TEST_NOTICE("*** Testing Jenkins hash:");
            hash_func = spifhash_jenkins;
        } else if (i == 1) {
            TEST_NOTICE("*** Testing Jenkins32 hash:");
            hash_func = spifhash_jenkins32;
        } else if (i == 2) {
#if WORDS_BIGENDIAN
            continue;
#else
            TEST_NOTICE("*** Testing JenkinsLE hash:");
            hash_func = spifhash_jenkinsLE;
#endif
        } else if (i == 3) {
            TEST_NOTICE("*** Testing rotating hash:");
            hash_func = spifhash_rotating;
        } else if (i == 4) {
            TEST_NOTICE("*** Testing one-at-a-time hash:");
            hash_func = spifhash_one_at_a_time;
        } else if (i == 5) {
            TEST_NOTICE("*** Testing FNV hash:");
            hash_func = spifhash_fnv;
        }

        TEST_BEGIN("effect of every input bit on every output bit");
        for (key_length = 0; key_length < MAXLEN; key_length++) {
            /* For each key length up to 70 bytes... */

            if ((hash_func == spifhash_jenkins32)
                && (key_length % 4)) {
                successes++;
                continue;
            }
            trials = 0;
            for (input_byte = 0; input_byte < key_length; input_byte++) {
                /* ...for each input byte... */
                spif_uint32_t input_bit;

                for (input_bit = 0; input_bit < 8; input_bit++) {
                    /* ...for each input bit... */
                    spif_uint32_t seed;

                    for (seed = 1; seed < 8; seed++) {
                        /* ...use several possible seeds... */
                        spif_uint32_t e, f, g, h, x, y;
                        spif_uint32_t bit_pair;

                        /* Initialize to ~0 (0xffffffff). */
                        e = f = g = h = x = y = ~(SPIF_CAST(uint32) 0);

                        /* ...to make sure every output bit is affected by every input bit. */
                        for (bit_pair = 0; bit_pair < MAXPAIR; bit_pair += 2) {
                            spif_uint8_t buff1[MAXLEN + 1], buff2[MAXLEN + 2];
                            spif_uint8_t *pbuff1 = &buff1[0], *pbuff2 = &buff2[1];
                            spif_uint32_t hash1, hash2;

                            for (j = 0; j < key_length + 1; j++) {
                                /* Initialize keys to all zeros. */
                                pbuff1[j] = pbuff2[j] = (spif_uint8_t) 0;
                            }

                            /* Then make them differ by exactly one bit, the input_bit.
                               bit_pair will always end in 0, so bit_pair + 1 will always
                               end in 1.  It's then shifted by input_bit to test the
                               current bit to test all 8 of the lowest bits in sequence. */
                            pbuff1[input_byte] ^= (bit_pair << input_bit);
                            pbuff1[input_byte] ^= (bit_pair >> (8 - input_bit));
                            pbuff2[input_byte] ^= ((bit_pair + 1) << input_bit);
                            pbuff2[input_byte] ^= ((bit_pair + 1) >> (8 - input_bit));

                            /* Hash them. */
                            if (hash_func == spifhash_jenkins32) {
                                hash1 = hash_func(pbuff1, key_length / 4, seed);
                                hash2 = hash_func(pbuff2, key_length / 4, seed);
                            } else {
                                hash1 = hash_func(pbuff1, key_length, seed);
                                hash2 = hash_func(pbuff2, key_length, seed);
                            }

                            /* Make sure every bit is 1 or 0 at least once. */
                            e &= (hash1 ^ hash2);     f &= ~(hash1 ^ hash2);
                            g &= hash1;               h &= ~hash1;
                            x &= hash2;               y &= ~hash2;
                            if (!(e | f | g | h | x | y)) {
                                /* They're all 0.  That means they've all changed at least once. */
                                break;
                            }
                        }
                        if (bit_pair > trials) {
                            trials = bit_pair;
                        }
                        if (bit_pair == MAXPAIR) {
#if UNUSED_BLOCK
                            printf("Some bit didn't change: ");
                            printf("%.8lx %.8lx %.8lx %.8lx %.8lx %.8lx  ",
                                   SPIF_CAST_C(unsigned long) e,
                                   SPIF_CAST_C(unsigned long) f,
                                   SPIF_CAST_C(unsigned long) g,
                                   SPIF_CAST_C(unsigned long) h,
                                   SPIF_CAST_C(unsigned long) x,
                                   SPIF_CAST_C(unsigned long) y);
                            printf("input_byte %lu  input_bit %lu  seed %lu  key length %lu\n",
                                   SPIF_CAST_C(unsigned long) input_byte,
                                   SPIF_CAST_C(unsigned long) input_bit,
                                   SPIF_CAST_C(unsigned long) seed,
                                   SPIF_CAST_C(unsigned long) key_length);
#endif
                        }
                        if (trials == MAXPAIR) {
                            /* Easy way to break out of a crapload of for loops. */
                            goto done;
                        }
                    }
                }
            }
        done:
            if (trials < MAXPAIR) {
                successes++;
#if UNUSED_BLOCK
                printf("Mix success:  %2lu-byte key required %2lu trials (%lu so far).\n",
                       SPIF_CAST_C(unsigned long) input_byte,
                       SPIF_CAST_C(unsigned long) trials / 2,
                       SPIF_CAST_C(unsigned long) successes);
#endif
            }
        }
        printf("%.2f%% mix success rate in %d key lengths...",
               (100.0 * successes / key_length), key_length);
        TEST_FAIL_IF(successes == 0);
        TEST_PASS();

        /* Make sure nothing but the key is hashed, regardless of alignment. */
        TEST_BEGIN("endian cleanliness");
        key_length = CONST_STRLEN(align1);
        if (hash_func == spifhash_jenkins32) {
            if (key_length % 4) {
                TEST_FAIL_IF(key_length);
            } else {
                key_length /= 4;
            }
        }
        ref_hash = hash_func(align1, key_length, 0);
        hash = hash_func(align2 + 1, key_length, 0);
        /*printf("Reference hash 0x%08x, hash 0x%08x for length %lu\n", ref_hash, hash, key_length);*/
        TEST_FAIL_IF(hash != ref_hash);
        hash = hash_func(align3 + 2, key_length, 0);
        TEST_FAIL_IF(hash != ref_hash);
        hash = hash_func(align4 + 3, key_length, 0);
        TEST_FAIL_IF(hash != ref_hash);

        for (j = 0, pbuff = buff + 1; j < 8; j++, pbuff++) {
            for (key_length = 0; key_length < MAXLEN; key_length++) {
                if ((hash_func == spifhash_jenkins32)
                    && (key_length % 4)) {
                    continue;
                }
                MEMSET(buff, 0, sizeof(buff));

                if (hash_func == spifhash_jenkins32) {
                    ref_hash = hash_func(pbuff, key_length / 4, 1);
                } else {
                    ref_hash = hash_func(pbuff, key_length, 1);
                }
                *(pbuff + key_length) = ~(SPIF_CAST(uint8) 0);
                *(pbuff - 1) = ~(SPIF_CAST(uint8) 0);
                if (hash_func == spifhash_jenkins32) {
                    hash = hash_func(pbuff, key_length / 4, 1);
                } else {
                    hash = hash_func(pbuff, key_length, 1);
                }
                /*printf("Reference hash 0x%08x, hash 0x%08x for length %lu\n", ref_hash, hash, key_length);*/
                TEST_FAIL_IF(hash != ref_hash);
            }
        }
        TEST_PASS();

        /* We cannot test the rotating hash or the FNV hash here.  The
           rotating hash repeats after 4 zero-length keys.  The FNV
           hash generates constant hash values for zero-length keys. */
        if ((hash_func != spifhash_rotating)
            && (hash_func != spifhash_fnv)) {
            spif_uint32_t null_hashes[8];
            spif_uint8_t one_byte;

            TEST_BEGIN("hashes of empty strings");
            one_byte = ~0;
            for (j = 0, hash = 0; j < 8; j++) {
                spif_uint32_t k;

                hash = hash_func(&one_byte, SPIF_CAST(uint32) 0, hash);
                null_hashes[j] = hash;
                /*printf("Empty string hash %lu is 0x%08x\n", j, hash);*/
                for (k = j - 1; k < 8; k--) {
                    TEST_FAIL_IF(null_hashes[j] == null_hashes[k]);
                }
            }
            TEST_PASS();
        }
    }

    TEST_PASSED("hash functions");
    return 0;
}

int
test_snprintf(void)
{

    return 0;
}

spif_charptr_t theme = NULL;
static void handle_theme(spif_charptr_t val_ptr) {theme = STRDUP(val_ptr);}

int
test_options(void)
{
    spif_uint32_t test_flag_var = 0;
    spif_charptr_t file_var = NULL, *exec_list = NULL;
    long num_var = 0, geom_var = 0;
    char *argv1[] = {
        "test", "-abvf", "somefile", "-n1", "-g", "3",
        "-e", "help", "me", "rhonda", NULL
    };
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
    spif_charptr_t display = NULL, name = NULL, *exec = NULL, *foo = NULL;
    long color = 0;
    spif_uint32_t options = 0;
    char *argv2[] = {
        "test", "-rt", "mytheme", "--name", "This is a name", "--exec=ssh foo@bar.com",
        "--scrollbar", "--buttonbar", "no", "some", "--login=0", "-mvd", "foo:0", "--color", "4",
        "stuff", "--foo", "blah", "-d", "eatme", NULL
    };
    int argc2 = 20;
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

    TEST_BEGIN("spifopt_parse() function");
    SPIFOPT_OPTLIST_SET(opts1);
    SPIFOPT_NUMOPTS_SET(sizeof(opts1) / sizeof(spifopt_t));
    SPIFOPT_ALLOWBAD_SET(0);
    SPIFOPT_FLAGS_SET(SPIFOPT_SETTING_PREPARSE);
    spifopt_parse(argc1, argv1);
    TEST_FAIL_IF(test_flag_var != 0x10);
    TEST_FAIL_IF(file_var != NULL);
    TEST_FAIL_IF(exec_list != NULL);
    TEST_FAIL_IF(num_var != 0);
    TEST_FAIL_IF(geom_var != 0);
    spifopt_parse(argc1, argv1);
    TEST_FAIL_IF(test_flag_var != 0x13);
    TEST_FAIL_IF(file_var == NULL);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(file_var), "somefile"));
    TEST_FAIL_IF(exec_list == NULL);
    TEST_FAIL_IF(num_var != 1);
    TEST_FAIL_IF(geom_var != 3);
    FREE(file_var);
    spiftool_free_array(exec_list, -1);

    SPIFOPT_OPTLIST_SET(opts2);
    SPIFOPT_NUMOPTS_SET(sizeof(opts2) / sizeof(spifopt_t));
    SPIFOPT_ALLOWBAD_SET(0);
    SPIFOPT_FLAGS_SET(SPIFOPT_SETTING_PREPARSE);
    spifopt_parse(argc2, argv2);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(display), "foo:0"));
    TEST_FAIL_IF(name != NULL);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(theme), "mytheme"));
    TEST_FAIL_IF(exec == NULL);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(exec[0]), "ssh"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(exec[1]), "foo@bar.com"));
    TEST_FAIL_IF(exec[2] != NULL);
    TEST_FAIL_IF(foo != NULL);
    TEST_FAIL_IF(color != 0);
    TEST_FAIL_IF(options != 0x08);
    spifopt_parse(argc2, argv2);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(display), "foo:0"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(name), "This is a name"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(theme), "mytheme"));
    TEST_FAIL_IF(exec == NULL);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(exec[0]), "ssh"));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(exec[1]), "foo@bar.com"));
    TEST_FAIL_IF(exec[2] != NULL);
    TEST_FAIL_IF(foo == NULL);
    TEST_FAIL_IF(SPIF_PTR_ISNULL(foo[0]));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(foo[0]), "blah"));
    TEST_FAIL_IF(SPIF_PTR_ISNULL(foo[1]));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(foo[1]), "-d"));
    TEST_FAIL_IF(SPIF_PTR_ISNULL(foo[2]));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(foo[2]), "eatme"));
    TEST_FAIL_IF(foo[3] != NULL);
    TEST_FAIL_IF(color != 4);
    TEST_FAIL_IF(options != 0x1e);
    TEST_FAIL_IF(strcmp(argv2[0], "test"));
    TEST_FAIL_IF(strcmp(argv2[1], "some"));
    TEST_FAIL_IF(strcmp(argv2[2], "stuff"));
    TEST_FAIL_IF(!SPIF_PTR_ISNULL(argv2[3]));
    FREE(display);
    FREE(name);
    FREE(theme);
    spiftool_free_array(exec, -1);
    spiftool_free_array(foo, -1);

    TEST_PASS();

    TEST_PASSED("options");
    return 0;
}

int
test_obj(void)
{
    spif_obj_t testobj;
    spif_class_t cls;

    TEST_BEGIN("spif_obj_new() function");
    testobj = spif_obj_new();
    TEST_FAIL_IF(SPIF_OBJ_ISNULL(testobj));
    TEST_PASS();

    TEST_BEGIN("spif_obj_get_classname() function");
    cls = spif_obj_get_class(testobj);
    TEST_FAIL_IF(cls != SPIF_CLASS_VAR(obj));
    TEST_PASS();

    TEST_BEGIN("spif_obj_del() function");
    TEST_FAIL_IF(spif_obj_del(testobj) != TRUE);
    TEST_PASS();

    TEST_PASSED("spif_obj_t");
    return 0;
}

int
test_str(void)
{
    spif_str_t teststr, test2str;
    spif_strclass_t cls;
    char tmp[] = "this is a test";
    spif_char_t buff[4096] = "abcde";
    char tmp2[] = "string #1\nstring #2";
    FILE *fp;
    int fd, mypipe[2];
    spif_charptr_t foo;

    TEST_BEGIN("spif_str_new() function");
    teststr = spif_str_new();
    TEST_FAIL_IF(SPIF_STR_ISNULL(teststr));
    TEST_PASS();

    TEST_BEGIN("spif_obj_get_classname() function");
    cls = SPIF_STR_CLASS(spif_obj_get_class(SPIF_OBJ(teststr)));
    TEST_FAIL_IF(cls != SPIF_STR_CLASS(SPIF_STRCLASS_VAR(str)));
    TEST_PASS();

    TEST_BEGIN("spif_str_del() function");
    TEST_FAIL_IF(spif_str_del(teststr) != TRUE);
    TEST_PASS();

    TEST_BEGIN("spif_str_new_from_ptr() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR(tmp));
    TEST_FAIL_IF(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR(tmp)));
    TEST_FAIL_IF(spif_str_get_size(teststr) != sizeof(tmp));
    TEST_FAIL_IF(spif_str_get_len(teststr) != (sizeof(tmp) - 1));
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_new_from_buff() function");
    teststr = spif_str_new_from_buff(buff, sizeof(buff));
    TEST_FAIL_IF(spif_str_casecmp_with_ptr(teststr, buff));
    TEST_FAIL_IF(spif_str_get_size(teststr) != sizeof(buff));
    TEST_FAIL_IF(spif_str_get_len(teststr) != 5);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_new_from_fp() function");
    pipe(mypipe);
    fd = mypipe[0];
    fp = fdopen(fd, "r");
    write(mypipe[1], tmp2, sizeof(tmp2));
    close(mypipe[1]);
    TEST_FAIL_IF(fp == NULL);
    teststr = spif_str_new_from_fp(fp);
    TEST_FAIL_IF(spif_str_ncmp_with_ptr(teststr, SPIF_CHARPTR("string #1"), 9));
    TEST_FAIL_IF(spif_str_get_len(teststr) != 9);
    TEST_FAIL_IF(spif_str_get_size(teststr) != 10);
    spif_str_del(teststr);
    teststr = spif_str_new_from_fp(fp);
    TEST_FAIL_IF(spif_str_ncasecmp_with_ptr(teststr, SPIF_CHARPTR("string #2"), 9));
    TEST_FAIL_IF(spif_str_get_len(teststr) != 9);
    TEST_FAIL_IF(spif_str_get_size(teststr) != 10);
    spif_str_del(teststr);
    fclose(fp);
    TEST_PASS();

    TEST_BEGIN("spif_str_new_from_fd() function");
    pipe(mypipe);
    fd = mypipe[0];
    write(mypipe[1], tmp2, sizeof(tmp2) - 1);
    close(mypipe[1]);
    teststr = spif_str_new_from_fd(fd);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(SPIF_STR_STR(teststr)), tmp2));
    TEST_FAIL_IF(spif_str_get_len(teststr) != (sizeof(tmp2) - 1));
    TEST_FAIL_IF(spif_str_get_size(teststr) != sizeof(tmp2));
    spif_str_del(teststr);
    close(fd);
    TEST_PASS();

    TEST_BEGIN("spif_str_new_from_num() function");
    teststr = spif_str_new_from_num(1234567890L);
    TEST_FAIL_IF(SPIF_STR_ISNULL(teststr));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("1234567890"))));
    spif_str_done(teststr);
    spif_str_init_from_num(teststr, 2147483647L);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("2147483647"))));
    spif_str_done(teststr);
    spif_str_init_from_num(teststr, -2147483647L);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("-2147483647"))));
    spif_str_done(teststr);
    spif_str_init_from_num(teststr, 0x00000000);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("0"))));
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_dup() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR(tmp));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(SPIF_STR_STR(teststr)), tmp));
    TEST_FAIL_IF(spif_str_get_size(teststr) != sizeof(tmp));
    TEST_FAIL_IF(spif_str_get_len(teststr) != (sizeof(tmp) - 1));
    test2str = spif_str_dup(teststr);
    TEST_FAIL_IF(test2str == teststr);
    TEST_FAIL_IF(SPIF_STR_STR(test2str) == SPIF_STR_STR(teststr));
    TEST_FAIL_IF(spif_str_cmp(teststr, test2str));
    TEST_FAIL_IF(spif_str_casecmp(teststr, test2str));
    TEST_FAIL_IF(spif_str_ncmp(teststr, test2str, spif_str_get_len(teststr)));
    TEST_FAIL_IF(spif_str_ncasecmp(teststr, test2str, spif_str_get_len(test2str)));
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(SPIF_STR_STR(test2str)), tmp));
    TEST_FAIL_IF(spif_str_get_size(test2str) != sizeof(tmp));
    TEST_FAIL_IF(spif_str_get_len(test2str) != (sizeof(tmp) - 1));
    spif_str_del(teststr);
    spif_str_del(test2str);
    TEST_PASS();

    TEST_BEGIN("spif_str_index() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR(tmp2));
    TEST_FAIL_IF(spif_str_index(teststr, '#') != 7);
    TEST_PASS();

    TEST_BEGIN("spif_str_rindex() function");
    TEST_FAIL_IF(spif_str_rindex(teststr, '#') != 17);
    TEST_PASS();

    TEST_BEGIN("spif_str_find() function");
    test2str = spif_str_new_from_ptr(SPIF_CHARPTR("ring"));
    TEST_FAIL_IF(spif_str_find(teststr, test2str) != 2);
    spif_str_del(test2str);
    TEST_PASS();

    TEST_BEGIN("spif_str_find_from_ptr() function");
    TEST_FAIL_IF(spif_str_find_from_ptr(teststr, SPIF_CHARPTR("in")) != 3);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_substr() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR(tmp));
    test2str = spif_str_substr(teststr, 2, 5);
    TEST_FAIL_IF(spif_str_cmp_with_ptr(test2str, SPIF_CHARPTR("is is")));
    TEST_FAIL_IF(spif_str_get_size(test2str) != 6);
    TEST_FAIL_IF(spif_str_get_len(test2str) != 5);
    spif_str_del(test2str);
    test2str = spif_str_substr(teststr, -4, 4);
    TEST_FAIL_IF(spif_str_cmp_with_ptr(test2str, SPIF_CHARPTR("test")));
    TEST_FAIL_IF(spif_str_get_size(test2str) != 5);
    TEST_FAIL_IF(spif_str_get_len(test2str) != 4);
    spif_str_del(test2str);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_substr_to_ptr() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR(tmp));
    foo = spif_str_substr_to_ptr(teststr, 2, 5);
    TEST_FAIL_IF(foo == NULL);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(foo), "is is"));
    FREE(foo);
    foo = spif_str_substr_to_ptr(teststr, -4, 4);
    TEST_FAIL_IF(foo == NULL);
    TEST_FAIL_IF(strcmp(SPIF_CHARPTR_C(foo), "test"));
    FREE(foo);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_to_num() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR("11001001"));
    TEST_FAIL_IF(spif_str_to_num(teststr, 2) != 201);
    TEST_FAIL_IF(spif_str_to_num(teststr, 10) != 11001001);
    spif_str_del(teststr);
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR("0132"));
    TEST_FAIL_IF(spif_str_to_num(teststr, 0) != 90);
    TEST_FAIL_IF(spif_str_to_num(teststr, 8) != 90);
    TEST_FAIL_IF(spif_str_to_num(teststr, 10) != 132);
    spif_str_del(teststr);
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR("0xff"));
    TEST_FAIL_IF(spif_str_to_num(teststr, 0) != 255);
    TEST_FAIL_IF(spif_str_to_num(teststr, 10) != 0);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_to_float() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR("3.1415"));
    TEST_FAIL_IF(spif_str_to_float(teststr) != 3.1415);
    spif_str_del(teststr);
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR("2.71"));
    TEST_FAIL_IF(spif_str_to_float(teststr) != 2.71);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_append() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR("copy"));
    test2str = spif_str_new_from_ptr(SPIF_CHARPTR("cat"));
    spif_str_append(teststr, test2str);
    TEST_FAIL_IF(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("copycat")));
    TEST_FAIL_IF(spif_str_get_size(teststr) != 8);
    TEST_FAIL_IF(spif_str_get_len(teststr) != 7);
    spif_str_del(test2str);
    TEST_PASS();

    TEST_BEGIN("spif_str_append_char() function");
    test2str = spif_str_new_from_ptr(SPIF_CHARPTR("Hello"));
    spif_str_append_char(test2str, ' ');
    spif_str_append_char(test2str, 'w');
    spif_str_append_char(test2str, 'o');
    spif_str_append_char(test2str, 'r');
    spif_str_append_char(test2str, 'l');
    spif_str_append_char(test2str, 'd');
    TEST_FAIL_IF(spif_str_cmp_with_ptr(test2str, SPIF_CHARPTR("Hello world")));
    TEST_FAIL_IF(spif_str_get_size(test2str) != 12);
    TEST_FAIL_IF(spif_str_get_len(test2str) != 11);
    spif_str_del(test2str);
    TEST_PASS();

    TEST_BEGIN("spif_str_append_from_ptr() function");
    spif_str_append_from_ptr(teststr, SPIF_CHARPTR("crime"));
    TEST_FAIL_IF(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("copycatcrime")));
    TEST_FAIL_IF(spif_str_get_size(teststr) != 13);
    TEST_FAIL_IF(spif_str_get_len(teststr) != 12);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_clear() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR("abcdefg"));
    spif_str_clear(teststr, 'x');
    TEST_FAIL_IF(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("xxxxxxx")));
    TEST_FAIL_IF(spif_str_get_size(teststr) != 8);
    TEST_FAIL_IF(spif_str_get_len(teststr) != 7);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_trim() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR("  \n \r\f       \t    testing 1 2 3    \v\r \n"));
    spif_str_trim(teststr);
    TEST_FAIL_IF(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("testing 1 2 3")));
    TEST_FAIL_IF(spif_str_get_size(teststr) != 14);
    TEST_FAIL_IF(spif_str_get_len(teststr) != 13);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_splice() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR(tmp));
    test2str = spif_str_new_from_ptr(SPIF_CHARPTR("lots of fun"));
    spif_str_splice(teststr, 8, 6, test2str);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("this is lots of fun"))));
    TEST_FAIL_IF(spif_str_get_size(teststr) != 20);
    TEST_FAIL_IF(spif_str_get_len(teststr) != 19);
    spif_str_del(test2str);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_splice_from_ptr() function");
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR(tmp));
    spif_str_splice_from_ptr(teststr, 8, 0, SPIF_CHARPTR("not "));
    TEST_FAIL_IF(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("this is not a test")));
    TEST_FAIL_IF(spif_str_get_size(teststr) != 19);
    TEST_FAIL_IF(spif_str_get_len(teststr) != 18);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_BEGIN("spif_str_reverse() function");
    teststr = spif_str_new_from_buff(buff, sizeof(buff));
    spif_str_reverse(teststr);
    TEST_FAIL_IF(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("edcba")));
    TEST_FAIL_IF(spif_str_get_size(teststr) != sizeof(buff));
    TEST_FAIL_IF(spif_str_get_len(teststr) != 5);
    spif_str_del(teststr);
    TEST_PASS();

    TEST_PASSED("spif_str_t");
    return 0;
}

int
test_tok(void)
{
    spif_tok_t testtok;
    spif_str_t teststr;
    spif_list_t testlist;
    spif_class_t cls;
    spif_charptr_t tmp = SPIF_CHARPTR("I \"can\'t\" feel my legs!");
    spif_charptr_t tmp2 = SPIF_CHARPTR(":::some:seedy:colon-delimited::data");
    spif_charptr_t tmp3 = SPIF_CHARPTR("\"this is one token\" and this \'over here\' is \"another one\"");
    spif_charptr_t tmp4 = SPIF_CHARPTR("\"there shouldn't be\"\' any problems at\'\"\"\'\'\' \'\"all parsing this\"");

    TEST_BEGIN("spif_tok_new() function");
    testtok = spif_tok_new();
    TEST_FAIL_IF(SPIF_OBJ_ISNULL(testtok));
    TEST_PASS();

    TEST_BEGIN("spif_obj_get_classname() function");
    cls = spif_obj_get_class(SPIF_OBJ(testtok));
    TEST_FAIL_IF(cls != SPIF_CLASS_VAR(tok));
    TEST_PASS();

    TEST_BEGIN("spif_tok_del() function");
    TEST_FAIL_IF(spif_tok_del(testtok) != TRUE);
    TEST_PASS();

    TEST_BEGIN("spif_tok_new_from_ptr() function");
    testtok = spif_tok_new_from_ptr(tmp);
    TEST_FAIL_IF(spif_str_cmp_with_ptr(spif_tok_get_src(testtok), tmp));
    spif_tok_del(testtok);
    TEST_PASS();

    TEST_BEGIN("spif_tok_eval() function");
    testtok = spif_tok_new_from_ptr(tmp);
    TEST_FAIL_IF(SPIF_TOK_ISNULL(testtok));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_tok_get_src(testtok), tmp)));
    spif_tok_eval(testtok);
    testlist = SPIF_CAST(list) spif_tok_get_tokens(testtok);
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 0);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("I"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 1);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("can't"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 2);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("feel"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 3);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("my"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 4);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("legs!"))));
    spif_tok_del(testtok);

    testtok = spif_tok_new_from_ptr(tmp2);
    TEST_FAIL_IF(SPIF_TOK_ISNULL(testtok));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_tok_get_src(testtok), tmp2)));
    teststr = spif_str_new_from_ptr(SPIF_CHARPTR(":"));
    spif_tok_set_sep(testtok, teststr);
    spif_tok_eval(testtok);
    testlist = SPIF_CAST(list) spif_tok_get_tokens(testtok);
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 0);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("some"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 1);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("seedy"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 2);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("colon-delimited"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 3);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("data"))));
    spif_tok_del(testtok);

    testtok = spif_tok_new_from_ptr(tmp3);
    TEST_FAIL_IF(SPIF_TOK_ISNULL(testtok));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_tok_get_src(testtok), tmp3)));
    spif_tok_eval(testtok);
    testlist = SPIF_CAST(list) spif_tok_get_tokens(testtok);
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 0);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("this is one token"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 1);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("and"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 2);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("this"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 3);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("over here"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 4);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("is"))));
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 5);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("another one"))));
    spif_tok_del(testtok);

    testtok = spif_tok_new_from_ptr(tmp4);
    TEST_FAIL_IF(SPIF_TOK_ISNULL(testtok));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_tok_get_src(testtok), tmp4)));
    spif_tok_eval(testtok);
    testlist = SPIF_CAST(list) spif_tok_get_tokens(testtok);
    teststr = SPIF_CAST(str) SPIF_LIST_GET(testlist, 0);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(teststr, SPIF_CHARPTR("there shouldn't be any problems at all parsing this"))));
    spif_tok_del(testtok);

    TEST_PASS();


    TEST_PASSED("spif_tok_t");
    return 0;
}

int
test_url(void)
{
    spif_url_t testurl, testurl2, testurl3, testurl4;
    spif_charptr_t tmp1 = SPIF_CHARPTR("http://www.kainx.org/journal/?view=20020104");
    spif_charptr_t tmp2 = SPIF_CHARPTR("mailto:foo@bar.com?Subject=Eat Me");
    spif_charptr_t tmp3 = SPIF_CHARPTR("/path/to/some/file.jpg");
    spif_charptr_t tmp4 = SPIF_CHARPTR("pop3://dummy:moo@pop.nowhere.com:110");

    TEST_BEGIN("spif_url_new_from_ptr() function");
    testurl = spif_url_new_from_ptr(tmp1);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(SPIF_STR(testurl), tmp1)));
    testurl2 = spif_url_new_from_ptr(tmp2);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(SPIF_STR(testurl2), tmp2)));
    testurl3 = spif_url_new_from_ptr(tmp3);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(SPIF_STR(testurl3), tmp3)));
    testurl4 = spif_url_new_from_ptr(tmp4);
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(SPIF_STR(testurl4), tmp4)));
    TEST_PASS();

    TEST_BEGIN("spif_url_parse() function");
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_proto(testurl), SPIF_CHARPTR("http"))));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_user(testurl)));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_passwd(testurl)));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_host(testurl), SPIF_CHARPTR("www.kainx.org"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_port(testurl), SPIF_CHARPTR("80"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_path(testurl), SPIF_CHARPTR("/journal/"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_query(testurl), SPIF_CHARPTR("view=20020104"))));
    spif_url_del(testurl);

    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_proto(testurl2), SPIF_CHARPTR("mailto"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_user(testurl2), SPIF_CHARPTR("foo"))));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_passwd(testurl2)));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_host(testurl2), SPIF_CHARPTR("bar.com"))));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_port(testurl2)));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_path(testurl2)));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_query(testurl2), SPIF_CHARPTR("Subject=Eat Me"))));
    spif_url_del(testurl2);

    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_proto(testurl3)));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_user(testurl3)));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_passwd(testurl3)));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_host(testurl3)));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_port(testurl3)));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_path(testurl3), tmp3)));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_query(testurl3)));
    spif_url_del(testurl3);

    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_proto(testurl4), SPIF_CHARPTR("pop3"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_user(testurl4), SPIF_CHARPTR("dummy"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_passwd(testurl4), SPIF_CHARPTR("moo"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_host(testurl4), SPIF_CHARPTR("pop.nowhere.com"))));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(spif_url_get_port(testurl4), SPIF_CHARPTR("110"))));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_path(testurl4)));
    TEST_FAIL_IF(!SPIF_STR_ISNULL(spif_url_get_query(testurl4)));
    spif_url_del(testurl4);

    TEST_PASS();

    TEST_PASSED("spif_url_t");
    return 0;
}

int
test_list(void)
{
    unsigned short i;
    spif_list_t testlist;
    spif_str_t s, s2;
    spif_obj_t *list_array;
    spif_iterator_t it;
    size_t j;

    for (i = 0; i < 3; i++) {
        if (i == 0) {
            TEST_NOTICE("*** Testing list interface class, linked_list instance:");
            testlist = SPIF_LIST_NEW(linked_list);
        } else if (i == 1) {
            TEST_NOTICE("*** Testing list interface class, dlinked_list instance:");
            testlist = SPIF_LIST_NEW(dlinked_list);
        } else if (i == 2) {
            TEST_NOTICE("*** Testing list interface class, array instance:");
            testlist = SPIF_LIST_NEW(array);
        } else if (i == 3) {
        }

        TEST_BEGIN("SPIF_LIST_APPEND() and SPIF_LIST_PREPEND() macros");
        SPIF_LIST_APPEND(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("1")));
        SPIF_LIST_APPEND(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("2")));
        SPIF_LIST_APPEND(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("3")));
        SPIF_LIST_APPEND(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("4")));
        SPIF_LIST_APPEND(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("5")));
        SPIF_LIST_PREPEND(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("0")));

        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 0);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("1"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 1);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("2"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 2);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 3);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 4);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 5);
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_CONTAINS() macro");
        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, s));
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_COUNT() macro");
        TEST_FAIL_IF(SPIF_LIST_COUNT(testlist) != 6);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_FIND() macro");
        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(SPIF_STR_COMP(s, SPIF_STR(SPIF_LIST_FIND(testlist, s))) != SPIF_CMP_EQUAL);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(SPIF_STR_COMP(s, SPIF_STR(SPIF_LIST_FIND(testlist, s))) != SPIF_CMP_EQUAL);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("8"));
        TEST_FAIL_IF(!SPIF_STR_ISNULL(SPIF_STR(SPIF_LIST_FIND(testlist, s))));
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_GET() macro");
        s = spif_str_new_from_ptr(SPIF_CHARPTR("2"));
        TEST_FAIL_IF(SPIF_STR_COMP(s, SPIF_STR(SPIF_LIST_GET(testlist, 2))) != SPIF_CMP_EQUAL);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4"));
        TEST_FAIL_IF(SPIF_STR_COMP(s, SPIF_STR(SPIF_LIST_GET(testlist, 4))) != SPIF_CMP_EQUAL);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("6"));
        TEST_FAIL_IF(!SPIF_STR_ISNULL(SPIF_STR(SPIF_LIST_GET(testlist, 6))));
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_INDEX() macro");
        s = spif_str_new_from_ptr(SPIF_CHARPTR("4"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 4);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("1"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 1);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("Q"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != ((size_t) -1));
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_INSERT() macro");
        SPIF_LIST_INSERT(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("2.5")));
        SPIF_LIST_INSERT(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("4.5")));
        TEST_FAIL_IF(SPIF_LIST_COUNT(testlist) != 8);
        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 0);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("2.5"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 3);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 4);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4.5"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 6);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 7);
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_INSERT_AT() macro");
        SPIF_LIST_INSERT_AT(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("MOO")), 0);
        SPIF_LIST_INSERT_AT(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("GRIN")), 4);
        SPIF_LIST_INSERT_AT(testlist, spif_str_new_from_ptr(SPIF_CHARPTR("BOB")), 12);
        TEST_FAIL_IF(SPIF_LIST_COUNT(testlist) != 13);
        s = spif_str_new_from_ptr(SPIF_CHARPTR("MOO"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 0);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("0"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 1);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("1"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 2);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("GRIN"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 4);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 9);
        spif_str_done(s);
        TEST_FAIL_IF(!SPIF_OBJ_ISNULL(SPIF_LIST_GET(testlist, 10)));
        TEST_FAIL_IF(!SPIF_OBJ_ISNULL(SPIF_LIST_GET(testlist, 11)));
        spif_str_init_from_ptr(s, SPIF_CHARPTR("BOB"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 12);
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_ITERATOR() macro");
        for (j = 0, it = SPIF_LIST_ITERATOR(testlist); SPIF_ITERATOR_HAS_NEXT(it); j++) {
            spif_str_t tmp;

            tmp = SPIF_CAST(str) SPIF_ITERATOR_NEXT(it);
        }
        TEST_FAIL_IF(j != 13);
        TEST_FAIL_IF(SPIF_ITERATOR_HAS_NEXT(it));
        TEST_FAIL_IF(!SPIF_OBJ_ISNULL(SPIF_ITERATOR_NEXT(it)));
        SPIF_ITERATOR_DEL(it);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_REMOVE() macro");
        s = spif_str_new_from_ptr(SPIF_CHARPTR("MOO"));
        s2 = SPIF_CAST(str) SPIF_LIST_REMOVE(testlist, s);
        TEST_FAIL_IF(SPIF_OBJ_ISNULL(s2));
        spif_str_del(s2);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("GRIN"));
        s2 = SPIF_CAST(str) SPIF_LIST_REMOVE(testlist, s);
        TEST_FAIL_IF(SPIF_OBJ_ISNULL(s2));
        spif_str_del(s2);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("BOB"));
        s2 = SPIF_CAST(str) SPIF_LIST_REMOVE(testlist, s);
        TEST_FAIL_IF(SPIF_OBJ_ISNULL(s2));
        spif_str_del(s2);
        spif_str_del(s);

        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 0);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("2.5"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 3);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 4);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4.5"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 6);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 7);
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_REMOVE_AT() macro");
        s2 = SPIF_CAST(str) SPIF_LIST_REMOVE_AT(testlist, 11);
        TEST_FAIL_IF(!SPIF_STR_ISNULL(s2));
        s2 = SPIF_CAST(str) SPIF_LIST_REMOVE_AT(testlist, 10);
        TEST_FAIL_IF(!SPIF_STR_ISNULL(s2));
        s2 = SPIF_CAST(str) SPIF_LIST_REMOVE_AT(testlist, 9);
        TEST_FAIL_IF(!SPIF_STR_ISNULL(s2));
        s2 = SPIF_CAST(str) SPIF_LIST_REMOVE_AT(testlist, 8);
        TEST_FAIL_IF(!SPIF_STR_ISNULL(s2));
        s2 = SPIF_CAST(str) SPIF_LIST_REMOVE_AT(testlist, 6);
        TEST_FAIL_IF(SPIF_STR_ISNULL(s2));
        spif_str_del(s2);
        s2 = SPIF_CAST(str) SPIF_LIST_REMOVE_AT(testlist, 3);
        TEST_FAIL_IF(SPIF_STR_ISNULL(s2));
        spif_str_del(s2);

        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 0);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("1"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 1);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("2"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 2);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 3);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 4);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 5);
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_REVERSE() macro");
        SPIF_LIST_REVERSE(testlist);

        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 5);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("1"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 4);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("2"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 3);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 2);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 1);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(SPIF_LIST_INDEX(testlist, s) != 0);
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_LIST_TO_ARRAY() macro");
        list_array = SPIF_LIST_TO_ARRAY(testlist);

        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(list_array[5], s)));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("1"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(list_array[4], s)));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("2"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(list_array[3], s)));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(list_array[2], s)));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(list_array[1], s)));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(list_array[0], s)));
        spif_str_del(s);
        SPIF_DEALLOC(list_array);
        TEST_PASS();

        /*SPIF_SHOW(testlist, stdout);*/
        SPIF_LIST_DEL(testlist);
    }

    TEST_PASSED("list interface class");
    return 0;
}

int
test_vector(void)
{
    unsigned short i;
    spif_vector_t testvector;
    spif_str_t s, s2;
    spif_obj_t *vector_array;
    spif_iterator_t it;
    size_t j;

    for (i = 0; i < 3; i++) {
        if (i == 0) {
            TEST_NOTICE("*** Testing vector interface class, linked_list instance:");
            testvector = SPIF_VECTOR_NEW(linked_list);
        } else if (i == 1) {
            TEST_NOTICE("*** Testing vector interface class, dlinked_list instance:");
            testvector = SPIF_VECTOR_NEW(dlinked_list);
        } else if (i == 2) {
            TEST_NOTICE("*** Testing vector interface class, array instance:");
            testvector = SPIF_VECTOR_NEW(array);
        } else if (i == 3) {
        }

        TEST_BEGIN("SPIF_VECTOR_INSERT() macro");
        SPIF_VECTOR_INSERT(testvector, spif_str_new_from_ptr(SPIF_CHARPTR("1")));
        s = spif_str_new_from_ptr(SPIF_CHARPTR("1"));
        TEST_FAIL_IF(!SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_done(s);
        SPIF_VECTOR_INSERT(testvector, spif_str_new_from_ptr(SPIF_CHARPTR("3")));
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(!SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_done(s);
        SPIF_VECTOR_INSERT(testvector, spif_str_new_from_ptr(SPIF_CHARPTR("2")));
        spif_str_init_from_ptr(s, SPIF_CHARPTR("2"));
        TEST_FAIL_IF(!SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_done(s);
        SPIF_VECTOR_INSERT(testvector, spif_str_new_from_ptr(SPIF_CHARPTR("5")));
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(!SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_done(s);
        SPIF_VECTOR_INSERT(testvector, spif_str_new_from_ptr(SPIF_CHARPTR("0")));
        spif_str_init_from_ptr(s, SPIF_CHARPTR("0"));
        TEST_FAIL_IF(!SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_done(s);
        SPIF_VECTOR_INSERT(testvector, spif_str_new_from_ptr(SPIF_CHARPTR("4")));
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4"));
        TEST_FAIL_IF(!SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_VECTOR_CONTAINS() macro");
        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(!SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(!SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(!SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("8"));
        TEST_FAIL_IF(SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("0.2"));
        TEST_FAIL_IF(SPIF_VECTOR_CONTAINS(testvector, s));
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_VECTOR_COUNT() macro");
        TEST_FAIL_IF(SPIF_VECTOR_COUNT(testvector) != 6);
        TEST_PASS();

        TEST_BEGIN("SPIF_VECTOR_FIND() macro");
        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(SPIF_STR_COMP(s, SPIF_STR(SPIF_VECTOR_FIND(testvector, s))) != SPIF_CMP_EQUAL);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("3"));
        TEST_FAIL_IF(SPIF_STR_COMP(s, SPIF_STR(SPIF_VECTOR_FIND(testvector, s))) != SPIF_CMP_EQUAL);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("8"));
        TEST_FAIL_IF(!SPIF_STR_ISNULL(SPIF_STR(SPIF_VECTOR_FIND(testvector, s))));
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_VECTOR_ITERATOR() macro");
        for (j = 0, it = SPIF_VECTOR_ITERATOR(testvector); SPIF_ITERATOR_HAS_NEXT(it); j++) {
            spif_str_t tmp;

            tmp = SPIF_CAST(str) SPIF_ITERATOR_NEXT(it);
            TEST_FAIL_IF(SPIF_STR_ISNULL(tmp));
        }
        TEST_FAIL_IF(j != 6);
        TEST_FAIL_IF(SPIF_ITERATOR_HAS_NEXT(it));
        TEST_FAIL_IF(!SPIF_OBJ_ISNULL(SPIF_ITERATOR_NEXT(it)));
        SPIF_ITERATOR_DEL(it);
        TEST_PASS();

        TEST_BEGIN("SPIF_VECTOR_REMOVE() macro");
        s = spif_str_new_from_ptr(SPIF_CHARPTR("3"));
        s2 = SPIF_CAST(str) SPIF_VECTOR_REMOVE(testvector, s);
        TEST_FAIL_IF(SPIF_OBJ_ISNULL(s2));
        spif_str_del(s2);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("GRIN"));
        s2 = SPIF_CAST(str) SPIF_VECTOR_REMOVE(testvector, s);
        TEST_FAIL_IF(!SPIF_OBJ_ISNULL(s2));
        spif_str_del(s);
        TEST_PASS();

        TEST_BEGIN("SPIF_VECTOR_TO_ARRAY() macro");
        vector_array = SPIF_VECTOR_TO_ARRAY(testvector);

        s = spif_str_new_from_ptr(SPIF_CHARPTR("0"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(vector_array[0], s)));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("1"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(vector_array[1], s)));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("2"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(vector_array[2], s)));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(vector_array[3], s)));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("5"));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(SPIF_OBJ_COMP(vector_array[4], s)));
        spif_str_del(s);
        SPIF_DEALLOC(vector_array);
        TEST_PASS();

        /*SPIF_SHOW(testvector, stdout);*/
        SPIF_VECTOR_DEL(testvector);
    }

    TEST_PASSED("vector interface class");
    return 0;
}

int
test_map(void)
{
    unsigned short i;
    spif_map_t testmap;
    spif_obj_t ret;
    spif_str_t key, value;
    spif_url_t homepage;
    spif_list_t testlist;
    spif_iterator_t it;
    size_t j;

    for (i = 0; i < 3; i++) {
        if (i == 0) {
            TEST_NOTICE("*** Testing map interface, linked_list class:");
            testmap = SPIF_MAP_NEW(linked_list);
        } else if (i == 1) {
            TEST_NOTICE("*** Testing map interface, dlinked_list class:");
            testmap = SPIF_MAP_NEW(dlinked_list);
        } else if (i == 2) {
            TEST_NOTICE("*** Testing map interface, array class:");
            testmap = SPIF_MAP_NEW(array);
        } else if (i == 3) {
        }

        TEST_BEGIN("SPIF_MAP_SET() macro");
        key = spif_str_new_from_ptr(SPIF_CHARPTR("name"));
        value = spif_str_new_from_ptr(SPIF_CHARPTR("Bob"));
        TEST_FAIL_IF(SPIF_MAP_SET(testmap, key, value));
        spif_str_done(key);
        spif_str_done(value);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("rank"));
        spif_str_init_from_ptr(value, SPIF_CHARPTR("Dweeb"));
        TEST_FAIL_IF(SPIF_MAP_SET(testmap, key, value));
        spif_str_done(key);
        spif_str_done(value);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("serial number"));
        spif_str_init_from_ptr(value, SPIF_CHARPTR("123456"));
        TEST_FAIL_IF(SPIF_MAP_SET(testmap, key, value));
        spif_str_done(key);
        spif_str_done(value);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("homepage"));
        homepage = spif_url_new_from_ptr(SPIF_CHARPTR("http://www.dweeb.com/"));
        TEST_FAIL_IF(SPIF_MAP_SET(testmap, key, homepage));
        spif_str_done(key);
        spif_url_del(homepage);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("e-mail"));
        spif_str_init_from_ptr(value, SPIF_CHARPTR("bob@dweeb.com"));
        TEST_FAIL_IF(SPIF_MAP_SET(testmap, key, value));
        spif_str_done(key);
        spif_str_done(value);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("loser"));
        spif_str_init_from_ptr(value, SPIF_CHARPTR("YES"));
        TEST_FAIL_IF(SPIF_MAP_SET(testmap, key, value));
        spif_str_del(key);
        spif_str_del(value);
        TEST_PASS();

        TEST_BEGIN("SPIF_MAP_COUNT() macro");
        TEST_FAIL_IF(SPIF_MAP_COUNT(testmap) != 6);
        TEST_PASS();

        TEST_BEGIN("SPIF_MAP_GET() macro");
        key = spif_str_new_from_ptr(SPIF_CHARPTR("serial number"));
        value = SPIF_CAST(str) SPIF_MAP_GET(testmap, key);
        TEST_FAIL_IF(SPIF_STR_ISNULL(value));
        TEST_FAIL_IF(!SPIF_OBJ_IS_STR(value));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(value, SPIF_CHARPTR("123456"))));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("loser"));
        value = SPIF_CAST(str) SPIF_MAP_GET(testmap, key);
        TEST_FAIL_IF(SPIF_STR_ISNULL(value));
        TEST_FAIL_IF(!SPIF_OBJ_IS_STR(value));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(value, SPIF_CHARPTR("YES"))));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("name"));
        value = SPIF_CAST(str) SPIF_MAP_GET(testmap, key);
        TEST_FAIL_IF(SPIF_STR_ISNULL(value));
        TEST_FAIL_IF(!SPIF_OBJ_IS_STR(value));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(value, SPIF_CHARPTR("Bob"))));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("e-mail"));
        value = SPIF_CAST(str) SPIF_MAP_GET(testmap, key);
        TEST_FAIL_IF(SPIF_STR_ISNULL(value));
        TEST_FAIL_IF(!SPIF_OBJ_IS_STR(value));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(value, SPIF_CHARPTR("bob@dweeb.com"))));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("rank"));
        value = SPIF_CAST(str) SPIF_MAP_GET(testmap, key);
        TEST_FAIL_IF(SPIF_STR_ISNULL(value));
        TEST_FAIL_IF(!SPIF_OBJ_IS_STR(value));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(value, SPIF_CHARPTR("Dweeb"))));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("homepage"));
        homepage = SPIF_CAST(url) SPIF_MAP_GET(testmap, key);
        TEST_FAIL_IF(SPIF_URL_ISNULL(homepage));
        TEST_FAIL_IF(!SPIF_OBJ_IS_URL(homepage));
        TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp_with_ptr(SPIF_STR(homepage), SPIF_CHARPTR("http://www.dweeb.com/"))));
        spif_str_del(key);
        TEST_PASS();

        TEST_BEGIN("SPIF_MAP_GET_KEYS() macro");
        testlist = SPIF_LIST_NEW(array);
        SPIF_MAP_GET_KEYS(testmap, testlist);
        TEST_FAIL_IF(SPIF_LIST_COUNT(testlist) != SPIF_MAP_COUNT(testmap));
        key = spif_str_new_from_ptr(SPIF_CHARPTR("serial number"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, key));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("loser"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, key));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("name"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, key));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("e-mail"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, key));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("rank"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, key));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("homepage"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, key));
        spif_str_del(key);
        value = spif_str_new_from_ptr(SPIF_CHARPTR("123456"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, value));
        spif_str_done(value);
        spif_str_init_from_ptr(value, SPIF_CHARPTR("YES"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, value));
        spif_str_done(value);
        spif_str_init_from_ptr(value, SPIF_CHARPTR("Bob"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, value));
        spif_str_done(value);
        spif_str_init_from_ptr(value, SPIF_CHARPTR("bob@dweeb.com"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, value));
        spif_str_done(value);
        spif_str_init_from_ptr(value, SPIF_CHARPTR("Dweeb"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, value));
        spif_str_del(value);
        homepage = spif_url_new_from_ptr(SPIF_CHARPTR("http://www.dweeb.com/"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, homepage));
        spif_url_del(homepage);
        SPIF_LIST_DEL(testlist);
        TEST_PASS();

        TEST_BEGIN("SPIF_MAP_GET_PAIRS() macro");
        testlist = SPIF_LIST_NEW(array);
        SPIF_MAP_GET_PAIRS(testmap, testlist);
        TEST_FAIL_IF(SPIF_LIST_COUNT(testlist) != SPIF_MAP_COUNT(testmap));
        SPIF_LIST_DEL(testlist);
        TEST_PASS();

        TEST_BEGIN("SPIF_MAP_GET_VALUES() macro");
        testlist = SPIF_LIST_NEW(array);
        SPIF_MAP_GET_VALUES(testmap, testlist);
        TEST_FAIL_IF(SPIF_LIST_COUNT(testlist) != SPIF_MAP_COUNT(testmap));
        key = spif_str_new_from_ptr(SPIF_CHARPTR("serial number"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, key));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("loser"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, key));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("name"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, key));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("e-mail"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, key));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("rank"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, key));
        spif_str_done(key);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("homepage"));
        TEST_FAIL_IF(SPIF_LIST_CONTAINS(testlist, key));
        spif_str_del(key);
        value = spif_str_new_from_ptr(SPIF_CHARPTR("123456"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, value));
        spif_str_done(value);
        spif_str_init_from_ptr(value, SPIF_CHARPTR("YES"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, value));
        spif_str_done(value);
        spif_str_init_from_ptr(value, SPIF_CHARPTR("Bob"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, value));
        spif_str_done(value);
        spif_str_init_from_ptr(value, SPIF_CHARPTR("bob@dweeb.com"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, value));
        spif_str_done(value);
        spif_str_init_from_ptr(value, SPIF_CHARPTR("Dweeb"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, value));
        spif_str_del(value);
        homepage = spif_url_new_from_ptr(SPIF_CHARPTR("http://www.dweeb.com/"));
        TEST_FAIL_IF(!SPIF_LIST_CONTAINS(testlist, homepage));
        spif_url_del(homepage);
        SPIF_LIST_DEL(testlist);
        TEST_PASS();

        TEST_BEGIN("SPIF_MAP_ITERATOR(), SPIF_MAP_HAS_KEY(), and SPIF_MAP_HAS_VALUE() macros");
        for (j = 0, it = SPIF_MAP_ITERATOR(testmap); SPIF_ITERATOR_HAS_NEXT(it); j++) {
            spif_objpair_t tmp;

            tmp = SPIF_CAST(objpair) SPIF_ITERATOR_NEXT(it);
            TEST_FAIL_IF(SPIF_OBJPAIR_ISNULL(tmp));
            TEST_FAIL_IF(!SPIF_MAP_HAS_KEY(testmap, tmp->key));
            TEST_FAIL_IF(!SPIF_MAP_HAS_VALUE(testmap, tmp->value));
        }
        TEST_FAIL_IF(j != 6);
        TEST_FAIL_IF(SPIF_ITERATOR_HAS_NEXT(it));
        TEST_FAIL_IF(!SPIF_OBJ_ISNULL(SPIF_ITERATOR_NEXT(it)));
        SPIF_ITERATOR_DEL(it);
        TEST_PASS();

        TEST_BEGIN("SPIF_MAP_REMOVE() macro");
        key = spif_str_new_from_ptr(SPIF_CHARPTR("name"));
        value = spif_str_new_from_ptr(SPIF_CHARPTR("Bob"));
        ret = SPIF_MAP_REMOVE(testmap, key);
        TEST_FAIL_IF(SPIF_OBJ_ISNULL(ret));
        TEST_FAIL_IF(!SPIF_OBJ_COMP(ret, value));
        TEST_FAIL_IF(SPIF_MAP_HAS_KEY(testmap, key));
        SPIF_OBJ_DEL(ret);
        spif_str_done(key);
        spif_str_done(value);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("rank"));
        spif_str_init_from_ptr(value, SPIF_CHARPTR("Dweeb"));
        ret = SPIF_MAP_REMOVE(testmap, key);
        TEST_FAIL_IF(SPIF_OBJ_ISNULL(ret));
        TEST_FAIL_IF(!SPIF_OBJ_COMP(ret, value));
        TEST_FAIL_IF(SPIF_MAP_HAS_KEY(testmap, key));
        SPIF_OBJ_DEL(ret);
        spif_str_done(key);
        spif_str_done(value);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("serial number"));
        spif_str_init_from_ptr(value, SPIF_CHARPTR("123456"));
        ret = SPIF_MAP_REMOVE(testmap, key);
        TEST_FAIL_IF(SPIF_OBJ_ISNULL(ret));
        TEST_FAIL_IF(!SPIF_OBJ_COMP(ret, value));
        TEST_FAIL_IF(SPIF_MAP_HAS_KEY(testmap, key));
        SPIF_OBJ_DEL(ret);
        spif_str_done(key);
        spif_str_done(value);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("homepage"));
        homepage = spif_url_new_from_ptr(SPIF_CHARPTR("http://www.dweeb.com/"));
        ret = SPIF_MAP_REMOVE(testmap, key);
        TEST_FAIL_IF(SPIF_OBJ_ISNULL(ret));
        TEST_FAIL_IF(!SPIF_OBJ_COMP(ret, homepage));
        TEST_FAIL_IF(SPIF_MAP_HAS_KEY(testmap, key));
        SPIF_OBJ_DEL(ret);
        spif_str_done(key);
        spif_url_del(homepage);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("e-mail"));
        spif_str_init_from_ptr(value, SPIF_CHARPTR("bob@dweeb.com"));
        ret = SPIF_MAP_REMOVE(testmap, key);
        TEST_FAIL_IF(SPIF_OBJ_ISNULL(ret));
        TEST_FAIL_IF(!SPIF_OBJ_COMP(ret, value));
        TEST_FAIL_IF(SPIF_MAP_HAS_KEY(testmap, key));
        SPIF_OBJ_DEL(ret);
        spif_str_done(key);
        spif_str_done(value);
        spif_str_init_from_ptr(key, SPIF_CHARPTR("loser"));
        spif_str_init_from_ptr(value, SPIF_CHARPTR("YES"));
        ret = SPIF_MAP_REMOVE(testmap, key);
        TEST_FAIL_IF(SPIF_OBJ_ISNULL(ret));
        TEST_FAIL_IF(!SPIF_OBJ_COMP(ret, value));
        TEST_FAIL_IF(SPIF_MAP_HAS_KEY(testmap, key));
        SPIF_OBJ_DEL(ret);
        spif_str_del(key);
        spif_str_del(value);
        TEST_FAIL_IF(SPIF_MAP_COUNT(testmap) != 0);
        TEST_PASS();

        /*SPIF_SHOW(testmap, stdout);*/
        SPIF_MAP_DEL(testmap);
    }

    TEST_PASSED("map interface");
    return 0;
}

int
test_socket(void)
{
    spif_socket_t src1, dest1, src2, dest2, listen1, listen2;
    spif_url_t url1, url2;
    spif_str_t data1, data2;
    spif_charptr_t tmp1 = SPIF_CHARPTR("tcp://127.0.0.1:31737");
    spif_charptr_t tmp2 = SPIF_CHARPTR("unix:/tmp/libast-test-socket");
    spif_charptr_t strdata = SPIF_CHARPTR("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");
    spif_bool_t b;

    /* Unlink the UNIX socket in case it exists. */
    unlink(SPIF_CHARPTR_C(tmp2) + 5);

    TEST_BEGIN("spif_socket_new_from_urls() function");
    url1 = spif_url_new_from_ptr(tmp1);
    TEST_FAIL_IF(SPIF_URL_ISNULL(url1));
    listen1 = spif_socket_new_from_urls(url1, SPIF_NULL_TYPE(url));
    dest1 = spif_socket_new_from_urls(SPIF_NULL_TYPE(url), url1);
    spif_url_del(url1);
    TEST_FAIL_IF(SPIF_SOCKET_ISNULL(listen1));
    TEST_FAIL_IF(SPIF_SOCKET_ISNULL(dest1));
    url2 = spif_url_new_from_ptr(tmp2);
    TEST_FAIL_IF(SPIF_URL_ISNULL(url2));
    listen2 = spif_socket_new_from_urls(url2, SPIF_NULL_TYPE(url));
    dest2 = spif_socket_new_from_urls(SPIF_NULL_TYPE(url), url2);
    spif_url_del(url2);
    TEST_FAIL_IF(SPIF_SOCKET_ISNULL(listen2));
    TEST_FAIL_IF(SPIF_SOCKET_ISNULL(dest2));
    TEST_PASS();

    TEST_BEGIN("spif_socket_open() function");
    b = spif_socket_open(listen1);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_open(listen2);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_open(dest1);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_open(dest2);
    TEST_FAIL_IF(b == FALSE);
    TEST_PASS();

    TEST_BEGIN("spif_socket_set_nbio() function");
    b = spif_socket_set_nbio(listen1);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_set_nbio(listen2);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_set_nbio(dest1);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_set_nbio(dest2);
    TEST_FAIL_IF(b == FALSE);
    TEST_PASS();

    TEST_BEGIN("spif_socket_accept() function");
    src1 = spif_socket_accept(listen1);
    TEST_FAIL_IF(SPIF_SOCKET_ISNULL(src1));
    src2 = spif_socket_accept(listen2);
    TEST_FAIL_IF(SPIF_SOCKET_ISNULL(src2));
    TEST_PASS();

    TEST_BEGIN("spif_socket_send() and spif_socket_recv() functions");
    signal(SIGPIPE, SIG_IGN);
    data1 = spif_str_new_from_ptr(strdata);
    TEST_FAIL_IF(SPIF_STR_ISNULL(data1));
    b = spif_socket_send(dest1, data1);
    TEST_FAIL_IF(b == FALSE);
    data2 = spif_socket_recv(src1);
    TEST_FAIL_IF(SPIF_STR_ISNULL(data2));
    TEST_FAIL_IF(!SPIF_CMP_IS_EQUAL(spif_str_cmp(data1, data2)));
    spif_str_del(data1);
    spif_str_del(data2);
    TEST_PASS();

    TEST_BEGIN("spif_socket_del() function");
    b = spif_socket_del(listen1);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_del(listen2);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_del(src1);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_del(src2);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_del(dest1);
    TEST_FAIL_IF(b == FALSE);
    b = spif_socket_del(dest2);
    TEST_FAIL_IF(b == FALSE);
    TEST_PASS();

    TEST_PASSED("spif_socket_t");
    return 0;
}

int
test_regexp(void)
{
#if LIBAST_REGEXP_SUPPORT_PCRE
    {
        spif_charptr_t regexp_list[] = {
            SPIF_CHARPTR("cdefg"),
            SPIF_CHARPTR("^abcde"),
            SPIF_CHARPTR("efghi$"),
            SPIF_CHARPTR("^\\d+\\.\\d+\\.\\d{1,3}\\.\\d{1,3}$"),
            SPIF_CHARPTR("^([-\\w]+\\.){1,}(com|net|org|edu|info|biz|name|sex|[a-z]{2})$")
        };
        spif_str_t s;
        spif_regexp_t rexp;

        TEST_BEGIN("spif_regexp_matches_str() function");
        s = spif_str_new_from_ptr(regexp_list[0]);
        rexp = spif_regexp_new_from_str(s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefg"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cde"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("CDEFG"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[1]);
        spif_regexp_init_from_str(rexp, s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefg"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cdefghi"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cde"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("ABCDE"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[2]);
        spif_regexp_init_from_str(rexp, s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefg"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cde"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("EFGHI"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[3]);
        spif_regexp_init_from_str(rexp, s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("127.0.0.1"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4.3.7.1"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("192.168.137.111"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("812.555.1212"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("..."));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[4]);
        spif_regexp_init_from_str(rexp, s);
        spif_regexp_set_flags(rexp, SPIF_CHARPTR("i"));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("www.stuff-and-other-stuff.com"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("ETERM.ORG"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("411.biz"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR(".com"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("www.@@@.com"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_del(s);
        spif_regexp_del(rexp);

        TEST_PASS();
    }
#elif defined(LIBAST_REGEXP_SUPPORT_POSIX)
    {
        spif_charptr_t regexp_list[] = {
            SPIF_CHARPTR("cdefg"),
            SPIF_CHARPTR("^abcde"),
            SPIF_CHARPTR("efghi$"),
            SPIF_CHARPTR("^([0-9]{1,3}\\.){1,3}[0-9]{1,3}$"),
            SPIF_CHARPTR("^([-a-z0-9_]+\\.){1,}(com|net|org|edu|info|biz|name|sex|[a-z]{2})$")
        };
        spif_str_t s;
        spif_regexp_t rexp;

        TEST_BEGIN("spif_regexp_matches_str() function");
        s = spif_str_new_from_ptr(regexp_list[0]);
        rexp = spif_regexp_new_from_str(s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefg"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cde"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("CDEFG"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[1]);
        spif_regexp_init_from_str(rexp, s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefg"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cdefghi"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cde"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("ABCDE"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[2]);
        spif_regexp_init_from_str(rexp, s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefg"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cde"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("EFGHI"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[3]);
        spif_regexp_init_from_str(rexp, s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("127.0.0.1"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4.3.7.1"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("192.168.137.111"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("812.555.1212"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("..."));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[4]);
        spif_regexp_init_from_str(rexp, s);
        spif_regexp_set_flags(rexp, SPIF_CHARPTR("i"));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("www.stuff-and-other-stuff.com"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("ETERM.ORG"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("411.biz"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR(".com"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("www.@@@.com"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_del(s);
        spif_regexp_del(rexp);

        TEST_PASS();
    }
#elif defined(LIBAST_REGEXP_SUPPORT_BSD)
    {
        spif_charptr_t regexp_list[] = {
            SPIF_CHARPTR("cdefg"),
            SPIF_CHARPTR("^abcde"),
            SPIF_CHARPTR("efghi$"),
            SPIF_CHARPTR("^[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?$")
        };
        spif_str_t s;
        spif_regexp_t rexp;

        TEST_BEGIN("spif_regexp_matches_str() function");
        s = spif_str_new_from_ptr(regexp_list[0]);
        rexp = spif_regexp_new_from_str(s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefg"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cde"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("CDEFG"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[1]);
        spif_regexp_init_from_str(rexp, s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefg"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cdefghi"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cde"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("ABCDE"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[2]);
        spif_regexp_init_from_str(rexp, s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("abcdefg"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cdefghi"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("cde"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("EFGHI"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        spif_str_init_from_ptr(s, regexp_list[3]);
        spif_regexp_init_from_str(rexp, s);
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("127.0.0.1"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("4.3.7.1"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("192.168.137.111"));
        TEST_FAIL_IF(!spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("812.555.1212"));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_str_init_from_ptr(s, SPIF_CHARPTR("..."));
        TEST_FAIL_IF(spif_regexp_matches_str(rexp, s));
        spif_str_done(s);
        spif_regexp_done(rexp);

        TEST_PASS();
    }
#endif

    TEST_PASSED("spif_regexp_t");
    return 0;
}

int
main(int argc, char *argv[])
{
    int ret = 0;

    USE_VAR(argc);
    USE_VAR(argv);

    DEBUG_LEVEL = 0;

    if ((ret = test_macros()) != 0) {
        return ret;
    }
    if ((ret = test_mem()) != 0) {
        return ret;
    }
    if ((ret = test_strings()) != 0) {
        return ret;
    }
    if ((ret = test_snprintf()) != 0) {
        return ret;
    }
    if ((ret = test_options()) != 0) {
        return ret;
    }
    if ((ret = test_obj()) != 0) {
        return ret;
    }
    if ((ret = test_str()) != 0) {
        return ret;
    }
    if ((ret = test_tok()) != 0) {
        return ret;
    }
    if ((ret = test_url()) != 0) {
        return ret;
    }
    if ((ret = test_list()) != 0) {
        return ret;
    }
    if ((ret = test_vector()) != 0) {
        return ret;
    }
    if ((ret = test_map()) != 0) {
        return ret;
    }
    if ((ret = test_socket()) != 0) {
        return ret;
    }
    if ((ret = test_regexp()) != 0) {
        return ret;
    }
    if ((ret = test_hash_functions()) != 0) {
        return ret;
    }

    printf("All tests passed.\n\n");

    /*MALLOC_DUMP();*/
    return 0;
}
