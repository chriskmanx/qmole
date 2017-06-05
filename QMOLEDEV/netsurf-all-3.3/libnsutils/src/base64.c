/*
 * Copyright 2014 Vincent Sanders <vince@netsurf-browser.org>
 *
 * This file is part of libnsutils.
 *
 * Licensed under the MIT License,
 *                http://www.opensource.org/licenses/mit-license.php
 */

/**
 * \file
 * Base64 encoding and decoding implementation.
 *
 * Implements RFC4648 (https://tools.ietf.org/html/rfc4648)
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "nsutils/base64.h"

static uint8_t decoding_table[256];
static uint8_t encoding_table[] = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
                                'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
                                'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                                'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
                                'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
                                'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                                'w', 'x', 'y', 'z', '0', '1', '2', '3',
                                '4', '5', '6', '7', '8', '9', '+', '/'};
static unsigned int mod_table[] = {0, 2, 1};

/* exported interface documented in nsutils/base64.h */
nsuerror nsu_base64_encode_alloc(const uint8_t *input,
                            size_t input_length,
                            uint8_t **output,
                            size_t *output_length)
{
        uint8_t *encoded;
        size_t encoded_len;
        size_t i; /* input index */
        size_t j; /* output index */

        encoded_len = 4 * ((input_length + 2) / 3);

        encoded = malloc(encoded_len);
        if (encoded == NULL) {
                return NSUERROR_NOMEM;
        }

        for (i = 0, j = 0; i < input_length;) {

                uint32_t octet_a = i < input_length ? input[i++] : 0;
                uint32_t octet_b = i < input_length ? input[i++] : 0;
                uint32_t octet_c = i < input_length ? input[i++] : 0;

                uint32_t triple = (octet_a << 0x10) + (octet_b << 0x08) + octet_c;

                encoded[j++] = encoding_table[(triple >> 3 * 6) & 0x3F];
                encoded[j++] = encoding_table[(triple >> 2 * 6) & 0x3F];
                encoded[j++] = encoding_table[(triple >> 1 * 6) & 0x3F];
                encoded[j++] = encoding_table[(triple >> 0 * 6) & 0x3F];
        }

        for (i = 0; i < mod_table[input_length % 3]; i++) {
                encoded[encoded_len - 1 - i] = '=';
        }

        *output = encoded;
        *output_length = encoded_len;

        return NSUERROR_OK;
}


/* exported interface documented in nsutils/base64.h */
nsuerror nsu_base64_decode_alloc(const uint8_t *input,
                            size_t input_length,
                            uint8_t **output,
                            size_t *output_length)
{
        static bool decode_initialised = false;
        uint8_t *decoded;
        size_t decoded_len;
        size_t idx;
        size_t opidx;
        uint8_t sextet[4];
        int sextet_idx;

        if (!decode_initialised) {
                memset(decoding_table, 0xff, sizeof(decoding_table));
                for (idx = 0; idx < 64; idx++) {
                        decoding_table[encoding_table[idx]] = idx;
                }
                decoding_table['='] = 64;
                decode_initialised = true;
        }

        decoded_len = ((input_length + 3) / 4) * 3;
        if (input[input_length - 1] == '=') (decoded_len)--;
        if (input[input_length - 2] == '=') (decoded_len)--;

        decoded = malloc(decoded_len);
        if (decoded == NULL) {
                return NSUERROR_NOMEM;
        }

        sextet_idx = 0;
        idx = 0;
        opidx = 0;
        while (idx < input_length) {
                sextet[sextet_idx] = decoding_table[input[idx++]];
                if (sextet[sextet_idx] >= 64) {
                        /* not in encoding set */
                        if (sextet[sextet_idx] == 64) {
                                break; /* pad recived - input complete */
                        }
                } else {
                        sextet_idx++;
                        if (sextet_idx == 4) {
                                if (opidx >= (decoded_len - 3)) {
                                        break; /* insufficient output buffer space */
                                }
                                decoded[opidx++] = (sextet[0] << 2) | (sextet[1] >> 4);
                                decoded[opidx++] = (sextet[1] << 4) | (sextet[2] >> 2);
                                decoded[opidx++] = (sextet[2] << 6) | (sextet[3]);

                                sextet_idx = 0;
                        }
                }
        }

        /* deal with any remaining recived bytes ensuring output buffer is not overrun */
        switch (sextet_idx) {
        case 1:
                if (opidx < decoded_len) {
                        decoded[opidx++] = (sextet[0] << 2);
                }
                break;

        case 2:
                if (opidx < decoded_len) {
                        decoded[opidx++] = (sextet[0] << 2) | (sextet[1] >> 4);
                }
                if (opidx < decoded_len) {
                        decoded[opidx++] = (sextet[1] << 4);
                }
                break;

        case 3:
                if (opidx < decoded_len) {
                        decoded[opidx++] = (sextet[0] << 2) | (sextet[1] >> 4);
                }
                if (opidx < decoded_len) {
                        decoded[opidx++] = (sextet[1] << 4) | (sextet[2] >> 2);
                }
                if (opidx < decoded_len) {
                        decoded[opidx++] = (sextet[2] << 6);
                }
                break;

        case 4:
                if (opidx < decoded_len) {
                        decoded[opidx++] = (sextet[0] << 2) | (sextet[1] >> 4);
                }
                if (opidx < decoded_len) {
                        decoded[opidx++] = (sextet[1] << 4) | (sextet[2] >> 2);
                }
                if (opidx < decoded_len) {
                        decoded[opidx++] = (sextet[2] << 6) | (sextet[3]);
                }
                break;
        }

        *output = decoded;
        *output_length = opidx;

        return NSUERROR_OK;
}
