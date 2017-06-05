/*
    the our_* routines are implementations for the corresponding library
    routines. for a while, i tried to actually name them wctomb etc
    but stopped that after i found a system which made wchar_t an
    unsigned char.
*/
int our_wctomb (char *s, U16 wc);
int our_mbtowc (U16 * p, char *s, unsigned n);
