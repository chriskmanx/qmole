////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef _RAND_H_INCLUDED
#define _RAND_H_INCLUDED

extern int random_active;

void random_init(void);
int  random_byte(void);
void random_add_noise(void *noise, int length);
void random_add_heavynoise(void *noise, int length);
void random_get_savedata(void **data, int *len);

#endif //_RAND_H_INCLUDED
