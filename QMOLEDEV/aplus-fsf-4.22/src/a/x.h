#ifndef included_a_x_h
#define included_a_x_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#define A_ 0
#define IA 1
#define FA 2
#define CA 3

#define P_ 4
#define IP 5
#define FP 6
#define CP 7

#define V_ 8
#define IV 9
#define FV 10
#define CV 11

#define U_ 12
#define IU 13
#define FU 14
#define CU 15

/*
   _ - any    I - integer F - double C - character
   A - array  P - pointer V - value  U - unique array

 (A,C*,I,V)foo(A,IA,FA,CA,*,I*,F*,C*,I,F,C,U,IU,FU,CU)

return V_ void
 xinstall(fun,"sym",result_type,num_args,arg_type[s],doc) 
   if num_args is negative then any number more than that 
   if arg_type is -1 then pass stack and number
   doc should be char string or NULL (0).
*/

#endif
