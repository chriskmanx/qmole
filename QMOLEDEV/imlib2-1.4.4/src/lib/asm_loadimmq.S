/*\ constants and macros for x86 assembly files. \*/


/*\ Constants for asm_blend.S and asm_blend_cmod.S \*/
/* m0X000000: .byte   0,   0,   0,   0,   0,   0, 255,   0 */
#define m0X000000_H 0x00FF0000
#define m0X000000_L 0x00000000
/* m10000000: .byte   0,   0,   0,   0,   0,   0,   0,   1 */
#define m10000000_H 0x01000000
#define m10000000_L 0x00000000
/* m00XXXXXX: .byte 255, 255, 255, 255, 255, 255,   0,   0 */
#define m00XXXXXX_H 0x0000FFFF
#define m00XXXXXX_L 0xFFFFFFFF
/* mVX000000: .byte   0,   0,   0,   0,   0,   0, 255, 127 */
#define mVX000000_H 0x7FFF0000
#define mVX000000_L 0x00000000
/* mV0000000: .byte   0,   0,   0,   0,   0,   0,   0, 128 */
#define mV0000000_H 0x80000000
#define mV0000000_L 0x00000000
/* m0XXX0XXX: .byte 255, 255, 255,   0, 255, 255, 255,   0 */
#define m0XXX0XXX_H 0x00FFFFFF
#define m0XXX0XXX_L 0x00FFFFFF
/* mX000X000: .byte   0,   0,   0, 255,   0,   0,   0, 255 */
#define mX000X000_H 0xFF000000
#define mX000X000_L 0xFF000000
/* m10001000: .byte   0,   0,   0,   1,   0,   0,   0,   1 */
#define m10001000_H 0x01000000
#define m10001000_L 0x01000000
/* m000V0V0V: .byte 127,   0, 127,   0, 127,   0,   0,   0 */
#define m000V0V0V_H 0x0000007F
#define m000V0V0V_L 0x007F007F
/* mI0000000: .byte   0,   0,   0,   0,   0,   0,   0,  64 */
#define mI0000000_H 0x40000000
#define mI0000000_L 0x00000000
/* m0VVV0VVV: .byte 127, 127, 127,   0, 127, 127, 127,   0 */
#define m0VVV0VVV_H 0x007F7F7F
#define m0VVV0VVV_L 0x007F7F7F
/* c1: .word 0x1, 0x1, 0x1, 0x1 */
#define c1_H 0x00010001
#define c1_L 0x00010001


/*\ Constants for asm_rgba.S \*/
/* m_rb: .long 0x00f800f8, 0x00f800f8 */
#define m_rb_H 0x00f800f8
#define m_rb_L 0x00f800f8
/* m_r:  .long 0xf800f800, 0xf800f800 */
#define m_r_H 0xf800f800
#define m_r_L 0xf800f800
/* m_g6: .long 0x0000fc00, 0x0000fc00 */
#define m_g6_H 0x0000fc00
#define m_g6_L 0x0000fc00
/* m_g5: .long 0x0000f800, 0x0000f800 */
#define m_g5_H 0x0000f800
#define m_g5_L 0x0000f800
/*\ Multiply constants to fake two shifts at once \*/
/* mul_rgb565: .long 0x20000004, 0x20000004 */
#define mul_rgb565_H 0x20000004
#define mul_rgb565_L 0x20000004
/* mul_bgr565: .long 0x00042000, 0x00042000 */
#define mul_bgr565_H 0x00042000
#define mul_bgr565_L 0x00042000
/* mul_rgb555: .long 0x20000008, 0x20000008 */
#define mul_rgb555_H 0x20000008
#define mul_rgb555_L 0x20000008
/* mul_bgr555: .long 0x00082000, 0x00082000 */
#define mul_bgr555_H 0x00082000
#define mul_bgr555_L 0x00082000


/*\ Load an 8-byte constant to an mmx register \*/
#define LOAD_IMMQ(mask, reg)		\
	pushl	$mask##_H		;\
	pushl	$mask##_L		;\
	movq	(%esp), reg
#define CLEANUP_IMMQ_LOADS(num_loaded)	\
	addl	$ num_loaded * 8, %esp
