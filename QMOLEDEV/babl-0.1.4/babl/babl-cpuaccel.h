/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005-2008, Øyvind Kolås and others.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, see
 * <http://www.gnu.org/licenses/>.
 */

#ifndef _BABL_CPU_ACCEL_H
#define _BABL_CPU_ACCEL_H

typedef enum
{
  BABL_CPU_ACCEL_NONE        = 0x0,

  /* x86 accelerations */
  BABL_CPU_ACCEL_X86_MMX     = 0x01000000,
  BABL_CPU_ACCEL_X86_3DNOW   = 0x40000000,
  BABL_CPU_ACCEL_X86_MMXEXT  = 0x20000000,
  BABL_CPU_ACCEL_X86_SSE     = 0x10000000,
  BABL_CPU_ACCEL_X86_SSE2    = 0x08000000,
  BABL_CPU_ACCEL_X86_SSE3    = 0x02000000,

  /* powerpc accelerations */
  BABL_CPU_ACCEL_PPC_ALTIVEC = 0x04000000
} BablCpuAccelFlags;


BablCpuAccelFlags  babl_cpu_accel_get_support (void);
void               babl_cpu_accel_set_use     (unsigned int use);


#endif  /* _BABL_CPU_ACCEL_H */
