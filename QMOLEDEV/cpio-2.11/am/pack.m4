# This file is part of GNU cpio
# Copyright (C) 2009 Free Software Foundation
#
# GNU cpio is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU cpio is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU cpio.  If not, see <http://www.gnu.org/licenses/>.

# CPIO_PACKED_STRUCTS is based on code from ClamAV
AC_DEFUN([CPIO_PACKED_STRUCTS],[
  dnl check for __attribute__((packed))
  AC_MSG_CHECKING([for structure packing via __attribute__((packed))])
  AC_CACHE_VAL(cpio_cv_have_attrib_packed,[
    AC_TRY_COMPILE(,
      [struct { int i __attribute__((packed)); } s; ],
      [have_attrib_packed=yes],
      [have_attrib_packed=no])
    ])
  AC_MSG_RESULT($have_attrib_packed)

  if test "$have_attrib_packed" = no; then
    AC_MSG_CHECKING(for structure packing via pragma)
    AC_CACHE_VAL(cpio_cv_have_pragma_pack,[
      AC_TRY_RUN(CPIO_FLUSHLEFT([
                 int main(int argc, char **argv) {
                 #pragma pack(1)               
                    struct { char c; long l; } s;
                    return sizeof(s)==sizeof(s.c)+sizeof(s.l) ? 0:1; } ]),
                 [have_pragma_pack=yes],
                 [have_pragma_pack=no])
      ])
    AC_MSG_RESULT($have_pragma_pack)
    if test "$have_pragma_pack" = yes; then
      AC_DEFINE(HAVE_PRAGMA_PACK, 1, "pragma pack")
    else
      AC_MSG_CHECKING(for structure packing via hppa/hp-ux pragma)
      AC_CACHE_VAL(cpio_cv_have_pragma_pack_hpux,[
        AC_TRY_RUN(CPIO_FLUSHLEFT([
                   /* hppa/hp-ux wants pragma outside of function */
                   #pragma pack 1
                   struct { char c; long l; } s;
                   int main(int argc, char **argv) {
                      return sizeof(s)==sizeof(s.c)+sizeof(s.l) ? 0:1; } ]),
                   [have_pragma_pack_hpux=yes],
                   [have_pragma_pack_hpux=no])
        ])
      AC_MSG_RESULT($have_pragma_pack_hpux)
      AC_DEFINE(HAVE_PRAGMA_PACK_HPPA, 1, "pragma pack hppa/hp-ux style")
    fi
  fi

  if test "${have_attrib_packed}${have_pragma_pack}$have_pragma_pack_hpux" = "nonono"; then
    AC_MSG_ERROR(Need to know how to pack structures with this compiler)
  fi

  if test "$have_attrib_packed" = yes; then
    AC_DEFINE(HAVE_ATTRIB_PACKED, 1, [attrib packed])
  fi
])
