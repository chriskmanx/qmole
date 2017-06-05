////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Flags defining different user selection in COverwriteDlg and CDeleteDlg (soon :)
////////////////////////////////////////////////////////////////////////////

#ifndef _OPCODES_H__
#define _OPCODES_H__

//supported operations
#define OP_COPY      1
#define OP_MOVE      2
#define OP_DELETE    3
#define OP_MKDIR     4
#define OP_RENAME    5
#define OP_PACK      6
#define OP_UNPACK    7
#define OP_ENCRYPT   8
#define OP_DECRYPT   9
#define OP_SPLIT     10
#define OP_MERGE     11
#define OP_HASH      12
#define OP_CONNECT   13

//NOTE: part of the code flags are ment for current file only 
//        and the others are ment for all files forward inside the same file operation
#define OPF_TMP_FLAGS_MASK        0x007F    // mask for temporary flags

//single file operation settings
#define OPF_OK                    0x0001
#define OPF_ABORT                0x0002
#define OPF_SKIP                0x0004    // 
#define OPF_OVERWRITE            0x0008    //
#define OPF_CPY_RENAME            0x0010
#define OPF_CPY_APPEND            0x0020
#define OPF_CPY_RESUME            0x0040
#define OPF_DELETE                0x0080
#define OPF_RETRY                0x0100

//multiple file operation settings
#define OPF_DEL_SKIP_ALL_DIRS       0x0100   // TOFIX not used? skip deleting all non-empty dirs ("Dir is not empty?")
#define OPF_DEL_ALL_DIRS            0x0200   // delete all non-empty dirs without asking ("Dir is not empty?")
#define OPF_DEL_ALL_RO_FILES        0x0400   // delete all read-only files without asking
#define OPF_CPY_OVERWRITE_ALL       0x0800   //
#define OPF_CPY_SKIP_ALL            0x1000   // skip overwriting file
#define OPF_CPY_OVERWRITE_ALL_OLDER 0x2000
#define OPF_CPY_ERR_SKIP_ALL	    0x4000	 // skip asking to abort when failed to copy file
#define OPF_DEL_SKIP_ON_ERR         0x8000   // do not notify error when delete fails

#endif // _OPCODES_H__


