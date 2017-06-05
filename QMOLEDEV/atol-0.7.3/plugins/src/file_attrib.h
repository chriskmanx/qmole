#ifndef _FILE_ATTRIB_H
#define _FILE_ATTRIB_H

//
// define portable file attribute flags
//

#define ATTR_UNIX    0x0001    //attribute type is stored in first flag

//common flags
#define ATTR_DIR     0x0002
#define ATTR_LINK    0x0004

//UNIX only flags
#define ATTR_R_USR   0x0008
#define ATTR_W_USR   0x0010
#define ATTR_X_USR   0x0020
#define ATTR_R_GRP   0x0040
#define ATTR_W_GRP   0x0080
#define ATTR_X_GRP   0x0100
#define ATTR_R_OTH   0x0200
#define ATTR_W_OTH   0x0400
#define ATTR_X_OTH   0x0800

//MSDOS only flags
#define ATTR_RONLY   0x0008
#define ATTR_ARCH    0x0010
#define ATTR_HIDDEN  0x0020
#define ATTR_SYSTEM  0x0040

#endif
