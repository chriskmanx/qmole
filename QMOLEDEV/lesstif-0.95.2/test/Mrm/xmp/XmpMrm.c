/*
 *
 * XmpMrm.c - Xmp widgets Mrm registration.
 *
 */

#include <Mrm/MrmPublic.h>
#include <Blob.h>
#include <String.h>
#include <Grid.h>

/*
 * XmpMrmInitialize - register Xmp widget classes with Mrm
 */

int XmpMrmInitialize()
{
    MrmRegisterClass (MrmwcUnknown, "XmpBlob",
			"XmpCreateBlob", XmpCreateBlob,
			xmpBlobWidgetClass);
    MrmRegisterClass (MrmwcUnknown, "XmpString",
			"XmpCreateString", XmpCreateString,
			xmpStringWidgetClass);
    MrmRegisterClass (MrmwcUnknown, "XmpGrid",
			"XmpCreateGrid", XmpCreateGrid,
			xmpGridWidgetClass);
    return (0);
}

