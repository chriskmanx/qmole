/*
  Methods to Read/Write/Invoke Delegates.
*/
#ifndef _DELEGATES_H
#define _DELEGATES_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/*
  Delegate structure definitions.
*/
typedef struct _DelegateInfo
{
  char
    decode_tag[MaxTextExtent],
    encode_tag[MaxTextExtent],
    *commands;

  int
    direction;

  struct _DelegateInfo
    *previous,
    *next;
} DelegateInfo;

/*
  Exported delegate methods.
*/
Export char
  *GetDelegateCommand(const ImageInfo *,const Image *,const char *,
    const char *);

Export DelegateInfo
  *SetDelegateInfo(DelegateInfo *);

Export unsigned int
  GetDelegateInfo(const char *,const char *,DelegateInfo *),
  InvokeDelegate(const ImageInfo *,Image *,const char *,const char *);

Export void
  DestroyDelegateInfo(void),
  ListDelegateInfo(FILE *);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
