/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%        SSSSS  IIIII   GGGG  N   N   AAA   TTTTT  U   U  RRRR   EEEEE        %
%        SS       I    G      NN  N  A   A    T    U   U  R   R  E            %
%         SSS     I    G  GG  N N N  AAAAA    T    U   U  RRRR   EEE          %
%           SS    I    G   G  N  NN  A   A    T    U   U  R R    E            %
%        SSSSS  IIIII   GGG   N   N  A   A    T     UUU   R  R   EEEEE        %
%                                                                             %
%                                                                             %
%             Methods to Compute a Digital Signature for an Image             %
%                                                                             %
%                                                                             %
%                             Software Design                                 %
%                               John Cristy                                   %
%                              December 1992                                  %
%                                                                             %
%                                                                             %
%  Copyright 1999 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission is hereby granted, free of charge, to any person obtaining a    %
%  copy of this software and associated documentation files ("ImageMagick"),  %
%  to deal in ImageMagick without restriction, including without limitation   %
%  the rights to use, copy, modify, merge, publish, distribute, sublicense,   %
%  and/or sell copies of ImageMagick, and to permit persons to whom the       %
%  ImageMagick is furnished to do so, subject to the following conditions:    %
%                                                                             %
%  The above copyright notice and this permission notice shall be included in %
%  all copies or substantial portions of ImageMagick.                         %
%                                                                             %
%  The software is provided "as is", without warranty of any kind, express or %
%  implied, including but not limited to the warranties of merchantability,   %
%  fitness for a particular purpose and noninfringement.  In no event shall   %
%  E. I. du Pont de Nemours and Company be liable for any claim, damages or   %
%  other liability, whether in an action of contract, tort or otherwise,      %
%  arising from, out of or in connection with ImageMagick or the use or other %
%  dealings in ImageMagick.                                                   %
%                                                                             %
%  Except as contained in this notice, the name of the E. I. du Pont de       %
%  Nemours and Company shall not be used in advertising or otherwise to       %
%  promote the sale, use or other dealings in ImageMagick without prior       %
%  written authorization from the E. I. du Pont de Nemours and Company.       %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Routine SignatureImage computes a digital signature from the image
%  pixels.  This signature uniquely identifies the image.  The digital
%  signature is from RSA Data Security MD5 Digest Algorithm described in
%  Internet draft [MD5], July 1992.
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "defines.h"

/*
  Typedef declarations.
*/
typedef struct _MessageDigest
{
  unsigned long
    number_bits[2],
    accumulator[4];

  unsigned char
    message[64],
    digest[16];
} MessageDigest;

/*
  Method prototypes.
*/
static void
  TransformMessageDigest(MessageDigest *,unsigned long *),
  UpdateMessageDigest(MessageDigest *,const unsigned char *,
    const unsigned long);

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   C o m p u t e M e s s a g e D i g e s t                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ComputeMessageDigest computes the message digest.
%
%  The format of the ComputeMessageDigest method is:
%
%      void SignatureImage(Image *image)
%
%  A description of each parameter follows:
%
%    o message_digest: The address of a structure of type MessageDigest.
%
%
*/
static void ComputeMessageDigest(MessageDigest *message_digest)
{
  int
    number_bytes;

  register unsigned char
    *p;

  register unsigned int
    i;

  unsigned char
    padding[64];

  unsigned long
    message[16],
    padding_length;

  /*
    Save number of bits.
  */
  message[14]=message_digest->number_bits[0];
  message[15]=message_digest->number_bits[1];
  /*
    Compute number of bytes mod 64.
  */
  number_bytes=(int) ((message_digest->number_bits[0] >> 3) & 0x3F);
  /*
    Pad message to 56 mod 64.
  */
  padding_length=(number_bytes < 56) ? (56-number_bytes) : (120-number_bytes);
  padding[0]=0x80;
  for (i=1; i < padding_length; i++)
    padding[i]=(char) 0;
  UpdateMessageDigest(message_digest,padding,padding_length);
  /*
    Append length in bits and transform.
  */
  p=message_digest->message;
  for (i=0; i < 14; i++)
  {
    message[i]=(unsigned long) (*p++);
    message[i]|=((unsigned long) (*p++)) << 8;
    message[i]|=((unsigned long) (*p++)) << 16;
    message[i]|=((unsigned long) (*p++)) << 24;
  }
  TransformMessageDigest(message_digest,message);
  /*
    Store message in digest.
  */
  p=message_digest->digest;
  for (i=0; i < 4; i++)
  {
    *p++=(unsigned char) (message_digest->accumulator[i] & 0xff);
    *p++=(unsigned char) ((message_digest->accumulator[i] >> 8) & 0xff);
    *p++=(unsigned char) ((message_digest->accumulator[i] >> 16) & 0xff);
    *p++=(unsigned char) ((message_digest->accumulator[i] >> 24) & 0xff);
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   I n i t i a l i z e M e s s a g e D i g e s t                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method InitializeMessageDigest initializes the message digest structure.
%
%  The format of the InitializeMessageDigest method is:
%
%      InitializeMessageDigest(message_digest)
%
%  A description of each parameter follows:
%
%    o message_digest: The address of a structure of type MessageDigest.
%
%
*/
static void InitializeMessageDigest(MessageDigest *message_digest)
{
  message_digest->number_bits[0]=(unsigned long) 0;
  message_digest->number_bits[1]=(unsigned long) 0;
  /*
    Load magic initialization constants.
  */
  message_digest->accumulator[0]=(unsigned long) 0x67452301;
  message_digest->accumulator[1]=(unsigned long) 0xefcdab89;
  message_digest->accumulator[2]=(unsigned long) 0x98badcfe;
  message_digest->accumulator[3]=(unsigned long) 0x10325476;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   T r a n s f o r m M e s s a g e D i g e s t                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method TransformMessageDigest updates the message digest.
%
%  The format of the TransformMessageDigest method is:
%
%      TransformMessageDigest(message_digest,message)
%
%  A description of each parameter follows:
%
%    o message_digest: The address of a structure of type MessageDigest.
%
%
*/
static void TransformMessageDigest(MessageDigest *message_digest,
  unsigned long *message)
{
#define F(x,y,z)  (((x) & (y)) | ((~x) & (z)))
#define G(x,y,z)  (((x) & (z)) | ((y) & (~z)))
#define H(x,y,z)  ((x) ^ (y) ^ (z))
#define I(x,y,z)  ((y) ^ ((x) | (~z)))
#define RotateLeft(x,n)  (((x) << (n)) | (((x) & 0xffffffff) >> (32-(n))))

  static const unsigned long
    additive_constant[64]=  /* 4294967296*abs(sin(i)), i in radians */
    {
      0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf,
      0x4787c62a, 0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af,
      0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e,
      0x49b40821, 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
      0xd62f105d, 0x2441453,  0xd8a1e681, 0xe7d3fbc8, 0x21e1cde6,
      0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8,
      0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122,
      0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
      0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x4881d05,  0xd9d4d039,
      0xe6db99e5, 0x1fa27cf8, 0xc4ac5665, 0xf4292244, 0x432aff97,
      0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d,
      0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
      0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
    };

  register int
    i;

  register unsigned int
    j;

  register unsigned long
    a,
    b,
    c,
    d;

  register const unsigned long
    *p;

  /*
    Save accumulator.
  */
  a=message_digest->accumulator[0];
  b=message_digest->accumulator[1];
  c=message_digest->accumulator[2];
  d=message_digest->accumulator[3];
  /*
    a=b+((a+F(b,c,d)+X[k]+t) <<< s).
  */
  p=additive_constant;
  j=0;
  for (i=0; i < 4; i++)
  {
    a+=F(b,c,d)+message[j & 0x0f]+(*p++);
    a=RotateLeft(a,7)+b;
    j++;
    d+=F(a,b,c)+message[j & 0x0f]+(*p++);
    d=RotateLeft(d,12)+a;
    j++;
    c+=F(d,a,b)+message[j & 0x0f]+(*p++);
    c=RotateLeft(c,17)+d;
    j++;
    b+=F(c,d,a)+message[j & 0x0f]+(*p++);
    b=RotateLeft(b,22)+c;
    j++;
  }
  /*
    a=b+((a+G(b,c,d)+X[k]+t) <<< s).
  */
  j=1;
  for (i=0; i < 4; i++)
  {
    a+=G(b,c,d)+message[j & 0x0f]+(*p++);
    a=RotateLeft(a,5)+b;
    j+=5;
    d+=G(a,b,c)+message[j & 0x0f]+(*p++);
    d=RotateLeft(d,9)+a;
    j+=5;
    c+=G(d,a,b)+message[j & 0x0f]+(*p++);
    c=RotateLeft(c,14)+d;
    j+=5;
    b+=G(c,d,a)+message[j & 0x0f]+(*p++);
    b=RotateLeft(b,20)+c;
    j+=5;
  }
  /*
    a=b+((a+H(b,c,d)+X[k]+t) <<< s).
  */
  j=5;
  for (i=0; i < 4; i++)
  {
    a+=H(b,c,d)+message[j & 0x0f]+(*p++);
    a=RotateLeft(a,4)+b;
    j+=3;
    d+=H(a,b,c)+message[j & 0x0f]+(*p++);
    d=RotateLeft(d,11)+a;
    j+=3;
    c+=H(d,a,b)+message[j & 0x0f]+(*p++);
    c=RotateLeft(c,16)+d;
    j+=3;
    b+=H(c,d,a)+message[j & 0x0f]+(*p++);
    b=RotateLeft(b,23)+c;
    j+=3;
  }
  /*
    a=b+((a+I(b,c,d)+X[k]+t) <<< s).
  */
  j=0;
  for (i=0; i < 4; i++)
  {
    a+=I(b,c,d)+message[j & 0x0f]+(*p++);
    a=RotateLeft(a,6)+b;
    j+=7;
    d+=I(a,b,c)+message[j & 0x0f]+(*p++);
    d=RotateLeft(d,10)+a;
    j+=7;
    c+=I(d,a,b)+message[j & 0x0f]+(*p++);
    c=RotateLeft(c,15)+d;
    j+=7;
    b+=I(c,d,a)+message[j & 0x0f]+(*p++);
    b=RotateLeft(b,21)+c;
    j+=7;
  }
  /*
    Increment accumulator.
  */
  message_digest->accumulator[0]+=a;
  message_digest->accumulator[1]+=b;
  message_digest->accumulator[2]+=c;
  message_digest->accumulator[3]+=d;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   U p d a t e M e s s a g e D i g e s t                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method UpdateMessageDigest updates the message digest.
%
%  The format of the UpdateMessageDigest method is:
%
%      UpdateMessageDigest(message_digest,input_message,message_length)
%
%  A description of each parameter follows:
%
%    o message_digest: The address of a structure of type MessageDigest.
%
%
*/
static void UpdateMessageDigest(MessageDigest *message_digest,
  const unsigned char *input_message,const unsigned long message_length)
{
  register unsigned char
    *p;

  register unsigned int
    i,
    j;

  unsigned long
    message[16],
    number_bits,
    number_bytes;

  /*
    Compute number of bits and bytes.
  */
  number_bytes=(long) ((message_digest->number_bits[0] >> 3) & 0x3F);
  number_bits=message_digest->number_bits[0]+(message_length << 3);
  if ((number_bits & 0xffffffff) < message_digest->number_bits[0])
    message_digest->number_bits[1]++;
  message_digest->number_bits[0]+=message_length << 3;
  message_digest->number_bits[1]+=message_length >> 29;
  for (i=0; i < message_length; i++)
  {
    /*
      Add new character to message.
    */
    message_digest->message[number_bytes++]=(*input_message++);
    if (number_bytes == 0x40)
      {
        /*
          Transform message digest 64 bytes at a time.
        */
        p=message_digest->message;
        for (j=0; j < 16; j++)
        {
          message[j]=(unsigned long) (*p++);
          message[j]|=((unsigned long) (*p++)) << 8;
          message[j]|=((unsigned long) (*p++)) << 16;
          message[j]|=((unsigned long) (*p++)) << 24;
        }
        TransformMessageDigest(message_digest,message);
        number_bytes=0;
      }
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S i g n a t u r e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SignatureImage computes a digital signature from an image.  This
%  signature uniquely identifies the image and is convenient for determining
%  if the colormap of a sequence of images is identical when animating.  The
%  digital signature is from RSA Data Security MD5 Digest Algorithm described
%  in Internet draft [MD5], July 1992.
%
%  The format of the SignatureImage method is:
%
%      SignatureImage(image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
%
*/
Export void SignatureImage(Image *image)
{
  const char
    hex[] = "0123456789abcdef";

  int
    x;

  MessageDigest
    message_digest;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  register unsigned char
    *q;

  unsigned char
    *message;

  unsigned short
    value;

  assert(image != (Image *) NULL);
  if (image->pixels == (RunlengthPacket *) NULL)
    return;
  /*
    Allocate memory for digital signature.
  */
  if (image->signature != (char *) NULL)
    FreeMemory((char *) image->signature);
  image->signature=(char *) AllocateMemory(33*sizeof(char));
  message=(unsigned char *)
    AllocateMemory(image->columns*sizeof(RunlengthPacket));
  if ((image->signature == (char *) NULL) ||
      (message == (unsigned char *) NULL))
    {
      MagickWarning(ResourceLimitWarning,"Unable to compute digital signature",
        "Memory allocation failed");
      return;
    }
  /*
    Compute image digital signature.
  */
  InitializeMessageDigest(&message_digest);
  x=0;
  p=image->pixels;
  q=message;
  for (i=0; i < (int) image->packets; i++)
  {
    for (j=0; j <= ((int) p->length); j++)
    {
      WriteQuantum(p->red,q);
      WriteQuantum(p->green,q);
      WriteQuantum(p->blue,q);
      if (image->matte)
        WriteQuantum(p->index,q);
      x++;
      if (x == (int) image->columns)
        {
          UpdateMessageDigest(&message_digest,message,q-message);
          q=message;
          x=0;
        }
    }
    p++;
  }
  FreeMemory((char *) message);
  /*
    Convert digital signature to a 32 character hex string.
  */
  ComputeMessageDigest(&message_digest);
  q=(unsigned char *) image->signature;
  for (i=0; i < 16; i++)
  {
    *q++=hex[(message_digest.digest[i] >> 4) & 0xf];
    *q++=hex[message_digest.digest[i] & 0xf];
  }
  *q='\0';
}
