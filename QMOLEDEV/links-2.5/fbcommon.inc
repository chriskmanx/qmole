/* n is in bytes. dest must begin on pixel boundary. If n is not a whole number
 * of pixels, rounding is performed downwards.
 * if bmpixelsize is 1, no alignment is required.
 * if bmpixelsize is 2, dest must be aligned to 2 bytes.
 * if bmpixelsize is 3, no alignment is required.
 * if bmpixelsize is 4, dest must be aligned to 4 bytes.
 * -- The following do not occur, this is only for forward compatibility.
 * if bmpixelsize is 5, no alignment is required.
 * if bmpixelsize is 6, dest must be aligned to 2 bytes.
 * if bmpixelsize is 7, no alignment is required.
 * if bmpixelsize is 8, dest must be aligned to 8 bytes.
 */

static inline void pixel_set(unsigned char *dest, int n,void * pattern)
{
	switch(fb_pixelsize)
	{
		case 1:
		memset(dest,*(char *)pattern,n);
		break;

		case 2:
		{
#ifdef t2c
			t2c v=*(t2c *)do_not_optimize_here(pattern);
#ifdef __ICC
			volatile	/* ICC has an optimization bug here */
#endif
			int a;

			if ((v & 255) == ((v >> 8) & 255)) {
				memset(dest, v, n);
			} else {
#ifdef t8c
				t8c vvvv=((t8c)v << 48) | ((t8c)v << 32) | ((t8c)v << 16) | v;
#elif defined(t4c)
				t4c vv=((t4c)v << 16) | v;
#endif
				a = n >> 1;
				while (a) {
#ifdef t8c
					if (!((unsigned long)dest & 7) && a >= 4) {
						do {
							*((t8c *)dest) = vvvv;
							dest += 8;
							a -= 4;
						} while (a >= 4);
					} else
#elif defined(t4c)
					if (!((unsigned long)dest & 3) && a >= 2) {
						do {
							*((t4c *)dest) = vv;
							dest += 4;
							a -= 2;
						} while (a >= 2);
					} else
#endif
					{
						*((t2c *)dest) = v;
						dest += 2;
						a--;
					}
				}
			}
#else
			unsigned char a,b;
			int i;

			a=*(char*)pattern;
			b=((char*)pattern)[1];
			if (a == b) memset(dest, a, n);
			else for (i=0;i<=n-2;i+=2){
				dest[i]=a;
				dest[i+1]=b;
			}
#endif
		}
		break;

		case 3:
		{
			unsigned char a,b,c;
			int i;

			a=*(char*)pattern;
			b=((char*)pattern)[1];
			c=((char*)pattern)[2];
			if (a == b && b == c) memset(dest, a, n);
			else for (i=0;i<=n-3;i+=3){
				dest[i]=a;
				dest[i+1]=b;
				dest[i+2]=c;
			}
		}
		break;

		case 4:
		{
#ifdef t4c
			t4c v=*(t4c *)do_not_optimize_here(pattern);
#ifdef __ICC
			volatile	/* ICC has an optimization bug here */
#endif
			int a;

			{
#ifdef t8c
				t8c vv = ((t8c)v << 32) | v;
#endif
				a = n >> 2;
				while (a) {
#ifdef t8c
					if (!((unsigned long)dest & 7) && a >= 2) {
						do {
							*((t8c *)dest) = vv;
							dest += 8;
							a -= 2;
						} while (a >= 2);
					} else
#endif
					{
						*(t4c *)dest = v;
						dest += 4;
						a--;
					}
				}
			}
#else
			unsigned char a,b,c,d;
			int i;

			a=*(char*)pattern;
			b=((char*)pattern)[1];
			c=((char*)pattern)[2];
			d=((char*)pattern)[3];
			for (i=0;i<=n-4;i+=4){
				dest[i]=a;
				dest[i+1]=b;
				dest[i+2]=c;
				dest[i+3]=d;
			}
#endif
		}
		break;

		default:
		{
			int a;
			for (a=0;a<n/fb_pixelsize;a++,dest+=fb_pixelsize) memcpy(dest,pattern,fb_pixelsize);
		}
		break;
	}
}

