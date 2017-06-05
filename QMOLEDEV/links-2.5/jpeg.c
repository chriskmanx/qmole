/* jpeg.c
 * JPEG decoding
 * (c) 2002 Karel 'Clock' Kulhavy
 * This file is a part of the Links program, released under GPL.
 */

#include "cfg.h"

#ifdef G
#include "links.h"

#ifdef HAVE_JPEG
#include <jpeglib.h>

#if BITS_IN_JSAMPLE != 8
#error "You have a weird jpeglib compiled for 12 bits per sample that is not able to read ordinary JPEG's. \
See INSTALL for description how to compile Links with jpeglib statically to supply your own \"good\" version \
of jpeglib or reinstall your system's jpeglib to be a normal one."
#endif /* #if BITS_IN_JSAMPLE != 8 */

struct jerr_struct{
	struct jpeg_error_mgr pub;
	jmp_buf setjmp_buffer;
};

static struct jerr_struct *global_jerr;
static struct jpeg_decompress_struct *global_cinfo;
static int mesg_unsup_emitted; /* Defaults to zero at program startup and once
				* set is never reset back to zero */


METHODDEF(void) my_error_exit(j_common_ptr cinfo)
{
	longjmp(global_jerr->setjmp_buffer,2);
}

METHODDEF(void) /* Only for the sake of libjpeg */
nop(j_decompress_ptr cinfo)
{
}

METHODDEF(void)
my_output_message(j_common_ptr cinfo)
{
}

METHODDEF(boolean) my_fill_input_buffer(j_decompress_ptr cinfo)
{
 return FALSE; /* We utilize I/O suspension (or emulsion? ;-) ) */
}

METHODDEF(void) my_skip_input_data(j_decompress_ptr cinfo,long num_bytes)
{
	if ((unsigned long)num_bytes>cinfo->src->bytes_in_buffer)
	{
	 	/* We have to enter skipping state */
	 	cinfo->src->next_input_byte+=cinfo->src->bytes_in_buffer;
		((struct jpg_decoder *)(global_cimg->decoder))->skip_bytes
  			=num_bytes-cinfo->src->bytes_in_buffer;
  		cinfo->src->bytes_in_buffer=0;
	}
	else
	{
		/* We only pull out some bytes from buffer. */
		cinfo->src->next_input_byte+=num_bytes;
		cinfo->src->bytes_in_buffer-=num_bytes;
	}
}

void jpeg_start(struct cached_image *cimg)
{
	struct jpg_decoder *jd;

	global_cinfo=mem_alloc(sizeof(*global_cinfo));
	global_jerr=mem_alloc(sizeof(*global_jerr));
	global_cinfo->err = jpeg_std_error(&(global_jerr->pub));
	global_jerr->pub.error_exit=my_error_exit;
	global_jerr->pub.output_message=my_output_message;
	if (setjmp(global_jerr->setjmp_buffer)){
g19_2000:
		mem_free(global_cinfo);
		mem_free(global_jerr);
		img_end(cimg);
		return;
	}
	jpeg_create_decompress(global_cinfo);
	if (setjmp(global_jerr->setjmp_buffer)){
		jpeg_destroy_decompress(global_cinfo);
		goto g19_2000;
		return;
	}
	jpeg_stdio_src(global_cinfo,stdin);
	global_cinfo->src->init_source=&nop;
	global_cinfo->src->fill_input_buffer=&my_fill_input_buffer;
	global_cinfo->src->skip_input_data=&my_skip_input_data;
	global_cinfo->src->resync_to_restart=&jpeg_resync_to_restart;
	global_cinfo->src->term_source=nop;
	global_cinfo->src->bytes_in_buffer=0;
	global_cinfo->src->next_input_byte=NULL;
	cimg->decoder=mem_alloc(sizeof(struct jpg_decoder));
	jd=(struct jpg_decoder *)cimg->decoder;
	jd->cinfo=global_cinfo;
	jd->jerr=global_jerr;
	jd->state=0;
	jd->skip_bytes=0;
	jd->jdata=NULL;
	/* Scanlines can be left unititialized */
}

/* This is here because libjpeg doesn't support transformation from CMYK
 * to RGB so that we must do it ourselves.
 *
 * data must be non-NULL. */
static void cmyk_to_rgb(unsigned char *data, int pixels)
{
	for (;pixels;pixels--, data+=4)
	{
		/* C -> R */
		data[0]=((data[0])*(data[3])+127)/255;

		/* M -> G */
		data[1]=((data[1])*(data[3])+127)/255;

		/* Y -> B */
		data[2]=((data[2])*(data[3])+127)/255;

		/* Put alpha=1 instead of K */
		data[3]=255;
	}
}

/* data must be non-NULL */
static void gray_to_rgb(unsigned char *data, int pixels)
{
	unsigned char *dest;

	dest=data+(pixels-1)*3;
	data+=pixels-1;
	for(;pixels;pixels--,data--,dest-=3){
		dest[2]=*data;
		dest[1]=*data;
		dest[0]=*data;
	}
}

/* Fixes returned data in case they are CMYK or grayscale. */
static inline void fix_data( struct jpg_decoder *deco, int lines_read)
{
	int a;

	switch (global_cinfo->output_components){
		case 1:
		for (a=0; a<lines_read; a++)
			gray_to_rgb(deco->scanlines[a], global_cinfo
				->output_width);
		break;

		case 3:
		break;

		case 4:
		cmyk_to_rgb(deco->scanlines[0], global_cinfo
			->output_width*lines_read);
		break;

		default: internal("Invalid output_components");
	}
}

void jpeg_restart(struct cached_image *cimg, unsigned char *data, int length)
{
	struct jpg_decoder *deco;

	deco=(struct jpg_decoder *)(cimg->decoder);
#ifdef DEBUG
	if (!deco) internal("NULL decoder in jpeg_restart");
#endif /* #ifdef DEBUG */
	global_cinfo=((struct jpg_decoder *)(cimg->decoder))->cinfo;
	global_jerr=((struct jpg_decoder *)(cimg->decoder))->jerr;
	/* These global variables are here so that we don't have to pass lots
	 * of structure pointers into each function. The jpeg decoder is never
	 * running twice at the same time so it doesn't matter.
	 */

	/* If the decoder wants us to skip bytes it's not interested in */
	if (deco->skip_bytes>=length){
		/* If the decoder wants to skip as much as or more bytes than
		 * the chunk that has just arrived */
		deco->skip_bytes-=length;
		return;
	}else{
		/* If the decoder wants to skip less bytes than the chunk
		 * that has just arrived */
		data+=deco->skip_bytes;
		length-=deco->skip_bytes;
		deco->skip_bytes=0;
	}

	/* Add the arrived data chunk into the decoder buffer. Sometimes the
	 * chunks are so small the decoder can't move on on a single chunk
	 * so it has to accumulate more chunks together. This is why the buffer
	 * is there. */
	if ((unsigned)global_cinfo->src->bytes_in_buffer + (unsigned)length > MAXINT) overalloc();
	if ((unsigned)global_cinfo->src->bytes_in_buffer + (unsigned)length < (unsigned)length) overalloc();
	if (deco->jdata){
		/* If there is already some decoder buffer, we have to
		 * allocate more space */
		memmove(deco->jdata,global_cinfo->src->next_input_byte,
			global_cinfo->src->bytes_in_buffer);
		deco->jdata=mem_realloc(
			deco->jdata, global_cinfo->src->bytes_in_buffer+length);
	}else{
		/* If there is no decoder buffer we'll have to allocate
		 * space for a new buffer */
		deco->jdata=mem_alloc(global_cinfo->src->bytes_in_buffer+length);
	}

	/* Copy the data iself into the decoder buffer */
	memcpy(deco->jdata+global_cinfo->src->bytes_in_buffer
		,data,length);

	/* Update the next input byte pointer for the decoder to continue at
	 * the right position */
	global_cinfo->src->next_input_byte=deco->jdata;

	/* ...:::...:..:.:::.:.::::.::.:.:.:.::..::::.::::.:...: */
	/* Update the length of data in the decoder buffer */
	global_cinfo->src->bytes_in_buffer+=length;

	if (setjmp(global_jerr->setjmp_buffer)) goto decoder_ended;
	switch(deco->state){
		case 0:
		/* jpeg_read_header */
	   	if (JPEG_SUSPENDED==jpeg_read_header(global_cinfo,TRUE))
			break;
		global_cinfo->buffered_image=TRUE;
		deco->state=1;

		case 1:
		/* If the scaling is sufficiently brutal we can leave out
		 * some DCT coefficients...: */
		/* jpeg_start_decompress */
		if (jpeg_start_decompress(global_cinfo)==FALSE)
			break;

		cimg->width=global_cinfo->output_width;
		cimg->height=global_cinfo->output_height;

		switch(cimg->buffer_bytes_per_pixel=
			global_cinfo->output_components)
		{
			case 1:
				/* We'll do the conversion ourselves
				 * because libjpeg seems to be buggy */
				cimg->buffer_bytes_per_pixel=3;
				break;


			case 3: /* RGB or YCrCb. We will ask libjpeg to
				 * possibly convert from YCrCb to RGB. */

				global_cinfo->out_color_space=JCS_RGB;
				break;

			case 4:
				/* CMYK or YCCK. We need to enable conversion
				 * to CMYK and then convert CMYK data to RGBA
				 * with dummy A ourselves.
				 * We will ask libjpeg to possibly convert from
				 * YCCK to CMYK. */
				global_cinfo->out_color_space=JCS_CMYK;
				break;

			default:
			/* Let's make a decompression fatal error here */

			if (!mesg_unsup_emitted){
				fprintf(stderr,
			"Unsupported JPEG output components number: %d.\n",
			cimg->buffer_bytes_per_pixel);
				mesg_unsup_emitted=1;
			}
			longjmp(global_jerr->setjmp_buffer,2);

			/* longjmp()  and  siglongjmp() make programs hard to
			 * understand and maintain.  If possible an alternative
			 * should be used. Hahaha :) ;-)
			 */
			/* Free will makes people hard to understand 
			 * and maintain. If possible an alternative should be 
			 * used.
			 */
			/* With our new LongJump(TM) your jumps will be longer
			 * than with ordinary commercially available jumps.
			 */
		}
		cimg->red_gamma=sRGB_gamma;
		cimg->green_gamma=sRGB_gamma;
		cimg->blue_gamma=sRGB_gamma;
		/* This is defined in the JPEG standard somehow that sRGB
		 * color space is used. */

		cimg->strip_optimized=0;
		/* Strip optimization yet waits to be written. This will
		 * allow huge jpegs to be processed without consuming
		 * Links memory and consuming Xserver memory instead ;-)
		 * However strip optimization is already written for PNG's.
		 */

		if (header_dimensions_known(cimg)) {
			longjmp(global_jerr->setjmp_buffer,2);
		}
new_scan:
		deco->state=2;

		case 2:
		/* jpeg_start_output */
		if (FALSE==jpeg_start_output(global_cinfo,global_cinfo->input_scan_number)){
susp0:
			/* Suspended */
			break;
		}
		deco->state=3;

		case 3:
		/* jpeg_read_scanlines */
			/* color */
        	while (global_cinfo->output_scanline
			<global_cinfo->output_height){
			int a, lines;

			for (a=0;a<16;a++){
				deco->scanlines[a]=cimg->buffer
				+(global_cinfo
				->output_scanline+a)
				*global_cinfo->output_width*cimg->
					buffer_bytes_per_pixel;
			}
		
         		if ((lines=
				jpeg_read_scanlines(
				global_cinfo,deco->scanlines,1))){
				/* Some lines were written into cimg buffer */
				cimg->rows_added=1;
				
				fix_data(deco, lines);
			}else{
				/* No lines have been written into cimg
				 * buffer */
                	 	/* We are suspended and we want more data */
         	 		goto susp0; /* Break the outer 
					     * switch statement */
			}
		}
		deco->state=4;

		case 4:
		/* jpeg_finish_output */
		if (FALSE==jpeg_finish_output(global_cinfo))
		{
			/* Suspended */
			break;
		}
		if (!jpeg_input_complete(global_cinfo))
		{
			/* Some more scans awaited... */
			goto new_scan;
		}
		deco->state=5;

		case 5:
		/* jpeg_finish_decompress */
		if (FALSE==jpeg_finish_decompress(global_cinfo))
			break;
decoder_ended:
		img_end(cimg);
	}
}
#endif /* #ifdef HAVE_JPEG */

#endif /* #ifdef G */

