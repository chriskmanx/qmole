/* (c) 2001 Karel 'Clock' Kulhavy
 * Serves the purpose to manipulate grayscale PNG images according to a
 * linear command list.
 * commandline format: improcess input_filename command_filename output_filename
 * Internal format: a 2D field of unsigned's
 *  0x000000 black
 *  0xffffff white
 * Commandfile format:
 * command argument1 argument2...
 * command argument1 argument2...
 * .
 * .
 * .
 */

#include <stdio.h>
#include <stdlib.h>
#include <png.h>

int *image;
int xs,ys;
int force_gamma_1;

void read_png(unsigned char *filename)
{
	unsigned char *temporary_array;
	png_structp png_ptr;
	png_infop info_ptr;
	double gamma;
	int y1,number_of_passes;
	unsigned char **ptrs;
	FILE *f;
	
	f=fopen(filename,"r");
	png_ptr=png_create_read_struct(PNG_LIBPNG_VER_STRING,
			NULL, NULL, NULL);
	info_ptr=png_create_info_struct(png_ptr);
	png_init_io(png_ptr,f);
	png_read_info(png_ptr, info_ptr);
	xs=png_get_image_width(png_ptr,info_ptr);
	ys=png_get_image_height(png_ptr,info_ptr);
	if (png_get_gAMA(png_ptr,info_ptr, &gamma))
	{
		if (force_gamma_1) png_set_gamma(png_ptr, 1.0, 1.0);
		/* Forcing gamma is here for repairing files after processing
		 * with GIMP, which deliberately writes gamma=00 00 b1 8f
		 * even when input gamma was 00 01 86 a0 and no gamma setting
		 * change is performed by user (simply saind, GIMP is
		 * braindead)
		 */
		else png_set_gamma(png_ptr, 1.0, gamma);
	}
	else
	{
		png_set_gamma(png_ptr, 1.0, 0.454545);
	}
	{
		int bit_depth;
		int color_type;

		color_type=png_get_color_type(png_ptr, info_ptr);
		bit_depth=png_get_bit_depth(png_ptr, info_ptr);
		if (color_type==PNG_COLOR_TYPE_GRAY){
			if (bit_depth<8){
				 png_set_expand(png_ptr);
			}
			if (bit_depth==16){
				 png_set_strip_16(png_ptr);
			}
		}
		if (color_type==PNG_COLOR_TYPE_PALETTE){
			png_set_expand(png_ptr);
			png_set_rgb_to_gray(png_ptr,1,54.0/256,183.0/256);
		}
		if (color_type & PNG_COLOR_MASK_ALPHA){
			png_set_strip_alpha(png_ptr);
		}
		if (color_type==PNG_COLOR_TYPE_RGB ||
			color_type==PNG_COLOR_TYPE_RGB_ALPHA){
			png_set_rgb_to_gray(png_ptr, 1, 54.0/256, 183.0/256);
		}
		
	}
	/* If the depth is different from 8 bits/gray, make the libpng expand
	 * it to 8 bit gray.
	 */
	number_of_passes=png_set_interlace_handling(png_ptr);
	png_read_update_info(png_ptr,info_ptr);
	temporary_array=malloc(xs*ys);
	image=malloc(xs*ys*sizeof(image));
	ptrs=malloc(ys*sizeof(*ptrs));
	for (y1=0;y1<ys;y1++) ptrs[y1]=temporary_array+xs*y1;
	for (;number_of_passes;number_of_passes--){
		png_read_rows(png_ptr, ptrs, NULL, ys);
	}
	png_read_end(png_ptr, NULL);
	png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
	{
		int *imptr=image;
		unsigned char *tptr=temporary_array;
		int a;

		for (y1=ys*xs;y1;y1--){
			a=*tptr++;
			a=a|(a<<8)|(a<<16);
			*imptr++=a;
		}
	}
	free(ptrs);
	free(temporary_array);
	fclose(f);
}

void write_png(unsigned char *filename)
{
	unsigned char *temporary;
	unsigned char *tptr;
	int *iptr,a;
	FILE *f;
	png_structp png_ptr;
	png_infop info_ptr;

	f=fopen(filename,"w");
	if (!f){
		fprintf(stderr,"Unable to open file %s.\n",filename);
		exit(1);
	}
	temporary=malloc(xs*ys);
	if (!temporary){
		fprintf(stderr,"Out of memory.\n");
		exit(1);
	}
	iptr=image;
	tptr=temporary;
	for (a=xs*ys;a;a--){
		*tptr++=(*iptr++)>>16;
	}
 	png_ptr=png_create_write_struct(PNG_LIBPNG_VER_STRING,NULL,NULL,NULL);
	info_ptr=png_create_info_struct(png_ptr);
	png_init_io(png_ptr,f);
	png_set_filter(png_ptr,0,PNG_FILTER_NONE|PNG_FILTER_SUB|PNG_FILTER_UP
		|PNG_FILTER_AVG|PNG_FILTER_PAETH);
	png_set_compression_level(png_ptr,Z_BEST_COMPRESSION);
	png_set_IHDR(png_ptr,info_ptr,xs,ys,8,PNG_COLOR_TYPE_GRAY,PNG_INTERLACE_NONE,
		PNG_COMPRESSION_TYPE_DEFAULT,PNG_FILTER_TYPE_DEFAULT);
	png_set_gAMA(png_ptr,info_ptr,1.0);
	png_write_info(png_ptr,info_ptr);
	tptr=temporary;
	for (a=ys;a;a--){
		png_write_row(png_ptr,tptr);
		tptr+=xs;
	}
 	png_write_end(png_ptr,info_ptr);
	png_destroy_write_struct(&png_ptr,&info_ptr);
	free(temporary);
	fclose(f);
}

void clip(void)
{
	int a;
	int *iptr;
	int val;

	iptr=image;
	for (a=xs*ys;a;a--){
		val=*iptr;
		if (val<0) val=0;
		if (val>0xffffff) val=0xffffff;
		*iptr++=val;
	}
}

void threshold(int param)
{
	int a;
	int *iptr;
	int val;

	iptr=image;
	for (a=xs*ys;a;a--){
		val=*iptr;
		*iptr++=(val>=param)?0xffffff:0;
	}
}

void mul(int param)
{
	int a;
	int *iptr;
	int val;

	iptr=image;
	for (a=xs*ys;a;a--){
		val=*iptr;
		val*=param;
		*iptr++=val;
	}
}

void add(int param)
{
	int a;
	int *iptr;
	int val;

	iptr=image;
	for (a=xs*ys;a;a--){
		val=*iptr;
		val+=param;
		*iptr++=val;
	}
}

void divide(int param)
{
	int a;
	int *iptr;
	int val;

	iptr=image;
	for (a=xs*ys;a;a--){
		val=*iptr;
		val/=param;
		*iptr++=val;
	}
}

void right_shift(int param)
{
	int a;
	int *iptr;
	int val;

	iptr=image;
	for (a=xs*ys;a;a--){
		val=*iptr;
		val>>=param;
		*iptr++=val;
	}
}

void left_shift(int param)
{
	int a;
	int *iptr;
	int val;

	iptr=image;
	for (a=xs*ys;a;a--){
		val=*iptr;
		val<<=param;
		*iptr++=val;
	}
}

void flip()
{
	int *new;
	int x,y;
	int *iptr,*nptr;

	new=malloc(xs*ys*sizeof(*new));
	if (!new){
		fprintf(stderr,"Out of memory when flipping.\n");
		exit(1);
	}
	iptr=image;
	nptr=new;
	for (y=ys;y;y--){
		for(x=xs;x;x--){
			*nptr=*iptr;
			iptr++;
			nptr+=ys;
		}
		nptr-=xs*ys;
		nptr++;
	}
	free(image);
	image=new;
	x=xs;
	xs=ys;
	ys=x;
}

void mirror(void){
	int y;
	int *fptr, *rptr, *lptr;
	int xchg;

	lptr=image;
	for (y=ys;y;y--){
		fptr=lptr;
		rptr=lptr+xs-1;
		while(rptr>fptr){
			xchg=*rptr;
			*rptr=*fptr;
			*fptr=xchg;
			fptr++;
			rptr--;
		}
		lptr+=xs;
	}
}

void append(int lines, int value){
	int *ptr;
	
	if (lines<=0) return;
	image=realloc(image,xs*(ys+lines)*sizeof(*image));
	if (!image){
		fprintf(stderr,"Out of memory when appending lines to the image.\n");
		exit(1);
	}
	ptr=image+xs*ys;
	ys+=lines;
	for (lines*=xs;lines;lines--){
		*ptr++=value;
	}
}

void detract(int lines){
	if (lines>=ys) return; /* Invalid */
	ys-=lines;
	image=realloc(image,xs*ys*sizeof(*image));
	if (!image){
		fprintf(stderr,"Out of memory at detract.\n");
		exit(1);
	}
}

/* Blurs so that each pixel is replaced by sum of its value, value of "pixels" pixels
 * left and "pixels" pixels right, divided by 2*pixels+1.
 */
void blurbox(int pixels)
{
	int *templine, *iptr;
	int *tptr;
	int *addptr,*subptr;
	int divisor=pixels*2+1;
	int y,x;
	int val;
	
	templine=malloc((2*pixels+xs)*sizeof(*templine));
	if (!templine){
		fprintf(stderr,"Out of memory when box blurring.\n");
		exit(1);
	}
	iptr=image;
	for (y=ys;y;y--){
		tptr=templine;
		val=*iptr/divisor;
		for (x=pixels;x;x--){
			*tptr++=val;
		}
		for (x=xs;x;x--){
			*tptr++=(*iptr++)/divisor;
		}
		val=iptr[-1]/divisor;
		iptr-=xs;
		for (x=pixels;x;x--){
			*tptr++=val;
		}
		val=0;
		tptr=templine;
		for (x=divisor-1;x;x--){
			val+=*tptr++;
		}
		addptr=tptr;
		subptr=templine;
		for (x=xs;x;x--){
			val+=*addptr++;
			*iptr++=val;
			val-=*subptr++;
		}
	}
	free(templine);
}

void perform_command_line(unsigned char *command)
{
	char *ptr;
	int param1, param2;
	
	/* Find the first space, newline, tab or null */
	ptr=command;
	while(!(*ptr==0||*ptr==10||*ptr==32||*ptr==9)) ptr++;
	if (!*ptr) return; /* Invalid */
	*ptr=0;
	ptr++;
	if (!strcmp(command,"clip")){
		clip();
	}else if (!strcmp(command,"threshold")){
		param1=strtol(ptr,NULL,0);
		threshold(param1);
	}else if (!strcmp(command,"flip")){
		flip();
	}else if (!strcmp(command,"append")){
		param1=strtol(ptr,&ptr,0);
		param2=strtol(ptr,&ptr,0);
		append(param1,param2);
	}else if (!strcmp (command,"detract")){
		param1=strtol(ptr,&ptr,0);
		detract(param1);
	}else if (!strcmp(command,"mirror")){
		mirror();
	}else if (!strcmp(command,"blurbox")){
		param1=strtol(ptr,&ptr,0);
		blurbox(param1);
	}else if (!strcmp(command,"gaussian")){
		param1=strtol(ptr,&ptr,0);
		param2=strtol(ptr,&ptr,0);
		if (param1>0){
			for (;param1;param1--){
				blurbox(param2);
			}
		}
	}else if (!strcmp(command,"*")){
		param1=strtol(ptr,&ptr,0);
		mul(param1);
	}else if (!strcmp(command,"+")){
		param1=strtol(ptr,&ptr,0);
		add(param1);
	}else if (!strcmp(command,"/")){
		param1=strtol(ptr,&ptr,0);
		if (param1) divide(param1);
	}else if (!strcmp(command,">>")){
		param1=strtol(ptr,&ptr,0);
		right_shift(param1);
	}else if (!strcmp(command,"<<")){
		param1=strtol(ptr,&ptr,0);
		left_shift(param1);
	}else{
		fprintf(stderr,"Invalid command %s encountered.\n",command);
	}
}

void process_commands(unsigned char *filename)
{
	FILE *f;
	unsigned char string[1024];

	if (!strlen(filename)) return;
	/* "" as command filename */
	f=fopen(filename,"r");
	if (!f){
		fprintf(stderr,"Can't open command file %s.\n",filename);
		exit(1);
	}
	while(fgets(string, sizeof(string),f)){
		perform_command_line(string);
	}
	fclose(f);
}

int main(int argc, char** argv)
{
	if (argc<4){
usage:
		fprintf(stderr,
			"Usage: improcess [-f] input_filename "
			"command_filename output_filename\nIf command_filename\
is \"\", then no operations are performed.\n");
		return 0;
	}
	if (argv[1][0]=='-'&&argv[1][1]=='f'&&!argv[1][2])
	{
		/* -f flag */
		if (argc<5) goto usage;
		force_gamma_1=1;
		read_png(argv[2]);
		process_commands(argv[3]);
		write_png(argv[4]);
	}else{
		read_png(argv[1]);
		process_commands(argv[2]);
		write_png(argv[3]);
	}
	return 0;
}
