/* (c) 2000 Karel Kulhavy, Clocksoft
 * clock@atrey.karlin.mff.cuni.cz
 * This program is a stdin filter that performs these operations:
 * 1) Downconverts 17x15 oversampled binary image data (black-or-white
 *    pixels, 1 bit per pixel, pbmraw format) to 256-level grayscale
 *    pgmraw format and saves it to basename_%d.png.
 * 2) At the end, generates necessary html's
 * 3) The input pages must be concatenated
 * 4) Recommended source of the pbm: Aladdin Ghostscript
 * If there is an input argument (one number as first arg), it's treated
 * as number of first page that comes at fd #0. Default value is 0.
 * Parameters: <hundred_dpi> <basename_> <titlestring> <bottom_html_code>
 * <pageoffset> <input_filename> [first page number]

 * Little benchmarks:   Normal (gs+pbm2png)     1:32
                        Without pbm2png         0:28
                        Without libpng          0:51
                        Without grayscaling     0:32
                        
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <png.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

unsigned char *basename_;
/*
	basename:
		pbm2png.c:32: `basename' redeclared as different kind of symbol
		/usr/include/string.h:245: previous declaration of `basename'
*/
unsigned char *titlestring;
unsigned char *bottom;
int pageoffset=-13;
unsigned long t1[256]; /* Conversion from 1-bit to 2-bit expansion */
unsigned long t2[256]; /* Conversion from 2-bit to 4-bit expansion */
unsigned char *l1; /* Input data, output data. lw<<1 bytes long */
unsigned long *l2; /* 2-bit expansion lw unsigned long's long*/
unsigned long long *l4; /* 4-bit expansion lw unsigned long long's long */
int ox,oy;
int lw; /* Width of the input in 16-pixel units. */
int lb; /* Width of the input in 8-pixel units */
FILE *of;
int filenumber;
int x,y;
png_structp png_ptr;
png_infop info_ptr;
unsigned long ppm;
float dpi;
unsigned char string[8192];
unsigned char *filename;
int ifd;

void
gentables(void)
{
        int a,d;
        unsigned long b;
        unsigned char c;

        for (a=0;a<256;a++){
                b=0;
                c=a;
                for (d=7;d>=0;d--){
                        b<<=2;
                        b|=(c>>d)&1;
                }
                t1[a]=b;
                b=0;
                c=a;
                for (d=4;d;d--){
                      b<<=4;
                      b|=c&3;
                      c>>=2;
               }
                t2[a]=b;
        }
}

void
make_index(void)
{
	FILE *f;
	
	f=fopen("index.html","w");

	fprintf(f,"<html><head><title>%s</title></head><body bgcolor=\"#000000\" text=\"#00ff00\" link=\"#ffff00\""
		" vlink=\"#00ffff\" alink=\"#ffff00\">\n",titlestring);
	fprintf(f,"<a href=\"../\"><img src=\"up.png\" border=0></a><br>"
		"<a href=\"%s.pdf\">Download in PDF format</a><br>"
		"<h1>%s</h1><h2>Page index</h2>",basename_,titlestring);
	{
		int n;
		unsigned char *t;
		FILE *g;
	
		fprintf(f,"<table cellspacing=0 cellpadding=0 border=1>\n<tr>");
		for (n=0;n<filenumber;n++){
			if (!(n%40)&&n){
				fprintf(f,"</tr>\n<tr>");
			}
			fprintf(f,"<td><a href=\"%s%d.html\">%d</a></td>\n",basename_,n,n+pageoffset);
		}
		
		fprintf(f,"</tr></table><h2>");
		g=fopen("index.dir","rb");
		if (g)
		{	
			t=malloc(65536);
			fprintf(f,"Contents Index</h2><table border=0 cellspacing=0 cellpadding=0>");
			while(fgets(t,65536,g)){
				int val=0;
				int len=strlen(t);
				unsigned char *u=t;
				if (len&&t[len-1]=='\n') t[len-1]=0;
				if (*t>'9'||*t<'0') continue;
				while(*u>='0'&&*u<='9'){
					val*=10;
					val+=*u-'0';
					u++;
				}	
				val--;
				u++; /* The char behind the number is ignored */
				fprintf(f,"<tr><td>%d </td><td><a href=\"%s%d.html\">%s</a></td></tr>\n",val+pageoffset
					,basename_,val,u);
			}
			fprintf(f,"</table>");
			fclose(g);
			free(t);
		}
		fprintf(f,"</body></html>");
		fclose(f);
			
	}
}


void
make_page(int index)
{
 FILE *f;
 
 sprintf(string,"%s%d.html",basename_,index);
 f=fopen(string,"w");

 fprintf(f,"<html><head><title>%s</title></head><body bgcolor=\"#000000\" text=\"#00ff00\" link=\"#ffff00\" vlink=\"#00ffff\"
alink=\"#ffff00\">\n",titlestring);
 fprintf(f,"<h1>%s, Page %d</h1>\n",titlestring,index+pageoffset);
 fprintf(f,"<p><table border=0 cellspacing=0 cellpadding=2><tr>");
 fprintf(f,"<td valign=\"top\"><table border=0 cellpadding=0 cellspacing=0><tr><td>");
 if (index)
 {
  fprintf(f,"<a href=\"%s%d.html\"><img src=\"left.png\" border=0
></a>\n",basename_,index-1);
 }
 else
 {
  fprintf(f,"<img src=\"left.png\" border=0>\n");
 }
 fprintf(f,"</td><td>");
 if (index<filenumber-1)
 {
  fprintf(f,"<a href=\"%s%d.html\">
<img src=\"right.png\" border=0></a>\n",basename_,index+1);
 }
 else
 {
  fprintf(f,"<img src=\"right.png\" border=0>\n");
 }
 fprintf(f,"</td></tr><tr><td colspan=2><a href=\"index.html\">
<img src=\"idx.png\" border=0 ></a></td>");
 fprintf(f,"</tr></table>");
 fprintf(f,"</td><td><img src=\"%s%d.png\" border=\"0\"
></td>\n</tr></table>",basename_,index);
 fprintf(f,"</body></html>");
 fclose(f);
}

/* Returns 0 is OK, exit(1) on error, returns 1 on broken pipe */
int 
sure_read(unsigned char *dest, size_t len)
{
        ssize_t rd;
        if (!len) return 0;
        again:
        rd=read(ifd,dest,len);
        if (rd==len) return 0;
        if (!rd) return 1;
        if (rd<0&&(errno==EINTR||errno==EAGAIN||errno==EWOULDBLOCK)) goto again;
        if (rd>0&&rd<len){
                /*fprintf(stderr,"read %d/%d",rd,len);*/
                len-=rd;
                dest+=rd;
                fflush(stderr);
                goto again;
        }
        fprintf(stderr,"read error.\n");
        perror("");
        exit(1);
}

int
eat_up_whitespace_and_comments(void)
{
        again:
        sure_read(string,1);
        if (*string==' '||*string=='\t'){
                goto again;
        }
        if (*string=='\n'||*string=='\r'){
                goto again;
        }
        if (*string=='#'){
                /* Eat up comment */
                aa:
                sure_read(string,1);
                if (*string!='\n'&&*string!='\r') goto aa;
                goto again;
        }
        return *string;
}

int
read_header(void)
{
        

        /*
        if (filenumber)
        {
                int a;
                
                for (a=0;a<256;a++){
                        sure_read(string,1);
                        printf("%d ", *string);
                        fflush(stdout);
                }
                exit(0); 
        }
        */
        if (sure_read(string,2)) return 1; /* P1 */
        x=eat_up_whitespace_and_comments()-'0';
        again:
        sure_read(string,1);
        if (*string!=' '&&*string!='\t'&&*string!='\n'&&*string!='\r'){
                x*=10;
                x+=*string-'0';
                goto again;
        }
        y=eat_up_whitespace_and_comments()-'0';
        bgain:
        sure_read(string,1);
        if (*string!=' '&&*string!='\t'&&*string!='\n'&&*string!='\r'){
                y*=10;
                y+=*string-'0';
                goto bgain;
        }
        return 0;
}

void
open_png(void)
{
 png_ptr=png_create_write_struct(PNG_LIBPNG_VER_STRING,NULL,NULL,NULL);
 info_ptr=png_create_info_struct(png_ptr);
 png_init_io(png_ptr,of);
 png_set_filter(png_ptr,0,PNG_FILTER_NONE|PNG_FILTER_SUB|PNG_FILTER_UP
  |PNG_FILTER_AVG|PNG_FILTER_PAETH);
 png_set_compression_level(png_ptr,Z_BEST_COMPRESSION);
 png_set_IHDR(png_ptr,info_ptr,ox,oy,8,PNG_COLOR_TYPE_GRAY,PNG_INTERLACE_NONE,
  PNG_COMPRESSION_TYPE_DEFAULT,PNG_FILTER_TYPE_DEFAULT);
 png_set_gAMA(png_ptr,info_ptr,1.0);
 png_set_pHYs(png_ptr,info_ptr,ppm,ppm,PNG_RESOLUTION_METER);
 png_write_info(png_ptr,info_ptr);
}

void
close_png(void)
{
 png_write_end(png_ptr,info_ptr);
 png_destroy_write_struct(&png_ptr,&info_ptr);
}

/* Represents the same pixels that are in l2 in l4. */
void
move_2_to_4(void)
{
        unsigned long long bit4;
        unsigned long bit2;
        int index;
                
        for (index=lw-1;index>=0;index--){
                bit2=l2[index];
                bit4=t2[bit2&255];
                bit4<<=16;
                bit2>>=8;
                bit4|=t2[bit2&255];
                bit4<<=16;
                bit2>>=8;
                bit4|=t2[bit2&255];
                bit4<<=16;
                bit2>>=8;
                bit4|=t2[bit2&255];
                l4[index]=bit4;
        }
}

/* Adds the same pixels that are in l2 to l4. */
void
add_2_to_4(void)
{
        unsigned long long bit4;
        unsigned long bit2;
        int index;
                
        for (index=lw-1;index>=0;index--){
                bit2=l2[index];
                bit4=t2[bit2&255];
                bit4<<=16;
                bit2>>=8;
                bit4|=t2[bit2&255];
                bit4<<=16;
                bit2>>=8;
                bit4|=t2[bit2&255];
                bit4<<=16;
                bit2>>=8;
                bit4|=t2[bit2&255];
                l4[index]+=bit4;
        }
}

/* Moves pixels from l1 to l2 with format change */
void
move_1_to_2(void)
{
        int index;
        
        for (index=lw-1;index>=0;index--)
                l2[index]=(t1[l1[index<<1]]<<16)|t1[l1[(index<<1)+1]];
}

/* Adds pixels from l1 to l2 with format change */
void
add_1_to_2(void)
{
        int index;
        
        for (index=lw-1;index>=0;index--)
                l2[index]+=(t1[l1[index<<1]]<<16)|t1[l1[(index<<1)+1]];
}

/* Loads 1 line into l1 */
void
load(void)
{
        sure_read(l1,lb);
}

/* Loads 3 lines into l2 */
void
load_to_2(void)
{
        load();
        move_1_to_2();
        load();
        add_1_to_2();
        load();
        add_1_to_2(); 
}

/* Loads 15 lines into l4 */
void
load_to_4(void)
{
        load_to_2();
        move_2_to_4();
        load_to_2();
        add_2_to_4();
        load_to_2();
        add_2_to_4();
        load_to_2();
        add_2_to_4();
        load_to_2();
        add_2_to_4();
}

/* Converts the data from l4 to l1 including suming up 17 adjacent pixels */
void
export_from_4(void)
{
        int a,b,reg_hold;
        unsigned char sum;
        unsigned long long *loader=l4;
        unsigned long long reg=0;

        reg_hold=0;
        for (a=0;a<ox;a++){
                sum=0;
                for (b=17;b;b--){
                      if (!reg_hold){
                              reg=*loader++;
                              reg_hold=16;
                      }
                      sum+=reg&15;
                      reg>>=4;
                      reg_hold--;
                }
                l1[a]=sum;
        }
        png_write_row(png_ptr,l1);
}

/*
 * Parameters: <hundred_dpi> <basename_> <titlestring> <bottom_html_code>
 * <pageoffset> [first page number]
 */
int
main(int argc, char **argv)
{
        int a,z;
 
        if (argc<7){
                fprintf(stderr,"Usage: pbm2png <hundred_dpi> <basename_> <titlestring>
<bottom_html_code> <pageoffset> <ifname> [starting_filenumber]\n");
        return 0;
        }
        dpi=atof(argv[1])/10;
        ppm=dpi*1000/25.4;
        basename_=argv[2];
        titlestring=argv[3];
	bottom=argv[4];
        pageoffset=atol(argv[5]);
        filename=argv[6];
	again0:
        ifd=open(filename,O_RDONLY);
	if (ifd<0){
		if (errno==EAGAIN||errno==EINTR||errno==EWOULDBLOCK) goto
			again0;
		else {
			perror("");
			exit(1);
		}
	}
        fprintf(stderr,"filename %s, %d\n",filename,ifd);
        if (argc>=8){
                filenumber=atol(argv[7]);
        }
        gentables();
        again:
        fprintf(stderr,"\nFile %i\n",filenumber);
        if (read_header()){
                for (a=0;a<filenumber;a++)
                        make_page(a);
		make_index();
                return 0;
        }
        lw=(x+15)>>4;
        lb=(x+7)>>3;
        ox=x/17;
        oy=y/15;
        fprintf(stderr,"Input: %i*%i pixels, %f*%f dpi, %.1fMB.\n",x,y,dpi*17,dpi*15,(float)lb*y/1048576);
        fprintf(stderr,"Ouput: %i*%i pixels, %f*%f dpi, %.1fKB raw data.\n",ox,oy,dpi,dpi,(float)ox*oy/1024);
        l1=(unsigned char*)malloc(lw*2);
        l2=(unsigned long*)malloc(lw*sizeof(unsigned long));
        l4=(unsigned long long*)malloc(lw*sizeof(unsigned long long));
        sprintf(string,"%s%d.png",basename_,filenumber);
        filenumber++;
        of=fopen(string,"w");
        open_png();
        for (z=oy;z;z--){
                if (!(z&15)){
                        fprintf(stderr,".");
                        fflush(stderr);
                } 
                load_to_4();
                export_from_4();
        }
        close_png();
        fprintf(stderr,"\nWritten %lu bytes of data, ratio %.1f%%\n",ftell(of),(float)ftell(of)*100/ox/oy);
        fclose(of);
        for (a=y%15;a;a--)
                load();
        free(l1);
        free(l2);
        free(l4);
        goto again;
}
