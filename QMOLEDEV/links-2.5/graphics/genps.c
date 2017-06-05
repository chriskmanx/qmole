#include <stdio.h>

/* Begin of user-settable variables */
float font_pos=300; /* Default: 300 */
float font_height=392.9619; /* Default: 392.9619 */
float h_margin=100; /* Default: 100 */
float v_margin=120; /* Default: 120 */
float paper_height=842; /* Do not change paper_height unless you know what you are
                         * doing. Default: 842 */
/* End of user=settable variables */

#define FONT "Links-generated"

int main(int argc, char **argv)
{
	FILE *f=fopen("letters.ps","w");
	int a;

	fprintf (f,"%%!PS-Adobe-1.0\n%%%%DocumentFonts: %s\n"
	"%%%%Title: Bitmapped font generator\n%%%%Pages: %d\n"
/*	"%%%%BoundingBox: 0 0 150 250\n" */
	"%%%%EndProlog\n\n",FONT,256);
/*	fprintf(f,"/.setpagesize { /statusdict .systemvar "
	"begin .setpagesize end } bind def\n"
	"userdict begin\n"
	"/links {15 25 //.setpagesize exec} bind def\n"
	"end\n"
	"currentdict /.setpagesize .undef\n");
*/
	for (a=0;a<256;a++){
		fprintf (f,"%%%%Page: %d %d\n",a,a+1);
		fprintf (f,"/%s\n",FONT);
		fprintf (f,"findfont %f scalefont setfont %f %f"
			" moveto\n",font_height,h_margin,
			font_pos);
		fprintf (f,"<%02x> show ",a);
		fprintf (f,"0 %f rmoveto 0 %f ",paper_height-font_pos
			-v_margin,v_margin);
		fprintf (f,"rlineto %f %f ",h_margin
			,paper_height);
		fprintf (f,"lineto %f %f ",h_margin,paper_height-v_margin);
		fprintf (f,"lineto fill %f 0 moveto <%02x> ",h_margin,a);
		fprintf (f,"stringwidth rlineto 0 %f rlineto %f %f ",
			v_margin, h_margin,v_margin);
		fprintf (f,"lineto fill showpage\n");
	}
	fprintf (f,"%%%%Trailer\n");
	fclose(f);
	return 0;
}

