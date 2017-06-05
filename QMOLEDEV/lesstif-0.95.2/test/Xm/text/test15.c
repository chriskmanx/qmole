/* $Id: test15.c,v 1.7 2001/05/30 08:15:14 amai Exp $ */
/** --------------------------------------------------------------- **/

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include <Xm/TextP.h>
#include <Xm/TextStrSoP.h>
#include <Xm/Text.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>

XtAppContext	app_context;
XmTextWidget 	text = 0;
int argInitialString = False ;
int argLongString = False ;
int argStringValues = False ;
int argShowDrawCursor = False ;
int argShowReadProc = False ;

#ifndef Out_Rows

#define Out_XDraw(OUTPUT) ( OUTPUT -> insertx )
#define Out_YDraw(OUTPUT) ( OUTPUT -> inserty )
#define Out_XOffset(OUTPUT) ( OUTPUT -> hoffset )
#define Out_DrawGC(OUTPUT) ( OUTPUT -> gc )
#define Out_DrawGCInverted(OUTPUT) ( OUTPUT -> have_inverted_image_gc )
#define Out_Rows(OUTPUT) ( OUTPUT -> rows )
#define Text_OutputData( WID )	( ((XmTextWidget)WID) -> text.output -> data )
#define Text_InputData( WID )	( ((XmTextWidget)WID) -> text.input -> data )
#define Text_Line( WID )	( ((XmTextWidget)WID) -> text.line )
#define Text_LineTable( WID )	( ((XmTextWidget)WID) -> text.line_table )
#define Text_LastPos( WID )	( ((XmTextWidget)WID) -> text.last_position )
#define Text_Source( WID )	( ((XmTextWidget)WID) -> text.source )
#define Text_TopLine( WID )	( ((XmTextWidget)WID) -> text.top_line )
#define Text_TotalLines( WID )	( ((XmTextWidget)WID) -> text.total_lines )
#define Text_Highlight( WID )	( ((XmTextWidget)WID) -> text.highlight )
#define Text_OldHighlight( WID )	( ((XmTextWidget)WID) -> text.old_highlight )
#define Text_CurPos( WID )	( ((XmTextWidget)WID) -> text.cursor_position )

#define Out_Font( OUTPUT ) ( OUTPUT -> font )
#define Out_TabWidth( OUTPUT ) ( OUTPUT -> tabwidth )
#define Out_FontHeight( OUTPUT ) ( OUTPUT -> font -> ascent + OUTPUT -> font -> descent )
#define Text_LastTopChar(w) (((XmTextWidget)(w))->text.last_top_char)

#if XmVersion == 2000
extern LineNum _XmTextPosToLine(XmTextWidget widget,
                                XmTextPosition position);

#endif
#endif

#ifndef Text
#define Text( WID )	( ((XmTextWidget)WID) -> text )
#define Source_CurPos(SOURCE) ( SOURCE -> data -> widgets[0] -> text.cursor_position )
#endif

static int indent = 0;
void ShowRangeData ( XmTextWidget, int, Boolean  );
void ShowLine ( XmTextWidget, LineNum, int  );
void ShowLineTable ( XmTextWidget, LineNum, int  );
void ShowSecondarySelectionData ( XmTextWidget );
void ShowHighlightData ( XmTextWidget );
void ShowSelectionData ( XmTextWidget );

/* Pointers to hooks which I "catch" */
static	DrawProc DefaultDraw = 0;
ReplaceProc DefaultReplace;
static DrawInsertionPointProc DefaultDrawInsertionPoint;
static MakePositionVisibleProc DefaultMakePositionVisible;
ReadProc DefaultRead;
static GetSelectionProc DefaultGetSelectionProc = 0;
static MeasureLineProc DefaultMeasureLineProc = 0;
static MoveLinesProc DefaultMoveLines;
static SetSelectionProc DefaultSetSelectionProc = 0;
static InputInvalidateProc DefaultInputInvalidateProc = 0;
static InvalidateProc DefaultOutputInvalidateProc = 0;
static CountLinesProc DefaultCountLines = 0;
static InputGetSecResProc DefaultInputGetSecResDataProc = 0;
static ScanProc DefaultScanProc = 0;
XtProc DefaultClassInitialize ;
XtWidgetClassProc DefaultClassPartInitialize ;
OutputCreateProc DefaultOutputCreate;
InputCreateProc DefaultInputCreate;
XtRealizeProc DefaultRealize ;
XtWidgetProc DefaultResize;
XtExposeProc DefaultExpose ;
XYToPosProc DefaultXYToPos ;
PosToXYProc DefaultPosToXY ;
GetPreferredSizeProc DefaultGetPreferredSize ;
SetValuesProc DefaultOutputSetValues ;
GetValuesProc DefaultOutputGetValues ;
XmRealizeOutProc DefaultOutputRealize ;
XmResizeFlagProc DefaultOutputResize;
XtExposeProc DefaultOutputExpose ;
XtInitProc DefaultInitialize ;

XmTextStatus MyReplaceProc( XmTextWidget, XEvent *, XmTextPosition *,
    		XmTextPosition *, XmTextBlock , Boolean);
XmTextPosition MyReadProc( XmTextSource, XmTextPosition, XmTextPosition, XmTextBlock );
XmTextPosition MyXYToPos( XmTextWidget, Position, Position );
XmTextPosition MyScanProc( XmTextSource, XmTextPosition, XmTextScanType,
    		XmTextScanDirection, int, Boolean);
Boolean MyGetSelectionProc  ( XmTextSource , XmTextPosition* , XmTextPosition* );
Boolean MyMeasureLineProc ( XmTextWidget, LineNum, XmTextPosition, XmTextPosition *,
			LineTableExtraRec **);
Boolean MyMoveLinesProc( XmTextWidget, LineNum, LineNum, LineNum );
Boolean MyPosToXY( XmTextWidget, XmTextPosition, Position *, Position *);
Boolean MyOutputSetValues( Widget, Widget, Widget, ArgList, Cardinal*);
int MyCountLines ( XmTextSource, XmTextPosition, unsigned long);
void MyDraw ( XmTextWidget, LineNum, XmTextPosition, XmTextPosition, XmHighlightMode);
void MyDrawInsertionPointProc ( XmTextWidget, XmTextPosition, OnOrOff );
void MyMakePositionVisibleProc ( XmTextWidget, XmTextPosition );
void MyInvalidateProc ( XmTextWidget, XmTextPosition, XmTextPosition, long );
void MySetSelectionProc ( XmTextSource, XmTextPosition, XmTextPosition, Time );
void MyInputInvalidateProc ( XmTextWidget, XmTextPosition, XmTextPosition, long );
void MyClassInitialize();
void MyClassPartInitialize ( WidgetClass wclass);
void MyOutputCreate( Widget, ArgList, Cardinal);
void MyInputCreate( Widget, ArgList, Cardinal);
void MyRealize( Widget, XtValueMask*, XSetWindowAttributes*);
void MyResize ( Widget );
void MyExpose ( Widget, XEvent*, Region );
void MyGetPreferredSize( Widget, Dimension *, Dimension *);
void MyOutputGetValues( Widget, ArgList, Cardinal);
void MyOutputRealize( Widget, Mask *, XSetWindowAttributes *);
void MyOutputResize( Widget, Boolean);
void MyOutputExpose( Widget, XEvent*, Region );
void MyInitialize ( Widget, Widget, ArgList, Cardinal* );

XmTextPart copyOfTextInternals;
XmSourceDataRec copyOfTextSourceInternals;
InputDataRec copyOfTextInputInternals;
OutputDataRec copyOfTextOutputInternals;
RangeRec copyOfRangeAtIndex0;
LineRec copyOfLineRec [ 100 ]; /* 100 seems to be a reasonably enough huge number */
Boolean widgetInitialized = False;
XmTextLineTableRec copyOfLineTable[101];

static int printedBanner =0;
void InitTextInternals ( )
{
	memset ( &copyOfTextInternals, 170, sizeof ( XmTextPart ) );
	memset ( &copyOfTextSourceInternals, 170, sizeof ( XmSourceDataRec ) );
	copyOfTextSourceInternals.ptr = copyOfTextSourceInternals.value = 
		copyOfTextSourceInternals.gap_start = copyOfTextSourceInternals.gap_end = 
		copyOfTextSourceInternals.PSWC_NWLN = 0;
	memset ( &copyOfTextInputInternals, 170, sizeof ( InputDataRec ) );
	memset ( &copyOfTextOutputInternals, 170, sizeof ( OutputDataRec ) );
	memset ( &copyOfRangeAtIndex0, 170, sizeof ( RangeRec ) );
	memset ( copyOfLineRec, 170, sizeof ( LineRec ) * 100 ) ; 
/*	memset ( &copyOfLineTable, 170, sizeof ( LineTable ) * 100 ) ; 
*/
/*CP: due to a change in type from Motif 1.2 to 2.0 */
	copyOfTextInternals.cursor_position_x = 0;
}

void CopyTextInternals ( XmTextWidget toBeCopied )
{
	memcpy ( &copyOfTextInternals, &(toBeCopied -> text), sizeof ( XmTextPart ) );
	if ( ! widgetInitialized )
		return;
	memcpy ( &copyOfTextSourceInternals, toBeCopied -> text.source->data, sizeof ( XmSourceDataRec ) );
	memcpy ( &copyOfTextInputInternals, toBeCopied -> text.input->data, sizeof ( InputDataRec ) );
	memcpy ( &copyOfTextOutputInternals, toBeCopied -> text.output->data, sizeof ( OutputDataRec ) );
	memcpy ( &copyOfRangeAtIndex0, &toBeCopied -> text.repaint.range[0], sizeof ( RangeRec ) );
	memcpy ( copyOfLineRec, toBeCopied -> text.line, 
		sizeof ( LineRec ) * ( toBeCopied -> text.maximum_lines < 100 ? toBeCopied -> text.maximum_lines : 100 ) );
	if ( toBeCopied -> text.line_table ) 
		memcpy ( copyOfLineTable, toBeCopied -> text.line_table, 
			sizeof ( XmTextLineTableRec ) * ( toBeCopied -> text.total_lines < 100 ? toBeCopied -> text.total_lines : 100 ) );
}

void PrintTime ( Widget w, Time timeToCheck )
{
	Time lastTime =	XtLastTimestampProcessed ( XtDisplay ( w ) ) ;

	if ( timeToCheck == lastTime )
		printf ( " is last processed timestamp\n" );
	else if ( timeToCheck == 0 )
		printf ( " 0\n" );
	else
		printf ( " %u ( is the difference between last time and set time ) \n",
			(unsigned int)(lastTime - timeToCheck) );
}

void PrintDiffDesc ( char* part, char* start )
{
	printf ( "\t\t%s %s", part, start );
}
#define PRINT_BANNER \
		if ( ! printedBanner )		\
			{			\
			printf ( "\tDiffing text with previous saved version:\n" ); \
			printedBanner = 1;	\
			}			\


void PrintInts ( char* part, char* nameToBeChecked, long int copy, long int original )
{
	if ( copy != original ) 
                {
                PRINT_BANNER 
                PrintDiffDesc ( part, nameToBeChecked );
                if ( copy == PASTENDPOS )
                        printf ( " ( old = PASTENDPOS  new = %li )\n", original );

                else if ( original == PASTENDPOS )
                        printf ( " ( old = %li  new = PASTENDPOS )\n", copy );
                else
                        printf ( " ( old = %li  new = %li )\n", copy, original );
                }
}

#define CHECK_X_SMALL_VALUE(nameToBeChecked,format,cast) \
	if ( COPY.nameToBeChecked != COMPARE.nameToBeChecked ) \
		{				\
		PRINT_BANNER			\
		PrintDiffDesc ( PART, #nameToBeChecked );	\
                printf ( format, COPY.nameToBeChecked, COMPARE.nameToBeChecked ); \
		}


void PrintDiffs ( XmTextWidget toBeCompared )
{
	int i;
	XmTextPart textPart = toBeCompared->text;
	printedBanner = 0;
	if ( text != 0 && text != toBeCompared )
		/* for when I create another text I do not want to see them
			cross pollution each other */
		return;
#define CHECK_X_CHANGED(nameToBeChecked,cast) \
	if ( COPY.nameToBeChecked != COMPARE.nameToBeChecked ) \
		{				\
		PRINT_BANNER			\
		PrintDiffDesc ( PART, #nameToBeChecked );	\
		if ( ! COMPARE.nameToBeChecked ) \
			printf ( " is zero now\n" ); \
		else if ( ! COPY.nameToBeChecked ) \
			printf ( " is non-zero now\n" ); \
		else 		\
		    printf ( " has changed values\n" ); \
		}

#define CHECK_X_VALUE(nameToBeChecked,format,cast) \
	PrintInts ( PART, #nameToBeChecked, (long int)COPY.nameToBeChecked, (long int)COMPARE.nameToBeChecked );

#define CHECK_TIME(nameToBeChecked) \
	if ( COPY.nameToBeChecked != COMPARE.nameToBeChecked ) \
		{				\
		PRINT_BANNER			\
		PrintDiffDesc ( PART, #nameToBeChecked );	\
		PrintTime((Widget)toBeCompared, COMPARE.nameToBeChecked ); \
		}

#define CHECK_PTR_CHANGED(nameToBeChecked) \
	CHECK_X_CHANGED(nameToBeChecked, void* )

#define CHECK_PTR_VALUE(nameToBeChecked) \
	CHECK_X_VALUE(nameToBeChecked, " ( old = %p  new = %p )\n", void* )

#define CHECK_INT_VALUE(nameToBeChecked) \
	CHECK_X_VALUE(nameToBeChecked, " ( old = %i  new = %i )\n", int )

#define CHECK_LONG_VALUE(nameToBeChecked) \
	CHECK_X_VALUE(nameToBeChecked, " ( old = %li  new = %li )\n", long )

#define CHECK_CHAR_VALUE(nameToBeChecked) \
	CHECK_X_SMALL_VALUE(nameToBeChecked, " ( old = %i  new = %i )\n", char )

#define CHECK_STRING_VALUE(nameToBeChecked) \
	if ( COPY.nameToBeChecked != COMPARE.nameToBeChecked ) \
		{				\
		PRINT_BANNER			\
		PrintDiffDesc ( PART, #nameToBeChecked );	\
		printf ( " ( old = <%s>  new = <%s> )\n", 	\
			((char*)COPY.nameToBeChecked)? (char*)COPY.nameToBeChecked : "", \
			((char*)COMPARE.nameToBeChecked)? (char*)COMPARE.nameToBeChecked  : ""); \
		}

#define CHECK_SHORT_VALUE(nameToBeChecked) \
	CHECK_X_VALUE(nameToBeChecked, " ( old = %i  new = %i )\n", short )

#define COPY copyOfTextInternals
#define COMPARE textPart
#define PART "       "

	CHECK_PTR_CHANGED ( activate_callback )
	CHECK_PTR_CHANGED ( focus_callback )
	CHECK_PTR_CHANGED ( losing_focus_callback )
	CHECK_PTR_CHANGED ( value_changed_callback )
	CHECK_PTR_CHANGED ( modify_verify_callback )
	CHECK_PTR_CHANGED ( wcs_modify_verify_callback )
	CHECK_PTR_CHANGED ( motion_verify_callback )
	CHECK_PTR_CHANGED ( gain_primary_callback )
	CHECK_PTR_CHANGED ( lose_primary_callback )

	CHECK_PTR_CHANGED ( value )
	CHECK_PTR_CHANGED ( wc_value ) 
	CHECK_INT_VALUE ( margin_height )
	CHECK_INT_VALUE ( margin_width )

#define NITTY_GRITTY_POS
#ifdef NITTY_GRITTY_POS
	CHECK_INT_VALUE ( cursor_position_x )
#endif

	CHECK_PTR_CHANGED ( output_create )
	CHECK_PTR_CHANGED ( input_create )

	CHECK_INT_VALUE ( top_character )
	CHECK_INT_VALUE ( bottom_position )
	CHECK_INT_VALUE ( cursor_position )
	CHECK_INT_VALUE ( max_length )
	CHECK_INT_VALUE ( edit_mode )

	CHECK_CHAR_VALUE ( auto_show_cursor_position )
	CHECK_CHAR_VALUE ( editable )
	CHECK_CHAR_VALUE ( verify_bell )
	CHECK_CHAR_VALUE ( add_mode )
	CHECK_CHAR_VALUE ( traversed )
	CHECK_CHAR_VALUE ( in_redisplay )
	CHECK_CHAR_VALUE ( needs_redisplay )
	CHECK_CHAR_VALUE ( in_refigure_lines )
	CHECK_CHAR_VALUE ( needs_refigure_lines )
	CHECK_CHAR_VALUE ( in_setvalues )
	CHECK_CHAR_VALUE ( in_resize )
	CHECK_CHAR_VALUE ( in_expose )
	CHECK_CHAR_VALUE ( highlight_changed )
	if ( widgetInitialized && Text( toBeCompared ) . highlight_changed &&
			! copyOfTextInternals . highlight_changed  )
		ShowHighlightData ( toBeCompared );

	CHECK_CHAR_VALUE ( pendingoff )

	CHECK_CHAR_VALUE ( char_size )
	CHECK_CHAR_VALUE ( on_or_off )

	CHECK_INT_VALUE ( first_position )
	CHECK_INT_VALUE ( last_position )
	CHECK_INT_VALUE ( forget_past )
	CHECK_INT_VALUE ( force_display )
	CHECK_INT_VALUE ( new_top )
	CHECK_INT_VALUE ( last_top_char )
	CHECK_INT_VALUE ( dest_position )

	CHECK_INT_VALUE ( disable_depth )
	CHECK_INT_VALUE ( pending_scroll )
	CHECK_INT_VALUE ( total_lines )
	if ( widgetInitialized && textPart.line_table && textPart.total_lines > copyOfTextInternals.total_lines )
		for ( i = 0; i <= textPart.total_lines; i++ )
			{
			ShowLineTable ( toBeCompared, i, True );
			}
	else if ( widgetInitialized && textPart.line_table )
		{
		for ( i = 0; i < textPart.total_lines && i < 100; i++ )
			if ( copyOfLineTable[i].start_pos != textPart.line_table[i].start_pos )
				{
				ShowLineTable ( toBeCompared, i, True );
				}
		}
	CHECK_INT_VALUE ( top_line )
	CHECK_INT_VALUE ( vsbar_scrolling )

	CHECK_INT_VALUE ( number_lines )
/*
	if ( textPart.number_lines > copyOfTextInternals.number_lines )
		ShowLine ( toBeCompared, textPart.number_lines - 1, True );
*/
	CHECK_INT_VALUE ( maximum_lines )
	CHECK_PTR_CHANGED ( line )
	if ( widgetInitialized && textPart.line != copyOfTextInternals.line )
		{
		int i = (copyOfTextInternals.maximum_lines>0 ? copyOfTextInternals.maximum_lines : 0);
		for ( ; i < textPart.maximum_lines ; i ++ )
			{
			Line lineData = &textPart.line [ i ];
			if ( lineData -> changed_position != 0 )
				{
				static int isInitialized = 0;
				if ( isInitialized )
					{
					printf ("Motif does not intialize these properly so I am doing it so that it is easier to may comparisions later\n");
					isInitialized = 1;
					}
				lineData -> changed_position = 0;
				}
			}
		}
	if ( widgetInitialized  )
		for ( i = 0; i < textPart.maximum_lines ; i ++ )
			if ( copyOfLineRec[i].start != textPart.line[i].start
				|| copyOfLineRec[i].changed != textPart.line[i].changed
				|| copyOfLineRec[i].extra != textPart.line[i].extra
				|| copyOfLineRec[i].past_end != textPart.line[i].past_end
				|| copyOfLineRec[i].changed_position != textPart.line[i].changed_position )
				{
				PRINT_BANNER
				if ( copyOfLineRec[i].start >=  0 ) /*CP: This means that 
						the correct values have not yet been initialized here */
				    {	
				    if ( copyOfLineRec[i].start != textPart.line[i].start )
					printf ( " \t\t\t\tstart was %i\n", (int) copyOfLineRec[i].start);
				    if ( copyOfLineRec[i].changed != textPart.line[i].changed )
					printf ( " \t\t\t\tchanged was %s\n", 
							(copyOfLineRec[i].changed?"true":"false" ) );
				    if ( copyOfLineRec[i].changed_position != textPart.line[i].changed_position )
					printf ( " \t\t\t\tchanged_position was %i\n",  
							(int)copyOfLineRec[i].changed_position );
				    if ( copyOfLineRec[i].past_end != textPart.line[i].past_end )
					printf ( " \t\t\t\tpast_end was %s\n", 
							(copyOfLineRec[i].past_end?"true":"false" ) );
				    if ( copyOfLineRec[i].extra != textPart.line[i].extra )
					{
					printf ( " \t\t\t\textra ptr changed " );
					if ( i > 0 && textPart.line[i].extra == copyOfLineRec[i-1].extra )
						printf ( "( copy of prev line's extra !)" );
					if ( i > 0 && textPart.line[i].extra == copyOfLineRec[i+1].extra )
						printf ( "( copy of next line's extra !)" );
					printf ( "\n" );
					}
				    }
				ShowLine  ( toBeCompared, i, True );
				}

	CHECK_PTR_CHANGED ( repaint.range )
	CHECK_PTR_CHANGED ( highlight.list )
	CHECK_PTR_CHANGED ( old_highlight.list )
	CHECK_PTR_CHANGED ( inner_widget )

	CHECK_PTR_CHANGED ( line_table )
	CHECK_INT_VALUE ( table_size )
	CHECK_INT_VALUE ( table_index )
	CHECK_INT_VALUE ( repaint.number )
	CHECK_INT_VALUE ( repaint.maximum )
	if ( widgetInitialized && textPart.repaint.number != copyOfTextInternals.repaint.number )
		ShowRangeData ( toBeCompared, True, True );
		
	CHECK_INT_VALUE ( highlight.number )
	if ( widgetInitialized && textPart.highlight.number != copyOfTextInternals.highlight.number )
		ShowHighlightData ( toBeCompared );
	if ( ! widgetInitialized )
		{
		CopyTextInternals ( toBeCompared );
		return;
		}

#undef COPY
#define COPY copyOfRangeAtIndex0
#undef COMPARE
#define COMPARE textPart.repaint.range[0]
#undef PART
#define PART "range[0] "
		if ( widgetInitialized && textPart.repaint.number > 0 )
			{
			CHECK_INT_VALUE ( from )
			CHECK_INT_VALUE ( to )
			}

#undef COPY
#define COPY copyOfTextSourceInternals
#undef COMPARE
#define COMPARE (*textPart.source->data)
#undef PART
#define PART "source "

	CHECK_INT_VALUE ( left )
	CHECK_INT_VALUE ( right )

	if ( argStringValues )
		{
		CHECK_STRING_VALUE ( ptr )
		CHECK_STRING_VALUE ( value )
		CHECK_STRING_VALUE ( gap_start )
		CHECK_STRING_VALUE ( gap_end )
		}

	CHECK_INT_VALUE ( length )
	CHECK_INT_VALUE ( maxlength )
	CHECK_INT_VALUE ( old_length )
	CHECK_INT_VALUE ( maxallowed )
	CHECK_TIME ( prim_time )

/*
	CHECK_CHAR_VALUE ( PSWC_NWLN[0] )
*/
	if ( copyOfTextSourceInternals.PSWC_NWLN && 
		copyOfTextSourceInternals.PSWC_NWLN[0] != textPart.source->data->PSWC_NWLN[0] )
		{
		PRINT_BANNER
		PrintDiffDesc ( PART, "PSWC_NWLN[0]" );
                printf (  "( old = %i  new = %i )\n", 
			copyOfTextSourceInternals.PSWC_NWLN[0],
			textPart.source->data->PSWC_NWLN[0] );
		}
	else if ( ! copyOfTextSourceInternals.PSWC_NWLN && textPart.source->data->PSWC_NWLN[0])
		{
		PRINT_BANNER
		PrintDiffDesc ( PART, "PSWC_NWLN[0]" );
                printf (  "( old = <>  new = %i )\n", 
			textPart.source->data->PSWC_NWLN[0] );
		}

	CHECK_CHAR_VALUE ( hasselection )
	if ( widgetInitialized && GetSrc ( toBeCompared ) -> data -> hasselection &&
			! copyOfTextSourceInternals . hasselection  )
		ShowSelectionData ( toBeCompared );

	CHECK_CHAR_VALUE ( editable )

#undef COPY
#define COPY copyOfTextInputInternals
#undef COMPARE
#define COMPARE (*textPart.input->data)
#undef PART
#define PART "input  "

	CHECK_INT_VALUE ( new_sel_length )
	CHECK_INT_VALUE ( threshold )
	CHECK_CHAR_VALUE ( selectionHint.x )
	CHECK_CHAR_VALUE ( selectionHint.y )
	CHECK_CHAR_VALUE ( Sel2Hint.x )
	CHECK_CHAR_VALUE ( Sel2Hint.y )
	CHECK_INT_VALUE  (select_id )
	CHECK_CHAR_VALUE ( stype )
	CHECK_CHAR_VALUE ( extendDir )
	CHECK_CHAR_VALUE ( Sel2ExtendDir )
	CHECK_INT_VALUE ( origLeft )
	CHECK_INT_VALUE ( origRight )
	CHECK_INT_VALUE ( Sel2OrigLeft )
	CHECK_INT_VALUE ( Sel2OrigRight )
	CHECK_INT_VALUE ( stuffpos )
	CHECK_INT_VALUE ( sel2Left )
	CHECK_INT_VALUE ( sel2Right )
	CHECK_INT_VALUE ( anchor )
	CHECK_INT_VALUE ( select_pos_x )
	CHECK_INT_VALUE ( select_pos_y )
	CHECK_CHAR_VALUE ( pendingdelete )
	CHECK_CHAR_VALUE ( syncing )
	CHECK_CHAR_VALUE ( extending )
	CHECK_CHAR_VALUE ( Sel2Extending )
	CHECK_CHAR_VALUE ( hasSel2 )
	if ( widgetInitialized && Text_InputData( toBeCompared ) -> hasSel2 &&
			! copyOfTextInputInternals.hasSel2  )
		ShowSecondarySelectionData ( toBeCompared );

	CHECK_CHAR_VALUE ( has_destination )
	CHECK_CHAR_VALUE ( selectionMove )
	CHECK_CHAR_VALUE ( cancel )
	CHECK_CHAR_VALUE ( overstrike )
	CHECK_CHAR_VALUE ( sel_start )
	CHECK_TIME ( dest_time )
	CHECK_TIME ( sec_time )
	CHECK_TIME ( lasttime )

#undef COPY
#define COPY copyOfTextOutputInternals
#undef COMPARE
#define COMPARE (*textPart.output->data)
#undef PART
#define PART "output "

	CHECK_PTR_CHANGED ( fontlist )
	CHECK_INT_VALUE ( blinkrate )
	CHECK_CHAR_VALUE ( wordwrap )
	CHECK_CHAR_VALUE ( cursor_position_visible )
	CHECK_CHAR_VALUE ( autoshowinsertpoint )
	CHECK_CHAR_VALUE ( hasfocus )
	CHECK_CHAR_VALUE ( has_rect )
	CHECK_CHAR_VALUE ( handlingexposures )
	CHECK_CHAR_VALUE ( exposevscroll )
	CHECK_CHAR_VALUE ( exposehscroll )
	CHECK_CHAR_VALUE ( resizewidth )
	CHECK_CHAR_VALUE ( resizeheight )
	CHECK_CHAR_VALUE ( scrollvertical )
	CHECK_CHAR_VALUE ( scrollhorizontal )
	CHECK_CHAR_VALUE ( scrollleftside )
	CHECK_CHAR_VALUE ( scrolltopside )
	CHECK_CHAR_VALUE ( ignorevbar )
	CHECK_CHAR_VALUE ( ignorehbar )


	if ( argShowDrawCursor )
		{
		CHECK_SHORT_VALUE ( cursor_on )
		CHECK_CHAR_VALUE ( refresh_ibeam_off )
		CHECK_CHAR_VALUE ( blinkstate )
		}

	CHECK_CHAR_VALUE ( suspend_hoffset )
	CHECK_CHAR_VALUE ( use_fontset )
	CHECK_CHAR_VALUE ( have_inverted_image_gc )

	CHECK_INT_VALUE ( insertx )
	CHECK_INT_VALUE ( inserty )

	CHECK_INT_VALUE ( number_lines )
	CHECK_INT_VALUE ( leftmargin )
	CHECK_INT_VALUE ( rightmargin )
	CHECK_INT_VALUE ( topmargin )
	CHECK_INT_VALUE ( bottommargin )

	CHECK_INT_VALUE ( scrollwidth )

	CHECK_INT_VALUE ( vsliderSize )
	CHECK_INT_VALUE ( hoffset )
	CHECK_INT_VALUE ( averagecharwidth )
	CHECK_INT_VALUE ( tabwidth )
	CHECK_CHAR_VALUE ( columns )
	CHECK_CHAR_VALUE ( rows )
	CHECK_INT_VALUE ( lineheight )
	CHECK_INT_VALUE ( minwidth )
	CHECK_INT_VALUE ( minheight )
	CHECK_INT_VALUE ( prevW )
	CHECK_INT_VALUE ( prevH )
	CHECK_INT_VALUE ( cursorwidth )
	CHECK_INT_VALUE ( cursorheight )
	CHECK_INT_VALUE ( font_ascent )
	CHECK_INT_VALUE ( font_descent )
	CHECK_PTR_CHANGED ( timerid )

	if ( argShowDrawCursor )
		{
		CHECK_INT_VALUE ( cursor )
		CHECK_INT_VALUE ( add_mode_cursor )
		CHECK_INT_VALUE ( ibeam_off )
		CHECK_INT_VALUE ( stipple_tile )
		CHECK_PTR_VALUE ( gc )
		CHECK_PTR_VALUE ( imagegc )
		CHECK_PTR_CHANGED ( save_gc )
		}

	CHECK_PTR_CHANGED ( vbar )
	CHECK_PTR_CHANGED ( hbar )
	CHECK_PTR_CHANGED ( font )
	CHECK_CHAR_VALUE ( columns_set )
	CHECK_CHAR_VALUE ( rows_set )
/* what the hell is this !!! 
	CHECK_CHAR_VALUE ( fontlist_created )
*/
	CopyTextInternals ( toBeCompared );
	if ( ! printedBanner )
		printf ( "\tNo difference between text with previous saved version:\n" );
}

void PrintLead()
{
	int i ;
	for ( i = 0; i < indent ; i++ )
		printf ( "->" );
}

void ShowLine (  XmTextWidget text, LineNum lineNum, int calledFromDiff )
{
	Line lineData ;
	if ( (int)lineNum < 0 )
		return;

	lineData = &Text_Line(text) [ lineNum ];
	indent ++;
	if ( calledFromDiff )
		printf ( "\t\t\t" );
	else
		PrintLead();
	printf ( "Line : no %i : ", lineNum );

	if ( lineData -> start != PASTENDPOS )
		{
		printf ( "start %i  changed %s  changed_position %i  past_end %s ",
			(int)lineData -> start, ( lineData->changed ? "yes" : "no " ),
			(int)lineData->changed_position, ( lineData->past_end ? "yes" : "no " ) );
		if ( lineData -> extra  )
			{
			/* printf ( "extra ( NOT-SHOWING-WIDTH " ); */
			printf ( "extra ( width = %i ", (int)lineData -> extra->width );
			if ( lineData -> extra -> wrappedbychar )
				printf ( "wrappedByChar = %c", (int)lineData -> extra->wrappedbychar );
			printf ( ")" );
			}
		}
	else
		{
		printf ( "empty" );
		}
	printf ( "\n" );
	indent --;
}

void ShowLineTable (  XmTextWidget text, LineNum lineNum, int calledFromDiff )
{
	XmTextLineTableRec lineData = Text_LineTable(text) [ lineNum ];
	indent ++;
	if ( calledFromDiff )
		printf ( "\t\t\t" );
	else
		PrintLead();
	printf ( "LineTable : no %i : ", lineNum );
	if ( lineData.start_pos != PASTENDPOS )
		{
		printf ( "start %i  \n", (int)lineData . start_pos );
		}
	else
		{
		printf ( "empty\n" );
		}
	indent --;
}


void ShowSecondarySelectionData ( XmTextWidget text )
{
	if ( Text_InputData( text ) -> hasSel2 )
		{
		PrintLead();
		printf ( " Secondary Selection : hint ( x = %i, y = %i )\n", 
			Text_InputData( text ) -> Sel2Hint.x, 
			Text_InputData( text ) -> Sel2Hint.y );
		PrintLead();
		printf ( " Secondary Selection : original ( left = %i, right = %i )\n", 
			(int) Text_InputData( text ) -> Sel2OrigLeft, 
			(int) Text_InputData( text ) -> Sel2OrigRight );
		PrintLead();
		printf ( " Secondary Selection : current ( left = %i, right = %i )\n", 
			(int) Text_InputData( text ) -> sel2Left,
			(int) Text_InputData( text ) -> sel2Right );
		PrintLead();
		printf ( " Secondary Selection : extending = %i, time of ownership = %li\n", 
			Text_InputData( text ) -> Sel2Extending,
			Text_InputData( text ) -> sec_time );
		}
}

void ShowHighlightData ( XmTextWidget  text )
{
	int i = 0;
	PrintLead();
	printf ( " Selection : highlight ( number = %i, max = %i ), changed = %s\n", 
		Text_Highlight( text ) . number, Text_Highlight( text ) . maximum,
		Text ( text ).highlight_changed ?  "yes" : "no " );

	while ( i < Text_Highlight( text ) . number )
			{
			PrintLead();
			printf ( " Selection : highlight no %i ( pos %i, mode %i ) )\n", 
				i+1, 
				(int)Text_Highlight( text ) . list[i].position, 
				Text_Highlight( text ) . list[i].mode );
			i++;
			} 


	PrintLead();
	printf ( " Selection : old highlight ( number = %i, max = %i )\n", 
		Text_OldHighlight( text ) . number, Text_OldHighlight( text ) . maximum );
	i = 0;
	while ( i < Text_OldHighlight( text ) . number && Text_OldHighlight( text ) . maximum )
			{
			PrintLead();
			printf ( " Selection : old highlight no %i ( pos %i, mode %i ) )\n", 
				i+1, 
				(int) Text_OldHighlight( text ) . list[i].position, 
				Text_OldHighlight( text ) . list[i].mode );
			i++;
			} 
}


void ShowSelectionData ( XmTextWidget  text )
{
	if ( GetSrc ( text ) -> data -> hasselection )
		{
		PrintLead();
		printf ( " Selection : length = %i, button hint ( x = %i, y = %i ), extend %s\n", 
			Text_InputData( text ) -> new_sel_length, 
			Text_InputData( text ) -> selectionHint.x,
			Text_InputData( text ) -> selectionHint.y,
			Text_InputData( text ) -> extending ? 
				( Text_InputData( text ) -> extendDir == XmsdLeft ? "left " : "right" )
				: "no" 
				);

		PrintLead();
		printf ( " Selection : anchor = %i, original ( left = %i, right = %i )\n", 
			(int) Text_InputData( text ) -> anchor,
			(int) Text_InputData( text ) -> origLeft,
			(int) Text_InputData( text ) -> origRight );

		ShowHighlightData ( text );
		}
}

void ShowAllSelectionData ( XmTextWidget text )
{
	ShowSelectionData ( text );
	ShowSecondarySelectionData ( text );
}

void PrintTextVerifyPtr ( Widget text, String callback, XmTextVerifyPtr data )
{
	PrintDiffs ( (XmTextWidget)text );
	PrintLead();
	printf ( "callback %s", callback );
	switch ( data -> reason )
		{
		case XmCR_LOSING_FOCUS:
			printf ( " : reason XmCR_LOSING_FOCUS\n");
			break;
		case XmCR_MODIFYING_TEXT_VALUE:
			printf ( " : reason XmCR_MODIFYING_TEXT_VALUE\n");
			break;
		case XmCR_MOVING_INSERT_CURSOR:
			printf ( " : reason XmCR_MOVING_INSERT_CURSOR\n");
			break;
		default :
			printf ( "PrintTextVerifyPtr: INVALID REASON \n\n" );
		}

	printf ( "\tcurrInsert = %i\tnewInsert = %i\n", 
		(int) data ->currInsert, (int) data ->newInsert );

	if ( data -> reason != XmCR_MOVING_INSERT_CURSOR )
		printf ( "\tstartPos   = %i\tendPos = %i\n", 
			(int) data ->startPos, (int) data ->endPos );

	if ( data -> reason == XmCR_MODIFYING_TEXT_VALUE )
		printf ( "\ttext\n\t\tlength = %i\t\tformat = %li\n\t\tptr = %s\n",
			 data->text->length, data->text->format, data->text->ptr );

	ShowAllSelectionData ( (XmTextWidget)text );
	fflush (NULL);
}

void	ShowRangeData ( 
			XmTextWidget text,
			int fromComparision,
			Boolean maxOrNum )
{
	int i;
	int upto = maxOrNum ? text->text.repaint.maximum : text->text.repaint.number;
	for ( i = 0; i < upto; i++ )
		{
		if ( fromComparision )
			printf ( "\t\t" );
		else
			PrintLead();
		printf ( " Range : no. %d from position %i to position %i\n",
					(int)i,
					(int)text->text.repaint.range[i].from,
					(int)text->text.repaint.range[i].to );
		}
}

void	TextActionHook (
						Widget		w,
						XtPointer	clientData_unused,
						String		actionName,
						XEvent*		event,
						String*		params,
						Cardinal*	noOfParams )
{
	if ( ! XmIsText( w ) )
		return;
	if ( ( strncmp ( actionName, "focus", strlen("focus")) == 0 )
		|| ( strcmp ( actionName, "enter" ) == 0 )
		|| ( strcmp ( actionName, "PrimitiveFocusIn" ) == 0 )
		|| ( strcmp ( actionName, "leave" ) == 0 ) )
		return;

	PrintDiffs ( (XmTextWidget)w );
	indent ++;
	PrintLead();
	printf ("action %s called", actionName );
	if ( *noOfParams > 0 )
		{
		int i=0;
		printf (" with %i paramaters :", *noOfParams );
		while ( i < *noOfParams )
			{
			printf ("%s:", params[i] );
			i++;
			}
		}
	printf ("\n" );
/*	ShowAllSelectionData ( (XmTextWidget)w );
*/
	fflush (NULL);
	indent --;
}

/*-------------------------------------------------------------
**	ModifyVerifyCB
**		The text in the text widget has been altered.
*/
/*ARGSUSED*/
void	TextModifyVerifyCB (
				Widget		w,
				XtPointer	unused_client_data,
				XmTextVerifyPtr call_data )
{
	indent ++;
	PrintTextVerifyPtr ( w, "TextModifyVerifyCB", call_data );
	indent --;
}


static void	TextFocusCB (
				Widget		w,	
				Boolean		client_data,
				XmAnyCallbackStruct*	call_data )
	{
	static XtActionHookId actionHookId = 0;
	indent ++;
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "TextFocusCB start : %s focus\n", 
			(call_data -> reason == XmCR_LOSING_FOCUS ? "loosing" : "gaining" )  );
	if ( call_data -> reason == XmCR_LOSING_FOCUS )
		XtRemoveActionHook ( actionHookId );
	else
		actionHookId = XtAppAddActionHook (
				app_context, (XtActionHookProc)TextActionHook, NULL );
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "TextFocusCB stop\n" );
	indent --;
	}






/*-------------------------------------------------------------
**	MotionCB
**		Text widget motion callback.
*/
static void TextMotionCB (
				Widget			w,
				XtPointer		client_data,
				XmTextVerifyPtr	call_data )
	{
	PrintTextVerifyPtr ( w, "TextMotionCB", call_data );
	}




/*-------------------------------------------------------------
**	TextValueChangedCB 
**		Text widget motion callback.
*/
void TextValueChangedCB  (
				Widget			w,
				XtPointer		client_data,
				XmAnyCallbackStruct*	call_data )
	{
	indent ++;
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "TextValueChangedCB" );
	switch ( call_data -> reason )
		{
		case XmCR_LOSING_FOCUS:
			printf ( " : reason XmCR_LOSING_FOCUS\n");
			break;
		case XmCR_MODIFYING_TEXT_VALUE:
			printf ( " : reason XmCR_MODIFYING_TEXT_VALUE\n");
			break;
		case XmCR_MOVING_INSERT_CURSOR:
			printf ( " : reason XmCR_MOVING_INSERT_CURSOR\n");
			break;
		case XmCR_VALUE_CHANGED:
			printf ( " : reason XmCR_VALUE_CHANGED\n");
			break;
		default :
			printf ( ": INVALID REASON %i\n\n", call_data -> reason );
		}
	indent --;
	}


void MyDraw  (
					XmTextWidget		wid,
					LineNum				lineNum,
					XmTextPosition		pos1,
					XmTextPosition		pos2,
					XmHighlightMode		high)
	{
	indent ++;

	PrintDiffs ( wid );

	PrintLead();
	printf ("Draw: start\n" );

	ShowRangeData ( wid, 0, False );

	if ( ( DefaultDraw != NULL ) && ( DefaultDraw != MyDraw ) )
		DefaultDraw ( wid, lineNum, pos1, pos2, high );
	PrintDiffs ( wid );

	PrintLead();
	printf ("Draw: lineNum = %i  left = %i  right = %i  highlightMode = %i\n",
		lineNum, (int) pos1, (int) pos2, high) ;
	PrintLead();
	printf ("Draw: booleans :") ;

	if ( Text_OutputData(wid)-> handlingexposures )
		printf (" handlingExposures, ") ;
		
	if ( Text(wid). in_redisplay )
		printf (" in_redisplay, ") ;
		
	if ( Text(wid). needs_redisplay )
		printf (" needs_redisplay, ") ;
		
	if ( Text(wid). in_refigure_lines )
		printf (" in_refigure_lines, ") ;
		
	if ( Text(wid). needs_refigure_lines )
		printf (" needs_refigure_lines, ") ;
		
	if ( Text(wid). in_resize )
		printf (" in_resize, ") ;
		
	if ( Text(wid). in_expose )
		printf (" in_expose, ") ;
		
	if ( Text(wid). highlight_changed )
		printf (" highlight_changed, ") ;
				
	printf ("\n") ;
	indent --;
	}



XmTextStatus MyReplaceProc(
    		XmTextWidget wid,
			XEvent *ev,
    		XmTextPosition *start,
    		XmTextPosition *end,
    		XmTextBlock block,
		Boolean callcallback )
{
	LineNum lineNum;
	XmTextStatus status;
	XmTextPosition oldStart = *start, oldEnd = *end;
	indent ++;
	PrintDiffs ( wid );
	PrintLead();
	printf ( "ReplaceProc : start = %i  end = %i\n", (int) *start,(int) *end);
	printf ( "\t\tblock  ( length = %i  ptr =\"%s\" ) :\n",
			 block->length, block->ptr );

	lineNum = _XmTextPosToLine( wid, *start );
	if ( lineNum - Text_TopLine(wid) > Out_Rows ( Text_OutputData(wid) ) )
		printf ( "*****CRAZY value for _XmTextPosToLine( wid, %i ) = %i\n", 
					(int)*start, lineNum );
	else
		{
		ShowLine ( wid, lineNum, False );
		if ( lineNum > 0 )
			ShowLine ( wid, lineNum-1, False );
		}
	status = DefaultReplace ( wid, ev, start, end, block, callcallback );
	PrintDiffs ( wid );
	lineNum = _XmTextPosToLine( wid, *start );
	if ( lineNum - Text_TopLine(wid) > Out_Rows ( Text_OutputData(wid) ) )
		printf ( "*****CRAZY value for _XmTextPosToLine( wid, %i ) = %i\n", 
					(int)*start, lineNum );
	else
		{
		ShowLine ( wid, lineNum, False );
		if ( lineNum > 0 )
			ShowLine ( wid, lineNum-1, False );
		}

	PrintLead();
	printf ( "ReplaceProc : start = %i%s  end = %i%s",
			(int) *start, (*start != oldStart)?"CHANGED":"",
			(int) *end, (*end != oldEnd)?"CHANGED":"" );
	printf ( "  block  ( length = %i  ptr =\"%s\" ) %s : ",
			 block->length, block->ptr,
			(callcallback ? "call-callback" : "no-callback") );
	switch ( status )
		{
		case EditDone : printf ( "EditDone\n" ); break;
		case EditError : printf ( "EditError\n" ); break;
		case EditReject : printf ( "EditReject\n" ); break;
		}

	indent --;
	return status;
}



XmTextPosition MyReadProc(
    		XmTextSource source,
    		XmTextPosition start,
    		XmTextPosition end,
    		XmTextBlock block )
{
	char *str;
	XmTextPosition result ;
	indent ++;
	PrintDiffs ( source->data->widgets[0] );
	PrintLead();
	printf ( "ReadProc : start = %i  end = %i\n", (int) start,(int) end);
	result = DefaultRead ( source, start, end, block );
	PrintDiffs ( source->data->widgets[0] );
	PrintLead();
	str = XtMalloc ( sizeof (char) * (block->length + 1 ));
	strncpy ( str, block->ptr, block->length );
	str [ block->length ] = 0;
	printf ( "ReadProc : block  ( length = %i  ptr =\"%s\" ) : result = %i\n",
			 block->length, str, (int)result );
	XtFree ( str );
	indent --;
	return result;
}

Boolean MyGetSelectionProc  (
    		XmTextSource source,
    		XmTextPosition* left,
    		XmTextPosition* right
			)
{
	Boolean retStatus ;
	indent ++;
	PrintDiffs ( source->data->widgets[0] );
	PrintLead();
	printf ( "GetSelectionProc: start\n" );
#ifdef SHOW_INITIAL_VALUES_FOR_IO_PARAMS
	printf ( "\t\tinital values for *left = %i  *right = %i \n", (int)*left, (int)*right );
#endif
	retStatus = DefaultGetSelectionProc ( source, left, right );
	PrintDiffs ( source->data->widgets[0] );
	PrintLead();
	printf ( "GetSelectionProc: returned " );
	if ( retStatus )
		printf ( "left = %i  right = %i \n", (int)*left, (int)*right );
	else
		printf ( "false\n" );
	indent --;
	return retStatus;
}

Boolean MyMeasureLineProc ( 
			XmTextWidget wid,
			LineNum line,
			XmTextPosition pos,
			XmTextPosition *nextPos,
			LineTableExtraRec **extra)
{
	Line lineData = &Text_Line(text) [ line ];
	Boolean retStatus ;
	indent ++;
	PrintDiffs ( wid );
	PrintLead();
	printf ( "MeasureLineProc: start : pos = %i  \n", (int)pos );
	if ( ! lineData -> past_end )
		printf ( "\t\tinital values for *nextPos = %i\n", nextPos==0 ? 0 : (int)*nextPos );
	ShowLine ( wid, line, False );

	if ( lineData -> start != PASTENDPOS && ! lineData -> past_end && line < wid->text.maximum_lines - 1 )
		ShowLine ( wid, line+1, False );

	retStatus = DefaultMeasureLineProc ( wid, line, pos, nextPos, extra );
	PrintDiffs ( wid );
	ShowLine ( wid, line, False );
	if ( lineData -> start != PASTENDPOS && ! lineData -> past_end && line < wid->text.maximum_lines - 1 )
		ShowLine ( wid, line+1, False );
	PrintLead();
	printf ( "MeasureLineProc: line = %i  pos = %i : returned = %s  ",
			(int)line, (int)pos, retStatus == 1 ? "true " : "false"  );
	if ( nextPos )
		{
		if ( retStatus == True)
			printf ( "nextPos = %i ", (int)*nextPos );
		}
	else
		printf ( "nextPos = NULL " );

	if ( extra && *extra )
		{
		/* printf ( "extra ( NOT-SHOWING-WIDTH " ); */
		printf ( "extra ( width = %i ", (int)(*extra)->width );
		if ( (*extra) -> wrappedbychar )
			printf ( "wrappedByChar = %c", (int)(*extra)->wrappedbychar );
		printf ( ")" );
		}

	printf ( "\n" );
	indent --;
	return retStatus;
}

void MyDrawInsertionPointProc(
			XmTextWidget text,
			XmTextPosition pos,
			OnOrOff onOrOff )
{
	indent ++;
	PrintDiffs ( text );
	PrintLead();
	printf ( "DrawInsertionPointProc: start pos = %i :  %s \n",
			(int)pos, onOrOff == on ? "on" : "off"  );

	DefaultDrawInsertionPoint ( text, pos, onOrOff );	
	PrintDiffs ( text );
	PrintLead();
	printf ( "DrawInsertionPointProc: stop \n" );
	indent --;
}

void MyMakePositionVisibleProc(
			XmTextWidget text,
			XmTextPosition pos )
{
	indent ++;
	PrintDiffs ( text );
	PrintLead();
	printf ( "MakePositionVisibleProc: start pos = %i \n", (int)pos  );

	DefaultMakePositionVisible ( text, pos );	
	PrintDiffs ( text );
	PrintLead();
	printf ( "MakePositionVisibleProc: stop\n" );
	indent --;
}

/* Text would like to move some lines around on the screen.  It would like to
 * move lines fromline through toline (inclusive) to now start at line
 * destline.  If output can perform this move by means of a XCopyArea or
 * similar simple call, it does so and returns TRUE; otherwise, it will return
 * FALSE.  If TRUE, output should modify affected values in the
 * "extra" entries in the linetable, because after returning text will go ahead
 * and move linetable entries around.
 */
Boolean MyMoveLinesProc(
			XmTextWidget text,
			LineNum from,
			LineNum to,
			LineNum dest )
{
	Boolean retStatus ;	
	indent ++;
	PrintDiffs ( text );
	PrintLead();
	printf ( "MoveLinesProc: start\n" );
	retStatus = DefaultMoveLines ( text, from, to, dest );	
	PrintDiffs ( text );

	PrintLead();
	printf ( "MoveLinesProc: from = %i  to = %i  destination = %i  : possible = %s\n", 
			(int)from, (int)to, (int)dest, retStatus ? "yes" : "no"  );
	indent --;
	return retStatus;
}


void MySetSelectionProc  (
    		XmTextSource source,
    		XmTextPosition left,
    		XmTextPosition right,
    		Time time
			)
{
	XmTextSourceRec* src ;
	src = (XmTextSourceRec*)source;
	PrintDiffs ( source->data->widgets[0] );
	indent ++;
	PrintLead();
	printf ( "SetSelectionProc: left = %i  right = %i  time =", (int)left, (int)right );
	PrintTime ( (Widget)source->data->widgets[0], time );
	DefaultSetSelectionProc ( source, left, right, time );
	PrintDiffs ( source->data->widgets[0] );

	PrintLead();
	printf ( "SetSelectionProc: stop\n" );
	indent --;
}


void MyInputInvalidateProc (
			XmTextWidget  ctx,
			XmTextPosition position,
			XmTextPosition topos,
			long     delta		)
{
	indent ++;
	PrintDiffs ( ctx );
	PrintLead();
	printf ( "InInvalidateProc: start - position = %i  topos = %i  : delta = %li\n",
				(int)position, (int)topos, delta );
	DefaultInputInvalidateProc ( ctx, position, topos, delta );
	PrintDiffs ( ctx );
	PrintLead();
	printf ( "InInvalidateProc: stop\n" );
 	indent --;
}

void MyOutputInvalidateProc (
			XmTextWidget  ctx,
			XmTextPosition position,
			XmTextPosition topos,
			long     delta		)
{
	indent ++;
	PrintDiffs ( ctx );
	PrintLead();
	printf ( "OutputInvalidateProc: start - position = %i  topos = %i  : delta = %li\n",
				(int)position, (int)topos, delta );
	DefaultOutputInvalidateProc ( ctx, position, topos, delta );
	PrintDiffs ( ctx );
	PrintLead();
	printf ( "OutInvalidateProc: stop\n"  );
	indent --;
}


int MyCountLines (
     		XmTextSource source,
     		XmTextPosition start,
     		unsigned long length)
{
	int result;
	PrintDiffs ( source->data->widgets[0] );
	indent ++;
	PrintLead();
	printf ( "CountLines: start\n" );
	result = DefaultCountLines ( source, start, length );
	PrintLead();
	printf ( "CountLines: start = %i  length = %li  : result = %i", (int) start, length, result );
	indent --;
	return result;
}


void MyGetSecResData (
     		XmSecondaryResourceData *secResData)
{
	indent ++;
	PrintLead();
	printf ( "GetSecResData: start\n" );

	DefaultInputGetSecResDataProc ( secResData );
/*	PrintDiffs ( (XmTextWidget)w );
*/
	PrintLead();
	printf ( "GetSecResData: for resource %s\n", (*secResData)->name );
	indent --;
}



XmTextPosition MyScanProc(
    		XmTextSource source,
    		XmTextPosition pos,
    		XmTextScanType type,
    		XmTextScanDirection dir,	/* Either XmsdLeft or XmsdRight. */
    		int count,
    		Boolean include )
{
	XmTextPosition result;
	indent ++;
	PrintDiffs ( source->data->widgets[0] );
	PrintLead();
	printf ( "Scan: start\n" );
	result = DefaultScanProc ( source, pos, type, dir, count, include );
	PrintDiffs ( source->data->widgets[0] );
	PrintLead();
	printf ( "Scan: curPos = %i  fromPosition = %i  type = ", (int)Source_CurPos(source), (int) pos );
	switch ( type )
		{
		case XmSELECT_POSITION:
			printf ( "POSITION" );
			break;
		case XmSELECT_WHITESPACE:
			printf ( "WHITESPACE" );
			break;
		case XmSELECT_WORD:
			printf ( "WORD" );
			break;
		case XmSELECT_LINE:
			printf ( "LINE" );
			break;
		case XmSELECT_ALL:
			printf ( "ALL" );
			break;
		case XmSELECT_PARAGRAPH:
			printf ( "PARAGRAPH" );
			break;
		default:
			printf ( "**UNKNOWN**" );
			break;
		}
	printf ( "  direction = %s  count = %i  include = %s : ",
			dir == XmsdLeft ? "left " : "right",
			count,
			include == 1 ? "yes" : "no " );
	printf ( " result = %i\n", (int) result );

	indent --;
	return result;
}


void MyClassInitialize()
{
	indent++;
	PrintLead();
	printf ( "ClassInitialize: start\n" );
	DefaultClassInitialize();
	PrintLead();
	printf ( "ClassInitialize: stop\n" );
	indent--;
}

void  MyClassPartInitialize ( WidgetClass wclass)
{
	indent++;
	PrintLead();
	printf ( "ClassPartInitialize: start\n" );
	DefaultClassPartInitialize ( wclass );
	PrintLead();
	printf ( "ClassPartInitialize: stop\n" );
	indent--;
}

void MyOutputCreate( Widget w, ArgList al, Cardinal num)
{
	indent++;
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "OutputCreate: start\n" );
	DefaultOutputCreate ( w,  al, num );
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "OutputCreate: stop\n" );
	indent--;

#ifdef INITIALIAZE_MYPROCS_AT_OUTPUT_CREATION_PROC
	AddMyProcs ( w );
#endif

}

void AddMyProcs( XmTextWidget textWid )
	{

	DefaultDraw = textWid->text.output->Draw;
	textWid->text.output->Draw = MyDraw;

	DefaultMeasureLineProc = textWid->text.output->MeasureLine;
	textWid->text.output->MeasureLine = MyMeasureLineProc;

	if ( argShowDrawCursor )
		{
		DefaultDrawInsertionPoint = textWid->text.output->DrawInsertionPoint;
		textWid->text.output->DrawInsertionPoint =  (DrawInsertionPointProc)MyDrawInsertionPointProc ;
		}

	DefaultInputGetSecResDataProc = textWid->text.input->GetSecResData ;
	textWid->text.input->GetSecResData =  MyGetSecResData;

	DefaultInputInvalidateProc = textWid->text.input-> Invalidate;
	textWid->text.input->Invalidate =  MyInputInvalidateProc;

	DefaultMakePositionVisible = textWid->text.output->MakePositionVisible;
	textWid->text.output->MakePositionVisible =  MyMakePositionVisibleProc;

	DefaultMoveLines = textWid->text.output->MoveLines ;
	textWid->text.output->MoveLines =  MyMoveLinesProc;

	DefaultXYToPos =  textWid->text.output-> XYToPos;
	textWid->text.output-> XYToPos = MyXYToPos;

	DefaultPosToXY =  textWid->text.output-> PosToXY;
	textWid->text.output-> PosToXY = MyPosToXY;

	DefaultGetPreferredSize =  textWid->text.output->GetPreferredSize ;
	textWid->text.output->GetPreferredSize  = MyGetPreferredSize;

	DefaultOutputSetValues =  textWid->text.output-> SetValues;
	textWid->text.output->SetValues  = MyOutputSetValues;

	DefaultOutputRealize =  textWid->text.output-> realize;
	textWid->text.output->realize  = MyOutputRealize;

	DefaultOutputResize =  textWid->text.output-> resize;
	textWid->text.output->resize  = MyOutputResize;

	DefaultOutputExpose =  textWid->text.output-> expose;
	textWid->text.output->expose  = MyOutputExpose;

	DefaultScanProc = textWid->text.source->Scan;
	textWid->text.source->Scan = MyScanProc;
	
	DefaultCountLines = textWid->text.source->CountLines;
	textWid->text.source->CountLines = MyCountLines;
	
	DefaultGetSelectionProc = textWid->text.source->GetSelection;
	textWid->text.source->GetSelection = MyGetSelectionProc;
	
	DefaultSetSelectionProc = textWid->text.source->SetSelection;
	textWid->text.source->SetSelection = MySetSelectionProc;

	DefaultReplace = textWid->text.source->Replace;
	textWid->text.source->Replace = MyReplaceProc;

	if ( argShowReadProc )
		{
		DefaultRead = textWid->text.source->ReadSource;
		textWid->text.source->ReadSource = MyReadProc;
		}

	DefaultOutputInvalidateProc = textWid->text.output-> Invalidate;
	textWid->text.output->Invalidate =  MyOutputInvalidateProc;

}

void MyInputCreate( Widget w, ArgList al, Cardinal num)
{
	indent++;
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "InputCreate: start\n" );
	DefaultInputCreate ( w,  al, num );
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "InputCreate: stop\n" );
	indent--;
}


void MyRealize( Widget widget, XtValueMask* mask, XSetWindowAttributes* attributes )
{
	indent++;
	PrintDiffs ( (XmTextWidget)widget );
	PrintLead();
	printf ( "Realize: start\n" );
	DefaultRealize ( widget, mask, attributes );
	PrintDiffs ( (XmTextWidget)widget );
	PrintLead();
	printf ( "Realize: stop\n" );
	indent--;
}

void MyResize ( Widget w )
{
	indent++;
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "Resize: start\n" );
	DefaultResize ( w );
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "Resize: stop\n" );
	indent--;
}

void MyExpose ( Widget widget, XEvent* event, Region region )
{
	indent++;
	PrintDiffs ( (XmTextWidget)widget );
	PrintLead();
	printf ( "Expose: start\n" );
	DefaultExpose ( widget, event, region );
	PrintDiffs ( (XmTextWidget)widget );
	PrintLead();
	printf ( "Expose: stop\n" );
	indent--;
}

/*-------------------------------------------------------------
**	AddTextCBs 
**		Create Text.
*/
/* ARGSUSED */
static void  AddTextCBs ( Widget textA )
	{
	XtAddCallback (textA, XmNmodifyVerifyCallback,
				(XtCallbackProc) TextModifyVerifyCB, NULL);

	XtAddCallback (textA, XmNvalueChangedCallback,
				(XtCallbackProc) TextValueChangedCB, NULL);

	XtAddCallback (textA, XmNmotionVerifyCallback,
				(XtCallbackProc) TextMotionCB, NULL);

	XtAddCallback (textA, XmNfocusCallback,
				(XtCallbackProc) TextFocusCB, (XtPointer) True );

	XtAddCallback (textA, XmNlosingFocusCallback,
				(XtCallbackProc) TextFocusCB, (XtPointer) False );

	}

void MyActivateCB (
		Widget but,
		XmTextWidget textw ,
		XtPointer unused1)
{
	XmTextPosition pos = 10; 
	Widget w = (Widget)textw;
	XtUngrabPointer(w, CurrentTime);

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextInsert ( w, %i, \"4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n\" );\n", (int)pos );
	XmTextInsert( w, pos, "4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n24\n225\n226\n227\n228\n29\n210\n211\n212\n13\n" );
	PrintDiffs ( textw );

	printf ( "\n\n*******************************************************************\n" );
	XmTextInsert( w, XmTextGetLastPosition(w), "4444\n5555\n6666\n7777\n8\n9\n10\n11\n12\n13\n24\n225\n226\n227\n228\n29\n210\n211\n212\n13\n" );
	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
}
#if 0
void f(XmTextWidget textw)
	{
	LineNum line;
	XmTextPosition pos = 10; 
	Widget w = (Widget)textw;
	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextInsert ( w, %i, \"14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n\" );\n", (int)pos );
	XmTextInsert( w, pos, "14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n" );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextGetLastPosition( w );\n" );
	pos = XmTextGetLastPosition( w );
	printf ( "result = %i \n\n", (int)pos );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextSetInsertionPosition ( w, 14);\n" );
	XmTextSetInsertionPosition ( w, 14 );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextSetInsertionPosition ( w, 45);\n" );
	XmTextSetInsertionPosition ( w, 45 );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextScroll ( w, -20);\n" );
	XmTextScroll ( w, -20 );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextScroll ( w, 20);\n" );
	XmTextScroll ( w, 20 );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextScroll ( w, -30);\n" );
	XmTextScroll ( w, -20 );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextScroll ( w, 40);\n" );
	XmTextScroll ( w, 20 );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextSetCursorPosition( w, 30 );\n" );
	_XmTextSetCursorPosition( w, 30 ) ;

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextShowPosition ( w, 0);\n" );
	XmTextShowPosition( w, 0 );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextShowPosition ( w, XmTextGetLastPosition( w ));\n" );
	XmTextShowPosition( w, XmTextGetLastPosition( w ) );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextSetTopCharacter ( w, 50 );\n" );
	XmTextSetTopCharacter ( w, 50 );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextSetTopCharacter ( w, 50 );\n" );
	_XmTextSetTopCharacter ( w, 50 );

	{
	XmTextPosition left, right;
	Boolean setSelection, getSelection;

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	__XmTextSetSel2( textw, 40, 70, CurrentTime );\n" );
	setSelection = _XmTextSetSel2( textw, 40, 70, CurrentTime );
	printf ( "result = %i \n\n", (int)setSelection );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	__XmTextGetSel2( textw, &left, &right );\n" );
	getSelection = _XmTextGetSel2( textw, &left, &right );
	printf ( "result  hasSel = %i  left = %i  right = %i \n\n", getSelection, (int)left, (int)right );
	}

	{
	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextMarkRedraw( textw, 10, 30 );\n" );
	_XmTextMarkRedraw( textw, 10, 30 );
	}
	
	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextSetString ( w, \"this is \n a test \n over 3 lines\n\" );\n" );
	XmTextSetString ( w, "this is \n a test \n over 3 lines\n" );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextReplace ( w, 0, 3, \"replaced\" );\n" );
	XmTextReplace ( w, 0, 3, "replaced" );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	XmTextSetSelection ( w, 4, 7, CurrentTime );\n" );
	XmTextSetSelection ( w, 4, 7, CurrentTime );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextPosToLine( w, 10 );\n" );
	line = _XmTextPosToLine( textw, 10 );
	printf ( "result = %i \n\n", line );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextPosToLine( w, 1 );\n" );
	line = _XmTextPosToLine( textw, 1 );
	printf ( "result = %i \n\n", line );

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextSetCursorPosition( w, 30 );\n" );
	_XmTextSetCursorPosition( w, 30 ) ;

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextSetCursorPosition( w, 50 );\n" );
	_XmTextSetCursorPosition( w, 50 ) ;

	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextGetTableIndex( w, 50 );\n" );
	pos = _XmTextGetTableIndex( (XmTextWidget)w, 50 ) ;
	printf ( "result = %i \n\n", (int)pos );

	{
	int numberOfLines ;
	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextGetNumberLines ( w );\n" );
	numberOfLines = _XmTextGetNumberLines( textw ) ;
	printf ( "result = %i \n\n", (int)numberOfLines );
	}

	{
	int totalLines  ;
	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextGetTotalLines ( w );\n" );
	totalLines = _XmTextGetTotalLines( w ) ;
	printf ( "result = %i \n\n", (int)totalLines );
	}

	{
	int totalLines  ;
	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextNumLines ( w );\n" );
	totalLines = _XmTextNumLines( textw  ) ;
	printf ( "result = %i \n\n", (int)totalLines );
	}

	{
	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextMovingCursorPosition( textw, 7 );\n" );
	_XmTextMovingCursorPosition( textw, 7 );
	}

	if(0){
	XmTextBlockRec block;
	block.length = 0;
	PrintDiffs ( textw );
	printf ( "\n\n*******************************************************************\n" );
	printf ( "*******calling 	_XmTextUpdateLineTable( w, 5, 25, NULL, 0 );\n" );
	_XmTextUpdateLineTable( w, 5, 25, &block, 0 ) ;
	}
	
	
	if ( 0 ){
	Widget secondText = XmCreateScrolledText ( XtParent(w), "text2", NULL, 0 );
	XtManageChild ( secondText );
	XtVaSetValues ( secondText,
			XmNsource, GetSrc ( w ),
			NULL );
	}
/* to test :

extern XmTextPosition _XmTextFindScroll( 
                        XmTextWidget widget,
                        XmTextPosition start,
                        int delta) ;

extern XmTextLineTable _XmTextGetLineTable( 
                        Widget widget,
                        int *total_lines) ;

extern void _XmTextRealignLineTable( 
                        XmTextWidget widget,
                        XmTextLineTable *temp_table,
                        int *temp_table_size,
                        register unsigned int cur_index,
                        register XmTextPosition cur_start,
                        register XmTextPosition cur_end) ;

extern void _XmTextLineInfo( 
                        XmTextWidget widget,
                        LineNum line,
                        XmTextPosition *startpos,
                        LineTableExtra *extra) ;

extern int _XmTextCountCharacters( 
                        char *str,
                        int num_count_bytes) ;

--- from Out:

extern void _XmTextFreeContextData( 
                        Widget w,
                        XtPointer clientData,
                        XtPointer callData) ;

extern void _XmTextResetClipOrigin( 
                        XmTextWidget tw,
                        XmTextPosition position,
                        int clip_mask_reset) ;

extern void _XmTextAdjustGC( 
                        XmTextWidget tw) ;

extern Boolean _XmTextShouldWordWrap( 
                        XmTextWidget widget) ;

extern Boolean _XmTextScrollable( 
                        XmTextWidget widget) ;

extern XmTextPosition _XmTextFindLineEnd( 
                        XmTextWidget widget,
                        XmTextPosition position,
                        LineTableExtra *extra) ;

extern void _XmTextOutputGetSecResData( 
                        XmSecondaryResourceData *secResDataRtn) ;

extern void _XmTextDrawDestination( 
                        XmTextWidget widget) ;

extern void _XmTextClearDestination( 
                        XmTextWidget widget,
                        int ignore_sens) ;

extern void _XmTextDestinationVisible( 
                        Widget w,
                        int turn_on) ;

extern void _XmTextChangeBlinkBehavior( 
                        XmTextWidget widget,
                        int newvalue) ;

extern Boolean _XmTextGetBaselines( 
                        Widget widget,
                        Dimension **baselines,
                        int *line_count) ;

extern Boolean _XmTextGetDisplayRect( 
                        Widget w,
                        XRectangle *display_rect) ;

extern void _XmTextMarginsProc( 
                        Widget w,
                        XmBaselineMargins *margins_rec) ;

extern void _XmTextChangeHOffset( 
                        XmTextWidget widget,
                        int length) ;

extern void _XmTextToggleCursorGC( 
                        Widget widget) ;


extern Widget _XmTextGetDropReciever( 
                        Widget w) ;

extern Boolean _XmTextHasDestination( 
                        Widget w) ;

extern Boolean _XmTextSetDestinationSelection( 
                        Widget w,
                        XmTextPosition position,
                        int disown,
                        Time set_time) ;

extern void _XmTextInputGetSecResData( 
                        XmSecondaryResourceData *secResDataRtn) ;

extern XmTextPosition _XmTextGetAnchor( 
                        XmTextWidget tw) ;

-- SOURCE
extern char * _XmStringSourceGetString( 
                        XmTextWidget tw,
                        XmTextPosition from,
                        XmTextPosition to,
                        int want_wchar) ;

extern Boolean _XmTextFindStringBackwards( 
                        Widget w,
                        XmTextPosition start,
                        char *search_string,
                        XmTextPosition *position) ;

extern Boolean _XmTextFindStringForwards( 
                        Widget w,
                        XmTextPosition start,
                        char *search_string,
                        XmTextPosition *position) ;

extern Boolean _XmStringSourceFindString( 
                        Widget w,
                        XmTextPosition start,
                        char *string,
                        XmTextPosition *position) ;

extern void _XmStringSourceSetGappedBuffer( 
                        XmSourceData data,
                        XmTextPosition position) ;

extern Boolean _XmTextModifyVerify(
			XmTextWidget initiator,
        		XEvent *event,
        		XmTextPosition *start,
        		XmTextPosition *end,
        		XmTextPosition *cursorPos,
        		XmTextBlock block,
        		XmTextBlock newblock,
			Boolean *freeBlock) ;

extern XmTextSource _XmStringSourceCreate( 
                        char *value,
                        int is_wchar) ;

extern void _XmStringSourceDestroy( 
                        XmTextSource source) ;

extern char * _XmStringSourceGetValue( 
                        XmTextSource source,
                        int want_wchar) ;

extern void _XmStringSourceSetValue( 
                        XmTextWidget widget,
                        char *value) ;

extern Boolean _XmStringSourceHasSelection( 
                        XmTextSource source) ;

extern Boolean _XmStringSourceGetEditable( 
                        XmTextSource source) ;

extern void _XmStringSourceSetEditable( 
                        XmTextSource source,
                        int editable) ;

extern int _XmStringSourceGetMaxLength( 
                        XmTextSource source) ;

extern void _XmStringSourceSetMaxLength( 
                        XmTextSource source,
                        int max) ;

extern void _XmTextValueChanged(XmTextWidget initiator,
				XEvent *event);
*/
	PrintDiffs ( (XmTextWidget)w );

}
#endif

XmTextPosition MyXYToPos( XmTextWidget w, Position x, Position y)
{
	XmTextPosition result;
	indent++;
	PrintDiffs(w);
	PrintLead();
	printf ( "XYToPos started with args x=%i  y=%i\n", x, y );
	result = DefaultXYToPos ( w, x, y );
	PrintDiffs ( w );
	PrintLead();
	printf ( "XYToPos: stop with result %i\n", (int) result );
	indent--;
	return result;
}

Boolean MyPosToXY( XmTextWidget w, XmTextPosition pos, Position *x, Position *y)
{
	Boolean result;
	indent++;
	PrintDiffs ( w );
	PrintLead();
	printf ( "PosToXY: start with arg pos=%i\n", (int)pos );
#ifdef SHOW_INITIAL_VALUES_FOR_IO_PARAMS
	printf ( "\t\tinitial values for *x=%i  *y=%i\n",  (int)*x, (int)*y );
#endif
	result = DefaultPosToXY ( w, pos, x, y );
	PrintLead();
	if ( result )
		printf ( "PosToXY: stop with results x=%i  y=%i\n", (int)*x, (int)*y );
	else
		printf ( "PosToXY: stop with false\n" );
	indent--;
	return result;
}

void MyGetPreferredSize( Widget w, Dimension *width, Dimension *height)
{
	indent++;
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "OutputGetPreferredSize: start\n");
#ifdef SHOW_INITIAL_VALUES_FOR_IO_PARAMS
	printf ( "\t\tinitial values for width=%i  height=%i\n", *width, *height );
#endif
	DefaultGetPreferredSize ( w, width, height );
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "OutputGetPreferredSize: stop with width=%i  height=%i\n", *width, *height );
	indent--;
}

Boolean MyOutputSetValues( Widget oldw, Widget reqw, Widget new,
			ArgList args, Cardinal *numArgs)
{
	Boolean result;
	indent++;
	PrintDiffs ( (XmTextWidget)new );
	PrintLead();
	printf ( "OutputSetValues: start\n" );
	result = DefaultOutputSetValues ( oldw, reqw, new, args, numArgs );
	PrintDiffs ( (XmTextWidget)new );
	PrintLead();
	printf ( "OutputSetValues: stop with result %s\n", ( result ? "true" : "false" ) );
	indent--;
	return result;
}

void MyOutputGetValues( Widget w, ArgList args, Cardinal numArgs)
{
	indent++;
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "OutputGetValues: start\n" );
	DefaultOutputGetValues ( w, args, numArgs );
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "OutputGetValues: stop\n" );
	indent--;
}

void MyOutputRealize( Widget w, Mask *mask, XSetWindowAttributes *attrs)
{
	indent++;
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "OutputRealize: start\n" );
	DefaultOutputRealize ( w, mask, attrs );
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "OutputRealize: stop\n" );
	indent--;
}

void MyOutputResize( Widget w, Boolean flag)
{
	indent++;
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "OutputResize: start\n" );
	DefaultOutputResize ( w, flag );
	PrintDiffs ( (XmTextWidget)w );
	PrintLead();
	printf ( "OutputResize: stop\n" );
	indent--;
}

void MyOutputExpose( Widget	widget, XEvent*	event, Region	region )
{
	indent++;
	PrintDiffs ( (XmTextWidget)widget );
	PrintLead();
	printf ( "OutputExpose: start\n" );
	DefaultOutputExpose ( widget, event, region );
	PrintDiffs ( (XmTextWidget)widget );
	PrintLead();
	printf ( "OutputExpose: stop\n" );
	indent--;
}

void MyInitialize (    Widget request, Widget new, ArgList args, Cardinal*	num_args )
{
#ifdef VERBOSE_INIT
	XmTextWidget text = (XmTextWidget)request;

	indent++;
/* not necessary since this is the first thing we will do 
	PrintDiffs ( (XmTextWidget)new );
*/	PrintLead();
	printf ( "Initialize: start\n" );
#endif

#ifdef INITIALIAZE_MYPROCS_AT_OUTPUT_CREATION_PROC
#endif

#ifdef VERBOSE_INIT 
	PrintDiffs ( (XmTextWidget)new );
	PrintLead();
	printf ( "Initialize: stop\n" );
	indent--;
#endif

}

/*-------------------------------------------------------------
**	CreateText
**		Create Text.
*/
Widget  CreateText ( Widget  	parent )
	{
	Widget	textA, w,
			form;
	XmTextWidget textWid;
	Arg		al[25];		/*  arg list		*/
	Cardinal 	ac, n;		/*  arg count	*/


	ac = 0;
	XtSetArg (al[ac],	XmNnavigationType,	XmNONE			);ac++;
	form = XmCreateForm (parent, "TextForm", al, ac );
	XtManageChild ( form );

	InitTextInternals();

	{
	XmTextClassRec* textClass = (XmTextClassRec*)xmTextWidgetClass;
	CoreClassPart* coreClassPart = & textClass -> core_class;
/* these 2 do not seem to be very interesting 

	DefaultClassInitialize = coreClassPart -> class_initialize;
	coreClassPart -> class_initialize = MyClassInitialize;

	DefaultClassPartInitialize = coreClassPart -> class_part_initialize;
	coreClassPart -> class_part_initialize = MyClassPartInitialize;

*/
#ifdef INITIALIAZE_MYPROCS_AT_OUTPUT_CREATION_PROC
	DefaultInitialize = coreClassPart -> initialize;
	coreClassPart -> initialize = MyInitialize;
#endif

	DefaultRealize = coreClassPart -> realize;
	coreClassPart -> realize = MyRealize;

	DefaultResize = coreClassPart -> resize;
	coreClassPart -> resize = MyResize;

	DefaultExpose = coreClassPart -> expose;
	coreClassPart -> expose = MyExpose;

	}

	if ( 1 )
		{
		char *str = NULL;

		int i,j;
		if ( argInitialString || argLongString)
			{
			str = XtMalloc( 10000 );
			strcpy ( str, "0\n" );
			for ( i = 1; i < 100; i++ )
				{
				sprintf ( str , "%s\n%i", str, i );
				if ( argLongString )
					{
					for ( j = i; j < i+30; j++ )
						sprintf ( str , "%s %i", str, j );
					}
				}
			}

		ac = 0;
		XtSetArg (al[ac],	XmNtopAttachment,		XmATTACH_FORM		);ac++;
		XtSetArg (al[ac],	XmNtopOffset,			1					);ac++;
		XtSetArg (al[ac],	XmNbottomAttachment,	XmATTACH_FORM		);ac++;
		XtSetArg (al[ac],	XmNrightAttachment,		XmATTACH_FORM		);ac++;
		XtSetArg (al[ac],	XmNleftAttachment,		XmATTACH_FORM		);ac++;
		XtSetArg (al[ac],	XmNhighlightOnEnter,	True				);ac++;
		XtSetArg (al[ac],	XmNhighlightThickness,	1					);ac++;
		XtSetArg (al[ac],	XmNshadowThickness,	1					);ac++;
		XtSetArg (al[ac],	XmNcursorPosition,	4					);ac++;

/*CP: Problem with this and lesstif 
		XtSetArg (al[ac],	XmNhighlightThickness,	3					);ac++;
*/
		if ( str != NULL )
			{
			XtSetArg (al[ac],	XmNvalue,				str					);ac++;
			}

		textA = XmCreateScrolledText (form, "Text",al, ac );
		widgetInitialized = True;

		if ( Text_InputData( textA ) -> new_sel_length != 0 )
			Text_InputData( textA ) -> new_sel_length = 0;
		if (  Text_InputData( textA ) -> stuffpos != 0 )
			Text_InputData( textA ) -> stuffpos = 0;
			/*CP: The following is not used in Motif any more */
		if ( Text_LastTopChar( textA ) != 0 )
			Text_LastTopChar( textA ) = 0;

		printf ("CreateText start : \n" );
		PrintDiffs ( (XmTextWidget)textA );
		ac = 0;
		XtSetArg (al[ac],	XmNrows,				24					);ac++;
		XtSetArg (al[ac],	XmNcolumns,				80					);ac++;
		XtSetArg (al[ac],	XmNautoShowCursorPosition,	False			);ac++;
		XtSetArg (al[ac],	XmNeditMode,			XmMULTI_LINE_EDIT	);ac++;
		XtSetArg (al[ac],	XmNcursorPositionVisible,True				);ac++;
		XtSetValues ( textA, al, ac );
		printf ("CreateText setValues : \n" );
		PrintDiffs ( (XmTextWidget)textA );
		}
	else
		{
		n = 0;
		XtSetArg (al[n],	XmNtopAttachment,		XmATTACH_FORM		);n++;
		XtSetArg (al[n],	XmNbottomAttachment,	XmATTACH_FORM		);n++;
		XtSetArg (al[n],	XmNrightAttachment,		XmATTACH_FORM		);n++;
		XtSetArg (al[n],	XmNleftAttachment,		XmATTACH_FORM		);n++;
		XtSetArg(al[n], XmNwidth,	 400);		   n++;
		XtSetArg(al[n], XmNheight, 300);	   n++;
		XtSetArg(al[n], XmNhighlightThickness, 1);		   n++;
		XtSetArg(al[n], XmNscrollingPolicy, XmAUTOMATIC);	   n++;
		w = XtCreateManagedWidget("noteSW",
					xmScrolledWindowWidgetClass, form,
					al, n);
		XtManageChild ( w );
	
		n = 0;
		XtSetArg(al[n], XmNeditMode,	 XmMULTI_LINE_EDIT);	   n++;
		textA = XtCreateWidget("note",
				xmTextWidgetClass, w, al, n);
		}

	AddTextCBs ( textA );

	textWid = (XmTextWidget)textA;

#ifndef INITIALIAZE_MYPROCS_AT_OUTPUT_CREATION_PROC
	AddMyProcs ( textWid );
#endif

	printf ("CreateText end : " );
	PrintDiffs( (XmTextWidget)textA );
	return ( textA );
	}

char* fallbackResources[7] = {
	"textTest.main.*.XmText.*.Translations : #override \n\
		!<Key>q 	: insert-string(yyyy)\n\
		!<Key>k		: insert-string(xxxx)\n",
	"textTest.main.*.XmText.fontList	: fixed\n",
	"textTest.main.*.XmText.*.fontList	: fixed\n",
	"textTest.main.*.XmText.*.background 	: red\n",
	"textTest.main.*.XmText.*.foreground 	: black\n",
	0 } ;

void Assert(void)
{
	abort ();
}


void	ParseArgs	(
			int	argc,
			String*		argv)
{
	int i;
	Boolean abortAtExit = False;

	for ( i = 1; i < argc; i++ )
		{
		if ( ( strcmp( argv[i], "-abortAtExit" ) == 0 )
		 	|| ( strcmp( argv[i], "-a" ) == 0 ) )
	    	{
	    	abortAtExit = True;
	    	atexit ( Assert );
	    	}
		else if ( ( strcmp( argv[i], "-initialString" ) == 0 )
			||  ( strcmp( argv[i], "-I" ) == 0 ) )
	    	argInitialString = True;
		else if ( ( strcmp( argv[i], "-longString" ) == 0 )
			|| ( strcmp( argv[i], "-l" ) == 0 ) )
	    	argLongString = True;
		else if ( ( strcmp( argv[i], "-showCursor" ) == 0 )
			|| ( strcmp( argv[i], "-c" ) == 0 ) )
	    	argShowDrawCursor = True;
		else if ( ( strcmp( argv[i], "-internalStringValues" ) == 0 )
			|| ( strcmp( argv[i], "-s" ) == 0 ) )
	    	argStringValues = True;
		else if ( ( strcmp( argv[i], "-showReadProc" ) == 0 )
			|| ( strcmp( argv[i], "-r" ) == 0 ) )
	    	argShowReadProc = True;
/*		else if ( strcmp( argv[i], "-secondView" ) == 0 )
	    	argSecondView = True;
*/
		else
			{
			/*CP: unknown args so show help and exit */
			fprintf ( stderr, argv[0] );
			fprintf ( stderr, " : usage : %s [ options ]\n", argv[0] );
			fprintf ( stderr, "\t\twhere options are :\n" );
			fprintf ( stderr, "\t\t -a or -abortAtExit\t: useful if an exit happens when XtMalloc gets a very big arg\n" );
			fprintf ( stderr, "\t\t -I or -initialString\t: set an initial string\n" );
			fprintf ( stderr, "\t\t -l or -longString\t: set a \"wide\" initial string\n" );
			fprintf ( stderr, "\t\t -c or -showCursor\t: show cursor related data\n" );
			fprintf ( stderr, "\t\t -r or -showReadProc\t: show the ReadProc calls and results\n" );
			fprintf ( stderr, "\t\t -s or -internalStringValues\t: show internal values of the TextOut structure\n" );
			fprintf ( stderr, "\t any other option causes this message to be printed\n" );
			exit ( -1 );
			}
		}
	printf ( "Starting with the options:\n" );
	if ( abortAtExit )
		printf ( "\t\taborting when exit is called\n" );
	if ( argInitialString )
		printf ( "\t\tset initial string\n" );
	if ( argLongString )
		printf ( "\t\tset long initial string\n" );
	if ( argShowDrawCursor )
		printf ( "\t\tshowing cursor related data\n" );
	if ( argShowReadProc )
		printf ( "\t\tshow the ReadProc calls and results\n" );
	if ( argStringValues )
		{
		printf ( "\t\tshow internal values of TextOut\n" );
		}
}

int	main	(
			int	argc,
			String*		argv)

	{
	Widget		topLevel,
			mainW, popup, push  ;
	Arg		al [ 5 ];
	int		ac = 0;


	topLevel = XtAppInitialize ( &app_context, "textTest", NULL, 0,
						&argc,  argv, fallbackResources, NULL, 0 );
	if ( argc > 1 )
	    ParseArgs ( argc, argv );

	XtAppSetFallbackResources(app_context, fallbackResources);	
	ac = 0;
/*	XtSetArg (al[ac],	XmNshadowThickness,	0);  ac++;
*/	mainW = XmCreateMainWindow (topLevel, "mainW", al, ac );
	text = (XmTextWidget)CreateText ( mainW );
	XtManageChild ( (Widget)text );

	ac = 0;
	popup = XmCreateMenuBar ( mainW, "menuBar", al, ac );
	XtManageChild ( popup );

	push = XmCreateCascadeButton ( popup, "but1", al, ac );
	XtManageChild ( push );
	XtAddCallback ( push, XmNactivateCallback, 
			(XtCallbackProc)MyActivateCB, (XtPointer)text );

	XtManageChild ( mainW );
	XtRealizeWidget( topLevel );
#ifdef TEST_STANDALONE
	XtAppMainLoop  ( app_context );
#else
	LessTifTestMainLoop(topLevel);
#endif
	return 0;
	}
