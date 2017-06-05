#include <stdio.h>

#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>

void entry_cb(
  Widget	w,
  XtPointer	client_data,
  XtPointer	call_data )
{
  int*		val = (int*) client_data ;
  
  XmRowColumnCallbackStruct * cbs =
    (XmRowColumnCallbackStruct *) call_data ;
        
  printf("%d: %s\n", (*val), XtName(w) );
  
  if ( cbs->reason != XmCR_ACTIVATE )
  {
    printf( "Unexpected callback reason %d\n",
      cbs->reason );
    return ;
  }
  
  printf( "  Selected : %s\n", XtName( cbs->widget ) );
}

int main ( int argc, char *argv[] )
{
  XtAppContext	app_con ;

  Widget	toplevel ;
  Widget	menu_widget ;
  Widget	opt_widget ;
  Widget	button ;
  
  Arg		arg[5] ;
  Cardinal	arg_num ;
  int 		i ;
  int		val[] = { 0, 1, 2, 3, 4, 5 } ;
  int		j = 0 ;
  char 		buffer[10] ;
    
  toplevel = XtVaAppInitialize( &app_con,
    NULL,
    NULL, 0,
    &argc, argv,
    NULL,
    NULL );
    
  menu_widget = XmCreatePulldownMenu(
    toplevel, "pulldown", NULL, 0 );

#if 0
  /* 
   * if this callback is added, all subsequent callbacks get called too 
   */
  XtAddCallback( menu_widget, XmNentryCallback, entry_cb, &val[j++] );  
#endif

  arg_num = 0 ;
  XtSetArg( arg[arg_num], XmNsubMenuId, menu_widget );	arg_num++ ;
  opt_widget = XmCreateOptionMenu(
    toplevel, "options", arg, arg_num );

#if 0
  /* 
   * if this callback is added, all subsequent callbacks get called too 
   */
  XtAddCallback( menu_widget, XmNentryCallback, entry_cb, &val[j++] );  
#endif

  for ( i = 0 ; i < 5 ; i++ )
  {
    sprintf( buffer, "item%d", i );
    button = XmCreatePushButtonGadget( 
      menu_widget, buffer, NULL, 0 );
    XtManageChild( button );
  }

  XtAddCallback( menu_widget, XmNentryCallback, entry_cb, &val[j++] );  

#if 1
  /* 
   * if this button is created, lesstif doesn't call the entry callback
   * for item0 .. item4, but calls it for item5
   */
    sprintf( buffer, "item%d", i );
    button = XmCreatePushButtonGadget( 
      menu_widget, buffer, NULL, 0 );
    XtManageChild( button );
#endif

  XtManageChild( opt_widget );

  XtAddCallback( menu_widget, XmNentryCallback, entry_cb, &val[j++] );  

  XtRealizeWidget( toplevel );
  
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	80,	35,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	4,	29,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	10,	3,	67,	29,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
  XtAppMainLoop( app_con );
  */
  
  return 0;
}
