#include"fx.h"
#include "FXExpression.h"


/* Evaluate expression with arguments */
int main(int argc,char **argv){
  FXdouble parameter[10];
  FXExpressionError err;
  FXExpression expr;
  FXdouble result;
  FXint i;
#ifndef NDEBUG
  fxTraceLevel=101;
#endif
  if(1<argc){
    fxmessage("evaluate(%s) ",argv[1]);
    err=expr.parse(argv[1],(2<argc)?argv[2]:NULL);
    if(err!=EXPRERR_OK){
      fxwarning("Error: %s\n",FXExpression::getError(err));
      exit(1);
      }
    for(i=3; i<argc; i++){
      parameter[i-3]=strtod(argv[i],NULL);
      }
    result=expr.evaluate(parameter);
    fxmessage("evaluate(%s) ",argv[1]);
    if(2<argc){
      fxmessage("{ ");
      for(i=0; i<argc-3; i++){ fxmessage("%.10g ",parameter[i]); }
      fxmessage("}");
      }
    fxmessage(" = %.10g\n",result);
    }
  return 0;
  }


