#ifndef XFILEVIEW_H
#define XFILEVIEW_H



class ViewWindow;


// Main Application class
class XFileView : public FXApp
{
    FXDECLARE(XFileView)
public:
    ViewWindowList windowlist; // Window list
private:
    XFileView()
    {}
    XFileView(const XFileView&);
    XFileView& operator=(const XFileView&);
public:
    enum{
        ID_CLOSEALL=FXApp::ID_LAST,
        ID_LAST
    };
public:
    long onCmdCloseAll(FXObject*,FXSelector,void*);
public:

    // Construct application object
    XFileView(const FXString&, const FXString&);

    // Initialize application
    virtual void init(int& argc,char** argv,bool connect=TRUE);

    // Exit application
    virtual void exit(int code=0);

    // Delete application object
    virtual ~XFileView();
};

#endif

