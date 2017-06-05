// main.cc for fbdesk
// Copyright (c) 2002 - 2006 Henrik Kinnunen (fluxgen at users.sourceforge.net)
// 
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

#include "App.hh"
#include "FbDesk.hh"
#include "version.h"

#include <iostream>
#include <stdexcept>
#include <signal.h>
#include <sys/wait.h>

using namespace std;

///  handles system signals
void signalhandler(int sig) {
    if (sig == SIGCHLD) { // we don't want the child process to kill us
        int status;
        waitpid(-1, &status, WNOHANG | WUNTRACED);
    } else {
        FbTk::App::instance()->end();
    }
}

/// handles X error events
int handleXError(Display *disp, XErrorEvent *event) {
    /*
    char errtxt[128];
	
    XGetErrorText(disp, event->error_code, errtxt, 128);
    cerr<<"FbDesk: X error: "<<errtxt<<"("<<event->error_code<<") opcodes "<<
        event->request_code<<"/"<<event->minor_code<<endl<<"resource 0x"<<
        hex<<event->resourceid<<dec<<endl;
    */
    return False;
}

void showUsage(const char *name) {
    cout<<"Usage: "<<name<<" [option]"<<endl;
    cout<<"Options:"<<endl;
    cout<<"-display <display name>   Display connection"<<endl<<
        "-rc <resource file>         Resource file (Default: ~/.fluxbox/fbdesk)"<<endl<<
        "-v                          Show version"<<endl<<
        "-na                         Disable antialias"<<endl<<endl;
}

int main(int argc, char **argv) try {
    string displaystr, resource_filename("~/.fluxbox/fbdesk");
    bool antialias = true;
    // determine arguments
    for (int i=1; i<argc; ++i) {
        if (strcmp("-display", argv[i]) == 0 && // display connection
            i + 1 < argc) {
            displaystr = argv[++i];
        } else if (strcmp("-rc", argv[i]) == 0 && // resource file
                   i + 1 < argc) {
            resource_filename = argv[++i];
        } if (strcmp("-v", argv[i]) == 0) {
            cout<<"FbDesk "<<FBDESK_VERSION<<" Copyright (c) 2003-2006 Henrik Kinnunen"<<endl;
            exit(0);
        } else {
            cout<<"FbDesk "<<FBDESK_VERSION<<" Copyright (c) 2003-2006 Henrik Kinnunen"<<endl;
            showUsage(argv[0]);
            exit(0);
        }
    }


		
    FbTk::App app(displaystr.c_str());
    // setup signals
    signal(SIGSEGV, signalhandler);
    signal(SIGFPE, signalhandler);
    signal(SIGTERM, signalhandler);
    signal(SIGINT, signalhandler);
    signal(SIGUSR1, signalhandler);
    signal(SIGUSR2, signalhandler);
    signal(SIGHUP, signalhandler);
    signal(SIGCHLD, signalhandler);
    // catch x errors
    XSetErrorHandler(handleXError);
    // create fbdesk
    FbDesk::FbDesk desk(resource_filename.c_str());

    // finaly enter main event loop
    app.eventLoop();

    return 0;

 } catch (std::string error_str) {
    cerr<<"FbDesk > Error: "<<error_str<<endl;
    return -1;
 } catch (out_of_range &oor) {
    cerr<<"Out of range: "<<oor.what()<<endl;
    return -1;
 } catch (exception &ee) {
    cerr<<"Exception thrown: "<<ee.what()<<endl;
    return -1;
 }

