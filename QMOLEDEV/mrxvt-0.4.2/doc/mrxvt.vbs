'
' All portions of code are copyright by their respective author/s.
' Copyright (c) 2004     Matthew Melendy <melendy@lucent.com>
'
' This program is free software; you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation; either version 2 of the License, or
' (at your option) any later version.
'
' This program is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.
'
' You should have received a copy of the GNU General Public License
' along with this program; if not, write to the Free Software
' Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
'
'
' $Id: mrxvt.vbs,v 1.1 2004/11/10 22:24:01 cvs Exp $
'
'
' This program is used to launch mrxvt directly from a Windows
' shortcut for Cygwin users.
'
' You should change the CYGWIN_ROOT and PATH variables according to
' your installation path of cygwin and mrxvt.exe.
'
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Setup variables
'
option explicit
dim program, CYGWIN_ROOT, CurrentPath, WshProcessEnv, mrxvtCommandLine
CYGWIN_ROOT = "C:\Cygwin"
mrxvtCommandLine = "mrxvt.exe -e /bin/bash --login -i"
'
' Create WSH scripting object and obtain current PATH value
'
set program = CreateObject("WScript.Shell")
set WshProcessEnv = program.Environment("PROCESS")
CurrentPath = WshProcessEnv("PATH")
'
' Set environment variables required by Xwin
'
WshProcessEnv("DISPLAY") = "127.0.0.1:0.0"
WshProcessEnv("CYGWIN_ROOT")=CYGWIN_ROOT
WshProcessEnv("PATH")=CYGWIN_ROOT & "\bin;" & CYGWIN_ROOT & "\usr\X11R6\bin;" & CYGWIN_ROOT & "\usr\local\bin;" & CurrentPath
'
' Launch the XWin server with the console window hidden and continue
'
program.run mrxvtCommandLine,0,0
'
' Close WSH script object and exit
'
set program=nothing
