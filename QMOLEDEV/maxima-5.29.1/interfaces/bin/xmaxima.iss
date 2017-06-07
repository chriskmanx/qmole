; -*-mode: text; fill-column: 75; tab-width: 8; coding: iso-latin-1-dos -*-
;
; $Id: xmaxima.iss,v 1.3 2002-09-24 02:38:03 mikeclarkson Exp $
;

[Setup]
AppName=Maxima
AppVerName=Maxima 5.9.0
AppPublisher=The Maxima Development Team
AppPublisherURL=http://maxima.sourceforge.net
AppSupportURL=http://maxima.sourceforge.net
AppUpdatesURL=http://maxima.sourceforge.net
DefaultDirName=C:\Programs\maxima-5.9
DisableDirPage=yes
DefaultGroupName=Maxima 5.9
AllowNoIcons=yes
AlwaysCreateUninstallIcon=yes
LicenseFile=C:\Programs\maxima-5.9\COPYING.txt
InfoBeforeFile=C:\Programs\maxima-5.9\COPYING1.txt
Uninstallable=yes
UninstallFilesDir=C:\Programs\maxima-5.9\uninst
; uncomment the following line if you want your installation to run on NT 3.51 too.
; MinVersion=4,3.51

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; MinVersion: 4,4

[Files]
Source: "C:\Programs\maxima-5.9\xmaxima\xmaxima.exe"; DestDir: "{app}\xmaxima"; CopyMode: alwaysoverwrite
Source: "C:\Programs\maxima-5.9\*.*"; DestDir: "{app}\";  Flags: recursesubdirs

[Icons]
Name: "{group}\Maxima"; Filename: "{app}\xmaxima\xmaxima.exe"
Name: "{group}\Maxima"; Filename: "{app}\share\maxima\5.9.0\doc\html\maxima_toc.html"
Name: "{userdesktop}\XMaxima"; Filename: "{app}\xmaxima\xmaxima.exe"; MinVersion: 4,4; Tasks: desktopicon

[Run]
Filename: "{app}\xmaxima\xmaxima.exe"; Description: "Launch Maxima"; Flags: shellexec postinstall skipifsilent

