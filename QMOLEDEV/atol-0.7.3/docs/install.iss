;
; Atol installation script file (Inno Setup)
;
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

[Setup]
AppName=Atol
AppVerName=Atol v0.7.0
AppCopyright=Copyright © 2003-2006 Atol development team
DefaultDirName={pf}\Atol
DefaultGroupName=Atol
UninstallDisplayIcon={app}\atol.exe
;it is possible to skip creating program group
AllowNoIcons=yes
; display this file before selection the destination
InfoBeforeFile=readme.txt

[Tasks]
Name: desktopicon; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:";
Name: quicklaunchicon; Description: "Create a &Quick Launch icon"; GroupDescription: "Additional icons:"; Flags: unchecked
Name: startmenuicon; Description: "Create a &start menu icon"; GroupDescription: "Additional icons:"; Flags: unchecked
Name: gtkinstall; Description: "Install GTK";

[Files]
Source: "atol.exe"; DestDir: "{app}"; Flags: promptifolder
Source: "iconv.dll"; DestDir: "{app}"; Flags: promptifolder
Source: "readme.txt"; DestDir: "{app}"; Flags: isreadme
Source: "atol.url"; DestDir: "{app}"
Source: "msvcrt.dll"; DestDir: "{sys}"; Flags: onlyifdoesntexist  uninsneveruninstall
Source: "unrar.dll"; DestDir: "{sys}"; Flags: onlyifdoesntexist  uninsneveruninstall
Source: "gtk-2.6.10-win32-1.exe"; Tasks: gtkinstall; DestDir: "{tmp}"; Flags: deleteafterinstall;
Source: "plugins\ArjLib.atp"; DestDir: "{app}\plugins"
Source: "plugins\Bz2Lib.atp"; DestDir: "{app}\plugins"
Source: "plugins\GzLib.atp"; DestDir: "{app}\plugins"
Source: "plugins\LstLib.atp"; DestDir: "{app}\plugins"
Source: "plugins\RarLib.atp"; DestDir: "{app}\plugins"
Source: "plugins\TarLib.atp"; DestDir: "{app}\plugins"
Source: "plugins\ZipLib.atp"; DestDir: "{app}\plugins"
Source: "plugins\ZLib.atp"; DestDir: "{app}\plugins"
Source: "locale\hr\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\hr\LC_MESSAGES"
Source: "locale\zh_CN\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\zh_CN\LC_MESSAGES"
Source: "locale\pl\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\pl\LC_MESSAGES"
Source: "locale\sv\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\sv\LC_MESSAGES"
Source: "locale\es\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\es\LC_MESSAGES"
Source: "locale\nl\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\nl\LC_MESSAGES"
Source: "locale\de\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\de\LC_MESSAGES"
Source: "locale\da\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\da\LC_MESSAGES"
Source: "locale\fr\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\fr\LC_MESSAGES"
Source: "locale\fr_CA\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\fr_CA\LC_MESSAGES"
Source: "locale\pt_BR\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\pt_BR\LC_MESSAGES"
Source: "locale\ru\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\ru\LC_MESSAGES"
Source: "locale\id\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\id\LC_MESSAGES"
Source: "locale\no\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\no\LC_MESSAGES"
Source: "locale\tr\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\tr\LC_MESSAGES"
Source: "locale\pt\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\pt\LC_MESSAGES"
Source: "locale\ta\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\ta\LC_MESSAGES"
Source: "locale\et\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\et\LC_MESSAGES"
Source: "locale\cs\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\cs\LC_MESSAGES"
Source: "locale\it\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\it\LC_MESSAGES"
Source: "locale\sl\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\sl\LC_MESSAGES"
Source: "locale\sk\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\sk\LC_MESSAGES"
Source: "locale\hu\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\hu\LC_MESSAGES"
Source: "locale\ja\LC_MESSAGES\atol.mo"; DestDir: "{app}\locale\ja\LC_MESSAGES"
Source: "msvcrt.dll"; DestDir: "{sys}"; Flags: onlyifdoesntexist uninsneveruninstall

[Run]
; execute gtk installer if selected
Filename: "{tmp}\gtk-2.6.10-win32-1.exe";  Tasks: gtkinstall;  Flags: hidewizard skipifdoesntexist; AfterInstall: UpdateAppPath;

[Icons]
Name: "{group}\Atol"; Filename: "{app}\atol.exe"; WorkingDir: "{app}"
Name: "{group}\Visit the Web Site"; Filename: "{app}\atol.url"
Name: "{group}\Read me"; Filename: "{app}\readme.txt"
Name: "{group}\Uninstall Atol"; Filename: "{uninstallexe}"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\Atol"; Filename: "{app}\atol.exe"; Tasks: quicklaunchicon;  WorkingDir: "{app}"
Name: "{userdesktop}\Atol"; Filename: "{app}\atol.exe"; Tasks: desktopicon; WorkingDir: "{app}"
Name: "{userstartmenu}\Atol"; Filename: "{app}\atol.exe"; Tasks: startmenuicon; WorkingDir: "{app}"

[Registry]
; This adds the GTK+ libraries to atol.exe's path
Root: HKLM; Subkey: "Software\Microsoft\Windows\CurrentVersion\App Paths\atol.exe"; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: "Software\Microsoft\Windows\CurrentVersion\App Paths\atol.exe"; ValueType: string; ValueData: "{app}\atol.exe"; Flags: uninsdeletevalue
; this one is a placeholder, its value is filled in UpdateAppPath();
Root: HKLM; Subkey: "Software\Microsoft\Windows\CurrentVersion\App Paths\atol.exe"; ValueType: string; ValueName: "Path"; ValueData: ""; Flags: uninsdeletevalue

[Code]

var
  Exists: Boolean;
  GtkPath: String;

function GetGtkInstalled(): Boolean;
begin
  Exists := RegQueryStringValue (HKLM, 'Software\GTK\2.0', 'Path', GtkPath);
  if not Exists then begin
    Exists := RegQueryStringValue (HKCU, 'Software\GTK\2.0', 'Path', GtkPath);
  end;
   Result := Exists
end;

procedure UpdateAppPath();
var
 AppPath: String;
 RegValue: String;
begin
    AppPath := ExpandConstant('{app}');

    GetGtkInstalled (); //first read Gtk Path

    //create and write registry key
    RegValue := AppPath;
    RegValue := RegValue + ';';
    RegValue := RegValue + GtkPath;
    RegValue := RegValue + '\bin';

    //MsgBox(RegValue, mbInformation, MB_OK);
    RegWriteStringValue (HKLM, 'Software\Microsoft\Windows\CurrentVersion\App Paths\atol.exe', 'Path', RegValue);
end;
