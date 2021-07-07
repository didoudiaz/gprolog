; GNU Prolog
; Copyright (C) 1999-2021 Daniel Diaz 
;
; WIN32 installation script for Inno Setup

#include "gp-vars-iss.txt"
#define PROLOG_NAME_NO_SPC         StringChange(PROLOG_NAME, " ", "-")

[Setup]
AppName={#PROLOG_NAME}
AppVerName={#PROLOG_NAME} version {#PROLOG_VERSION}
AppVersion={#PROLOG_VERSION}
AppPublisher=Daniel Diaz
AppPublisherURL=http://www.gprolog.org
AppSupportURL=http://www.gprolog.org 
AppUpdatesURL=http://www.gprolog.org
SetupIconFile=gprolog.ico
DefaultDirName={sd}\{#PROLOG_NAME_NO_SPC}
DefaultGroupName={#PROLOG_NAME}
AllowNoIcons=yes
SourceDir={#WIN_TMP_DIR}\gprolog_win32
OutputDir={#WIN_TMP_DIR}
OutputBaseFileName=setup-gprolog-{#PROLOG_VERSION}-{#WIN_CC_VER}

VersionInfoTextVersion={AppVerName}
VersionInfoCopyright={#PROLOG_COPYRIGHT}

ChangesEnvironment=yes
ChangesAssociations=yes

;Compression=lzma
;SolidCompression=yes
PrivilegesRequired=none

[Tasks]
Name: desktopicon; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; 

Name: assocPl; Description: "&Associate {#PROLOG_NAME} with .pl files"; GroupDescription: "File associations:";
Name: assocPro; Description: "A&ssociate {#PROLOG_NAME} with .pro files"; GroupDescription: "File associations:";
Name: assocProlog; Description: "A&ssociate {#PROLOG_NAME} with .prolog files"; GroupDescription: "File associations:";

[Files]
Source: "*.*"; DestDir: "{app}";  Flags: ignoreversion
Source: "bin\*.*"; DestDir: "{app}\bin";  Flags: ignoreversion
Source: "lib\*.*"; DestDir: "{app}\lib";  Flags: ignoreversion
Source: "lib\*.dll"; DestDir: "{app}\bin";  Flags: ignoreversion skipifsourcedoesntexist
Source: "include\*.*"; DestDir: "{app}\include";  Flags: ignoreversion
Source: "doc\*.*"; DestDir: "{app}\doc";  Flags: ignoreversion
Source: "doc\html_node\*.*"; DestDir: "{app}\doc\html_node";  Flags: ignoreversion
Source: "examples\ExamplesPl\*.*"; DestDir: "{app}\examples\ExamplesPl";  Flags: ignoreversion
Source: "examples\ExamplesFD\*.*"; DestDir: "{app}\examples\ExamplesFD";  Flags: ignoreversion
Source: "examples\ExamplesC\*.*"; DestDir: "{app}\examples\ExamplesC";  Flags: ignoreversion

[INI]
Filename: "{app}\gprolog.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://www.gprolog.org"

[Icons]
Name: "{group}\{#PROLOG_NAME}"; Filename: "{app}\bin\gprolog.exe"
Name: "{group}\Help"; Filename: "{app}\doc\gprolog.chm"
Name: "{group}\Html Manual"; Filename: "{app}\doc\html_node\index.html"
Name: "{group}\The {#PROLOG_NAME} Web Site"; Filename: "{app}\gprolog.url"
Name: "{group}\Uninstall {#PROLOG_NAME}"; Filename: "{uninstallexe}"

Name: "{userdesktop}\{#PROLOG_NAME}"; Filename: "{app}\bin\gprolog.exe"; MinVersion: 4,4; Tasks: desktopicon


[Registry]
; admin user
Root: HKLM; Subkey: "SOFTWARE\{#PROLOG_NAME}"; ValueType: string; ValueName: "Version"; ValueData: "{#PROLOG_VERSION}"; Flags: uninsdeletekey noerror
Root: HKLM; Subkey: "SOFTWARE\{#PROLOG_NAME}"; ValueType: string; ValueName: "RootPath"; ValueData: "{app}"; Flags: uninsdeletekey noerror
; non-admin user
Root: HKCU; Subkey: "Software\{#PROLOG_NAME}"; ValueType: string; ValueName: "RootPath"; ValueData: "{app}"; Flags: uninsdeletekey
Root: HKCU; Subkey: "Software\{#PROLOG_NAME}"; ValueType: string; ValueName: "Version"; ValueData: "{#PROLOG_VERSION}"; Flags: uninsdeletekey

; create an association for .pl, .pro and .prolog files

; admin user
Root: HKLM; Subkey: "SOFTWARE\Classes\.pl"; ValueType: string; ValueName: ""; ValueData: "PrologFile"; Flags: uninsdeletekey noerror; Tasks: assocPl;
Root: HKLM; Subkey: "SOFTWARE\Classes\.pro"; ValueType: string; ValueName: ""; ValueData: "PrologFile"; Flags: uninsdeletekey noerror; Tasks: assocPro;
Root: HKLM; Subkey: "SOFTWARE\Classes\.prolog"; ValueType: string; ValueName: ""; ValueData: "PrologFile"; Flags: uninsdeletekey noerror; Tasks: assocProlog;
; non-admin user
Root: HKCU; Subkey: "Software\Classes\.pl"; ValueType: string; ValueName: ""; ValueData: "PrologFile"; Flags: uninsdeletekey; Tasks: assocPl;
Root: HKCU; Subkey: "Software\Classes\.pro"; ValueType: string; ValueName: ""; ValueData: "PrologFile"; Flags: uninsdeletekey; Tasks: assocPro;
Root: HKCU; Subkey: "Software\Classes\.prolog"; ValueType: string; ValueName: ""; ValueData: "PrologFile"; Flags: uninsdeletekey; Tasks: assocProlog;

; could also use ;Check: not IsAdminLoggedOn

; admin user
Root: HKLM; Subkey: "SOFTWARE\Classes\PrologFile"; ValueType: string; ValueName: ""; ValueData: "Prolog File"; Flags: uninsdeletekey noerror; Tasks: assocPl assocPro assocProlog
Root: HKLM; Subkey: "SOFTWARE\Classes\PrologFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\bin\gprolog.exe,0"; Flags: uninsdeletekey noerror; Tasks: assocPl assocPro assocProlog
Root: HKLM; Subkey: "SOFTWARE\Classes\PrologFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\gprolog.exe"" --entry-goal ""consult(`%1`)"""; Flags: uninsdeletekey noerror; Tasks: assocPl assocPro assocProlog
; non-admin user
Root: HKCU; Subkey: "Software\Classes\PrologFile"; ValueType: string; ValueName: ""; ValueData: "Prolog File"; Flags: uninsdeletekey; Tasks: assocPl assocPro assocProlog
Root: HKCU; Subkey: "Software\Classes\PrologFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\bin\gprolog.exe,0"; Flags: uninsdeletekey; Tasks: assocPl assocPro assocProlog
Root: HKCU; Subkey: "Software\Classes\PrologFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\gprolog.exe"" --entry-goal ""consult(`%1`)"""; Flags: uninsdeletekey; Tasks: assocPl assocPro assocProlog




[Run]
Filename: "{app}\bin\create_bat.exe"; Parameters: """{app}"""; Description: "Create {app}\gprologvars.bat"
Filename: "{app}\bin\gprolog.exe"; Description: "Launch {#PROLOG_NAME}"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: files; Name: "{app}\gprologvars.bat"
Type: files; Name: "{app}\gprolog.url"


