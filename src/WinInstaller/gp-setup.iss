; GNU Prolog
; Copyright (C) 1999-2026 Daniel Diaz 
;
; WIN32 installation script for Inno Setup

#include "gp-vars-iss.txt"
#define PROLOG_NAME_NO_SPC         StringChange(PROLOG_NAME, " ", "-")

[Setup]
AppId={{4BDA5D76-2819-4523-9C4A-CDA2C5F6B701}}
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
WizardStyle=modern
Compression=lzma2
SolidCompression=yes
AllowNoIcons=yes
SourceDir={#WIN_TMP_DIR}\gprolog_win32
OutputDir={#WIN_TMP_DIR}
OutputBaseFileName=setup-gprolog-{#PROLOG_VERSION}-{#WIN_CC_DESCR}

VersionInfoTextVersion={AppVerName}
VersionInfoCopyright={#PROLOG_COPYRIGHT}

ChangesEnvironment=yes
ChangesAssociations=yes

; install for "single user" (use HKA=HKCU - see win_registry.h)
PrivilegesRequired=lowest 

; The following asks whether to install for the current user or for all users.
; "All users" requires admin privileges and uses HKA = HKLM (instead of HKCU).
; This would require non-trivial adaptations in win_registry.h to detect at runtime
; whether HKCU or HKLM is being used.
; Additionally, in 32-bit mode on 64-bit Windows, HKLM keys are redirected to
; HKLM\Software\WOW6432Node\..., which further complicates win_registry.h handling.
;
; PrivilegesRequired=lowest
; PrivilegesRequiredOverridesAllowed=dialog

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
Root: HKA; Subkey: "Software\{#PROLOG_NAME}"; ValueType: string; ValueName: "version"; ValueData: "{#PROLOG_VERSION}"; Flags: uninsdeletekey

; ---------- create a ProgId "PrologFile" (common for .pl .pro .prolog) 

Root: HKA; Subkey: "Software\Classes\PrologFile"; ValueType: string; ValueName: ""; ValueData: "Prolog source file"; Flags: uninsdeletekey; Tasks: assocPl assocPro assocProlog

Root: HKA; Subkey: "Software\Classes\PrologFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData:  "{app}\bin\gprolog.exe,0"; Flags: uninsdeletekey; Tasks: assocPl assocPro assocProlog

Root: HKA; Subkey: "Software\Classes\PrologFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\gprolog.exe"" --entry-goal ""consult(`%1`)"""; Flags: uninsdeletevalue; Tasks: assocPl assocPro assocProlog

; ---------- create associations .pl .pro .prolog -> PrologFile

Root: HKA; Subkey: "Software\Classes\.pl\OpenWithProgids"; ValueType: string; ValueName: "PrologFile"; ValueData: ""; Flags: uninsdeletevalue; Tasks: assocPl
Root: HKA; Subkey: "Software\Classes\.pro\OpenWithProgids"; ValueType: string; ValueName: "PrologFile"; ValueData: ""; Flags: uninsdeletevalue; Tasks: assocPro
Root: HKA; Subkey: "Software\Classes\.prolog\OpenWithProgids"; ValueType: string; ValueName: "PrologFile"; ValueData: ""; Flags: uninsdeletevalue; Tasks: assocProlog

; ---------- add gprolog.exe SupportedTypes for .pl .pro. .prolog

Root: HKA; Subkey: "Software\Classes\Applications\gprolog.exe\SupportedTypes"; ValueType: string; ValueName: ".pl"; ValueData: ""; Flags: uninsdeletevalue; Tasks: assocPl
Root: HKA; Subkey: "Software\Classes\Applications\gprolog.exe\SupportedTypes"; ValueType: string; ValueName: ".pro"; ValueData: ""; Flags: uninsdeletevalue; Tasks: assocPro
Root: HKA; Subkey: "Software\Classes\Applications\gprolog.exe\SupportedTypes"; ValueType: string; ValueName: ".prolog"; ValueData: ""; Flags: uninsdeletevalue; Tasks: assocProlog


[Run]
Filename: "{app}\bin\create_bat.exe"; Parameters: """{app}"""; Description: "Create {app}\gprologvars.bat"
Filename: "{app}\bin\gprolog.exe"; Description: "Launch {#PROLOG_NAME}"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: files; Name: "{app}\gprologvars.bat"
Type: files; Name: "{app}\gprolog.url"


