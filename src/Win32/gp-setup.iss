; GNU Prolog WIN32 installation script for Inno Setup 4

[Setup]
AppName=GNU Prolog
AppVerName=GNU Prolog version 1.2.19
AppVersion=1.2.19
AppPublisher=Daniel Diaz
AppPublisherURL=http://gprolog.inria.fr
AppSupportURL=http://gprolog.inria.fr
AppUpdatesURL=http://gprolog.inria.fr
DefaultDirName={sd}\GNU-Prolog
DefaultGroupName=GNU Prolog
AllowNoIcons=yes
SourceDir=\gprolog_win32
OutputDir=
OutputBaseFileName=setup-gprolog-1.2.19

; TO DO: create an association for .pl and .pro files
; ChangesAssociations=yes

; uncomment the following line if you want your installation to run on NT 3.51 too.
; MinVersion=4,3.51

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; MinVersion: 4,4

[Files]
Source: "*.*"; DestDir: "{app}";  Flags: ignoreversion 
Source: "bin\*.*"; DestDir: "{app}\bin";  Flags: ignoreversion 
Source: "lib\*.*"; DestDir: "{app}\lib";  Flags: ignoreversion 
Source: "lib\*.dll"; DestDir: "{app}\bin";  Flags: ignoreversion skipifsourcedoesntexist
Source: "include\*.*"; DestDir: "{app}\include";  Flags: ignoreversion 
Source: "doc\*.*"; DestDir: "{app}\doc";  Flags: ignoreversion 
Source: "doc\Html\*.*"; DestDir: "{app}\doc\Html";  Flags: ignoreversion 
Source: "ExamplesPl\*.*"; DestDir: "{app}\ExamplesPl";  Flags: ignoreversion 
Source: "ExamplesFD\*.*"; DestDir: "{app}\ExamplesFD";  Flags: ignoreversion 
Source: "ExamplesC\*.*"; DestDir: "{app}\ExamplesC";  Flags: ignoreversion 

[INI]
Filename: "{app}\gprolog.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://gprolog.inria.fr"

[Icons]
Name: "{group}\GNU Prolog"; Filename: "{app}\bin\gprolog.exe"
Name: "{group}\Help"; Filename: "{app}\doc\Html\manual.chm"
Name: "{group}\Html Manual"; Filename: "{app}\doc\Html\index.html"
Name: "{group}\The GNU Prolog Web Site"; Filename: "{app}\gprolog.url"
Name: "{group}\Uninstall GNU Prolog"; Filename: "{uninstallexe}"

Name: "{userdesktop}\GNU Prolog"; Filename: "{app}\bin\gprolog.exe"; MinVersion: 4,4; Tasks: desktopicon

[Registry]
Root: HKCU; Subkey: "Software\GnuProlog"; ValueType: string; ValueName: "Version"; ValueData: "1.2.19"
Root: HKCU; Subkey: "Software\GnuProlog"; ValueType: string; ValueName: "RootPath"; ValueData: "{app}"

[Run]
Filename: "{app}\bin\create_bat.exe"; Parameters: """{sd}"" ""{app}"" install"; Description: "Create {sd}\gprologvars.bat and update autoexec.bat"
Filename: "{app}\bin\gprolog.exe"; Description: "Launch GNU Prolog"; Flags: nowait postinstall skipifsilent

[UninstallRun]
Filename: "{app}\bin\create_bat.exe"; Parameters: """{sd}"" ""{app}"" uninstall"

[UninstallDelete]
Type: files; Name: "{app}\gprolog.url"


