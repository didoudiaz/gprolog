; GNU Prolog WIN32 installation script for Inno Setup 4

[Setup]
AppName=GNU Prolog
AppVerName=GNU Prolog version 1.3.2
AppVersion=1.3.2
AppPublisher=Daniel Diaz
AppPublisherURL=http://www.gprolog.org
AppSupportURL=http://www.gprolog.org
AppUpdatesURL=http://www.gprolog.org
DefaultDirName={sd}\GNU-Prolog
DefaultGroupName=GNU Prolog
AllowNoIcons=yes
SourceDir=C:\cygwin\tmp\gprolog_win32
OutputDir=C:\cygwin\tmp
OutputBaseFileName=setup-gprolog-1.3.2-msvc-15.00

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
Source: "doc\html_node\*.*"; DestDir: "{app}\doc\html_node";  Flags: ignoreversion 
Source: "examples\ExamplesPl\*.*"; DestDir: "{app}\examples\ExamplesPl";  Flags: ignoreversion 
Source: "examples\ExamplesFD\*.*"; DestDir: "{app}\examples\ExamplesFD";  Flags: ignoreversion 
Source: "examples\ExamplesC\*.*"; DestDir: "{app}\examples\ExamplesC";  Flags: ignoreversion 

[INI]
Filename: "{app}\gprolog.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://www.gprolog.org"

[Icons]
Name: "{group}\GNU Prolog"; Filename: "{app}\bin\gprolog.exe"
Name: "{group}\Help"; Filename: "{app}\doc\gprolog.chm"
Name: "{group}\Html Manual"; Filename: "{app}\doc\html_node\index.html"
Name: "{group}\The GNU Prolog Web Site"; Filename: "{app}\gprolog.url"
Name: "{group}\Uninstall GNU Prolog"; Filename: "{uninstallexe}"

Name: "{userdesktop}\GNU Prolog"; Filename: "{app}\bin\gprolog.exe"; MinVersion: 4,4; Tasks: desktopicon

[Registry]
Root: HKCU; Subkey: "Software\GnuProlog"; ValueType: string; ValueName: "Version"; ValueData: "1.3.2"
Root: HKCU; Subkey: "Software\GnuProlog"; ValueType: string; ValueName: "RootPath"; ValueData: "{app}"

[Run]
Filename: "{app}\bin\create_bat.exe"; Parameters: """{sd}"" ""{app}"" install"; Description: "Create {sd}\gprologvars.bat"
Filename: "{app}\bin\gprolog.exe"; Description: "Launch GNU Prolog"; Flags: nowait postinstall skipifsilent

[UninstallRun]
Filename: "{app}\bin\create_bat.exe"; Parameters: """{sd}"" ""{app}"" uninstall"

[UninstallDelete]
Type: files; Name: "{app}\gprolog.url"


