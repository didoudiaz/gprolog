@echo off
# GNU Prolog
# Set PATH to ease local development (avoiding make install)
# execute this file under cmd.exe:

set a=%cd%
set PATH=%a%\TopComp;%a%\EnginePl;%a%\Pl2Wam;%a%\Wam2Ma;%a%\Ma2Asm;%a%\Fd2C;%a%\DevUtils;%a%\W32GUICons;%PATH%

REM Under Windows tell git to ignore file mode changes (executable attribute)
REM Check that git.exe exists
where git >nul 2>&1
if not errorlevel 1 (
   git config --local core.filemode false 2>nul
)
