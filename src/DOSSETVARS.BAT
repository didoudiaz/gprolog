@echo off
REM Set PATH to ease local development (avoiding make install)
set A=%CD%
SET PATH=%A%\TopComp;%A%\EnginePl;%A%\Pl2Wam;%A%\Wam2Ma;%A%\Ma2Asm;%A%\Fd2C;%A%\DevUtils;%A%\W32GUICons;%PATH%

REM Under Windows tell git to ignore file mode changes (executable attribute)
git config core.filemode false

REM Check that git.exe exists
where git >nul 2>&1
if not errorlevel 1 (
   git config --local core.filemode false 2>nul
)
