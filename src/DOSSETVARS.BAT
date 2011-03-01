@REM SET A=C:\cygwin\home\diaz\GP\src\src
CD >winpwd
SET /P A= <winpwd
DEL winpwd
SET PATH=%A%\TopComp;%A%\EnginePl;%A%\Pl2Wam;%A%\Wam2Ma;%A%\Ma2Asm;%A%\Fd2C;%A%\DevUtils;%A%\W32GUICons;%PATH%
