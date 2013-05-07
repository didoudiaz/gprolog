/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine + Compiler                                        *
 * File  : machine1.c                                                      *
 * Descr.: machine dependent features                                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2013 Daniel Diaz                                     *
 *                                                                         *
 * This file is part of GNU Prolog                                         *
 *                                                                         *
 * GNU Prolog is free software: you can redistribute it and/or             *
 * modify it under the terms of either:                                    *
 *                                                                         *
 *   - the GNU Lesser General Public License as published by the Free      *
 *     Software Foundation; either version 3 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or                                                                      *
 *                                                                         *
 *   - the GNU General Public License as published by the Free             *
 *     Software Foundation; either version 2 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or both in parallel, as here.                                           *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful,           *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received copies of the GNU General Public License and   *
 * the GNU Lesser General Public License along with this program.  If      *
 * not, see http://www.gnu.org/licenses/.                                  *
 *-------------------------------------------------------------------------*/


#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>

#include "gp_config.h"
#include "bool.h"

#if 0

#define USE_ALONE
#define DEBUG
#if 0
#define USE_W32_GUI_CONSOLE
#endif

#endif



#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <sys/param.h>
#include <time.h>
#else
#include <windows.h>
#include <winnt.h>
#include <process.h>
#include <io.h>
#include <fcntl.h>
#endif


#ifdef __CYGWIN__
#include <process.h>
#endif


#define MACHINE1_FILE
#include "machine1.h"

#ifdef USE_W32_GUI_CONSOLE
#include "../Linedit/linedit.h"
#define printf LE_Printf
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#if defined(_WIN32) && !defined(CYGWIN)
static Bool Get_Windows_OS_Name(char *buff);
#endif




/*-------------------------------------------------------------------------*
 * PL_INIT_MACHINE1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Init_Machine1(void)
{
#if defined(__unix__) || defined(__CYGWIN__)

  struct utsname uname_info;

  pl_m_os_type = M_OS_UNIX;

  if (uname(&uname_info) < 0)
    {
      strcpy(pl_m_architecture, "unknown architecture");
      strcpy(pl_m_os_version, "unknown OS version");
      return;
    }

  strcpy(pl_m_architecture, uname_info.machine);

  sprintf(pl_m_os_version, "%s %s", uname_info.sysname, uname_info.release);

#else

  SYSTEM_INFO si;

  GetSystemInfo(&si);
  if (si.wProcessorLevel >= 3 && si.wProcessorLevel < 10)
    sprintf(pl_m_architecture, "i%c86", si.wProcessorLevel + '0');
  else
    sprintf(pl_m_architecture, "i%ld", si.dwProcessorType);

  pl_m_os_type = M_OS_WINDOWS;
  if (!Get_Windows_OS_Name(pl_m_os_version))
    strcpy(pl_m_os_version, "unknown OS version");

#endif
}




#if defined(_WIN32) && !defined(__CYGWIN__)

	/* This is for MinGW (gcc 4.6.2), not mingw-w64
	 * Fix a lot of missing definitions in winnt.h from MSDN
	 */

#ifndef PRODUCT_ULTIMATE

#define VER_SUITE_WH_SERVER 0x00008000 
 
#define PRODUCT_UNDEFINED                         0x0 
 
#define PRODUCT_ULTIMATE                          0x1 
#define PRODUCT_HOME_BASIC                        0x2 
#define PRODUCT_HOME_PREMIUM                      0x3 
#define PRODUCT_ENTERPRISE                        0x4 
#define PRODUCT_HOME_BASIC_N                      0x5 
#define PRODUCT_BUSINESS                          0x6 
#define PRODUCT_STANDARD_SERVER                   0x7 
#define PRODUCT_DATACENTER_SERVER                 0x8 
#define PRODUCT_SMALLBUSINESS_SERVER              0x9 
#define PRODUCT_ENTERPRISE_SERVER                 0xa 
#define PRODUCT_STARTER                           0xb 
#define PRODUCT_DATACENTER_SERVER_CORE            0xc 
#define PRODUCT_STANDARD_SERVER_CORE              0xd 
#define PRODUCT_ENTERPRISE_SERVER_CORE            0xe 
#define PRODUCT_ENTERPRISE_SERVER_IA64            0xf 
#define PRODUCT_BUSINESS_N                        0x10 
#define PRODUCT_WEB_SERVER                        0x11 
#define PRODUCT_CLUSTER_SERVER                    0x12 
#define PRODUCT_HOME_SERVER                       0x13 
#define PRODUCT_STORAGE_EXPRESS_SERVER            0x14 
#define PRODUCT_STORAGE_STANDARD_SERVER           0x15 
#define PRODUCT_STORAGE_WORKGROUP_SERVER          0x16 
#define PRODUCT_STORAGE_ENTERPRISE_SERVER         0x17 
#define PRODUCT_SERVER_FOR_SMALLBUSINESS          0x18 
#define PRODUCT_SMALLBUSINESS_SERVER_PREMIUM      0x19 
#define PRODUCT_HOME_PREMIUM_N                    0x1a 
#define PRODUCT_ENTERPRISE_N                      0x1b 
#define PRODUCT_ULTIMATE_N                        0x1c 
#define PRODUCT_WEB_SERVER_CORE                   0x1d 
#define PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT  0x1e 
#define PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY    0x1f 
#define PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING   0x20 
#define PRODUCT_SERVER_FOUNDATION                 0x21 
#define PRODUCT_HOME_PREMIUM_SERVER               0x22 
#define PRODUCT_SERVER_FOR_SMALLBUSINESS_V        0x23 
#define PRODUCT_STANDARD_SERVER_V                 0x24 
#define PRODUCT_DATACENTER_SERVER_V               0x25 
#define PRODUCT_ENTERPRISE_SERVER_V               0x26 
#define PRODUCT_DATACENTER_SERVER_CORE_V          0x27 
#define PRODUCT_STANDARD_SERVER_CORE_V            0x28 
#define PRODUCT_ENTERPRISE_SERVER_CORE_V          0x29 
#define PRODUCT_HYPERV                            0x2a 
#define PRODUCT_STORAGE_EXPRESS_SERVER_CORE       0x2b 
#define PRODUCT_STORAGE_STANDARD_SERVER_CORE      0x2c 
#define PRODUCT_STORAGE_WORKGROUP_SERVER_CORE     0x2d 
#define PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE    0x2e 
#define PRODUCT_STARTER_N                         0x2f 
#define PRODUCT_PROFESSIONAL                      0x30 
#define PRODUCT_PROFESSIONAL_N                    0x31 
#define PRODUCT_SB_SOLUTION_SERVER                0x32 
#define PRODUCT_SERVER_FOR_SB_SOLUTIONS           0x33 
#define PRODUCT_STANDARD_SERVER_SOLUTIONS         0x34 
#define PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE    0x35 
#define PRODUCT_SB_SOLUTION_SERVER_EM             0x36 
#define PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM        0x37 
#define PRODUCT_SOLUTION_EMBEDDEDSERVER           0x38 
#define PRODUCT_SOLUTION_EMBEDDEDSERVER_CORE      0x39 
#define PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT     0x3B 
#define PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL     0x3C 
#define PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC  0x3D 
#define PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC  0x3E 
#define PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE 0x3f 
#define PRODUCT_CLUSTER_SERVER_V                  0x40 
#define PRODUCT_EMBEDDED                          0x41 
#define PRODUCT_STARTER_E                         0x42 
#define PRODUCT_HOME_BASIC_E                      0x43 
#define PRODUCT_HOME_PREMIUM_E                    0x44 
#define PRODUCT_PROFESSIONAL_E                    0x45 
#define PRODUCT_ENTERPRISE_E                      0x46 
#define PRODUCT_ULTIMATE_E                        0x47 
 
#define PRODUCT_UNLICENSED                        0xabcdabcd 

#endif /* !PRODUCT_ULTIMATE_E */

/*-------------------------------------------------------------------------*
 * GET_WINDOWS_OS_NAME                                                     *
 *                                                                         *
 * code obtained from                                                      *
 * http://msdn.microsoft.com/library/default.asp?url=/library/en-us/       *
 *        sysinfo/base/getting_the_system_version.asp                      *
 * then:                                                                   *
 *  - indent (with .indent.pro in src or with -gnu -bap -npcs)             *
 *  - replace function by "static Bool Get_Windows_OS_Name(char *pszOS)"   *
 *  - fix bug replace if(bOsVersionInfoEx != NULL ) return 1; by           *
 *                    if(bOsVersionInfoEx == 0) return FALSE;              *
 *  - after "if (VER_PLATFORM_WIN32_NT == osvi.dwPlatformId..." add        *
 *          pl_m_os_type = M_OS_WINDOWS_NT;                                *
 *  - add the following defines (BUFSIZE is ignored)                       *
 *    #define StringCchCat(d, sz, s)           strcat(d, s)                *
 *    #define StringCchCopy(d, sz, s)          strcpy(d, s)                *
 *    #define StringCchPrintf(b, sz, fmt, ...) sprintf(b, fmt, __VA_ARGS__)*
 *  - fix warnings %d -> %ld                                               *
 *-------------------------------------------------------------------------*/
static Bool
Get_Windows_OS_Name(char *pszOS)
{
typedef void (WINAPI *PGNSI) (LPSYSTEM_INFO);
typedef BOOL (WINAPI *PGPI) (DWORD, DWORD, DWORD, DWORD, PDWORD);

#define StringCchCat(d, sz, s)           strcat(d, s)
#define StringCchCopy(d, sz, s)          strcpy(d, s)
#define StringCchPrintf(b, sz, fmt, ...) sprintf(b, fmt, __VA_ARGS__)

  OSVERSIONINFOEX osvi;
  SYSTEM_INFO si;
  PGNSI pGNSI;
  PGPI pGPI;
  BOOL bOsVersionInfoEx;
  DWORD dwType;
  TCHAR buf[80];


  ZeroMemory(&si, sizeof(SYSTEM_INFO));
  ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));

  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
  bOsVersionInfoEx = GetVersionEx((OSVERSIONINFO *) & osvi);

  if (bOsVersionInfoEx == 0)
    return FALSE;

  // Call GetNativeSystemInfo if supported or GetSystemInfo otherwise.

  pGNSI = (PGNSI) GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
				 "GetNativeSystemInfo");
  if (NULL != pGNSI)
    pGNSI(&si);
  else
    GetSystemInfo(&si);

  if (VER_PLATFORM_WIN32_NT == osvi.dwPlatformId && osvi.dwMajorVersion > 4)
    {
      pl_m_os_type = M_OS_WINDOWS_NT;      
      StringCchCopy(pszOS, BUFSIZE, TEXT("Microsoft "));

      // Test for the specific product.

      if (osvi.dwMajorVersion == 6)
	{
	  if (osvi.dwMinorVersion == 0)
	    {
	      if (osvi.wProductType == VER_NT_WORKSTATION)
		StringCchCat(pszOS, BUFSIZE, TEXT("Windows Vista "));
	      else
		StringCchCat(pszOS, BUFSIZE, TEXT("Windows Server 2008 "));
	    }

	  if (osvi.dwMinorVersion == 1)
	    {
	      if (osvi.wProductType == VER_NT_WORKSTATION)
		StringCchCat(pszOS, BUFSIZE, TEXT("Windows 7 "));
	      else
		StringCchCat(pszOS, BUFSIZE,
			     TEXT("Windows Server 2008 R2 "));
	    }

	  pGPI =
	    (PGPI) GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
				  "GetProductInfo");

	  pGPI(osvi.dwMajorVersion, osvi.dwMinorVersion, 0, 0, &dwType);

	  switch (dwType)
	    {
	    case PRODUCT_ULTIMATE:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Ultimate Edition"));
	      break;
	    case PRODUCT_PROFESSIONAL:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Professional"));
	      break;
	    case PRODUCT_HOME_PREMIUM:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Home Premium Edition"));
	      break;
	    case PRODUCT_HOME_BASIC:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Home Basic Edition"));
	      break;
	    case PRODUCT_ENTERPRISE:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition"));
	      break;
	    case PRODUCT_BUSINESS:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Business Edition"));
	      break;
	    case PRODUCT_STARTER:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Starter Edition"));
	      break;
	    case PRODUCT_CLUSTER_SERVER:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Cluster Server Edition"));
	      break;
	    case PRODUCT_DATACENTER_SERVER:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Edition"));
	      break;
	    case PRODUCT_DATACENTER_SERVER_CORE:
	      StringCchCat(pszOS, BUFSIZE,
			   TEXT("Datacenter Edition (core installation)"));
	      break;
	    case PRODUCT_ENTERPRISE_SERVER:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition"));
	      break;
	    case PRODUCT_ENTERPRISE_SERVER_CORE:
	      StringCchCat(pszOS, BUFSIZE,
			   TEXT("Enterprise Edition (core installation)"));
	      break;
	    case PRODUCT_ENTERPRISE_SERVER_IA64:
	      StringCchCat(pszOS, BUFSIZE,
			   TEXT("Enterprise Edition for Itanium-based Systems"));
	      break;
	    case PRODUCT_SMALLBUSINESS_SERVER:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Small Business Server"));
	      break;
	    case PRODUCT_SMALLBUSINESS_SERVER_PREMIUM:
	      StringCchCat(pszOS, BUFSIZE,
			   TEXT("Small Business Server Premium Edition"));
	      break;
	    case PRODUCT_STANDARD_SERVER:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Standard Edition"));
	      break;
	    case PRODUCT_STANDARD_SERVER_CORE:
	      StringCchCat(pszOS, BUFSIZE,
			   TEXT("Standard Edition (core installation)"));
	      break;
	    case PRODUCT_WEB_SERVER:
	      StringCchCat(pszOS, BUFSIZE, TEXT("Web Server Edition"));
	      break;
	    }
	}

      if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2)
	{
	  if (GetSystemMetrics(SM_SERVERR2))
	    StringCchCat(pszOS, BUFSIZE, TEXT("Windows Server 2003 R2, "));
	  else if (osvi.wSuiteMask & VER_SUITE_STORAGE_SERVER)
	    StringCchCat(pszOS, BUFSIZE,
			 TEXT("Windows Storage Server 2003"));
	  else if (osvi.wSuiteMask & VER_SUITE_WH_SERVER)
	    StringCchCat(pszOS, BUFSIZE, TEXT("Windows Home Server"));
	  else if (osvi.wProductType == VER_NT_WORKSTATION &&
		   si.wProcessorArchitecture ==
		   PROCESSOR_ARCHITECTURE_AMD64)
	    {
	      StringCchCat(pszOS, BUFSIZE,
			   TEXT("Windows XP Professional x64 Edition"));
	    }
	  else
	    StringCchCat(pszOS, BUFSIZE, TEXT("Windows Server 2003, "));

	  // Test for the server type.
	  if (osvi.wProductType != VER_NT_WORKSTATION)
	    {
	      if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_IA64)
		{
		  if (osvi.wSuiteMask & VER_SUITE_DATACENTER)
		    StringCchCat(pszOS, BUFSIZE,
				 TEXT("Datacenter Edition for Itanium-based Systems"));
		  else if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
		    StringCchCat(pszOS, BUFSIZE,
				 TEXT("Enterprise Edition for Itanium-based Systems"));
		}

	      else if (si.wProcessorArchitecture ==
		       PROCESSOR_ARCHITECTURE_AMD64)
		{
		  if (osvi.wSuiteMask & VER_SUITE_DATACENTER)
		    StringCchCat(pszOS, BUFSIZE,
				 TEXT("Datacenter x64 Edition"));
		  else if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
		    StringCchCat(pszOS, BUFSIZE,
				 TEXT("Enterprise x64 Edition"));
		  else
		    StringCchCat(pszOS, BUFSIZE,
				 TEXT("Standard x64 Edition"));
		}

	      else
		{
		  if (osvi.wSuiteMask & VER_SUITE_COMPUTE_SERVER)
		    StringCchCat(pszOS, BUFSIZE,
				 TEXT("Compute Cluster Edition"));
		  else if (osvi.wSuiteMask & VER_SUITE_DATACENTER)
		    StringCchCat(pszOS, BUFSIZE,
				 TEXT("Datacenter Edition"));
		  else if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
		    StringCchCat(pszOS, BUFSIZE,
				 TEXT("Enterprise Edition"));
		  else if (osvi.wSuiteMask & VER_SUITE_BLADE)
		    StringCchCat(pszOS, BUFSIZE, TEXT("Web Edition"));
		  else
		    StringCchCat(pszOS, BUFSIZE, TEXT("Standard Edition"));
		}
	    }
	}

      if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1)
	{
	  StringCchCat(pszOS, BUFSIZE, TEXT("Windows XP "));
	  if (osvi.wSuiteMask & VER_SUITE_PERSONAL)
	    StringCchCat(pszOS, BUFSIZE, TEXT("Home Edition"));
	  else
	    StringCchCat(pszOS, BUFSIZE, TEXT("Professional"));
	}

      if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0)
	{
	  StringCchCat(pszOS, BUFSIZE, TEXT("Windows 2000 "));

	  if (osvi.wProductType == VER_NT_WORKSTATION)
	    {
	      StringCchCat(pszOS, BUFSIZE, TEXT("Professional"));
	    }
	  else
	    {
	      if (osvi.wSuiteMask & VER_SUITE_DATACENTER)
		StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Server"));
	      else if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
		StringCchCat(pszOS, BUFSIZE, TEXT("Advanced Server"));
	      else
		StringCchCat(pszOS, BUFSIZE, TEXT("Server"));
	    }
	}

      // Include service pack (if any) and build number.

      if (strlen(osvi.szCSDVersion) > 0)
	{
	  StringCchCat(pszOS, BUFSIZE, TEXT(" "));
	  StringCchCat(pszOS, BUFSIZE, osvi.szCSDVersion);
	}

      StringCchPrintf(buf, 80, TEXT(" (build %ld)"), osvi.dwBuildNumber);
      StringCchCat(pszOS, BUFSIZE, buf);

      if (osvi.dwMajorVersion >= 6)
	{
	  if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
	    StringCchCat(pszOS, BUFSIZE, TEXT(", 64-bit"));
	  else if (si.wProcessorArchitecture ==
		   PROCESSOR_ARCHITECTURE_INTEL)
	    StringCchCat(pszOS, BUFSIZE, TEXT(", 32-bit"));
	}

      return TRUE;
    }
  else
    {
      return FALSE;
    }
}

#endif




/*-------------------------------------------------------------------------*
 * PL_M_CREATE_SHELL_COMMAND                                               *
 *                                                                         *
 * Create a shell command if != NULL (or else a shell invocation)          *
 *-------------------------------------------------------------------------*/
char **
Pl_M_Create_Shell_Command(char *cmd)
{
  static char *arg[4];
  char *p;

#if defined(__unix__) || defined(__CYGWIN__)

  arg[0] = ((p = getenv("SHELL")) != NULL) ? p : "/bin/sh";
  arg[1] = "-c";

#else

  arg[0] = ((p = getenv("COMSPEC")) != NULL)
    ? p : (pl_m_os_type == M_OS_WINDOWS_NT) ? "cmd.exe" : "c:\\command.com";
  arg[1] = "/c";

#endif

  if (cmd)
    {
      arg[2] = cmd;
      arg[3] = NULL;
    }
  else
    arg[1] = NULL;

  return arg;
}




/*-------------------------------------------------------------------------*
 * PL_M_CMD_LINE_TO_ARGV                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char **
Pl_M_Cmd_Line_To_Argv(char *cmd, int *argc)
{
  static char **arg = NULL;
  static int nb_arg = 0;
  char *p = cmd;
  int i = 0;

  for (;;)
    {
      while (*p == ' ' || *p == '\t')
	p++;

      if (*p == '\0')
	break;

      if (i >= nb_arg)
	{
	  nb_arg += 64;
	  arg = (arg == NULL) ? malloc(nb_arg * sizeof(char *))
	    : realloc(arg, nb_arg * sizeof(char *));
	}
      arg[i++] = p;

      while (*p != ' ' && *p != '\t' && *p != '\0')
	{
	  if (*p == '"')
	  {
	    do
	      p++;
	    while (*p != '"' && *p != '\0');
	    if (*p == '"')
	      p++;
	  }
	  else
	    p++;
	}

      if (*p == '\0')
	break;

      *p++ = '\0';
    }

  arg[i] = NULL;
  if (argc != NULL)
    *argc = i;
  return arg;
}




/*-------------------------------------------------------------------------*
 * PL_M_SHELL                                                              *
 *                                                                         *
 * Invoke a shell (eventually passing a cmd if != NULL)                    *
 *-------------------------------------------------------------------------*/
int
Pl_M_Shell(char *cmd)
{
  return Pl_M_Spawn(Pl_M_Create_Shell_Command(cmd));
}




/*-------------------------------------------------------------------------*
 * PL_M_SPAWN                                                              *
 *                                                                         *
 * Execute a command with arguments in arg[], (arg[0]=the name of the cmd) *
 * a NULL must follow the last argument.                                   *
 * if arg[1]==(char *) 1 then arg[0] is considered as a command-line.      *
 * return the status or -1 if cannot execute (errno is set) or -2 else     *
 * (errno is not set).                                                     *
 *-------------------------------------------------------------------------*/
int
Pl_M_Spawn(char *arg[])
{
#if defined(__unix__)
  int pid;

  fflush(stdout);
  fflush(stderr);

  if (arg[1] == (char *) 1)
    arg = Pl_M_Cmd_Line_To_Argv(arg[0], NULL);

  pid = fork();

  if (pid == -1)
    return -1;

  if (pid == 0)			/* child process */
    {
      execvp(arg[0], arg);	/* only returns on error */
      exit((errno == ENOENT || errno == ENOTDIR) ? 126 : 127);
    }

  return Pl_M_Get_Status(pid);

#else

#if defined(_MSC_VER)
  _flushall();
#endif

  /*  printf("COMMAND: <%s>\n", arg[0]); */
  if (arg[1] == (char *) 1)
    arg = Pl_M_Cmd_Line_To_Argv(arg[0], NULL);
  /*
  {
    int i;
    for(i = 0; arg[i] != NULL; i++)
      printf("Arg :%d: <%s>\n", i, arg[i]);
  }
  */
  

  return spawnvp(_P_WAIT, arg[0], (char *const *) arg);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_M_SPAWN_REDIRECT                                                     *
 *                                                                         *
 * Execute a command with arguments in arg[], (arg[0]=the name of the cmd) *
 * a NULL must follow the last argument.                                   *
 * if arg[1]==(char *) 1 then arg[0] is considered as a command-line.      *
 * detach: 1 for a detached process (cannot obtain its status then).       *
 * f_in, f_out, f_err: ptrs to FILE * vars. if NULL not redirected,        *
 * f_out==f_err the 2 output streams are merged in f_out.                  *
 * In case of error return -1 if errno is set or else -2.                  *
 * In case of success, return 0 if detached or the pid else (the function  *
 * Pl_M_Get_Status() should be called later to avoid zombie processes).    *
 *-------------------------------------------------------------------------*/
int
Pl_M_Spawn_Redirect(char *arg[], int detach,
		    FILE **f_in, FILE **f_out, FILE **f_err)
{
#if defined(__unix__ ) || defined(__CYGWIN__)
  int pipe_in[2], pipe_out[2], pipe_err[2];
  int pid, status;

  fflush(stdout);
  fflush(stderr);

  if (arg[1] == (char *) 1)
    arg = Pl_M_Cmd_Line_To_Argv(arg[0], NULL);

  if ((f_in && pipe(pipe_in)) ||
      (f_out && pipe(pipe_out)) ||
      (f_err && f_err != f_out && pipe(pipe_err)))
    goto err;

  pid = (int) fork();
  if (pid == -1)
    goto err;

  if (pid == 0)			/* the child process */
    {
      if (!detach || fork() == 0)	/* pid needed ? */
	{			/* nested fork to detach exec process to avoid zombie process */
	  if (f_in && (close(pipe_in[1]) ||
		       (pipe_in[0] != 0 &&
			(dup2(pipe_in[0], 0) == -1 || close(pipe_in[0])))))
	    goto err;

	  if (f_out && (close(pipe_out[0]) ||
			(pipe_out[1] != 1 &&
			 (dup2(pipe_out[1], 1) == -1
			  || close(pipe_out[1])))))
	    goto err;

	  if (f_err)
	    {
	      if (f_err != f_out)
		{
		  if (close(pipe_err[0]) ||
		      (pipe_err[1] != 2 &&
		       (dup2(pipe_err[1], 2) == -1 || close(pipe_err[1]))))
		    goto err;
		}
	      else if (dup2(1, 2) == -1)
		goto err;
	    }

	  execvp(arg[0], arg);	/* only returns on error */
#ifdef DEBUG
	  DBGPRINTF("ERROR EXEC errno=%d\n", errno);
#endif
	  exit((errno == ENOENT || errno == ENOTDIR) ? 126 : 127);
	}
      else
	exit(0);		/* detatch: terminate child */
    }

  if (detach)			/* wait child termination */
    {
      if (waitpid(pid, &status, 0) < 0)
	goto err;
      pid = 0;
    }

  if (f_in && (close(pipe_in[0]) ||
	       (*f_in = fdopen(pipe_in[1], "wt")) == NULL))
    goto err;

  if (f_out && (close(pipe_out[1]) ||
		(*f_out = fdopen(pipe_out[0], "rt")) == NULL))
    goto err;

  if (f_err && f_err != f_out &&
      (close(pipe_err[1]) || (*f_err = fdopen(pipe_err[0], "rt")) == NULL))
    goto err;

  return pid;			/* NB: if detach: pid = 0 */

err:
  return -1;

#else

  int status;
  SECURITY_ATTRIBUTES sa = { 0 };
  STARTUPINFO si = { 0 };
  PROCESS_INFORMATION pi = { 0 };
  HANDLE pipe_in_r = NULL;
  HANDLE pipe_in_w = NULL;
  HANDLE pipe_out_r = NULL;
  HANDLE pipe_out_w = NULL;
  HANDLE pipe_err_r = NULL;
  HANDLE pipe_err_w = NULL;
  static char buff[4096];
  char *cmd, *p;
  static char delim[2] = { '\0', '\0' };
  int i, n;

  sa.nLength = sizeof(sa);
  sa.bInheritHandle = TRUE;
  sa.lpSecurityDescriptor = NULL;

  if ((f_in && !CreatePipe(&pipe_in_r, &pipe_in_w, &sa, 0)) ||
      (f_out && !CreatePipe(&pipe_out_r, &pipe_out_w, &sa, 0)) ||
      (f_err && f_err != f_out
       && !CreatePipe(&pipe_err_r, &pipe_err_w, &sa, 0)))
    goto windows_err;

  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
  si.wShowWindow = SW_HIDE;
  si.hStdInput = (f_in) ? pipe_in_r : GetStdHandle(STD_INPUT_HANDLE);
  si.hStdOutput = (f_out) ? pipe_out_w : GetStdHandle(STD_OUTPUT_HANDLE);
  si.hStdError = (f_err) ? ((f_err == f_out) ? pipe_out_w : pipe_err_w)
    : GetStdHandle(STD_ERROR_HANDLE);
  if (arg[1] == NULL || arg[1] == (char *) 1)
    cmd = arg[0];
  else
    {
      for (n = i = 0; arg[i]; i++)
	{
	  *delim = '\0';
	  for (p = arg[i]; *p; p++)
	    if (*p == ' ' || *p == '\t')
	      {
		*delim = '"';
		break;
	      }
	  n += sprintf(buff + n, "%s%s%s ", delim, arg[i], delim);
	}
      buff[n - 1] = '\0';
      cmd = buff;
    }

#ifdef DEBUG
  DBGPRINTF("   cmd=<%s>\n", cmd);
#endif
  if (!CreateProcess(NULL, cmd, NULL, NULL, TRUE,
		     (detach) ? DETACHED_PROCESS : 0, NULL, NULL, &si, &pi))
    {
      status = GetLastError();
#ifdef DEBUG
      DBGPRINTF("ERROR from Create_Process=%d\n", status);
#endif
      if (status == ERROR_FILE_NOT_FOUND || status == ERROR_PATH_NOT_FOUND)
	{
	  errno = ENOENT;
	  goto err;
	}
      goto windows_err;
    }

  if ((f_in && !CloseHandle(pipe_in_r)) ||
      (f_out && !CloseHandle(pipe_out_w)) ||
      (f_err && f_err != f_out && !CloseHandle(pipe_err_w)))
    goto windows_err;

  if (f_in &&
      (*f_in =
       fdopen(_open_osfhandle((PlLong) pipe_in_w, _O_TEXT), "wt")) == NULL)
    goto err;

  if (f_out &&
      (*f_out =
       fdopen(_open_osfhandle((PlLong) pipe_out_r, _O_TEXT), "rt")) == NULL)
    goto err;

  if (f_err && f_err != f_out &&
      (*f_err =
       fdopen(_open_osfhandle((PlLong) pipe_err_r, _O_TEXT), "rt")) == NULL)
    goto err;

  /* return (detach) ? 0 : (int) pi.hProcess;
   * JAT: Changed to use id rather than handle (64 bits) because of fixed bitness
   * OpenProcess function may be needed else where to get handle back
   */
  return (detach) ? 0 : pi.dwProcessId;

err:
  return -1;

windows_err:
  return M_ERROR_WIN32;
#endif
}




/*-------------------------------------------------------------------------*
 * PL_M_GET_STATUS                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_M_Get_Status(int pid)
{
  int status = 0;

#if defined(__unix__) || defined(__CYGWIN__)

  if (waitpid(pid, &status, 0) < 0)
    return -1;

  if (WIFEXITED(status))
    {
      status = WEXITSTATUS(status);
      if (status == 127)
	status = -2;
      else if (status == 126)
	{
	  status = -1;
	  errno = ENOENT;
	}
    }

#elif defined(_WIN32)

  /* JAT: See above
   * DD (bug XP/Vista) HANDLE phandle = OpenProcess(PROCESS_ALL_ACCESS, 1, pid);
   */
  HANDLE phandle = OpenProcess(SYNCHRONIZE | PROCESS_QUERY_INFORMATION, 1, pid);

  if (phandle == 0)
    {
#ifdef DEBUG
      printf("ERROR from OpenProcess: %d\n", (int) GetLastError());
#endif
      status = M_ERROR_WIN32;
      return status;
    }
  if (WaitForSingleObject(phandle, INFINITE) == WAIT_FAILED) 
    {
#ifdef DEBUG
      printf("ERROR from WaitForSingleObject: %d\n", (int) GetLastError());
#endif
      status = M_ERROR_WIN32;
    } 
  else if (!GetExitCodeProcess(phandle, (LPDWORD) &status))
    {
#ifdef DEBUG
      printf("ERROR from GetExitCodeProcess: %d\n", (int) GetLastError());
#endif
      status = M_ERROR_WIN32;
    }

  CloseHandle(phandle);
#endif

  return status;
}




/*-------------------------------------------------------------------------*
 * PL_M_MKTEMP                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Mktemp(char *tmpl)
{
				/* redefined to avoid link warning */
#if defined(__unix__) || defined(__CYGWIN__)
				/* this code comes from glibc */
  int len;
  char *XXXXXX;
  static PlULong value;
  int count;
  struct stat buf;
  static const char letters[] =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

#ifndef TMP_MAX
#define TMP_MAX 238328
#endif

  len = strlen (tmpl);
  if (len < 6 || strcmp(&tmpl[len - 6], "XXXXXX"))
    {
      errno = EINVAL;
      return NULL;
    }

  /* This is where the Xs start.  */
  XXXXXX = &tmpl[len - 6];

  value += (PlULong) time(NULL) ^ getpid();

  for (count = 0; count < TMP_MAX; value += 7777, ++count)
    {
      PlULong v = value;

      /* Fill in the random bits.  */
      XXXXXX[0] = letters[v % 62];
      v /= 62;
      XXXXXX[1] = letters[v % 62];
      v /= 62;
      XXXXXX[2] = letters[v % 62];
      v /= 62;
      XXXXXX[3] = letters[v % 62];
      v /= 62;
      XXXXXX[4] = letters[v % 62];
      v /= 62;
      XXXXXX[5] = letters[v % 62];

      if (lstat(tmpl, &buf) < 0)
	{
	  if (errno == ENOENT)
	    {
	      errno = 0;
	      return tmpl;
	    }
	  else
	    /* Give up now. */
	    return NULL;
	}
    }

  /* We got out of the loop because we ran out of combinations to try.  */
  errno = EEXIST;
  return NULL;

#else

  errno = 0;
  return mktemp(tmpl);

#endif
}




/*-------------------------------------------------------------------------*
 * PL_M_TEMPNAM                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Tempnam(char *dir, char *pfx)
{
#if defined(__unix__) || defined(__CYGWIN__)
				/* this code comes from glibc */
  char tmpl[MAXPATHLEN];
  char *d;
  int dlen, plen;
  struct stat buf;

#ifndef P_tmpdir
#define P_tmpdir "/tmp"
#endif

#define Dir_Exists(dir) (stat(dir, &buf) == 0 && S_ISDIR (buf.st_mode))


  if (!pfx || !pfx[0])
    {
      pfx = "file";
      plen = 4;
    }
  else
    {
      plen = strlen(pfx);
      if (plen > 5)
        plen = 5;
    }

  d = getenv("TMPDIR");
  if (d != NULL && Dir_Exists(d))
    dir = d;
  else if (dir != NULL && Dir_Exists(dir))
    /* nothing */ ;
  else
    dir = NULL;

  if (dir == NULL)
    {
      if (Dir_Exists(P_tmpdir))
        dir = P_tmpdir;
      else if (strcmp(P_tmpdir, "/tmp") != 0 && Dir_Exists("/tmp"))
        dir = "/tmp";
      else
        {
          errno = ENOENT;
          return NULL;
        }
    }

  dlen = strlen(dir);
  while (dlen > 1 && dir[dlen - 1] == '/')
    dlen--;                     /* remove trailing slashes */

  /* check we have room for "${dir}/${pfx}XXXXXX\0" */
  if (MAXPATHLEN < dlen + 1 + plen + 6 + 1)
    {
      errno = EINVAL;
      return NULL;
    }

  sprintf(tmpl, "%.*s/%.*sXXXXXX", dlen, dir, plen, pfx);
  d = Pl_M_Mktemp(tmpl);
  if (d)
    d = strdup(d);
  return d;

#else

  errno = 0;
  if (dir == NULL && getenv("TMP") == NULL) /* under Win32, _tempnam checks TMP */
    dir = "./";
  return tempnam(dir, pfx);

#endif
}



#ifdef USE_ALONE

/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 * to compile alone active USE_ALONE and simply compile this file.         *
 * Under Win32 to also test with the GUI Console active USE_W32_GUI_CONSOLE*
 * and compile with gplc machine1.c or gplc machine1.c --gui-console       *
 * WIN32 WARNING: it seems that the executable file name must be at least 2*
 * characters long (e.g. x.exe is not OK but xx.exe yes).                  *
 *-------------------------------------------------------------------------*/

#if defined(__unix__) || defined(__CYGWIN__)
#define PREFIX_DIR
#else
#define PREFIX_DIR "c:\\cygwin\\bin\\"
#endif


#define READ(str, f)					\
{							\
  char buff[1024];					\
 							\
  DBGPRINTF("\n   Reading redirected %s\n", str);	\
  while (fgets(buff, sizeof(buff), f))			\
    {							\
      if (buff[strlen(buff) - 1] == '\n')		\
        buff[strlen(buff) - 1] = '\0';			\
      DBGPRINTF("   <%s>\n", buff);			\
      if (feof(f))					\
        break;						\
    }							\
  fclose(f);						\
  DBGPRINTF("   End reading redirected %s\n", str);	\
}


#define CHECK(pid)						\
{								\
  if (pid == -1)						\
    {								\
      DBGPRINTF("   ERROR executing Spawn: errno=%d\n", errno);	\
      exit(1);							\
    }								\
  if (pid == -2)						\
    {								\
      DBGPRINTF("ERROR executing Spawn: unknown error\n");	\
      exit(1);							\
    }								\
  DBGPRINTF("   pid=%d (%x)\n", pid, pid);			\
}


#define STAT(pid)				\
{						\
  int status = Pl_M_Get_Status(pid);		\
  STATUS(status)				\
}

#define STATUS(status)				\
{						\
  DBGPRINTF("   status=%d", status);		\
  if (status == -1)				\
    DBGPRINTF(" errno=%d", errno);		\
  DBGPRINTF("\n\n");				\
}


#if 1
#define COMMAND								\
  strcpy(buff, PREFIX_DIR "bc --q");    /* should be modifiable */	\
  arg[0] = buff;							\
  arg[1] = (char *) 1;
#else
#define COMMAND
 arg[0]=PREFIX_DIR "bc";			\
 arg[1]="-q";					\
 arg[2]=NULL;
#endif

#define CDE_STRING "1+255\n$foo\n2^10\nquit\n"
#define CDE_INPUT  fprintf(i, CDE_STRING); fclose(i);


#if 0
#define POLL
#include <sys/poll.h>
#endif
#ifdef POLL
  {
    int fd = fileno(i);
    struct pollfd ufd = { fd, 7, 0 };
    int r = poll(&ufd, 1, 100);
    DBGPRINTF("poll ret:%d on fd %d returned events :%x\n", r, fd, ufd.revents);
    return 0;
  }
#endif

int
main(int argc, char *argv[])
{
  FILE *i, *o, *e;
  int pid, status;
  char *arg[10];
  char buff[256];

  Pl_Init_Machine1();
  printf("OS used:%s\n", pl_m_os_version);


#if defined(_MSC_VER)
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
#endif

#ifdef USE_W32_GUI_CONSOLE
  {
    char buff[100];

    DBGPRINTF("HELLO World\n");
    Pl_LE_Gets(buff);
  }
#endif

#if 1
  if (argc > 1)
    {
      DBGPRINTF("1- Executing from argv[1]...=%s... no redirect\n",
		argv[1]);
      pid = Pl_M_Spawn_Redirect(argv + 1, 0, NULL, NULL, NULL);
      CHECK(pid);
      STAT(pid);

      DBGPRINTF("1b- Executing from argv[1]...=%s... Spawn\n", argv[1]);
      status = Pl_M_Spawn(argv + 1);
      STATUS(status);
    }
  else
    DBGPRINTF("1- Executing from argv[1] - ignored\n");

#endif

#if 1
  DBGPRINTF("2- Executing uname -a with redirected output\n");
  strcpy(buff, PREFIX_DIR "uname -a");	/* should be modifiable */
  arg[0] = buff;
  arg[1] = (char *) 1;
  pid = Pl_M_Spawn_Redirect(arg, 0, NULL, &o, NULL);
  CHECK(pid);
  READ("output", o);
  STAT(pid);
#endif

  COMMAND;
  DBGPRINTF("Command is: %s with following input:\n" CDE_STRING,
	    arg[0]);
  DBGPRINTF("--- end of input\n");



#if 1
  DBGPRINTF("3- command with redirected input\n");
  COMMAND;
  pid = Pl_M_Spawn_Redirect(arg, 0, &i, NULL, NULL);
  CHECK(pid);

  CDE_INPUT;
  STAT(pid);
#endif

#if 1
  DBGPRINTF("4- command with redirected input and output\n");
  COMMAND;
  pid = Pl_M_Spawn_Redirect(arg, 0, &i, &o, NULL);
  CHECK(pid);
  CDE_INPUT;
  READ("output", o);
  STAT(pid);
#endif

#if 1
  DBGPRINTF("5- command with redirected input output and error\n");
  COMMAND;
  pid = Pl_M_Spawn_Redirect(arg, 0, &i, &o, &e);
  CHECK(pid);
  CDE_INPUT;
  READ("output", o);
  READ("error", e);
  STAT(pid);
#endif

#if 1
  DBGPRINTF("6- command with redirected input and output=error\n");
  COMMAND;
  pid = Pl_M_Spawn_Redirect(arg, 0, &i, &o, &o);
  CHECK(pid);
  CDE_INPUT;
  READ("output/error", o);
  STAT(pid);
#endif

#ifdef USE_W32_GUI_CONSOLE
  {				/* for W32GUICons */
    char buff[100];

    DBGPRINTF("Terminated - press ENTER\n");
    Pl_LE_Gets(buff);
  }
#endif

  return 0;
}

#endif /* USE_ALONE */
