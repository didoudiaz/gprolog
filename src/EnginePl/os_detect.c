/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : os_detect.c                                                     *
 * Descr.: OS information detection                                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2026 Daniel Diaz                                     *
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

#include "gp_config.h"

#include <stdio.h>
#include <string.h>

#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#include <sys/utsname.h>
#else
#define WIN32_LEAN_AND_MEAN
#define WIN32_NO_STATUS
#include <windows.h>
#undef WIN32_NO_STATUS
#include <ntstatus.h>
#include <winnt.h>
#endif


#include "bool.h"
#define OS_DETECT_FILE
#include "os_detect.h"



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define M_OS_UNIX                  0
#define M_OS_WINDOWS               1
#define M_OS_WINDOWS_NT            2



/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#if defined(_WIN32) && !defined(__CYGWIN__)
static Bool Get_Windows_OS_Name(char *buff);
#endif




/*-------------------------------------------------------------------------*
 * PL_INIT_OS_DETECT                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Init_OS_Detect(void)
{
#if defined(__unix__) || defined(__CYGWIN__)

  struct utsname uname_info;

  pl_os_type = M_OS_UNIX;

  if (uname(&uname_info) < 0)
    {
      strcpy(pl_architecture, "unknown architecture");
      strcpy(pl_os_version, "unknown OS version");
      return;
    }

  strcpy(pl_architecture, uname_info.machine);

  sprintf(pl_os_version, "%s %s", uname_info.sysname, uname_info.release);

#else

  SYSTEM_INFO si;

  GetSystemInfo(&si);
  if (si.wProcessorLevel >= 3 && si.wProcessorLevel < 10)
    sprintf(pl_architecture, "i%c86", si.wProcessorLevel + '0');
  else
    sprintf(pl_architecture, "i%ld", si.dwProcessorType);

  pl_os_type = M_OS_WINDOWS;
  if (!Get_Windows_OS_Name(pl_os_version))
    strcpy(pl_os_version, "unknown OS version");

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
 *  - indent (with -gnu -bap -bad -npcs -cs -l80 -lc80)                    *
 *  - replace function by "static Bool Get_Windows_OS_Name(char *pszOS)"   *
 *  - fix bug replace if(bOsVersionInfoEx != NULL ) return 1; by           *
 *                    if(bOsVersionInfoEx == 0) return FALSE;              *
 *  - after "if (VER_PLATFORM_WIN32_NT == osvi.dwPlatformId..." add        *
 *          pl_os_type = M_OS_WINDOWS_NT;                                  *
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
      pl_os_type = M_OS_WINDOWS_NT;      
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
