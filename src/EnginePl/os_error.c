/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : os_error.c                                                      *
 * Descr.: OS error management                                             *
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
#include <errno.h>

#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>
#endif

#include "os_error.h"


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define UNKNOWN_SYS_ERRNO          "Unknown error (%d)"

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/


/*-------------------------------------------------------------------------*
 * PL_SYS_ERR_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Sys_Err_String(int err_no)
{
#ifdef M_sparc32_sunos
  extern char *sys_errlist[];
  extern int sys_nerr;
#endif

  char *str;
  static char buff[64];

#if defined(_WIN32) || defined(__CYGWIN__)
  if (err_no == OS_ERROR_WIN32)
    {
      int status = GetLastError();

      if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, status,
			MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US),
			buff, sizeof(buff), NULL) == 0)
	sprintf(buff, "Windows " UNKNOWN_SYS_ERRNO, status);
      else
	{			/* windows adds a ".\r\n" at end - remove it */
	  char *p = buff + strlen(buff);
	  while (--p > buff && (isspace(*p) || *p == '.'))
	    {
	    }
	  p[1] = '\0';
	}

      return buff;
    }
#endif


#if defined(M_sparc32_sunos)
  str = (err_no >= 0 && err_no < sys_nerr) ? sys_errlist[err_no] : NULL;
#else
  str = strerror(err_no);
#endif

  if (str)
    return str;

  sprintf(buff, UNKNOWN_SYS_ERRNO, err_no);
  return buff;
}




#ifdef _WIN32
 
/*-------------------------------------------------------------------------*
 * PL_WIN_ERROR_TO_ERRNO                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Win_Error_To_Errno(void)
{
  switch(GetLastError())
    {
    case 0:
      return errno = 0;
      
    case ERROR_BAD_ENVIRONMENT:
      return errno = E2BIG;

    case ERROR_ACCESS_DENIED:
    case ERROR_CANNOT_MAKE:
    case ERROR_CURRENT_DIRECTORY:
    case ERROR_DRIVE_LOCKED:
    case ERROR_FAIL_I24:
    case ERROR_LOCK_FAILED:
    case ERROR_NOT_LOCKED:
    case ERROR_SEEK_ON_DEVICE:
    case ERROR_SHARING_VIOLATION:
      return errno = EACCES;

    case ERROR_MAX_THRDS_REACHED:
    case ERROR_NESTING_NOT_ALLOWED:
    case ERROR_NO_PROC_SLOTS:
      return errno = EAGAIN;

    case ERROR_DIRECT_ACCESS_HANDLE:
      /*  case ERROR_INVALID_HANDLE: put it in EINVAL */
    case ERROR_INVALID_TARGET_HANDLE:
      return errno = EBADF;
      
    case ERROR_CHILD_NOT_COMPLETE:
    case ERROR_WAIT_NO_CHILDREN:
      return errno = ECHILD;

    case ERROR_ALREADY_EXISTS:
    case ERROR_FILE_EXISTS:
      return errno = EEXIST;

    case ERROR_INVALID_ACCESS:
    case ERROR_INVALID_DATA:
    case ERROR_INVALID_FUNCTION:
    case ERROR_INVALID_HANDLE:
    case ERROR_INVALID_PARAMETER:
    case ERROR_NEGATIVE_SEEK:
      return errno = EINVAL;

    case ERROR_TOO_MANY_OPEN_FILES:
      return errno = EMFILE;
      
    case ERROR_BAD_NET_NAME:
    case ERROR_BAD_NETPATH:
    case ERROR_BAD_PATHNAME:
    case ERROR_DELETE_PENDING:
    case ERROR_FILE_NOT_FOUND:
    case ERROR_FILENAME_EXCED_RANGE:
    case ERROR_INVALID_DRIVE:
    case ERROR_NO_MORE_FILES:
    case ERROR_PATH_NOT_FOUND:
      return errno = ENOENT;
      
    case ERROR_BAD_FORMAT:
      return errno = ENOEXEC;

    case ERROR_ARENA_TRASHED:
    case ERROR_INVALID_BLOCK:
    case ERROR_NOT_ENOUGH_MEMORY:
    case ERROR_NOT_ENOUGH_QUOTA:
      return errno = ENOMEM;

    case ERROR_DISK_FULL:
      return errno = ENOSPC;

    case ERROR_DIR_NOT_EMPTY:
      return errno = ENOTEMPTY;

    case ERROR_BROKEN_PIPE:
      return errno = EPIPE;

    case ERROR_NOT_SAME_DEVICE:
      return errno = EXDEV;
    }
  return errno = EINVAL;
}

#endif	/* _WIN32 */
