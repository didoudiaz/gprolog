/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : configuration and system                                        *
 * File  : arch_dep.c                                                      *
 * Descr.: architecture dependent features (provide unix fct)              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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
#include <errno.h>

#include "gp_config.h"

#ifdef _WIN32
#include <windows.h>
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


/* see: https://doxygen.postgresql.org/dirent_8c_source.html */

/*-------------------------------------------------------------------------*
 * OPENDIR                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
DIR *
opendir(char *dir_path)
{
  DWORD att;
  DIR *dir;
  
  att = GetFileAttributes(dir_path);
  if (att == INVALID_FILE_ATTRIBUTES) /* check exists and is a directory */
    {
      errno = ENOENT;
      return NULL;
    }
  if ((att & FILE_ATTRIBUTE_DIRECTORY) != FILE_ATTRIBUTE_DIRECTORY)
    {
      errno = ENOTDIR;
      return NULL;
    }
  
  dir = malloc(sizeof(DIR));
  if (!dir)
    {
      errno = ENOMEM;
      return NULL;
    }

  dir->dirname = malloc(strlen(dir_path) + 4);
  if (!dir->dirname)
    {
      errno = ENOMEM;
      free(dir);
      return NULL;
    }
  strcpy(dir->dirname, dir_path);

  if (dir->dirname[strlen(dir->dirname) - 1] != '/' &&
      dir->dirname[strlen(dir->dirname) - 1] != '\\')
    strcat(dir->dirname, "\\");   /* Append backslash if not already there */
  strcat(dir->dirname, "*");    /* Search for entries named anything */

  dir->handle = INVALID_HANDLE_VALUE; /* to allow to detect first readdir */
  dir->ret.d_ino = 0;           /* what else ? */
  dir->ret.d_reclen = 0;        /* what else ? */
  dir->ret.d_type = DT_UNKNOWN;
  
  return dir;
}
  



/*-------------------------------------------------------------------------*
 * READDIR                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
struct dirent *
readdir(DIR *dir)
{
  WIN32_FIND_DATA fd;
  
  if (dir->handle == INVALID_HANDLE_VALUE)
    {
      dir->handle = FindFirstFile(dir->dirname, &fd);
      if (dir->handle == INVALID_HANDLE_VALUE)
	{
	  if (GetLastError() != ERROR_FILE_NOT_FOUND) /* no more files: errno is unchanged */
	    Pl_Win_Error_To_Errno();
	  return NULL;
	}
    }
  else
    {
      if (!FindNextFile(dir->handle, &fd))
	{
	  if (GetLastError() != ERROR_NO_MORE_FILES) /* errno unchanged */
	    Pl_Win_Error_To_Errno();
	  return NULL;
	}
    }

  strcpy(dir->ret.d_name, fd.cFileName);

  /* Detect file type (to be completed ?) */

  if ((fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0)
    dir->ret.d_type = DT_DIR;
  else if ((fd.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0 &&
	   (fd.dwReserved0 == IO_REPARSE_TAG_MOUNT_POINT))   /* dwReserved0 contains ReparseTag */
    dir->ret.d_type = DT_LNK;
  else
    dir->ret.d_type = DT_REG;	/* default case */
  
  return &dir->ret;
}




/*-------------------------------------------------------------------------*
 * CLOSEDIR                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
closedir(DIR *dir)
{
  int err = 0;
  if (dir->handle != INVALID_HANDLE_VALUE)
    {
      FindClose(dir->handle);
      err = Pl_Win_Error_To_Errno();
    }
  free(dir->dirname);
  free(dir);

  errno = err;
  return (err == 0) ? 0 : -1;
}
#endif
