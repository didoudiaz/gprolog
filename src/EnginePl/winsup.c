/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : winsup.c                                                        *
 * Descr.: Unix compatibility replacements for Windows                     *
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

#if defined(_WIN32) && !defined(__CYGWIN__)

#include "gp_config.h"

#include <stdio.h>
#include <errno.h>
#include <windows.h>

#include "bool.h"
#include "winsup.h"
#include "os_error.h"


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


/*
 * Windows simple realpath() implementation.
 *
 * Best-effort algorithm:
 *  - Obtain an absolute path (GetFullPathName).
 *  - Try full dereference by opening a handle and calling
 *    GetFinalPathNameByHandle (follows symlinks and junctions).
 *  - If opening fails due to access or sharing restrictions, read reparse data
 *    (FSCTL_GET_REPARSE_POINT) and continue with the link target.
 *  - Loop with a bounded depth to avoid infinite cycles (can occur even if
 *    it seems with a ReparseTag not handled here).
 *
 * Paths come in several forms:
 *  - Win32 user paths: C:\x or \\server\share
 *    (parsed by Win32 APIs and historically limited to MAX_PATH (260).
 *  - Win32 extended-length paths: \\?\...
 *    (introduced to bypass Win32 parsing and length limitations).
 *  - NT kernel paths: internally used by the kernel (\??\...).
 *
 * Win32 APIs accept user paths and extended-length paths, but not NT kernel paths.
 * Some interfaces (e.g. FSCTL_GET_REPARSE_POINT) may return NT-style paths.
 * Therefore paths must be normalized during resolution (NT -> Win32-extended).
 * At the end, NT and extended prefixes are stripped for user-visible output.
 *
 * Prefix mapping table:
 *
 * Path form              Drive path   UNC path
 * -----------------------------------------------
 * NT path                \??\C:\x     \??\UNC\a\b
 * Win32 extended-length  \\?\C:\x     \\?\UNC\a\b
 * User-visible           C:\x         \\a\b
 *
 * Limits:
 *  - Windows-native only: no MSYS2/Cygwin POSIX mounts.
 *  - Handles only SYMLINK and MOUNT_POINT reparse tags.
 *  - Access-denied fallback may stop before full canonicalization.
 */


#ifndef MAXIMUM_REPARSE_DATA_BUFFER_SIZE
#define MAXIMUM_REPARSE_DATA_BUFFER_SIZE (16 * 1024) /* from winnt.h */
#endif

#ifndef _REPARSE_DATA_BUFFER_DEFINED
#define _REPARSE_DATA_BUFFER_DEFINED
typedef struct _REPARSE_DATA_BUFFER {
  ULONG  ReparseTag;
  USHORT ReparseDataLength;
  USHORT Reserved;
  union {
    struct {
      USHORT SubstituteNameOffset
      ;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      ULONG  Flags;
      WCHAR  PathBuffer[1];
    } SymbolicLinkReparseBuffer;
    struct {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      WCHAR  PathBuffer[1];
    } MountPointReparseBuffer;
    struct { UCHAR DataBuffer[1]; } GenericReparseBuffer;
  };
} REPARSE_DATA_BUFFER;
#endif




static Bool Resolve_By_Handle(const char *path, char *resolved_path);
static int Resolve_By_Reparse(const char *path, char *resolved_path);
static void Make_Win32_Extended_Path(char *path);
static void Make_User_Path(char *path);
static Bool Is_Absolute_Path(const char *path);


/*-------------------------------------------------------------------------*
 * REALPATH                                                                *
 *                                                                         *
 * Limited POSIX realpath(3) support. Expands symbolic links and resolves  *
 * /./, /../ and extra '/' characters in path to produce an absolute path. *
 * The resulting path is stored in the buffer resolved_path.               *
 * If resolved_path is NULL, memory is allocated using malloc().           *
 *                                                                         *
 * Due to limitations (e.g. only Windows symlinks and junctions are        *
 * handled), the resulting path may not be fully canonical.                *
 *                                                                         *
 * Returns a pointer to the resolved path.                                 *
 * A result is always returned on a best-effort basis.                     *
 *-------------------------------------------------------------------------*/
char *
realpath(const char *path, char *resolved_path)
{
  static char buff1[MAXPATHLEN];
  static char buff2[MAXPATHLEN];
  char *src = buff1;
  char *dst = buff2;
  int i, ret;
  
#define SWAP_SRC_DST { char *tmp = src; src = dst; dst = tmp; }

#define MAX_TRY 64

  strcpy(src, path);
  for (i = 0; i < MAX_TRY; i++)
    {
      ret = GetFullPathNameA(src, MAXPATHLEN, dst, NULL);
      SWAP_SRC_DST;
      if (ret == 0)
	break;
#ifdef DEBUG
      if (strcmp(src, dst))
	printf("absolute path: %s = %s\n", dst, src);
#endif
      /* first try deref to a final path via the handle */
      if (Resolve_By_Handle(src, dst))
	break;

      /* try to keep going by reading reparse data */
      ret = Resolve_By_Reparse(src, dst);
      SWAP_SRC_DST;
      if (ret <= 0) {
	break;
      }
#ifdef DEBUG
      if (strcmp(src, dst))
	printf("%s: %s ---> %s\n", (ret == 1) ? "symlink" : "junction", dst, src);
#endif
      if (!Is_Absolute_Path(src))
	{			/* reuse link source dirname */
	  SWAP_SRC_DST;
	  sprintf(src + strlen(src), "\\..\\%s", dst);
	  printf("link is relative append: %s\n", src);
	}
    }

  Make_User_Path(dst);
  
#ifdef DEBUG
  if (strcmp(src, dst))
    printf("final: %s = %s\n", src, dst);
#endif

  if (resolved_path == NULL)
    resolved_path = strdup(dst);
  else
    strcpy(resolved_path, dst);
  
  return resolved_path;
}


/*-------------------------------------------------------------------------*
 * RESOLVE_BY_HANDLE                                                       *
 *                                                                         *
 * Attempt full path resolution by opening the path and using the handle.  *
 * Follows Windows symbolic links and junctions to obtain the final target.*
 * On success we have a canonalized path (on failure, we will try to read  *
 * the reparse target).                                                    *
 *                                                                         *
 * Returns TRUE on success                                                 *
 *-------------------------------------------------------------------------*/
static Bool
Resolve_By_Handle(const char *path, char *resolved_path)
{
  HANDLE h;
  DWORD n;
  
  h = CreateFileA(path,
		  0,		/* no specific access needed */
		  FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		  NULL,
		  OPEN_EXISTING,
		  FILE_FLAG_BACKUP_SEMANTICS, /* follow links */
		  NULL);
  if (h == INVALID_HANDLE_VALUE)
    return FALSE;

  n = GetFinalPathNameByHandleA(h, resolved_path, MAXPATHLEN, FILE_NAME_NORMALIZED);
  CloseHandle(h);

  if (n == 0)
    return FALSE;

  Make_Win32_Extended_Path(resolved_path);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * RESOLVE_BY_REPARSE                                                      *
 *                                                                         *
 * Read reparse-point metadata and extract the link target without         *
 * dereferencing it (opens with FILE_FLAG_OPEN_REPARSE_POINT and uses      *
 * FSCTL_GET_REPARSE_POINT). This is used as a fallback when full handle   *
 * resolution cannot be performed.                                         *
 *                                                                         *
 * Returns:                                                                *
 *   0  not an handled reparse point (no progress)                         *
 *   1  symlink target extracted (IO_REPARSE_TAG_SYMLINK)                  *
 *   2  junction/mount-point target extracted (IO_REPARSE_TAG_MOUNT_POINT) *
 *  -1  error (e.g. cannot query attributes, open, or read reparse data)   *
 *                                                                         *
 * In all cases, resolved_path is filled with the best path available:     *
 * input path on no-progress/error, or extracted target on success.        *
 *                                                                         *
 * Handles only symlinks and junctions. Other reparse tags are ignored     *
 * (e.g. IO_REPARSE_TAG_APPEXECLINK, IO_REPARSE_TAG_LX_SYMLINK,            *
 * IO_REPARSE_TAG_WOF, ...).                                               *
 *-------------------------------------------------------------------------*/
static int
Resolve_By_Reparse(const char *path, char *resolved_path)
{
  DWORD attr;
  HANDLE h;
  static BYTE raw_buff[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
  REPARSE_DATA_BUFFER *r = (REPARSE_DATA_BUFFER *) raw_buff;
  BOOL ok;
  USHORT off, len;
  WCHAR *base;
  int n, ret;

  strcpy(resolved_path, path);

  attr = GetFileAttributesA(path);
  if (attr == INVALID_FILE_ATTRIBUTES)
    return -1;
  
  if ((attr & FILE_ATTRIBUTE_REPARSE_POINT) == 0)
    return 0;

  /* open with FILE_FLAG_OPEN_REPARSE_POINT: do not follow links */
  h = CreateFileA(path,
		  FILE_READ_ATTRIBUTES,
		  FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		  NULL,
		  OPEN_EXISTING,
		  FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
		  NULL);
  if (h == INVALID_HANDLE_VALUE)
    return -1;

  ok = DeviceIoControl(h, FSCTL_GET_REPARSE_POINT, NULL, 0,
		       raw_buff, (DWORD) sizeof(raw_buff), NULL, NULL);
  CloseHandle(h);
  if (!ok)
    return -1;

  if (r->ReparseTag == IO_REPARSE_TAG_SYMLINK)
    {
      off = r->SymbolicLinkReparseBuffer.SubstituteNameOffset;
      len = r->SymbolicLinkReparseBuffer.SubstituteNameLength;
      base = r->SymbolicLinkReparseBuffer.PathBuffer;
      ret = 1;			/* symlink */
    }
  else if (r->ReparseTag == IO_REPARSE_TAG_MOUNT_POINT)
    {
      off = r->MountPointReparseBuffer.SubstituteNameOffset;
      len = r->MountPointReparseBuffer.SubstituteNameLength;
      base = r->MountPointReparseBuffer.PathBuffer;
      ret = 2;			/* junction */
    }
  else	 /* others links not handled : stop (not fully canonalized) */
    return 0;

  /* convert off (len) to wide characters offset (count) */
  off /= sizeof(WCHAR);
  len /= sizeof(WCHAR);
  base += off;			/* base is a WCHAR ptr */

  n = WideCharToMultiByte(CP_ACP, 0, base, len,
			  resolved_path, MAXPATHLEN, NULL, NULL);
  if (n <= 0)
    return -1;

  resolved_path[n] = '\0';
  Make_Win32_Extended_Path(resolved_path);
  return ret;
}




/*-------------------------------------------------------------------------*
 * IS_ABSOLUTE_PATH                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Is_Absolute_Path(const char *path)
{
#define ISALPHA(c) ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))

  return (*path == '\\' || (ISALPHA(path[0]) && path[1] == ':'));
}




/*-------------------------------------------------------------------------*
 * MAKE_WIN32_EXTENDED_PATH                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Make_Win32_Extended_Path(char *path)
{
  /* \??\X:\...  -> \\?\X:\... and \??\UNC\... -> \\?\UNC\... */
  if (strncmp(path, "\\??\\", 4) == 0)
    path[1] = '\\';
}




/*-------------------------------------------------------------------------*
 * MAKE_USER_PATH                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Make_User_Path(char *path)
{
  if (strncmp(path, "\\\\?\\UNC\\", 8) == 0)
    {	/* \??\UNC\... -> \\?\UNC\... */
      memmove(path, path + 6, strlen(path + 6) + 1);
      path[0] = '\\'; 
    }
  else if (strncmp(path, "\\??\\", 4) == 0 || strncmp(path, "\\\\?\\", 4) == 0)
    {	/* remove \\?\ and \??\ */
      memmove(path, path + 4, strlen(path + 4) + 1);
    }
}



#ifdef USE_ALONE
/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  printf("Executing realpath(%s)...\n", argv[1]);
  printf("real path: %s\n", realpath(argv[1], NULL));
  return 0;
}
#endif

#endif	/*_WIN32 && !__CYGWIN__ */
