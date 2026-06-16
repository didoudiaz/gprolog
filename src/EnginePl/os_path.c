/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : os_path.c                                                       *
 * Descr.: cross-platform pathname utilities                               *
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
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>
#include <io.h>
#define READ_REGISTRY_ONLY
#include "win_registry.h"
#endif


#if defined(__unix__) || defined(__CYGWIN__)
#include <strings.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>
#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif
#else /* _WIN32 */
#include <process.h>
#include <direct.h>
#endif

#include "pl_long.h"
#include "bool.h"
#include "winsup.h"
#define OS_PATH_FILE
#include "os_path.h"

#if 0
#define DEBUG
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

#if defined(_WIN32) || defined(__CYGWIN__)
static char *Msys_Root_Prefix(char *src);
#endif


static Bool Is_Executable(const char *path);



static char *Get_Prolog_Path_From_Exec(char *path, Bool *devel_mode);
static Bool Is_A_Valid_Root(char *str, Bool *devel_mode);

#define COMPILER_EXE  GPLC EXE_SUFFIX




#define Has_Drive_Specif(str) \
  (((*(str) >= 'a' && *(str) <= 'z') || (*(str) >= 'A' && *(str) <= 'Z')) && (str)[1] == ':')




/*-------------------------------------------------------------------------*
 * Pl_GET_PROLOG_PATH                                                      *
 *                                                                         *
 * Returns the GNU Prolog start path (or NULL) and detects devel_mode.     *
 * The returned buffer can be written.                                     *
 *-------------------------------------------------------------------------*/
char *
Pl_Get_Prolog_Path(char *argv0, Bool *devel_mode)
{
  static char *prolog_path_cache = NULL;
  static Bool devel_mode_cache = FALSE;
  static char resolved[MAXPATHLEN];
  char *p;

  if (prolog_path_cache != NULL)
    {
      *devel_mode = devel_mode_cache;
      strcpy(resolved, prolog_path_cache);
      return resolved;
    }

  if ((p = getenv(ENV_VARIABLE)) != NULL)
    {
      strcpy(resolved, p);
#ifdef DEBUG
      fprintf(stderr, "1- Prolog path from " ENV_VARIABLE ": %s\n", resolved);
#endif
      p = resolved;
      if (Is_A_Valid_Root(resolved, devel_mode))
        goto ok;
    }

  if (argv0 != NULL &&
      (p = Get_Prolog_Path_From_Exec(argv0, devel_mode)) != NULL)
    {
#ifdef DEBUG
      fprintf(stderr, "2- Prolog path from argv[0]: %s\n", p);
#endif
      goto ok;
    }

  if ((p = Pl_Search_Path(COMPILER_EXE)) == NULL)
    {
#if defined(_WIN32) || defined(__CYGWIN__)
      if (Read_Windows_Registry("RootPath", REG_SZ, resolved, sizeof(resolved)) &&
	  *resolved)
        {
#ifdef DEBUG
          fprintf(stderr, "3- Prolog path from Registry: %s\n", resolved);
#endif
	  p = resolved;
          if (Is_A_Valid_Root(resolved, devel_mode))
            goto ok;
        }
#endif
      return NULL;
    }

#ifdef DEBUG
  fprintf(stderr, "4- " COMPILER_EXE " found from PATH: %s\n", p);
#endif

  if ((p = Get_Prolog_Path_From_Exec(p, devel_mode)) == NULL)
    return NULL;

#ifdef DEBUG
  fprintf(stderr, "5- Prolog path from " COMPILER_EXE " executable: %s\n", p);
#endif

 ok:
  strcpy(resolved, Pl_Absolute_Path_Name(p));
  devel_mode_cache = *devel_mode;
  prolog_path_cache = strdup(resolved);

#if defined(__CYGWIN__)
  /*
   * Under Windows, CreateProcess searches first in the directory from
   * which the application was loaded. This is convenient when
   * launching gprolog.exe by double-clicking (so Pl_Spawn_Redirect
   * can find pl2wam). However, under Cygwin, execvp only searches
   * the directories listed in PATH. We thus prepend pl_home/bin to PATH
   * (which must be converted to a Cygwin (POSIX) path beforehand).
   */
  if (!*devel_mode)
    {
      char *path_env;
      size_t len_cyg_resolv, len;
      char *new_path_env;

      path_env = getenv("PATH");
      if (path_env == NULL)
	path_env = "";

      len_cyg_resolv = cygwin_conv_path(CCP_WIN_A_TO_POSIX, resolved, NULL, 0) - 1;
      if (len_cyg_resolv > 0)
	{
	  len = strlen(path_env) + len_cyg_resolv + 16;
	  new_path_env = malloc(len);

	  strcpy(new_path_env, "PATH=");
	  cygwin_conv_path(CCP_WIN_A_TO_POSIX, resolved, new_path_env + 5, len - 5);
	  sprintf(new_path_env + 5 + len_cyg_resolv, "/bin:%s", path_env);
	  putenv(new_path_env);
	  /* do not free(new_path_env) - new_path_env is now part of the environment */
	}
    }
#endif

  return resolved;
}




/*-------------------------------------------------------------------------*
 * GET_PROLOG_PATH_FROM_EXEC                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Get_Prolog_Path_From_Exec(char *path, Bool *devel_mode)
{
  static char resolved[MAXPATHLEN];
  char *p;

  if (*path == '\0' || realpath(path, resolved) == NULL)
    return NULL;

#ifdef DEBUG
  fprintf(stderr, "Exec link resolution: %s\n", resolved);
#endif

  p = resolved + strlen(resolved) - 1;
  while (p > resolved && !Is_Dir_Sep(*p)) /* skip exec_name */
    p--;

  if (p == resolved)
    return NULL;

  while (p > resolved && Is_Dir_Sep(*p)) /* skip / */
    p--;

  if (p == resolved)
    return NULL;

  while (p > resolved && !Is_Dir_Sep(*p)) /* skip previous dir name */
    p--;

  if (p == resolved)
    return NULL;

  p[1] = '\0';

#ifdef DEBUG
  fprintf(stderr, "Prolog path from resolution: %s (to be checked)\n", resolved);
#endif

  if (Is_A_Valid_Root(resolved, devel_mode))
    return resolved;
  return NULL;
}



/*-------------------------------------------------------------------------*
 * IS_A_VALID_ROOT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Is_A_Valid_Root(char *path, Bool *devel_mode)
{
  char *p;

#ifdef DEBUG
  fprintf(stderr, "Testing if valid root: %s\n", path);
#endif

  p = path + strlen(path) - 1;

  while (p >= path && Is_Dir_Sep(*p))
    p--;

  p++;
  if (p == path)
    goto invalid;

  strcpy(p, DIR_SEP_S "bin" DIR_SEP_S COMPILER_EXE);
  if (Is_Executable(path))
    {
      *p = '\0';
      *devel_mode = FALSE;
    valid:
#ifdef DEBUG
      fprintf(stderr, "valid root: %s\n", path);
#endif
      return TRUE;
    }

  /* for development mode, only test the existence of TopComp directory */

  strcpy(p, DIR_SEP_S "TopComp");
  if (access(path, F_OK) == 0)
    {
      *p = '\0';
      *devel_mode = TRUE;
#ifdef DEBUG
      fprintf(stderr, "development mode detected\n");
#endif
      goto valid;
    }

 invalid:
#ifdef DEBUG
  *p = '\0';
  fprintf(stderr, "invalid root: %s (not found: bin/" COMPILER_EXE
	  " nor TopComp)\n", path);
#endif
  return FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_SEARCH_PATH                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Search_Path(char *exe_file)
{
#if defined(__unix__) || defined(__CYGWIN__)

  char *path = getenv("PATH");
  char *p;
  int l;
  static char buff[MAXPATHLEN];

  if (path == NULL)
    return NULL;

  p = path;
  for (;;)
    {
      if ((p = strchr(path, ':')) != NULL)
        {
          l = p - path;
          strncpy(buff, path, l);
        }
      else
        {
          strcpy(buff, path);
          l = strlen(buff);
        }

      buff[l++] = DIR_SEP_C;

      strcpy(buff + l, exe_file);

      if (Is_Executable(buff))
        return buff;

      if (p == NULL)
        break;

      path = p + 1;
    }

  return NULL;

#else

  static char buff[MAXPATHLEN];
  char *file_part;

  if (SearchPath(NULL, exe_file, ".exe", MAXPATHLEN, buff, &file_part) == 0)
    return NULL;

  return buff;

#endif
}




/*-------------------------------------------------------------------------*
 * IS_EXECUTABLE                                                           *
 *                                                                         *
 * Special case of Pl_Access(..., X_OK)                                    *
 *-------------------------------------------------------------------------*/
static Bool
Is_Executable(const char *path)
{
#ifdef _WIN32
  int mode = F_OK;  /* simply test existence (we know it is a .exe) */
#else
  int mode = X_OK;
#endif
  return (access(path, mode) == 0);
}  




/*-------------------------------------------------------------------------*
 * PL_GET_WORKING_DIR                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Get_Working_Dir(void)
{
  static char cur_work_dir[MAXPATHLEN];

  if (getcwd(cur_work_dir, sizeof(cur_work_dir) - 1) == NULL)
    strcpy(cur_work_dir, ".");
  return cur_work_dir;
}




/*-------------------------------------------------------------------------*
 * PL_SET_WORKING_DIR                                                      *
 *                                                                         *
 * must preserve errno if fails (used in os_interf_c.c)                    *
 *-------------------------------------------------------------------------*/
Bool
Pl_Set_Working_Dir(char *path)
{
  char *new_path = Pl_Absolute_Path_Name(path);

  return (new_path != NULL && chdir(new_path) == 0);
}




/*-------------------------------------------------------------------------*
 * PL_ABSOLUTE_PATH_NAME0                                                  *
 *                                                                         *
 * del_trail_slash: TRUE: remove ending / (usually wanted behavior)        *
 *                  FALSE: keep it (we can see the user wanted a directory)*
 * returns an absolute file name (in a static buffer: buff1 or buff2).     *
 *-------------------------------------------------------------------------*/
#ifdef __GNUC__
#pragma GCC diagnostic push
#if 0 /* was ifdef __clang__ reactivate if needed */
#pragma GCC diagnostic ignored "-Wunknown-warning-option"
#endif
#pragma GCC diagnostic ignored "-Wformat-overflow"
#endif
char *
Pl_Absolute_Path_Name0(char *path, Bool del_trail_slash)
{
  static char buff1[MAXPATHLEN];
  static char buff2[MAXPATHLEN];
  char *src = path;
  char *dst;
  char *p, *q;
  char c;
  Bool add_slash = FALSE;

#define SWAP_SRC_DST { char *tmp = src; src = dst; dst = tmp; }

  /* --- Expand $VARNAME (and %VARNAME% under Windows) --- */
  dst = buff1;
  while ((*dst++ = *src))
    {
      c = *src++;
      if (c == '$'
#if defined(_WIN32) || defined(__CYGWIN__)
          || c == '%'
#endif
	  )
        {
          p = dst;
          while (isalnum(*src) || *src == '_')
            *dst++ = *src++;
#if defined(_WIN32) || defined(__CYGWIN__)
          if (c == '%' && *src != '%')
            continue;
#endif
          *dst = '\0';
          q = getenv(p);
          if (q)
            {
              p--;
              strcpy(p, q);
              dst = p + strlen(p);
#if defined(_WIN32) || defined(__CYGWIN__)
              if (c == '%')
                src++;
#endif
            }
#if defined(_WIN32) || defined(__CYGWIN__)
          else if (c == '%')
            *dst++ = *src++;
#endif
        }
    }
  *dst = '\0';

  src = buff1;
  dst = buff2;

  /* --- Expand ~/ (with HOME) and ~user/ (under Unix) --- */
  if (src[0] == '~')
    {
      if (Is_Dir_Sep(src[1]) || src[1] == '\0') /* ~/... cf $HOME */
        {
	  q = "";
          if ((p = getenv("HOME")) == NULL)
	    {
#if defined(_WIN32) || defined(__CYGWIN__)
	      if ((p = getenv("USERPROFILE")) == NULL)
		{
		  if ((p = getenv("HOMEPATH")) == NULL ||
		      (q = getenv("HOMEDRIVE")) == NULL)
		    return NULL;
		}
#else
	      return NULL;
#endif
	    }
          sprintf(dst, "%s%s/%s", q, p, src + 1);
	  SWAP_SRC_DST;
        }
#if defined(__unix__) || defined(__CYGWIN__)
      else                      /* ~user/... read passwd */
        {
          struct passwd *pw;

          p = src + 1;
          while (*p && !Is_Dir_Sep(*p))
            p++;

          src[0] = *p;
          *p = '\0';
          if ((pw = getpwnam(src + 1)) == NULL)
            return NULL;

          *p = src[0];

          sprintf(dst, "%s/%s", pw->pw_dir, p);
	  SWAP_SRC_DST;
        }
#endif
    }

  /* --- Do not change Prolog special file 'user' --- */
  if (strcmp(src, "user") == 0)
    return src;

  if (*src == '\0')
    return NULL;
  
  add_slash = (!del_trail_slash && Pl_Path_Ends_With_Dir(src));

#if defined(_WIN32) || defined(__CYGWIN__)

  /* --- Windows : Expand msys root PATH --- */
  
  p = Msys_Root_Prefix(src);
  if (p != NULL && *p != '\0')
    {
      sprintf(dst, "%s%s", p, src);
      SWAP_SRC_DST;
    }

  /* --- Get Windows absolute path --- */
  /* Under native msys we do not use cygwin_conv_path since it only works in
   * one side: posix -> windows or windows -> posix while the received path
   * can be of any form. We use the windows API (provided by kernel32 lib)
   */
  if (GetFullPathNameA(src, MAXPATHLEN, dst, NULL) == 0)
    return NULL;
  
  /* ---  Convert \ to / --- */
  for (p = dst; *p; p++)
    if (*p == '\\')
      *p = '/';

#else /* __unix__ */

  /* --- Unix : Convert relative to absolute --- */

  if (src[0] != '/')      /* add current directory */
    {
      sprintf(dst, "%s/%.*s", Pl_Get_Working_Dir(),
	      MAXPATHLEN, src); /* MAXPATHLEN is to avoid gcc warning */
      SWAP_SRC_DST;
    }

  /* --- Clean (remove ./ ../  //) --- */
  p = dst;
  while ((*p++ = *src))
    {
      if (*src++ != '/')
        continue;

    collapse:
      while (*src == '/')       /* collapse /////... as / */
        src++;

      if (*src != '.')
        continue;

      if (src[1] == '/' || src[1] == '\0')      /* /./ or final /. becomes / */
        {
          src++;
          goto collapse;
        }

      if (src[1] != '.' || (src[2] != '/' && src[2] != '\0'))
        continue;
      /* case /../ */
      src += 2;
      p -= 2;
      while (p >= dst && *p != '/')
        p--;

      if (p < dst)
        return NULL;
    }

  p--;                        /* p points the \0 */
#endif	/* __unix__ */

  /* --- Ensure a minimal prefix and maybe add_slash --- */

  /* here the result is in dst and p points the \0 */

#if defined(_WIN32) || defined(__CYGWIN__)
#define MIN_PREFIX 3            /* Windows minimal path c:\  */
#else
#define MIN_PREFIX 1            /* Unix minimal path /    */
#endif

  if (p - dst < MIN_PREFIX) /* all removed, e.g. with /src/../ then add / */
    strcpy(p, "/");
  else
    {
      if (p - dst > MIN_PREFIX && Is_Dir_Sep(p[-1]))
	*--p = '\0';		/* remove last / or \ */

      if (add_slash)	    	/* and maybe (re)add a last / if needed */
	strcpy(p, "/");
    }

#if 0
  printf("FINAL: %s\n", dst);
#endif
  return dst;
}




#if defined(_WIN32) || defined(__CYGWIN__)

/*-------------------------------------------------------------------------*
 * MSYS_ROOT_PREFIX                                                        *
 *                                                                         *
 * Detects a msys-rooted path (e.g. /home/foo) and returns the msys root   *
 * path (e.g. c:/msys64/) to add before src (or NULL or "").               *
 *                                                                         *
 * Also modifies src to deal with /c/foo which is replaced by c:/foo       *
 *                                                                         *
 * Under pure msys (__CYGWIN__) we use cygwin_conv_path to obtain the root.*
 * However, when compiled under msys native environments (mingw32, mingw64,*
 * ucrt64, ...) cygwin_conv_path is not available. We thus try to execute  *
 * the (/usr/bin/)cygpath utility to obtain the prefix (e.g. c:/msys64).   *
 * For efficiency, we only call cygpath once.                              *
 *-------------------------------------------------------------------------*/
static char *
Msys_Root_Prefix(char *src)
{
  static char *msys_root_path = NULL;
  /* msys recognized entries: if ends with slash full match else prefix match */
  static char *msys_root_entry[] = 
    { "bin/", "dev/", "etc/", "home/", "opt/", "proc/", "tmp/", "usr/", "var/",
      "msys", "mingw32", "mingw64", "ucrt64", "clang64", "clangarm64", NULL };
  char **entry;
  char *p, *q;
  char buff[MAXPATHLEN];

  /* --- Replace /c/ by c:/ (Windows drive letter) */
  if (*src == '/' && isalpha(src[1]) && src[2] == '/')
    {
      src[0] = toupper(src[1]);
      src[1] = ':';
      return NULL;
    }

  if (*src != '/' || (msys_root_path != NULL && *msys_root_path == '\0'))
    return NULL;

  for(entry = msys_root_entry; *entry; entry++)
    {
      for(p = *entry, q = src + 1;*p && *p == *q; p++, q++)
	{
	}
      
      if (*p == '\0' || (*q == '\0' && *p == '/'))
	break;			/* matching entry found */
    }

  if (*entry == NULL)		/* no entry found */
    return NULL;

  if (msys_root_path == NULL)
    {
#ifdef __CYGWIN__
      cygwin_conv_path(CCP_POSIX_TO_WIN_A, "/", buff, MAXPATHLEN);
      msys_root_path = strdup(buff);
#else
      FILE *f;
      msys_root_path = "";
      /* Use popen instead of Pl_Spawn_Redirect to avoid dependency */
      if (Pl_Search_Path("cygpath.exe") != NULL &&
	  (f = popen("cygpath -m /", "r")) != NULL)
	{
	  int c;
	  p = buff;
	  while((c = fgetc(f)) != EOF && c >= ' ')
	    *p++ = c;
	  if (p > buff && p[-1] == '/')
	    p--;
	  *p = '\0';
	  fclose(f);
	  msys_root_path = strdup(buff);
	}	
#endif
#ifdef DEBUG
      DBGPRINTF("msys root path: %s\n", msys_root_path);
#endif
    }

  return msys_root_path;
}
#endif

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif




/*-------------------------------------------------------------------------*
 * PL_IS_ABSOLUTE_FILE_NAME                                                *
 *                                                                         *
 * Test if a path name is absolute (i.e. not relative).                    *
 *-------------------------------------------------------------------------*/
Bool
Pl_Is_Absolute_File_Name(char *path)
{
  if (Is_Dir_Sep(*path))
    return TRUE;

  /*
   * Windows: path starting with a drive specif is considered as absolute
   * (even if not followed by an antislash, e.g. c:foo is absolute).
   * Indeed, for a relative path, it is always 
   * possible to add before it the current working directory and it is not 
   * possible before a drive specif.
   * This is the behavior of Win32 PathIsRelative() function.
   * (to use it #include <shlwapi.h> and link with shlwapi.dll
   */
#if defined(_WIN32) || defined(__CYGWIN__)

  if (Has_Drive_Specif(path))
    return TRUE;

#endif

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_DECOMPOSE_FILE_NAME                                                  *
 *                                                                         *
 * Decompose a path name into the dir, base and suffix (extension).        *
 *                                                                         *
 * path:   the path to decompose                                           *
 * del_trail_slashes: see below                                            *
 * base:   points the buffer which will receive the basename               *
 *         (or "" if none. It includes the suffix                          *
 * suffix: points inside base to the suffix part                           *
 *         (or "" if none, i.e. at the end of base)                        *
 * returns the dirname part (or "" if none)                                *
 *                                                                         *
 * Returned pointers are on 2 static buffers (dir and base) which can be   *
 * written.                                                                *
 *                                                                         *
 * del_trail_slashes: delete trailing slashes from dir ?                   *
 *  FALSE: nothing is done (path is simply split into dir and base).       *
 *         Concatenating dir and base yields the complete pathname.        *
 *  TRUE:  trailing slashes of the dir part are removed (similarly to      *
 *         dirname(3) except that initial trailing slashes of path are not *
 *         removed). If the dir part is empty then "." is returned.        *
 *         Concatenating dir "/" and base yields a complete pathname.      *
 *                                                                         *
 * To remove the extension from base simply do *suffix = '\0'              *
 * To add/change the suffix simply do strcpy(suffix, ".txt");              *
 *-------------------------------------------------------------------------*/
char *
Pl_Decompose_File_Name(char *path, Bool del_trail_slashes, char **base, char **suffix)
{
  static char buff_dir[MAXPATHLEN];
  static char buff_base[MAXPATHLEN];
  size_t dir_start_pos = 0;	/* on _WIN32 maybe there is a drive specif */

#if 0 && defined(_WIN32)	/* uncomment to explicitely use _splitpath() on Windows */

  char direct[_MAX_DIR];
  char ext[_MAX_EXT];
  
  _splitpath(path, buff_dir, direct, buff_base, ext); /* buff_dir contains the drive */
  dir_start_pos = strlen(buff_dir);
  strcat(buff_dir, direct);	/* concat the dirname */

  *suffix = buff_base + strlen(buff_base); /* buff_base contains the basename */
  strcpy(*suffix, ext);		     /* concat the suffix */
    
#else

  /* This version works for both Windows and Unix */

  char *p;

  strcpy(buff_dir, path);
#if defined(_WIN32) || defined(__CYGWIN__)
  if (Has_Drive_Specif(buff_dir))
    dir_start_pos = 2;
#endif

  Find_Last_Dir_Sep(p, buff_dir);

  p = (p == NULL) ? buff_dir + dir_start_pos : p + 1;

  strcpy(buff_base, p);
  *p = '\0';

  if ((p = strrchr(buff_base, '.')) != NULL)
    *suffix = p;      
  else
    *suffix = buff_base + strlen(buff_base); /* i.e. suffix = "" */

#endif

  if (del_trail_slashes)
    {
      if (buff_dir[dir_start_pos] == '\0') /* if dir is empty it becomes "." */
	strcat(buff_dir, ".");
      else
	{
	  size_t len = strlen(buff_dir); 	/* remove all trailing / */
	  while(--len >= dir_start_pos && Is_Dir_Sep(buff_dir[len]))
	    ;

	  if (len < dir_start_pos)		/* if all are / keep one */
	    len = dir_start_pos;
	  buff_dir[len + 1] = '\0';
	}
    }

#if 0			 /* uncomment to avoid extension with only one '.' */
  if ((*suffix)[0] == '.' && (*suffix)[1] == '\0') /* not really a suffix: undo it */
    (*suffix)++;		     /* points the \0 */
#endif

  *base = buff_base;

  return buff_dir;
}




/*-------------------------------------------------------------------------*
 * PL_PATH_ENDS_WITH_DIR                                                   *
 *                                                                         *
 * Return TRUE if ends with / or /. or /..                                 *
 *-------------------------------------------------------------------------*/
Bool
Pl_Path_Ends_With_Dir(char *path)
{
  size_t len = strlen(path);
  return ((len > 1 && Is_Dir_Sep(path[len - 1])) ||
	  (len > 2 && Is_Dir_Sep(path[len - 2]) && path[len - 1] == '.') ||
	  (len > 3 && Is_Dir_Sep(path[len - 3]) && path[len - 2] == '.' && path[len - 1] == '.'));
}




/*-------------------------------------------------------------------------*
 * PL_IS_DIR_NAME                                                          *
 *                                                                         *
 * Return TRUE or FALSE or -1 (errno) in case of error                     *
 *-------------------------------------------------------------------------*/
int
Pl_Is_Dir_Name(char *path, Bool inexistent_as_error)
{
  struct stat file_info;

  if (stat(path, &file_info) < 0)
    {
      if ((errno != ENOENT && errno != ENOTDIR) || inexistent_as_error)
	return -1;

      return Pl_Path_Ends_With_Dir(path); /* does not exist: directory is marked in path */
    }

  return S_ISDIR(file_info.st_mode);
}




#if defined(_WIN32) && !defined(__CYGWIN__)

/*-------------------------------------------------------------------------*
 * PL_ACCESS                                                               *
 *                                                                         *
 * Win32 replacement for access() since the X_OK test is no longer handled *
 * by _access() with ucrt (works with mscvrt). Workaround: for X_OK, we    *
 * check the existence and if the extension is handled by CreateProcess:   *
 * .exe | .bat | .cmd                                                      *
 * Return 0 on success, -1 on failure (as access()).                       *
 *-------------------------------------------------------------------------*/
int
Pl_Access(const char *path, int mode)
{
  const char *p;

  /* do not pass X_OK to _access() (ucrt rejects it) */
  if (access(path, mode & ~X_OK) != 0)
    return -1;

  if ((mode & X_OK) != 0)
    {
      p = path + strlen(path) - 4;
      if (p < path || (strcasecmp(p, ".exe") &&
		       strcasecmp(p, ".bat") &&
		       strcasecmp(p, ".cmd")))
	{
	  errno = EACCES;   /* not executable */
	  return -1;
	}
    }

  return 0;
}

#endif	/* _WIN32 */




/*-------------------------------------------------------------------------*
 * PL_MKTEMP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Mktemp(char *tmpl)
{
#if defined(__unix__) || defined(__CYGWIN__)
		/* redefined to avoid link warning (mktemp deprecated) */
		/* this code comes from glibc */
  struct timeval tv;
  int len;
  char *XXXXXX;
  static PlULong value;
  int count;
  struct stat buf;
  static const char letters[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

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

  /* Get some more or less random data.  */
  gettimeofday (&tv, NULL);
  value += ((PlULong) tv.tv_usec << 16) ^ tv.tv_sec ^ getpid ();
  value ^= (PlULong) 0x9e3779b1;  /* diffusion */


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
 * PL_TEMPNAM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Tempnam(char *dir, char *pfx)
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

  if ((d = getenv("TMPDIR")) != NULL && Dir_Exists(d))
    dir = d;
  else if (dir != NULL && Dir_Exists(dir))
    /* nothing */ ;
  else
    dir = NULL;

#ifdef __CYGWIN__	     /* try other windows standard temp dir */
  if ((d = getenv("TMP")) != NULL && Dir_Exists(d))
    dir = d;
  else if ((d = getenv("TEMP")) != NULL && Dir_Exists(d))
    dir = d;
#endif
  
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
  d = Pl_Mktemp(tmpl);
  if (d)
    d = strdup(d);
  return d;

#else

  errno = 0;
  if (dir == NULL && getenv("TMP") == NULL) /* Win32 _tempnam checks TMP */
    dir = "./";
  return tempnam(dir, pfx); /* returns a mallocated string */

#endif
}




