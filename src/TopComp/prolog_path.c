/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog Compiler                                                 *
 * File  : prolog_path.c                                                   *
 * Descr.: Prolog installation path detector                               *
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


#ifndef _PROLOG_PATH_C
#define _PROLOG_PATH_C

/* This file is included by top_comp.c, engine.c and w32_console.c */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>
#include <io.h>
#endif

#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#include <sys/param.h>
#endif

#include "../EnginePl/gp_config.h"

#if 0
#define DEBUG
#endif

static char *Get_Prolog_Path_From_Exec(char *str, int *devel_mode);
static char *Is_A_Valid_Root(char *str, int *devel_mode);
static char *Search_Path(char *file);

#define COMPILER_EXE  GPLC EXE_SUFFIX



#if defined(_WIN32) || defined(__CYGWIN__)

/*-------------------------------------------------------------------------*
 * READ_WINDOWS_REGISTRY                                                   *
 *                                                                         *
 * key_name : name of the key                                              *
 * key_type : type of the key (use Windows constant REG_SZ,...)            *
 * buff     : buffer to receive the value                                  *
 * buff_size: size of the buffer                                           *
 *                                                                         *
 * return: 1 in case of success (0 else)                                   *
 *-------------------------------------------------------------------------*/
static int
Read_Windows_Registry(char *key_name, DWORD key_type, void *buff, DWORD buff_size)
{
  DWORD dw_type;                                                \

#define INIT_REGISTRY_ACCESS                                    \
  HKEY hkey_software, hkey_prolog;                              \
  DWORD disp;                                                   \
  int r;                                                        \
                                                                \
  if (RegOpenKeyEx(HKEY_CURRENT_USER, "Software", 0,            \
                   KEY_QUERY_VALUE, &hkey_software) != 0)       \
    return 0;                                                   \
                                                                \
  if (RegCreateKeyEx(hkey_software, "GnuProlog", 0,             \
                     NULL, 0, KEY_ALL_ACCESS, NULL,             \
                     &hkey_prolog, &disp) != 0)                 \
    {                                                           \
      RegCloseKey(hkey_software);                               \
      return 0;                                                 \
    }

  INIT_REGISTRY_ACCESS;

  dw_type = key_type;           /* useless in fact: it is a output arg */
  memset(buff, 0, buff_size);

  r = RegQueryValueEx(hkey_prolog, key_name, 0, &dw_type, (LPBYTE) buff, &buff_size);

  if (r == ERROR_SUCCESS && dw_type != key_type)    /* not good key type: et an error */
    r++;

  RegCloseKey(hkey_prolog);
  RegCloseKey(hkey_software);

  return r == ERROR_SUCCESS;
}

#ifndef READ_REGISTRY_ONLY      /* to avoid gcc warning on unused static fct */
/*-------------------------------------------------------------------------*
 * WRITE_WINDOWS_REGISTRY                                                  *
 *                                                                         *
 * key_name : name of the key                                              *
 * key_type : type of the key (use Windows constant REG_SZ,...)            *
 * buff     : buffer containing the value                                  *
 * buff_size: size of the data to write                                    *
 *                                                                         *
 * return: 1 in case of success (0 else)                                   *
 *-------------------------------------------------------------------------*/
static int
Write_Windows_Registry(char *key_name, DWORD key_type, void *buff, DWORD buff_size)
{
  INIT_REGISTRY_ACCESS;

  r = RegSetValueEx(hkey_prolog, key_name, 0, key_type, (LPBYTE) buff, buff_size);

  RegCloseKey(hkey_prolog);
  RegCloseKey(hkey_software);

  return r == ERROR_SUCCESS;
}


/*-------------------------------------------------------------------------*
 * DELETE_WINDOWS_REGISTRY                                                 *
 *                                                                         *
 * key_name : name of the key                                              *
 *                                                                         *
 * return: 1 in case of success (0 else)                                   *
 *-------------------------------------------------------------------------*/
static int
Delete_Windows_Registry(char *key_name)
{
  INIT_REGISTRY_ACCESS;

  r = RegDeleteValue(hkey_prolog, key_name);

  RegCloseKey(hkey_prolog);
  RegCloseKey(hkey_software);

  return r == ERROR_SUCCESS;
}
#endif  /* !TOP_COMP_C */
#endif


/*-------------------------------------------------------------------------*
 * GET_PROLOG_PATH                                                         *
 *                                                                         *
 * Returns the GNU Prolog start path (or NULL) and if in devel_mode.       *
 * The returned buffer can be written.                                     *
 *-------------------------------------------------------------------------*/
static char *
Get_Prolog_Path(char *argv0, int *devel_mode)
{
  static char *prolog_path_cache = NULL;
  static int devel_mode_cache = 0;
  static char resolved[MAXPATHLEN];
  char *p;

  if (prolog_path_cache != NULL)
    {
      *devel_mode = devel_mode_cache;
      return prolog_path_cache;
    }

  if ((p = getenv(ENV_VARIABLE)) != NULL)
    {
      strcpy(resolved, p);
#ifdef DEBUG
      fprintf(stderr, "Prolog path from " ENV_VARIABLE ": %s\n", resolved);
#endif
      if ((p = Is_A_Valid_Root(resolved, devel_mode)) != NULL)
        goto ok;
    }

  if (argv0 != NULL && (p = Get_Prolog_Path_From_Exec(argv0, devel_mode)) != NULL)
    {
#ifdef DEBUG
      fprintf(stderr, "Prolog path from argv[0]: %s\n", p);
#endif
      goto ok;
    }


  if ((p = Search_Path(COMPILER_EXE)) == NULL)
    {
#if defined(_WIN32) || defined(__CYGWIN__)
      if (Read_Windows_Registry("RootPath", REG_SZ, resolved, sizeof(resolved)) && *resolved)
        {
#ifdef DEBUG
          fprintf(stderr, "Prolog path from Registry: %s\n", resolved);
#endif
          if ((p = Is_A_Valid_Root(resolved, devel_mode)) == NULL)
            goto ok;
        }
#endif
      return NULL;
    }

#ifdef DEBUG
  fprintf(stderr, GPLC " found from PATH: %s\n", p);
#endif

  if ((p = Get_Prolog_Path_From_Exec(p, devel_mode)) != NULL)
    {
    ok:
      devel_mode_cache = *devel_mode;
      prolog_path_cache = strdup(p);
      return p;
    }
  return NULL;
}




/*-------------------------------------------------------------------------*
 * GET_PROLOG_PATH_FROM_EXEC                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Get_Prolog_Path_From_Exec(char *str, int *devel_mode)
{
  static char resolved[MAXPATHLEN];
  char *p;

#if defined(__unix__) || defined(__CYGWIN__)
  if (realpath(str, resolved) == NULL)
    return NULL;
#else  /* realpath useless under Win32 since SearchPath resolves it */
  strcpy(resolved, str);
#endif

#ifdef DEBUG
  fprintf(stderr, "link resolution: %s\n", resolved);
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
  fprintf(stderr, "Prolog path from resolution: %s\n", resolved);
#endif

  return Is_A_Valid_Root(resolved, devel_mode);
}



/*-------------------------------------------------------------------------*
 * IS_A_VALID_ROOT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Is_A_Valid_Root(char *str, int *devel_mode)
{
  char *p;

#ifdef DEBUG
  fprintf(stderr, "test if valid root and detect devel mode: %s\n", str);
#endif

  p = str + strlen(str) - 1;

  while (p >= str && Is_Dir_Sep(*p))
    p--;

  if (p < str)
    goto invalid;

  p++;
  strcpy(p, DIR_SEP_S "bin" DIR_SEP_S COMPILER_EXE);
  if (access(str, X_OK) == 0)
    {
      *p = '\0';
      *devel_mode = 0;
    valid:
#ifdef DEBUG
      fprintf(stderr, "valid root: %s\n", str);
#endif
      return str;
    }

  /* for development mode, only test the existence of TopComp directory */

  strcpy(p, DIR_SEP_S "TopComp");
  if (access(str, F_OK) == 0)
    {
      *p = '\0';
      *devel_mode = 1;
#ifdef DEBUG
      fprintf(stderr, "development mode detected\n");
#endif
      goto valid;
    }

 invalid:
#ifdef DEBUG
  fprintf(stderr, "invalid root: %s\n", str);
#endif
  return NULL;
}




/*-------------------------------------------------------------------------*
 * SEARCH_PATH                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Search_Path(char *file)
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

      strcpy(buff + l, file);

      if (access(buff, X_OK) == 0)
        return buff;

      if (p == NULL)
        break;

      path = p + 1;
    }

  return NULL;

#else

  static char buff[MAXPATHLEN];
  char *file_part;

  if (SearchPath(NULL, file, ".exe", MAXPATHLEN, buff, &file_part) == 0)
    return NULL;

  return buff;

#endif
}


#ifdef USE_ALONE
/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  int devel_mode = 0;
  char *start_path = Get_Prolog_Path();

  if (start_path == NULL)
    {
      puts("error: GNU Prolog path not found");
      return 1;
    }

  if (*start_path == '@')       /* development mode */
    {
      start_path++;
      devel_mode = 1;
    }

  puts(start_path);
  if (argc > 1 && devel_mode)
    puts("development mode");

  return 0;
}

#endif

#endif  /* _PROLOG_PATH_C */
