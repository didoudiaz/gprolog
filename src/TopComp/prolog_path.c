/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog Compiler                                                 *
 * File  : prolog_path.c                                                   *
 * Descr.: Prolog installation path detector                               *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation; either version 2, or any later version.       *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU General Public License along *
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     *
 *-------------------------------------------------------------------------*/

/* $Id$ */

/* This file is included by top_comp.c and w32_console.c */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>
#include <io.h>
#endif

#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#endif

#include "../EnginePl/gp_config.h"

#if 0
#define DEBUG
#endif

static char *Is_A_Valid_Root(char *str, int *devel_mode);
static char *Search_Path(char *file);

#if defined(_WIN32) || defined(__CYGWIN__)
static int Read_Write_Registry(int read, char *key_name, char *buff,
			       long buff_size);
#endif

#define COMPILER_EXE  GPLC EXE_SUFFIX



/*-------------------------------------------------------------------------*
 * GET_PROLOG_PATH                                                         *
 *                                                                         *
 * returns the GNU Prolog start path (or NULL) and if devel_mode           *
 * the returned buffer can be written.                                     *
 *-------------------------------------------------------------------------*/
static char *
Get_Prolog_Path(int *devel_mode)
{
  static char resolved[MAXPATHLEN];
  char *p;

  if ((p = getenv(ENV_VARIABLE)) != NULL)
    {
      strcpy(resolved, p);
#ifdef DEBUG
      fprintf(stderr, "Prolog path from " ENV_VARIABLE ": %s\n", resolved);
#endif
      if ((p = Is_A_Valid_Root(resolved, devel_mode)) != NULL)
	return p;
    }

  if ((p = Search_Path(COMPILER_EXE)) == NULL)
    {
#if defined(_WIN32) || defined(__CGYWIN__)
      if (Read_Write_Registry(1, "RootPath", resolved, sizeof(resolved)))
	{
#ifdef DEBUG
	  fprintf(stderr, "Prolog path from Registry: %s\n", resolved);
#endif
	  return Is_A_Valid_Root(resolved, devel_mode);
	}
#endif
      return NULL;
    }

#ifdef DEBUG
  fprintf(stderr, GPLC " found from PATH: %s\n", p);
#endif

#if defined(__unix__) || defined(__CYGWIN__)
  if (realpath(p, resolved) == NULL)
    return NULL;
#else  /* realpath useless under Win32 since SearchPath resolves it */
  strcpy(resolved, p);
#endif

#ifdef DEBUG
  fprintf(stderr, "link resolution: %s\n", resolved);
#endif

  p = resolved + strlen(resolved) - 1;
  while (p > resolved && *p != DIR_SEP_C)	/* skip exec_name */
    p--;

  while (p > resolved && *p == DIR_SEP_C)	/* skip / */
    p--;

  while (p > resolved && *p != DIR_SEP_C)	/* skip previous dir name */
    p--;

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

  while (p >= str && *p == DIR_SEP_C)
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




#if defined(_WIN32) || defined(__CYGWIN__)

/*-------------------------------------------------------------------------*
 * READ_WRITE_REGISTRY                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Read_Write_Registry(int read, char *key_name, char *buff, long buff_size)
{
  HKEY hkey_software, hkey_prolog;
  DWORD dw_type;
  unsigned long disp;

  if (RegOpenKeyExA(HKEY_CURRENT_USER,
		    "Software", 0, KEY_QUERY_VALUE, &hkey_software) != 0)
    return 0;

  if (RegCreateKeyEx
      (hkey_software, "GnuProlog", 0, NULL, 0, KEY_ALL_ACCESS, NULL,
       &hkey_prolog, &disp) != 0)
    {
      RegCloseKey(hkey_software);
      return 0;
    }

  if (read)
    {
      dw_type = REG_SZ;
      memset(buff, 0, buff_size);
      if (RegQueryValueExA
	  (hkey_prolog, key_name, 0, &dw_type, buff, &buff_size) != 0
	  || *buff == '\0')
	{
	  RegCloseKey(hkey_software);
	  *buff = '\0';
	  return 0;
	}
    }
  else
    {
      RegSetValueEx(hkey_prolog, key_name, 0, REG_SZ, buff,
		    strlen(buff) + 1);
    }
  RegCloseKey(hkey_prolog);
  RegCloseKey(hkey_software);
  return 1;
}

#endif



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

  if (*start_path == '@')	/* development mode */
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
