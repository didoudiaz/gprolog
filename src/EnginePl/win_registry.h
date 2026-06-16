/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : win_registry.h - header file                                    *
 * Descr.: Windows registry handling                                       *
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

/* This file is included by engine.c, os_path.c and w32_console.c */

#ifndef _WIN_REGISTRY_H
#define _WIN_REGISTRY_H

#include <windows.h>
#include <string.h>

#include "bool.h"


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

/*-------------------------------------------------------------------------*
 * READ_WINDOWS_REGISTRY                                                   *
 *                                                                         *
 * key_name : name of the key                                              *
 * key_type : type of the key (use Windows constant REG_SZ,...)            *
 * buff     : buffer to receive the value                                  *
 * buff_size: size of the buffer                                           *
 *                                                                         *
 * return: TRUE in case of success                                         *
 *-------------------------------------------------------------------------*/
static Bool
Read_Windows_Registry(char *key_name, DWORD key_type, void *buff,
		      DWORD buff_size)
{
  DWORD dw_type;

#define INIT_REGISTRY_ACCESS                                    \
  HKEY hkey_software, hkey_prolog;                              \
  DWORD disp;                                                   \
  int r;                                                        \
                                                                \
  if (RegOpenKeyEx(HKEY_CURRENT_USER, "Software", 0,            \
                   KEY_QUERY_VALUE, &hkey_software) != 0)       \
    return FALSE;                                               \
                                                                \
  if (RegCreateKeyEx(hkey_software, PROLOG_NAME, 0,             \
                     NULL, 0, KEY_ALL_ACCESS, NULL,             \
                     &hkey_prolog, &disp) != 0)                 \
    {                                                           \
      RegCloseKey(hkey_software);                               \
      return FALSE;                                             \
    }

  INIT_REGISTRY_ACCESS;

  dw_type = key_type;           /* useless in fact: it is a output arg */
  memset(buff, 0, buff_size);

  r = RegQueryValueEx(hkey_prolog, key_name, 0, &dw_type,
		      (LPBYTE) buff, &buff_size);

  /* not good key type: set an error */
  if (r == ERROR_SUCCESS && dw_type != key_type) 
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
 * return: TRUE in case of success                                         *
 *-------------------------------------------------------------------------*/
static Bool
Write_Windows_Registry(char *key_name, DWORD key_type, void *buff,
		       DWORD buff_size)
{
  INIT_REGISTRY_ACCESS;

  r = RegSetValueEx(hkey_prolog, key_name, 0, key_type,
		    (LPBYTE) buff, buff_size);

  RegCloseKey(hkey_prolog);
  RegCloseKey(hkey_software);

  return r == ERROR_SUCCESS;
}


/*-------------------------------------------------------------------------*
 * DELETE_WINDOWS_REGISTRY                                                 *
 *                                                                         *
 * key_name : name of the key                                              *
 *                                                                         *
 * return: TRUE in case of success                                         *
 *-------------------------------------------------------------------------*/
static Bool
Delete_Windows_Registry(char *key_name)
{
  INIT_REGISTRY_ACCESS;

  r = RegDeleteValue(hkey_prolog, key_name);

  RegCloseKey(hkey_prolog);
  RegCloseKey(hkey_software);

  return r == ERROR_SUCCESS;
}
#endif  /* !READ_REGISTRY_ONLY */
#endif	/* _WIN32 || __CYGWIN__ */

#endif /* !_WIN_REGISTRY_H */
