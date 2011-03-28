/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : file_c.c                                                        *
 * Descr.: file name management - C part                                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2011 Daniel Diaz                                     *
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

/* $Id$ */

#include <string.h>

#include "engine_pl.h"
#include "bips_pl.h"

#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#include <sys/param.h>
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




/*-------------------------------------------------------------------------*
 * PL_ABSOLUTE_FILE_NAME_2                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Absolute_File_Name_2(WamWord f1_word, WamWord f2_word)
{
  char *f1, *f2;

  f1 = pl_atom_tbl[Pl_Rd_Atom_Check(f1_word)].name;

  f2 = Pl_M_Absolute_Path_Name(f1);
  if (f2 == NULL)
    Pl_Err_Domain(pl_domain_os_path, f1_word);

  return Pl_Un_String_Check(f2, f2_word);
}




/*-------------------------------------------------------------------------*
 * PL_DECOMPOSE_FILE_NAME_4                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Decompose_File_Name_4(WamWord path_word, WamWord dir_word,
		      WamWord prefix_word, WamWord suffix_word)
{
  char *path;
  char *p;
  int atom;
  char c;

  strcpy(pl_glob_buff, pl_atom_tbl[Pl_Rd_Atom_Check(path_word)].name);
  path = pl_glob_buff;


  Pl_Check_For_Un_Atom(dir_word);
  Pl_Check_For_Un_Atom(prefix_word);
  Pl_Check_For_Un_Atom(suffix_word);

  p = strrchr(path, DIR_SEP_C);
#ifdef _WIN32
  {
    char *q = strrchr(path, '/');

    if (p == NULL || p > q)
      p = q;
  }
#endif

  if (p != NULL)
    {
      p++;
      c = *p;
      *p = '\0';
      atom = Pl_Create_Allocate_Atom(path);
      *p = c;
      path = p;
    }
  else
    atom = pl_atom_void;

  if (!Pl_Get_Atom(atom, dir_word))
    return FALSE;


  p = strrchr(path, '.');
  if (p != NULL)
    {
      *p = '\0';
      atom = Pl_Create_Allocate_Atom(path);
      *p = '.';
      path = p;
    }
  else
    atom = Pl_Create_Allocate_Atom(path);

  if (!Pl_Get_Atom(atom, prefix_word))
    return FALSE;

  if (p != NULL)
    atom = Pl_Create_Allocate_Atom(path);
  else
    atom = pl_atom_void;

  return Pl_Get_Atom(atom, suffix_word);
}




/*-------------------------------------------------------------------------*
 * PL_PROLOG_FILE_NAME_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Prolog_File_Name_2(WamWord f1_word, WamWord f2_word)
{
  int atom;
  char *f1;
  int len;
  char *p;
  Bool suffix_pl;

  atom = Pl_Rd_Atom_Check(f1_word);
  f1 = pl_atom_tbl[atom].name;

  f1 = Pl_M_Absolute_Path_Name(f1);
  if (f1 == NULL)
    Pl_Err_Domain(pl_domain_os_path, f1_word);

  if (strcmp(f1, "user") == 0)
    {
    same:
      return Pl_Un_Atom_Check(atom, f2_word);
    }

  p = strrchr(f1, DIR_SEP_C);
#ifdef _WIN32
  {
    char *q = strrchr(f1, '/');

    if (p == NULL || p > q)
      p = q;
  }
#endif

  if (strchr((p) ? p : f1, '.'))
    goto same;

  strcpy(pl_glob_buff, f1);
  len = strlen(f1);

  strcpy(pl_glob_buff + len, ".pl");
  suffix_pl = TRUE;
  if (access(pl_glob_buff, F_OK))	/* f1.pl does not exist */
    {
      strcpy(pl_glob_buff + len, ".pro");
      suffix_pl = FALSE;
      if (access(pl_glob_buff, F_OK))	/* f1.pro does not exist */
	suffix_pl = TRUE;
    }

  sprintf(pl_glob_buff, "%s%s", pl_atom_tbl[atom].name,
	  (suffix_pl) ? ".pl" : ".pro");

  return Pl_Un_String_Check(pl_glob_buff, f2_word);
}
