/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : file_c.c                                                        *
 * Descr.: file name management - C part                                   *
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
Pl_Absolute_File_Name_2(WamWord path1_word, WamWord path2_word)
{
  char *path1, *path2;

  path1 = pl_atom_tbl[Pl_Rd_Atom_Check(path1_word)].name;

  path2 = Pl_M_Absolute_Path_Name(path1);
  if (path2 == NULL)
    Pl_Err_Domain(pl_domain_os_path, path1_word);

  return Pl_Un_String_Check(path2, path2_word);
}




/*-------------------------------------------------------------------------*
 * PL_IS_ABSOLUTE_FILE_NAME_1                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Is_Absolute_File_Name_1(WamWord path_word)
{
  char *path = pl_atom_tbl[Pl_Rd_Atom_Check(path_word)].name;

  return Pl_M_Is_Absolute_File_Name(path);
}




/*-------------------------------------------------------------------------*
 * PL_IS_RELATIVE_FILE_NAME_1                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Is_Relative_File_Name_1(WamWord path_word)
{
  return !Pl_Is_Absolute_File_Name_1(path_word);
}




/*-------------------------------------------------------------------------*
 * PL_DECOMPOSE_FILE_NAME_4                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Decompose_File_Name_4(WamWord path_word, WamWord dir_word,
			 WamWord prefix_word, WamWord suffix_word)
{
  char *path = pl_atom_tbl[Pl_Rd_Atom_Check(path_word)].name;
  char *dir, *base, *suffix;
  char c;

  Pl_Check_For_Un_Atom(dir_word);
  Pl_Check_For_Un_Atom(prefix_word);
  Pl_Check_For_Un_Atom(suffix_word);

  dir = Pl_M_Decompose_File_Name(path, FALSE, &base, &suffix);

  if (!Pl_Un_String(dir, dir_word))
    return FALSE;

  c = *suffix;
  *suffix = '\0';

  if (!Pl_Un_String(base, prefix_word))
    return FALSE;

  *suffix = c;

  return Pl_Un_String(suffix, suffix_word);
}




/*-------------------------------------------------------------------------*
 * PL_PROLOG_FILE_NAME_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Prolog_File_Name_2(WamWord path1_word, WamWord path2_word)
{
  int atom;
  char *path1;
  int len;
  char *p;
  Bool suffix_pl;

  atom = Pl_Rd_Atom_Check(path1_word);
  path1 = pl_atom_tbl[atom].name;

  path1 = Pl_M_Absolute_Path_Name(path1);
  if (path1 == NULL)
    Pl_Err_Domain(pl_domain_os_path, path1_word);

  if (strcmp(path1, "user") == 0)
    {
    same:
      return Pl_Un_Atom_Check(atom, path2_word);
    }

  Find_Last_Dir_Sep(p, path1);

  if (strchr((p) ? p : path1, '.'))
    goto same;

  strcpy(pl_glob_buff, path1);
  len = strlen(path1);

  strcpy(pl_glob_buff + len, ".pl");
  suffix_pl = TRUE;
  if (access(pl_glob_buff, F_OK))	/* path1.pl does not exist */
    {
      strcpy(pl_glob_buff + len, ".pro");
      suffix_pl = FALSE;
      if (access(pl_glob_buff, F_OK))	/* path1.pro does not exist */
	suffix_pl = TRUE;
    }

  sprintf(pl_glob_buff, "%s%s", pl_atom_tbl[atom].name,
	  (suffix_pl) ? ".pl" : ".pro");

  return Pl_Un_String_Check(pl_glob_buff, path2_word);
}
