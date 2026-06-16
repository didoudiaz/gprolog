/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : os_path.h - header file                                         *
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

#ifndef _OS_PATH_H
#define _OS_PATH_H

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

#define Is_Dir_Sep(c)              ((c) == DIR_SEP_C || (c) == DIR_SEP_C_ALT)


#define Find_Last_Dir_Sep(_p, _path)			\
  do {							\
    char *_ptr;						\
							\
    for((_p) = NULL, _ptr = (_path); *_ptr; _ptr++)	\
      if (Is_Dir_Sep(*_ptr))				\
	(_p) = _ptr;					\
  } while (0)




char *Pl_Get_Prolog_Path(char *argv0, Bool *devel_mode);

char *Pl_Search_Path(char *exe_file);

char *Pl_Get_Working_Dir(void);

Bool Pl_Set_Working_Dir(char *path);

char *Pl_Absolute_Path_Name0(char *path, Bool del_trail_slash);

#define Pl_Absolute_Path_Name(p) Pl_Absolute_Path_Name0(p, TRUE)

Bool Pl_Is_Absolute_File_Name(char *path);

char *Pl_Decompose_File_Name(char *path, Bool del_trail_slashes, 
			     char **base, char **suffix);

Bool Pl_Path_Ends_With_Dir(char *path);

int Pl_Is_Dir_Name(char *path, Bool inexistent_as_error);


#if defined(_WIN32) && !defined(__CYGWIN__)
int Pl_Access(const char *path, int mode);
#else
#define Pl_Access(p, m) access(p, m)
#endif

char *Pl_Mktemp(char *tmpl);
char *Pl_Tempnam(char *dir, char *pfx);

#endif /* !_OS_PATH_H */
