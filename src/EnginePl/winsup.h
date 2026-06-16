/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : winsup.c                                                        *
 * Descr.: Unix compatibility replacements for Windows - header file       *
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

/* see unix <dirent.h> */

struct dirent	     	
{
  long d_ino;
  unsigned short d_reclen;
  unsigned char d_type;
  char d_name[MAXPATHLEN];
};
  
  
#define DT_UNKNOWN      0 /* d_type: file types.  */
#define DT_FIFO         1
#define DT_CHR          2
#define DT_DIR          4
#define DT_BLK          6
#define DT_REG          8
#define DT_LNK          10
#define DT_SOCK         12
#define DT_WHT          14

struct DIR
{
  char *dirname;
  struct dirent ret;          /* Used to return to caller */
  void *handle;		      /* equivalent of windows HANDLE (avoid to include windows.h) */
};

typedef struct DIR DIR;
  
/* Protoypes */

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

DIR *opendir(char *dir_path);
struct dirent *readdir(DIR *dir);
int closedir(DIR *dir);

char *realpath(const char *path, char *resolved_path);

#endif	/*_WIN32 && !__CYGWIN__ */
