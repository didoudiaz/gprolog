/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine + Compiler                                        *
 * File  : machine1.h                                                      *
 * Descr.: machine dependent features - Header file                        *
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


#include <stdio.h>

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define M_OS_UNIX                  0
#define M_OS_WINDOWS               1
#define M_OS_WINDOWS_NT            2


#define M_ERROR_WIN32              -2 /* read GetLastError instead of errno */



/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef MACHINE1_FILE

int pl_m_os_type;
char pl_m_architecture[32];
char pl_m_os_version[256];

#else

extern int pl_m_os_type;
extern char pl_m_architecture[];
extern char pl_m_os_version[];

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Pl_Init_Machine1(void);

char **Pl_M_Create_Shell_Command(char *cmd);

char **Pl_M_Cmd_Line_To_Argv(char *cmd, int *argc);

int Pl_M_Shell(char *cmd);

int Pl_M_Spawn(char *arg[]);

int Pl_M_Spawn_Redirect(char *arg[], int detach,
		     FILE **f_in, FILE **f_out, FILE **f_err);

int Pl_M_Get_Status(int pid);



char *Pl_M_Mktemp(char *tmp_template);

char *Pl_M_Tempnam(char *dir, char *pfx);



#define   DBGPRINTF             printf
