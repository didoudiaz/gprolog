/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine + Compiler                                        *
 * File  : machine1.h                                                      *
 * Descr.: machine dependent features - Header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2004 Daniel Diaz                                     *
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

#include <stdio.h>

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define M_OS_UNIX                  0
#define M_OS_WINDOWS               1
#define M_OS_WINDOWS_NT            2




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef MACHINE1_FILE

int m_os_type;
char m_architecture[32];
char m_os_version[256];

#else

extern int m_os_type;
extern char m_architecture[];
extern char m_os_version[];

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Init_Machine1(void);

char **M_Create_Shell_Command(char *cmd);

char **M_Cmd_Line_To_Argv(char *cmd, int *argc);

int M_Shell(char *cmd);

int M_Spawn(char *arg[]);

int M_Spawn_Redirect(char *arg[], int detach,
		     FILE **f_in, FILE **f_out, FILE **f_err);

int M_Get_Status(int pid);



char *M_Mktemp(char *tmp_template);

char *M_Tempnam(char *dir, char *pfx);



#define   DBGPRINTF             printf
