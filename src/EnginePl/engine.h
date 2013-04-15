/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : engine.h                                                        *
 * Descr.: general engine - header file                                    *
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


#ifdef NO_STACK_TEST
#   undef  M_Check_Stacks()
#   define M_Check_Stacks()
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


#define cpp_recurs(p, n)           p##__a##n
#define Prolog_Predicate(p, n)     cpp_recurs(p, n)

#define Prolog_Prototype(p, n)     void Prolog_Predicate(p, n)()




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/



/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef ENGINE_FILE

int pl_os_argc;
char **pl_os_argv;

char *pl_home;
int pl_devel_mode;

char pl_glob_buff[10240];

PlLong *pl_base_fl;		/* overwritten by foreign if present */
double *pl_base_fd;		/* overwritten by foreign if present */

int pl_le_mode;			/* LE_MODE_HOOK if GUI */

#else

extern int pl_os_argc;
extern char **pl_os_argv;

extern char *pl_home;
extern int pl_devel_mode;

extern char pl_glob_buff[];

extern PlLong *pl_base_fl;
extern double *pl_base_fd;

extern int pl_le_mode;

#endif



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Pl_Start_Prolog(int argc, char *argv[]);

void Pl_Stop_Prolog(void);

void Pl_Reset_Prolog(void);

void Pl_Reset_Prolog_In_Signal(void);

void Pl_Set_Heap_Actual_Start(WamWord *heap_actual_start);



void Pl_Execute_Directive(int pl_file, int pl_line, Bool is_system,
			  CodePtr proc);

Bool Pl_Try_Execute_Top_Level(void);

Bool Pl_Call_Prolog(CodePtr codep);

Bool Pl_Call_Prolog_Next_Sol(WamWord *query_b);

void Pl_Keep_Rest_For_Prolog(WamWord *query_b);

void Pl_Exit_With_Exception(void);

void Pl_Execute_A_Continuation(CodePtr codep);



#define   Goto_Predicate(p, n)  ((*Prolog_Predicate(p, n))())
