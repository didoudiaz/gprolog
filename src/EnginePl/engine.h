/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : engine.h                                                        *
 * Descr.: general engine - header file                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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

#ifdef NO_STACK_TEST
#   undef  M_Check_Stacks()
#   define M_Check_Stacks()
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


#define cpp_recurs(p,n)            p##_##n
#define Prolog_Predicate(p,n)      cpp_recurs(p,n)

#define Prolog_Prototype(p,n)      void Prolog_Predicate(p,n)()




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/



/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef ENGINE_FILE

int os_argc;
char **os_argv;

char glob_buff[10240];

long *base_fl;			/* overwritten by foreign if present */
double *base_fd;		/* overwritten by foreign if present */

#else

extern int os_argc;
extern char **os_argv;

extern char glob_buff[];

extern long *base_fl;
extern double *base_fd;

#endif



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Start_Prolog(int argc, char *argv[]);

void Stop_Prolog(void);

void Reset_Prolog(void);

void Reset_Prolog_In_Signal(void);

void Set_Heap_Actual_Start(WamWord *heap_actual_start);



void Execute_Directive(int pl_file, int pl_line, Bool is_system,
		       CodePtr proc);

Bool Try_Execute_Top_Level(void);

Bool Call_Prolog(CodePtr codep);

Bool Call_Prolog_Next_Sol(WamWord *query_b);

void Keep_Rest_For_Prolog(WamWord *query_b);

void Exit_With_Exception(void);

void Execute_A_Continuation(CodePtr codep);



#define   Goto_Predicate(p,n)   ((*Prolog_Predicate(p,n))())
