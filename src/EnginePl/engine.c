/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : engine.c                                                        *
 * Descr.: general engine                                                  *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <locale.h>

#include "gp_config.h"
#ifdef M_ix86_win32
#include <windows.h>
#endif

#define ENGINE_FILE

#include "engine_pl.h"


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define ERR_DIRECTIVE_FAILED       "warning: %s:%d: %s directive failed\n"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#if !defined(NO_USE_REGS) && NB_OF_USED_MACHINE_REGS > 0
static WamWord init_buff_regs[NB_OF_USED_MACHINE_REGS];
#endif


static WamWord *heap_actual_start;

static int nb_user_directives = 0;

static jmp_buf *p_jumper;
static WamWord *p_buff_save;

static CodePtr cont_jmp;	/* we use a global var to support DEC alpha */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Call_Prolog_Fail(void);

static void Call_Prolog_Success(void);

static Bool Call_Next(CodePtr codep);

void Call_Compiled(CodePtr codep);	/* defined in engine1.c */




/*-------------------------------------------------------------------------*
 * START_PROLOG                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Start_Prolog(int argc, char *argv[])
{
  int i, x;
  char *p;

#if 0				/* causes pb when printing float numbers ... */
  setlocale(LC_ALL, "");
#endif
  Init_Machine();

  os_argc = argc;
  os_argv = argv;

  Set_Line_Buf(stdout);
  Set_Line_Buf(stderr);

  for (i = 0; i < NB_OF_STACKS; i++)
    {
      if (fd_init_solver == NULL && strcmp(stk_tbl[i].name, "cstr") == 0)
	{			/* FD solver not linked */
	  stk_tbl[i].size = 0;
	  continue;
	}

      if ((stk_tbl[i].size = KBytes_To_Wam_Words(*(stk_tbl[i].p_def_size)))
	  == 0)
	stk_tbl[i].size = stk_tbl[i].default_size;

      if (!fixed_sizes && *stk_tbl[i].env_var_name)
	{
	  p = (char *) getenv(stk_tbl[i].env_var_name);
	  if (p && *p)
	    {
	      sscanf(p, "%d", &x);
	      stk_tbl[i].size = KBytes_To_Wam_Words(x);
	    }
	}
    }

  M_Allocate_Stacks();
  Save_Machine_Regs(init_buff_regs);

#ifndef NO_MACHINE_REG_FOR_REG_BANK
  Init_Reg_Bank(Global_Stack);  /* allocated X regs + other non alloc regs */
  Global_Stack += REG_BANK_SIZE; /* at the beginning of the heap */
  Global_Size -= REG_BANK_SIZE;
#endif

  /* must be changed to store global info (see the debugger) */
  heap_actual_start = Global_Stack;

  Init_Atom();
  Init_Pred();
  Init_Oper();

  Reset_Prolog();
  Fd_Init_Solver();

  Find_Linked_Objects();

  return nb_user_directives;
}




/*-------------------------------------------------------------------------*
 * STOP_PROLOG                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Stop_Prolog(void)
{
#ifdef DEREF_STATS
  double d = (double) chain_len / (double) nb_deref;
  printf("Deref: nb: %ld  avg len = %g\n", nb_deref, d);
#endif
  Restore_Machine_Regs(init_buff_regs);
}




/*-------------------------------------------------------------------------*
 * RESET_PROLOG                                                            *
 *                                                                         *
 * Reset top stack pointers and create first choice point (for Call_Prolog)*
 *-------------------------------------------------------------------------*/
void
Reset_Prolog(void)
{
  E = B = LSSA = Local_Stack;
  H = heap_actual_start;	/* restart after needed global terms */
  TR = Trail_Stack;
  CP = NULL;
  STAMP = 0;
  CS = Cstr_Stack;
  BCI = 0;			/* BCI only needed for byte-code (cf. bips prolog) */

  Create_Choice_Point(Call_Prolog_Fail, 0);	/* 1st choice point */

  Fd_Reset_Solver();
}




/*-------------------------------------------------------------------------*
 * RESET_PROLOG_IN_SIGNAL                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reset_Prolog_In_Signal(void)
{
  Restore_Protect_Regs_For_Signal;
}




/*-------------------------------------------------------------------------*
 * SET_HEAP_ACTUAL_START                                                   *
 *                                                                         *
 * Called to store permanent terms (cf. debugger).                         *
 *-------------------------------------------------------------------------*/
void
Set_Heap_Actual_Start(WamWord *new_heap_actual_start)
{
  heap_actual_start = new_heap_actual_start;
}




/*-------------------------------------------------------------------------*
 * EXECUTE_DIRECTIVE                                                       *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void
Execute_Directive(int pl_file, int pl_line, Bool is_system, CodePtr proc)
{
  Reset_Prolog();

  if (!is_system)
    nb_user_directives++;

  if (!Call_Prolog(proc))
    fprintf(stderr,
	    ERR_DIRECTIVE_FAILED, atom_tbl[pl_file].name, pl_line,
	    (is_system) ? "system" : "user");

  Reset_Prolog();
}




/*-------------------------------------------------------------------------*
 * TRY_EXECUTE_TOP_LEVEL                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Try_Execute_Top_Level(void)
{
  PredInf *pred;

  Reset_Prolog();

  pred = Lookup_Pred(Create_Atom("top_level"), 0);

  if (pred != NULL)
    {
      Call_Prolog((CodePtr) (pred->codep));
      return TRUE;
    }

  Reset_Prolog();
  return FALSE;
}




/*-------------------------------------------------------------------------*
 * CALL_PROLOG                                                             *
 *                                                                         *
 * Call_Prolog runs the execution of one prolog goal.                      *
 * The current choice point is updated to set ALTB to Call_Prolog_Fail and *
 * CP is set to Call_Prolog_Success. At the end ALTB and CP are restored.  *
 * To ensure that a choice point always exists before invoking Call_Prolog,*
 * Start_Prolog reserve the space for a feint choice point, i.e ALTB can be*
 * safely modified.                                                        *
 *                                                                         *
 * Call_Prolog returns TRUE if the predicate has succeed, FALSE otherwise. *
 * The called predicate can be non-deterministic.                          *
 *-------------------------------------------------------------------------*/
Bool
Call_Prolog(CodePtr codep)
{
  WamWord *query_b = B;
  WamCont save_CP = CP;
  WamCont save_ALTB = ALTB(query_b);
  Bool ok;

  ALTB(query_b) = (CodePtr) Call_Prolog_Fail;	/* modify choice point */

  CP = Adjust_CP(Call_Prolog_Success);
#ifdef M_ix86_win32
  _try
  {
#endif
    ok = Call_Next(codep);
#ifdef M_ix86_win32
  }
  _except(Is_Win32_SEGV(GetExceptionInformation()))
  {
    SIGSEGV_Handler();
  }
#endif

  CP = save_CP;			/* restore continuation */
  ALTB(query_b) = save_ALTB;	/* restore choice point */

  return ok;
}




/*-------------------------------------------------------------------------*
 * CALL_PROLOG_NEXT_SOL                                                    *
 *                                                                         *
 * Call_Prolog_Next_Sol bactracks over the next solution.                  *
 *-------------------------------------------------------------------------*/
Bool
Call_Prolog_Next_Sol(WamWord *query_b)
{
  WamCont save_CP = CP;
  WamCont save_ALTB = ALTB(query_b);
  Bool ok;

  ALTB(query_b) = (CodePtr) Call_Prolog_Fail;	/* modify choice point */

  CP = Adjust_CP(Call_Prolog_Success);	/* should be useless since */
  /* alternative will restore CP */

  ok = Call_Next(ALTB(B));

  CP = save_CP;			/* restore continuation */
  ALTB(query_b) = save_ALTB;	/* restore choice point */

  return ok;
}




/*-------------------------------------------------------------------------*
 * KEEP_REST_FOR_PROLOG                                                    *
 *                                                                         *
 * Update CP in choices points to be used by classical Prolog engine       *
 * (some CPB(b) have been set to Call_Prolog_Success due to Call_Prolog).  *
 *-------------------------------------------------------------------------*/
void
Keep_Rest_For_Prolog(WamWord *query_b)
{
  WamWord *b, *e, *query_e;

  for (b = B; b > query_b; b = BB(b))
    if (CPB(b) == Adjust_CP(Call_Prolog_Success))
      CPB(b) = CP;

  query_e = EB(query_b);

  for (e = EB(B); e > query_e; e = EE(e))
    if (CPE(e) == Adjust_CP(Call_Prolog_Success))
      CPE(e) = CP;
}




/*-------------------------------------------------------------------------*
 * CALL_NEXT                                                               *
 *                                                                         *
 * Call_Next saves the context with setjmp. Since Call_Prolog can be nested*
 * we handle a stack of jumpers (i.e. contexts) directely in the C stack.  *
 * The global variables p_jumper is the top of the stack and points to the *
 * current jumper. Similarly for the stack of machine register save buffers*
 *-------------------------------------------------------------------------*/
static Bool
Call_Next(CodePtr codep)
{
  int jmp_val;
  jmp_buf *old_jumper = p_jumper;
  jmp_buf new_jumper;
  WamWord *old_buff_save = p_buff_save;
  WamWord buff_save_machine_regs[NB_OF_USED_MACHINE_REGS + 1];	/* +1 if = 0 */
  WamWord buff_save_all_regs[NB_OF_REGS];

  p_jumper = &new_jumper;
  p_buff_save = buff_save_machine_regs;

  Save_All_Regs(buff_save_all_regs);
  Save_Machine_Regs(buff_save_machine_regs);

  jmp_val = setjmp(*p_jumper);

  Restore_Machine_Regs(buff_save_machine_regs);

  if (jmp_val == 0)		/* normal call to codep */
    Call_Compiled(codep);

  if (jmp_val == 3)		/* return with a continuation in jmp_val */
    Call_Compiled(cont_jmp);

  /* normal return */
  p_jumper = old_jumper;
  p_buff_save = old_buff_save;

  if (jmp_val < 0)		/* false: restore WAM registers */
    {
      Restore_All_Regs(buff_save_all_regs);
      return FALSE;
    }

  return jmp_val;		/* 1 (TRUE) or 3 (exception) */
}




	  /*------------------------------------------------------------*
           * Call_Prolog_Fail: Prolog continuation after failure.       *
           * Return in Call_Next with a longjmp (value -1)              *
           *------------------------------------------------------------*/

static void
Call_Prolog_Fail(void)
{
  Save_Machine_Regs(p_buff_save);
  longjmp(*p_jumper, -1);
}




	  /*------------------------------------------------------------*
           * Call_Prolog_Success: Prolog continuation after success.    *
           * Return in Call_Next with a longjmp (value 1)               *
           *------------------------------------------------------------*/

static void
Call_Prolog_Success(void)
{
  Save_Machine_Regs(p_buff_save);
  longjmp(*p_jumper, 1);
}




	  /*------------------------------------------------------------*
           * Exit_With_Exception:                                       *
           * Similar to a success but Call_Prolog returns 2 instead of 1*
           * (i.e. TRUE)                                                *
           * Return in Call_Next with a longjmp (value 2)               *
           *------------------------------------------------------------*/

void
Exit_With_Exception(void)
{
  Save_Machine_Regs(p_buff_save);
  longjmp(*p_jumper, 2);
}




	  /*------------------------------------------------------------*
           * Execute_A_Continuation:                                    *
           * Similar to a nested Call_Prolog but faster, and if a fail  *
           * occurs it is normally handled by the prolog engine, i.e.   *
           * the last choice point is reconsidered.                     *
           * Return in Call_Next with a longjmp (value 3 cont_jmp=codep)*
           *------------------------------------------------------------*/

void
Execute_A_Continuation(CodePtr codep)
{
  Save_Machine_Regs(p_buff_save);

  cont_jmp = codep;
  longjmp(*p_jumper, 3);
}
