/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : engine.c                                                        *
 * Descr.: general engine                                                  *
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
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

#include "gp_config.h"
#include "set_locale.h"

#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>
#define READ_REGISTRY_ONLY
#include "../TopComp/prolog_path.c"
#else
#include <sys/param.h>
#endif

#define ENGINE_FILE

#include "engine_pl.h"

#ifndef NO_USE_LINEDIT
#include "../Linedit/linedit.h"
#endif

#include "../TopComp/prolog_path.c"



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

void (*pl_init_stream_supp)();  /* overwritten by foreign if present */

#if !defined(NO_USE_REGS) && NB_OF_USED_MACHINE_REGS > 0
static WamWord init_buff_regs[NB_OF_USED_MACHINE_REGS];
#endif


static WamWord *heap_actual_start;

static int nb_user_directives = 0;

static sigjmp_buf *p_jumper;
static WamWord *p_buff_save;

static CodePtr cont_jmp;        /* we use a global var to support DEC alpha */



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Call_Prolog_Fail(void);

static void Call_Prolog_Success(void);

static Bool Call_Next(CodePtr codep);

void Pl_Call_Compiled(CodePtr codep);   /* defined in engine1.c */




/*-------------------------------------------------------------------------*
 * PL_START_PROLOG                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Start_Prolog(int argc, char *argv[])
{
  int i, x;
  char *p;
  void (*copy_of_pl_init_stream_supp)() = Pl_Dummy_Ptr(pl_init_stream_supp);
#if defined(_WIN32) || defined(__CYGWIN__)
  DWORD y;
#endif

  Set_Locale();

  pl_os_argc = argc;
  pl_os_argv = argv;

  pl_home = Get_Prolog_Path(argv[0], &pl_devel_mode);

  Pl_Init_Machine();

  Set_Line_Buf(stdout);
  Set_Line_Buf(stderr);

  for (i = 0; i < NB_OF_STACKS; i++)
    {
      if (pl_fd_init_solver == NULL && strcmp(pl_stk_tbl[i].name, "cstr") == 0)
        {                       /* FD solver not linked */
          pl_stk_tbl[i].size = 0;
          continue;
        }

      if ((pl_stk_tbl[i].size = KBytes_To_Wam_Words(*(pl_stk_tbl[i].p_def_size))) == 0)
        pl_stk_tbl[i].size = pl_stk_tbl[i].default_size;

      if (!pl_fixed_sizes && *pl_stk_tbl[i].env_var_name)
        {
          p = (char *) getenv(pl_stk_tbl[i].env_var_name);
          if (p && *p)
            {
              sscanf(p, "%d", &x);
              pl_stk_tbl[i].size = KBytes_To_Wam_Words(x);
            }
#if defined(_WIN32) || defined(__CYGWIN__)
          if (Read_Windows_Registry(pl_stk_tbl[i].env_var_name, REG_DWORD, &y, sizeof(x)))
            pl_stk_tbl[i].size = KBytes_To_Wam_Words(y);
#endif
        }      
    }

  /* similar treatment for max_atom */

  if ((pl_max_atom = pl_def_max_atom) == 0)
    pl_max_atom = DEFAULT_MAX_ATOM;
  
  if (!pl_fixed_sizes)
    {
      p = (char *) getenv(ENV_VAR_MAX_ATOM);
      if (p && *p)
	{
	  sscanf(p, "%d", &x);
	  pl_max_atom = x;
	}
#if defined(_WIN32) || defined(__CYGWIN__)
      if (Read_Windows_Registry(ENV_VAR_MAX_ATOM, REG_DWORD, &y, sizeof(x)))
	pl_max_atom = y;
#endif
    }

  Pl_Allocate_Stacks();
  Save_Machine_Regs(init_buff_regs);

#ifndef NO_MACHINE_REG_FOR_REG_BANK
  Init_Reg_Bank(Global_Stack);  /* allocated X regs + other non alloc regs */
  Global_Stack += REG_BANK_SIZE; /* at the beginning of the heap */
  Global_Size -= REG_BANK_SIZE;
#endif

  /* must be changed to store global info (see the debugger) */
  heap_actual_start = Global_Stack;

  Pl_Init_Atom();
  Pl_Init_Pred();
  Pl_Init_Oper();

  pl_le_mode = 0;	/* not compiled with linedit or deactivated (using env var) */

#ifndef NO_USE_LINEDIT
  if (pl_le_initialize != NULL)
    pl_le_mode = (*pl_le_initialize)();
#endif

  if (copy_of_pl_init_stream_supp)
    (*copy_of_pl_init_stream_supp)();

  Pl_Reset_Prolog();
  Pl_Fd_Init_Solver();

  Pl_Find_Linked_Objects();

  return nb_user_directives;
}




/*-------------------------------------------------------------------------*
 * PL_STOP_PROLOG                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Stop_Prolog(void)
{
#ifdef DEREF_STATS
  double d = (double) chain_len / (double) nb_deref;
  fprintf(stderr, "Deref: nb: %" PL_FMT_d "  avg len = %g\n", nb_deref, d);
#endif
  Restore_Machine_Regs(init_buff_regs);
}




/*-------------------------------------------------------------------------*
 * PL_RESET_PROLOG                                                         *
 *                                                                         *
 * Reset top stack pointers and create first choice point (for Call_Prolog)*
 *-------------------------------------------------------------------------*/
void
Pl_Reset_Prolog(void)
{
  E = B = LSSA = Local_Stack;
  H = heap_actual_start;        /* restart after needed global terms */
  TR = Trail_Stack;
  CP = NULL;
  STAMP = 0;
  CS = Cstr_Stack;
  BCI = 0;                      /* BCI only needed for byte-code (cf. bips prolog) */

  Pl_Create_Choice_Point(Call_Prolog_Fail, 0);  /* 1st choice point */

  Pl_Fd_Reset_Solver();
}




/*-------------------------------------------------------------------------*
 * PL_RESET_PROLOG_IN_SIGNAL                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Reset_Prolog_In_Signal(void)
{
  Restore_Protect_Regs_For_Signal;
}




/*-------------------------------------------------------------------------*
 * PL_SET_HEAP_ACTUAL_START                                                *
 *                                                                         *
 * Called to store permanent terms (cf. debugger).                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Heap_Actual_Start(WamWord *new_heap_actual_start)
{
  heap_actual_start = new_heap_actual_start;
}




/*-------------------------------------------------------------------------*
 * PL_EXECUTE_DIRECTIVE                                                    *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Execute_Directive(int pl_file, int pl_line, Bool is_system, CodePtr proc)
{
  Pl_Reset_Prolog();

  if (!is_system)
    nb_user_directives++;

  if (!Pl_Call_Prolog(proc))
    fprintf(stderr,
            ERR_DIRECTIVE_FAILED, pl_atom_tbl[pl_file].name, pl_line,
            (is_system) ? "system" : "user");

  Pl_Reset_Prolog();
}




/*-------------------------------------------------------------------------*
 * PL_TRY_EXECUTE_TOP_LEVEL                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Try_Execute_Top_Level(void)
{
  PredInf *pred;

  Pl_Reset_Prolog();

  pred = Pl_Lookup_Pred(Pl_Create_Atom("top_level"), 0);

  if (pred != NULL)
    {
      Pl_Call_Prolog((CodePtr) (pred->codep));
      return TRUE;
    }

  Pl_Reset_Prolog();
  return FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_CALL_PROLOG                                                          *
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
Pl_Call_Prolog(CodePtr codep)
{
  WamWord *query_b = B;
  WamCont save_CP = CP;
  WamCont save_ALTB = ALTB(query_b);
  Bool ok;

  ALTB(query_b) = (CodePtr) Call_Prolog_Fail;   /* modify choice point */

  CP = Adjust_CP(Call_Prolog_Success);

  ok = Call_Next(codep);

  CP = save_CP;                 /* restore continuation */
  ALTB(query_b) = save_ALTB;    /* restore choice point */

  return ok;
}




/*-------------------------------------------------------------------------*
 * PL_CALL_PROLOG_NEXT_SOL                                                 *
 *                                                                         *
 * Call_Prolog_Next_Sol bactracks over the next solution.                  *
 *-------------------------------------------------------------------------*/
Bool
Pl_Call_Prolog_Next_Sol(WamWord *query_b)
{
  WamCont save_CP = CP;
  WamCont save_ALTB = ALTB(query_b);
  Bool ok;

  ALTB(query_b) = (CodePtr) Call_Prolog_Fail;   /* modify choice point */

  CP = Adjust_CP(Call_Prolog_Success);  /* should be useless since */
  /* alternative will restore CP */

  ok = Call_Next(ALTB(B));

  CP = save_CP;                 /* restore continuation */
  ALTB(query_b) = save_ALTB;    /* restore choice point */

  return ok;
}




/*-------------------------------------------------------------------------*
 * PL_KEEP_REST_FOR_PROLOG                                                 *
 *                                                                         *
 * Update CP in choices points to be used by classical Prolog engine       *
 * (some CPB(b) have been set to Call_Prolog_Success due to Call_Prolog).  *
 *-------------------------------------------------------------------------*/
void
Pl_Keep_Rest_For_Prolog(WamWord *query_b)
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
  sigjmp_buf *old_jumper = p_jumper;
  sigjmp_buf new_jumper;
  WamWord *old_buff_save = p_buff_save;
  WamWord buff_save_machine_regs[NB_OF_USED_MACHINE_REGS + 1];  /* +1 if = 0 */
#if 0
  WamWord buff_save_all_regs[NB_OF_REGS];
#endif

  p_jumper = &new_jumper;
  p_buff_save = buff_save_machine_regs;

#if 0
  Save_All_Regs(buff_save_all_regs);
#endif
  Save_Machine_Regs(buff_save_machine_regs);

  jmp_val = sigsetjmp(*p_jumper, 1);

  Restore_Machine_Regs(buff_save_machine_regs);

  if (jmp_val == 0)             /* normal call to codep */
    Pl_Call_Compiled(codep);

  if (jmp_val == 3)             /* return with a continuation in jmp_val */
    Pl_Call_Compiled(cont_jmp);

                                /* normal return */
  p_jumper = old_jumper;
  p_buff_save = old_buff_save;

  if (jmp_val < 0)              /* false: restore WAM registers */
    {
#if 0
      Restore_All_Regs(buff_save_all_regs);
#endif
      return FALSE;
    }

  return jmp_val;               /* 1 (TRUE) or 3 (exception) */
}




          /*------------------------------------------------------------*
           * Call_Prolog_Fail: Prolog continuation after failure.       *
           * Return in Call_Next with a longjmp (value -1)              *
           *------------------------------------------------------------*/
#if 1/*!(defined(M_x86_64) && defined(_MSC_VER))*//* see file engine_asm.s */

static void
Call_Prolog_Fail(void)
{
#ifdef M_ix86_darwin            /* see comment in Ma2Asm/ix86_any.c */
  asm("subl $4,%esp");
#elif defined(M_x86_64) && !defined(_MSC_VER)   /* see comment in Ma2Asm/x86_64_any.c */
  asm("subq $8,%rsp");
#endif
  Save_Machine_Regs(p_buff_save);
  siglongjmp(*p_jumper, -1);
}




          /*------------------------------------------------------------*
           * Call_Prolog_Success: Prolog continuation after success.    *
           * Return in Call_Next with a longjmp (value 1)               *
           *------------------------------------------------------------*/

static void
Call_Prolog_Success(void)
{
#ifdef M_ix86_darwin            /* see comment in Ma2Asm/ix86_any.c */
  asm("subl $4,%esp");
#elif defined(M_x86_64) && !defined(_MSC_VER)   /* see comment in Ma2Asm/x86_64_any.c */
  asm("subq $8,%rsp");
#endif
  Save_Machine_Regs(p_buff_save);
  siglongjmp(*p_jumper, 1);
}

#endif

          /*------------------------------------------------------------*
           * Exit_With_Exception:                                       *
           * Similar to a success but Call_Prolog returns 2 instead of 1*
           * (i.e. TRUE)                                                *
           * Return in Call_Next with a longjmp (value 2)               *
           *------------------------------------------------------------*/

void
Pl_Exit_With_Exception(void)
{
  Save_Machine_Regs(p_buff_save);
  siglongjmp(*p_jumper, 2);
}




          /*------------------------------------------------------------*
           * Execute_A_Continuation:                                    *
           * Similar to a nested Call_Prolog but faster, and if a fail  *
           * occurs it is normally handled by the prolog engine, i.e.   *
           * the last choice point is reconsidered.                     *
           * Return in Call_Next with a longjmp (value 3 cont_jmp=codep)*
           *------------------------------------------------------------*/

void
Pl_Execute_A_Continuation(CodePtr codep)
{
  Save_Machine_Regs(p_buff_save);

  cont_jmp = codep;
  siglongjmp(*p_jumper, 3);
}
