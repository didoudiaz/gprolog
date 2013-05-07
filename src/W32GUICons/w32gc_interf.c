/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Win32 GUI console                                               *
 * File  : w32gc_interf.c                                                  *
 * Descr.: line editor <--> GUICons interface                              *
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
#include <windows.h>

#include "../EnginePl/gp_config.h"
#include "../EnginePl/pl_params.h"


/* we include the minimum from ../EnginePl to allow for testing (test_linedit, test_noecho)
 * with Win GUI console separately (see project MS Visual C++ in ../Linedit)
 * this implies some redundant definitions (WamWord, Wam_Words_To_KBytes)
 */

typedef PlLong WamWord;
#include "../EnginePl/wam_stacks.h"
#include "../EnginePl/atom.h"
#define Wam_Words_To_KBytes(ww)    (ww * sizeof(WamWord) / 1024)

#include "w32gc_interf.h"


/* from linedit.h */

extern char *Pl_LE_Get_Separators(void);
extern int Pl_LE_Get_Prompt_Length(void);


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static void Start_GUI(int silent);

#ifdef GUI_CONSOLE_WITH_STACK_SIZES
static PlLong Query_Stack(QueryStackCmd cmd, int stack_no);
#endif

/* overwrite to customize linedit */

void (*pl_le_hook_start) () = Start_GUI;

void (*pl_le_hook_emit_beep) ();
void (*pl_le_hook_put_char) ();
int (*pl_le_hook_get_char0) ();
void (*pl_le_hook_ins_mode) ();

void (*pl_le_hook_screen_size) ();
int (*pl_le_hook_kbd_is_not_empty) ();

void (*pl_le_hook_backd) ();
void (*pl_le_hook_forwd) ();
void (*pl_le_hook_displ) ();
void (*pl_le_hook_displ_str) ();
void (*pl_le_hook_erase) ();
void (*le_hook_confirm) ();

void (*pl_le_hook_set_line_buffering) ();
int (*pl_le_hook_get_line_buffering) ();
void (*pl_le_hook_flush) ();

int (*pl_le_hook_confirm_box) ();
void (*pl_le_hook_message_box) ();
void (*pl_le_hook_exit_process) ();




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static FARPROC Find_Fct(HANDLE h, char *name);

#ifdef __GNUC__
typedef int (*Fct) ();
#else
typedef __declspec(dllimport) int (*Fct) ();
#endif

/*-------------------------------------------------------------------------*
 * START_GUI                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Start_GUI(int silent)
{
  Fct W32GC_Start_Window;
  HANDLE h;

#if 1 /* O to force console mode */
  h = LoadLibrary(DLL_W32GUICONS);
#else
  h = NULL;
#endif

  if (h == NULL)
    {
      if (!silent)
	fprintf(stderr, "warning: cannot load DLL " DLL_W32GUICONS
		" - text console used (error: %d)\n", (int) GetLastError());
      pl_le_hook_start = NULL;
      return;
    }
  pl_le_hook_put_char = (void (*)()) Find_Fct(h, "_W32GC_Put_Char");
  pl_le_hook_get_char0 = (int (*)()) Find_Fct(h, "_W32GC_Get_Char0");
  pl_le_hook_kbd_is_not_empty = (int (*)()) Find_Fct(h, "_W32GC_Kbd_Is_Not_Empty");
  pl_le_hook_screen_size = (void (*)()) Find_Fct(h, "_W32GC_Screen_Size");

  /* the following are not mandatory for linedit and could be commented out */

  pl_le_hook_emit_beep = (void (*)()) Find_Fct(h, "_W32GC_Emit_Beep");
  pl_le_hook_ins_mode = (void (*)()) Find_Fct(h, "_W32GC_Ins_Mode");
  pl_le_hook_backd = (void (*)()) Find_Fct(h, "_W32GC_Backd");
  pl_le_hook_forwd = (void (*)()) Find_Fct(h, "_W32GC_Forwd");
  pl_le_hook_displ = (void (*)()) Find_Fct(h, "_W32GC_Displ");
  pl_le_hook_displ_str = (void (*)()) Find_Fct(h, "_W32GC_Displ_Str");
  pl_le_hook_erase = (void (*)()) Find_Fct(h, "_W32GC_Erase");

  /* the following are not used by linedit but by stream_supp.c */

  pl_le_hook_set_line_buffering = (void (*)()) Find_Fct(h, "_W32GC_Set_Line_Buffering");
  pl_le_hook_get_line_buffering = (int (*)()) Find_Fct(h, "_W32GC_Get_Line_Buffering");
  pl_le_hook_flush = (void (*)()) Find_Fct(h, "_W32GC_Flush");

  pl_le_hook_confirm_box = (int (*)()) Find_Fct(h, "_W32GC_Confirm_Box");
  pl_le_hook_message_box = (void (*)()) Find_Fct(h, "_W32GC_Message_Box");
  pl_le_hook_exit_process = (void (*)()) Find_Fct(h, "_W32GC_Exit_Process");

  W32GC_Start_Window = (Fct) Find_Fct(h, "_W32GC_Start_Window");
  (*W32GC_Start_Window) (Pl_LE_Get_Separators, Pl_LE_Get_Prompt_Length,
#ifdef GUI_CONSOLE_WITH_STACK_SIZES
                         Query_Stack
#else
                         NULL
#endif
                         );
}



/*-------------------------------------------------------------------------*
 * FIND_FCT                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static FARPROC
Find_Fct(HANDLE h, char *name)
{
  FARPROC p;			/* init here to avoid lcc (buggy) warning */

  p = GetProcAddress(h, name + 1);

  if (p)
    return p;

  /* useless for MSVC++ but useful for lcc for instance */

  p = GetProcAddress(h, name);

  if (p == NULL)
    {
      fprintf(stderr,
	      "warning: cannot load fct %s in DLL " DLL_W32GUICONS "\n",
	      name);
      exit(1);
    }

  return p;
}


#ifdef GUI_CONSOLE_WITH_STACK_SIZES

/*-------------------------------------------------------------------------*
 * QUERY_STACK                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static PlLong
Query_Stack(QueryStackCmd cmd, int stack_no)
{
  switch(cmd)
    {
    case QUERY_STACK_GET_NB_OF_STACKS:
      return (PlLong) NB_OF_STACKS + 1; /* +1 for max_atom */

    case QUERY_STACK_HAS_FIXED_SIZES:
      return (PlLong) pl_fixed_sizes;

    case QUERY_STACK_GET_NAME:
      if (stack_no == NB_OF_STACKS)
	return (PlLong) "atoms";
      return (PlLong) pl_stk_tbl[stack_no].name;

    case QUERY_STACK_GET_DESC:
      if (stack_no == NB_OF_STACKS)
	return (PlLong) "atom table";
      return (PlLong) pl_stk_tbl[stack_no].desc;

    case QUERY_STACK_GET_ENV_VAR_NAME:
      if (stack_no == NB_OF_STACKS)
	return (PlLong) ENV_VAR_MAX_ATOM;
      return (PlLong) pl_stk_tbl[stack_no].env_var_name;

    case QUERY_STACK_GET_DEFAULT_SIZE:
      if (stack_no == NB_OF_STACKS)
	return (PlLong) DEFAULT_MAX_ATOM;
      return (PlLong) Wam_Words_To_KBytes(pl_stk_tbl[stack_no].default_size);

    case QUERY_STACK_GET_SIZE:
      if (stack_no == NB_OF_STACKS)
	return (PlLong) pl_max_atom;
      return (PlLong) Wam_Words_To_KBytes(pl_stk_tbl[stack_no].size);
    }

  return 0;
}

#endif  /* GUI_CONSOLE_WITH_STACK_SIZES */
