/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Win32 GUI console                                               *
 * File  : w32gc_interf.c                                                  *
 * Descr.: line editor <--> GUICons interface                              *
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
#include <windows.h>


#include "../EnginePl/gp_config.h"


/* from linedit.h */

extern char *LE_Get_Separators(void);
extern int LE_Get_Prompt_Length(void);


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static void Start_GUI(void);

/* overwrite to customize linedit */

void (*le_hook_start) () = Start_GUI;

void (*le_hook_emit_beep) ();
void (*le_hook_put_char) ();
int (*le_hook_get_char0) ();
void (*le_hook_ins_mode) ();

void (*le_hook_screen_size) ();
int (*le_hook_kbd_is_not_empty) ();

void (*le_hook_backd) ();
void (*le_hook_forwd) ();
void (*le_hook_displ) ();
void (*le_hook_displ_str) ();
void (*le_hook_erase) ();
void (*le_hook_confirm) ();

void (*le_hook_set_line_buffering) ();
int (*le_hook_get_line_buffering) ();
void (*le_hook_flush) ();

int (*le_hook_confirm_box) ();
void (*le_hook_message_box) ();
void (*le_hook_exit_process) ();




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static FARPROC Find_Fct(HANDLE h, char *name);


typedef
__declspec(dllimport)
     int (*Fct) ();


/*-------------------------------------------------------------------------*
 * START_GUI                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
     static void Start_GUI(void)
{
  Fct W32GC_Start_Window;
  HANDLE h;

#if 1
  h = LoadLibrary(DLL_W32GUICONS);
#else
  h = NULL;
#endif

  if (h == NULL)
    {
      fprintf(stderr,
	      "warning: cannot load DLL " DLL_W32GUICONS
	      " - text console used\n");
      le_hook_start = NULL;
      return;
    }

  le_hook_put_char = (void (*)()) Find_Fct(h, "_W32GC_Put_Char");
  le_hook_get_char0 = (int (*)()) Find_Fct(h, "_W32GC_Get_Char0");
  le_hook_kbd_is_not_empty =
    (int (*)()) Find_Fct(h, "_W32GC_Kbd_Is_Not_Empty");
  le_hook_screen_size = (void (*)()) Find_Fct(h, "_W32GC_Screen_Size");

  /* the following are not mandatory for linedit and could be commented out */

  le_hook_emit_beep = (void (*)()) Find_Fct(h, "_W32GC_Emit_Beep");
  le_hook_ins_mode = (void (*)()) Find_Fct(h, "_W32GC_Ins_Mode");
  le_hook_backd = (void (*)()) Find_Fct(h, "_W32GC_Backd");
  le_hook_forwd = (void (*)()) Find_Fct(h, "_W32GC_Forwd");
  le_hook_displ = (void (*)()) Find_Fct(h, "_W32GC_Displ");
  le_hook_displ_str = (void (*)()) Find_Fct(h, "_W32GC_Displ_Str");
  le_hook_erase = (void (*)()) Find_Fct(h, "_W32GC_Erase");

  /* the following are not used by linedit but by stream_supp.c */

  le_hook_set_line_buffering =
    (void (*)()) Find_Fct(h, "_W32GC_Set_Line_Buffering");
  le_hook_get_line_buffering =
    (int (*)()) Find_Fct(h, "_W32GC_Get_Line_Buffering");
  le_hook_flush = (void (*)()) Find_Fct(h, "_W32GC_Flush");

  le_hook_confirm_box = (int (*)()) Find_Fct(h, "_W32GC_Confirm_Box");
  le_hook_message_box = (void (*)()) Find_Fct(h, "_W32GC_Message_Box");
  le_hook_exit_process = (void (*)()) Find_Fct(h, "_W32GC_Exit_Process");

  W32GC_Start_Window = (Fct) Find_Fct(h, "_W32GC_Start_Window");
  (*W32GC_Start_Window) (LE_Get_Separators, LE_Get_Prompt_Length);
}



/*-------------------------------------------------------------------------*
 * FIND_FCT                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static FARPROC
Find_Fct(HANDLE h, char *name)
{
  FARPROC p = GetProcAddress(h, name + 1);

  if (p)
    return p;

  /* use necessary for MSVC++ but useful for lcc for instance */

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
