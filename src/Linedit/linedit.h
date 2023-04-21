/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : linedit.h                                                       *
 * Descr.: line editor - header file                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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

#ifndef _LINEDIT_H
#define _LINEDIT_H

#include "completion.h"


/* Windows uses 2 codepages (which give the meaning of 0x80..0xFF chars):
 * "OEM codepages" for console programs and "ANSI codepages" for GUI programs.
 * For instance 'é' (\'e) is returned as 130 in OEM (with codepage 850) and
 * as 233 in ANSI. The problem is that isalpha(130) is false...
 * I use 2 Win32 functions: OemToChar() (when reading) and CharToOem()
 * (when writing)... */

#if 1
#define WIN32_CONVERT_OEM_ASCII
#endif

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

enum {
  LE_MODE_DEACTIVATED = 0,	/* linedit is deactivated */
  LE_MODE_TTY,			/* linedit runs in console mode */
  LE_MODE_HOOK			/* linedit runs via a hook (i.e. GUI) */
};




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/* overwritten if needed to customize linedit */

void (*pl_le_hook_start) (int silent);

				/* is it mandatory to define a hook ? */
void (*pl_le_hook_put_char) (int c);	/* mandatory */
int (*pl_le_hook_get_char0) (void);	/* mandatory */
void (*pl_le_hook_emit_beep) (void);
void (*pl_le_hook_ins_mode) (int ins_mode);

void (*pl_le_hook_screen_size) (int *row, int *col);	/* mandatory */
int (*pl_le_hook_kbd_is_not_empty) (void);	/* mandatory */

void (*pl_le_hook_backd) (int n);
void (*pl_le_hook_forwd) (int n, char *str);
void (*pl_le_hook_displ) (int n, char *str);
void (*pl_le_hook_displ_str) (char *str);
void (*pl_le_hook_erase) (int n);

				/* functions not used by linedit itself */
void (*pl_le_hook_set_line_buffering) (int is_buffered);
int (*pl_le_hook_get_line_buffering) (void);
void (*pl_le_hook_flush) (FILE *f);

int (*pl_le_hook_confirm_box) (char *title, char *msg);
void (*pl_le_hook_message_box) (char *title, char *msg, int type);
void (*pl_le_hook_exit_process) (int ret_val);


#ifdef LE_DEFINE_HOOK_MACROS

#define EMIT_BEEP           ((*pl_le_hook_emit_beep)())
#define PUT_CHAR(c)         ((*pl_le_hook_put_char)(c))
#define GET_CHAR0           ((*pl_le_hook_get_char0)())
#define INS_MODE(ins_mode)  ((*pl_le_hook_ins_mode)(ins_mode))

#define SCREEN_SIZE(r, c)   ((*pl_le_hook_screen_size)(r, c))
#define KBD_IS_NOT_EMPTY    ((*pl_le_hook_kbd_is_not_empty)())

#define BACKD(n)            ((*pl_le_hook_backd)((int) (n)))
#define FORWD(n, str)       ((*pl_le_hook_forwd)((int) (n), str))
#define DISPL(n, str)       ((*pl_le_hook_displ)((int) (n), str))
#define DISPL_STR(str)      ((*pl_le_hook_displ_str)(str))
#define ERASE(n)            ((*pl_le_hook_erase)((int) (n)))

#endif


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Pl_LE_Initialize(void);


char *Pl_LE_Gets(char *str);

char *Pl_LE_FGets(char *str, int size, char *prompt, int display_prompt);


PlLong Pl_LE_Get_Ctrl_C_Return_Value(void);

#define LE_Interrupted_By_Ctrl_C(r)  ((PlLong) r == (PlLong) -2)


int Pl_LE_Get_Prompt_Length(void);

int Pl_LE_Get_Current_Position(void);

void Pl_LE_Get_Current_Word(char *word);

char *Pl_LE_Get_Separators(void);

char *Pl_LE_Set_Separators(char *sep_str);

char *Pl_LE_Adjust_For_Completion(char *word);


int Pl_LE_Get_Key(int echo, int catch_ctrl_c);


int Pl_LE_Printf(char *format, ...);



#ifdef TERMINAL_FILE

int (*pl_le_initialize)() = Pl_LE_Initialize;

#else

int (*pl_le_initialize)();

#endif

#endif	/* linedit.h */
