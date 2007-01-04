/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : linedit.h                                                       *
 * Descr.: line editor - header file                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2007 Daniel Diaz                                     *
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
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/* $Id$ */


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

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/* overwritten if needed to customize linedit */

void (*le_hook_start) ();

				/* is it mandatory to define a hook ? */
void (*le_hook_put_char) ();	/* mandatory */
int (*le_hook_get_char0) ();	/* mandatory */
void (*le_hook_emit_beep) ();
void (*le_hook_ins_mode) ();

void (*le_hook_screen_size) ();	/* mandatory */
int (*le_hook_kbd_is_not_empty) ();	/* mandatory */

void (*le_hook_backd) ();
void (*le_hook_forwd) ();
void (*le_hook_displ) ();
void (*le_hook_displ_str) ();
void (*le_hook_erase) ();

				/* functions not used by linedit itself */
void (*le_hook_set_line_buffering) ();
int (*le_hook_get_line_buffering) ();
void (*le_hook_flush) ();
int (*le_hook_confirm_box) ();
void (*le_hook_message_box) ();
void (*le_hook_exit_process) ();


#ifdef LE_DEFINE_HOOK_MACROS

#define EMIT_BEEP           ((*le_hook_emit_beep)())
#define PUT_CHAR(c)         ((*le_hook_put_char)(c))
#define GET_CHAR0           ((*le_hook_get_char0)())
#define INS_MODE(ins_mode)  ((*le_hook_ins_mode)(ins_mode))

#define SCREEN_SIZE(r, c)   ((*le_hook_screen_size)(r, c))
#define KBD_IS_NOT_EMPTY    ((*le_hook_kbd_is_not_empty)())

#define BACKD(n)            ((*le_hook_backd)(n))
#define FORWD(n, str)       ((*le_hook_forwd)(n, str))
#define DISPL(n, str)       ((*le_hook_displ)(n, str))
#define DISPL_STR(str)      ((*le_hook_displ_str)(str))
#define ERASE(n)            ((*le_hook_erase)(n))

#endif


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int LE_Initialize(void);


char *LE_Gets(char *str);

char *LE_FGets(char *str, int size, char *prompt, int display_prompt);


long LE_Get_Ctrl_C_Return_Value(void);

#define LE_Interrupted_By_Ctrl_C(r)  ((long) r == -2L)


int LE_Get_Prompt_Length(void);

int LE_Get_Current_Position(void);

void LE_Get_Current_Word(char *word);

char *LE_Get_Separators(void);

char *LE_Set_Separators(char *sep_str);



char *LE_Compl_Add_Word(char *word, int word_length);

char *LE_Compl_Del_Word(char *word);

char *LE_Compl_Init_Match(char *prefix, int *nb_match, int *max_lg);

char *LE_Compl_Find_Match(int *is_last);


int LE_Get_Key(int echo, int catch_ctrl_c);


int LE_Printf(char *format, ...);



#ifdef TERMINAL_FILE

int (*le_initialize)() = LE_Initialize;

#else

int (*le_initialize)();

#endif
