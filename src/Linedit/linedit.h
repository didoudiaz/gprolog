/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : linedit.h                                                       *
 * Descr.: line editor - header file                                       *
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

int le_hook_present;

void (*le_hook_emit_beep) ();
void (*le_hook_put_char) ();
int  (*le_hook_get_char0) ();
void (*le_hook_ins_mode) ();

void (*le_hook_screen_size) ();
int  (*le_hook_kbd_is_not_empty) ();

void (*le_hook_backd) ();
void (*le_hook_forwd) ();
void (*le_hook_displ) ();
void (*le_hook_displ_str) ();
void (*le_hook_erase) ();

#ifdef LE_DEFINE_HOOK_MACROS

#define EMIT_BEEP(fd_out)         ((*le_hook_emit_beep)(fd_out))
#define PUT_CHAR(c, fd_out)       ((*le_hook_put_char)(c, fd_out))
#define GET_CHAR0(fd_in)          ((*le_hook_get_char0)(fd_in))
#define INS_MODE(ins_mode)        ((*le_hook_ins_mode)(ins_mode))

#define SCREEN_SIZE(fd_out, r, c) ((*le_hook_screen_size)(fd_out, r, c))
#define KBD_IS_NOT_EMPTY(fd_in)   ((*le_hook_kbd_is_not_empty)(fd_in))

#define BACKD(fd_out, n)          ((*le_hook_backd)(fd_out, n))
#define FORWD(fd_out, n, str)     ((*le_hook_forwd)(fd_out, n, str))
#define DISPL(fd_out, n, str)     ((*le_hook_displ)(fd_out, n, str))
#define DISPL_STR(fd_out, str)    ((*le_hook_displ_str)(fd_out, str))
#define ERASE(fd_out, n)          ((*le_hook_erase)(fd_out, n))

#endif


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

char *LE_Gets(char *str);

char *LE_FGets(char *str, int size, int fd_in, int fd_out,
	       char *prompt, int display_prompt);

void LE_Get_Current_Word(char *word);

char *LE_Get_Separators(void);

char *LE_Set_Separators(char *sep_str);

char *LE_Compl_Add_Word(char *word, int word_length);

char *LE_Compl_Del_Word(char *word);

char *LE_Compl_Init_Match(char *prefix, int *nb_match, int *max_lg);

char *LE_Compl_Find_Match(int *is_last);

int LE_FGetc_No_Echo(int fd_in, int fd_out);

int LE_Printf(char *format, ...);
