/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : line-edit library                                               */
/* File  : linedit.h                                                       */
/* Descr.: line editor - header file                                       */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/

/*---------------------------------*/
/* Constants                       */
/*---------------------------------*/

/*---------------------------------*/
/* Type Definitions                */
/*---------------------------------*/

/*---------------------------------*/
/* Global Variables                */
/*---------------------------------*/

                              /* overwritten by W32 GUI console if present */

int    w32gc_present;
void (*w32gc_screen_size)       (); 
int  (*w32gc_kbd_is_not_empty)  ();
void (*w32gc_emit_beep)         ();
void (*w32gc_msg)               ();
void (*w32gc_ins_mode)          (); 
int  (*w32gc_get_char0)         ();
void (*w32gc_put_char)          ();
void (*w32gc_erase)             ();
void (*w32gc_backd)             (); 
void (*w32gc_forwd)             (); 
void (*w32gc_displ)             (); 
void (*w32gc_display_string)    (); 
void (*w32gc_set_title)	        ();
void (*w32gc_restart_exit_msg)  ();
void (*w32gc_restart)           ();
void (*w32gc_adjust_stack_sizes)();




/*---------------------------------*/
/* Function Prototypes             */
/*---------------------------------*/

char     *LE_Gets               (char *str);
char     *LE_FGets              (char *str,int size,int fd_in,int fd_out,
                                 char *prompt,int display_prompt);

void      LE_Get_Current_Word   (char *word);
char     *LE_Get_Separators     (void);
char     *LE_Set_Separators     (char *sep_str);

char     *LE_Compl_Add_Word     (char *word,int word_length);
char     *LE_Compl_Del_Word     (char *word);
char     *LE_Compl_Init_Match   (char *prefix,int *nb_match,int *max_lg);
char     *LE_Compl_Find_Match   (int *is_last);

int       LE_FGetc_No_Echo      (int fd_in,int fd_out);

int       LE_Printf             (char *format,...);






