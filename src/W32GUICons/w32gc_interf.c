/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Win32 GUI console                                               */
/* File  : w32gc_interf.c                                                  */
/* Descr.: line editor <--> GUICons interface                              */
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
#include "w32gc_interf.h"

/*---------------------------------*/
/* Constants                       */
/*---------------------------------*/

/*---------------------------------*/
/* Type Definitions                */
/*---------------------------------*/

/*---------------------------------*/
/* Global Variables                */
/*---------------------------------*/

/*---------------------------------*/
/* Function Prototypes             */
/*---------------------------------*/


                                               /* overwrite var of linedit */

int    w32gc_present               =1;
void (*w32gc_screen_size)        ()=W32GC_Screen_Size;
int  (*w32gc_kbd_is_not_empty)   ()=W32GC_Kbd_Is_Not_Empty;
void (*w32gc_emit_beep)          ()=W32GC_Emit_Beep;
void (*w32gc_msg)                ()=W32GC_Msg;
void (*w32gc_ins_mode)           ()=W32GC_Ins_Mode;
int  (*w32gc_get_char0)          ()=W32GC_Get_Char0;
void (*w32gc_put_char)           ()=W32GC_Put_Char;
void (*w32gc_erase)              ()=W32GC_Erase;
void (*w32gc_backd)              ()=W32GC_Backd;
void (*w32gc_forwd)              ()=W32GC_Forwd;
void (*w32gc_displ)              ()=W32GC_Displ;
void (*w32gc_display_string)     ()=W32GC_Display_String;
void (*w32gc_set_title)		     ()=W32GC_Set_Title;
void (*w32gc_restart_exit_msg)   ()=W32GC_Restart_Exit_Msg;
void (*w32gc_restart)		     ()=W32GC_Restart;
void (*w32gc_adjust_stack_sizes) ()=W32GC_Adjust_Stack_Sizes;