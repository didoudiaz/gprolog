/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Win32 GUI console                                               *
 * File  : w32gc_interf.c                                                  *
 * Descr.: line editor <--> GUICons interface                              *
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

#include "w32gc_interf.h"

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/* overwrite to customize linedit */

int le_hook_present = 1;

void (*le_hook_emit_beep) () = W32GC_Emit_Beep;
void (*le_hook_put_char) () = W32GC_Put_Char;
int  (*le_hook_get_char0) () = W32GC_Get_Char0;
void (*le_hook_ins_mode) () = W32GC_Ins_Mode;

void (*le_hook_screen_size) () = W32GC_Screen_Size;
int  (*le_hook_kbd_is_not_empty) () = W32GC_Kbd_Is_Not_Empty;

void (*le_hook_backd) () = W32GC_Backd;
void (*le_hook_forwd) () = W32GC_Forwd;
void (*le_hook_displ) () = W32GC_Displ;
void (*le_hook_displ_str) () = W32GC_Displ_Str;
void (*le_hook_erase) () = W32GC_Erase;


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

