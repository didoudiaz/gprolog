/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Win32 GUI console                                               *
 * File  : w32gc_interf.h                                                  *
 * Descr.: line editor <--> GUICons interface - header file                *
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

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

/* hooks for linedit */

void W32GC_Emit_Beep(int fd_out);

void W32GC_Put_Char(int c, int fd_out);

int W32GC_Get_Char0(int fd_in);

void W32GC_Ins_Mode(int ins_mode);


void W32GC_Screen_Size(int fd_out, int *row, int *col);

int W32GC_Kbd_Is_Not_Empty(int fd_in);


void W32GC_Backd(int fd_out, int n);

void W32GC_Forwd(int fd_out, int n);

void W32GC_Displ(int fd_out, int n, char *str);

void W32GC_Displ_Str(int fd_out, char *str);

void W32GC_Erase(int fd_out, int n);


/* Other utility functions */

void W32GC_Msg(char *s);

void W32GC_Set_Title(char *title);

void W32GC_Restart_Exit_Msg(char *msg);

void W32GC_Restart(void);

void W32GC_Adjust_Stack_Sizes(int *s1, int *s2, int *s3, int *s4);
