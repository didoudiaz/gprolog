/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : stty.h                                                          *
 * Descr.: basic terminal operations - header file                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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


#if 0
#define DEBUG
#endif


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


#define KEY_BACKSPACE              '\b'
#define KEY_DELETE                 0x7f

#define KEY_ID(code)               KEY_ID2(KEY_MODIF_NONE, code)
#define KEY_ID2(modif, code)       (((modif) << 9) | ((1 << 8) | ((code) & 0x7f)))

#define GET_MODIF(x)               (((x) >> 9) & 7)
#define GET_CODE(x)                ((x) & 0x1ff)


#define KEY_CTRL(x)                ((x) & 0x1f)

#define KEY_ESC(x)                 KEY_ID((1 << 7) | (x) | 0x20) /* 0x20 to be case insensitive */
#define IS_ESC_COMB(x)             ((x) & (1 << 7))
#define GET_ESC_COMB(x)            ((x) & 0x7f)



#define KEY_MODIF_NONE             0  /* modifiers (additive) */
#define KEY_MODIF_SHIFT            1
#define KEY_MODIF_ALT              2
#define KEY_MODIF_CTRL             4

#if defined(__unix__) || defined(__CYGWIN__)	/* Unix */
#define KEY_EXT_FCT_1              KEY_ID(11)
#define KEY_EXT_FCT_2              KEY_ID(12)
#define KEY_EXT_FCT_3              KEY_ID(13)
#define KEY_EXT_FCT_4              KEY_ID(14)
#define KEY_EXT_FCT_5              KEY_ID(15)

#define KEY_EXT_FCT_6              KEY_ID(17)
#define KEY_EXT_FCT_7              KEY_ID(18)
#define KEY_EXT_FCT_8              KEY_ID(19)
#define KEY_EXT_FCT_9              KEY_ID(20)
#define KEY_EXT_FCT_10             KEY_ID(21)

#define KEY_EXT_FCT_11             KEY_ID(23)
#define KEY_EXT_FCT_12             KEY_ID(24)

#define KEY_EXT_UP                 KEY_ID('A')
#define KEY_EXT_DOWN               KEY_ID('B')
#define KEY_EXT_RIGHT              KEY_ID('C')
#define KEY_EXT_LEFT               KEY_ID('D')

#define KEY_EXT_HOME               KEY_ID('H')
#define KEY_EXT_END                KEY_ID('F')
#define KEY_EXT_PAGE_UP            KEY_ID(5)
#define KEY_EXT_PAGE_DOWN          KEY_ID(6)
#define KEY_EXT_INSERT             KEY_ID(2)
#define KEY_EXT_DELETE             KEY_ID(3)

#elif defined(_WIN32)		/* Win32 */

#include <windows.h>

#define KEY_EXT_FCT_1              KEY_ID(VK_F1)
#define KEY_EXT_FCT_2              KEY_ID(VK_F2)
#define KEY_EXT_FCT_3              KEY_ID(VK_F3)
#define KEY_EXT_FCT_4              KEY_ID(VK_F4)
#define KEY_EXT_FCT_5              KEY_ID(VK_F5)

#define KEY_EXT_FCT_6              KEY_ID(VK_F6)
#define KEY_EXT_FCT_7              KEY_ID(VK_F7)
#define KEY_EXT_FCT_8              KEY_ID(VK_F8)
#define KEY_EXT_FCT_9              KEY_ID(VK_F9)
#define KEY_EXT_FCT_10             KEY_ID(VK_F10)

#define KEY_EXT_FCT_11             KEY_ID(VK_F11)
#define KEY_EXT_FCT_12             KEY_ID(VK_F12)

#define KEY_EXT_UP                 KEY_ID(VK_UP)
#define KEY_EXT_DOWN               KEY_ID(VK_DOWN)
#define KEY_EXT_RIGHT              KEY_ID(VK_RIGHT)
#define KEY_EXT_LEFT               KEY_ID(VK_LEFT)

#define KEY_EXT_HOME               KEY_ID(VK_HOME)
#define KEY_EXT_END                KEY_ID(VK_END)
#define KEY_EXT_PAGE_UP            KEY_ID(VK_PRIOR)
#define KEY_EXT_PAGE_DOWN          KEY_ID(VK_NEXT)
#define KEY_EXT_INSERT             KEY_ID(VK_INSERT)
#define KEY_EXT_DELETE             KEY_ID(VK_DELETE)

#endif


#if defined(_WIN32)
#define KEY_IS_EOF(c)   ((c) == KEY_CTRL('D') || (c) == KEY_CTRL('Z'))
#else
#define KEY_IS_EOF(c)   ((c) == KEY_CTRL('D'))
#endif




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Pl_LE_Open_Terminal(void);

void Pl_LE_Close_Terminal(void);

void Pl_LE_Screen_Size(int *row, int *col);

void Pl_LE_Ins_Mode(int ins_mode);

int Pl_LE_Kbd_Is_Not_Empty(void);

int Pl_LE_Is_Interrupt_Key(int c);

void Pl_LE_Emit_Beep(void);

void Pl_LE_Put_Char(int c);

int Pl_LE_Get_Char(void);

#ifdef DEBUG
void Debug_Printf(char *fmt, ...) ATTR_PRINTF(1);
void Debug_Check_Positions(int lin_pos);
#else
#define Debug_Printf(fmt, ...)
#define Debug_Check_Positions(lin_pos)
#endif

