/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : stty.h                                                          *
 * Descr.: basic terminal operations - header file                         *
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


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


#define KEY_CTRL(x)                ((x) & 0x1f)

#define KEY_ESC(x)                 ((2<<8) | ((x)|0x20))

#define KEY_BACKSPACE              '\b'
#define KEY_DELETE                 0x7f


#if defined(__unix__) || defined(__CYGWIN__)	/* Unix */
#define KEY_EXT_FCT_1              ((1<<8) | 11)
#define KEY_EXT_FCT_2              ((1<<8) | 12)
#define KEY_EXT_FCT_3              ((1<<8) | 13)
#define KEY_EXT_FCT_4              ((1<<8) | 14)
#define KEY_EXT_FCT_5              ((1<<8) | 15)

#define KEY_EXT_FCT_6              ((1<<8) | 17)
#define KEY_EXT_FCT_7              ((1<<8) | 18)
#define KEY_EXT_FCT_8              ((1<<8) | 19)
#define KEY_EXT_FCT_9              ((1<<8) | 20)
#define KEY_EXT_FCT_10             ((1<<8) | 21)

#define KEY_EXT_FCT_11             ((1<<8) | 23)
#define KEY_EXT_FCT_12             ((1<<8) | 24)

#define KEY_EXT_UP                 ((1<<8) | 'A')
#define KEY_EXT_DOWN               ((1<<8) | 'B')
#define KEY_EXT_RIGHT              ((1<<8) | 'C')
#define KEY_EXT_LEFT               ((1<<8) | 'D')

#define KEY_EXT_HOME               ((1<<8) | 'H')
#define KEY_EXT_END                ((1<<8) | 'F')
#define KEY_EXT_PAGE_UP            ((1<<8) | 5)
#define KEY_EXT_PAGE_DOWN          ((1<<8) | 6)
#define KEY_EXT_INSERT             ((1<<8) | 2)
#define KEY_EXT_DELETE             ((1<<8) | 3)

#define KEY_CTRL_EXT_UP            ((1<<8) | 'a')
#define KEY_CTRL_EXT_DOWN          ((1<<8) | 'b')
#define KEY_CTRL_EXT_RIGHT         ((1<<8) | 'c')
#define KEY_CTRL_EXT_LEFT          ((1<<8) | 'd')

#define KEY_CTRL_EXT_HOME          ((1<<8) | 'e')
#define KEY_CTRL_EXT_END           ((1<<8) | 'f')
#define KEY_CTRL_EXT_PAGE_UP       ((1<<8) | 'g')
#define KEY_CTRL_EXT_PAGE_DOWN     ((1<<8) | 'h')
#define KEY_CTRL_EXT_INSERT        ((1<<8) | 'i')
#define KEY_CTRL_EXT_DELETE        ((1<<8) | 'j')

#elif defined(_WIN32)		/* Win32 */

#include <windows.h>

#define KEY_EXT_FCT_1              ((1<<8) | VK_F1)
#define KEY_EXT_FCT_2              ((1<<8) | VK_F2)
#define KEY_EXT_FCT_3              ((1<<8) | VK_F3)
#define KEY_EXT_FCT_4              ((1<<8) | VK_F4)
#define KEY_EXT_FCT_5              ((1<<8) | VK_F5)

#define KEY_EXT_FCT_6              ((1<<8) | VK_F6)
#define KEY_EXT_FCT_7              ((1<<8) | VK_F7)
#define KEY_EXT_FCT_8              ((1<<8) | VK_F8)
#define KEY_EXT_FCT_9              ((1<<8) | VK_F9)
#define KEY_EXT_FCT_10             ((1<<8) | VK_F10)

#define KEY_EXT_FCT_11             ((1<<8) | VK_F11)
#define KEY_EXT_FCT_12             ((1<<8) | VK_F12)

#define KEY_EXT_UP                 ((1<<8) | VK_UP)
#define KEY_EXT_DOWN               ((1<<8) | VK_DOWN)
#define KEY_EXT_RIGHT              ((1<<8) | VK_RIGHT)
#define KEY_EXT_LEFT               ((1<<8) | VK_LEFT)

#define KEY_EXT_HOME               ((1<<8) | VK_HOME)
#define KEY_EXT_END                ((1<<8) | VK_END)
#define KEY_EXT_PAGE_UP            ((1<<8) | VK_PRIOR)
#define KEY_EXT_PAGE_DOWN          ((1<<8) | VK_NEXT)
#define KEY_EXT_INSERT             ((1<<8) | VK_INSERT)
#define KEY_EXT_DELETE             ((1<<8) | VK_DELETE)

#define KEY_CTRL_EXT_UP            ((2<<8) | VK_UP)
#define KEY_CTRL_EXT_DOWN          ((2<<8) | VK_DOWN)
#define KEY_CTRL_EXT_RIGHT         ((2<<8) | VK_RIGHT)
#define KEY_CTRL_EXT_LEFT          ((2<<8) | VK_LEFT)

#define KEY_CTRL_EXT_HOME          ((2<<8) | VK_HOME)
#define KEY_CTRL_EXT_END           ((2<<8) | VK_END)
#define KEY_CTRL_EXT_PAGE_UP       ((2<<8) | VK_PRIOR)
#define KEY_CTRL_EXT_PAGE_DOWN     ((2<<8) | VK_NEXT)
#define KEY_CTRL_EXT_INSERT        ((2<<8) | VK_INSERT)
#define KEY_CTRL_EXT_DELETE        ((2<<8) | VK_DELETE)

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



