/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : ctrl_c.c                                                        *
 * Descr.: Ctrl+C management                                               *
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
#include <signal.h>
#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#endif

#if defined(_WIN32) && !defined(__CYGWIN__)
#define WIN32_CONSOLE_CTRL_HANDLER
#endif


#ifdef WIN32_CONSOLE_CTRL_HANDLER
#include <windows.h>
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

static long (*ctrl_c_handler) ();

#ifdef WIN32_CONSOLE_CTRL_HANDLER
static HANDLE event_ctrl_handler_exited;
#endif

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/



/*-------------------------------------------------------------------------*
 * WRAPPER_HANDLER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
#ifdef WIN32_CONSOLE_CTRL_HANDLER
static BOOL WINAPI
Wrapper_Handler(DWORD sig)
#else
static void
Wrapper_Handler(int sig)
#endif
{
#if defined(__unix__) || defined(__CYGWIN__)
  sigset_t set;
#endif

  static int inside_ctrl_c;

  if (inside_ctrl_c)
    {
      printf("Already in a Ctrl+C handler - ignored\n");
      fflush(stdout);
      return;
    }
  inside_ctrl_c = 1;

  (*ctrl_c_handler) (0);

#if defined(__unix__) || defined(__CYGWIN__)
  sigemptyset(&set);
  sigaddset(&set, sig);
  sigprocmask(SIG_UNBLOCK, &set, NULL);
#elif !defined(WIN32_CONSOLE_CTRL_HANDLER)
  signal(sig, Wrapper_Handler);
#endif

  inside_ctrl_c = 0;

#ifdef WIN32_CONSOLE_CTRL_HANDLER
  SetEvent(event_ctrl_handler_exited);
  return TRUE;
#endif
}




/*-------------------------------------------------------------------------*
 * EMIT_CTRL_C                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
long
Emit_Ctrl_C(void)
{
#if 1

  return (*ctrl_c_handler)(1);

#else

#if defined(__unix__) || defined(__CYGWIN__)

  kill(getpid(), SIGINT);

#elif defined(_WIN32) && !defined(WIN32_CONSOLE_CTRL_HANDLER)

  raise(SIGINT);

#elif defined(WIN32_CONSOLE_CTRL_HANDLER)

  GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
  WaitForSingleObject(event_ctrl_handler_exited, INFINITE);

#else

  printf("don't know how to send a Ctrl+C\n");

#endif

#endif
}




/*-------------------------------------------------------------------------*
 * INSTALL_CTRL_C_HANDLER                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Install_Ctrl_C_Handler(long (*handler) (int))
{
  ctrl_c_handler = handler;
#ifdef WIN32_CONSOLE_CTRL_HANDLER
  event_ctrl_handler_exited = CreateEvent(NULL, FALSE, FALSE, NULL);
  SetConsoleCtrlHandler(Wrapper_Handler, TRUE);
#else
  signal(SIGINT, Wrapper_Handler);
#endif
}









