/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : ctrl_c.c                                                        *
 * Descr.: Ctrl+C management                                               *
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


#include "../EnginePl/gp_config.h"

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

static PlLong (*ctrl_c_handler) ();
static int from_callback;
static PlLong ret_val;
static int inside_ctrl_c;

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

  int from_callback1;

#if defined(__unix__) || defined(__CYGWIN__)
  sigemptyset(&set);
  sigaddset(&set, sig);
  sigprocmask(SIG_UNBLOCK, &set, NULL);
#elif !defined(WIN32_CONSOLE_CTRL_HANDLER)
  signal(sig, Wrapper_Handler);
#endif

  if (inside_ctrl_c)
    {
      printf("Already in a Ctrl+C handler - ignored\n");
      fflush(stdout);
      ret_val = 0;
    }
  else
    {
      inside_ctrl_c = from_callback; /* only 1 if from_callback */

      from_callback1 = from_callback;
      from_callback = 0;
      ret_val = (*ctrl_c_handler) (from_callback1);
    }


  inside_ctrl_c = 0;

#ifdef WIN32_CONSOLE_CTRL_HANDLER
  SetEvent(event_ctrl_handler_exited);
  return TRUE;
#endif
}




/*-------------------------------------------------------------------------*
 * PL_EMIT_CTRL_C                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlLong
Pl_Emit_Ctrl_C(void)
{
  from_callback = 1;

#if 1

#if defined(__unix__) || defined(__CYGWIN__)

  kill(getpid(), SIGINT);

#elif defined(_WIN32) && !defined(WIN32_CONSOLE_CTRL_HANDLER)

  raise(SIGINT);

#elif defined(WIN32_CONSOLE_CTRL_HANDLER)

  if (!inside_ctrl_c)
    {
      GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
      WaitForSingleObject(event_ctrl_handler_exited, INFINITE);
    }

#else

  printf("don't know how to send a Ctrl+C\n");

#endif
  return ret_val;

#else  /* pb: CTRL+C under linedit does not generate a signal
	* i.e. problem under gdb to debug */

  return (*ctrl_c_handler)(1);

#endif

}




/*-------------------------------------------------------------------------*
 * PL_INSTALL_CTRL_C_HANDLER                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Install_Ctrl_C_Handler(PlLong (*handler) (int))
{
  ctrl_c_handler = handler;
#ifdef WIN32_CONSOLE_CTRL_HANDLER
  event_ctrl_handler_exited = CreateEvent(NULL, FALSE, FALSE, NULL);
  SetConsoleCtrlHandler(Wrapper_Handler, TRUE);
#else
  signal(SIGINT, Wrapper_Handler);
#endif
}
