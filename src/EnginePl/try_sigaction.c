/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : configuration                                                   *
 * File  : try_sigaction.c                                                 *
 * Descr.: Detection of working sigaction                                  *
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


#define _XOPEN_SOURCE 700
/* #define _GNU_SOURCE  */ /* see /usr/include/features.h */
#define _XOPEN_SOURCE_EXTENDED

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>

/*#include <sys/siginfo.h>*/

/* chose an address ending by 0 (else can trigger a SIGBUS, and on some archs (sparc/OpenBSD)
 * si_addr is wrong for SIGBUS :-(
 */
#define BAD_ADDR  ((int *) 0x2EA4F0)

void
SIGSEGV_Handler(int sig, siginfo_t * sip)
{
  int *addr = (int *) sip->si_addr;
#if 0
  printf("bad addr: %p\n", addr);
#endif
  _exit(addr != BAD_ADDR);
}



int
main(int argc, char *argv[])
{
  struct sigaction act;

  act.sa_handler = NULL;
  act.sa_sigaction = (void (*)()) SIGSEGV_Handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO | SA_RESTART;

  sigaction(SIGSEGV, &act, NULL);
#if defined(SIGBUS) && SIGBUS != SIGSEGV
  sigaction(SIGBUS, &act, NULL);
#endif

  *BAD_ADDR = 128;

  return 1;
}
