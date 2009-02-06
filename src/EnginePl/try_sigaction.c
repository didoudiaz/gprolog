/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : configuration                                                   *
 * File  : try_sigaction.c                                                 *
 * Descr.: Detection of working sigaction                                  *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2009 Daniel Diaz                                     *
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
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#define BAD_ADDR  0xFEA4F

void
SIGSEGV_Handler(int sig, siginfo_t * sip)
{
  long addr = (long) sip->si_addr;

  exit ((addr == BAD_ADDR) ? 0 : 1);
}



int
main(int argc, char *argv[])
{
  struct sigaction act;

  act.sa_handler = NULL;
  act.sa_sigaction = (void (*)()) SIGSEGV_Handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;

  sigaction(SIGSEGV, &act, NULL);

  * (long *) BAD_ADDR = 128;

  return 1;
}
