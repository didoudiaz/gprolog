/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : consult_c.c                                                     *
 * Descr.: file consulting - C part                                        *
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

#include <sys/types.h>

#include "engine_pl.h"
#include "bips_pl.h"

#if defined(M_ix86_cygwin) || defined(M_ix86_win32)
#include <process.h>
#endif

#ifdef M_ix86_win32
#include <io.h>
#include <fcntl.h>
#else
#include <unistd.h>
#include <sys/wait.h>
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

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#if 1




/*-------------------------------------------------------------------------*
 * CONSULT_2                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Consult_2(WamWord tmp_file_word, WamWord pl_file_word)
{
  char *tmp_file = Rd_String_Check(tmp_file_word);
  char *pl_file = Rd_String_Check(pl_file_word);
  char *singl_warn = (Flag_Value(FLAG_SINGLETON_WARNING)) ? NULL

    : "--no-singl-warn";
  StmInf *pstm = stm_tbl + stm_top_level_output;
  int pid;
  FILE *f_out;
  int status, c;
  char *arg[] = { "pl2wam", "-w", "--compile-msg", "--no-redef-error",
    "--pl-state", tmp_file, "-o", tmp_file, pl_file,
    singl_warn, NULL
  };

  Flush_All_Streams();
  pid = M_Spawn_Redirect(arg, 0, NULL, &f_out, &f_out);
  Os_Test_Error(pid == -1);
  if (pid == -2)
    {
    error_pl2wam:
      Pl_Err_System(Create_Atom("error trying to execute pl2wam "
				"(maybe not found)"));
      return FALSE;
    }

  for (;;)
    {
      c = fgetc(f_out);
      if (c < 0)
	break;

      Stream_Putc(c, pstm);
    }

  fclose(f_out);

  status = M_Get_Status(pid);
  if (status < 0)
    goto error_pl2wam;

  return status == 0;
}

#endif
