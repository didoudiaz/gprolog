/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : consult_c.c                                                     *
 * Descr.: file consulting - C part                                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
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
#include "linedit.h"

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
  StmInf *pstm_o = stm_tbl[stm_top_level_output];
  StmInf *pstm_i = stm_tbl[stm_top_level_input];
  int pid;
  FILE *f_out, *f_in;
  FILE **pf_in;
  long save;
  unsigned char *p = NULL;
  int status, c;
  char *arg[] = { "pl2wam", "-w", "--compile-msg", "--no-redef-error",
		  "--pl-state", tmp_file, "-o", tmp_file, pl_file,
		  singl_warn, NULL };

  save = SYS_VAR_SAY_GETC;
#ifndef NO_USE_PIPED_STDIN_FOR_CONSULT
  SYS_VAR_SAY_GETC = 1;
  pf_in = &f_in;
#else
  f_in = NULL;
  pf_in = NULL;
#endif
  Write_Pl_State_File(tmp_file_word);
  SYS_VAR_SAY_GETC = save;

  Flush_All_Streams();
  pid = M_Spawn_Redirect(arg, 0, pf_in, &f_out, &f_out);
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
#if 1
      c = fgetc(f_out);
#else
      char c0;
      c = (read(fileno(f_out), &c0, 1) == 1) ? c0 : EOF;
#endif
      if (c == EOF)
	break;
	  
#ifndef NO_USE_PIPED_STDIN_FOR_CONSULT
      if (c == CHAR_TO_EMIT_WHEN_CHAR)
	{
	  if (p == NULL)
	    {
	      if ((c = Stream_Getc(pstm_i)) == EOF)
		{
		eof_reached:
		  p = "end_of_file.\n";
		  c = *p++;
		}
	    }
	  else
	    {
	      if (*p == '\0')
		goto eof_reached;
	      else
		c = *p++;
	    }
	  fputc(c, f_in);
	  fflush(f_in);
	  continue;
	}
#endif
      Stream_Putc(c, pstm_o);
    }
  if (f_in)
    fclose(f_in);
  fclose(f_out);

  status = M_Get_Status(pid);
  if (status < 0)
    goto error_pl2wam;

  return status == 0;
}

#endif
