/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : consult_c.c                                                     *
 * Descr.: file consulting - C part                                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2025 Daniel Diaz                                     *
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


#include <errno.h>
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
 * PL_CONSULT_3                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Consult_1(WamWord pl2wam_args_word)
{
  StmInf *pstm_o = pl_stm_tbl[pl_stm_top_level_output];
  StmInf *pstm_i = pl_stm_tbl[pl_stm_top_level_input];
  int pid;
  FILE *f_in = M_SPAWN_REDIRECT_CREATE;
  FILE *f_out = M_SPAWN_REDIRECT_CREATE;
  FILE **pf_in;
  unsigned char *p = NULL;
  int status, c;
  int save_use_le_prompt;
#if 0
  /* MSVC does not accept C99 VLA (variable length arrays)
     could use alloca (#include <alloca.h> on linux)
     on windows/MSVC: _alloca/_malloca and #include <malloc.h>)
  */
  char *arg[2 + Pl_List_Length(pl2wam_args_word)];
 #else
  char *arg[64];
#endif
  WamWord word, tag_mask;
  WamWord *lst_adr;
  int n = 0;

  arg[n++] = "pl2wam";

  for (;;)
    {
      DEREF(pl2wam_args_word, word, tag_mask);
      if (word == NIL_WORD)
        break;

      lst_adr = UnTag_LST(word);

      arg[n++] = Pl_Rd_String(Car(lst_adr));
      pl2wam_args_word = Cdr(lst_adr);
    }
  arg[n] = NULL;

#ifndef NO_USE_PIPED_STDIN_FOR_CONSULT
  pf_in = &f_in;
#else
  f_in = NULL;
  pf_in = NULL;
#endif

  Pl_Flush_All_Streams();
  pid = Pl_M_Spawn_Redirect(arg, 0, pf_in, &f_out, &f_out);

  /* If pl2wam is not found we get ENOENT under Windows. 
   * Under Unix the information is only obtained at Pl_M_Get_Status(). */

  if (pid == -1 && errno != ENOENT)
    Os_Test_Error(pid); /* ENOENT is for Windows */
  if (pid < 0)
    {
    error_pl2wam:
      Pl_Err_System(Pl_Create_Atom("error trying to execute pl2wam (maybe not found)"));
      return FALSE;
    }

  save_use_le_prompt = pl_use_le_prompt;
  pl_use_le_prompt = 0;
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
      if (c == CHAR_TO_EMIT_ON_PIPED_GETC)
	{
	  if (p == NULL)
	    {
	      c = Pl_Stream_Getc(pstm_i);
	      if (c == EOF)
		{
		eof_reached:
		  p = (unsigned char *) "end_of_file.\n";
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
      Pl_Stream_Putc(c, pstm_o);
    }
  pl_use_le_prompt = save_use_le_prompt;
  if (f_in)
    fclose(f_in);
  fclose(f_out);

  status = Pl_M_Get_Status(pid);
  if (status < 0)
    goto error_pl2wam;

  return status == 0;
}

#endif
