/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : stream_c.c                                                      *
 * Descr.: stream selection and control management - C part                *
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


#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>

#include "engine_pl.h"
#include "bips_pl.h"

#ifndef NO_USE_LINEDIT
#include "linedit.h"
#endif

#if defined(_WIN32) || defined(__CYGWIN__)
#include <io.h>
#endif

#ifndef _WIN32
#include <unistd.h>
#include <sys/fcntl.h>
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define TERM_STREAM_WRITE_BLOCK    1024




	  /* Error Messages */

#define ERR_NEEDS_SPECIAL_CLOSE    "special stream: needs appropriate close predicate"
#define ERR_CANNOT_CLOSE_STREAM    "cannot close stream"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  int buff_size;
  Bool buff_is_alloc;
  char *buff;
  char *ptr;
}
TermSInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#define CURRENT_STREAM_ALT         X1_2463757272656E745F73747265616D5F616C74
#define CURRENT_ALIAS_ALT          X1_2463757272656E745F616C6961735F616C74
#define CURRENT_MIRROR_ALT         X1_2463757272656E745F6D6972726F725F616C74

Prolog_Prototype(CURRENT_STREAM_ALT, 0);
Prolog_Prototype(CURRENT_ALIAS_ALT, 0);
Prolog_Prototype(CURRENT_MIRROR_ALT, 0);




/*-------------------------------------------------------------------------*
 * PL_CURRENT_INPUT_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Input_1(WamWord stm_word)
{
  return Pl_Get_Integer(pl_stm_input, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_OUTPUT_1                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Output_1(WamWord stm_word)
{
  return Pl_Get_Integer(pl_stm_output, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_SET_INPUT_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Input_1(WamWord sora_word)
{
  pl_stm_input = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);
}




/*-------------------------------------------------------------------------*
 * PL_SET_OUTPUT_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Output_1(WamWord sora_word)
{
  pl_stm_output = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);
}




/*-------------------------------------------------------------------------*
 * PL_SET_TOP_LEVEL_STREAMS_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Top_Level_Streams_2(WamWord sora_in_word, WamWord sora_out_word)
{
  pl_stm_top_level_input = Pl_Get_Stream_Or_Alias(sora_in_word, STREAM_CHECK_INPUT);
  pl_stm_top_level_output = Pl_Get_Stream_Or_Alias(sora_out_word, STREAM_CHECK_OUTPUT);

  Pl_Reassign_Alias(pl_atom_top_level_input, pl_stm_top_level_input);
  Pl_Reassign_Alias(pl_atom_top_level_output, pl_stm_top_level_output);
}




/*-------------------------------------------------------------------------*
 * PL_SET_DEBUGGER_STREAMS_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Debugger_Streams_2(WamWord sora_in_word, WamWord sora_out_word)
{
  pl_stm_debugger_input = Pl_Get_Stream_Or_Alias(sora_in_word, STREAM_CHECK_INPUT);
  pl_stm_debugger_output = Pl_Get_Stream_Or_Alias(sora_out_word, STREAM_CHECK_OUTPUT);

  Pl_Reassign_Alias(pl_atom_debugger_input, pl_stm_debugger_input);
  Pl_Reassign_Alias(pl_atom_debugger_output, pl_stm_debugger_output);
}




/*-------------------------------------------------------------------------*
 * PL_OPEN_3                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Open_3(WamWord source_sink_word, WamWord mode_word, WamWord stm_word)
{
  WamWord word, tag_mask;
  int atom;
  int mode;
  Bool text;
  StmProp prop;
  char *path;
  int atom_file_name;
  int stm;
  FILE *f;
  int mask = SYS_VAR_OPTION_MASK;
  Bool reposition;


  DEREF(source_sink_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();
  if (tag_mask != TAG_ATM_MASK)
    Pl_Err_Domain(pl_domain_source_sink, source_sink_word);

  atom_file_name = UnTag_ATM(word);
  path = pl_atom_tbl[atom_file_name].name;
  if ((path = Pl_M_Absolute_Path_Name(path)) == NULL)
    Pl_Err_Existence(pl_existence_source_sink, source_sink_word);

  text = mask & 1;
  mask >>= 1;

  atom = Pl_Rd_Atom_Check(mode_word);
  if (atom == pl_atom_read)
    mode = STREAM_MODE_READ;
  else if (atom == pl_atom_write)
    mode = STREAM_MODE_WRITE;
  else if (atom == pl_atom_append)
    mode = STREAM_MODE_APPEND;
  else
    Pl_Err_Domain(pl_domain_io_mode, mode_word);

  stm = Pl_Add_Stream_For_Stdio_File(path, mode, text);
  if (stm < 0)
    {
      if (errno == ENOENT || errno == ENOTDIR)
	Pl_Err_Existence(pl_existence_source_sink, source_sink_word);
      else
	Pl_Err_Permission(pl_permission_operation_open,
			  pl_permission_type_source_sink, source_sink_word);
    }

  prop = pl_stm_tbl[stm]->prop;
  f = (FILE *) pl_stm_tbl[stm]->file;

				/* change properties wrt to specified ones */

  if ((mask & 2) != 0)		/* reposition specified */
    {
      reposition = mask & 1;
      if (reposition && !prop.reposition)
	{
	  fclose(f);
	  word = Pl_Put_Structure(pl_atom_reposition, 1);
	  Pl_Unify_Atom(pl_atom_true);
	  Pl_Err_Permission(pl_permission_operation_open,
			    pl_permission_type_source_sink, word);
	}

      prop.reposition = reposition;
    }
  mask >>= 2;

  if ((mask & 4) != 0)		/* eof_action specified */
      prop.eof_action = mask & 3;
  mask >>= 3;


  if ((mask & 4) != 0)		/* buffering specified */
    if (prop.buffering != (unsigned) (mask & 3)) /* cast for MSVC warning */
      {
	prop.buffering = mask & 3;
	Pl_Stdio_Set_Buffering(f, prop.buffering);
      }
  mask >>= 3;

  pl_stm_tbl[stm]->atom_file_name = atom_file_name;
  pl_stm_tbl[stm]->prop = prop;

  Pl_Get_Integer(stm, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_TEST_ALIAS_NOT_ASSIGNED_1                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Test_Alias_Not_Assigned_1(WamWord alias_word)
{
  return Pl_Find_Stream_By_Alias(Pl_Rd_Atom_Check(alias_word)) < 0;
}




/*-------------------------------------------------------------------------*
 * PL_FROM_ALIAS_TO_STREAM_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_From_Alias_To_Stream_2(WamWord alias_word, WamWord stm_word)
{
  int stm;

  stm = Pl_Find_Stream_By_Alias(Pl_Rd_Atom_Check(alias_word));

  return stm >= 0 && Pl_Get_Integer(stm, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_ADD_STREAM_ALIAS_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Add_Stream_Alias_2(WamWord sora_word, WamWord alias_word)
{
  int stm;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);

  return Pl_Add_Alias_To_Stream(Pl_Rd_Atom_Check(alias_word), stm);
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_VALID_MIRROR_1                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_Valid_Mirror_1(WamWord mirror_word)
{
  Pl_Get_Stream_Or_Alias(mirror_word, STREAM_CHECK_OUTPUT);
}




/*-------------------------------------------------------------------------*
 * PL_ADD_STREAM_MIRROR_2                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Add_Stream_Mirror_2(WamWord sora_word, WamWord mirror_word)
{
  int stm;
  int m_stm;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  m_stm = Pl_Get_Stream_Or_Alias(mirror_word, STREAM_CHECK_OUTPUT);

  Pl_Add_Mirror_To_Stream(stm, m_stm);
}




/*-------------------------------------------------------------------------*
 * PL_REMOVE_STREAM_MIRROR_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Remove_Stream_Mirror_2(WamWord sora_word, WamWord mirror_word)
{
  int stm;
  int m_stm;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  m_stm = Pl_Get_Stream_Or_Alias(mirror_word, STREAM_CHECK_EXIST);

  return Pl_Del_Mirror_From_Stream(stm, m_stm);
}




/*-------------------------------------------------------------------------*
 * PL_SET_STREAM_TYPE_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Stream_Type_2(WamWord sora_word, WamWord is_text_word)
{
  int stm;
  StmInf *pstm;
  int text;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  text = Pl_Rd_Integer_Check(is_text_word);
  if ((unsigned) text == pstm->prop.text)
    return;

  if (pstm->char_count)
    Pl_Err_Permission(pl_permission_operation_modify,
		      pl_permission_type_stream, sora_word);

  pstm->prop.text = text;
#if defined(_WIN32) || defined(__CYGWIN__)
  {
    FILE *f;

    f = Pl_Stdio_Desc_Of_Stream(stm);
    if (f == NULL)
      return;

    setmode(fileno(f), (text) ? O_TEXT : O_BINARY);
  }
#endif
}




/*-------------------------------------------------------------------------*
 * PL_SET_STREAM_EOF_ACTION_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Stream_Eof_Action_2(WamWord sora_word, WamWord action_word)
{
  int stm;
  StmInf *pstm;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  if (pstm->prop.output)
    Pl_Err_Permission(pl_permission_operation_modify,
		      pl_permission_type_stream, sora_word);

  pstm->prop.eof_action = Pl_Rd_Integer_Check(action_word);
}




/*-------------------------------------------------------------------------*
 * PL_SET_STREAM_BUFFERING_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Stream_Buffering_2(WamWord sora_word, WamWord buff_mode_word)
{
  int stm;
  StmInf *pstm;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  pstm->prop.buffering = Pl_Rd_Integer_Check(buff_mode_word);
  Pl_Set_Stream_Buffering(stm);
}




/*-------------------------------------------------------------------------*
 * PL_CLOSE_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Close_1(WamWord sora_word)
{
  int stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  Pl_Close_Stm(stm, SYS_VAR_OPTION_MASK & 1);
}


/*-------------------------------------------------------------------------*
 * PL_CLOSE_STM                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Close_Stm(int stm, Bool force)
{
  StmInf *pstm = pl_stm_tbl[stm];
  int fd = 0;

  Pl_Stream_Flush(pstm);

  if (stm == pl_stm_stdin || stm == pl_stm_stdout)
    return;

  if (stm == pl_stm_top_level_input || stm == pl_stm_top_level_output)
    return;

  if (stm == pl_stm_debugger_input || stm == pl_stm_debugger_output)
    return;

  if (stm == pl_stm_input)
    pl_stm_input = pl_stm_stdin;
  else if (stm == pl_stm_output)
    pl_stm_output = pl_stm_stdout;

  if (pstm->prop.special_close)
    Pl_Err_System(Pl_Create_Atom(ERR_NEEDS_SPECIAL_CLOSE));

  if (pstm->fct_close == fclose)
    fd = fileno((FILE *) (pstm->file));

  if (Pl_Stream_Close(pstm) != 0)
    {
      if (force == 0)
	Pl_Err_System(Pl_Create_Atom(ERR_CANNOT_CLOSE_STREAM));

      /* else force close */
      if (fd > 2)
	close(fd);
    }

  Pl_Delete_Stream(stm);
}




/*-------------------------------------------------------------------------*
 * PL_FLUSH_OUTPUT_1                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Flush_Output_1(WamWord sora_word)
{
  int stm;


  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_output : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);

  pl_last_output_sora = sora_word;

  Pl_Stream_Flush(pl_stm_tbl[stm]);
}




/*-------------------------------------------------------------------------*
 * PL_FLUSH_OUTPUT_0                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Flush_Output_0(void)
{
  Pl_Flush_Output_1(NOT_A_WAM_WORD);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_STREAM_1                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Stream_1(WamWord stm_word)
{
  WamWord word, tag_mask;
  int stm = 0;


  DEREF(stm_word, word, tag_mask);	/* either an INT or a REF */
  if (tag_mask == TAG_INT_MASK)
    {
      stm = UnTag_INT(word);
      return (stm >= 0 && stm <= pl_stm_last_used && pl_stm_tbl[stm] != NULL);
    }

  for (; stm <= pl_stm_last_used; stm++)
    if (pl_stm_tbl[stm])
      break;

  if (stm >= pl_stm_last_used)
    {
      if (stm > pl_stm_last_used)
	return FALSE;
    }
  else				/* non deterministic case */
    {
      A(0) = stm_word;
      A(1) = stm + 1;
      Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_STREAM_ALT, 0),
			  2);
    }

  return Pl_Get_Integer(stm, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_STREAM_ALT_0                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Stream_Alt_0(void)
{
  WamWord stm_word;
  int stm;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_STREAM_ALT, 0), 0);

  stm_word = AB(B, 0);
  stm = AB(B, 1);

  for (; stm <= pl_stm_last_used; stm++)
    if (pl_stm_tbl[stm])
      break;

  if (stm >= pl_stm_last_used)
    {
      Delete_Last_Choice_Point();
      if (stm > pl_stm_last_used)
	return FALSE;
    }
  else				/* non deterministic case */
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = stm_word;
#endif
      AB(B, 1) = stm + 1;
    }

  return Pl_Get_Integer(stm, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PROP_FILE_NAME_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Prop_File_Name_2(WamWord file_name_word, WamWord stm_word)
{
  int stm;

  stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */

  return Pl_Un_Atom_Check(pl_stm_tbl[stm]->atom_file_name, file_name_word);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PROP_MODE_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Prop_Mode_2(WamWord mode_word, WamWord stm_word)
{
  int stm;
  int atom;

  stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */

  switch (pl_stm_tbl[stm]->prop.mode)
    {
    case STREAM_MODE_READ:
      atom = pl_atom_read;
      break;

    case STREAM_MODE_WRITE:
      atom = pl_atom_write;
      break;

    case STREAM_MODE_APPEND:
      atom = pl_atom_append;
      break;
    }

  return Pl_Un_Atom_Check(atom, mode_word);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PROP_INPUT_1                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Prop_Input_1(WamWord stm_word)
{
  int stm;

  stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */

  return pl_stm_tbl[stm]->prop.input;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PROP_OUTPUT_1                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Prop_Output_1(WamWord stm_word)
{
  int stm;

  stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */

  return pl_stm_tbl[stm]->prop.output;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PROP_TYPE_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Prop_Type_2(WamWord type_word, WamWord stm_word)
{
  int stm;
  int atom;

  stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */

  atom = (pl_stm_tbl[stm]->prop.text) ? pl_atom_text : pl_atom_binary;

  return Pl_Un_Atom_Check(atom, type_word);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PROP_REPOSITION_2                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Prop_Reposition_2(WamWord reposition_word, WamWord stm_word)
{
  int stm;
  int atom;

  stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */

  atom = (pl_stm_tbl[stm]->prop.reposition) ? pl_atom_true : pl_atom_false;

  return Pl_Un_Atom_Check(atom, reposition_word);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PROP_EOF_ACTION_2                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Prop_Eof_Action_2(WamWord eof_action_word, WamWord stm_word)
{
  int stm;
  int atom;

  stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */

  switch (pl_stm_tbl[stm]->prop.eof_action)
    {
    case STREAM_EOF_ACTION_ERROR:
      atom = pl_atom_error;
      break;

    case STREAM_EOF_ACTION_EOF_CODE:
      atom = pl_atom_eof_code;
      break;

    case STREAM_EOF_ACTION_RESET:
      atom = pl_atom_reset;
      break;
    }

  return Pl_Un_Atom_Check(atom, eof_action_word);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PROP_BUFFERING_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Prop_Buffering_2(WamWord buffering_word, WamWord stm_word)
{
  int stm;
  int atom;

  stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */

#ifndef NO_USE_LINEDIT		/* if GUI: ask it for buffering */
  if (pl_stm_tbl[stm]->file == (PlLong) stdout && pl_le_hook_get_line_buffering)
    {
      if ((*pl_le_hook_get_line_buffering)())
	pl_stm_tbl[stm]->prop.buffering = STREAM_BUFFERING_LINE;
      else
	pl_stm_tbl[stm]->prop.buffering = STREAM_BUFFERING_NONE;
    }
#endif

  switch (pl_stm_tbl[stm]->prop.buffering)
    {
    case STREAM_BUFFERING_NONE:
      atom = pl_atom_none;
      break;

    case STREAM_BUFFERING_LINE:
      atom = pl_atom_line;
      break;

    case STREAM_BUFFERING_BLOCK:
      atom = pl_atom_block;
      break;
    }

  return Pl_Un_Atom_Check(atom, buffering_word);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PROP_END_OF_STREAM_2                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Prop_End_Of_Stream_2(WamWord end_of_stream_word, WamWord stm_word)
{
  int stm;
  int atom;

  stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */

  switch (Pl_Stream_End_Of_Stream(pl_stm_tbl[stm]))
    {
    case STREAM_EOF_NOT:
      atom = pl_atom_not;
      break;

    case STREAM_EOF_AT:
      atom = pl_atom_at;
      break;

    case STREAM_EOF_PAST:
      atom = pl_atom_past;
      break;
    }

  return Pl_Un_Atom_Check(atom, end_of_stream_word);
}




/*-------------------------------------------------------------------------*
 * PL_AT_END_OF_STREAM_1                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_At_End_Of_Stream_1(WamWord sora_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  return Pl_Stream_End_Of_Stream(pl_stm_tbl[stm]) != STREAM_EOF_NOT;
}




/*-------------------------------------------------------------------------*
 * PL_AT_END_OF_STREAM_0                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_At_End_Of_Stream_0(void)
{
  return Pl_At_End_Of_Stream_1(NOT_A_WAM_WORD);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_ALIAS_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Alias_2(WamWord stm_word, WamWord alias_word)
{
  WamWord word, tag_mask;
  int stm;
  HashScan scan;
  AliasInf *alias;
  AliasInf *save_alias;

  stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */

  DEREF(alias_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    return Pl_Find_Stream_By_Alias(Pl_Rd_Atom_Check(word)) == stm;

  for (alias = (AliasInf *) Pl_Hash_First(pl_alias_tbl, &scan); alias;
       alias = (AliasInf *) Pl_Hash_Next(&scan))
    if (alias->stm == stm)
      break;

  if (alias == NULL)
    return FALSE;

  save_alias = alias;

  for (;;)
    {
      alias = (AliasInf *) Pl_Hash_Next(&scan);
      if (alias == NULL || alias->stm == stm)
	break;
    }


  if (alias)			/* non deterministic case */
    {
      A(0) = stm;
      A(1) = alias_word;
      A(2) = (WamWord) scan.endt;
      A(3) = (WamWord) scan.cur_t;
      A(4) = (WamWord) scan.cur_p;
      A(5) = (WamWord) alias;
      Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_ALIAS_ALT, 0),
			  6);
    }

  Pl_Get_Atom(save_alias->atom, alias_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_ALIAS_ALT_0                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Alias_Alt_0(void)
{
  int stm;
  WamWord alias_word;
  HashScan scan;
  AliasInf *alias;
  AliasInf *save_alias;


  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_ALIAS_ALT, 0), 0);

  stm = AB(B, 0);
  alias_word = AB(B, 1);
  scan.endt = (char *) AB(B, 2);
  scan.cur_t = (char *) AB(B, 3);
  scan.cur_p = (char *) AB(B, 4);
  alias = (AliasInf *) AB(B, 5);


  save_alias = alias;

  for (;;)
    {
      alias = (AliasInf *) Pl_Hash_Next(&scan);
      if (alias == NULL || alias->stm == stm)
	break;
    }


  if (alias)			/* non deterministic case */
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = stm;
      AB(B, 1) = alias_word;
      AB(B, 2) = (WamWord) scan.endt;
#endif
      AB(B, 3) = (WamWord) scan.cur_t;
      AB(B, 4) = (WamWord) scan.cur_p;
      AB(B, 5) = (WamWord) alias;
    }
  else
    Delete_Last_Choice_Point();

  Pl_Get_Atom(save_alias->atom, alias_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_MIRROR_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Mirror_2(WamWord stm_word, WamWord m_stm_word)
{
  int stm = Pl_Rd_Integer_Check(stm_word);	/* stm is a valid stream entry */
  StmInf *pstm = pl_stm_tbl[stm];
  StmLst *m = pstm->mirror;

				/* From here, the code also works with     */
				/* m = m_pstm->mirror_of. Could be used    */
				/* if m_stm_word is given and not stm_word */
  if (m == NULL)
    return FALSE;

  if (m->next != NULL) /* non deterministic case */
    {
      A(0) = stm;		/* useless in fact */
      A(1) = m_stm_word;
      A(2) = (WamWord) m->next;
      Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_MIRROR_ALT, 0), 3);
    }

  return Pl_Get_Integer(m->stm, m_stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_MIRROR_ALT_0                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Mirror_Alt_0(void)
{
  /*  int stm; */
  WamWord m_stm_word;
  StmLst *m;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_MIRROR_ALT, 0), 0);

  /*  stm = AB(B, 0); */
  m_stm_word = AB(B, 1);
  m = (StmLst *) AB(B, 2);

  if (m->next)			/* non deterministic case */
    {
#if 0				/* the following data is unchanged */
      AB(B, 0) = stm;
      AB(B, 1) = m_stm_word;
#endif
      AB(B, 2) = (WamWord) m->next;
    }
  else
    Delete_Last_Choice_Point();

  return Pl_Get_Integer(m->stm, m_stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_POSITION_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Position_2(WamWord sora_word, WamWord position_word)
{
  WamWord word, tag_mask;
  WamWord p_word[4];
  PlLong p[4];
  int i;
  int stm;
  StmInf *pstm;


  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  Pl_Stream_Get_Position(pstm, p, p + 1, p + 2, p + 3);

  if (!Pl_Get_Structure(pl_atom_stream_position, 4, position_word))
  dom_error:
    Pl_Err_Domain(pl_domain_stream_position, position_word);

  for (i = 0; i < 4; i++)
    {
      p_word[i] = Pl_Unify_Variable();

      DEREF(p_word[i], word, tag_mask);
      if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK)
	goto dom_error;
    }

  for (i = 0; i < 4; i++)
    if (!Pl_Get_Integer(p[i], p_word[i]))
      return FALSE;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_SET_STREAM_POSITION_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Set_Stream_Position_2(WamWord sora_word, WamWord position_word)
{
  WamWord word, tag_mask;
  WamWord p_word[4];
  int p[4];
  int i;
  int stm;
  StmInf *pstm;


  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  if (!pstm->prop.reposition)
    Pl_Err_Permission(pl_permission_operation_reposition,
		      pl_permission_type_stream, sora_word);

  DEREF(position_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (!Pl_Get_Structure(pl_atom_stream_position, 4, position_word))
  dom_error:
    Pl_Err_Domain(pl_domain_stream_position, position_word);

  for (i = 0; i < 4; i++)
    {
      p_word[i] = Pl_Unify_Variable();

      DEREF(p_word[i], word, tag_mask);
      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (tag_mask != TAG_INT_MASK)
	goto dom_error;

      p[i] = UnTag_INT(word);
    }

  return Pl_Stream_Set_Position(pstm, SEEK_SET, p[0], p[1], p[2], p[3]) == 0;
}




/*-------------------------------------------------------------------------*
 * PL_SEEK_4                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Seek_4(WamWord sora_word, WamWord whence_word, WamWord offset_word,
	  WamWord new_loc_word)
{
  int stm;
  StmInf *pstm;
  int whence;
  PlLong offset;
  int atom;
  PlLong p[4];


  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  if (!pstm->prop.reposition)
    Pl_Err_Permission(pl_permission_operation_reposition,
		      pl_permission_type_stream, sora_word);

  if (pstm->prop.text)
    Pl_Err_Permission(pl_permission_operation_reposition,
		      pl_permission_type_text_stream, sora_word);

  atom = Pl_Rd_Atom_Check(whence_word);

  if (atom == pl_atom_bof)
    whence = SEEK_SET;
  else if (atom == pl_atom_current)
    whence = SEEK_CUR;
  else if (atom == pl_atom_eof)
    whence = SEEK_END;
  else
    Pl_Err_Domain(pl_domain_stream_seek_method, whence_word);

  offset = Pl_Rd_Integer_Check(offset_word);
  Pl_Check_For_Un_Integer(new_loc_word);

  if (Pl_Stream_Set_Position(pstm, whence, offset, offset, 0, 0) != 0)
    return FALSE;

  Pl_Stream_Get_Position(pstm, &offset, p + 1, p + 2, p + 3);

  return Pl_Get_Integer(offset, new_loc_word);
}




/*-------------------------------------------------------------------------*
 * PL_CHARACTER_COUNT_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Character_Count_2(WamWord sora_word, WamWord count_word)
{
  int stm;
  StmInf *pstm;
  PlLong offset, char_count, line_count, line_pos;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  Pl_Stream_Get_Position(pstm, &offset, &char_count, &line_count, &line_pos);

  return Pl_Un_Integer_Check(char_count, count_word);
}




/*-------------------------------------------------------------------------*
 * PL_LINE_COUNT_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Line_Count_2(WamWord sora_word, WamWord count_word)
{
  int stm;
  StmInf *pstm;
  PlLong offset, char_count, line_count, line_pos;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  if (!pstm->prop.text)
    Pl_Err_Permission(pl_permission_operation_access,
		      pl_permission_type_binary_stream, sora_word);

  Pl_Stream_Get_Position(pstm, &offset, &char_count, &line_count, &line_pos);

  return Pl_Un_Integer_Check(line_count, count_word);
}




/*-------------------------------------------------------------------------*
 * PL_LINE_POSITION_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Line_Position_2(WamWord sora_word, WamWord count_word)
{
  int stm;
  StmInf *pstm;
  PlLong offset, char_count, line_count, line_pos;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  if (!pstm->prop.text)
    Pl_Err_Permission(pl_permission_operation_access,
		      pl_permission_type_binary_stream, sora_word);

  Pl_Stream_Get_Position(pstm, &offset, &char_count, &line_count, &line_pos);

  return Pl_Un_Integer_Check(line_pos, count_word);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_LINE_COLUMN_3                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stream_Line_Column_3(WamWord sora_word, WamWord line_word, WamWord col_word)
{
  int stm;
  StmInf *pstm;
  PlLong offset, char_count, line_count, line_pos;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  if (!pstm->prop.text)
    Pl_Err_Permission(pl_permission_operation_access,
		      pl_permission_type_binary_stream, sora_word);

  Pl_Stream_Get_Position(pstm, &offset, &char_count, &line_count, &line_pos);

  return Pl_Un_Integer_Check(line_count + 1, line_word) &&
    Pl_Un_Integer_Check(line_pos + 1, col_word);
}




/*-------------------------------------------------------------------------*
 * PL_SET_STREAM_LINE_COLUMN_3                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Set_Stream_Line_Column_3(WamWord sora_word, WamWord line_word,
			    WamWord col_word)
{
  int stm;
  StmInf *pstm;
  PlLong line_count, line_pos;


  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  if (!pstm->prop.reposition)
    Pl_Err_Permission(pl_permission_operation_reposition,
		      pl_permission_type_stream, sora_word);

  if (!pstm->prop.text)
    Pl_Err_Permission(pl_permission_operation_reposition,
		      pl_permission_type_binary_stream, sora_word);


  line_count = Pl_Rd_Integer_Check(line_word) - 1;
  line_pos = Pl_Rd_Integer_Check(col_word) - 1;

  return line_count >= 0 && line_pos >= 0 &&
    Pl_Stream_Set_Position_LC(pstm, line_count, line_pos) == 0;
}




/*-------------------------------------------------------------------------*
 * Operations on term_streams                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/



/*-------------------------------------------------------------------------*
 * PL_OPEN_INPUT_TERM_STREAM_2                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Open_Input_Term_Stream_2(WamWord sink_term_word, WamWord stm_word)
{
  char *str;
  int stm;
  int n;

  if (SYS_VAR_OPTION_MASK == TERM_STREAM_ATOM)
    str = pl_atom_tbl[Pl_Rd_Atom_Check(sink_term_word)].name;
  else
    {
      n = Pl_List_Length(sink_term_word);	/* -1 if not a list */
      if (n >= 0)
	str = Malloc(n + 1);	/* +1 for \0 */
      else
	str = pl_glob_buff;

      if (SYS_VAR_OPTION_MASK == TERM_STREAM_CHARS)
	Pl_Rd_Chars_Str_Check(sink_term_word, str);
      else
	Pl_Rd_Codes_Str_Check(sink_term_word, str);
    }

  stm = Pl_Add_Str_Stream(str, SYS_VAR_OPTION_MASK);

  Pl_Get_Integer(stm, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_CLOSE_INPUT_TERM_STREAM_1                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Close_Input_Term_Stream_1(WamWord sora_word)
{
  int stm;
  StmInf *pstm;
  StrSInf *str_stream;
  int type;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  type = pstm->prop.other;

  if (type < 1 || type > 3)
    Pl_Err_Domain(pl_domain_term_stream_or_alias, sora_word);

  if (pstm->prop.output)
    Pl_Err_Permission(pl_permission_operation_close,
		      pl_permission_type_stream, sora_word);

  if (type != TERM_STREAM_ATOM)
    {
      str_stream = (StrSInf *) (pstm->file);
      Free(str_stream->buff);
    }

  Pl_Delete_Str_Stream(stm);
}




/*-------------------------------------------------------------------------*
 * PL_OPEN_OUTPUT_TERM_STREAM_1                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Open_Output_Term_Stream_1(WamWord stm_word)
{
  int stm;

  stm = Pl_Add_Str_Stream(NULL, SYS_VAR_OPTION_MASK);

  Pl_Get_Integer(stm, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_CLOSE_OUTPUT_TERM_STREAM_2                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Close_Output_Term_Stream_2(WamWord sora_word, WamWord sink_term_word)
{
  int stm;
  StmInf *pstm;
  int type;
  char *str;

  stm = Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_EXIST);
  pstm = pl_stm_tbl[stm];

  type = pstm->prop.other;

  if (type < 1 || type > 3)
    Pl_Err_Domain(pl_domain_term_stream_or_alias, sora_word);

  if (pstm->prop.input)
    Pl_Err_Permission(pl_permission_operation_close,
		      pl_permission_type_stream, sora_word);

  str = Pl_Term_Write_Str_Stream(stm);

  switch (SYS_VAR_OPTION_MASK)
    {
    case TERM_STREAM_ATOM:
      if (!Pl_Un_String_Check(str, sink_term_word))
	return FALSE;
      break;

    case TERM_STREAM_CHARS:
      if (!Pl_Un_Chars_Check(str, sink_term_word))
	return FALSE;
      break;

    case TERM_STREAM_CODES:
      if (!Pl_Un_Codes_Check(str, sink_term_word))
	return FALSE;
      break;

    }

  Pl_Delete_Str_Stream(stm);
  return TRUE;
}
