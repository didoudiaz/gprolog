/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : stream_supp.c                                                   *
 * Descr.: stream support                                                  *
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
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


#ifdef clearerr	/* prevent the case clearerr is (also) a macro */
#undef clearerr
extern void clearerr(FILE *stream);
#endif

#if defined(FOR_EXTERNAL_USE) && defined(W32_GUI_CONSOLE)
#undef W32_GUI_CONSOLE
#endif

#define STREAM_SUPP_FILE

#include "engine_pl.h"
#include "bips_pl.h"

#ifndef NO_USE_LINEDIT
#include "linedit.h"
#endif

#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define START_ALIAS_TBL_SIZE       128

#define STR_STREAM_WRITE_BLOCK     1024

#define TTY_BUFFER_SIZE            1024

#define BIG_BUFFER                 65535




	  /* Error Messages */

#define ERR_TELL_OR_SEEK_UNDEFINED "fct tell or seek undefined\n"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static void Init_Stream_Supp();
void (*pl_init_stream_supp)() = Init_Stream_Supp; /* overwrite var of engine.c */

static int atom_constant_term_stream;

static WamWord stream_1;

static WamWord word_current_input_stream;
static WamWord word_current_output_stream;

static StrSInf static_str_stream_rd = { NULL, NULL, 0 }; /* input */
static StrSInf static_str_stream_wr = { NULL, NULL, 0 }; /* output */


#ifndef NO_USE_LINEDIT
static char tty_first_buff[TTY_BUFFER_SIZE];	/* current buffer (end with '\0') */
static char *tty_buff;
static char *tty_ptr = NULL;	/* current pointer into the buff  */
#endif



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Find_Free_Stream(void);

static void Del_Aliases_Of_Stream(int stm);

static void Update_Mirrors_To_Del_Stream(int stm);

static Bool Remove_In_Stream_List(int stm, StmLst **p_start);


#ifndef NO_USE_LINEDIT

static int TTY_Getc(void);

static int TTY_Get_Key(Bool echo, Bool catch_ctrl_c);

static void TTY_Clearerr(void);

#endif

static int Basic_Call_Fct_Getc(StmInf *pstm);

static void Basic_Call_Fct_Putc(int c, StmInf *pstm);

static int Str_Stream_Getc(StrSInf *str_stream);

static void Str_Stream_Putc(int c, StrSInf *str_stream);





/*-------------------------------------------------------------------------*
 * INIT_STREAM_SUPP                                                        *
 *                                                                         *
 * no declared as other initializers, since we must be sure it has been    *
 * initialized before others.                                              *
 *-------------------------------------------------------------------------*/
static void
Init_Stream_Supp(void)
{
#ifndef NO_USE_LINEDIT
  StmInf *pstm;
#endif

  pl_stm_tbl_size = 32;
  pl_stm_tbl = (StmInf **) Calloc(pl_stm_tbl_size, sizeof(StmInf *));
  pl_stm_last_used = -1;

  pl_alias_tbl = Pl_Hash_Alloc_Table(START_ALIAS_TBL_SIZE, sizeof(AliasInf));

  pl_atom_stream = Pl_Create_Atom("$stream");
  stream_1 = Functor_Arity(pl_atom_stream, 1);

  atom_constant_term_stream = Pl_Create_Atom("constant term stream");

  word_current_input_stream = Tag_ATM(Pl_Create_Atom("current_input_stream"));
  word_current_output_stream = Tag_ATM(Pl_Create_Atom("current_output_stream"));

  pl_atom_user_input = Pl_Create_Atom("user_input");
  pl_atom_user_output = Pl_Create_Atom("user_output");
  pl_atom_user_error = Pl_Create_Atom("user_error");

  pl_atom_top_level_input = Pl_Create_Atom("top_level_input");
  pl_atom_top_level_output = Pl_Create_Atom("top_level_output");

  pl_atom_debugger_input = Pl_Create_Atom("debugger_input");
  pl_atom_debugger_output = Pl_Create_Atom("debugger_output");

  pl_atom_read = Pl_Create_Atom("read");
  pl_atom_write = Pl_Create_Atom("write");
  pl_atom_append = Pl_Create_Atom("append");

  pl_atom_reposition = Pl_Create_Atom("reposition");

  pl_atom_stream_position = Pl_Create_Atom("$stream_position");

  pl_atom_text = Pl_Create_Atom("text");
  pl_atom_binary = Pl_Create_Atom("binary");

  pl_atom_error = Pl_Create_Atom("error");
  pl_atom_eof_code = Pl_Create_Atom("eof_code");
  pl_atom_reset = Pl_Create_Atom("reset");

  pl_atom_none = Pl_Create_Atom("none");
  pl_atom_line = Pl_Create_Atom("line");
  pl_atom_block = Pl_Create_Atom("block");

  pl_atom_not = Pl_Create_Atom("not");
  pl_atom_at = Pl_Create_Atom("at");
  pl_atom_past = Pl_Create_Atom("past");

  pl_atom_bof = Pl_Create_Atom("bof");
  pl_atom_current = Pl_Create_Atom("current");
  pl_atom_eof = Pl_Create_Atom("eof");

  pl_le_prompt = "";
  pl_use_le_prompt = TRUE;

  pl_stm_stdin = Pl_Add_Stream_For_Stdio_Desc(stdin, pl_atom_user_input,
					      STREAM_MODE_READ, TRUE);

#ifndef NO_USE_LINEDIT
  if (pl_le_mode == LE_MODE_HOOK || (pl_le_mode == LE_MODE_TTY && isatty(0)))
    {
      pstm = pl_stm_tbl[pl_stm_stdin];
      pstm->fct_getc = (StmFct) TTY_Getc;
      pstm->fct_putc = STREAM_FCT_UNDEFINED;
      pstm->fct_flush = STREAM_FCT_UNDEFINED;
      pstm->fct_close = STREAM_FCT_UNDEFINED;
      pstm->fct_tell = STREAM_FCT_UNDEFINED;
      pstm->fct_seek = STREAM_FCT_UNDEFINED;
      pstm->fct_clearerr = (StmFct) TTY_Clearerr;
      pl_stream_use_linedit = TRUE;
    }
#endif
  Pl_Add_Alias_To_Stream(pl_atom_user_input, pl_stm_stdin);
  pl_stm_input = pl_stm_stdin;



  pl_stm_stdout = Pl_Add_Stream_For_Stdio_Desc(stdout, pl_atom_user_output,
					       STREAM_MODE_WRITE, TRUE);

#if !defined(NO_USE_LINEDIT) && defined(_WIN32)
		/* ok for both GUI and console EOM<->ANSI conversion */
  pstm = pl_stm_tbl[pl_stm_stdout];
  pstm->prop.buffering = STREAM_BUFFERING_LINE;
  if (pl_le_hook_put_char && isatty(1))
    pstm->fct_putc = (StmFct) pl_le_hook_put_char;

  if (pl_le_hook_flush && isatty(1))
    pstm->fct_flush = (StmFct) pl_le_hook_flush;
#endif
  Pl_Add_Alias_To_Stream(pl_atom_user_output, pl_stm_stdout);
  pl_stm_output = pl_stm_stdout;



  pl_stm_stderr = Pl_Add_Stream_For_Stdio_Desc(stderr, pl_atom_user_error,
					       STREAM_MODE_WRITE, TRUE);

#if !defined(NO_USE_LINEDIT) && defined(_WIN32)
		/* ok for both GUI and console EOM<->ANSI conversion */
  pstm = pl_stm_tbl[pl_stm_stderr];
  pstm->prop.buffering = STREAM_BUFFERING_LINE;
  if (pl_le_hook_put_char && isatty(2))
    pstm->fct_putc = (StmFct) pl_le_hook_put_char;

  if (pl_le_hook_flush && isatty(2))
    pstm->fct_flush = (StmFct) pl_le_hook_flush;
#endif
  Pl_Add_Alias_To_Stream(pl_atom_user_error, pl_stm_stderr);
  pl_stm_error = pl_stm_stderr;

  pl_stm_top_level_input = pl_stm_debugger_input = pl_stm_input;
  pl_stm_top_level_output = pl_stm_debugger_output = pl_stm_output;

  Pl_Add_Alias_To_Stream(pl_atom_top_level_input, pl_stm_top_level_input);
  Pl_Add_Alias_To_Stream(pl_atom_top_level_output, pl_stm_top_level_output);

  Pl_Add_Alias_To_Stream(pl_atom_debugger_input, pl_stm_debugger_input);
  Pl_Add_Alias_To_Stream(pl_atom_debugger_output, pl_stm_debugger_output);
}




#ifndef FOR_EXTERNAL_USE

/*-------------------------------------------------------------------------*
 * PL_PROP_AND_STDIO_MODE                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
StmProp
Pl_Prop_And_Stdio_Mode(int mode, Bool text, char *open_str)
{
  StmProp prop;

  prop.mode = mode;

  switch(mode)
    {
    case STREAM_MODE_READ:
      prop.input = TRUE;
      prop.output = FALSE;
      *open_str++ = 'r';
      break;

    case STREAM_MODE_WRITE:
      prop.input = FALSE;
      prop.output = TRUE;
      *open_str++ = 'w';
      break;

    case STREAM_MODE_APPEND:
      prop.input = FALSE;
      prop.output = TRUE;
      *open_str++ = 'a';
    }

  prop.text = text;
  prop.reposition = TRUE;
  prop.eof_action = STREAM_EOF_ACTION_EOF_CODE;
  prop.buffering = STREAM_BUFFERING_BLOCK;
  prop.special_close = FALSE;
  prop.other = 0;

  *open_str++ = (text) ? 't' : 'b';
  *open_str = '\0';

  return prop;
}




/*-------------------------------------------------------------------------*
 * PL_ADD_STREAM_FOR_STDIO_DESC                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Add_Stream_For_Stdio_Desc(FILE *f, int atom_path, int mode, int text)
{
  char open_str[10];
  StmProp prop = Pl_Prop_And_Stdio_Mode(mode, text, open_str);

  prop.reposition = Pl_Stdio_Is_Repositionable(f);
  prop.buffering = (prop.reposition) ? STREAM_BUFFERING_BLOCK :
    STREAM_BUFFERING_LINE;

  Pl_Stdio_Set_Buffering(f, prop.buffering);
  if (isatty(fileno(f)))
    prop.eof_action = STREAM_EOF_ACTION_RESET;

  return Pl_Add_Stream(atom_path, (PlLong) f, prop,
		       NULL, NULL, NULL, NULL, NULL, NULL, NULL);
}




/*-------------------------------------------------------------------------*
 * PL_ADD_STREAM_FOR_STDIO_FILE                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Add_Stream_For_Stdio_File(char *path, int mode, Bool text)
{
  FILE *f;
  char open_str[10];
  int atom_path;

  Pl_Prop_And_Stdio_Mode(mode, text, open_str); /* only for open_str */

  if ((f = fopen(path, open_str)) == NULL)
    return -1;

  atom_path = Pl_Create_Allocate_Atom(path);

  return Pl_Add_Stream_For_Stdio_Desc(f, atom_path, mode, text);
}




#endif /* !FOR_EXTERNAL_USE */

/*-------------------------------------------------------------------------*
 * INIT_STREAM_STRUCT                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Init_Stream_Struct(int atom_file_name, PlLong file, StmProp prop,
		   StmFct fct_getc, StmFct fct_putc,
		   StmFct fct_flush, StmFct fct_close,
		   StmFct fct_tell, StmFct fct_seek, StmFct fct_clearerr,
		   StmInf *pstm)
{
  pstm->atom_file_name = atom_file_name;
  pstm->file = file;
  pstm->prop = prop;
  pstm->mirror = NULL;
  pstm->mirror_of = NULL;

#define INIT_FCT(f, d) pstm->f = (f) ? f : (StmFct) d

  INIT_FCT(fct_getc, fgetc);
  INIT_FCT(fct_putc, fputc);
  INIT_FCT(fct_flush, fflush);
  INIT_FCT(fct_close, fclose);
  INIT_FCT(fct_tell, ftell);
  INIT_FCT(fct_seek, fseek);
  INIT_FCT(fct_clearerr, clearerr);

  /* Works only because putc will be called with c as 1st arg
     and flush's arg is ignored */

  pstm->eof_reached = FALSE;
  PB_Init(pstm->pb_char);

  pstm->char_count = 0;
  pstm->line_count = 0;
  pstm->line_pos = 0;
  PB_Init(pstm->pb_line_pos);
}




/*-------------------------------------------------------------------------*
 * PL_ADD_STREAM                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Add_Stream(int atom_file_name, PlLong file, StmProp prop,
	      StmFct fct_getc, StmFct fct_putc,
	      StmFct fct_flush, StmFct fct_close,
	      StmFct fct_tell, StmFct fct_seek, StmFct fct_clearerr)
{
  int stm;
  StmInf *pstm;

  stm = Find_Free_Stream();

  if (prop.reposition && (fct_tell == STREAM_FCT_UNDEFINED ||
			  fct_seek == STREAM_FCT_UNDEFINED))
    Pl_Fatal_Error(ERR_TELL_OR_SEEK_UNDEFINED);


  pstm = pl_stm_tbl[stm];
  Init_Stream_Struct(atom_file_name, file, prop, fct_getc, fct_putc,
		     fct_flush, fct_close, fct_tell, fct_seek, fct_clearerr,
		     pstm);

  return stm;
}




/*-------------------------------------------------------------------------*
 * REMOVE_STREAM                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Delete_Stream(int stm)
{
  Del_Aliases_Of_Stream(stm);

  Update_Mirrors_To_Del_Stream(stm);

  Free(pl_stm_tbl[stm]);
  pl_stm_tbl[stm] = NULL;

  while(pl_stm_tbl[pl_stm_last_used] == NULL)
    pl_stm_last_used--;
}




/*-------------------------------------------------------------------------*
 * FIND_FREE_STREAM                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Find_Free_Stream(void)
{
  int stm;

  for (stm = 0; stm < pl_stm_tbl_size; stm++)
    if (pl_stm_tbl[stm] == NULL)
      break;

  if (stm == pl_stm_tbl_size)
    Pl_Extend_Array((char **) &pl_stm_tbl, &pl_stm_tbl_size, sizeof(StmInf *), TRUE);

  pl_stm_tbl[stm] = (StmInf *) Malloc(sizeof(StmInf));

  if (stm > pl_stm_last_used)
    pl_stm_last_used = stm;

  return stm;
}




/*-------------------------------------------------------------------------*
 * PL_FIND_STREAM_BY_ALIAS                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Find_Stream_By_Alias(int atom_alias)
{
  AliasInf *alias;

  alias = (AliasInf *) Pl_Hash_Find(pl_alias_tbl, atom_alias);

  return (alias == NULL) ? -1 : alias->stm;
}




/*-------------------------------------------------------------------------*
 * PL_ADD_ALIAS_TO_STREAM                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Add_Alias_To_Stream(int atom_alias, int stm)
{
  AliasInf *alias;
  AliasInf alias_info;

  alias = (AliasInf *) Pl_Hash_Find(pl_alias_tbl, atom_alias);
  if (alias != NULL)
    return alias->stm == stm;	/* fail if assigned to another stream */

  Pl_Extend_Table_If_Needed(&pl_alias_tbl);

  alias_info.atom = atom_alias;
  alias_info.stm = stm;

  alias = (AliasInf *) Pl_Hash_Insert(pl_alias_tbl, (char *) &alias_info, FALSE);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_REASSIGN_ALIAS                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Reassign_Alias(int atom_alias, int stm)
{
  AliasInf *alias;

  alias = (AliasInf *) Pl_Hash_Find(pl_alias_tbl, atom_alias);

  if (alias != NULL)
    alias->stm = stm;
}




/*-------------------------------------------------------------------------*
 * DEL_ALIASES_OF_STREAM                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Del_Aliases_Of_Stream(int stm)
{
  HashScan scan;
  AliasInf *alias;

  for (alias = (AliasInf *) Pl_Hash_First(pl_alias_tbl, &scan); alias;
       alias = (AliasInf *) Pl_Hash_Next(&scan))
    if (alias->stm == stm)
      Pl_Hash_Delete(pl_alias_tbl, alias->atom);
}




/*-------------------------------------------------------------------------*
 * PL_ADD_MIRROR_TO_STREAM                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Add_Mirror_To_Stream(int stm, int m_stm)
{
  StmInf *pstm = pl_stm_tbl[stm];
  StmInf *m_pstm = pl_stm_tbl[m_stm];
  StmLst *m;

  if (stm == m_stm)
    return;

  for(m = pstm->mirror; m ; m = m->next)
    if (m->stm == m_stm)	/* already present */
      return;

  m = (StmLst *) Malloc(sizeof(StmLst));
  m->stm = m_stm;
  m->next = pstm->mirror;
  pstm->mirror = m;

  m = (StmLst *) Malloc(sizeof(StmLst));
  m->stm = stm;
  m->next = m_pstm->mirror_of;

  m_pstm->mirror_of = m;
}




/*-------------------------------------------------------------------------*
 * PL_DEL_MIRROR_FROM_STREAM                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Del_Mirror_From_Stream(int stm, int m_stm)
{
  StmInf *pstm = pl_stm_tbl[stm];
  StmInf *m_pstm = pl_stm_tbl[m_stm];

  if (!Remove_In_Stream_List(m_stm, &pstm->mirror))
    return FALSE;		/* not found */

  Remove_In_Stream_List(stm, &m_pstm->mirror_of);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * UPDATE_MIRRORS_TO_DEL_STREAM                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Update_Mirrors_To_Del_Stream(int stm)
{
  StmInf *pstm = pl_stm_tbl[stm];
  StmInf *m_pstm;
  StmLst *m, *m1;

  m = pstm->mirror;
  while(m)
    {
      m1 = m;
      m_pstm = pl_stm_tbl[m->stm];
      m = m->next;
      Free(m1);
      Remove_In_Stream_List(stm, &m_pstm->mirror_of);
    }

  m = pstm->mirror_of;
  while(m)
    {
      m1 = m;
      m_pstm = pl_stm_tbl[m->stm];
      m = m->next;
      Free(m1);
      Remove_In_Stream_List(stm, &m_pstm->mirror);
    }
}




/*-------------------------------------------------------------------------*
 * REMOVE_IN_STREAM_LIST                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Remove_In_Stream_List(int stm, StmLst **p_start)
{
  StmLst *m;

  for(;;)
    {
      m = *p_start;

      if (m == NULL)
	break;

      if (m->stm == stm)	/* found */
	{
	  *p_start = m->next;
	  Free(m);
	  return TRUE;
	}

      p_start = &m->next;
    }

  return FALSE;			/* not found */
}




/*-------------------------------------------------------------------------*
 * PL_FIND_STREAM_FROM_PSTM                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Find_Stream_From_PStm(StmInf *pstm)
{
  int stm;

  for (stm = 0; stm <= pl_stm_last_used; stm++)
    if (pl_stm_tbl[stm] == pstm)
      return stm;

  return -1;
}




/*-------------------------------------------------------------------------*
 * PL_FLUSH_ALL_STREAMS                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Flush_All_Streams(void)
{
  int stm;

  for (stm = 0; stm <= pl_stm_last_used ; stm++)
    if (pl_stm_tbl[stm])
      Pl_Stream_Flush(pl_stm_tbl[stm]);
}




/*-------------------------------------------------------------------------*
 * PL_SET_STREAM_BUFFERING                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Stream_Buffering(int stm)
{
  StmInf *pstm = pl_stm_tbl[stm];
  FILE *f;

  f = Pl_Stdio_Desc_Of_Stream(stm);
  if (f == NULL)
    {
      pstm->prop.buffering = STREAM_BUFFERING_NONE;
      return;
    }

#ifndef NO_USE_LINEDIT		/* if GUI: inform it about buffering */
  if ((pstm->file == (PlLong) stdout || pstm->file == (PlLong) stderr) && 
      pl_le_hook_set_line_buffering)
    (*pl_le_hook_set_line_buffering)(pstm->prop.buffering != STREAM_BUFFERING_NONE);
  else
#endif
    Pl_Stdio_Set_Buffering(f, pstm->prop.buffering);
}




#ifndef FOR_EXTERNAL_USE

/*-------------------------------------------------------------------------*
 * PL_GET_STREAM_OR_ALIAS                                                  *
 *                                                                         *
 * return the associated stm or -1 if not exist (test==STREAM_CHECK_VALID) *
 *-------------------------------------------------------------------------*/
int
Pl_Get_Stream_Or_Alias(WamWord sora_word, int test)
{
  WamWord word, tag_mask, tag_mask1;
  int atom;
  WamWord *stc_adr;
  PlLong stm = 0;		/* only for the compiler (NB: defined as PlLong to check validity) */
  int perm_oper;


  DEREF(sora_word, word, tag_mask);
  if (tag_mask == TAG_ATM_MASK)	/* alias ? */
    {
      atom = UnTag_ATM(word);
      stm = Pl_Find_Stream_By_Alias(atom);
      goto next_test;
    }

  if (tag_mask == TAG_STC_MASK) /* stream ? */
    {
      stc_adr = UnTag_STC(word);
      DEREF(Arg(stc_adr, 0), word, tag_mask1);
      stm = UnTag_INT(word);

      if (Functor_And_Arity(stc_adr) == stream_1 &&
	  tag_mask1 == TAG_INT_MASK)
	goto next_test;
    }

  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  Pl_Err_Domain(pl_domain_stream_or_alias, sora_word);

 next_test:

  if ((PlULong) stm > (PlULong) pl_stm_last_used || pl_stm_tbl[stm] == NULL)
    {
      if (test == STREAM_CHECK_VALID)
	return -1;

      Pl_Err_Existence(pl_existence_stream, sora_word);
    }

  if (test == STREAM_CHECK_VALID || test == STREAM_CHECK_EXIST)
    goto ok;

  if (test == STREAM_CHECK_INPUT)
    {
      if (pl_stm_tbl[stm]->prop.input)
	goto ok;

      perm_oper = pl_permission_operation_input;
    }
  else				/* test == STREAM_CHECK_OUTPUT */
    {
      if (pl_stm_tbl[stm]->prop.output)
	goto ok;

      perm_oper = pl_permission_operation_output;
    }

  Pl_Err_Permission(perm_oper, pl_permission_type_stream, sora_word);

 ok:
  return (int) stm;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_STREAM_TYPE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_Stream_Type(int stm, Bool check_text, Bool for_input)
{
  int perm_oper;
  int perm_type;
  WamWord sora_word;


  if (check_text)
    {
      if (pl_stm_tbl[stm]->prop.text)
	return;

      perm_type = pl_permission_type_binary_stream;
    }
  else				/* check binary */
    {
      if (!pl_stm_tbl[stm]->prop.text)
	return;

      perm_type = pl_permission_type_text_stream;
    }
  /* here there is an error */
  if (for_input)
    {
      perm_oper = pl_permission_operation_input;
      sora_word = (pl_last_input_sora == NOT_A_WAM_WORD)
	? word_current_input_stream : pl_last_input_sora;
    }
  else				/* for output */
    {
      perm_oper = pl_permission_operation_output;
      sora_word = (pl_last_output_sora == NOT_A_WAM_WORD)
	? word_current_output_stream : pl_last_output_sora;
    }


  Pl_Err_Permission(perm_oper, perm_type, sora_word);
}

#endif /* !FOR_EXTERNAL_USE */


/*-------------------------------------------------------------------------*
 * PL_MAKE_STREAM_TAGGED_WORD                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Make_Stream_Tagged_Word(int stm)
{
  static WamWord h[2];

  h[0] = stream_1;
  h[1] = Tag_INT(stm);

  return Tag_STC(h);
}




/*-------------------------------------------------------------------------*
 * PL_STDIO_IS_REPOSITIONABLE                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Stdio_Is_Repositionable(FILE *f)
{
  int fd = fileno(f);

  return !isatty(fd) && lseek(fd, 0, SEEK_CUR) >= 0;
}




/*-------------------------------------------------------------------------*
 * PL_STDIO_SET_BUFFERING                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Stdio_Set_Buffering(FILE *f, int buffering)
{
  int buff_flag;

  switch (buffering)
    {
    case STREAM_BUFFERING_NONE:
      buff_flag = _IONBF;
      break;

    case STREAM_BUFFERING_LINE:
      buff_flag = _IOLBF;
#ifdef _WIN32
#ifndef NO_USE_LINEDIT
      if (!pl_le_mode != LE_MODE_HOOK)	/* in Win32 console app, line buff = full */
#endif
	buff_flag = _IONBF;	/* I prefer no buffering */
#endif
      break;

    case STREAM_BUFFERING_BLOCK:
      buff_flag = _IOFBF;
      break;
    }

  setvbuf(f, NULL, buff_flag, BUFSIZ);
}




/*-------------------------------------------------------------------------*
 * PL_STDIO_DESC_OF_STREAM                                                 *
 *                                                                         *
 * return a FILE * of a stream or NULL if it is not a stdio stream.        *
 *-------------------------------------------------------------------------*/
FILE *
Pl_Stdio_Desc_Of_Stream(int stm)
{
  StmInf *pstm = pl_stm_tbl[stm];

  if (stm == pl_stm_stdin)		/* works also for stdin with linedit */
    return stdin;

  if (pstm->fct_getc == (StmFct) fgetc)
    return (FILE *) (pstm->file);

  return NULL;
}




/*-------------------------------------------------------------------------*
 * PL_IO_FILENO_OF_STREAM                                                  *
 *                                                                         *
 * return the fileno of a stream or -1 if this stream has not a fileno.    *
 *-------------------------------------------------------------------------*/
int
Pl_Io_Fileno_Of_Stream(int stm)
{
  FILE *f;

  f = Pl_Stdio_Desc_Of_Stream(stm);
  if (f)
    return fileno(f);

  return -1;
}




/*-------------------------------------------------------------------------*
 * The following functions replace standard fgetc/... on stdin if a TTY.   *
 * It uses linedit to provide a more comfortable interface.                *
 * These functions should not be used directly but via the common interface*
 * provided by the Stream_Getc/... functions (see below).                  *
 *-------------------------------------------------------------------------*/

#ifndef NO_USE_LINEDIT

#define SAVE_FOR_REENTRANCY				\
{							\
  int save_sys_var_option_mask = SYS_VAR_OPTION_MASK;	\
  int save_last_read_line = pl_last_read_line;		\
  int save_last_read_col = pl_last_read_col;


#define RESTORE_FOR_REENTRANCY				\
  SYS_VAR_OPTION_MASK = save_sys_var_option_mask;	\
  pl_last_read_line = save_last_read_line;		\
  pl_last_read_col = save_last_read_col;		\
}


/*-------------------------------------------------------------------------*
 * TTY_GETC                                                                *
 *                                                                         *
 * we must take care to reentrancy: e.g. top_level calls TTY_Getc which    *
 * calls LE_FGets + Ctrl_C + b(reak) + new top_level + TTY_Getc...         *
 *-------------------------------------------------------------------------*/
static int
TTY_Getc(void)
{
  int c;
  StmInf *pstm;
  static int tty_linedit_depth = 0;

  if (tty_ptr == NULL)
    {
      if (tty_linedit_depth++ == 0)
	tty_buff = tty_first_buff;
      else
	tty_buff = (char *) Malloc(TTY_BUFFER_SIZE);

				/* tty_ptr must remain NULL for reentrancy */
      SAVE_FOR_REENTRANCY;
      tty_buff = Pl_LE_FGets(tty_buff, TTY_BUFFER_SIZE, pl_le_prompt,
			     pl_use_le_prompt);
      pl_use_le_prompt = 0;
      RESTORE_FOR_REENTRANCY;
      tty_linedit_depth--;

      if (LE_Interrupted_By_Ctrl_C(tty_buff))
	Pl_Execute_A_Continuation((CodePtr) Pl_LE_Get_Ctrl_C_Return_Value());


      if (tty_buff == NULL)
	{
	  c = EOF;
	  goto test_free_buff;
	}

      tty_ptr = tty_buff;
    				/* simulate the echo (+ '\n') on output */
      pstm = pl_stm_tbl[pl_stm_stdout];
      pstm->char_count += strlen(tty_buff);
      pstm->line_count++;
      pstm->line_pos = 0;
    }

  c = *tty_ptr++;

  if (*tty_ptr == '\0')
    {
    test_free_buff:
      if (tty_buff != tty_first_buff)
	Free(tty_buff);
      tty_ptr = NULL;
    }

  return c;
}




/*-------------------------------------------------------------------------*
 * TTY_GET_KEY                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
TTY_Get_Key(Bool echo, Bool catch_ctrl_c)
{
  int c;

  if (tty_ptr != NULL)
    {
      c = *tty_ptr++;

      if (*tty_ptr == '\0')
	{
	  if (tty_buff != tty_first_buff)
	    Free(tty_buff);
	  tty_ptr = NULL;
	}

      return c;
    }

  SAVE_FOR_REENTRANCY;
  c = Pl_LE_Get_Key(echo, catch_ctrl_c);
  RESTORE_FOR_REENTRANCY;

  if (LE_Interrupted_By_Ctrl_C(c))
    Pl_Execute_A_Continuation((CodePtr) Pl_LE_Get_Ctrl_C_Return_Value());

  return c;
}




/*-------------------------------------------------------------------------*
 * TTY_CLEARERR                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
TTY_Clearerr(void)
{
  clearerr(stdin);
}


#endif /* NO_USE_LINEDIT */


/*-------------------------------------------------------------------------*
 * Only the following functions should be used to read/write a stream.     *
 *-------------------------------------------------------------------------*/


#ifdef FOR_EXTERNAL_USE

#define Before_Reading(pstm, file)

#else

#define Before_Reading(pstm, file)					\
{									\
  if (pstm->eof_reached)						\
    {									\
      if (pstm->prop.eof_action == STREAM_EOF_ACTION_ERROR)		\
        Pl_Err_Permission(pl_permission_operation_input,		\
                          pl_permission_type_past_end_of_stream,	\
                          (pl_last_input_sora == NOT_A_WAM_WORD)	\
                          ? word_current_input_stream 			\
			  : pl_last_input_sora);			\
									\
      if (pstm->prop.eof_action == STREAM_EOF_ACTION_EOF_CODE)		\
        return EOF;							\
									\
      /* here: eof_action == STREAM_EOF_ACTION_RESET */			\
      pstm->eof_reached = FALSE;					\
      if (pstm->prop.reposition)					\
        Pl_Stream_Set_Position(pstm, SEEK_SET, 0, 0, 0, 0);		\
      if (pstm->fct_clearerr != STREAM_FCT_UNDEFINED)			\
        (*pstm->fct_clearerr) (file);					\
    }									\
}

#endif /* FOR_EXTERNAL_USE */


#define Update_Counters(pstm, c)		\
  if (c != EOF)                   		\
    pstm->char_count++;       			\
  if (c == '\n')              			\
    {                      			\
      pstm->line_count++;   			\
      pstm->line_pos = 0;     			\
    }                      			\
  else                     			\
    pstm->line_pos++




/*-------------------------------------------------------------------------*
 * BASIC_CALL_FCT_GETC                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Basic_Call_Fct_Getc(StmInf *pstm)
{
  int c;
  StmLst *m;
#ifndef NO_USE_PIPED_STDIN_FOR_CONSULT
  if (SYS_VAR_SAY_GETC && pstm->file == (PlLong) stdin)
    /* could also test pstm->fct_getc == fgetc */
    {
      putchar(CHAR_TO_EMIT_WHEN_CHAR);
      fflush(stdout);
    }
#endif
  c = (*pstm->fct_getc) (pstm->file);

  if (c != EOF)
    for (m = pstm->mirror; m ; m = m->next)
      Pl_Stream_Putc(c, pl_stm_tbl[m->stm]);

  return c;
}




/*-------------------------------------------------------------------------*
 * BASIC_CALL_FCT_PUTC                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Basic_Call_Fct_Putc(int c, StmInf *pstm)
{
  StmLst *m;

  (*pstm->fct_putc) (c, pstm->file);

  for (m = pstm->mirror; m ; m = m->next)
    Pl_Stream_Putc(c, pl_stm_tbl[m->stm]);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_GET_KEY                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Stream_Get_Key(StmInf *pstm, int echo, int catch_ctrl_c)
{
  int c;
  PlLong file = pstm->file;
  Bool simulate;

#ifndef NO_USE_LINEDIT
  if (pstm == pl_stm_tbl[pl_stm_stdin]) /* the stdin stream used with linedit */
    simulate = FALSE;
  else
#endif
    simulate = TRUE;

  Before_Reading(pstm, file);

  if (!PB_Is_Empty(pstm->pb_char))
    {
      PB_Pop(pstm->pb_char, c);
    }
  else
    {
      Start_Protect_Regs_For_Signal;
      if (simulate)
	c = Basic_Call_Fct_Getc(pstm);
#ifndef NO_USE_LINEDIT
      else
	c = TTY_Get_Key(echo, catch_ctrl_c);
#endif
      Stop_Protect_Regs_For_Signal;
    }


  if (simulate && c != '\n')
    {
      while (Basic_Call_Fct_Getc(pstm) >= ' ')
	;

      Update_Counters(pl_stm_tbl[pl_stm_stdout], '\n'); /* reflect \n */
    }

  if (c == EOF)
    pstm->eof_reached = TRUE;

  if (c == '\n')
    PB_Push(pstm->pb_line_pos, pstm->line_pos);

  Update_Counters(pstm, c);

  return c;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_GETC                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Stream_Getc(StmInf *pstm)
{
  int c;
  PlLong file = pstm->file;

  Before_Reading(pstm, file);

  if (!PB_Is_Empty(pstm->pb_char))
    {
      PB_Pop(pstm->pb_char, c);
    }
  else
    {
      Start_Protect_Regs_For_Signal;
      c = Basic_Call_Fct_Getc(pstm);
      Stop_Protect_Regs_For_Signal;
    }
  if (c == EOF)
    pstm->eof_reached = TRUE;

  if (c == '\n')
    PB_Push(pstm->pb_line_pos, pstm->line_pos);

  Update_Counters(pstm, c);

  return c;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_UNGETC                                                        *
 *                                                                         *
 * Several issues should not occur except if more '\n' are unget than read *
 * (when a Pl_Stream_Set_Position() is used the number of read '\n' is 0). *
 *-------------------------------------------------------------------------*/
void
Pl_Stream_Ungetc(int c, StmInf *pstm)
{
  PB_Push(pstm->pb_char, c);
  pstm->eof_reached = FALSE;

  if (pstm->char_count > 0)	/* test should be useless */
    pstm->char_count--;

  if (c == '\n')
    {
      if (pstm->line_count > 0)	/* test should be useless */
	pstm->line_count--;

      if (!PB_Is_Empty(pstm->pb_line_pos))
	PB_Pop(pstm->pb_line_pos, pstm->line_pos);
      else
	pstm->line_pos = 0;	/* should not occur */
    }
  else if (pstm->line_pos > 0)	/* test should be useless */
    pstm->line_pos--;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PEEKC                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Stream_Peekc(StmInf *pstm)
{
  int c;
  PlLong file = pstm->file;


  Before_Reading(pstm, file);

  if (!PB_Is_Empty(pstm->pb_char))
    PB_Top(pstm->pb_char, c);
  else
    {
      c = Basic_Call_Fct_Getc(pstm);
      PB_Push(pstm->pb_char, c);
    }

  return c;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_GETS                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Stream_Gets(char *str, int size, StmInf *pstm)
{
  int c;
  char *p = str;

  for (;;)
    {
      if (p - str >= size)
	break;

      c = Pl_Stream_Getc(pstm);

      if (c == EOF)
	break;

      *p++ = c;

      if (c == '\n')
	break;
    }

  if (c == EOF && p == str)
    return NULL;

  *p = '\0';
  return str;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_GETS_PROMPT                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Stream_Gets_Prompt(char *prompt, StmInf *pstm_o,
		      char *str, int size, StmInf *pstm_i)

{
#ifndef NO_USE_LINEDIT
  char *save_le_prompt = pl_le_prompt;
  int save_use_le_prompt = pl_use_le_prompt;
  pl_le_prompt = prompt;

  pl_use_le_prompt = 1;

  if (pstm_i->fct_getc != TTY_Getc)
#endif
    Pl_Stream_Printf(pstm_o, prompt);

  str = Pl_Stream_Gets(str, size, pstm_i);

#ifndef NO_USE_LINEDIT
  pl_use_le_prompt = save_use_le_prompt;
  pl_le_prompt = save_le_prompt;
#endif
  return str;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PUTC                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Stream_Putc(int c, StmInf *pstm)
{
  Basic_Call_Fct_Putc(c, pstm);
  Update_Counters(pstm, c);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PUTS                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Stream_Puts(char *str, StmInf *pstm)
{
  char *p;
  int c;

  for (p = str; *p; p++)
    {
      c = *p;
      Basic_Call_Fct_Putc(c, pstm); /* like Stream_Putc */
      Update_Counters(pstm, c);
    }

  return p - str;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_PRINTF                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Stream_Printf(StmInf *pstm, char *format, ...)
{
  va_list arg_ptr;
  static char str[BIG_BUFFER];
  char *p;
  int c;


  va_start(arg_ptr, format);
  vsprintf(str, format, arg_ptr);
  va_end(arg_ptr);

  for (p = str; *p; p++)
    {
      c = *p;
      Basic_Call_Fct_Putc(c, pstm); /* like Stream_Putc */
      Update_Counters(pstm, c);
    }

  return p - str;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_FLUSH                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Stream_Flush(StmInf *pstm)
{
  PlLong file = pstm->file;

  if (pstm->prop.output && pstm->fct_flush != STREAM_FCT_UNDEFINED)
    (*pstm->fct_flush) (file);
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_CLOSE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Stream_Close(StmInf *pstm)
{
  PlLong file = pstm->file;
  int ret = 0;

  if (pstm->fct_close != STREAM_FCT_UNDEFINED)
    ret = (*pstm->fct_close) (file);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_END_OF_STREAM                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Stream_End_Of_Stream(StmInf *pstm)
{
  int c;

  if (pstm->prop.eof_action == STREAM_EOF_ACTION_RESET || !pstm->prop.input)
    return STREAM_EOF_NOT;

  if (pstm->eof_reached)
    return STREAM_EOF_PAST;

  c = Pl_Stream_Peekc(pstm);
  if (c == EOF)
    return STREAM_EOF_AT;

  return STREAM_EOF_NOT;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_GET_POSITION                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Stream_Get_Position(StmInf *pstm, PlLong *offset, PlLong *char_count,
		       PlLong *line_count, PlLong *line_pos)
{
  PlLong file = pstm->file;

  *offset = 0;
  if (pstm->prop.reposition && pstm->fct_tell != STREAM_FCT_UNDEFINED)
    {
      if ((*offset = (*pstm->fct_tell) (file)) < 0)
	*offset = 0;
      else
	{
	  *offset = *offset - pstm->pb_char.nb_elems;
	  if (*offset < 0)
	    *offset = 0;
	}
    }

  *char_count = pstm->char_count;

  if (pstm->prop.text)
    {
      *line_count = pstm->line_count;
      *line_pos = pstm->line_pos;
    }
  else
    {
      *line_count = 0;
      *line_pos = 0;
    }
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_SET_POSITION                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Stream_Set_Position(StmInf *pstm, int whence, PlLong offset, PlLong char_count,
		       PlLong line_count, PlLong line_pos)
{
  PlLong file = pstm->file;
  int x;

  x = (*pstm->fct_seek) (file, (PlLong) offset, whence);
  if (x != 0)
    return x;

  pstm->char_count = char_count;

  if (pstm->prop.text)
    {
      pstm->line_count = line_count;
      pstm->line_pos = line_pos;
    }

  if (pstm->eof_reached)
    {
      pstm->eof_reached = FALSE;
      if (pstm->fct_clearerr != STREAM_FCT_UNDEFINED)
	(*pstm->fct_clearerr) (file);
    }

  PB_Init(pstm->pb_char);
  PB_Init(pstm->pb_line_pos);

  return 0;
}




/*-------------------------------------------------------------------------*
 * PL_STREAM_SET_POSITION_LC                                               *
 *                                                                         *
 * Only the line count and the line position are given.                    *
 *-------------------------------------------------------------------------*/
int
Pl_Stream_Set_Position_LC(StmInf *pstm, PlLong line_count, PlLong line_pos)
{
  PlLong file = pstm->file;
  int x;
  PlLong *p;
  int c;
  int offset;
  Bool save_eof_reached;
  int save_char_count, save_line_count, save_line_pos;
  int save_char_nb_elems;


  offset = (*pstm->fct_tell) (file);
  if (offset < 0)
    return offset;

  x = (*pstm->fct_seek) (file, (PlLong) 0, SEEK_SET);
  if (x != 0)
    return x;


  save_eof_reached = pstm->eof_reached;
  save_char_count = pstm->char_count;
  save_line_count = pstm->line_count;
  save_line_pos = pstm->line_pos;
  save_char_nb_elems = pstm->pb_char.nb_elems;


  pstm->char_count = 0;
  pstm->line_count = 0;
  pstm->line_pos = 0;
  pstm->pb_char.nb_elems = 0;

  if (pstm->eof_reached)
    {
      pstm->eof_reached = FALSE;
      if (pstm->fct_clearerr != STREAM_FCT_UNDEFINED)
	(*pstm->fct_clearerr) (file);
    }

  p = &(pstm->line_count);

  while (*p < line_count)
    if (Pl_Stream_Getc(pstm) == EOF)
      goto err;

  p = &(pstm->line_pos);

  while (*p < line_pos)
    {
      if ((c = Pl_Stream_Getc(pstm)) == EOF)
	goto err;
      if (c == '\n')
	goto err;
    }


  PB_Init(pstm->pb_char);
  PB_Init(pstm->pb_line_pos);
  return 0;

err:

  pstm->eof_reached = save_eof_reached;
  pstm->char_count = save_char_count;
  pstm->line_count = save_line_count;
  pstm->line_pos = save_line_pos;
  pstm->pb_char.nb_elems = save_char_nb_elems;

  x = (*pstm->fct_seek) (file, (PlLong) offset, SEEK_SET);
  if (x != 0)
    return x;

  return -2;
}




/*-------------------------------------------------------------------------*
 * The following functions allows the user to handle streams on C strings  *
 * Any stream can be a string stream. To avoid unnecessary malloc/free, we *
 * use as long as possible 2 str stream statically allocated (1 for input, *
 * 1 for output). This optimizes the use of preds like write_to_atom/2,... *
 * NB: The buff of the output static str stream is reused (no free on it). *
 * A dynamic str stream is allocated when it is not possible to use static *
 * ones. Such a str stream is freed at the close.                          *
 *-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*
 * PL_ADD_STR_STREAM                                                       *
 *                                                                         *
 * buff == NULL means output stream mode (str_stream->buff_alloc_size != 0)*
 *-------------------------------------------------------------------------*/
int
Pl_Add_Str_Stream(char *buff, int prop_other)
{
  int stm;
  StmInf *pstm;
  StmProp prop;
  StrSInf *str_stream;

  str_stream = (buff) ? &static_str_stream_rd : &static_str_stream_wr;
  if (str_stream->ptr != NULL)	/* in use ? */
    {
      str_stream = (StrSInf *) Malloc(sizeof(StrSInf));
      str_stream->buff_alloc_size = 0;
    }

  if (buff)
    {
      str_stream->buff = buff;

      prop.mode = STREAM_MODE_READ;
      prop.input = TRUE;
      prop.output = FALSE;
    }
  else
    {
      if (str_stream->buff_alloc_size == 0)
	{
	  str_stream->buff = (char *) Malloc(STR_STREAM_WRITE_BLOCK);
	  str_stream->buff_alloc_size = STR_STREAM_WRITE_BLOCK;
	}

      prop.mode = STREAM_MODE_WRITE;
      prop.input = FALSE;
      prop.output = TRUE;
    }

  str_stream->ptr = str_stream->buff; /* ptr != NULL <=> in use for global */

  prop.text = 1;
  prop.reposition = FALSE;
  prop.buffering = STREAM_BUFFERING_NONE;
  prop.eof_action = STREAM_EOF_ACTION_EOF_CODE;
  prop.special_close = TRUE;
  prop.other = prop_other;

  stm = Find_Free_Stream();
  pstm = pl_stm_tbl[stm];

  Init_Stream_Struct(atom_constant_term_stream, (PlLong) str_stream, prop,
		     (StmFct) Str_Stream_Getc, (StmFct) Str_Stream_Putc,
		     STREAM_FCT_UNDEFINED, STREAM_FCT_UNDEFINED,
		     STREAM_FCT_UNDEFINED, STREAM_FCT_UNDEFINED,
		     STREAM_FCT_UNDEFINED, pstm);

  return stm;
}




/*-------------------------------------------------------------------------*
 * PL_DELETE_STR_STREAM                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Delete_Str_Stream(int stm)
{
  StrSInf *str_stream = (StrSInf *) (pl_stm_tbl[stm]->file);

  if (str_stream == &static_str_stream_rd ||
      str_stream == &static_str_stream_wr)
    {
      str_stream->ptr = NULL;	/* not in use */
    }
  else
    {
      if (str_stream->buff_alloc_size)
	Free(str_stream->buff);
      Free(str_stream);
    }

  Pl_Delete_Stream(stm);
}




/*-------------------------------------------------------------------------*
 * PL_TERM_WRITE_STR_STREAM                                                *
 *                                                                         *
 * only needed for output string stream.                                   *
 *-------------------------------------------------------------------------*/
char *
Pl_Term_Write_Str_Stream(int stm)
{
  StrSInf *str_stream;

  str_stream = (StrSInf *) (pl_stm_tbl[stm]->file);
  *(str_stream->ptr) = '\0';

  return str_stream->buff;
}




/*-------------------------------------------------------------------------*
 * STR_STREAM_GETC                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Str_Stream_Getc(StrSInf *str_stream)
{
  int c;

  c = *(str_stream->ptr);
  if (c == '\0')
    return EOF;

  (str_stream->ptr)++;

  return c;
}




/*-------------------------------------------------------------------------*
 * STR_STREAM_PUTC                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Str_Stream_Putc(int c, StrSInf *str_stream)
{
  int size = str_stream->ptr - str_stream->buff;
  int new_size;


  if (size >= str_stream->buff_alloc_size - 1)	/* -1 for last '\0' */
    {
      new_size = str_stream->buff_alloc_size + STR_STREAM_WRITE_BLOCK;

      str_stream->buff = Realloc(str_stream->buff, new_size);

      str_stream->buff_alloc_size = new_size;
      str_stream->ptr = str_stream->buff + size;
    }

  *(str_stream->ptr)++ = c;
}
