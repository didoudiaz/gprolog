/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : stream_supp.c                                                   *
 * Descr.: stream support                                                  *
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

#ifdef M_ix86_win32
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

#define BIG_BUFFER                 2048




	  /* Error Messages */

#define ERR_TELL_OR_SEEK_UNDEFINED "fct tell or seek undefined\n"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static void Init_Stream_Supp();
void (*init_stream_supp)() = Init_Stream_Supp; /* overwrite var of engine.c */

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



#ifndef NO_USE_LINEDIT

static int TTY_Getc(void);

static int TTY_Get_Key(Bool echo, Bool catch_ctrl_c);

static void TTY_Clearerr(void);

#endif

static int Basic_Call_Fct_Getc(StmInf *pstm);

static int Str_Stream_Getc(StrSInf *str_stream);

static void Str_Stream_Putc(int c, StrSInf *str_stream);





/*-------------------------------------------------------------------------*
 * INIT_STREAM_SUPP                                                        *
 *                                                                         *
 * no declare as other initializers, since we must be sure it has been     *
 * initialized before others.                                              *
 *-------------------------------------------------------------------------*/
static void
Init_Stream_Supp(void)
{
  StmProp prop;
  int istty;
  StmFct fct_putc = NULL;
  StmFct fct_flush = NULL;

  stm_tbl_size = 32;
  stm_tbl = (StmInf **) Calloc(stm_tbl_size, sizeof(StmInf *));
  stm_last_used = -1;

  alias_tbl = Hash_Alloc_Table(START_ALIAS_TBL_SIZE, sizeof(AliasInf));

  atom_stream = Create_Atom("$stream");
  stream_1 = Functor_Arity(atom_stream, 1);

  atom_constant_term_stream = Create_Atom("constant term stream");

  word_current_input_stream = Tag_ATM(Create_Atom("current_input_stream"));
  word_current_output_stream = Tag_ATM(Create_Atom("current_output_stream"));

  atom_user_input = Create_Atom("user_input");
  atom_user_output = Create_Atom("user_output");

  atom_top_level_input = Create_Atom("top_level_input");
  atom_top_level_output = Create_Atom("top_level_output");

  atom_debugger_input = Create_Atom("debugger_input");
  atom_debugger_output = Create_Atom("debugger_output");

  atom_read = Create_Atom("read");
  atom_write = Create_Atom("write");
  atom_append = Create_Atom("append");

  atom_reposition = Create_Atom("reposition");

  atom_stream_position = Create_Atom("$stream_position");

  atom_text = Create_Atom("text");
  atom_binary = Create_Atom("binary");

  atom_error = Create_Atom("error");
  atom_eof_code = Create_Atom("eof_code");
  atom_reset = Create_Atom("reset");

  atom_none = Create_Atom("none");
  atom_line = Create_Atom("line");
  atom_block = Create_Atom("block");

  atom_not = Create_Atom("not");
  atom_at = Create_Atom("at");
  atom_past = Create_Atom("past");

  atom_bof = Create_Atom("bof");
  atom_current = Create_Atom("current");
  atom_eof = Create_Atom("eof");

  le_prompt = "";

  istty = (use_gui) || isatty(0);

  prop.mode = STREAM_MODE_READ;
  prop.input = TRUE;
  prop.output = FALSE;
  prop.text = TRUE;
  prop.reposition = FALSE;
  prop.eof_action = STREAM_EOF_ACTION_RESET;
  prop.buffering = (istty) ? STREAM_BUFFERING_LINE : STREAM_BUFFERING_BLOCK;
  prop.special_close = FALSE;
  prop.other = 0;

#ifndef NO_USE_LINEDIT
  if (istty)
    stm_stdin = Add_Stream(atom_user_input, (long) stdin, prop,
			   (StmFct) TTY_Getc, STREAM_FCT_UNDEFINED,
			   STREAM_FCT_UNDEFINED, STREAM_FCT_UNDEFINED,
			   STREAM_FCT_UNDEFINED, STREAM_FCT_UNDEFINED,
			   (StmFct) TTY_Clearerr);
  else
#endif
    stm_stdin = Add_Stream(atom_user_input, (long) stdin, prop,
			   NULL, NULL, NULL, STREAM_FCT_UNDEFINED, NULL,
			   NULL, NULL);

  Add_Alias_To_Stream(atom_user_input, stm_stdin);
  stm_input = stm_stdin;

  istty = (use_gui) || isatty(1);

  prop.mode = STREAM_MODE_APPEND;
  prop.input = FALSE;
  prop.output = TRUE;
  prop.text = TRUE;
  prop.reposition = FALSE;
  prop.eof_action = STREAM_EOF_ACTION_RESET;
  prop.buffering = (istty) ? STREAM_BUFFERING_LINE : STREAM_BUFFERING_BLOCK;
  prop.special_close = FALSE;
  prop.other = 0;

#if !defined(NO_USE_LINEDIT) && defined(M_ix86_win32)
		/* ok for both GUI and console EOM<->ANSI conversion */
  if (le_hook_put_char && istty)
    fct_putc = (StmFct) le_hook_put_char;

  if (le_hook_flush && istty)
    fct_flush = (StmFct) le_hook_flush;
#endif

  stm_stdout = Add_Stream(atom_user_output, (long) stdout, prop,
			  NULL, fct_putc, fct_flush,
			  STREAM_FCT_UNDEFINED, NULL, NULL, NULL);
  Add_Alias_To_Stream(atom_user_output, stm_stdout);
  stm_output = stm_stdout;

  stm_top_level_input = stm_debugger_input = stm_input;
  stm_top_level_output = stm_debugger_output = stm_output;

  Add_Alias_To_Stream(atom_top_level_input, stm_top_level_input);
  Add_Alias_To_Stream(atom_top_level_output, stm_top_level_output);
  Add_Alias_To_Stream(atom_debugger_input, stm_debugger_input);
  Add_Alias_To_Stream(atom_debugger_output, stm_debugger_output);
}



#ifndef FOR_EXTERNAL_USE

/*-------------------------------------------------------------------------*
 * GET_STREAM_MODE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
StmProp
Get_Stream_Mode(WamWord mode_word, Bool only_rw, char *open_str)
{
  int atom;
  StmProp prop;

  atom = Rd_Atom_Check(mode_word);

  if (atom == atom_read)
    {
      prop.mode = STREAM_MODE_READ;
      prop.input = TRUE;
      prop.output = FALSE;
      strcpy(open_str, "r");
      goto end;
    }

  if (atom == atom_write)
    {
      prop.mode = STREAM_MODE_WRITE;
      prop.input = FALSE;
      prop.output = TRUE;
      strcpy(open_str, "w");
      goto end;
    }

  if (only_rw)
    Pl_Err_Domain(domain_io_mode, mode_word);

  if (atom == atom_append)
    {
      prop.mode = STREAM_MODE_APPEND;
      prop.input = FALSE;
      prop.output = TRUE;
      strcpy(open_str, "a");
      goto end;
    }

  Pl_Err_Domain(domain_io_mode, mode_word);

end:
  return prop;
}

#endif /* !FOR_EXTERNAL_USE */

/*-------------------------------------------------------------------------*
 * INIT_STREAM_STRUCT                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Init_Stream_Struct(int atom_file_name, long file, StmProp prop,
		   StmFct fct_getc, StmFct fct_putc,
		   StmFct fct_flush, StmFct fct_close,
		   StmFct fct_tell, StmFct fct_seek, StmFct fct_clearerr,
		   StmInf *pstm)
{
  pstm->atom_file_name = atom_file_name;
  pstm->file = file;
  pstm->prop = prop;

#define INIT_FCT(f, d) pstm->f = (f) ? f : (StmFct) d

  INIT_FCT(fct_getc, fgetc);
  INIT_FCT(fct_putc, fputc);
  INIT_FCT(fct_flush, fflush);
  INIT_FCT(fct_close, fclose);
  INIT_FCT(fct_tell, ftell);
  INIT_FCT(fct_seek, fseek);
  INIT_FCT(fct_clearerr, clearerr);

  /* Works only because putc will be called with c as 1st and flush's arg
     is ignored */

  pstm->eof_reached = FALSE;
  PB_Init(pstm->pb_char);

  pstm->char_count = 0;
  pstm->line_count = 0;
  pstm->line_pos = 0;
  PB_Init(pstm->pb_line_pos);
}




/*-------------------------------------------------------------------------*
 * ADD_STREAM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Add_Stream(int atom_file_name, long file, StmProp prop,
	   StmFct fct_getc, StmFct fct_putc,
	   StmFct fct_flush, StmFct fct_close,
	   StmFct fct_tell, StmFct fct_seek, StmFct fct_clearerr)
{
  int stm;
  StmInf *pstm;

  stm = Find_Free_Stream();

  if (prop.reposition && (fct_tell == STREAM_FCT_UNDEFINED ||
			  fct_seek == STREAM_FCT_UNDEFINED))
    Fatal_Error(ERR_TELL_OR_SEEK_UNDEFINED);


  pstm = stm_tbl[stm];
  Init_Stream_Struct(atom_file_name, file, prop, fct_getc, fct_putc,
		     fct_flush, fct_close, fct_tell, fct_seek, fct_clearerr,
		     pstm);

  Set_Stream_Buffering(stm);

  return stm;
}




/*-------------------------------------------------------------------------*
 * REMOVE_STREAM                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Delete_Stream(int stm)
{
  Del_Aliases_Of_Stream(stm);

  Free(stm_tbl[stm]);
  stm_tbl[stm] = NULL;

  while(stm_tbl[stm_last_used] == NULL)
    stm_last_used--;
}




/*-------------------------------------------------------------------------*
 * FIND_FREE_STREAM                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Find_Free_Stream(void)
{
  int stm;

  for (stm = 0; stm < stm_tbl_size; stm++)
    if (stm_tbl[stm] == NULL)
      break;

  if (stm == stm_tbl_size)
    Extend_Array((char **) &stm_tbl, &stm_tbl_size, sizeof(StmInf *), TRUE);

  stm_tbl[stm] = (StmInf *) Malloc(sizeof(StmInf));

  if (stm > stm_last_used)
    stm_last_used = stm;

  return stm;
}




/*-------------------------------------------------------------------------*
 * FIND_STREAM_BY_ALIAS                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Find_Stream_By_Alias(int atom_alias)
{
  AliasInf *alias;

  alias = (AliasInf *) Hash_Find(alias_tbl, atom_alias);

  return (alias == NULL) ? -1 : alias->stm;
}




/*-------------------------------------------------------------------------*
 * ADD_ALIAS_TO_STREAM                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Add_Alias_To_Stream(int atom_alias, int stm)
{
  AliasInf *alias;
  AliasInf alias_info;

  alias = (AliasInf *) Hash_Find(alias_tbl, atom_alias);
  if (alias != NULL)
    return alias->stm == stm;	/* fail if assigned to another stream */

  Extend_Table_If_Needed(&alias_tbl);

  alias_info.atom = atom_alias;
  alias_info.stm = stm;

  alias = (AliasInf *) Hash_Insert(alias_tbl, (char *) &alias_info, FALSE);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * REASSIGN_ALIAS                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reassign_Alias(int atom_alias, int stm)
{
  AliasInf *alias;

  alias = (AliasInf *) Hash_Find(alias_tbl, atom_alias);

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

  for (alias = (AliasInf *) Hash_First(alias_tbl, &scan); alias;
       alias = (AliasInf *) Hash_Next(&scan))
    if (alias->stm == stm)
      Hash_Delete(alias_tbl, alias->atom);
}




/*-------------------------------------------------------------------------*
 * FIND_STREAM_FROM_PSTM                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Find_Stream_From_PStm(StmInf *pstm)
{
  int stm;

  for (stm = 0; stm <= stm_last_used; stm++)
    if (stm_tbl[stm] == pstm)
      return stm;

  return -1;
}




/*-------------------------------------------------------------------------*
 * FLUSH_ALL_STREAMS                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Flush_All_Streams(void)
{
  int stm;

  for (stm = 0; stm <= stm_last_used ; stm++)
    if (stm_tbl[stm])
      Stream_Flush(stm_tbl[stm]);
}




/*-------------------------------------------------------------------------*
 * SET_STREAM_BUFFERING                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Stream_Buffering(int stm)
{
  StmInf *pstm = stm_tbl[stm];
  int buff_flag;
  FILE *f;

  f = File_Star_Of_Stream(stm);
  if (f == NULL)
    {
      pstm->prop.buffering = STREAM_BUFFERING_NONE;
      return;
    }

  switch (pstm->prop.buffering)
    {
    case STREAM_BUFFERING_NONE:
      buff_flag = _IONBF;
      break;

    case STREAM_BUFFERING_LINE:
      buff_flag = _IOLBF;
#ifdef M_ix86_win32
      if (!use_gui)		/* in Win32 console app, line buff = full */
	buff_flag = _IONBF;	/* I prefer no buffering */
#endif
      break;

    case STREAM_BUFFERING_BLOCK:
      buff_flag = _IOFBF;
      break;
    }

  /* for those architectures we cannot modify */
  /* buffering when an I/O as already occured */
#if defined(M_ix86_bsd) || defined(M_ix86_cygwin)
  return;
#endif

#ifndef NO_USE_LINEDIT		/* if use_gui == 1 */
  if (pstm->file == (long) stdout && le_hook_set_line_buffering)
    (*le_hook_set_line_buffering)(buff_flag != _IONBF);
  else
#endif
    setvbuf(f, NULL, buff_flag, BUFSIZ);
}



#ifndef FOR_EXTERNAL_USE

/*-------------------------------------------------------------------------*
 * GET_STREAM_OR_ALIAS                                                     *
 *                                                                         *
 * return the associated stm or -1 if not exist (test==STREAM_CHECK_VALID) *
 *-------------------------------------------------------------------------*/
int
Get_Stream_Or_Alias(WamWord sora_word, int test)
{
  WamWord word, tag_mask, tag_mask1;
  int atom;
  WamWord *stc_adr;
  int stm = 0;			/* only for the compiler */
  int perm_oper;


  DEREF(sora_word, word, tag_mask);
  if (tag_mask == TAG_ATM_MASK)	/* alias ? */
    {
      atom = UnTag_ATM(word);
      stm = Find_Stream_By_Alias(atom);
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

  Pl_Err_Domain(domain_stream_or_alias, sora_word);

 next_test:

  if ((unsigned) stm > stm_last_used || stm_tbl[stm] == NULL)
    {
      if (test == STREAM_CHECK_VALID)
	return -1;

      Pl_Err_Existence(existence_stream, sora_word);
    }

  if (test == STREAM_CHECK_VALID || test == STREAM_CHECK_EXIST)
    goto ok;

  if (test == STREAM_CHECK_INPUT)
    {
      if (stm_tbl[stm]->prop.input)
	goto ok;

      perm_oper = permission_operation_input;
    }
  else				/* test == STREAM_CHECK_OUTPUT */
    {
      if (stm_tbl[stm]->prop.output)
	goto ok;

      perm_oper = permission_operation_output;
    }

  Pl_Err_Permission(perm_oper, permission_type_stream, sora_word);

 ok:
  return stm;
}




/*-------------------------------------------------------------------------*
 * CHECK_STREAM_TYPE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_Stream_Type(int stm, Bool check_text, Bool for_input)
{
  int perm_oper;
  int perm_type;
  WamWord sora_word;


  if (check_text)
    {
      if (stm_tbl[stm]->prop.text)
	return;

      perm_type = permission_type_binary_stream;
    }
  else				/* check binary */
    {
      if (!stm_tbl[stm]->prop.text)
	return;

      perm_type = permission_type_text_stream;
    }
  /* here there is an error */
  if (for_input)
    {
      perm_oper = permission_operation_input;
      sora_word = (last_input_sora == NOT_A_WAM_WORD)
	? word_current_input_stream : last_input_sora;
    }
  else				/* for output */
    {
      perm_oper = permission_operation_output;
      sora_word = (last_output_sora == NOT_A_WAM_WORD)
	? word_current_output_stream : last_output_sora;
    }


  Pl_Err_Permission(perm_oper, perm_type, sora_word);
}

#endif /* !FOR_EXTERNAL_USE */


/*-------------------------------------------------------------------------*
 * MAKE_STREAM_TAGGED_WORD                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Make_Stream_Tagged_Word(int stm)
{
  static WamWord h[2];

  h[0] = stream_1;
  h[1] = Tag_INT(stm);

  return Tag_STC(h);
}




/*-------------------------------------------------------------------------*
 * FILE_NUMBER_OF_STREAM                                                   *
 *                                                                         *
 * return the fileno of a stream or -1 if this stream has not a fileno.    *
 *-------------------------------------------------------------------------*/
int
File_Number_Of_Stream(int stm)
{
  FILE *f;

  f = File_Star_Of_Stream(stm);
  if (f)
    return fileno(f);

  return -1;
}




/*-------------------------------------------------------------------------*
 * FILE_STAR_OF_STREAM                                                     *
 *                                                                         *
 * return a FILE * of a stream or NULL if it is not a stdio stream.        *
 *-------------------------------------------------------------------------*/
FILE *
File_Star_Of_Stream(int stm)
{
  StmInf *pstm = stm_tbl[stm];

  if (stm == stm_stdin)		/* works also for stdin with linedit */
    return stdin;

  if (pstm->fct_getc == (StmFct) fgetc)
    return (FILE *) (pstm->file);

  return NULL;
}




/*-------------------------------------------------------------------------*
 * The following functions replaces standard fgetc/... on stdin if a TTY.  *
 * It uses linedit to provide a more comfortable interface.                *
 * These functions should not be used directly but via the common interface*
 * provided by the Stream_Getc/... functions (see below).                  *
 *-------------------------------------------------------------------------*/

#ifndef NO_USE_LINEDIT

#define SAVE_FOR_REENTRANCY				\
{							\
  int save_sys_var_option_mask = SYS_VAR_OPTION_MASK;	\
  int save_last_read_line = last_read_line;		\
  int save_last_read_col = last_read_col;


#define RESTORE_FOR_REENTRANCY				\
  SYS_VAR_OPTION_MASK = save_sys_var_option_mask;	\
  last_read_line = save_last_read_line;			\
  last_read_col = save_last_read_col;			\
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
      tty_buff = LE_FGets(tty_buff, TTY_BUFFER_SIZE, le_prompt, 1);
      RESTORE_FOR_REENTRANCY;
      tty_linedit_depth--;

      if (LE_Interrupted_By_Ctrl_C(tty_buff))
	Execute_A_Continuation((CodePtr) LE_Get_Ctrl_C_Return_Value());
	

      if (tty_buff == NULL)
	{
	  c = EOF;
	  goto test_free_buff;
	}

      tty_ptr = tty_buff;
    				/* simulate the echo (+ '\n') on output */
      pstm = stm_tbl[stm_stdout];
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
  c = LE_Get_Key(echo, catch_ctrl_c);
  RESTORE_FOR_REENTRANCY;

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

#define Before_Reading(pstm, file)					    \
{									    \
  if (pstm->eof_reached)						    \
    {									    \
      if (pstm->prop.eof_action == STREAM_EOF_ACTION_ERROR)		    \
        Pl_Err_Permission(permission_operation_input,			    \
                          permission_type_past_end_of_stream,		    \
                          (last_input_sora == NOT_A_WAM_WORD)		    \
                          ? word_current_input_stream : last_input_sora);   \
 									    \
      if (pstm->prop.eof_action == STREAM_EOF_ACTION_EOF_CODE)		    \
        return EOF;							    \
 									    \
      /* here: eof_action==STREAM_EOF_ACTION_RESET */			    \
      pstm->eof_reached = FALSE;					    \
      if (pstm->prop.reposition)					    \
        Stream_Set_Position(pstm, SEEK_SET, 0, 0, 0, 0);		    \
      if (pstm->fct_clearerr != STREAM_FCT_UNDEFINED)			    \
        (*pstm->fct_clearerr) (file);					    \
    }									    \
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
 * only useful if !defined(NO_USE_PIPED_STDIN_FOR_CONSULT)                 *
 *-------------------------------------------------------------------------*/
static int
Basic_Call_Fct_Getc(StmInf *pstm)
{
  int c;
#ifndef NO_USE_PIPED_STDIN_FOR_CONSULT
  if (SYS_VAR_SAY_GETC && pstm->file == (long) stdin)
    /* could also test pstm->fct_getc == fgetc) */
    {
      putchar(CHAR_TO_EMIT_WHEN_CHAR);
      fflush(stdout);
    }
#endif
  c = (*pstm->fct_getc) (pstm->file);
 
  return c;
}




/*-------------------------------------------------------------------------*
 * STREAM_GET_KEY                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Stream_Get_Key(StmInf *pstm, int echo, int catch_ctrl_c)
{
  int c;
  long file = pstm->file;
  Bool simulate;

#ifndef NO_USE_LINEDIT
  if (pstm == stm_tbl[stm_stdin]) /* the stdin stream used with linedit */
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

      Update_Counters(stm_tbl[stm_stdout], '\n'); /* reflect \n */
    }

  if (c == EOF)
    pstm->eof_reached = TRUE;

  if (c == '\n')
    PB_Push(pstm->pb_line_pos, pstm->line_pos);

  Update_Counters(pstm, c);

  return c;
}




/*-------------------------------------------------------------------------*
 * STREAM_GETC                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Stream_Getc(StmInf *pstm)
{
  int c;
  long file = pstm->file;

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
 * STREAM_UNGETC                                                           *
 *                                                                         *
 * Several issues should not occur except if more '\n' are unget than read *
 * (when a Stream_Set_Position() is used the number of read '\n' is 0).    *
 *-------------------------------------------------------------------------*/
void
Stream_Ungetc(int c, StmInf *pstm)
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
 * STREAM_PEEKC                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Stream_Peekc(StmInf *pstm)
{
  int c;
  long file = pstm->file;


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
 * STREAM_GETS                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Stream_Gets(char *str, int size, StmInf *pstm)
{
  int c;
  char *p = str;

  for (;;)
    {
      if (p - str >= size)
	break;

      c = Stream_Getc(pstm);

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
 * STREAM_GET_LINE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Stream_Gets_Prompt(char *prompt, StmInf *pstm_o,
		   char *str, int size, StmInf *pstm_i)

{
#ifndef NO_USE_LINEDIT
  char *save_le_prompt = le_prompt;
  le_prompt = prompt;

  if (pstm_i->fct_getc != TTY_Getc)
#endif
    Stream_Printf(pstm_o, prompt);

  str = Stream_Gets(str, size, pstm_i);

#ifndef NO_USE_LINEDIT
  le_prompt = save_le_prompt;
#endif
  return str;
}




/*-------------------------------------------------------------------------*
 * STREAM_PUTC                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Stream_Putc(int c, StmInf *pstm)
{
  long file = pstm->file;

  (*pstm->fct_putc) (c, file);

  Update_Counters(pstm, c);
}




/*-------------------------------------------------------------------------*
 * STREAM_PUTS                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Stream_Puts(char *str, StmInf *pstm)
{
  long file = pstm->file;
  char *p;
  int c;

  for (p = str; *p; p++)
    {
      c = *p;
      (*pstm->fct_putc) (c, file);	/* like Stream_Putc */
      Update_Counters(pstm, c);
    }

  return p - str;
}




/*-------------------------------------------------------------------------*
 * STREAM_PRINTF                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Stream_Printf(StmInf *pstm, char *format, ...)
{
  long file = pstm->file;
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
      (*pstm->fct_putc) (c, file);	/* like Stream_Putc */
      Update_Counters(pstm, c);
    }

  return p - str;
}




/*-------------------------------------------------------------------------*
 * STREAM_FLUSH                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Stream_Flush(StmInf *pstm)
{
  long file = pstm->file;

  if (pstm->prop.output && pstm->fct_flush != STREAM_FCT_UNDEFINED)
    (*pstm->fct_flush) (file);
}




/*-------------------------------------------------------------------------*
 * STREAM_CLOSE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Stream_Close(StmInf *pstm)
{
  long file = pstm->file;
  int ret = 0;

  if (pstm->fct_close != STREAM_FCT_UNDEFINED)
    ret = (*pstm->fct_close) (file);

  return 0;
}




/*-------------------------------------------------------------------------*
 * STREAM_END_OF_STREAM                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Stream_End_Of_Stream(StmInf *pstm)
{
  int c;

  if (pstm->prop.eof_action == STREAM_EOF_ACTION_RESET || !pstm->prop.input)
    return STREAM_EOF_NOT;

  if (pstm->eof_reached)
    return STREAM_EOF_PAST;

  c = Stream_Peekc(pstm);
  if (c == EOF)
    return STREAM_EOF_AT;

  return STREAM_EOF_NOT;
}




/*-------------------------------------------------------------------------*
 * STREAM_GET_POSITION                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Stream_Get_Position(StmInf *pstm, int *offset, int *char_count,
		    int *line_count, int *line_pos)
{
  long file = pstm->file;

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
 * STREAM_SET_POSITION                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Stream_Set_Position(StmInf *pstm, int whence, int offset, int char_count,
		    int line_count, int line_pos)
{
  long file = pstm->file;
  int x;

  x = (*pstm->fct_seek) (file, (long) offset, whence);
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
 * STREAM_SET_POSITION_LC                                                  *
 *                                                                         *
 * Only the line count and the line position are given.                    *
 *-------------------------------------------------------------------------*/
int
Stream_Set_Position_LC(StmInf *pstm, int line_count, int line_pos)
{
  long file = pstm->file;
  int x;
  int *p;
  int c;
  int offset;
  Bool save_eof_reached;
  int save_char_count, save_line_count, save_line_pos;
  int save_char_nb_elems;


  offset = (*pstm->fct_tell) (file);
  if (offset < 0)
    return offset;

  x = (*pstm->fct_seek) (file, (long) 0, SEEK_SET);
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
    if (Stream_Getc(pstm) == EOF)
      goto err;

  p = &(pstm->line_pos);

  while (*p < line_pos)
    {
      if ((c = Stream_Getc(pstm)) == EOF)
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

  x = (*pstm->fct_seek) (file, (long) offset, SEEK_SET);
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
 * ADD_STR_STREAM                                                          *
 *                                                                         *
 * buff == NULL means output stream mode (str_stream->buff_alloc_size != 0)*
 *-------------------------------------------------------------------------*/
int
Add_Str_Stream(char *buff, int prop_other)
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
  pstm = stm_tbl[stm];

  pstm->atom_file_name = atom_constant_term_stream;

  pstm->file = (long) str_stream;
  pstm->prop = prop;

  pstm->fct_getc = (StmFct) Str_Stream_Getc;
  pstm->fct_putc = (StmFct) Str_Stream_Putc;
  pstm->fct_flush = STREAM_FCT_UNDEFINED;
  pstm->fct_close = STREAM_FCT_UNDEFINED;
  pstm->fct_tell = STREAM_FCT_UNDEFINED;
  pstm->fct_seek = STREAM_FCT_UNDEFINED;
  pstm->fct_clearerr = STREAM_FCT_UNDEFINED;

  pstm->eof_reached = FALSE;

  PB_Init(pstm->pb_char);

  pstm->char_count = 0;
  pstm->line_count = 0;
  pstm->line_pos = 0;
  PB_Init(pstm->pb_line_pos);

  return stm;
}




/*-------------------------------------------------------------------------*
 * REMOVE_STR_STREAM                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Delete_Str_Stream(int stm)
{
  StrSInf *str_stream = (StrSInf *) (stm_tbl[stm]->file);

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

  Delete_Stream(stm);
}




/*-------------------------------------------------------------------------*
 * TERM_WRITE_STR_STREAM                                                   *
 *                                                                         *
 * only needed for output string stream.                                   *
 *-------------------------------------------------------------------------*/
char *
Term_Write_Str_Stream(int stm)
{
  StrSInf *str_stream;

  str_stream = (StrSInf *) (stm_tbl[stm]->file);
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
