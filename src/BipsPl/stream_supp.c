/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : stream_supp.c                                                   *
 * Descr.: stream support                                                  *
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

#define OBJ_INIT Stream_Supp_Initializer

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

#ifndef NO_USE_LINEDIT
typedef struct			/* TTY input stream information   */
{				/* ------------------------------ */
  FILE *f;			/* associated input stream        */
  int fd_in;			/* associated input  file desc.   */
  int fd_out;			/* associated output file desc.   */
  char buff[TTY_BUFFER_SIZE];	/* current buffer (end with '\0') */
  char *ptr;			/* current pointer into the buff  */
}
TTYInf;
#endif




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int atom_stream;
static WamWord stream_1;

static WamWord word_current_input_stream;
static WamWord word_current_output_stream;

static StrSInf glob_str_stream = { NULL, NULL, 0 };




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Find_Free_Stream(void);



#ifndef NO_USE_LINEDIT

static int TTY_Getc(TTYInf *tty);

static int TTY_Getc_No_Echo(TTYInf *tty);

static void TTY_Putc(int c, TTYInf *tty);

static void TTY_Flush(TTYInf *tty);

static int TTY_Close(TTYInf *tty);

static void TTY_Clearerr(TTYInf *tty);

#endif



#ifdef W32_GUI_CONSOLE

static void W32_Putc(int c, long file);

#endif

static int Str_Stream_Getc(StrSInf *str_stream);

static void Str_Stream_Putc(int c, StrSInf *str_stream);





/*-------------------------------------------------------------------------*
 * STREAM_SUPP_INITIALIZER                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Stream_Supp_Initializer(void)
{
  StmProp prop;
  int istty;

  alias_tbl = Hash_Alloc_Table(START_ALIAS_TBL_SIZE, sizeof(AliasInf));

  atom_stream = Create_Atom("$stream");
  stream_1 = Functor_Arity(atom_stream, 1);

  word_current_input_stream = Tag_ATM(Create_Atom("current_input_stream"));
  word_current_output_stream = Tag_ATM(Create_Atom("current_output_stream"));

  atom_glob_stream_alias = Create_Atom("$glob_stream_alias");

  /* this could be in stream_c.c with an initializer but when */
  /* executed we must be sure that Stream_Supp_Initializer    */
  /* has already been initialized. It is simpler like this    */

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

#ifdef W32_GUI_CONSOLE
  if (le_hook_present)
    istty = 1;
  else
#endif
    istty = (isatty(fileno(stdin)) != 0);

  prop.mode = STREAM_MODE_READ;
  prop.input = TRUE;
  prop.output = FALSE;
  prop.text = TRUE;
  prop.reposition = FALSE;
  prop.eof_action = STREAM_EOF_ACTION_RESET;
#ifdef M_ix86_win32  /* MSVC++ doesn't detect a tty on stdin/out under RXVT */
  prop.buffering = STREAM_BUFFERING_NONE;
#else
  prop.buffering = (istty) ? STREAM_BUFFERING_LINE : STREAM_BUFFERING_BLOCK;
#endif
  prop.tty = istty;
  prop.special_close = FALSE;
  prop.other = 0;

  stm_stdin = Add_Stream(atom_user_input, (long) stdin, prop,
			 NULL, NULL, NULL, STREAM_FCT_UNDEFINED, NULL, NULL,
			 NULL);
  Add_Alias_To_Stream(atom_user_input, stm_stdin);
  stm_input = stm_stdin;



#ifdef W32_GUI_CONSOLE
  if (le_hook_present)		/* linked with W32GUICons */
    istty = 1;
  else
#endif
    istty = (isatty(fileno(stdout)) != 0);

  prop.mode = STREAM_MODE_APPEND;
  prop.input = FALSE;
  prop.output = TRUE;
  prop.text = TRUE;
  prop.reposition = FALSE;
  prop.eof_action = STREAM_EOF_ACTION_RESET;
#ifdef M_ix86_win32  /* MSVC++ doesn't detect a tty on stdin/out under RXVT */
  prop.buffering = STREAM_BUFFERING_NONE;
#else
  prop.buffering = (istty) ? STREAM_BUFFERING_LINE : STREAM_BUFFERING_BLOCK;
#endif
  prop.tty = istty;
  prop.special_close = FALSE;
  prop.other = 0;

  stm_stdout = Add_Stream(atom_user_output, (long) stdout, prop,
			  NULL, NULL, NULL, STREAM_FCT_UNDEFINED, NULL,
			  NULL, NULL);
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
  int d;
  static StmFct def[3][7] = { {(StmFct)
#ifndef M_ix86_bsd
			       fgetc
#else
			       getc
#endif
			       , (StmFct) fputc,
			       (StmFct) fflush, (StmFct) fclose,
			       (StmFct) ftell, (StmFct) fseek,
			       (StmFct) clearerr}
#ifndef NO_USE_LINEDIT
			      ,
			      {(StmFct) TTY_Getc, (StmFct) TTY_Putc,
			       (StmFct) TTY_Flush, (StmFct) TTY_Close,
			       STREAM_FCT_UNDEFINED, STREAM_FCT_UNDEFINED,
			       (StmFct) TTY_Clearerr}
#endif
#ifdef W32_GUI_CONSOLE
			      ,
			      {(StmFct) NULL, (StmFct) W32_Putc,
			       STREAM_FCT_UNDEFINED, STREAM_FCT_UNDEFINED,
			       STREAM_FCT_UNDEFINED, STREAM_FCT_UNDEFINED,
			       STREAM_FCT_UNDEFINED}
#endif
  };


  pstm->atom_file_name = atom_file_name;
  pstm->file = file;
  d = 0;

#ifndef NO_USE_LINEDIT
  if (prop.tty)
    {
      if (prop.input)
	{
	  TTYInf *tty = (TTYInf *) Malloc(sizeof(TTYInf));

	  pstm->file = (long) tty;
	  tty->f = (FILE *) file;
	  tty->fd_in = fileno((FILE *) file);
#if defined(M_ix86_win32)
	  tty->fd_out = 1;
#else
	  {
	    char *p;

	    if ((p = ttyname(tty->fd_in)) == NULL ||
		(tty->fd_out = open(p, O_WRONLY)) == -1)
	      tty->fd_out = 1;
	  }
#endif
	  tty->buff[0] = '\0';
	  tty->ptr = tty->buff;
	  d = 1;
	}
#ifdef W32_GUI_CONSOLE
      else if (le_hook_present)
	{
	  d = 2;
	}
#endif /* W32_GUI_CONSOLE */
    }
#endif /* NO_USE_LINEDIT */

  pstm->prop = prop;

  pstm->fct_getc = (fct_getc) ? fct_getc : def[d][0];
  pstm->fct_putc = (fct_putc) ? fct_putc : def[d][1];
  pstm->fct_flush = (fct_flush) ? fct_flush : def[d][2];
  pstm->fct_close = (fct_close) ? fct_close : def[d][3];
  pstm->fct_tell = (fct_tell) ? fct_tell : def[d][4];
  pstm->fct_seek = (fct_seek) ? fct_seek : def[d][5];
  pstm->fct_clearerr = (fct_clearerr) ? fct_clearerr : def[d][6];

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


  pstm = stm_tbl + stm;
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
#ifndef NO_USE_LINEDIT
  StmInf *pstm = stm_tbl + stm;
  StmProp prop = pstm->prop;
  TTYInf *tty;
#endif

#ifndef NO_USE_LINEDIT
  if (prop.tty && prop.input)
    {
      tty = (TTYInf *) pstm->file;

      if (tty->fd_out > 2)
	close(tty->fd_out);
      Free((char *) (tty));
    }
#endif

  Del_Aliases_Of_Stream(stm);

  stm_tbl[stm].file = 0;

  if (stm == stm_last_used)
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

  for (stm = 0; stm < MAX_STREAM; stm++)
    if (stm_tbl[stm].file == 0)
      break;

#ifndef FOR_EXTERNAL_USE
  if (stm >= MAX_STREAM)
    Pl_Err_Resource(resource_too_many_open_streams);
#endif /* !FOR_EXTERNAL_USE */

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
 * FLUSH_ALL_STREAMS                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Flush_All_Streams(void)
{
  int stm;

  for (stm = 0; stm < MAX_STREAM; stm++)
    if (stm_tbl[stm].file != 0)
      Stream_Flush(stm_tbl + stm);
}




/*-------------------------------------------------------------------------*
 * SET_STREAM_BUFFERING                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Stream_Buffering(int stm)
{
  StmInf *pstm = stm_tbl + stm;
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
#ifdef M_ix86_win32		/* in Win32 console app, line buff = full buff */
      buff_flag = _IONBF;	/* I prefer no buff... */
#else
      buff_flag = _IOLBF;
#endif
      break;

    case STREAM_BUFFERING_BLOCK:
      buff_flag = _IOFBF;
      break;
    }

  /* for those architectures we cannot modify */
  /* buffering when an I/O as already occured */
#if !defined(M_ix86_bsd) && !defined(M_ix86_cygwin)
  setvbuf(f, NULL, buff_flag, BUFSIZ);
#endif
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
      stm = (atom == atom_glob_stream_alias) ? MAX_STREAM
	: Find_Stream_By_Alias(atom);
      goto next_test;
    }

  if (tag_mask == TAG_STC_MASK) /* stream ? */
    {
      stc_adr = UnTag_STC(word);
      DEREF(Arg(stc_adr, 0), word, tag_mask1);
      stm = UnTag_INT(word);

      if (Functor_And_Arity(stc_adr) == stream_1 && tag_mask1 == TAG_INT_MASK)
	goto next_test;
    }

  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  Pl_Err_Domain(domain_stream_or_alias, sora_word);


 next_test:

  if (stm < 0 || stm > MAX_STREAM || stm_tbl[stm].file == 0 ||
      (stm == MAX_STREAM && glob_str_stream.ptr == NULL))	/* global not in use */
    {
      if (test == STREAM_CHECK_VALID)
	return -1;

      Pl_Err_Existence(existence_stream, sora_word);
    }

  if (test == STREAM_CHECK_VALID || test == STREAM_CHECK_EXIST)
    goto ok;

  if (test == STREAM_CHECK_INPUT)
    {
      if (stm_tbl[stm].prop.input)
	goto ok;

      perm_oper = permission_operation_input;
    }
  else				/* test == STREAM_CHECK_OUTPUT */
    {
      if (stm_tbl[stm].prop.output)
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
      if (stm_tbl[stm].prop.text)
	return;

      perm_type = permission_type_binary_stream;
    }
  else				/* check binary */
    {
      if (!stm_tbl[stm].prop.text)
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
  StmInf *pstm = stm_tbl + stm;

#ifndef NO_USE_LINEDIT
  TTYInf *tty;
#endif

  if (pstm->fct_getc == (StmFct) fgetc)
    return (FILE *) (pstm->file);

#ifndef NO_USE_LINEDIT
  tty = (TTYInf *) pstm->file;

  if (pstm->fct_getc == (StmFct) TTY_Getc)
    return tty->f;
#endif

  return NULL;
}




/*-------------------------------------------------------------------------*
 * The following functions replaces standard fgetc/... when the input is a *
 * TTY. It uses linedit to provide a more comfortable interface.           *
 * These functions should not be used directly but via the common interface*
 * provided by the Stream_Getc/... functions (see below).                  *
 *-------------------------------------------------------------------------*/

#ifndef NO_USE_LINEDIT


/*-------------------------------------------------------------------------*
 * TTY_GETC                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
TTY_Getc(TTYInf *tty)
{
  int c;
  StmInf *pstm;

  c = *(tty->ptr)++;
  if (c != '\0')
    return c;

  tty->ptr = tty->buff;		/* before LE_FGets since to allow ctrl+c */
  /* handler needs to use TTY_Getc();      */
  if (
      (LE_FGets
       (tty->buff, TTY_BUFFER_SIZE, tty->fd_in, tty->fd_out, le_prompt,
	1)) == NULL)
    {
      tty->buff[0] = '\0';
      return EOF;
    }

  c = *(tty->ptr)++;

  if (tty->fd_in == 0)
    {				/* simulate the '\n' on the output */
      pstm = stm_tbl + stm_stdout;
      pstm->char_count++;
      pstm->line_count++;
      pstm->line_pos = 0;
    }

  return c;
}




/*-------------------------------------------------------------------------*
 * TTY_GETC_NO_ECHO                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
TTY_Getc_No_Echo(TTYInf *tty)
{
  int c;

  c = *(tty->ptr)++;
  if (c != '\0')
    return c;

  tty->buff[0] = '\0';
  tty->ptr = tty->buff;

  c = LE_FGetc_No_Echo(tty->fd_in, tty->fd_out);

  return c;
}




/*-------------------------------------------------------------------------*
 * TTY_PUTC                                                                *
 *                                                                         *
 * normally this function is not used since TTYInf * are for input streams *
 *-------------------------------------------------------------------------*/
static void
TTY_Putc(int c, TTYInf *tty)
{
  fputc(c, tty->f);
}




/*-------------------------------------------------------------------------*
 * TTY_FLUSH                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
TTY_Flush(TTYInf *tty)
{
  fflush(tty->f);
}




/*-------------------------------------------------------------------------*
 * TTY_CLOSE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
TTY_Close(TTYInf *tty)
{
  return fclose(tty->f);
}




/*-------------------------------------------------------------------------*
 * TTY_CLEARERR                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
TTY_Clearerr(TTYInf *tty)
{
  clearerr(tty->f);
}


#endif /* NO_USE_LINEDIT */




/*-------------------------------------------------------------------------*
 * The following functions replaces standard fputc/... for the std output  *
 * which is mapped to the W32 GUI consoleTTY.                              *
 * These functions should not be used directly but via the common interface*
 * provided by the Stream_Putc/... functions (see below).                  *
 *-------------------------------------------------------------------------*/

#ifdef W32_GUI_CONSOLE


/*-------------------------------------------------------------------------*
 * W32_PUTC                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
W32_Putc(int c, long file)
{
  (*le_hook_put_char) (c, file);
}




#endif /* W32_GUI_CONSOLE */



/*-------------------------------------------------------------------------*
 * Only the following functions should be used to read/write a stream.     *
 *-------------------------------------------------------------------------*/


#ifdef FOR_EXTERNAL_USE

#define Before_Reading(pstm, file)

#else

#define Before_Reading(pstm,file)                                           \
 if (pstm->eof_reached)                                                     \
    {                                                                       \
     if (pstm->prop.eof_action==STREAM_EOF_ACTION_ERROR)                    \
         Pl_Err_Permission(permission_operation_input,                      \
                           permission_type_past_end_of_stream,              \
                           (last_input_sora==NOT_A_WAM_WORD)                \
                                 ? word_current_input_stream                \
                                 : last_input_sora);                        \
                                                                            \
     if (pstm->prop.eof_action==STREAM_EOF_ACTION_EOF_CODE)                 \
         return EOF;                                                        \
                                                                            \
                            /* here: eof_action==STREAM_EOF_ACTION_RESET */ \
     pstm->eof_reached=FALSE;                                               \
     if (pstm->prop.reposition)                                             \
         Stream_Set_Position(pstm,SEEK_SET,0,0,0,0);                        \
     if (pstm->fct_clearerr!=STREAM_FCT_UNDEFINED)                          \
         (*pstm->fct_clearerr)(file);                                       \
    }

#endif /* FOR_EXTERNAL_USE */


#define Update_Counters(pstm,c)                                             \
 if (c!=EOF)                                                                \
     pstm->char_count++;                                                    \
     if (c=='\n')                                                           \
        {                                                                   \
         pstm->line_count++;                                                \
         pstm->line_pos=0;                                                  \
        }                                                                   \
      else                                                                  \
         pstm->line_pos++



/*-------------------------------------------------------------------------*
 * STREAM_GETC_NO_ECHO                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Stream_Getc_No_Echo(StmInf *pstm)
{
  int c;
  long file = pstm->file;

  if (!pstm->prop.tty)
    return Stream_Getc(pstm);

  Before_Reading(pstm, file);

  if (!PB_Is_Empty(pstm->pb_char))
    {
      PB_Pop(pstm->pb_char, c);
    }
  else
    {
      Start_Protect_Regs_For_Signal;
#ifndef NO_USE_LINEDIT
      c = TTY_Getc_No_Echo((TTYInf *) file);
      Stop_Protect_Regs_For_Signal;
    }
#else
      c = pstm->fct_getc((FILE *) file);
      Stop_Protect_Regs_For_Signal;
    }

  if (c != '\n')
    while (pstm->fct_getc((FILE *) file) >= ' ')
      ;

  if (pstm - stm_tbl == stm_stdin)
    Update_Counters((stm_tbl + stm_stdout), '\n');
#endif

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
      c = (*pstm->fct_getc) (file);
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
      c = (*pstm->fct_getc) (file);
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
 * Any stream can be a string stream. There is a global string stream used *
 * by predicates like write_to_atom/2,...                                  *
 *-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*
 * ADD_STR_STREAM                                                          *
 *                                                                         *
 * buff==NULL <=> write mode <=> str_stream->buff_alloc_size!=0            *
 *-------------------------------------------------------------------------*/
int
Add_Str_Stream(Bool use_global, char *buff)
{
  int stm;
  StmInf *pstm;
  StmProp prop;
  StrSInf *str_stream;

  if (use_global)
    {
      str_stream = &glob_str_stream;
      if (buff && str_stream->buff_alloc_size != 0)
	Free(str_stream->buff);
    }
  else
    str_stream = (StrSInf *) Malloc(sizeof(StrSInf));

  if (buff)
    {
      str_stream->buff = buff;
      str_stream->ptr = buff;
      str_stream->buff_alloc_size = 0;

      prop.mode = STREAM_MODE_READ;
      prop.input = TRUE;
      prop.output = FALSE;
    }
  else
    {
      if (!use_global || str_stream->buff_alloc_size == 0)
	str_stream->buff = (char *) Malloc(STR_STREAM_WRITE_BLOCK);

      str_stream->ptr = str_stream->buff;
      str_stream->buff_alloc_size = STR_STREAM_WRITE_BLOCK;

      prop.mode = STREAM_MODE_WRITE;
      prop.input = FALSE;
      prop.output = TRUE;
    }

  prop.text = 1;
  prop.tty = FALSE;
  prop.reposition = FALSE;
  prop.buffering = STREAM_BUFFERING_NONE;
  prop.eof_action = STREAM_EOF_ACTION_EOF_CODE;
  prop.special_close = TRUE;
  prop.other = 0;

  stm = (use_global) ? MAX_STREAM : Find_Free_Stream();
  pstm = stm_tbl + stm;

  pstm->atom_file_name = atom_void;

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
  StrSInf *str_stream;

  if (stm == MAX_STREAM)	/* the global str stream */
    {
      glob_str_stream.ptr = NULL;	/* ie. global stream not in use */
      return;
    }

  str_stream = (StrSInf *) (stm_tbl[stm].file);
  if (str_stream->buff_alloc_size)
    Free(str_stream->buff);
  Free(str_stream);

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

  str_stream = (StrSInf *) (stm_tbl[stm].file);
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

