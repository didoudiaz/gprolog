/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : stream_supp.h                                                   *
 * Descr.: stream support - header file                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


#define STREAM_PB_SIZE             8	/* push back buffer size */




#define STREAM_MODE_READ           0
#define STREAM_MODE_WRITE          1
#define STREAM_MODE_APPEND         2

#define STREAM_EOF_ACTION_ERROR    0
#define STREAM_EOF_ACTION_EOF_CODE 1
#define STREAM_EOF_ACTION_RESET    2

#define STREAM_BUFFERING_NONE      0
#define STREAM_BUFFERING_LINE      1
#define STREAM_BUFFERING_BLOCK     2

#define STREAM_EOF_NOT             0
#define STREAM_EOF_AT              1
#define STREAM_EOF_PAST            2



					/* possible values for 'test' in Pl_Get_Stream_Or_Alias */
#define STREAM_CHECK_VALID         0	/* simply a valid stream or alias */
#define STREAM_CHECK_EXIST         1	/* valid and exist */
#define STREAM_CHECK_INPUT         2	/* valid, exist and mode=input  */
#define STREAM_CHECK_OUTPUT        3	/* valid, exist and mode=output */
				/* additional OR masks */
#define STREAM_CHECK_ACCEPT_VAR    (1 << 8)
#define STREAM_CHECK_ACCEPT_NULL   (1 << 9)
#define STREAM_CHECK_HAS_FILENO    (1 << 10)




#define STREAM_FCT_UNDEFINED       ((StmFct) (-1)) /* for optional fct */




				     /* Constant term streams (prop.other) */
#define TERM_STREAM_ATOM           1 /* values also used in stream.pl */
#define TERM_STREAM_CHARS          2
#define TERM_STREAM_CODES          3




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct			/* Stream properties              */
{				/* ------------------------------ */
  unsigned mode:2;		/* see STREAM_MODE_xxx defs       */
  unsigned input:1;		/* is it an input  stream ?       */
  unsigned output:1;		/* is it an output stream ?       */
  unsigned text:1;		/* is it a text stream . (or bin) */
  unsigned reposition:1;	/* can it be repositioned ?       */
  unsigned eof_action:2;	/* see STREAM_EOF_ACTION_xxx defs */
  unsigned buffering:2;		/* see STREAM_BUFFERING_xxx defs  */
  unsigned special_close:1;	/* does it need a special close ? */
  unsigned other:5;		/* other prop (1,2,3=term_streams */
}				/*             4=socket_stream)   */
StmProp;




typedef struct			/* Push Back stack                */
{				/* ------------------------------ */
  int buff[STREAM_PB_SIZE];	/* the buffer                     */
  int *ptr;			/* pointer into the buffer        */
  int nb_elems;			/* # of elements in the buffer    */
}
PbStk;




typedef int (*StmFct) ();	/* generic type for file fctions */

typedef struct stm_lst *PStmLst;

typedef struct stm_lst		/* Chained stream list            */
{				/* ------------------------------ */
  int stm;			/* the stream                     */
  PStmLst next;			/* next entry                     */
} StmLst;


typedef struct stm_inf		/* Stream information             */
{				/* ------------------------------ */
  int atom_file_name;		/* atom associated to filename    */
  PlLong file;			/* accessor (FILE *,TTYInf *) != 0*/
  int fileno;			/* fileno (-1 if not a POSIX file)*/
  StmProp prop;			/* assoctiated properties         */
  StmLst *mirror;		/* mirror streams                 */
  StmLst *mirror_of;            /* streams this stream as mirror  */
				/* ----- Basic I/O functions ---- */
  StmFct fct_getc;		/* get char function (mandatory)  */
  StmFct fct_putc;		/* put char function (mandatory)  */
  StmFct fct_flush;		/* flush    function (optional)   */
  StmFct fct_close;		/* close    function (optional)   */
  StmFct fct_tell;		/* tell     function (optional)   */
  StmFct fct_seek;		/* seek     function (optional)   */
  StmFct fct_clearerr;		/* clearerr function (optional)   */
				/* ------ Read information  ----- */
  Bool eof_reached;		/* has eof char been read ?       */
  PbStk pb_char;		/* character push back stack      */
				/* ---- Position information  --- */
  PlLong char_count;		/* character read count           */
  PlLong line_count;		/* line read count                */
  PlLong line_pos;		/* line position                  */
  PbStk pb_line_pos;		/* line position push back stack  */
}
StmInf;




typedef struct			/* Alias information              */
{				/* ------------------------------ */
  PlLong atom;			/* atom of the alias (the key)    */
  int stm;			/* associated stream              */
}
AliasInf;




typedef struct			/* String Stream information      */
{				/* ------------------------------ */
  char *buff;			/* the I/O buffer                 */
  char *ptr;			/* current position into the buff */
  Bool buff_alloc_size;		/* mallocated size (iff output)   */
}
StrSInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef STREAM_SUPP_FILE

StmInf **pl_stm_tbl;
int pl_stm_tbl_size;
int pl_stm_last_used;

char *pl_alias_tbl;

WamWord pl_last_input_sora;
WamWord pl_last_output_sora;

int pl_stm_stdin;
int pl_stm_stdout;
int pl_stm_stderr;

AliasInf *pl_alias_user_input;
AliasInf *pl_alias_user_output;
AliasInf *pl_alias_user_error;

AliasInf *pl_alias_current_input;
AliasInf *pl_alias_current_output;

AliasInf *pl_alias_top_level_input;
AliasInf *pl_alias_top_level_output;

AliasInf * pl_alias_debugger_input;
AliasInf *pl_alias_debugger_output;

Bool pl_stream_use_linedit;
char *pl_le_prompt;
int pl_use_le_prompt;

int pl_atom_stream;

int pl_atom_user_input;
int pl_atom_user_output;
int pl_atom_user_error;

int pl_atom_current_input;
int pl_atom_current_output;

int pl_atom_top_level_input;
int pl_atom_top_level_output;

int pl_atom_debugger_input;
int pl_atom_debugger_output;

int pl_atom_read;
int pl_atom_write;
int pl_atom_append;

int pl_atom_reposition;

int pl_atom_stream_position;

int pl_atom_text;
int pl_atom_binary;

int pl_atom_error;
int pl_atom_eof_code;
int pl_atom_reset;

int pl_atom_none;
int pl_atom_line;
int pl_atom_block;

int pl_atom_not;
int pl_atom_at;
int pl_atom_past;

int pl_atom_bof;
int pl_atom_current;
int pl_atom_eof;

int pl_atom_null;

#else

extern StmInf **pl_stm_tbl;
extern int pl_stm_tbl_size;
extern int pl_stm_last_used;

extern char *pl_alias_tbl;


extern WamWord pl_last_input_sora;
extern WamWord pl_last_output_sora;

extern int pl_stm_stdin;
extern int pl_stm_stdout;
extern int pl_stm_stderr;

extern AliasInf *pl_alias_user_input;
extern AliasInf *pl_alias_user_output;
extern AliasInf *pl_alias_user_error;

extern AliasInf *pl_alias_current_input;
extern AliasInf *pl_alias_current_output;

extern AliasInf *pl_alias_top_level_input;
extern AliasInf *pl_alias_top_level_output;

extern AliasInf *pl_alias_debugger_input;
extern AliasInf *pl_alias_debugger_output;

extern Bool pl_stream_use_linedit;
extern char *pl_le_prompt;
extern int pl_use_le_prompt;

extern int pl_atom_stream;

extern int pl_atom_user_input;
extern int pl_atom_user_output;
extern int pl_atom_user_error;

extern int pl_atom_current_input;
extern int pl_atom_current_output;

extern int pl_atom_top_level_input;
extern int pl_atom_top_level_output;

extern int pl_atom_debugger_input;
extern int pl_atom_debugger_output;

extern int pl_atom_read;
extern int pl_atom_write;
extern int pl_atom_append;

extern int pl_atom_reposition;

extern int pl_atom_stream_position;

extern int pl_atom_text;
extern int pl_atom_binary;

extern int pl_atom_error;
extern int pl_atom_eof_code;
extern int pl_atom_reset;

extern int pl_atom_none;
extern int pl_atom_line;
extern int pl_atom_block;

extern int pl_atom_not;
extern int pl_atom_at;
extern int pl_atom_past;

extern int pl_atom_bof;
extern int pl_atom_current;
extern int pl_atom_eof;

extern int pl_atom_null;

#endif


/* macros (pseudo-vars) to access to the stm of a predefined alias */

#define pl_stm_user_input         (pl_alias_user_input->stm)
#define pl_stm_user_output        (pl_alias_user_output->stm)
#define pl_stm_user_error         (pl_alias_user_error->stm)

#define pl_stm_current_input      (pl_alias_current_input->stm)
#define pl_stm_current_output     (pl_alias_current_output->stm)

#define pl_stm_top_level_input    (pl_alias_top_level_input->stm)
#define pl_stm_top_level_output   (pl_alias_top_level_output->stm)

#define pl_stm_debugger_input     (pl_alias_debugger_input->stm)
#define pl_stm_debugger_output    (pl_alias_debugger_output->stm)




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Pl_Add_Stream(int atom_file_name, PlLong file, int fileno, StmProp prop,
		  StmFct fct_getc, StmFct fct_putc,
		  StmFct fct_flush, StmFct fct_close,
		  StmFct fct_tell, StmFct fct_seek, StmFct fct_clearerr);

int Pl_Add_Stream_For_Stdio_Desc(FILE *f, int atom_path, int mode, Bool text,
				 Bool force_eof_reset);

int Pl_Add_Stream_For_Stdio_File(char *path, int mode, Bool text);

void Pl_Delete_Stream(int stm, Bool keep_stream);

int Pl_Find_Stream_By_Alias(int atom_alias);

AliasInf *Pl_Set_Alias_To_Stream(int atom_alias, int stm, Bool reassign);

void Pl_Add_Mirror_To_Stream(int stm, int m_stm);

Bool Pl_Del_Mirror_From_Stream(int stm, int m_stm);

int Pl_Find_Stream_From_PStm(StmInf *pstm);

void Pl_Flush_All_Streams(void);

void Pl_Set_Stream_Buffering(int stm);

int Pl_Get_Stream_Or_Alias(WamWord sora_word, int test_mask);

Bool Pl_Get_Stream(int stm, WamWord start_word);

void Pl_Check_Stream_Type(int stm, Bool check_text, Bool for_input);

WamWord Pl_Make_Stream_Tagged_Word(int stm);

Bool Pl_Stdio_Is_Repositionable(FILE *f);

void Pl_Stdio_Set_Buffering(FILE *f, int buffering);

FILE *Pl_Stdio_Desc_Of_Stream(int stm);

int Pl_Io_Fileno_Of_Stream(int stm);



void Pl_PB_Empty_Buffer(StmInf *pstm);

int Pl_Stream_Get_Key(StmInf *pstm, Bool echo, Bool catch_ctrl_c);

int Pl_Stream_Getc(StmInf *pstm);

void Pl_Stream_Ungetc(int c, StmInf *pstm);

int Pl_Stream_Peekc(StmInf *pstm);

char *Pl_Stream_Gets(char *str, int size, StmInf *pstm);

char *Pl_Stream_Gets_Prompt(char *prompt, StmInf *pstm_o,
			 char *str, int size, StmInf *pstm_i);

void Pl_Stream_Putc(int c, StmInf *pstm);

int Pl_Stream_Puts(char *str, StmInf *pstm);

int Pl_Stream_Printf(StmInf *pstm, char *format, ...);

void Pl_Stream_Flush(StmInf *pstm);

int Pl_Stream_Close(StmInf *pstm);

int Pl_Stream_End_Of_Stream(StmInf *pstm);



void Pl_Stream_Get_Position(StmInf *pstm, PlLong *offset,
			    PlLong *char_count, PlLong *line_count, PlLong *line_pos);

int Pl_Stream_Set_Position(StmInf *pstm, int whence, PlLong offset,
			   PlLong char_count, PlLong line_count, PlLong line_pos);

int Pl_Stream_Set_Position_LC(StmInf *pstm, PlLong line_count, PlLong line_pos);



int Pl_Add_Str_Stream(char *buff, int prop_other);

void Pl_Delete_Str_Stream(int stm);

char *Pl_Term_Write_Str_Stream(int stm);


void Pl_Close_Stm(int stm, Bool force); /* from close_c.c */


#define PB_Init(pb)          pb.ptr = pb.buff, pb.nb_elems = 0;



#define PB_Is_Empty(pb)      (pb.nb_elems == 0)



#define PB_Push(pb, elem)                  		\
  do							\
    {							\
      *(pb.ptr) = (elem);				\
      if (pb.ptr != pb.buff + STREAM_PB_SIZE - 1)	\
	pb.ptr++;					\
      else						\
	pb.ptr = pb.buff;				\
      if (pb.nb_elems < STREAM_PB_SIZE)			\
	pb.nb_elems++;					\
    }							\
  while (0)




#define PB_Pop(pb, elem)                    	\
  do						\
    {						\
      if (pb.ptr != pb.buff)			\
	pb.ptr--;				\
      else					\
	pb.ptr = pb.buff + STREAM_PB_SIZE - 1;	\
      (elem) = *pb.ptr;				\
      pb.nb_elems--;				\
    }						\
  while (0)




#define PB_Top(pb, elem)                    	\
  do						\
    {						\
      if (pb.ptr != pb.buff)			\
	(elem) = pb.ptr[-1];			\
      else					\
	(elem) = pb.buff[STREAM_PB_SIZE - 1];	\
    }						\
  while (0)

