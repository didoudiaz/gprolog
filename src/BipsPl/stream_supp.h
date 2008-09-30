/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : stream_supp.h                                                   *
 * Descr.: stream support - header file                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2008 Daniel Diaz                                     *
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



					/* values for Get_Stream_Or_Alias */
#define STREAM_CHECK_VALID         0	/* simply a valid stream */
#define STREAM_CHECK_EXIST         1	/* valid and exist */
#define STREAM_CHECK_INPUT         2	/* valid, exist and mode=input  */
#define STREAM_CHECK_OUTPUT        3	/* valid, exist and mode=output */




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
  unsigned other:8;		/* other prop (1,2,3=term_streams */
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
  long file;			/* accessor (FILE *,TTYInf *) != 0*/
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
  int char_count;		/* character read count           */
  int line_count;		/* line read count                */
  int line_pos;			/* line position                  */
  PbStk pb_line_pos;		/* line position push back stack  */
}
StmInf;




typedef struct			/* Alias information              */
{				/* ------------------------------ */
  long atom;			/* atom of the alias (the key)    */
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

StmInf **stm_tbl;
int stm_tbl_size;
int stm_last_used;

char *alias_tbl;

WamWord last_input_sora;
WamWord last_output_sora;

int stm_stdin;
int stm_stdout;

int stm_input;
int stm_output;

int stm_top_level_input;
int stm_top_level_output;
int stm_debugger_input;
int stm_debugger_output;

char *le_prompt;
int use_le_prompt;

int atom_stream;

int atom_user_input;
int atom_user_output;

int atom_top_level_input;
int atom_top_level_output;

int atom_debugger_input;
int atom_debugger_output;

int atom_read;
int atom_write;
int atom_append;

int atom_reposition;

int atom_stream_position;

int atom_text;
int atom_binary;

int atom_error;
int atom_eof_code;
int atom_reset;

int atom_none;
int atom_line;
int atom_block;

int atom_not;
int atom_at;
int atom_past;

int atom_bof;
int atom_current;
int atom_eof;

#else

extern StmInf **stm_tbl;
extern int stm_tbl_size;
extern int stm_last_used;

extern char *alias_tbl;


extern WamWord last_input_sora;
extern WamWord last_output_sora;

extern int stm_stdin;
extern int stm_stdout;

extern int stm_input;
extern int stm_output;

extern int stm_top_level_input;
extern int stm_top_level_output;
extern int stm_debugger_input;
extern int stm_debugger_output;

extern char *le_prompt;
extern int use_le_prompt;

extern int atom_stream;

extern int atom_user_input;
extern int atom_user_output;

extern int atom_top_level_input;
extern int atom_top_level_output;

extern int atom_debugger_input;
extern int atom_debugger_output;

extern int atom_read;
extern int atom_write;
extern int atom_append;

extern int atom_reposition;

extern int atom_stream_position;

extern int atom_text;
extern int atom_binary;

extern int atom_error;
extern int atom_eof_code;
extern int atom_reset;

extern int atom_none;
extern int atom_line;
extern int atom_block;

extern int atom_not;
extern int atom_at;
extern int atom_past;

extern int atom_bof;
extern int atom_current;
extern int atom_eof;

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Add_Stream(int atom_file_name, long file, StmProp prop,
	       StmFct fct_getc, StmFct fct_putc,
	       StmFct fct_flush, StmFct fct_close,
	       StmFct fct_tell, StmFct fct_seek, StmFct fct_clearerr);

int Add_Stream_For_Stdio_Desc(FILE *f, int atom_path, int mode, int text);

int Add_Stream_For_Stdio_File(char *path, int mode, Bool text);

void Delete_Stream(int stm);

int Find_Stream_By_Alias(int atom_alias);

Bool Add_Alias_To_Stream(int atom_alias, int stm);

void Reassign_Alias(int atom_alias, int stm);

void Add_Mirror_To_Stream(int stm, int m_stm);

Bool Del_Mirror_From_Stream(int stm, int m_stm);

int Find_Stream_From_PStm(StmInf *pstm);

void Flush_All_Streams(void);

void Set_Stream_Buffering(int stm);

int Get_Stream_Or_Alias(WamWord sora_word, int test_mask);

void Check_Stream_Type(int stm, Bool check_text, Bool for_input);

WamWord Make_Stream_Tagged_Word(int stm);

Bool Stdio_Is_Repositionable(FILE *f);

void Stdio_Set_Buffering(FILE *f, int buffering);

FILE *Stdio_Desc_Of_Stream(int stm);

int Io_Fileno_Of_Stream(int stm);




int Stream_Getc(StmInf *pstm);

int Stream_Get_Key(StmInf *pstm, Bool echo, Bool catch_ctrl_c);

void Stream_Ungetc(int c, StmInf *pstm);

int Stream_Peekc(StmInf *pstm);

char *Stream_Gets(char *str, int size, StmInf *pstm);

char *Stream_Gets_Prompt(char *prompt, StmInf *pstm_o,
			 char *str, int size, StmInf *pstm_i);

void Stream_Putc(int c, StmInf *pstm);

int Stream_Puts(char *str, StmInf *pstm);

int Stream_Printf(StmInf *pstm, char *format, ...);

void Stream_Flush(StmInf *pstm);

int Stream_Close(StmInf *pstm);

int Stream_End_Of_Stream(StmInf *pstm);



void Stream_Get_Position(StmInf *pstm, int *offset,
			 int *char_count, int *line_count, int *line_pos);

int Stream_Set_Position(StmInf *pstm, int whence, int offset,
			int char_count, int line_count, int line_pos);

int Stream_Set_Position_LC(StmInf *pstm, int line_count, int line_pos);



int Add_Str_Stream(char *buff, int prop_other);

void Delete_Str_Stream(int stm);

char *Term_Write_Str_Stream(int stm);


void Close_Stm(int stm, Bool force); /* from close_c.c */


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

