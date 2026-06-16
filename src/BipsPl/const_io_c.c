/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : const_io_c.c                                                    *
 * Descr.: input/output from/to constant term management - C part          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2026 Daniel Diaz                                     *
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

#include "gp_config.h"

#include "engine_pl.h"
#include "bips_pl.h"




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

	  /* from write_c.c */

void Pl_Write_Term_2(WamWord sora_word, WamWord term_word);

void Pl_Write_2(WamWord sora_word, WamWord term_word);

void Pl_Writeq_2(WamWord sora_word, WamWord term_word);

void Pl_Write_Canonical_2(WamWord sora_word, WamWord term_word);

void Pl_Display_2(WamWord sora_word, WamWord term_word);

void Pl_Print_2(WamWord sora_word, WamWord term_word);



	  /* from format_c.c */

void Pl_Format_3(WamWord sora_word, WamWord format_word, WamWord args_word);



	  /* from read_c.c */

Bool Pl_Read_Term_5(WamWord sora_word, WamWord term_word,
		    WamWord vars_word, WamWord var_names_word,
		    WamWord sing_names_word);

Bool Pl_Read_Token_2(WamWord sora_word, WamWord token_word);




/*----- OUTPUT -----*/

#if 0
#define OUT_TO_STR(const_stream_type, str, stm_word, code_out, code_after) \
{									\
  int stm;								\
  WamWord stm_word;							\
  char *str;								\
									\
  stm = Pl_Add_Str_Stream(NULL, const_stream_type);			\
  stm_word = Pl_Make_Stream_Tagged_Word(stm);				\
									\
  { code_out; }								\
									\
  str = Pl_Terminate_Output_Str_Stream(stm);				\
									\
  { code_after; }							\
									\
  Pl_Delete_Str_Stream(stm);						\
}
#else
#define OUT_TO_STR(const_stream_type, str, stm_word, code_out, code_after) \
{								\
  char *str;							\
  OUT_TO_STR_BEFORE(const_stream_type, stm, stm_word);		\
								\
  { code_out; }							\
								\
  OUT_TO_STR_AFTER(str, stm, code_after);			\
}
#endif



#define OUT_TO_STR_BEFORE(const_stream_type, stm, stm_word) 	\
  int stm;							\
  WamWord stm_word;						\
								\
  stm = Pl_Add_Str_Stream(NULL, const_stream_type);		\
  stm_word = Pl_Make_Stream_Tagged_Word(stm);


#define OUT_TO_STR_AFTER(str, stm, code_after)			\
  str = Pl_Terminate_Output_Str_Stream(stm);			\
								\
  { code_after; }						\
								\
  Pl_Delete_Str_Stream(stm);




/*-------------------------------------------------------------------------*
 * PL_WRITE_TO_STRING (foreign interface)                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Write_To_String(WamWord term_word)
{
  char *ret_str;

  OUT_TO_STR(TERM_STREAM_ATOM, str, stm_word,
	     Pl_Write_2(stm_word, term_word),
	     ret_str = Strdup(str));

  return ret_str;
}




/*-------------------------------------------------------------------------*
 * PL_WRITEQ_TO_STRING (foreign interface)                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Writeq_To_String(WamWord term_word)
{
  char *ret_str;

  OUT_TO_STR(TERM_STREAM_ATOM, str, stm_word,
	     Pl_Writeq_2(stm_word, term_word),
	     ret_str = Strdup(str));

  return ret_str;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_CANONICAL_TO_STRING (foreign interface)                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Write_Canonical_To_String(WamWord term_word)
{
  char *ret_str;

  OUT_TO_STR(TERM_STREAM_ATOM, str, stm_word,
	     Pl_Write_Canonical_2(stm_word, term_word),
	     ret_str = Strdup(str));

  return ret_str;
}




/*-------------------------------------------------------------------------*
 * PL_DISPLAY_TO_STRING (foreign interface)                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Display_To_String(WamWord term_word)
{
  char *ret_str;

  OUT_TO_STR(TERM_STREAM_ATOM, str, stm_word,
	     Pl_Display_2(stm_word, term_word),
	     ret_str = Strdup(str));

  return ret_str;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TO_ATOM_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_ATOM, str, stm_word,
	     Pl_Write_2(stm_word, term_word),
	     ret = Pl_Un_String_Check(str, atom_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TO_CHARS_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CHARS, str, stm_word,
	     Pl_Write_2(stm_word, term_word),
	     ret = Pl_Un_Chars_Check(str, chars_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TO_CODES_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CODES, str, stm_word,
	     Pl_Write_2(stm_word, term_word),
	     ret = Pl_Un_Codes_Check(str, codes_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITEQ_TO_ATOM_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Writeq_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_ATOM, str, stm_word,
	     Pl_Writeq_2(stm_word, term_word),
	     ret = Pl_Un_String_Check(str, atom_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITEQ_TO_CHARS_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Writeq_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CHARS, str, stm_word,
	     Pl_Writeq_2(stm_word, term_word),
	     ret = Pl_Un_Chars_Check(str, chars_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITEQ_TO_CODES_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Writeq_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CODES, str, stm_word,
	     Pl_Writeq_2(stm_word, term_word),
	     ret = Pl_Un_Codes_Check(str, codes_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_CANONICAL_TO_ATOM_2                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_Canonical_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_ATOM, str, stm_word,
	     Pl_Write_Canonical_2(stm_word, term_word),
	     ret = Pl_Un_String_Check(str, atom_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_CANONICAL_TO_CHARS_2                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_Canonical_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CHARS, str, stm_word,
	     Pl_Write_Canonical_2(stm_word, term_word),
	     ret = Pl_Un_Chars_Check(str, chars_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_CANONICAL_TO_CODES_2                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_Canonical_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CODES, str, stm_word,
	     Pl_Write_Canonical_2(stm_word, term_word),
	     ret = Pl_Un_Codes_Check(str, codes_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_DISPLAY_TO_ATOM_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Display_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_ATOM, str, stm_word,
	     Pl_Display_2(stm_word, term_word),
	     ret = Pl_Un_String_Check(str, atom_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_DISPLAY_TO_CHARS_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Display_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CHARS, str, stm_word,
	     Pl_Display_2(stm_word, term_word),
	     ret = Pl_Un_Chars_Check(str, chars_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_DISPLAY_TO_CODES_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Display_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CODES, str, stm_word,
	     Pl_Display_2(stm_word, term_word),
	     ret = Pl_Un_Codes_Check(str, codes_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_PRINT_TO_ATOM_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Print_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_ATOM, str, stm_word,
	     Pl_Print_2(stm_word, term_word),
	     ret = Pl_Un_String_Check(str, atom_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_PRINT_TO_CHARS_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Print_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CHARS, str, stm_word,
	     Pl_Print_2(stm_word, term_word),
	     ret = Pl_Un_Chars_Check(str, chars_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_PRINT_TO_CODES_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Print_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CODES, str, stm_word,
	     Pl_Print_2(stm_word, term_word),
	     ret = Pl_Un_Codes_Check(str, codes_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * write_term_to_atom/chars/codes are more complex than e.g. write_to_atom *
 * since the option variable_names assigns variables to '$VARNAME'(name)   *
 * and these assignments must be undone (via backtracking).                *
 * We split into 2 parts: before/after backtracking occurs.                *
 *-------------------------------------------------------------------------*/

static int last_str_stm;


/*-------------------------------------------------------------------------*
 * PL_WRITE_TERM_TO_ATOM                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Write_Term_To_Atom_Before_1(WamWord term_word)
{
  OUT_TO_STR_BEFORE(TERM_STREAM_ATOM, stm, stm_word);
  
  Pl_Write_Term_2(stm_word, term_word);
  last_str_stm = stm;
}


Bool
Pl_Write_Term_To_Atom_After_1(WamWord atom_word)
{
  char *str;
  Bool ret;
  
  OUT_TO_STR_AFTER(str, last_str_stm,
		   ret = Pl_Un_String_Check(str, atom_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TERM_TO_CHARS                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Write_Term_To_Chars_Before_1(WamWord term_word)
{
  OUT_TO_STR_BEFORE(TERM_STREAM_CHARS, stm, stm_word);
  
  Pl_Write_Term_2(stm_word, term_word);
  last_str_stm = stm;
}


Bool
Pl_Write_Term_To_Chars_After_1(WamWord chars_word)
{
  char *str;
  Bool ret;
  
  OUT_TO_STR_AFTER(str, last_str_stm,
		   ret = Pl_Un_Chars_Check(str, chars_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TERM_TO_CODES                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Write_Term_To_Codes_Before_1(WamWord term_word)
{
  OUT_TO_STR_BEFORE(TERM_STREAM_CODES, stm, stm_word);
  
  Pl_Write_Term_2(stm_word, term_word);
  last_str_stm = stm;
}


Bool
Pl_Write_Term_To_Codes_After_1(WamWord codes_word)
{
  char *str;
  Bool ret;
  
  OUT_TO_STR_AFTER(str, last_str_stm,
		   ret = Pl_Un_Codes_Check(str, codes_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_FORMAT_TO_ATOM_3                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Format_To_Atom_3(WamWord atom_word, WamWord format_word, WamWord args_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_ATOM, str, stm_word,
	     Pl_Format_3(stm_word, format_word, args_word),
	     ret = Pl_Un_String_Check(str, atom_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_FORMAT_TO_CHARS_3                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Format_To_Chars_3(WamWord chars_word, WamWord format_word, WamWord args_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CHARS, str, stm_word,
	     Pl_Format_3(stm_word, format_word, args_word),
	     ret = Pl_Un_Chars_Check(str, chars_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_FORMAT_TO_CODES_3                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Format_To_Codes_3(WamWord codes_word, WamWord format_word, WamWord args_word)
{
  Bool ret;

  OUT_TO_STR(TERM_STREAM_CODES, str, stm_word,
	     Pl_Format_3(stm_word, format_word, args_word),
	     ret = Pl_Un_Codes_Check(str, codes_word));

  return ret;
}




/*----- INPUT -----*/

#define IN_FROM_STR(const_stream_type, str, stm_word, code_in)	\
{								\
  int stm;							\
  WamWord stm_word;						\
								\
  stm = Pl_Add_Str_Stream(str, const_stream_type);		\
  stm_word = Pl_Make_Stream_Tagged_Word(stm);			\
								\
  { code_in; }							\
								\
  Pl_Delete_Str_Stream(stm);					\
}




/*-------------------------------------------------------------------------*
 * PL_READ_FROM_STRING (foreign interface)                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Read_From_String(char *str)
{
  WamWord term_word = Pl_Mk_Variable();

  /* this corresponds to defaults in read.pl ('$set_read_defaults') and read_c.c */
  SYS_VAR_OPTION_MASK = 0;	/* nothing */
  SYS_VAR_OPTION_MASK |= (1 << 3); /* end_of_term = EOF */


  SYS_VAR_SYNTAX_ERROR_ACTON = -1; /* on syntax error use value fo flags syntax_error */


  IN_FROM_STR(TERM_STREAM_ATOM, str, stm_word, 
	      Pl_Read_Term_5(stm_word, term_word, 0, 0, 0));

  return term_word;
}




/*-------------------------------------------------------------------------*
 * PL_READ_FROM_ATOM_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_From_Atom_2(WamWord atom_word, WamWord term_word)
{
  Bool ret;

  IN_FROM_STR(TERM_STREAM_ATOM, pl_atom_tbl[Pl_Rd_Atom_Check(atom_word)].name, stm_word, 
	      ret = Pl_Read_Term_5(stm_word, term_word, 0, 0, 0));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_FROM_CHARS_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_From_Chars_2(WamWord chars_word, WamWord term_word)
{
  Bool ret;

  IN_FROM_STR(TERM_STREAM_CHARS, Pl_Rd_Chars_Check(chars_word), stm_word,
	      ret = Pl_Read_Term_5(stm_word, term_word, 0, 0, 0));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_FROM_CODES_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_From_Codes_2(WamWord codes_word, WamWord term_word)
{
  Bool ret;

  IN_FROM_STR(TERM_STREAM_CODES, Pl_Rd_Codes_Check(codes_word), stm_word,
	      ret = Pl_Read_Term_5(stm_word, term_word, 0, 0, 0));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TERM_FROM_ATOM_5                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Term_From_Atom_5(WamWord atom_word, WamWord term_word,
			 WamWord vars_word, WamWord var_names_word,
			 WamWord sing_names_word)
{
  Bool ret;

  IN_FROM_STR(TERM_STREAM_ATOM, pl_atom_tbl[Pl_Rd_Atom_Check(atom_word)].name, stm_word,
	      ret = Pl_Read_Term_5(stm_word, term_word, vars_word, var_names_word, sing_names_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TERM_FROM_CHARS_5                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Term_From_Chars_5(WamWord chars_word, WamWord term_word,
			  WamWord vars_word, WamWord var_names_word,
			  WamWord sing_names_word)
{
  Bool ret;

  IN_FROM_STR(TERM_STREAM_CHARS, Pl_Rd_Chars_Check(chars_word), stm_word,
	      ret = Pl_Read_Term_5(stm_word, term_word, vars_word, var_names_word, sing_names_word));

  return ret;
}





/*-------------------------------------------------------------------------*
 * PL_READ_TERM_FROM_CODES_5                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Term_From_Codes_5(WamWord codes_word, WamWord term_word,
			  WamWord vars_word, WamWord var_names_word,
			  WamWord sing_names_word)
{
  Bool ret;

  IN_FROM_STR(TERM_STREAM_CODES, Pl_Rd_Codes_Check(codes_word), stm_word,
	      ret = Pl_Read_Term_5(stm_word, term_word, vars_word, var_names_word, sing_names_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN_FROM_ATOM_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Token_From_Atom_2(WamWord atom_word, WamWord token_word)
{
  Bool ret;

  IN_FROM_STR(TERM_STREAM_ATOM, pl_atom_tbl[Pl_Rd_Atom_Check(atom_word)].name, stm_word,
	      ret = Pl_Read_Token_2(stm_word, token_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN_FROM_CHARS_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Token_From_Chars_2(WamWord chars_word, WamWord token_word)
{
  Bool ret;

  IN_FROM_STR(TERM_STREAM_CHARS, Pl_Rd_Chars_Check(chars_word), stm_word,
	      ret = Pl_Read_Token_2(stm_word, token_word));

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN_FROM_CODES_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Token_From_Codes_2(WamWord codes_word, WamWord token_word)
{
  Bool ret;

  IN_FROM_STR(TERM_STREAM_CODES, Pl_Rd_Codes_Check(codes_word), stm_word,
	      ret = Pl_Read_Token_2(stm_word, token_word));

  return ret;
}
