/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : const_io_c.c                                                    *
 * Descr.: input/output from/to constant term management - C part          *
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

void Write_Term_2(WamWord sora_word, WamWord term_word);

void Write_2(WamWord sora_word, WamWord term_word);

void Writeq_2(WamWord sora_word, WamWord term_word);

void Write_Canonical_2(WamWord sora_word, WamWord term_word);

void Display_2(WamWord sora_word, WamWord term_word);

void Print_2(WamWord sora_word, WamWord term_word);



	  /* from format_c.c */

void Format_3(WamWord sora_word, WamWord format_word, WamWord args_word);



	  /* from read_c.c */

Bool Read_Term_5(WamWord sora_word, WamWord term_word,
		 WamWord vars_word, WamWord var_names_word,
		 WamWord sing_names_word);

Bool Read_Token_2(WamWord sora_word, WamWord token_word);



#define GLOBAL_STREAM_ALIAS_WORD Tag_ATM(atom_glob_stream_alias)




/*-------------------------------------------------------------------------*
 * WRITE_TO_ATOM_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Write_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Write_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_String_Check(str, atom_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_TO_CHARS_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Write_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Write_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Chars_Check(str, chars_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_TO_CODES_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Write_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Write_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Codes_Check(str, codes_word);
}




/*-------------------------------------------------------------------------*
 * WRITEQ_TO_ATOM_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Writeq_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Writeq_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_String_Check(str, atom_word);
}




/*-------------------------------------------------------------------------*
 * WRITEQ_TO_CHARS_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Writeq_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Writeq_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Chars_Check(str, chars_word);
}




/*-------------------------------------------------------------------------*
 * WRITEQ_TO_CODES_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Writeq_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Writeq_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Codes_Check(str, codes_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_CANONICAL_TO_ATOM_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Write_Canonical_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Write_Canonical_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_String_Check(str, atom_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_CANONICAL_TO_CHARS_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Write_Canonical_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Write_Canonical_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Chars_Check(str, chars_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_CANONICAL_TO_CODES_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Write_Canonical_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Write_Canonical_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Codes_Check(str, codes_word);
}




/*-------------------------------------------------------------------------*
 * DISPLAY_TO_ATOM_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Display_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Display_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_String_Check(str, atom_word);
}




/*-------------------------------------------------------------------------*
 * DISPLAY_TO_CHARS_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Display_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Display_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Chars_Check(str, chars_word);
}




/*-------------------------------------------------------------------------*
 * DISPLAY_TO_CODES_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Display_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Display_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Codes_Check(str, codes_word);
}




/*-------------------------------------------------------------------------*
 * PRINT_TO_ATOM_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Print_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Print_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_String_Check(str, atom_word);
}




/*-------------------------------------------------------------------------*
 * PRINT_TO_CHARS_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Print_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Print_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Chars_Check(str, chars_word);
}




/*-------------------------------------------------------------------------*
 * PRINT_TO_CODES_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Print_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Print_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Codes_Check(str, codes_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_TERM_TO_ATOM_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Write_Term_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Write_Term_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_String_Check(str, atom_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_TERM_TO_CHARS_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Write_Term_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Write_Term_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Chars_Check(str, chars_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_TERM_TO_CODES_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Write_Term_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Write_Term_2(GLOBAL_STREAM_ALIAS_WORD, term_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Codes_Check(str, codes_word);
}




/*-------------------------------------------------------------------------*
 * FORMAT_TO_ATOM_3                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Format_To_Atom_3(WamWord atom_word, WamWord format_word, WamWord args_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Format_3(GLOBAL_STREAM_ALIAS_WORD, format_word, args_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_String_Check(str, atom_word);
}




/*-------------------------------------------------------------------------*
 * FORMAT_TO_CHARS_3                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Format_To_Chars_3(WamWord chars_word, WamWord format_word,
		  WamWord args_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Format_3(GLOBAL_STREAM_ALIAS_WORD, format_word, args_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Chars_Check(str, chars_word);
}




/*-------------------------------------------------------------------------*
 * FORMAT_TO_CODES_3                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Format_To_Codes_3(WamWord codes_word, WamWord format_word,
		  WamWord args_word)
{
  int stm;
  char *str;

  stm = Add_Str_Stream(TRUE, NULL);

  Format_3(GLOBAL_STREAM_ALIAS_WORD, format_word, args_word);

  str = Term_Write_Str_Stream(stm);
  Delete_Str_Stream(stm);

  return Un_Codes_Check(str, codes_word);
}




/*-------------------------------------------------------------------------*
 * READ_FROM_ATOM_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_From_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  str = atom_tbl[Rd_Atom_Check(atom_word)].name;

  stm = Add_Str_Stream(TRUE, str);

  ret = Read_Term_5(GLOBAL_STREAM_ALIAS_WORD, term_word, 0, 0, 0);

  Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * READ_FROM_CHARS_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_From_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  Bool ret;

  stm = Add_Str_Stream(TRUE, Rd_Chars_Check(chars_word));

  ret = Read_Term_5(GLOBAL_STREAM_ALIAS_WORD, term_word, 0, 0, 0);

  Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * READ_FROM_CODES_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_From_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  Bool ret;

  stm = Add_Str_Stream(TRUE, Rd_Codes_Check(codes_word));

  ret = Read_Term_5(GLOBAL_STREAM_ALIAS_WORD, term_word, 0, 0, 0);

  Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * READ_TERM_FROM_ATOM_5                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Term_From_Atom_5(WamWord atom_word, WamWord term_word,
		      WamWord vars_word, WamWord var_names_word,
		      WamWord sing_names_word)
{
  int stm;
  char *str;
  Bool ret;

  str = atom_tbl[Rd_Atom_Check(atom_word)].name;

  stm = Add_Str_Stream(TRUE, str);

  ret = Read_Term_5(GLOBAL_STREAM_ALIAS_WORD, term_word,
		    vars_word, var_names_word, sing_names_word);

  Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * READ_TERM_FROM_CHARS_5                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Term_From_Chars_5(WamWord chars_word, WamWord term_word,
		       WamWord vars_word, WamWord var_names_word,
		       WamWord sing_names_word)
{
  int stm;
  Bool ret;

  stm = Add_Str_Stream(TRUE, Rd_Chars_Check(chars_word));

  ret = Read_Term_5(GLOBAL_STREAM_ALIAS_WORD, term_word,
		    vars_word, var_names_word, sing_names_word);

  Delete_Str_Stream(stm);

  return ret;
}





/*-------------------------------------------------------------------------*
 * READ_TERM_FROM_CODES_5                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Term_From_Codes_5(WamWord codes_word, WamWord term_word,
		       WamWord vars_word, WamWord var_names_word,
		       WamWord sing_names_word)
{
  int stm;
  Bool ret;

  stm = Add_Str_Stream(TRUE, Rd_Codes_Check(codes_word));

  ret = Read_Term_5(GLOBAL_STREAM_ALIAS_WORD, term_word,
		    vars_word, var_names_word, sing_names_word);

  Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * READ_TOKEN_FROM_ATOM_2                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Token_From_Atom_2(WamWord atom_word, WamWord token_word)
{
  int stm;
  char *str;
  Bool ret;

  str = atom_tbl[Rd_Atom_Check(atom_word)].name;

  stm = Add_Str_Stream(TRUE, str);

  ret = Read_Token_2(GLOBAL_STREAM_ALIAS_WORD, token_word);

  Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * READ_TOKEN_FROM_CHARS_2                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Token_From_Chars_2(WamWord chars_word, WamWord token_word)
{
  int stm;
  Bool ret;

  stm = Add_Str_Stream(TRUE, Rd_Chars_Check(chars_word));

  ret = Read_Token_2(GLOBAL_STREAM_ALIAS_WORD, token_word);

  Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * READ_TOKEN_FROM_CODES_2                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Token_From_Codes_2(WamWord codes_word, WamWord token_word)
{
  int stm;
  Bool ret;

  stm = Add_Str_Stream(TRUE, Rd_Codes_Check(codes_word));

  ret = Read_Token_2(GLOBAL_STREAM_ALIAS_WORD, token_word);

  Delete_Str_Stream(stm);

  return ret;
}
