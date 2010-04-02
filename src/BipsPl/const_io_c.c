/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : const_io_c.c                                                    *
 * Descr.: input/output from/to constant term management - C part          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2010 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU Lesser General Public License as published   *
 * by the Free Software Foundation; either version 3, or any later version.*
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU Lesser General Public License*
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
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




/*-------------------------------------------------------------------------*
 * PL_WRITE_TO_ATOM_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_ATOM);

  Pl_Write_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_String_Check(str, atom_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TO_CHARS_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CHARS);

  Pl_Write_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Chars_Check(str, chars_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TO_CODES_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CODES);

  Pl_Write_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Codes_Check(str, codes_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITEQ_TO_ATOM_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Writeq_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_ATOM);

  Pl_Writeq_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_String_Check(str, atom_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITEQ_TO_CHARS_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Writeq_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CHARS);

  Pl_Writeq_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Chars_Check(str, chars_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITEQ_TO_CODES_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Writeq_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CODES);

  Pl_Writeq_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Codes_Check(str, codes_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_CANONICAL_TO_ATOM_2                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_Canonical_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_ATOM);

  Pl_Write_Canonical_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_String_Check(str, atom_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_CANONICAL_TO_CHARS_2                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_Canonical_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CHARS);

  Pl_Write_Canonical_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Chars_Check(str, chars_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_CANONICAL_TO_CODES_2                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_Canonical_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CODES);

  Pl_Write_Canonical_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Codes_Check(str, codes_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_DISPLAY_TO_ATOM_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Display_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_ATOM);

  Pl_Display_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_String_Check(str, atom_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_DISPLAY_TO_CHARS_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Display_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CHARS);

  Pl_Display_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Chars_Check(str, chars_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_DISPLAY_TO_CODES_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Display_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CODES);

  Pl_Display_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Codes_Check(str, codes_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_PRINT_TO_ATOM_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Print_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_ATOM);

  Pl_Print_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_String_Check(str, atom_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_PRINT_TO_CHARS_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Print_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CHARS);

  Pl_Print_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Chars_Check(str, chars_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_PRINT_TO_CODES_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Print_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CODES);

  Pl_Print_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Codes_Check(str, codes_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TERM_TO_ATOM_2                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_Term_To_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_ATOM);

  Pl_Write_Term_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_String_Check(str, atom_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TERM_TO_CHARS_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_Term_To_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CHARS);

  Pl_Write_Term_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Chars_Check(str, chars_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TERM_TO_CODES_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Write_Term_To_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CODES);

  Pl_Write_Term_2(Pl_Make_Stream_Tagged_Word(stm), term_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Codes_Check(str, codes_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_FORMAT_TO_ATOM_3                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Format_To_Atom_3(WamWord atom_word, WamWord format_word, WamWord args_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_ATOM);

  Pl_Format_3(Pl_Make_Stream_Tagged_Word(stm), format_word, args_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_String_Check(str, atom_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_FORMAT_TO_CHARS_3                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Format_To_Chars_3(WamWord chars_word, WamWord format_word,
		  WamWord args_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CHARS);

  Pl_Format_3(Pl_Make_Stream_Tagged_Word(stm), format_word, args_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Chars_Check(str, chars_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_FORMAT_TO_CODES_3                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Format_To_Codes_3(WamWord codes_word, WamWord format_word,
		  WamWord args_word)
{
  int stm;
  char *str;
  Bool ret;

  stm = Pl_Add_Str_Stream(NULL, TERM_STREAM_CODES);

  Pl_Format_3(Pl_Make_Stream_Tagged_Word(stm), format_word, args_word);

  str = Pl_Term_Write_Str_Stream(stm);
  ret = Pl_Un_Codes_Check(str, codes_word);
  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_FROM_ATOM_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_From_Atom_2(WamWord atom_word, WamWord term_word)
{
  int stm;
  char *str;
  Bool ret;

  str = pl_atom_tbl[Pl_Rd_Atom_Check(atom_word)].name;

  stm = Pl_Add_Str_Stream(str, TERM_STREAM_ATOM);

  ret = Pl_Read_Term_5(Pl_Make_Stream_Tagged_Word(stm), term_word, 0, 0, 0);

  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_FROM_CHARS_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_From_Chars_2(WamWord chars_word, WamWord term_word)
{
  int stm;
  Bool ret;

  stm = Pl_Add_Str_Stream(Pl_Rd_Chars_Check(chars_word), TERM_STREAM_CHARS);

  ret = Pl_Read_Term_5(Pl_Make_Stream_Tagged_Word(stm), term_word, 0, 0, 0);

  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_FROM_CODES_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_From_Codes_2(WamWord codes_word, WamWord term_word)
{
  int stm;
  Bool ret;

  stm = Pl_Add_Str_Stream(Pl_Rd_Codes_Check(codes_word), TERM_STREAM_CODES);

  ret = Pl_Read_Term_5(Pl_Make_Stream_Tagged_Word(stm), term_word, 0, 0, 0);

  Pl_Delete_Str_Stream(stm);

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
  int stm;
  char *str;
  Bool ret;

  str = pl_atom_tbl[Pl_Rd_Atom_Check(atom_word)].name;

  stm = Pl_Add_Str_Stream(str, TERM_STREAM_ATOM);

  ret = Pl_Read_Term_5(Pl_Make_Stream_Tagged_Word(stm), term_word,
		    vars_word, var_names_word, sing_names_word);

  Pl_Delete_Str_Stream(stm);

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
  int stm;
  Bool ret;

  stm = Pl_Add_Str_Stream(Pl_Rd_Chars_Check(chars_word), TERM_STREAM_CHARS);

  ret = Pl_Read_Term_5(Pl_Make_Stream_Tagged_Word(stm), term_word,
		    vars_word, var_names_word, sing_names_word);

  Pl_Delete_Str_Stream(stm);

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
  int stm;
  Bool ret;

  stm = Pl_Add_Str_Stream(Pl_Rd_Codes_Check(codes_word), TERM_STREAM_CODES);

  ret = Pl_Read_Term_5(Pl_Make_Stream_Tagged_Word(stm), term_word,
		    vars_word, var_names_word, sing_names_word);

  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN_FROM_ATOM_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Token_From_Atom_2(WamWord atom_word, WamWord token_word)
{
  int stm;
  char *str;
  Bool ret;

  str = pl_atom_tbl[Pl_Rd_Atom_Check(atom_word)].name;

  stm = Pl_Add_Str_Stream(str, TERM_STREAM_ATOM);

  ret = Pl_Read_Token_2(Pl_Make_Stream_Tagged_Word(stm), token_word);

  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN_FROM_CHARS_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Token_From_Chars_2(WamWord chars_word, WamWord token_word)
{
  int stm;
  Bool ret;

  stm = Pl_Add_Str_Stream(Pl_Rd_Chars_Check(chars_word), TERM_STREAM_CHARS);

  ret = Pl_Read_Token_2(Pl_Make_Stream_Tagged_Word(stm), token_word);

  Pl_Delete_Str_Stream(stm);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN_FROM_CODES_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Token_From_Codes_2(WamWord codes_word, WamWord token_word)
{
  int stm;
  Bool ret;

  stm = Pl_Add_Str_Stream(Pl_Rd_Codes_Check(codes_word), TERM_STREAM_CODES);

  ret = Pl_Read_Token_2(Pl_Make_Stream_Tagged_Word(stm), token_word);

  Pl_Delete_Str_Stream(stm);

  return ret;
}
