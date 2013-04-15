/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : char_io_c.c                                                     *
 * Descr.: character input-output management - C part                      *
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




/*-------------------------------------------------------------------------*
 * PL_GET_KEY_2                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Key_2(WamWord sora_word, WamWord code_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, TRUE);

  Pl_Check_For_Un_Integer(code_word);

  c = Pl_Stream_Get_Key(pl_stm_tbl[stm], TRUE, TRUE);

  if (c == EOF)
    c = -1;

  return Pl_Get_Integer(c, code_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_KEY_1                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Key_1(WamWord code_word)
{
  return Pl_Get_Key_2(NOT_A_WAM_WORD, code_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_KEY_NO_ECHO_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Key_No_Echo_2(WamWord sora_word, WamWord code_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, TRUE);

  Pl_Check_For_Un_Integer(code_word);

  c = Pl_Stream_Get_Key(pl_stm_tbl[stm], FALSE, TRUE);

  if (c == EOF)
    c = -1;

  return Pl_Get_Integer(c, code_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_KEY_NO_ECHO_1                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Key_No_Echo_1(WamWord code_word)
{
  return Pl_Get_Key_No_Echo_2(NOT_A_WAM_WORD, code_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_CHAR_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Char_2(WamWord sora_word, WamWord char_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, TRUE);

  Pl_Check_For_Un_In_Char(char_word);

  c = Pl_Stream_Getc(pl_stm_tbl[stm]);
  if (c != EOF && !Is_Valid_Code(c))
    Pl_Err_Representation(pl_representation_character);

  return Pl_Get_Atom((c == EOF) ? pl_atom_end_of_file : ATOM_CHAR(c), char_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_CHAR_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Char_1(WamWord char_word)
{
  return Pl_Get_Char_2(NOT_A_WAM_WORD, char_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_CODE_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Code_2(WamWord sora_word, WamWord code_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, TRUE);

  Pl_Check_For_Un_In_Code(code_word);

  c = Pl_Stream_Getc(pl_stm_tbl[stm]);
  if (c != EOF && !Is_Valid_Code(c))
    Pl_Err_Representation(pl_representation_character);

  if (c == EOF)
    c = -1;

  return Pl_Get_Integer(c, code_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_CODE_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Code_1(WamWord code_word)
{
  return Pl_Get_Code_2(NOT_A_WAM_WORD, code_word);
}





/*-------------------------------------------------------------------------*
 * PL_GET_BYTE_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Byte_2(WamWord sora_word, WamWord byte_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, FALSE, TRUE);

  Pl_Check_For_Un_In_Byte(byte_word);

  c = Pl_Stream_Getc(pl_stm_tbl[stm]);
  if (c == EOF)
    c = -1;

  return Pl_Get_Integer(c, byte_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_BYTE_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Byte_1(WamWord byte_word)
{
  return Pl_Get_Byte_2(NOT_A_WAM_WORD, byte_word);
}






/*-------------------------------------------------------------------------*
 * PL_UNGET_CHAR_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Unget_Char_2(WamWord sora_word, WamWord char_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, TRUE);

  Pl_Stream_Ungetc(Pl_Rd_Char_Check(char_word), pl_stm_tbl[stm]);
}




/*-------------------------------------------------------------------------*
 * PL_UNGET_CHAR_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Unget_Char_1(WamWord char_word)
{
  Pl_Unget_Char_2(NOT_A_WAM_WORD, char_word);
}




/*-------------------------------------------------------------------------*
 * PL_UNGET_CODE_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Unget_Code_2(WamWord sora_word, WamWord code_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, TRUE);

  Pl_Stream_Ungetc(Pl_Rd_Code_Check(code_word), pl_stm_tbl[stm]);
}




/*-------------------------------------------------------------------------*
 * PL_UNGET_CODE_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Unget_Code_1(WamWord code_word)
{
  Pl_Unget_Code_2(NOT_A_WAM_WORD, code_word);
}





/*-------------------------------------------------------------------------*
 * PL_UNGET_BYTE_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Unget_Byte_2(WamWord sora_word, WamWord byte_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, FALSE, TRUE);

  Pl_Stream_Ungetc(Pl_Rd_Byte_Check(byte_word), pl_stm_tbl[stm]);
}




/*-------------------------------------------------------------------------*
 * PL_UNGET_BYTE_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Unget_Byte_1(WamWord byte_word)
{
  Pl_Unget_Byte_2(NOT_A_WAM_WORD, byte_word);
}




/*-------------------------------------------------------------------------*
 * PL_PEEK_CHAR_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Peek_Char_2(WamWord sora_word, WamWord char_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, TRUE);

  Pl_Check_For_Un_In_Char(char_word);

  c = Pl_Stream_Peekc(pl_stm_tbl[stm]);
  if (c != EOF && !Is_Valid_Code(c))
    Pl_Err_Representation(pl_representation_character);

  return Pl_Get_Atom((c == EOF) ? pl_atom_end_of_file : ATOM_CHAR(c), char_word);
}




/*-------------------------------------------------------------------------*
 * PL_PEEK_CHAR_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Peek_Char_1(WamWord char_word)
{
  return Pl_Peek_Char_2(NOT_A_WAM_WORD, char_word);
}




/*-------------------------------------------------------------------------*
 * PL_PEEK_CODE_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Peek_Code_2(WamWord sora_word, WamWord code_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, TRUE);

  Pl_Check_For_Un_In_Code(code_word);

  c = Pl_Stream_Peekc(pl_stm_tbl[stm]);
  if (c != EOF && !Is_Valid_Code(c))
    Pl_Err_Representation(pl_representation_character);

  if (c == EOF)
    c = -1;

  return Pl_Get_Integer(c, code_word);
}




/*-------------------------------------------------------------------------*
 * PL_PEEK_CODE_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Peek_Code_1(WamWord code_word)
{
  return Pl_Peek_Code_2(NOT_A_WAM_WORD, code_word);
}




/*-------------------------------------------------------------------------*
 * PL_PEEK_BYTE_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Peek_Byte_2(WamWord sora_word, WamWord byte_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_input : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  pl_last_input_sora = sora_word;
  Pl_Check_Stream_Type(stm, FALSE, TRUE);

  Pl_Check_For_Un_In_Byte(byte_word);

  c = Pl_Stream_Peekc(pl_stm_tbl[stm]);
  if (c == EOF)
    c = -1;

  return Pl_Get_Integer(c, byte_word);
}




/*-------------------------------------------------------------------------*
 * PL_PEEK_BYTE_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Peek_Byte_1(WamWord byte_word)
{
  return Pl_Peek_Byte_2(NOT_A_WAM_WORD, byte_word);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_CHAR_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Put_Char_2(WamWord sora_word, WamWord char_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_output : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);

  pl_last_output_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, FALSE);

  Pl_Stream_Putc(Pl_Rd_Char_Check(char_word), pl_stm_tbl[stm]);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_CHAR_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Put_Char_1(WamWord char_word)
{
  Pl_Put_Char_2(NOT_A_WAM_WORD, char_word);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_CODE_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Put_Code_2(WamWord sora_word, WamWord code_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_output : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);

  pl_last_output_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, FALSE);

  Pl_Stream_Putc(Pl_Rd_Code_Check(code_word), pl_stm_tbl[stm]);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_CODE_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Put_Code_1(WamWord code_word)
{
  Pl_Put_Code_2(NOT_A_WAM_WORD, code_word);
}





/*-------------------------------------------------------------------------*
 * PL_PUT_BYTE_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Put_Byte_2(WamWord sora_word, WamWord byte_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_output : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);

  pl_last_output_sora = sora_word;
  Pl_Check_Stream_Type(stm, FALSE, FALSE);

  Pl_Stream_Putc(Pl_Rd_Byte_Check(byte_word), pl_stm_tbl[stm]);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_BYTE_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Put_Byte_1(WamWord byte_word)
{
  Pl_Put_Byte_2(NOT_A_WAM_WORD, byte_word);
}
