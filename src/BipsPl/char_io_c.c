/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : char_io_c.c                                                     *
 * Descr.: character input-output management - C part                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
 * GET_CODE_NO_ECHO_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Code_No_Echo_2(WamWord sora_word, WamWord code_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, TRUE, TRUE);

#if 0				/* not tested to allow for function keys */
  Check_For_Un_Code(code_word);
#else
  Check_For_Un_Integer(code_word);
#endif


  c = Stream_Getc_No_Echo(stm_tbl + stm);
#if 0				/* not tested to allow for function keys */
  if (c != EOF && !Is_Valid_Code(c))
    Pl_Err_Representation(representation_character);
#endif

  if (c == EOF)
    c = -1;

  return Get_Integer(c, code_word);
}




/*-------------------------------------------------------------------------*
 * GET_CODE_NO_ECHO_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Code_No_Echo_1(WamWord code_word)
{
  return Get_Code_No_Echo_2(NOT_A_WAM_WORD, code_word);
}




/*-------------------------------------------------------------------------*
 * GET_CHAR_2                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Char_2(WamWord sora_word, WamWord char_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, TRUE, TRUE);

  Check_For_Un_In_Char(char_word);

  c = Stream_Getc(stm_tbl + stm);
  if (c != EOF && !Is_Valid_Code(c))
    Pl_Err_Representation(representation_character);

  return Get_Atom((c == EOF) ? atom_end_of_file : ATOM_CHAR(c), char_word);
}




/*-------------------------------------------------------------------------*
 * GET_CHAR_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Char_1(WamWord char_word)
{
  return Get_Char_2(NOT_A_WAM_WORD, char_word);
}




/*-------------------------------------------------------------------------*
 * GET_CODE_2                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Code_2(WamWord sora_word, WamWord code_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, TRUE, TRUE);

  Check_For_Un_In_Code(code_word);

  c = Stream_Getc(stm_tbl + stm);
  if (c != EOF && !Is_Valid_Code(c))
    Pl_Err_Representation(representation_character);

  if (c == EOF)
    c = -1;

  return Get_Integer(c, code_word);
}




/*-------------------------------------------------------------------------*
 * GET_CODE_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Code_1(WamWord code_word)
{
  return Get_Code_2(NOT_A_WAM_WORD, code_word);
}





/*-------------------------------------------------------------------------*
 * GET_BYTE_2                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Byte_2(WamWord sora_word, WamWord byte_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, FALSE, TRUE);

  Check_For_Un_In_Byte(byte_word);

  c = Stream_Getc(stm_tbl + stm);
  if (c == EOF)
    c = -1;

  return Get_Integer(c, byte_word);
}




/*-------------------------------------------------------------------------*
 * GET_BYTE_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Byte_1(WamWord byte_word)
{
  return Get_Byte_2(NOT_A_WAM_WORD, byte_word);
}






/*-------------------------------------------------------------------------*
 * UNGET_CHAR_2                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Unget_Char_2(WamWord sora_word, WamWord char_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, TRUE, TRUE);

  Stream_Ungetc(Rd_Char_Check(char_word), stm_tbl + stm);
}




/*-------------------------------------------------------------------------*
 * UNGET_CHAR_1                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Unget_Char_1(WamWord char_word)
{
  Unget_Char_2(NOT_A_WAM_WORD, char_word);
}




/*-------------------------------------------------------------------------*
 * UNGET_CODE_2                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Unget_Code_2(WamWord sora_word, WamWord code_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, TRUE, TRUE);

  Stream_Ungetc(Rd_Code_Check(code_word), stm_tbl + stm);
}




/*-------------------------------------------------------------------------*
 * UNGET_CODE_1                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Unget_Code_1(WamWord code_word)
{
  Unget_Code_2(NOT_A_WAM_WORD, code_word);
}





/*-------------------------------------------------------------------------*
 * UNGET_BYTE_2                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Unget_Byte_2(WamWord sora_word, WamWord byte_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, FALSE, TRUE);

  Stream_Ungetc(Rd_Byte_Check(byte_word), stm_tbl + stm);
}




/*-------------------------------------------------------------------------*
 * UNGET_BYTE_1                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Unget_Byte_1(WamWord byte_word)
{
  Unget_Byte_2(NOT_A_WAM_WORD, byte_word);
}




/*-------------------------------------------------------------------------*
 * PEEK_CHAR_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Peek_Char_2(WamWord sora_word, WamWord char_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, TRUE, TRUE);

  Check_For_Un_In_Char(char_word);

  c = Stream_Peekc(stm_tbl + stm);
  if (c != EOF && !Is_Valid_Code(c))
    Pl_Err_Representation(representation_character);

  return Get_Atom((c == EOF) ? atom_end_of_file : ATOM_CHAR(c), char_word);
}




/*-------------------------------------------------------------------------*
 * PEEK_CHAR_1                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Peek_Char_1(WamWord char_word)
{
  return Peek_Char_2(NOT_A_WAM_WORD, char_word);
}




/*-------------------------------------------------------------------------*
 * PEEK_CODE_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Peek_Code_2(WamWord sora_word, WamWord code_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, TRUE, TRUE);

  Check_For_Un_In_Code(code_word);

  c = Stream_Peekc(stm_tbl + stm);
  if (c != EOF && !Is_Valid_Code(c))
    Pl_Err_Representation(representation_character);

  if (c == EOF)
    c = -1;

  return Get_Integer(c, code_word);
}




/*-------------------------------------------------------------------------*
 * PEEK_CODE_1                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Peek_Code_1(WamWord code_word)
{
  return Peek_Code_2(NOT_A_WAM_WORD, code_word);
}




/*-------------------------------------------------------------------------*
 * PEEK_BYTE_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Peek_Byte_2(WamWord sora_word, WamWord byte_word)
{
  int stm;
  int c;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, FALSE, TRUE);

  Check_For_Un_In_Byte(byte_word);

  c = Stream_Peekc(stm_tbl + stm);
  if (c == EOF)
    c = -1;

  return Get_Integer(c, byte_word);
}




/*-------------------------------------------------------------------------*
 * PEEK_BYTE_1                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Peek_Byte_1(WamWord byte_word)
{
  return Peek_Byte_2(NOT_A_WAM_WORD, byte_word);
}




/*-------------------------------------------------------------------------*
 * PUT_CHAR_2                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Put_Char_2(WamWord sora_word, WamWord char_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_output : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);

  last_output_sora = sora_word;
  Check_Stream_Type(stm, TRUE, FALSE);

  Stream_Putc(Rd_Char_Check(char_word), stm_tbl + stm);
}




/*-------------------------------------------------------------------------*
 * PUT_CHAR_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Put_Char_1(WamWord char_word)
{
  Put_Char_2(NOT_A_WAM_WORD, char_word);
}




/*-------------------------------------------------------------------------*
 * PUT_CODE_2                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Put_Code_2(WamWord sora_word, WamWord code_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_output : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);

  last_output_sora = sora_word;
  Check_Stream_Type(stm, TRUE, FALSE);

  Stream_Putc(Rd_Code_Check(code_word), stm_tbl + stm);
}




/*-------------------------------------------------------------------------*
 * PUT_CODE_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Put_Code_1(WamWord code_word)
{
  Put_Code_2(NOT_A_WAM_WORD, code_word);
}





/*-------------------------------------------------------------------------*
 * PUT_BYTE_2                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Put_Byte_2(WamWord sora_word, WamWord byte_word)
{
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_output : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);

  last_output_sora = sora_word;
  Check_Stream_Type(stm, FALSE, FALSE);

  Stream_Putc(Rd_Byte_Check(byte_word), stm_tbl + stm);
}




/*-------------------------------------------------------------------------*
 * PUT_BYTE_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Put_Byte_1(WamWord byte_word)
{
  Put_Byte_2(NOT_A_WAM_WORD, byte_word);
}
