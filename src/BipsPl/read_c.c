/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : read_c.c                                                        *
 * Descr.: read/1 and friends - C part                                     *
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

#define CURRENT_CHAR_CONVERSION_ALT X1_2463757272656E745F636861725F636F6E76657273696F6E5F616C74

Prolog_Prototype(CURRENT_CHAR_CONVERSION_ALT, 0);


#define CHECK_STREAM_AND_GET_STM(sora_word, stm)		\
  stm = (sora_word == NOT_A_WAM_WORD)				\
         ? pl_stm_input :					\
         Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);	\
								\
  pl_last_input_sora = sora_word;				\
  Pl_Check_Stream_Type(stm, TRUE, TRUE)




#define CHECK_RESULT_AND_UNIFY(returned_word, term_word)	\
  if (returned_word == NOT_A_WAM_WORD)				\
    {								\
      Pl_Syntax_Error((SYS_VAR_SYNTAX_ERROR_ACTON < 0)		\
		   ? Flag_Value(syntax_error)		\
		   : SYS_VAR_SYNTAX_ERROR_ACTON);		\
      return FALSE;						\
    }								\
								\
  if (!Pl_Unify(word, term_word))				\
    return FALSE




/*-------------------------------------------------------------------------*
 * PL_READ_TERM_5                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Term_5(WamWord sora_word, WamWord term_word,
	    WamWord vars_word, WamWord var_names_word,
	    WamWord sing_names_word)
{
  WamWord word;
  int stm;
  int i;
  int parse_end_of_term = (SYS_VAR_OPTION_MASK >> 3) & 1;

  CHECK_STREAM_AND_GET_STM(sora_word, stm);
  word = Pl_Read_Term(pl_stm_tbl[stm], parse_end_of_term);
  CHECK_RESULT_AND_UNIFY(word, term_word);

  /* list of variables (i.e. [Var,...]) */

  if (SYS_VAR_OPTION_MASK & 1)
    {
      for (i = 0; i < pl_parse_nb_var; i++)
	{
	  if (!Pl_Get_List(vars_word) || !Pl_Unify_Value(pl_parse_dico_var[i].word))
	    return FALSE;

	  vars_word = Pl_Unify_Variable();
	}

      if (!Pl_Get_Nil(vars_word))
	return FALSE;
    }


  /* list of variable names (i.e. ['Name'=Var,...]) */

  if (SYS_VAR_OPTION_MASK & 2)
    {
      for (i = 0; i < pl_parse_nb_var; i++)
	{
	  if (!pl_parse_dico_var[i].named)
	    continue;
	  /* pl_glob_dico_var: variable names (atoms) */
	  pl_glob_dico_var[i] = Pl_Create_Allocate_Atom(pl_parse_dico_var[i].name);

	  word = Pl_Put_Structure(ATOM_CHAR('='), 2);
	  Pl_Unify_Atom(pl_glob_dico_var[i]);
	  Pl_Unify_Value(pl_parse_dico_var[i].word);

	  if (!Pl_Get_List(var_names_word) || !Pl_Unify_Value(word))
	    return FALSE;

	  var_names_word = Pl_Unify_Variable();
	}

      if (!Pl_Get_Nil(var_names_word))
	return FALSE;
    }

  /* list of singletons (i.e. ['Name'=Var,...]) */

  if (SYS_VAR_OPTION_MASK & 4)
    {
      for (i = 0; i < pl_parse_nb_var; i++)
	{
	  if (!pl_parse_dico_var[i].named || pl_parse_dico_var[i].nb_of_uses > 1)
	    continue;

	  if ((SYS_VAR_OPTION_MASK & 2) == 0)	/* not yet allocated */
	    pl_glob_dico_var[i] = Pl_Create_Allocate_Atom(pl_parse_dico_var[i].name);

	  word = Pl_Put_Structure(ATOM_CHAR('='), 2);
	  Pl_Unify_Atom(pl_glob_dico_var[i]);
	  Pl_Unify_Value(pl_parse_dico_var[i].word);

	  if (!Pl_Get_List(sing_names_word) || !Pl_Unify_Value(word))
	    return FALSE;

	  sing_names_word = Pl_Unify_Variable();
	}

      if (!Pl_Get_Nil(sing_names_word))
	return FALSE;
    }


  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TERM_4                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Term_4(WamWord term_word,
	    WamWord vars_word, WamWord var_names_word,
	    WamWord sing_names_word)
{
  return Pl_Read_Term_5(NOT_A_WAM_WORD, term_word, vars_word, var_names_word,
		     sing_names_word);
}




/*-------------------------------------------------------------------------*
 * PL_READ_1                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_1(WamWord term_word)
{
  return Pl_Read_Term_5(NOT_A_WAM_WORD, term_word, 0, 0, 0);
}




/*-------------------------------------------------------------------------*
 * PL_READ_2                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_2(WamWord sora_word, WamWord term_word)
{
  return Pl_Read_Term_5(sora_word, term_word, 0, 0, 0);
}




/*-------------------------------------------------------------------------*
 * PL_READ_ATOM_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Atom_2(WamWord sora_word, WamWord atom_word)
{
  WamWord word;
  int stm;

  Pl_Check_For_Un_Atom(atom_word);

  CHECK_STREAM_AND_GET_STM(sora_word, stm);
  word = Pl_Read_Atom(pl_stm_tbl[stm]);
  CHECK_RESULT_AND_UNIFY(word, atom_word);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_READ_ATOM_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Atom_1(WamWord atom_word)
{
  return Pl_Read_Atom_2(NOT_A_WAM_WORD, atom_word);
}




/*-------------------------------------------------------------------------*
 * PL_READ_INTEGER_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Integer_2(WamWord sora_word, WamWord integer_word)
{
  WamWord word;
  int stm;
  Pl_Check_For_Un_Integer(integer_word);

  CHECK_STREAM_AND_GET_STM(sora_word, stm);
  word = Pl_Read_Integer(pl_stm_tbl[stm]);
  CHECK_RESULT_AND_UNIFY(word, integer_word);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_READ_INTEGER_1                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Integer_1(WamWord integer_word)
{
  return Pl_Read_Integer_2(NOT_A_WAM_WORD, integer_word);
}




/*-------------------------------------------------------------------------*
 * PL_READ_NUMBER_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Number_2(WamWord sora_word, WamWord number_word)
{
  WamWord word;
  int stm;

  Pl_Check_For_Un_Number(number_word);

  CHECK_STREAM_AND_GET_STM(sora_word, stm);
  word = Pl_Read_Number(pl_stm_tbl[stm]);
  CHECK_RESULT_AND_UNIFY(word, number_word);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_READ_NUMBER_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Number_1(WamWord number_word)
{
  return Pl_Read_Number_2(NOT_A_WAM_WORD, number_word);
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Token_2(WamWord sora_word, WamWord token_word)
{
  WamWord word;
  int stm;

  CHECK_STREAM_AND_GET_STM(sora_word, stm);
  word = Pl_Read_Token(pl_stm_tbl[stm]);
  CHECK_RESULT_AND_UNIFY(word, token_word);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Token_1(WamWord token_word)
{
  return Pl_Read_Token_2(NOT_A_WAM_WORD, token_word);
}




/*-------------------------------------------------------------------------*
 * PL_LAST_READ_START_LINE_COLUMN_2                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Last_Read_Start_Line_Column_2(WamWord line_word, WamWord col_word)
{
  return Pl_Un_Integer_Check(pl_last_read_line, line_word) &&
    Pl_Un_Integer_Check(pl_last_read_col, col_word);
}




/*-------------------------------------------------------------------------*
 * PL_CHAR_CONVERSION_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Char_Conversion_2(WamWord in_char_word, WamWord out_char_word)
{
  int c_in, c_out;

  c_in = Pl_Rd_Char_Check(in_char_word);
  c_out = Pl_Rd_Char_Check(out_char_word);

  pl_char_conv[c_in] = c_out;
}




#define Find_Next_Char_Conversion(c_in, c_out)	\
  while (++c_in < 256)				\
    {						\
      c_out = pl_char_conv[c_in];			\
      if (c_in != c_out)			\
	break;					\
    }

/*-------------------------------------------------------------------------*
 * PL_CURRENT_CHAR_CONVERSION_2                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Char_Conversion_2(WamWord in_char_word, WamWord out_char_word)
{
  WamWord word, tag_mask;
  int c_in, c_out;
  int c_in1, c_out1;

  Pl_Check_For_Un_Char(out_char_word);

  DEREF(in_char_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    {
      c_in = Pl_Rd_Char_Check(word);
      c_out = pl_char_conv[c_in];
      return c_in != c_out && Pl_Un_Char_Check(c_out, out_char_word);
    }

  c_in = -1;
  Find_Next_Char_Conversion(c_in, c_out);
  if (c_in >= 256)
    return FALSE;

  c_in1 = c_in;
  Find_Next_Char_Conversion(c_in1, c_out1);

  if (c_in1 < 256)		/* non deterministic case */
    {
      A(0) = in_char_word;
      A(1) = out_char_word;
      A(2) = c_in1;
      A(3) = c_out1;

      Pl_Create_Choice_Point((CodePtr)
			  Prolog_Predicate(CURRENT_CHAR_CONVERSION_ALT, 0),
			  4);
    }

  return Pl_Get_Atom(ATOM_CHAR(c_in), in_char_word) &&
    Pl_Get_Atom(ATOM_CHAR(c_out), out_char_word);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_CHAR_CONVERSION_ALT_0                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Char_Conversion_Alt_0(void)
{
  WamWord in_char_word, out_char_word;
  int c_in, c_out;
  int c_in1, c_out1;


  Pl_Update_Choice_Point((CodePtr)
		      Prolog_Predicate(CURRENT_CHAR_CONVERSION_ALT, 0), 0);

  in_char_word = AB(B, 0);
  out_char_word = AB(B, 1);
  c_in = AB(B, 2);
  c_out = AB(B, 3);

  c_in1 = c_in;
  Find_Next_Char_Conversion(c_in1, c_out1);

  if (c_in1 >= 256)
    Delete_Last_Choice_Point();
  else				/* non deterministic case */
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = in_char_word;
      AB(B, 1) = out_char_word;
#endif
      AB(B, 2) = c_in1;
      AB(B, 3) = c_out1;
    }

  return Pl_Get_Atom(ATOM_CHAR(c_in), in_char_word) &&
    Pl_Get_Atom(ATOM_CHAR(c_out), out_char_word);
}
