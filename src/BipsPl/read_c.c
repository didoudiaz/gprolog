/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : read_c.c                                                        *
 * Descr.: read/1 and friends - C part                                     *
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

static Bool Common_Read(WamWord (*read_fct) (),
			WamWord sora_word, WamWord term_word);



#define CURRENT_CHAR_CONVERSION_ALT X2463757272656E745F636861725F636F6E76657273696F6E5F616C74

Prolog_Prototype(CURRENT_CHAR_CONVERSION_ALT, 0);




/*-------------------------------------------------------------------------*
 * COMMON_READ                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Common_Read(WamWord (*read_fct) (), WamWord sora_word, WamWord term_word)
{
  WamWord word;
  int stm;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_input : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_INPUT);

  last_input_sora = sora_word;
  Check_Stream_Type(stm, TRUE, TRUE);


  word = (*read_fct) (stm_tbl + stm);
  if (word == NOT_A_WAM_WORD)
    {
      Syntax_Error((SYS_VAR_SYNTAX_ERROR_ACTON < 0)
		   ? Flag_Value(FLAG_SYNTAX_ERROR)
		   : SYS_VAR_SYNTAX_ERROR_ACTON);
      return FALSE;
    }

  return Unify(word, term_word);
}




/*-------------------------------------------------------------------------*
 * READ_TERM_5                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Term_5(WamWord sora_word, WamWord term_word,
	    WamWord vars_word, WamWord var_names_word,
	    WamWord sing_names_word)
{
  WamWord word;
  int i;

  parse_end_of_term = (SYS_VAR_OPTION_MASK >> 3) & 1;

  if (!Common_Read(Read_Term, sora_word, term_word))
    return FALSE;

  /* list of variables (i.e. [Var,...]) */

  if (SYS_VAR_OPTION_MASK & 1)
    {
      for (i = 0; i < parse_nb_var; i++)
	{
	  if (!Get_List(vars_word) || !Unify_Value(parse_dico_var[i].word))
	    return FALSE;

	  vars_word = Unify_Variable();
	}

      if (!Get_Nil(vars_word))
	return FALSE;
    }


  /* list of variable names (i.e. ['Name'=Var,...]) */

  if (SYS_VAR_OPTION_MASK & 2)
    {
      for (i = 0; i < parse_nb_var; i++)
	{
	  if (!parse_dico_var[i].named)
	    continue;
	  /* glob_dico_var: variable names (atoms) */
	  glob_dico_var[i] = Create_Allocate_Atom(parse_dico_var[i].name);

	  word = Put_Structure(ATOM_CHAR('='), 2);
	  Unify_Atom(glob_dico_var[i]);
	  Unify_Value(parse_dico_var[i].word);

	  if (!Get_List(var_names_word) || !Unify_Value(word))
	    return FALSE;

	  var_names_word = Unify_Variable();
	}

      if (!Get_Nil(var_names_word))
	return FALSE;
    }

  /* list of singletons (i.e. ['Name'=Var,...]) */

  if (SYS_VAR_OPTION_MASK & 4)
    {
      for (i = 0; i < parse_nb_var; i++)
	{
	  if (!parse_dico_var[i].named || parse_dico_var[i].nb_of_uses > 1)
	    continue;

	  if ((SYS_VAR_OPTION_MASK & 2) == 0)	/* not yet allocated */
	    glob_dico_var[i] = Create_Allocate_Atom(parse_dico_var[i].name);

	  word = Put_Structure(ATOM_CHAR('='), 2);
	  Unify_Atom(glob_dico_var[i]);
	  Unify_Value(parse_dico_var[i].word);

	  if (!Get_List(sing_names_word) || !Unify_Value(word))
	    return FALSE;

	  sing_names_word = Unify_Variable();
	}

      if (!Get_Nil(sing_names_word))
	return FALSE;
    }


  return TRUE;
}




/*-------------------------------------------------------------------------*
 * READ_TERM_4                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Term_4(WamWord term_word,
	    WamWord vars_word, WamWord var_names_word,
	    WamWord sing_names_word)
{
  return Read_Term_5(NOT_A_WAM_WORD, term_word, vars_word, var_names_word,
		     sing_names_word);
}




/*-------------------------------------------------------------------------*
 * READ_1                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_1(WamWord term_word)
{
  return Read_Term_5(NOT_A_WAM_WORD, term_word, 0, 0, 0);
}




/*-------------------------------------------------------------------------*
 * READ_2                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_2(WamWord sora_word, WamWord term_word)
{
  return Read_Term_5(sora_word, term_word, 0, 0, 0);
}




/*-------------------------------------------------------------------------*
 * READ_ATOM_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Atom_2(WamWord sora_word, WamWord atom_word)
{
  Check_For_Un_Atom(atom_word);
  return Common_Read(Read_Atom, sora_word, atom_word);
}




/*-------------------------------------------------------------------------*
 * READ_ATOM_1                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Atom_1(WamWord atom_word)
{
  return Read_Atom_2(NOT_A_WAM_WORD, atom_word);
}




/*-------------------------------------------------------------------------*
 * READ_INTEGER_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Integer_2(WamWord sora_word, WamWord integer_word)
{
  Check_For_Un_Integer(integer_word);
  return Common_Read(Read_Integer, sora_word, integer_word);
}




/*-------------------------------------------------------------------------*
 * READ_INTEGER_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Integer_1(WamWord integer_word)
{
  return Read_Integer_2(NOT_A_WAM_WORD, integer_word);
}




/*-------------------------------------------------------------------------*
 * READ_NUMBER_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Number_2(WamWord sora_word, WamWord number_word)
{
  Check_For_Un_Number(number_word);
  return Common_Read(Read_Number, sora_word, number_word);
}




/*-------------------------------------------------------------------------*
 * READ_NUMBER_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Number_1(WamWord number_word)
{
  return Read_Number_2(NOT_A_WAM_WORD, number_word);
}




/*-------------------------------------------------------------------------*
 * READ_TOKEN_2                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Token_2(WamWord sora_word, WamWord token_word)
{
  return Common_Read(Read_Token, sora_word, token_word);
}




/*-------------------------------------------------------------------------*
 * READ_TOKEN_1                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Token_1(WamWord token_word)
{
  return Read_Token_2(NOT_A_WAM_WORD, token_word);
}




/*-------------------------------------------------------------------------*
 * LAST_READ_START_LINE_COLUMN_2                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Last_Read_Start_Line_Column_2(WamWord line_word, WamWord col_word)
{
  return Un_Integer_Check(last_read_line, line_word) &&
    Un_Integer_Check(last_read_col, col_word);
}




/*-------------------------------------------------------------------------*
 * CHAR_CONVERSION_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Char_Conversion_2(WamWord in_char_word, WamWord out_char_word)
{
  int c_in, c_out;

  c_in = Rd_Char_Check(in_char_word);
  c_out = Rd_Char_Check(out_char_word);

  char_conv[c_in] = c_out;
}




#define Find_Next_Char_Conversion(c_in, c_out)	\
  while (++c_in < 256)				\
    {						\
      c_out = char_conv[c_in];			\
      if (c_in != c_out)			\
	break;					\
    }

/*-------------------------------------------------------------------------*
 * CURRENT_CHAR_CONVERSION_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Current_Char_Conversion_2(WamWord in_char_word, WamWord out_char_word)
{
  WamWord word, tag_mask;
  int c_in, c_out;
  int c_in1, c_out1;

  Check_For_Un_Char(out_char_word);

  DEREF(in_char_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    {
      c_in = Rd_Char_Check(word);
      c_out = char_conv[c_in];
      return c_in != c_out && Un_Char_Check(c_out, out_char_word);
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

      Create_Choice_Point((CodePtr)
			  Prolog_Predicate(CURRENT_CHAR_CONVERSION_ALT, 0),
			  4);
    }

  return Get_Atom(ATOM_CHAR(c_in), in_char_word) &&
    Get_Atom(ATOM_CHAR(c_out), out_char_word);
}




/*-------------------------------------------------------------------------*
 * CURRENT_CHAR_CONVERSION_ALT_0                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Current_Char_Conversion_Alt_0(void)
{
  WamWord in_char_word, out_char_word;
  int c_in, c_out;
  int c_in1, c_out1;


  Update_Choice_Point((CodePtr)
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

  return Get_Atom(ATOM_CHAR(c_in), in_char_word) &&
    Get_Atom(ATOM_CHAR(c_out), out_char_word);
}
