/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : parse_supp.c                                                    *
 * Descr.: parser support                                                  *
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
#include <string.h>
#include <ctype.h>
#include <setjmp.h>

#define OBJ_INIT Parse_Supp_Initializer

#define PARSE_SUPP_FILE

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define COMMA_ANY                  FALSE


#define GENERAL_TERM               0
#define TRYING_PREFIX              1
#define INSIDE_ANY_OP              2




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static StmInf *pstm_i;

static Bool tok_present;

static jmp_buf jumper;

#if !defined(NO_USE_REGS) && NB_OF_USED_MACHINE_REGS>0
static WamWord buff_save_machine_regs[NB_OF_USED_MACHINE_REGS];
#endif

static int atom_var;
static int atom_string;
static int atom_punct;
static int atom_atom;
static int atom_back_quotes;
static int atom_full_stop;
static int atom_extend;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Read_Next_Token(Bool comma_is_punct);

static WamWord Parse_Term(int cur_prec, int context, Bool comma_is_punct);

static WamWord Parse_Args_Of_Functor(int atom);

static WamWord Parse_Bracketed_Term(void);

static WamWord Parse_List(Bool can_be_empty);

static int Lookup_In_Dico_Var(char *name);

static void Parse_Error(char *err_msg);



#define   Unget_Token           tok_present=TRUE



#define Update_Last_Read_Position                                           \
    {                                                                       \
     last_read_line=token.line;                                             \
     last_read_col =token.col;                                              \
    }




/*-------------------------------------------------------------------------*
 * PARSE_SUPP_INITIALIZER                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Parse_Supp_Initializer(void)
{
  atom_var = Create_Atom("var");
  atom_string = Create_Atom("string");
  atom_punct = Create_Atom("punct");
  atom_atom = Create_Atom("atom");
  atom_back_quotes = Create_Atom("back_quotes");
  atom_full_stop = Create_Atom("full_stop");
  atom_extend = Create_Atom("extend");

  parse_end_of_term = PARSE_END_OF_TERM_DOT;
}




/*-------------------------------------------------------------------------*
 * READ_NEXT_TOKEN                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Read_Next_Token(Bool comma_is_punct)
{
  char *err_msg;

  if (tok_present)
    tok_present = FALSE;
  else if ((err_msg = Scan_Token(pstm_i, comma_is_punct)) != NULL)
    Parse_Error(err_msg);
}




/*-------------------------------------------------------------------------*
 * READ_TERM                                                               *
 *                                                                         *
 * Returns a Prolog term as a WAM word or NOT_A_WAM_WORD on syntax error.  *
 *-------------------------------------------------------------------------*/
WamWord
Read_Term(StmInf *pstm)
{
  int jmp_val;
  WamWord term;

  parse_nb_var = 0;
  last_read_line = -1;

  pstm_i = pstm;

  tok_present = FALSE;
  Save_Machine_Regs(buff_save_machine_regs);

  jmp_val = setjmp(jumper);

  Restore_Machine_Regs(buff_save_machine_regs);

  if (jmp_val == 0)
    {
      term = Parse_Term(MAX_PREC, GENERAL_TERM, COMMA_ANY);
      Read_Next_Token(COMMA_ANY);

      if (term == NOT_A_WAM_WORD)
	{
	  if (token.type == TOKEN_END_OF_FILE)
	    {
	      term = Put_Atom(atom_end_of_file);
	      goto finish;
	    }
	  else
	    Parse_Error("expression expected");
	}

      if (parse_end_of_term == PARSE_END_OF_TERM_DOT)
	{
	  if (token.type == TOKEN_FULL_STOP)
	    goto finish;
	  else
	    {
	      Unget_Token;
	      Parse_Error(". or operator expected after expression");
	    }
	}
      /* parse_end_of_term==PARSE_END_OF_TERM_EOF */
      if (token.type == TOKEN_END_OF_FILE)
	goto finish;
      else
	{
	  Unget_Token;
	  Parse_Error("eof or operator expected after expression");
	}
    }
  else				/* Syntax Error (Restore) */
    {
      Restore_Machine_Regs(buff_save_machine_regs);
      term = NOT_A_WAM_WORD;
    }

finish:
  return term;
}




/*-------------------------------------------------------------------------*
 * PARSE_TERM                                                              *
 *                                                                         *
 * Recursively parses a term whose precedence is <=cur_prec and returns a  *
 * WamWord associated to the term. If a syntax error is encountered the    *
 * parser returns NOT_A_WAM_WORD and update syntax error information       *
 * (see Set_Syntax_Error_Info() in error_supp.c).                          *
 * The flag comma_is_punct specifies if an eventual ',' following the term *
 * must be considered as a punctuation (separator of args of compound term *
 * or of a list) or as an atom. The value COMMA_ANY is used when this flag *
 * is not relevant (only for comprehensivity).                             *
 * Since the Scan_Token() only consumes necessary characters, the function *
 * Stream_Peekc() returns the character immediately after the token.       *
 * This feature is used to detect negative numbers and open bracket        *
 * (thus the next call to Scan_Token() will return TOKEN_IMMEDIAT_OPEN).   *
 * We use Scan_Peek_Char() instead of Stream_Peekc() only to deal with     *
 * character conversion.                                                   *
 *-------------------------------------------------------------------------*/
static WamWord
Parse_Term(int cur_prec, int context, Bool comma_is_punct)
{
  Bool bracket;
  int atom;
  OperInf *oper;
  Bool infix_op;
  int cur_left = 0;
  int i;
  int flag_value;
  WamWord term, term1;
  WamWord w[2];
  Bool left_is_op = FALSE;


  Read_Next_Token(context != TRYING_PREFIX || comma_is_punct);
  if (last_read_line == -1)
    Update_Last_Read_Position;

  switch (token.type)
    {
    case TOKEN_VARIABLE:
      i = Lookup_In_Dico_Var(token.name);
      if (++parse_dico_var[i].nb_of_uses == 1)	/* first occurence */
	{
	  term = Put_X_Variable();
	  parse_dico_var[i].word = term;
	}
      else			/* other occurence */
	term = parse_dico_var[i].word;
      break;

    case TOKEN_INTEGER:
      term = Put_Integer(token.int_num);
      break;

    case TOKEN_FLOAT:
      term = Put_Float(token.float_num);
      break;

    case TOKEN_STRING:
      flag_value = Flag_Value(FLAG_DOUBLE_QUOTES);
      if (flag_value == FLAG_DOUBLE_QUOTES_ATOM)
	{
	  atom = Create_Allocate_Atom(token.name);
	  goto a_name;
	}

      i = strlen(token.name);
      term = NIL_WORD;		/* faster than Put_Nil() */
      while (i--)
	{
	  term1 = Put_List();
	  if (flag_value == FLAG_DOUBLE_QUOTES_CODES)
	    Unify_Integer(token.name[i]);
	  else
	    Unify_Atom(ATOM_CHAR(token.name[i]));

	  Unify_Value(term);
	  term = term1;
	}
      break;

    case TOKEN_IMMEDIAT_OPEN:
      token.punct = '(';	/* and then like TOKEN_PUNCTUATION */
    case TOKEN_PUNCTUATION:
      if (!strchr("({[", token.punct))
	{
	  term = NOT_A_WAM_WORD;
	  goto finish;
	}

      atom = (token.punct == '{') ? atom_curly_brackets : ATOM_NIL;

      term = Parse_Bracketed_Term();
      if (term == NOT_A_WAM_WORD)	/* name: {} or [] */
	goto a_name;

      break;

    case TOKEN_NAME:
    case TOKEN_BACK_QUOTED:	/* undefined in ISO */
      atom = Create_Allocate_Atom(token.name);

    a_name:
      bracket = (Scan_Peek_Char(pstm_i, TRUE) == '(');
      if (bracket)
	{
	  term = Parse_Args_Of_Functor(atom);
	  break;
	}

      /* maybe a prefix operator */
      if ((oper = Lookup_Oper(atom, PREFIX)) && cur_prec >= oper->prec)
	{			/* negative number */
	  if (token.name[0] == '-' && token.name[1] == '\0' &&
	      isdigit(Scan_Peek_Char(pstm_i, TRUE)))
	    {
	      Read_Next_Token(COMMA_ANY);
	      term = (token.type == TOKEN_INTEGER)
		? Put_Integer(-token.int_num) : Put_Float(-token.float_num);
	      break;
	    }

	  /* try a prefix operator */
	  cur_left = oper->prec;
	  term = Parse_Term(oper->right, TRYING_PREFIX, comma_is_punct);
	  if (term != NOT_A_WAM_WORD)
	    {
	      term = Mk_Compound(atom, 1, &term);
	      break;
	    }
	  /* prefix operator as a name */
	  if (context != GENERAL_TERM)
	    Parse_Error
	      ("expression expected or previous operator needs brackets");
	}

      left_is_op = (Check_Oper_Any_Type(atom));

      if (left_is_op && context != GENERAL_TERM)	/* in operator context */
	{
	  if (Check_Oper(atom, INFIX) || Check_Oper(atom, POSTFIX))
	    Parse_Error("current or previous operator needs brackets");
	  else
	    Parse_Error("current operator needs brackets");
	}

      term = Put_Atom(atom);
      break;

    default:			/* TOKEN_END_OF_FILE, TOKEN_FULL_STOP, TOKEN_EXTENDED */
      term = NOT_A_WAM_WORD;
      goto finish;
    }


  for (;;)
    {
      Read_Next_Token(comma_is_punct);

      if (token.type != TOKEN_NAME)
	break;

      atom = Create_Allocate_Atom(token.name);
      if ((oper = Lookup_Oper(atom, INFIX)))
	infix_op = TRUE;
      else if ((oper = Lookup_Oper(atom, POSTFIX)))
	infix_op = FALSE;
      else
	break;

      if (left_is_op)
	Parse_Error("previous operator needs brackets");

      if (cur_prec < oper->prec || cur_left > oper->left)
	break;

      if (infix_op)		/* infix operator */
	{
	  w[0] = term;
	  w[1] = Parse_Term(oper->right, INSIDE_ANY_OP, comma_is_punct);

	  if (w[1] == NOT_A_WAM_WORD)
	    Parse_Error("right operand expected for infix operator");

	  term = Mk_Compound(atom, 2, w);
	}
      else
	term = Mk_Compound(atom, 1, &term);	/* postfix operator */

      cur_left = oper->prec;
    }

finish:
  Unget_Token;

  return term;
}




/*-------------------------------------------------------------------------*
 * PARSE_ARGS_OF_FUNCTOR                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Parse_Args_Of_Functor(int atom)
{
  WamWord w[NB_OF_X_REGS];
  int i;

  Read_Next_Token(COMMA_ANY);	/* the immediate ( */

  i = 0;
  do
    {
      if (i >= MAX_ARITY)
	Parse_Error("too big compound term (exceeds max_arity)");

      w[i] = Parse_Term(MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM, TRUE);

      if (w[i++] == NOT_A_WAM_WORD)
	Parse_Error("expression expected");
      Read_Next_Token(TRUE);
    }
  while (token.type == TOKEN_PUNCTUATION && token.punct == ',');

  if (token.type != TOKEN_PUNCTUATION || token.punct != ')')
    Parse_Error(", or ) expected");

  return Mk_Compound(atom, i, w);
}




/*-------------------------------------------------------------------------*
 * PARSE_BRACKETED_TERM                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Parse_Bracketed_Term(void)
{
  WamWord term;


  switch (token.punct)
    {
    case '(':
      term = Parse_Term(MAX_PREC, GENERAL_TERM, COMMA_ANY);
      if (term == NOT_A_WAM_WORD)
	Parse_Error("expression expected");

      Read_Next_Token(COMMA_ANY);
      if (token.type != TOKEN_PUNCTUATION || token.punct != ')')
	Parse_Error(") or operator expected");
      break;

    case '{':
      term = Parse_Term(MAX_PREC, GENERAL_TERM, COMMA_ANY);

      Read_Next_Token(COMMA_ANY);
      if (token.type != TOKEN_PUNCTUATION || token.punct != '}')
	Parse_Error("} or operator expected");

      if (term != NOT_A_WAM_WORD)	/* term==NOT_A_WAM_WORD if {} */
	term = Mk_Compound(atom_curly_brackets, 1, &term);
      break;

    case '[':
      term = Parse_List(TRUE);	/* term==NOT_A_WAM_WORD if [] */
      break;
    }


  return term;
}




/*-------------------------------------------------------------------------*
 * PARSE_LIST                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Parse_List(Bool can_be_empty)
{
  WamWord term;
  WamWord car_word, cdr_word;


  car_word = Parse_Term(MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM, TRUE);

  Read_Next_Token(TRUE);

  if (car_word == NOT_A_WAM_WORD)
    {
      if (!can_be_empty)
	Parse_Error("expression expected in list");
      else if (token.type != TOKEN_PUNCTUATION || token.punct != ']')
	Parse_Error("expression or ] expected in list");

      return NOT_A_WAM_WORD;
    }


  if (token.type != TOKEN_PUNCTUATION || !strchr(",|]", token.punct))
    Parse_Error(", | ] or operator expected in list");

  switch (token.punct)
    {
    case ',':			/* [X,[...]] */
      cdr_word = Parse_List(FALSE);
      break;

    case '|':			/* [X|Y] */
      cdr_word =
	Parse_Term(MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM, COMMA_ANY);
      if (cdr_word == NOT_A_WAM_WORD)
	Parse_Error("expression expected in list");

      Read_Next_Token(TRUE);
      if (token.type != TOKEN_PUNCTUATION || token.punct != ']')
	Parse_Error("] or operator expected in list");

      break;

    case ']':			/* [X] */
      cdr_word = NIL_WORD;	/* faster than Put_Nil() */
      break;
    }

  term = Put_List();
  Unify_Value(car_word);
  Unify_Value(cdr_word);

  return term;
}




/*-------------------------------------------------------------------------*
 * LOOKUP_IN_DICO_VAR                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Lookup_In_Dico_Var(char *name)
{
  int i;
  Bool named;

  if (name[0] != '_' || name[1] != '\0')
    {
      named = TRUE;
      for (i = 0;
	   i < parse_nb_var && strcmp(name, parse_dico_var[i].name) != 0;
	   i++)
	;
    }
  else
    {
      named = FALSE;
      i = parse_nb_var;
    }

  if (i == parse_nb_var)
    {
      if (parse_nb_var >= MAX_VAR_IN_TERM)
	Parse_Error("too many variables in a term");

      strcpy(parse_dico_var[parse_nb_var].name, name);
      parse_dico_var[parse_nb_var].word = NOT_A_WAM_WORD;
      parse_dico_var[parse_nb_var].named = named;
      parse_dico_var[parse_nb_var].nb_of_uses = 0;
      parse_nb_var++;
    }

  return i;
}




/*-------------------------------------------------------------------------*
 * PARSE_ERROR                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Parse_Error(char *err_msg)
{
  Set_Last_Syntax_Error(atom_tbl[pstm_i->atom_file_name].name,
			token.line, token.col, err_msg);

  Recover_After_Error(pstm_i);

  Save_Machine_Regs(buff_save_machine_regs);
  longjmp(jumper, 1);
}




	  /* Other facilities */


/*-------------------------------------------------------------------------*
 * READ_ATOM                                                               *
 *                                                                         *
 * Returns a Prolog atom as a WAM word or NOT_A_WAM_WORD on syntax error.  *
 *-------------------------------------------------------------------------*/
WamWord
Read_Atom(StmInf *pstm)
{
  char *err_msg;

  if ((err_msg = Scan_Next_Atom(pstm)) != NULL)
    {
      Set_Last_Syntax_Error(atom_tbl[pstm->atom_file_name].name,
			    token.line, token.col, err_msg);
      return NOT_A_WAM_WORD;
    }

  Update_Last_Read_Position;

  return Put_Atom(Create_Allocate_Atom(token.name));
}




/*-------------------------------------------------------------------------*
 * READ_INTEGER                                                            *
 *                                                                         *
 * Returns a Prolog integer as a WAM word or NOT_A_WAM_WORD on syntax error*
 *-------------------------------------------------------------------------*/
WamWord
Read_Integer(StmInf *pstm)
{
  char *err_msg;

  if ((err_msg = Scan_Next_Number(pstm, TRUE)) != NULL)
    {
      Set_Last_Syntax_Error(atom_tbl[pstm->atom_file_name].name,
			    token.line, token.col, err_msg);
      return NOT_A_WAM_WORD;
    }

  Update_Last_Read_Position;

  return Put_Integer(token.int_num);
}





/*-------------------------------------------------------------------------*
 * READ_NUMBER                                                             *
 *                                                                         *
 * Returns a Prolog number as a WAM word or NOT_A_WAM_WORD on syntax error.*
 *-------------------------------------------------------------------------*/
WamWord
Read_Number(StmInf *pstm)
{
  char *err_msg;

  if ((err_msg = Scan_Next_Number(pstm, FALSE)) != NULL)
    {
      Set_Last_Syntax_Error(atom_tbl[pstm->atom_file_name].name,
			    token.line, token.col, err_msg);
      return NOT_A_WAM_WORD;
    }

  Update_Last_Read_Position;

  return (token.type == TOKEN_INTEGER) ? Put_Integer(token.int_num)
    : Put_Float(token.float_num);
}




/*-------------------------------------------------------------------------*
 * READ_TOKEN                                                              *
 *                                                                         *
 * Returns a Prolog token as a WAM word or NOT_A_WAM_WORD on syntax error. *
 *-------------------------------------------------------------------------*/
WamWord
Read_Token(StmInf *pstm)
{
  WamWord term, arg;
  int func, atom;
  char *err_msg;

  if ((err_msg = Scan_Token(pstm, FALSE)) != NULL)
    {
      Set_Last_Syntax_Error(atom_tbl[pstm->atom_file_name].name,
			    token.line, token.col, err_msg);
      return NOT_A_WAM_WORD;
    }

  Update_Last_Read_Position;

  term = NOT_A_WAM_WORD;

  switch (token.type)
    {
    case TOKEN_VARIABLE:
      func = atom_var;
    arg_of_struct:
      atom = Create_Allocate_Atom(token.name);
      arg = Put_Atom(atom);
      break;

    case TOKEN_INTEGER:
      term = Put_Integer(token.int_num);
      break;

    case TOKEN_FLOAT:
      term = Put_Float(token.float_num);
      break;

    case TOKEN_STRING:
      func = atom_string;
      goto arg_of_struct;

    case TOKEN_IMMEDIAT_OPEN:
      token.punct = '(';	/* and then like TOKEN_PUNCTUATION */
    case TOKEN_PUNCTUATION:
      func = atom_punct;
      atom = ATOM_CHAR(token.punct);
      arg = Put_Atom(atom);
      break;

    case TOKEN_NAME:
      atom = Create_Allocate_Atom(token.name);
      term = Put_Atom(atom);
      break;

    case TOKEN_BACK_QUOTED:	/* undefined in ISO */
      func = atom_back_quotes;
      goto arg_of_struct;

    case TOKEN_FULL_STOP:
      func = atom_punct;
      arg = Put_Atom(atom_full_stop);
      break;

    case TOKEN_END_OF_FILE:
      func = atom_punct;
      arg = Put_Atom(atom_end_of_file);
      break;

    case TOKEN_EXTENDED:
      func = atom_extend;
      goto arg_of_struct;
    }

  if (term == NOT_A_WAM_WORD)
    {
      term = Put_Structure(func, 1);
      Unify_Value(arg);
    }

  return term;
}
