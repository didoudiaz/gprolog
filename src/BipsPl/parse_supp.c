/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : parse_supp.c                                                    *
 * Descr.: parser support                                                  *
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
static TokInf unget_tok;


static sigjmp_buf jumper;

#if !defined(NO_USE_REGS) && NB_OF_USED_MACHINE_REGS > 0
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

static WamWord Create_Structure(int func, int arity, WamWord *arg);

static int Lookup_In_Dico_Var(char *name);

static void Parse_Error(char *err_msg);


/* we simply save line/col (for error report) to avoid to 
 * duplicate the entire token (with the big buffer for names).
 * After Unget_Token it is possible to restore pl_token.line/col */

#define Unget_Token				\
do {						\
  tok_present = TRUE;				\
  unget_tok = pl_token;				\
} while(0)



#define Update_Last_Read_Position		\
{						\
  pl_last_read_line = pl_token.line;		\
  pl_last_read_col = pl_token.col;		\
}




/*-------------------------------------------------------------------------*
 * PARSE_SUPP_INITIALIZER                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Parse_Supp_Initializer(void)
{
  atom_var = Pl_Create_Atom("var");
  atom_string = Pl_Create_Atom("string");
  atom_punct = Pl_Create_Atom("punct");
  atom_atom = Pl_Create_Atom("atom");
  atom_back_quotes = Pl_Create_Atom("back_quotes");
  atom_full_stop = Pl_Create_Atom("full_stop");
  atom_extend = Pl_Create_Atom("extend");
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
    {
      tok_present = FALSE;
      pl_token = unget_tok;
      if (comma_is_punct && pl_token.type == TOKEN_NAME &&  !pl_token.quoted &&
	  pl_token.name[0] == ',' && pl_token.name[1] == '\0')
	{
	  pl_token.type = TOKEN_PUNCTUATION;
	  pl_token.punct = ',';
	}
      else if (!comma_is_punct && pl_token.type == TOKEN_PUNCTUATION && 
	       pl_token.punct == ',')
	{
	  pl_token.type = TOKEN_NAME;
	  pl_token.quoted = FALSE;
	  pl_token.name[0] = ',';
	  pl_token.name[1] = '\0';
	}
    }
  else if ((err_msg = Pl_Scan_Token(pstm_i, comma_is_punct)) != NULL)
    Parse_Error(err_msg);
}




/*-------------------------------------------------------------------------*
 * PL_READ_TERM                                                            *
 *                                                                         *
 * Returns a Prolog term as a WAM word or NOT_A_WAM_WORD on syntax error.  *
 * parse_end_of_term controls the end of term (see parse_supp.h).          *
 * Uses the value of: FLAG_DOUBLE_QUOTES, FLAG_BACK_QUOTES                 *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Read_Term(StmInf *pstm, int parse_end_of_term)
{
  int jmp_val;
  WamWord term;
  int save_use_le_prompt = pl_use_le_prompt;

  pl_use_le_prompt = 1;

  pl_parse_nb_var = 0;
  pl_last_read_line = -1;

  pstm_i = pstm;

  tok_present = FALSE;
  Save_Machine_Regs(buff_save_machine_regs);

  jmp_val = sigsetjmp(jumper, 1);

  Restore_Machine_Regs(buff_save_machine_regs);

  if (jmp_val == 0)
    {
      term = Parse_Term(MAX_PREC, GENERAL_TERM, COMMA_ANY);

      Read_Next_Token(COMMA_ANY);
      if (term == NOT_A_WAM_WORD)
	{
	  if (pl_token.type == TOKEN_END_OF_FILE)
	    {
	      term = Pl_Put_Atom(pl_atom_end_of_file);
	      goto finish;
	    }
	  else
	    {
	      /* Unget_Token; */ /* useless if followed by Parse_Error */
	      Parse_Error("expression expected");
	    }
	}

      if (parse_end_of_term == PARSE_END_OF_TERM_DOT)
	{
	  if (pl_token.type == TOKEN_FULL_STOP)
	    goto finish;
	  else
	    {
	      /* Unget_Token; */ /* useless if followed by Parse_Error */
	      Parse_Error(". or operator expected after expression");
	    }
	}
      /* parse_end_of_term == PARSE_END_OF_TERM_EOF */
      if (pl_token.type == TOKEN_END_OF_FILE)
	goto finish;
      else
	{
          /* Unget_Token; */ /* useless if followed by Parse_Error */
	  Parse_Error("eof or operator expected after expression");
	}
    }
  else				/* Syntax Error (Restore) */
    {
      Restore_Machine_Regs(buff_save_machine_regs);
      term = NOT_A_WAM_WORD;
    }

 finish:
  pl_use_le_prompt = save_use_le_prompt;
  return term;
}




/*-------------------------------------------------------------------------*
 * PARSE_TERM                                                              *
 *                                                                         *
 * Recursively parses a term whose precedence is <=cur_prec and returns a  *
 * WamWord associated to the term. If a syntax error is encountered the    *
 * parser returns NOT_A_WAM_WORD and update syntax error information       *
 * (see Set_Last_Syntax_Error_Info() in error_supp.c).                     *
 * The flag comma_is_punct specifies if an eventual ',' following the term *
 * must be considered as a punctuation (separator of args of compound term *
 * or of a list) or as an atom. The value COMMA_ANY is used when this flag *
 * is not relevant (only for comprehensivity).                             *
 * Since the Pl_Scan_Token() only consumes necessary characters, the       *
 * function Pl_Stream_Peekc() returns the character immediately after the  *
 * pl_token.                                                               *
 * This feature is used to detect negative numbers and open bracket        *
 * (thus the next call to Pl_Scan_Token() will return TOKEN_IMMEDIAT_OPEN).*
 * We use Pl_Scan_Peek_Char() instead of Pl_Stream_Peekc() only to deal    *
 * with character conversion.                                              *
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
  if (pl_last_read_line == -1)
    Update_Last_Read_Position;

  switch (pl_token.type)
    {
    case TOKEN_VARIABLE:
      i = Lookup_In_Dico_Var(pl_token.name);
      if (++pl_parse_dico_var[i].nb_of_uses == 1)	/* first occurence */
	{
	  term = Pl_Put_X_Variable();
	  pl_parse_dico_var[i].word = term;
	}
      else			/* other occurence */
	term = pl_parse_dico_var[i].word;
      break;

    case TOKEN_INTEGER:
      if (pl_token.int_num > INT_GREATEST_VALUE)
	Parse_Error("integer overflow (exceeds max_integer)");
      if (pl_token.int_num < INT_LOWEST_VALUE)
	Parse_Error("integer underflow (exceeds min_integer)");
      term = Pl_Put_Integer(pl_token.int_num);
      break;

    case TOKEN_FLOAT:
      term = Pl_Put_Float(pl_token.float_num);
      break;

    case TOKEN_STRING:
    case TOKEN_BACK_QUOTED:	/* undefined in ISO */
      flag_value = (pl_token.type == TOKEN_STRING) ?
	Flag_Value(double_quotes) : Flag_Value(back_quotes);

      flag_value &= PF_QUOT_AS_PART_MASK;
      if (flag_value == PF_QUOT_AS_ATOM)
	{
	  atom = Pl_Create_Allocate_Atom(pl_token.name);
	  goto a_name;
	}

      i = strlen(pl_token.name);
      term = NIL_WORD;		/* faster than Pl_Put_Nil() */
      while (i--)
	{
	  term1 = Pl_Put_List();
	  if (flag_value == PF_QUOT_AS_CODES)
	    Pl_Unify_Integer(pl_token.name[i]);
	  else
	    Pl_Unify_Atom(ATOM_CHAR(pl_token.name[i]));

	  Pl_Unify_Value(term);
	  term = term1;
	}
      break;

    case TOKEN_IMMEDIAT_OPEN:
      pl_token.punct = '(';	/* and then like TOKEN_PUNCTUATION */
    case TOKEN_PUNCTUATION:
      if (!strchr("({[", pl_token.punct))
	{
	  term = NOT_A_WAM_WORD;
	  goto finish;
	}

      atom = (pl_token.punct == '{') ? pl_atom_curly_brackets : ATOM_NIL;

      term = Parse_Bracketed_Term();
      if (term == NOT_A_WAM_WORD)	/* name: {} or [] */
	goto a_name;

      break;

    case TOKEN_NAME:
      atom = Pl_Create_Allocate_Atom(pl_token.name);

    a_name:
      bracket = (Pl_Scan_Peek_Char(pstm_i, TRUE) == '(');
      if (bracket)
	{
	  term = Parse_Args_Of_Functor(atom);
	  break;
	}

      /* test if it is a negative number */
      if (pl_token.name[0] == '-' && pl_token.name[1] == '\0'
#ifdef MINUS_SIGN_CANNOT_BE_FOLLOWED_BY_SPACES
	  && isdigit(Pl_Scan_Peek_Char(pstm_i, TRUE))
#endif
	  )
	{
	  int save_line = pl_token.line;
	  int save_col = pl_token.col;

	  Read_Next_Token(COMMA_ANY);
	  if (pl_token.type == TOKEN_INTEGER)
	    {
	      if (pl_token.int_num > -INT_LOWEST_VALUE)
		Parse_Error("integer underflow (exceeds min_integer)");
	      term = Pl_Put_Integer(-pl_token.int_num);
	      break;
	    }
	  
	  if (pl_token.type == TOKEN_FLOAT)
	    {
	      term = Pl_Put_Float(-pl_token.float_num);
	      break;
	    }

	  /* '-' not followed by a number, pushback this token */
	  /* (cannot occur ifdef MINUS_SIGN_CANNOT_BE_FOLLOWED_BY_SPACES) */

	  Unget_Token;

	  /* restore token */
	  pl_token.type = TOKEN_NAME;
	  strcpy(pl_token.name, "-");
	  pl_token.line = save_line;
	  pl_token.col = save_col;
	}


      /* maybe a prefix operator */
      if ((oper = Pl_Lookup_Oper(atom, PREFIX)) && cur_prec >= oper->prec)
	{
	  /* try a prefix operator */
	  cur_left = oper->prec;
	  term = Parse_Term(oper->right, TRYING_PREFIX, comma_is_punct);
	  if (term != NOT_A_WAM_WORD)
	    {
	      term = Create_Structure(atom, 1, &term);
	      break;
	    }
	  /* prefix operator as a name */
	  if (context != GENERAL_TERM)
	    Parse_Error("expression expected or previous operator needs brackets");
	}

      left_is_op = (Check_Oper_Any_Type(atom));

      if (left_is_op && context != GENERAL_TERM)   /* in operator context */
	{
	  if (Check_Oper(atom, INFIX) || Check_Oper(atom, POSTFIX))
	    Parse_Error("current or previous operator needs brackets");
	  else
	    Parse_Error("current operator needs brackets");
	}

      term = Pl_Put_Atom(atom);
      break;

    default:	  /* TOKEN_END_OF_FILE, TOKEN_FULL_STOP, TOKEN_EXTENDED */
      term = NOT_A_WAM_WORD;
      goto finish;
    }


  for (;;)
    {
      Read_Next_Token(comma_is_punct);

#if 1 /* to allow | to be unquoted if it is an infix operator with prec > 1000 */
      if (pl_token.type == TOKEN_PUNCTUATION && pl_token.punct == '|' &&
	  (oper = Pl_Lookup_Oper(atom = ATOM_CHAR('|'), INFIX)) && oper->prec > 1000 && cur_prec >= oper->prec)
	infix_op = TRUE;
      else
#endif
	{
	  if (pl_token.type != TOKEN_NAME)
	    break;

	  atom = Pl_Create_Allocate_Atom(pl_token.name);
	  if ((oper = Pl_Lookup_Oper(atom, INFIX)))
	    infix_op = TRUE;
	  else if ((oper = Pl_Lookup_Oper(atom, POSTFIX)))
	    infix_op = FALSE;
	  else
	    break;
	}

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

	  if (atom == ATOM_CHAR('.'))
	    {
	      term = Pl_Put_List();
	      Pl_Unify_Value(w[0]);
	      Pl_Unify_Value(w[1]);
	    }
 	  else
	    term = Create_Structure(atom, 2, w);
	}
      else
	term = Create_Structure(atom, 1, &term); /* postfix operator */

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
  while (pl_token.type == TOKEN_PUNCTUATION && pl_token.punct == ',');

  if (pl_token.type != TOKEN_PUNCTUATION || pl_token.punct != ')')
    Parse_Error(", or ) expected");

  return Create_Structure(atom, i, w);
}




/*-------------------------------------------------------------------------*
 * PARSE_BRACKETED_TERM                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Parse_Bracketed_Term(void)
{
  WamWord term;


  switch (pl_token.punct)
    {
    case '(':
      term = Parse_Term(MAX_PREC, GENERAL_TERM, COMMA_ANY);
      if (term == NOT_A_WAM_WORD)
	Parse_Error("expression expected");

      Read_Next_Token(COMMA_ANY);
      if (pl_token.type != TOKEN_PUNCTUATION || pl_token.punct != ')')
	Parse_Error(") or operator expected");
      break;

    case '{':
      term = Parse_Term(MAX_PREC, GENERAL_TERM, COMMA_ANY);

      Read_Next_Token(COMMA_ANY);
      if (pl_token.type != TOKEN_PUNCTUATION || pl_token.punct != '}')
	Parse_Error("} or operator expected");

      if (term != NOT_A_WAM_WORD)	/* term == NOT_A_WAM_WORD if {} */
	term = Create_Structure(pl_atom_curly_brackets, 1, &term);
      break;

    case '[':
      term = Parse_List(TRUE);	/* term == NOT_A_WAM_WORD if [] */
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
      else if (pl_token.type != TOKEN_PUNCTUATION || pl_token.punct != ']')
	Parse_Error("expression or ] expected in list");

      return NOT_A_WAM_WORD;
    }


  if (pl_token.type != TOKEN_PUNCTUATION || !strchr(",|]", pl_token.punct))
    Parse_Error(", | ] or operator expected in list");

  switch (pl_token.punct)
    {
    case ',':			/* [X,Y...] */
      cdr_word = Parse_List(FALSE);
      break;

    case '|':			/* [X|Y] */
      cdr_word = Parse_Term(MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM, COMMA_ANY);
      if (cdr_word == NOT_A_WAM_WORD)
	Parse_Error("expression expected in list");

      Read_Next_Token(TRUE);
      if (pl_token.type != TOKEN_PUNCTUATION || pl_token.punct != ']')
	Parse_Error("] or operator expected in list");
      break;

    case ']':			/* [X] */
      cdr_word = NIL_WORD;	/* faster than Pl_Put_Nil() */
      break;
    }

  term = Pl_Put_List();
  Pl_Unify_Value(car_word);
  Pl_Unify_Value(cdr_word);

  return term;
}




/*-------------------------------------------------------------------------*
 * CREATE_STRUCTURE                                                        *
 *                                                                         *
 * like Mk_Compound but simplified since we know arity != 0 and arg != NULL*
 *-------------------------------------------------------------------------*/
static WamWord
Create_Structure(int func, int arity, WamWord *arg)
{
  WamWord res_word;
  int i;

  if (arity == 2 && func == ATOM_CHAR('.'))
    {
      res_word = Pl_Put_List();
      Pl_Unify_Value(arg[0]);
      Pl_Unify_Value(arg[1]);
    }
  else
    {
      res_word = Pl_Put_Structure(func, arity);
      for (i = 0; i < arity; i++)
	Pl_Unify_Value(arg[i]);
    }

  return res_word;
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
      for (i = 0; i < pl_parse_nb_var && strcmp(name, pl_parse_dico_var[i].name) != 0; i++)
	;
    }
  else
    {
      named = FALSE;
      i = pl_parse_nb_var;
    }

  if (i == pl_parse_nb_var)
    {
      if (pl_parse_nb_var >= MAX_VAR_IN_TERM)
	Parse_Error("too many variables in a term");

      strcpy(pl_parse_dico_var[pl_parse_nb_var].name, name);
      pl_parse_dico_var[pl_parse_nb_var].word = NOT_A_WAM_WORD;
      pl_parse_dico_var[pl_parse_nb_var].named = named;
      pl_parse_dico_var[pl_parse_nb_var].nb_of_uses = 0;
      pl_parse_nb_var++;
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
  Pl_Set_Last_Syntax_Error(pl_atom_tbl[pstm_i->atom_file_name].name,
			   pl_token.line, pl_token.col, err_msg);

  if (pl_token.type != TOKEN_FULL_STOP)
    Pl_Recover_After_Error(pstm_i);

  Save_Machine_Regs(buff_save_machine_regs);
  siglongjmp(jumper, 1);
}




	  /* Other facilities */


/*-------------------------------------------------------------------------*
 * PL_READ_ATOM                                                            *
 *                                                                         *
 * Returns a Prolog atom as a WAM word or NOT_A_WAM_WORD on syntax error.  *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Read_Atom(StmInf *pstm)
{
  char *err_msg;

  if ((err_msg = Pl_Scan_Next_Atom(pstm)) != NULL)
    {
      Pl_Set_Last_Syntax_Error(pl_atom_tbl[pstm->atom_file_name].name,
			    pl_token.line, pl_token.col, err_msg);
      return NOT_A_WAM_WORD;
    }

  Update_Last_Read_Position;

  return Pl_Put_Atom(Pl_Create_Allocate_Atom(pl_token.name));
}




/*-------------------------------------------------------------------------*
 * PL_READ_INTEGER                                                         *
 *                                                                         *
 * Returns a Prolog integer as a WAM word or NOT_A_WAM_WORD on syntax error*
 *-------------------------------------------------------------------------*/
WamWord
Pl_Read_Integer(StmInf *pstm)
{
  char *err_msg;

  if ((err_msg = Pl_Scan_Next_Number(pstm, TRUE)) != NULL)
    {
      Pl_Set_Last_Syntax_Error(pl_atom_tbl[pstm->atom_file_name].name,
			    pl_token.line, pl_token.col, err_msg);
      return NOT_A_WAM_WORD;
    }

  Update_Last_Read_Position;

  return Pl_Put_Integer(pl_token.int_num);
}





/*-------------------------------------------------------------------------*
 * PL_READ_NUMBER                                                          *
 *                                                                         *
 * Returns a Prolog number as a WAM word or NOT_A_WAM_WORD on syntax error.*
 *-------------------------------------------------------------------------*/
WamWord
Pl_Read_Number(StmInf *pstm)
{
  char *err_msg;

  if ((err_msg = Pl_Scan_Next_Number(pstm, FALSE)) != NULL)
    {
      Pl_Set_Last_Syntax_Error(pl_atom_tbl[pstm->atom_file_name].name,
			    pl_token.line, pl_token.col, err_msg);
      return NOT_A_WAM_WORD;
    }

  Update_Last_Read_Position;

  return (pl_token.type == TOKEN_INTEGER) ? Pl_Put_Integer(pl_token.int_num)
    : Pl_Put_Float(pl_token.float_num);
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN                                                           *
 *                                                                         *
 * Returns a Prolog token as a WAM word or NOT_A_WAM_WORD on syntax error. *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Read_Token(StmInf *pstm)
{
  WamWord term, arg;
  int func, atom;
  char *err_msg;

  if ((err_msg = Pl_Scan_Token(pstm, FALSE)) != NULL)
    {
      Pl_Set_Last_Syntax_Error(pl_atom_tbl[pstm->atom_file_name].name,
			       pl_token.line, pl_token.col, err_msg);
      return NOT_A_WAM_WORD;
    }

  Update_Last_Read_Position;

  term = NOT_A_WAM_WORD;

  switch (pl_token.type)
    {
    case TOKEN_VARIABLE:
      func = atom_var;
    arg_of_struct:
      atom = Pl_Create_Allocate_Atom(pl_token.name);
      arg = Pl_Put_Atom(atom);
      break;

    case TOKEN_INTEGER:
      term = Pl_Put_Integer(pl_token.int_num);
      break;

    case TOKEN_FLOAT:
      term = Pl_Put_Float(pl_token.float_num);
      break;

    case TOKEN_STRING:
      func = atom_string;
      goto arg_of_struct;

    case TOKEN_IMMEDIAT_OPEN:
      pl_token.punct = '(';	/* and then like TOKEN_PUNCTUATION */
    case TOKEN_PUNCTUATION:
      func = atom_punct;
      atom = ATOM_CHAR(pl_token.punct);
      arg = Pl_Put_Atom(atom);
      break;

    case TOKEN_NAME:
      atom = Pl_Create_Allocate_Atom(pl_token.name);
      term = Pl_Put_Atom(atom);
      break;

    case TOKEN_BACK_QUOTED:	/* undefined in ISO */
      func = atom_back_quotes;
      goto arg_of_struct;

    case TOKEN_FULL_STOP:
      func = atom_punct;
      arg = Pl_Put_Atom(atom_full_stop);
      break;

    case TOKEN_END_OF_FILE:
      func = atom_punct;
      arg = Pl_Put_Atom(pl_atom_end_of_file);
      break;

    case TOKEN_EXTENDED:
      func = atom_extend;
      goto arg_of_struct;
    }

  if (term == NOT_A_WAM_WORD)
    {
      term = Pl_Put_Structure(func, 1);
      Pl_Unify_Value(arg);
    }

  return term;
}
