/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : write_supp.c                                                    *
 * Descr.: write term support                                              *
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

#include <string.h>
#include <ctype.h>

#define OBJ_INIT Write_Supp_Initializer

#define WRITE_SUPP_FILE

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define W_NOTHING                  0	/* for last_writing */
#define W_NUMBER                   1
#define W_IDENTIFIER               2
#define W_QUOTED                   3
#define W_GRAPHIC                  4




#define W_NO_PREFIX_OP             0	/* for last_prefix_op */
#define W_PREFIX_OP_ANY            1
#define W_PREFIX_OP_MINUS          2




#define GENERAL_TERM               0
#define INSIDE_LEFT_ASSOC_OP       2
#define INSIDE_ANY_OP              1




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static WamWord curly_brackets_1;
static WamWord dollar_var_1;
static WamWord dollar_varname_1;

static int atom_dots;

static CodePtr try_portray_code = NULL;



static StmInf *pstm_o;
static Bool quoted;
static Bool ignore_op;
static Bool number_vars;
static Bool name_vars;
static Bool space_args;
static Bool portrayed;

static int last_prefix_op = W_NO_PREFIX_OP;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Show_Term(int depth, int prec, int context, WamWord term_word);

static void Show_Global_Var(WamWord *adr);

static void Show_Atom(int context, int atom);

static void Show_Integer(long x);

static void Show_Fd_Variable(WamWord *fdv_adr);

static void Show_Float(double x);

static void Show_List_Arg(int depth, WamWord *lst_adr);

static
  void Show_Structure(int depth, int prec, int context, WamWord *stc_adr);

static void Out_String(char *str);

static void Out_Char(int c);

static void Need_Space(int c);

static Bool Try_Portray(WamWord word);




/*-------------------------------------------------------------------------*
 * WRITE_SUPP_INITIALIZER                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Write_Supp_Initializer(void)
{
  atom_dots = Create_Atom("...");

  curly_brackets_1 = Functor_Arity(atom_curly_brackets, 1);
  dollar_var_1 = Functor_Arity(Create_Atom("$VAR"), 1);
  dollar_varname_1 = Functor_Arity(Create_Atom("$VARNAME"), 1);
}




/*-------------------------------------------------------------------------*
 * WRITE_TERM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_Term(StmInf *pstm, int depth, int prec, int mask, WamWord term_word)
{
  pstm_o = pstm;

  quoted = mask & WRITE_QUOTED;
  ignore_op = mask & WRITE_IGNORE_OP;
  number_vars = mask & WRITE_NUMBER_VARS;
  name_vars = mask & WRITE_NAME_VARS;
  space_args = mask & WRITE_SPACE_ARGS;
  portrayed = mask & WRITE_PORTRAYED;

  last_writing = W_NOTHING;

  Show_Term(depth, prec, GENERAL_TERM, term_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_SIMPLE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_Simple(WamWord term_word)
{
  StmInf *pstm = stm_tbl + stm_output;

  Write_Term(pstm, -1, MAX_PREC, WRITE_NUMBER_VARS | WRITE_NAME_VARS,
	     term_word);
  /* like write/1 */
}




/*-------------------------------------------------------------------------*
 * WRITE_A_CHAR                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_A_Char(StmInf *pstm, int c)
{
  pstm_o = pstm;
  Out_Char(c);
}




/*-------------------------------------------------------------------------*
 * SHOW_TERM                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Term(int depth, int prec, int context, WamWord term_word)
{
  WamWord word, tag, *adr;

  if (depth == 0)
    {
      Show_Atom(GENERAL_TERM, atom_dots);
      return;
    }

  Deref(term_word, word, tag, adr);
  if (tag != REF && Try_Portray(word))
    return;

  switch (tag)
    {
    case REF:
      if (Is_A_Local_Adr(adr))
	{
	  Show_Global_Var(H);	/* Write before, because H */
	  Globalize_Local_Unbound_Var(adr);
	}
      else
	Show_Global_Var(adr);
      break;

    case ATM:
      Show_Atom(context, UnTag_ATM(word));
      break;

    case FDV:
      Show_Fd_Variable(UnTag_FDV(word));
      break;

    case INT:
      Show_Integer(UnTag_INT(word));
      break;

    case FLT:
      Show_Float(Obtain_Float(UnTag_FLT(word)));
      break;

    case LST:
      adr = UnTag_LST(word);
      if (ignore_op)
	{
	  Out_String("'.'(");
	  Show_Term(depth - 1, MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM,
		    Car(adr));
	  Out_Char(',');
	  Show_Term(depth - 1, MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM,
		    Cdr(adr));
	  Out_Char(')');
	}
      else
	{
	  Out_Char('[');
	  Show_List_Arg(depth, adr);
	  Out_Char(']');
	}
      break;

    case STC:
      adr = UnTag_STC(word);
      Show_Structure(depth, prec, context, adr);
      break;
    }
}




/*-------------------------------------------------------------------------*
 * SHOW_GLOBAL_VAR                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Global_Var(WamWord *adr)
{
  char str[32];

  sprintf(str, "_%d", (int) Global_Offset(adr));
  Out_String(str);

  last_writing = W_IDENTIFIER;
}




/*-------------------------------------------------------------------------*
 * SHOW_ATOM                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Atom(int context, int atom)
{
  char *p, *q;
  char str[32];
  Bool bracket = FALSE;
  int c, c_type;
  AtomProp prop;


  prop = atom_tbl[atom].prop;

  if (context != GENERAL_TERM && Check_Oper_Any_Type(atom))
    {
      Out_Char('(');
      bracket = TRUE;
    }


  if (!quoted || !prop.needs_quote)
    {
      Out_String(atom_tbl[atom].name);

      switch (prop.type)
	{
	case IDENTIFIER_ATOM:
	  last_writing = W_IDENTIFIER;
	  break;

	case GRAPHIC_ATOM:
	  last_writing = W_GRAPHIC;
	  break;

	case SOLO_ATOM:
	  last_writing = W_NOTHING;
	  break;

	case OTHER_ATOM:
	  c = atom_tbl[atom].name[prop.length - 1];
	  c_type = char_type[c];
	  if (c_type & (UL | CL | SL | DI))
	    last_writing = W_IDENTIFIER;
	  else if (c == '\'')
	    last_writing = W_QUOTED;
	  else if (c_type == GR)
	    last_writing = W_GRAPHIC;
	  else
	    last_writing = W_NOTHING;
	}
    }
  else
    {
      Out_Char('\'');

      if (prop.needs_scan)
	{
	  for (p = atom_tbl[atom].name; *p; p++)
	    if ((q = (char *) strchr(escape_char, *p)))
	      {
		Out_Char('\\');
		Out_Char(escape_symbol[q - escape_char]);
	      }
	    else if (*p == '\'' || *p == '\\')	/* display twice */
	      {
		Out_Char(*p);
		Out_Char(*p);
	      }
	    else if (!isprint(*p))
	      {
		sprintf(str, "\\x%x\\", (unsigned) *p);
		Out_String(str);
	      }
	    else
	      Out_Char(*p);
	}
      else
	Out_String(atom_tbl[atom].name);

      Out_Char('\'');

      last_writing = W_QUOTED;
    }

  if (bracket)
    Out_Char(')');
}




/*-------------------------------------------------------------------------*
 * SHOW_INTEGER                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Integer(long x)
{
  char str[32];

  sprintf(str, "%ld", x);
  Out_String(str);

  last_writing = W_NUMBER;
}




/*-------------------------------------------------------------------------*
 * SHOW_FD_VARIABLE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Fd_Variable(WamWord *fdv_adr)
{
  char str[32];

  sprintf(str, "_#%d(", (int) Cstr_Offset(fdv_adr));
  Out_String(str);

  Out_String(Fd_Variable_To_String(fdv_adr));
  Out_Char(')');

  last_writing = W_IDENTIFIER;
}




/*-------------------------------------------------------------------------*
 * SHOW_FLOAT                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Float(double x)
{
  Out_String(Float_To_String(x));

  last_writing = W_NUMBER;
}




/*-------------------------------------------------------------------------*
 * SHOW_LIST_ARG                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_List_Arg(int depth, WamWord *lst_adr)
{
  WamWord word, tag, *adr;
  int atom;

  depth--;

  Show_Term(depth, MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM, Car(lst_adr));

  if (depth == 0)		/* dots already written by Show_Term */
    return;


  Deref(Cdr(lst_adr), word, tag, adr);

  switch (tag)
    {
    case REF:
      Out_Char('|');
      Show_Global_Var(adr);
      break;

    case ATM:
      atom = UnTag_ATM(word);
      if (atom != ATOM_NIL)
	{
	  Out_Char('|');
	  if (Try_Portray(word))
	    return;

	  Show_Atom(GENERAL_TERM, atom);
	}
      break;

    case FDV:
      Out_Char('|');
      if (Try_Portray(word))
	return;

      Show_Fd_Variable(UnTag_FDV(word));
      break;

    case INT:
      Out_Char('|');
      if (Try_Portray(word))
	return;

      Show_Integer(UnTag_INT(word));
      break;

    case FLT:
      Out_Char('|');
      if (Try_Portray(word))
	return;

      Show_Float(Obtain_Float(UnTag_FLT(word)));
      break;

    case LST:
      adr = UnTag_LST(word);
      Out_Char(',');
      if (space_args)
	Out_Char(' ');
      Show_List_Arg(depth, adr);
      break;

    case STC:
      adr = UnTag_STC(word);
      Out_Char('|');
      if (Try_Portray(word))
	return;

      Show_Structure(depth, MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM, adr);
      break;
    }
}




/*-------------------------------------------------------------------------*
 * SHOW_STRUCTURE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Structure(int depth, int prec, int context, WamWord *stc_adr)
{
  WamWord word, tag, *adr;
  WamWord f_n = Functor_And_Arity(stc_adr);
  int functor = Functor(stc_adr);
  int arity = Arity(stc_adr);
  OperInf *oper;
  int nb_args_to_disp;
  int i, j, n;
  char str[32];
  Bool bracket;
  Bool surround_space;


  depth--;

  if (name_vars && f_n == dollar_varname_1)
    {
      Deref(Arg(stc_adr, 0), word, tag, adr);
      if (tag == ATM)
	{
	  Out_String(atom_tbl[UnTag_ATM(word)].name);
	  last_writing = W_IDENTIFIER;
	  return;
	}
    }

  if (number_vars && f_n == dollar_var_1)
    {
      Deref(Arg(stc_adr, 0), word, tag, adr);
      if (tag == INT && (n = UnTag_INT(word)) >= 0)
	{
	  i = n % 26;
	  j = n / 26;

	  Out_Char('A' + i);

	  if (j)
	    {
	      sprintf(str, "%d", j);
	      Out_String(str);
	    }

	  last_writing = W_IDENTIFIER;
	  return;
	}
    }

  if (ignore_op || arity > 2)
    goto functional;

  if (f_n == curly_brackets_1)
    {
      Out_Char('{');
      Show_Term(depth, MAX_PREC, GENERAL_TERM, Arg(stc_adr, 0));
      Out_Char('}');
      return;
    }

  bracket = FALSE;

  if (arity == 1 && (oper = Lookup_Oper(functor, PREFIX)))
    {
#if 1
      /* Koen de Bosschere says "in case of ambiguity :          */
      /* select the associative operator over the nonassociative */
      /* select prefix over postfix".                            */

      OperInf *oper1;

      if (oper->prec > oper->right
	  && (oper1 = Lookup_Oper(functor, POSTFIX))
	  && oper1->left == oper1->prec)
	{
	  oper = oper1;
	  goto postfix;
	}
#endif
      if (oper->prec > prec || (context == INSIDE_LEFT_ASSOC_OP &&
				(oper->prec == oper->right
				 && oper->prec == prec)))
	{			/* prevent also the case: fy T yf(x) */
	  Out_Char('(');
	  bracket = TRUE;
	}


      Show_Atom(GENERAL_TERM, functor);

      last_prefix_op = (strcmp(atom_tbl[functor].name, "-") == 0)
	? W_PREFIX_OP_MINUS : W_PREFIX_OP_ANY;
      Show_Term(depth, oper->right, INSIDE_ANY_OP, Arg(stc_adr, 0));
      last_prefix_op = W_NO_PREFIX_OP;

      if (bracket)
	Out_Char(')');

      return;
    }


  if (arity == 1 && (oper = Lookup_Oper(functor, POSTFIX)))
    {
    postfix:
      if (oper->prec > prec)
	{
	  Out_Char('(');
	  bracket = TRUE;
	}

      context =
	(oper->left == oper->prec) ? INSIDE_LEFT_ASSOC_OP : INSIDE_ANY_OP;

      Show_Term(depth, oper->left, context, Arg(stc_adr, 0));
      Show_Atom(GENERAL_TERM, functor);

      if (bracket)
	Out_Char(')');

      return;
    }


  if (arity == 2 && (oper = Lookup_Oper(functor, INFIX)))
    {
      if (oper->prec > prec || (context == INSIDE_LEFT_ASSOC_OP &&
				(oper->prec == oper->right
				 && oper->prec == prec)))
	{			/* prevent also the case: T xfy U yf(x) */
	  Out_Char('(');
	  bracket = TRUE;
	}

      context =
	(oper->left == oper->prec) ? INSIDE_LEFT_ASSOC_OP : INSIDE_ANY_OP;

      Show_Term(depth, oper->left, context, Arg(stc_adr, 0));

      if (functor == ATOM_CHAR(',') && !ignore_op)
	Out_Char(',');
      else
	{
	  surround_space = FALSE;

	  if (atom_tbl[functor].prop.type == IDENTIFIER_ATOM ||
	      atom_tbl[functor].prop.type == OTHER_ATOM)
	    {
	      surround_space = TRUE;
	      Out_Char(' ');
	    }

	  Show_Atom(GENERAL_TERM, functor);

	  if (surround_space)
	    Out_Char(' ');
	}

      Show_Term(depth, oper->right, INSIDE_ANY_OP, Arg(stc_adr, 1));

      if (bracket)
	Out_Char(')');

      return;
    }



functional:			/* functional notation */

  Show_Atom(GENERAL_TERM, functor);
  Out_Char('(');

  nb_args_to_disp = i = (arity < depth + 1
			 || depth < 0) ? arity : depth + 1;
  adr = &Arg(stc_adr, 0);

  goto start_display;

  do
    {
      Out_Char(',');
      if (space_args)
	Out_Char(' ');
    start_display:
      Show_Term(depth, MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM, *adr++);
    }
  while (--i);

  if (arity != nb_args_to_disp)
    {
      Out_Char(',');
      if (space_args)
	Out_Char(' ');
      Show_Atom(GENERAL_TERM, atom_dots);
    }

  Out_Char(')');
}




/*-------------------------------------------------------------------------*
 * OUT_CHAR                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Out_Char(int c)
{
  Need_Space(c);
  Stream_Putc(c, pstm_o);
}




/*-------------------------------------------------------------------------*
 * OUT_STRING                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Out_String(char *str)
{
  Need_Space(*str);
  Stream_Puts(str, pstm_o);
}




/*-------------------------------------------------------------------------*
 * NEED_SPACE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Need_Space(int c)
{
  int c_type = char_type[c];
  int space;



  switch (last_writing)
    {
    case W_NUMBER:
      space = (c_type & (UL | CL | SL | DI)) || c == '.';
      break;

    case W_IDENTIFIER:
      space = (c_type & (UL | CL | SL | DI)) || c == '[' || c == '{';
      break;

    case W_QUOTED:
      space = (c_type == QT);
      break;

    case W_GRAPHIC:
      space = (c_type == GR);
      break;

    default:
      space = FALSE;
    }

  if (space || (c == '(' && last_prefix_op) ||
      (c_type == DI && last_prefix_op == W_PREFIX_OP_MINUS))
    Stream_Putc(' ', pstm_o);

  last_prefix_op = W_NO_PREFIX_OP;
  last_writing = W_NOTHING;
}




/*-------------------------------------------------------------------------*
 * TRY_PORTRAY                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Try_Portray(WamWord word)
{
  PredInf *pred;
  StmInf *print_pstm_o;
  Bool print_quoted;
  Bool print_ignore_op;
  Bool print_number_vars;
  Bool print_name_vars;
  Bool print_space_args;
  Bool print_portrayed;
  Bool print_ok;


  if (!portrayed)
    return FALSE;

  if (try_portray_code == NULL)
    {
      pred = Lookup_Pred(Create_Atom("$try_portray"), 1);
      if (pred == NULL || pred->codep == NULL)
	Pl_Err_Resource(resource_print_object_not_linked);

      try_portray_code = (CodePtr) (pred->codep);
    }

  SYS_VAR_PRINT_STM = pstm_o - stm_tbl;	/* for get_print_stream/1 */

  print_pstm_o = pstm_o;
  print_quoted = quoted;
  print_ignore_op = ignore_op;
  print_number_vars = number_vars;
  print_name_vars = name_vars;
  print_space_args = space_args;
  print_portrayed = portrayed;

  A(0) = word;
  print_ok = Call_Prolog(try_portray_code);

  pstm_o = print_pstm_o;
  quoted = print_quoted;
  ignore_op = print_ignore_op;
  number_vars = print_number_vars;
  name_vars = print_name_vars;
  space_args = print_space_args;
  portrayed = print_portrayed;

  return print_ok;
}
