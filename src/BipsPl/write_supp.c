/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : write_supp.c                                                    *
 * Descr.: write term support                                              *
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


#include <string.h>
#include <ctype.h>

#define OBJ_INIT Write_Supp_Initializer

#define WRITE_SUPP_FILE

#include "engine_pl.h"
#include "bips_pl.h"


				/* spaces for non-assoc op (fx, xfx, xf) */
#if 0
#define SPACE_ARGS_RESTRICTED
#endif
				/* spaces around the | inside lists */
#if 0
#define SPACE_ARGS_FOR_LIST_PIPE
#endif



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define W_NOTHING                  0	/* for pl_last_writing */
#define W_NUMBER                   1
#define W_NUMBER_0                 2    /* to avoid 0'f ' if 'f ' is an op (avoid 0'char) */
#define W_IDENTIFIER               3
#define W_QUOTED                   4
#define W_GRAPHIC                  5




#define W_NO_PREFIX_OP             0	/* for last_prefix_op */
#define W_PREFIX_OP_ANY            1
#define W_PREFIX_OP_MINUS          2




#define GENERAL_TERM               0
#define INSIDE_ANY_OP              1
#define INSIDE_LEFT_ASSOC_OP       2




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

static StmInf *pstm_o;
static Bool quoted;
static Bool ignore_op;
static Bool number_vars;
static Bool name_vars;
static Bool space_args;
static Bool portrayed;

static WamWord *name_number_above_H;

static int last_prefix_op = W_NO_PREFIX_OP;
static Bool *p_bracket_minus;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Need_Space(int c);

static void Out_Space(void);

static void Out_Char(int c);

static void Out_String(char *str);

static void Show_Term(int depth, int prec, int context, WamWord term_word);

static void Show_Global_Var(WamWord *adr);

static void Show_Atom(int context, int atom);

static void Show_Integer(PlLong x);

#ifndef NO_USE_FD_SOLVER
static void Show_Fd_Variable(WamWord *fdv_adr);
#endif

static void Show_Float(double x);

static void Show_List_Arg(int depth, WamWord *lst_adr);

static void Show_Structure(int depth, int prec, int context,
			   WamWord *stc_adr);

static Bool Try_Portray(WamWord word);




/*-------------------------------------------------------------------------*
 * WRITE_SUPP_INITIALIZER                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Write_Supp_Initializer(void)
{
  atom_dots = Pl_Create_Atom("...");

  curly_brackets_1 = Functor_Arity(pl_atom_curly_brackets, 1);
  dollar_var_1 = Functor_Arity(Pl_Create_Atom("$VAR"), 1);
  dollar_varname_1 = Functor_Arity(Pl_Create_Atom("$VARNAME"), 1);
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_TERM                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Write_Term(StmInf *pstm, int depth, int prec, int mask, WamWord *above_H, 
	      WamWord term_word)
{
  pstm_o = pstm;

  quoted = mask & WRITE_QUOTED;
  ignore_op = mask & WRITE_IGNORE_OP;
  number_vars = mask & WRITE_NUMBER_VARS;
  name_vars = mask & WRITE_NAME_VARS;
  space_args = mask & WRITE_SPACE_ARGS;
  portrayed = mask & WRITE_PORTRAYED;

  name_number_above_H = above_H;

  pl_last_writing = W_NOTHING;

  Show_Term(depth, prec, (prec >= 1200) ? GENERAL_TERM : INSIDE_ANY_OP, term_word);
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_SIMPLE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Write(WamWord term_word)
{
  StmInf *pstm = pl_stm_tbl[pl_stm_output];

  Pl_Write_Term(pstm, -1, MAX_PREC, WRITE_NUMBER_VARS | WRITE_NAME_VARS, NULL,
		term_word);
  /* like write/1 */
}




/*-------------------------------------------------------------------------*
 * OUT_SPACE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Out_Space(void)
{
  Pl_Stream_Putc(' ', pstm_o);
  pl_last_writing = W_NOTHING;
}




/*-------------------------------------------------------------------------*
 * OUT_CHAR                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Out_Char(int c)
{
  Need_Space(c);
  Pl_Stream_Putc(c, pstm_o);
}




/*-------------------------------------------------------------------------*
 * OUT_STRING                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Out_String(char *str)
{
  Need_Space(*str);
  Pl_Stream_Puts(str, pstm_o);
}




/*-------------------------------------------------------------------------*
 * NEED_SPACE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Need_Space(int c)
{
  int c_type = pl_char_type[c];
  int space;

  switch (pl_last_writing)
    {
    case W_NUMBER_0:
      if (c_type == QT)
	{
	  space = TRUE;
	  break;
	} /* then in W_NUMBER */
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

  if (space || (c == '(' && last_prefix_op != W_NO_PREFIX_OP))
    Pl_Stream_Putc(' ', pstm_o);
  else if (c_type == DI && last_prefix_op == W_PREFIX_OP_MINUS)
    {
#ifdef MINUS_SIGN_CANNOT_BE_FOLLOWED_BY_SPACES
      Pl_Stream_Putc(' ', pstm_o);
#else
      (*p_bracket_minus)++;
      /* to show it is an operator notation we also display a space (not strictly necessary) */
#if 1
      Pl_Stream_Putc(' ', pstm_o); 
#endif
      Pl_Stream_Putc('(', pstm_o);
#endif
    }

  last_prefix_op = W_NO_PREFIX_OP;
  pl_last_writing = W_NOTHING;
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_A_FULL_STOP                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Write_A_Full_Stop(StmInf *pstm)
{
  pstm_o = pstm;
  if (pl_last_writing == W_NUMBER_0 || pl_last_writing == W_NUMBER)
    pl_last_writing = W_NOTHING;

  Out_Char('.');
  Out_Char('\n');
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_A_CHAR                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Write_A_Char(StmInf *pstm, int c)
{
  pstm_o = pstm;
  Out_Char(c);
}




/*-------------------------------------------------------------------------*
 * PL_FLOAT_TO_STRING                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Float_To_String(double d)
{
  char *p, *q, *e;
  static char buff[32];

  sprintf(buff, "%#.17g", d);	/* a . with 16 significant digits */

  p = buff;			/* skip leading blanks */
  while (*p == ' ')
    p++;

  if (p != buff)		/* remove leading blanks */
    {
      q = buff;
      while ((*q++ = *p++))
	;
    }

  p = strchr(buff, '.');
  if (p == NULL)		/* if p==NULL then NaN or +/-inf (ignore) */
    return buff;

  if (p[1] == '\0')		/* a dot but no decimal numbers */
    {
      strcat(buff, "0");
      return buff;
    }

  e = strchr(buff, 'e');	/* search exposant part */
  if (e == NULL)
    e = buff + strlen(buff);
  p = e - 1;
  while (*p == '0')
    p--;

  q = (*p == '.') ? p + 2 : p + 1;	/* but keep at least one 0 */

  if (q != e)
    while ((*q++ = *e++))	/* move exposant part */
      ;

  return buff;
}




/*-------------------------------------------------------------------------*
 * SHOW_TERM                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Term(int depth, int prec, int context, WamWord term_word)
{
  WamWord word, tag_mask;
  WamWord *adr;

  if (depth == 0)
    {
      Show_Atom(GENERAL_TERM, atom_dots);
      return;
    }

  DEREF(term_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK && Try_Portray(word))
    return;

  switch (Tag_From_Tag_Mask(tag_mask))
    {
    case REF:
      adr = UnTag_REF(word);
      if (Is_A_Local_Adr(adr))
	{
	  Globalize_Local_Unbound_Var(adr, word);
	  adr = UnTag_REF(word);
	}
      Show_Global_Var(adr);
      break;

    case ATM:
      Show_Atom(context, UnTag_ATM(word));
      break;

#ifndef NO_USE_FD_SOLVER
    case FDV:
      Show_Fd_Variable(UnTag_FDV(word));
      break;
#endif

    case INT:
      Show_Integer(UnTag_INT(word));
      break;

    case FLT:
      Show_Float(Pl_Obtain_Float(UnTag_FLT(word)));
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

  pl_last_writing = W_IDENTIFIER;
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


  prop = pl_atom_tbl[atom].prop;

  if (context != GENERAL_TERM && Check_Oper_Any_Type(atom))
    {
      Out_Char('(');
      bracket = TRUE;
    }


  if (!quoted || !prop.needs_quote)
    {
      Out_String(pl_atom_tbl[atom].name);

      switch (prop.type)
	{
	case IDENTIFIER_ATOM:
	  pl_last_writing = W_IDENTIFIER;
	  break;

	case GRAPHIC_ATOM:
	  pl_last_writing = W_GRAPHIC;
	  break;

	case SOLO_ATOM:
	  pl_last_writing = W_NOTHING;
	  break;

	case OTHER_ATOM:
	  if (prop.length == 0)
            {
              pl_last_writing = W_NOTHING;
              break;
            }
	  c = pl_atom_tbl[atom].name[prop.length - 1];
	  c_type = pl_char_type[c];
	  if (c_type & (UL | CL | SL | DI))
	    pl_last_writing = W_IDENTIFIER;
	  else if (c == '\'')
	    pl_last_writing = W_QUOTED;
	  else if (c_type == GR)
	    pl_last_writing = W_GRAPHIC;
	  else
	    pl_last_writing = W_NOTHING;
	}
    }
  else
    {
      Out_Char('\'');

      if (prop.needs_scan)
	{
	  for (p = pl_atom_tbl[atom].name; *p; p++)
	    if ((q = (char *) strchr(pl_escape_char, *p)))
	      {
		Out_Char('\\');
		Out_Char(pl_escape_symbol[q - pl_escape_char]);
	      }
	    else if (*p == '\'' || *p == '\\')	/* display twice */
	      {
		Out_Char(*p);
		Out_Char(*p);
	      }
	    else if (!isprint(*p))
	      {
		sprintf(str, "\\x%x\\", (unsigned) (unsigned char) *p);
		Out_String(str);
	      }
	    else
	      Out_Char(*p);
	}
      else
	Out_String(pl_atom_tbl[atom].name);

      Out_Char('\'');

      pl_last_writing = W_QUOTED;
    }

  if (bracket)
    Out_Char(')');
}




/*-------------------------------------------------------------------------*
 * SHOW_INTEGER                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Integer(PlLong x)
{
  char str[32];

  sprintf(str, "%" PL_FMT_d, x);
  Out_String(str);

  if (*str == '0' && str[1] == '\0')
    pl_last_writing = W_NUMBER_0;
  else
    pl_last_writing = W_NUMBER;
}



#ifndef NO_USE_FD_SOLVER
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

  pl_last_writing = W_IDENTIFIER;
}
#endif



/*-------------------------------------------------------------------------*
 * SHOW_FLOAT                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Float(double x)
{
  Out_String(Pl_Float_To_String(x));

  pl_last_writing = W_NUMBER;
}




/*-------------------------------------------------------------------------*
 * SHOW_LIST_ARG                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/

#ifdef SPACE_ARGS_FOR_LIST_PIPE
#define SHOW_LIST_PIPE if (space_args) Out_String(" | "); else Out_Char('|')
#else
#define SHOW_LIST_PIPE Out_Char('|')
#endif

static void
Show_List_Arg(int depth, WamWord *lst_adr)
{
  WamWord word, tag_mask;

 terminal_rec:
  depth--;

  Show_Term(depth, MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM, Car(lst_adr));

  if (depth == 0)		/* dots already written by Show_Term */
    return;


  DEREF(Cdr(lst_adr), word, tag_mask);

  switch (Tag_From_Tag_Mask(tag_mask))
    {
    case REF:
      SHOW_LIST_PIPE;
      Show_Global_Var(UnTag_REF(word));
      break;

    case ATM:
      if (word != NIL_WORD)
	{
	  SHOW_LIST_PIPE;
	  if (Try_Portray(word))
	    return;

	  Show_Atom(GENERAL_TERM, UnTag_ATM(word));
	}
      break;

#ifndef NO_USE_FD_SOLVER
    case FDV:
      SHOW_LIST_PIPE;
      if (Try_Portray(word))
	return;

      Show_Fd_Variable(UnTag_FDV(word));
      break;
#endif

    case INT:
      SHOW_LIST_PIPE;
      if (Try_Portray(word))
	return;

      Show_Integer(UnTag_INT(word));
      break;

    case FLT:
      SHOW_LIST_PIPE;
      if (Try_Portray(word))
	return;

      Show_Float(Pl_Obtain_Float(UnTag_FLT(word)));
      break;

    case LST:
      Out_Char(',');
      if (space_args)
	Out_Space();
      lst_adr = UnTag_LST(word);
      goto terminal_rec;
      break;

    case STC:
      SHOW_LIST_PIPE;
      if (Try_Portray(word))
	return;

      Show_Structure(depth, MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM,
		     UnTag_STC(word));
      break;
    }
}



/*-------------------------------------------------------------------------*
 * IS_VALID_VAR_NAME                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Is_Valid_Var_Name(char *str)

{
  int c_type;

  c_type = pl_char_type[(unsigned) *str];
  if ((c_type & (UL | CL)) == 0) /* neither underline nor capital letter */
    return FALSE;

  while(*++str != '\0')
    {
      c_type = pl_char_type[(unsigned) *str];
      if ((c_type & (UL | CL | SL | DI)) == 0)
	return FALSE;
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_IS_VALID_VAR_NAME_1                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Is_Valid_Var_Name_1(WamWord name_word)
{
  WamWord word, tag_mask;

  DEREF(name_word, word, tag_mask);
  return (tag_mask == TAG_ATM_MASK) && Is_Valid_Var_Name(pl_atom_tbl[UnTag_ATM(word)].name);
}




/*-------------------------------------------------------------------------*
 * SHOW_STRUCTURE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Structure(int depth, int prec, int context, WamWord *stc_adr)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord f_n = Functor_And_Arity(stc_adr);
  int functor = Functor(stc_adr);
  int arity = Arity(stc_adr);
  OperInf *oper;
  int nb_args_to_disp;
  int i, j, n;
  char str[32];
  Bool bracket;
  Bool surround_space;
  char *p;


  depth--;

  if (name_vars && f_n == dollar_varname_1 && stc_adr >= name_number_above_H)
    {
      DEREF(Arg(stc_adr, 0), word, tag_mask);
      if (tag_mask == TAG_ATM_MASK)
	{
	  p = pl_atom_tbl[UnTag_ATM(word)].name;
	  if (Is_Valid_Var_Name(p))
	    {
	      Out_String(p);
	      pl_last_writing = W_IDENTIFIER;
	      return;
	    }
	}
    }

  if (number_vars && f_n == dollar_var_1 && stc_adr >= name_number_above_H)
    {
      DEREF(Arg(stc_adr, 0), word, tag_mask);
      if (tag_mask == TAG_INT_MASK && (n = UnTag_INT(word)) >= 0)
	{
	  i = n % 26;
	  j = n / 26;

	  Out_Char('A' + i);

	  if (j)
	    {
	      sprintf(str, "%d", j);
	      Out_String(str);
	    }

	  pl_last_writing = W_IDENTIFIER;
	  return;
	}
    }

  if (ignore_op || arity > 2)
    goto functional;

  if (f_n == curly_brackets_1)
    {
      Out_Char('{');
      if (space_args)
	Out_Space();
      Show_Term(depth, MAX_PREC, GENERAL_TERM, Arg(stc_adr, 0));
      if (space_args)
	Out_Space();
      Out_Char('}');
      return;
    }

  bracket = FALSE;

  if (arity == 1 && (oper = Pl_Lookup_Oper(functor, PREFIX)))
    {
#if 1
      /* Koen de Bosschere says "in case of ambiguity :          */
      /* select the associative operator over the nonassociative */
      /* select prefix over postfix".                            */

      OperInf *oper1;

      if (oper->prec > oper->right
	  && (oper1 = Pl_Lookup_Oper(functor, POSTFIX))
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

      last_prefix_op = W_PREFIX_OP_ANY;

      if (space_args
#if SPACE_ARGS_RESTRICTED	/* space_args -> space after fx operator */
	  && oper->prec > oper->right
#endif
	  )
	Out_Space();
      else
	if (strcmp(pl_atom_tbl[functor].name, "-") == 0)
	  {
	    last_prefix_op = W_PREFIX_OP_MINUS;
	    p_bracket_minus = &bracket;
	  }

      Show_Term(depth, oper->right, INSIDE_ANY_OP, Arg(stc_adr, 0));
      last_prefix_op = W_NO_PREFIX_OP;

      /* Here we need a while(bracket--) instead of if(bracket) because
       * in some cases with the minus op and additional bracket is needed.
       * Example: with op(100, xfx, &) (recall the prec of - is 200). 
       * The term ((-(1)) & b must be displayed as: (- (1)) & b
       * Concerning the sub-term - (1), the first ( is emitted  10 lines above
       * because the precedence of - (200) is > precedence of & (100).
       * The second ( is emitted by Need_Space() because the argument of - begins 
       * by a digit. At the return we have to close 2 ).
       */

      while (bracket--)	
	Out_Char(')');

      return;
    }


  if (arity == 1 && (oper = Pl_Lookup_Oper(functor, POSTFIX)))
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

      if (space_args
#if SPACE_ARGS_RESTRICTED	/* space_args -> space before xf operator */
	  && oper->prec > oper->left
#endif
	  )
	Out_Space();

      Show_Atom(GENERAL_TERM, functor);

      if (bracket)
	Out_Char(')');

      return;
    }


  if (arity == 2 && (oper = Pl_Lookup_Oper(functor, INFIX)))
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

#if 1 /* to show | unquoted if it is an infix operator with prec > 1000 */
      if (functor == ATOM_CHAR('|') && oper->prec > 1000)
	{
	  if (space_args)
	    Out_Space();
	  Out_Char('|');
	  if (space_args)
	    Out_Space();
	}
      else
#endif
	if (functor == ATOM_CHAR(','))
	  {
	    Out_Char(',');
	    if (space_args)
	      Out_Space();
	  }
	else
	  {
	    surround_space = FALSE;

	    if (pl_atom_tbl[functor].prop.type == IDENTIFIER_ATOM ||
		pl_atom_tbl[functor].prop.type == OTHER_ATOM ||
		(space_args
#ifdef SPACE_ARGS_RESTRICTED	/* space_args -> space around xfx operators */
		 && oper->left != oper->prec && oper->right != oper->prec
#endif
		 ))
	      {
		surround_space = TRUE;
		Out_Space();
	      }

	    Show_Atom(GENERAL_TERM, functor);

	    if (surround_space)
	      Out_Space();
	  }

      Show_Term(depth, oper->right, INSIDE_ANY_OP, Arg(stc_adr, 1));

      if (bracket)
	Out_Char(')');

      return;
    }



 functional:			/* functional notation */

  Show_Atom(GENERAL_TERM, functor);
  Out_Char('(');

  nb_args_to_disp = i = (arity < depth + 1 || depth < 0) ? arity : depth + 1;
  adr = &Arg(stc_adr, 0);

  goto start_display;

  do
    {
      Out_Char(',');
      if (space_args)
	Out_Space();
    start_display:
      Show_Term(depth, MAX_ARG_OF_FUNCTOR_PREC, GENERAL_TERM, *adr++);
    }
  while (--i);

  if (arity != nb_args_to_disp)
    {
      Out_Char(',');
      if (space_args)
	Out_Space();
      Show_Atom(GENERAL_TERM, atom_dots);
    }

  Out_Char(')');
}




/*-------------------------------------------------------------------------*
 * TRY_PORTRAY                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Try_Portray(WamWord word)
{
#ifdef FOR_EXTERNAL_USE
  return FALSE;
#else
  PredInf *pred;
  StmInf *print_pstm_o;
  Bool print_quoted;
  Bool print_ignore_op;
  Bool print_number_vars;
  Bool print_name_vars;
  Bool print_space_args;
  Bool print_portrayed;
  Bool print_ok;
  static CodePtr try_portray_code = NULL;

  if (!portrayed)
    return FALSE;

  if (try_portray_code == NULL)
    {
      pred = Pl_Lookup_Pred(Pl_Create_Atom("$try_portray"), 1);
      if (pred == NULL || pred->codep == NULL)
	Pl_Err_Resource(pl_resource_print_object_not_linked);

      try_portray_code = (CodePtr) (pred->codep);
    }

  print_pstm_o = pstm_o;
  print_quoted = quoted;
  print_ignore_op = ignore_op;
  print_number_vars = number_vars;
  print_name_vars = name_vars;
  print_space_args = space_args;
  print_portrayed = portrayed;

  A(0) = word;
  print_ok = Pl_Call_Prolog(try_portray_code);

  pstm_o = print_pstm_o;
  quoted = print_quoted;
  ignore_op = print_ignore_op;
  number_vars = print_number_vars;
  name_vars = print_name_vars;
  space_args = print_space_args;
  portrayed = print_portrayed;

  return print_ok;
#endif
}




/*-------------------------------------------------------------------------*
 * PL_GET_PRINT_STM_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Print_Stm_1(WamWord stm_word)
{
  int stm = Pl_Find_Stream_From_PStm(pstm_o);

  if (stm < 0)
    stm = pl_stm_output;

  return Pl_Get_Integer(stm, stm_word);
}

