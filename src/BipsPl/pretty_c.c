/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pretty_c.c                                                      *
 * Descr.: pretty print clause management - C part                         *
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

#define OBJ_INIT Pretty_Initializer

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define GENERAL_BODY               0

#define LEFT_AND                   1
#define RIGHT_AND                  2

#define LEFT_OR                    3
#define RIGHT_OR                   4

#define LEFT_IF                    5
#define RIGHT_IF                   6




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int atom_clause;
static int atom_dcg;
static int atom_if;

static int atom_dollar_var;
static int atom_dollar_varname;

static WamWord dollar_var_1;
static WamWord dollar_varname_1;
static WamWord equal_2;

static long *singl_var_ptr;
static int nb_singl_var;

static int nb_to_try;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Portray_Clause(StmInf *pstm, WamWord term_word);

static
  Bool Check_Structure(WamWord term_word, int func, int arity,
		       WamWord arg_word[]);

static Bool Is_Cut(WamWord body_word);

static
  void Show_Body(StmInf *pstm, int level, int context, WamWord body_word);

static void Start_Line(StmInf *pstm, int level, char c_before);

static void Collect_Singleton(WamWord *adr);

static int Var_Name_To_Var_Number(int atom);

static void Exclude_A_Var_Number(int n);

static void Collect_Excluded_Rec(WamWord start_word);

static void Bind_Variable(WamWord *adr, WamWord word);




/*-------------------------------------------------------------------------*
 * PRETTY_INITIALIZER                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Pretty_Initializer(void)
{
  atom_clause = Create_Atom(":-");
  atom_dcg = Create_Atom("-->");
  atom_if = Create_Atom("->");

  atom_dollar_var = Create_Atom("$VAR");
  atom_dollar_varname = Create_Atom("$VARNAME");

  dollar_var_1 = Functor_Arity(atom_dollar_var, 1);
  dollar_varname_1 = Functor_Arity(atom_dollar_varname, 1);
  equal_2 = Functor_Arity(ATOM_CHAR('='), 2);
}




/*-------------------------------------------------------------------------*
 * PORTRAY_CLAUSE_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Portray_Clause_2(WamWord sora_word, WamWord term_word)
{
  int stm;
  StmInf *pstm;


  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_output : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);
  pstm = stm_tbl + stm;

  last_output_sora = sora_word;
  Check_Stream_Type(stm, TRUE, FALSE);

  Portray_Clause(pstm, term_word);
}




/*-------------------------------------------------------------------------*
 * PORTRAY_CLAUSE_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Portray_Clause_1(WamWord term_word)
{
  Portray_Clause_2(NOT_A_WAM_WORD, term_word);
}


#define WRITE_MASK   (WRITE_NUMBER_VARS | WRITE_NAME_VARS | \
                      WRITE_QUOTED      | WRITE_SPACE_ARGS)




/*-------------------------------------------------------------------------*
 * PORTRAY_CLAUSE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Portray_Clause(StmInf *pstm, WamWord term_word)
{
  WamWord word, tag_mask;
  WamWord arg_word[2];

  if (Check_Structure(term_word, atom_clause, 2, arg_word))
    {
      Write_Term(pstm, -1, 1200 - 1, WRITE_MASK, arg_word[0]);
      DEREF(arg_word[1], word, tag_mask);
      if (tag_mask != TAG_ATM_MASK || UnTag_ATM(word) != atom_true)
	{
	  Stream_Puts(" :-", pstm);
	  Start_Line(pstm, 0, ' ');
	  Show_Body(pstm, 0, GENERAL_BODY, arg_word[1]);
	}
      Write_A_Char(pstm, '.');
      return;
    }

  if (Check_Structure(term_word, atom_dcg, 2, arg_word))
    {
      Write_Term(pstm, -1, 1200 - 1, WRITE_MASK, arg_word[0]);
      DEREF(arg_word[1], word, tag_mask);
      if (tag_mask != TAG_ATM_MASK || UnTag_ATM(word) != atom_true)
	{
	  Stream_Puts(" -->", pstm);
	  Start_Line(pstm, 0, ' ');
	  Show_Body(pstm, 0, GENERAL_BODY, arg_word[1]);
	}
      Write_A_Char(pstm, '.');
      return;
    }

  if (Check_Structure(term_word, atom_clause, 1, arg_word))
    {
      Stream_Puts(":-\t", pstm);
      Show_Body(pstm, 0, GENERAL_BODY, arg_word[0]);
      Write_A_Char(pstm, '.');
      return;
    }

  Write_Term(pstm, -1, MAX_PREC, WRITE_MASK, term_word);
  Write_A_Char(pstm, '.');
}




/*-------------------------------------------------------------------------*
 * CHECK_STRUCTURE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Check_Structure(WamWord term_word, int func, int arity, WamWord arg_word[])
{
  WamWord word, tag_mask;
  WamWord *adr;
  int i;


  DEREF(term_word, word, tag_mask);
  if (tag_mask != TAG_STC_MASK)
    return FALSE;

  adr = UnTag_STC(word);

  if (Functor_And_Arity(adr) != Functor_Arity(func, arity))
    return FALSE;

  for (i = 0; i < arity; i++)
    arg_word[i] = Arg(adr, i);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * IS_CUT                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Is_Cut(WamWord body_word)
{
  WamWord word, tag_mask;
  WamWord arg_word[2];

  while (Check_Structure(body_word, ATOM_CHAR(','), 2, arg_word))
    body_word = arg_word[0];

  DEREF(body_word, word, tag_mask);
  return (word == Tag_ATM(ATOM_CHAR('!')));
}




/*-------------------------------------------------------------------------*
 * SHOW_BODY                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Show_Body(StmInf *pstm, int level, int context, WamWord body_word)
{
  WamWord arg_word[2];
  static 
    int prec[] = { 1200 - 1, 1000 - 1, 1000, 1100 - 1, 1100, 1050 - 1, 1050 };


  if (Check_Structure(body_word, ATOM_CHAR(','), 2, arg_word))
    {
      Show_Body(pstm, level, LEFT_AND, arg_word[0]);
      Stream_Putc(',', pstm);
      if (Is_Cut(arg_word[1]))
	Stream_Putc(' ', pstm);
      else
	Start_Line(pstm, level, ' ');
      Show_Body(pstm, level, RIGHT_AND, arg_word[1]);
      return;
    }


  if (Check_Structure(body_word, ATOM_CHAR(';'), 2, arg_word))
    {
      if (context != RIGHT_OR)
	{
	  Stream_Puts("(   ", pstm);
	  level++;
	}

      Show_Body(pstm, level, LEFT_OR, arg_word[0]);
      Start_Line(pstm, level, ';');
      Show_Body(pstm, level, RIGHT_OR, arg_word[1]);

      if (context != RIGHT_OR)
	{
	  Start_Line(pstm, level - 1, ' ');
	  Stream_Putc(')', pstm);
	}
      return;
    }


  if (Check_Structure(body_word, atom_if, 2, arg_word))
    {
      if (context != LEFT_OR && context != RIGHT_OR)
	{
	  Stream_Puts("(   ", pstm);
	  level++;
	}

      Show_Body(pstm, level, LEFT_IF, arg_word[0]);
      Stream_Puts(" ->", pstm);
      Start_Line(pstm, level, ' ');
      Show_Body(pstm, level, RIGHT_IF, arg_word[1]);

      if (context != LEFT_OR && context != RIGHT_OR)
	{
	  Start_Line(pstm, level - 1, ' ');
	  Stream_Putc(')', pstm);
	}

      return;
    }

  Write_Term(pstm, -1, prec[context], WRITE_MASK, body_word);
}




/*-------------------------------------------------------------------------*
 * START_LINE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Start_Line(StmInf *pstm, int level, char c_before)
{
  char *p = glob_buff;
  int i;


  *p++ = '\n';
  *p++ = '\t';

  for (i = 0; i < 4 * (level - 1); i++)
    *p++ = ' ';

  if (level != 0)
    {
      *p++ = c_before;
      *p++ = ' ';
      *p++ = ' ';
      *p++ = ' ';
    }

  *p = '\0';

  Stream_Puts(glob_buff, pstm);
  last_writing = 0;		/* ie. W_NOTHING */
}




/*-------------------------------------------------------------------------*
 * NAME_SINGLETON_VARS_1                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Name_Singleton_Vars_1(WamWord start_word)
{
  WamWord word;

  singl_var_ptr = glob_dico_var;	/* glob_dico_var: stores singletons */
  nb_singl_var = 0;

  Treat_Vars_Of_Term(start_word, FALSE, Collect_Singleton);

  if (nb_singl_var == 0)
    return;

  word = Put_Structure(atom_dollar_varname, 1);
  Unify_Atom(ATOM_CHAR('_'));	/* bind to '$VARNAME'('_') */

  while (--singl_var_ptr >= glob_dico_var)
    {
      if (*singl_var_ptr & 1)	/* marked - not a singleton */
	continue;

      Bind_UV((WamWord *) *singl_var_ptr, word);
    }
}




/*-------------------------------------------------------------------------*
 * COLLECT_SINGLETON                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Collect_Singleton(WamWord *adr)
{
  long *p;

  for (p = glob_dico_var; p < singl_var_ptr; p++)
    if ((*p & ~1) == (long) adr)	/* not a singleton */
      {
	if ((*p & 1) == 0)	/* not yet marked - mark it */
	  {
	    *p |= 1;
	    nb_singl_var--;
	  }
	return;
      }

  if (singl_var_ptr - glob_dico_var >= MAX_VAR_IN_TERM)
    Pl_Err_Representation(representation_too_many_variables);

  *singl_var_ptr++ = (long) adr;
  nb_singl_var++;
}




/*-------------------------------------------------------------------------*
 * NAME_QUERY_VARS_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Name_Query_Vars_2(WamWord query_list_word, WamWord rest_list_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr, *stc_adr;

  save_list_word = query_list_word;

  Check_For_Un_List(rest_list_word);

  for (;;)
    {
      DEREF(query_list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, save_list_word);

      lst_adr = UnTag_LST(word);

      DEREF(Car(lst_adr), word, tag_mask);
      stc_adr = UnTag_STC(word);
      if (tag_mask == TAG_STC_MASK && Functor_And_Arity(stc_adr) == equal_2)
	{			/* form: Name=Value */
	  DEREF(Arg(stc_adr, 0), word, tag_mask);
	  if (tag_mask != TAG_ATM_MASK)
	    goto unchanged;
				/* Value is a variable */
	  DEREF(Arg(stc_adr, 1), word, tag_mask);
	  if (tag_mask != TAG_REF_MASK)
	    goto unchanged;
				/* Value is a variable */
	  Get_Structure(atom_dollar_varname, 1, word);
	  Unify_Value(Arg(stc_adr, 0));	/* bind Value to '$VARNAME'(Name) */
	}
      else
	{
	unchanged:
	  if (!Get_List(rest_list_word) || !Unify_Value(Car(lst_adr)))
	    return FALSE;

	  rest_list_word = Unify_Variable();
	}
      query_list_word = Cdr(lst_adr);
    }

  return Get_Nil(rest_list_word);
}




#define BIND_WITH_NUMBERVAR        (sys_var[0]==0)


/*-------------------------------------------------------------------------*
 * BIND_VARIABLES_4                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Bind_Variables_4(WamWord term_word, WamWord exclude_list_word,
		 WamWord from_word, WamWord next_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr, *stc_adr;
  int i;

  for (i = 0; i < MAX_VAR_IN_TERM; i++)
    glob_dico_var[i] = 0;	/* glob_dico_var: excluded var ? (0/1) */

  nb_to_try = Rd_Positive_Check(from_word);

  save_list_word = exclude_list_word;

  for (;;)
    {
      DEREF(exclude_list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, save_list_word);

      lst_adr = UnTag_LST(word);

      DEREF(Car(lst_adr), word, tag_mask);
      Collect_Excluded_Rec(word);

      stc_adr = UnTag_STC(word);
      if (tag_mask == TAG_STC_MASK && Functor_And_Arity(stc_adr) == equal_2)
	{			/* form: Name=Value */
	  DEREF(Arg(stc_adr, 0), word, tag_mask);
	  if (tag_mask == TAG_ATM_MASK)
	    Exclude_A_Var_Number(Var_Name_To_Var_Number(UnTag_ATM(word)));
	}

      exclude_list_word = Cdr(lst_adr);
    }

  Treat_Vars_Of_Term(term_word, FALSE, Bind_Variable);

  return Un_Integer_Check(nb_to_try, next_word);
}




/*-------------------------------------------------------------------------*
 * VAR_NAME_TO_VAR_NUMBER                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Var_Name_To_Var_Number(int atom)
{
  char *p, *q;
  int n;

  p = atom_tbl[atom].name;
  if (*p < 'A' || *p > 'Z')
    return -1;

  n = strtol(p + 1, &q, 10);
  if (*q)
    return -1;

  n = n * 26 + *p - 'A';

  return n;
}




/*-------------------------------------------------------------------------*
 * EXCLUDE_A_VAR_NUMBER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Exclude_A_Var_Number(int n)
{
  if (n >= 0 && n < MAX_VAR_IN_TERM)
    glob_dico_var[n] = 1;
}




/*-------------------------------------------------------------------------*
 * COLLECT_EXCLUDED_REC                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Collect_Excluded_Rec(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord *stc_adr;
  int i;

 terminal_rec:

  DEREF(start_word, word, tag_mask);
  
  if (tag_mask == TAG_LST_MASK)
    {
      adr = UnTag_LST(word);
      adr = &Car(adr);
      Collect_Excluded_Rec(*adr++);
      start_word = *adr;
      goto terminal_rec;
    }

  if (tag_mask != TAG_STC_MASK)
    return;

  stc_adr = UnTag_STC(word);
  if (Functor_And_Arity(stc_adr) == dollar_var_1)
    {
      DEREF(Arg(stc_adr, 0), word, tag_mask);
      if (tag_mask != TAG_INT_MASK)
	goto normal_compound;

      Exclude_A_Var_Number(UnTag_INT(word));
      return;
    }

  if (Functor_And_Arity(stc_adr) == dollar_varname_1)
    {
      DEREF(Arg(stc_adr, 0), word, tag_mask);
      if (tag_mask != TAG_ATM_MASK)
	goto normal_compound;

      Exclude_A_Var_Number(Var_Name_To_Var_Number(UnTag_ATM(word)));
      return;
    }

 normal_compound:
  i = Arity(stc_adr);
  adr = &Arg(stc_adr, 0);
  while (--i)
    Collect_Excluded_Rec(*adr++);
  start_word = *adr;
  goto terminal_rec;
}




/*-------------------------------------------------------------------------*
 * BIND_VARIABLE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Bind_Variable(WamWord *adr, WamWord word)
{
  int i, j;
  char buff[16];

  while (glob_dico_var[nb_to_try] && nb_to_try < MAX_VAR_IN_TERM)
    nb_to_try++;

  if (BIND_WITH_NUMBERVAR)
    {
      Get_Structure(atom_dollar_var, 1, word);
      Unify_Integer(nb_to_try++);
      return;
    }

  i = nb_to_try % 26;
  j = nb_to_try / 26;
  nb_to_try++;

  buff[0] = 'A' + i;

  if (j)
    sprintf(buff + 1, "%d", j);
  else
    buff[1] = '\0';

  Get_Structure(atom_dollar_varname, 1, word);
  Unify_Atom(Create_Allocate_Atom(buff));
}
