/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pretty_c.c                                                      *
 * Descr.: pretty print clause management - C part                         *
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
static int atom_soft_if;	/* soft-cut */

static int atom_dollar_var;
static int atom_dollar_varname;

static WamWord dollar_var_1;
static WamWord dollar_varname_1;
static WamWord equal_2;

static PlLong *singl_var_ptr;
static int nb_singl_var;

static int nb_to_try;

static WamWord *above_H;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Portray_Clause(StmInf *pstm, WamWord term_word);

static Bool Check_Structure(WamWord term_word, int func, int arity,
			    WamWord arg_word[]);

static Bool Is_Cut(WamWord body_word);

static void Show_Body(StmInf *pstm, int level, int context, WamWord body_word);

static void Start_Line(StmInf *pstm, int level, char c_before);

static Bool Collect_Singleton(WamWord *adr);

static int Var_Name_To_Var_Number(int atom);

static void Exclude_A_Var_Number(int n);

static void Collect_Excluded_Rec(WamWord start_word);

static Bool Bind_Variable(WamWord *adr, WamWord word);




/*-------------------------------------------------------------------------*
 * PRETTY_INITIALIZER                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Pretty_Initializer(void)
{
  atom_clause = Pl_Create_Atom(":-");
  atom_dcg = Pl_Create_Atom("-->");
  atom_if = Pl_Create_Atom("->");
  atom_soft_if = Pl_Create_Atom("*->");

  atom_dollar_var = Pl_Create_Atom("$VAR");
  atom_dollar_varname = Pl_Create_Atom("$VARNAME");

  dollar_var_1 = Functor_Arity(atom_dollar_var, 1);
  dollar_varname_1 = Functor_Arity(atom_dollar_varname, 1);
  equal_2 = Functor_Arity(ATOM_CHAR('='), 2);
}




/*-------------------------------------------------------------------------*
 * PL_PORTRAY_CLAUSE_3                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Portray_Clause_3(WamWord sora_word, WamWord term_word, WamWord above_word)
{
  int stm;
  StmInf *pstm;
  WamWord *b;
  

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_output : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);
  pstm = pl_stm_tbl[stm];

  pl_last_output_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, FALSE);

  b = LSSA + Pl_Rd_Integer(above_word); /* see Pl_Get_Current_Choice / Pl_Cut */
  above_H = HB(b);

  Portray_Clause(pstm, term_word);
}




/*-------------------------------------------------------------------------*
 * PL_PORTRAY_CLAUSE_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Portray_Clause_2(WamWord term_word, WamWord above_word)
{
  Pl_Portray_Clause_3(NOT_A_WAM_WORD, term_word, above_word);
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
  int atom;

  if (Check_Structure(term_word, atom_clause, 2, arg_word))
    {
      Pl_Write_Term(pstm, -1, 1200 - 1, WRITE_MASK, above_H, arg_word[0]);
      DEREF(arg_word[1], word, tag_mask);
      atom = UnTag_ATM(word);
      if (tag_mask != TAG_ATM_MASK || atom != pl_atom_true)
	{
	  Pl_Stream_Puts(" :-", pstm);
	  Start_Line(pstm, 0, ' ');
	  Show_Body(pstm, 0, GENERAL_BODY, arg_word[1]);
	}
      Pl_Write_A_Full_Stop(pstm);
      return;
    }

  if (Check_Structure(term_word, atom_dcg, 2, arg_word))
    {
      Pl_Write_Term(pstm, -1, 1200 - 1, WRITE_MASK, above_H, arg_word[0]);
      DEREF(arg_word[1], word, tag_mask);
      atom = UnTag_ATM(word);
      if (tag_mask != TAG_ATM_MASK || atom != pl_atom_true)
	{
	  Pl_Stream_Puts(" -->", pstm);
	  Start_Line(pstm, 0, ' ');
	  Show_Body(pstm, 0, GENERAL_BODY, arg_word[1]);
	}
      Pl_Write_A_Full_Stop(pstm);
      return;
    }

  if (Check_Structure(term_word, atom_clause, 1, arg_word))
    {
      Pl_Stream_Puts(":-\t", pstm);
      Show_Body(pstm, 0, GENERAL_BODY, arg_word[0]);
      Pl_Write_A_Full_Stop(pstm);
      return;
    }

  Pl_Write_Term(pstm, -1, MAX_PREC, WRITE_MASK, above_H, term_word);
  Pl_Write_A_Full_Stop(pstm);
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
  int soft_cut;
  static int prec[] = { 1200 - 1, 1000 - 1, 1000, 1100 - 1, 1100, 1050 - 1, 1050 };


  if (Check_Structure(body_word, ATOM_CHAR(','), 2, arg_word))
    {
      Show_Body(pstm, level, LEFT_AND, arg_word[0]);
      Pl_Stream_Putc(',', pstm);
      if (Is_Cut(arg_word[1]))
	Pl_Stream_Putc(' ', pstm);
      else
	Start_Line(pstm, level, ' ');
      Show_Body(pstm, level, RIGHT_AND, arg_word[1]);
      return;
    }


  if (Check_Structure(body_word, ATOM_CHAR(';'), 2, arg_word))
    {
      if (context != RIGHT_OR)
	{
	  Pl_Stream_Puts("(   ", pstm);
	  level++;
	}

      Show_Body(pstm, level, LEFT_OR, arg_word[0]);
      Start_Line(pstm, level, ';');
      Show_Body(pstm, level, RIGHT_OR, arg_word[1]);

      if (context != RIGHT_OR)
	{
	  Start_Line(pstm, level - 1, ' ');
	  Pl_Stream_Putc(')', pstm);
	}
      return;
    }


  soft_cut = 0;
  if (Check_Structure(body_word, atom_if, 2, arg_word) ||
      (soft_cut = 1, Check_Structure(body_word, atom_soft_if, 2, arg_word)))
    {
      if (context != LEFT_OR && context != RIGHT_OR)
	{
	  Pl_Stream_Puts("(   ", pstm);
	  level++;
	}

      Show_Body(pstm, level, LEFT_IF, arg_word[0]);
      Pl_Stream_Puts((soft_cut == 0) ? " ->" : " *-> ", pstm);
      Start_Line(pstm, level, ' ');
      Show_Body(pstm, level, RIGHT_IF, arg_word[1]);

      if (context != LEFT_OR && context != RIGHT_OR)
	{
	  Start_Line(pstm, level - 1, ' ');
	  Pl_Stream_Putc(')', pstm);
	}

      return;
    }

  Pl_Write_Term(pstm, -1, prec[context], WRITE_MASK, above_H, body_word);
}




/*-------------------------------------------------------------------------*
 * START_LINE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Start_Line(StmInf *pstm, int level, char c_before)
{
  char *p = pl_glob_buff;
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

  Pl_Stream_Puts(pl_glob_buff, pstm);
  pl_last_writing = 0;		/* ie. W_NOTHING */
}




/*-------------------------------------------------------------------------*
 * PL_NAME_SINGLETON_VARS_1                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Name_Singleton_Vars_1(WamWord start_word)
{
  WamWord word;

  if (!Pl_Acyclic_Term_1(start_word))
    return;

  singl_var_ptr = pl_glob_dico_var;	/* pl_glob_dico_var: stores singletons */
  nb_singl_var = 0;


  Pl_Treat_Vars_Of_Term(start_word, FALSE, Collect_Singleton);

  if (nb_singl_var == 0)
    return;

  word = Pl_Put_Structure(atom_dollar_varname, 1);
  Pl_Unify_Atom(ATOM_CHAR('_'));	/* bind to '$VARNAME'('_') */

  while (--singl_var_ptr >= pl_glob_dico_var)
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
static Bool
Collect_Singleton(WamWord *adr)
{
  PlLong *p;

  for (p = pl_glob_dico_var; p < singl_var_ptr; p++)
    if ((*p & ~1) == (PlLong) adr)	/* not a singleton */
      {
	if ((*p & 1) == 0)	/* not yet marked - mark it */
	  {
	    *p |= 1;
	    nb_singl_var--;
	  }
	return TRUE;
      }

  if (singl_var_ptr - pl_glob_dico_var >= MAX_VAR_IN_TERM)
    Pl_Err_Representation(pl_representation_too_many_variables);

  *singl_var_ptr++ = (PlLong) adr;
  nb_singl_var++;
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_NAME_QUERY_VARS_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Name_Query_Vars_2(WamWord query_list_word, WamWord rest_list_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr, *stc_adr;

  save_list_word = query_list_word;

  Pl_Check_For_Un_List(rest_list_word);

  for (;;)
    {
      DEREF(query_list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(pl_type_list, save_list_word);

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
	  Pl_Get_Structure(atom_dollar_varname, 1, word);
	  Pl_Unify_Value(Arg(stc_adr, 0));	/* bind Value to '$VARNAME'(Name) */
	}
      else
	{
	unchanged:
	  if (!Pl_Get_List(rest_list_word) || !Pl_Unify_Value(Car(lst_adr)))
	    return FALSE;

	  rest_list_word = Pl_Unify_Variable();
	}
      query_list_word = Cdr(lst_adr);
    }

  return Pl_Get_Nil(rest_list_word);
}




#define BIND_WITH_NUMBERVAR        (pl_sys_var[0] == 0)


/*-------------------------------------------------------------------------*
 * PL_BIND_VARIABLES_4                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Bind_Variables_4(WamWord term_word, WamWord exclude_list_word,
		    WamWord from_word, WamWord next_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr, *stc_adr;
  int i;

  for (i = 0; i < MAX_VAR_IN_TERM; i++)
    pl_glob_dico_var[i] = 0;	/* pl_glob_dico_var: excluded var ? (0/1) */

  nb_to_try = Pl_Rd_Positive_Check(from_word);

  save_list_word = exclude_list_word;

  for (;;)
    {
      DEREF(exclude_list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(pl_type_list, save_list_word);

      lst_adr = UnTag_LST(word);

      DEREF(Car(lst_adr), word, tag_mask);
      if (Pl_Acyclic_Term_1(word))
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

  if (Pl_Acyclic_Term_1(term_word))
    Pl_Treat_Vars_Of_Term(term_word, FALSE, Bind_Variable);

  return Pl_Un_Integer_Check(nb_to_try, next_word);
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

  p = pl_atom_tbl[atom].name;
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
    pl_glob_dico_var[n] = 1;
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
static Bool
Bind_Variable(WamWord *adr, WamWord word)
{
  int i, j;
  char buff[16];

  while (pl_glob_dico_var[nb_to_try] && nb_to_try < MAX_VAR_IN_TERM)
    nb_to_try++;

  if (BIND_WITH_NUMBERVAR)
    {
      Pl_Get_Structure(atom_dollar_var, 1, word);
      Pl_Unify_Integer(nb_to_try++);
      return TRUE;
    }

  i = nb_to_try % 26;
  j = nb_to_try / 26;
  nb_to_try++;

  buff[0] = 'A' + i;

  if (j)
    sprintf(buff + 1, "%d", j);
  else
    buff[1] = '\0';

  Pl_Get_Structure(atom_dollar_varname, 1, word);
  Pl_Unify_Atom(Pl_Create_Allocate_Atom(buff));

  return TRUE;
}
