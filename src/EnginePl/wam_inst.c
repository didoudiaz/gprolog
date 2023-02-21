/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : wam_inst.c                                                      *
 * Descr.: WAM instruction implementation                                  *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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
#include <stdlib.h>
#include <string.h>

#include "engine_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef union
{
  double d;
  WamWord i[2];
}
DblInt;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static SwtInf *Locate_Swt_Element(SwtTbl t, int size, PlLong key);




/*-------------------------------------------------------------------------*
 * PL_CREATE_FUNCTOR_ARITY_TAGGED                                          *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Create_Functor_Arity_Tagged(char *func_str, int arity)
{
  int func = Pl_Create_Atom(func_str);

  return Functor_Arity(func, arity);
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_SWT_TABLE                                                     *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/

SwtTbl FC
Pl_Create_Swt_Table(int size)
{
  SwtTbl t;

  size++;			/* +1 to ensure that one free cell exists */

  t = (SwtTbl) Calloc(size, sizeof(SwtInf));

  return t;
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_SWT_ATM_ELEMENT                                               *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Create_Swt_Atm_Element(SwtTbl t, int size, int atom, CodePtr codep)
{
  SwtInf *swt = Locate_Swt_Element(t, size, atom);

  swt->key = atom;
  swt->codep = codep;
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_SWT_STC_ELEMENT                                               *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Create_Swt_Stc_Element(SwtTbl t, int size, int func, int arity,
			  CodePtr codep)
{
  PlLong key = Functor_Arity(func, arity);

  SwtInf *swt = Locate_Swt_Element(t, size, key);

  swt->key = key;
  swt->codep = codep;
}



/*-------------------------------------------------------------------------*
 * LOCATE_SWT_ELEMENT                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static SwtInf *
Locate_Swt_Element(SwtTbl t, int size, PlLong key)
{
  int n;
  SwtInf *swt, *endt;

  size++;			/* +1 to ensure that one free cell exists */

#if 1
  n = key % size;
#else
  n = (key ^ ((PlULong) key >> 16)) % size;
#endif
				/* here either the key is in the table */
				/* or there is at least one free cell. */
  swt = t + n;
  endt = t + size;

  while (swt->codep && swt->key != key)
    {
      swt++;
      if (swt == endt)
	swt = t;
    }

  return swt;
}




/*-------------------------------------------------------------------------*
 * PL_GET_ATOM_TAGGED                                                      *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Get_Atom_Tagged(WamWord w, WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
      Bind_UV(UnTag_REF(word), w);
      return TRUE;
    }

 return (word == w);
}




/*-------------------------------------------------------------------------*
 * PL_GET_ATOM                                                             *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Get_Atom(int atom, WamWord start_word)
{
  return Pl_Get_Atom_Tagged(Tag_ATM(atom), start_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_INTEGER_TAGGED                                                   *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Get_Integer_Tagged(WamWord w, WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
      Bind_UV(UnTag_REF(word), w);
      return TRUE;
    }

#ifndef NO_USE_FD_SOLVER
  if (tag_mask == TAG_FDV_MASK)
    return Fd_Unify_With_Integer(UnTag_FDV(word), UnTag_INT(w));
#endif

  return (word == w);
}




/*-------------------------------------------------------------------------*
 * PL_GET_INTEGER                                                          *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Get_Integer(PlLong n, WamWord start_word)
{
  return Pl_Get_Integer_Tagged(Tag_INT(n), start_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_FLOAT                                                            *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Get_Float(double n, WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
      Bind_UV(UnTag_REF(word), Tag_FLT(H));
      Pl_Global_Push_Float(n);
      return TRUE;
    }

  return (tag_mask == TAG_FLT_MASK && Pl_Obtain_Float(UnTag_FLT(word)) == n);
}




/*-------------------------------------------------------------------------*
 * PL_GET_NIL                                                              *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Get_Nil(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
      Bind_UV(UnTag_REF(word), NIL_WORD);
      return TRUE;
    }

  return (word == NIL_WORD);
}




/*-------------------------------------------------------------------------*
 * PL_GET_LIST                                                             *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Get_List(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
      Bind_UV(UnTag_REF(word), Tag_LST(H));
      S = WRITE_MODE;
      return TRUE;
    }

  if (tag_mask == TAG_LST_MASK)
    {
      S = UnTag_LST(word) + OFFSET_CAR;
      return TRUE;
    }

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_GET_STRUCTURE_TAGGED                                                 *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Get_Structure_Tagged(WamWord w, WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *adr;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
      WamWord *cur_H = H;
      *cur_H = w;
      H++;
      S = WRITE_MODE;
      Bind_UV(UnTag_REF(word), Tag_STC(cur_H));
      return TRUE;
    }


  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);
      if (Functor_And_Arity(adr) != w)
	return FALSE;

      S = adr + OFFSET_ARG;
      return TRUE;
    }

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_GET_STRUCTURE                                                        *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Get_Structure(int func, int arity, WamWord start_word)
{
  return Pl_Get_Structure_Tagged(Functor_Arity(func, arity), start_word);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_X_VARIABLE                                                       *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_X_Variable(void)
{
  WamWord res_word;
  WamWord *cur_H = H;
 
  res_word = Make_Self_Ref(cur_H);
  *cur_H = res_word;
  H++;

  return res_word;
}




/*-------------------------------------------------------------------------*
 * PL_PUT_Y_VARIABLE                                                       *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_Y_Variable(WamWord *y_adr)
{
  return *y_adr = Make_Self_Ref(y_adr);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_UNSAFE_VALUE                                                     *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_Unsafe_Value(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord res_word;

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK &&
      (adr = UnTag_REF(word)) >= (WamWord *) EE(E))
    {
      Globalize_Local_Unbound_Var(adr, res_word);
      return res_word;
    }

  Do_Copy_Of_Word(tag_mask, word);
  return word;
}




/*-------------------------------------------------------------------------*
 * PL_PUT_ATOM_TAGGED                                                      *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_Atom_Tagged(WamWord w)
{
  return w;
}




/*-------------------------------------------------------------------------*
 * PL_PUT_ATOM                                                             *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_Atom(int atom)
{
  return Tag_ATM(atom);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_INTEGER_TAGGED                                                   *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_Integer_Tagged(WamWord w)
{
  return w;
}




/*-------------------------------------------------------------------------*
 * PL_PUT_INTEGER                                                          *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_Integer(PlLong n)
{
  return Tag_INT(n);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_FLOAT                                                            *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_Float(double n)
{
  WamWord res_word;

  res_word = Tag_FLT(H);
  Pl_Global_Push_Float(n);
  return res_word;
}




/*-------------------------------------------------------------------------*
 * PL_PUT_NIL                                                              *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_Nil(void)
{
  return NIL_WORD;
}




/*-------------------------------------------------------------------------*
 * PL_PUT_LIST                                                             *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_List(void)
{
  S = WRITE_MODE;
  return Tag_LST(H);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_STRUCTURE_TAGGED                                                 *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_Structure_Tagged(WamWord w)
{
  WamWord *cur_H = H;
  *cur_H = w;
  H++;
  S = WRITE_MODE;
  return Tag_STC(cur_H);
}




/*-------------------------------------------------------------------------*
 * PL_PUT_STRUCTURE                                                        *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Put_Structure(int func, int arity)
{
  return Pl_Put_Structure_Tagged(Functor_Arity(func, arity));
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_VARIABLE                                                       *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Unify_Variable(void)
{
  WamWord tag_mask, word;
  WamWord res_word;
  WamWord *cur_H;

  if (S != WRITE_MODE)
    {
      word = *S++;
      tag_mask = Tag_Mask_Of(word);
      Do_Copy_Of_Word(tag_mask, word);
      return word;
    }

  cur_H = H;
  res_word = Make_Self_Ref(cur_H);
  *cur_H = res_word;
  H++;

  return res_word;
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_VOID                                                           *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Unify_Void(int n)
{
  WamWord *cur_H;

  if (S != WRITE_MODE)
    {
      S += n;
      return;
    }

  cur_H = H;
  H += n;
  do
    {
      *cur_H = Make_Self_Ref(cur_H);
      cur_H++;
    }
  while(--n > 0);
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_VALUE                                                          *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Unify_Value(WamWord start_word)
{
  if (S != WRITE_MODE)
    return Pl_Unify(start_word, *S++);

  Global_Push(start_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_LOCAL_VALUE                                                    *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Unify_Local_Value(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *adr;

  if (S != WRITE_MODE)
    return Pl_Unify(start_word, *S++);

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK && Is_A_Local_Adr(adr = UnTag_REF(word)))
    Globalize_Local_Unbound_Var(adr, word);
  else
    {
      Do_Copy_Of_Word(tag_mask, word);
      Global_Push(word);
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_ATOM_TAGGED                                                    *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Unify_Atom_Tagged(WamWord w)
{
  WamWord word, tag_mask;

  if (S != WRITE_MODE)
    {
      DEREF(*S, word, tag_mask);
      S++;

      if (tag_mask == TAG_REF_MASK)
	{
	  Bind_UV(UnTag_REF(word), w);
	  return TRUE;
	}

      return (word == w);
    }
      
  Global_Push(w);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_ATOM                                                           *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Unify_Atom(int atom)
{
  return Pl_Unify_Atom_Tagged(Tag_ATM(atom));
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_INTEGER_TAGGED                                                 *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Unify_Integer_Tagged(WamWord w)
{
  WamWord word, tag_mask;

  if (S != WRITE_MODE)
    {
      DEREF(*S, word, tag_mask);
      S++;
      if (tag_mask == TAG_REF_MASK)
	{
	  Bind_UV(UnTag_REF(word), w);
	  return TRUE;
	}

#ifndef NO_USE_FD_SOLVER
      if (tag_mask == TAG_FDV_MASK)
	return Fd_Unify_With_Integer(UnTag_FDV(word), UnTag_INT(w));
#endif

      return (word == w);
    }

  Global_Push(w);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_INTEGER                                                        *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Unify_Integer(PlLong n)
{
  return Pl_Unify_Integer_Tagged(Tag_INT(n));
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_NIL                                                            *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Unify_Nil(void)
{
  WamWord word, tag_mask;

  if (S != WRITE_MODE)
    {
      DEREF(*S, word, tag_mask);
      S++;
      if (tag_mask == TAG_REF_MASK)
	{
	  Bind_UV(UnTag_REF(word), NIL_WORD);
	  return TRUE;
	}
      
      return (word == NIL_WORD);
    }

  Global_Push(NIL_WORD);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_LIST                                                           *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Unify_List(void)
{
  WamWord *cur_H;

  if (S != WRITE_MODE)
    return Pl_Get_List(*S);

  cur_H = H;
  *cur_H = Tag_LST(cur_H + 1);
  H++;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_STRUCTURE_TAGGED                                               *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Unify_Structure_Tagged(WamWord w)
{
  WamWord *cur_H;

  if (S != WRITE_MODE)
    return Pl_Get_Structure_Tagged(w, *S);

  cur_H = H;
  *cur_H = Tag_STC(cur_H + 1);
  cur_H[1] = w;
  H += 2;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY_STRUCTURE                                                      *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Unify_Structure(int func, int arity)
{
  return Pl_Unify_Structure_Tagged(Functor_Arity(func, arity));
}




/*-------------------------------------------------------------------------*
 * PL_GLOBALIZE_IF_IN_LOCAL                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Globalize_If_In_Local(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *adr;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
      adr = UnTag_REF(word);
      if (Is_A_Local_Adr(adr))
	Globalize_Local_Unbound_Var(adr, start_word);
    }

  return start_word;
}




/*-------------------------------------------------------------------------*
 * PL_ALLOCATE                                                             *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Allocate(int n)
{
  WamWord *old_E = E;
  WamWord *cur_E = Local_Top + ENVIR_STATIC_SIZE + n;

  E = cur_E;

  CPE(cur_E) = (WamCont) CP;
  BCIE(cur_E) = BCI;
  EE(cur_E) = (WamWord *) old_E;


#ifdef GARBAGE_COLLECTOR
  NBYE(cur_E) = n;

  cur_E = &Y(cur_E, 0);
  while(n-- > 0)
    {
      *cur_E = Make_Self_Ref(cur_E);
      cur_E--;
    }
#endif
}




/*-------------------------------------------------------------------------*
 * PL_DEALLOCATE                                                           *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Deallocate(void)
{
  WamWord *cur_E = E;

  CP = CPE(cur_E);
  BCI = BCIE(cur_E);
  E = EE(cur_E);
}




/*-------------------------------------------------------------------------*
 * SWITCH_ON_TERM and specialized versions                                 *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
CodePtr FC
Pl_Switch_On_Term(CodePtr c_var, CodePtr c_atm, CodePtr c_int,
	       CodePtr c_lst, CodePtr c_stc)
{
  WamWord word, tag_mask;
  CodePtr codep;

  DEREF(A(0), word, tag_mask);
  A(0) = word;

  if (tag_mask == TAG_INT_MASK)
    codep = c_int;
  else if (tag_mask == TAG_ATM_MASK)
    codep = c_atm;
  else if (tag_mask == TAG_LST_MASK)
    codep = c_lst;
  else if (tag_mask == TAG_STC_MASK)
    codep = c_stc;
  else				/* REF or FDV */
    codep = c_var;

  return (codep) ? codep : ALTB(B);
}




CodePtr FC
Pl_Switch_On_Term_Var_Atm(CodePtr c_var, CodePtr c_atm)
{
  WamWord word, tag_mask;

  DEREF(A(0), word, tag_mask);
  A(0) = word;

  if (tag_mask == TAG_ATM_MASK)
    return c_atm;

  if (tag_mask == TAG_REF_MASK
#ifndef NO_USE_FD_SOLVER
      || tag_mask == TAG_FDV_MASK
#endif
      )
    return c_var;

  return ALTB(B);
}




CodePtr FC
Pl_Switch_On_Term_Var_Stc(CodePtr c_var, CodePtr c_stc)
{
  WamWord word, tag_mask;

  DEREF(A(0), word, tag_mask);
  A(0) = word;

  if (tag_mask == TAG_STC_MASK)
    return c_stc;

  if (tag_mask == TAG_REF_MASK
#ifndef NO_USE_FD_SOLVER
      || tag_mask == TAG_FDV_MASK
#endif
      )
    return c_var;

  return ALTB(B);
}




CodePtr FC
Pl_Switch_On_Term_Var_Atm_Lst(CodePtr c_var, CodePtr c_atm, CodePtr c_lst)
{
  WamWord word, tag_mask;

  DEREF(A(0), word, tag_mask);
  A(0) = word;

  if (tag_mask == TAG_LST_MASK)
    return c_lst;

  if (tag_mask == TAG_ATM_MASK)
    return c_atm;

  if (tag_mask == TAG_REF_MASK
#ifndef NO_USE_FD_SOLVER
      || tag_mask == TAG_FDV_MASK
#endif
      )
    return c_var;

  return ALTB(B);
}




CodePtr FC
Pl_Switch_On_Term_Var_Atm_Stc(CodePtr c_var, CodePtr c_atm, CodePtr c_stc)
{
  WamWord word, tag_mask;

  DEREF(A(0), word, tag_mask);
  A(0) = word;

  if (tag_mask == TAG_STC_MASK)
    return c_stc;

  if (tag_mask == TAG_ATM_MASK)
    return c_atm;

  if (tag_mask == TAG_REF_MASK
#ifndef NO_USE_FD_SOLVER
      || tag_mask == TAG_FDV_MASK
#endif
      )
    return c_var;

  return ALTB(B);
}




/*-------------------------------------------------------------------------*
 * PL_SWITCH_ON_ATOM                                                       *
 *                                                                         *
 * switch_on_atom always occurs after a switch_on_term, thus A(0) is       *
 * dereferenced and has been updated with its deref word.                  *
 * Look in the hash table t and return the adr of the corresponding code.  *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
CodePtr FC
Pl_Switch_On_Atom(SwtTbl t, int size)
{
  SwtInf *swt;

  swt = Locate_Swt_Element(t, size, (PlLong) UnTag_ATM(A(0)));

  return (swt->codep) ? swt->codep : ALTB(B);
}




/*-------------------------------------------------------------------------*
 * PL_SWITCH_ON_INTEGER                                                    *
 *                                                                         *
 * switch_on_integer always occurs after a switch_on_term, thus A(0) is    *
 * dereferenced and has been updated with its deref word.                  *
 * Simply return the integer since the switch is done by the assembly code.*
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
PlLong FC
Pl_Switch_On_Integer(void)
{
  return UnTag_INT(A(0));
}




/*-------------------------------------------------------------------------*
 * PL_SWITCH_ON_STRUCTURE                                                  *
 *                                                                         *
 * switch_on_structure always occurs after a switch_on_term, thus A(0) is  *
 * dereferenced and has been updated with its deref word.                  *
 * Look in the hash table t and return the adr of the corresponding code.  *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
CodePtr FC
Pl_Switch_On_Structure(SwtTbl t, int size)
{
  SwtInf *swt;

  swt = Locate_Swt_Element(t, size, Functor_And_Arity(UnTag_STC(A(0))));

  return (swt->codep) ? swt->codep : ALTB(B);
}




#ifdef USE_WATERMARK

/*-------------------------------------------------------------------------*/
/* CREATE_WATER_MARK                                                       */
/*                                                                         */
/* Request warning if backtracking is not complete.                        */
/*-------------------------------------------------------------------------*/

int        wmark_count = 0;            /* number of active watermarks */
Watermark  wmark[MAXWMARK];            /* The stack of watermarks */
WatermarkP wmp = wmark-1;              /* Top of watermark stack */

void FC
Create_Water_Mark (void (*fun)(), void *par)
{
  if (++wmark_count >= MAXWMARK) {
    Fatal_Error ("Watermark stack size (%d) exceeded! - aborting", MAXWMARK);
  }
  ++wmp;
  wmp->Bvalue     = B;
  wmp->destructor = fun;
  wmp->parameter  = par;
}

void FC
Update_Water_Mark (void (*fun)(), void *par)
{
  if (wmark_count > 0) {
    wmp->destructor = fun;
    wmp->parameter  = par;
  }
  else {
    fprintf(stderr, "warning: Update_Water_Mark called with empty stack!\n");
  }
}

#endif /* USE_WATERMARK */



/*-------------------------------------------------------------------------*
 * PL_GET_CURRENT_CHOICE                                                   *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Get_Current_Choice(void)
{
  return From_B_To_WamWord(B);
}




/*-------------------------------------------------------------------------*
 * PL_CUT                                                                  *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Cut(WamWord b_word)
{
  Assign_B(From_WamWord_To_B(b_word));
}




/*-------------------------------------------------------------------------*
 * PL_SOFT_CUT                                                             *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Soft_Cut(WamWord b_word)
{
  WamWord *kill_B = From_WamWord_To_B(b_word);
  WamWord *cur_B = B;
  WamWord *prev_B;

  /* soft cut: unchain the choice-point pointed by kill_B */

  if (cur_B == kill_B)
    {
      Assign_B(BB(cur_B));
      return;
    }

  for(;;)
    {
      prev_B = BB(cur_B);

      if (prev_B == kill_B)	/* found */
	{
	  BB(cur_B) = BB(kill_B);
	  break;
	}

      if (cur_B < kill_B)	/* not found (can occur at backtracking since already unchained) */
	break;

      cur_B = prev_B;
    }
}




	  /* Auxiliary Functions */




/*-------------------------------------------------------------------------*
 * PL_GLOBAL_PUSH_FLOAT                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Global_Push_Float(double n)
{
  DblInt di;

  di.d = n;
  *H++ = di.i[0];

#if WORD_SIZE == 32
  *H++ = di.i[1];
#endif
}




/*-------------------------------------------------------------------------*
 * PL_OBTAIN_FLOAT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
double FC
Pl_Obtain_Float(WamWord *adr)
{
  DblInt di;

  di.i[0] = adr[0];

#if WORD_SIZE == 32
  di.i[1] = adr[1];
#endif

  return di.d;
}




/*-------------------------------------------------------------------------*
 * CREATE_CHOICE_POINT and specialized versions                            *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/

#define CREATE_CHOICE_COMMON_PART(arity)                   \
  WamWord *old_B = B;                                      \
  WamWord *cur_B = Local_Top + CHOICE_STATIC_SIZE + arity; \
                                                           \
  B = cur_B;                                               \
                                                           \
  ALTB(cur_B) = codep_alt;                                 \
  CPB(cur_B) = CP;                                         \
  BCIB(cur_B) = BCI;                                       \
  EB(cur_B) = E;                                           \
  BB(cur_B) = old_B;                                       \
  HB(cur_B) = HB1 = H;                                     \
  TRB(cur_B) = TR;                                         \
  CSB(cur_B) = CS;                                         \
                                                           \
  STAMP++


				/* common part for update/delete */
				/* restore registers except B and HB1 */
#define UPDATE_DELETE_COMMON_PART \
  WamWord *cur_B = B;             \
                                  \
  Pl_Untrail(TRB(cur_B));         \
                                  \
  CP = CPB(cur_B);                \
  BCI = BCIB(cur_B);              \
  H = HB(cur_B);		  \
  E = EB(cur_B);                  \
  CS = CSB(cur_B)



				/* update ALTB, restore HB1 */
#define UPDATE_CHOICE_COMMON_PART \
  UPDATE_DELETE_COMMON_PART;      \
  ALTB(cur_B) = codep_alt;        \
  HB1 = H


				/* restore B (and HB1), update STAMP */
#define DELETE_CHOICE_COMMON_PART \
  UPDATE_DELETE_COMMON_PART;      \
  Assign_B(BB(cur_B));            \
  STAMP--




void FC
Pl_Create_Choice_Point(CodePtr codep_alt, int arity)
{
  int i;
  CREATE_CHOICE_COMMON_PART(arity);

  for (i = 0; i < arity; i++)
    AB(cur_B, i) = A(i);
}




void FC
Pl_Create_Choice_Point0(CodePtr codep_alt)
{
  CREATE_CHOICE_COMMON_PART(0);
}




void FC
Pl_Create_Choice_Point1(CodePtr codep_alt)
{
  CREATE_CHOICE_COMMON_PART(1);

  AB(cur_B, 0) = A(0);
}




void FC
Pl_Create_Choice_Point2(CodePtr codep_alt)
{
  CREATE_CHOICE_COMMON_PART(2);

  AB(cur_B, 0) = A(0);
  AB(cur_B, 1) = A(1);
}




void FC
Pl_Create_Choice_Point3(CodePtr codep_alt)
{
  CREATE_CHOICE_COMMON_PART(3);

  AB(cur_B, 0) = A(0);
  AB(cur_B, 1) = A(1);
  AB(cur_B, 2) = A(2);
}




void FC
Pl_Create_Choice_Point4(CodePtr codep_alt)
{
  CREATE_CHOICE_COMMON_PART(4);

  AB(cur_B, 0) = A(0);
  AB(cur_B, 1) = A(1);
  AB(cur_B, 2) = A(2);
  AB(cur_B, 3) = A(3);
}




/*-------------------------------------------------------------------------*
 * UPDATE_CHOICE_POINT and specialized versions                            *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/

void FC
Pl_Update_Choice_Point(CodePtr codep_alt, int arity)
{
  int i;
  UPDATE_CHOICE_COMMON_PART;

  for (i = 0; i < arity; i++)
    A(i) = AB(cur_B, i);
}




void FC
Pl_Update_Choice_Point0(CodePtr codep_alt)
{
  UPDATE_CHOICE_COMMON_PART;
}




void FC
Pl_Update_Choice_Point1(CodePtr codep_alt)
{
  UPDATE_CHOICE_COMMON_PART;

  A(0) = AB(cur_B, 0);
}




void FC
Pl_Update_Choice_Point2(CodePtr codep_alt)
{
  UPDATE_CHOICE_COMMON_PART;

  A(0) = AB(cur_B, 0);
  A(1) = AB(cur_B, 1);
}




void FC
Pl_Update_Choice_Point3(CodePtr codep_alt)
{
  UPDATE_CHOICE_COMMON_PART;

  A(0) = AB(cur_B, 0);
  A(1) = AB(cur_B, 1);
  A(2) = AB(cur_B, 2);
}




void FC
Pl_Update_Choice_Point4(CodePtr codep_alt)
{
  UPDATE_CHOICE_COMMON_PART;

  A(0) = AB(cur_B, 0);
  A(1) = AB(cur_B, 1);
  A(2) = AB(cur_B, 2);
  A(3) = AB(cur_B, 3);
}




/*-------------------------------------------------------------------------*
 * PL_DELETE_CHOICE_POINT                                                  *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Delete_Choice_Point(int arity)
{
  int i;
  DELETE_CHOICE_COMMON_PART;

  for (i = 0; i < arity; i++)
    A(i) = AB(cur_B, i);
}




void FC
Pl_Delete_Choice_Point0(void)
{
  DELETE_CHOICE_COMMON_PART;
}




void FC
Pl_Delete_Choice_Point1(void)
{
  DELETE_CHOICE_COMMON_PART;

  A(0) = AB(cur_B, 0);
}




void FC
Pl_Delete_Choice_Point2(void)
{
  DELETE_CHOICE_COMMON_PART;

  A(0) = AB(cur_B, 0);
  A(1) = AB(cur_B, 1);
}




void FC
Pl_Delete_Choice_Point3(void)
{
  DELETE_CHOICE_COMMON_PART;

  A(0) = AB(cur_B, 0);
  A(1) = AB(cur_B, 1);
  A(2) = AB(cur_B, 2);
}




void FC
Pl_Delete_Choice_Point4(void)
{
  DELETE_CHOICE_COMMON_PART;

  A(0) = AB(cur_B, 0);
  A(1) = AB(cur_B, 1);
  A(2) = AB(cur_B, 2);
  A(3) = AB(cur_B, 3);
}




/*-------------------------------------------------------------------------*
 * PL_DEFEASIBLE_OPEN                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Defeasible_Open()
{
  Pl_Create_Choice_Point0(NULL);
}




/*-------------------------------------------------------------------------*
 * PL_DEFEASIBLE_UNDO                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Defeasible_Undo()
{
  Pl_Update_Choice_Point0(NULL);
}




/*-------------------------------------------------------------------------*
 * PL_DEFEASIBLE_CLOSE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Defeasible_Close(Bool success) 
{
  if (success)
    Assign_B(BB(B));  /* like a cut */  
  else
    Pl_Delete_Choice_Point0();  /* untrail */
}




/*-------------------------------------------------------------------------*
 * PL_UNTRAIL                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Untrail(WamWord *low_adr)
{
  WamWord word;
  WamWord *adr;
  int nb;

  while (TR > low_adr)
    {
      word = Trail_Pop;
      adr = (WamWord *) (Trail_Value_Of(word));

      switch (Trail_Tag_Of(word))
	{
	case TUV:
	  *adr = Make_Self_Ref(adr);
	  break;

	case TOV:
	  *adr = Trail_Pop;
	  break;

	case TMV:
	  nb = (int) Trail_Pop;
	  TR -= nb;
	  Mem_Word_Cpy(adr, TR, nb);
	  break;

	default:		/* TFC */
	  adr = (WamWord *) Trail_Pop; /* fct adr no longer word aligned */
	  nb = (int) Trail_Pop;
	  TR -= nb;
	  (*((int (*)()) adr)) (nb, TR);
	}
    }
}




/*-------------------------------------------------------------------------*
 * PL_UNIFY                                                                *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
#define UNIFY_FCT_NAME Pl_Unify

#include "unify.c"




/*-------------------------------------------------------------------------*
 * PL_UNIFY_OCCURS_CHECK                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
#undef  UNIFY_FCT_NAME
#define UNIFY_FCT_NAME Pl_Unify_Occurs_Check

#define OCCURS_CHECK

#include "unify.c"



/* --- Contexts --- */


#define cur_K X(255)
#define cur_CK X(254)
static WamWord stamp_K;
static WamWord stamp_CK;

WamCont Cxt_Call_Tagged(WamWord key, WamWord call_K) FC;

WamCont Cxt_Call(int func, int arity, WamWord call_K) FC;

void Cxt_Assign_K(WamWord new_K) FC;

Bool Cxt_Arg_Load(int arg_no, WamWord term_word, WamWord *sub_term_word) FC;

Bool Cxt_Arg_Unify(int arg_no, WamWord term_word, WamWord sub_term_word) FC;




/*-------------------------------------------------------------------------*
 * CXT_INIT                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Cxt_Init(void)
{
  cur_K = NIL_WORD;
  cur_CK = NIL_WORD;
  stamp_K = 0;
  stamp_CK = 0;
}




/*-------------------------------------------------------------------------*
 * CXT_ASSIGN_K                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Cxt_Assign_K(WamWord new_K)
{
  if (stamp_K != STAMP)
    {
      if (new_K == cur_K)
	return;

      Trail_OV(&cur_K);
      Trail_OV(&stamp_K);
      stamp_K = STAMP;
    }
  cur_K = new_K;
}




/*-------------------------------------------------------------------------*
 * CXT_ASSIGN_CK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Cxt_Assign_CK(WamWord new_CK)
{
  if (stamp_CK != STAMP)
    {
      if (new_CK == cur_CK)
	return;

      Trail_OV(&cur_CK);
      Trail_OV(&stamp_CK);
      stamp_CK = STAMP;
    }
  cur_CK = new_CK;
}




/*-------------------------------------------------------------------------*
 * CXT_LOOKUP_PRED_WITH_K                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PredInf * FC
Cxt_Lookup_Pred_With_K(long key, WamWord cxt_call_K)
{
  WamWord call_K = cxt_call_K;
  WamWord word, tag_mask;
  WamWord *lst_adr;
  int atom, arity;
  char *ctable;
  PredInf *pred;

  Cxt_Assign_CK(cxt_call_K);

#if 1

  for (;;)
    {
      DEREF(call_K, word, tag_mask);

      if (tag_mask == TAG_STC_MASK) { /* non-list context */
	atom = Functor(UnTag_STC(word));
	arity = Arity(UnTag_STC(word));
	lst_adr = NULL;
      }
      else if (tag_mask != TAG_LST_MASK)
	break;
      else {
	lst_adr = UnTag_LST(word);
	DEREF(Car(lst_adr), word, tag_mask);

	if (tag_mask == TAG_ATM_MASK) {
	  atom = UnTag_ATM(word);
	  arity = 0;
	}
	else if (tag_mask == TAG_STC_MASK) {
	  atom = Functor(UnTag_STC(word));
	  arity = Arity(UnTag_STC(word));
	}
	else
	  goto next;		/* or display an error ? */
      }

      if (pl_atom_tbl[atom].modules &&
	  (ctable = pl_atom_tbl[atom].modules[arity]) &&
	  (pred = (PredInf *) Pl_Hash_Find(ctable, key)))
	{
	  Cxt_Assign_K(call_K);
	  return pred;
	}
    next:
      if (lst_adr)
	call_K = Cdr(lst_adr);
      else
	break;
    }
				/* IS IT CORRECT ??? */
  Cxt_Assign_K(cxt_call_K);
  return (PredInf *) Pl_Hash_Find(pl_pred_tbl, key);

#else  /* FIXME: TRY THIS (lookup in global first) LATER... */

  if ((pred = (PredInf *) Hash_Find(pred_tbl, key))) {
    printf ("Cxt_Lookup_Pred_With_K(%s/%d): found global\n",
	    atom_tbl[Functor_Of(key)].name,
	    Arity_Of(key));
    return pred;
  }

  for (;;) {
    DEREF(call_K, word, tag_mask);

    if (tag_mask != TAG_LST_MASK)
      break;

    lst_adr = UnTag_LST(word);
    DEREF(Car(lst_adr), word, tag_mask);

    if (tag_mask == TAG_STC_MASK) { /* non-atomic: regular case */
      atom = Functor(UnTag_STC(word));
      arity = Arity(UnTag_STC(word));
    }
    else if (tag_mask == TAG_ATM_MASK) { /* atomic: like ATOM/0 */
      atom = UnTag_ATM(word);
      arity = 0;
    }
    else
      goto next;		/* or display an error ? */

    if (atom_tbl[atom].modules &&
	(ctable = atom_tbl[atom].modules[arity]) &&
	(pred = (PredInf *) Hash_Find(ctable, key)))
      {			/* found it */
	Cxt_Assign_K(call_K);
	printf ("Cxt_Lookup_Pred_With_K(%s/%d): found in %s/%d\n",
		atom_tbl[Functor_Of(key)].name,
		Arity_Of(key),
		atom_tbl[atom].name,
		arity);
	return pred;
      }
  next:
    call_K = Cdr(lst_adr);
  }
				/* IS IT CORRECT ??? */
  Cxt_Assign_K(cxt_call_K);
  return (PredInf *) Hash_Find(pred_tbl, key);

#endif
}




/*-------------------------------------------------------------------------*
 * CXT_LOOKUP_PRED                                                         *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
PredInf * FC
Cxt_Lookup_Pred(long key)
{
  return Cxt_Lookup_Pred_With_K(key, cur_K);
}

#include "../BipsPl/error_supp.h"


/*-------------------------------------------------------------------------*
 * CXT_CALL_TAGGED                                                         *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamCont FC
Cxt_Call_Tagged(WamWord key, WamWord cxt_call_K)
{
  PredInf *pred = Cxt_Lookup_Pred_With_K(key, cxt_call_K);
  WamCont Pl_BC_Emulate_Pred(int, void *);

  if (pred == NULL)
    {
      WamWord call_K = cxt_call_K;
      static char pseudo_bip_name[512];
      char *p = pseudo_bip_name;
//    int n = 0;
//    int l = sizeof (pseudo_bip_name);
      char *sep = "";
      WamWord *lst_adr;
      WamWord word, tag_mask;

      strcpy(pseudo_bip_name, "context([");
      p += strlen(p);
      for (;;) {
	DEREF(call_K, word, tag_mask);

	if (tag_mask != TAG_LST_MASK)
	  break;

	lst_adr = UnTag_LST(word);
	DEREF(Car(lst_adr), word, tag_mask);

	if (tag_mask == TAG_ATM_MASK)
	  sprintf (p, "%s%s/%d", sep, pl_atom_tbl[UnTag_ATM(word)].name, 0);
	else if (tag_mask == TAG_STC_MASK)
	  sprintf (p, "%s%s/%ld", sep,
		   pl_atom_tbl[Functor(UnTag_STC(word))].name,
		   Arity(UnTag_STC(word)));
	else
	  goto next;		/* or display an error ? */

	p += strlen(p);
	sep = ", ";

      next:
	call_K = Cdr(lst_adr);
      }
      sprintf (p, "])");
      
      Pl_Set_C_Bip_Name(pseudo_bip_name, -1); // FIXME: properly dump context
      Pl_Unknown_Pred_Error(Functor_Of(key), Arity_Of(key));

      return ALTB(B);		/* i.e. fail */
    }

  if (pred->codep)
    return (WamCont) pred->codep;
  else if (pred->dyn)
    return Pl_BC_Emulate_Pred ((int) key, pred->dyn);
  else
    return ALTB(B);		/* fail for dynamic w/o any clauses */
}




/*-------------------------------------------------------------------------*
 * CXT_CALL                                                                *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamCont FC
Cxt_Call(int func, int arity, WamWord cxt_call_K)
{
  return Cxt_Call_Tagged(Functor_Arity(func, arity), cxt_call_K);
}




/*-------------------------------------------------------------------------*
 * CXT_ARG_LOAD                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Cxt_Arg_Load(int arg_no, WamWord term_word, WamWord *sub_term_adr)
{
  WamWord word, tag_mask;
  WamWord *adr;

  DEREF(term_word, word, tag_mask);

  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);
      if ((unsigned) arg_no >= (unsigned) Arity(adr))
	return FALSE;
      *sub_term_adr = Arg(adr, arg_no);
      return TRUE;
    }

  if (tag_mask == TAG_LST_MASK)
    {
      adr = UnTag_LST(word);
      if ((unsigned) arg_no >= 2)
	return FALSE;
      *sub_term_adr = (arg_no == 0) ? Car(adr) : Cdr(adr);
      return TRUE;
    }

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * CXT_ARG_UNIFY                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Cxt_Arg_Unify(int arg_no, WamWord term_word, WamWord sub_term_word)
{
  WamWord word;

  if (!Cxt_Arg_Load(arg_no, term_word, &word))
    return FALSE;

  return Pl_Unify(sub_term_word, word);
}
