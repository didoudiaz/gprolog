/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog engine                                                   */
/* File  : wam_inst.c                                                      */
/* Descr.: WAM instruction implementation                                  */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>

#include "engine_pl.h"




/*---------------------------------*/
/* Constants                       */
/*---------------------------------*/

/*---------------------------------*/
/* Type Definitions                */
/*---------------------------------*/

typedef union
    {
     double  d;
     WamWord i[2];
    }DblInt;




/*---------------------------------*/
/* Global Variables                */
/*---------------------------------*/

/*---------------------------------*/
/* Function Prototypes             */
/*---------------------------------*/

static 
SwtInf   *Locate_Swt_Element    (SwtTbl t,int size,long key);




/*-------------------------------------------------------------------------*/
/* CREATE_SWT_TABLE                                                        */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
SwtTbl Create_Swt_Table(int size)

{
 SwtTbl t;

 size++;                         /* +1 to ensure that one free cell exists */

 t=(SwtTbl) Calloc(size,sizeof(SwtInf));

 return t;
}




/*-------------------------------------------------------------------------*/
/* CREATE_SWT_ATM_ELEMENT                                                  */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
void Create_Swt_Atm_Element(SwtTbl t,int size,int atom,CodePtr codep)

{
 SwtInf *swt=Locate_Swt_Element(t,size,atom);

 swt->key  =atom;
 swt->codep=codep;
}




/*-------------------------------------------------------------------------*/
/* CREATE_SWT_STC_ELEMENT                                                  */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
void Create_Swt_Stc_Element(SwtTbl t,int size,int func,int arity,
                            CodePtr codep)

{
 long key=Functor_Arity(func,arity);

 SwtInf *swt=Locate_Swt_Element(t,size,key);

 swt->key  =key;
 swt->codep=codep;
}



/*-------------------------------------------------------------------------*/
/* LOCATE_SWT_ELEMENT                                                      */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static SwtInf *Locate_Swt_Element(SwtTbl t,int size,long key)

{
 int     n;
 SwtInf *swt,*endt;

 size++;                         /* +1 to ensure that one free cell exists */

#if 1
 n=key % size;
#else
 n=(key ^ ((unsigned long) key>>16)) % size;
#endif
                                    /* here either the key is in the table */
                                    /* or there is at least one free cell. */
 swt =t+n;
 endt=t+size;

 while(swt->codep && swt->key!=key)
    {
     swt++;
     if (swt==endt)
         swt=t;
    }

 return swt;
}




/*-------------------------------------------------------------------------*/
/* GET_ATOM                                                                */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Get_Atom(int atom,WamWord start_word)

{
 WamWord word,tag,*adr;

 Deref(start_word,word,tag,adr)
 switch(tag)
    {
     case REF:
         Bind_UV(adr,Tag_Value(ATM,atom))
         return TRUE;

     case ATM:
         return (UnTag_ATM(word)==atom);
    }

 return FALSE;
}




/*-------------------------------------------------------------------------*/
/* GET_INTEGER                                                             */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Get_Integer(int n,WamWord start_word)

{
 WamWord word,tag,*adr;

 Deref(start_word,word,tag,adr)
 switch(tag)
    {
     case INT:
         return (UnTag_INT(word)==n);

     case REF:
         Bind_UV(adr,Tag_Value(INT,n))
         return TRUE;

     case FDV:
         return Fd_Unify_With_Integer(UnTag_FDV(word),n);
    }

 return FALSE;
}




/*-------------------------------------------------------------------------*/
/* GET_FLOAT                                                               */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Get_Float(double n,WamWord start_word)

{
 WamWord word,tag,*adr;

 Deref(start_word,word,tag,adr)
 switch(tag)
    {
     case REF:
         Bind_UV(adr,Tag_Value(FLT,H))
         Global_Push_Float(n);
         return TRUE;

     case FLT:
         return (Obtain_Float(UnTag_FLT(word))==n);
    }

 return FALSE;
}




/*-------------------------------------------------------------------------*/
/* GET_NIL                                                                 */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Get_Nil(WamWord start_word)

{
 WamWord word,tag,*adr;

 Deref(start_word,word,tag,adr)
 if (tag==REF)
    {
     Bind_UV(adr,NIL_WORD)
     return TRUE;
    }

 return (word==NIL_WORD);
}




/*-------------------------------------------------------------------------*/
/* GET_LIST                                                                */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Get_List(WamWord start_word)

{
 WamWord word,tag,*adr;

 Deref(start_word,word,tag,adr)
 switch(tag)
    {
     case REF:
         Bind_UV(adr,Tag_Value(LST,H))
         S=WRITE_MODE; 
         return TRUE;

     case LST:                                   /* init S, i.e. MODE=READ */
         S=(WamWord *) UnTag_LST(word)+OFFSET_CAR;
         return TRUE;	 
    }

 return FALSE;
}




/*-------------------------------------------------------------------------*/
/* GET_STRUCTURE                                                           */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Get_Structure(int func,int arity,WamWord start_word)

{
 WamWord word,tag,*adr;

 Deref(start_word,word,tag,adr)
 switch(tag)
    {
     case REF:
         Bind_UV(adr,Tag_Value(STC,H))
         Global_Push(Functor_Arity(func,arity));
         S=WRITE_MODE;
	 return TRUE;

     case STC:                                   /* init S, i.e. MODE=READ */
         adr=UnTag_STC(word);
         if (Functor_And_Arity(adr)!=Functor_Arity(func,arity))
             return FALSE;

         S=adr+OFFSET_ARG;
	 return TRUE;
    }

 return FALSE;
}




/*-------------------------------------------------------------------------*/
/* PUT_X_VARIABLE                                                          */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
WamWord Put_X_Variable(void)

{
 WamWord res_word;

 res_word=Make_Self_Ref(H);
 Global_Push(res_word);
 return res_word;
}




/*-------------------------------------------------------------------------*/
/* PUT_Y_VARIABLE                                                          */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
WamWord Put_Y_Variable(WamWord *y_adr)

{
 return *y_adr=Make_Self_Ref(y_adr);
}




/*-------------------------------------------------------------------------*/
/* PUT_UNSAFE_VALUE                                                        */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
WamWord Put_Unsafe_Value(WamWord start_word)

{
 WamWord word,tag,*adr;
 WamWord res_word;

 Deref(start_word,word,tag,adr)

 if (tag==REF && adr>=(WamWord *) EE(E))
    {
     res_word=Tag_Value(REF,H);
     Globalize_Local_Unbound_Var(adr)
    }
  else
     res_word=Make_Copy_Of_Word(tag,word);

 return res_word;
}




/*-------------------------------------------------------------------------*/
/* PUT_ATOM                                                                */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
WamWord Put_Atom(int atom)

{
 return Tag_Value(ATM,atom);
}




/*-------------------------------------------------------------------------*/
/* PUT_INTEGER                                                             */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
WamWord Put_Integer(int n)

{
 return Tag_Value(INT,n);
}




/*-------------------------------------------------------------------------*/
/* PUT_FLOAT                                                               */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
WamWord Put_Float(double n)

{
 WamWord res_word;

 res_word=Tag_Value(FLT,H);
 Global_Push_Float(n);
 return res_word;
}




/*-------------------------------------------------------------------------*/
/* PUT_NIL                                                                 */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
WamWord Put_Nil(void)

{
 return NIL_WORD;
}




/*-------------------------------------------------------------------------*/
/* PUT_LIST                                                                */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
WamWord Put_List(void)

{
 S=WRITE_MODE; 
 return Tag_Value(LST,H);
}




/*-------------------------------------------------------------------------*/
/* PUT_STRUCTURE                                                           */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
WamWord Put_Structure(int func,int arity)

{
 WamWord res_word;

 res_word=Tag_Value(STC,H);
 Global_Push(Functor_Arity(func,arity));
 S=WRITE_MODE;
 return res_word;
}




/*-------------------------------------------------------------------------*/
/* UNIFY_VARIABLE                                                          */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
WamWord Unify_Variable(void)

{
 WamWord tag,word;

 if (S!=WRITE_MODE)
    {
     tag=Tag_Of(word= *S);
     S++;
     return Make_Copy_Of_Word(tag,word);
    }


 word=Make_Self_Ref(H);
 Global_Push(word);
 return word;
}




/*-------------------------------------------------------------------------*/
/* UNIFY_VOID                                                              */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
void Unify_Void(int n)

{
 WamWord *end_adr;

 if (S!=WRITE_MODE)
     S+=n;
  else
     for(end_adr=H+(n);H<end_adr;++H)
         *H=Make_Self_Ref(H);
}




/*-------------------------------------------------------------------------*/
/* UNIFY_VALUE                                                             */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Unify_Value(WamWord start_word)

{
 if (S!=WRITE_MODE)
    {
     if (!Unify(start_word,*S))
         return FALSE;
     S++;
     return TRUE;
    }

 Global_Push(start_word);
 return TRUE;
}




/*-------------------------------------------------------------------------*/
/* UNIFY_LOCAL_VALUE                                                       */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Unify_Local_Value(WamWord start_word)

{
 WamWord word,tag,*adr;

 if (S!=WRITE_MODE)
     return Unify(start_word,*S++);

 Deref(start_word,word,tag,adr)

 if (tag==REF && Is_A_Local_Adr(adr))
     Globalize_Local_Unbound_Var(adr)
  else
     Global_Push(Make_Copy_Of_Word(tag,word));

 return TRUE;
}




/*-------------------------------------------------------------------------*/
/* UNIFY_ATOM                                                              */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Unify_Atom(int atom)

{
 WamWord word,tag,*adr;

 if (S!=WRITE_MODE)
    {
     Deref(*S,word,tag,adr)
     S++;
     switch(tag)
        {
         case REF:
             Bind_UV(adr,Tag_Value(ATM,atom))
	     return TRUE;

         case ATM:
	     return (UnTag_ATM(word)==atom);
        }

     return FALSE;
    }

 Global_Push(Tag_Value(ATM,atom));

 return TRUE;
}




/*-------------------------------------------------------------------------*/
/* UNIFY_INTEGER                                                           */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Unify_Integer(int n)

{
 WamWord word,tag,*adr;

 if (S!=WRITE_MODE)
    {
     Deref(*S,word,tag,adr)
     S++;     
     switch(tag)
        {
         case INT:
             return (UnTag_INT(word)==n);

         case REF:
             Bind_UV(adr,Tag_Value(INT,n))
             return TRUE;

         case FDV:
             return Fd_Unify_With_Integer(UnTag_FDV(word),n);
        }

     return FALSE;
    }

 Global_Push(Tag_Value(INT,n));

 return TRUE;
}




/*-------------------------------------------------------------------------*/
/* UNIFY_NIL                                                               */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Unify_Nil(void)

{
 WamWord word,tag,*adr;

 if (S!=WRITE_MODE)
    {
     Deref(*S,word,tag,adr)
     S++;
     if (tag==REF)
        {
         Bind_UV(adr,NIL_WORD)
         return TRUE;
        }
      else
         return (word==NIL_WORD);
    }

 Global_Push(NIL_WORD);

 return TRUE;
}




/*-------------------------------------------------------------------------*/
/* UNIFY_LIST                                                              */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Unify_List(void)

{
 WamWord *adr;

 if (S!=WRITE_MODE)
     return Get_List(*S);

 adr=H++;
 *adr=Put_List();

 return TRUE;
}




/*-------------------------------------------------------------------------*/
/* UNIFY_STRUCTURE                                                         */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
Bool Unify_Structure(int func,int arity)

{
 WamWord *adr;

 if (S!=WRITE_MODE)
     return Get_Structure(func,arity,*S);

 adr=H++;
 *adr=Put_Structure(func,arity);

 return TRUE;
}




/*-------------------------------------------------------------------------*/
/* ALLOCATE                                                                */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
void Allocate(int n)

{
 WamWord *adr;

 adr=E;
 E=Local_Top+ENVIR_STATIC_SIZE+n;

 CPE(E) =(WamCont) CP;
 BCIE(E)=BCI;
 EE(E)  =(WamWord *) adr;
}




/*-------------------------------------------------------------------------*/
/* DEALLOCATE                                                              */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
void Deallocate(void)

{
 CP =CPE(E);
 BCI=BCIE(E);
 E  =EE(E);                 /* Warning E must be the last element restored */
}




/*-------------------------------------------------------------------------*/
/* SWITCH_ON_TERM                                                          */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
CodePtr Switch_On_Term(CodePtr c_var,CodePtr c_atm,CodePtr c_int,
                       CodePtr c_lst,CodePtr c_stc)

{
 WamWord word,tag,*adr;
 CodePtr codep;

 Deref(A(0),word,tag,adr)
 A(0)=word;
 switch(tag)
    {
     case INT:
         codep=c_int;
	 break;

     case ATM:
         codep=c_atm;
	 break;

     case LST:
         codep=c_lst;
	 break;

     case STC:
         codep=c_stc;
	 break;

     default:                                                /* REF or FDV */
         codep=c_var;
	 break;
    }

 return (codep) ? codep : ALTB(B);
}




/*-------------------------------------------------------------------------*/
/* SWITCH_ON_ATOM                                                          */
/*                                                                         */
/* switch_on_atom always occurs after a switch_on_term, thus A(0) is       */
/* dereferenced and has been updated with its deref word.                  */
/* Look in the hash table t and return the adr of the corresponding code.  */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
CodePtr Switch_On_Atom(SwtTbl t,int size)

{
 SwtInf *swt;

 swt=Locate_Swt_Element(t,size,(long) UnTag_ATM(A(0)));

 return (swt->codep) ? swt->codep : ALTB(B);
}




/*-------------------------------------------------------------------------*/
/* SWITCH_ON_INTEGER                                                       */
/*                                                                         */
/* switch_on_integer always occurs after a switch_on_term, thus A(0) is    */
/* dereferenced and has been updated with its deref word.                  */
/* Simply return the integer since the switch is done by the assembly code.*/
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
int Switch_On_Integer(void)

{
 return UnTag_INT(A(0));
}




/*-------------------------------------------------------------------------*/
/* SWITCH_ON_STRUCTURE                                                     */
/*                                                                         */
/* switch_on_structure always occurs after a switch_on_term, thus A(0) is  */
/* dereferenced and has been updated with its deref word.                  */
/* Look in the hash table t and return the adr of the corresponding code.  */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
CodePtr Switch_On_Structure(SwtTbl t,int size)

{
 SwtInf *swt;
             
 swt=Locate_Swt_Element(t,size,Functor_And_Arity(UnTag_STC(A(0))));

 return (swt->codep) ? swt->codep : ALTB(B);
}




/*-------------------------------------------------------------------------*/
/* LOAD_CUT_LEVEL                                                          */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
void Load_Cut_Level(WamWord *word_adr)

{
 *word_adr=Tag_Value(INT,B);
}




/*-------------------------------------------------------------------------*/
/* CUT                                                                     */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
void Cut(WamWord b_word)

{
 B=UnTag_REF(b_word);
}




          /* Auxiliary Functions */




/*-------------------------------------------------------------------------*/
/* GLOBAL_PUSH_FLOAT                                                       */
/*                                                                         */
/*-------------------------------------------------------------------------*/
void Global_Push_Float(double n)

{
 DblInt di;

 di.d=n;
 *H++=di.i[0];

#if WORD_SIZE==32
 *H++=di.i[1];
#endif
}




/*-------------------------------------------------------------------------*/
/* OBTAIN_FLOAT                                                            */
/*                                                                         */
/*-------------------------------------------------------------------------*/
double Obtain_Float(WamWord *adr)

{
 DblInt di;

 di.i[0]=adr[0];

#if WORD_SIZE==32
 di.i[1]=adr[1];
#endif

 return di.d;
}




/*-------------------------------------------------------------------------*/
/* CREATE_CHOICE_POINT                                                     */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
void Create_Choice_Point(CodePtr codep_alt,int arity)

{
 WamWord *adr;
 int      i;


 adr=B;
 B=Local_Top+CHOICE_STATIC_SIZE+arity;

 ALTB(B)=codep_alt;
 CPB(B) =CP;
 BCIB(B)=BCI;
 EB(B)  =E;
 BB(B)  =adr;
 HB(B)  =H;
 TRB(B) =TR;
 CSB(B) =CS;

 for(i=0;i<arity;i++)
     AB(B,i)=A(i);

 STAMP++;
}




/*-------------------------------------------------------------------------*/
/* UPDATE_CHOICE_POINT                                                     */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
void Update_Choice_Point(CodePtr codep_alt,int arity)

{
 int i;


 ALTB(B)=codep_alt;

 Untrail(TRB(B));

 CP =CPB(B);
 BCI=BCIB(B);
 E  =EB(B);
 H  =HB(B);
 CS =CSB(B);

 for(i=0;i<arity;i++)
     A(i)=AB(B,i);
}




/*-------------------------------------------------------------------------*/
/* DELETE_CHOICE_POINT                                                     */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
void Delete_Choice_Point(int arity)

{
 int i;


 Untrail(TRB(B));

 CP =CPB(B);
 E  =EB(B);
 H  =HB(B);
 CS =CSB(B);
 BCI=BCIB(B);

 for(i=0;i<arity;i++)
     A(i)=AB(B,i);

 B=BB(B);                   /* warning B must be the last element restored */

 STAMP--;
}





/*-------------------------------------------------------------------------*/
/* UNTRAIL                                                                 */
/*                                                                         */
/*-------------------------------------------------------------------------*/
void Untrail(WamWord *low_adr)

{
 WamWord  word;
 WamWord *adr;
 int      nb;


 while(TR>low_adr)
    {
     word=Trail_Pop;
     adr=(WamWord *) (Trail_Value_Of(word));

     switch(Trail_Tag_Of(word))
        {
         case TUV:
             *adr=Make_Self_Ref(adr);
             break;

         case TOV:
             *adr=Trail_Pop;
             break;

         case TMV:
             nb=Trail_Pop;
             TR-=nb;
             Mem_Word_Cpy(adr,TR,nb)
             break;

         default:                                                   /* TFC */
             (*  ((int (*)()) adr)) ();
        }
    }
}




/*-------------------------------------------------------------------------*/
/* MAKE_COPY_OF_WORD                                                       */
/*                                                                         */
/*-------------------------------------------------------------------------*/
WamWord Make_Copy_Of_Word(int tag,WamWord word)

{
 WamWord *adr;

 if (Dont_Separate_Tag(tag))
    {
     adr=UnTag_REF(word);                       /* in fact UnTag_XXX(word) */
     word=Tag_Value(REF,adr);
    }

 return word;
}




/*-------------------------------------------------------------------------*/
/* UNIFY                                                                   */
/*                                                                         */
/* Called by compiled prolog code.                                         */
/*-------------------------------------------------------------------------*/
#define UNIFY_FCT_NAME Unify

#include "unify.c"




/*-------------------------------------------------------------------------*/
/* UNIFY_OCCURS_CHECK                                                      */
/*                                                                         */
/*-------------------------------------------------------------------------*/
#undef  UNIFY_FCT_NAME
#define UNIFY_FCT_NAME Unify_Occurs_Check

#define OCCURS_CHECK

#include "unify.c"
