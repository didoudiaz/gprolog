/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : wam_inst.h                                                      *
 * Descr.: WAM instruction implementation - header file                    *
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

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define NOT_A_WAM_WORD             Tag_Value(-1,-1)

#define NIL_WORD                   Tag_Value(ATM,ATOM_NIL)




	  /* Read/Write Modes */

	  /* if S==NULL iff we are in the write mode */

#define WRITE_MODE                 NULL




	  /* Environment Frame */

#define ENVIR_STATIC_SIZE          3

#define CPE(e)                     ((WamCont)   (e[-1]))
#define BCIE(e)                    ((WamWord)   (e[-2]))
#define EE(e)                      ((WamWord *) (e[-3]))
#define Y(e,y)                     ((WamWord)   (e[-4-(y)]))

#define ENVIR_NAMES                {"CPE","BCIE","EE"}




	  /* Choice Point Frame */

#define CHOICE_STATIC_SIZE         8

#define ALTB(b)                    ((CodePtr)   (b[-1]))
#define CPB(b)                     ((WamCont)   (b[-2]))
#define BCIB(b)                    ((WamWord)   (b[-3]))
#define EB(b)                      ((WamWord *) (b[-4]))
#define BB(b)                      ((WamWord *) (b[-5]))
#define HB(b)                      ((WamWord *) (b[-6]))
#define TRB(b)                     ((WamWord *) (b[-7]))
#define CSB(b)                     ((WamWord *) (b[-8]))
#define AB(b,a)                    ((WamWord)   (b[-9-(a)]))

#define CHOICE_NAMES               {"ALTB","CPB","BCIB","EB","BB","HB",     \
                                    "TRB","CSB"}




	  /* Wam Objects Manipulation */

	  /* Trail Tags */

#define NB_OF_TRAIL_TAGS           4

#define TUV                        0	/* Trail Unbound Variable   */
#define TOV                        1	/* Trail One Value          */
#define TMV                        2	/* Trail Multiple Values    */
#define TFC                        3	/* Trail for Function Call  */


#define TRAIL_TAG_NAMES            {"TUV","TOV","TMV","TFC"}


#define Trail_Tag_Value(t,v)       ((unsigned long) (v) | (t))
#define Trail_Tag_Of(w)            ((unsigned long) (w) & 0x3)
#define Trail_Value_Of(w)          ((unsigned long) (w) & (~0x3))




	  /* Functor/arity */

#define Functor_Arity(f,n)         (((n) << ATOM_SIZE) + (f))
#define Functor_Of(word)           ((word) & (MAX_ATOM-1))
#define Arity_Of(word)             ((word) >> ATOM_SIZE)


#define Dont_Separate_Tag(t)       ((t)==FDV)




	  /* Unbound Variables */

#define Make_Self_Ref(adr)         (Tag_Value(REF,adr))




	  /* Atom */




	  /* Integer */

#define INT_GREATEST_VALUE         ((long) ((1L<<(WORD_SIZE-TAG_SIZE-1))-1))
#define INT_LOWEST_VALUE           ((long) ((-INT_GREATEST_VALUE)-1))




	  /* List */

#define OFFSET_CAR                 0

#define Car(adr)                   (((WamWord *) adr)[OFFSET_CAR])
#define Cdr(adr)                   (((WamWord *) adr)[OFFSET_CAR+1])




	  /* Structure */

#define OFFSET_ARG                 1

#define Functor(adr)               (Functor_Of(Functor_And_Arity(adr)))
#define Arity(adr)                 (Arity_Of(Functor_And_Arity(adr)))
#define Functor_And_Arity(adr)     (((WamWord *) (adr))[0])
#define Arg(adr,i)                 (((WamWord *) (adr))[OFFSET_ARG+i])
							/* i in 0..arity-1 */




	  /* Stacks */

#define Global_Push(word)          (*H++=(WamWord) (word))

#define Global_Pop                 (*--H)




#define Trail_Push(word)           (*TR++=(WamWord) (word))

#define Trail_Pop                  (*--TR)




#define Is_A_Local_Adr(adr)        ((adr)>=Local_Stack)




	  /* CP management */

#ifdef M_sparc

#define Adjust_CP(cp)              ((WamCont) ((unsigned long) (cp)-8))
#define UnAdjust_CP(cp)            ((WamCont) ((unsigned long) (cp)+8))

#else

#define Adjust_CP(p)               ((WamCont) (p))
#define UnAdjust_CP(cp)            (cp)

#endif




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct			/* Switch item information         */
{				/* ------------------------------- */
  long key;			/* key: atm, int (if no_opt), f/n  */
  CodePtr codep;		/* compiled code pointer if static */
}
SwtInf;

typedef SwtInf *SwtTbl;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

SwtTbl Create_Swt_Table(int size) FC;

void Create_Swt_Atm_Element(SwtTbl t, int size, int atom, CodePtr codep) FC;

void Create_Swt_Stc_Element(SwtTbl t, int size, int func, int arity,
			    CodePtr codep) FC;

Bool Get_Atom(int atom, WamWord start_word) FC;

Bool Get_Integer(long n, WamWord start_word) FC;

Bool Get_Float(double n, WamWord start_word) FC;

Bool Get_Nil(WamWord start_word) FC;

Bool Get_List(WamWord start_word) FC;

Bool Get_Structure(int func, int arity, WamWord start_word) FC;

WamWord Put_X_Variable(void) FC;

WamWord Put_Y_Variable(WamWord *y_adr) FC;

WamWord Put_Unsafe_Value(WamWord start_word) FC;

WamWord Put_Atom(int atom) FC;

WamWord Put_Integer(long n) FC;

WamWord Put_Float(double n) FC;

WamWord Put_Nil(void) FC;

WamWord Put_List(void) FC;

WamWord Put_Structure(int func, int arity) FC;

WamWord Unify_Variable(void) FC;

void Unify_Void(int n) FC;

Bool Unify_Value(WamWord start_word) FC;

Bool Unify_Local_Value(WamWord start_word) FC;

Bool Unify_Atom(int atom) FC;

Bool Unify_Integer(long n) FC;

Bool Unify_Nil(void) FC;

Bool Unify_List(void) FC;

Bool Unify_Structure(int func, int arity) FC;

void Allocate(int n) FC;

void Deallocate(void) FC;

CodePtr Switch_On_Term(CodePtr c_var, CodePtr c_atm, CodePtr c_int,
		       CodePtr c_lst, CodePtr c_stc) FC;

CodePtr Switch_On_Atom(SwtTbl t, int size) FC;

long Switch_On_Integer(void) FC;

CodePtr Switch_On_Structure(SwtTbl t, int size) FC;

void Load_Cut_Level(WamWord *word_adr) FC;

void Cut(WamWord b_word) FC;

void Global_Push_Float(double n) FC;

double Obtain_Float(WamWord *adr) FC;

void Create_Choice_Point(CodePtr codep_alt, int arity) FC;

void Update_Choice_Point(CodePtr codep_alt, int arity) FC;

void Delete_Choice_Point(int arity) FC;

void Untrail(WamWord *low_adr) FC;

WamWord Make_Copy_Of_Word(int tag, WamWord word) FC;

Bool Unify(WamWord start_u_word, WamWord start_v_word) FC;

Bool Unify_Occurs_Check(WamWord start_u_word, WamWord start_v_word) FC;




/*---------------------------------*
 * Auxiliary engine macros         *
 *---------------------------------*/


	  /*---------------------------------------------------------------*
           * Deref dereferences the word start_word and sets :             *
           *   word : dereferenced word                                    *
           *   tag  : dereferenced word's tag                              *
           *   adr  : only if tag==REF then adr==value==self adress        *
           *---------------------------------------------------------------*/

#define Deref(start_word,word,tag,adr)                                      \
    do {                                                                    \
     WamWord *working_adr;                                                  \
     word=start_word;                                                       \
     adr=NULL;                                                              \
     for(;;)                                                                \
        {                                                                   \
         tag=Tag_Of(word);                                                  \
         if (tag!=REF || (working_adr=UnTag_REF(word))==adr)                \
             break;                                                         \
                                                                            \
         adr=working_adr;                                                   \
         word=*adr;                                                         \
        }                                                                   \
    } while(0)




	  /* Trail Stack Management */

#define Word_Needs_Trailing(adr)           ((adr)<(WamWord *) HB(B) ||      \
                                            (Is_A_Local_Adr(adr) && (adr)<B))



#define Bind_UV(adr,word)                                                   \
    do {                                                                    \
     if (Word_Needs_Trailing(adr))                                          \
         Trail_UV(adr);                                                     \
     *(adr)=(word);                                                         \
    } while(0)




#define Bind_OV(adr,word)                                                   \
    do {                                                                    \
     if (Word_Needs_Trailing(adr))                                          \
         Trail_OV(adr);                                                     \
     *(adr)=(word);                                                         \
    } while(0)




#define Bind_MV(adr,nb,real_adr)                                            \
    do {                                                                    \
     if (Word_Needs_Trailing(adr))                                          \
         Trail_MV(adr,nb);                                                  \
     Mem_Word_Cpy(adr,real_adr,nb)                                          \
    } while(0)




#define Trail_UV(adr)                                                       \
     Trail_Push(Trail_Tag_Value(TUV,adr))




#define Trail_OV(adr)                                                       \
    do {                                                                    \
     Trail_Push(*(adr));                                                    \
     Trail_Push(Trail_Tag_Value(TOV,adr));                                  \
    } while(0)




#define Trail_MV(adr,nb)                                                    \
    do {                                                                    \
     Mem_Word_Cpy(TR,adr,nb);                                               \
     TR+=nb;                                                                \
     Trail_Push(nb);                                                        \
     Trail_Push(Trail_Tag_Value(TMV,adr));                                  \
    } while(0)




#define Trail_FC(fct)                                                       \
     Trail_Push(Trail_Tag_Value(TFC,fct))




#define Delete_Last_Choice_Point()         B=BB(B)




	    /* Globalization */

#define Globalize_Local_Unbound_Var(adr)                                    \
    do {                                                                    \
     WamWord word;                                                          \
                                                                            \
     Bind_UV(adr,Tag_Value(REF,H));                                         \
     word=Make_Self_Ref(H);                                                 \
     Global_Push(word);                                                     \
    } while(0)




#define Mem_Word_Cpy(dst,src,nb)                                            \
    do {                                                                    \
     register long *s=(long *) (src);                                       \
     register long *d=(long *) (dst);                                       \
     register int   counter=(nb);                                           \
                                                                            \
     while(counter--)                                                       \
         *d++ = *s++;                                                       \
    } while(0)
