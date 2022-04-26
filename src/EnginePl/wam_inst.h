/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : wam_inst.h                                                      *
 * Descr.: WAM instruction implementation - header file                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
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

#if 0
#define GARBAGE_COLLECTOR
#endif


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define NOT_A_WAM_WORD             Tag_REF(0)

#define NIL_WORD                   Tag_ATM(ATOM_NIL)




	  /* Read/Write Modes */

	  /* if S==NULL iff we are in the write mode */

#define WRITE_MODE                 NULL




	  /* Environment Frame */

#ifdef GARBAGE_COLLECTOR

#define ENVIR_STATIC_SIZE          4

#define CPE(e)                     (*(WamCont *)   &(e[-1]))
#define BCIE(e)                    ((WamWord)   (e[-2]))
#define EE(e)                      ((WamWord *) (e[-3]))
#define NBYE(e)                    ((WamWord)   (e[-4]))
#define Y(e, y)                    ((WamWord)   (e[-5 - (y)]))

#define ENVIR_NAMES                {"CPE", "BCIE", "EE", "NBYE"}

#else

#define ENVIR_STATIC_SIZE          3

#define CPE(e)                     (*(WamCont *)   &(e[-1]))
#define BCIE(e)                    (*(WamWord *)   &(e[-2]))
#define EE(e)                      (*(WamWord **)  &(e[-3]))
#define Y(e, y)                    (*(WamWord *)   &(e[-4 - (y)]))

#define ENVIR_NAMES                {"CPE", "BCIE", "EE"}

#endif


	  /* Choice Point Frame */

#define CHOICE_STATIC_SIZE         8

#define ALTB(b)                    (*(CodePtr *)   &(b[-1]))
#define CPB(b)                     (*(WamCont *)   &(b[-2]))
#define BCIB(b)                    (*(WamWord *)   &(b[-3]))
#define EB(b)                      (*(WamWord **)  &(b[-4]))
#define BB(b)                      (*(WamWord **)  &(b[-5]))
#define HB(b)                      (*(WamWord **)  &(b[-6]))
#define TRB(b)                     (*(WamWord **)  &(b[-7]))
#define CSB(b)                     (*(WamWord **)  &(b[-8]))
#define AB(b, a)                   (*(WamWord *)   &(b[-9 - (a)]))

#define CHOICE_NAMES               {"ALTB", "CPB", "BCIB", "EB", "BB", \
                                    "HB", "TRB", "CSB"}




	  /* Wam Objects Manipulation */

	  /* Trail Tags */

#define NB_OF_TRAIL_TAGS           4

#define TUV                        0	/* Trail Unbound Variable   */
#define TOV                        1	/* Trail One Value          */
#define TMV                        2	/* Trail Multiple Values    */
#define TFC                        3	/* Trail for Function Call  */


#define TRAIL_TAG_NAMES            {"TUV", "TOV", "TMV", "TFC"}


#define Trail_Tag_Value(t, v)      ((unsigned long) (v) | (t))
#define Trail_Tag_Of(w)            ((unsigned long) (w) & 0x3)
#define Trail_Value_Of(w)          ((unsigned long) (w) & (~0x3))




	  /* Functor/arity */

#define Functor_Arity(f, n)        (((n) << ATOM_SIZE) + (f))
#define Functor_Of(word)           ((word) & (MAX_ATOM - 1))
#define Arity_Of(word)             ((word) >> ATOM_SIZE)


#ifndef NO_USE_FD_SOLVER
#define Dont_Separate_Tag(tag_mask) ((tag_mask) == TAG_FDV_MASK)
#else
#define Dont_Separate_Tag(tag_mask) (0)
#endif


#define Do_Copy_Of_Word(tag_mask, word) \
  if (Dont_Separate_Tag(tag_mask)) \
    word = Tag_REF(UnTag_Address(word))



	  /* Unbound Variables */

#define Make_Self_Ref(adr)         (Tag_REF(adr))




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
#define Arg(adr, i)                (((WamWord *) (adr))[OFFSET_ARG+i])
							/* i in 0..arity-1 */




	  /* Stacks */

#define Global_Push(word)          (*H++ = (WamWord) (word))

#define Global_Pop                 (*--H)




#define Trail_Push(word)           (*TR++ = (WamWord) (word))

#define Trail_Pop                  (*--TR)




#define Is_A_Local_Adr(adr)        ((adr) >= LSSA)




         /* Cut Management */

#define From_B_To_WamWord(b)       (Tag_INT((b) - LSSA))
#define From_WamWord_To_B(word)    (LSSA + UnTag_INT(word))




	  /* CP management */

#ifdef M_sparc

#define Adjust_CP(cp)              ((WamCont) ((unsigned long) (cp) - 8))
#define UnAdjust_CP(cp)            ((WamCont) ((unsigned long) (cp) + 8))

#else

#define Adjust_CP(p)               ((WamCont) (p))
#define UnAdjust_CP(cp)            (cp)

#endif


#ifndef FRAMES_ONLY

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

WamWord Create_Functor_Arity_Tagged(char *func_str, int arity) FC;

SwtTbl Create_Swt_Table(int size) FC;

void Create_Swt_Atm_Element(SwtTbl t, int size, int atom, CodePtr codep) FC;

void Create_Swt_Stc_Element(SwtTbl t, int size, int func, int arity,
			    CodePtr codep) FC;

Bool Get_Atom_Tagged(WamWord w, WamWord start_word) FC;

Bool Get_Atom(int atom, WamWord start_word) FC;

Bool Get_Integer_Tagged(WamWord w, WamWord start_word) FC;

Bool Get_Integer(long n, WamWord start_word) FC;

Bool Get_Float(double n, WamWord start_word) FC;

Bool Get_Nil(WamWord start_word) FC;

Bool Get_List(WamWord start_word) FC;

Bool Get_Structure_Tagged(WamWord w, WamWord start_word) FC;

Bool Get_Structure(int func, int arity, WamWord start_word) FC;

WamWord Put_X_Variable(void) FC;

WamWord Put_Y_Variable(WamWord *y_adr) FC;

WamWord Put_Unsafe_Value(WamWord start_word) FC;

WamWord Put_Atom_Tagged(WamWord w) FC;

WamWord Put_Atom(int atom) FC;

WamWord Put_Integer_Tagged(WamWord w) FC;

WamWord Put_Integer(long n) FC;

WamWord Put_Float(double n) FC;

WamWord Put_Nil(void) FC;

WamWord Put_List(void) FC;

WamWord Put_Structure_Tagged(WamWord w) FC;

WamWord Put_Structure(int func, int arity) FC;

WamWord Unify_Variable(void) FC;

void Unify_Void(int n) FC;

Bool Unify_Value(WamWord start_word) FC;

Bool Unify_Local_Value(WamWord start_word) FC;

Bool Unify_Atom_Tagged(WamWord w) FC;

Bool Unify_Atom(int atom) FC;

Bool Unify_Integer_Tagged(WamWord w) FC;

Bool Unify_Integer(long n) FC;

Bool Unify_Nil(void) FC;

Bool Unify_List(void) FC;

Bool Unify_Structure_Tagged(WamWord w) FC;

Bool Unify_Structure(int func, int arity) FC;

WamWord Globalize_If_In_Local(WamWord start_word) FC;

void Allocate(int n) FC;

void Deallocate(void) FC;

CodePtr Switch_On_Term(CodePtr c_var, CodePtr c_atm, CodePtr c_int,
		       CodePtr c_lst, CodePtr c_stc) FC;
CodePtr Switch_On_Term_Var_Atm(CodePtr c_var, CodePtr c_atm) FC;
CodePtr Switch_On_Term_Var_Stc(CodePtr c_var, CodePtr c_stc) FC;
CodePtr Switch_On_Term_Var_Atm_Lst(CodePtr c_var, CodePtr c_atm,
				   CodePtr c_lst) FC;
CodePtr Switch_On_Term_Var_Atm_Stc(CodePtr c_var, CodePtr c_atm,
				   CodePtr c_stc) FC;

CodePtr Switch_On_Atom(SwtTbl t, int size) FC;

long Switch_On_Integer(void) FC;

CodePtr Switch_On_Structure(SwtTbl t, int size) FC;

void Load_Cut_Level(WamWord *word_adr) FC;

void Cut(WamWord b_word) FC;

void Global_Push_Float(double n) FC;

double Obtain_Float(WamWord *adr) FC;


void Create_Choice_Point(CodePtr codep_alt, int arity) FC;
void Create_Choice_Point1(CodePtr codep_alt) FC;
void Create_Choice_Point2(CodePtr codep_alt) FC;
void Create_Choice_Point3(CodePtr codep_alt) FC;
void Create_Choice_Point4(CodePtr codep_alt) FC;


void Update_Choice_Point(CodePtr codep_alt, int arity) FC;
void Update_Choice_Point1(CodePtr codep_alt) FC;
void Update_Choice_Point2(CodePtr codep_alt) FC;
void Update_Choice_Point3(CodePtr codep_alt) FC;
void Update_Choice_Point4(CodePtr codep_alt) FC;

void Delete_Choice_Point(int arity) FC;
void Delete_Choice_Point1(void) FC;
void Delete_Choice_Point2(void) FC;
void Delete_Choice_Point3(void) FC;
void Delete_Choice_Point4(void) FC;

void Untrail(WamWord *low_adr) FC;

Bool Unify(WamWord start_u_word, WamWord start_v_word) FC;

Bool Unify_Occurs_Check(WamWord start_u_word, WamWord start_v_word) FC;


#endif /* FRAME_ONLY */

/*---------------------------------*
 * Auxiliary engine macros         *
 *---------------------------------*/


	  /*---------------------------------------------------------------*
           * DEREF dereferences the word start_word and sets :             *
           *   word    : dereferenced word                                 *
           *   tag_mask: dereferenced word's tag mask                      *
           *---------------------------------------------------------------*/

#if 0
#define DEREF_STATS
#endif

#ifdef DEREF_STATS
long nb_deref;
long chain_len;
#define DEREF_COUNT(x)  x++
#else
#define DEREF_COUNT(x)
#endif


#define DEREF(start_word, word, tag_mask)	\
  do						\
    {						\
      WamWord deref_last_word;			\
						\
      word = start_word;			\
						\
      DEREF_COUNT(nb_deref);			\
      do					\
	{					\
	  DEREF_COUNT(chain_len);		\
	  deref_last_word = word;		\
	  tag_mask = Tag_Mask_Of(word);		\
	  if (tag_mask != TAG_REF_MASK)		\
	    break;				\
	  word = *(UnTag_REF(word));		\
	}					\
      while (word != deref_last_word);		\
    }						\
  while (0)



  /* Trail Stack Management */

#define Word_Needs_Trailing(adr)			\
  ((adr) < HB1 || (Is_A_Local_Adr(adr) && (adr) < B))



#define Bind_UV(adr, word)			\
  do						\
    {						\
      if (Word_Needs_Trailing(adr))		\
	Trail_UV(adr);				\
      *(adr) = (word);				\
    }						\
  while (0)




#define Bind_OV(adr, word)			\
  do						\
    {						\
      if (Word_Needs_Trailing(adr))		\
	Trail_OV(adr);				\
      *(adr) = (word);				\
    }						\
  while (0)




#define Bind_MV(adr, nb, real_adr)		\
  do						\
    {						\
      if (Word_Needs_Trailing(adr))		\
	Trail_MV(adr, nb);			\
      Mem_Word_Cpy(adr, real_adr, nb);		\
    }						\
  while (0)




#define Trail_UV(adr)				\
  Trail_Push(Trail_Tag_Value(TUV, adr))


#define Trail_OV(adr)				\
  do						\
    {						\
      Trail_Push(*(adr));			\
      Trail_Push(Trail_Tag_Value(TOV, adr));	\
    }						\
  while (0)




#define Trail_MV(adr, nb)			\
  do						\
    {						\
      Mem_Word_Cpy(TR, adr, nb);		\
      TR += nb;					\
      Trail_Push(nb);				\
      Trail_Push(Trail_Tag_Value(TMV, adr));	\
    }						\
  while (0)




#define Trail_FC(fct, nb, arg)			\
  do						\
    {						\
      Mem_Word_Cpy(TR, arg, nb);		\
      TR += nb;					\
      Trail_Push(nb);				\
      Trail_Push(Trail_Tag_Value(TFC, fct));	\
    }						\
  while (0)


/* -- context support ------------------------------------------------------ */

#ifdef UnTag_ATM

static inline PredInf * FC
Cxt_Lookup_Pred_In (int key, WamWord unit, int arity)
{
  int unit_atom = UnTag_ATM (unit);
  void *ptable = pred_tbl;

  if (unit_atom && unit_atom != ATOM_NIL &&
      atom_tbl[unit_atom].modules && atom_tbl[unit_atom].modules[arity])
    ptable = atom_tbl[unit_atom].modules[arity];
  else
    ptable = pred_tbl;

  return (PredInf *) Hash_Find (ptable, key);
}

#endif

/* ------------------------------------------------------------------------- */


#ifndef FRAMES_ONLY
#ifndef USE_WATERMARK

/* -- regular GNU Prolog w/o watermark code -------------------------------- */

#define Call_CP_Destructors(b)     0
#define Create_Water_Mark(a,b)
#define Update_Water_Mark(a,b)

#define Assign_B(newB)              (B = (newB), HB1 = HB(B))

#else

/* -- support for watermarks ----------------------------------------------- */

#define Assign_B(newB)      (B = (newB), HB1 = HB(B), Call_CP_Destructors(B))

#ifndef MAXWMARK                       /* Number of active watermarks */
#define MAXWMARK 10240                 /* ie. pending destructors */
#endif

typedef struct Watermark {             /* A watermark on the stack: */
  WamWord *Bvalue;                     /* Choice-point */
  void (*destructor) ();               /* Function to call */
  void *parameter;                     /* parameter to above */
} Watermark, *WatermarkP;

extern int        wmark_count;         /* number of active watermarks */
extern Watermark  wmark[];             /* The stack of watermarks */
extern WatermarkP wmp;                 /* Top of watermark stack */

static inline WamWord *Call_CP_Destructors (WamWord *TargetB)
{
  while ((wmark_count > 0) && (TargetB < wmp->Bvalue)) {
    (*wmp->destructor) (wmp->parameter);

    --wmp;
    --wmark_count;
  }
  return TargetB;
}

void Create_Water_Mark (void (*fun)(), void *par) FC;
void Update_Water_Mark (void (*fun)(), void *par) FC;

/* ------------------------------------------------------------------------- */
#endif /* USE_WATERMARK */
#endif /* FRAMES_ONLY */


#define Delete_Last_Choice_Point()  Assign_B(BB(B))




  /* Globalization */

#define Globalize_Local_Unbound_Var(adr, res_word)	\
  do							\
    {							\
      WamWord *cur_H = H;				\
							\
      res_word = Make_Self_Ref(cur_H);			\
      *cur_H = res_word;				\
      H++;						\
      Bind_UV(adr, res_word);				\
    }							\
  while (0)




#define Mem_Word_Cpy(dst, src, nb)		\
  do						\
    {						\
      register long *s = (long *) (src);	\
      register long *d = (long *) (dst);	\
      register int counter = (nb);		\
						\
      do					\
	*d++ = *s++;				\
      while (--counter);			\
    }						\
  while (0)
