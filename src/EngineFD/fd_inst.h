/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_inst.h                                                       *
 * Descr.: FD instruction implementation - header file                     *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2004 Daniel Diaz                                     *
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

	  /* FD Variable Frame */

#define FD_VARIABLE_FRAME_SIZE     (OFFSET_RANGE+RANGE_SIZE+CHAINS_SIZE)
#define FD_INT_VARIABLE_FRAME_SIZE (OFFSET_RANGE+RANGE_SIZE)

#define OFFSET_RANGE               5
#define RANGE_SIZE                 (2+(sizeof(Range)/sizeof(WamWord)))

#define OFFSET_CHAINS              (OFFSET_RANGE+RANGE_SIZE)
#define CHAINS_SIZE                8



#define FD_Tag_Value(fdv_adr)      (((WamWord *) fdv_adr)[0])
#define FD_INT_Date(fdv_adr)       (((WamWord *) fdv_adr)[1])

#define Queue_Date_At_Push(fdv_adr)(((WamWord *) fdv_adr)[2])
#define Queue_Propag_Mask(fdv_adr) (((WamWord *) fdv_adr)[3])
#define Queue_Next_Fdv_Adr(fdv_adr)(((WamWord *) fdv_adr)[4])

#define Range_Stamp(fdv_adr)       (((WamWord *) fdv_adr)[OFFSET_RANGE])
#define Nb_Elem(fdv_adr)           (((WamWord *) fdv_adr)[OFFSET_RANGE+1])
#define Range(fdv_adr)             ((Range *) ((WamWord *) fdv_adr+OFFSET_RANGE+2))

#define Chains_Stamp(fdv_adr)      (((WamWord *) fdv_adr)[OFFSET_CHAINS])
#define Nb_Cstr(fdv_adr)           (((WamWord *) fdv_adr)[OFFSET_CHAINS+1])
#define Chains_Mask(fdv_adr)       (((WamWord *) fdv_adr)[OFFSET_CHAINS+2])
#define Chain_Min(fdv_adr)         (((WamWord *) fdv_adr)[OFFSET_CHAINS+3])
#define Chain_Max(fdv_adr)         (((WamWord *) fdv_adr)[OFFSET_CHAINS+4])
#define Chain_Min_Max(fdv_adr)     (((WamWord *) fdv_adr)[OFFSET_CHAINS+5])
#define Chain_Dom(fdv_adr)         (((WamWord *) fdv_adr)[OFFSET_CHAINS+6])
#define Chain_Val(fdv_adr)         (((WamWord *) fdv_adr)[OFFSET_CHAINS+7])




	  /* Shorthands for Range(fdv_adr)'s fields */

#define Extra_Cstr(fdv_adr)        (Range(fdv_adr)->extra_cstr)
#define Min(fdv_adr)               (Range(fdv_adr)->min)
#define Max(fdv_adr)               (Range(fdv_adr)->max)
#define Vec(fdv_adr)               (Range(fdv_adr)->vec)




	  /* Chain / Propagation Mask */


#define CHAIN_NB_MIN               0
#define CHAIN_NB_MAX               1
#define CHAIN_NB_MIN_MAX           2
#define CHAIN_NB_DOM               3
#define CHAIN_NB_VAL               4

#define MASK_EMPTY                 0
#define MASK_MIN                   1
#define MASK_MAX                   2
#define MASK_MIN_MAX               4
#define MASK_DOM                   8
#define MASK_VAL                   16


#define Has_Min_Mask(mask)         ((mask) & MASK_MIN)
#define Has_Max_Mask(mask)         ((mask) & MASK_MAX)
#define Has_Min_Max_Mask(mask)     ((mask) & MASK_MIN_MAX)
#define Has_Dom_Mask(mask)         ((mask) & MASK_DOM)
#define Has_Val_Mask(mask)         ((mask) & MASK_VAL)


#define Set_Min_Mask(mask)         ((mask) |= MASK_MIN)
#define Set_Max_Mask(mask)         ((mask) |= MASK_MAX)
#define Set_Min_Max_Mask(mask)     ((mask) |= MASK_MIN_MAX)
#define Set_Dom_Mask(mask)         ((mask) |= MASK_DOM)
#define Set_Val_Mask(mask)         ((mask) |= MASK_VAL)




	  /* Chain Record Frame */

#define CHAIN_RECORD_FRAME_SIZE    2

#define CF_Pointer(rec_adr)        ((WamWord *) (rec_adr[0]))
#define Next_Chain(rec_adr)        ((WamWord *) (rec_adr[1]))




	  /* Constraint Frame */

#define CONSTRAINT_FRAME_SIZE      3

#define OFFSET_OF_OPTIM_POINTER    1	/* this offset must corresponds to */

#define AF_Pointer(cf)             ((WamWord *)  (cf[0]))
#define Optim_Pointer(cf)          ((WamWord *)  (cf[1]))	/* this cell */
#define Cstr_Address(cf)           ((long (*)()) (cf[2]))




	  /* Miscellaneous */

#define ENV_VAR_VECTOR_MAX         "VECTORMAX"
#define DEFAULT_VECTOR_MAX         127


#define Fd_Variable_Is_Ground(fdv_adr) (Tag_Of(FD_Tag_Value(fdv_adr))==INT)




#define math_min(x, y)             ((x) <= (y) ? (x) : (y))
#define math_max(x, y)             ((x) >= (y) ? (x) : (y))




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef FD_INST_FILE

WamWord DATE;
WamWord *TP;
WamWord vec_size;
WamWord vec_max_integer;

#else

extern WamWord DATE;
extern WamWord *TP;
extern WamWord vec_size;
extern WamWord vec_max_integer;

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

WamWord *Fd_Prolog_To_Fd_Var(WamWord arg_word, Bool pl_var_ok);

Range *Fd_Prolog_To_Range(WamWord list_word);

int Fd_Prolog_To_Value(WamWord arg_word);

WamWord *Fd_Prolog_To_Array_Int(WamWord list_word);

WamWord *Fd_Prolog_To_Array_Any(WamWord list_word);

WamWord *Fd_Prolog_To_Array_Fdv(WamWord list_word, Bool pl_var_ok);

void Fd_List_Int_To_Range(Range *range, WamWord list_word);

WamWord *Fd_New_Variable(void);

WamWord *Fd_New_Bool_Variable(void);

WamWord *Fd_New_Int_Variable(int n);

WamWord *Fd_Create_C_Frame(long (*cstr_fct) (), WamWord *AF,
			   WamWord *fdv_adr, Bool optim2);

void Fd_Add_Dependency(WamWord *fdv_adr, int chain_nb, WamWord *CF);

void Fd_Add_List_Dependency(WamWord *array, int chain_nb, WamWord *CF);



void Fd_Before_Add_Cstr(void);

Bool Fd_After_Add_Cstr(void);

void Fd_Stop_Constraint(WamWord *CF);



Bool Fd_Tell_Value(WamWord *fdv_adr, int n);

Bool Fd_Tell_Not_Value(WamWord *fdv_adr, int n);

Bool Fd_Tell_Int_Range(WamWord *fdv_adr, Range *range);

Bool Fd_Tell_Interv_Interv(WamWord *fdv_adr, int min, int max);

Bool Fd_Tell_Range_Range(WamWord *fdv_adr, Range *range);

void Fd_Display_Extra_Cstr(WamWord *fdv_adr);



void Fd_Init_Solver0(void);

void Fd_Reset_Solver0(void);

Bool Fd_Assign_Value(WamWord *fdv_adr, int n);

Bool Fd_Unify_With_Integer0(WamWord *fdv_adr, int n);

Bool Fd_Unify_With_Fd_Var0(WamWord *fdv_adr1, WamWord *fdv_adr2);

Bool Fd_Use_Vector(WamWord *fdv_adr);

Bool Fd_Check_For_Bool_Var(WamWord x_word);

int Fd_Variable_Size0(WamWord *fdv_adr);

int Fd_Copy_Variable0(WamWord *dst_adr, WamWord *fdv_adr);

char *Fd_Variable_To_String0(WamWord *fdv_adr);




#define Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask)         \
  DEREF(fdv_word, word, tag_mask);                              \
  if (tag_mask == TAG_REF_MASK)                                 \
    Pl_Err_Instantiation();                                     \
                                                                \
  if (tag_mask != TAG_INT_MASK && tag_mask != TAG_FDV_MASK)     \
    Pl_Err_Type(type_fd_variable, word)
