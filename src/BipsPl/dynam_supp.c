/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : dynam_supp.c                                                    *
 * Descr.: dynamic predicate support                                       *
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


#include <stdlib.h>

#include "engine_pl.h"
#include "bips_pl.h"


#if 0
#define DEBUG
#endif
#if 0
#define DEBUG1
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define DYN_STAMP_NONE             ((DynStamp) -1)
#define ALL_MUST_BE_ERASED         (DynCInf *) 2 /* bit 0 used for mark */

#define MAX_KBYTES_BEFORE_CLEAN    512
#define MAX_SIZE_BEFORE_CLEAN      (MAX_KBYTES_BEFORE_CLEAN * 1024 / sizeof(WamWord))


#define START_DYNAMIC_SWT_SIZE     32

#define NO_INDEX                   0
#define VAR_INDEX                  1
#define ATM_INDEX                  2
#define INT_INDEX                  3
#define LST_INDEX                  4
#define STC_INDEX                  5




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct			/* Dynamic clause scanning info   */
{				/* --------- input data --------- */
  ScanFct alt_fct;		/* fct to call for each clause    */
  int alt_size_info;		/* user alt info size             */
  int owner_func;		/* func  of the owner (for dbg)   */
  int owner_arity;		/* arity of the owner (for dbg)   */
  DynPInf *dyn;			/* associated dyn info            */
  int stop_cl_no;		/* clause # to reach to stop scan */
  DynStamp erase_stamp;		/* max stamp to perform a retract */
  Bool xxx_is_seq_chain;        /* scan all clauses ?             */
  DynCInf *xxx_ind_chain;	/* current assoc idx (->clause)   */
  DynCInf *var_ind_chain;	/* current var   idx (->clause)   */
  DynCInf *clause;		/* current clause                 */
}
DynScan;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static DynStamp erase_stamp = 1;
static DynPInf *first_dyn_with_erase = NULL;
static int size_of_erased = 0;



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static DynPInf *Alloc_Init_Dyn_Info(PredInf *pred, int arity);

static int Index_From_First_Arg(WamWord first_arg_word, PlLong *key);

static void Add_To_2Chain(D2ChHdr *hdr, DynCInf *clause, Bool in_seq_chain,
			  Bool asserta);

static void Remove_From_2Chain(D2ChHdr *hdr, DynCInf *clause,
			       Bool in_seq_chain);

static void Erase_All(DynPInf *dyn);

static void Erase_All_Clauses_Of_File(DynPInf *dyn, int pl_file);

static void Clean_Erased_Clauses(void);

static void Unlink_Clause(DynCInf *clause);

static void Free_Clause(DynCInf *clause);



static DynScan *Get_Scan_Choice_Point(WamWord *b);

static DynCInf *Scan_Dynamic_Pred_Next(DynScan *scan);



#if defined(DEBUG) || defined(DEBUG1)

static void Check_Dynamic_Clauses(DynPInf *dyn);

static void Check_Hash(char *t, int index_no);

static void Check_Chain(D2ChHdr *p, int index_no);

#endif



#define SCAN_DYN_TEST_ALT          X1_247363616E5F64796E5F746573745F616C74

#define SCAN_DYN_JUMP_ALT          X1_247363616E5F64796E5F6A756D705F616C74

Prolog_Prototype(SCAN_DYN_TEST_ALT, 0);
Prolog_Prototype(SCAN_DYN_JUMP_ALT, 0);




/*-------------------------------------------------------------------------*
 * Dynamic clause management                                               *
 *                                                                         *
 * Dynamic clauses are stored in clause frames allocated by malloc.        *
 * The frame consists of:                                                  *
 *                                                                         *
 *   - a number (<0 if asserta >=0 if assertz) to order them.              *
 *   - a forward  sequential chain (chronological chain).                  *
 *   - a backward sequential chain (chronological chain).                  *
 *   - a forward  indexing   chain                                         *
 *   - a backward indexing   chain                                         *
 *   - the clause number                                                   *
 *   - the erase stamp (only if the clause is reased, DYN_STAMP_NONE else) *
 *   - the pointer to the next erased clause (only if the clause is erased)*
 *   - the pointer to the byte-code (or NULL if the clause is interpreted) *
 *   - the size of the Prolog term                                         *
 *   - the corresponding Prolog term of the form [Head|Body] for Head:-Body*
 *                                                                         *
 * For a dynamic predicate the structure DynPInfo has 6 entry-points for   *
 * clause chaining (2-link chains, with next of last = NULL):              *
 * 1 for the sequential chain, 5 for indexing chains, depending on the     *
 * first argument of the Head:                                             *
 *                                                                         *
 *   - seq_chain    : a chain to the first clause                          *
 *   - var_ind_chain: a chain to the first clause with a var as 1st arg    *
 *   - atm_htbl     : a hash table: key=atm/info=chain to the first clause *
 *   - int_htbl     : a hash table: key=int/info=chain to the first clause *
 *   - lst_ind_chain: a chain to the first clause with a list as 1st arg   *
 *   - stc_htbl     : a hash table: key=f_n/info=chain to the first clause *
 *                                                                         *
 * This clause management uses the logical database update view, ie. the   *
 * different altenatives of a predicate are not influenced by subsequent   *
 * actions. When a predicate must be scanned (cf. Scan_Dynamic_Pred) we    *
 * must ensure that subsequent retracted clause must be selected and all   *
 * subsequent added clause must be ignored.                                *
 * For added clause we use the count_z value when the selection starts (cf.*
 * stop_cl_no). A clause is ignored if its clause number (cl_no) is >= to  *
 * stop_cl_no.                                                             *
 * For retracted clause we use a stamp incremented at each selection. When *
 * a clause is retracted its erase_stamp is set to the current stamp. Then *
 * an erased clause ignored for a selection if its stamp is <= to the stamp*
 * of the selection.                                                       *
 * All erased clause of a predicate are linked (first_erased_cl /          *
 * next_erased_cl).                                                        *
 * All dynamic predicates with at least one erased clause are linked       *
 * (first_dyn_with_erase / next_dyn_with_erase).                           *
 * Erased clauses are physically cleaned when MAX_KBYTES_BEFORE_CLEAN are  *
 * reached. Then the local stack is scanned to detect all predicates with  *
 * selections and to mark them. All erased clauses of a predicate which is *
 * not marked are physically destroyed (free).                             *
 *                                                                         *
 * pl_file is the file name of its definition (or -1). Used for multifile  *
 * predicates by consult/1 (see Pl_Update_Dynamic_Pred).
 *-------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------*
 * PL_ADD_DYNAMIC_CLAUSE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
DynCInf *
Pl_Add_Dynamic_Clause(WamWord head_word, WamWord body_word, Bool asserta,
		      Bool check_perm, int pl_file)
{
  WamWord word;
  WamWord *first_arg_adr;
  int func, arity;
  PredInf *pred;
  int index_no;
  PlLong key;
  DynCInf *clause;
  DynPInf *dyn;
  char **p_ind_htbl;
  D2ChHdr *p_ind_hdr;
  DSwtInf swt_info;
  DSwtInf *swt;
  int size;
  WamWord lst_h_b;

  first_arg_adr = Pl_Rd_Callable_Check(head_word, &func, &arity);

#ifdef DEBUG
  DBGPRINTF("\tarity: %d", arity);
  if (arity > 0)
    {
      DBGPRINTF("\tfirst arg: ");
      Pl_Write(*first_arg_adr);
    }
  DBGPRINTF("\n");
#endif

  if ((pred = Pl_Lookup_Pred(func, arity)) == NULL)
    pred = Pl_Create_Pred(func, arity, pl_atom_user_input,
		       pl_stm_tbl[pl_stm_stdin]->line_count,
		       MASK_PRED_DYNAMIC | MASK_PRED_PUBLIC, NULL);
  else if (check_perm && !(pred->prop & MASK_PRED_DYNAMIC))
    {
      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Atom(func);
      Pl_Unify_Integer(arity);
      Pl_Err_Permission(pl_permission_operation_modify,
			pl_permission_type_static_procedure, word);
    }

  if (pl_file == pl_atom_void)
    pl_file = -1;

  dyn = (DynPInf *) (pred->dyn);
  if (dyn == NULL)		/* dynamic info not yet allocated ? */
    dyn = Alloc_Init_Dyn_Info(pred, arity);


  index_no = (dyn->arity) ? Index_From_First_Arg(*first_arg_adr, &key)
    : NO_INDEX;

#ifdef DEBUG
  DBGPRINTF("\n");
  DBGPRINTF("asserta: %d  Clause: ", asserta);
  DBGPRINTF("\thead: ");
  Pl_Write(head_word);
  DBGPRINTF("\tbody: ");
  Pl_Write(body_word);
  DBGPRINTF("\nByte Code at :%p\n", pl_byte_code);
#endif


  lst_h_b = Tag_LST(H);
  H[0] = head_word;
  H[1] = body_word;

  size = Pl_Term_Size(lst_h_b);
  clause = (DynCInf *)
    Malloc(sizeof(DynCInf) - 3 * sizeof(WamWord) + size * sizeof(WamWord));

  Add_To_2Chain(&dyn->seq_chain, clause, TRUE, asserta);

  clause->dyn = dyn;
  clause->cl_no = (asserta) ? dyn->count_a-- : dyn->count_z++;
  clause->pl_file = pl_file;
  clause->erase_stamp = DYN_STAMP_NONE;
  clause->next_erased_cl = NULL;
  clause->term_size = size;

  Pl_Copy_Term(&clause->term_word, &lst_h_b);

  clause->byte_code = pl_byte_code;
  pl_byte_code = NULL;


  switch(index_no)
    {
    case NO_INDEX:
      clause->ind_chain.next = NULL;
      clause->ind_chain.prev = NULL;
      p_ind_hdr = NULL;
      p_ind_htbl = NULL;
      break;

    case VAR_INDEX:
      p_ind_hdr = &(dyn->var_ind_chain);
      p_ind_htbl = NULL;
      break;

    case LST_INDEX:
      p_ind_hdr = &(dyn->lst_ind_chain);
      p_ind_htbl = NULL;
      break;

    case ATM_INDEX:
      p_ind_htbl = &(dyn->atm_htbl);
      break;

    case INT_INDEX:
      p_ind_htbl = &(dyn->int_htbl);
      break;

    case STC_INDEX:
      p_ind_htbl = &(dyn->stc_htbl);
      break;
    }

  clause->p_ind_htbl = p_ind_htbl;
  if (p_ind_htbl)
    {
      if (*p_ind_htbl == NULL)
	*p_ind_htbl = Pl_Hash_Alloc_Table(START_DYNAMIC_SWT_SIZE, sizeof(DSwtInf));

      swt_info.key = key;
      swt_info.ind_chain.first = swt_info.ind_chain.last = NULL;

      Pl_Extend_Table_If_Needed(p_ind_htbl);
      swt = (DSwtInf *) Pl_Hash_Insert(*p_ind_htbl, (char *) &swt_info, FALSE);

      p_ind_hdr = &(swt->ind_chain);
    }

  clause->p_ind_hdr = p_ind_hdr;
  if (p_ind_hdr)
    Add_To_2Chain(p_ind_hdr, clause, FALSE, asserta);

#ifdef DEBUG
  Check_Dynamic_Clauses(dyn);
#endif

  return clause;
}




/*-------------------------------------------------------------------------*
 * ALLOC_INIT_DYN_INFO                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static DynPInf *
Alloc_Init_Dyn_Info(PredInf *pred, int arity)
{
  DynPInf *dyn;

  dyn = (DynPInf *) Malloc(sizeof(DynPInf));

  dyn->seq_chain.first = dyn->seq_chain.last = NULL;
  dyn->var_ind_chain.first = dyn->var_ind_chain.last = NULL;
  dyn->lst_ind_chain.first = dyn->lst_ind_chain.last = NULL;
  dyn->atm_htbl = dyn->int_htbl = dyn->stc_htbl = NULL;
  dyn->arity = arity;
  dyn->count_a = -1;
  dyn->count_z = 0;
  dyn->first_erased_cl = NULL;
  dyn->next_dyn_with_erase = NULL;

  pred->dyn = (PlLong *) dyn;

  return dyn;
}




/*-------------------------------------------------------------------------*
 * INDEX_FROM_FIRST_ARG                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Index_From_First_Arg(WamWord first_arg_word, PlLong *key)
{
  WamWord word, tag_mask;
  int index_no;


  DEREF(first_arg_word, word, tag_mask);
  switch (Tag_From_Tag_Mask(tag_mask))
    {
    case REF:
#ifndef NO_USE_FD_SOLVER
    case FDV:
#endif
      index_no = VAR_INDEX;
      break;

    case INT:
      index_no = INT_INDEX;
      *key = UnTag_INT(word);
      break;

    case ATM:
      index_no = ATM_INDEX;
      *key = (PlLong) UnTag_ATM(word);
      break;

    case FLT:
      index_no = NO_INDEX;
      break;

    case LST:
      index_no = LST_INDEX;
      break;

    default:			/* tag==STC */
      index_no = STC_INDEX;
      *key = (PlLong) Functor_And_Arity(UnTag_STC(word));
      break;
    }

  return index_no;
}




/*-------------------------------------------------------------------------*
 * ADD_TO_2CHAIN                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Add_To_2Chain(D2ChHdr *hdr, DynCInf *clause, Bool in_seq_chain, Bool asserta)
{
  D2ChCell *cell = (in_seq_chain) ? &clause->seq_chain : &clause->ind_chain;

  if (hdr->first == NULL)	/* empty chain ? */
    {
      hdr->first = hdr->last = clause;
      cell->next = cell->prev = NULL;
      return;
    }

  if (asserta)
    {
      cell->next = hdr->first;
      cell->prev = NULL;
      hdr->first = clause;
      if (in_seq_chain)
	cell->next->seq_chain.prev = clause;
      else
	cell->next->ind_chain.prev = clause;
    }
  else
    {
      if (in_seq_chain)
	hdr->last->seq_chain.next = clause;
      else
	hdr->last->ind_chain.next = clause;
      cell->next = NULL;
      cell->prev = hdr->last;
      hdr->last = clause;
    }
}




/*-------------------------------------------------------------------------*
 * REMOVE_FROM_2CHAIN                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Remove_From_2Chain(D2ChHdr *hdr, DynCInf *clause, Bool in_seq_chain)
{
  D2ChCell *cell = (in_seq_chain) ? &clause->seq_chain : &clause->ind_chain;
  DynCInf *prev = cell->prev;
  DynCInf *next = cell->next;

  if (prev == NULL)		/* first cell ? */
    hdr->first = next;
  else
    {
      if (in_seq_chain)
	prev->seq_chain.next = next;
      else
	prev->ind_chain.next = next;
    }

  if (next == NULL)		/* last cell ? */
    hdr->last = prev;
  else
    {
      if (in_seq_chain)
	next->seq_chain.prev = prev;
      else
	next->ind_chain.prev = prev;
    }
}



/*-------------------------------------------------------------------------*
 * PL_DELETE_DYNAMIC_CLAUSE                                                *
 *                                                                         *
 * This comes down to erase the clause, ie. set it to current erase_stamp  *
 *-------------------------------------------------------------------------*/
void
Pl_Delete_Dynamic_Clause(DynCInf *clause)
{
  DynPInf *dyn;
  Bool first;

  dyn = clause->dyn;
  first = (dyn->first_erased_cl == NULL);

  clause->erase_stamp = erase_stamp;
  clause->next_erased_cl = dyn->first_erased_cl;
  dyn->first_erased_cl = clause;

  if (first)
    {
      dyn->next_dyn_with_erase = first_dyn_with_erase;
      first_dyn_with_erase = dyn;
    }

  size_of_erased += clause->term_size;
  Clean_Erased_Clauses();

#ifdef DEBUG
  Check_Dynamic_Clauses(dyn);
#endif
}




/*-------------------------------------------------------------------------*
 * ERASE_ALL_CLAUSES_OF_FILE                                               *
 *                                                                         *
 * This function is called to erase all clauses associated to a given file *
 * (this is for consult/1 on a multifile pred).                            *
 *-------------------------------------------------------------------------*/
static void
Erase_All_Clauses_Of_File(DynPInf *dyn, int pl_file)
{
  DynCInf *clause;

  if (dyn == NULL)
    return;

  for (clause = dyn->seq_chain.first; clause;
       clause = clause->seq_chain.next)
    {
      if (clause->erase_stamp == DYN_STAMP_NONE && clause->pl_file == pl_file)
	Pl_Delete_Dynamic_Clause(clause);
    }

#if 0
  Clean_Erased_Clauses();
#endif
}




/*-------------------------------------------------------------------------*
 * ERASE_ALL                                                               *
 *                                                                         *
 * This function is called to erase all clauses and, when possible, to free*
 * dyn with all associated info (hash tables,...)                          *
 *-------------------------------------------------------------------------*/
static void
Erase_All(DynPInf *dyn)
{
  Bool first;
  DynCInf *clause;

  if (dyn == NULL)
    return;

  first = (dyn->first_erased_cl == NULL);
  dyn->first_erased_cl = ALL_MUST_BE_ERASED;

  if (first)
    {
      dyn->next_dyn_with_erase = first_dyn_with_erase;
      first_dyn_with_erase = dyn;
    }

  for (clause = dyn->seq_chain.first; clause;
       clause = clause->seq_chain.next)
    {
      if (clause->erase_stamp == DYN_STAMP_NONE)
	size_of_erased += clause->term_size;
    }

  Clean_Erased_Clauses();
}




/*-------------------------------------------------------------------------*
 * CLEAN_ERASED_CLAUSES                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Clean_Erased_Clauses(void)
{
  WamWord *b, *base;
  DynScan *scan;
  DynPInf *dyn, *dyn1, **prev;
  DynCInf *clause, *clause1;

  if (size_of_erased <= MAX_SIZE_BEFORE_CLEAN)
    return;

  base = Local_Stack;
  for (b = B; b > base; b = BB(b))
    {
      scan = Get_Scan_Choice_Point(b);
      if (scan == NULL)
	continue;

      dyn = scan->dyn;

      if (dyn->first_erased_cl)	/* we must keep it - free impossible */
	dyn->first_erased_cl = (DynCInf *)
	  ((PlULong) (dyn->first_erased_cl) | 1);	/* mark it */
    }


  prev = &first_dyn_with_erase;
  for (dyn = first_dyn_with_erase; dyn; dyn = dyn1)
    {
      dyn1 = dyn->next_dyn_with_erase;
      if ((PlLong) (dyn->first_erased_cl) & 1)	/* marked ? */
	{			/* cannot free it */
	  dyn->first_erased_cl = (DynCInf *)
	    ((PlULong) (dyn->first_erased_cl) & (~1));
	  prev = &(dyn->next_dyn_with_erase);
	  continue;
	}

      /* not marked - can be cleaned */
      *prev = dyn->next_dyn_with_erase;

      if (dyn->first_erased_cl == ALL_MUST_BE_ERASED)	/* clean all ? */
	{
	  for (clause = dyn->seq_chain.first; clause; clause = clause1)
	    {
	      clause1 = clause->seq_chain.next;
	      size_of_erased -= clause->term_size;
	      Free_Clause(clause);
	    }

	  if (dyn->atm_htbl)
	    Pl_Hash_Free_Table(dyn->atm_htbl);

	  if (dyn->int_htbl)
	    Pl_Hash_Free_Table(dyn->int_htbl);

	  if (dyn->stc_htbl)
	    Pl_Hash_Free_Table(dyn->stc_htbl);

	  Free(dyn);
	  continue;
	}

      for (clause = dyn->first_erased_cl; clause; clause = clause1)
	{
	  clause1 = clause->next_erased_cl;
	  size_of_erased -= clause->term_size;
	  Unlink_Clause(clause);
	  Free_Clause(clause);
	}
      dyn->first_erased_cl = NULL;
      dyn->next_dyn_with_erase = NULL;

      if (dyn->seq_chain.first == NULL)	/* no more clauses */
	{
	  if (dyn->atm_htbl)
	    Pl_Hash_Free_Table(dyn->atm_htbl);

	  if (dyn->int_htbl)
	    Pl_Hash_Free_Table(dyn->int_htbl);

	  if (dyn->stc_htbl)
	    Pl_Hash_Free_Table(dyn->stc_htbl);

	  dyn->atm_htbl = dyn->int_htbl = dyn->stc_htbl = NULL;
	  dyn->count_a = -1;
	  dyn->count_z = 0;
	}
#ifdef DEBUG1
      Check_Dynamic_Clauses(dyn);
#endif
    }
}




/*-------------------------------------------------------------------------*
 * UNLINK_CLAUSE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Unlink_Clause(DynCInf *clause)
{
  DynPInf *dyn = clause->dyn;
  PlLong *p_key;
  DSwtInf swt_info;

  Remove_From_2Chain(&dyn->seq_chain, clause, TRUE);
  if (clause->p_ind_hdr)
    Remove_From_2Chain(clause->p_ind_hdr, clause, FALSE);

  if (clause->p_ind_htbl && clause->ind_chain.prev == NULL &&
      clause->ind_chain.next == NULL)
    {
      p_key = (PlLong *) ((char *) clause->p_ind_hdr -
	((char *) &(swt_info.ind_chain) - (char *) &(swt_info.key)));
#ifdef DEBUG1
      DBGPRINTF("Removing last ind key in a hash table  (%" PL_FMT_d ")\n", *p_key);
#endif
      Pl_Hash_Delete(*clause->p_ind_htbl, *p_key);
    }
}




/*-------------------------------------------------------------------------*
 * FREE_CLAUSE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Free_Clause(DynCInf *clause)
{
  if (clause->byte_code)
    Free(clause->byte_code);

  Free(clause);
}




/*-------------------------------------------------------------------------*
 * PL_UPDATE_DYNAMIC_PRED                                                  *
 *                                                                         *
 * what_to_do: bit 0: with check dynamic ?                                 *
 *             bit 1: also predicate definition ?                          *
 * examples  : 0 for consult/1                                             *
 *             1 for retractall/1                                          *
 *             2 for '$remove_predicate'/2                                 *
 *             3 for abolish/1                                             *
 *                                                                         *
 * pl_file_for_multi is for consulting a multifle pred defined in this file*
 *    (else pl_file_for_multi = -1). In this case, and if a previous       *
 *    predicate exists, only the clauses defined in this file are removed. *
 *                                                                         *
 * returns a pointer to associated pred or NULL if it does not exist.      *
 *-------------------------------------------------------------------------*/
PredInf *
Pl_Update_Dynamic_Pred(int func, int arity, int what_to_do, int pl_file_for_multi)
{
  WamWord word;
  PredInf *pred;

  pred = Pl_Lookup_Pred(func, arity);
  if (pred == NULL)
    return NULL;

  if ((what_to_do & 1) && !(pred->prop & MASK_PRED_DYNAMIC))
    {
      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Atom(func);
      Pl_Unify_Integer(arity);
      Pl_Err_Permission(pl_permission_operation_modify,
			pl_permission_type_static_procedure, word);
    }

  if (pl_file_for_multi >= 0 && (pred->prop & MASK_PRED_MULTIFILE))
    {
      Erase_All_Clauses_Of_File((DynPInf *) (pred->dyn), pl_file_for_multi);
    }
  else
    {
      Erase_All((DynPInf *) (pred->dyn));
      pred->dyn = NULL;
    }

  if ((what_to_do & 2))
    {
      Pl_Delete_Pred(func, arity);
      return NULL;
    }

  return pred;
}




/*-------------------------------------------------------------------------*
 * GET_SCAN_CHOICE_POINT                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static DynScan *
Get_Scan_Choice_Point(WamWord *b)
{
  DynScan *scan;
  int i;

  if (ALTB(b) != (CodePtr) Prolog_Predicate(SCAN_DYN_TEST_ALT, 0) &&
      ALTB(b) != (CodePtr) Prolog_Predicate(SCAN_DYN_JUMP_ALT, 0))
    return NULL;

  i = sizeof(DynScan) / sizeof(WamWord) - 1;
  scan = (DynScan *) &AB(b, i);

  return scan;
}




/*-------------------------------------------------------------------------*
 * PL_SCAN_DYNAMIC_PRED                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
DynCInf *
Pl_Scan_Dynamic_Pred(int owner_func, int owner_arity,
		     DynPInf *dyn, WamWord first_arg_word,
		     ScanFct alt_fct, int alt_fct_type,
		     int alt_info_size, WamWord *alt_info)
{
  int index_no;
  PlLong key;
  char **p_ind_htbl;
  DSwtInf *swt;
  DynScan scan;
  DynCInf *clause;
  WamWord *adr;
  int i;
  CodePtr scan_alt;

  if (owner_func < 0)
    owner_func = Pl_Get_Current_Bip(&owner_arity);

  index_no = (dyn->arity) ? Index_From_First_Arg(first_arg_word, &key)
    : NO_INDEX;

  scan.alt_fct = alt_fct;
  scan.alt_size_info = alt_info_size;
  scan.owner_func = owner_func;
  scan.owner_arity = owner_arity;
  scan.dyn = dyn;
  scan.stop_cl_no = dyn->count_z;
  scan.erase_stamp = erase_stamp++;

  switch (index_no)
    {
    case NO_INDEX:
    case VAR_INDEX:
      scan.xxx_is_seq_chain = TRUE;
      scan.xxx_ind_chain = dyn->seq_chain.first;
      p_ind_htbl = NULL;
      break;

    case LST_INDEX:
      scan.xxx_is_seq_chain = FALSE;
      scan.xxx_ind_chain = dyn->lst_ind_chain.first;
      p_ind_htbl = NULL;
      break;

    case ATM_INDEX:
      p_ind_htbl = &(dyn->atm_htbl);
      break;

    case INT_INDEX:
      p_ind_htbl = &(dyn->int_htbl);
      break;

    case STC_INDEX:
      p_ind_htbl = &(dyn->stc_htbl);
      break;

    }

  if (p_ind_htbl)
    {
      scan.xxx_is_seq_chain = FALSE;
      if (*p_ind_htbl &&
	  (swt = (DSwtInf *) Pl_Hash_Find(*p_ind_htbl, key)) != NULL)
	scan.xxx_ind_chain = swt->ind_chain.first;
      else
	scan.xxx_ind_chain = NULL;
    }

  if (scan.xxx_is_seq_chain)
    scan.var_ind_chain = NULL;
  else
    scan.var_ind_chain = dyn->var_ind_chain.first;

  clause = Scan_Dynamic_Pred_Next(&scan);
  if (clause == NULL)
    return NULL;

  if (Scan_Dynamic_Pred_Next(&scan) != NULL)	/* non deterministic case */
    {
      i = (sizeof(DynScan) + sizeof(WamWord) - 1) / sizeof(WamWord) +
	alt_info_size;

      if (alt_fct_type == DYN_ALT_FCT_FOR_TEST)
	scan_alt = (CodePtr) Prolog_Predicate(SCAN_DYN_TEST_ALT, 0);
      else
	scan_alt = (CodePtr) Prolog_Predicate(SCAN_DYN_JUMP_ALT, 0);

      Pl_Create_Choice_Point(scan_alt, i);
      adr = &AB(B, i) + 1;

      i = alt_info_size;
      while (i--)
	*adr++ = *alt_info++;

      *(DynScan *) adr = scan;
    }

  return clause;
}




/*-------------------------------------------------------------------------*
 * SCAN_DYNAMIC_PRED_NEXT                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static DynCInf *
Scan_Dynamic_Pred_Next(DynScan *scan)
{
  DynCInf *xxx_ind_chain, *var_ind_chain;
  DynCInf *xxx_clause, *var_clause;
  PlLong xxx_nb, var_nb;
  DynCInf *clause;

#ifdef DEBUG
  DBGPRINTF("Looking for next clause stamp:%" PL_FMT_d,
	    (PlLong) (scan->erase_stamp));
  Check_Dynamic_Clauses(scan->dyn);
#endif

  scan->clause = NULL;

start:
  xxx_ind_chain = scan->xxx_ind_chain;
  if (xxx_ind_chain)
    {
      xxx_clause = xxx_ind_chain;
      xxx_nb = xxx_clause->cl_no;
    }
  else
    xxx_nb = INT_GREATEST_VALUE;

  var_ind_chain = scan->var_ind_chain;
  if (var_ind_chain)
    {
      var_clause = var_ind_chain;
      var_nb = var_clause->cl_no;
    }
  else
    var_nb = INT_GREATEST_VALUE;

  if (xxx_nb <= var_nb)
    {
      if (xxx_nb == INT_GREATEST_VALUE)
	return NULL;

      clause = xxx_clause;
      if (scan->xxx_is_seq_chain)
	scan->xxx_ind_chain = xxx_ind_chain->seq_chain.next;
      else
	scan->xxx_ind_chain = xxx_ind_chain->ind_chain.next;
    }
  else
    {
      clause = var_clause;
      scan->var_ind_chain = var_ind_chain->ind_chain.next;
    }

  if (clause->cl_no >= scan->stop_cl_no)
    return NULL;

  if (clause->erase_stamp <= scan->erase_stamp)
    goto start;

  scan->clause = clause;

  return clause;
}




/*-------------------------------------------------------------------------*
 * PL_SCAN_DYNAMIC_PRED_ALT_0                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlLong
Pl_Scan_Dynamic_Pred_Alt_0(void)
{
  WamWord *alt_info;
  DynScan *scan;
  DynCInf *clause;
  Bool is_last;
  WamWord *adr;
  int i;
  CodePtr scan_alt;

  scan_alt = ALTB(B);
  Pl_Update_Choice_Point(scan_alt, 0);

  i = (sizeof(DynScan) + sizeof(WamWord) - 1) / sizeof(WamWord) - 1;

  scan = (DynScan *) &AB(B, i);
  adr = (WamWord *) scan;
  alt_info = (WamWord *) (adr - scan->alt_size_info);

  clause = scan->clause;

  is_last = (Scan_Dynamic_Pred_Next(scan) == NULL);

  if (is_last)
    Delete_Last_Choice_Point();

  return (*scan->alt_fct) (clause, alt_info, is_last);
}




/*-------------------------------------------------------------------------*
 * PL_SCAN_CHOICE_POINT_PRED                                               *
 *                                                                         *
 * returns the functor and initializes the arity of the scan choice point b*
 * or -1 if b is not a scan choice point.                                  *
 *-------------------------------------------------------------------------*/
int
Pl_Scan_Choice_Point_Pred(WamWord *b, int *arity)
{
  DynScan *scan;

  scan = Get_Scan_Choice_Point(b);
  if (scan == NULL)
    return -1;

  *arity = scan->owner_arity;

  return scan->owner_func;
}




/*-------------------------------------------------------------------------*
 * PL_COPY_CLAUSE_TO_HEAP                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Copy_Clause_To_Heap(DynCInf *clause, WamWord *head_word, WamWord *body_word)
{
  Pl_Copy_Contiguous_Term(H, &clause->term_word);	/* *H=<LST,H+1> */
  *head_word = H[1];
  *body_word = H[2];
  H += clause->term_size;
}




#if defined(DEBUG) || defined(DEBUG1)

/*-------------------------------------------------------------------------*
 * CHECK_DYNAMIC_CLAUSES                                                   *
 *                                                                         *
 * (debug function)                                                        *
 *-------------------------------------------------------------------------*/
static void
Check_Dynamic_Clauses(DynPInf *dyn)
{
  DBGPRINTF("\nFirst dyn with erase:%p\n",
	    first_dyn_with_erase);
  DBGPRINTF("Dyn:%p  arity:%d  count_a:%d  count_z:%d  "
	    "1st erased:%p  next dyn with erase:%p\n",
	    dyn, dyn->arity, dyn->count_a, dyn->count_z,
	    dyn->first_erased_cl, dyn->next_dyn_with_erase);

  Check_Chain(&dyn->seq_chain, NO_INDEX);
  Check_Chain(&dyn->var_ind_chain, VAR_INDEX);
  Check_Hash(dyn->atm_htbl, ATM_INDEX);
  Check_Hash(dyn->int_htbl, INT_INDEX);
  Check_Chain(&dyn->lst_ind_chain, LST_INDEX);
  Check_Hash(dyn->stc_htbl, STC_INDEX);
}




/*-------------------------------------------------------------------------*
 * CHECK_HASH                                                              *
 *                                                                         *
 * (debug function)                                                        *
 *-------------------------------------------------------------------------*/
static void
Check_Hash(char *t, int index_no)
{
  DSwtInf *swt;
  HashScan scan;

  if (t == NULL)
    return;

  switch (index_no)
    {
    case ATM_INDEX:
      DBGPRINTF("\nAtom\n");
      break;

    case INT_INDEX:
      DBGPRINTF("\nInteger\n");
      break;

    case STC_INDEX:
      DBGPRINTF("\nStructure\n");
      break;
    }

  for (swt = (DSwtInf *) Pl_Hash_First(t, &scan); swt;
       swt = (DSwtInf *) Pl_Hash_Next(&scan))
    {
      if (index_no == ATM_INDEX)
	DBGPRINTF("val <%s>\n", pl_atom_tbl[swt->key].name);

      if (index_no == INT_INDEX)
	DBGPRINTF("val <%" PL_FMT_d ">\n", swt->key);

      if (index_no == STC_INDEX)
	DBGPRINTF("val <%s/%d>\n", pl_atom_tbl[Functor_Of(swt->key)].name,
		  (int) Arity_Of(swt->key));

      Check_Chain(&swt->ind_chain, index_no);
    }
}




/*-------------------------------------------------------------------------*
 * CHECK_LIST                                                              *
 *                                                                         *
 * (debug function)                                                        *
 *-------------------------------------------------------------------------*/
static void
Check_Chain(D2ChHdr *hdr, int index_no)
{
  DynCInf *clause, *clause_b, *clause_f;

  if (hdr->first == NULL)
    return;

  switch (index_no)
    {
    case NO_INDEX:
      DBGPRINTF("\nSequential\n");
      break;

    case VAR_INDEX:
      DBGPRINTF("\nVariable\n");
      break;

    case LST_INDEX:
      DBGPRINTF("\nList\n");
      break;
    }

  for(clause = hdr->first; clause; clause = clause_f)
    {
      clause_f = clause_b = NULL;
      if (index_no == NO_INDEX)
	{
	  clause_f = clause->seq_chain.next;
	  clause_b = clause->seq_chain.prev;
	}
      else
	{
	  clause_f = clause->ind_chain.next;
	  clause_b = clause->ind_chain.prev;
	}

      DBGPRINTF(" %3d  %3d  %p  %p <-> %p  ",
		clause->cl_no, clause->term_size, clause,
		clause_b, clause_f);
      Pl_Write(clause->head_word);
      DBGPRINTF(":-");
      Pl_Write(clause->body_word);
      if (clause->erase_stamp != DYN_STAMP_NONE)
	DBGPRINTF("  erased at:%" PL_FMT_d "   next erased: %p",
		  clause->erase_stamp, (clause->next_erased_cl));
      DBGPRINTF("\n");
    }
}

#endif
