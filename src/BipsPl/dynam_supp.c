/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : dynam_supp.c                                                    *
 * Descr.: dynamic predicate support                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2025 Daniel Diaz                                     *
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
#include <stddef.h>
#include <string.h>


#include "engine_pl.h"
#include "bips_pl.h"


#define DEBUG_LEVEL 0


/* see dynam_supp.h to deactivate and below for explanation */
#ifndef OPTIM_FIRST_FOR_SCAN
#define first_for_scan first
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define DYN_STAMP_ALIVE            ((DynStamp) -1)

#define ALL_MUST_BE_ERASED         ((DynCInf *) 2) /* bit 0 used for mark */

#define MAX_SKIP_BEFORE_CLEAN      100 /* not too big since O(n^2/2) */
#define MAX_CLAUSES_BEFORE_CLEAN   1000


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
  DynStamp erase_stamp;		/* max stamp for LDUV             */
  Bool xxx_is_seq_chain;        /* scan all clauses ?             */
  DynCInf *xxx_ind_chain;	/* current assoc idx (->clause)   */
  DynCInf *var_ind_chain;	/* current var   idx (->clause)   */
  DynCInf *clause;		/* current clause                 */
}
DynScan;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static DynPInf *first_dyn_with_erase = NULL;

static int longest_skip_erased = 0; /* max nb of skipped clauses during a scan */
static int nb_erased_clauses = 0;   /* number of clauses waiting to be cleaned */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

/* gcc 13.3 emits a warning ‘hdr->first’ may be undefined [-Wsequence-point] 
  if written as a triple assignment in case of OPTIM_FIRST_FOR_SCAN is not defined */
#define INIT_2CHAIN_HDR(hdr)       ((hdr).first = (hdr).last = NULL, (hdr).first_for_scan = NULL)
      
#define Is_Clause_Erased(clause)   ((clause)->erase_stamp != DYN_STAMP_ALIVE)

static DynPInf *Alloc_Init_Dyn_Info(int func, int arity);

static int Index_From_First_Arg(WamWord first_arg_word, PlLong *key);

static void Add_To_2Chain(D2ChHdr *hdr, DynCInf *clause, Bool in_seq_chain, Bool asserta);

static void Remove_From_2Chain(D2ChHdr *hdr, DynCInf *clause, Bool in_seq_chain);

static void Free_Clause(DynCInf *clause);

static void Unlink_And_Free_Clause(DynCInf *clause);

static void Erase_All(DynPInf *dyn);

static void Erase_All_Clauses_Of_File(DynPInf *dyn, int pl_file);

static DynCInf *Scan_Dynamic_Pred_Next(DynScan *scan);

static DynScan *Get_Scan_Choice_Point(WamWord *b);

static void Clean_Erased_Clauses(void);

/* size of a DynScan in WamWords (rounded up) */
#define DYNSCAN_SIZE ((sizeof(DynScan) + sizeof(WamWord) - 1) / sizeof(WamWord))


#if DEBUG_LEVEL != 0

#define Print_Scan_Info(msg, sc)					\
  DBGPRINTF(msg ": %s/%d (owner: %s/%d) stamp: %" PL_FMT_u "\n",	\
	    pl_atom_tbl[(sc)->dyn->func].name, (sc)->dyn->arity,	\
	    pl_atom_tbl[(sc)->owner_func].name, (sc)->owner_arity,	\
	    (sc)->erase_stamp)

/* not defined as static to avoid compiler warning if unused */

void Print_Dynamic_Clause_Term(DynCInf *clause);

void Print_Dynamic_Clause(const char *msg, DynCInf *clause);

void Print_Dynamic_Info(DynPInf *dyn, const char *loc, Bool after_clean);

void Print_Hash_Table(char *t, int index_no, Bool after_clean);

void Print_Chain(D2ChHdr *p, int index_no, Bool after_clean);

#endif



#define SCAN_DYN_TEST_ALT          X1_247363616E5F64796E5F746573745F616C74

#define SCAN_DYN_JUMP_ALT          X1_247363616E5F64796E5F6A756D705F616C74

Prolog_Prototype(SCAN_DYN_TEST_ALT, 0);
Prolog_Prototype(SCAN_DYN_JUMP_ALT, 0);




/*
 * Dynamic clause management
 *
 * Dynamic clauses are stored in clause frames allocated by malloc.
 * The frame consists of:
 *
 *   - a number (<0 if asserta >=0 if assertz) to order them.
 *   - a 2-link (forward/backward) sequential chain (chronological chain)
 *   - a 2-link (forward/backward) indexing   chain
 *   - the clause number
 *   - owner Prolog file (see below)
 *   - the erase stamp (only if the clause is erased, DYN_STAMP_ALIVE else)*
 *   - the pointer to the next erased clause (only if the clause is erased)*
 *   - the pointer to the byte-code (or NULL if the clause is interpreted)
 *   - the size of the Prolog term
 *   - the corresponding Prolog term of the form [Head|Body] for Head:-Body
 *
 * For a dynamic predicate the structure DynPInfo has 6 entry-points for
 * clause chaining (2-link chains, with prev of first = next of last = NULL):
 * 1 for the sequential chain, 5 for indexing chains, depending on the
 * first argument of the head:
 *
 *   - seq_chain    : a chain to the first clause
 *   - var_ind_chain: a chain to the first clause with a var as 1st arg
 *   - atm_htbl     : a hash table: key=atm, info=chain to the first clause
 *   - int_htbl     : a hash table: key=int, info=chain to the first clause
 *   - lst_ind_chain: a chain to the first clause with a list as 1st arg
 *   - stc_htbl     : a hash table: key=f_n, info=chain to the first clause
 *
 * We follow the ISO logical database update view (LDUV): the different 
 * altenatives of a predicate are not influenced by subsequent actions 
 * (assert/retract). NB: LDUV only applies for dynamic predicates with 
 * alternatives (more than one answer -> choice-point).
 * See: https://stackoverflow.com/questions/28116244/how-prologs-logical-
 * update-view-works-for-assert-and-retract
 * The selection of "candidate" clauses is done by Scan_Dynamic_Pred which
 * creates a scan-point (for LDUV, we are interested here in scan-points
 * giving rise to a choice-point). When a scan-point is crearted, we must 
 * ensure that subsequent added/removed clauses do not impact the set of
 * candidate clauses.
 *
 * For added clauses we record count_z (stop_cl_no) when the scan starts:
 * a clause is ignored if its clause no (cl_no) >= stop_cl_no.
 *
 * For retracted clauses we use a stamp incremented at each scan: when a
 * clause is retracted its erase_stamp is set to the current stamp. It then
 * will be ignored (skipped) by subsequent scans. An erased clause is 
 * ignored (skipped) in a scan-point if its stamp <= the stamp of the scan.
 *
 I However, this can leads to a O(n^2/2) scan (when the amount of skipped
 * clauses gradually increases at each retraction). To avoid this, it is 
 * necessary to really clean erased clauses (unlink them and free space).
 * This is done by a GC-clause (see below).
 *
 * Another improvement is the OPTIM_FIRST_FOR_SCAN: for each 2-link chain, 
 * we maintain (along the 'first' pointer) a 'first_for_scan' pointer which 
 * will be used by the next scans as initial starting point. When the first
 * clause is retracted (after its erase_stamp is set) the first_for_scan 
 * pointer is set to the next clause (which is not NULL). So, subsequent
 * scans will quickly find the first clause by reducing the amount of
 * erased clause to skip (avoiding the O(n^2/2) problem in the common case 
 * the first clause is retracted). This optim does not interfer with LDUV 
 * since existing scans have their chains already initialized (and follow 
 * the 'next' pointers). When the GC-clause is triggered, for each recoverable
 * dyn pred, the unlink+free procedure reinitializes first and first_for_scan
 * ensuring both points to the same clause value at the end (in debug mode, 
 * the Print_... functions check this). The cost of OPTIM_FIRST_FOR_SCAN, 
 * is an extra pointer in all chains of a dynamic predicate (including 
 * those for each key of index hash tables). I'm not sure if the gain is 
 * important if the GC-clause is well tuned (i.e. executed enough often). 
 * This optimisation can be deactivated in dynam_supp.h.
 *
 * GC-clause: clean the space (free) of erased clauses. It is triggered
 * when MAX_CLAUSES_BEFORE_CLEAN or when MAX_SKIP_BEFORE_CLEAN is reached. 
 * The local stack is traversed to detect all active dyn predicate (i.e.
 * with a scan-point in the stack) and mark them. Then, all erased clauses 
 * of a not marked dyn predicate are reclaimed (freed). For this, all erased 
 * clauses of a predicate are linked (first/next_erased_cl). All dynamic 
 * predicates with at least one erased clause are linked 
 * (first/next_dyn_with_erase).
 */

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
  PlLong key = 0;		/* init for the compiler */
  DynCInf *clause;
  DynPInf *dyn;
  char **p_ind_htbl;
  D2ChHdr *p_ind_hdr;
  DSwtInf swt_info;
  DSwtInf *swt;
  int size;
  WamWord lst_h_b;

  first_arg_adr = Pl_Rd_Callable_Check(head_word, &func, &arity);

  if ((pred = Pl_Lookup_Pred(func, arity)) == NULL)
    pred = Pl_Create_Pred(func, arity, pl_atom_user_input,
			  (int) pl_stm_tbl[pl_stm_stdin]->line_count,
			  MASK_PRED_DYNAMIC | MASK_PRED_PUBLIC, NULL);
  else if (check_perm && !(pred->prop & MASK_PRED_DYNAMIC))
    {
      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Atom(func);
      Pl_Unify_Integer(arity);
      Pl_Err_Permission(pl_permission_operation_modify,
			pl_permission_type_static_procedure, word);
    }

  /* pl_file is the file name of its definition (or -1). Used for multifile
   * predicates by consult/1 (see Pl_Update_Dynamic_Pred) 
   */
  if (pl_file == pl_atom_void)
    pl_file = -1;

  if (pred->dyn == NULL)		/* dynamic info not yet allocated ? */
    pred->dyn = Alloc_Init_Dyn_Info(func, arity);
  dyn = pred->dyn;
  
  MPROBE_PTR("dyn", dyn);

  index_no = (dyn->arity) ? Index_From_First_Arg(*first_arg_adr, &key) : NO_INDEX;

  lst_h_b = Tag_LST(H);
  H[0] = head_word;
  H[1] = body_word;

  size = Pl_Term_Size(lst_h_b);
#if DEBUG_LEVEL >= 5
  DBGPRINTF("assert term to copy: ");
  Pl_Write(lst_h_b);
  DBGPRINTF(" - size: %d   H: %p\n", size, H);
#endif
  clause = (DynCInf *) Malloc(sizeof(DynCInf) + (size - 3) * sizeof(WamWord));

  Add_To_2Chain(&dyn->seq_chain, clause, TRUE, asserta);

  clause->dyn = dyn;
  clause->cl_no = (asserta) ? dyn->count_a-- : dyn->count_z++;
  clause->pl_file = pl_file;
  clause->erase_stamp = DYN_STAMP_ALIVE;
  clause->next_erased_cl = NULL;
  clause->term_size = size;

  Pl_Copy_Term(&clause->term_word, &lst_h_b);

  clause->byte_code = pl_byte_code;
  pl_byte_code = NULL;

#if DEBUG_LEVEL >= 1
  Print_Dynamic_Clause("Add clause:", clause);
  DBGPRINTF("\t| index_no: %d  byte-code: %p\n", index_no, pl_byte_code);
#if 0
  if (arity > 0)
    {
      DBGPRINTF("\tfirst arg: ");
      Pl_Write(*first_arg_adr);
      DBGPRINTF("\n");
    }
#endif
#endif


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
      INIT_2CHAIN_HDR(swt_info.ind_chain);

      Pl_Extend_Table_If_Needed(p_ind_htbl);
      swt = (DSwtInf *) Pl_Hash_Insert(*p_ind_htbl, (char *) &swt_info, FALSE);

      p_ind_hdr = &(swt->ind_chain);
    }

  clause->p_ind_hdr = p_ind_hdr;
  if (p_ind_hdr)
    Add_To_2Chain(p_ind_hdr, clause, FALSE, asserta);

#if DEBUG_LEVEL >= 5
  Print_Dynamic_Info(dyn, __func__, FALSE);
#endif

  MPROBE_PTR("clause", clause);
  return clause;
}




/*-------------------------------------------------------------------------*
 * ALLOC_INIT_DYN_INFO                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static DynPInf *
Alloc_Init_Dyn_Info(int func, int arity)
{
  DynPInf *dyn;

  dyn = (DynPInf *) Malloc(sizeof(DynPInf));

  INIT_2CHAIN_HDR(dyn->seq_chain);
  INIT_2CHAIN_HDR(dyn->var_ind_chain);
  INIT_2CHAIN_HDR(dyn->lst_ind_chain);
  dyn->atm_htbl = dyn->int_htbl = dyn->stc_htbl = NULL;
  dyn->func = func;
  dyn->arity = arity;
  dyn->count_a = -1;
  dyn->count_z = 0;
  dyn->first_erased_cl = NULL;
  dyn->next_dyn_with_erase = NULL;

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

  MPROBE_PTR("clause", clause);

  if (hdr->first == NULL)	/* empty chain ? */
    {
      hdr->first = hdr->last = clause;
      hdr->first_for_scan = clause;
      cell->next = cell->prev = NULL;
      return;
    }

  if (asserta)
    {
      cell->next = hdr->first;
      cell->prev = NULL;
      hdr->first = clause;
      hdr->first_for_scan = clause;
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

  MPROBE_PTR("clause", clause);

  if (prev == NULL)		/* first cell ? */
    {
      hdr->first = next;
      hdr->first_for_scan = next;
    }
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
 * FREE_CLAUSE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Free_Clause(DynCInf *clause)
{
  MPROBE_PTR("clause", clause);

  if (clause->byte_code)
    Free(clause->byte_code);

#if DEBUG_LEVEL >= 4
  DBGPRINTF("Free clause no: %d at %p\n", clause->cl_no, clause);
#endif

  nb_erased_clauses--;
  Free(clause);
}




/*-------------------------------------------------------------------------*
 * UNLINK_AND_FREE_CLAUSE                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Unlink_And_Free_Clause(DynCInf *clause)
{
  DynPInf *dyn = clause->dyn;
  PlLong *p_key;

  MPROBE_PTR("clause", clause);

#if DEBUG_LEVEL >= 2
  Print_Dynamic_Clause("Unlink+Free clause:", clause);
#endif

  MPROBE_PTR("dyn", dyn);

  Remove_From_2Chain(&dyn->seq_chain, clause, TRUE);
  if (clause->p_ind_hdr)
    Remove_From_2Chain(clause->p_ind_hdr, clause, FALSE);

  if (clause->p_ind_htbl && clause->ind_chain.prev == NULL && clause->ind_chain.next == NULL)
    {
      p_key = (PlLong *) ((char *) clause->p_ind_hdr - offsetof(DSwtInf, ind_chain));
#if DEBUG_LEVEL >= 4
      DBGPRINTF("Removing last ind key in a hash table  (%" PL_FMT_d ")\n", *p_key);
#endif
      Pl_Hash_Delete(*clause->p_ind_htbl, *p_key);
    }
#if DEBUG_LEVEL >= 3
  Print_Dynamic_Info(dyn, __func__, FALSE);
#endif

  Free_Clause(clause);
}




/*-------------------------------------------------------------------------*
 * PL_DELETE_DYNAMIC_CLAUSE                                                *
 *                                                                         *
 * This comes down to erase the clause, ie. set it to current erase stamp  *
 *-------------------------------------------------------------------------*/
void
Pl_Delete_Dynamic_Clause(DynCInf *clause)
{
  DynPInf *dyn;

  MPROBE_PTR("clause", clause);

  /* Test if clause is already deleted. This can occurs with LDUV with
   * foo(1).
   * foo(2).
   *
   * | ?- retract(foo(X)), retract(foo(2)).
   * foo(2) is deleted after X=1 and on backtracking (LDUV) with X=2 (then failure).
   */
  if (Is_Clause_Erased(clause))
    return;

  dyn = clause->dyn;

  MPROBE_PTR("dyn", dyn);

#if DEBUG_LEVEL >= 1
  Print_Dynamic_Clause("Delete clause", clause);
#endif

  if (dyn->first_erased_cl == NULL) /* first clause deletion -> link dyn with erase */
    {
      dyn->next_dyn_with_erase = first_dyn_with_erase;
      first_dyn_with_erase = dyn;
    }

  clause->erase_stamp = dyn->curr_stamp;
  clause->next_erased_cl = dyn->first_erased_cl;
  dyn->first_erased_cl = clause;

  nb_erased_clauses++;

#ifdef OPTIM_FIRST_FOR_SCAN
  if (dyn->seq_chain.first_for_scan == clause && clause->seq_chain.next)
    dyn->seq_chain.first_for_scan = clause->seq_chain.next;

  if (clause->p_ind_hdr &&
      clause->p_ind_hdr->first_for_scan == clause && clause->ind_chain.next)
      clause->p_ind_hdr->first_for_scan = clause->ind_chain.next;
#endif

  Clean_Erased_Clauses();

#if DEBUG_LEVEL >= 5
  Print_Dynamic_Info(dyn, __func__, FALSE);
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

  MPROBE_PTR("dyn", dyn);

  for (clause = dyn->seq_chain.first; clause; clause = clause->seq_chain.next)
    {
      MPROBE_PTR("clause", clause);

      if (clause->pl_file == pl_file)
	Pl_Delete_Dynamic_Clause(clause);
    }

  Clean_Erased_Clauses();

#if DEBUG_LEVEL >= 5
  Print_Dynamic_Info(dyn, __func__, FALSE);
#endif
}




/*-------------------------------------------------------------------------*
 * ERASE_ALL                                                               *
 *                                                                         *
 * This function is called to erase all clauses and to free dyn with       *
 * associated info (hash tables,...).                                      *
 * Effectively done by the GC-clause when it will be possible.             *
 *-------------------------------------------------------------------------*/
static void
Erase_All(DynPInf *dyn)
{
  DynCInf *clause;

  if (dyn == NULL)
    return;

  MPROBE_PTR("dyn", dyn);

  if (dyn->first_erased_cl == NULL) /* first clause deletion -> link dyn with erase */
    {
      dyn->next_dyn_with_erase = first_dyn_with_erase;
      first_dyn_with_erase = dyn;
    }

  dyn->first_erased_cl = ALL_MUST_BE_ERASED;

  for (clause = dyn->seq_chain.first; clause; clause = clause->seq_chain.next)
    {
      MPROBE_PTR("clause", clause);
      
      if (!Is_Clause_Erased(clause))
	nb_erased_clauses++;
    }

  Clean_Erased_Clauses();
  /* Do not call Print_Dynamic_Info(dyn); since dyn is in an unstable state (parts freed) */
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
      Erase_All_Clauses_Of_File(pred->dyn, pl_file_for_multi);
    }
  else
    {
      Erase_All(pred->dyn);
      pred->dyn = NULL;		/* pred->dyn will be reallocated if needed */
    }

  if ((what_to_do & 2))
    {
      Pl_Delete_Pred(func, arity);
      return NULL;
    }

  return pred;
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

  /* Here is a good place to free erased clauses, just before a choice-point 
   * is created for this dynamic predicate. Here is a naive pred (use findall) 
   * to retract all clauses creating a list:
   *
   * r([X|L]) :- retract(foo(X)), !, r(L).
   * r([]).
   *
   * with the cut we can really clean (free) step-by-step the n facts of foo/1
   * (depending on MAX_CLAUSES_BEFORE_CLEAN, to check set it to 0).
   * If we do not call the GC here, a choice-point (for recursive retract),
   * will be created, making it impossible to clean erased clauses of foo/1.
   *
   * NB: the cut is important (removes the choice-point) else leads to a O(n^2/2) 
   * complexity: at each recursion a new clause is skipped/ignored due to LDUV.
   * see OPTIM_FIRST_FOR_SCAN to improve this.
   */

  Clean_Erased_Clauses();
  MPROBE_PTR("dyn", dyn);
  
  if (owner_func < 0)
    owner_func = Pl_Get_Current_Bip(&owner_arity);

  index_no = (dyn->arity) ? Index_From_First_Arg(first_arg_word, &key) : NO_INDEX;

  scan.alt_fct = alt_fct;
  scan.alt_size_info = alt_info_size;
  scan.owner_func = owner_func;
  scan.owner_arity = owner_arity;
  scan.dyn = dyn;
  scan.stop_cl_no = dyn->count_z;
  scan.erase_stamp = dyn->curr_stamp;

#if DEBUG_LEVEL >= 1
  Print_Scan_Info("SCAN DYNAMIC", &scan);
#endif

  switch (index_no)
    {
    case NO_INDEX:
    case VAR_INDEX:
      scan.xxx_is_seq_chain = TRUE;
      scan.xxx_ind_chain = dyn->seq_chain.first_for_scan;
      p_ind_htbl = NULL;
      break;

    case LST_INDEX:
      scan.xxx_is_seq_chain = FALSE;
      scan.xxx_ind_chain = dyn->lst_ind_chain.first_for_scan;
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
      if (*p_ind_htbl && (swt = (DSwtInf *) Pl_Hash_Find(*p_ind_htbl, key)) != NULL)
	scan.xxx_ind_chain = swt->ind_chain.first_for_scan;
      else
	scan.xxx_ind_chain = NULL;
    }

  if (scan.xxx_is_seq_chain)
    scan.var_ind_chain = NULL;
  else
    scan.var_ind_chain = dyn->var_ind_chain.first_for_scan;

  clause = Scan_Dynamic_Pred_Next(&scan);
  if (clause == NULL)
    return NULL;

  if (Scan_Dynamic_Pred_Next(&scan) != NULL)	/* non deterministic case */
    {
      dyn->curr_stamp++;	/* LDUV needs care only when there are more than one answer */

      if (alt_fct_type == DYN_ALT_FCT_FOR_TEST)
	scan_alt = (CodePtr) Prolog_Predicate(SCAN_DYN_TEST_ALT, 0);
      else
	scan_alt = (CodePtr) Prolog_Predicate(SCAN_DYN_JUMP_ALT, 0);

      i = DYNSCAN_SIZE + alt_info_size;
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

#if DEBUG_LEVEL >= 1
  Print_Scan_Info("SCAN Next", scan);
#endif

#if DEBUG_LEVEL >= 5
  Print_Dynamic_Info(scan->dyn, __func__, FALSE);
#endif

  scan->clause = NULL;

  MPROBE_PTR("dyn", scan->dyn);

  int nb_skip_erased = -1;		/* count erased clauses */
  do
    {
      nb_skip_erased++;

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

      MPROBE_PTR("clause", clause);

      /* Detect when remaining clauses are beyond the scan point (created after it) */
      if (clause->cl_no >= scan->stop_cl_no)
	{
#if DEBUG_LEVEL >= 2
	  DBGPRINTF("STOP NOW on clause %p since %d >= %d -- ",
		    clause, clause->cl_no, scan->stop_cl_no);
	  Print_Dynamic_Clause_Term(clause);
#endif
	  return NULL;
	}
      
#if DEBUG_LEVEL >= 2
      if (clause->erase_stamp <= scan->erase_stamp)
	{
	  DBGPRINTF(">>> IGNORED clause: %" PL_FMT_u " <= %" PL_FMT_u " skip erased: %d - ",
		    clause->erase_stamp, scan->erase_stamp, nb_skip_erased);
	  Print_Dynamic_Clause_Term(clause);
	}
#endif
    }
  while(clause->erase_stamp <= scan->erase_stamp);

  if (nb_skip_erased > longest_skip_erased)
    longest_skip_erased = nb_skip_erased;

#if DEBUG_LEVEL >= 1
  if (nb_skip_erased > 0)
    {
      DBGPRINTF("||| nb_skip_erased: %d (erased clauses) - head: ", nb_skip_erased);
      Print_Dynamic_Clause_Term(clause);
    }
#endif

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

  i = DYNSCAN_SIZE - 1;
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
 * PL_COPY_CLAUSE_TO_HEAP                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Copy_Clause_To_Heap(DynCInf *clause, WamWord *head_word, WamWord *body_word)
{
  MPROBE_PTR("clause", clause);

  Pl_Copy_Contiguous_Term(H, &clause->term_word);	/* *H=<LST,H+1> */
  *head_word = H[1];
  *body_word = H[2];
  H += clause->term_size;
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

  i = DYNSCAN_SIZE - 1;
  scan = (DynScan *) &AB(b, i);

  return scan;
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
 * CLEAN_ERASED_CLAUSES                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Clean_Erased_Clauses(void)
{
  WamWord *b, *base;
  DynScan *scan;
  DynPInf *dyn, *dyn1, **p_last_dyn;
  DynCInf *clause, *clause1;
 
  if (nb_erased_clauses < MAX_CLAUSES_BEFORE_CLEAN &&
      longest_skip_erased < MAX_SKIP_BEFORE_CLEAN)
    return;

#if DEBUG_LEVEL >= 3
  DBGPRINTF("/// GC-DYN-ERASE: recoverable nb of clauses: %d\n", nb_erased_clauses);
#endif
  
  /* other possibility to mark: use -count_a (thus a positive number)  */
#define MARK_AS_KEEP(dyn)       dyn->first_erased_cl = (DynCInf *) ((PlULong) (dyn->first_erased_cl) | 1)
#define IS_MARKED_AS_KEEP(dyn)  ((PlULong) (dyn->first_erased_cl) & 1)
#define UNMARK_AS_KEEP(dyn)     dyn->first_erased_cl = (DynCInf *) ((PlULong) (dyn->first_erased_cl) & (~1))

  /* GC-clauses: mark and clean */

  /* Traverse all choice-points from top to bottom
   * marking those corresponding to a dyn scan (thus in use) */
  base = Local_Stack;
  for (b = B; b > base; b = BB(b))
    {
      scan = Get_Scan_Choice_Point(b);
      if (scan == NULL)
	{
#if DEBUG_LEVEL >= 5
	  DBGPRINTF("GC-DYN-ERASE: chc-point other\n");
#endif
	  continue;
	}

      dyn = scan->dyn;
#if DEBUG_LEVEL >= 4
      Print_Scan_Info("GC-DYN-ERASE: chc-point mark scan", scan);
#endif
      if (dyn->first_erased_cl)	/* has erased clause but is scanned: mark it */
	MARK_AS_KEEP(dyn);
    }

  p_last_dyn = &first_dyn_with_erase;
  for (dyn = first_dyn_with_erase; dyn; dyn = dyn1)
    {
      MPROBE_PTR("dyn", dyn);
      dyn1 = dyn->next_dyn_with_erase;
      if (IS_MARKED_AS_KEEP(dyn))
	{			/* marked: cannot be cleaned (unmark it) */
	  UNMARK_AS_KEEP(dyn);
	  p_last_dyn = &(dyn->next_dyn_with_erase);
	  continue;
	}

      /* not marked: can be cleaned */
      *p_last_dyn = dyn->next_dyn_with_erase;

      if (dyn->first_erased_cl == ALL_MUST_BE_ERASED)	
	{			/* avoid to remove one by one all entries in hash tables */
#if DEBUG_LEVEL >= 4
	      DBGPRINTF("Free-all clause of dyn %p\n", dyn);
#endif
	  for (clause = dyn->seq_chain.first; clause; clause = clause1)
	    {
	      clause1 = clause->seq_chain.next;
	      Free_Clause(clause);
	    }

	  if (dyn->atm_htbl)
	    Pl_Hash_Free_Table(dyn->atm_htbl);

	  if (dyn->int_htbl)
	    Pl_Hash_Free_Table(dyn->int_htbl);

	  if (dyn->stc_htbl)
	    Pl_Hash_Free_Table(dyn->stc_htbl);

	  Free(dyn);		/* has been re-allocated if needed, so it is safe to free */
	  continue;
	}

      for (clause = dyn->first_erased_cl; clause; clause = clause1)
	{
	  clause1 = clause->next_erased_cl;
	  Unlink_And_Free_Clause(clause);
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

#if DEBUG_LEVEL >= 2
      Print_Dynamic_Info(dyn, __func__, TRUE);
#endif
    }

  longest_skip_erased = 0;
  
#if DEBUG_LEVEL >= 3
  DBGPRINTF("\\\\\\ GC-DYN-ERASE: remaining recoverable clauses: %d\n", nb_erased_clauses);
#endif
}




#if DEBUG_LEVEL != 0

/*-------------------------------------------------------------------------*
 * PRINT_DYNAMIC_CLAUSE                                                    *
 *                                                                         *
 * (debug function)                                                        *
 *-------------------------------------------------------------------------*/
void
Print_Dynamic_Clause(const char *msg, DynCInf *clause)
{
  
  MPROBE_PTR("clause", clause);	      
  MPROBE_PTR("dyn", clause->dyn);
  
  DBGPRINTF("%s %p  %s/%d  no: %d  size: %d", msg, clause,
	    pl_atom_tbl[clause->dyn->func].name, clause->dyn->arity,
	    clause->cl_no, clause->term_size);
  if (Is_Clause_Erased(clause))
    DBGPRINTF(" erased at:%" PL_FMT_u "   next erased: %p", clause->erase_stamp,
	      clause->next_erased_cl);
  DBGPRINTF("\n\t| ");  
  Print_Dynamic_Clause_Term(clause);
  DBGPRINTF("\t| seq: %p <-> %p   ind: %p <-> %p",
	    clause->seq_chain.prev, clause->seq_chain.next,
	    clause->ind_chain.prev, clause->ind_chain.next);
  DBGPRINTF("\n");
}




/*-------------------------------------------------------------------------*
 * PRINT_DYNAMIC_CLAUSE_TERM                                               *
 *                                                                         *
 * (debug function)                                                        *
 *-------------------------------------------------------------------------*/
void
Print_Dynamic_Clause_Term(DynCInf *clause)
{
  /* do not use Pl_Write(clause->head_word) since, in presence of variables, 
   * vars are globalized (put in heap) which modifies the term (no longer stored in malloc)
   */
  Pl_Copy_Contiguous_Term(H, &clause->term_word);	/* *H=<LST,H+1> */
  Pl_Write(H[1]);
  DBGPRINTF(":-");
  Pl_Write(H[2]);
  DBGPRINTF("\n");
}




/*-------------------------------------------------------------------------*
 * PRINT_DYNAMIC_INFO                                                      *
 *                                                                         *
 * (debug function)                                                        *
 *-------------------------------------------------------------------------*/
void
Print_Dynamic_Info(DynPInf *dyn, const char *loc, Bool after_clean)
{
  DBGPRINTF("--- CHECKING Dyn part (%s) ---", loc);
  MPROBE_PTR("dyn", dyn);
  DBGPRINTF("\nFirst_dyn_with_erase: %p  nb_erased_clauses: %d\n",
	    first_dyn_with_erase, nb_erased_clauses);
  DBGPRINTF("Dyn: %p  %s/%d  count_a: %d  count_z: %d  "
	    "first_erased_cl: %p  next_dyn_with_erase: %p\n",
	    dyn, pl_atom_tbl[dyn->func].name, dyn->arity, dyn->count_a, dyn->count_z,
	    dyn->first_erased_cl, dyn->next_dyn_with_erase);

  if (nb_erased_clauses < 0)
    DBGPRINTF("ERROR 1\n");

  if (after_clean && (nb_erased_clauses == 0) != (first_dyn_with_erase == NULL)) /* xor */
    DBGPRINTF("ERROR 2 nb_erased_clauses: %d  first_dyn_with_erase: %p\n",
	      nb_erased_clauses, first_dyn_with_erase);

  Print_Chain(&dyn->seq_chain, NO_INDEX, after_clean);
  Print_Chain(&dyn->var_ind_chain, VAR_INDEX, after_clean);
  Print_Hash_Table(dyn->atm_htbl, ATM_INDEX, after_clean);
  Print_Hash_Table(dyn->int_htbl, INT_INDEX, after_clean);
  Print_Chain(&dyn->lst_ind_chain, LST_INDEX, after_clean);
  Print_Hash_Table(dyn->stc_htbl, STC_INDEX, after_clean);
  DBGPRINTF("--- CHECKING END ---\n");
}




/*-------------------------------------------------------------------------*
 * PRINT_HASH_TABLE                                                        *
 *                                                                         *
 * (debug function)                                                        *
 *-------------------------------------------------------------------------*/
void
Print_Hash_Table(char *t, int index_no, Bool after_clean)
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
      DBGPRINTF("\nStructure");
      break;
    }

  for (swt = (DSwtInf *) Pl_Hash_First(t, &scan); swt; swt = (DSwtInf *) Pl_Hash_Next(&scan))
    {
      if (index_no == ATM_INDEX)
	DBGPRINTF("\n  val atm <%s>\n", pl_atom_tbl[swt->key].name);

      if (index_no == INT_INDEX)
	DBGPRINTF("\n  val int <%" PL_FMT_d ">\n", swt->key);

      if (index_no == STC_INDEX)
	DBGPRINTF("\n  val stc <%s/%d>\n", pl_atom_tbl[Functor_Of(swt->key)].name,
		  (int) Arity_Of(swt->key));

      Print_Chain(&swt->ind_chain, index_no, after_clean);
    }
}




/*-------------------------------------------------------------------------*
 * PRINT_CHAIN                                                             *
 *                                                                         *
 * (debug function)                                                        *
 *-------------------------------------------------------------------------*/
void
Print_Chain(D2ChHdr *hdr, int index_no, Bool after_clean)
{
  DynCInf *clause, *clause1;

#ifdef OPTIM_FIRST_FOR_SCAN
  if (after_clean && hdr->first != hdr->first_for_scan)
    {
      DBGPRINTF("!!! ERROR %p != %p\n", hdr->first, hdr->first_for_scan);
      exit(1);
    }
#endif

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

  for(clause = hdr->first; clause; clause = clause1)
    {
      clause1 = (index_no == NO_INDEX) ? clause->seq_chain.next : clause->ind_chain.next;
      Print_Dynamic_Clause("   ", clause);
      if (after_clean && clause->cl_no < clause->dyn->seq_chain.first->cl_no)
	printf("ERROR 4");
    }
}

#endif
