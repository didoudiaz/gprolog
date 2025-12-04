/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : WAM to mini-assembler translator                                *
 * File  : wam2ma.c                                                        *
 * Descr.: code generation                                                 *
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


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <locale.h>

#include "../EnginePl/gp_config.h"
#define ONLY_TAG_PART
#include "../EnginePl/wam_archi.h"
#include "../EnginePl/pl_params.h"
#include "../EnginePl/pred.h"

#include "wam_parser.h"
#include "wam_protos.h"
#include "../TopComp/copying.c"
#include "../TopComp/decode_hexa.c"


/* we need several maps of same type: key=string -> value=int (sequential no) */
#define MAP_KEY_TYPE char *
#define MAP_KEY_CMP(x, y) strcmp(x, y)
#define MAP_VALUE_TYPE int
#define MAP_PUT_ENTRY(map, entry) ((entry)->value = map_counter_add(map) - 1)
#include "../Tools/map_rbtree.h"


#ifdef FC_USED_TO_COMPILE_CORE
#define FAST "fast "
#else
#define FAST
#endif

#if 1
#define USE_TAGGED_CALLS_FOR_WAM_FCTS
#endif

/* basic switch_on_integer with hash table (as switch_on_atom) 
 * or else with binary search (dichotomy) via swicth_ret MA instruction
 */
#if 1
#define SWT_INT_DICHOTOMY
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define DEFAULT_OUTPUT_SUFFIX      ".ma"

#define MAX_PRED_NAME_LENGTH       2048
#define MAX_HEXA_LENGTH            MAX_PRED_NAME_LENGTH * 2 + 2 + 16
#define MAX_LABEL_LENGTH           32


#define ANY_SIZE                   1

#define FOREIGN_TYPE_INTEGER       0
#define FOREIGN_TYPE_POSITIVE      1
#define FOREIGN_TYPE_FLOAT         2
#define FOREIGN_TYPE_NUMBER        3
#define FOREIGN_TYPE_ATOM          4
#define FOREIGN_TYPE_BOOLEAN       5
#define FOREIGN_TYPE_CHAR          6
#define FOREIGN_TYPE_IN_CHAR       7
#define FOREIGN_TYPE_CODE          8
#define FOREIGN_TYPE_IN_CODE       9
#define FOREIGN_TYPE_BYTE          10
#define FOREIGN_TYPE_IN_BYTE       11
#define FOREIGN_TYPE_STRING        12
#define FOREIGN_TYPE_CHARS         13
#define FOREIGN_TYPE_CODES         14
#define FOREIGN_TYPE_TERM          15

#define FOREIGN_TBL_SIZE           16

#define FOREIGN_MODE_IN            0
#define FOREIGN_MODE_OUT           1
#define FOREIGN_MODE_IN_OUT        2




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct swt_elt
{
  struct map_entry *atom;
  PlLong n;
  int label;
}
SwtElt;




typedef struct swt_tbl *PSwtTbl;

typedef struct swt_tbl
{
  enum
  {
    TBL_ATM,			/* key: atom */
#ifndef SWT_INT_DICHOTOMY
    TBL_INT,			/* key: n */
#endif
    TBL_STC			/* key: atom/n */
  }
  type;
  int tbl_no;			/* sequential no of the table */
  PSwtTbl next;			/* next table */
  int nb_elem;			/* number of elements */
  SwtElt elem[ANY_SIZE];	/* table of switch elements */
}
SwtTbl;



typedef struct predinf *PredP;

typedef struct predinf
{
  struct map_entry *module;		/* not NULL */
  struct map_entry *functor;
  int arity;
  char *hexa;
  int line_no;
  int prop;
  struct map_entry *pl_file;
  int pl_line;
  SwtTbl *swt_tbl[3];
  PredP next;
}
Pred;



typedef struct directinf *DirectP;

typedef struct directinf
{
  struct map_entry *pl_file;
  int pl_line;
  Bool system;
  DirectP next;
}
Direct;



/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char *file_name_in;
char *file_name_out;
Bool comment;

FILE *file_out;

struct map_rbt map_atom = MAP_INIT;
struct map_rbt map_tagged_atom = MAP_INIT;
struct map_rbt map_tagged_f_n = MAP_INIT;

struct map_entry *cur_pl_file;

char buff_hexa[MAX_HEXA_LENGTH];

Pred dummy_pred_start;
Pred *pred_end = &dummy_pred_start;

Direct dummy_direct_start;
Direct *direct_end = &dummy_direct_start;

int nb_swt_tbl = 0;

Pred *cur_pred;
int cur_pred_no = 0;
int cur_arity;
int cur_sub_label;

int cur_direct_no = 0;

char *foreign_tbl[FOREIGN_TBL_SIZE];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

SwtTbl *Create_Switch_Table(int type, int nb_elem);

void Init_Foreign_Table(void);


void Emit_Obj_Initializer(void);

void Emit_Exec_Directives(void);

void Emit_One_Atom(struct map_entry *entry);

void Emit_One_Atom_Tagged(struct map_entry *entry);

int Add_F_N_Tagged(char *atom, int n);

void Emit_One_F_N_Tagged(struct map_entry *entry);

void Label_Printf(char *label, ...) ATTR_PRINTF(1);

void Inst_Printf(char *op, char *operands, ...) ATTR_PRINTF(2);


void Parse_Arguments(int argc, char *argv[]);

void Display_Help(void);




#define Check_Arg(i, str)      (strncmp(argv[i], str, strlen(argv[i])) == 0)



#define DEF_STR(str)          char *str

#define LOAD_STR(str)         Get_Arg(top, char *, str)




#define DEF_ATOM(atom)        struct map_entry *atom; char *str_##atom

#define LOAD_ATOM_T(atom, t)  Get_Arg(top, char *, str_##atom); \
			      atom = map_put(&t, str_##atom, NULL)

#define LOAD_ATOM_0(atom)     LOAD_ATOM_T(atom, map_atom)
#define LOAD_ATOM_1(atom)     LOAD_ATOM_T(atom, map_tagged_atom)

#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
#define LOAD_ATOM(atom)       LOAD_ATOM_1(atom)
#else
#define LOAD_ATOM(atom)       LOAD_ATOM_0(atom)
#endif




#define DEF_INTEGER(n)        PlLong n

#define LOAD_INTEGER(n)       Get_Arg(top, PlLong, n)


	/* a sub-type of INTEGER which fit in a C int type */
#define DEF_C_INT(n)          int n

#define LOAD_C_INT(n)         Get_Arg(top, int, n)



#define DEF_FLOAT(n)          double n

#define LOAD_FLOAT(n)         Get_Arg(top, double, n)




#define DEF_X_Y(xy)           int xy; char c

#define LOAD_X_Y(xy)          Get_Arg(top, int, xy); \
                              if (xy < 5000) c = 'X'; else xy -= 5000, c='Y'




#define DEF_F_N_0(atom, n)    DEF_ATOM(atom); DEF_C_INT(n)
#define DEF_F_N_1(atom, n)    DEF_STR(str_##atom); DEF_C_INT(n); int f_n_no

#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
#define DEF_F_N(atom, n)      DEF_F_N_1(atom, n)
#else
#define DEF_F_N(atom, n)      DEF_F_N_0(atom, n)
#endif

#define LOAD_F_N_0(atom, n)   LOAD_ATOM_0(atom); LOAD_C_INT(n)
#define LOAD_F_N_1(atom, n)   LOAD_STR(str_##atom); LOAD_C_INT(n);\
                              f_n_no = Add_F_N_Tagged(str_##atom, (int) n)

#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
#define LOAD_F_N(atom, n)     LOAD_F_N_1(atom, n)
#else
#define LOAD_F_N(atom, n)     LOAD_F_N_0(atom, n)
#endif




#define DEF_MP_N(m, p, n)     DEF_STR(m); DEF_STR(p); DEF_C_INT(n)

#define LOAD_MP_N(m, p, n)    LOAD_STR(m); LOAD_STR(p); LOAD_C_INT(n)



#define DEF_LABEL(l)          char l[MAX_LABEL_LENGTH]; int val_##l

#define LOAD_LABEL(l)         Get_Arg(top, int, val_##l); \
                              if (val_##l==-1) strcpy(l, "0"); \
                              else sprintf(l, FORMAT_LABEL(val_##l))




#define Args0                 ArgVal *top = arg

#define Args1(a1)             ArgVal *top = arg; DEF_##a1; \
                              LOAD_##a1

#define Args2(a1, a2)         ArgVal *top = arg; DEF_##a1; DEF_##a2; \
                              LOAD_##a1; LOAD_##a2

#define Args3(a1, a2, a3)     ArgVal *top = arg; DEF_##a1; DEF_##a2; DEF_##a3;\
                              LOAD_##a1; LOAD_##a2; LOAD_##a3

#define Args4(a1, a2, a3, a4) ArgVal *top = arg; \
                              DEF_##a1; DEF_##a2; DEF_##a3; DEF_##a4; \
                              LOAD_##a1; LOAD_##a2; LOAD_##a3; LOAD_##a4

#define Args5(a1, a2, a3, a4, a5) \
                              ArgVal *top = arg; \
                              DEF_##a1; DEF_##a2; DEF_##a3; DEF_##a4; DEF_##a5; \
                              LOAD_##a1; LOAD_##a2; LOAD_##a3; LOAD_##a4; LOAD_##a5

#define Args6(a1, a2, a3, a4, a5, a6) \
                              ArgVal *top = arg; \
                              DEF_##a1; DEF_##a2; DEF_##a3; DEF_##a4; DEF_##a5; DEF_##a6; \
                              LOAD_##a1; LOAD_##a2; LOAD_##a3; LOAD_##a4; LOAD_##a5; LOAD_##a6



#define FORMAT_LABEL(l)       ".pred%d_%d", cur_pred_no, (l)

#define FORMAT_SUB_LABEL(sl)  ".pred%d_sub_%d", cur_pred_no, (sl)


#define CREATE_CHOICE_INST(l)                                               \
  if (cur_arity >= 1 && cur_arity <= 4)                                     \
    Inst_Printf("call_c", FAST "Pl_Create_Choice_Point%d(&%s)", cur_arity, l); \
  else                                                                      \
    Inst_Printf("call_c", FAST "Pl_Create_Choice_Point(&%s,%d)", l, cur_arity)


#define UPDATE_CHOICE_INST(l)                                               \
  if (cur_arity >= 1 && cur_arity <= 4)                                     \
    Inst_Printf("call_c", FAST "Pl_Update_Choice_Point%d(&%s)", cur_arity, l); \
  else                                                                      \
    Inst_Printf("call_c", FAST "Pl_Update_Choice_Point(&%s,%d)", l, cur_arity)


#define DELETE_CHOICE_INST                                                  \
  if (cur_arity >= 1 && cur_arity <= 4)                                     \
    Inst_Printf("call_c", FAST "Pl_Delete_Choice_Point%d()", cur_arity);    \
  else                                                                      \
    Inst_Printf("call_c", FAST "Pl_Delete_Choice_Point(%d)", cur_arity)




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  setlocale(LC_ALL, "");	/* really needed (to be checked) */
  setlocale(LC_NUMERIC, "C");	/* make sure floats come out right... */

  Parse_Arguments(argc, argv);

  if (file_name_out == NULL)
    file_out = stdout;
  else if ((file_out = fopen(file_name_out, "wt")) == NULL)
    {
      fprintf(stderr, "cannot open output file %s\n", file_name_out);
      exit(1);
    }

#if 0	  /* done with MAP_INIT */
  map_init(&map_atom);
  map_init(&map_tagged_atom);
  map_init(&map_tagged_f_n);
#endif

  Init_Foreign_Table();
  dummy_pred_start.next = NULL;
  dummy_direct_start.next = NULL;

  if (!Parse_Wam_File(file_name_in, comment))
    {
      fprintf(stderr, "Translation aborted\n");
      exit(1);
    }

  Emit_Obj_Initializer();
  Emit_Exec_Directives();

  if (file_out != stdout)
    fclose(file_out);

  exit(0);
}




/*-------------------------------------------------------------------------*
 * SOURCE_LINE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Source_Line(int line_no, char *cmt)
{
  Label_Printf("\t; %6d: %s", line_no, cmt);
}




/*-------------------------------------------------------------------------*
 * F_FILE_NAME                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_file_name(ArgVal arg[])
{
  Args1(STR(pl_file));

  cur_pl_file = map_put(&map_atom, pl_file, NULL);
}



/*-------------------------------------------------------------------------*
 * F_PREDICATE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_predicate(ArgVal arg[])
{
  struct map_entry *atom_module = NULL;
  struct map_entry *atom_functor;
  Bool module_user_system = FALSE;
  int prop = 0;			/* init for the compiler */
  Bool local_symbol = FALSE;
  Args6(MP_N(module, functor, arity), C_INT(pl_line),
	STR(static_dynamic), STR(public_private), STR(mono_multi), STR(built_in_local_global));

  if (cur_pl_file == NULL)
    Syntax_Error("file_name declaration missing");

  atom_functor = map_put(&map_atom, functor, NULL);

  cur_arity = (int) arity;
  cur_sub_label = 0;

  if (strcmp(static_dynamic, "dynamic") == 0)
    prop = MASK_PRED_DYNAMIC;
  else if (strcmp(static_dynamic, "static") == 0)
    prop = MASK_PRED_NATIVE_CODE;
  else
    Syntax_Error("static or dynamic expected");

  if (strcmp(public_private, "public") == 0)
    prop |= MASK_PRED_PUBLIC;
  else if (strcmp(public_private, "private") != 0)
    Syntax_Error("public or private expected");

  if (strcmp(mono_multi, "monofile") == 0)
    ;
  else if (strcmp(mono_multi, "multifile") == 0)
    {
      prop |= MASK_PRED_MULTIFILE;
      prop &= ~MASK_PRED_NATIVE_CODE; /* if multifile it needs to be emulated */
    }
  else
    {
      Syntax_Error("multifile or multifile expected");
    }



  if (strcmp(built_in_local_global, "built_in") == 0)
    prop |= MASK_PRED_BUILTIN;
  else if (strcmp(built_in_local_global, "built_in_fd") == 0)
    prop |= MASK_PRED_BUILTIN_FD;
  else if (strcmp(built_in_local_global, "local") == 0)
    local_symbol = TRUE;
  else if (strcmp(built_in_local_global, "user") != 0 &&
	   strcmp(built_in_local_global, "global") != 0)
    Syntax_Error("built_in, built_in_fd, local or global (or user) expected");
  /* 'user' is accepted for compatibility as 'global' - no longer generated */


  if (!local_symbol)
    prop |= MASK_PRED_EXPORTED;

  cur_pred_no++;

  cur_pred = (Pred *) malloc(sizeof(Pred));
  if (cur_pred == NULL)
    {
      fprintf(stderr, "Cannot allocate memory for predicate #%d (%s/%d)\n",
	      cur_pred_no, functor, arity);
      exit(1);
    }

  if (module == NULL || *module == '\0') /* 'module' should be given in the future */
    {
      if (prop & MASK_PRED_BUILTIN || prop & MASK_PRED_BUILTIN_FD)
	module = "system";
      else
	module = "user";
    }
  atom_module = map_put(&map_atom, module, NULL);
  if (strcmp(module, "user") == 0 || strcmp(module, "system") == 0)
    module_user_system = TRUE;


  cur_pred->module = atom_module;
  cur_pred->functor = atom_functor;
  cur_pred->arity = (int) arity;
  cur_pred->pl_file = cur_pl_file;
  cur_pred->pl_line = (int) pl_line;
  cur_pred->prop = prop;
  cur_pred->swt_tbl[0] = NULL;
  cur_pred->swt_tbl[1] = NULL;
  cur_pred->swt_tbl[2] = NULL;
  cur_pred->next = NULL;

  pred_end->next = cur_pred;
  pred_end = cur_pred;

  if (comment)
    {
      Label_Printf("\n\n; *** Predicate: %s:%s/%d (%s:%d)",
		   module, functor, arity, cur_pl_file->key, pl_line);
    }

  /* do not qualif with module in Encode_Hexa if:
   *    - it is not an exported predicate (i.e. it is a local_symbol)
   *    - it belongs to module 'user' or 'system'
   */

  Encode_Hexa((local_symbol || module_user_system) ? NULL : module, functor, (int) arity, buff_hexa + 1);
  *buff_hexa = '&';
  cur_pred->hexa = strdup(buff_hexa);

  Label_Printf("\n\npl_code %s %s", (local_symbol) ? "local" : "global", buff_hexa + 1);
}




/*-------------------------------------------------------------------------*
 * F_DIRECTIVE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_directive(ArgVal arg[])
{
  Direct *p;
  Bool system = FALSE;		/* init for the compiler */
  Args2(C_INT(pl_line), STR(user_system));

  if (cur_pl_file == NULL)
    Syntax_Error("file_name declaration missing");


  if (strcmp(user_system, "system") == 0)
    system = TRUE;
  else if (strcmp(user_system, "user") == 0)
    system = FALSE;
  else
    Syntax_Error("user or system expected");


  cur_direct_no++;
  p = (Direct *) malloc(sizeof(Direct));
  if (p == NULL)
    {
      fprintf(stderr, "Cannot allocate memory for directive #%d\n", cur_direct_no);
      exit(1);
    }

  p->pl_file = cur_pl_file;
  p->pl_line = (int) pl_line;
  p->system = system;
  p->next = NULL;

  direct_end->next = p;
  direct_end = p;

  if (comment)
    Label_Printf("\n\n; *** %s Directive (%s:%d)", (system) ? "System" : "User", cur_pl_file->key, pl_line);

  Label_Printf("\n\npl_code local directive_%d", cur_direct_no);
}




/*-------------------------------------------------------------------------*
 * F_ENSURE_LINKED                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_ensure_linked(ArgVal arg[])
{
  DEF_MP_N(m, p, n);
  Args1(C_INT(nb_elem));

  Label_Printf("\n\npl_code local ensure_linked");
  while (nb_elem--)
    {
      LOAD_MP_N(m, p, n);
      Encode_Hexa(m, p, (int) n, buff_hexa);
      Inst_Printf("pl_jump", "%s", buff_hexa);
    }
}




/*-------------------------------------------------------------------------*
 * F_GET_VARIABLE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_variable(ArgVal arg[])
{
  Args2(X_Y(xy), C_INT(a));
  Inst_Printf("move", "X(%d),%c(%d)", a, c, xy);
}




/*-------------------------------------------------------------------------*
 * F_GET_VALUE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_value(ArgVal arg[])
{
  Args2(X_Y(xy), C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Unify(%c(%d),X(%d))", c, xy, a);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_ATOM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_atom(ArgVal arg[])
{
  Args2(ATOM(atom), C_INT(a));
#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
  Inst_Printf("call_c", FAST "Pl_Get_Atom_Tagged(ta(%d),X(%d))", atom->value, a);
#else
  Inst_Printf("call_c", FAST "Pl_Get_Atom(at(%d),X(%d))", atom->value, a);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_INTEGER                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_integer(ArgVal arg[])
{
  Args2(INTEGER(n), C_INT(a));
#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
  Inst_Printf("call_c", FAST "Pl_Get_Integer_Tagged(%" PL_FMT_d ",X(%d))", Tag_INT(n), a);
#else
  Inst_Printf("call_c", FAST "Pl_Get_Integer(%" PL_FMT_d ",X(%d))", n, a);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_FLOAT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_float(ArgVal arg[])
{
  Args2(FLOAT(n), C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Get_Float(%1.20e,X(%d))", n, a);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_NIL                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_nil(ArgVal arg[])
{
  Args1(C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Get_Nil(X(%d))", a);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_LIST                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_list(ArgVal arg[])
{
  Args1(C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Get_List(X(%d))", a);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_STRUCTURE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_structure(ArgVal arg[])
{
  Args2(F_N(atom, n), C_INT(a));
#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
  Inst_Printf("call_c", FAST "Pl_Get_Structure_Tagged(fn(%d),X(%d))", f_n_no, a);
#else
  Inst_Printf("call_c", FAST "Pl_Get_Structure(at(%d),%d,X(%d))", atom->value, n, a);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_PUT_VARIABLE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_variable(ArgVal arg[])
{
  Args2(X_Y(xy), C_INT(a));
  if (c == 'X')
    {
      Inst_Printf("call_c", FAST "Pl_Put_X_Variable()");
      Inst_Printf("move_ret", "X(%d)", a);
      Inst_Printf("move", "X(%d),X(%d)", a, xy);
    }
  else
    {
      Inst_Printf("call_c", FAST "Pl_Put_Y_Variable(&Y(%d))", xy);
      Inst_Printf("move_ret", "X(%d)", a);
    }
}




/*-------------------------------------------------------------------------*
 * F_PUT_VOID                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_void(ArgVal arg[])
{
  Args1(C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Put_X_Variable()");
  Inst_Printf("move_ret", "X(%d)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_VALUE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_value(ArgVal arg[])
{
  Args2(X_Y(xy), C_INT(a));
  Inst_Printf("move", "%c(%d),X(%d)", c, xy, a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_UNSAFE_VALUE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_unsafe_value(ArgVal arg[])
{
  Args2(X_Y(xy), C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Put_Unsafe_Value(%c(%d))", c, xy);
  Inst_Printf("move_ret", "X(%d)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_ATOM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_atom(ArgVal arg[])
{
  Args2(ATOM(atom), C_INT(a));
#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
  Inst_Printf("call_c", FAST "Pl_Put_Atom_Tagged(ta(%d))", atom->value);
#else
  Inst_Printf("call_c", FAST "Pl_Put_Atom(at(%d))", atom->value);
#endif
  Inst_Printf("move_ret", "X(%d)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_INTEGER                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_integer(ArgVal arg[])
{
  Args2(INTEGER(n), C_INT(a));
#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
  Inst_Printf("call_c", FAST "Pl_Put_Integer_Tagged(%" PL_FMT_d ")", Tag_INT(n));
#else
  Inst_Printf("call_c", FAST "Pl_Put_Integer(%" PL_FMT_d ")", n);
#endif
  Inst_Printf("move_ret", "X(%d)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_FLOAT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_float(ArgVal arg[])
{
  Args2(FLOAT(n), C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Put_Float(%1.20e)", n);
  Inst_Printf("move_ret", "X(%d)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_NIL                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_nil(ArgVal arg[])
{
  Args1(C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Put_Nil()");
  Inst_Printf("move_ret", "X(%d)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_LIST                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_list(ArgVal arg[])
{
  Args1(C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Put_List()");
  Inst_Printf("move_ret", "X(%d)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_STRUCTURE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_structure(ArgVal arg[])
{
  Args2(F_N(atom, n), C_INT(a));
#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
  Inst_Printf("call_c", FAST "Pl_Put_Structure_Tagged(fn(%d))", f_n_no);
#else
  Inst_Printf("call_c", FAST "Pl_Put_Structure(at(%d),%d)", atom->value, n);
#endif
  Inst_Printf("move_ret", "X(%d)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_META_TERM                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_meta_term(ArgVal arg[])
{
  Args3(ATOM(module), C_INT(x), C_INT(a));
#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
  Inst_Printf("call_c", FAST "Pl_Put_Meta_Term_Tagged(ta(%d), X(%d))", module->value, x);
#else
  Inst_Printf("call_c", FAST "Pl_Put_Meta_Term(at(%d), X(%d))", module->value, x);
#endif
  Inst_Printf("move_ret", "X(%d)", a);
}




/*-------------------------------------------------------------------------*
 * F_MATH_LOAD_VALUE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_math_load_value(ArgVal arg[])
{
  Args2(X_Y(xy), C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Math_Load_Value(%c(%d),&X(%d))", c, xy, a);
}




/*-------------------------------------------------------------------------*
 * F_MATH_FAST_LOAD_VALUE                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_math_fast_load_value(ArgVal arg[])
{
  Args2(X_Y(xy), C_INT(a));
  Inst_Printf("call_c", FAST "Pl_Math_Fast_Load_Value(%c(%d),&X(%d))", c, xy, a);
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_VARIABLE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_variable(ArgVal arg[])
{
  Args1(X_Y(xy));
  Inst_Printf("call_c", FAST "Pl_Unify_Variable()");
  Inst_Printf("move_ret", "%c(%d)", c, xy);
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_VOID                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_void(ArgVal arg[])
{
  Args1(C_INT(n));
  Inst_Printf("call_c", FAST "Pl_Unify_Void(%d)", n);
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_VALUE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_value(ArgVal arg[])
{
  Args1(X_Y(xy));
  Inst_Printf("call_c", FAST "Pl_Unify_Value(%c(%d))", c, xy);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_LOCAL_VALUE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_local_value(ArgVal arg[])
{
  Args1(X_Y(xy));
  Inst_Printf("call_c", FAST "Pl_Unify_Local_Value(%c(%d))", c, xy);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_ATOM                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_atom(ArgVal arg[])
{
  Args1(ATOM(atom));
#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
  Inst_Printf("call_c", FAST "Pl_Unify_Atom_Tagged(ta(%d))", atom->value);
#else
  Inst_Printf("call_c", FAST "Pl_Unify_Atom(at(%d))", atom->value);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_INTEGER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_integer(ArgVal arg[])
{
  Args1(INTEGER(n));
#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
  Inst_Printf("call_c", FAST "Pl_Unify_Integer_Tagged(%" PL_FMT_d ")", Tag_INT(n));
#else
  Inst_Printf("call_c", FAST "Pl_Unify_Integer(%" PL_FMT_d ")", n);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_NIL                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_nil(ArgVal arg[])
{
  Inst_Printf("call_c", FAST "Pl_Unify_Nil()");
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_LIST                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_list(ArgVal arg[])
{
  Inst_Printf("call_c", FAST "Pl_Unify_List()");
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_STRUCTURE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_structure(ArgVal arg[])
{
  Args1(F_N(atom, n));
#ifdef USE_TAGGED_CALLS_FOR_WAM_FCTS
  Inst_Printf("call_c", FAST "Pl_Unify_Structure_Tagged(fn(%d))", f_n_no);
#else
  Inst_Printf("call_c", FAST "Pl_Unify_Structure(at(%d),%" PL_FMT_d ")", atom->value, n);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_ALLOCATE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_allocate(ArgVal arg[])
{
  Args1(C_INT(n));
  Inst_Printf("call_c", FAST "Pl_Allocate(%d)", n);
}




/*-------------------------------------------------------------------------*
 * F_DEALLOCATE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_deallocate(ArgVal arg[])
{
  Inst_Printf("call_c", FAST "Pl_Deallocate()");
}




/*-------------------------------------------------------------------------*
 * F_CALL                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_call(ArgVal arg[])
{
  Args1(MP_N(m, p, n));

  Encode_Hexa(m, p, (int) n, buff_hexa);

  Inst_Printf("pl_call", "%s", buff_hexa);
}




/*-------------------------------------------------------------------------*
 * F_EXECUTE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_execute(ArgVal arg[])
{
  Args1(MP_N(m, p, n));

  Encode_Hexa(m, p, (int) n, buff_hexa);

  Inst_Printf("pl_jump", "%s", buff_hexa);
}




/*-------------------------------------------------------------------------*
 * F_PROCEED                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_proceed(ArgVal arg[])
{
  Inst_Printf("pl_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_FAIL                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_fail(ArgVal arg[])
{
  Inst_Printf("pl_fail", "");
}




/*-------------------------------------------------------------------------*
 * F_LABEL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_label(ArgVal arg[])
{
  Args1(LABEL(l));
  Label_Printf("\n%s:", l);
}




/*-------------------------------------------------------------------------*
 * F_SWITCH_ON_TERM                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_switch_on_term(ArgVal arg[])
{
#define NB_SWT_LIST 5

#define LVAR 1
#define LATM 2
#define LINT 4
#define LLST 8
#define LSTC 16

  Args0;
  DEF_C_INT(val_label);
  static char l[NB_SWT_LIST][MAX_LABEL_LENGTH];
  int mask = 0, i;

  for (i = 0; i < NB_SWT_LIST; i++)
    {
      LOAD_C_INT(val_label);

      if (val_label == -1)
	strcpy(l[i], "0");
      else
	{
	  sprintf(l[i], "&" FORMAT_LABEL(val_label));
	  mask |= (1 << i);
	}
    }

  switch(mask)			/* some specialized functions */
    {
    case LVAR | LATM:
      Inst_Printf("call_c", FAST "Pl_Switch_On_Term_Var_Atm(%s,%s)", l[0], l[1]);
      break;

    case LVAR | LSTC:
      Inst_Printf("call_c", FAST "Pl_Switch_On_Term_Var_Stc(%s,%s)", l[0], l[4]);
      break;

    case LVAR | LATM | LLST:
      Inst_Printf("call_c", FAST "Pl_Switch_On_Term_Var_Atm_Lst(%s,%s,%s)", l[0], l[1], l[3]);
      break;

    case LVAR | LATM | LSTC:
      Inst_Printf("call_c", FAST "Pl_Switch_On_Term_Var_Atm_Stc(%s,%s,%s)", l[0], l[1], l[4]);
      break;

    default:
      Inst_Printf("call_c", FAST "Pl_Switch_On_Term(%s,%s,%s,%s,%s)", l[0], l[1], l[2], l[3], l[4]);
      break;
    }

  Inst_Printf("jump_ret", "");
}




/*-------------------------------------------------------------------------*
 * CREATE_SWITCH_TABLE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
SwtTbl *
Create_Switch_Table(int type, int nb_elem)
{
  SwtTbl *t;

  t = (SwtTbl *) malloc(sizeof(SwtTbl) + sizeof(SwtElt) * (nb_elem - 1));
  if (t == NULL)
    {
      fprintf(stderr, "Cannot allocate memory for switch table\n");
      exit(1);
    }

  t->type = type;
  t->tbl_no = nb_swt_tbl++;
  t->next = cur_pred->swt_tbl[type];
  t->nb_elem = nb_elem;

  cur_pred->swt_tbl[type] = t;

  return t;
}




/*-------------------------------------------------------------------------*
 * F_SWITCH_ON_ATOM                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_switch_on_atom(ArgVal arg[])
{
  SwtTbl *t;
  SwtElt *elem;

  DEF_STR(str);
  DEF_C_INT(label);
  Args1(C_INT(nb_elem));


  t = Create_Switch_Table(TBL_ATM, (int) nb_elem);

  for (elem = t->elem; nb_elem--; elem++)
    {
      LOAD_STR(str);
      LOAD_C_INT(label);
      elem->atom = map_put(&map_atom, str, NULL);
      elem->label = label;
    }

  Inst_Printf("call_c", FAST "Pl_Switch_On_Atom(st(%d),%d)", nb_swt_tbl - 1, t->nb_elem);
  Inst_Printf("jump_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_SWITCH_ON_INTEGER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_switch_on_integer(ArgVal arg[])
{
#ifndef SWT_INT_DICHOTOMY	/* basic switch_on_integer */
  SwtTbl *t;
  SwtElt *elem;

  DEF_INTEGER(n);
  DEF_C_INT(label);
  Args1(C_INT(nb_elem));

  t = Create_Switch_Table(TBL_INT, nb_elem);

  for (elem = t->elem; nb_elem--; elem++)
    {
      LOAD_INTEGER(n);
      LOAD_C_INT(label);
      elem->n = n;
      elem->label = label;
    }

  Inst_Printf("call_c", FAST "Pl_Switch_On_Integer(st(%d),%d)", nb_swt_tbl - 1, t->nb_elem);
  Inst_Printf("jump_ret", "");

#else

  char c;
  int i;

  DEF_INTEGER(n);
  DEF_LABEL(l);
  Args1(C_INT(nb_elem));

  Inst_Printf("call_c", FAST "Pl_Switch_On_Integer_For_Dichotomy()");
  Inst_Printf("switch_ret", NULL);	/* NULL to avoid newline */
  c = '(';
  for(i = 0; i < nb_elem; i++)
    {
      LOAD_C_INT(n);
      LOAD_LABEL(l);
      if (i % 5 == 0 && i > 0)
	{
	  fprintf(file_out, "%c\\\n\t\t   ", c);
	  c = ' ';
	}
      fprintf(file_out, "%c%" PL_FMT_d "=%s", c, n, l);
      c = ',';
    }
  fprintf(file_out, ")\n");
#endif
}




/*-------------------------------------------------------------------------*
 * F_SWITCH_ON_STRUCTURE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_switch_on_structure(ArgVal arg[])
{
  SwtTbl *t;
  SwtElt *elem;

  DEF_STR(str);
  DEF_C_INT(arity);
  DEF_C_INT(label);
  Args1(C_INT(nb_elem));


  t = Create_Switch_Table(TBL_STC, (int) nb_elem);

  for (elem = t->elem; nb_elem--; elem++)
    {
      LOAD_STR(str);
      LOAD_C_INT(arity);
      LOAD_C_INT(label);
      elem->atom = map_put(&map_atom, str, NULL);
      elem->n = arity;
      elem->label = label;
    }

  Inst_Printf("call_c", FAST "Pl_Switch_On_Structure(st(%d),%d)", nb_swt_tbl - 1, t->nb_elem);
  Inst_Printf("jump_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_TRY_ME_ELSE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_try_me_else(ArgVal arg[])
{
  Args1(LABEL(l));
  CREATE_CHOICE_INST(l);
}




/*-------------------------------------------------------------------------*
 * F_RETRY_ME_ELSE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_retry_me_else(ArgVal arg[])
{
  Args1(LABEL(l));
  UPDATE_CHOICE_INST(l);
}




/*-------------------------------------------------------------------------*
 * F_TRUST_ME_ELSE_FAIL                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_trust_me_else_fail(ArgVal arg[])
{
  DELETE_CHOICE_INST;
}




/*-------------------------------------------------------------------------*
 * F_TRY                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_try(ArgVal arg[])
{
  char sl[MAX_LABEL_LENGTH];

  Args1(LABEL(l));

  sprintf(sl, FORMAT_SUB_LABEL(cur_sub_label++));

  CREATE_CHOICE_INST(sl);
  Inst_Printf("jump", "%s", l);
  Label_Printf("%s:", sl);
}




/*-------------------------------------------------------------------------*
 * F_RETRY                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_retry(ArgVal arg[])
{
  char sl[MAX_LABEL_LENGTH];

  Args1(LABEL(l));

  sprintf(sl, FORMAT_SUB_LABEL(cur_sub_label++));

  UPDATE_CHOICE_INST(sl);
  Inst_Printf("jump", "%s", l);
  Label_Printf("%s:", sl);
}




/*-------------------------------------------------------------------------*
 * F_TRUST                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_trust(ArgVal arg[])
{
  Args1(LABEL(l));

  DELETE_CHOICE_INST;
  Inst_Printf("jump", "%s", l);
}




/*-------------------------------------------------------------------------*
 * F_PRAGMA_ARITY                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_pragma_arity(ArgVal arg[])
{
  Args1(C_INT(a));

  /* Used for for a pred/arity with cuts (not soft cuts).
   * Since the cut level is stored in X(arity) we have to save it in choice-points
   * This pragma adjusts the number of args to save in choice-points.
   */

  cur_arity = (int) a;	
}





/*-------------------------------------------------------------------------*
 * F_GET_CURRENT_CHOICE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_current_choice(ArgVal arg[])
{
  Args1(X_Y(xy));

  Inst_Printf("call_c", FAST "Pl_Get_Current_Choice()");
  Inst_Printf("move_ret", "%c(%d)", c, xy);
}




/*-------------------------------------------------------------------------*
 * F_CUT                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_cut(ArgVal arg[])
{
  Args1(X_Y(xy));
  Inst_Printf("call_c", FAST "Pl_Cut(%c(%d))", c, xy);
}




/*-------------------------------------------------------------------------*
 * F_CUT                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_soft_cut(ArgVal arg[])
{
  Args1(X_Y(xy));
  Inst_Printf("call_c", FAST "Pl_Soft_Cut(%c(%d))", c, xy);
}




/*-------------------------------------------------------------------------*
 * F_CALL_C                                                                *
 *                                                                         *
 * call_c(F, [T,...], [W,...])                                             *
 *   F=FctName, T=option only these options are relevant:                  *
 *    - jump/boolean/x(X)/y(Y) (jump at / test / move returned value)      *
 *    - set_cp (set CP before the call at the next instruction)            *
 *    - fast_call (use a fast call convention)                             *
 *    - tagged (use tagged calls for atoms, integers and F/N)              *
 *   W= atom  &,fun,arity  integer  double  x(X)  y(Y)  &,x(X)  &,y(Y)     *
 *-------------------------------------------------------------------------*/
void
F_call_c(ArgVal arg[])
{
  int ret = 0;			/* 1: boolean, 2: jump, 3: move */
  Bool fast_call = FALSE;
  Bool tagged = FALSE;
  Bool set_cp = FALSE;
  char *str;
  Bool adr_of;
  int ret_xy = 0;		/* init for the compiler */
  char ret_c = 0;		/* init for the compiler */
  int i;

  DEF_STR(c_option);
  DEF_C_INT(arg_type);
  DEF_ATOM(atom);
  DEF_STR(aux_functor);
  DEF_C_INT(aux_arity);
  DEF_INTEGER(n);
  DEF_FLOAT(n1);
  DEF_X_Y(xy);

  Args2(STR(fct_name), C_INT(nb_elem));

  for (i = 0; i < nb_elem; i++)
    {
      LOAD_C_INT(arg_type);
      if (arg_type == X_Y)	/* move_ret x(X) or y(Y) */
	{
	  LOAD_X_Y(xy);
	  ret = 3;
	  ret_xy = xy;
	  ret_c = c;
	  continue;
	}
      /* else should be ATOM */
      LOAD_STR(c_option);
      if (strcmp(c_option, "boolean") == 0)
	ret = 1;
      else if (strcmp(c_option, "jump") == 0)
	ret = 2;
      else if (strcmp(c_option, "fast_call") == 0)
	fast_call = TRUE;
      else if (strcmp(c_option, "tagged") == 0)
	tagged = TRUE;
      else if (strcmp(c_option, "set_cp") == 0)
	set_cp = TRUE;
    }

  LOAD_C_INT(nb_elem);

  if (set_cp)
    Inst_Printf("prep_cp", "");

  Inst_Printf("call_c", NULL);
  if (fast_call)
    fputs(FAST "", file_out);

  fprintf(file_out, "%s(", fct_name);

  i = 0;
  adr_of = FALSE;
  goto write_a_arg;
  while(i < nb_elem)
    {
      fputc(',', file_out);

    write_a_arg:
      LOAD_C_INT(arg_type);
      switch(arg_type)
	{
	case ATOM:		/* detect  &,func,arity   &,x(X)   &,y(Y) */
	  str = *((char **) top);
	  if (*str == '&' && str[1] == '\0')
	    {
	      if ((i < nb_elem - 1 && *(PlLong *) (top+1) == X_Y) ||
		  (i < nb_elem - 2 && *(PlLong *) (top+1) == ATOM && *(PlLong *) (top+3) == INTEGER))
		{
		  adr_of = TRUE;
		  i++;
		  top++;
		  goto write_a_arg;
		}
	    }

	  if (adr_of)
	    {
	      LOAD_STR(aux_functor);
	      i++;
	      top++;
	      LOAD_C_INT(aux_arity);
	      Encode_Hexa(NULL, aux_functor, (int) aux_arity, buff_hexa);
	      fprintf(file_out, "&%s", buff_hexa);
	      adr_of = FALSE;
	    }
	  else if (tagged)
	    {
	      LOAD_ATOM_1(atom);
	      fprintf(file_out, "ta(%d)", atom->value);
	    }
	  else
	    {
	      LOAD_ATOM_0(atom);
	      fprintf(file_out, "at(%d)", atom->value);
	    }
	  break;

	case INTEGER:
	  LOAD_INTEGER(n);
	  fprintf(file_out, "%" PL_FMT_d, (tagged) ? (PlLong) Tag_INT(n) : n);
	  break;

	case FLOAT:
	  LOAD_FLOAT(n1);
	  fprintf(file_out, "%1.20e", n1);
	  break;

	case X_Y:
	  LOAD_X_Y(xy);
	  if (adr_of)
	    {
	      fprintf(file_out, "&");
	      adr_of = FALSE;
	    }
	  fprintf(file_out, "%c(%d)", c, xy);
	  break;

	case F_N:
	  if (tagged)
	    {
	      DEF_F_N_1(atom, n);
	      LOAD_F_N_1(atom, n);
	      fprintf(file_out, "fn(%d)", f_n_no);
	    }
	  else
	    {
	      DEF_F_N_0(atom, n);
	      LOAD_F_N_0(atom, n);
	      fprintf(file_out, "at(%d),%d", atom->value, n);
	    }
	  break;
	}
      i++;
    }

  fprintf(file_out, ")\n");
  if (ret == 1)
    Inst_Printf("fail_ret", "");
  else if (ret == 2)
    Inst_Printf("jump_ret", "");
  else if (ret == 3)
    Inst_Printf("move_ret", "%c(%d)", ret_c, ret_xy);

  if (set_cp)
    Inst_Printf("here_cp", "");
}




/*-------------------------------------------------------------------------*
 * INIT_FOREIGN_TABLE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Init_Foreign_Table(void)
{
  foreign_tbl[FOREIGN_TYPE_INTEGER] = "Integer";
  foreign_tbl[FOREIGN_TYPE_POSITIVE] = "Positive";
  foreign_tbl[FOREIGN_TYPE_FLOAT] = "Float";
  foreign_tbl[FOREIGN_TYPE_NUMBER] = "Number";
  foreign_tbl[FOREIGN_TYPE_ATOM] = "Atom";
  foreign_tbl[FOREIGN_TYPE_BOOLEAN] = "Boolean";
  foreign_tbl[FOREIGN_TYPE_CHAR] = "Char";
  foreign_tbl[FOREIGN_TYPE_IN_CHAR] = "In_Char";
  foreign_tbl[FOREIGN_TYPE_CODE] = "Code";
  foreign_tbl[FOREIGN_TYPE_IN_CODE] = "In_Code";
  foreign_tbl[FOREIGN_TYPE_BYTE] = "Byte";
  foreign_tbl[FOREIGN_TYPE_IN_BYTE] = "In_Byte";
  foreign_tbl[FOREIGN_TYPE_STRING] = "String";
  foreign_tbl[FOREIGN_TYPE_CHARS] = "Chars";
  foreign_tbl[FOREIGN_TYPE_CODES] = "Codes";
  foreign_tbl[FOREIGN_TYPE_TERM] = "Term";
}




/*-------------------------------------------------------------------------*
 * F_FOREIGN_CALL_C                                                        *
 *                                                                         *
 * foreign_call_c(F, T0, P/N, K, [(M1, T1),...])                           *
 *   F=FctName, T0=Return, P/N=BipName/BipArity, K=ChcSize                 *
 *   Mi=mode (in/out/in_out), Ti=type                                      *
 *-------------------------------------------------------------------------*/
void
F_foreign_call_c(ArgVal arg[])

#define F_Double(t) ((t)==FOREIGN_TYPE_FLOAT || (t)==FOREIGN_TYPE_NUMBER)
#define F_Array_Letter(t) (F_Double(t) ? 'D' : 'L')

{
  static int mode[NB_OF_X_REGS], type[NB_OF_X_REGS];
  int i, j, n, fio_arg_index = 0, nb_c_str = 0, s_dup, complex_jump_ret = 0;
  char c;
  char l[MAX_LABEL_LENGTH];

  DEF_STR(str_mode);
  DEF_STR(str_type);

  Args6(STR(fct_name), STR(ret_mode), STR(bip_name), C_INT(bip_arity), C_INT(chc_size), C_INT(nb_elem));

  for (i = 0; i < nb_elem; i++)
    {
      LOAD_STR(str_mode);
      LOAD_STR(str_type);

      if (strcmp(str_mode, "in") == 0)
	mode[i] = FOREIGN_MODE_IN;
      else if (strcmp(str_mode, "out") == 0)
	mode[i] =  FOREIGN_MODE_OUT;
      else if (strcmp(str_mode, "in_out") == 0)
	mode[i] =  FOREIGN_MODE_IN_OUT;

      j = 0;
      for (;;)
	if (strcasecmp(foreign_tbl[j], str_type) == 0)
	  break;
	else if (++j >= FOREIGN_TBL_SIZE)
	  {
	    fprintf(stderr, "invalid foreign type:%s\n", str_type);
	    exit(1);
	  }

      type[i] = j;
      if ((mode[i] == FOREIGN_MODE_IN || mode[i] == FOREIGN_MODE_IN_OUT) &&
	  (j == FOREIGN_TYPE_CHARS || j == FOREIGN_TYPE_CODES))
	nb_c_str++;
    }

  if (chc_size >= 0)
    {
      sprintf(l, FORMAT_LABEL(1));
      Inst_Printf("call_c", "Pl_Foreign_Create_Choice(&%s,%d,%d)", l, cur_arity, chc_size);
      Label_Printf("%s:", l);
      Inst_Printf("call_c", "Pl_Foreign_Update_Choice(&%s,%d,%d)", l, cur_arity, chc_size);
    }

  if (*bip_name || bip_arity != -2)
    Inst_Printf("call_c", "Pl_Set_C_Bip_Name(\"%s\",%d)", bip_name, bip_arity);

  for (i = 0; i < nb_elem; i++)
    {
      n = type[i];
      c = F_Array_Letter(n);

      s_dup = (mode[i] == FOREIGN_MODE_IN || mode[i] == FOREIGN_MODE_IN_OUT) &&
	(n == FOREIGN_TYPE_CHARS || n == FOREIGN_TYPE_CODES) && --nb_c_str != 0;

      switch (mode[i])
	{
	case FOREIGN_MODE_IN:
	  if (n != FOREIGN_TYPE_TERM)
	    {
	      Inst_Printf("call_c", "Pl_Rd_%s_Check(X(%d))", foreign_tbl[n], i);
	      Inst_Printf("move_ret", "F%c(%d)", c, i);

	      if (s_dup)
		{
		  Inst_Printf("call_c", "Pl_Strdup_Check(FL(%d),\"call generated by %s\",%d)",
			      i, __FILE__, __LINE__);
		  Inst_Printf("move_ret", "FL(%d)", i);
		}
	    }
	  break;

	case FOREIGN_MODE_OUT:
	  complex_jump_ret = 1;	/* arg to unif. complex jump_ret */
	  if (n != FOREIGN_TYPE_TERM)
	    Inst_Printf("call_c", "Pl_Check_For_Un_%s(X(%d))", foreign_tbl[n], i);
	  break;

	case FOREIGN_MODE_IN_OUT:
	  complex_jump_ret = 1;	/* arg to unif. complex jump_ret */
	  if (n != FOREIGN_TYPE_TERM)
	    Inst_Printf("call_c", "Pl_Foreign_Rd_IO_Arg(%d,X(%d),"
			"&Pl_Rd_%s_Check,%d)", (c == 'L') + s_dup,	/* 0,1 or 2 if strdup */
			i, foreign_tbl[n], fio_arg_index++);
	  else
	    Inst_Printf("call_c", "Pl_Foreign_Rd_IO_Arg(1,X(%d),0,%d)", i, fio_arg_index++);
	  Inst_Printf("move_ret", "FL(%d)", i);
	  break;
	}
    }

  Inst_Printf("call_c", NULL);
  fprintf(file_out, "%s(", fct_name);
  for (i = 0; i < nb_elem; i++)
    {
      n = type[i];
      c = (mode[i] == FOREIGN_MODE_IN_OUT) ? 'L' : F_Array_Letter(n);
      if (i > 0)
	fputc(',', file_out);
      if (mode[i] == FOREIGN_MODE_OUT)
	fprintf(file_out, "&");
      if (n == FOREIGN_TYPE_TERM && mode[i] == FOREIGN_MODE_IN)
	fprintf(file_out, "X(%d)", i);
      else
	fprintf(file_out, "F%c(%d)", c, i);
    }

  fprintf(file_out, ")\n");
  if (strcmp(ret_mode, "jump") == 0)
    {
      if (!complex_jump_ret)
	Inst_Printf("jump_ret", "");
      else
	Inst_Printf("move_ret", "FL(%d)", NB_OF_X_REGS - 1);
    }
  else
    {
      complex_jump_ret = 0;
      if (strcmp(ret_mode, "boolean") == 0)
	Inst_Printf("fail_ret", "");
    }


  for (i = 0; i < nb_elem; i++)
    {
      n = type[i];
      c = F_Array_Letter(type[i]);
      switch (mode[i])
	{
	case FOREIGN_MODE_OUT:
	  if (n != FOREIGN_TYPE_TERM)
	    Inst_Printf("call_c", "Pl_Un_%s(F%c(%d),X(%d))", foreign_tbl[n], c, i, i);
	  else
	    Inst_Printf("call_c", FAST "Pl_Unify(X(%d),FL(%d))", i, i);
	  Inst_Printf("fail_ret", "");
	  break;

	case FOREIGN_MODE_IN_OUT:
	    Inst_Printf("call_c", "Pl_Foreign_Un_IO_Arg(%d,&Pl_Un_%s,FL(%d),"
			"X(%d))", c == 'L', foreign_tbl[n], i, i);
	  Inst_Printf("fail_ret", "");
	  break;
	}
    }


  if (complex_jump_ret)
    {
      Inst_Printf("call_c", "Pl_Foreign_Jump_Ret(FL(%d))", NB_OF_X_REGS - 1);
      Inst_Printf("jump_ret", "");
    }
}




/*-------------------------------------------------------------------------*
 * EMIT_OBJ_INITIALIZER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_Obj_Initializer(void)
{
  SwtTbl *t;
  Pred *p;
  int i, j;
  char l[MAX_LABEL_LENGTH];
  char *q;

  Label_Printf("\n");

  if (map_atom.size)
    Label_Printf("long local at(%d)", map_atom.size);

  if (map_tagged_atom.size)
    Label_Printf("long local ta(%d)", map_tagged_atom.size);

  if (map_tagged_f_n.size)
    Label_Printf("long local fn(%d)", map_tagged_f_n.size);

  if (nb_swt_tbl)
    Label_Printf("long local st(%d)", nb_swt_tbl);

  Label_Printf("\n");

  Label_Printf("c_code  initializer Object_Initializer\n");
  Inst_Printf("call_c", "Pl_New_Object(&Prolog_Object_Initializer,&System_Directives,&User_Directives)");
  Inst_Printf("c_ret", "");
  Label_Printf("\n");

  Label_Printf("c_code  local Prolog_Object_Initializer\n");

#ifdef DEBUG
  Inst_Printf("call_c", "printf(\"executing init obj of %s\\n\")", file_name_in);
#endif

  map_foreach(&map_atom, entry)
    {
      Emit_One_Atom(entry);
    }
  map_foreach(&map_tagged_atom, entry)
    {
      Emit_One_Atom_Tagged(entry);
    }

  map_foreach(&map_tagged_f_n, entry)
    {
      Emit_One_F_N_Tagged(entry);
    }

  cur_pred_no = 0;
  for (p = dummy_pred_start.next; p; p = p->next)
    {
      fputc('\n', file_out);

      if (p->prop & MASK_PRED_NATIVE_CODE)
	q = p->hexa;
      else
	q = "0";

#if 0  /* uncomment this to support modules */
      Inst_Printf("call_c", FAST "Pl_Create_Pred(at(%d),at(%d),%d,at(%d),%d,%d,%s)",
		  p->module->value, p->functor->value, p->arity, p->pl_file->value, p->pl_line,
		  p->prop, q);
#else
      Inst_Printf("call_c", FAST "Pl_Create_Pred(at(%d),%d,at(%d),%d,%d,%s)",
		  p->functor->value, p->arity, p->pl_file->value, p->pl_line,
		  p->prop, q);
#endif

      cur_pred_no++;		/* for FORMAT_LABEL */

      for (i = 0; i < 3; i++)
	for (t = p->swt_tbl[i]; t != NULL; t = t->next)
	  {
	    Inst_Printf("call_c", FAST "Pl_Create_Swt_Table(%d)", t->nb_elem);
	    Inst_Printf("move_ret", "st(%d)", t->tbl_no);

	    switch (i)
	      {
	      case TBL_ATM:
		for (j = 0; j < t->nb_elem; j++)
		  {
		    sprintf(l, FORMAT_LABEL(t->elem[j].label));
		    Inst_Printf("call_c", FAST
				"Pl_Create_Swt_Atm_Element(st(%d),%d,at(%d),&%s)",
				t->tbl_no, t->nb_elem, (t->elem[j].atom)->value, l);
		  }
		break;

#ifndef SWT_INT_DICHOTOMY	/* basic switch_on_integer */
	      case TBL_INT:
		for (j = 0; j < t->nb_elem; j++)
		  {
		    sprintf(l, FORMAT_LABEL(t->elem[j].label));
		    Inst_Printf("call_c", FAST
				"Pl_Create_Swt_Int_Element(st(%d),%d,%" PL_FMT_d ",&%s)",
				t->tbl_no, t->nb_elem, t->elem[j].n, l);
		  }
		break;
#endif

	      default:
		for (j = 0; j < t->nb_elem; j++)
		  {
		    sprintf(l, FORMAT_LABEL(t->elem[j].label));
		    Inst_Printf("call_c", FAST
				"Pl_Create_Swt_Stc_Element(st(%d),%d,at(%d),%" PL_FMT_d ",&%s)",
				t->tbl_no, t->nb_elem,
				(t->elem[j].atom)->value, t->elem[j].n, l);
		  }
	      }
	  }
    }

  Inst_Printf("c_ret", "");
}




/*-------------------------------------------------------------------------*
 * EMIT_EXEC_DIRECTIVES                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_Exec_Directives(void)
{
  int i;
  Direct *p;

  fputc('\n', file_out);
  Label_Printf("c_code  local System_Directives\n");

  i = 0;
  for (p = dummy_direct_start.next; p; p = p->next)
    {
      i++;
      if (!p->system)
	continue;

#ifdef DEBUG
      { static int flag = 0;
	if (!flag)
	  Inst_Printf("call_c", "printf(\"executing syst directives of %s\\n\")", file_name_in);
	flag = 1;
      }
#endif
      Inst_Printf("call_c", "Pl_Execute_Directive(at(%d),%d,%d,&directive_%d)",
		  p->pl_file->value, p->pl_line, 1, i);
    }

  Inst_Printf("c_ret", "");

  fputc('\n', file_out);
  Label_Printf("c_code  local User_Directives\n");


  i = 0;
  for (p = dummy_direct_start.next; p; p = p->next)
    {
      i++;
      if (p->system)
	continue;

#ifdef DEBUG
      { static int flag = 0;
	if (!flag)
	  Inst_Printf("call_c", "printf(\"executing user directives of %s\\n\")", file_name_in);
	flag = 1;
      }
#endif
      Inst_Printf("call_c", "Pl_Execute_Directive(at(%d),%d,%d,&directive_%d)",
		  p->pl_file->value, p->pl_line, 0, i);
    }

  Inst_Printf("c_ret", "");
}




/*-------------------------------------------------------------------------*
 * EMIT_ONE_ATOM                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_One_Atom(struct map_entry *entry)
{
  Inst_Printf("call_c", "Pl_Create_Atom(\"%s\")", entry->key);
  Inst_Printf("move_ret", "at(%d)", entry->value);
}




/*-------------------------------------------------------------------------*
 * EMIT_ONE_ATOM_TAGGED                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_One_Atom_Tagged(struct map_entry *entry)
{
  struct map_entry *atom = map_get(&map_atom, entry->key);

  if (atom)			/* optim: reuse the atom to avoid re-hashing */
    Inst_Printf("call_c", FAST "Pl_Put_Atom(at(%d))", atom->value);
  else
    Inst_Printf("call_c", FAST "Pl_Create_Atom_Tagged(\"%s\")", entry->key);

  Inst_Printf("move_ret", "ta(%d)", entry->value);
}




/*-------------------------------------------------------------------------*
 * ADD_F_N_TAGGED                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Add_F_N_Tagged(char *atom, int n)
{
  int l = (int) strlen(atom);

  atom = (char *) realloc(atom, l + 5 + 1);
  sprintf(atom + l, "/%d", n);

  return map_put(&map_tagged_f_n, atom, NULL)->value;
}




/*-------------------------------------------------------------------------*
 * EMIT_ONE_F_N_TAGGED                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_One_F_N_Tagged(struct map_entry *entry)
{
  int n;
  char *str = entry->key;
  char *p;

  for(p = str + strlen(str) - 1; *p != '/'; p--)
    ;

  n = atoi(p+1);
  *p = '\0';

  Inst_Printf("call_c", FAST "Pl_Create_Functor_Arity_Tagged(\"%s\",%d)", str, n);
  Inst_Printf("move_ret", "fn(%d)", entry->value);
}




/*-------------------------------------------------------------------------*
 * LABEL_PRINTF                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Label_Printf(char *label, ...)
{
  va_list arg_ptr;

  va_start(arg_ptr, label);

  vfprintf(file_out, label, arg_ptr);

  va_end(arg_ptr);
  fputc('\n', file_out);
}




/*-------------------------------------------------------------------------*
 * INST_PRINTF                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Inst_Printf(char *op, char *operands, ...)
{
  va_list arg_ptr;

  va_start(arg_ptr, operands);

  fprintf(file_out, "\t%-10s ", op);
  if (operands)
    {
      vfprintf(file_out, operands, arg_ptr);
      fputc('\n', file_out);
    }

  va_end(arg_ptr);
}




/*-------------------------------------------------------------------------*
 * PARSE_ARGUMENTS                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Parse_Arguments(int argc, char *argv[])
{
  static char str[1024];
  int i;


  file_name_in = file_name_out = NULL;
  comment = FALSE;

  for (i = 1; i < argc; i++)
    {
      if (*argv[i] == '-' && argv[i][1] != '\0')
	{
	  if (Check_Arg(i, "-o") || Check_Arg(i, "--output"))
	    {
	      if (++i >= argc)
		{
		  fprintf(stderr, "FILE missing after %s option\n", argv[i - 1]);
		  exit(1);
		}

	      file_name_out = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--comment"))
	    {
	      comment = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--version"))
	    {
	      Display_Copying("WAM to Mini-Assembly Compiler");
	      exit(0);
	    }

	  if (Check_Arg(i, "-h") || Check_Arg(i, "--help"))
	    {
	      Display_Help();
	      exit(0);
	    }

	  fprintf(stderr, "unknown option %s - try wam2ma --help\n", argv[i]);
	  exit(1);
	}

      if (file_name_in != NULL)
	{
	  fprintf(stderr, "input file already specified (%s)\n", file_name_in);
	  exit(1);
	}
      file_name_in = argv[i];
    }


  if (file_name_in != NULL && strcmp(file_name_in, "-") == 0)
    file_name_in = NULL;

  if (file_name_out == NULL && file_name_in != NULL)
    {
      strcpy(str, file_name_in);
      i = (int) strlen(str);
      if (strcmp(str + i - 4, ".wam") == 0)
	strcpy(str + i - 4, DEFAULT_OUTPUT_SUFFIX);
      else
	strcpy(str + i, DEFAULT_OUTPUT_SUFFIX);
      file_name_out = str;
    }

  if (file_name_out != NULL && strcmp(file_name_out, "-") == 0)
    file_name_out = NULL;
}




/*-------------------------------------------------------------------------*
 * DISPLAY_HELP                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Display_Help(void)
#define L(msg)  fprintf(stderr, "%s\n", msg)
{
  L("Usage: wam2ma [OPTION...] FILE");
  L("");
  L("Options:");
  L("  -o FILE, --output FILE      set output file name");
  L("  --comment                   include comments in the output file");
  L("  -h, --help                  print this help and exit");
  L("  --version                   print version number and exit");
  L("");
  L("'-' can be given as FILE for the standard input/output");
  L("");
  L("Report bugs to bug-prolog@gnu.org.");
}

#undef L
