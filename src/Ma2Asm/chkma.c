/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : chkma.c                                                         *
 * Descr.: test file for MA translation                                    *
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


#include <stdio.h>
#include <string.h>

#ifndef FAST  /* see Makefile */
#define FC /* define FC to force arch_dep.h to no use FC */
#endif

int pl_max_atom;		/* to not need atom.o */

#define IF_NO_FD_FILE
//#include "engine_pl.h"
#include "../EnginePl/engine.c"


#if !defined(FC_USED_TO_COMPILE_CORE) && defined(FAST) /* see Makefile */
#error FAST defined but cannot compile for FC
#endif


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#if 1

#define MA_ARRAY     ma_array
#define MA_GLOBAL_VAR1 ma_global_var1
#define MA_GLOBAL_VAR2 ma_global_var2

#else

#define MA_ARRAY     _ma_array
#define MA_GLOBAL_VAR1 _ma_global_var1
#define MA_GLOBAL_VAR2 _ma_global_var2
#define MA_LOCAL_VAR2 _ma_local_var2

#endif


/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/* these 4 lines are get from foreign_supp.c */

PlLong pl_foreign_long[NB_OF_X_REGS];
double pl_foreign_double[NB_OF_X_REGS];
PlLong *base_fl = pl_foreign_long;	  /* overwrite var of engine.c */
double *base_fd = pl_foreign_double; /* overwrite var of engine.c */


WamWord stack[4096];

int initialised = 0;
PlLong x;
PlLong ret;
PlLong swt[] = { 0, 4, 15, 4095, 123456, 2456789, -1 };
PlLong i;

PlLong MA_ARRAY[5000];
PlLong MA_GLOBAL_VAR1;
PlLong MA_GLOBAL_VAR2;
PlLong MA_LOCAL_VAR2;		/* should not be the same as in check_ma.ma */


#if !defined(NO_USE_REGS) && NB_OF_USED_MACHINE_REGS > 0
static WamWord init_buff_regs[NB_OF_USED_MACHINE_REGS];
#endif

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void test_initializer(void);

void test_declaration(void);

void test_pl_jump_and_pl_ret(void);
void ma_test_pl_jump_and_pl_ret(void);

void test_pl_call_and_pl_ret_and_pl_fail(void);
void ma_test_pl_call_and_pl_ret_and_pl_fail(void);

void test_prep_cp_and_here_cp(void);
void ma_test_prep_cp_and_here_cp(void);

void test_jump_and_c_ret(void);
void ma_test_jump_and_c_ret(void);

void test_call_c(void);
void ma_test_call_c(void);

void test_move_x_y(void);
void ma_test_move_x_y(void);

void test_move_y_x(void);
void ma_test_move_y_x(void);

void test_arg_int(void);
void ma_test_arg_int(void);

void test_arg_double(void);
void ma_test_arg_double(void);

void test_arg_mixed(void);
void ma_test_arg_mixed(void);

void test_arg_string(void);
void ma_test_arg_string(void);

void test_arg_mem_l(void);
void ma_test_arg_mem_l(void);

void test_arg_x(void);
void ma_test_arg_x(void);

void test_arg_y(void);
void ma_test_arg_y(void);

void test_arg_fl_array(void);
void ma_test_arg_fl_array(void);

void test_arg_fd_array(void);
void ma_test_arg_fd_array(void);

void test_call_c_lot_args(void);
void ma_test_call_c_lot_args(void);

void test_jump_ret(void);
void ma_test_jump_ret(void);

void test_fail_ret(void);
void ma_test_fail_ret(void);

void test_move_ret_mem(void);
void ma_test_move_ret_mem(void);

void test_move_ret_x(void);
void ma_test_move_ret_x(void);

void test_move_ret_y(void);
void ma_test_move_ret_y(void);

void test_move_ret_fl(void);
void ma_test_move_ret_fl(void);

void test_move_ret_fd(void);
void ma_test_move_ret_fd(void);

void test_switch_ret(void);
void ma_test_switch_ret(void);


void (*tbl[]) () =
{
  test_initializer,
  test_declaration,
  test_pl_jump_and_pl_ret,
  test_pl_call_and_pl_ret_and_pl_fail,
  test_prep_cp_and_here_cp,
  test_jump_and_c_ret,
  test_call_c,
  test_move_x_y,
  test_move_y_x,
  test_arg_int,
  test_arg_double,
  test_arg_mixed,
  test_arg_string,
  test_arg_mem_l,
  test_arg_x,
  test_arg_y,
  test_arg_fl_array,
  test_arg_fd_array,
  test_call_c_lot_args,
  test_jump_ret,
  test_fail_ret,
  test_move_ret_mem,
  test_move_ret_x,
  test_move_ret_y,
  test_move_ret_fl,
  test_move_ret_fd,
  test_switch_ret,
  NULL
};

#define PRINTRET void *adr = _AddressOfReturnAddress(); printf("adr return: %p\n", adr)


/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  int i = 0;

#if defined(FC_USED_TO_COMPILE_CORE)
#ifdef FAST
  printf("check running with FC (fast call)\n");
#else
  printf("check running without FC (NO fast call)\n");
#endif

#elif !defined(FC_USED_TO_COMPILE_CORE)

#ifdef FAST
#error FAST defined but cannot compile for FC
#endif

#else

#warning WITH FC

#endif



#ifdef _WIN32
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
#endif

#if 1
  pl_foreign_double[0] = 1.2e30;
  pl_foreign_double[47] = -1.234567;
#endif

  Save_Machine_Regs(init_buff_regs);

#ifndef NO_MACHINE_REG_FOR_REG_BANK
  pl_reg_bank = stack;
  B = stack + NB_OF_X_REGS + 100;
#else
  B = stack;
#endif
  E = B + 1024;
  printf("pl_reg_bank=&X(0):%#" PL_FMT_x "   B:%#" PL_FMT_x "   E:%#" PL_FMT_x "  &Y(0):%#" PL_FMT_x "\n",
	 (PlULong) pl_reg_bank, (PlULong) B, (PlULong) E, (PlULong) &Y(E, 0));

  printf("stack:%#" PL_FMT_x "\n", (PlULong) stack);


  //  { PlLong *disp_stack(); printf("rsp : %p\n", disp_stack()); }
  while (tbl[i++])
    {
      printf("test %d: ", i);
      (*tbl[i - 1]) ();
      printf("test %d  OK\n", i);
    }

  Restore_Machine_Regs(init_buff_regs);
  printf("MA checks suceeded\n");
  return 0;
}



void
Init_CP(WamCont p)
{
  CP = Adjust_CP(p);
}

				/* can be called by MA code to print a PlLong */
void Write_Long(PlLong x)
{
  printf("\nValue x: %#" PL_FMT_x "\n", x);
}



void FC
Allocate(int x)			/* only to update the register for E */
{
}



WamCont save_CP;
void Save_CP()
{
  save_CP = CP;
}



void Restore_CP()
{
  CP = save_CP;
}




void
error(void)
{
  Restore_Machine_Regs(init_buff_regs);
  printf("*** ERROR ***\n");
  fflush(NULL);
  exit(1);
}




void
Call_Pl(void (*code) (), int must_succeed)
{
  int ok = Pl_Call_Prolog(code);

  if (ok != must_succeed)
    error();
}


void
Initializer(void)
{
  initialised = 1;
}


void
test_initializer(void)
{
#ifdef _MSC_VER
  printf("c_code intializer (ignored)...\n");
#else
  printf("c_code intializer...\n");
  if (!initialised)
    error();
#endif
}

void
test_declaration(void)
{
  PlLong *adr = MA_ARRAY + 5000;
  int i;

  printf("long local/global ...\n");

  if (&MA_GLOBAL_VAR1 >= MA_ARRAY && &MA_GLOBAL_VAR1 < adr)
    error();

  for (i = 0; i < 5000; i++)
    MA_ARRAY[i] = i;

  if (MA_GLOBAL_VAR2 != 12345)
    error();

  if (MA_LOCAL_VAR2 != 0)
    error();
}




void
test_pl_jump_and_pl_ret(void)
{
  printf("pl_jump/pl_ret...\n");
  Call_Pl(ma_test_pl_jump_and_pl_ret, 1);
}




void
test_pl_call_and_pl_ret_and_pl_fail(void)
{
  printf("pl_call/pl_ret/pl_fail...\n");
  Call_Pl(ma_test_pl_call_and_pl_ret_and_pl_fail, 0);
}



#if 1
void
test_prep_cp_and_here_cp(void)
{
  printf("prep_cp/here_cp...\n");
  Call_Pl(ma_test_prep_cp_and_here_cp, 1);
}




void
test_jump_and_c_ret(void)
{
  printf("jump/c_ret...\n");
  ALTB(B) = (WamCont) error;
  Init_CP(error);
  ma_test_jump_and_c_ret();
}




void
test_call_c(void)
{
  printf("call_c(void)...\n");
  x = 0;
  Call_Pl(ma_test_call_c, 1);
  if (x != 1)
    error();
}


void FC
test_call_c1(void)
{
  x++;
}




void
test_move_x_y(void)
{
  printf("move X(i) to Y(j)...\n");
  X(0) = 24680;
  X(10) = 13579;
  X(255) = 123456789;
  Y(E, 0) = -1;
  Y(E, 9) = -1;
  Y(E, 15) = -1;
  Call_Pl(ma_test_move_x_y, 1);
  if (Y(E, 3) != 24680 || Y(E, 0) != 13579 || Y(E, 15) != 123456789)
    error();
}




void
test_move_y_x(void)
{
  printf("move Y(i) to X(j)...\n");
  Y(E, 0) = 24680;
  Y(E, 10) = 13579;
  Y(E, 23) = 123456789;
  X(0) = -1;
  X(12) = -1;
  X(31) = -1;
  Call_Pl(ma_test_move_y_x, 1);
  if (X(0) != 24680 || X(31) != 13579 || X(12) != 123456789)
    error();
}




void
test_arg_int(void)
{
  printf("call_c(int)...\n");
  x = 0;
  Call_Pl(ma_test_arg_int, 1);
  if (x != 1)
    error();
}


void FC
test_arg_int1(int a, int b, int c, int d)
{
  if (a != 12 || b != -1 || c != 4095 || d != 123456789)
    error();
  x++;
}




void
test_arg_double(void)
{
  printf("call_c(double)...\n");
  x = 0;
  Call_Pl(ma_test_arg_double, 1);
  if (x != 1)
    error();
}


void FC
test_arg_double1(double a, double b, double c, double d, double e, double f)
{
  static double loc_d; loc_d = a + c + f; // check some double alignment
  if (a != 12.456 || b != -1.3e-102 || c != -3.141593 ||
      d != 12.456 || e != -1.3e-102 || f != -3.141593)
    error();
  x++;
}




// JAT: rumour that fast call (default on x86_64) allows only 4 params in regs,
// no matter what type: new test required
void test_arg_mixed(void)
{
  printf("call_c(mixed)...\n");
  x = 0;
  Call_Pl(ma_test_arg_mixed, 1);
  if (x != 1)
    error();
}


void FC
test_arg_mixed1(int ai, double a, double b, int bi, int ci, double c, int di)
{
#ifdef DEBUG
  printf("Results: ai %d, a %g, b %g, bi %d, c %g, ci %d, di %d\n", ai, a, b, bi, c, ci, di);
#endif
  if (a != 12.456 || b != -1.3e-102 || c != -3.141593 ||
      ai != -19 || bi != 365 || ci != 987654321 || di != -110101)
    error();
  x++;
}




void
test_arg_string(void)
{
  printf("call_c(string)...\n");
  x = 0;
  Call_Pl(ma_test_arg_string, 1);
  if (x != 1)
    error();
}


void FC
test_arg_string1(char *a, char *b)
{
#ifdef DEBUG
  printf("b:<%s>\n", a);
  printf("a:<%s>\n", b);
#endif
  if (strcmp(a, "a string") || strcmp(b, "abcd\01489d\37711ef\n\r"))
    error();
  x++;
}




void
test_arg_mem_l(void)
{
  printf("call_c(mem,&label,mem(...),&mem(...))...\n");
  x = 0;

  Call_Pl(ma_test_arg_mem_l, 1);
  if (x != 1)
    error();
}


void FC
test_arg_mem_l1(PlLong a, PlLong b, PlLong *c, PlLong d, PlLong e, PlLong *f)
{
  // JAT: needed more detail here
#ifdef DEBUG
  printf("Results: a %" PL_FMT_d ", b %" PL_FMT_d ", c %p (test_arg_m_l %p), d %" PL_FMT_d " (MA_ARRAY[0] %" PL_FMT_d "), e %" PL_FMT_d " (MA_ARRAY[4097] %" PL_FMT_d "), f %p (&MA_ARRAY[4500] %p)\n",
         a,b,c,test_arg_mem_l,d,MA_ARRAY[0],e,MA_ARRAY[4097],f,&MA_ARRAY[4500]);
#endif
  if (a != 128 || b != 12345 || c != (PlLong *) test_arg_mem_l
      || d != MA_ARRAY[0] || e != MA_ARRAY[4097] || f != &MA_ARRAY[4500])
    error();
  x++;
}




void
test_arg_x(void)
{
  printf("call_c(X())...\n");
  x = 0;
  X(0) = 123987;
  X(255) = 987654321;
  Call_Pl(ma_test_arg_x, 1);
  if (x != 1)
    error();
}


void FC
test_arg_x1(WamWord a, WamWord *b, WamWord c, WamWord *d)
{
  if (a != 123987 || b != &X(0) || c != 987654321 || d != &X(128))
    error();
  x++;
}




void
test_arg_y(void)
{
  printf("call_c(Y())...\n");
  x = 0;
  Y(E, 0) = 1928374;
  Y(E, 12) = 456789;
  Call_Pl(ma_test_arg_y, 1);
  if (x != 1)
    error();
}


void FC
test_arg_y1(WamWord a, WamWord *b, WamWord c, WamWord *d)
{
  if (a != 1928374 || b != &Y(E, 0) || c != 456789 || d != &Y(E, 6))
    error();
  x++;
}




void
test_arg_fl_array(void)
{
  printf("call_c(FL())...\n");
  x = 0;
  pl_foreign_long[0] = 12;
  pl_foreign_long[10] = 14;

  Call_Pl(ma_test_arg_fl_array, 1);
  if (x != 1)
    error();
}


void FC
test_arg_fl_array1(PlLong a, PlLong b, PlLong *c, PlLong *d)
{
#ifdef DEBUG
  printf("a=%d b=%d c=%x e=%x (fl=%x fl+56=%x)\n",
	 a, b, c, d, pl_foreign_long, pl_foreign_long + 56);
#endif
  if (a != 12 || b != 14 || c != pl_foreign_long || d != pl_foreign_long + 56)
    error();
  x++;
}




void
test_arg_fd_array(void)
{
  printf("call_c(FD())...\n");
  x = 0;
  pl_foreign_double[0] = 1.2e30;
  pl_foreign_double[47] = -1.234567;


  Call_Pl(ma_test_arg_fd_array, 1);
  if (x != 1)
    error();
}


void FC
test_arg_fd_array1(double a, double b, double *c, double *d)
{
  if (a != 1.2e30 || b != -1.234567 || c != pl_foreign_double
      || d != pl_foreign_double + 127)
    error();
  x++;
}




void
test_call_c_lot_args(void)
{
  printf("call_c(lot_of_args)...\n");
  x = 0;

  X(0) = 123987;
  X(255) = 987654321;

  Y(E, 0) = 1928374;
  Y(E, 12) = 456789;

  //#ifndef M_powerpc_linux
  Call_Pl(ma_test_call_c_lot_args, 1);
  if (x != 1)
    error();
  //#endif
}


void FC
test_call_c_lot_args1(WamWord n0, WamWord n1, WamWord n2, WamWord n3,
		      WamWord n4, WamWord n5,
		      void (*a) (), PlLong b, int c, int d, double e, char *f,
		      WamWord g, WamWord *h, WamWord i, WamWord *j,
		      WamWord k, WamWord *l, WamWord m, WamWord *n, double o)
{
  if (n0 != 0 || n1 != 0 || n2 != 0 || n3 != 0 || n4 != 0 || n5 != 0 ||
      a != test_call_c_lot_args || b != 128 || c != 4095 || d != 123456789
      || e != -3.141593 || strcmp(f, "abcd\01489def\n\r") || g != 123987
      || h != &X(0) || i != 987654321 || j != &X(128) || k != 1928374
      || l != &Y(E, 0) || m != 456789 || n != &Y(E, 6) || o != 1.23456)
    error();
  x++;
}




void
test_jump_ret(void)
{
  printf("call_c()+jump_ret...\n");
  x = 0;
  Call_Pl(ma_test_jump_ret, 1);
  if (x != 2)
    error();
}


PlLong FC
test_jump_ret1(PlLong addr)
{
#ifdef DEBUG
  extern void ma_test_jump_ret1();

  printf("%x %x\n", addr, ma_test_jump_ret1);
#endif
  x++;
  return addr;
}




void FC
test_jump_ret2(void)
{
#ifdef DEBUG
  printf("in test jump_ret2\n");
#endif
  x++;
}




void
test_fail_ret(void)
{
  printf("call_c()+fail_ret...\n");
  x = 0;
  ret = 1;
  Call_Pl(ma_test_fail_ret, 1);
  if (x != 1)
    error();

  ret = 0;
  Call_Pl(ma_test_fail_ret, 0);
}


int FC
test_fail_ret1(void)
{
  x++;
  return ret;
}




void
test_move_ret_mem(void)
{
  printf("call_c()+move_ret mem...\n");
  x = 0;

  Call_Pl(ma_test_move_ret_mem, 1);
  if (x != 3)
    error();
  if (MA_GLOBAL_VAR1 != 123456789 || MA_ARRAY[64] != 123456789 ||
      MA_ARRAY[4097] != 123456789)
    error();
}


PlLong FC
test_move_ret_mem1(void)
{
  x++;
  return 123456789;
}




void
test_move_ret_x(void)
{
  printf("call_c()+move_ret X()...\n");
  x = 0;
  X(0) = -1;
  X(255) = -1;
  Call_Pl(ma_test_move_ret_x, 1);
  if (x != 2)
    error();
  if (X(0) != 1234987 || X(255) != 45678)
    error();
}


PlLong FC
test_move_ret_x1(void)
{
  x++;
  return (x == 1) ? 1234987 : 45678;
}




void
test_move_ret_y(void)
{
  printf("call_c()+move_ret Y()...\n");
  x = 0;
  Y(E, 0) = -1;
  Y(E, 11) = -1;
  Call_Pl(ma_test_move_ret_y, 1);
  if (x != 2)
    error();
  if (Y(E, 0) != 1234987 || Y(E, 11) != 45678)
    error();
}


PlLong FC
test_move_ret_y1(void)
{
  x++;
  return (x == 1) ? 1234987 : 45678;
}




void
test_move_ret_fl(void)
{
  printf("call_c()+move_ret FL()...\n");
  x = 0;
  pl_foreign_long[0] = -1;
  pl_foreign_long[11] = -1;
  Call_Pl(ma_test_move_ret_fl, 1);
  if (x != 2)
    error();
  if (pl_foreign_long[0] != 1234987 || pl_foreign_long[11] != 45678)
    error();
}


PlLong FC
test_move_ret_fl1(void)
{
  x++;
  return (x == 1) ? 1234987 : 45678;
}




void
test_move_ret_fd(void)
{
  printf("call_c()+move_ret FD()...\n");
  x = 0;
  pl_foreign_double[0] = -1.0;
  pl_foreign_double[11] = -1;
  Call_Pl(ma_test_move_ret_fd, 1);
  if (x != 2)
    error();
  if (pl_foreign_double[0] != 1.234987 || pl_foreign_double[11] != -3.141593)
    error();
}


double FC
test_move_ret_fd1(void)
{
  x++;
  return (x == 1) ? 1.234987 : -3.141593;
}




void
test_switch_ret(void)
{
  printf("call_c()+switch_ret...\n");

  ALTB(B) = (WamCont) error;
  for (i = 0; swt[i] >= 0; i++)
    Call_Pl(ma_test_switch_ret, 1);

  Call_Pl(ma_test_switch_ret, 0);	/* here swt[i]= -1 switch should fail */
}


PlLong FC
test_switch_ret1(void)
{
  return swt[i];
}


void FC
test_switch_ret2(int k)
{
  if (k != i)
    error();
}

#endif



/*--- dummy functions needed by engine.c ---*/

void
Pl_Init_Atom(void)
{
}
void
Pl_Init_Oper(void)
{
}
void
Pl_Init_Pred(void)
{
}
void
Pl_Init_Machine(void)
{
}
void
Pl_Find_Linked_Objects(void)
{
}
void
Pl_Fd_Init_Engine(void)
{
}
int
Pl_Create_Atom(char *name)
{
  return 1;
}

PredInf * FC
Pl_Lookup_Pred(int func, int arity)
{
  return NULL;
}

void
Pl_Allocate_Stacks(void)
{
}
AtomInf *pl_atom_tbl;
void FC
Pl_Create_Choice_Point(CodePtr codep_alt, int arity)
{
}

void
Pl_Fd_Init_Solver(void)
{
}
void
Pl_Fd_Reset_Solver(void)
{
}

void
SIGSEGV_Handler(void)
{
}
int
Is_Win32_SEGV(void *exp)
{
  return 0;
}

#ifdef USE_SEH /* (defined(_WIN32) || defined(__CYGWIN__)) && !defined(M_x86_64)*/

EXCEPT_DISPOSITION
Win32_SEH_Handler(EXCEPTION_RECORD *excp_rec, void *establisher_frame,
                  CONTEXT *context_rec, void *dispatcher_cxt)
{
  return 0;
}
#endif


void
Pl_Fatal_Error(char *format, ...)
{
}

void *
Pl_Dummy_Ptr(void *p) 
{ 
  return p;
}
