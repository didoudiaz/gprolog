/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : chkma.c                                                         *
 * Descr.: test file for MA translation                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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
#include "../EnginePl/pl_long.h"

#if 0
#define DEBUG
#endif


#ifndef FAST  /* see Makefile */
#ifdef FC
#undef FC
#endif
#define FC /* define FC to force arch_dep.h to not use FC */
#endif

PlULong pl_max_atom;		/* to not need atom.o */

#define IF_NO_FD_FILE
//#include "engine_pl.h"
#include "../EnginePl/engine.c"


#if !defined(FC_USED_TO_COMPILE_CORE) && defined(FAST) /* see Makefile */
#error FAST defined but cannot compile for FC
#endif


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define YY(k) Y(E,k)

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

/* Recall: PlLong, PlTerm and WamWord are synonyms (all are intptr_t) */

/* these 4 lines are get from foreign_supp.c */

PlLong pl_foreign_long[NB_OF_X_REGS];
double pl_foreign_double[NB_OF_X_REGS];
PlLong *pl_base_fl = pl_foreign_long;   /* overwrite var of engine.c */
double *pl_base_fd = pl_foreign_double; /* overwrite var of engine.c */


WamWord stack[1024 * 128];

Bool initialised = FALSE;
PlLong x;
PlLong ret;
PlLong swt[] = { 0, 4, 15, 4095, 123456, 2456789, -257, 3, 8, 328, -9, -999999 };
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

void Avoid_Warning_Double(double d) {}

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
#if 1
  test_initializer,
  test_declaration,
  test_pl_jump_and_pl_ret,
  test_pl_call_and_pl_ret_and_pl_fail,
//#else
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
 #endif
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
  E = B + 1024*20;		/* To allow hign Y-index testing */
  printf("pl_reg_bank=&X(0):%p   B:%p   E:%p  &Y(0):%p\n",
	 pl_reg_bank, B, E, &YY(0));

  printf("stack:%p\n", stack);


  //  { PlLong *disp_stack(); printf("rsp : %p\n", disp_stack()); }
  while (tbl[i++])
    {
      printf("test %d: ", i);
      (*tbl[i - 1]) ();
      printf("test %d  OK\n", i);
    }

  Restore_Machine_Regs(init_buff_regs);
  printf("MA checks succeeded\n");
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
  printf("\nValue x: %" PL_FMT_d " =  %#" PL_FMT_x "\n", x, x);
}



void FC
Allocate(int x)			/* only to update the register for E */
{
}



WamCont save_CP;
void Save_CP()
{
  //printf("in Save_CP\n");
  save_CP = CP;
}



void Restore_CP()
{
  CP = save_CP;
}




#define CHECK_RESULT(name_var, var, val, test, fmt, type)	\
  if (test) \
    { \
      printf("*** %s is " fmt " instead of " fmt "\n", name_var, var, (type) (val)); \
      error(); \
    }


#define CHECK_RESULT_LONG(var, val) CHECK_RESULT(#var, var, val, (var) != (val), "%" PL_FMT_d, PlLong)

#define CHECK_RESULT_DOUBLE(var, val) CHECK_RESULT(#var, var, val, (var) != (val), "%.8g", double)

#define CHECK_RESULT_ADDR(var, val) CHECK_RESULT(#var, var, val, (PlLong *) (var) != (PlLong *) (val), "%p", PlLong *)

#define CHECK_RESULT_STRING(var, val) CHECK_RESULT(#var, var, val, strcmp((var), (val)), "<%s>", char *)




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

  //  printf("returning with ok: %d  must_succeed: %d\n", ok, must_succeed);
  if (ok != must_succeed)
    error();
}


void
Initializer(void)
{
  printf("Inside initializer\n");
  initialised = TRUE;
}


#if 1
void
test_initializer(void)
{
#ifdef _MSC_VER
  printf("c_code initializer (ignored)...\n");
#else
  printf("c_code initializer...\n");
  if (!initialised)
    error();
#endif
}

void Several_Calls()
{
  Save_CP();
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

  CHECK_RESULT_LONG(MA_GLOBAL_VAR2, 12345);
  CHECK_RESULT_LONG(MA_LOCAL_VAR2, 0);
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


/* test_call_c before test_move_x_y because we need to call a 
 * C function in almost all tests, e.g. Allocate() 
 */

void
test_call_c(void)
{
  printf("call_c(void)...\n");
  x = 0;

  Call_Pl(ma_test_call_c, 1);

  CHECK_RESULT_LONG(x, 1);
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
  YY(0) = -1;
  YY(9) = -1;
  YY(15) = -1;
  
  Call_Pl(ma_test_move_x_y, 1);

  CHECK_RESULT_LONG(YY(3), 24680);
  CHECK_RESULT_LONG(YY(0), 13579);
  CHECK_RESULT_LONG(YY(15), 123456789);
  CHECK_RESULT_LONG(YY(15000), 123456789);

  CHECK_RESULT_LONG(X(0), 24680);
}




void
test_move_y_x(void)
{
  printf("move Y(i) to X(j)...\n");
  YY(0) = 24680;
  YY(10) = 13579;
  YY(23) = 123456789;
  YY(16000) = 2718281;
  X(0) = -1;
  X(12) = -1;
  X(31) = -1;

  Call_Pl(ma_test_move_y_x, 1);

  CHECK_RESULT_LONG(X(0), 24680);
  CHECK_RESULT_LONG(X(31), 13579);
  CHECK_RESULT_LONG(X(12), 123456789);
  CHECK_RESULT_LONG(X(254), 2718281);
}




void
test_arg_int(void)
{
  printf("call_c(int)...\n");
  x = 0;

  Call_Pl(ma_test_arg_int, 1);

  CHECK_RESULT_LONG(x, 1);
}


void FC
test_arg_int1(PlLong a, PlLong b, PlLong c, PlLong d)
{
  CHECK_RESULT_LONG(a, 12);
  CHECK_RESULT_LONG(b, -1);
  CHECK_RESULT_LONG(c, 4095);
  CHECK_RESULT_LONG(d, 123456789);

  x++;
}




void
test_arg_double(void)
{
  printf("call_c(double)...\n");
  x = 0;

  Call_Pl(ma_test_arg_double, 1);

  CHECK_RESULT_LONG(x, 1);
}


void FC
test_arg_double1(double a, double b, double c, double d, double e, double f)
{
  static double loc_d; loc_d = a + c + f; // check some double alignment

  CHECK_RESULT_DOUBLE(a, 12.456);
  CHECK_RESULT_DOUBLE(b, -1.3e-102);
  CHECK_RESULT_DOUBLE(c, -3.141593);

  CHECK_RESULT_DOUBLE(d, 12.456);
  CHECK_RESULT_DOUBLE(e, -1.3e-102);
  CHECK_RESULT_DOUBLE(f, -3.141593);

  x++;
  Avoid_Warning_Double(loc_d);
}




// JAT: rumour that fast call (default on x86_64) allows only 4 params in regs,
// no matter what type: new test required
void test_arg_mixed(void)
{
  printf("call_c(mixed)...\n");
  x = 0;

  Call_Pl(ma_test_arg_mixed, 1);

  CHECK_RESULT_LONG(x, 1);

}


void FC
test_arg_mixed1(PlLong ai, double a, double b, PlLong bi, PlLong ci, double c, PlLong di)
{
  CHECK_RESULT_LONG(ai, -19);
  CHECK_RESULT_DOUBLE(a, 12.456);
  CHECK_RESULT_DOUBLE(b, -1.3e-102);
  CHECK_RESULT_LONG(bi, 365);
  CHECK_RESULT_LONG(ci, 987654321);
  CHECK_RESULT_DOUBLE(c, -3.141593);
  CHECK_RESULT_LONG(di, -110101);

  x++;
}




void
test_arg_string(void)
{
  printf("call_c(string)...\n");
  x = 0;

  Call_Pl(ma_test_arg_string, 1);

  CHECK_RESULT_LONG(x, 1);
}


void FC
test_arg_string1(char *a, char *b)
{
  CHECK_RESULT_STRING(a, "a string");
  CHECK_RESULT_STRING(b, "abcd\01489d\37711ef\n\r");

  x++;
}




void
test_arg_mem_l(void)
{
  printf("call_c(mem,&label,mem(...),&mem(...))...\n");
  x = 0;

  Call_Pl(ma_test_arg_mem_l, 1);

  CHECK_RESULT_LONG(x, 1);
}


void FC
test_arg_mem_l1(PlLong a, PlLong b, PlLong *c, PlLong d, PlLong e, PlLong *f)
{
  CHECK_RESULT_LONG(a, 128);
  CHECK_RESULT_LONG(b, 12345);
  CHECK_RESULT_ADDR(c, test_arg_mem_l);
  CHECK_RESULT_LONG(d, MA_ARRAY[0]);
  CHECK_RESULT_LONG(e, MA_ARRAY[4097]);
  CHECK_RESULT_ADDR(f, &MA_ARRAY[4500]);

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

  CHECK_RESULT_LONG(x, 1);
}


void FC
test_arg_x1(PlLong a, PlLong *b, PlLong c, PlLong *d)
{
  CHECK_RESULT_LONG(a, 123987);
  CHECK_RESULT_ADDR(b, &X(0));
  CHECK_RESULT_LONG(c, 987654321);
  CHECK_RESULT_ADDR(d, &X(128));

  x++;
}




void
test_arg_y(void)
{
  printf("call_c(Y())...\n");
  x = 0;
  YY(0) = 1928374;
  YY(12) = 456789;
  YY(17000) = 123456;

  Call_Pl(ma_test_arg_y, 1);

  CHECK_RESULT_LONG(x, 1);
}


void FC
test_arg_y1(PlLong a, PlLong *b, PlLong c, PlLong *d, PlLong e)
{
  CHECK_RESULT_LONG(a, 1928374);
  CHECK_RESULT_ADDR(b, &YY(0));
  CHECK_RESULT_LONG(c, 456789);
  CHECK_RESULT_ADDR(d, &YY(6));
  CHECK_RESULT_LONG(e, 123456);

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

  CHECK_RESULT_LONG(x, 1);
}


void FC
test_arg_fl_array1(PlLong a, PlLong b, PlLong *c, PlLong *d)
{
  CHECK_RESULT_LONG(a, 12);
  CHECK_RESULT_LONG(b, 14);
  CHECK_RESULT_ADDR(c, pl_foreign_long);
  CHECK_RESULT_ADDR(d, pl_foreign_long + 56);

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

  CHECK_RESULT_LONG(x, 1);
}


void FC
test_arg_fd_array1(double a, double b, double *c, double *d)
{
  CHECK_RESULT_DOUBLE(a, 1.2e30);
  CHECK_RESULT_DOUBLE(b, -1.234567);
  CHECK_RESULT_ADDR(c, pl_foreign_double);
  CHECK_RESULT_ADDR(d, pl_foreign_double + 127);

  x++;
}




void
test_call_c_lot_args(void)
{
  printf("call_c(lot_of_args)...\n");
  x = 0;

  X(0) = 123987;
  X(255) = 987654321;

  YY(0) = 1928374;
  YY(12) = 456789;

  Call_Pl(ma_test_call_c_lot_args, 1);

  CHECK_RESULT_LONG(x, 1);
}


void FC
test_call_c_lot_args1(double n0, PlLong n1, double n2, PlLong n3, double n4, PlLong n5,
		      void (*a) (), PlLong b, PlLong c, PlLong d, double e, char *f,
		      PlLong g, PlLong *h, PlLong i, PlLong *j,
		      PlLong k, PlLong *l, PlLong m, PlLong *n,
		      double d1, double d2, double d3, double d4, double d5, double d6)
{
  CHECK_RESULT_DOUBLE(n0, 0.1);
  CHECK_RESULT_LONG(n1, 0);
  CHECK_RESULT_DOUBLE(n2, 0.2);
  CHECK_RESULT_LONG(n3, 0);
  CHECK_RESULT_DOUBLE(n4, 0.3);
  CHECK_RESULT_LONG(n5, 0);
  CHECK_RESULT_ADDR(a, test_call_c_lot_args);
  CHECK_RESULT_LONG(b, 128);
  CHECK_RESULT_LONG(c, 4095);
  CHECK_RESULT_LONG(d, 123456789);
  CHECK_RESULT_DOUBLE(e, -3.141593);
  CHECK_RESULT_STRING(f, "abcd\01489def\n\r");
  CHECK_RESULT_LONG(g, 123987);
  CHECK_RESULT_ADDR(h, &X(0));
  CHECK_RESULT_LONG(i, 987654321);
  CHECK_RESULT_ADDR(j, &X(128));
  CHECK_RESULT_LONG(k, 1928374);
  CHECK_RESULT_ADDR(l, &YY(0));
  CHECK_RESULT_LONG(m, 456789);
  CHECK_RESULT_ADDR(n, &YY(6));
  CHECK_RESULT_DOUBLE(d1, 1.1);
  CHECK_RESULT_DOUBLE(d2, 2.2);
  CHECK_RESULT_DOUBLE(d3, 3.3);
  CHECK_RESULT_DOUBLE(d4, 4.4);
  CHECK_RESULT_DOUBLE(d5, 5.5);
  CHECK_RESULT_DOUBLE(d6, 6.6);

  x++;
}




void
test_jump_ret(void)
{
  printf("call_c()+jump_ret...\n");
  x = 0;

  Call_Pl(ma_test_jump_ret, 1);

  CHECK_RESULT_LONG(x, 2);
}


PlLong FC
test_jump_ret1(PlLong addr)
{
#ifdef DEBUG
  void ma_test_jump_ret1();

  printf("addr: %p ma_test_jump_ret1: %p\n", (PlLong *) addr, ma_test_jump_ret1);
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

  CHECK_RESULT_LONG(x, 1);

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

  CHECK_RESULT_LONG(x, 3);

  CHECK_RESULT_LONG(MA_GLOBAL_VAR1, 123456789);
  CHECK_RESULT_LONG(MA_ARRAY[64], 123456789);
  CHECK_RESULT_LONG(MA_ARRAY[4097], 123456789);
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

  CHECK_RESULT_LONG(x, 2);
  CHECK_RESULT_LONG(X(0), 1234987);
  CHECK_RESULT_LONG(X(255), 45678);

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
  YY(0) = -1;
  YY(11) = -1;

  Call_Pl(ma_test_move_ret_y, 1);

  CHECK_RESULT_LONG(x, 2);
  CHECK_RESULT_LONG(YY(0), 1234987);
  CHECK_RESULT_LONG(YY(18000), 45678);
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

  CHECK_RESULT_LONG(x, 2);
  CHECK_RESULT_LONG(pl_foreign_long[0], 1234987);
  CHECK_RESULT_LONG(pl_foreign_long[11], 45678);
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

  CHECK_RESULT_LONG(x, 2);
  CHECK_RESULT_DOUBLE(pl_foreign_double[0], 1.234987);
  CHECK_RESULT_DOUBLE(pl_foreign_double[11], -3.141593);
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
  for (i = 0; swt[i] != -999999; i++)
    Call_Pl(ma_test_switch_ret, 1);

  Call_Pl(ma_test_switch_ret, 0);	/* here swt[i] = -999999 switch should fail */
}


PlLong FC
test_switch_ret1(void)
{
  return swt[i];
}


void FC
test_switch_ret2(PlLong k)
{
  CHECK_RESULT_LONG(k, i);
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
