#include <stdio.h>
#include <stdlib.h>

#define OBJ_INIT initializer_fct

#include "../../EnginePl/gp_config.h"
#include "../../EnginePl/pl_params.h"
#if 1 /* #if 0, cp ../../EnginePl/wam_archi.h . and customize it if needed */
#include "../../EnginePl/wam_archi.h"
#else
#include "wam_archi.h"
#endif
#include "../../EnginePl/machine.h"
#include "../../EnginePl/obj_chain.h"
#include "../../EnginePl/wam_inst.h"


#include "mach.h"

#define YY(k) Y(E,k)
extern unsigned var;
extern void *label;
intptr_t v1[100];
static intptr_t v2[100];
extern intptr_t v3[100];

void foo();
void foo1();
void pl_call1();

int x, y;

extern intptr_t pl_foreign_long[];
extern double pl_foreign_double[];





/* to define Asm_Start() */



/* to define Code_Start() (global/not global) and Code_Stop() */

void
TRANS_code_start_global()
{
}



static void
TRANS_code_start_non_global()
{
}




/* to define Pl_Jump() */

void
TRANS_pl_jump()
{
  M_Direct_Goto(foo1);
_foo1:;
}



/* to define Pl_Call() */

void
TRANS_pl_call()
{
#ifdef __GNUC__
  CP = (CodePtr) &&cont;
  M_Direct_Goto(foo);
 cont:
#else
  CP = (CodePtr) pl_call1;
  M_Direct_Goto(foo);
_foo:;
#endif
  dummy(CP);
}


void TRANS_pl_call_another()
{
  CP = (CodePtr) &&cont;
  foo();
 cont:
  CP = (CodePtr) &var;
  dummy(CP);
 cont2:
  CP = (CodePtr) &&cont2;
  dummy(var);
}

/* to define Pl_Fail() */

void
TRANS_pl_fail()
{
  M_Indirect_Goto(ALTB(B));
}





/* to define Pl_Ret() */

void
TRANS_pl_ret()
{
  M_Indirect_Goto(CP);
}


/* to define Prep_CP() and Here_CP() */
void
TRANS_prep_cp_here_cp() {
  CP = &&a;
  if (x<3) {
    bar(x);
  }
 a:;
}


/* to define Jump() */

void
TRANS_jump()
{
  if (x < 3)
    {
      bar(x);
      goto a;
    }
  x++;
a:;
}


/* to define Move_From/To_Reg_X/Y() */

void
TRANS_move_x_to_x()
{
  X(3) = X(5);
}

void
TRANS_move_x_to_y()
{
  YY(10) = X(2);
}

void
TRANS_move_y_to_x()
{
  X(0) = YY(3);
}

void
TRANS_move_y_to_y()
{
  YY(2) = YY(4);
}



/* to define Call_C_Start() + Call_C_Arg()... + Call_C_Stop() */

void
TRANS_call_c(void)
{
  dummy();
/*  &label,var,int, double,         string */
  bar(foo, var, 12, 4098, -4095, (double) 1.20e-10, "this is a string",
      "a\14b");

  /* v(index) */
  bar1(v1[2], v1[0], &v1[12], v2[2], v2[0], &v2[4]);
  bar1(v3[4], &v3[2], v3[0]);

  /* regs / &regs */
  bar2(X(0), &X(4), YY(0), &YY(12));
}



void
TRANS_call_c_lot_of_args(void)
{
/*  &label,var,int, double,         string */
  bar(3, 4, 5, 6, 7, 8, 9, 10, foo, var, 12, 4098, -4095, (double) 1.20e-10,
      "this is a string", "a\14b");

  /* regs / &regs */
  bar1(0, 0, 0, 0, 0, 0, X(2), &X(4), YY(0), &YY(12));
}



void
TRANS_call_c_foreign(void)
{
  bar(pl_foreign_long[0], pl_foreign_long[4], &pl_foreign_long[0], &pl_foreign_long[8]);
  bar(pl_foreign_double[0], pl_foreign_double[4], &pl_foreign_double[0],
      &pl_foreign_double[8]);
}

/* to define Jump_Ret() */

void
TRANS_jump_ret()
{
#if defined(M_ix86_win32)
  register intptr_t adr = (intptr_t) bar(12, "toto");

  _asm
  {
  jmp adr}
#else
  goto *bar(12, "toto");
#endif
}





/* to define Fail_Ret() */

void
TRANS_fail_ret()
{
  if (test(1, 2, 3) == 0)
    goto a;

  x++;
a:;
}



/* to define Move_Ret_To_Mem() */

void
TRANS_move_ret_to_mem()
{
  var = bar(3);
  v1[4096] = bar(15);
}



/* to define Move_Ret_To_Reg_X() */

void
TRANS_move_ret_to_reg_x()
{
  X(4) = bar(3);
}



/* to define Move_Ret_To_Reg_Y() */

void
TRANS_move_ret_to_reg_y()
{
  YY(2) = bar(3);
}



/* to define Move_Ret_To_Pl_Foreign_L() */

void
TRANS_move_ret_to_pl_foreign_l()
{
  pl_foreign_long[123] = bar(3);
}




/* to define Move_Ret_To_Pl_Foreign_D() */

void
TRANS_move_ret_to_pl_foreign_d()
{
  double bard(void);

  pl_foreign_double[123] = bard();
}




/* to define Cmp_Ret_And_Int() */

void
TRANS_cmp_ret_and_int()
{
  if (bar(foo) == 0)		/* case ret = 0 */
    goto a;

  if (bar(foo) == 12345678)	/* case ret !- 0 */
    goto a;

  x++;
a:;
}


/* to define Jump_If_Equal() */

void
TRANS_jump_if_equal()
{
  if (x == y)
    goto a;

  x++;
a:;
}




/* to define Jump_If_Greater() */
/* maybe the C compiler does not generate a jg but a jl, reverse if needed */

void
TRANS_jump_if_greater()
{
  if (x > 12)
    goto a;

  if (y > x)
    goto a;

  x++;
a:
  foo(1);
}


/* to define C_Ret() */

void
TRANS_c_ret()
{
}



/* to define Dico_String_Start() + Dico_String() + Dico_String_Stop() */
/* see definitions of strings in the asm file produced                */

void
TRANS_dico_string()
{
  bar("str1", "str2", "str3", "str\r\tend\n", "str\019toto");
}




/* to define Dico_Long_Start() + Dico_Long() + Dico_Long_Stop()         */
/* see definitions of intptr_ts in the asm file produced (global/not global)*/

static intptr_t var_long_static_uninit;
static intptr_t var_long_static_init0;
static intptr_t var_long_static_init100 = 100;
intptr_t var_long_common_unint;
intptr_t var_long_common_init128 = 128;


intptr_t ma_array[5000];
intptr_t ma_global_var1;
intptr_t ma_global_var2 = 12345;
static intptr_t ma_local_var1;
static intptr_t ma_local_var2 = 128;


static intptr_t var_array_static128[128];
intptr_t var_array_common128[128];

/* to define Data_Start() + Data_Stop */
/* between obj_chain_start and obj_chain_stop */

static void
initializer_fct()
{ /* the following printf to ensure gcc does not remove unused static vars */
  printf("%p %p\n", &ma_local_var1, &ma_local_var2);
  printf("%ld %ld %ld %p\n", var_long_static_uninit, var_long_static_init0,
	 var_long_static_init100, var_array_static128);
  dummy(12);
}



# if 0

/* this should not be useful */

int
see_switch()
{
  int y;
  int x = bar();

  if (x == 10)
    y += 11;
  if (x > 10)
    y += 12;
  if (x < 10)
    y += 13;


  if (x == 0)
    y += 1;
  if (x > 0)
    y += 2;
  if (x < 0)
    y += 3;

  return y;
}


#endif
