#include <stdio.h>
#include <stdlib.h>

/* to define Data_Start(intializer_fct) */
#define OBJ_INIT initializer_fct


#include "../../EnginePl/gp_config.h"
#include "../../EnginePl/pl_params.h"
#if 1 /* #if 0, cp ../../EnginePl/wam_archi.h . and customize it if needed */
#include "../../EnginePl/wam_archi.h"
#else
#include "wam_archi.h"
#endif

#if 0
/* to define Reload_E_In_Register() */
register PlLong *EINREG  asm("%l0");

void
TRANS_Reload_E_In_Register(void)
{
  EINREG = E;
}
#endif

#include "../../EnginePl/machine.h"
#include "../../EnginePl/obj_chain.h"
#include "../../EnginePl/wam_inst.h"


#include "mach.h"


#define YY(k) Y(E,k)

#define FL(k) pl_foreign_long[k]
#define FD(k) pl_foreign_double[k]


extern PlLong pl_foreign_long[];
extern double pl_foreign_double[];

/* to define Dico_Long_Start() + Dico_Long() + Dico_Long_Stop() */
/* see definitions of PlLongs in the asm file produced (global/not global) */

static PlLong var_long_static_uninit;
static PlLong var_long_static_init0;
static PlLong var_long_static_init100 = 100;
PlLong var_long_common_unint;
PlLong var_long_common_init128 = 128;


PlLong ma_array[5000];
PlLong ma_global_var1;
PlLong ma_global_var2 = 12345;
static PlLong ma_local_var1;
static PlLong ma_local_var2 = 128;

static PlLong var_array_static128[128];
PlLong var_array_common128[128];

PlLong Dummy();
PlLong Dummy1();
PlLong Dummy2();
PlLong Save_CP();
PlLong foo();
PlLong bar();
PlLong bar1();
PlLong bar2();

extern PlULong var;
extern void *label;
PlLong v1[100];
static PlLong v2[100];
extern PlLong v3[100];
PlLong x, y;




/* to define Data_Start() + Data_Stop */

static void
initializer_fct(void)
{ /* printf to ensure gcc does not remove unused static vars */
  printf("%p %p\n", &ma_local_var1, &ma_local_var2);
  printf("%ld %ld %ld %p\n", var_long_static_uninit, var_long_static_init0,
	 var_long_static_init100, var_array_static128);
  Dummy(12);
}


/* to define Asm_Start() */

/* to define Code_Start() (global/local) and Code_Stop() */

void
TRANS_Code_Start_global(void)
{
  Dummy();
  Save_CP();
  foo(1,2,3);
}



static void
TRANS_Code_Start_local(void)
{
  Dummy();
}




/* to define Pl_Jump() */

void
TRANS_Pl_Jump(void)
{
  M_Direct_Goto(label);
_label:;
}

//static
void check(void)
{
  CodePtr adr = (CodePtr) &&cont;
  Dummy1(adr);
 cont:
  Dummy2(CP);
}


/* to define Pl_Call() */

void
TRANS_Pl_Call(void)
{
#ifdef __GNUC__
  CP = (CodePtr) &&cont;
  M_Direct_Goto(lab);
 cont:
#else
  CP = (CodePtr) foo;
  M_Direct_Goto(lab);
_lab:;
#endif
  Dummy(CP);
}


void TRANS_Pl_Call_2(void)
{
  CP = (CodePtr) &&cont;
  foo();
 cont:
  CP = (CodePtr) &var;
  Dummy(CP);
 cont2:
  CP = (CodePtr) &&cont2;
  Dummy(var);
}

/* to define Pl_Fail() */

void
TRANS_Pl_Fail(void)
{
  M_Indirect_Goto(ALTB(B));
}





/* to define Pl_Ret() */

void
TRANS_Pl_Ret(void)
{
  M_Indirect_Goto(CP);
}


/* to define Prep_CP() and Here_CP() */
void
TRANS_Prep_CP_Here_CP(void) {
  CP = &&a;
  if (x<3) {
    bar(x);
  }
 a:;
}


/* to define Jump() */

void
TRANS_Jump(void)
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
TRANS_Move_X_To_X(void)
{
  X(3) = X(5);
}

void
TRANS_Move_X_To_Y(void)
{
  YY(10) = X(2);
}

void
TRANS_Move_Y_To_X(void)
{
  X(0) = YY(3);
}

void
TRANS_Move_Y_To_Y(void)
{
  YY(2) = YY(4);
}


void
test_move_x_y(void)
{
  YY(3) = X(0);
  YY(0) = X(10);
  YY(15) = X(255);
  YY(15000) = X(255);
  YY(16000) = X(10);
}

void
test_move_y_x(void)
{
  X(0)  = YY(0); 
  X(31) = YY(10);
  X(12) = YY(23);
  X(254) = YY(16000);
}




/* to define Call_C_Start() + Call_C_Arg_xxx() + Call_C_Stop() */

void
TRANS_Call_C(void)
{
  Dummy();
  /*  &label,var,int, double,         string */
  bar(foo, var, (PlLong) 12, (PlLong) 4098, (PlLong) -4095, (double) 1.20e-10, "this is a string",
      "a\14b");

  /* v(index) */
  bar1(v1[2], v1[0], &v1[12], v2[2], v2[0], &v2[4]);
  bar1(v3[4], &v3[2], v3[0]);

  /* regs / &regs */
  bar2(X(0), &X(4), YY(0), &YY(12));
}



void
test_call_c_arg_int(void)
{
  void FC test_arg_int1(PlLong a, PlLong b, PlLong c, PlLong d);

  test_arg_int1(12, -1, 4095, 123456789);
}


void
test_call_c_arg_double(void)
{
  void FC  test_arg_double1(double a, double b, double c, double d, double e, double f);
  
  test_arg_double1(12.456, -1.3e-102, -3.141593, 12.456, -1.3e-102, -3.141593);
}


void
test_call_c_arg_mixed(void)
{
  void FC test_arg_mixed1(PlLong ai, double a, double b, PlLong bi, PlLong ci, double c, PlLong di);

  test_arg_mixed1(-19, 12.456, -1.3e-102, 365, 987654321, -3.141593, -110101);
}




void
test_call_c_arg_string(void)
{
  void FC test_arg_string1(char *a, char *b);

  test_arg_string1("a string", "abcd\01489d\37711ef\n\r");
}


void
test_call_c_arg_mem_l(void)
{
  void FC test_arg_mem_l1(PlLong a, PlLong b, PlLong *c, PlLong d, PlLong e, PlLong *f);

  test_arg_mem_l1(ma_local_var2,ma_global_var2,(PlLong *) &test_arg_mem_l1,ma_array[0],ma_array[4097],&ma_array[4500]);
}


void
test_call_c_arg_x(void)
{
  void FC test_arg_x1(PlLong a, PlLong *b, PlLong c, PlLong *d);

  test_arg_x1(X(0), &X(0), X(255), &X(128));
}


void
test_call_c_arg_y(void)
{
  void FC test_arg_y1(PlLong a, PlLong *b, PlLong c, PlLong *d, PlLong e);
  
  test_arg_y1(YY(0), &YY(0), YY(12), &YY(6), YY(17000));
}


void
test_call_c_arg_fl_array(void)
{
  void FC test_arg_fl_array1(PlLong a, PlLong b, PlLong *c, PlLong *d);

  test_arg_fl_array1(FL(0), FL(10), &FL(0), &FL(56));
}


void
test_call_c_arg_fd_array(void)
{
  void FC  test_arg_fd_array1(double a, double b, double *c, double *d);

  test_arg_fd_array1(FD(0), FD(47), &FD(0), &FD(127));
}

void
test_call_c_lot_args(void)
{
  void FC test_call_c_lot_args1(double n0, PlLong n1, double n2, PlLong n3, double n4, PlLong n5,
				void (*a) (), PlLong b, PlLong c, PlLong d, double e, char *f,
				PlLong g, PlLong *h, PlLong i, PlLong *j,
				PlLong k, PlLong *l, PlLong m, PlLong *n, double o);
  
  test_call_c_lot_args1(0.1, 0, 0.2, 0, 0.3, 0, &test_call_c_lot_args1, ma_local_var2, 4095, 123456789, -3.141593, "abcd\01489def\n\r", X(0), &X(0), X(255), &X(128), YY(0), &YY(0), YY(12), &YY(6), 1.23456);
}

void
test_call_c_lot_args2(void)
{
  /*  &label,var,int, double,         string */
  bar(3, 4, 5, 6, 7, 8, 9, 10, foo, var, 12, 4098, -4095, (double) 1.20e-10, "this is a string", "a\14b");

  /* regs / &regs */
  bar1(0, 0, 0, 0, 0, 0, X(2), &X(4), YY(0), &YY(12));
}


void
test_call_c_lot_args3(void)
{
  void my_func(PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong, PlLong);

  my_func(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19);
}


void test_call_c_lot_args_dbl(void)
{
  void my_lot_dbl(double, double, double, double, double, double, double, double, double, double, double, double, double, double, double, double, double, double, double, double);
  my_lot_dbl(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18, 19.19, 20.20);
}


/* to define Jump_Ret() */

void
TRANS_Jump_Ret(void)
{
#if defined(M_ix86_win32)
  register PlLong adr = (PlLong) bar(12, "toto");

  _asm { jmp adr }
#elif defined(__clang__)
  register PlLong adr = (PlLong) bar(12, "toto");
  M_Indirect_Goto(adr);
#else
  goto *bar(12, "toto");
#endif
}





/* to define Fail_Ret() */

void
TRANS_Fail_Ret(void)
{
  if (bar(1, 2, 3) == 0)
    goto a;

  x++;
a:;
}



/* to define Move_Ret_To_Mem_L() */

void
TRANS_Move_Ret_To_Mem_L(void)
{
  var = bar(3);
  v1[40] = bar(15);
}


void
test_move_ret_mem(void)
{
  PlLong FC test_move_ret_mem1(void);

  ma_global_var1 = test_move_ret_mem1();

  ma_array[64] = test_move_ret_mem1();

  ma_array[4097] = test_move_ret_mem1();
}

/* to define Move_Ret_To_Reg_X() */

void
TRANS_Move_Ret_To_Reg_X(void)
{
  X(4) = bar(3);
}


void
test_move_ret_x(void)
{
  PlLong FC test_move_ret_x1(void);
 
  X(0) = test_move_ret_x1();
  X(255) = test_move_ret_x1();
}


/* to define Move_Ret_To_Reg_Y() */

void
TRANS_Move_Ret_To_Reg_Y(void)
{
  YY(2) = bar(3);
}


void
test_move_ret_y(void)
{
  PlLong FC test_move_ret_y1(void);
 
  YY(0) = test_move_ret_y1();
  YY(18000) = test_move_ret_y1();
}



/* to define Move_Ret_To_Foreign_L() */

void
TRANS_Move_Ret_To_Foreign_L(void)
{
  FL(123) = bar(3);
}


void
test_move_ret_fl(void)
{
  PlLong FC test_move_ret_fl1(void);
 
  FL(0) = test_move_ret_fl1();
  FL(11) = test_move_ret_fl1();
}


/* to define Move_Ret_To_Foreign_D() */

void
TRANS_Move_Ret_To_Foreign_D(void)
{
  double bard(void);

  FD(123) = bard();
}


void
test_move_ret_fd(void)
{
  double FC test_move_ret_fd1(void);
 
  FD(0) = test_move_ret_fd1();
  FD(11) = test_move_ret_fd1();
}




/* to define Cmp_Ret_And_Int() */

void
TRANS_Cmp_Ret_And_Int(void)
{
  if (bar(foo) == 0)		/* case ret = 0 */
    goto a;

  if (bar(foo) == 12345678)	/* case ret != 0 */
    goto a;

  x++;
a:;
}


/* to define Jump_If_Equal() */

void
TRANS_Jump_If_Equal(void)
{
  if (x == y)
    goto a;

  x++;
a:;
}




/* to define Jump_If_Greater() */
/* maybe the C compiler does not generate a jg but a jl, reverse if needed */

void
TRANS_Jump_If_Greater(void)
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
TRANS_C_Ret(void)
{
}



/* to define Dico_String_Start() + Dico_String() + Dico_String_Stop() */
/* see definitions of strings in the asm file produced                */

void
TRANS_Dico_String(void)
{
  bar("str1", "str2", "str3", "str\r\tend\n", "str\019toto");
  bar("1", "12", "123", "1234", "12345", "123456", "1234567", "12345678", "123456789", "123456789A", "123456789AB", "123456789ABC", "123456789ABCD", "123456789ABCDE", "123456789ABCDEF", "123456789ABCDEF1");
}






# if 0  /* this should not be useful */
int
see_switch(void)
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

#if 0
extern PlLong vlong;
void barr1(PlLong *);
void barr2(PlLong);
void my_external_adr(void)
{
  barr1(&vlong);
}

void my_external_content(void)
{
  barr2(vlong);
}
#endif
