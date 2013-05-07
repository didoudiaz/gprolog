/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_to_c.h                                                       *
 * Descr.: FD to C macros - header file                                    *
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



#ifndef _FD_TO_C_H
#define _FD_TO_C_H

#include <stdio.h>

#include "../EnginePl/pl_params.h"
#include "../EnginePl/wam_archi.h"
#include "engine_fd.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/



	  /* Environment Frame */

#define Frame_Variable(fv)         ((WamWord *)(AF[fv]))
#define Frame_Range_Parameter(fp)  ((Range *)  (AF[fp]))
#define Frame_Term_Parameter(fp)   ((int)      (AF[fp]))
#define Frame_List_Parameter(fp)   ((WamWord *)(AF[fp]))


#define chain_min                  CHAIN_NB_MIN
#define chain_max                  CHAIN_NB_MAX
#define chain_min_max              CHAIN_NB_MIN_MAX
#define chain_dom                  CHAIN_NB_DOM
#define chain_val                  CHAIN_NB_VAL




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/




/*---------------------------------*
 * Auxiliary engine macros         *
 *---------------------------------*/

#define DivDn(x, y)                ((x) / (y))
#define DivUp(x, y)                (((x) + (y) - 1) / (y))


#define R(r_no)                    rr##r_no

	  /* Interface with Prolog clauses instructions */

#define fd_create_a_frame(nb_arg)		\
  AF = CS;					\
  CS += nb_arg;




#define fd_int_in_a_frame(arg, offset)			\
  AF[offset] = (WamWord) Pl_Fd_Prolog_To_Value(fd_##arg);




#define fd_range_in_a_frame(arg, offset)			\
  AF[offset] = (WamWord) Pl_Fd_Prolog_To_Range(fd_##arg);




#define fd_fdv_in_a_frame(arg, offset)				\
  AF[offset] = (WamWord) Pl_Fd_Prolog_To_Fd_Var(fd_##arg, TRUE);




#define fd_fdv_in_a_frame(arg, offset)				\
  AF[offset] = (WamWord) Pl_Fd_Prolog_To_Fd_Var(fd_##arg, TRUE);




#define fd_any_in_a_frame(arg, offset)		\
  AF[offset] = (WamWord) fd_##arg;




#define fd_l_int_in_a_frame(arg, offset)			\
  AF[offset] = (WamWord) Pl_Fd_Prolog_To_Array_Int(fd_##arg);




#define fd_l_range_in_a_frame(arg, offset)			\
  printf("fd_l_range_in_a_frame  not yet implemented...\n");




#define fd_l_fdv_in_a_frame(arg, offset)				\
  AF[offset] = (WamWord) Pl_Fd_Prolog_To_Array_Fdv(fd_##arg, TRUE);




#define fd_l_any_in_a_frame(arg, offset)			\
  AF[offset] = (WamWord) Pl_Fd_Prolog_To_Array_Any(fd_##arg);




#define fd_cf_in_a_frame(offset)		\
  AF[offset] = (WamWord) CF;




#define fd_call_internal(fct_name)			\
  if (!fct_name(AF))					\
    {							\
      ret_val = FALSE;					\
      goto lab_exit;					\
    }




#define fd_call_internal_and_test_switch_simple(fct_name)	\
{								\
  PlLong (*fct) () = (PlLong (*)()) fct_name(AF);		\
								\
  if (fct == (PlLong (*)()) FALSE)				\
    {								\
      ret_val = FALSE;						\
      goto lab_exit;						\
    }								\
  if (fct != (PlLong (*)()) TRUE)/* FD switch case triggered */	\
    {								\
      if ((*fct) (AF) == FALSE)					\
	{							\
	  ret_val = FALSE;					\
	  goto lab_exit;					\
	}							\
    }								\
}




#define fd_call_internal_and_test_switch(fct_name)		\
{								\
  PlLong (*fct) () = (PlLong (*)()) fct_name(AF);		\
								\
  if (fct == (PlLong (*)()) FALSE)				\
    {								\
      ret_val = FALSE;						\
      goto lab_exit;						\
    }								\
  if (fct != (PlLong (*)()) TRUE)/* FD switch case triggered */	\
    {								\
      if ((*fct) (AF) == FALSE)					\
	{							\
	  ret_val = FALSE;					\
	  goto lab_exit;					\
	}							\
								\
      Pl_Fd_Stop_Constraint(CF);				\
    }								\
}






#define fd_stop_constraint(offset)					  \
  if (AF[offset])                                                         \
    Pl_Fd_Stop_Constraint((WamWord *) (AF[offset]));




	  /* Install instructions */

#define fd_create_c_frame(fct_name, tell_fv, optim2)			   \
  CF = Pl_Fd_Create_C_Frame(fct_name, AF, 				   \
                         (tell_fv == -1) ? NULL : Frame_Variable(tell_fv), \
                         optim2);




#define fd_add_dependency(fv, ch)				\
  Pl_Fd_Add_Dependency(Frame_Variable(fv), chain_##ch, CF);




#define fd_add_list_dependency(fv, ch)				\
  Pl_Fd_Add_List_Dependency(Frame_Variable(fv), chain_##ch, CF);




	  /* Constraint instructions */

#define fd_before_add_constraint		\
  Pl_Fd_Before_Add_Cstr();




#define fd_after_add_constraint			\
  if (!Pl_Fd_After_Add_Cstr())			\
    {						\
      ret_val = FALSE;				\
      goto lab_exit;				\
    }



#define fd_allocate				\
{						\
  WamWord *save_CS = CS;			\
  CS += pl_vec_size;




#define fd_deallocate				\
  CS = save_CS;					\
}




#define fd_tell_value(fv, t)			\
{						\
  fdv_adr = Frame_Variable(fv);			\
  if (!Pl_Fd_Tell_Value(fdv_adr, t))		\
    {						\
      ret_val = FALSE;				\
      goto lab_exit;				\
    }						\
}




#define fd_tell_not_value(fv, t)		\
{						\
  fdv_adr = Frame_Variable(fv);			\
  if (!Pl_Fd_Tell_Not_Value(fdv_adr, t))	\
    {						\
      ret_val = FALSE;				\
      goto lab_exit;				\
    }						\
}




#define fd_tell_interval(fv, t_min, t_max)		\
{							\
  fdv_adr = Frame_Variable(fv);				\
  if (!Pl_Fd_Tell_Interval(fdv_adr, t_min, t_max))	\
    {							\
      ret_val = FALSE;					\
      goto lab_exit;					\
    }							\
}




#define fd_tell_range(fv, r)			\
{						\
  fdv_adr = Frame_Variable(fv);			\
  if (!Pl_Fd_Tell_Range(fdv_adr, &R(r)))	\
    {						\
      ret_val = FALSE;				\
      goto lab_exit;				\
    }						\
}




#define fd_check_fct(fct)			\
  if (!fct)					\
    {						\
      ret_val = FALSE;				\
      goto lab_exit;				\
    }


	  /* Tests */

#define fd_test_exit_condition(t)		\
  if (t)					\
    goto lab_exit;



#define fd_test_fail_condition(t)		\
  if (!t)					\
    {						\
      ret_val = FALSE;				\
      goto lab_exit;				\
    }





#define fd_test_switch_condition(t, fct_name)	\
  if (t)					\
    {						\
      ret_val = (PlLong) fct_name;		\
      goto lab_exit;				\
    }




	  /* Range */

#define fd_range_interval(r, t_min, t_max)	\
  Range_Init_Interval(&R(r), t_min, t_max);




#define fd_load_range(r, fp)			\
  R(r).vec = NULL;				\
  Pl_Range_Copy(&R(r), Frame_Range_Parameter(fp));




#define fd_load_dom(r, fv)			\
  fdv_adr = Frame_Variable(fv);			\
  R(r).vec = NULL;				\
  Pl_Range_Copy(&R(r), Range(fdv_adr));




#define fd_range_union(r, r1)			\
  Pl_Range_Union(&R(r), &R(r1));




#define fd_range_inter(r, r1)			\
  Pl_Range_Inter(&R(r), &R(r1));




#define fd_range_compl(r)			\
  Pl_Range_Compl(&R(r));




#define fd_range_empty(r)			\
  R(r).vec = NULL;				\
  Set_To_Empty(&R(r));




#define fd_range_full(r)				\
  Range_Init_Interval(&R(r), 0, INTERVAL_MAX_INTEGER);




#define fd_range_set_value(r, t)		\
  Pl_Range_Set_Value(&R(r), t);




#define fd_range_reset_value(r, t)		\
  Pl_Range_Reset_Value(&R(r), t);




#define fd_range_add_range(r, r1)		\
  Pl_Range_Add_Range(&R(r), &R(r1));




#define fd_range_sub_range(r, r1)		\
  Pl_Range_Sub_Range(&R(r), &R(r1));




#define fd_range_mul_range(r, r1)		\
  Pl_Range_Mul_Range(&R(r), &R(r1));




#define fd_range_div_range(r, r1)		\
  Pl_Range_Div_Range(&R(r), &R(r1));




#define fd_range_mod_range(r, r1)		\
  Pl_Range_Mod_Range(&R(r), &R(r1));




#define fd_range_add_value(r, t)		\
  Pl_Range_Add_Value(&R(r), t);




#define fd_range_sub_value(r, t)		\
  Pl_Range_Add_Value(&R(r), -(t));




#define fd_range_mul_value(r, t)		\
  Pl_Range_Mul_Value(&R(r), t);




#define fd_range_div_value(r, t)		\
  Pl_Range_Div_Value(&R(r), t);




#define fd_range_mod_value(r, t)		\
  Pl_Range_Mod_Value(&R(r), t);




#define fd_range_copy(r, r1)			\
  R(r).vec = NULL;				\
  Pl_Range_Copy(&R(r), &R(r1));




#define fd_range_fct(fct_name, r, args)		\
{						\
  void fct_name();				\
  R(r).vec = NULL;				\
  fct_name(&R(r), args);			\
}



	  /* term */

#define fd_load_int(var_name, fp)		\
  var_name = Frame_Term_Parameter(fp);




#define fd_load_min(var_name, fv)		\
  fdv_adr = Frame_Variable(fv);			\
  var_name = Min(fdv_adr);




#define fd_load_max(var_name, fv)		\
  fdv_adr = Frame_Variable(fv);			\
  var_name = Max(fdv_adr);




#define fd_load_min_max(var_name_min, var_name_max, fv)	\
  fdv_adr = Frame_Variable(fv);				\
  var_name_min = Min(fdv_adr);				\
  var_name_max = Max(fdv_adr);




#define fd_load_val(var_name, fv)		\
  fdv_adr = Frame_Variable(fv);			\
  if (Fd_Variable_Is_Ground(fdv_adr))		\
      var_name = Min(fdv_adr);			\
   else						\
      goto lab_exit;




#define fd_min_of_range(var_name, r)		\
    var_name = R(r).min;




#define fd_max_of_range(var_name, r)		\
    var_name = R(r).max;




#define fd_value_copy(t, t1)			\
    (t) = (t1);




#define fd_load_l_int(var_name, fp)		\
  var_name = Frame_List_Parameter(fp);




#define fd_load_l_fdv(var_name, fp)		\
  var_name = Frame_List_Parameter(fp);




#define fd_load_l_any(var_name, fp)		\
  var_name = Frame_List_Parameter(fp);




#define arg_1(a1)                                 a1
#define arg_2(a1, a2)                             a1, a2
#define arg_3(a1, a2, a3)                         a1, a2, a3
#define arg_4(a1, a2, a3, a4)                     a1, a2, a3, a4
#define arg_5(a1, a2, a3, a4, a5)                 a1, a2, a3, a4, a5
#define arg_6(a1, a2, a3, a4, a5, a6)             a1, a2, a3, a4, a5, a6
#define arg_7(a1, a2, a3, a4, a5, a6, a7)         a1, a2, a3, a4, a5, a6, a7
#define arg_8(a1, a2, a3, a4, a5, a6, a7, a8)     a1, a2, a3, a4, a5, a6, a7, a8
#define arg_9(a1, a2, a3, a4, a5, a6, a7, a8, a9) a1, a2, a3, a4, a5, a6, a7, a8, a9




#define range_arg(r)              &R(r)	/* by address */




/*---------------------------------*
 * Interface with C files          *
 *---------------------------------*/

#define max_integer                INTERVAL_MAX_INTEGER




#define FdArg(arg)                 WamWord fd_##arg




#define fd_begin_user_constraint(name_args)	\
Bool						\
name_args					\
{						\
  WamWord *AF;					\
  PlLong ret_val = TRUE;




#define fd_end_user_constraint			\
}




#define fd_begin_internal(fct_name)		\
static PlLong					\
fct_name(WamWord *AF)				\
{						\
  PlLong ret_val = TRUE;




#define fd_end_internal				\
}




#define fd_exit_point				\
  lab_exit:



#define fd_return				\
  return ret_val;




#define fd_local_value_var(var_name)		\
  int var_name;




#define fd_local_range_var(r)			\
  Range R(r);    /*  = {FALSE, 0, 0, NULL} init should be useless  */




#define fd_local_l_int_var(var_name)		\
  WamWord *var_name;




#define fd_local_l_fdv_var(var_name)		\
  WamWord *var_name;




#define fd_local_l_any_var(var_name)		\
  WamWord *var_name;




#define fd_local_cf_pointer			\
  WamWord *CF;


#ifdef __GNUC__
#define fd_local_fdv_adr			\
  WamWord *fdv_adr __attribute__((unused));
#else
#define fd_local_fdv_adr			\
  WamWord *fdv_adr;
#endif



#define fd_init_local_value_var(var_name, term)	\
  var_name = (term);




#define fd_forall(fv, l_fv)	\
{				\
  int n = *l_fv++;		\
  while (n--)			\
    {				\
       AF[fv] = *l_fv++;




#define fd_forall_end      	\
    }				\
}


#endif /* !_FD_TO_C_H */
