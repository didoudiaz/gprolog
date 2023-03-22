/* C file generated for /Users/diaz/GP/src/BipsFD/t_fd.fd */

#include "fd_to_c.h"

/* line:44 begin included code */

Bool Pl_Unify(WamWord, WamWord);

/* line:46 end   included code */

	/*---------------------*/
	/* User constraint: dd */
	/*---------------------*/


	/* Bloc #1 */

fd_begin_internal(dd_bloc_1)

   fd_local_fdv_adr
   fd_local_value_var(intXXX)
   fd_local_any_var(anyx)
   fd_local_any_var(anyy)

   fd_load_int(intXXX,1)
   fd_load_any(anyx,2)
   fd_load_any(anyy,3)
   fd_tell_interval(0,intXXX,Pl_Unify(anyx,anyy))
 fd_exit_point
   fd_return

fd_end_internal


	/* Entry point for dd */

fd_begin_user_constraint(dd(FdArg(V),FdArg(XXX),FdArg(x),FdArg(y)))

   fd_create_a_frame(4)
   fd_fdv_in_a_frame(V,0)
   fd_int_in_a_frame(XXX,1)
   fd_any_in_a_frame(x,2)
   fd_any_in_a_frame(y,3)

   fd_before_add_constraint
   fd_call_internal(dd_bloc_1)
 fd_exit_point
   fd_after_add_constraint
   fd_return

fd_end_user_constraint

