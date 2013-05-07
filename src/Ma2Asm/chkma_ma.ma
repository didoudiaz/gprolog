;/*-------------------------------------------------------------------------*
; * GNU Prolog                                                              *
; *                                                                         *
; * Part  : mini-assembler to assembler translator                          *
; * File  : chkma_ma.ma                                                     *
; * Descr.: test file for MA translation                                    *
; * Author: Daniel Diaz                                                     *
; *                                                                         *
; * Copyright (C) 1999-2013 Daniel Diaz                                     *
; *                                                                         *
; * This file is part of GNU Prolog                                         *
; *                                                                         *
; * GNU Prolog is free software: you can redistribute it and/or             *
; * modify it under the terms of either:                                    *
; *                                                                         *
; *   - the GNU Lesser General Public License as published by the Free      *
; *     Software Foundation; either version 3 of the License, or (at your   *
; *     option) any later version.                                          *
; *                                                                         *
; * or                                                                      *
; *                                                                         *
; *   - the GNU General Public License as published by the Free             *
; *     Software Foundation; either version 2 of the License, or (at your   *
; *     option) any later version.                                          *
; *                                                                         *
; * or both in parallel, as here.                                           *
; *                                                                         *
; * GNU Prolog is distributed in the hope that it will be useful,           *
; * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
; * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
; * General Public License for more details.                                *
; *                                                                         *
; * You should have received copies of the GNU General Public License and   *
; * the GNU Lesser General Public License along with this program.  If      *
; * not, see http://www.gnu.org/licenses/.                                  *
; *-------------------------------------------------------------------------*/

; maybe comment this if something goes wrong from start
; (maybe it is call_c which does not work !)

c_code  initializer Object_Initializer
        call_c Initializer()
        c_ret


pl_code global ma_test_pl_jump_and_pl_ret
;	call_c Write_Long(&X(0))
	pl_ret
	pl_jump ma_test_ret

pl_code global ma_test_ret
	pl_ret

pl_code global ma_test_pl_call_and_pl_ret_and_pl_fail
	pl_call ma_test_pl_jump_and_pl_ret
	pl_fail

pl_code global ma_test_prep_cp_and_here_cp
	call_c Save_CP()
	prep_cp
	pl_jump ma_test_ret
	pl_fail
	here_cp
	call_c Restore_CP()
	pl_ret

c_code global ma_test_jump_and_c_ret
	jump lab1
	pl_fail

lab1:
	jump lab2
	pl_fail

lab2:
	c_ret


pl_code global ma_test_call_c
	call_c fast test_call_c1()
	pl_ret

pl_code global ma_test_move_x_y
	call_c fast Allocate(1)
	move X(0),Y(3)
	move X(10),Y(0)
	move X(255),Y(15)
	pl_ret

pl_code global ma_test_move_y_x
	call_c fast Allocate(1)
	move Y(0),X(0)
	move Y(10),X(31)
	move Y(23),X(12)
	pl_ret

pl_code global ma_test_arg_int
	call_c fast test_arg_int1(12,-1,4095,123456789)
	pl_ret

pl_code global ma_test_arg_double
	call_c fast test_arg_double1(12.456,-1.3e-102,-3.141593,12.456,-1.3e-102,-3.141593)
	pl_ret

pl_code global ma_test_arg_mixed
	call_c fast test_arg_mixed1(-19, 12.456, -1.3e-102,365, 987654321, -3.141593, -110101)
	pl_ret

pl_code global ma_test_arg_string
	call_c fast test_arg_string1("a string","abcd\01489d\37711ef\n\r")
	pl_ret

pl_code global ma_test_arg_mem_l
	call_c fast test_arg_mem_l1(ma_local_var2,ma_global_var2,&test_arg_mem_l,ma_array(0),ma_array(4097),&ma_array(4500))
	pl_ret

pl_code global ma_test_arg_x
	call_c fast test_arg_x1(X(0),&X(0),X(255),&X(128))
	pl_ret

pl_code global ma_test_arg_y
	call_c fast Allocate(1)
	call_c fast test_arg_y1(Y(0),&Y(0),Y(12),&Y(6))
	pl_ret

pl_code global ma_test_arg_fl_array
	call_c fast Allocate(1)
	call_c fast test_arg_fl_array1(FL(0),FL(10),&FL(0),&FL(56))
	pl_ret

pl_code global ma_test_arg_fd_array
	call_c fast Allocate(1)
	call_c fast test_arg_fd_array1(FD(0),FD(47),&FD(0),&FD(127))
	pl_ret

pl_code global ma_test_call_c_lot_args
	call_c fast Allocate(1)
	call_c fast test_call_c_lot_args1(0,0,0,0,0,0,&test_call_c_lot_args,ma_local_var2,4095,123456789,-3.141593,"abcd\01489def\n\r",X(0),&X(0),X(255),&X(128),Y(0),&Y(0),Y(12),&Y(6), 1.23456)
	pl_ret

pl_code global ma_test_jump_ret
	call_c fast test_jump_ret1(&ma_test_jump_ret1)
	jump_ret
	pl_ret

pl_code global ma_test_jump_ret1
	call_c fast test_jump_ret2()
	pl_ret

pl_code global ma_test_fail_ret
	call_c fast test_fail_ret1()
	fail_ret
	pl_ret

pl_code global ma_test_move_ret_mem
	call_c fast test_move_ret_mem1()
	move_ret ma_global_var1
	call_c fast test_move_ret_mem1()
	move_ret ma_array(64)
	call_c fast test_move_ret_mem1()
	move_ret ma_array(4097)
	pl_ret

pl_code global ma_test_move_ret_x
	call_c fast test_move_ret_x1()
	move_ret X(0)
	call_c fast test_move_ret_x1()
	move_ret X(255)
	pl_ret

pl_code global ma_test_move_ret_y
	call_c fast Allocate(1)
	call_c fast test_move_ret_y1()
	move_ret Y(0)
	call_c fast test_move_ret_y1()
	move_ret Y(11)
	pl_ret

pl_code global ma_test_move_ret_fl
	call_c fast Allocate(1)
	call_c fast test_move_ret_fl1()
	move_ret FL(0)
	call_c fast test_move_ret_fl1()
	move_ret FL(11)
	pl_ret

pl_code global ma_test_move_ret_fd
	call_c fast Allocate(1)
	call_c fast test_move_ret_fd1()
	move_ret FD(0)
	call_c fast test_move_ret_fd1()
	move_ret FD(11)
	pl_ret

pl_code global ma_test_switch_ret
	call_c fast test_switch_ret1()
	switch_ret (0=sl0,4=sl1,15=sl2,4095=sl3,123456=sl4,2456789=sl5)

sl0:	call_c fast test_switch_ret2(0)
	pl_ret

sl1:	call_c fast test_switch_ret2(1)
	pl_ret

sl2:	call_c fast test_switch_ret2(2)
	pl_ret

sl3:	call_c fast test_switch_ret2(3)
	pl_ret

sl4:	call_c fast test_switch_ret2(4)
	pl_ret

sl5:	call_c fast test_switch_ret2(5)
	pl_ret


; ma_array must be just before ma_global_var1 (in alphabetic order)
long global ma_array(5000)
long global ma_global_var1
long global ma_global_var2 = 12345
long local  ma_local_var1
long local  ma_local_var2 = 128

