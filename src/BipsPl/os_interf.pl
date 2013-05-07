/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : os_interf.pl                                                    *
 * Descr.: operating system interface management                           *
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


:-	built_in.

'$use_os_interf'.


make_directory(PathName) :-
	set_bip_name(make_directory, 1),
	'$call_c_test'('Pl_Make_Directory_1'(PathName)).




delete_directory(PathName) :-
	set_bip_name(delete_directory, 2),
	'$call_c_test'('Pl_Delete_Directory_1'(PathName)).




working_directory(Path) :-
	set_bip_name(working_directory, 1),
	'$call_c_test'('Pl_Working_Directory_1'(Path)).




change_directory(Path) :-
	set_bip_name(change_directory, 1),
	'$call_c_test'('Pl_Change_Directory_1'(Path)).



directory_files(PathName, List) :-
	set_bip_name(directory_files, 2),
	'$call_c_test'('Pl_Directory_Files_2'(PathName, List)).




rename_file(PathName1, PathName2) :-
	set_bip_name(rename_file, 2),
	'$call_c_test'('Pl_Rename_File_2'(PathName1, PathName2)).




unlink(PathName) :-
	set_bip_name(unlink, 2),
	'$call_c'('Pl_Unlink_1'(PathName)).




delete_file(PathName) :-
	set_bip_name(delete_file, 2),
	'$call_c_test'('Pl_Delete_File_1'(PathName)).




file_exists(PathName) :-
	set_bip_name(file_exists, 1),
	'$call_c_test'('Pl_File_Exists_1'(PathName)).


file_permission(PathName, PermList) :-
	set_bip_name(file_permission, 2),
	'$call_c_test'('Pl_File_Permission_2'(PathName, PermList)).




file_property(PathName, Property) :-
	set_bip_name(file_property, 2),
	'$check_file_prop'(Property), !,
	'$file_prop'(Property, PathName).




'$check_file_prop'(Property) :-
	var(Property).

'$check_file_prop'(absolute_file_name(_)).

'$check_file_prop'(real_file_name(_)).

'$check_file_prop'(type(_)).

'$check_file_prop'(size(_)).

'$check_file_prop'(permission(_)).

'$check_file_prop'(creation(_)).

'$check_file_prop'(last_access(_)).

'$check_file_prop'(last_modification(_)).

'$check_file_prop'(Property) :-
	'$pl_err_domain'(os_file_property, Property).




'$file_prop'(absolute_file_name(AbsolutePathName), PathName) :-
	'$call_c_test'('Pl_File_Prop_Absolute_File_Name_2'(AbsolutePathName, PathName)).

'$file_prop'(real_file_name(RealPathName), PathName) :-
	'$call_c_test'('Pl_File_Prop_Real_File_Name_2'(RealPathName, PathName)).

'$file_prop'(type(Type), PathName) :-
	'$call_c_test'('Pl_File_Prop_Type_2'(Type, PathName)).

'$file_prop'(size(Size), PathName) :-
	'$call_c_test'('Pl_File_Prop_Size_2'(Size, PathName)).

'$file_prop'(permission(Perm), PathName) :-
	'$call_c_test'('Pl_Check_Prop_Perm_And_File_2'(Perm, PathName)),
	'$file_prop_perm'(Perm, PathName).

'$file_prop'(creation(DateTime), PathName) :-
	'$sys_var_write'(0, 0),
	'$call_c_test'('Pl_File_Prop_Date_2'(DateTime, PathName)).

'$file_prop'(last_access(DateTime), PathName) :-
	'$sys_var_write'(0, 1),
	'$call_c_test'('Pl_File_Prop_Date_2'(DateTime, PathName)).

'$file_prop'(last_modification(DateTime), PathName) :-
	'$sys_var_write'(0, 2),
	'$call_c_test'('Pl_File_Prop_Date_2'(DateTime, PathName)).


'$file_prop_perm'(read, PathName) :-
	file_permission(PathName, read).

'$file_prop_perm'(write, PathName) :-
	file_permission(PathName, write).

'$file_prop_perm'(execute, PathName) :-
	file_permission(PathName, execute).

'$file_prop_perm'(search, PathName) :-
	file_permission(PathName, search).




temporary_name(Template, PathName) :-
	set_bip_name(temporary_name, 2),
	'$call_c_test'('Pl_Temporary_Name_2'(Template, PathName)).




temporary_file(Dir, Prefix, PathName) :-
	set_bip_name(temporary_file, 3),
	'$call_c_test'('Pl_Temporary_File_3'(Dir, Prefix, PathName)).




date_time(DateTime) :-
	set_bip_name(date_time, 1),
	'$call_c_test'('Pl_Date_Time_1'(DateTime)).




host_name(HostName) :-
	set_bip_name(host_name, 1),
	'$call_c_test'('Pl_Host_Name_1'(HostName)).




os_version(OsVersion) :-
	set_bip_name(os_version, 1),
	'$call_c_test'('Pl_Os_Version_1'(OsVersion)).




architecture(Architecture) :-
	set_bip_name(architecture, 1),
	'$call_c_test'('Pl_Architecture_1'(Architecture)).




shell :-
	set_bip_name(shell, 0),
	'$call_c_test'('Pl_Shell_2'('', 0)).

shell(Cmd) :-
	set_bip_name(shell, 1),
	'$call_c_test'('Pl_Shell_2'(Cmd, 0)).

shell(Cmd, Status) :-
	set_bip_name(shell, 2),
	'$call_c_test'('Pl_Shell_2'(Cmd, Status)).




system(Cmd) :-
	set_bip_name(system, 1),
	'$call_c_test'('Pl_System_2'(Cmd, 0)).

system(Cmd, Status) :-
	set_bip_name(system, 2),
	'$call_c_test'('Pl_System_2'(Cmd, Status)).




spawn(Cmd, LArg) :-
	set_bip_name(spawn, 2),
	'$call_c_test'('Pl_Spawn_3'(Cmd, LArg, 0)).

spawn(Cmd, LArg, Status) :-
	set_bip_name(spawn, 3),
	'$call_c_test'('Pl_Spawn_3'(Cmd, LArg, Status)).




sleep(Seconds) :-
	set_bip_name(sleep, 1),
	'$call_c'('Pl_Sleep_1'(Seconds)).




popen(Cmd, Mode, Stream) :-
	set_bip_name(popen, 3),
	'$get_open_stm'(Stream, Stm),
	'$call_c_test'('Pl_Popen_3'(Cmd, Mode, Stm)).



          % exec mask in sys_var[0]:
          %     b0
          %     0/1
          %  Pid used

exec(Cmd, StreamIn, StreamOut, StreamErr, Pid) :-
	set_bip_name(exec, 5),
	(   nonvar(Pid) ->
	    '$pl_err_uninstantiation'(Pid)
	;   true
	),
	'$sys_var_write'(0, 0),
	'$sys_var_set_bit'(0, 0),
	'$exec'(Cmd, StreamIn, StreamOut, StreamErr, Pid).



exec(Cmd, StreamIn, StreamOut, StreamErr) :-
	set_bip_name(exec, 4),
	'$sys_var_write'(0, 0),
	'$exec'(Cmd, StreamIn, StreamOut, StreamErr, 0).




'$exec'(Cmd, StreamIn, StreamOut, StreamErr, Pid) :-
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$get_open_stm'(StreamErr, StmErr),
	'$call_c_test'('Pl_Exec_5'(Cmd, StmIn, StmOut, StmErr, Pid)).





create_pipe(StreamIn, StreamOut) :-
	set_bip_name(create_pipe, 2),
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$call_c_test'('Pl_Create_Pipe_2'(StmIn, StmOut)).




fork_prolog(Pid) :-
	set_bip_name(fork_prolog, 1),
	(   nonvar(Pid) ->
	    '$pl_err_uninstantiation'(Pid)
	;   true
	),
	'$call_c_test'('Pl_Fork_Prolog_1'(Pid)).




select(Reads, ReadyReads, Writes, ReadyWrites, TimeOut) :-
	set_bip_name(select, 5),
	'$call_c_test'('Pl_Select_5'(Reads, ReadyReads, Writes, ReadyWrites, TimeOut)).




prolog_pid(PrologPid) :-
	set_bip_name(prolog_pid, 1),
	'$call_c_test'('Pl_Prolog_Pid_1'(PrologPid)).




send_signal(Pid, Signal) :-
	set_bip_name(send_signal, 2),
	'$call_c_test'('Pl_Send_Signal_2'(Pid, Signal)).




wait(Pid, Status) :-
	set_bip_name(wait, 2),
	'$call_c_test'('Pl_Wait_2'(Pid, Status)).
