/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : sockets.pl                                                      *
 * Descr.: sockets management                                              *
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

'$use_sockets'.


socket(Domain, Socket) :-
	set_bip_name(socket, 2),
	'$call_c_test'('Pl_Socket_2'(Domain, Socket)).




socket_close(Socket) :-
	set_bip_name(socket_close, 1),
	'$call_c_test'('Pl_Socket_Close_1'(Socket)).




socket_bind(Socket, Address) :-
	set_bip_name(socket_bind, 2),
	'$call_c_test'('Pl_Socket_Bind_2'(Socket, Address)).




socket_connect(Socket, Address, StreamIn, StreamOut) :-
	set_bip_name(socket_connect, 4),
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$call_c_test'('Pl_Socket_Connect_4'(Socket, Address, StmIn, StmOut)).




socket_listen(Socket, Length) :-
	set_bip_name(socket_listen, 2),
	'$call_c_test'('Pl_Socket_Listen_2'(Socket, Length)).




socket_accept(Socket, StreamIn, StreamOut) :-
	set_bip_name(socket_accept, 3),
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$call_c_test'('Pl_Socket_Accept_4'(Socket, _, StmIn, StmOut)).


socket_accept(Socket, Client, StreamIn, StreamOut) :-
	set_bip_name(socket_accept, 4),
	(   var(Client) ->
	    true
	;   '$pl_err_uninstantiation'(Client)
	),
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$call_c_test'('Pl_Socket_Accept_4'(Socket, Client, StmIn, StmOut)).




'$assoc_socket_streams'(Socket, StreamIn, StreamOut) :-
	set_bip_name('$assoc_socket_streams', 3),
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$call_c_test'('Pl_Assoc_Socket_Streams_3'(Socket, StmIn, StmOut)).




hostname_address(HostName, HostAddress) :-
	set_bip_name(hostname_address, 2),
	'$call_c_test'('Pl_Hostname_Address_2'(HostName, HostAddress)).
