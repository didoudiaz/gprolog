/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : sockets.pl                                                      * 
 * Descr.: sockets management                                              * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2007 Daniel Diaz                                     * 
 *                                                                         * 
 * GNU Prolog is free software; you can redistribute it and/or modify it   * 
 * under the terms of the GNU General Public License as published by the   * 
 * Free Software Foundation; either version 2, or any later version.       * 
 *                                                                         * 
 * GNU Prolog is distributed in the hope that it will be useful, but       * 
 * WITHOUT ANY WARRANTY; without even the implied warranty of              * 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        * 
 * General Public License for more details.                                * 
 *                                                                         * 
 * You should have received a copy of the GNU General Public License along * 
 * with this program; if not, write to the Free Software Foundation, Inc.  * 
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

:-	built_in.

'$use_sockets'.


socket(Domain, Socket) :-
	set_bip_name(socket, 2),
	'$call_c_test'('Socket_2'(Domain, Socket)).




socket_close(Socket) :-
	set_bip_name(socket_close, 1),
	'$call_c_test'('Socket_Close_1'(Socket)).




socket_bind(Socket, Address) :-
	set_bip_name(socket_bind, 2),
	'$call_c_test'('Socket_Bind_2'(Socket, Address)).




socket_connect(Socket, Address, StreamIn, StreamOut) :-
	set_bip_name(socket_connect, 4),
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$call_c_test'('Socket_Connect_4'(Socket, Address, StmIn, StmOut)).




socket_listen(Socket, Length) :-
	set_bip_name(socket_listen, 2),
	'$call_c_test'('Socket_Listen_2'(Socket, Length)).




socket_accept(Socket, StreamIn, StreamOut) :-
	set_bip_name(socket_accept, 3),
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$call_c_test'('Socket_Accept_4'(Socket, _, StmIn, StmOut)).


socket_accept(Socket, Client, StreamIn, StreamOut) :-
	set_bip_name(socket_accept, 4),
	(   var(Client) ->
	    true
	;   '$pl_err_type'(variable, Client)
	),
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$call_c_test'('Socket_Accept_4'(Socket, Client, StmIn, StmOut)).




'$assoc_socket_streams'(Socket, StreamIn, StreamOut) :-
	set_bip_name('$assoc_socket_streams', 3),
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$call_c_test'('Assoc_Socket_Streams_3'(Socket, StmIn, StmOut)).


	

hostname_address(HostName, HostAddress) :-
	set_bip_name(hostname_address, 2),
	'$call_c_test'('Hostname_Address_2'(HostName, HostAddress)).
