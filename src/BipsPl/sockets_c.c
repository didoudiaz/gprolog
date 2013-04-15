/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : sockets_c.c                                                     *
 * Descr.: sockets management - C part                                     *
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

#include "gp_config.h"

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#ifndef _WIN32
#include <unistd.h>
#include <sys/socket.h>
#else
#include <io.h>
#include <fcntl.h>
#include <winsock2.h>
#define SO_OPENTYPE     0x7008
#define SO_SYNCHRONOUS_NONALERT 0x20
#endif

       /* old versions of CYGWIN do not support AF_UNIX - modify next line */
#if defined(__unix__) || defined(__CYGWIN__)
#define SUPPORT_AF_UNIX
#endif

#ifdef SUPPORT_AF_UNIX
#include <sys/un.h>
#endif
#ifndef _WIN32
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#endif

#define OBJ_INIT Socket_Initializer

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef SUPPORT_AF_UNIX
static int atom_AF_UNIX;
#endif
static int atom_AF_INET;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static Bool Create_Socket_Streams(int sock, char *stream_name,
				  int *stm_in, int *stm_out);




/*-------------------------------------------------------------------------*
 * SOCKET_INITIALIZER                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Socket_Initializer(void)
{
#ifdef _WIN32
  WORD versReqstd = MAKEWORD( 2, 2);		// Current Winsock 2 DLL's
  WSADATA wsaData;
  int err;
  int optionValue = SO_SYNCHRONOUS_NONALERT;
#endif

#ifdef SUPPORT_AF_UNIX
  atom_AF_UNIX = Pl_Create_Atom("AF_UNIX");
#endif
  atom_AF_INET = Pl_Create_Atom("AF_INET");

#ifdef _WIN32
  if ((err = WSAStartup(versReqstd, &wsaData)) != 0 ||
      wsaData.wVersion != versReqstd)
    {
      Pl_Stream_Printf(pl_stm_tbl[pl_stm_top_level_output],
		    "warning: cannot find a usable WinSock DLL\n");
      if (err == 0)
	WSACleanup();
    }
  /* Allow Windows sockets to act as filehandles */
  if (setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
		 (char *)&optionValue,
		 sizeof(optionValue)) == SOCKET_ERROR)
    Pl_Os_Error(-1);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_SOCKET_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Socket_2(WamWord domain_word, WamWord socket_word)
{
  int domain;
#ifdef _WIN32
  SOCKET sock;
  int proto = IPPROTO_TCP;

#else
  int sock;
  int proto = 0;
#endif

  domain = Pl_Rd_Atom_Check(domain_word);
  if (
#ifdef SUPPORT_AF_UNIX
      domain != atom_AF_UNIX &&
#endif
      domain != atom_AF_INET)
    Pl_Err_Domain(pl_domain_socket_domain, domain_word);

  Pl_Check_For_Un_Variable(socket_word);

#ifdef SUPPORT_AF_UNIX
  if (domain == atom_AF_UNIX)
    sock = socket(AF_UNIX, SOCK_STREAM, proto);
  else
#endif
    sock = socket(AF_INET, SOCK_STREAM, proto);

#ifdef _WIN32
  Os_Test_Error(sock); /* NB: on error returns INVALID_SOCKET == -1 */
  /*
   * Windows (by default) causes sockets to be inherited
   * by child processes.  Turn this off.
   */
  SetHandleInformation((HANDLE) sock, HANDLE_FLAG_INHERIT, 0);
#else
  Os_Test_Error(sock);
#endif

  return Pl_Get_Integer(sock, socket_word);
}




/*-------------------------------------------------------------------------*
 * PL_SOCKET_CLOSE_1                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Socket_Close_1(WamWord socket_word)
{
#ifndef _WIN32
  int sock;
#else
  SOCKET sock;
#endif

  sock = Pl_Rd_Integer_Check(socket_word);
  if (sock < 2)
    {
      errno = EBADF;
      Os_Test_Error(-1);
    }
  else
#ifndef _WIN32
    Os_Test_Error(close(sock));
#else
    Os_Test_Error(closesocket(sock));
#endif

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_SOCKET_BIND_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Socket_Bind_2(WamWord socket_word, WamWord address_word)
{
  WamWord word, tag_mask;
  WamWord *stc_adr;
  int dom;
  int sock;
  int port;
  socklen_t l;

#ifdef SUPPORT_AF_UNIX
  char *path_name;
  struct sockaddr_un adr_un;
#endif
  struct sockaddr_in adr_in;
  static int atom_host_name = -1; /* not created in an init since */
				  /* establishes a connection */

  sock = Pl_Rd_Integer_Check(socket_word);


  DEREF(address_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_STC_MASK)
    {
    err_domain:
      Pl_Err_Domain(pl_domain_socket_address, word);
    }

  stc_adr = UnTag_STC(word);

#ifdef SUPPORT_AF_UNIX
  if (Functor_Arity(atom_AF_UNIX, 1) == Functor_And_Arity(stc_adr))
    dom = AF_UNIX;
  else
#endif
    if (Functor_Arity(atom_AF_INET, 2) == Functor_And_Arity(stc_adr))
      dom = AF_INET;
    else
      goto err_domain;

#ifdef SUPPORT_AF_UNIX
  if (dom == AF_UNIX)
    {
      path_name = Pl_Rd_String_Check(Arg(stc_adr, 0));
      if ((path_name = Pl_M_Absolute_Path_Name(path_name)) == NULL)
	Pl_Err_Domain(pl_domain_os_path, Arg(stc_adr, 0));

      adr_un.sun_family = AF_UNIX;
      strcpy(adr_un.sun_path, path_name);
      unlink(path_name);
      Os_Test_Error(bind(sock, (struct sockaddr *) &adr_un, sizeof(adr_un)));
      return TRUE;
    }
#endif
  /* case AF_INET */

  DEREF(Arg(stc_adr, 0), word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
      if (atom_host_name < 0)
	atom_host_name = Pl_Create_Allocate_Atom(Pl_M_Host_Name_From_Name(NULL));

      Pl_Get_Atom(atom_host_name, word);
    }
  else
    Pl_Rd_Atom_Check(word);	/* only to test the type */

  port = 0;
  DEREF(Arg(stc_adr, 1), word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    port = Pl_Rd_Integer_Check(word);

  adr_in.sin_port = htons((unsigned short) port);
  adr_in.sin_family = AF_INET;
  adr_in.sin_addr.s_addr = INADDR_ANY;

  Os_Test_Error(bind(sock, (struct sockaddr *) &adr_in, sizeof(adr_in)));
  if (tag_mask == TAG_INT_MASK)
    return TRUE;

  l = sizeof(adr_in);
  Os_Test_Error(getsockname(sock, (struct sockaddr *) &adr_in, &l));

  port = ntohs(adr_in.sin_port);

  return Pl_Get_Integer(port, word);
}




/*-------------------------------------------------------------------------*
 * PL_SOCKET_CONNECT_4                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Socket_Connect_4(WamWord socket_word, WamWord address_word,
		 WamWord stm_in_word, WamWord stm_out_word)
{
  WamWord word, tag_mask;
  WamWord *stc_adr;
  int dom;
  int sock;
  int port;
  char *host_name;

#ifdef SUPPORT_AF_UNIX
  char *path_name;
  struct sockaddr_un adr_un;
#endif
  struct sockaddr_in adr_in;
  struct hostent *host_entry;
  int stm_in, stm_out;
  char stream_name[256];

  sock = Pl_Rd_Integer_Check(socket_word);

  DEREF(address_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_STC_MASK)
    {
    err_domain:
      Pl_Err_Domain(pl_domain_socket_address, word);
    }

  stc_adr = UnTag_STC(word);

#ifdef SUPPORT_AF_UNIX
  if (Functor_Arity(atom_AF_UNIX, 1) == Functor_And_Arity(stc_adr))
    dom = AF_UNIX;
  else
#endif
  if (Functor_Arity(atom_AF_INET, 2) == Functor_And_Arity(stc_adr))
    dom = AF_INET;
  else
    goto err_domain;

#ifdef SUPPORT_AF_UNIX
  if (dom == AF_UNIX)
    {
      path_name = Pl_Rd_String_Check(Arg(stc_adr, 0));
      if ((path_name = Pl_M_Absolute_Path_Name(path_name)) == NULL)
	Pl_Err_Domain(pl_domain_os_path, Arg(stc_adr, 0));

      adr_un.sun_family = AF_UNIX;
      strcpy(adr_un.sun_path, path_name);
      Os_Test_Error(connect
		    (sock, (struct sockaddr *) &adr_un, sizeof(adr_un)));
      sprintf(stream_name, "socket_stream(connect('AF_UNIX'('%s')),%d)",
	      path_name, sock);
#ifdef _WIN32
	  /* Check for in-progress connection */
	  Os_Test_Error( send(sock, "", 0, 0) );
#endif
      goto create_streams;
    }
#endif
  /* case AF_INET */
  host_name = Pl_Rd_String_Check(Arg(stc_adr, 0));
  port = Pl_Rd_Integer_Check(Arg(stc_adr, 1));

  host_entry = gethostbyname(host_name);
  if (host_entry == NULL)
    return FALSE;

  adr_in.sin_family = AF_INET;
  adr_in.sin_port = htons((unsigned short) port);
  memcpy(&adr_in.sin_addr, host_entry->h_addr_list[0],
	 host_entry->h_length);

  Os_Test_Error(connect(sock, (struct sockaddr *) &adr_in, sizeof(adr_in)));
  sprintf(stream_name, "socket_stream(connect('AF_INET'('%s',%d)),%d)",
	  host_name, port, sock);

#ifdef SUPPORT_AF_UNIX
create_streams:
#endif
  if (!Create_Socket_Streams(sock, stream_name, &stm_in, &stm_out))
    return FALSE;

  Pl_Get_Integer(stm_in, stm_in_word);
  Pl_Get_Integer(stm_out, stm_out_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_SOCKET_LISTEN_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Socket_Listen_2(WamWord socket_word, WamWord length_word)
{
  int sock;
  int length;

  sock = Pl_Rd_Integer_Check(socket_word);
  length = Pl_Rd_Integer_Check(length_word);

  Os_Test_Error(listen(sock, length));
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_SOCKET_ACCEPT_4                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Socket_Accept_4(WamWord socket_word, WamWord client_word,
		WamWord stm_in_word, WamWord stm_out_word)
{
  int sock, cli_sock;
  socklen_t l;
  struct sockaddr_in adr_in;
  int stm_in, stm_out;
  char *cli_ip_adr = "AF_UNIX";
  char stream_name[256];


  l = sizeof(adr_in);
  sock = Pl_Rd_Integer_Check(socket_word);

  cli_sock = accept(sock, (struct sockaddr *) &adr_in, &l);

  Os_Test_Error(cli_sock);

  if (adr_in.sin_family == AF_INET)
    {
      cli_ip_adr = inet_ntoa(adr_in.sin_addr);
      if (cli_ip_adr == NULL)
	return FALSE;
      Pl_Get_Atom(Pl_Create_Allocate_Atom(cli_ip_adr), client_word);
    }

  sprintf(stream_name, "socket_stream(accept('%s'),%d)", cli_ip_adr,
	  cli_sock);

  if (!Create_Socket_Streams(cli_sock, stream_name, &stm_in, &stm_out))
    return FALSE;

  Pl_Get_Integer(stm_in, stm_in_word);
  Pl_Get_Integer(stm_out, stm_out_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_ASSOC_SOCKET_STREAMS_3                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Assoc_Socket_Streams_3(WamWord socket_word,
		       WamWord stm_in_word, WamWord stm_out_word)
{
  int stm_in, stm_out;
  char stream_name[256];

  int sock = Pl_Rd_Integer_Check(socket_word);

  sprintf(stream_name, "socket_stream(assoc(%d))", sock);
  if (!Create_Socket_Streams(sock, stream_name, &stm_in, &stm_out))
    return FALSE;

  Pl_Get_Integer(stm_in, stm_in_word);
  Pl_Get_Integer(stm_out, stm_out_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * CREATE_SOCKET_STREAMS                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Create_Socket_Streams(int sock, char *stream_name,
		      int *stm_in, int *stm_out)
{
  int fd;
  FILE *f_in, *f_out;
  int atom;
  int stm;

#ifdef _WIN32
  int r;

  Os_Test_Error((fd = _open_osfhandle(sock, _O_BINARY | _O_RDWR | _O_BINARY)));
  Os_Test_Error((r = dup(fd)));
  Os_Test_Error_Null((f_out = fdopen(fd, "w")));
  Os_Test_Error_Null((f_in = fdopen(r, "r")));
#else
  Os_Test_Error((fd = dup(sock)));
  Os_Test_Error_Null((f_in = fdopen(sock, "rt")));
  Os_Test_Error_Null((f_out = fdopen(fd, "wt")));
#endif

  atom = Pl_Create_Allocate_Atom(stream_name);

  stm = Pl_Add_Stream_For_Stdio_Desc(f_in, atom, STREAM_MODE_READ, TRUE);
  pl_stm_tbl[stm]->prop.eof_action = STREAM_EOF_ACTION_RESET;
  pl_stm_tbl[stm]->prop.other = 4;

  *stm_in = stm;

  stm = Pl_Add_Stream_For_Stdio_Desc(f_out, atom, STREAM_MODE_WRITE, TRUE);
  pl_stm_tbl[stm]->prop.other = 4;

  *stm_out = stm;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_HOSTNAME_ADDRESS_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Hostname_Address_2(WamWord host_name_word, WamWord host_address_word)
{
  WamWord word, tag_mask;
  char *host_name;
  char *host_address;
  struct hostent *host_entry;
  struct in_addr iadr;

  DEREF(host_name_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
      host_address = Pl_Rd_String_Check(host_address_word);
      host_name = Pl_M_Host_Name_From_Adr(host_address);
      return host_name && Pl_Un_String_Check(host_name, host_name_word);
    }

  host_name = Pl_Rd_String_Check(word);

  Pl_Check_For_Un_Atom(host_address_word);

  host_entry = gethostbyname(host_name);
  if (host_entry == NULL)
    return FALSE;

  memcpy(&iadr.s_addr, host_entry->h_addr_list[0], host_entry->h_length);
  host_address = inet_ntoa(iadr);

  return Pl_Un_String_Check(host_address, host_address_word);
}
