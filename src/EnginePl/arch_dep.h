/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : configuration                                                   *
 * File  : arch_dep.h                                                      *
 * Descr.: architecture dependent features - Header file                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2005 Daniel Diaz                                     *
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

#ifndef _ARCH_DEP_H
#define _ARCH_DEP_H

#if defined(_WIN32) && !defined(__CYGWIN__)

#define MAXPATHLEN                 1024
#define SIGQUIT                    SIGTERM
#define fdopen                     _fdopen
#define dup                        _dup
#define dup2                       _dup2
#define getcwd                     _getcwd
#define chdir                      _chdir
#define close                      _close
#define pclose                     _pclose
#define popen                      _popen
#define pclose                     _pclose
#define getpid                     _getpid
#define strcasecmp                 stricmp
#define strncasecmp                strnicmp
#define unlink                     _unlink
#define tzset                      _tzset
#define access                     _access

#ifndef F_OK
#define F_OK                       00
#define W_OK                       02
#define R_OK                       04
#define X_OK                       F_OK
#endif

#ifndef S_ISDIR
#define	S_ISDIR(m)	           (((m)&_S_IFMT) == _S_IFDIR)
#define	S_ISCHR(m)                 (((m)&_S_IFMT) == _S_IFCHR)
#define	S_ISFIFO(m)	           (((m)&_S_IFMT) == _S_IFIFO)
#define	S_ISREG(m)	           (((m)&_S_IFMT) == _S_IFREG)
#endif

#ifndef S_IRUSR
#define S_IRUSR                    _S_IREAD
#define S_IWUSR                    _S_IWRITE
#define S_IXUSR                    _S_IEXEC
#endif

#define DIR_SEP_S                  "\\"
#define DIR_SEP_C                  '\\'

#else

#define DIR_SEP_S                  "/"
#define DIR_SEP_C                  '/'

#endif /* defined(_WIN32) && !defined(__CYGWIN__) */

#if defined(M_ix86_cygwin) || defined(M_ix86_sco)
#define Set_Line_Buf(s)            setvbuf(s, NULL, _IOLBF, 0)
#elif defined(_WIN32)
#define Set_Line_Buf(s)            setbuf(s, NULL)
#else
#define Set_Line_Buf(s)            setlinebuf(s)
#endif


#ifndef NO_USE_GUI_CONSOLE
#define W32_GUI_CONSOLE
#endif

#ifdef M_sparc_sunos
#define __USE_FIXED_PROTOTYPES__
#endif


#if defined(M_ix86_sco)

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#endif



#if !defined(_WIN32) && !defined(__unix__)
#define __unix__
#endif

#ifndef HAVE_FGETC
#define fgetc getc
#endif

				/* Fast call macros */
#if defined(M_ix86)

/* FC_MAX_ARGS_IN_REGS can be decreased (0, 1, 2) - but the inline_asm_data
 * is compiled with 3, if changed, change inlined code in mapper */

#define COULD_COMPILE_FOR_FC
#ifdef __GNUC__
#define FC_MAX_ARGS_IN_REGS 3	
#define FC_SET_OF_REGISTERS { "%eax", "%edx", "%ecx" };
#define FC_ATTRIB __attribute__((regparm(FC_MAX_ARGS_IN_REGS)))
#else  /* MSVC++ ? */
/* TO DO */
#endif

#endif


#if !defined(NO_USE_FAST_CALL) && defined(FC_ATTRIB)
#define FC_USED_TO_COMPILE_CORE
#define FC FC_ATTRIB
#else
#define FC
#endif


/* Win32 SEH macros */

#if defined(_WIN32) || defined(__CYGWIN__)
				/* from MSVC++ windows.h + renaming */
typedef enum {
    ExceptContinueExecution,
    ExceptContinueSearch,
    ExceptNestedException,
    ExceptCollidedUnwind
} EXCEPT_DISPOSITION;

typedef struct _excp_lst
{
  struct _excp_lst *chain;
  EXCEPT_DISPOSITION (*handler)();
} excp_lst;
  

#ifdef __GNUC__

#  define SEH_PUSH(new_handler)			\
{						\
  excp_lst e;					\
  EXCEPT_DISPOSITION new_handler();		\
  e.handler = new_handler;			\
  asm("movl %%fs:0,%0" : "=r" (e.chain));	\
  asm("movl %0,%%fs:0" : : "r" (&e));

 
#  define SEH_POP				\
  asm("movl %0,%%fs:0" : : "r" (e.chain));	\
}

#elif defined(_MSC_VER)

#  define SEH_PUSH(new_handler)			\
{						\
  excp_lst e;					\
  EXCEPT_DISPOSITION new_handler();		\
  e.handler = new_handler;			\
  __asm push eax				\
  __asm mov eax,dword ptr fs:[0]		\
  __asm mov dword ptr [e.chain],eax		\
  __asm lea eax,[e]				\
  __asm mov dword ptr fs:[0],eax		\
  __asm pop eax

#  define SEH_POP				\
  __asm push eax				\
  __asm mov eax,dword ptr [e.chain]		\
  __asm mov dword ptr fs:[0],eax		\
  __asm pop eax					\
}

#elif defined(__LCC__)
 /* below in movl %eax,%e and movel %e,%eax %e should be %e.chain the lcc asm 
    does not support it. Here %e works since chain is the 1st field */
#  define SEH_PUSH(new_handler)			\
{						\
  excp_lst e;					\
  EXCEPT_DISPOSITION new_handler();		\
  e.handler = new_handler;			\
  _asm("pushl %eax");				\
  _asm("movl %fs:0,%eax");			\
  _asm("movl %eax,%e");				\
  _asm("leal %e,%eax");				\
  _asm("movl %eax,%fs:0");			\
  _asm("popl %eax");

#  define SEH_POP				\
  _asm("pushl %eax");				\
  _asm("movl %e,%eax");				\
  _asm("movl %eax,%fs:0");			\
  _asm("popl %eax");				\
}

#else

#  error macros SEH_PUSH/POP undefined for this compiler

#endif

#endif /* defined(_WIN32) || defined(__CYGWIN__) */

#endif /* !_ARCH_DEP_H */

