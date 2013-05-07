/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : configuration                                                   *
 * File  : arch_dep.h                                                      *
 * Descr.: architecture dependent features - Header file                   *
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


#ifndef _ARCH_DEP_H
#define _ARCH_DEP_H

#define CPP_STR1(s) #s
#define CPP_STR(s) CPP_STR1(s)

#define CPP_CAT1(x, y)   x ## y
#define CPP_CAT(x, y)    CPP_CAT1(x, y)


/* C compiler version (for more general handling see http://sourceforge.net/projects/predef) */

#if defined(__clang__)		/* put before because also defines __GNUC__ */

#define CC_MAJOR       __clang_major__
#define CC_MINOR       __clang_minor__
#define CC_PATCHLEVEL  __clang_patchlevel__

#elif defined(__GNUC__)

#define CC_MAJOR       __GNUC__
#define CC_MINOR       __GNUC_MINOR__
#define CC_PATCHLEVEL  __GNUC_PATCHLEVEL__

#elif defined(_MSC_FULL_VER)

#define CC_MAJOR       (_MSC_FULL_VER / 1000000)
#define CC_MINOR       (_MSC_FULL_VER % 1000000 / 10000)
#define CC_PATCHLEVEL  (_MSC_FULL_VER % 10000)

#elif defined(_MSC_VER)

#define CC_MAJOR       (_MSC_VER / 100)
#define CC_MINOR       (_MSC_VER % 100)
#define CC_PATCHLEVEL  0

#else

#define CC_MAJOR       0
#define CC_MINOR       0
#define CC_PATCHLEVEL  0

#endif



/* Compile date */

#if defined(__DATE__) && defined(__TIME__)
#define COMPILED_AT __DATE__ ", " __TIME__
#else
#define COMPILED_AT "unknown date"
#endif




#if defined(_WIN32) && !defined(__CYGWIN__)

/* There are 2 kinds of MSVC warning C4996 one wants to remove:
 * 1) XXX was declared deprecated ... This function or variable may be unsafe
 *    solution: #define _CRT_SECURE_NO_DEPRECATE 1
 * 2) The POSIX name for this item is deprecated
 *    solution: #define _CRT_NONSTDC_NO_DEPRECATE 1
 * However, these defines only work if they are before any #include <...>
 * So: pass to cl: -D_CRT_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_DEPRECATE
 * or deactivate the warning with the following pragma. We do both !
 */

#ifdef _MSC_VER
#pragma warning(disable : 4996)
#endif

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
#define tempnam                    _tempnam
#define unlink                     _unlink
#define tzset                      _tzset
#define access                     _access
#ifdef _MSC_VER
#define strcasecmp                 stricmp
#define strncasecmp                strnicmp
#define spawnvp                    _spawnvp
#endif

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
#define DIR_SEP_C_ALT              '/'

#elif defined(__CYGWIN__)

#define DIR_SEP_S                  "/"
#define DIR_SEP_C                  '/'
#define DIR_SEP_C_ALT              '\\'

#else  /* Unix */

#define DIR_SEP_S                  "/"
#define DIR_SEP_C                  '/'
#define DIR_SEP_C_ALT              '/'

#endif

#define Is_Dir_Sep(c)              ((c) == DIR_SEP_C || (c) == DIR_SEP_C_ALT)



#define Find_Last_Dir_Sep(_p, _path)			\
  do {							\
    char *_ptr;						\
							\
    for((_p) = NULL, _ptr = (_path); *_ptr; _ptr++)	\
      if (Is_Dir_Sep(*_ptr))				\
	(_p) = _ptr;					\
  } while(0)



#define Has_Drive_Specif(str) \
  (((*(str) >= 'a' && *(str) <= 'z') || (*(str) >= 'A' && *(str) <= 'Z')) && (str)[1] == ':')



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


#ifndef HAVE_SIGSETJMP
#define sigjmp_buf jmp_buf
#define sigsetjmp(jb, x) setjmp(jb)
#define siglongjmp longjmp
#endif

#if defined(_WIN64) && !defined(_MSC_VER)
/* Mingw64-gcc implements setjmp with msvcrt's _setjmp. This _setjmp
 * has an additional (hidden) argument. If it is NULL, longjmp will NOT do
 * stack unwinding (needed for SEH). By default the the second argument is
 * NOT null (it is $rsp), then longjmp will try a stack unwinding which will
 * crash gprolog.
 * NB: _setjmp stores this argument in the jmp_buf (in the first bytes) 
 * Mingw-gcc v < 4.6 fixed this at longjmp (before calling msvcrt's _longjmp)
 * (see file: lib64_libmingwex_a-mingw_getsp.o in library libmingwex.a)
 * 
 * 0000000000000006 <longjmp>:                              # x86_64 ABI: jmp_buf is in $rcx
 *    6:   31 c0                   xor    %eax,%eax
 *    8:   89 01                   mov    %eax,(%rcx)       # set 0 in the first word of jmp_buf
 *    a:   48 8d 05 00 00 00 00    lea    0x0(%rip),%rax    # this will call dll msvcrt's longjmp
 *   11:   ff 20                   jmpq   *(%rax)
 *
 * while in >= 4.6: (no more fixes)
 *
 * 0000000000000006 <longjmp>:
 *    6:   48 8d 05 00 00 00 00    lea    0x0(%rip),%rax    # this will call dll msvcrt's longjmp
 *    d:   ff 20                   jmpq   *(%rax)
 *    f:   90                      nop
 *
 */
#ifdef setjmp
#undef setjmp
#endif
#define setjmp(buf) _setjmp(buf, NULL)
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
#elif 0  /* under MSVC++ we can use __fastcall convention (#elif 1 if wanted) */
         /* see file ix86_any.c to see why it is not selected by default */
#define FC_MAX_ARGS_IN_REGS 2
#define FC_SET_OF_REGISTERS { "%ecx", "%edx" };
#define FC_ATTRIB __fastcall
#else
#define FC_MAX_ARGS_IN_REGS 0
#define FC_SET_OF_REGISTERS { NULL };
#define FC_ATTRIB
#endif

#endif

#if !defined(NO_USE_FAST_CALL) && defined(FC_ATTRIB)
#define FC_USED_TO_COMPILE_CORE
#ifndef FC /* to compile Ma2Asm/check.c without FC */
#define FC FC_ATTRIB
#endif
#else
#define FC
#endif


/* Win32 SEH macros */

#if defined(_WIN32) && !defined(_WIN64) || defined(__CYGWIN__)
#define USE_SEH
#endif

#if defined(USE_SEH)
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

#  pragma warning(disable:4733) /* we know what we are doing with SEH */

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

#endif /* defined(USE_SEH) */

#endif /* !_ARCH_DEP_H */

