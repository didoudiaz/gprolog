/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : configuration                                                   *
 * File  : arch_dep.h                                                      *
 * Descr.: architecture dependent features - header file                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2026 Daniel Diaz                                     *
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


/* MSVC general defines */
#define _USE_MATH_DEFINES
#define _CRT_SECURE_NO_WARNINGS 1
#define _CRT_NONSTDC_NO_WARNINGS 1


/* C Preprocessor utilities */

#define PP_STR1(s) #s
#define PP_STR(s) PP_STR1(s)

#define PP_CAT1(x, y)   x ## y
#define PP_CAT(x, y)    PP_CAT1(x, y)




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


#define DBGPRINTF printf



/* check printf arguments */

#if defined(__GNUC__)
#define ATTR_PRINTF(x) __attribute__((format(printf, x, x + 1)))
#pragma GCC diagnostic ignored "-Wformat-zero-length"
#else
#define ATTR_PRINTF(x)
#endif

#define AVOID_UNUSED_WARNING(x) ((void)(x))


/* Alignment compiler attribute
 * in MSVC the attribute must be before the variable,
 * in GCC is seems OK before or after the variable.
 */
#ifdef __GNUC__			/* GCC accepts sizeof(type) */
#   define ATTR_ALIGN(x) __attribute__((aligned(x)))
#elif defined(_MSC_VER)		/* MSVC needs a constant */
#  define ATTR_ALIGN(x)  __declspec(align(x))
#else
#  define ATTR_ALIGN(x)
#endif

/* noreturn compiler attribute */
#ifdef __GNUC__
#   define ATTR_NORETURN __attribute__((noreturn))
#elif defined(_MSC_VER)
#   define ATTR_NORETURN __declspec(noreturn)
#else
#   define ATTR_NORETURN
#endif



#if defined(_WIN32) && !defined(__CYGWIN__)

/* There are 2 kinds of MSVC warning C4996 one wants to remove:
 * 1) XXX was declared deprecated ... This function or variable may be unsafe
 *    solution: #define _CRT_SECURE_NO_WARNINGS 1
 * 2) The POSIX name for this item is deprecated
 *    solution: #define _CRT_NONSTDC_NO_WARNINGS 1
 * However, these defines only work if they are before any #include <...>
 * So: pass to cl: -D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_WARNINGS
 * or deactivate the warning with the following pragma. We do both !
 */

#ifdef _MSC_VER
#pragma warning(disable : 4996)
/*#pragma warning(disable : 4244)*/ /* possible loss of data */
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

#ifndef HAVE_ISNAN
#define isnan                      _isnan
#endif	/* !HAVE_ISNAN */

#endif	/* _MSC_VER */

#ifndef F_OK
#define F_OK                       0
#define X_OK                       1  /* NB: see Pl_Access */
#define W_OK                       2
#define R_OK                       4
#endif	/* !F_OK */

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


#if defined(__CYGWIN__) || defined(M_ix86_sco)
#define Set_Line_Buf(s)            setvbuf(s, NULL, _IOLBF, 0)
#elif defined(_WIN32)
#define Set_Line_Buf(s)            setbuf(s, NULL)
#else
#define Set_Line_Buf(s)            setlinebuf(s)
#endif


#ifndef NO_USE_GUI_CONSOLE
#define W32_GUI_CONSOLE
#endif




#if !defined(_WIN32) && !defined(__unix__) /* maybe too radical */
#define __unix__
#endif

#ifndef HAVE_FGETC
#define fgetc getc
#endif




				/* Fast call macros */
#if defined(M_ix86)

/* FC_MAX_ARGS_IN_REGS can be decreased (0, 1, 2) */

#define COULD_COMPILE_FOR_FC
#ifdef __GNUC__
#define FC_MAX_ARGS_IN_REGS 3
#define FC_SET_OF_REGISTERS { "%eax", "%edx", "%ecx" };
#define FC_ATTRIB __attribute__((regparm(FC_MAX_ARGS_IN_REGS)))
#elif defined(_MSC_VER)
	/* under MSVC++ we can use __fastcall convention */
        /* however maybe little gain (see file ix86_any.c) */
#define FC_MAX_ARGS_IN_REGS 2
#define FC_SET_OF_REGISTERS { "%ecx", "%edx" };
#define FC_ATTRIB __fastcall
#else
#define FC_MAX_ARGS_IN_REGS 0
#define FC_SET_OF_REGISTERS { NULL };
#define FC_ATTRIB
#endif /* !_MSC_VER */

#endif /* M_ix86 */

#if defined(USE_FAST_CALL) && defined(FC_ATTRIB)
#define FC_USED_TO_COMPILE_CORE
#ifndef FC /* to compile Ma2Asm/check.c without FC */
#define FC FC_ATTRIB
#endif
#else
#define FC
#endif

#if defined(__OpenBSD__) || defined(M_bsd)
#define USE_DL_MALLOC
#endif


/* sunos, SCO are very very old, remove the following ? */

#ifdef M_alpha_osf
#define _XOPEN_SOURCE 700
#endif

#ifdef M_sparc32_sunos
#define __USE_FIXED_PROTOTYPES__
#endif

#ifdef M_ix86_sco
#define _XOPEN_SOURCE 700
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
#endif

#endif /* !_ARCH_DEP_H */
