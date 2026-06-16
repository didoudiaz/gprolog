/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pl_setjmp.h                                                     *
 * Descr.: setjmp.h replacement for gprolog runtime - header file          *
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

#ifndef _PL_SETJMP_H
#define _PL_SETJMP_H

#include <setjmp.h>

/*
 * Workaround for setjmp/longjmp on Win64 (x86_64 and arm64).
 *
 * The default setjmp/longjmp performs Windows stack unwinding as part of
 * C++/SEH (executing the cleanup logic). This crashes gprolog.
 * To bypass stack unwinding, we redefine the behavior of setjmp/longjmp by
 * redefining them as macros and/or by providing an alternative assembly
 * implementation (see trampoline.S).
 *
 * IMPORTANT: do not include <setjmp.h> after this block, even if it is
 * already included here. With MinGW-w64, <setjmp.h> can redefine setjmp
 * in a way that overrides our definitions (see /mingw64/include/setjmp.h
 * and the _INC_SETJMP macro).
 */

#ifdef _WIN64

/*
 * On windows 64 bits (_WIN64), setjmp/longjmp performs a stack unwinding
 * which crashes gprolog.
 * We define replacement functions: pl_stejmp/pl_longjmp in trampoline.S
 *
 * Other alternatives (less robust):
 *
 * On x86_64: setjmp is implemented by _setjmp which accepts a second parameter.
 * If it is NULL, longjmp will not do stack unwinding.
 * By default the the second argument is rsp (frame), in which case
 * longjmp performs a stack unwinding which crashes gprolog.
 * We can redefine setjmp to pass NULL:
 * see https://github.com/qemu/qemu/blob/master/include/system/os-win32.h
 *
 * On arm64, setjmp is available in only one variant, and longjmp always does
 * stack unwinding which crashes gprolog.
 * https://github.com/qemu/qemu/blob/master/include/system/os-win32.h
 * On mingw, it is possible to use another implementation of setjmp
 * (not windows one), coming from mingw, which never performs stack unwinding.
 */

#ifdef setjmp
#undef setjmp
#endif

#ifdef longjmp
#undef longjmp
#endif

#define setjmp(env) pl_setjmp(env)
#define longjmp(env, val) pl_longjmp(env, val)

int pl_setjmp(jmp_buf env);
void ATTR_NORETURN pl_longjmp(jmp_buf env, int val);

#endif	/* _WIN64 */

#ifndef HAVE_SIGSETJMP
#define sigjmp_buf jmp_buf
#define sigsetjmp(jb, x) setjmp(jb)
#define siglongjmp longjmp
#endif

#endif	/* _PL_SETJMP_H */
