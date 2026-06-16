/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Tools                                                           *
 * File  : ctor.h                                                          *
 * Descr.: portable startup constructors (INITIALIZER macro)               *
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

/*
 * INITIALIZER(f) to define initializer functions executed before the main():
 *
 * - GCC/clang (ELF/Mach-O): uses __attribute__((constructor))
 * - MSVC (COFF/PE): places a function pointer in .CRT$XCU.
 *   NB: do not use /OPT:REF (remove unused symbols). To use this option
 *   force he linker to keep the symbole with a linker /include directive.
 *   Something like: __pragma(comment(linker, "/include:" #f "_"))
 *   (to be checked: under 32bits maybe an underscore is needed before f)
 *   But the symbol needs to be global (remove the static keyword).
 *   (thus all initializer function names f must be different).
 *
 * Usage: INITIALIZER(my_initializer) { ...code... }
 *
 * The macro expands to a static function called during startup, before main().
 *
 * Priorities (possible future extension ideas)
 * - GCC/clang: accept an optional integer priority and emit:
 *       __attribute__((constructor(PRIO)))
 *
 * - MSVC: emulate by choosing a different subsection name in the .CRT$XC
 *   range (lexicographic order), e.g. .CRT$XCU$000", ".CRT$XCU$100", etc.
 */

#ifndef _CTOR_H
#define _CTOR_H

#if defined(_MSC_VER)

#pragma section(".CRT$XCU", read)

#define INITIALIZER(f)					\
  static void f(void);					\
  __declspec(allocate(".CRT$XCU"))			\
  static void (* const f##_)(void) = f; 		\
  static void f(void)

#else /* GCC/clang */

#define INITIALIZER(f)					\
  static void f(void) __attribute__((constructor));	\
  static void f(void)

#endif

#endif	/* !_CTOR_H */
