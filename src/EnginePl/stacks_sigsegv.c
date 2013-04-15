/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : stacks_sigsegv.c                                                *
 * Descr.: stack hardware overflow detection (SIGSEGV)                     *
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


/* copy this from try_sigaction.c */

#if defined(M_ix86_sco)
#define _XOPEN_SOURCE 700
#define _XOPEN_SOURCE_EXTENDED
#endif

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "gp_config.h"          /* ensure __unix__ defined if not Win32 */

#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#elif defined(_WIN32)
#include <windows.h>
#endif

#include "engine_pl.h"

 /* see configure.in */
#if !defined(HAVE_WORKING_SIGACTION) && defined(LINUX_NEEDS_ASM_SIGCONTEXT)
#include <asm/sigcontext.h>
#endif

#ifdef HAVE_SYS_SIGINFO_H
#include <sys/siginfo.h>
#endif

#if defined(HAVE_MMAP) && !defined(_WIN32)
#include <sys/mman.h>

#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
#define MAP_ANON MAP_ANONYMOUS
#endif
#endif



#if 0
#define DEBUG
#endif



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define M_MAGIC1                   0x12345678
#define M_MAGIC2                   0xdeadbeef


#define MAX_SIGSEGV_HANDLER        10


          /* Error Messages */

#define ERR_STACKS_ALLOCATION      "Memory allocation fault"

#define ERR_CANNOT_OPEN_DEV0       "Cannot open /dev/zero : %s"
#define ERR_CANNOT_UNMAP           "unmap failed : %s"

#define ERR_CANNOT_FREE            "VirtualFree failed : %" PL_FMT_u
#define ERR_CANNOT_PROTECT         "VirtualProtect failed : %" PL_FMT_u

#define ERR_STACK_OVERFLOW_ENV     "%s stack overflow (size: %d Kb, reached: %d Kb, environment variable used: %s)"

#define ERR_STACK_OVERFLOW_NO_ENV  "%s stack overflow (size: %d Kb, reached: %d Kb - fixed size)"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int page_size;

static SegvHdlr tbl_handler[MAX_SIGSEGV_HANDLER];
static int nb_handler = 0;


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Install_SIGSEGV_Handler(void);

static void SIGSEGV_Handler();

static void Handle_Bad_Address(void *bad_addr);

static int Default_SIGSEGV_Handler(void *bad_addr);

static char *Stack_Overflow_Err_Msg(int stk_nb);



#define Round_Up(x, y)          (((x) + (y) - 1) / (y) * (y))

#define Round_Down(x, y)        ((x) / (y) * (y))




#if defined(_WIN32)

/*-------------------------------------------------------------------------*
 * GETPAGESIZE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
getpagesize(void)
{
  SYSTEM_INFO si;

  GetSystemInfo(&si);
  return si.dwPageSize;
}

#endif


/*-------------------------------------------------------------------------*
 * VIRTUAL_MEM_ALLOC                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void *
Virtual_Mem_Alloc(void *addr, int length)
{
#if defined(_WIN32)

  addr = (void *) VirtualAlloc(addr, length,
			       MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);

#elif defined(HAVE_MMAP)

#ifndef MAP_ANON
  static int fd = -1;

  if (fd == -1)
    fd = open("/dev/zero", 0);

  if (fd == -1)
    Pl_Fatal_Error(ERR_CANNOT_OPEN_DEV0, Pl_M_Sys_Err_String(-1));
#endif /* !MAP_ANON */

  addr = (void *) mmap((void *) addr, length, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE
#ifdef MMAP_NEEDS_FIXED
                          | MAP_FIXED
#endif
#ifdef MAP_ANON
                          | MAP_ANON, -1,
#else
                          , fd,
#endif /* !MAP_ANON */
                          0);

  if (addr == (void *) -1)
    addr = NULL;

#else  /* !HAVE_MMAP */

  addr = (void *) Calloc(length, 1);

#endif	/* !HAVE_MMAP */

  return addr;
}



#if TAG_SIZE_HIGH > 0
/*-------------------------------------------------------------------------*
 * VIRTUAL_MEM_FREE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Virtual_Mem_Free(void *addr, int length)
{
#if defined(_WIN32)

  if (!VirtualFree(addr, 0, MEM_RELEASE))
    Pl_Fatal_Error(ERR_CANNOT_FREE, GetLastError());

#elif defined(HAVE_MMAP)

  if (munmap((void *) addr, length) == -1)
    Pl_Fatal_Error(ERR_CANNOT_UNMAP, Pl_M_Sys_Err_String(-1));

#else

  Free(addr);

#endif
}
#endif  /* TAG_SIZE_HIGH > 0 */



/*-------------------------------------------------------------------------*
 * VIRTUAL_MEM_PROTECT                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Virtual_Mem_Protect(void *addr, int length)
{
  WamWord *end = (WamWord *) addr;
#if defined(_WIN32)
  DWORD old_prot;

  if (!VirtualProtect(addr, length, PAGE_NOACCESS, &old_prot))
    Pl_Fatal_Error(ERR_CANNOT_PROTECT, GetLastError());

#elif defined(HAVE_MMAP)

#  ifdef HAVE_MPROTECT
  if (mprotect((void *) addr, length, PROT_NONE) == -1)
#  endif
    if (munmap((void *) addr, length) == -1)
      Pl_Fatal_Error(ERR_CANNOT_UNMAP, Pl_M_Sys_Err_String(-1));

#endif
  end[-16] = M_MAGIC1;
  end[-32] = M_MAGIC2;           /* and rest (end[-1,...]) should be 0 */
  end[-33] = 0;
}




/*-------------------------------------------------------------------------*
 * PL_ALLOCATE_STACKS                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Allocate_Stacks(void)
{
  unsigned length = 0, stk_sz;
  WamWord *addr;
  int i;
  WamWord *addr_to_try[] = {
#ifndef MMAP_NEEDS_FIXED
    NULL,
#endif
#ifdef M_MMAP_HIGH_ADR1
    (WamWord *) M_MMAP_HIGH_ADR1,
#endif
#ifdef M_MMAP_HIGH_ADR2
    (WamWord *) M_MMAP_HIGH_ADR2,
#endif
#ifdef M_MMAP_HIGH_ADR3
    (WamWord *) M_MMAP_HIGH_ADR3,
#endif
    (WamWord *) -1 };

  page_size = getpagesize() / sizeof(WamWord);

  for (i = 0; i < NB_OF_STACKS; i++)
    {
      stk_sz = pl_stk_tbl[i].size = Round_Up(pl_stk_tbl[i].size, page_size);
      if (stk_sz == 0)
	stk_sz = page_size;	/* at leat one page to write magic numbers */
      length += stk_sz + page_size;
    }
  length *= sizeof(WamWord);

  addr = NULL;
  for(i = 0; addr == NULL && addr_to_try[i] != (WamWord *) -1; i++)
    {
      addr = addr_to_try[i];
#ifdef DEBUG
      DBGPRINTF("trying at high addr: %p --> ", addr);
#endif
      if (addr)
        {
          addr = (WamWord *) Round_Down((PlULong) addr, getpagesize());
          addr = (WamWord *) ((PlULong) (addr) - length);
        }
#ifdef DEBUG
      DBGPRINTF("base: %p length: %d Kb\n", addr, length / 1024);
#endif
      addr = Virtual_Mem_Alloc(addr, length);

#ifdef DEBUG
      DBGPRINTF("obtaining: %p  (end: %p)\n", addr, (WamWord *) ((PlULong) addr + length));
#endif
#if TAG_SIZE_HIGH > 0
      if (addr && (((PlULong) (addr) + length) >> (WORD_SIZE - TAG_SIZE_HIGH)) != 0)
        {
#ifdef DEBUG
          DBGPRINTF("  -> invalid high bits addr\n");
#endif
          Virtual_Mem_Free(addr, length);
          addr = NULL;
        }
#endif /* TAG_SIZE_HIGH > 0 */
    }

  if (addr == NULL)
    Pl_Fatal_Error(ERR_STACKS_ALLOCATION);

  for (i = 0; i < NB_OF_STACKS; i++)
    {
      pl_stk_tbl[i].stack = addr;
      stk_sz = pl_stk_tbl[i].size;
      if (stk_sz == 0)
	stk_sz = page_size;	/* at least one page for magic numbers */
#ifdef DEBUG
      DBGPRINTF("  stack: %d %-10s length: %5ld Kb   addr:[%p..%p[ + 1 free page, next addr: %p\n", 
		i, pl_stk_tbl[i].name, stk_sz * sizeof(WamWord) / 1024, 
		addr, addr + stk_sz, addr + stk_sz + page_size);
#endif
      addr += stk_sz;
      Virtual_Mem_Protect(addr, page_size * sizeof(WamWord));
      addr += page_size;
    }

  Install_SIGSEGV_Handler();	/* install the real (and unique) SIGSEGV handler */
  Pl_Push_SIGSEGV_Handler(Default_SIGSEGV_Handler); /* install initial user SIGSEGV handler */
}




#if defined(__unix__) || defined(__CYGWIN__)|| defined(_WIN64)

/*-------------------------------------------------------------------------*
 * SIGSEGV_HANDLER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
#if defined(HAVE_WORKING_SIGACTION) || \
  defined(M_sparc_solaris) || defined(M_ix86_solaris) || defined(M_x86_64_solaris)

static void
SIGSEGV_Handler(int sig, siginfo_t *sip)
{
  void *addr = (void *) sip->si_addr;
  Handle_Bad_Address(addr);
}

#elif defined(M_sparc_sunos)

static void
SIGSEGV_Handler(int sig, int code, int scp, void *addr)
{
  Handle_Bad_Address(addr);
}

#elif defined(M_alpha_osf)

static void
SIGSEGV_Handler(int sig, int code, struct sigcontext *scp)
{
  void *addr = (void *) (scp->sc_traparg_a0);
  Handle_Bad_Address(addr);
}

#elif defined(M_alpha_linux)

static void
SIGSEGV_Handler(int sig, struct sigcontext scp)
{
  void *addr = (void *) (scp.sc_fp_trigger_inst); /* why this one? */
  /* void *addr=(void *) (scp.sc_traparg_a0); */
  Handle_Bad_Address(addr);
}

#elif defined(M_ix86_linux)

static void
SIGSEGV_Handler(int sig, struct sigcontext scp)
{
  void *addr = (void *) scp.cr2;
  Handle_Bad_Address(addr);
}

#elif defined(M_powerpc_linux)

static void
SIGSEGV_Handler(int sig, struct sigcontext scp)
{
  void *addr = (void *) scp.regs->dar;
  Handle_Bad_Address(addr);
}


#elif defined(M_ix86_sco)
#include <sys/siginfo.h>
static void
SIGSEGV_Handler(int sig, siginfo_t * si)
{
  void *addr = (void *) si->si_addr;
  Handle_Bad_Address(addr);
}

#elif defined(M_ix86_bsd) || defined(M_powerpc_bsd) || defined(M_sparc_bsd)
static void
SIGSEGV_Handler(int sig, int code, struct sigcontext *scp)
{
  void *addr = (void *) scp->sc_err;
  Handle_Bad_Address(addr);
}

#elif defined(M_x86_64_linux) || defined(M_x86_64_bsd) || defined(M_x86_64_darwin)

static void
SIGSEGV_Handler(int sig, siginfo_t *sip, void *scp)
{
  void *addr = (void *) sip->si_addr;
  Handle_Bad_Address(addr);
}

#elif defined(M_mips_irix)

static void
SIGSEGV_Handler(int sig, int code, struct sigcontext *scp)
{
  void *addr = scp->sc_regs[16];
  Handle_Bad_Address(addr);
}

#else

static void
SIGSEGV_Handler(int sig)	/* cannot detect fault addr */
{
#ifdef __GNUC__
#warning SIGSEGV_Handler does not know how to detect fault addr - use magic numbers
#endif

#define M_USE_MAGIC_NB_TO_DETECT_STACK_NAME

  Handle_Bad_Address(NULL);
}

#endif

#else  /* WINDOWS */

/*-------------------------------------------------------------------------*
 * WIN32_EXCEPTION_HANDLER                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static LONG WINAPI 
Win32_Exception_Handler(LPEXCEPTION_POINTERS ei)
{
  void *addr;
  switch(ei->ExceptionRecord->ExceptionCode)  
    {  
    case EXCEPTION_ACCESS_VIOLATION: /* Windows SIGSEGV */
    case STATUS_STACK_OVERFLOW:
      addr = (void *) ei->ExceptionRecord->ExceptionInformation[1];
      Handle_Bad_Address(addr);
      break;  

#ifdef DEBUG
    default:  
      printf("UNKNOWN exception\n");
      break;  
#endif
    }

  return EXCEPTION_EXECUTE_HANDLER;
}

#endif	/* WINDOWS */




/*-------------------------------------------------------------------------*
 * INSTALL_SIGSEGV_HANDLER                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Install_SIGSEGV_Handler(void)
{
#if defined(HAVE_WORKING_SIGACTION) || \
  defined(M_sparc_solaris) || defined(M_ix86_solaris) || \
  defined(M_ix86_sco) || defined(M_x86_64_solaris)

  struct sigaction act;

  act.sa_sigaction = (void (*)()) SIGSEGV_Handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO | SA_RESTART;

  sigaction(SIGSEGV, &act, NULL);
#  if defined(SIGBUS) && SIGBUS != SIGSEGV
  sigaction(SIGBUS, &act, NULL);
#  endif

#elif defined(_WIN32) && !defined(_WIN64)

  SetUnhandledExceptionFilter(Win32_Exception_Handler);

#else

  signal(SIGSEGV, (void (*)(int)) SIGSEGV_Handler);

#endif
}




/*-------------------------------------------------------------------------*
 * HANDLE_BAD_ADDRESS                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Handle_Bad_Address(void *bad_addr)
{
  int i = nb_handler;

  while(--i >= 0)
    {
      if ((*tbl_handler[i])(bad_addr))
	return;

    }
  
  Pl_Fatal_Error("Segmentation Violation at: %p", bad_addr);
  exit(1);
}




/*-------------------------------------------------------------------------*
 * PL_PUSH_SIGSEGV_HANDLER                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Push_SIGSEGV_Handler(SegvHdlr handler)
{
  if (nb_handler >= MAX_SIGSEGV_HANDLER)
    Pl_Fatal_Error("too many SIGSEGV handlers (max: %d)", MAX_SIGSEGV_HANDLER);

  tbl_handler[nb_handler++] = handler;
}




/*-------------------------------------------------------------------------*
 * PL_POP_SIGSEGV_HANDLER                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Pop_SIGSEGV_Handler(void)
{
  if (nb_handler > 0)
    nb_handler--;
}




/*-------------------------------------------------------------------------*
 * DEFAULT_SIGSEGV_HANDLER                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Default_SIGSEGV_Handler(void *bad_addr)
{
#ifdef M_USE_MAGIC_NB_TO_DETECT_STACK_NAME

  M_Check_Magic_Words();

  Pl_Fatal_Error("Segmentation Violation");

#else  /* !M_USE_MAGIC_NB_TO_DETECT_STACK_NAME */

  int i;
  WamWord *addr = (WamWord *) bad_addr;

#ifdef DEBUG
  DBGPRINTF("BAD ADDRESS:%p \n", addr);
#endif

  i = NB_OF_STACKS - 1;
  if (addr < pl_stk_tbl[i].stack + pl_stk_tbl[i].size + page_size)
    while (i >= 0)
      {
#ifdef DEBUG
        DBGPRINTF("STACK[%d].stack + size: %p\n",
		  i, pl_stk_tbl[i].stack + pl_stk_tbl[i].size);
#endif
        if (addr >= pl_stk_tbl[i].stack + pl_stk_tbl[i].size)
	  {
#ifdef DEBUG
	    DBGPRINTF("Found overflow on stack[%d]\n", i);
#endif
	    Pl_Fatal_Error(Stack_Overflow_Err_Msg(i));
	  }
        i--;
      }

  Pl_Fatal_Error("Segmentation Violation (bad address: %p)", addr);

#endif /* !M_USE_MAGIC_NB_TO_DETECT_STACK_NAME */

  return 1;	    /* treated, anyway this handler never returns (exit()) */
}




#ifdef M_USE_MAGIC_NB_TO_DETECT_STACK_NAME

#ifndef NO_USE_LINEDIT
#include "../Linedit/linedit.h"
#endif

/*-------------------------------------------------------------------------*
 * M_CHECK_MAGIC_WORDS                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
M_Check_Magic_Words(void)
{
  int i, err = 0;
  WamWord *end, *top;
  char *msg;
  char *sever;

  for (i = 0; i < NB_OF_STACKS; i++)
    {
      if (pl_stk_tbl[i].size == 0)
        continue;

      end = pl_stk_tbl[i].stack + pl_stk_tbl[i].size;
      top = Stack_Top(i);
#ifdef DEBUG
      DBGPRINTF("stack: %s start: %p  end: %p  top: %p\n", pl_stk_tbl[i].name,
                pl_stk_tbl[i].stack, end, top);
#endif
      sever = NULL;
      if (end[-16] != M_MAGIC1 || end[-32] != M_MAGIC2 || end[-33] != 0)
        sever = "Probable Error";
      else if (top < pl_stk_tbl[i].stack || top >= end)
        sever = "Possible Error";

      if (sever)
        {
          err++;
          msg = Stack_Overflow_Err_Msg(i);
#ifndef NO_USE_LINEDIT
          if (pl_le_hook_message_box)
            (*pl_le_hook_message_box)(sever, msg, 0);
          else
#endif
            fprintf(stderr, "%s: %s\n", sever, msg);
        }
    }
  if (err)
    Pl_Exit_With_Value(1);
}

#endif



/*-------------------------------------------------------------------------*
 * STACK_OVERFLOW_ERR_MSG                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Stack_Overflow_Err_Msg(int stk_nb)
{
  InfStack *s = pl_stk_tbl + stk_nb;
  char *var = s->env_var_name;
  int size = s->size;
  int usage = Stack_Top(stk_nb) - s->stack;
  static char msg[256];

  if (s->stack == Global_Stack)
    size += REG_BANK_SIZE;      /* see Init_Engine */

  size = Wam_Words_To_KBytes(size);
  usage = Wam_Words_To_KBytes(usage);

  if (pl_fixed_sizes || var[0] == '\0')
    sprintf(msg, ERR_STACK_OVERFLOW_NO_ENV, s->name, size, usage);
  else
    sprintf(msg, ERR_STACK_OVERFLOW_ENV, s->name, size, usage, var);

  return msg;
}
