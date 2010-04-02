/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : machine.c                                                       *
 * Descr.: machine dependent features                                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2010 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU Lesser General Public License as published   *
 * by the Free Software Foundation; either version 3, or any later version.*
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU Lesser General Public License*
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/* $Id$ */

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

#include "gp_config.h"		/* ensure __unix__ defined if not Win32 */

#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>		/* warning: windows.h defines _WIN32 */
#endif

#if defined(__unix__) || defined(__CYGWIN__)
#include <pwd.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>
#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif
#else /* _WIN32 */
#include <process.h>
#include <direct.h>
#endif

#include "engine_pl.h"		/* before netdb.h which declares a function */
			 /* gcc cannot define a global reg var after a fct */

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifndef NO_USE_SOCKETS
#  if defined(__unix__) || defined(__CYGWIN__)
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#  endif
#define INET_MANAGEMENT
#endif


#if 0
#define DEBUG
#endif



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define M_MAGIC1                   0x12345678
#define M_MAGIC2                   0xdeadbeef

#define UNKOWN_SYS_ERRNO           "Unknown error (%d)"




	  /* Error Messages */

#define ERR_STACKS_ALLOCATION      "Memory allocation fault"

#define ERR_CANNOT_OPEN_DEV0       "Cannot open /dev/zero : %s"
#define ERR_CANNOT_UNMAP           "unmap failed : %s"

#define ERR_CANNOT_FREE        	   "VirtualFree failed : %lu"
#define ERR_CANNOT_PROTECT         "VirtualProtect failed : %lu"

#define ERR_CANNOT_EXEC_GETCWD     "cannot execute getcwd"


#define ERR_STACK_OVERFLOW_ENV     "%s stack overflow (size: %d Kb, environment variable used: %s)"

#define ERR_STACK_OVERFLOW_NO_ENV  "%s stack overflow (size: %d Kb - fixed size)"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static long start_user_time = 0;
static long start_system_time = 0;
static long start_real_time = 0;

static int cur_seed = 1;

static int page_size;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void SIGSEGV_Handler();

static char *Stack_Overflow_Err_Msg(int stk_nb);

#ifdef INET_MANAGEMENT

static char *Host_Name_From_Alias(struct hostent *host_entry);

#endif



#define Round_Up(x, y)          (((x) + (y) - 1) / (y) * (y))

#define Round_Down(x, y)        ((x) / (y) * (y))




/*-------------------------------------------------------------------------*
 * PL_INIT_MACHINE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Init_Machine(void)
{
  tzset();

  start_user_time = Pl_M_User_Time();
  start_system_time = Pl_M_System_Time();
  start_real_time = Pl_M_Real_Time();

#if defined(HAVE_MALLOPT) && defined(M_MMAP_MAX)
  mallopt(M_MMAP_MAX, 0);
#endif

  Pl_Init_Machine1();
}



#if defined(_WIN32) && !defined(__CYGWIN__)

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



#if HAVE_MMAP && !defined(_WIN32) && !defined(__CYGWIN__)
#include <sys/mman.h>

#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
#define MAP_ANON MAP_ANONYMOUS
#endif
#endif

/*-------------------------------------------------------------------------*
 * VIRTUAL_MEM_ALLOC                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord *
Virtual_Mem_Alloc(WamWord *addr, int length)
{
#if defined(_WIN32) || defined(__CYGWIN__)

  addr = (WamWord *) VirtualAlloc(addr, length,
				  MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);

#elif defined(HAVE_MMAP)

#ifndef MAP_ANON
  static int fd = -1;

  if (fd == -1)
    fd = open("/dev/zero", 0);

  if (fd == -1)
    Pl_Fatal_Error(ERR_CANNOT_OPEN_DEV0, Pl_M_Sys_Err_String(errno));
#endif /* !MAP_ANON */

  addr = (WamWord *) mmap((void *) addr, length, PROT_READ | PROT_WRITE,
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

  if (addr == (WamWord *) -1)
    addr = NULL;

#else

  addr = (WamWord *) Calloc(length, 1);

#endif

  return addr;
}



#if TAG_SIZE_HIGH > 0
/*-------------------------------------------------------------------------*
 * VIRTUAL_MEM_FREE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Virtual_Mem_Free(WamWord *addr, int length)
{
#if defined(_WIN32) || defined(__CYGWIN__)

  if (!VirtualFree(addr, 0, MEM_RELEASE))
    Pl_Fatal_Error(ERR_CANNOT_FREE, GetLastError());

#elif defined(HAVE_MMAP)

  if (munmap((void *) addr, length) == -1)
    Pl_Fatal_Error(ERR_CANNOT_UNMAP, Pl_M_Sys_Err_String(errno));

#else 

  Free(addr);
  
#endif
}
#endif	/* TAG_SIZE_HIGH > 0 */



/*-------------------------------------------------------------------------*
 * VIRTUAL_MEM_PROTECT                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Virtual_Mem_Protect(WamWord *addr, int length)
{
#if defined(_WIN32) || defined(__CYGWIN__)
  DWORD old_prot;

  if (!VirtualProtect(addr, length, PAGE_NOACCESS, &old_prot))
    Pl_Fatal_Error(ERR_CANNOT_PROTECT, GetLastError());

#elif defined(HAVE_MMAP)

#ifdef HAVE_MPROTECT
  if (mprotect((void *) addr, length, PROT_NONE) == -1)
#endif
    if (munmap((void *) addr, length) == -1)
      Pl_Fatal_Error(ERR_CANNOT_UNMAP, Pl_M_Sys_Err_String(errno));

#else

  addr[0] = M_MAGIC1;
  addr[1] = M_MAGIC2;		/* and rest (addr[1...]) should be 0 */

#endif
}




/*-------------------------------------------------------------------------*
 * PL_M_ALLOCATE_STACKS                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_M_Allocate_Stacks(void)
{
  unsigned length = 0;
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
      pl_stk_tbl[i].size = Round_Up(pl_stk_tbl[i].size, page_size);
      length += pl_stk_tbl[i].size + page_size;
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
	  addr = (WamWord *) Round_Down((long) addr, getpagesize());
	  addr -= length;
	}
#ifdef DEBUG
      DBGPRINTF("base: %p length: %d\n", addr, length);
#endif
      addr = Virtual_Mem_Alloc(addr, length);
#ifdef DEBUG
      DBGPRINTF("obtaining: %p\n", addr);
#endif
#if TAG_SIZE_HIGH > 0
      if (addr && (((unsigned long) (addr) + length) >> (WORD_SIZE - TAG_SIZE_HIGH)) != 0)
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
      addr += pl_stk_tbl[i].size;
      Virtual_Mem_Protect(addr, page_size);
      addr += page_size;
    }

#if defined(HAVE_WORKING_SIGACTION) || \
  defined(M_sparc_solaris) || defined(M_ix86_solaris) || \
  defined(M_ix86_sco) || defined(M_x86_64)

  {
    struct sigaction act;

    act.sa_sigaction = (void (*)()) SIGSEGV_Handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = SA_SIGINFO;

    sigaction(SIGSEGV, &act, NULL);
#ifdef M_darwin
    sigaction(SIGBUS, &act, NULL);
#endif
  }

#elif !defined(_WIN32) && !defined(__CYGWIN__)
  signal(SIGSEGV, (void (*)()) SIGSEGV_Handler);
#endif
}




#if defined(_WIN32) || defined(__CYGWIN__)

/*-------------------------------------------------------------------------*
 * WIN32_SEH_HANDLER                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
EXCEPT_DISPOSITION
Win32_SEH_Handler(EXCEPTION_RECORD *excp_rec, void *establisher_frame, 
		  CONTEXT *context_rec, void *dispatcher_cxt)
{
  WamWord *addr;
  
  if (excp_rec->ExceptionFlags)
    return ExceptContinueSearch; /* unwind and others */

  if (excp_rec->ExceptionCode != EXCEPTION_ACCESS_VIOLATION)
    return ExceptContinueSearch;
  
  addr = (WamWord *) excp_rec->ExceptionInformation[1];
  SIGSEGV_Handler(addr);
  return ExceptContinueExecution;
}

#endif


/*-------------------------------------------------------------------------*
 * SIGSEGV_HANDLER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
#if defined(HAVE_WORKING_SIGACTION) || \
  defined(M_sparc_solaris) || defined(M_ix86_solaris) || defined(M_x86_64_solaris)
void
SIGSEGV_Handler(int sig, siginfo_t *sip)

#elif defined(M_sparc_sunos)
static void
SIGSEGV_Handler(int sig, int code, int scp, WamWord *addr)

#elif defined(M_alpha_osf)
static void
SIGSEGV_Handler(int sig, int code, struct sigcontext *scp)

#elif defined(M_ix86_linux) || defined(M_powerpc_linux) || \
      defined(M_alpha_linux)

#ifdef LINUX_NEEDS_ASM_SIGCONTEXT // see configure.in
#include <asm/sigcontext.h>
#endif

#if 0				/* old linux */
static void
SIGSEGV_Handler(int sig, struct sigcontext_struct scp)
#else
static void
SIGSEGV_Handler(int sig, struct sigcontext scp)
#endif

#elif defined(M_ix86_sco)
#define _XOPEN_SOURCE_EXTENDED
#include <signal.h>
#include <sys/siginfo.h>
static void
SIGSEGV_Handler(int sig, siginfo_t * si)

#elif defined(M_ix86_bsd) || defined(M_powerpc_bsd) || defined(M_sparc_bsd)
static void
SIGSEGV_Handler(int sig, int code, struct sigcontext *scp)

#elif defined(_WIN32) || defined(__CYGWIN__)
void
SIGSEGV_Handler(WamWord *addr)

#elif defined(M_mips_irix)
#include <signal.h>
void
SIGSEGV_Handler(int sig, int code, struct sigcontext *scp)

#elif defined(M_x86_64_linux) || defined(M_x86_64_bsd)
void
SIGSEGV_Handler(int sig, siginfo_t *sip, void *scp)

#else
static void
SIGSEGV_Handler(int sig)
#endif
{
#if defined(HAVE_WORKING_SIGACTION) || \
  defined(M_sparc_solaris) || defined(M_ix86_solaris) || defined(M_x86_64_solaris)

  WamWord *addr = (WamWord *) sip->si_addr;

#elif defined(M_alpha_osf)

  WamWord *addr = (WamWord *) (scp->sc_traparg_a0);

#elif defined(M_alpha_linux)

  WamWord *addr = (WamWord *) (scp.sc_fp_trigger_inst);	/* why this one? */

  /* WamWord *addr=(WamWord *) (scp.sc_traparg_a0); */


#elif defined(M_ix86_linux)

  WamWord *addr = (WamWord *) scp.cr2;

#elif defined(M_x86_64_linux) || defined(M_x86_64_bsd)

  WamWord *addr = (WamWord *) sip->si_addr;

#elif defined(M_ix86_sco)

  WamWord *addr = (WamWord *) si->si_addr;

#elif defined(M_ix86_bsd)

  WamWord *addr = (WamWord *) scp->sc_err;

#elif defined(M_powerpc_linux)

  WamWord *addr = (WamWord *) scp.regs->dar;

#elif defined(M_mips_irix)

  WamWord *addr = scp->sc_regs[16];

#elif defined(_WIN32)
				/* addr passed as argument */
#else
				/* cannot detect fault addr */
#warning SIGSEGV_Handler does not know how to detect fault addr - use magic numbers

#define M_USE_MAGIC_NB_TO_DETECT_STACK_NAME

#endif


#ifdef M_USE_MAGIC_NB_TO_DETECT_STACK_NAME

  M_Check_Magic_Words();

#else  /* !M_USE_MAGIC_NB_TO_DETECT_STACK_NAME */

  int i;

#ifdef DEBUG
  DBGPRINTF("BAD ADDRESS:%lx\n", (long) addr);
#endif

  i = NB_OF_STACKS - 1;
  if (addr < pl_stk_tbl[i].stack + pl_stk_tbl[i].size + page_size)
    while (i >= 0)
      {
#ifdef DEBUG
	DBGPRINTF("STACK[%d].stack + size: %lx\n",
		  i, (long) (pl_stk_tbl[i].stack + pl_stk_tbl[i].size));
#endif
	if (addr >= pl_stk_tbl[i].stack + pl_stk_tbl[i].size)
	  Pl_Fatal_Error(Stack_Overflow_Err_Msg(i));
	i--;
      }
#endif /* !M_USE_MAGIC_NB_TO_DETECT_STACK_NAME */

  Pl_Fatal_Error("Segmentation Violation");

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

  for (i = 0; i < NB_OF_STACKS; i++)
    {
      if (pl_stk_tbl[i].size == 0)
	continue;

      end = pl_stk_tbl[i].stack + pl_stk_tbl[i].size;
#ifdef DEBUG
      DBGPRINTF("stack: %s start: %p  end: %p  top: %p\n", pl_stk_tbl[i].name, 
		pl_stk_tbl[i].stack, end, Stack_Top(i));
#endif
      if ((end[0] != M_MAGIC1 || end[1] != M_MAGIC2 || end[8] != 0)
	  || (top = Stack_Top(i)) >= end)
	{
	  err++;
	  msg = Stack_Overflow_Err_Msg(i);
#ifndef NO_USE_LINEDIT
	  if (pl_le_hook_message_box)
	    (*pl_le_hook_message_box)("Possible Error", msg, 0);
	  else
#endif
	    fprintf(stderr, "Possible Error: %s\n", msg);
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
  static char msg[256];

  if (s->stack == Global_Stack)
    size += REG_BANK_SIZE;	/* see Init_Engine */

  size = Wam_Words_To_KBytes(size);

  if (pl_fixed_sizes || var[0] == '\0')
    sprintf(msg, ERR_STACK_OVERFLOW_NO_ENV, s->name, size);
  else
    sprintf(msg, ERR_STACK_OVERFLOW_ENV, s->name, size, var);

  return msg;
}




/*-------------------------------------------------------------------------*
 * PL_M_SYS_ERR_STRING                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Sys_Err_String(int err_no)
{
#ifdef M_sparc_sunos
  extern char *sys_errlist[];
  extern int sys_nerr;
#endif

  char *str;
  static char buff[32];

#if defined(M_sparc_sunos)
  str = (err_no >= 0 && err_no < sys_nerr) ? sys_errlist[err_no] : NULL;
#else
  str = strerror(err_no);
#endif

  if (str)
    return str;

  sprintf(buff, UNKOWN_SYS_ERRNO, err_no);
  return buff;
}




#if 0
#define ULL unsigned long long
#else
#define ULL unsigned __int64
#endif

/*-------------------------------------------------------------------------*/
/* M_USER_TIME                                                             */
/*                                                                         */
/* returns the user time used since the start of the process (in ms).      */
/*-------------------------------------------------------------------------*/
long
Pl_M_User_Time(void)
{
  long user_time;

#if defined(__unix__) && !defined(__CYGWIN__)
  struct rusage rsr_usage;

  getrusage(RUSAGE_SELF, &rsr_usage);

  user_time = (rsr_usage.ru_utime.tv_sec * 1000) +
    (rsr_usage.ru_utime.tv_usec / 1000);

#elif defined(_WIN32) || defined(__CYGWIN__)
  FILETIME creat_t, exit_t, kernel_t, user_t;

  /* Success on Windows NT */
  if (GetProcessTimes(GetCurrentProcess(),
		      &creat_t, &exit_t, &kernel_t, &user_t))
    user_time = (long) (((ULL) user_t.dwHighDateTime << 32) +
			(ULL) user_t.dwLowDateTime) / 10000;
  else				/* not implemented on Windows 95/98 */
    user_time = (long) ((double) clock() * 1000 / CLOCKS_PER_SEC);

#else

  Pl_Fatal_Error("user time not available");
  return 0;

#endif

  return user_time - start_user_time;
}




/*-------------------------------------------------------------------------*
 * PL_M_SYSTEM_TIME                                                        *
 *                                                                         *
 * returns the system time used since the start of the process (in ms).    *
 *-------------------------------------------------------------------------*/
long
Pl_M_System_Time(void)
{
  long system_time;

#if defined(__unix__) && !defined(__CYGWIN__)
  struct rusage rsr_usage;

  getrusage(RUSAGE_SELF, &rsr_usage);

  system_time = (rsr_usage.ru_stime.tv_sec * 1000) +
    (rsr_usage.ru_stime.tv_usec / 1000);

#elif defined(_WIN32) || defined(__CYGWIN__)
  FILETIME creat_t, exit_t, kernel_t, user_t;

  /* Success on Windows NT */
  if (GetProcessTimes(GetCurrentProcess(),
		      &creat_t, &exit_t, &kernel_t, &user_t))
    system_time = (long) (((ULL) kernel_t.dwHighDateTime << 32) +
			  (ULL) kernel_t.dwLowDateTime) / 10000;
  else				/* not implemented on Windows 95/98 */
    system_time = 0;

#else

  Pl_Fatal_Error("system time not available");
  return 0;

#endif

  return system_time - start_system_time;
}




/*-------------------------------------------------------------------------*
 * PL_M_REAL_TIME                                                          *
 *                                                                         *
 * returns the real time used since the start of the process (in ms).      *
 *-------------------------------------------------------------------------*/
long
Pl_M_Real_Time(void)
{
  long real_time;

#if defined(__unix__) && !defined(__CYGWIN__)
  struct timeval tv;

  gettimeofday(&tv, NULL);
  real_time = (tv.tv_sec * 1000) + (tv.tv_usec / 1000);

#elif defined(_WIN32) || defined(__CYGWIN__)

  real_time = (long) ((double) clock() * 1000 / CLOCKS_PER_SEC);

#else

  Pl_Fatal_Error("real time not available");
  return 0;

#endif

  return real_time - start_real_time;
}




#ifndef RAND_MAX
#define RAND_MAX  (((unsigned) -1) >> 1)
#endif




/*-------------------------------------------------------------------------*
 * PL_M_RANDOMIZE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_M_Randomize(void)
{
  static int count = 0;
#if defined(_WIN32) || defined(__CYGWIN__)
  int seed = GetTickCount();
#else
  struct timeval tv;
  int seed;

  gettimeofday(&tv, NULL);
  seed = (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
#endif
  count = (count + rand()) % 0xFFFF;
  seed = seed ^ (getpid() << (seed & 0xFF));
  seed *= count;
  seed = seed & 0xFFFFFF;

  Pl_M_Set_Seed(seed);
}




/*-------------------------------------------------------------------------*
 * PL_M_SET_SEED                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_M_Set_Seed(int n)
{
  cur_seed = n;
  srand(cur_seed);
}




/*-------------------------------------------------------------------------*
 * PL_M_GET_SEED                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_M_Get_Seed(void)
{
  return cur_seed;
}




/*-------------------------------------------------------------------------*
 * PL_M_RANDOM_INTEGER                                                     *
 *                                                                         *
 * return an integer x s.t. 0 <= x < n                                     *
 *-------------------------------------------------------------------------*/
int
Pl_M_Random_Integer(int n)
{
  return (int) ((double) n * rand() / (RAND_MAX + 1.0));
}




/*-------------------------------------------------------------------------*
 * PL_M_RANDOM_FLOAT                                                       *
 *                                                                         *
 * return a double x s.t. 0 <= x < n                                       *
 *-------------------------------------------------------------------------*/
double
Pl_M_Random_Float(double n)
{
  return n * rand() / (RAND_MAX + 1.0);
}




/*-------------------------------------------------------------------------*
 * PL_M_HOST_NAME_FROM_NAME                                                *
 *                                                                         *
 * if host_name == NULL use current host name.                             *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Host_Name_From_Name(char *host_name)
{
  static char buff[4096];

#ifdef INET_MANAGEMENT
  struct hostent *host_entry;
#endif

  if (host_name == NULL)
    {
      long length = sizeof(buff);
      host_name = buff;
#if defined(_WIN32) && !defined(__CYGWIN__) && defined(NO_USE_SOCKETS)
      if (GetComputerName(buff, &length) == 0)
#else
      if (gethostname(buff, length))
#endif
	{
	  strcpy(buff, "unknown host name");
	  goto finish;
	}
    }

  if (strchr(host_name, '.') != NULL)	/* qualified name */
    goto finish;

#ifdef INET_MANAGEMENT

  host_entry = gethostbyname(host_name);	/* use name server */
  if (host_entry == NULL)
    goto finish;

  host_name = Host_Name_From_Alias(host_entry);

#endif

finish:
  return host_name;
}




/*-------------------------------------------------------------------------*
 * PL_M_HOST_NAME_FROM_ADR                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Host_Name_From_Adr(char *host_address)
{
#ifdef INET_MANAGEMENT
  struct hostent *host_entry;
  struct in_addr iadr;

#if defined(M_sparc_sunos) || defined(M_sparc_solaris) || \
    defined(M_ix86_cygwin) || defined(M_ix86_solaris)  || \
    defined(_WIN32)
  if ((iadr.s_addr = inet_addr(host_address)) == -1)
#else
  if (inet_aton(host_address, &iadr) == 0)
#endif
    return FALSE;

  host_entry = gethostbyaddr((char *) &iadr, sizeof(iadr), AF_INET);
  if (host_entry == NULL)
    return NULL;

  return Host_Name_From_Alias(host_entry);
#else
  return NULL;
#endif
}




#ifdef INET_MANAGEMENT

/*-------------------------------------------------------------------------*
 * HOST_NAME_FROM_ALIAS                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Host_Name_From_Alias(struct hostent *host_entry)
{
  char *name;
  char **alias;
  char *p;

  name = (char *) host_entry->h_name;
  alias = host_entry->h_aliases;
  while ((p = strchr(name, '.')) == NULL && *alias)
    name = *alias++;

  if (p)
    return name;

  return (char *) host_entry->h_name;
}

#endif



/*-------------------------------------------------------------------------*
 * PL_M_SET_WORKING_DIR                                                    *
 *                                                                         *
 * must preserve errno if fails (used in os_interf_c.c)                    *
 *-------------------------------------------------------------------------*/
Bool
Pl_M_Set_Working_Dir(char *path)
{
  char *new_path = Pl_M_Absolute_Path_Name(path);

  return (new_path != NULL && chdir(new_path) == 0);
}




/*-------------------------------------------------------------------------*
 * PL_M_GET_WORKING_DIR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Get_Working_Dir(void)
{
  static char cur_work_dir[MAXPATHLEN];

  if (getcwd(cur_work_dir, sizeof(cur_work_dir) - 1) == NULL)
    strcpy(cur_work_dir, ".");
  return cur_work_dir;
}




/*-------------------------------------------------------------------------*
 * PL_M_ABSOLUTE_PATH_NAME                                                 *
 *                                                                         *
 * returns an absolute file name.                                          *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Absolute_Path_Name(char *src)
{
  static char buff[2][MAXPATHLEN];
  int res = 0;
  char *dst;
  char *p, *q;
  char c;

  dst = buff[res];
  while ((*dst++ = *src))	/* expand $VARNAME and %VARNAME% (Win32) */
    {
      c = *src++;
      if (c == '$'
#if defined(_WIN32) || defined(__CYGWIN__)
	  || c == '%'
#endif
	)
	{
	  p = dst;
	  while (isalnum(*src))
	    *dst++ = *src++;
#if defined(_WIN32) || defined(__CYGWIN__)
	  if (c == '%' && *src != '%')
	    continue;
#endif
	  *dst = '\0';
	  q = getenv(p);
	  if (q)
	    {
	      p--;
	      strcpy(p, q);
	      dst = p + strlen(p);
#if defined(_WIN32) || defined(__CYGWIN__)
	      if (c == '%')
		src++;
#endif
	    }
#if defined(_WIN32) || defined(__CYGWIN__)
	  else if (c == '%')
	    *dst++ = *src++;
#endif
	}
    }
  *dst = '\0';

  if (buff[res][0] == '~')
    {
      if (buff[res][1] == DIR_SEP_C || buff[res][1] == '\0')	/* ~/... cf $HOME */
	{
	  if ((p = getenv("HOME")) == NULL && (p = getenv("HOMEPATH")) == NULL)
	    return NULL;

	  sprintf(buff[1 - res], "%s/%s", p, buff[res] + 1);
	  res = 1 - res;
	}
#if defined(__unix__) || defined(__CYGWIN__)
      else			/* ~user/... read passwd */
	{
	  struct passwd *pw;

	  p = buff[res] + 1;
	  while (*p && *p != '/')
	    p++;

	  buff[res][0] = *p;
	  *p = '\0';
	  if ((pw = getpwnam(buff[res] + 1)) == NULL)
	    return NULL;

	  *p = buff[res][0];

	  sprintf(buff[1 - res], "%s/%s", pw->pw_dir, p);
	  res = 1 - res;
	}
#endif
    }

  if (strcmp(buff[res], "user") == 0)	/* prolog special file 'user' */
    return buff[res];

#if defined(_WIN32) && !defined(__CYGWIN__)

  if (_fullpath(buff[1 - res], buff[res], MAXPATHLEN) == NULL)
    return NULL;

  res = 1 - res;
  for (dst = buff[res]; *dst; dst++)	/* \ becomes / */
    if (*dst == '\\')
      *dst = '/';

  /* dst points the \0 */

#else  /* __unix__ || __CYGWIN__ */

#if defined(__CYGWIN__)

  cygwin_conv_to_full_posix_path(buff[res], buff[1 - res]);
  res = 1 - res;

#endif

  if (buff[res][0] != '/')	/* add current directory */
    {
      sprintf(buff[1 - res], "%s/%s", Pl_M_Get_Working_Dir(), buff[res]);
      res = 1 - res;
    }

  src = buff[res];
  res = 1 - res;
  dst = buff[res];

  while ((*dst++ = *src))
    {
      if (*src++ != '/')
	continue;

    collapse:
      while (*src == '/')	/* collapse /////... as / */
	src++;

      if (*src != '.')
	continue;

      if (src[1] == '/' || src[1] == '\0')	/* /./ removed */
	{
	  src++;
	  goto collapse;
	}

      if (src[1] != '.' || (src[2] != '/' && src[2] != '\0'))
	continue;
      /* case /../ */
      src += 2;
      p = dst - 2;
      while (p >= buff[res] && *p != '/')
	p--;

      if (p < buff[res])
	return NULL;

      dst = p;
    }

  dst--;			/* dst points the \0 */

#endif


#if defined(_WIN32) && !defined(__CYGWIN__)
#define MIN_PREFIX 3		/* win32 minimal path c:\  */
#else
#define MIN_PREFIX 1		/* unix  minimal path /    */
#endif

  if (dst - buff[res] > MIN_PREFIX && (dst[-1] == '/' || dst[-1] == '\\'))
    dst[-1] = '\0';		/* remove last / or \ */

  return buff[res];
}
