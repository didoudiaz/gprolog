/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : machine.c                                                       *
 * Descr.: machine dependent features                                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     *
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

#include "gp_config.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef M_ix86_win32
#include <windows.h>
#include <process.h>
#include <direct.h>
#else
#include <pwd.h>
#include <unistd.h>
#include <sys/param.h>
#endif

#ifdef M_ix86_cygwin
#include <process.h>
#include <sys/cygwin.h>
#include <sys/times.h>
#endif

#include "engine_pl.h"		/* before netdb.h which declares a function */
			 /* gcc cannot define a global reg var after a fct */

#ifndef NO_USE_SOCKETS
#  ifndef M_ix86_win32
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

#define M_SECURITY_MARGIN          128	/* in WamWords */
#define M_MAGIC                    0x12345678

#define UNKOWN_SYS_ERRNO           "Unknown error (%d)"




	  /* Error Messages */

#define ERR_STACKS_ALLOCATION      "Memory allocation fault"

#define ERR_CANNOT_OPEN_DEV0       "Cannot open /dev/zero : %s"
#define ERR_CANNOT_UNMAP           "unmap failed : %s"

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

#ifdef M_ix86_cygwin
static long tps = 0;
static clock_t rt0;
static struct tms time_buf0;
#endif

static long start_user_time = 0;
static long start_system_time = 0;
static long start_real_time = 0;

static int cur_seed = 1;

static char cur_work_dir[MAXPATHLEN];


#ifdef M_USE_MAGIC_NB_TO_DETECT_STACK_NAME

static WamWord *check_adr[NB_OF_STACKS + 1];

#else

static int page_size;

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#ifdef M_USE_MAGIC_NB_TO_DETECT_STACK_NAME

static void Fill_Magic_Adr_Table(void);

#endif

#ifndef M_ix86_win32

static void SIGSEGV_Handler();

#endif

static void Stack_Overflow(int stk_nb);

#ifdef INET_MANAGEMENT

static char *Host_Name_From_Alias(struct hostent *host_entry);

#endif



#define Round_Up(x, y)          (((x) + (y) - 1) / (y) * (y))

#define Round_Down(x, y)        ((x) / (y) * (y))




/*-------------------------------------------------------------------------*
 * INIT_MACHINE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Init_Machine(void)
{
#ifdef M_ix86_cygwin
  tps = sysconf(_SC_CLK_TCK);
  rt0 = times(&time_buf0);
#endif

  tzset();

  start_user_time = M_User_Time();
  start_system_time = M_System_Time();
  start_real_time = M_Real_Time();

  getcwd(cur_work_dir, sizeof(cur_work_dir) - 1);

#if defined(HAVE_MALLOPT) && defined(M_MMAP_MAX)
  mallopt(M_MMAP_MAX, 0);
#endif

  Init_Machine1();
}




#ifdef M_USE_MALLOC

/*-------------------------------------------------------------------------*
 * M_ALLOCATE_STACKS                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
M_Allocate_Stacks(void)
{
  unsigned len = 0;
  WamWord *addr;
  int i;

  for (i = 0; i < NB_OF_STACKS; i++)
    len += stk_tbl[i].size;

  addr = (WamWord *) Calloc(len, sizeof(WamWord));
  if (addr == NULL)
    Fatal_Error(ERR_STACKS_ALLOCATION);

  for (i = 0; i < NB_OF_STACKS; i++)
    {
      stk_tbl[i].stack = addr;
      addr += stk_tbl[i].size;
    }

#ifdef M_USE_MAGIC_NB_TO_DETECT_STACK_NAME
  Fill_Magic_Adr_Table();
#endif
}

#endif




#ifdef M_USE_MMAP

#ifndef M_ix86_win32
#include <sys/mman.h>

#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
#define MAP_ANON MAP_ANONYMOUS
#endif
#endif

/*-------------------------------------------------------------------------*
 * M_ALLOCATE_STACKS                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
M_Allocate_Stacks(void)
{
  unsigned len = 0;
  WamWord *addr;
  int i;

#if !defined(M_ix86_win32) && !defined(MAP_ANON)
  int fd;

  fd = open("/dev/zero", 0);

  if (fd == -1)
    Fatal_Error(ERR_CANNOT_OPEN_DEV0, M_Sys_Err_String(errno));

#endif

  page_size = getpagesize() / sizeof(WamWord);

  for (i = 0; i < NB_OF_STACKS; i++)
    {
      stk_tbl[i].size = Round_Up(stk_tbl[i].size, page_size);
      len += stk_tbl[i].size + page_size;
    }

  addr = (WamWord *) M_MMAP_HIGH_ADR;
  len *= sizeof(WamWord);

#if !defined(M_ix86_win32) && defined(M_MMAP_HIGH_ADR_ALT)
  i = 0;
try_mmap:
#endif
#ifdef DEBUG
  DBGPRINTF("trying at high addr:%lx\n", (long) addr);
#endif
  addr = (WamWord *) Round_Down((long) addr, getpagesize());
  addr -= len;

#ifdef M_ix86_win32
  addr = (WamWord *) VirtualAlloc(addr, len,
				  MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
#else
  addr = (WamWord *) mmap((caddr_t) addr, len, PROT_READ | PROT_WRITE,
#ifdef MAP_ANON
			  MAP_PRIVATE | MAP_ANON, -1,
#elif defined(M_sparc_sunos) || defined(M_sparc_solaris)
			  MAP_PRIVATE | MAP_FIXED, fd,
#else
			  MAP_PRIVATE, fd,
#endif
			  0);
#endif


#ifdef DEBUG
  DBGPRINTF("start addr:%lx   size:%d\n", (long) addr, len);
#endif

#ifdef M_ix86_win32
  if (addr == NULL)
#else
  if ((long) addr == -1
      || ((long) addr & (((1L << TAG_SIZE) - 1) << VALUE_SIZE)) != 0)
#endif
    {
#if !defined(M_ix86_win32) && defined(M_MMAP_HIGH_ADR_ALT)
      if (i == 0)
	{
	  i = 1;
	  if ((long) addr >= 0)
	    munmap(addr, len);
	  addr = (WamWord *) M_MMAP_HIGH_ADR_ALT;
	  goto try_mmap;
	}
#endif

      Fatal_Error(ERR_STACKS_ALLOCATION);
    }

  for (i = 0; i < NB_OF_STACKS; i++)
    {
      stk_tbl[i].stack = addr;
      addr += stk_tbl[i].size;
#ifdef M_ix86_win32
      {
	DWORD old_prot;

	if (!VirtualProtect(addr, page_size, PAGE_NOACCESS, &old_prot))
	  Fatal_Error(ERR_CANNOT_PROTECT, GetLastError());
      }
#else
      if (munmap((caddr_t) addr, page_size) == -1)
	Fatal_Error(ERR_CANNOT_UNMAP, M_Sys_Err_String(errno));
#endif

      addr += page_size;
    }

#ifdef M_USE_MAGIC_NB_TO_DETECT_STACK_NAME
  Fill_Magic_Adr_Table();
#endif

#if defined(M_sparc_solaris) || defined(M_ix86_solaris) || \
    defined(M_ix86_sco)
  {
    struct sigaction act;

    act.sa_handler = NULL;
    act.sa_sigaction = (void (*)()) SIGSEGV_Handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = SA_SIGINFO;

    sigaction(SIGSEGV, &act, NULL);
  }

#elif !defined(M_ix86_win32)
  signal(SIGSEGV, (void (*)()) SIGSEGV_Handler);
#endif
}

#endif /* M_USE_MMAP */




#ifdef M_USE_MAGIC_NB_TO_DETECT_STACK_NAME

/*-------------------------------------------------------------------------*
 * FILL_MAGIC_ADR_TABLE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Fill_Magic_Adr_Table(void)
{
  int i;

  for (i = 0; i < NB_OF_STACKS; i++)
    {
      check_adr[i] = stk_tbl[i].stack + stk_tbl[i].size - M_SECURITY_MARGIN;
      *check_adr[i] = M_MAGIC;
    }

  check_adr[NB_OF_STACKS] = NULL;
}

#endif



#ifdef M_ix86_win32

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




static long *fault_addr;

/*-------------------------------------------------------------------------*
 * IS_WIN32_SEGV                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Is_Win32_SEGV(void *exp)
{
  LPEXCEPTION_POINTERS err = (LPEXCEPTION_POINTERS) exp;
  PEXCEPTION_RECORD per = err->ExceptionRecord;

  if (per->ExceptionCode != EXCEPTION_ACCESS_VIOLATION)
    return EXCEPTION_CONTINUE_SEARCH;

  fault_addr = (long *) (per->ExceptionInformation[1]);
  return EXCEPTION_EXECUTE_HANDLER;
}

#endif /* M_ix86_win32 */




/*-------------------------------------------------------------------------*
 * SIGSEGV_HANDLER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
#if defined(M_sparc_sunos)

static void
SIGSEGV_Handler(int sig, int code, int scp, WamWord *addr)
#elif defined(M_sparc_solaris) || defined(M_ix86_solaris)

void
SIGSEGV_Handler(int sig, siginfo_t * sip)
#elif defined(M_alpha_osf)

static void
SIGSEGV_Handler(int sig, int code, struct sigcontext *scp)
#elif defined(M_ix86_linux) || defined(M_powerpc_linux) || defined(M_alpha_linux)
#include <asm/sigcontext.h>

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
#elif defined(M_ix86_bsd)

static void
SIGSEGV_Handler(int sig, int code, struct sigcontext *scp)
#elif defined(M_ix86_win32)

void
SIGSEGV_Handler(void)
#elif defined(M_mips_irix)

#include <signal.h>

void
SIGSEGV_Handler(int sig, int code, struct sigcontext *scp)
#else

static void
SIGSEGV_Handler(int sig)
#endif
{
#ifdef M_USE_MAGIC_NB_TO_DETECT_STACK_NAME

  M_Check_Magic_Words();

#else

#if defined(M_alpha_osf)

  WamWord *addr = (WamWord *) (scp->sc_traparg_a0);

#elif defined(M_alpha_linux)

  WamWord *addr = (WamWord *) (scp.sc_fp_trigger_inst);	/* why this one? */

  /* WamWord *addr=(WamWord *) (scp.sc_traparg_a0); */

#elif defined(M_sparc_solaris) || defined(M_ix86_solaris)

  WamWord *addr = (WamWord *) sip->si_addr;

#elif defined(M_ix86_linux)

  WamWord *addr = (WamWord *) scp.cr2;

#elif defined(M_ix86_sco)

  WamWord *addr = (WamWord *) si->si_addr;

#elif defined(M_ix86_bsd)

  WamWord *addr = (WamWord *) scp->sc_err;

#elif defined(M_powerpc_linux)

  WamWord *addr = (WamWord *) scp.regs->dar;

#elif defined(M_mips_irix)

  WamWord *addr = scp->sc_regs[16];

#elif defined(M_ix86_win32)

  WamWord *addr = (WamWord *) fault_addr;

#endif

  int i;

#ifdef DEBUG
  DBGPRINTF("BAD ADDRESS:%lx\n", (long) addr);
#endif /* DEBUG */

  i = NB_OF_STACKS - 1;
  if (addr < stk_tbl[i].stack + stk_tbl[i].size + page_size)
    while (i >= 0)
      {
#ifdef DEBUG
	DBGPRINTF("STACK[%d].stack + size: %lx\n",
		  i, (long) (stk_tbl[i].stack + stk_tbl[i].size));
#endif
	if (addr >= stk_tbl[i].stack + stk_tbl[i].size)
	  Stack_Overflow(i);
	i--;
      }
#endif

  Fatal_Error("Segmentation Violation");
}




#ifdef M_USE_MAGIC_NB_TO_DETECT_STACK_NAME

/*-------------------------------------------------------------------------*
 * M_CHECK_MAGIC_WORDS                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
M_Check_Magic_Words(void)
{
  WamWord **p = (WamWord **) check_adr;

  for (p = (WamWord **) check_adr; *p; p++)
    if (**p != M_MAGIC)
      Stack_Overflow(p - (WamWord **) check_adr);
}

#endif




/*-------------------------------------------------------------------------*
 * STACK_OVERFLOW                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Stack_Overflow(int stk_nb)
{
  InfStack *s = stk_tbl + stk_nb;
  char *var = s->env_var_name;
  int size = s->size;

  if (s->stack == Global_Stack)
    size += REG_BANK_SIZE;	/* see Init_Engine */

  size = Wam_Words_To_KBytes(size);

  if (fixed_sizes || var[0] == '\0')
    Fatal_Error(ERR_STACK_OVERFLOW_NO_ENV, s->name, size);

  Fatal_Error(ERR_STACK_OVERFLOW_ENV, s->name, size, var);
}




/*-------------------------------------------------------------------------*
 * M_SYS_ERR_STRING                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
M_Sys_Err_String(int err_no)
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




#if defined(__unix__) || defined(__CYGWIN__)

#include <sys/time.h>
#include <sys/resource.h>

#endif

/*-------------------------------------------------------------------------*
 * M_USER_TIME                                                             *
 *                                                                         *
 * returns the user time used since the start of the process (in ms).      *
 *-------------------------------------------------------------------------*/
long
M_User_Time(void)
{
  long user_time;

#if defined(__unix__)
  struct rusage rsr_usage;

  getrusage(RUSAGE_SELF, &rsr_usage);

  user_time = (rsr_usage.ru_utime.tv_sec * 1000) +
    (rsr_usage.ru_utime.tv_usec / 1000);

#elif defined(M_ix86_cygwin)	/* does not work well, returns real_time */

  struct tms time_buf1;

  times(&time_buf1);
  user_time = (time_buf1.tms_utime - time_buf0.tms_utime) * 1000 / tps;

#elif defined(M_ix86_win32)

  user_time = (long) ((double) clock() * 1000 / CLOCKS_PER_SEC);

#else

  Fatal_Error("user time not available");
  return 0;

#endif

  return user_time - start_user_time;
}




/*-------------------------------------------------------------------------*
 * M_SYSTEM_TIME                                                           *
 *                                                                         *
 * returns the system time used since the start of the process (in ms).    *
 *-------------------------------------------------------------------------*/
long
M_System_Time(void)
{
  long system_time;

#if defined(__unix__)
  struct rusage rsr_usage;

  getrusage(RUSAGE_SELF, &rsr_usage);

  system_time = (rsr_usage.ru_stime.tv_sec * 1000) +
    (rsr_usage.ru_stime.tv_usec / 1000);

#elif defined(M_ix86_cygwin)	/* does not work well, returns 0 */

  struct tms time_buf1;

  times(&time_buf1);
  system_time = (time_buf1.tms_stime - time_buf0.tms_stime) * 1000 / tps;

#elif defined(M_ix86_win32)

  system_time = 0;

#else

  Fatal_Error("system time not available");
  return 0;

#endif

  return system_time - start_system_time;
}




/*-------------------------------------------------------------------------*
 * M_REAL_TIME                                                             *
 *                                                                         *
 * returns the real time used since the start of the process (in ms).      *
 *-------------------------------------------------------------------------*/
long
M_Real_Time(void)
{
  long real_time;

#if defined(__unix__) || defined(__CYGWIN__)
  struct timeval tv;

  gettimeofday(&tv, NULL);
  real_time = (tv.tv_sec * 1000) + (tv.tv_usec / 1000);

#elif defined(M_ix86_win32)

  real_time = M_User_Time();

#else

  Fatal_Error("real time not available");
  return 0;

#endif

  return real_time - start_real_time;
}




#ifndef RAND_MAX
#define RAND_MAX  (((unsigned) -1) >> 1)
#endif




/*-------------------------------------------------------------------------*
 * M_RANDOMIZE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
M_Randomize(void)
{
  M_Set_Seed(M_Real_Time());
  M_Set_Seed(M_Random_Integer(256));
}




/*-------------------------------------------------------------------------*
 * M_SET_SEED                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
M_Set_Seed(int n)
{
  cur_seed = n;
  srand(cur_seed);
}




/*-------------------------------------------------------------------------*
 * M_GET_SEED                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
M_Get_Seed(void)
{
  return cur_seed;
}




/*-------------------------------------------------------------------------*
 * M_RANDOM_INTEGER                                                        *
 *                                                                         *
 * return an integer x s.t. 0 <= x < n                                     *
 *-------------------------------------------------------------------------*/
int
M_Random_Integer(int n)
{
  return (int) ((double) n * rand() / (RAND_MAX + 1.0));
}




/*-------------------------------------------------------------------------*
 * M_RANDOM_FLOAT                                                          *
 *                                                                         *
 * return a double x s.t. 0 <= x < n                                       *
 *-------------------------------------------------------------------------*/
double
M_Random_Float(double n)
{
  return n * rand() / (RAND_MAX + 1.0);
}




/*-------------------------------------------------------------------------*
 * M_HOST_NAME_FROM_NAME                                                   *
 *                                                                         *
 * if host_name==NULL use current host name.                               *
 *-------------------------------------------------------------------------*/
char *
M_Host_Name_From_Name(char *host_name)
{
  static char buff[4096];

#ifdef M_ix86_win32
  long length = sizeof(buff);
#endif
#ifdef INET_MANAGEMENT
  struct hostent *host_entry;
#endif

  if (host_name == NULL)
    {
      host_name = buff;
#if defined(M_ix86_win32) && defined(NO_USE_SOCKETS)
      if (GetComputerName(buff, &length) == 0)
#else
      if (gethostname(buff, sizeof(buff)))
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
 * M_HOST_NAME_FROM_ADR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
M_Host_Name_From_Adr(char *host_address)
{
#ifdef INET_MANAGEMENT
  struct hostent *host_entry;
  struct in_addr iadr;

#if defined(M_sparc_sunos) || defined(M_sparc_solaris) || \
    defined(M_ix86_cygwin) || defined(M_ix86_solaris)  || \
    defined(M_ix86_win32)
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
 * M_SET_WORKING_DIR                                                       *
 *                                                                         *
 * must preserve errno if fails (used in os_interf_c.c)                    *
 *-------------------------------------------------------------------------*/
Bool
M_Set_Working_Dir(char *path)
{
  char *new_path = M_Absolute_Path_Name(path);

  if (new_path != NULL && chdir(new_path) == 0)
    {
      strcpy(cur_work_dir, new_path);
      return TRUE;
    }

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * M_GET_WORKING_DIR                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
M_Get_Working_Dir(void)
{
  return cur_work_dir;
}




/*-------------------------------------------------------------------------*
 * M_ABSOLUTE_PATH_NAME                                                    *
 *                                                                         *
 * returns an absolute file name.                                          *
 *-------------------------------------------------------------------------*/
char *
M_Absolute_Path_Name(char *src)
{
  static char buff[2][MAXPATHLEN];
  int res = 0;
  char *dst;
  char *p, *q;

#ifdef M_ix86_win32
  char c;
#endif


  dst = buff[res];
  while ((*dst++ = *src))	/* expand $VARNAME */
    {
#ifdef M_ix86_win32
      c = *src;
#endif
      if (*src++ == '$'
#ifdef M_ix86_win32
	  || c == '%'
#endif
	)
	{
	  p = dst;
	  while (isalnum(*src))
	    *dst++ = *src++;
#ifdef M_ix86_win32
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
#ifdef M_ix86_win32
	      if (c == '%')
		src++;
#endif
	    }
#ifdef M_ix86_win32
	  else if (c == '%')
	    *dst++ = *src++;
#endif
	}
    }
  *dst = '\0';

#if defined(M_ix86_win32) || defined(M_ix86_cygwin)
  for (src = buff[res]; *src; src++)	/* \ becomes / */
    if (*src == '\\')
      *src = '/';
#endif

  if (strcmp(buff[res], "user") == 0)	/* prolog special file 'user' */
    return buff[res];


  if (buff[res][0] == '~')
    {
      if (buff[res][1] == DIR_SEP_C || buff[res][1] == '\0')	/* ~/... cf $HOME */
	{
	  if ((p = getenv("HOME")) == NULL)
	    return NULL;

	  sprintf(buff[1 - res], "%s/%s", p, buff[res] + 1);
	  res = 1 - res;
	}
#if defined(__unix__) || defined(__CYGWIN__)
      else			/* ~user/... read passwd */
	{
	  struct passwd *pw;

	  p = buff[res] + 1;
	  while (*p && *p != DIR_SEP_C)
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

#ifdef M_ix86_cygwin
  cygwin_conv_to_full_posix_path(buff[res], buff[1 - res]);
  res = 1 - res;
#endif

#ifdef M_ix86_win32
  if (_fullpath(buff[1 - res], buff[res], MAXPATHLEN) == NULL)
    return NULL;
  res = 1 - res;
#else
  if (buff[res][0] != DIR_SEP_C)	/* add cur_work_dir */
    {
      sprintf(buff[1 - res], "%s/%s", cur_work_dir, buff[res]);
      res = 1 - res;
    }
#endif

  res = 1 - res;
  src = buff[1 - res];
  dst = buff[res];

  while ((*dst++ = *src))
    {
      if (*src++ != DIR_SEP_C)
	continue;

    collapse:
#ifndef M_ix86_cygwin		/* CYGWIN uses //<drive>/<path> for <drive>:\<path> */
      while (*src == DIR_SEP_C)	/* collapse /////... as / */
	src++;
#endif
      if (*src != '.')
	continue;

      if (src[1] == DIR_SEP_C || src[1] == '\0')	/* /./ removed */
	{
	  src++;
	  goto collapse;
	}

      if (src[1] != '.' || (src[2] != DIR_SEP_C && src[2] != '\0'))
	continue;
      /* case /../ */
      src += 2;
      p = dst - 2;
      while (p >= buff[res] && *p != DIR_SEP_C)
	p--;

      if (p < buff[res])
	return NULL;

      dst = p;
    }

  if (dst[-2] == DIR_SEP_C)
    dst[-2] = '\0';		/* remove last / */

  return buff[res];
}
