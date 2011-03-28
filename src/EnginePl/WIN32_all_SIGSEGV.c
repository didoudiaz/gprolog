#include <signal.h>
#include <stdio.h>
#include <windows.h>
#include <stdint.h>
#include <inttypes.h>

#if defined(__GNUC__) || defined(__LCC__)

typedef enum {
    ExceptionContinueExecution,
    ExceptionContinueSearch,
    ExceptionNestedException,
    ExceptionCollidedUnwind
} EXCEPTION_DISPOSITION;

#endif

typedef struct _excp_lst
{
  struct _excp_lst *chain;
  EXCEPTION_DISPOSITION (*handler)();
} excp_lst;


#ifdef __GNUC__

#  define SEH_PUSH(new_handler)			\
{						\
  excp_lst e;					\
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


PlLong *fault_addr;

EXCEPTION_DISPOSITION
ExceptionWrapper(EXCEPTION_RECORD *excp_rec, void *establisher_frame,
		 CONTEXT *context_rec, void *dispatcher_cxt)
{
  if (excp_rec->ExceptionFlags)
    return ExceptionContinueSearch; /* unwind and others */

  if (excp_rec->ExceptionCode == EXCEPTION_ACCESS_VIOLATION)
    fault_addr = (PlLong *) excp_rec->ExceptionInformation[1];

  printf("addr:%p\n", fault_addr);
  /* exit(1); */
  return ExceptionContinueSearch;
}

int
SIGSEGV_Handler(int sig)
{

  printf("Segmentation Violation at: %p\n", fault_addr);
  exit(1);
}

EXCEPTION_DISPOSITION
ExceptionHandler(EXCEPTION_RECORD *excp_rec, void *establisher_frame,
		 CONTEXT *context_rec, void *dispatcher_cxt)
{
  char *addr;
  DWORD old_prot;

  if (excp_rec->ExceptionFlags)
    return ExceptionContinueSearch; /* unwind and others */

  if (excp_rec->ExceptionCode != EXCEPTION_ACCESS_VIOLATION)
    return ExceptionContinueSearch;

  addr = (char *) excp_rec->ExceptionInformation[1];

  printf("access violation at addr:%p - unprotect this page and retry\n", addr);
  if (!VirtualProtect(addr, 4096, PAGE_READWRITE, &old_prot))
    {
      printf("In Handler VirtualProtect failed: %" PL_FMT_u "\n", GetLastError());
      return ExceptionContinueSearch;
    }

  return ExceptionContinueExecution;
}

int bar=1;

int
main()
{
#if 0
  PlLong *x;
  SEH_PUSH(ExceptionWrapper);
  signal(SIGSEGV, (void (*)(int)) SIGSEGV_Handler);
  x = (PlLong *) 0xffff040;
  *x = 12 / bar;		/* set bar to 1 to test div by 0 exception */
  SEH_POP;
#else
  char *addr;
  int page_size, i;
  DWORD old_prot;
  page_size = 4096;
  addr = (char *) VirtualAlloc(0, 8192, MEM_RESERVE | MEM_COMMIT,
			       PAGE_READWRITE);
  if (addr == NULL)
    {
      printf("VirtualAlloc failed : %" PL_FMT_u "\n", GetLastError());
      return 1;
    }

  if (!VirtualProtect(addr + 4096, page_size, PAGE_NOACCESS, &old_prot))
    {
      printf("VirtualProtect failed : %" PL_FMT_u "\n", GetLastError());
      return 1;
    }
  SEH_PUSH(ExceptionHandler);

  printf("One page allocated at:%p  no access at: %p\n", addr, addr + 4096);
  for(i = 0; i < 8192; i++)
    addr[i] = i & 0x7f;
  printf("seem OK, checking...\n");
  for(i = 0; i < 8192; i++)
    if (addr[i] != (i & 0x7f))
      {
	printf("ERROR AT %p\n", addr + i);
	return 1;
      }
  printf("OK !\n");

  SEH_POP;
#endif
  return 0;
}
