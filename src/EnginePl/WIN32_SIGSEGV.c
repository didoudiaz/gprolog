#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

int
getpagesize(void)
{
  SYSTEM_INFO si;

  GetSystemInfo(&si);
  return si.dwPageSize;
}


long *fault_addr;

int
Is_Win32_SEGV(LPEXCEPTION_POINTERS err)
{
  PEXCEPTION_RECORD per = err->ExceptionRecord;

  if (per->ExceptionCode != EXCEPTION_ACCESS_VIOLATION)
    return EXCEPTION_CONTINUE_SEARCH;

  fault_addr = (long *) (per->ExceptionInformation[1]);
  return EXCEPTION_EXECUTE_HANDLER;
}

main(int argc, char *argv[])
{
  long *addr = NULL;
  int i;
  DWORD old_prot;

  setbuf(stdout, NULL);

  printf("Page Size:%d bytes\n", getpagesize());

  if (argc > 1)
    addr = (long *) strtoul(argv[1], NULL, 16);

  printf("TRYING at %#x\n", addr);

  addr =
    VirtualAlloc(addr, 4096 * 2, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
  if (addr == NULL)
    {
      printf("ERROR Alloc %lu\n", GetLastError());
      exit(1);
    }

  if (!VirtualProtect(addr + 1024, 4096, PAGE_NOACCESS, &old_prot))
    {
      printf("ERROR protect %lu\n", GetLastError());
      exit(1);
    }

  printf("ALLOC at %#x\n", addr);

  _try
  {
    long *a = (long *) 0x12345678;

    *a = 12;
  }
  _except(Is_Win32_SEGV(GetExceptionInformation()))
  {
    printf("ACCESS VIOLATION at addr=%#x\n", fault_addr);
  }

  _try
  {
    for (i = 0; i < 1025; i++)
      addr[i] = i;

    for (i = 0; i < 1024; i++)
      if (addr[i] != i)
	printf("ERROR at [%d]=%d\n", i, addr[i]);
  }
  _except(Is_Win32_SEGV(GetExceptionInformation()))
  {
    printf("ACCESS VIOLATION at [%d] addr=%#x (%#x)\n", i, fault_addr,
	   addr + i);
  }
  printf("FINISHED\n");
}
