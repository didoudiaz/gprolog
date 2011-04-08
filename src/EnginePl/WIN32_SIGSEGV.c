#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <stdint.h>
#include <inttypes.h>

int
getpagesize(void)
{
  SYSTEM_INFO si;

  GetSystemInfo(&si);
  return si.dwPageSize;
}


PlLong *fault_addr;

int
Is_Win32_SEGV(LPEXCEPTION_POINTERS err)
{
  PEXCEPTION_RECORD per = err->ExceptionRecord;

  if (per->ExceptionCode != EXCEPTION_ACCESS_VIOLATION)
    return EXCEPTION_CONTINUE_SEARCH;

  fault_addr = (PlLong *) (per->ExceptionInformation[1]);
  return EXCEPTION_EXECUTE_HANDLER;
}

main(int argc, char *argv[])
{
  PlLong *addr = NULL;
  int i;
  DWORD old_prot;

  setbuf(stdout, NULL);

  printf("Page Size:%d bytes\n", getpagesize());

  if (argc > 1)
    addr = (PlLong *) _strtoul(argv[1], NULL, 16);

  printf("TRYING at %#x\n", addr);

  addr =
    VirtualAlloc(addr, 4096 * 2, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
  if (addr == NULL)
    {
      printf("ERROR Alloc %" PL_FMT_u "\n", GetLastError());
      exit(1);
    }

  if (!VirtualProtect(addr + 1024, 4096, PAGE_NOACCESS, &old_prot))
    {
      printf("ERROR protect %" PL_FMT_u "\n", GetLastError());
      exit(1);
    }

  printf("ALLOC at %#x\n", addr);

  _try
  {
    PlLong *a = (PlLong *) 0x12345678;

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
