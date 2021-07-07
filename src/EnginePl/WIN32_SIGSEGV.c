#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <stdint.h>
#include <inttypes.h>

#include "pl_long.h"

#define ARRAY_SIZE 1024

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

  int page_size = getpagesize();
  printf("Page Size:%d bytes\n", page_size);

  if (argc > 1)
    addr = (PlLong *) strtoul(argv[1], NULL, 16);

  int size = ARRAY_SIZE * sizeof(*addr);
  int pages = (size + page_size - 1) / page_size;
  pages += 1; // reserve one more page for protect
  
  printf("ALLOC asked at %p (allocation for %d pages > %d bytes)\n", addr, pages, size);

  size = pages * page_size;
  
  // In Win64 does not work with only 2 pages of 4096 bytes - why ? (work with 3)
  addr = VirtualAlloc(addr, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
  if (addr == NULL)
    {
      printf("ERROR Alloc %d\n", GetLastError());
      exit(1);
    }

  printf("ALLOC obtained at %p\n", addr);
  
  printf("PROTECT at %p\n", addr + ARRAY_SIZE);
  if (!VirtualProtect(addr + ARRAY_SIZE, page_size, PAGE_NOACCESS, &old_prot))
    {
      printf("ERROR protect %d\n", GetLastError());
      exit(1);
    }


  _try
  {
        PlLong *a = (PlLong *) 0x12345678;

        *a = 12;
  }
  _except(Is_Win32_SEGV(GetExceptionInformation()))
  {
    printf("ACCESS VIOLATION at addr=%p\n", fault_addr);
  }

  _try
  {
    for (i = 0; i < ARRAY_SIZE; i++)
      addr[i] = i;

    for (i = 0; i < ARRAY_SIZE + 1; i++) // +1 to trigger a failure
      if (addr[i] != i)
	printf("ERROR at [%d]=%" PL_FMT_u "\n", i, addr[i]);
  }
  _except(Is_Win32_SEGV(GetExceptionInformation()))
  {
    printf("ACCESS VIOLATION at [%d] addr=%p (%p)\n", i, fault_addr, addr + i);
  }
  printf("FINISHED\n");
}
