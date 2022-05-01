#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

typedef long WamWord;

#ifdef __CYGWIN__
#error CYGWIN not supported
#endif

LONG WINAPI Win32_Exception_Handler(LPEXCEPTION_POINTERS ei)
{
  WamWord *addr;
  switch(ei->ExceptionRecord->ExceptionCode)  
    {  
    case EXCEPTION_ACCESS_VIOLATION: /* Windows SIGSEGV */
      addr = (WamWord *) ei->ExceptionRecord->ExceptionInformation[1];
      printf("Segmentation Violation at: %p\n", addr);
      exit(1);
      break;  

    default:  
      printf("UNKNOWN exception\n");
      break;  
    }

  return EXCEPTION_EXECUTE_HANDLER;
}


void Install_SIGSEGV_Handler(void);

main()
{
  long *x;

  Install_SIGSEGV_Handler();
  //  SetUnhandledExceptionFilter(Win32_Exception_Handler);
  x = (long *) 0xFEA4F124;
  *x = 12;
}


void
Install_SIGSEGV_Handler(void)
{
  SetUnhandledExceptionFilter(Win32_Exception_Handler);
}

