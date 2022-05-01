#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

typedef long WamWord;



void
SIGSEGV_Handler(int sig, siginfo_t * sip)
{
  WamWord *addr = (WamWord *) sip->si_addr;

  printf("Segmentation Violation at: %p\n", addr);
  exit(1);
}



main()
{
  long *x;

#if 0
  signal(SIGSEGV, (void (*)()) SIGSEGV_Handler);
#else
  struct sigaction act;

  act.sa_handler = NULL;
  act.sa_sigaction = (void (*)()) SIGSEGV_Handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;

  sigaction(SIGSEGV, &act, NULL);
#endif

  x = (long *) 0xFEA4F124;
  *x = 12;

}
