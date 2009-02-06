#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#define BAD_ADDR  0xFEA4F

void
SIGSEGV_Handler(int sig, siginfo_t * sip)
{
  long addr = (long) sip->si_addr;

  exit ((addr == BAD_ADDR) ? 0 : 1);
}



int
main(int argc, char *argv[])
{
  struct sigaction act;

  act.sa_handler = NULL;
  act.sa_sigaction = (void (*)()) SIGSEGV_Handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;

  sigaction(SIGSEGV, &act, NULL);

  * (long *) BAD_ADDR = 128;

  return 1;
}
