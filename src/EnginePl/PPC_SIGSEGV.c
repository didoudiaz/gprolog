#include <signal.h>
#include <stdio.h>

typedef long WamWord;


#if 0
void
SIGSEGV_Handler(int sig)
{
  /* LINUX: recovering the bad address...                       */
  /* There is no documented way to get the address that has     */
  /* caused SIGSEGV. I have looked at the sources of the kernel */
  /* (files from /usr/src/linux/arch/i386/)                     */
  /*                                                            */
  /* mm/fault.c:104:      current->tss.cr2 = address;           */
  /*    a bad address is found by the memory manager.           */
  /*                                                            */
  /* kernel/signal.c:203: put_user(current->tss.cr2, frame+23); */
  /*    a context pointed by 'frame' is created in the stack,   */
  /*    it contains the bad addess at +23.                      */
  /* Inside the handler SIGSEGV_Handler(int sig) the context    */
  /* (frame) can be found at &sig-1.                            */

  long *frame = (long *) &sig - 1;
  WamWord *addr = (WamWord *) (frame[+23]);

  printf("Segmentation Violation at: %lx\n", addr);
  exit(1);
}


#else

#include <asm/sigcontext.h>

void
SIGSEGV_Handler(int sig, struct sigcontext_struct scp)
{
  WamWord *addr = (WamWord *) scp.regs->dar;

  printf("Segmentation Violation at: %lx\n", addr);

  exit(1);
}

#endif


main()
{
  long *x;

  signal(SIGSEGV, (void (*)()) SIGSEGV_Handler);

  x = (long *) 0xffff040;
  *x = 12;

}
