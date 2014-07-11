#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

FILE *f;

void
Delay_Printf(char *op, char *operands, ...)
{
  char buff[100];
  va_list arg_ptr;

  va_start(arg_ptr, operands);

  sprintf(buff, "\t%s\t", op);
  vsprintf(buff + strlen(buff), operands, arg_ptr);

  va_end(arg_ptr);
  printf("%s\n", buff);
  fprintf(f, "%s\n", buff);
}


void
synthetize_setx(long value, char *dstreg)
{
  int upper32 = value >> 32;
  int lower32 = value;
  char *upper_dstreg = "%g1";	/* a temp reg */
  int need_hh22_p = 0, need_hm10_p = 0, need_hi22_p = 0, need_lo10_p = 0;
  int need_xor10_p = 0;

  /* What to output depends on the number if it's constant.
     Compute that first, then output what we've decided upon.  */
 
  /* Only need hh22 if `or' insn can't handle constant.  */
  if (upper32 < -(1 << 12) || upper32 >= (1 << 12))
    need_hh22_p = 1;

  /* Does bottom part (after sethi) have bits?  */
  if ((need_hh22_p && (upper32 & 0x3ff) != 0)
      /* No hh22, but does upper32 still have bits we can't set
	 from lower32?  */
      || (! need_hh22_p && upper32 != 0 && upper32 != -1))
    need_hm10_p = 1;

  /* If the lower half is all zero, we build the upper half directly
     into the dst reg.  */
  if (lower32 != 0
      /* Need lower half if number is zero or 0xffffffff00000000.  */
      || (! need_hh22_p && ! need_hm10_p))
    {
      /* No need for sethi if `or' insn can handle constant.  */
      if (lower32 < -(1 << 12) || lower32 >= (1 << 12)
	  /* Note that we can't use a negative constant in the `or'
	     insn unless the upper 32 bits are all ones.  */
	  || (lower32 < 0 && upper32 != -1)
	  || (lower32 >= 0 && upper32 == -1))
	need_hi22_p = 1;

      if (need_hi22_p && upper32 == -1)
	need_xor10_p = 1;

      /* Does bottom part (after sethi) have bits?  */
      else if ((need_hi22_p && (lower32 & 0x3ff) != 0)
	       /* No sethi.  */
	       || (! need_hi22_p && (lower32 & 0x1fff) != 0)
	       /* Need `or' if we didn't set anything else.  */
	       || (! need_hi22_p && ! need_hh22_p && ! need_hm10_p))
	need_lo10_p = 1;
    }
  else
    /* Output directly to dst reg if lower 32 bits are all zero.  */
    upper_dstreg = dstreg;


#define MK_IMM22(x)               (((x) >> 10) & 0x3fffff)
#define MK_SIMM10_13(x, want10)  ((x) & ((want10) ?  0x3ff : 0x1fff))

  if (need_hh22_p)
    Delay_Printf("sethi", "%d,%s", MK_IMM22(upper32), upper_dstreg);

  if (need_hi22_p)
    Delay_Printf("sethi", "%d,%s", MK_IMM22(need_xor10_p ? ~lower32 : lower32), dstreg);

  if (need_hm10_p)
    Delay_Printf("or", "%s,%ld,%s", (need_hh22_p ? upper_dstreg : "%g0"), MK_SIMM10_13(upper32, need_hh22_p), upper_dstreg);

  if (need_lo10_p)
    Delay_Printf("or", "%s,%ld,%s", (need_hi22_p ? dstreg : "%g0"), MK_SIMM10_13(lower32, need_hi22_p), dstreg);

  /* If we needed to build the upper part, shift it into place.  */
  if (need_hh22_p || need_hm10_p)
    Delay_Printf("sllx", "%s,32,%s", upper_dstreg, upper_dstreg);

  /* To get -1 in upper32, we do sethi %hi(~x), r; xor r, -0x400 | x, r.  */
  if (need_xor10_p)
    Delay_Printf("xor", "%s,%ld,%s", dstreg, 0x1c00 | (lower32 & 0x3ff), dstreg);
  /* If we needed to build both upper and lower parts, OR them together.  */
  else if ((need_hh22_p || need_hm10_p) && (need_hi22_p || need_lo10_p))
    Delay_Printf("or", "%s,%s,%s", dstreg, upper_dstreg, dstreg);
}

void
as_disass(char *pref)
{
  char buff[100];

  printf("AS-DISAS %s.s\n", pref);
  sprintf(buff, "as -s -K PIC -Av9a -64 -o %s.o %s.s", pref, pref);
  if (system(buff) != 0)
    exit(1);

  sprintf(buff, "cp %s.o z.o", pref); 	/* to have the same object file name at disass */
  system(buff);

  sprintf(buff, "objdump --disassemble z.o >%s-1.s", pref);
  if (system(buff) != 0)
    exit(1);
}


int
main(int argc, char *argv[]) 
{
  long value = strtol(argv[1], NULL, 0);
  printf("Value: %ld   0x%lx\n", value, value);

  f = fopen("x.s", "w");
  
  synthetize_setx(value, "%o1");
  fclose(f);

  printf("\n");


  f = fopen("y.s", "w");
  fprintf(f, "\tsetx %ld,%%g1,%%o1\n", value);
  fclose(f);

  as_disass("x");
  as_disass("y");

  if (system("cmp x-1.s y-1.s") != 0)
    {
      printf("ERROR\n");
      printf("diff x-1.s y-1.s\n");
      system("diff x-1.s y-1.s");
  }
  return 0;
}



