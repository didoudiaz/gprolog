/*-------------------------------------------------------------------------*
 * Prolog To Wam Compiler                INRIA Rocquencourt - CLoE Project *
 * C Run-time                                           Daniel Diaz - 1994 *
 *                                                                         *
 * Machine Dependent Features - header file                                *
 *                                                                         *
 * machine.h                                                               *
 *-------------------------------------------------------------------------*/

/*---------------------------------*
 * Asm Labels, Symbols and Gotos   *
 *---------------------------------*/

#if defined(M_sony_news) || defined(M_ultrix_dec) || defined(M_alpha_osf) ||\
    defined(M_ix86_linux) || defined(M_ix86_sco) || defined(M_ix86_bsd) ||\
    defined(M_x86_64_linux) || defined(m_x86_64_bsd) || \
    defined(M_powerpc_bsd) || defined(M_sparc_bsd) || defined(__ELF__)

#   define M_Asm_Symbol1(name)     #name
#   define M_Asm_Symbol(name)      M_Asm_Symbol1(name)

#elif defined (M_ix86_win32)

#   define M_Asm_Symbol1(name)     _##name
#   define M_Asm_Symbol(name)      M_Asm_Symbol1(name)

#else

#   define M_Asm_Symbol1(name)     "_"#name
#   define M_Asm_Symbol(name)      M_Asm_Symbol1(name)

#endif


#if defined(__clang__) || defined(_MSC_VER) /* indirect goto does not work with clang */

#if defined(M_ix86_win32)

#define Indirect_Goto(p_lab) {register long adr=(long) p_lab; _asm jmp adr;}

#elif defined(M_x86_64)

#define Indirect_Goto(p_lab) __asm__ volatile("jmp *%0" :: "r"(p_lab) : "memory")

#elif defined(M_arm32)

#define Indirect_Goto(p_lab) __asm__ volatile("bx %0" :: "r"(p_lab) : "memory")

#elif defined(M_arm64)

#define Indirect_Goto(p_lab) __asm__ volatile("br %0" :: "r"(p_lab) : "memory")

#elif defined(M_riscv32) || defined(M_riscv64)

#define Indirect_Goto(p_lab) __asm__ volatile("jr %0" :: "r" (p_lab) : "memory")

#elif defined(M_sparc32) || efined(M_sparc64)

#define Indirect_Goto(p_lab) __asm__ volatile("jmp %0\n\tnop" :: "r"(p_lab) : "memory")

#elif defined(M_mips32) || defined(M_mips64)

#define Indirect_Goto(p_lab) __asm__ volatile("jr %0\n\tnop" :: "r"(p_lab) : "memory")

#elif defined(M_ppc32) || defined(M_ppc64)

#define Indirect_Goto(p_lab) __asm__ volatile("mtctr %0\n\tbctr" :: "r"(p_lab) : "ctr","memory")

#elif defined(M_alpha)

#define Indirect_Goto(p_lab) __asm__ volatile("jmp $31,(%0),0" :: "r"(p_lab) : "memory")

#endif

#else /* GCC standard case (the above jmpq should work also) */

#define Indirect_Goto(p_lab) goto *(void*) p_lab

#endif


#if defined(M_sparc_sunos) || defined(M_ultrix_dec) || \
    defined(M_sony_news) ||  defined(M_hppa_NeXT)

#    define Direct_Goto(lab)     {lab(); return;}


#elif defined(M_alpha_osf)

#    define Direct_Goto(lab)     {asm("lda $28," M_Asm_Symbol(lab));      \
                                    asm("jmp $31,($28)," M_Asm_Symbol(lab));\
                                    return;}

#elif defined(M_ix86) && defined(__GNUC__)

#    define Direct_Goto(lab)     {asm("jmp " M_Asm_Symbol(lab)); return;}

#elif defined(_MSC_VER)

#    define Direct_Goto(lab)     {_asm {jmp M_Asm_Symbol(lab)}; return;}

#elif defined(M_x86_64)

#    define Direct_Goto(lab)     {asm("jmp " M_Asm_Symbol(lab)); return;}

#elif defined(M_powerpc_linux) || defined(M_powerpc_bsd)

#    define Direct_Goto(lab)     {asm("b " M_Asm_Symbol(lab)); return;}

#elif defined(M_m68k_NeXT)

#    define Direct_Goto(lab)     {asm("jmp " M_Asm_Symbol(lab)); return;}

#elif defined(M_arm32) || defined(M_arm64)

#    define Direct_Goto(lab)     {asm("b " M_Asm_Symbol(lab)); return;}

#elif defined(M_mips32) || defined(M_riscv64)

#    define Direct_Goto(lab)     {asm("j " M_Asm_Symbol(lab)); return;}

#endif




/*---------------------------------*
 * WAM                             *
 *---------------------------------*/

#if 0

#if defined(M_ix86)

register WamWord *reg_bank asm("ebx");

#elif defined(M_powerpc)

register WamWord *reg_bank asm("r31");

#elif defined(M_x86_64)

register WamWord *reg_bank asm("r12");

#else

WamWord *reg_bank;

#endif

#endif


