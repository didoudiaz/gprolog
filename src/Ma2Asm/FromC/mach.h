/*-------------------------------------------------------------------------* 
 * Prolog To Wam Compiler                INRIA Rocquencourt - CLoE Project * 
 * C Run-time                                           Daniel Diaz - 1994 * 
 *                                                                         * 
 * Machine Dependent Features - Header file                                * 
 *                                                                         * 
 * machine.h                                                               * 
 *-------------------------------------------------------------------------*/

/*---------------------------------* 
 * Asm Labels, Symbols and Gotos   * 
 *---------------------------------*/

#if defined(M_sony_news) || defined(M_ultrix_dec) || defined(M_alpha_osf) ||\
    defined(M_ix86_linux) || defined(M_ix86_sco) || defined(M_ix86_bsd) ||\
    defined(M_x86_64_linux)

#   define M_Asm_Symbol1(name)     #name
#   define M_Asm_Symbol(name)      M_Asm_Symbol1(name)

#elif defined (M_ix86_win32)

#   define M_Asm_Symbol1(name)     _##name
#   define M_Asm_Symbol(name)      M_Asm_Symbol1(name)

#else

#   define M_Asm_Symbol1(name)     "_"#name
#   define M_Asm_Symbol(name)      M_Asm_Symbol1(name)

#endif



#if defined(M_ix86_win32)

#define M_Indirect_Goto(p_lab) {register long adr=(long) p_lab; _asm jmp adr}

#else

#define M_Indirect_Goto(p_lab) {goto *(void*) p_lab;}

#endif


#if defined(M_sparc_sunos) || defined(M_ultrix_dec) || \
    defined(M_sony_news) ||  defined(M_hppa_NeXT)

#    define M_Direct_Goto(lab)     {lab(); return;}


#elif defined(M_alpha_osf)

#    define M_Direct_Goto(lab)     {asm("lda $28," M_Asm_Symbol(lab));      \
                                    asm("jmp $31,($28)," M_Asm_Symbol(lab));\
                                    return;}

#elif defined(M_ix86_linux) || defined(M_ix86_sco) || defined(M_ix86_bsd)

#    define M_Direct_Goto(lab)     {asm("jmp " M_Asm_Symbol(lab)); return;}

#elif defined(M_ix86_cygwin)

#    define M_Direct_Goto(lab)     {asm("jmp " M_Asm_Symbol(lab)); return;}

#elif defined(M_ix86_win32)

#    define M_Direct_Goto(lab)     {_asm {jmp M_Asm_Symbol(lab)}; return;}

#elif defined(M_x86_64_linux)

#    define M_Direct_Goto(lab)     {asm("jmp " M_Asm_Symbol(lab)); return;}

#elif defined(M_powerpc_linux)

#    define M_Direct_Goto(lab)     {asm("b " M_Asm_Symbol(lab)); return;}

#elif defined(M_m68k_NeXT)

#    define M_Direct_Goto(lab)     {asm("jmp " M_Asm_Symbol(lab)); return;}

#endif




/*---------------------------------* 
 * WAM                             * 
 *---------------------------------*/

#if 0

#if defined(M_ix86_linux) || defined(M_ix86_sco) || defined(M_ix86_bsd)

register WamWord *reg_bank asm("ebx");

#elif defined(M_powerpc_linux)

register WamWord *reg_bank asm("r31");

#elif defined(M_x86_64_linux)

register WamWord *reg_bank asm("r12");

#else

WamWord *reg_bank;

#endif

#endif


