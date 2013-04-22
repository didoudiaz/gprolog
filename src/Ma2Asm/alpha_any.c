/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : alpha_any.c                                                     *
 * Descr.: translation file for Linux/OSF on alpha                         *
 * Author: Alexander Diemand, Daniel Diaz                                  *
 *                                                                         *
 * Copyright (C) 1999-2013 Daniel Diaz                                     *
 *                                                                         *
 * This file is part of GNU Prolog                                         *
 *                                                                         *
 * GNU Prolog is free software: you can redistribute it and/or             *
 * modify it under the terms of either:                                    *
 *                                                                         *
 *   - the GNU Lesser General Public License as published by the Free      *
 *     Software Foundation; either version 3 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or                                                                      *
 *                                                                         *
 *   - the GNU General Public License as published by the Free             *
 *     Software Foundation; either version 2 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or both in parallel, as here.                                           *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful,           *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received copies of the GNU General Public License and   *
 * the GNU Lesser General Public License along with this program.  If      *
 * not, see http://www.gnu.org/licenses/.                                  *
 *-------------------------------------------------------------------------*/


#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define STRING_PREFIX              "$LC"

#define MAX_C_ARGS_IN_C_CODE       32

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char asm_reg_bank[10];
char asm_reg_e[10];
char asm_reg_b[10];
char asm_reg_cp[10];

int w_label = 0;

char dbl_arg_buffer[8192] = "\0";	/* a temp buffer for the double arguments */

char act_routine[512] = "\0";	/* remembers the actual routine we are building */

int inPrologCode = 0;	/* whether we are currently compiling a prolog code */

	  /* variables for ma_parser.c / ma2asm.c */

int can_produce_pic_code = 0;
char *comment_prefix = "#";
char *local_symb_prefix = "$";
int strings_need_null = 1;
int call_c_reverse_args = 0;

char *inline_asm_data[] = { NULL };




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/



/*-------------------------------------------------------------------------*
 * INLINED CODE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
/* all %s will be replaced with the function's name
 * all %d will be replaced with the current nb_inlines
 */
static long nb_inlines = 0;
static char *def_inlines[] = {
  /* name            code */
  "Put_X_Variable", "   # %s inlined\n\
                     s8addq  $10,0,$0    # Make_Self_Ref(H)            \n\
                     or      $0,1,$0                                   \n\
                     stq     $0,0($10)   # Global_Push                 \n\
                     addq    $10,8,$10                                 \n",

  "Put_Y_Variable", "   # %s inlined\n\
                     s8addq  $16,0,$0    # Make_Self_Ref(y_adr)        \n\
                     or      $0,1,$0                                   \n\
                     stq     $0,0($16)   # save in *y_adr              \n",

  "Put_Atom", "   # %s inlined\n\
                     s8addq  $16,0,$0    # Tag_ATM(n)            \n\
                     or      $0,3,$0                                   \n",

  "Put_Integer", "   # %s inlined\n\
                     s8addq  $16,0,$0    # Tag_INT(n)            \n",

  "Put_Float", "   # %s inlined\n\
                     s8addq  $10,8,$0    # res_word = Tag_FLT(H) \n\
                     stt     $f16,0($10) # Global_Push_Float(n)        \n\
                     or      $0,4,$0                                   \n\
                     addq    $10,8,$10                                 \n",

  "Put_Nil", "   # %s inlined\n\
                     lda     $0,14131                                  \n",

  "Put_List", "   # %s inlined\n\
                     s8addq  $10,0,$0    # Tag_LST(H)            \n\
                     stq     $31,2056($9) # S = 0 (WriteMode)          \n\
                     or      $0,5,$0                                   \n",

  "Put_Structure", "   # %s inlined\n\
                     sll     $17,0x10,$17                              \n\
                     addl    $17,$16,$17                               \n\
                     s8addq  $10,0,$0                                  \n\
                     or      $0,0x6,$0                                 \n\
                     stq     $17,0($10)                                \n\
                     addq    $10,0x8,$10                               \n\
                     stq     $31,2056($9)                              \n",


  "Cut", "   # %s inlined\n\
                     srl     $16,3,$11                                 \n",

  "Switch_On_Integer", "   # %s inlined\n\
                     ldq     $0,0($9)                                  \n\
                     srl     $0,3,$0                                   \n",

  "Switch_On_Term", "   # %s inlined\n\
                     ldq     $1,0($9)    # deref(A(0),word,tag,adr)    \n\
                     mov     $1,$2       # word = A(0)                 \n\
                     clr     $5          # working_adr                 \n\
                    %s_1_%d:                                           \n\
                     and     $2,7,$3     # Tag_Of                      \n\
                     cmpeq   $3,1,$4     # REF?                        \n\
                     beq     $4,%s_2_%d  # no -> break                 \n\
                     srl     $2,3,$6     # UnTag_REF(word)             \n\
                     cmpeq   $5,$6,$4    # working_adr == adr          \n\
                     bne     $4,%s_2_%d  # yes -> break                \n\
                     ldq     $2,0($6)                                  \n\
                     mov     $6,$5                                     \n\
                     br      %s_1_%d                                   \n\
                                                                       \n\
                    %s_2_%d:                                           \n\
                     stq     $2,0($9)    # A(0) = word                 \n\
                     cmoveq  $3,$18,$0   # move c_int to return if tag == (INT = 0)\n\
                     beq     $3,%s_3_%d                                \n\
                     subq    $3,3,$4                                   \n\
                     cmoveq  $4,$17,$0   # move c_atm to return if tag == (ATM = 3)\n\
                     beq     $4,%s_3_%d                                \n\
                     subq    $3,5,$4                                   \n\
                     cmoveq  $4,$19,$0   # move c_lst to return if tag == (LST = 5)\n\
                     beq     $4,%s_3_%d                                \n\
                     subq    $3,6,$4                                   \n\
                     cmoveq  $4,$20,$0   # move c_stc to return if tag == (STC = 6)\n\
                     beq     $4,%s_3_%d                                \n\
                     mov     $16,$0      # for all the rest            \n\
                                                                       \n\
                    %s_3_%d:                                           \n\
                     bne $0,%s_4_%d                                    \n\
                     ldq $0,-8($11)      # return ALTB(B)              \n\
                    %s_4_%d:                                           \n",


  /*
     00000001200d2c40 <Switch_On_Structure>:
     1200d2c40:   11 00 bb 27     ldah    gp,17(t12)
     1200d2c44:   10 2f bd 23     lda     gp,12048(gp)
     1200d2c48:   00 00 29 a4     ldq     t0,0(s0)
     1200d2c4c:   19 30 20 42     addl    a1,0x1,t11
     1200d2c50:   22 97 20 4b     sll     t11,0x4,t1
     1200d2c54:   81 76 20 48     srl     t0,0x3,t0
     1200d2c58:   04 04 02 42     addq    a0,t1,t3
     1200d2c5c:   00 00 61 a4     ldq     t2,0(t0)
     1200d2c60:   18 04 63 44     mov     t2,t10
     1200d2c64:   28 b9 7d a7     ldq     t12,-18136(gp)
     1200d2c68:   9e 6b fb 6a     jsr     t9,(t12),1200cdae4 <Scan_Quoted+0x504>
     1200d2c6c:   11 00 b7 27     ldah    gp,17(t9)
     1200d2c70:   e4 2e bd 23     lda     gp,12004(gp)
     1200d2c74:   1b 00 7f 43     addl    t12,zero,t12
     1200d2c78:   3b 97 60 4b     sll     t12,0x4,t12
     1200d2c7c:   02 04 1b 42     addq    a0,t12,t1
     1200d2c80:   08 00 02 a4     ldq     v0,8(t1)
     1200d2c84:   0a 00 00 e4     beq     v0,1200d2cb0 <Switch_On_Structure+0x70>
     1200d2c88:   1f 04 ff 47     nop
     1200d2c8c:   00 00 e0 2f     unop
     1200d2c90:   00 00 22 a4     ldq     t0,0(t1)
     1200d2c94:   a1 05 23 40     cmpeq   t0,t2,t0
     1200d2c98:   05 00 20 f4     bne     t0,1200d2cb0 <Switch_On_Structure+0x70>
     1200d2c9c:   02 14 42 40     addq    t1,0x10,t1
     1200d2ca0:   a1 05 44 40     cmpeq   t1,t3,t0
     1200d2ca4:   c2 04 30 44     cmovne  t0,a0,t1
     1200d2ca8:   08 00 02 a4     ldq     v0,8(t1)
     1200d2cac:   f8 ff 1f f4     bne     v0,1200d2c90 <Switch_On_Structure+0x50>
     1200d2cb0:   01 00 00 f4     bne     v0,1200d2cb8 <Switch_On_Structure+0x78>
     1200d2cb4:   f8 ff 0b a4     ldq     v0,-8(s2)
     1200d2cb8:   01 80 fa 6b     ret     zero,(ra),0x1
     1200d2cbc:   00 00 e0 2f     unop
   */
  /*
     00000001200d2ba0 <Switch_On_Atom>:
     1200d2ba0:   11 00 bb 27     ldah    gp,17(t12)
     1200d2ba4:   b0 2f bd 23     lda     gp,12208(gp)
     1200d2ba8:   00 00 29 a4     ldq     t0,0(s0)
     1200d2bac:   19 30 20 42     addl    a1,0x1,t11
     1200d2bb0:   22 97 20 4b     sll     t11,0x4,t1
     1200d2bb4:   81 76 20 48     srl     t0,0x3,t0
     1200d2bb8:   04 04 02 42     addq    a0,t1,t3
     1200d2bbc:   23 f6 21 48     zapnot  t0,0xf,t2
     1200d2bc0:   18 04 63 44     mov     t2,t10
     1200d2bc4:   28 b9 7d a7     ldq     t12,-18136(gp)
     1200d2bc8:   c6 6b fb 6a     jsr     t9,(t12),1200cdae4 <Scan_Quoted+0x504>
     1200d2bcc:   11 00 b7 27     ldah    gp,17(t9)
     1200d2bd0:   84 2f bd 23     lda     gp,12164(gp)
     1200d2bd4:   1b 00 7f 43     addl    t12,zero,t12
     1200d2bd8:   3b 97 60 4b     sll     t12,0x4,t12
     1200d2bdc:   02 04 1b 42     addq    a0,t12,t1
     1200d2be0:   08 00 02 a4     ldq     v0,8(t1)
     1200d2be4:   0a 00 00 e4     beq     v0,1200d2c10 <Switch_On_Atom+0x70>
     1200d2be8:   1f 04 ff 47     nop
     1200d2bec:   00 00 e0 2f     unop
     1200d2bf0:   00 00 22 a4     ldq     t0,0(t1)
     1200d2bf4:   a1 05 23 40     cmpeq   t0,t2,t0
     1200d2bf8:   05 00 20 f4     bne     t0,1200d2c10 <Switch_On_Atom+0x70>
     1200d2bfc:   02 14 42 40     addq    t1,0x10,t1
     1200d2c00:   a1 05 44 40     cmpeq   t1,t3,t0
     1200d2c04:   c2 04 30 44     cmovne  t0,a0,t1
     1200d2c08:   08 00 02 a4     ldq     v0,8(t1)
     1200d2c0c:   f8 ff 1f f4     bne     v0,1200d2bf0 <Switch_On_Atom+0x50>
     1200d2c10:   01 00 00 f4     bne     v0,1200d2c18 <Switch_On_Atom+0x78>
     1200d2c14:   f8 ff 0b a4     ldq     v0,-8(s2)
     1200d2c18:   01 80 fa 6b     ret     zero,(ra),0x1
     1200d2c1c:   00 00 e0 2f     unop
   */
  "Unify_Variable", "   # %s inlined\n\
                     ldq     $0,2056($9) # S == 0 (WriteMode)?         \n\
                     mov     $0,$1                                     \n\
                     beq     $0,%s_1_%d                                \n\
                                                                       \n\
                     # NOT WRITE_MODE                                  \n\
                     ldq     $0,0($1)    # word = *S                   \n\
                     and     $0,7,$2     # tag = Tag_Of(word= *S)      \n\
                     addq    $1,8,$1                                   \n\
                     stq     $1,2056($9) # S++                         \n\
                                                                       \n\
                     # Make_Copy_Of_Word(tag=$2,word=$0)               \n\
                     cmpeq   $2,2,$2     # Dont_Separate_Tag(tag)      \n\
                     beq     $2,%s_99_%d # false                       \n\
                                                                       \n\
                     andnot  $0,7,$1     # adr=UnTag_Ref(word)         \n\
                     or      $1,1,$0     # word=Tag_REF(adr)     \n\
                                                                       \n\
                     br      %s_99_%d                                  \n\
                                                                       \n\
                     # WRITE_MODE                                      \n\
                    %s_1_%d:                                           \n\
                     s8addq  $10,0,$0    # word = Make_Self_Ref(H)     \n\
                     or      $0,1,$0                                   \n\
                     stq     $0,0($10)   # Global_Push(word)           \n\
                     addq    $10,8,$10                                 \n\
                                                                       \n\
                    %s_99_%d:                                          \n\
                                         # continue                    \n",
  "Unify_Void", "   # %s inlined\n\
                     ldq     $0,2056($9) # S == 0 (WriteMode)?         \n\
                     mov     $0,$1                                     \n\
                     beq     $0,%s_1_%d                                \n\
                                                                       \n\
                     # NOT WRITE_MODE                                  \n\
                     s8addq  $16,$1,$1                                 \n\
                     stq     $1,2056($9) # S = S + n                   \n\
                     br      %s_99_%d                                  \n\
                                                                       \n\
                     # WRITE_MODE                                      \n\
                    %s_1_%d:                                           \n\
                     s8addq  $16,$10,$16 # end_adr = H+n               \n\
                     cmpult  $10,$16,$1                                \n\
                     beq     $1,%s_99_%d # H < end_adr                 \n\
                    %s_2_%d:                                           \n\
                     s8addq  $10,0,$1                                  \n\
                     or      $1,1,$1                                   \n\
                     stq     $1,0($10)   # ++H                         \n\
                     addq    $10,8,$10                                 \n\
                     cmpult  $10,$16,$1                                \n\
                     bne     $1,%s_2_%d  # H < end_adr                 \n\
                                                                       \n\
                    %s_99_%d:                                          \n\
                                         # continue                    \n",

  "Create_Choice_Point", "   # %s inlined\n\
                     cmpule  $11,$14,$2  # Local_Top                   \n\
                     mov     $14,$3                                    \n\
                     cmoveq  $2,$11,$3                                 \n\
                     mov     $11,$4      # adr = B                     \n\
                     s8addq  $17,64,$1   # +CHOICE_STATIC_SIZE+arity   \n\
                     addq    $3,$1,$11   # -> B                        \n\
                                                                       \n\
                     stq     $16,-8($11) # ALTB(B) = codep_alt         \n\
                     stq     $13,-16($11)# CPB(B) = CP                 \n\
                     cmpeq   $31,$17,$3  # arity == 0?                 \n\
                     ldq     $1,2072($9) # BCI                         \n\
                     stq     $1,-24($11) # BCIB(B) = BCI               \n\
                     stq     $14,-32($11)# EB(B) = E                   \n\
                     stq     $4,-40($11) # BB(B) = adr                 \n\
                     stq     $10,-48($11)# HB(B) = H                   \n\
                     stq     $12,-56($11)# TRB(B) = TR                 \n\
                     clr     $2                                        \n\
                     ldq     $1,2048($9) # CS                          \n\
                     lda     $4,-9                                     \n\
                     stq     $1,-64($11) # CSB(B) = CS                 \n\
                     bne     $3,%s_2_%d  # skip                        \n\
                                                                       \n\
                    %s_1_%d:                                           \n\
                     s8addq  $2,$9,$1                                  \n\
                     subl    $4,$2,$6                                  \n\
                     ldq     $5,0($1)    # A(i)                        \n\
                     s8addq  $6,$11,$0                                 \n\
                     addq    $2,1,$2     # i++                         \n\
                     stq     $5,0($0)                                  \n\
                     cmplt   $2,$17,$3   # i<arity                     \n\
                     bne     $3,%s_1_%d                                \n\
                                                                       \n\
                    %s_2_%d:                                           \n\
                     ldq     $1,2064($9)                               \n\
                     addq    $1,1,$1                                   \n\
                     stq     $1,2064($9)                               \n",


/*
00000001200d2dc0 <Update_Choice_Point>:
   1200d2dc0:   11 00 bb 27     ldah    gp,17(t12)
   1200d2dc4:   90 2d bd 23     lda     gp,11664(gp)
   1200d2dc8:   3e 15 c2 43     subq    sp,0x10,sp
   1200d2dcc:   08 00 fe b5     stq     fp,8(sp)
   1200d2dd0:   00 00 5e b7     stq     ra,0(sp)
   1200d2dd4:   0f 04 31 46     mov     a1,fp
   1200d2dd8:   f8 ff 0b b6     stq     a0,-8(s2)
   1200d2ddc:   c8 ff 0b a6     ldq     a0,-56(s2)
   1200d2de0:   60 b9 7d a7     ldq     t12,-18080(gp)
   1200d2de4:   46 40 5b 6b     jsr     ra,(t12),1200d2f00 <Untrail>
   1200d2de8:   11 00 ba 27     ldah    gp,17(ra)
   1200d2dec:   68 2d bd 23     lda     gp,11624(gp)
   1200d2df0:   04 04 ff 47     clr     t3
   1200d2df4:   e8 ff 4b 8d     ldt     $f10,-24(s2)
   1200d2df8:   a1 09 ef 43     cmplt   zero,fp,t0
   1200d2dfc:   f0 ff ab a5     ldq     s4,-16(s2)
   1200d2e00:   18 08 49 9d     stt     $f10,2072(s0)
   1200d2e04:   c0 ff 6b 8d     ldt     $f11,-64(s2)
   1200d2e08:   e0 ff cb a5     ldq     s5,-32(s2)
   1200d2e0c:   d0 ff 4b a5     ldq     s1,-48(s2)
   1200d2e10:   00 08 69 9d     stt     $f11,2048(s0)
   1200d2e14:   0a 00 20 e4     beq     t0,1200d2e40 <Update_Choice_Point+0x80>
   1200d2e18:   f7 ff bf 20     lda     t4,-9(zero)
   1200d2e1c:   00 00 e0 2f     unop
   1200d2e20:   21 01 a4 40     subl    t4,t3,t0
   1200d2e24:   41 06 2b 40     s8addq  t0,s2,t0
   1200d2e28:   43 06 89 40     s8addq  t3,s0,t2
   1200d2e2c:   00 00 41 8d     ldt     $f10,0(t0)
   1200d2e30:   04 30 80 40     addl    t3,0x1,t3
   1200d2e34:   a2 09 8f 40     cmplt   t3,fp,t1
   1200d2e38:   00 00 43 9d     stt     $f10,0(t2)
   1200d2e3c:   f8 ff 5f f4     bne     t1,1200d2e20 <Update_Choice_Point+0x60>
   1200d2e40:   00 00 5e a7     ldq     ra,0(sp)
   1200d2e44:   08 00 fe a5     ldq     fp,8(sp)
   1200d2e48:   1e 14 c2 43     addq    sp,0x10,sp
   1200d2e4c:   01 80 fa 6b     ret     zero,(ra),0x1
   1200d2e50:   1f 04 ff 47     nop
*/
/*
00000001200d2e60 <Delete_Choice_Point>:
   1200d2e60:   11 00 bb 27     ldah    gp,17(t12)
   1200d2e64:   f0 2c bd 23     lda     gp,11504(gp)
   1200d2e68:   3e 15 c2 43     subq    sp,0x10,sp
   1200d2e6c:   08 00 fe b5     stq     fp,8(sp)
   1200d2e70:   00 00 5e b7     stq     ra,0(sp)
   1200d2e74:   0f 04 10 46     mov     a0,fp
   1200d2e78:   c8 ff 0b a6     ldq     a0,-56(s2)
   1200d2e7c:   60 b9 7d a7     ldq     t12,-18080(gp)
   1200d2e80:   1f 40 5b 6b     jsr     ra,(t12),1200d2f00 <Untrail>
   1200d2e84:   11 00 ba 27     ldah    gp,17(ra)
   1200d2e88:   cc 2c bd 23     lda     gp,11468(gp)
   1200d2e8c:   04 04 ff 47     clr     t3
   1200d2e90:   c0 ff 4b 8d     ldt     $f10,-64(s2)
   1200d2e94:   a1 09 ef 43     cmplt   zero,fp,t0
   1200d2e98:   f0 ff ab a5     ldq     s4,-16(s2)
   1200d2e9c:   e0 ff cb a5     ldq     s5,-32(s2)
   1200d2ea0:   d0 ff 4b a5     ldq     s1,-48(s2)
   1200d2ea4:   00 08 49 9d     stt     $f10,2048(s0)
   1200d2ea8:   e8 ff 6b 8d     ldt     $f11,-24(s2)
   1200d2eac:   18 08 69 9d     stt     $f11,2072(s0)
   1200d2eb0:   0b 00 20 e4     beq     t0,1200d2ee0 <Delete_Choice_Point+0x80>
   1200d2eb4:   f7 ff bf 20     lda     t4,-9(zero)
   1200d2eb8:   1f 04 ff 47     nop
   1200d2ebc:   00 00 e0 2f     unop
   1200d2ec0:   21 01 a4 40     subl    t4,t3,t0
   1200d2ec4:   41 06 2b 40     s8addq  t0,s2,t0
   1200d2ec8:   43 06 89 40     s8addq  t3,s0,t2
   1200d2ecc:   00 00 41 8d     ldt     $f10,0(t0)
   1200d2ed0:   04 30 80 40     addl    t3,0x1,t3
   1200d2ed4:   a2 09 8f 40     cmplt   t3,fp,t1
   1200d2ed8:   00 00 43 9d     stt     $f10,0(t2)
   1200d2edc:   f8 ff 5f f4     bne     t1,1200d2ec0 <Delete_Choice_Point+0x60>
   1200d2ee0:   10 08 29 a4     ldq     t0,2064(s0)
   1200d2ee4:   d8 ff 6b a5     ldq     s2,-40(s2)
   1200d2ee8:   21 35 20 40     subq    t0,0x1,t0
   1200d2eec:   10 08 29 b4     stq     t0,2064(s0)
   1200d2ef0:   00 00 5e a7     ldq     ra,0(sp)
   1200d2ef4:   08 00 fe a5     ldq     fp,8(sp)
   1200d2ef8:   1e 14 c2 43     addq    sp,0x10,sp
   1200d2efc:   01 80 fa 6b     ret     zero,(ra),0x1
 */
  "Allocate", "   # %s inlined\n\
                     cmpule  $11,$14,$1  # Local_Top                   \n\
                     mov     $14,$2                                    \n\
                     cmoveq  $1,$11,$2                                 \n\
                     mov     $14,$3      # adr = E                     \n\
                     ldq     $4,2072($9) # BCIE(E) = BCI               \n\
                     s8addq  $16,24,$16  # +ENVIR_STATIC_SIZE+3        \n\
                     addq    $2,$16,$14  # -> E                        \n\
                                                                       \n\
                     stq     $13,-8($14) # CPE(E) = CP                 \n\
                     #ldq     $4,2072($9) # BCIE(E) = BCI               \n\
                     stq     $3,-24($14) # EE(E) = adr                 \n\
                     stq     $4,-16($14)                               \n",

  "Deallocate", "   # %s inlined\n\
                     ldq     $13,-8($14) # CP=CPE(E)                   \n\
                     ldq     $1,-16($14) # BCIE(E)                     \n\
                     stq     $1,2072($9) # BCI=BCIE(E)($1)             \n\
                     ldq     $14,-24($14) # E=EE(E)                    \n",

  0, 0				/* end of list */
};



/*-------------------------------------------------------------------------*
 * MAKE_INLINE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
/* when it finds a function to inline it will do so immediatly and return 1
 * else it fails and returns 0
 */
static int
make_inline(char *fct_name, int nb_args)
{
  char *fp;
  int counter;

  /* user can set an environment variable to control this */
  if (!getenv("GPROLOG_ASM_INLINE"))
    return 0;

  counter = 0;
  while (def_inlines[counter])
    {
      if (strcmp(fct_name, def_inlines[counter]) == 0)
	{
	  /* found code to inline, emit */
	  fp = def_inlines[++counter];
	  while (*fp != '\0')
	    {
	      if (*fp == '%' && *(fp + 1) == 's')
		{
		  String_Out(fct_name);
		  fp++;
		}
	      else if (*fp == '%' && *(fp + 1) == 'd')
		{
		  Int_Out(nb_inlines);
		  fp++;
		}
	      else
		{
		  Char_Out(*fp);
		}
	      fp++;
	    }
	  nb_inlines++;
	  return 1;
	}
      counter++;
    }
  return 0;
}




/*-------------------------------------------------------------------------*
 * ASM_START                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Start(void)
{
#ifdef MAP_REG_BANK
  sprintf(asm_reg_bank, "%s", MAP_REG_BANK);
#else
  strcpy(asm_reg_bank, "$9");
#endif
  Inst_Printf("# asm_reg_bank ", asm_reg_bank);

#ifdef MAP_REG_E
  sprintf(asm_reg_e, "%s", MAP_REG_E);
#else
  sprintf(asm_reg_e, "%d(%s)", MAP_OFFSET_E, asm_reg_bank);
#endif
  Inst_Printf("# REG_E ", asm_reg_e);

#ifdef MAP_REG_B
  sprintf(asm_reg_b, "%s", MAP_REG_B);
#else
  sprintf(asm_reg_b, "%d(%s)", MAP_OFFSET_B, asm_reg_bank);
#endif
  Inst_Printf("# REG_B ", asm_reg_b);

#ifdef MAP_REG_CP
  sprintf(asm_reg_cp, "%s", MAP_REG_CP);
#else
  sprintf(asm_reg_cp, "%d(%s)", MAP_OFFSET_CP, asm_reg_bank);
#endif
  Inst_Printf("# REG_CP ", asm_reg_cp);

  Inst_Printf(".set", "noat");
  Inst_Printf(".set", "noreorder");

  Label_Printf(".text");

}




/*-------------------------------------------------------------------------*
 * ASM_STOP                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Stop(void)
{
  /* we are printing the fixed doubles at the end of the file,
   * they will appear in the data section */
  if (dbl_arg_buffer[0] != '\0')
    {
#ifdef M_alpha_linux
      Label_Printf(".section\t.rodata");
#else
      Label_Printf(".rdata");
#endif
      Label_Printf(dbl_arg_buffer);
      dbl_arg_buffer[0] = '\0';
    }
}




/*-------------------------------------------------------------------------*
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(char *label, int prolog, int global)
{

  if (act_routine[0] != '\0')
    Code_Stop();		/* we first have to close the previous code */

  Inst_Printf(".align", "5");
  if (global)
    Inst_Printf(".globl", "%s", label);
  Inst_Printf(".ent", "%s", label);

  Label(label);

  /* remember this label */
  strcpy(act_routine, label);

  if (prolog)
    {
      /* prolog code does not need any stack space */
      inPrologCode = 1;
      Inst_Printf(".frame", "$30,0,$26,0");
      Inst_Printf(".mask", "0x4000000,0");
      Inst_Printf("ldgp", "$gp,0($27)");
      Inst_Printf(".prologue", "1");
    }
  else
    {
      /* for c code we need to save some registers */
      inPrologCode = 0;
      Inst_Printf(".frame", "$30,32,$26,0");
      Inst_Printf(".mask", "0x4008000,-32");
      Inst_Printf("ldgp", "$gp,0($27)");
      Inst_Printf("subq", "$30,32,$30");
      Inst_Printf("stq", "$26,0($30)");
      Inst_Printf("stq", "$15,8($30)");
      Inst_Printf(".prologue", "1");
    }
}




/*-------------------------------------------------------------------------*
 * CODE_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Stop(void)
{
  Inst_Printf(".end", "%s", act_routine);

  act_routine[0] = '\0';
}




/*-------------------------------------------------------------------------*
 * LABEL                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Label(char *label)
{
  Label_Printf("\n%s:", label);
}




/*-------------------------------------------------------------------------*
 * RELOAD_E_IN_REGISTER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reload_E_In_Register(void)
{
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
#ifdef M_alpha_linux		/* also works for OSF but 'as' warns */
  Inst_Printf("jmp", "$31,%s", label);	/* about macro using $at */
#else
  Inst_Printf("lda", "$27,%s", label);
  Inst_Printf("jmp", "$31,($27),%s", label);
#endif
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
#ifdef MAP_REG_CP
  Inst_Printf("lda", "%s,$Lcont%d", asm_reg_cp, w_label);	/* CP = $Lcont%d */
#else
  Inst_Printf("lda", "$4,$Lcont%d", w_label);	/* CP = $Lcont%d */
  Inst_Printf("stq", "$4,%s", asm_reg_cp);
#endif
}




/*-------------------------------------------------------------------------*
 * HERE_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Here_CP(void)
{
  Label_Printf("$Lcont%d:", w_label++);

  Inst_Printf("ldgp","$gp,0($27)");
}




/*-------------------------------------------------------------------------*
 * PL_CALL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Call(char *label)
{
  Prep_CP();
  Pl_Jump(label);
  Here_CP();
}




/*-------------------------------------------------------------------------*
 * PL_FAIL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fail(void)
{
#ifdef MAP_REG_B
  Inst_Printf("ldq", "$27,-8(%s)", asm_reg_b);
#else
  Inst_Printf("ldq", "$4,%s", asm_reg_b);
  Inst_Printf("ldq", "$27,-8($4)");
#endif

  Inst_Printf("jmp", "$31,($27),0");
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
#ifdef MAP_REG_CP
  Inst_Printf("mov", "%s,$27", asm_reg_cp);	/* make a copy of it in $27 */
#else
  Inst_Printf("ldq", "$27,%s", asm_reg_cp);
#endif
  Inst_Printf("jmp", "$31,($27),0");	/* jump to CP */
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("lda", "$3,%s", label);
  Inst_Printf("jmp", "$31,($3),%s", label);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("ldq", "$1,%d(%s)", 8 * index, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
#ifdef MAP_REG_E
  Inst_Printf("ldq", "$1,%d(%s)", Y_OFFSET(index), asm_reg_e);
#else
  Inst_Printf("ldq", "$4,%s", asm_reg_e);
  Inst_Printf("ldq", "$1,%d($4)", Y_OFFSET(index));
#endif

}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("stq", "$1,%d(%s)", 8 * index, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
#ifdef MAP_REG_E
  Inst_Printf("stq", "$1,%d(%s)", Y_OFFSET(index), asm_reg_e);
#else
  Inst_Printf("ldq", "$4,%s", asm_reg_e);
  Inst_Printf("stq", "$1,%d($4)", Y_OFFSET(index));
#endif
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, int fc, int nb_args, int nb_args_in_words,
	     char **p_inline)
{
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, PlLong int_val)
{
  switch (offset)
    {
    case 0:
      Inst_Printf("lda", "$16,%ld", int_val);
      break;
    case 1:
      Inst_Printf("lda", "$17,%ld", int_val);
      break;
    case 2:
      Inst_Printf("lda", "$18,%ld", int_val);
      break;
    case 3:
      Inst_Printf("lda", "$19,%ld", int_val);
      break;
    case 4:
      Inst_Printf("lda", "$20,%ld", int_val);
      break;
    case 5:
      Inst_Printf("lda", "$21,%ld", int_val);
      break;
    default:
      Inst_Printf("lda", "$1,%ld", int_val);
      Inst_Printf("stq", "$1,%d($30)", (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_DOUBLE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Double(int offset, double dbl_val)
{
  char buf[1024];

  sprintf(buf, "\t.align 3\n$LD%d:\n\t.t_floating %1.20e\n", w_label++,
	  dbl_val);
  strcat(dbl_arg_buffer, buf);
  Inst_Printf("lda", "$1,$LD%d", (w_label - 1));
  switch (offset)
    {
    case 0:
      Inst_Printf("ldt", "$f16,0($1)");
      break;
    case 1:
      Inst_Printf("ldt", "$f17,0($1)");
      break;
    case 2:
      Inst_Printf("ldt", "$f18,0($1)");
      break;
    case 3:
      Inst_Printf("ldt", "$f19,0($1)");
      break;
    case 4:
      Inst_Printf("ldt", "$f20,0($1)");
      break;
    case 5:
      Inst_Printf("ldt", "$f21,0($1)");
      break;
    default:
      Inst_Printf("ldt", "$f1,0($1)");
      Inst_Printf("stt", "$f1,%d($30)", (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, int str_no)
{
  switch (offset)
    {
    case 0:
      Inst_Printf("lda", "$16,%s%d", STRING_PREFIX, str_no);
      break;
    case 1:
      Inst_Printf("lda", "$17,%s%d", STRING_PREFIX, str_no);
      break;
    case 2:
      Inst_Printf("lda", "$18,%s%d", STRING_PREFIX, str_no);
      break;
    case 3:
      Inst_Printf("lda", "$19,%s%d", STRING_PREFIX, str_no);
      break;
    case 4:
      Inst_Printf("lda", "$20,%s%d", STRING_PREFIX, str_no);
      break;
    case 5:
      Inst_Printf("lda", "$21,%s%d", STRING_PREFIX, str_no);
      break;
    default:
      Inst_Printf("lda", "$1,%s%d", STRING_PREFIX, str_no);
      Inst_Printf("stq", "$1,%d($30)", (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_MEM_L                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Mem_L(int offset, int adr_of, char *name, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$16");
      break;
    case 1:
      sprintf(dest, "%s", "$17");
      break;
    case 2:
      sprintf(dest, "%s", "$18");
      break;
    case 3:
      sprintf(dest, "%s", "$19");
      break;
    case 4:
      sprintf(dest, "%s", "$20");
      break;
    case 5:
      sprintf(dest, "%s", "$21");
      break;
    default:
      sprintf(dest, "%s", "$1");
      break;
    }

  if (!adr_of)
    {
      Inst_Printf("lda", "$2,%s", name);
      Inst_Printf("ldq", "%s,%d($2)", dest, index * 8);
    }
  else
    {
      Inst_Printf("lda", "%s,%s+%d", dest, name, index * 8);
    }
  if (offset > 5)
    {
      Inst_Printf("stq", "%s,%d($30)", dest, (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_X                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_X(int offset, int adr_of, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$16");
      break;
    case 1:
      sprintf(dest, "%s", "$17");
      break;
    case 2:
      sprintf(dest, "%s", "$18");
      break;
    case 3:
      sprintf(dest, "%s", "$19");
      break;
    case 4:
      sprintf(dest, "%s", "$20");
      break;
    case 5:
      sprintf(dest, "%s", "$21");
      break;
    default:
      sprintf(dest, "%s", "$1");
      break;
    }

  if (!adr_of)
    {
      Inst_Printf("ldq", "%s,%d(%s)", dest, index * 8, asm_reg_bank);
    }
  else
    {
      if (index == 0)
	{
	  Inst_Printf("mov", "%s,%s", asm_reg_bank, dest);
	}
      else
	{
	  Inst_Printf("lda", "%s,%d(%s)", dest, index * 8, asm_reg_bank);
	}
    }
  if (offset > 5)
    {
      Inst_Printf("stq", "%s,%d($30)", dest, (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_Y                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_Y(int offset, int adr_of, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$16");
      break;
    case 1:
      sprintf(dest, "%s", "$17");
      break;
    case 2:
      sprintf(dest, "%s", "$18");
      break;
    case 3:
      sprintf(dest, "%s", "$19");
      break;
    case 4:
      sprintf(dest, "%s", "$20");
      break;
    case 5:
      sprintf(dest, "%s", "$21");
      break;
    default:
      sprintf(dest, "%s", "$1");
      break;
    }

  if (!adr_of)
    {
#ifdef MAP_REG_E
      Inst_Printf("ldq", "%s,%d(%s)", dest, Y_OFFSET(index), asm_reg_e);
#else
      Inst_Printf("ldq", "$4,%s", asm_reg_e);
      Inst_Printf("ldq", "%s,%d($4)", dest, Y_OFFSET(index));
#endif
    }
  else
    {
#ifdef MAP_REG_E
      Inst_Printf("lda", "%s,%d(%s)", dest, Y_OFFSET(index), asm_reg_e);
#else
      Inst_Printf("ldq", "$4,%s", asm_reg_e);
      Inst_Printf("lda", "%s,%d($4)", dest, Y_OFFSET(index));
#endif
    }
  if (offset > 5)
    {
      Inst_Printf("stq", "%s,%d($30)", dest, (offset - 6) * 8);
    }

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_L                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_L(int offset, int adr_of, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$16");
      break;
    case 1:
      sprintf(dest, "%s", "$17");
      break;
    case 2:
      sprintf(dest, "%s", "$18");
      break;
    case 3:
      sprintf(dest, "%s", "$19");
      break;
    case 4:
      sprintf(dest, "%s", "$20");
      break;
    case 5:
      sprintf(dest, "%s", "$21");
      break;
    default:
      sprintf(dest, "%s", "$1");
      break;
    }

  Inst_Printf("lda", "$2,pl_foreign_long");
  if (!adr_of)
    {
      Inst_Printf("ldq", "%s,%d($2)", dest, index * 8);
    }
  else
    {
      Inst_Printf("lda", "%s,%d($2)", dest, index * 8);
    }
  if (offset > 5)
    {
      Inst_Printf("stq", "%s,%d($30)", dest, (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_D                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_D(int offset, int adr_of, int index)
{
  char dest[8];

  if (adr_of)
    {
      switch (offset)
	{
	case 0:
	  sprintf(dest, "%s", "$16");
	  break;
	case 1:
	  sprintf(dest, "%s", "$17");
	  break;
	case 2:
	  sprintf(dest, "%s", "$18");
	  break;
	case 3:
	  sprintf(dest, "%s", "$19");
	  break;
	case 4:
	  sprintf(dest, "%s", "$20");
	  break;
	case 5:
	  sprintf(dest, "%s", "$21");
	  break;
	default:
	  sprintf(dest, "%s", "$1");
	  break;
	}
      Inst_Printf("lda", "%s,pl_foreign_double+%d", dest, index * 8);
      if (offset > 5)
	{
	  Inst_Printf("stq", "%s,%d($30)", dest, (offset - 6) * 8);
	}
      return 1;
    }
  else
    {
      switch (offset)
	{
	case 0:
	  sprintf(dest, "%s", "$f16");
	  break;
	case 1:
	  sprintf(dest, "%s", "$f17");
	  break;
	case 2:
	  sprintf(dest, "%s", "$f18");
	  break;
	case 3:
	  sprintf(dest, "%s", "$f19");
	  break;
	case 4:
	  sprintf(dest, "%s", "$f20");
	  break;
	case 5:
	  sprintf(dest, "%s", "$f21");
	  break;
	default:
	  sprintf(dest, "%s", "$f1");
	  break;
	}
      Inst_Printf("lda", "$1,pl_foreign_double+%d", index * 8);
      Inst_Printf("ldt", "%s,0($1)", dest);
      if (offset > 5)
	{
	  Inst_Printf("stt", "%s,%d($30)", dest, (offset - 6) * 8);
	}
      return 1;
    }
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, int fc, int nb_args, int nb_args_in_words)
{
  if (!make_inline(fct_name, nb_args))
    {
      Inst_Printf("jsr", "$26,%s", fct_name);
      Inst_Printf("ldgp", "$gp,0($26)");
    }
}




/*-------------------------------------------------------------------------*
 * CALL_C_STOP                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Stop(char *fct_name, int nb_args, char **p_inline)
{
}




/*-------------------------------------------------------------------------*
 * JUMP_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_Ret(void)
{
  Inst_Printf("mov", "$0,$27");
  Inst_Printf("jmp", "$31,($27),0");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("bne", "$0,$Lcont%d", w_label);
  Pl_Fail();
  Label_Printf("$Lcont%d:", w_label++);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  Inst_Printf("lda", "$1,%s", name);
  if (index * 8 > 1 << 15)
    {
      Inst_Printf("lda", "$2,%d", index * 8);
      Inst_Printf("addq", "$1,$2,$1");
      index = 0;
    }
  Inst_Printf("stq", "$0,%d($1)", index * 8);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Inst_Printf("stq", "$0,%d(%s)", index * 8, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
#ifdef MAP_REG_E
  Inst_Printf("stq", "$0,%d(%s)", Y_OFFSET(index), asm_reg_e);
#else
  Inst_Printf("ldq", "$4,%s", asm_reg_e);
  Inst_Printf("stq", "$0,%d($4)", Y_OFFSET(index));
#endif
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("lda", "$1,pl_foreign_long");
  Inst_Printf("stq", "$0,%d($1)", index * 8);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("lda", "$1,pl_foreign_double");
  Inst_Printf("stt", "$f0,%d($1)", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
{
  Inst_Printf("lda", "$1,%ld", int_val);
  Inst_Printf("subq", "$0,$1,$1");
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("beq", "$1,%s", label);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  /* this is based on the comparison we did with Cmp_Ret_And_Int */
  /* means this is more or less a Jump_If_Not_Equal ! */
  Inst_Printf("bgt", "$1,%s", label);
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("ldq", "$26,0($30)");
  Inst_Printf("addq", "$30,32,$30");
  Inst_Printf("ret", "$31,($26),1");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb_consts)
{
#ifdef M_alpha_linux
  Label_Printf(".section\t.rodata");
#else
  Label_Printf(".rdata");
#endif
}




/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(int str_no, char *asciiz)
{
  Label_Printf("%s%d:", STRING_PREFIX, str_no);
  Inst_Printf(".ascii", "%s", asciiz);
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_STOP                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Stop(int nb_consts)
{
}




/*-------------------------------------------------------------------------*
 * DICO_LONG_START                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long_Start(int nb_longs)
{
#ifdef M_alpha_linux
  Label_Printf(".section\t.sdata,\"aw\"");
#else
  Label_Printf(".data");
#endif
  Inst_Printf(".align", "3");
}




/*-------------------------------------------------------------------------*
 * DICO_LONG                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long(char *name, int global, VType vtype, PlLong value)
{
  switch (vtype)
    {
    case NONE:
      value = 1;		/* then in case ARRAY_SIZE */
    case ARRAY_SIZE:
      Inst_Printf(".align", "3");
#ifdef M_alpha_linux
      Label_Printf(".section\t.bss");
#endif
      if (!global)
	{
#ifdef M_alpha_linux
	  Inst_Printf(".type", "%s,@object", name);
	  Inst_Printf(".size", "%s,%ld", name, value * 8);
	  Inst_Printf(".align", "3");
	  Label_Printf("%s:", name);
	  Inst_Printf(".zero", "%ld", value * 8);
#else
	  Inst_Printf(".lcomm", "%s,%ld", name, value * 8);
#endif
	}
      else
	{
#ifdef M_alpha_linux
	  Inst_Printf(".comm", "%s,%ld,8", name, value * 8);
#else
	  Inst_Printf(".comm", "%s,%ld", name, value * 8);
#endif
	}
      break;

    case INITIAL_VALUE:
#ifdef M_alpha_linux
      Label_Printf(".section\t.sdata,\"aw\"");
#endif
      if (global)
	{
	  Inst_Printf(".globl", "%s", name);
	  Inst_Printf(".align", "3");
#ifdef M_alpha_linux
	  Inst_Printf(".type", "%s,@object", name);
	  Inst_Printf(".size", "%s,8", name);
#endif
	  Label_Printf("%s:", name);
	  Inst_Printf(".quad", "%ld", value);
	}
      else
	{
	  Inst_Printf(".align", "3");
#ifdef M_alpha_linux
	  Inst_Printf(".type", "%s,@object", name);
	  Inst_Printf(".size", "%s,8", name);
#endif
	  Label_Printf("%s:", name);
	  Inst_Printf(".quad", "%ld", value);
	}
      break;
    }
}




/*-------------------------------------------------------------------------*
 * DICO_LONG_STOP                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long_Stop(int nb_longs)
{
}




/*-------------------------------------------------------------------------*
 * DATA_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Start(char *initializer_fct)
{
				/* last routine has to be closed first */
  if (act_routine[0] != '\0')
    {
      Inst_Printf("ret", "$31,($26),1");
      Inst_Printf(".end", "%s", act_routine);

      act_routine[0] = '\0';
    }

  if (initializer_fct == NULL)
    return;

  Inst_Printf(".section", ".ctors,\"aw\"");
  Inst_Printf(".quad", "%s", initializer_fct);
}




/*-------------------------------------------------------------------------*
 * DATA_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Stop(char *initializer_fct)
{
}
