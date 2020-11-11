# mark_description "Intel(R) Fortran Intel(R) 64 Compiler for applications running on Intel(R) 64, Version 16.0.4.258 Build 2016";
# mark_description "0811";
# mark_description "-O3 -S";
	.file "tors.f90"
	.text
..TXTST0:
# -- Begin  tors_
	.text
# mark_begin;
       .align    16,0x90
	.globl tors_
# --- TORS
tors_:
# parameter 1: %rdi
# parameter 2: %rsi
# parameter 3: %rdx
# parameter 4: %rcx
# parameter 5: %r8
# parameter 6: %r9
# parameter 7: 192 + %rsp
..B1.1:                         # Preds ..B1.0
	.cfi_startproc
..___tag_value_tors_.1:
..L2:
                                                          #1.12
        pushq     %r12                                          #1.12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
        pushq     %r13                                          #1.12
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
        pushq     %r14                                          #1.12
	.cfi_def_cfa_offset 32
	.cfi_offset 14, -32
        pushq     %r15                                          #1.12
	.cfi_def_cfa_offset 40
	.cfi_offset 15, -40
        pushq     %rbx                                          #1.12
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
        pushq     %rbp                                          #1.12
	.cfi_def_cfa_offset 56
	.cfi_offset 6, -56
        subq      $136, %rsp                                    #1.12
	.cfi_def_cfa_offset 192
        movq      %rsi, %rbp                                    #1.12
        movq      %rdi, %rbx                                    #1.12
        movl      $__STRLITPACK_47, %edi                        #39.6
        movl      $7, %esi                                      #39.6
        xorl      %eax, %eax                                    #39.6
        movq      %rcx, 128(%rsp)                               #1.12
        movq      %r9, %r15                                     #1.12
        movq      %r8, %r13                                     #1.12
        movq      %rdx, %r12                                    #1.12
..___tag_value_tors_.16:
        call      clock_push_                                   #39.6
..___tag_value_tors_.17:
                                # LOE rbx rbp r12 r13 r15
..B1.2:                         # Preds ..B1.1
        xorl      %r14d, %r14d                                  #42.1
        movl      $1, %edi                                      #44.1
        movslq    (%r15), %r8                                   #44.1
        movl      $20, %esi                                     #44.1
        testq     %r8, %r8                                      #44.1
        jle       ..B1.8        # Prob 2%                       #44.1
                                # LOE rbx rbp rsi rdi r8 r12 r13 r14
..B1.3:                         # Preds ..B1.2
        movq      192(%rsp), %r9                                #1.12
                                # LOE rbx rbp rsi rdi r8 r9 r12 r13 r14
..B1.4:                         # Preds ..B1.6 ..B1.3
        movl      $1431655766, %eax                             #48.30
        movl      -20(%rsi,%r9), %r15d                          #48.12
        imull     %r15d                                         #48.30
        movl      $1431655766, %eax                             #49.30
        movl      %edx, %r10d                                   #48.30
        movl      -16(%rsi,%r9), %r11d                          #49.12
        imull     %r11d                                         #49.30
        movl      -12(%rsi,%r9), %ecx                           #50.12
        movl      %ecx, %eax                                    #50.12
        sarl      $31, %r15d                                    #48.30
        subl      %r15d, %r10d                                  #48.30
        movl      %edx, %r15d                                   #49.30
        cltd                                                    #50.12
        xorl      %edx, %ecx                                    #50.12
        movl      $1431655766, %eax                             #50.36
        subl      %edx, %ecx                                    #50.12
        imull     %ecx                                          #50.36
        sarl      $31, %r11d                                    #49.30
        sarl      $31, %ecx                                     #50.36
        subl      %r11d, %r15d                                  #49.30
        movl      %edx, %r11d                                   #50.36
        subl      %ecx, %r11d                                   #50.36
        movl      -8(%rsi,%r9), %ecx                            #51.12
        movl      %ecx, %eax                                    #51.12
        cltd                                                    #51.12
        xorl      %edx, %ecx                                    #51.12
        movl      $1431655766, %eax                             #51.36
        subl      %edx, %ecx                                    #51.12
        imull     %ecx                                          #51.36
        movslq    %r10d, %r10                                   #52.3
        sarl      $31, %ecx                                     #51.36
        movslq    %r15d, %r15                                   #52.8
        subl      %ecx, %edx                                    #51.36
        movslq    %r11d, %r11                                   #52.30
        movslq    %edx, %rdx                                    #52.57
        movl      5548+coord_(,%r10,4), %ecx                    #52.8
        orl       5548+coord_(,%r15,4), %ecx                    #52.30
        orl       5548+coord_(,%r11,4), %ecx                    #52.57
        orl       5548+coord_(,%rdx,4), %ecx                    #53.30
        testb     $1, %cl                                       #53.30
        jne       ..B1.6        # Prob 40%                      #53.30
                                # LOE rbx rbp rsi rdi r8 r9 r12 r13 r14 edx r10d r11d r15d
..B1.5:                         # Preds ..B1.4
        incq      %r14                                          #62.3
        incl      %r10d                                         #63.3
        movslq    -4(%rsi,%r9), %rcx                            #60.3
        incl      %edx                                          #66.3
        incl      %r15d                                         #64.3
        incl      %r11d                                         #65.3
        movl      %r10d, -4+tors_$TORS_IS.0.1(,%r14,4)          #63.3
        movl      %edx, -4+tors_$TORS_LS.0.1(,%r14,4)           #66.3
        movq      -8(%rbp,%rcx,8), %rax                         #67.3
        movq      -8(%rbx,%rcx,8), %rdx                         #68.3
        movq      -8(%r12,%rcx,8), %r10                         #69.3
        movl      %r15d, -4+tors_$TORS_JS.0.1(,%r14,4)          #64.3
        movl      %r11d, -4+tors_$TORS_KS.0.1(,%r14,4)          #65.3
        movq      %rax, -8+tors_$TORS_PNS.0.1(,%r14,8)          #67.3
        movq      %rdx, -8+tors_$TORS_PKS.0.1(,%r14,8)          #68.3
        movq      %r10, -8+tors_$TORS_PHASES.0.1(,%r14,8)       #69.3
                                # LOE rbx rbp rsi rdi r8 r9 r12 r13 r14
..B1.6:                         # Preds ..B1.5 ..B1.4
        incq      %rdi                                          #44.1
        addq      $20, %rsi                                     #44.1
        cmpq      %r8, %rdi                                     #44.1
        jle       ..B1.4        # Prob 82%                      #44.1
                                # LOE rbx rbp rsi rdi r8 r9 r12 r13 r14
..B1.8:                         # Preds ..B1.6 ..B1.2
        movl      $__STRLITPACK_36, %edi                        #72.6
        movl      $4, %esi                                      #72.6
        xorl      %eax, %eax                                    #72.6
..___tag_value_tors_.18:
        call      clock_look_                                   #72.6
..___tag_value_tors_.19:
                                # LOE r13 r14
..B1.9:                         # Preds ..B1.8
        movq      128(%rsp), %rax                               #73.1
        xorl      %ebx, %ebx                                    #74.1
        pxor      %xmm1, %xmm1                                  #73.1
        movsd     %xmm1, (%rax)                                 #73.1
                                # LOE rbx r13 r14
..B1.10:                        # Preds ..B1.11 ..B1.9
        imulq     $1848, %rbx, %rdi                             #74.1
        addq      %r13, %rdi                                    #74.1
        xorl      %esi, %esi                                    #74.1
        movl      $1848, %edx                                   #74.1
        call      _intel_fast_memset                            #74.1
                                # LOE rbx r13 r14
..B1.11:                        # Preds ..B1.10
        incq      %rbx                                          #74.1
        cmpq      $3, %rbx                                      #74.1
        jb        ..B1.10       # Prob 66%                      #74.1
                                # LOE rbx r13 r14
..B1.12:                        # Preds ..B1.11
        xorl      %ebx, %ebx                                    #76.1
        testq     %r14, %r14                                    #76.1
        pxor      %xmm1, %xmm1                                  #
        jle       ..B1.23       # Prob 2%                       #76.1
                                # LOE rbx r13 r14 xmm1
..B1.13:                        # Preds ..B1.12
        movq      %r14, (%rsp)                                  #118.33
                                # LOE rbx r13
..B1.14:                        # Preds ..B1.27 ..B1.13
        movslq    tors_$TORS_KS.0.1(,%rbx,4), %r12              #79.3
        movslq    tors_$TORS_LS.0.1(,%rbx,4), %rbp              #80.3
        movslq    tors_$TORS_IS.0.1(,%rbx,4), %r15              #77.3
        movslq    tors_$TORS_JS.0.1(,%rbx,4), %r14              #78.3
        movsd     -8+coord_(,%r12,8), %xmm5                     #87.3
        movsd     -8+coord_(,%rbp,8), %xmm6                     #88.3
        movhpd    1840+coord_(,%r12,8), %xmm5                   #87.3
        movhpd    1840+coord_(,%rbp,8), %xmm6                   #88.3
        movsd     -8+coord_(,%r15,8), %xmm3                     #85.3
        movsd     -8+coord_(,%r14,8), %xmm4                     #86.3
        movaps    %xmm5, tors_$POS_K.0.1(%rip)                  #87.3
        movaps    %xmm6, tors_$POS_L.0.1(%rip)                  #88.3
        movsd     tors_$TORS_PNS.0.1(,%rbx,8), %xmm0            #81.3
        movsd     tors_$TORS_PKS.0.1(,%rbx,8), %xmm1            #82.3
        movsd     tors_$TORS_PHASES.0.1(,%rbx,8), %xmm2         #83.3
        movhpd    1840+coord_(,%r15,8), %xmm3                   #85.3
        movsd     3688+coord_(,%r15,8), %xmm7                   #85.3
        movhpd    1840+coord_(,%r14,8), %xmm4                   #86.3
        movsd     3688+coord_(,%r14,8), %xmm8                   #86.3
        movsd     3688+coord_(,%r12,8), %xmm5                   #87.3
        movsd     3688+coord_(,%rbp,8), %xmm6                   #88.3
        movsd     %xmm0, 120(%rsp)                              #81.3
        movsd     %xmm1, 24(%rsp)                               #82.3
        movsd     %xmm2, 8(%rsp)                                #83.3
        movaps    %xmm3, tors_$POS_I.0.1(%rip)                  #85.3
        movsd     %xmm7, 16+tors_$POS_I.0.1(%rip)               #85.3
        movaps    %xmm4, tors_$POS_J.0.1(%rip)                  #86.3
        movsd     %xmm8, 16+tors_$POS_J.0.1(%rip)               #86.3
        movsd     %xmm5, 16+tors_$POS_K.0.1(%rip)               #87.3
        movsd     %xmm6, 16+tors_$POS_L.0.1(%rip)               #88.3
        testb     $1, 168+pbcbox_(%rip)                         #89.8
        je        ..B1.19       # Prob 60%                      #89.8
                                # LOE rbx rbp r12 r13 r14 r15 xmm5 xmm6 xmm7 xmm8
..B1.15:                        # Preds ..B1.14
        movl      $tors_$POS_I.0.1, %edi                        #94.10
        movl      $tors_$POS_J.0.1, %esi                        #94.10
        xorl      %eax, %eax                                    #94.10
..___tag_value_tors_.20:
        call      pbc_                                          #94.10
..___tag_value_tors_.21:
                                # LOE rbx rbp r12 r13 r14 r15
..B1.16:                        # Preds ..B1.15
        movl      $tors_$POS_J.0.1, %edi                        #95.10
        movl      $tors_$POS_K.0.1, %esi                        #95.10
        xorl      %eax, %eax                                    #95.10
..___tag_value_tors_.22:
        call      pbc_                                          #95.10
..___tag_value_tors_.23:
                                # LOE rbx rbp r12 r13 r14 r15
..B1.17:                        # Preds ..B1.16
        movl      $tors_$POS_K.0.1, %edi                        #96.10
        movl      $tors_$POS_L.0.1, %esi                        #96.10
        xorl      %eax, %eax                                    #96.10
..___tag_value_tors_.24:
        call      pbc_                                          #96.10
..___tag_value_tors_.25:
                                # LOE rbx rbp r12 r13 r14 r15
..B1.18:                        # Preds ..B1.17
        movsd     16+tors_$POS_I.0.1(%rip), %xmm7               #104.3
        movsd     16+tors_$POS_J.0.1(%rip), %xmm8               #104.3
        movsd     16+tors_$POS_K.0.1(%rip), %xmm5               #105.3
        movsd     16+tors_$POS_L.0.1(%rip), %xmm6               #106.3
                                # LOE rbx rbp r12 r13 r14 r15 xmm5 xmm6 xmm7 xmm8
..B1.19:                        # Preds ..B1.18 ..B1.14
        movaps    tors_$POS_J.0.1(%rip), %xmm1                  #104.3
        movaps    %xmm8, %xmm4                                  #104.3
        movaps    tors_$POS_K.0.1(%rip), %xmm2                  #105.3
        movaps    %xmm1, %xmm0                                  #104.3
        movaps    tors_$POS_L.0.1(%rip), %xmm3                  #106.3
        movl      $tors_$RIJ.0.1, %edi                          #107.8
        subpd     tors_$POS_I.0.1(%rip), %xmm0                  #104.3
        subpd     %xmm2, %xmm1                                  #105.3
        subpd     %xmm2, %xmm3                                  #106.3
        subsd     %xmm7, %xmm4                                  #104.3
        subsd     %xmm5, %xmm8                                  #105.3
        subsd     %xmm5, %xmm6                                  #106.3
        movl      $tors_$RKJ.0.1, %esi                          #107.8
        movl      $tors_$N1.0.1, %edx                           #107.8
        xorl      %eax, %eax                                    #107.8
        movaps    %xmm0, tors_$RIJ.0.1(%rip)                    #104.3
        movaps    %xmm1, tors_$RKJ.0.1(%rip)                    #105.3
        movaps    %xmm3, tors_$RKL.0.1(%rip)                    #106.3
        movsd     %xmm4, 16+tors_$RIJ.0.1(%rip)                 #104.3
        movsd     %xmm8, 16+tors_$RKJ.0.1(%rip)                 #105.3
        movsd     %xmm6, 16+tors_$RKL.0.1(%rip)                 #106.3
..___tag_value_tors_.26:
        call      cross_                                        #107.8
..___tag_value_tors_.27:
                                # LOE rbx rbp r12 r13 r14 r15
..B1.20:                        # Preds ..B1.19
        movl      $tors_$RKJ.0.1, %edi                          #108.8
        movl      $tors_$RKL.0.1, %esi                          #108.8
        movl      $tors_$N2.0.1, %edx                           #108.8
        xorl      %eax, %eax                                    #108.8
..___tag_value_tors_.28:
        call      cross_                                        #108.8
..___tag_value_tors_.29:
                                # LOE rbx rbp r12 r13 r14 r15
..B1.21:                        # Preds ..B1.20
        movsd     tors_$N2.0.1(%rip), %xmm0                     #110.8
        movsd     8+tors_$N2.0.1(%rip), %xmm10                  #110.8
        movaps    %xmm0, %xmm2                                  #110.8
        movsd     tors_$N1.0.1(%rip), %xmm8                     #109.8
        movaps    %xmm10, %xmm7                                 #110.8
        movsd     8+tors_$N1.0.1(%rip), %xmm9                   #109.8
        movaps    %xmm8, %xmm3                                  #109.8
        movsd     tors_$RKJ.0.1(%rip), %xmm4                    #111.11
        movaps    %xmm9, %xmm6                                  #109.8
        movsd     8+tors_$RKJ.0.1(%rip), %xmm15                 #111.11
        movaps    %xmm4, %xmm1                                  #112.11
        movsd     tors_$RIJ.0.1(%rip), %xmm5                    #111.11
        movaps    %xmm15, %xmm13                                #112.11
        movsd     8+tors_$RIJ.0.1(%rip), %xmm11                 #111.11
        mulsd     %xmm4, %xmm5                                  #111.11
        mulsd     %xmm0, %xmm2                                  #110.8
        mulsd     %xmm10, %xmm7                                 #110.8
        mulsd     %xmm8, %xmm0                                  #114.15
        mulsd     %xmm9, %xmm10                                 #114.15
        addsd     %xmm7, %xmm2                                  #110.8
        mulsd     %xmm15, %xmm11                                #111.11
        mulsd     %xmm8, %xmm3                                  #109.8
        addsd     %xmm10, %xmm0                                 #114.15
        mulsd     %xmm9, %xmm6                                  #109.8
        addsd     %xmm11, %xmm5                                 #111.11
        mulsd     %xmm4, %xmm1                                  #112.11
        mulsd     %xmm15, %xmm13                                #112.11
        addsd     %xmm6, %xmm3                                  #109.8
        addsd     %xmm13, %xmm1                                 #112.11
        movsd     16+tors_$N1.0.1(%rip), %xmm10                 #109.8
        movsd     16+tors_$N2.0.1(%rip), %xmm11                 #110.8
        movaps    %xmm10, %xmm6                                 #109.8
        movaps    %xmm11, %xmm7                                 #110.8
        mulsd     %xmm10, %xmm6                                 #109.8
        mulsd     %xmm11, %xmm7                                 #110.8
        addsd     %xmm6, %xmm3                                  #109.8
        addsd     %xmm7, %xmm2                                  #110.8
        movsd     %xmm3, 56(%rsp)                               #109.8
        mulsd     %xmm2, %xmm3                                  #114.9
        sqrtsd    %xmm3, %xmm3                                  #114.9
        movsd     %xmm10, 80(%rsp)                              #109.8
        mulsd     %xmm11, %xmm10                                #114.15
        movsd     %xmm8, 88(%rsp)                               #109.8
        addsd     %xmm10, %xmm0                                 #114.15
        divsd     %xmm3, %xmm0                                  #114.9
        movsd     %xmm9, 48(%rsp)                               #109.8
        movsd     16+tors_$RKJ.0.1(%rip), %xmm9                 #111.11
        movsd     16+tors_$RIJ.0.1(%rip), %xmm8                 #111.11
        mulsd     %xmm9, %xmm8                                  #111.11
        movsd     tors_$RKL.0.1(%rip), %xmm12                   #113.11
        addsd     %xmm8, %xmm5                                  #111.11
        mulsd     %xmm12, %xmm4                                 #113.11
        movsd     %xmm5, 112(%rsp)                              #111.11
        movaps    %xmm9, %xmm5                                  #112.11
        mulsd     %xmm9, %xmm5                                  #112.11
        movsd     8+tors_$RKL.0.1(%rip), %xmm14                 #113.11
        addsd     %xmm5, %xmm1                                  #112.11
        mulsd     %xmm14, %xmm15                                #113.11
        movsd     %xmm1, 104(%rsp)                              #112.11
        addsd     %xmm15, %xmm4                                 #113.11
        movsd     16+tors_$RKL.0.1(%rip), %xmm1                 #113.11
        mulsd     %xmm1, %xmm9                                  #113.11
        movsd     %xmm12, 32(%rsp)                              #113.11
        addsd     %xmm9, %xmm4                                  #113.11
        movsd     %xmm14, 16(%rsp)                              #113.11
        movsd     %xmm11, 72(%rsp)                              #110.8
        movsd     %xmm2, 64(%rsp)                               #110.8
        movsd     %xmm1, 40(%rsp)                               #113.11
        movsd     %xmm4, 96(%rsp)                               #113.11
        call      acos                                          #114.9
                                # LOE rbx rbp r12 r13 r14 r15 xmm0
..B1.28:                        # Preds ..B1.21
        mulsd     120(%rsp), %xmm0                              #118.22
        subsd     8(%rsp), %xmm0                                #118.22
        call      __libm_sse2_sincos                            #118.22
                                # LOE rbx rbp r12 r13 r14 r15 xmm0 xmm1
..B1.27:                        # Preds ..B1.28
        movsd     104(%rsp), %xmm3                              #119.48
        pxor      %xmm7, %xmm7                                  #119.48
        sqrtsd    %xmm3, %xmm7                                  #119.48
        movsd     24(%rsp), %xmm6                               #118.42
        pxor      %xmm9, %xmm9                                  #119.3
        movsd     120(%rsp), %xmm4                              #119.17
        incq      %rbx                                          #76.1
        mulsd     %xmm6, %xmm4                                  #119.17
        divsd     %xmm7, %xmm4                                  #119.27
        movsd     88(%rsp), %xmm10                              #115.24
        movsd     48(%rsp), %xmm5                               #115.24
        mulsd     32(%rsp), %xmm10                              #115.24
        mulsd     16(%rsp), %xmm5                               #115.24
        mulsd     %xmm0, %xmm4                                  #119.40
        addsd     %xmm5, %xmm10                                 #115.24
        movsd     40(%rsp), %xmm8                               #115.24
        movsd     80(%rsp), %xmm2                               #115.24
        mulsd     %xmm2, %xmm8                                  #115.24
        movsd     .L_2il0floatpacket.1(%rip), %xmm11            #118.33
        addsd     %xmm8, %xmm10                                 #115.24
        addsd     %xmm11, %xmm1                                 #118.33
        comisd    %xmm10, %xmm9                                 #119.3
        mulsd     %xmm6, %xmm1                                  #118.42
        movsd     .L_2il0floatpacket.0(%rip), %xmm12            #119.3
        ja        ..L30         # Prob 50%                      #119.3
        movaps    %xmm11, %xmm12                                #119.3
..L30:                                                          #
        movq      128(%rsp), %rax                               #118.3
        mulsd     %xmm12, %xmm4                                 #119.48
        addsd     (%rax), %xmm1                                 #118.3
        xorps     .L_2il0floatpacket.2(%rip), %xmm4             #119.3
        movsd     %xmm1, (%rax)                                 #118.3
        movaps    %xmm4, %xmm1                                  #120.3
        divsd     56(%rsp), %xmm1                               #120.3
        movsd     64(%rsp), %xmm13                              #123.3
        xorps     .L_2il0floatpacket.2(%rip), %xmm13            #123.3
        divsd     %xmm13, %xmm4                                 #123.14
        movsd     112(%rsp), %xmm0                              #121.17
        mulsd     %xmm1, %xmm0                                  #121.17
        mulsd     %xmm3, %xmm1                                  #122.17
        mulsd     %xmm4, %xmm3                                  #124.17
        mulsd     96(%rsp), %xmm4                               #125.17
        movaps    %xmm0, %xmm8                                  #121.17
        movaps    %xmm1, %xmm14                                 #122.17
        unpcklpd  %xmm8, %xmm8                                  #121.17
        movaps    %xmm3, %xmm11                                 #124.17
        movaps    tors_$N1.0.1(%rip), %xmm5                     #121.17
        movaps    %xmm4, %xmm15                                 #125.17
        mulpd     %xmm5, %xmm8                                  #121.3
        mulsd     %xmm2, %xmm0                                  #121.3
        mulsd     %xmm1, %xmm2                                  #122.3
        unpcklpd  %xmm14, %xmm14                                #122.17
        movaps    %xmm8, %xmm6                                  #128.54
        mulpd     %xmm14, %xmm5                                 #122.3
        unpcklpd  %xmm11, %xmm11                                #124.17
        movaps    %xmm0, %xmm1                                  #128.54
        movaps    tors_$N2.0.1(%rip), %xmm9                     #124.17
        subsd     %xmm2, %xmm1                                  #128.54
        mulpd     %xmm9, %xmm11                                 #124.3
        subpd     %xmm5, %xmm6                                  #128.54
        unpcklpd  %xmm15, %xmm15                                #125.17
        mulpd     %xmm15, %xmm9                                 #125.3
        movsd     -8(%r13,%r15,8), %xmm14                       #127.3
        movhpd    1840(%r13,%r15,8), %xmm14                     #127.3
        addpd     %xmm5, %xmm14                                 #127.3
        subpd     %xmm9, %xmm6                                  #128.62
        movsd     %xmm14, -8(%r13,%r15,8)                       #127.3
        movhpd    %xmm14, 1840(%r13,%r15,8)                     #127.3
        movaps    %xmm9, tors_$FCRKL.0.1(%rip)                  #125.3
        movsd     -8(%r13,%r14,8), %xmm7                        #128.3
        subpd     %xmm11, %xmm9                                 #129.54
        movhpd    1840(%r13,%r14,8), %xmm7                      #128.3
        addpd     %xmm6, %xmm7                                  #128.3
        subpd     %xmm8, %xmm9                                  #129.62
        movsd     %xmm7, -8(%r13,%r14,8)                        #128.3
        movhpd    %xmm7, 1840(%r13,%r14,8)                      #128.3
        movsd     -8(%r13,%r12,8), %xmm10                       #129.3
        movhpd    1840(%r13,%r12,8), %xmm10                     #129.3
        addpd     %xmm9, %xmm10                                 #129.3
        movsd     %xmm10, -8(%r13,%r12,8)                       #129.3
        movhpd    %xmm10, 1840(%r13,%r12,8)                     #129.3
        movsd     -8(%r13,%rbp,8), %xmm12                       #130.3
        movhpd    1840(%r13,%rbp,8), %xmm12                     #130.3
        addpd     %xmm11, %xmm12                                #130.3
        movsd     72(%rsp), %xmm13                              #124.3
        mulsd     %xmm13, %xmm3                                 #124.3
        mulsd     %xmm4, %xmm13                                 #125.3
        movsd     %xmm12, -8(%r13,%rbp,8)                       #130.3
        subsd     %xmm13, %xmm1                                 #128.62
        movhpd    %xmm12, 1840(%r13,%rbp,8)                     #130.3
        movsd     3688(%r13,%r15,8), %xmm4                      #127.3
        movsd     %xmm13, 16+tors_$FCRKL.0.1(%rip)              #125.3
        addsd     %xmm2, %xmm4                                  #127.3
        subsd     %xmm3, %xmm13                                 #129.54
        movsd     %xmm4, 3688(%r13,%r15,8)                      #127.3
        subsd     %xmm0, %xmm13                                 #129.62
        addsd     3688(%r13,%r14,8), %xmm1                      #128.3
        movsd     %xmm1, 3688(%r13,%r14,8)                      #128.3
        movsd     %xmm0, 16+tors_$FBRIJ.0.1(%rip)               #121.3
        movaps    %xmm8, tors_$FBRIJ.0.1(%rip)                  #121.3
        movaps    %xmm5, tors_$FBRKJ.0.1(%rip)                  #122.3
        movaps    %xmm11, tors_$FCRKJ.0.1(%rip)                 #124.3
        movsd     %xmm2, 16+tors_$FBRKJ.0.1(%rip)               #122.3
        movsd     %xmm3, 16+tors_$FCRKJ.0.1(%rip)               #124.3
        cmpq      (%rsp), %rbx                                  #76.1
        addsd     3688(%r13,%r12,8), %xmm13                     #129.3
        movsd     %xmm13, 3688(%r13,%r12,8)                     #129.3
        movsd     3688(%r13,%rbp,8), %xmm0                      #130.3
        addsd     %xmm3, %xmm0                                  #130.3
        movsd     %xmm0, 3688(%r13,%rbp,8)                      #130.3
        jb        ..B1.14       # Prob 99%                      #76.1
                                # LOE rbx r13
..B1.23:                        # Preds ..B1.27 ..B1.12
        xorl      %eax, %eax                                    #134.6
        addq      $136, %rsp                                    #134.6
	.cfi_def_cfa_offset 56
	.cfi_restore 6
        popq      %rbp                                          #134.6
	.cfi_def_cfa_offset 48
	.cfi_restore 3
        popq      %rbx                                          #134.6
	.cfi_def_cfa_offset 40
	.cfi_restore 15
        popq      %r15                                          #134.6
	.cfi_def_cfa_offset 32
	.cfi_restore 14
        popq      %r14                                          #134.6
	.cfi_def_cfa_offset 24
	.cfi_restore 13
        popq      %r13                                          #134.6
	.cfi_def_cfa_offset 16
	.cfi_restore 12
        popq      %r12                                          #134.6
	.cfi_def_cfa_offset 8
        jmp       clock_pop_                                    #134.6
        .align    16,0x90
                                # LOE
	.cfi_endproc
# mark_end;
	.type	tors_,@function
	.size	tors_,.-tors_
	.bss
	.align 32
	.align 32
tors_$TORS_IS.0.1:
	.type	tors_$TORS_IS.0.1,@object
	.size	tors_$TORS_IS.0.1,592
	.space 592	# pad
	.space 16	# pad
	.align 32
tors_$TORS_LS.0.1:
	.type	tors_$TORS_LS.0.1,@object
	.size	tors_$TORS_LS.0.1,592
	.space 592	# pad
	.space 16	# pad
	.align 32
tors_$TORS_JS.0.1:
	.type	tors_$TORS_JS.0.1,@object
	.size	tors_$TORS_JS.0.1,592
	.space 592	# pad
	.space 16	# pad
	.align 32
tors_$TORS_KS.0.1:
	.type	tors_$TORS_KS.0.1,@object
	.size	tors_$TORS_KS.0.1,592
	.space 592	# pad
	.space 16	# pad
	.align 32
tors_$TORS_PNS.0.1:
	.type	tors_$TORS_PNS.0.1,@object
	.size	tors_$TORS_PNS.0.1,1184
	.space 1184	# pad
	.align 32
tors_$TORS_PKS.0.1:
	.type	tors_$TORS_PKS.0.1,@object
	.size	tors_$TORS_PKS.0.1,1184
	.space 1184	# pad
	.align 32
tors_$TORS_PHASES.0.1:
	.type	tors_$TORS_PHASES.0.1,@object
	.size	tors_$TORS_PHASES.0.1,1184
	.space 1184	# pad
	.align 16
tors_$POS_K.0.1:
	.type	tors_$POS_K.0.1,@object
	.size	tors_$POS_K.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$POS_L.0.1:
	.type	tors_$POS_L.0.1,@object
	.size	tors_$POS_L.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$POS_I.0.1:
	.type	tors_$POS_I.0.1,@object
	.size	tors_$POS_I.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$POS_J.0.1:
	.type	tors_$POS_J.0.1,@object
	.size	tors_$POS_J.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$RIJ.0.1:
	.type	tors_$RIJ.0.1,@object
	.size	tors_$RIJ.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$RKJ.0.1:
	.type	tors_$RKJ.0.1,@object
	.size	tors_$RKJ.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$N1.0.1:
	.type	tors_$N1.0.1,@object
	.size	tors_$N1.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$RKL.0.1:
	.type	tors_$RKL.0.1,@object
	.size	tors_$RKL.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$N2.0.1:
	.type	tors_$N2.0.1,@object
	.size	tors_$N2.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$FCRKL.0.1:
	.type	tors_$FCRKL.0.1,@object
	.size	tors_$FCRKL.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$FBRIJ.0.1:
	.type	tors_$FBRIJ.0.1,@object
	.size	tors_$FBRIJ.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$FBRKJ.0.1:
	.type	tors_$FBRKJ.0.1,@object
	.size	tors_$FBRKJ.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
tors_$FCRKJ.0.1:
	.type	tors_$FCRKJ.0.1,@object
	.size	tors_$FCRKJ.0.1,24
	.space 24	# pad
	.data
# -- End  tors_
	.section .rodata, "a"
	.align 16
	.align 16
.L_2il0floatpacket.2:
	.long	0x00000000,0x80000000,0x00000000,0x00000000
	.type	.L_2il0floatpacket.2,@object
	.size	.L_2il0floatpacket.2,16
	.align 8
.L_2il0floatpacket.0:
	.long	0x00000000,0xbff00000
	.type	.L_2il0floatpacket.0,@object
	.size	.L_2il0floatpacket.0,8
	.align 8
.L_2il0floatpacket.1:
	.long	0x00000000,0x3ff00000
	.type	.L_2il0floatpacket.1,@object
	.size	.L_2il0floatpacket.1,8
	.section .rodata.str1.4, "aMS",@progbits,1
	.align 4
	.align 4
__STRLITPACK_47:
	.long	1936879476
	.long	7237481
	.type	__STRLITPACK_47,@object
	.size	__STRLITPACK_47,8
	.align 4
__STRLITPACK_36:
	.long	1936879476
	.byte	0
	.type	__STRLITPACK_36,@object
	.size	__STRLITPACK_36,5
	.data
	.comm coord_,6472,32
	.comm pbcbox_,180,32
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .eh_frame
	.section .eh_frame,"a",@progbits
.eh_frame_seg:
	.align 8
# End
