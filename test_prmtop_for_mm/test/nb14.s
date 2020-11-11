# mark_description "Intel(R) Fortran Intel(R) 64 Compiler for applications running on Intel(R) 64, Version 16.0.4.258 Build 2016";
# mark_description "0811";
# mark_description "-O3 -S";
	.file "nb14.f90"
	.text
..TXTST0:
# -- Begin  nb14_
	.text
# mark_begin;
       .align    16,0x90
	.globl nb14_
# --- NB14
nb14_:
# parameter 1: %rdi
# parameter 2: %rsi
# parameter 3: %rdx
# parameter 4: %rcx
# parameter 5: %r8
# parameter 6: %r9
# parameter 7: 272 + %rsp
# parameter 8: 280 + %rsp
# parameter 9: 288 + %rsp
# parameter 10: 296 + %rsp
# parameter 11: 304 + %rsp
# parameter 12: 312 + %rsp
# parameter 13: 320 + %rsp
# parameter 14: 328 + %rsp
# parameter 15: 336 + %rsp
# parameter 16: 344 + %rsp
..B1.1:                         # Preds ..B1.0
	.cfi_startproc
..___tag_value_nb14_.1:
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
        subq      $216, %rsp                                    #1.12
	.cfi_def_cfa_offset 272
        xorl      %eax, %eax                                    #65.6
        movq      %rsi, 136(%rsp)                               #1.12
        movl      $4, %esi                                      #65.6
        movq      %rdi, 144(%rsp)                               #1.12
        movl      $__STRLITPACK_84, %edi                        #65.6
        movq      %r9, %r13                                     #1.12
        movq      %r8, %r15                                     #1.12
        movq      %rcx, %r14                                    #1.12
        movq      %rdx, %r12                                    #1.12
..___tag_value_nb14_.16:
        call      clock_push_                                   #65.6
..___tag_value_nb14_.17:
                                # LOE r12 r13 r14 r15
..B1.2:                         # Preds ..B1.1
        lea       304(%rsp), %rdx                               #1.12
        movq      (%rdx), %rax                                  #1.12
        xorl      %ebx, %ebx                                    #70.1
        movq      16(%rdx), %rdx                                #1.12
        pxor      %xmm0, %xmm0                                  #68.1
        pxor      %xmm1, %xmm1                                  #69.1
        movq      312(%rsp), %rbp                               #70.1
        movsd     %xmm0, (%rax)                                 #68.1
        movsd     %xmm1, (%rdx)                                 #69.1
                                # LOE rbx rbp r12 r13 r14 r15
..B1.3:                         # Preds ..B1.4 ..B1.2
        imulq     $1848, %rbx, %rdi                             #70.1
        addq      %rbp, %rdi                                    #70.1
        xorl      %esi, %esi                                    #70.1
        movl      $1848, %edx                                   #70.1
        call      _intel_fast_memset                            #70.1
                                # LOE rbx rbp r12 r13 r14 r15
..B1.4:                         # Preds ..B1.3
        incq      %rbx                                          #70.1
        cmpq      $3, %rbx                                      #70.1
        jb        ..B1.3        # Prob 66%                      #70.1
                                # LOE rbx rbp r12 r13 r14 r15
..B1.5:                         # Preds ..B1.4
        movq      328(%rsp), %rbp                               #71.1
        xorl      %ebx, %ebx                                    #71.1
                                # LOE rbx rbp r12 r13 r14 r15
..B1.6:                         # Preds ..B1.7 ..B1.5
        imulq     $1848, %rbx, %rdi                             #71.1
        addq      %rbp, %rdi                                    #71.1
        xorl      %esi, %esi                                    #71.1
        movl      $1848, %edx                                   #71.1
        call      _intel_fast_memset                            #71.1
                                # LOE rbx rbp r12 r13 r14 r15
..B1.7:                         # Preds ..B1.6
        incq      %rbx                                          #71.1
        cmpq      $3, %rbx                                      #71.1
        jb        ..B1.6        # Prob 66%                      #71.1
                                # LOE rbx rbp r12 r13 r14 r15
..B1.8:                         # Preds ..B1.7
        xorl      %ebx, %ebx                                    #73.1
        lea       (%rsp), %rdi                                  #76.1
        movl      $99, %esi                                     #76.1
        movq      $0x1208384ff00, %rdx                          #76.1
        movl      $__STRLITPACK_108.0.1, %ecx                   #76.1
        lea       64(%rsp), %r8                                 #76.1
        xorl      %eax, %eax                                    #76.1
        xorl      %ebp, %ebp                                    #74.1
        movq      $0, (%rdi)                                    #76.1
        movq      $6, 64(%rdi)                                  #76.1
        movq      $__STRLITPACK_83, 72(%rdi)                    #76.1
        movq      $4, 80(%rdi)                                  #76.1
        call      for_open                                      #76.1
                                # LOE rbx rbp r12 r13 r14 r15
..B1.9:                         # Preds ..B1.8
        movl      $98, %esi                                     #77.1
        lea       (%rsp), %rdi                                  #77.1
        movq      $0x1208384ff00, %rdx                          #77.1
        movl      $__STRLITPACK_109.0.1, %ecx                   #77.1
        xorl      %eax, %eax                                    #77.1
        lea       88(%rsp), %r8                                 #77.1
        movq      $0, (%rdi)                                    #77.1
        movq      $5, 88(%rdi)                                  #77.1
        movq      $__STRLITPACK_82, 96(%rdi)                    #77.1
        movq      $4, 104(%rdi)                                 #77.1
        call      for_open                                      #77.1
                                # LOE rbx rbp r12 r13 r14 r15
..B1.10:                        # Preds ..B1.9
        movq      336(%rsp), %rax                               #1.12
        xorl      %edx, %edx                                    #78.1
        movslq    (%rax), %r10                                  #78.1
        xorl      %eax, %eax                                    #78.1
        testq     %r10, %r10                                    #78.1
        jle       ..B1.25       # Prob 2%                       #78.1
                                # LOE rax rdx rbx rbp r10 r12 r13 r14 r15
..B1.11:                        # Preds ..B1.10
        movsd     .L_2il0floatpacket.0(%rip), %xmm0             #104.16
        movq      272(%rsp), %r9                                #1.12
        movq      280(%rsp), %r8                                #1.12
        movq      288(%rsp), %rdi                               #1.12
        movq      296(%rsp), %rsi                               #1.12
        movq      344(%rsp), %rcx                               #1.12
        movq      %rbp, 168(%rsp)                               #1.12
        movq      %r10, 160(%rsp)                               #1.12
        movq      %r12, 128(%rsp)                               #1.12
        movq      %r14, 120(%rsp)                               #1.12
        movq      %rdx, %r14                                    #1.12
        movq      %r15, 112(%rsp)                               #1.12
        movq      %rax, %r15                                    #1.12
        movq      %r13, 152(%rsp)                               #1.12
                                # LOE rcx rbx r14 r15
..B1.12:                        # Preds ..B1.23 ..B1.11
        cmpl      $0, 8(%r15,%rcx)                              #83.27
        jl        ..B1.23       # Prob 16%                      #83.27
                                # LOE rcx rbx r14 r15
..B1.13:                        # Preds ..B1.12
        movl      12(%r15,%rcx), %esi                           #83.38
        testl     %esi, %esi                                    #83.57
        jl        ..B1.23       # Prob 16%                      #83.57
                                # LOE rcx rbx r14 r15 esi
..B1.14:                        # Preds ..B1.13
        movl      $1431655766, %eax                             #84.32
        movl      (%r15,%rcx), %r12d                            #84.14
        imull     %r12d                                         #84.32
        sarl      $31, %r12d                                    #84.32
        movl      $1431655766, %eax                             #85.32
        subl      %r12d, %edx                                   #84.32
        movslq    %edx, %rbp                                    #84.5
        imull     %esi                                          #85.32
        sarl      $31, %esi                                     #85.32
        incq      %rbp                                          #84.5
        subl      %esi, %edx                                    #85.32
        movslq    %edx, %r12                                    #85.5
        movl      5544+coord_(,%rbp,4), %edi                    #86.10
        orl       5548+coord_(,%r12,4), %edi                    #86.32
        incq      %r12                                          #85.5
        testl     $1, %edi                                      #86.32
        jne       ..B1.23       # Prob 40%                      #86.32
                                # LOE rcx rbx rbp r12 r14 r15
..B1.15:                        # Preds ..B1.14
        movsd     -8+coord_(,%rbp,8), %xmm0                     #93.5
        movhpd    1840+coord_(,%rbp,8), %xmm0                   #93.5
        movsd     -8+coord_(,%r12,8), %xmm1                     #94.5
        movhpd    1840+coord_(,%r12,8), %xmm1                   #94.5
        movq      3688+coord_(,%rbp,8), %rsi                    #93.5
        movq      3688+coord_(,%r12,8), %rdi                    #94.5
        movaps    %xmm0, nb14_$POS_I.0.1(%rip)                  #93.5
        movaps    %xmm1, nb14_$POS_L.0.1(%rip)                  #94.5
        movq      %rsi, 16+nb14_$POS_I.0.1(%rip)                #93.5
        movq      %rdi, 16+nb14_$POS_L.0.1(%rip)                #94.5
        testb     $1, 168+pbcbox_(%rip)                         #95.10
        je        ..B1.17       # Prob 60%                      #95.10
                                # LOE rcx rbx rbp r12 r14 r15
..B1.16:                        # Preds ..B1.15
        movl      $nb14_$POS_I.0.1, %edi                        #98.12
        movl      $nb14_$POS_L.0.1, %esi                        #98.12
        xorl      %eax, %eax                                    #98.12
..___tag_value_nb14_.18:
        call      pbc_                                          #98.12
..___tag_value_nb14_.19:
                                # LOE rbx rbp r12 r14 r15
..B1.68:                        # Preds ..B1.16
        movq      344(%rsp), %rcx                               #
                                # LOE rcx rbx rbp r12 r14 r15
..B1.17:                        # Preds ..B1.15 ..B1.68
        movq      296(%rsp), %r9                                #101.5
        movl      $nb14_$POS_I.0.1, %edi                        #102.11
        movq      280(%rsp), %r8                                #101.5
        movl      $nb14_$POS_L.0.1, %esi                        #102.11
        movq      288(%rsp), %r11                               #101.5
        xorl      %eax, %eax                                    #102.11
        movl      -4(%r9,%rbp,4), %r10d                         #101.5
        decl      %r10d                                         #101.5
        imull     (%r8), %r10d                                  #101.5
        addl      -4(%r9,%r12,4), %r10d                         #101.5
        movslq    %r10d, %r10                                   #101.15
        movslq    -4(%r11,%r10,4), %r13                         #101.5
..___tag_value_nb14_.20:
        call      length_                                       #102.11
..___tag_value_nb14_.21:
                                # LOE rbx rbp r12 r13 r14 r15 xmm0
..B1.65:                        # Preds ..B1.17
        movq      344(%rsp), %rcx                               #
        movaps    %xmm0, %xmm1                                  #102.11
                                # LOE rcx rbx rbp r12 r13 r14 r15 xmm1
..B1.18:                        # Preds ..B1.65
        movq      144(%rsp), %rsi                               #106.11
        testq     %r13, %r13                                    #103.15
        movq      152(%rsp), %rdi                               #107.17
        movslq    16(%r15,%rcx), %r8                            #106.43
        movsd     -8(%rsi,%rbp,8), %xmm7                        #106.11
        movsd     -8(%rsi,%r12,8), %xmm6                        #106.28
        movsd     -8(%rdi,%r8,8), %xmm8                         #107.17
        jl        ..B1.59       # Prob 16%                      #103.15
                                # LOE rcx rbx rbp r8 r12 r13 r14 r15 xmm1 xmm6 xmm7 xmm8
..B1.19:                        # Preds ..B1.18
        movsd     .L_2il0floatpacket.0(%rip), %xmm0             #104.16
        comisd    %xmm1, %xmm0                                  #104.16
        jb        ..B1.21       # Prob 50%                      #104.16
                                # LOE rcx rbx rbp r8 r12 r13 r14 r15 xmm1 xmm6 xmm7 xmm8
..B1.20:                        # Preds ..B1.19
        movaps    %xmm7, %xmm12                                 #106.26
        movaps    %xmm1, %xmm2                                  #111.25
        mulsd     %xmm6, %xmm12                                 #106.26
        mulsd     %xmm1, %xmm2                                  #111.25
        movaps    %xmm12, %xmm0                                 #106.43
        movaps    %xmm2, %xmm14                                 #124.32
        divsd     %xmm1, %xmm0                                  #106.43
        mulsd     %xmm2, %xmm1                                  #111.25
        mulsd     %xmm2, %xmm14                                 #124.32
        mulsd     %xmm8, %xmm0                                  #107.15
        divsd     %xmm1, %xmm12                                 #111.17
        mulsd     %xmm8, %xmm12                                 #111.32
        mulsd     %xmm14, %xmm2                                 #124.60
        movaps    nb14_$POS_I.0.1(%rip), %xmm3                  #111.32
        movaps    %xmm12, %xmm10                                #111.32
        subpd     nb14_$POS_L.0.1(%rip), %xmm3                  #112.27
        unpcklpd  %xmm10, %xmm10                                #111.32
        movsd     16+nb14_$POS_I.0.1(%rip), %xmm4               #111.32
        movq      272(%rsp), %rsi                               #126.11
        movq      304(%rsp), %rdi                               #105.9
        movq      136(%rsp), %r9                                #124.11
        movsd     -8(%rsi,%r8,8), %xmm5                         #126.11
        mulpd     %xmm3, %xmm10                                 #110.9
        subsd     16+nb14_$POS_L.0.1(%rip), %xmm4               #112.27
        addsd     (%rdi), %xmm0                                 #105.9
        mulsd     %xmm4, %xmm12                                 #110.9
        movq      312(%rsp), %r8                                #113.9
        movq      128(%rsp), %r10                               #124.39
        movsd     %xmm0, (%rdi)                                 #105.9
        movsd     -8(%r8,%rbp,8), %xmm1                         #113.9
        movhpd    1840(%r8,%rbp,8), %xmm1                       #113.9
        addpd     %xmm10, %xmm1                                 #113.9
        movsd     3688(%r8,%rbp,8), %xmm9                       #113.9
        movsd     %xmm1, -8(%r8,%rbp,8)                         #113.9
        addsd     %xmm12, %xmm9                                 #113.9
        movhpd    %xmm1, 1840(%r8,%rbp,8)                       #113.9
        movsd     %xmm9, 3688(%r8,%rbp,8)                       #113.9
        movsd     -8(%r8,%r12,8), %xmm11                        #114.9
        movhpd    1840(%r8,%r12,8), %xmm11                      #114.9
        subpd     %xmm10, %xmm11                                #114.9
        movaps    %xmm14, %xmm10                                #124.32
        mulsd     %xmm14, %xmm10                                #124.32
        mulsd     %xmm10, %xmm14                                #124.32
        movsd     -8(%r9,%r13,8), %xmm1                         #124.11
        movsd     -8(%r10,%r13,8), %xmm0                        #124.39
        movaps    %xmm1, %xmm9                                  #124.27
        divsd     %xmm14, %xmm9                                 #124.27
        movsd     .L_2il0floatpacket.0(%rip), %xmm14            #129.20
        movaps    %xmm0, %xmm15                                 #124.55
        divsd     %xmm2, %xmm15                                 #124.55
        mulsd     %xmm1, %xmm14                                 #129.20
        subsd     %xmm15, %xmm9                                 #124.37
        mulsd     %xmm10, %xmm2                                 #129.46
        mulsd     %xmm5, %xmm9                                  #126.9
        divsd     %xmm2, %xmm14                                 #129.38
        movsd     .L_2il0floatpacket.1(%rip), %xmm2             #130.20
        mulsd     %xmm0, %xmm2                                  #130.20
        divsd     %xmm10, %xmm2                                 #130.38
        movsd     3688(%r8,%r12,8), %xmm13                      #114.9
        subsd     %xmm2, %xmm14                                 #130.11
        subsd     %xmm12, %xmm13                                #114.9
        mulsd     %xmm5, %xmm14                                 #132.11
        movaps    %xmm14, %xmm12                                #132.11
        unpcklpd  %xmm12, %xmm12                                #132.11
        mulpd     %xmm3, %xmm12                                 #128.9
        mulsd     %xmm14, %xmm4                                 #128.9
        movq      328(%rsp), %r13                               #134.9
        movsd     %xmm11, -8(%r8,%r12,8)                        #114.9
        movhpd    %xmm11, 1840(%r8,%r12,8)                      #114.9
        movsd     -8(%r13,%rbp,8), %xmm3                        #134.9
        movhpd    1840(%r13,%rbp,8), %xmm3                      #134.9
        addpd     %xmm12, %xmm3                                 #134.9
        movsd     3688(%r13,%rbp,8), %xmm11                     #134.9
        movsd     %xmm3, -8(%r13,%rbp,8)                        #134.9
        addsd     %xmm4, %xmm11                                 #134.9
        movhpd    %xmm3, 1840(%r13,%rbp,8)                      #134.9
        movsd     %xmm11, 3688(%r13,%rbp,8)                     #134.9
        movsd     %xmm13, 3688(%r8,%r12,8)                      #114.9
        movsd     -8(%r13,%r12,8), %xmm13                       #135.9
        movq      320(%rsp), %r11                               #123.9
        movhpd    1840(%r13,%r12,8), %xmm13                     #135.9
        movsd     3688(%r13,%r12,8), %xmm2                      #135.9
        subpd     %xmm12, %xmm13                                #135.9
        addsd     (%r11), %xmm9                                 #123.9
        subsd     %xmm4, %xmm2                                  #135.9
        movsd     %xmm9, (%r11)                                 #123.9
        movaps    %xmm12, nb14_$FORCE.0.1(%rip)                 #128.9
        movsd     %xmm4, 16+nb14_$FORCE.0.1(%rip)               #128.9
        movsd     %xmm13, -8(%r13,%r12,8)                       #135.9
        movhpd    %xmm13, 1840(%r13,%r12,8)                     #135.9
        movsd     %xmm2, 3688(%r13,%r12,8)                      #135.9
        jmp       ..B1.22       # Prob 100%                     #135.9
                                # LOE rcx rbx rbp r12 r14 r15 xmm0 xmm1 xmm5 xmm6 xmm7 xmm8
..B1.21:                        # Preds ..B1.19
        movq      272(%rsp), %rsi                               #146.7
        movq      136(%rsp), %rdi                               #144.7
        movsd     -8(%rsi,%r8,8), %xmm5                         #146.7
        movq      128(%rsp), %r8                                #145.7
        movsd     -8(%rdi,%r13,8), %xmm1                        #144.7
        movsd     -8(%r8,%r13,8), %xmm0                         #145.7
                                # LOE rcx rbx rbp r12 r14 r15 xmm0 xmm1 xmm5 xmm6 xmm7 xmm8
..B1.22:                        # Preds ..B1.20 ..B1.21
        movl      %ebp, nb14_$NB14_LJ6_IS.0.1(,%rbx,4)          #139.7
        movl      %r12d, nb14_$NB14_LJ6_LS.0.1(,%rbx,4)         #140.7
        incq      %rbx                                          #138.7
        movsd     %xmm8, -8+nb14_$NB14_LJ6_SCEES.0.1(,%rbx,8)   #143.7
        mulsd     %xmm7, %xmm8                                  #148.24
        movsd     %xmm1, -8+nb14_$NB14_LJ6_CN1S.0.1(,%rbx,8)    #144.7
        movsd     %xmm5, -8+nb14_$NB14_LJ6_SCNBS.0.1(,%rbx,8)   #146.7
        mulsd     %xmm6, %xmm8                                  #147.7
        mulsd     %xmm5, %xmm1                                  #149.7
        mulsd     %xmm0, %xmm5                                  #151.7
        movsd     %xmm7, -8+nb14_$NB14_LJ6_QIS.0.1(,%rbx,8)     #141.7
        movsd     %xmm6, -8+nb14_$NB14_LJ6_QLS.0.1(,%rbx,8)     #142.7
        movsd     %xmm0, -8+nb14_$NB14_LJ6_CN2S.0.1(,%rbx,8)    #145.7
        movsd     %xmm8, -8+nb14_$NB14_LJ6_SCQIQJS.0.1(,%rbx,8) #147.7
        movsd     %xmm1, -8+nb14_$NB14_LJ6_SCCN1S.0.1(,%rbx,8)  #149.7
        movsd     %xmm5, -8+nb14_$NB14_LJ6_SCCN2S.0.1(,%rbx,8)  #151.7
                                # LOE rcx rbx r14 r15
..B1.23:                        # Preds ..B1.22 ..B1.62 ..B1.14 ..B1.13 ..B1.12
                                #      
        incq      %r14                                          #78.1
        addq      $20, %r15                                     #78.1
        cmpq      160(%rsp), %r14                               #78.1
        jb        ..B1.12       # Prob 82%                      #78.1
                                # LOE rcx rbx r14 r15
..B1.24:                        # Preds ..B1.23
        movq      168(%rsp), %rbp                               #
                                # LOE rbx rbp
..B1.25:                        # Preds ..B1.24 ..B1.10
        movq      304(%rsp), %r9                                #206.1
        lea       (%rsp), %rdi                                  #206.1
        movl      $99, %esi                                     #206.1
        movq      $0x1208384ff00, %rdx                          #206.1
        movl      $__STRLITPACK_141.0.1, %ecx                   #206.1
        lea       176(%rsp), %r8                                #206.1
        movq      (%r9), %r10                                   #206.1
        xorl      %eax, %eax                                    #206.1
        movq      $0, (%rdi)                                    #206.1
        movq      %r10, 176(%rdi)                               #206.1
        call      for_write_seq_lis                             #206.1
                                # LOE rbx rbp
..B1.26:                        # Preds ..B1.25
        movq      312(%rsp), %r9                                #207.1
        lea       (%rsp), %rdi                                  #207.1
        movl      $99, %esi                                     #207.1
        movq      $0x1208384ff00, %rdx                          #207.1
        movl      $__STRLITPACK_142.0.1, %ecx                   #207.1
        lea       112(%rsp), %r8                                #207.1
        xorl      %eax, %eax                                    #207.1
        movq      $0, (%rdi)                                    #207.1
        movq      $5544, 112(%rdi)                              #207.1
        movq      %r9, 120(%rdi)                                #207.1
        call      for_write_seq_lis                             #207.1
                                # LOE rbx rbp
..B1.27:                        # Preds ..B1.26
        movq      320(%rsp), %r9                                #208.1
        lea       (%rsp), %rdi                                  #208.1
        movl      $99, %esi                                     #208.1
        movq      $0x1208384ff00, %rdx                          #208.1
        movl      $__STRLITPACK_143.0.1, %ecx                   #208.1
        lea       184(%rsp), %r8                                #208.1
        movq      (%r9), %r10                                   #208.1
        xorl      %eax, %eax                                    #208.1
        movq      $0, (%rdi)                                    #208.1
        movq      %r10, 184(%rdi)                               #208.1
        call      for_write_seq_lis                             #208.1
                                # LOE rbx rbp
..B1.28:                        # Preds ..B1.27
        movq      328(%rsp), %r9                                #209.1
        lea       (%rsp), %rdi                                  #209.1
        movl      $99, %esi                                     #209.1
        movq      $0x1208384ff00, %rdx                          #209.1
        movl      $__STRLITPACK_144.0.1, %ecx                   #209.1
        lea       128(%rsp), %r8                                #209.1
        xorl      %eax, %eax                                    #209.1
        movq      $0, (%rdi)                                    #209.1
        movq      $5544, 128(%rdi)                              #209.1
        movq      %r9, 136(%rdi)                                #209.1
        call      for_write_seq_lis                             #209.1
                                # LOE rbx rbp
..B1.29:                        # Preds ..B1.28
        lea       320(%rsp), %rdx                               #214.1
        movq      (%rdx), %rax                                  #214.1
        xorl      %r12d, %r12d                                  #216.1
        movq      -16(%rdx), %rdx                               #215.1
        pxor      %xmm0, %xmm0                                  #214.1
        pxor      %xmm1, %xmm1                                  #215.1
        movq      328(%rsp), %r13                               #216.1
        movsd     %xmm0, (%rax)                                 #214.1
        movsd     %xmm1, (%rdx)                                 #215.1
                                # LOE rbx rbp r12 r13
..B1.30:                        # Preds ..B1.31 ..B1.29
        imulq     $1848, %r12, %rdi                             #216.1
        addq      %r13, %rdi                                    #216.1
        xorl      %esi, %esi                                    #216.1
        movl      $1848, %edx                                   #216.1
        call      _intel_fast_memset                            #216.1
                                # LOE rbx rbp r12 r13
..B1.31:                        # Preds ..B1.30
        incq      %r12                                          #216.1
        cmpq      $3, %r12                                      #216.1
        jb        ..B1.30       # Prob 66%                      #216.1
                                # LOE rbx rbp r12 r13
..B1.32:                        # Preds ..B1.31
        movq      312(%rsp), %r13                               #217.1
        xorl      %r12d, %r12d                                  #217.1
                                # LOE rbx rbp r12 r13
..B1.33:                        # Preds ..B1.34 ..B1.32
        imulq     $1848, %r12, %rdi                             #217.1
        addq      %r13, %rdi                                    #217.1
        xorl      %esi, %esi                                    #217.1
        movl      $1848, %edx                                   #217.1
        call      _intel_fast_memset                            #217.1
                                # LOE rbx rbp r12 r13
..B1.34:                        # Preds ..B1.33
        incq      %r12                                          #217.1
        cmpq      $3, %r12                                      #217.1
        jb        ..B1.33       # Prob 66%                      #217.1
                                # LOE rbx rbp r12 r13
..B1.35:                        # Preds ..B1.34
        xorl      %r12d, %r12d                                  #218.1
        testq     %rbx, %rbx                                    #218.1
        jle       ..B1.44       # Prob 2%                       #218.1
                                # LOE rbx rbp r12
..B1.36:                        # Preds ..B1.35
        movq      %rbp, 168(%rsp)                               #246.13
        movsd     .L_2il0floatpacket.0(%rip), %xmm8             #239.12
        movq      328(%rsp), %rbp                               #246.13
        movq      320(%rsp), %rdx                               #246.13
        movq      312(%rsp), %r13                               #246.13
        movq      304(%rsp), %rcx                               #246.13
                                # LOE rdx rcx rbx rbp r12 r13 xmm8
..B1.37:                        # Preds ..B1.42 ..B1.36
        movslq    nb14_$NB14_LJ6_IS.0.1(,%r12,4), %r14          #219.3
        movslq    nb14_$NB14_LJ6_LS.0.1(,%r12,4), %r15          #220.3
        movsd     nb14_$NB14_LJ6_SCQIQJS.0.1(,%r12,8), %xmm2    #221.3
        movsd     -8+coord_(,%r14,8), %xmm5                     #225.3
        movsd     1840+coord_(,%r14,8), %xmm7                   #225.3
        movsd     3688+coord_(,%r14,8), %xmm6                   #225.3
        movsd     -8+coord_(,%r15,8), %xmm0                     #226.3
        movsd     1840+coord_(,%r15,8), %xmm1                   #226.3
        movsd     3688+coord_(,%r15,8), %xmm9                   #226.3
        movsd     nb14_$NB14_LJ6_SCCN1S.0.1(,%r12,8), %xmm3     #222.3
        movsd     nb14_$NB14_LJ6_SCCN2S.0.1(,%r12,8), %xmm4     #223.3
        movsd     %xmm5, nb14_$POS_I.0.1(%rip)                  #225.3
        movsd     %xmm7, 8+nb14_$POS_I.0.1(%rip)                #225.3
        movsd     %xmm6, 16+nb14_$POS_I.0.1(%rip)               #225.3
        movsd     %xmm0, nb14_$POS_L.0.1(%rip)                  #226.3
        movsd     %xmm1, 8+nb14_$POS_L.0.1(%rip)                #226.3
        movsd     %xmm9, 16+nb14_$POS_L.0.1(%rip)               #226.3
        testb     $1, 168+pbcbox_(%rip)                         #227.8
        je        ..B1.40       # Prob 60%                      #227.8
                                # LOE rdx rcx rbx rbp r12 r13 r14 r15 xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 xmm8 xmm9
..B1.38:                        # Preds ..B1.37
        movl      $nb14_$POS_I.0.1, %edi                        #230.10
        movl      $nb14_$POS_L.0.1, %esi                        #230.10
        xorl      %eax, %eax                                    #230.10
        movsd     %xmm2, 144(%rsp)                              #230.10
        movsd     %xmm4, 152(%rsp)                              #230.10
        movsd     %xmm3, 160(%rsp)                              #230.10
..___tag_value_nb14_.22:
        call      pbc_                                          #230.10
..___tag_value_nb14_.23:
                                # LOE rbx rbp r12 r13 r14 r15
..B1.39:                        # Preds ..B1.38
        movq      304(%rsp), %rcx                               #
        movq      320(%rsp), %rdx                               #
        movsd     .L_2il0floatpacket.0(%rip), %xmm8             #
        movsd     160(%rsp), %xmm3                              #
        movsd     152(%rsp), %xmm4                              #
        movsd     144(%rsp), %xmm2                              #
        movsd     16+nb14_$POS_I.0.1(%rip), %xmm6               #233.3
        movsd     nb14_$POS_I.0.1(%rip), %xmm5                  #233.3
        movsd     8+nb14_$POS_I.0.1(%rip), %xmm7                #233.3
        movsd     16+nb14_$POS_L.0.1(%rip), %xmm9               #233.3
        movsd     nb14_$POS_L.0.1(%rip), %xmm0                  #233.3
        movsd     8+nb14_$POS_L.0.1(%rip), %xmm1                #233.3
                                # LOE rdx rcx rbx rbp r12 r13 r14 r15 xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 xmm8 xmm9
..B1.40:                        # Preds ..B1.39 ..B1.37
        pxor      %xmm10, %xmm10                                #235.10
        subsd     %xmm0, %xmm5                                  #233.3
        subsd     %xmm1, %xmm7                                  #233.3
        subsd     %xmm9, %xmm6                                  #233.3
        movaps    %xmm5, %xmm1                                  #234.10
        movaps    %xmm7, %xmm0                                  #234.10
        mulsd     %xmm5, %xmm1                                  #234.10
        movaps    %xmm6, %xmm9                                  #234.10
        mulsd     %xmm7, %xmm0                                  #234.10
        mulsd     %xmm6, %xmm9                                  #234.10
        addsd     %xmm0, %xmm1                                  #234.10
        movsd     %xmm5, nb14_$RLI.0.1(%rip)                    #233.3
        addsd     %xmm9, %xmm1                                  #234.10
        sqrtsd    %xmm1, %xmm10                                 #235.10
        movaps    %xmm1, %xmm0                                  #237.15
        mulsd     %xmm1, %xmm0                                  #237.15
        comisd    %xmm8, %xmm10                                 #239.12
        mulsd     %xmm1, %xmm0                                  #237.3
        movsd     %xmm7, 8+nb14_$RLI.0.1(%rip)                  #233.3
        movsd     %xmm6, 16+nb14_$RLI.0.1(%rip)                 #233.3
        ja        ..B1.42       # Prob 50%                      #239.12
                                # LOE rdx rcx rbx rbp r12 r13 r14 r15 xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 xmm8 xmm10
..B1.41:                        # Preds ..B1.40
        movaps    %xmm2, %xmm9                                  #240.43
        movaps    %xmm5, %xmm13                                 #241.3
        divsd     %xmm10, %xmm9                                 #240.43
        mulsd     %xmm1, %xmm10                                 #236.3
        movaps    %xmm7, %xmm15                                 #241.3
        addsd     (%rcx), %xmm9                                 #240.3
        mulsd     %xmm0, %xmm1                                  #246.60
        divsd     %xmm10, %xmm2                                 #241.23
        mulsd     %xmm2, %xmm13                                 #241.3
        mulsd     %xmm2, %xmm15                                 #241.3
        mulsd     %xmm6, %xmm2                                  #241.3
        movsd     -8(%r13,%r14,8), %xmm10                       #242.3
        movsd     1840(%r13,%r14,8), %xmm11                     #242.3
        addsd     %xmm13, %xmm10                                #242.3
        addsd     %xmm15, %xmm11                                #242.3
        movsd     3688(%r13,%r14,8), %xmm12                     #242.3
        movsd     %xmm10, -8(%r13,%r14,8)                       #242.3
        addsd     %xmm2, %xmm12                                 #242.3
        movsd     %xmm11, 1840(%r13,%r14,8)                     #242.3
        movsd     %xmm12, 3688(%r13,%r14,8)                     #242.3
        movsd     3688(%r13,%r15,8), %xmm10                     #243.3
        movsd     %xmm9, (%rcx)                                 #240.3
        subsd     %xmm2, %xmm10                                 #243.3
        movaps    %xmm3, %xmm2                                  #244.33
        addsd     %xmm3, %xmm3                                  #246.18
        divsd     %xmm0, %xmm2                                  #244.33
        divsd     %xmm0, %xmm3                                  #246.31
        movsd     -8(%r13,%r15,8), %xmm14                       #243.3
        subsd     %xmm4, %xmm3                                  #246.38
        subsd     %xmm4, %xmm2                                  #244.40
        subsd     %xmm13, %xmm14                                #243.3
        mulsd     .L_2il0floatpacket.1(%rip), %xmm3             #246.13
        divsd     %xmm0, %xmm2                                  #244.54
        divsd     %xmm1, %xmm3                                  #246.52
        mulsd     %xmm3, %xmm5                                  #246.3
        addsd     (%rdx), %xmm2                                 #244.3
        mulsd     %xmm3, %xmm7                                  #246.3
        mulsd     %xmm3, %xmm6                                  #246.3
        movsd     -8(%rbp,%r14,8), %xmm0                        #247.3
        movsd     1840(%rbp,%r14,8), %xmm1                      #247.3
        addsd     %xmm5, %xmm0                                  #247.3
        addsd     %xmm7, %xmm1                                  #247.3
        movsd     3688(%rbp,%r14,8), %xmm3                      #247.3
        movsd     %xmm0, -8(%rbp,%r14,8)                        #247.3
        addsd     %xmm6, %xmm3                                  #247.3
        movsd     %xmm1, 1840(%rbp,%r14,8)                      #247.3
        movsd     %xmm3, 3688(%rbp,%r14,8)                      #247.3
        movsd     -8(%rbp,%r15,8), %xmm4                        #248.3
        movsd     %xmm5, nb14_$FORCE.0.1(%rip)                  #246.3
        subsd     %xmm5, %xmm4                                  #248.3
        movsd     1840(%rbp,%r15,8), %xmm5                      #248.3
        movsd     %xmm7, 8+nb14_$FORCE.0.1(%rip)                #246.3
        subsd     %xmm7, %xmm5                                  #248.3
        movsd     1840(%r13,%r15,8), %xmm9                      #243.3
        movsd     3688(%rbp,%r15,8), %xmm7                      #248.3
        subsd     %xmm15, %xmm9                                 #243.3
        subsd     %xmm6, %xmm7                                  #248.3
        movsd     %xmm14, -8(%r13,%r15,8)                       #243.3
        movsd     %xmm9, 1840(%r13,%r15,8)                      #243.3
        movsd     %xmm10, 3688(%r13,%r15,8)                     #243.3
        movsd     %xmm2, (%rdx)                                 #244.3
        movsd     %xmm6, 16+nb14_$FORCE.0.1(%rip)               #246.3
        movsd     %xmm4, -8(%rbp,%r15,8)                        #248.3
        movsd     %xmm5, 1840(%rbp,%r15,8)                      #248.3
        movsd     %xmm7, 3688(%rbp,%r15,8)                      #248.3
                                # LOE rdx rcx rbx rbp r12 r13 xmm8
..B1.42:                        # Preds ..B1.40 ..B1.41
        incq      %r12                                          #218.1
        cmpq      %rbx, %r12                                    #218.1
        jb        ..B1.37       # Prob 99%                      #218.1
                                # LOE rdx rcx rbx rbp r12 r13 xmm8
..B1.43:                        # Preds ..B1.42
        movq      168(%rsp), %rbp                               #
                                # LOE rbp
..B1.44:                        # Preds ..B1.35 ..B1.43
        xorl      %ebx, %ebx                                    #250.1
        testq     %rbp, %rbp                                    #250.1
        jle       ..B1.53       # Prob 2%                       #250.1
                                # LOE rbx rbp
..B1.45:                        # Preds ..B1.44
        movq      %rbp, 168(%rsp)                               #277.16
        movq      320(%rsp), %r12                               #277.16
        movq      312(%rsp), %rbp                               #277.16
        movq      304(%rsp), %r13                               #277.16
                                # LOE rbx rbp r12 r13
..B1.46:                        # Preds ..B1.51 ..B1.45
        movslq    nb14_$NB14_LJ10_IS.0.1(,%rbx,4), %r14         #257.5
        movslq    nb14_$NB14_LJ10_LS.0.1(,%rbx,4), %r15         #258.5
        movsd     nb14_$NB14_LJ10_QIS.0.1(,%rbx,8), %xmm1       #259.5
        movsd     -8+coord_(,%r14,8), %xmm3                     #266.5
        movsd     -8+coord_(,%r15,8), %xmm4                     #267.5
        movsd     nb14_$NB14_LJ10_QLS.0.1(,%rbx,8), %xmm0       #260.5
        movsd     nb14_$NB14_LJ10_SCNBS.0.1(,%rbx,8), %xmm2     #264.5
        movhpd    1840+coord_(,%r14,8), %xmm3                   #266.5
        movhpd    1840+coord_(,%r15,8), %xmm4                   #267.5
        movq      3688+coord_(,%r14,8), %rax                    #266.5
        movq      3688+coord_(,%r15,8), %rdx                    #267.5
        movsd     %xmm1, 160(%rsp)                              #259.5
        movsd     %xmm0, 144(%rsp)                              #260.5
        movsd     nb14_$NB14_LJ10_SCEES.0.1(,%rbx,8), %xmm1     #261.5
        movsd     %xmm2, 152(%rsp)                              #264.5
        movaps    %xmm3, nb14_$POS_I.0.1(%rip)                  #266.5
        movaps    %xmm4, nb14_$POS_L.0.1(%rip)                  #267.5
        movq      %rax, 16+nb14_$POS_I.0.1(%rip)                #266.5
        movq      %rdx, 16+nb14_$POS_L.0.1(%rip)                #267.5
        testb     $1, 168+pbcbox_(%rip)                         #268.10
        je        ..B1.48       # Prob 60%                      #268.10
                                # LOE rbx rbp r12 r13 r14 r15 xmm1
..B1.47:                        # Preds ..B1.46
        movl      $nb14_$POS_I.0.1, %edi                        #271.12
        movl      $nb14_$POS_L.0.1, %esi                        #271.12
        xorl      %eax, %eax                                    #271.12
        movsd     %xmm1, 192(%rsp)                              #271.12
..___tag_value_nb14_.24:
        call      pbc_                                          #271.12
..___tag_value_nb14_.25:
                                # LOE rbx rbp r12 r13 r14 r15
..B1.69:                        # Preds ..B1.47
        movsd     192(%rsp), %xmm1                              #
                                # LOE rbx rbp r12 r13 r14 r15 xmm1
..B1.48:                        # Preds ..B1.46 ..B1.69
        movl      $nb14_$POS_I.0.1, %edi                        #275.11
        movl      $nb14_$POS_L.0.1, %esi                        #275.11
        xorl      %eax, %eax                                    #275.11
        movsd     %xmm1, 192(%rsp)                              #275.11
..___tag_value_nb14_.26:
        call      length_                                       #275.11
..___tag_value_nb14_.27:
                                # LOE rbx rbp r12 r13 r14 r15 xmm0
..B1.66:                        # Preds ..B1.48
        movsd     192(%rsp), %xmm1                              #
        movaps    %xmm0, %xmm2                                  #275.11
                                # LOE rbx rbp r12 r13 r14 r15 xmm1 xmm2 ymm1 zmm1
..B1.49:                        # Preds ..B1.66
        movsd     .L_2il0floatpacket.0(%rip), %xmm0             #277.16
        comisd    %xmm2, %xmm0                                  #277.16
        jb        ..B1.51       # Prob 50%                      #277.16
                                # LOE rbx rbp r12 r13 r14 r15 xmm1 xmm2 ymm1 zmm1
..B1.50:                        # Preds ..B1.49
        movsd     160(%rsp), %xmm5                              #283.19
        movaps    %xmm2, %xmm0                                  #291.25
        mulsd     144(%rsp), %xmm5                              #283.19
        mulsd     %xmm2, %xmm0                                  #291.25
        movaps    %xmm5, %xmm4                                  #283.29
        movaps    %xmm0, %xmm11                                 #304.22
        divsd     %xmm2, %xmm4                                  #283.29
        mulsd     %xmm0, %xmm2                                  #291.25
        pxor      %xmm15, %xmm15                                #304.17
        mulsd     %xmm1, %xmm4                                  #284.15
        mulsd     %xmm0, %xmm11                                 #304.22
        divsd     %xmm2, %xmm5                                  #291.17
        mulsd     %xmm5, %xmm1                                  #291.32
        movaps    %xmm11, %xmm14                                #304.22
        addsd     (%r13), %xmm4                                 #282.9
        mulsd     %xmm11, %xmm14                                #304.22
        mulsd     %xmm11, %xmm0                                 #304.40
        movaps    nb14_$POS_I.0.1(%rip), %xmm2                  #291.32
        movaps    %xmm1, %xmm8                                  #291.32
        subpd     nb14_$POS_L.0.1(%rip), %xmm2                  #292.27
        unpcklpd  %xmm8, %xmm8                                  #291.32
        pxor      %xmm12, %xmm12                                #304.35
        movsd     16+nb14_$POS_I.0.1(%rip), %xmm3               #291.32
        mulpd     %xmm2, %xmm8                                  #290.9
        subsd     16+nb14_$POS_L.0.1(%rip), %xmm3               #292.27
        mulsd     %xmm3, %xmm1                                  #290.9
        movsd     -8(%rbp,%r14,8), %xmm6                        #293.9
        movhpd    1840(%rbp,%r14,8), %xmm6                      #293.9
        addpd     %xmm8, %xmm6                                  #293.9
        movsd     3688(%rbp,%r14,8), %xmm7                      #293.9
        movsd     %xmm6, -8(%rbp,%r14,8)                        #293.9
        addsd     %xmm1, %xmm7                                  #293.9
        movhpd    %xmm6, 1840(%rbp,%r14,8)                      #293.9
        movsd     %xmm7, 3688(%rbp,%r14,8)                      #293.9
        movsd     3688(%rbp,%r15,8), %xmm10                     #294.9
        movq      328(%rsp), %rax                               #320.9
        subsd     %xmm1, %xmm10                                 #294.9
        movaps    %xmm14, %xmm1                                 #304.22
        mulsd     %xmm11, %xmm1                                 #304.22
        mulsd     %xmm0, %xmm11                                 #304.40
        mulsd     %xmm0, %xmm14                                 #315.36
        divsd     %xmm1, %xmm15                                 #304.17
        pxor      %xmm0, %xmm0                                  #315.28
        movaps    %xmm15, %xmm13                                #304.27
        divsd     %xmm11, %xmm12                                #304.35
        divsd     %xmm14, %xmm0                                 #315.28
        movsd     152(%rsp), %xmm1                              #306.9
        subsd     %xmm12, %xmm13                                #304.27
        subsd     %xmm15, %xmm0                                 #316.11
        mulsd     %xmm1, %xmm13                                 #306.9
        mulsd     %xmm0, %xmm1                                  #318.11
        addsd     (%r12), %xmm13                                #303.9
        mulsd     %xmm1, %xmm3                                  #314.9
        movaps    %xmm1, %xmm5                                  #318.11
        unpcklpd  %xmm5, %xmm5                                  #318.11
        mulpd     %xmm2, %xmm5                                  #314.9
        movsd     -8(%rax,%r14,8), %xmm2                        #320.9
        movhpd    1840(%rax,%r14,8), %xmm2                      #320.9
        addpd     %xmm5, %xmm2                                  #320.9
        movsd     %xmm4, (%r13)                                 #282.9
        movsd     3688(%rax,%r14,8), %xmm4                      #320.9
        movsd     %xmm2, -8(%rax,%r14,8)                        #320.9
        addsd     %xmm3, %xmm4                                  #320.9
        movhpd    %xmm2, 1840(%rax,%r14,8)                      #320.9
        movsd     %xmm4, 3688(%rax,%r14,8)                      #320.9
        movsd     -8(%rbp,%r15,8), %xmm9                        #294.9
        movsd     -8(%rax,%r15,8), %xmm6                        #321.9
        movhpd    1840(%rbp,%r15,8), %xmm9                      #294.9
        movhpd    1840(%rax,%r15,8), %xmm6                      #321.9
        subpd     %xmm8, %xmm9                                  #294.9
        subpd     %xmm5, %xmm6                                  #321.9
        movsd     3688(%rax,%r15,8), %xmm7                      #321.9
        movsd     %xmm9, -8(%rbp,%r15,8)                        #294.9
        subsd     %xmm3, %xmm7                                  #321.9
        movhpd    %xmm9, 1840(%rbp,%r15,8)                      #294.9
        movsd     %xmm10, 3688(%rbp,%r15,8)                     #294.9
        movsd     %xmm13, (%r12)                                #303.9
        movaps    %xmm5, nb14_$FORCE.0.1(%rip)                  #314.9
        movsd     %xmm3, 16+nb14_$FORCE.0.1(%rip)               #314.9
        movsd     %xmm6, -8(%rax,%r15,8)                        #321.9
        movhpd    %xmm6, 1840(%rax,%r15,8)                      #321.9
        movsd     %xmm7, 3688(%rax,%r15,8)                      #321.9
                                # LOE rbx rbp r12 r13
..B1.51:                        # Preds ..B1.49 ..B1.50
        incq      %rbx                                          #250.1
        cmpq      168(%rsp), %rbx                               #250.1
        jb        ..B1.46       # Prob 99%                      #250.1
                                # LOE rbx rbp r12 r13
..B1.53:                        # Preds ..B1.51 ..B1.44
        movq      304(%rsp), %rax                               #327.1
        lea       (%rsp), %rdi                                  #327.1
        movl      $98, %esi                                     #327.1
        movl      $__STRLITPACK_167.0.1, %ecx                   #327.1
        movq      $0, (%rdi)                                    #327.1
        lea       192(%rsp), %r8                                #327.1
        movq      (%rax), %rdx                                  #327.1
        xorl      %eax, %eax                                    #327.1
        movq      %rdx, 192(%rdi)                               #327.1
        movq      $0x1208384ff00, %rdx                          #327.1
        call      for_write_seq_lis                             #327.1
                                # LOE
..B1.54:                        # Preds ..B1.53
        movq      312(%rsp), %rax                               #328.1
        lea       (%rsp), %rdi                                  #328.1
        movq      %rax, 152(%rdi)                               #328.1
        movl      $98, %esi                                     #328.1
        movq      $0x1208384ff00, %rdx                          #328.1
        movl      $__STRLITPACK_168.0.1, %ecx                   #328.1
        xorl      %eax, %eax                                    #328.1
        lea       144(%rsp), %r8                                #328.1
        movq      $0, (%rdi)                                    #328.1
        movq      $5544, 144(%rdi)                              #328.1
        call      for_write_seq_lis                             #328.1
                                # LOE
..B1.55:                        # Preds ..B1.54
        movq      320(%rsp), %rax                               #329.1
        lea       (%rsp), %rdi                                  #329.1
        movl      $98, %esi                                     #329.1
        movl      $__STRLITPACK_169.0.1, %ecx                   #329.1
        movq      $0, (%rdi)                                    #329.1
        lea       200(%rsp), %r8                                #329.1
        movq      (%rax), %rdx                                  #329.1
        xorl      %eax, %eax                                    #329.1
        movq      %rdx, 200(%rdi)                               #329.1
        movq      $0x1208384ff00, %rdx                          #329.1
        call      for_write_seq_lis                             #329.1
                                # LOE
..B1.56:                        # Preds ..B1.55
        movq      328(%rsp), %rax                               #330.1
        lea       (%rsp), %rdi                                  #330.1
        movq      %rax, 168(%rdi)                               #330.1
        movl      $98, %esi                                     #330.1
        movq      $0x1208384ff00, %rdx                          #330.1
        movl      $__STRLITPACK_170.0.1, %ecx                   #330.1
        xorl      %eax, %eax                                    #330.1
        lea       160(%rsp), %r8                                #330.1
        movq      $0, (%rdi)                                    #330.1
        movq      $5544, 160(%rdi)                              #330.1
        call      for_write_seq_lis                             #330.1
                                # LOE
..B1.57:                        # Preds ..B1.56
        xorl      %eax, %eax                                    #333.6
..___tag_value_nb14_.28:
        call      clock_pop_                                    #333.6
..___tag_value_nb14_.29:
                                # LOE
..B1.58:                        # Preds ..B1.57
        addq      $216, %rsp                                    #336.1
	.cfi_def_cfa_offset 56
	.cfi_restore 6
        popq      %rbp                                          #336.1
	.cfi_def_cfa_offset 48
	.cfi_restore 3
        popq      %rbx                                          #336.1
	.cfi_def_cfa_offset 40
	.cfi_restore 15
        popq      %r15                                          #336.1
	.cfi_def_cfa_offset 32
	.cfi_restore 14
        popq      %r14                                          #336.1
	.cfi_def_cfa_offset 24
	.cfi_restore 13
        popq      %r13                                          #336.1
	.cfi_def_cfa_offset 16
	.cfi_restore 12
        popq      %r12                                          #336.1
	.cfi_def_cfa_offset 8
        ret                                                     #336.1
	.cfi_def_cfa_offset 272
	.cfi_offset 3, -48
	.cfi_offset 6, -56
	.cfi_offset 12, -16
	.cfi_offset 13, -24
	.cfi_offset 14, -32
	.cfi_offset 15, -40
                                # LOE
..B1.59:                        # Preds ..B1.18                 # Infreq
        movsd     .L_2il0floatpacket.0(%rip), %xmm0             #154.16
        comisd    %xmm1, %xmm0                                  #154.16
        jb        ..B1.61       # Prob 50%                      #154.16
                                # LOE rcx rbx rbp r8 r12 r13 r14 r15 xmm1 xmm6 xmm7 xmm8
..B1.60:                        # Preds ..B1.59                 # Infreq
        movaps    %xmm7, %xmm12                                 #156.26
        movaps    %xmm1, %xmm0                                  #161.25
        mulsd     %xmm6, %xmm12                                 #156.26
        mulsd     %xmm1, %xmm0                                  #161.25
        movaps    %xmm12, %xmm4                                 #156.43
        movaps    %xmm0, %xmm15                                 #174.22
        divsd     %xmm1, %xmm4                                  #156.43
        mulsd     %xmm0, %xmm1                                  #161.25
        mulsd     %xmm0, %xmm15                                 #174.22
        mulsd     %xmm8, %xmm4                                  #157.15
        divsd     %xmm1, %xmm12                                 #161.17
        mulsd     %xmm8, %xmm12                                 #161.32
        movaps    %xmm15, %xmm14                                #174.22
        mulsd     %xmm15, %xmm0                                 #174.40
        movaps    nb14_$POS_I.0.1(%rip), %xmm1                  #161.32
        movaps    %xmm12, %xmm10                                #161.32
        subpd     nb14_$POS_L.0.1(%rip), %xmm1                  #162.27
        unpcklpd  %xmm10, %xmm10                                #161.32
        movq      272(%rsp), %rsi                               #176.11
        movsd     16+nb14_$POS_I.0.1(%rip), %xmm2               #161.32
        mulpd     %xmm1, %xmm10                                 #160.9
        subsd     16+nb14_$POS_L.0.1(%rip), %xmm2               #162.27
        movsd     -8(%rsi,%r8,8), %xmm3                         #176.11
        movq      312(%rsp), %r8                                #163.9
        movq      304(%rsp), %rdi                               #155.9
        movq      328(%rsp), %r10                               #184.9
        movsd     -8(%r8,%rbp,8), %xmm5                         #163.9
        movhpd    1840(%r8,%rbp,8), %xmm5                       #163.9
        addpd     %xmm10, %xmm5                                 #163.9
        mulsd     %xmm2, %xmm12                                 #160.9
        addsd     (%rdi), %xmm4                                 #155.9
        movsd     %xmm5, -8(%r8,%rbp,8)                         #163.9
        movhpd    %xmm5, 1840(%r8,%rbp,8)                       #163.9
        movaps    %xmm15, %xmm5                                 #174.22
        movsd     3688(%r8,%rbp,8), %xmm9                       #163.9
        mulsd     %xmm15, %xmm5                                 #174.22
        addsd     %xmm12, %xmm9                                 #163.9
        mulsd     %xmm0, %xmm15                                 #174.40
        mulsd     %xmm5, %xmm14                                 #174.22
        mulsd     %xmm5, %xmm0                                  #179.36
        movsd     %xmm9, 3688(%r8,%rbp,8)                       #163.9
        movsd     -8(%r8,%r12,8), %xmm11                        #164.9
        pxor      %xmm9, %xmm9                                  #174.17
        movhpd    1840(%r8,%r12,8), %xmm11                      #164.9
        subpd     %xmm10, %xmm11                                #164.9
        divsd     %xmm14, %xmm9                                 #174.17
        pxor      %xmm10, %xmm10                                #179.28
        pxor      %xmm14, %xmm14                                #174.35
        divsd     %xmm0, %xmm10                                 #179.28
        divsd     %xmm15, %xmm14                                #174.35
        movsd     %xmm11, -8(%r8,%r12,8)                        #164.9
        subsd     %xmm9, %xmm10                                 #180.11
        mulsd     %xmm3, %xmm10                                 #182.11
        movhpd    %xmm11, 1840(%r8,%r12,8)                      #164.9
        movaps    %xmm10, %xmm11                                #182.11
        unpcklpd  %xmm11, %xmm11                                #182.11
        mulpd     %xmm1, %xmm11                                 #178.9
        mulsd     %xmm10, %xmm2                                 #178.9
        movsd     -8(%r10,%rbp,8), %xmm1                        #184.9
        movhpd    1840(%r10,%rbp,8), %xmm1                      #184.9
        addpd     %xmm11, %xmm1                                 #184.9
        movsd     %xmm4, (%rdi)                                 #155.9
        movaps    %xmm9, %xmm4                                  #174.27
        movsd     3688(%r10,%rbp,8), %xmm0                      #184.9
        subsd     %xmm14, %xmm4                                 #174.27
        addsd     %xmm2, %xmm0                                  #184.9
        mulsd     %xmm3, %xmm4                                  #176.9
        movsd     3688(%r8,%r12,8), %xmm13                      #164.9
        movsd     %xmm1, -8(%r10,%rbp,8)                        #184.9
        subsd     %xmm12, %xmm13                                #164.9
        movhpd    %xmm1, 1840(%r10,%rbp,8)                      #184.9
        movsd     %xmm0, 3688(%r10,%rbp,8)                      #184.9
        movsd     -8(%r10,%r12,8), %xmm12                       #185.9
        movq      320(%rsp), %r9                                #173.9
        movhpd    1840(%r10,%r12,8), %xmm12                     #185.9
        movsd     %xmm13, 3688(%r8,%r12,8)                      #164.9
        subpd     %xmm11, %xmm12                                #185.9
        addsd     (%r9), %xmm4                                  #173.9
        movsd     3688(%r10,%r12,8), %xmm13                     #185.9
        movsd     %xmm4, (%r9)                                  #173.9
        subsd     %xmm2, %xmm13                                 #185.9
        movaps    %xmm11, nb14_$FORCE.0.1(%rip)                 #178.9
        movsd     %xmm2, 16+nb14_$FORCE.0.1(%rip)               #178.9
        movsd     %xmm12, -8(%r10,%r12,8)                       #185.9
        movhpd    %xmm12, 1840(%r10,%r12,8)                     #185.9
        movsd     %xmm13, 3688(%r10,%r12,8)                     #185.9
        jmp       ..B1.62       # Prob 100%                     #185.9
                                # LOE rcx rbx rbp r12 r13 r14 r15 xmm3 xmm6 xmm7 xmm8
..B1.61:                        # Preds ..B1.59                 # Infreq
        movq      272(%rsp), %rsi                               #196.7
        movsd     -8(%rsi,%r8,8), %xmm3                         #196.7
                                # LOE rcx rbx rbp r12 r13 r14 r15 xmm3 xmm6 xmm7 xmm8
..B1.62:                        # Preds ..B1.60 ..B1.61         # Infreq
        movq      168(%rsp), %rsi                               #189.7
        movsd     %xmm8, -8+nb14_$NB14_LJ10_SCEES.0.1(,%rbx,8)  #193.7
        pxor      %xmm0, %xmm0                                  #194.7
        mulsd     %xmm7, %xmm8                                  #198.24
        pxor      %xmm1, %xmm1                                  #195.7
        movl      %ebp, nb14_$NB14_LJ10_IS.0.1(,%rsi,4)         #189.7
        movq      120(%rsp), %rbp                               #200.9
        movl      %r12d, nb14_$NB14_LJ10_LS.0.1(,%rsi,4)        #190.7
        incq      %rsi                                          #188.7
        movsd     %xmm3, -8+nb14_$NB14_LJ10_SCNBS.0.1(,%rbx,8)  #196.7
        movq      112(%rsp), %r12                               #202.9
        movsd     -8(%rbp,%r13,8), %xmm2                        #200.9
        movq      %rsi, 168(%rsp)                               #188.7
        mulsd     %xmm6, %xmm8                                  #197.7
        mulsd     %xmm3, %xmm2                                  #199.7
        mulsd     -8(%r12,%r13,8), %xmm3                        #201.7
        movsd     %xmm7, -8+nb14_$NB14_LJ10_QIS.0.1(,%rbx,8)    #191.7
        movsd     %xmm6, -8+nb14_$NB14_LJ10_QLS.0.1(,%rbx,8)    #192.7
        movsd     %xmm0, -8+nb14_$NB14_LJ10_ASOLS.0.1(,%rbx,8)  #194.7
        movsd     %xmm1, -8+nb14_$NB14_LJ10_BSOLS.0.1(,%rbx,8)  #195.7
        movsd     %xmm8, -8+nb14_$NB14_LJ10_SCQIQJS.0.1(,%rsi,8) #197.7
        movsd     %xmm2, -8+nb14_$NB14_LJ10_SCASOLS.0.1(,%rsi,8) #199.7
        movsd     %xmm3, -8+nb14_$NB14_LJ10_SCBSOLS.0.1(,%rsi,8) #201.7
        jmp       ..B1.23       # Prob 100%                     #201.7
        .align    16,0x90
                                # LOE rcx rbx r14 r15
	.cfi_endproc
# mark_end;
	.type	nb14_,@function
	.size	nb14_,.-nb14_
	.bss
	.align 32
	.align 32
nb14_$NB14_LJ6_IS.0.1:
	.type	nb14_$NB14_LJ6_IS.0.1,@object
	.size	nb14_$NB14_LJ6_IS.0.1,592
	.space 592	# pad
	.space 16	# pad
	.align 32
nb14_$NB14_LJ6_LS.0.1:
	.type	nb14_$NB14_LJ6_LS.0.1,@object
	.size	nb14_$NB14_LJ6_LS.0.1,592
	.space 592	# pad
	.space 16	# pad
	.align 32
nb14_$NB14_LJ6_SCEES.0.1:
	.type	nb14_$NB14_LJ6_SCEES.0.1,@object
	.size	nb14_$NB14_LJ6_SCEES.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ6_CN1S.0.1:
	.type	nb14_$NB14_LJ6_CN1S.0.1,@object
	.size	nb14_$NB14_LJ6_CN1S.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ6_SCNBS.0.1:
	.type	nb14_$NB14_LJ6_SCNBS.0.1,@object
	.size	nb14_$NB14_LJ6_SCNBS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ6_QIS.0.1:
	.type	nb14_$NB14_LJ6_QIS.0.1,@object
	.size	nb14_$NB14_LJ6_QIS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ6_QLS.0.1:
	.type	nb14_$NB14_LJ6_QLS.0.1,@object
	.size	nb14_$NB14_LJ6_QLS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ6_CN2S.0.1:
	.type	nb14_$NB14_LJ6_CN2S.0.1,@object
	.size	nb14_$NB14_LJ6_CN2S.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ6_SCQIQJS.0.1:
	.type	nb14_$NB14_LJ6_SCQIQJS.0.1,@object
	.size	nb14_$NB14_LJ6_SCQIQJS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ6_SCCN1S.0.1:
	.type	nb14_$NB14_LJ6_SCCN1S.0.1,@object
	.size	nb14_$NB14_LJ6_SCCN1S.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ6_SCCN2S.0.1:
	.type	nb14_$NB14_LJ6_SCCN2S.0.1,@object
	.size	nb14_$NB14_LJ6_SCCN2S.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ10_IS.0.1:
	.type	nb14_$NB14_LJ10_IS.0.1,@object
	.size	nb14_$NB14_LJ10_IS.0.1,592
	.space 592	# pad
	.space 16	# pad
	.align 32
nb14_$NB14_LJ10_LS.0.1:
	.type	nb14_$NB14_LJ10_LS.0.1,@object
	.size	nb14_$NB14_LJ10_LS.0.1,592
	.space 592	# pad
	.space 16	# pad
	.align 32
nb14_$NB14_LJ10_QIS.0.1:
	.type	nb14_$NB14_LJ10_QIS.0.1,@object
	.size	nb14_$NB14_LJ10_QIS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ10_QLS.0.1:
	.type	nb14_$NB14_LJ10_QLS.0.1,@object
	.size	nb14_$NB14_LJ10_QLS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ10_SCNBS.0.1:
	.type	nb14_$NB14_LJ10_SCNBS.0.1,@object
	.size	nb14_$NB14_LJ10_SCNBS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ10_SCEES.0.1:
	.type	nb14_$NB14_LJ10_SCEES.0.1,@object
	.size	nb14_$NB14_LJ10_SCEES.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ10_ASOLS.0.1:
	.type	nb14_$NB14_LJ10_ASOLS.0.1,@object
	.size	nb14_$NB14_LJ10_ASOLS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ10_BSOLS.0.1:
	.type	nb14_$NB14_LJ10_BSOLS.0.1,@object
	.size	nb14_$NB14_LJ10_BSOLS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ10_SCQIQJS.0.1:
	.type	nb14_$NB14_LJ10_SCQIQJS.0.1,@object
	.size	nb14_$NB14_LJ10_SCQIQJS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ10_SCASOLS.0.1:
	.type	nb14_$NB14_LJ10_SCASOLS.0.1,@object
	.size	nb14_$NB14_LJ10_SCASOLS.0.1,1184
	.space 1184	# pad
	.align 32
nb14_$NB14_LJ10_SCBSOLS.0.1:
	.type	nb14_$NB14_LJ10_SCBSOLS.0.1,@object
	.size	nb14_$NB14_LJ10_SCBSOLS.0.1,1184
	.space 1184	# pad
	.align 16
nb14_$POS_I.0.1:
	.type	nb14_$POS_I.0.1,@object
	.size	nb14_$POS_I.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
nb14_$POS_L.0.1:
	.type	nb14_$POS_L.0.1,@object
	.size	nb14_$POS_L.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
nb14_$FORCE.0.1:
	.type	nb14_$FORCE.0.1,@object
	.size	nb14_$FORCE.0.1,24
	.space 24	# pad
	.space 8	# pad
	.align 16
nb14_$RLI.0.1:
	.type	nb14_$RLI.0.1,@object
	.size	nb14_$RLI.0.1,24
	.space 24	# pad
	.section .rodata, "a"
	.align 8
	.align 4
__STRLITPACK_108.0.1:
	.long	853048
	.long	1704201
	.long	1
	.byte	0
	.space 3, 0x00 	# pad
	.align 4
__STRLITPACK_109.0.1:
	.long	853048
	.long	1704201
	.long	1
	.byte	0
	.space 3, 0x00 	# pad
	.align 4
__STRLITPACK_141.0.1:
	.long	65840
	.byte	9
	.space 3, 0x00 	# pad
	.align 4
__STRLITPACK_142.0.1:
	.long	66864
	.byte	9
	.space 3, 0x00 	# pad
	.align 4
__STRLITPACK_143.0.1:
	.long	65840
	.byte	9
	.space 3, 0x00 	# pad
	.align 4
__STRLITPACK_144.0.1:
	.long	66864
	.byte	9
	.space 3, 0x00 	# pad
	.align 4
__STRLITPACK_167.0.1:
	.long	65840
	.byte	9
	.space 3, 0x00 	# pad
	.align 4
__STRLITPACK_168.0.1:
	.long	66864
	.byte	9
	.space 3, 0x00 	# pad
	.align 4
__STRLITPACK_169.0.1:
	.long	65840
	.byte	9
	.space 3, 0x00 	# pad
	.align 4
__STRLITPACK_170.0.1:
	.long	66864
	.byte	9
	.data
# -- End  nb14_
	.section .rodata, "a"
	.space 3, 0x00 	# pad
	.align 8
.L_2il0floatpacket.0:
	.long	0x00000000,0x40280000
	.type	.L_2il0floatpacket.0,@object
	.size	.L_2il0floatpacket.0,8
	.align 8
.L_2il0floatpacket.1:
	.long	0x00000000,0x40180000
	.type	.L_2il0floatpacket.1,@object
	.size	.L_2il0floatpacket.1,8
	.section .rodata.str1.4, "aMS",@progbits,1
	.align 4
	.align 4
__STRLITPACK_84:
	.long	875651694
	.byte	0
	.type	__STRLITPACK_84,@object
	.size	__STRLITPACK_84,5
	.space 3, 0x00 	# pad
	.align 4
__STRLITPACK_83:
	.long	1868981602
	.word	25970
	.byte	0
	.type	__STRLITPACK_83,@object
	.size	__STRLITPACK_83,7
	.space 1, 0x00 	# pad
	.align 4
__STRLITPACK_82:
	.long	1702127201
	.word	114
	.type	__STRLITPACK_82,@object
	.size	__STRLITPACK_82,6
	.data
	.comm coord_,6472,32
	.comm pbcbox_,180,32
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .eh_frame
	.section .eh_frame,"a",@progbits
.eh_frame_seg:
	.align 8
# End
