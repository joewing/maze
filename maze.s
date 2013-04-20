// Maze generator in x86-64 assembly language
// Joe Wingbermuehle
// 2013-04-20

// The size of the maze (must be odd)
.set     width,   39
.set     height,  21

.globl   _main

.text
_main:
   pushq    %rbp
   movq     %rsp, %rbp

   // Seed the random number generator.
   xorq     %rdi, %rdi
   callq    _time
   movq     %rax, %rdi
   callq    _srand

   // Initialize the maze array.
   movq     $width * height, %rcx
   leaq     maze(%rip), %rdi
   xorb     %al, %al
   rep stosb
   incb     %al
   movq     $width, %rcx
   leaq     maze(%rip), %rdi
   push     %rcx
   rep stosb
   pop      %rcx
   leaq     maze + width * (height - 1)(%rip), %rdi
   rep stosb
   leaq     maze(%rip), %rdi
   movq     $width, %rcx
initialize_sides:
   movb     %al, (%rdi)
   addq     $width - 1, %rdi
   movb     %al, (%rdi)
   incq     %rdi
   loop     initialize_sides

   // Carve the maze
   leaq     maze + width * 2 + 2(%rip), %r12
   callq    carve_maze

   // Carve the start/finish.
   leaq     maze + width + 2(%rip), %rdi
   movb     $1, (%rdi)
   leaq     maze + width * (height - 2) + width - 3(%rip), %rdi
   movb     $1, (%rdi)

   // Display the maze.
   leaq     maze(%rip), %rbx
   movq     $height, %rcx
print_maze_y:
   movq     %rcx, %r12
   movq     $width, %rcx
print_maze_x:
   movq     %rcx, %r13
   leaq     wall_str(%rip), %rdi
   movb     (%rbx), %al
   or       %al, %al
   jz       print_maze_continue
   leaq     space_str(%rip), %rdi
print_maze_continue:
   xorq     %rsi, %rsi
   callq    _printf
   incq     %rbx
   movq     %r13, %rcx
   loop     print_maze_x
   leaq     nl_str(%rip), %rdi
   xorq     %rsi, %rsi
   callq    _printf
   movq     %r12, %rcx
   loop     print_maze_y

   // Return.
   xorq     %rax, %rax
   popq     %rbp
   ret

// Carve the maze.
carve_maze:
   pushq    %rbp
   movq     %rsp, %rbp
carve_maze_again:
   callq    _rand
   andq     $3 << 3, %rax
   movq     %rax, %r15
   movq     $4, %r14
   movq     %r12, %rdi
   movb     $1, (%rdi)
carve_maze_loop:
   leaq     offsets(%rip), %rsi
   addq     %r15, %rsi
   movq     (%rsi), %rdx
   movq     %rdx, %rax
   addq     %rdi, %rax
   movb     (%rax), %bl
   addq     %rax, %rdx
   orb      (%rdx), %bl
   jnz      carve_maze_continue
   incb     %bl
   movb     %bl, (%rax)
   movb     %bl, (%rdx)
   movq     %rdx, %r12
   pushq    %r12
   subq     $8, %rsp
   callq    carve_maze
   addq     $8, %rsp
   popq     %r12
   jmp      carve_maze_again
carve_maze_continue:
   addq     $8, %r15
   andq     $3 << 3, %r15
   decq     %r14
   jnz      carve_maze_loop
   popq     %rbp
   ret

.data
wall_str:   .asciz   "[]"
space_str:  .asciz   "  "
nl_str:     .asciz   "\n"
offsets:    .quad    1, -1, width, -width

.lcomm   maze, width * height + 8

