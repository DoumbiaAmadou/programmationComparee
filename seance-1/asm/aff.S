 .globl _start
 DATA:   .ascii  "Hello World\n"      
 _start: mov     $4      , %eax 
         mov     $1      , %ebx
         mov     $DATA   , %ecx
         mov     $12     , %edx
         int     $0x80         
 
         mov     $1      , %eax
         mov     $0      , %ebx
         int     $0x80         