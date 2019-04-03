 
  section .text
  extern equal 
  extern error
  extern print
  extern input
  global our_code_starts_here
f0:
  ;; Stack_setup for f0
  push ebp
  mov ebp, esp
  sub esp, 0x4
fun_f0_body: ; Body for f0
  mov eax, 0
  ;; Clean up for f0
  add esp, 0x4
  mov esp, ebp
  pop ebp
  ret ; Return for f0
f1:
  ;; Stack_setup for f1
  push ebp
  mov ebp, esp
  sub esp, 0x4
fun_f1_body: ; Body for f1
  ;; Prepare for tailcall to function fun_f0
  jmp near fun_f0_body
  ;; Clean up for f1
  add esp, 0x4
  mov esp, ebp
  pop ebp
  ret ; Return for f1
f2:
  ;; Stack_setup for f2
  push ebp
  mov ebp, esp
  sub esp, 0x4
fun_f2_body: ; Body for f2
  ;; Prepare for tailcall to function fun_f1
  jmp near fun_f1_body
  ;; Clean up for f2
  add esp, 0x4
  mov esp, ebp
  pop ebp
  ret ; Return for f2
f3:
  ;; Stack_setup for f3
  push ebp
  mov ebp, esp
  sub esp, 0x4
fun_f3_body: ; Body for f3
  ;; Prepare for tailcall to function fun_f2
  jmp near fun_f2_body
  ;; Clean up for f3
  add esp, 0x4
  mov esp, ebp
  pop ebp
  ret ; Return for f3
our_code_starts_here:
  ;; heap start
  mov esi, [esp+4]
  add esi, 7
  and esi, 0xfffffff8
  ;; stack start
  push ebp
  mov ebp, esp
  sub esp, 0
  ;; body start
  ;; Prepare for tailcall to function fun_f3
  jmp near fun_f3_body
  ;; postlude start
  mov esp, ebp
  pop ebp
  ret
logic_expected_a_boolean:
  push eax
  push 1
  call error
logic_expected_a_boolean_edx:
  push edx
  push 1
  call error
error_not_boolean_if:
  push eax
  push 2
  call error
arithmetic_expected_a_number:
  push eax
  push 3
  call error
arithmetic_expected_a_number_EDX:
  push edx
  push 3
  call error
comparison_expected_a_number:
  push eax
  push 4
  call error
comparison_expected_a_number_EDX:
  push edx
  push 4
  call error
overflow:
  push eax
  push 5
  call error
error_not_tuple:
  push eax
  push 6
  call error
index_too_high:
  push edx
  push 7
  call error
index_too_low:
  push eax
  push 8
  call error
