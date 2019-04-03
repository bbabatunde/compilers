 
  section .text
  extern equal 
  extern error
  extern print
  extern input
  global our_code_starts_here
our_code_starts_here:
  ;; heap start
  mov esi, [esp+4]
  add esi, 7
  and esi, 0xfffffff8
  ;; stack start
  push ebp
  mov ebp, esp
  sub esp, 12
  ;; body start
  ;; Prepare to call function fun_print
  mov eax, 2
  push eax ; Argument 1
  call print
  add esp, 0x4
  mov [ebp-4], eax
  ;; Prepare to call function fun_print
  mov eax, 2
  push eax ; Argument 1
  call print
  add esp, 0x4
  mov [ebp-8], eax
  mov eax, [ebp-4]
  mov [ebp-16], eax
  mov eax, [ebp-8]
  mov [ebp-20], eax
  mov eax, [ebp-16]
  test eax, 0x1
  jnz near arithmetic_expected_a_number
  mov edx, [ebp-20]
  test edx, 0x1
  jnz near arithmetic_expected_a_number_EDX
  sub eax, edx
  jo near overflow
  mov [ebp-12], eax
  ;; Prepare to call function fun_print
  mov eax, [ebp-12]
  push eax ; Argument binop_3
  call print
  add esp, 0x4
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
