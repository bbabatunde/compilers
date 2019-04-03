 
  section .text
  extern equal 
  extern error
  extern print
  extern input
  global our_code_starts_here
fact:
  ;; Stack_setup for fact
  push ebp
  mov ebp, esp
  sub esp, 0x10
fun_fact_body: ; Body for fact
  mov eax, [ebp+8]
  mov [ebp-8], eax
  mov eax, 4
  mov [ebp-12], eax
  mov eax, [ebp-8]
  test eax, 0x1
  jnz near comparison_expected_a_number
  mov edx, [ebp-12]
  test edx, 0x1
  jnz near comparison_expected_a_number_EDX
  cmp eax, edx
  mov eax, 0xffffffff
  jl near less_18
  mov eax, 0x7fffffff
less_18:
  mov [ebp-4], eax
  mov eax, [ebp-4]
  cmp eax, 0xffffffff
  jne near if_false_5
if_true_5:
  mov eax, 2
  jmp near if_done_5
if_false_5:
  cmp eax, 0x7fffffff
  jne near error_not_boolean_if
  mov eax, [ebp+8]
  mov [ebp-20], eax
  mov eax, 2
  mov [ebp-24], eax
  mov eax, [ebp-20]
  test eax, 0x1
  jnz near arithmetic_expected_a_number
  mov edx, [ebp-24]
  test edx, 0x1
  jnz near arithmetic_expected_a_number_EDX
  sub eax, edx
  jo near overflow
  mov [ebp-16], eax
  ;; Prepare to call function fun_fact
  mov eax, [ebp-16]
  push eax ; Argument binop_18
  call fact
  add esp, 0x4
  mov [ebp-20], eax
  mov eax, [ebp+8]
  mov [ebp-24], eax
  mov eax, [ebp-20]
  mov [ebp-28], eax
  mov eax, [ebp-24]
  test eax, 0x1
  jnz near arithmetic_expected_a_number
  mov edx, [ebp-28]
  test edx, 0x1
  jnz near arithmetic_expected_a_number_EDX
  imul eax, edx
  jo near overflow
  sar eax, 1
  jo near overflow
if_done_5:
  ;; Clean up for fact
  add esp, 0x10
  mov esp, ebp
  pop ebp
  ret ; Return for fact
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
  ;; Prepare to call function fun_fact
  mov eax, 10
  push eax ; Argument 5
  call fact
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
