 
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
  sub esp, 4
  ;; body start
  ;; compile bindings start 
  jmp near inner_lambda_end_5
inner_lambda_5:
  push DWORD ebp
  mov ebp, esp
  sub esp, 4
  mov ecx, [ebp+8]
  sub ecx, 0x5
  mov edx, [ecx+12]
  mov [ebp-4], edx
  mov eax, [ebp+12]
  mov [ebp-12], eax
  mov eax, 4
  mov [ebp-16], eax
  mov eax, [ebp-12]
  test eax, 0x1
  jnz near comparison_expected_a_number
  mov edx, [ebp-16]
  test edx, 0x1
  jnz near comparison_expected_a_number_EDX
  cmp eax, edx
  mov eax, 0xffffffff
  jl near less_21
  mov eax, 0x7fffffff
less_21:
  mov [ebp-8], eax
  mov eax, [ebp-8]
  cmp eax, 0xffffffff
  jne near if_false_7
if_true_7:
  mov eax, 2
  jmp near if_done_7
if_false_7:
  cmp eax, 0x7fffffff
  jne near error_not_boolean_if
  mov eax, [ebp+12]
  mov [ebp-24], eax
  mov eax, 2
  mov [ebp-28], eax
  mov eax, [ebp-24]
  test eax, 0x1
  jnz near arithmetic_expected_a_number
  mov edx, [ebp-28]
  test edx, 0x1
  jnz near arithmetic_expected_a_number_EDX
  sub eax, edx
  jo near overflow
  mov [ebp-20], eax
  ;; calling functions
  mov ecx, [ebp-4]
  and ecx, 0x7
  cmp ecx, 0x5
  jne near error_not_closure
  mov eax, [ebp-4]
  sub eax, 0x5
  cmp DWORD [eax+0], 1
  jne near error_wrong_arity
  push DWORD [ebp-20]
  mov eax, [ebp-4]
  push DWORD eax
  sub eax, 0x5
  call [eax+4]
  add esp, 8
  mov [ebp-24], eax
  mov eax, [ebp+12]
  mov [ebp-28], eax
  mov eax, [ebp-24]
  mov [ebp-32], eax
  mov eax, [ebp-28]
  test eax, 0x1
  jnz near arithmetic_expected_a_number
  mov edx, [ebp-32]
  test edx, 0x1
  jnz near arithmetic_expected_a_number_EDX
  imul eax, edx
  jo near overflow
  sar eax, 1
  jo near overflow
if_done_7:
  mov esp, ebp
  pop ebp
  ret
inner_lambda_end_5:
  mov [esi+0], DWORD 1
  mov [esi+4], DWORD inner_lambda_5
  mov [esi+8], DWORD 0
  mov [esi+12], esi
  add [esi+12], DWORD 0x5
  mov eax, esi
  add eax, 0x5
  add esi, 16
  add esi, 0x7
  and esi, 0xfffffff8
  ;; move EAX to EBP
  mov [ebp-4], eax
  ;; compile bindings end 
  ;; calling functions
  mov ecx, [ebp-4]
  and ecx, 0x7
  cmp ecx, 0x5
  jne near error_not_closure
  mov eax, [ebp-4]
  sub eax, 0x5
  cmp DWORD [eax+0], 1
  jne near error_wrong_arity
  push DWORD 10
  mov eax, [ebp-4]
  push DWORD eax
  sub eax, 0x5
  call [eax+4]
  add esp, 8
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
error_not_closure:
  push eax
  push 8
  call error
error_wrong_arity:
  push eax
  push 8
  call error
