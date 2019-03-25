 
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
  sub esp, 8
  ;; body start
  mov eax, 20
  mov [ebp-4], eax
  jmp near inner_lambda_end_6
inner_lambda_6:
  push DWORD ebp
  mov ebp, esp
  mov ecx, [ebp+8]
  sub ecx, 5
  mov eax, [ecx+12]
  mov [ebp-4], eax
  ;; function_body_start
  mov eax, [ebp-4]
  mov [ebp-4], eax
  mov eax, [ebp+0]
  mov [ebp-8], eax
  mov eax, [ebp-4]
  test eax, 0x1
  jnz near arithmetic_expected_a_number
  mov edx, [ebp-8]
  test edx, 0x1
  jnz near arithmetic_expected_a_number_EDX
  add eax, edx
  jo near overflow
  ;; function_body_end
  mov esp, ebp
  pop DWORD ebp
  ret
inner_lambda_end_6:
  mov [esi+0], DWORD 1
  mov [esi+4], DWORD inner_lambda_6
  mov [esi+8], DWORD 1
  mov eax, [ebp-4]
  mov [esi+12], eax
  mov eax, esi
  add eax, 0x5
  add esi, 7
  and esi, 0xfffffff8
  mov [ebp-8], eax
  ;; calling functions
  mov ecx, [ebp-8]
  and ecx, 0x7
  cmp ecx, 0x5
  jne near error_not_closure
  mov eax, [ebp-8]
  cmp eax, 1
  jne near error_wrong_arity
  push DWORD 20
  push DWORD eax
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
  push 10
  call error
error_wrong_arity:
  push eax
  push 11
  call error
