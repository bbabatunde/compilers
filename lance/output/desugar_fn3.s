 
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
  ;; compile bindings start 
  jmp near inner_lambda_end_4
inner_lambda_4:
  push DWORD ebp
  mov ebp, esp
  sub esp, 4
  mov ecx, [ebp+8]
  sub ecx, 0x5
  mov edx, [ecx+12]
  mov [ebp-4], edx
  mov eax, [ebp+16]
  mov [ebp-8], eax
  mov eax, [ebp+12]
  mov [ebp-12], eax
  mov eax, [ebp-8]
  test eax, 0x1
  jnz near arithmetic_expected_a_number
  mov edx, [ebp-12]
  test edx, 0x1
  jnz near arithmetic_expected_a_number_EDX
  add eax, edx
  jo near overflow
  mov esp, ebp
  pop ebp
  ret
inner_lambda_end_4:
  mov [esi+0], DWORD 2
  mov [esi+4], DWORD inner_lambda_4
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
  jmp near inner_lambda_end_8
inner_lambda_8:
  push DWORD ebp
  mov ebp, esp
  sub esp, 8
  mov ecx, [ebp+8]
  sub ecx, 0x5
  mov edx, [ecx+12]
  mov [ebp-8], edx
  mov edx, [ecx+16]
  mov [ebp-4], edx
  mov eax, [ebp+12]
  mov [ebp-12], eax
  mov eax, DWORD [ebp-12]
  mov ecx, eax
  and ecx, 0x7
  cmp ecx, 0x1
  jne near error_not_tuple
  sub eax, 0x1
  mov edx, 0
  mov ecx, 0x0
  cmp edx, ecx
  jl near index_too_low
  cmp edx, [eax+0]
  jge near index_too_high
  mov eax, [eax+4]
  mov [ebp-16], eax
  mov eax, DWORD [ebp-12]
  mov ecx, eax
  and ecx, 0x7
  cmp ecx, 0x1
  jne near error_not_tuple
  sub eax, 0x1
  mov edx, 1
  mov ecx, 0x0
  cmp edx, ecx
  jl near index_too_low
  cmp edx, [eax+0]
  jge near index_too_high
  mov eax, [eax+8]
  mov [ebp-20], eax
  ;; calling functions
  mov ecx, [ebp-4]
  and ecx, 0x7
  cmp ecx, 0x5
  jne near error_not_closure
  mov eax, [ebp-4]
  sub eax, 0x5
  cmp DWORD [eax+0], 2
  jne near error_wrong_arity
  push DWORD [ebp-20]
  push DWORD [ebp-16]
  mov eax, [ebp-4]
  push DWORD eax
  sub eax, 0x5
  call [eax+4]
  add esp, 12
  mov esp, ebp
  pop ebp
  ret
inner_lambda_end_8:
  mov [esi+0], DWORD 1
  mov [esi+4], DWORD inner_lambda_8
  mov [esi+8], DWORD 1
  mov eax, [ebp-4]
  mov [esi+16], eax
  mov [esi+12], esi
  add [esi+12], DWORD 0x5
  mov eax, esi
  add eax, 0x5
  add esi, 20
  add esi, 0x7
  and esi, 0xfffffff8
  ;; move EAX to EBP
  mov [ebp-8], eax
  jmp near inner_lambda_end_21
inner_lambda_21:
  push DWORD ebp
  mov ebp, esp
  sub esp, 8
  mov ecx, [ebp+8]
  sub ecx, 0x5
  mov edx, [ecx+12]
  mov [ebp-8], edx
  mov edx, [ecx+16]
  mov [ebp-4], edx
  mov [esi+0], DWORD 2
  mov eax, DWORD 2
  mov DWORD [esi+4], eax
  mov eax, DWORD 4
  mov DWORD [esi+8], eax
  mov eax, esi
  add eax, 0x1
  add esi, 12
  add esi, 0x7
  and esi, 0xfffffff8
  mov [ebp-12], eax
  ;; calling functions
  mov ecx, [ebp-4]
  and ecx, 0x7
  cmp ecx, 0x5
  jne near error_not_closure
  mov eax, [ebp-4]
  sub eax, 0x5
  cmp DWORD [eax+0], 1
  jne near error_wrong_arity
  push DWORD [ebp-12]
  mov eax, [ebp-4]
  push DWORD eax
  sub eax, 0x5
  call [eax+4]
  add esp, 8
  mov esp, ebp
  pop ebp
  ret
inner_lambda_end_21:
  mov [esi+0], DWORD 0
  mov [esi+4], DWORD inner_lambda_21
  mov [esi+8], DWORD 1
  mov eax, [ebp-8]
  mov [esi+16], eax
  mov [esi+12], esi
  add [esi+12], DWORD 0x5
  mov eax, esi
  add eax, 0x5
  add esi, 20
  add esi, 0x7
  and esi, 0xfffffff8
  ;; move EAX to EBP
  mov [ebp-12], eax
  ;; compile bindings end 
  ;; calling functions
  mov ecx, [ebp-12]
  and ecx, 0x7
  cmp ecx, 0x5
  jne near error_not_closure
  mov eax, [ebp-12]
  sub eax, 0x5
  cmp DWORD [eax+0], 0
  jne near error_wrong_arity
  mov eax, [ebp-12]
  push DWORD eax
  sub eax, 0x5
  call [eax+4]
  add esp, 4
  ;; postlude start
  mov esp, ebp
  pop ebp
  ret
logic_expected_a_boolean:
  push eax
  push 3
  call error
logic_expected_a_boolean_edx:
  push edx
  push 3
  call error
error_not_boolean_if:
  push eax
  push 4
  call error
arithmetic_expected_a_number:
  push eax
  push 2
  call error
arithmetic_expected_a_number_EDX:
  push edx
  push 2
  call error
comparison_expected_a_number:
  push eax
  push 1
  call error
comparison_expected_a_number_EDX:
  push edx
  push 1
  call error
overflow:
  push eax
  push 5
  call error
error_not_tuple:
  push ecx
  push 6
  call error
index_too_high:
  push edx
  push 8
  call error
index_too_low:
  push eax
  push 7
  call error
error_not_closure:
  push ecx
  push 10
  call error
error_wrong_arity:
  push eax
  push 11
  call error
