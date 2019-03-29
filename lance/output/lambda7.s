 
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
  jmp near inner_lambda_end_7
inner_lambda_7:
  push DWORD ebp
  mov ebp, esp
  sub esp, 8
  mov ecx, [ebp+8]
  sub ecx, 0x5
  mov edx, [ecx+12]
  mov [ebp-8], edx
  mov edx, [ecx+16]
  mov [ebp-4], edx
  ;; calling functions
  mov ecx, [ebp-4]
  and ecx, 0x7
  cmp ecx, 0x5
  jne near error_not_closure
  mov eax, [ebp-4]
  sub eax, 0x5
  cmp DWORD [eax+0], 1
  jne near error_wrong_arity
  push DWORD [ebp+12]
  mov eax, [ebp-4]
  push DWORD eax
  sub eax, 0x5
  call [eax+4]
  add esp, 8
  mov esp, ebp
  pop ebp
  ret
inner_lambda_end_7:
  mov [esi+0], DWORD 1
  mov [esi+4], DWORD inner_lambda_7
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
  ;; compile bindings end 
  ;; calling functions
  mov ecx, [ebp-8]
  and ecx, 0x7
  cmp ecx, 0x5
  jne near error_not_closure
  mov eax, [ebp-8]
  sub eax, 0x5
  cmp DWORD [eax+0], 1
  jne near error_wrong_arity
  push DWORD 2
  mov eax, [ebp-8]
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
