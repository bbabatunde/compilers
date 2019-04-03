 
  section .text
  extern equal 
  extern error
  extern print
  extern input
  global our_code_starts_here
length:
  ;; Stack_setup for length
  push ebp
  mov ebp, esp
  sub esp, 0x10
fun_length_body: ; Body for length
  mov eax, [ebp+8]
  mov [ebp-8], eax
  mov eax, 0x1
  mov [ebp-12], eax
  mov eax, [ebp-8]
  mov edx, [ebp-12]
  push eax
  push edx
  call equal
  add esp, 8
  mov [ebp-4], eax
  mov eax, [ebp-4]
  cmp eax, 0xffffffff
  jne near if_false_17
if_true_17:
  mov eax, 0
  jmp near if_done_17
if_false_17:
  cmp eax, 0x7fffffff
  jne near error_not_boolean_if
  mov eax, [ebp+8]
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
  mov [ebp-16], eax
  ;; Prepare to call function fun_length
  mov eax, [ebp-16]
  push eax ; Argument eget_22
  call length
  add esp, 0x4
  mov [ebp-20], eax
  mov eax, 2
  mov [ebp-24], eax
  mov eax, [ebp-20]
  mov [ebp-28], eax
  mov eax, [ebp-24]
  test eax, 0x1
  jnz near arithmetic_expected_a_number
  mov edx, [ebp-28]
  test edx, 0x1
  jnz near arithmetic_expected_a_number_EDX
  add eax, edx
  jo near overflow
if_done_17:
  ;; Clean up for length
  add esp, 0x10
  mov esp, ebp
  pop ebp
  ret ; Return for length
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
  mov [esi+0], DWORD 2
  mov eax, DWORD 6
  mov DWORD [esi+4], eax
  mov eax, DWORD 0x1
  mov DWORD [esi+8], eax
  mov eax, esi
  add eax, 1
  add esi, 12
  add esi, 7
  and esi, 0xfffffff8
  mov [ebp-4], eax
  mov [esi+0], DWORD 2
  mov eax, DWORD 4
  mov DWORD [esi+4], eax
  mov eax, DWORD [ebp-4]
  mov DWORD [esi+8], eax
  mov eax, esi
  add eax, 1
  add esi, 12
  add esi, 7
  and esi, 0xfffffff8
  mov [ebp-8], eax
  mov [esi+0], DWORD 2
  mov eax, DWORD 2
  mov DWORD [esi+4], eax
  mov eax, DWORD [ebp-8]
  mov DWORD [esi+8], eax
  mov eax, esi
  add eax, 1
  add esi, 12
  add esi, 7
  and esi, 0xfffffff8
  mov [ebp-12], eax
  ;; Prepare to call function fun_length
  mov eax, [ebp-12]
  push eax ; Argument mylistlength_26
  call length
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
