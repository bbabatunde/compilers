 
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
  sub esp, 16
  ;; body start
  mov [esi+0], DWORD 2
  mov eax, DWORD 2
  mov DWORD [esi+4], eax
  mov eax, DWORD 12
  mov DWORD [esi+8], eax
  mov eax, esi
  add eax, 1
  add esi, 12
  add esi, 7
  and esi, 0xfffffff8
  mov [ebp-4], eax
  mov [esi+0], DWORD 3
  mov eax, DWORD 8
  mov DWORD [esi+4], eax
  mov eax, DWORD 10
  mov DWORD [esi+8], eax
  mov eax, DWORD [ebp-4]
  mov DWORD [esi+12], eax
  mov eax, esi
  add eax, 1
  add esi, 16
  add esi, 7
  and esi, 0xfffffff8
  mov [ebp-8], eax
  mov [esi+0], DWORD 2
  mov eax, DWORD 2
  mov DWORD [esi+4], eax
  mov eax, DWORD 12
  mov DWORD [esi+8], eax
  mov eax, esi
  add eax, 1
  add esi, 12
  add esi, 7
  and esi, 0xfffffff8
  mov [ebp-12], eax
  mov [esi+0], DWORD 3
  mov eax, DWORD 8
  mov DWORD [esi+4], eax
  mov eax, DWORD 10
  mov DWORD [esi+8], eax
  mov eax, DWORD [ebp-12]
  mov DWORD [esi+12], eax
  mov eax, esi
  add eax, 1
  add esi, 16
  add esi, 7
  and esi, 0xfffffff8
  mov [ebp-16], eax
  mov eax, [ebp-8]
  mov [ebp-20], eax
  mov eax, [ebp-16]
  mov [ebp-24], eax
  mov eax, [ebp-20]
  mov edx, [ebp-24]
  push eax
  push edx
  call equal
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
