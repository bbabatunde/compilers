 
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
  mov eax, 0x1
  and eax, 7
  cmp eax, 1
  mov eax, 0x7fffffff
  jne near istuple_labeldone_5
istuple_label_5:
  mov eax, 0xffffffff
istuple_labeldone_5:
  mov [ebp-4], eax
  mov eax, [ebp-4]
  mov [ebp-8], eax
  mov eax, 0xffffffff
  mov [ebp-12], eax
  mov eax, [ebp-8]
  mov edx, [ebp-12]
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
