 
  section .text
  extern equal 
  extern error
  extern print
  extern input
  global our_code_starts_here
link:
  ;; Stack_setup for link
  push ebp
  mov ebp, esp
  sub esp, 0x4
fun_link_body: ; Body for link
  mov [esi+0], DWORD 2
  mov eax, DWORD [ebp+8]
  mov DWORD [esi+4], eax
  mov eax, DWORD [ebp+12]
  mov DWORD [esi+8], eax
  mov eax, esi
  add eax, 1
  add esi, 12
  add esi, 7
  and esi, 0xfffffff8
  ;; Clean up for link
  add esp, 0x4
  mov esp, ebp
  pop ebp
  ret ; Return for link
length:
  ;; Stack_setup for length
  push ebp
  mov ebp, esp
  sub esp, 0x10
fun_length_body: ; Body for length
  mov eax, [ebp+8]
  mov [ebp-8], eax
  mov eax, 0x7fffffff
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
  jne near if_false_24
if_true_24:
  mov eax, 0
  jmp near if_done_24
if_false_24:
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
  push eax ; Argument eget_32
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
if_done_24:
  ;; Clean up for length
  add esp, 0x10
  mov esp, ebp
  pop ebp
  ret ; Return for length
sum:
  ;; Stack_setup for sum
  push ebp
  mov ebp, esp
  sub esp, 0x14
fun_sum_body: ; Body for sum
  mov eax, [ebp+8]
  mov [ebp-8], eax
  mov eax, 0x7fffffff
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
  jne near if_false_41
if_true_41:
  mov eax, 0
  jmp near if_done_41
if_false_41:
  cmp eax, 0x7fffffff
  jne near error_not_boolean_if
  mov eax, [ebp+8]
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
  mov [ebp-20], eax
  ;; Prepare to call function fun_sum
  mov eax, [ebp-20]
  push eax ; Argument eget_51
  call sum
  add esp, 0x4
  mov [ebp-24], eax
  mov eax, [ebp-16]
  mov [ebp-28], eax
  mov eax, [ebp-24]
  mov [ebp-32], eax
  mov eax, [ebp-28]
  test eax, 0x1
  jnz near arithmetic_expected_a_number
  mov edx, [ebp-32]
  test edx, 0x1
  jnz near arithmetic_expected_a_number_EDX
  add eax, edx
  jo near overflow
if_done_41:
  ;; Clean up for sum
  add esp, 0x14
  mov esp, ebp
  pop ebp
  ret ; Return for sum
reverseHelper:
  ;; Stack_setup for reverseHelper
  push ebp
  mov ebp, esp
  sub esp, 0x1c
fun_reverseHelper_body: ; Body for reverseHelper
  mov eax, [ebp+12]
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
  mov [ebp-4], eax
  mov eax, [ebp+8]
  mov edx, [ebp-4]
  mov ecx, eax
  and ecx, 0x7
  cmp ecx, 0x1
  jne near error_not_tuple
  sub eax, 0x1
  mov [eax+4], edx
  add eax, 0x1
  mov [ebp-8], eax
  mov eax, [ebp+12]
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
  mov [ebp-12], eax
  mov eax, [ebp-12]
  mov [ebp-20], eax
  mov eax, 0x7fffffff
  mov [ebp-24], eax
  mov eax, [ebp-20]
  mov edx, [ebp-24]
  push eax
  push edx
  call equal
  add esp, 8
  mov [ebp-16], eax
  mov eax, [ebp-16]
  cmp eax, 0xffffffff
  jne near if_false_64
if_true_64:
  mov eax, [ebp+8]
  jmp near if_done_64
if_false_64:
  cmp eax, 0x7fffffff
  jne near error_not_boolean_if
  ;; Prepare to call function fun_link
  mov eax, [ebp+8]
  push eax ; Argument built_54
  mov eax, 0x7fffffff
  push eax ; Argument false
  call link
  add esp, 0x8
  mov [ebp-28], eax
  mov eax, [ebp+12]
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
  mov [ebp-32], eax
  ;; Prepare for tailcall to function fun_reverseHelper
  mov eax, [ebp-32]
  push eax ; Save eget_87 onto our stack
  mov eax, [ebp-28]
  push eax ; Save tmp_80 onto our stack
  pop eax
  mov [ebp+8], eax ; Argument tmp_80 (idx 1)
  pop eax
  mov [ebp+12], eax ; Argument eget_87 (idx 2)
  jmp near fun_reverseHelper_body
if_done_64:
  ;; Clean up for reverseHelper
  add esp, 0x1c
  mov esp, ebp
  pop ebp
  ret ; Return for reverseHelper
reverse:
  ;; Stack_setup for reverse
  push ebp
  mov ebp, esp
  sub esp, 0x8
fun_reverse_body: ; Body for reverse
  ;; Prepare to call function fun_link
  mov eax, 0x7fffffff
  push eax ; Argument false
  mov eax, 0x7fffffff
  push eax ; Argument false
  call link
  add esp, 0x8
  mov [ebp-4], eax
  ;; Prepare to call function fun_reverseHelper
  mov eax, [ebp+8]
  push eax ; Argument l_90
  mov eax, [ebp-4]
  push eax ; Argument app_98
  call reverseHelper
  add esp, 0x8
  ;; Clean up for reverse
  add esp, 0x8
  mov esp, ebp
  pop ebp
  ret ; Return for reverse
append:
  ;; Stack_setup for append
  push ebp
  mov ebp, esp
  sub esp, 0x18
fun_append_body: ; Body for append
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
  mov [ebp-4], eax
  mov eax, [ebp-4]
  mov [ebp-12], eax
  mov eax, 0x7fffffff
  mov [ebp-16], eax
  mov eax, [ebp-12]
  mov edx, [ebp-16]
  push eax
  push edx
  call equal
  add esp, 8
  mov [ebp-8], eax
  mov eax, [ebp-8]
  cmp eax, 0xffffffff
  jne near if_false_98
if_true_98:
  mov eax, [ebp+8]
  mov edx, [ebp+12]
  mov ecx, eax
  and ecx, 0x7
  cmp ecx, 0x1
  jne near error_not_tuple
  sub eax, 0x1
  mov [eax+8], edx
  add eax, 0x1
  mov [ebp-16], eax
  mov eax, [ebp+8]
  jmp near if_done_98
if_false_98:
  cmp eax, 0x7fffffff
  jne near error_not_boolean_if
  mov eax, [ebp+8]
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
  mov [ebp-20], eax
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
  mov [ebp-24], eax
  ;; Prepare to call function fun_append
  mov eax, [ebp+12]
  push eax ; Argument l2_105
  mov eax, [ebp-24]
  push eax ; Argument eget_130
  call append
  add esp, 0x8
  mov [ebp-28], eax
  ;; Prepare for tailcall to function fun_link
  mov eax, [ebp-28]
  push eax ; Save app_129 onto our stack
  mov eax, [ebp-20]
  push eax ; Save eget_127 onto our stack
  pop eax
  mov [ebp+8], eax ; Argument eget_127 (idx 1)
  pop eax
  mov [ebp+12], eax ; Argument app_129 (idx 2)
  jmp near fun_link_body
if_done_98:
  ;; Clean up for append
  add esp, 0x18
  mov esp, ebp
  pop ebp
  ret ; Return for append
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
  ;; Prepare to call function fun_link
  mov eax, 0x7fffffff
  push eax ; Argument false
  mov eax, 6
  push eax ; Argument 3
  call link
  add esp, 0x8
  mov [ebp-4], eax
  ;; Prepare to call function fun_link
  mov eax, [ebp-4]
  push eax ; Argument app_141
  mov eax, 4
  push eax ; Argument 2
  call link
  add esp, 0x8
  mov [ebp-8], eax
  ;; Prepare to call function fun_link
  mov eax, [ebp-8]
  push eax ; Argument app_139
  mov eax, 2
  push eax ; Argument 1
  call link
  add esp, 0x8
  mov [ebp-12], eax
  ;; Prepare to call function fun_length
  mov eax, [ebp-12]
  push eax ; Argument mylist_135
  call length
  add esp, 0x4
  mov [ebp-16], eax
  ;; Prepare to call function fun_sum
  mov eax, [ebp-12]
  push eax ; Argument mylist_135
  call sum
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
