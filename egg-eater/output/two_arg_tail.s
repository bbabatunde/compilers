 
  section .text
  extern equal 
  extern error
  extern print
  extern input
  global our_code_starts_here
f:
  ;; Stack_setup for f
  push ebp
  mov ebp, esp
  sub esp, 0xc
fun_f_body: ; Body for f
  mov eax, [ebp+12]
  mov [ebp-8], eax
  mov eax, 0
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
  jne near if_false_6
if_true_6:
  mov eax, [ebp+8]
  jmp near if_done_6
if_false_6:
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
  ;; Prepare for tailcall to function fun_g
  mov eax, [ebp-16]
  push eax ; Save binop_20 onto our stack
  mov eax, [ebp+12]
  push eax ; Save y_5 onto our stack
  pop eax
  mov [ebp+8], eax ; Argument y_5 (idx 1)
  pop eax
  mov [ebp+12], eax ; Argument binop_20 (idx 2)
  jmp near fun_g_body
if_done_6:
  ;; Clean up for f
  add esp, 0xc
  mov esp, ebp
  pop ebp
  ret ; Return for f
g:
  ;; Stack_setup for g
  push ebp
  mov ebp, esp
  sub esp, 0xc
fun_g_body: ; Body for g
  mov eax, [ebp+12]
  mov [ebp-8], eax
  mov eax, 0
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
  jne near if_false_21
if_true_21:
  mov eax, [ebp+8]
  jmp near if_done_21
if_false_21:
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
  ;; Prepare for tailcall to function fun_f
  mov eax, [ebp-16]
  push eax ; Save binop_41 onto our stack
  mov eax, [ebp+12]
  push eax ; Save y_26 onto our stack
  pop eax
  mov [ebp+8], eax ; Argument y_26 (idx 1)
  pop eax
  mov [ebp+12], eax ; Argument binop_41 (idx 2)
  jmp near fun_f_body
if_done_21:
  ;; Clean up for g
  add esp, 0xc
  mov esp, ebp
  pop ebp
  ret ; Return for g
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
  ;; Prepare to call function fun_f
  mov eax, 2
  push eax ; Argument 1
  mov eax, 2
  push eax ; Argument 1
  call f
  add esp, 0x8
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
