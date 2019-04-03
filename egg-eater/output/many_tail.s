 
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
  sub esp, 0x8
fun_f_body: ; Body for f
  mov eax, [ebp+8]
  mov [ebp-8], eax
  mov eax, 2
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
  jne near if_false_9
if_true_9:
  ;; Prepare to call function fun_print
  mov eax, [ebp+12]
  push eax ; Argument b_5
  call print
  add esp, 0x4
  jmp near if_done_9
if_false_9:
  cmp eax, 0x7fffffff
  jne near error_not_boolean_if
  ;; Prepare for tailcall to function fun_f
  mov eax, [ebp+8]
  push eax ; Save a_3 onto our stack
  mov eax, [ebp+24]
  push eax ; Save e_11 onto our stack
  mov eax, [ebp+20]
  push eax ; Save d_9 onto our stack
  mov eax, [ebp+16]
  push eax ; Save c_7 onto our stack
  mov eax, [ebp+12]
  push eax ; Save b_5 onto our stack
  pop eax
  mov [ebp+8], eax ; Argument b_5 (idx 1)
  pop eax
  mov [ebp+12], eax ; Argument c_7 (idx 2)
  pop eax
  mov [ebp+16], eax ; Argument d_9 (idx 3)
  pop eax
  mov [ebp+20], eax ; Argument e_11 (idx 4)
  pop eax
  mov [ebp+24], eax ; Argument a_3 (idx 5)
  jmp near fun_f_body
if_done_9:
  ;; Clean up for f
  add esp, 0x8
  mov esp, ebp
  pop ebp
  ret ; Return for f
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
  mov eax, 6
  push eax ; Argument 3
  mov eax, 2
  push eax ; Argument 1
  mov eax, 0
  push eax ; Argument 0
  mov eax, 0
  push eax ; Argument 0
  mov eax, 0
  push eax ; Argument 0
  call f
  add esp, 0x14
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
