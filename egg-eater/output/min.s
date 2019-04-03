 
  section .text
  extern equal 
  extern error
  extern print
  extern input
  global our_code_starts_here
min:
  ;; Stack_setup for min
  push ebp
  mov ebp, esp
  sub esp, 0x8
fun_min_body: ; Body for min
  mov eax, [ebp+8]
  mov [ebp-8], eax
  mov eax, [ebp+12]
  mov [ebp-12], eax
  mov eax, [ebp-8]
  test eax, 0x1
  jnz near comparison_expected_a_number
  mov edx, [ebp-12]
  test edx, 0x1
  jnz near comparison_expected_a_number_EDX
  cmp eax, edx
  mov eax, 0xffffffff
  jl near less_12
  mov eax, 0x7fffffff
less_12:
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
  ;; Prepare for tailcall to function fun_min
  mov eax, [ebp+8]
  push eax ; Save x_3 onto our stack
  mov eax, [ebp+12]
  push eax ; Save y_5 onto our stack
  pop eax
  mov [ebp+8], eax ; Argument y_5 (idx 1)
  pop eax
  mov [ebp+12], eax ; Argument x_3 (idx 2)
  jmp near fun_min_body
if_done_6:
  ;; Clean up for min
  add esp, 0x8
  mov esp, ebp
  pop ebp
  ret ; Return for min
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
  ;; Prepare to call function fun_min
  mov eax, 4
  push eax ; Argument 2
  mov eax, 6
  push eax ; Argument 3
  call min
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
