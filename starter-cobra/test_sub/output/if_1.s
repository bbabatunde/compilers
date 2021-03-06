section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 4
  mov eax, 108
  cmp eax, 0xffffffff
  jne if_false_3
if_true_3:
  mov eax, 0xffffffff
  jmp if_done_3
if_false_3:
  cmp eax, 0x7fffffff
  jne error_not_boolean_if
  mov eax, 0x7fffffff
if_done_3:
  mov [ebp-4], eax
  mov eax, [ebp-4]
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
