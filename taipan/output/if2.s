section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 12
  mov eax, 0
  mov [ebp-8], eax
  mov edx, 2
  mov [ebp-12], edx
  mov eax, [ebp-8]
  test eax, 0x1
  jnz comparison_expected_a_number
  mov edx, [ebp-12]
  test edx, 0x1
  jnz comparison_expected_a_number_EDX
  cmp eax, edx
  mov eax, 0xffffffff
  jg greater_6
  mov eax, 0x7fffffff
greater_6:
  mov [ebp-4], eax
  mov eax, [ebp-4]
  cmp eax, 0xffffffff
  jne if_false_2
if_true_2:
  mov eax, 8
  jmp if_done_2
if_false_2:
  cmp eax, 0x7fffffff
  jne error_not_boolean_if
  mov eax, 4
if_done_2:
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
