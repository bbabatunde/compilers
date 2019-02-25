section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 16
  mov eax, 110
  test eax, 0x1
  jnz arithmetic_expected_a_number
  sub eax, 2
  jo overflow
  mov [ebp-4], eax
  mov eax, [ebp-4]
  mov [ebp-12], eax
  mov edx, 108
  mov [ebp-16], edx
  mov eax, [ebp-12]
  test eax, 0x1
  jnz comparison_expected_a_number
  mov edx, [ebp-16]
  test edx, 0x1
  jnz comparison_expected_a_number_EDX
  cmp eax, edx
  mov eax, 0xffffffff
  jl less_27
  mov eax, 0x7fffffff
less_27:
  mov [ebp-8], eax
  mov eax, [ebp-8]
  cmp eax, 0xffffffff
  jne if_false_5
if_true_5:
  mov eax, 2
  mov [ebp-24], eax
  mov edx, 0
  mov [ebp-28], edx
  mov eax, [ebp-24]
  test eax, 0x1
  jnz comparison_expected_a_number
  mov edx, [ebp-28]
  test edx, 0x1
  jnz comparison_expected_a_number_EDX
  cmp eax, edx
  mov eax, 0xffffffff
  jg greater_23
  mov eax, 0x7fffffff
greater_23:
  mov [ebp-20], eax
  mov eax, [ebp-20]
  cmp eax, 0xffffffff
  jne if_false_17
if_true_17:
  mov eax, 4
  test eax, 0x1
  jnz arithmetic_expected_a_number
  add eax, 2
  jo overflow
  jmp if_done_17
if_false_17:
  cmp eax, 0x7fffffff
  jne error_not_boolean_if
  mov eax, 6
  test eax, 0x1
  jnz arithmetic_expected_a_number
  add eax, 2
  jo overflow
if_done_17:
  jmp if_done_5
if_false_5:
  cmp eax, 0x7fffffff
  jne error_not_boolean_if
  mov eax, 0
  mov [ebp-28], eax
  mov edx, 0
  mov [ebp-32], edx
  mov eax, [ebp-28]
  mov edx, [ebp-32]
  cmp eax, edx
  mov eax, 0xffffffff
  je equal_13
  mov eax, 0x7fffffff
equal_13:
  mov [ebp-24], eax
  mov eax, [ebp-24]
  cmp eax, 0xffffffff
  jne if_false_7
if_true_7:
  mov eax, 8
  test eax, 0x1
  jnz arithmetic_expected_a_number
  sub eax, 2
  jo overflow
  jmp if_done_7
if_false_7:
  cmp eax, 0x7fffffff
  jne error_not_boolean_if
  mov eax, 10
  test eax, 0x1
  jnz arithmetic_expected_a_number
  sub eax, 2
  jo overflow
if_done_7:
if_done_5:
  mov [ebp-12], eax
  mov eax, [ebp-12]
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
