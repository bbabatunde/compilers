section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 24
  mov eax, 6
  mov [ebp-4], eax
  mov eax, 18
  mov [ebp-8], eax
  mov eax, 0xffffffff
  mov [ebp-12], eax
  mov eax, [ebp-12]
  test eax, 0x1
  jz isBOOL_false_12
  mov eax, 0xffffffff
  jmp isBool_done_12
isBOOL_false_12:
  mov eax, 0x7fffffff
isBool_done_12:
  mov [ebp-16], eax
  mov eax, [ebp-16]
  cmp eax, 0xffffffff
  jne if_false_16
if_true_16:
  mov eax, [ebp-4]
  mov [ebp-32], eax
  mov eax, [ebp-8]
  mov [ebp-36], eax
  mov eax, [ebp-32]
  test eax, 0x1
  jnz arithmetic_expected_a_number
  mov edx, [ebp-36]
  test edx, 0x1
  jnz arithmetic_expected_a_number_EDX
  imul eax, edx
  jo overflow
  sar eax, 1
  jo overflow
  mov [ebp-28], eax
  mov eax, [ebp-28]
  jmp if_done_16
if_false_16:
  cmp eax, 0x7fffffff
  jne error_not_boolean_if
  mov eax, [ebp-4]
  mov [ebp-36], eax
  mov eax, [ebp-8]
  mov [ebp-40], eax
  mov eax, [ebp-36]
  test eax, 0x1
  jnz arithmetic_expected_a_number
  mov edx, [ebp-40]
  test edx, 0x1
  jnz arithmetic_expected_a_number_EDX
  add eax, edx
  jo overflow
  mov [ebp-32], eax
  mov eax, [ebp-32]
if_done_16:
  mov [ebp-20], eax
  mov eax, [ebp-20]
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
