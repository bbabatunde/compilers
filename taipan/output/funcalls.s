section .text
extern error
extern print
global our_code_starts_here
fact:
  push ebp
  mov ebp, esp
  sub esp, 4
  mov eax, [ebp-4]
  mov [ebp-8], eax
  mov edx, 4
  mov [ebp-12], edx
  mov eax, [ebp-8]
  test eax, 0x1
  jnz comparison_expected_a_number
  mov edx, [ebp-12]
  test edx, 0x1
  jnz comparison_expected_a_number_EDX
  cmp eax, edx
  mov eax, 0xffffffff
  jl less_18
  mov eax, 0x7fffffff
less_18:
  mov [ebp-4], eax
  mov eax, [ebp-4]
  cmp eax, 0xffffffff
  jne if_false_5
if_true_5:
  mov eax, 2
  jmp if_done_5
if_false_5:
  cmp eax, 0x7fffffff
  jne error_not_boolean_if
  mov eax, [ebp-4]
  mov [ebp-20], eax
  mov edx, 2
  mov [ebp-24], edx
  mov eax, [ebp-20]
  test eax, 0x1
  jnz arithmetic_expected_a_number
  mov edx, [ebp-24]
  test edx, 0x1
  jnz arithmetic_expected_a_number_EDX
  sub eax, edx
  jo overflow
  mov [ebp-16], eax
  mov eax, [ebp-16]
  push eax
  sub esp, 12
  call fact
  add esp, 12
  mov [ebp-20], eax
  mov eax, [ebp-4]
  mov [ebp-24], eax
  mov edx, [ebp-20]
  mov [ebp-28], edx
  mov eax, [ebp-24]
  test eax, 0x1
  jnz arithmetic_expected_a_number
  mov edx, [ebp-28]
  test edx, 0x1
  jnz arithmetic_expected_a_number_EDX
  imul eax, edx
  jo overflow
  sar eax, 1
  jo overflow
if_done_5:
  mov esp, ebp
  pop ebp
  ret
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 0
  mov eax, 10
  push eax
  sub esp, 12
  call fact
  add esp, 12
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
