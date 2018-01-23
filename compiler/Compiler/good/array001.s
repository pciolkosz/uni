section .text
global main
extern malloc
extern printInt
extern printString
extern readInt
extern readString
extern error
extern __CONCAT_STRINGS__

main:
push ebp
mov ebp, esp
sub esp, 20
push DWORD 10
mov eax, 10
add eax, 1
mov eax, eax
imul eax, 4
mov eax, eax
push DWORD eax
call malloc
mov edx, [esp+4]
mov [eax], edx
add esp, 8
push DWORD eax
pop DWORD [ebp - 4]
mov DWORD [ebp - 8], 0
jmp L0
L1:
mov eax, [ebp - 8]
add eax, 1
mov [ebp - 12], eax
mov eax, [ebp - 12]
imul eax, 4
mov [ebp - 12], eax
add eax, [ebp - 4]
mov edx, [ebp - 8]
mov [eax], edx
inc DWORD [ebp - 8]
L0:
mov DWORD eax, [ebp - 4]
mov eax, [eax]
mov [ebp - 12], eax
mov eax, [ebp - 8]
cmp eax, [ebp - 12]
setl al
and eax, 0xff
mov [ebp - 12], eax
test eax, eax
jne L1
mov DWORD [ebp - 12], 0
jmp L2
L3:
mov eax, [ebp - 12]
add eax, 1
mov [ebp - 16], eax
mov eax, [ebp - 16]
imul eax, 4
mov [ebp - 16], eax
add eax, [ebp - 4]
mov eax, [eax]
mov [ebp - 16], eax
mov eax, [ebp - 16]
mov [ebp - 16], eax
push DWORD [ebp - 16]
call printInt
add esp, 4
inc DWORD [ebp - 12]
L2:
mov DWORD eax, [ebp - 4]
mov eax, [eax]
mov [ebp - 16], eax
mov eax, [ebp - 12]
cmp eax, [ebp - 16]
setl al
and eax, 0xff
mov [ebp - 16], eax
test eax, eax
jne L3
mov DWORD [ebp - 16], 45
push DWORD [ebp - 16]
call printInt
add esp, 4
mov eax, 0
leave
ret


section .data
