section .text
global main
extern malloc
extern printInt
extern printString
extern readInt
extern readString
extern error
extern __CONCAT_STRINGS__

doubleArray:
push ebp
mov ebp, esp
sub esp, 28
mov DWORD eax, [ebp + 8]
mov eax, [eax]
mov [ebp - 4], eax
push DWORD [ebp - 4]
mov eax, [ebp - 4]
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
mov DWORD [ebp - 12], 0
jmp L0
L1:
mov eax, [ebp - 12]
add eax, 1
mov [ebp - 16], eax
mov eax, [ebp - 16]
imul eax, 4
mov [ebp - 16], eax
add eax, [ebp + 8]
mov eax, [eax]
mov [ebp - 16], eax
mov eax, [ebp - 16]
mov [ebp - 16], eax
mov eax, 2
imul eax, [ebp - 16]
mov [ebp - 24], eax
mov eax, [ebp - 8]
add eax, 1
mov [ebp - 20], eax
mov eax, [ebp - 20]
imul eax, 4
mov [ebp - 20], eax
add eax, [ebp - 4]
mov edx, [ebp - 24]
mov [eax], edx
inc DWORD [ebp - 8]
inc DWORD [ebp - 12]
L0:
mov DWORD eax, [ebp + 8]
mov eax, [eax]
mov [ebp - 16], eax
mov eax, [ebp - 12]
cmp eax, [ebp - 16]
setl al
and eax, 0xff
mov [ebp - 16], eax
test eax, eax
jne L1
mov eax, [ebp - 4]
leave
ret
shiftLeft:
push ebp
mov ebp, esp
sub esp, 20
mov eax, 0
add eax, 1
mov [ebp - 4], eax
mov eax, [ebp - 4]
imul eax, 4
mov [ebp - 4], eax
add eax, [ebp + 8]
mov eax, [eax]
mov [ebp - 4], eax
mov eax, [ebp - 4]
mov [ebp - 4], eax
mov DWORD [ebp - 8], 0
jmp L2
L3:
mov eax, [ebp - 8]
add eax, 1
mov [ebp - 16], eax
mov eax, [ebp - 16]
add eax, 1
mov [ebp - 16], eax
mov eax, [ebp - 16]
imul eax, 4
mov [ebp - 16], eax
add eax, [ebp + 8]
mov eax, [eax]
mov [ebp - 16], eax
mov eax, [ebp - 8]
add eax, 1
mov [ebp - 12], eax
mov eax, [ebp - 12]
imul eax, 4
mov [ebp - 12], eax
add eax, [ebp + 8]
mov edx, [ebp - 16]
mov [eax], edx
inc DWORD [ebp - 8]
L2:
mov DWORD eax, [ebp + 8]
mov eax, [eax]
mov [ebp - 12], eax
mov eax, [ebp - 12]
sub eax, 1
mov [ebp - 12], eax
mov eax, [ebp - 8]
cmp eax, [ebp - 12]
setl al
and eax, 0xff
mov [ebp - 12], eax
test eax, eax
jne L3
mov DWORD eax, [ebp + 8]
mov eax, [eax]
mov [ebp - 12], eax
mov eax, [ebp - 12]
sub eax, 1
mov [ebp - 12], eax
mov eax, [ebp - 12]
add eax, 1
mov [ebp - 12], eax
mov eax, [ebp - 12]
imul eax, 4
mov [ebp - 12], eax
add eax, [ebp + 8]
mov edx, [ebp - 4]
mov [eax], edx
leave
ret
scalProd:
push ebp
mov ebp, esp
sub esp, 20
mov DWORD [ebp - 4], 0
mov DWORD [ebp - 8], 0
jmp L4
L5:
mov eax, [ebp - 8]
add eax, 1
mov [ebp - 12], eax
mov eax, [ebp - 12]
imul eax, 4
mov [ebp - 12], eax
add eax, [ebp + 12]
mov eax, [eax]
mov [ebp - 12], eax
mov eax, [ebp - 8]
add eax, 1
mov [ebp - 16], eax
mov eax, [ebp - 16]
imul eax, 4
mov [ebp - 16], eax
add eax, [ebp + 8]
mov eax, [eax]
mov [ebp - 16], eax
mov eax, [ebp - 12]
imul eax, [ebp - 16]
mov [ebp - 12], eax
mov eax, [ebp - 4]
add eax, [ebp - 12]
mov [ebp - 12], eax
mov [ebp - 4], eax
inc DWORD [ebp - 8]
L4:
mov DWORD eax, [ebp + 12]
mov eax, [eax]
mov [ebp - 12], eax
mov eax, [ebp - 8]
cmp eax, [ebp - 12]
setl al
and eax, 0xff
mov [ebp - 12], eax
test eax, eax
jne L5
mov eax, [ebp - 4]
leave
ret
main:
push ebp
mov ebp, esp
sub esp, 28
push DWORD 5
mov eax, 5
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
jmp L6
L7:
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
L6:
mov DWORD eax, [ebp - 4]
mov eax, [eax]
mov [ebp - 12], eax
mov eax, [ebp - 8]
cmp eax, [ebp - 12]
setl al
and eax, 0xff
mov [ebp - 12], eax
test eax, eax
jne L7
push DWORD [ebp - 4]
call shiftLeft
add esp, 4
push DWORD [ebp - 4]
call doubleArray
add esp, 4
mov DWORD [ebp - 12], eax
mov DWORD [ebp - 16], 0
jmp L8
L9:
mov eax, [ebp - 16]
add eax, 1
mov [ebp - 20], eax
mov eax, [ebp - 20]
imul eax, 4
mov [ebp - 20], eax
add eax, [ebp - 4]
mov eax, [eax]
mov [ebp - 20], eax
mov eax, [ebp - 20]
mov [ebp - 20], eax
push DWORD [ebp - 20]
call printInt
add esp, 4
inc DWORD [ebp - 16]
L8:
mov DWORD eax, [ebp - 4]
mov eax, [eax]
mov [ebp - 20], eax
mov eax, [ebp - 16]
cmp eax, [ebp - 20]
setl al
and eax, 0xff
mov [ebp - 20], eax
test eax, eax
jne L9
mov DWORD [ebp - 20], 0
jmp L10
L11:
mov eax, [ebp - 20]
add eax, 1
mov [ebp - 24], eax
mov eax, [ebp - 24]
imul eax, 4
mov [ebp - 24], eax
add eax, [ebp - 12]
mov eax, [eax]
mov [ebp - 24], eax
mov eax, [ebp - 24]
mov [ebp - 24], eax
push DWORD [ebp - 24]
call printInt
add esp, 4
inc DWORD [ebp - 20]
L10:
mov DWORD eax, [ebp - 12]
mov eax, [eax]
mov [ebp - 24], eax
mov eax, [ebp - 20]
cmp eax, [ebp - 24]
setl al
and eax, 0xff
mov [ebp - 24], eax
test eax, eax
jne L11
push DWORD [ebp - 4]
push DWORD [ebp - 12]
call scalProd
add esp, 8
push DWORD eax
call printInt
add esp, 4
mov eax, 0
leave
ret


section .data
