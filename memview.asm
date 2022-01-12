org 0x100
cpu 8086

CONST_SCREENWIDTH   equ 80
CONST_SCREENHEIGHT  equ 25

CONST_BYTESTHATFIT  equ 80*24

CONST_SCREENRAMSEG  equ 0xB800

CONST_CGAREGSELECT  equ 0x3D4
CONST_CGAREGCONTENT equ 0x3D5

%macro pusha 0
    push ds 
    push es 
    push si 
    push di 
    push ax 
    push bx 
    push cx 
    push dx 
%endmacro

%macro popa 0
    pop dx 
    pop cx 
    pop bx 
    pop ax
    pop di 
    pop si 
    pop es 
    pop ds 
%endmacro 

%macro copybytes 5
    ; Expects (bytes, fromseg, fromoffset, toseg, tooffset)
    pusha 
    mov cx, %1
    mov ds, %2
    mov si, %3
    mov ax, %4
    mov es, ax
    mov di, %5
    cld
    %%_loop:
        movsb
        dec cx 
        inc di 
        cmp cx, 0
        je %%_loopexit 
        jmp %%_loop 
    %%_loopexit:
    popa 
%endmacro

%macro writetoport 2
    ; Expects (port, data) port is a word, data is a byte
    pusha
    mov dx, %1
    mov al, %2
    out dx, al 
    popa 
%endmacro 

%macro writetocga 2
    ; Expects (register, data)
    writetoport CONST_CGAREGSELECT,  %1
    writetoport CONST_CGAREGCONTENT, %2
%endmacro 

entry:
    pusha
    mov ah, 0
    mov al, 3
    int 0x10
    mov ah, 0x01
    mov ch, 10   ; Start line
    mov cl, 15  ; End line
    int 0x10
    mov ax, cs 
    copybytes CONST_STRING_LEN, ax, STRING_ribbon, CONST_SCREENRAMSEG, (CONST_BYTESTHATFIT*2)
main_loop:
    mov al, [DATA_cursoroffset]
    writetocga 0x0F, al ; set low byte cursor pos
    mov al, [DATA_cursoroffset+1]
    writetocga 0x0E, al ; set high byte cursor pos
    mov ax, [DATA_segment]
    mov bx, [DATA_offset]
    copybytes CONST_BYTESTHATFIT, ax, bx, CONST_SCREENRAMSEG, 0 
    call SUB_displayribbon
    ; Check if there's keys in the buffer
    mov ah, 0x01
    int 0x16
    jz main_loop ; No keys in buffer
key_handle:
    mov ax, 0
    int 0x16    ; Get keyboard input

    ; Up
    cmp ah, 72  ; Up arrow
    je _handle_up 
    ; Down
    cmp ah, 80  ; Down arrow
    je _handle_down 
    ; Continue
    cmp ah, 77  ; Numpad right/6
    je _handle_right
    ; Back
    cmp ah, 1   ; Escape
    je _handle_exit 
    cmp ah, 75  ; Numpad left/4
    je _handle_left 

    cmp ah, 49  ; page up
    je _handle_pageup 

    cmp ah, 51  ; page down
    je _handle_pagedown

    cmp al, 'a'
    je _handle_a 

    cmp al, 's'
    je _handle_s

    cmp al, 'd'
    je _handle_d 

    cmp al, 'w'
    je _handle_w 

    jmp main_loop

_handle_exit:
    popa 
    jmp 0

_handle_left:
    mov ax, [DATA_offset]
    sub ax, CONST_SCREENWIDTH 
    mov [DATA_offset], ax 
    jmp main_loop 

_handle_right:
    mov ax, [DATA_offset]
    add ax, CONST_SCREENWIDTH 
    mov [DATA_offset], ax 
    jmp main_loop

_handle_down:
    mov ax, [DATA_segment]
    add ax, 20
    mov [DATA_segment], ax 
    jmp main_loop

_handle_up:
    mov ax, [DATA_segment]
    sub ax, 20
    mov [DATA_segment], ax 
    jmp main_loop

_handle_pageup:
    mov ax, [DATA_segment]
    sub ax, 0x0100
    mov [DATA_segment], ax 
    jmp main_loop
_handle_pagedown:
    mov ax, [DATA_segment]
    add ax, 0x0100
    mov [DATA_segment], ax 
    jmp main_loop

_handle_a:
    mov ax, [DATA_cursoroffset]
    dec ax 
    mov [DATA_cursoroffset], ax 
    jmp main_loop

_handle_s:
    mov ax, [DATA_cursoroffset]
    add ax, CONST_SCREENWIDTH
    mov [DATA_cursoroffset], ax 
    jmp main_loop

_handle_d:
    mov ax, [DATA_cursoroffset]
    inc ax 
    mov [DATA_cursoroffset], ax 
    jmp main_loop

_handle_w:
    mov ax, [DATA_cursoroffset]
    sub ax, CONST_SCREENWIDTH 
    mov [DATA_cursoroffset], ax 
    jmp main_loop

SUB_convertto16hex:
    ; ax contains 16 bit value
    push bx 
	push ax
	mov al, ah
    mov bx, STRING_hex
	call SUB_convertto8hex
	
	pop ax
    add bx, 2
	call SUB_convertto8hex
    pop bx 
	ret

SUB_convertto8hex:
	pusha 
    push ax 
	; al contains the 8 bit number
    ; bx contains the pointer to the bytes to overwrite in memory
	and ax, 0x00F0
	mov cl, 4
	shr ax, cl	    ; shr ax, 4 is only valid on the 80186 and later, so this is a workaround
	add al, '0'	    ; align 0 with '0'
	cmp al, '9'	
	jle _print8bithex_MS
	add al, ('A'-'9'-1) 
                    ; If digit is larger than '9' then add the offset required to make 10 align with 'A'
    _print8bithex_MS:
        mov [bx], al

        pop ax
        and ax, 0x000F
        add al, '0'
        cmp al, '9'
        jle _print8bithex_LS
        add al, ('A'-'9'-1)
    _print8bithex_LS:
        mov [bx+1], al 
        
        popa 
        ret

SUB_displayribbon:
    pusha 
    ; First display screen segment
    mov ax, [DATA_segment]
    call SUB_convertto16hex
    mov ax, ds 
    copybytes 4, ax, STRING_hex, CONST_SCREENRAMSEG, (CONST_BYTESTHATFIT*2)+CONST_STRING_SCREEN_SEG 
    ; screen offset 
    mov ax, [DATA_offset]
    call SUB_convertto16hex
    mov ax, ds 
    copybytes 4, ax, STRING_hex, CONST_SCREENRAMSEG, (CONST_BYTESTHATFIT*2)+CONST_STRING_SCREEN_OFF 

    ; cursor value 
    mov bx, [DATA_offset]
    add bx, [DATA_cursoroffset]

    push ds 
    mov ax, [DATA_segment]
    mov ds, ax 
    mov al, [bx]
    pop ds 
     ; at this point al contains the cursor value 
    mov bx, STRING_hex 
    call SUB_convertto8hex
    mov ax, ds 
    copybytes 2, ax, STRING_hex, CONST_SCREENRAMSEG, (CONST_BYTESTHATFIT*2)+CONST_STRING_SEL_VAL 

    ; cursor offset 
    mov ax, [DATA_offset]
    add ax, [DATA_cursoroffset]
    call SUB_convertto16hex
    mov ax, ds 
    copybytes 4, ax, STRING_hex, CONST_SCREENRAMSEG, (CONST_BYTESTHATFIT*2)+COSNT_STRING_SEL_OFF 

    popa 
    ret 



CONST_STRING_LEN equ 38
CONST_STRING_SCREEN_SEG equ 8*2
CONST_STRING_SCREEN_OFF equ 14*2
COSNT_STRING_SEL_OFF equ 33*2
CONST_STRING_SEL_VAL equ 29*2
STRING_ribbon:
    db "Screen(S    ,O    )Selected(V  ,O    )"

STRING_hex:
    db 0,0,0,0

DATA_segment:
    db word 0x0000
DATA_offset:
    db word 0x0000

DATA_cursoroffset:
    db word 0x0000