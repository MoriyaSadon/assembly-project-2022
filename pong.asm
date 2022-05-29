; Moriya Sadon - final work!!!!!!!
IDEAL
MODEL small
STACK 100h
P186 ; !!!
DATASEG
; --------------------------
; Your variables here
; --------------------------
	
	; screens
	start_screen    db 'pong2.bmp',    0
	end_screen      db 'gameOver.bmp', 0

	; ball
	BALL_SIZE       EQU 14
	ball_x		    dw  145
	ball_y		    dw  100
	ball_color 	    db  15 ; white
	delta_x         dw  7  ; left or right
	delta_y         dw  7  ; up or down
	
	; racket
	RACKET_X       	DW  12
	racket_color    db  15 ;white
	racket_y       	dw  150
	racket_y_down  	dw  0  ; bottom border of the racket
	
	; middle Line
	middleLineCount db  10 ; counts for the middle line loop.
	middleLine_y    dw  0
	
	end_game        db  0  ; 0 = keep playing, 1 = game over
	
	points          db  0
	
	blank_msg       db  ' $'
	
CODESEG


start:
	mov ax, @data
	mov ds, ax
; --------------------------
; Your code here
; --------------------------

; prints start screen
	;graphic mode
	mov ax, 13h
	int 10h
	
	mov ax, offset start_screen
	
	call MOR_SCREEN
	
	;waits for press
	mov ah, 0h
	int 16h
	
;----------------------prints everything for the first time---------------------------------
	; set GRAPHIC MODE
	mov ax, 13h
	int 10h

	call drawMiddleLine
	
	call drawWall
	
	call drawRacket
	
	call drawBall
	
;--------------------------------------------------------------------------------------------

game_loop:	
; always happens
repeat:	
; checks changing diractions
	call drawMiddleLine
	call move_ball
	call touch_racket
	call left_border
	
	; checks if game is over. 1 = over, 0 = keep playing
	cmp [end_game], 1
	je game_over
	
	call touch_wall	
	
	; if the player has 9 points the game ends
	cmp [points], 9
	je game_over
	
	call print_score
	call top_border
	call bottom_border
	
	; sleep 
	mov ax,70
	call MOR_SLEEP
		
	; read from keyboard
	CALL MOR_GET_KEY
	
	jz  repeat
	
; up = -5, down = 5
	
; moving according to key press and conditions
	cmp al,'w'
	jne not_w
	
	; checks if its on a border	
	cmp [racket_y], 0
	jbe not_everything
	
; up (racket)
	mov cx, [RACKET_X]	
	mov dx, -5
	call moveRacket
	

not_w:
	cmp al,'s'
	jne not_everything
	
	; checks if its on a border
	cmp [racket_y], 147
	jae not_everything
	
;down (racket)
	mov cx, [RACKET_X]	
	mov dx, 5
	call moveRacket
		
not_everything:
	
	jmp game_loop

game_over:
	; prints end game screen	
	mov ax, offset end_screen
	
	call MOR_SCREEN
	
; print the score
	; prints a blank msg
	mov dh, 5       ; row number
	mov dl, 101   ; column number
	mov cx, offset blank_msg
	call putMessage
	
	;prints the score
	xor ax, ax
	mov al, [points]
	call mor_print_num
	
	;waits for press
	mov ah, 0h
	int 16h	
	
	;back to normal
	mov ax, 2h
	int 10h

exit:
	mov ax, 4c00h
	int 21h
	
;==================================================================
; =====      PROCEDURES 
;==================================================================
;==================================================================
;   drawMiddleLine – draw the middle line
;   IN: [middleLineCount]
;   OUT:  NONE
;	EFFECTED REGISTERS : NONE
;==================================================================
proc drawMiddleLine
; set middle Line	
	mov [middleLineCount], 10

	mov dx,0  ;y
	mov cx, 155 ;x (stays the same)

loop2:
	mov ah, 5    ; width
	mov bl, 10   ; hight
	mov al, 15   ; color (white)
	call drawRect
	
	add dx, 21
	
	dec [middleLineCount]
	jne loop2
	
	ret
endp

;==================================================================
;   drawWall – draw the left wall
;   IN: NONE
;   OUT:  NONE
;	EFFECTED REGISTERS : NONE
;==================================================================
proc drawWall
	PUSHA
	; draw wall (right)
	mov cx, 300     ;(x)
	mov dx, 10      ;(y)
	mov al, 15      ;color
	mov ah, 5       ;width
	mov bl, 175     ;hight
	call drawRect   ;IN: CX=X  , DX =Y , AL = COLOR , AH = WIDTH , BL = HIGHT 
	
	POPA
	ret
endp

;==================================================================
;   drawRacket  – draw the racket (5x55) acording to numbers
;   IN: [racket_y], [RACKET_X], [racket_color]
;   OUT:  NONE
;	EFFECTED REGISTERS : NONE
;==================================================================
proc drawRacket
	PUSHA
	; draw racket 2 (left one)
	mov cx, [RACKET_X]    ;(x)
	mov dx, [racket_y]    ;(y)
	mov al,	[racket_color] ;color
	mov ah, 5              ;width
	mov bl, 50             ;hight
	call drawRect          ;IN: CX=X  , DX =Y , AL = COLOR , AH = WIDTH , BL = HIGHT  
	
	POPA
	ret
endp

;==================================================================
;   drawBall  – draw the ball (14x14) acording to  ball_x , ball_y
;   IN: [ball_x], [ball_y], BALL_SIZE
;   OUT:  NONE
;	EFFECTED REGISTERS : NONE
;==================================================================
proc drawBall 
	PUSHA 
	mov cx,[ball_x]
	mov dx,[ball_y]
	mov ah,BALL_SIZE
	mov bl,BALL_SIZE
    call drawRect  ; IN: CX=X  , DX =Y , AL = COLOR , AH = WIDTH ,  BL = HIGHT 
	POPA
	ret 
endp

;==================================================================
;   moveRacket – erase the racket and repaint it in its new location
;   IN:  DX = 5 / -5 (up or down), [racket_y], [racket_color]
;   OUT:  update the racket to the new location
;	EFFECTED REGISTERS : NONE
;==================================================================
proc moveRacket
	PUSHA 
	PUSHA ; !!!!   TWICE

	; erase current racket 
	mov [racket_color], 0 ; color = black
    call drawRacket  

; update racket_y and draw racket in new place
	POPA
	add [racket_y],dx
	mov [racket_color], 15 ;color = white
    call drawRacket

	POPA
	ret 
endp

;==================================================================
;   touch_racket – checks if the ball touched the racket and changes
;   the direction in case it did
;   IN:  [ball_x], [ball_y], [delta_x]
;   OUT:  none
;	EFFECTED REGISTERS : NONE
;==================================================================
proc touch_racket
	pusha
	; checks x placement
	cmp [ball_x], 19
	ja sof_touch_racket
	
top_ball_range:	
	; checks top range
	xor ax, ax
	mov ax, [racket_y]
	cmp [ball_y], ax
	jb bottom_ball_range
	
	; checks bottom range
	add ax, 50	
	cmp [ball_y], ax
	ja bottom_ball_range
	
	; in the range
	mov [delta_x], 7

bottom_ball_range:
	; checks top range
	xor ax, ax
	mov ax, [ball_y]
	add ax, 14
	cmp ax, [racket_y]
	jb sof_touch_racket
	
	; checks bottom range
	xor bx, bx
	mov bx, [racket_y]
	add bx, 50
	cmp ax, bx
	ja sof_touch_racket
	
	; in the range
	mov [delta_x], 7
	
sof_touch_racket:
	popa
	ret 
endp

;==================================================================
;   left_border – checks if the ball touched the racket and changes
;   the direction in case it did
;   IN:  [ball_x], [end_game]
;   OUT:  none
;	EFFECTED REGISTERS : NONE
;==================================================================
proc left_border
	pusha
	cmp [ball_x], 5
	ja sof_left_border
	; touching the left border
	mov [end_game], 1	
sof_left_border:
	popa
	ret
endp

;==================================================================
;   touch_wall – checks if the ball touched the wall and changes
;   the direction in case it did
;   IN:  [ball_x], [delta_x], [points]
;   OUT:  none
;	EFFECTED REGISTERS : NONE
;==================================================================
proc touch_wall
	pusha
	cmp [ball_x], 278
	jne sof_touch_wall
	; touching the wall
	mov [delta_x], -7
	inc [points]
sof_touch_wall:
	popa
	ret
endp

;==================================================================
;   top_border – checks if the ball touched the top border and changes
;   the direction in case it did
;   IN:  [ball_y], [delta_y]
;   OUT:  none
;	EFFECTED REGISTERS : NONE
;==================================================================
proc top_border
	pusha
	cmp [ball_y], 2
	jne sof_top_border
	; touching the top border
	mov [delta_y], 7
sof_top_border:
	popa
	ret
endp

;==================================================================
;   bottom_border – checks if the ball touched the bottom border and
;   changes the direction in case it did
;   IN:  [ball_y], [delta_y]
;   OUT:  none
;	EFFECTED REGISTERS : NONE
;==================================================================
proc bottom_border
	pusha
	cmp [ball_y], 191
	jne sof_bottom_border
	; touching the bottom border
	mov [delta_y], -7
sof_bottom_border:
	popa
	ret
endp

;==================================================================
;   move_ball – moves the ball
;   IN:  [ball_x], [ball_y], [delta_x], [delta_y], [ball_color]
;   OUT:  the ball in the new place
;	EFFECTED REGISTERS : NONE
;==================================================================
proc move_ball
	pusha
	; erase the ball
	mov al, 0  
	mov cx, [ball_x]
	mov dx, [ball_y]
	call drawBall
	
	; draws ball in new location
	add cx, [delta_x]
	add dx, [delta_y]
	mov [ball_x], cx
	mov [ball_y], dx
	mov al, [ball_color]
	call drawBall
	
	popa
	ret
endp

;==================================================================
;   print_score – prints the current score
;   IN:  [blank_msg], [points]
;   OUT:  the printed score
;	EFFECTED REGISTERS : NONE
;==================================================================
proc print_score
	pusha
; print the score
	; prints a blank msg
	mov dh, 1       ; row number
	mov dl, 56  ; column number
	mov cx, offset blank_msg
	call putMessage
	
	; prints the score
	xor ax, ax
	mov al, [points]
	call mor_print_num
	popa 
	ret
endp


INCLUDE "MOR_LIBG.ASM"
END start
