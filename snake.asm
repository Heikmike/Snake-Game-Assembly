;	set game state memory location
.equ    HEAD_X,         0x1000  ; Snake head's position on x
.equ    HEAD_Y,         0x1004  ; Snake head's position on y
.equ    TAIL_X,         0x1008  ; Snake tail's position on x
.equ    TAIL_Y,         0x100C  ; Snake tail's position on Y
.equ    SCORE,          0x1010  ; Score address
.equ    GSA,            0x1014  ; Game state array address

.equ    CP_VALID,       0x1200  ; Whether the checkpoint is valid.
.equ    CP_HEAD_X,      0x1204  ; Snake head's X coordinate. (Checkpoint)
.equ    CP_HEAD_Y,      0x1208  ; Snake head's Y coordinate. (Checkpoint)
.equ    CP_TAIL_X,      0x120C  ; Snake tail's X coordinate. (Checkpoint)
.equ    CP_TAIL_Y,      0x1210  ; Snake tail's Y coordinate. (Checkpoint)
.equ    CP_SCORE,       0x1214  ; Score. (Checkpoint)
.equ    CP_GSA,         0x1218  ; GSA. (Checkpoint)

.equ    LEDS,           0x2000  ; LED address
.equ    SEVEN_SEGS,     0x1198  ; 7-segment display addresses
.equ    RANDOM_NUM,     0x2010  ; Random number generator address
.equ    BUTTONS,        0x2030  ; Buttons addresses

; button state
.equ    BUTTON_NONE,    0
.equ    BUTTON_LEFT,    1
.equ    BUTTON_UP,      2
.equ    BUTTON_DOWN,    3
.equ    BUTTON_RIGHT,   4
.equ    BUTTON_CHECKPOINT,    5

; array state
.equ    DIR_LEFT,       1       ; leftward direction
.equ    DIR_UP,         2       ; upward direction
.equ    DIR_DOWN,       3       ; downward direction
.equ    DIR_RIGHT,      4       ; rightward direction
.equ    FOOD,           5       ; food

; constants
.equ    NB_ROWS,        8       ; number of rows
.equ    NB_COLS,        12      ; number of columns
.equ    NB_CELLS,       96      ; number of cells in GSA
.equ    RET_ATE_FOOD,   1       ; return value for hit_test when food was eaten
.equ    RET_COLLISION,  2       ; return value for hit_test when a collision was detected
.equ    ARG_HUNGRY,     0       ; a0 argument for move_snake when food wasn't eaten
.equ    ARG_FED,        1       ; a0 argument for move_snake when food was eaten

; initialize stack pointer
addi    sp, zero, LEDS

; main
; arguments
;     none
;
; return values
;     This procedure should never return.
main:
	addi sp, sp, -4
    stw ra, 0 (sp)
	call init_game
	addi sp, sp, 4
    ldw ra, 0 (sp)
	main_loop:
		addi sp, sp, -4
		stw ra, 0 (sp)
		call clear_leds
		addi sp, sp, 4
    	ldw ra, 0 (sp)
		call get_input 
		addi sp, sp, -4
		stw ra, 0 (sp)
		call hit_test
		addi sp, sp, 4
    	ldw ra, 0 (sp)
		add t1, zero, v0
		add t3, zero, zero
		beq t1, t3, continue_move
		addi t3, zero, 2
		beq t1, t3, end_game
		addi sp, sp, -4
		stw ra, 0 (sp)
		call create_food
		addi sp, sp, 4
    	ldw ra, 0 (sp)
		addi sp, sp, -4
		stw ra, 0 (sp)
		call blink_score
		addi sp, sp, 4
    	ldw ra, 0 (sp)
		continue_move:
		addi sp, sp, -4
		stw ra, 0 (sp)
		call move_snake
		addi sp, sp, 4
    	ldw ra, 0 (sp)
		addi sp, sp, -4
		stw ra, 0 (sp)
		call draw_array
		addi sp, sp, 4
    	ldw ra, 0 (sp)
		jmpi main_loop
	end_game:
		ret

; BEGIN: clear_leds
clear_leds:
 	stw zero, LEDS (zero)
    stw zero, LEDS + 4 (zero)
    stw zero, LEDS + 8 (zero)
    ret
; END: clear_leds


; BEGIN: set_pixel
set_pixel:
    slli t0, a0, 3
    add t0, t0, a1
	set_cmp:
	addi t6, zero, 32
	blt t0, t6, set_op
    sub t0, t0, t6
	jmpi set_cmp
	set_op:
    addi t1, zero, 1 ;
    sll t1, t1, t0

    add t2, zero, zero
    addi t4, zero, 4
    blt a0, t4, enable_pixel 

    addi t2, t2, 4
    addi t4, t4, 4
    blt a0, t4, enable_pixel

    addi t2, t2, 4
    jmpi enable_pixel

    enable_pixel:
        ldw t3, LEDS (t2) 
        or t3, t3, t1
        stw t3, LEDS (t2)

        ret
; END:set_pixel


; BEGIN: display_score
display_score:
	ldw t0, SCORE (zero) ; t0 = score

	ldw t7, digit_map (zero)
	stw t7, SEVEN_SEGS (zero)
	stw t7, SEVEN_SEGS + 4 (zero) ; set the two first segments to 0

	addi t2, zero, 10 ; t2 = 10
	add t3, zero, zero ; tens
	get_tens:
		blt t0, t2, get_final ; check if score < 10
		addi t3, t3, 1 ; if not, add 1 to the tens 
		addi t0, t0, -10 ; and decrement the score by then
		jmpi get_tens ; then loop until we have no more tenths 

	get_final:
		slli t3, t3, 2 ; t3 = t3*4 to get the word of the corresponding tens
		ldw t4, digit_map (t3) ; load the adress to store it in the seven segments display
		stw t4, SEVEN_SEGS + 8 (zero) ; store the tens 

		slli t0, t0, 2 ; t0 now contains the units. t0 = t0*4 to get the word of the corresponding units
		ldw t5, digit_map(t0)
		stw t5, SEVEN_SEGS + 12 (zero)

		ret
; END: display_score


; BEGIN: init_game
init_game:
	; initial snake in 
	; initial position: (0,0)
	; initial length: 1
	; initial direction: DIR_RIGHT
	; add v0, zero, zero 
	; TODO: LIUZHENTAO: I dont know whether should set pixel here
	addi a0, zero, 0 ; a0 = x
    addi a1, zero, 0 ; a1 = y
	stw a0, HEAD_X (zero)
    stw a1, HEAD_Y (zero)
	stw a0, TAIL_X (zero)
    stw a1, TAIL_Y (zero)
	add t0, zero, a0
	add t1, zero, a1
	slli t4, t0, 3 ; 8*x 
    add t4, t4, t1 ; 8*x + y
    slli t4, t4, 2 ; 4*(8*x + y)
	addi t3, zero, DIR_RIGHT
	stw t3, GSA (t4)

    addi a0, zero, 0 ; a0 = x
    addi a1, zero, 0 ; a1 = y
	; set_pixel(x,y)
	addi sp, sp, -20
   	stw s0, 0 (sp)
    stw s1, 4 (sp)
    stw a0, 8 (sp)
    stw a1, 12 (sp)
    stw ra, 16 (sp)
    call set_pixel
    	ldw s0, 0 (sp)
    	ldw s1, 4 (sp)
   		ldw a0, 8 (sp)
    	ldw a1, 12 (sp)
    	ldw ra, 16 (sp)
    	addi sp, sp, 20
	addi sp, sp, -4
    stw ra, 0 (sp)
	call create_food
    ldw ra, 0 (sp)
   	addi sp, sp, 4
	ret


; END: init_game


; BEGIN: create_food
create_food:
	ldw t0, RANDOM_NUM (zero) ; t0 = random number
	addi t1, zero, 255 ; mask = 0x000000FF
	and t1, t0, t1 ; we get the last byte of the random number = t1
	addi t2, zero, 96 ; t2 = 96

	; we check if t1 is a valid number of the grid (numbered from 0 to 95). If not the case, we loop to get another number
	bge t1, t2, create_food
	blt t1, zero, create_food

	slli t1, t1, 2 ; t1 = 4*t1 to get the memory location of the food in the GSA
	ldw t3, GSA (t1) ; we load the position of the food

	; we check if there's nothing on this position, i.e a part of the snake or a food already => the pixel value must be 0
	; if it's not the case, we loop to get another number
	bne t3, zero, create_food

	; now that all checks are done, we can add the value 5 to the array, i.e the value of a food
	addi t4, zero, 5
	
	stw t4, GSA (t1)

	ret
; END: create_food


; BEGIN: hit_test
hit_test:
	ldw t0, HEAD_X (zero) ; t0 = x
	ldw t1, HEAD_Y (zero) ; t1 = y
	add v0, zero, zero ; v0 = 0

	; we compute the direction of the snake
	slli t2, t0, 3 ; t2 = 8x
	add t2, t2, t1 ; t2 = 8x + y
	slli t2, t2, 2 ; t2 = 4(8x + y) to get the memory location 
	ldw t3, GSA (t2) ; current head direction of the snake 

	; we want to find in which direction the snake is going
	addi t4, zero, 1
	beq t3, t4, next_left

	addi t4, zero, 2
	beq t3, t4, next_up

	addi t4, zero, 3
	beq t3, t4, next_down

	addi t4, zero, 4
	beq t3, t4, next_right

	; now we write procedures to get the next pixel in which the snake will go depending on the direction
	next_left:
		addi t0, t0, -1 ; x = x - 1
		blt t0, zero, outside_array ; check if the next position is in the GSA

		slli t5, t0, 3 ; t5 = 8(x - 1)
		add t5, t5, t1 ; t5 = 8(x - 1) + y
		slli t5, t5, 4 ; t5 = 4(8(x - 1) + y)
		ldw t6, GSA (t5) ; next position if the snake goes left 

		; check if the next position is a food 
		addi t7, zero, 5
		beq t6, t7, hit_food

		; check if the next position is the snake itself 
		addi t7, zero, 1
		beq t6, t7, hit_itself
		addi t7, zero, 2
		beq t6, t7, hit_itself
		addi t7, zero, 3
		beq t6, t7, hit_itself
		addi t7, zero, 4
		beq t6, t7, hit_itself
		addi v0, zero, 0
		ret ; if nothing happens

	next_up:
		addi t1, t1, -1 ; y = y - 1
		blt t1, zero, outside_array ; check if the next position is in the GSA

		slli t5, t0, 3 ; t5 = 8(x)
		add t5, t5, t1 ; t5 = 8(x) + y - 1
		slli t5, t5, 4 ; t5 = 4(8(x) + y - 1)
		ldw t6, GSA (t5) ; next position if the snake goes up 

		; check if the next position is a food 
		addi t7, zero, 5
		beq t6, t7, hit_food

		; check if the next position is the snake itself 
		addi t7, zero, 1
		beq t6, t7, hit_itself
		addi t7, zero, 2
		beq t6, t7, hit_itself
		addi t7, zero, 3
		beq t6, t7, hit_itself
		addi t7, zero, 4
		beq t6, t7, hit_itself
		addi v0, zero, 0
		ret ; if nothing happens

	next_down:
		addi t1, t1, 1 ; y = y + 1
		addi t4, zero, 8 ; t4 = 8
		bge t1, t4, outside_array ; check if the next position is in the GSA

		slli t5, t0, 3 ; t5 = 8(x)
		add t5, t5, t1 ; t5 = 8(x) + y + 1
		slli t5, t5, 4 ; t5 = 4(8(x) + y + 1)
		ldw t6, GSA (t5) ; next position if the snake goes down 

		; check if the next position is a food 
		addi t7, zero, 5
		beq t6, t7, hit_food

		; check if the next position is the snake itself 
		addi t7, zero, 1
		beq t6, t7, hit_itself
		addi t7, zero, 2
		beq t6, t7, hit_itself
		addi t7, zero, 3
		beq t6, t7, hit_itself
		addi t7, zero, 4
		beq t6, t7, hit_itself
		addi v0, zero, 0
		ret ; if nothing happens 

	next_right:
		addi t0, t0, 1 ; x = x + 1
		addi t4, zero, 12 ; t4 = 12
		bge t0, t4, outside_array ; check if the next position is in the GSA

		slli t5, t0, 3 ; t5 = 8(x + 1)
		add t5, t5, t1 ; t5 = 8(x + 1) + y
		slli t5, t5, 4 ; t5 = 4(8(x + 1) + y)
		ldw t6, GSA (t5) ; next position if the snake goes right 

		; check if the next position is a food 
		addi t7, zero, 5
		beq t6, t7, hit_food

		; check if the next position is the snake itself 
		addi t7, zero, 1
		beq t6, t7, hit_itself
		addi t7, zero, 2
		beq t6, t7, hit_itself
		addi t7, zero, 3
		beq t6, t7, hit_itself
		addi t7, zero, 4
		beq t6, t7, hit_itself
		addi v0, zero, 0
		ret ; if nothing happens 

	outside_array:
		addi v0, zero, 2
		ret

	hit_food:
		addi v0, zero, 1
		;stw zero, GSA (t5) -> for debug
		ret

	hit_itself:
		addi v0, zero, 2 ; not really efficient, does the same as outside_array but anyway, it's to distinguish the two cases
		ret
; END: hit_test


; BEGIN: get_input
get_input:
	add v0, zero, zero ; default return value

    ldw t0, BUTTONS + 4 (zero) ; edgecapture
    ldw t1, HEAD_X (zero) ; x position of the head
    ldw t2, HEAD_Y (zero) ; y position if the head 

	stw zero, BUTTONS + 4 (zero) ; reset the edgecapture to 0

    slli t4, t1, 3 ; t4 = 8x
    add t4, t4, t2 ; 8x + y
    slli t4, t4, 2 ; 4*(8*x + y) to load the memory 
    ldw t5, GSA (t4) ; t5 contains the current direction of the head


	; check the current direction of the head
    addi t6, zero, DIR_LEFT
    beq t5, t6, leftAndRight_direction ; check if current direction is left

    addi t6, zero, DIR_UP
    beq t5, t6, upAndDown_direction ; check if current direction is up

    addi t6, zero, DIR_DOWN
    beq t5, t6, upAndDown_direction ; check if current direction is down

    addi t6, zero, DIR_RIGHT
    beq t5, t6, leftAndRight_direction ; check if current direction is right  

	ret ; in case there's a problem with the direction s
 
    leftAndRight_direction:
        andi t7, t0, 16
        bne t7, zero, checkpoint ; first check if we pressed the checkpoint button

        andi t7, t0, 2
        bne t7, zero, go_up ; check if we pressed the up button

        andi t7, t0, 4
        bne t7, zero, go_down ; check if we pressed the down button

		ret; if we don't find any button, it means we don't do anything

    upAndDown_direction:
        andi t7, t0, 16
        bne t7, zero, checkpoint ; first check if we pressed the checkpoint button

        andi t7, t0, 1
        bne t7, zero, go_left ; check if we pressed the left button

        andi t7, t0, 8
        bne t7, zero, go_right ; check if we pressed the right button

		ret

    go_left:
        addi t2, zero, BUTTON_LEFT
        add v0, zero, t2
        stw t2, GSA (t4) ; change the current direction to left aka 1

        ret

    go_up:
        addi t2, zero, BUTTON_UP
        add v0, zero, t2
        stw t2, GSA (t4) ; change the current direction to up aka 2

        ret

    go_down:
        addi t2, zero, BUTTON_DOWN
        add v0, zero, t2
        stw t2, GSA (t4) ; change the current direction to down aka 3

        ret

    go_right:
        addi t2, zero, BUTTON_RIGHT
        add v0, zero, t2
        stw t2, GSA (t4) ; change the current direction to right aka 4

        ret

    checkpoint:
        addi v0, zero, BUTTON_CHECKPOINT ; if checkpoint, we do nothing apart assign v0 register to 5

        ret
; END: get_input


; BEGIN: draw_array
draw_array:
	addi s0, zero, 0 ; we set s0 to x = 0
	loop_x:
  		addi s1, zero, 0 ; we set s1 to y = 0
  	loop_y:
    	add t0, s0, zero ; t0 = x
    	add t1, s1, zero ; t1 = y
    	slli t0, t0, 3 ; 8*x
    	add t2, t0, t1 ; t2 = 8*x + y
    	slli t2, t2, 2 ; t2 = 4*(8*x + y)
    	ldw t3, GSA (t2) ; t3 contains the current direction of GSA(t2)
    	beq t3, zero, operate_pixel ; if t3 == 0 then jumps to operate_pixel. understand why
		; (This meanswe if the position has no direction, we should add x/y to check another )  
		; before calling set_pixel, moving sp to save registers
		; TODO: changes the orders
		addi sp, sp, -20
   		stw s0, 0 (sp)
    	stw s1, 4 (sp)
    	stw a0, 8 (sp)
    	stw a1, 12 (sp)
    	stw ra, 16 (sp)
		; before calling set_pixel, set arguments by a0, a1
    	addi a0, s0, 0 ; a0 = x
    	addi a1, s1, 0 ; a1 = y
		; set_pixel(x,y)
    	call set_pixel
		; after calling functions, load saved registers from sp-sp+20
    	ldw s0, 0 (sp)
    	ldw s1, 4 (sp)
   		ldw a0, 8 (sp)
    	ldw a1, 12 (sp)
    	ldw ra, 16 (sp)
    	addi sp, sp, 20
  	operate_pixel:
  		addi s1, s1, 1 ; s1 = s1 + 1 (y = y + 1)
    	addi t1, zero, 8 ; t1 = 8
 		bne s1, t1, loop_y  
  		addi s0, s0, 1 ; s0 = s0 + 1 (x = x + 1)
  		addi t0, zero, 12 ; t0 = 12
 	 	bne s0, t0, loop_x
	ret
; END: draw_array



; BEGIN: move_snake
move_snake:
	; Get the position of Head
	; t3 register in move_snake!!! Be careful with this guy. Think Twice before use it!!!!!
    ldw t1, HEAD_X (zero) ; t1 == x
    ldw t2, HEAD_Y (zero) ; t2 == y
	addi t3, zero, 0 ; decide where to jump after compute_direction
	jmpi compute_direction
	compute_direction:
		slli t4, t1, 3 ; 8*x 
    	add t4, t4, t2 ; 8*x + y
    	slli t4, t4, 2 ; 4*(8*x + y)
		beq t3, zero, snake_head_decide_direction
    	addi t7, zero, 1 
    	beq t3, t7, snake_tail_move_part1
		jmpi snake_tail_move_part2
	snake_head_decide_direction:
    	ldw t5, GSA (t4) ; t5 contains the current direction of the head
		addi t6, zero, BUTTON_LEFT
		beq t5, t6, head_position_change_left 
		addi t6, zero, BUTTON_UP
		beq t5, t6, head_position_change_up 
		addi t6, zero, BUTTON_DOWN
		beq t5, t6, head_position_change_down 
		addi t6, zero, BUTTON_RIGHT
		beq t5, t6, head_position_change_right
	snake_head_move:
		stw t1, HEAD_X (zero)
		stw t2, HEAD_Y (zero)
		addi t3, zero, 1 ; so we can jump back after compute_direction
		jmpi compute_direction
	snake_tail_move_part1:
 		stw t5, GSA (t4) ; we store t5 in GSA (t4) (new position of the snake)
 		; TODO The following two lines come from the internet, BUT I don't know what fucking he is doing. So I comment it, and the snake can work!
		;addi t0, zero, 1 ; TODO: I don't know what this line is doing 
 		; Now we finishing head move, we can start tail move
		; if v0 == 1 to we don't need tail move, and just jump to the end.
		; TODO I just don't know where v0 is fucking coming from.
 		;beq v0, t0, move_snake_end 
		ldw t1, TAIL_X (zero) ; t1 == x
		ldw t2, TAIL_Y (zero) ; t2 == y
		addi t3, zero, 2 ; so we can jump back after compute_direction
		jmpi compute_direction
	snake_tail_move_part2:
    	ldw t5, GSA (t4) ; t5 contains the current direction of the tail
    	stw zero, GSA (t4); make tail to BUTTON_NONE first
    	addi t6, zero, BUTTON_LEFT
		beq t5, t6, tail_position_change_left 
		addi t6, zero, BUTTON_UP
		beq t5, t6, tail_position_change_up 
		addi t6, zero, BUTTON_DOWN
		beq t5, t6, tail_position_change_down 
		addi t6, zero, BUTTON_RIGHT
		beq t5, t6, tail_position_change_right
	snake_tail_move_part3:
		; We cam now update the new tail position by t0, t1
		stw t1, TAIL_X (zero)
    	stw t2, TAIL_Y (zero)

	move_snake_end:
		ret
	head_position_change_left:
		addi t1, t1, -1
		jmpi snake_head_move
	head_position_change_up:
		addi t2, t2, -1
		jmpi snake_head_move
	head_position_change_down:
		addi t2, t2, 1
		jmpi snake_head_move
	head_position_change_right:
		addi t1, t1, 1
		jmpi snake_head_move

	tail_position_change_left:
		addi t1, t1, -1
		jmpi snake_tail_move_part3
	tail_position_change_up:
		addi t2, t2, -1
		jmpi snake_tail_move_part3
	tail_position_change_down:
		addi t2, t2, 1
		jmpi snake_tail_move_part3
	tail_position_change_right:
		addi t1, t1, 1
		jmpi snake_tail_move_part3
; END: move_snake


; BEGIN: save_checkpoint
save_checkpoint:

; END: save_checkpoint


; BEGIN: restore_checkpoint
restore_checkpoint:

; END: restore_checkpoint


; BEGIN: blink_score
blink_score:
	; clear the seven segments display
	stw zero, SEVEN_SEGS (zero)
	stw zero, SEVEN_SEGS + 4 (zero)
	stw zero, SEVEN_SEGS + 8 (zero)
	stw zero, SEVEN_SEGS + 12 (zero) 

	jmpi wait 
	jmpi display_score

	stw zero, SEVEN_SEGS (zero)
	stw zero, SEVEN_SEGS + 4 (zero)
	stw zero, SEVEN_SEGS + 8 (zero)
	stw zero, SEVEN_SEGS + 12 (zero) 

	jmpi wait 
	jmpi display_score

	stw zero, SEVEN_SEGS (zero)
	stw zero, SEVEN_SEGS + 4 (zero)
	stw zero, SEVEN_SEGS + 8 (zero)
	stw zero, SEVEN_SEGS + 12 (zero) 

	jmpi wait 
	jmpi display_score

	ret ; 3 blinks
; END: blink_score

; BEGIN: wait
wait:
	;only for test
	addi t5, zero, 10 ; first counter 
	addi t6, zero, 10 ; second counter
	;addi t5, zero, 10000 ; first counter 
	;addi t6, zero, 10000 ; second counter
	jmpi loop_1 

	; we want to perform 10e8 iterations to wait 0.5s
	loop_1:
		beq t5, zero, loop_2
		addi t5, t5, -1
		jmpi loop_1

	loop_2:
		beq t6, zero, return 
		addi t6, t6, -1
		jmpi loop_2 

	return:
		ret 
; END: wait

digit_map:
	.word 0xFC ; 0
	.word 0x60 ; 1
	.word 0xDA ; 2
	.word 0xF2 ; 3
	.word 0x66 ; 4
	.word 0xB6 ; 5
	.word 0xBE ; 6
	.word 0xE0 ; 7
	.word 0xFE ; 8
	.word 0xF6 ; 9