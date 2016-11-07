; Lab 3 code 
				THUMB 		; Thumb instruction set 
                AREA 		My_code, CODE, READONLY
                EXPORT 		__MAIN
				ENTRY  
				
__MAIN
				LDR 	  R9, =FIO2PIN ; loading onto R9. Store the address of the pin
				LDR R10, =LED_BASE_ADR; ; R10 is a permenant pointer to the base address for the LEDs, offset of 0x20 and 0x40 for the ports
				LDR R6, [R9]
START
				BL 		LED_OFF

; This line is very important in your main program
; Initializes R11 to a 16-bit non-zero value and NOTHING else can write to R11 !!

				MOV		R11, #0xABCD		; Init the random number generator with a non-zero number

;COUNTER 0 - 0xFF
				MOVT R0, #0x0    ;Move the value 1000 into R0
				MOV R0, #0x03E8 
				MOV R3, #0x0     ;Clear R3


countdown	; Count down loops				
		    ; Branch to display number sequence

				BL DISPLAY_NUM
                
				BL DELAY        ;Branch to delay sequence
				ADDS R3, #0x1   ;Add one to R3, the counter
				CMP R3, #0xFF   ;Compare the counter with 255
				BNE countdown 	;If R3 is not equal to 255, continue count down
				
				
; Usefull commaands:  RBIT (reverse bits), BFC (bit field clear), LSR & LSL to shift bits left and right, ORR & AND and EOR for bitwise operations
;SET UP REACTION TIMER			
				
				BL			LED_OFF ; Turn off lights
				
				BL 			RandomNum ; Get a random number, store it in R11
				
				MOV 		R0, #0x0 ;Clear R0
				
                BFI 		R0, R11, #0, #16     ; Move 16 bits of random number in to R0
				
;Scale and offset
				MOV	    R8, #0x64  ; 100
				MOV     R12,#0x7A  ; 122
				MUL	    R0, R12		
				UDIV	R0, R8
				
				
				BL DELAY ; Wait the random time interval
				
				; Turn on just LED p1.29 to indicate when to push the button

				MOV	   R3, #0x90000000		
				STR    R3, [R10, #0x20] 	
                
                ; Branch to reaction timer

				BL REACTION
                
                ; Branch to turn the led off
				BL 			LED_OFF

; R11 holds a 16-bit random number via a pseudo-random sequence as per the Linear feedback shift register (Fibonacci) on WikiPedia
; R11 holds a non-zero 16-bit number.  If a zero is fed in the pseudo-random sequence will stay stuck at 0
; Take as many bits of R11 as you need.  If you take the lowest 4 bits then you get a number between 1 and 15.
; If you take bits 5..1 you'll get a number between 0 and 15 (assuming you right shift by 1 bit).
;
; R11 MUST be initialized to a non-zero 16-bit value at the start of the program OR ELSE!
; R11 can be read anywhere in the code but must only be written to by this subroutine

RandomNum		STMFD		R13!,{R1, R2, R3, R14}

				AND			R1, R11, #0x8000
				AND			R2, R11, #0x2000
				LSL			R2, #2
				EOR			R3, R1, R2
				AND			R1, R11, #0x1000
				LSL			R1, #3
				EOR			R3, R3, R1
				AND			R1, R11, #0x0400
				LSL			R1, #5
				EOR			R3, R3, R1		; the new bit to go into the LSB is present
				LSR			R3, #15
				LSL			R11, #1
				ORR			R11, R11, R3
				
				LDMFD		R13!,{R1, R2, R3, R15}

;Delay 0.1ms (100us) * R0 times
;aim for better than 10% accuracy

DELAY		STMFD		R13!,{R2, R14}
			MOV	R2, #0x007D ; 125
			MUL 	R2, R2, R0;
				
wait		SUBS	R2, #1 			; Decrement r0 and set N,Z,V,C status bits
			BNE			wait
				
exitDelay	LDMFD		R13!,{R2, R15}
				
;LED OFF
; Turn the LED off

LED_OFF    STMFD		R13!,{R3, R14}	; push R3 and Link Register (return address) on stack
           MOV 		R3, #0xB0000000	; Move the value used to turn off the LED
		   STR 		R3, [r10, #0x20] ; Turn off three LEDs on port 1 by moving the turn-off value into the port 1 address
		   MOV 		    R3, #0x0000007C   ; Move the value used to turn off the LED
 		   STR 		    R3, [R10, #0x40]  ; Turn off five LEDs on port 2
	
           LDMFD		R13!,{R3, R15}	  ; restore R3 and LR to R15 the Program Counter to return

;LED ON
LED_ON   	STMFD		R13!,{R3, R14}	 ; Move the value used to turn on the LED
            MOV 		R3, #0x0	; Turn on three LEDs on port 1	 
            STR 		R3, [r10, #0x20]; Turn on five LEDs on port 2	 
            STR 		R3, [R10, #0x40]  ; Restore R3 and LR to R15 the Program Counter to return
            LDMFD		R13!,{R3, R15}

;DISPLAY NUM
;Displays the number passed in R3
; push R0, R1, R2, R3, R4 and Link Register (return address) on stack

DISPLAY_NUM	STMFD		R13!, {R0, R1, R2, R3, R4, R14}	
			MOV 		R1, #0x0;	R1 holds the value for port 1 that will be loaded on; clear first
			MOV 		R2, #0x0; 	R2 holds the value for port 2 that will be loaded on; clear first
			
			;Split the number in R0 into two registers
			;R1: Port 1
			;R2: Port 2
			;depending on which port that bit is on
			; Take the first five bits of R3 and store it in R2
			; We need these values for port2 : 6, 5, 4, 3, 2, 1

			BFI 		R2, R3, #0, #5
			LSR 		R3, R3, #5 ;shifting the 5 bits that are put into R2
			BFI 		R1, R3, #0, #3 ; Take the first three bits of R3 and store in R1
						 	; We need these values in port 1: 31, 29, 28	
;PORT 1	
			RBIT 		R1, R1 ;Reverse the bits in R1. R0 was bit 28, 29, 31 and we need it in 31, 29, 28
			; Shift R1 to the right, then add 0b010... to move the 30th bit to the 31st position						
			LSR 		R1, R1, #1			
			ADD 		R1, #0x40000000		
			EOR 		R1, #0xFFFFFFFF	; Invert R1 since `1` is LED OFF and `0` is LED ON
			STR 		R1, [R10, #0x20]; Turn on three LEDs on port 1  
			

;PORT 2
			
			RBIT		R2, R2 ; Reverse the bits in R2. R0 was 1, 2, 3, 4, 5, 6, and we want 6, 5, 4, 3, 2, 1		
			LSR 		R2, R2, #25  ; Shift R2 to the right by 25 to put it in the correct position for pin address
			EOR 		R2,#0xFFFFFFFF			;0 becomes 1 and 1 becomes 0:	Register for Port 2 complete
			STR 		R2, [R10, #0x40]	
			LDMFD		R13!, {R0, R1, R2, R3, R4, R15}	; Restore R3 and LR to R15 the Program Counter to return


;REACTION TIMER
;Turns on the light, count how much time has passed since light turned on


REACTION	    STMFD		R13!, {R0, R14}
                MOV 		R0, #0x1;
		        MOV		    R7, #0 ; Use R7 as the reaction timer and clear


REACTION_LOOP	BL DELAY
                
              
				ADD R7, #0x1   ; Add one to the counter for the reaction timer
               
				LDR R6, [R9];  Load the value of the button's state
				MOVT R12, #0x0;
				
				MOV R12, #0x3B83;; This is the value when the button is pressed stored in R12
				CMP R6, R12   ; Compare the button's state with the pressed down value            
             
				BEQ LAB_OVER    ; If equal, it is pressed down and the lab is over

				B REACTION_LOOP ; If not equal continue reaction loop timer
                
; Go to this subroutine when the button is pushed

LAB_OVER						
				MOV R5, #0x4;COUNTER in R5 for 32 bits of reaction time, split into 4 8 bit numbers
				MOV R6, R7 ; Move the reaction time into R6
			
; Display the time on the LED's  
          
DISPLAY_TIME
				
				MOV R3, #0x0       ; Clear out R3 at the beginning of each loop				
				BFI R3, R6, #0, #8 ; Move least significant 8 bits to R3 to display				
				BL DISPLAY_NUM     ; Display the 8 bit number stored in R6    
				LSR R6, #8			
				MOVT R0, #0x0;
				MOV R0, #0x4E20;
				BL DELAY           ; Delay for two seconds

                
				SUBS R5, #0x1     ; Decremeent the counter of 4 by 1
				
                
				BNE DISPLAY_TIME   ; Display the time if not equal to 0
				
				
				MOVT R0, #0x0;
				MOV R0, #0xC350;
				BL DELAY	; Delay for five seconds
				
                
				B LAB_OVER      ; Display the time all over again


LED_BASE_ADR	EQU 	0x2009c000 		; Base address of the memory that controls the LEDs 
PINSEL3			EQU 	0x4002c00c 		; Address of Pin Select Register 3 for P1[31:16]
PINSEL4			EQU 	0x4002c010 		; Address of Pin Select Register 4 for P2[15:0]
FIO2PIN 		EQU 	0x2009c054
;BTN_PRESSED		EQU				; When the value at the pin is this, then the button is down
;	Usefull GPIO Registers
;	FIODIR  - register to set individual pins as input or output
;	FIOPIN  - register to read and write pins
;	FIOSET  - register to set I/O pins to 1 by writing a 1
;	FIOCLR  - register to clr I/O pins to 0 by writing a 1

				ALIGN 

				END 




; LAB REPORT

; Q1:

; The maximum number that can be encoded into each bit is:
; 8 bits - 255; 16 bits - 65 535; 24 bits - 16 777 215; 32 bits - 4 294 967 295

; Since each number represents 0.1 ms, the maximum time we can display is:

; 8 bits: 255 * 0.1 ms = 25.5 ms = 0.0255 s

; 16 bits: 6535 ms = 6.535 s

; 24 bits: 1677721.5 ms = 1677.7215 s = 27.962025 minutes

; 32 bits: 429496729.5 ms = 429496.7295 s = 7158.278825 minutes = 119.304647083 hours which is almost 5 days

; Q2:

; The average human reaction time to a visual stimulus is 0.25 second or 250 ms.
; Given that 8 bits displays 0.0255 seconds at maximum, the time is too less for a human being to react.
; Therefore, the best size would be 16 bits, as it can display up to 6.535 seconds and any human will be able to react within that time.
