;*----------------------------------------------------------------------------
;* Name:    Lab_2_program.s 
;* Purpose: This code template is for Lab 2
;* Author: Eric Praetzel and Rasoul Keshavarzi 
;*----------------------------------------------------------------------------*/
		THUMB 		; Declare THUMB instruction set 
                AREA 		My_code, CODE, READONLY 	; 
                EXPORT 		__MAIN 		; Label __MAIN is used externally q
		ENTRY 
__MAIN
; The following lines are similar to Lab-1 but use an address, in r4, to make it easier.
; Note that one still needs to use the offsets of 0x20 and 0x40 to access the ports
;

; Turn off all LEDs 
				MOV 		R2, #0xC000
				MOV 		R3, #0xB0000000	
				MOV 		R4, #0x0
				MOVT 		R4, #0x2009
				ADD 		R4, R4, R2 			; 0x2009C000 - the base address for dealing with the ports
				STR 		R3, [r4, #0x20]		; Turn off the three LEDs on port 1
				MOV 		R3, #0x0000007C
				STR 		R3, [R4, #0x40] 	; Turn off five LEDs on port 2 


ResetLUT
				LDR       	R5, =InputLUT       ; assign R5 to the address at label LUT

NextChar
				LDRB        R0, [R5]			; Read a character to convert to Morse
				MOV			R10, #0xF			; Initialize counter to 16, which decrements as the hex-Morse-code-representation is read
				ADD         R5, #1              ; point to next value for number of delays, jump by 1 byte
				TEQ         R0, #0              ; If we hit 0 (null at end of the string) then reset to the start of lookup table
				
				BNE			ProcessChar			; If we have a character process it

				MOV			R0, #4				; delay 4 extra spaces (7 total) between words
				BL			DELAY
				BEQ         ResetLUT			; repeat for the same set of characters (or different)


ProcessChar	BL		CHAR2MORSE	; convert ASCII to Morse pattern in R1		
;	This is confusing as we're shifting a 32-bit value left, but the data is ONLY in the lowest 16 bits, so test starting at bit 15 for 1 or 0
;	Then loop thru all of the data bits:
;

LeadingZeroes	MOV			R6, #0x8000			; Init R6 with the value for the bit, 15th, which we wish to test
				
				SUB 		R10, R10, #1		; Decrement R10, which counts down as each hex bit is read.R10 has value of 16 as it is the counter register.
				LSL			R1, R1, #1			; Shift R1 left by 1, store in R1; read next bit
				ANDS 		R7, R1, R6			; R7 gets R1 AND R6, Zero bit gets set telling us if the bit is 0 or 1
				BEQ 		LeadingZeroes		; As long as R7 is 0, continue to loop LeadingZeroes
				BL			LED_ON				; Branch LED_ON - turn on the LED 
				

FlashLetter		MOV			R6,#0x8000			; Init R6 with the value for the bit, 15th, which we wish to test. #
				
				TEQ 		R10,#0X0000			; Check if all 16 hex bits have been read
				BEQ 		EndOfLetter			; If there are no more bits to read, branch to EndOfLetter
				
				LSL 		R1, R1, #1			; Shift R1 left by 1, store in R1; read next bit
				ANDS		R7, R1, R6			; R7 gets R1 AND R6, Zero bit gets set telling us if the bit is 0 or 1
				SUB 		R10, R10,#1			; Decrement counter as there is one fewer bit to read
				BLEQ 		LED_OFF	 			
				BLNE 		LED_ON				; If R7 is equal to 1, branch to LED_ON
				B			FlashLetter

EndOfLetter		BL			LED_OFF				; turn LED OFF
				MOV 		R0,#2				; Add another 2 half second delays between letters
				BL 			DELAY				; branch to DELAY
				B 			NextChar			; process the next character


; Subroutines
;
;			convert ASCII character to Morse pattern
;			pass ASCII character in R0, output in R1
;			index into MorseLuT must be by steps of 2 bytes
CHAR2MORSE	STMFD		R13!,{R14}			; push Link Register (return address) on stack
		
			SUB			R0, R0, #0x41		; Convert ASCII value to Morse Code Table entry
			LSL			R0, R0, #1			; Shift R0 left by 1 bit
			LDR			R4, =MorseLUT		; Load R4 with address of Morse Code Table 
			LDRH		R1, [R4, R0]		; Load half word from Morse Code Table in R1
		
			LDMFD		R13!,{R15}			; restore LR to R15 the Program Counter to return


; Turn the LED on, but deal with the stack in a simpler way
; NOTE: This method of returning from subroutine (BX  LR) does NOT work if subroutines are nested!!


LED_ON 	   	STMFD		R13!,{R3, R14}			; preserve R3 and R4 on the R13 stack
	
			MOV			R3, #0xA0000000		; Initialize R3 to the value that turns on the LED
		    LDR			R4, =LED_PORT_ADR	; Load R4 with address of LED port
		    STR			R3, [R4, #0x20]		; Store value of R3 into address to which R4-with-offset-of-20
		    MOV			R0, #1				; Delay by 1 500ms period
			
			BL			DELAY				; Branch to DELAY
		    LDMFD		R13!,{R3, R15}
		   				
		   

; Turn the LED off, but deal with the stack in the proper way
; the Link register gets pushed onto the stack so that subroutines can be nested
;
LED_OFF	   	STMFD		R13!,{R3, R14}		; push R3 and Link Register (return address) on stack
		
			MOV			R3, #0xB0000000		; Initialize R3 to the value that turns off the LED
	  	    LDR			R4, =LED_PORT_ADR	; Load R4 with address of LED port
		    STR			R3, [R4, #0x20]		; Store value of R3 into address to which R4-with-offset-of-20 points
		    MOV			R0, #1				; Delay by 1 500ms period
		    BL			DELAY				; Branch to DELAY
		    LDMFD		R13!,{R3, R15}		; restore R3 and LR to R15 the Program Counter to return

;	Delay 500ms * R0 times
;	Use the delay loop from Lab-1 but loop R0 times around


DELAY	   STMFD		R13!,{R2, R14}
		   MOVT 		R7, #0X000B			; Initialize counter for 500 ms delay
		   MUL			R7,R7, R0			; Delay 500ms * R0 times

DelayLoop	
		   SUBS			R7, #1				; Decrement counter
		   BNE 			DelayLoop			; Continue to loop delayloop until R7 equals 0
		   B			exitDelay			; Branch to exitDelay
				
exitDelay		LDMFD		R13!,{R2, R15}

;
; Data used in the program
; DCB is Define Constant Byte size
; DCW is Define Constant Word (16-bit) size
; EQU is EQUate or assign a value.  This takes no memory but instead of typing the same address in many places one can just use an EQU
;
		ALIGN				; make sure things fall on word addresses

; One way to provide a data to convert to Morse code is to use a string in memory.
; Simply read bytes of the string until the NULL or "0" is hit.  This makes it very easy to loop until done.
;


InputLUT	DCB		"BIRD", 0	; strings must be stored, and read, as BYTES	
								

		ALIGN				; make sure things fall on word addresses
MorseLUT 
		DCW 	0x17, 0x1D5, 0x75D, 0x75 	; A, B, C, D
		DCW 	0x1, 0x15D, 0x1DD, 0x55 	; E, F, G, H
		DCW 	0x5, 0x1777, 0x1D7, 0x175 	; I, J, K, L
		DCW 	0x77, 0x1D, 0x777, 0x5DD 	; M, N, O, P
		DCW 	0x1DD7, 0x5D, 0x15, 0x7 	; Q, R, S, T
		DCW 	0x57, 0x157, 0x177, 0x757 	; U, V, W, X
		DCW 	0x1D77, 0x775 				; Y, Z

; One can also define an address using the EQUate directive
;
LED_PORT_ADR	EQU	0x2009c000	; Base address of the memory that controls I/O like LEDs

		END 
