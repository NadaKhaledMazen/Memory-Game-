
RED_LED EQU P1.7
BLUE_LED EQU P1.6
LED_PORT EQU P2
RS BIT P1.0
E  BIT P1.1
SEVEN_SEGMENT EQU P3
BUTTON_PORT EQU P0

;MAIN

;------------------------------------------------


ORG 00H 

MOV R2,#0
JMP MAIN
ORG 000BH
 JMP ISRT0
 
 



ORG 0100H

MAIN:
;================== TO BE DELETED JUST FOR TESTING LEVELS
   ; MOV R6,#12
    ;JMP LEVEL_3
  
;===============

MOV LED_PORT , #00H
LEVEL_1: 
	MOV LED_PORT,#0H
	MOV DPTR,#LCD_L1
	MOV R6 ,#0
	ACALL LCD
	MOV DPTR ,#LEVEL1 
	MOV R3 ,#04H 
	MOV R5 , #9H
	ACALL VIEW_LED
	
	MOV R7 ,#04H
	MOV DPTR ,#LEVEL1 
	ACALL GAME
	MOV DPTR,#PASS
	ACALL LCD
	JMP LEVEL_2
LEVEL_2:
	MOV LED_PORT,#0H 
        ACALL RESET_TIMER
	MOV DPTR,#LCD_L2
	ACALL LCD
	MOV DPTR ,#LEVEL2 
	MOV R3 ,#05H 
	MOV R5 , #8H
	ACALL VIEW_LED
	MOV R7 ,#05H
	MOV DPTR ,#LEVEL2 
	ACALL GAME
	MOV DPTR,#PASS
	ACALL LCD
	JMP LEVEL_3
LEVEL_3:
	MOV LED_PORT,#0H
        ACALL RESET_TIMER
	MOV DPTR,#LCD_L3
	ACALL LCD
	MOV DPTR ,#LEVEL3 
	MOV R3 ,#05H 
	MOV R5 , #7H
	ACALL VIEW_LED
	MOV R7 ,#05H
	MOV DPTR ,#LEVEL3 
	ACALL GAME
	MOV DPTR,#PASS
	ACALL LCD
	JMP LEVEL_4
	
LEVEL_4:
	MOV LED_PORT,#0H
        ACALL RESET_TIMER
	MOV DPTR,#LCD_L4
	ACALL LCD
	MOV DPTR ,#LEVEL4 
	MOV R3 ,#06H 
	MOV R5 , #6H
	ACALL VIEW_LED
	MOV R7 ,#06H
	MOV DPTR ,#LEVEL4 
	ACALL GAME 
	MOV DPTR,#PASS
	ACALL LCD
	JMP LEVEL_5
	
LEVEL_5:
	MOV LED_PORT,#0H
        ACALL RESET_TIMER
	MOV DPTR,#LCD_L5
	ACALL LCD
	MOV DPTR ,#LEVEL5 
	MOV R3 ,#05H 
	MOV R5 , #4H
	ACALL VIEW_LED
	MOV R7 ,#06H
	MOV DPTR ,#LEVEL5 
	ACALL GAME
	MOV DPTR,#PASS
	ACALL LCD
	JMP LEVEL_6
	
LEVEL_6:
	MOV LED_PORT,#0H
	ACALL RESET_TIMER
	MOV DPTR,#LCD_L6
	ACALL LCD
	MOV DPTR ,#LEVEL6 
	MOV R3 ,#06H 
	MOV R5 , #8H
	ACALL VIEW_LED
	MOV R7 ,#06H
	MOV DPTR ,#LEVEL6 
	ACALL GAME
	MOV DPTR,#PASS
	ACALL LCD
	JMP LEVEL_7
LEVEL_7:
	MOV LED_PORT,#0H
        ACALL RESET_TIMER
	MOV DPTR,#LCD_L7
	ACALL LCD
	MOV DPTR ,#LEVEL7 
	MOV R3 ,#06H 
	MOV R5 , #8H
	ACALL VIEW_LED
	MOV R7 ,#06H
	MOV DPTR ,#LEVEL7 
	ACALL GAME
	MOV DPTR,#WON
	ACALL LCD
	JMP EXIT	
;------------------------------------------------
GAME:
        ACALL START_TIMER ; START THE TIMER AT THE BEGINNING OF EVERY LEVEL  
IS_PRESSED:			;CHECK THE TIMER WHILE WAITING FOR THE USER TO PRESS A BUTTON
        CJNE R4,#0,NATURAL_FLOW ;IF NOT EQUAL ZERO ACCEPT THE PRESSED BUTTON ELSE 
        JMP NOT_EQ		;JUMP TO NOT EQUAL 
        

NATURAL_FLOW:
	       
		MOV P0, #0FFH 
		JNB P0.0 , B1
		JNB P0.1 , B2
		JNB P0.2 , B3
		JNB P0.3 , B4
		JNB P0.4 , B5
		JNB P0.5 , B6
		JNB P0.6 , B7
		JNB P0.7 , B8
		SJMP  IS_PRESSED   
;------------------------------------------------	
	B1: 	
		MOV R3,#01H
		JMP COMPARE
	B2:
		MOV R3,#02H
		JMP COMPARE
	B3:	
		MOV R3,#04H
		JMP COMPARE
	B4:	
		MOV R3,#08H
		JMP COMPARE
	B5:
		MOV R3,#10H
		JMP COMPARE
	B6:	
		MOV R3,#20H
		JMP COMPARE
	B7:	
		MOV R3,#40H
		JMP COMPARE
	B8:	
		MOV R3,#80H
		JMP COMPARE	
;------------------------------------------------		
	COMPARE:
		MOV A , BUTTON_PORT
		CPL A
		MOV LED_PORT,A    
		CLR A
		MOVC A ,@A+DPTR
		;SUBB A,R6
		;JNZ NOT_EQ
		CPL A
		ANL A,R3
		CJNE A,#0,NOT_EQ
		
		CLR  RED_LED            ;IF EQUAL, RED LED OFF
		SETB   BLUE_LED            ;BLUE LED ON
		MOV   B, #9           ;COUNTER B INITIAL VALUE = 9 FOR DELAY
		LOOP2:	    	
			ACALL DELAY           ;APPLY SOME DELAY
			DJNZ  B, LOOP2        ;DECREMENT COUNTER B, JUMP IF NOT 0
		CLR  BLUE_LED            ;BLUE LED OFF
		INC DPTR
		DJNZ  R7, IS_PRESSED
		
		
		;PASSED CASE MEANS THAT USER SUCCEEDED IN PATTERN WE WILL UPDATE SCORE AND RETURN
		;--------------------------------
		MOV B,R6 ; CURRENT VALUE OF SCORE
		MOV A,R4 ; MOV THE REMAING TIME THAT IS STORED IN R4 TO THE ACCUMLATOR 
		ADD A ,B ; ADD THE DIFFERENCE IN TIME TO THE CURRENT VALUE OF SCORE
		MOV R6,A ; RESTORE THE NEW SCORE TO THE R6 REGISTER 
		
		;---------------------------------------
		
RET	
;------------------------------------------------
	
NOT_EQ:  ACALL RESET_TIMER
	SETB   RED_LED            ;RED LED ON
	MOV   B, #9           ;COUNTER B INITIAL VALUE = 9
	LOOP3:          
		ACALL DELAY   
		DJNZ B , LOOP3         ;APPLY SOME DELAY
	CLR  RED_LED
	
	;HERE WE SHOULD CHECK THE SCORE
	ACALL CHECK_SCORE_AFTER_LOSS
	
	
	MOV DPTR,#LOST
	ACALL LCD
	JMP $	
;------------------------------------------------			
VIEW_LED:
	CLR A
	MOVC A ,@A+DPTR	
	;CPL   A 
	MOV LED_PORT ,A
	MOV B , R5
	LOOP0:	  
		ACALL DELAY           
		DJNZ  B, LOOP0        
	INC DPTR
	DJNZ R3,VIEW_LED
	MOV LED_PORT,#0
RET	


;------------------------------------------------
	
DELAY:      
	MOV   A, #0BBH         
LOOP4:      
	MOV   R1, #0BBH 
LOOP5:      

	DJNZ  R1, LOOP5
	DEC A
	CJNE A,0,LOOP4
	
	
RET	
;------------------------------------------------
EXIT:
	JMP $
;--------------------------------------------------
LCD:

	MOV A, #02H
	CALL CMDWRITE

	MOV A, #28H
	CALL CMDWRITE

	MOV A, #0CH
	CALL CMDWRITE

	 
	MOV A, #01H
	CALL CMDWRITE


	;BEGIN AT 5TH COL, 1ST LINE
	MOV A, #84H
	CALL CMDWRITE

	S1:	CLR A
		MOVC A, @A+DPTR
		JZ NEXT
		ACALL DATAWRITE
		;ACALL DELAY
	
		
		INC DPTR
		SJMP S1
		
NEXT: 
 ;==========
 	
         MOV A,#0C0H ;MAKES NEW LINE
	 ACALL CMDWRITE
	 
         MOV DPTR,#SCORE_MESSAGE
 SCORE_VIEW:	
         CLR A
	 MOVC A,@A+DPTR
	 JZ SCORE_VALUE_VIEW
	 ACALL DATAWRITE
	 INC DPTR
	 SJMP SCORE_VIEW
	  
SCORE_VALUE_VIEW: 
                
                 CLR A ; CLEAR THE A
                 MOV A,R6 ; MOV THE SCORE TO THE ACUMLATOR
                 MOV B,#10; 
                 DIV AB ;
                 ADD A,#30H ; CONVERT THE VALUE TO A CHARCTER 
                 ACALL DATAWRITE;SEND THE TENS
                 
                 MOV A,B
                 ADD A,#30H
                 ACALL DATAWRITE;SEND THE UNITS
                 
                 
                 
                ; ADD A,#30H
                 ;ADD A,R6
                 ;ACALL DATAWRITE 
                 
                 
              
	 
	 
;==========

EE:

RET 

;-----------------------------------------------------------
; SUBROUTINES TO ADJUST THE NIBBLES TO MATCH THE LCD DATA BUS 
HIGH_NIBBLE_ADJUST:
	ANL A, #11110000B ;MASK HIGHER NIBBLE
	RR A
	RR A
	
RET
;-----------------------------------------------------------
LOW_NIBBLE_ADJUST:
	RL A
	RL A
	RL A
	RL A
	ANL A, #11110000B ;MASK HIGHER NIBBLE
	RR A
	RR A
	
RET
;------------------------------------------------------------
CMDWRITE:
	; THE DATA NEEDS TO BE ADJUSTED SO THE NIBBLE CAN BE PLACED IN THE 5, 4, 3, 2 OUT OF (7 ~ 0) BITS
	MOV R0, A ;MAKE A COPY OF THE COMMAND
	CALL HIGH_NIBBLE_ADJUST
	MOV P1, A
	CLR RS
	SETB E
	CALL DELAY
	CLR E
	
	
	;PREPARE THE LOWER NIBBLE BY SHIFTING
	MOV A, R0 
	CALL LOW_NIBBLE_ADJUST
	MOV P1, A
	CLR RS
	SETB E
	CALL DELAY
	CLR E
	
	;CALL DELAY
RET
;-------------------------------------------------------
;;;; DATA WRITE SUBROUTINE FOR 4-BIT MODE;;;;
DATAWRITE:
        MOV R0, A ;MAKE A COPY OF THE DATA
	CALL HIGH_NIBBLE_ADJUST
	MOV P1, A
	SETB RS
	SETB E
	CALL DELAY
	CLR E
	
	; PREPARE THE LOWER NIBBLE BY SHIFTING
	MOV A, R0 
	CALL LOW_NIBBLE_ADJUST
	MOV P1, A
	SETB RS
	SETB E
	CALL DELAY
	CLR E
	
	;CALL DELAY
RET
;------------------------------------------------

START_TIMER:
    
  
       MOV IE, #10000010B	;ENABLE EXTERNAL INTERRUPTS ET0, EX0
        SETB IT0 ; NEGATIVE EDGE TRIGGER FOR INTERRUPT 0

     	MOV R2,#0 ; NUMBER OF OVERFLOWS	
	MOV R4,#9
	MOV SEVEN_SEGMENT,R4
	
	MOV TMOD, #00000001B ; TIMER 0 AS 16-BIT TIMERS
         

	
;TIMER CLK = 11.0592/12*1 = 0.9216 MHZ (1 CYCLE = 1.085.... µS)
	;   50000 US = (65536 - COUNT) * (1 / 0.9216). COUNT = 19456 => 4C00H
	;OR 50000 US / (1 / 0.9216) US = 19456 (65536 - 19456 = 19456 => 4C00H)
	
	
        MOV TH0,#4CH
        MOV TL0,#00H
        
	SETB TR0
	


RET


ISRT0:
; 1 SEC REQUIRES 20 OVERFLOW



        ;OVERFLOW TIMER INTERRUPT
  
        CLR TR0
        ;RELOAD TIMERS
        MOV TH0, #4CH
        MOV TL0, #00H
	;CLR TF0
	CJNE R2 ,#20,NOT_END_YET
	
	MOV R2,#0; RESTART OVERFLOW COUNTER
	DEC R4
	MOV SEVEN_SEGMENT,R4
	
        CJNE R4,#0,TIMER_NOT_FINISH
      
     
        
        JMP END_TIMER
        
        
        
TIMER_NOT_FINISH:        
	   
	JMP RESTART_TIMER

           
NOT_END_YET:	
	INC R2	
	;RUN AGAIN
 RESTART_TIMER: SETB TR0
                
 
 END_TIMER:
        
        RETI
        

RESET_TIMER:
            CLR TR0 
            MOV R4,#9
            MOV SEVEN_SEGMENT,#9
            RET

;------------------------------------------------------

CHECK_SCORE_AFTER_LOSS:
 
            ;WE ARE CHECKING ON R6
            
            MOV A,R6
            
          ;{CJNE SRC,DEST} : (FIRST ADD SECOND)  IF (FIRST > = SECOND) => [CARRYFLAG = 0] ELSE [CARRYFLAG = 1]
          
          CJNE A,#12,CHECK1 
          ACALL TRY_AGAIN_PRINT
          JMP LEVEL_2 ;IF SCORE = 12
            
   CHECK1: JC GOTO_LEVEL_1  ; A : SCORE  # A < 12 CARRY FLAG WILL IS 1 # GO TO LEVEL 1 
          
          
          
          
          CJNE A,#20 ,CHECK2 ; IS SCORE LESS THAN 20 JMP TO LEVEL 2
          ACALL TRY_AGAIN_PRINT
          JMP LEVEL_3 ; ELSE GO TO LEVEL 3
           
           
        
        
   CHECK2:  JC GOTO_LEVEL_2 
   
           CJNE A,#35,CHECK3
           ACALL TRY_AGAIN_PRINT
           JMP LEVEL_4       
                    
   CHECK3: JC GOTO_LEVEL_4 
            JMP LEVEL_3  ;IF SCORE <25 BUT GREATER THAN 20          
            
                    
     GOTO_LEVEL_1: RET 
           
           
     GOTO_LEVEL_2 : ACALL TRY_AGAIN_PRINT
                   JMP LEVEL_2   
         
     GOTO_LEVEL_3 :ACALL TRY_AGAIN_PRINT 
                   JMP LEVEL_3                  
      
     GOTO_LEVEL_4 : ACALL TRY_AGAIN_PRINT
                      JMP LEVEL_4                  
      
RET

TRY_AGAIN_PRINT:
 
	MOV A, #01H
	ACALL CMDWRITE


	;BEGIN AT 5TH COL, 1ST LINE
	MOV A, #84H
	ACALL CMDWRITE

                
        
        MOV DPTR,#TRY_AGAIN_MESSAGE
        M:  CLR A
           MOVC A,@A+DPTR
           JZ Z
           INC DPTR
           ACALL DATAWRITE       
           SJMP M
                  
 Z:                 
 RET
                  


;-----------------------------------------------
ORG 400H 
  LEVEL1: DB 01H, 02H, 04H, 08H
  LEVEL2: DB 08H, 02H, 04H, 02H, 10H
  LEVEL3: DB 04H, 08H, 80H, 20H, 01H
  LEVEL4: DB 02H, 80H, 02H, 40H, 01H, 10H
  LEVEL5: DB 10H, 08H, 01H, 08H, 04H, 02H
  LEVEL6: DB 04H, 20H, 10H, 02H, 80H, 04H
  LEVEL7: DB 20H, 80H, 08H, 02H, 10H, 08H
;------------------------------------------  
  LCD_L1 : DB "LEVEL 1",0
  LCD_L2 : DB "LEVEL 2",0
  LCD_L3 : DB "LEVEL 3",0
  LCD_L4 : DB "LEVEL 4",0
  LCD_L5 : DB "LEVEL 5",0
  LCD_L6 : DB "LEVEL 6",0
  LCD_L7 : DB "LEVEL 7",0

  PASS: DB "PASSED",0
  WON: DB "YOU WON",0
  LOST: DB "GAME OVER!",0
  SCORE_MESSAGE: DB "SCORE:",0
  
  TRY_AGAIN_MESSAGE: DB "TRY AGAIN",0
  
      
END