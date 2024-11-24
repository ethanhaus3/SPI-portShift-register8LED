;*******************************************************************************
;
;    CS 107: Computer Architecture and Organization -- LAB 8
;    Filename: Lab08.s
;    Date: 5/4/23
;    Author: Ethan Haus
;
;*******************************************************************************
;
	GLOBAL __main
	AREA main, CODE, READONLY
	EXPORT __main
	EXPORT __use_two_region_memory
__use_two_region_memory EQU 0
	EXPORT SystemInit
	ENTRY

	GET		BOARD.S

main_delay	EQU	500000

; System Init routine
SystemInit
;
; Remember: for 'nested' procedure calls you have to save LR...
	PUSH {LR}       ;push load register to start program
;
; Call your 'SetClock' procedure (Lab07) with parameter '1'
; CPU clock = 12 MHz from OSC
;
	MOV R1, #1                ;pass in parameter 1 to control clock speed
	BL SetClock                ;call SetClock subroutine with given parameter
	

;
; Call your 'SetupSPI' procedure.
;
	BL SetupSPI                ;call setupSPI subroutine to setup SPI ports

;
; return from procedure
;
	POP {PC}                     ;return to given code

	ALIGN

__main
;
; --- You may write some additional initialization code here...
; Assign register(s) for SPI_BASE, GPIO_PORT base and for N
;
; R0 = SPI_BASE
; R1 = GGPIO_BASE
; N (R2) = 0
;
	LDR R0, =C595_SSP          ;constant R0 register is SSP base address
	LDR R1, =GPIO_P1_BASE       ;constant R1 register is GPIO base address
	MOV R2, #0                 ; register R2 is N, N is 0
	
	

loop
;
; delay(main_delay)
;
; Call your 'delay' procedure (Lab05). Pass 'main_delay' as parameter
;
	LDR	R3, =main_delay       ;load given delay value
	BL delay                     ;call delay function
	
	

;
; N = (N + 1) and 0xFF
;
	ADD R2, R2, #1                 ;N = N + 1
	AND R2, R2, #0xFF               ;AND N with given #0xFF value, this is new N value
	

;
; Set RCK low...
;
	LDR R3, =C595_RCK_BIT         ;load specific SSP bit
	STR R3, [R1, #CLR]              ;clear the bit at a given address
	

;
; Send N to SPI
;
	STR R2, [R0, #DR]            ;store N to SPI

;
wait_for_SPI
;
;
	LDR R3, [R0, #SR]             ;loop to make sure SPI is configurated
	TST R3, #SR_BSY
	BNE wait_for_SPI
	

; 
; Set RCK High (copy shift register to output)
;
	LDR R3, =C595_RCK_BIT               ;load same bit as the Set Low
	STR R3, [R1, #SET]                  ;use given set value to load a certain value into the given bit at the given address

;
	B		loop	; Loop forever!
	
	ALIGN

;
; SetupSPI - procedure to set pins, power SPI module on and
; setup SPI interface. Use guidelines in Lab08.doc file and
; sample code from lecture presentation.
;
; Use some difinitions made in 'board.s' file.
;
SetupSPI
;
; Setup SPI/RCK pins...
;
	PUSH {R0-R2, LR}
	LDR R0, =C595_IOCONF_PORT                ;load base address for SPI in R0
	LDR R1, =C595_IOCONF_RCK_CFG               ;load local configuration into R1
	STR R1, [R0, #C595_IOCONF_RCK_PIN]          ;store PIN value of address into local configuration
	LDR R1, =C595_IOCONF_SCK_CFG                ;repeat steps 2 AND 3
	STR R1, [R0, #C595_IOCONF_SCK_PIN]
	LDR R1, =C595_IOCONF_MOSI_CFG
	STR R1, [R0, #C595_IOCONF_MOSI_PIN]
	

;
; RCK Pin set to GPIO output and clear the pin (set to 0)...
;
	LDR R0, =C595_RCK_BIT               ;gpio CONFIGURATION
	LDR R1, =GPIO_P1_BASE               ;GPIO base
;	LDR R2, [R1, #DIR]
;	ORR	R2, R2, R0
	STR R0, [R1, #DIR] 	;R1, #DIR             ;set config to output
	STR R0, [R1, #CLR]                       ;clear config

;
; Turn SPI power on...
;
	LDR R1, =SYSR_PCONP
	LDR R2, [R1] ; R2 = PCONP –- Read!
	ORR R2, #PCONP_PCSSP0 ; Modify! Turn On...
	STR R2, [R1] ; PCONP = R2 –- Write!

;
; Setup SPI...
;
	LDR R1, =C595_SSP
	LDR R0, =(CR0_DSS_8:OR:CR0_FRF_SPI:OR:CR0_CPOL_LOW:OR:CR0_CPHA_FRST:OR:(0<<CR0_SCR_OFFSET))
	STR R0, [R1, #CR0]            ;store base offset into possible PIN
	MOV R0, #2
;	LDR R1, =SSP0_CPSR
	STR R0, [R1, #CPSR]             ;pass 2 into
;	LDR R1, =SSE
	LDR R0, =CR1_SSE
	STR R0, [R1, #CR1]            ;why do we pass 2(because it's the value of CR1_SSE) 
	
	

;
; return from procedure
;
	POP	{R0-R2,PC}

	ALIGN

;
; 'delay' procedure. Use the one you did for Lab05.
; If necessary, change the working register (register you
; are using as the count and/or to pass the parameter into 
; procedure; 
;
; -- copy your 'delay' procedure here (use your Lab05 source file)
delay
	PUSH{R3, LR}
delay_loop
	SUBS	R3, #1                       ;subtract r3 by 1
	BNE		delay_loop   ;break if zero
	POP {R3, PC}
	ALIGN

;
; proc SetClock(r1)
; 
; R1 = 0 -- Set CPU clock to 12 MHz from IRC (Optionally: turn OSC and PLL off)
; R1 = 1 -- Set CPU clock to 12 MHz from OSC
; R1 = 2 -- Set CPU clock to 24 MHz from PLL0
; R1 = 3 -- Set CPU clock to 120 MHz from PLL0
;
; R0 = SysCtl base address
; R2 - working var
;
; -- copy your 'SetClock' procedure here (use your Lab07 source file)
SetClock
;--- Write your SetClock procedure here!
	PUSH {R0-R2,LR}
	LDR R0, =SYSTEM_CTRL_BASE
	CMP R1, #0
	BNE SET_12_MHZ_PLL0
	LDR R2, [R0, #CCLKSEL]
	AND R2, #CCLKSEL_CCLKSEL
	STR R2, [R0, #CCLKSEL]
	MOV R2, #0
	STR R2, [R0, #CLKSRCSEL]
	LDR R2, [R0, #SCS]
	BIC R2, #SCS_OSCEN
	STR R2, [R0, #SCS]
	POP{R0-R2, PC}
	ALIGN
SET_12_MHZ_PLL0
	LDR	R2,[R0, #SCS] ;R2 == SCS -- Read!
	ORR R2, #SCS_OSCEN ; Modify! set bit...
	STR R2, [R0, #SCS]  ;SCS == R2 -- Write
	
wait_for_osc_ready
	LDR R2, [R0, #SCS]  ;R2 == SCS
	TST R2, #SCS_OSCSTAT  ;check OSCSTAT bit
	BEQ wait_for_osc_ready  ;repeat if still 0
	;Set CLKSRCSEL.CLKSRC bit to '1'
	; 'write all at once' method
	MOV R2, #CLKSRCSEL_CLKSRC  ;R2 = 1
	STR R2, [R0, #CLKSRCSEL] ; CLKSRCSEL = R2
	CMP R1, #1
	POPEQ {R0-R2,PC}
	ALIGN
	; THe IF Statement
	CMP R1, #1
	BNE notEqualsTwo
	;M = 24/12
	;P = 24(2 * 4) = 24 * 2P WITHIN RANGE 156-320 MHZ
	LDREQ R2, =(PLLCFG_MSEL_2:OR:PLLCFG_PSEL_4)
	BEQ skip
	
notEqualsTwo
	;M = 120/12
	;P = 120(2 * 1) = 120 * 2P within range 156-320 MHZ
	LDR R2, =(PLLCFG_MSEL_10:OR:PLLCFG_PSEL_1)
	
skip
	STR R2, [R0, #PLLCFG0]  ;PLLCFG0 = R2
;set PLLCON0.PLLE bit to '1'
; 'Write all at once'
	MOV R2, #PLLCON_PLLE
	STR R2, [R0, #PLLCON0]
	MOV R2, #0xAA
	STR R2, [R0, #PLLFEED0] ;PLLFEED0 = 0xAA
	MOV R2, #0x55
	STR R2, [R0, #PLLFEED0] ;PLLFEED0 = 0x55
; Wait for '1' in PLLSTAT0.PLOCK
;'do .. while' loop
wait_for_pll
	LDR R2, [R0, #PLLSTAT0] ; R2 = PLLSTAT0
	TST R2, #PLLSTAT_PLOCK  ; check PLOCK bit
	BEQ wait_for_pll  ; repeat if still 0
;set CLKSEL.CCLKSEL bit to '1'
;READ-MODIFY_WRITE method
	LDR R2, =(CCLKSEL_CCLKSEL + 1) 
	STR R2, [R0, #CCLKSEL]
	POP{R0-R2,PC}
; In case you need some data 'constants', define it here
;

	AREA variables, DATA, ALIGN=2
;
; In case you need some 'variables', define it here
;
	END