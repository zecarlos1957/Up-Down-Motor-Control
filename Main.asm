; Autor Jose Carlos Jesus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;    9-02-2019
;   add RC6  IR protocol  (Philips)
;   add SIRC IR protocol  (Sony)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;    7-01-2019
;
;    2 stage protection with programable delay
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   28-12-2018
;
;   transfer IR decoder code to 0x200
;    added funtion FuncPTR to return protocol
;    decoder function address
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   24-12-2018
;   add RC5  IR protocol   (Philips)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,,
;
;   21-12-2018
;   add  PWC (NEC) InfraRed protocol
;   interrupt on RB0 change IR receiver
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   25-11-2018
;   add detection  up/down limit switch
;      PORTA_1  input PWM  I_out Triac (R4 = 3,3hom)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   1-09-2018 basic button interface
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;****************************************
;

;   1   RA2    btn Up
;   2   RA3    btn down
;   3   RA4    btn Cfg  (Schmith Trigger)
;   4   RA5    mode     (Schmitt Trigger input only)
;   5   GND
;   6   RB0    IR
;   7   RB1
;   8   RB2
;   9   RB3     -
;  10   RB4    led Up        (I=6ma)
;  11   RB5    Triac  Up     (I=7ma)
;  12   RB6    led Down
;  13   RB7    Triac Down
;  14   VDD
;  15   Osc2
;  16   Osc1
;  17   RA0     -
;  18   RA1    PWM pulse - (Schmith Trigger)  I_out Triac (R4 = 3,3hom)

; cmcon configuration '111'



    PROCESSOR 16F627

    #include "P16f627.inc"           ; include all register declaration
        ;_INTRC_OSC_NOCLKOUT / _XT_OSC
    	__CONFIG _XT_OSC  & _MCLRE_OFF & _LVP_OFF & _BODEN_OFF & _PWRTE_ON &  _WDT_OFF & _CP_OFF  & _DATA_CP_OFF
    	            ; Using XT OSC
    	            ; MCLR = I/O RA5
    	            ; Disable low voltage programming, PGM pin can be used as standard I/O
    	            ; Enable Brown out detect
    	            ; Enable Power-up Timer
    	            ; Disable Watch Dog Timer



       cblock 0x20
bIntSaveOption
bIntSaveStatus
bIntSaveFSR
bIntSavePCLATH
bIntTemp

bFlags
bdata
delay_k200
delay_mult
T1
addr
ir_cmd_up     ;0x2A
ir_cmd_down
byte_sz
k_id
H_limit
L_limit       ; 0x30
T_limit
T_pulse
Tbit
sync
temp
     endc

      cblock   0x70
bCount        ; equ 0x70
curr          ; equ 0x71
stage_delay   ; equ 0x73
bIntSaveW     ; equ 0x74
       endc

buf equ 0xa0

#define CONFIG_1     b'10000111'  ; OPTION_REG config
;#define CONFIG_2    b'10000101'
;#define OPTION_MASK b'11111101'



#define _IR_DETECT     bFlags,0
#define _PROTECT_STAGE bFlags,1
#define _IR_FRAME      bFlags,2
#define _IR_CFG        bFlags,3
#define _SMART_BTN     bFlags,4
#define _LONG_PRESS    bFlags,5


    ORG 0x00

           goto Main


     ORG 0x04
    ; Save context
     movwf bIntSaveW
     swapf STATUS,W
     bcf STATUS,RP0
     movwf bIntSaveStatus
     movfw FSR
     movwf bIntSaveFSR
     movf PCLATH,W
     movwf bIntSavePCLATH
     clrf PCLATH

     ;   RB0 interrupt
      btfsc INTCON,INTF
      goto Key_IR


     ; TMR1 interrupt
     btfsc PIR1,TMR1IF
     goto TMR1_ISR

     ; TMR0 interrupt
     btfsc INTCON,T0IF
     goto TMR0_ISR

IntReturn
     bcf STATUS,RP0
     movf bIntSavePCLATH,W
     movwf PCLATH
     movf bIntSaveFSR,W
     movwf FSR
     swapf bIntSaveStatus,W
     movwf STATUS
     swapf bIntSaveW,F
     swapf bIntSaveW,W
     retfie



Main
  ;   movlw 0x20
  ;   movwf FSR
  ;   movlw .80
  ;   call _memclr



  ;   movlw 0xA0
  ;   movwf FSR
  ;   movlw .80
  ;   call _memclr

     movlw 0x16
     movwf H_limit
     movlw 0x12
     movwf L_limit
     movlw 1         ; -> set delay protection  stage to 524,288ms * n
     movwf T_limit
                     ; (x=100) LBYTE -> (x*256*8) = 204,800ms
                     ; (x=125) LBYTE -> (x*256*8) = 256ms
                     ; (x=150) LBYTE -> (150*256*8) = 307,200ms
                     ; (x=170) LBYTE -> (170*256*8) = 348,160ms
                     ; (x=180) LBYTE -> (180*256*8) = 368,640ms
                     ; (x=200) LBYTE -> (200*256*8) = 409,600ms
                     ; (x=250) LBYTE -> (250*256*8) = 512ms

                     ; HBYTE -> (256*256*8) = 524,288 ms
     call Setup

     clrf curr
     clrf bFlags

     ; update  device address
      ; and IR command keys

     movlw 0x7D
;     bcf INTCON,GIE
     bsf STATUS,RP0
     movwf EEADR
     call read_eeprom
     bcf STATUS,RP0
     movwf addr
     bsf STATUS,RP0
     incf EEADR
     call read_eeprom
     bcf STATUS,RP0
     movwf ir_cmd_up
     bsf STATUS,RP0
     incf EEADR
     call read_eeprom
     bcf STATUS,RP0
     movwf ir_cmd_down

     ; start IR interface
     movlw buf
     movwf FSR
     btfss PORTB,0
     goto $-1
     movlw 0xff
     call delay_ms
     movlw 0xff
     call delay_ms
     bcf INTCON,INTF
     bsf INTCON,INTE    ; enable RB0 int

     bsf INTCON,GIE

;     call testeIR
 MainLoop:

     btfss PORTA,4
     goto IR_Cfg     ; Configure IR command keys

     btfsc _IR_DETECT
     goto Ir_Command  ; execute IR command

     movfw PORTA
     andlw 0x0C
     btfss STATUS,Z
     call KeyPress   ;  manual Key interface

     btfsc T1CON,TMR1ON
     call engine     ;  if command event, doit
     goto MainLoop



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

;  signal repetition interval
; NEC      110ms
; RC5      100ms
; Samsung  100ms
; RC6       83m2
; CIRC      45ms

; wait 131ms without signal on RB0


wait_IR_key_depress

    movlw 2        ; 256*256 = 65536us * N
    movwf curr
    bcf INTCON,T0IF
                    ; 256*256 = 65536us * 2 = 131ms
   clrf TMR0        ; 256*256 = 65536us * 3 = 196ms
                    ; 256*256 = 65536us * 4 = 262ms
k_loop
    btfss PORTB,0
    clrf TMR0
    btfss INTCON,T0IF
    goto k_loop
    bcf INTCON,T0IF
    decfsz curr
    goto k_loop
    return

testeIR
    btfss _IR_FRAME
    goto $-1
    bcf _IR_FRAME
    bcf _IR_DETECT
    bsf PORTB,4
    call wait_IR_key_depress
    bcf PORTB,4
    movlw 0xff
    call delay_ms
     bcf INTCON,INTF
     bsf INTCON,INTE    ; enable RB0 int

    goto testeIR
    return



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;   in : RA1 Iout PWM pulse
;        TMR0  20ms
;   out:
;    return  T lenght from RA1 low pulse in 20ms period
;    exit on rising edge of RA1

get_Iout
    clrf bCount
    bcf INTCON,T0IF
    movlw .176        ; (256-176)*256 = 20,480us
    btfss _IR_DETECT  ; prevent TMR0 corruption when  reading IR signal ( RB0 INT enabled)
    movwf TMR0
wait_20ms
    btfsc _IR_DETECT
    return            ; goto IR_Command func if RB0 INT start building IR frame
    ;   T = lenght low pulse on RA1
     btfsc PORTA,1
     goto set_data
     movf bCount,W    ; RA1 is LOW
     btfss STATUS,Z
     goto time_out
     movfw TMR0       ; if bCount == 0
     movwf bCount     ; hold first ocurrence
     goto time_out          ;
set_data
    movf bCount,W     ;
    btfsc STATUS,Z    ; if bCount > 0
    goto time_out
    subwf TMR0,W      ; T=TMR0-bCount
    return           ; exit 20ms loop
time_out                    ; [bCount==0]
    btfss INTCON,T0IF ;  if time_out exit loop
    goto wait_20ms

  ; if RA1 is LOW loop back
    btfsc PORTA,1
    return
    bcf INTCON,T0IF
    goto wait_20ms+2
    return


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;

engine
      movlw buf
     movwf FSR


    movfw T_limit
    movwf stage_delay

    bcf _PROTECT_STAGE
    bcf _LONG_PRESS
wait_limit

     call get_Iout
;          movlw 0xb2
;          movwf bCount
;          movlw 0x10
    btfsc _IR_DETECT
    return

     movwf T_pulse            ; save data T period

;;;;;;;;;;;;;;;;;;;;;

   ;  TMR1 is runing now, starting from button press.
   ;
   ; switch  stage protection after T_limit
   ;  T_limit ( HBYTE )


     movf stage_delay,W   ; process HBYTE
     bnz stage1
     bsf _PROTECT_STAGE
     goto stage2
stage1               ;  Compare to Hi_limit
     movfw T_pulse         ;
     subwf H_limit,W
     btfss STATUS,C  ; if Iout > Hi_limit
     clrf bCount     ; disconnect
      goto Key_event
stage2               ;  Compare to Low_limit
     movfw T_pulse
     subwf L_limit,W
     btfss STATUS,C    ; if Iout > Low_limit
     clrf bCount       ; disconnect

Key_event  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    movlw 0x80          ; to confirm btn depress
    movwf temp          ; we repeat btn state testing temp times
    movfw PORTA
    andlw 0x0C
    btfss STATUS,Z      ; jmp if btn release
    goto k_press
    decfsz temp         ; (n * loop_inst*1)us -> 255*8 = 2,040ms
    goto Key_event+2    ; wait stable low voltage
    ; a  key as been depress
    bcf INTCON,INTF
    bsf INTCON,INTE     ; enable RB0 INT after IR AGC reajust because leds are ON (IR ambient light)
    btfsc _SMART_BTN
    goto limitSW
     btfsc _LONG_PRESS    ; if long btn press
     goto TurnOFF         ; TurnOFF
     bsf _SMART_BTN
     goto limitSW
k_press   ; a key as been pressed
     btfsc _SMART_BTN
     goto TurnOFF         ; TurnOFF
limitSW
     movf bCount,W
     btfss STATUS,Z        ;  if Iout == 0 all done
     goto wait_limit
all_done
    goto TurnOFF



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; RA2/RA3 - Buttons
; RB7/RB5 - Triacs
; RB6/RB4 - Leds

KeyPress
   movlw 0xff
   movwf temp
j2
   movfw PORTA
   andlw 0x0C
   btfsc STATUS,Z
   goto MainLoop
   decfsz temp
   goto j2        ; wait stable hi voltage on button IO
   bcf INTCON,INTE  ; disable to prevent RB0 INT trigger by led light
                    ; enable it after depress button giving time to IR AGC reajust
   movwf bdata
   movlw 0x20
   call delay_ms
    btfss PORTB,5  ;  if motor is runing up
    btfsc PORTB,7  ;  or down
    goto TurnOFF   ;   shutdown and return
    btfsc PORTA,3
    goto Cmd_Down
    btfsc PORTA,2
    goto Cmd_Up
   return



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



Cmd_Up:
    bsf PORTB,5     ;    GO Up
    bsf PORTB,4     ;   set led on
    bsf T1CON,TMR1ON ;    start TMR1
    return



Cmd_Down:
    bsf PORTB,7    ;  GO Down
    bsf PORTB,6   ; set led
    bsf T1CON,TMR1ON
    return



TurnOFF

     bcf T1CON,TMR1ON
     movlw 0x0b
     movwf TMR1H
     movlw 0xdb
     movwf TMR1L
     movlw 2
     movwf T1
     clrf PORTB

     movfw PORTA
     andlw 0x0C
     movwf bdata     ; update command data
     movlw 0xff
     movwf temp
     movfw PORTA
     andlw 0x0C
     btfss STATUS,Z
     goto $-5     ; wait btn depress
     decfsz temp  ; (n * loop_inst*1)us -> 255*8 = 2,040ms
     goto $-5     ; wait stable low voltage
     bcf _SMART_BTN

     ;   save environment:
     ; 0x79 -> 0x8C         PCON
     ; 0x7A -> 0x26     bdata  ->  command key
     ; 0x7B -> 0x32     T_pulse   ->  PWM Amp. current
     ; 0x7C -> 0x70     bCount ->  trigger to stop
     ; 0x7D -> IR addr
     ; 0x7E -> IR cmd Up
     ; 0x7F -> IR cmd Down

     bcf INTCON,GIE
     bsf STATUS,RP0
     movlw PCON
     movwf FSR
     movlw 0x79
     movwf EEADR
     movfw INDF
     movwf EEDATA
     call write_eeprom
     incf EEADR
     movlw bdata
     movwf FSR
     movfw INDF
     movwf EEDATA
     call write_eeprom
     incf EEADR
     movlw T_pulse
     movwf FSR
     movfw INDF
     movwf EEDATA
     call write_eeprom
     incf EEADR
     movlw bCount
     movwf FSR
     movfw INDF
     movwf EEDATA
     call write_eeprom
      bcf STATUS,RP0
     bsf INTCON,GIE
     return



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Loop while building a receved frame by PORTB,0
;   ISR (interrupt service routine)

Ir_Command

    btfss _IR_FRAME
    goto Ir_Command
    ; all done
    bcf _IR_DETECT
    bcf _IR_FRAME

    movlw buf
    subwf FSR,W
    movwf bCount    ; save # edges list size

;---------------------------------------------
    movwf curr      ; update  # edges list size
    movlw buf
    movwf FSR
    bsf STATUS,RP0
    clrf EEADR
loop3
    movfw INDF
    movwf EEDATA
    bcf INTCON,GIE
    call write_eeprom
    bsf INTCON,GIE
    incf EEADR
    incf FSR
    decfsz curr
    goto loop3
    bcf STATUS,RP0
;...........................
process
    call validate_NEC_frame
    addlw 0
    btfss STATUS,Z
    goto protocol
    call validate_RC5_frame
    addlw 0
    btfss STATUS,Z
    goto protocol
    call validate_RC6_frame
    addlw 0
    btfss STATUS,Z
    goto protocol
    call validate_SIRC_frame
    addlw 0
    btfss STATUS,Z
    goto protocol
    call validate_SAMSUNG_frame
    addlw 0
    btfss STATUS,Z
    goto protocol
    goto done
protocol
   movwf curr         ; update function index
   decf curr          ; adjust index
   movlw high FuncPTR
   movwf PCLATH
   movfw curr
   call FuncPTR
   movwf PCL          ; goto protocol code

   ;;;;;;;;;

do_it
    movfw bdata
    subwf ir_cmd_up,W
    btfsc STATUS,Z
    goto goStop

    movfw bdata
    subwf ir_cmd_down,W
    btfss STATUS,Z
    goto done
goStop
    movfw PORTB
    andlw 0xA0      ; if some triac is on, shutdown
    btfsc STATUS,Z
    goto goUp
    call TurnOFF
    goto done

goUp
    movf bdata,W
    subwf ir_cmd_up,W
    btfss STATUS,Z
    goto goDown
    call Cmd_Up
    goto done
goDown
    movf bdata,W
    subwf ir_cmd_down,W
    btfsc STATUS,Z
    call Cmd_Down
done
    call wait_IR_key_depress
    bcf INTCON,INTF
    bsf INTCON,INTE      ; enable RB0 int
    btfsc _IR_CFG
    bcf _IR_CFG
    goto MainLoop



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;_memclr
;    movwf 0x7f
;loop2
;    clrf INDF
;    incf FSR
;    decfsz 0x7f
;    goto loop2
;    return

;---------------------------------------------

;_memcpy

;    bsf STATUS,RP0
;loop1
;    movfw INDF
;    movwf EEDATA
;    call write_eeprom
;    incf EEADR
;    incf FSR
;    decfsz curr
;    goto loop1
;    bcf STATUS,RP0
;    return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


IR_Cfg
 ;  movlw 0xff
 ;  movwf temp
;j1
;   btfsc PORTA,4
;   goto MainLoop
 ;  decfsz temp
 ;  goto j1
   movlw 0xff
   call delay_ms
   btfss PORTA,4
   goto $-1

   bsf _IR_CFG
   movlw 2
   movwf k_id    ; identifier to func btn up/down

 nxt
   bsf T1CON,TMR1ON    ; start blinking led on k_id
loop_cfg
     btfss _IR_FRAME
    goto loop_cfg   ; wait for RB0 INT ISR building IR frame
    ; all done
    bcf T1CON,TMR1ON
    movlw 0xaf    ; efface leds
    andwf PORTB,F

    bcf _IR_DETECT
    bcf _IR_FRAME

    movlw buf
    subwf FSR,W
    movwf bCount
    goto  process
;;;;;;;;;;;;;;;;;;;

;    call validate_NEC_frame
;    addlw 0
;    btfss STATUS,Z
;    goto protocol
;    call validate_RC5_frame
;    addlw 0
;    btfss STATUS,Z
;    goto protocol
;    call validate_RC6_frame
;    addlw 0
;    btfss STATUS,Z
;    goto protocol
;    call validate_SIRC_frame
;    addlw 0
;    btfss STATUS,Z
;    goto protocol
;    call validate_SAMSUNG_frame
;    addlw 0
;    btfss STATUS,Z
;    goto protocol
;    bcf _IR_CFG    ; On error exit
;    goto done


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Carrier freq   / bit period / rep interval
; NEC      38Khz  /   1.125ms  /  110ms      ( Pulse Distance Modulation)
; RC5      36Khz  /   1.778ms  /  100ms      ( Manchester Code ( bi-phase))
; RC6      36Khz  /   895us    /  83ms       ( Manchester Code ( bi-phase))
; CIRC     40Khz  /   1.2ms    /  45ms       ( Pulse Width Modulation)
; Samsung  37.9Khz/   590us    /  100ms      [ Pulse Distance


Key_IR
    bcf INTCON,INTF
    btfsc _IR_DETECT
    goto measure
    bsf _IR_DETECT
    movlw buf
    movwf bIntSaveFSR
    movlw 0xC0
    movwf TMR0
    bcf INTCON,T0IF
    bsf INTCON,T0IE

measure:
     movlw 0xef
     subwf FSR,W
     btfsc STATUS,C
     goto IntReturn  ; return if mem overflow
     movfw TMR0
    andlw 0x3F     ; discard key repetition data
    movwf INDF
    movlw 0xC0     ; max time to look for next edge
    movwf TMR0     ; 63 * 256 = 16.128ms

    incf bIntSaveFSR
    bsf STATUS,RP0
    comf OPTION_REG,W
    andlw 0x40
    bcf OPTION_REG,6
    iorwf OPTION_REG,F   ; Invert trigger edge
    bcf STATUS,RP0

    goto IntReturn




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


TMR0_ISR

     bcf INTCON,T0IF
     bsf _IR_FRAME
     bcf INTCON,T0IE
    bsf STATUS,RP0
    movlw CONFIG_1
    movwf OPTION_REG   ; restore INT edge trigger
    bcf STATUS,RP0
    bcf INTCON,INTE  ; prevent memory corruption with new data  IR buffer
     goto IntReturn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




TMR1_ISR
     bcf PIR1,TMR1IF

     movlw 0x0b
     movwf TMR1H
     movlw 0xdb
     movwf TMR1L

     btfsc _IR_CFG
     goto Blink

     btfss _PROTECT_STAGE
     decf stage_delay

     decfsz T1,F
     goto IntReturn
     movlw 2
     movwf T1
     btfss _SMART_BTN   ; if button depress after 1sec
     bsf _LONG_PRESS     ; set LONG_PRESS
     goto IntReturn


Blink:

     btfsc k_id,0
     goto blkUp
;blkDown
     comf PORTB,W
     andlw 0x10
     bcf PORTB,4
     iorwf PORTB,F
     goto IntReturn
blkUp:
     comf PORTB,W
     andlw 0x40
     bcf PORTB,6
     iorwf PORTB,F
     goto IntReturn


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Setup:

     banksel TRISA
     ;     OPTION_REG
     ;       1-------    disable pullup resistor
     ;       -0------    RBO INT on falling edge
     ;       --0-----    TMR0 clk  RA4 / source internal
     ;       ---x----
     ;       ----0---     prescaler to TMR0
     ;       -----111     prescaler rate 1:256

;    movlw b'10000111'
     movlw CONFIG_1
     movwf OPTION_REG



     ;       PORTA
     ;  RA0  '-------1'   -
     ;  AN1  '------1-'  input- comp  AN_Signal
     ;  AN2  '-----1--'   input+ comp VRef
     ;  RA3  '----x---'
     ;  RA4  '---1----'   btn Cfg
     ;  RA5  '--1-----'
     ;  RA6  '-x------'   Osc2
     ;  RA7  'x-------'   Osc1

;     movlw  b'11111111'
;     movwf TRISA



    ;        PORTB
    ;   RB0  '-------1'   input IR
    ;   RB1  '------1-'
    ;   RB2  '-----1--'   btn down
    ;   RB3  '----1---'   btn Up
    ;   RB4  '---0----'   led Up
    ;   RB5  '--0-----'   Triac Up
    ;   RB6  '-0------'   led Down
    ;   RB7  '0-------'   Triac Down

     movlw  b'00001111'
     movwf TRISB

    bsf PIE1,TMR1IE

    banksel PORTA
     clrf PORTA
     clrf PORTB

     ; Comparator off
     movlw b'00000111'
     movwf CMCON

   ; Setup TMR1  to (0xffff-0x0bdb)*prescaler*T1
   ;                 62500 * 8 * 2 = 1000000us

     movlw 0x0b
     movwf TMR1H
     movlw 0xdb
     movwf TMR1L
     movlw .2
     movwf T1


   ;        xx11----  TMR1  prescaler 1:8
   ;        xx--0---  osc off
   ;        xx---1--  no sync extern clock
   ;        xx----0-  internal osc (Fosc/4)

    movlw b'00110100'
    movwf T1CON

    bcf PIR1,TMR1IF
    bcf PIR1,CMIF

     bsf INTCON,PEIE

;     bsf INTCON,GIE

     movlw 0x03
     movwf PCON

     clrf buf
     return





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




delay_ms             ; W*1ms  at 4MHz
    movwf delay_mult
del_1ms              ;
    nop
    movlw  d'199'
    movwf delay_k200
del_5us
    nop
    nop
    decfsz delay_k200,f
    goto del_5us
    decfsz delay_mult,f
    goto del_1ms
    return



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,,





read_eeprom
    bsf EECON1, RD       ; EE begin read
    btfsc EECON1,RD
    goto $-1
    movf EEDATA, w       ; EEDATA -> w
    return


;*******************************************************************************



write_eeprom

     bsf EECON1, WREN
     movlw 0x55
     movwf EECON2
     movlw 0xAA
     movwf EECON2
     bsf EECON1,WR
     nop
     btfsc EECON1,WR
     goto $-1
     return


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;













     org 0x200


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FuncPTR
    addwf PCL,F
    retlw NEC     ; 1
    retlw RC5     ; 2
    retlw RC6     ; 3
    retlw SIRC    ; 4
    retlw SAMSUNG ; 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Nec, Toshiba

NEC
   ; this is a NEC frame data
    btfss _IR_CFG
    goto nec_exec

    movlw 8          ; 8 bits frame data
    movwf byte_sz
    clrf bdata
    call NEC_extract_byte   ; read command

    movfw bdata
    btfss k_id,0
    movwf ir_cmd_up
    btfsc k_id,0
    movwf ir_cmd_down

   call wait_IR_key_depress

    bcf  INTCON,INTF
    bsf INTCON,INTE  ; enable RB0 INT

    decfsz k_id
    goto nxt
    goto backup

nec_exec
    comf bdata,W
     subwf addr,W
    btfss STATUS,Z   ; if not for our device abort
    goto done
    movlw 8          ; 8 bits frame data
    movwf byte_sz
    clrf bdata
    call NEC_extract_byte

    goto do_it


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Philips, Grundig

RC5
   ; this is a RC5 frame data
    movfw bdata
    movwf temp
    andlw 0x01     ; preserv last bit status
    movwf bdata
    movfw bCount   ; download number of edges
    movlw 6             ; 6 bits frame data
    movwf byte_sz
    call RC5_extract_byte  ; read command data
    btfss _IR_CFG
    goto rc5_exec

    movfw bdata
    btfss k_id,0
    movwf ir_cmd_up
    btfsc k_id,0
    movwf ir_cmd_down
    call wait_IR_key_depress
    bcf  INTCON,INTF
    bsf INTCON,INTE  ; enable RB0 INT
    decfsz k_id
    goto nxt
    goto backup
rc5_exec
    movfw temp
    andlw 0x1f    ; discard toggle bit from address
    subwf addr,W
    btfss STATUS,Z
    goto done      ; if not for our device abort
    goto do_it      ; doit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Philips

RC6
    ; this is a RC6 (Philips) frame data
    movfw bdata    ; hold address data
    movwf temp
    andlw 0x01     ; preserv last bit status
    movwf bdata
    movlw 8
    movwf byte_sz
    call RC6_extract_byte  ; read command data
    btfss _IR_CFG
    goto rc6_exec
    movfw bdata
    btfss k_id,0
    movwf ir_cmd_up
    btfsc k_id,0
    movwf ir_cmd_down
    call wait_IR_key_depress
    bcf  INTCON,INTF
    bsf INTCON,INTE  ; enable RB0 INT
    decfsz k_id
    goto nxt
    goto  backup
rc6_exec
    movfw temp
    subwf addr,W
    btfss STATUS,Z
    goto done      ; if not for our device abort
    goto do_it      ; doit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Sony

SIRC
    ; this is a SIRC frame data
    movfw bdata
    movwf temp
    movlw buf
    addlw 2
    movwf FSR
    movlw 7
    movwf byte_sz
    clrf bdata
    call SIRC_extract_byte
    btfss _IR_CFG
    goto sirc_exec
    movfw bdata
    btfss k_id,0
    movwf ir_cmd_up
    btfsc k_id,0
    movwf ir_cmd_down
    call wait_IR_key_depress
    bcf  INTCON,INTF
    bsf INTCON,INTE  ; enable RB0 INT
    decfsz k_id
    goto nxt
    goto  backup
sirc_exec
    movfw temp
    subwf addr,W
    btfss STATUS,Z
    goto done      ; if not for our device abort
    goto do_it      ; doit





SAMSUNG
    ; this is a Samsung frame data

    movfw bdata
    movwf temp
    movlw .16
    addwf FSR
    movlw 8
    movwf byte_sz
    clrf bdata
    call SAMSUNG_extract_byte
    comf bdata
    btfss _IR_CFG
    goto sirc_exec
    movfw bdata
    btfss k_id,0
    movwf ir_cmd_up
    btfsc k_id,0
    movwf ir_cmd_down
    call wait_IR_key_depress
    bcf  INTCON,INTF
    bsf INTCON,INTE  ; enable RB0 INT
    decfsz k_id
    goto nxt
    goto  backup

samsung_exec

       movfw temp
    subwf addr,W
    btfss STATUS,Z
    goto done      ; if not for our device abort
    goto do_it      ; doit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       header, addr, and command data
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;
;      |----header---|
;       _________
;     _|         |____
;          9ms    4.5ms
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;
;         110ms
;    |----repeat-----|
;    |-frame-|
;     _               _
;    | |||||||_______| ||________
;



    ; teste PDC (NEC) header
validate_NEC_frame
    movlw buf+1
    movwf FSR
    movfw INDF
    sublw 0x23      ; +- 8.960ms  (9ms)
    btfss STATUS,Z
    retlw 0
    incf FSR
    movfw INDF
    sublw 0x11       ; +- 4.352ms  (4.5ms)
    btfss STATUS,Z
    retlw 0       ;  not NEC code
    movlw 8
    movwf byte_sz
    incf FSR
    clrf bdata
    call NEC_extract_byte   ; read addr1
    movfw bdata
    btfsc _IR_CFG
    movwf addr
    movlw 8
    movwf byte_sz
    clrf bdata
    call NEC_extract_byte    ; read addr2
    comf bdata,W    ; inverted addr
    subwf addr,W
    btfsc STATUS,Z
    retlw 1        ; return function addr index
    retlw 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Grundig
;     __    __
;   _|  |__|  |
;   889 889 889us


validate_RC5_frame
    movlw buf+1
    movwf FSR
    movfw INDF
    incf FSR
    subwf INDF,W
    btfss STATUS,Z
    retlw 0
    movfw INDF
    sublw 3            ;  (889us)
     btfss STATUS,Z
     retlw 0        ;  not RC5 frame
    incf FSR
    clrf sync
    movlw 3
    movwf Tbit
    movlw 6             ; 6 bits frame data
    movwf byte_sz
    clrf bdata
    bsf bdata,0
    call RC5_extract_byte  ; read address
    btfss _IR_CFG
    retlw 2
    movfw bdata
    andlw 0x1f     ; discard toggle bit
    movwf addr
    retlw 2



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;   Philips
;
;                    1bit| 3 bits |   1 bit |  8 bits   |  8 bits   |
;   |------Leader----|---|--mode--|--toggle-|--address--|--command--|
;    ___________      _      _   _      ____
;  _|           |____| |____| |_| |____|    |||||||||||||||||||||||||
;      2.66ms    889us
;
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;
;    |-----Leader----|---data---|-free time-|
;     ___________
;   _|           |___||||||||||||____________
;

;




validate_RC6_frame
    movlw buf+1
    movwf FSR
    movfw INDF
    sublw 0x0A
    btfss STATUS,Z
    retlw 0
    incf FSR
    movfw INDF
    sublw 0x03       ;
    btfss STATUS,Z
    retlw 0
    incf FSR
    clrf sync
    movlw 1
    movwf bdata
    movlw 5
    movwf byte_sz
    movlw 1
    movwf Tbit
    call RC6_extract_byte  ; read mode
    movfw bdata
    andlw 0x06    ; discard bit 1 + toggle bit
    btfss STATUS,Z  ; teste mode == 0
    retlw 0
    btfsc bdata,0
    goto adj    ; because special case
    decf FSR    ; of toggle bit ( Toggle bit = 2*Tbit)
    decf sync   ; we need to adjust pointers
adj
    movlw 8
    movwf byte_sz
    movlw 1
    andwf bdata,F
    call RC6_extract_byte  ; read address
    btfss _IR_CFG
    retlw 3
    movfw bdata
    movwf addr      ; update device address
    retlw 3





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;   Sony
;
;   |------Leader----|- 7 bit Command-|-5 bits address-| (12 bits)
;    ___________
;  _|           |____|
;      2.4ms     600us
;
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,


validate_SIRC_frame
    ; calculate frame size bits 12-15-20
    movlw .16
    subwf bCount,W
    movwf byte_sz
    bcf STATUS,C
    rrf byte_sz,F

    movlw buf+1
    movwf FSR
    movfw INDF
    sublw 0x09
    btfss STATUS,Z
    retlw 0
    incf FSR
    movfw INDF
    sublw 0x02       ;
    btfss STATUS,Z
    retlw 0
    movlw .14
    addwf FSR     ; point to address LSB
    clrf bdata
    clrf curr
    call SIRC_extract_byte  ; read address
    btfss _IR_CFG
    retlw 4
    movfw bdata
    movwf addr      ; update device address
    retlw 4


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Samsung
;     __
;   _|  |__   32 bits data + addr
;    4.5 4.5ms



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


validate_SAMSUNG_frame
   movlw buf+1
   movwf FSR
   movfw INDF
   sublw 0x11
   btfss STATUS,Z
   retlw 0
   incf FSR
    movfw INDF
   sublw 0x11
   btfss STATUS,Z
   retlw 0
   incf FSR
   incf FSR
   clrf bdata
   movlw 8
   movwf byte_sz
   movlw 2
   movwf Tbit
   call SAMSUNG_extract_byte
   comf bdata
    btfss _IR_CFG
    retlw 5
    movfw bdata
    movwf addr      ; update device address
    retlw 5





NEC_extract_byte
    movfw INDF
    incf FSR
    subwf INDF,W
    rrf bdata,1
    btfsc STATUS,Z
    bcf bdata,7
    incf FSR
    decfsz byte_sz
    goto NEC_extract_byte

    return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


RC5_extract_byte
    movfw INDF
    subwf Tbit,W
    btfsc STATUS,C
    goto rc5_data_ready
    ; invert signal
    comf bdata,W
    andlw 0x01
    bcf bdata,0
    iorwf bdata,F
    incf sync
rc5_data_ready   ; data is ready
    incf FSR
    incf sync
    btfss sync,0
    decfsz byte_sz
    goto rc5_shift
    return
rc5_shift
    btfsc sync,0
    goto rc5_loop  ; not middle byte jump to next edge
    bcf STATUS,C
    rlf bdata
    btfsc bdata,1
    bsf bdata,0    ; preserve last bit value
rc5_loop
    goto RC5_extract_byte


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




RC6_extract_byte
    movfw INDF
    subwf Tbit,W
    btfsc STATUS,C
    goto data_ready
    ; invert signal
    comf bdata,W
    andlw 0x01
    bcf bdata,0
    iorwf bdata,F
    incf sync
data_ready   ; data is ready
    incf FSR
    incf sync
    btfsc sync,0
    decfsz byte_sz
    goto shift
    return
shift
    btfss sync,0
    goto jmp  ; not middle byte jump to next edge
    bcf STATUS,C
    rlf bdata
    btfsc bdata,1
    bsf bdata,0    ; preserve last bit value
jmp
    goto RC6_extract_byte




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


SIRC_extract_byte
    movfw byte_sz
    sublw 8
    movwf curr

    incf FSR
    movfw INDF
    sublw 2
    btfss STATUS,Z
    bsf bdata,7
    incf FSR
    decfsz byte_sz
    goto pb
    bcf STATUS,C
    rrf bdata,F
    decfsz curr
    goto $-2
    return
pb
    bcf STATUS,C
    rrf bdata
    goto SIRC_extract_byte+3



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


SAMSUNG_extract_byte
   movfw INDF
   subwf Tbit,W
   rrf bdata
   decfsz byte_sz
   goto array
   return
array
   incf FSR
   incf FSR
   goto SAMSUNG_extract_byte


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PULSE_DISTANCE_TO_BIN
    movfw Tbit
    subwf INDF,W
    return


PULSE_WIDTH_TO_BIN
    movfw Tbit
    subwf INDF,W
    return

MANCHESTER_TO_BIN
    return



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

backup
    bcf _IR_CFG
    bcf INTCON,GIE
    ;   save CFG IR frame to eeprom
;   movfw bCount
;    movwf curr
;    movlw buf
;    movwf FSR
;    movlw 0x48
;    bsf STATUS,RP0
;    movwf EEADR
;    call _memcpy

   ;   save address and key commands
   movlw 3
    movwf curr
    movlw addr
    movwf FSR
    bsf STATUS,RP0
    movlw 0x7D
    movwf EEADR
rec
    movfw INDF
    movwf EEDATA
    call write_eeprom    ; save IR addr and key commands
    incf EEADR
    incf FSR
    decfsz curr
    goto rec

    bcf STATUS,RP0
    bsf INTCON,GIE

    goto MainLoop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


      end
