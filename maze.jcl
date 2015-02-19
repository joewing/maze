//MAZEJOB JOB 1
//*
//* MAZE GENERATOR IN IBM SYSTEM/360 ASSEMBLY LANGUAGE
//* JOE WINGBERMUEHLE 2015-02-16
//*
//MAZEGEN EXEC ASMFCLG
//ASM.SYSGO DD UNIT=SYSDA
//ASM.SYSIN DD *
MAZEGEN START   0
        SAVE    (14,12)
        BALR    12,0                SET BASE OFFSET
        USING   *,12
        ST      13,MAZESAV+4        SAVE PREVIOUS SAVE AREA
        LA      13,MAZESAV          SET NEW SAVE AREA
        B       INIT                BRANCH TO THE START
        DC      CL8'MAZEGEN'        PROGRAM NAME
        DC      CL8'&SYSDATE'       DATE ASSEMBLED
        DC      CL8'&SYSTIME'       TIME ASSEMBLED
WIDTH   DC      F'39'               MAZE WIDTH, MUST BE ODD
HEIGHT  DC      F'23'               MAZE HEIGHT, MUST BE ODD
INIT    EQU     *
*
* ALLOCATE MEMORY FOR THE MAZE ARRAY AND CARVE BUFFER
* NEED 4*4*(WIDTH*HEIGHT)/4 = 4*WIDTH*HEIGHT BYTES FOR BUFFER
* NEED WIDTH*HEIGHT BYTES FOR ARRAY
* TOTAL OF 5*WIDTH*HEIGHT BYTES
*
        L       11,WIDTH
        M       10,HEIGHT           ARRAY SIZE IN R11
        AR      10,11               R10 = R11 = ARRAY SIZE
        SLA     11,2                BUFFER SIZE IN R11
        AR      10,11               TOTAL BYTES TO ALLOCATE IN R10
        ST      10,MPLIST
        LA      1,MPLIST            PARAM LIST IN R1
        SVC     4                   GETMAIN
        LTR     15,15
        BNZ     EXIT                FAILURE IF R15 != 0
        L       10,MEMPTR@          ADDRESS OF BUFFER IN R10
        AR      11,10               ADDRESS OF ARRAY IN R11
*
* ZERO OUT THE MAZE ARRAY
*
        L       3,WIDTH
        M       2,HEIGHT            TOTAL SIZE IN R3
        L       2,=X'00000001'      CHARACTER IN R2
INITLP1 S       3,=F'1'
        STC     2,0(3,11)
        BNZ     INITLP1
*
* LEFT AND RIGHT BORDERS
*
        L       3,WIDTH
        M       2,HEIGHT            TOTAL SIZE IN R3
        XR      2,2                 CHARACTER IN R2
INITLP2 S       3,=F'1'
        STC     2,0(3,11)           RIGHT BORDER
        A       3,=F'1'
        S       3,WIDTH
        STC     2,0(3,11)           LEFT BORDER
        BNZ     INITLP2
*
* TOP AND BOTTOM BORDERS
*
        L       5,WIDTH
        M       4,HEIGHT
        L       3,WIDTH
INITLP3 S       5,=F'1'
        S       3,=F'1'
        STC     2,0(3,11)
        STC     2,0(5,11)
        BNZ     INITLP3
*
* CARVE THE MAZE
*
        XR      4,4
        SL      4,WIDTH
        ST      4,OFFSETS+2*4   NEGATIVE Y-OFFSET
        L       4,WIDTH
        ST      4,OFFSETS+3*4   POSITIVE Y-OFFSET
        A       4,WIDTH
        A       4,=F'2'         R4=WIDTH*2+2
        BAL     5,CARVE
        L       2,=F'2'
        A       2,WIDTH
        XR      4,4
        STC     4,0(2,11)       CARVE ENTRY
        L       3,WIDTH
        M       2,HEIGHT
        S       3,WIDTH
        S       3,=F'3'
        STC     4,0(3,11)       CARVE EXIT
*
* DISPLAY THE MAZE
*
DISP    EQU     *
        OPEN    (MAZEOUT,OUTPUT)
        PUT     MAZEOUT,=CL80'MAZEGEN BY JOE WINGBERMUEHLE'
        XR      2,2                 OFFSET IN MAZE ARRAY
        L       3,HEIGHT            Y-COORDINATE
DISPLP1 L       4,WIDTH             X-COORDINATE
        XR      5,5                 OFFSET IN OUTPUT BUFFER
DISPLP2 IC      6,0(2,11)
        N       6,=X'00000001'
        BZ      DISPSP
        LH      7,=C'[]'
        B       DISPNXT
DISPSP  LH      7,=C'  '
DISPNXT STH     7,OUTBUF(5)
        A       2,=F'1'             NEXT POSITION IN MAZE
        A       5,=F'2'             NEXT POSITION IN OUTPUT BUFFER
        S       4,=F'1'             NEXT X-COORDINATE
        BNZ     DISPLP2
        PUT     MAZEOUT,OUTBUF
        S       3,=F'1'             NEXT Y-COORDINATE
        BNZ     DISPLP1
        CLOSE   (MAZEOUT)
*
* FREE MEMORY USED BY THE MAZE ARRAY
*
        LA      1,MPLIST            PARAM LIST
        SVC     5                   FREEMAIN
*
* EXIT
*
EXIT    EQU     *
        L       13,MAZESAV+4
        XR      0,0
        RETURN  (14,12)
*
* CARVE STARTING AT R4
* RETURN ADDRESS IN R5
* BUFFER FOR RECURSION IN R11
*
CARVE   EQU     *
        XR      1,1                 R1 IS ZERO
        STC     1,0(4,11)
        L       3,RAND              GET THE NEXT RANDOM NUMBER
        M       2,=F'1664525'
        AL      3,=F'1013904223'
        ST      3,RAND
        L       2,=F'4'             COUNT IN 2
CARVELP N       3,=X'0000000C'      DIRECTION OFFSET IN 3
        L       9,OFFSETS(3)        BYTE OFFSET IN 9
        XR      6,6
        ALR     6,9
        ALR     6,4                 FIRST POSITION IN 6
        ALR     9,6                 SECOND POSITION IN 9
        XR      7,7
        IC      7,0(6,11)           FIRST VALUE IN 7
        XR      8,8
        IC      8,0(9,11)           SECOND VALUE IN 8
        NR      7,8                 CHECK IF CARVED
        BZ      CARVEN
        STC     1,0(6,11)           CARVE
        STC     1,0(9,11)
        XR      4,4
        ALR     4,9                 MOVE TO THE NEXT POSITION
        STM     2,5,0(10)           SAVE CONTEXT
        AL      10,=F'16'           UPDATE BUFFER LOCATION
        BAL     5,CARVE
        S       10,=F'16'           RETURN BUFFER LOCATION
        LM      2,5,0(10)           RESTORE CONTEXT
        B       CARVE
CARVEN  A       3,=X'00000004'      NEXT DIRECTION
        S       2,=F'1'             UPDATE COUNT
        BNZ     CARVELP
        BR      5                   RETURN
*
MAZEOUT DCB     DSORG=PS,LRECL=80,MACRF=PM,DDNAME=SYSOUT
*
MAZESAV DS      18F
RAND    DC      1F'38587391'
OFFSETS DC      F'1'
        DC      F'-1'
        DC      F'0'
        DC      F'0'
MEMPTR@ DS      A
MPLIST  EQU     *
        DS      A                   BYTES TO ALLOCATE/FREE
        DC      A(MEMPTR@)          ADDRESS OF ALLOCATION
        DC      X'0000'             REQUEST TYPE
OUTBUF  DC      CL80' '
        END
/*
//GO.SYSOUT DD SYSOUT=*
//
