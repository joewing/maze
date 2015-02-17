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
INIT    EQU     *
*
* ZERO OUT THE MAZE ARRAY
*
        L       3,WIDTH
        M       2,HEIGHT            TOTAL SIZE IN R3
        L       2,=X'00000001'      CHARACTER IN R2
INITLP1 STC     2,MAZE-1(3)
        S       3,=F'1'
        BNZ     INITLP1
*
* LEFT AND RIGHT BORDERS
*
        L       3,WIDTH
        M       2,HEIGHT            TOTAL SIZE IN R3
        XR      2,2                 CHARACTER IN R2
INITLP2 STC     2,MAZE-1(3)         RIGHT BORDER
        S       3,WIDTH
        STC     2,MAZE(3)           LEFT BORDER
        BNZ     INITLP2
*
* TOP AND BOTTOM BORDERS
*
        L       5,WIDTH
        M       4,HEIGHT
        L       3,WIDTH
INITLP3 STC     2,MAZE-1(3)
        STC     2,MAZE-1(5)
        S       5,=F'1'
        S       3,=F'1'
        BNZ     INITLP3
*
* CARVE THE MAZE
*
        XR      2,2
        SL      2,WIDTH
        ST      2,OFFSETS+2*4   NEGATIVE Y-OFFSET
        L       2,WIDTH
        ST      2,OFFSETS+3*4   POSITIVE Y-OFFSET
        A       2,WIDTH
        A       2,=F'2'         R2=WIDTH*2+2
        XR      11,11
        BAL     14,CARVE
        L       2,=F'2'
        A       2,WIDTH
        STC     11,MAZE(2)      CARVE ENTRY
        L       3,WIDTH
        M       2,HEIGHT
        S       3,WIDTH
        S       3,=F'3'
        STC     11,MAZE(3)      CARVE EXIT
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
DISPLP2 IC      6,MAZE(2)
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
* DONE
*
        L       13,MAZESAV+4
        XR      0,0
        RETURN  (14,12)
*
* CARVE STARTING AT R2
* BUFFER FOR RECURSION IN R11
* RETURN ADDRESS IN R14
*
CARVE   EQU     *
        XR      7,7
        STC     7,MAZE(2)
        L       5,RAND              GET THE NEXT RANDOM NUMBER
        M       4,=F'1664525'
        AL      5,=F'1013904223'
        ST      5,RAND
        L       3,=F'4'             COUNT IN 3
CARVELP N       5,=X'0000000C'      DIRECTION OFFSET IN 5
        L       4,OFFSETS(5)        BYTE OFFSET IN 4
        XR      6,6
        ALR     6,4
        ALR     6,2                 FIRST POSITION IN 6
        ALR     4,6                 SECOND POSITION IN 5
        XR      7,7
        IC      7,MAZE(6)           FIRST VALUE IN 7
        XR      8,8
        IC      8,MAZE(4)           SECOND VALUE IN 8
        NR      7,8                 CHECK IF CARVED
        BZ      CARVEN
        XR      7,7                 CARVE
        STC     7,MAZE(6)
        STC     7,MAZE(4)
        XR      2,2
        ALR     2,4                 MOVE TO THE NEXT POSITION
        ST      2,CARVBUF+0(11)     SAVE POSITION
        ST      3,CARVBUF+4(11)     SAVE COUNT
        ST      5,CARVBUF+8(11)     SAVE DIRECTION
        ST      14,CARVBUF+12(11)   SAVE RETURN ADDRESS
        AL      11,=F'16'           UPDATE BUFFER LOCATION
        BAL     14,CARVE        
        S       11,=F'16'           RETURN BUFFER LOCATION
        L       2,CARVBUF+0(11)     LOAD POSITION
        L       3,CARVBUF+4(11)     LOAD COUNT
        L       5,CARVBUF+8(11)     LOAD DIRECTION
        L       14,CARVBUF+12(11)   LOAD RETURN ADDRESS
        B       CARVE
CARVEN  A       5,=X'00000004'      NEXT DIRECTION
        S       3,=F'1'             UPDATE COUNT
        BNZ     CARVELP
        BR      14                  RETURN
*
MAZEOUT DCB     DSORG=PS,LRECL=80,MACRF=PM,DDNAME=SYSOUT
*
WIDTH   DC      F'33'               MAZE WIDTH, MUST BE ODD
HEIGHT  DC      F'23'               MAZE HEIGHT, MUST BE ODD
MAZESAV DS      18F
RAND    DC      1F'38587391'
OFFSETS DC      F'1'
        DC      F'-1'
        DC      F'0'
        DC      F'0'
CARVBUF DS      (4*100)F            LOCATION,DIR,COUNT,RETURN
MAZE    DS      (39*23)B
OUTBUF  DC      CL80' '
        END
/*
//GO.SYSOUT DD SYSOUT=*
//
