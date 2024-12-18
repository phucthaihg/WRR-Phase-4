000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.  CNP02B.                                             00020000
000300******************************************************************00030000
000400*  THIS PROGRAM PERFORMS THE REGULAR ASSIGNED JOBS INQUIRY       *00040000
000500***************************************************************** 00050000
000600*  DATE   INITIAL  LOG#   DESCRIPTION                             00060000
000700*-------- ------- ------  --------------------------------------- 00070000
000800*12/03/90   ERW           CICS CONVERSION.                        00080000
000900*08/05/96   RCW  CNC0090  ADD REST TO SCREEN.                     00090000
001000*                CNC0089  DON'T DISPLAY JOB OWNER IF HE IS ON A   00100000
001100*                         TEMP ASSIGNMENT.                        00110000
001200*                CNC0021/22 DISPLAY RETURN DATE FROM VACATION OR  00120000
001300*                         OFF MILES.                              00130000
001400*04/03/98   AMF           ADDED HELP LOGIC.                       00140000
001500*05/06/98   LPS           Y2K MODIFICATIONS FOR THE TASK LIST.    00150000
001600*07/28/98   AMF           YEAR 2000 SWEEP.                        00160000
001700*09/21/98   RDH  C442     ENLARGE SELECTION TO 3 DIGITS.          00170000
001800*01/20/99   NJB           YEAR 2000 CLEAN-UP                      00180000
001900*12/11/99   MOO  CNC0183  ADD CHECK FOR MTOY & MTOR               00190000
002000*05/03/00   AJK  CNC0183  REMOVE CHECK FOR MTOY & MTOR.           00200000
002100*07/03/00   PLS  CNC0311  INCLUDE 2-HOUR LEAD TIME IN PERSONAL    00210000
002200*                         CALCULATION FOR LOCALS.                 00220000
002300*10/05/04   KJS  CNC0386A ADD DAY ASSOCIATED WITH PERSONAL REST   00230000
002400*02/04/08   AJK           DON'T LINK TO CNP903 IF PERS-REST-TIME  00240000
002500*                         IS ZEROES (P4130).  WE GET AN ASRA IF   00250000
002600*                         WE DO.                                  00260000
002700* 02/22/08  RXJ  CNC0454  FOR CANADIAN EMPLOYEES, LEAD TIME WILL  00270000
002800*                         NOT BE APPLIED TO THE REST EXPIRE TIME  00280000
002900*08/07/08   SXO  C780     CORRECTED THE DISPLAY OF REST TIMES -   00290000
003000*                         EMPLOYEE US REST, PERSONAL REST         00300000
003100*                         AND MANDATORY REST TIMES (WHICHEVER IS  00310000
003200*                         GREATER AMONG THEM).                    00320000
003300*09/23/08   AJK  C780     PER CN'S REQUEST, WE NOW APPLY LEAD     00330000
003400*                         TIME TO YARD EMPLOYEES' PERSONAL REST   00340000
003500*                         AVAILIBILITY TIMES, NOT JUST LOCALS.    00350000
003600*02/10/09   JES  CNC0436B RECOMPILE ONLY FOR WSTASK EXPANSION.    00360002
003700*04/27/10   AXK  CNC0492  ALWAYS CHECK FOR A DUEBACK/BOOK-ON TASK 00370005
003800*                         AND IF EXISTS, DISPLAY RETURN DATE/TIME.00380005
003900*                         PERFORMING P1520-GET-DUEBACK-DATE.      00390008
004000*                         THIS IS ONLY FOR OTHER LAYOFF CODES.    00400006
004010*08/13/13   AXK  CNC0531   == RECOMPILE ONLY ==                   00401013
004020*                         ADDED YARD CRAFTS Y0-Y9 AND YT          00402013
004100***************************************************************** 00410000
004200 ENVIRONMENT DIVISION.                                            00420000
004300 CONFIGURATION SECTION.                                           00430000
004400 SOURCE-COMPUTER.  IBM-9370.                                      00440000
004500 OBJECT-COMPUTER.  IBM-9370.                                      00450000
004600 DATA DIVISION.                                                   00460000
004700 WORKING-STORAGE SECTION.                                         00470000
004800 01  FILLER                  PIC X(10)  VALUE 'PGM02B W/S'.       00480000
004900*01  WS-ABSTIME              PIC S9(15) COMP-3 VALUE +0.          00490000
005000                                                                  00500000
005100 01  WS-SUBSCRIPTS.                                               00510000
005200     02  I                   PIC S9(4)  COMP VALUE +0.            00520000
005300     02  J                   PIC S9(4)  COMP VALUE +0.            00530000
005400     02  REST-SUB            PIC S9(4)  COMP VALUE +0.            00540000
005500     02  STRING-POINTER      PIC S9(4)  COMP VALUE +0.            00550000
005600     02  SET-NUM             PIC S9(4)  COMP VALUE +0.            00560000
005700     02  ROW-NUM             PIC S9(4)  COMP VALUE +0.            00570000
005800                                                                  00580000
005900 01  WS-FLAGS.                                                    00590000
006000     02  SCREEN-FLAG         PIC X      VALUE '0'.                00600000
006100         88  CONTINUE-SCREEN            VALUE '0'.                00610000
006200         88  CREATE-SCREEN              VALUE '1'.                00620000
006300         88  SEND-BUFFER                VALUE '2'.                00630000
006400     02  DONE-CODE           PIC  X     VALUE 'N'.                00640000
006500         88  DONE                       VALUE 'Y'.                00650000
006600         88  NOT-DONE                   VALUE 'N'.                00660000
006700     02  SCH-DONE-CODE       PIC  X     VALUE 'N'.                00670000
006800         88  SCH-DONE                   VALUE 'Y'.                00680000
006900         88  SCH-NOT-DONE               VALUE 'N'.                00690000
007000     02  JOB-QUAL-CODE       PIC  X     VALUE 'N'.                00700000
007100         88  JOB-QUALIFIES              VALUE 'Y'.                00710000
007200         88  JOB-NOT-QUALIFIED          VALUE 'N'.                00720000
007300     02  SHIFTS-DONE-CODE    PIC  X     VALUE 'N'.                00730000
007400         88  SHIFTS-DONE                VALUE 'Y'.                00740000
007500         88  SHIFTS-NOT-DONE            VALUE 'N'.                00750000
007600     02  WS-TASK-DONE-CODE   PIC  X     VALUE 'N'.                00760000
007700         88  TASK-NOT-DONE              VALUE 'N'.                00770000
007800         88  TASK-DONE                  VALUE 'Y'.                00780000
007900     02  WS-APPLY-LEAD-TIME-FLAG     PIC X(001) VALUE 'Y'.        00790000
008000         88  APPLY-LEAD-TIME                    VALUE 'Y'.        00800000
008100         88  DONT-APPLY-LEAD-TIME               VALUE 'N'.        00810000
008200     02  WS-COMPANY-CODE-FLAG        PIC X(001) VALUE ' '.        00820000
008300         88  WS-CANADIAN-COMPANY                VALUE 'C'.        00830000
008400         88  WS-US-COMPANY                      VALUE 'U'.        00840000
008500     02  WS-DUEBACK-FOUND-FLAG       PIC X(001) VALUE '0'.        00850008
008600         88  WS-DUEBACK-FOUND-N                 VALUE '0'.        00860008
008700         88  WS-DUEBACK-FOUND-Y                 VALUE '1'.        00870008
008800                                                                  00880000
008900 01  WS-AJ-EFF-DATE-CENT.                                         00890000
009000     05 WS-AJ-EFF-CE        PIC X(002).                           00900000
009100     05 WS-AJ-EFF-DATE.                                           00910000
009200        10 WS-AJ-EFF-YR     PIC X(002).                           00920000
009300        10 WS-AJ-EFF-MO     PIC X(002).                           00930000
009400        10 WS-AJ-EFF-DY     PIC X(002).                           00940000
009500                                                                  00950000
009600 01  WS-MISCELLANEOUS.                                            00960000
009700     05 WS-SHIFT              PIC X(001) VALUE SPACE.             00970000
009800                                                                  00980000
009900     05 WS-FORMAT-HRMN.                                           00990000
010000        10  WS-FORMAT-HR      PIC 9(002).                         01000000
010100        10  WS-FORMAT-MN      PIC 9(002).                         01010000
010200                                                                  01020000
010300     05 DISPLAY-TIME.                                             01030000
010400        10  DISP-HOUR         PIC 99.                             01040000
010500        10  FILLER            PIC X   VALUE ':'.                  01050000
010600        10  DISP-MIN          PIC 99.                             01060000
010700                                                                  01070000
010800*    05 WS-DATE-TIME.                                             01080000
010900*       10  WS-DATE.                                              01090000
011000*           15  WS-YR         PIC 99.                             01100000
011100*           15  WS-MO         PIC 99.                             01110000
011200*           15  WS-DY         PIC 99.                             01120000
011300*       10  WS-TIME.                                              01130000
011400*           15  WS-HRMN.                                          01140000
011500*               20  WS-HR     PIC 99.                             01150000
011600*               20  WS-MN     PIC 99.                             01160000
011700*           15  FILLER        PIC 9(4).                           01170000
011800     05 WS-DAY                PIC 9(1).                           01180000
011900     05 WS-WORK-DATE-TIME     PIC 9(10).                          01190000
012000     05 WS-WORK-DATE-TIME-C REDEFINES WS-WORK-DATE-TIME PIC X(10).01200000
012100                                                                  01210000
012200******************************************************************01220000
012300***                  TEMPORARY STORAGE QUEUE                   ***01230000
012400******************************************************************01240000
012500 01  P02BTSQ-QUEUE-ITEM         PIC S9(4)  COMP VALUE +1.         01250000
012600 01  P02BTSQ-MAP-QUEUE-ID.                                        01260000
012700     05  P02BTSQ-MAP-QUEUE      PIC X(4)   VALUE '02BM'.          01270000
012800     05  P02BTSQ-MAP-TERM-ID    PIC X(4)   VALUE SPACES.          01280000
012900 01  P02BTSQ-CA-QUEUE-ID.                                         01290000
013000     05  P02BTSQ-CA-QUEUE       PIC X(4)   VALUE '02BC'.          01300000
013100     05  P02BTSQ-CA-TERM-ID     PIC X(4)   VALUE SPACES.          01310000
013200 01  P02BTSQ-QLGTH              PIC S9(4)  COMP VALUE +1.         01320000
013300***************************************************************** 01330000
013400***                 I/O STATUS CHECK FIELDS                       01340000
013500***************************************************************** 01350000
013600 01  WS-RESPONSE              PIC S9(8) COMP VALUE ZEROES.        01360000
013700 01  FILE-STATUS              PIC 9(4)  VALUE ZEROES.             01370000
013800     COPY IOCODES.                                                01380000
013900***************************************************************** 01390000
014000***                      COMMAREA COPYBOOKS                       01400000
014100***************************************************************** 01410000
014200     COPY PSTCOMM.                                                01420000
014300     COPY P998COMM.                                               01430000
014400     COPY FICOMM.                                                 01440000
014500***************************************************************** 01450000
014600***                     MAP AREA COPYBOOK                         01460000
014700***************************************************************** 01470000
014800     COPY PSM02BRE.                                               01480000
014900***************************************************************** 01490000
015000***                   PROGRAM NAMES COPYBOOKS                     01500000
015100***************************************************************** 01510000
015200     COPY PSTCB02.                                                01520000
015300     COPY PSTCB02B.                                               01530000
015400     COPY PSTCB998.                                               01540000
015500***************************************************************** 01550000
015600***                 CALLED ROUTINES COPYBOOKS.                    01560000
015700***************************************************************** 01570000
015800     COPY PSTERAR.                                                01580000
015900     COPY P901COMM.                                               01590000
016000     COPY P903COMM.                                               01600000
016100***************************************************************** 01610000
016200***                     FILE COPYBOOKS                            01620000
016300***************************************************************** 01630000
016400     COPY WSMSTR.                                                 01640000
016500     COPY WSASGN.                                                 01650000
016600     COPY WSSWASGN.                                               01660000
016700     COPY WSFICT.                                                 01670000
016800     COPY WSJS.                                                   01680000
016900     COPY WSAJ.                                                   01690000
017000     COPY WSTRCN.                                                 01700000
017100     COPY WSCNTL.                                                 01710000
017200     COPY WSTASK.                                                 01720000
017300***************************************************************** 01730000
017400***                     MISC. COPYBOOKS                           01740000
017500***************************************************************** 01750000
017600     COPY PSTKEYS.                                                01760000
017700     COPY PSTATTR.                                                01770000
017800     COPY WSCENTER.                                               01780000
017900     COPY WSMSG.                                                  01790000
018000     COPY WSDAYWK.                                                01800000
018100     COPY WSAJDEFN.                                               01810000
018200     COPY PSTCCRFT.                                               01820000
018300     COPY WSZONE.                                                 01830000
018400     COPY WSSYDTTM.                                               01840000
018500     COPY WSEDDATE.                                               01850000
018600     COPY WSBUFFER.                                               01860000
018700*                                                                 01870000
018800 LINKAGE SECTION.                                                 01880000
018900*                                                                 01890000
019000 01  DFHCOMMAREA.                                                 01900000
019100     05  FILLER                  PIC X(170).                      01910000
019200*                                                                 01920000
019300 PROCEDURE DIVISION.                                              01930000
019400*                                                                 01940000
019500 P0000-MAINLINE.                                                  01950000
019600*                                                                 01960000
019700     EXEC CICS IGNORE CONDITION                                   01970000
019800               ERROR                                              01980000
019900     END-EXEC                                                     01990000
020000     EXEC CICS HANDLE ABEND                                       02000000
020100               LABEL(P9999-GOT-PROBLEM)                           02010000
020200     END-EXEC                                                     02020000
020300     COPY ABSTIME.                                                02030000
020400     IF EIBCALEN = ZERO                                           02040000
020500        PERFORM P9990-CLEAR-SCREEN                                02050000
020600     END-IF                                                       02060000
020700     MOVE DFHCOMMAREA             TO PSTCOMM-AREA                 02070000
020800     IF EIBTRNID = P998-TRAN                                      02080000
020900        SET CREATE-SCREEN TO TRUE                                 02090000
021000        MOVE LOW-VALUES TO PSTS02B                                02100000
021100        MOVE P998CA-CURSOR-POS TO EIBCPOSN                        02110000
021200        PERFORM P7010-READ-TSQUEUE                                02120000
021300        PERFORM P9000-SEND-MAP-AND-RETURN                         02130000
021400     END-IF                                                       02140000
021500     MOVE FICA-FICT-RECORD-KEY    TO FICTKEY                      02150000
021600     EXEC CICS READ                                               02160000
021700               DATASET(FICT-VIA-LOC-SEQ)                          02170000
021800               INTO(WS-FICT)                                      02180000
021900               LENGTH(FICTLOSQ-RLGTH)                             02190000
022000               RIDFLD(FICTKEY)                                    02200000
022100               KEYLENGTH(FICTLOSQ-KLGTH)                          02210000
022200               RESP(WS-RESPONSE)                                  02220000
022300     END-EXEC                                                     02230000
022400     MOVE WS-RESPONSE TO FILE-STATUS                              02240000
022500     IF NOT SUCCESS                                               02250000
022600        MOVE 'P0000-1'            TO ERR-PARAGRAPH                02260000
022700        MOVE FICTKEY              TO ERR-KEY                      02270000
022800        PERFORM P9999-GOT-PROBLEM                                 02280000
022900     END-IF                                                       02290000
023000     IF EIBTRNID NOT = P02B-TRAN                                  02300000
023100        SET CREATE-SCREEN         TO TRUE                         02310000
023200        MOVE LOW-VALUES           TO PSTS02B                      02320000
023300        PERFORM P0200-GET-DATES                                   02330000
023400        MOVE SPACES               TO FICA-NEXT-JOB                02340000
023500        IF FICT-JOB-SHIFT NOT NUMERIC                             02350000
023600           MOVE 1                 TO FICA-SHIFT-NUM               02360000
023700        ELSE                                                      02370000
023800           MOVE FICT-JOB-SHIFT    TO FICA-SHIFT                   02380000
023900        END-IF                                                    02390000
024000        PERFORM P4000-BUILD-SCREEN                                02400000
024100        PERFORM P9000-SEND-MAP-AND-RETURN                         02410000
024200     END-IF                                                       02420000
024300     MOVE EIBAID TO PF-CHECK                                      02430000
024400     IF PFKEY11                                                   02440000
024500        SET PSTCA-FLD-RET-PFKEY11     TO TRUE                     02450000
024600     END-IF                                                       02460000
024700     IF EXIT-KEY OR PFKEY11                                       02470000
024800        PERFORM P9100-SETUP-SCR02                                 02480000
024900     END-IF                                                       02490000
025000     PERFORM P0100-PROCESS-INPUT                                  02500000
025100     PERFORM P9000-SEND-MAP-AND-RETURN.                           02510000
025200*                                                                 02520000
025300 P0100-PROCESS-INPUT.                                             02530000
025400*                                                                 02540000
025500     MOVE P02B-MAP-VERSION(PSTCA-SUB) TO P02B-MAP                 02550000
025600     EXEC CICS RECEIVE MAP(P02B-MAP)                              02560000
025700                       MAPSET(P02B-SET)                           02570000
025800                       INTO(PSTS02B)                              02580000
025900                       RESP(WS-RESPONSE)                          02590000
026000     END-EXEC                                                     02600000
026100     MOVE WS-RESPONSE TO FILE-STATUS                              02610000
026200     IF NOT SUCCESS                                               02620000
026300        MOVE 'P0100'   TO ERR-PARAGRAPH                           02630000
026400        PERFORM P9999-GOT-PROBLEM                                 02640000
026500     END-IF                                                       02650000
026600     IF PFKEY1                                                    02660000
026700        PERFORM P7000-WRITE-TSQUEUE                               02670000
026800        PERFORM P9500-SETUP-SCR998                                02680000
026900     END-IF                                                       02690000
027000                                                                  02700000
027100     PERFORM P0200-GET-DATES                                      02710000
027200                                                                  02720000
027300     IF PFKEY8 AND FICA-NEXT-JOB NOT > SPACES                     02730000
027400        SET ENTER-KEY           TO TRUE                           02740000
027500     END-IF                                                       02750000
027600     IF ENTER-KEY                                                 02760000
027700        MOVE SPACES             TO FICA-NEXT-JOB                  02770000
027800        IF FICT-JOB-SHIFT NOT NUMERIC                             02780000
027900           MOVE 1               TO FICA-SHIFT-NUM                 02790000
028000        ELSE                                                      02800000
028100           MOVE FICT-JOB-SHIFT  TO FICA-SHIFT                     02810000
028200        END-IF                                                    02820000
028300     ELSE                                                         02830000
028400        IF NOT PFKEY8                                             02840000
028500*               INVALID-FUNC-MSG                                  02850000
028600           MOVE 'I006' TO MSGLOG-CODE                             02860000
028700           PERFORM P9000-SEND-MAP-AND-RETURN                      02870000
028800        END-IF                                                    02880000
028900     END-IF                                                       02890000
029000     PERFORM P4000-BUILD-SCREEN.                                  02900000
029100                                                                  02910000
029200*                                                                 02920000
029300 P0200-GET-DATES.                                                 02930000
029400*                                                                 02940000
029500     EXEC CICS ASKTIME                                            02950000
029600               ABSTIME(WS-ABSTIME)                                02960000
029700     END-EXEC                                                     02970000
029800     ADD WS-ABSTIME-OFFSET TO WS-ABSTIME                          02980000
029900     EXEC CICS FORMATTIME                                         02990000
030000               ABSTIME(WS-ABSTIME)                                03000000
030100               YYYYMMDD(WS-SYSTEM-DATE-CENT)                      03010000
030200               TIME(WS-SYSTEM-TIME-AREA)                          03020000
030300     END-EXEC                                                     03030000
030400*                                                                 03040000
030500*    INSTALL APPLICATION DATE/TIME                                03050000
030600*                                                                 03060000
030700     IF PSTCA-DATE-TIME-OFFSET > SPACES                           03070000
030800        MOVE ZEROS                  TO DATE-CONVERSION-PARMS      03080000
030900        MOVE WS-SYSTEM-DATE         TO PARM-PRI-DATE-GREG         03090000
031000        MOVE WS-SYSTEM-TIME         TO PARM-PRI-HRMN              03100000
031100        PERFORM P9810-PROCESS-OFFSET                              03110000
031200        MOVE PARM-RES-GREG-CENT     TO WS-SYSTEM-CENT             03120000
031300        MOVE PARM-RES-DATE-GREG     TO WS-SYSTEM-DATE             03130000
031400        MOVE PARM-RES-HRMN          TO WS-SYSTEM-TIME             03140000
031500        MOVE PARM-RES-DAY-OF-WEEK   TO WS-DAY                     03150000
031600     ELSE                                                         03160000
031700        MOVE ZEROS                  TO DATE-CONVERSION-PARMS      03170000
031800        MOVE WS-SYSTEM-DATE         TO PARM-PRI-DATE-GREG         03180000
031900        SET PARM-CONV               TO TRUE                       03190000
032000        EXEC CICS LINK                                            03200000
032100                  PROGRAM(P903-PGM)                               03210000
032200                  COMMAREA(DATE-CONVERSION-PARMS)                 03220000
032300                  LENGTH(P903-LGTH)                               03230000
032400                  RESP(WS-RESPONSE)                               03240000
032500        END-EXEC                                                  03250000
032600        IF NOT SUCCESS                                            03260000
032700           MOVE 'P0200' TO ERR-PARAGRAPH                          03270000
032800           PERFORM P9999-GOT-PROBLEM                              03280000
032900        END-IF                                                    03290000
033000        MOVE PARM-PRI-DAY-OF-WEEK   TO WS-DAY                     03300000
033100     END-IF.                                                      03310000
033200*                                                                 03320000
033300 P4000-BUILD-SCREEN.                                              03330000
033400*                                                                 03340000
033500     MOVE FICT-JOB-DIST             TO SCR02B-DIST                03350000
033600     MOVE FICT-JOB-SUB-DIST         TO SCR02B-SUB-DIST            03360000
033700     MOVE FICT-DESC(PSTCA-SUB)      TO CTXT-UNF-FIELD             03370000
033800     MOVE 30 TO CTXT-UNF-FIELD-LEN                                03380000
033900     PERFORM P8994-CENTER-TEXT                                    03390000
034000     MOVE CTXT-FOR-FIELD            TO SCR02B-TITLE               03400000
034100     PERFORM VARYING I FROM 1 BY 1                                03410000
034200               UNTIL I > 2                                        03420000
034300       MOVE SPACES                  TO SCR02B-JOB-NAME(I)         03430000
034400                                       SCR02B-JOB-DESC(I)         03440000
034500                                       SCR02B-JOB-ODT(I)          03450000
034600       PERFORM VARYING J FROM 1 BY 1                              03460000
034700               UNTIL J > 7                                        03470000
034800         MOVE LOW-VALUES            TO SCR02B-EMP-NAME-HI(I J)    03480000
034900         MOVE SPACES                TO SCR02B-EMP-NAME(I J)       03490000
035000                                       SCR02B-CRAFT-CODE(I J)     03500000
035100                                       SCR02B-EMP-STAT(I J)       03510000
035200                                       SCR02B-REST-TIME(I J)      03520000
035300                                       SCR02B-REST-DY(I J)        03530000
035400                                       SCR02B-RETURN-TIME(I J)    03540011
035410                                       SCR02B-RETURN-DY  (I J)    03541011
035500                                       SCR02B-REST-DAYS(I J)      03550000
035600       END-PERFORM                                                03560000
035700     END-PERFORM                                                  03570000
035800     MOVE SPACES                    TO SCR02B-ERRORMSG            03580000
035900     IF FICT-JOB-TYPE = 'A'                                       03590000
036000        SET SHIFTS-NOT-DONE         TO TRUE                       03600000
036100        MOVE 1                      TO SET-NUM                    03610000
036200        PERFORM UNTIL SHIFTS-DONE                                 03620000
036300           PERFORM P8000-START-TRCNKEY3                           03630000
036400           PERFORM UNTIL DONE OR SET-NUM > 2                      03640000
036500              PERFORM P8100-READNEXT-TRCNKEY3                     03650000
036600              IF JOB-QUALIFIES                                    03660000
036700                 PERFORM P4010-CHECK-SHIFT                        03670000
036800              END-IF                                              03680000
036900           END-PERFORM                                            03690000
037000           EXEC CICS ENDBR                                        03700000
037100                     DATASET(TRAIN-CN-VIA-DSD-ASGN)               03710000
037200                     RESP(WS-RESPONSE)                            03720000
037300           END-EXEC                                               03730000
037400           IF SET-NUM > 2                                         03740000
037500              SET SHIFTS-DONE       TO TRUE                       03750000
037600           ELSE                                                   03760000
037700              IF FICT-JOB-SHIFT > SPACE                           03770000
037800                 SET SHIFTS-DONE    TO TRUE                       03780000
037900              ELSE                                                03790000
038000                 IF FICA-SHIFT-NUM < 3                            03800000
038100                    ADD 1           TO FICA-SHIFT-NUM             03810000
038200                    MOVE SPACES     TO FICA-NEXT-JOB              03820000
038300                 ELSE                                             03830000
038400                    SET SHIFTS-DONE TO TRUE                       03840000
038500                 END-IF                                           03850000
038600              END-IF                                              03860000
038700           END-IF                                                 03870000
038800        END-PERFORM                                               03880000
038900     ELSE                                                         03890000
039000        SET DONE                    TO TRUE                       03900000
039100     END-IF                                                       03910000
039200     IF DONE                                                      03920000
039300*            'END OF JOB LIST'                                    03930000
039400        MOVE 'E008' TO MSGLOG-CODE                                03940000
039500        MOVE SPACES                 TO FICA-NEXT-JOB              03950000
039600     ELSE                                                         03960000
039700        MOVE TRCN-ASSIGNMENT        TO FICA-NEXT-JOB              03970000
039800     END-IF.                                                      03980000
039900*                                                                 03990000
040000 P4010-CHECK-SHIFT.                                               04000000
040100*                                                                 04010000
040200     MOVE ZEROS                     TO ROW-NUM                    04020000
040300     PERFORM VARYING I FROM 1 BY 1                                04030000
040400        UNTIL I > NUMBER-OF-JOB-TYPES                             04040000
040500        IF JOB-DEF-TBL-CLASS(I) = JOB-DEF-CLASS                   04050000
040600           PERFORM VARYING J FROM 1 BY 1                          04060000
040700              UNTIL J > 7                                         04070000
040800              IF JOB-DEF-TBL-CRAFT-CODE(I, J) > SPACE             04080000
040900                 MOVE TRCN-KEY3     TO AJJOBKEY-AREA              04090000
041000                 MOVE JOB-DEF-TBL-CRAFT-CODE(I, J)                04100000
041100                                    TO AJ-JOB-ASGN-CC             04110000
041200                 PERFORM P8200-READ-AJJOBKEY                      04120000
041300                 IF SUCCESS                                       04130000
041400                    SET DE-YYMMDD-FORMAT TO TRUE                  04140000
041500                    MOVE WS-AJ-EFF-DATE  TO DE-YYMMDD             04150000
041600                    PERFORM P8998-DATEEDIT                        04160000
041700                    MOVE DE-CCYYMMDD     TO WS-AJ-EFF-DATE-CENT   04170000
041800                    IF WS-AJ-EFF-DATE-CENT NOT >                  04180000
041900                       WS-SYSTEM-DATE-CENT                        04190000
042000*                   IF AJ-EFF-DATE NOT > WS-SYSTEM-DATE           04200000
042100                       PERFORM P4020-GET-JOB-SCHEDULE             04210000
042200                    END-IF                                        04220000
042300                 ELSE                                             04230000
042400                    IF NOT NO-RECORD-FND                          04240000
042500                       MOVE 'P4010-1'                             04250000
042600                                    TO ERR-PARAGRAPH              04260000
042700                       MOVE AJJOBKEY                              04270000
042800                                    TO ERR-KEY                    04280000
042900                       PERFORM P9999-GOT-PROBLEM                  04290000
043000                    END-IF                                        04300000
043100                 END-IF                                           04310000
043200              END-IF                                              04320000
043300           END-PERFORM                                            04330000
043400           MOVE NUMBER-OF-JOB-TYPES TO I                          04340000
043500        END-IF                                                    04350000
043600     END-PERFORM                                                  04360000
043700     IF ROW-NUM > ZERO                                            04370000
043800        ADD 1                       TO SET-NUM                    04380000
043900     END-IF.                                                      04390000
044000*                                                                 04400000
044100 P4020-GET-JOB-SCHEDULE.                                          04410000
044200*                                                                 04420000
044300     MOVE AJJOBKEY-AREA             TO WORK-JS-KEY1               04430000
044400     MOVE WS-SYSTEM-DATE            TO WK-JSK1-EXP-DATE           04440000
044500     MOVE WORK-JS-KEY1              TO JSKEY1                     04450000
044600     EXEC CICS READ                                               04460000
044700               DATASET(JS-VIA-JSKEY1)                             04470000
044800               INTO(WS-JOB-SCHEDULE)                              04480000
044900               LENGTH(JSKEY1-RLGTH)                               04490000
045000               RIDFLD(JSKEY1)                                     04500000
045100               KEYLENGTH(JSKEY1-KLGTH)                            04510000
045200               GTEQ                                               04520000
045300               RESP(WS-RESPONSE)                                  04530000
045400     END-EXEC                                                     04540000
045500     MOVE WS-RESPONSE               TO FILE-STATUS                04550000
045600     IF SUCCESS                                                   04560000
045700        IF JSK1-ASGN-DIST = WK-JSK1-ASGN-DIST                     04570000
045800           AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST         04580000
045900           AND JSK1-ASSIGNMENT = WK-JSK1-ASSIGNMENT               04590000
046000           AND JSK1-ASGN-STATS                                    04600000
046100           CONTINUE                                               04610000
046200        ELSE                                                      04620000
046300           SET NO-RECORD-FND        TO TRUE                       04630000
046400        END-IF                                                    04640000
046500     END-IF                                                       04650000
046600     IF NOT SUCCESS                                               04660000
046700        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     04670000
046800           MOVE 'P4020-1'           TO ERR-PARAGRAPH              04680000
046900           MOVE JSKEY1              TO ERR-KEY                    04690000
047000           PERFORM P9999-GOT-PROBLEM                              04700000
047100        END-IF                                                    04710000
047200     END-IF                                                       04720000
047300     IF SUCCESS                                                   04730000
047400        IF FICT-JOB-SHIFT NOT > SPACE                             04740000
047500           MOVE FICA-SHIFT          TO WS-SHIFT                   04750000
047600        ELSE                                                      04760000
047700           MOVE FICT-JOB-SHIFT      TO WS-SHIFT                   04770000
047800        END-IF                                                    04780000
047900        MOVE JS-KEY1                TO WORK-JS-KEY1               04790000
048000        MOVE WS-DAY                 TO WK-JSK1-ASGN-DAY-NUM       04800000
048100        SET SCH-NOT-DONE            TO TRUE                       04810000
048200        PERFORM UNTIL SCH-DONE                                    04820000
048300           MOVE WORK-JS-KEY1        TO JSKEY1                     04830000
048400           EXEC CICS READ                                         04840000
048500                     DATASET(JS-VIA-JSKEY1)                       04850000
048600                     INTO(WS-JOB-SCHEDULE)                        04860000
048700                     LENGTH(JSKEY1-RLGTH)                         04870000
048800                     RIDFLD(JSKEY1)                               04880000
048900                     KEYLENGTH(JSKEY1-KLGTH)                      04890000
049000                     GTEQ                                         04900000
049100                     RESP(WS-RESPONSE)                            04910000
049200           END-EXEC                                               04920000
049300           MOVE WS-RESPONSE         TO FILE-STATUS                04930000
049400           IF SUCCESS                                             04940000
049500              IF JSK1-ASGN-DIST = WK-JSK1-ASGN-DIST               04950000
049600                 AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST   04960000
049700                 AND JSK1-ASSIGNMENT = WK-JSK1-ASSIGNMENT         04970000
049800                 AND JSK1-EXP-DATE = WK-JSK1-EXP-DATE             04980000
049900                 AND JSK1-ASGN-DAY-NUM = WS-DAY                   04990000
050000                 MOVE JS-KEY1       TO WORK-JS-KEY1               05000000
050100                 MOVE JSK1-ASGN-START-TIME                        05010000
050200                                    TO JOB-SHIFT-TIME             05020000
050300                 IF (WS-SHIFT = '1' AND 1ST-SHIFT-JOB)            05030000
050400                    OR (WS-SHIFT = '2' AND 2ND-SHIFT-JOB)         05040000
050500                    OR (WS-SHIFT = '3' AND 3RD-SHIFT-JOB)         05050000
050600                    PERFORM P4100-MOVE-JOB-TO-SCREEN              05060000
050700                    SET SCH-DONE    TO TRUE                       05070000
050800                 ELSE                                             05080000
050900                    ADD 1           TO WK-JSK1-ASGN-START-TIME-NUM05090000
051000                 END-IF                                           05100000
051100              ELSE                                                05110000
051200                 SET SCH-DONE       TO TRUE                       05120000
051300              END-IF                                              05130000
051400           ELSE                                                   05140000
051500              SET SCH-DONE          TO TRUE                       05150000
051600              IF NOT (NO-RECORD-FND OR END-OF-FILE)               05160000
051700                 MOVE 'P4020-2'     TO ERR-PARAGRAPH              05170000
051800                 MOVE JSKEY1        TO ERR-KEY                    05180000
051900                 PERFORM P9999-GOT-PROBLEM                        05190000
052000              END-IF                                              05200000
052100           END-IF                                                 05210000
052200        END-PERFORM                                               05220000
052300     END-IF.                                                      05230000
052400*                                                                 05240000
052500 P4100-MOVE-JOB-TO-SCREEN.                                        05250000
052600*                                                                 05260000
052700     ADD 1                          TO ROW-NUM                    05270000
052800     IF SCR02B-JOB-NAME(SET-NUM) NOT > SPACES                     05280000
052900        MOVE AJ-JOB-ASGN-ID         TO SCR02B-JOB-NAME(SET-NUM)   05290000
053000        MOVE TRCN-DESCRIPTION       TO SCR02B-JOB-DESC(SET-NUM)   05300000
053100        MOVE JSK1-ASGN-START-TIME   TO WS-FORMAT-HRMN             05310000
053200        MOVE WS-FORMAT-HR           TO DISP-HOUR                  05320000
053300        MOVE WS-FORMAT-MN           TO DISP-MIN                   05330000
053400        MOVE DISPLAY-TIME           TO SCR02B-JOB-ODT(SET-NUM)    05340000
053500     END-IF                                                       05350000
053600     MOVE AJ-JOB-ASGN-CC            TO                            05360000
053700                              SCR02B-CRAFT-CODE(SET-NUM, ROW-NUM) 05370000
053800*                                                                 05380000
053900     MOVE WORK-JS-KEY1              TO JS-KEY1                    05390000
054000     SET JSK1-ASGN-STATS            TO TRUE                       05400000
054100     MOVE SPACES                    TO JSK1-ASGN-START-TIME       05410000
054200     MOVE JS-KEY1                   TO JSKEY1                     05420000
054300     EXEC CICS READ                                               05430000
054400               DATASET(JS-VIA-JSKEY1)                             05440000
054500               INTO(WS-JOB-SCHEDULE)                              05450000
054600               LENGTH(JSKEY1-RLGTH)                               05460000
054700               RIDFLD(JSKEY1)                                     05470000
054800               KEYLENGTH(JSKEY1-KLGTH)                            05480000
054900               RESP(WS-RESPONSE)                                  05490000
055000     END-EXEC                                                     05500000
055100     MOVE WS-RESPONSE               TO FILE-STATUS                05510000
055200     IF NOT SUCCESS                                               05520000
055300        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     05530000
055400           MOVE 'P4100-1'           TO ERR-PARAGRAPH              05540000
055500           MOVE JSKEY1              TO ERR-KEY                    05550000
055600           PERFORM P9999-GOT-PROBLEM                              05560000
055700        END-IF                                                    05570000
055800        MOVE SPACES                 TO WS-JOB-SCHEDULE            05580000
055900     END-IF                                                       05590000
056000*                                                                 05600000
056100     MOVE 1                         TO STRING-POINTER             05610000
056200     PERFORM VARYING REST-SUB FROM 1 BY 1                         05620000
056300         UNTIL REST-SUB > 7                                       05630000
056400        IF JOB-ON-REST-DAY(REST-SUB)                              05640000
056500           STRING DAYOFWK-2CHAR(REST-SUB, PSTCA-SUB)              05650000
056600                  DELIMITED BY SIZE                               05660000
056700                  INTO SCR02B-REST-DAYS(SET-NUM, ROW-NUM)         05670000
056800                  POINTER STRING-POINTER                          05680000
056900           ADD 1                    TO STRING-POINTER             05690000
057000        END-IF                                                    05700000
057100     END-PERFORM                                                  05710000
057200     PERFORM P4120-EMPLOYEE-INFO.                                 05720000
057300*                                                                 05730000
057400 P4120-EMPLOYEE-INFO.                                             05740000
057500*                                                                 05750000
057600     SET ASGN-AJ-JOB OF ASGN-JOB-TYPE                             05760000
057700                                    TO TRUE                       05770000
057800     MOVE AJ-JOB-DIST               TO ASGN-DIST                  05780000
057900     MOVE AJ-JOB-SUB-DIST           TO ASGN-SUB-DIST              05790000
058000     MOVE AJ-JOB-ASSIGNMENT         TO                            05800000
058100                                   ASGN-AJ-JOB OF ASGN-ASSIGNMENT 05810000
058200     MOVE WS-ASGN-FILE              TO WS-ASSIGN-RECORD           05820000
058300     IF FICT-EMPLOYEE-SELECTION = 'O'                             05830000
058400        SET GET-JOB-RECORD          TO TRUE                       05840000
058500     END-IF                                                       05850000
058600     IF FICT-EMPLOYEE-SELECTION = 'T'                             05860000
058700        SET GET-LATEST-TEMP-EMP     TO TRUE                       05870000
058800     END-IF                                                       05880000
058900     EXEC CICS LINK                                               05890000
059000               PROGRAM(P901-PGM)                                  05900000
059100               COMMAREA(WS-ASSIGN-COMMAREA)                       05910000
059200               LENGTH(P901-LGTH)                                  05920000
059300               RESP(WS-RESPONSE)                                  05930000
059400     END-EXEC                                                     05940000
059500     MOVE WS-RESPONSE               TO FILE-STATUS                05950000
059600     IF NOT SUCCESS                                               05960000
059700        MOVE 'P4120-1'              TO ERR-PARAGRAPH              05970000
059800        PERFORM P9999-GOT-PROBLEM                                 05980000
059900     END-IF                                                       05990000
060000     MOVE WS-ASSIGN-RECORD          TO WS-ASGN-FILE               06000000
060100     IF ASGN-EMP-NO = 0 AND GET-LATEST-TEMP-EMP                   06010000
060200        SET ASGN-AJ-JOB OF ASGN-JOB-TYPE                          06020000
060300                                    TO TRUE                       06030000
060400        MOVE AJ-JOB-DIST            TO ASGN-DIST                  06040000
060500        MOVE AJ-JOB-SUB-DIST        TO ASGN-SUB-DIST              06050000
060600        MOVE AJ-JOB-ASSIGNMENT      TO                            06060000
060700                                  ASGN-AJ-JOB OF ASGN-ASSIGNMENT  06070000
060800        MOVE WS-ASGN-FILE           TO WS-ASSIGN-RECORD           06080000
060900        SET GET-JOB-RECORD          TO TRUE                       06090000
061000        EXEC CICS LINK                                            06100000
061100                  PROGRAM(P901-PGM)                               06110000
061200                  COMMAREA(WS-ASSIGN-COMMAREA)                    06120000
061300                  LENGTH(P901-LGTH)                               06130000
061400                  RESP(WS-RESPONSE)                               06140000
061500        END-EXEC                                                  06150000
061600        MOVE WS-RESPONSE            TO FILE-STATUS                06160000
061700        IF NOT SUCCESS                                            06170000
061800           MOVE 'P4120-2'           TO ERR-PARAGRAPH              06180000
061900           PERFORM P9999-GOT-PROBLEM                              06190000
062000        END-IF                                                    06200000
062100        MOVE WS-ASSIGN-RECORD       TO WS-ASGN-FILE               06210000
062200***                                                               06220000
062300***  LIST PERMANENT ASSIGNMENTS AS 'OPEN' IF THE                  06230000
062400***  EMPLOYEE IS ON A TEMPORARY ASSIGNMENT.   CNC0089/CW0896      06240000
062500***                                                               06250000
062600        IF ASGN-EMP-NO > ZERO                                     06260000
062700           PERFORM P4150-GET-EMPS-LATEST-TEMP                     06270000
062800           IF WS-ASSIGN-RECORD > SPACES                           06280000
062900              MOVE ZERO             TO ASGN-EMP-NO                06290000
063000           END-IF                                                 06300000
063100        END-IF                                                    06310000
063200     END-IF                                                       06320000
063300     MOVE SCR02B-CRAFT-CODE(SET-NUM, ROW-NUM)                     06330000
063400                                    TO WS-CRAFT-CODE-CHECK        06340000
063500     IF ASGN-EMP-NO <= ZERO                                       06350000
063600        AND YARD-CRAFT                                            06360000
063700        MOVE AJ-JOB-DIST            TO SWASSGN-K-DISTRICT         06370000
063800        MOVE AJ-JOB-SUB-DIST        TO SWASSGN-K-SUB-DIST         06380000
063900        MOVE WS-DAY                 TO SWASSGN-K-DAY              06390000
064000        MOVE AJ-JOB-ASSIGNMENT      TO SWASSGN-K-ASSIGN           06400000
064100        MOVE SWASSGN-KEY            TO SWJOBKEY                   06410000
064200        EXEC CICS READ                                            06420000
064300                  DATASET(SWASSGN-VIA-ASSIGNMENT)                 06430000
064400                  INTO(WS-SWASSGN-FILE)                           06440000
064500                  LENGTH(SWASSIGN-RLGTH)                          06450000
064600                  RIDFLD(SWJOBKEY)                                06460000
064700                  KEYLENGTH(SWASSIGN-KLGTH)                       06470000
064800                  RESP(WS-RESPONSE)                               06480000
064900        END-EXEC                                                  06490000
065000        MOVE WS-RESPONSE            TO FILE-STATUS                06500000
065100        IF SUCCESS                                                06510000
065200           MOVE SWASSGN-EMP-NO      TO ASGN-EMP-NO                06520000
065300        END-IF                                                    06530000
065400     END-IF                                                       06540000
065500     MOVE DEFAULT-ATTR              TO                            06550000
065600                              SCR02B-EMP-NAME-HI(SET-NUM ROW-NUM) 06560000
065700     IF ASGN-EMP-NO <= ZERO                                       06570000
065800        MOVE AJ-OPEN-ASGN-MSG(PSTCA-SUB)                          06580000
065900                              TO SCR02B-EMP-NAME(SET-NUM ROW-NUM) 06590000
066000     ELSE                                                         06600000
066100        MOVE ASGN-EMP-NO            TO MSTRNBRK                   06610000
066200        EXEC CICS READ                                            06620000
066300                  DATASET(MSTR-VIA-EMP-NBR)                       06630000
066400                  INTO(WS-MSTR)                                   06640000
066500                  LENGTH(MSTRENBR-RLGTH)                          06650000
066600                  RIDFLD(MSTRNBRK)                                06660000
066700                  KEYLENGTH(MSTRENBR-KLGTH)                       06670000
066800                  RESP(WS-RESPONSE)                               06680000
066900        END-EXEC                                                  06690000
067000        MOVE WS-RESPONSE            TO FILE-STATUS                06700000
067100        IF SUCCESS                                                06710000
067200           PERFORM P4130-MOVE-EMPLOYEE-INFO                       06720000
067300        END-IF                                                    06730000
067400     END-IF                                                       06740000
067500     .                                                            06750000
067600*                                                                 06760000
067700 P4130-MOVE-EMPLOYEE-INFO.                                        06770000
067800*                                                                 06780000
067900     INITIALIZE WS-WORK-DATE-TIME                                 06790000
068000*                                                                 06800000
068100     PERFORM P5200-CHECK-COMPANY-CD                               06810000
068200     IF EMP-PERS-REST-NUM NUMERIC                                 06820000
068300        AND EMP-PERS-REST-NUM            > ZEROES                 06830001
068400        MOVE ZEROES                     TO DATE-CONVERSION-PARMS  06840001
068500        SET PARM-ADD                    TO TRUE                   06850001
068600        MOVE EMP-PERS-REST-DATE         TO PARM-PRI-DATE-GREG     06860001
068700        MOVE EMP-PERS-REST-TIME         TO PARM-PRI-HRMN          06870001
068800        IF APPLY-LEAD-TIME                                        06880000
068900           MOVE '0200'                  TO PARM-SEC-HRMN          06890001
069000           EXEC CICS LINK                                         06900000
069100                     PROGRAM(P903-PGM)                            06910000
069200                     COMMAREA(DATE-CONVERSION-PARMS)              06920000
069300                     LENGTH(P903-LGTH)                            06930000
069400                     RESP(WS-RESPONSE)                            06940000
069500           END-EXEC                                               06950000
069600           MOVE WS-RESPONSE             TO FILE-STATUS            06960001
069700           IF NOT SUCCESS                                         06970000
069800              MOVE 'P4130-1'            TO ERR-PARAGRAPH          06980001
069900              MOVE 'P903'               TO ERR-KEY                06990001
070000              PERFORM P9999-GOT-PROBLEM                           07000000
070100           END-IF                                                 07010000
070200           MOVE PARM-RES-GREG-CENT      TO DE-COMPARE1-CE         07020001
070300           MOVE PARM-RES-DATE-GREG      TO DE-COMPARE1-YYMMDD     07030001
070400           MOVE PARM-RES-HRMN           TO DE-COMPARE1-TIME       07040001
070500        ELSE                                                      07050000
070600           SET DE-YYMMDD-FORMAT         TO TRUE                   07060001
070700           MOVE EMP-PERS-REST-DATE      TO DE-YYMMDD              07070001
070800           PERFORM P8998-DATEEDIT                                 07080000
070900           MOVE DE-CCYYMMDD             TO DE-COMPARE1-DATE       07090001
071000           MOVE EMP-PERS-REST-TIME      TO DE-COMPARE1-TIME       07100001
071100        END-IF                                                    07110000
071200     ELSE                                                         07120000
071300        MOVE SPACES                     TO DE-COMPARE1-DATE-TIME  07130001
071400     END-IF                                                       07140000
071500*                                                                 07150000
071600     SET DE-YYMMDD-FORMAT        TO TRUE                          07160000
071700     MOVE EMP-MTOD-DATE          TO DE-YYMMDD                     07170000
071800     PERFORM P8998-DATEEDIT                                       07180000
071900     MOVE DE-CCYYMMDD            TO DE-COMPARE2-DATE              07190000
072000     MOVE EMP-MTOD-TIME          TO DE-COMPARE2-TIME              07200000
072100                                                                  07210000
072200*SXO C780                                                         07220000
072300     IF WS-CANADIAN-COMPANY                                       07230000
072400        MOVE SPACES              TO DE-COMPARE3-DATE-TIME         07240000
072500     ELSE                                                         07250000
072600        SET DE-YYMMDD-FORMAT     TO TRUE                          07260000
072700        MOVE EMP-US-RSTD-DATE    TO DE-YYMMDD                     07270000
072800        PERFORM P8998-DATEEDIT                                    07280000
072900        MOVE DE-CCYYMMDD         TO DE-COMPARE3-DATE              07290000
073000        MOVE EMP-US-RSTD-TIME    TO DE-COMPARE3-TIME              07300000
073100     END-IF                                                       07310000
073200*                                                                 07320000
073300*    CNC0183 - MATT - 12/11/99                                    07330000
073400*                                                                 07340000
073500     IF DE-COMPARE2-DATE-TIME > DE-COMPARE1-DATE-TIME             07350000
073600        IF DE-COMPARE2-DATE-TIME >  DE-COMPARE3-DATE-TIME         07360000
073700           MOVE DE-COMPARE2-DATE-TIME                             07370000
073800                                 TO DE-COMPARE1-DATE-TIME         07380000
073900        ELSE                                                      07390000
074000           MOVE DE-COMPARE3-DATE-TIME                             07400000
074100                                 TO DE-COMPARE1-DATE-TIME         07410000
074200        END-IF                                                    07420000
074300     ELSE                                                         07430000
074400        IF DE-COMPARE1-DATE-TIME <  DE-COMPARE3-DATE-TIME         07440000
074500           MOVE DE-COMPARE3-DATE-TIME                             07450000
074600                                 TO DE-COMPARE1-DATE-TIME         07460000
074700        END-IF                                                    07470000
074800     END-IF                                                       07480000
074900*SXO C780                                                         07490000
075000     MOVE DE-COMPARE1-DATE-TIME(3:10)                             07500000
075100                                 TO WS-WORK-DATE-TIME-C           07510000
075200                                                                  07520000
075300*-------------------------------------------------------*         07530000
075400*     CONVERT TO SYSTEM TIME ZONE                       *         07540000
075500*-------------------------------------------------------*         07550000
075600     IF WS-WORK-DATE-TIME-C NUMERIC                               07560000
075700        AND WS-WORK-DATE-TIME > ZERO                              07570000
075800        MOVE SPACES                 TO WS-CNTL-FILE               07580000
075900        SET SUB-DIST-TYPE-REC       TO TRUE                       07590000
076000        MOVE DIST IN WS-MSTR        TO CNTL-DIST                  07600000
076100        MOVE SUB-DIST IN WS-MSTR TO CNTL-SUB-DIST                 07610000
076200        PERFORM P8210-READ-CNTLFILE                               07620000
076300        MOVE CNTL-TIME-ZONE         TO TZ-IN-ZONE                 07630000
076400        MOVE WS-WORK-DATE-TIME TO TZ-IN-DATE-TIME                 07640000
076500        SET TZ-OUT-SYSTEM-ZONE TO TRUE                            07650000
076600        PERFORM P8996-TIMEZONE                                    07660000
076700        IF TZ-INVALID-PARAMETERS                                  07670000
076800           MOVE 'P4130-2'           TO ERR-PARAGRAPH              07680000
076900           PERFORM P8996-TZERROR                                  07690000
077000        END-IF                                                    07700000
077100*       IF TZ-OUT-DATE-TIME > WS-DATE-TIME                        07710000
077200        IF TZ-OUT-DATE-TIME-CENT > WS-PRESENT-TIME-CENT           07720000
077300           IF DIST IN WS-MSTR = SCR02B-DIST                       07730000
077400              AND SUB-DIST IN WS-MSTR = SCR02B-SUB-DIST           07740000
077500              CONTINUE                                            07750000
077600           ELSE                                                   07760000
077700*-------------------------------------------------------*         07770000
077800*             CONVERT TO SCREEN ZONE                    *         07780000
077900*-------------------------------------------------------*         07790000
078000              MOVE SPACES             TO WS-CNTL-FILE             07800000
078100              SET SUB-DIST-TYPE-REC TO TRUE                       07810000
078200              MOVE DIST IN WS-MSTR TO CNTL-DIST                   07820000
078300              MOVE SUB-DIST IN WS-MSTR TO CNTL-SUB-DIST           07830000
078400              PERFORM P8210-READ-CNTLFILE                         07840000
078500              MOVE CNTL-TIME-ZONE TO TZ-IN-ZONE                   07850000
078600              MOVE WS-WORK-DATE-TIME TO TZ-IN-DATE-TIME           07860000
078700              MOVE PSTCA-TIME-ZONE TO TZ-OUT-ZONE                 07870000
078800              PERFORM P8996-TIMEZONE                              07880000
078900              IF TZ-INVALID-PARAMETERS                            07890000
079000                 MOVE 'P4130-3'       TO ERR-PARAGRAPH            07900000
079100                 PERFORM P8996-TZERROR                            07910000
079200              END-IF                                              07920000
079300              MOVE TZ-OUT-DATE-TIME TO WS-WORK-DATE-TIME          07930000
079400           END-IF                                                 07940000
079500           MOVE WS-WORK-DATE-TIME(7:4) TO                         07950000
079600                      SCR02B-REST-TIME(SET-NUM ROW-NUM)           07960000
079700           MOVE WS-WORK-DATE-TIME(5:2) TO                         07970000
079800                      SCR02B-REST-DY(SET-NUM ROW-NUM)             07980000
079900        END-IF                                                    07990000
080000     END-IF                                                       08000000
080100     MOVE EMP-NAME TO SCR02B-EMP-NAME(SET-NUM ROW-NUM)            08010000
080200     EVALUATE TRUE                                                08020000
080300        WHEN AVAILABLE                                            08030000
080400           IF PSTCA-SUB = 2                                       08040000
080500             MOVE 'DISPONIBL' TO SCR02B-EMP-STAT(SET-NUM ROW-NUM) 08050000
080600           ELSE                                                   08060000
080700             MOVE 'AVAILABLE' TO SCR02B-EMP-STAT(SET-NUM ROW-NUM) 08070000
080800           END-IF                                                 08080000
080900        WHEN WORKING                                              08090000
081000           IF PSTCA-SUB = 2                                       08100000
081100              MOVE 'TRAVAIL' TO SCR02B-EMP-STAT(SET-NUM ROW-NUM)  08110000
081200           ELSE                                                   08120000
081300              MOVE 'WORKING' TO SCR02B-EMP-STAT(SET-NUM ROW-NUM)  08130000
081400           END-IF                                                 08140000
081500        WHEN OFF-MILES-DAYS                                       08150000
081600           IF EMP-MILES-DATE NUMERIC                              08160000
081700              AND EMP-MILES-DATE-NUM > 0                          08170000
081800              AND EMP-MILES-DATE-NUM < 32                         08180000
081900*             MOVE WS-MO TO                                       08190000
082000              MOVE WS-SYS-MO TO                                   08200000
082100                   SCR02B-RETURN-TIME-MM(SET-NUM ROW-NUM)         08210011
082200              MOVE EMP-MILES-DATE-NUM TO                          08220000
082300                   SCR02B-RETURN-TIME-DD(SET-NUM ROW-NUM)         08230011
082400*             IF EMP-MILES-DATE-NUM < WS-DY                       08240000
082500              IF EMP-MILES-DATE-NUM < WS-SYS-DY                   08250000
082600                 ADD 1 TO SCR02B-RETURN-TIME-MM(SET-NUM ROW-NUM)  08260011
082700                 IF SCR02B-RETURN-TIME-MM(SET-NUM ROW-NUM) > 12   08270011
082800                    MOVE 1 TO                                     08280000
082900                         SCR02B-RETURN-TIME-MM(SET-NUM ROW-NUM)   08290011
083000                 END-IF                                           08300000
083100              END-IF                                              08310000
083200           ELSE                                                   08320000
083300              MOVE EMP-MILES-DATE TO                              08330000
083400                   SCR02B-RETURN-TIME(SET-NUM ROW-NUM)            08340011
083500           END-IF                                                 08350000
083510           MOVE SPACES       TO SCR02B-RETURN-DY(SET-NUM ROW-NUM) 08351011
083600           PERFORM P4180-OFF-STATUS                               08360000
083700        WHEN VACATION                                             08370000
083800           PERFORM P4170-GET-RETURN-DATE                          08380000
083900           PERFORM P4180-OFF-STATUS                               08390000
084000        WHEN OTHER                                                08400000
084100*CNC0492 - FOR OTHER LAYOFF CODES, GET DUEBACK/BOOK-ON DATE/TIME  08410005
084200           IF NOT AVAILABLE  AND                                  08420007
084300              NOT WORKING    AND                                  08430007
084400              NOT TO-PLACE                                        08440007
084500              AND LAYOFF-TIME NUMERIC                             08450007
084600              AND LAYOFF-TIME > ZERO                              08460007
084700              PERFORM P1520-GET-DUEBACK-DATE                      08470008
084800           END-IF                                                 08480007
084900           PERFORM P4180-OFF-STATUS                               08490007
085000     END-EVALUATE                                                 08500000
085100     .                                                            08510000
085200*===============================================================* 08520008
085300 P1520-GET-DUEBACK-DATE.                                          08530008
085400*===============================================================* 08540008
085500                                                                  08550008
085600     INITIALIZE TASK-EMPLOYEE-KEY                                 08560008
085700     SET WS-DUEBACK-FOUND-N             TO TRUE                   08570008
085800     MOVE EMP-NBR OF WS-MSTR            TO EMP-NBR OF WS-TASK     08580008
085900     PERFORM P8300-START-TASK-FILE                                08590008
086000     IF SUCCESS                                                   08600008
086100        PERFORM P8310-READNEXT-TASK-FILE                          08610008
086200        PERFORM UNTIL NOT SUCCESS                                 08620008
086300           OR WS-DUEBACK-FOUND-Y                                  08630008
086400           OR EMP-NBR OF WS-MSTR NOT = EMP-NBR OF WS-TASK         08640008
086500           IF   TASK-LAYOFF-MARKUP                                08650008
086600           AND (TASK-DUE-BACK OR TASK-LO1 = 'A')                  08660008
086700              SET WS-DUEBACK-FOUND-Y    TO TRUE                   08670008
086800              MOVE TASK-LO-EXP-TIME OF WS-TASK TO                 08680011
086900                   SCR02B-RETURN-TIME(SET-NUM ROW-NUM)            08690011
087000              MOVE TASK-LO-EXP-DY   OF WS-TASK TO                 08700011
087100                   SCR02B-RETURN-DY(SET-NUM ROW-NUM)              08710012
087200           ELSE                                                   08720008
087300              PERFORM P8310-READNEXT-TASK-FILE                    08730008
087400           END-IF                                                 08740008
087500        END-PERFORM                                               08750008
087600     END-IF                                                       08760008
087700     PERFORM P8320-ENDBR-TASK-FILE                                08770008
087800     .                                                            08780008
087900*                                                                 08790000
088000 P4150-GET-EMPS-LATEST-TEMP.                                      08800000
088100*                                                                 08810000
088200     MOVE SPACES                     TO WS-ASSIGN-COMMAREA        08820000
088300     SET GET-EMPS-LATEST-TEMP-JOB    TO TRUE                      08830000
088400     MOVE ASGN-EMP-NO                TO                           08840000
088500          WS-ASSIGN-RECORD(LENGTH OF ASGNKEY1 + 1 :               08850000
088600                           LENGTH OF ASGN-EMP-NO)                 08860000
088700     EXEC CICS LINK                                               08870000
088800               PROGRAM(P901-PGM)                                  08880000
088900               COMMAREA(WS-ASSIGN-COMMAREA)                       08890000
089000               LENGTH(P901-LGTH)                                  08900000
089100               RESP(WS-RESPONSE)                                  08910000
089200     END-EXEC                                                     08920000
089300     MOVE WS-RESPONSE                TO FILE-STATUS               08930000
089400     IF NOT SUCCESS                                               08940000
089500        MOVE 'P4150-1'               TO ERR-PARAGRAPH             08950000
089600        MOVE 'P901LINK'              TO ERR-KEY                   08960000
089700        PERFORM P9999-GOT-PROBLEM                                 08970000
089800     END-IF                                                       08980000
089900***CLEAR RECORD IF NO ASSIGNMENT FOUND                            08990000
090000     IF WS-ASSIGN-RECORD(1 : LENGTH OF ASGNKEY1) = SPACES         09000000
090100        MOVE SPACES TO WS-ASSIGN-RECORD                           09010000
090200     END-IF                                                       09020000
090300     .                                                            09030000
090400                                                                  09040000
090500***************************************************************** 09050000
090600 P4170-GET-RETURN-DATE.                                           09060000
090700*    FOR EMPLOYEES ON VACATION, FIND THE BOOK-ON RECORD IN        09070000
090800*    THE TASK LIST.  THIS CONTAINS THE EXPECTED RETURN DATE.      09080000
090900***************************************************************** 09090000
091000     INITIALIZE TASK-EMPLOYEE-KEY                                 09100000
091100     MOVE EMP-NBR OF WS-MSTR TO EMP-NBR OF WS-TASK                09110000
091200     PERFORM P8300-START-TASK-FILE                                09120000
091300     IF SUCCESS                                                   09130000
091400        SET TASK-NOT-DONE TO TRUE                                 09140000
091500        PERFORM P8310-READNEXT-TASK-FILE                          09150000
091600        PERFORM UNTIL TASK-DONE                                   09160000
091700           IF SUCCESS                                             09170000
091800              AND EMP-NBR OF WS-MSTR = EMP-NBR OF WS-TASK         09180000
091900              IF TASK-LAYOFF-MARKUP                               09190000
092000                 SET TASK-DONE TO TRUE                            09200005
092100                 MOVE EFF-MO OF WS-TASK TO                        09210005
092200                      SCR02B-RETURN-TIME-MM(SET-NUM ROW-NUM)      09220011
092300                 MOVE EFF-DY OF WS-TASK TO                        09230005
092400                      SCR02B-RETURN-TIME-DD(SET-NUM ROW-NUM)      09240011
092410                 MOVE SPACES TO SCR02B-RETURN-DY(SET-NUM ROW-NUM) 09241011
092500              ELSE                                                09250000
092600                 PERFORM P8310-READNEXT-TASK-FILE                 09260000
092700              END-IF                                              09270000
092800           ELSE                                                   09280000
092900              SET TASK-DONE TO TRUE                               09290000
093000           END-IF                                                 09300000
093100        END-PERFORM                                               09310000
093200     END-IF                                                       09320000
093300     PERFORM P8320-ENDBR-TASK-FILE                                09330000
093400     .                                                            09340000
093500                                                                  09350000
093600*                                                                 09360000
093700 P4180-OFF-STATUS.                                                09370000
093800*                                                                 09380000
093900     IF PSTCA-SUB = 2                                             09390000
094000        MOVE ' COGES ' TO SCR02B-EMP-STAT(SET-NUM ROW-NUM)        09400000
094100     ELSE                                                         09410000
094200        MOVE '   OFF ' TO SCR02B-EMP-STAT(SET-NUM ROW-NUM)        09420000
094300     END-IF                                                       09430000
094400     .                                                            09440000
094500                                                                  09450000
094600*                                                                 09460000
094700 P5200-CHECK-COMPANY-CD.                                          09470000
094800*                                                                 09480000
094900     MOVE SPACES                 TO CNTLKEY                       09490000
095000                                    CNTLKEY-AREA                  09500000
095100     MOVE DIST OF WS-MSTR        TO CNTL-DIST                     09510000
095200     SET DIST-TYPE-REC           TO TRUE                          09520000
095300     MOVE CNTLKEY-AREA           TO CNTLKEY                       09530000
095400     PERFORM P8210-READ-CNTLFILE                                  09540000
095500     IF CNTL-BCR-COMPANY                                          09550000
095600        OR CNTL-CN-COMPANY                                        09560000
095700        OR CNTL-ACR-COMPANY                                       09570000
095800        SET WS-CANADIAN-COMPANY  TO TRUE                          09580000
095900     ELSE                                                         09590000
096000        SET WS-US-COMPANY        TO TRUE                          09600000
096100     END-IF                                                       09610000
096200     IF WS-CANADIAN-COMPANY                                       09620000
096300        SET DONT-APPLY-LEAD-TIME TO TRUE                          09630000
096400     ELSE                                                         09640000
096500        SET APPLY-LEAD-TIME      TO TRUE                          09650000
096600     END-IF                                                       09660000
096700     .                                                            09670000
096800*                                                                 09680000
096900 P7000-WRITE-TSQUEUE.                                             09690000
097000*                                                                 09700000
097100*                                                                 09710000
097200*      WRITE MAP TSQUEUE                                          09720000
097300*                                                                 09730000
097400     EXEC CICS ASSIGN                                             09740000
097500          EXTDS(WS-CICS-EXTDS-CODE)                               09750000
097600     END-EXEC                                                     09760000
097700*                                                                 09770000
097800     IF SCREEN-HAS-EXT-ATTR                                       09780000
097900        EXEC CICS SEND STRFIELD                                   09790000
098000                  FROM(WS-STRFIELD)                               09800000
098100                  LENGTH(WS-STRFIELD-LGTH)                        09810000
098200                  RESP(WS-RESPONSE)                               09820000
098300        END-EXEC                                                  09830000
098400        MOVE WS-RESPONSE           TO FILE-STATUS                 09840000
098500        IF NOT SUCCESS                                            09850000
098600           MOVE 'P7000-1'          TO ERR-PARAGRAPH               09860000
098700           MOVE 'SEND STRFIELD'    TO ERR-KEY                     09870000
098800           PERFORM P9999-GOT-PROBLEM                              09880000
098900        END-IF                                                    09890000
099000     END-IF                                                       09900000
099100*                                                                 09910000
099200     MOVE LENGTH OF WS-BUFFER-DATA TO WS-BUFFER-LGTH              09920000
099300     EXEC CICS RECEIVE BUFFER                                     09930000
099400               INTO(WS-BUFFER-DATA)                               09940000
099500               LENGTH(WS-BUFFER-LGTH)                             09950000
099600               RESP(WS-RESPONSE)                                  09960000
099700     END-EXEC                                                     09970000
099800     MOVE WS-RESPONSE              TO FILE-STATUS                 09980000
099900     IF NOT SUCCESS AND NOT EOC                                   09990000
100000        MOVE 'P7000-2'             TO ERR-PARAGRAPH               10000000
100100        MOVE 'RECEIVE BUFFER'      TO ERR-KEY                     10010000
100200        PERFORM P9999-GOT-PROBLEM                                 10020000
100300     END-IF                                                       10030000
100400     MOVE EIBCPOSN                 TO WS-BUFFER-CURSOR            10040000
100500                                                                  10050000
100600                                                                  10060000
100700     MOVE LENGTH OF WS-BUFFER-AREA TO P02BTSQ-QLGTH               10070000
100800     MOVE EIBTRMID                 TO P02BTSQ-MAP-TERM-ID         10080000
100900     EXEC CICS WRITEQ TS                                          10090000
101000               QUEUE(P02BTSQ-MAP-QUEUE-ID)                        10100000
101100               FROM(WS-BUFFER-AREA)                               10110000
101200               LENGTH(P02BTSQ-QLGTH)                              10120000
101300               RESP(WS-RESPONSE)                                  10130000
101400     END-EXEC                                                     10140000
101500     MOVE WS-RESPONSE              TO FILE-STATUS                 10150000
101600     IF NOT SUCCESS                                               10160000
101700        MOVE 'P7000-3'             TO ERR-PARAGRAPH               10170000
101800        PERFORM P9999-GOT-PROBLEM                                 10180000
101900     END-IF                                                       10190000
102000     MOVE EIBTRMID TO P02BTSQ-CA-TERM-ID                          10200000
102100     EXEC CICS WRITEQ TS                                          10210000
102200               QUEUE(P02BTSQ-CA-QUEUE-ID)                         10220000
102300               FROM(PSTCOMM-AREA)                                 10230000
102400               LENGTH(PSTCOMM-LGTH)                               10240000
102500               ITEM(P02BTSQ-QUEUE-ITEM)                           10250000
102600               RESP(WS-RESPONSE)                                  10260000
102700     END-EXEC                                                     10270000
102800     MOVE WS-RESPONSE              TO FILE-STATUS                 10280000
102900     IF NOT SUCCESS                                               10290000
103000        MOVE 'P7000-4'             TO ERR-PARAGRAPH               10300000
103100        PERFORM P9999-GOT-PROBLEM                                 10310000
103200     END-IF.                                                      10320000
103300*                                                                 10330000
103400 P7010-READ-TSQUEUE.                                              10340000
103500*                                                                 10350000
103600*              READ THE MAPS TSQUEUE                              10360000
103700*                                                                 10370000
103800     MOVE LENGTH OF WS-BUFFER-AREA TO P02BTSQ-QLGTH               10380000
103900     MOVE EIBTRMID                 TO P02BTSQ-MAP-TERM-ID         10390000
104000     EXEC CICS READQ TS                                           10400000
104100               QUEUE(P02BTSQ-MAP-QUEUE-ID)                        10410000
104200               INTO(WS-BUFFER-AREA)                               10420000
104300               LENGTH(P02BTSQ-QLGTH)                              10430000
104400               ITEM(P02BTSQ-QUEUE-ITEM)                           10440000
104500               RESP(WS-RESPONSE)                                  10450000
104600     END-EXEC                                                     10460000
104700     MOVE WS-RESPONSE              TO FILE-STATUS                 10470000
104800     IF SUCCESS                                                   10480000
104900        SET SEND-BUFFER            TO TRUE                        10490000
105000     ELSE                                                         10500000
105100        SET CREATE-SCREEN          TO TRUE                        10510000
105200        MOVE LOW-VALUES            TO PSTS02B                     10520000
105300     END-IF                                                       10530000
105400     MOVE EIBTRMID TO P02BTSQ-CA-TERM-ID                          10540000
105500     EXEC CICS READQ TS                                           10550000
105600               QUEUE(P02BTSQ-CA-QUEUE-ID)                         10560000
105700               INTO(PSTCOMM-AREA)                                 10570000
105800               LENGTH(PSTCOMM-LGTH)                               10580000
105900               ITEM(P02BTSQ-QUEUE-ITEM)                           10590000
106000               RESP(WS-RESPONSE)                                  10600000
106100     END-EXEC                                                     10610000
106200     MOVE WS-RESPONSE TO FILE-STATUS                              10620000
106300     IF NOT SUCCESS                                               10630000
106400        MOVE SPACES TO PSTCOMM-AREA                               10640000
106500     END-IF                                                       10650000
106600     PERFORM P7020-DELETE-TSQUEUE.                                10660000
106700*                                                                 10670000
106800 P7020-DELETE-TSQUEUE.                                            10680000
106900*                                                                 10690000
107000     MOVE EIBTRMID TO P02BTSQ-MAP-TERM-ID                         10700000
107100     EXEC CICS DELETEQ TS                                         10710000
107200               QUEUE(P02BTSQ-MAP-QUEUE-ID)                        10720000
107300               RESP(WS-RESPONSE)                                  10730000
107400     END-EXEC                                                     10740000
107500     MOVE EIBTRMID TO P02BTSQ-CA-TERM-ID                          10750000
107600     EXEC CICS DELETEQ TS                                         10760000
107700               QUEUE(P02BTSQ-CA-QUEUE-ID)                         10770000
107800               RESP(WS-RESPONSE)                                  10780000
107900     END-EXEC.                                                    10790000
108000*                                                                 10800000
108100 P8000-START-TRCNKEY3.                                            10810000
108200*                                                                 10820000
108300     SET NOT-DONE                   TO TRUE                       10830000
108400     MOVE SPACES                    TO TRCN-KEY3                  10840000
108500     MOVE FICT-JOB-DIST             TO TRCN-DIST3                 10850000
108600     MOVE FICT-JOB-SUB-DIST         TO TRCN-SDIST3                10860000
108700     MOVE FICA-NEXT-JOB             TO TRCN-ASSIGNMENT            10870000
108800     IF TRCN-ASSIGNMENT NOT > SPACES                              10880000
108900        MOVE '     A'               TO TRCN-ASSIGNMENT            10890000
109000     END-IF                                                       10900000
109100     MOVE TRCN-KEY3                 TO TRCNKEY3                   10910000
109200     EXEC CICS STARTBR                                            10920000
109300               DATASET(TRAIN-CN-VIA-DSD-ASGN)                     10930000
109400               RIDFLD(TRCNKEY3)                                   10940000
109500               GTEQ                                               10950000
109600               RESP(WS-RESPONSE)                                  10960000
109700     END-EXEC                                                     10970000
109800     MOVE WS-RESPONSE               TO FILE-STATUS                10980000
109900     IF NOT SUCCESS                                               10990000
110000        SET DONE                    TO TRUE                       11000000
110100        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     11010000
110200           MOVE 'P8000-1'           TO ERR-PARAGRAPH              11020000
110300           MOVE TRCNKEY3            TO ERR-KEY                    11030000
110400           PERFORM P9999-GOT-PROBLEM                              11040000
110500        END-IF                                                    11050000
110600     END-IF.                                                      11060000
110700*                                                                 11070000
110800 P8100-READNEXT-TRCNKEY3.                                         11080000
110900*                                                                 11090000
111000     SET JOB-NOT-QUALIFIED          TO TRUE                       11100000
111100     EXEC CICS READNEXT                                           11110000
111200               DATASET(TRAIN-CN-VIA-DSD-ASGN)                     11120000
111300               INTO(WS-TRCN-FILE)                                 11130000
111400               LENGTH(TRAINCN-DSD-RLGTH)                          11140000
111500               RIDFLD(TRCNKEY3)                                   11150000
111600               KEYLENGTH(TRAINCN-DSD-KLGTH)                       11160000
111700               RESP(WS-RESPONSE)                                  11170000
111800     END-EXEC                                                     11180000
111900     MOVE WS-RESPONSE               TO FILE-STATUS                11190000
112000     IF SUCCESS                                                   11200000
112100        IF TRCN-DIST3 = FICT-JOB-DIST                             11210000
112200           AND TRCN-SDIST3 = FICT-JOB-SUB-DIST                    11220000
112300           IF TRCN-ASSIGNMENT > FICA-NEXT-JOB                     11230000
112400              SET JOB-QUALIFIES     TO TRUE                       11240000
112500              MOVE TRCN-ASSIGNMENT  TO JOB-DEF-CHECK              11250000
112600              IF JOB-DEF-RELIEF-ASGN                              11260000
112700                 SET JOB-NOT-QUALIFIED                            11270000
112800                                    TO TRUE                       11280000
112900              END-IF                                              11290000
113000              IF FICT-JOB-GRP-PL-XB > SPACE                       11300000
113100                 AND FICT-JOB-GRP-PL-XB NOT = JOB-DEF-YARD-CODE   11310000
113200                 SET JOB-NOT-QUALIFIED                            11320000
113300                                    TO TRUE                       11330000
113400              END-IF                                              11340000
113500              IF FICT-JOB-CLASS > SPACE                           11350000
113600                 AND FICT-JOB-CLASS NOT = JOB-DEF-CLASS           11360000
113700                 SET JOB-NOT-QUALIFIED                            11370000
113800                                    TO TRUE                       11380000
113900              END-IF                                              11390000
114000           END-IF                                                 11400000
114100        ELSE                                                      11410000
114200           SET DONE                 TO TRUE                       11420000
114300        END-IF                                                    11430000
114400     ELSE                                                         11440000
114500        SET DONE                    TO TRUE                       11450000
114600        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     11460000
114700           MOVE 'P8100-1'           TO ERR-PARAGRAPH              11470000
114800           MOVE TRCNKEY3            TO ERR-KEY                    11480000
114900           PERFORM P9999-GOT-PROBLEM                              11490000
115000        END-IF                                                    11500000
115100     END-IF.                                                      11510000
115200*                                                                 11520000
115300 P8200-READ-AJJOBKEY.                                             11530000
115400*                                                                 11540000
115500     MOVE AJJOBKEY-AREA             TO AJJOBKEY                   11550000
115600     EXEC CICS READ                                               11560000
115700               DATASET(AJ-VIA-JNAME-JCRAFT)                       11570000
115800               INTO(WS-ASGNED-JOBS)                               11580000
115900               LENGTH(AJNAMECR-RLGTH)                             11590000
116000               RIDFLD(AJJOBKEY)                                   11600000
116100               KEYLENGTH(AJNAMECR-KLGTH)                          11610000
116200               RESP(WS-RESPONSE)                                  11620000
116300     END-EXEC                                                     11630000
116400     MOVE WS-RESPONSE               TO FILE-STATUS                11640000
116500     IF NOT SUCCESS                                               11650000
116600        IF NOT NO-RECORD-FND                                      11660000
116700           MOVE 'P8200-1'           TO ERR-PARAGRAPH              11670000
116800           MOVE AJJOBKEY            TO ERR-KEY                    11680000
116900           PERFORM P9999-GOT-PROBLEM                              11690000
117000        END-IF                                                    11700000
117100     END-IF.                                                      11710000
117200*                                                                 11720000
117300 P8210-READ-CNTLFILE.                                             11730000
117400*                                                                 11740000
117500     MOVE CNTLKEY-AREA             TO CNTLKEY                     11750000
117600     EXEC CICS READ                                               11760000
117700               DATASET(CNTL-FILE-VIA-CNTLKEY)                     11770000
117800               INTO(WS-CNTL-FILE)                                 11780000
117900               LENGTH(CNTLFILE-RLGTH)                             11790000
118000               RIDFLD(CNTLKEY)                                    11800000
118100               KEYLENGTH(CNTLFILE-KLGTH)                          11810000
118200               RESP(WS-RESPONSE)                                  11820000
118300     END-EXEC                                                     11830000
118400     MOVE WS-RESPONSE               TO FILE-STATUS                11840000
118500     IF NOT SUCCESS                                               11850000
118600        IF NOT NO-RECORD-FND                                      11860000
118700           MOVE 'P8210-1'           TO ERR-PARAGRAPH              11870000
118800           MOVE CNTLKEY             TO ERR-KEY                    11880000
118900           PERFORM P9999-GOT-PROBLEM                              11890000
119000        ELSE                                                      11900000
119100           MOVE 'E'                 TO CNTL-TIME-ZONE             11910000
119200        END-IF                                                    11920000
119300     END-IF.                                                      11930000
119400                                                                  11940000
119500***************************************************************** 11950000
119600 P8300-START-TASK-FILE.                                           11960000
119700***************************************************************** 11970000
119800     MOVE TASK-EMPLOYEE-KEY TO TASKEMPK                           11980000
119900     EXEC CICS STARTBR                                            11990000
120000               DATASET(TASK-VIA-EMP-NBR)                          12000000
120100               RIDFLD(TASKEMPK)                                   12010000
120200               GTEQ                                               12020000
120300               RESP(WS-RESPONSE)                                  12030000
120400     END-EXEC                                                     12040000
120500     MOVE WS-RESPONSE TO FILE-STATUS                              12050000
120600     IF NOT SUCCESS                                               12060000
120700        IF NO-RECORD-FND OR END-OF-FILE                           12070000
120800           CONTINUE                                               12080000
120900        ELSE                                                      12090000
121000           MOVE 'P8300-1' TO ERR-PARAGRAPH                        12100000
121100           MOVE TASKEMPK TO ERR-KEY                               12110000
121200           PERFORM P9999-GOT-PROBLEM                              12120000
121300        END-IF                                                    12130000
121400     END-IF                                                       12140000
121500     .                                                            12150000
121600                                                                  12160000
121700***************************************************************** 12170000
121800 P8310-READNEXT-TASK-FILE.                                        12180000
121900***************************************************************** 12190000
122000     EXEC CICS READNEXT                                           12200000
122100               DATASET(TASK-VIA-EMP-NBR)                          12210000
122200               INTO(WS-TASK)                                      12220000
122300               LENGTH(TASKENBR-RLGTH)                             12230000
122400               RIDFLD(TASKEMPK)                                   12240000
122500               KEYLENGTH(TASKENBR-KLGTH)                          12250000
122600               RESP(WS-RESPONSE)                                  12260000
122700     END-EXEC                                                     12270000
122800     MOVE WS-RESPONSE TO FILE-STATUS                              12280000
122900     IF NOT SUCCESS                                               12290000
123000        IF NO-RECORD-FND OR END-OF-FILE                           12300000
123100           CONTINUE                                               12310000
123200        ELSE                                                      12320000
123300           MOVE 'P8310-1' TO ERR-PARAGRAPH                        12330000
123400           MOVE TASKEMPK TO ERR-KEY                               12340000
123500           PERFORM P9999-GOT-PROBLEM                              12350000
123600        END-IF                                                    12360000
123700     END-IF                                                       12370000
123800     .                                                            12380000
123900                                                                  12390000
124000***************************************************************** 12400000
124100 P8320-ENDBR-TASK-FILE.                                           12410000
124200***************************************************************** 12420000
124300     EXEC CICS ENDBR                                              12430000
124400               DATASET(TASK-VIA-EMP-NBR)                          12440000
124500               RESP(WS-RESPONSE)                                  12450000
124600     END-EXEC                                                     12460000
124700     MOVE WS-RESPONSE TO FILE-STATUS                              12470000
124800     IF NOT SUCCESS                                               12480000
124900        MOVE 'P8320-1'  TO ERR-PARAGRAPH                          12490000
125000        MOVE TASKEMPK   TO ERR-KEY                                12500000
125100        PERFORM P9999-GOT-PROBLEM                                 12510000
125200     END-IF                                                       12520000
125300     .                                                            12530000
125400*                                                                 12540000
125500     COPY CNTRTXT.                                                12550000
125600*                                                                 12560000
125700     COPY TIMEZONE.                                               12570000
125800*                                                                 12580000
125900     COPY DATEEDIT.                                               12590000
126000*                                                                 12600000
126100     COPY TZERROR.                                                12610000
126200*                                                                 12620000
126300 P9000-SEND-MAP-AND-RETURN.                                       12630000
126400*                                                                 12640000
126500     MOVE REV-VIDEO TO SCR02B-TITLE-HI                            12650000
126600     MOVE WHITE     TO SCR02B-TITLE-COLOR                         12660000
126700                                                                  12670000
126800     IF MSGLOG-CODE > SPACES                                      12680000
126900         PERFORM P9030-GET-MESSAGE                                12690000
127000         MOVE MSGLOG-MESSAGE-AREA TO SCR02B-ERRORMSG              12700000
127100     END-IF                                                       12710000
127200                                                                  12720000
127300     MOVE P02B-MAP-VERSION(PSTCA-SUB) TO P02B-MAP                 12730000
127400     IF CREATE-SCREEN                                             12740000
127500        PERFORM P9010-SEND-PHYSICAL-MAP                           12750000
127600     ELSE                                                         12760000
127700        IF CONTINUE-SCREEN                                        12770000
127800           PERFORM P9020-SEND-DATAONLY-MAP                        12780000
127900        ELSE                                                      12790000
128000           PERFORM P9035-SEND-BUFFER                              12800000
128100        END-IF                                                    12810000
128200     END-IF                                                       12820000
128300     EXEC CICS RETURN                                             12830000
128400               TRANSID(P02B-TRAN)                                 12840000
128500               COMMAREA(PSTCOMM-AREA)                             12850000
128600               LENGTH(PSTCOMM-LGTH)                               12860000
128700     END-EXEC.                                                    12870000
128800*                                                                 12880000
128900 P9010-SEND-PHYSICAL-MAP.                                         12890000
129000*                                                                 12900000
129100     EXEC CICS SEND MAP(P02B-MAP)                                 12910000
129200                    MAPSET(P02B-SET)                              12920000
129300                    FROM(PSTS02B)                                 12930000
129400                    ERASE                                         12940000
129500                    RESP(WS-RESPONSE)                             12950000
129600     END-EXEC                                                     12960000
129700     MOVE WS-RESPONSE TO FILE-STATUS                              12970000
129800     IF NOT SUCCESS                                               12980000
129900        MOVE 'P9010'   TO ERR-PARAGRAPH                           12990000
130000        PERFORM P9999-GOT-PROBLEM                                 13000000
130100     END-IF.                                                      13010000
130200*                                                                 13020000
130300 P9020-SEND-DATAONLY-MAP.                                         13030000
130400*                                                                 13040000
130500     EXEC CICS SEND MAP(P02B-MAP)                                 13050000
130600                    MAPSET(P02B-SET)                              13060000
130700                    FROM(PSTS02B)                                 13070000
130800                    DATAONLY                                      13080000
130900                    RESP(WS-RESPONSE)                             13090000
131000     END-EXEC                                                     13100000
131100     MOVE WS-RESPONSE TO FILE-STATUS                              13110000
131200     IF NOT SUCCESS                                               13120000
131300        MOVE 'P9020' TO ERR-PARAGRAPH                             13130000
131400        PERFORM P9999-GOT-PROBLEM                                 13140000
131500     END-IF.                                                      13150000
131600*                                                                 13160000
131700 P9030-GET-MESSAGE.                                               13170000
131800*                                                                 13180000
131900     MOVE PSTCA-SUB TO MSGLOG-SUB-CODE                            13190000
132000     EXEC CICS READ                                               13200000
132100               DATASET(MSGLOG-VIA-CODE)                           13210000
132200               INTO(MSGLOG-AREA)                                  13220000
132300               LENGTH(MSGLOG-RLGTH)                               13230000
132400               RIDFLD(MSGLOG-KEY)                                 13240000
132500               KEYLENGTH(MSGLOG-KLGTH)                            13250000
132600               RESP(WS-RESPONSE)                                  13260000
132700     END-EXEC                                                     13270000
132800     MOVE WS-RESPONSE TO FILE-STATUS                              13280000
132900     IF NOT SUCCESS                                               13290000
133000        IF PSTCA-SUB = 1                                          13300000
133100           MOVE 'NO MESSAGE ON FILE' TO MSGLOG-MESSAGE            13310000
133200        ELSE                                                      13320000
133300           MOVE 'AUCUN MESSAGE'      TO MSGLOG-MESSAGE            13330000
133400        END-IF                                                    13340000
133500     END-IF                                                       13350000
133600     MOVE MSGLOG-CODE     TO MSGLOG-MSG-CODE                      13360000
133700     MOVE '-'             TO MSGLOG-MSG-SEP                       13370000
133800     MOVE MSGLOG-SUB-CODE TO MSGLOG-MSG-SUB-CODE.                 13380000
133900*                                                                 13390000
134000 P9035-SEND-BUFFER.                                               13400000
134100*                                                                 13410000
134200     EXEC CICS SEND                                               13420000
134300               FROM(WS-BUFFER-DATA)                               13430000
134400               LENGTH(WS-BUFFER-LGTH)                             13440000
134500               ERASE                                              13450000
134600               RESP(WS-RESPONSE)                                  13460000
134700     END-EXEC                                                     13470000
134800     MOVE WS-RESPONSE       TO FILE-STATUS                        13480000
134900     IF NOT SUCCESS                                               13490000
135000        MOVE 'P9035-1'      TO ERR-PARAGRAPH                      13500000
135100        MOVE 'SEND BUFFER'  TO ERR-KEY                            13510000
135200        PERFORM P9999-GOT-PROBLEM                                 13520000
135300     END-IF                                                       13530000
135400     EXEC CICS SEND                                               13540000
135500               CONTROL                                            13550000
135600               CURSOR(WS-BUFFER-CURSOR)                           13560000
135700               RESP(WS-RESPONSE)                                  13570000
135800     END-EXEC                                                     13580000
135900     MOVE WS-RESPONSE       TO FILE-STATUS                        13590000
136000     IF NOT SUCCESS                                               13600000
136100        MOVE 'P9035-2'      TO ERR-PARAGRAPH                      13610000
136200        MOVE 'SEND CURSOR'  TO ERR-KEY                            13620000
136300        PERFORM P9999-GOT-PROBLEM                                 13630000
136400     END-IF.                                                      13640000
136500*                                                                 13650000
136600 P9100-SETUP-SCR02.                                               13660000
136700*                                                                 13670000
136800     EXEC CICS XCTL                                               13680000
136900               PROGRAM(P02-PGM)                                   13690000
137000               COMMAREA(PSTCOMM-AREA)                             13700000
137100               LENGTH(PSTCOMM-LGTH)                               13710000
137200               RESP(WS-RESPONSE)                                  13720000
137300     END-EXEC                                                     13730000
137400     MOVE WS-RESPONSE TO FILE-STATUS                              13740000
137500     IF NOT SUCCESS                                               13750000
137600         MOVE 'P9100' TO ERR-PARAGRAPH                            13760000
137700         PERFORM P9999-GOT-PROBLEM                                13770000
137800     END-IF.                                                      13780000
137900*                                                                 13790000
138000 P9500-SETUP-SCR998.                                              13800000
138100*                                                                 13810000
138200     MOVE SPACES            TO P998COMM-AREA                      13820000
138300     MOVE P02B-PGM          TO P998CA-FROM-PROGRAM                13830000
138400     MOVE P02B-MAP          TO P998CA-SCREEN-ID                   13840000
138500     MOVE EIBCPOSN          TO P998CA-CURSOR-POS                  13850000
138600     EXEC CICS XCTL                                               13860000
138700               PROGRAM(P998-PGM)                                  13870000
138800               COMMAREA(PSTCOMM-AREA)                             13880000
138900               LENGTH(PSTCOMM-LGTH)                               13890000
139000               RESP(WS-RESPONSE)                                  13900000
139100     END-EXEC                                                     13910000
139200     MOVE WS-RESPONSE       TO FILE-STATUS                        13920000
139300     IF NOT SUCCESS                                               13930000
139400        MOVE 'P9500'        TO ERR-PARAGRAPH                      13940000
139500        PERFORM P9999-GOT-PROBLEM                                 13950000
139600     END-IF.                                                      13960000
139700*                                                                 13970000
139800 P9810-PROCESS-OFFSET.                                            13980000
139900*                                                                 13990000
140000     MOVE PSTCA-DT-OS-FUN       TO PARM-CONV-TYPE                 14000000
140100     MOVE PSTCA-DT-OS-DAYS      TO PARM-SEC-JULIAN-DAY            14010000
140200     MOVE PSTCA-DT-OS-HRMN      TO PARM-SEC-HRMN                  14020000
140300     EXEC CICS LINK                                               14030000
140400               PROGRAM(P903-PGM)                                  14040000
140500               COMMAREA(DATE-CONVERSION-PARMS)                    14050000
140600               LENGTH(P903-LGTH)                                  14060000
140700               RESP(WS-RESPONSE)                                  14070000
140800     END-EXEC                                                     14080000
140900     MOVE WS-RESPONSE           TO FILE-STATUS                    14090000
141000     IF NOT SUCCESS                                               14100000
141100        MOVE 'P9810-1'          TO ERR-PARAGRAPH                  14110000
141200        MOVE 'P903'             TO ERR-KEY                        14120000
141300        PERFORM P9999-GOT-PROBLEM                                 14130000
141400     END-IF.                                                      14140000
141500*                                                                 14150000
141600 P9990-CLEAR-SCREEN.                                              14160000
141700*                                                                 14170000
141800     EXEC CICS SEND CONTROL                                       14180000
141900                    ERASE                                         14190000
142000                    FREEKB                                        14200000
142100     END-EXEC                                                     14210000
142200     EXEC CICS RETURN END-EXEC.                                   14220000
142300*                                                                 14230000
142400 P9999-GOT-PROBLEM.                                               14240000
142500*                                                                 14250000
142600     MOVE P02B-PGM  TO ERR-PROGRAM                                14260000
142700     MOVE DFHEIBLK  TO ERR-EIBLK                                  14270000
142800     EXEC CICS LINK                                               14280000
142900               PROGRAM(PSTERR-PGM)                                14290000
143000               COMMAREA(PSTERAR-AREA)                             14300000
143100               LENGTH(PSTERAR-LGTH)                               14310000
143200               RESP(WS-RESPONSE)                                  14320000
143300     END-EXEC                                                     14330000
143400     MOVE WS-RESPONSE TO FILE-STATUS                              14340000
143500     IF NOT SUCCESS                                               14350000
143600        EXEC CICS ABEND                                           14360000
143700                  ABCODE(PSTERR-ABCODE)                           14370000
143800        END-EXEC                                                  14380000
143900     END-IF                                                       14390000
144000     EXEC CICS RETURN END-EXEC.                                   14400000
144100*                                                                 14410000
144200 X9999-GOBACK.                                                    14420000
144300     GOBACK.                                                      14430000
