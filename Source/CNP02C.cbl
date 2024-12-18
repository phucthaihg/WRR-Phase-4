000100 IDENTIFICATION DIVISION.                                         00010006
000200 PROGRAM-ID.  CNP02C.                                             00020006
000300******************************************************************00030006
000400*        R E L I E F  J O B  F I E L D  I N Q U I R Y            *00040006
000500******************************************************************00050006
000600*  DATE   INITIAL  LOG#   DESCRIPTION                             00060006
000700*-------- ------- ------  --------------------------------------- 00070006
000800*12/03/90   ERW           CICS CONVERSION.                        00080006
000900*08/05/96   RCW  CNC0090  ADD REST TO SCREEN.                     00090006
001000*                CNC0089  DON'T DISPLAY JOB OWNER IF HE IS ON A   00100006
001100*                         TEMP ASSIGNMENT.                        00110006
001200*                CNC0021/22 DISPLAY RETURN DATE FROM VACATION OR  00120006
001300*                         OFF MILES.                              00130006
001400*04/08/98   AMF           ADDED HELP LOGIC.                       00140006
001500*05/06/98   LPS           Y2K MODIFICATIONS TO THE TASK LIST.     00150006
001600*07/28/98   GBJ           YEAR 2000 SWEEP.                        00160006
001700*09/21/98   RDH   C442    ENLARGE SELECTION TO 3 DIGITS           00170006
001800*12/11/99   MOO   CNC0183 ADD CHECK FOR MTOY & MTOR.              00180006
001900*05/03/00   AJK   CNC0183 REMOVE CHECK FOR MTOY & MTOR.           00190006
002000*07/03/00   PLS   CNC0311 FOR LOCALS, INCLUDE 2 HOUR CALL TIME    00200006
002100*                         WHEN CALCULATING PERSONAL REST.         00210006
002200*10/05/04   KJS  CNC0386A ADD DAY ASSOCIATED WITH PERSONAL REST   00220006
002300*02/22/08   RXJ  CNC0454  FOR CANADIAN EMPLOYEES, LEAD TIME WILL  00230006
002400*                         NOT BE APPLIED TO THE REST EXPIRE TIME  00240006
002500*08/07/08   SXO  C780     CORRECTED THE DISPLAY OF REST TIMES -   00250006
002600*                         EMPLOYEE US REST, PERSONAL REST         00260006
002700*                         AND MANDATORY REST TIMES (WHICHEVER IS  00270006
002800*                         GREATER AMONG THEM).                    00280006
002900*09/23/08   AJK  C780     PER CN'S REQUEST, WE NOW APPLY LEAD     00290011
003000*                         TIME TO YARD EMPLOYEES' PERSONAL REST   00300011
003100*                         AVAILIBILITY TIMES, NOT JUST LOCALS.    00310011
003200*02/10/09   JES  CNC0436B RECOMPILE ONLY FOR WSTASK EXPANSION.    00320013
003210*08/13/13   AXK  CNC0531   == RECOMPILE ONLY ==                   00321014
003220*                         ADDED YARD CRAFTS Y0-Y9 AND YT          00322014
003300***************************************************************** 00330006
003400 ENVIRONMENT DIVISION.                                            00340006
003500 CONFIGURATION SECTION.                                           00350006
003600 SOURCE-COMPUTER.  IBM-9370.                                      00360006
003700 OBJECT-COMPUTER.  IBM-9370.                                      00370006
003800 DATA DIVISION.                                                   00380006
003900 WORKING-STORAGE SECTION.                                         00390006
004000 01  FILLER                  PIC X(10)  VALUE 'PGM02C W/S'.       00400006
004100*01  WS-ABSTIME              PIC S9(15) COMP-3 VALUE +0.          00410006
004200                                                                  00420006
004300 01  WS-SUBSCRIPTS.                                               00430006
004400     02  I                   PIC S9(4)  COMP VALUE +0.            00440006
004500     02  J                   PIC S9(4)  COMP VALUE +0.            00450006
004600     02  K                   PIC S9(4)  COMP VALUE +0.            00460006
004700     02  SUB1                PIC S9(4)  COMP VALUE +0.            00470006
004800     02  SET-NUM             PIC S9(4)  COMP VALUE +0.            00480006
004900     02  ROW-NUM             PIC S9(4)  COMP VALUE +0.            00490006
005000                                                                  00500006
005100 01  WS-FLAGS.                                                    00510006
005200     02  SCREEN-FLAG         PIC X      VALUE '0'.                00520006
005300         88  CONTINUE-SCREEN            VALUE '0'.                00530006
005400         88  CREATE-SCREEN              VALUE '1'.                00540006
005500         88  SEND-BUFFER                VALUE '2'.                00550006
005600     02  DONE-CODE           PIC  X     VALUE 'N'.                00560006
005700         88  DONE                       VALUE 'Y'.                00570006
005800         88  NOT-DONE                   VALUE 'N'.                00580006
005900     02  SCH-DONE-CODE       PIC  X     VALUE 'N'.                00590006
006000         88  SCH-DONE                   VALUE 'Y'.                00600006
006100         88  SCH-NOT-DONE               VALUE 'N'.                00610006
006200     02  WS-TASK-DONE-CODE   PIC  X     VALUE 'N'.                00620006
006300         88  TASK-NOT-DONE              VALUE 'N'.                00630006
006400         88  TASK-DONE                  VALUE 'Y'.                00640006
006500     02  JOB-QUAL-CODE       PIC  X     VALUE 'N'.                00650006
006600         88  JOB-QUALIFIES              VALUE 'Y'.                00660006
006700         88  JOB-NOT-QUALIFIED          VALUE 'N'.                00670006
006800     02  WS-APPLY-LEAD-TIME-FLAG     PIC X(001) VALUE 'Y'.        00680006
006900         88  APPLY-LEAD-TIME                    VALUE 'Y'.        00690006
007000         88  DONT-APPLY-LEAD-TIME               VALUE 'N'.        00700006
007100     02  WS-COMPANY-CODE-FLAG        PIC X(001) VALUE ' '.        00710008
007200         88  WS-CANADIAN-COMPANY                VALUE 'C'.        00720008
007300         88  WS-US-COMPANY                      VALUE 'U'.        00730008
007400                                                                  00740006
007500 01  WS-MISCELLANEOUS.                                            00750006
007600*    05 WS-DATE-TIME.                                             00760006
007700*       10  WS-DATE.                                              00770006
007800*           15  WS-YR         PIC 99.                             00780006
007900*           15  WS-MO         PIC 99.                             00790006
008000*           15  WS-DY         PIC 99.                             00800006
008100*       10  WS-TIME.                                              00810006
008200*           15  WS-HRMN.                                          00820006
008300*               20  WS-HR     PIC 99.                             00830006
008400*               20  WS-MN     PIC 99.                             00840006
008500*            15 FILLER        PIC 9(4).                           00850006
008600     05 WS-DAY                PIC 9(1).                           00860006
008700     05 WS-WORK-DATE-TIME     PIC 9(10).                          00870006
008800     05 WS-WORK-DATE-TIME-C REDEFINES WS-WORK-DATE-TIME PIC X(10).00880006
008900                                                                  00890006
009000******************************************************************00900006
009100***                  TEMPORARY STORAGE QUEUE                   ***00910006
009200******************************************************************00920006
009300 01  P02CTSQ-QUEUE-ITEM         PIC S9(4)  COMP VALUE +1.         00930006
009400 01  P02CTSQ-MAP-QUEUE-ID.                                        00940006
009500     05  P02CTSQ-MAP-QUEUE      PIC X(4)   VALUE '02CM'.          00950006
009600     05  P02CTSQ-MAP-TERM-ID    PIC X(4)   VALUE SPACES.          00960006
009700 01  P02CTSQ-CA-QUEUE-ID.                                         00970006
009800     05  P02CTSQ-CA-QUEUE       PIC X(4)   VALUE '02CC'.          00980006
009900     05  P02CTSQ-CA-TERM-ID     PIC X(4)   VALUE SPACES.          00990006
010000 01  P02CTSQ-QLGTH              PIC S9(4)  COMP VALUE +1.         01000006
010100***************************************************************** 01010006
010200***                 I/O STATUS CHECK FIELDS                       01020006
010300***************************************************************** 01030006
010400 01  WS-RESPONSE              PIC S9(8) COMP VALUE ZEROES.        01040006
010500 01  FILE-STATUS              PIC 9(4)  VALUE ZEROES.             01050006
010600     COPY IOCODES.                                                01060006
010700***************************************************************** 01070006
010800***                      COMMAREA COPYBOOKS                       01080006
010900***************************************************************** 01090006
011000     COPY PSTCOMM.                                                01100006
011100     COPY P998COMM.                                               01110006
011200     COPY FICOMM.                                                 01120006
011300***************************************************************** 01130006
011400***                     MAP AREA COPYBOOK                         01140006
011500***************************************************************** 01150006
011600     COPY PSM02CRE.                                               01160006
011700***************************************************************** 01170006
011800***                   PROGRAM NAMES COPYBOOKS                     01180006
011900***************************************************************** 01190006
012000     COPY PSTCB02.                                                01200006
012100     COPY PSTCB02C.                                               01210006
012200     COPY PSTCB998.                                               01220006
012300***************************************************************** 01230006
012400***                 CALLED ROUTINES COPYBOOKS.                    01240006
012500***************************************************************** 01250006
012600     COPY PSTERAR.                                                01260006
012700     COPY P901COMM.                                               01270006
012800     COPY P903COMM.                                               01280006
012900***************************************************************** 01290006
013000***                     FILE COPYBOOKS                            01300006
013100***************************************************************** 01310006
013200     COPY WSMSTR.                                                 01320006
013300     COPY WSASGN.                                                 01330006
013400     COPY WSSWASGN.                                               01340006
013500     COPY WSFICT.                                                 01350006
013600     COPY WSJS.                                                   01360006
013700     COPY WSAJ.                                                   01370006
013800     COPY WSTRCN.                                                 01380006
013900     COPY WSCNTL.                                                 01390006
014000     COPY WSTASK.                                                 01400006
014100***************************************************************** 01410006
014200***                     MISC. COPYBOOKS                           01420006
014300***************************************************************** 01430006
014400     COPY PSTKEYS.                                                01440006
014500     COPY PSTATTR.                                                01450006
014600     COPY WSCENTER.                                               01460006
014700     COPY WSMSG.                                                  01470006
014800     COPY WSDAYWK.                                                01480006
014900     COPY WSAJDEFN.                                               01490006
015000     COPY PSTCCRFT.                                               01500006
015100     COPY WSZONE.                                                 01510006
015200     COPY WSEDDATE.                                               01520006
015300     COPY WSSYDTTM.                                               01530006
015400     COPY WSBUFFER.                                               01540006
015500*                                                                 01550006
015600 LINKAGE SECTION.                                                 01560006
015700*                                                                 01570006
015800 01  DFHCOMMAREA.                                                 01580006
015900     05  FILLER                  PIC X(170).                      01590006
016000*                                                                 01600006
016100 PROCEDURE DIVISION.                                              01610006
016200*                                                                 01620006
016300 P0000-MAINLINE.                                                  01630006
016400*                                                                 01640006
016500     EXEC CICS IGNORE CONDITION                                   01650006
016600               ERROR                                              01660006
016700     END-EXEC                                                     01670006
016800     EXEC CICS HANDLE ABEND                                       01680006
016900               LABEL(P9999-GOT-PROBLEM)                           01690006
017000     END-EXEC                                                     01700006
017100     COPY ABSTIME.                                                01710006
017200     IF EIBCALEN = ZERO                                           01720006
017300        PERFORM P9990-CLEAR-SCREEN                                01730006
017400     END-IF                                                       01740006
017500     MOVE DFHCOMMAREA TO  PSTCOMM-AREA                            01750006
017600     IF EIBTRNID NOT = P02C-TRAN                                  01760006
017700        SET CREATE-SCREEN TO TRUE                                 01770006
017800        MOVE LOW-VALUES   TO PSTS02C                              01780006
017900        IF EIBTRNID = P998-TRAN                                   01790006
018000           MOVE P998CA-CURSOR-POS TO EIBCPOSN                     01800006
018100           PERFORM P7010-READ-TSQUEUE                             01810006
018200           PERFORM P9000-SEND-MAP-AND-RETURN                      01820006
018300        END-IF                                                    01830006
018400        PERFORM P0200-GET-DATES                                   01840006
018500        MOVE SPACES       TO FICA-NEXT-JOB                        01850006
018600        PERFORM P4000-BUILD-SCREEN                                01860006
018700        PERFORM P9000-SEND-MAP-AND-RETURN                         01870006
018800     END-IF                                                       01880006
018900     MOVE EIBAID TO PF-CHECK                                      01890006
019000     IF PFKEY11                                                   01900006
019100        SET PSTCA-FLD-RET-PFKEY11  TO TRUE                        01910006
019200     END-IF                                                       01920006
019300     IF EXIT-KEY OR PFKEY11                                       01930006
019400        PERFORM P9100-SETUP-SCR02                                 01940006
019500     END-IF                                                       01950006
019600     PERFORM P0100-PROCESS-INPUT                                  01960006
019700     PERFORM P9000-SEND-MAP-AND-RETURN.                           01970006
019800*                                                                 01980006
019900 P0100-PROCESS-INPUT.                                             01990006
020000*                                                                 02000006
020100     MOVE P02C-MAP-VERSION(PSTCA-SUB) TO P02C-MAP                 02010006
020200     EXEC CICS RECEIVE MAP(P02C-MAP)                              02020006
020300                       MAPSET(P02C-SET)                           02030006
020400                       INTO(PSTS02C)                              02040006
020500                       RESP(WS-RESPONSE)                          02050006
020600     END-EXEC                                                     02060006
020700     MOVE WS-RESPONSE TO FILE-STATUS                              02070006
020800     IF NOT SUCCESS                                               02080006
020900        MOVE 'P0100'   TO ERR-PARAGRAPH                           02090006
021000        PERFORM P9999-GOT-PROBLEM                                 02100006
021100     END-IF                                                       02110006
021200     IF PFKEY1                                                    02120006
021300        PERFORM P7000-WRITE-TSQUEUE                               02130006
021400        PERFORM P9500-SETUP-SCR998                                02140006
021500     END-IF                                                       02150006
021600                                                                  02160006
021700     PERFORM P0200-GET-DATES                                      02170006
021800                                                                  02180006
021900     IF ENTER-KEY                                                 02190006
022000        MOVE SPACES        TO FICA-NEXT-JOB                       02200006
022100     ELSE                                                         02210006
022200        IF NOT PFKEY8                                             02220006
022300*               INVALID-FUNC-MSG                                  02230006
022400           MOVE 'I006' TO MSGLOG-CODE                             02240006
022500           PERFORM P9000-SEND-MAP-AND-RETURN                      02250006
022600        END-IF                                                    02260006
022700     END-IF                                                       02270006
022800     PERFORM P4000-BUILD-SCREEN.                                  02280006
022900                                                                  02290006
023000*                                                                 02300006
023100 P0200-GET-DATES.                                                 02310006
023200*                                                                 02320006
023300     EXEC CICS ASKTIME                                            02330006
023400               ABSTIME(WS-ABSTIME)                                02340006
023500     END-EXEC                                                     02350006
023600     ADD WS-ABSTIME-OFFSET TO WS-ABSTIME                          02360006
023700     EXEC CICS FORMATTIME                                         02370006
023800               ABSTIME(WS-ABSTIME)                                02380006
023900               YYYYMMDD(WS-SYSTEM-DATE-CENT)                      02390006
024000               TIME(WS-SYSTEM-TIME-AREA)                          02400006
024100     END-EXEC                                                     02410006
024200*                                                                 02420006
024300*    INSTALL APPLICATION DATE/TIME                                02430006
024400*                                                                 02440006
024500     IF PSTCA-DATE-TIME-OFFSET > SPACES                           02450006
024600        MOVE ZEROS                  TO DATE-CONVERSION-PARMS      02460006
024700        MOVE WS-SYSTEM-DATE         TO PARM-PRI-DATE-GREG         02470006
024800        MOVE WS-SYSTEM-TIME         TO PARM-PRI-HRMN              02480006
024900        PERFORM P9810-PROCESS-OFFSET                              02490006
025000        MOVE PARM-RES-DATE-GREG     TO WS-SYSTEM-DATE             02500006
025100        MOVE PARM-RES-HRMN          TO WS-SYSTEM-TIME             02510006
025200        MOVE PARM-RES-DAY-OF-WEEK   TO WS-DAY                     02520006
025300     ELSE                                                         02530006
025400        MOVE ZEROS                  TO DATE-CONVERSION-PARMS      02540006
025500        MOVE WS-SYSTEM-DATE         TO PARM-PRI-DATE-GREG         02550006
025600        SET PARM-CONV               TO TRUE                       02560006
025700        EXEC CICS LINK                                            02570006
025800                  PROGRAM(P903-PGM)                               02580006
025900                  COMMAREA(DATE-CONVERSION-PARMS)                 02590006
026000                  LENGTH(P903-LGTH)                               02600006
026100                  RESP(WS-RESPONSE)                               02610006
026200        END-EXEC                                                  02620006
026300        IF NOT SUCCESS                                            02630006
026400           MOVE 'P0200' TO ERR-PARAGRAPH                          02640006
026500           PERFORM P9999-GOT-PROBLEM                              02650006
026600        END-IF                                                    02660006
026700        MOVE PARM-PRI-DAY-OF-WEEK   TO WS-DAY                     02670006
026800     END-IF                                                       02680006
026900     SET DE-YYMMDD-FORMAT           TO TRUE                       02690006
027000     MOVE WS-SYSTEM-DATE            TO DE-YYMMDD                  02700006
027100     PERFORM P8998-DATEEDIT                                       02710006
027200     MOVE DE-YYMMDD-CE              TO WS-SYSTEM-CENT.            02720006
027300*                                                                 02730006
027400 P4000-BUILD-SCREEN.                                              02740006
027500*                                                                 02750006
027600     MOVE FICA-FICT-RECORD-KEY      TO FICTKEY                    02760006
027700     EXEC CICS READ                                               02770006
027800               DATASET(FICT-VIA-LOC-SEQ)                          02780006
027900               INTO(WS-FICT)                                      02790006
028000               LENGTH(FICTLOSQ-RLGTH)                             02800006
028100               RIDFLD(FICTKEY)                                    02810006
028200               KEYLENGTH(FICTLOSQ-KLGTH)                          02820006
028300               RESP(WS-RESPONSE)                                  02830006
028400     END-EXEC                                                     02840006
028500     MOVE WS-RESPONSE TO FILE-STATUS                              02850006
028600     IF NOT SUCCESS                                               02860006
028700        MOVE 'P4000-1'            TO ERR-PARAGRAPH                02870006
028800        MOVE FICTKEY              TO ERR-KEY                      02880006
028900        PERFORM P9999-GOT-PROBLEM                                 02890006
029000     END-IF                                                       02900006
029100     MOVE FICT-JOB-DIST             TO SCR02C-DIST                02910006
029200     MOVE FICT-JOB-SUB-DIST         TO SCR02C-SUB-DIST            02920006
029300     MOVE FICT-DESC(PSTCA-SUB)      TO CTXT-UNF-FIELD             02930006
029400     MOVE 30 TO CTXT-UNF-FIELD-LEN                                02940006
029500     PERFORM P8994-CENTER-TEXT                                    02950006
029600     MOVE CTXT-FOR-FIELD            TO SCR02C-TITLE               02960006
029700     PERFORM VARYING I FROM 1 BY 1                                02970006
029800             UNTIL I > 2                                          02980006
029900       MOVE SPACES                  TO SCR02C-JOB(I)              02990006
030000                                       SCR02C-JOB-DESC(I)         03000006
030100       PERFORM VARYING J FROM 1 BY 1                              03010006
030200               UNTIL J > 7                                        03020006
030300         MOVE LOW-VALUES            TO SCR02C-EMP-NAME-HI(I J)    03030006
030400         MOVE SPACES                TO SCR02C-EMP-NAME(I J)       03040006
030500                                       SCR02C-CRAFT(I J)          03050006
030600                                       SCR02C-STATUS(I J)         03060006
030700                                       SCR02C-REST-TIME(I J)      03070006
030800                                       SCR02C-REST-DY(I J)        03080006
030900                                       SCR02C-RETURN-DATE(I J)    03090006
031000         PERFORM VARYING K FROM 1 BY 1                            03100006
031100                 UNTIL K > 2                                      03110006
031200            MOVE SPACES             TO SCR02C-ASGN-TIME(I J K)    03120006
031300                                       SCR02C-ASGN(I J K)         03130006
031400         END-PERFORM                                              03140006
031500       END-PERFORM                                                03150006
031600     END-PERFORM                                                  03160006
031700     MOVE SPACES                    TO SCR02C-ERRORMSG            03170006
031800     IF FICT-JOB-TYPE = 'X'                                       03180006
031900        PERFORM P8000-START-TRCNKEY3                              03190006
032000        MOVE 1                      TO SET-NUM                    03200006
032100        PERFORM UNTIL DONE OR SET-NUM > 2                         03210006
032200           PERFORM P8100-READNEXT-TRCNKEY3                        03220006
032300           IF JOB-QUALIFIES                                       03230006
032400              PERFORM P4010-SETUP-NEW-JOB                         03240006
032500           END-IF                                                 03250006
032600        END-PERFORM                                               03260006
032700        EXEC CICS ENDBR                                           03270006
032800                  DATASET(TRAIN-CN-VIA-DSD-ASGN)                  03280006
032900                  RESP(WS-RESPONSE)                               03290006
033000        END-EXEC                                                  03300006
033100     ELSE                                                         03310006
033200        SET DONE                    TO TRUE                       03320006
033300     END-IF                                                       03330006
033400     IF DONE                                                      03340006
033500*            'END OF JOB LIST'                                    03350006
033600        MOVE 'E008' TO MSGLOG-CODE                                03360006
033700        MOVE SPACES                 TO FICA-NEXT-JOB              03370006
033800     ELSE                                                         03380006
033900        MOVE TRCN-ASSIGNMENT        TO FICA-NEXT-JOB              03390006
034000     END-IF.                                                      03400006
034100*                                                                 03410006
034200 P4010-SETUP-NEW-JOB.                                             03420006
034300*                                                                 03430006
034400     MOVE ZEROS                     TO ROW-NUM                    03440006
034500     PERFORM VARYING I FROM 1 BY 1                                03450006
034600        UNTIL I > NUMBER-OF-JOB-TYPES                             03460006
034700        IF JOB-DEF-TBL-CLASS(I) = JOB-DEF-CLASS                   03470006
034800           PERFORM VARYING J FROM 1 BY 1                          03480006
034900              UNTIL J > 7                                         03490006
035000              IF JOB-DEF-TBL-CRAFT-CODE(I, J) > SPACE             03500006
035100                 MOVE TRCN-KEY3     TO AJJOBKEY-AREA              03510006
035200                 MOVE JOB-DEF-TBL-CRAFT-CODE(I, J)                03520006
035300                                    TO AJ-JOB-ASGN-CC             03530006
035400                 PERFORM P8200-READ-AJJOBKEY                      03540006
035500                 IF SUCCESS                                       03550006
035600                     SET DE-YYMMDD-FORMAT      TO TRUE            03560006
035700                     MOVE AJ-EFF-DATE          TO DE-YYMMDD       03570006
035800                     PERFORM P8998-DATEEDIT                       03580006
035900                     MOVE DE-CCYYMMDD          TO DE-COMPARE1-DATE03590006
036000*                    IF AJ-EFF-DATE NOT > WS-DATE                 03600006
036100                     IF DE-COMPARE1-DATE NOT > WS-SYSTEM-DATE-CENT03610006
036200                       PERFORM P4100-MOVE-JOB-TO-SCREEN           03620006
036300                    END-IF                                        03630006
036400                 ELSE                                             03640006
036500                    IF NOT NO-RECORD-FND                          03650006
036600                       MOVE 'P4010-1'                             03660006
036700                                    TO ERR-PARAGRAPH              03670006
036800                       MOVE AJJOBKEY                              03680006
036900                                    TO ERR-KEY                    03690006
037000                       PERFORM P9999-GOT-PROBLEM                  03700006
037100                    END-IF                                        03710006
037200                 END-IF                                           03720006
037300              END-IF                                              03730006
037400           END-PERFORM                                            03740006
037500           MOVE NUMBER-OF-JOB-TYPES TO I                          03750006
037600        END-IF                                                    03760006
037700     END-PERFORM                                                  03770006
037800     IF ROW-NUM > ZERO                                            03780006
037900        ADD 1                       TO SET-NUM                    03790006
038000     END-IF.                                                      03800006
038100*                                                                 03810006
038200 P4100-MOVE-JOB-TO-SCREEN.                                        03820006
038300*                                                                 03830006
038400     ADD 1                          TO ROW-NUM                    03840006
038500     IF SCR02C-JOB(SET-NUM) NOT > SPACES                          03850006
038600        MOVE AJ-JOB-ASGN-ID         TO SCR02C-JOB(SET-NUM)        03860006
038700        MOVE TRCN-DESCRIPTION       TO SCR02C-JOB-DESC(SET-NUM)   03870006
038800     END-IF                                                       03880006
038900     MOVE AJ-JOB-ASGN-CC            TO                            03890006
039000                              SCR02C-CRAFT(SET-NUM, ROW-NUM)      03900006
039100*                                                                 03910006
039200     PERFORM P4120-EMPLOYEE-INFO                                  03920006
039300*                                                                 03930006
039400*    LOAD UP THE CURRENT SCHEDULE FOR THIS ASSIGNMENT             03940006
039500*                                                                 03950006
039600     IF ROW-NUM = 01                                              03960006
039700        MOVE AJJOBKEY-AREA          TO WORK-JS-KEY1               03970006
039800        MOVE WS-SYSTEM-DATE         TO WK-JSK1-EXP-DATE           03980006
039900        MOVE WORK-JS-KEY1           TO JSKEY1                     03990006
040000        EXEC CICS READ                                            04000006
040100                  DATASET(JS-VIA-JSKEY1)                          04010006
040200                  INTO(WS-JOB-SCHEDULE)                           04020006
040300                  LENGTH(JSKEY1-RLGTH)                            04030006
040400                  RIDFLD(JSKEY1)                                  04040006
040500                  KEYLENGTH(JSKEY1-KLGTH)                         04050006
040600                  GTEQ                                            04060006
040700                  RESP(WS-RESPONSE)                               04070006
040800        END-EXEC                                                  04080006
040900        MOVE WS-RESPONSE            TO FILE-STATUS                04090006
041000        IF SUCCESS                                                04100006
041100           IF JSK1-ASGN-DIST = WK-JSK1-ASGN-DIST                  04110006
041200              AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST      04120006
041300              AND JSK1-ASSIGNMENT = WK-JSK1-ASSIGNMENT            04130006
041400              AND JSK1-ASGN-STATS                                 04140006
041500              CONTINUE                                            04150006
041600           ELSE                                                   04160006
041700              SET NO-RECORD-FND     TO TRUE                       04170006
041800           END-IF                                                 04180006
041900        END-IF                                                    04190006
042000        IF NOT SUCCESS                                            04200006
042100           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  04210006
042200              MOVE 'P4100-1'        TO ERR-PARAGRAPH              04220006
042300              MOVE JSKEY1           TO ERR-KEY                    04230006
042400              PERFORM P9999-GOT-PROBLEM                           04240006
042500           END-IF                                                 04250006
042600        END-IF                                                    04260006
042700        IF SUCCESS                                                04270006
042800           MOVE JS-KEY1             TO WORK-JS-KEY1               04280006
042900           MOVE 001                 TO WK-JSK1-ASGN-DAY-NUM       04290006
043000           SET SCH-NOT-DONE         TO TRUE                       04300006
043100           MOVE ZEROS               TO K                          04310006
043200           PERFORM UNTIL SCH-DONE                                 04320006
043300              MOVE WORK-JS-KEY1     TO JSKEY1                     04330006
043400              EXEC CICS READ                                      04340006
043500                        DATASET(JS-VIA-JSKEY1)                    04350006
043600                        INTO(WS-JOB-SCHEDULE)                     04360006
043700                        LENGTH(JSKEY1-RLGTH)                      04370006
043800                        RIDFLD(JSKEY1)                            04380006
043900                        KEYLENGTH(JSKEY1-KLGTH)                   04390006
044000                        GTEQ                                      04400006
044100                        RESP(WS-RESPONSE)                         04410006
044200              END-EXEC                                            04420006
044300              MOVE WS-RESPONSE      TO FILE-STATUS                04430006
044400              IF SUCCESS                                          04440006
044500                 IF JSK1-ASGN-DIST = WK-JSK1-ASGN-DIST            04450006
044600                    AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST04460006
044700                    AND JSK1-ASSIGNMENT = WK-JSK1-ASSIGNMENT      04470006
044800                    AND JSK1-EXP-DATE = WK-JSK1-EXP-DATE          04480006
044900                    IF JSK1-ASGN-DAY NOT = WK-JSK1-ASGN-DAY       04490006
045000                       MOVE ZEROS      TO K                       04500006
045100                    END-IF                                        04510006
045200                    MOVE JSK1-ASGN-DAY-NUM                        04520006
045300                                       TO SUB1                    04530006
045400                    IF K < 2                                      04540006
045500                       ADD 1           TO K                       04550006
045600                    END-IF                                        04560006
045700                    MOVE JSK1-ASGN-START-TIME TO                  04570006
045800                               SCR02C-ASGN-TIME(SET-NUM, SUB1, K) 04580006
045900                    IF JOB-SCHED-ASGN-OFF                         04590006
046000                       OR JOB-SCHED-ASGN-XTRA                     04600006
046100                       MOVE JOB-SCHED-SPEC-COND TO                04610006
046200                               SCR02C-ASGN(SET-NUM, SUB1, K)      04620006
046300                    ELSE                                          04630006
046400                       MOVE JSK2-ASSIGNMENT     TO                04640006
046500                               SCR02C-ASGN(SET-NUM, SUB1, K)      04650006
046600                    END-IF                                        04660006
046700                    MOVE JS-KEY1       TO WORK-JS-KEY1            04670006
046800                    ADD 1              TO                         04680006
046900                                       WK-JSK1-ASGN-START-TIME-NUM04690006
047000                 ELSE                                             04700006
047100                    SET SCH-DONE       TO TRUE                    04710006
047200                 END-IF                                           04720006
047300              ELSE                                                04730006
047400                 SET SCH-DONE          TO TRUE                    04740006
047500                 IF NOT (NO-RECORD-FND OR END-OF-FILE)            04750006
047600                    MOVE 'P4100-2'     TO ERR-PARAGRAPH           04760006
047700                    MOVE JSKEY1        TO ERR-KEY                 04770006
047800                    PERFORM P9999-GOT-PROBLEM                     04780006
047900                 END-IF                                           04790006
048000              END-IF                                              04800006
048100           END-PERFORM                                            04810006
048200        END-IF                                                    04820006
048300     END-IF.                                                      04830006
048400*                                                                 04840006
048500 P4120-EMPLOYEE-INFO.                                             04850006
048600*                                                                 04860006
048700     SET ASGN-AJ-JOB OF ASGN-JOB-TYPE                             04870006
048800                                    TO TRUE                       04880006
048900     MOVE AJ-JOB-DIST               TO ASGN-DIST                  04890006
049000     MOVE AJ-JOB-SUB-DIST           TO ASGN-SUB-DIST              04900006
049100     MOVE AJ-JOB-ASSIGNMENT         TO                            04910006
049200                                   ASGN-AJ-JOB OF ASGN-ASSIGNMENT 04920006
049300     MOVE WS-ASGN-FILE              TO WS-ASSIGN-RECORD           04930006
049400     IF FICT-EMPLOYEE-SELECTION = 'O'                             04940006
049500        SET GET-JOB-RECORD          TO TRUE                       04950006
049600     END-IF                                                       04960006
049700     IF FICT-EMPLOYEE-SELECTION = 'T'                             04970006
049800        SET GET-LATEST-TEMP-EMP     TO TRUE                       04980006
049900     END-IF                                                       04990006
050000     EXEC CICS LINK                                               05000006
050100               PROGRAM(P901-PGM)                                  05010006
050200               COMMAREA(WS-ASSIGN-COMMAREA)                       05020006
050300               LENGTH(P901-LGTH)                                  05030006
050400               RESP(WS-RESPONSE)                                  05040006
050500     END-EXEC                                                     05050006
050600     MOVE WS-RESPONSE               TO FILE-STATUS                05060006
050700     IF NOT SUCCESS                                               05070006
050800        MOVE 'P4120-1'              TO ERR-PARAGRAPH              05080006
050900        PERFORM P9999-GOT-PROBLEM                                 05090006
051000     END-IF                                                       05100006
051100     MOVE WS-ASSIGN-RECORD          TO WS-ASGN-FILE               05110006
051200     IF ASGN-EMP-NO = 0 AND GET-LATEST-TEMP-EMP                   05120006
051300        SET ASGN-AJ-JOB OF ASGN-JOB-TYPE                          05130006
051400                                    TO TRUE                       05140006
051500        MOVE AJ-JOB-DIST            TO ASGN-DIST                  05150006
051600        MOVE AJ-JOB-SUB-DIST        TO ASGN-SUB-DIST              05160006
051700        MOVE AJ-JOB-ASSIGNMENT      TO                            05170006
051800                                  ASGN-AJ-JOB OF ASGN-ASSIGNMENT  05180006
051900        MOVE WS-ASGN-FILE           TO WS-ASSIGN-RECORD           05190006
052000        SET GET-JOB-RECORD          TO TRUE                       05200006
052100        EXEC CICS LINK                                            05210006
052200                  PROGRAM(P901-PGM)                               05220006
052300                  COMMAREA(WS-ASSIGN-COMMAREA)                    05230006
052400                  LENGTH(P901-LGTH)                               05240006
052500                  RESP(WS-RESPONSE)                               05250006
052600        END-EXEC                                                  05260006
052700        MOVE WS-RESPONSE            TO FILE-STATUS                05270006
052800        IF NOT SUCCESS                                            05280006
052900           MOVE 'P4120-2'           TO ERR-PARAGRAPH              05290006
053000           PERFORM P9999-GOT-PROBLEM                              05300006
053100        END-IF                                                    05310006
053200        MOVE WS-ASSIGN-RECORD       TO WS-ASGN-FILE               05320006
053300***  LIST PERMANENT ASSIGNMENTS AS 'OPEN' IF THE                  05330006
053400***  EMPLOYEE IS ON A TEMPORARY ASSIGNMENT.   CNC0089/CW0896      05340006
053500***                                                               05350006
053600        IF ASGN-EMP-NO > ZERO                                     05360006
053700           PERFORM P4150-GET-EMPS-LATEST-TEMP                     05370006
053800           IF WS-ASSIGN-RECORD > SPACES                           05380006
053900              MOVE ZERO             TO ASGN-EMP-NO                05390006
054000           END-IF                                                 05400006
054100        END-IF                                                    05410006
054200     END-IF                                                       05420006
054300     MOVE SCR02C-CRAFT(SET-NUM, ROW-NUM)                          05430006
054400                                    TO WS-CRAFT-CODE-CHECK        05440006
054500     IF ASGN-EMP-NO NOT > ZEROES                                  05450006
054600        AND YARD-CRAFT                                            05460006
054700        MOVE AJ-JOB-DIST            TO SWASSGN-K-DISTRICT         05470006
054800        MOVE AJ-JOB-SUB-DIST        TO SWASSGN-K-SUB-DIST         05480006
054900        MOVE WS-DAY                 TO SWASSGN-K-DAY              05490006
055000        MOVE AJ-JOB-ASSIGNMENT      TO SWASSGN-K-ASSIGN           05500006
055100        MOVE SWASSGN-KEY            TO SWJOBKEY                   05510006
055200        EXEC CICS READ                                            05520006
055300                  DATASET(SWASSGN-VIA-ASSIGNMENT)                 05530006
055400                  INTO(WS-SWASSGN-FILE)                           05540006
055500                  LENGTH(SWASSIGN-RLGTH)                          05550006
055600                  RIDFLD(SWJOBKEY)                                05560006
055700                  KEYLENGTH(SWASSIGN-KLGTH)                       05570006
055800                  RESP(WS-RESPONSE)                               05580006
055900        END-EXEC                                                  05590006
056000        MOVE WS-RESPONSE            TO FILE-STATUS                05600006
056100        IF SUCCESS                                                05610006
056200           MOVE SWASSGN-EMP-NO      TO ASGN-EMP-NO                05620006
056300        END-IF                                                    05630006
056400     END-IF                                                       05640006
056500     MOVE DEFAULT-ATTR              TO                            05650006
056600                              SCR02C-EMP-NAME-HI(SET-NUM ROW-NUM) 05660006
056700     IF ASGN-EMP-NO NOT > ZEROES                                  05670006
056800        MOVE AJ-OPEN-ASGN-MSG(PSTCA-SUB)                          05680006
056900                              TO SCR02C-EMP-NAME(SET-NUM ROW-NUM) 05690006
057000     ELSE                                                         05700006
057100        MOVE ASGN-EMP-NO            TO MSTRNBRK                   05710006
057200        EXEC CICS READ                                            05720006
057300                  DATASET(MSTR-VIA-EMP-NBR)                       05730006
057400                  INTO(WS-MSTR)                                   05740006
057500                  LENGTH(MSTRENBR-RLGTH)                          05750006
057600                  RIDFLD(MSTRNBRK)                                05760006
057700                  KEYLENGTH(MSTRENBR-KLGTH)                       05770006
057800                  RESP(WS-RESPONSE)                               05780006
057900        END-EXEC                                                  05790006
058000        MOVE WS-RESPONSE            TO FILE-STATUS                05800006
058100        IF SUCCESS                                                05810006
058200           PERFORM P4130-MOVE-EMPLOYEE-INFO                       05820006
058300        END-IF                                                    05830006
058400     END-IF                                                       05840006
058500     .                                                            05850006
058600*                                                                 05860006
058700 P4130-MOVE-EMPLOYEE-INFO.                                        05870006
058800*                                                                 05880006
058900     INITIALIZE WS-WORK-DATE-TIME                                 05890006
059000*                                                                 05900006
059100     PERFORM P5200-CHECK-COMPANY-CD                               05910010
059200     IF EMP-PERS-REST-NUM NUMERIC                                 05920011
059300        AND EMP-PERS-REST-NUM            > ZEROES                 05930012
059400        IF APPLY-LEAD-TIME                                        05940011
059500           MOVE ZEROES                  TO DATE-CONVERSION-PARMS  05950012
059600           SET PARM-ADD                 TO TRUE                   05960012
059700           MOVE EMP-PERS-REST-DATE      TO PARM-PRI-DATE-GREG     05970012
059800           MOVE EMP-PERS-REST-TIME      TO PARM-PRI-HRMN          05980012
059900           MOVE '0200'                  TO PARM-SEC-HRMN          05990012
060000           EXEC CICS LINK                                         06000011
060100                     PROGRAM(P903-PGM)                            06010011
060200                     COMMAREA(DATE-CONVERSION-PARMS)              06020011
060300                     LENGTH(P903-LGTH)                            06030011
060400                     RESP(WS-RESPONSE)                            06040011
060500           END-EXEC                                               06050011
060600           MOVE WS-RESPONSE             TO FILE-STATUS            06060012
060700           IF NOT SUCCESS                                         06070011
060800              MOVE 'P4130-1'            TO ERR-PARAGRAPH          06080012
060900              MOVE 'P903'               TO ERR-KEY                06090012
061000              PERFORM P9999-GOT-PROBLEM                           06100011
061100           END-IF                                                 06110011
061200           MOVE PARM-RES-DATE-GREG      TO DE-COMPARE1-YYMMDD     06120012
061300           MOVE PARM-RES-GREG-CENT      TO DE-COMPARE1-CE         06130012
061400           MOVE PARM-RES-HRMN           TO DE-COMPARE1-TIME       06140012
061500        ELSE                                                      06150011
061600           SET DE-YYMMDD-FORMAT         TO TRUE                   06160012
061700           MOVE EMP-PERS-REST-DATE      TO DE-YYMMDD              06170012
061800           PERFORM P8998-DATEEDIT                                 06180011
061900           MOVE DE-CCYYMMDD             TO DE-COMPARE1-DATE       06190012
062000           MOVE EMP-PERS-REST-TIME      TO DE-COMPARE1-TIME       06200012
062100        END-IF                                                    06210011
062200     ELSE                                                         06220011
062300        MOVE SPACES                     TO DE-COMPARE1-DATE-TIME  06230012
062400     END-IF                                                       06240011
062500                                                                  06250006
062600     SET DE-YYMMDD-FORMAT       TO TRUE                           06260006
062700     MOVE EMP-MTOD-DATE         TO DE-YYMMDD                      06270006
062800     PERFORM P8998-DATEEDIT                                       06280006
062900     MOVE DE-CCYYMMDD           TO DE-COMPARE2-DATE               06290006
063000     MOVE EMP-MTOD-TIME         TO DE-COMPARE2-TIME               06300006
063100                                                                  06310006
063200*SXO C780                                                         06320006
063300     IF WS-CANADIAN-COMPANY                                       06330008
063400        MOVE SPACES              TO DE-COMPARE3-DATE-TIME         06340007
063500     ELSE                                                         06350007
063600        SET DE-YYMMDD-FORMAT     TO TRUE                          06360007
063700        MOVE EMP-US-RSTD-DATE    TO DE-YYMMDD                     06370007
063800        PERFORM P8998-DATEEDIT                                    06380007
063900        MOVE DE-CCYYMMDD         TO DE-COMPARE3-DATE              06390007
064000        MOVE EMP-US-RSTD-TIME    TO DE-COMPARE3-TIME              06400007
064100     END-IF                                                       06410007
064200*                                                                 06420006
064300*    CNC0183 - MATT - 12/11/99                                    06430006
064400*                                                                 06440006
064500     IF DE-COMPARE2-DATE-TIME > DE-COMPARE1-DATE-TIME             06450006
064600        IF DE-COMPARE2-DATE-TIME >  DE-COMPARE3-DATE-TIME         06460006
064700           MOVE DE-COMPARE2-DATE-TIME                             06470006
064800                                 TO DE-COMPARE1-DATE-TIME         06480006
064900        ELSE                                                      06490006
065000           MOVE DE-COMPARE3-DATE-TIME                             06500006
065100                                 TO DE-COMPARE1-DATE-TIME         06510006
065200        END-IF                                                    06520006
065300     ELSE                                                         06530006
065400        IF DE-COMPARE1-DATE-TIME <  DE-COMPARE3-DATE-TIME         06540006
065500           MOVE DE-COMPARE3-DATE-TIME                             06550006
065600                                 TO DE-COMPARE1-DATE-TIME         06560006
065700        END-IF                                                    06570006
065800     END-IF                                                       06580006
065900*SXO C780                                                         06590006
066000     MOVE DE-COMPARE1-DATE-TIME(3:10)                             06600006
066100                                 TO WS-WORK-DATE-TIME-C           06610006
066200                                                                  06620006
066300*-------------------------------------------------------*         06630006
066400*     CONVERT TO SYSTEM TIME ZONE                       *         06640006
066500*-------------------------------------------------------*         06650006
066600     IF WS-WORK-DATE-TIME-C NUMERIC                               06660006
066700        AND WS-WORK-DATE-TIME > ZERO                              06670006
066800        MOVE SPACES                 TO WS-CNTL-FILE               06680006
066900        SET SUB-DIST-TYPE-REC       TO TRUE                       06690006
067000        MOVE DIST IN WS-MSTR        TO CNTL-DIST                  06700006
067100        MOVE SUB-DIST IN WS-MSTR TO CNTL-SUB-DIST                 06710006
067200        PERFORM P8210-READ-CNTLFILE                               06720006
067300        MOVE CNTL-TIME-ZONE         TO TZ-IN-ZONE                 06730006
067400        MOVE WS-WORK-DATE-TIME TO TZ-IN-DATE-TIME                 06740006
067500        SET TZ-OUT-SYSTEM-ZONE TO TRUE                            06750006
067600        PERFORM P8996-TIMEZONE                                    06760006
067700        IF TZ-INVALID-PARAMETERS                                  06770006
067800           MOVE 'P4130-2'           TO ERR-PARAGRAPH              06780006
067900           PERFORM P8996-TZERROR                                  06790006
068000        END-IF                                                    06800006
068100*       IF TZ-OUT-DATE-TIME > WS-DATE-TIME                        06810006
068200        IF TZ-OUT-DATE-TIME-CENT > WS-PRESENT-TIME-CENT           06820006
068300           IF DIST IN WS-MSTR = SCR02C-DIST                       06830006
068400              AND SUB-DIST IN WS-MSTR = SCR02C-SUB-DIST           06840006
068500              CONTINUE                                            06850006
068600           ELSE                                                   06860006
068700*-------------------------------------------------------*         06870006
068800*             CONVERT TO SCREEN TIME ZONE               *         06880006
068900*-------------------------------------------------------*         06890006
069000              MOVE SPACES             TO WS-CNTL-FILE             06900006
069100              SET SUB-DIST-TYPE-REC TO TRUE                       06910006
069200              MOVE DIST IN WS-MSTR TO CNTL-DIST                   06920006
069300              MOVE SUB-DIST IN WS-MSTR TO CNTL-SUB-DIST           06930006
069400              PERFORM P8210-READ-CNTLFILE                         06940006
069500              MOVE CNTL-TIME-ZONE TO TZ-IN-ZONE                   06950006
069600              MOVE WS-WORK-DATE-TIME TO TZ-IN-DATE-TIME           06960006
069700              MOVE PSTCA-TIME-ZONE TO TZ-OUT-ZONE                 06970006
069800              PERFORM P8996-TIMEZONE                              06980006
069900              IF TZ-INVALID-PARAMETERS                            06990006
070000                 MOVE 'P4130-3'       TO ERR-PARAGRAPH            07000006
070100                 PERFORM P8996-TZERROR                            07010006
070200              END-IF                                              07020006
070300              MOVE TZ-OUT-DATE-TIME TO WS-WORK-DATE-TIME          07030006
070400           END-IF                                                 07040006
070500           MOVE WS-WORK-DATE-TIME(7:4) TO                         07050006
070600                      SCR02C-REST-TIME(SET-NUM ROW-NUM)           07060006
070700           MOVE WS-WORK-DATE-TIME(5:2) TO                         07070006
070800                      SCR02C-REST-DY(SET-NUM ROW-NUM)             07080006
070900        END-IF                                                    07090006
071000     END-IF                                                       07100006
071100     MOVE EMP-NAME TO SCR02C-EMP-NAME(SET-NUM ROW-NUM)            07110006
071200     EVALUATE TRUE                                                07120006
071300        WHEN AVAILABLE                                            07130006
071400           IF PSTCA-SUB = 2                                       07140006
071500             MOVE 'DISPON' TO SCR02C-STATUS(SET-NUM ROW-NUM)      07150006
071600           ELSE                                                   07160006
071700             MOVE 'AVAIL' TO SCR02C-STATUS(SET-NUM ROW-NUM)       07170006
071800           END-IF                                                 07180006
071900        WHEN WORKING                                              07190006
072000           IF PSTCA-SUB = 2                                       07200006
072100             MOVE 'TRAVAIL' TO SCR02C-STATUS(SET-NUM ROW-NUM)     07210006
072200           ELSE                                                   07220006
072300             MOVE 'WORKING' TO SCR02C-STATUS(SET-NUM ROW-NUM)     07230006
072400           END-IF                                                 07240006
072500        WHEN OFF-MILES-DAYS                                       07250006
072600           IF EMP-MILES-DATE NUMERIC                              07260006
072700              AND EMP-MILES-DATE-NUM > 0                          07270006
072800              AND EMP-MILES-DATE-NUM < 32                         07280006
072900*              MOVE WS-MO TO                                      07290006
073000               MOVE WS-SYS-MO TO                                  07300006
073100                   SCR02C-RETURN-DATE-MM(SET-NUM ROW-NUM)         07310006
073200              MOVE EMP-MILES-DATE-NUM TO                          07320006
073300                   SCR02C-RETURN-DATE-DD(SET-NUM ROW-NUM)         07330006
073400*             IF EMP-MILES-DATE-NUM < WS-DY                       07340006
073500              IF EMP-MILES-DATE-NUM < WS-SYS-DY                   07350006
073600                 ADD 1 TO SCR02C-RETURN-DATE-MM(SET-NUM ROW-NUM)  07360006
073700                 IF SCR02C-RETURN-DATE-MM(SET-NUM ROW-NUM) > 12   07370006
073800                    MOVE 01 TO                                    07380006
073900                         SCR02C-RETURN-DATE-MM(SET-NUM ROW-NUM)   07390006
074000                 END-IF                                           07400006
074100              END-IF                                              07410006
074200           ELSE                                                   07420006
074300              MOVE EMP-MILES-DATE TO                              07430006
074400                   SCR02C-RETURN-DATE(SET-NUM ROW-NUM)            07440006
074500           END-IF                                                 07450006
074600           PERFORM P4180-OFF-STATUS                               07460006
074700        WHEN VACATION                                             07470006
074800           PERFORM P4170-GET-RETURN-DATE                          07480006
074900           PERFORM P4180-OFF-STATUS                               07490006
075000        WHEN OTHER                                                07500006
075100           PERFORM P4180-OFF-STATUS                               07510006
075200     END-EVALUATE                                                 07520006
075300     .                                                            07530006
075400*                                                                 07540006
075500 P4150-GET-EMPS-LATEST-TEMP.                                      07550006
075600*                                                                 07560006
075700     MOVE SPACES                     TO WS-ASSIGN-COMMAREA        07570006
075800     SET GET-EMPS-LATEST-TEMP-JOB    TO TRUE                      07580006
075900     MOVE ASGN-EMP-NO                TO                           07590006
076000          WS-ASSIGN-RECORD(LENGTH OF ASGNKEY1 + 1 :               07600006
076100                           LENGTH OF ASGN-EMP-NO)                 07610006
076200     EXEC CICS LINK                                               07620006
076300               PROGRAM(P901-PGM)                                  07630006
076400               COMMAREA(WS-ASSIGN-COMMAREA)                       07640006
076500               LENGTH(P901-LGTH)                                  07650006
076600               RESP(WS-RESPONSE)                                  07660006
076700     END-EXEC                                                     07670006
076800     MOVE WS-RESPONSE                TO FILE-STATUS               07680006
076900     IF NOT SUCCESS                                               07690006
077000        MOVE 'P4150-1'               TO ERR-PARAGRAPH             07700006
077100        MOVE 'P901LINK'              TO ERR-KEY                   07710006
077200        PERFORM P9999-GOT-PROBLEM                                 07720006
077300     END-IF                                                       07730006
077400***CLEAR RECORD IF NO ASSIGNMENT FOUND                            07740006
077500     IF WS-ASSIGN-RECORD(1 : LENGTH OF ASGNKEY1) = SPACES         07750006
077600        MOVE SPACES TO WS-ASSIGN-RECORD                           07760006
077700     END-IF                                                       07770006
077800     .                                                            07780006
077900                                                                  07790006
078000***************************************************************** 07800006
078100 P4170-GET-RETURN-DATE.                                           07810006
078200*    FOR EMPLOYEES ON VACATION, FIND THE BOOK-ON RECORD IN        07820006
078300*    THE TASK LIST.  THIS CONTAINS THE EXPECTED RETURN DATE.      07830006
078400***************************************************************** 07840006
078500     INITIALIZE TASK-EMPLOYEE-KEY                                 07850006
078600     MOVE EMP-NBR OF WS-MSTR TO EMP-NBR OF WS-TASK                07860006
078700     PERFORM P8300-START-TASK-FILE                                07870006
078800     IF SUCCESS                                                   07880006
078900        SET TASK-NOT-DONE TO TRUE                                 07890006
079000        PERFORM P8310-READNEXT-TASK-FILE                          07900006
079100        PERFORM UNTIL TASK-DONE                                   07910006
079200           IF SUCCESS                                             07920006
079300              AND EMP-NBR OF WS-MSTR = EMP-NBR OF WS-TASK         07930006
079400              IF TASK-LAYOFF-MARKUP                               07940006
079500                 SET TASK-DONE TO TRUE                            07950006
079600                 MOVE EFF-MO OF WS-TASK TO                        07960006
079700                      SCR02C-RETURN-DATE-MM(SET-NUM ROW-NUM)      07970006
079800                 MOVE EFF-DY OF WS-TASK TO                        07980006
079900                      SCR02C-RETURN-DATE-DD(SET-NUM ROW-NUM)      07990006
080000              ELSE                                                08000006
080100                 PERFORM P8310-READNEXT-TASK-FILE                 08010006
080200              END-IF                                              08020006
080300           ELSE                                                   08030006
080400              SET TASK-DONE TO TRUE                               08040006
080500           END-IF                                                 08050006
080600        END-PERFORM                                               08060006
080700     END-IF                                                       08070006
080800     PERFORM P8320-ENDBR-TASK-FILE                                08080006
080900     .                                                            08090006
081000                                                                  08100006
081100*                                                                 08110006
081200 P4180-OFF-STATUS.                                                08120006
081300*                                                                 08130006
081400     IF PSTCA-SUB = 2                                             08140006
081500        MOVE 'COGES' TO SCR02C-STATUS(SET-NUM ROW-NUM)            08150006
081600     ELSE                                                         08160006
081700        MOVE 'OFF' TO SCR02C-STATUS(SET-NUM ROW-NUM)              08170006
081800     END-IF                                                       08180006
081900     .                                                            08190006
082000*                                                                 08200006
082100 P5200-CHECK-COMPANY-CD.                                          08210010
082200*                                                                 08220006
082300     MOVE SPACES                 TO CNTLKEY                       08230006
082400                                    CNTLKEY-AREA                  08240006
082500     MOVE DIST OF WS-MSTR        TO CNTL-DIST                     08250006
082600     SET DIST-TYPE-REC           TO TRUE                          08260006
082700     MOVE CNTLKEY-AREA           TO CNTLKEY                       08270006
082800     PERFORM P8210-READ-CNTLFILE                                  08280006
082900     IF CNTL-BCR-COMPANY                                          08290008
083000        OR CNTL-CN-COMPANY                                        08300008
083100        OR CNTL-ACR-COMPANY                                       08310008
083200        SET WS-CANADIAN-COMPANY  TO TRUE                          08320008
083300     ELSE                                                         08330008
083400        SET WS-US-COMPANY        TO TRUE                          08340008
083500     END-IF                                                       08350008
083600     IF WS-CANADIAN-COMPANY                                       08360009
083700        SET DONT-APPLY-LEAD-TIME TO TRUE                          08370006
083800     ELSE                                                         08380009
083900        SET APPLY-LEAD-TIME      TO TRUE                          08390009
084000     END-IF                                                       08400006
084100     .                                                            08410006
084200*                                                                 08420006
084300 P7000-WRITE-TSQUEUE.                                             08430006
084400*                                                                 08440006
084500*                                                                 08450006
084600*      WRITE MAP TSQUEUE                                          08460006
084700*                                                                 08470006
084800     EXEC CICS ASSIGN                                             08480006
084900          EXTDS(WS-CICS-EXTDS-CODE)                               08490006
085000     END-EXEC                                                     08500006
085100*                                                                 08510006
085200     IF SCREEN-HAS-EXT-ATTR                                       08520006
085300        EXEC CICS SEND STRFIELD                                   08530006
085400                  FROM(WS-STRFIELD)                               08540006
085500                  LENGTH(WS-STRFIELD-LGTH)                        08550006
085600                  RESP(WS-RESPONSE)                               08560006
085700        END-EXEC                                                  08570006
085800        MOVE WS-RESPONSE           TO FILE-STATUS                 08580006
085900        IF NOT SUCCESS                                            08590006
086000           MOVE 'P7000-1'          TO ERR-PARAGRAPH               08600006
086100           MOVE 'SEND STRFIELD'    TO ERR-KEY                     08610006
086200           PERFORM P9999-GOT-PROBLEM                              08620006
086300        END-IF                                                    08630006
086400     END-IF                                                       08640006
086500*                                                                 08650006
086600     MOVE LENGTH OF WS-BUFFER-DATA TO WS-BUFFER-LGTH              08660006
086700     EXEC CICS RECEIVE BUFFER                                     08670006
086800               INTO(WS-BUFFER-DATA)                               08680006
086900               LENGTH(WS-BUFFER-LGTH)                             08690006
087000               RESP(WS-RESPONSE)                                  08700006
087100     END-EXEC                                                     08710006
087200     MOVE WS-RESPONSE              TO FILE-STATUS                 08720006
087300     IF NOT SUCCESS AND NOT EOC                                   08730006
087400        MOVE 'P7000-2'             TO ERR-PARAGRAPH               08740006
087500        MOVE 'RECEIVE BUFFER'      TO ERR-KEY                     08750006
087600        PERFORM P9999-GOT-PROBLEM                                 08760006
087700     END-IF                                                       08770006
087800     MOVE EIBCPOSN                 TO WS-BUFFER-CURSOR            08780006
087900                                                                  08790006
088000                                                                  08800006
088100     MOVE LENGTH OF WS-BUFFER-AREA TO P02CTSQ-QLGTH               08810006
088200     MOVE EIBTRMID                 TO P02CTSQ-MAP-TERM-ID         08820006
088300     EXEC CICS WRITEQ TS                                          08830006
088400               QUEUE(P02CTSQ-MAP-QUEUE-ID)                        08840006
088500               FROM(WS-BUFFER-AREA)                               08850006
088600               LENGTH(P02CTSQ-QLGTH)                              08860006
088700               RESP(WS-RESPONSE)                                  08870006
088800     END-EXEC                                                     08880006
088900     MOVE WS-RESPONSE              TO FILE-STATUS                 08890006
089000     IF NOT SUCCESS                                               08900006
089100        MOVE 'P7000-3'             TO ERR-PARAGRAPH               08910006
089200        PERFORM P9999-GOT-PROBLEM                                 08920006
089300     END-IF                                                       08930006
089400     MOVE EIBTRMID TO P02CTSQ-CA-TERM-ID                          08940006
089500     EXEC CICS WRITEQ TS                                          08950006
089600               QUEUE(P02CTSQ-CA-QUEUE-ID)                         08960006
089700               FROM(PSTCOMM-AREA)                                 08970006
089800               LENGTH(PSTCOMM-LGTH)                               08980006
089900               ITEM(P02CTSQ-QUEUE-ITEM)                           08990006
090000               RESP(WS-RESPONSE)                                  09000006
090100     END-EXEC                                                     09010006
090200     MOVE WS-RESPONSE              TO FILE-STATUS                 09020006
090300     IF NOT SUCCESS                                               09030006
090400        MOVE 'P7000-4'             TO ERR-PARAGRAPH               09040006
090500        PERFORM P9999-GOT-PROBLEM                                 09050006
090600     END-IF.                                                      09060006
090700*                                                                 09070006
090800 P7010-READ-TSQUEUE.                                              09080006
090900*                                                                 09090006
091000*              READ THE MAPS TSQUEUE                              09100006
091100*                                                                 09110006
091200     MOVE LENGTH OF WS-BUFFER-AREA TO P02CTSQ-QLGTH               09120006
091300     MOVE EIBTRMID                 TO P02CTSQ-MAP-TERM-ID         09130006
091400     EXEC CICS READQ TS                                           09140006
091500               QUEUE(P02CTSQ-MAP-QUEUE-ID)                        09150006
091600               INTO(WS-BUFFER-AREA)                               09160006
091700               LENGTH(P02CTSQ-QLGTH)                              09170006
091800               ITEM(P02CTSQ-QUEUE-ITEM)                           09180006
091900               RESP(WS-RESPONSE)                                  09190006
092000     END-EXEC                                                     09200006
092100     MOVE WS-RESPONSE              TO FILE-STATUS                 09210006
092200     IF SUCCESS                                                   09220006
092300        SET SEND-BUFFER            TO TRUE                        09230006
092400     ELSE                                                         09240006
092500        SET CREATE-SCREEN          TO TRUE                        09250006
092600        MOVE LOW-VALUES            TO PSTS02C                     09260006
092700     END-IF                                                       09270006
092800     MOVE EIBTRMID TO P02CTSQ-CA-TERM-ID                          09280006
092900     EXEC CICS READQ TS                                           09290006
093000               QUEUE(P02CTSQ-CA-QUEUE-ID)                         09300006
093100               INTO(PSTCOMM-AREA)                                 09310006
093200               LENGTH(PSTCOMM-LGTH)                               09320006
093300               ITEM(P02CTSQ-QUEUE-ITEM)                           09330006
093400               RESP(WS-RESPONSE)                                  09340006
093500     END-EXEC                                                     09350006
093600     MOVE WS-RESPONSE TO FILE-STATUS                              09360006
093700     IF NOT SUCCESS                                               09370006
093800        MOVE SPACES TO PSTCOMM-AREA                               09380006
093900     END-IF                                                       09390006
094000     PERFORM P7020-DELETE-TSQUEUE.                                09400006
094100*                                                                 09410006
094200 P7020-DELETE-TSQUEUE.                                            09420006
094300*                                                                 09430006
094400     MOVE EIBTRMID TO P02CTSQ-MAP-TERM-ID                         09440006
094500     EXEC CICS DELETEQ TS                                         09450006
094600               QUEUE(P02CTSQ-MAP-QUEUE-ID)                        09460006
094700               RESP(WS-RESPONSE)                                  09470006
094800     END-EXEC                                                     09480006
094900     MOVE EIBTRMID TO P02CTSQ-CA-TERM-ID                          09490006
095000     EXEC CICS DELETEQ TS                                         09500006
095100               QUEUE(P02CTSQ-CA-QUEUE-ID)                         09510006
095200               RESP(WS-RESPONSE)                                  09520006
095300     END-EXEC.                                                    09530006
095400*                                                                 09540006
095500 P8000-START-TRCNKEY3.                                            09550006
095600*                                                                 09560006
095700     SET NOT-DONE                   TO TRUE                       09570006
095800     MOVE SPACES                    TO TRCN-KEY3                  09580006
095900     MOVE FICT-JOB-DIST             TO TRCN-DIST3                 09590006
096000     MOVE FICT-JOB-SUB-DIST         TO TRCN-SDIST3                09600006
096100     MOVE FICA-NEXT-JOB             TO TRCN-ASSIGNMENT            09610006
096200     MOVE TRCN-KEY3                 TO TRCNKEY3                   09620006
096300     EXEC CICS STARTBR                                            09630006
096400               DATASET(TRAIN-CN-VIA-DSD-ASGN)                     09640006
096500               RIDFLD(TRCNKEY3)                                   09650006
096600               GTEQ                                               09660006
096700               RESP(WS-RESPONSE)                                  09670006
096800     END-EXEC                                                     09680006
096900     MOVE WS-RESPONSE               TO FILE-STATUS                09690006
097000     IF NOT SUCCESS                                               09700006
097100        SET DONE                    TO TRUE                       09710006
097200        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     09720006
097300           MOVE 'P8000-1'           TO ERR-PARAGRAPH              09730006
097400           MOVE TRCNKEY3            TO ERR-KEY                    09740006
097500           PERFORM P9999-GOT-PROBLEM                              09750006
097600        END-IF                                                    09760006
097700     END-IF.                                                      09770006
097800*                                                                 09780006
097900 P8100-READNEXT-TRCNKEY3.                                         09790006
098000*                                                                 09800006
098100     SET JOB-NOT-QUALIFIED          TO TRUE                       09810006
098200     EXEC CICS READNEXT                                           09820006
098300               DATASET(TRAIN-CN-VIA-DSD-ASGN)                     09830006
098400               INTO(WS-TRCN-FILE)                                 09840006
098500               LENGTH(TRAINCN-DSD-RLGTH)                          09850006
098600               RIDFLD(TRCNKEY3)                                   09860006
098700               KEYLENGTH(TRAINCN-DSD-KLGTH)                       09870006
098800               RESP(WS-RESPONSE)                                  09880006
098900     END-EXEC                                                     09890006
099000     MOVE WS-RESPONSE               TO FILE-STATUS                09900006
099100     IF SUCCESS                                                   09910006
099200        IF TRCN-DIST3 = FICT-JOB-DIST                             09920006
099300           AND TRCN-SDIST3 = FICT-JOB-SUB-DIST                    09930006
099400           IF TRCN-ASSIGNMENT > FICA-NEXT-JOB                     09940006
099500              SET JOB-QUALIFIES     TO TRUE                       09950006
099600              MOVE TRCN-ASSIGNMENT  TO JOB-DEF-CHECK              09960006
099700              IF NOT JOB-DEF-RELIEF-ASGN                          09970006
099800                 SET JOB-NOT-QUALIFIED                            09980006
099900                                    TO TRUE                       09990006
100000              END-IF                                              10000006
100100              IF FICT-JOB-GRP-PL-XB > SPACE                       10010006
100200                 AND FICT-JOB-GRP-PL-XB NOT = JOB-DEF-YARD-CODE   10020006
100300                 SET JOB-NOT-QUALIFIED                            10030006
100400                                    TO TRUE                       10040006
100500              END-IF                                              10050006
100600              IF FICT-JOB-CLASS > SPACE                           10060006
100700                 AND FICT-JOB-CLASS NOT = JOB-DEF-CLASS           10070006
100800                 SET JOB-NOT-QUALIFIED                            10080006
100900                                    TO TRUE                       10090006
101000              END-IF                                              10100006
101100           END-IF                                                 10110006
101200        ELSE                                                      10120006
101300           SET DONE                 TO TRUE                       10130006
101400        END-IF                                                    10140006
101500     ELSE                                                         10150006
101600        SET DONE                    TO TRUE                       10160006
101700        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     10170006
101800           MOVE 'P8100-1'           TO ERR-PARAGRAPH              10180006
101900           MOVE TRCNKEY3            TO ERR-KEY                    10190006
102000           PERFORM P9999-GOT-PROBLEM                              10200006
102100        END-IF                                                    10210006
102200     END-IF.                                                      10220006
102300*                                                                 10230006
102400 P8200-READ-AJJOBKEY.                                             10240006
102500*                                                                 10250006
102600     MOVE AJJOBKEY-AREA             TO AJJOBKEY                   10260006
102700     EXEC CICS READ                                               10270006
102800               DATASET(AJ-VIA-JNAME-JCRAFT)                       10280006
102900               INTO(WS-ASGNED-JOBS)                               10290006
103000               LENGTH(AJNAMECR-RLGTH)                             10300006
103100               RIDFLD(AJJOBKEY)                                   10310006
103200               KEYLENGTH(AJNAMECR-KLGTH)                          10320006
103300               RESP(WS-RESPONSE)                                  10330006
103400     END-EXEC                                                     10340006
103500     MOVE WS-RESPONSE               TO FILE-STATUS                10350006
103600     IF NOT SUCCESS                                               10360006
103700        IF NOT NO-RECORD-FND                                      10370006
103800           MOVE 'P8200-1'           TO ERR-PARAGRAPH              10380006
103900           MOVE AJJOBKEY            TO ERR-KEY                    10390006
104000           PERFORM P9999-GOT-PROBLEM                              10400006
104100        END-IF                                                    10410006
104200     END-IF.                                                      10420006
104300*                                                                 10430006
104400 P8210-READ-CNTLFILE.                                             10440006
104500*                                                                 10450006
104600     MOVE CNTLKEY-AREA             TO CNTLKEY                     10460006
104700     EXEC CICS READ                                               10470006
104800               DATASET(CNTL-FILE-VIA-CNTLKEY)                     10480006
104900               INTO(WS-CNTL-FILE)                                 10490006
105000               LENGTH(CNTLFILE-RLGTH)                             10500006
105100               RIDFLD(CNTLKEY)                                    10510006
105200               KEYLENGTH(CNTLFILE-KLGTH)                          10520006
105300               RESP(WS-RESPONSE)                                  10530006
105400     END-EXEC                                                     10540006
105500     MOVE WS-RESPONSE               TO FILE-STATUS                10550006
105600     IF NOT SUCCESS                                               10560006
105700        IF NOT NO-RECORD-FND                                      10570006
105800           MOVE 'P8210-1'           TO ERR-PARAGRAPH              10580006
105900           MOVE CNTLKEY             TO ERR-KEY                    10590006
106000           PERFORM P9999-GOT-PROBLEM                              10600006
106100        ELSE                                                      10610006
106200           MOVE 'E'                 TO CNTL-TIME-ZONE             10620006
106300        END-IF                                                    10630006
106400     END-IF                                                       10640006
106500     .                                                            10650006
106600***************************************************************** 10660006
106700 P8300-START-TASK-FILE.                                           10670006
106800***************************************************************** 10680006
106900     MOVE TASK-EMPLOYEE-KEY TO TASKEMPK                           10690006
107000     EXEC CICS STARTBR                                            10700006
107100               DATASET(TASK-VIA-EMP-NBR)                          10710006
107200               RIDFLD(TASKEMPK)                                   10720006
107300               GTEQ                                               10730006
107400               RESP(WS-RESPONSE)                                  10740006
107500     END-EXEC                                                     10750006
107600     MOVE WS-RESPONSE TO FILE-STATUS                              10760006
107700     IF NOT SUCCESS                                               10770006
107800        IF NO-RECORD-FND OR END-OF-FILE                           10780006
107900           CONTINUE                                               10790006
108000        ELSE                                                      10800006
108100           MOVE 'P8300-1' TO ERR-PARAGRAPH                        10810006
108200           MOVE TASKEMPK TO ERR-KEY                               10820006
108300           PERFORM P9999-GOT-PROBLEM                              10830006
108400        END-IF                                                    10840006
108500     END-IF                                                       10850006
108600     .                                                            10860006
108700                                                                  10870006
108800***************************************************************** 10880006
108900 P8310-READNEXT-TASK-FILE.                                        10890006
109000***************************************************************** 10900006
109100     EXEC CICS READNEXT                                           10910006
109200               DATASET(TASK-VIA-EMP-NBR)                          10920006
109300               INTO(WS-TASK)                                      10930006
109400               LENGTH(TASKENBR-RLGTH)                             10940006
109500               RIDFLD(TASKEMPK)                                   10950006
109600               KEYLENGTH(TASKENBR-KLGTH)                          10960006
109700               RESP(WS-RESPONSE)                                  10970006
109800     END-EXEC                                                     10980006
109900     MOVE WS-RESPONSE TO FILE-STATUS                              10990006
110000     IF NOT SUCCESS                                               11000006
110100        IF NO-RECORD-FND OR END-OF-FILE                           11010006
110200           CONTINUE                                               11020006
110300        ELSE                                                      11030006
110400           MOVE 'P8310-1' TO ERR-PARAGRAPH                        11040006
110500           MOVE TASKEMPK TO ERR-KEY                               11050006
110600           PERFORM P9999-GOT-PROBLEM                              11060006
110700        END-IF                                                    11070006
110800     END-IF                                                       11080006
110900     .                                                            11090006
111000                                                                  11100006
111100***************************************************************** 11110006
111200 P8320-ENDBR-TASK-FILE.                                           11120006
111300***************************************************************** 11130006
111400     EXEC CICS ENDBR                                              11140006
111500               DATASET(TASK-VIA-EMP-NBR)                          11150006
111600               RESP(WS-RESPONSE)                                  11160006
111700     END-EXEC                                                     11170006
111800     MOVE WS-RESPONSE TO FILE-STATUS                              11180006
111900     IF NOT SUCCESS                                               11190006
112000        MOVE 'P8320-1'  TO ERR-PARAGRAPH                          11200006
112100        MOVE TASKEMPK   TO ERR-KEY                                11210006
112200        PERFORM P9999-GOT-PROBLEM                                 11220006
112300     END-IF                                                       11230006
112400     .                                                            11240006
112500*                                                                 11250006
112600     COPY CNTRTXT.                                                11260006
112700*                                                                 11270006
112800     COPY TIMEZONE.                                               11280006
112900*                                                                 11290006
113000     COPY DATEEDIT.                                               11300006
113100*                                                                 11310006
113200     COPY TZERROR.                                                11320006
113300*                                                                 11330006
113400 P9000-SEND-MAP-AND-RETURN.                                       11340006
113500*                                                                 11350006
113600     MOVE REV-VIDEO TO SCR02C-TITLE-HI                            11360006
113700     MOVE WHITE     TO SCR02C-TITLE-COLOR                         11370006
113800                                                                  11380006
113900     IF MSGLOG-CODE > SPACES                                      11390006
114000         PERFORM P9030-GET-MESSAGE                                11400006
114100         MOVE MSGLOG-MESSAGE-AREA TO SCR02C-ERRORMSG              11410006
114200     END-IF                                                       11420006
114300                                                                  11430006
114400     MOVE P02C-MAP-VERSION(PSTCA-SUB) TO P02C-MAP                 11440006
114500     IF CREATE-SCREEN                                             11450006
114600        PERFORM P9010-SEND-PHYSICAL-MAP                           11460006
114700     ELSE                                                         11470006
114800        IF CONTINUE-SCREEN                                        11480006
114900           PERFORM P9020-SEND-DATAONLY-MAP                        11490006
115000        ELSE                                                      11500006
115100           PERFORM P9035-SEND-BUFFER                              11510006
115200        END-IF                                                    11520006
115300     END-IF                                                       11530006
115400     EXEC CICS RETURN                                             11540006
115500               TRANSID(P02C-TRAN)                                 11550006
115600               COMMAREA(PSTCOMM-AREA)                             11560006
115700               LENGTH(PSTCOMM-LGTH)                               11570006
115800     END-EXEC.                                                    11580006
115900*                                                                 11590006
116000 P9010-SEND-PHYSICAL-MAP.                                         11600006
116100*                                                                 11610006
116200     EXEC CICS SEND MAP(P02C-MAP)                                 11620006
116300                    MAPSET(P02C-SET)                              11630006
116400                    FROM(PSTS02C)                                 11640006
116500                    ERASE                                         11650006
116600                    RESP(WS-RESPONSE)                             11660006
116700     END-EXEC                                                     11670006
116800     MOVE WS-RESPONSE TO FILE-STATUS                              11680006
116900     IF NOT SUCCESS                                               11690006
117000        MOVE 'P9010'   TO ERR-PARAGRAPH                           11700006
117100        PERFORM P9999-GOT-PROBLEM                                 11710006
117200     END-IF.                                                      11720006
117300*                                                                 11730006
117400 P9020-SEND-DATAONLY-MAP.                                         11740006
117500*                                                                 11750006
117600     EXEC CICS SEND MAP(P02C-MAP)                                 11760006
117700                    MAPSET(P02C-SET)                              11770006
117800                    FROM(PSTS02C)                                 11780006
117900                    DATAONLY                                      11790006
118000                    RESP(WS-RESPONSE)                             11800006
118100     END-EXEC                                                     11810006
118200     MOVE WS-RESPONSE TO FILE-STATUS                              11820006
118300     IF NOT SUCCESS                                               11830006
118400        MOVE 'P9020' TO ERR-PARAGRAPH                             11840006
118500        PERFORM P9999-GOT-PROBLEM                                 11850006
118600     END-IF.                                                      11860006
118700*                                                                 11870006
118800 P9030-GET-MESSAGE.                                               11880006
118900*                                                                 11890006
119000     MOVE PSTCA-SUB TO MSGLOG-SUB-CODE                            11900006
119100     EXEC CICS READ                                               11910006
119200               DATASET(MSGLOG-VIA-CODE)                           11920006
119300               INTO(MSGLOG-AREA)                                  11930006
119400               LENGTH(MSGLOG-RLGTH)                               11940006
119500               RIDFLD(MSGLOG-KEY)                                 11950006
119600               KEYLENGTH(MSGLOG-KLGTH)                            11960006
119700               RESP(WS-RESPONSE)                                  11970006
119800     END-EXEC                                                     11980006
119900     MOVE WS-RESPONSE TO FILE-STATUS                              11990006
120000     IF NOT SUCCESS                                               12000006
120100        IF PSTCA-SUB = 1                                          12010006
120200           MOVE 'NO MESSAGE ON FILE' TO MSGLOG-MESSAGE            12020006
120300        ELSE                                                      12030006
120400           MOVE 'AUCUN MESSAGE'      TO MSGLOG-MESSAGE            12040006
120500        END-IF                                                    12050006
120600     END-IF                                                       12060006
120700     MOVE MSGLOG-CODE     TO MSGLOG-MSG-CODE                      12070006
120800     MOVE '-'             TO MSGLOG-MSG-SEP                       12080006
120900     MOVE MSGLOG-SUB-CODE TO MSGLOG-MSG-SUB-CODE.                 12090006
121000*                                                                 12100006
121100 P9035-SEND-BUFFER.                                               12110006
121200*                                                                 12120006
121300     EXEC CICS SEND                                               12130006
121400               FROM(WS-BUFFER-DATA)                               12140006
121500               LENGTH(WS-BUFFER-LGTH)                             12150006
121600               ERASE                                              12160006
121700               RESP(WS-RESPONSE)                                  12170006
121800     END-EXEC                                                     12180006
121900     MOVE WS-RESPONSE       TO FILE-STATUS                        12190006
122000     IF NOT SUCCESS                                               12200006
122100        MOVE 'P9035-1'      TO ERR-PARAGRAPH                      12210006
122200        MOVE 'SEND BUFFER'  TO ERR-KEY                            12220006
122300        PERFORM P9999-GOT-PROBLEM                                 12230006
122400     END-IF                                                       12240006
122500     EXEC CICS SEND                                               12250006
122600               CONTROL                                            12260006
122700               CURSOR(WS-BUFFER-CURSOR)                           12270006
122800               RESP(WS-RESPONSE)                                  12280006
122900     END-EXEC                                                     12290006
123000     MOVE WS-RESPONSE       TO FILE-STATUS                        12300006
123100     IF NOT SUCCESS                                               12310006
123200        MOVE 'P9035-2'      TO ERR-PARAGRAPH                      12320006
123300        MOVE 'SEND CURSOR'  TO ERR-KEY                            12330006
123400        PERFORM P9999-GOT-PROBLEM                                 12340006
123500     END-IF.                                                      12350006
123600*                                                                 12360006
123700 P9100-SETUP-SCR02.                                               12370006
123800*                                                                 12380006
123900     EXEC CICS XCTL                                               12390006
124000               PROGRAM(P02-PGM)                                   12400006
124100               COMMAREA(PSTCOMM-AREA)                             12410006
124200               LENGTH(PSTCOMM-LGTH)                               12420006
124300               RESP(WS-RESPONSE)                                  12430006
124400     END-EXEC                                                     12440006
124500     MOVE WS-RESPONSE TO FILE-STATUS                              12450006
124600     IF NOT SUCCESS                                               12460006
124700         MOVE 'P9100' TO ERR-PARAGRAPH                            12470006
124800         PERFORM P9999-GOT-PROBLEM                                12480006
124900     END-IF.                                                      12490006
125000*                                                                 12500006
125100 P9500-SETUP-SCR998.                                              12510006
125200*                                                                 12520006
125300     MOVE SPACES            TO P998COMM-AREA                      12530006
125400     MOVE P02C-PGM          TO P998CA-FROM-PROGRAM                12540006
125500     MOVE P02C-MAP          TO P998CA-SCREEN-ID                   12550006
125600     MOVE EIBCPOSN          TO P998CA-CURSOR-POS                  12560006
125700     EXEC CICS XCTL                                               12570006
125800               PROGRAM(P998-PGM)                                  12580006
125900               COMMAREA(PSTCOMM-AREA)                             12590006
126000               LENGTH(PSTCOMM-LGTH)                               12600006
126100               RESP(WS-RESPONSE)                                  12610006
126200     END-EXEC                                                     12620006
126300     MOVE WS-RESPONSE       TO FILE-STATUS                        12630006
126400     IF NOT SUCCESS                                               12640006
126500        MOVE 'P9500'        TO ERR-PARAGRAPH                      12650006
126600        PERFORM P9999-GOT-PROBLEM                                 12660006
126700     END-IF.                                                      12670006
126800*                                                                 12680006
126900 P9810-PROCESS-OFFSET.                                            12690006
127000*                                                                 12700006
127100     MOVE PSTCA-DT-OS-FUN       TO PARM-CONV-TYPE                 12710006
127200     MOVE PSTCA-DT-OS-DAYS      TO PARM-SEC-JULIAN-DAY            12720006
127300     MOVE PSTCA-DT-OS-HRMN      TO PARM-SEC-HRMN                  12730006
127400     EXEC CICS LINK                                               12740006
127500               PROGRAM(P903-PGM)                                  12750006
127600               COMMAREA(DATE-CONVERSION-PARMS)                    12760006
127700               LENGTH(P903-LGTH)                                  12770006
127800               RESP(WS-RESPONSE)                                  12780006
127900     END-EXEC                                                     12790006
128000     MOVE WS-RESPONSE           TO FILE-STATUS                    12800006
128100     IF NOT SUCCESS                                               12810006
128200        MOVE 'P9810-1'          TO ERR-PARAGRAPH                  12820006
128300        MOVE 'P903'             TO ERR-KEY                        12830006
128400        PERFORM P9999-GOT-PROBLEM                                 12840006
128500     END-IF.                                                      12850006
128600*                                                                 12860006
128700 P9990-CLEAR-SCREEN.                                              12870006
128800*                                                                 12880006
128900     EXEC CICS SEND CONTROL                                       12890006
129000                    ERASE                                         12900006
129100                    FREEKB                                        12910006
129200     END-EXEC                                                     12920006
129300     EXEC CICS RETURN END-EXEC.                                   12930006
129400*                                                                 12940006
129500 P9999-GOT-PROBLEM.                                               12950006
129600*                                                                 12960006
129700     MOVE P02C-PGM  TO ERR-PROGRAM                                12970006
129800     MOVE DFHEIBLK  TO ERR-EIBLK                                  12980006
129900     EXEC CICS LINK                                               12990006
130000               PROGRAM(PSTERR-PGM)                                13000006
130100               COMMAREA(PSTERAR-AREA)                             13010006
130200               LENGTH(PSTERAR-LGTH)                               13020006
130300               RESP(WS-RESPONSE)                                  13030006
130400     END-EXEC                                                     13040006
130500     MOVE WS-RESPONSE TO FILE-STATUS                              13050006
130600     IF NOT SUCCESS                                               13060006
130700        EXEC CICS ABEND                                           13070006
130800                  ABCODE(PSTERR-ABCODE)                           13080006
130900        END-EXEC                                                  13090006
131000     END-IF                                                       13100006
131100     EXEC CICS RETURN END-EXEC.                                   13110006
131200*                                                                 13120006
131300 X9999-GOBACK.                                                    13130006
131400     GOBACK.                                                      13140006
