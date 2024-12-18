000100 IDENTIFICATION DIVISION.                                         00010003
000200 PROGRAM-ID.  CNP12.                                              00020003
000300******************************************************************00030003
000400*             A S S I G N E D  J O B  I N Q U I R Y              *00040003
000500******************************************************************00050003
000600*  DATE   INITIAL  LOG#   DESCRIPTION                             00060003
000700*-------- ------- ------  --------------------------------------- 00070003
000800*08/25/93   MOO           ORIGINAL                                00080003
000900*09/25/96   RCW     ?     NORTHERN QUEBEC SPAREBOARD - PHASE 1    00090003
001000*10/07/96   PLS    C-316  PREVENT ANNUL OF RELIEF ASSIGNMENT      00100003
001100*07/21/97   RCW    BUG ?  POPULATE P922-ADD-CREW-PROF             00110003
001200*03/24/98   AMF           ADDED HELP LOGIC.                       00120003
001300*07/28/98   LPS           YEAR 2000 SWEEP.                        00130003
001400*04/06/99   KMW   CNC0210 ADD COST CENTER AND UCA COST CENTER.    00140003
001500*04/28/99   GBJ           JSK1-EXP-DATE Y2K COMPARE.              00150003
001600*05/20/99   BLD   CNC0222 ADD LINK TO CNP966 TO VALIDATE COST     00160003
001700*                         CENTER AND UCA COST CENTER.             00170003
001800*05/24/99   NJB           ASGN SEN                                00180003
001900*12/11/99   MOO   CNC0183 ADD CHECK FOR MTOY & MTOR.              00190003
002000*05/03/00   AJK   CNC0183 REMOVE CHECK FOR MTOR & MTOY.           00200003
002100*08/30/00   BLD  CNC0269A REMOVE COST CENTER                      00210003
002200*10/04/04   KJS  CNC0386A ADD DAY ASSOCIATED WITH PERSONAL REST   00220003
002300*08/28/07   JAJ   C735    RM9897-SHOW OUT OF TOWN EMPLOYEES AS    00230003
002400*                         OUT OF TOWN INSTEAD OF AVAILABLE.       00240003
002500*03/21/18   GDB   C1190   VALIDATE THE ASSIGNMENT ID, ANNUL TO &  00250003
002600*                         FROM DATES BEFORE XCTL TO CNP12A        00260003
009000*08/16/24   RJA  CNLD-309 MTOY AND MTOR ARE OBSOLETE.             00261003
002700***************************************************************** 00262003
002800 ENVIRONMENT DIVISION.                                            00263003
002900 CONFIGURATION SECTION.                                           00264003
003000 SOURCE-COMPUTER.  IBM-370.                                       00265003
003100 OBJECT-COMPUTER.  IBM-370.                                       00266003
003200 DATA DIVISION.                                                   00267003
003300 WORKING-STORAGE SECTION.                                         00268003
003400 01  FILLER                          PIC X(10)  VALUE 'PGM12 W/S'.00269003
003500*01  WS-ABSTIME                      PIC S9(15) COMP-3 VALUE +0.  00270003
003600                                                                  00280003
003700 01  P12ACA-LGTH                     PIC S9(04) COMP VALUE +320.  00290003
003800                                                                  00300003
003900 01  WS-SUBSCRIPTS.                                               00310003
004000     02  ARRAY-MAX                   PIC S99   COMP  VALUE +7.    00320003
004100     02  SUB1                        PIC 9(02) VALUE ZERO.        00330003
004200     02  SUB2                        PIC 9(02) VALUE ZERO.        00340003
004300     02  SUB3                        PIC 9(02) VALUE ZERO.        00350003
004400                                                                  00360003
004500 01  WS-FLAGS.                                                    00370003
004600     02  SCREEN-FLAG                 PIC X     VALUE '0'.         00380003
004700         88  CONTINUE-SCREEN                   VALUE '0'.         00390003
004800         88  CREATE-SCREEN                     VALUE '1'.         00400003
004900         88  SEND-BUFFER                       VALUE '2'.         00410003
005000     02  DONE-CODE                   PIC X     VALUE '0'.         00420003
005100         88  DONE                              VALUE '1'.         00430003
005200     02  WS-ASGN-DONE-CODE           PIC X     VALUE 'N'.         00440003
005300         88  ASGN-DONE                         VALUE 'Y'.         00450003
005400     02  TEMP-ASGN-XB-AUG-FLAG       PIC X     VALUE ' '.         00460003
005500         88  TEMP-ASGN-XB-AUG                  VALUE 'Y'.         00470003
005600     02  ON-DUTY-OUT-TOWN-CODE       PIC X(10) VALUE '9999999999'.00480003
005700         88  OUT-TOWN                          VALUE '0000000000'.00490003
005800     02  WS-DAILY-MARK-CODE          PIC X     VALUE ' '.         00500003
005900         88  DAILY-MARK-YARD                   VALUE 'Y'.         00510003
006000     02  WS-CHANGE-MADE-FLAG         PIC X     VALUE ' '.         00520003
006100         88  NO-CHANGE                         VALUE 'N'.         00530003
006200         88  CHANGE-MADE                       VALUE 'Y'.         00540003
006300                                                                  00550003
006400 01  WS-MISC.                                                     00560003
006500     02  DAY-OF-WK                   PIC 9     VALUE ZERO.        00570003
006600     02  ASGN-OWNER                  PIC X(09) VALUE SPACE.       00580003
006700         88  HAVE-ASGN-OWNER                   VALUE '000000001'  00590003
006800                                                THRU '999999995'. 00600003
006900     02  ASGN-TEMPORARY-EMP          PIC X(09) VALUE SPACE.       00610003
007000         88  HAVE-ASGN-TEMPORARY-EMP            VALUE '000000001' 00620003
007100                                                 THRU '999999995'.00630003
007200     02  ASGN-ON-DUTY-EMP            PIC X(09) VALUE SPACE.       00640003
007300         88  HAVE-ASGN-ON-DUTY-EMP             VALUE '000000001'  00650003
007400                                                THRU '999999995'. 00660003
007500                                                                  00670003
007600 01  NORMAL-ASGNMT-FLAG              PIC X   VALUE SPACE.         00680003
007700     88  NORM-ASGN-UFP                       VALUE 'U'.           00690003
007800     88  NORM-ASGN-XB                        VALUE 'X'.           00700003
007900     88  NORM-ASGN-AJ                        VALUE 'A'.           00710003
008000 01  NORMAL-ASGNMT.                                               00720003
008100     02  NA-DIST                     PIC XX VALUE SPACE.          00730003
008200     02  NA-SUB-DIST                 PIC XX VALUE SPACE.          00740003
008300     02  NA-AREA.                                                 00750003
008400         03  NA-1                    PIC X(06).                   00760003
008500         03  NA-2 REDEFINES NA-1.                                 00770003
008600             04  NA-POOL             PIC XX.                      00780003
008700             04  NA-TURN             PIC X(4).                    00790003
008800         03  NA-3 REDEFINES NA-1.                                 00800003
008900             04  NA-FILLER           PIC XX.                      00810003
009000             04  NA-XB-TURN          PIC X(4).                    00820003
009100         03  NA-CC                   PIC XX.                      00830003
009200                                                                  00840003
009300 01  TEMPORARY-ASGNMT-FLAG           PIC X   VALUE SPACE.         00850003
009400     88  TEMP-ASGN-UFP                       VALUE 'U'.           00860003
009500     88  TEMP-ASGN-XB                        VALUE 'X'.           00870003
009600     88  TEMP-ASGN-AJ                        VALUE 'A'.           00880003
009700 01  TEMPORARY-ASGNMT.                                            00890003
009800     02  TA-DIST                     PIC XX   VALUE SPACE.        00900003
009900     02  TA-SUB-DIST                 PIC XX   VALUE SPACE.        00910003
010000     02  TA-AREA.                                                 00920003
010100         03  TA-1                    PIC X(06).                   00930003
010200         03  TA-2 REDEFINES TA-1.                                 00940003
010300             04  TA-POOL             PIC XX.                      00950003
010400             04  TA-TURN             PIC X(4).                    00960003
010500         03  TA-3 REDEFINES TA-1.                                 00970003
010600             04  TA-FILLER           PIC XX.                      00980003
010700             04  TA-XB-TURN          PIC X(4).                    00990003
010800         03  TA-CC                   PIC XX  VALUE SPACE.         01000003
010900                                                                  01010003
011000 01  ON-DUTY-ASGNMT-FLAG             PIC X   VALUE SPACE.         01020003
011100     88  ON-DUTY-UFP                         VALUE 'U'.           01030003
011200     88  ON-DUTY-AJ                          VALUE 'A'.           01040003
011300 01  ON-DUTY-ASGNMT.                                              01050003
011400     02  OD-DIST                     PIC XX  VALUE SPACE.         01060003
011500     02  OD-SUB-DIST                 PIC XX  VALUE SPACE.         01070003
011600     02  OD-AREA.                                                 01080003
011700         03  OD-1                    PIC X(06).                   01090003
011800         03  OD-2 REDEFINES OD-1.                                 01100003
011900             04  OD-POOL             PIC XX.                      01110003
012000             04  OD-TURN             PIC X(4).                    01120003
012100         03  OD-CC                   PIC XX  VALUE SPACE.         01130003
012200                                                                  01140003
012300 01  WORK-ASGNKEY1.                                               01150003
012400     02  WK-ASGN-JOB-TYPE            PIC X  VALUE ' '.            01160003
012500     02  WK-ASGN-ASSIGNMENT.                                      01170003
012600         03  WK-ASGN-DIST            PIC XX VALUE SPACE.          01180003
012700         03  WK-ASGN-SUB-DIST        PIC XX VALUE SPACE.          01190003
012800         03  WK-ASGN-ASSIGN.                                      01200003
012900             04  WK-ASGN-POOL        PIC XX VALUE SPACE.          01210003
013000             04  WK-ASGN-TURN        PIC X(4) VALUE SPACE.        01220003
013100         03  WK-ASGN-CC              PIC XX VALUE SPACE.          01230003
013200     02  WK-ASGN-REC-TYPE            PIC X  VALUE '1'.            01240003
013300     02  WK-ASGN-DATE-TIME           PIC 9(10) VALUE 0.           01250003
013400                                                                  01260003
013500 01  WORK-ASGNKEY2.                                               01270003
013600     02  WK-ASGN-EMP-NO              PIC 9(9) VALUE 0.            01280003
013700     02  WK-ASGN-EMP-REC-TYPE        PIC X  VALUE SPACE.          01290003
013800     02  WK-ASGN-EMP-DATE-TIME       PIC 9(10) VALUE 0.           01300003
013900                                                                  01310003
014000 01  WS-SAVE-ASGN-FILE               PIC X(128).                  01320003
014100                                                                  01330003
014200*01  WS-SYSTEM-DATE-TIME.                                         01340003
014300*    02  WS-SYSTEM-DATE              PIC 9(06).                   01350003
014400*    02  WS-SYSTEM-TIME.                                          01360003
014500*        03  WS-SYSTEM-HRMN          PIC 9(04).                   01370003
014600*        03  FILLER                  PIC 9(04).                   01380003
014700*01  WS-CURRENT-DATE-TIME                                         01390003
014800*    REDEFINES WS-SYSTEM-DATE-TIME.                               01400003
014900*    02  WS-CURR-DATE-TIME           PIC X(10).                   01410003
015000*    02  FILLER                      PIC X(04).                   01420003
015100*01  WS-LOCAL-DATE-TIME.                                          01430003
015200*    02  WS-LOCAL-DATE               PIC X(06).                   01440003
015300*    02  WS-LOCAL-TIME               PIC X(04).                   01450003
015400                                                                  01460003
015500******************************************************************01470003
015600***                  TEMPORARY STORAGE QUEUE                   ***01480003
015700******************************************************************01490003
015800 01  P12TSQ-QUEUE-ITEM          PIC S9(4)  COMP VALUE +1.         01500003
015900 01  P12TSQ-MAP-QUEUE-ID.                                         01510003
016000     05  P12TSQ-MAP-QUEUE       PIC X(4)   VALUE '12M'.           01520003
016100     05  P12TSQ-MAP-TERM-ID     PIC X(4)   VALUE SPACES.          01530003
016200 01  P12TSQ-CA-QUEUE-ID.                                          01540003
016300     05  P12TSQ-CA-QUEUE        PIC X(4)   VALUE '12C'.           01550003
016400     05  P12TSQ-CA-TERM-ID      PIC X(4)   VALUE SPACES.          01560003
016500 01  P12TSQ-QLGTH               PIC S9(4)  COMP VALUE +1.         01570003
016600***                                                               01580003
016700*C1190 BEGIN                                                      01590003
016800 01  P12TSQ-DELTA-ID.                                             01600003
016900     05  P12TSQ-CA-QUEUE        PIC X(4)   VALUE '12D'.           01610003
017000     05  P12TSQ-DELTA-TERM-ID   PIC X(4)   VALUE SPACES.          01620003
017100 01  P12-DELTA-QLGTH            PIC S9(4)  COMP VALUE +18.        01630003
017200 01  P12-DELTA-FIELDS.                                            01640003
017300     05  P12TSQ-DELTA-ASGN-ID   PIC X(6)   VALUE SPACES.          01650003
017400     05  P12TSQ-DELTA-FROM-DATE PIC X(6)   VALUE SPACES.          01660003
017500     05  P12TSQ-DELTA-TO-DATE   PIC X(6)   VALUE SPACES.          01670003
017600*C1190 END                                                        01680003
017700***************************************************************** 01690003
017800***                 I/O STATUS CHECK FIELDS                       01700003
017900***************************************************************** 01710003
018000 01  WS-RESPONSE                     PIC S9(8) COMP VALUE ZEROES. 01720003
018100 01  FILE-STATUS                     PIC 9(4)  VALUE ZEROES.      01730003
018200     COPY IOCODES.                                                01740003
018300***************************************************************** 01750003
018400***                    COMMAREA COPYBOOKS                         01760003
018500***************************************************************** 01770003
018600     COPY PSTCOMM.                                                01780003
018700     COPY P998COMM.                                               01790003
018800     COPY P12COMM.                                                01800003
018900     COPY P12ACOMM.                                               01810003
019000***************************************************************** 01820003
019100***                     MAP AREA COPYBOOK                         01830003
019200***************************************************************** 01840003
019300     COPY PSM12RE.                                                01850003
019400***************************************************************** 01860003
019500***                   PROGRAM NAMES COPYBOOKS                     01870003
019600***************************************************************** 01880003
019700     COPY PSTCB03.                                                01890003
019800     COPY PSTCB12.                                                01900003
019900     COPY PSTCB12A.                                               01910003
020000     COPY PSTCB998.                                               01920003
020100***************************************************************** 01930003
020200***                 CALLED ROUTINES COPYBOOKS.                    01940003
020300***************************************************************** 01950003
020400     COPY PSTERAR.                                                01960003
020500     COPY P903COMM.                                               01970003
020600     COPY P922COMM.                                               01980003
020700***************************************************************** 01990003
020800***                     FILE COPYBOOKS                            02000003
020900***************************************************************** 02010003
021000     COPY WSAJ.                                                   02020003
021100     COPY WSJS.                                                   02030003
021200     COPY WSASGN.                                                 02040003
021300     COPY WSCNTL.                                                 02050003
021400     COPY WSMSG.                                                  02060003
021500     COPY WSMSTR.                                                 02070003
021600     COPY WSSWASGN.                                               02080003
021700     COPY WSTRCN.                                                 02090003
021800***************************************************************** 02100003
021900***                     MISC. COPYBOOKS                           02110003
022000***************************************************************** 02120003
022100     COPY PSTATTR.                                                02130003
022200     COPY PSTCCRFT.                                               02140003
022300     COPY PSTKEYS.                                                02150003
022400     COPY WSAJDEFN.                                               02160003
022500     COPY WSDAYWK.                                                02170003
022600     COPY WSZONE.                                                 02180003
022700     COPY WSEDDATE.                                               02190003
022800     COPY WSSYDTTM.                                               02200003
022900     COPY WSBUFFER.                                               02210003
023000*                                                                 02220003
023100 LINKAGE SECTION.                                                 02230003
023200*                                                                 02240003
023300 01  DFHCOMMAREA.                                                 02250003
023400     05  DFHCOMMAREA-1               PIC X(170).                  02260003
023500*                                                                 02270003
023600 PROCEDURE DIVISION.                                              02280003
023700*                                                                 02290003
023800 P0000-MAIN-CONTROL.                                              02300003
023900*                                                                 02310003
024000     EXEC CICS IGNORE                                             02320003
024100               CONDITION                                          02330003
024200               ERROR                                              02340003
024300     END-EXEC                                                     02350003
024400     EXEC CICS HANDLE                                             02360003
024500               ABEND                                              02370003
024600               LABEL(P9999-GOT-PROBLEM)                           02380003
024700     END-EXEC                                                     02390003
024800*                                                                 02400003
024900     MOVE ZEROES                 TO WS-ABSTIME-OFFSET             02410003
025000*                                                                 02420003
025100     IF EIBCALEN = ZERO                                           02430003
025200        PERFORM P9990-CLEAR-SCREEN                                02440003
025300     END-IF                                                       02450003
025400     MOVE DFHCOMMAREA-1            TO PSTCOMM-AREA                02460003
025500     IF EIBTRNID NOT = P12-TRAN                                   02470003
025600        SET CREATE-SCREEN          TO TRUE                        02480003
025700        MOVE LOW-VALUES            TO PSTS12                      02490003
025800        IF EIBTRNID = P998-TRAN                                   02500003
025900           MOVE P998CA-CURSOR-POS TO EIBCPOSN                     02510003
026000           PERFORM P7010-READ-TSQUEUE                             02520003
026100           PERFORM P9000-SEND-MAP-AND-RETURN                      02530003
026200        END-IF                                                    02540003
026300        PERFORM P0050-CLEAR-SCREEN                                02550003
026400        MOVE PSTCA-DIST            TO SCR12-DIST                  02560003
026500        MOVE PSTCA-SUB-DIST        TO SCR12-SUB-DIST              02570003
026600        IF P12CA-ASGN-ID > SPACES                                 02580003
026700           MOVE P12CA-ASGN-ID      TO SCR12-ASGN-ID               02590003
026800*C1190 BEGIN                                                      02600003
026900           MOVE P12CA-ASGN-ID      TO P12TSQ-DELTA-ASGN-ID        02610003
027000*C1190 END                                                        02620003
027100           SET ENTER-KEY           TO TRUE                        02630003
027200           PERFORM P1000-INQUIRE                                  02640003
027300        ELSE                                                      02650003
027400           MOVE SPACES             TO SCR12-ASGN-ID               02660003
027500                                      P12COMM-AREA                02670003
027600           MOVE -1                 TO SCR12-ASGN-ID-CURSOR        02680003
027700        END-IF                                                    02690003
027800        PERFORM P9000-SEND-MAP-AND-RETURN                         02700003
027900     END-IF                                                       02710003
028000     MOVE EIBAID   TO PF-CHECK                                    02720003
028100     IF EXIT-KEY OR MAPFAIL                                       02730003
028200*C1190 BEGIN                                                      02740003
028300        PERFORM P7400-DELETE-DELTA-QUEUE                          02750003
028400*C1190 END                                                        02760003
028500        PERFORM P9100-XCTL-TO-CALLING-PROGRAM                     02770003
028600     END-IF                                                       02780003
028700     PERFORM P0100-PROCESS-INPUT                                  02790003
028800     PERFORM P9000-SEND-MAP-AND-RETURN.                           02800003
028900*                                                                 02810003
029000 P0050-CLEAR-SCREEN.                                              02820003
029100*                                                                 02830003
029200     MOVE SPACES                   TO SCR12-JOB-DESC              02840003
029300                                      SCR12-ORIGIN-STN            02850003
029400                                      SCR12-FINAL-STN             02860003
029500                                      SCR12-PROFILE               02870003
029600                                      SCR12-A-FROM-DATE           02880003
029700                                      SCR12-A-TO-DATE             02890003
029800                                      SCR12-ERRORMSG              02900003
029900*                                                                 02910003
030000     PERFORM VARYING SUB1 FROM 1 BY 1                             02920003
030100             UNTIL SUB1 > ARRAY-MAX                               02930003
030200        MOVE SPACES                TO SCR12-CRAFT(SUB1)           02940003
030300                                      SCR12-EMP-NAME(SUB1)        02950003
030400                                      SCR12-EMP-ST(SUB1)          02960003
030500                                      SCR12-EMP-EC(SUB1)          02970003
030600                                      SCR12-EMP-MTOD(SUB1)        02980003
030700                                      SCR12-EMP-USHR(SUB1)        02990003
030800                                      SCR12-EMP-PERS-TM(SUB1)     03000003
030900                                      SCR12-EMP-PERS-DY(SUB1)     03010003
031000                                      SCR12-REST-DAYS(SUB1)       03020003
031100                                      SCR12-EMP-MSG(SUB1)         03030003
031200     END-PERFORM                                                  03040003
031300     IF P12CA-INQ-ONLY                                            03050003
031400        IF PSTCA-SUB = 1                                          03060003
031500           STRING '                     '                         03070003
031600                  'PF1=HELP    PFKEY3=EXIT'                       03080003
031700                  DELIMITED BY SIZE                               03090003
031800                  INTO SCR12-PFKEY-LINE                           03100003
031900        ELSE                                                      03110003
032000           STRING '                    '                          03120003
032100                  'PF1=AIDE    PFKEY3=SORTIE'                     03130003
032200                  DELIMITED BY SIZE                               03140003
032300                  INTO SCR12-PFKEY-LINE                           03150003
032400        END-IF                                                    03160003
032500        MOVE AUTOSKIP-MDT           TO SCR12-ASGN-ID-ATTR         03170003
032600                                       SCR12-A-FROM-DATE-ATTR     03180003
032700                                       SCR12-A-TO-DATE-ATTR       03190003
032800     ELSE                                                         03200003
032900        MOVE MDT                    TO SCR12-ASGN-ID-ATTR         03210003
033000                                       SCR12-A-FROM-DATE-ATTR     03220003
033100                                       SCR12-A-TO-DATE-ATTR       03230003
033200        IF PSTCA-SUB = 1                                          03240003
033300           STRING 'ENTER=INQUIRE  '                               03250003
033400                  'F1=HELP  '                                     03260003
033500                  'F3=EXIT  '                                     03270003
033600                  'F5=UPDATE  '                                   03280003
033700                  'F10=ASGN DETAIL  '                             03290003
033800                  'F12=NEXT ASGN  '                               03300003
033900                  DELIMITED BY SIZE                               03310003
034000                  INTO SCR12-PFKEY-LINE                           03320003
034100        ELSE                                                      03330003
034200           STRING 'ENTER=RENSEIG  '                               03340003
034300                  'F1=AIDE  '                                     03350003
034400                  'F3=SORTIE  '                                   03360003
034500                  'F5=UPDATE  '                                   03370003
034600                  'F10=DETAILS AFF  '                             03380003
034700                  'F12=PROC AFF   '                               03390003
034800                  DELIMITED BY SIZE                               03400003
034900                  INTO SCR12-PFKEY-LINE                           03410003
035000        END-IF                                                    03420003
035100     END-IF.                                                      03430003
035200*                                                                 03440003
035300 P0100-PROCESS-INPUT.                                             03450003
035400*                                                                 03460003
035500     MOVE P12-MAP-VERSION(PSTCA-SUB) TO P12-MAP                   03470003
035600     EXEC CICS RECEIVE MAP(P12-MAP)                               03480003
035700                       MAPSET(P12-SET)                            03490003
035800                       INTO(PSTS12)                               03500003
035900                       RESP(WS-RESPONSE)                          03510003
036000     END-EXEC                                                     03520003
036100     MOVE WS-RESPONSE                TO FILE-STATUS               03530003
036200     IF NOT SUCCESS                                               03540003
036300        MOVE 'P0100'                 TO ERR-PARAGRAPH             03550003
036400        PERFORM P9999-GOT-PROBLEM                                 03560003
036500     END-IF                                                       03570003
036600*C1190 BEGIN                                                      03580003
036700     PERFORM P7500-READ-DELTA-QUEUE                               03590003
036800*C1190 END                                                        03600003
036900     IF PFKEY1                                                    03610003
037000        PERFORM P7000-WRITE-TSQUEUE                               03620003
037100        PERFORM P9500-SETUP-SCR998                                03630003
037200     END-IF                                                       03640003
037300*                                                                 03650003
037400*    RESET ATTRIBUTES AND INITIALIZE MAP AREA                     03660003
037500*                                                                 03670003
037600     MOVE DEFAULT-ATTR               TO SCR12-ASGN-ID-HI          03680003
037700                                        SCR12-A-FROM-DATE-HI      03690003
037800                                        SCR12-A-TO-DATE-HI        03700003
037900     MOVE SPACES                     TO SCR12-ERRORMSG            03710003
038000*                                                                 03720003
038100     IF P12CA-INQ-ONLY                                            03730003
038200        OR NOT (ENTER-KEY OR PFKEY2 OR PFKEY5 OR PFKEY10 OR       03740003
038300                PFKEY12)                                          03750003
038400        MOVE -1                      TO SCR12-ASGN-ID-CURSOR      03760003
038500*            INVALID-FUNC-MSG                                     03770003
038600        MOVE 'I006'                  TO MSGLOG-CODE               03780003
038700        PERFORM P9000-SEND-MAP-AND-RETURN                         03790003
038800     END-IF                                                       03800003
038900     IF ENTER-KEY                                                 03810003
039000        IF SCR12-ASGN-ID NOT > SPACES                             03820003
039100           MOVE -1                   TO SCR12-ASGN-ID-CURSOR      03830003
039200           MOVE REV-VIDEO            TO SCR12-ASGN-ID-HI          03840003
039300*               REQD-FIELD-MSG                                    03850003
039400           MOVE 'P039'               TO MSGLOG-CODE               03860003
039500           PERFORM P9000-SEND-MAP-AND-RETURN                      03870003
039600        END-IF                                                    03880003
039700     END-IF                                                       03890003
039800*                                                                 03900003
039900     IF PFKEY10                                                   03910003
040000*C1190 BEGIN                                                      03920003
040100        PERFORM P7400-DELETE-DELTA-QUEUE                          03930003
040200*C1190 END                                                        03940003
040300        PERFORM P9200-SETUP-SCR12A                                03950003
040400     ELSE                                                         03960003
040500        IF PFKEY5                                                 03970003
040600           PERFORM P2000-UPDATE                                   03980003
040700        ELSE                                                      03990003
040800           PERFORM P1000-INQUIRE                                  04000003
040900        END-IF                                                    04010003
041000     END-IF.                                                      04020003
041100*                                                                 04030003
041200 P1000-INQUIRE.                                                   04040003
041300*                                                                 04050003
041400     PERFORM P0050-CLEAR-SCREEN                                   04060003
041500*                                                                 04070003
041600*    SELECT THE NEXT ASSIGNMENT TO BE VIEWED FROM THE PROFILES    04080003
041700*                                                                 04090003
041800     MOVE SCR12-DIST                 TO TRCN-DIST3                04100003
041900     MOVE SCR12-SUB-DIST             TO TRCN-SDIST3               04110003
042000     IF SCR12-ASGN-ID NOT > SPACES                                04120003
042100        MOVE '     A'                TO TRCN-ASSIGNMENT           04130003
042200     ELSE                                                         04140003
042300        MOVE SCR12-ASGN-ID           TO TRCN-ASSIGNMENT           04150003
042400     END-IF                                                       04160003
042500     MOVE TRCN-KEY3                  TO TRCNKEY3                  04170003
042600     IF PFKEY12                                                   04180003
042700        EXEC CICS STARTBR                                         04190003
042800                  DATASET(TRAIN-CN-VIA-DSD-ASGN)                  04200003
042900                  RIDFLD(TRCNKEY3)                                04210003
043000                  GTEQ                                            04220003
043100                  RESP(WS-RESPONSE)                               04230003
043200        END-EXEC                                                  04240003
043300        MOVE WS-RESPONSE             TO FILE-STATUS               04250003
043400        IF SUCCESS                                                04260003
043500           EXEC CICS READNEXT                                     04270003
043600                     DATASET(TRAIN-CN-VIA-DSD-ASGN)               04280003
043700                     INTO(WS-TRCN-FILE)                           04290003
043800                     LENGTH(TRAINCN-DSD-RLGTH)                    04300003
043900                     RIDFLD(TRCNKEY3)                             04310003
044000                     KEYLENGTH(TRAINCN-DSD-KLGTH)                 04320003
044100                     RESP(WS-RESPONSE)                            04330003
044200           END-EXEC                                               04340003
044300           MOVE WS-RESPONSE          TO FILE-STATUS               04350003
044400           IF SUCCESS                                             04360003
044500              IF SCR12-ASGN-ID = P12CA-ASGN-ID                    04370003
044600                 AND SCR12-ASGN-ID > SPACES                       04380003
044700                 EXEC CICS READNEXT                               04390003
044800                           DATASET(TRAIN-CN-VIA-DSD-ASGN)         04400003
044900                           INTO(WS-TRCN-FILE)                     04410003
045000                           LENGTH(TRAINCN-DSD-RLGTH)              04420003
045100                           RIDFLD(TRCNKEY3)                       04430003
045200                           KEYLENGTH(TRAINCN-DSD-KLGTH)           04440003
045300                           RESP(WS-RESPONSE)                      04450003
045400                 END-EXEC                                         04460003
045500                 MOVE WS-RESPONSE    TO FILE-STATUS               04470003
045600              END-IF                                              04480003
045700           END-IF                                                 04490003
045800        END-IF                                                    04500003
045900        IF SUCCESS                                                04510003
046000           IF TRCN-DIST3 NOT = SCR12-DIST                         04520003
046100              OR TRCN-SDIST3 NOT = SCR12-SUB-DIST                 04530003
046200              SET NO-RECORD-FND      TO TRUE                      04540003
046300           END-IF                                                 04550003
046400        END-IF                                                    04560003
046500        EXEC CICS ENDBR                                           04570003
046600                  DATASET(TRAIN-CN-VIA-DSD-ASGN)                  04580003
046700                  RESP(WS-RESPONSE)                               04590003
046800        END-EXEC                                                  04600003
046900     ELSE                                                         04610003
047000        EXEC CICS READ                                            04620003
047100                  DATASET(TRAIN-CN-VIA-DSD-ASGN)                  04630003
047200                  INTO(WS-TRCN-FILE)                              04640003
047300                  LENGTH(TRAINCN-DSD-RLGTH)                       04650003
047400                  RIDFLD(TRCNKEY3)                                04660003
047500                  KEYLENGTH(TRAINCN-DSD-KLGTH)                    04670003
047600                  RESP(WS-RESPONSE)                               04680003
047700        END-EXEC                                                  04690003
047800        MOVE WS-RESPONSE             TO FILE-STATUS               04700003
047900     END-IF                                                       04710003
048000     IF NOT SUCCESS                                               04720003
048100        IF (NO-RECORD-FND OR END-OF-FILE)                         04730003
048200           MOVE -1                   TO SCR12-ASGN-ID-CURSOR      04740003
048300           MOVE SPACES               TO P12CA-ASGN-ID             04750003
048400           IF PFKEY12                                             04760003
048500*                  END-FILE-MSG                                   04770003
048600              MOVE 'E011'            TO MSGLOG-CODE               04780003
048700           ELSE                                                   04790003
048800*                  NOT-FOUND-MSG                                  04800003
048900              MOVE 'N015'            TO MSGLOG-CODE               04810003
049000           END-IF                                                 04820003
049100           PERFORM P9000-SEND-MAP-AND-RETURN                      04830003
049200        ELSE                                                      04840003
049300           MOVE 'P1000-1'            TO ERR-PARAGRAPH             04850003
049400           MOVE TRCNKEY3             TO ERR-KEY                   04860003
049500           PERFORM P9999-GOT-PROBLEM                              04870003
049600        END-IF                                                    04880003
049700     END-IF                                                       04890003
049800     PERFORM P1010-SETUP-NEW-JOB                                  04900003
049900*                                                                 04910003
050000*    SETUP CRAFT ARRAY                                            04920003
050100*                                                                 04930003
050200     PERFORM VARYING SUB1 FROM 1 BY 1                             04940003
050300        UNTIL SUB1 > ARRAY-MAX                                    04950003
050400        IF SCR12-CRAFT(SUB1) > SPACES                             04960003
050500           MOVE SPACES              TO AJJOBKEY-AREA              04970003
050600           MOVE SCR12-DIST          TO AJ-JOB-DIST                04980003
050700           MOVE SCR12-SUB-DIST      TO AJ-JOB-SUB-DIST            04990003
050800           MOVE SCR12-ASGN-ID       TO AJ-JOB-ASGN-ID             05000003
050900           MOVE SCR12-CRAFT(SUB1)   TO AJ-JOB-ASGN-CC             05010003
051000           MOVE AJJOBKEY-AREA       TO AJJOBKEY                   05020003
051100           EXEC CICS READ                                         05030003
051200                     DATASET(AJ-VIA-JNAME-JCRAFT)                 05040003
051300                     INTO(WS-ASGNED-JOBS)                         05050003
051400                     LENGTH(AJNAMECR-RLGTH)                       05060003
051500                     RIDFLD(AJJOBKEY)                             05070003
051600                     KEYLENGTH(AJNAMECR-KLGTH)                    05080003
051700                     RESP(WS-RESPONSE)                            05090003
051800           END-EXEC                                               05100003
051900           MOVE WS-RESPONSE         TO FILE-STATUS                05110003
052000           IF SUCCESS                                             05120003
052100*C1190 BEGIN                                                      05130003
052200              IF AJ-JOB-ASGN-ID     > SPACES                      05140003
052300                MOVE AJ-JOB-ASGN-ID TO P12TSQ-DELTA-ASGN-ID       05150003
052400              ELSE                                                05160003
052500                MOVE SPACES         TO P12TSQ-DELTA-ASGN-ID       05170003
052600              END-IF                                              05180003
052700*C1190 END                                                        05190003
052800              IF (P12CA-INQ-ONLY AND P12CA-CALL-DAY-PICX NUMERIC) 05200003
052900                 PERFORM P1030-GET-JOB-SCHED                      05210003
053000              END-IF                                              05220003
053100              PERFORM P1020-DISPLAY-JOB-DATA                      05230003
053200           ELSE                                                   05240003
053300              IF NOT (NO-RECORD-FND OR END-OF-FILE)               05250003
053400                 MOVE 'P1000-2'     TO ERR-PARAGRAPH              05260003
053500                 MOVE AJJOBKEY      TO ERR-KEY                    05270003
053600                 PERFORM P9999-GOT-PROBLEM                        05280003
053700              END-IF                                              05290003
053800           END-IF                                                 05300003
053900        END-IF                                                    05310003
054000     END-PERFORM                                                  05320003
054100     MOVE -1                        TO SCR12-ASGN-ID-CURSOR.      05330003
054200*                                                                 05340003
054300 P1010-SETUP-NEW-JOB.                                             05350003
054400*                                                                 05360003
054500     EXEC CICS ASKTIME                                            05370003
054600               ABSTIME(WS-ABSTIME)                                05380003
054700     END-EXEC                                                     05390003
054800*                                                                 05400003
054900     ADD WS-ABSTIME-OFFSET TO WS-ABSTIME                          05410003
055000*                                                                 05420003
055100     EXEC CICS FORMATTIME                                         05430003
055200               ABSTIME(WS-ABSTIME)                                05440003
055300               YYYYMMDD(WS-SYSTEM-DATE-CENT)                      05450003
055400               TIME(WS-SYSTEM-TIME-AREA)                          05460003
055500     END-EXEC                                                     05470003
055600*                                                                 05480003
055700*    INSTALL APPLICATION DATE/TIME                                05490003
055800*                                                                 05500003
055900     IF PSTCA-DATE-TIME-OFFSET > SPACES                           05510003
056000        MOVE ZEROS                  TO DATE-CONVERSION-PARMS      05520003
056100        MOVE WS-SYSTEM-DATE         TO PARM-PRI-DATE-GREG         05530003
056200        MOVE WS-SYSTEM-TIME         TO PARM-PRI-HRMN              05540003
056300        PERFORM P9810-PROCESS-OFFSET                              05550003
056400        MOVE PARM-RES-GREG-CENT     TO WS-SYSTEM-CENT             05560003
056500        MOVE PARM-RES-DATE-GREG     TO WS-SYSTEM-DATE             05570003
056600        MOVE PARM-RES-HRMN          TO WS-SYSTEM-TIME             05580003
056700     END-IF                                                       05590003
056800*                                                                 05600003
056900*    SEE IF THIS SUB DISTRICT IS A DAILY MARK YARD                05610003
057000*                                                                 05620003
057100     MOVE SPACES                    TO CNTLKEY-AREA               05630003
057200     MOVE '02'                      TO CNTL-REC-TYPE              05640003
057300     MOVE TRCN-DIST                 TO CNTL-DIST                  05650003
057400     MOVE TRCN-SDIST                TO CNTL-SUB-DIST              05660003
057500     MOVE CNTLKEY-AREA              TO CNTLKEY                    05670003
057600     PERFORM P8000-READ-CNTLFILE                                  05680003
057700     IF SUCCESS                                                   05690003
057800        MOVE CNTL-DAILY-MARKUP      TO WS-DAILY-MARK-CODE         05700003
057900        MOVE CNTL-TIME-ZONE         TO PSTCA-TIME-ZONE            05710003
058000     ELSE                                                         05720003
058100        MOVE 'P1010-1'              TO ERR-PARAGRAPH              05730003
058200        MOVE CNTLKEY                TO ERR-KEY                    05740003
058300        PERFORM P9999-GOT-PROBLEM                                 05750003
058400     END-IF                                                       05760003
058500*                                                                 05770003
058600*    CONVERT THE SYSTEM DATE/TIME TO LOCAL DATE/TIME              05780003
058700*                                                                 05790003
058800     MOVE SPACES                    TO TZ-PARAMETERS              05800003
058900     MOVE WS-PRESENT-TIME           TO TZ-IN-DATE-TIME            05810003
059000     SET TZ-IN-SYSTEM-ZONE          TO TRUE                       05820003
059100     MOVE PSTCA-TIME-ZONE           TO TZ-OUT-ZONE                05830003
059200     PERFORM P8996-TIMEZONE                                       05840003
059300     MOVE TZ-OUT-DATE-TIME-CENT     TO WS-LOCAL-DATE-TIME-CENT    05850003
059400*                                                                 05860003
059500*    CONVERT THE CURRENT LOCAL DATE/TIME TO A DAY OF THE WEEK     05870003
059600*                                                                 05880003
059700     MOVE ZEROS                     TO DATE-CONVERSION-PARMS      05890003
059800     SET PARM-CONV                  TO TRUE                       05900003
059900     MOVE WS-LOCAL-DATE             TO PARM-PRI-DATE-GREG         05910003
060000     EXEC CICS LINK                                               05920003
060100               PROGRAM(P903-PGM)                                  05930003
060200               COMMAREA(DATE-CONVERSION-PARMS)                    05940003
060300               LENGTH(P903-LGTH)                                  05950003
060400               RESP(WS-RESPONSE)                                  05960003
060500     END-EXEC                                                     05970003
060600     MOVE WS-RESPONSE               TO FILE-STATUS                05980003
060700     IF NOT SUCCESS                                               05990003
060800        MOVE 'P1010-2'              TO ERR-PARAGRAPH              06000003
060900        MOVE 'P903LINK'             TO ERR-KEY                    06010003
061000        PERFORM P9999-GOT-PROBLEM                                 06020003
061100     END-IF                                                       06030003
061200     MOVE PARM-PRI-DAY-OF-WEEK      TO DAY-OF-WK                  06040003
061300*                                                                 06050003
061400     MOVE TRCN-DESCRIPTION          TO SCR12-JOB-DESC             06060003
061500     MOVE TRCN-DIST                 TO SCR12-DIST                 06070003
061600     MOVE TRCN-SDIST                TO SCR12-SUB-DIST             06080003
061700     MOVE TRCN-ASSIGNMENT           TO SCR12-ASGN-ID              06090003
061800                                       P12CA-ASGN-ID              06100003
061900*C1190 BEGIN                                                      06110003
062000                                       P12TSQ-DELTA-ASGN-ID       06120003
062100*C1190 END                                                        06130003
062200     MOVE TRCN-ORIGIN-STN           TO SCR12-ORIGIN-STN           06140003
062300     MOVE TRCN-FNL-STN              TO SCR12-FINAL-STN            06150003
062400     MOVE TRCN-TRAIN-ASGN           TO SCR12-PROFILE              06160003
062500     SET JOB-IND                    TO 1                          06170003
062600     MOVE TRCN-ASSIGNMENT           TO JOB-DEF-CHECK              06180003
062700     SEARCH JOB-ENTRIES VARYING JOB-IND                           06190003
062800        AT END                                                    06200003
062900           MOVE 1 TO SUB2                                         06210003
063000        WHEN JOB-DEF-TBL-CLASS(JOB-IND) = JOB-DEF-CLASS           06220003
063100           SET SUB2                 TO JOB-IND                    06230003
063200     PERFORM VARYING SUB1 FROM 1 BY 1                             06240003
063300        UNTIL SUB1 > ARRAY-MAX                                    06250003
063400        MOVE JOB-DEF-TBL-CRAFT-CODE(SUB2, SUB1)                   06260003
063500                                    TO SCR12-CRAFT(SUB1)          06270003
063600        SET CT-IND                  TO 1                          06280003
063700        SEARCH CRAFT-CODE-TABLE VARYING CT-IND                    06290003
063800           AT END CONTINUE                                        06300003
063900           WHEN CT-CRAFT-CODE(CT-IND) = SCR12-CRAFT(SUB1)         06310003
064000              IF CT-CRAFT-CODE(CT-IND) > SPACE                    06320003
064100                 MOVE CT-CRAFT-COLOR(CT-IND)                      06330003
064200                                    TO SCR12-CRAFT-COLOR(SUB1)    06340003
064300                                       SCR12-EMP-NAME-COLOR(SUB1) 06350003
064400              END-IF                                              06360003
064500        END-SEARCH                                                06370003
064600     END-PERFORM.                                                 06380003
064700*                                                                 06390003
064800 P1020-DISPLAY-JOB-DATA.                                          06400003
064900*                                                                 06410003
065000     IF  AJ-ANNUL-FROM-DATE > '000000'                            06420003
065100*C1190 BEGIN                                                      06430003
065200        MOVE AJ-ANNUL-FROM-DATE     TO SCR12-A-FROM-DATE          06440003
065300        MOVE AJ-ANNUL-FROM-DATE     TO P12TSQ-DELTA-FROM-DATE     06450003
065400     ELSE                                                         06460003
065500        MOVE SPACES                 TO SCR12-A-FROM-DATE          06470003
065600        MOVE SPACES                 TO P12TSQ-DELTA-FROM-DATE     06480003
065700     END-IF                                                       06490003
065800     IF  AJ-ANNUL-TO-DATE > '000000'                              06500003
065900        MOVE AJ-ANNUL-TO-DATE       TO SCR12-A-TO-DATE            06510003
066000        MOVE AJ-ANNUL-TO-DATE       TO P12TSQ-DELTA-TO-DATE       06520003
066100     ELSE                                                         06530003
066200        MOVE SPACES                 TO SCR12-A-TO-DATE            06540003
066300        MOVE SPACES                 TO P12TSQ-DELTA-TO-DATE       06550003
066400     END-IF                                                       06560003
066500     PERFORM P7400-DELETE-DELTA-QUEUE                             06570003
066600     PERFORM P7600-WRITE-DELTA-QUEUE                              06580003
066700*C1190 END                                                        06590003
066800     PERFORM P8020-GET-ASGN-EMPS                                  06600003
066900     IF HAVE-ASGN-TEMPORARY-EMP OR HAVE-ASGN-OWNER                06610003
067000        OR (P12CA-INQ-ONLY AND P12CA-CALL-DAY-PICX IS NUMERIC     06620003
067100           AND HAVE-ASGN-ON-DUTY-EMP)                             06630003
067200        IF (P12CA-INQ-ONLY AND P12CA-CALL-DAY-PICX IS NUMERIC     06640003
067300           AND HAVE-ASGN-ON-DUTY-EMP)                             06650003
067400           MOVE ASGN-ON-DUTY-EMP    TO MSTRNBRK                   06660003
067500        ELSE                                                      06670003
067600           IF HAVE-ASGN-TEMPORARY-EMP                             06680003
067700              MOVE ASGN-TEMPORARY-EMP                             06690003
067800                                    TO MSTRNBRK                   06700003
067900           ELSE                                                   06710003
068000              MOVE ASGN-OWNER       TO MSTRNBRK                   06720003
068100           END-IF                                                 06730003
068200        END-IF                                                    06740003
068300        PERFORM P8500-READ-MASTER                                 06750003
068400        IF DIST IN WS-MSTR NOT = SCR12-DIST                       06760003
068500           OR SUB-DIST OF WS-MSTR NOT = SCR12-SUB-DIST            06770003
068600           PERFORM P8060-GET-EMP-TIME-ZONE                        06780003
068700           MOVE SPACES              TO TZ-PARAMETERS              06790003
068800           MOVE CNTL-TIME-ZONE      TO TZ-IN-ZONE                 06800003
068900           MOVE PSTCA-TIME-ZONE     TO TZ-OUT-ZONE                06810003
069000           IF EMP-MTOD > '0000000000'                             06820003
069100              MOVE EMP-MTOD         TO TZ-IN-DATE-TIME            06830003
069200              PERFORM P8996-TIMEZONE                              06840003
069300              MOVE TZ-OUT-DATE-TIME TO EMP-MTOD                   06850003
069400           END-IF                                                 06860003
069500*CNLD-309 B  COMMENTING                                           06870003
069500*          IF EMP-MTOY > '0000000000'                             06880003
069600*             MOVE EMP-MTOY         TO TZ-IN-DATE-TIME            06890003
069700*             PERFORM P8996-TIMEZONE                              06900003
069800*             MOVE TZ-OUT-DATE-TIME TO EMP-MTOY                   06910003
069900*          END-IF                                                 06920003
070000*          IF EMP-MTOR > '0000000000'                             06930003
070100*             MOVE EMP-MTOR         TO TZ-IN-DATE-TIME            06940003
070200*             PERFORM P8996-TIMEZONE                              06950003
070300*             MOVE TZ-OUT-DATE-TIME TO EMP-MTOR                   06960003
070400*          END-IF                                                 06970003
069500*CNLD-309 E                                                       06980003
070500           IF EMP-US-RSTD > '0000000000'                          06990003
070600              MOVE EMP-US-RSTD      TO TZ-IN-DATE-TIME            07000003
070700              PERFORM P8996-TIMEZONE                              07010003
070800              MOVE TZ-OUT-DATE-TIME TO EMP-US-RSTD                07020003
070900           END-IF                                                 07030003
071000           IF EMP-PERS-REST > '0000000000'                        07040003
071100              MOVE EMP-PERS-REST    TO TZ-IN-DATE-TIME            07050003
071200              PERFORM P8996-TIMEZONE                              07060003
071300              MOVE TZ-OUT-DATE-TIME TO EMP-PERS-REST              07070003
071400           END-IF                                                 07080003
071500        END-IF                                                    07090003
071600        IF (P12CA-INQ-ONLY AND P12CA-CALL-DAY-PICX IS NUMERIC     07100003
071700           AND HAVE-ASGN-ON-DUTY-EMP)                             07110003
071800           CONTINUE                                               07120003
071900        ELSE                                                      07130003
072000           IF HAVE-ASGN-TEMPORARY-EMP                             07140003
072100              IF TEMPORARY-ASGNMT NOT = AJJOBKEY-AREA             07150003
072200                 MOVE 'TV'          TO LAYOFF-CODE IN WS-MSTR     07160003
072300                 MOVE SPACES        TO LAYOFF-EM-CODE             07170003
072400              END-IF                                              07180003
072500           ELSE                                                   07190003
072600              IF TEMPORARY-ASGNMT > SPACE                         07200003
072700                 MOVE 'TV'          TO LAYOFF-CODE IN WS-MSTR     07210003
072800                 MOVE SPACES        TO LAYOFF-EM-CODE             07220003
072900              END-IF                                              07230003
073000           END-IF                                                 07240003
073100        END-IF                                                    07250003
073200        IF LAYOFF-CODE IN WS-MSTR = 'TV'                          07260003
073300           STRING 'TV=' TEMPORARY-ASGNMT                          07270003
073400                  DELIMITED BY SIZE                               07280003
073500                  INTO SCR12-EMP-MSG(SUB1)                        07290003
073600        END-IF                                                    07300003
073700     ELSE                                                         07310003
073800        MOVE SPACES                 TO WS-MSTR                    07320003
073900        MOVE AJ-OPEN-ASGN-MSG(PSTCA-SUB)                          07330003
074000                                    TO EMP-NAME IN WS-MSTR        07340003
074100     END-IF                                                       07350003
074200     MOVE EMP-NAME IN WS-MSTR       TO SCR12-EMP-NAME(SUB1)       07360003
074300     IF LAYOFF-CODE IN WS-MSTR = 'A0'                             07370003
074400        IF OUT-TOWN                                               07380003
074500           MOVE 'OT'                TO SCR12-EMP-ST(SUB1)         07390003
074600           IF ON-DUTY-ASGNMT NOT = AJJOBKEY-AREA                  07400003
074700              STRING 'OT='                                        07410003
074800                     ON-DUTY-ASGNMT                               07420003
074900                     DELIMITED BY SIZE                            07430003
075000                     INTO SCR12-EMP-MSG(SUB1)                     07440003
075100           END-IF                                                 07450003
075200        END-IF                                                    07460003
075300     ELSE                                                         07470003
075400        MOVE LAYOFF-CODE            TO SCR12-EMP-ST(SUB1)         07480003
075500        IF LAYOFF-EM-CODE > SPACES                                07490003
075600           MOVE LAYOFF-EM-CODE      TO SCR12-EMP-EC(SUB1)         07500003
075700        END-IF                                                    07510003
075800        IF WORKING IN WS-MSTR                                     07520003
075900           IF ON-DUTY-ASGNMT NOT = AJJOBKEY-AREA                  07530003
076000              STRING LAYOFF-CODE IN WS-MSTR                       07540003
076100                     '='                                          07550003
076200                     ON-DUTY-ASGNMT                               07560003
076300                     DELIMITED BY SIZE                            07570003
076400                     INTO SCR12-EMP-MSG(SUB1)                     07580003
076500           END-IF                                                 07590003
076600        END-IF                                                    07600003
076700     END-IF                                                       07610003
076800     MOVE SCR12-CRAFT(SUB1)         TO WS-CRAFT-CODE-CHECK        07620003
076900*                                                                 07630003
077000     SET DE-YYMMDD-FORMAT       TO TRUE                           07640003
077100     MOVE EMP-MTOD(1:6)         TO DE-YYMMDD                      07650003
077200     PERFORM P8998-DATEEDIT                                       07660003
077300     MOVE DE-CCYYMMDD           TO DE-COMPARE1-DATE               07670003
077400     MOVE EMP-MTOD(7:4)         TO DE-COMPARE1-TIME               07680003
077500*                                                                 07690003
077600     MOVE ZEROS                 TO DE-COMPARE2-DATE-TIME          07700003
077700     SET DE-YYMMDD-FORMAT       TO TRUE                           07710003
077800     MOVE EMP-PERS-REST(1:6)    TO DE-YYMMDD                      07720003
077900     PERFORM P8998-DATEEDIT                                       07730003
078000     MOVE DE-CCYYMMDD           TO DE-COMPARE2-DATE               07740003
078100     MOVE EMP-PERS-REST(7:4)    TO DE-COMPARE2-TIME               07750003
078200*                                                                 07760003
078300     SET DE-YYMMDD-FORMAT       TO TRUE                           07770003
078400     MOVE EMP-US-RSTD(1:6)      TO DE-YYMMDD                      07780003
078500     PERFORM P8998-DATEEDIT                                       07790003
078600     MOVE DE-CCYYMMDD           TO DE-COMPARE3-DATE               07800003
078700     MOVE EMP-US-RSTD(7:4)      TO DE-COMPARE3-TIME               07810003
078800*                                                                 07820003
078900     IF ENGINE-CRAFT                                              07830003
079000        IF TRCN-EN-REST-REQ = ('C' OR 'B')                        07840003
079100           OR (TRCN-EN-REST-REQ = 'N'                             07850003
079200           AND EMP-LAST-TOUR-OUT-OF-BOX)                          07860003
079300*          IF EMP-MTOD > WS-LOCAL-DATE-TIME                       07870003
079400           IF DE-COMPARE1-DATE-TIME > WS-LOCAL-DATE-TIME-CENT     07880003
079500              MOVE DE-COMPARE1-TIME TO SCR12-EMP-MTOD(SUB1)       07890003
079600           END-IF                                                 07900003
079700        END-IF                                                    07910003
079800        IF TRCN-EN-REST-REQ = ('U' OR 'B')                        07920003
079900*          IF EMP-US-RSTD > WS-LOCAL-DATE-TIME                    07930003
080000           IF DE-COMPARE3-DATE-TIME > WS-LOCAL-DATE-TIME-CENT     07940003
080100              MOVE EMP-US-RSTD-TIME TO SCR12-EMP-USHR(SUB1)       07950003
080200           END-IF                                                 07960003
080300        END-IF                                                    07970003
080400     ELSE                                                         07980003
080500        IF TRCN-TR-REST-REQ = ('C' OR 'B')                        07990003
080600           OR (TRCN-TR-REST-REQ = 'N'                             08000003
080700           AND EMP-LAST-TOUR-OUT-OF-BOX)                          08010003
080800*          IF EMP-MTOD > WS-LOCAL-DATE-TIME                       08020003
080900           IF DE-COMPARE1-DATE-TIME > WS-LOCAL-DATE-TIME-CENT     08030003
081000              MOVE DE-COMPARE1-TIME TO SCR12-EMP-MTOD(SUB1)       08040003
081100           END-IF                                                 08050003
081200        END-IF                                                    08060003
081300        IF TRCN-TR-REST-REQ = ('U' OR 'B')                        08070003
081400*          IF EMP-US-RSTD > WS-LOCAL-DATE-TIME                    08080003
081500           IF DE-COMPARE3-DATE-TIME > WS-LOCAL-DATE-TIME-CENT     08090003
081600              MOVE EMP-US-RSTD-TIME TO SCR12-EMP-USHR(SUB1)       08100003
081700           END-IF                                                 08110003
081800        END-IF                                                    08120003
081900     END-IF                                                       08130003
082000*    IF EMP-PERS-REST > WS-LOCAL-DATE-TIME                        08140003
082100     IF DE-COMPARE2-DATE-TIME > WS-LOCAL-DATE-TIME-CENT           08150003
082200        MOVE EMP-PERS-REST-TIME      TO SCR12-EMP-PERS-TM(SUB1)   08160003
082300        MOVE EMP-PERS-REST-DATE(5:2) TO SCR12-EMP-PERS-DY(SUB1)   08170003
082400     END-IF                                                       08180003
082500     MOVE AJJOBKEY-AREA             TO JS-KEY1                    08190003
082600     IF (P12CA-INQ-ONLY AND P12CA-CALL-DATE-FROM > SPACE)         08200003
082700        MOVE P12CA-CALL-DATE-FROM   TO JSK1-EXP-DATE              08210003
082800     ELSE                                                         08220003
082900        MOVE WS-LOCAL-DATE          TO JSK1-EXP-DATE              08230003
083000     END-IF                                                       08240003
083100     MOVE JS-KEY1                   TO JSKEY1                     08250003
083200     PERFORM P8010-READ-JSKEY1-GTEQ                               08260003
083300     IF SUCCESS                                                   08270003
083400        SET DE-YYMMDD-FORMAT       TO TRUE                        08280003
083500        MOVE JSK1-EXP-DATE         TO DE-YYMMDD                   08290003
083600        PERFORM P8998-DATEEDIT                                    08300003
083700        MOVE DE-CCYYMMDD           TO DE-COMPARE1-DATE            08310003
083800        IF JSK1-ASGN-DIST         = AJ-JOB-DIST                   08320003
083900           AND JSK1-ASGN-SUB-DIST = AJ-JOB-SUB-DIST               08330003
084000           AND JSK1-ASSIGNMENT    = AJ-JOB-ASSIGNMENT             08340003
084100           AND DE-COMPARE1-DATE  >= WS-LOCAL-DATE-CENT            08350003
084200           AND JSK1-ASGN-STATS                                    08360003
084300           CONTINUE                                               08370003
084400        ELSE                                                      08380003
084500           SET NO-RECORD-FND        TO TRUE                       08390003
084600        END-IF                                                    08400003
084700     END-IF                                                       08410003
084800     IF NOT SUCCESS                                               08420003
084900        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     08430003
085000           MOVE 'P1020-1'           TO ERR-PARAGRAPH              08440003
085100           MOVE JSKEY1              TO ERR-KEY                    08450003
085200           PERFORM P9999-GOT-PROBLEM                              08460003
085300        END-IF                                                    08470003
085400        PERFORM VARYING SUB2 FROM 1 BY 1                          08480003
085500           UNTIL SUB2 > 7                                         08490003
085600           SET JOB-ON-REST-DAY(SUB2)                              08500003
085700                                    TO TRUE                       08510003
085800        END-PERFORM                                               08520003
085900     END-IF                                                       08530003
086000     MOVE ZEROS                     TO SUB3                       08540003
086100     PERFORM VARYING SUB2 FROM 1 BY 1                             08550003
086200        UNTIL SUB2 > 7                                            08560003
086300        IF JOB-ON-REST-DAY(SUB2)                                  08570003
086400           ADD 1                    TO SUB3                       08580003
086500           MOVE DAYOFWK-2CHAR(SUB2, PSTCA-SUB)                    08590003
086600                                    TO SCR12-REST-DAY(SUB1, SUB3) 08600003
086700        END-IF                                                    08610003
086800     END-PERFORM.                                                 08620003
086900*                                                                 08630003
087000 P1030-GET-JOB-SCHED.                                             08640003
087100*                                                                 08650003
087200     MOVE SPACES                    TO WORK-JS-KEY1               08660003
087300                                       WORK-JS-KEY2               08670003
087400     MOVE AJ-JOB-DIST               TO WK-JSK1-ASGN-DIST          08680003
087500     MOVE AJ-JOB-SUB-DIST           TO WK-JSK1-ASGN-SUB-DIST      08690003
087600     MOVE AJ-JOB-ASSIGNMENT         TO WK-JSK1-ASSIGNMENT         08700003
087700     MOVE P12CA-CALL-DATE-FROM      TO WK-JSK1-EXP-DATE           08710003
087800     MOVE ZEROS                     TO WK-JSK1-ASGN-DAY           08720003
087900     MOVE WORK-JS-KEY1              TO JSKEY1                     08730003
088000     PERFORM P8010-READ-JSKEY1-GTEQ                               08740003
088100     IF SUCCESS                                                   08750003
088200        IF JSK1-ASGN-DIST = WK-JSK1-ASGN-DIST                     08760003
088300           AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST         08770003
088400           AND JSK1-ASSIGNMENT = WK-JSK1-ASSIGNMENT               08780003
088500           AND JSK1-ASGN-STATS                                    08790003
088600           CONTINUE                                               08800003
088700        ELSE                                                      08810003
088800           SET NO-RECORD-FND        TO TRUE                       08820003
088900        END-IF                                                    08830003
089000     END-IF                                                       08840003
089100     IF SUCCESS                                                   08850003
089200        MOVE JS-KEY1                TO WORK-JS-KEY1               08860003
089300        MOVE P12CA-CALL-DAY         TO WK-JSK1-ASGN-DAY           08870003
089400        MOVE P12CA-CALL-FROM        TO WK-JSK1-ASGN-START-TIME    08880003
089500        MOVE WORK-JS-KEY1           TO JSKEY1                     08890003
089600        PERFORM P8010-READ-JSKEY1-GTEQ                            08900003
089700        IF SUCCESS                                                08910003
089800           IF JSK1-ASGN-DIST = WK-JSK1-ASGN-DIST                  08920003
089900              AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST      08930003
090000              AND JSK1-ASSIGNMENT = WK-JSK1-ASSIGNMENT            08940003
090100              AND JSK1-EXP-DATE = WK-JSK1-EXP-DATE                08950003
090200              AND JSK1-ASGN-DAY = WK-JSK1-ASGN-DAY                08960003
090300              CONTINUE                                            08970003
090400           ELSE                                                   08980003
090500              SET NO-RECORD-FND     TO TRUE                       08990003
090600           END-IF                                                 09000003
090700        END-IF                                                    09010003
090800        IF NOT SUCCESS                                            09020003
090900           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  09030003
091000              MOVE 'P1030-1'        TO ERR-PARAGRAPH              09040003
091100              MOVE JSKEY1           TO ERR-KEY                    09050003
091200              PERFORM P9999-GOT-PROBLEM                           09060003
091300           END-IF                                                 09070003
091400           IF P12CA-CALL-TO < P12CA-CALL-FROM                     09080003
091500              ADD 1                 TO WK-JSK1-ASGN-DAY-NUM       09090003
091600              IF WK-JSK1-ASGN-DAY-NUM > 7                         09100003
091700                 MOVE 001           TO WK-JSK1-ASGN-DAY-NUM       09110003
091800              END-IF                                              09120003
091900              MOVE ZEROS            TO WK-JSK1-ASGN-START-TIME    09130003
092000              MOVE WORK-JS-KEY1     TO JSKEY1                     09140003
092100              PERFORM P8010-READ-JSKEY1-GTEQ                      09150003
092200              IF SUCCESS                                          09160003
092300                 IF JSK1-ASGN-DIST = WK-JSK1-ASGN-DIST            09170003
092400                    AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST09180003
092500                    AND JSK1-ASSIGNMENT = WK-JSK1-ASSIGNMENT      09190003
092600                    AND JSK1-EXP-DATE = WK-JSK1-EXP-DATE          09200003
092700                    AND JSK1-ASGN-DAY = WK-JSK1-ASGN-DAY          09210003
092800                    AND JSK1-ASGN-START-TIME NOT > P12CA-CALL-TO  09220003
092900                    CONTINUE                                      09230003
093000                 ELSE                                             09240003
093100                    SET NO-RECORD-FND                             09250003
093200                                    TO TRUE                       09260003
093300                 END-IF                                           09270003
093400              END-IF                                              09280003
093500           END-IF                                                 09290003
093600        END-IF                                                    09300003
093700        IF SUCCESS                                                09310003
093800           IF JSK2-ASGN-DIST = AJ-JOB-DIST                        09320003
093900              AND JSK2-ASGN-SUB-DIST = AJ-JOB-SUB-DIST            09330003
094000              AND JSK2-ASSIGNMENT = AJ-JOB-ASSIGNMENT             09340003
094100              AND JSK2-ASGN(1:6) = AJ-JOB-ASGN-ID                 09350003
094200              AND JSK2-ASGN-CC = AJ-JOB-ASGN-CC                   09360003
094300              CONTINUE                                            09370003
094400           ELSE                                                   09380003
094500              MOVE JS-KEY2          TO WORK-JS-KEY2               09390003
094600           END-IF                                                 09400003
094700        ELSE                                                      09410003
094800           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  09420003
094900              MOVE 'P1030-2'        TO ERR-PARAGRAPH              09430003
095000              MOVE JSKEY1           TO ERR-KEY                    09440003
095100              PERFORM P9999-GOT-PROBLEM                           09450003
095200           END-IF                                                 09460003
095300        END-IF                                                    09470003
095400     ELSE                                                         09480003
095500        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     09490003
095600           MOVE 'P1030-3'           TO ERR-PARAGRAPH              09500003
095700           MOVE JSKEY1              TO ERR-KEY                    09510003
095800           PERFORM P9999-GOT-PROBLEM                              09520003
095900        END-IF                                                    09530003
096000     END-IF.                                                      09540003
096100*                                                                 09550003
096200 P2000-UPDATE.                                                    09560003
096300*                                                                 09570003
096400*    EDIT FOR ERRORS                                              09580003
096500*                                                                 09590003
096600     IF SCR12-ASGN-ID NOT = P12CA-ASGN-ID                         09600003
096700        MOVE -1                      TO SCR12-ASGN-ID-CURSOR      09610003
096800        MOVE REV-VIDEO               TO SCR12-ASGN-ID-HI          09620003
096900*               MUST INQUIRE BEFORE UPDATE                        09630003
097000        MOVE 'M142'                  TO MSGLOG-CODE               09640003
097100        PERFORM P9000-SEND-MAP-AND-RETURN                         09650003
097200     END-IF                                                       09660003
097300*                                                                 09670003
097400*    EDITS FOR ANNUL FROM-TO DATES                                09680003
097500*                                                                 09690003
097600     MOVE SCR12-ASGN-ID TO JOB-DEF-CHECK                          09700003
097700     IF JOB-DEF-RELIEF-ASGN AND                                   09710003
097800        (SCR12-A-FROM-DATE > SPACES OR                            09720003
097900         SCR12-A-TO-DATE > SPACES)                                09730003
098000        MOVE -1                      TO SCR12-A-FROM-DATE-CURSOR  09740003
098100        MOVE REV-VIDEO               TO SCR12-A-FROM-DATE-HI      09750003
098200*        RELIEF JOB CANNOT BE ANNULLED. MUST ANNUL PRIMARY ASGN.  09760003
098300        MOVE 'A064'                  TO MSGLOG-CODE               09770003
098400        PERFORM P9000-SEND-MAP-AND-RETURN                         09780003
098500     END-IF                                                       09790003
098600     IF  SCR12-A-FROM-DATE NOT > SPACES                           09800003
098700     AND SCR12-A-TO-DATE       > SPACES                           09810003
098800        MOVE -1                      TO SCR12-A-FROM-DATE-CURSOR  09820003
098900        MOVE REV-VIDEO               TO SCR12-A-FROM-DATE-HI      09830003
099000*               FROM DATE REQUIRED WITH TO DATE                   09840003
099100        MOVE 'F033'                  TO MSGLOG-CODE               09850003
099200        PERFORM P9000-SEND-MAP-AND-RETURN                         09860003
099300     END-IF                                                       09870003
099400     IF  SCR12-A-FROM-DATE     > SPACES                           09880003
099500     AND SCR12-A-TO-DATE NOT   > SPACES                           09890003
099600        MOVE -1                      TO SCR12-A-TO-DATE-CURSOR    09900003
099700        MOVE REV-VIDEO               TO SCR12-A-TO-DATE-HI        09910003
099800*               TO DATE REQUIRED WITH FROM DATE                   09920003
099900        MOVE 'T093'                  TO MSGLOG-CODE               09930003
100000        PERFORM P9000-SEND-MAP-AND-RETURN                         09940003
100100     END-IF                                                       09950003
100200     IF SCR12-A-FROM-DATE > SPACES                                09960003
100300        SET DE-YYMMDD-FORMAT         TO TRUE                      09970003
100400        MOVE SCR12-A-FROM-DATE       TO DE-YYMMDD                 09980003
100500        PERFORM P8998-DATEEDIT                                    09990003
100600        IF DE-INVALID-DATE                                        10000003
100700           MOVE -1                   TO SCR12-A-FROM-DATE-CURSOR  10010003
100800           MOVE REV-VIDEO            TO SCR12-A-FROM-DATE-HI      10020003
100900*               INVALID DATE MSG                                  10030003
101000           MOVE 'I034'               TO MSGLOG-CODE               10040003
101100           PERFORM P9000-SEND-MAP-AND-RETURN                      10050003
101200        END-IF                                                    10060003
101300        MOVE DE-CCYYMMDD             TO DE-COMPARE1-DATE          10070003
101400        SET DE-YYMMDD-FORMAT         TO TRUE                      10080003
101500        MOVE SCR12-A-TO-DATE         TO DE-YYMMDD                 10090003
101600        PERFORM P8998-DATEEDIT                                    10100003
101700        IF DE-INVALID-DATE                                        10110003
101800           MOVE -1                   TO SCR12-A-TO-DATE-CURSOR    10120003
101900           MOVE REV-VIDEO            TO SCR12-A-TO-DATE-HI        10130003
102000*               INVALID DATE MSG                                  10140003
102100           MOVE 'I034'               TO MSGLOG-CODE               10150003
102200           PERFORM P9000-SEND-MAP-AND-RETURN                      10160003
102300        END-IF                                                    10170003
102400        MOVE DE-CCYYMMDD             TO DE-COMPARE2-DATE          10180003
102500*       IF SCR12-A-FROM-DATE > SCR12-A-TO-DATE                    10190003
102600        IF DE-COMPARE1-DATE  > DE-COMPARE2-DATE                   10200003
102700           MOVE -1                   TO SCR12-A-FROM-DATE-CURSOR  10210003
102800           MOVE REV-VIDEO            TO SCR12-A-FROM-DATE-HI      10220003
102900*               FROM DATE CANNOT BE GREATER THAN TO DATE          10230003
103000           MOVE 'F034'               TO MSGLOG-CODE               10240003
103100           PERFORM P9000-SEND-MAP-AND-RETURN                      10250003
103200        END-IF                                                    10260003
103300     END-IF                                                       10270003
103400*                                                                 10280003
103500     SET NO-CHANGE                   TO TRUE                      10290003
103600     PERFORM VARYING SUB1 FROM 1 BY 1                             10300003
103700        UNTIL SUB1 > ARRAY-MAX                                    10310003
103800        IF SCR12-EMP-NAME(SUB1) > SPACES                          10320003
103900           MOVE SPACES               TO AJJOBKEY-AREA             10330003
104000           MOVE SCR12-DIST           TO AJ-JOB-DIST               10340003
104100           MOVE SCR12-SUB-DIST       TO AJ-JOB-SUB-DIST           10350003
104200           MOVE SCR12-ASGN-ID        TO AJ-JOB-ASGN-ID            10360003
104300           MOVE SCR12-CRAFT(SUB1)    TO AJ-JOB-ASGN-CC            10370003
104400           MOVE AJJOBKEY-AREA        TO AJJOBKEY                  10380003
104500           EXEC CICS READ                                         10390003
104600                     DATASET(AJ-VIA-JNAME-JCRAFT)                 10400003
104700                     INTO(WS-ASGNED-JOBS)                         10410003
104800                     LENGTH(AJNAMECR-RLGTH)                       10420003
104900                     RIDFLD(AJJOBKEY)                             10430003
105000                     KEYLENGTH(AJNAMECR-KLGTH)                    10440003
105100                     RESP(WS-RESPONSE)                            10450003
105200           END-EXEC                                               10460003
105300           MOVE WS-RESPONSE          TO FILE-STATUS               10470003
105400           IF SUCCESS                                             10480003
105500                 IF  AJ-ANNUL-FROM-DATE = SCR12-A-FROM-DATE       10490003
105600                 AND AJ-ANNUL-TO-DATE   = SCR12-A-TO-DATE         10500003
105700                    CONTINUE                                      10510003
105800                 ELSE                                             10520003
105900                    SET CHANGE-MADE  TO TRUE                      10530003
106000                 END-IF                                           10540003
106100              END-IF                                              10550003
106200           END-IF                                                 10560003
106300     END-PERFORM                                                  10570003
106400*                                                                 10580003
106500     IF CHANGE-MADE                                               10590003
106600        PERFORM VARYING SUB1 FROM 1 BY 1                          10600003
106700           UNTIL SUB1 > ARRAY-MAX                                 10610003
106800           IF SCR12-EMP-NAME(SUB1) > SPACES                       10620003
106900              MOVE SPACES            TO AJJOBKEY-AREA             10630003
107000              MOVE SCR12-DIST        TO AJ-JOB-DIST               10640003
107100              MOVE SCR12-SUB-DIST    TO AJ-JOB-SUB-DIST           10650003
107200              MOVE SCR12-ASGN-ID     TO AJ-JOB-ASGN-ID            10660003
107300              MOVE SCR12-CRAFT(SUB1) TO AJ-JOB-ASGN-CC            10670003
107400              MOVE AJJOBKEY-AREA     TO AJJOBKEY                  10680003
107500              EXEC CICS READ                                      10690003
107600                        DATASET(AJ-VIA-JNAME-JCRAFT)              10700003
107700                        INTO(WS-ASGNED-JOBS)                      10710003
107800                        LENGTH(AJNAMECR-RLGTH)                    10720003
107900                        RIDFLD(AJJOBKEY)                          10730003
108000                        KEYLENGTH(AJNAMECR-KLGTH)                 10740003
108100                        RESP(WS-RESPONSE)                         10750003
108200              END-EXEC                                            10760003
108300              MOVE WS-RESPONSE       TO FILE-STATUS               10770003
108400              IF SUCCESS                                          10780003
108500                 PERFORM P2010-SETUP-P922-COMM                    10790003
108600              ELSE                                                10800003
108700                 IF NOT (NO-RECORD-FND OR END-OF-FILE)            10810003
108800                    MOVE 'P2000-1'   TO ERR-PARAGRAPH             10820003
108900                    MOVE AJJOBKEY    TO ERR-KEY                   10830003
109000                    PERFORM P9999-GOT-PROBLEM                     10840003
109100                 END-IF                                           10850003
109200              END-IF                                              10860003
109300           END-IF                                                 10870003
109400        END-PERFORM                                               10880003
109500*              UPDATE-MSG                                         10890003
109600        MOVE 'U001'                  TO MSGLOG-CODE               10900003
109700     END-IF                                                       10910003
109800     MOVE -1                         TO SCR12-ASGN-ID-CURSOR.     10920003
109900*                                                                 10930003
110000 P2010-SETUP-P922-COMM.                                           10940003
110100*                                                                 10950003
110200     MOVE SPACES                     TO P922-COMMAREA-PARMS       10960003
110300     SET P922-UPDATE-FUNCTION        TO TRUE                      10970003
110400     MOVE AJ-JOB-DIST                TO P922-AJ-DIST              10980003
110500     MOVE AJ-JOB-SUB-DIST            TO P922-AJ-SUB-DIST          10990003
110600     MOVE AJ-JOB-ASGN-ID             TO P922-AJ-ASGN              11000003
110700     MOVE AJ-JOB-ASGN-CC             TO P922-AJ-CC                11010003
110800     MOVE PSTCA-TIME-ZONE            TO P922-TIME-ZONE            11020003
110900     MOVE AJ-EFF-DATE                TO P922-ADD-EFF-DATE         11030003
111000*NJB MOVE AJ-ROSTER-CODE             TO P922-ADD-SEN-LIST         11040003
111100*NJB MOVE AJ-ROSTER-CC               TO P922-ADD-SEN-LIST-CC      11050003
111200     IF SCR12-A-FROM-DATE > SPACES                                11060003
111300        MOVE SCR12-A-FROM-DATE       TO P922-ADD-ANNUL-FROM-DATE  11070003
111400        MOVE SCR12-A-TO-DATE         TO P922-ADD-ANNUL-TO-DATE    11080003
111500     ELSE                                                         11090003
111600        MOVE '000000'                TO P922-ADD-ANNUL-FROM-DATE  11100003
111700                                        P922-ADD-ANNUL-TO-DATE    11110003
111800     END-IF                                                       11120003
111900     MOVE AJ-TEMPORARY-ASGN-CODE     TO P922-ADD-TEMP-ASGN-FLAG   11130003
112000     MOVE AJ-SPECIAL-COND-CODE       TO P922-ADD-SPEC-COND-FLAG   11140003
112100     MOVE AJ-OPEN-DATE               TO P922-ADD-LAST-OPEN-DATE   11150003
112200     MOVE AJ-OPEN-TIME               TO P922-ADD-LAST-OPEN-TIME   11160003
112300     MOVE AJ-OPEN-ASGN-MSG(1)        TO P922-ADD-OPEN-ASG-MSG1    11170003
112400     MOVE AJ-OPEN-ASGN-MSG(2)        TO P922-ADD-OPEN-ASG-MSG2    11180003
112500     MOVE AJ-CREW-PROF               TO P922-ADD-CREW-PROF        11190003
112600     EXEC CICS LINK                                               11200003
112700               PROGRAM(P922-PGM)                                  11210003
112800               COMMAREA(P922-COMMAREA-PARMS)                      11220003
112900               LENGTH(P922-LGTH)                                  11230003
113000               RESP(WS-RESPONSE)                                  11240003
113100     END-EXEC                                                     11250003
113200     MOVE WS-RESPONSE                TO FILE-STATUS               11260003
113300*C1190 BEGIN                                                      11270003
113400     IF SUCCESS                                                   11280003
113500        MOVE SCR12-A-FROM-DATE       TO P12TSQ-DELTA-FROM-DATE    11290003
113600        MOVE SCR12-A-TO-DATE         TO P12TSQ-DELTA-TO-DATE      11300003
113700        MOVE SCR12-ASGN-ID           TO P12TSQ-DELTA-ASGN-ID      11310003
113800        PERFORM P7400-DELETE-DELTA-QUEUE                          11320003
113900        PERFORM P7600-WRITE-DELTA-QUEUE                           11330003
114000     ELSE                                                         11340003
114100*C1190 END                                                        11350003
114200        MOVE 'P2010-1'               TO ERR-PARAGRAPH             11360003
114300        MOVE 'P922LINK'              TO ERR-KEY                   11370003
114400        PERFORM P9999-GOT-PROBLEM                                 11380003
114500     END-IF.                                                      11390003
114600*                                                                 11400003
114700 P7000-WRITE-TSQUEUE.                                             11410003
114800*                                                                 11420003
114900*                                                                 11430003
115000*      WRITE MAP TSQUEUE                                          11440003
115100*                                                                 11450003
115200     EXEC CICS ASSIGN                                             11460003
115300          EXTDS(WS-CICS-EXTDS-CODE)                               11470003
115400     END-EXEC                                                     11480003
115500*                                                                 11490003
115600     IF SCREEN-HAS-EXT-ATTR                                       11500003
115700        EXEC CICS SEND STRFIELD                                   11510003
115800                  FROM(WS-STRFIELD)                               11520003
115900                  LENGTH(WS-STRFIELD-LGTH)                        11530003
116000                  RESP(WS-RESPONSE)                               11540003
116100        END-EXEC                                                  11550003
116200        MOVE WS-RESPONSE           TO FILE-STATUS                 11560003
116300        IF NOT SUCCESS                                            11570003
116400           MOVE 'P7000-1'          TO ERR-PARAGRAPH               11580003
116500           MOVE 'SEND STRFIELD'    TO ERR-KEY                     11590003
116600           PERFORM P9999-GOT-PROBLEM                              11600003
116700        END-IF                                                    11610003
116800     END-IF                                                       11620003
116900*                                                                 11630003
117000     MOVE LENGTH OF WS-BUFFER-DATA TO WS-BUFFER-LGTH              11640003
117100     EXEC CICS RECEIVE BUFFER                                     11650003
117200               INTO(WS-BUFFER-DATA)                               11660003
117300               LENGTH(WS-BUFFER-LGTH)                             11670003
117400               RESP(WS-RESPONSE)                                  11680003
117500     END-EXEC                                                     11690003
117600     MOVE WS-RESPONSE              TO FILE-STATUS                 11700003
117700     IF NOT SUCCESS AND NOT EOC                                   11710003
117800        MOVE 'P7000-2'             TO ERR-PARAGRAPH               11720003
117900        MOVE 'RECEIVE BUFFER'      TO ERR-KEY                     11730003
118000        PERFORM P9999-GOT-PROBLEM                                 11740003
118100     END-IF                                                       11750003
118200     MOVE EIBCPOSN                 TO WS-BUFFER-CURSOR            11760003
118300                                                                  11770003
118400                                                                  11780003
118500     MOVE LENGTH OF WS-BUFFER-AREA TO P12TSQ-QLGTH                11790003
118600     MOVE EIBTRMID                 TO P12TSQ-MAP-TERM-ID          11800003
118700     EXEC CICS WRITEQ TS                                          11810003
118800               QUEUE(P12TSQ-MAP-QUEUE-ID)                         11820003
118900               FROM(WS-BUFFER-AREA)                               11830003
119000               LENGTH(P12TSQ-QLGTH)                               11840003
119100               RESP(WS-RESPONSE)                                  11850003
119200     END-EXEC                                                     11860003
119300     MOVE WS-RESPONSE              TO FILE-STATUS                 11870003
119400     IF NOT SUCCESS                                               11880003
119500        MOVE 'P7000-3'             TO ERR-PARAGRAPH               11890003
119600        PERFORM P9999-GOT-PROBLEM                                 11900003
119700     END-IF                                                       11910003
119800     MOVE EIBTRMID TO P12TSQ-CA-TERM-ID                           11920003
119900     EXEC CICS WRITEQ TS                                          11930003
120000               QUEUE(P12TSQ-CA-QUEUE-ID)                          11940003
120100               FROM(PSTCOMM-AREA)                                 11950003
120200               LENTGH(P12ACA-LGTH)                                11960003
120300               ITEM(P12TSQ-QUEUE-ITEM)                            11970003
120400               RESP(WS-RESPONSE)                                  11980003
120500     END-EXEC                                                     11990003
120600     MOVE WS-RESPONSE              TO FILE-STATUS                 12000003
120700     IF NOT SUCCESS                                               12010003
120800        MOVE 'P7000-4'             TO ERR-PARAGRAPH               12020003
120900        PERFORM P9999-GOT-PROBLEM                                 12030003
121000     END-IF.                                                      12040003
121100*                                                                 12050003
121200 P7010-READ-TSQUEUE.                                              12060003
121300*                                                                 12070003
121400*              READ THE MAPS TSQUEUE                              12080003
121500*                                                                 12090003
121600     MOVE LENGTH OF WS-BUFFER-AREA TO P12TSQ-QLGTH                12100003
121700     MOVE EIBTRMID                 TO P12TSQ-MAP-TERM-ID          12110003
121800     EXEC CICS READQ TS                                           12120003
121900               QUEUE(P12TSQ-MAP-QUEUE-ID)                         12130003
122000               INTO(WS-BUFFER-AREA)                               12140003
122100               LENGTH(P12TSQ-QLGTH)                               12150003
122200               ITEM(P12TSQ-QUEUE-ITEM)                            12160003
122300               RESP(WS-RESPONSE)                                  12170003
122400     END-EXEC                                                     12180003
122500     MOVE WS-RESPONSE              TO FILE-STATUS                 12190003
122600     IF SUCCESS                                                   12200003
122700        SET SEND-BUFFER            TO TRUE                        12210003
122800     ELSE                                                         12220003
122900        SET CREATE-SCREEN          TO TRUE                        12230003
123000        MOVE LOW-VALUES            TO PSTS12                      12240003
123100     END-IF                                                       12250003
123200     MOVE EIBTRMID TO P12TSQ-CA-TERM-ID                           12260003
123300     EXEC CICS READQ TS                                           12270003
123400               QUEUE(P12TSQ-CA-QUEUE-ID)                          12280003
123500               INTO(PSTCOMM-AREA)                                 12290003
123600               LENGTH(P12ACA-LGTH)                                12300003
123700               ITEM(P12TSQ-QUEUE-ITEM)                            12310003
123800               RESP(WS-RESPONSE)                                  12320003
123900     END-EXEC                                                     12330003
124000     MOVE WS-RESPONSE TO FILE-STATUS                              12340003
124100     IF NOT SUCCESS                                               12350003
124200        MOVE SPACES TO PSTCOMM-AREA                               12360003
124300     END-IF                                                       12370003
124400     PERFORM P7020-DELETE-TSQUEUE.                                12380003
124500*                                                                 12390003
124600 P7020-DELETE-TSQUEUE.                                            12400003
124700*                                                                 12410003
124800     MOVE EIBTRMID TO P12TSQ-MAP-TERM-ID                          12420003
124900     EXEC CICS DELETEQ TS                                         12430003
125000               QUEUE(P12TSQ-MAP-QUEUE-ID)                         12440003
125100               RESP(WS-RESPONSE)                                  12450003
125200     END-EXEC                                                     12460003
125300     MOVE EIBTRMID TO P12TSQ-CA-TERM-ID                           12470003
125400     EXEC CICS DELETEQ TS                                         12480003
125500               QUEUE(P12TSQ-CA-QUEUE-ID)                          12490003
125600               RESP(WS-RESPONSE)                                  12500003
125700     END-EXEC.                                                    12510003
125800*                                                                 12520003
125900 P7400-DELETE-DELTA-QUEUE.                                        12530003
126000*                                                                 12540003
126100     MOVE EIBTRMID            TO P12TSQ-DELTA-TERM-ID             12550003
126200     EXEC CICS DELETEQ TS                                         12560003
126300               QUEUE(P12TSQ-DELTA-ID)                             12570003
126400               RESP(WS-RESPONSE)                                  12580003
126500     END-EXEC.                                                    12590003
126600*                                                                 12600003
126700 P7500-READ-DELTA-QUEUE.                                          12610003
126800*                                                                 12620003
126900     MOVE EIBTRMID            TO P12TSQ-DELTA-TERM-ID             12630003
127000     EXEC CICS READQ TS                                           12640003
127100               QUEUE(P12TSQ-DELTA-ID)                             12650003
127200               INTO(P12-DELTA-FIELDS)                             12660003
127300               LENGTH(P12-DELTA-QLGTH)                            12670003
127400               RESP(WS-RESPONSE)                                  12680003
127500     END-EXEC                                                     12690003
127600     MOVE WS-RESPONSE TO FILE-STATUS                              12700003
127700     IF SUCCESS                                                   12710003
127800        IF PFKEY10                                                12720003
127900          IF (P12TSQ-DELTA-FROM-DATE = SCR12-A-FROM-DATE          12730003
128000          AND P12TSQ-DELTA-TO-DATE   = SCR12-A-TO-DATE            12740003
128100          AND P12TSQ-DELTA-ASGN-ID   = SCR12-ASGN-ID)             12750003
128200          OR                                                      12760003
128300            ((P12TSQ-DELTA-FROM-DATE = ZEROES                     12770003
128400          AND SCR12-A-FROM-DATE  NOT > SPACES)                    12780003
128500          AND (P12TSQ-DELTA-TO-DATE  = ZEROES                     12790003
128600          AND SCR12-A-TO-DATE    NOT > SPACES)                    12800003
128700          AND P12TSQ-DELTA-ASGN-ID   = SCR12-ASGN-ID)             12810003
128800              CONTINUE                                            12820003
128900          ELSE                                                    12830003
129000              PERFORM P7700-PROCESS-ERROR                         12840003
129100          END-IF                                                  12850003
129200        END-IF                                                    12860003
129300     END-IF.                                                      12870003
129400*                                                                 12880003
129500 P7600-WRITE-DELTA-QUEUE.                                         12890003
129600*                                                                 12900003
129700     EXEC CICS WRITEQ TS                                          12910003
129800               QUEUE(P12TSQ-DELTA-ID)                             12920003
129900               FROM(P12-DELTA-FIELDS)                             12930003
130000               LENGTH(P12-DELTA-QLGTH)                            12940003
130100               RESP(WS-RESPONSE)                                  12950003
130200     END-EXEC                                                     12960003
130300     MOVE WS-RESPONSE              TO FILE-STATUS                 12970003
130400     IF NOT SUCCESS                                               12980003
130500        MOVE 'P7600-1'             TO ERR-PARAGRAPH               12990003
130600        PERFORM P9999-GOT-PROBLEM                                 13000003
130700     END-IF.                                                      13010003
130800*                                                                 13020003
130900 P7700-PROCESS-ERROR.                                             13030003
131000*                                                                 13040003
131100*            UPDATE DATA MESSAGE                                  13050003
131200     MOVE 'D120'                  TO MSGLOG-CODE                  13060003
131300*                                                                 13070003
131400     IF P12TSQ-DELTA-TO-DATE      = SCR12-A-TO-DATE               13080003
131500        CONTINUE                                                  13090003
131600     ELSE                                                         13100003
131700        MOVE -1                   TO SCR12-A-TO-DATE-CURSOR       13110003
131800        MOVE REV-VIDEO            TO SCR12-A-TO-DATE-HI           13120003
131900     END-IF                                                       13130003
132000     IF P12TSQ-DELTA-FROM-DATE    = SCR12-A-FROM-DATE             13140003
132100        CONTINUE                                                  13150003
132200     ELSE                                                         13160003
132300        MOVE -1                   TO SCR12-A-FROM-DATE-CURSOR     13170003
132400        MOVE REV-VIDEO            TO SCR12-A-FROM-DATE-HI         13180003
132500     END-IF                                                       13190003
132600     IF P12TSQ-DELTA-ASGN-ID      = SCR12-ASGN-ID                 13200003
132700        CONTINUE                                                  13210003
132800     ELSE                                                         13220003
132900        MOVE -1                   TO SCR12-ASGN-ID-CURSOR         13230003
133000        MOVE REV-VIDEO            TO SCR12-ASGN-ID-HI             13240003
133100     END-IF                                                       13250003
133200     PERFORM P9000-SEND-MAP-AND-RETURN.                           13260003
133300*                                                                 13270003
133400 P8000-READ-CNTLFILE.                                             13280003
133500*                                                                 13290003
133600     EXEC CICS READ                                               13300003
133700               DATASET(CNTL-FILE-VIA-CNTLKEY)                     13310003
133800               INTO(WS-CNTL-FILE)                                 13320003
133900               LENGTH(CNTLFILE-RLGTH)                             13330003
134000               RIDFLD(CNTLKEY)                                    13340003
134100               KEYLENGTH(CNTLFILE-KLGTH)                          13350003
134200               RESP(WS-RESPONSE)                                  13360003
134300     END-EXEC                                                     13370003
134400     MOVE WS-RESPONSE               TO FILE-STATUS.               13380003
134500*                                                                 13390003
134600 P8010-READ-JSKEY1-GTEQ.                                          13400003
134700*                                                                 13410003
134800     EXEC CICS READ                                               13420003
134900               DATASET(JS-VIA-JSKEY1)                             13430003
135000               INTO(WS-JOB-SCHEDULE)                              13440003
135100               LENGTH(JSKEY1-RLGTH)                               13450003
135200               RIDFLD(JSKEY1)                                     13460003
135300               KEYLENGTH(JSKEY1-KLGTH)                            13470003
135400               GTEQ                                               13480003
135500               RESP(WS-RESPONSE)                                  13490003
135600     END-EXEC                                                     13500003
135700     MOVE WS-RESPONSE               TO FILE-STATUS.               13510003
135800*                                                                 13520003
135900 P8020-GET-ASGN-EMPS.                                             13530003
136000*                                                                 13540003
136100     MOVE 'A'                       TO WK-ASGN-JOB-TYPE           13550003
136200     IF (P12CA-INQ-ONLY AND P12CA-CALL-DAY-PICX IS NUMERIC        13560003
136300        AND WORK-JS-KEY2 > SPACES)                                13570003
136400        MOVE WK-JSK2-ASGN-DIST      TO WK-ASGN-DIST               13580003
136500        MOVE WK-JSK2-ASGN-SUB-DIST  TO WK-ASGN-SUB-DIST           13590003
136600        MOVE WK-JSK2-ASSIGNMENT     TO WK-ASGN-ASSIGN             13600003
136700        MOVE WK-JSK2-ASGN-CC        TO WK-ASGN-CC                 13610003
136800     ELSE                                                         13620003
136900        MOVE AJ-JOB-DIST            TO WK-ASGN-DIST               13630003
137000        MOVE AJ-JOB-SUB-DIST        TO WK-ASGN-SUB-DIST           13640003
137100        MOVE AJ-JOB-ASGN-ID         TO WK-ASGN-ASSIGN             13650003
137200        MOVE AJ-JOB-ASGN-CC         TO WK-ASGN-CC                 13660003
137300     END-IF                                                       13670003
137400     MOVE ZEROS                     TO ASGN-OWNER                 13680003
137500                                       ASGN-TEMPORARY-EMP         13690003
137600                                       ASGN-ON-DUTY-EMP           13700003
137700     IF DAILY-MARK-YARD                                           13710003
137800        MOVE WK-ASGN-DIST           TO SWASSGN-K-DISTRICT         13720003
137900        MOVE WK-ASGN-SUB-DIST       TO SWASSGN-K-SUB-DIST         13730003
138000        MOVE WK-ASGN-ASSIGN         TO SWASSGN-K-ASSIGN           13740003
138100        MOVE WK-ASGN-CC             TO SWASSGN-K-ASSIGN(7:2)      13750003
138200        MOVE DAY-OF-WK              TO SWASSGN-K-DAY              13760003
138300        MOVE SWASSGN-KEY            TO SWJOBKEY                   13770003
138400        EXEC CICS READ                                            13780003
138500                  DATASET(SWASSGN-VIA-ASSIGNMENT)                 13790003
138600                  INTO(WS-SWASSGN-FILE)                           13800003
138700                  LENGTH(SWASSIGN-RLGTH)                          13810003
138800                  RIDFLD(SWJOBKEY)                                13820003
138900                  KEYLENGTH(SWASSIGN-KLGTH)                       13830003
139000                  RESP(WS-RESPONSE)                               13840003
139100        END-EXEC                                                  13850003
139200        MOVE WS-RESPONSE            TO FILE-STATUS                13860003
139300        IF NOT SUCCESS                                            13870003
139400           IF NOT NO-RECORD-FND                                   13880003
139500              MOVE 'P8020-1'        TO ERR-PARAGRAPH              13890003
139600              MOVE SWJOBKEY         TO ERR-KEY                    13900003
139700              PERFORM P9999-GOT-PROBLEM                           13910003
139800           END-IF                                                 13920003
139900        ELSE                                                      13930003
140000           IF WS-LOCAL-DATE = SWASSGN-DATE                        13940003
140100              AND CALLER-CONFIRMED                                13950003
140200              MOVE SWASSGN-EMP-NO   TO ASGN-OWNER                 13960003
140300           END-IF                                                 13970003
140400        END-IF                                                    13980003
140500     END-IF                                                       13990003
140600     IF NOT HAVE-ASGN-OWNER                                       14000003
140700        PERFORM PXXXX-JOB-OWNER                                   14010003
140800        IF ASGN-EMP-NO > ZERO                                     14020003
140900           MOVE ASGN-EMP-NO         TO ASGN-OWNER                 14030003
141000        END-IF                                                    14040003
141100        PERFORM PXXXX-LATEST-TEMP                                 14050003
141200        IF ASGN-EMP-NO > ZERO                                     14060003
141300           MOVE ASGN-EMP-NO         TO ASGN-TEMPORARY-EMP         14070003
141400        END-IF                                                    14080003
141500     END-IF                                                       14090003
141600     PERFORM PXXXX-ON-DUTY-EMP                                    14100003
141700     IF ASGN-EMP-NO > ZERO                                        14110003
141800        MOVE ASGN-EMP-NO            TO ASGN-ON-DUTY-EMP           14120003
141900     END-IF.                                                      14130003
142000*                                                                 14140003
142100 P8060-GET-EMP-TIME-ZONE.                                         14150003
142200*                                                                 14160003
142300     MOVE SPACES                    TO CNTLKEY-AREA               14170003
142400     MOVE '02'                      TO CNTL-REC-TYPE              14180003
142500     MOVE DIST IN WS-MSTR           TO CNTL-DIST                  14190003
142600     MOVE SUB-DIST IN WS-MSTR       TO CNTL-SUB-DIST              14200003
142700     MOVE CNTLKEY-AREA              TO CNTLKEY                    14210003
142800     PERFORM P8000-READ-CNTLFILE                                  14220003
142900     IF NOT SUCCESS                                               14230003
143000        MOVE 'P8060-1'              TO ERR-PARAGRAPH              14240003
143100        MOVE CNTLKEY                TO ERR-KEY                    14250003
143200        PERFORM P9999-GOT-PROBLEM                                 14260003
143300     END-IF.                                                      14270003
143400*                                                                 14280003
143500 P8500-READ-MASTER.                                               14290003
143600*                                                                 14300003
143700     EXEC CICS READ                                               14310003
143800               DATASET(MSTR-VIA-EMP-NBR)                          14320003
143900               INTO(WS-MSTR)                                      14330003
144000               LENGTH(MSTRENBR-RLGTH)                             14340003
144100               RIDFLD(MSTRNBRK)                                   14350003
144200               KEYLENGTH(MSTRENBR-KLGTH)                          14360003
144300               RESP(WS-RESPONSE)                                  14370003
144400     END-EXEC                                                     14380003
144500     MOVE WS-RESPONSE               TO FILE-STATUS                14390003
144600     IF SUCCESS                                                   14400003
144700        PERFORM P8510-READ-MASTER-JOBS                            14410003
144800     ELSE                                                         14420003
144900        MOVE 'P8500-1'              TO ERR-PARAGRAPH              14430003
145000        MOVE MSTRNBRK               TO ERR-KEY                    14440003
145100        PERFORM P9999-GOT-PROBLEM                                 14450003
145200     END-IF.                                                      14460003
145300*                                                                 14470003
145400 P8510-READ-MASTER-JOBS.                                          14480003
145500*                                                                 14490003
145600     MOVE SPACES                    TO WS-ASGN-FILE               14500003
145700     MOVE EMP-NBR OF WS-MSTR        TO WK-ASGN-EMP-NO             14510003
145800     PERFORM PXXXX-JOB-OWNED                                      14520003
145900     MOVE ASGN-JOB-TYPE             TO NORMAL-ASGNMT-FLAG         14530003
146000     MOVE ASGN-ASSIGNMENT           TO NORMAL-ASGNMT              14540003
146100     MOVE SPACES                    TO WS-ASGN-FILE               14550003
146200     PERFORM PXXXX-LATEST-TEMP-JOB                                14560003
146300     MOVE ASGN-JOB-TYPE             TO TEMPORARY-ASGNMT-FLAG      14570003
146400     MOVE SPACES                    TO TEMP-ASGN-XB-AUG-FLAG      14580003
146500     IF ASGN-JOB-TYPE = 'X'                                       14590003
146600        AND AUGMENTED-TO-EXTRA-BOARD                              14600003
146700        SET TEMP-ASGN-XB-AUG        TO TRUE                       14610003
146800     END-IF                                                       14620003
146900     MOVE ASGN-ASSIGNMENT           TO TEMPORARY-ASGNMT           14630003
147000     MOVE SPACE                     TO WS-ASGN-FILE               14640003
147100     PERFORM PXXXX-JOB-ON-DUTY                                    14650003
147200     MOVE ASGN-JOB-TYPE             TO ON-DUTY-ASGNMT-FLAG        14660003
147300     MOVE ASGN-ASSIGNMENT           TO ON-DUTY-ASGNMT             14670003
147400     MOVE ASGN-ON-DUTY-DATE-TIME    TO ON-DUTY-OUT-TOWN-CODE.     14680003
147500*                                                                 14690003
147600 PXXXX-JOB-OWNER.                                                 14700003
147700*                                                                 14710003
147800     MOVE WORK-ASGNKEY1             TO ASGNKEY1                   14720003
147900     SET ASGN-OWNER-REC             TO TRUE                       14730003
148000     MOVE ZERO                      TO ASGN-DATE-TIME             14740003
148100     MOVE ASGNKEY1                  TO ASGNJOB                    14750003
148200     EXEC CICS READ                                               14760003
148300               DATASET(ASGN-VIA-ASGNJOB)                          14770003
148400               INTO(ASGN-AREA)                                    14780003
148500               LENGTH(ASGNJOB-RLGTH)                              14790003
148600               RIDFLD(ASGNJOB)                                    14800003
148700               KEYLENGTH(ASGNJOB-KLGTH)                           14810003
148800               RESP(WS-RESPONSE)                                  14820003
148900     END-EXEC                                                     14830003
149000     MOVE WS-RESPONSE               TO FILE-STATUS                14840003
149100     IF NOT SUCCESS                                               14850003
149200        MOVE ZEROS                  TO ASGN-EMP-NO                14860003
149300     END-IF.                                                      14870003
149400*                                                                 14880003
149500 PXXXX-LATEST-TEMP.                                               14890003
149600*                                                                 14900003
149700     MOVE SPACES                    TO WS-SAVE-ASGN-FILE          14910003
149800     MOVE WORK-ASGNKEY1             TO ASGNKEY1                   14920003
149900     SET ASGN-TEMP-REC              TO TRUE                       14930003
150000     MOVE ZERO                      TO ASGN-DATE-TIME             14940003
150100     MOVE ASGNKEY1                  TO ASGNJOB                    14950003
150200     EXEC CICS STARTBR                                            14960003
150300               DATASET(ASGN-VIA-ASGNJOB)                          14970003
150400               RIDFLD(ASGNJOB)                                    14980003
150500               GTEQ                                               14990003
150600               RESP(WS-RESPONSE)                                  15000003
150700     END-EXEC                                                     15010003
150800     MOVE WS-RESPONSE               TO FILE-STATUS                15020003
150900     IF SUCCESS                                                   15030003
151000        MOVE 'N' TO WS-ASGN-DONE-CODE                             15040003
151100        PERFORM UNTIL ASGN-DONE                                   15050003
151200           EXEC CICS READNEXT                                     15060003
151300                     DATASET(ASGN-VIA-ASGNJOB)                    15070003
151400                     INTO(ASGN-AREA)                              15080003
151500                     LENGTH(ASGNJOB-RLGTH)                        15090003
151600                     RIDFLD(ASGNJOB)                              15100003
151700                     KEYLENGTH(ASGNJOB-KLGTH)                     15110003
151800                     RESP(WS-RESPONSE)                            15120003
151900           END-EXEC                                               15130003
152000           MOVE WS-RESPONSE         TO FILE-STATUS                15140003
152100           IF SUCCESS                                             15150003
152200              IF WK-ASGN-DIST = ASGN-DIST                         15160003
152300                 AND WK-ASGN-SUB-DIST = ASGN-SUB-DIST             15170003
152400                 AND WK-ASGN-ASSIGN = ASGN-AJ-JOB-NO              15180003
152500                 AND WK-ASGN-CC = ASGN-AJ-JOB-CC                  15190003
152600                 AND ASGN-TEMP-REC                                15200003
152700                 MOVE ASGN-AREA     TO WS-SAVE-ASGN-FILE          15210003
152800              ELSE                                                15220003
152900                 SET ASGN-DONE      TO TRUE                       15230003
153000              END-IF                                              15240003
153100           ELSE                                                   15250003
153200              SET ASGN-DONE         TO TRUE                       15260003
153300           END-IF                                                 15270003
153400        END-PERFORM                                               15280003
153500        EXEC CICS ENDBR                                           15290003
153600                  DATASET(ASGN-VIA-ASGNJOB)                       15300003
153700                  RESP(WS-RESPONSE)                               15310003
153800        END-EXEC                                                  15320003
153900     END-IF                                                       15330003
154000     IF WS-SAVE-ASGN-FILE > SPACE                                 15340003
154100        MOVE WS-SAVE-ASGN-FILE      TO ASGN-AREA                  15350003
154200     ELSE                                                         15360003
154300        MOVE ZEROS                  TO ASGN-EMP-NO                15370003
154400     END-IF.                                                      15380003
154500*                                                                 15390003
154600 PXXXX-ON-DUTY-EMP.                                               15400003
154700*                                                                 15410003
154800     MOVE WORK-ASGNKEY1             TO ASGNKEY1                   15420003
154900     SET ASGN-ON-DUTY-REC           TO TRUE                       15430003
155000     MOVE ZERO                      TO ASGN-DATE-TIME             15440003
155100     MOVE ASGNKEY1                  TO ASGNJOB                    15450003
155200     EXEC CICS READ                                               15460003
155300               DATASET(ASGN-VIA-ASGNJOB)                          15470003
155400               INTO(ASGN-AREA)                                    15480003
155500               LENGTH(ASGNJOB-RLGTH)                              15490003
155600               RIDFLD(ASGNJOB)                                    15500003
155700               KEYLENGTH(ASGNJOB-KLGTH)                           15510003
155800               RESP(WS-RESPONSE)                                  15520003
155900     END-EXEC                                                     15530003
156000     MOVE WS-RESPONSE               TO FILE-STATUS                15540003
156100     IF NOT SUCCESS                                               15550003
156200        MOVE ZEROS                  TO ASGN-EMP-NO                15560003
156300     END-IF.                                                      15570003
156400*                                                                 15580003
156500 PXXXX-JOB-OWNED.                                                 15590003
156600*                                                                 15600003
156700     MOVE WORK-ASGNKEY2             TO ASGNKEY2                   15610003
156800     MOVE '1'                       TO ASGN-EMP-NO-REC-TYPE       15620003
156900     MOVE ZERO                      TO ASGN-EMP-DATE-TIME         15630003
157000     MOVE ASGNKEY2                  TO ASGNEMP                    15640003
157100     EXEC CICS READ                                               15650003
157200               DATASET(ASGN-VIA-ASGNEMP)                          15660003
157300               INTO(WS-ASGN-FILE)                                 15670003
157400               LENGTH(ASGNEMP-RLGTH)                              15680003
157500               RIDFLD(ASGNEMP)                                    15690003
157600               KEYLENGTH(ASGNEMP-KLGTH)                           15700003
157700               RESP(WS-RESPONSE)                                  15710003
157800     END-EXEC                                                     15720003
157900     MOVE WS-RESPONSE               TO FILE-STATUS                15730003
158000     IF NOT SUCCESS                                               15740003
158100        MOVE SPACES                 TO WS-ASGN-FILE               15750003
158200     END-IF.                                                      15760003
158300*                                                                 15770003
158400 PXXXX-LATEST-TEMP-JOB.                                           15780003
158500*                                                                 15790003
158600     MOVE WORK-ASGNKEY2             TO ASGNKEY2                   15800003
158700     MOVE '2'                       TO ASGN-EMP-NO-REC-TYPE       15810003
158800     MOVE ZERO                      TO ASGN-EMP-DATE-TIME         15820003
158900     MOVE ASGNKEY2                  TO ASGNEMP                    15830003
159000     MOVE SPACES                    TO WS-ASGN-FILE               15840003
159100                                       WS-SAVE-ASGN-FILE          15850003
159200     EXEC CICS STARTBR                                            15860003
159300               DATASET(ASGN-VIA-ASGNEMP)                          15870003
159400               RIDFLD(ASGNEMP)                                    15880003
159500               GTEQ                                               15890003
159600               RESP(WS-RESPONSE)                                  15900003
159700     END-EXEC                                                     15910003
159800     MOVE WS-RESPONSE               TO FILE-STATUS                15920003
159900     IF SUCCESS                                                   15930003
160000        MOVE 'N'                    TO WS-ASGN-DONE-CODE          15940003
160100        PERFORM UNTIL ASGN-DONE                                   15950003
160200           EXEC CICS READNEXT                                     15960003
160300                     DATASET(ASGN-VIA-ASGNEMP)                    15970003
160400                     INTO(ASGN-AREA)                              15980003
160500                     LENGTH(ASGNEMP-RLGTH)                        15990003
160600                     RIDFLD(ASGNEMP)                              16000003
160700                     KEYLENGTH(ASGNEMP-KLGTH)                     16010003
160800                     RESP(WS-RESPONSE)                            16020003
160900           END-EXEC                                               16030003
161000           MOVE WS-RESPONSE         TO FILE-STATUS                16040003
161100           IF SUCCESS                                             16050003
161200              IF ASGN-EMP-NO = WK-ASGN-EMP-NO                     16060003
161300                 AND ASGN-EMP-NO-REC-TYPE = '2'                   16070003
161400                 MOVE ASGN-AREA     TO WS-SAVE-ASGN-FILE          16080003
161500              ELSE                                                16090003
161600                 SET ASGN-DONE      TO TRUE                       16100003
161700              END-IF                                              16110003
161800           ELSE                                                   16120003
161900              SET ASGN-DONE         TO TRUE                       16130003
162000           END-IF                                                 16140003
162100        END-PERFORM                                               16150003
162200        EXEC CICS ENDBR                                           16160003
162300                  DATASET(ASGN-VIA-ASGNEMP)                       16170003
162400                  RESP(WS-RESPONSE)                               16180003
162500        END-EXEC                                                  16190003
162600     END-IF                                                       16200003
162700     IF WS-SAVE-ASGN-FILE > SPACES                                16210003
162800        MOVE WS-SAVE-ASGN-FILE      TO WS-ASGN-FILE               16220003
162900     ELSE                                                         16230003
163000        MOVE SPACES                 TO WS-ASGN-FILE               16240003
163100     END-IF.                                                      16250003
163200*                                                                 16260003
163300 PXXXX-JOB-ON-DUTY.                                               16270003
163400*                                                                 16280003
163500     MOVE WORK-ASGNKEY2             TO ASGNKEY2                   16290003
163600     MOVE '3'                       TO ASGN-EMP-NO-REC-TYPE       16300003
163700     MOVE ZERO                      TO ASGN-EMP-DATE-TIME         16310003
163800     MOVE ASGNKEY2                  TO ASGNEMP                    16320003
163900     EXEC CICS READ                                               16330003
164000               DATASET(ASGN-VIA-ASGNEMP)                          16340003
164100               INTO(ASGN-AREA)                                    16350003
164200               LENGTH(ASGNEMP-RLGTH)                              16360003
164300               RIDFLD(ASGNEMP)                                    16370003
164400               KEYLENGTH(ASGNEMP-KLGTH)                           16380003
164500               RESP(WS-RESPONSE)                                  16390003
164600     END-EXEC                                                     16400003
164700     MOVE WS-RESPONSE               TO FILE-STATUS                16410003
164800     IF NOT SUCCESS                                               16420003
164900        MOVE SPACES                 TO WS-ASGN-FILE               16430003
165000     END-IF.                                                      16440003
165100*                                                                 16450003
165200 COPY TIMEZONE.                                                   16460003
165300*                                                                 16470003
165400 COPY DATEEDIT.                                                   16480003
165500*                                                                 16490003
165600 P9000-SEND-MAP-AND-RETURN.                                       16500003
165700*                                                                 16510003
165800*C1190 BEGIN                                                      16520003
165900     PERFORM P7400-DELETE-DELTA-QUEUE                             16530003
166000     PERFORM P7600-WRITE-DELTA-QUEUE                              16540003
166100*C1190 END                                                        16550003
166200     IF MSGLOG-CODE > SPACES                                      16560003
166300        PERFORM P9030-GET-MESSAGE                                 16570003
166400        MOVE MSGLOG-MESSAGE-AREA     TO SCR12-ERRORMSG            16580003
166500     END-IF                                                       16590003
166600                                                                  16600003
166700     MOVE P12-MAP-VERSION(PSTCA-SUB) TO P12-MAP                   16610003
166800     IF CREATE-SCREEN                                             16620003
166900        PERFORM P9010-SEND-PHYSICAL-MAP                           16630003
167000     ELSE                                                         16640003
167100        IF CONTINUE-SCREEN                                        16650003
167200           PERFORM P9020-SEND-DATAONLY-MAP                        16660003
167300        ELSE                                                      16670003
167400           PERFORM P9035-SEND-BUFFER                              16680003
167500        END-IF                                                    16690003
167600     END-IF                                                       16700003
167700     EXEC CICS RETURN                                             16710003
167800               TRANSID(P12-TRAN)                                  16720003
167900               COMMAREA(PSTCOMM-AREA)                             16730003
168000               LENGTH(PSTCOMM-LGTH)                               16740003
168100     END-EXEC.                                                    16750003
168200*                                                                 16760003
168300 P9010-SEND-PHYSICAL-MAP.                                         16770003
168400*                                                                 16780003
168500     MOVE -1                        TO SCR12-ASGN-ID-CURSOR       16790003
168600     EXEC CICS SEND                                               16800003
168700               MAP(P12-MAP)                                       16810003
168800               MAPSET(P12-SET)                                    16820003
168900               FROM(PSTS12)                                       16830003
169000               CURSOR                                             16840003
169100               ERASE                                              16850003
169200               RESP(WS-RESPONSE)                                  16860003
169300     END-EXEC                                                     16870003
169400     MOVE WS-RESPONSE               TO FILE-STATUS                16880003
169500     IF NOT SUCCESS                                               16890003
169600        MOVE 'P9010'                TO ERR-PARAGRAPH              16900003
169700        PERFORM P9999-GOT-PROBLEM                                 16910003
169800     END-IF.                                                      16920003
169900*                                                                 16930003
170000 P9020-SEND-DATAONLY-MAP.                                         16940003
170100*                                                                 16950003
170200     EXEC CICS SEND                                               16960003
170300               MAP(P12-MAP)                                       16970003
170400               MAPSET(P12-SET)                                    16980003
170500               FROM(PSTS12)                                       16990003
170600               DATAONLY                                           17000003
170700               CURSOR                                             17010003
170800               RESP(WS-RESPONSE)                                  17020003
170900     END-EXEC                                                     17030003
171000     MOVE WS-RESPONSE               TO FILE-STATUS                17040003
171100     IF NOT SUCCESS                                               17050003
171200        MOVE 'P9020'                TO ERR-PARAGRAPH              17060003
171300        PERFORM P9999-GOT-PROBLEM                                 17070003
171400     END-IF.                                                      17080003
171500*                                                                 17090003
171600 P9030-GET-MESSAGE.                                               17100003
171700*                                                                 17110003
171800     MOVE PSTCA-SUB                 TO MSGLOG-SUB-CODE            17120003
171900     EXEC CICS READ                                               17130003
172000               DATASET(MSGLOG-VIA-CODE)                           17140003
172100               INTO(MSGLOG-AREA)                                  17150003
172200               LENGTH(MSGLOG-RLGTH)                               17160003
172300               RIDFLD(MSGLOG-KEY)                                 17170003
172400               KEYLENGTH(MSGLOG-KLGTH)                            17180003
172500               RESP(WS-RESPONSE)                                  17190003
172600     END-EXEC                                                     17200003
172700     MOVE WS-RESPONSE TO FILE-STATUS                              17210003
172800     IF NOT SUCCESS                                               17220003
172900        IF PSTCA-SUB = 1                                          17230003
173000           MOVE 'NO MESSAGE ON FILE' TO MSGLOG-MESSAGE            17240003
173100        ELSE                                                      17250003
173200           MOVE 'AUCUN MESSAGE'      TO MSGLOG-MESSAGE            17260003
173300        END-IF                                                    17270003
173400     END-IF                                                       17280003
173500     MOVE MSGLOG-CODE                TO MSGLOG-MSG-CODE           17290003
173600     MOVE '-'                        TO MSGLOG-MSG-SEP            17300003
173700     MOVE MSGLOG-SUB-CODE            TO MSGLOG-MSG-SUB-CODE.      17310003
173800*                                                                 17320003
173900 P9035-SEND-BUFFER.                                               17330003
174000*                                                                 17340003
174100     EXEC CICS SEND                                               17350003
174200               FROM(WS-BUFFER-DATA)                               17360003
174300               LENGTH(WS-BUFFER-LGTH)                             17370003
174400               ERASE                                              17380003
174500               RESP(WS-RESPONSE)                                  17390003
174600     END-EXEC                                                     17400003
174700     MOVE WS-RESPONSE       TO FILE-STATUS                        17410003
174800     IF NOT SUCCESS                                               17420003
174900        MOVE 'P9035-1'      TO ERR-PARAGRAPH                      17430003
175000        MOVE 'SEND BUFFER'  TO ERR-KEY                            17440003
175100        PERFORM P9999-GOT-PROBLEM                                 17450003
175200     END-IF                                                       17460003
175300     EXEC CICS SEND                                               17470003
175400               CONTROL                                            17480003
175500               CURSOR(WS-BUFFER-CURSOR)                           17490003
175600               RESP(WS-RESPONSE)                                  17500003
175700     END-EXEC                                                     17510003
175800     MOVE WS-RESPONSE       TO FILE-STATUS                        17520003
175900     IF NOT SUCCESS                                               17530003
176000        MOVE 'P9035-2'      TO ERR-PARAGRAPH                      17540003
176100        MOVE 'SEND CURSOR'  TO ERR-KEY                            17550003
176200        PERFORM P9999-GOT-PROBLEM                                 17560003
176300     END-IF.                                                      17570003
176400*                                                                 17580003
176500 P9100-XCTL-TO-CALLING-PROGRAM.                                   17590003
176600*                                                                 17600003
176700     IF PSTCA-FROM-PROGRAM NOT > SPACES                           17610003
176800        MOVE P03-PGM                 TO PSTCA-FROM-PROGRAM        17620003
176900     END-IF                                                       17630003
177000     MOVE SPACES                     TO PSTCA-VARIABLE-AREA       17640003
177100     EXEC CICS XCTL                                               17650003
177200               PROGRAM(PSTCA-FROM-PROGRAM)                        17660003
177300               COMMAREA(PSTCOMM-AREA)                             17670003
177400               LENGTH(PSTCOMM-LGTH)                               17680003
177500               RESP(WS-RESPONSE)                                  17690003
177600     END-EXEC                                                     17700003
177700     MOVE WS-RESPONSE                TO FILE-STATUS               17710003
177800     IF NOT SUCCESS                                               17720003
177900        MOVE 'P9100'                 TO ERR-PARAGRAPH             17730003
178000        PERFORM P9999-GOT-PROBLEM                                 17740003
178100     END-IF.                                                      17750003
178200*                                                                 17760003
178300 P9200-SETUP-SCR12A.                                              17770003
178400*                                                                 17780003
178500     MOVE SPACES                     TO P12ACOMM-AREA             17790003
178600     MOVE SCR12-ASGN-ID              TO P12ACA-ASGN-ID            17800003
178700     MOVE SCR12-A-FROM-DATE          TO P12ACA-A-FROM-DATE        17810003
178800     MOVE SCR12-A-TO-DATE            TO P12ACA-A-TO-DATE          17820003
178900     EXEC CICS XCTL                                               17830003
179000               PROGRAM(P12A-PGM)                                  17840003
179100               COMMAREA(PSTCOMM-AREA)                             17850003
179200               LENGTH(P12ACA-LGTH)                                17860003
179300               RESP(WS-RESPONSE)                                  17870003
179400     END-EXEC                                                     17880003
179500     MOVE WS-RESPONSE                TO FILE-STATUS               17890003
179600     IF NOT SUCCESS                                               17900003
179700        MOVE 'P9200'                 TO ERR-PARAGRAPH             17910003
179800        PERFORM P9999-GOT-PROBLEM                                 17920003
179900     END-IF.                                                      17930003
180000*                                                                 17940003
180100 P9500-SETUP-SCR998.                                              17950003
180200*                                                                 17960003
180300     MOVE SPACES            TO P998COMM-AREA                      17970003
180400     MOVE P12-PGM           TO P998CA-FROM-PROGRAM                17980003
180500     MOVE P12-MAP           TO P998CA-SCREEN-ID                   17990003
180600     MOVE EIBCPOSN          TO P998CA-CURSOR-POS                  18000003
180700     EXEC CICS XCTL                                               18010003
180800               PROGRAM(P998-PGM)                                  18020003
180900               COMMAREA(PSTCOMM-AREA)                             18030003
181000               LENGTH(PSTCOMM-LGTH)                               18040003
181100               RESP(WS-RESPONSE)                                  18050003
181200     END-EXEC                                                     18060003
181300     MOVE WS-RESPONSE       TO FILE-STATUS                        18070003
181400     IF NOT SUCCESS                                               18080003
181500        MOVE 'P9500'        TO ERR-PARAGRAPH                      18090003
181600        PERFORM P9999-GOT-PROBLEM                                 18100003
181700     END-IF.                                                      18110003
181800*                                                                 18120003
181900 P9810-PROCESS-OFFSET.                                            18130003
182000*                                                                 18140003
182100     MOVE PSTCA-DT-OS-FUN            TO PARM-CONV-TYPE            18150003
182200     MOVE PSTCA-DT-OS-DAYS           TO PARM-SEC-JULIAN-DAY       18160003
182300     MOVE PSTCA-DT-OS-HRMN           TO PARM-SEC-HRMN             18170003
182400     EXEC CICS LINK                                               18180003
182500               PROGRAM(P903-PGM)                                  18190003
182600               COMMAREA(DATE-CONVERSION-PARMS)                    18200003
182700               LENGTH(P903-LGTH)                                  18210003
182800               RESP(WS-RESPONSE)                                  18220003
182900     END-EXEC                                                     18230003
183000     MOVE WS-RESPONSE                TO FILE-STATUS               18240003
183100     IF NOT SUCCESS                                               18250003
183200        MOVE 'P9810-1'               TO ERR-PARAGRAPH             18260003
183300        MOVE 'P903'                  TO ERR-KEY                   18270003
183400        PERFORM P9999-GOT-PROBLEM                                 18280003
183500     END-IF.                                                      18290003
183600*                                                                 18300003
183700 P9990-CLEAR-SCREEN.                                              18310003
183800*                                                                 18320003
183900     EXEC CICS SEND CONTROL                                       18330003
184000                    ERASE                                         18340003
184100                    FREEKB                                        18350003
184200     END-EXEC                                                     18360003
184300     EXEC CICS RETURN END-EXEC.                                   18370003
184400*                                                                 18380003
184500 P9999-GOT-PROBLEM.                                               18390003
184600*                                                                 18400003
184700     MOVE P12-PGM                   TO ERR-PROGRAM                18410003
184800     MOVE DFHEIBLK                  TO ERR-EIBLK                  18420003
184900     EXEC CICS XCTL                                               18430003
185000               PROGRAM(PSTERR-PGM)                                18440003
185100               COMMAREA(PSTERAR-AREA)                             18450003
185200               LENGTH(PSTERAR-LGTH)                               18460003
185300               RESP(WS-RESPONSE)                                  18470003
185400     END-EXEC                                                     18480003
185500     EXEC CICS ABEND                                              18490003
185600               ABCODE(PSTERR-ABCODE)                              18500003
185700               CANCEL                                             18510003
185800     END-EXEC.                                                    18520003
185900*                                                                 18530003
186000 X9999-GOBACK.                                                    18540003
186100     GOBACK.                                                      18550003
