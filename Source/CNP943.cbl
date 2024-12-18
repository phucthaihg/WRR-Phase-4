000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID. CNP943.                                              00020000
000300***************************************************************** 00030000
000400* W R I T E   J O B   H I S T O R Y   R O U T I N E             * 00040000
000500***************************************************************** 00050000
000600*  DATE   INITIAL  LOG#   DESCRIPTION                             00060000
000700*-------- ------- ------  --------------------------------------- 00070000
000800*11/03/95   JES           WRITE TO JOB HISTORY FILE FOR CN.       00080000
000900*11/14/97   SAM           WRITE TO EMPLOYEE HISTORY FILE FOR CN.  00090000
001000*04/02/98   LMB   CNC0156 UPDATE EMP HIST POINTER FILE.           00100000
001100*05/11/98   GBJ     Y2K   Y2K TO JHIST-CLOCK-TIME.                00110000
001200*05/20/98   SWS           Y2K FOR WSHIST.                         00120000
001300*06/04/98   LMB   CNC0156 ACTIVATE POINTER FILE AND CONVERT EFF   00130000
001400*                         TIME TO SYSTEM TIME ZONE.               00140000
001500*08/26/98   LMB   CNC0160 ASSIGNMENT TYPE POINTER RECORDS CAPTURE 00150000
001600*                         EXTENDED RUN INFO.                      00160000
001700*                         CREATE POINTER RECORDS FOR ADJ-TU.      00170000
001800*                         CREATE ASGN TYPE POINTER RECORDS IF:    00180000
001900*                         - MARK UP ENDS NTFY HOLD                00190000
002000*                         - CANCEL AT HOME TERMINAL               00200000
002100*                         - TIE UP                                00210000
002200*                         - DEADHEAD                              00220000
002300*10/02/98   LMB   CNC0160 FIX BUG GETTING BLOCK CODE FOR XB JOBS. 00230000
002400*11/20/98   LMB   CNC0179 SET PERS REST IN GUAR POINTER RECORD.   00240000
002500*12/02/98   RRR   CNC0173 RECOMPILE FOR REVISED COPYBOOKS.        00250000
002600*12/15/98   AMF           YEAR 2000 SWEEP.                        00260000
002700*02/23/99   LMB   CNC0208 LOST WORK RECS ARE TYPE '03'.           00270000
002800*                         CREATE TYPE '02' RECS FOR ALL LAYOFF    00280000
002900*                         AND MARKUP RECS.                        00290000
003000*03/08/99   LMB   CNC0208 UPDATE LOST WORK FUNCTION.              00300000
003100*04/26/99   AJK   CNC0228 WRITE INFO TO JHIST DEPENDING ON        00310000
003200*                         EMPLOYEE FUNCTION (SEE P2050)           00320000
003300*10/12/99   PLS   CNC0263 ADD PHONE LOG PROCESSING.               00330000
003400*12/14/99   GER   CNC0259 ADD IC ATTEMPT-NOTIFY FUNCTIONS.        00340000
003500*12/20/99   AJK   CNC0268 ADD PHONE UPDATE VIA VRU CHANGES.       00350001
003600*03/24/00   AJK   CNC0275 ADD MERGER CLASSIFICATION LOGIC.        00360000
003700*                                                                 00370000
003800*02/13/01   TJR   CNC0338 ADD CODE TO SUPPORT NEW FUNCTION TYPES  00380000
003900*                         36-37 PENDING LAYOFF/MARKUP             00390000
004000*                                                                 00400000
004100*03/09/01   TJR   CNC0260 ADD WORK HISTORY 30 - ATTEMPT-NTFY-FUN  00410001
004200*                         TO THE POINTER FILE. JUST NEED TO       00420001
004300*                         RECOMPILE FOR COPYBOOK WSPOINTR.        00430001
004310*10/11/04   KJS  CNC0386A ADD P943-MSTR-RC-ADD-REST-DYS PROCESSING00431013
004400***************************************************************** 00440000
004500 ENVIRONMENT DIVISION.                                            00450000
004600 CONFIGURATION SECTION.                                           00460000
004700 SOURCE-COMPUTER. IBM-370.                                        00470000
004800 OBJECT-COMPUTER. IBM-370.                                        00480000
004900 DATA DIVISION.                                                   00490000
005000 WORKING-STORAGE SECTION.                                         00500000
005100 01  FILLER                        PIC X(10) VALUE 'P943 W/S'.    00510000
005200                                                                  00520000
005300 01  WS-FLAGS.                                                    00530000
005400     02  DONE-FLAG                 PIC X(1)  VALUE SPACE.         00540000
005500         88  DONE                            VALUE '1'.           00550000
005600     02  FOUND-FLAG                PIC X(1)  VALUE SPACE.         00560000
005700         88  FOUND-IT                        VALUE '1'.           00570000
005800     02  WRITE-JHIST-FLAG          PIC X(1)  VALUE SPACE.         00580000
005900         88  WRITE-JHIST                     VALUE ' '.           00590000
006000         88  DONT-WRITE-JHIST                VALUE '1'.           00600000
006010     02  WRITE-LW2-FLAG            PIC X(1)  VALUE SPACE.         00601006
006020         88  WRITE-LW2                       VALUE ' '.           00602006
006030         88  DONT-WRITE-LW2                  VALUE '1'.           00603006
006100                                                                  00610000
006200 01  WS-MISC.                                                     00620000
006300     02  WS-SAVE-AHM-KEY.                                         00630000
006400         03  FILLER                PIC X(16) VALUE SPACES.        00640000
006500         03  WS-SAVE-AHM-TRAIN     PIC X(10) VALUE SPACES.        00650000
006600         03  FILLER                PIC X(17) VALUE SPACES.        00660000
006700     02  WS-COMP-AHM-KEY.                                         00670000
006800         03  FILLER                PIC X(16) VALUE SPACES.        00680000
006900         03  WS-COMP-AHM-TRAIN     PIC X(10) VALUE SPACES.        00690000
007000         03  FILLER                PIC X(17) VALUE SPACES.        00700000
007100     02  WS-BLOCK                  PIC X(02) VALUE SPACES.        00710000
007200     02  LENGTH-OF-DUTY.                                          00720000
007300         04  LENGTH-OF-DUTY-HR     PIC 9(02)  VALUE ZEROS.        00730000
007400         04  LENGTH-OF-DUTY-MN     PIC 9(02)  VALUE ZEROS.        00740000
007500     02  TOTAL-TIME-DIFF           PIC 9(05)  VALUE ZEROS.        00750000
007600     02  FILLER REDEFINES TOTAL-TIME-DIFF.                        00760000
007700         04  TOTAL-TIME-HOURS      PIC 9(03).                     00770000
007800         04  TOTAL-TIME-MINUTES    PIC 9(02).                     00780000
007900     02  WS-HOLD-TIME              PIC 9(05)  VALUE ZEROS.        00790000
008000     02  WS-XREF-KEY               PIC X(26)  VALUE SPACES.       00800000
008100     02  WS-FUN-TYPE               PIC X(02)  VALUE SPACES.       00810000
008200         88  CALL-TYPE-FUN                    VALUE '01',         00820000
008300                                                    '04',         00830000
008400                                                    '16',         00840000
008500                                                    '35',         00850000
008600                                                    '39'.         00860000
008700                                                                  00870000
008800 01  WS-SUBSCRIPTS.                                               00880000
008900     02  WS-WRITE-MAX              PIC 9(2)  VALUE 25.            00890000
009000                                                                  00900000
009100 01  WS-APPLID                     PIC X(8)  VALUE SPACES.        00910000
009200                                                                  00920000
009300 01  WS-HIST-CLOCK-TIME.                                          00930000
009400     02  WS-HIST-CCYYMMDD-TIME.                                   00940000
009500         03  WS-HIST-CE             PIC 9(02)  VALUE ZEROS.       00950000
009600         03  WS-HIST-DATE.                                        00960000
009700             04  WS-HIST-YY         PIC 9(02)  VALUE ZEROS.       00970000
009800             04  WS-HIST-MM         PIC 9(02)  VALUE ZEROS.       00980000
009900             04  WS-HIST-DD         PIC 9(02)  VALUE ZEROS.       00990000
010000         03  WS-HIST-TIME           PIC 9(04)  VALUE ZEROS.       01000000
010100         03  FILLER                 PIC 9(02)  VALUE ZEROS.       01010000
010200         03  WS-HIST-DATE-TIME-TIE  PIC 9(02)  VALUE ZEROS.       01020000
010300 01  FILLER REDEFINES WS-HIST-CLOCK-TIME.                         01030000
010400     02  WS-HIST-DATE-CCYYMMDD      PIC X(08).                    01040000
010500     02  WS-HIST-TIME-AREA          PIC X(08).                    01050000
010600 01  FILLER REDEFINES WS-HIST-CLOCK-TIME.                         01060000
010700     02  FILLER                     PIC X(02).                    01070000
010800     02  WS-HIST-DATE-TIME          PIC X(14).                    01080000
010900                                                                  01090000
011000 01  WS-SAVE-JOB-HIST               PIC X(179) VALUE SPACES.      01100000
011100*                                                                 01110000
011200 01  WORK-CNTLKEY.                                                01120000
011300     02  WK-CNTL-REC-TYPE           PIC X(2).                     01130000
011400     02  WK-CNTL-DIST               PIC X(2).                     01140000
011500     02  WK-CNTL-SUB-DIST           PIC X(2).                     01150000
011600     02  WK-CNTL-CRAFT-XB           PIC X(2).                     01160000
011700     02  WK-CNTL-ASGN-TYPE          PIC X(1).                     01170000
011800     02  WK-CNTL-ASGN.                                            01180000
011900         04  WK-CNTL-ASGN-1         PIC X(1).                     01190000
012000         04  WK-CNTL-ASGN-2         PIC X(1).                     01200000
012100         04  WK-CNTL-ASGN-3         PIC X(1).                     01210000
012200         04  WK-CNTL-ASGN-4         PIC X(1).                     01220000
012300         04  WK-CNTL-ASGN-5         PIC X(1).                     01230000
012400         04  WK-CNTL-ASGN-6         PIC X(1).                     01240000
012500     02  FILLER                     PIC X(5).                     01250000
012600                                                                  01260000
012700 01  WS-KEYS.                                                     01270000
012800     02  WS-AHIST-KEY.                                            01280000
012900         04  WS-AHK1-DIST           PIC X(02)  VALUE SPACES.      01290000
013000         04  WS-AHK1-SUB-DIST       PIC X(02)  VALUE SPACES.      01300000
013100         04  WS-AHK1-POOL           PIC X(02)  VALUE SPACES.      01310000
013200         04  WS-AHK1-EFF-DATE-TIME.                               01320000
013300             05  WS-AHK1-EFF-DATE.                                01330000
013400                 06  WS-AHK1-EFF-YR PIC X(02)  VALUE SPACES.      01340000
013500                 06  WS-AHK1-EFF-MO PIC X(02)  VALUE SPACES.      01350000
013600                 06  WS-AHK1-EFF-DY PIC X(02)  VALUE SPACES.      01360000
013700             05  WS-AHK1-EFF-TIME.                                01370000
013800                 06  WS-AHK1-EFF-HR PIC X(02)  VALUE SPACES.      01380000
013900                 06  WS-AHK1-EFF-MN PIC X(02)  VALUE SPACES.      01390000
014000         04  WS-AHK1-ASSIGNMENT.                                  01400000
014100             05  WS-AHK1-AJOB-ASGN  PIC X(08)  VALUE SPACES.      01410000
014200             05  FILLER             PIC X(02)  VALUE SPACES.      01420000
014300                                                                  01430000
014400***************************************************************** 01440000
014500***                 I/O STATUS CHECK FIELDS                       01450000
014600***************************************************************** 01460000
014700 01  WS-RESPONSE                    PIC S9(8)  COMP VALUE ZEROES. 01470000
014800 01  FILE-STATUS                    PIC 9(4)   VALUE ZEROES.      01480000
014900     COPY IOCODES.                                                01490000
015000***************************************************************** 01500000
015100***                   COMMAREA COPYBOOKS                          01510000
015200***************************************************************** 01520000
015300     COPY P943COMM.                                               01530000
015400***************************************************************** 01540000
015500***                  CALLED ROUTINES COPYBOOKS.                   01550000
015600***************************************************************** 01560000
015700     COPY P901COMM.                                               01570000
015800     COPY P903COMM.                                               01580000
015900     COPY P909COMM.                                               01590000
016000     COPY P942COMM.                                               01600000
016100     COPY P980COMM.                                               01610000
016200     COPY PSTERAR.                                                01620000
016300***************************************************************** 01630000
016400***                       FILE COPYBOOKS                          01640000
016500***************************************************************** 01650000
016600     COPY WSJHIST.                                                01660000
016700     COPY WSHIST.                                                 01670000
016800     COPY WSCNTL.                                                 01680000
016900     COPY WSAHIST.                                                01690000
017000     COPY WSASGN.                                                 01700000
017100     COPY WSEMPTRK.                                               01710000
017200     COPY WSMSTR.                                                 01720000
017300     COPY WSPHONLG.                                               01730000
017400     COPY WSPOINTR.                                               01740000
017500     COPY WSAHMISC.                                               01750000
017600***************************************************************** 01760000
017700***                      MISC COPYBOOKS                           01770000
017800***************************************************************** 01780000
017900     COPY PSTCCRFT.                                               01790000
018000     COPY WSAJDEFN.                                               01800000
018100     COPY WSSYDTTM.                                               01810000
018200     COPY WSZONE.                                                 01820000
018300     COPY WSEDDATE.                                               01830000
018400                                                                  01840000
018500 LINKAGE SECTION.                                                 01850000
018600 01  DFHCOMMAREA.                                                 01860000
018700     05  FILLER                     PIC X(153).                   01870000
018800                                                                  01880000
018900 PROCEDURE DIVISION.                                              01890000
019000*                                                                 01900000
019100 P0000-MAINLINE.                                                  01910000
019200*                                                                 01920000
019300     EXEC CICS IGNORE CONDITION                                   01930000
019400               ERROR                                              01940000
019500     END-EXEC                                                     01950000
019600     EXEC CICS HANDLE ABEND                                       01960000
019700               LABEL(P9999-GOT-PROBLEM)                           01970000
019800     END-EXEC                                                     01980000
019900     COPY ABSTIME.                                                01990000
020000     IF EIBCALEN = ZERO                                           02000000
020100        PERFORM P9999-GOT-PROBLEM                                 02010000
020200     END-IF                                                       02020000
020300     MOVE DFHCOMMAREA              TO P943-COMMAREA-PARMS         02030000
020400     PERFORM P1000-PROCESS                                        02040000
020500     PERFORM P9000-RETURN.                                        02050000
020600*                                                                 02060000
020700 P1000-PROCESS.                                                   02070000
020800*                                                                 02080000
020900     IF P943-COMMAREA-PARMS NOT > SPACES                          02090000
021000        MOVE 'P1000-1'             TO ERR-PARAGRAPH               02100000
021100        MOVE 'NOPARMS'             TO ERR-KEY                     02110000
021200        PERFORM P9999-GOT-PROBLEM                                 02120000
021300     END-IF                                                       02130000
021400                                                                  02140000
021500     EXEC CICS ASSIGN                                             02150000
021600               APPLID(WS-APPLID)                                  02160000
021700     END-EXEC                                                     02170000
021800                                                                  02180000
021900     EVALUATE TRUE                                                02190000
022000        WHEN P943-JOB-HIST-FUNCTION                               02200000
022100           PERFORM P3000-JOB-HIST-FUNCTION                        02210000
022200        WHEN P943-EMPLOYEE-FUNCTION                               02220000
022300           PERFORM P2000-EMPLOYEE-FUNCTION                        02230000
022400        WHEN P943-UPD-LOST-WORK-FUN                               02240000
022500           PERFORM P4000-UPDATE-LOST-WORK                         02250000
022510        WHEN P943-UPD-LOST-WORK2-FUN                              02251006
022520           PERFORM P4500-UPDATE-LOST-WORK2                        02252006
022600        WHEN P943-EMP-PHONELOG-FUNCTION                           02260000
022700           PERFORM P5000-EMP-PHONELOG-FUNCTION                    02270000
022800        WHEN OTHER                                                02280000
022900           MOVE 'P1000-2'          TO ERR-PARAGRAPH               02290000
023000           MOVE 'BADFUNC'          TO ERR-KEY                     02300000
023100           PERFORM P9999-GOT-PROBLEM                              02310000
023200     END-EVALUATE.                                                02320000
023300*                                                                 02330000
023400 P2000-EMPLOYEE-FUNCTION.                                         02340000
023500*                                                                 02350000
023600     IF P943-EMP-NBR                 NOT > SPACES                 02360009
023700        MOVE 'P2000-1'                  TO ERR-PARAGRAPH          02370009
023800        MOVE 'NOEMPNBR'                 TO ERR-KEY                02380009
023900        PERFORM P9999-GOT-PROBLEM                                 02390000
024000     END-IF                                                       02400000
024100     IF P943-EMP-FUNCTION            NOT > SPACES                 02410009
024200        MOVE 'P2000-2'                  TO ERR-PARAGRAPH          02420009
024300        MOVE 'NOFUNC'                   TO ERR-KEY                02430009
024400        PERFORM P9999-GOT-PROBLEM                                 02440000
024500     END-IF                                                       02450000
024600                                                                  02460000
024700     IF P943-USERID                  NOT > SPACES                 02470009
024800     OR P943-USERID                      = WS-APPLID              02480009
024900        MOVE EIBTRNID                   TO P943-USERID            02490009
025000     END-IF                                                       02500000
025100                                                                  02510000
025200     IF P943-ADD-SHOW-UP-FUN                                      02520000
025300     OR P943-DEL-SHOW-UP-FUN                                      02530009
025500        EXEC CICS ASSIGN                                          02550000
025600                  USERID(P943-SHOW-UP-USERID)                     02560000
025700        END-EXEC                                                  02570000
025800     END-IF                                                       02580000
025900                                                                  02590000
026400     IF  P943-MISC-COMMENTS-FUN                                   02640009
026500     AND P943-CLOCK-TIME                 > SPACES                 02650009
026600        SET  DE-REFORMAT-ONLY           TO TRUE                   02660009
026700        SET  DE-YYMMDD-FORMAT           TO TRUE                   02670009
026800        MOVE P943-C-YYMMDD              TO DE-YYMMDD              02680009
026900                                           WS-HIST-DATE           02690009
027000        PERFORM P8998-DATEEDIT                                    02700000
027100        MOVE DE-YYMMDD-CE               TO WS-HIST-CE             02710009
027200        MOVE P943-TIME-AREA             TO WS-HIST-TIME-AREA      02720009
027300     ELSE                                                         02730000
027400        EXEC CICS ASKTIME                                         02740000
027500                  ABSTIME(WS-ABSTIME)                             02750000
027600        END-EXEC                                                  02760000
027700        ADD  WS-ABSTIME-OFFSET          TO WS-ABSTIME             02770009
027800        EXEC CICS FORMATTIME                                      02780000
027900                  ABSTIME(WS-ABSTIME)                             02790000
028000                  YYYYMMDD(WS-SYSTEM-DATE-CENT)                   02800000
028100                  TIME(WS-SYSTEM-TIME-AREA)                       02810000
028200        END-EXEC                                                  02820000
028300        MOVE WS-SYSTEM-DATE-CENT        TO WS-HIST-DATE-CCYYMMDD  02830009
028400        MOVE WS-SYSTEM-TIME-AREA        TO WS-HIST-TIME-AREA      02840009
028500     END-IF                                                       02850000
028900                                                                  02890000
029000     MOVE ZEROS                         TO WS-HIST-DATE-TIME-TIE  02900009
029100     MOVE SPACES                        TO WS-HIST                02910009
029200     MOVE P943-EMP-NBR                  TO HIST-EMP-NBR           02920009
029300     MOVE P943-EMP-FUNCTION             TO HIST-FUN               02930009
029400     MOVE P943-DIST                     TO HIST-DIST              02940009
029500     MOVE P943-SDIST                    TO HIST-SDIST             02950009
029600     MOVE P943-REC-TYPE-1               TO HIST-REC-TYPE-1        02960009
029700                                                                  02970000
029800     IF  P943-ATTEMPT-NTFY-FUN                                    02980009
029900     AND P943-ATTEMPT-TYPE               > SPACES                 02990009
030000        MOVE P943-ATTEMPT-TYPE          TO HIST-ATTEMPT-TYPE      03000009
030100     END-IF                                                       03010000
030200                                                                  03020000
030300     MOVE P943-EMP-FUNCTION             TO WS-FUN-TYPE            03030009
030400     IF CALL-TYPE-FUN                                             03040000
030500        MOVE P943-ASGN-MERG-CLASS       TO HIST-ASGN-MERG-CLASS   03050009
030600     ELSE                                                         03060000
030700        MOVE SPACES                     TO HIST-ASGN-MERG-CLASS   03070009
030800     END-IF                                                       03080000
030900                                                                  03090000
031000     IF P943-CALL-FUN                                             03100009
031100     OR P943-TIE-UP-FUN                                           03110009
031200     OR P943-LAYOFF-FUN                                           03120009
031300     OR P943-MARKUP-FUN                                           03130009
031400     OR P943-PENDING-LAYOFF-FUN                                   03140009
031500     OR P943-PENDING-MARKUP-FUN                                   03150009
031600     OR P943-LOST-WORK-FUN                                        03160009
031700        MOVE SPACES                     TO P980-COMMAREA-PARMS    03170009
031800        SET P980-QUICKTIE-RESTRICT-FUNC TO TRUE                   03180000
031900        MOVE P943-EMP-NBR               TO P980-EMP-NO            03190009
032000        EXEC CICS LINK                                            03200000
032100                  PROGRAM (P980-PGM)                              03210009
032200                  COMMAREA(P980-COMMAREA-PARMS)                   03220000
032300                  LENGTH  (P980-LGTH)                             03230009
032400                  RESP    (WS-RESPONSE)                           03240009
032500        END-EXEC                                                  03250000
032600        MOVE WS-RESPONSE                TO FILE-STATUS            03260009
032700        IF NOT SUCCESS                                            03270000
032800           MOVE 'P2000-3'               TO ERR-PARAGRAPH          03280009
032900           MOVE 'LINK980'               TO ERR-KEY                03290009
033000           PERFORM P9999-GOT-PROBLEM                              03300000
033100        END-IF                                                    03310000
033200        IF P980-QUICKTIE-RESTRICTED                               03320000
033300           SET  HIST-QUICK-TIE          TO TRUE                   03330009
033400        END-IF                                                    03340000
033500     END-IF                                                       03350000
033600                                                                  03360010
033610     IF P943-LOST-WORK-FUN                                        03361009
033611        MOVE SPACES                     TO WS-EMP-TRACK           03361109
033612        MOVE P943-EMP-NBR               TO EMPTRK-EMP-NO          03361209
033613        MOVE P943-FUN39-TRK-TYPE        TO EMPTRK-EMP-TYPE        03361309
033614        MOVE P943-FUN39-TRK-SEQ         TO EMPTRK-SEQ             03361409
033615        MOVE EMPTRKKEY-AREA             TO ETRACK-EMPKEY          03361509
033616        EXEC CICS READ                                            03361609
033617                  DATASET  (ETRACK-VIA-EMP)                       03361709
033618                  INTO     (WS-EMP-TRACK)                         03361809
033619                  RIDFLD   (ETRACK-EMPKEY)                        03361909
033620                  LENGTH   (ETRACK-EMP-RLGTH)                     03362009
033621                  KEYLENGTH(ETRACK-EMP-KLGTH)                     03362109
033622                  RESP     (WS-RESPONSE)                          03362209
033623        END-EXEC                                                  03362309
033624        MOVE WS-RESPONSE                TO FILE-STATUS            03362409
033625        IF NOT SUCCESS                                            03362509
033626           MOVE 'P2000-4'               TO ERR-PARAGRAPH          03362609
033627           MOVE ETRACK-EMPKEY           TO ERR-KEY                03362709
033628           PERFORM P9999-GOT-PROBLEM                              03362809
033629        END-IF                                                    03362909
033635        MOVE EMPTRK-TRACKING-REASON     TO HIST-FUN39-LW-REASON   03363509
033636     END-IF                                                       03363609
033640                                                                  03364010
033700     MOVE '01'                          TO P943-RETURN-STATUS     03370009
033800     SET  NO-RECORD-FND                 TO TRUE                   03380009
033900     PERFORM UNTIL SUCCESS                                        03390009
034000        OR WS-HIST-DATE-TIME-TIE         > WS-WRITE-MAX           03400009
034100        MOVE WS-HIST-CLOCK-TIME         TO HIST-CLOCK-TIME        03410009
034200        MOVE HIST-NBR-KEY               TO HISTNBR                03420009
034300        PERFORM P8000-WRITE-HISTORY                               03430009
034400        MOVE FILE-STATUS                TO P943-RETURN-STATUS     03440009
034500        ADD  1                          TO WS-HIST-DATE-TIME-TIE  03450009
034600        IF NOT SUCCESS                                            03460009
034700           IF NOT DUP-KEY-ERR                                     03470009
034800              MOVE 'P2000-5'            TO ERR-PARAGRAPH          03480009
034900              MOVE HISTNBR              TO ERR-KEY                03490009
035000              PERFORM P9999-GOT-PROBLEM                           03500009
035100           END-IF                                                 03510009
035200        END-IF                                                    03520009
035300     END-PERFORM                                                  03530009
035400                                                                  03540010
035500     MOVE P943-EMP-FUNCTION             TO POINT-FUN              03550009
035600     IF   P943-EMP-NBR                   < '999999996'            03560009
035700     AND (POINT-ASGN-CHANGE-FUN                                   03570009
035800     OR   POINT-GUARANTEE-FUN                                     03580009
035900     OR   POINT-LOST-WORK-FUN)                                    03590009
036000        IF  P943-MASTER-RC-FUN                                    03600009
036100        AND HIST-MSTR-RC-FIELD       NOT = 'PERS REST '           03610009
036200           CONTINUE                                               03620000
036300        ELSE                                                      03630000
036400           PERFORM P2200-CREATE-POINTER                           03640000
036500        END-IF                                                    03650000
036600     END-IF                                                       03660000
036700                                                                  03670010
036800     PERFORM P2050-WRITE-JOB-HISTORY                              03680009
036810     .                                                            03681009
036900*                                                                 03690000
037000 P2050-WRITE-JOB-HISTORY.                                         03700000
037100*                                                                 03710000
037200     MOVE SPACES                       TO WS-JOB-HIST             03720000
037300                                          WRITE-JHIST-FLAG        03730000
037400                                          FOUND-FLAG              03740000
037500     IF P943-CALL-FUN                                             03750000
037600     OR P943-TIE-UP-FUN                                           03760000
037700     OR P943-ADJ-TIE-UP-FUN                                       03770000
037800     OR P943-CALL-REL-FUN                                         03780000
037900        MOVE SPACES                     TO WS-AHIST-MISC          03790000
038000        IF P943-CALL-FUN                                          03800000
038100           MOVE P943-FUN01-XREF-KEY     TO AHM-KEY                03810000
038200                                           WS-XREF-KEY            03820000
038300        ELSE                                                      03830000
038400           IF P943-TIE-UP-FUN                                     03840000
038500           OR P943-ADJ-TIE-UP-FUN                                 03850000
038600              MOVE P943-FUN02-XREF-KEY  TO AHM-KEY                03860000
038700                                           WS-XREF-KEY            03870000
038800           ELSE                                                   03880000
038900              MOVE P943-FUN04-XREF-KEY  TO AHM-KEY                03890000
039000                                           WS-XREF-KEY            03900000
039100           END-IF                                                 03910000
039200        END-IF                                                    03920000
039300        SET AHM-EMPLOYEE-RECORD         TO TRUE                   03930000
039400        MOVE '01'                       TO AHM-CRAFT-SEQ          03940000
039500        PERFORM P8700-GET-AHMISC-RECORD                           03950000
039600        IF FOUND-IT                                               03960000
039700           MOVE AHM-ODA-ASGN            TO JHIST-JOB-ASGN         03970000
039800                                           JHIST-JOB-ASGN2        03980000
039900           MOVE AHM-ODA-CC              TO JHIST-JOB-CRAFT        03990000
040000                                           JHIST-JOB-CRAFT2       04000000
040100        ELSE                                                      04010000
040200           IF P943-CALL-REL-FUN                                   04020000
040300              IF P943-NORM-ASGN-NO > SPACES                       04030000
040400                    MOVE P943-NORM-ASGN-NO TO JHIST-JOB-ASGN      04040000
040500                                              JHIST-JOB-ASGN2     04050000
040600                    MOVE P943-NORM-ASGN-CC TO JHIST-JOB-CRAFT     04060000
040700                                              JHIST-JOB-CRAFT2    04070000
040800              ELSE                                                04080000
040900                 SET DONT-WRITE-JHIST   TO TRUE                   04090000
041000              END-IF                                              04100000
041100           ELSE                                                   04110000
041200              SET DONT-WRITE-JHIST      TO TRUE                   04120000
041300           END-IF                                                 04130000
041400        END-IF                                                    04140000
041500     END-IF                                                       04150000
041600                                                                  04160000
041700     IF P943-POOL-ASG > SPACES                                    04170000
041800        IF P943-POOL-ASG = JOB-DEF-YARD-POOL                      04180000
041900           MOVE 'Y'                     TO JHIST-JOB-TYPE         04190000
042000                                           JHIST-JOB-TYPE2        04200000
042100        ELSE                                                      04210000
042200           IF P943-POOL-ASG = JOB-DEF-LOCAL-POOL                  04220000
042300              MOVE 'L'                  TO JHIST-JOB-TYPE         04230000
042400                                           JHIST-JOB-TYPE2        04240000
042500           ELSE                                                   04250000
042600              MOVE 'U'                  TO JHIST-JOB-TYPE         04260000
042700                                           JHIST-JOB-TYPE2        04270000
042800           END-IF                                                 04280000
042900        END-IF                                                    04290000
043000     ELSE                                                         04300000
043100        IF P943-ASG-XB                                            04310000
043200            MOVE 'X'                    TO JHIST-JOB-TYPE         04320000
043300                                           JHIST-JOB-TYPE2        04330000
043400        ELSE                                                      04340000
043500            MOVE 'U'                    TO JHIST-JOB-TYPE         04350000
043600                                           JHIST-JOB-TYPE2        04360000
043700        END-IF                                                    04370000
043800     END-IF                                                       04380000
043900                                                                  04390000
044000     MOVE P943-DIST                     TO JHIST-JOB-DIST         04400000
044100                                           JHIST-JOB-DIST2        04410000
044200     MOVE P943-SDIST                    TO JHIST-JOB-SUB-DIST     04420000
044300                                           JHIST-JOB-SUB-DIST2    04430000
044400                                                                  04440000
044500     IF P943-REPOSITION-FUN                                       04450000
044600        IF P943-FUN05-OLD-DATE-TIME > SPACES                      04460000
044700           MOVE P943-FUN05-OLD-DATE-TIME                          04470000
044800                             TO JHIST-FUN05-OLD-DATE-TIME         04480000
044900        END-IF                                                    04490000
045000     END-IF                                                       04500000
045100                                                                  04510000
045200     IF  (NOT P943-CALL-FUN)                                      04520000
045300     AND (NOT P943-TIE-UP-FUN)                                    04530000
045400     AND (NOT P943-ADJ-TIE-UP-FUN)                                04540000
045500     AND (NOT P943-CALL-REL-FUN)                                  04550000
045600        IF P943-CARRTURN-FUN                                      04560000
045700           MOVE P943-CARRIED-TURN-BY(5:6)  TO JHIST-JOB-ASGN      04570000
045800                                              JHIST-JOB-ASGN2     04580000
045900           MOVE P943-CRAFT                 TO JHIST-JOB-CRAFT     04590000
046000                                              JHIST-JOB-CRAFT2    04600000
046100        ELSE                                                      04610000
046200           IF P943-NORM-ASGN-NO > SPACES                          04620000
046300              MOVE P943-NORM-ASGN-NO       TO JHIST-JOB-ASGN      04630000
046400                                              JHIST-JOB-ASGN2     04640000
046500              MOVE P943-NORM-ASGN-CC       TO JHIST-JOB-CRAFT     04650000
046600                                              JHIST-JOB-CRAFT2    04660000
046700           ELSE                                                   04670000
046800              MOVE P943-TEMP-ASGN-NO       TO JHIST-JOB-ASGN      04680000
046900                                              JHIST-JOB-ASGN2     04690000
047000              MOVE P943-TEMP-ASGN-CC       TO JHIST-JOB-CRAFT     04700000
047100                                              JHIST-JOB-CRAFT2    04710000
047200           END-IF                                                 04720000
047300        END-IF                                                    04730000
047400     END-IF                                                       04740000
047500                                                                  04750000
047600     EXEC CICS ASSIGN                                             04760000
047700               USERID(JHIST-USERID)                               04770000
047800     END-EXEC                                                     04780000
047900                                                                  04790000
048000     MOVE P943-EMP-FUNCTION             TO JHIST-FUNCTION         04800000
048100     MOVE P943-EFF-DATE-TIME            TO JHIST-EFF-DATE-TIME    04810000
048200     MOVE P943-EMP-NBR                  TO JHIST-EMP-NBR          04820000
048300     MOVE P943-EMP-NBR-AFFECTED         TO JHIST-EMP-NBR-AFFECTED 04830000
048400     MOVE P943-IN-OUT                   TO JHIST-IN-OUT           04840000
048500                                                                  04850000
048600     MOVE P943-GENERIC-AREA(1:26)       TO WS-AHIST-KEY           04860000
048700     EVALUATE TRUE                                                04870000
048800        WHEN P943-CALL-FUN                                        04880000
048900             MOVE P943-FUN01-XREF-KEY   TO JHIST-FUN01-XREF-KEY   04890000
049000                                                                  04900000
049100             MOVE P943-FUN01-TYPE       TO JHIST-FUN01-TYPE       04910000
049200             IF JHIST-EMP-NBR-AFFECTED NOT > SPACES               04920000
049300                MOVE P943-EMP-NBR       TO JHIST-EMP-NBR-AFFECTED 04930000
049400             END-IF                                               04940000
049500        WHEN P943-TIE-UP-FUN                                      04950000
049600             MOVE P943-FUN02-XREF-KEY   TO JHIST-FUN02-XREF-KEY   04960000
049700             MOVE ZEROS                 TO DATE-CONVERSION-PARMS  04970000
049800             SET PARM-DIFF              TO TRUE                   04980000
049900             MOVE P943-E-DATE           TO PARM-PRI-DATE-GREG     04990000
050000             MOVE P943-E-TOD            TO PARM-PRI-HRMN          05000000
050100             MOVE WS-AHK1-EFF-DATE      TO PARM-SEC-DATE-GREG     05010000
050200             MOVE WS-AHK1-EFF-TIME      TO PARM-SEC-HRMN          05020000
050300             PERFORM P2075-COMPUTE-TIME-WORKED                    05030000
050400             MOVE LENGTH-OF-DUTY        TO JHIST-FUN02-TIME-WORKED05040000
050500             MOVE P943-FUN02-ARR-DATE   TO JHIST-FUN02-ARR-DATE   05050000
050600             MOVE P943-FUN02-ARR-TIME   TO JHIST-FUN02-ARR-TIME   05060000
050700             MOVE P943-FUN02-MILES      TO JHIST-FUN02-MILES      05070000
050800        WHEN P943-CALL-REL-FUN                                    05080000
050900             MOVE P943-FUN04-XREF-KEY   TO JHIST-FUN04-XREF-KEY   05090000
051000             MOVE ZEROS                 TO DATE-CONVERSION-PARMS  05100000
051100             SET PARM-DIFF              TO TRUE                   05110000
051200             MOVE P943-E-DATE           TO PARM-PRI-DATE-GREG     05120000
051300             MOVE P943-E-TOD            TO PARM-PRI-HRMN          05130000
051400             MOVE WS-AHK1-EFF-DATE      TO PARM-SEC-DATE-GREG     05140000
051500             MOVE WS-AHK1-EFF-TIME      TO PARM-SEC-HRMN          05150000
051600             PERFORM P2075-COMPUTE-TIME-WORKED                    05160000
051700             MOVE LENGTH-OF-DUTY        TO JHIST-FUN04-TIME-WORKED05170000
051800             MOVE P943-EFF-DATE-TIME TO JHIST-FUN04-ARR-DATE-TIME 05180000
051900             MOVE P943-FUN04-TYPE       TO JHIST-FUN04-TYPE       05190000
052000             MOVE P943-FUN04-MILES      TO JHIST-FUN04-MILES      05200000
052100        WHEN P943-REPOSITION-FUN                                  05210000
052200             CONTINUE                                             05220000
052300        WHEN P943-ADJ-TIE-UP-FUN                                  05230000
052400             MOVE P943-FUN02-XREF-KEY   TO JHIST-FUN06-XREF-KEY   05240000
052500             MOVE ZEROS                 TO DATE-CONVERSION-PARMS  05250000
052600             SET PARM-DIFF              TO TRUE                   05260000
052700             MOVE P943-E-DATE           TO PARM-PRI-DATE-GREG     05270000
052800             MOVE P943-E-TOD            TO PARM-PRI-HRMN          05280000
052900             MOVE WS-AHK1-EFF-DATE      TO PARM-SEC-DATE-GREG     05290000
053000             MOVE WS-AHK1-EFF-TIME      TO PARM-SEC-HRMN          05300000
053100             PERFORM P2075-COMPUTE-TIME-WORKED                    05310000
053200             MOVE LENGTH-OF-DUTY        TO JHIST-FUN06-TIME-WORKED05320000
053300             MOVE P943-FUN02-ARR-DATE   TO JHIST-FUN06-ARR-DATE   05330000
053400             MOVE P943-FUN02-ARR-TIME   TO JHIST-FUN06-ARR-TIME   05340000
053500        WHEN P943-UFP-ADD-FUN                                     05350000
053600             CONTINUE                                             05360000
053700        WHEN P943-UFP-TRANSFER-FUN                                05370000
053800             CONTINUE                                             05380000
053900        WHEN P943-UFP-CUT-FUN                                     05390000
054000             CONTINUE                                             05400000
054100        WHEN P943-XB-ADD-FUN                                      05410000
054200                CONTINUE                                          05420000
054300        WHEN P943-XB-CUT-FUN                                      05430000
054400                CONTINUE                                          05440000
054500        WHEN P943-XB-REP-FUN                                      05450000
054600                CONTINUE                                          05460000
054700        WHEN P943-DISPLACED-FUN                                   05470000
054800             IF HIST-MOVE-TYPE > SPACE                            05480000
054900                MOVE HIST-MOVE-TYPE     TO JHIST-FUN20-MOVE-TYPE  05490000
055000             END-IF                                               05500000
055100             IF JHIST-EMP-NBR-AFFECTED NOT > SPACES               05510000
055200                MOVE P943-EMP-NBR       TO JHIST-EMP-NBR-AFFECTED 05520000
055300             END-IF                                               05530000
055400        WHEN P943-SEN-MOVE-FUN                                    05540000
055500             MOVE P943-MOVE-TYPE        TO JHIST-FUN22-MOVE-TYPE  05550000
055600             MOVE P943-MOVE-REASON      TO JHIST-FUN22-MOVE-REASON05560000
055700             IF HIST-MOVE-TYPE > SPACE                            05570000
055800                MOVE HIST-MOVE-TYPE                               05580000
055900                                 TO JHIST-FUN22-SEN-MOVE-ASGN-TYPE05590000
056000             END-IF                                               05600000
056100**AJK        MOVE P943-MOVE-TEMP-ASGN-FLAG                        05610000
056200**AJK             TO JHIST-FUN22-TEMP-ASGN-FLAG                   05620000
056300        WHEN P943-REJECT-TURN-FUN                                 05630000
056400             CONTINUE                                             05640000
056500        WHEN P943-CARRTURN-FUN                                    05650000
056600             MOVE P943-CARRIED-TURN                               05660000
056700                                   TO JHIST-FUN53-CARRIED-TURN    05670000
056800             MOVE P943-CARRIED-TURN-BY                            05680000
056900                                   TO JHIST-FUN53-CARRIED-TURN-BY 05690000
057000*????   WHEN P943-CALLED-WITHIN-SPREAD-FUN                        05700000
057100*                                                                 05710000
057200        WHEN OTHER                                                05720000
057300           SET DONT-WRITE-JHIST TO TRUE                           05730000
057400     END-EVALUATE                                                 05740000
057500**************************************************************    05750000
057600     IF WRITE-JHIST                                               05760000
057700        PERFORM P3020-RESOLVE-JHIST-DUPS                          05770000
057800     END-IF.                                                      05780000
057900*                                                                 05790000
058000 P2075-COMPUTE-TIME-WORKED.                                       05800000
058100*                                                                 05810000
058200     EXEC CICS LINK                                               05820000
058300               PROGRAM(P903-PGM)                                  05830000
058400               COMMAREA(DATE-CONVERSION-PARMS)                    05840000
058500               LENGTH(P903-LGTH)                                  05850000
058600               RESP(WS-RESPONSE)                                  05860000
058700     END-EXEC                                                     05870000
058800     MOVE WS-RESPONSE TO FILE-STATUS                              05880000
058900     IF NOT SUCCESS                                               05890000
059000        MOVE 'P2075-1'           TO ERR-PARAGRAPH                 05900000
059100        MOVE '903 LINK'          TO ERR-SENTENCE                  05910000
059200        PERFORM P9999-GOT-PROBLEM                                 05920000
059300     END-IF                                                       05930000
059400     MOVE PARM-RES-HRMN          TO WS-HOLD-TIME                  05940000
059500     COMPUTE TOTAL-TIME-HOURS = (PARM-RES-TOT-DAYS * 24)          05950000
059600     ADD WS-HOLD-TIME            TO TOTAL-TIME-DIFF               05960000
059700     MOVE TOTAL-TIME-DIFF        TO LENGTH-OF-DUTY.               05970000
059800*                                                                 05980000
059900 P2200-CREATE-POINTER.                                            05990000
060000*                                                                 06000000
060100     MOVE SPACES                   TO WS-POINTER                  06010000
060200     MOVE P943-EMP-NBR             TO POINT-EMP-NBR               06020000
060300*                                                                 06030000
060400*    CONVERT EFFECTIVE TIME TO SYSTEM TIME ZONE                   06040000
060500*                                                                 06050000
060600     MOVE SPACE                    TO WS-CNTL-FILE                06060000
060700     SET SUB-DIST-TYPE-REC         TO TRUE                        06070000
060710     IF P943-DIST      > SPACES                                   06071015
060720        AND P943-SDIST > SPACES                                   06072015
060800        MOVE P943-DIST             TO CNTL-DIST                   06080015
060900        MOVE P943-SDIST            TO CNTL-SUB-DIST               06090015
060901     ELSE                                                         06090115
060910        MOVE P943-EMP-NBR          TO MSTRNBRK                    06091015
060920        PERFORM P8500-READ-MASTER                                 06092015
060921        MOVE DIST      OF WS-MSTR  TO CNTL-DIST                   06092115
060922        MOVE SUB-DIST  OF WS-MSTR  TO CNTL-SUB-DIST               06092215
060930     END-IF                                                       06093015
061000     MOVE CNTLKEY-AREA             TO CNTLKEY                     06100000
061100     PERFORM P8100-READ-CNTL                                      06110000
061200     IF NOT SUCCESS                                               06120000
061300        MOVE 'P2200-1'             TO ERR-PARAGRAPH               06130000
061400        MOVE CNTLKEY               TO ERR-KEY                     06140000
061500        PERFORM P9999-GOT-PROBLEM                                 06150000
061600     END-IF                                                       06160000
061700     IF CNTL-TIME-ZONE = TZ-SYSTEM-TIME-ZONE                      06170000
061800        MOVE P943-EFF-DATE-TIME    TO POINT-EFF-DATE-TIME         06180000
061900     ELSE                                                         06190000
062000        MOVE CNTL-TIME-ZONE        TO TZ-IN-ZONE                  06200000
062100        MOVE P943-EFF-DATE-TIME    TO TZ-IN-DATE-TIME             06210000
062200        MOVE TZ-SYSTEM-TIME-ZONE   TO TZ-OUT-ZONE                 06220000
062300        PERFORM P8996-TIMEZONE                                    06230000
062400        MOVE TZ-OUT-DATE           TO POINT-EFF-DATE              06240000
062500        MOVE TZ-OUT-TIME           TO POINT-EFF-TIME              06250000
062600     END-IF                                                       06260000
062700                                                                  06270000
062800     SET DE-REFORMAT-ONLY          TO TRUE                        06280000
062900     SET DE-YYMMDD-FORMAT          TO TRUE                        06290000
063000     MOVE POINT-EFF-DATE           TO DE-YYMMDD                   06300000
063100     PERFORM P8998-DATEEDIT                                       06310000
063200     MOVE DE-YYMMDD-CE             TO POINT-EFF-CE                06320000
063300                                                                  06330000
063400     MOVE ZEROES                   TO POINT-SEQ                   06340000
063500     MOVE P943-EMP-FUNCTION        TO POINT-FUN                   06350000
063600     MOVE HIST-NBR-KEY             TO POINT-HIST-KEY              06360000
063700     IF POINT-ASGN-CHANGE-FUN                                     06370000
063800        IF  P943-CALL-REL-FUN                                     06380009
063900        AND NOT P943-FUN04-CAN-AT-HOME                            06390009
064000           CONTINUE                                               06400009
064100        ELSE                                                      06410000
064200           SET POINT-ASGN-CHANGE   TO TRUE                        06420000
064300           PERFORM P2220-ASGN-CHANGE                              06430000
064310           PERFORM P8300-WRITE-POINTER-RECORD                     06431009
064400        END-IF                                                    06440000
064500     END-IF                                                       06450000
064600     IF POINT-GUARANTEE-FUN                                       06460000
064700        SET POINT-GUARANTEE        TO TRUE                        06470000
064800        MOVE SPACES                TO POINT-VARIABLE-AREA         06480000
064900        IF POINT-REST-FUN                                         06490000
065000           PERFORM P2230-EXCESSIVE-REST                           06500000
065100        END-IF                                                    06510000
065200        PERFORM P8300-WRITE-POINTER-RECORD                        06520000
065300     END-IF                                                       06530000
065400     IF POINT-LOST-WORK-FUN                                       06540000
065500        SET POINT-LOST-WORK-REC    TO TRUE                        06550000
065600        MOVE SPACES                TO POINT-VARIABLE-AREA         06560000
065700        PERFORM P2240-LOST-WORK                                   06570000
065800        PERFORM P8300-WRITE-POINTER-RECORD                        06580000
065900     END-IF                                                       06590009
066000     .                                                            06600009
066010*                                                                 06601009
066100 P2220-ASGN-CHANGE.                                               06610000
066110*                                                                 06611009
066200     MOVE P943-EMP-NBR             TO MSTRNBRK                    06620000
066300     PERFORM P8500-READ-MASTER                                    06630000
066400     PERFORM P8510-GET-PERM-JOB                                   06640000
066500     IF ASGN-ASSIGNMENT > SPACES                                  06650000
066600        MOVE ASGN-JOB-TYPE         TO POINT-NORM-ASGN-TYPE        06660000
066700        MOVE ASGN-ASSIGNMENT       TO POINT-NORM-ASGN             06670000
066800        PERFORM P2400-GET-BLOCK-CODE                              06680000
066900        MOVE WS-BLOCK              TO POINT-NORM-BLOCK            06690000
067000     END-IF                                                       06700000
067100     PERFORM P8520-GET-LATEST-TEMP-JOB                            06710000
067200     IF ASGN-ASSIGNMENT > SPACES                                  06720000
067300        MOVE ASGN-JOB-TYPE         TO POINT-TEMP-ASGN-TYPE        06730000
067400        MOVE ASGN-ASSIGNMENT       TO POINT-TEMP-ASGN             06740000
067500        PERFORM P2400-GET-BLOCK-CODE                              06750000
067600        MOVE WS-BLOCK              TO POINT-TEMP-BLOCK            06760000
067700     END-IF                                                       06770000
067800     MOVE EMP-XR-ASGN-TYPE         TO POINT-XR-ASGN-TYPE          06780000
067900                                      ASGN-JOB-TYPE               06790000
068000     MOVE EMP-XR-ASSIGNMENT        TO POINT-XR-ASGN               06800000
068100                                      ASGN-ASSIGNMENT             06810000
068200     MOVE EMP-XR-FLAG              TO POINT-XR-FLAG               06820000
068300     IF EMP-XR-ASGN-TYPE > SPACES  AND                            06830000
068400        NOT EMP-XR-NO-JOB                                         06840000
068500        PERFORM P2400-GET-BLOCK-CODE                              06850000
068600        MOVE WS-BLOCK              TO POINT-XR-BLOCK              06860000
068700     END-IF                                                       06870000
068900     .                                                            06890009
068910*                                                                 06891009
069000 P2230-EXCESSIVE-REST.                                            06900000
069100*                                                                 06910000
069200     IF ((POINT-TIE-UP-FUN OR POINT-ADJ-TIE-UP-FUN)               06920003
069300     AND  HIST-FUN02-ADD-REST-HRMN > '0000')                      06930003
069400     OR  (POINT-CALL-REL-FUN                                      06940003
069500     AND  HIST-FUN04-ADD-REST-HRMN > '0000')                      06950003
069600     OR  (POINT-MARKUP-FUN                                        06960003
069700     AND  HIST-FUN12-ADD-REST      > '0000')                      06970003
069800     OR  (POINT-MARKUP-FUN                                        06980003
069900     AND  HIST-FUN37-ADD-REST      > '0000')                      06990003
070000     OR  (POINT-MASTER-RC-FUN                                     07000003
070100     AND (HIST-MSTR-RC-ADD-REST    > '0000' OR                    07010013
070110          HIST-MSTR-RC-ADD-REST-DYS > '000'))                     07011013
070200        MOVE P943-EMP-NBR          TO MSTRNBRK                    07020000
070300        PERFORM P8500-READ-MASTER                                 07030000
070400        MOVE EMP-PERS-REST         TO POINT-PERS-REST             07040000
070500     END-IF                                                       07050003
070501                                                                  07050103
070510     IF   POINT-TIE-UP-FUN                                        07051003
070520     AND  HIST-ADD-MAN-REST                                       07052004
070592        MOVE P943-EMP-NBR          TO MSTRNBRK                    07059203
070593        PERFORM P8500-READ-MASTER                                 07059303
070594        MOVE EMP-MTOD              TO POINT-MTOD-DATE-TIME        07059403
070595     END-IF.                                                      07059503
070600*                                                                 07060000
070700 P2240-LOST-WORK.                                                 07070000
070800*                                                                 07080000
072800     MOVE EMPTRK-ASGN-OWNED-TYPE        TO POINT-LW-ASGN-TYPE     07280010
072900     MOVE EMPTRK-ASGN-OWNED             TO POINT-LW-ASGN          07290010
073000     MOVE EMPTRK-LAYOFF-CODE            TO POINT-LW-LO-CODE       07300010
073100     MOVE EMPTRK-EM-CODE                TO POINT-LW-EM-CODE       07310010
073200     MOVE EMPTRK-TRACKING-REASON        TO POINT-LW-REASON        07320010
073300     MOVE EMPTRK-EFF-DATE-TIME          TO POINT-LW-INIT-DATE-TIME07330010
073400     SET  DE-REFORMAT-ONLY              TO TRUE                   07340010
073500     SET  DE-YYMMDD-FORMAT              TO TRUE                   07350010
073600     MOVE EMPTRK-EFF-DATE               TO DE-YYMMDD              07360010
073700     PERFORM P8998-DATEEDIT                                       07370000
073800     MOVE DE-YYMMDD-CE                  TO POINT-LW-INIT-CE       07380010
073900     MOVE P943-FUN39-LAST-FLAG          TO POINT-LW-LAST-FLAG     07390010
073910     .                                                            07391010
074000*                                                                 07400000
074100 P2400-GET-BLOCK-CODE.                                            07410000
074200*                                                                 07420000
074300     MOVE SPACES                   TO WORK-CNTLKEY                07430000
074400     MOVE '09'                     TO WK-CNTL-REC-TYPE            07440000
074500     MOVE ASGN-DIST                TO WK-CNTL-DIST                07450000
074600     MOVE ASGN-SUB-DIST            TO WK-CNTL-SUB-DIST            07460000
074700     IF ASGN-XB-JOB OF ASGN-JOB-TYPE                              07470000
074800        MOVE ASGN-XB-CC            TO WS-CRAFT-CODE-CHECK         07480000
074900                                      ASGN-XB-PREFIX              07490000
075000     ELSE                                                         07500000
075100        MOVE ASGN-AJ-JOB-CC        TO WS-CRAFT-CODE-CHECK         07510000
075200     END-IF                                                       07520000
075300     EVALUATE TRUE                                                07530000
075400        WHEN BRAKEMAN-CRAFT                                       07540000
075500           MOVE BRAKEMAN-MSTR-CC     TO WK-CNTL-CRAFT-XB          07550000
075600        WHEN SWITCHMAN-CRAFT                                      07560000
075700           MOVE SWITCHMAN-MSTR-CC    TO WK-CNTL-CRAFT-XB          07570000
075800        WHEN CLERICAL-CRAFT                                       07580000
075900           MOVE CLERICAL-MSTR-CC     TO WK-CNTL-CRAFT-XB          07590000
076000        WHEN OTHER                                                07600000
076100           MOVE ASGN-XB-CC           TO WK-CNTL-CRAFT-XB          07610000
076200     END-EVALUATE                                                 07620000
076300     IF ASGN-XB-JOB OF ASGN-JOB-TYPE OR ASGN-UFP-JOB              07630000
076400        MOVE ASGN-JOB-TYPE           TO WK-CNTL-ASGN-TYPE         07640000
076500     ELSE                                                         07650000
076600        MOVE ASGN-AJ-JOB-NO          TO JOB-DEF-CHECK             07660000
076700        IF JOB-DEF-YARD-ASGN                                      07670000
076800           MOVE 'Y'                  TO WK-CNTL-ASGN-TYPE         07680000
076900        ELSE                                                      07690000
077000           MOVE 'O'                  TO WK-CNTL-ASGN-TYPE         07700000
077100        END-IF                                                    07710000
077200     END-IF                                                       07720000
077300     MOVE ASGN-AJ-JOB-NO             TO WK-CNTL-ASGN              07730000
077400     PERFORM P7100-READ-SEN-CNTL                                  07740000
077500     IF NO-RECORD-FND                                             07750000
077600        MOVE '*'                     TO WK-CNTL-ASGN-5            07760000
077700                                        WK-CNTL-ASGN-6            07770000
077800        PERFORM P7100-READ-SEN-CNTL                               07780000
077900        IF NO-RECORD-FND                                          07790000
078000           IF ASGN-UFP-JOB                                        07800000
078100              MOVE '*'               TO WK-CNTL-ASGN-3            07810000
078200                                        WK-CNTL-ASGN-4            07820000
078300              PERFORM P7100-READ-SEN-CNTL                         07830000
078400              IF NO-RECORD-FND                                    07840000
078500                 MOVE '*'            TO WK-CNTL-ASGN-1            07850000
078600                                        WK-CNTL-ASGN-2            07860000
078700                 PERFORM P7100-READ-SEN-CNTL                      07870000
078800              END-IF                                              07880000
078900           ELSE                                                   07890000
079000              MOVE '*'               TO WK-CNTL-ASGN-1            07900000
079100                                        WK-CNTL-ASGN-2            07910000
079200                                        WK-CNTL-ASGN-3            07920000
079300                                        WK-CNTL-ASGN-4            07930000
079400              PERFORM P7100-READ-SEN-CNTL                         07940000
079500           END-IF                                                 07950000
079600           IF NO-RECORD-FND                                       07960000
079700              MOVE '*'               TO WK-CNTL-ASGN-TYPE         07970000
079800              PERFORM P7100-READ-SEN-CNTL                         07980000
079900           END-IF                                                 07990000
080000        END-IF                                                    08000000
080100     END-IF                                                       08010000
080200     IF NOT SUCCESS                                               08020000
080300        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     08030000
080400           MOVE 'P2400-1'            TO ERR-PARAGRAPH             08040000
080500           MOVE CNTLKEY              TO ERR-KEY                   08050000
080600           PERFORM P9999-GOT-PROBLEM                              08060000
080700        END-IF                                                    08070000
080800        MOVE SPACES                  TO WS-CNTL-FILE              08080000
080900        MOVE ZERO                    TO WS-BLOCK                  08090000
081000     ELSE                                                         08100000
081100        MOVE CNTL-SEN-BLOCK          TO WS-BLOCK                  08110000
081200     END-IF.                                                      08120000
081300*                                                                 08130000
081400 P3000-JOB-HIST-FUNCTION.                                         08140000
081500*                                                                 08150000
081600     MOVE SPACES                   TO WS-JOB-HIST                 08160000
081700     MOVE P943-JHIST-JOB-TYPE      TO JHIST-JOB-TYPE              08170000
081800                                      JHIST-JOB-TYPE2             08180000
081900     MOVE P943-JHIST-DIST          TO JHIST-JOB-DIST              08190000
082000                                      JHIST-JOB-DIST2             08200000
082100     MOVE P943-JHIST-SUB-DIST      TO JHIST-JOB-SUB-DIST          08210000
082200                                      JHIST-JOB-SUB-DIST2         08220000
082300     MOVE P943-JHIST-POOL          TO JHIST-JOB-POOL              08230000
082400                                      JHIST-JOB-POOL2             08240000
082500     MOVE P943-JHIST-TURN          TO JHIST-JOB-TURN              08250000
082600                                      JHIST-JOB-TURN2             08260000
082700     MOVE P943-JHIST-CRAFT         TO JHIST-JOB-CRAFT             08270000
082800                                      JHIST-JOB-CRAFT2            08280000
082900     EXEC CICS ASSIGN                                             08290000
083000               USERID(JHIST-USERID)                               08300000
083100     END-EXEC                                                     08310000
083200     MOVE P943-JHIST-FUNCTION      TO JHIST-FUNCTION              08320000
083300     MOVE P943-JHIST-KEY-DATA      TO JHIST-AHMISC-KEY            08330000
083400     MOVE P943-JHIST-KEY-TYPE      TO JHIST-KEY-TYPE              08340000
083500     MOVE P943-JHIST-P-W-XB-FLAG   TO JHIST-PROT-WORK-XB-FLAG     08350000
083600     MOVE P943-JHIST-ORIG-TURN-NBR TO JHIST-ORIG-TURN-NBR         08360000
083700     MOVE WS-JOB-HIST              TO WS-SAVE-JOB-HIST            08370000
083800                                                                  08380000
083900     IF P943-NO-DUPS                                              08390000
084000        MOVE P943-JHIST-KEY-DATA   TO WS-SAVE-AHM-KEY             08400000
084100        PERFORM P3010-WRITE-JHIST-NO-DUPS                         08410000
084200     ELSE                                                         08420000
084300        PERFORM P3020-RESOLVE-JHIST-DUPS                          08430000
084400     END-IF.                                                      08440000
084500*                                                                 08450000
084600 P3010-WRITE-JHIST-NO-DUPS.                                       08460000
084700*                                                                 08470000
084800     MOVE SPACES                   TO TZ-PARAMETERS               08480000
084900     MOVE P943-JHIST-TIME-ZONE     TO TZ-IN-ZONE                  08490000
085000     MOVE P943-JHIST-CLOCK-TIME    TO TZ-IN-DATE-TIME             08500000
085100     MOVE TZ-SYSTEM-TIME-ZONE      TO TZ-OUT-ZONE                 08510000
085200     PERFORM P8996-TIMEZONE                                       08520000
085300     MOVE ZEROS                    TO WS-HIST-CLOCK-TIME          08530000
085400     MOVE TZ-OUT-CE                TO WS-HIST-CE                  08540000
085500     MOVE TZ-OUT-DATE              TO WS-HIST-DATE                08550000
085600     MOVE TZ-OUT-TIME              TO WS-HIST-TIME                08560000
085700                                                                  08570000
085800     MOVE '0'                      TO DONE-FLAG                   08580000
085900     PERFORM UNTIL DONE                                           08590000
086000        MOVE WS-HIST-CLOCK-TIME      TO JHIST-CLOCK-TIME          08600000
086100                                        JHIST-CLOCK-KEY-TIME-CENT 08610000
086200        MOVE JHIST-JOB-KEY           TO JHISTJOB                  08620000
086300        PERFORM P8200-READ-JOB-HIST                               08630000
086400        IF SUCCESS                                                08640000
086500           MOVE JHIST-AHMISC-KEY   TO WS-COMP-AHM-KEY             08650000
086600           IF WS-SAVE-AHM-TRAIN = WS-COMP-AHM-TRAIN               08660000
086700              SET DONE             TO TRUE                        08670000
086800           ELSE                                                   08680000
086900              ADD 1                TO WS-HIST-DATE-TIME-TIE       08690000
087000              IF WS-HIST-DATE-TIME-TIE > 90                       08700000
087100                 SET DONE          TO TRUE                        08710000
087200                 MOVE 'P3010-1'    TO ERR-PARAGRAPH               08720000
087300                 MOVE JHISTJOB     TO ERR-KEY                     08730000
087400                 PERFORM P9999-GOT-PROBLEM                        08740000
087500              END-IF                                              08750000
087600           END-IF                                                 08760000
087700        ELSE                                                      08770000
087800           IF NO-RECORD-FND                                       08780000
087900              SET DONE             TO TRUE                        08790000
088000              MOVE WS-SAVE-JOB-HIST                               08800000
088100                                   TO WS-JOB-HIST                 08810000
088200              MOVE WS-HIST-CLOCK-TIME                             08820000
088300                                   TO JHIST-CLOCK-TIME            08830000
088400                                      JHIST-CLOCK-KEY-TIME-CENT   08840000
088500              PERFORM P8210-WRITE-JOB-HIST                        08850000
088600              IF NOT SUCCESS                                      08860000
088700                 MOVE 'P3010-2'    TO ERR-PARAGRAPH               08870000
088800                 MOVE JHISTJOB     TO ERR-KEY                     08880000
088900                 PERFORM P9999-GOT-PROBLEM                        08890000
089000              END-IF                                              08900000
089100           ELSE                                                   08910000
089200              MOVE 'P3010-3'       TO ERR-PARAGRAPH               08920000
089300              MOVE JHISTJOB        TO ERR-KEY                     08930000
089400              PERFORM P9999-GOT-PROBLEM                           08940000
089500           END-IF                                                 08950000
089600        END-IF                                                    08960000
089700     END-PERFORM.                                                 08970000
089800*                                                                 08980000
089900 P3020-RESOLVE-JHIST-DUPS.                                        08990000
090000*                                                                 09000000
090100     MOVE SPACES                   TO TZ-PARAMETERS               09010000
090200     IF P943-EMPLOYEE-FUNCTION                                    09020000
090300        MOVE ZEROS                 TO WS-HIST-DATE-TIME-TIE       09030000
090400        MOVE WS-JOB-HIST           TO WS-SAVE-JOB-HIST            09040000
090500     ELSE                                                         09050000
090600        MOVE P943-JHIST-TIME-ZONE  TO TZ-IN-ZONE                  09060000
090700        MOVE P943-JHIST-CLOCK-TIME TO TZ-IN-DATE-TIME             09070000
090800        MOVE TZ-SYSTEM-TIME-ZONE   TO TZ-OUT-ZONE                 09080000
090900        PERFORM P8996-TIMEZONE                                    09090000
091000        MOVE ZEROS                 TO WS-HIST-CLOCK-TIME          09100000
091100        MOVE TZ-OUT-CE             TO WS-HIST-CE                  09110000
091200        MOVE TZ-OUT-DATE           TO WS-HIST-DATE                09120000
091300        MOVE TZ-OUT-TIME           TO WS-HIST-TIME                09130000
091400     END-IF                                                       09140000
091500                                                                  09150000
091600     MOVE '0'                      TO DONE-FLAG                   09160000
091700     PERFORM UNTIL DONE                                           09170000
091800        MOVE WS-HIST-CLOCK-TIME    TO JHIST-CLOCK-TIME            09180000
091900                                      JHIST-CLOCK-KEY-TIME-CENT   09190000
092000        MOVE JHIST-JOB-KEY         TO JHISTJOB                    09200000
092100        PERFORM P8200-READ-JOB-HIST                               09210000
092200        IF SUCCESS                                                09220000
092300           ADD 1                   TO WS-HIST-DATE-TIME-TIE       09230000
092400           IF WS-HIST-DATE-TIME-TIE > 90                          09240000
092500              SET DONE             TO TRUE                        09250000
092600              MOVE 'P3020-1'       TO ERR-PARAGRAPH               09260000
092700              MOVE JHISTJOB        TO ERR-KEY                     09270000
092800              PERFORM P9999-GOT-PROBLEM                           09280000
092900           END-IF                                                 09290000
093000        ELSE                                                      09300000
093100           IF NO-RECORD-FND                                       09310000
093200              SET DONE             TO TRUE                        09320000
093300              MOVE WS-SAVE-JOB-HIST                               09330000
093400                                   TO WS-JOB-HIST                 09340000
093500              MOVE WS-HIST-CLOCK-TIME                             09350000
093600                                   TO JHIST-CLOCK-TIME            09360000
093700                                      JHIST-CLOCK-KEY-TIME-CENT   09370000
093800              PERFORM P8210-WRITE-JOB-HIST                        09380000
093900              IF NOT SUCCESS                                      09390000
094000                 MOVE 'P3020-2'    TO ERR-PARAGRAPH               09400000
094100                 MOVE JHISTJOB     TO ERR-KEY                     09410000
094200                 PERFORM P9999-GOT-PROBLEM                        09420000
094300              END-IF                                              09430000
094400           ELSE                                                   09440000
094500              MOVE 'P3020-3'       TO ERR-PARAGRAPH               09450000
094600              MOVE JHISTJOB        TO ERR-KEY                     09460000
094700              PERFORM P9999-GOT-PROBLEM                           09470000
094800           END-IF                                                 09480000
094900        END-IF                                                    09490000
095000     END-PERFORM.                                                 09500000
095100*                                                                 09510000
095200 P4000-UPDATE-LOST-WORK.                                          09520000
095300*                                                                 09530000
095400     MOVE P943-LOST-WORK-KEY        TO HISTNBR                    09540000
095500     PERFORM VARYING HIST-SUB FROM 1 BY 1 UNTIL HIST-SUB > 3      09550000
095600        MOVE HISTORY-VIA-EMP-NBR(HIST-SUB)                        09560000
095700                                    TO HIST-DATASET               09570000
095800        EXEC CICS READ                                            09580000
095900                  UPDATE                                          09590000
096000                  DATASET(HIST-DATASET)                           09600000
096100                  INTO(WS-HIST)                                   09610000
096200                  LENGTH(HISTNBR-RLGTH)                           09620000
096300                  RIDFLD(HISTNBR)                                 09630000
096400                  KEYLENGTH(HISTNBR-KLGTH)                        09640000
096500                  RESP(WS-RESPONSE)                               09650000
096600        END-EXEC                                                  09660000
096700        MOVE WS-RESPONSE            TO FILE-STATUS                09670000
096800        IF SUCCESS                                                09680000
097000           IF P943-LOST-WORK-TS-NO       > SPACES                 09700012
097001              MOVE P943-LOST-WORK-TS-NO TO HIST-FUN39-TIMESLIP-NO 09700112
097002           END-IF                                                 09700212
097010           IF P943-FUN39-LW-REASON       > SPACES                 09701012
097020              MOVE P943-FUN39-LW-REASON TO HIST-FUN39-LW-REASON   09702012
097030           END-IF                                                 09703012
097100           EXEC CICS REWRITE                                      09710000
097200                     DATASET(HIST-DATASET)                        09720000
097300                     FROM(WS-HIST)                                09730000
097400                     LENGTH(HISTNBR-RLGTH)                        09740000
097500                     RESP(WS-RESPONSE)                            09750000
097600           END-EXEC                                               09760000
097700           MOVE WS-RESPONSE         TO FILE-STATUS                09770000
097800           IF NOT SUCCESS                                         09780000
097900              MOVE 'P4000-1'        TO ERR-PARAGRAPH              09790000
098000              MOVE HISTNBR          TO ERR-KEY                    09800000
098100              PERFORM P9999-GOT-PROBLEM                           09810000
098200           END-IF                                                 09820000
098300           MOVE 4                   TO HIST-SUB                   09830000
098400        ELSE                                                      09840000
098500           IF HIST-SUB = 3                                        09850000
098600              MOVE 'P4000-2'        TO ERR-PARAGRAPH              09860000
098700              MOVE HISTNBR          TO ERR-KEY                    09870000
098800              PERFORM P9999-GOT-PROBLEM                           09880000
098900           END-IF                                                 09890000
099000        END-IF                                                    09900000
099100     END-PERFORM.                                                 09910000
099200*                                                                 09920000
099210 P4500-UPDATE-LOST-WORK2.                                         09921006
099220*                                                                 09922006
099221* CHECK IF THE SECOND LOST WORK RECORD ALREADY EXISTS.  IF SO,    09922106
099222* UPDATE IT WITH THE HELD AWAY TS NUMBER.                         09922206
099223*                                                                 09922306
099225     SET  DONT-WRITE-LW2                TO TRUE                   09922507
099226     MOVE P943-LOST-WORK-KEY            TO HIST-NBR-KEY           09922606
099227     ADD  1                             TO HIST-C-SCML-NUM        09922706
099228     MOVE HIST-NBR-KEY                  TO HISTNBR                09922806
099229     PERFORM VARYING HIST-SUB FROM 1 BY 1                         09922906
099230        UNTIL HIST-SUB > 3                                        09923006
099231        MOVE HISTORY-VIA-EMP-NBR(HIST-SUB) TO HIST-DATASET        09923106
099232        EXEC CICS READ                                            09923206
099233                  UPDATE                                          09923306
099234                  DATASET  (HIST-DATASET)                         09923406
099235                  INTO     (WS-HIST)                              09923506
099236                  LENGTH   (HISTNBR-RLGTH)                        09923606
099237                  RIDFLD   (HISTNBR)                              09923706
099238                  KEYLENGTH(HISTNBR-KLGTH)                        09923806
099239                  RESP     (WS-RESPONSE)                          09923906
099240        END-EXEC                                                  09924006
099241        MOVE WS-RESPONSE                TO FILE-STATUS            09924106
099242        IF SUCCESS                                                09924206
099243           MOVE P943-LOST-WORK-TS-NO  TO HIST-FUN39-HA-TIMESLIP-NO09924306
099244           EXEC CICS REWRITE                                      09924406
099245                     DATASET(HIST-DATASET)                        09924506
099246                     FROM   (WS-HIST)                             09924606
099247                     LENGTH (HISTNBR-RLGTH)                       09924706
099248                     RESP   (WS-RESPONSE)                         09924806
099249           END-EXEC                                               09924906
099250           MOVE WS-RESPONSE             TO FILE-STATUS            09925006
099251           IF NOT SUCCESS                                         09925106
099252              MOVE 'P4500-1'            TO ERR-PARAGRAPH          09925206
099253              MOVE HISTNBR              TO ERR-KEY                09925306
099254              PERFORM P9999-GOT-PROBLEM                           09925406
099255           END-IF                                                 09925506
099256           MOVE 4                       TO HIST-SUB               09925606
099257        ELSE                                                      09925706
099258           IF HIST-SUB                   = 3                      09925806
099260              SET  WRITE-LW2            TO TRUE                   09926006
099261           END-IF                                                 09926106
099312        END-IF                                                    09931206
099313     END-PERFORM                                                  09931306
099315*                                                                 09931506
099316* THE SECOND LOST WORK RECORD DOES NOT EXIST.  READ THE FIRST LOST09931606
099317* WORK RECORD AND ADD 1 TO THE SCML.  BUILD THE SECOND LOST WORK  09931706
099318* RECORD AND WRITE IT TO THE SAME HISTORY FILE.                   09931806
099319*                                                                 09931906
099320     IF WRITE-LW2                                                 09932006
099321        MOVE P943-LOST-WORK-KEY         TO HISTNBR                09932106
099322        PERFORM VARYING HIST-SUB FROM 1 BY 1                      09932206
099323           UNTIL HIST-SUB > 3                                     09932306
099324           MOVE HISTORY-VIA-EMP-NBR(HIST-SUB)                     09932406
099325                                        TO HIST-DATASET           09932506
099326           EXEC CICS READ                                         09932606
099328                     DATASET(HIST-DATASET)                        09932806
099329                     INTO(WS-HIST)                                09932906
099330                     LENGTH(HISTNBR-RLGTH)                        09933006
099331                     RIDFLD(HISTNBR)                              09933106
099332                     KEYLENGTH(HISTNBR-KLGTH)                     09933206
099333                     RESP(WS-RESPONSE)                            09933306
099334           END-EXEC                                               09933406
099335           MOVE WS-RESPONSE             TO FILE-STATUS            09933506
099336           IF SUCCESS                                             09933606
099337              ADD  1                    TO HIST-C-SCML-NUM        09933706
099338              MOVE HIST-NBR-KEY         TO HISTNBR                09933806
099339              SET  LOST-WORK2-FUN       TO TRUE                   09933906
099340              MOVE SPACES               TO HIST-FUN39-AREA        09934006
099341              MOVE P943-LOST-WORK-TS-NO                           09934106
099342                                      TO HIST-FUN39-HA-TIMESLIP-NO09934206
099343              EXEC CICS WRITE                                     09934306
099344                        DATASET(HIST-DATASET)                     09934406
099345                        FROM   (WS-HIST)                          09934506
099346                        LENGTH (HISTNBR-RLGTH)                    09934606
099347                        RIDFLD (HISTNBR)                          09934706
099348                        RESP   (WS-RESPONSE)                      09934806
099349              END-EXEC                                            09934906
099350              MOVE WS-RESPONSE          TO FILE-STATUS            09935006
099351              IF NOT SUCCESS                                      09935106
099352                 MOVE 'P4500-2'         TO ERR-PARAGRAPH          09935206
099353                 MOVE HISTNBR           TO ERR-KEY                09935306
099354                 PERFORM P9999-GOT-PROBLEM                        09935406
099355              END-IF                                              09935506
099356              MOVE 4                    TO HIST-SUB               09935606
099357           ELSE                                                   09935706
099358              IF HIST-SUB                = 3                      09935806
099359                 MOVE 'P4500-3'         TO ERR-PARAGRAPH          09935906
099360                 MOVE HISTNBR           TO ERR-KEY                09936006
099361                 PERFORM P9999-GOT-PROBLEM                        09936106
099363              END-IF                                              09936306
099364           END-IF                                                 09936406
099365        END-PERFORM                                               09936506
099366     END-IF                                                       09936606
099367     .                                                            09936706
099368*                                                                 09936806
099370 P5000-EMP-PHONELOG-FUNCTION.                                     09937000
099400*                                                                 09940000
099500     IF P943-PLOG-EMP-NBR  NOT > SPACES                           09950000
099600        MOVE 'P5000-1'             TO ERR-PARAGRAPH               09960000
099700        MOVE 'NO PHONLOG EMPNBR'   TO ERR-KEY                     09970000
099800        PERFORM P9999-GOT-PROBLEM                                 09980000
099900     END-IF                                                       09990000
100000                                                                  10000000
100100     MOVE SPACES                   TO WS-PHONE-LOG                10010000
100200                                                                  10020000
100300     EXEC CICS ASKTIME                                            10030000
100400               ABSTIME(WS-ABSTIME)                                10040000
100500     END-EXEC                                                     10050000
100600     ADD WS-ABSTIME-OFFSET         TO WS-ABSTIME                  10060000
100700     EXEC CICS FORMATTIME                                         10070000
100800               ABSTIME(WS-ABSTIME)                                10080000
100900               YYYYMMDD(WS-SYSTEM-DATE-CENT)                      10090000
101000               TIME(WS-SYSTEM-TIME-AREA)                          10100000
101100     END-EXEC                                                     10110000
101200     MOVE WS-SYSTEM-DATE-TIME      TO PLOG-CLOCK-TIME             10120000
101300     MOVE ZEROS                    TO PLOG-C-ML                   10130000
101400     MOVE P943-PLOG-EMP-NBR        TO PLOG-EMP-NBR                10140000
101500     MOVE P943-PLOG-PHONE-NUMBER   TO PLOG-PHONE-NUMBER           10150000
101600     MOVE P943-PLOG-TRAIN-ASGN     TO PLOG-TRAIN-ASGN             10160000
101700     MOVE P943-PLOG-ASGN-CC        TO PLOG-ASGN-CC                10170000
101800     MOVE P943-PLOG-FUNCTION       TO PLOG-FUNCTION               10180000
101900     MOVE P943-PLOG-TYPE           TO PLOG-TYPE                   10190000
102000                                                                  10200000
102100     EXEC CICS ASSIGN                                             10210000
102200               USERID(PLOG-USERID)                                10220000
102300     END-EXEC                                                     10230000
102400     MOVE '01'                     TO P943-RETURN-STATUS          10240000
102500     SET NO-RECORD-FND             TO TRUE                        10250000
102600     PERFORM UNTIL SUCCESS                                        10260000
102700             OR PLOG-C-ML > WS-WRITE-MAX                          10270000
102800        MOVE PLOG-NBR-KEY          TO PLOGNBR                     10280000
102900        PERFORM P8600-WRITE-PHONELOG                              10290000
103000        MOVE FILE-STATUS           TO P943-RETURN-STATUS          10300000
103100        ADD 1                      TO PLOG-C-ML                   10310000
103200        IF NOT SUCCESS                                            10320000
103300           IF NOT DUP-KEY-ERR                                     10330000
103400              MOVE 'P5000-2'       TO ERR-PARAGRAPH               10340000
103500              MOVE PLOGNBR         TO ERR-KEY                     10350000
103600              PERFORM P9999-GOT-PROBLEM                           10360000
103700           END-IF                                                 10370000
103800        END-IF                                                    10380000
103900     END-PERFORM                                                  10390000
104000     IF PLOG-C-ML > WS-WRITE-MAX                                  10400000
104100        MOVE 'P5000-3'       TO ERR-PARAGRAPH                     10410000
104200        MOVE PLOGNBR         TO ERR-KEY                           10420000
104300        PERFORM P9999-GOT-PROBLEM                                 10430000
104400     END-IF.                                                      10440000
104500*                                                                 10450000
104600 P7000-READ-AH-KEY-3.                                             10460000
104700*                                                                 10470000
104800     MOVE AH-KEY-3                  TO AH3KEY                     10480000
104900     PERFORM VARYING AH-SUB FROM 1 BY 1 UNTIL AH-SUB > 3          10490000
105000        MOVE AHIST-VIA-K3(AH-SUB)   TO AH-DATASET                 10500000
105100        EXEC CICS READ                                            10510000
105200                  DATASET(AH-DATASET)                             10520000
105300                  INTO(WS-AHIST)                                  10530000
105400                  LENGTH(AH3-RLGTH)                               10540000
105500                  RIDFLD(AH3KEY)                                  10550000
105600                  KEYLENGTH(AH3-KLGTH)                            10560000
105700                  RESP(WS-RESPONSE)                               10570000
105800        END-EXEC                                                  10580000
105900        MOVE WS-RESPONSE            TO FILE-STATUS                10590000
106000        IF SUCCESS                                                10600000
106100           MOVE 4                   TO AH-SUB                     10610000
106200        ELSE                                                      10620000
106300           MOVE SPACES              TO WS-AHIST                   10630000
106400           IF NOT (SUCCESS OR NO-RECORD-FND)                      10640000
106500              MOVE 'P7000-1'        TO ERR-PARAGRAPH              10650000
106600              MOVE AH3KEY           TO ERR-KEY                    10660000
106700              PERFORM P9999-GOT-PROBLEM                           10670000
106800           END-IF                                                 10680000
106900        END-IF                                                    10690000
107000     END-PERFORM.                                                 10700000
107100*                                                                 10710000
107200 P7100-READ-SEN-CNTL.                                             10720000
107300*                                                                 10730000
107400     MOVE WORK-CNTLKEY               TO CNTLKEY                   10740000
107500     PERFORM P8100-READ-CNTL                                      10750000
107600     IF SUCCESS                                                   10760000
107700        IF CNTL-SEN-BLOCK NOT > SPACES                            10770000
107800           SET NO-RECORD-FND         TO TRUE                      10780000
107900        END-IF                                                    10790000
108000     END-IF.                                                      10800000
108100*                                                                 10810000
108200 P8000-WRITE-HISTORY.                                             10820000
108300*                                                                 10830000
108400     EXEC CICS WRITE                                              10840000
108500               DATASET(HIST-VIA-EMP-NBR)                          10850000
108600               FROM(WS-HIST)                                      10860000
108700               LENGTH(HISTNBR-RLGTH)                              10870000
108800               RIDFLD(HISTNBR)                                    10880000
108900               RESP(WS-RESPONSE)                                  10890000
109000     END-EXEC                                                     10900000
109100     MOVE WS-RESPONSE            TO FILE-STATUS.                  10910000
109200*                                                                 10920000
109300 P8100-READ-CNTL.                                                 10930000
109400*                                                                 10940000
109500     EXEC CICS READ                                               10950000
109600               DATASET(CNTL-FILE-VIA-CNTLKEY)                     10960000
109700               INTO(WS-CNTL-FILE)                                 10970000
109800               LENGTH(CNTLFILE-RLGTH)                             10980000
109900               RIDFLD(CNTLKEY)                                    10990000
110000               KEYLENGTH(CNTLFILE-KLGTH)                          11000000
110100               RESP(WS-RESPONSE)                                  11010000
110200     END-EXEC                                                     11020000
110300     MOVE WS-RESPONSE TO FILE-STATUS.                             11030000
110400*                                                                 11040000
110500 P8200-READ-JOB-HIST.                                             11050000
110600*                                                                 11060000
110700     EXEC CICS READ                                               11070000
110800               DATASET(JHIST-VIA-JOB)                             11080000
110900               INTO(WS-JOB-HIST)                                  11090000
111000               LENGTH(JHISTJOB-RLGTH)                             11100000
111100               RIDFLD(JHISTJOB)                                   11110000
111200               KEYLENGTH(JHISTJOB-KLGTH)                          11120000
111300               RESP(WS-RESPONSE)                                  11130000
111400     END-EXEC                                                     11140000
111500     MOVE WS-RESPONSE TO FILE-STATUS.                             11150000
111600*                                                                 11160000
111700 P8210-WRITE-JOB-HIST.                                            11170000
111800*                                                                 11180000
111900     EXEC CICS WRITE                                              11190000
112000               DATASET(JHIST-VIA-JOB)                             11200000
112100               FROM(WS-JOB-HIST)                                  11210000
112200               LENGTH(JHISTJOB-RLGTH)                             11220000
112300               RIDFLD(JHISTJOB)                                   11230000
112400               RESP(WS-RESPONSE)                                  11240000
112500     END-EXEC                                                     11250000
112600     MOVE WS-RESPONSE            TO FILE-STATUS.                  11260000
112700*                                                                 11270000
112800 P8300-WRITE-POINTER-RECORD.                                      11280000
112900     SET NO-RECORD-FND             TO TRUE                        11290000
113000     MOVE ZEROS                    TO WS-HIST-DATE-TIME-TIE       11300000
113100     PERFORM UNTIL SUCCESS                                        11310000
113200             OR WS-HIST-DATE-TIME-TIE > WS-WRITE-MAX              11320000
113300        MOVE WS-HIST-DATE-TIME-TIE TO POINT-SEQ                   11330000
113400        MOVE POINT-NBR-KEY         TO POINTKEY                    11340000
113500        EXEC CICS WRITE                                           11350000
113600                  DATASET(POINT-FILE-VIA-EMP)                     11360000
113700                  FROM(WS-POINTER)                                11370000
113800                  LENGTH(POINT-RLGTH)                             11380000
113900                  RIDFLD(POINTKEY)                                11390000
114000                  RESP(WS-RESPONSE)                               11400000
114100        END-EXEC                                                  11410000
114200        MOVE WS-RESPONSE           TO FILE-STATUS                 11420000
114300        ADD 1                      TO WS-HIST-DATE-TIME-TIE       11430000
114400        IF NOT SUCCESS                                            11440000
114500           IF NOT DUP-KEY-ERR                                     11450000
114600              MOVE 'P8300-1'       TO ERR-PARAGRAPH               11460000
114700              MOVE POINTKEY        TO ERR-KEY                     11470000
114800              PERFORM P9999-GOT-PROBLEM                           11480000
114900           END-IF                                                 11490000
115000        END-IF                                                    11500000
115100     END-PERFORM.                                                 11510000
115200                                                                  11520000
115300 P8500-READ-MASTER.                                               11530000
115400     EXEC CICS READ                                               11540000
115500               DATASET(MSTR-VIA-EMP-NBR)                          11550000
115600               INTO(WS-MSTR)                                      11560000
115700               LENGTH(MSTRENBR-RLGTH)                             11570000
115800               RIDFLD(MSTRNBRK)                                   11580000
115900               KEYLENGTH(MSTRENBR-KLGTH)                          11590000
116000               RESP(WS-RESPONSE)                                  11600000
116100     END-EXEC                                                     11610000
116200     MOVE WS-RESPONSE           TO FILE-STATUS                    11620000
116300     IF NOT SUCCESS                                               11630000
116400        MOVE 'P8500'            TO ERR-PARAGRAPH                  11640000
116500        MOVE MSTRNBRK           TO ERR-KEY                        11650000
116600        PERFORM P9999-GOT-PROBLEM                                 11660000
116700     END-IF.                                                      11670000
116800*                                                                 11680000
116900 P8510-GET-PERM-JOB.                                              11690000
117000*                                                                 11700000
117100     MOVE SPACES                     TO WS-ASSIGN-COMMAREA        11710000
117200                                        WS-ASGN-FILE              11720000
117300     MOVE P943-EMP-NBR               TO ASGN-EMP-NO               11730000
117400     SET GET-JOB-EMPLOYEE-OWNS       TO TRUE                      11740000
117500     PERFORM P8550-LINK-TO-901.                                   11750000
117600*                                                                 11760000
117700 P8520-GET-LATEST-TEMP-JOB.                                       11770000
117800*                                                                 11780000
117900     MOVE SPACES                     TO WS-ASSIGN-COMMAREA        11790000
118000                                        WS-ASGN-FILE              11800000
118100     MOVE P943-EMP-NBR               TO ASGN-EMP-NO               11810000
118200     SET GET-EMPS-LATEST-TEMP-JOB    TO TRUE                      11820000
118300     PERFORM P8550-LINK-TO-901.                                   11830000
118400*                                                                 11840000
118500 P8550-LINK-TO-901.                                               11850000
118600     MOVE WS-ASGN-FILE               TO WS-ASSIGN-RECORD          11860000
118700     EXEC CICS LINK                                               11870000
118800               PROGRAM(P901-PGM)                                  11880000
118900               COMMAREA(WS-ASSIGN-COMMAREA)                       11890000
119000               LENGTH(P901-LGTH)                                  11900000
119100               RESP(WS-RESPONSE)                                  11910000
119200     END-EXEC                                                     11920000
119300     MOVE WS-RESPONSE                TO FILE-STATUS               11930000
119400     IF NOT SUCCESS                                               11940000
119500        MOVE 'P8550-1'               TO ERR-PARAGRAPH             11950000
119600        MOVE 'P901LINK'              TO ERR-KEY                   11960000
119700        MOVE 'LINK TO 901 FAILED'    TO ERR-SENTENCE              11970000
119800        PERFORM P9999-GOT-PROBLEM                                 11980000
119900     END-IF                                                       11990000
120000     MOVE WS-ASSIGN-RECORD           TO WS-ASGN-FILE.             12000000
120100                                                                  12010000
120200*                                                                 12020000
120300 P8600-WRITE-PHONELOG.                                            12030000
120400*                                                                 12040000
120500*    WRITE RECORD TO PHONE LOG FILE                               12050000
120600*                                                                 12060000
120700     EXEC CICS WRITE                                              12070000
120800               DATASET(PLOG-VIA-EMP-NBR)                          12080000
120900               FROM(WS-PHONE-LOG)                                 12090000
121000               LENGTH(PLOGNBR-RLGTH)                              12100000
121100               RIDFLD(PLOGNBR)                                    12110000
121200               RESP(WS-RESPONSE)                                  12120000
121300     END-EXEC                                                     12130000
121400     MOVE WS-RESPONSE TO FILE-STATUS.                             12140000
121500*                                                                 12150000
121600     COPY TIMEZONE.                                               12160000
121700     COPY DATEEDIT.                                               12170000
121800*                                                                 12180000
121900 P8700-GET-AHMISC-RECORD.                                         12190000
122000*                                                                 12200000
122100     MOVE AHM-KEY                TO AHMKEY                        12210000
122200     EXEC CICS STARTBR                                            12220000
122300               DATASET(AHM-FILE)                                  12230000
122400               RIDFLD(AHMKEY)                                     12240000
122500               GTEQ                                               12250000
122600               RESP(WS-RESPONSE)                                  12260000
122700     END-EXEC                                                     12270000
122800     MOVE WS-RESPONSE            TO FILE-STATUS                   12280000
122900     IF SUCCESS                                                   12290000
123000        MOVE SPACE               TO DONE-FLAG                     12300000
123100        PERFORM UNTIL DONE                                        12310000
123200           EXEC CICS READNEXT                                     12320000
123300                     DATASET(AHM-FILE)                            12330000
123400                     INTO(WS-AHIST-MISC)                          12340000
123500                     LENGTH(AHM-RLGTH)                            12350000
123600                     RIDFLD(AHMKEY)                               12360000
123700                     KEYLENGTH(AHM-KLGTH)                         12370000
123800                     RESP(WS-RESPONSE)                            12380000
123900           END-EXEC                                               12390000
124000           MOVE WS-RESPONSE      TO FILE-STATUS                   12400000
124100           IF SUCCESS                                             12410000
124200              IF AHM-AHIST-KEY = WS-XREF-KEY                      12420000
124300              AND AHM-EMPLOYEE-RECORD                             12430000
124400              AND AHM-EMPLOYEE-NUMBER = P943-EMP-NBR              12440000
124500              AND AHM-ODA-CC          = P943-TEMP-ASGN-CC         12450000
124600                 SET DONE        TO TRUE                          12460000
124700                 SET FOUND-IT    TO TRUE                          12470000
124800              ELSE                                                12480000
124900                 IF AHM-AHIST-KEY > WS-XREF-KEY                   12490000
125000                    MOVE SPACES  TO WS-JOB-HIST                   12500000
125100                    SET DONE     TO TRUE                          12510000
125200                 END-IF                                           12520000
125300              END-IF                                              12530000
125400           ELSE                                                   12540000
125500              SET DONE           TO TRUE                          12550000
125600              IF NOT (NO-RECORD-FND OR END-OF-FILE)               12560000
125700                 MOVE 'P8700-1'  TO ERR-PARAGRAPH                 12570000
125800                 MOVE AHMKEY     TO ERR-KEY                       12580000
125900                 PERFORM P9999-GOT-PROBLEM                        12590000
126000              END-IF                                              12600000
126100           END-IF                                                 12610000
126200        END-PERFORM                                               12620000
126300        EXEC CICS ENDBR                                           12630000
126400                  DATASET(AHM-FILE)                               12640000
126500                  RESP(WS-RESPONSE)                               12650000
126600        END-EXEC                                                  12660000
126700     ELSE                                                         12670000
126800        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     12680000
126900           MOVE 'P8700-2'  TO ERR-PARAGRAPH                       12690000
127000           MOVE AHMKEY     TO ERR-KEY                             12700000
127100           PERFORM P9999-GOT-PROBLEM                              12710000
127200        END-IF                                                    12720000
127300     END-IF.                                                      12730000
127400*                                                                 12740000
127500 P9000-RETURN.                                                    12750000
127600*                                                                 12760000
127700     MOVE P943-COMMAREA-PARMS    TO DFHCOMMAREA                   12770000
127800     EXEC CICS RETURN END-EXEC.                                   12780000
127900*                                                                 12790000
128000 P9999-GOT-PROBLEM.                                               12800000
128100*                                                                 12810000
128200     MOVE P943-PGM               TO ERR-PROGRAM                   12820000
128300     MOVE DFHEIBLK               TO ERR-EIBLK                     12830000
128400     EXEC CICS XCTL                                               12840000
128500               PROGRAM(PSTERR-PGM)                                12850000
128600               COMMAREA(PSTERAR-AREA)                             12860000
128700               LENGTH(PSTERAR-LGTH)                               12870000
128800               RESP(WS-RESPONSE)                                  12880000
128900     END-EXEC                                                     12890000
129000     EXEC CICS ABEND                                              12900000
129100               ABCODE(PSTERR-ABCODE)                              12910000
129200               CANCEL                                             12920000
129300     END-EXEC.                                                    12930000
129400*                                                                 12940000
129500 X9999-GOBACK.                                                    12950000
129600     GOBACK.                                                      12960000
