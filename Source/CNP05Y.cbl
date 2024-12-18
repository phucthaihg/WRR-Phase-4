000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.  CNP05Y.
000300******************************************************************
000400* EMPLOYEE TIME ON DUTY LOG                                      *
000500*                                                                *
000600*  DATE   INITIAL  LOG#    DESCRIPTION                           *
000700*-------- ------- -------- --------------------------------------*
000800*02/08/01   SWS   CNC0331  PROGRAM CREATED.                      *
000900*10/05/01   AJK   CNC0331  DEFAULT FILTER START TIME TO SYS TIME *
001000*11/01/01   AJK   CNC0331A ADDENDUM A.                           *
001100*03/19/03   BLD   CNC0331B CALL CNPS08 FOR TOTAL TIME            *
001200*03/24/04   AJK   CNC0331G ADD 'STATUS AT CALL' AND 'CHG' COLUMNS*
001300*05/05/05   MET   CNC0331H HOURS OF SERVICE CLAIMS.              *
001400*09/15/05   VER   CNC0396  CHANGE HOS FROM 24 TO 36.             *
001500*                            MUST CALCULATE CORRECT HOURS WHEN   *
001600*                            OVER 24 HOURS. CHANGE FLAG @CNC0396 *
001700*12/11/06   JES   CNC0413  ADD FLAG TO FILTER VIRTUAL TOURS.     *
001800*06/11/07   RAL   CNC0331E ADD END DATE/TIME AND REST FIELD      *
001900*03/26/09   SXK   CNC0484  SETUP EXIT TO MAIN MENU (P03-PGM)     *
002000*05/11/09   JES   CNC0484  ADD F12 KEY(NEXT EMP) PROCESSING      *
002100*03/07/23   SXP   CNC0600  WRR PHASE 2: ADDED 28DAYS TTOD CALC.  *
002200*03/24/23   RJA   CNC0600  ADD CONTACT/COMMUTE TYPE RECORDS      *
002200*04/11/23   RJA   C600-CR1 ADD W INCIDENTAL TYPE FOR EXISTING    *
002200*                (CNLD-183 MANUAL RECORDS.                       *
002200*                 CNLD-184 CHANGE 'TOD LOG' HISTORY TO           *
002200*                 CNLD-185)'TOD ADD'/'TOD DEL'/'TOD CHG'.        *
002200*                          ADD EDITS TO GUIDE USER ON C FUNC.    *
002200*04/17/23   RJA   CNLD-196 EMPTOD-RESET-BREAK-REC RECORDS THAT   *
002200*                          END AT 0559 WILL BE DISPLAYED AS 0600.*
      *04/23/23   RKW   CNLD-212 FIX 7 DAY TOTAL DISPLAYING 28 DAY.    *
      *04/24/23   DXC   CNLD-216 DISPLAY 28 DAY FIELDS ON WRR-CAN FLAG *
      *05/08/23   RKW   CNC0600  CNLD-214  DISPLAY NAT OR SYS RESET BRK*
      *05/26/23   DXC   CNC0600  CNLD-234  FIX 28 DAY TO 7 DAY LOG ISSUE
      *06/05/23   DXC   CNC0600  CNLD-242  FIX RESET COS REC TO DISPLAY
      *06/21/23   DXC   CNLD-241 FIX 7 DAY TOTAL FIELD NOT INITIALIZE
      *                          ON EMPLOYEE SWITCH
      *08/21/23   RJA   CNLD-249 COMPUTE AND DISPLAY '7DY RESET:'
      *09/05/23   RJA   CNLD-260 SAVE EMPTOD-TIME-ZONE FOR
      *                          COMMUTE/CONTACT/MANUAL HOS.
      *09/15/23   RJA   CNLD-276 REMOVE CNLD-196 MASKING 0559 AS 0600.
      *                          RENAME RESETCOS TO RESETMNL.
      *                          ADD RESETS32/N32/M32.
      *09/15/23   RJA   CNLD-281 ADD TOTAL TIME FIELD.
002300******************************************************************
002400 ENVIRONMENT DIVISION.
002500 CONFIGURATION SECTION.
002600 SOURCE-COMPUTER. IBM-9370.
002700 OBJECT-COMPUTER. IBM-9370.
002800 DATA DIVISION.
002900 WORKING-STORAGE SECTION.
003000 01  FILLER                          PIC X(10) VALUE 'PGM05Y W/S'.
003100
003200 01  WS-SUBSCRIPTS.
003300     05  I                           PIC 9(02) VALUE ZEROES.
003400     05  SUB1                        PIC 9(02) VALUE ZEROES.
003500     05  ARRAY-MAX                   PIC 9(02) VALUE 10.
003600
003700 01  WS-FLAGS.
003800     05  FIRST-TIME-FLAG             PIC X     VALUE '0'.
003900         88  FIRST-TIME                        VALUE '0'.
004000         88  SECOND-TIME                       VALUE '1'.
004100     05  SCREEN-FLAG                 PIC X     VALUE '0'.
004200         88  CONTINUE-SCREEN                   VALUE '0'.
004300         88  CREATE-SCREEN                     VALUE '1'.
004400         88  SEND-BUFFER                       VALUE '2'.
004500     05  DONE-CODE                   PIC 9     VALUE 0.
004600         88  NOT-DONE                          VALUE 0.
004700         88  DONE                              VALUE 1.
004800     05  FILE-COMPLETE-FLAG          PIC 9     VALUE 0.
004900         88  FILE-NOT-COMPLETE                 VALUE 0.
005000         88  FILE-COMPLETE                     VALUE 1.
005100     05  ENTRY-FOUND-FLAG            PIC 9     VALUE 0.
005200         88  ENTRY-NOT-FOUND                   VALUE 0.
005300         88  ENTRY-FOUND                       VALUE 1.
005400     05  FUNC-FLAG-CHECK             PIC X     VALUE ' '.
005500         88  VALID-FUNC-FLAG                   VALUE 'A',
005600                                                     'C',
005700                                                     'D'.
005800         88  ADD-FUNC                          VALUE 'A'.
005900         88  CHANGE-FUNC                       VALUE 'C'.
006000         88  DELETE-FUNC                       VALUE 'D'.
006100     05  RECORD-FLAG                 PIC X(01) VALUE SPACE.
006200         88  GET-ALL-RECORDS                   VALUE SPACE.
006300         88  FILTER-BY-DATE                    VALUE 'Y'.
006400     05  EMP-DONE-CODE               PIC X     VALUE '0'.
006500         88  EMP-DONE                          VALUE '1'.
006600         88  EMP-NOT-DONE-YET                  VALUE '0'.
006700     05  SEARCH-BY-NAME-FLAG         PIC X     VALUE '0'.
006800         88  FOUND-SAME-NAME                   VALUE '1'.
006900         88  NO-SAME-NAME                      VALUE '0'.
007000     05  GOT-EMPLOYEE-FLAG           PIC X     VALUE '0'.
007100         88  GOT-EMPLOYEE                      VALUE '1'.
007200*CNLD-216-B
007000     05  WS-CAN-WRR-FLAG             PIC X     VALUE ' '.
007100         88  WS-CAN-WRR-NEW                    VALUE 'N'.
007200*CNLD-216-E
007300
007400 01  WS-DT-TM                                  VALUE SPACES.
007500     05  WS-CE                       PIC X(02).
007600     05  WS-DT.
007700         07  WS-YR                   PIC X(02).
007800         07  WS-MO                   PIC X(02).
007900         07  WS-DY                   PIC X(02).
008000     05  WS-TM.
008100         07  WS-HR                   PIC X(02).
008200         07  WS-MN                   PIC X(02).
008300
008400*\/* - BEGIN - @CNC0396 - NEW WS FIELDS
008500 01  WS-CNC0396-CALC-FIELDS.
008600     05  WS-TOT-DAYS                 PIC 9(5).
008700     05  WS-TOT-HOURS                PIC 9(4).
008800     05  WS-REMAINDER-HOURS          PIC 9(4).
008900*/\* - END   - @CNC0396
009000
009100 01  WS-FORMATTED-DT-TM.
009200     05  WS-FORMATTED-DT.
009300         07  WS-FORMATTED-YR         PIC X(02) VALUE SPACES.
009400         07  FILLER                  PIC X(01) VALUE '/'.
009500         07  WS-FORMATTED-MO         PIC X(02) VALUE SPACES.
009600         07  FILLER                  PIC X(01) VALUE '/'.
009700         07  WS-FORMATTED-DY         PIC X(02) VALUE SPACES.
009800     05  FILLER                      PIC X(01) VALUE ' '.
009900     05  WS-FORMATTED-TM.
010000         07  WS-FORMATTED-HR         PIC X(02) VALUE SPACES.
010100         07  FILLER                  PIC X(01) VALUE ':'.
010200         07  WS-FORMATTED-MN         PIC X(02) VALUE SPACES.
010300
010400 01  WS-FORMATTED-TOTAL-TIME.
010500     05  WS-FORMATTED-TOT-HRS        PIC X(04) VALUE SPACES.
010600     05  FILLER                      PIC X(01) VALUE ':'.
010700     05  WS-FORMATTED-TOT-MNS        PIC X(02) VALUE SPACES.
010800
010900 01  CONV-START-DATE-TIME.
011000     05  CONV-START-DATE-CENT.
011100         07  CONV-START-CE           PIC X(02) VALUE SPACES.
011200         07  CONV-START-DATE         PIC X(06) VALUE SPACES.
011300     05  CONV-START-TIME             PIC X(04) VALUE SPACES.
011400
011500 01  CONV-END-DATE-TIME.
011600     05  CONV-END-DATE-CENT.
011700         07  CONV-END-CE             PIC X(02) VALUE SPACES.
011800         07  CONV-END-DATE           PIC X(06) VALUE SPACES.
011900     05  CONV-END-TIME               PIC X(04) VALUE SPACES.
012000
      *CNLD-216-B
      *
       01  WS-LBL-VARIABLE-1               PIC X(19) VALUE
                                                  'DISPLAY 28 DAY LOG?'.
       01  WS-LBL-VARIABLE-2               PIC X(05) VALUE
                                                  '(Y/N)'.
      *CNLD-249-B
      *01  WS-LBL-VARIABLE-3               PIC X(13) VALUE
      *                                           '28-DAY TOTAL:'.
      *01  WS-LBL-VARIABLE-4               PIC X(12) VALUE
      *                                           '7-DAY TOTAL:'.
      *01  WS-LBL-VARIABLE-5               PIC X(12) VALUE
      *                                           'TOTAL TIME :'.
       01  WS-LBL-VARIABLE-3               PIC X(09) VALUE
                                                  '28DY TOT:'.
       01  WS-LBL-VARIABLE-4               PIC X(08) VALUE
                                                  '7DY TOT:'.
       01  WS-LBL-VARIABLE-5               PIC X(08) VALUE
                                                  'TOTL TM:'.
       01  WS-LBL-VARIABLE-6               PIC X(10) VALUE
                                                  '7DY RESET:'.
      *CNLD-249-E
      *
      *CNLD-216-E
012000
012100 01  WS-MISC.
      *
014210*CNLD-216-B
014300     05  SAVE-CNTL-FILE              PIC X(256) VALUE SPACES.
014210*CNLD-216-E
      *
014301*CNC0600-E
      *CNC0600-CNLD-212
160500     05  WS-SAVE-7-HR-TTOD           PIC 9(07) VALUE ZEROES.
012200     05  WS-CALC-NBR-DAYS            PIC 9(02) VALUE ZEROES.
012300     05  WS-MINUTES                  PIC 9(03) VALUE ZEROES.
012400     05  SAVE-TOD-HRMN               PIC X(04) VALUE SPACES.
101200*C600-CR1 - B
012400     05  SAVE-OLD-REST-STATUS        PIC X     VALUE SPACES.
101200*C600-CR1 - E
012500     05  PRESENT-TIME-X                        VALUE ZEROS.
012600         10  PRESENT-DATE            PIC 9(06).
012700         10  PRESENT-TOD             PIC 9(08).
012800     05  WS-TOTAL-TIME-ON-DUTY       PIC 9(007).
012900     05  FILLER REDEFINES WS-TOTAL-TIME-ON-DUTY.
013000         10  FILLER                  PIC X(001).
013100         10  WS-TOTAL-HRS-ON-DUTY    PIC X(004).
013200         10  WS-TOTAL-MNS-ON-DUTY    PIC X(002).
013300     05  WS-VALUES.
013400         10  WS-GREEN                PIC X(007) VALUE 'GREEN  '.
013500         10  WS-YELLOW               PIC X(007) VALUE 'YELLOW '.
013600         10  WS-RED                  PIC X(007) VALUE 'RED    '.
013700         10  WS-RESPITE              PIC X(007) VALUE 'RESPITE'.
013800     05  END-MSG                     PIC X(42) VALUE
013900         'END OF EMPLOYEES IN DIST/SUB-DIST         '.
014000 01  WS-MSGA.
014100     05  FILLER                  PIC X(23)
014200                                  VALUE ' EMPLOYEE IN DISTRICT: '.
014300     05  WS-MSGA-DIST            PIC XX   VALUE SPACE.
014400     05  FILLER                  PIC X(16)
014500                                        VALUE '  SUB-DISTRICT: '.
014600     05  WS-MSGA-SUB-DIST        PIC XX   VALUE SPACE.
014700
014800******************************************************************
014900***                  TEMPORARY STORAGE QUEUE                   ***
015000******************************************************************
015100 01  P05YTSQ-QUEUE-ITEM         PIC S9(4)  COMP VALUE +1.
015200 01  P05YTSQ-MAP-QUEUE-ID.
015300     05  P05YTSQ-MAP-QUEUE      PIC X(4)   VALUE '05YM'.
015400     05  P05YTSQ-MAP-TERM-ID    PIC X(4)   VALUE SPACES.
015500 01  P05YTSQ-CA-QUEUE-ID.
015600     05  P05YTSQ-CA-QUEUE       PIC X(4)   VALUE '05YC'.
015700     05  P05YTSQ-CA-TERM-ID     PIC X(4)   VALUE SPACES.
015800 01  P05YTSQ-QLGTH              PIC S9(4)  COMP VALUE +1.
015900*****************************************************************
016000***                 I/O STATUS CHECK FIELDS
016100*****************************************************************
016200 01  WS-RESPONSE              PIC S9(8) COMP VALUE ZEROES.
016300 01  FILE-STATUS              PIC 9(4)  VALUE ZEROES.
016400     COPY IOCODES.
016500*****************************************************************
016600***                    COMMAREA COPYBOOKS
016700*****************************************************************
016800     COPY PSTCOMM.
016900     COPY P05YCOMM.
017000     COPY P05COMM.
017100     COPY P998COMM.
017200*****************************************************************
017300***                     MAP AREA COPYBOOK
017400*****************************************************************
017500     COPY PSM05YRE.
017600*****************************************************************
017700***                   PROGRAM NAMES COPYBOOKS
017800*****************************************************************
031100*CNC0600 - RJA - B
017900     COPY PSTCB02.
031100*CNC0600 - RJA - E
017900     COPY PSTCB03.
018000     COPY PSTCB05A.
018100     COPY PSTCB05Y.
018200     COPY PSTCB998.
018300*****************************************************************
018400***                 CALLED ROUTINES COPYBOOKS.
018500*****************************************************************
018600     COPY PSTERAR.
018700     COPY P902COMM.
018800     COPY P903COMM.
018900     COPY P943COMM.
019000     COPY PS08COMM.
019100*****************************************************************
019200***                     FILE COPYBOOKS
019300*****************************************************************
019400     COPY WSEMPTOD.
019500     COPY WSCNTL.
019600     COPY WSMSTR.
019600*CNLD-249 RJA B
019600     COPY WSMSTR3.
019600*CNLD-249 RJA E
019700*****************************************************************
019800***                     MISC. COPYBOOKS
019900*****************************************************************
020000     COPY PSTKEYS.
020100     COPY PSTATTR.
020200     COPY WSBUFFER.
020300     COPY WSMSG.
020400     COPY WSOFFSET.
020500     COPY WSSYDTTM.
020600     COPY WSZONE.
020700     COPY WSBIF.
020800     COPY WSEDDATE.
020900     COPY WSEDTIME.
021000*****************************************************************
021100 LINKAGE SECTION.
021200 01  DFHCOMMAREA.
021300     05  LK-PST-CA                PIC X(170).
021400*****************************************************************
021500 PROCEDURE DIVISION.
021600******************************************************************
021700 P0000-MAINLINE.
021800******************************************************************
021900     EXEC CICS IGNORE CONDITION ERROR END-EXEC
022000     EXEC CICS HANDLE ABEND LABEL(P9999-GOT-PROBLEM) END-EXEC
022100
022200     COPY ABSTIME.
022300
022400     IF EIBCALEN = ZERO
022500        PERFORM P9990-CLEAR-SCREEN
022600     END-IF
022700
022800     MOVE LK-PST-CA                 TO PSTCOMM-AREA
022900
023000     IF EIBTRNID NOT = P05Y-TRAN
023100        IF EIBTRNID = P998-TRAN
023200           SET CREATE-SCREEN      TO TRUE
023300           MOVE LOW-VALUES        TO PSTS05Y
023400           MOVE P998CA-CURSOR-POS TO EIBCPOSN
023500           PERFORM P7110-READ-TSQUEUE
023600           PERFORM P9000-SEND-MAP-AND-RETURN
023700        END-IF
023800        PERFORM P0010-INITIAL-SETUP
023900     END-IF
024000
024100     MOVE EIBAID                  TO PF-CHECK
024200
024200
024300     IF EXIT-KEY
024400        IF PSTCA-FROM-PROGRAM = P03-PGM
024500           PERFORM P9200-XFER-TO-MAIN-MENU
024600        ELSE
031100*CNC0600 - RJA - B
024400           IF PSTCA-FROM-PROGRAM = P02-PGM
024500              PERFORM P9250-XFER-TO-FLD-MENU
025200           END-IF
031100*CNC0600 - RJA - E
024700           MOVE PSTCA-DIST           TO SCR05Y-DIST
024800           MOVE PSTCA-SUB-DIST       TO SCR05Y-SUB-DIST
024900           MOVE PSTCA-EMP-NBR        TO SCR05Y-EMP-NBR
025000           MOVE P05CA-EMP-NAME       TO SCR05Y-EMP-NAME
025100           PERFORM P9100-XFER-TO-05A
025200        END-IF
025300     END-IF
025400
025500     PERFORM P0100-PROCESS-INPUT
025600
025700     MOVE -1                      TO SCR05Y-START-DATE-CURSOR
025800     PERFORM P9000-SEND-MAP-AND-RETURN.
025900******************************************************************
026000 P0010-INITIAL-SETUP.
026100******************************************************************
026200     SET CREATE-SCREEN               TO TRUE
026300     SET ENTER-KEY                   TO TRUE
026400     MOVE LOW-VALUES                 TO PSTS05Y
026500     PERFORM P0020-PREPARE-SCREEN
026600     PERFORM P0030-RESET-ATTRIBUTES
026700     MOVE PSTCA-DIST                 TO SCR05Y-DIST
026800     MOVE PSTCA-SUB-DIST             TO SCR05Y-SUB-DIST
026900     MOVE PSTCA-EMP-NBR              TO SCR05Y-EMP-NBR
027000     MOVE P05CA-EMP-NAME             TO SCR05Y-EMP-NAME
027100     MOVE SPACES                     TO P05YCA-START-KEY
027200                                        P05YCA-PF7-KEY
027300                                        P05YCA-PF8-KEY
027400
027500     PERFORM P0060-GET-DATE-TIME
      **CNLD-216-B
027600     PERFORM P4500-GET-LABELS
      **CNLD-216-E
027700     IF EIBTRNID = P03-TRAN
027800     OR (P05YCA-EMP-NAME      NOT > SPACES
027900     AND P05YCA-EMP-NO        NOT > SPACES)
028000        MOVE PSTCA-DIST          TO SCR05Y-DIST
028100        MOVE PSTCA-SUB-DIST      TO SCR05Y-SUB-DIST
028200        MOVE SPACES              TO P05YCOMM-AREA
028300        MOVE -1                  TO SCR05Y-EMP-NAME-CURSOR
028400        IF  PSTCA-EMP-NBR         > ZEROS
028500        AND EIBTRNID              = P03-TRAN
028600           MOVE PSTCA-EMP-NBR    TO SCR05Y-EMP-NBR
028700        ELSE
028800           PERFORM P9000-SEND-MAP-AND-RETURN
028900        END-IF
029000     END-IF
029100     PERFORM P1000-INQUIRY
029200     MOVE -1                         TO SCR05Y-START-DATE-CURSOR
029300     PERFORM P9000-SEND-MAP-AND-RETURN.
029400******************************************************************
029500 P0020-PREPARE-SCREEN.
029600******************************************************************
029700     MOVE SPACES             TO SCR05Y-DIST
029800                                SCR05Y-SUB-DIST
029900                                SCR05Y-EMP-NBR
030000                                SCR05Y-EMP-NAME
030100                                SCR05Y-START-DATE
030200                                SCR05Y-START-TIME
030300                                SCR05Y-END-DATE
030400                                SCR05Y-END-TIME
030500                                SCR05Y-INCL-VIRTUAL
030600                                SCR05Y-FUNC-FLAG
030700                                SCR05Y-TIME-SPENT-FLTR
030800                                SCR05Y-AS-OF-DATE
030900                                SCR05Y-AS-OF-TIME
031000                                SCR05Y-REST-CODE
031100*CNC0600 - RJA - B
031200                                SCR05Y-TYPE
031300*CNC0600 - RJA - E
031400     PERFORM P0025-CLEAR-ARRAY.
031500******************************************************************
031600 P0025-CLEAR-ARRAY.
031700******************************************************************
031800     PERFORM VARYING SUB1 FROM 1 BY 1
031900       UNTIL SUB1 > ARRAY-MAX
032000         MOVE SPACES         TO SCR05Y-FUNC(SUB1)
032100                                SCR05Y-OD-DT-TM(SUB1)
032200                                SCR05Y-DISTR(SUB1)
032300                                SCR05Y-SUB-DISTR(SUB1)
032400                                SCR05Y-TRAIN(SUB1)
032500                                SCR05Y-ASGN(SUB1)
032600                                SCR05Y-CRAFT(SUB1)
032700                                SCR05Y-OFF-DT-TM(SUB1)
032800                                SCR05Y-TIME-SPENT(SUB1)
032900                                SCR05Y-CALL-STATUS(SUB1)
033000                                SCR05Y-MANUAL-UPD(SUB1)
033100     END-PERFORM.
033200******************************************************************
033300 P0030-RESET-ATTRIBUTES.
033400******************************************************************
033500     MOVE DEFAULT-ATTR       TO SCR05Y-DIST-HI
033600                                SCR05Y-SUB-DIST-HI
033700                                SCR05Y-EMP-NBR-HI
033800                                SCR05Y-EMP-NAME-HI
033900                                SCR05Y-START-DATE-HI
034000                                SCR05Y-START-TIME-HI
034100                                SCR05Y-END-DATE-HI
034200                                SCR05Y-END-TIME-HI
034300                                SCR05Y-INCL-VIRTUAL-HI
034400                                SCR05Y-FUNC-FLAG-HI
034500                                SCR05Y-TIME-SPENT-FLTR-HI
034600                                SCR05Y-AS-OF-DATE-HI
034700                                SCR05Y-AS-OF-TIME-HI
034800                                SCR05Y-REST-CODE-HI
034900*CNC0600 - RJA - B
035000                                SCR05Y-TYPE-HI
035100*CNC0600 - RJA - E
035200     PERFORM VARYING SUB1 FROM 1 BY 1
035300       UNTIL SUB1 > ARRAY-MAX
035400         MOVE DEFAULT-ATTR   TO SCR05Y-FUNC-HI(SUB1)
035500                                SCR05Y-OD-DT-TM-HI(SUB1)
035600                                SCR05Y-DISTR-HI(SUB1)
035700                                SCR05Y-SUB-DISTR-HI(SUB1)
035800                                SCR05Y-TRAIN-HI(SUB1)
035900                                SCR05Y-ASGN-HI(SUB1)
036000                                SCR05Y-CRAFT-HI(SUB1)
036100                                SCR05Y-OFF-DT-TM-HI(SUB1)
036200                                SCR05Y-TIME-SPENT-HI(SUB1)
036300                                SCR05Y-CALL-STATUS-HI(SUB1)
036400                                SCR05Y-MANUAL-UPD-HI(SUB1)
036500     END-PERFORM
036600
036700     MOVE ZEROS              TO SCR05Y-DIST-CURSOR
036800                                SCR05Y-SUB-DIST-CURSOR
036900                                SCR05Y-EMP-NBR-CURSOR
037000                                SCR05Y-EMP-NAME-CURSOR
037100                                SCR05Y-START-DATE-CURSOR
037200                                SCR05Y-START-TIME-CURSOR
037300                                SCR05Y-END-DATE-CURSOR
037400                                SCR05Y-END-TIME-CURSOR
037500                                SCR05Y-INCL-VIRTUAL-CURSOR
037600                                SCR05Y-FUNC-FLAG-CURSOR
037700                                SCR05Y-TIME-SPENT-FLTR-CURSOR
037800                                SCR05Y-AS-OF-DATE-CURSOR
037900                                SCR05Y-AS-OF-TIME-CURSOR
038000                                SCR05Y-REST-CODE-CURSOR
038100*CNC0600 - RJA - B
038200                                SCR05Y-TYPE-CURSOR
038300*CNC0600 - RJA - E
038400     PERFORM VARYING SUB1 FROM 1 BY 1
038500       UNTIL SUB1 > ARRAY-MAX
038600         MOVE ZEROS          TO SCR05Y-FUNC-CURSOR(SUB1)
038700                                SCR05Y-OD-DT-TM-CURSOR(SUB1)
038800                                SCR05Y-DISTR-CURSOR(SUB1)
038900                                SCR05Y-SUB-DISTR-CURSOR(SUB1)
039000                                SCR05Y-TRAIN-CURSOR(SUB1)
039100                                SCR05Y-ASGN-CURSOR(SUB1)
039200                                SCR05Y-CRAFT-CURSOR(SUB1)
039300                                SCR05Y-OFF-DT-TM-CURSOR(SUB1)
039400                                SCR05Y-TIME-SPENT-CURSOR(SUB1)
039500                                SCR05Y-CALL-STATUS-CURSOR(SUB1)
039600                                SCR05Y-MANUAL-UPD-CURSOR(SUB1)
039700     END-PERFORM
031100*CNC0600 - RJA - B
031100     IF PSTCA-FROM-FLD-MENU
145700        MOVE PROTECT-MDT     TO SCR05Y-DIST-ATTR
033600                                SCR05Y-SUB-DIST-ATTR
033700                                SCR05Y-EMP-NBR-ATTR
033800                                SCR05Y-EMP-NAME-ATTR
034400                                SCR05Y-FUNC-FLAG-ATTR
034500                                SCR05Y-TIME-SPENT-FLTR-ATTR
034600                                SCR05Y-AS-OF-DATE-ATTR
034700                                SCR05Y-AS-OF-TIME-ATTR
034800                                SCR05Y-REST-CODE-ATTR
035000                                SCR05Y-TYPE-ATTR
038400        PERFORM VARYING SUB1 FROM 1 BY 1
038500          UNTIL SUB1 > ARRAY-MAX
038600            MOVE PROTECT-MDT TO SCR05Y-FUNC-ATTR(SUB1)
039700        END-PERFORM
039700     END-IF.
031100*CNC0600 - RJA - E
039800******************************************************************
039900 P0060-GET-DATE-TIME.
040000******************************************************************
040100     EXEC CICS ASKTIME
040200               ABSTIME(WS-ABSTIME)
040300     END-EXEC
040400     ADD WS-ABSTIME-OFFSET           TO WS-ABSTIME
040500     EXEC CICS FORMATTIME
040600               ABSTIME(WS-ABSTIME)
040700               YYYYMMDD(WS-SYSTEM-DATE-CENT)
040800               TIME(WS-SYSTEM-TIME-AREA)
040900     END-EXEC
041000*
041100*    INSTALL APPLICATION DATE/TIME
041200*
041300     IF PSTCA-DATE-TIME-OFFSET > SPACES
041400        MOVE ZEROS                   TO DATE-CONVERSION-PARMS
041500        MOVE WS-SYSTEM-DATE          TO PARM-PRI-DATE-GREG
041600        MOVE WS-SYSTEM-TIME          TO PARM-PRI-HRMN
041700        PERFORM P9810-PROCESS-OFFSET
041800        MOVE PARM-RES-GREG-CENT      TO WS-SYSTEM-CENT
041900        MOVE PARM-RES-DATE-GREG      TO WS-SYSTEM-DATE
042000        MOVE PARM-RES-HRMN           TO WS-SYSTEM-TIME
042100     END-IF
042200*
042300*    CONVERT SYSTEM DATE/TIME TO LOCAL DATE/TIME
042400*
042500     MOVE SPACES                     TO TZ-PARAMETERS
042600     MOVE TZ-SYSTEM-TIME-ZONE        TO TZ-IN-ZONE
042700     MOVE WS-PRESENT-TIME            TO TZ-IN-DATE-TIME
042800     MOVE PSTCA-TIME-ZONE            TO TZ-OUT-ZONE
042900     PERFORM P8996-TIMEZONE
043000     IF TZ-INVALID-PARAMETERS
043100        MOVE 'A0000TZ'               TO ERR-PARAGRAPH
043200        PERFORM P8996-TZERROR
043300     END-IF
043400     MOVE TZ-OUT-DATE-TIME-CENT      TO WS-LOCAL-DATE-TIME-CENT.
043500******************************************************************
043600 P0100-PROCESS-INPUT.
043700******************************************************************
043800     MOVE P05Y-MAP-VERSION(PSTCA-SUB) TO P05Y-MAP
043900     EXEC CICS RECEIVE MAP(P05Y-MAP)
044000                       MAPSET(P05Y-SET)
044100                       INTO(PSTS05Y)
044200                       RESP(WS-RESPONSE)
044300     END-EXEC
044400     MOVE WS-RESPONSE  TO FILE-STATUS
044500     IF NOT SUCCESS
044600        MOVE 'P0100-1' TO ERR-PARAGRAPH
044700        PERFORM P9999-GOT-PROBLEM
044800     END-IF
044900     IF PFKEY1
045000        PERFORM P7100-WRITE-TSQUEUE
045100        PERFORM P9500-SETUP-SCR998
045200     END-IF
045300     SET CONTINUE-SCREEN TO TRUE
045400     PERFORM P0030-RESET-ATTRIBUTES
045500     PERFORM P0060-GET-DATE-TIME
045600     MOVE SPACES TO SCR05Y-ERRORMSG
045700     IF P05YCA-EMP-NAME              = SPACES
045800        MOVE LOW-VALUES             TO P05YCA-EMP-NAME
045900     END-IF
046000
046100     IF SCR05Y-EMP-NBR               > SPACES
046200        IF SCR05Y-EMP-NBR           NOT NUMERIC
046300           MOVE SCR05Y-EMP-NBR      TO BIF-FIELD
046400           MOVE +9                  TO BIF-LEN
046500           PERFORM P8999-BIFEDIT
046600           IF BIF-ERROR
046700           OR BIF-EDITED-FIELD       = ZEROES
046800*               'INVALID EMPLOYEE NUMBER'
046900               MOVE 'I036'          TO MSGLOG-CODE
047000               MOVE -1              TO SCR05Y-EMP-NBR-CURSOR
047100               MOVE REV-VIDEO       TO SCR05Y-EMP-NBR-HI
047200               PERFORM P9000-SEND-MAP-AND-RETURN
047300           END-IF
047400           MOVE BIF-EDITED-FIELD    TO SCR05Y-EMP-NBR
047500        END-IF
047600     END-IF
047700
031100*CNC0600 - RJA - B
031100     IF PSTCA-FROM-FLD-MENU
023900        IF PFKEY5 OR PFKEY12
050100*          FUNCTION NOT AVAILABLE FROM FIELD TERMINAL
050200           MOVE 'F041' TO MSGLOG-CODE
023600           PERFORM P9000-SEND-MAP-AND-RETURN
023900        END-IF
023900     END-IF
031100*CNC0600 - RJA - E
      *
047700
047800     EVALUATE TRUE
047900        WHEN ENTER-KEY OR PFKEY12
048000           PERFORM P1000-INQUIRY
048100        WHEN PFKEY5
048200           PERFORM P2000-UPDATE
048300           PERFORM P1000-INQUIRY
048400        WHEN PFKEY7
048500           IF  P05YCA-PF7-KEY        = P05YCA-START-KEY
048600*                'NO FURTHER SCROLLING POSSIBLE'
048700              MOVE 'N012'           TO MSGLOG-CODE
048800              MOVE -1               TO SCR05Y-START-DATE-CURSOR
048900              PERFORM P9000-SEND-MAP-AND-RETURN
049000           END-IF
049100           PERFORM P1000-INQUIRY
049200        WHEN PFKEY8
049300           IF P05YCA-PF8-KEY     NOT > SPACES
049400*                'NO FURTHER SCROLLING POSSIBLE'
049500              MOVE 'N012'           TO MSGLOG-CODE
049600              MOVE -1               TO SCR05Y-START-DATE-CURSOR
049700              PERFORM P9000-SEND-MAP-AND-RETURN
049800           END-IF
049900           PERFORM P1000-INQUIRY
050000        WHEN OTHER
050100*             INVALID-FUNC-MSG
050200        MOVE 'I006' TO MSGLOG-CODE
050300     END-EVALUATE.
050400******************************************************************
050500 P1000-INQUIRY.
050600******************************************************************
      *
      *CNLD-216-B
           PERFORM P4500-GET-LABELS
      *CNLD-216-E
050700     IF SCR05Y-EMP-NBR NOT > SPACES
050800        MOVE SPACES                 TO SCR05Y-EMP-NBR
050900     END-IF
051000     IF (P05YCA-EMP-NAME = SCR05Y-EMP-NAME
051100        AND NOT PFKEY12
051200        AND SCR05Y-EMP-NBR > ZEROES)
051300        OR ((SCR05Y-EMP-NAME = SPACES OR LOW-VALUES)
051400        AND (SCR05Y-EMP-NBR > ZEROES AND NOT PFKEY12))
051500        MOVE SCR05Y-EMP-NBR             TO BIF-FIELD
051600        MOVE +9                         TO BIF-LEN
051700        PERFORM P8999-BIFEDIT
051800        IF BIF-ERROR
051900*               'INVALID EMPLOYEE NUMBER'
052000           MOVE 'I036'                  TO MSGLOG-CODE
052100           MOVE -1                      TO SCR05Y-EMP-NBR-CURSOR
052200           MOVE REV-VIDEO               TO SCR05Y-EMP-NBR-HI
052300           PERFORM P9000-SEND-MAP-AND-RETURN
052400        END-IF
052500        MOVE BIF-EDITED-FIELD           TO SCR05Y-EMP-NBR
052600        IF SCR05Y-EMP-NBR > ZEROES
052700           MOVE SCR05Y-EMP-NBR          TO MSTRNBRK
052800           EXEC CICS READ
052900                     DATASET(MSTR-VIA-EMP-NBR)
053000                     INTO(WS-MSTR)
053100                     LENGTH(MSTRENBR-RLGTH)
053200                     RIDFLD(MSTRNBRK)
053300                     KEYLENGTH(MSTRENBR-KLGTH)
053400                     RESP(WS-RESPONSE)
053500           END-EXEC
053600           MOVE WS-RESPONSE             TO FILE-STATUS
053700           IF SUCCESS
053800              IF DIST         OF WS-MSTR = SCR05Y-DIST
053900                 AND SUB-DIST OF WS-MSTR = SCR05Y-SUB-DIST
054000                 PERFORM P1010-SETUP-INQUIRY
054100              ELSE
054200*                   EMP NOT IN THIS DIST/SUBDIST
054300                 MOVE 'E053' TO MSGLOG-CODE
054400                 MOVE -1                TO SCR05Y-EMP-NBR-CURSOR
054500                 MOVE REV-VIDEO         TO SCR05Y-EMP-NBR-HI
054600                 PERFORM P9000-SEND-MAP-AND-RETURN
054700              END-IF
054800           ELSE
054900              IF NO-RECORD-FND OR END-OF-FILE
055000*                     NOT-FOUND-MSG
055100                 MOVE 'N015'            TO MSGLOG-CODE
055200                 MOVE -1                TO SCR05Y-EMP-NBR-CURSOR
055300                 MOVE REV-VIDEO         TO SCR05Y-EMP-NBR-HI
055400              ELSE
055500                 MOVE 'P1000-?'         TO ERR-PARAGRAPH
055600                 MOVE SPACES            TO ERR-KEY
055700                 PERFORM P9999-GOT-PROBLEM
055800              END-IF
055900           END-IF
056000        ELSE
056100*               'INVALID EMPLOYEE NUMBER ENTERED'
056200           MOVE 'I036'                  TO MSGLOG-CODE
056300           MOVE -1                      TO SCR05Y-EMP-NBR-CURSOR
056400           MOVE REV-VIDEO               TO SCR05Y-EMP-NBR-HI
056500        END-IF
056600     ELSE
056700        PERFORM P5010-INQUIRE-VIA-NAME
056800     END-IF
056900     .
057000******************************************************************
057100 P1010-SETUP-INQUIRY.
057200******************************************************************
057300     MOVE SCR05Y-FUNC-FLAG TO FUNC-FLAG-CHECK
057400     PERFORM P0025-CLEAR-ARRAY
057500
057600     MOVE EMP-NBR  OF WS-MSTR TO SCR05Y-EMP-NBR
057700                                 PSTCA-EMP-NBR
057800     MOVE EMP-NAME OF WS-MSTR TO SCR05Y-EMP-NAME
057900                                 P05YCA-EMP-NAME
058000     MOVE DIST     OF WS-MSTR TO SCR05Y-DIST
058100                                 PSTCA-DIST
058200     MOVE SUB-DIST OF WS-MSTR TO SCR05Y-SUB-DIST
058300                                 PSTCA-SUB-DIST
058400
058500     IF PSTCA-EMP-NBR NOT = P05YCA-EMP-NO
058600        MOVE SPACES           TO P05YCA-PF7-KEY
058700                                 P05YCA-PF8-KEY
058800                                 P05YCA-START-KEY
058900                                 P05YCA-START-DATE-TIME
059000        MOVE PSTCA-EMP-NBR    TO P05YCA-EMP-NO
059100        SET ENTER-KEY         TO TRUE
059200     END-IF
059300
059400     IF PFKEY7
059500        PERFORM P1300-PAGE-UP
059600     ELSE
059700        PERFORM P1100-READ-ETOD-FILE
059800     END-IF.
059900******************************************************************
060000 P1100-READ-ETOD-FILE.
060100******************************************************************
060200     MOVE 0                        TO SUB1
060300     INITIALIZE WS-EMP-TOD
060400     MOVE SCR05Y-EMP-NBR           TO EMPTOD-EMP-NBR
060500     PERFORM P1200-CHECK-FILTERS
060600     IF PFKEY8
060700        MOVE P05YCA-PF8-KEY        TO EMPTOD-OD-DATE-TIME
060800     END-IF
060900     PERFORM P8000-STARTBR-ETOD
061000     IF SUCCESS
061100        SET NOT-DONE               TO TRUE
061200        PERFORM UNTIL DONE
061300*** 331E - MAKE SURE OD-DATE-TIME IS > CONV-START-DATE-TIME
061400           PERFORM P8010-READNEXT-ETOD
061500           IF SUCCESS AND EMPTOD-EMP-NBR = SCR05Y-EMP-NBR
061600              AND EMPTOD-OFFD-DATE-TIME  > CONV-START-DATE-TIME
061700              AND (EMPTOD-OD-DATE-TIME   < CONV-END-DATE-TIME
061800                   OR GET-ALL-RECORDS)
061900                 IF  EMPTOD-VIRTUAL-ENTRY
062000                 AND SCR05Y-INCL-VIRTUAL NOT = 'Y'
062100                    CONTINUE
062200                 ELSE
062300                    ADD 1             TO SUB1
062400                    IF SUB1               > ARRAY-MAX
062500                       SET DONE       TO TRUE
062600                       MOVE EMPTOD-OD-DATE-TIME
062700                                      TO P05YCA-PF8-KEY
062800                    ELSE
062900                       IF SUB1 = 1
063000                          IF ENTER-KEY
063100                             MOVE EMPTOD-OD-DATE-TIME
063200                                      TO P05YCA-START-KEY
063300                          END-IF
063400                          MOVE EMPTOD-OD-DATE-TIME
063500                                      TO P05YCA-PF7-KEY
063600                       END-IF
063700                       PERFORM P4100-BUILD-SCREEN-ARRAY
063800                    END-IF
063900                 END-IF
064000*** 331E - ADD CONTINUE TO BYPASS OLDER RECORDS BUT NOT END READ
064100           ELSE
064200              IF SUCCESS
064300                 AND EMPTOD-EMP-NBR = SCR05Y-EMP-NBR
064400                 AND EMPTOD-OFFD-DATE-TIME <= CONV-START-DATE-TIME
064500                 CONTINUE
064600              ELSE
064700                 SET DONE             TO TRUE
064800              END-IF
064900           END-IF
065000        END-PERFORM
065100        PERFORM P8020-ENDBR-ETOD
065200        PERFORM P4300-GET-TOTAL-TIME
065300*CNLD-249-B 9/14/23 RJA
065300**CNC0600-B
065400*        PERFORM P4400-GET-28D-TTOD
065500**CNC0600-E
              MOVE SPACES           TO SCR05Y-28DAY-TTOD
                                       SCR05Y-7DAY-RESET
065600        IF WS-CAN-WRR-NEW
065400           PERFORM P4400-GET-28D-TTOD
065400           PERFORM P4310-GET-7D-END-PERIOD
065600        END-IF
065500*CNLD-249-E 9/14/23 RJA
065600     END-IF
065700     IF NOT PFKEY5
065800        EVALUATE TRUE
065900           WHEN PFKEY8
066000              IF SUB1 < ARRAY-MAX
066100*                'NO FURTHER SCROLLING POSSIBLE'
066200                 MOVE 'N012' TO MSGLOG-CODE
066300                 MOVE SPACES TO P05YCA-PF8-KEY
066400              ELSE
066500*                'SCROLL SUCCESSFUL'
066600                 MOVE 'S041' TO MSGLOG-CODE
066700              END-IF
066800           WHEN CHANGE-FUNC
066900*             'PLEASE SELECT RECORDS, MAKE CHANGES AND PRESS F5'
067000              MOVE 'P192'    TO MSGLOG-CODE
067100           WHEN ADD-FUNC
067200*             'PLEASE ENTER DATA AND PRESS F5'
067300              MOVE 'P193'    TO MSGLOG-CODE
067400              MOVE -1        TO SCR05Y-TIME-SPENT-FLTR-CURSOR
067500              MOVE REV-VIDEO TO SCR05Y-TIME-SPENT-FLTR-HI
067600                                SCR05Y-AS-OF-DATE-HI
067700                                SCR05Y-AS-OF-TIME-HI
067800                                SCR05Y-REST-CODE-HI
031100*CNC0600 - RJA - B
031200                                SCR05Y-TYPE-HI
031300*CNC0600 - RJA - E
067900           WHEN DELETE-FUNC
068000*             'PLEASE SELECT RECORDS AND PRESS F5'
068100              MOVE 'P194'    TO MSGLOG-CODE
068200           WHEN OTHER
068300              IF SUB1 < ARRAY-MAX
068400*                END-OF-FILE
068500                 MOVE 'E011' TO MSGLOG-CODE
068600                 MOVE SPACES TO P05YCA-PF8-KEY
068700              END-IF
068800        END-EVALUATE
068900     END-IF.
069000******************************************************************
069100 P1200-CHECK-FILTERS.
069200******************************************************************
069300*CNC0600-B
069300*
069300*CNLD-234-DXC-B
           IF WS-CAN-WRR-NEW
           AND P05YCA-28-DAY-LOG-YES
           AND (SCR05Y-DISPLAY-28D = 'N' OR SPACES)
              SET P05YCA-28-DAY-LOG-NO TO TRUE
              MOVE SPACES              TO SCR05Y-START-DATE
                                          SCR05Y-START-TIME
           END-IF
069300*CNLD-234-DXC-E
069400     IF SCR05Y-DISPLAY-28D = 'Y'
              SET P05YCA-28-DAY-LOG-YES TO TRUE
069500        MOVE ZEROES              TO DATE-CONVERSION-PARMS
069600        SET PARM-SUBTRACT        TO TRUE
069700        MOVE WS-LOCAL-DATE       TO PARM-PRI-DATE-GREG
069800        MOVE '000028'            TO PARM-SEC-DATE-GREG
069900        EXEC CICS LINK
070000                  PROGRAM(P903-PGM)
070100                  COMMAREA(DATE-CONVERSION-PARMS)
070200                  LENGTH(P903-LGTH)
070300                  RESP(WS-RESPONSE)
070400        END-EXEC
070500        MOVE WS-RESPONSE TO FILE-STATUS
070600        IF NOT SUCCESS
070700           MOVE 'P1200-1' TO ERR-PARAGRAPH
070800           PERFORM P9000-SEND-MAP-AND-RETURN
070900        END-IF
071000        MOVE PARM-RES-DATE-GREG TO SCR05Y-START-DATE
071100     END-IF
071200*CNC0600-E
071300     IF SCR05Y-START-DATE NOT > SPACES
071400        MOVE ZEROES              TO DATE-CONVERSION-PARMS
071500        SET PARM-SUBTRACT        TO TRUE
071600        MOVE WS-LOCAL-DATE       TO PARM-PRI-DATE-GREG
071700        MOVE '000007'            TO PARM-SEC-DATE-GREG
071800        EXEC CICS LINK
071900                  PROGRAM(P903-PGM)
072000                  COMMAREA(DATE-CONVERSION-PARMS)
072100                  LENGTH(P903-LGTH)
072200                  RESP(WS-RESPONSE)
072300        END-EXEC
072400        MOVE WS-RESPONSE TO FILE-STATUS
072500        IF NOT SUCCESS
072600           MOVE 'P1200-1' TO ERR-PARAGRAPH
072700           PERFORM P9000-SEND-MAP-AND-RETURN
072800        END-IF
072900        MOVE PARM-RES-DATE-GREG TO SCR05Y-START-DATE
073000     END-IF
073100
073200     MOVE SCR05Y-START-DATE     TO DE-YYMMDD
073300     SET DE-YYMMDD-FORMAT       TO TRUE
073400     PERFORM P8998-DATEEDIT
073500     IF DE-INVALID-DATE
073600*        'INVALID DATE ENTERED'
073700        MOVE 'I034'             TO MSGLOG-CODE
073800        MOVE -1                 TO SCR05Y-START-DATE-CURSOR
073900        MOVE REV-VIDEO          TO SCR05Y-START-DATE-HI
074000        PERFORM P9000-SEND-MAP-AND-RETURN
074100     END-IF
074200     MOVE DE-CCYYMMDD           TO EMPTOD-OD-DATE-CENT
074300                                   P05YCA-START-DATE-CENT
074400                                   CONV-START-DATE-CENT
074500*** 331E - START BROWSE 24 HOURS EARLIER SO WE CAN INCLUDE
074600***        ALL RECORDS THAT FALL WITHIN THE RANGE
074700*
074800     MOVE ZEROES                 TO DATE-CONVERSION-PARMS
074900     MOVE CONV-START-DATE        TO PARM-PRI-DATE-GREG
075000     MOVE 1                      TO PARM-SEC-DATE-GREG
075100     SET  PARM-SUBTRACT          TO TRUE
075200     PERFORM P9300-LINK-P903
075300     MOVE PARM-RES-DATE-GREG     TO EMPTOD-OD-DATE
075400     MOVE PARM-RES-GREG-CENT     TO EMPTOD-OD-CE
075500*
075600     IF SCR05Y-START-TIME > SPACES
075700        MOVE SCR05Y-START-TIME   TO TE-MILITARY-TIME
075800        SET TE-MILITARY-FORMAT   TO TRUE
075900        PERFORM P8997-TIMEEDIT
076000        IF TE-INVALID-TIME
076100*           'INVALID TIME ENTERED'
076200           MOVE 'I022'           TO MSGLOG-CODE
076300           MOVE -1               TO SCR05Y-START-TIME-CURSOR
076400           MOVE REV-VIDEO        TO SCR05Y-START-TIME-HI
076500           PERFORM P9000-SEND-MAP-AND-RETURN
076600        END-IF
076700        MOVE SCR05Y-START-TIME   TO EMPTOD-OD-TIME
076800                                    P05YCA-START-TIME
076900     ELSE
077000        MOVE WS-LOCAL-TIME       TO SCR05Y-START-TIME
077100                                    EMPTOD-OD-TIME
077200                                    P05YCA-START-TIME
077300     END-IF
077400     MOVE SCR05Y-START-TIME      TO CONV-START-TIME
077500
077600     IF SCR05Y-END-DATE          <= SPACES AND
077700        SCR05Y-END-TIME          <= SPACES
077800        SET GET-ALL-RECORDS      TO TRUE
077900     ELSE
078000        SET FILTER-BY-DATE       TO TRUE
078100     END-IF
078200
078300     IF SCR05Y-END-DATE > SPACES
078400        MOVE SCR05Y-END-DATE     TO DE-YYMMDD
078500        SET DE-YYMMDD-FORMAT     TO TRUE
078600        PERFORM P8998-DATEEDIT
078700        IF DE-INVALID-DATE
078800*           'INVALID DATE ENTERED'
078900           MOVE 'I034'           TO MSGLOG-CODE
079000           MOVE -1               TO SCR05Y-END-DATE-CURSOR
079100           MOVE REV-VIDEO        TO SCR05Y-END-DATE-HI
079200           PERFORM P9000-SEND-MAP-AND-RETURN
079300        END-IF
079400        MOVE DE-CCYYMMDD         TO CONV-END-DATE-CENT
079500     ELSE
079600        MOVE WS-LOCAL-DATE-CENT  TO CONV-END-DATE-CENT
079700     END-IF
079800     IF SCR05Y-END-TIME > SPACES
079900        MOVE SCR05Y-END-TIME     TO TE-MILITARY-TIME
080000        SET TE-MILITARY-FORMAT   TO TRUE
080100        PERFORM P8997-TIMEEDIT
080200        IF TE-INVALID-TIME
080300*           'INVALID TIME ENTERED'
080400           MOVE 'I022'           TO MSGLOG-CODE
080500           MOVE -1               TO SCR05Y-END-TIME-CURSOR
080600           MOVE REV-VIDEO        TO SCR05Y-END-TIME-HI
080700           PERFORM P9000-SEND-MAP-AND-RETURN
080800        END-IF
080900        MOVE SCR05Y-END-TIME     TO CONV-END-TIME
081000     ELSE
081100        MOVE WS-LOCAL-TIME       TO CONV-END-TIME
081200     END-IF
081300
081400     IF CONV-END-DATE-TIME        > SPACE AND
081500                                  < CONV-START-DATE-TIME
081600*           'START TIME CANNOT BE GREATER THAN END TIME
081700        MOVE 'E196'           TO MSGLOG-CODE
081800        MOVE -1               TO SCR05Y-END-DATE-CURSOR
081900        MOVE REV-VIDEO        TO SCR05Y-END-DATE-HI
082000        PERFORM P9000-SEND-MAP-AND-RETURN
082100     END-IF
082200     MOVE ZEROES                 TO DATE-CONVERSION-PARMS
082300     SET  PARM-DIFF              TO TRUE
082400*** 331E
082500*    MOVE WS-LOCAL-DATE          TO PARM-PRI-DATE-GREG
082600*    MOVE WS-LOCAL-TIME          TO PARM-PRI-HRMN
082700     MOVE CONV-END-DATE          TO PARM-PRI-DATE-GREG
082800     MOVE CONV-END-TIME          TO PARM-PRI-HRMN
082900     MOVE SCR05Y-START-DATE      TO PARM-SEC-DATE-GREG
083000     MOVE SCR05Y-START-TIME      TO PARM-SEC-HRMN
083100     EXEC CICS LINK
083200               PROGRAM(P903-PGM)
083300               COMMAREA(DATE-CONVERSION-PARMS)
083400               LENGTH(P903-LGTH)
083500               RESP(WS-RESPONSE)
083600     END-EXEC
083700     MOVE WS-RESPONSE            TO FILE-STATUS
083800     IF NOT SUCCESS
083900        MOVE 'P1200-2'           TO ERR-PARAGRAPH
084000        PERFORM P9000-SEND-MAP-AND-RETURN
084100     END-IF
084200     IF PARM-RES-HRMN             > ZEROES
084300        ADD  1                   TO PARM-RES-TOT-DAYS
084400     END-IF
084500     MOVE PARM-RES-TOT-DAYS      TO WS-CALC-NBR-DAYS
084600
084700     IF SCR05Y-INCL-VIRTUAL NOT > SPACES
084800        MOVE 'N'                 TO SCR05Y-INCL-VIRTUAL
084900     ELSE
085000        IF SCR05Y-INCL-VIRTUAL = 'Y' OR 'N'
085100           CONTINUE
085200        ELSE
085300*          'MUST ENTER (Y/N)'
085400           MOVE 'M070'           TO MSGLOG-CODE
085500           MOVE -1               TO SCR05Y-INCL-VIRTUAL-CURSOR
085600           MOVE REV-VIDEO        TO SCR05Y-INCL-VIRTUAL-HI
085700           PERFORM P9000-SEND-MAP-AND-RETURN
085800        END-IF
085900     END-IF
086000     .
086100******************************************************************
086200 P1300-PAGE-UP.
086300******************************************************************
086400     MOVE ARRAY-MAX                TO SUB1
086500     INITIALIZE WS-EMP-TOD
086600     MOVE SCR05Y-EMP-NBR           TO EMPTOD-EMP-NBR
086700     PERFORM P1200-CHECK-FILTERS
086800     MOVE P05YCA-PF7-KEY           TO EMPTOD-OD-DATE-TIME
086900                                      P05YCA-PF8-KEY
087000     PERFORM P8000-STARTBR-ETOD
087100     IF SUCCESS
087200        PERFORM P8010-READNEXT-ETOD
087300        IF SUCCESS
087400           PERFORM P8060-READPREV-ETOD
087500        END-IF
087600     END-IF
087700     IF SUCCESS
087800        SET NOT-DONE               TO TRUE
087900        PERFORM UNTIL DONE
088000           PERFORM P8060-READPREV-ETOD
088100           IF SUCCESS AND EMPTOD-EMP-NBR = SCR05Y-EMP-NBR
088200           AND EMPTOD-OD-DATE-TIME   >= P05YCA-START-DATE-TIME
088300           AND EMPTOD-OFFD-DATE-TIME <= CONV-END-DATE-TIME
088400                 IF  EMPTOD-VIRTUAL-ENTRY
088500                 AND SCR05Y-INCL-VIRTUAL NOT = 'Y'
088600                    CONTINUE
088700                 ELSE
088800                    IF SUB1 = 1
088900                       SET DONE          TO TRUE
089000                       MOVE EMPTOD-OD-DATE-TIME
089100                                      TO P05YCA-PF7-KEY
089200                    END-IF
089300                    PERFORM P4100-BUILD-SCREEN-ARRAY
089400                    SUBTRACT 1         FROM SUB1
089500                 END-IF
089600           ELSE
089700              SET DONE             TO TRUE
089800              IF SUB1 > 1
089900                 MOVE SPACES       TO P05YCA-PF7-KEY
090000              END-IF
090100           END-IF
090200        END-PERFORM
090300        PERFORM P8020-ENDBR-ETOD
090400        PERFORM P4300-GET-TOTAL-TIME
065500*CNLD-249-B 9/14/23 RJA
090500**CNC0600-B
090600*        PERFORM P4400-GET-28D-TTOD
090700**CNC0600-E
              MOVE SPACES           TO SCR05Y-28DAY-TTOD
                                       SCR05Y-7DAY-RESET
065600        IF WS-CAN-WRR-NEW
065400           PERFORM P4400-GET-28D-TTOD
065400           PERFORM P4310-GET-7D-END-PERIOD
065600        END-IF
065500*CNLD-249-E 9/14/23 RJA
090800     END-IF
090900     IF P05YCA-PF7-KEY NOT > SPACES
091000*          'NO FURTHER SCROLLING POSSIBLE'
091100        MOVE 'N012' TO MSGLOG-CODE
091200     ELSE
091300*          'SCROLL SUCCESSFUL'
091400        MOVE 'S041'          TO MSGLOG-CODE
091500     END-IF.
091600******************************************************************
091700 P2000-UPDATE.
091800******************************************************************
091900     MOVE SCR05Y-FUNC-FLAG          TO FUNC-FLAG-CHECK
092000     PERFORM P3000-EDIT-SCREEN
092100     EVALUATE TRUE
092200        WHEN ADD-FUNC
092300           PERFORM P2010-ADD-ENTRY
092400        WHEN CHANGE-FUNC
092500           SET ENTRY-NOT-FOUND      TO TRUE
092600           PERFORM VARYING I FROM 1 BY 1
092700              UNTIL I > ARRAY-MAX
092800              IF SCR05Y-FUNC(I) = 'X'
092900                 IF SCR05Y-OD-DT-TM(I) NOT > SPACES
101200*C600-CR1 - B
093000*                    'NO RECORD FOUND TO DELETE'
093100*                   MOVE 'N007'     TO MSGLOG-CODE
093000*                    'NO RECORD FOUND TO CHANGE'
093100                    MOVE 'N265'     TO MSGLOG-CODE
101200*C600-CR1 - E
093200                    MOVE -1         TO SCR05Y-FUNC-CURSOR(I)
093300                    MOVE REV-VIDEO  TO SCR05Y-FUNC-HI(I)
093400                    PERFORM P9000-SEND-MAP-AND-RETURN
093500                 END-IF
093600                 SET ENTRY-FOUND    TO TRUE
101200*C600-CR1 - B
145700                 IF (SCR05Y-REST-CODE > SPACE OR
145700                     P05YCA-MODIFY-OFFD-CAN)
145700                    AND NOT (SCR05Y-TIME-SPENT-FLTR > SPACE OR
145700                             SCR05Y-AS-OF-DATE      > SPACE OR
145700                             SCR05Y-AS-OF-TIME      > SPACE OR
145700                             SCR05Y-TYPE            > SPACE)
145800                    CONTINUE
145800                 ELSE
145700                    MOVE SPACES     TO SCR05Y-TIME-SPENT-FLTR
145700                                       SCR05Y-AS-OF-DATE
145700                                       SCR05Y-AS-OF-TIME
145700                                       SCR05Y-TYPE
145700                                       SCR05Y-REST-CODE
145400                    PERFORM P2021-UNLOCK-OFFDUTY-FIELDS
101200*CNLD-196 - B
114700                    IF SCR05Y-TRAIN-RESETBRK(I) OR
114700                       SCR05Y-TRAIN-RESETNAT(I) OR
      *CNLD-242-DXC-B
114700                       SCR05Y-TRAIN-RESETSYS(I) OR
114700                       SCR05Y-TRAIN-RESETMNL(I)
      *CNLD-242-DXC-E
112200*                   'RESET BREAK RECORD CANNOT BE CHANGED'
112300                       MOVE 'R136'     TO MSGLOG-CODE
112400                       MOVE -1         TO SCR05Y-FUNC-CURSOR(I)
112500                       MOVE REV-VIDEO  TO SCR05Y-TRAIN-HI(I)
112500                                          SCR05Y-FUNC-HI(I)
112600                       PERFORM P9000-SEND-MAP-AND-RETURN
114700                    END-IF
101200*CNLD-196 - E
094900*                   'PLEASE MODIFY OFF-DUTY TIME OR
094900*                    SELECT NEW REST AND PRESS F5'
095100                    MOVE 'P288'  TO MSGLOG-CODE
095200                    MOVE -1      TO SCR05Y-REST-CODE-CURSOR
095300                    MOVE REV-VIDEO
095300                                 TO SCR05Y-FUNC-HI(I)
095300                                    SCR05Y-OFF-DT-TM-HI(I)
095300                                    SCR05Y-REST-CODE-HI
095400                    PERFORM P9000-SEND-MAP-AND-RETURN
145800                 END-IF
101200*C600-CR1 - E
093700                 PERFORM P2020-UPDATE-ENTRY
093800              ELSE
093900                 IF SCR05Y-FUNC(I) > SPACES
094000*                      'INVALID ENTRY CODE'
094100                    MOVE 'I041'     TO MSGLOG-CODE
094200                    MOVE -1         TO SCR05Y-FUNC-CURSOR(I)
094300                    MOVE REV-VIDEO  TO SCR05Y-FUNC-HI(I)
094400                    PERFORM P9000-SEND-MAP-AND-RETURN
094500                 END-IF
094600              END-IF
094700           END-PERFORM
094800           IF ENTRY-NOT-FOUND
101200*C600-CR1 - B
145700              MOVE SPACES           TO SCR05Y-TIME-SPENT-FLTR
145700                                       SCR05Y-AS-OF-DATE
145700                                       SCR05Y-AS-OF-TIME
145700                                       SCR05Y-TYPE
145700                                       SCR05Y-REST-CODE
114000              MOVE REV-VIDEO        TO SCR05Y-REST-CODE-HI
145700              PERFORM P2021-UNLOCK-OFFDUTY-FIELDS
094900*              'PLEASE SELECT A RECORD;
094900*               MODIFY OFF-DUTY OR SELECT NEW REST; AND F5'
101200*C600-CR1 - E
095100              MOVE 'P191'           TO MSGLOG-CODE
095200              MOVE -1               TO SCR05Y-FUNC-CURSOR(1)
095300              MOVE REV-VIDEO        TO SCR05Y-FUNC-HI(1)
095400              PERFORM P9000-SEND-MAP-AND-RETURN
095500           END-IF
095600        WHEN DELETE-FUNC
095700           SET ENTRY-NOT-FOUND      TO TRUE
095800           PERFORM VARYING I FROM 1 BY 1
095900              UNTIL I > ARRAY-MAX
096000              IF SCR05Y-FUNC(I) = 'X'
096100                 SET ENTRY-FOUND    TO TRUE
096200                 IF SCR05Y-OD-DT-TM(I) NOT > SPACES
096300*                    'NO RECORD FOUND TO DELETE'
096400                    MOVE 'N007'     TO MSGLOG-CODE
096500                    MOVE -1         TO SCR05Y-FUNC-CURSOR(I)
096600                    MOVE REV-VIDEO  TO SCR05Y-FUNC-HI(I)
096700                    PERFORM P9000-SEND-MAP-AND-RETURN
096800                 END-IF
096900                 PERFORM P2030-DELETE-ENTRY
097000              END-IF
097100           END-PERFORM
097200           IF ENTRY-NOT-FOUND
097300*                'NO RECORD FOUND TO DELETE'
097400              MOVE 'N007'           TO MSGLOG-CODE
097500              MOVE -1               TO SCR05Y-FUNC-CURSOR(1)
097600              MOVE REV-VIDEO        TO SCR05Y-FUNC-HI(1)
097700              PERFORM P9000-SEND-MAP-AND-RETURN
097800           END-IF
097900     END-EVALUATE
098000     MOVE SPACES                    TO SCR05Y-FUNC-FLAG
098100                                       SCR05Y-TIME-SPENT-FLTR
098200                                       SCR05Y-AS-OF-DATE
098300                                       SCR05Y-AS-OF-TIME
098400                                       SCR05Y-REST-CODE
098500*CNC0600 - RJA - B
098600                                       SCR05Y-TYPE
098700*CNC0600 - RJA - E
098800                                       FUNC-FLAG-CHECK.
098900******************************************************************
099000 P2010-ADD-ENTRY.
099100******************************************************************
099200     MOVE SPACES                    TO WS-EMP-TOD
099300     MOVE SCR05Y-EMP-NBR            TO EMPTOD-EMP-NBR
099400
099500     MOVE SCR05Y-AS-OF-DATE         TO DE-YYMMDD
099600     SET DE-YYMMDD-FORMAT           TO TRUE
099700     PERFORM P8998-DATEEDIT
099800     MOVE DE-CCYYMMDD               TO EMPTOD-OD-DATE-CENT
099900     MOVE SCR05Y-AS-OF-TIME         TO EMPTOD-OD-TIME
100000
100100     INITIALIZE WS-MSTR
100200     MOVE SCR05Y-EMP-NBR            TO MSTRNBRK
100300     PERFORM P5000-READ-MASTER
100400     IF NOT SUCCESS
100500        MOVE 'P2010-1'              TO ERR-PARAGRAPH
100600        MOVE MSTRNBRK               TO ERR-KEY
100700        PERFORM P9999-GOT-PROBLEM
100800     END-IF
100900     MOVE DIST OF WS-MSTR           TO EMPTOD-DIST
101000     MOVE SUB-DIST OF WS-MSTR       TO EMPTOD-SDIST
101200*C600-CR1 - B
097100*    MOVE 'MANUAL'                  TO EMPTOD-TRAIN-ASGN
101200*C600-CR1 - E
101200*CNC0600 - RJA - B
101300     EVALUATE TRUE
101400        WHEN SCR05Y-TYPE-CONTACT
101700           SET EMPTOD-CONTACT       TO TRUE
101600        WHEN SCR05Y-TYPE-COMMUTE
101700           SET EMPTOD-COMMUTE       TO TRUE
101200*C600-CR1 - B
101600        WHEN SCR05Y-TYPE-MANUAL
101700           SET EMPTOD-MANUAL        TO TRUE
101200*C600-CR1 - E
101800     END-EVALUATE
101900*CNC0600 - RJA - E
102000     MOVE CRAFT OF WS-MSTR          TO EMPTOD-CRAFT
102100     MOVE SCR05Y-REST-CODE          TO EMPTOD-REST-STATUS-CODE
102200
102200*CNLD-260-RJA-B
102200     MOVE PSTCA-TIME-ZONE           TO EMPTOD-TIME-ZONE
102200*CNLD-260-RJA-E
102200
102300     INITIALIZE DATE-CONVERSION-PARMS
102400     SET PARM-ADD                   TO TRUE
102500     MOVE SCR05Y-AS-OF-DATE         TO PARM-PRI-DATE-GREG
102600     MOVE SCR05Y-AS-OF-TIME         TO PARM-PRI-HRMN
102700     MOVE SCR05Y-TIME-SPENT-FLTR    TO PARM-SEC-HRMN
102800     PERFORM P9300-LINK-P903
102900     MOVE PARM-RES-GREG-CENT        TO EMPTOD-OFFD-CE
103000     MOVE PARM-RES-DATE-GREG        TO EMPTOD-OFFD-DATE
103100     MOVE PARM-RES-HRMN             TO EMPTOD-OFFD-TIME
103200     SET EMPTOD-MANUALLY-UPDATED    TO TRUE
103300
103400     MOVE EMPTOD-KEY-AREA           TO EMPTOD-EMPKEY
103500     EXEC CICS WRITE
103600               DATASET(EMPTOD-VIA-EMP)
103700               FROM(WS-EMP-TOD)
103800               LENGTH(EMPTOD-EMP-RLGTH)
103900               RIDFLD(EMPTOD-EMPKEY)
104000               KEYLENGTH(EMPTOD-EMP-KLGTH)
104100               RESP(WS-RESPONSE)
104200     END-EXEC
104300     MOVE WS-RESPONSE TO FILE-STATUS
104400     IF NOT SUCCESS
104500        IF DUP-KEY-ERR
104600*             'RECORD ALREADY EXISTS FOR THIS DATE/TIME'
104700           MOVE 'R008'              TO MSGLOG-CODE
104800           MOVE -1                  TO SCR05Y-AS-OF-DATE-CURSOR
104900           MOVE REV-VIDEO           TO SCR05Y-AS-OF-DATE-HI
105000                                       SCR05Y-AS-OF-TIME-HI
105100           PERFORM P9000-SEND-MAP-AND-RETURN
105200        ELSE
105300           MOVE 'P2010-1'           TO ERR-PARAGRAPH
105400           MOVE EMPTOD-EMPKEY       TO ERR-KEY
105500           PERFORM P9999-GOT-PROBLEM
105600        END-IF
105700     END-IF
105800*
105900*    WRITE EMPLOYEE HISTORY
106000*
106100     MOVE SPACES                TO P943-COMMAREA-PARMS
106200     MOVE SCR05Y-AS-OF-DATE     TO P943-E-DATE(1:6)
106300     MOVE SCR05Y-AS-OF-TIME     TO P943-E-TOD
106500*C600-CR1 - P943-FUN70-OLD-TOD SHOULD BE POPULATED STILL AS IT
106500*C600-CR1 - WILL BE USED IN CNP738.
106400     MOVE '0000'                 TO P943-FUN70-OLD-TOD
106500     MOVE SCR05Y-TIME-SPENT-FLTR TO P943-FUN70-NEW-TOD
101200*CNC0600 - RJA - B
106500     MOVE EMPTOD-TRAIN-ASGN      TO P943-TEMP-ASGN-TRAIN
101200*CNC0600 - RJA - E
101200*C600-CR1 - B
106500     SET P943-FUN70-ADD             TO TRUE
106500     MOVE EMPTOD-REST-STATUS-CODE   TO P943-FUN70-NEW-REST-STATUS
101200*C600-CR1 - E
106600     PERFORM P8900-WRITE-HISTORY
106700*       'UPDATE WAS SUCCESSFUL'
106800     MOVE 'U001'                    TO MSGLOG-CODE.
106900******************************************************************
107000 P2020-UPDATE-ENTRY.
107100******************************************************************
101200*CNLD-196 - B
114700     IF SCR05Y-TRAIN-RESETBRK(I) OR
114700        SCR05Y-TRAIN-RESETNAT(I) OR
      *CNLD-242-DXC-B
114700        SCR05Y-TRAIN-RESETSYS(I) OR
              SCR05Y-TRAIN-RESETMNL(I)
      *CNLD-242-DXC-E
112200*          'RESET BREAK RECORD CANNOT BE CHANGED'
112300        MOVE 'R136'                 TO MSGLOG-CODE
112400        MOVE -1                     TO SCR05Y-FUNC-CURSOR(I)
112500        MOVE REV-VIDEO              TO SCR05Y-TRAIN-HI(I)
112500                                       SCR05Y-FUNC-HI(I)
112600        PERFORM P9000-SEND-MAP-AND-RETURN
114700     END-IF
101200*CNLD-196 - E
112000
107200*
107300*    VALIDATE NEW OFF DUTY DATE/TIME
107400*
107500     MOVE SCR05Y-OFF-DT-TM(I)       TO WS-FORMATTED-DT-TM
107600     SET DE-YYMMDD-FORMAT           TO TRUE
107700     MOVE WS-FORMATTED-YR           TO DE-YYMMDD-YY
107800     MOVE WS-FORMATTED-MO           TO DE-YYMMDD-MM
107900     MOVE WS-FORMATTED-DY           TO DE-YYMMDD-DD
108000     PERFORM P8998-DATEEDIT
108100     IF DE-INVALID-DATE
108200*          'INVALID DATE/TIME - MUST BE IN YY/MM/DD HH:MM FORMAT'
108300        MOVE 'I458'                 TO MSGLOG-CODE
108400        MOVE -1                     TO SCR05Y-OFF-DT-TM-CURSOR(I)
108500        MOVE REV-VIDEO              TO SCR05Y-OFF-DT-TM-HI(I)
108600        PERFORM P9000-SEND-MAP-AND-RETURN
108700     END-IF
108800     MOVE DE-CCYYMMDD               TO DE-COMPARE1-DATE
108900
109000     SET TE-MILITARY-FORMAT         TO TRUE
109100     MOVE WS-FORMATTED-HR           TO TE-MILITARY-HR
109200     MOVE WS-FORMATTED-MN           TO TE-MILITARY-MN
109300     PERFORM P8997-TIMEEDIT
109400     IF TE-INVALID-TIME
109500*          'INVALID DATE/TIME - MUST BE IN YY/MM/DD HH:MM FORMAT'
109600        MOVE 'I458'                 TO MSGLOG-CODE
109700        MOVE -1                     TO SCR05Y-OFF-DT-TM-CURSOR(I)
109800        MOVE REV-VIDEO              TO SCR05Y-OFF-DT-TM-HI(I)
109900        PERFORM P9000-SEND-MAP-AND-RETURN
110000     END-IF
110100     MOVE TE-MILITARY-TIME          TO DE-COMPARE1-TIME
110200*
110300*    MAKE SURE TIME ON DUTY DOESN'T EXCEED 16 HOURS
110400*
110500     MOVE SPACES                    TO WS-EMP-TOD
110600     MOVE SCR05Y-EMP-NBR            TO EMPTOD-EMP-NBR
110700     MOVE SCR05Y-OD-DT-TM(I)        TO WS-FORMATTED-DT-TM
110800
110900     SET DE-YYMMDD-FORMAT           TO TRUE
111000     MOVE WS-FORMATTED-YR           TO DE-YYMMDD-YY
111100     MOVE WS-FORMATTED-MO           TO DE-YYMMDD-MM
111200     MOVE WS-FORMATTED-DY           TO DE-YYMMDD-DD
111300     PERFORM P8998-DATEEDIT
111400     MOVE DE-CCYYMMDD               TO EMPTOD-OD-DATE-CENT
111500                                       DE-COMPARE2-DATE
111600
111700     MOVE WS-FORMATTED-HR           TO EMPTOD-OD-HR
111800     MOVE WS-FORMATTED-MN           TO EMPTOD-OD-MN
111900     MOVE EMPTOD-OD-TIME            TO DE-COMPARE2-TIME
112000
112100     IF DE-COMPARE2-DATE-TIME > DE-COMPARE1-DATE-TIME
112200*          'OFF DUTY DAY-TIME CANNOT PRECEDE ON DUTY DAY-TIME'
112300        MOVE 'O035'                 TO MSGLOG-CODE
112400        MOVE -1                     TO SCR05Y-OFF-DT-TM-CURSOR(I)
112500        MOVE REV-VIDEO              TO SCR05Y-OFF-DT-TM-HI(I)
112600        PERFORM P9000-SEND-MAP-AND-RETURN
112700     END-IF
112800
112900     INITIALIZE DATE-CONVERSION-PARMS
113000     SET PARM-DIFF                  TO TRUE
113100     MOVE DE-COMPARE2-YYMMDD        TO PARM-PRI-DATE-GREG
113200     MOVE DE-COMPARE2-TIME          TO PARM-PRI-HRMN
113300     MOVE DE-COMPARE1-YYMMDD        TO PARM-SEC-DATE-GREG
113400     MOVE DE-COMPARE1-TIME          TO PARM-SEC-HRMN
113500     PERFORM P9300-LINK-P903
113600     IF PARM-RES-TOT-DAYS > 0
113700        OR PARM-RES-HRMN > 1600
113800*          'TIME SPENT MAY NOT EXCEED 16 HOURS'
113900        MOVE 'T214'              TO MSGLOG-CODE
114000        MOVE -1                  TO SCR05Y-OFF-DT-TM-CURSOR(I)
114100        MOVE REV-VIDEO           TO SCR05Y-OFF-DT-TM-HI(I)
114200        PERFORM P9000-SEND-MAP-AND-RETURN
114300     END-IF
114400     MOVE PARM-RES-HRMN          TO SAVE-TOD-HRMN
114500*
114600     MOVE EMPTOD-KEY-AREA           TO EMPTOD-EMPKEY
101200*C600-CR1 - B
114700     PERFORM P8070-READ-ETOD
114700     IF DE-COMPARE1-DATE-TIME = EMPTOD-OFFD-DATE-TIME AND
114700        (SCR05Y-REST-CODE     = EMPTOD-REST-STATUS-CODE OR
114700         SCR05Y-REST-CODE     = SPACE)
113800*          'NO CHANGES FOUND IN THE SELECTION;
113800*           CHANGE OFF-DUTY OR SELECT NEW REST'
113900        MOVE 'N266'              TO MSGLOG-CODE
114000        MOVE -1                  TO SCR05Y-REST-CODE-CURSOR
114100        MOVE REV-VIDEO           TO SCR05Y-FUNC-HI(I)
114100                                    SCR05Y-OFF-DT-TM-HI(I)
114100                                    SCR05Y-CALL-STATUS-HI(I)
114100                                    SCR05Y-REST-CODE-HI
145700        PERFORM P2021-UNLOCK-OFFDUTY-FIELDS
114200        PERFORM P9000-SEND-MAP-AND-RETURN
114700     END-IF
101200*C600-CR1 - E
114700     PERFORM P8030-READ-UPDATE-ETOD
114800     MOVE DE-COMPARE1-DATE-TIME     TO EMPTOD-OFFD-DATE-TIME
114900     SET EMPTOD-MANUALLY-UPDATED    TO TRUE
101200*C600-CR1 - B
115100     MOVE EMPTOD-REST-STATUS-CODE   TO SAVE-OLD-REST-STATUS
101200*C600-CR1 - E
115000     IF SCR05Y-REST-CODE             > SPACE
115100        MOVE SCR05Y-REST-CODE       TO EMPTOD-REST-STATUS-CODE
115200     END-IF
115300     PERFORM P8040-REWRITE-ETOD
115400
101200*C600-CR1 - B
114100     MOVE SPACE                     TO SCR05Y-FUNC(I)
101200*C600-CR1 - E
115400
115500*
115600*    WRITE EMPLOYEE HISTORY
115700*
115800     MOVE SPACES                    TO P943-COMMAREA-PARMS
115900     MOVE SCR05Y-OD-DT-TM(I)(1:2)   TO P943-E-DATE(1:2)
116000     MOVE SCR05Y-OD-DT-TM(I)(4:2)   TO P943-E-DATE(3:2)
116100     MOVE SCR05Y-OD-DT-TM(I)(7:2)   TO P943-E-DATE(5:2)
116200     MOVE SCR05Y-OD-DT-TM(I)(10:2)  TO P943-E-TOD(1:2)
116300     MOVE SCR05Y-OD-DT-TM(I)(13:2)  TO P943-E-TOD(3:2)
116400     MOVE SCR05Y-TIME-SPENT(I)(1:2) TO P943-FUN70-OLD-TOD(1:2)
116500     MOVE SCR05Y-TIME-SPENT(I)(4:2) TO P943-FUN70-OLD-TOD(3:2)
116600     MOVE SAVE-TOD-HRMN             TO P943-FUN70-NEW-TOD
101200*C600-CR1 - B
116400     MOVE SAVE-TOD-HRMN(1:2)        TO SCR05Y-TIME-SPENT(I)(1:2)
116500     MOVE SAVE-TOD-HRMN(3:2)        TO SCR05Y-TIME-SPENT(I)(4:2)
106500     MOVE SAVE-OLD-REST-STATUS      TO P943-FUN70-OLD-REST-STATUS
106500     MOVE EMPTOD-REST-STATUS-CODE   TO P943-FUN70-NEW-REST-STATUS
148700     EVALUATE TRUE
148800        WHEN EMPTOD-REST-GREEN
148900           MOVE WS-GREEN            TO SCR05Y-CALL-STATUS(I)
149000        WHEN EMPTOD-REST-YELLOW
149100           MOVE WS-YELLOW           TO SCR05Y-CALL-STATUS(I)
149200        WHEN EMPTOD-REST-RED
149300           MOVE WS-RED              TO SCR05Y-CALL-STATUS(I)
149800     END-EVALUATE
106500     SET P943-FUN70-CHG             TO TRUE
101200*C600-CR1 - E
101200*CNC0600 - RJA - B
106500     MOVE EMPTOD-TRAIN-ASGN         TO P943-TEMP-ASGN-TRAIN
101200*CNC0600 - RJA - E
116700     PERFORM P8900-WRITE-HISTORY.
101200*C600-CR1 - B
116800******************************************************************
116900 P2021-UNLOCK-OFFDUTY-FIELDS.
117000******************************************************************
145400     IF NOT P05YCA-MODIFY-OFFD-CAN
145500        SET P05YCA-MODIFY-OFFD-CAN TO TRUE
031800        PERFORM VARYING SUB1 FROM 1 BY 1
031900          UNTIL SUB1 > ARRAY-MAX
032000           IF SCR05Y-OFF-DT-TM(SUB1) > SPACE
032100              MOVE MDT
032200                TO SCR05Y-OFF-DT-TM-ATTR(SUB1)
032200              MOVE WHITE
032200                TO SCR05Y-OFF-DT-TM-COLOR(SUB1)
145800           END-IF
033100        END-PERFORM
145800     END-IF.
101200*C600-CR1 - E
116800******************************************************************
116900 P2030-DELETE-ENTRY.
117000******************************************************************
101200*CNLD-196 - B
114700     IF SCR05Y-TRAIN-RESETBRK(I) OR
114700        SCR05Y-TRAIN-RESETNAT(I) OR
      *CNLD-242-DXC-B
114700        SCR05Y-TRAIN-RESETSYS(I) OR
              SCR05Y-TRAIN-RESETMNL(I)
      *CNLD-242-DXC-E
112200*          'RESET BREAK RECORD CANNOT BE MANUALLY DELETED'
112300        MOVE 'R137'                 TO MSGLOG-CODE
112400        MOVE -1                     TO SCR05Y-FUNC-CURSOR(I)
112500        MOVE REV-VIDEO              TO SCR05Y-TRAIN-HI(I)
112500                                       SCR05Y-FUNC-HI(I)
112600        PERFORM P9000-SEND-MAP-AND-RETURN
114700     END-IF
101200*CNLD-196 - E
117100     MOVE SPACES                    TO WS-EMP-TOD
117200     MOVE SCR05Y-EMP-NBR            TO EMPTOD-EMP-NBR
117300     MOVE SCR05Y-OD-DT-TM(I)        TO WS-FORMATTED-DT-TM
117400
117500     SET DE-YYMMDD-FORMAT           TO TRUE
117600     MOVE WS-FORMATTED-YR           TO DE-YYMMDD-YY
117700     MOVE WS-FORMATTED-MO           TO DE-YYMMDD-MM
117800     MOVE WS-FORMATTED-DY           TO DE-YYMMDD-DD
117900     PERFORM P8998-DATEEDIT
118000     MOVE DE-CCYYMMDD               TO EMPTOD-OD-DATE-CENT
118100
118200     MOVE WS-FORMATTED-HR           TO EMPTOD-OD-HR
118300     MOVE WS-FORMATTED-MN           TO EMPTOD-OD-MN
118400*
118500     MOVE EMPTOD-KEY-AREA           TO EMPTOD-EMPKEY
101200*CNC0600 - RJA - B
118500     PERFORM P8070-READ-ETOD
101200*CNC0600 - RJA - E
118600     PERFORM P8050-DELETE-ETOD
118700*
118800*    WRITE EMPLOYEE HISTORY
118900*
119000     MOVE SPACES                    TO P943-COMMAREA-PARMS
119100     MOVE SCR05Y-OD-DT-TM(I)(1:2)   TO P943-E-DATE(1:2)
119200     MOVE SCR05Y-OD-DT-TM(I)(4:2)   TO P943-E-DATE(3:2)
119300     MOVE SCR05Y-OD-DT-TM(I)(7:2)   TO P943-E-DATE(5:2)
119400     MOVE SCR05Y-OD-DT-TM(I)(10:2)  TO P943-E-TOD(1:2)
119500     MOVE SCR05Y-OD-DT-TM(I)(13:2)  TO P943-E-TOD(3:2)
119600     MOVE SCR05Y-TIME-SPENT(I)(1:2) TO P943-FUN70-OLD-TOD(1:2)
119700     MOVE SCR05Y-TIME-SPENT(I)(4:2) TO P943-FUN70-OLD-TOD(3:2)
106500*C600-CR1 - P943-FUN70-NEW-TOD SHOULD BE POPULATED STILL AS IT
106500*C600-CR1 - WILL BE USED IN CNP738.
119800     MOVE '0000'                    TO P943-FUN70-NEW-TOD
101200*CNC0600 - RJA - B
106500     MOVE EMPTOD-TRAIN-ASGN         TO P943-TEMP-ASGN-TRAIN
101200*CNC0600 - RJA - E
101200*C600-CR1 - B
106500     MOVE EMPTOD-REST-STATUS-CODE   TO P943-FUN70-OLD-REST-STATUS
106500     SET P943-FUN70-DEL             TO TRUE
101200*C600-CR1 - E
119900     PERFORM P8900-WRITE-HISTORY.
120000******************************************************************
120100 P3000-EDIT-SCREEN.
120200******************************************************************
120300     IF SCR05Y-EMP-NBR NOT > SPACES
120400        MOVE SPACES                 TO SCR05Y-EMP-NBR
120500     ELSE
120600        MOVE SCR05Y-EMP-NBR         TO BIF-FIELD
120700        MOVE +9                     TO BIF-LEN
120800        PERFORM P8999-BIFEDIT
120900        IF BIF-ERROR
121000*          INVALID-EMPLOYEE-NUMBER MSG
121100           MOVE 'I036'              TO MSGLOG-CODE
121200           MOVE -1                  TO SCR05Y-EMP-NBR-CURSOR
121300           MOVE REV-VIDEO           TO SCR05Y-EMP-NBR-HI
121400        END-IF
121500        MOVE BIF-EDITED-FIELD       TO SCR05Y-EMP-NBR
121600     END-IF
121700
121800     IF SCR05Y-START-DATE > SPACES
121900        MOVE SCR05Y-START-DATE   TO DE-YYMMDD
122000        SET DE-YYMMDD-FORMAT     TO TRUE
122100        PERFORM P8998-DATEEDIT
122200        IF DE-INVALID-DATE
122300*           'INVALID DATE ENTERED'
122400           MOVE 'I034'           TO MSGLOG-CODE
122500           MOVE -1               TO SCR05Y-START-DATE-CURSOR
122600           MOVE REV-VIDEO        TO SCR05Y-START-DATE-HI
122700           PERFORM P9000-SEND-MAP-AND-RETURN
122800        END-IF
122900        MOVE DE-CCYYMMDD         TO CONV-START-DATE-CENT
123000     END-IF
123100
123200     IF SCR05Y-START-TIME > SPACES
123300        MOVE SCR05Y-START-TIME   TO TE-MILITARY-TIME
123400        SET TE-MILITARY-FORMAT   TO TRUE
123500        PERFORM P8997-TIMEEDIT
123600        IF TE-INVALID-TIME
123700*           'INVALID TIME ENTERED'
123800           MOVE 'I022'           TO MSGLOG-CODE
123900           MOVE -1               TO SCR05Y-START-TIME-CURSOR
124000           MOVE REV-VIDEO        TO SCR05Y-START-TIME-HI
124100           PERFORM P9000-SEND-MAP-AND-RETURN
124200        END-IF
124300        MOVE SCR05Y-START-TIME   TO CONV-START-TIME
124400     END-IF
124500
124600     IF SCR05Y-END-DATE > SPACES
124700        MOVE SCR05Y-END-DATE     TO DE-YYMMDD
124800        SET DE-YYMMDD-FORMAT     TO TRUE
124900        PERFORM P8998-DATEEDIT
125000        IF DE-INVALID-DATE
125100*           'INVALID DATE ENTERED'
125200           MOVE 'I034'           TO MSGLOG-CODE
125300           MOVE -1               TO SCR05Y-END-DATE-CURSOR
125400           MOVE REV-VIDEO        TO SCR05Y-END-DATE-HI
125500           PERFORM P9000-SEND-MAP-AND-RETURN
125600        END-IF
125700        MOVE DE-CCYYMMDD         TO CONV-END-DATE-CENT
125800     END-IF
125900     IF SCR05Y-END-TIME > SPACES
126000        MOVE SCR05Y-END-TIME     TO TE-MILITARY-TIME
126100        SET TE-MILITARY-FORMAT   TO TRUE
126200        PERFORM P8997-TIMEEDIT
126300        IF TE-INVALID-TIME
126400*           'INVALID TIME ENTERED'
126500           MOVE 'I022'           TO MSGLOG-CODE
126600           MOVE -1               TO SCR05Y-END-TIME-CURSOR
126700           MOVE REV-VIDEO        TO SCR05Y-END-TIME-HI
126800           PERFORM P9000-SEND-MAP-AND-RETURN
126900        END-IF
127000        MOVE SCR05Y-END-TIME     TO CONV-END-TIME
127100     END-IF
127200
127300     IF CONV-END-DATE-TIME        > SPACE AND
127400                                  < CONV-START-DATE-TIME
127500*           'START TIME CANNOT BE GREATER THAN END TIME
127600        MOVE 'E196'           TO MSGLOG-CODE
127700        MOVE -1               TO SCR05Y-END-DATE-CURSOR
127800        MOVE REV-VIDEO        TO SCR05Y-END-DATE-HI
127900        PERFORM P9000-SEND-MAP-AND-RETURN
128000     END-IF
128100
128200     IF SCR05Y-FUNC-FLAG > SPACES
128300        MOVE SCR05Y-FUNC-FLAG    TO FUNC-FLAG-CHECK
128400        IF PFKEY5
128500           IF NOT VALID-FUNC-FLAG
128600*                'VALID ENTRY IS 'A', 'C', OR 'D''
128700              MOVE 'V149'        TO MSGLOG-CODE
128800              MOVE -1            TO SCR05Y-FUNC-FLAG-CURSOR
128900              MOVE REV-VIDEO     TO SCR05Y-FUNC-FLAG-HI
129000              PERFORM P9000-SEND-MAP-AND-RETURN
129100           END-IF
129200        END-IF
129300     ELSE
129400        IF PFKEY5
129500*             'VALID ENTRY IS 'A', 'C', OR 'D''
129600           MOVE 'V149'           TO MSGLOG-CODE
129700           MOVE -1               TO SCR05Y-FUNC-FLAG-CURSOR
129800           MOVE REV-VIDEO        TO SCR05Y-FUNC-FLAG-HI
129900           PERFORM P9000-SEND-MAP-AND-RETURN
130000        END-IF
130100     END-IF
130200
130300     IF SCR05Y-TIME-SPENT-FLTR > SPACES
130400        AND PFKEY5
130500        AND ADD-FUNC
139100*CNC0600 - RJA - B
141100        IF SCR05Y-TYPE-CONTACT
046200           IF SCR05Y-TIME-SPENT-FLTR   NOT NUMERIC
046300              MOVE SCR05Y-TIME-SPENT-FLTR TO BIF-FIELD
046400              MOVE +4                  TO BIF-LEN
046500              PERFORM P8999-BIFEDIT
046600              IF BIF-ERROR
130600*              'INVALID TIME ENTERED'
130700                 MOVE 'I022'     TO MSGLOG-CODE
130800                 MOVE -1         TO SCR05Y-TIME-SPENT-FLTR-CURSOR
140900                 MOVE REV-VIDEO  TO SCR05Y-TIME-SPENT-FLTR-HI
141000                 PERFORM P9000-SEND-MAP-AND-RETURN
047300              END-IF
047400              MOVE BIF-EDITED-FIELD    TO SCR05Y-TIME-SPENT-FLTR
047500           END-IF
139100           IF SCR05Y-TIME-SPENT-FLTR-NUM > ZERO
130600              MOVE SCR05Y-TIME-SPENT-FLTR TO TE-MILITARY-TIME
130700              SET TE-MILITARY-FORMAT      TO TRUE
130800              PERFORM P8997-TIMEEDIT
130900              IF TE-INVALID-TIME
131000*              'INVALID TIME ENTERED'
131100                 MOVE 'I022'     TO MSGLOG-CODE
131200                 MOVE -1         TO SCR05Y-TIME-SPENT-FLTR-CURSOR
131300                 MOVE REV-VIDEO  TO SCR05Y-TIME-SPENT-FLTR-HI
131400                 PERFORM P9000-SEND-MAP-AND-RETURN
131500              END-IF
131500           END-IF
141100        ELSE
139100*CNC0600 - RJA - E
130600           MOVE SCR05Y-TIME-SPENT-FLTR TO TE-MILITARY-TIME
130700           SET TE-MILITARY-FORMAT      TO TRUE
130800           PERFORM P8997-TIMEEDIT
130900           IF TE-INVALID-TIME
131000*           'INVALID TIME ENTERED'
131100              MOVE 'I022'        TO MSGLOG-CODE
131200              MOVE -1            TO SCR05Y-TIME-SPENT-FLTR-CURSOR
131300              MOVE REV-VIDEO     TO SCR05Y-TIME-SPENT-FLTR-HI
131400              PERFORM P9000-SEND-MAP-AND-RETURN
131500           END-IF
139100*CNC0600 - RJA - B
141100        END-IF
139100*CNC0600 - RJA - E
130900
131600        IF SCR05Y-TIME-SPENT-FLTR > 1600
131700*             'TIME SPENT MAY NOT EXCEED 16 HOURS'
131800           MOVE 'T214'           TO MSGLOG-CODE
131900           MOVE -1               TO SCR05Y-TIME-SPENT-FLTR-CURSOR
132000           MOVE REV-VIDEO        TO SCR05Y-TIME-SPENT-FLTR-HI
132100           PERFORM P9000-SEND-MAP-AND-RETURN
132200        END-IF
132300     ELSE
132400        IF PFKEY5
132500           AND ADD-FUNC
139100*CNC0600 - RJA - B
131600           IF SCR05Y-TYPE-CONTACT
131600              MOVE '0000'        TO SCR05Y-TIME-SPENT-FLTR
141100           ELSE
139100*CNC0600 - RJA - E
132600*             'INVALID TIME ENTERED'
132700              MOVE 'I022'        TO MSGLOG-CODE
132800              MOVE -1            TO SCR05Y-TIME-SPENT-FLTR-CURSOR
132900              MOVE REV-VIDEO     TO SCR05Y-TIME-SPENT-FLTR-HI
133000              PERFORM P9000-SEND-MAP-AND-RETURN
139100*CNC0600 - RJA - B
141100           END-IF
139100*CNC0600 - RJA - E
133100        END-IF
133200     END-IF
133300
133400     IF SCR05Y-AS-OF-DATE > SPACES
133500        MOVE SCR05Y-AS-OF-DATE   TO DE-YYMMDD
133600        SET DE-YYMMDD-FORMAT     TO TRUE
133700        PERFORM P8998-DATEEDIT
133800        IF DE-INVALID-DATE
133900*           'INVALID DATE ENTERED'
134000           MOVE 'I034'           TO MSGLOG-CODE
134100           MOVE -1               TO SCR05Y-AS-OF-DATE-CURSOR
134200           MOVE REV-VIDEO        TO SCR05Y-AS-OF-DATE-HI
134300           PERFORM P9000-SEND-MAP-AND-RETURN
134400        END-IF
134500     ELSE
134600        IF PFKEY5
134700           AND ADD-FUNC
134800*             'INVALID DATE ENTERED'
134900           MOVE 'I034'           TO MSGLOG-CODE
135000           MOVE -1               TO SCR05Y-AS-OF-DATE-CURSOR
135100           MOVE REV-VIDEO        TO SCR05Y-AS-OF-DATE-HI
135200           PERFORM P9000-SEND-MAP-AND-RETURN
135300        END-IF
135400     END-IF
135500
135600     IF SCR05Y-AS-OF-TIME > SPACES
135700        MOVE SCR05Y-AS-OF-TIME TO TE-MILITARY-TIME
135800        SET TE-MILITARY-FORMAT      TO TRUE
135900        PERFORM P8997-TIMEEDIT
136000        IF TE-INVALID-TIME
136100*           'INVALID TIME ENTERED'
136200           MOVE 'I022'           TO MSGLOG-CODE
136300           MOVE -1               TO SCR05Y-AS-OF-TIME-CURSOR
136400           MOVE REV-VIDEO        TO SCR05Y-AS-OF-TIME-HI
136500           PERFORM P9000-SEND-MAP-AND-RETURN
136600        END-IF
136700     ELSE
136800        IF PFKEY5
136900           AND ADD-FUNC
137000*             'INVALID TIME ENTERED'
137100           MOVE 'I022'           TO MSGLOG-CODE
137200           MOVE -1               TO SCR05Y-AS-OF-TIME-CURSOR
137300           MOVE REV-VIDEO        TO SCR05Y-AS-OF-TIME-HI
137400           PERFORM P9000-SEND-MAP-AND-RETURN
137500        END-IF
137600     END-IF
137700
137800     IF SCR05Y-REST-CODE         <  SPACE
137900        MOVE SPACE               TO SCR05Y-REST-CODE
138000     END-IF
138100     IF NOT SCR05Y-REST-CODE-VALID
138200        IF ADD-FUNC OR
138300          (CHANGE-FUNC AND SCR05Y-REST-CODE NOT = SPACE)
138400           MOVE 'I020'           TO MSGLOG-CODE
138500           MOVE -1               TO SCR05Y-REST-CODE-CURSOR
138600           MOVE REV-VIDEO        TO SCR05Y-REST-CODE-HI
138700           PERFORM P9000-SEND-MAP-AND-RETURN
138800        END-IF
138900     END-IF
139000
139100*CNC0600 - RJA - B
139200     IF SCR05Y-TYPE              <  SPACE
139300        MOVE SPACE               TO SCR05Y-TYPE
139400     END-IF
139500     IF PFKEY5
139600        IF ADD-FUNC AND
139700           (NOT SCR05Y-TYPE-VALID)
139800*          'INVALID SELECTION'
139900           MOVE 'I015'           TO MSGLOG-CODE
140000           MOVE -1               TO SCR05Y-TYPE-CURSOR
140100           MOVE REV-VIDEO        TO SCR05Y-TYPE-HI
140200           PERFORM P9000-SEND-MAP-AND-RETURN
140300        END-IF
141200     END-IF
141300*CNC0600 - RJA - E
141400
141500     IF SCR05Y-INCL-VIRTUAL NOT > SPACES
141600        MOVE 'N'                 TO SCR05Y-INCL-VIRTUAL
141700     ELSE
141800        IF SCR05Y-INCL-VIRTUAL = 'Y' OR 'N'
141900           CONTINUE
142000        ELSE
142100*          'MUST ENTER (Y/N)'
142200           MOVE 'M070'           TO MSGLOG-CODE
142300           MOVE -1               TO SCR05Y-INCL-VIRTUAL-CURSOR
142400           MOVE REV-VIDEO        TO SCR05Y-INCL-VIRTUAL-HI
142500           PERFORM P9000-SEND-MAP-AND-RETURN
142600        END-IF
142700     END-IF
142800     .
142900******************************************************************
143000 P4100-BUILD-SCREEN-ARRAY.
143100******************************************************************
143200     MOVE SPACES                 TO WS-DT-TM
143300     MOVE EMPTOD-OD-DATE-TIME    TO WS-DT-TM
143400     PERFORM P4200-FORMAT-DATE-TIME
143500     MOVE WS-FORMATTED-DT-TM     TO SCR05Y-OD-DT-TM(SUB1)
143600     MOVE EMPTOD-DIST            TO SCR05Y-DISTR(SUB1)
143700     MOVE EMPTOD-SDIST           TO SCR05Y-SUB-DISTR(SUB1)
143800     IF EMPTOD-IMPOSED-REST-REC
143900        MOVE 'IMP-REST'          TO SCR05Y-TRAIN(SUB1)
144000     ELSE
      *CNC0600-B
              IF EMPTOD-RESET-BREAK-REC
101200*CNLD-196 - B
      *          MOVE 'RESETBRK'       TO SCR05Y-TRAIN(SUB1)
      *CNLD-214B
009180*          SET SCR05Y-TRAIN-RESETBRK(SUB1) TO TRUE
009180
010200*CNLD-259 09/13/23 B
010200*
010200*      ->  UD2-END-TIME IS ALWAYS COMPUTED TO 0559 IN EMP TIMEZONE
010200*
010200*      ->  RESETNAT/RESETSYS/RESETMNL SHOULD BE DISPLAYED WHEN
010200*          RESET ENDS AT 0559  (I.E., UD2-END = RESET-END; AND
010200*          EMPTOD-OD-TIME ON FILE WILL BE STAMPED AS 0559 EMP TZ
010200*          AT THE TIME OF RECORDING THE RESET)
010200*
010200*      ->  RESETN32/RESETS32/RESETM32 SHOULD BE DISPLAYED WHEN
010200*          RESET ENDS AFTER 0559 (I.E., UD2-END < RESET-END; AND
010200*          EMPTOD-OD-TIME ON FILE WILL BE STAMPED WITH A VALUE
010200*          MORE THAN 0559 IN EMP TZ AT THE TIME OF RECORDING THE
010200*          RESET)
010200*
010200*CNLD-259 09/13/23 E
009180
                 EVALUATE TRUE
                 WHEN EMPTOD-RESET-BREAK-NAT
                    SET SCR05Y-TRAIN-RESETNAT(SUB1) TO TRUE
010200*CNLD-259 09/13/23 B
010200              IF EMPTOD-OD-TIME > '0559'
                       SET SCR05Y-TRAIN-RESETN32(SUB1) TO TRUE
010200              END-IF
010200*CNLD-259 09/13/23 E
                 WHEN EMPTOD-RESET-BREAK-SYS
                    SET SCR05Y-TRAIN-RESETSYS(SUB1) TO TRUE
010200*CNLD-259 09/13/23 B
010200              IF EMPTOD-OD-TIME > '0559'
                       SET SCR05Y-TRAIN-RESETS32(SUB1) TO TRUE
010200              END-IF
010200*CNLD-259 09/13/23 E
      *CNLD-242-DXC-B
                 WHEN EMPTOD-RESET-BREAK-MNL
                    SET SCR05Y-TRAIN-RESETMNL(SUB1) TO TRUE
010200*CNLD-259 09/13/23 B
010200              IF EMPTOD-OD-TIME > '0559'
                       SET SCR05Y-TRAIN-RESETM32(SUB1) TO TRUE
010200              END-IF
010200*CNLD-259 09/13/23 E
      *CNLD-242-DXC-E
                 WHEN OTHER
                    SET SCR05Y-TRAIN-RESETBRK(SUB1) TO TRUE
                 END-EVALUATE
      *CNLD-214E
101200*CNLD-196 - E
              ELSE
      *CNC0600-E
144100           IF EMPTOD-HOS-CLAIM
144200              MOVE EMPTOD-TRAIN-ASGN      TO SCR05Y-TRAIN(SUB1)
144300           ELSE
144400              MOVE EMPTOD-TRAIN-ASGN(1:8) TO SCR05Y-TRAIN(SUB1)
144500              MOVE EMPTOD-TRAIN-ASGN(9:2) TO SCR05Y-ASGN(SUB1)
144600              MOVE EMPTOD-CRAFT           TO SCR05Y-CRAFT(SUB1)
144700           END-IF
      *CNC0600-B
              END-IF
      *CNC0600-E
144800     END-IF
144900     MOVE SPACES                 TO WS-DT-TM
145000     MOVE EMPTOD-OFFD-DATE-TIME  TO WS-DT-TM
145100     PERFORM P4200-FORMAT-DATE-TIME
145200     MOVE WS-FORMATTED-DT-TM     TO SCR05Y-OFF-DT-TM(SUB1)
145300     IF CHANGE-FUNC
145400        MOVE MDT                 TO SCR05Y-OFF-DT-TM-ATTR(SUB1)
145500        MOVE WHITE               TO SCR05Y-OFF-DT-TM-COLOR(SUB1)
101200*C600-CR1 - B
145500        SET P05YCA-MODIFY-OFFD-CAN TO TRUE
101200*C600-CR1 - E
145600     ELSE
145700        MOVE PROTECT-MDT         TO SCR05Y-OFF-DT-TM-ATTR(SUB1)
145800        MOVE CYAN                TO SCR05Y-OFF-DT-TM-COLOR(SUB1)
101200*C600-CR1 - B
145500        SET P05YCA-MODIFY-OFFD-CANNOT TO TRUE
101200*C600-CR1 - E
145900     END-IF
146000     MOVE ZEROS                  TO DATE-CONVERSION-PARMS
146100     SET PARM-DIFF               TO TRUE
146200     MOVE EMPTOD-OD-DATE         TO PARM-PRI-DATE-GREG
146300     MOVE EMPTOD-OD-TIME         TO PARM-PRI-HRMN
146400     MOVE EMPTOD-OFFD-DATE       TO PARM-SEC-DATE-GREG
146500     MOVE EMPTOD-OFFD-TIME       TO PARM-SEC-HRMN
146600     PERFORM P9300-LINK-P903
146700     MOVE SPACES                 TO WS-DT-TM
146800
146900*\/* - BEGIN - @CNC0396 MUST CHECK FOR MORE THAN 1 DAY
147000     IF ( PARM-RES-TOT-DAYS > ZERO )
147100        MOVE ZERO                TO WS-TOT-DAYS
147200        MOVE ZERO                TO WS-TOT-HOURS
147300        MOVE ZERO                TO WS-REMAINDER-HOURS
147400        MOVE PARM-RES-TOT-DAYS   TO WS-TOT-DAYS
147500        MOVE PARM-RES-HRMN       TO WS-REMAINDER-HOURS
147600        MULTIPLY WS-TOT-DAYS     BY 2400
147700           GIVING WS-TOT-HOURS
147800        ADD  WS-REMAINDER-HOURS  TO WS-TOT-HOURS
147900        MOVE WS-TOT-HOURS        TO WS-TM
148000     ELSE
148100        MOVE PARM-RES-HRMN       TO WS-TM
148200     END-IF
148300*/\* - END   - @CNC0396
148400
148500     PERFORM P4200-FORMAT-DATE-TIME
148600     MOVE WS-FORMATTED-TM        TO SCR05Y-TIME-SPENT(SUB1)
148700     EVALUATE TRUE
148800        WHEN EMPTOD-REST-GREEN
148900           MOVE WS-GREEN         TO SCR05Y-CALL-STATUS(SUB1)
149000        WHEN EMPTOD-REST-YELLOW
149100           MOVE WS-YELLOW        TO SCR05Y-CALL-STATUS(SUB1)
149200        WHEN EMPTOD-REST-RED
149300           MOVE WS-RED           TO SCR05Y-CALL-STATUS(SUB1)
149400        WHEN EMPTOD-REST-RESPITE
149500           MOVE WS-RESPITE       TO SCR05Y-CALL-STATUS(SUB1)
149600        WHEN OTHER
149700           MOVE SPACES           TO SCR05Y-CALL-STATUS(SUB1)
149800     END-EVALUATE
149900     IF EMPTOD-MANUALLY-UPDATED
150000        IF EMPTOD-VIRTUAL-ENTRY
150100           MOVE '@'              TO SCR05Y-MANUAL-UPD(SUB1)
150200        ELSE
150300           MOVE '*'              TO SCR05Y-MANUAL-UPD(SUB1)
150400        END-IF
150500     ELSE
150600        IF EMPTOD-VIRTUAL-ENTRY
150700           MOVE '#'              TO SCR05Y-MANUAL-UPD(SUB1)
150800        ELSE
150900           IF EMPTOD-HOS-CLAIM
151000              MOVE '>'           TO SCR05Y-MANUAL-UPD(SUB1)
151100           END-IF
151200        END-IF
151300     END-IF
151400     .
151500******************************************************************
151600 P4200-FORMAT-DATE-TIME.
151700******************************************************************
151800     MOVE SPACES             TO WS-FORMATTED-YR
151900                                WS-FORMATTED-MO
152000                                WS-FORMATTED-DY
152100                                WS-FORMATTED-HR
152200                                WS-FORMATTED-MN
152300     MOVE WS-YR              TO WS-FORMATTED-YR
152400     MOVE WS-MO              TO WS-FORMATTED-MO
152500     MOVE WS-DY              TO WS-FORMATTED-DY
152600     MOVE WS-HR              TO WS-FORMATTED-HR
152700     MOVE WS-MN              TO WS-FORMATTED-MN.
152800******************************************************************
152900 P4300-GET-TOTAL-TIME.
153000******************************************************************
153100     INITIALIZE PS08-COMMAREA-PARMS
153200     SET  PS08-INQUIRY-FROM-05Y         TO TRUE
153300     MOVE SCR05Y-EMP-NBR                TO PS08-EMP-NBR
      *CNLD-241-DXC-B
      *2023/09/11 RJA B
      *    IF SCR05Y-DISPLAY-28D = 'Y'
           IF SCR05Y-DSPLY-LBL-4 =  WS-LBL-VARIABLE-4
      *2023/09/11 RJA E
157700        MOVE 07                         TO PS08-CALC-NBR-DAYS
157900        MOVE CONV-END-DATE-TIME         TO PS08-CALC-DATE-TIME
           ELSE
153400        MOVE WS-CALC-NBR-DAYS           TO PS08-CALC-NBR-DAYS
153500*** 331E
153600*       MOVE WS-LOCAL-DATE-TIME-CENT    TO PS08-CALC-DATE-TIME
153700        MOVE CONV-END-DATE-TIME         TO PS08-CALC-DATE-TIME
153800        MOVE CONV-START-DATE-TIME       TO PS08-OD-DATE-TIME-CENT
154100     END-IF
      *CNLD-241-DXC-E
153900     IF SCR05Y-INCL-VIRTUAL = 'Y'
154000        SET PS08-INCLUDE-VIRTUALS       TO TRUE
154100     END-IF
154200
154300     EXEC CICS LINK
154400               PROGRAM(PS08-PGM)
154500               COMMAREA(PS08-COMMAREA-PARMS)
154600               LENGTH(PS08-LGTH)
154700               RESP(WS-RESPONSE)
154800     END-EXEC
154900     MOVE WS-RESPONSE                   TO FILE-STATUS
155000     IF NOT SUCCESS
155100        MOVE 'P4300-1'                  TO ERR-PARAGRAPH
155200        MOVE 'PS08LINK'                 TO ERR-KEY
155300        PERFORM P9999-GOT-PROBLEM
155400     END-IF
155500     IF NOT PS08-NO-ERRORS
155600        MOVE 'P4300-2'                  TO ERR-PARAGRAPH
155700        MOVE PS08-RETURN-ERRORS         TO ERR-KEY
155800        MOVE 'CHECK S08 INPUT PARAMETERS'
155900                                        TO ERR-SENTENCE
156000        PERFORM P9999-GOT-PROBLEM
156100     END-IF
156200     INITIALIZE WS-FORMATTED-TOTAL-TIME
      *CNLD-241-DXC-B
      *CNC0600-CNLD-212B
156300     MOVE PS08-TOTAL-TIME-ON-DUTY       TO WS-TOTAL-TIME-ON-DUTY
      *    IF SCR05Y-DISPLAY-28D = 'Y'
      *       MOVE WS-SAVE-7-HR-TTOD          TO WS-TOTAL-TIME-ON-DUTY
      *    ELSE
156300*       MOVE PS08-TOTAL-TIME-ON-DUTY    TO WS-TOTAL-TIME-ON-DUTY
      *    END-IF
      *CNC0600-CNLD-212E
      *CNLD-241-DXC-E
156400     MOVE WS-TOTAL-HRS-ON-DUTY          TO WS-FORMATTED-TOT-HRS
156500     MOVE WS-TOTAL-MNS-ON-DUTY          TO WS-FORMATTED-TOT-MNS
      *CNLD-281-RJA-B
156600*    MOVE WS-FORMATTED-TOTAL-TIME       TO SCR05Y-TOTAL-TIME
156600     MOVE WS-FORMATTED-TOTAL-TIME       TO SCR05Y-7DAY-TTOD
      *CNLD-281-RJA-E
156700     MOVE CONV-END-DATE                 TO SCR05Y-TOTAL-DATE
156800     MOVE CONV-END-TIME                 TO SCR05Y-TOTAL-HRMN
      *CNLD-281-RJA-B
           IF SCR05Y-DSPLY-LBL-5 =  WS-LBL-VARIABLE-5
153100        INITIALIZE PS08-COMMAREA-PARMS
153200        SET  PS08-INQUIRY-FROM-05Y      TO TRUE
153300        MOVE SCR05Y-EMP-NBR             TO PS08-EMP-NBR
153400        MOVE WS-CALC-NBR-DAYS           TO PS08-CALC-NBR-DAYS
153700        MOVE CONV-END-DATE-TIME         TO PS08-CALC-DATE-TIME
153800        MOVE CONV-START-DATE-TIME       TO PS08-OD-DATE-TIME-CENT
153900        IF SCR05Y-INCL-VIRTUAL = 'Y'
154000           SET PS08-INCLUDE-VIRTUALS       TO TRUE
154100        END-IF
154200
154300        EXEC CICS LINK
154400                  PROGRAM(PS08-PGM)
154500                  COMMAREA(PS08-COMMAREA-PARMS)
154600                  LENGTH(PS08-LGTH)
154700                  RESP(WS-RESPONSE)
154800        END-EXEC
154900        MOVE WS-RESPONSE                   TO FILE-STATUS
155000        IF NOT SUCCESS
155100           MOVE 'P4300-3'                  TO ERR-PARAGRAPH
155200           MOVE 'PS08LINK'                 TO ERR-KEY
155300           PERFORM P9999-GOT-PROBLEM
155400        END-IF
155500        IF NOT PS08-NO-ERRORS
155600           MOVE 'P4300-4'                  TO ERR-PARAGRAPH
155700           MOVE PS08-RETURN-ERRORS         TO ERR-KEY
155800           MOVE 'CHECK S08 INPUT PARAMETERS'
155900                                           TO ERR-SENTENCE
156000           PERFORM P9999-GOT-PROBLEM
156100        END-IF
156100
156200        INITIALIZE WS-FORMATTED-TOTAL-TIME
156300        MOVE PS08-TOTAL-TIME-ON-DUTY    TO WS-TOTAL-TIME-ON-DUTY
156400        MOVE WS-TOTAL-HRS-ON-DUTY       TO WS-FORMATTED-TOT-HRS
156500        MOVE WS-TOTAL-MNS-ON-DUTY       TO WS-FORMATTED-TOT-MNS
156600        MOVE WS-FORMATTED-TOTAL-TIME    TO SCR05Y-TOTAL-TIME
           ELSE
              MOVE SPACES                     TO SCR05Y-TOTAL-TIME
           END-IF
      *CNLD-281-RJA-E
156900     .
157000*CNLD-249-B
157100******************************************************************
157200 P4310-GET-7D-END-PERIOD.
157300******************************************************************
157000*CNLD-249-B 9/14/23 RJA
048500*    IF EMPLOYEE IS ON-DUTY, DISPLAY '7DY RESET' FROM MSC03 AS IT
048500*    WOULD HAVE UPDATED 7DAY END DURING CALL PROCESS (OR IVR INIT)
           IF WORKING OF WS-MSTR
              MOVE SCR05Y-EMP-NBR TO MSTR3NBRK
434300        EXEC CICS READ
434400                  DATASET(MSTR3-VIA-EMP-NBR)
434500                  INTO(WS-MSTR3)
434600                  LENGTH(MSTR3ENBR-RLGTH)
434700                  RIDFLD(MSTR3NBRK)
434800                  KEYLENGTH(MSTR3ENBR-KLGTH)
434900                  RESP(WS-RESPONSE)
435000        END-EXEC
435100        MOVE WS-RESPONSE TO FILE-STATUS
047900        IF NOT SUCCESS
048000*          MSTR3 SHOULD BE PRESENT IF A CALL IS PROCESSED
048100           MOVE 'P4310-1'          TO ERR-PARAGRAPH
048200           MOVE MSTR3NBRK          TO ERR-KEY
048300           PERFORM P9999-GOT-PROBLEM
048500        ELSE
                 STRING MSTR3-7DAY-END-DATE ' '
                        MSTR3-7DAY-END-TIME
                 DELIMITED BY SIZE INTO SCR05Y-7DAY-RESET
048500        END-IF
048500     ELSE
157000*CNLD-249-E 9/14/23 RJA
511400        INITIALIZE PS08-COMMAREA-PARMS
511500        SET PS08-RESET-BRK-7DAY-END-FUN    TO TRUE
511600        MOVE SCR05Y-EMP-NBR                TO PS08-EMP-NBR
504100
158500        EXEC CICS LINK
158600                  PROGRAM(PS08-PGM)
158700                  COMMAREA(PS08-COMMAREA-PARMS)
158800                  LENGTH(PS08-LGTH)
158900                  RESP(WS-RESPONSE)
159000        END-EXEC
159100        MOVE WS-RESPONSE                   TO FILE-STATUS
159200        IF NOT SUCCESS
159300           MOVE 'P4310-2'                  TO ERR-PARAGRAPH
159400           MOVE 'PS08LINK'                 TO ERR-KEY
159500           PERFORM P9999-GOT-PROBLEM
159600        END-IF
159700        IF NOT PS08-NO-ERRORS
159800           MOVE 'P4310-3'                  TO ERR-PARAGRAPH
159900           MOVE PS08-RETURN-ERRORS         TO ERR-KEY
160000           MOVE 'CHECK S08 INPUT PARAMETERS'
160100                                           TO ERR-SENTENCE
160200           PERFORM P9999-GOT-PROBLEM
160300        END-IF
157000*
157000*CNLD-249-RJA-B
157000*       PS08-7DAY-END-DTTM (7DY RESET) IS THE TIME BEFORE WHICH
157000*       EMPLOYEE SHOULD GET A RESET-BREAK;
157000*       THIS TIME IS COMPUTED BY ADDING 6DYS 23HRS 59MINS TO THE
157000*       START TIME OF THE FIRST HOS IN THE CYCLE;
157000*       IF A HOS IS YET TO BE RECORDED IN THE CURRENT CYCLE, THEN
157000*       '7DY RESET' IS DISPLAYED AS <TBD>
              IF PS08-7DAY-END-DTTM NOT > '0000000000'
                 MOVE '<TBD>'                 TO SCR05Y-7DAY-RESET

                 MOVE SCR05Y-EMP-NBR TO MSTR3NBRK
434300           EXEC CICS READ
434400                     DATASET(MSTR3-VIA-EMP-NBR)
434500                     INTO(WS-MSTR3)
434600                     LENGTH(MSTR3ENBR-RLGTH)
434700                     RIDFLD(MSTR3NBRK)
434800                     KEYLENGTH(MSTR3ENBR-KLGTH)
434900                     RESP(WS-RESPONSE)
435000           END-EXEC
435100           MOVE WS-RESPONSE TO FILE-STATUS
047900           IF NOT SUCCESS
048000              IF NOT (NO-RECORD-FND OR END-OF-FILE)
048100                 MOVE 'P4310-4'          TO ERR-PARAGRAPH
048200                 MOVE MSTR3NBRK          TO ERR-KEY
048300                 PERFORM P9999-GOT-PROBLEM
048400              END-IF
048500           ELSE
048500*          IF A SYSTEM RESET IS RECORDED FOR A FUTURE TIME VIA
048500*          MA0V OR TIE-UP OR CHANGE-OF-STATUS(P16-PGM) PROCESSES,
048500*          THAT RESET WOULD MARK THE END OF THE CURRENT CYCLE.
048500              IF MSTR3-SYSTEM-RESET-BRK-NUM >= WS-LOCAL-DATE-TIME
                       STRING MSTR3-SYSTEM-RESET-DATE  ' '
                              MSTR3-SYSTEM-RESET-TIME
                       DELIMITED BY SIZE INTO SCR05Y-7DAY-RESET
048500              END-IF
048500           END-IF
              ELSE
                 STRING PS08-7DAY-END-DATE-YYMMDD ' '
                        PS08-7DAY-END-TIME
                 DELIMITED BY SIZE INTO SCR05Y-7DAY-RESET
              END-IF
157000*CNLD-249-RJA-E
157000*CNLD-249-B 9/14/23 RJA
048500     END-IF
157000*CNLD-249-E 9/14/23 RJA
157000*
502700     .
157000*CNLD-249-E
157000*
157000*CNC0600-B
157100******************************************************************
157200 P4400-GET-28D-TTOD.
157300******************************************************************
157400     INITIALIZE PS08-COMMAREA-PARMS
157500     SET  PS08-INQUIRY-FUN              TO TRUE
157600     MOVE SCR05Y-EMP-NBR                TO PS08-EMP-NBR
157700     MOVE 28                            TO PS08-CALC-NBR-DAYS
157900     MOVE CONV-END-DATE-TIME            TO PS08-CALC-DATE-TIME
158100     IF SCR05Y-INCL-VIRTUAL = 'Y'
158200        SET PS08-INCLUDE-VIRTUALS       TO TRUE
158300     END-IF
158400
158500     EXEC CICS LINK
158600               PROGRAM(PS08-PGM)
158700               COMMAREA(PS08-COMMAREA-PARMS)
158800               LENGTH(PS08-LGTH)
158900               RESP(WS-RESPONSE)
159000     END-EXEC
159100     MOVE WS-RESPONSE                   TO FILE-STATUS
159200     IF NOT SUCCESS
159300        MOVE 'P4400-1'                  TO ERR-PARAGRAPH
159400        MOVE 'PS08LINK'                 TO ERR-KEY
159500        PERFORM P9999-GOT-PROBLEM
159600     END-IF
159700     IF NOT PS08-NO-ERRORS
159800        MOVE 'P4400-2'                  TO ERR-PARAGRAPH
159900        MOVE PS08-RETURN-ERRORS         TO ERR-KEY
160000        MOVE 'CHECK S08 INPUT PARAMETERS'
160100                                        TO ERR-SENTENCE
160200        PERFORM P9999-GOT-PROBLEM
160300     END-IF
160400     INITIALIZE WS-FORMATTED-TOTAL-TIME
160500     MOVE PS08-TOTAL-TIME-ON-DUTY       TO WS-TOTAL-TIME-ON-DUTY
160600     MOVE WS-TOTAL-HRS-ON-DUTY          TO WS-FORMATTED-TOT-HRS
160700     MOVE WS-TOTAL-MNS-ON-DUTY          TO WS-FORMATTED-TOT-MNS
160800     MOVE WS-FORMATTED-TOTAL-TIME       TO SCR05Y-28DAY-TTOD
160900     .
161000*CNC0600-E
      *
031100*CNLD-216-B
      ******************************************************************
       P4500-GET-LABELS.
      ******************************************************************
           MOVE WS-CNTL-FILE                  TO SAVE-CNTL-FILE
           MOVE SPACES                        TO WS-CNTL-FILE
           SET  SUB-DIST-TYPE-REC             TO TRUE
           MOVE PSTCA-DIST                    TO CNTL-DIST
           MOVE PSTCA-SUB-DIST                TO CNTL-SUB-DIST
           MOVE CNTLKEY-AREA                  TO CNTLKEY
           PERFORM P8080-READ-CNTL-RECORD
      *
           IF CNTL-CAN-WRR-NEW
              SET  WS-CAN-WRR-NEW             TO TRUE
              MOVE WS-LBL-VARIABLE-1          TO SCR05Y-DSPLY-LBL-1
              MOVE WS-LBL-VARIABLE-2          TO SCR05Y-DSPLY-LBL-2
              MOVE WS-LBL-VARIABLE-3          TO SCR05Y-DSPLY-LBL-3
              MOVE WS-LBL-VARIABLE-4          TO SCR05Y-DSPLY-LBL-4
      *CNLD-281-B
              MOVE WS-LBL-VARIABLE-5          TO SCR05Y-DSPLY-LBL-5
      *CNLD-281-E
      *CNLD-249-B
              MOVE WS-LBL-VARIABLE-6          TO SCR05Y-DSPLY-LBL-6
      *CNLD-249-E
              MOVE BRIGHT-MDT                 TO SCR05Y-DISPLAY-28D-ATTR
           ELSE
              MOVE SPACES                     TO SCR05Y-DSPLY-LBL-1
                                                 SCR05Y-DSPLY-LBL-2
                                                 SCR05Y-DSPLY-LBL-3
      *CNLD-281-B
                                                 SCR05Y-DSPLY-LBL-5
                                                 SCR05Y-DSPLY-LBL-6
      *CNLD-281-E
                                                 SCR05Y-DISPLAY-28D
              MOVE WS-LBL-VARIABLE-5          TO SCR05Y-DSPLY-LBL-4
              MOVE AUTOSKIP-MDT               TO SCR05Y-DISPLAY-28D-ATTR
           END-IF
           MOVE SAVE-CNTL-FILE                TO WS-CNTL-FILE.
031100*CNLD-216-E
161100******************************************************************
161200 P5000-READ-MASTER.
161300******************************************************************
161400     EXEC CICS READ
161500               DATASET(MSTR-VIA-EMP-NBR)
161600               INTO(WS-MSTR)
161700               LENGTH(MSTRENBR-RLGTH)
161800               RIDFLD(MSTRNBRK)
161900               KEYLENGTH(MSTRENBR-KLGTH)
162000               RESP(WS-RESPONSE)
162100     END-EXEC
162200     MOVE WS-RESPONSE TO FILE-STATUS
162300     IF SUCCESS
162400        CONTINUE
162500     ELSE
162600        IF NO-RECORD-FND OR END-OF-FILE
162700           MOVE SPACES   TO WS-MSTR
162800        ELSE
162900           MOVE 'P5000'  TO ERR-PARAGRAPH
163000           MOVE MSTRNBRK TO ERR-KEY
163100           PERFORM P9999-GOT-PROBLEM
163200        END-IF
163300     END-IF
163400     .
163500******************************************************************
163600 P5010-INQUIRE-VIA-NAME.
163700******************************************************************
163800     MOVE '0'                           TO GOT-EMPLOYEE-FLAG
163900     MOVE SCR05Y-EMP-NAME               TO EMP-NAME OF WS-MSTR
164000     IF SCR05Y-EMP-NBR > ZEROES
164100        MOVE SCR05Y-EMP-NBR             TO EMP-NBR OF WS-MSTR
164200     ELSE
164300        MOVE ZEROES                     TO EMP-NBR OF WS-MSTR
164400     END-IF
164500     MOVE EMP-NAME-NBR-KEY              TO MSTREMPK
164600     EXEC CICS STARTBR
164700               DATASET(MSTR-VIA-EMP-NAME)
164800               RIDFLD(MSTREMPK)
164900               GTEQ
165000               RESP(WS-RESPONSE)
165100     END-EXEC
165200     MOVE WS-RESPONSE                   TO FILE-STATUS
165300     IF SUCCESS
165400        SET EMP-NOT-DONE-YET            TO TRUE
165500        PERFORM P5015-READ-SEQUENTIAL UNTIL EMP-DONE
165600        EXEC CICS ENDBR
165700                  DATASET(MSTR-VIA-EMP-NAME)
165800                  RESP(WS-RESPONSE)
165900        END-EXEC
166000        IF GOT-EMPLOYEE
166100           PERFORM P1010-SETUP-INQUIRY
166200        END-IF
166300     ELSE
166400        IF END-OF-FILE OR NO-RECORD-FND
166500*               NOT-FOUND-MSG
166600           MOVE 'N015'                  TO MSGLOG-CODE
166700        ELSE
166800           MOVE 'P5010'                 TO ERR-PARAGRAPH
166900           MOVE SPACES                  TO ERR-KEY
167000           PERFORM P9999-GOT-PROBLEM
167100        END-IF
167200     END-IF
167300     .
167400******************************************************************
167500 P5015-READ-SEQUENTIAL.
167600******************************************************************
167700     EXEC CICS READNEXT
167800               DATASET(MSTR-VIA-EMP-NAME)
167900               INTO(WS-MSTR)
168000               LENGTH(MSTRENAM-RLGTH)
168100               RIDFLD(MSTREMPK)
168200               KEYLENGTH(MSTRENAM-KLGTH)
168300               RESP(WS-RESPONSE)
168400     END-EXEC
168500     MOVE WS-RESPONSE TO FILE-STATUS
168600     IF SUCCESS
168700        IF DIST OF WS-MSTR = SCR05Y-DIST AND
168800           SUB-DIST OF WS-MSTR = SCR05Y-SUB-DIST
168900             IF PFKEY12
169000                IF SECOND-TIME
169100                   SET EMP-DONE TO TRUE
169200                   SET GOT-EMPLOYEE TO TRUE
169300                ELSE
169400                   IF SCR05Y-EMP-NAME = SPACES OR LOW-VALUES
169500                     OR P05YCA-EMP-NAME = SPACES OR LOW-VALUES
169600                     OR P05YCA-EMP-NAME NOT = SCR05Y-EMP-NAME
169700                      SET EMP-DONE TO TRUE
169800                      SET GOT-EMPLOYEE TO TRUE
169900                   END-IF
170000                   SET SECOND-TIME TO TRUE
170100                END-IF
170200             ELSE
170300                SET EMP-DONE TO TRUE
170400                SET GOT-EMPLOYEE TO TRUE
170500             END-IF
170600        ELSE
170700           IF SCR05Y-EMP-NAME = EMP-NAME OF WS-MSTR
170800              MOVE DIST     OF WS-MSTR TO WS-MSGA-DIST
170900              MOVE SUB-DIST OF WS-MSTR TO WS-MSGA-SUB-DIST
171000              SET FOUND-SAME-NAME TO TRUE
171100           END-IF
171200        END-IF
171300     ELSE
171400        SET EMP-DONE          TO TRUE
171500        IF NO-RECORD-FND OR END-OF-FILE
171600           IF PFKEY12
171700*                  END-MSG
171800              MOVE 'E044' TO MSGLOG-CODE
171900           ELSE
172000              IF SCR05Y-EMP-NAME = SPACES OR LOW-VALUES
172100*                     END-MSG
172200                 MOVE 'E044' TO MSGLOG-CODE
172300              ELSE
172400                 IF FOUND-SAME-NAME
172500*                 WS-MSGA(EMP NOT IN THIS DIST/SUBDIST
172600                    MOVE 'E053' TO MSGLOG-CODE
172700                 ELSE
172800*                        NOT-FOUND-MSG
172900                    MOVE 'N015' TO MSGLOG-CODE
173000                    MOVE REV-VIDEO TO SCR05Y-EMP-NAME-HI
173100                 END-IF
173200              END-IF
173300           END-IF
173400        ELSE
173500           MOVE 'P5015' TO ERR-PARAGRAPH
173600           MOVE SPACES  TO ERR-KEY
173700           PERFORM P9999-GOT-PROBLEM
173800        END-IF
173900     END-IF
174000     .
174100******************************************************************
174200 P7100-WRITE-TSQUEUE.
174300******************************************************************
174400*
174500*      WRITE MAP TSQUEUE
174600*
174700     EXEC CICS ASSIGN
174800          EXTDS(WS-CICS-EXTDS-CODE)
174900     END-EXEC
175000*
175100     IF SCREEN-HAS-EXT-ATTR
175200        EXEC CICS SEND STRFIELD
175300                  FROM(WS-STRFIELD)
175400                  LENGTH(WS-STRFIELD-LGTH)
175500                  RESP(WS-RESPONSE)
175600        END-EXEC
175700        MOVE WS-RESPONSE           TO FILE-STATUS
175800        IF NOT SUCCESS
175900           MOVE 'P7100-1'          TO ERR-PARAGRAPH
176000           MOVE 'SEND STRFIELD'    TO ERR-KEY
176100           PERFORM P9999-GOT-PROBLEM
176200        END-IF
176300     END-IF
176400*
176500     MOVE LENGTH OF WS-BUFFER-DATA TO WS-BUFFER-LGTH
176600     EXEC CICS RECEIVE BUFFER
176700               INTO(WS-BUFFER-DATA)
176800               LENGTH(WS-BUFFER-LGTH)
176900               RESP(WS-RESPONSE)
177000     END-EXEC
177100     MOVE WS-RESPONSE              TO FILE-STATUS
177200     IF NOT SUCCESS AND NOT EOC
177300        MOVE 'P7100-2'             TO ERR-PARAGRAPH
177400        MOVE 'RECEIVE BUFFER'      TO ERR-KEY
177500        PERFORM P9999-GOT-PROBLEM
177600     END-IF
177700     MOVE EIBCPOSN                 TO WS-BUFFER-CURSOR
177800
177900
178000     MOVE LENGTH OF WS-BUFFER-AREA TO P05YTSQ-QLGTH
178100     MOVE EIBTRMID                 TO P05YTSQ-MAP-TERM-ID
178200     EXEC CICS WRITEQ TS
178300               QUEUE(P05YTSQ-MAP-QUEUE-ID)
178400               FROM(WS-BUFFER-AREA)
178500               LENGTH(P05YTSQ-QLGTH)
178600               RESP(WS-RESPONSE)
178700     END-EXEC
178800     MOVE WS-RESPONSE              TO FILE-STATUS
178900     IF NOT SUCCESS
179000        MOVE 'P7100-3'             TO ERR-PARAGRAPH
179100        PERFORM P9999-GOT-PROBLEM
179200     END-IF
179300     MOVE EIBTRMID TO P05YTSQ-CA-TERM-ID
179400     EXEC CICS WRITEQ TS
179500               QUEUE(P05YTSQ-CA-QUEUE-ID)
179600               FROM(PSTCOMM-AREA)
179700               LENGTH(PSTCOMM-LGTH)
179800               ITEM(P05YTSQ-QUEUE-ITEM)
179900               RESP(WS-RESPONSE)
180000     END-EXEC
180100     MOVE WS-RESPONSE              TO FILE-STATUS
180200     IF NOT SUCCESS
180300        MOVE 'P7100-4'             TO ERR-PARAGRAPH
180400        PERFORM P9999-GOT-PROBLEM
180500     END-IF.
180600*
180700 P7110-READ-TSQUEUE.
180800*
180900*              READ THE MAPS TSQUEUE
181000*
181100     MOVE LENGTH OF WS-BUFFER-AREA TO P05YTSQ-QLGTH
181200     MOVE EIBTRMID                 TO P05YTSQ-MAP-TERM-ID
181300     EXEC CICS READQ TS
181400               QUEUE(P05YTSQ-MAP-QUEUE-ID)
181500               INTO(WS-BUFFER-AREA)
181600               LENGTH(P05YTSQ-QLGTH)
181700               ITEM(P05YTSQ-QUEUE-ITEM)
181800               RESP(WS-RESPONSE)
181900     END-EXEC
182000     MOVE WS-RESPONSE              TO FILE-STATUS
182100     IF SUCCESS
182200        SET SEND-BUFFER            TO TRUE
182300     ELSE
182400        SET CREATE-SCREEN          TO TRUE
182500        MOVE LOW-VALUES            TO PSTS05Y
182600     END-IF
182700     MOVE EIBTRMID TO P05YTSQ-CA-TERM-ID
182800     EXEC CICS READQ TS
182900               QUEUE(P05YTSQ-CA-QUEUE-ID)
183000               INTO(PSTCOMM-AREA)
183100               LENGTH(PSTCOMM-LGTH)
183200               ITEM(P05YTSQ-QUEUE-ITEM)
183300               RESP(WS-RESPONSE)
183400     END-EXEC
183500     MOVE WS-RESPONSE TO FILE-STATUS
183600     IF NOT SUCCESS
183700        MOVE SPACES TO PSTCOMM-AREA
183800     END-IF
183900     PERFORM P7120-DELETE-TSQUEUE.
184000*
184100 P7120-DELETE-TSQUEUE.
184200*
184300     MOVE EIBTRMID TO P05YTSQ-MAP-TERM-ID
184400     EXEC CICS DELETEQ TS
184500               QUEUE(P05YTSQ-MAP-QUEUE-ID)
184600               RESP(WS-RESPONSE)
184700     END-EXEC
184800     MOVE EIBTRMID TO P05YTSQ-CA-TERM-ID
184900     EXEC CICS DELETEQ TS
185000               QUEUE(P05YTSQ-CA-QUEUE-ID)
185100               RESP(WS-RESPONSE)
185200     END-EXEC.
185300*
185400******************************************************************
185500 P8000-STARTBR-ETOD.
185600******************************************************************
185700     MOVE EMPTOD-KEY-AREA  TO EMPTOD-EMPKEY
185800     EXEC CICS STARTBR
185900               DATASET(EMPTOD-VIA-EMP)
186000               RIDFLD(EMPTOD-EMPKEY)
186100               GTEQ
186200               RESP(WS-RESPONSE)
186300     END-EXEC
186400     MOVE WS-RESPONSE      TO FILE-STATUS
186500     IF NOT(SUCCESS OR NO-RECORD-FND OR END-OF-FILE)
186600       MOVE 'P8000-1'      TO ERR-PARAGRAPH
186700       MOVE EMPTOD-EMPKEY  TO ERR-KEY
186800       PERFORM P9999-GOT-PROBLEM
186900     END-IF.
187000******************************************************************
187100 P8010-READNEXT-ETOD.
187200******************************************************************
187300     MOVE EMPTOD-KEY-AREA  TO EMPTOD-EMPKEY
187400     EXEC CICS READNEXT
187500               DATASET(EMPTOD-VIA-EMP)
187600               INTO(WS-EMP-TOD)
187700               RIDFLD(EMPTOD-EMPKEY)
187800               LENGTH(EMPTOD-EMP-RLGTH)
187900               KEYLENGTH(EMPTOD-EMP-KLGTH)
188000               RESP(WS-RESPONSE)
188100     END-EXEC
188200     MOVE WS-RESPONSE      TO FILE-STATUS
188300     IF NOT(SUCCESS OR NO-RECORD-FND OR END-OF-FILE)
188400       MOVE 'P8010-1'      TO ERR-PARAGRAPH
188500       MOVE EMPTOD-EMPKEY  TO ERR-KEY
188600       PERFORM P9999-GOT-PROBLEM
188700     END-IF
102200     .
188800******************************************************************
188900 P8020-ENDBR-ETOD.
189000******************************************************************
189100     EXEC CICS ENDBR
189200               DATASET(EMPTOD-VIA-EMP)
189300               RESP(WS-RESPONSE)
189400     END-EXEC
189500     MOVE WS-RESPONSE      TO FILE-STATUS
189600     IF NOT SUCCESS
189700       MOVE 'P8020-1'      TO ERR-PARAGRAPH
189800       PERFORM P9999-GOT-PROBLEM
189900     END-IF.
190000******************************************************************
190100 P8030-READ-UPDATE-ETOD.
190200******************************************************************
190300     EXEC CICS READ
190400               UPDATE
190500               DATASET(EMPTOD-VIA-EMP)
190600               INTO(WS-EMP-TOD)
190700               LENGTH(EMPTOD-EMP-RLGTH)
190800               RIDFLD(EMPTOD-EMPKEY)
190900               KEYLENGTH(EMPTOD-EMP-KLGTH)
191000               RESP(WS-RESPONSE)
191100     END-EXEC
191200     MOVE WS-RESPONSE      TO FILE-STATUS
191300     IF NOT SUCCESS
191400       MOVE 'P8030-1'      TO ERR-PARAGRAPH
191500       MOVE EMPTOD-EMPKEY  TO ERR-KEY
191600       PERFORM P9999-GOT-PROBLEM
191700     END-IF
102200     .
191800******************************************************************
191900 P8040-REWRITE-ETOD.
192000******************************************************************
192100     EXEC CICS REWRITE
192200               DATASET(EMPTOD-VIA-EMP)
192300               FROM(WS-EMP-TOD)
192400               LENGTH(EMPTOD-EMP-RLGTH)
192500               RESP(WS-RESPONSE)
192600     END-EXEC
192700     MOVE WS-RESPONSE      TO FILE-STATUS
192800     IF NOT SUCCESS
192900       MOVE 'P8040-1'      TO ERR-PARAGRAPH
193000       MOVE EMPTOD-EMPKEY  TO ERR-KEY
193100       PERFORM P9999-GOT-PROBLEM
193200     ELSE
193300*      UPDATE MSG
193400       MOVE 'U001'         TO MSGLOG-CODE
193500     END-IF.
193600******************************************************************
193700 P8050-DELETE-ETOD.
193800******************************************************************
193900     EXEC CICS DELETE
194000               DATASET(EMPTOD-VIA-EMP)
194100               RIDFLD(EMPTOD-EMPKEY)
194200               RESP(WS-RESPONSE)
194300     END-EXEC
194400     MOVE WS-RESPONSE         TO FILE-STATUS
194500     IF NOT SUCCESS
194600       IF NO-RECORD-FND
194700*         NOT FOUND MSG
194800          MOVE 'N015'         TO MSGLOG-CODE
194900       ELSE
195000          MOVE 'P8050-1'      TO ERR-PARAGRAPH
195100          MOVE EMPTOD-EMPKEY  TO ERR-KEY
195200          PERFORM P9999-GOT-PROBLEM
195300       END-IF
195400     ELSE
195500*      DELETE MSG
195600       MOVE 'D006'            TO MSGLOG-CODE
195700     END-IF.
195800******************************************************************
195900 P8060-READPREV-ETOD.
196000******************************************************************
196100     MOVE EMPTOD-KEY-AREA  TO EMPTOD-EMPKEY
196200     EXEC CICS READPREV
196300               DATASET(EMPTOD-VIA-EMP)
196400               INTO(WS-EMP-TOD)
196500               RIDFLD(EMPTOD-EMPKEY)
196600               LENGTH(EMPTOD-EMP-RLGTH)
196700               KEYLENGTH(EMPTOD-EMP-KLGTH)
196800               RESP(WS-RESPONSE)
196900     END-EXEC
197000     MOVE WS-RESPONSE      TO FILE-STATUS
197100     IF NOT(SUCCESS OR NO-RECORD-FND OR END-OF-FILE)
197200       MOVE 'P8060-1'      TO ERR-PARAGRAPH
197300       MOVE EMPTOD-EMPKEY  TO ERR-KEY
197400       PERFORM P9999-GOT-PROBLEM
197500     END-IF
102200     .
101200*CNC0600 - RJA - B
190000******************************************************************
190100 P8070-READ-ETOD.
190200******************************************************************
190300     EXEC CICS READ
190500               DATASET(EMPTOD-VIA-EMP)
190600               INTO(WS-EMP-TOD)
190700               LENGTH(EMPTOD-EMP-RLGTH)
190800               RIDFLD(EMPTOD-EMPKEY)
190900               KEYLENGTH(EMPTOD-EMP-KLGTH)
191000               RESP(WS-RESPONSE)
191100     END-EXEC
191200     MOVE WS-RESPONSE      TO FILE-STATUS
191300     IF NOT SUCCESS
191400       MOVE 'P8070-1'      TO ERR-PARAGRAPH
191500       MOVE EMPTOD-EMPKEY  TO ERR-KEY
191600       PERFORM P9999-GOT-PROBLEM
191700     END-IF
102200     .
101200*CNC0600 - RJA - E
      *
190000******************************************************************
190100 P8080-READ-CNTL-RECORD.
190200******************************************************************
219400     EXEC CICS READ
219500               DATASET(CNTL-FILE-VIA-CNTLKEY)
219600               INTO(WS-CNTL-FILE)
219700               LENGTH(CNTLFILE-RLGTH)
219800               RIDFLD(CNTLKEY)
219900               KEYLENGTH(CNTLFILE-KLGTH)
220000               RESP(WS-RESPONSE)
220100     END-EXEC
220200
220300     MOVE WS-RESPONSE TO FILE-STATUS.
220400     IF SUCCESS OR NO-RECORD-FND
220500        CONTINUE
220600     ELSE
220700        MOVE 'P8080-1'       TO ERR-PARAGRAPH
220800        MOVE CNTLKEY         TO ERR-KEY
220900        PERFORM P9999-GOT-PROBLEM
221000     END-IF.
197600******************************************************************
197700 P8900-WRITE-HISTORY.
197800******************************************************************
197900     INITIALIZE WS-MSTR
198000     MOVE SCR05Y-EMP-NBR        TO MSTRNBRK
198100     PERFORM P5000-READ-MASTER
198200     IF NOT SUCCESS
198300        MOVE 'P8900-1'          TO ERR-PARAGRAPH
198400        MOVE MSTRNBRK           TO ERR-KEY
198500        PERFORM P9999-GOT-PROBLEM
198600     END-IF
198700
198800     SET P943-EMPLOYEE-FUNCTION TO TRUE
198900     MOVE PSTCA-TIME-ZONE       TO P943-EMP-TIME-ZONE
199000
199100     SET P943-TOD-CHANGED-FUN   TO TRUE
199200     MOVE EMP-NBR OF WS-MSTR    TO P943-EMP-NBR
199300     MOVE WS-SYSTEM-DATE-TIME(1:14)
199400                                TO PRESENT-TIME-X
199500     ADD 1                      TO PRESENT-TOD
199600     MOVE PRESENT-TIME-X        TO P943-CLOCK-TIME
199700     MOVE DIST     OF WS-MSTR   TO P943-DIST
199800     MOVE SUB-DIST OF WS-MSTR   TO P943-SDIST
199900     MOVE CRAFT    OF WS-MSTR   TO P943-CRAFT
200000     MOVE LAYOFF-CODE           TO P943-LO
200100     EXEC CICS ASSIGN
200200               USERID(P943-USERID)
200300     END-EXEC
200400     EXEC CICS LINK
200500               PROGRAM(P943-PGM)
200600               COMMAREA(P943-COMMAREA-PARMS)
200700               LENGTH(P943-LGTH)
200800               RESP(WS-RESPONSE)
200900     END-EXEC
201000     MOVE WS-RESPONSE           TO FILE-STATUS
201100     IF NOT SUCCESS
201200        MOVE 'P8900-1'          TO ERR-PARAGRAPH
201300        MOVE 'P943'             TO ERR-KEY
201400        PERFORM P9999-GOT-PROBLEM
201500     END-IF
201600     MOVE SPACES                TO P943-COMMAREA-PARMS.
201700******************************************************************
201800 COPY TIMEEDIT.
201900*
202000 COPY DATEEDIT.
202100*
202200 COPY TZERROR.
202300*
202400 COPY BIFEDIT.
202500*
202600 COPY TIMEZONE.
202700******************************************************************
202800 P9000-SEND-MAP-AND-RETURN.
202900******************************************************************
203000     IF MSGLOG-CODE > SPACES
203100         PERFORM P9030-GET-MESSAGE
203200         MOVE MSGLOG-MESSAGE-AREA TO SCR05Y-ERRORMSG
203300     END-IF
203400
203500     MOVE P05Y-MAP-VERSION(PSTCA-SUB) TO P05Y-MAP
203600     IF CREATE-SCREEN
203700        PERFORM P9010-SEND-PHYSICAL-MAP
203800     ELSE
203900        IF CONTINUE-SCREEN
204000           PERFORM P9020-SEND-DATAONLY-MAP
204100        ELSE
204200           PERFORM P9035-SEND-BUFFER
204300        END-IF
204400     END-IF
204500     EXEC CICS RETURN
204600               TRANSID(P05Y-TRAN)
204700               COMMAREA(PSTCOMM-AREA)
204800               LENGTH(PSTCOMM-LGTH)
204900     END-EXEC.
205000*
205100******************************************************************
205200 P9010-SEND-PHYSICAL-MAP.
205300******************************************************************
205400     EXEC CICS SEND MAP(P05Y-MAP)
205500                    MAPSET(P05Y-SET)
205600                    FROM(PSTS05Y)
205700                    CURSOR
205800                    ERASE
205900                    FREEKB
206000                    RESP(WS-RESPONSE)
206100     END-EXEC
206200     MOVE WS-RESPONSE TO FILE-STATUS
206300     IF NOT SUCCESS
206400        MOVE 'P9010'   TO ERR-PARAGRAPH
206500        PERFORM P9999-GOT-PROBLEM
206600     END-IF.
206700*
206800******************************************************************
206900 P9020-SEND-DATAONLY-MAP.
207000******************************************************************
207100     EXEC CICS SEND MAP(P05Y-MAP)
207200                    MAPSET(P05Y-SET)
207300                    FROM(PSTS05Y)
207400                    DATAONLY
207500                    CURSOR
207600                    FREEKB
207700                    RESP(WS-RESPONSE)
207800     END-EXEC
207900     MOVE WS-RESPONSE TO FILE-STATUS
208000     IF NOT SUCCESS
208100        MOVE 'P9020' TO ERR-PARAGRAPH
208200        PERFORM P9999-GOT-PROBLEM
208300     END-IF.
208400*
208500******************************************************************
208600 P9030-GET-MESSAGE.
208700******************************************************************
208800     MOVE PSTCA-SUB TO MSGLOG-SUB-CODE
208900     EXEC CICS READ
209000               DATASET(MSGLOG-VIA-CODE)
209100               INTO(MSGLOG-AREA)
209200               LENGTH(MSGLOG-RLGTH)
209300               RIDFLD(MSGLOG-KEY)
209400               KEYLENGTH(MSGLOG-KLGTH)
209500               RESP(WS-RESPONSE)
209600     END-EXEC
209700     MOVE WS-RESPONSE TO FILE-STATUS
209800     IF NOT SUCCESS
209900        IF PSTCA-SUB = 1
210000           MOVE 'NO MESSAGE ON FILE' TO MSGLOG-MESSAGE
210100        ELSE
210200           MOVE 'AUCUN MESSAGE'      TO MSGLOG-MESSAGE
210300        END-IF
210400     END-IF
210500     MOVE MSGLOG-CODE     TO MSGLOG-MSG-CODE
210600     MOVE '-'             TO MSGLOG-MSG-SEP
210700     MOVE MSGLOG-SUB-CODE TO MSGLOG-MSG-SUB-CODE.
210800*
210900 P9035-SEND-BUFFER.
211000*
211100     EXEC CICS SEND
211200               FROM(WS-BUFFER-DATA)
211300               LENGTH(WS-BUFFER-LGTH)
211400               ERASE
211500               RESP(WS-RESPONSE)
211600     END-EXEC
211700     MOVE WS-RESPONSE       TO FILE-STATUS
211800     IF NOT SUCCESS
211900        MOVE 'P9035-1'      TO ERR-PARAGRAPH
212000        MOVE 'SEND BUFFER'  TO ERR-KEY
212100        PERFORM P9999-GOT-PROBLEM
212200     END-IF
212300     EXEC CICS SEND
212400               CONTROL
212500               CURSOR(WS-BUFFER-CURSOR)
212600               RESP(WS-RESPONSE)
212700     END-EXEC
212800     MOVE WS-RESPONSE       TO FILE-STATUS
212900     IF NOT SUCCESS
213000        MOVE 'P9035-2'      TO ERR-PARAGRAPH
213100        MOVE 'SEND CURSOR'  TO ERR-KEY
213200        PERFORM P9999-GOT-PROBLEM
213300     END-IF.
213400******************************************************************
213500 P9100-XFER-TO-05A.
213600******************************************************************
213700     PERFORM P7120-DELETE-TSQUEUE
213800
213900     MOVE SCR05Y-DIST        TO PSTCA-DIST
214000     MOVE SCR05Y-SUB-DIST    TO PSTCA-SUB-DIST
214100     MOVE SCR05Y-EMP-NBR     TO PSTCA-EMP-NBR
214200                                P05CA-EMP-NO
214300     MOVE SCR05Y-EMP-NAME    TO P05CA-EMP-NAME
214400     MOVE P05Y-PGM           TO PSTCA-FROM-PROGRAM
214500
214600     EXEC CICS XCTL
214700               PROGRAM(P05A-PGM)
214800               COMMAREA(PSTCOMM-AREA)
214900               LENGTH(PSTCOMM-LGTH)
215000               RESP(WS-RESPONSE)
215100     END-EXEC
215200     MOVE WS-RESPONSE TO FILE-STATUS
215300     IF NOT SUCCESS
215400         MOVE 'P9100' TO ERR-PARAGRAPH
215500         PERFORM P9999-GOT-PROBLEM
215600     END-IF.
215700*
215800******************************************************************
215900 P9200-XFER-TO-MAIN-MENU.
216000******************************************************************
216100     PERFORM P7120-DELETE-TSQUEUE
216200
216300     EXEC CICS XCTL
216400               PROGRAM(P03-PGM)
216500               COMMAREA(PSTCOMM-AREA)
216600               LENGTH(PSTCOMM-LGTH)
216700               RESP(WS-RESPONSE)
216800     END-EXEC
216900     MOVE WS-RESPONSE TO FILE-STATUS
217000     IF NOT SUCCESS
217100         MOVE 'P9200' TO ERR-PARAGRAPH
217200         PERFORM P9999-GOT-PROBLEM
217300     END-IF.
217400*
215700*CNC0600 - RJA - B
215800******************************************************************
215900 P9250-XFER-TO-FLD-MENU.
216000******************************************************************
216100     PERFORM P7120-DELETE-TSQUEUE
216200
216300     EXEC CICS XCTL
216400               PROGRAM(P02-PGM)
216500               COMMAREA(PSTCOMM-AREA)
216600               LENGTH(PSTCOMM-LGTH)
216700               RESP(WS-RESPONSE)
216800     END-EXEC
216900     MOVE WS-RESPONSE TO FILE-STATUS
217000     IF NOT SUCCESS
217100         MOVE 'P9250' TO ERR-PARAGRAPH
217200         PERFORM P9999-GOT-PROBLEM
217300     END-IF.
217400*
031100*CNC0600 - RJA - E
217400*
217500******************************************************************
217600 P9300-LINK-P903.
217700******************************************************************
217800     EXEC CICS LINK
217900               PROGRAM(P903-PGM)
218000               COMMAREA(DATE-CONVERSION-PARMS)
218100               LENGTH(P903-LGTH)
218200               RESP(WS-RESPONSE)
218300     END-EXEC
218400     MOVE WS-RESPONSE              TO FILE-STATUS
218500     IF NOT SUCCESS
218600        MOVE 'P9300-1'             TO ERR-PARAGRAPH
218700        MOVE 'P903LINK'            TO ERR-KEY
218800        PERFORM P9999-GOT-PROBLEM
218900     END-IF.
219000******************************************************************
219100 P9500-SETUP-SCR998.
219200******************************************************************
219300     MOVE SPACES            TO P998COMM-AREA
219400     MOVE P05Y-PGM          TO P998CA-FROM-PROGRAM
219500     MOVE P05Y-MAP          TO P998CA-SCREEN-ID
219600     MOVE EIBCPOSN          TO P998CA-CURSOR-POS
219700     EXEC CICS XCTL
219800               PROGRAM(P998-PGM)
219900               COMMAREA(PSTCOMM-AREA)
220000               LENGTH(PSTCOMM-LGTH)
220100               RESP(WS-RESPONSE)
220200     END-EXEC
220300     MOVE WS-RESPONSE       TO FILE-STATUS
220400     IF NOT SUCCESS
220500        MOVE 'P9500'        TO ERR-PARAGRAPH
220600        PERFORM P9999-GOT-PROBLEM
220700     END-IF.
220800******************************************************************
220900 P9810-PROCESS-OFFSET.
221000******************************************************************
221100     MOVE PSTCA-DT-OS-FUN       TO PARM-CONV-TYPE
221200     MOVE PSTCA-DT-OS-DAYS      TO PARM-SEC-JULIAN-DAY
221300     MOVE PSTCA-DT-OS-HRMN      TO PARM-SEC-HRMN
221400     PERFORM P9300-LINK-P903.
221500*
221600******************************************************************
221700 P9990-CLEAR-SCREEN.
221800******************************************************************
221900     EXEC CICS SEND CONTROL
222000                    ERASE
222100                    FREEKB
222200     END-EXEC
222300     EXEC CICS RETURN END-EXEC.
222400*
222500******************************************************************
222600 P9999-GOT-PROBLEM.
222700******************************************************************
222800     MOVE P05Y-PGM TO ERR-PROGRAM
222900     MOVE DFHEIBLK TO ERR-EIBLK
223000     EXEC CICS LINK
223100               PROGRAM(PSTERR-PGM)
223200               COMMAREA(PSTERAR-AREA)
223300               LENGTH(PSTERAR-LGTH)
223400               RESP(WS-RESPONSE)
223500     END-EXEC
223600     MOVE WS-RESPONSE TO FILE-STATUS
223700     IF NOT SUCCESS
223800        EXEC CICS ABEND
223900                  ABCODE(PSTERR-ABCODE)
224000        END-EXEC
224100     END-IF
224200     EXEC CICS RETURN END-EXEC.
224300*
224400******************************************************************
224500 X9999-GOBACK.
224600******************************************************************
224700     GOBACK.
