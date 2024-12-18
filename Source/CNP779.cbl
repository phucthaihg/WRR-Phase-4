000100 IDENTIFICATION DIVISION.                                         00010009
000200 PROGRAM-ID. CNP779.                                              00020009
000300*----------------------------------------------------------------*00030002
000400*                  CALL BOARD REPORT                             *00040002
000500*----------------------------------------------------------------*00050002
000600*   DATE    LOG#  PRO  DESCRIPTION                               *00060002
000700* -------------------------------------------------------------- *00070002
000800* 01/95           DCK  MODIFIED CNP700 FOR CALL BOARD REPORT     *00080002
000900* 02/28/95        DCK  CHG TO GET HOME-AWAY TERMINALS FROM       *00090002
001000*                      '3B' CONTROL RECORDS                      *00100002
001100* 07/14/95        DCK  MODIFIED TO LOOK FOR SWITCHMAN            *00110002
001200*                      ASSIGNMENTS FOR TODAY OR YESTERDAY        *00120002
001300*                      DEPENDING ON THE STATUS OF THE            *00130002
001400*                      SWITCHMEN ASSIGNMENTS CONTROL RECORD      *00140002
001500* 02/13/97        ERW  CHANGED P2400 TO INCLUDE YARD JOBS.       *00150002
001600*                      CHANGED P5000 TO PRINT TO-PLACE EMPLOYEES.*00160002
001700* 03/10/97 E97002 SDR  CREWS IN-TOWN ENHANCEMENT (P2510 P2600).  *00170002
001800* 04/30/97        SDR  INCLUDE POOL 'JN' IN DIST/SDIST LM/JX.    *00180002
001900*                      CHANGED TRAINS ENROUTE LOGIC.             *00190002
002000*                      CHANGED POOLS LOGIC.                      *00200002
002100* 05/23/97        SAM  CHANGED TO PREVENT LO TRAINS IN LM/MC     *00210002
002200*                      FROM PRINTING ON THE JX REPORT.           *00220002
002300* 11/03/97        NRL  IF EXTRABOARD UNAVAILABE DON'T OVERLAY    *00230002
002400*                      MESSAGE WITH REST TIME.                   *00240002
002500* 06/21/00 CNC0311 PLS ADJUST PERSONAL REST CALCULATION TO       *00250002
002600*                      INCLUDE 2 HOUR CALL TIME.                 *00260009
002700* 08/28/00        AJK  ADVANCE ONE PAGE BEFORE PRINTING 1ST PAGE *00270002
002800* 03/04/03 C529   SAM  ADDED READ TO JOB SCHEDULE FILE TO SEE IF *00280002
002900*                      THE ASSIGNMENT IS CURRENT - IN-TOWN JOBS  *00290002
003000* 10/31/03 CNC0378 AJK EXTENDED SPAREBOARD SCHEDULE ENH.         *00300002
003100* 12/28/06         AJK IF EMP-PERS-REST IS ZEROES, CHANGE-TO     *00310002
003200*                      '000101010001' PRIOR TO ADDING 2 HOURS    *00320002
003300*                      TO IT.                                    *00330009
003400* 02/22/08 CNC0454 RXJ FOR CANADIAN EMPLOYEES, LEAD TIME WILL    *00340002
003500*                      NOT BE APPLIED TO THE REST EXPIRE TIME    *00350002
003600* 09/18/08 C780    AJK CHECK COMPANY CODE, NOT DIVISION CODE,    *00360005
003700*                      WHEN DETERMINING IF LEAD TIME IS TO BE    *00370005
003800*                      FACTORED INTO AN EMPLOYEE'S AVAILABLE TIME*00380005
003900*                      (P5200)                                   *00390009
004000* 04/17/09 CNC0484 CXB PRINT OUT THE HOURS OF SERVICE COUNTS FOR *00400009
004100*                      EACH EMPLOYEE.                            *00410009
004200* 05/20/09 CNC0484 BLD PASS EMPLOYEE'S TIME ZONE TO PS94B-PGM    *00420019
004300* 06/16/09 CNC0484 SXO FIXED THE HOS DISPLAYS.                   *00430025
004400* 08/04/09 CNC0484 RXS PROD FIX. USE LOCAL DATE TIME FOR REST    *00440028
004500*                      COMPARISIONS.                             *00450028
004600* 08/21/09 C831    DXM FIX - WAS SETTING TEMP XB TO UNASSIGNED   *00460030
004700*                      WHEN ON A GIVEN BOARD.                    *00470029
004800* 11/02/09 CNC0488 RXS POPULATE PS94-MSTR-HOS-GROUP              *00480039
004900* 10/11/10 C886    AJK GET STATUS CODE DESCRITPIONS OFF OF       *00490039
005000*                      THE STATUS CONTROL RECORD, RATHER THAN    *00500039
005100*                      OFF OF WSLOCODE TABLE. (P5100)            *00510039
005200* 09/15/11 CNC0516 VXD SHOW LAYOFF CODE AND RETURN DATE/TIME     *00520039
005300*                      FOR EMPLOYEE WHO ARE UNAVAILABLE OR       *00530039
005400*                      THERE IS VACANCY IN POOL                  *00540039
005500* 11/02/11 CNC0516 AXK MADE THE CHANGES TO CHECK FOR             *00550057
005600*                      E95'S RETURN > THAN CURRENT OFF-STATUS    *00560057
005610* 12/03/13 C975    BLD REPLACE WS-PRESENT-TIME-CENT BY           *00570058
005620*                      WS-LOCAL-DATE-TIME-CENT IN ORDER TO       *00580058
005630*                      DISPLAY EXTRABOARD REST.                  *00590058
005640* 01/12/16 CNC0573 MFO MASK STATUS/REASON FIELDS.                 00600060
005650* 02/23/16 CNC0576 MFO MASK STATUS/REASON FIELDS - HOLD TURN.     00610068
005660* 06/30/16 C1129   RXB ADD BOOK-ON RECORD FROM TASK LIST TO      *00620071
005670*                      DISPLAY CORRECT RETURN DATE FOR VACATION  *00630071
005700*----------------------------------------------------------------*00640028
005800                                                                  00650002
005900 ENVIRONMENT DIVISION.                                            00660002
006000                                                                  00670002
006100 CONFIGURATION SECTION.                                           00680002
006200                                                                  00690002
006300 SOURCE-COMPUTER. IBM-9370.                                       00700002
006400 OBJECT-COMPUTER. IBM-9370.                                       00710002
006500                                                                  00720002
006600 INPUT-OUTPUT SECTION.                                            00730002
006700                                                                  00740002
006800 FILE-CONTROL.                                                    00750002
006900                                                                  00760002
007000     COPY FCAJ.                                                   00770002
007100     COPY FCJS.                                                   00780002
007200     COPY FCASGN.                                                 00790002
007300     COPY FCCNTL.                                                 00800002
007400     COPY FCEB.                                                   00810002
007500     COPY FCMSTR.                                                 00820002
007600     COPY FCSEN.                                                  00830002
007700     COPY FCTRCN.                                                 00840002
007800     COPY FCUFP.                                                  00850002
007900     COPY FCSWASGN.                                               00860002
008000*CNC0516-BEG                                                      00870036
008100     COPY FCTASK.                                                 00880036
008200     COPY FCMSTR2.                                                00890056
008300*CNC0516-END                                                      00900036
008400                                                                  00910002
008500     SELECT PL                                                    00920002
008600     ASSIGN TO PR-CALLBRD                                         00930002
008700     ORGANIZATION IS SEQUENTIAL                                   00940002
008800     ACCESS IS SEQUENTIAL                                         00950002
008900     FILE STATUS IS FILE-STATUS.                                  00960002
009000                                                                  00970002
009100 DATA DIVISION.                                                   00980002
009200                                                                  00990002
009300 FILE SECTION.                                                    01000002
009400                                                                  01010002
009500     COPY FSAJ.                                                   01020002
009600     COPY FSJS.                                                   01030002
009700     COPY FSASGN.                                                 01040002
009800     COPY FSCNTL.                                                 01050002
009900     COPY FSEB.                                                   01060002
010000     COPY FSMSTR.                                                 01070002
010100     COPY FSSEN.                                                  01080002
010200     COPY FSTRCN.                                                 01090002
010300     COPY FSUFP.                                                  01100002
010400     COPY FSSWASGN.                                               01110002
010500*CNC0516-BEG                                                      01120036
010600     COPY FSTASK.                                                 01130056
010700     COPY FSMSTR2.                                                01140056
010800*CNC0516-END                                                      01150036
010900                                                                  01160002
011000 FD  PL                                                           01170002
011100     LABEL RECORDS ARE OMITTED                                    01180021
011200     RECORDING MODE  F.                                           01190021
011300 01  PL-R                            PIC X(132).                  01200002
011400                                                                  01210002
011500 WORKING-STORAGE SECTION.                                         01220002
011600                                                                  01230002
011700 01  SUBCRIPTS.                                                   01240002
011800     05  RPT-MAX                     PIC 9(002) VALUE 98.         01250002
011900     05  I                           PIC 9(003) VALUE ZEROS.      01260002
012000     05  J                           PIC 9(003) VALUE ZEROS.      01270002
012100     05  X2                          PIC 9(003) VALUE ZEROS.      01280002
012200     05  SB-SUB                      PIC 9(002) VALUE ZEROS.      01290002
012300     05  OB-SUB                      PIC 9(002) VALUE ZEROS.      01300002
012400     05  POOL-SUB                    PIC 9(002) VALUE ZEROS.      01310002
012500     05  EXT-SUB                     PIC 9(002) VALUE ZEROS.      01320002
012600     05  SUB2                        PIC 9(002) VALUE ZEROS.      01330002
012700     05  WS-POS-CNT                  PIC 9(003) VALUE ZEROS.      01340002
012800     05  POOL-POS-CNT                PIC 9(002) VALUE ZEROS.      01350002
012900     05  SAVE-CNT                    PIC 9(002) VALUE ZEROS.      01360002
013000     05  WS-YESTERDAY                PIC S9(01) VALUE ZEROS.      01370002
013100                                                                  01380002
013200 01  HARD-CODED-VALUES.                                           01390002
013300     05  WS-DIST-LM                  PIC X(002) VALUE 'LM'.       01400002
013400     05  WS-SUB-DIST-JX              PIC X(002) VALUE 'JX'.       01410002
013500     05  WS-SUB-DIST-MC              PIC X(002) VALUE 'MC'.       01420002
013600     05  WS-POOL-JN                  PIC X(002) VALUE 'JN'.       01430002
013700                                                                  01440002
013800 01  WS-FLAGS-AND-SWITCHES.                                       01450002
013900     05  NEW-POOL-FLAG               PIC X(001) VALUE '0'.        01460002
014000         88  NEW-POOL                           VALUE '1'.        01470002
014100     05  GOT-EMPLOYEE-FLAG           PIC X(001) VALUE '0'.        01480002
014200         88  GOT-EMPLOYEE                       VALUE '1'.        01490002
014300     05  DONE-CODE                   PIC X(001) VALUE '0'.        01500002
014400         88  NOT-DONE                           VALUE '0'.        01510002
014500         88  DONE                               VALUE '1'.        01520002
014600     05  STOP-JOB-CODE               PIC X(001) VALUE '0'.        01530002
014700         88  SAME-JOB                           VALUE '0'.        01540002
014800         88  END-OF-JOB                         VALUE '1'.        01550002
014900     05  XB-DONE-CODE                PIC X(001) VALUE '0'.        01560002
015000         88  XB-NOT-DONE                        VALUE '0'.        01570002
015100         88  XB-DONE                            VALUE '1'.        01580002
015200     05  ASGN-DONE-CODE              PIC 9(001) VALUE 0.          01590002
015300         88  ASGN-DONE                          VALUE 1.          01600002
015400     05  EN-ID-POOL-FLAG             PIC X(001) VALUE '0'.        01610002
015500         88  EN-ID-POOL                         VALUE '1'.        01620002
015600     05  TR-ID-POOL-FLAG             PIC X(001) VALUE '0'.        01630002
015700         88  TR-ID-POOL                         VALUE '1'.        01640002
015800     05  POOL-SERVICE-FLAG           PIC X(001) VALUE ' '.        01650002
015900         88  MINE-TURN-SVC                      VALUE 'M'.        01660002
016000     05  SEARCH-DONE-CODE            PIC 9(001) VALUE 0.          01670002
016100         88  SEARCH-NOT-DONE                    VALUE 0.          01680002
016200         88  SEARCH-DONE                        VALUE 1.          01690002
016300     05  TEMP-EMP-FLAG               PIC X(001) VALUE ' '.        01700002
016400         88  TEMP-EMP-FOUND                     VALUE 'Y'.        01710002
016500     05  DIST-SDIST-POOL-FLAG        PIC X(001) VALUE ' '.        01720002
016600         88  DIST-SDIST-POOL-LM-MC-JN           VALUE 'Y'.        01730002
016700     05  TRAINS-DONE-CODE            PIC X(001) VALUE '0'.        01740002
016800         88  TRAINS-DONE                        VALUE '1'.        01750002
016900         88  TRAINS-NOT-DONE                    VALUE '0'.        01760002
017000     05  LOCALS-DONE-CODE            PIC X(001) VALUE '0'.        01770002
017100         88  LOCALS-DONE                        VALUE '1'.        01780002
017200         88  LOCALS-NOT-DONE                    VALUE '0'.        01790002
017300     05  CC-FOUND-FLAG               PIC 9(001) VALUE 0.          01800002
017400         88  CC-FOUND                           VALUE 1.          01810002
017500     05  UNDISTURBED-REST-FLAG       PIC X(001) VALUE ' '.        01820002
017600         88  UNDISTURBED-REST                   VALUE 'Y'.        01830002
017700     05  CURRENT-JOB-FLAG            PIC X(001) VALUE ' '.        01840002
017800         88  JOB-CURRENT                        VALUE 'Y'.        01850002
017900     05  WS-XB-SCHEDULED-FLAG        PIC X(001) VALUE SPACES.     01860002
018000         88  WS-SCHEDULED-XB                    VALUE 'Y' 'E'.    01870002
018100         88  WS-XB-SCHEDULED                    VALUE 'Y'.        01880002
018200         88  WS-XB-EXTENDED-SCHED               VALUE 'E'.        01890002
018300     05  ON-REST-DAY-FLAG            PIC X(001) VALUE SPACES.     01900002
018400         88  NOT-ON-REST-DAY                    VALUE ' '.        01910002
018500         88  ON-REST-DAY                        VALUE 'Y'.        01920002
018600     05  WS-JS-SW                    PIC X(001) VALUE 'F'.        01930002
018700         88  FIRST-JS                           VALUE 'F'.        01940002
018800         88  MORE-JS                            VALUE '1'.        01950002
018900         88  JS-DONE                            VALUE '9'.        01960002
019000     05  WS-SCHEDULED-TURN-SW        PIC X(001) VALUE SPACE.      01970002
019100         88  WS-SCHEDULED-REST-DAY              VALUE 'D'.        01980002
019200         88  WS-SCHEDULED-REST-PERIOD           VALUE 'P'.        01990002
019300     05  DISPLAY-EMP-FLAG            PIC X(001) VALUE 'N'.        02000002
019400         88  DONT-DISPLAY-EMP                   VALUE 'N'.        02010002
019500         88  DISPLAY-EMP                        VALUE 'Y'.        02020002
019600     05  WS-APPLY-LEAD-TIME-FLAG     PIC X(001) VALUE 'Y'.        02030002
019700         88  APPLY-LEAD-TIME                    VALUE 'Y'.        02040002
019800         88  DONT-APPLY-LEAD-TIME               VALUE 'N'.        02050002
019900     05  WS-COMPANY-CODE-FLAG        PIC X(001) VALUE ' '.        02060006
020000         88  WS-CANADIAN-COMPANY                VALUE 'C'.        02070006
020100         88  WS-US-COMPANY                      VALUE 'U'.        02080006
020200     05  WS-TEMP-BOARD-FLAG          PIC X(001) VALUE 'D'.        02090029
020300         88 TEMP-DIFF-BOARD                     VALUE 'D'.        02100029
020400         88 TEMP-SAME-BOARD                     VALUE 'S'.        02110029
020500*CNC0516-BEG                                                      02120034
020600     05  WS-DUEBACK-FOUND-FLAG       PIC 9      VALUE 0.          02130034
020700         88  WS-DUEBACK-FOUND-N                 VALUE 0.          02140034
020800         88  WS-DUEBACK-FOUND-Y                 VALUE 1.          02150034
020900     05  WS-TASK-DONE-CODE           PIC X      VALUE 'N'.        02160034
021000         88  TASK-NOT-DONE                      VALUE 'N'.        02170034
021100         88  TASK-DONE                          VALUE 'Y'.        02180034
021200*CNC0516-END                                                      02190034
021210*CNC0573 - BEG                                                    02200066
021211     05  WS-MASK-FLD-SCR-FL          PIC X      VALUE SPACE.      02210066
021212         88  WS-MASK-FLD-SCR-YES                VALUE 'Y'.        02220066
021230*CNC0573 - END                                                    02230066
021300                                                                  02240002
021400 01  MARRIED-CRAFT-FLAGS.                                         02250002
021500     05  EN-FI-MARRIED-FLAG          PIC X(001) VALUE SPACES.     02260002
021600         88  EN-FI-MARRIED                      VALUE 'Y', '1'.   02270002
021700         88  EN-SE-MARRIED                      VALUE '1'.        02280002
021800     05  EN-ET-MARRIED-FLAG          PIC X(001) VALUE SPACES.     02290002
021900         88  EN-ET-MARRIED                      VALUE 'Y'.        02300002
022000     05  CO-BK-MARRIED-FLAG          PIC X(001) VALUE SPACES.     02310002
022100         88  CO-BK-MARRIED                      VALUE 'Y'.        02320002
022200     05  CO-B1-MARRIED-FLAG          PIC X(001) VALUE SPACES.     02330002
022300         88  CO-B1-MARRIED                      VALUE 'Y'.        02340002
022400     05  CO-B2-MARRIED-FLAG          PIC X(001) VALUE SPACES.     02350002
022500         88  CO-B2-MARRIED                      VALUE 'Y'.        02360002
022600     05  CO-BG-MARRIED-FLAG          PIC X(001) VALUE SPACES.     02370002
022700         88  CO-BG-MARRIED                      VALUE 'Y'.        02380002
022800     05  CO-TT-MARRIED-FLAG          PIC X(001) VALUE SPACES.     02390002
022900         88  CO-TT-MARRIED                      VALUE 'Y'.        02400002
023000     05  B1-B2-MARRIED-FLAG          PIC X(001) VALUE SPACES.     02410002
023100         88  B1-B2-MARRIED                      VALUE 'Y'.        02420002
023200                                                                  02430002
023300 01  WS-TIME-ZONE                    PIC X(001) VALUE SPACES.     02440019
023400     88  WS-SYSTEM-TIME-ZONE                    VALUE 'E'.        02450019
023500 01  WS-LIMBO-TIME.                                               02460010
023600     05 FILLER                       PIC X(002) VALUE SPACES.     02470010
023700     05 WS-LIMBO-TM.                                              02480010
023800        10 WS-LIMBO-TM-HH            PIC 9(002) VALUE ZEROES.     02490010
023900        10 WS-LIMBO-TM-MM            PIC 9(002) VALUE ZEROES.     02500010
024000 01  WS-TOT-TIME.                                                 02510010
024100     05 FILLER                       PIC X(001) VALUE SPACES.     02520010
024200     05 WS-TOT-TM.                                                02530010
024300        10 WS-TOT-TM-HH              PIC 9(003) VALUE ZEROES.     02540010
024400        10 WS-TOT-TM-MM              PIC 9(002) VALUE ZEROES.     02550010
024500 01  WS-CONSEC-STARTS                PIC X(002) VALUE SPACES.     02560016
024600                                                                  02570010
024700 01  WS-SWASSGN-ASGN.                                             02580002
024800     05  WS-SW-EXTRA                 PIC X(002) VALUE 'EX'.       02590002
024900     05  WS-SW-POSITION              PIC X(004) VALUE SPACES.     02600002
025000     05  WS-SW-CRAFT                 PIC X(002) VALUE SPACES.     02610002
025100                                                                  02620002
025200 01  WS-FORMAT-NAME-AUG                         VALUE SPACES.     02630002
025300     05  WS-FORMAT-EMP-NAME          PIC X(021).                  02640002
025400     05  WS-FORMAT-NAME-AUG-FIELD    PIC X(005).                  02650002
025500                                                                  02660002
025600 01  FORMAT-DATE-AREA.                                            02670002
025700     05  FORM-YR                     PIC X(002) VALUE SPACES.     02680002
025800     05  FILLER                      PIC X(001) VALUE '/'.        02690002
025900     05  FORMAT-PART-DATE.                                        02700002
026000         10  FORM-MO                 PIC X(002) VALUE SPACES.     02710002
026100         10  FILLER                  PIC X(001) VALUE '/'.        02720002
026200         10  FORM-DY                 PIC X(002) VALUE SPACES.     02730002
026300         10  FILLER                  PIC X(001) VALUE '-'.        02740002
026400         10  FORM-HRMN               PIC X(004) VALUE SPACES.     02750002
026500                                                                  02760002
026600 01  WS-TODAYS-DATE.                                              02770002
026700     05  CUR-YR                      PIC 9(002) VALUE ZEROS.      02780002
026800     05  CUR-MO                      PIC 9(002) VALUE ZEROS.      02790002
026900     05  CUR-DY                      PIC 9(002) VALUE ZEROS.      02800002
027000                                                                  02810002
027100 01  POOL-TABLE                                 VALUE SPACES.     02820002
027200     05  POOL-DATA OCCURS 98 TIMES.                               02830002
027300         10  POOL-P                  PIC X(002).                  02840002
027400                                                                  02850002
027500 01  SPAREBOARD-TABLE                           VALUE SPACES.     02860002
027600     05  SB-DATA OCCURS 98 TIMES.                                 02870002
027700         10  SB-P                    PIC X(002).                  02880002
027800                                                                  02890002
027900 01  WS-YESTERDAY-START-TIME         PIC X(004) VALUE SPACES.     02900002
028000 01  WS-YESTERDAY-END-TIME           PIC X(004) VALUE SPACES.     02910002
028100 01  WS-JOB-SCHED-REST-DAYS.                                      02920002
028200     02  WS-JOB-SCHED-REST-DAY       PIC X(001) OCCURS 7 TIMES.   02930002
028300         88  WS-JOB-ON-REST-DAY                 VALUE '1'.        02940002
028400                                                                  02950002
028500 01  SAVE-CNTL-AREA                  PIC X(256) VALUE SPACES.     02960002
028600 01  SAVE-ASGN-AREA                  PIC X(128) VALUE SPACES.     02970002
028700 01  XXXX-ASGNKEY1                              VALUE SPACES.     02980002
028800     05  XX-ASGN-JOB-TYPE            PIC X(001).                  02990002
028900     05  XX-ASGN-DIST                PIC X(002).                  03000002
029000     05  XX-ASGN-SUB-DIST            PIC X(002).                  03010002
029100     05  XX-ASGN-JOB                 PIC X(008).                  03020002
029200     05  XX-ASGN-REC-TYPE            PIC X(001).                  03030002
029300     05  XX-ASGN-DATE-TIME           PIC X(010).                  03040002
029400 01  XXXX-ASGNKEY2.                                               03050002
029500     05  XX-ASGN-EMP                 PIC 9(009) VALUE ZEROS.      03060002
029600     05  XX-ASGN2-REC-TYPE           PIC X(001) VALUE SPACES.     03070002
029700     05  XX-ASGN2-DATE-TIME          PIC X(010) VALUE SPACES.     03080002
029800                                                                  03090002
029900 01  WORK-UFPPOS-KEY.                                             03100002
030000     05  POS-DIST                    PIC X(002) VALUE SPACES.     03110002
030100     05  POS-SUB-DIST                PIC X(002) VALUE SPACES.     03120002
030200     05  POS-POOL                    PIC X(002) VALUE SPACES.     03130002
030300     05  POS-CC                      PIC X(002) VALUE SPACES.     03140002
030400     05  POS-TERMINAL.                                            03150002
030500         10  FILLER                  PIC 9(001) VALUE 0.          03160002
030600         10  POS-TERM                PIC 9(001) VALUE 0.          03170002
030700     05  POS-TIME.                                                03180002
030800         10  POS-ON-OFF              PIC X(001) VALUE SPACES.     03190002
030900             88  POS-ON                         VALUE '0'         03200002
031000                                                THRU '8'.         03210002
031100             88  POS-OFF                        VALUE '9'.        03220002
031200         10  POS-DATE-TIME           PIC X(014) VALUE SPACES.     03230002
031300                                                                  03240002
031400 01  WORK-XB-POS-KEY                            VALUE SPACES.     03250002
031500     05  XB-POS-DIST                 PIC X(002).                  03260002
031600     05  XB-POS-SUB-DIST             PIC X(002).                  03270002
031700     05  XB-POS-CC                   PIC X(002).                  03280002
031800     05  XB-POS.                                                  03290002
031900         10  XB-POS-ON-OFF           PIC X(001).                  03300002
032000         10  XB-POS-BOARD            PIC X(001).                  03310002
032100         10  XB-POS-TIME             PIC X(014).                  03320002
032200                                                                  03330002
032300 01  WORK-XB-TURN-KEY                           VALUE SPACES.     03340002
032400     05  XB-TURN-DIST                PIC X(002).                  03350002
032500     05  XB-TURN-SUB-DIST            PIC X(002).                  03360002
032600     05  XB-TURN-CC                  PIC X(002).                  03370002
032700     05  XB-TURN                     PIC X(004).                  03380002
032800                                                                  03390002
032900 01  WORK-SENKEY2                               VALUE SPACES.     03400002
033000     05  WK-SEN-EMP-NO               PIC X(009).                  03410002
033100     05  WK-SEN-ROSTER               PIC X(004).                  03420002
033200     05  WK-SEN-CC                   PIC X(002).                  03430002
033300                                                                  03440002
033400 01  WEEK-DAY                        PIC 9(002) VALUE 01.         03450002
033500 01  DAY1                            PIC 9(002) VALUE ZEROS.      03460002
033600 01  SW-DAY1                         PIC 9(002) VALUE ZEROS.      03470002
033700 01  WS-TERM                         PIC 9(002) VALUE ZEROS.      03480002
033800 01  CC-MAX                          PIC 9(002) VALUE 10.         03490002
033900 01  CC-SUB                          PIC 9(002) VALUE ZEROS.      03500002
034000 01  CC-SUB2                         PIC 9(002) VALUE ZEROS.      03510002
034100 01  WS-CNTL-XB                      PIC X(002) VALUE SPACES.     03520002
034200                                                                  03530002
034300 01  WORK-TIME.                                                   03540002
034400     05  WORK-DATE.                                               03550002
034500         10  WK-YR                   PIC 9(002) VALUE ZEROS.      03560002
034600         10  WK-MO                   PIC 9(002) VALUE ZEROS.      03570002
034700         10  WK-DY                   PIC 9(002) VALUE ZEROS.      03580002
034800     05  WORK-HR-MN.                                              03590002
034900         10  WK-HR                   PIC 9(002) VALUE ZEROS.      03600002
035000         10  WK-MN                   PIC 9(002) VALUE ZEROS.      03610002
035100                                                                  03620002
035200 01  WS-END-DATE-TIME.                                            03630002
035300     05  WS-END-DATE                 PIC 9(006) VALUE ZEROS.      03640002
035400     05  WS-END-TIME                 PIC 9(004) VALUE ZEROS.      03650002
035500                                                                  03660002
035600 01  CRAFT-TABLE-AREA                           VALUE SPACES.     03670002
035700     05  CRAFT-TABLE-ARRAY OCCURS 10.                             03680002
035800         10  PRIMARY-CC              PIC X(002).                  03690002
035900         10  PRIMARY-CC-LAST-KEY     PIC X(025).                  03700002
036000             88  PRIMARY-CC-DONE                VALUE             03710002
036100                                     'DONE                     '. 03720002
036200         10  ASSOC-CC-ARRAY.                                      03730002
036300             15  ASSOC-CC            PIC X(002) OCCURS 10.        03740002
036400             15  ASSOC-CC-TYPE       PIC X(001) OCCURS 10.        03750002
036500                 88  ASSOC-CC-OPT               VALUE 'Y'.        03760002
036600                                                                  03770002
036700 01  FOREMAN-OR-SWITCHMAN            PIC X(010).                  03780002
036800     88  HE-IS-FOREMAN                         VALUE 'FOREMAN   '.03790002
036900     88  HE-IS-SWITCHMAN                       VALUE 'SWITCHMAN '.03800002
037000                                                                  03810002
037100 01  TITLE-CUSTOMER.                                              03820002
037200     05  FILLER                      PIC X(001) VALUE SPACES.     03830002
037300     05  TITLE-DATE-TIME.                                         03840002
037400         10  TIT-YR                  PIC 9(002) VALUE ZEROS.      03850002
037500         10  FILLER                  PIC X(001) VALUE '/'.        03860002
037600         10  TIT-MO                  PIC 9(002) VALUE ZEROS.      03870002
037700         10  FILLER                  PIC X(001) VALUE '/'.        03880002
037800         10  TIT-DY                  PIC 9(002) VALUE ZEROS.      03890002
037900         10  FILLER                  PIC X(001) VALUE '-'.        03900002
038000         10  TIT-HR                  PIC Z9     VALUE ZEROS.      03910002
038100         10  TIT-MN                  PIC 9(002) VALUE ZEROS.      03920002
038200     05  FILLER                      PIC X(013) VALUE SPACES.     03930002
038300     05  TITLE-CUST-NAME             PIC X(026) VALUE SPACES.     03940002
038400     05  FILLER                      PIC X(019) VALUE SPACES.     03950002
038500     05  FILLER                      PIC X(005) VALUE 'PAGE '.    03960002
038600     05  REPORT-TITLE-PAGE           PIC 9(003) VALUE ZEROS.      03970002
038700                                                                  03980002
038800 01  REPORT-TITLE-TERMINAL.                                       03990002
038900     05  FILLER                      PIC X(027) VALUE SPACES.     04000002
039000     05  REPORT-TITLE-TERM           PIC X(026) VALUE SPACES.     04010002
039100     05  FILLER                      PIC X(026) VALUE SPACES.     04020002
039200                                                                  04030002
039300 01  REPORT-TITLE.                                                04040002
039400     05  FILLER                      PIC X(031) VALUE SPACES.     04050002
039500     05  FILLER                      PIC X(017) VALUE             04060002
039600         'CALL BOARD REPORT'.                                     04070002
039700     05  FILLER                      PIC X(017) VALUE SPACES.     04080002
039800     05  FILLER                      PIC X(006) VALUE             04090002
039900         'CNP779'.                                                04100002
040000                                                                  04110002
040100 01  LINE-COUNT                      PIC 9(002) VALUE ZEROS.      04120002
040200 01  LINE-OF-DASHES                  PIC X(132) VALUE ALL '-'.    04130002
040300 01  SPACE-LINE                      PIC X(001) VALUE SPACES.     04140002
040400                                                                  04150002
040500                                                                  04160002
040600 01  SAVE-CREW-INFORMATION                      VALUE SPACES.     04170002
040700     05  SAVE-CREW-INFO OCCURS 30 TIMES.                          04180002
040800         10  SAVE-CREW-CODE          PIC X(002).                  04190002
040900         10  SAVE-CREW-CRAFT         PIC X(010).                  04200002
041000         10  SAVE-CREW-POSITION      PIC X(002).                  04210002
041100         10  SAVE-CREW-NAME          PIC X(026).                  04220002
041200         10  SAVE-CREW-TEMP-EMP-FLAG PIC X(001).                  04230002
041300             88 SAVE-CREW-TEMP-EMP              VALUE 'Y'.        04240002
041400*CNC0516-BEG                                                      04250049
041500         10  SAVE-CREW-RESTED        PIC X(014).                  04260039
041600*CNC0516-END                                                      04270049
041700         10  SAVE-CREW-TURN          PIC X(004).                  04280002
041800         10  SAVE-CREW-HOS-TOTAL.                                 04290010
041900             20 SAVE-CREW-HOS-TOTAL-HR PIC X(003).                04300010
042000             20 SAVE-CREW-HOS-TOTAL-MM PIC X(002).                04310010
042100         10  SAVE-CREW-HOS-LIMBO.                                 04320010
042200             20 SAVE-CREW-HOS-LIMBO-HR PIC X(002).                04330010
042300             20 SAVE-CREW-HOS-LIMBO-MM PIC X(002).                04340010
042400         10  SAVE-CREW-HOS-ST        PIC X(002).                  04350017
042500                                                                  04360002
042600 01  SAVE-TEMP-CREW-INFORMATION                 VALUE SPACES.     04370002
042700     05  SAVE-TEMP-CREW-INFO OCCURS 30 TIMES.                     04380002
042800         10  SAVE-TEMP-CREW-CRAFT    PIC X(010).                  04390002
042900         10  SAVE-TEMP-CREW-NAME     PIC X(026).                  04400002
043000*CNC0516-BEG                                                      04410049
043100         10  SAVE-TEMP-CREW-RESTED   PIC X(014).                  04420039
043200*CNC0516-END                                                      04430049
043300         10  SAVE-TEMP-CREW-TOTAL.                                04440010
043400             20 SAVE-TEMP-CREW-TOTAL-HR PIC X(003).               04450010
043500             20 SAVE-TEMP-CREW-TOTAL-MM PIC X(002).               04460010
043600         10  SAVE-TEMP-CREW-LIMBO.                                04470010
043700             20 SAVE-TEMP-CREW-LIMBO-HR PIC X(002).               04480010
043800             20 SAVE-TEMP-CREW-LIMBO-MM PIC X(002).               04490010
043900         10  SAVE-TEMP-CREW-ST       PIC X(002).                  04500017
044000                                                                  04510002
044100 01  TRAIN-COUNT                     PIC 9(003) VALUE ZEROS.      04520002
044200 01  LOCAL-COUNT                     PIC 9(003) VALUE ZEROS.      04530002
044300 01  CREW-COUNT                      PIC 9(003) VALUE ZEROS.      04540002
044400 01  AJ-COUNT                        PIC 9(003) VALUE ZEROS.      04550002
044500 01  POS-COUNT                       PIC 9(002) VALUE ZEROS.      04560002
044600 01  SAVE-TERM                       PIC 9(001) VALUE ZEROS.      04570002
044700 01  WK-DESC                         PIC X(020) VALUE SPACES.     04580002
044800 01  WK-FROM                         PIC X(014) VALUE SPACES.     04590002
044900                                                                  04600002
045000 01  TRAINS-ENROUTE-TITLE.                                        04610002
045100     02  FILLER                   PIC X     VALUE SPACES.         04620002
045200     02  FILLER                   PIC X(15)                       04630002
045300                                  VALUE 'TRAINS ENROUTE '.        04640002
045400     02  FILLER                   PIC X(65) VALUE SPACE.          04650002
045500 01  TRAINS-ENROUTE-1.                                            04660002
045600     02  FILLER                   PIC X(05) VALUE SPACE.          04670002
045700     02  TRAIN-ENROUTE            PIC X(10) VALUE SPACE.          04680002
045800     02  FILLER                   PIC X  VALUE SPACE.             04690002
045900     02  TRAIN-DUTY               PIC X(14) VALUE SPACES.         04700002
046000     02  TRAIN-ENROUTE-FROM       PIC X(17) VALUE SPACE.          04710002
046100     02  FILLER                   PIC X  VALUE SPACE.             04720002
046200     02  FILLER                   PIC XXX VALUE 'AT '.            04730002
046300     02  TRAIN-YR                 PIC XX   VALUE SPACE.           04740002
046400     02  TRAIN-FIL1               PIC X    VALUE SPACE.           04750002
046500     02  TRAIN-MO                 PIC XX   VALUE SPACE.           04760002
046600     02  TRAIN-FIL2               PIC X    VALUE SPACE.           04770002
046700     02  TRAIN-DY                 PIC XX   VALUE SPACE.           04780002
046800     02  TRAIN-FIL3               PIC X    VALUE SPACE.           04790002
046900     02  TRAIN-HR-MN              PIC XXXX VALUE SPACE.           04800002
047000     02  FILLER                   PIC X(16) VALUE SPACE.          04810002
047100 01  TRAINS-ENROUTE-2.                                            04820002
047200     02  FILLER                   PIC X(10) VALUE SPACE.          04830014
047300     02  TRAIN-CC                 PIC X(10) VALUE SPACE.          04840002
047400     02  FILLER                   PIC XX VALUE SPACE.             04850002
047500     02  TRAIN-EMP                PIC X(26) VALUE SPACE.          04860002
047600     02  FILLER                   PIC XX VALUE SPACE.             04870002
047700     02  TRAIN-TURN               PIC X(4) VALUE SPACE.           04880002
047800     02  FILLER                   PIC X(02) VALUE SPACE.          04890009
047900     02  TRAIN-HOS-AREA           PIC X(21) VALUE SPACE.          04900027
048000                                                                  04910009
048100 01  LOCALS-ENROUTE-1.                                            04920002
048200     02  FILLER                   PIC X(05) VALUE SPACE.          04930002
048300     02  LOCAL-ENROUTE            PIC X(10) VALUE SPACE.          04940002
048400     02  FILLER                   PIC X  VALUE SPACE.             04950002
048500     02  LOCAL-ENROUTE-NAME       PIC X(20) VALUE SPACES.         04960002
048600     02  LOCAL-DUTY               PIC X(13) VALUE SPACES.         04970002
048700     02  LOCAL-ENROUTE-FROM       PIC X(14) VALUE SPACE.          04980002
048800     02  FILLER                   PIC X  VALUE SPACE.             04990002
048900     02  FILLER                   PIC XXX VALUE 'AT '.            05000002
049000     02  LOCAL-YR                 PIC XX   VALUE SPACE.           05010002
049100     02  LOCAL-FIL1               PIC X    VALUE SPACE.           05020002
049200     02  LOCAL-MO                 PIC XX   VALUE SPACE.           05030002
049300     02  LOCAL-FIL2               PIC X    VALUE SPACE.           05040002
049400     02  LOCAL-DY                 PIC XX   VALUE SPACE.           05050002
049500     02  LOCAL-FIL3               PIC X    VALUE SPACE.           05060002
049600     02  LOCAL-HR-MN              PIC XXXX VALUE SPACE.           05070002
049700                                                                  05080002
049800 01  MORE-TURNS-CODE              PIC X VALUE '0'.                05090002
049900     88  MORE-TURNS                     VALUE '1'.                05100002
050000     88  NO-MORE-TURNS                  VALUE '0'.                05110002
050100 01  POOLS-DONE-CODE              PIC X VALUE '0'.                05120002
050200     88  POOLS-DONE                     VALUE '1'.                05130002
050300     88  POOLS-NOT-DONE                 VALUE '0'.                05140002
050400*                                                                 05150002
050500 01  CREWS-IN-TOWN-AREA.                                          05160002
050600     02  CREWS-IN-TOWN-TITLE.                                     05170002
050700*        03  FILLER              PIC X          VALUE SPACES.     05180027
050800         03  FILLER              PIC X(14)      VALUE             05190002
050900             'CREWS IN TOWN '.                                    05200002
051000         03  FILLER              PIC X(65)      VALUE SPACES.     05210002
051100     02  CREWS-IN-TOWN-1.                                         05220002
051200         03  FILLER              PIC X(1)       VALUE SPACES.     05230027
051300         03  FILLER              PIC X(6)       VALUE             05240002
051400             'POOL: '.                                            05250002
051500         03  CREWS-IN-TOWN-POOL  PIC X(14).                       05260002
051600         03  FILLER              PIC X(24)      VALUE SPACES.     05270027
051700*CNC0516-BEG                                                      05280049
051800         03  FILLER              PIC X(14)      VALUE             05290039
051900             'RESTED/STATUS'.                                     05300002
052000         03  FILLER              PIC X(21)      VALUE SPACES.     05310039
052100*CNC0516-END                                                      05320049
052200     02  CREWS-IN-TOWN-2.                                         05330002
052300         03  FILLER              PIC X(02)      VALUE SPACES.     05340027
052400*        03  CREWS-IT-POSITION   PIC X(2).                        05350027
052500*        03  FILLER              PIC X(1)       VALUE SPACES.     05360027
052600         03  CREWS-IT-CRAFT      PIC X(10).                       05370027
052700         03  FILLER              PIC X(1)       VALUE SPACES.     05380027
052800         03  CREWS-IT-TURN       PIC X(4).                        05390002
052900         03  FILLER              PIC X(1)       VALUE SPACES.     05400027
053000*CNC0516-BEG                                                      05410049
053100         03  CREWS-IT-NAME       PIC X(25).                       05420039
053200*CNC0516-END                                                      05430049
053300         03  FILLER              PIC X(1)       VALUE SPACES.     05440027
053400*CNC0516-BEG                                                      05450049
053500         03  CREWS-IT-RESTED     PIC X(14).                       05460039
053600*CNC0516-END                                                      05470049
053700         03  FILLER              PIC X(01)      VALUE SPACES.     05480009
053800         03  CREWS-IT-HOS-AREA   PIC X(21)      VALUE SPACES.     05490027
053900*        03  FILLER              PIC X(01)      VALUE SPACES.     05500027
054000                                                                  05510010
054100     02  CREWS-MESS-LINE.                                         05520002
054200         03  FILLER              PIC X(09)      VALUE SPACES.     05530002
054300         03  CREW-MESSAGE        PIC X(28)      VALUE SPACES.     05540002
054400         03  FILLER              PIC X(43)      VALUE SPACES.     05550002
054500     02  LOCALS-IN-TOWN-1.                                        05560002
054600         03  FILLER              PIC X(1)       VALUE SPACES.     05570027
054700         03  LIT-ASGN-DESC       PIC X(7)       VALUE             05580002
054800             'LOCAL: '.                                           05590002
054900         03  LOCAL-IN-TOWN       PIC X(6).                        05600002
055000         03  FILLER              PIC X          VALUE SPACE.      05610002
055100         03  LOCAL-IN-TOWN-DESC  PIC X(26).                       05620002
055200         03  FILLER              PIC X(01)      VALUE SPACES.     05630027
055300*CNC0516-BEG                                                      05640049
055400         03  FILLER              PIC X(14)      VALUE             05650039
055500             'RESTED/STATUS'.                                     05660002
055600         03  FILLER              PIC X(19)      VALUE SPACES.     05670039
055700*CNC0516-END                                                      05680049
055800     02  LOCALS-IN-TOWN-2.                                        05690002
055900         03  LOCALS-IT-FILLER    PIC X(02)      VALUE SPACES.     05700027
056000         03  LOCALS-IT-CRAFT     PIC X(10).                       05710002
056100         03  FILLER              PIC X(2)       VALUE SPACES.     05720002
056200*CNC0516-BEG                                                      05730049
056300         03  LOCALS-IT-NAME      PIC X(25).                       05740039
056400*CNC0516-END                                                      05750049
056500*        03  FILLER              PIC X(8)       VALUE SPACES.     05760009
056600         03  FILLER              PIC X(1)       VALUE SPACES.     05770009
056700*CNC0516-BEG                                                      05780049
056800         03  LOCALS-IT-RESTED    PIC X(14).                       05790039
056900*CNC0516-END                                                      05800049
057000         03  FILLER              PIC X(1)       VALUE SPACES.     05810010
057100         03  LOCALS-IT-HOS-AREA  PIC X(21)      VALUE SPACES.     05820027
057200*        03  FILLER              PIC X(01)      VALUE SPACES.     05830027
057300*                                                                 05840002
057400 01  CREWS-OUT-TOWN-AREA.                                         05850002
057500     02  CREWS-OUT-TOWN-TITLE.                                    05860002
057600*        03  FILLER              PIC X          VALUE SPACES.     05870027
057700         03  FILLER              PIC X(18)      VALUE             05880002
057800             'CREWS OUT OF TOWN '.                                05890002
057900         03  FILLER              PIC X(61)      VALUE SPACES.     05900002
058000     02  CREWS-OUT-TOWN-1.                                        05910002
058100         03  FILLER              PIC X(5)       VALUE SPACES.     05920002
058200         03  FILLER              PIC X(6)       VALUE             05930002
058300             'POOL: '.                                            05940002
058400         03  CREWS-OUT-TOWN-POOL PIC X(14).                       05950002
058500         03  FILLER              PIC X(34)      VALUE SPACES.     05960002
058600*CNC0516-BEG                                                      05970049
058700         03  FILLER              PIC X(14)      VALUE             05980039
058800             'RESTED/STATUS'.                                     05990002
058900         03  FILLER              PIC X(07)      VALUE SPACES.     06000039
059000*CNC0516-END                                                      06010049
059100     02  CREWS-OUT-TOWN-1A.                                       06020002
059200         03  FILLER              PIC X(07)      VALUE SPACES.     06030002
059300         03  CREWS-OUT-TOWN-TERM PIC X(20).                       06040002
059400         03  FILLER              PIC X(53)      VALUE SPACES.     06050002
059500     02  CREWS-OUT-TOWN-2.                                        06060002
059600         03  FILLER              PIC X(09)      VALUE SPACES.     06070002
059700         03  CREWS-OT-POSITION   PIC X(2).                        06080002
059800         03  FILLER              PIC X(2)       VALUE SPACES.     06090002
059900         03  CREWS-OT-CRAFT      PIC X(10).                       06100002
060000         03  FILLER              PIC X(2)       VALUE SPACES.     06110002
060100         03  CREWS-OT-TURN       PIC X(4).                        06120002
060200         03  FILLER              PIC X(2)       VALUE SPACES.     06130002
060300         03  CREWS-OT-NAME       PIC X(26).                       06140049
060400         03  FILLER              PIC X(2)       VALUE SPACES.     06150002
060500*CNC0516-BEG                                                      06160049
060600         03  CREWS-OT-RESTED     PIC X(14).                       06170039
060700         03  FILLER              PIC X(07)      VALUE SPACES.     06180049
060800*CNC0516-END                                                      06190049
060900*                                                                 06200002
061000 01  EXTRABOARDS-AREA.                                            06210002
061100     02  EXTRABOARDS-TITLE.                                       06220002
061200         03  FILLER              PIC X          VALUE SPACES.     06230002
061300         03  FILLER              PIC X(14)      VALUE             06240002
061400             'EXTRA BOARDS  '.                                    06250002
061500         03  FILLER              PIC X(65)      VALUE SPACES.     06260002
061600     02  EXTRABOARDS-1.                                           06270002
061700         03  FILLER              PIC X(2)       VALUE SPACES.     06280027
061800         03  EXTRABOARD-DESC     PIC X(23)      VALUE SPACES.     06290027
061900         03  FILLER              PIC X(11)      VALUE SPACES.     06300027
062000*CNC0516-BEG                                                      06310049
062100         03  FILLER              PIC X(14)      VALUE             06320039
062200             'RESTED/STATUS'.                                     06330002
062300         03  FILLER              PIC X(21)      VALUE SPACES.     06340039
062400*CNC0516-END                                                      06350049
062500     02  EXTRABOARDS-2.                                           06360002
062600         03  FILLER              PIC X(03)      VALUE SPACES.     06370027
062700         03  EXTRABOARD-POSITION PIC X(03)      VALUE SPACES.     06380027
062800         03  FILLER              PIC X(02)      VALUE SPACES.     06390027
062900         03  EXTRABOARD-NAME     PIC X(26)      VALUE SPACES.     06400002
063000*CNC0516-BEG                                                      06410049
063100         03  FILLER              PIC X(01)      VALUE SPACES.     06420039
063200         03  EXTRABOARD-RESTED   PIC X(14)      VALUE SPACES.     06430039
063300*CNC0516-END                                                      06440049
063400*        03  FILLER              PIC X(08)      VALUE SPACES.     06450009
063500         03  FILLER              PIC X(01)      VALUE SPACES.     06460009
063600         03  EXTRABOARD-HOS-AREA PIC X(21)      VALUE SPACES.     06470027
063700         03  FILLER              PIC X(01)      VALUE SPACES.     06480027
063800*CNC0516-BEG                                                      06490034
063900 01 WS-EXTRABOARD-RESTED.                                         06500034
063910*CNC0573 - BEG                                                    06510060
064000*    05 WS-LAYOFF-CODE           PIC X(02)      VALUE SPACES.     06520060
064100*    05 WS-LAYOFF-EM-CODE        PIC X(02)      VALUE SPACES.     06530060
064200*    05 FILLER                   PIC X          VALUE SPACE.      06540060
064210     05 WS-LAYOFF-AREA           PIC X(05)      VALUE SPACES.     06550062
064230     05 FILLER REDEFINES WS-LAYOFF-AREA.                          06560060
064240        10 WS-LAYOFF-CODE        PIC X(02).                       06570062
064250        10 WS-LAYOFF-EM-CODE     PIC X(02).                       06580062
064260        10 FILLER                PIC X.                           06590062
064270*CNC0573 - END                                                    06600060
064300     05 WS-RETURN-DATE           PIC X(04)      VALUE SPACES.     06610052
064400     05 FILLER                   PIC X          VALUE SPACE.      06620052
064500     05 WS-RETURN-TIME           PIC X(04)      VALUE SPACES.     06630052
064600 01 WS-CREWS-IT-RESTED.                                           06640034
064610*CNC0573 - BEG                                                    06650060
064700*    05 WS-LAYOFF-CODE1          PIC X(02)      VALUE SPACES.     06660060
064800*    05 WS-LAYOFF-EM-CODE1       PIC X(02)      VALUE SPACES.     06670060
064900*    05 FILLER                   PIC X          VALUE SPACE.      06680060
064901     05 WS-LAYOFF-AREA1          PIC X(05)      VALUE SPACES.     06690062
064903     05 FILLER REDEFINES WS-LAYOFF-AREA1.                         06700060
064904        10 WS-LAYOFF-CODE1       PIC X(02).                       06710062
064905        10 WS-LAYOFF-EM-CODE1    PIC X(02).                       06720062
064906        10 FILLER                PIC X.                           06730062
064910*CNC0573 - END                                                    06740060
065000     05 WS-RETURN-DATE1          PIC X(04)      VALUE SPACES.     06750052
065100     05 FILLER                   PIC X          VALUE SPACE.      06760052
065200     05 WS-RETURN-TIME1          PIC X(04)      VALUE SPACES.     06770052
065300 01 WS-RESTED1.                                                   06780034
065310*CNC0573 - BEG                                                    06790060
065400*    05 WS-LAYOFF-CODE2          PIC X(02)      VALUE SPACES.     06800060
065500*    05 WS-LAYOFF-EM-CODE2       PIC X(02)      VALUE SPACES.     06810060
065600*    05 FILLER                   PIC X          VALUE SPACE.      06820060
065601     05 WS-RESTED-AREA           PIC X(05)      VALUE SPACES.     06830062
065603     05 FILLER REDEFINES WS-RESTED-AREA.                          06840060
065604        10 WS-LAYOFF-CODE2       PIC X(02).                       06850062
065605        10 WS-LAYOFF-EM-CODE2    PIC X(02).                       06860062
065606        10 FILLER                PIC X.                           06870062
065610*CNC0573 - END                                                    06880060
065700     05 WS-RETURN-DATE2          PIC X(04)      VALUE SPACES.     06890052
065800     05 FILLER                   PIC X          VALUE SPACE.      06900052
065900     05 WS-RETURN-TIME2          PIC X(04)      VALUE SPACES.     06910052
066000 01 WS-E95.                                                       06920055
066010*CNC0573 - BEG                                                    06930060
066100*    05 WS-E95-CODE              PIC X(02)      VALUE SPACES.     06940060
066200*    05 WS-E95-EM-CODE           PIC X(02)      VALUE SPACES.     06950060
066300*    05 FILLER                   PIC X          VALUE SPACE.      06960060
066301     05 WS-LO-E95-AREA           PIC X(05)      VALUE SPACES.     06970062
066303     05 FILLER REDEFINES WS-LO-E95-AREA.                          06980060
066304        10 WS-E95-CODE           PIC X(02).                       06990062
066305        10 WS-E95-EM-CODE        PIC X(02).                       07000062
066306        10 FILLER                PIC X.                           07010062
066310*CNC0573 - END                                                    07020060
066400     05 WS-E95-DATE              PIC X(04)      VALUE SPACES.     07030056
066500     05 FILLER                   PIC X          VALUE SPACE.      07040056
066600     05 WS-E95-TIME              PIC X(04)      VALUE SPACES.     07050056
066700*CNC0516-END                                                      07060034
066800                                                                  07070002
066900 01  OB-DONE-CODE                PIC X VALUE '0'.                 07080002
067000     88  OB-NOT-DONE                   VALUE '0'.                 07090002
067100     88  OB-DONE                       VALUE '1'.                 07100002
067200*                                                                 07110002
067300 01  LAYOFF-AREA.                                                 07120002
067400     02  LAYOFF-TITLE.                                            07130002
067500         03  FILLER              PIC X          VALUE SPACES.     07140002
067600         03  LAYOFF-TITLE-FIELD  PIC X(79)      VALUE SPACES.     07150002
067700     02  LAYOFF-1.                                                07160002
067800         03  FILLER              PIC X(07)      VALUE SPACES.     07170014
067900         03  LAYOFF-NAME         PIC X(26)      VALUE SPACES.     07180002
068000         03  FILLER              PIC X(02)      VALUE SPACES.     07190002
068100         03  LAYOFF-STATUS       PIC X(16)      VALUE SPACES.     07200031
068200*        03  FILLER              PIC X(29)      VALUE SPACES.     07210009
068300         03  FILLER              PIC X(01)      VALUE SPACES.     07220031
068400         03  LAYOFF-HOS-AREA     PIC X(21)      VALUE SPACES.     07230027
068500*        03  FILLER              PIC X(02)      VALUE SPACES.     07240027
068600                                                                  07250002
068700 01  FORMAT-TIME.                                                 07260002
068800     02  F-YR                    PIC 99.                          07270002
068900     02  FILLER                  PIC X VALUE '/'.                 07280002
069000     02  F-MO                    PIC 99.                          07290002
069100     02  FILLER                  PIC X VALUE '/'.                 07300002
069200     02  F-DY                    PIC 99.                          07310002
069300     02  FILLER                  PIC X VALUE '-'.                 07320002
069400     02  F-HR-MN                 PIC 9999.                        07330002
069500                                                                  07340002
069600 01  CHECK-REST-TIME-CENT.                                        07350002
069700     02  CHECK-REST-CE           PIC 99 VALUE ZERO.               07360002
069800     02  CHECK-REST-DATE.                                         07370002
069900      03  CK-YR                  PIC 99 VALUE ZERO.               07380002
070000      03  CK-MO                  PIC 99 VALUE ZERO.               07390002
070100      03  CK-DY                  PIC 99 VALUE ZERO.               07400002
070200     02  CK-HR-MN                PIC 9999 VALUE ZERO.             07410002
070300                                                                  07420002
070400*CNC0516-BEG                                                      07430049
070500 01  WS-RESTED                   PIC X(14) VALUE SPACES.          07440048
070600*CNC0516-BEG                                                      07450049
070700 01  CREWS-IT-POSITION           PIC X(02) VALUE SPACES.          07460027
070800*CNC0516-BEG                                                      07470034
070900 01  WS-RETURN-DATE-1            PIC X(08) VALUE SPACES.          07480052
071000 01  FILLER REDEFINES WS-RETURN-DATE-1.                           07490034
071100     05  WS-RETURN-DATE-MM       PIC 99.                          07500039
071200     05  WS-RETURN-DATE-DD       PIC 99.                          07510039
071300     05  WS-RETURN-DATE-HRMN     PIC 9(04).                       07520051
071400 01  WS-EFFECTIVE-DATE-TIME.                                      07530053
071500     05  WS-EFF-DATE.                                             07540053
071600         10  WS-EFF-YR           PIC 99.                          07550053
071700         10  WS-EFF-MO           PIC 99.                          07560053
071800         10  WS-EFF-DY           PIC 99.                          07570053
071900     05  WS-EFF-HR-MN.                                            07580053
072000         10  WS-EFF-HR           PIC 99.                          07590053
072100         10  WS-EFF-MN           PIC 99.                          07600053
072200*CNC0516-END                                                      07610034
072300                                                                  07620002
072400 01  WS-CONVERT-TIME.                                             07630002
072500     02  WS-CONV-HRMN.                                            07640002
072600         03  WS-CONV-HR          PIC 99.                          07650002
072700             88  TIME-IS-AM         VALUES 00 THRU 11.            07660002
072800             88  TIME-IS-PM         VALUES 12 THRU 23.            07670002
072900         03  WS-CONV-MN          PIC 99.                          07680002
073000     02  WS-CONV-AMPM            PIC X.                           07690002
073100                                                                  07700002
073200 01  WORK-CNTLKEY.                                                07710002
073300     02  WK-CNTL-REC-TYPE        PIC XX VALUE SPACE.              07720002
073400     02  WK-CNTL-DIST            PIC XX VALUE SPACE.              07730002
073500     02  WK-CNTL-SUB-DIST        PIC XX VALUE SPACE.              07740002
073600     02  WK-CNTL-POOL            PIC XX VALUE SPACE.              07750002
073700     02  WK-CNTL-XB REDEFINES WK-CNTL-POOL PIC XX.                07760002
073800     02  WK-CNTL-CC REDEFINES WK-CNTL-XB   PIC XX.                07770002
073900     02  WK-CNTL-POOL-TYPE       PIC X VALUE SPACE.               07780002
074000     02  FILLER                  PIC X(11) VALUE SPACE.           07790002
074100 01  NORMAL-ASGNMT-FLAG          PIC X VALUE SPACE.               07800002
074200     88  NORM-ASGN-UFP                 VALUE 'U'.                 07810002
074300     88  NORM-ASGN-XB                  VALUE 'X'.                 07820002
074400     88  NORM-ASGN-AJ                  VALUE 'A'.                 07830002
074500 01  NORMAL-ASGNMT.                                               07840002
074600     02  NA-DIST                 PIC XX VALUE SPACE.              07850002
074700     02  NA-SUB-DIST             PIC XX VALUE SPACE.              07860002
074800     02  NA-AREA.                                                 07870002
074900       03  NA-1                  PIC X(6) VALUE SPACE.            07880002
075000       03  NA-2 REDEFINES NA-1.                                   07890002
075100         04  NA-POOL             PIC XX.                          07900002
075200         04  NA-TURN             PIC X(4).                        07910002
075300       03  NA-3 REDEFINES NA-1.                                   07920002
075400         04  NA-FILLER           PIC XX.                          07930002
075500         04  NA-XB-TURN          PIC X(4).                        07940002
075600       03  NA-CC                 PIC XX VALUE SPACE.              07950002
075700 01  TEMPORARY-ASGNMT-FLAG       PIC X VALUE SPACE.               07960002
075800     88  TEMP-UFP                      VALUE 'U'.                 07970002
075900     88  TEMP-ASGN-XB                  VALUE 'X'.                 07980002
076000     88  TEMP-AJ                       VALUE 'A'.                 07990002
076100 01  TEMPORARY-ASGNMT.                                            08000002
076200     02  TA-DIST                 PIC XX VALUE SPACE.              08010002
076300     02  TA-SUB-DIST             PIC XX VALUE SPACE.              08020002
076400     02  TA-AREA.                                                 08030002
076500       03  TA-1                  PIC X(6) VALUE SPACE.            08040002
076600       03  TA-2 REDEFINES TA-1.                                   08050002
076700         04  TA-POOL             PIC XX.                          08060002
076800         04  TA-TURN             PIC X(4).                        08070002
076900       03  TA-3 REDEFINES TA-1.                                   08080002
077000         04  TA-FILLER           PIC XX.                          08090002
077100         04  TA-XB-TURN          PIC X(4).                        08100002
077200       03  TA-CC                 PIC XX VALUE SPACE.              08110002
077300 01  TEMP-ASGNMT-DATE-TIME       PIC X(10).                       08120002
077400 01  TEMP-ASGN-XB-AUG-FLAG       PIC X VALUE SPACE.               08130002
077500     88  TEMP-ASGN-XB-AUG              VALUE 'Y'.                 08140002
077600 01  ON-DUTY-ASGNMT-FLAG         PIC X VALUE SPACE.               08150002
077700     88  ON-DUTY-UFP                   VALUE 'U'.                 08160002
077800     88  ON-DUTY-AJ                    VALUE 'A'.                 08170002
077900 01  ON-DUTY-OUT-TOWN-CODE       PIC 9(10) VALUE 9999999999.      08180002
078000     88  OUT-TOWN                          VALUE ZERO.            08190002
078100 01  ON-DUTY-ASGNMT.                                              08200002
078200     02  OD-DIST                 PIC XX VALUE SPACE.              08210002
078300     02  OD-SUB-DIST             PIC XX VALUE SPACE.              08220002
078400     02  OD-AREA.                                                 08230002
078500       03  OD-1                  PIC X(6) VALUE SPACE.            08240002
078600       03  OD-2 REDEFINES OD-1.                                   08250002
078700         04  OD-POOL             PIC XX.                          08260002
078800         04  OD-TURN             PIC X(4).                        08270002
078900       03  OD-CC                 PIC XX VALUE SPACE.              08280002
079000 01  OWNER-EMP-NBR               PIC 9(9) VALUE ZERO.             08290002
079100 01  TEMP-EMP-ONE                PIC 9(9) VALUE ZERO.             08300002
079200 01  ON-DUTY-EMP                 PIC 9(9) VALUE ZERO.             08310002
079300                                                                  08320002
079400 01  WS-HOLD-CNTL-FILE           PIC X(256) VALUE SPACES.         08330002
079500                                                                  08340002
079600 01  WS-SAVE-LINK-DIST           PIC XX   VALUE SPACES.           08350002
079700 01  WS-SAVE-LINK-SUB-DIST       PIC XX   VALUE SPACES.           08360002
079800                                                                  08370002
079900******************************************************************08380002
080000*                     COMMAREA COPYBOOKS                          08390002
080100******************************************************************08400002
080200     COPY P903COMM.                                               08410002
080300     COPY P907COMM.                                               08420002
080400     COPY P942COMM.                                               08430002
080500*    COPY P942TSQ.                                                08440002
080600     COPY P956COMM.                                               08450002
080700     COPY PS42COMM.                                               08460002
080800     COPY PS94COMM.                                               08470009
080900******************************************************************08480002
081000*                       MISC  COPYBOOKS                           08490002
081100******************************************************************08500002
081200     COPY WSBATERR.                                               08510002
081300     COPY WSLOCODE.                                               08520002
081400     COPY WSERRMSG.                                               08530002
081500     COPY PSTBATCH.                                               08540002
081600     COPY PSTCCRFT.                                               08550002
081700     COPY WSCENTER.                                               08560002
081800     COPY WSZONE.                                                 08570002
081900     COPY WSOFFSET.                                               08580002
082000     COPY WSAJDEFN.                                               08590002
082100     COPY WSDAYWK.                                                08600002
082200******************************************************************08610002
082300*                       FILE COPYBOOKS                            08620002
082400******************************************************************08630002
082500     COPY WSMSTR.                                                 08640002
082600     COPY WSSEN.                                                  08650002
082700     COPY WSUFP.                                                  08660002
082800     COPY WSEB.                                                   08670002
082900     COPY WSAJ.                                                   08680002
083000     COPY WSJS.                                                   08690002
083100     COPY WSASGN.                                                 08700002
083200     COPY WSCNTL.                                                 08710002
083300     COPY WSTRCN.                                                 08720002
083400     COPY WSANAL.                                                 08730002
083500     COPY WSSWASGN.                                               08740002
083600     COPY WSSYDTTM.                                               08750002
083700     COPY WSEDDATE.                                               08760002
083800*CNC0516-BEG                                                      08770036
083900     COPY WSTASK.                                                 08780036
084000     COPY WSMSTR2.                                                08790056
084100*CNC0516-END                                                      08800036
084200******************************************************************08810002
084300*                    I/O STATUS CHECK COPYBOOK                    08820002
084400******************************************************************08830002
084500 01  FILE-STATUS                      PIC XX  VALUE SPACE.        08840002
084600     COPY BIOCODES.                                               08850002
084700                                                                  08860002
084800 LINKAGE SECTION.                                                 08870002
084900                                                                  08880002
085000 01  LINK-PARAMETERS.                                             08890002
085100     02  LINK-PARM-LEN                PIC S9(4) COMP.             08900002
085200     02  LINK-DIST                    PIC XX.                     08910002
085300     02  LINK-SUB-DIST                PIC XX.                     08920002
085400     02  LINK-USERID                  PIC X(8).                   08930002
085410*CNC0573 - BEG                                                    08940066
085411     02  LINK-MASK-FLD-SCR-FL         PIC X.                      08950066
085420*CNC0573 - END                                                    08960066
085500                                                                  08970002
085600                                                                  08980002
085700 PROCEDURE DIVISION USING LINK-PARAMETERS.                        08990002
085800                                                                  09000002
085900 P0001-BEGIN-PROGRAM.                                             09010002
086000                                                                  09020002
086100     MOVE SPACES       TO WS-ERROR-INFO                           09030002
086200     MOVE P779-PGM     TO ERR-PROG                                09040002
086300                                                                  09050002
086400     PERFORM P9500-OPEN-FILES                                     09060002
086500     COPY Y2KDATE.                                                09070002
086600*-----------------------------------------------------------------09080002
086700*    SEE IF ANY APPLICATION DATE/TIME OVERRIDE                    09090002
086800*-----------------------------------------------------------------09100002
086900     MOVE LINK-USERID  TO WS-DATE-TIME-USERID                     09110002
086910*CNC0573 - BEG                                                    09120066
086911     MOVE LINK-MASK-FLD-SCR-FL TO WS-MASK-FLD-SCR-FL              09130066
086920*CNC0573 - END                                                    09140066
087000                                                                  09150002
087100     MOVE SPACES TO PL-R                                          09160002
087200     WRITE PL-R AFTER ADVANCING PAGE                              09170002
087300                                                                  09180002
087400     PERFORM P9800-GET-TIME-OFFSET                                09190002
087500                                                                  09200002
087600     PERFORM P0025-GET-TITLES                                     09210002
087700                                                                  09220002
087800     PERFORM P0050-GET-REPORTS                                    09230002
087900                                                                  09240002
088000     PERFORM P9501-CLOSE-FILES                                    09250002
088100                                                                  09260002
088200     STOP RUN.                                                    09270002
088300                                                                  09280002
088400 P0025-GET-TITLES.                                                09290002
088500                                                                  09300002
088600     MOVE 'P0025'           TO ERR-PARAGRAPH                      09310002
088700                                                                  09320002
088800     MOVE LINK-DIST         TO POS-DIST                           09330002
088900                               XB-POS-DIST                        09340002
089000                               XB-TURN-DIST                       09350002
089100                               WK-CNTL-DIST                       09360002
089200     MOVE LINK-SUB-DIST     TO POS-SUB-DIST                       09370002
089300                               XB-POS-SUB-DIST                    09380002
089400                               XB-TURN-SUB-DIST                   09390002
089500                               WK-CNTL-SUB-DIST                   09400002
089600                                                                  09410002
089700     MOVE SPACES TO WS-CNTL-FILE                                  09420002
089800     SET COMPANY-TYPE-REC TO TRUE                                 09430002
089900     MOVE CNTLKEY-AREA TO CNTL-FS-KEY                             09440002
090000     PERFORM P8000-READ-CNTLFILE                                  09450002
090100     IF COMPANY-TYPE-REC AND CNTL-COMPANY-NAME > SPACES           09460002
090200        MOVE 26                TO CTXT-UNF-FIELD-LEN              09470002
090300        MOVE CNTL-COMPANY-NAME TO CTXT-UNF-FIELD                  09480002
090400        PERFORM P8994-CENTER-TEXT                                 09490002
090500        MOVE CTXT-FOR-FIELD    TO TITLE-CUST-NAME                 09500002
090600     ELSE                                                         09510002
090700        MOVE '<< NONE ON FILE >>' TO TITLE-CUST-NAME              09520002
090800     END-IF                                                       09530002
090900                                                                  09540002
091000     PERFORM P9820-GET-CURRENT-TIME                               09550002
091100     MOVE ZEROS                TO DATE-CONVERSION-PARMS           09560002
091200     SET PARM-CONV             TO TRUE                            09570002
091300     MOVE WS-SYSTEM-DATE       TO PARM-PRI-DATE-GREG              09580002
091400     CALL P803-PGM             USING DATE-CONVERSION-PARMS        09590002
091500     MOVE PARM-PRI-DAY-OF-WEEK TO DAY1                            09600002
091600                                  WEEK-DAY                        09610002
091700     MOVE WS-LOCAL-YR          TO TIT-YR  CUR-YR                  09620059
091800     MOVE WS-LOCAL-MO          TO TIT-MO  CUR-MO                  09630059
091900     MOVE WS-LOCAL-DY          TO TIT-DY  CUR-DY                  09640059
092000     MOVE WS-LOCAL-HR          TO TIT-HR                          09650059
092100     MOVE WS-LOCAL-MN          TO TIT-MN                          09660059
092200                                                                  09670002
092300     MOVE SPACES               TO POOL-TABLE                      09680059
092400     MOVE SPACES               TO SPAREBOARD-TABLE                09690059
092500     PERFORM P0060-LOAD-TABLES.                                   09700002
092600                                                                  09710002
092700 P0050-GET-REPORTS.                                               09720002
092800                                                                  09730002
092900     MOVE SPACES                 TO CNTLKEY                       09740025
093000                                    CNTLKEY-AREA                  09750025
093100                                    WS-CNTL-FILE                  09760026
093200     MOVE LINK-DIST              TO CNTL-DIST                     09770026
093300     SET DIST-TYPE-REC           TO TRUE                          09780025
093400     MOVE CNTLKEY-AREA           TO CNTL-FS-KEY                   09790025
093500     PERFORM P8000-READ-CNTLFILE                                  09800025
093600     IF CNTL-BCR-COMPANY                                          09810025
093700        OR CNTL-CN-COMPANY                                        09820025
093800        OR CNTL-ACR-COMPANY                                       09830025
093900        SET WS-CANADIAN-COMPANY  TO TRUE                          09840025
094000     ELSE                                                         09850025
094100        SET WS-US-COMPANY        TO TRUE                          09860025
094200     END-IF                                                       09870025
094300     MOVE SPACES                 TO DIST-SDIST-POOL-FLAG          09880026
094400                                                                  09890002
094500     PERFORM P1000-TRAINS-ENROUTE                                 09900002
094600                                                                  09910002
094700     IF LINK-DIST = WS-DIST-LM AND                                09920002
094800        LINK-SUB-DIST = WS-SUB-DIST-JX                            09930002
094900        SET DIST-SDIST-POOL-LM-MC-JN TO TRUE                      09940002
095000        MOVE WS-SUB-DIST-MC  TO LINK-SUB-DIST                     09950002
095100        PERFORM P1000-TRAINS-ENROUTE                              09960002
095200        MOVE WS-SUB-DIST-JX  TO LINK-SUB-DIST                     09970002
095300        MOVE SPACES          TO DIST-SDIST-POOL-FLAG              09980002
095400     END-IF                                                       09990002
095500                                                                  10000002
095600     PERFORM P2000-POOLS                                          10010002
095700                                                                  10020002
095800     IF LINK-DIST = WS-DIST-LM AND                                10030002
095900        LINK-SUB-DIST = WS-SUB-DIST-MC AND                        10040002
096000        DIST-SDIST-POOL-LM-MC-JN                                  10050002
096100        MOVE WS-SUB-DIST-JX  TO LINK-SUB-DIST                     10060002
096200        MOVE SPACES          TO DIST-SDIST-POOL-FLAG              10070002
096300     END-IF                                                       10080002
096400                                                                  10090002
096500     PERFORM P3000-SPAREBOARDS                                    10100002
096600                                                                  10110002
096700     PERFORM P5000-OFF-BOARDS                                     10120002
096800                                                                  10130002
096900     IF LINE-COUNT > 55                                           10140046
097000        PERFORM P9000-TITLE                                       10150002
097100     END-IF                                                       10160002
097200     MOVE SPACES          TO PL-R                                 10170002
097300     MOVE '                              << END OF REPORT >>'     10180025
097400                          TO PL-R                                 10190002
097500     WRITE PL-R AFTER ADVANCING 2 LINES                           10200002
097600     MOVE SPACES TO PL-R                                          10210002
097700     WRITE PL-R BEFORE ADVANCING PAGE.                            10220002
097800                                                                  10230002
097900 P0060-LOAD-TABLES.                                               10240002
098000                                                                  10250002
098100     SET NOT-DONE         TO TRUE                                 10260002
098200     MOVE SPACES          TO WS-CNTL-FILE                         10270002
098300     MOVE LINK-DIST       TO WK-CNTL-DIST                         10280002
098400     MOVE LINK-SUB-DIST   TO WK-CNTL-SUB-DIST                     10290002
098500     MOVE '03'            TO WK-CNTL-REC-TYPE                     10300002
098600     MOVE WORK-CNTLKEY    TO CNTL-FS-KEY                          10310002
098700     PERFORM P8000-START-CNTLFILE                                 10320002
098800     IF SUCCESS                                                   10330002
098900       PERFORM VARYING POOL-SUB FROM 1 BY 1                       10340002
099000           UNTIL DONE OR POOL-SUB > RPT-MAX                       10350002
099100        PERFORM P8000-READ-NEXT-CNTLFILE                          10360002
099200        IF SUCCESS                                                10370002
099300           IF CNTL-DIST = LINK-DIST AND                           10380002
099400              CNTL-SUB-DIST = LINK-SUB-DIST AND                   10390002
099500              POOL-TYPE-REC AND                                   10400002
099600              CNTL-POOL-TYPE = 'F'                                10410002
099700               MOVE CNTL-POOL-CODE TO POOL-P(POOL-SUB)            10420002
099800           END-IF                                                 10430002
099900        ELSE                                                      10440002
100000           IF LINK-DIST = WS-DIST-LM AND                          10450002
100100              LINK-SUB-DIST = WS-SUB-DIST-JX                      10460002
100200              MOVE WS-POOL-JN      TO POOL-P(POOL-SUB)            10470002
100300           END-IF                                                 10480002
100400           SET DONE                TO TRUE                        10490002
100500        END-IF                                                    10500002
100600       END-PERFORM                                                10510002
100700     END-IF                                                       10520002
100800                                                                  10530002
100900     SET NOT-DONE         TO TRUE                                 10540002
101000     MOVE SPACES          TO WS-CNTL-FILE                         10550002
101100     MOVE LINK-DIST       TO WK-CNTL-DIST                         10560002
101200     MOVE LINK-SUB-DIST   TO WK-CNTL-SUB-DIST                     10570002
101300     MOVE '08'            TO WK-CNTL-REC-TYPE                     10580002
101400     MOVE WORK-CNTLKEY    TO CNTL-FS-KEY                          10590002
101500     PERFORM P8000-START-CNTLFILE                                 10600002
101600     IF SUCCESS                                                   10610002
101700       PERFORM VARYING EXT-SUB FROM 1 BY 1                        10620002
101800           UNTIL DONE OR EXT-SUB > RPT-MAX                        10630002
101900        PERFORM P8000-READ-NEXT-CNTLFILE                          10640002
102000        IF SUCCESS                                                10650002
102100           IF CNTL-DIST = LINK-DIST AND                           10660002
102200              CNTL-SUB-DIST = LINK-SUB-DIST AND                   10670002
102300              EXTRABOARD-TYPE-REC                                 10680002
102400               MOVE CNTL-XB        TO SB-P(EXT-SUB)               10690002
102500           END-IF                                                 10700002
102600        ELSE                                                      10710002
102700           SET DONE                TO TRUE                        10720002
102800        END-IF                                                    10730002
102900       END-PERFORM                                                10740002
103000     END-IF.                                                      10750002
103100                                                                  10760002
103200 P1000-TRAINS-ENROUTE.                                            10770002
103300                                                                  10780002
103400     MOVE 'P1000'       TO ERR-PARAGRAPH                          10790002
103500     MOVE 0             TO LINE-COUNT                             10800002
103600     MOVE SPACES        TO TRAIN-ENROUTE                          10810002
103700     PERFORM P1010-GET-TRAINS-ENROUTE                             10820002
103800     PERFORM P1500-GET-LOCALS-ENROUTE.                            10830002
103900                                                                  10840002
104000 P1010-GET-TRAINS-ENROUTE.                                        10850002
104100                                                                  10860002
104200     MOVE 'P1010'              TO ERR-PARAGRAPH                   10870002
104300     MOVE SPACES               TO UFPTRAIN-AREA                   10880002
104400     MOVE LINK-DIST            TO DIST3 OF WS-UFP                 10890002
104500     MOVE LINK-SUB-DIST        TO SUB-DIST3 OF WS-UFP             10900002
104600     MOVE UFPTRAIN-AREA        TO UFPTRAIN-FS-KEY UFPTRAIN        10910002
104700     START UFP-FILE KEY > UFPTRAIN-FS-KEY                         10920002
104800           INVALID KEY CONTINUE                                   10930002
104900     END-START                                                    10940002
105000     IF SUCCESS                                                   10950002
105100        SET TRAINS-NOT-DONE TO TRUE                               10960002
105200        MOVE ZEROES         TO TRAIN-COUNT                        10970002
105300        PERFORM UNTIL TRAINS-DONE                                 10980002
105400          READ UFP-FILE NEXT RECORD INTO WS-UFP                   10990002
105500               AT END CONTINUE                                    11000002
105600          END-READ                                                11010002
105700          IF DIST3     OF WS-UFP = LINK-DIST     AND              11020002
105800             SUB-DIST3 OF WS-UFP = LINK-SUB-DIST AND              11030002
105900             POOL-NAME OF WS-UFP = WS-POOL-JN    AND              11040002
106000             DIST-SDIST-POOL-LM-MC-JN                             11050002
106100             ADD 1 TO LINE-COUNT                                  11060002
106200             PERFORM P1015-PROCESS-UFP-RECORD                     11070002
106300          ELSE                                                    11080002
106400             IF NOT DIST-SDIST-POOL-LM-MC-JN                      11090002
106500                PERFORM P1015-PROCESS-UFP-RECORD                  11100002
106600             ELSE                                                 11110002
106700                SET TRAINS-DONE TO TRUE                           11120002
106800             END-IF                                               11130002
106900          END-IF                                                  11140002
107000        END-PERFORM                                               11150002
107100     ELSE                                                         11160002
107200        SET TRAINS-DONE TO TRUE                                   11170002
107300        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     11180002
107400           MOVE UFPTRAIN-FS-KEY       TO ERR-KEY                  11190002
107500           MOVE FILE-STATUS           TO ERR-FSTAT                11200002
107600           MOVE 'UFPFILE'             TO ERR-FNAME                11210002
107700           MOVE 'CANNOT START UFPFILE' TO ERR-DESC                11220002
107800           GO TO P9999-GOT-PROBLEM                                11230002
107900        ELSE                                                      11240002
108000           IF NOT DIST-SDIST-POOL-LM-MC-JN                        11250002
108100              SET TRAINS-DONE TO TRUE                             11260002
108200              PERFORM P1020-SET-TRAIN-INFO THRU                   11270002
108300                      P1020-SET-TRAIN-INFO-EXIT                   11280002
108400           END-IF                                                 11290002
108500        END-IF                                                    11300002
108600     END-IF.                                                      11310002
108700                                                                  11320002
108800 P1015-PROCESS-UFP-RECORD.                                        11330002
108900                                                                  11340002
109000     IF SUCCESS                                                   11350002
109100        IF DIST3      OF WS-UFP = LINK-DIST  AND                  11360002
109200           SUB-DIST3  OF WS-UFP = LINK-SUB-DIST                   11370002
109300           IF TRAIN-SYMBOL > SPACES                               11380002
109400              ADD  1     TO TRAIN-COUNT                           11390002
109500              PERFORM P1020-SET-TRAIN-INFO THRU                   11400002
109600                      P1020-SET-TRAIN-INFO-EXIT                   11410002
109700           END-IF                                                 11420002
109800        ELSE                                                      11430002
109900           SET TRAINS-DONE TO TRUE                                11440002
110000           PERFORM P1020-SET-TRAIN-INFO THRU                      11450002
110100                   P1020-SET-TRAIN-INFO-EXIT                      11460002
110200        END-IF                                                    11470002
110300     ELSE                                                         11480002
110400        SET TRAINS-DONE TO TRUE                                   11490002
110500        PERFORM P1020-SET-TRAIN-INFO THRU                         11500002
110600                P1020-SET-TRAIN-INFO-EXIT                         11510002
110700     END-IF.                                                      11520002
110800                                                                  11530002
110900 P1020-SET-TRAIN-INFO.                                            11540002
111000                                                                  11550002
111100     IF TRAINS-DONE                                               11560002
111200        IF TRAIN-COUNT NOT > ZEROES                               11570002
111300           PERFORM P1050-TRAINS-ENROUTE-TITLE                     11580002
111400           MOVE SPACES              TO TRAINS-ENROUTE-1           11590002
111500           MOVE 'NO TRAINS ENROUTE' TO TRAIN-ENROUTE-FROM         11600002
111600           WRITE PL-R FROM TRAINS-ENROUTE-1 AFTER ADVANCING 1 LINE11610002
111700           ADD 1                    TO LINE-COUNT                 11620002
111800           GO TO P1020-SET-TRAIN-INFO-EXIT                        11630002
111900        ELSE                                                      11640002
112000           PERFORM P1300-WRITE-TRAIN-DETAIL                       11650002
112100           GO TO P1020-SET-TRAIN-INFO-EXIT                        11660002
112200        END-IF                                                    11670002
112300     END-IF                                                       11680002
112400                                                                  11690002
112500     IF TRAIN-SYMBOL = TRAIN-ENROUTE                              11700002
112600        PERFORM P1200-GET-TRAIN-DETAIL-INFO                       11710002
112700     ELSE                                                         11720002
112800        IF TRAIN-ENROUTE = SPACES                                 11730002
112900           IF NOT DIST-SDIST-POOL-LM-MC-JN                        11740002
113000              PERFORM P1050-TRAINS-ENROUTE-TITLE                  11750002
113100           END-IF                                                 11760002
113200           PERFORM P1100-WRITE-NEW-TRAIN-LINE                     11770002
113300           PERFORM P1200-GET-TRAIN-DETAIL-INFO                    11780002
113400        ELSE                                                      11790002
113500           PERFORM P1300-WRITE-TRAIN-DETAIL                       11800002
113600           PERFORM P1100-WRITE-NEW-TRAIN-LINE                     11810002
113700           PERFORM P1200-GET-TRAIN-DETAIL-INFO                    11820002
113800        END-IF                                                    11830002
113900     END-IF.                                                      11840002
114000                                                                  11850002
114100 P1020-SET-TRAIN-INFO-EXIT.                                       11860002
114200     EXIT.                                                        11870002
114300                                                                  11880002
114400 P1050-TRAINS-ENROUTE-TITLE.                                      11890002
114500                                                                  11900002
114600     PERFORM P9000-TITLE                                          11910002
114700     WRITE PL-R FROM TRAINS-ENROUTE-TITLE                         11920002
114800                         AFTER ADVANCING 2 LINES                  11930002
114900     ADD 2 TO LINE-COUNT.                                         11940002
115000                                                                  11950002
115100 P1100-WRITE-NEW-TRAIN-LINE.                                      11960002
115200                                                                  11970002
115300     MOVE SPACES           TO TRAINS-ENROUTE-1                    11980002
115400     MOVE '  ON DUTY AT: ' TO TRAIN-DUTY                          11990002
115500     IF LINE-COUNT > 55                                           12000046
115600       PERFORM P1050-TRAINS-ENROUTE-TITLE                         12010002
115700     END-IF                                                       12020002
115800     MOVE TRAIN-SYMBOL   TO TRAIN-ENROUTE                         12030002
115900     MOVE SPACE          TO TRAIN-ENROUTE-FROM                    12040002
116000     MOVE SPACE          TO WORK-CNTLKEY                          12050002
116100     MOVE '03'           TO WK-CNTL-REC-TYPE                      12060002
116200     MOVE DIST OF WS-UFP TO WK-CNTL-DIST                          12070002
116300     MOVE SUB-DIST OF WS-UFP  TO WK-CNTL-SUB-DIST                 12080002
116400     MOVE POOL-NAME OF WS-UFP TO WK-CNTL-POOL                     12090002
116500     MOVE 'F' TO WK-CNTL-POOL-TYPE                                12100002
116600     MOVE WORK-CNTLKEY TO CNTL-FS-KEY                             12110002
116700     PERFORM P8000-READ-CNTLFILE                                  12120002
116800     IF SUCCESS                                                   12130002
116900       IF IN-TOWN                                                 12140002
117000          MOVE CNTL-POOL-HOME           TO TRAIN-ENROUTE-FROM     12150002
117100       ELSE                                                       12160002
117200          MOVE CNTL-AWAY-TERM(IN-OUT-TERMINAL)                    12170002
117300                                        TO TRAIN-ENROUTE-FROM     12180002
117400       END-IF                                                     12190002
117500     ELSE                                                         12200002
117600       MOVE 'NO CNTL RECORD' TO TRAIN-ENROUTE-FROM                12210002
117700     END-IF                                                       12220002
117800     IF ON-DUTY-TIME > ZEROES                                     12230002
117900        MOVE ON-DUTY-TIME TO WORK-TIME                            12240002
118000        MOVE WK-YR TO TRAIN-YR                                    12250002
118100        MOVE WK-MO TO TRAIN-MO                                    12260002
118200        MOVE WK-DY TO TRAIN-DY                                    12270002
118300        MOVE WORK-HR-MN TO TRAIN-HR-MN                            12280002
118400        MOVE '/' TO TRAIN-FIL1 TRAIN-FIL2                         12290002
118500        MOVE ' - ' TO TRAIN-FIL3                                  12300002
118600     ELSE                                                         12310002
118700        MOVE SPACES TO TRAIN-HR-MN TRAIN-YR TRAIN-MO TRAIN-DY     12320002
118800             TRAIN-FIL1 TRAIN-FIL2 TRAIN-FIL3                     12330002
118900     END-IF                                                       12340002
119000     WRITE PL-R FROM TRAINS-ENROUTE-1 AFTER ADVANCING 2 LINE      12350002
119100     ADD 2 TO LINE-COUNT.                                         12360002
119200                                                                  12370002
119300 P1200-GET-TRAIN-DETAIL-INFO.                                     12380002
119400                                                                  12390002
119500     SET SEARCH-NOT-DONE TO TRUE                                  12400002
119600     PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CRAFT-TABLE-MAX   12410002
119700                    OR SEARCH-DONE                                12420002
119800       IF POOL-CRAFT-CODE = CT-CRAFT-CODE(J)                      12430002
119900          SET SEARCH-DONE           TO TRUE                       12440002
120000          MOVE TURN-NBR OF WS-UFP   TO SAVE-CREW-TURN(J)          12450002
120100          PERFORM P7500-GET-UFP-EMPS                              12460002
120200          IF ON-DUTY-EMP > ZERO                                   12470002
120300             MOVE ON-DUTY-EMP TO MSTRNBRK                         12480002
120400             PERFORM P8500-READ-MASTER                            12490002
120500             MOVE EMP-NAME OF WS-MSTR TO SAVE-CREW-NAME(J)        12500002
120600             MOVE EMP-NBR  OF WS-MSTR TO PS94-EMP-NBR             12510009
120700             PERFORM P4000-GET-HOS                                12520009
120800             MOVE WS-TOT-TM           TO SAVE-CREW-HOS-TOTAL(J)   12530010
120900             MOVE WS-LIMBO-TM         TO SAVE-CREW-HOS-LIMBO(J)   12540010
121000             MOVE WS-CONSEC-STARTS    TO SAVE-CREW-HOS-ST(J)      12550017
121100          ELSE                                                    12560010
121200             MOVE '<< VACANT TURN >>' TO SAVE-CREW-NAME(J)        12570010
121300          END-IF                                                  12580002
121400          IF CT-CRAFT-CODE(J) = 'B1'                              12590002
121500             MOVE 'REAR BRKM:'         TO SAVE-CREW-CRAFT(J)      12600002
121600          ELSE                                                    12610002
121700             IF CT-CRAFT-CODE(J) = 'B2'                           12620002
121800                MOVE 'HEAD BRKM:'         TO SAVE-CREW-CRAFT(J)   12630002
121900             ELSE                                                 12640002
122000                MOVE CT-CRAFT-DESC(J)     TO SAVE-CREW-CRAFT(J)   12650002
122100             END-IF                                               12660002
122200          END-IF                                                  12670002
122300       END-IF                                                     12680002
122400     END-PERFORM.                                                 12690002
122500                                                                  12700002
122600 P1300-WRITE-TRAIN-DETAIL.                                        12710002
122700                                                                  12720002
122800     PERFORM VARYING J FROM 1 BY 1                                12730002
122900         UNTIL J > 30                                             12740002
123000            MOVE SPACES                   TO TRAINS-ENROUTE-2     12750002
123100            IF SAVE-CREW-CRAFT(J) > SPACES                        12760002
123200               MOVE SAVE-CREW-CRAFT(J)    TO TRAIN-CC             12770002
123300               MOVE SAVE-CREW-NAME(J)     TO TRAIN-EMP            12780002
123400               MOVE SAVE-CREW-TURN(J)     TO TRAIN-TURN           12790002
123500               IF SAVE-CREW-NAME(J) = '<< VACANT TURN >>' OR      12800017
123600                                      '<< VACANT JOB  >>'         12810018
123700                  MOVE SPACES             TO TRAIN-HOS-AREA       12820017
123800               ELSE                                               12830017
123900                  IF WS-CANADIAN-COMPANY                          12840025
124000                     MOVE SPACES          TO TRAIN-HOS-AREA       12850025
124100                  ELSE                                            12860025
124200                     STRING 'T '                                  12870027
124300                       SAVE-CREW-HOS-TOTAL-HR(J) ':'              12880017
124400                       SAVE-CREW-HOS-TOTAL-MM(J)                  12890017
124500                         ' L '                                    12900027
124600                       SAVE-CREW-HOS-LIMBO-HR(J) ':'              12910017
124700                       SAVE-CREW-HOS-LIMBO-MM(J)                  12920017
124800                         ' S:'                                    12930027
124900                       SAVE-CREW-HOS-ST(J)                        12940017
125000                       DELIMITED BY SIZE INTO TRAIN-HOS-AREA      12950017
125100                  END-IF                                          12960025
125200               END-IF                                             12970017
125300               WRITE PL-R FROM TRAINS-ENROUTE-2 AFTER             12980002
125400                                          ADVANCING 1 LINES       12990002
125500               ADD    1                   TO LINE-COUNT           13000002
125600            END-IF                                                13010002
125700     END-PERFORM                                                  13020002
125800                                                                  13030002
125900     MOVE SPACES              TO SAVE-CREW-INFORMATION            13040002
126000                                 SAVE-TEMP-CREW-INFORMATION.      13050002
126100                                                                  13060002
126200 P1500-GET-LOCALS-ENROUTE.                                        13070002
126300                                                                  13080002
126400     MOVE 'P1500'              TO ERR-PARAGRAPH                   13090002
126500     MOVE SPACES               TO AJTRAINKEY-AREA                 13100002
126600     MOVE LINK-DIST            TO AJ-TRAIN-DIST                   13110002
126700     MOVE LINK-SUB-DIST        TO AJ-TRAIN-SUB-DIST               13120002
126800     MOVE AJTRAINKEY-AREA      TO AJ-FS-TRAINKEY AJTRAINKEY       13130002
126900     START AJ-FILE KEY > AJ-FS-TRAINKEY                           13140002
127000           INVALID KEY CONTINUE                                   13150002
127100     END-START                                                    13160002
127200     IF SUCCESS                                                   13170002
127300        SET LOCALS-NOT-DONE TO TRUE                               13180002
127400        MOVE ZEROES         TO LOCAL-COUNT                        13190002
127500        PERFORM UNTIL LOCALS-DONE                                 13200002
127600        READ AJ-FILE NEXT RECORD INTO WS-ASGNED-JOBS              13210002
127700               AT END CONTINUE                                    13220002
127800        END-READ                                                  13230002
127900           IF SUCCESS                                             13240002
128000              IF AJ-TRAIN-DIST        = LINK-DIST  AND            13250002
128100                 AJ-TRAIN-SUB-DIST    = LINK-SUB-DIST             13260002
128200                 IF DIST-SDIST-POOL-LM-MC-JN                      13270002
128300                    CONTINUE                                      13280002
128400                 ELSE                                             13290002
128500                    MOVE AJ-JOB-ASGN-ID    TO JOB-DEF-CHECK       13300002
128600                    IF AJ-JOB-ON-DUTY                             13310002
128700                       ADD  1     TO LOCAL-COUNT                  13320002
128800                       PERFORM P1520-SET-LOCAL-INFO THRU          13330002
128900                               P1520-SET-LOCAL-INFO-EXIT          13340002
129000                    END-IF                                        13350002
129100                 END-IF                                           13360002
129200              ELSE                                                13370002
129300                 SET LOCALS-DONE TO TRUE                          13380002
129400                 PERFORM P1520-SET-LOCAL-INFO THRU                13390002
129500                         P1520-SET-LOCAL-INFO-EXIT                13400002
129600              END-IF                                              13410002
129700           ELSE                                                   13420002
129800              SET LOCALS-DONE TO TRUE                             13430002
129900              PERFORM P1520-SET-LOCAL-INFO THRU                   13440002
130000                      P1520-SET-LOCAL-INFO-EXIT                   13450002
130100           END-IF                                                 13460002
130200        END-PERFORM                                               13470002
130300     ELSE                                                         13480002
130400        SET LOCALS-DONE TO TRUE                                   13490002
130500        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     13500002
130600           MOVE AJ-FS-TRAINKEY        TO ERR-KEY                  13510002
130700           MOVE FILE-STATUS           TO ERR-FSTAT                13520002
130800           MOVE 'AJFILE '             TO ERR-FNAME                13530002
130900           MOVE 'CANNOT START AJFILE ' TO ERR-DESC                13540002
131000           GO TO P9999-GOT-PROBLEM                                13550002
131100        ELSE                                                      13560002
131200           SET LOCALS-DONE TO TRUE                                13570002
131300           PERFORM P1520-SET-LOCAL-INFO THRU                      13580002
131400                   P1520-SET-LOCAL-INFO-EXIT                      13590002
131500        END-IF                                                    13600002
131600     END-IF.                                                      13610002
131700                                                                  13620002
131800 P1520-SET-LOCAL-INFO.                                            13630002
131900                                                                  13640002
132000     IF LOCALS-DONE                                               13650002
132100        IF DIST-SDIST-POOL-LM-MC-JN                               13660002
132200           PERFORM P1015-PROCESS-UFP-RECORD                       13670002
132300           GO TO P1520-SET-LOCAL-INFO-EXIT                        13680002
132400        END-IF                                                    13690002
132500        IF LOCAL-COUNT NOT > ZEROES                               13700002
132600           GO TO P1520-SET-LOCAL-INFO-EXIT                        13710002
132700        ELSE                                                      13720002
132800           PERFORM P1300-WRITE-TRAIN-DETAIL                       13730002
132900           GO TO P1520-SET-LOCAL-INFO-EXIT                        13740002
133000        END-IF                                                    13750002
133100     END-IF                                                       13760002
133200                                                                  13770002
133300     IF AJ-TRAIN-SYMBOL = LOCAL-ENROUTE                           13780002
133400        PERFORM P1700-GET-LOCAL-DETAIL-INFO                       13790002
133500     ELSE                                                         13800002
133600        IF LOCAL-ENROUTE = SPACES                                 13810002
133700           PERFORM P1600-WRITE-NEW-LOCAL-LINE                     13820002
133800           PERFORM P1700-GET-LOCAL-DETAIL-INFO                    13830002
133900        ELSE                                                      13840002
134000           PERFORM P1300-WRITE-TRAIN-DETAIL                       13850002
134100           PERFORM P1600-WRITE-NEW-LOCAL-LINE                     13860002
134200           PERFORM P1700-GET-LOCAL-DETAIL-INFO                    13870002
134300        END-IF                                                    13880002
134400     END-IF.                                                      13890002
134500                                                                  13900002
134600 P1520-SET-LOCAL-INFO-EXIT.                                       13910002
134700     EXIT.                                                        13920002
134800                                                                  13930002
134900 P1600-WRITE-NEW-LOCAL-LINE.                                      13940002
135000                                                                  13950002
135100     MOVE SPACES              TO LOCALS-ENROUTE-1                 13960002
135200     MOVE ' ON DUTY AT: '     TO LOCAL-DUTY                       13970002
135300     IF LINE-COUNT > 55                                           13980046
135400       PERFORM P1050-TRAINS-ENROUTE-TITLE                         13990002
135500     END-IF                                                       14000002
135600     ADD 1                    TO LINE-COUNT                       14010002
135700     MOVE AJ-TRAIN-SYMBOL     TO LOCAL-ENROUTE                    14020002
135800     PERFORM P1800-GET-LOCAL-DESCS                                14030002
135900     MOVE WK-DESC             TO LOCAL-ENROUTE-NAME               14040002
136000     MOVE WK-FROM             TO LOCAL-ENROUTE-FROM               14050002
136100     IF AJ-ON-DUTY-TIME > ZEROES                                  14060002
136200        MOVE AJ-ON-DUTY-TIME TO WORK-TIME                         14070002
136300        MOVE WK-YR TO LOCAL-YR                                    14080002
136400        MOVE WK-MO TO LOCAL-MO                                    14090002
136500        MOVE WK-DY TO LOCAL-DY                                    14100002
136600        MOVE WORK-HR-MN TO LOCAL-HR-MN                            14110002
136700        MOVE '/' TO LOCAL-FIL1 LOCAL-FIL2                         14120002
136800        MOVE ' - ' TO LOCAL-FIL3                                  14130002
136900     ELSE                                                         14140002
137000        MOVE SPACES TO LOCAL-HR-MN LOCAL-YR LOCAL-MO LOCAL-DY     14150002
137100             LOCAL-FIL1 LOCAL-FIL2 LOCAL-FIL3                     14160002
137200     END-IF                                                       14170002
137300     WRITE PL-R FROM LOCALS-ENROUTE-1 AFTER ADVANCING 2 LINE      14180002
137400     ADD 2 TO LINE-COUNT.                                         14190002
137500                                                                  14200002
137600 P1700-GET-LOCAL-DETAIL-INFO.                                     14210002
137700                                                                  14220002
137800     SET SEARCH-NOT-DONE               TO TRUE                    14230002
137900     PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CRAFT-TABLE-MAX   14240002
138000        OR SEARCH-DONE                                            14250002
138100       IF AJ-TRAIN-CC = CT-CRAFT-CODE(J)                          14260002
138200          SET SEARCH-DONE             TO TRUE                     14270002
138300          MOVE SPACES                 TO SAVE-CREW-TURN(J)        14280002
138400          PERFORM P7800-GET-AJ-EMPS                               14290002
138500          IF ON-DUTY-EMP > ZERO                                   14300002
138600             MOVE ON-DUTY-EMP TO MSTRNBRK                         14310002
138700             PERFORM P8500-READ-MASTER                            14320002
138800             MOVE EMP-NAME OF WS-MSTR TO SAVE-CREW-NAME(J)        14330002
138900             MOVE EMP-NBR  OF WS-MSTR TO PS94-EMP-NBR             14340009
139000             PERFORM P4000-GET-HOS                                14350009
139100             MOVE WS-TOT-TM           TO SAVE-CREW-HOS-TOTAL(J)   14360010
139200             MOVE WS-LIMBO-TM         TO SAVE-CREW-HOS-LIMBO(J)   14370010
139300             MOVE WS-CONSEC-STARTS    TO SAVE-CREW-HOS-ST(J)      14380017
139400          ELSE                                                    14390002
139500             MOVE '<< VACANT JOB  >>' TO SAVE-CREW-NAME(J)        14400002
139600          END-IF                                                  14410002
139700          IF CT-CRAFT-CODE(J) = 'B1'                              14420002
139800             MOVE 'REAR BRKM:'         TO SAVE-CREW-CRAFT(J)      14430002
139900          ELSE                                                    14440002
140000             IF CT-CRAFT-CODE(J) = 'B2'                           14450002
140100                MOVE 'HEAD BRKM:'         TO SAVE-CREW-CRAFT(J)   14460002
140200             ELSE                                                 14470002
140300                MOVE CT-CRAFT-DESC(J)     TO SAVE-CREW-CRAFT(J)   14480002
140400             END-IF                                               14490002
140500          END-IF                                                  14500002
140600       END-IF                                                     14510002
140700     END-PERFORM.                                                 14520002
140800                                                                  14530002
140900                                                                  14540002
141000 P1800-GET-LOCAL-DESCS.                                           14550002
141100                                                                  14560002
141200     MOVE SPACES                    TO WS-TRCN-FILE               14570002
141300                                       TRCNKEY3                   14580002
141400     IF AJ-TRAIN-DIST > SPACES                                    14590002
141500        MOVE AJ-TRAIN-DIST             TO TRCN-DIST3              14600002
141600     ELSE                                                         14610002
141700        MOVE AJ-JOB-DIST               TO TRCN-DIST3              14620002
141800     END-IF                                                       14630002
141900     IF AJ-TRAIN-SUB-DIST > SPACES                                14640002
142000        MOVE AJ-TRAIN-SUB-DIST         TO TRCN-SDIST3             14650002
142100     ELSE                                                         14660002
142200        MOVE AJ-JOB-SUB-DIST           TO TRCN-SDIST3             14670002
142300     END-IF                                                       14680002
142400     MOVE AJ-JOB-ASGN-ID            TO TRCN-ASSIGNMENT            14690002
142500     MOVE TRCN-KEY3                 TO TRCN-FS-KEY3 TRCNKEY3      14700002
142600     READ TRCN-FILE INTO WS-TRCN-FILE                             14710002
142700        KEY IS TRCN-FS-KEY3                                       14720002
142800        INVALID KEY CONTINUE                                      14730002
142900     END-READ                                                     14740002
143000     IF SUCCESS                                                   14750002
143100        IF TRCN-DIST = (AJ-TRAIN-DIST OR AJ-JOB-DIST)          AND14760002
143200           TRCN-SDIST = (AJ-TRAIN-SUB-DIST OR AJ-JOB-SUB-DIST) AND14770002
143300           TRCN-DESCRIPTION > SPACES                              14780002
143400           MOVE TRCN-DESCRIPTION       TO WK-DESC                 14790002
143500        ELSE                                                      14800002
143600           MOVE SPACES               TO WK-DESC                   14810002
143700        END-IF                                                    14820002
143800     ELSE                                                         14830002
143900        MOVE SPACES                 TO WK-DESC                    14840002
144000        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     14850002
144100           MOVE 'P1800'             TO ERR-PARAGRAPH              14860002
144200           MOVE TRCN-FS-KEY3        TO ERR-KEY                    14870002
144300           MOVE FILE-STATUS         TO ERR-FSTAT                  14880002
144400           MOVE 'TRCNKEY3'          TO ERR-FNAME                  14890002
144500           MOVE 'START'             TO ERR-DESC                   14900002
144600           GO TO P9999-GOT-PROBLEM                                14910002
144700        END-IF                                                    14920002
144800     END-IF                                                       14930002
144900                                                                  14940002
145000     MOVE SPACES                     TO WS-CNTL-FILE              14950002
145100     SET SUB-DIST-TYPE-REC           TO TRUE                      14960002
145200     MOVE AJ-TRAIN-DIST              TO CNTL-DIST                 14970002
145300     MOVE AJ-TRAIN-SUB-DIST          TO CNTL-SUB-DIST             14980002
145400     MOVE CNTLKEY-AREA               TO CNTL-FS-KEY               14990002
145500     PERFORM P8000-READ-CNTLFILE                                  15000002
145600     IF SUB-DIST-TYPE-REC AND CNTL-SUB-DIST-NAME > SPACES         15010002
145700        MOVE CNTL-SUB-DIST-NAME      TO WK-FROM                   15020002
145800     ELSE                                                         15030002
145900        MOVE '<NOT ON FILE> '        TO WK-FROM                   15040002
146000     END-IF.                                                      15050002
146100                                                                  15060002
146200 P2000-POOLS.                                                     15070002
146300                                                                  15080002
146400     MOVE 'P2000' TO ERR-PARAGRAPH                                15090002
146500     IF LINE-COUNT > 55                                           15100046
146600       PERFORM P2300-POOL-TITLE                                   15110002
146700     ELSE                                                         15120002
146800       WRITE PL-R FROM CREWS-IN-TOWN-TITLE                        15130002
146900                            AFTER ADVANCING 2 LINES               15140002
147000       ADD 2 TO LINE-COUNT                                        15150002
147100     END-IF                                                       15160002
147200     MOVE ZEROES         TO WS-TERM                               15170002
147300     PERFORM P2100-POOLS-IN-TOWN                                  15180002
147400                                                                  15190002
147500     IF LINK-DIST = WS-DIST-LM AND                                15200002
147600        LINK-SUB-DIST = WS-SUB-DIST-MC AND                        15210002
147700        DIST-SDIST-POOL-LM-MC-JN                                  15220002
147800        MOVE WS-SUB-DIST-JX  TO LINK-SUB-DIST                     15230002
147900        MOVE SPACES          TO DIST-SDIST-POOL-FLAG              15240002
148000     END-IF                                                       15250002
148100                                                                  15260002
148200     PERFORM P2400-LOCALS-IN-TOWN                                 15270002
148300     MOVE 1              TO WS-TERM                               15280002
148400     IF LINE-COUNT > 55                                           15290046
148500       PERFORM P2300-POOL-TITLE                                   15300002
148600     ELSE                                                         15310002
148700       WRITE PL-R FROM CREWS-OUT-TOWN-TITLE                       15320002
148800                            AFTER ADVANCING 2 LINES               15330002
148900       ADD 2             TO LINE-COUNT                            15340002
149000     END-IF                                                       15350002
149100     PERFORM P2100-POOLS-IN-TOWN.                                 15360002
149200                                                                  15370002
149300 P2100-POOLS-IN-TOWN.                                             15380002
149400                                                                  15390002
149500     MOVE 'P2100'                 TO ERR-PARAGRAPH                15400002
149600     MOVE SPACES                  TO DIST-SDIST-POOL-FLAG         15410002
149700     SET NOT-DONE                 TO TRUE                         15420002
149800     PERFORM VARYING POOL-SUB FROM 1 BY 1                         15430002
149900         UNTIL DONE OR POOL-SUB > RPT-MAX                         15440002
150000       IF POOL-P(POOL-SUB) = SPACES                               15450002
150100          SET DONE                TO TRUE                         15460002
150200       ELSE                                                       15470002
150300          MOVE ZEROS              TO CREW-COUNT                   15480002
150400          MOVE SPACES             TO WS-CNTL-FILE                 15490002
150500          MOVE SPACE              TO WORK-CNTLKEY                 15500002
150600                                     UNDISTURBED-REST-FLAG        15510002
150700          MOVE '3A'               TO WK-CNTL-REC-TYPE             15520002
150800          MOVE LINK-DIST          TO WK-CNTL-DIST                 15530002
150900          MOVE LINK-SUB-DIST      TO WK-CNTL-SUB-DIST             15540002
151000          IF DIST-SDIST-POOL-LM-MC-JN                             15550002
151100             MOVE WS-SUB-DIST-JX  TO WK-CNTL-SUB-DIST             15560002
151200                                     LINK-SUB-DIST                15570002
151300          END-IF                                                  15580002
151400          IF LINK-DIST = WS-DIST-LM AND                           15590002
151500             LINK-SUB-DIST = WS-SUB-DIST-JX AND                   15600002
151600             POOL-P(POOL-SUB) = WS-POOL-JN                        15610002
151700             MOVE WS-DIST-LM      TO WK-CNTL-DIST                 15620002
151800             MOVE WS-SUB-DIST-MC  TO WK-CNTL-SUB-DIST             15630002
151900                                     LINK-SUB-DIST                15640002
152000             SET DIST-SDIST-POOL-LM-MC-JN                         15650002
152100                                  TO TRUE                         15660002
152200          END-IF                                                  15670002
152300          MOVE POOL-P(POOL-SUB)   TO WK-CNTL-POOL                 15680002
152400          MOVE 'F'                TO WK-CNTL-POOL-TYPE            15690002
152500          MOVE WORK-CNTLKEY       TO CNTL-FS-KEY                  15700002
152600          PERFORM P8000-READ-CNTLFILE                             15710002
152700          MOVE CNTL-POOL-UNDIST-REST-FLAG                         15720002
152800                                  TO UNDISTURBED-REST-FLAG        15730002
152900          MOVE '03'               TO WK-CNTL-REC-TYPE             15740002
153000          MOVE WORK-CNTLKEY       TO CNTL-FS-KEY                  15750002
153100          PERFORM P8000-READ-CNTLFILE                             15760002
153200          MOVE SPACES          TO EN-ID-POOL-FLAG                 15770002
153300                                  TR-ID-POOL-FLAG                 15780002
153400                                  POOL-SERVICE-FLAG               15790002
153500          SET NEW-POOL         TO TRUE                            15800002
153600          IF SUCCESS                                              15810002
153700             MOVE WS-LOCAL-DATE-TIME-CENT                         15820028
153800                                TO CHECK-REST-TIME-CENT           15830002
153900              IF UNDISTURBED-REST                                 15840002
154000                 MOVE ZEROS        TO DATE-CONVERSION-PARMS       15850002
154100                 SET PARM-SUBTRACT TO TRUE                        15860002
154200                 MOVE CHECK-REST-DATE                             15870002
154300                                   TO PARM-PRI-DATE-GREG          15880002
154400                 MOVE CK-HR-MN     TO PARM-PRI-HRMN               15890002
154500                 MOVE '0400'       TO PARM-SEC-HRMN               15900002
154600                 CALL P803-PGM  USING DATE-CONVERSION-PARMS       15910002
154700                 MOVE PARM-RES-DATE-GREG                          15920002
154800                                   TO CHECK-REST-DATE             15930002
154900                 MOVE PARM-RES-GREG-CENT                          15940002
155000                                   TO CHECK-REST-CE               15950002
155100                 MOVE PARM-RES-HRMN                               15960002
155200                                   TO CK-HR-MN                    15970002
155300              END-IF                                              15980002
155400              IF CNTL-ALT-DIST > SPACE                            15990002
155500                 AND CNTL-ALT-SUB-DIST > SPACE                    16000002
155600                 IF CNTL-ALT-EN-POOL > SPACE                      16010002
155700                    SET EN-ID-POOL TO TRUE                        16020002
155800                 END-IF                                           16030002
155900                 IF CNTL-ALT-TR-POOL > SPACE                      16040002
156000                    SET TR-ID-POOL TO TRUE                        16050002
156100                 END-IF                                           16060002
156200              END-IF                                              16070002
156300             MOVE CNTL-POOL-SVC   TO POOL-SERVICE-FLAG            16080002
156400             MOVE CNTL-POOL-NAME  TO CREWS-IN-TOWN-POOL           16090002
156500             PERFORM P2110-LOAD-CC-ARRAY                          16100002
156600             IF LINE-COUNT > 55                                   16110046
156700                 PERFORM P2300-POOL-TITLE                         16120002
156800             END-IF                                               16130002
156900             WRITE PL-R FROM CREWS-IN-TOWN-1 AFTER                16140002
157000                     ADVANCING 2 LINES                            16150002
157100             ADD 2                 TO LINE-COUNT                  16160002
157200             MOVE SPACES           TO WORK-UFPPOS-KEY             16170002
157300             MOVE LINK-DIST        TO POS-DIST                    16180002
157400             MOVE LINK-SUB-DIST    TO POS-SUB-DIST                16190002
157500             MOVE POOL-P(POOL-SUB) TO POS-POOL                    16200002
157600             MOVE ZEROES           TO POS-TERMINAL                16210002
157700                                      POS-TIME                    16220002
157800             MOVE ZEROES           TO CREW-COUNT                  16230002
157900                                      SAVE-TERM                   16240002
158000             PERFORM P2150-CHECK-POOL-STATUS                      16250002
158100             IF CREW-COUNT NOT > ZEROES                           16260002
158200                IF WS-TERM = ZEROES                               16270002
158300                   MOVE '<<   NO CREWS IN TOWN   >>'              16280002
158400                                      TO CREW-MESSAGE             16290002
158500                   WRITE PL-R FROM CREWS-MESS-LINE AFTER          16300002
158600                                      ADVANCING 1 LINES           16310002
158700                   ADD 1              TO LINE-COUNT               16320002
158800                ELSE                                              16330002
158900                   MOVE '<< NO CREWS OUT OF TOWN >>'              16340002
159000                                      TO CREW-MESSAGE             16350002
159100                   WRITE PL-R FROM CREWS-MESS-LINE AFTER          16360002
159200                                      ADVANCING 1 LINES           16370002
159300                   ADD 1              TO LINE-COUNT               16380002
159400                END-IF                                            16390002
159500             END-IF                                               16400002
159600          END-IF                                                  16410002
159700       END-IF                                                     16420002
159800     END-PERFORM.                                                 16430002
159900                                                                  16440002
160000 P2110-LOAD-CC-ARRAY.                                             16450002
160100                                                                  16460002
160200     MOVE SPACES                TO CRAFT-TABLE-AREA               16470002
160300     MOVE CNTL-EN-FI TO EN-FI-MARRIED-FLAG                        16480002
160400     IF EN-FI-MARRIED                                             16490002
160500        IF CNTL-POOL-FI-CRAFT NOT = ('Y' AND 'O')                 16500002
160600           SET EN-SE-MARRIED TO TRUE                              16510002
160700        END-IF                                                    16520002
160800     END-IF                                                       16530002
160900     MOVE CNTL-CO-B1-B2 TO CO-BK-MARRIED-FLAG                     16540002
161000     IF CO-BK-MARRIED                                             16550002
161100        IF CNTL-POOL-B1-CRAFT = ('Y' OR 'O')                      16560002
161200           SET CO-B1-MARRIED TO TRUE                              16570002
161300        END-IF                                                    16580002
161400        IF CNTL-POOL-B2-CRAFT = ('Y' OR 'O')                      16590002
161500           SET CO-B2-MARRIED TO TRUE                              16600002
161600        END-IF                                                    16610002
161700        IF CNTL-POOL-BG-CRAFT = ('Y' OR 'O')                      16620002
161800           SET CO-BG-MARRIED TO TRUE                              16630002
161900        END-IF                                                    16640002
162000     END-IF                                                       16650002
162100     MOVE CNTL-B1-B2 TO B1-B2-MARRIED-FLAG                        16660002
162200     MOVE CNTL-EN-ET TO EN-ET-MARRIED-FLAG                        16670002
162300     MOVE CNTL-CO-TT TO CO-TT-MARRIED-FLAG                        16680002
162400*                                                                 16690002
162500*    LOAD THE 'CRAFT CODES'                                       16700002
162600*                                                                 16710002
162700     MOVE ZERO                  TO CC-SUB                         16720002
162800                                   CC-SUB2                        16730002
162900*                                                                 16740002
163000*    PROCESS ENGINEER                                             16750002
163100*                                                                 16760002
163200     IF CNTL-POOL-EN-CRAFT = ('Y' OR 'O')                         16770002
163300        ADD 1                      TO CC-SUB                      16780002
163400        MOVE ENGINEER-MSTR-CC      TO PRIMARY-CC(CC-SUB)          16790002
163500        IF EN-FI-MARRIED                                          16800002
163600           ADD 1 TO CC-SUB2                                       16810002
163700           IF EN-SE-MARRIED                                       16820002
163800              MOVE 2ND-ENGINEER-MSTR-CC                           16830002
163900                                   TO ASSOC-CC(CC-SUB, CC-SUB2)   16840002
164000              IF CNTL-POOL-SE-CRAFT = 'O'                         16850002
164100                 SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE        16860002
164200              END-IF                                              16870002
164300           ELSE                                                   16880002
164400              MOVE FIREMAN-MSTR-CC                                16890002
164500                                  TO ASSOC-CC(CC-SUB, CC-SUB2)    16900002
164600              IF CNTL-POOL-FI-CRAFT = 'O'                         16910002
164700                 SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE        16920002
164800              END-IF                                              16930002
164900           END-IF                                                 16940002
165000        END-IF                                                    16950002
165100        IF EN-ET-MARRIED                                          16960002
165200           ADD 1 TO CC-SUB2                                       16970002
165300           MOVE ENGINEER-TRAINEE-MSTR-CC                          16980002
165400                     TO ASSOC-CC(CC-SUB, CC-SUB2)                 16990002
165500           SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE              17000002
165600        END-IF                                                    17010002
165700*                                                                 17020002
165800*       PROCESS RESERVE ENGINEER/SECOND ENGINEER                  17030002
165900*                                                                 17040002
166000        IF NOT EN-FI-MARRIED                                      17050002
166100           IF CNTL-POOL-FI-CRAFT = ('Y' OR 'O')                   17060002
166200              ADD 1 TO CC-SUB                                     17070002
166300              MOVE FIREMAN-MSTR-CC TO PRIMARY-CC(CC-SUB)          17080002
166400           ELSE                                                   17090002
166500              IF CNTL-POOL-SE-CRAFT  = ('Y' OR 'O')               17100002
166600                 ADD 1 TO CC-SUB                                  17110002
166700                 MOVE 2ND-ENGINEER-MSTR-CC TO PRIMARY-CC(CC-SUB)  17120002
166800              END-IF                                              17130002
166900           END-IF                                                 17140002
167000        END-IF                                                    17150002
167100*                                                                 17160002
167200*       PROCESS ENGINEER TRAINEE                                  17170002
167300*                                                                 17180002
167400        IF NOT EN-ET-MARRIED                                      17190002
167500           IF CNTL-POOL-EN-CRAFT = ('Y' OR 'O')                   17200002
167600              ADD 1 TO CC-SUB                                     17210002
167700              MOVE ENGINEER-TRAINEE-MSTR-CC TO PRIMARY-CC(CC-SUB) 17220002
167800           END-IF                                                 17230002
167900        END-IF                                                    17240002
168000     END-IF                                                       17250002
168100*                                                                 17260002
168200*    PROCESS CONDUCTOR                                            17270002
168300*                                                                 17280002
168400     IF CNTL-POOL-CO-CRAFT = ('Y' OR 'O')                         17290002
168500        ADD 1                       TO CC-SUB                     17300002
168600        MOVE ZEROS                  TO CC-SUB2                    17310002
168700        MOVE CONDUCTOR-MSTR-CC      TO PRIMARY-CC(CC-SUB)         17320002
168800        IF CO-BK-MARRIED                                          17330002
168900           IF CO-B1-MARRIED                                       17340002
169000              ADD 1 TO CC-SUB2                                    17350002
169100              MOVE B1-MSTR-CC TO ASSOC-CC(CC-SUB, CC-SUB2)        17360002
169200              IF CNTL-POOL-B1-CRAFT = 'O'                         17370002
169300                 SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE        17380002
169400              END-IF                                              17390002
169500           END-IF                                                 17400002
169600           IF CO-B2-MARRIED                                       17410002
169700              ADD 1 TO CC-SUB2                                    17420002
169800              MOVE B2-MSTR-CC TO ASSOC-CC(CC-SUB, CC-SUB2)        17430002
169900              IF CNTL-POOL-B2-CRAFT = 'O'                         17440002
170000                 SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE        17450002
170100              END-IF                                              17460002
170200           END-IF                                                 17470002
170300           IF CO-BG-MARRIED                                       17480002
170400              ADD 1 TO CC-SUB2                                    17490002
170500              MOVE BAGGAGEMAN-MSTR-CC TO ASSOC-CC(CC-SUB, CC-SUB2)17500002
170600              IF CNTL-POOL-BG-CRAFT = 'O'                         17510002
170700                 SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE        17520002
170800              END-IF                                              17530002
170900           END-IF                                                 17540002
171000        END-IF                                                    17550002
171100        IF CO-TT-MARRIED                                          17560002
171200           ADD 1 TO CC-SUB2                                       17570002
171300           MOVE TRAINMAN-TRAINEE-MSTR-CC                          17580002
171400                     TO ASSOC-CC(CC-SUB, CC-SUB2)                 17590002
171500           SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE              17600002
171600        END-IF                                                    17610002
171700     END-IF                                                       17620002
171800*                                                                 17630002
171900*       PROCESS BRAKEMAN                                          17640002
172000*                                                                 17650002
172100        IF NOT CO-BK-MARRIED                                      17660002
172200           IF CNTL-POOL-B1-CRAFT = ('Y' OR 'O')                   17670002
172300              ADD 1                  TO CC-SUB                    17680002
172400              MOVE ZEROS             TO CC-SUB2                   17690002
172500              MOVE B1-MSTR-CC             TO PRIMARY-CC(CC-SUB)   17700002
172600              IF B1-B2-MARRIED                                    17710002
172700                 ADD 1               TO CC-SUB2                   17720002
172800                 MOVE B2-MSTR-CC   TO ASSOC-CC(CC-SUB, CC-SUB2)   17730002
172900              END-IF                                              17740002
173000           END-IF                                                 17750002
173100        END-IF                                                    17760002
173200*                                                                 17770002
173300*       PROCESS HEAD BRAKEMAN                                     17780002
173400*                                                                 17790002
173500        IF NOT CO-BK-MARRIED                                      17800002
173600           AND NOT B1-B2-MARRIED                                  17810002
173700           IF CNTL-POOL-B2-CRAFT = ('Y' OR 'O')                   17820002
173800              ADD 1 TO CC-SUB                                     17830002
173900              MOVE B2-MSTR-CC TO PRIMARY-CC(CC-SUB)               17840002
174000           END-IF                                                 17850002
174100        END-IF                                                    17860002
174200*                                                                 17870002
174300*       PROCESS TRAINMAN TRAINEE                                  17880002
174400*                                                                 17890002
174500        IF NOT CO-TT-MARRIED                                      17900002
174600           ADD 1 TO CC-SUB                                        17910002
174700           MOVE TRAINMAN-TRAINEE-MSTR-CC TO PRIMARY-CC(CC-SUB)    17920002
174800        END-IF                                                    17930002
174900*                                                                 17940002
175000*       PROCESS BAGGAGEMAN                                        17950002
175100*                                                                 17960002
175200        IF NOT CO-BG-MARRIED                                      17970002
175300           IF CNTL-POOL-BG-CRAFT = ('Y' OR 'O')                   17980002
175400              ADD 1 TO CC-SUB                                     17990002
175500              MOVE BAGGAGEMAN-MSTR-CC TO PRIMARY-CC(CC-SUB)       18000002
175600           END-IF                                                 18010002
175700        END-IF                                                    18020002
175800*                                                                 18030002
175900*       PROCESS ASSISTANT CONDUCTOR                               18040002
176000*                                                                 18050002
176100        IF CNTL-POOL-AC-CRAFT = ('Y' OR 'O')                      18060002
176200           ADD 1 TO CC-SUB                                        18070002
176300           MOVE ASST-COND-MSTR-CC TO PRIMARY-CC(CC-SUB)           18080002
176400        END-IF.                                                   18090002
176500                                                                  18100002
176600 P2150-CHECK-POOL-STATUS.                                         18110002
176700                                                                  18120002
176800     MOVE SPACES                 TO WS-UFP                        18130002
176900     MOVE ZEROS                  TO POOL-POS-CNT                  18140002
177000     SET POOLS-NOT-DONE          TO TRUE                          18150002
177100     PERFORM UNTIL POOLS-DONE                                     18160002
177200        MOVE ZEROS               TO CC-FOUND-FLAG                 18170002
177300        ADD 1                    TO POOL-POS-CNT                  18180002
177400        MOVE ZEROES              TO POS-TIME                      18190002
177500        IF MINE-TURN-SVC                                          18200002
177600           MOVE SPACES           TO POS-DATE-TIME                 18210002
177700        END-IF                                                    18220002
177800        PERFORM VARYING CC-SUB FROM 1 BY 1                        18230002
177900           UNTIL CC-SUB > CC-MAX                                  18240002
178000           IF PRIMARY-CC(CC-SUB) > SPACE                          18250002
178100              AND NOT PRIMARY-CC-DONE(CC-SUB)                     18260002
178200              IF PRIMARY-CC-LAST-KEY(CC-SUB) NOT > SPACE          18270002
178300                 MOVE PRIMARY-CC(CC-SUB) TO POS-CC                18280002
178400              ELSE                                                18290002
178500                 MOVE PRIMARY-CC-LAST-KEY(CC-SUB)                 18300002
178600                      TO WORK-UFPPOS-KEY                          18310002
178700              END-IF                                              18320002
178800              MOVE WORK-UFPPOS-KEY     TO UFPPOS-FS-KEY           18330002
178900              START UFP-FILE KEY > UFPPOS-FS-KEY                  18340002
179000                    INVALID KEY CONTINUE                          18350002
179100              END-START                                           18360002
179200              IF SUCCESS                                          18370002
179300                 READ UFP-FILE NEXT RECORD INTO WS-UFP            18380002
179400                      AT END CONTINUE                             18390002
179500                 END-READ                                         18400002
179600                 IF SUCCESS                                       18410002
179700                    MOVE POOL-CRAFT-CODE2 IN WS-UFP               18420002
179800                                          TO WS-CRAFT-CODE-CHECK  18430002
179900                    SET DE-YYMMDD-FORMAT  TO TRUE                 18440002
180000                    MOVE UFP-POS-DATE-TIME(1:6) TO DE-YYMMDD      18450002
180100                    PERFORM P8998-DATEEDIT                        18460002
180200                    MOVE DE-CCYYMMDD      TO DE-COMPARE1-DATE     18470002
180300                    MOVE UFP-POS-DATE-TIME(7:4)                   18480002
180400                                          TO DE-COMPARE1-TIME     18490002
180500                    IF DIST OF WS-UFP = POS-DIST                  18500002
180600                      AND SUB-DIST OF WS-UFP = POS-SUB-DIST       18510002
180700                      AND POOL-NAME OF WS-UFP = POS-POOL          18520002
180800                      AND POOL-CRAFT-CODE2 = POS-CC               18530002
180900                      AND ((ENGINE-CRAFT AND EN-ID-POOL)          18540002
181000                        OR (TRAIN-CRAFT AND TR-ID-POOL)           18550002
181100                        OR MINE-TURN-SVC OR                       18560002
181200                      DE-COMPARE1-DATE-TIME NOT >                 18570002
181300                                WS-PRESENT-TIME-CENT)             18580002
181400                      MOVE UFPPOS-AREA                            18590002
181500                          TO PRIMARY-CC-LAST-KEY(CC-SUB)          18600002
181600                      SET CC-FOUND         TO TRUE                18610002
181700                      IF UFP-ON-BOARD                             18620002
181800                        IF (WS-TERM = ZEROES AND IN-TOWN)         18630002
181900                           OR                                     18640002
182000                           (WS-TERM > ZEROES AND OUT-OF-TOWN)     18650002
182100                            ADD 1                TO CREW-COUNT    18660002
182200                            PERFORM P2200-GET-TURN-DETAIL         18670002
182300                            PERFORM P2250-WRITE-POOL-DETAIL       18680002
182400                        END-IF                                    18690002
182500                      END-IF                                      18700002
182600                    ELSE                                          18710002
182700                       SET PRIMARY-CC-DONE(CC-SUB) TO TRUE        18720002
182800                    END-IF                                        18730002
182900                 ELSE                                             18740002
183000                    SET PRIMARY-CC-DONE(CC-SUB) TO TRUE           18750002
183100                 END-IF                                           18760002
183200              ELSE                                                18770002
183300                 SET PRIMARY-CC-DONE(CC-SUB)    TO TRUE           18780002
183400              END-IF                                              18790002
183500           END-IF                                                 18800002
183600        END-PERFORM                                               18810002
183700        IF NOT CC-FOUND                                           18820002
183800           SET POOLS-DONE TO TRUE                                 18830002
183900        END-IF                                                    18840002
184000     END-PERFORM.                                                 18850002
184100                                                                  18860002
184200 P2200-GET-TURN-DETAIL.                                           18870002
184300                                                                  18880002
184400     MOVE POOL-CRAFT-CODE2     TO WS-CRAFT-CODE-CHECK             18890002
184500                                                                  18900002
184600     IF WS-TERM > ZEROES                                          18910002
184700        IF SAVE-TERM NOT = IN-OUT-TERMINAL OF WS-UFP              18920002
184800           MOVE IN-OUT-TERMINAL OF WS-UFP TO SAVE-TERM            18930002
184900           MOVE WS-CNTL-FILE   TO SAVE-CNTL-AREA                  18940002
185000           MOVE SPACE          TO WORK-CNTLKEY                    18950002
185100           MOVE '03'           TO WK-CNTL-REC-TYPE                18960002
185200           MOVE DIST OF WS-UFP TO WK-CNTL-DIST                    18970002
185300           MOVE SUB-DIST OF WS-UFP  TO WK-CNTL-SUB-DIST           18980002
185400           MOVE POOL-NAME OF WS-UFP TO WK-CNTL-POOL               18990002
185500           MOVE 'F' TO WK-CNTL-POOL-TYPE                          19000002
185600           MOVE WORK-CNTLKEY TO CNTL-FS-KEY                       19010002
185700           PERFORM P8000-READ-CNTLFILE                            19020002
185800           MOVE CNTL-AWAY-TERM(SAVE-TERM)                         19030002
185900                                        TO CREWS-OUT-TOWN-TERM    19040002
186000           WRITE PL-R FROM CREWS-OUT-TOWN-1A AFTER                19050002
186100                          ADVANCING 1 LINES                       19060002
186200           ADD 1                          TO LINE-COUNT           19070002
186300           MOVE SAVE-CNTL-AREA TO WS-CNTL-FILE                    19080002
186400           MOVE CNTLKEY        TO WORK-CNTLKEY                    19090002
186500        END-IF                                                    19100002
186600     END-IF                                                       19110002
186700                                                                  19120002
186800     PERFORM P7500-GET-UFP-EMPS                                   19130002
186900                                                                  19140002
187000     MOVE TURN-NBR OF WS-UFP TO CREWS-IT-TURN                     19150002
187100                                                                  19160002
187200     MOVE SPACES             TO CREWS-IT-CRAFT                    19170002
187300     PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CRAFT-TABLE-MAX   19180002
187400        IF POOL-CRAFT-CODE2 = CT-CRAFT-CODE(J)                    19190002
187500          IF CT-CRAFT-CODE(J) = 'B1'                              19200002
187600             MOVE 'REAR BRKM:'         TO CREWS-IT-CRAFT          19210027
187700          ELSE                                                    19220002
187800             IF CT-CRAFT-CODE(J) = 'B2'                           19230002
187900                MOVE 'HEAD BRKM:'      TO CREWS-IT-CRAFT          19240027
188000             ELSE                                                 19250002
188100                MOVE CT-CRAFT-DESC(J)  TO CREWS-IT-CRAFT          19260002
188200                DISPLAY 'CREWS-IT-CRAFT  =' CREWS-IT-CRAFT        19270027
188300             END-IF                                               19280002
188400          END-IF                                                  19290002
188500        END-IF                                                    19300002
188600     END-PERFORM                                                  19310002
188700     IF CREWS-IT-CRAFT = SPACES                                   19320002
188800        MOVE 'UNKNOWN   '    TO CREWS-IT-CRAFT                    19330002
188900     END-IF                                                       19340002
189000                                                                  19350002
189100     IF NOT NEW-POOL                                              19360002
189200        IF POOL-POS-CNT NOT = SAVE-CNT                            19370002
189300            MOVE POOL-POS-CNT   TO CREWS-IT-POSITION              19380002
189400                                   SAVE-CNT                       19390002
189500        ELSE                                                      19400002
189600            MOVE SPACES         TO CREWS-IT-POSITION              19410002
189700        END-IF                                                    19420002
189800     ELSE                                                         19430002
189900        MOVE POOL-POS-CNT   TO CREWS-IT-POSITION                  19440002
190000                               SAVE-CNT                           19450002
190100        MOVE ZEROES         TO NEW-POOL-FLAG                      19460002
190200     END-IF                                                       19470002
190300*                                                                 19480023
190400     MOVE SPACES            TO CREWS-IT-HOS-AREA                  19490023
190500     IF (NOT IN-TOWN OR SUB-DIST OF WS-UFP NOT = SUB-DIST2        19500002
190600         OF WS-UFP) AND ON-DUTY-EMP NOT > ZERO                    19510002
190700       IF UFP-BLANK-TURN                                          19520002
190800         MOVE 'VACNT/BLANKABLE TURN' TO CREWS-IT-NAME             19530002
190900       ELSE                                                       19540002
191000         IF UFP-CLOSED-TURN                                       19550002
191100           MOVE 'CLOSED TURN         '                            19560002
191200                TO CREWS-IT-NAME                                  19570002
191300         ELSE                                                     19580002
191400           MOVE '<< VACANT TURN >>' TO CREWS-IT-NAME              19590002
191500         END-IF                                                   19600002
191600       END-IF                                                     19610002
191700       MOVE ZERO TO MSTRNBRK                                      19620002
191800       IF TEMP-EMP-ONE > ZERO AND TEMP-EMP-ONE NOT = ON-DUTY-EMP  19630002
191900         MOVE TEMP-EMP-ONE TO MSTRNBRK                            19640002
192000       ELSE                                                       19650002
192100         IF OWNER-EMP-NBR NOT = ON-DUTY-EMP                       19660002
192200           MOVE OWNER-EMP-NBR TO MSTRNBRK                         19670002
192300         END-IF                                                   19680002
192400       END-IF                                                     19690002
192500       IF MSTRNBRK > ZERO                                         19700002
192600         PERFORM P8500-READ-MASTER                                19710002
192700         MOVE EMP-NAME TO CREWS-IT-NAME                           19720009
192800         IF WS-CANADIAN-COMPANY                                   19730025
192900            MOVE SPACES          TO CREWS-IT-HOS-AREA             19740025
193000         ELSE                                                     19750025
193100*CNC0516-BEG                                                      19760049
193200            MOVE EMP-NBR OF WS-MSTR  TO PS94-EMP-NBR              19770041
193300*CNC0516-END                                                      19780049
193400            PERFORM P4000-GET-HOS                                 19790025
193500            STRING 'T '                                           19800027
193600              WS-TOT-TM-HH ':'                                    19810010
193700              WS-TOT-TM-MM                                        19820010
193800                ' L '                                             19830027
193900              WS-LIMBO-TM-HH ':'                                  19840010
194000              WS-LIMBO-TM-MM                                      19850010
194100                ' S:'                                             19860027
194200              WS-CONSEC-STARTS                                    19870017
194300              DELIMITED BY SIZE INTO CREWS-IT-HOS-AREA            19880010
194400         END-IF                                                   19890025
194500       END-IF                                                     19900002
194600     ELSE                                                         19910002
194700       IF ON-DUTY-EMP > ZERO                                      19920002
194800         MOVE ON-DUTY-EMP TO MSTRNBRK                             19930002
194900         PERFORM P8500-READ-MASTER                                19940002
195000         PERFORM P2210-GET-MASTER-INFO-FOR-TURN                   19950002
195100         MOVE EMP-NAME TO CREWS-IT-NAME                           19960002
195200         IF WS-CANADIAN-COMPANY                                   19970025
195300            MOVE SPACES          TO CREWS-IT-HOS-AREA             19980025
195400         ELSE                                                     19990025
195500*CNC0516-BEG                                                      20000049
195600            MOVE EMP-NBR OF WS-MSTR TO PS94-EMP-NBR               20010041
195700*CNC0516-END                                                      20020049
195800            PERFORM P4000-GET-HOS                                 20030025
195900            STRING 'T '                                           20040027
196000              WS-TOT-TM-HH ':'                                    20050010
196100              WS-TOT-TM-MM                                        20060010
196200                ' L '                                             20070027
196300              WS-LIMBO-TM-HH ':'                                  20080010
196400              WS-LIMBO-TM-MM                                      20090010
196500                ' S:'                                             20100027
196600              WS-CONSEC-STARTS                                    20110017
196700              DELIMITED BY SIZE INTO CREWS-IT-HOS-AREA            20120010
196800         END-IF                                                   20130025
196900       ELSE                                                       20140002
197000         IF TEMP-EMP-ONE > ZERO                                   20150002
197100           MOVE TEMP-EMP-ONE TO MSTRNBRK                          20160002
197200           PERFORM P8500-READ-MASTER                              20170002
197300           PERFORM P2210-GET-MASTER-INFO-FOR-TURN                 20180002
197400           MOVE EMP-NAME TO CREWS-IT-NAME                         20190002
197500           IF WS-CANADIAN-COMPANY                                 20200025
197600              MOVE SPACES          TO CREWS-IT-HOS-AREA           20210025
197700           ELSE                                                   20220025
197800*CNC0516-BEG                                                      20230049
197900              MOVE EMP-NBR OF WS-MSTR TO PS94-EMP-NBR             20240041
198000*CNC0516-END                                                      20250049
198100              PERFORM P4000-GET-HOS                               20260025
198200              STRING 'T '                                         20270027
198300                WS-TOT-TM-HH ':'                                  20280010
198400                WS-TOT-TM-MM                                      20290010
198500                  ' L '                                           20300027
198600                WS-LIMBO-TM-HH ':'                                20310010
198700                WS-LIMBO-TM-MM                                    20320010
198800                  ' S:'                                           20330027
198900                WS-CONSEC-STARTS                                  20340017
199000                DELIMITED BY SIZE INTO CREWS-IT-HOS-AREA          20350010
199100           END-IF                                                 20360025
199200         ELSE                                                     20370002
199300            IF OWNER-EMP-NBR = ZERO                               20380002
199400               MOVE SPACES           TO CREWS-IT-RESTED           20390003
199500               MOVE SPACES           TO CREWS-IT-HOS-AREA         20400023
199600               IF UFP-BLANK-TURN                                  20410002
199700                 MOVE '  BLANK  '    TO CREWS-IT-NAME             20420003
199800               ELSE                                               20430002
199900                 IF UFP-CLOSED-TURN                               20440002
200000                   MOVE '  CLOSED  ' TO CREWS-IT-NAME             20450002
200100                 ELSE                                             20460002
200200                   MOVE '<< OPEN TURN >>'                         20470002
200300                                     TO CREWS-IT-NAME             20480002
200400                 END-IF                                           20490002
200500               END-IF                                             20500002
200600            ELSE                                                  20510002
200700              MOVE OWNER-EMP-NBR TO MSTRNBRK                      20520002
200800              PERFORM P8500-READ-MASTER                           20530002
200900              PERFORM P2210-GET-MASTER-INFO-FOR-TURN              20540002
201000              MOVE EMP-NAME TO CREWS-IT-NAME                      20550002
201100              IF WS-CANADIAN-COMPANY                              20560025
201200                 MOVE SPACES          TO CREWS-IT-HOS-AREA        20570025
201300              ELSE                                                20580025
201400*CNC0516-BEG                                                      20590049
201500                 MOVE EMP-NBR OF WS-MSTR  TO PS94-EMP-NBR         20600041
201600*CNC0516-END                                                      20610049
201700                 PERFORM P4000-GET-HOS                            20620025
201800                 STRING 'T '                                      20630027
201900                   WS-TOT-TM-HH ':'                               20640010
202000                   WS-TOT-TM-MM                                   20650010
202100                     ' L '                                        20660027
202200                   WS-LIMBO-TM-HH ':'                             20670010
202300                   WS-LIMBO-TM-MM                                 20680010
202400                     ' S:'                                        20690027
202500                   WS-CONSEC-STARTS                               20700017
202600                   DELIMITED BY SIZE INTO CREWS-IT-HOS-AREA       20710010
202700              END-IF                                              20720025
202800            END-IF                                                20730002
202900         END-IF                                                   20740002
203000       END-IF                                                     20750002
203100     END-IF.                                                      20760002
203200*                                                                 20770002
203300 P2210-GET-MASTER-INFO-FOR-TURN.                                  20780002
203400*                                                                 20790002
203500     MOVE SPACES                 TO CREWS-IT-RESTED               20800002
203600     IF MSTRNBRK = ZEROES                                         20810002
203700         MOVE '<< VACANCY >>'    TO CREWS-IT-RESTED               20820002
203800     ELSE                                                         20830002
203900        IF AVAILABLE                                              20840002
204000           IF ON-DUTY-ASGNMT > SPACES                             20850002
204100               AND ON-DUTY-ASGNMT NOT = UFPTURN-AREA              20860002
204200               MOVE '<< VACANCY >>'  TO CREWS-IT-RESTED           20870002
204300           ELSE                                                   20880002
204400              IF TEMPORARY-ASGNMT > SPACES                        20890002
204500                  AND TEMPORARY-ASGNMT NOT = UFPTURN-AREA         20900002
204600                  MOVE '<< VACANCY >>'  TO CREWS-IT-RESTED        20910002
204700              ELSE                                                20920002
204800                   SET  DE-YYMMDD-FORMAT     TO TRUE              20930002
204900                   MOVE EMP-US-RSTD-DATE     TO DE-YYMMDD         20940002
205000                   PERFORM P8998-DATEEDIT                         20950002
205100                   MOVE DE-CCYYMMDD          TO DE-COMPARE1-DATE  20960002
205200                   MOVE EMP-US-RSTD-TIME     TO DE-COMPARE1-TIME  20970002
205300*                                                                 20980002
205400                   IF EMP-PERS-REST-NUM NOT > ZEROES              20990002
205500                      MOVE '000101010001'    TO EMP-PERS-REST     21000002
205600                   END-IF                                         21010002
205700                   PERFORM P5200-CHECK-COMPANY-CD                 21020005
205800                   IF APPLY-LEAD-TIME                             21030002
205900                      MOVE ZEROS          TO DATE-CONVERSION-PARMS21040002
206000                      SET PARM-ADD         TO TRUE                21050002
206100                      MOVE EMP-PERS-REST-DATE                     21060002
206200                                           TO PARM-PRI-DATE-GREG  21070002
206300                      MOVE EMP-PERS-REST-TIME                     21080002
206400                                           TO PARM-PRI-HRMN       21090002
206500                      MOVE '0200'          TO PARM-SEC-HRMN       21100002
206600                      CALL P803-PGM USING DATE-CONVERSION-PARMS   21110002
206700                      MOVE PARM-RES-DATE-GREG                     21120002
206800                                           TO DE-COMPARE2-YYMMDD  21130002
206900                      MOVE PARM-RES-GREG-CENT                     21140002
207000                                           TO DE-COMPARE2-CE      21150002
207100                      MOVE PARM-RES-HRMN   TO DE-COMPARE2-TIME    21160002
207200                   ELSE                                           21170002
207300                      SET  DE-YYMMDD-FORMAT   TO TRUE             21180002
207400                      MOVE EMP-PERS-REST-DATE TO DE-YYMMDD        21190002
207500                      PERFORM P8998-DATEEDIT                      21200002
207600                      MOVE DE-CCYYMMDD        TO DE-COMPARE2-DATE 21210002
207700                      MOVE EMP-PERS-REST-TIME TO DE-COMPARE2-TIME 21220002
207800                   END-IF                                         21230002
207900*                                                                 21240002
208000                   SET  DE-YYMMDD-FORMAT     TO TRUE              21250002
208100                   MOVE EMP-MTOD-DATE        TO DE-YYMMDD         21260002
208200                   PERFORM P8998-DATEEDIT                         21270002
208300                   MOVE DE-CCYYMMDD          TO DE-COMPARE3-DATE  21280002
208400                   MOVE EMP-MTOD-TIME        TO DE-COMPARE3-TIME  21290002
208500                   SET  DE-YYMMDD-FORMAT     TO TRUE              21300002
208600                   MOVE CHECK-REST-DATE      TO DE-YYMMDD         21310002
208700                   PERFORM P8998-DATEEDIT                         21320002
208800                   MOVE DE-YYMMDD-CE         TO CHECK-REST-CE     21330002
208900                   IF DE-COMPARE1-DATE-TIME >                     21340002
209000                                  CHECK-REST-TIME-CENT            21350002
209100                      MOVE EMP-US-RSTD-DATE TO WORK-DATE          21360002
209200                      MOVE EMP-US-RSTD-TIME TO WORK-HR-MN         21370002
209300                      MOVE WK-YR            TO FORM-YR            21380002
209400                      MOVE WK-DY            TO FORM-DY            21390002
209500                      MOVE WK-MO            TO FORM-MO            21400002
209600                      MOVE WORK-HR-MN       TO FORM-HRMN          21410002
209700                      MOVE FORMAT-DATE-AREA TO CREWS-IT-RESTED    21420002
209800                   END-IF                                         21430002
209900                   IF DE-COMPARE2-DATE-TIME >                     21440002
210000                                  CHECK-REST-TIME-CENT  AND       21450002
210100                      DE-COMPARE2-DATE-TIME >                     21460002
210200                                  DE-COMPARE1-DATE-TIME           21470002
210300                      MOVE DE-COMPARE2-YYMMDD TO WORK-DATE        21480002
210400                      MOVE DE-COMPARE2-TIME   TO WORK-HR-MN       21490002
210500                      MOVE WK-YR            TO FORM-YR            21500002
210600                      MOVE WK-DY            TO FORM-DY            21510002
210700                      MOVE WK-MO            TO FORM-MO            21520002
210800                      MOVE WORK-HR-MN       TO FORM-HRMN          21530002
210900                      MOVE FORMAT-DATE-AREA TO CREWS-IT-RESTED    21540002
211000                   END-IF                                         21550002
211100                   IF DE-COMPARE3-DATE-TIME >                     21560002
211200                                  CHECK-REST-TIME-CENT  AND       21570002
211300                      DE-COMPARE3-DATE-TIME >                     21580002
211400                                  DE-COMPARE2-DATE-TIME AND       21590002
211500                      DE-COMPARE3-DATE-TIME >                     21600002
211600                                  DE-COMPARE1-DATE-TIME           21610002
211700                      MOVE EMP-MTOD-DATE    TO WORK-DATE          21620002
211800                      MOVE EMP-MTOD-TIME    TO WORK-HR-MN         21630002
211900                      MOVE WK-YR            TO FORM-YR            21640002
212000                      MOVE WK-DY            TO FORM-DY            21650002
212100                      MOVE WK-MO            TO FORM-MO            21660002
212200                      MOVE WORK-HR-MN       TO FORM-HRMN          21670002
212300                      MOVE FORMAT-DATE-AREA TO CREWS-IT-RESTED    21680002
212400                   END-IF                                         21690002
212500                   IF CREWS-IT-RESTED NOT > SPACE                 21700002
212600                      MOVE '** RESTED **' TO CREWS-IT-RESTED      21710002
212700                   END-IF                                         21720002
212800              END-IF                                              21730002
212900           END-IF                                                 21740002
213000        ELSE                                                      21750002
213100*CNC0516-BEG                                                      21760039
213200            PERFORM P2215-GET-POOL-STATUS                         21770034
213300*           MOVE '<< VACANCY >>'  TO CREWS-IT-RESTED              21780034
213400*CNC0516-END                                                      21790044
213500        END-IF                                                    21800002
213600     END-IF.                                                      21810002
213700*                                                                 21820034
213800*CNC0516-BEG                                                      21830039
213900 P2215-GET-POOL-STATUS.                                           21840034
214000                                                                  21850034
214100     INITIALIZE WS-CREWS-IT-RESTED                                21860034
214200     EVALUATE TRUE                                                21870034
214300          WHEN OFF-MILES-DAYS                                     21880034
214400               PERFORM P9830-RETRIEVE-CNTL-INFO                   21890034
214500               IF P956-ST-RSN-NBR-DAYS-REQ                        21900034
214600               OR P956-ST-RSN-EXP-DATE-REQ                        21910034
214700                  PERFORM P3265-GET-DUEBACK-DATE                  21920034
214800                  IF WS-DUEBACK-FOUND-Y                           21930034
214900                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE121940034
215000                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME121950034
215010*CNC0573 - BEG                                                    21960060
215020                     IF WS-MASK-FLD-SCR-YES                       21970066
215050                        IF P956-MASK-FLD-SCR-YES                  21980060
215051*CNC0576 - BEG                                                    21990068
215054                        OR P956-MASK-HOLD-TURN                    22000068
215055                           MOVE '**'            TO WS-LAYOFF-CODE122010068
215056                           IF P956-MASK-HOLD-TURN                 22020068
215057                              MOVE 'HT' TO WS-LAYOFF-EM-CODE1     22030068
215058                           ELSE                                   22040068
215059                              MOVE '**' TO WS-LAYOFF-EM-CODE1     22050068
215060                           END-IF                                 22060068
215061*CNC0576 - END                                                    22070068
215062                        ELSE                                      22080060
215063                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE122090060
215064                           MOVE LAYOFF-EM-CODE  TO                22100060
215065                                WS-LAYOFF-EM-CODE1                22110060
215070                        END-IF                                    22120060
215080                     ELSE                                         22130060
215100                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE122140060
215200                        MOVE LAYOFF-EM-CODE     TO                22150060
215300                             WS-LAYOFF-EM-CODE1                   22160060
215301                     END-IF                                       22170066
215310*CNC0573 - END                                                    22180060
215400                     MOVE WS-CREWS-IT-RESTED    TO CREWS-IT-RESTED22190034
215500                  ELSE                                            22200034
215600                     MOVE '<< VACANCY >>'       TO                22210034
215700                           CREWS-IT-RESTED                        22220034
215800                  END-IF                                          22230034
215900               ELSE                                               22240039
216000                  PERFORM P3266-OFF-MILES-RETURN-DATE             22250039
216100                  MOVE WS-RETURN-DATE-1(1:4)    TO WS-RETURN-DATE122260051
216200                  MOVE SPACES                   TO WS-RETURN-TIME122270034
216210*CNC0573 - BEG                                                    22280060
216220                  IF WS-MASK-FLD-SCR-YES                          22290066
216240                     IF P956-MASK-FLD-SCR-YES                     22300060
216241*CNC0576 - BEG                                                    22310068
216251                     OR P956-MASK-HOLD-TURN                       22320068
216253                        MOVE '**'               TO WS-LAYOFF-CODE122330068
216254                        IF P956-MASK-HOLD-TURN                    22340068
216255                           MOVE 'HT'         TO WS-LAYOFF-EM-CODE122350069
216256                        ELSE                                      22360068
216257                           MOVE '**'         TO WS-LAYOFF-EM-CODE122370069
216258                        END-IF                                    22380068
216260*CNC0576 - END                                                    22390068
216261                     ELSE                                         22400068
216262                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE122410068
216263                        MOVE LAYOFF-EM-CODE     TO                22420068
216264                             WS-LAYOFF-EM-CODE1                   22430068
216265                     END-IF                                       22440068
216270                  ELSE                                            22450060
216300                     MOVE LAYOFF-CODE           TO WS-LAYOFF-CODE122460060
216400                     MOVE LAYOFF-EM-CODE        TO                22470060
216500                          WS-LAYOFF-EM-CODE1                      22480060
216501                  END-IF                                          22490066
216510*CNC0573 - END                                                    22500060
216600                  MOVE WS-CREWS-IT-RESTED       TO CREWS-IT-RESTED22510034
216700               END-IF                                             22520034
216800          WHEN VACATION                                           22530034
216900               PERFORM P3267-VACATION-RETURN-DATE                 22540034
217000               MOVE WS-RETURN-DATE-1(1:4)       TO WS-RETURN-DATE122550051
217100               MOVE WS-RETURN-DATE-1(5:4)       TO WS-RETURN-TIME122560051
217110*CNC0573 - BEG                                                    22570060
217120               IF WS-MASK-FLD-SCR-YES                             22580066
217131                  PERFORM P9830-RETRIEVE-CNTL-INFO                22590060
217140                  IF P956-MASK-FLD-SCR-YES                        22600060
217141*CNC0576 - BEG                                                    22610068
217142                  OR P956-MASK-HOLD-TURN                          22620068
217151                     MOVE '**'                  TO WS-LAYOFF-CODE122630068
217152                     IF P956-MASK-HOLD-TURN                       22640068
217153                        MOVE 'HT'            TO WS-LAYOFF-EM-CODE122650068
217154                     ELSE                                         22660068
217155                        MOVE '**'            TO WS-LAYOFF-EM-CODE122670068
217156                     END-IF                                       22680068
217157*CNC0576 - END                                                    22690068
217158                  ELSE                                            22700060
217159                     MOVE LAYOFF-CODE           TO WS-LAYOFF-CODE122710060
217160                     MOVE LAYOFF-EM-CODE        TO                22720060
217161                          WS-LAYOFF-EM-CODE1                      22730060
217170               ELSE                                               22740060
217200                  MOVE LAYOFF-CODE              TO WS-LAYOFF-CODE122750060
217300                  MOVE LAYOFF-EM-CODE           TO                22760060
217400                       WS-LAYOFF-EM-CODE1                         22770060
217401               END-IF                                             22780066
217410*CNC0573 - END                                                    22790060
217500               MOVE WS-CREWS-IT-RESTED          TO CREWS-IT-RESTED22800034
217600          WHEN EXCUSED-ABSENCE                                    22810034
217700           AND LAYOFF-EM-CODE = '69'                              22820034
217800               PERFORM P3265-GET-DUEBACK-DATE                     22830034
217900               IF WS-DUEBACK-FOUND-Y                              22840034
218000                 MOVE TASK-LO-EXP-DATE(3:4)     TO WS-RETURN-DATE122850039
218100                 MOVE TASK-LO-EXP-TIME          TO WS-RETURN-TIME122860039
218110*CNC0573 - BEG                                                    22870060
218111                 IF WS-MASK-FLD-SCR-YES                           22880066
218113                    PERFORM P9830-RETRIEVE-CNTL-INFO              22890060
218114                    IF P956-MASK-FLD-SCR-YES                      22900068
218115*CNC0576 - BEG                                                    22910068
218116                    OR P956-MASK-HOLD-TURN                        22920068
218117                       MOVE '**'                TO WS-LAYOFF-CODE122930068
218118                       IF P956-MASK-HOLD-TURN                     22940068
218119                          MOVE 'HT'          TO WS-LAYOFF-EM-CODE122950068
218120                       ELSE                                       22960068
218121                          MOVE '**'          TO WS-LAYOFF-EM-CODE122970068
218122                       END-IF                                     22980068
218123*CNC0576 - END                                                    22990068
218126                    ELSE                                          23000060
218127                       MOVE LAYOFF-CODE         TO WS-LAYOFF-CODE123010060
218128                       MOVE LAYOFF-EM-CODE      TO                23020060
218129                            WS-LAYOFF-EM-CODE1                    23030060
218130                    END-IF                                        23040060
218140                 ELSE                                             23050060
218200                    MOVE LAYOFF-CODE            TO WS-LAYOFF-CODE123060060
218300                    MOVE LAYOFF-EM-CODE         TO                23070060
218400                         WS-LAYOFF-EM-CODE1                       23080060
218401                 END-IF                                           23090066
218410*CNC0573 - END                                                    23100060
218500                 MOVE WS-CREWS-IT-RESTED        TO                23110039
218600                       CREWS-IT-RESTED                            23120034
218700               ELSE                                               23130039
218800                  MOVE '<< VACANCY >>'          TO CREWS-IT-RESTED23140039
218900               END-IF                                             23150039
219000          WHEN OTHER                                              23160034
219100              IF NOT AVAILABLE  AND                               23170034
219200                 NOT WORKING    AND                               23180034
219300                 NOT TO-PLACE                                     23190034
219400                 AND LAYOFF-TIME NUMERIC                          23200034
219500                  AND LAYOFF-TIME > ZERO                          23210034
219600                  PERFORM P3265-GET-DUEBACK-DATE                  23220034
219700                  IF WS-DUEBACK-FOUND-Y                           23230034
219800                    PERFORM P3268-CHECK-FOR-E95-DTTM              23240056
219900                    IF WS-E95 > SPACES                            23250056
219910*CNC0573 - BEG                                                    23260060
219911                       IF WS-MASK-FLD-SCR-YES                     23270066
219915                          PERFORM P9830-RETRIEVE-CNTL-INFO        23280064
219916                          IF P956-MASK-FLD-SCR-YES                23290060
219917*CNC0576 - BEG                                                    23300068
219919                          OR P956-MASK-HOLD-TURN                  23310068
219920                             MOVE '**'          TO WS-E95-CODE    23320068
219921                             IF P956-MASK-HOLD-TURN               23330068
219922                                MOVE 'HT'       TO WS-E95-EM-CODE 23340068
219923                             ELSE                                 23350068
219924                                MOVE '**'       TO WS-E95-EM-CODE 23360068
219925                             END-IF                               23370068
219927                          END-IF                                  23380070
219928                       END-IF                                     23390070
219932                       MOVE WS-E95              TO CREWS-IT-RESTED23400070
219934*CNC0576 - BEG                                                    23410070
219940*CNC0573 - END                                                    23420060
220100                    ELSE                                          23430056
220200                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE123440056
220300                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME123450056
220310*CNC0573 - BEG                                                    23460060
220311                     IF WS-MASK-FLD-SCR-YES                       23470066
220313                        PERFORM P9830-RETRIEVE-CNTL-INFO          23480060
220314                        IF P956-MASK-FLD-SCR-YES                  23490060
220315*CNC0576 - BEG                                                    23500068
220316                        OR P956-MASK-HOLD-TURN                    23510068
220317                           MOVE '**'           TO WS-LAYOFF-CODE1 23520069
220318                           IF P956-MASK-HOLD-TURN                 23530068
220319                              MOVE 'HT'     TO WS-LAYOFF-EM-CODE1 23540069
220320                           ELSE                                   23550068
220321                              MOVE '**'     TO WS-LAYOFF-EM-CODE1 23560069
220322                           END-IF                                 23570068
220323*CNC0576 - END                                                    23580068
220325                        ELSE                                      23590060
220326                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE123600060
220327                           MOVE LAYOFF-EM-CODE  TO                23610060
220328                                WS-LAYOFF-EM-CODE1                23620060
220329                        END-IF                                    23630060
220330                     ELSE                                         23640060
220400                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE123650060
220500                        MOVE LAYOFF-EM-CODE     TO                23660060
220600                             WS-LAYOFF-EM-CODE1                   23670060
220601                     END-IF                                       23680060
220610*CNC0573 - END                                                    23690060
220700                     MOVE WS-CREWS-IT-RESTED    TO CREWS-IT-RESTED23700056
220800                    END-IF                                        23710056
220900                  ELSE                                            23720034
221000                    MOVE '<< VACANCY >>'        TO CREWS-IT-RESTED23730056
221100                  END-IF                                          23740034
221200              END-IF                                              23750034
221300     END-EVALUATE.                                                23760039
221400*CNC0516-END                                                      23770034
221500                                                                  23780034
221600                                                                  23790002
221700 P2250-WRITE-POOL-DETAIL.                                         23800002
221800                                                                  23810002
221900     IF LINE-COUNT > 55                                           23820046
222000       PERFORM P2300-POOL-TITLE                                   23830002
222100     END-IF                                                       23840002
222200     WRITE  PL-R FROM CREWS-IN-TOWN-2 AFTER                       23850002
222300           ADVANCING 1 LINES                                      23860002
222400     ADD 1 TO LINE-COUNT.                                         23870002
222500                                                                  23880002
222600                                                                  23890002
222700 P2300-POOL-TITLE.                                                23900002
222800                                                                  23910002
222900     PERFORM P9000-TITLE                                          23920002
223000     IF WS-TERM = ZEROES                                          23930002
223100        WRITE PL-R FROM CREWS-IN-TOWN-TITLE                       23940002
223200               AFTER ADVANCING 2 LINES                            23950002
223300     ELSE                                                         23960002
223400        WRITE PL-R FROM CREWS-OUT-TOWN-TITLE                      23970002
223500               AFTER ADVANCING 2 LINES                            23980002
223600     ADD 3 TO LINE-COUNT.                                         23990002
223700                                                                  24000002
223800 P2400-LOCALS-IN-TOWN.                                            24010002
223900                                                                  24020002
224000*                                                                 24030002
224100*       THE FOLLOWING LOGIC WAS CHANGED 2/13/97 TO ALSO PRINT     24040002
224200*       YARD ASSIGNMENTS AT IC'S REQUEST. ERW                     24050002
224300*                                                                 24060002
224400     MOVE ZEROES               TO LOCAL-COUNT                     24070002
224500     MOVE SPACES               TO SAVE-CREW-INFORMATION           24080002
224600                                  SAVE-TEMP-CREW-INFORMATION      24090002
224700     MOVE SPACES               TO AJJOBKEY-AREA                   24100002
224800     MOVE LINK-DIST            TO AJ-JOB-DIST                     24110002
224900     MOVE LINK-SUB-DIST        TO AJ-JOB-SUB-DIST                 24120002
225000     MOVE AJJOBKEY-AREA        TO AJ-FS-JOBKEY AJJOBKEY           24130002
225100     START AJ-FILE KEY > AJ-FS-JOBKEY                             24140002
225200           INVALID KEY CONTINUE                                   24150002
225300     END-START                                                    24160002
225400     IF SUCCESS                                                   24170002
225500        SET LOCALS-NOT-DONE TO TRUE                               24180002
225600        MOVE ZEROES         TO LOCAL-COUNT                        24190002
225700        PERFORM UNTIL LOCALS-DONE                                 24200002
225800          READ AJ-FILE NEXT RECORD INTO WS-ASGNED-JOBS            24210002
225900               AT END CONTINUE                                    24220002
226000          END-READ                                                24230002
226100          IF SUCCESS                                              24240002
226200             IF AJ-JOB-DIST          = LINK-DIST  AND             24250002
226300                AJ-JOB-SUB-DIST      = LINK-SUB-DIST              24260002
226400                SET DE-YYMMDD-FORMAT   TO TRUE                    24270002
226500                MOVE AJ-EFF-DATE       TO DE-YYMMDD               24280002
226600                PERFORM P8998-DATEEDIT                            24290002
226700                MOVE DE-CCYYMMDD       TO DE-COMPARE1-DATE        24300002
226800*               IF AJ-EFF-DATE        NOT  > SYSTEM-DATE          24310002
226900                IF DE-COMPARE1-DATE   NOT  > WS-SYSTEM-DATE-CENT  24320002
227000                   PERFORM P2405-CHECK-JOB-SCHEDULE               24330002
227100                   IF JOB-CURRENT                                 24340002
227200                      MOVE AJ-JOB-ASGN-ID TO JOB-DEF-CHECK        24350002
227300                      IF JOB-DEF-LOCAL-ASGN                       24360002
227400                         MOVE 'LOCAL:' TO LIT-ASGN-DESC           24370002
227500                      ELSE                                        24380002
227600                         MOVE 'YARD :' TO LIT-ASGN-DESC           24390002
227700                      END-IF                                      24400002
227800                      ADD 1 TO LOCAL-COUNT                        24410002
227900                      PERFORM P2410-SET-LOCAL-INFO THRU           24420002
228000                              P2410-SET-LOCAL-INFO-EXIT           24430002
228100                   END-IF                                         24440002
228200                END-IF                                            24450002
228300             ELSE                                                 24460002
228400                SET LOCALS-DONE         TO TRUE                   24470002
228500                PERFORM P2410-SET-LOCAL-INFO THRU                 24480002
228600                        P2410-SET-LOCAL-INFO-EXIT                 24490002
228700             END-IF                                               24500002
228800          ELSE                                                    24510002
228900             SET LOCALS-DONE         TO TRUE                      24520002
229000             PERFORM P2410-SET-LOCAL-INFO THRU                    24530002
229100                     P2410-SET-LOCAL-INFO-EXIT                    24540002
229200          END-IF                                                  24550002
229300        END-PERFORM                                               24560002
229400     END-IF.                                                      24570002
229500                                                                  24580002
229600 P2405-CHECK-JOB-SCHEDULE.                                        24590002
229700                                                                  24600002
229800*                                                                 24610002
229900*    LOCK IN ON THE "ACTIVE" SCHEDULE                             24620002
230000*                                                                 24630002
230100     MOVE SPACES                    TO WORK-JS-KEY1               24640002
230200                                       CURRENT-JOB-FLAG           24650002
230300     MOVE AJJOBKEY-AREA             TO JS-KEY1                    24660002
230400     MOVE WS-SYSTEM-DATE            TO JSK1-EXP-DATE              24670002
230500     SET JSK1-ASGN-STATS            TO TRUE                       24680002
230600     MOVE JS-KEY1                   TO JS-FS-KEY1                 24690002
230700     START JS-FILE                                                24700002
230800           KEY NOT LESS JS-FS-KEY1                                24710002
230900           INVALID KEY CONTINUE                                   24720002
231000     END-START                                                    24730002
231100     IF SUCCESS                                                   24740002
231200        READ JS-FILE                                              24750002
231300             NEXT RECORD                                          24760002
231400             INTO WS-JOB-SCHEDULE                                 24770002
231500             AT END CONTINUE                                      24780002
231600        END-READ                                                  24790002
231700     END-IF                                                       24800002
231800     IF SUCCESS                                                   24810002
231900        IF JSK1-ASGN-DIST = AJ-JOB-DIST                           24820002
232000         AND JSK1-ASGN-SUB-DIST = AJ-JOB-SUB-DIST                 24830002
232100         AND JSK1-ASGN = AJ-JOB-ASGN-ID                           24840002
232200         AND JSK1-ASGN-CC = AJ-JOB-ASGN-CC                        24850002
232300         AND JSK1-ASGN-STATS                                      24860002
232400           PERFORM P2407-CHECK-FOR-PROFILE                        24870002
232500        ELSE                                                      24880002
232600           SET NO-RECORD-FND        TO TRUE                       24890002
232700        END-IF                                                    24900002
232800     END-IF.                                                      24910002
232900                                                                  24920002
233000 P2407-CHECK-FOR-PROFILE.                                         24930002
233100                                                                  24940002
233200     MOVE SPACES                    TO WS-TRCN-FILE               24950002
233300                                       TRCNKEY3                   24960002
233400     IF AJ-TRAIN-DIST > SPACES                                    24970002
233500        MOVE AJ-TRAIN-DIST          TO TRCN-DIST3                 24980002
233600     ELSE                                                         24990002
233700        MOVE AJ-JOB-DIST            TO TRCN-DIST3                 25000002
233800     END-IF                                                       25010002
233900     IF AJ-TRAIN-SUB-DIST > SPACES                                25020002
234000        MOVE AJ-TRAIN-SUB-DIST      TO TRCN-SDIST3                25030002
234100     ELSE                                                         25040002
234200        MOVE AJ-JOB-SUB-DIST        TO TRCN-SDIST3                25050002
234300     END-IF                                                       25060002
234400     MOVE AJ-JOB-ASGN-ID            TO TRCN-ASSIGNMENT            25070002
234500     MOVE TRCN-KEY3                 TO TRCN-FS-KEY3 TRCNKEY3      25080002
234600     READ TRCN-FILE INTO WS-TRCN-FILE                             25090002
234700        KEY IS TRCN-FS-KEY3                                       25100002
234800        INVALID KEY CONTINUE                                      25110002
234900     END-READ                                                     25120002
235000     IF SUCCESS                                                   25130002
235100        IF TRCN-DIST = (AJ-TRAIN-DIST OR AJ-JOB-DIST)          AND25140002
235200           TRCN-SDIST = (AJ-TRAIN-SUB-DIST OR AJ-JOB-SUB-DIST) AND25150002
235300           TRCN-DESCRIPTION > SPACES                              25160002
235400           SET JOB-CURRENT           TO TRUE                      25170002
235500        END-IF                                                    25180002
235600     ELSE                                                         25190002
235700        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     25200002
235800           MOVE 'P2407'             TO ERR-PARAGRAPH              25210002
235900           MOVE TRCN-FS-KEY3        TO ERR-KEY                    25220002
236000           MOVE FILE-STATUS         TO ERR-FSTAT                  25230002
236100           MOVE 'TRCNKEY3'          TO ERR-FNAME                  25240002
236200           MOVE 'READ'              TO ERR-DESC                   25250002
236300           GO TO P9999-GOT-PROBLEM                                25260002
236400        END-IF                                                    25270002
236500     END-IF.                                                      25280002
236600                                                                  25290002
236700 P2410-SET-LOCAL-INFO.                                            25300002
236800                                                                  25310002
236900     IF LOCALS-DONE                                               25320002
237000        IF LOCAL-COUNT NOT > ZEROES                               25330002
237100           MOVE '<< NO LOCAL/YARD JOBS IN TOWN >>' TO CREW-MESSAGE25340002
237200           WRITE PL-R FROM CREWS-MESS-LINE AFTER                  25350002
237300                                        ADVANCING 2 LINES         25360002
237400           ADD 2                 TO LINE-COUNT                    25370002
237500           GO TO P2410-SET-LOCAL-INFO-EXIT                        25380002
237600         ELSE                                                     25390002
237700           PERFORM P2600-WRITE-LOCALS-IN-TOWN                     25400002
237800           GO TO P2410-SET-LOCAL-INFO-EXIT                        25410002
237900         END-IF                                                   25420002
238000     END-IF                                                       25430002
238100                                                                  25440002
238200     IF AJ-JOB-ASGN-ID = LOCAL-IN-TOWN                            25450002
238300        PERFORM P2500-GET-CREW-DETAIL                             25460002
238400     ELSE                                                         25470002
238500        IF LOCAL-IN-TOWN = SPACES                                 25480002
238600           PERFORM P2450-WRITE-NEW-IN-TOWN-LINE                   25490002
238700           PERFORM P2500-GET-CREW-DETAIL                          25500002
238800        ELSE                                                      25510002
238900           PERFORM P2600-WRITE-LOCALS-IN-TOWN                     25520002
239000           PERFORM P2450-WRITE-NEW-IN-TOWN-LINE                   25530002
239100           PERFORM P2500-GET-CREW-DETAIL                          25540002
239200        END-IF                                                    25550002
239300     END-IF.                                                      25560002
239400                                                                  25570002
239500 P2410-SET-LOCAL-INFO-EXIT.                                       25580002
239600     EXIT.                                                        25590002
239700                                                                  25600002
239800 P2450-WRITE-NEW-IN-TOWN-LINE.                                    25610002
239900                                                                  25620002
240000     MOVE SPACES              TO LOCAL-IN-TOWN                    25630002
240100                                 LOCAL-IN-TOWN-DESC               25640002
240200     MOVE AJ-JOB-ASGN-ID      TO LOCAL-IN-TOWN                    25650002
240300     PERFORM  P1800-GET-LOCAL-DESCS                               25660002
240400     MOVE WK-DESC             TO LOCAL-IN-TOWN-DESC               25670002
240500     IF LINE-COUNT > 55                                           25680046
240600       PERFORM P2300-POOL-TITLE                                   25690002
240700     END-IF                                                       25700002
240800     WRITE PL-R FROM LOCALS-IN-TOWN-1 AFTER ADVANCING 2 LINE      25710002
240900     ADD 2 TO LINE-COUNT.                                         25720002
241000                                                                  25730002
241100 P2500-GET-CREW-DETAIL.                                           25740002
241200                                                                  25750002
241300     PERFORM P7800-GET-AJ-EMPS                                    25760002
241400     MOVE SPACES                TO TEMP-EMP-FLAG                  25770002
241500     MOVE ZEROES                TO MSTRNBRK                       25780002
241600     IF OWNER-EMP-NBR > ZEROES                                    25790002
241700        MOVE OWNER-EMP-NBR      TO MSTRNBRK                       25800002
241800        PERFORM P8500-READ-MASTER                                 25810002
241900        PERFORM P2510-GET-CREW-DETAIL                             25820002
242000     ELSE                                                         25830002
242100        MOVE '<< OPEN TURN >>'  TO                                25840002
242200                       EMP-NAME OF WS-MSTR                        25850002
242300        PERFORM P2510-GET-CREW-DETAIL                             25860002
242400     END-IF                                                       25870002
242500     IF TEMP-EMP-ONE > ZEROES                                     25880002
242600        SET  TEMP-EMP-FOUND     TO TRUE                           25890002
242700        MOVE TEMP-EMP-ONE       TO MSTRNBRK                       25900002
242800        PERFORM P8500-READ-MASTER                                 25910002
242900        PERFORM P2510-GET-CREW-DETAIL                             25920002
243000     END-IF.                                                      25930002
243100                                                                  25940002
243200 P2510-GET-CREW-DETAIL.                                           25950002
243300                                                                  25960002
243400     MOVE SPACES                TO WS-RESTED                      25970002
243500     IF MSTRNBRK = ZEROES                                         25980002
243600        MOVE '<< VACANCY >>'    TO WS-RESTED                      25990002
243700     ELSE                                                         26000002
243800        IF AVAILABLE                                              26010002
243900           CONTINUE                                               26020002
244000        ELSE                                                      26030002
244100           IF WORKING                                             26040002
244200              MOVE '<< WORKING >>' TO WS-RESTED                   26050002
244300           ELSE                                                   26060002
244400*CNC0516-BEG                                                      26070034
244500              PERFORM P2515-GET-CREW-STATUS                       26080034
244600*             MOVE '<< VACANCY >>' TO WS-RESTED                   26090034
244700*CNC0516-END                                                      26100034
244800           END-IF                                                 26110002
244900        END-IF                                                    26120002
245000     END-IF                                                       26130002
245100                                                                  26140002
245200     IF WS-RESTED = SPACES                                        26150002
245300        MOVE WS-LOCAL-DATE-TIME-CENT  TO CHECK-REST-TIME-CENT     26160028
245400        MOVE ZEROS         TO DATE-CONVERSION-PARMS               26170002
245500        SET PARM-SUBTRACT  TO TRUE                                26180002
245600        MOVE CHECK-REST-DATE                                      26190002
245700                           TO PARM-PRI-DATE-GREG                  26200002
245800        MOVE CK-HR-MN      TO PARM-PRI-HRMN                       26210002
245900        MOVE '0400'        TO PARM-SEC-HRMN                       26220002
246000        CALL P803-PGM  USING DATE-CONVERSION-PARMS                26230002
246100        MOVE PARM-RES-DATE-GREG                                   26240002
246200                           TO CHECK-REST-DATE                     26250002
246300        MOVE PARM-RES-GREG-CENT                                   26260002
246400                           TO CHECK-REST-CE                       26270002
246500        MOVE PARM-RES-HRMN                                        26280002
246600                           TO CK-HR-MN                            26290002
246700        IF EMP-US-RSTD-NUM NOT > ZEROES                           26300028
246800           MOVE '000101010001'    TO EMP-US-RSTD                  26310028
246900        END-IF                                                    26320028
247000        SET  DE-YYMMDD-FORMAT     TO TRUE                         26330028
247100        MOVE EMP-US-RSTD-DATE     TO DE-YYMMDD                    26340002
247200        PERFORM P8998-DATEEDIT                                    26350002
247300        MOVE DE-CCYYMMDD          TO DE-COMPARE1-DATE             26360002
247400        MOVE EMP-US-RSTD-TIME     TO DE-COMPARE1-TIME             26370002
247500*                                                                 26380002
247600        MOVE ZEROS                TO DATE-CONVERSION-PARMS        26390002
247700        SET PARM-ADD              TO TRUE                         26400002
247800        IF EMP-PERS-REST-NUM NOT > ZEROES                         26410002
247900           MOVE '000101010001'    TO EMP-PERS-REST                26420002
248000        END-IF                                                    26430002
248100        PERFORM P5200-CHECK-COMPANY-CD                            26440005
248200        IF APPLY-LEAD-TIME                                        26450002
248300           MOVE EMP-PERS-REST-DATE   TO PARM-PRI-DATE-GREG        26460002
248400           MOVE EMP-PERS-REST-TIME   TO PARM-PRI-HRMN             26470002
248500           MOVE '0200'               TO PARM-SEC-HRMN             26480002
248600           CALL P803-PGM USING DATE-CONVERSION-PARMS              26490002
248700           MOVE PARM-RES-DATE-GREG   TO DE-COMPARE2-YYMMDD        26500002
248800           MOVE PARM-RES-GREG-CENT   TO DE-COMPARE2-CE            26510002
248900           MOVE PARM-RES-HRMN        TO DE-COMPARE2-TIME          26520002
249000        ELSE                                                      26530002
249100           SET  DE-YYMMDD-FORMAT     TO TRUE                      26540002
249200           MOVE EMP-PERS-REST-DATE   TO DE-YYMMDD                 26550002
249300           PERFORM P8998-DATEEDIT                                 26560002
249400           MOVE DE-CCYYMMDD          TO DE-COMPARE2-DATE          26570002
249500           MOVE EMP-PERS-REST-TIME   TO DE-COMPARE2-TIME          26580002
249600        END-IF                                                    26590002
249700*                                                                 26600002
249800        SET  DE-YYMMDD-FORMAT     TO TRUE                         26610002
249900        MOVE EMP-MTOD-DATE        TO DE-YYMMDD                    26620002
250000        PERFORM P8998-DATEEDIT                                    26630002
250100        MOVE DE-CCYYMMDD          TO DE-COMPARE3-DATE             26640002
250200        MOVE EMP-MTOD-TIME        TO DE-COMPARE3-TIME             26650002
250300        IF DE-COMPARE1-DATE-TIME > CHECK-REST-TIME-CENT           26660002
250400           MOVE EMP-US-RSTD-DATE TO WORK-DATE                     26670002
250500           MOVE EMP-US-RSTD-TIME TO WORK-HR-MN                    26680002
250600           MOVE WK-YR            TO FORM-YR                       26690002
250700           MOVE WK-DY            TO FORM-DY                       26700002
250800           MOVE WK-MO            TO FORM-MO                       26710002
250900           MOVE WORK-HR-MN       TO FORM-HRMN                     26720002
251000           MOVE FORMAT-DATE-AREA TO WS-RESTED                     26730002
251100        END-IF                                                    26740002
251200        IF DE-COMPARE2-DATE-TIME > CHECK-REST-TIME-CENT AND       26750002
251300           DE-COMPARE2-DATE-TIME > DE-COMPARE1-DATE-TIME          26760002
251400           MOVE DE-COMPARE2-YYMMDD   TO WORK-DATE                 26770002
251500           MOVE DE-COMPARE2-TIME     TO WORK-HR-MN                26780002
251600           MOVE WK-YR                TO FORM-YR                   26790002
251700           MOVE WK-DY                TO FORM-DY                   26800002
251800           MOVE WK-MO                TO FORM-MO                   26810002
251900           MOVE WORK-HR-MN           TO FORM-HRMN                 26820002
252000           MOVE FORMAT-DATE-AREA     TO WS-RESTED                 26830002
252100        END-IF                                                    26840002
252200        IF DE-COMPARE3-DATE-TIME > CHECK-REST-TIME-CENT AND       26850002
252300           DE-COMPARE3-DATE-TIME > DE-COMPARE2-DATE-TIME AND      26860002
252400           DE-COMPARE3-DATE-TIME > DE-COMPARE1-DATE-TIME          26870002
252500           MOVE EMP-MTOD-DATE    TO WORK-DATE                     26880002
252600           MOVE EMP-MTOD-TIME    TO WORK-HR-MN                    26890002
252700           MOVE WK-YR            TO FORM-YR                       26900002
252800           MOVE WK-DY            TO FORM-DY                       26910002
252900           MOVE WK-MO            TO FORM-MO                       26920002
253000           MOVE WORK-HR-MN       TO FORM-HRMN                     26930002
253100           MOVE FORMAT-DATE-AREA TO WS-RESTED                     26940002
253200        END-IF                                                    26950002
253300        IF WS-RESTED NOT > SPACE                                  26960002
253400           MOVE ' ** RESTED **'  TO WS-RESTED                     26970002
253500        END-IF                                                    26980002
253600     END-IF                                                       26990002
253700                                                                  27000002
253800     SET SEARCH-NOT-DONE              TO TRUE                     27010002
253900*                                                                 27020023
254000     PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CRAFT-TABLE-MAX   27030002
254100        OR SEARCH-DONE                                            27040002
254200        IF AJ-JOB-ASGN-CC = CT-CRAFT-CODE(J)                      27050002
254300           IF TEMP-EMP-FOUND                                      27060002
254400              SET SAVE-CREW-TEMP-EMP(J)   TO TRUE                 27070002
254500              MOVE EMP-NAME OF WS-MSTR    TO                      27080002
254600                                          SAVE-TEMP-CREW-NAME(J)  27090002
254700              MOVE EMP-NBR  OF WS-MSTR    TO PS94-EMP-NBR         27100009
254800              PERFORM P4000-GET-HOS                               27110009
254900              MOVE WS-TOT-TM              TO                      27120010
255000                                          SAVE-TEMP-CREW-TOTAL(J) 27130010
255100              MOVE WS-LIMBO-TM            TO                      27140010
255200                                          SAVE-TEMP-CREW-LIMBO(J) 27150010
255300              MOVE WS-CONSEC-STARTS       TO SAVE-TEMP-CREW-ST(J) 27160017
255400              MOVE WS-RESTED              TO                      27170002
255500                                          SAVE-TEMP-CREW-RESTED(J)27180002
255600              IF CT-CRAFT-CODE(J) = 'B1'                          27190002
255700                 MOVE 'REAR BRKM:'        TO                      27200002
255800                                          SAVE-TEMP-CREW-CRAFT(J) 27210002
255900              ELSE                                                27220002
256000                 IF CT-CRAFT-CODE(J) = 'B2'                       27230002
256100                    MOVE 'HEAD BRKM:'     TO                      27240002
256200                                          SAVE-TEMP-CREW-CRAFT(J) 27250002
256300                 ELSE                                             27260002
256400                    MOVE CT-CRAFT-DESC(J) TO                      27270002
256500                                          SAVE-TEMP-CREW-CRAFT(J) 27280002
256600                 END-IF                                           27290002
256700              END-IF                                              27300002
256800           ELSE                                                   27310002
256900              MOVE EMP-NAME OF WS-MSTR    TO SAVE-CREW-NAME(J)    27320002
257000              MOVE EMP-NBR  OF WS-MSTR    TO PS94-EMP-NBR         27330009
257100              PERFORM P4000-GET-HOS                               27340009
257200              MOVE WS-TOT-TM             TO SAVE-CREW-HOS-TOTAL(J)27350010
257300              MOVE WS-LIMBO-TM           TO SAVE-CREW-HOS-LIMBO(J)27360010
257400              MOVE WS-CONSEC-STARTS      TO SAVE-CREW-HOS-ST(J)   27370017
257500              IF TEMP-EMP-ONE > ZEROES                            27380002
257600                 IF NOT AVAILABLE                                 27390002
257700                    IF NOT WORKING                                27400002
257800                       MOVE ' *** OFF *** ' TO WS-RESTED          27410002
257900                    END-IF                                        27420002
258000                 END-IF                                           27430002
258100                 IF NORMAL-ASGNMT > SPACES AND                    27440002
258200                    NORMAL-ASGNMT NOT = AJJOBKEY-AREA             27450002
258300                       MOVE '<< ON TEMP >>' TO WS-RESTED          27460002
258400                 END-IF                                           27470002
258500                 IF TEMPORARY-ASGNMT > SPACES AND                 27480002
258600                    TEMPORARY-ASGNMT NOT = AJJOBKEY-AREA          27490002
258700                       MOVE '<< ON TEMP >>' TO WS-RESTED          27500002
258800                 END-IF                                           27510002
258900              ELSE                                                27520002
259000                 IF NORMAL-ASGNMT > SPACES AND                    27530002
259100                    NORMAL-ASGNMT NOT = AJJOBKEY-AREA             27540002
259200                       MOVE '<<TEMP VCNY>>' TO WS-RESTED          27550002
259300                 END-IF                                           27560002
259400                 IF TEMPORARY-ASGNMT > SPACES AND                 27570002
259500                    TEMPORARY-ASGNMT NOT = AJJOBKEY-AREA          27580002
259600                       MOVE '<<TEMP VCNY>>' TO WS-RESTED          27590002
259700                 END-IF                                           27600002
259800              END-IF                                              27610002
259900              MOVE WS-RESTED              TO SAVE-CREW-RESTED(J)  27620002
260000              IF CT-CRAFT-CODE(J) = 'B1'                          27630002
260100                 MOVE 'REAR BRKM:'        TO SAVE-CREW-CRAFT(J)   27640002
260200              ELSE                                                27650002
260300                 IF CT-CRAFT-CODE(J) = 'B2'                       27660002
260400                    MOVE 'HEAD BRKM:'     TO SAVE-CREW-CRAFT(J)   27670002
260500                 ELSE                                             27680002
260600                    MOVE CT-CRAFT-DESC(J) TO SAVE-CREW-CRAFT(J)   27690002
260700                 END-IF                                           27700002
260800              END-IF                                              27710002
260900           END-IF                                                 27720002
261000        END-IF                                                    27730002
261100     END-PERFORM.                                                 27740002
261200*CNC0516-BEG                                                      27750034
261300 P2515-GET-CREW-STATUS.                                           27760034
261400                                                                  27770034
261500     INITIALIZE WS-RESTED1                                        27780034
261600     EVALUATE TRUE                                                27790034
261700          WHEN OFF-MILES-DAYS                                     27800034
261800               PERFORM P9830-RETRIEVE-CNTL-INFO                   27810034
261900               IF P956-ST-RSN-NBR-DAYS-REQ                        27820034
262000               OR P956-ST-RSN-EXP-DATE-REQ                        27830034
262100                  PERFORM P3265-GET-DUEBACK-DATE                  27840034
262200                  IF WS-DUEBACK-FOUND-Y                           27850034
262300                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE227860034
262400                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME227870034
262410*CNC0573 - BEG                                                    27880060
262420                     IF WS-MASK-FLD-SCR-YES                       27890066
262440                        IF P956-MASK-FLD-SCR-YES                  27900060
262441*CNC0576 - BEG                                                    27910068
262451                        OR P956-MASK-HOLD-TURN                    27920068
262452                           MOVE '**'            TO WS-LAYOFF-CODE227930069
262453                           IF P956-MASK-HOLD-TURN                 27940068
262454                              MOVE 'HT'      TO WS-LAYOFF-EM-CODE227950069
262455                           ELSE                                   27960068
262456                              MOVE '**'      TO WS-LAYOFF-EM-CODE227970069
262457                           END-IF                                 27980068
262458*CNC0576 - END                                                    27990068
262460                        ELSE                                      28000060
262470                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE228010060
262480                           MOVE LAYOFF-EM-CODE  TO                28020060
262490                                WS-LAYOFF-EM-CODE1                28030060
262491                        END-IF                                    28040060
262492                     ELSE                                         28050060
262493                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE228060060
262494                        MOVE LAYOFF-EM-CODE     TO                28070060
262495                             WS-LAYOFF-EM-CODE1                   28080060
262496                     END-IF                                       28090060
262497*CNC0573 - END                                                    28100060
262800                     MOVE WS-RESTED1            TO WS-RESTED      28110034
262900                  ELSE                                            28120034
263000                     MOVE '<< VACANCY >>'       TO WS-RESTED      28130034
263100                  END-IF                                          28140034
263200               ELSE                                               28150039
263300                  PERFORM P3266-OFF-MILES-RETURN-DATE             28160034
263400                  MOVE WS-RETURN-DATE-1(1:4)    TO WS-RETURN-DATE228170051
263500                  MOVE SPACES                   TO WS-RETURN-TIME228180034
263510*CNC0573 - BEG                                                    28190060
263511                  IF WS-MASK-FLD-SCR-YES                          28200066
263513                     IF P956-MASK-FLD-SCR-YES                     28210060
263514*CNC0576 - BEG                                                    28220068
263515                     OR P956-MASK-HOLD-TURN                       28230068
263516                        MOVE '**'               TO WS-LAYOFF-CODE228240068
263517                        IF P956-MASK-HOLD-TURN                    28250068
263518                           MOVE 'HT'         TO WS-LAYOFF-EM-CODE228260068
263519                        ELSE                                      28270068
263520                           MOVE '**'         TO WS-LAYOFF-EM-CODE228280068
263521                        END-IF                                    28290068
263522*CNC0576 - END                                                    28300068
263524                     ELSE                                         28310060
263525                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE228320060
263526                        MOVE LAYOFF-EM-CODE     TO                28330060
263527                             WS-LAYOFF-EM-CODE2                   28340060
263528                     END-IF                                       28350060
263530                  ELSE                                            28360060
263600                     MOVE LAYOFF-CODE           TO WS-LAYOFF-CODE228370060
263700                     MOVE LAYOFF-EM-CODE        TO                28380060
263800                         WS-LAYOFF-EM-CODE2                       28390060
263801                  END-IF                                          28400060
263810*CNC0573 - END                                                    28410060
263900                  MOVE WS-RESTED1               TO WS-RESTED      28420034
264000               END-IF                                             28430034
264100          WHEN VACATION                                           28440034
264200               PERFORM P3267-VACATION-RETURN-DATE                 28450034
264300               MOVE WS-RETURN-DATE-1(1:4)       TO WS-RETURN-DATE228460051
264400               MOVE WS-RETURN-DATE-1(5:4)       TO WS-RETURN-TIME228470051
264410*CNC0573 - BEG                                                    28480060
264420               IF WS-MASK-FLD-SCR-YES                             28490066
264431                  PERFORM P9830-RETRIEVE-CNTL-INFO                28500060
264440                  IF P956-MASK-FLD-SCR-YES                        28510060
264441*CNC0576 - BEG                                                    28520068
264442                  OR P956-MASK-HOLD-TURN                          28530068
264443                     MOVE '**'                  TO WS-LAYOFF-CODE228540068
264444                     IF P956-MASK-HOLD-TURN                       28550068
264445                        MOVE 'HT'            TO WS-LAYOFF-EM-CODE228560068
264446                     ELSE                                         28570068
264447                        MOVE '**'            TO WS-LAYOFF-EM-CODE228580068
264448                     END-IF                                       28590068
264449*CNC0576 - END                                                    28600068
264460                  ELSE                                            28610060
264461                     MOVE LAYOFF-CODE           TO WS-LAYOFF-CODE228620060
264462                     MOVE LAYOFF-EM-CODE        TO                28630060
264463                          WS-LAYOFF-EM-CODE2                      28640060
264470                  END-IF                                          28650060
264480               ELSE                                               28660060
264500                  MOVE LAYOFF-CODE              TO WS-LAYOFF-CODE228670060
264600                  MOVE LAYOFF-EM-CODE           TO                28680060
264700                       WS-LAYOFF-EM-CODE2                         28690060
264701               END-IF                                             28700060
264710*CNC0573 - END                                                    28710060
264800               MOVE WS-RESTED1                  TO WS-RESTED      28720034
264900          WHEN EXCUSED-ABSENCE                                    28730034
265000           AND LAYOFF-EM-CODE = '69'                              28740034
265100               PERFORM P3265-GET-DUEBACK-DATE                     28750034
265200               IF WS-DUEBACK-FOUND-Y                              28760034
265300                  MOVE TASK-LO-EXP-DATE(3:4)    TO WS-RETURN-DATE228770034
265400                  MOVE TASK-LO-EXP-TIME         TO WS-RETURN-TIME228780034
265410*CNC0573 - BEG                                                    28790060
265420                  IF WS-MASK-FLD-SCR-YES                          28800066
265440                     PERFORM P9830-RETRIEVE-CNTL-INFO             28810060
265450                     IF P956-MASK-FLD-SCR-YES                     28820060
265451*CNC0576 - BEG                                                    28830068
265452                     OR P956-MASK-HOLD-TURN                       28840068
265453                        MOVE '**'               TO WS-LAYOFF-CODE228850068
265454                        IF P956-MASK-HOLD-TURN                    28860068
265455                           MOVE 'HT'         TO WS-LAYOFF-EM-CODE228870068
265456                        ELSE                                      28880068
265457                           MOVE '**'         TO WS-LAYOFF-EM-CODE228890068
265458                        END-IF                                    28900068
265459*CNC0576 - END                                                    28910068
265470                     ELSE                                         28920060
265471                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE228930060
265472                        MOVE LAYOFF-EM-CODE     TO                28940060
265473                             WS-LAYOFF-EM-CODE2                   28950060
265480                     END-IF                                       28960060
265490                  ELSE                                            28970060
265500                     MOVE LAYOFF-CODE           TO WS-LAYOFF-CODE228980060
265600                     MOVE LAYOFF-EM-CODE        TO                28990060
265700                          WS-LAYOFF-EM-CODE2                      29000060
265701                  END-IF                                          29010060
265710*CNC0573 - END                                                    29020060
265800                  MOVE WS-RESTED1               TO WS-RESTED      29030034
265900               ELSE                                               29040034
266000                  MOVE '<< VACANCY >>'          TO WS-RESTED      29050034
266100               END-IF                                             29060034
266200          WHEN OTHER                                              29070034
266300               IF NOT AVAILABLE  AND                              29080034
266400                  NOT WORKING    AND                              29090034
266500                  NOT TO-PLACE                                    29100034
266600                  AND LAYOFF-TIME NUMERIC                         29110034
266700                  AND LAYOFF-TIME > ZERO                          29120034
266800                  PERFORM P3265-GET-DUEBACK-DATE                  29130034
266900                  IF WS-DUEBACK-FOUND-Y                           29140034
267000                    PERFORM P3268-CHECK-FOR-E95-DTTM              29150056
267100                    IF WS-E95 > SPACES                            29160056
267110*CNC0573 - BEG                                                    29170060
267120                       IF WS-MASK-FLD-SCR-YES                     29180066
267140                          PERFORM P9830-RETRIEVE-CNTL-INFO        29190064
267150                          IF P956-MASK-FLD-SCR-YES                29200060
267151*CNC0576 - BEG                                                    29210068
267152                          OR P956-MASK-HOLD-TURN                  29220068
267153                             MOVE '**'          TO WS-E95-CODE    29230068
267154                             IF P956-MASK-HOLD-TURN               29240068
267155                                MOVE 'HT'       TO WS-E95-EM-CODE 29250068
267156                             ELSE                                 29260068
267157                                MOVE '**'       TO WS-E95-EM-CODE 29270068
267158                             END-IF                               29280068
267170                          END-IF                                  29290070
267193                       END-IF                                     29300060
267194                       MOVE WS-E95              TO WS-RESTED      29310070
267195*CNC0576 - END                                                    29320070
267210*CNC0573 - END                                                    29330060
267300                    ELSE                                          29340056
267400                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE229350056
267500                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME229360056
267510*CNC0573 - BEG                                                    29370060
267520                     IF WS-MASK-FLD-SCR-YES                       29380066
267540                        PERFORM P9830-RETRIEVE-CNTL-INFO          29390060
267550                        IF P956-MASK-FLD-SCR-YES                  29400060
267551*CNC0576 - BEG                                                    29410068
267552                        OR P956-MASK-HOLD-TURN                    29420068
267553                           MOVE '**'            TO WS-LAYOFF-CODE229430069
267554                           IF P956-MASK-HOLD-TURN                 29440068
267555                              MOVE 'HT'      TO WS-LAYOFF-EM-CODE229450069
267556                           ELSE                                   29460068
267557                              MOVE '**'      TO WS-LAYOFF-EM-CODE229470069
267558                           END-IF                                 29480068
267559*CNC0576 - END                                                    29490068
267570                        ELSE                                      29500060
267571                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE229510060
267572                           MOVE LAYOFF-EM-CODE  TO                29520060
267573                                WS-LAYOFF-EM-CODE2                29530060
267580                        END-IF                                    29540060
267590                     ELSE                                         29550060
267600                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE229560060
267700                        MOVE LAYOFF-EM-CODE     TO                29570060
267800                             WS-LAYOFF-EM-CODE2                   29580060
267801                     END-IF                                       29590060
267810*CNC0573 - END                                                    29600060
267900                     MOVE WS-RESTED1            TO WS-RESTED      29610056
268000                    END-IF                                        29620056
268100                  ELSE                                            29630034
268200                    MOVE '<< VACANCY >>'        TO WS-RESTED      29640056
268300                  END-IF                                          29650034
268400               END-IF                                             29660034
268500     END-EVALUATE.                                                29670039
268600*CNC0516-END                                                      29680034
268700 P2600-WRITE-LOCALS-IN-TOWN.                                      29690002
268800                                                                  29700002
268900     PERFORM VARYING J FROM 1 BY 1                                29710002
269000         UNTIL J > 30                                             29720002
269100            MOVE SPACES                   TO LOCALS-IN-TOWN-2     29730002
269200            IF SAVE-CREW-CRAFT(J) > SPACES                        29740002
269300               MOVE SAVE-CREW-CRAFT(J)    TO LOCALS-IT-CRAFT      29750002
269400               MOVE SAVE-CREW-CRAFT(J)    TO FOREMAN-OR-SWITCHMAN 29760002
269500               IF HE-IS-FOREMAN                                   29770002
269600                  MOVE 'CONDUCTOR'        TO LOCALS-IT-CRAFT      29780002
269700               END-IF                                             29790002
269800               IF HE-IS-SWITCHMAN                                 29800002
269900                  MOVE 'BRAKEMAN'         TO LOCALS-IT-CRAFT      29810002
270000               END-IF                                             29820002
270100               MOVE SAVE-CREW-NAME(J)     TO LOCALS-IT-NAME       29830002
270200*              MOVE SAVE-CREW-HOS(J)      TO LOCALS-IT-HOS        29840010
270300               IF WS-CANADIAN-COMPANY                             29850025
270400                  MOVE SPACES             TO LOCALS-IT-HOS-AREA   29860025
270500               ELSE                                               29870025
270600                  STRING 'T '                                     29880027
270700                    SAVE-CREW-HOS-TOTAL-HR(J) ':'                 29890010
270800                    SAVE-CREW-HOS-TOTAL-MM(J)                     29900010
270900                      ' L '                                       29910027
271000                    SAVE-CREW-HOS-LIMBO-HR(J) ':'                 29920023
271100                    SAVE-CREW-HOS-LIMBO-MM(J)                     29930010
271200                      ' S:'                                       29940027
271300                    SAVE-CREW-HOS-ST(J)                           29950010
271400                    DELIMITED BY SIZE INTO LOCALS-IT-HOS-AREA     29960010
271500               END-IF                                             29970025
271600*                                                                 29980023
271700               IF LOCALS-IT-NAME  = '<< OPEN TURN >> '            29990023
271800                  MOVE SPACES             TO LOCALS-IT-HOS-AREA   30000023
271900               END-IF                                             30010023
272000               MOVE SAVE-CREW-RESTED(J)   TO LOCALS-IT-RESTED     30020002
272100               WRITE PL-R FROM LOCALS-IN-TOWN-2 AFTER             30030002
272200                                          ADVANCING 1 LINES       30040002
272300               ADD    1                   TO LINE-COUNT           30050002
272400               IF SAVE-CREW-TEMP-EMP(J)                           30060002
272500                  MOVE '      TEMP'       TO LOCALS-IT-FILLER     30070002
272600                  MOVE SAVE-TEMP-CREW-NAME(J)                     30080002
272700                                          TO LOCALS-IT-NAME       30090002
272800*                 MOVE SAVE-TEMP-CREW-HOS(J)                      30100010
272900*                                         TO LOCALS-IT-HOS        30110010
273000                  IF WS-CANADIAN-COMPANY                          30120025
273100                     MOVE SPACES          TO LOCALS-IT-HOS-AREA   30130025
273200                  ELSE                                            30140025
273300                     STRING 'T '                                  30150027
273400                       SAVE-TEMP-CREW-TOTAL-HR(J) ':'             30160024
273500                       SAVE-TEMP-CREW-TOTAL-MM(J)                 30170024
273600                         ' L '                                    30180027
273700                       SAVE-TEMP-CREW-LIMBO-HR(J) ':'             30190024
273800                       SAVE-TEMP-CREW-LIMBO-MM(J)                 30200024
273900                         ' S:'                                    30210027
274000                       SAVE-TEMP-CREW-ST(J)                       30220024
274100                       DELIMITED BY SIZE INTO LOCALS-IT-HOS-AREA  30230010
274200                  END-IF                                          30240025
274300                  MOVE SAVE-TEMP-CREW-RESTED(J)                   30250002
274400                                          TO LOCALS-IT-RESTED     30260002
274500                  WRITE PL-R FROM LOCALS-IN-TOWN-2 AFTER          30270002
274600                                          ADVANCING 1 LINES       30280002
274700                  ADD    1                TO LINE-COUNT           30290002
274800               END-IF                                             30300002
274900            END-IF                                                30310002
275000     END-PERFORM                                                  30320002
275100                                                                  30330002
275200     MOVE SPACES              TO SAVE-CREW-INFORMATION            30340002
275300                                 SAVE-TEMP-CREW-INFORMATION.      30350002
275400                                                                  30360002
275500                                                                  30370002
275600 P3000-SPAREBOARDS.                                               30380002
275700                                                                  30390002
275800     MOVE 'P3000'            TO ERR-PARAGRAPH                     30400002
275900     IF LINE-COUNT > 55                                           30410046
276000       PERFORM P3800-XB-TITLE                                     30420002
276100     ELSE                                                         30430002
276200       WRITE PL-R FROM EXTRABOARDS-TITLE                          30440002
276300                             AFTER ADVANCING 2 LINES              30450002
276400       ADD 2                 TO LINE-COUNT                        30460002
276500     END-IF                                                       30470002
276600     SET NOT-DONE            TO TRUE                              30480002
276700     PERFORM VARYING EXT-SUB FROM 1 BY 1 UNTIL DONE               30490002
276800                             OR EXT-SUB > RPT-MAX                 30500002
276900        IF SB-P(EXT-SUB) = SPACES                                 30510002
277000           SET DONE             TO TRUE                           30520002
277100        ELSE                                                      30530002
277200           MOVE SPACES             TO WS-CNTL-FILE                30540002
277300           MOVE SPACE              TO WORK-CNTLKEY                30550002
277400           MOVE '08'               TO WK-CNTL-REC-TYPE            30560002
277500           MOVE LINK-DIST          TO WK-CNTL-DIST                30570002
277600           MOVE LINK-SUB-DIST      TO WK-CNTL-SUB-DIST            30580002
277700           MOVE SB-P(EXT-SUB)      TO WK-CNTL-XB                  30590002
277800           MOVE WORK-CNTLKEY       TO CNTL-FS-KEY                 30600002
277900           PERFORM P8000-READ-CNTLFILE                            30610002
278000           IF SUCCESS                                             30620002
278100              MOVE WS-CNTL-FILE  TO WS-HOLD-CNTL-FILE             30630002
278200              MOVE SPACES        TO WORK-XB-POS-KEY               30640002
278300              MOVE CNTL-DIST     TO XB-POS-DIST XB-TURN-DIST      30650002
278400                                    ASGN-DIST                     30660002
278500              MOVE CNTL-SUB-DIST TO XB-POS-SUB-DIST               30670002
278600                                    XB-TURN-SUB-DIST              30680002
278700                                    ASGN-SUB-DIST                 30690002
278800              MOVE CNTL-XB       TO XB-POS-CC XB-TURN-CC          30700002
278900                                    WS-CNTL-XB                    30710002
279000              MOVE CNTL-XB-DESC  TO EXTRABOARD-DESC               30720002
279100              IF CNTL-XB-EXTENDED-SCHED                           30730002
279200                 SET WS-XB-EXTENDED-SCHED  TO TRUE                30740002
279300              ELSE                                                30750002
279400                 IF CNTL-XB-SCHEDULED                             30760002
279500                    SET WS-XB-SCHEDULED    TO TRUE                30770002
279600                 ELSE                                             30780002
279700                    MOVE SPACES            TO WS-XB-SCHEDULED-FLAG30790002
279800                 END-IF                                           30800002
279900              END-IF                                              30810002
280000              IF LINE-COUNT > 55                                  30820046
280100                 PERFORM P3800-XB-TITLE                           30830002
280200              END-IF                                              30840002
280300              WRITE PL-R FROM EXTRABOARDS-1                       30850002
280400                                 AFTER ADVANCING 1 LINES          30860002
280500              ADD 1              TO LINE-COUNT                    30870002
280600              MOVE SPACES        TO P942-COMMAREA-PARMS           30880002
280700              SET P942-ASGN-SEN-FUNCTION                          30890002
280800                                 TO TRUE                          30900002
280900              SET P942-ASGN-XB   TO TRUE                          30910002
281000              MOVE CNTL-DIST     TO P942-ASGN-DIST                30920002
281100              MOVE CNTL-SUB-DIST TO P942-ASGN-SUB-DIST            30930002
281200              MOVE '******'      TO P942-ASGN-ASGN                30940002
281300              MOVE CNTL-XB-ROSTER-CC    TO P942-ASGN-CC           30950002
281400              PERFORM P8900-SEN-ANALYSIS                          30960002
281500              PERFORM VARYING X2 FROM 1 BY 1                      30970002
281600                UNTIL X2 > SA-ARRAY-MAX                           30980002
281700                  IF P942-ASGN-SEN-CC(X2) = CNTL-XB-ROSTER-CC     30990002
281800                     MOVE P942-ASGN-SEN-RSTR(X2)                  31000002
281900                                     TO WK-SEN-ROSTER             31010002
282000                     MOVE P942-ASGN-SEN-CC(X2)                    31020002
282100                                     TO WK-SEN-CC                 31030002
282200                     MOVE SA-ARRAY-MAX                            31040002
282300                                     TO X2                        31050002
282400                  END-IF                                          31060002
282500              END-PERFORM                                         31070002
282600              MOVE ZEROES            TO WS-POS-CNT                31080002
282700              MOVE ZERO              TO XB-POS                    31090002
282800* NEEDS TO BE RESTORED AFTER P8900 FOR DUEL-XB CHECK              31100029
282900              MOVE WS-HOLD-CNTL-FILE    TO WS-CNTL-FILE           31110029
283000              IF DUAL-XB                                          31120002
283100                 MOVE 1              TO XB-POS-BOARD              31130002
283200                 PERFORM P3100-GET-XB                             31140002
283300                 MOVE ZEROES         TO LINE-COUNT                31150002
283400                 MOVE ZEROES         TO WS-POS-CNT                31160002
283500                 MOVE ZEROES         TO XB-POS                    31170002
283600                 MOVE 2              TO XB-POS-BOARD              31180002
283700              END-IF                                              31190002
283800              PERFORM P3100-GET-XB                                31200002
283900           ELSE                                                   31210002
284000              IF NOT (END-OF-FILE OR NO-RECORD-FND)               31220002
284100                 MOVE CNTL-FS-KEY           TO ERR-KEY            31230002
284200                 MOVE FILE-STATUS           TO ERR-FSTAT          31240002
284300                 MOVE 'CNTLFILE'            TO ERR-FNAME          31250002
284400                 MOVE 'FAIL READ ON CNTLFILE' TO ERR-DESC         31260002
284500                 GO TO P9999-GOT-PROBLEM                          31270002
284600              END-IF                                              31280002
284700           END-IF                                                 31290002
284800        END-IF                                                    31300002
284900     END-PERFORM.                                                 31310002
285000                                                                  31320002
285100 P3100-GET-XB.                                                    31330002
285200                                                                  31340002
285300*    MOVE WS-HOLD-CNTL-FILE    TO WS-CNTL-FILE                    31350029
285400                                                                  31360002
285500     SET XB-NOT-DONE TO TRUE                                      31370002
285600     PERFORM P3900-START-XB-POS                                   31380002
285700     IF SUCCESS                                                   31390002
285800       PERFORM UNTIL XB-DONE                                      31400002
285900         PERFORM P3920-READ-NEXT-XB-POS                           31410002
286000         IF SUCCESS                                               31420002
286100           SET DE-YYMMDD-FORMAT TO TRUE                           31430002
286200           MOVE EB-POS-DATE-TIME(1:6) TO DE-YYMMDD                31440002
286300           PERFORM P8998-DATEEDIT                                 31450002
286400           MOVE DE-CCYYMMDD     TO DE-COMPARE1-DATE               31460002
286500           MOVE EB-POS-DATE-TIME(7:4) TO DE-COMPARE1-TIME         31470002
286600           IF XB-POS-DIST = DIST OF WS-EXTRA-BOARD                31480002
286700             AND XB-POS-SUB-DIST = SUB-DIST OF WS-EXTRA-BOARD     31490002
286800             AND XB-POS-CC = CRAFT-CODE OF WS-EXTRA-BOARD         31500029
286900             AND EB-ON-BOARD                                      31510029
287000             AND XB-POS-BOARD = EB-POS-BOARD OF WS-EXTRA-BOARD    31520029
287100             AND DE-COMPARE1-DATE-TIME NOT >                      31530029
287200                                  WS-PRESENT-TIME-CENT            31540029
287300               PERFORM P3200-GET-XB-DETAIL                        31550029
287400           ELSE                                                   31560002
287500               SET XB-DONE TO TRUE                                31570002
287600           END-IF                                                 31580002
287700         ELSE                                                     31590002
287800           SET XB-DONE TO TRUE                                    31600002
287900         END-IF                                                   31610002
288000       END-PERFORM                                                31620002
288100     END-IF.                                                      31630002
288200*                                                                 31640002
288300*    SET XB-NOT-DONE TO TRUE                                      31650002
288400*    MOVE SPACES     TO XB-TURN                                   31660002
288500*    PERFORM P3910-START-XB-TURN                                  31670002
288600*    IF SUCCESS                                                   31680002
288700*      PERFORM UNTIL XB-DONE                                      31690002
288800*        PERFORM P3930-READ-NEXT-XB-TURN                          31700002
288900*        IF SUCCESS                                               31710002
289000*          IF XB-TURN-DIST  = DIST OF WS-EXTRA-BOARD              31720002
289100*            AND XB-TURN-SUB-DIST = SUB-DIST OF WS-EXTRA-BOARD    31730002
289200*            AND XB-TURN-CC = CRAFT-CODE OF WS-EXTRA-BOARD        31740002
289300*            IF (EB-OFF-BOARD AND                                 31750002
289400*               XB-POS-BOARD = EB-POS-BOARD OF WS-EXTRA-BOARD)    31760002
289500*            OR (EB-ON-BOARD AND                                  31770002
289600*               EB-POS-DATE-TIME > PRESENT-TIME AND               31780002
289700*               XB-POS-BOARD = EB-POS-BOARD OF WS-EXTRA-BOARD)    31790002
289800*              PERFORM P3200-GET-XB-DETAIL                        31800002
289900*            END-IF                                               31810002
290000*          ELSE                                                   31820002
290100*            SET XB-DONE TO TRUE                                  31830002
290200*          END-IF                                                 31840002
290300*        ELSE                                                     31850002
290400*          SET XB-DONE TO TRUE                                    31860002
290500*        END-IF                                                   31870002
290600*      END-PERFORM                                                31880002
290700*    END-IF.                                                      31890002
290800                                                                  31900002
290900 P3200-GET-XB-DETAIL.                                             31910002
291000                                                                  31920002
291100     IF EB-ON-BOARD                                               31930002
291200       ADD 1 TO WS-POS-CNT                                        31940002
291300       MOVE WS-POS-CNT TO EXTRABOARD-POSITION                     31950002
291400     ELSE                                                         31960002
291500       MOVE '999' TO EXTRABOARD-POSITION                          31970002
291600     END-IF                                                       31980002
291700     MOVE ZERO     TO ASGN-EMP-NO                                 31990002
291800                      GOT-EMPLOYEE-FLAG                           32000002
291900     MOVE SPACES   TO WS-MSTR                                     32010002
292000     MOVE TURN-NBR OF WS-EXTRA-BOARD TO ASGN-XB-TURN              32020002
292100     MOVE 'EX'          TO ASGN-XB-PREFIX                         32030002
292200     MOVE 'X'           TO ASGN-JOB-TYPE                          32040002
292300     MOVE WS-CNTL-XB    TO ASGN-XB-CC                             32050002
292400     SET ASGN-TEMP-REC  TO TRUE                                   32060002
292500     MOVE ZEROES        TO ASGN-DATE-TIME                         32070002
292600     MOVE LINK-DIST     TO ASGN-DIST                              32080002
292700     MOVE LINK-SUB-DIST TO ASGN-SUB-DIST                          32090002
292800     PERFORM PXXXX-LATEST-TEMP                                    32100002
292900     IF ASGN-EMP-NO NOT > ZERO                                    32110002
293000        MOVE TURN-NBR OF WS-EXTRA-BOARD TO ASGN-XB-TURN           32120002
293100        MOVE 'EX'          TO ASGN-XB-PREFIX                      32130002
293200        MOVE 'X'           TO ASGN-JOB-TYPE                       32140002
293300        MOVE WS-CNTL-XB    TO ASGN-XB-CC                          32150002
293400        SET ASGN-OWNER-REC TO TRUE                                32160002
293500        MOVE ZEROES        TO ASGN-DATE-TIME                      32170002
293600        MOVE LINK-DIST     TO ASGN-DIST                           32180002
293700        MOVE LINK-SUB-DIST TO ASGN-SUB-DIST                       32190002
293800        PERFORM PXXXX-JOB-OWNER                                   32200002
293900     END-IF                                                       32210002
294000     IF ASGN-EMP-NO NOT > ZERO                                    32220002
294100        PERFORM P3300-SEARCH-SWASSGN                              32230002
294200     END-IF                                                       32240002
294300     IF ASGN-EMP-NO > ZERO                                        32250002
294400        MOVE ASGN-EMP-NO TO MSTRNBRK                              32260002
294500                            EMP-NBR-KEY                           32270002
294600                            MSTR-FS-NBRK                          32280002
294700        PERFORM P8500-READ-MASTER                                 32290002
294800        SET GOT-EMPLOYEE TO TRUE                                  32300002
294900*                                                                 32310002
295000*       IF EMPLOYEE IS UNAVAILABLE, ONLY DISPLAY THEM IF THEIR    32320002
295100*       STATUS CODE'S 'DISPLAY WITHOUT TRACKING' FLAG IS SET.     32330002
295200*                                                                 32340002
295300        IF AVAILABLE                                              32350002
295400           SET DISPLAY-EMP                 TO TRUE                32360002
295500        ELSE                                                      32370002
295600           SET DONT-DISPLAY-EMP            TO TRUE                32380002
295700           MOVE SPACES                     TO P956-COMMAREA-PARMS 32390002
295800           MOVE LAYOFF-CODE-1 OF WS-MSTR   TO P956-STATUS-CODE    32400002
295900           SET P956-GET-CNTL-STATUS-REASON TO TRUE                32410002
296000           MOVE LAYOFF-EM-CODE OF WS-MSTR  TO P956-REASON-CODE    32420002
296100           MOVE DIST  OF WS-MSTR           TO P956-DIST           32430002
296200           MOVE SUB-DIST OF WS-MSTR        TO P956-SDIST          32440002
296300           MOVE CRAFT OF WS-MSTR           TO P956-CC             32450002
296400           IF TEMPORARY-ASGNMT > SPACE                            32460002
296500              MOVE TEMPORARY-ASGNMT-FLAG   TO P956-ASGN-TYPE      32470002
296600              MOVE TA-1                    TO P956-ASGN           32480002
296700              MOVE TA-DIST                 TO P956-DIST           32490002
296800              MOVE TA-SUB-DIST             TO P956-SDIST          32500002
296900              IF TEMP-ASGN-XB                                     32510002
297000                 MOVE TA-CC                TO P956-XB             32520002
297100              END-IF                                              32530002
297200           ELSE                                                   32540002
297300              IF NORMAL-ASGNMT > SPACES                           32550002
297400                 MOVE NORMAL-ASGNMT-FLAG   TO P956-ASGN-TYPE      32560002
297500                 MOVE NA-1                 TO P956-ASGN           32570002
297600                 MOVE NA-DIST              TO P956-DIST           32580002
297700                 MOVE NA-SUB-DIST          TO P956-SDIST          32590002
297800                 IF NORM-ASGN-XB                                  32600002
297900                    MOVE NA-CC             TO P956-XB             32610002
298000                 END-IF                                           32620002
298100              END-IF                                              32630002
298200           END-IF                                                 32640002
298300           CALL P856-PGM USING P956-COMMAREA-PARMS                32650002
298400           IF P956-ST-RSN-DISP-WO-TRACKING                        32660002
298500              SET DISPLAY-EMP              TO TRUE                32670002
298600           END-IF                                                 32680002
298700        END-IF                                                    32690002
298800     END-IF                                                       32700002
298900     IF GOT-EMPLOYEE                                              32710002
299000        IF DISPLAY-EMP                                            32720002
299100           IF TEMPORARY-ASGNMT > SPACE                            32730002
299200               AND TEMP-ASGN-XB-AUG                               32740002
299300               AND TA-DIST = DIST-REPEAT                          32750002
299400               AND TA-SUB-DIST = SUBDIST-REPEAT                   32760002
299500               AND TA-XB-TURN = TURN-NBR OF WS-EXTRA-BOARD        32770002
299600               AND TA-CC = CRAFT-CODE-REPEAT                      32780002
299700               MOVE EMP-NAME OF WS-MSTR TO WS-FORMAT-NAME-AUG     32790002
299800               MOVE ' /AUG'          TO WS-FORMAT-NAME-AUG-FIELD  32800002
299900               MOVE WS-FORMAT-NAME-AUG TO EXTRABOARD-NAME         32810002
300000           ELSE                                                   32820002
300100               MOVE EMP-NAME OF WS-MSTR TO EXTRABOARD-NAME        32830002
300200               IF WS-CANADIAN-COMPANY                             32840025
300300                  MOVE SPACES           TO EXTRABOARD-HOS-AREA    32850025
300400               ELSE                                               32860025
300500                  MOVE EMP-NBR  OF WS-MSTR TO PS94-EMP-NBR        32870025
300600                  PERFORM P4000-GET-HOS                           32880025
300700                  STRING 'T '                                     32890027
300800                    WS-TOT-TM-HH ':'                              32900010
300900                    WS-TOT-TM-MM                                  32910010
301000                      ' L '                                       32920027
301100                    WS-LIMBO-TM-HH ':'                            32930010
301200                    WS-LIMBO-TM-MM                                32940010
301300                      ' S:'                                       32950027
301400                    WS-CONSEC-STARTS                              32960017
301500                    DELIMITED BY SIZE INTO EXTRABOARD-HOS-AREA    32970010
301600               END-IF                                             32980025
301700           END-IF                                                 32990002
301800        END-IF                                                    33000002
301900     ELSE                                                         33010002
302000        MOVE '<<  OPEN TURN  >>'     TO EXTRABOARD-NAME           33020002
302100        MOVE SPACES                  TO EXTRABOARD-HOS-AREA       33030023
302200     END-IF                                                       33040002
302300                                                                  33050029
302400* IF XB TEMP-ASGN AND EMP IS ON THE BOARD WE ARE PROCESSING,      33060029
302500* HE COULD STILL BE CONSIDERED AVAILABLE.                         33070029
302600     SET TEMP-DIFF-BOARD TO TRUE                                  33080029
302700     IF TEMPORARY-ASGNMT > SPACES                                 33090029
302800        IF TEMP-ASGN-XB                                           33100029
302900           IF TA-DIST     = DIST       OF WS-EXTRA-BOARD AND      33110029
303000              TA-SUB-DIST = SUB-DIST   OF WS-EXTRA-BOARD AND      33120029
303100              TA-CC       = CRAFT-CODE OF WS-EXTRA-BOARD          33130029
303200              SET TEMP-SAME-BOARD TO TRUE                         33140029
303300           END-IF                                                 33150029
303400       END-IF                                                     33160029
303500     END-IF                                                       33170029
303600                                                                  33180029
303700     IF GOT-EMPLOYEE                                              33190002
303800        AND DISPLAY-EMP                                           33200002
303900        IF (TEMPORARY-ASGNMT > SPACES                             33210029
304000            AND TEMP-DIFF-BOARD)        OR                        33220029
304100           NOT (AVAILABLE)              OR                        33230002
304200           (AVAILABLE AND NOT-NOTIFIED) OR                        33240002
304300           OUT-TOWN                                               33250002
304400*CNC0516-BEG                                                      33260035
304500*          MOVE '<UNAVAILABLE>'           TO EXTRABOARD-RESTED    33270035
304600           PERFORM P3260-GET-STATUS                               33280035
304700*CNC0516-END                                                      33290035
304800        END-IF                                                    33300002
304900     END-IF                                                       33310002
305000*                                                                 33320002
305100*    IF THIS IS A SCHEDULED EXTRABOARD (EXTENDED OR REGULAR), SEE 33330002
305200*    IF THE TURN IS ON A REST DAY.                                33340002
305300*                                                                 33350002
305400     IF EXTRABOARD-RESTED NOT > SPACES                            33360002
305500        AND WS-SCHEDULED-XB                                       33370002
305600        SET NOT-ON-REST-DAY       TO TRUE                         33380002
305700        MOVE SPACES               TO WS-SCHEDULED-TURN-SW         33390002
305800        IF WS-XB-SCHEDULED                                        33400002
305900           PERFORM P3210-CHECK-REST-DAY                           33410002
306000        ELSE                                                      33420002
306100           PERFORM P3250-CHECK-EXTENDED-REST-DAY                  33430002
306200        END-IF                                                    33440002
306300        IF ON-REST-DAY                                            33450002
306400           MOVE '<UNAVAILABLE>'           TO EXTRABOARD-RESTED    33460002
306500        END-IF                                                    33470002
306600     END-IF                                                       33480002
306700     IF GOT-EMPLOYEE                                              33490002
306800        AND DISPLAY-EMP                                           33500002
306900        IF EMP-MTOD IS NUMERIC                                    33510002
307000           SET DE-YYMMDD-FORMAT   TO TRUE                         33520002
307100           MOVE EMP-MTOD-DATE     TO DE-YYMMDD                    33530002
307200           PERFORM P8998-DATEEDIT                                 33540002
307300           MOVE DE-CCYYMMDD       TO DE-COMPARE1-DATE             33550002
307400           MOVE EMP-MTOD-TIME     TO DE-COMPARE1-TIME             33560002
307500        END-IF                                                    33570002
307600        IF EMP-US-RSTD IS NUMERIC                                 33580002
307700           SET DE-YYMMDD-FORMAT   TO TRUE                         33590002
307800           MOVE EMP-US-RSTD-DATE  TO DE-YYMMDD                    33600002
307900           PERFORM P8998-DATEEDIT                                 33610002
308000           MOVE DE-CCYYMMDD       TO DE-COMPARE2-DATE             33620002
308100           MOVE EMP-US-RSTD-TIME  TO DE-COMPARE2-TIME             33630002
308200        END-IF                                                    33640002
308300        IF EMP-PERS-REST IS NUMERIC                               33650002
308400*                                                                 33660002
308500           IF EMP-PERS-REST-NUM NOT > ZEROES                      33670002
308600              MOVE '000101010001' TO EMP-PERS-REST                33680002
308700           END-IF                                                 33690002
308800           PERFORM P5200-CHECK-COMPANY-CD                         33700005
308900           IF APPLY-LEAD-TIME                                     33710002
309000              MOVE ZEROS             TO DATE-CONVERSION-PARMS     33720002
309100              SET PARM-ADD           TO TRUE                      33730002
309200              MOVE EMP-PERS-REST-DATE TO PARM-PRI-DATE-GREG       33740002
309300              MOVE EMP-PERS-REST-TIME TO PARM-PRI-HRMN            33750002
309400              MOVE '0200'            TO PARM-SEC-HRMN             33760002
309500              CALL P803-PGM USING DATE-CONVERSION-PARMS           33770002
309600              MOVE PARM-RES-DATE-GREG TO DE-COMPARE3-YYMMDD       33780002
309700              MOVE PARM-RES-GREG-CENT TO DE-COMPARE3-CE           33790002
309800              MOVE PARM-RES-HRMN     TO DE-COMPARE3-TIME          33800002
309900           ELSE                                                   33810002
310000              SET DE-YYMMDD-FORMAT    TO TRUE                     33820002
310100              MOVE EMP-PERS-REST-DATE TO DE-YYMMDD                33830002
310200              PERFORM P8998-DATEEDIT                              33840002
310300              MOVE DE-CCYYMMDD        TO DE-COMPARE3-DATE         33850002
310400              MOVE EMP-PERS-REST-TIME TO DE-COMPARE3-TIME         33860002
310500           END-IF                                                 33870002
310600*                                                                 33880002
310700        END-IF                                                    33890002
310800     END-IF                                                       33900002
310900     IF GOT-EMPLOYEE                                              33910002
311000        AND DISPLAY-EMP                                           33920002
311100        AND EXTRABOARD-RESTED  NOT > SPACES                       33930002
311200        IF EMP-MTOD IS NUMERIC                                    33940002
311310*C975-BEG                                                         33950058
311320*          AND DE-COMPARE1-DATE-TIME > WS-PRESENT-TIME-CENT       33960058
311330           AND DE-COMPARE1-DATE-TIME > WS-LOCAL-DATE-TIME-CENT    33970058
311340*C975-END                                                         33980058
311400           MOVE EMP-MTOD-NUM     TO WORK-TIME                     33990002
311500           MOVE WK-YR            TO FORM-YR                       34000002
311600           MOVE WK-DY            TO FORM-DY                       34010002
311700           MOVE WK-MO            TO FORM-MO                       34020002
311800           MOVE WORK-HR-MN       TO FORM-HRMN                     34030002
311900           MOVE FORMAT-DATE-AREA TO EXTRABOARD-RESTED             34040002
312000        END-IF                                                    34050002
312100        IF EMP-US-RSTD IS NUMERIC                                 34060002
312110*C975-BEG                                                         34070058
312200*          AND DE-COMPARE2-DATE-TIME > WS-PRESENT-TIME-CENT       34080058
312210           AND DE-COMPARE2-DATE-TIME > WS-LOCAL-DATE-TIME-CENT    34090058
312220*C975-END                                                         34100058
312300           MOVE EMP-US-RSTD      TO WORK-TIME                     34110002
312400           MOVE WK-YR            TO FORM-YR                       34120002
312500           MOVE WK-DY            TO FORM-DY                       34130002
312600           MOVE WK-MO            TO FORM-MO                       34140002
312700           MOVE WORK-HR-MN       TO FORM-HRMN                     34150002
312800           MOVE FORMAT-DATE-AREA TO EXTRABOARD-RESTED             34160002
312900        END-IF                                                    34170002
313000        IF EMP-PERS-REST IS NUMERIC                               34180002
313110*C975-BEG                                                         34190058
313120*          AND DE-COMPARE3-DATE-TIME > WS-PRESENT-TIME-CENT       34200058
313130           AND DE-COMPARE3-DATE-TIME > WS-LOCAL-DATE-TIME-CENT    34210058
313140*C975-END                                                         34220058
313200           MOVE DE-COMPARE3-YYMMDD TO WORK-DATE                   34230002
313300           MOVE DE-COMPARE3-TIME TO WORK-HR-MN                    34240002
313400           MOVE WK-YR            TO FORM-YR                       34250002
313500           MOVE WK-DY            TO FORM-DY                       34260002
313600           MOVE WK-MO            TO FORM-MO                       34270002
313700           MOVE WORK-HR-MN       TO FORM-HRMN                     34280002
313800           MOVE FORMAT-DATE-AREA TO EXTRABOARD-RESTED             34290002
313900        END-IF                                                    34300002
314000     END-IF                                                       34310002
314100*                                                                 34320002
314200*    IF DONT-DISPLAY-EMP, BACK OUT THE WS-POS-CNT CHANGES WE MADE 34330002
314300*    EARLIER.                                                     34340002
314400*                                                                 34350002
314500     IF GOT-EMPLOYEE                                              34360002
314600        AND DONT-DISPLAY-EMP                                      34370002
314700        IF EB-ON-BOARD                                            34380002
314800           AND WS-POS-CNT > 0                                     34390002
314900           SUBTRACT 1            FROM WS-POS-CNT                  34400002
315000        END-IF                                                    34410002
315100     ELSE                                                         34420002
315200        WRITE PL-R FROM EXTRABOARDS-2 AFTER                       34430002
315300                        ADVANCING 1 LINES                         34440002
315400        ADD 1                    TO LINE-COUNT                    34450002
315500     END-IF                                                       34460002
315600     MOVE SPACES                 TO EXTRABOARDS-2                 34470002
315700     .                                                            34480002
315800*                                                                 34490002
315900 P3210-CHECK-REST-DAY.                                            34500002
316000*                                                                 34510002
316100     MOVE SPACES                        TO WS-JOB-SCHED-REST-DAYS 34520002
316200     MOVE SPACES                        TO WORK-JS-KEY1           34530002
316300     MOVE DIST       IN WS-EXTRA-BOARD  TO WK-JSK1-ASGN-DIST      34540002
316400     MOVE SUB-DIST   IN WS-EXTRA-BOARD  TO WK-JSK1-ASGN-SUB-DIST  34550002
316500     STRING 'EX' TURN-NBR OF WS-EXTRA-BOARD                       34560002
316600            DELIMITED BY SIZE                                     34570002
316700            INTO WK-JSK1-ASGN                                     34580002
316800     MOVE CRAFT-CODE IN WS-EXTRA-BOARD  TO WK-JSK1-ASGN-CC        34590002
316900     MOVE 999999                        TO WK-JSK1-EXP-DATE       34600002
317000     MOVE '000'                         TO WK-JSK1-ASGN-DAY       34610002
317100     MOVE WORK-JS-KEY1                  TO JS-FS-KEY1             34620002
317200     PERFORM P8240-READ-JS                                        34630002
317300     IF SUCCESS                                                   34640002
317400        AND JSK1-ASGN-DIST               = WK-JSK1-ASGN-DIST      34650002
317500        AND JSK1-ASGN-SUB-DIST           = WK-JSK1-ASGN-SUB-DIST  34660002
317600        AND JSK1-ASSIGNMENT              = WK-JSK1-ASSIGNMENT     34670002
317700        AND JSK1-EXP-DATE                = WK-JSK1-EXP-DATE       34680002
317800        AND JSK1-ASGN-DAY                = WK-JSK1-ASGN-DAY-NUM   34690002
317900        MOVE JOB-SCHED-REST-DAYS        TO WS-JOB-SCHED-REST-DAYS 34700002
318000     END-IF                                                       34710002
318100                                                                  34720002
318200     MOVE SPACES                        TO WORK-JS-KEY1           34730002
318300     MOVE DIST       OF WS-EXTRA-BOARD  TO WK-JSK1-ASGN-DIST      34740002
318400     MOVE SUB-DIST   OF WS-EXTRA-BOARD  TO WK-JSK1-ASGN-SUB-DIST  34750002
318500     STRING 'EX' TURN-NBR OF WS-EXTRA-BOARD                       34760002
318600            DELIMITED BY SIZE                                     34770002
318700            INTO WK-JSK1-ASGN                                     34780002
318800     MOVE CRAFT-CODE OF WS-EXTRA-BOARD  TO WK-JSK1-ASGN-CC        34790002
318900     MOVE 999999                        TO WK-JSK1-EXP-DATE       34800002
319000     MOVE WEEK-DAY                      TO WK-JSK1-ASGN-DAY-NUM   34810002
319100     MOVE WORK-JS-KEY1                  TO JS-FS-KEY1             34820002
319200     SET FIRST-JS                       TO TRUE                   34830002
319300     PERFORM P8200-STARTBR-JS                                     34840002
319400     IF SUCCESS                                                   34850002
319500        PERFORM P8220-READNEXT-JS                                 34860002
319600     END-IF                                                       34870002
319700     PERFORM UNTIL JS-DONE                                        34880002
319800        IF SUCCESS                                                34890002
319900           AND JSK1-ASGN-DIST            = WK-JSK1-ASGN-DIST      34900002
320000           AND JSK1-ASGN-SUB-DIST        = WK-JSK1-ASGN-SUB-DIST  34910002
320100           AND JSK1-ASSIGNMENT           = WK-JSK1-ASSIGNMENT     34920002
320200           AND JSK1-EXP-DATE             = WK-JSK1-EXP-DATE       34930002
320300           AND JSK1-ASGN-DAY             = WK-JSK1-ASGN-DAY-NUM   34940002
320400           PERFORM P3220-CHECK-REST-PERIOD                        34950002
320500        ELSE                                                      34960002
320600           SET WS-SCHEDULED-REST-PERIOD TO TRUE                   34970002
320700           SET JS-DONE                  TO TRUE                   34980002
320800        END-IF                                                    34990002
320900        IF FIRST-JS                                               35000002
321000           SET MORE-JS                  TO TRUE                   35010002
321100        END-IF                                                    35020002
321200        IF MORE-JS                                                35030002
321300           PERFORM P8220-READNEXT-JS                              35040002
321400        END-IF                                                    35050002
321500     END-PERFORM                                                  35060002
321600     IF WS-SCHEDULED-REST-PERIOD                                  35070002
321700        OR WS-SCHEDULED-REST-DAY                                  35080002
321800        SET ON-REST-DAY                 TO TRUE                   35090002
321900     END-IF                                                       35100002
322000     .                                                            35110002
322100                                                                  35120002
322200*************************************************************     35130002
322300 P3220-CHECK-REST-PERIOD.                                         35140002
322400*   THIS ROUTINE DETERMINES IF THE EMPLOYEE IS ON A REST          35150002
322500*   PERIOD, BY EVALUATING THE TIMES HE IS SCHEDULED TO            35160002
322600*   WORK.                                                         35170002
322700*************************************************************     35180002
322800      EVALUATE TRUE                                               35190002
322900         WHEN JSK1-ASGN-START-TIME > WS-LOCAL-TIME                35200002
323000            IF FIRST-JS                                           35210002
323100               IF WS-JOB-ON-REST-DAY(WEEK-DAY)                    35220002
323200                  SET WS-SCHEDULED-REST-PERIOD   TO TRUE          35230002
323300               ELSE                                               35240002
323400                  PERFORM P3230-CHECK-YESTERDAY                   35250002
323500               END-IF                                             35260002
323600            ELSE                                                  35270002
323700               SET WS-SCHEDULED-REST-PERIOD      TO TRUE          35280002
323800            END-IF                                                35290002
323900            SET JS-DONE                          TO TRUE          35300002
324000         WHEN JSK1-ASGN-START-TIME <= WS-LOCAL-TIME               35310002
324100            IF WS-JOB-ON-REST-DAY(WEEK-DAY)                       35320002
324200               SET WS-SCHEDULED-REST-DAY         TO TRUE          35330002
324300               SET JS-DONE                       TO TRUE          35340002
324400            END-IF                                                35350002
324500            IF JOB-SCHED-END-TIME >= WS-LOCAL-TIME                35360002
324600               SET JS-DONE                       TO TRUE          35370002
324700            ELSE                                                  35380002
324800               IF JOB-SCHED-END-TIME < JSK1-ASGN-START-TIME       35390002
324900                  SET JS-DONE                    TO TRUE          35400002
325000               END-IF                                             35410002
325100            END-IF                                                35420002
325200      END-EVALUATE                                                35430002
325300      .                                                           35440002
325400                                                                  35450002
325500*************************************************************     35460002
325600 P3230-CHECK-YESTERDAY.                                           35470002
325700*      IF THE VIEW TIME IS PRIOR TO THE FIRST SCHEDULED           35480002
325800*      WORK PERIOD, THEN THE PREVIOUS DAYS RECORDS HAVE TO BE     35490002
325900*      EVALUATED TO DETERMINE WHETHER THE EMPLOYEE IS STILL ON    35500002
326000*      THE PREVIOUS DAYS ASSIGNMENT.                              35510002
326100*************************************************************     35520002
326200      MOVE ZERO                         TO WS-YESTERDAY-START-TIME35530002
326300                                           WS-YESTERDAY-END-TIME  35540002
326400      IF WEEK-DAY = 1                                             35550002
326500         MOVE 07                         TO WS-YESTERDAY          35560002
326600      ELSE                                                        35570002
326700         SUBTRACT 01 FROM WEEK-DAY   GIVING WS-YESTERDAY          35580002
326800      END-IF                                                      35590002
326900      MOVE SPACES                       TO WORK-JS-KEY1           35600002
327000      MOVE DIST       OF WS-EXTRA-BOARD TO WK-JSK1-ASGN-DIST      35610002
327100      MOVE SUB-DIST   OF WS-EXTRA-BOARD TO WK-JSK1-ASGN-SUB-DIST  35620002
327200      STRING 'EX' TURN-NBR OF WS-EXTRA-BOARD                      35630002
327300             DELIMITED BY SIZE                                    35640002
327400             INTO WK-JSK1-ASGN                                    35650002
327500      MOVE CRAFT-CODE OF WS-EXTRA-BOARD TO WK-JSK1-ASGN-CC        35660002
327600      MOVE 999999                       TO WK-JSK1-EXP-DATE       35670002
327700      MOVE WS-YESTERDAY                 TO WK-JSK1-ASGN-DAY-NUM   35680002
327800      MOVE WORK-JS-KEY1                 TO JS-FS-KEY1             35690002
327900      PERFORM P8200-STARTBR-JS                                    35700002
328000      IF SUCCESS                                                  35710002
328100         PERFORM P8220-READNEXT-JS                                35720002
328200      END-IF                                                      35730002
328300      PERFORM UNTIL JS-DONE                                       35740002
328400         IF SUCCESS                                               35750002
328500            AND JSK1-ASGN-DIST           = WK-JSK1-ASGN-DIST      35760002
328600            AND JSK1-ASGN-SUB-DIST       = WK-JSK1-ASGN-SUB-DIST  35770002
328700            AND JSK1-ASSIGNMENT          = WK-JSK1-ASSIGNMENT     35780002
328800            AND JSK1-EXP-DATE            = WK-JSK1-EXP-DATE       35790002
328900            AND JSK1-ASGN-DAY            = WK-JSK1-ASGN-DAY-NUM   35800002
329000            MOVE JSK1-ASGN-START-TIME   TO WS-YESTERDAY-START-TIME35810002
329100            MOVE JOB-SCHED-END-TIME     TO WS-YESTERDAY-END-TIME  35820002
329200            SET MORE-JS                 TO TRUE                   35830002
329300         ELSE                                                     35840002
329400            SET JS-DONE                 TO TRUE                   35850002
329500         END-IF                                                   35860002
329600         IF MORE-JS                                               35870002
329700            PERFORM P8220-READNEXT-JS                             35880002
329800         END-IF                                                   35890002
329900      END-PERFORM                                                 35900002
330000      IF WS-YESTERDAY-END-TIME < WS-YESTERDAY-START-TIME          35910002
330100         AND WS-YESTERDAY-END-TIME >= WS-LOCAL-TIME               35920002
330200         CONTINUE                                                 35930002
330300      ELSE                                                        35940002
330400         SET WS-SCHEDULED-REST-PERIOD   TO TRUE                   35950002
330500      END-IF                                                      35960002
330600     .                                                            35970002
330700*                                                                 35980002
330800 P3250-CHECK-EXTENDED-REST-DAY.                                   35990002
330900*                                                                 36000002
331000     MOVE SPACES                        TO PS42-COMMAREA-PARMS    36010002
331100     SET PS42-GET-CURRENT-SCHED         TO TRUE                   36020002
331200     MOVE DIST       OF WS-EXTRA-BOARD  TO PS42-DIST-CD           36030002
331300     MOVE SUB-DIST   OF WS-EXTRA-BOARD  TO PS42-SUB-DIST-CD       36040002
331400     MOVE CRAFT-CODE OF WS-EXTRA-BOARD  TO PS42-BOARD-ID          36050002
331500     MOVE TURN-NBR   OF WS-EXTRA-BOARD  TO PS42-TURN-CD           36060002
331600     MOVE WS-LOCAL-DATE                 TO PS42-START-DATE        36070002
331700     MOVE WS-LOCAL-TIME                 TO PS42-FROM-TIME         36080002
331800*                                                                 36090002
331900     CALL P656-PGM USING PS42-COMMAREA-PARMS                      36100002
332000     IF PS42-SCHEDULES-EXIST                                      36110002
332100        OR PS42-CURR-SCHED-FOUND                                  36120002
332200        IF PS42-ON-REST-DAY                                       36130002
332300           SET ON-REST-DAY              TO TRUE                   36140002
332400        END-IF                                                    36150002
332500     ELSE                                                         36160002
332600        SET ON-REST-DAY                 TO TRUE                   36170002
332700     END-IF                                                       36180002
332800     .                                                            36190002
332900*CNC0516-BEG                                                      36200036
333000 P3260-GET-STATUS.                                                36210036
333100     INITIALIZE WS-EXTRABOARD-RESTED                              36220036
333200     EVALUATE TRUE                                                36230036
333300          WHEN OFF-MILES-DAYS                                     36240036
333400               PERFORM P9830-RETRIEVE-CNTL-INFO                   36250036
333500               IF P956-ST-RSN-NBR-DAYS-REQ                        36260036
333600               OR P956-ST-RSN-EXP-DATE-REQ                        36270036
333700                  PERFORM P3265-GET-DUEBACK-DATE                  36280036
333800                  IF WS-DUEBACK-FOUND-Y                           36290036
333900                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE 36300036
334000                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME 36310036
334010*CNC0573 - BEG                                                    36320060
334020                     IF WS-MASK-FLD-SCR-YES                       36330066
334040                        IF P956-MASK-FLD-SCR-YES                  36340060
334041*CNC0576 - BEG                                                    36350068
334042                        OR P956-MASK-HOLD-TURN                    36360068
334043                           MOVE '**'            TO WS-LAYOFF-CODE 36370068
334044                           IF P956-MASK-HOLD-TURN                 36380068
334045                              MOVE 'HT'      TO WS-LAYOFF-EM-CODE 36390068
334046                           ELSE                                   36400068
334047                              MOVE '**'      TO WS-LAYOFF-EM-CODE 36410068
334048                           END-IF                                 36420068
334049*CNC0576 - END                                                    36430068
334060                        ELSE                                      36440060
334070                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE 36450060
334080                           MOVE LAYOFF-EM-CODE  TO                36460060
334090                                WS-LAYOFF-EM-CODE                 36470060
334091                        END-IF                                    36480060
334092                     ELSE                                         36490060
334100                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE 36500060
334200                        MOVE LAYOFF-EM-CODE     TO                36510060
334300                             WS-LAYOFF-EM-CODE                    36520060
334301                     END-IF                                       36530060
334310*CNC0573 - END                                                    36540060
334400                     MOVE WS-EXTRABOARD-RESTED  TO                36550036
334500                          EXTRABOARD-RESTED                       36560036
334600                  ELSE                                            36570036
334700                     MOVE '<UNAVAILABLE>'       TO                36580036
334800                           EXTRABOARD-RESTED                      36590036
334900                  END-IF                                          36600036
335000               ELSE                                               36610036
335100                  PERFORM P3266-OFF-MILES-RETURN-DATE             36620036
335200                  MOVE WS-RETURN-DATE-1(1:4) TO WS-RETURN-DATE    36630051
335300                  MOVE SPACES                TO WS-RETURN-TIME    36640051
335310*CNC0573 - BEG                                                    36650060
335320                  IF WS-MASK-FLD-SCR-YES                          36660066
335340                     IF P956-MASK-FLD-SCR-YES                     36670060
335341*CNC0576 - BEG                                                    36680068
335342                     OR P956-MASK-HOLD-TURN                       36690068
335343                        MOVE '**'            TO WS-LAYOFF-CODE    36700068
335344                        IF P956-MASK-HOLD-TURN                    36710068
335345                           MOVE 'HT'      TO WS-LAYOFF-EM-CODE    36720068
335346                        ELSE                                      36730068
335347                           MOVE '**'      TO WS-LAYOFF-EM-CODE    36740068
335348                        END-IF                                    36750068
335349*CNC0576 - END                                                    36760068
335360                     ELSE                                         36770060
335361                        MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE    36780060
335362                        MOVE LAYOFF-EM-CODE  TO WS-LAYOFF-EM-CODE 36790060
335370                     END-IF                                       36800060
335380                  ELSE                                            36810060
335381                     MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE    36820060
335382                     MOVE LAYOFF-EM-CODE     TO WS-LAYOFF-EM-CODE 36830060
335390                  END-IF                                          36840060
335510*CNC0573 - END                                                    36850060
335600                  MOVE WS-EXTRABOARD-RESTED  TO EXTRABOARD-RESTED 36860051
335700               END-IF                                             36870036
335800          WHEN VACATION                                           36880036
335900               PERFORM P3267-VACATION-RETURN-DATE                 36890036
336000               MOVE WS-RETURN-DATE-1(1:4) TO WS-RETURN-DATE       36900051
336100               MOVE WS-RETURN-DATE-1(5:4) TO WS-RETURN-TIME       36910051
336110*CNC0573 - BEG                                                    36920060
336120               IF WS-MASK-FLD-SCR-YES                             36930066
336131                  PERFORM P9830-RETRIEVE-CNTL-INFO                36940060
336140                  IF P956-MASK-FLD-SCR-YES                        36950060
336141*CNC0576 - BEG                                                    36960068
336142                  OR P956-MASK-HOLD-TURN                          36970068
336143                     MOVE '**'            TO WS-LAYOFF-CODE       36980068
336144                     IF P956-MASK-HOLD-TURN                       36990068
336145                        MOVE 'HT'      TO WS-LAYOFF-EM-CODE       37000068
336146                     ELSE                                         37010068
336147                        MOVE '**'      TO WS-LAYOFF-EM-CODE       37020068
336148                     END-IF                                       37030068
336149*CNC0576 - END                                                    37040068
336160                  ELSE                                            37050060
336161                     MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE       37060060
336162                     MOVE LAYOFF-EM-CODE  TO WS-LAYOFF-EM-CODE    37070060
336170                  END-IF                                          37080060
336180               ELSE                                               37090060
336200                  MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE       37100060
336300                  MOVE LAYOFF-EM-CODE     TO WS-LAYOFF-EM-CODE    37110060
336301               END-IF                                             37120060
336310*CNC0573 - END                                                    37130060
336400               MOVE WS-EXTRABOARD-RESTED TO EXTRABOARD-RESTED     37140036
336500          WHEN EXCUSED-ABSENCE                                    37150036
336600           AND LAYOFF-EM-CODE = '69'                              37160036
336700               PERFORM P3265-GET-DUEBACK-DATE                     37170036
336800               IF WS-DUEBACK-FOUND-Y                              37180036
336900                  MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE    37190036
337000                  MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME    37200036
337010*CNC0573 - BEG                                                    37210060
337020                  IF WS-MASK-FLD-SCR-YES                          37220066
337040                     PERFORM P9830-RETRIEVE-CNTL-INFO             37230060
337050                     IF P956-MASK-FLD-SCR-YES                     37240060
337051*CNC0576 - BEG                                                    37250068
337052                     OR P956-MASK-HOLD-TURN                       37260068
337053                        MOVE '**'         TO WS-LAYOFF-CODE       37270068
337054                        IF P956-MASK-HOLD-TURN                    37280068
337055                           MOVE 'HT'      TO WS-LAYOFF-EM-CODE    37290068
337056                        ELSE                                      37300068
337057                           MOVE '**'      TO WS-LAYOFF-EM-CODE    37310068
337058                        END-IF                                    37320068
337059*CNC0576 - END                                                    37330068
337070                     ELSE                                         37340060
337071                        MOVE LAYOFF-CODE  TO WS-LAYOFF-CODE       37350060
337072                        MOVE LAYOFF-EM-CODE                       37360060
337073                                          TO WS-LAYOFF-EM-CODE    37370060
337080                     END-IF                                       37380060
337090                  ELSE                                            37390060
337100                     MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE       37400060
337200                     MOVE LAYOFF-EM-CODE  TO WS-LAYOFF-EM-CODE    37410060
337301                  END-IF                                          37420060
337310*CNC0573 - END                                                    37430060
337400                  MOVE WS-EXTRABOARD-RESTED  TO                   37440036
337500                       EXTRABOARD-RESTED                          37450036
337600               ELSE                                               37460036
337700                  MOVE '<UNAVAILABLE>'       TO                   37470036
337800                       EXTRABOARD-RESTED                          37480036
337900               END-IF                                             37490036
338000          WHEN OTHER                                              37500036
338100               IF NOT AVAILABLE  AND                              37510036
338200                  NOT WORKING    AND                              37520036
338300                  NOT TO-PLACE                                    37530036
338400                  AND LAYOFF-TIME NUMERIC                         37540036
338500                  AND LAYOFF-TIME > ZERO                          37550036
338600                  PERFORM P3265-GET-DUEBACK-DATE                  37560036
338700                  IF WS-DUEBACK-FOUND-Y                           37570036
338800                    PERFORM P3268-CHECK-FOR-E95-DTTM              37580056
338900                    IF WS-E95 > SPACES                            37590056
338910*CNC0573 - BEG                                                    37600060
338920                       IF WS-MASK-FLD-SCR-YES                     37610066
338940                          PERFORM P9830-RETRIEVE-CNTL-INFO        37620064
338950                          IF P956-MASK-FLD-SCR-YES                37630060
338951*CNC0576 - BEG                                                    37640068
338952                          OR P956-MASK-HOLD-TURN                  37650068
338953                             MOVE '**'        TO WS-E95-CODE      37660068
338954                             IF P956-MASK-HOLD-TURN               37670068
338955                                MOVE 'HT'     TO WS-E95-EM-CODE   37680068
338956                             ELSE                                 37690068
338957                                MOVE '**'     TO WS-E95-EM-CODE   37700068
338958                             END-IF                               37710068
338970                          END-IF                                  37720070
338994                       END-IF                                     37730060
338995                       MOVE WS-E95            TO EXTRABOARD-RESTED37740070
338996*CNC0576 - END                                                    37750070
339110*CNC0573 - END                                                    37760060
339200                    ELSE                                          37770056
339300                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE 37780056
339400                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME 37790056
339410*CNC0573 - BEG                                                    37800060
339420                     IF WS-MASK-FLD-SCR-YES                       37810066
339440                        PERFORM P9830-RETRIEVE-CNTL-INFO          37820060
339450                        IF P956-MASK-FLD-SCR-YES                  37830060
339451*CNC0576 - BEG                                                    37840068
339452                        OR P956-MASK-HOLD-TURN                    37850068
339453                           MOVE '**'            TO WS-LAYOFF-CODE 37860068
339454                           IF P956-MASK-HOLD-TURN                 37870068
339455                              MOVE 'HT'      TO WS-LAYOFF-EM-CODE 37880068
339456                           ELSE                                   37890068
339457                              MOVE '**'      TO WS-LAYOFF-EM-CODE 37900068
339458                           END-IF                                 37910068
339459*CNC0576 - END                                                    37920068
339470                        ELSE                                      37930060
339471                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE 37940060
339472                           MOVE LAYOFF-EM-CODE  TO                37950060
339473                                WS-LAYOFF-EM-CODE                 37960060
339480                        END-IF                                    37970060
339490                     ELSE                                         37980060
339500                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE 37990060
339600                        MOVE LAYOFF-EM-CODE     TO                38000060
339700                             WS-LAYOFF-EM-CODE                    38010060
339701                     END-IF                                       38020060
339710*CNC0573 - END                                                    38030060
339800                     MOVE WS-EXTRABOARD-RESTED  TO                38040056
339900                          EXTRABOARD-RESTED                       38050056
340000                    END-IF                                        38060056
340100                  ELSE                                            38070036
340200                    MOVE '<UNAVAILABLE>'        TO                38080056
340300                         EXTRABOARD-RESTED                        38090036
340400                  END-IF                                          38100036
340500               END-IF                                             38110036
340600     END-EVALUATE.                                                38120036
340700                                                                  38130036
340800 P3265-GET-DUEBACK-DATE.                                          38140036
340900                                                                  38150036
341000     MOVE SPACES                        TO TASK-EMPLOYEE-KEY      38160036
341100     SET WS-DUEBACK-FOUND-N             TO TRUE                   38170036
341200     MOVE EMP-NBR OF WS-MSTR            TO TASKEMP-FS-KEY         38180038
341300     PERFORM P8300-START-TASK-FILE                                38190036
341400     IF SUCCESS                                                   38200036
341500        PERFORM P8310-READNEXT-TASK-FILE                          38210036
341600        PERFORM UNTIL NOT SUCCESS                                 38220036
341700           OR WS-DUEBACK-FOUND-Y                                  38230036
341800           OR EMP-NBR OF WS-MSTR NOT = EMP-NBR OF WS-TASK         38240036
341900           IF   TASK-LAYOFF-MARKUP                                38250036
342000           AND ((TASK-DUE-BACK AND TASK-LO1 = 'A') OR             38260050
342100                 TASK-LO1 = 'A')                                  38270050
342200              SET WS-DUEBACK-FOUND-Y    TO TRUE                   38280036
342300           ELSE                                                   38290036
342400              PERFORM P8310-READNEXT-TASK-FILE                    38300036
342500           END-IF                                                 38310036
342600        END-PERFORM                                               38320036
342700     END-IF                                                       38330036
342800     .                                                            38340036
342900*                                                                 38350036
343000 P3266-OFF-MILES-RETURN-DATE.                                     38360036
343100*                                                                 38370036
343200     INITIALIZE WS-RETURN-DATE-1                                  38380052
343300     IF EMP-MILES-DATE NUMERIC                                    38390036
343400        AND EMP-MILES-DATE-NUM > 0                                38400036
343500        AND EMP-MILES-DATE-NUM < 32                               38410036
343600        MOVE WS-SYS-MO TO WS-RETURN-DATE-MM                       38420036
343700        MOVE EMP-MILES-DATE-NUM TO WS-RETURN-DATE-DD              38430036
343800        IF EMP-MILES-DATE-NUM < WS-SYS-DY                         38440036
343900           ADD 1 TO WS-RETURN-DATE-MM                             38450036
344000           IF WS-RETURN-DATE-MM > 12                              38460036
344100              MOVE 1 TO WS-RETURN-DATE-MM                         38470036
344200           END-IF                                                 38480036
344300        END-IF                                                    38490036
344400     ELSE                                                         38500036
344500        MOVE EMP-MILES-DATE TO WS-RETURN-DATE-1(1:4)              38510052
344600     END-IF                                                       38520036
344700     .                                                            38530036
344800*                                                                 38540036
344900 P3267-VACATION-RETURN-DATE.                                      38550036
345000*                                                                 38560036
345100     INITIALIZE WS-RETURN-DATE-1                                  38570052
345200                WS-EFFECTIVE-DATE-TIME                            38580054
345300     INITIALIZE TASK-EMPLOYEE-KEY                                 38590036
345400     MOVE EMP-NBR OF WS-MSTR TO TASKEMP-FS-KEY                    38600038
345500     PERFORM P8300-START-TASK-FILE                                38610036
345600     IF SUCCESS                                                   38620036
345700        SET TASK-NOT-DONE TO TRUE                                 38630036
345800        PERFORM P8310-READNEXT-TASK-FILE                          38640036
345900        PERFORM UNTIL TASK-DONE                                   38650036
346000           IF SUCCESS                                             38660036
346100              AND EMP-NBR OF WS-MSTR = EMP-NBR OF WS-TASK         38670036
346200              IF TASK-LAYOFF-MARKUP                               38680036
346300*C1129-BEG                                                        38690071
346400                AND TASK-LO1 = 'A'                                38700071
346401*C1129-END                                                        38710071
346402                 SET TASK-DONE           TO TRUE                  38720053
346403                 MOVE SPACES             TO WS-CNTL-FILE          38730053
346500                 SET SUB-DIST-TYPE-REC   TO TRUE                  38740053
346600                 MOVE TASK-DIST          TO CNTL-DIST             38750053
346700                 MOVE TASK-SUB-DIST      TO CNTL-SUB-DIST         38760053
346800                 MOVE CNTLKEY-AREA       TO CNTL-FS-KEY           38770054
346900                 PERFORM P8000-READ-CNTLFILE                      38780053
347000                 MOVE SPACES             TO TZ-PARAMETERS         38790053
347100                 SET TZ-IN-EASTERN-ZONE  TO TRUE                  38800053
347200                 MOVE EFFECTIVE-DATE-TIME    TO TZ-IN-DATE-TIME   38810053
347300                 MOVE CNTL-TIME-ZONE     TO TZ-OUT-ZONE           38820053
347400                 PERFORM P8996-TIMEZONE                           38830053
347500                 MOVE TZ-OUT-DATE-TIME   TO WS-EFFECTIVE-DATE-TIME38840053
347600                 MOVE WS-EFF-MO          TO WS-RETURN-DATE-MM     38850053
347700                 MOVE WS-EFF-DY          TO WS-RETURN-DATE-DD     38860053
347800                 MOVE WS-EFF-HR-MN       TO WS-RETURN-DATE-HRMN   38870053
347900              ELSE                                                38880036
348000                 PERFORM P8310-READNEXT-TASK-FILE                 38890036
348100              END-IF                                              38900036
348200           ELSE                                                   38910036
348300              SET TASK-DONE TO TRUE                               38920036
348400           END-IF                                                 38930036
348500        END-PERFORM                                               38940036
348600     END-IF                                                       38950036
348700     .                                                            38960036
348800 P3268-CHECK-FOR-E95-DTTM.                                        38970056
348900     MOVE SPACES                       TO WS-E95                  38980057
349000     MOVE EMP-NBR OF WS-MSTR           TO MSTR2NBRK               38990056
349100     PERFORM P8600-READ-MSTR2                                     39000056
349200     IF SUCCESS                                                   39010056
349300        IF MSTR2-RSA-LO-ST-RSN         = 'E95'                    39020056
349400           IF MSTR2-RSA-EXP-DTTM       > TASK-LO-EXP-DATE-TIME    39030056
349500              MOVE MSTR2-RSA-EXP-DTTM(3:4)  TO WS-E95-DATE        39040056
349600              MOVE MSTR2-RSA-EXP-DTTM(7:4)  TO WS-E95-TIME        39050056
349700              MOVE MSTR2-RSA-STATUS         TO WS-E95-CODE        39060056
349800              MOVE MSTR2-RSA-RSN-CD         TO WS-E95-EM-CODE     39070056
349900           END-IF                                                 39080056
350000        END-IF                                                    39090056
350100     END-IF.                                                      39100056
350200*CNC0516-END                                                      39110036
350300 P3300-SEARCH-SWASSGN.                                            39120002
350400                                                                  39130002
350500     MOVE SPACES                       TO WS-SWCNTL-FILE          39140002
350600     MOVE DIST OF WS-EXTRA-BOARD       TO SWCNTL-K-DISTRICT       39150002
350700     MOVE SUB-DIST OF WS-EXTRA-BOARD   TO SWCNTL-K-SUB-DIST       39160002
350800     MOVE DAY1                         TO SWCNTL-K-DAY            39170002
350900     MOVE ALL '9'                      TO SWCNTL-K-ASSIGN         39180002
351000     MOVE SWCNTL-KEY                   TO SWJOB-FS-KEY            39190002
351100     READ SWASSGN-FILE RECORD                                     39200002
351200          INTO WS-SWCNTL-FILE                                     39210002
351300          INVALID KEY CONTINUE                                    39220002
351400     END-READ                                                     39230002
351500     MOVE DAY1                         TO SW-DAY1                 39240002
351600     IF SUCCESS AND SWCNTL-DATE = WS-SYSTEM-DATE                  39250002
351700        IF SWCNTL-SHIFT3 NOT = 'X'                                39260002
351800          IF SW-DAY1 = 1                                          39270002
351900             MOVE 7                 TO SW-DAY1                    39280002
352000          ELSE                                                    39290002
352100             SUBTRACT 1 FROM SW-DAY1                              39300002
352200          END-IF                                                  39310002
352300        END-IF                                                    39320002
352400     ELSE                                                         39330002
352500       IF SW-DAY1 = 1                                             39340002
352600          MOVE 7                    TO SW-DAY1                    39350002
352700       ELSE                                                       39360002
352800          SUBTRACT 1 FROM SW-DAY1                                 39370002
352900       END-IF                                                     39380002
353000     END-IF                                                       39390002
353100*                                                                 39400002
353200     MOVE SPACES                       TO WS-SWASSGN-FILE         39410002
353300     MOVE DIST OF WS-EXTRA-BOARD       TO SWASSGN-K-DISTRICT      39420002
353400     MOVE SUB-DIST OF WS-EXTRA-BOARD   TO SWASSGN-K-SUB-DIST      39430002
353500     MOVE SW-DAY1                      TO SWASSGN-K-DAY           39440002
353600     MOVE TURN-NBR OF WS-EXTRA-BOARD   TO WS-SW-POSITION          39450002
353700     MOVE CRAFT-CODE OF WS-EXTRA-BOARD TO WS-SW-CRAFT             39460002
353800     MOVE WS-SWASSGN-ASGN              TO SWASSGN-K-ASSIGN        39470002
353900     MOVE SWASSGN-KEY                  TO SWJOB-FS-KEY            39480002
354000     READ SWASSGN-FILE RECORD                                     39490002
354100          INTO WS-SWASSGN-FILE                                    39500002
354200          INVALID KEY CONTINUE                                    39510002
354300     END-READ                                                     39520002
354400     IF SUCCESS                                                   39530002
354500        MOVE SWASSGN-EMP-NO            TO ASGN-EMP-NO             39540002
354600     END-IF.                                                      39550002
354700                                                                  39560002
354800*                                                                 39570002
354900 P3800-XB-TITLE.                                                  39580002
355000                                                                  39590002
355100     PERFORM P9000-TITLE                                          39600002
355200     WRITE PL-R FROM EXTRABOARDS-TITLE AFTER                      39610002
355300                                 ADVANCING 2 LINES                39620002
355400     ADD 2                       TO LINE-COUNT.                   39630002
355500                                                                  39640002
355600 P3900-START-XB-POS.                                              39650002
355700                                                                  39660002
355800     MOVE WORK-XB-POS-KEY TO EBPOS-FS-KEY                         39670002
355900     START EB-FILE KEY > EBPOS-FS-KEY                             39680002
356000       INVALID KEY CONTINUE                                       39690002
356100     END-START                                                    39700002
356200     IF NOT SUCCESS                                               39710002
356300        IF NOT (END-OF-FILE OR NO-RECORD-FND)                     39720002
356400           MOVE EBPOS-FS-KEY          TO ERR-KEY                  39730002
356500           MOVE FILE-STATUS           TO ERR-FSTAT                39740002
356600           MOVE 'EBPOS'               TO ERR-FNAME                39750002
356700           MOVE 'FAILED START EBFILE' TO ERR-DESC                 39760002
356800           GO TO P9999-GOT-PROBLEM                                39770002
356900        END-IF                                                    39780002
357000     END-IF.                                                      39790002
357100                                                                  39800002
357200*P3910-START-XB-TURN.                                             39810002
357300*    MOVE WORK-XB-TURN-KEY TO EBTURN-FS-KEY                       39820002
357400*    START EB-FILE KEY > EBTURN-FS-KEY                            39830002
357500*      INVALID KEY CONTINUE                                       39840002
357600*    END-START                                                    39850002
357700*    IF NOT SUCCESS                                               39860002
357800*       IF NOT (END-OF-FILE OR NO-RECORD-FND)                     39870002
357900*          MOVE EBTURN-FS-KEY         TO ERR-KEY                  39880002
358000*          MOVE FILE-STATUS           TO ERR-FSTAT                39890002
358100*          MOVE 'EBTURN'              TO ERR-FNAME                39900002
358200*          MOVE 'FAILED START EBFILE' TO ERR-DESC                 39910002
358300*          GO TO P9999-GOT-PROBLEM                                39920002
358400*       END-IF                                                    39930002
358500*    END-IF.                                                      39940002
358600                                                                  39950002
358700 P3920-READ-NEXT-XB-POS.                                          39960002
358800                                                                  39970002
358900     READ EB-FILE NEXT RECORD INTO WS-EXTRA-BOARD                 39980002
359000       AT END CONTINUE                                            39990002
359100     END-READ                                                     40000002
359200     IF NOT SUCCESS                                               40010002
359300        IF NOT (END-OF-FILE OR NO-RECORD-FND)                     40020002
359400           MOVE EBPOS-FS-KEY          TO ERR-KEY                  40030002
359500           MOVE FILE-STATUS           TO ERR-FSTAT                40040002
359600           MOVE 'EBPOS'               TO ERR-FNAME                40050002
359700           MOVE 'FAILED READNXT EBFILE' TO ERR-DESC               40060002
359800           GO TO P9999-GOT-PROBLEM                                40070002
359900        END-IF                                                    40080002
360000     END-IF.                                                      40090002
360100                                                                  40100002
360200*P3930-READ-NEXT-XB-TURN.                                         40110002
360300*                                                                 40120002
360400*    READ EB-FILE NEXT RECORD INTO WS-EXTRA-BOARD                 40130002
360500*      AT END CONTINUE                                            40140002
360600*    END-READ.                                                    40150002
360700*    IF NOT SUCCESS                                               40160002
360800*       IF NOT (END-OF-FILE OR NO-RECORD-FND)                     40170002
360900*          MOVE EBTURN-FS-KEY         TO ERR-KEY                  40180002
361000*          MOVE FILE-STATUS           TO ERR-FSTAT                40190002
361100*          MOVE 'EBTURN'              TO ERR-FNAME                40200002
361200*          MOVE 'FAILED READNXT EBFILE' TO ERR-DESC               40210002
361300*          GO TO P9999-GOT-PROBLEM                                40220002
361400*       END-IF                                                    40230002
361500*    END-IF.                                                      40240002
361600*                                                                 40250002
361700 P4000-GET-HOS.                                                   40260009
361800*                                                                 40270009
361900     INITIALIZE WS-TOT-TIME                                       40280010
362000                WS-LIMBO-TIME                                     40290010
362100                WS-CONSEC-STARTS                                  40300010
362200                                                                  40310023
362300     PERFORM P4100-GET-TIME-ZONE                                  40320020
362400     SET PS94-SUM-FUNC                 TO TRUE                    40330009
362500     MOVE MSTR-HOS-GROUP               TO PS94-MSTR-HOS-GROUP     40340030
362600     MOVE WS-LOCAL-DATE-TIME TO PS94-HSL-EVENT-STRT-TS-DTTM       40350009
362700                                PS94-HSL-EVENT-END-TS-DTTM        40360018
362800     MOVE WS-TIME-ZONE       TO PS94-TIME-ZONE                    40370020
362900     CALL PS94B-PGM USING PS94-COMMAREA-PARMS                     40380009
363000     IF NOT PS94-ERROR-ENCOUNTERED                                40390009
363100        MOVE PS94-MTD-TOTAL-TM        TO WS-TOT-TIME              40400010
363200        MOVE PS94-MTD-LIMBO-TM        TO WS-LIMBO-TIME            40410010
363300        MOVE PS94-CONSECUTIVE-STARTS  TO WS-CONSEC-STARTS         40420010
363400     END-IF                                                       40430009
363500     .                                                            40440009
363600*                                                                 40450019
363700 P4100-GET-TIME-ZONE.                                             40460019
363800*                                                                 40470019
363900     MOVE SPACES                        TO WS-CNTL-FILE           40480019
364000     SET  SUB-DIST-TYPE-REC             TO TRUE                   40490019
364100     MOVE DIST     OF WS-MSTR           TO CNTL-DIST              40500019
364200     MOVE SUB-DIST OF WS-MSTR           TO CNTL-SUB-DIST          40510019
364300     MOVE CNTLKEY-AREA                  TO CNTL-FS-KEY            40520019
364400     PERFORM P8000-READ-CNTLFILE                                  40530019
364500     IF SUCCESS                                                   40540019
364600        MOVE CNTL-TIME-ZONE             TO WS-TIME-ZONE           40550019
364700     ELSE                                                         40560019
364800        SET  WS-SYSTEM-TIME-ZONE        TO TRUE                   40570020
364900     END-IF                                                       40580019
365000     .                                                            40590019
365100*                                                                 40600009
365200 P5000-OFF-BOARDS.                                                40610002
365300*                                                                 40620002
365400     MOVE 'P5000'            TO ERR-PARAGRAPH                     40630002
365500     IF LINE-COUNT > 55                                           40640046
365600       PERFORM P9000-TITLE                                        40650002
365700     END-IF                                                       40660002
365800                                                                  40670002
365900     PERFORM VARYING OB-SUB FROM 1 BY 1 UNTIL OB-SUB > 3          40680002
366000       IF LINE-COUNT > 55                                         40690046
366100          PERFORM P9000-TITLE                                     40700002
366200       END-IF                                                     40710002
366300       IF OB-SUB = 1                                              40720002
366400          MOVE 'ENGINEERS OFF'   TO LAYOFF-TITLE-FIELD            40730002
366500          WRITE PL-R FROM LAYOFF-TITLE AFTER                      40740002
366600                                 ADVANCING 2 LINES                40750002
366700          ADD 2                  TO LINE-COUNT                    40760002
366800       ELSE                                                       40770002
366900          IF OB-SUB = 2                                           40780002
367000             MOVE 'TRAINMEN OFF '   TO LAYOFF-TITLE-FIELD         40790002
367100             WRITE PL-R FROM LAYOFF-TITLE AFTER                   40800002
367200                                    ADVANCING 2 LINES             40810002
367300             ADD 2                  TO LINE-COUNT                 40820002
367400          ELSE                                                    40830002
367500             MOVE 'SWITCHMAN OFF'   TO LAYOFF-TITLE-FIELD         40840002
367600             WRITE PL-R FROM LAYOFF-TITLE AFTER                   40850002
367700                                    ADVANCING 2 LINES             40860002
367800             ADD 2                  TO LINE-COUNT                 40870002
367900          END-IF                                                  40880002
368000       END-IF                                                     40890002
368100       MOVE SPACES         TO WS-MSTR                             40900002
368200       SET OB-NOT-DONE     TO TRUE                                40910002
368300       MOVE LINK-DIST      TO DIST OF WS-MSTR                     40920002
368400       MOVE LINK-SUB-DIST  TO SUB-DIST OF WS-MSTR                 40930002
368500       MOVE DIST-SDIST-CRFT-EMPNO-KEY TO MSTR-FS-DSCEK            40940002
368600       START MASTER-FILE  KEY > MSTR-FS-DSCEK                     40950002
368700             INVALID KEY CONTINUE                                 40960002
368800       END-START                                                  40970002
368900       IF SUCCESS                                                 40980002
369000          PERFORM UNTIL OB-DONE                                   40990002
369100             READ MASTER-FILE NEXT RECORD INTO WS-MSTR            41000002
369200                  AT END CONTINUE                                 41010002
369300             END-READ                                             41020002
369400             IF SUCCESS                                           41030002
369500                IF DIST OF WS-MSTR = LINK-DIST                    41040002
369600                  AND SUB-DIST OF WS-MSTR = LINK-SUB-DIST         41050002
369700                      IF NOT AVAILABLE AND NOT WORKING            41060002
369800                         MOVE CRAFT OF WS-MSTR TO                 41070002
369900                                    WS-CRAFT-CODE-CHECK           41080002
370000                         PERFORM P5100-WRITE-OFF-DETAIL           41090002
370100                             THRU P5100-WRITE-OFF-DETAIL-EXIT     41100002
370200                      END-IF                                      41110002
370300                ELSE                                              41120002
370400                   SET OB-DONE TO TRUE                            41130002
370500                END-IF                                            41140002
370600             ELSE                                                 41150002
370700                SET OB-DONE TO TRUE                               41160002
370800             END-IF                                               41170002
370900          END-PERFORM                                             41180002
371000       END-IF                                                     41190002
371100     END-PERFORM.                                                 41200002
371200*                                                                 41210002
371300 P5100-WRITE-OFF-DETAIL.                                          41220002
371400*                                                                 41230002
371500     IF OB-SUB = 1                                                41240002
371600        IF NOT ENGINE-CRAFT                                       41250002
371700           GO TO P5100-WRITE-OFF-DETAIL-EXIT                      41260002
371800        ELSE                                                      41270002
371900           IF HOSTLER-CRAFT                                       41280002
372000              GO TO P5100-WRITE-OFF-DETAIL-EXIT                   41290002
372100           END-IF                                                 41300002
372200        END-IF                                                    41310002
372300     ELSE                                                         41320002
372400        IF OB-SUB = 2                                             41330002
372500           IF NOT CONDUCTOR-CRAFT AND NOT BRAKEMAN-CRAFT          41340002
372600                 GO TO P5100-WRITE-OFF-DETAIL-EXIT                41350002
372700           END-IF                                                 41360002
372800        ELSE                                                      41370002
372900           IF NOT SWITCHMAN-CRAFT                                 41380002
373000              GO TO P5100-WRITE-OFF-DETAIL-EXIT                   41390002
373100           END-IF                                                 41400002
373200        END-IF                                                    41410002
373300     END-IF                                                       41420002
373400*                                                                 41430002
373500     PERFORM P8500-GET-MASTER-JOBS                                41440002
373600     MOVE SPACES TO LAYOFF-1                                      41450002
373700     MOVE EMP-NAME TO LAYOFF-NAME                                 41460002
373800     IF WS-CANADIAN-COMPANY                                       41470025
373900        MOVE SPACES          TO LAYOFF-HOS-AREA                   41480025
374000     ELSE                                                         41490025
374100        MOVE EMP-NBR  OF WS-MSTR TO PS94-EMP-NBR                  41500025
374200        PERFORM P4000-GET-HOS                                     41510025
374300        MOVE SPACES                 TO LAYOFF-HOS-AREA            41520025
374400        STRING 'T '                                               41530027
374500          WS-TOT-TM-HH ':'                                        41540010
374600          WS-TOT-TM-MM                                            41550010
374700            ' L '                                                 41560027
374800          WS-LIMBO-TM-HH ':'                                      41570010
374900          WS-LIMBO-TM-MM                                          41580010
375000            ' S:'                                                 41590027
375100          WS-CONSEC-STARTS                                        41600017
375200          DELIMITED BY SIZE INTO LAYOFF-HOS-AREA                  41610010
375300     END-IF                                                       41620025
375400*    PERFORM VARYING I FROM 1 BY 1 UNTIL I > LO-ARRAY-MAX         41630031
375500*      IF LAYOFF-CODE-1 = WS-LO-CODE(I)                           41640031
375600*        MOVE WS-LO-CODE-DESC(I, 1) TO LAYOFF-STATUS              41650031
375700*        MOVE  LO-ARRAY-MAX TO I                                  41660031
375800*      END-IF                                                     41670031
375900*    END-PERFORM                                                  41680031
376000                                                                  41690031
376100     MOVE SPACES                    TO P956-COMMAREA-PARMS        41700031
376200     SET P956-GET-CNTL-STATUS-CODE  TO TRUE                       41710031
376300     MOVE LAYOFF-CODE-1             TO P956-STATUS-CODE           41720031
376400     CALL P856-PGM USING P956-COMMAREA-PARMS                      41730031
376500     IF P956-ERROR-FOUND                                          41740031
376600        MOVE 'NOT FOUND'            TO LAYOFF-STATUS              41750031
376700     ELSE                                                         41760031
376800        MOVE P956-STATUS-LONG-DESC  TO LAYOFF-STATUS              41770031
376801*CNC0573 - BEG                                                    41780065
376802        IF WS-MASK-FLD-SCR-YES                                    41790067
376803           PERFORM P9830-RETRIEVE-CNTL-INFO                       41800067
376804           IF P956-MASK-FLD-SCR-YES                               41810067
376805              MOVE '**     OFF     ** ' TO LAYOFF-STATUS          41820068
376806*CNC0576 - BEG                                                    41830068
376807           ELSE                                                   41840068
376808              IF P956-MASK-HOLD-TURN                              41850068
376809                 MOVE '**OFF HOLD TURN** ' TO LAYOFF-STATUS       41860068
376810              END-IF                                              41870068
376811*CNC0576 - END                                                    41880068
376812           END-IF                                                 41890067
376813        END-IF                                                    41900067
376820*CNC0573 - END                                                    41910067
376900     END-IF                                                       41920031
377000                                                                  41930031
377100*    EVALUATE TRUE                                                41940033
377200*      WHEN NOT-NOTIFIED   MOVE 'NOT NOTIFIED' TO LAYOFF-STATUS   41950033
377300*      WHEN ON-CALL        MOVE 'ON CALL'      TO LAYOFF-STATUS   41960033
377400*    END-EVALUATE                                                 41970033
377500                                                                  41980002
377600     WRITE PL-R FROM LAYOFF-1 AFTER ADVANCING 1 LINE              41990002
377700     ADD 1 TO LINE-COUNT                                          42000002
377800     IF LINE-COUNT > 55                                           42010046
377900        PERFORM P9000-TITLE                                       42020002
378000     END-IF.                                                      42030002
378100*                                                                 42040002
378200 P5100-WRITE-OFF-DETAIL-EXIT.                                     42050002
378300     EXIT.                                                        42060002
378400*                                                                 42070002
378500 P5200-CHECK-COMPANY-CD.                                          42080005
378600*                                                                 42090002
378700     MOVE SPACES                 TO CNTLKEY                       42100002
378800                                    CNTLKEY-AREA                  42110002
378900     MOVE DIST OF WS-MSTR        TO CNTL-DIST                     42120002
379000     SET DIST-TYPE-REC           TO TRUE                          42130002
379100     MOVE CNTLKEY-AREA           TO CNTL-FS-KEY                   42140008
379200     PERFORM P8000-READ-CNTLFILE                                  42150006
379300     IF CNTL-BCR-COMPANY                                          42160005
379400        OR CNTL-CN-COMPANY                                        42170005
379500        OR CNTL-ACR-COMPANY                                       42180005
379600        SET WS-CANADIAN-COMPANY  TO TRUE                          42190005
379700     ELSE                                                         42200005
379800        SET WS-US-COMPANY        TO TRUE                          42210005
379900     END-IF                                                       42220005
380000     IF WS-CANADIAN-COMPANY                                       42230005
380100        SET DONT-APPLY-LEAD-TIME TO TRUE                          42240005
380200     ELSE                                                         42250005
380300        SET APPLY-LEAD-TIME      TO TRUE                          42260005
380400     END-IF                                                       42270005
380500     .                                                            42280005
380600*                                                                 42290002
380700 P7500-GET-UFP-EMPS.                                              42300002
380800                                                                  42310002
380900     MOVE ZERO TO OWNER-EMP-NBR TEMP-EMP-ONE ON-DUTY-EMP          42320002
381000     MOVE SPACE TO WS-ASGN-FILE                                   42330002
381100     MOVE 'U' TO ASGN-JOB-TYPE                                    42340002
381200     MOVE UFPTURN-AREA TO ASGN-ASSIGNMENT                         42350002
381300     PERFORM PXXXX-JOB-OWNER                                      42360002
381400     MOVE ASGN-EMP-NO TO OWNER-EMP-NBR                            42370002
381500                                                                  42380002
381600     MOVE SPACE TO WS-ASGN-FILE                                   42390002
381700     MOVE 'U' TO ASGN-JOB-TYPE                                    42400002
381800     MOVE UFPTURN-AREA TO ASGN-ASSIGNMENT                         42410002
381900     PERFORM PXXXX-ON-DUTY-EMP                                    42420002
382000     MOVE ASGN-EMP-NO TO ON-DUTY-EMP                              42430002
382100                                                                  42440002
382200     MOVE SPACE TO WS-ASGN-FILE                                   42450002
382300     MOVE 'U' TO ASGN-JOB-TYPE                                    42460002
382400     MOVE UFPTURN-AREA TO ASGN-ASSIGNMENT                         42470002
382500     PERFORM PXXXX-LATEST-TEMP                                    42480002
382600     MOVE ASGN-EMP-NO TO TEMP-EMP-ONE.                            42490002
382700                                                                  42500002
382800 P7800-GET-AJ-EMPS.                                               42510002
382900                                                                  42520002
383000     MOVE ZERO TO OWNER-EMP-NBR TEMP-EMP-ONE ON-DUTY-EMP          42530002
383100     MOVE SPACE TO WS-ASGN-FILE                                   42540002
383200     SET ASGN-AJ-JOB IN ASGN-JOB-TYPE TO TRUE                     42550002
383300     MOVE AJ-JOB-DIST                 TO ASGN-DIST                42560002
383400     MOVE AJ-JOB-SUB-DIST             TO ASGN-SUB-DIST            42570002
383500     MOVE AJ-JOB-ASSIGNMENT           TO ASGN-AJ-JOB              42580002
383600                                         IN ASGN-ASSIGNMENT       42590002
383700     PERFORM PXXXX-JOB-OWNER                                      42600002
383800     MOVE ASGN-EMP-NO TO OWNER-EMP-NBR                            42610002
383900                                                                  42620002
384000     MOVE SPACE TO WS-ASGN-FILE                                   42630002
384100     SET ASGN-AJ-JOB IN ASGN-JOB-TYPE TO TRUE                     42640002
384200     MOVE AJ-JOB-DIST                 TO ASGN-DIST                42650002
384300     MOVE AJ-JOB-SUB-DIST             TO ASGN-SUB-DIST            42660002
384400     MOVE AJ-JOB-ASSIGNMENT           TO ASGN-AJ-JOB              42670002
384500                                         IN ASGN-ASSIGNMENT       42680002
384600     PERFORM PXXXX-ON-DUTY-EMP                                    42690002
384700     MOVE ASGN-EMP-NO TO ON-DUTY-EMP                              42700002
384800                                                                  42710002
384900     MOVE SPACE TO WS-ASGN-FILE                                   42720002
385000     SET ASGN-AJ-JOB IN ASGN-JOB-TYPE TO TRUE                     42730002
385100     MOVE AJ-JOB-DIST                 TO ASGN-DIST                42740002
385200     MOVE AJ-JOB-SUB-DIST             TO ASGN-SUB-DIST            42750002
385300     MOVE AJ-JOB-ASSIGNMENT           TO ASGN-AJ-JOB              42760002
385400                                         IN ASGN-ASSIGNMENT       42770002
385500     PERFORM PXXXX-LATEST-TEMP                                    42780002
385600     MOVE ASGN-EMP-NO TO TEMP-EMP-ONE.                            42790002
385700                                                                  42800002
385800 P8000-READ-CNTLFILE.                                             42810002
385900                                                                  42820002
386000     READ CONTROL-FILE RECORD                                     42830002
386100                       INTO WS-CNTL-FILE                          42840002
386200                       KEY IS CNTL-FS-KEY                         42850002
386300          INVALID KEY CONTINUE                                    42860002
386400     END-READ.                                                    42870002
386500                                                                  42880002
386600 P8000-READ-NEXT-CNTLFILE.                                        42890002
386700                                                                  42900002
386800     READ CONTROL-FILE NEXT RECORD INTO WS-CNTL-FILE              42910002
386900          AT END CONTINUE                                         42920002
387000     END-READ.                                                    42930002
387100                                                                  42940002
387200 P8000-START-CNTLFILE.                                            42950002
387300                                                                  42960002
387400     START CONTROL-FILE KEY > CNTL-FS-KEY                         42970002
387500           INVALID KEY CONTINUE                                   42980002
387600     END-START.                                                   42990002
387700                                                                  43000036
387800*CNC0516-BEG                                                      43010036
387900 P8300-START-TASK-FILE.                                           43020036
388000     START TASK-FILE KEY >= TASKEMP-FS-KEY                        43030044
388100           INVALID KEY CONTINUE                                   43040036
388200     END-START.                                                   43050036
388300 P8310-READNEXT-TASK-FILE.                                        43060036
388400     READ TASK-FILE NEXT RECORD INTO WS-TASK                      43070036
388500          AT END CONTINUE                                         43080036
388600     END-READ.                                                    43090036
388700*CNC0516-END                                                      43100037
388800*                                                                 43110002
388900 P8200-STARTBR-JS.                                                43120002
389000*                                                                 43130002
389100     START JS-FILE                                                43140002
389200           KEY >= JS-FS-KEY1                                      43150002
389300           INVALID KEY CONTINUE                                   43160002
389400     END-START                                                    43170002
389500     .                                                            43180002
389600                                                                  43190002
389700*                                                                 43200002
389800 P8220-READNEXT-JS.                                               43210002
389900*                                                                 43220002
390000     READ JS-FILE                                                 43230002
390100          NEXT RECORD                                             43240002
390200          INTO WS-JOB-SCHEDULE                                    43250002
390300          AT END CONTINUE                                         43260002
390400     END-READ                                                     43270002
390500     .                                                            43280002
390600*                                                                 43290002
390700 P8240-READ-JS.                                                   43300002
390800*                                                                 43310002
390900     READ JS-FILE RECORD                                          43320002
391000          INTO WS-JOB-SCHEDULE                                    43330002
391100          INVALID KEY CONTINUE                                    43340002
391200     END-READ                                                     43350002
391300     .                                                            43360002
391400                                                                  43370002
391500 P8500-READ-MASTER.                                               43380002
391600                                                                  43390002
391700     MOVE MSTRNBRK TO ERR-KEY                                     43400002
391800                      EMP-NBR-KEY                                 43410002
391900                      MSTR-FS-NBRK                                43420002
392000     IF MSTRNBRK > ZERO                                           43430002
392100       READ MASTER-FILE RECORD INTO WS-MSTR                       43440002
392200            INVALID KEY CONTINUE                                  43450002
392300       END-READ                                                   43460002
392400       IF NOT SUCCESS                                             43470002
392500          IF NOT (END-OF-FILE OR NO-RECORD-FND)                   43480002
392600             MOVE MSTR-FS-NBRK          TO ERR-KEY                43490002
392700             MOVE FILE-STATUS           TO ERR-FSTAT              43500002
392800             MOVE 'MSTRFILE'            TO ERR-FNAME              43510002
392900             MOVE 'FAILED READ MSTRFILE' TO ERR-DESC              43520002
393000             GO TO P9999-GOT-PROBLEM                              43530002
393100          END-IF                                                  43540002
393200       ELSE                                                       43550002
393300         PERFORM P8500-GET-MASTER-JOBS                            43560002
393400       END-IF                                                     43570002
393500     END-IF.                                                      43580002
393600                                                                  43590002
393700 P8500-GET-MASTER-JOBS.                                           43600002
393800                                                                  43610002
393900     MOVE SPACES TO WS-ASGN-FILE                                  43620002
394000          NORMAL-ASGNMT TEMPORARY-ASGNMT ON-DUTY-ASGNMT           43630002
394100     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       43640002
394200     PERFORM PXXXX-JOB-OWNED                                      43650002
394300     MOVE ASGN-JOB-TYPE TO NORMAL-ASGNMT-FLAG                     43660002
394400     MOVE ASGN-ASSIGNMENT TO NORMAL-ASGNMT                        43670002
394500                                                                  43680002
394600     MOVE SPACES TO WS-ASGN-FILE                                  43690002
394700     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       43700002
394800     PERFORM PXXXX-LATEST-TEMP-JOB                                43710002
394900     MOVE ASGN-JOB-TYPE TO TEMPORARY-ASGNMT-FLAG                  43720002
395000     MOVE SPACE TO TEMP-ASGN-XB-AUG-FLAG                          43730002
395100     IF ASGN-JOB-TYPE = 'X'                                       43740002
395200       AND AUGMENTED-TO-EXTRA-BOARD                               43750002
395300           SET TEMP-ASGN-XB-AUG TO TRUE                           43760002
395400     END-IF                                                       43770002
395500     MOVE ASGN-ASSIGNMENT TO TEMPORARY-ASGNMT                     43780002
395600                                                                  43790002
395700     MOVE SPACES TO WS-ASGN-FILE                                  43800002
395800     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       43810002
395900     PERFORM PXXXX-JOB-ON-DUTY                                    43820002
396000     MOVE ASGN-JOB-TYPE TO ON-DUTY-ASGNMT-FLAG                    43830002
396100     MOVE ASGN-ASSIGNMENT TO ON-DUTY-ASGNMT                       43840002
396200     MOVE ASGN-ON-DUTY-DATE-TIME TO ON-DUTY-OUT-TOWN-CODE.        43850002
396300                                                                  43860002
396400*CNC0516-BEG                                                      43870056
396500*                                                                 43880056
396600 P8600-READ-MSTR2.                                                43890056
396700*                                                                 43900056
396800     MOVE MSTR2NBRK TO ERR-KEY                                    43910056
396900                       MSTR2-EMP-NBR                              43920056
397000                       MSTR2-FS-NBRK                              43930056
397100     IF MSTR2NBRK > ZERO                                          43940056
397200       READ MASTER-FILE2 RECORD INTO WS-MSTR2                     43950056
397300            INVALID KEY CONTINUE                                  43960056
397400       END-READ                                                   43970056
397500       IF NOT SUCCESS                                             43980056
397600          IF NOT (END-OF-FILE OR NO-RECORD-FND)                   43990056
397700             MOVE MSTR2-FS-NBRK           TO ERR-KEY              44000056
397800             MOVE FILE-STATUS             TO ERR-FSTAT            44010056
397900             MOVE 'MSTR2FILE'             TO ERR-FNAME            44020056
398000             MOVE 'FAILED READ MSTRFILE2' TO ERR-DESC             44030056
398100             GO TO P9999-GOT-PROBLEM                              44040056
398200          END-IF                                                  44050056
398300       END-IF                                                     44060056
398400     END-IF.                                                      44070056
398500*CNC0516-END                                                      44080056
398600*P8800-FORMAT-TIME.                                               44090002
398700*                                                                 44100002
398800*    MOVE WK-YR TO F-YR    MOVE WK-MO TO F-MO                     44110002
398900*    MOVE WK-DY TO F-DY    MOVE WORK-HR-MN TO F-HR-MN.            44120002
399000                                                                  44130002
399100                                                                  44140002
399200 COPY SENANAL.                                                    44150002
399300 COPY CNTRTXT.                                                    44160002
399400 COPY DATEEDIT.                                                   44170002
399500 COPY OFFSET.                                                     44180002
399600 COPY TIMEZONE.                                                   44190002
399700                                                                  44200002
399800 P9000-TITLE.                                                     44210002
399900                                                                  44220002
400000     MOVE SPACES TO WS-CNTL-FILE                                  44230002
400100     SET SUB-DIST-TYPE-REC TO TRUE                                44240002
400200     MOVE LINK-DIST     TO CNTL-DIST                              44250002
400300     IF DIST-SDIST-POOL-LM-MC-JN                                  44260002
400400        MOVE WS-SUB-DIST-JX TO CNTL-SUB-DIST                      44270002
400500     ELSE                                                         44280002
400600        MOVE LINK-SUB-DIST  TO CNTL-SUB-DIST                      44290002
400700     END-IF                                                       44300002
400800     MOVE CNTLKEY-AREA  TO CNTL-FS-KEY                            44310002
400900     PERFORM P8000-READ-CNTLFILE                                  44320002
401000     IF SUB-DIST-TYPE-REC AND CNTL-SUB-DIST-NAME > SPACES         44330002
401100        MOVE 26                 TO CTXT-UNF-FIELD-LEN             44340002
401200        MOVE CNTL-SUB-DIST-NAME TO CTXT-UNF-FIELD                 44350002
401300        PERFORM P8994-CENTER-TEXT                                 44360002
401400        MOVE CTXT-FOR-FIELD    TO REPORT-TITLE-TERM               44370002
401500     ELSE                                                         44380002
401600        MOVE '<< NONE ON FILE >>' TO REPORT-TITLE-TERM            44390002
401700     END-IF                                                       44400002
401800                                                                  44410002
401900     MOVE WS-SYS-YR TO TIT-YR MOVE WS-SYS-MO TO TIT-MO            44420002
402000     MOVE WS-SYS-DY TO TIT-DY MOVE WS-SYS-HR TO TIT-HR            44430002
402100     MOVE WS-SYS-MN TO TIT-MN                                     44440002
402200                                                                  44450002
402300     ADD 1 TO REPORT-TITLE-PAGE                                   44460002
402400     WRITE PL-R FROM TITLE-CUSTOMER AFTER ADVANCING PAGE          44470002
402500     WRITE PL-R FROM REPORT-TITLE-TERMINAL AFTER ADVANCING 1 LINE 44480002
402600     WRITE PL-R FROM REPORT-TITLE          AFTER ADVANCING 1 LINE 44490002
402700     MOVE 3 TO LINE-COUNT.                                        44500002
402800                                                                  44510040
402900                                                                  44520040
403000 PXXXX-JOB-OWNER.                                                 44530002
403100                                                                  44540002
403200     SET ASGN-OWNER-REC TO TRUE                                   44550002
403300     MOVE ZERO TO ASGN-DATE-TIME                                  44560002
403400     MOVE ASGN-AREA TO ASGN-FS-JOB                                44570002
403500     READ ASGN-FILE RECORD INTO ASGN-AREA                         44580002
403600       INVALID KEY CONTINUE                                       44590002
403700     END-READ                                                     44600002
403800     IF SUCCESS                                                   44610002
403900       CONTINUE                                                   44620002
404000     ELSE                                                         44630002
404100       IF NO-RECORD-FND OR END-OF-FILE                            44640002
404200         MOVE 000000 TO ASGN-EMP-NO                               44650002
404300       END-IF                                                     44660002
404400     END-IF.                                                      44670002
404500                                                                  44680002
404600 PXXXX-LATEST-TEMP.                                               44690002
404700                                                                  44700002
404800     MOVE SPACES       TO SAVE-ASGN-AREA                          44710002
404900     SET ASGN-TEMP-REC TO TRUE                                    44720002
405000     MOVE ZEROS        TO ASGN-DATE-TIME                          44730002
405100     MOVE ASGNKEY1     TO ASGNJOB XXXX-ASGNKEY1                   44740002
405200     MOVE ASGN-AREA    TO ASGN-FS-JOB                             44750002
405300     START ASGN-FILE KEY > ASGN-FS-JOB                            44760002
405400           INVALID KEY CONTINUE                                   44770002
405500     END-START                                                    44780002
405600     IF SUCCESS                                                   44790002
405700       MOVE 0 TO ASGN-DONE-CODE                                   44800002
405800       PERFORM UNTIL ASGN-DONE                                    44810002
405900         READ ASGN-FILE NEXT RECORD INTO ASGN-AREA                44820002
406000           AT END CONTINUE                                        44830002
406100         END-READ                                                 44840002
406200         IF SUCCESS                                               44850002
406300           IF XX-ASGN-DIST = ASGN-DIST AND                        44860002
406400              XX-ASGN-SUB-DIST = ASGN-SUB-DIST AND                44870002
406500              XX-ASGN-JOB = ASGN-AJ-JOB OF ASGN-ASSIGNMENT        44880002
406600             AND ASGN-TEMP-REC                                    44890002
406700             MOVE ASGN-AREA TO SAVE-ASGN-AREA                     44900002
406800*              SET ASGN-DONE TO TRUE                              44910002
406900           ELSE                                                   44920002
407000             MOVE ZERO TO ASGN-EMP-NO                             44930002
407100             SET ASGN-DONE TO TRUE                                44940002
407200           END-IF                                                 44950002
407300         ELSE                                                     44960002
407400           IF NO-RECORD-FND OR END-OF-FILE                        44970002
407500             MOVE ZERO TO ASGN-EMP-NO                             44980002
407600             SET ASGN-DONE TO TRUE                                44990002
407700           ELSE                                                   45000002
407800             MOVE ZERO TO ASGN-EMP-NO                             45010002
407900             SET ASGN-DONE TO TRUE                                45020002
408000           END-IF                                                 45030002
408100         END-IF                                                   45040002
408200       END-PERFORM                                                45050002
408300     ELSE                                                         45060002
408400         MOVE ZERO TO ASGN-EMP-NO                                 45070002
408500     END-IF.                                                      45080002
408600     IF SAVE-ASGN-AREA > SPACE                                    45090002
408700       MOVE SAVE-ASGN-AREA TO ASGN-AREA                           45100002
408800     ELSE                                                         45110002
408900       MOVE ZERO TO ASGN-EMP-NO                                   45120002
409000     END-IF.                                                      45130002
409100                                                                  45140002
409200 PXXXX-ON-DUTY-EMP.                                               45150002
409300                                                                  45160002
409400     SET ASGN-ON-DUTY-REC TO TRUE                                 45170002
409500     MOVE ZERO            TO ASGN-DATE-TIME                       45180002
409600     MOVE ASGNKEY1        TO ASGNJOB ASGN-FS-JOB                  45190002
409700     READ ASGN-FILE RECORD INTO ASGN-AREA                         45200002
409800          INVALID KEY CONTINUE                                    45210002
409900     END-READ                                                     45220002
410000     IF SUCCESS                                                   45230002
410100       CONTINUE                                                   45240002
410200     ELSE                                                         45250002
410300       MOVE ZERO TO ASGN-EMP-NO                                   45260002
410400     END-IF.                                                      45270002
410500                                                                  45280002
410600 PXXXX-JOB-OWNED.                                                 45290002
410700                                                                  45300002
410800     MOVE '1'       TO ASGN-EMP-NO-REC-TYPE                       45310002
410900     MOVE ZEROS     TO ASGN-EMP-DATE-TIME                         45320002
411000     MOVE ASGNKEY2  TO ASGNEMP ASGN-FS-EMP                        45330002
411100     READ ASGN-FILE RECORD INTO WS-ASGN-FILE                      45340002
411200          KEY IS ASGN-FS-EMP                                      45350002
411300          INVALID KEY CONTINUE                                    45360002
411400     END-READ                                                     45370002
411500     IF SUCCESS                                                   45380002
411600       CONTINUE                                                   45390002
411700     ELSE                                                         45400002
411800         MOVE SPACE TO ASGN-ASSIGNMENT                            45410002
411900     END-IF.                                                      45420002
412000                                                                  45430002
412100 PXXXX-LATEST-TEMP-JOB.                                           45440002
412200                                                                  45450002
412300     MOVE SPACES        TO SAVE-ASGN-AREA                         45460002
412400     SET ASGN-EMP-TEMP-REC TO TRUE                                45470002
412500     MOVE ZEROS         TO ASGN-EMP-DATE-TIME                     45480002
412600     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       45490002
412700     MOVE ASGNKEY2      TO XXXX-ASGNKEY2                          45500002
412800     MOVE XXXX-ASGNKEY2 TO ASGNKEY2 ASGN-FS-EMP                   45510002
412900     START ASGN-FILE KEY > ASGN-FS-EMP                            45520002
413000          INVALID KEY CONTINUE                                    45530002
413100     END-START                                                    45540002
413200     IF SUCCESS                                                   45550002
413300        MOVE 0 TO ASGN-DONE-CODE                                  45560002
413400        PERFORM UNTIL ASGN-DONE                                   45570002
413500           READ ASGN-FILE NEXT RECORD INTO WS-ASGN-FILE           45580002
413600                AT END CONTINUE                                   45590002
413700           END-READ                                               45600002
413800           IF SUCCESS                                             45610002
413900              IF ASGN-EMP-NO = XX-ASGN-EMP AND                    45620002
414000                 ASGN-EMP-NO-REC-TYPE = '2'                       45630002
414100                 MOVE ASGN-AREA TO SAVE-ASGN-AREA                 45640002
414200              ELSE                                                45650002
414300                 SET ASGN-DONE TO TRUE                            45660002
414400              END-IF                                              45670002
414500           ELSE                                                   45680002
414600              IF NO-RECORD-FND OR END-OF-FILE                     45690002
414700                 SET ASGN-DONE TO TRUE                            45700002
414800              END-IF                                              45710002
414900           END-IF                                                 45720002
415000        END-PERFORM                                               45730002
415100     END-IF                                                       45740002
415200     IF SAVE-ASGN-AREA > SPACES                                   45750002
415300        MOVE SAVE-ASGN-AREA TO ASGN-AREA                          45760002
415400     ELSE                                                         45770002
415500        MOVE SPACE          TO ASGN-ASSIGNMENT                    45780002
415600     END-IF.                                                      45790002
415700                                                                  45800002
415800 PXXXX-JOB-ON-DUTY.                                               45810002
415900                                                                  45820002
416000     MOVE '3'       TO ASGN-EMP-NO-REC-TYPE                       45830002
416100     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       45840002
416200     MOVE ZEROS     TO ASGN-EMP-DATE-TIME                         45850002
416300     MOVE ASGNKEY2  TO ASGNEMP ASGN-FS-EMP                        45860002
416400                       XXXX-ASGNKEY2                              45870002
416500     READ ASGN-FILE RECORD INTO WS-ASGN-FILE                      45880002
416600          KEY IS ASGN-FS-EMP                                      45890002
416700          INVALID KEY CONTINUE                                    45900002
416800     END-READ                                                     45910002
416900     IF SUCCESS                                                   45920002
417000        IF ASGN-EMP-NO = XX-ASGN-EMP AND                          45930002
417100          ASGN-EMP-NO-REC-TYPE = '3'                              45940002
417200            CONTINUE                                              45950002
417300        ELSE                                                      45960002
417400            MOVE SPACES TO ASGN-ASSIGNMENT                        45970002
417500        END-IF                                                    45980002
417600     ELSE                                                         45990002
417700       MOVE SPACE TO ASGN-ASSIGNMENT                              46000002
417800     END-IF.                                                      46010002
417900                                                                  46020002
418000 P9500-OPEN-FILES.                                                46030002
418100                                                                  46040002
418200     MOVE 'P9500' TO ERR-PARAGRAPH                                46050002
418300     OPEN OUTPUT PL                                               46060002
418400     IF NOT SUCCESS                                               46070002
418500        MOVE SPACES                TO ERR-KEY                     46080002
418600        MOVE FILE-STATUS           TO ERR-FSTAT                   46090002
418700        MOVE 'PL'                  TO ERR-FNAME                   46100002
418800        MOVE 'CANNOT OPEN PRINTER' TO ERR-DESC                    46110002
418900        GO TO P9999-GOT-PROBLEM                                   46120002
419000     END-IF                                                       46130002
419100                                                                  46140002
419200     OPEN INPUT AJ-FILE                                           46150002
419300     IF NOT SUCCESS                                               46160002
419400        MOVE AJ-FS-JOBKEY          TO ERR-KEY                     46170002
419500        MOVE 'AJJOBKEY'            TO ERR-FNAME                   46180002
419600        MOVE FILE-STATUS           TO ERR-FSTAT                   46190002
419700        MOVE 'CANNOT OPEN JOB ASSIGNMENT FILE' TO ERR-DESC        46200002
419800        GO TO P9999-GOT-PROBLEM                                   46210002
419900     END-IF                                                       46220002
420000                                                                  46230002
420100     OPEN INPUT JS-FILE                                           46240002
420200     IF NOT SUCCESS                                               46250002
420300        MOVE JS-FS-KEY1            TO ERR-KEY                     46260002
420400        MOVE 'JSKEY1'              TO ERR-FNAME                   46270002
420500        MOVE FILE-STATUS           TO ERR-FSTAT                   46280002
420600        MOVE 'CANNOT OPEN JOB SCHEDULE FILE' TO ERR-DESC          46290002
420700        GO TO P9999-GOT-PROBLEM                                   46300002
420800     END-IF                                                       46310002
420900                                                                  46320002
421000     OPEN INPUT ASGN-FILE                                         46330002
421100     IF NOT SUCCESS                                               46340002
421200        MOVE ASGN-FS-JOB           TO ERR-KEY                     46350002
421300        MOVE 'ASGNJOB'             TO ERR-FNAME                   46360002
421400        MOVE FILE-STATUS           TO ERR-FSTAT                   46370002
421500        MOVE 'CANNOT OPEN ASGN FILE' TO ERR-DESC                  46380002
421600        GO TO P9999-GOT-PROBLEM                                   46390002
421700     END-IF                                                       46400002
421800                                                                  46410002
421900     OPEN INPUT CONTROL-FILE                                      46420002
422000     IF NOT SUCCESS                                               46430002
422100        MOVE CNTL-FS-KEY           TO ERR-KEY                     46440002
422200        MOVE 'CNTLKEY'             TO ERR-FNAME                   46450002
422300        MOVE FILE-STATUS           TO ERR-FSTAT                   46460002
422400        MOVE 'CANNOT OPEN CONTROL FILE' TO ERR-DESC               46470002
422500        GO TO P9999-GOT-PROBLEM                                   46480002
422600     END-IF                                                       46490002
422700                                                                  46500002
422800     OPEN INPUT EB-FILE                                           46510002
422900     IF NOT SUCCESS                                               46520002
423000        MOVE 'EBPOS'               TO ERR-FNAME                   46530002
423100        MOVE EBPOS-FS-KEY          TO ERR-KEY                     46540002
423200        MOVE FILE-STATUS           TO ERR-FSTAT                   46550002
423300        MOVE 'CANNOT OPEN EXTRABOARD FILE' TO ERR-DESC            46560002
423400        GO TO P9999-GOT-PROBLEM                                   46570002
423500     END-IF                                                       46580002
423600                                                                  46590002
423700     OPEN INPUT MASTER-FILE                                       46600002
423800     IF NOT SUCCESS                                               46610002
423900        MOVE MSTR-FS-NBRK          TO ERR-KEY                     46620002
424000        MOVE 'MSTRNBRK'            TO ERR-FNAME                   46630002
424100        MOVE FILE-STATUS           TO ERR-FSTAT                   46640002
424200        MOVE 'CANNOT OPEN MASTER FILE' TO ERR-DESC                46650002
424300        GO TO P9999-GOT-PROBLEM                                   46660002
424400     END-IF                                                       46670002
424500                                                                  46680002
424600     OPEN INPUT SEN-FILE                                          46690002
424700     IF NOT SUCCESS                                               46700002
424800        MOVE SEN-FS-KEY1           TO ERR-KEY                     46710002
424900        MOVE 'SENKEY1'             TO ERR-FNAME                   46720002
425000        MOVE FILE-STATUS           TO ERR-FSTAT                   46730002
425100        MOVE 'CANNOT OPEN SENIORITY FILE' TO ERR-DESC             46740002
425200        GO TO P9999-GOT-PROBLEM                                   46750002
425300     END-IF                                                       46760002
425400                                                                  46770002
425500     OPEN INPUT TRCN-FILE                                         46780002
425600     IF NOT SUCCESS                                               46790002
425700        MOVE 'TRCNFILE'            TO ERR-FNAME                   46800002
425800        MOVE FILE-STATUS           TO ERR-FSTAT                   46810002
425900        MOVE 'CANNOT OPEN TRCN FILE' TO ERR-DESC                  46820002
426000        GO TO P9999-GOT-PROBLEM                                   46830002
426100     END-IF                                                       46840002
426200                                                                  46850002
426300     OPEN INPUT UFP-FILE                                          46860002
426400     IF NOT SUCCESS                                               46870002
426500        MOVE UFPPOS-FS-KEY         TO ERR-KEY                     46880002
426600        MOVE 'UFPPOS'              TO ERR-FNAME                   46890002
426700        MOVE FILE-STATUS           TO ERR-FSTAT                   46900002
426800        MOVE 'CANNOT OPEN POOL FILE' TO ERR-DESC                  46910002
426900        GO TO P9999-GOT-PROBLEM                                   46920002
427000     END-IF                                                       46930002
427100                                                                  46940002
427200     OPEN INPUT SWASSGN-FILE                                      46950002
427300     IF NOT SUCCESS                                               46960002
427400        MOVE SWJOB-FS-KEY          TO ERR-KEY                     46970002
427500        MOVE 'SWJOBKEY'            TO ERR-FNAME                   46980002
427600        MOVE FILE-STATUS           TO ERR-FSTAT                   46990002
427700        MOVE 'CANNOT OPEN SWASSGN FILE' TO ERR-DESC               47000002
427800        GO TO P9999-GOT-PROBLEM                                   47010002
427900     END-IF.                                                      47020002
428000*CNC0516-BEG                                                      47030042
428100     OPEN INPUT TASK-FILE                                         47040042
428200     IF NOT SUCCESS                                               47050042
428300        MOVE TASKEMP-FS-KEY        TO ERR-KEY                     47060042
428400        MOVE 'TASKEMPKEY'          TO ERR-FNAME                   47070043
428500        MOVE FILE-STATUS           TO ERR-FSTAT                   47080042
428600        MOVE 'CANNOT OPEN TASK FILE' TO ERR-DESC                  47090042
428700        GO TO P9999-GOT-PROBLEM                                   47100042
428800     END-IF.                                                      47110042
428900     OPEN INPUT MASTER-FILE2                                      47120056
429000     IF NOT SUCCESS                                               47130056
429100        MOVE MSTR2-FS-NBRK         TO ERR-KEY                     47140056
429200        MOVE 'MSTR2NBRK'           TO ERR-FNAME                   47150056
429300        MOVE FILE-STATUS           TO ERR-FSTAT                   47160056
429400        MOVE 'CANNOT OPEN MASTER2 FILE' TO ERR-DESC               47170056
429500        GO TO P9999-GOT-PROBLEM                                   47180056
429600     END-IF.                                                      47190056
429700*CNC0516-END                                                      47200042
429800                                                                  47210042
429900 P9501-CLOSE-FILES.                                               47220002
430000                                                                  47230002
430100     MOVE 'P9501-01' TO ERR-PARAGRAPH                             47240002
430200     CLOSE PL                                                     47250002
430300     CLOSE AJ-FILE                                                47260002
430400     CLOSE JS-FILE                                                47270002
430500     CLOSE ASGN-FILE                                              47280002
430600     CLOSE CONTROL-FILE                                           47290002
430700     CLOSE EB-FILE                                                47300002
430800     CLOSE MASTER-FILE                                            47310002
430900     CLOSE SEN-FILE                                               47320002
431000     CLOSE TRCN-FILE                                              47330002
431100     CLOSE UFP-FILE                                               47340002
431200     CLOSE SWASSGN-FILE.                                          47350002
431300*CNC0516-BEG                                                      47360042
431400     CLOSE TASK-FILE.                                             47370042
431500     CLOSE MASTER-FILE2.                                          47380056
431600*CNC0516-END                                                      47390042
431700                                                                  47400002
431800*                                                                 47410002
431900 P9820-GET-CURRENT-TIME.                                          47420002
432000*                                                                 47430002
432100     ACCEPT WS-SYSTEM-DATE      FROM DATE                         47440002
432200     ACCEPT WS-SYSTEM-TIME-AREA FROM TIME                         47450002
432300     COPY ABSTIMEB.                                               47460002
432400*                                                                 47470002
432500*    INSTALL APPLICATION DATE/TIME                                47480002
432600*                                                                 47490002
432700     IF WS-DATE-TIME-OFFSET > SPACES                              47500002
432800        MOVE ZEROS            TO DATE-CONVERSION-PARMS            47510002
432900        MOVE WS-SYSTEM-DATE   TO PARM-PRI-DATE-GREG               47520002
433000        MOVE WS-SYSTEM-TIME   TO PARM-PRI-HRMN                    47530002
433100        PERFORM P9810-PROCESS-OFFSET                              47540002
433200        MOVE PARM-RES-DATE-GREG                                   47550002
433300                            TO WS-SYSTEM-DATE                     47560002
433400        MOVE PARM-RES-GREG-CENT                                   47570002
433500                            TO WS-SYSTEM-CENT                     47580002
433600        MOVE PARM-RES-HRMN                                        47590002
433700                            TO WS-SYSTEM-TIME                     47600002
433800     END-IF                                                       47610002
433900*                                                                 47620002
434000*    CONVERT SYSTEM TIME TO LOCAL TIME                            47630002
434100*                                                                 47640002
434200     MOVE SPACES                        TO WS-CNTL-FILE           47650002
434300     SET SUB-DIST-TYPE-REC              TO TRUE                   47660002
434400     MOVE LINK-DIST                     TO CNTL-DIST              47670002
434500     MOVE LINK-SUB-DIST                 TO CNTL-SUB-DIST          47680002
434600     MOVE CNTLKEY-AREA                  TO CNTL-FS-KEY            47690002
434700     PERFORM P8000-READ-CNTLFILE                                  47700002
434800     IF NOT SUCCESS                                               47710002
434900        MOVE 'P9820-1'                  TO ERR-PARAGRAPH          47720002
435000        MOVE CNTL-FS-KEY                TO ERR-KEY                47730002
435100        MOVE FILE-STATUS                TO ERR-FSTAT              47740002
435200        MOVE 'CNTLKEY'                  TO ERR-FNAME              47750002
435300        PERFORM P9999-GOT-PROBLEM                                 47760002
435400     END-IF                                                       47770002
435500                                                                  47780002
435600     MOVE SPACES                        TO TZ-PARAMETERS          47790002
435700     SET TZ-IN-EASTERN-ZONE             TO TRUE                   47800002
435800     MOVE WS-PRESENT-TIME               TO TZ-IN-DATE-TIME        47810002
435900     MOVE CNTL-TIME-ZONE                TO TZ-OUT-ZONE            47820002
436000     PERFORM P8996-TIMEZONE                                       47830002
436100     MOVE TZ-OUT-DATE-TIME-CENT         TO WS-LOCAL-DATE-TIME-CENT47840002
436200     .                                                            47850002
436300*CNC0516-BEG                                                      47860047
436400 P9830-RETRIEVE-CNTL-INFO.                                        47870047
436500*                                                                 47880047
436600     MOVE SPACES                     TO P956-COMMAREA-PARMS       47890047
436700     MOVE LAYOFF-CODE-1 OF WS-MSTR   TO P956-STATUS-CODE          47900047
436800     SET P956-GET-CNTL-STATUS-REASON TO TRUE                      47910047
436900     MOVE LAYOFF-EM-CODE OF WS-MSTR  TO P956-REASON-CODE          47920047
437000     MOVE DIST     OF WS-MSTR        TO P956-DIST                 47930047
437100     MOVE SUB-DIST OF WS-MSTR        TO P956-SDIST                47940047
437200     MOVE CRAFT OF WS-MSTR           TO P956-CC                   47950047
437300     IF TEMPORARY-ASGNMT > SPACE                                  47960047
437400        MOVE TEMPORARY-ASGNMT-FLAG   TO P956-ASGN-TYPE            47970047
437500        MOVE TA-1                    TO P956-ASGN                 47980047
437600        MOVE TA-DIST                 TO P956-DIST                 47990047
437700        MOVE TA-SUB-DIST             TO P956-SDIST                48000047
437800        IF TEMP-ASGN-XB                                           48010047
437900           MOVE TA-CC                TO P956-XB                   48020047
438000        END-IF                                                    48030047
438100     ELSE                                                         48040047
438200        IF NORMAL-ASGNMT > SPACES                                 48050047
438300           MOVE NORMAL-ASGNMT-FLAG   TO P956-ASGN-TYPE            48060047
438400           MOVE NA-1                 TO P956-ASGN                 48070047
438500           MOVE NA-DIST              TO P956-DIST                 48080047
438600           MOVE NA-SUB-DIST          TO P956-SDIST                48090047
438700           IF NORM-ASGN-XB                                        48100047
438800              MOVE NA-CC             TO P956-XB                   48110047
438900           END-IF                                                 48120047
439000        END-IF                                                    48130047
439100     END-IF                                                       48140047
439200     CALL P856-PGM USING P956-COMMAREA-PARMS.                     48150047
439300*CNC0516-END                                                      48160047
439310 P9840-RETRIEVE-CNTL-INFO.                                        48170060
439320*                                                                 48180060
439330     MOVE SPACES                     TO P956-COMMAREA-PARMS       48190060
439331     SET P956-GET-CNTL-STATUS-REASON TO TRUE                      48200060
439340     MOVE WS-E95-CODE                TO P956-STATUS-CODE          48210060
439360     MOVE WS-E95-EM-CODE             TO P956-REASON-CODE          48220060
439370     MOVE DIST     OF WS-MSTR        TO P956-DIST                 48230060
439380     MOVE SUB-DIST OF WS-MSTR        TO P956-SDIST                48240060
439390     MOVE CRAFT OF WS-MSTR           TO P956-CC                   48250060
439391     IF TEMPORARY-ASGNMT > SPACE                                  48260060
439392        MOVE TEMPORARY-ASGNMT-FLAG   TO P956-ASGN-TYPE            48270060
439393        MOVE TA-1                    TO P956-ASGN                 48280060
439394        MOVE TA-DIST                 TO P956-DIST                 48290060
439395        MOVE TA-SUB-DIST             TO P956-SDIST                48300060
439396        IF TEMP-ASGN-XB                                           48310060
439397           MOVE TA-CC                TO P956-XB                   48320060
439398        END-IF                                                    48330060
439399     ELSE                                                         48340060
439400        IF NORMAL-ASGNMT > SPACES                                 48350060
439401           MOVE NORMAL-ASGNMT-FLAG   TO P956-ASGN-TYPE            48360060
439402           MOVE NA-1                 TO P956-ASGN                 48370060
439403           MOVE NA-DIST              TO P956-DIST                 48380060
439404           MOVE NA-SUB-DIST          TO P956-SDIST                48390060
439405           IF NORM-ASGN-XB                                        48400060
439406              MOVE NA-CC             TO P956-XB                   48410060
439407           END-IF                                                 48420060
439408        END-IF                                                    48430060
439409     END-IF                                                       48440060
439410     CALL P856-PGM USING P956-COMMAREA-PARMS.                     48450060
439420*                                                                 48460002
439500 P9999-GOT-PROBLEM.                                               48470002
439600                                                                  48480002
439700     DISPLAY  WS-ERROR-INFO UPON SYSOUT                           48490002
439800     STOP RUN.                                                    48500002
439900                                                                  48510002
