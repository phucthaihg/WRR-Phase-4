000100******************************************************************00010001
000200***               (WSMSTR) EMPLOYEE MASTER FILE                   00020001
000300***                     RECORD LENGTH = 500                       00030001
000400******************************************************************00040001
000500*                                                                 00050001
000600 01  MSTR-VIA-EMP-NBR              PIC X(8)  VALUE 'CNVMSC01'.    00060001
000700 01  MSTRENBR-RLGTH                PIC S9(4) COMP VALUE +500.     00070001
000800 01  MSTRENBR-KLGTH                PIC S9(4) COMP VALUE +9.       00080001
000900 01  MSTATK1                       PIC XX.                        00090001
001000 01  MSTR-EMP-NBR-KEY.                                            00100001
001100     02  MSTRNBRK                  PIC X(9).                      00110001
001200*                                                                 00120001
001300 01  MSTR-VIA-EMP-NAME             PIC X(8)  VALUE 'CNVMSP01'.    00130001
001400 01  MSTRENAM-RLGTH                PIC S9(4) COMP VALUE +500.     00140001
001500 01  MSTRENAM-KLGTH                PIC S9(4) COMP VALUE +35.      00150001
001600 01  MSTATK2                       PIC XX.                        00160001
001700 01  MSTR-EMP-NAME-KEY.                                           00170001
001800     02  MSTREMPK                  PIC X(35).                     00180001
001900*                                                                 00190001
002000 01  MSTR-VIA-DIST-SDIST-CRFT-EMP  PIC X(8)  VALUE 'CNVMSP02'.    00200001
002100 01  MSTRDSCE-RLGTH                PIC S9(4) COMP VALUE +500.     00210001
002200 01  MSTRDSCE-KLGTH                PIC S9(4) COMP VALUE +41.      00220001
002300 01  MSTATK3                       PIC XX.                        00230001
002400 01  MSTR-DIST-SDIST-CRFT-EMP-KEY.                                00240001
002500     02  MSTRDSCEK                 PIC X(41).                     00250001
002600******************************************************************00260001
002700***         (WSMSTR) EMPLOYEE MASTER FILE RECORD LAYOUT           00270001
002800******************************************************************00280001
002900 01  WS-MSTR.                                                     00290001
003000     02  DIST-SDIST-CRFT-EMPNO-KEY.                               00300001
003100         04  DIST                      PIC XX.                    00310001
003200         04  SUB-DIST                  PIC XX.                    00320001
003300         04  CRAFT                     PIC XX.                    00330001
003400             88  CONDUCTOR    VALUE 'CO' 'B1' THRU 'B9'.          00340001
003500             88  ENGINEER     VALUE 'EN' 'EY' 'E1' THRU 'E9'.     00350001
003600             88  BRAKEMAN     VALUE 'BK' 'B1' THRU 'B9'.          00360001
003700             88  FIREMAN      VALUE 'FI'.                         00370001
003800             88  SWITCHMAN    VALUE 'SW' 'FO' 'ST' 'S1' THRU 'S9'.00380001
003900             88  HOSTLER      VALUE 'HO' 'HH'.                    00390001
004000             88  YARDMASTER   VALUE 'YA'.                         00400001
004100         04  EMP-NAME-NBR-KEY.                                    00410001
004200             06  EMP-NAME              PIC X(26).                 00420001
004300             06  EMP-NBR-KEY.                                     00430001
004400                 08  EMP-NBR           PIC X(9).                  00440001
004500     02  MSTR-IVR-SAVE-DEC-KEY         PIC X(20).                 00450001
004600     02  MSTR-IVR-SAVE-LAST-POS        PIC X(28).                 00460001
004700     02  MSTR-IVR-SAVE-LAST-SEN        PIC X(08).                 00470001
004800     02  MSTR-IVR-SAVE-LAST-STARTS     PIC X(02).                 00480001
004900     02  MSTR-LAST-LAYOFF-CODE         PIC X(2).                  00490001
005000     02  MSTR-LAST-STATUS-REASON       PIC X(2).                  00500001
005010     02  MSTR-FUTURE-BKOFF-MSG-DATE    PIC X(6).                  00501001
005100     02  FILLER                        PIC X(12).                 00502001
005200     02  LAST-ON-DUTY-TIME.                                       00503001
005300         04  ODT-DATE.                                            00504001
005400             06  ODT-YEAR              PIC 99.                    00505001
005500             06  ODT-MONTH             PIC 99.                    00506001
005600             06  ODT-DAY               PIC 99.                    00507001
005700         04  ODT-TIME-OF-DAY.                                     00508001
005800             06  ODT-HOUR              PIC 99.                    00509001
005900             06  ODT-MINUTE            PIC 99.                    00510001
006000     02  LAST-ARRIVAL-TIME.                                       00520001
006100         04  AT-DATE.                                             00530001
006200             06  AT-YEAR               PIC 99.                    00540001
006300             06  AT-MONTH              PIC 99.                    00550001
006400             06  AT-DAY                PIC 99.                    00560001
006500         04  AT-TIME-OF-DAY.                                      00570001
006600             06  AT-HOUR               PIC 99.                    00580001
006700             06  AT-MINUTE             PIC 99.                    00590001
006800     02  LAST-TIE-UP-TIME.                                        00600001
006900         04  TUT-DATE.                                            00610001
007000             06  TUT-YEAR              PIC 99.                    00620001
007100             06  TUT-MONTH             PIC 99.                    00630001
007200             06  TUT-DAY               PIC 99.                    00640001
007300         04  TUT-TIME-OF-DAY.                                     00650001
007400             06  TUT-HOUR              PIC 99.                    00660001
007500             06  TUT-MINUTE            PIC 99.                    00670001
007600     02  FILLER                        PIC X(01).                 00680001
007700     02  LAST-SUB-STATUS               PIC X(01).                 00690001
007800     02  MSTR-EMP-INITIALS             PIC X(03).                 00700001
007900     02  MSTR-HOS-GROUP                PIC X(01).                 00710001
007910         88  MSTR-HOS-GROUP-1                  VALUES '1'.        00720001
007920         88  MSTR-HOS-GROUP-2                  VALUES '2'.        00730001
007930     02  MSTR-CD-MESSAGE-FLAG          PIC X(01).                 00740001
007940         88  MSTR-CD-PIN-REQUIRED              VALUE 'P'.         00750001
007950     02  PAY-AS-FLAG                   PIC X(01).                 00760001
007960         88  PAY-AS-US                         VALUE 'U'.         00770001
007970         88  PAY-AS-CAN                        VALUES 'C' ' '.    00780001
007980     02  EMP-YARDMASTER-SENIORITY-FLAG PIC 9(02).                 00790001
007990         88  EMP-YARDMASTER-SENIORITY        VALUES 01 THRU 99.   00791001
008000     02  EMP-OVT-STARTS-EXCPT-FLAG     PIC X(01).                 00792001
008100         88  EMP-RETAIN-OVT-STARTS             VALUES '1'.        00793001
008200         88  EMP-RESET-OVT-STARTS              VALUES '2'.        00794001
008300     02  EMP-STT-STARTS-EXCPT-FLAG     PIC X(01).                 00795001
008400         88  EMP-RETAIN-STT-STARTS             VALUES '1'.        00796001
008500         88  EMP-RESET-STT-STARTS              VALUES '2'.        00797001
008600         88  EMP-ADD-STT-STARTS                VALUES '3'.        00798001
008700     02  MSTR-WAIT-TURN-FLAG           PIC X(01).                 00799001
008800         88  MSTR-PENDED-WAIT-TURN             VALUE  '#'.        00800001
008900     02  LAST-JOB-ASG-TYPE             PIC X(01).                 00810001
009000         88  LAST-JOB-ASG-UFP-JOB              VALUE 'U'.         00820001
009100         88  LAST-JOB-ASG-AJ-JOB               VALUE 'A'.         00830001
009200         88  LAST-JOB-ASG-XB-JOB               VALUE 'X'.         00840001
009300     02  PERS-LEAVE-AREA.                                         00850001
009400         04  NO-OF-PERSONAL-DAYS-1     PIC 99.                    00860001
009500         04  NO-OF-DAYS-OFF-1          PIC 99.                    00870001
009600         04  NO-OF-PERSONAL-DAYS-2     PIC 99.                    00880001
009700         04  NO-OF-DAYS-OFF-2          PIC 99.                    00890001
009800     02  PLA-X  REDEFINES PERS-LEAVE-AREA.                        00900001
009900         04  PLA-Z  OCCURS 2 TIMES.                               00910001
010000             06  NOPD.                                            00920001
010100                 08  NOPD-NUM          PIC 99.                    00930001
010200             06  NODO.                                            00940001
010300                 08  NODO-NUM          PIC 99.                    00950001
010400     02  PAGER-NO                      PIC X(6).                  00960001
010500     02  UNAVAILABLE-LAST-FLAG         PIC X.                     00970001
010600         88  LAST-STATUS-WAS-UNAVAILABLE       VALUES 'C' 'D' 'E' 00980001
010700                                                      'F' 'G' 'H' 00990001
010800                                                      'I' 'J' 'K' 01000001
010900                                                      'L' 'M' 'N' 01010001
011000                                                      'O' 'P' 'Q' 01020001
011100                                                      'R' 'S' 'T' 01030001
011200                                                      'U' 'V' 'W' 01040001
011300                                                      'X' 'Y' 'Z'.01050001
011400     02  LAYOFF-CODE.                                             01060001
011500         04  LAYOFF-CODE-1             PIC X.                     01070001
011600             88  AVAILABLE                     VALUE 'A'.         01080001
011700             88  WORKING                       VALUE 'B'.         01090001
011800             88  TO-PLACE                      VALUE 'C'.         01100001
011900             88  BEREAVEMENT                   VALUE 'D'.         01110001
012000             88  EXCUSED-ABSENCE               VALUE 'E'.         01120001
012100             88  OFF-DUTY-INJURY               VALUE 'F'.         01130001
012200             88  ON-DUTY-INJURY                VALUE 'G'.         01140001
012300             88  HELD-OUT-OF-SERVICE           VALUE 'H'.         01150001
012400             88  INVESTIGATION                 VALUE 'I'.         01160001
012500             88  JURY-DUTY                     VALUE 'J'.         01170001
012600             88  ABSENT-WITHOUT-LEAVE          VALUE 'K'.         01180001
012700             88  LEAVE-OF-ABSENCE              VALUE 'L'.         01190001
012800             88  OFF-MILES-DAYS                VALUE 'M'.         01200001
012900             88  COMPANY-BUSINESS              VALUE 'N'.         01210001
013000             88  PERSONAL-BUSINESS             VALUE 'O'.         01220001
013100             88  PERSONAL-LEAVE-DAY            VALUE 'P'.         01230001
013200             88  HOLD-FOR-VCNY                 VALUE 'Q'.         01240001
013300             88  TERMINATION                   VALUE 'R'.         01250001
013400             88  SICK                          VALUE 'S'.         01260001
013500             88  OFF-TRAINING                  VALUE 'T'.         01270001
013600             88  UNION-BUSINESS                VALUE 'U'.         01280001
013700             88  VACATION                      VALUE 'V'.         01290001
013800             88  FURLOUGHED                    VALUE 'W'.         01300001
013900             88  MISSED-CALL                   VALUE 'X'.         01310001
014000             88  TRANSFER-TO-NON-RUN-CRAFT     VALUE 'Y'.         01320001
014100             88  STRIKE                        VALUE 'Z'.         01330001
014200             88  WAIT-TURN                     VALUE '#'.         01340001
014300         04  LAYOFF-CODE-2             PIC X.                     01350001
014400             88  NORMAL                        VALUE '0', '9'.    01360001
014500             88  NOT-NOTIFIED                  VALUE '1'.         01370001
014600             88  ON-CALL                       VALUE '2'.         01380001
014700             88  IVR-CALL-IN-PROCESS           VALUE '8'.         01390001
014800             88  NOT-NOTIFIED-AWARD            VALUE '9'.         01400001
014900     02  MSTR-CONS-MISSED-CALLS        PIC XX.                    01410001
015000     02  MSTR-CONS-MISSED-CALLS-NUM REDEFINES                     01420001
015100         MSTR-CONS-MISSED-CALLS        PIC 99.                    01430001
015200     02  LAYOFF-TIME.                                             01440001
015300         04  LAYOFF-DATE.                                         01450001
015400             06  LAYOFF-YEAR           PIC 99.                    01460001
015500             06  LAYOFF-MONTH          PIC 99.                    01470001
015600             06  LAYOFF-DAY            PIC 99.                    01480001
015700         04  LAYOFF-TIME-OF-DAY.                                  01490001
015800             06  LAYOFF-HOUR           PIC 99.                    01500001
015900             06  LAYOFF-MINUTE         PIC 99.                    01510001
016000     02  MARKUP-TIME.                                             01520001
016100         04  MARKUP-YEAR               PIC 99.                    01530001
016200         04  MARKUP-MONTH              PIC 99.                    01540001
016300         04  MARKUP-DAY                PIC 99.                    01550001
016400         04  MARKUP-TIME-OF-DAY.                                  01560001
016500             06  MARKUP-HOUR           PIC 99.                    01570001
016600             06  MARKUP-MINUTE         PIC 99.                    01580001
016700     02  LAST-TRAIN-SYMBOL             PIC X(10).                 01590001
016800     02  LAST-JOB-ASG                  PIC X(12).                 01600001
016900     02  SAVE-PREV-DUTY                PIC X(4).                  01610001
017000     02  SAVE-PREV-ODT                 PIC X(10).                 01620001
017100     02  EMP-REST-DAY-ARRAY.                                      01630001
017200         04  EMP-REST-DAY  OCCURS 2 TIMES  PIC X.                 01640001
017300     02  EMP-REST-DAY-NUM-ARRAY REDEFINES EMP-REST-DAY-ARRAY.     01650001
017400         04  EMP-REST-DAY-NUM OCCURS 2 TIMES PIC 9.               01660001
017500     02  EMP-DH-CODE                   PIC X.                     01670001
017600         88 EMP-CDS                             VALUE 'C'.        01680001
017700     02  EMP-EMERGENCY-COND-CODE       PIC X.                     01690001
017800         88 EMP-KILLER-B                        VALUE 'B'.        01700001
017900     02  EMP-TRHIST-KEY                PIC X(26).                 01710001
018000                                                                  01720001
018100     02  MSTR-LAST-VACANCY-REJECTED                               01730001
018200         REDEFINES EMP-TRHIST-KEY.                                01740001
018300         04  MSTR-LVR-TRAIN            PIC X(10).                 01750001
018400         04  MSTR-LVR-DATE-TIME.                                  01760001
018500             06  MSTR-LVR-DATE         PIC X(6).                  01770001
018600             06  MSTR-LVR-TIME         PIC X(4).                  01780001
018700         04  MSTR-LVR-CRAFT            PIC X(2).                  01790001
018800         04  MSTR-LVR-FUNC             PIC X(4).                  01800001
018900     02  EMP-RLV-DTY                   PIC X(10).                 01810001
019000     02  EMP-HOME-TERM-CODE            PIC X(5).                  01820001
019100     02  EMP-HOME-TERM-CODE-NUM REDEFINES                         01830001
019200                 EMP-HOME-TERM-CODE    PIC 9(5).                  01840001
019300     02  EMP-HOME-ONLY-FLAG            PIC X.                     01850001
019400         88 EMP-HOME-ONLY-REQUEST          VALUE 'Y'.             01860001
019500***  02  FILLER                        PIC X.                     01870001
019600***  TEMP - REMOVE SHOW UP FLAG AND UNCOMMENT FILLER, ABOVE,      01880001
019700***  AFTER TESTING CNC0436C CHANGES.                              01890001
019800     02  SHOW-UP-FLAG                  PIC X.                     01900001
019900         88 SHOW-UP-RECORD-EXISTS                VALUE 'S'.       01910001
020000     02  PROTECTION-LOST-FLAG          PIC X.                     01920001
020100         88 EMP-HAS-LOST-PROTECTION              VALUE 'L'.       01930001
020200     02  OVER-TIME-FLAGS.                                         01940001
020300         04  CURRENT-JOB-OT            PIC X.                     01950001
020400             88  CURRENT-JOB-IS-OT               VALUE '1'.       01960001
020500         04  PREVIOUS-JOB-OT           PIC X.                     01970001
020600             88  PREVIOUS-JOB-WAS-OT             VALUE '1'.       01980001
020700     02  EMP-MILES-DATE.                                          01990001
020800         04  EMP-MILES-DATE-NUM        PIC 99.                    02000001
020900     02  EMP-MILES-PERIOD.                                        02010001
021000         04  EMP-MILES-YY              PIC 99.                    02020001
021100         04  EMP-MILES-MM              PIC 99.                    02030001
021200         04  EMP-MILES-DD              PIC 99.                    02040001
021300     02  EMP-IC-EMP-NO                 PIC X(6).                  02050001
021400     02  EMP-MTOD.                                                02060001
021500         04  EMP-MTOD-NUM              PIC 9(10).                 02070001
021600         04  FILLER                                               02080001
021700             REDEFINES EMP-MTOD-NUM.                              02090001
021800             05  EMP-MTOD-DATE         PIC X(6).                  02100001
021900             05  EMP-MTOD-TIME         PIC X(4).                  02110001
022000     02  EMP-PREV-DUTY-MTOD.                                      02120001
022100         04  EMP-PREV-DUTY-MTOD-NUM    PIC 9(4).                  02130001
022200     02  EMP-US-RSTD.                                             02140001
022300         04  EMP-US-RSTD-NUM           PIC 9(10).                 02150001
022400         04  FILLER                                               02160001
022500             REDEFINES EMP-US-RSTD-NUM.                           02170001
022600             05  EMP-US-RSTD-DATE      PIC X(6).                  02180001
022700             05  EMP-US-RSTD-TIME      PIC X(4).                  02190001
022800     02  EMP-PREV-DUTY.                                           02200001
022900         04  EMP-PREV-DUTY-NUM         PIC 9(4).                  02210001
023000     02  EMP-PERS-REST.                                           02220001
023100         04  EMP-PERS-REST-NUM         PIC 9(10).                 02230001
023200         04  FILLER                                               02240001
023300             REDEFINES EMP-PERS-REST-NUM.                         02250001
023400             05  EMP-PERS-REST-DATE    PIC X(6).                  02260001
023500             05  EMP-PERS-REST-TIME    PIC X(4).                  02270001
023600     02  EMP-LANG-PREF                 PIC X.                     02280001
023700     02  EMP-TITLE                     PIC X.                     02290001
023800     02  DOS-STARTED-TIME.                                        02300001
023900         04  DOS-STARTED-DATE.                                    02310001
024000             06  DOS-STARTED-YEAR      PIC 99.                    02320001
024100             06  DOS-STARTED-MONTH     PIC 99.                    02330001
024200             06  DOS-STARTED-DAY       PIC 99.                    02340001
024300         04  DOS-STARTED-TIME-OF-DAY.                             02350001
024400             06  DOS-STARTED-HOUR      PIC 99.                    02360001
024500             06  DOS-STARTED-MINUTE    PIC 99.                    02370001
024600     02  EMP-LAST-TOUR-FLAG            PIC X(01).                 02380001
024700         88  EMP-LAST-TOUR-OUT-OF-BOX      VALUE 'Y'.             02390001
024800     02  EMP-LAST-SVC                  PIC X(01).                 02400001
024900         88  EMP-LAST-SVC-ROAD             VALUE 'R'.             02410001
025000         88  EMP-LAST-SVC-YARD             VALUE 'Y'.             02420001
025100     02  PREV-MTOD-1.                                             02430001
025200         03  PREV-MTOD-1-NUM           PIC 9(04).                 02440001
025300     02  EMP-LAST-MTOD-AMT             PIC X(04).                 02450001
025400     02  SAVE-PREV-TU-TIME.                                       02460001
025500         03  SAVE-PREV-TU-DATE         PIC X(06).                 02470001
025600         03  SAVE-PREV-TU-HRMN         PIC X(04).                 02480001
025700     02  EMP-HI-LVL-RESTRICTION        PIC X.                     02490001
025800         88  EMP-HI-LVL-OK                       VALUE '*'.       02500001
025900     02  LAYOFF-EM-CODE                PIC X(02).                 02510001
026000     02  MSTR-EM-CODE                  PIC X(01).                 02520001
026100         88  EMP-FROM-EM                         VALUE '*'.       02530001
026200     02  EMP-FTV-DATE                  PIC X(06).                 02540001
026300     02  EMP-TRACK-MTOD-DATE-TIME      PIC X(10).                 02550001
026400     02  EMP-TRACK-US-RSTD-DATE-TIME   PIC X(10).                 02560001
026500     02  EMP-ALT-REST-REQUEST          PIC X(04).                 02570001
026600     02  EMP-ALT-REST REDEFINES                                   02580001
026700         EMP-ALT-REST-REQUEST          PIC 9(04).                 02590001
026800     02  EMP-ALT-TIE-UP-TIME.                                     02600001
026900         04  EMP-ALT-TU-CE             PIC X(2).                  02610001
027000         04  EMP-ALT-TU-DATE-TIME.                                02620001
027100            05  EMP-ALT-TU-DATE        PIC X(6).                  02630001
027200            05  EMP-ALT-TU-TIME        PIC X(4).                  02640001
027300     02  EMP-EXTENDED-RUN-INFO.                                   02650001
027400         04  EMP-XR-ASGN-TYPE          PIC X(01).                 02660001
027500             88  EMP-XR-UFP-JOB                  VALUE 'U'.       02670001
027600             88  EMP-XR-AJ-JOB                   VALUE 'A'.       02680001
027700             88  EMP-XR-XB-JOB                   VALUE 'X'.       02690001
027800             88  EMP-XR-NO-JOB                   VALUE 'N'.       02700001
027900         04  EMP-XR-ASSIGNMENT         PIC X(12).                 02710001
028000         04  EMP-XR-FLAG               PIC X(01).                 02720001
028100             88  EMP-ON-EXTENDED-RUN             VALUE 'X'.       02730001
028200     02  MSTR-NOTIFIED-DATE-TIME.                                 02740001
028300         04  MSTR-NOTIFIED-DATE-CENT.                             02750001
028400             05  MSTR-NOTIFIED-CENT    PIC X(02).                 02760001
028500             05  MSTR-NOTIFIED-DATE    PIC X(06).                 02770001
028600         04  MSTR-NOTIFIED-TIME        PIC X(04).                 02780001
028700     02  MSTR-NOTIFIED-ASSIGNMENT.                                02790001
028800         04  MSTR-NOTIFIED-ASGN-TYPE   PIC X(01).                 02800001
028900         04  MSTR-NOTIFIED-ASGN.                                  02810001
029000             05  MSTR-NOTIFIED-DIST    PIC X(02).                 02820001
029100             05  MSTR-NOTIFIED-SDIST   PIC X(02).                 02830001
029200             05  MSTR-NOTIFIED-ASSIGN  PIC X(06).                 02840001
029300             05  MSTR-NOTIFIED-CC      PIC X(02).                 02850001
029400*CNLD-309 B                                                       02860001
029400*    02  EMP-MTOY.                                                02870001
029500*        04  EMP-MTOY-NUM              PIC 9(10).                 02880001
029600*        04  FILLER                                               02890001
029700*            REDEFINES EMP-MTOY-NUM.                              02900001
029800*            05  EMP-MTOY-DATE         PIC X(6).                  02910001
029900*            05  EMP-MTOY-TIME         PIC X(4).                  02920001
030000*    02  EMP-MTOR.                                                02930001
030100*        04  EMP-MTOR-NUM              PIC 9(10).                 02940001
030200*        04  FILLER                                               02950001
030300*            REDEFINES EMP-MTOR-NUM.                              02960001
030400*            05  EMP-MTOR-DATE         PIC X(6).                  02970001
030500*            05  EMP-MTOR-TIME         PIC X(4).                  02980001
029400     02  FILLER                        PIC X(20).                 02990001
029400*CNLD-309 E                                                       03000001
030600     02  MSTR-SPRD-CALL-DATE-TIME.                                03010001
030700         04  MSTR-SPRD-CALL-DATE       PIC X(06).                 03020001
030800         04  MSTR-SPRD-CALL-TIME       PIC X(04).                 03030001
030900     02  MSTR-SPRD-CALL-ASGN           PIC X(10).                 03040001
031000     02  MSTR-MTOD-ADJUSTED-FLAG       PIC X(01).                 03050001
031100         88  MSTR-MTOD-CAN-ADJUST                VALUE ' '.       03060001
031200         88  MSTR-MTOD-ADJUSTED                  VALUE 'A'.       03070001
