000100******************************************************************00010000
000200***            (WSMSTR3) EMPLOYEE MASTER FILE 3                   00020000
000300***                     RECORD LENGTH = 600                       00030000
000400******************************************************************00040000
000500*                                                                 00050000
000600 01  MSTR3-VIA-EMP-NBR               PIC X(08) VALUE 'CNVMSC03'.  00060000
000700 01  MSTR3ENBR-RLGTH                 PIC S9(4) COMP VALUE +200.   00070000
000800 01  MSTR3ENBR-KLGTH                 PIC S9(4) COMP VALUE +9.     00080000
000900 01  M3STATK1                        PIC X(01).                   00090000
001000 01  MSTR3-EMP-NBR-KEY.                                           00100000
001100     02  MSTR3NBRK                   PIC X(09).                   00110000
001200*                                                                 00120000
001300******************************************************************00130000
001400***    (WSMSTR3) EMPLOYEE MASTER FILE 3 RECORD LAYOUT             00140000
001500******************************************************************00150000
001600 01  WS-MSTR3.                                                    00160000
001700     02  MSTR3-KEY.                                               00170000
001800         04  MSTR3-EMP-NBR              PIC 9(09).                00180000
001900     02  FILLER                         PIC X(05).                00190000
002000     02  MSTR3-CURRENT-BANKTIME         PIC 9(07).                00200000
002100     02  MSTR3-BANKTIME-CARRYOVER       PIC 9(07).                00210000
002200     02  FILLER                         PIC X(02).                00220000
002300     02  MSTR3-BCR-OT-PAY-YEAR          PIC X(04).                00230000
002400     02  MSTR3-BCR-OT-PAY-PERIOD        PIC X(02).                00240000
002500     02  MSTR3-BCR-BT-PAY-YEAR          PIC X(04).                00250000
002600     02  MSTR3-BCR-BT-PAY-PERIOD        PIC X(02).                00260000
002700     02  MSTR3-CURR-PLD-ROSTER          PIC X(08).                00270000
002800     02  MSTR3-CURR-PLD-YR              PIC X(04).                00280000
002900     02  MSTR3-CURR-PLD-DIST            PIC X(02).                00290000
003000     02  MSTR3-CURR-PLD-SUB-DIST        PIC X(02).                00300000
003100     02  MSTR3-CURR-PLD-CRAFT           PIC X(02).                00310000
003200     02  MSTR3-NEXT-PLD-ROSTER          PIC X(08).                00320000
003300     02  MSTR3-NEXT-PLD-YR              PIC X(04).                00330000
003400     02  MSTR3-NEXT-PLD-DIST            PIC X(02).                00340000
003500     02  MSTR3-NEXT-PLD-SUB-DIST        PIC X(02).                00350000
003600     02  MSTR3-NEXT-PLD-CRAFT           PIC X(02).                00360000
003700*CNC0600-B                                                        00370000
003800*    02  FILLER                         PIC X(122).               00380000
003900     02  MSTR3-7DAY-END-PERIOD.                                   00390000
004000         04  MSTR3-7DAY-END-PERIOD-NUM    PIC 9(10).              00400000
004100         04  MSTR3-7DAY-END-PERIOD-CH                             00410000
004200             REDEFINES MSTR3-7DAY-END-PERIOD-NUM.                 00420000
004300             05 MSTR3-7DAY-END-DATE       PIC X(06).              00430000
004400             05 MSTR3-7DAY-END-TIME       PIC X(04).              00440000
003900     02  MSTR3-PREV-7DAY-END.                                     00450000
004000         04  MSTR3-PREV-7DAY-END-NUM      PIC 9(10).              00460000
004100         04  MSTR3-PREV-7DAY-END-CH                               00470000
004200             REDEFINES MSTR3-PREV-7DAY-END-NUM.                   00480000
004300             05 MSTR3-PREV-7DAY-END-DATE  PIC X(06).              00490000
003700             05 MSTR3-PREV-7DAY-END-TIME  PIC X(04).              00500000
003800     02  MSTR3-PROJ-RESET-BRK.                                    00510000
003900         04  MSTR3-PROJ-RESET-BRK-NUM     PIC 9(10).              00520000
004000         04  MSTR3-PROJ-RESET-BRK-CH                              00530000
004100             REDEFINES MSTR3-PROJ-RESET-BRK-NUM.                  00540000
004200             05 MSTR3-PROJ-RESET-DATE     PIC X(06).              00550000
004300             05 MSTR3-PROJ-RESET-TIME     PIC X(04).              00560000
004400     02  MSTR3-PROJ-UD1-FROM.                                     00570000
003900         04  MSTR3-PROJ-UD1-FROM-NUM      PIC 9(10).              00580000
004000         04  MSTR3-PROJ-UD1-FROM-CH                               00590000
004100             REDEFINES MSTR3-PROJ-UD1-FROM-NUM.                   00600000
004200             05 MSTR3-PROJ-UD1-FROM-DATE  PIC X(06).              00610000
004300             05 MSTR3-PROJ-UD1-FROM-TIME  PIC X(04).              00620000
004400     02  MSTR3-PROJ-UD1-TO.                                       00630000
005800         04  MSTR3-PROJ-UD1-TO-NUM        PIC 9(10).              00640000
005900         04  MSTR3-PROJ-UD1-TO-CH                                 00650000
006000             REDEFINES MSTR3-PROJ-UD1-TO-NUM.                     00660000
006100             05 MSTR3-PROJ-UD1-TO-DATE    PIC X(06).              00670000
006200             05 MSTR3-PROJ-UD1-TO-TIME    PIC X(04).              00680000
006300     02  MSTR3-PROJ-UD2-FROM.                                     00690000
006400         04  MSTR3-PROJ-UD2-FROM-NUM      PIC 9(10).              00700000
006500         04  MSTR3-PROJ-UD2-FROM-CH                               00710000
006600             REDEFINES MSTR3-PROJ-UD2-FROM-NUM.                   00720000
006700             05 MSTR3-PROJ-UD2-FROM-DATE  PIC X(06).              00730000
006800             05 MSTR3-PROJ-UD2-FROM-TIME  PIC X(04).              00740000
006900     02  MSTR3-PROJ-UD2-TO.                                       00750000
007000         04  MSTR3-PROJ-UD2-TO-NUM        PIC 9(10).              00760000
007100         04  MSTR3-PROJ-UD2-TO-CH                                 00770000
007200         REDEFINES MSTR3-PROJ-UD2-TO-NUM.                         00780000
007300             05 MSTR3-PROJ-UD2-TO-DATE    PIC X(06).              00790000
007400             05 MSTR3-PROJ-UD2-TO-TIME    PIC X(04).              00800000
007500     02  MSTR3-SYSTEM-RESET-BRK.                                  00810000
007600         04  MSTR3-SYSTEM-RESET-BRK-NUM   PIC 9(10).              00820000
007700         04  MSTR3-SYSTEM-RESET-BRK-CH                            00830000
007800             REDEFINES MSTR3-SYSTEM-RESET-BRK-NUM.                00840000
007900             05 MSTR3-SYSTEM-RESET-DATE   PIC X(06).              00850000
008000             05 MSTR3-SYSTEM-RESET-TIME   PIC X(04).              00860000
008100     02  MSTR3-SYSTEM-UD1-FROM.                                   00870000
008200         04  MSTR3-SYSTEM-UD1-FROM-NUM    PIC 9(10).              00880000
008300         04  FILLER                                               00890000
008400             REDEFINES MSTR3-SYSTEM-UD1-FROM-NUM.                 00900000
008500             05 MSTR3-SYSTEM-UD1-FRM-DATE PIC X(06).              00910000
008600             05 MSTR3-SYSTEM-UD1-FRM-TIME PIC X(04).              00920000
008700     02  MSTR3-SYSTEM-UD1-TO.                                     00930000
008800         04  MSTR3-SYSTEM-UD1-TO-NUM      PIC 9(10).              00940000
008900         04  FILLER                                               00950000
009000             REDEFINES MSTR3-SYSTEM-UD1-TO-NUM.                   00960000
009100             05 MSTR3-SYSTEM-UD1-TO-DATE  PIC X(06).              00970000
009200             05 MSTR3-SYSTEM-UD1-TO-TIME  PIC X(04).              00980000
009300     02  MSTR3-SYSTEM-UD2-FROM.                                   00990000
009400         04  MSTR3-SYSTEM-UD2-FROM-NUM    PIC 9(10).              01000000
009500         04  FILLER                                               01010000
009600             REDEFINES MSTR3-SYSTEM-UD2-FROM-NUM.                 01020000
009700             05 MSTR3-SYSTEM-UD2-FRM-DATE PIC X(06).              01030000
009800             05 MSTR3-SYSTEM-UD2-FRM-TIME PIC X(04).              01040000
009900     02  MSTR3-SYSTEM-UD2-TO.                                     01050000
010000         04  MSTR3-SYSTEM-UD2-TO-NUM      PIC 9(10).              01060000
010100         04  FILLER                                               01070000
010200             REDEFINES MSTR3-SYSTEM-UD2-TO-NUM.                   01080000
010300             05 MSTR3-SYSTEM-UD2-TO-DATE  PIC X(06).              01090000
010400             05 MSTR3-SYSTEM-UD2-TO-TIME  PIC X(04).              01100000
010500     02  FILLER                           PIC X(002).             01110000
010600*CNC0600-E                                                        01120000
