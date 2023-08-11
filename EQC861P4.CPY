000100 01  EQC861P3.
000200     05  TAX-FORM-KEY.
000300         10  RECORD-ID                       PIC 9(03)   COMP-3.
000400             88  TAX-FORM-RECORD                 VALUE 861.
000500         10  TAX-YEAR                        PIC 9(04).
000600         10  FORM-TYPE                       PIC X(02).
000700             88  FORM-W2                         VALUE 'W2'.
000800             88  FORM-1099-DIV                   VALUE '9D'.
000900             88  FORM-1099-MISC                  VALUE '9M'.
001000             88  FORM-1099-R                     VALUE '9R'.
001100             88  FORM-5498                       VALUE '98'.
001200             88  FORM-1099-INT                   VALUE '9I'.
001300         10  TAPE-NUMBER                     PIC 9(03)   COMP-3.
001400         10  OWNER-USER.
001500             15  OWNER-ID                    PIC X(03).
001600             15  USER-ID                     PIC X(03).
001700         10  PAYER-NUMBER                    PIC 9(03)   COMP-3.
001800         10  CORRECTION-FLAG                 PIC X(01).
001900             88 CORRECTION-RECORD-TYPE-1         VALUE 'X'.
002000             88 CORRECTION-RECORD-TYPE-2         VALUE 'Z'.
002100         10  RECORD-TYPE                     PIC X.
002200             88  TAX-FORMS-PAYER-RECORD          VALUE 'A'.
002300             88  TAX-FORMS-PAYEE-RECORD          VALUE 'B'.
002400         10  FEDERAL-IDENTIFICATION-NUMBER   PIC 9(09)   COMP-3.
002500         10  KEY-INFORMATION                 PIC X(24).
002600         10  PAYER-KEY   REDEFINES   KEY-INFORMATION.
002700             15  USED-FIELDS                 PIC X(03).
002800             15  FILLER                      PIC X(01).
002900             15  TYPE-OF-RETURN              PIC X.
003000                 88  RETURN-1099-DIV             VALUE '1'.
003100                 88  RETURN-1099-INT             VALUE '6'.
003200                 88  RETURN-1099-MISC            VALUE 'A'.
003300                 88  RETURN-1099-R               VALUE '9'.
003400                 88  RETURN-5498                 VALUE 'L'.
003500                 88  RETURN-W2                   VALUE ' '.
003600             15  TRANSMITTER-CONTROL-CODE    PIC X(05).
003700                 88  DST-TCC                     VALUE '09070'.
003800             15  COMBINED-FEDERAL-STATE      PIC X.
003900                 88  IS-COMBINED                 VALUE '1'.
004000             15  TRANSFER-AGENT-INDICATOR    PIC X.
004100             15  AMOUNT-INDICATORS.
004200                 20  AMT-INDIC               PIC X
004300                         OCCURS 12  TIMES  INDEXED  BY  IND-INDEX.
004400                     88  VALID-AMT-IND       VALUES '1' '2' '3'
004500                                                    '4' '5' '6'
004600                                                    '7' '8' '9'
004700                                                    'A' 'B' 'C'.
004800         10  PAYEE-KEY   REDEFINES   KEY-INFORMATION.
004900             15  CONTROL-NUMBER              PIC X(20).
005000             15  FILLER-USED                 PIC X(01).
005100             15  FILLER                      PIC X(03).
005200         10  FILLER      REDEFINES PAYEE-KEY.
005300             15  USER-CODE-SHORT             PIC X(02).
005400             15  SYSTEM-INDICATOR            PIC X(01).
005500                 88  ANN                         VALUE 'A'.
005600                 88  DSS                         VALUE 'D'.
005700                 88  RPS                         VALUE 'R'.
005800                 88  VANTAGE                     VALUE 'V'.
005900                 88  OTHER-USER-SYSTEM           VALUES 'O' 'P'
006000                                                        'Q' '1'
006100                                                        '2' '3'.
006200             15  DST-CONTRACT.
006300                 20  CONTRACT-NUMBER             PIC 9(07).
006400                 20  PARTICIPANT-NUMBER          PIC 9(09).
006500             15  PSC-CONTRACT   REDEFINES  DST-CONTRACT.
006600                 20  PSC-CONTRACT-NUMBER         PIC X(16).
006700             15  FILLER         REDEFINES  DST-CONTRACT.
006800                 20  CONTRACT-1ST-CHAR           PIC X(01).
006900                     88  CONTRACT-1ST-CHAR-BLANK     VALUE ' '.
007000                 20  FILLER                      PIC X(15).
007100             15  DOCUMENT-SPECIFIC-CODE.
007200                 20  DISTRIBUTION-CODE-1     PIC X.
007300                 20  DISTRIBUTION-CODE-2     PIC X.
007400             15  FILLER                          PIC X(03).
007500         10  AGENT-KEY   REDEFINES   KEY-INFORMATION.
007600             15  SUPER-AGENT-CODE            PIC X(02).
007700             15  GENERAL-AGENT-NUMBER.
007800                 20  GA-NUMBER               PIC X(07).
007900                 20  GA-BRANCH               PIC X(03).
008000             15  WRITING-AGENT-NUMBER        PIC X(07).
008100             15  AGENT-BLANKIT               PIC X(02).
008200             15  FILLER                      PIC X(03).
008300     05  POLICY-RECIP-NBR                    PIC 9(2)  COMP-3.
008400     05  TAX-FORM-PHASE-NBR                  PIC 9(3)  COMP-3.
008500     05  IRS-1099R-DOC-CDE                   PIC X(2).
008600     05  FORM-TAPE-OPTION                    PIC X(1).
008700         88  FORM-ONLY                           VALUE 'F'.
008800         88  TAPE-ONLY                           VALUE 'T'.
008900         88  BOTH-FORM-AND-TAPE                  VALUE 'B'.
009000** PMW36100109010 *** 05/13/1999 ****** CT06165 ******* START ****
009100*    05  FILLER                              PIC X(9).
      ***** IMP57560 ****** 10/08/2002 ****** CT06000 ******* START ***
009200*    05  FILLER                              PIC X(6).
           05  PHASE-BYPASS-CDE                    PIC X(1).
           05  FILLER                              PIC X(5).
      ***** IMP57560 ****** 10/08/2002 ****** CT06000 *******  END  ***
009300     05  LINE-OF-BUSINESS                    PIC X(3).
009400** PMW36100109010 *** 05/13/1999 ****** CT06165 *******  END  ****
009500     05  FORM-SUPPRESSION-FLAG               PIC X(01).
009600         88  PRINT-THIS                          VALUE SPACE.
009700         88  DO-NOT-PRINT-THIS                   VALUE 'X'.
009800     05  NAME-CONTROL                        PIC X(04).
009900     05  NAME-1                              PIC X(40).
010000     05  NAME-2                              PIC X(40).
010100     05  NAME-3                              PIC X(40).
010200     05  STREET-ADDRESS                      PIC X(40).
010300     05  CITY-STATE-ZIP.
010400         10  TAX-CITY                        PIC X(40).
010500         10  FILLER                          PIC X.
010600** PMW36100073020 *** 07/15/1999 ****** CT06165 ******* START ****
010700*        10  TAX-STATE                       PIC X(02).
010800*        10  FILLER                          PIC X.
010900         10  TAX-STATE                       PIC X(03).
011000** PMW36100073020 *** 07/15/1999 ****** CT06165 *******  END  ****
011100         10  TAX-ZIP.
011200             15  TAX-ZIP-CODE                PIC 9(05).
011300             15  TAX-ZIP-DASH                PIC X.
011400             15  TAX-ZIP-ROUTE               PIC 9(04).
011500     05  FOREIGN-POSTAL-CODE                 PIC X(05).
011600     05  TAX-ZIP-LOCATION                    PIC X(02).
011700     05  IRA-SEP-SIMPLE-BOX.
011800         10  98-IRA-BOX                      PIC X(01).
011900         10  98-SEP-BOX                      PIC X(01).
012000         10  98-SIMPLE-BOX                   PIC X(01).
012100*        10  98-ROTH-CONV-BOX                PIC X(01).
009000** AMR90003217    *** 09/14/2001 ****** ZC00208 ******* START ****
      *        10  98-RECHAR-BOX                   PIC X(01).
009000** AMR90003217    *** 09/14/2001 ****** ZC00208 ******* END   ****
012200         10  98-ROTH-IRA-BOX                 PIC X(01).
      ***** Z006082 **** 59869 ******* 07/28/03 ** START **********
012300*    05  FILLER                              PIC X(02).
012300*    05  FILLER                              PIC X(03).
012300     05  FILLER                              PIC X(02).
011700     05  MRD-BOX.
012000         10  98-MRD-BOX                      PIC X(01).
      ***** Z006082 **** 59869 ******* 07/28/03 **  END  **********
012400     05  STANDARD-PAYEE-INFO.
012500         10  FOREIGN-NA-CODE                 PIC X.
012600             88  FOREIGN-NA                      VALUE 'F'.
012700             88  PROTECTORATE-NA                 VALUE 'P'.
012800             88  DOMESTIC-NA                     VALUE 'D'.
012900         10  SOCIAL-CODE                     PIC 9(03)    COMP-3.
013000         10  IRA-SEP-SIMPLE-FLAG             PIC X.
013100             88  IRA-SEP-SIMPLE                  VALUE 'X'.
013200         10  TYPE-OF-TIN                     PIC X.
013300             88  TYPE-EIN                        VALUE '1'.
013400             88  TYPE-SSN                        VALUE '2'.
013500         10  STATE-CODE                      PIC X(02).
013600         10  TAX-NOT-DETERMINED-BOX          PIC X(01).
013700             88  TAX-NOT-DETERMINED              VALUE 'X'.
013800         10  SPECIAL-DATA-ENTRIES            PIC X(67).
013900         10  TOTAL-DISTRIBUTION-BOX          PIC X(01).
014000             88  TOTAL-DISTRIBUTION              VALUE 'X'.
014100         10  FILLER                          PIC X(19).
014200     05  STANDARD-PAYER-INFO   REDEFINES STANDARD-PAYEE-INFO.
014300         10  FILLER                          PIC X(67).
014400         10  TYPE-OF-EMPLOYMENT              PIC X(01).
014500         10  PAYROLL-UNIT                    PIC X(04).
014600         10  LIMITATION-OF-LIABILITY-IND     PIC X(01).
014700         10  FOREIGN-CORPORATION-INDICATOR   PIC X(01).
014800             88  FOREIGN-CORPORATION             VALUE '1'.
014900         10  FILLER                          PIC X(21).
015000     05  REMAINING-INFORMATION               PIC X(207).
015100
015200     05  AMT-TABLE-AREA   REDEFINES   REMAINING-INFORMATION.
015300         10  AMOUNT-TABLE.
015400             15  TBL-AMOUNT   OCCURS   12  TIMES
015500                              INDEXED  BY  AMT-IDX
015600                                             PIC 9(16)V99 COMP-3.
015700         10  FILLER                          PIC X(87).
015800
015900     05  PAYER-RECORD-INFO   REDEFINES   REMAINING-INFORMATION.
016000         10  CONTROL-TL-1                    PIC 9(16)V99 COMP-3.
016100         10  CONTROL-TL-2                    PIC 9(16)V99 COMP-3.
016200         10  CONTROL-TL-3                    PIC 9(16)V99 COMP-3.
016300         10  CONTROL-TL-4                    PIC 9(16)V99 COMP-3.
016400         10  CONTROL-TL-5                    PIC 9(16)V99 COMP-3.
016500         10  CONTROL-TL-6                    PIC 9(16)V99 COMP-3.
016600         10  CONTROL-TL-7                    PIC 9(16)V99 COMP-3.
016700         10  CONTROL-TL-8                    PIC 9(16)V99 COMP-3.
016800         10  CONTROL-TL-9                    PIC 9(16)V99 COMP-3.
016900         10  CONTROL-TL-10                   PIC 9(16)V99 COMP-3.
017000         10  CONTROL-TL-11                   PIC 9(16)V99 COMP-3.
017100         10  CONTROL-TL-12                   PIC 9(16)V99 COMP-3.
017200         10  TL-PAYEES                       PIC 9(07)    COMP-3.
017300         10  TL-PARTICIPANTS                 PIC 9(07)    COMP-3.
017400         10  PAYER-GROSS-AMOUNT              PIC 9(16)V99 COMP-3.
017500         10  FILLER                          PIC X(69).
017600
017700     05  9D-PAYEE-RECORD-INFO   REDEFINES   REMAINING-INFORMATION.
017800         10  9D1-ORDINARY-DIVIDENDS          PIC 9(16)V99 COMP-3.
017900         10  9D2-TOTAL-CAP-GAIN-DISTR        PIC 9(16)V99 COMP-3.
018000         10  9D3-28-RATE-GAIN                PIC 9(16)V99 COMP-3.
018100         10  9D4-UNRECAP-SEC1250-GAIN        PIC 9(16)V99 COMP-3.
018200         10  9D5-SECTION-1202-GAIN           PIC 9(16)V99 COMP-3.
018300         10  9D6-NON-TAXABLE-DISTRIBUTION    PIC 9(16)V99 COMP-3.
018400         10  9D7-FEDERAL-INCOME-TAX-WH       PIC 9(16)V99 COMP-3.
018500         10  9D8-INVESTMENT-EXPENSES         PIC 9(16)V99 COMP-3.
018600         10  9D9-FOREIGN-TAX-PAID            PIC 9(16)V99 COMP-3.
018700         10  9D10-CASH-LIQUIDATIONS          PIC 9(16)V99 COMP-3.
018800         10  9D11-NON-CASH-LIQUIDATIONS      PIC 9(16)V99 COMP-3.
018900         10  9D12-NOT-USED                   PIC 9(16)V99 COMP-3.
019000         10  9D-NOT-USED                     PIC 9(7)V99  COMP-3.
019100         10  FILLER                          PIC X(82).
019200
019300     05  9M-PAYEE-RECORD-INFO   REDEFINES   REMAINING-INFORMATION.
019400         10  9M1-RENTS                       PIC 9(16)V99 COMP-3.
019500         10  9M2-ROYALTIES                   PIC 9(16)V99 COMP-3.
019600         10  9M3-AWARDS                      PIC 9(16)V99 COMP-3.
019700         10  9M4-FEDERAL-TAX-WITHHELD        PIC 9(16)V99 COMP-3.
019800         10  9M5-BOAT-PROCEEDS               PIC 9(16)V99 COMP-3.
019900         10  9M6-HEALTH-CARE-PAYMENTS        PIC 9(16)V99 COMP-3.
020000         10  9M7-NON-EMPLOYEE-COMPENSATION   PIC 9(16)V99 COMP-3.
020100         10  9M8-NOT-USED                    PIC 9(16)V99 COMP-3.
020200         10  9M9-NOT-USED                    PIC 9(16)V99 COMP-3.
020300         10  9M10-NOT-USED                   PIC 9(16)V99 COMP-3.
020400         10  9M11-EPP-CONTRIB                PIC 9(16)V99 COMP-3.
020500         10  9M12-ATTORNEY-PMT               PIC 9(16)V99 COMP-3.
020600         10  9M-STATE-TAX-WITHHELD           PIC 9(16)V99 COMP-3.
020700         10  9M-STATE-CODE                   PIC X(02).
020800         10  FILLER                          PIC X(18).
020900         10  9M-USER-CODE-SHORT              PIC X(02).
021000         10  9M-SYSTEM-INDICATOR             PIC X(01).
021100         10  9M-CONTROL-NUMBER               PIC X(07).
021200         10  FILLER                          PIC X(47).
021300
021400     05  9R-PAYEE-RECORD-INFO   REDEFINES   REMAINING-INFORMATION.
021500         10  9R1-GROSS-DISTRIBUTION          PIC 9(16)V99 COMP-3.
021600         10  9R2-TAXABLE-AMOUNT              PIC 9(16)V99 COMP-3.
021700         10  9R3-CAPITAL-GAINS               PIC 9(16)V99 COMP-3.
021800         10  9R4-FEDERAL-TAX-WITHHELD        PIC 9(16)V99 COMP-3.
021900         10  9R5-EMPLOYEE-CONTRIB-OR-INS     PIC 9(16)V99 COMP-3.
022000         10  9R6-SECURITIES-APPRECIATION     PIC 9(16)V99 COMP-3.
022100**** BEGIN CHANGES--4/1/1998    CT05953
022200*        10  9R7-NOT-USED                    PIC 9(11)V99 COMP-3.
022300         10  9R7-TOT-EMP-CONT-9B             PIC 9(16)V99 COMP-3.
022400**** END CHANGES--4/1/1998    CT05953
022500         10  9R8-OTHER                       PIC 9(16)V99 COMP-3.
022600         10  9R9-STATE-TAX-WITHHELD          PIC 9(16)V99 COMP-3.
022700         10  9R10-NOT-USED                   PIC 9(16)V99 COMP-3.
022800         10  9R11-NOT-USED                   PIC 9(16)V99 COMP-3.
022900         10  9R12-NOT-USED                   PIC 9(16)V99 COMP-3.
023000         10  9R-GROSS-AMOUNT                 PIC 9(16)V99 COMP-3.
023100         10  9R-OTHER-PERCENT                PIC 9(03)    COMP-3.
023200         10  9R-TOTAL-PERCENT                PIC 9(03)    COMP-3.
023300         10  9R-WITHDRAWAL-REASON-CODE       PIC X(01).
023400         10  9R-LOCAL-TAX-WITHHELD           PIC 9(16)V99 COMP-3.
023500         10  9R-NAME-OF-LOCALITY             PIC X(02).
      ***** Z002349 **** 884607******* 07/15/03 ** START ***************
023600*        10  FILLER                          PIC X(58).
               10  9R-MOD-EXCL-AMT                 PIC 9(16)V99 COMP-3.
      ***** IMP884989****** 04/02/2014 ****** Z002349 ******* START ****
023600*        10  FILLER                          PIC X(48).           02520001
               10  9R-W8-W9-FORM-RECEIVED             PIC X.
      ******* IMP884880 ***** Z002291 *****START  ***** 08/21/14********
      *        10  FILLER                          PIC X(47).
               10  INCOME-EDGE-IND                 PIC X.
               10  FILLER                          PIC X(46).
      ****** IMP884880 ***** Z002291 *****END  ***** 08/21/14***********
      ***** IMP884989****** 04/02/2014 ****** Z002349 ******* END   ****
      ***** Z002349 **** 884607******* 07/15/03 ** END   **********
023800     05  98-PAYEE-RECORD-INFO   REDEFINES   REMAINING-INFORMATION.
023900         10  981-IRA-CONTRIB                 PIC 9(16)V99 COMP-3.
024000         10  982-ROLLOVER-CONTRIB            PIC 9(16)V99 COMP-3.
024100         10  983-ROTH-CONV                   PIC 9(16)V99 COMP-3.
009000** AMR90003217    *** 09/14/2001 ****** ZC00208 ******* START ****
024200*        10  984-YEAR-END-VALUE              PIC 9(16)V99 COMP-3.
024300*        10  985-LIFE-INSURANCE-COST         PIC 9(16)V99 COMP-3.
024400*        10  986-NOT-USED                    PIC 9(16)V99 COMP-3.
024500*        10  987-SEP-CONTRIB                 PIC 9(16)V99 COMP-3.
024600*        10  988-SIMPLE-CONTRIB              PIC 9(16)V99 COMP-3.
024700*        10  989-ROTH-CONTRIB                PIC 9(16)V99 COMP-3.
024800*        10  9810-NOT-USED                   PIC 9(16)V99 COMP-3.
024200         10  984-RECHAR-CONTRIB              PIC 9(16)V99 COMP-3.
024200         10  985-YEAR-END-VALUE              PIC 9(16)V99 COMP-3.
024300         10  986-LIFE-INSURANCE-COST         PIC 9(16)V99 COMP-3.
024400         10  987-NOT-USED                    PIC 9(16)V99 COMP-3.
024500         10  988-SEP-CONTRIB                 PIC 9(16)V99 COMP-3.
024600         10  989-SIMPLE-CONTRIB              PIC 9(16)V99 COMP-3.
024700         10  9810-ROTH-CONTRIB               PIC 9(16)V99 COMP-3.
009000** AMR90003217    *** 09/14/2001 ****** ZC00208 ******* END   ****
      ***** Z006082 **** 59869 ******* 07/28/03 ** START **********
024900*        10  9811-NOT-USED                   PIC 9(16)V99 COMP-3.
               10  9811-MRD                        PIC 9(16)V99 COMP-3.
      ***** Z006082 **** 59869 ******* 07/28/03 **  END  **********
025000         10  9812-NOT-USED                   PIC 9(16)V99 COMP-3.
025100         10  FILLER                          PIC X(87).
025200
025300     05  9I-PAYEE-RECORD-INFO   REDEFINES   REMAINING-INFORMATION.
025400         10  9I1-EARNINGS                    PIC 9(16)V99 COMP-3.
025500         10  9I2-EARLY-WITHDRAWAL-PENALTY    PIC 9(16)V99 COMP-3.
025600         10  9I3-SAVINGS-BONDS               PIC 9(16)V99 COMP-3.
025700         10  9I4-FEDERAL-TAX-WITHHELD        PIC 9(16)V99 COMP-3.
025800*        10  9I5-FOREIGN-TAX-PAID            PIC 9(16)V99 COMP-3.
025900*        10  9I6-NOT-USED                    PIC 9(16)V99 COMP-3.
               10  9I5-INVESTMENT-EXPENSES         PIC 9(16)V99 COMP-3.
               10  9I6-FOREIGN-TAX-PAID            PIC 9(16)V99 COMP-3.
026000         10  9I7-NOT-USED                    PIC 9(16)V99 COMP-3.
026100         10  9I8-NOT-USED                    PIC 9(16)V99 COMP-3.
026200         10  9I9-NOT-USED                    PIC 9(16)V99 COMP-3.
026300         10  9I10-NOT-USED                   PIC 9(16)V99 COMP-3.
026400         10  9I11-NOT-USED                   PIC 9(16)V99 COMP-3.
026500         10  9I12-NOT-USED                   PIC 9(16)V99 COMP-3.
026600         10  FILLER                          PIC X(87).
026700
026800     05  W2-PAYEE-RECORD-INFO   REDEFINES   REMAINING-INFORMATION.
026900         10  W21-SS-WAGES                    PIC 9(11)V99 COMP-3.
027000         10  W22-ADV-EARNED-INC-CREDIT       PIC 9(11)V99 COMP-3.
027100         10  W23-GROSS-TIPS-WAGES-OTH-COMP   PIC 9(11)V99 COMP-3.
027200         10  W24-SS-TAX-WITHHELD             PIC 9(11)V99 COMP-3.
027300         10  W25-FEDERAL-TAX-WITHHELD        PIC 9(11)V99 COMP-3.
027400         10  W26-STATE-WAGES                 PIC 9(11)V99 COMP-3.
027500         10  W27-STATE-TAX-WITHHELD          PIC 9(11)V99 COMP-3.
027600         10  W28-LOCAL-WAGES                 PIC 9(11)V99 COMP-3.
027700         10  W29-LOCAL-TAX-WITHHELD          PIC 9(11)V99 COMP-3.
027800         10  W2-LOCALITY-NAME                PIC X(02).
027900         10  FILLER                          PIC X(28).
028000         10  W2-CONTROL-NUMBER               PIC X(07).
028100
