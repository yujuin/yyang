***********************************************************************!
!                                                                        !
!                                 TABLES                                 !
!                          January 2011 Version                          !
!                                                                        !
!   TABLES is a component of the Water Rights Analysis Package (WRAP)    !
!   which is documented by:                                              !
!   Water Rights Analysis Package (WRAP) Modeling System, Reference      !
!   and Users Manuals, TWRI TR-255 and TR-256, 7th Edition, July 2010.   !
!                                                                        !
!************************************************************************!
!
      Module COMVAR
!
!  Module COMVAR (COMmon VARiables) specifies the type and number of array
!  dimensions for variables shared by the subroutines and main program.
!
      Integer CRHEAD,CRMF,CRMF7,CRSFF,CR1F,CR1,CR2,CR3,EQFLAG,DSSMES,
     +        HECDSS,IO1,IO2,MAXCP,MAXRES,MAXWR,MONTHS,MPLOT,NC,NCPTS,
     +        NEWPAGE,NEWPAGE2,NID,NPRDS,NREOUT,NTITLE,NWR,NWROUT,NYR,
     +        NYRS,OUTFORM,RECD,STATUS,TID,TNYRS,UO,XMORE,YRST,YR1,ZZ,
     +        ZZFLAG,ZZWRNUM,ZZZFILE
      Integer BEGMON,BEGYR,DAYS,ENDMON,ENDYR,NDAY(12),NCPO2,NREOUT2,NTI,
     +        NWROUT2,SIMD,SFFOPEN,SFFREAD
      Integer IFLTAB(600)
!
      Integer,Allocatable,Dimension(:)::ICP,ZZWRI
!
      Real CF,CR4,XO(12)
!
      Real,Allocatable,Dimension(:)::EXPP,VALUES
      Real,Allocatable,Dimension(:,:)::PLOT,SFFARR,ZPLOT
      Real,Allocatable,Dimension(:,:,:,:)::ZZF
!
      Integer,Allocatable,Dimension(:,:)::DDATA                             !day
      Real,Allocatable,Dimension(:,:)::PLOTD                                !day
!
      Character(len=3)  DSSMON
      Character(len=4)  YRSTDSS
      Character(len=5)  MONTH1,UNHP,UNIT,UNC,UNL
      Character(len=6)  C1,IDCP(80),IDRES(80)
      Character(len=8)  C2,C3,IDEN8(80)
      Character(len=16) C4,IDEN16(80)
      Character(len=120) FILEN,OROOT,SROOT,TROOT
      Character(len=76) TITLE(5)
!
      Character(len=6),Allocatable,Dimension(:)::ZZCP
      Character(len=16),Allocatable,Dimension(:)::ZZWR
!
      End Module COMVAR
!
!  ***********************************************************************
!
      Module CRMVAR
!
!  Module CRMVAR contains variables shared by the conditional reliability
!  modeling (CRM) subroutines CRMSFF, CRMIPA, and LOGNORMAL.
!
      Integer DIST,FILE1,FILE2,FIT,FM,INTZERO,NFLOW,NSTOR,NUMVAL,
     +        READINI,RESFLAG,SFFARRAYF,TCR1,TCR2,TYPEP
      Character(len=6) CPS(15),CPF(15),RESIDBRS
      Character(len=50) INBRS,OUTPUTSFF
      Real CORREL(2),IS(15),INISTOR,LOWLIM,UPLIM,MFACTOR,MEANCRM,STDCRM
      Real(8) A,B,C,AA,BB,CC
      Real,Allocatable,Dimension(:)::QARRAY
      Real,Allocatable,Dimension(:,:)::XFREQ
      Logical EXIST
      End Module CRMVAR
!
!  ***********************************************************************
!
      Program TABLES
!
!  The Main Program opens files, reads the TIN input file, performs various
!  preliminary tasks, and calls the subroutines that create the tables.
!
      Use COMVAR
!
      Integer BUD,DSS,HRR,I,ISTAT,J,JIDEN,JPLOT,JTEST,JTYPE1,JTYPE2,
     +        JTYPE3,JTYPE4,JTYPE5,JTYPE6,JTYPE7,JTYPE8,JTOTAL,KK1CPT,
     +        P,P3,PP,PT,ZZZ
      Character(len=1) CHAR
      Character(len=4) CD
      Character(len=8) TIMEX
      Character(len=9) DATEX
      Character(len=120) NAME2,XROOT
      Character(len=64) CNAME
      Logical EXIST
!
      CRHEAD=0
      CRSFF=0
      CRMF=0
      CRMF7=0
      CR1F=0
      DSSMON='   '
      DSS=0
      DSSMES=2
      HECDSS=0
      KK1CPT=0
      MPLOT=0
      NEWPAGE=0
      NEWPAGE2=0
      NPRDS=12                                                              !day
      OUTFORM=0
      SIMD=0                                                                !day
      SFFOPEN=0
      SFFREAD=0
      STATUS=0
      XMORE=0
      ZZZFILE=0
!
      Print*,'  *****************************************************'
      Print*,'  **                                                 **'
      Print*,'  **          Water Rights Analysis Package          **'
      Print*,'  **             Post-Simulation Program             **'
      Print*,'  **                     TABLES                      **'
      Print*,'  **              January 2011 Version               **'
      Print*,'  **                                                 **'
      Print*,'  *****************************************************'
      Print*,' '
      Print*,' '
!
!  The roots of the input and output filenames are specified.
!
      OROOT='DEFAULT'
      SROOT='DEFAULT'
10    Format(A)
      Print*,'   Defaults are for filenames to have the same root.'
      Print*,' '
      Print*,'   Root of TABLES input filename is entered.'
      Print*,' '
      Read(*,10) TROOT
      Print*,' '
      Print*,'   Root of TABLES output filename is entered.'
      Print*,' '
      Read(*,10) OROOT
      If(OROOT.EQ.'DEFAULT'.or.OROOT.EQ.' ') Then
         OROOT=TROOT
         PP=0
      Else
         PP=Index(OROOT,'.')
         If(PP.GT.0) Then
            P=PP-1
            XROOT=OROOT
            OROOT=OROOT(:P)
         Endif
      Endif
      Print*,' '
      Print*,'   Root of SIM/SIMD/SALT filenames is entered.'
      Print*,' '
      Read(*,10) SROOT
      Print*,' '
      If(SROOT.EQ.'DEFAULT'.or.SROOT.EQ.' ') SROOT=TROOT
!
!  The SIM and SIMD ouput files OUT, CRM, and SUB (units 4,7,10)
!  and SALT output file SAL (unit=12) are direct access.
!  The other files are accessed sequentially.
!
!   Unit=1  TIN  TABLES input file
!   Unit=2  TOU  TABLES output file
!   Unit=3  DAT  SIM/SIMD input file (Input record Types 1,3)
!   Unit=4  CRM  SIM/SIMD conditional reliability modeling output (Type 5)
!   Unit=4  OUT  SIM/SIMD monthly simulation results output (Types 2,3)
!   Unit=7  OUT  SIM/SIMD monthly output for 5CR1 or 5COR record (Type 5)
!   Unit=5  HRR  SIM/SIMD hydropower and reservoir release output (Type 4)
!   Unit=6  DIS  SIM/SIMD flow distribution input (KK1CPT from 1CPT record)
!   Unit=8  SFF  TABLES storage-frequency-flow file (Type 5)
!   Unit=9  BRS  SIM/SIMD beginning reservoir storage file (Type 8)
!   Unit=10 SUB  SIMD sub-monthly time step simulation results (Type 6)
!   Unit=11 AFF  SIMD annual flood frequency analysis file (Type 7)
!   Unit=12 SAL  SALT simulation results output file (Type 8)
!   Unit=17 ZZZ  SIM/SIMD ZZ record water right loop flows (Type 4)
!   Unit=20 TMS  TABLES message file
!   Unit=25 DSS  HEC-DSS Data Storage System
!
!  TABLES message TMS file is opened (unit=20).
!
      P=Index(OROOT,'   ')-1
      FILEN=OROOT(:P)//'.TMS'
      Write(*,20) FILEN
20    Format(1x,'   Opening TABLES Message File: ',A120)
      Inquire (FILE=FILEN,EXIST=EXIST)
      If(.NOT.EXIST) Then
         Open(UNIT=20,FILE=FILEN,STATUS='UNKNOWN')
      Else
         Open(UNIT=20,FILE=FILEN,STATUS='OLD')
         Close(UNIT=20,STATUS='DELETE')
         Open(UNIT=20,FILE=FILEN,STATUS='UNKNOWN')
      Endif
      Write(20,*) ' TABLES MESSAGE FILE '
      Write(20,*)
!
!  TABLES input TIN file is opened (unit=1).
!
      P=Index(TROOT,'   ')-1
      FILEN=TROOT(:P)//'.TIN'
      Write(*,30) FILEN
30    Format(1x,'   Opening TABLES Input File:   ',A120)
      Inquire(FILE=FILEN,EXIST=EXIST)
      If(.NOT.EXIST) Then
         Write(*,40) FILEN
         Write(20,40) FILEN
40       Format('    File does not exist: ',A120)
         P3=Index(FILEN,'   ')-1
         NAME2=FILEN(:P3)//'.TXT'
         Inquire(FILE=NAME2,EXIST=EXIST)
         If(EXIST) Then
            Print 50, NAME2
            Write(20,50) NAME2
50          Format(4x,'Extension TXT should be removed for file: ',A120)
         Endif
         Goto 720
      Endif
      Open(UNIT=1,FILE=FILEN,STATUS='OLD')
      Write(20,60) FILEN
60    Format('*** File was opened: ',A120)
!
!  TABLES output TOU file is opened (unit=2).
!
      If(PP.EQ.0) Then
         P=Index(OROOT,'   ')-1
         FILEN=OROOT(:P)//'.TOU'
      Else
         FILEN=XROOT
      Endif
      Write(*,70) FILEN
70    Format(1x,'   Opening TABLES Output File:  ',A120)
      Inquire (FILE=FILEN,EXIST=EXIST)
      If(.NOT.EXIST) Then
         Open(UNIT=2,FILE=FILEN,STATUS='UNKNOWN')
         Write(20,60) FILEN
      Else
         Print*,'   Output file already exists.',
     +          '  Replace? Yes(Y) or No(N)'
         Read(*,10) CHAR
         If(CHAR.NE.'N'.and.CHAR.NE.'n') Then
            Open(UNIT=2,FILE=FILEN,STATUS='OLD')
            Close(UNIT=2,STATUS='DELETE')
            Open(UNIT=2,FILE=FILEN,STATUS='UNKNOWN')
            Write(20,60) FILEN
         Else
            Print*,' '
            Print*,'   Program Terminated to Prevent Replacing File.'
            Write(20,80) FILEN
80          Format(/,'   Program Terminated to Prevent Replacing File: '
     +              ,A120)
            Goto 720
         Endif
      Endif
!
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Record identifiers are checked and counted.
!
      I = 0
      JTYPE1=0
      JTYPE2=0
      JTYPE3=0
      JTYPE4=0
      JTYPE5=0
      JTYPE6=0
      JTYPE7=0
      JTYPE8=0
      NTITLE=0
      JPLOT=0
      JTEST=0
      JIDEN=0                                                             !day
      BUD=0
      HRR=0
      ZZFLAG=0
      ZZZ=0
90    Read(1,100,End=170,IOSTAT=STATUS) CD
100   Format(A4)
      If(STATUS.NE.0) Then
         Write(20,110) CD,STATUS
110      Format(/,' ERROR: Fortran IOSTAT error occurred reading a',
     +            ' record identifier of ',A4,/,
     +            '    *** IOSTAT status variable (error code) =',I6)
         Write(20,120)
120      Format('    *** TABLES terminated from main program due to',
     +          ' error.')
         Call ERROR
      Endif
      If (CD.EQ.'TITL') Then
         NTITLE = NTITLE + 1
      Elseif (CD.EQ.'TEST') Then
         JTEST = JTEST + 1
      Elseif (CD.EQ.'ENDF') Then
         I = I + 1
         Goto 170
      Elseif (CD.EQ.'1CPT') Then
         JTYPE1 = JTYPE1 + 1
         Backspace(1)
         Read(1,130) KK1CPT
130      Format(4x,I4)
         If(KK1CPT.EQ.0) KK1CPT=1
      Elseif (CD.EQ.'****'.or.CD.EQ.'*** '.or.CD.EQ.'**  '.or.
     +        CD.EQ.'COMM'.or.CD.EQ.'PAGE'.or.CD.EQ.'UNIT') Then
         I = I + 1
      Elseif (CD.EQ.'FILE') Then
         I = I + 1
         Backspace(1)
         Read(1,135,IOSTAT=STATUS) CD,OUTFORM,DSSMES,DSSMON
135      Format(A4,2I4,1x,A3)
         If(STATUS.NE.0) Then
            Write(20,110) CD,STATUS
            Write(20,120)
            Call ERROR
         Endif
         If(OUTFORM.NE.0) OUTFORM=1
         If(DSSMES.NE.0.or.DSSMON.NE.'   ') DSS=DSS+1
         If(DSSMES.EQ.0) DSSMES=2
         If(DSSMES.EQ.-1) DSSMES=0
      Elseif (CD.EQ.'1REC'.or.CD.EQ.'1SRT'.or.CD.EQ.'1SUM'.or.
     +        CD.EQ.'1LEN') Then
         JTYPE1 = JTYPE1 + 1
      Elseif (CD.EQ.'2NAT'.or.CD.EQ.'2REG'.or.CD.EQ.'2UNA'.or.
     +        CD.EQ.'2CLO'.or.CD.EQ.'2CLC'.or.CD.EQ.'2RFR'.or.
     +        CD.EQ.'2URR'.or.CD.EQ.'2STO'.or.CD.EQ.'2EVA'.or.
     +        CD.EQ.'2DIV'.or.CD.EQ.'2TAR'.or.CD.EQ.'2SHT'.or.
     +        CD.EQ.'2DEP'.or.CD.EQ.'2IFT'.or.CD.EQ.'2IFS'.or.
     +        CD.EQ.'2ROR'.or.CD.EQ.'2RFL'.or.CD.EQ.'2ASF'.or.
     +        CD.EQ.'2HPS'.or.CD.EQ.'2HPE'.or.CD.EQ.'2RID'.or.
     +        CD.EQ.'2RIR'.or.CD.EQ.'2RAH'.or.CD.EQ.'2RNA'.or.
     +        CD.EQ.'2EPD'.or.CD.EQ.'2EVR'.or.CD.EQ.'2CPI'.or.
     +        CD.EQ.'2WSE'.or.CD.EQ.'2XAV'.or.CD.EQ.'2RSC'.or.
     +        CD.EQ.'2RSD'.or.CD.EQ.'2FSV'.or.CD.EQ.'2FSC') Then
         JTYPE2 = JTYPE2 + 1
         Backspace(1)
         Read(1,140,IOSTAT=STATUS) PT
140      Format(8x,I4)
         If(STATUS.NE.0) Then
            Write(20,110) CD,STATUS
            Write(20,120)
            Call ERROR
         Endif
         If(PT.GE.1.and.PT.LE.3) JPLOT=JPLOT+1
      Elseif (CD.EQ.'2SCP'.or.CD.EQ.'2SWR'.or.CD.EQ.'2SRE'.or.
     +        CD.EQ.'2SBA'.or.CD.EQ.'2SGP'.or.CD.EQ.'2REL'.or.
     +        CD.EQ.'2FRE'.or.CD.EQ.'2FRQ'.or.CD.EQ.'2RES') Then
         JTYPE2 = JTYPE2 + 1
      Elseif(CD.EQ.'IDEN') Then                                             !day
         JIDEN=JIDEN+1                                                      !day
      Elseif (CD.EQ.'2BUD') Then
         JTYPE2 = JTYPE2 + 1
!
!      BUD is zero unless one or more 2BUD records are read. BUD=9 means DAT
!      file is required. BUD=BEGSTO=1 means DAT and BRS files are required.
!
         If(BUD.EQ.0.or.BUD.EQ.9) Then
            Backspace(1)
            Read(1,150,IOSTAT=STATUS) J,BUD
150         Format(20x,2I4)
            If(STATUS.NE.0) Then
               Write(20,110) CD,STATUS
               Write(20,120)
               Call ERROR
            Endif
            If(BUD.NE.1) BUD=9
            If(J.EQ.1.and.KK1CPT.EQ.0) Then
               Write(20,*) ' ERROR: A 1CPT record is specified by',
     +                     ' ICPI in 2BUD record field 5.'
               Call ERROR
            Endif
         Endif
      Elseif (CD.EQ.'3NAT'.or.CD.EQ.'3REG'.or.CD.EQ.'3UNA'.or.
     +        CD.EQ.'3U+D'.or.CD.EQ.'3DEP'.or.CD.EQ.'3EPD') Then
         JTYPE3 = JTYPE3 + 1
      Elseif (CD.EQ.'4HRR') Then
         JTYPE4 = JTYPE4 + 1
         HRR=9
      Elseif (CD.EQ.'4ZZZ'.or.CD.EQ.'4ZZF') Then
         JTYPE4 = JTYPE4 + 1
         ZZZ=9
         If(CD.EQ.'4ZZZ') Then
            Backspace(1)
            Read(1,140,IOSTAT=STATUS) PT
            If(STATUS.NE.0) Then
               Write(20,110) CD,STATUS
               Write(20,120)
               Call ERROR
            Endif
            If(PT.EQ.1.or.PT.EQ.2.or.PT.LE.3) ZZFLAG=ZZFLAG+1
         Endif
      Elseif (CD.EQ.'5CRM'.or.CD.EQ.'5CR2') Then
         CRMF=9
         JTYPE5 = JTYPE5 + 1
      Elseif (CD.EQ.'5CR1'.or.CD.EQ.'5COR') Then
         CRMF7=9
         JTYPE5 = JTYPE5 + 1
      Elseif (CD.EQ.'6NAT'.or.CD.EQ.'6REG'.or.CD.EQ.'6UNA'.or.              !day
     +        CD.EQ.'6CLO'.or.CD.EQ.'6CLC'.or.CD.EQ.'6RFR'.or.              !day
     +        CD.EQ.'6URR'.or.CD.EQ.'6STO'.or.CD.EQ.'6EVA'.or.              !day
     +        CD.EQ.'6DIV'.or.CD.EQ.'6TAR'.or.CD.EQ.'6SHT'.or.              !day
     +        CD.EQ.'6DEP'.or.CD.EQ.'6IFT'.or.CD.EQ.'6IFS'.or.              !day
     +        CD.EQ.'6ROR'.or.CD.EQ.'6RFL'.or.CD.EQ.'6ASF'.or.              !day
     +        CD.EQ.'6HPS'.or.CD.EQ.'6HPE'.or.CD.EQ.'6RID'.or.              !day
     +        CD.EQ.'6RIR'.or.CD.EQ.'6RAH'.or.CD.EQ.'6RNA'.or.              !day
     +        CD.EQ.'6EPD'.or.CD.EQ.'6SCP'.or.CD.EQ.'6SWR'.or.              !day
     +        CD.EQ.'6SRE'.or.CD.EQ.'6SBA'.or.CD.EQ.'6SGP'.or.              !day
     +        CD.EQ.'6REL'.or.CD.EQ.'6RET'.or.CD.EQ.'6FRE'.or.              !day
     +        CD.EQ.'6FRQ'.or.CD.EQ.'6RES'.or.CD.EQ.'6XAV'.or.              !day
     +        CD.EQ.'6EVR'.or.CD.EQ.'6CPI'.or.CD.EQ.'6WSE'.or.
     +        CD.EQ.'6FSV'.or.CD.EQ.'6FSC') Then                            !day
         JTYPE6 = JTYPE6 + 1                                                !day
      Elseif (CD.EQ.'7FFA'.or.CD.EQ.'SKEW'.or.CD.EQ.'7VOL'.or.
     +        CD.EQ.'7DAM') Then
         JTYPE7 = JTYPE7 + 1
      Elseif (CD.EQ.'8SAL'.or.CD.EQ.'8FRE'.or.CD.EQ.'8FRQ'.or.
     +        CD.EQ.'8SUM'.or.CD.EQ.'8REL'.or.CD.EQ.'8CON') Then
         JTYPE8 = JTYPE8 + 1
         If(CD.EQ.'8SAL') JPLOT=JPLOT+1
      Else
         Print*,' '
         Print 160, CD
         Write(20,160) CD
160      Format(/,' ERROR: ',A4,' is not a valid record identifier.')
         Call ERROR
      Endif
!
!  The next record in the TIN file is read.
!
      Goto 90
170   Rewind(1)
!
!  Record count.
!
      JTOTAL=JTYPE1+JTYPE2+JTYPE3+JTYPE4+JTYPE5+JTYPE6+JTYPE7+JTYPE8
     +       +JTEST+I+NTITLE+JIDEN                                          !day
      Print 180, JTOTAL
180   Format('    The TABLES TIN input file contains',I3,' records.',/)
      Write(20,190) JTOTAL
190   Format('*** Identifiers for the',I3,' records in the TIN file',
     +       ' were checked.')
!
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  The remaining files are opened.
!
!  SIM DAT file is opened (unit=3).
!
      If(JTYPE1.GT.0.or.BUD.GT.0) Then
         Print*
         Print*,'   A SIM input file is required.'
         Print*
         P=Index(SROOT,'   ')-1
         FILEN=SROOT(:P)//'.DAT'
         Write(*,200) FILEN
         Print*,' '
200      Format(1x,'   Opening SIM DAT Input File: ',A120)
         Inquire (FILE=FILEN,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(*,40) FILEN
            Goto 720
         Endif
         Open(UNIT=3,FILE=FILEN,STATUS='OLD')
         Write(20,60) FILEN
!
!  SIM DIS file is opened (unit=6).
!
         If(KK1CPT.EQ.5.or.KK1CPT.EQ.6) Then
            FILEN=SROOT(:P)//'.DIS'
            Write(*,210) FILEN
210         Format(1x,'   Opening SIM DIS Input File: ',A120)
            Inquire (FILE=FILEN,EXIST=EXIST)
            If(.NOT.EXIST) Then
               Write(*,40) FILEN
               Goto 720
            Endif
            Open(UNIT=6,FILE=FILEN,STATUS='OLD')
            Write(20,60) FILEN
         Endif
      Endif
!
!  SIM or SIMD output files are opened.
!
      If(JTYPE2.GT.0.or.JTYPE3.GT.0.or.JTYPE4.GT.0.or.JTYPE5.GT.0.or.
     +   JTYPE6.GT.0.or.JTYPE7.GT.0.or.JTEST.GT.0) Then
         Print*
         Print*,'   One or more SIM/SIMD output files are required.'
         Print*
         P=Index(SROOT,'   ')-1
      Endif
!
!  SIM/SIMD OUT file is opened (unit=4).
!
      If((JTYPE2.GT.0.or.JTYPE3.GT.0.or.JTEST.GT.0).and.CRMF.EQ.0) Then
         FILEN=SROOT(:P)//'.OUT'
         Write(*,220) FILEN
220      Format(1x,'   Opening SIM OUT Output File: ',A120)
         Print*,' '
         Inquire (FILE=FILEN,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(*,40) FILEN
            Write(20,80)
            Goto 720
         Endif
         If(OUTFORM.EQ.1) Then
            Open(UNIT=4,FILE=FILEN,FORM='UNFORMATTED',
     +           ACCESS='DIRECT',RECL=20,STATUS='OLD')
         Else
            Open(UNIT=4,FILE=FILEN,ACCESS='DIRECT',FORM='FORMATTED',
     +           RECL=137,STATUS='OLD',CARRIAGECONTROL='LIST')
         Endif
         Write(20,60) FILEN
!
!  An OUT file can not be opened both as unit 7 and unit 4.
!
         If(CRMF7.GT.0) Then
            Write(20,225)
225         Format(' ERROR: A 5CR1 or 5COR record can not be included',
     +         ' in a TIN file with other',/,8x,'records requiring an',
     +         ' OUT file rather than a CRM file.')
            Call ERROR
         Endif
      Endif
!
!  SIM/SIMD CRM file is opened (unit=4).
!
      If(CRMF.GT.0) Then
         FILEN=SROOT(:P)//'.CRM'
         Write(*,230) FILEN
230      Format(1x,'   Opening SIM CRM Output File: ',A120)
         Inquire (FILE=FILEN,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(*,40) FILEN
            Write(20,80)
            Goto 720
         Endif
         If(OUTFORM.EQ.1) Then
            Open(UNIT=4,FILE=FILEN,FORM='UNFORMATTED',
     +           ACCESS='DIRECT',RECL=20,STATUS='OLD')
         Else
            Open(UNIT=4,FILE=FILEN,ACCESS='DIRECT',FORM='FORMATTED',
     +           RECL=137,STATUS='OLD',CARRIAGECONTROL='LIST')
         Endif
         Write(20,60) FILEN
      Endif
!
!  SIM/SIMD OUT file is opened for 5CR1 or 5COR record (unit=7).
!
      If(CRMF7.GT.0) Then
         FILEN=SROOT(:P)//'.OUT'
         Write(*,220) FILEN
         Inquire (FILE=FILEN,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(*,40) FILEN
            Write(20,80)
            Goto 720
         Endif
         If(OUTFORM.EQ.1) Then
            Open(UNIT=7,FILE=FILEN,FORM='UNFORMATTED',
     +           ACCESS='DIRECT',RECL=20,STATUS='OLD')
         Else
            Open(UNIT=7,FILE=FILEN,ACCESS='DIRECT',FORM='FORMATTED',
     +           RECL=137,STATUS='OLD',CARRIAGECONTROL='LIST')
         Endif
         Write(20,60) FILEN
      Endif
!
!  SIM/SIMD HRR file is opened (unit=5).
!
      If(HRR.GT.0) Then
         P=Index(SROOT,'   ')-1
         FILEN=SROOT(:P)//'.HRR'
         Write(*,240) FILEN
240      Format(1x,'   Opening SIM HRR Output File: ',A120)
         Inquire (FILE=FILEN,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(*,40) FILEN
            Goto 720
         Endif
         Open(UNIT=5,FILE=FILEN,FORM='Formatted',
     +        ACCESS='SEQUENTIAL',STATUS='OLD')
         Write(20,60) FILEN
      Endif
!
!  SIM/SIMD ZZZ file is opened (unit=17).
!
      If(ZZZ.GT.0) Then
         P=Index(SROOT,'   ')-1
         FILEN=SROOT(:P)//'.ZZZ'
         Write(*,250) FILEN
250      Format(1x,'   Opening SIM ZZZ Output File: ',A120)
         Inquire (FILE=FILEN,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(*,40) FILEN
            Goto 720
         Endif
         Open(UNIT=17,FILE=FILEN,FORM='Formatted',
     +        ACCESS='SEQUENTIAL',STATUS='OLD')
         Write(20,60) FILEN
      Endif
!
!  SIM/SIMD BRS file (unit=9) is opened if specified on 2BUD record.
!
      If(BUD.EQ.1) Then
         P=Index(SROOT,'   ')-1
         FILEN=SROOT(:P)//'.BRS'
         Write(*,260) FILEN
260      Format(1x,'   Opening SIM BRS Output File: ',A120)
         Inquire (FILE=FILEN,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(*,40) FILEN
            Goto 720
         Endif
         Open(UNIT=9,FILE=FILEN,STATUS='OLD')
         Write(20,60) FILEN
      Endif
!
!  SIMD SUB file is opened (unit=10).
!
      If(JTYPE6.GT.0) Then                                                  !day
         P=Index(SROOT,'   ')-1                                             !day
         FILEN=SROOT(:P)//'.SUB'                                            !day
         Write(*,270) FILEN                                                 !day
270      Format(1x,'   Opening SIMD Output File:    ',A120)                 !day
         Inquire (FILE=FILEN,EXIST=EXIST)                                   !day
         If(.NOT.EXIST) Then                                                !day
            Write(*,40) FILEN                                               !day
            Goto 720                                                        !day
         Endif                                                              !day
         If(OUTFORM.EQ.1) Then                                              !day
            Open(UNIT=10,FILE=FILEN,FORM='UNFORMATTED',ACCESS='DIRECT',     !day
     +           RECL=20,STATUS='OLD')                                      !day
         Else                                                               !day
            Open(UNIT=10,FILE=FILEN,FORM='FORMATTED',ACCESS='DIRECT',       !day
     +           RECL=137,STATUS='OLD',CARRIAGECONTROL='LIST')              !day
         Endif
         Write(20,60) FILEN                                                 !day
      Endif                                                                 !day
!
!  SIMD AFF file is opened (unit=11).                                       !day
!
      If(JTYPE7.GT.0) Then
         P=Index(SROOT,'   ')-1
         FILEN=SROOT(:P)//'.AFF'
         Write(*,280) FILEN
280      Format(1x,'   Opening Flood Frequency File: ',A120)
         Inquire (FILE=FILEN,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(*,40) FILEN
            Goto 720
         Endif
         Open(UNIT=11,FILE=FILEN,STATUS='OLD')
         Write(20,60) FILEN
      Endif
!
!  SALT SAL file is opened (unit=12).
!
      If(JTYPE8.GT.0) Then
         P=Index(SROOT,'   ')-1
         FILEN=SROOT(:P)//'.SAL'
         Write(*,290) FILEN
290      Format(1x,'   Opening SALT Output File:    ',A120)
         Inquire (FILE=FILEN,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(*,40) FILEN
            Goto 720
         Endif
         Open(UNIT=12,FILE=FILEN,FORM='Formatted',ACCESS='DIRECT',
     +        RECL=157,STATUS='OLD')
         Write(20,60) FILEN
      Endif
!
!  Error check for DSSMES from FILE record.
!
      If(DSSMES.LT.-1.or.DSSMES.GT.9) Then
         Write(20,300) DSSMES
300      Format(' ERROR: DSSMES of',I3,' in FILE record field 4',
     +          ' is not valid.')
         Call ERROR
      Endif
!
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Miscellaneous preparations are performed prior to calling the
!  subroutines to build the tables.
!
!  The OUT file is checked by reading 'WRAP' from first record.
!
      If((JTYPE2.GT.0.or.JTYPE3.GT.0.or.JTEST.GT.0).and.
     +    CRMF.EQ.0.and.CRMF7.EQ.0) Then
         If(OUTFORM.NE.1) Then
            Read(4,100,REC=1,IOSTAT=STATUS) CD
            If(STATUS.NE.0) Then
               Write(20,320)
320            Format(' ERROR: IOSTAT error occurred in main program',
     +                ' reading first record of OUT file.')
               Call ERROR
            Endif
            If(CD.NE.'WRAP') Then
               Write(20,330) CD
330            Format(' ERROR: The first record of OUT file should',
     +                ' begin with "WRAP" but following was read: ',A4)
               Call ERROR
            Endif
         Endif
      Endif
!
!  The title records are read from the TABLES TIN file.
!
      If(NTITLE.GE.1) Then
         Do I=1,NTITLE
340         Read(1,350) CD,TITLE(I)
350         Format(A4,A76)
            If(CD.NE.'TITL') Goto 340
         End Do
      Endif
!
!  The 5th record of SIM OUT file is read.
!
      If(JTYPE2.GT.0.or.JTYPE3.GT.0.or.CRMF.GT.0) Then
         If(OUTFORM.EQ.1) Then
            Read(4,REC=5,IOSTAT=STATUS) YRST,NYRS,NCPTS,NWROUT,
     +                                  NREOUT,CR1,CR2,CR3,CR4
         Else
            Read(4,360,REC=5,IOSTAT=STATUS) YRST,NYRS,NCPTS,NWROUT,
     +                                      NREOUT,CR1,CR2,CR3,CR4
360         Format(8I6,F8.0)
         Endif
         If(JTYPE5.EQ.0) Then
            CR1=0
            CR2=0
            CR4=0.0
         Endif
         If(STATUS.NE.0) Then
            Write(20,370) STATUS
370         Format(/,' ERROR: Fortran IOSTAT error occurred reading',
     +               ' 5th record of SIM output file.',/,
     +            8x,'SIM output file is not in the correct format.',/,
     +               '    *** IOSTAT status variable (error code) =',I4)
            Write(20,120)
            Call ERROR
         Endif
         If(NYRS.LE.0.or.NYRS.GT.9999999.or.YRST.LT.0.or.
     +      YRST.GT.9999999) Then
            Write(20,380) NYRS,YRST
380         Format(/,' ERROR: NYRS and YRST on 5th record of the SIM ',
     +             'output file are:',/,12x,F20.1,'  and  ',F20.1,/,8x,
     +             'SIM output file is probably in the wrong format.')
         Write(20,120)
         Call ERROR
         Endif
         If(NCPTS.LE.0.and.NWROUT.LE.0.and.NREOUT.LE.0) Then
            Write(20,390)
390         Format(/,' ERROR: NCPTS, NWROUT, and NREOUT on 5th record',
     +               ' of SIM output file are all zero.')
            Write(20,120)
            Call ERROR
         Endif
         TNYRS=NYRS
         If(CR1.GT.0) Then
            NPRDS=CR1
            If(CR2.LE.0) Then
               If(CR1.GT.12.and.CR3.LE.1) NPRDS=12
               NYRS=12*NYRS-CR1+1
            Elseif(CR2.GT.12) Then   
               Write(20,400)
400            Format(/,' ERROR: CR2 on 5th record of SIM output',
     +               ' file is greater than 12.')
               Write(20,120)
               Call ERROR
            Elseif(CR2.GT.0) Then
               If(CR1.GT.12.and.CR3.LE.1) NPRDS=12
               NYRS=NYRS-Int((CR1+CR2-2)/12)
            Endif
         Elseif(CR1.EQ.0.and.CR2.NE.0) Then
            Write(20,410)
410         Format(/,' ERROR: CR1 on 5th record of SIM output',
     +               ' file is zero but CR2 is not zero.')
            Write(20,120)
            Call ERROR
         Elseif(CR1.EQ.0.and.CR2.EQ.0) Then
            NPRDS=12
         Endif
         MONTHS=NPRDS*NYRS
      Endif
!
!  The 5th and 6th records of the WRAP-SIMD daily output file are read.     !day
!
      If(JTYPE6.GT.0) Then                                                  !day
         If(OUTFORM.EQ.1) Then                                              !day
            Read(10,REC=5,IOSTAT=STATUS) BEGYR,BEGMON,ENDYR,ENDMON,         !day
     +                                   DAYS,NCPO2,NWROUT2,NREOUT2         !day
         Else
            Read(10,420,REC=5,IOSTAT=STATUS) BEGYR,BEGMON,ENDYR,ENDMON,     !day
     +                                       DAYS,NCPO2,NWROUT2,NREOUT2     !day
420         Format(8I6)                                                     !day
         Endif                                                              !day
         If(STATUS.NE.0) Then                                               !day
            Write(20,430)                                                   !day
430         Format(/,' ERROR: Fortran IOSTAT error occurred reading',       !day
     +               ' 5th record of SIMD SUB output file.')                !day
            Write(20,110) CD,STATUS                                         !day
            Call ERROR                                                      !day
         Endif                                                              !day
         If(DAYS.LE.0) Then                                                 !day
            Write(20,440)                                                   !day
440         Format(/,' ERROR: DAYS on 5th record of SIMD SUB',              !day
     +              ' output file is zero.')                                !day
            Write(20,110) CD,STATUS                                         !day
            Call ERROR                                                      !day
         Endif                                                              !day
         If(NCPO2.LE.0.and.NWROUT2.LE.0.and.NREOUT2.LE.0) Then              !day
            Write(20,450)                                                   !day
450         Format(/,' ERROR: NCPO2, NWROUT2, and NREOUT2 on 5th',          !day
     +              ' record of SIMD SUB output file are all zero.')        !day
            Write(20,110) CD,STATUS                                         !day
            Call ERROR                                                      !day
         Endif                                                              !day
!
         If(OUTFORM.EQ.1) Then                                              !day
            Read(10,REC=6,IOSTAT=STATUS) NTI,NDAY                           !day
         Else                                                               !day
            Read(10,460,REC=6,IOSTAT=STATUS) NTI,NDAY                       !day
460         Format(13I6)                                                    !day
         Endif                                                              !day
         If(STATUS.NE.0) Then                                               !day
            Write(20,470)                                                   !day
470         Format(/,' ERROR: Fortran IOSTAT error occurred reading',       !day
     +               ' 6th record of SIMD SUB output file.')                !day
            Write(20,110) CD,STATUS                                         !day
            Call ERROR                                                      !day
         Endif                                                              !day
         If(MinVal(NDAY).EQ.0) Then                                         !day
            Write(20,480)                                                   !day
480         Format(/,' ERROR: NDAY on 6th record of SIMD SUB',              !day
     +              ' output file contains a zero value.')                  !day
            Write(20,110) CD,STATUS                                         !day
            Call ERROR                                                      !day
         Endif                                                              !day
         Allocate(PLOTD(DAYS,100))                                          !day
         Allocate(DDATA(DAYS,3))                                            !dat
*         Call DATE_DATA                                                     !day
      Endif                                                                 !day
!
!  The first record of the SALT output file is read.
!
      If(JTYPE8.GT.0) Then
         Read(12,490,REC=1,IOSTAT=STATUS) YRST,NYRS,NCPTS,NC,CF
490      Format(65x,4I5,F8.0)
         If(STATUS.NE.0) Then
            Write(20,500)
500         Format(/,' ERROR: Fortran IOSTAT error occurred reading',
     +               ' first record of SALT output file.')
            Write(20,120)
            Call ERROR
         Endif
         If(NYRS.LE.0.or.NCPTS.LE.0) Then
            Write(20,510) NYRS,NCPTS
510         Format(' ERROR: Number of years NYRS read from 1st record',
     +             ' of SALT output file is',I5,/,8x,'and number of',
     +             ' control points is',I6)
            Write(20,120)
            Call ERROR
         Endif
         NPRDS=12
         MONTHS=12*NYRS
      Endif
!
!  PLOT array used in Subroutines SERIES and SALT for columnar data
!  is allocated if needed by Type 2 series or 8SAL records.
!
      If(JPLOT.GE.1) Then
         Allocate(PLOT(MONTHS,100))
         PLOT=0.0
      Endif
!
!  For Type 1 tables, the SIM input file is read to count variables
!  for setting dimensions.
!
      If(JTYPE1.GT.0) Then
         MAXWR=1
         MAXCP=1
         MAXRES=1
520      Read(3,530,End=540) CD
530      Format(A2)
         If(CD.EQ.'ED'.or.CD.EQ.'SV'.or.CD.EQ.'DI') Then
            Rewind(3)
            Goto 540
         Endif
         If(CD.EQ.'CP') MAXCP=MAXCP+1
         If(CD.EQ.'WR'.or.CD.EQ.'IF') MAXWR=MAXWR+1
         If(CD.EQ.'WS') MAXRES=MAXRES+1
         Goto 520
      Endif
!
!  The 1CPT record creates an array ICP to connect upstream-downstream
!  read-in sequences of control points. The 2BUD record uses the ICP
!  array created by the 1CPT record.
!
540   If(KK1CPT.GE.1) Then
         Allocate(ICP(MAXCP))
      Endif
!
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  The identifier CD of each record of the TABLES input file is read and
!  the subroutine called that develops and writes the specified table or
!  data listing.  The loop is repeated until an ENDF record is read or
!  the end-of-file is reached.
!
      UNIT='AC-FT'
      UNHP='MW-HR'
      UNC='MG/L'
      UNL='TONS'
      MONTH1='  JAN'
550   Read(1,560,End=630) CD
560   Format(A4)
      If(CD.EQ.'UNIT') Then
         Backspace(1)
         Read(1,570) CD,UNIT,UNHP,MONTH1,NEWPAGE2
570      Format(A4,3A5,I5)
         MONTH1=Adjustr(MONTH1)
         If(UNIT.EQ.'     ') UNIT='AC-FT'
         If(UNHP.EQ.'     ') UNHP='MW-HR'
         If(MONTH1.EQ.'     ') MONTH1='  JAN'
         Goto 550
      Endif
      If(CD.EQ.'****'.or.CD.EQ.'*** '.or.CD.EQ.'**  ') Goto 550
      If(CD.EQ.'COMM'.or.CD.EQ.'FILE'.or.CD.EQ.'IDEN') Goto 550
      If(CD.EQ.'ENDF') Goto 640
      If(CD.EQ.'PAGE') Then
         If(JTYPE2.GT.0.or.JTYPE3.GT.0.or.JTYPE5.GT.0.or.JTYPE6.GT.0)
     +   Then
            Call CVPAGE
         Else
            Write(20,*)'   WARNING: PAGE record is used',
     +                 ' only with Type 2,3,5,6 records.'
         Endif
      Else
         Backspace(1)
      Endif
      If(CD.EQ.'TEST') Then
         Write(*,580) CD
580      Format(1X,'   Tests of SIM output file are activated by a ',A4,
     +    ' record.',/,4x,'Results are written to TABLES message file.')
         Write(20,590) CD
590      Format('*** Tests of SIM output file are activated by a ',A4,
     +          ' record.')
      Else
         Write(*,600) CD
600      Format(1X,'   Tables are being developed as specified by a ',
     +          A4,' record.')
         Write(20,610) CD
610      Format('*** Tables are being developed as specified by a ',A4,
     +          ' record.')
      Endif
      If(CD.EQ.'1REC'.or.CD.EQ.'1SRT'.or.CD.EQ.'1SUM') Then
         Call SUMDAT
      Elseif (CD.EQ.'1CPT') Then
         Call CONPTS
      Elseif (CD.EQ.'1LEN') Then
         Call LENGTH
      Elseif (CD.EQ.'TEST') Then
         If(OUTFORM.EQ.1) Then
            Write(20,*)'  WARNING: TEST record is not applicable to an',
     +                 ' unformated OUT file (SIM JD record field 7).'
            Goto 550
         Else
            Call TEST
         Endif
      Elseif (CD.EQ.'IDEN') Then
         Call IDEN
      Elseif (CD.EQ.'2NAT'.or.CD.EQ.'2REG'.or.CD.EQ.'2UNA'.or.
     +        CD.EQ.'2CLO'.or.CD.EQ.'2CLC'.or.CD.EQ.'2RFR'.or.
     +        CD.EQ.'2URR'.or.CD.EQ.'2STO'.or.CD.EQ.'2EVA'.or.
     +        CD.EQ.'2TAR'.or.CD.EQ.'2SHT'.or.CD.EQ.'2DIV'.or.
     +        CD.EQ.'2DEP'.or.CD.EQ.'2IFT'.or.CD.EQ.'2IFS'.or.
     +        CD.EQ.'2RFL'.or.CD.EQ.'2ASF'.or.CD.EQ.'2ROR'.or.
     +        CD.EQ.'2HPS'.or.CD.EQ.'2HPE'.or.CD.EQ.'2RID'.or.
     +        CD.EQ.'2RIR'.or.CD.EQ.'2RAH'.or.CD.EQ.'2RNA'.or.
     +        CD.EQ.'2EPD'.or.CD.EQ.'2EVR'.or.CD.EQ.'2CPI'.or.
     +        CD.EQ.'2WSE'.or.CD.EQ.'2XAV'.or.CD.EQ.'2RSC'.or.
     +        CD.EQ.'2RSD'.or.CD.EQ.'2FSV'.or.CD.EQ.'2FSC') Then
         Call SERIES
      Elseif (CD.EQ.'2SCP'.or.CD.EQ.'2SWR'.or.CD.EQ.'2SRE'.or.
     +        CD.EQ.'2SBA'.or.CD.EQ.'2SGP') Then
         Call SUMTAB
      Elseif (CD.EQ.'2REL') Then
         Call RELIAB
      Elseif (CD.EQ.'2RES') Then
         Call STORAGE
      Elseif (CD.EQ.'2FRE'.or.CD.EQ.'2FRQ') Then 
         Call FREQ
      Elseif (CD.EQ.'2BUD') Then
         Call BUDGET
      Elseif (CD.EQ.'3NAT'.or.CD.EQ.'3REG'.or.CD.EQ.'3UNA'.or.
     +        CD.EQ.'3DEP'.or.CD.EQ.'3U+D'.or.CD.EQ.'3EPD') Then
         Call FlowRec
      Elseif (CD.EQ.'4HRR') Then
         Call SYSTAB
      Elseif (CD.EQ.'4ZZZ') Then
         Call ZZFLOW
      Elseif (CD.EQ.'4ZZF') Then
         Call ZZFREQ
      Elseif (CD.EQ.'5CR1') Then
          CR1F=9
          Call CRMSFF
      Elseif (CD.EQ.'5CR2') Then
          CRSFF=9
          Call CRMIPA
      Elseif (CD.EQ.'5COR') Then
          CR1F=0
          Call CRMSFF
      Elseif(CD.EQ.'5CRM') Then
         Read(1,620) CD,CRHEAD
620      Format(A4,I4)
         Goto 550
      Elseif (CD.EQ.'6NAT'.or.CD.EQ.'6REG'.or.CD.EQ.'6UNA'.or.              !day
     +        CD.EQ.'6CLO'.or.CD.EQ.'6CLC'.or.CD.EQ.'6RFR'.or.              !day
     +        CD.EQ.'6URR'.or.CD.EQ.'6STO'.or.CD.EQ.'6EVA'.or.              !day
     +        CD.EQ.'6TAR'.or.CD.EQ.'6SHT'.or.CD.EQ.'6DIV'.or.              !day
     +        CD.EQ.'6DEP'.or.CD.EQ.'6IFT'.or.CD.EQ.'6IFS'.or.              !day
     +        CD.EQ.'6RFL'.or.CD.EQ.'6ASF'.or.CD.EQ.'6ROR'.or.              !day
     +        CD.EQ.'6HPS'.or.CD.EQ.'6HPE'.or.CD.EQ.'6RID'.or.              !day
     +        CD.EQ.'6RIR'.or.CD.EQ.'6RAH'.or.CD.EQ.'6RNA'.or.              !day
     +        CD.EQ.'6EPD'.or.CD.EQ.'6EVR'.or.CD.EQ.'6CPI'.or.              !day
     +        CD.EQ.'6WSE'.or.CD.EQ.'6XAV'.or.CD.EQ.'6FSV'.or.
     +        CD.EQ.'6FSC') Then                                            !day
         SIMD=1                                                             !day
         Call SERIES                                                        !day
         SIMD=0                                                             !day
      Elseif (CD.EQ.'6REL') Then                                            !day
         SIMD=1                                                             !day
         Call RELIAB                                                        !day
         SIMD=0                                                             !day
      Elseif (CD.EQ.'6RES') Then                                            !day
         SIMD=1                                                             !day
         Call STORAGE                                                       !day
         SIMD=0                                                             !day
      Elseif (CD.EQ.'6FRE'.or.CD.EQ.'6FRQ') Then                            !day
         SIMD=1                                                             !day
         Call FREQ                                                          !day
         SIMD=0                                                             !day
      Elseif (CD.EQ.'7FFA') Then
         Call FFA
      Elseif (CD.EQ.'8SAL') Then
         Call SALT
      Elseif (CD.EQ.'8SUM') Then
         Call SALTSUM
      Elseif (CD.EQ.'8FRE'.or.CD.EQ.'8FRQ') Then
         Call SALTFREQ
      Elseif (CD.EQ.'8REL') Then
         Call SALTREL
      Endif
!
!  End of loop that builds each table.
!
      Goto 550
!
!  The optional HEC-DSS file is closed.
!
      If(HECDSS.GT.0) Call ZCLOSE(IFLTAB)
!
!  Warning messages.
!
630   Print*
      Write(20,*) ' '
      Print*, '   ENDF record is missing.'
      Write(20,*) ' WARNING: ENDF record is missing.'
!
640   If(XMORE.GT.0) Then
         Write(20,650)
650      Format('WARNING: Missing MORE specification in time series ',
     +          'record field 4 to complete columnar table.')
      Endif
!
!  Closing messages.
!
      Print*
      Write(20,*) ' '
      If(PP.EQ.0) Then
         P=Index(OROOT,'   ')-1
         FILEN=OROOT(:P)//'.TOU'
      Else
         FILEN=XROOT
      Endif
      Print 660, FILEN
      Write(20,660) FILEN
660   Format(1x,'   Program TABLES output is in file ',A120)
      Print*
      If(HECDSS.GT.0) Then
         Write(20,*) ' '
         P=Index(OROOT,'   ')-1
         FILEN=OROOT(:P)//'.DSS'
         Print 670, FILEN
         Write(20,670) FILEN
670      Format(1x,'   Program TABLES output is in DSS file ',A120)
         Print*
      Endif
*      Call DATE(DATEX)
*      Call TIME(TIMEX)
*      Write(20,680) DATEX
*680   Format('    Date:  ',A9)
*      Write(20,690) TIMEX
*690   Format('    Time:  ',A8)
*      Write(20,*) ' '
      Print*, '   Normal Completion of Program TABLES'
      Write(20,*) ' ***** Normal Completion of Program TABLES *****'
!
!   Exit without normal completion.
!
720   Write(*,730)
730   Format(/,'    Exit TABLES',/)
      Read(*,740) CD
740   Format(A4)
      Stop
!
!  End of Main Program
!
      End
!
!  ***********************************************************************
!
      Subroutine ERROR
!
!   Subroutine ERROR writes a message to the monitor and stops execution
!   of the program.  It is called in conjunction with error messages.
!
      Use COMVAR
      Character(len=2) EXIT
      Write(*,10)
10    Format(/,'    *** Execution of TABLES terminated due to an',
     +       ' input error.')
      If(STATUS.NE.0) Then
         Write(20,10)
         Write(*,20) STATUS
         Write(20,20) STATUS
20       Format('    *** IOSTAT status variable (error code) =',I4)
         If(STATUS.EQ.61.or.STATUS.EQ.64) Then
            Write(*,30)
            Write(20,30)
30          Format('    *** Record contains data in wrong format.')
         Elseif(STATUS.EQ.-1) Then
            Write(*,40)
            Write(20,40)
40          Format('    *** End of file was reached without',
     +             ' finding data record.')
         Elseif(STATUS.EQ.-2) Then
            Write(*,50)
            Write(20,50)
50          Format('    *** End of record was reached without',
     +             ' finding data.')
         Endif
      Endif
      Write(*,60)
60    Format('    *** See message file.',/)
      Write(*,70)
70    Format(/,'    Exit TABLES')
      Read(*,80) EXIT
80    Format(A2)
      Stop
      Return
      End Subroutine ERROR
!
!  ***********************************************************************
!
      Subroutine IDEN
!
!   Subroutine IDEN reads identifiers from IDEN records of control points,
!   water rights, reservoirs, or water right groups.  Subroutine IDEN may
!   be called by the main program or Subroutines SERIES, RELIAB, or FREQ.
!
      Use COMVAR
!
      Integer BLANK,I,K,N
      Character(4) CD
!
!   Check of whether Subroutine IDEN is called by main program
!   or another subroutine.
!
      Backspace(1)
      Read(1,10) CD
10    Format(A4)
!
!   If Subroutine IDEN called from main program, TID and NID are read.
!
      If(CD.EQ.'IDEN') Then
         Read(1,20,IOSTAT=STATUS) CD,TID,NID
20       Format(A4,2I4)
         If(STATUS.NE.0) Then
            Write(20,30) CD
30          Format(' ERROR: Fortran IOSTAT error occured reading an',
     *          ' input record with CD of ',A4)
            Call ERROR
         Endif
      Endif
!
!   Error checks.
!
      If(TID.LT.0.or.TID.GT.3) Then
         Write(20,40) TID
40       Format(' ERROR: TID of',I3,' is not valid.')
         Call ERROR
      Endif
      If(NID.LT.0.or.NID.GT.80) Then
         Write(20,50) NID
50       Format(' ERROR: NID of',I3,' is not valid.')
         Call ERROR
      Endif
!
!   Identifiers are read from IDEN records.
!
      K = 1
      N = NID
      If(NID.GT.8) N=8
60    If(TID.EQ.0) Read(1,70,IOSTAT=STATUS) CD,(IDCP(I),I=K,N)
      If(TID.EQ.2) Read(1,70,IOSTAT=STATUS) CD,(IDRES(I),I=K,N)
      If(TID.EQ.3) Read(1,80,IOSTAT=STATUS) CD,(IDEN8(I),I=K,N)
      If(TID.EQ.1) Read(1,90,IOSTAT=STATUS) CD,(IDEN16(I),I=K,N)
70    Format(A4,8(2x,A6))
80    Format(A4,8A8)
90    Format(A4,8A16)
      If(STATUS.NE.0) Then
         Write(20,100) CD
100      Format(' ERROR: Fortran IOSTAT error occured reading an',
     *          ' IDEN record with CD of ',A4)
         Call ERROR
      Endif
      If(CD.NE.'IDEN') Then
         Write(20,110) CD
110      Format(' ERROR: Record with CD of ',A4,
     +          ' found rather than IDEN.')
         Call ERROR
      Endif
      If(NID.GT.N) Then
         K = K + 8
         N = N + 8
         If(N.GT.NID) N=NID
         Goto 60
      Endif
      BLANK=0
      Do I=1,NID
         If(TID.EQ.0) Then
            IDCP(I)=Adjustr(IDCP(I))
            If(IDCP(I).EQ.'      ') BLANK=BLANK+1
         Elseif(TID.EQ.2) Then
            IDRES(I)=Adjustr(IDRES(I))
            If(IDRES(I).EQ.'      ') BLANK=BLANK+1
         Elseif(TID.EQ.3) Then
            IDEN8(I)=Adjustr(IDEN8(I))
            If(IDEN8(I).EQ.'        ') BLANK=BLANK+1
         Elseif(TID.EQ.1) Then
            IDEN16(I)=Adjustr(IDEN16(I))
            If(IDEN16(I).EQ.'                ') BLANK=BLANK+1
         Endif
      End Do
!
!    Error message.
!
      If(BLANK.GT.0) Then
         Write(20,120) BLANK
120      Format(' WARNING:',I2,' identifiers on IDEN record are blank.')
      Endif
!
      Return
      End Subroutine IDEN
!
!  ***********************************************************************
!
      Subroutine TITLES
!
!   Subroutine TITLES writes the title records on the cover page and
!   at the top of each Job Type 1 and 2 table.
!
      Use COMVAR
      Integer I
      If(NEWPAGE.GT.0.and.NEWPAGE2.GE.0) Write(2,10) Char(12)
10    Format(A)
      If(NEWPAGE.GT.0.and.NEWPAGE2.LT.0) Write(2,*)
      If(NTITLE.GE.1) Then
         Do I=1,NTITLE
            Write(2,20) TITLE(I)
20          Format(3X,A76)
         End Do
      Write(2,*)
      Endif
      NEWPAGE=NEWPAGE+1
      Return
      End Subroutine TITLES
!
!  ***********************************************************************
!
      Subroutine CVPAGE
!
!   *-*-*-*-*-* PAGE RECORD *-*-*-*-*-*
!   Subroutine CVPAGE prints the cover page for the output tables.
!
      Use COMVAR
      Integer I,P
      Character(len=80) TITLE1,TITLE2,TITLE3
      Logical:: OUTOPEN,SUBOPEN                                             !day
!
!   Check to see if the OUT and/or SUB files are open.
!
      Inquire(4, OPENED=OUTOPEN)                                            !day
      Inquire(10,OPENED=SUBOPEN)                                            !day
!
      Write(2,*)'  ***************************************************'
      Write(2,*)'  **                                               **'
      Write(2,*)'  **         Water Rights Analysis Package         **'
      Write(2,*)'  **                    TABLES                     **'
      Write(2,*)'  **             January 2011 Version              **'
      Write(2,*)'  **                                               **'
      Write(2,*)'  ***************************************************'
!
      Write(2,*) ' '
      Write(2,*) ' '
      Write(2,*) ' '
      Write(2,*) ' '
!
!   SIM OUT output file.
!
      If(OUTOPEN) Then                                                      !day
         If(OUTFORM.EQ.1) Then
            Read(4,REC=2) TITLE1
            Read(4,REC=3) TITLE2
            Read(4,REC=4) TITLE3
         Else
            Read(4,40,REC=2) TITLE1
            Read(4,40,REC=3) TITLE2
            Read(4,40,REC=4) TITLE3
         Endif
      Endif                                                                 !day
      If(SUBOPEN) Then                                                      !day
         If(OUTFORM.EQ.1) Then
            Read(10,REC=2) TITLE1                                           !day
            Read(10,REC=3) TITLE2                                           !day
            Read(10,REC=4) TITLE3                                           !day
         Else
            Read(10,40,REC=2) TITLE1                                        !day
            Read(10,40,REC=3) TITLE2                                        !day
            Read(10,40,REC=4) TITLE3                                        !day
         Endif
      Endif                                                                 !day
40    Format(A80)
      If(OUTOPEN) Then                                                      !day
         Write(2,50)
         Write(2,70) TITLE1, TITLE2, TITLE3
         Write(2,80) NWROUT, NCPTS, NREOUT, NYRS, YRST
      Endif                                                                 !day
      If(SUBOPEN) Then                                                      !day
         Write(2,*)' '                                                      !day
         Write(2,*)' '                                                      !day
         Write(2,60)                                                        !day
         Write(2,70) TITLE1, TITLE2, TITLE3                                 !day
         Write(2,90) NWROUT2, NCPO2, NREOUT2                                !day
         Call SIMDHEADER                                                    !day
         Backspace(2)                                                       !day
      Endif                                                                 !day
50    Format(3X,'Title records from WRAP-SIM output file:',/)
60    Format(3X,'Title records from WRAP-SIMD daily output file:',/)        !day
70    Format(A80,/,A80,/,A80,//)
80    Format('   The program WRAP-SIM output file contains ',
     +       'simulation results for:',/,I10,' water rights',/,I10,
     +       ' control points',/,I10,' reservoirs',/,3X,'for a period',
     +       '-of-analysis of',I3,' years beginning in ',I4,'.')
90    Format('   The program WRAP-SIMD daily output file contains ',        !day
     +       'simulation results for:',/,I10,' water rights',/,I10,         !day
     +       ' control points',/,I10,' reservoirs','.')                     !day
!
!   Conditional reliability modeling (CRM) output headings.
!
      If(CR1.GT.0.and.CRHEAD.GE.0) Then
         If(CRSFF.EQ.0) Write(2,100)
         If(CRSFF.GT.0) Write(2,110)
         If(CR2.GT.0) Write(2,140) CR2
         If(CR2.EQ.0) Write(2,150) NYRS
         Write(2,100) CR1
         If(CR4.GT.0.0) Write(2,130) CR4
         Write(2,*) ' '
      Endif
100   Format(//,'   Conditional Reliability Modeling:',
     +          ' Equal-Weight Option')
110   Format('   Conditional Reliability Modeling:',
     +       ' Probability Array Option')
120   Format('   Length of simulation period (CR1) is',I3,' months.')
130   Format('   Initial storage multiplier (CR4) =',F7.3)
140   Format('   Annual cycles starting in month',I2)
150   Format('   Monthly cycle option with',I4,' sequences.')
!
!   TABLES TIN input file.
!
      If(NTITLE.GE.1) Then
         Write(2,*) ' '
         Write(2,160)
160      Format(//'   Title records from program TABLES input file:',/)
         Do I=1,NTITLE
            Write(2,170) TITLE(I)
170         Format(3X,A76)
         End Do
      Endif
      Write(2,*) ' '
      Write(2,*) ' '
!
!   Filenames and new page flag.
!
      P=Index(TROOT,'   ')-1
      FILEN=TROOT(:P)//'.TIN'
      Write(2,180) (FILEN)
180   Format('   Program TABLES input file name:  ',A120)
      P=Index(OROOT,'   ')-1
      FILEN=TROOT(:P)//'.TOU'
      Write(2,190) (FILEN)
190   Format('   Program TABLES output file name: ',A120)
      Write(2,*) ' '
      Write(2,200) Adjustl(SROOT)
200   Format('   Root of SIM input and output file names:  ',A120)
      Write(2,*) ' '
      Write(2,*) ' '
      NEWPAGE=NEWPAGE+1
      Return
      End Subroutine CVPAGE
!
!  ***********************************************************************
!
      Subroutine TEST
!
!   *-*-*-*-*-* TEST RECORD *-*-*-*-*-*
!   Subroutine TEST performs error checks on the SIM output OUT file.
!
      Use COMVAR
!
      Integer FLAG,IMONTH,IYEAR,I,J,M,MR,NERROR,NR,R,YR,YEAR
!
      Character(len=1) A(136)
      Character(len=4) CD
      Character(len=6) A2,CP(5000),RES(1500)
      Character(len=16) A3,A4
      Character(len=136) A5
!
      NERROR=0
!
!   First record of SIM output file is read and checked.
!
      Write(20,*) ' '
      Write(20,10)
10    Format('-+- The first record of the SIM output file is read.')
      Read(4,20,REC=1,IOSTAT=STATUS) A3
20    Format(A16)
      If(STATUS.NE.0) Then
         Write(20,30)
30       Format(' ERROR: Fortran IOSTAT error occurred reading first',
     +          ' record of SIM output file.',/,8x,'The problem may',
     +          ' be that the file is not in text (txt) format.')
         NERROR=NERROR+1
      Endif
      If(A3.NE.'Program WRAP-SIM') Then
         Write(20,40) A3
40       Format(' ERROR: First record should begin: Program WRAP-SIM',
     +          /,8x,'but TEST routine read: ',A16)
         Write(20,45)
45       Format(8x,'The problem may be that the file is not in text ',
     +             '(txt) format.')
         NERROR=NERROR+1
      Endif
      Read(4,50,REC=1,IOSTAT=STATUS) A5
50    Format(A134)
      If(STATUS.NE.0) Then
         Write(20,30)
         NERROR=NERROR+1
      Endif
      Write(20,50) A5
      Write(20,*) ' '
!
!   5th record of SIM output file is read and checked.
!
      Write(20,60)
60    Format('-+- The 5th record of the SIM output file is read.')
      Read(4,70,REC=5,IOSTAT=STATUS) YRST,NYRS,NCPTS,NWROUT,NREOUT,
     +                               CR1,CR2,CR3,CR4
70    Format(8I6,F8.0)
      If(STATUS.NE.0) Then
         Write(20,80)
80       Format(' ERROR: Fortran IOSTAT error occurred reading 5th',
     +          ' record of SIM output file.')
         NERROR=NERROR+1
      Endif
      Write(20,70) YRST,NYRS,NCPTS,NWROUT,NREOUT,CR1,CR2,CR3,CR4
      If(CR1.EQ.0) Then
         NR=5+(12*NYRS*(NCPTS+NWROUT+NREOUT))
         Write(20,90) NR
90       Format(' Total number of records expected based on 5th',
     +          ' record:',I9)
      Endif
      Write(20,*) ' '
!
!   All records remaining after the 5th record are read and checked.
!
      R=5
      Do 380 YR=1,NYRS
         YEAR=YRST+YR-1
         Write(*,95) YEAR
95       Format(4x,'WRAP-SIM output file records are being checked ',
     +             'for year',I5)
         Do 370 M=1,12
!
!   Water right output records.
!
            If(NWROUT.GT.0) Then
               If(M.EQ.1) Write(20,100) YEAR
100            Format('-+- Water right records are read for year',I5)
               Do I=1,NWROUT
                  A=' '
                  R=R+1
                  Read(4,110,REC=R,IOSTAT=STATUS) (A(J),J=1,4),
     +                 MR,(A(J),J=7,70),A3,A4,(A(J),J=103,110)
110               Format(4A1,I2,77A1,A16,A16,11A1)
                  If(STATUS.NE.0) Then
                     Write(20,120) YEAR
120                  Format(' ERROR: Fortran IOSTAT error occurred',
     +                        ' on water right record in year',I5)
                     NERROR=NERROR+1
                  Endif
                  FLAG=0
                  Do J=3,110
                     If(A(J).EQ.' ') Goto 130
                     If(A(J).EQ.'0') Goto 130
                     If(A(J).EQ.'1') Goto 130
                     If(A(J).EQ.'2') Goto 130
                     If(A(J).EQ.'3') Goto 130
                     If(A(J).EQ.'4') Goto 130
                     If(A(J).EQ.'5') Goto 130
                     If(A(J).EQ.'6') Goto 130
                     If(A(J).EQ.'7') Goto 130
                     If(A(J).EQ.'8') Goto 130
                     If(A(J).EQ.'9') Goto 130
                     If(A(J).EQ.'.') Goto 130
                     If(A(J).EQ.'-') Goto 130
                     If(A(J).EQ.'+') Goto 130
                     If(A(J).EQ.'E') Goto 130
                     FLAG=FLAG+1
130               End Do
                  If(FLAG.GE.1) Then
                     Write(20,140)
140                  Format(' ERROR: The following water right',
     +                      ' record has non-numeric data that',
     +                      ' should be numeric.')
                     Write(20,110) (A(J),J=1,4),MR,(A(J),J=7,70),
     +                              A3,A4,(A(J),J=103,110)
                     NERROR=NERROR+1
                  Endif
                  If(A(1).NE.'I'.or.A(2).NE.'F') Then
                     Read(4,150,REC=R,IOSTAT=STATUS) IYEAR,IMONTH
150                  Format(I4,I2)
                     If(STATUS.NE.0) Then
                        Write(20,160) YEAR
160                     Format(' ERROR: Fortran IOSTAT error occurred',
     +                         ' on water right record date in year',I5)
                        NERROR=NERROR+1
                     Endif
                     If(IYEAR.NE.YEAR) Then
                        Write(20,170) IYEAR,YEAR
170                     Format(' ERROR: Read year',I5,
     +                         ' when expecting',I5)
                        Write(20,110) (A(J),J=1,4),MR,(A(J),J=7,70),
     +                                 A3,A4,(A(J),J=103,110)
                        NERROR=NERROR+1
                     Endif
                     If(IMONTH.NE.M) Then
                        Write(20,180) IMONTH,M
180                     Format(' ERROR: Read month',I3,
     +                         ' when expecting month',I3)
                        Write(20,110) (A(J),J=1,4),MR,(A(J),J=7,70),
     +                                 A3,A4,(A(J),J=103,110)
                        NERROR=NERROR+1
                     Endif
                  Endif
               End Do
            Endif
!
!   Control point output record.
!
            If(NCPTS.GT.0) Then
               If(M.EQ.1) Write(20,200) YEAR
200            Format('-+- Control point records are read for year',I5)
               Do I=1,NCPTS
                  A=' '
                  R=R+1
                  Read(4,210,REC=R,IOSTAT=STATUS) A2,(A(J),J=7,136)
210               Format(A6,128A1)
                  If(STATUS.NE.0) Then
                     Write(20,220) YEAR
220                  Format(' ERROR: Fortran IOSTAT error occurred',
     +                      ' on record for control point ',A6)
                     NERROR=NERROR+1
                  Endif
                  FLAG=0
                  Do J=3,110
                     If(A(J).EQ.' ') Goto 230
                     If(A(J).EQ.'0') Goto 230
                     If(A(J).EQ.'1') Goto 230
                     If(A(J).EQ.'2') Goto 230
                     If(A(J).EQ.'3') Goto 230
                     If(A(J).EQ.'4') Goto 230
                     If(A(J).EQ.'5') Goto 230
                     If(A(J).EQ.'6') Goto 230
                     If(A(J).EQ.'7') Goto 230
                     If(A(J).EQ.'8') Goto 230
                     If(A(J).EQ.'9') Goto 230
                     If(A(J).EQ.'.') Goto 230
                     If(A(J).EQ.'-') Goto 230
                     If(A(J).EQ.'+') Goto 230
                     If(A(J).EQ.'E') Goto 230
                     FLAG=FLAG+1
230               End Do
                  If(FLAG.GE.1) Then
                     Write(20,240)
240                  Format(' ERROR: The following control point',
     +                      ' record has non-numeric data that',
     +                      ' should be numeric.')
                     Write(20,210) A2,(A(J),J=7,110)
                     NERROR=NERROR+1
                  Endif
                  If(I.LE.5000) Then
                     If(YR.EQ.1.and.M.EQ.1) Then
                        CP(I)=A2
                     Elseif(CP(I).NE.A2) Then
                        Write(20,250) A2,CP(I)
250                     Format(' ERROR: Read control point ID of ',A6,
     +                         ' when expecting ',A6)
                        NERROR=NERROR+1
                     Endif
                  Endif
               End Do
            Endif
!
!   Reservoir output record.
!
            If(NREOUT.GT.0) Then
               If(M.EQ.1) Write(20,300) YEAR
300            Format('-+- Reservoir/hydropower records are read',
     +                ' for year',I5)
               Do I=1,NREOUT
                  A=' '
                  R=R+1
                  Read(4,310,REC=R,IOSTAT=STATUS) A2,(A(J),J=7,136)
310               Format(A6,128A1)
                  If(STATUS.NE.0) Then
                     Write(20,320) YEAR
320                  Format(' ERROR: Fortran IOSTAT error occurred',
     +                      ' on record for reservoir ',A6)
                     NERROR=NERROR+1
                  Endif
                  FLAG=0
                  Do J=3,110
                     If(A(J).EQ.' ') Goto 330
                     If(A(J).EQ.'0') Goto 330
                     If(A(J).EQ.'1') Goto 330
                     If(A(J).EQ.'2') Goto 330
                     If(A(J).EQ.'3') Goto 330
                     If(A(J).EQ.'4') Goto 330
                     If(A(J).EQ.'5') Goto 330
                     If(A(J).EQ.'6') Goto 330
                     If(A(J).EQ.'7') Goto 330
                     If(A(J).EQ.'8') Goto 330
                     If(A(J).EQ.'9') Goto 330
                     If(A(J).EQ.'.') Goto 330
                     If(A(J).EQ.'-') Goto 330
                     If(A(J).EQ.'+') Goto 330
                     If(A(J).EQ.'E') Goto 330
                     FLAG=FLAG+1
330               End Do
                  If(FLAG.GE.1) Then
                     Write(20,340)
340                  Format(' ERROR: The following control point',
     +                      ' record has non-numeric data that',
     +                      ' should be numeric.')
                     Write(20,310) A2,(A(J),J=7,110)
                     NERROR=NERROR+1
                  Endif
                  If(I.LE.1500) Then
                     If(YR.EQ.1.and.M.EQ.1) Then
                        RES(I)=A2
                     Elseif(RES(I).NE.A2) Then
                        Write(20,350) A2,RES(I)
350                     Format(' ERROR: Read reservoir ID of ',A6,
     +                         ' when expecting ',A6)
                     NERROR=NERROR+1
                     Endif
                  Endif
               End Do
            Endif
!
!   Limit on number of error messages written.
!
            If(NERROR.GE.3000) Then
               Write(20,360) NERROR
360            Format(/,' TEST stopped upon reaching the limit of',
     +                ' 3,000 error messages.')
               Stop
               Return
            Endif
!
!   End of annual and monthly loops.
!
370      End Do
380   End Do
!
!   Record count comparison and error count.
!
      Write(20,400)
400   Format('-+- Record count:')
      Write(20,410) R
410   Format(' Number of records found in output file: ',I16)
      Write(20,420) NR
420   Format(' Number of records expected based on 5th record:',I9)
      If(NERROR.EQ.0) Then
         Write(20,430)
430      Format(' No errors were found.')
      Else
         Write(20,440) NERROR
440      Format(I5,' error messages were written.')
      Endif
!
      Write(20,450)
450   Format('-+- Completion of TABLES test of SIM output file.')
!
!   TABLES TEST record is read again now to prevent re-activation
!   in the main program.
!
      Read(1,460) CD
460   Format(A4)
      Write(*,*) ' '
      Write(*,470)
470   Format(4x,'Trace and error messages generated by TEST')
      Write(*,480)
480   Format(4x,'are written to the TABLES message (TMS) file.')
!
!  Return to main program from Subroutine TEST.
!
      Return
      End Subroutine TEST
!
!  ***********************************************************************
!
      Subroutine LENGTH
!
!   *-*-*-*-*-* 1LEN RECORD *-*-*-*-*-*
!   Subroutine DELETE performs error checks on the SIM output OUT file.
!
      Character(len=2) CD
      Character(len=126) A126
10    Read(1,20,End=30) CD,A126
20    Format(A2,A126)
      Write(2,20) CD,A126
      If(CD.NE.'ED') Goto 10
30    Return
      End Subroutine LENGTH
!
!  ***********************************************************************
!
      Subroutine SUMDAT
!
!   *-*-*-*-*-*  1REC, 1SUM, 1SRT RECORD  *-*-*-*-*-*
!   Subroutine SUMDAT builds input data summaries.
!
      Use COMVAR
!
      Integer I,II,IFF,J,K,KK,MATCH,NRES,NRIGHT,PRHI,PRLO,N,NGOUT,NUM,
     +     NUSES,NUMCP,RESFLG,RESNUM(MAXRES),RANK(MAXWR),RESSEC(MAXWR),
     +     SUMWRT,SUMRES,SCRNUM,TPRHI,TPRLO,WR,WRNUM(MAXWR,6)
!
      Real INACT,RESDAT(MAXRES),STOR,SUMDIV,SUMSTO,TOTDIV,TOTSTO,
     +     WRDAT(MAXWR,4),XDIV,XHP
!
      Character(len=2) RD,REC(50)
      Character(len=4) JOBT
      Character(len=6) CP,CPID(MAXCP),RES,RESID(MAXRES),TUSE,USEID(300)
      Character(len=8) GROUP(300),GP1,GP2,GP
      Character(len=16) WRID(MAXWR)
      Character(len=126) LINE
!
      IFF=0
!
!   TIN file record identifier is read.
!
      Read(1,10,IOSTAT=STATUS) JOBT,KK,IFF
10    Format(A4,2I4)
      If(STATUS.NE.0) Then
         Write(20,20) JOBT
20       Format(/,' ERROR: Fortran IOSTAT error occured reading a',
     +            ' input record with CD of ',A4)
         Call ERROR
      Endif
!
!   1REC record listing of DAT file records.
!
      If(JOBT.EQ.'1REC') Then
         Rewind(3)
         Backspace(1)
         NUM=IFF
         K = 1
         N = NUM
         If(NUM.GT.16) N = 16
30       Read(1,40) JOBT,KK,(REC(I),I=K,N)
40       Format(A4,I4,4x,16A4)
         If(NUM.GT.N) Then
            K = K + 16
            N = N + 16
            If(N.GT.NUM) N = NUM
            Goto 30
         Endif
!
         Call TITLES
         Write(2,50)
50       Format(3x,'LISTING OF SELECTED RECORDS FROM WRAP INPUT FILE'//)
!
60       Read(3,70,End=90) RD,LINE
70       Format(A2,A126)
         Do I=1,NUM
            If(KK.EQ.0) Then
               If(RD.EQ.REC(I)) Write(2,80) RD,LINE
80             Format(A2,A126)
            Else
               If(RD.NE.REC(I)) Write(2,80) RD,LINE
            Endif
         End Do
         If(RD.NE.'ED') Goto 60
90       Return
      Endif
!
!   1SRT and 1SUM record tables.
!
      If(JOBT.EQ.'1SRT'.or.JOBT.EQ.'1SUM') Then
         Rewind(3)
         MATCH = 0
!
!   Error checks for KK from input record.
!
         If(KK.LT.0.or.KK.GT.4) Then
            Write(20,110) KK,JOBT
110         Format(' ERROR: KK of',I3,' on ',A4,
     +             ' record is not valid.')
            Call ERROR
         Endif
         If(KK.EQ.4.and.NGOUT.EQ.0) Then
            Write(20,120) JOBT
120         Format(' WARNING: KK of 4 on ',A4,' record is not valid',
     +             ' without a GO record in DAT file.')
            Return
         Endif
!
!   Initialize data.
!
         Do I=1,MAXWR
            Do J=1,6
               WRNUM(I,J) = 0
            End Do
            Do J=1,4
               WRDAT(I,J) = 0.0
            End Do
            RESSEC(I)=0
         End Do
         Do I=1,MAXRES
            RESNUM(I)=0
            RESDAT(I)=0.0
         End Do
         NGOUT=0
!
!   DAT file records are read.
!
140      Format(A2)
150      Format(A2,A6)
160      Read(3,140) RD
         If(RD.EQ.'**'.or.RD.EQ.'T1'.or.RD.EQ.'T2'.or.RD.EQ.'T3'.or.
     +      RD.EQ.'JD'.or.RD.EQ.'JO'.or.RD.EQ.'FO'.or.RD.EQ.'CO'.or.
     +      RD.EQ.'RO'.or.RD.EQ.'WO'.or.RD.EQ.'FY'.or.RD.EQ.'XL'.or.
     +      RD.EQ.'CR'.or.RD.EQ.'OF'.or.RD.EQ.'ZZ'.or.RD.EQ.'RG') Then
            Goto 160
!
!   Read group identifiers.
!
         Elseif(RD.EQ.'GO') Then
            MATCH = MATCH + 1
            Backspace(3)
            Read(3,170) RD,NGOUT,(GROUP(J),J=1,5)
170         Format(A2,I6,5A8)
            If(NGOUT.GT.5) Then
               K = INT((NGOUT-1)/5)+1
               Do 190 I=1,K-1
                  Read(3,180) RD,(GROUP(J),J=(5*I+1),(5*I+5))
180               Format(A2,6X,5A8)
                  If(RD.NE.'GO') Then
                     MATCH = 0
                     Print*,' Warning: Missing GO Record'
                  Endif
190            End Do
            Endif
            Do J=1,NGOUT
               GROUP(J)=Adjustr(GROUP(J))
            End Do
            Goto 160
         Endif
!
!   Read water use type identifiers.
!
         If(RD.NE.'UC'.and.RD.NE.'**'.and.RD.NE.'CP'.and.
     +      RD.NE.'UP'.and.RD.NE.'RF') Then
            Write(20,200) RD
200         Format(' WARNING: Expecting to read UC or CP record,',
     +             ' but instead read CD of: ',A2)
            Goto 160
         Endif
         NUSES = 0
         If(RD.EQ.'UC') Then
210         Backspace(3)
            Read(3,150) RD,TUSE
            If(TUSE.NE.'      ') Then
               NUSES = NUSES + 1
               USEID(NUSES)=Adjustr(TUSE)
            Endif
215         Read(3,140) RD
            If(RD.EQ.'**') Goto 215
            If(RD.EQ.'UC') Goto 210
         Endif
!
!        Constant use distribution for blank use type on WR record.
!
         NUSES=NUSES+1
         USEID(NUSES)='      '
         NUSES=NUSES+1
         USEID(NUSES)=' NDAYS'
!
!   Read control point identifiers.
!
220      Backspace(3)
         NUMCP = 0
230      Read(3,150) RD,CPID(NUMCP+1)
         If(RD.EQ.'RF'.or.RD.EQ.'**'.or.RD.EQ.'UP') Goto 230
         If(RD.EQ.'CP') Then
            CPID(NUMCP+1)=Adjustr(CPID(NUMCP+1))
            NUMCP = NUMCP + 1
            Goto 230
         Endif
         If(NUMCP.LE.0) Then
            Write(20,240) RD
240         Format(/,' ERROR: Failed to read at least one CP record.',
     +             ' Last CD read: ',A2)
            Call ERROR
         Endif
!
!   Read water right data.
!
         Backspace(3)
         NRIGHT = 0
         NRES = 0
250      Read(3,140) RD
         If(RD.EQ.'**'.or.RD.EQ.'CI'.or.RD.EQ.'BU'.or.RD.EQ.'OR'
     +     .or.RD.EQ.'ML'.or.RD.EQ.'SO'.or.RD.EQ.'PX'.or.RD.EQ.'AX'
     +     .or.RD.EQ.'TO'.or.RD.EQ.'TS'.or.RD.EQ.'FS') Goto 250
         If(RD.EQ.'PV'.or.RD.EQ.'SV'.or.RD.EQ.'ED') Goto 350
         If(RD.EQ.'TE'.or.RD.EQ.'TQ'.or.RD.EQ.'MS') Goto 350
         If(RD.EQ.'DI'.or.RD.EQ.'EA') Goto 350
         If(RD.EQ.'IF'.and.JOBT.EQ.'1SUM') Goto 250
         If(RD.EQ.'IF'.and.IFF.EQ.0) Goto 250
         If(RD.EQ.'WR'.or.RD.EQ.'IF') Then
            Backspace(3)
            RESFLG = 0
            NRIGHT = NRIGHT + 1
            WRDAT(NRIGHT,2)=0.0
            If(RD.EQ.'WR') Then
               Read(3,260) CP,WRDAT(NRIGHT,1),TUSE,WRNUM(NRIGHT,1),
     +                     WRNUM(NRIGHT,2),WRID(NRIGHT),GP1,GP2
260            Format(2x,A6,F8.0,2x,A6,I8,I4,28x,A16,2A8)
            Elseif(RD.EQ.'IF') Then
               Read(3,265) CP,WRDAT(NRIGHT,1),TUSE,WRNUM(NRIGHT,1),
     +                     WRID(NRIGHT)
265            Format(2x,A6,F8.0,2x,A6,I8,16x,A16)
               WRNUM(NRIGHT,2) = 99
               GP1='        '
               GP2='        '
            Endif
            CP=Adjustr(CP)
            TUSE=Adjustr(TUSE)
            WRID(NRIGHT)=Adjustr(WRID(NRIGHT))
            GP1=Adjustr(GP1)
            GP2=Adjustr(GP2)
            If(WRNUM(NRIGHT,2).EQ.0)  WRNUM(NRIGHT,2) = 1
            If(WRNUM(NRIGHT,2).EQ.-1) WRNUM(NRIGHT,2) = 5
            If(WRNUM(NRIGHT,2).EQ.-3) WRNUM(NRIGHT,2) = 6
            Do I=1,NUMCP
               If(CP.EQ.CPID(I)) Then
                  WRNUM(NRIGHT,3) = I
                  Goto 270
               Endif
            End Do
270         Do I=1,NUSES
               If(TUSE.EQ.USEID(I)) Then
                  WRNUM(NRIGHT,4) = I
                  Goto 280
               Endif
            End Do
280         Do I=1,NGOUT
               If(GP1.EQ.GROUP(I).or.GP2.EQ.GROUP(I)) Then
                  WRNUM(NRIGHT,5) = I
                  Goto 250
               Endif
            End Do
!
!        Reservoir data are read.
!
         Elseif(RD.EQ.'WS') Then
            Backspace(3)
            Read(3,300) RES,STOR,INACT
300         Format(2X,A6,F8.0,24x,F8.0)
            RES=Adjustr(RES)
            Do 310 I=1,NRES
               If(RES.EQ.RESID(I)) Then
                  If(STOR.GT.RESDAT(I)) RESDAT(I) = STOR
                  Read(3,140) RD
                  If(RD.NE.'OR') Backspace(3)
                  Goto 340
               Endif
310         End Do
            NRES = NRES + 1
            RESID(NRES) = RES
            RESDAT(NRES) = STOR
!
!        Check CP identifiers for reservoir location identifier
!        If there is not a match, Then the reservoir is located
!        at the same control point as the water right.
!
            Read(3,140) RD
            Backspace(3)
            If(RD.EQ.'OR') Then
               Read(3,320) CP
320            Format(2X,A6)
               Do J=1,NUMCP
                   If(CP.EQ.CPID(J)) Then
                      RESNUM(NRES) = J
                      Goto 330
                   Endif
               End Do
            Endif
            RESNUM(NRES) = WRNUM(NRIGHT,3)
330         I = NRES
!
!        Primary reservoir associated with a type 1 or hydropower right.
!
340         If(RESFLG.EQ.0.and.(WRNUM(NRIGHT,2).EQ.1.or.
     +            WRNUM(NRIGHT,2).EQ.5.or.WRNUM(NRIGHT,2).EQ.7)) Then
               RESFLG = 1
               WRNUM(NRIGHT,6) = I
               WRDAT(NRIGHT,2) = STOR
               WRDAT(NRIGHT,3) = INACT
            Else
               RESFLG = RESFLG + 1
               WRDAT(NRIGHT,4) = WRDAT(NRIGHT,4) + STOR - INACT
               RESSEC(NRIGHT)=RESFLG
            Endif
         Endif
!
!   Read the next input record.
!
         Goto 250
!
!   Rank water rights by priority numbers.
!
350      Do I=1,MAXWR
            RANK(I)=I
         End Do
         Do I=1,NRIGHT-1
            Do II=I+1,NRIGHT
               If((WRNUM(RANK(II),1).LT.WRNUM(RANK(I),1)).or.
     +            (WRNUM(RANK(II),1).EQ.WRNUM(RANK(I),1).and.
     +             RANK(II).LT.RANK(I))) Then
                  K=RANK(II)
                  RANK(II)=RANK(I)
                  RANK(I)=K
               Endif
            End Do
         End Do
      Endif
!
!   Develop the specified 1SUM tables.
!
      If(JOBT.EQ.'1SUM') Then
         Call TITLES
         TOTDIV = 0.0
         TOTSTO = 0.0
         TPRHI = -1
         TPRLO = 99999999
         If(KK.LT.0.or.KK.GT.4) Then
            Write(20,390) KK
390         Format(/,' ERROR: KK of',I3,' is not valid.')
            Call ERROR
         Endif
         If(KK.EQ.1.or.KK.EQ.0) Then
            SCRNUM = NUMCP
            Write(2,410)
            Write(2,460) UNIT,UNIT
            If(NUMCP.LE.0) Then
               Write(20,400)
400            Format(/,' ERROR: KK is 0 or 1 but zero control point ',
     +                  'records found in WRAP-SIM input file.')
               Call ERROR
            Endif
         Endif
         If(KK.EQ.2) Then
            SCRNUM = NUSES
            Write(2,420)
            Write(2,450) UNIT,UNIT
         Endif
         If(KK.EQ.3) Then
            SCRNUM = 7
            Write(2,430)
            Write(2,470) UNIT,UNIT
         Endif
         If(KK.EQ.4) Then
            SCRNUM = NGOUT
            Write(2,440)
            Write(2,480) UNIT,UNIT
         Endif
410   Format('WATER RIGHTS INPUT DATA SUMMARY BY CONTROL POINT')
420   Format('WATER RIGHTS INPUT DATA SUMMARY BY USE')
430   Format('WATER RIGHTS INPUT DATA SUMMARY BY TYPE')
440   Format('WATER RIGHTS INPUT DATA SUMMARY BY GROUP')
450   Format('Reservoirs and reservoir storage may be',
     +       ' "double-counted" if not listed by control point.'/,
     +       87('-'),/,12X,'NUMBER',6X,'PERMITTED',6X,'NUMBER',
     +       7X,'RESERVOIR',8X,'PRIORITIES',/,14X,'OF',
     +       7X,'DIVERSIONS',8X,'OF',10X,'STORAGE',12X,'RANGE'/,
     +       ' USE',8X,'RIGHTS',5X,'(',A5,'/YR)',4X,'RESERVOIRS',
     +       6X,'(',A5,')',7X,'FROM',7X,'TO',/,87('-'))
460   Format('Reservoirs and reservoir storage may be',
     +       ' "double-counted" if not listed by control point.'/,
     +       87('-'),/,12X,'NUMBER',6X,'PERMITTED',6X,'NUMBER',
     +       7X,'RESERVOIR',8X,'PRIORITIES',/,'CONTROL',7X,'OF',
     +       7X,'DIVERSIONS',8X,'OF',10X,'STORAGE',12X,'RANGE'/,
     +       1X,'POINT',6X,'RIGHTS',5X,'(',A5,'/YR)',4X,'RESERVOIRS',
     +       6X,'(',A5,')',7X,'FROM',7X,'TO',/,87('-'))
470   Format('Reservoirs and reservoir storage may be',
     +       ' "double-counted" if not listed by control point.'/,
     +       87('-'),/,12X,'NUMBER',6X,'PERMITTED',6X,'NUMBER',
     +       7X,'RESERVOIR',8X,'PRIORITIES',/,14X,'OF',
     +       7X,'DIVERSIONS',8X,'OF',10X,'STORAGE',12X,'RANGE'/,
     +       2X,'TYPE',6X,'RIGHTS',5X,'(',A5,'/YR)',4X,'RESERVOIRS',
     +       6X,'(',A5,')',7X,'FROM',7X,'TO',/,87('-'))
480   Format('Reservoirs and reservoir storage may be',
     +       ' "double-counted" if not listed by control point.'/,
     +       87('-'),/,12X,'NUMBER',6X,'PERMITTED',6X,'NUMBER',
     +       7X,'RESERVOIR',8X,'PRIORITIES',/,14X,'OF',
     +       7X,'DIVERSIONS',8X,'OF',10X,'STORAGE',12X,'RANGE'/,
     +       1X,'GROUP',6X,'RIGHTS',5X,'(',A5,'/YR)',4X,'RESERVOIRS',
     +       6X,'(',A5,')',7X,'FROM',7X,'TO',/,87('-'))
490   Format(3X,I2,7X,I5,7X,F9.0,6X,I5,6X,F11.0,4X,I8,2X,I8)
500   Format(A6,6X,I5,7X,F9.0,6X,I5,6X,F11.0,4X,I8,2X,I8)
505   Format(A8,4X,I5,7X,F9.0,6X,I5,6X,F11.0,4X,I8,2X,I8)
510   Format(' SUM',8X,I5,5X,F11.0,6X,I5,6X,F11.0,4X,I8,2X,I8)
520   Format(87('-'))
!
         Do 540 I=1,SCRNUM
            SUMRES = 0
            SUMWRT = 0
            SUMDIV = 0.
            SUMSTO = 0.
            PRHI = -1
            PRLO = 99999999
            Do 530 J=1,NRIGHT
               If((KK.LE.1.and.WRNUM(J,3).EQ.I).or.
     +            (KK.EQ.2.and.WRNUM(J,4).EQ.I).or.
     +            (KK.EQ.3.and.ABS(WRNUM(J,2)).EQ.I).or.
     +            (KK.EQ.4.and.WRNUM(J,5).EQ.I)) Then
                  SUMWRT = SUMWRT + 1
                  If(WRNUM(J,2).GE.0.and.WRNUM(J,2).LE.4) Then
                     TOTDIV = TOTDIV + WRDAT(J,1)
                     SUMDIV = SUMDIV + WRDAT(J,1)
                  Endif
                  If(PRHI.LT.WRNUM(J,1))  PRHI = WRNUM(J,1)
                  If(TPRHI.LT.WRNUM(J,1)) TPRHI = WRNUM(J,1)
                  If(PRLO.GT.WRNUM(J,1))  PRLO = WRNUM(J,1)
                  If(TPRLO.GT.WRNUM(J,1)) TPRLO = WRNUM(J,1)
                  If(KK.LT.2) Goto 530
                  SUMRES = SUMRES + 1
                  SUMSTO = SUMSTO + WRDAT(J,2)
               Endif
530         End Do
            If(KK.LE.1.or.I.EQ.SCRNUM) Then
               Do J=1,NRES
                  If(KK.LE.1.and.I.EQ.RESNUM(J)) Then
                     SUMRES = SUMRES + 1
                     SUMSTO = SUMSTO + RESDAT(J)
                  Endif
                  If(I.EQ.SCRNUM) TOTSTO = TOTSTO + RESDAT(J)
               End Do
            Endif
            If(KK.EQ.1.and.SUMWRT.GT.0) Write(2,500) Adjustl(CPID(I)),
     +                     SUMWRT,SUMDIV,SUMRES,SUMSTO,PRLO,PRHI
            If(KK.EQ.2.and.SUMWRT.GT.0) Write(2,500) Adjustl(USEID(I)),
     +                     SUMWRT,SUMDIV,SUMRES,SUMSTO,PRLO,PRHI
            If(KK.EQ.3.and.SUMWRT.GT.0) Write(2,490) I,SUMWRT,
     +                     SUMDIV,SUMRES,SUMSTO,PRLO,PRHI
            If(KK.EQ.4) Write(2,505) Adjustl(GROUP(I)),SUMWRT,
     +                     SUMDIV,SUMRES,SUMSTO,PRLO,PRHI
540      End Do
         Write(2,510) NRIGHT,TOTDIV,NRES,TOTSTO,TPRLO,TPRHI
         Write(2,520)
      Endif
!
!   Develop the specified 1SRT tables.
!
      If(JOBT.EQ.'1SRT') Then
!
!   Write heading.
!
         Call TITLES
         If(KK.EQ.0) Then
            SCRNUM = 0
            Write(2,590)
         Endif
         If(KK.EQ.1) Then
            SCRNUM = NUMCP
            Write(2,550)
         Elseif(KK.EQ.2) Then
            SCRNUM = NUSES
            Write(2,560)
         Elseif(KK.EQ.3) Then
            SCRNUM = 7
            Write(2,570)
         Elseif(KK.EQ.4) Then
            SCRNUM = NGOUT
            Write(2,580)
         Endif
550      Format('WATER RIGHTS INPUT DATA IN PRIORITY ORDER SORTED',
     +          ' BY CONTROL POINT')
560      Format('WATER RIGHTS INPUT DATA IN PRIORITY ORDER SORTED',
     +          ' BY USE')
570      Format('WATER RIGHTS INPUT DATA IN PRIORITY ORDER SORTED',
     +          ' BY TYPE')
580      Format('WATER RIGHTS INPUT DATA IN PRIORITY ORDER SORTED',
     +          ' BY GROUP')
590      Format('WATER RIGHTS INPUT DATA IN PRIORITY ORDER')
!
         Write(2,600)
600      Format(105('-'))
         Write(2,610)
610      Format('WATER RIGHT',5x,'WR',35x,'HYDRO ',
     +          '!+++PRIMARY RESERVOIR++++!-SECONDARY-!   WR')
         Write(2,620)
620      Format(' IDENTIFIER',4x,'TYPE PRIORITY   CP    USE  DIVERSION',
     +       '  POWER',4x,'ID',5x,'TOTAL  INACTIVE NUM  ACTIVE   GROUP')
         Write(2,630) UNIT,UNHP,UNIT,UNIT,UNIT
630      Format(41x,'(',A5,'/YR)',1X,'(',A5,')',9x,'(',A5,')  ',
     +              '(',A5,')',5x,'(',A5,')')
         Write(2,600)
!
!   Write data to water right listing in table.
!
         If(KK.EQ.0) Then
            Do J=1,NRIGHT
               WR = RANK(J)
               If(WRNUM(WR,2).EQ.5.or.WRNUM(WR,2).EQ.6) Then
                  XDIV=0.0
                  XHP=WRDAT(WR,1)
               Else
                  XDIV=WRDAT(WR,1)
                  XHP=0.0
               Endif
               If(WRNUM(WR,5).GT.0) Then
                  GP=Adjustl(GROUP(WRNUM(WR,5)))
               Else
                  GP='        '
               Endif
               If(WRNUM(WR,6).GT.0) Then
                  RES=Adjustl(RESID(WRNUM(WR,6)))
               Else
                  RES='      '
               Endif
               If(WRNUM(WR,4).GT.0) Then
                  TUSE=Adjustl(USEID(WRNUM(WR,4)))
               Else
                  TUSE='      '
               Endif
               Write(2,640) Adjustl(WRID(WR)),WRNUM(WR,2),WRNUM(WR,1),
     +                 Adjustl(CPID(WRNUM(WR,3))),TUSE,XDIV,XHP,RES,
     +                 WRDAT(WR,2),WRDAT(WR,3),RESSEC(WR),WRDAT(WR,4),GP
640            Format(A16,I2,I10,1x,A6,1x,A6,F9.1,F8.0,1x,A6,2F9.1,I3,
     +                F9.0,1x,A8)
            End Do
         Else
            Do 650 I=1,SCRNUM
               Do J=1,NRIGHT
                  WR=RANK(J)
                  If((KK.EQ.1.and.WRNUM(WR,3).EQ.I).or.
     +               (KK.EQ.2.and.WRNUM(WR,4).EQ.I).or.
     +               (KK.EQ.3.and.Abs(WRNUM(WR,2)).EQ.I).or.
     +               (KK.EQ.4.and.WRNUM(WR,5).EQ.I).or.
     +               (KK.EQ.3.and.I.EQ.7.and.WRNUM(WR,2).EQ.99)) Then
                     If(WRNUM(WR,2).EQ.5.or.WRNUM(WR,2).EQ.6) Then
                        XDIV=0.0
                        XHP=WRDAT(WR,1)
                     Else
                        XDIV=WRDAT(WR,1)
                        XHP=0.0
                     Endif
                     If(WRNUM(WR,5).GT.0) Then
                        GP=Adjustl(GROUP(WRNUM(WR,5)))
                     Else
                        GP='        '
                     Endif
                     If(WRNUM(WR,6).GT.0) Then
                        RES=Adjustl(RESID(WRNUM(WR,6)))
                     Else
                        RES='      '
                     Endif
                     If(WRNUM(WR,4).GT.0) Then
                        TUSE=Adjustl(USEID(WRNUM(WR,4)))
                     Else
                        TUSE='      '
                     Endif
                     Write(2,640) Adjustl(WRID(WR)),WRNUM(WR,2),
     +                      WRNUM(WR,1),Adjustl(CPID(WRNUM(WR,3))),TUSE,
     +                      XDIV,XHP,RES,WRDAT(WR,2),WRDAT(WR,3),
     +                      RESSEC(WR),WRDAT(WR,4),GP
                  Endif
               End Do
650         End Do
         Endif
         Write(2,600)
      Endif
!
!  Return to main program from Subroutine SUMDAT.
!
660   Return
      End Subroutine SUMDAT
!
!  ***********************************************************************
!
      Subroutine CONPTS
!
!   *-*-*-*-*-*  1CPT RECORD  *-*-*-*-*-*
!   Subroutine CONPTS lists control points in upstream-to-downstream order.
!
      Use COMVAR
      Integer CP,I,J,K,KK,L,OO,IORDER,LIM,NCP
      Integer INMETHOD(MAXCP),NUP(MAXCP),NUPC(MAXCP),ORDER(MAXCP)
!
      Real CL(MAXCP),DA(MAXCP),DAWP
!
      Character(len=2)  CD
      Character(len=4)  JOBT
      Character(len=6)  CPID(MAXCP,2),CPUS(70),CPWP,CPLIM
      Character(len=14) LINE1(MAXCP)
      Character(len=24) LINE2(MAXCP)
      Character(len=64) LINE(MAXCP)
!
!   1CPT record is read from TABLES input file.
!
      KK=0
      OO=0
      LIM=0
      CPLIM='      '
      Read(1,10,IOSTAT=STATUS) JOBT,KK,OO,LIM,CPLIM
10    Format(A4,I4,2I8,2x,A6)
      If(STATUS.NE.0) Then
         Write(20,20) JOBT
20       Format(/,' ERROR: Fortran IOSTAT error occured reading a',
     *          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(JOBT.NE.'1CPT') Then
         Write(20,30) JOBT
30       Format(/,' ERROR: Read a CD of ',A4,' when expecting 1CPT')
         Call ERROR
      Endif
      CPLIM=Adjustr(CPLIM)
!
!   CP records are read from WRAP-SIM input file.
!
      NCP=0
      Rewind(3)
40    Read(3,50,End=110) CD
50    Format(A2)
      If(CD.EQ.'T1'.or.CD.EQ.'T2'.or.CD.EQ.'T3'.or.CD.EQ.'**') Goto 40
      If(CD.EQ.'FO'.or.CD.EQ.'JD'.or.CD.EQ.'JO'.or.CD.EQ.'  ') Goto 40
      If(CD.EQ.'XL'.or.CD.EQ.'FY'.or.CD.EQ.'CR'.or.CD.EQ.'ZZ') Goto 40
      If(CD.EQ.'CO'.or.CD.EQ.'RO'.or.CD.EQ.'WO'.or.CD.EQ.'GO') Goto 40
      If(CD.EQ.'UC'.or.CD.EQ.'UD'.or.CD.EQ.'UP'.or.CD.EQ.'RF') Goto 40
      If(CD.NE.'WR'.and.CD.NE.'IF'.and.CD.NE.'CI'.and.CD.NE.'ED') Then
         NCP=NCP+1
         Backspace(3)
         If(KK.EQ.4.or.KK.EQ.5.or.KK.EQ.6) Then
            Read(3,60,IOSTAT=STATUS)CD,CPID(NCP,1),CPID(NCP,2),
     +                LINE1(NCP),INMETHOD(NCP),LINE2(NCP),CL(NCP)
60          Format(A2,A6,2x,A6,A16,I8,A24,F8.4)
         Elseif(KK.EQ.0) Then
            Read(3,70,IOSTAT=STATUS)CD,CPID(NCP,1),CPID(NCP,2),
     +                LINE(NCP)
70          Format(A2,A6,2x,A6,A64)
         Else
            Read(3,70,IOSTAT=STATUS)CD,CPID(NCP,1),CPID(NCP,2)
         Endif
         If(STATUS.NE.0) Then
            Write(20,80) CD
80          Format(/,' ERROR: Fortran IOSTAT error occurred reading',
     +            ' CP records from SIM input file. Last CD read: ',A2)
            Call ERROR
         Endif
         If(CD.NE.'CP') Then
            Write(20,90) CD
90          Format(/,' ERROR: Read a CD of ',A2,' when expecting CP.')
            Call ERROR
         Endif
         CPID(NCP,1)=Adjustr(CPID(NCP,1))
         CPID(NCP,2)=Adjustr(CPID(NCP,2))
         If(LIM.NE.0.and.KK.LE.3) Then
            Backspace(3)
            Read(3,100) INMETHOD(NCP)
100         Format(32x,I8)
         Endif
         Go to 40
      Endif
!
!   If 1CPT record field 5 CPLIM option limits listing to control points
!   located downstream of CPLIM, skip to statement
!
110   If(CPLIM.NE.'      ') Goto 250
!
!   Order index and order array are initialized as zero along with
!   the NUP(I) array of number of upstream control points and the
!   NUPC(I) counter of the number already ordered during the loop.
!
      IORDER=0
      Do I=1,NCP
         ORDER(I)=0
         NUP(I)=0
         NUPC(I)=0
      End Do
!
!   NUP(I), the number of control points located immediately upstream is
!   counted for each control point.
!
      Do I=1,NCP
         Do J=1,NCP
            If(CPID(J,2).EQ.CPID(I,1)) NUP(I)=NUP(I)+1
         End Do
      End Do
!
!   Control points are sorted sequentially in upstream-to-downstream order,
!   based on the order option specified by OO from the 1CPT record field 3.
!
!         The most-upstream control point on the next branch is found.
!
120   J=0
130   J=J+1
      If(J.GT.NCP) Then
         Write(20,140)
140      Format(/,' ERROR: CP index J exceeds NCP in the',
     +            ' reordering routine.')
         Call ERROR
      Endif
      If(NUP(J).NE.0.or.ORDER(J).NE.0) Go to 130
      IORDER=IORDER+1
      ORDER(J)=IORDER
      If(IORDER.EQ.NCP) Goto 200
      If(CPID(J,2).EQ.'   OUT'.or.CPID(J,2).EQ.'      ') Goto 120
!
!         The ordering of control points cascades downstream.
!
150   K=0
160   K=K+1
      If(K.EQ.J) Goto 160
      If(K.GT.NCP) Then
         Write(20,170)
170      Format(/,' ERROR: CP index K exceeds NCP in the',
     +            ' reordering routine.')
         Call ERROR
      Endif
      If(CPID(K,1).EQ.CPID(J,2)) Then
         If(ORDER(K).NE.0) Goto 120
!
!         Upon reaching a confluence, the most-upstream cp on another
!         tributary upstream of that confluence is found.
!
         If(OO.LE.1.and.(NUPC(K)+1).LT.NUP(K)) Then
            NUPC(K)=NUPC(K)+1
180         J=0
190         J=J+1
            If(J.GT.NCP) Then
               Write(20,195)
195            Format(/,' ERROR: CP index J exceeds NCP in the',
     +                  ' reordering algorithm.')
               Call ERROR
            Endif
            If(ORDER(J).NE.0) Goto 190
            If(CPID(J,2).EQ.CPID(K,1)) Then
               If(NUP(J).EQ.0) Then
                  IORDER=IORDER+1
                  ORDER(J)=IORDER
                  If(IORDER.EQ.NCP) Goto 200
                  Goto 150
               Else
                  K=J
                  Goto 180
               Endif
            Else
               Goto 190
            Endif
         Endif
!
!         The ordering of control points continues to cascade on downstream.
!
         IORDER=IORDER+1
         ORDER(K)=IORDER
         If(IORDER.EQ.NCP) Goto 200
         If(CPID(K,2).EQ.'   OUT'.or.CPID(K,2).EQ.'      ') Goto 120
         J=K
         Goto 150
      Else
         Goto 160
      Endif
!
!   ICP array is created to connect upstream-downstream to read-in sequences.
!   ICP(I) is the order in which the control points are read from the WRAP-SIM
!   input file. The array index I represents the upstream-to-downstream order.
!
200   Do 220 I=1,NCP
         If(ORDER(I).EQ.0) Then
            Write(20,210) CPID(I,1)
210         Format(' WARNING: Control point ',A6,' was omitted in ',
     +                      'the reordering algorithm.')
         Endif
         ICP(I)=I
220   End Do
      Do 240 I=1,NCP-1
         Do 230 J=I+1,NCP
            If(ORDER(ICP(J)).LT.ORDER(ICP(I))) Then
               K=ICP(J)
               ICP(J)=ICP(I)
               ICP(I)=K
            Endif
230      End Do
240   End Do
!
!   Alternative routine used if 1CPT record field 5 CPLIM option limits
!   listing to control points located downstream of CPLIM.
!
250   If(CPLIM.NE.'      ') Then
         IORDER=1
         K=0
260      K=K+1
         If(K.GT.NCP) Then
            Write(20,265) CPLIM
265         Format(/,' ERROR: CPLIM ',A6,' from 1CPT record field 5',
     +               ' is not found in the CP records.')
            Call ERROR
         Endif
         If(CPID(K,1).NE.CPLIM) Goto 260
         ICP(IORDER)=K
270      J=0
280      J=J+1
         If(J.GT.NCP) Then
            Write(20,195)
            Call ERROR
         Endif
         If(CPID(J,1).NE.CPID(K,2)) Goto 280
         IORDER=IORDER+1
         ICP(IORDER)=J
         K=J
         If(CPID(J,2).NE.'   OUT'.and.CPID(J,2).NE.'      ') Goto 270
         NCP=IORDER
      Endif
!
!   Rearranged control points are written to output file.
!
!         Options 0, 1, 2, 3, or 4 from 1CPT record field 2.
!
      If(KK.LE.4) Then
         Do 390 I=1,NCP
            CP=ICP(I)
            If(NUP(CP).EQ.0.and.CPLIM.EQ.'      ') Write(2,290)
290         Format('***** Most upstream control point on a stream.')
            If(LIM.LT.0.and.INMETHOD(CP).GE.2) Goto 390
            If(KK.EQ.0) Then
               Write(2,300) CPID(CP,1),CPID(CP,2),LINE(CP)
300            Format('CP',A6,2x,A6,A64)
            Elseif(KK.EQ.1) Then
               Write(2,310) CPID(CP,1)
310            Format(A6)
            Elseif(KK.EQ.2) Then
               Write(2,320) CPID(CP,1),CPID(CP,2)
320            Format(2A6)
            Elseif(KK.EQ.3) Then
               If(NUP(CP).EQ.0) Then
                  Write(2,330) CPID(CP,1)
330               Format(A6)
                  Goto 390
               Endif
               J=0
               K=0
340            J=J+1
               If(CPID(CP,1).EQ.CPID(J,2)) Then
                  K=K+1
                  CPUS(K)=CPID(J,1)
                  If(K.EQ.NUP(CP)) Goto 360
                  If(K.GT.NCP) Then
                     Write(20,350) CPID(CP,1),K
350                  Format(/,' ERROR: Number of control points ',
     +               'upstream of ',A6,' =',I4,' which exceeds total.')
                     Call ERROR
                  Endif
               Endif
               If(J.LT.NCP) Go to 340
360            Write(2,370) CPID(CP,1),(CPUS(L),L=1,NUP(CP))
370            Format(A6,20(2x,A6))
            Elseif(KK.EQ.4) Then
               Write(2,380) CPID(CP,1),INMETHOD(CP),CL(CP)
380            Format(A6,I2,F8.4)
            Endif
            If((CPID(CP,2).EQ.'   OUT'.or.CPID(CP,2).EQ.'      ').and.
     +          CPLIM.EQ.'      ') Write(2,385)
385         Format('***** The above control point is the basin outlet.')
390      End Do
!
!         Options 5 and 6 from 1CPT record field 2.
!
      Elseif(KK.EQ.5.or.KK.EQ.6) Then
         Do I=1,NCP
            DA(I)=0.0
         Enddo
400      Read(8,410,End=450) CD
410      Format(A2)
         If(CD.EQ.'FD'.or.CD.EQ.'FC'.or.CD.EQ.'**') Goto 400
         If(CD.EQ.'ED') Goto 450
         If(CD.EQ.'WP') Then
            Backspace(8)
            Read(8,420) CD,CPWP,DAWP
420         Format(A2,A6,F8.0)
            CPWP=Adjustr(CPWP)
            J=0
430         J=J+1
            If(CPID(J,1).EQ.CPWP) Then
               DA(J)=DAWP
            Elseif(J.GT.NCP) Then
               Write(20,440) CPWP
440            Format(/,' ERROR: CP identifier of ',A6,' from FD ',
     +                  ' record was not found on any CP record.')
               Call ERROR
            Else
               Goto 430
            Endif
            Go to 400
         Endif
450      Do 500 I=1,NCP
            CP=ICP(I)
            If(LIM.LT.0.and.INMETHOD(CP).GE.2) Goto 500
            If(NUP(CP).EQ.0.and.CPLIM.EQ.'      ') Write(2,290)
            If(KK.EQ.5) Then
               Write(2,460) CPID(CP,1),INMETHOD(CP),CL(CP),DA(CP)
460            Format(A6,I2,F8.4,F8.1)
            Else
               If(NUP(CP).GT.0) Then
                  J=0
                  K=0
470               J=J+1
                  If(CPID(CP,1).EQ.CPID(J,2)) Then
                     K=K+1
                     CPUS(K)=CPID(J,1)
                     If(K.EQ.NUP(CP)) Goto 470
                     If(K.GT.NCP) Then
                        Write(20,350) CPID(CP,1),K
                        Call ERROR
                     Endif
                  Endif
                  If(J.LT.NCP) Go to 470
480               Write(2,490) CPID(CP,1),INMETHOD(CP),CL(CP),DA(CP),
     +                      CPID(CP,2),NUP(CP),(CPUS(L),L=1,NUP(CP))
490               Format(A6,I2,F8.4,F8.1,2x,A6,I8,20(2x,A6))
               Else
                  Write(2,490) CPID(CP,1),INMETHOD(CP),CL(CP),DA(CP),
     +                      CPID(CP,2),NUP(CP)
               Endif
            Endif
            If((CPID(CP,2).EQ.'   OUT'.or.CPID(CP,2).EQ.'      ').and.
     +          CPLIM.EQ.'      ') Write(2,385)
500      End Do
      Endif
!
!  Return to main program from Subroutine CONPTS.
!
      Return
      End Subroutine CONPTS
!
!  ***********************************************************************
!
      Subroutine SUMTAB
!
!  Subroutine SUMTAB develops summary tables of monthly and/or annual data
!  for a control point, water right, water right group, reservoir, or the
!  entire river basin.  Monthly data are read from a WRAP-SIM output file.
!  Annual data are computed by summing monthly data.  Basin data are
!  computed by summing control point data.
!
!  The monthly data, variable MDATA(I), included in a summary table for a
!  control point (2SCP record), water right (2SWR), or reservoir (2SRE), 
!  are listed below. Data included in a basin summary (2SBA record) and
!  a group summary (2SGP) are the same as for a control point (2SCP).
!  Storage change and actual diversion are computed from read-in data.
!  Other data are read directly from the SIM output file (unit=4).
!
!                                      2SCP       2SWR       2SRE
!      Naturalized streamflow        MDATA(8)      --         --
!      Return flow                   MDATA(7)      --         --
!      Unappropriated flow           MDATA(6)      --         --
!      Regulated streamflows         MDATA(10)     --         --
!      Available streamflow             --      MDATA(6)      --
!      Streamflow depletion          MDATA(5)   MDATA(5)      --
!      EOP storage                   MDATA(4)   MDATA(4)   MDATA(4)
!      Storage change                    --     MDATA(10)* MDATA(10)*
!      Evaporation                   MDATA(3)   MDATA(3)   MDATA(3)
!      Permitted diversion           MDATA(2)   MDATA(2)      --
!      Actual diversion              MDATA(9)*  MDATA(9)*     --
!      Shortage                      MDATA(1)   MDATA(1)      --
!      Releases from system              --     MDATA(7)      --
!      Power produced                    --        --      MDATA(2)
!      Power shortage                    --        --      MDATA(1)
!      Depletions into reservoir         --        --      MDATA(5)
!      Releases into reservoir           --        --      MDATA(6)
!      Depletions and releases into res  --        --      MDATA(9)*
!      Releases through outlet works     --        --      MDATA(7)
!      Releases from reservoir pool      --        --      MDATA(8)
!      Total releases from reservoir     --        --      MDATA(11)*
!
!        Note: * Denotes data computed from the other read-in data.
!
!   Declaration of variables used in Subroutine SUMTAB
!
      Use COMVAR
!
      Real MDATA(12),YDATA(11),SDATA(10),SUM(9),MEAN(9)
!
      Integer REC1,COUNT,CREC,MONTH,YEAR,MNAN,NUM,LOOP,N,
     +        ID,K,I,SKIP,MATCH,PERIOD
!
      Character(len=2) IFC
      Character(len=4) CD
      Character(len=6) WRAPID,CHAR
      Character(len=8) WRID2,WRID3
      Character(len=16) CHAR1,WRID1
!
!   Format statements for summary table headings
!
10    Format('MONTHLY SUMMARY TABLE FOR THE RIVER BASIN')
20    Format('ANNUAL SUMMARY TABLE FOR THE RIVER BASIN')
30    Format(/,3X,'Note:  For naturalized streamflow and unappropriated'
     +,' flow, the quantities shown represent the maximum flow at any ',
     +  /,10X,'control point in a given month, based on comparing all ',
     +  'control points.  All other quantities shown are the ',/,10X,
     +  'sum of the values for all the control points.',/)
40    Format('MONTHLY SUMMARY TABLE FOR CONTROL POINT ',A6,/)
50    Format('ANNUAL SUMMARY TABLE FOR CONTROL POINT ',A6,/)
60    Format(13X,'NATURALIZED',7X,'RETURN   STREAMFLOW',3X,
     +       'UNAPPROPRIATED',4X,'EOP',20X,'TARGET',8X,'ACTUAL',
     +        5X,'DIVERSION')
65    Format(13X,'NATURALIZED    REGULATED UNAPPROPRIATED     RETURN  ',
     +      ' STREAMFLOW',8X,'EOP',8X,'NET',10X,'ACTUAL',5X,'DIVERSION')
70    Format('YEAR  MONTH   STREAMFLOW',8X,'FLOW',5X,'DEPLETION',
     +       8X,'FLOW',7X,'STORAGE',2X,'EVAPORATION',4X,'DIVERSION',
     +       4X,'DIVERSION',5X,'SHORTAGE')
75    Format('YEAR  MONTH   STREAMFLOW   STREAMFLOW   STREAMFLOW',8X,
     +       'FLOW',5X,'DEPLETION',6X,'STORAGE',2X,'EVAPORATION',4X,
     +       'DIVERSION',5X,'SHORTAGE')
80    Format('YEAR',10X,'STREAMFLOW',8X,'FLOW',5X,'DEPLETION',8X,
     +       'FLOW',7X,'STORAGE',2X,'EVAPORATION',4X,'DIVERSION',4X,
     +       'DIVERSION',5X,'SHORTAGE')
85    Format('YEAR',10X,'STREAMFLOW   STREAMFLOW   STREAMFLOW',8X,
     +       'FLOW',5X,'DEPLETION',6X,'STORAGE',2X,'EVAPORATION',4X,
     +       'DIVERSION',5X,'SHORTAGE')
90    Format(15X,'(',A5,')',7X,'(',A5,')',6X,'(',A5,')',7X,
     +           '(',A5,')',5X,'(',A5,')',7X,'(',A5,')',4X,
     +           '(',A5,')',7X,'(',A5,')',6X,'(',A5,')')
100   Format('MONTHLY SUMMARY TABLE FOR WATER RIGHT ',A16,2A8,/)
110   Format('ANNUAL SUMMARY TABLE FOR WATER RIGHT ',A16,2A8,/)
120   Format(17X,'AVAILABLE',4X,'STREAMFLOW',9X,'EOP',9X,'STORAGE',20X,
     +           'TARGET',9X,'ACTUAL')
130   Format('YEAR   MONTH',4X,'STREAMFLOW',4X,'DEPLETION',8X,
     +       'STORAGE',8X,'CHANGE',3X,'EVAPORATION',5X,'DIVERSION',
     +        5X,'DIVERSION',6X,'SHORTAGE')
140   Format('YEAR',12X,'STREAMFLOW',4X,'DEPLETION',8X,'STORAGE',8X,
     +       'CHANGE',3X,'EVAPORATION',5X,'DIVERSION',5X,'DIVERSION',
     +        6X,'SHORTAGE')
150   Format(17X,'(',A5,')       (',A5,')',9X,'(',A5,')',7X,'(',A5,')',
     +       5X,'(',A5,')',8X,'(',A5,')',7X,'(',A5,')',8X,'(',A5,')')
160   Format('MONTHLY SUMMARY TABLE FOR RESERVOIR ',A6,/)
170   Format('ANNUAL SUMMARY TABLE FOR RESERVOIR ',A6,/)
180   Format(21X,'EOP',9X,'STORAGE',19X,'RELS + DEPLS',6X,'RELS',8X,
     +           'POWER',8X,'POWER')
190   Format('YEAR   MONTH',7X,'STORAGE',8X,'CHANGE',3X,
     +       'EVAPORATION',9X,'INTO',10X,'FROM',6X,'PRODUCED',3X,
     +       'SHT(+)/2nd(-)')
200   Format('YEAR',15X,'STORAGE',8X,'CHANGE',3X,
     +       'EVAPORATION',9X,'INTO',10X,'FROM',6X,'PRODUCED',3X,
     +       'SHT(+)/2nd(-)')
210   Format(19X,'(',A5,')',7X,'(',A5,')',5X,'(',A5,')',9X,
     +       '(',A5,')',7X,'(',A5,')',6X,'(',A5,')',6X,'(',A5,')')
220   Format(128('-'))
230   Format(124('-'))
240   Format(111('-'))
250   Format(17X,'AVAILABLE',4X,'STREAMFLOW',9X,'EOP',21X,' SYSTEM',
     +        8X,'TARGET',9X,'ACTUAL')
260   Format('YEAR   MONTH',4X,'STREAMFLOW',4X,'DEPLETION',8X,
     +       'STORAGE',3X,'EVAPORATION',5X,'RELEASES',6X,'DIVERSION',
     +                 5X,'DIVERSION',6X,'SHORTAGE')
270   Format('YEAR',12X,'STREAMFLOW',4X,'DEPLETION',8X,
     +       'STORAGE',3X,'EVAPORATION',5X,'RELEASES',6X,'DIVERSION',
     +       5X,'DIVERSION',6X,'SHORTAGE')
280   Format('MONTHLY SUMMARY TABLE FOR WATER RIGHT GROUP ',A8,/)
290   Format('ANNUAL SUMMARY TABLE FOR WATER RIGHT GROUP ',A8,/)
300   Format(14X,'STREAMFLOW',6X,'SYSTEM',6X,'TARGET',9X,'ACTUAL')
310   Format('YEAR  MONTH',4X,'DEPLETION',5X,'RELEASES',4X,
     +       'DIVERSION',5X,'DIVERSION',4X,'SHORTAGE')
320   Format('YEAR',11X,'DEPLETION',5X,'RELEASES',4X,
     +       'DIVERSION',5X,'DIVERSION',4X,'SHORTAGE')
330   Format(16X,'(',A5,')       (',A5,')     (',A5,')',7X,
     +           '(',A5,')',6X,'(',A5,')')
340   Format(76('-'))
!
!   Read output table specifications from input file (unit=1).
!
      Read(1,350,IOSTAT=STATUS) CD, MNAN, NUM
350   Format(A4,I4,I4)
      If(STATUS.NE.0) Then
         Write(20,360) CD
360      Format(/,' ERROR: Fortran IOSTAT error occured reading a',
     +            ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(CD.EQ.'2SCP') Then
         ID = 0
         If(NCPTS.LE.0) Then
            Write(20,370)
370         Format(/,' ERROR: 2SCP record but zero control point ',
     +               'records in WRAP-SIM output.')
            Call ERROR
         Endif
      Elseif (CD.EQ.'2SWR') Then
         ID = 1
         If(NWROUT.LE.0) Then
            Write(20,380)
380         Format(/,' ERROR: 2SWR record but zero water right ',
     +               'records in WRAP-SIM output.')
            Call ERROR
         Endif
      Elseif (CD.EQ.'2SRE') Then
         ID = 2
         If(NREOUT.LE.0) Then
            Write(20,390)
390         Format(/,' ERROR: 2SRE record but zero reservoir ',
     +               'records in WRAP-SIM output.')
            Call ERROR
         Endif
      Elseif (CD.EQ.'2SGP') Then
         ID = 3
         If(NWROUT.LE.0) Then
            Write(20,400)
400         Format(/,' ERROR: 2SGP record but zero water right ',
     +               'records in WRAP-SIM output.')
            Call ERROR
         Endif
      Endif
      If(NUM.GT.0) Then
         Backspace(1)
         K = 1
         N = NUM
         If(N.GT.8) N=8
410      If(ID.EQ.0) Read(1,420) (IDCP(I),I=K,N)
         If(ID.EQ.2) Read(1,420) (IDRES(I),I=K,N)
         If(ID.EQ.3) Read(1,430) (IDEN8(I),I=K,N)
         If(ID.EQ.1) Read(1,440) (IDEN16(I),I=K,N)
420      Format(12x,8(2x,A6))
430      Format(12x,8A8)
440      Format(12x,8A16)
         If(NUM.GT.N) Then
            K = K + 8
            N = N + 8
            If(N.GT.NUM) N=NUM
            Goto 410
         Endif
         Do I=1,NUM
            IDCP(I)=Adjustr(IDCP(I))
            IDRES(I)=Adjustr(IDRES(I))
            IDEN8(I)=Adjustr(IDEN8(I))
            IDEN16(I)=Adjustr(IDEN16(I))
         End Do
      Endif
      NUM = Abs(NUM)
!
!   Since the program WRAP-SIM output file (unit=4) is read as a direct
!   access file, record counters are devised for use in locating the
!   records to be read.
!
450   COUNT = NUM
      CREC = NWROUT + NCPTS + NREOUT
      If(CD.EQ.'2SWR') Then
         REC1 = 6
         SKIP = NWROUT
         If(NUM.EQ.0) COUNT = NWROUT
      Elseif (CD.EQ.'2SCP') Then
         REC1 = 6 + NWROUT
         SKIP = NCPTS
         If(NUM.EQ.0) COUNT = NCPTS
      Elseif (CD.EQ.'2SRE') Then
         REC1 = 6 + NWROUT + NCPTS
         SKIP = NREOUT
         If(NUM.EQ.0) COUNT = NREOUT
      Elseif (CD.EQ.'2SBA') Then
         REC1 = 5 + NWROUT
         COUNT = NCPTS
      Elseif (CD.EQ.'2SGP') Then
         REC1 = 6
         SKIP = NWROUT
      Endif
!
!   ++++++++++  Begin Loop  ++++++++++
!   Beginning of loop to develop the tables for the COUNT control
!   points, water rights, or reservoirs (2SCP, 2SWR, or 2SRE RECORD).
!   The loop extends from statement 500 to statement 980.
!
      If(CD.EQ.'2SBA') Goto 990
      LOOP = 0
500   LOOP = LOOP + 1
      YEAR = YRST
      MONTH = 0
      Do I=1,11
         YDATA(I) = 0.0
      End Do
      Call TITLES
!
!   Find the first record of WRAP-SIM output data (unit=4) to be included
!   in the first data line of the table (unit=2) and backup by CREC.
!
      N=1
      RECD = REC1
      If(CD.EQ.'2SWR'.or.CD.EQ.'2SGP') Then
         If(OUTFORM.EQ.1) Then
            Read(4,REC=RECD) IO1
            If(IO1.EQ.-1) Then
               IFC='IF'
               Read(4,REC=RECD) IO1,IO2,(XO(I),I=1,7),C4
            Else
               Read(4,REC=RECD) IO1,IO2,(XO(I),I=1,7),C4,C2,C3
            Endif
         Else
            Read(4,510,REC=RECD) IFC,WRID1,WRID2,WRID3
510         Format(A2,81x,A16,2A8)
         Endif
         WRID1=Adjustr(WRID1)
         WRID2=Adjustr(WRID2)
         WRID2=Adjustr(WRID3)
         If(NUM.EQ.0) Goto 600
520      Match=0
         If(CD.EQ.'2SWR'.and.WRID1.NE.IDEN16(LOOP)) MATCH=1
         If(CD.EQ.'2SGP'.and.(WRID2.NE.IDEN8(LOOP).and.
     +                        WRID3.NE.IDEN8(LOOP))) MATCH=1
         If(MATCH.EQ.1) Then
            RECD = RECD + 1
            N = N + 1
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) IO1
               If(IO1.EQ.-1) Then
                  IFC='IF'
                  Read(4,REC=RECD) IO1,IO2,(XO(I),I=1,7),C4
               Else
                  Read(4,REC=RECD) IO1,IO2,(XO(I),I=1,7),C4,C2,C3
               Endif
            Else
               Read(4,510,REC=RECD) IFC,WRID1,WRID2,WRID3
            Endif
            If(N.LE.SKIP) Goto 520
         Endif
         If(IFC.EQ.'IF') Then
            WRID2 = '        '
            WRID3 = '        '
         Endif
         Goto 600
      Else
         If(OUTFORM.EQ.1) Then
            Read(4,REC=RECD) WRAPID
         Else
            Read(4,530,REC=RECD) WRAPID
530         Format(A6)
         Endif
         WRAPID=Adjustr(WRAPID)
         If(NUM.EQ.0) Goto 600
540      If((ID.EQ.0.and.WRAPID.NE.IDCP(LOOP)).or.
     +      (ID.EQ.2.and.WRAPID.NE.IDRES(LOOP))) Then
            RECD = RECD + 1
            N = N + 1
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) WRAPID
            Else
               Read(4,530,REC=RECD) WRAPID
            Endif
            If(N.LE.SKIP) Goto 540
         Endif
         Goto 600
      Endif
      Write(20,550)
550   Format(/,' ERROR: Program terminated from Subroutine SUMTAB',
     +       ' because:',/)
      If(ID.EQ.3) Then
         Write(20,560) IDEN8(LOOP), CD
560      Format(8X,'identifier ',A8,' from record ',A4,', was not')
      Elseif(ID.EQ.0) Then
         Write(20,570) IDCP(LOOP), CD
570      Format(8X,'identifier ',A6,' from record ',A4,', was not')
      Elseif(ID.EQ.2) Then
         Write(20,570) IDRES(LOOP), CD
      Elseif(ID.EQ.1) Then
         Write(20,580) IDEN16(LOOP), CD
580      Format(8X,'identifier ',A16,' from record ',A4,', was not')
      Endif
      Write(20,590)
590   Format(8X,'found in the WRAP-SIM output file.')
      Call ERROR
600   RECD = RECD - CREC
!
!   *-*-*-*-*-* 2SCP Record *-*-*-*-*-*
!   Monthly summary table for a control point, as specified by a 2SCP record
!
      If(CD.EQ.'2SCP'.and.MNAN.GE.1) Then
         Write(2,40) Adjustl(WRAPID)
         Write(2,220)
         Write(2,65)
         Write(2,75)
         Write(2,90) UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT
         Write(2,220)
         Do I = 1,9
             SUM(I) = 0.0
         End Do
         Do 640 N = 1, NYRS*NPRDS
            RECD = RECD + CREC
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) CHAR,(MDATA(I),I=1,8),MDATA(10)
            Else
               Read(4,610,REC=RECD) CHAR,(MDATA(I),I=1,8),MDATA(10)
610            Format(A6,9F11.0)
            Endif
            If(MDATA(1).LT.0.0) MDATA(1)=0.0
            CHAR=Adjustr(CHAR)
            If(CHAR.NE.WRAPID) Then
               Print*,' '
               Write(20,620) CD, CHAR, WRAPID
620            Format(/,' ERROR: In developing monthly summary table ',
     +                  'for record ',A4,/,8x,'identifier of ',A6,
     +                  ' in subsequent month in wrap output file',/,8x,
     +                  'does not match first month identifer of ',A6)
               Call ERROR
            Endif
            MDATA(9) = MDATA(2) - MDATA(1)
            MONTH = MONTH + 1
            If(MONTH.GT.NPRDS) Then
               MONTH = 1
               YEAR = YEAR + 1
            Endif
            Write(2,630)YEAR,MONTH,MDATA(8),MDATA(10),MDATA(6),MDATA(7),
     +                     MDATA(5),MDATA(4),MDATA(3),MDATA(9),MDATA(1)
630         Format(I4,I5,F15.1,8F13.1)
            SUM(1) = SUM(1) + MDATA(8)
            SUM(2) = SUM(2) + MDATA(10)
            SUM(3) = SUM(3) + MDATA(6)
            SUM(4) = SUM(4) + MDATA(7)
            SUM(5) = SUM(5) + MDATA(5)
            SUM(6) = SUM(6) + MDATA(4)
            SUM(7) = SUM(7) + MDATA(3)
            SUM(8) = SUM(8) + MDATA(9)
            SUM(9) = SUM(9) + MDATA(1)
640      End Do
         Do I=1,9
            MEAN(I) = SUM(I) / (NYRS*NPRDS)
         End Do
         Write(2,650) (MEAN(I),I=1,9)
650      Format('MEAN',5X,F15.1,8F13.1)
         Write(2,220)
      Endif
!
!   Annual summary table for a control point, as specified by a 2SCP record.
!
      If(CD.EQ.'2SCP'.and.MNAN.EQ.0) Then
         Write(2,50) Adjustl(WRAPID)
         Write(2,220)
         Write(2,65)
         Write(2,85)
         Write(2,90) UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT
         Write(2,220)
         Do I=1,9
            SUM(I) = 0.0
         End Do
         Do 680 N=1,NYRS*NPRDS
            MONTH = MONTH + 1
            RECD = RECD + CREC
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) CHAR, (MDATA(I),I=1,8),MDATA(10)
            Else
               Read(4,610,REC=RECD) CHAR, (MDATA(I),I=1,8),MDATA(10)
            Endif
            If(MDATA(1).LT.0.0) MDATA(1)=0.0
            CHAR=Adjustr(CHAR)
            If(CHAR.NE.WRAPID) Then
               Write(20,660) CD, CHAR, WRAPID
660            Format(/,' ERROR: In developing annual summary table ',
     +                  'for record ',A4,/,8x,'identifier of ',A6,
     +                  ' in subsequent month in wrap output file',/,8x,
     +                  'does not match first month identifer of ',A6)
               Call ERROR
            Endif
            Do I=1,8
               YDATA(I) = YDATA(I) + MDATA(I)
            End Do
            YDATA(10) = YDATA(10) + MDATA(10)
            If(MONTH.EQ.NPRDS) Then
               YDATA(4) = MDATA(4)
               YDATA(9) = YDATA(2) - YDATA(1)
               Write(2,670) YEAR,YDATA(8),YDATA(10),YDATA(6),YDATA(7),
     +                   YDATA(5),YDATA(4),YDATA(3),YDATA(9),YDATA(1)
670            Format(I4,F20.1,8F13.1)
               SUM(1) = SUM(1) + YDATA(8)
               SUM(2) = SUM(2) + YDATA(10)
               SUM(3) = SUM(3) + YDATA(6)
               SUM(4) = SUM(4) + YDATA(7)
               SUM(5) = SUM(5) + YDATA(5)
               SUM(6) = SUM(6) + YDATA(4)
               SUM(7) = SUM(7) + YDATA(3)
               SUM(8) = SUM(8) + YDATA(9)
               SUM(9) = SUM(9) + YDATA(1)
               MONTH = 0
               YEAR = YEAR + 1
               Do I=1,8
                  YDATA(I) = 0.0
               End Do
               YDATA(10) = 0.0
            Endif
680      End Do
         Do I=1,9
            MEAN(I) = SUM(I) / NYRS
         End Do
         Write(2,690) (MEAN(I),I=1,9)
690      Format('MEAN',5X,F15.1,8F13.1)
         Write(2,220)
      Endif
!
!   *-*-*-*-*-* 2SWR Record *-*-*-*-*-*
!   Monthly summary table for a water right, as specified by a 2SWR record.
!
      If(CD.EQ.'2SWR'.and.MNAN.GE.1) Then
         Write(2,100) Adjustl(WRID1),Adjustl(WRID2),Adjustl(WRID3)
         Write(2,230)
         Write(2,250)
         Write(2,260)
         Write(2,150) UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT
         Write(2,230)
         Do I = 1,9
            SUM(I) = 0.0
         End Do
         Do 720 N = 1, NYRS*NPRDS
            RECD = RECD + CREC
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) IO1,IO2,(MDATA(I),I=1,7),CHAR1
            Else
               Read(4,700,REC=RECD) (MDATA(I),I=1,7),CHAR1
700            Format(6x,7F11.0,A16)
            Endif
            If(MDATA(1).LT.0.0) MDATA(1)=0.0
            CHAR1=Adjustr(CHAR1)
            If(CHAR1.NE.WRID1) Then
               Write(20,620) CD, CHAR1, WRID1
               Call ERROR
            Endif
            MDATA(9) = MDATA(2) - MDATA(1)
            MONTH = MONTH + 1
            If(MONTH.GT.NPRDS) Then
               MONTH = 1
               YEAR = YEAR + 1
            Endif
            Write(2,710) YEAR,MONTH,MDATA(6),MDATA(5),MDATA(4),
     *                   MDATA(3),MDATA(7),MDATA(2),MDATA(9),MDATA(1)
710         Format(I4,I6,2X,8F14.1)
            SUM(1) = SUM(1) + MDATA(6)
            SUM(2) = SUM(2) + MDATA(5)
            SUM(3) = SUM(3) + MDATA(4)
            SUM(4) = SUM(4) + MDATA(3)
            SUM(5) = SUM(5) + MDATA(7)
            SUM(6) = SUM(6) + MDATA(2)
            SUM(7) = SUM(7) + MDATA(9)
            SUM(8) = SUM(8) + MDATA(1)
720      End Do
         Do I=1,8
            MEAN(I) = SUM(I) / (NYRS*NPRDS)
         End Do
         Write(2,730) (MEAN(I),I=1,8)
730      Format('MEAN',8X,8F14.1)
         Write(2,230)
      Endif
!
!   Annual summary table for a water right, as specified by a 2SWR record.
!
      If(CD.EQ.'2SWR'.and.MNAN.EQ.0) Then
         Write(2,110) Adjustl(WRID1),Adjustl(WRID2),Adjustl(WRID3)
         Write(2,230)
         Write(2,250)
         Write(2,270)
         Write(2,150) UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT
         Write(2,230)
         Do I = 1,9
            SUM(I) = 0.0
         End Do
         Do 750 N=1,NYRS*NPRDS
            MONTH = MONTH + 1
            RECD = RECD + CREC
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) IO1,IO2,(MDATA(I),I=1,7),CHAR1
            Else
               Read(4,700,REC=RECD) (MDATA(I),I=1,7),CHAR1
            Endif
            If(MDATA(1).LT.0.0) MDATA(1)=0.0
            If(CHAR1.NE.WRID1) Then
               Write(20,620) CD, CHAR1, WRID1
               Call ERROR
            Endif
            Do I=1,7
               YDATA(I) = YDATA(I) + MDATA(I)
            End Do
            If(MONTH.EQ.NPRDS) Then
               YDATA(4) = MDATA(4)
               YDATA(9) = YDATA(2) - YDATA(1)
               Write(2,740) YEAR,YDATA(6),YDATA(5),YDATA(4),YDATA(3),
     +                      YDATA(7),YDATA(2),YDATA(9),YDATA(1)
740            Format(I4,2X,F20.1,7F14.1)
               SUM(1) = SUM(1) + YDATA(6)
               SUM(2) = SUM(2) + YDATA(5)
               SUM(3) = SUM(3) + YDATA(4)
               SUM(4) = SUM(4) + YDATA(3)
               SUM(5) = SUM(5) + YDATA(7)
               SUM(6) = SUM(6) + YDATA(2)
               SUM(7) = SUM(7) + YDATA(9)
               SUM(8) = SUM(8) + YDATA(1)
               MONTH = 0
               YEAR = YEAR + 1
               Do I=1,7
                  YDATA(I) = 0.0
               End Do
            Endif
750      End Do
         Do I=1,8
            MEAN(I) = SUM(I) / NYRS
         End Do
         Write(2,760) (MEAN(I),I=1,8)
760      Format('MEAN',8X,8F14.1)
         Write(2,230)
      Endif
!
!   *-*-*-*-*-* 2SRE Record *-*-*-*-*-*
!   Monthly summary table for a reservoir, as specified by a 2SRE record.
!
      If(CD.EQ.'2SRE'.and.MNAN.GE.1) Then
         Write(2,160) Adjustl(WRAPID)
         Write(2,240)
         Write(2,180)
         Write(2,190)
         Write(2,210) UNIT,UNIT,UNIT,UNIT,UNIT,UNHP,UNHP
         Write(2,240)
         Do I=1,9
            SUM(I) = 0.0
         End Do
         Do 770 N=1,NYRS*NPRDS
            RECD = RECD + CREC
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) CHAR, (MDATA(I),I=1,8)
            Else
               Read(4,610,REC=RECD) CHAR, (MDATA(I),I=1,8)
            Endif
            CHAR=Adjustr(CHAR)
            If(CHAR.NE.WRAPID) Then
               Write(20,620) CD, CHAR, WRAPID
               Call ERROR
            Endif
            MDATA(9) = MDATA(5) + MDATA(6)
            MDATA(11) = MDATA(7) + MDATA(8)
            MDATA(10) = MDATA(9) - MDATA(11) - MDATA(3)
            MONTH = MONTH + 1
            If(MONTH.GT.NPRDS) Then
               MONTH = 1
               YEAR = YEAR + 1
            Endif
            Write(2,710) YEAR,MONTH,MDATA(4),MDATA(10),MDATA(3),
     +                   MDATA(9),MDATA(11),MDATA(2),MDATA(1)
            SUM(1) = SUM(1) + MDATA(4)
            SUM(2) = SUM(2) + MDATA(10)
            SUM(3) = SUM(3) + MDATA(3)
            SUM(4) = SUM(4) + MDATA(9)
            SUM(5) = SUM(5) + MDATA(11)
            SUM(6) = SUM(6) + MDATA(2)
            SUM(7) = SUM(7) + MDATA(1)
770      End Do
         Do I=1,8
            MEAN(I) = SUM(I) / (NYRS*NPRDS)
         End Do
         Write(2,780) (MEAN(I),I=1,7)
780      Format('MEAN',8X,7F14.1)
         Write(2,240)
      Endif
!
!   Annual summary table for a reservoir, as specified by a 2SRE record.
!
      If(CD.EQ.'2SRE'.and.MNAN.EQ.0) Then
         Write(2,170) Adjustl(WRAPID)
         Write(2,240)
         Write(2,180)
         Write(2,200)
         Write(2,210) UNIT,UNIT,UNIT,UNIT,UNIT,UNHP,UNHP
         Write(2,240)
         Do I=1,9
            SUM(I) = 0.0
         End Do
         Do 790 N=1,NYRS*NPRDS
            MONTH = MONTH + 1
            RECD = RECD + CREC
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) CHAR, (MDATA(I),I=1,8)
            Else
               Read(4,610,REC=RECD) CHAR, (MDATA(I),I=1,8)
            Endif
            CHAR=Adjustr(CHAR)
            If(CHAR.NE.WRAPID) Then
               Write(20,620) CD, CHAR, WRAPID
               Call ERROR
            Endif
            Do I=1,8
               YDATA(I) = YDATA(I) + MDATA(I)
            End Do
            YDATA(9) =  YDATA(5) + YDATA(6)
            YDATA(11) = YDATA(7) + YDATA(8)
            If(MONTH.EQ.NPRDS) Then
               YDATA(4) = MDATA(4)
               YDATA(10) = YDATA(9) - YDATA(3) - YDATA(11)
               Write(2,740) YEAR,YDATA(4),YDATA(10),YDATA(3),YDATA(9),
     +                      YDATA(11),YDATA(2),YDATA(1)
               SUM(1) = SUM(1) + YDATA(4)
               SUM(2) = SUM(2) + YDATA(10)
               SUM(3) = SUM(3) + YDATA(3)
               SUM(4) = SUM(4) + YDATA(9)
               SUM(5) = SUM(5) + YDATA(11)
               SUM(6) = SUM(6) + YDATA(2)
               SUM(7) = SUM(7) + YDATA(1)
               MONTH = 0
               YEAR = YEAR + 1
               Do I=1,11
                  YDATA(I) = 0.0
               End Do
            Endif
790      End Do
         Do I=1,7
            MEAN(I) = SUM(I) / NYRS
         End Do
         Write(2,800) (MEAN(I),I=1,7)
800      Format('MEAN',8X,7F14.1)
         Write(2,240)
      Endif
!
!   *-*-*-*-*-* 2SGP Record *-*-*-*-*-*
!   Monthly (MNAN=1) or annual (MNAN=0) summary table for group of
!   water rights as specified by a 2SGP record.
!
      If(CD.EQ.'2SGP') Then
         If(MNAN.GE.1) Then
            Write(2,280) Adjustl(IDEN8(LOOP))
            Write(2,340)
            Write(2,300)
            Write(2,310)
            Write(2,330) UNIT,UNIT,UNIT,UNIT,UNIT
            Write(2,340)
         Endif
         If(MNAN.EQ.0) Then
            Write(2,290) Adjustl(IDEN8(LOOP))
            Write(2,340)
            Write(2,300)
            Write(2,320)
            Write(2,330) UNIT,UNIT,UNIT,UNIT,UNIT
            Write(2,340)
         Endif
!
!   Initialize variables.
!
         PERIOD = 0
         MONTH = 0
         YEAR = YRST
         RECD = REC1
         N = NYRS*NPRDS
         Do I=1,7
            YDATA(I) = 0
         End Do
         Do I=1,9
            SUM(I) = 0.0
         End Do
!
!   Begin loop which is repeated for each of N=NYRS*NPRDS periods (months).
!
900      PERIOD = PERIOD + 1
         RECD = REC1+PERIOD*CREC-CREC
         MONTH = MONTH + 1
         Do I=1,9
            SDATA(I) = 0
         End Do
         Do 940 K=1,NWROUT
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) IO1,IO2,(XO(I),I=1,7),C4,WRID2,WRID3
            Else
               Read(4,910,REC=RECD) WRID2,WRID3
910            Format(99x,2A8)
            Endif
            WRID2=Adjustr(WRID2)
            WRID3=Adjustr(WRID3)
            If(WRID2.NE.IDEN8(LOOP).and.WRID3.NE.IDEN8(LOOP)) Goto 930
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) IO1,IO2,MDATA(1),MDATA(2),XO(3),XO(4),
     +                          MDATA(5),XO(6),MDATA(7)
            Else
               Read(4,920,REC=RECD) MDATA(1),MDATA(2),MDATA(5),MDATA(7)
920            Format(6x,2F11.0,22x,F11.0,11x,F11.0)
            Endif
            If(MDATA(1).LT.0.0) MDATA(1)=0.0
            SDATA(1) = SDATA(1) + MDATA(1)
            SDATA(2) = SDATA(2) + MDATA(2)
            SDATA(5) = SDATA(5) + MDATA(5)
            SDATA(7) = SDATA(7) + MDATA(7)
930         RECD = RECD + 1
940      End Do
         YDATA(1) = YDATA(1) + SDATA(1)
         YDATA(2) = YDATA(2) + SDATA(2)
         YDATA(5) = YDATA(5) + SDATA(5)
         YDATA(7) = YDATA(7) + SDATA(7)
         If(MNAN.GE.1) Then
            SDATA(9) = SDATA(2) - SDATA(1)
            Write(2,950) YEAR,MONTH,SDATA(5),SDATA(7),SDATA(2),
     +                   SDATA(9),SDATA(1)
950         Format(I4,I5,F15.1,4F13.1)
         Endif
         If(MNAN.EQ.0.and.MONTH.EQ.NPRDS) Then
            YDATA(9) = YDATA(2) - YDATA(1)
            Write(2,960) YEAR,YDATA(5),YDATA(7),YDATA(2),
     +                   YDATA(9),YDATA(1)
960         Format(I4,F20.1,4F13.1)
            SUM(1) = SUM(1) + YDATA(5)
            SUM(2) = SUM(2) + YDATA(7)
            SUM(3) = SUM(3) + YDATA(2)
            SUM(4) = SUM(4) + YDATA(9)
            SUM(5) = SUM(5) + YDATA(1)
            Do I=1,9
               YDATA(I) = 0
            End Do
         Endif
         If(MONTH.EQ.NPRDS) Then
            MONTH = 0
            YEAR = YEAR + 1
         Endif
         If(PERIOD.LT.N) Goto 900
         If(MNAN.EQ.0) Then
            Do I=1,5
               MEAN(I) = SUM(I) / NYRS
            End Do
            Write(2,970) (MEAN(I),I=1,5)
970         Format('MEAN',7X,5F13.1)
            Write(2,340)
         Endif
      Endif
!
!   ++++++++++  End Loop  ++++++++++
!   End of loop to develop tables for the "COUNT" control points,
!   water rights, water rights groups, or reservoirs (2SCP, 2SWR,
!   2SGP, or 2SRE Record).
!
      If(NUM.EQ.0.and.CD.NE.'2SGP') REC1 = REC1 + 1
!
980   If(LOOP.LT.COUNT) Goto 500
!
!   *-*-*-*-*-* 2SBA Record *-*-*-*-*-*
!   Monthly (MNAN=1) or annual (MNAN=0) summary table for entire
!   basin, as specified by a 2SBA record.
!
990   If(CD.EQ.'2SBA') Then
         Call TITLES
         If(MNAN.GE.1) Then
            Write(2,10)
            Write(2,30)
            Write(2,220)
            Write(2,60)
            Write(2,70)
            Write(2,90) UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT
            Write(2,220)
         Endif
         If(MNAN.EQ.0) Then
            Write(2,20)
            Write(2,30)
            Write(2,220)
            Write(2,60)
            Write(2,80)
            Write(2,90) UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT,UNIT
            Write(2,220)
         Endif
!
!   Initialize variables.
!
         PERIOD = 0
         MONTH = 0
         YEAR = YRST
         RECD = REC1
         N = NYRS*NPRDS
         Do I=1,9
            YDATA(I) = 0
         End Do
         Do I=1,9
            SUM(I) = 0.
         End Do
!
!   Begin loop which is repeated for each of N=NYRS*NPRDS periods (months).
!
1000     PERIOD = PERIOD + 1
         MONTH = MONTH + 1
         Do I=1,9
            SDATA(I) = 0
         End Do
         Do K=1,NCPTS
            RECD = RECD + 1
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) C1,(MDATA(I),I=1,8)
            Else
               Read(4,1010,REC=RECD) (MDATA(I),I=1,8)
            Endif
1010        Format(6x,8F11.0)
            If(MDATA(1).LT.0.0) MDATA(1)=0.0
            Do I=1,5
               SDATA(I) = SDATA(I) + MDATA(I)
            End Do
            SDATA(7) = SDATA(7) + MDATA(7)
            SDATA(6) = Max(MDATA(6),SDATA(6))
            SDATA(8) = Max(MDATA(8),SDATA(8))
         End Do
         Do I=1,8
            YDATA(I) = YDATA(I) + SDATA(I)
         End Do
         If(MNAN.GE.1) Then
            SDATA(9) = SDATA(2) - SDATA(1)
            Write(2,630)YEAR,MONTH,SDATA(8),SDATA(7),SDATA(5),SDATA(6),
     *                  (SDATA(I),I=4,2,-1),SDATA(9),SDATA(1)
         Endif
         If(MNAN.EQ.0.and.MONTH.EQ.NPRDS) Then
            YDATA(4) = SDATA(4)
            YDATA(9) = YDATA(2) - YDATA(1)
            Write(2,670) YEAR,YDATA(8),YDATA(7),YDATA(5),YDATA(6),
     *                   (YDATA(I),I=4,2,-1),YDATA(9),YDATA(1)
            SUM(1) = SUM(1) + YDATA(8)
            SUM(2) = SUM(2) + YDATA(7)
            SUM(3) = SUM(3) + YDATA(5)
            SUM(4) = SUM(4) + YDATA(6)
            SUM(5) = SUM(5) + YDATA(4)
            SUM(6) = SUM(6) + YDATA(3)
            SUM(7) = SUM(7) + YDATA(2)
            SUM(8) = SUM(8) + YDATA(9)
            SUM(9) = SUM(9) + YDATA(1)
            Do I=1,9
               YDATA(I) = 0
            End Do
         Endif
         If(MONTH.EQ.NPRDS) Then
            MONTH = 0
            YEAR = YEAR + 1
         Endif
         RECD = RECD + CREC - NCPTS
         If(PERIOD.LT.N) Goto 1000
         If(MNAN.EQ.0) Then
            Do I = 1,9
               MEAN(I) = SUM(I) / NYRS
            End Do
            Write(2,1020) (MEAN(I),I=1,9)
1020        Format('MEAN',7X,9F13.1)
            Write(2,220)
         Endif
      Endif
!
!   *-*-*-*-*-* 2SCP, 2SWR, 2SRE, 2SGP, or 2SBA Record *-*-*-*-*-*
!   If NMAN=2, develop an annual summary table by repeating above loop
!   prior to reading next record from input file (unit=1).
!
      If(MNAN.EQ.2) Then
         MNAN = 0
         Goto 450
      Endif
!
!  Return to main program from Subroutine SUMTAB.
!
      Return
      End Subroutine SUMTAB
!
!  ***********************************************************************
!
      Subroutine BUDGET
!
!  *-*-*-*-*-*   2BUD Record   *-*-*-*-*-*
!  Subroutine BUDGET develops a water budget for an overall river basin
!  and/or for an individual control point.
!
      Use COMVAR
!
      Integer BASIN,BEGSTO,CIRECORD,CP,CPFLAG,HEADING,ICPI,I,J,K,M,N,
     +        MT,NCP,NEGINF,CPOUT,YEAR,YR,YREND,YRFIRST,YRLAST
      Integer NUP(NCPTS),CPUS(NCPTS,40)
!
      Real BPSTX,CIX,DEP,DIV,DSS,DTS,EP,FCL,FCLC,FOTH,FIN,FINNEG,FIN2,
     +     FNAT,FREG,ST,XSTO,XSUM,TOTFLOW(18)
      Real BSTO(NCPTS),CI(12),CINF(NCPTS,12),CL(NCPTS),CLC(NCPTS),
     +     DS(NCPTS),DT(NCPTS),EVAP(NCPTS),FOUT(NCPTS),NAT(NCPTS),
     +     REG(NCPTS),RET(NCPTS),STO(NCPTS)
!
      Character(len=2)  ID
      Character(len=4)  CD
      Character(len=6)  BRSID,CIID,CPBUD,CPID(NCPTS,2)
!
      HEADING=0
      TOTFLOW=0.0
      CINF=0.0
      BSTO=0.0
      CIX=1.0
!
!   SIM (UO=4) or SIMD (UO=10) output file unit number.
!
      If(SIMD.EQ.1) Then
         UO=10
      Else
         UO=4
      Endif
!
!  Reading Input Data
!
      Write(20,60)
60    Format('*** Starting to read input data for 2BUD record routine.')
!
!  2BUD record is read.
!
      Read(1,70) CD
70    Format(A4)
      If(CD.EQ.'2BUD') Then
         Backspace(1)
         Read(1,80,IOSTAT=STATUS) CD,CPBUD,YRFIRST,YRLAST,ICPI,BEGSTO,
     +                            NEGINF,CPOUT,BASIN
80       Format(A4,2x,A6,7I4)
         If(STATUS.NE.0) Then
            Write(20,*)' ERROR: Fortran IOSTAT error occurred reading',
     +                 ' 2BUD record.'
            Call ERROR
         Endif
         If(YRFIRST.EQ.0) YRFIRST=YRST
         If(YRLAST.EQ.0)  YRLAST=YRST+NYRS-1
         If(YRFIRST.LT.1) Then
            Write(20,*)' ERROR: Number of years in 2BUD record field 2',
     +                 ' must be at least one.'
            Call ERROR
         Endif
         If(YRLAST.GT.(YRST+NYRS-1)) Then
            Write(20,*)' ERROR: Last year from 2BUD record can not ',
     +                 ' the exceed year of SIM simulation.'
            Call ERROR
         Endif
         If(YRFIRST.LT.YRST) Then
            Write(20,*)' ERROR: First year from 2BUD record can not',
     +                 ' preceed the starting year of SIM simulation.'
            Call ERROR
         Endif
         If(BEGSTO.LT.0.or.BEGSTO.GT.3) Then
            Write(20,90) BEGSTO
90          Format(' ERROR: 2BUD record BEGSTO of',I3,' is not valid.')
            Call ERROR
         Endif
         If(NEGINF.LT.0.or.NEGINF.GT.3) Then
            Write(20,100) NEGINF
100         Format(' ERROR: 2BUD record NEGINF of',I3,' is not valid.')
            Call ERROR
         Endif
         If(ICPI.GE.1.and.ICP(1).EQ.0) Then
            Write(20,110) ICPI
110         Format(' ERROR: Ordering array is missing. ICPI of',I2,
     +             ' indicates cp sequencing by 1CPT record.')
            Call ERROR
         Endif
         Goto 140
      Else
         Write(20,120) CD
120      Format(' ERROR: Read CD of ',A4,' instead of 2BUD.')
         Call ERROR
      Endif
!
! ++++++++++++++++++++  WRAP-SIM Input File  ++++++++++++++++++++
!
!  CP records are read from SIM DAT file to determine spatial connectivity.
!
140   Write(20,150)
150   Format('*** Starting to read CP records from SIM input file.')
      Rewind(3)
      NCP=0
160   Format(A2)
170   Read(3,160,END=210) ID
      If(ID.EQ.'T1'.or.ID.EQ.'T2'.or.ID.EQ.'T3'.or.ID.EQ.'**') Goto 170
      If(ID.EQ.'FO'.or.ID.EQ.'JD'.or.ID.EQ.'JO'.or.ID.EQ.'FY') Goto 170
      If(ID.EQ.'CO'.or.ID.EQ.'RO'.or.ID.EQ.'WO'.or.ID.EQ.'GO') Goto 170
      If(ID.EQ.'CR'.or.ID.EQ.'UC'.or.ID.EQ.'UP'.or.ID.EQ.'RF') Goto 170
      If(ID.EQ.'WR'.or.ID.EQ.'IF'.or.ID.EQ.'CI'.or.ID.EQ.'ED') Goto 210
      If(ID.EQ.'OF'.or.ID.EQ.'ZZ') Goto 210
      NCP=NCP+1
      Backspace(3)
      Read(3,180,IOSTAT=STATUS) ID,CPID(NCP,1),CPID(NCP,2)
180   Format(A2,A6,2x,A6)
      CPID(NCP,1)=Adjustr(CPID(NCP,1))
      CPID(NCP,2)=Adjustr(CPID(NCP,2))
      If(CPID(NCP,2).EQ.'      ') CPID(NCP,2)='   OUT'
      If(STATUS.NE.0) Then
         Write(20,190)
190      Format(' ERROR: Fortran IOSTAT error occurred reading CP',
     +          ' record from WRAP-SIM input file')
         Call ERROR
      Endif
      If(ID.NE.'CP') Then
         Write(20,200) ID
200      Format(' ERROR: Read a ID of ',A2,' from SIM input file ',
     +                   'instead of CP')
         Call ERROR
      Endif
      Go to 170
210   If(NCP.NE.NCPTS) Then
         Write(20,220) NCP,NCPTS
220      Format(' ERROR: Number of control points in SIM input and',
     +         ' output files are',I5,' and',I5,'. Should be the same.')
         Call ERROR
      Endif
      Write(20,230) NCP
230   Format('*** Read',I5,' CP records from SIM input file.')
!
!  CP sequence is checked. If ICPI=0, sequence array ICP created by 1CPT
!  record sets sequencing. Otherwise, control points must be entered in
!  SIM input file in upstream-to-downstream order. 
!
      Do I=1,NCPTS
         If(ICPI.LE.0) Then
            CP=I
         Else
            CP=ICP(I)
         Endif
         If(CPID(CP,2).NE.'   OUT') Then
            K=0
240         K=K+1
            If(K.LE.NCPTS) Then
               If(ICPI.LE.0) Then
                  J=K
               Else
                  J=ICP(K)
               Endif
               If(CPID(CP,2).EQ.CPID(J,1)) Then
                  If(I.GT.K) Then
                     Write(20,250) CPID(J,1),CPID(CP,1)
250                  Format(' WARNING: Control points are out of',
     +                      ' sequence.',/,10x,'CP ',A6,
     +                      ' should not be after CP ',A6)
                  Endif
               Else
                  Goto 240
               Endif
            Endif
         Endif
      Enddo
      Write(20,260)
260   Format('*** Completed check of sequencing of control points.')
!
!  The control points located immediately upstream of each control point
!  are identified. NUP(I) are the number of upstream control points.
!  CPUS(I,J) are identifiers of the control points J upstream of CP.
!
      NUP=0
      CPUS=0
      Do CP=1,NCPTS
         K=0
         Do J=1,NCPTS
            If(CPID(CP,1).EQ.CPID(J,2)) Then
               NUP(CP)=NUP(CP)+1
               K=K+1
               CPUS(CP,K)=J
            Endif
         Enddo
      Enddo
!
!  The CI multiplier factor CIX is read from XL record field 5.
!
      If(ID.EQ.'XL') Then
         Backspace(3)
         Read(3,270,IOSTAT=STATUS) CIX
270      Format(24x,F8.0)
         If(STATUS.NE.0) Then
            Write(20,280)
280         Format(' ERROR: Fortran IOSTAT error occurred ',
     +             ' reading XL record from WRAP-SIM input file')
            Call ERROR
         Endif
         If(CIX.LE.0.00001) CIX=1.0
         Goto 170
      Endif
!
!  Constant monthly inflow/outflow CI records for control points
!  are read from SIM DAT file.
!
      If(ID.EQ.'CI') Then
         Write(20,290)
290      Format('*** Starting to read CI records from SIM input file.')
         Backspace(3)
         CIRECORD=0
300      Read(3,160) ID
         If(ID.EQ.'**') Goto 300
         If(ID.EQ.'CI') Then
            CIRECORD=CIRECORD+1
            Read(3,310) ID,CIID
310         Format(A2,A6)
            Backspace(3)
            Backspace(3)
            If(ID.EQ.'CI'.and.CIID.EQ.'      ') Then
               Read(3,320,IOSTAT=STATUS) ID,CIID,(CI(MT),MT=1,6)
320            Format(A2,A6,6F8.0)
               If(STATUS.NE.0) Then
                  Write(20,330)
330               Format(' ERROR: Fortran IOSTAT error occurred ',
     +                   ' reading CI record from WRAP-SIM input file')
                  Call ERROR
               Endif
               Read(3,340,IOSTAT=STATUS) ID,(CI(MT),MT=7,12)
340            Format(A2,6x,6F8.0)
               If(STATUS.NE.0) Then
                  Write(20,330)
                  Call ERROR
               Endif
            Else
               Read(3,350,IOSTAT=STATUS) ID,CIID,(CI(MT),MT=1,12)
350            Format(A2,A6,12F8.0)
               If(STATUS.NE.0) Then
                  Write(20,330)
                  Call ERROR
               Endif
            Endif
            CIID=Adjustr(CIID)
            Do K=1,NCPTS
               If(CIID.EQ.CPID(K,1)) Then
                  Do MT=1,12
                     CINF(K,MT)=CINF(K,MT)+(CIX*CI(MT))
                  End Do
                  Goto 300
               Endif
               If(K.EQ.NCPTS) Then
                  Write(20,360) CIID
360               Format(' ERROR: Control point identifier ',A6,' from',
     +                ' CI record matches no identifier on CP records.')
                  Call ERROR
               Endif
            End Do
         Endif
      Endif
      If(CIRECORD.GT.0) Then
         Write(20,370) CIRECORD
370      Format('*** Read',I4,' CI records from SIM input file.')
      Endif
!
!  Finished reading SIM input file.
!
      Write(20,380)
380   Format('*** Finished reading SIM input file.')
!
!  Beginning-of-simulation reservoir storage is either:
!  preceding end-of-month storage if YRFIRST is later than YRST (BEGSTO=0)
!  read from SIM BRS file (BEGSTO=1)
!  approximately computed from data from the SIM OUT file (BEGSTO=2)
!
      If(BEGSTO.LE.1.or.BEGSTO.EQ.3) Then
         Write(20,400)
400   Format('*** Beginning-of-simulation storage is to be determined.')
      Endif
!
!  Beginning-of-simulation storage is approximated from SIM OUT file data.
!
      If(BEGSTO.EQ.2) Then
         RECD=NWROUT+4
         Do CP=1,NCPTS
            RECD=RECD+1
            If(OUTFORM.EQ.1) Then
               Read(4,REC=RECD) C1,DSS,DTS,EP,ST,DEP
            Else
               Read(4,410,REC=RECD) DSS,DTS,EP,ST,DEP
410            Format(6x,5F11.0)
            Endif
            If(STATUS.NE.0) Then
               Write(20,*)' ERROR: Fortran IOSTAT error reading SIM',
     +               ' output file for beginning storage computations.'
               Call ERROR
            Endif
            BSTO(CP)=ST-DEP+EP+DTS-DSS
         Enddo
         Write(20,*) '** Beginning storage was computed from data ',
     +              'read from SIM output file.'
      Endif
!
!  Beginning-of-simulation storage is read from a BRS file.
!
      If(BEGSTO.EQ.1) Then
         Rewind(9)
         Do I=1,5
            Read(9,420) ID
420         Format(A2)
         Enddo
440      Read(9,450,IOSTAT=STATUS,END=480) BRSID,BPSTX
450      Format(18x,A6,10x,F10.0)
         CP=0
460      CP=CP+1
         If(BRSID.EQ.CPID(CP,1)) Then
            BSTO(CP)=BSTO(CP)+BPSTX
         Elseif(CP.LT.NCPTS) Then
            Go to 460
         Else
            Write(20,470) BRSID
470         Format(' ERROR: CP ',A6,' on BRS file is not in',
     +             ' SIM input file.')
            Call ERROR
         Endif
         Goto 440
480      Write(20,490)
490      Format('*** Beginning storage was read from BRS file.')
      Endif
!
! +++++++++++++++++++++  Beginning of Time Step Loop  +++++++++++++++++++++
!
!  Data are read and computations are performed for each month of each year.
!
      Write(20,500)
500   Format('*** Starting to read data from SIM OUT file and ',
     +       'perform computations.')
!
      RECD=5
      YREND=YRLAST-YRST+1
!
      Do 650 YR=1,YREND
         YEAR=YRST+YR-1
         Do 640 M=1,12
!
!  Any water rights output records in SIM output file are skipped over.
!
            RECD=RECD+NWROUT
!
!  Beginning-of-period reservoir storage volumes are set.
!  Pertinent volumes for all control points are read from SIM OUT file.
!
            Do CP=1,NCPTS
               If(YEAR.GT.YRFIRST.or.M.GT.1)   BSTO(CP)=STO(CP)
               If(YEAR.EQ.YRFIRST.and.YR.GT.1) BSTO(CP)=STO(CP)
               RECD=RECD+1
               If(OUTFORM.EQ.1) Then
                  Read(4,REC=RECD,IOSTAT=STATUS) C1,DS(CP),DT(CP),
     +                       EVAP(CP),STO(CP),XO(5),XO(6),RET(CP),
     +                       NAT(CP),REG(CP),CLC(CP),CL(CP)
               Else
                  Read(4,510,REC=RECD,IOSTAT=STATUS) DS(CP),DT(CP),
     +                            EVAP(CP),STO(CP),RET(CP),NAT(CP),
     +                            REG(CP),CLC(CP),CL(CP)
510               Format(6x,4F11.0,22x,4F11.0,F9.0)
               Endif
               If(STATUS.NE.0) Then
                  Write(20,*)' ERROR: Fortran IOSTAT error occurred',
     +                      ' reading SIM output file.'
                  Call ERROR
               Endif
            End Do
!
!  Any reservoir output records in SIM output file are skipped over.
!
            RECD=RECD+NREOUT
!
!  The starting year YRSTBUD from 2BUD record may be later than the
!  starting year YRST in the SIM output file. Computations are not
!  performed for years before YRST.
!
            If(YEAR.LT.YRFIRST) Goto 650
!
!  +++++++++++++  Beginning of Control Point Computation Loop  +++++++++++++
!
            FOUT=0.0
            CPFLAG=0
            Do 630 I=1,NCPTS
!
!  The ICP(I) array created by the preceding 1CPT record connects upstream-
!  downstream to read-in sequnces. ICP(I) is the order in which the control
!  points are read from the SIM input and output files. The array index I
!  represents upstream-to-downstream order.
!
               If(ICPI.LE.0) Then
                  CP=I
               Else
                  CP=ICP(I)
               Endif
!
!  Computations do not continue downstream of control
!  point CPBUD specified on the 2BUD record.
!
               If(CPFLAG.EQ.9) Goto 630
               If(CPID(CP,1).EQ.CPBUD.and.BASIN.EQ.2) CPFLAG=9
!
!  Variables are initialized as zero.
!
               DIV=0.0
               FCL=0.0
               FCLC=0.0
               FIN=0.0
               FINNEG=0.0
               FIN2=0.0
               FNAT=0.0
               FREG=0.0
!
!  Incremental naturalized flow FNAT entering CP.
!
               FNAT=NAT(CP)
               If(NUP(CP).GT.0) Then
                  Do K=1,NUP(CP)
                     J=CPUS(CP,K)
                     FNAT=FNAT-NAT(J)
                  Enddo
               Endif
!
!  Regulated flow FREG, channel losses FCL, and loss  
!  credits FCLC at control points upstream of CP.
!
               If(NUP(CP).GT.0) Then
                  Do K=1,NUP(CP)
                     J=CPUS(CP,K)
                     FREG=FREG+REG(J)
                     FCLC=FCLC+CLC(J)
                     FCL=FCL+CL(J)
                  Enddo
               Endif
!
!  Summation FIN of flows entering CP.
!
               FIN=FNAT+FREG+FCLC-FCL+RET(CP)+CINF(CP,M)
!
!  Negative inflows are recorded and adjusted.
!
               FINNEG=0.0
               If(FIN.LT.0.0) Then
                  FINNEG=FIN
                  If(NEGINF.EQ.3) FIN=0.0
                  If(NEGINF.EQ.2) Then
                     If(BSTO(CP).LE.0.0) Then
                        FIN=0.0
                     Elseif(BSTO(CP).LT.Abs(FIN)) Then
                        FIN=-BSTO(CP)
                     Endif
                  Endif
               Endif
!
!  Diversion is the target less shortage from SIM output file.
!
               DIV=DT(CP)-DS(CP)
!
!  Other flows FOTH such as reservoir releases for instream flow
!  requirements and hydropower are computed based a balance of
!  flow entering, flow leaving, and storage change.
!     FIN2 = flow in = flow out + storage change
!
               FIN2=REG(CP)+DIV+EVAP(CP)+STO(CP)-BSTO(CP)
               FOTH=FIN-FIN2
!
!  FOUT is the total flow leaving the control point excluding
!  net evaporation.
!
               FOUT(CP)=REG(CP)+DIV+FOTH
!
!  Control point water budget headings are written.
!
               If(CPOUT.GE.1) Then
                  If(HEADING.EQ.0) Then
                     HEADING=HEADING+1
                     Write(2,*)
                     Write(2,520)
520               Format('YEAR  M   CPID  NAT(CP)  CLC(CP)  CL(CP)   ',
     +                   '  FNAT     FREG  RET(CP)     CINF     FCLC ',
     +                   '     FCL  REG(CP)      DIV     FOTH EVAP(CP)',
     +                   ' BSTO(CP)  STO(CP)     XSTO     XSUM',/)
                  Endif
!
!  Control point water budget is written.
!
                  If(CPOUT.EQ.2.or.CPID(CP,1).EQ.CPBUD.or.
     +              (CPBUD.EQ.'      '.and.CPID(CP,2).EQ.'   OUT')) Then
                     XSTO=STO(CP)-BSTO(CP)
                     XSUM=FNAT+FREG+RET(CP)+CINF(CP,M)+FCLC-FCL-REG(CP)
     +                    -DIV-FOTH-EVAP(CP)
                     Write(2,530) YEAR,M,CPID(CP,1),NAT(CP),CLC(CP),
     +                            CL(CP),FNAT,FREG,RET(CP),CINF(CP,M),
     +                            FCLC,FCL,REG(CP),DIV,FOTH,EVAP(CP),
     +                            BSTO(CP),STO(CP),XSTO,XSUM
530                  Format(I4,I3,1x,A6,17F9.0)
                  Endif
               Endif
!
!  Quantities are totaled for basin summary table.
!
               If(BASIN.GE.0) Then
                  If((CPBUD.EQ.'      '.and.CPID(CP,2).EQ.'   OUT').
     +               or.CPID(CP,1).EQ.CPBUD) Then
                     TOTFLOW(6)=TOTFLOW(6)+REG(CP)
                     TOTFLOW(17)=TOTFLOW(17)+NAT(CP)
                  Endif
                  TOTFLOW(1)=TOTFLOW(1)+FNAT
                  TOTFLOW(2)=TOTFLOW(2)+RET(CP)
                  TOTFLOW(3)=TOTFLOW(3)+CINF(CP,M)
                  TOTFLOW(4)=TOTFLOW(4)+FCLC
                  TOTFLOW(5)=TOTFLOW(5)+FCL
                  TOTFLOW(7)=TOTFLOW(7)+DIV
                  TOTFLOW(8)=TOTFLOW(8)+EVAP(CP)
                  If(YEAR.EQ.YRFIRST.and.M.EQ.1) Then
                     TOTFLOW(9)=TOTFLOW(9)+BSTO(CP)
                  Elseif(YR.EQ.YREND.and.M.EQ.12) Then
                     TOTFLOW(10)=TOTFLOW(10)+STO(CP)
                  Endif
                  If(FNAT.LT.0) TOTFLOW(13)=TOTFLOW(13)-FNAT
                  TOTFLOW(14)=TOTFLOW(14)-FINNEG
                  TOTFLOW(16)=TOTFLOW(16)+FOTH
               Endif
!
!  +++++++++++++++++++++  End of Control Point Loop  ++++++++++++++++++++++
!
630         End Do
!
!  +++++++++++++++++++++++  End of Time Step Loop  ++++++++++++++++++++++++
!
640      End Do
650   End Do
!
!  Additional computations for the basin budget table.
!
      If(BASIN.GE.0) Then
         TOTFLOW(11)=TOTFLOW(10)-TOTFLOW(9)
         TOTFLOW(12)=TOTFLOW(1)+TOTFLOW(2)+TOTFLOW(3)+TOTFLOW(4)
     +         -TOTFLOW(5)-TOTFLOW(6)-TOTFLOW(7)-TOTFLOW(8)-TOTFLOW(16)
         TOTFLOW(15)=TOTFLOW(12)-TOTFLOW(11)
!
!  Totals are written to the basin budget table.
!
         N=YRLAST-YRFIRST+1
         Write(2,670)
         Write(2,680) YRFIRST,YRLAST,CPBUD
         Write(2,690)
         Write(2,700) TOTFLOW(1),TOTFLOW(1)/N
         Write(2,710) TOTFLOW(2),TOTFLOW(2)/N
         Write(2,720) TOTFLOW(3),TOTFLOW(3)/N
         Write(2,730) TOTFLOW(4),TOTFLOW(4)/N
         Write(2,740) TOTFLOW(5),TOTFLOW(5)/N
         Write(2,750) TOTFLOW(6),TOTFLOW(6)/N
         Write(2,760) TOTFLOW(7),TOTFLOW(7)/N
         Write(2,770) TOTFLOW(16),TOTFLOW(16)/N
         Write(2,780) TOTFLOW(8),TOTFLOW(8)/N
         Write(2,870)
         Write(2,790) TOTFLOW(12),TOTFLOW(12)/N
         Write(2,870)
         Write(2,800) TOTFLOW(9),TOTFLOW(9)/N
         Write(2,810) TOTFLOW(10),TOTFLOW(10)/N
         Write(2,870)
         Write(2,820) TOTFLOW(11),TOTFLOW(11)/N
         Write(2,870)
         Write(2,830) TOTFLOW(15),TOTFLOW(15)/N
         If(TOTFLOW(14).GT.0.0) Then
            Write(2,840) TOTFLOW(14),TOTFLOW(14)/N
         Endif
         If(TOTFLOW(13).GT.0.0) Write(2,850) TOTFLOW(13),TOTFLOW(13)/N
         Write(2,860) TOTFLOW(17),TOTFLOW(17)/N
!
670      Format(/,'River Basin Volume Budget Summary')
680      Format(I4,'-',I4,3x,A6)
690      Format(30x,'           Total  Mean Annual',/)
700      Format('Naturalized flows               ',F15.0,F12.1)
710      Format('Return flows                    ',F15.0,F12.1)
720      Format('CI record constant inflows      ',F15.0,F12.1)
730      Format('Channel loss credits            ',F15.0,F12.1)
740      Format('Channel losses                  ',F15.0,F12.1)
750      Format('Regulated flows at outlet       ',F15.0,F12.1)
760      Format('Diversions                      ',F15.0,F12.1)
770      Format('Other flows at control points   ',F15.0,F12.1)
780      Format('Net evaporation                 ',F15.0,F12.1)
790      Format('Inflows - Outflows              ',F15.0,F12.1)
800      Format('Beginning reservoir storage     ',F15.0,F12.1)
810      Format('Ending reservoir storage        ',F15.0,F12.1)
820      Format('Change in storage               ',F15.0,F12.1)
830      Format('Water balance difference        ',F15.0,F12.1)
840      Format(/,'Negative inflows to control points',F13.0,F12.1)
850      Format(/,'Negative incremental natural flows',F13.0,F12.1)
860      Format(/,'Naturalized flows at outlet   ',F17.0,F12.1)
870      Format(37x,'----------  ----------')
!
      Endif
!
      Write(20,900)
900   Format('*** Finished 2BUD record water budget.')
!
!  Return to main program from Subroutine BUDGET
!
      Return
      End Subroutine BUDGET
!
!  ***********************************************************************
!
      Subroutine SERIES
!
!   *-*-*-*-*  2NAT, 2REG, 2UNA, 2CLO, 2CLC, 2RFR, 2URR. 2STO,  *-*-*-*-*
!   *-*-*-*-*  2EVA, 2DEP, 2TAR, 2SHT, 2DIV, 2RFL, 2ASF, 2ROR,  *-*-*-*-*
!   *-*-*-*-*  2IFT, 2IFS, 2HPS, 2HPE, 2RID, 2XAV, 2RIR, 2RAH,  *-*-*-*-*
!   *-*-*-*-*  2RNA, 2EPD, 2EVR, 2WSE, 2CPI, 2RSC, 2RSD, 2FSV   *-*-*-*-*
!   *-*-*-*-*           or equivalent type 6 records            *-*-*-*-*
!
!  Subroutine SERIES develops tables of daily, monthly, and annual series of
!  data for a control point (ID=0), water right (ID=1), reservoir (ID=2), or
!  water right group (ID=3). Tables may be developed for essentially any of
!  the time seies variables included in the SIM OUT or SIMD SUB file.
!
      Use COMVAR
!
      Real AF,DX(DAYS),MDATA(12),MEAN(13),MX(MONTHS),SUM(13),TDATA,        !day
     +     TDATA1,TDATA2,TDATA3,X,XF,XXX,YTOTAL
!
      Integer COUNT,CREC,DP,I,ID,IP,J,K,KO,L,LOOP,MAM,MAN,MAT,MONTH,
     +        MORE,MYR,MM,MT,N,NN,NUM,PERIOD,PT,REC1,RTIME,SEQ,SKIP,
     +        TA,TIME,YEAR
      Integer IPLAN,ISTAT,NDSS,NPATH,NVALS
!
      Integer::DAY                                                         !day
      Integer,Allocatable,Dimension(:)::SCRIPTS                            !day
      Real,Allocatable,Dimension(:)::TEMP,YEARMEAN,MONTHMEAN               !day
!
      Character(len=1)  DECIMAL
      Character(len=2)  DSSDAY,IFFLAG
      Character(len=3)  M(23)
      Character(len=4)  CD,CTIME,CTYPE
      Character(len=5)  CUNITS
      Character(len=6)  ID6
      Character(len=8)  GPID1,GPID2,GP1,GP2,HEAD(100,2)
      Character(len=9)  CDATE
      Character(len=16) ID16
      Character(len=32) A,B,C,D,E,F,K1,K2
      Character(len=64) CPATH,CNAME
!
      MDATA=0.0
      MEAN=0.0
      SUM=0.0
      KO=0
!
!   SIM (UO=4) or SIMD (UO=10) output file unit number.
!
      If(SIMD.EQ.1) Then
         UO=10
      Else
         UO=4
      Endif
!
!   Allocation of daily (sub-monthly) arrays.
!
      Allocate(SCRIPTS(DAYS),TEMP(DAYS))                                    !day
      Allocate(YEARMEAN(100),MONTHMEAN(100))                                !day
!
!   Format statements for table headings.
!
10    Format('NATURALIZED STREAMFLOWS (',A5,') AT CONTROL POINT ',A6)   !2NAT
20    Format('REGULATED STREAMFLOWS (',A5,') AT CONTROL POINT ',A6)     !2REG
30    Format('UNAPPROPRIATED FLOWS (',A5,') AT CONTROL POINT ',A6)      !2UNA
40    Format('CHANNEL LOSSES (',A5,') FOR THE REACH BELOW',             !2CLO
     +          ' CONTROL POINT ',A6)
50    Format('CHANNEL LOSS CREDITS (',A5,') FOR THE REACH BELOW',       !2CLC
     +          ' CONTROL POINT ',A6)
60    Format('RETURN FLOW (',A5,') ENTERING AT CONTROL POINT ',A6)      !2RFR
70    Format('REGULATED FLOWS (',A5,') FROM UPSTREAM RESERVOIR',
     +        ' RELEASES AT CONTROL POINT ',A6)                         !2URR
80    Format('EOP RESERVOIR STORAGE (',A5,') AT CONTROL POINT ',A6)     !2STO
90    Format('EOP RESERVOIR STORAGE (',A5,') FOR WATER RIGHT ',A16)     !2STO
100   Format('END-OF-MONTH STORAGE (',A5,') FOR RESERVOIR ',A6)         !2STO
105   Format('END-OF-MONTH STORAGE (',A5,') FOR WATER RIGHT GROUP ',A8) !2STO
110   Format('EVAPORATION (',A5,') AT CONTROL POINT ',A6)               !2EVA
120   Format('EVAPORATION (',A5,') FOR WATER RIGHT ',A16)               !2EVA
130   Format('EVAPORATION (',A5,') FOR RESERVOIR ',A6)                  !2EVA
135   Format('EVAPORATION (',A5,') FOR WATER RIGHT GROUP ',A8)          !2EVA
140   Format('STREAMFLOW DEPLETIONS (',A5,') AT CONTROL POINT ',A6)     !2DEP
150   Format('STREAMFLOW DEPLETIONS (',A5,') FOR WATER RIGHT ',A16)     !2DEP
160   Format('STREAMFLOW DEPLETIONS (',A5,') FOR WATER RIGHT',          !2DEP
     +       ' GROUP ',A8)
170   Format('DIVERSION TARGETS (',A5,') AT CONTROL POINT ',A6)         !2TAR
180   Format('DIVERSION TARGETS (',A5,') FOR WATER RIGHT ',A16)         !2TAR
190   Format('DIVERSION TARGETS (',A5,') FOR WATER RIGHT',              !2TAR
     +       ' GROUP ',A8)
195   Format('RESERVOIR RELEASE TARGETS (',A5,') FOR INSTREAM FLOW '    !2TAR
     +       'REQUIREMENTS FOR IF RIGHT ',A16)
200   Format('DIVERSION SHORTAGES (',A5,') AT CONTROL POINT ',A6)       !2SHT
210   Format('DIVERSION SHORTAGES (',A5,') FOR WATER RIGHT ',A16)       !2SHT
220   Format('DIVERSION SHORTAGES (',A5,') FOR WATER RIGHT',            !2SHT
     +       ' GROUP ',A8)
225   Format('RESERVOIR RELEASE SHORTAGES (',A5,') FOR INSTREAM FLOW '  !2SHT
     +       'REQUIREMENTS FOR IF RIGHT ',A16)
230   Format('DIVERSIONS (',A5,') AT CONTROL POINT ',A6)                !2DIV
240   Format('DIVERSIONS (',A5,') FOR WATER RIGHT ',A16)                !2DIV
250   Format('DIVERSIONS (',A5,') FOR WATER RIGHT GROUP ',A8)           !2DIV
255   Format('RESERVOIR RELEASES (',A5,') FOR INSTREAM FLOW '           !2SHT
     +       'REQUIREMENTS FOR IF RIGHT ',A16)
260   Format('RETURN FLOW (',A5,') FOR WATER RIGHT ',A16)               !2RFL
270   Format('AVAILABLE STREAMFLOW (',A5,') FOR WATER RIGHT ',A16)      !2ASF
280   Format('RELEASES FROM RESERVOIR (',A5,') FOR WATER RIGHT ',A16)   !2ROR
290   Format('INSTREAM FLOW TARGETS (',A5,') FOR WATER RIGHT ',A16)     !2IFT
300   Format('INSTREAM FLOW SHORTAGES (',A5,') FOR WATER RIGHT ',A16)   !2IFS
310   Format('HYDROPOWER SHORTAGES(+) OR SECONDARY ENERGY(-)',          !2HPS
     +       ' (',A5,') FOR RESERVOIR ',A6)
320   Format('ENERGY GENERATED (',A5,') AT RESERVOIR ',A6)              !2HPE
330   Format('INFLOWS (',A5,') FROM STREAMFLOW DEPLETIONS AT ',         !2RID
     +       'RESERVOIR ',A6)
340   Format('INFLOWS FROM OTHER RELEASES (',A5,') AT RESERVOIR ',A6)   !2RIR
350   Format('RELEASES (',A5,') ACCESSIBLE TO HYDROPOWER AT ',
     +       'RESERVOIR ',A6)                                           !2RAH
360   Format('RELEASES (',A5,') NOT ACCESSIBLE TO HYDROPOWER AT ',
     +       'RESERVOIR ',A6)                                           !2RNA
370   Format('ADJUSTED EVAPORATION-PRECIPITATION RATE (depth/month) ',
     +       'FOR RESERVOIR ',A6)                                       !2EPD
380   Format('EVAPORATION-PRECIPITATION RATE (depth/month) FOR ',
     +       'RESERVOIR ',A6)                                           !2EVR
390   Format('WATER SURFACE ELEVATION FOR RESERVOIR ',A6)               !2WSE
400   Format('INFLOWS FOR CONTROL POINT ',A6)                           !2CPI
410   Format('INCREASE IN AVAILABLE STREAMFLOW RESULTING FROM '
     +       'PX RECORD CP LIMIT OPTION FOR WATER RIGHT ',A16)          !2XAV
420   Format('STORAGE CAPACITY (',A5,') FOR RESERVOIR ',A6)             !2RSC
430   Format('STORAGE DRAWDOWN (',A5,') FOR RESERVOIR ',A6)             !2RSD
440   Format(A4,8X,A3,11(6X,A3),7X,A5)
450   Format(A4,8X,A3,11(6X,A3),8X,A4)
460   Format(/,127('-'))
470   Format(127('-'))
      
500   Format('FS RECORD FLOW ACCUMULATED VOLUMES (',A5,
     +       ') FOR WATER RIGHT ',A16)                                  !2FSV
510   Format('FS RECORD FLOW COUNT FOR WATER RIGHT ',A16)               !2FSC
!
!   The order in which months are listed in the table headings is set based
!   on MONTH1 specified in the UNIT record, with a default of MONTH1=JAN.
!
      L=1
      If(MONTH1.EQ.'  JAN'.or.MONTH1.EQ.'  Jan') L=1
      If(MONTH1.EQ.'  FEB'.or.MONTH1.EQ.'  Feb') L=2
      If(MONTH1.EQ.'  MAR'.or.MONTH1.EQ.'  Mar') L=3
      If(MONTH1.EQ.'  APR'.or.MONTH1.EQ.'  Apr') L=4
      If(MONTH1.EQ.'  MAY'.or.MONTH1.EQ.'  May') L=5
      If(MONTH1.EQ.'  JUN'.or.MONTH1.EQ.'  Jun') L=6
      If(MONTH1.EQ.'  JUL'.or.MONTH1.EQ.'  Jul') L=7
      If(MONTH1.EQ.'  AUG'.or.MONTH1.EQ.'  Aug') L=8
      If(MONTH1.EQ.'  SEP'.or.MONTH1.EQ.'  Sep') L=9
      If(MONTH1.EQ.'  OCT'.or.MONTH1.EQ.'  Oct') L=10
      If(MONTH1.EQ.'  NOV'.or.MONTH1.EQ.'  Nov') L=11
      If(MONTH1.EQ.'  DEC'.or.MONTH1.EQ.'  Dec') L=12
      M(1) ='JAN'
      M(2) ='FEB'
      M(3) ='MAR'
      M(4) ='APR'
      M(5) ='MAY'
      M(6) ='JUN'
      M(7) ='JUL'
      M(8) ='AUG'
      M(9) ='SEP'
      M(10)='OCT'
      M(11)='NOV'
      M(12)='DEC'
      M(13)='JAN'
      M(14)='FEB'
      M(15)='MAR'
      M(16)='APR'
      M(17)='MAY'
      M(18)='JUN'
      M(19)='JUL'
      M(20)='AUG'
      M(21)='SEP'
      M(22)='OCT'
      M(23)='NOV'
!
!   Table specifications are read from the input file (unit=1) record.
!
      Read(1,720,IOSTAT=STATUS) CD,TA,PT,MORE,ID,NUM,DECIMAL,MAT,TIME,
     +                          XF,AF
720   Format(A4,5I4,3x,A1,2I4,2F8.0)
      If(Abs(XF).LT.0.00001) XF=1.0
      If(Abs(AF).LT.0.00001) AF=0.0
!
!   Input error checks.
!
      If(PT.GE.1.and.PT.LE.3) Then
         If(MORE.GE.1) XMORE=99
         If(MORE.EQ.0) XMORE=0
      Endif
      If(STATUS.NE.0) Then
         Write(20,730) CD
730      Format(' ERROR: Fortran IOSTAT error occured reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(PT.LT.0.or.PT.GT.6) Then
         Write(20 740) PT
740      Format(' ERROR: PT of',I3,' in field 3 is not valid.')
         Call ERROR
      Endif
      If(ID.LT.0.or.ID.GT.3) Then
         Write(20 750) ID
750      Format(' ERROR: ID of',I3,' in field 5 is not valid.')
         Call ERROR
      Endif
      If(DECIMAL.NE.' '.and.DECIMAL.NE.'0'.and.DECIMAL.NE.'1'.and.
     +   DECIMAL.NE.'2'.and.DECIMAL.NE.'3'.and.DECIMAL.NE.'4') Then
         Write(20 760) DECIMAL
760      Format(' ERROR: DECIMAL of ',A1,' in field 7 is not valid.')
         Call ERROR
      Endif
!
!   If an invalid ID is specified in field 5 of an input record,the default
!   ID is assigned and warning messages are written to the monitor.
!
      If((CD.EQ.'2NAT'.or.CD.EQ.'2UNA'.or.CD.EQ.'2REG'.or.CD.EQ.'2CLO'
     +                .or.CD.EQ.'2CLC'.or.CD.EQ.'2RFR'.or.CD.EQ.'2URR'
     +                .or.CD.EQ.'2CPI').and.ID.NE.0) Then
         Write(20,770) CD,ID
         ID=0
      Elseif((CD.EQ.'2STO'.or.CD.EQ.'2EVA').and.
     +       (ID.NE.0.and.ID.NE.1.and.ID.NE.2.and.ID.NE.3)) Then
         Write(20,770) CD,ID
         ID=0
      Elseif((CD.EQ.'2DEP'.or.CD.EQ.'2TAR'.or.CD.EQ.'2SHT'.or.
     +        CD.EQ.'2DIV').and.(ID.NE.0.and.ID.NE.1.and.ID.NE.3)) Then
         Write(20,770) CD,ID
         ID=0
      Elseif(CD.EQ.'2RFL'.and.(ID.NE.1.and.ID.NE.3)) Then
         Write(20,780) CD,ID
         ID=1
      Elseif((CD.EQ.'2ASF'.or.CD.EQ.'2ROR'.or.CD.EQ.'2IFT'.or.
     +        CD.EQ.'2IFS'.or.CD.EQ.'2XAV'.or.CD.EQ.'2FSV'.or.
     +        CD.EQ.'2FSC').and.(ID.NE.1)) Then
         Write(20,780) CD,ID
         ID=1
      Elseif((CD.EQ.'2HPS'.or.CD.EQ.'2HPE'.or.CD.EQ.'2RID'.or.
     +        CD.EQ.'2RIR'.or.CD.EQ.'2RAH'.or.CD.EQ.'2RNA'.or.
     +        CD.EQ.'2EPD'.or.CD.EQ.'2EVR'.or.CD.EQ.'2WSE'.or.
     +        CD.EQ.'2RSC'.or.CD.EQ.'2RSD').and.(ID.NE.2)) Then
         Write(20,790) CD,ID
         ID=2
      Endif
770   Format('  WARNING: ',A4,' record field 5 has an invalid ID of',I3,
     +                 /,10x,' The control point ID of 0 was assigned.')
780   Format('  WARNING: ',A4,' record field 5 has an invalid ID of',I3,
     +                 /,10x,' The water rights ID of 1 was assigned.')
790   Format('  WARNING: ',A4,' record field 5 has an invalid ID of',I3,
     +                 /,10x,' The resevoir ID of 2 was assigned.')
!
!   An error message is printed and the record skipped if field 5 of an
!   input record specifies control points, water rights, or reservoirs,
!   but there are none in the WRAP-SIM output file.
!
      If(SIMD.EQ.0) Then                                                    !day
         If(NCPTS.EQ.0.and.ID.EQ.0) Then
            Write(20,800) CD,ID
            Goto 2000
         Elseif(NWROUT.EQ.0.and.(ID.EQ.1.or.ID.EQ.3)) Then
            Write(20,810) CD,ID
            Goto 2000
         Elseif(NREOUT.EQ.0.and.ID.EQ.2) Then
            Write(20,820) CD,ID
            Goto 2000
         Endif
      Else                                                                  !day
         If(NCPO2.EQ.0.and.ID.EQ.0) Then                                    !day
            Write(20,830) CD,ID                                             !day
            Goto 2000                                                       !day
         Elseif(NWROUT2.EQ.0.and.(ID.EQ.1.or.ID.EQ.3)) Then                 !day
            Write(20,840) CD,ID                                             !day
            Goto 2000                                                       !day
         Elseif(NREOUT2.EQ.0.and.ID.EQ.2) Then                              !day
            Write(20,850) CD,ID                                             !day
            Goto 2000                                                       !day
         Endif                                                              !day
      Endif                                                                 !day
800   Format(' ERROR: ',A4,' record field 5 has an ID of',I2,' but',
     +       ' there are no control points in the WRAP-SIM OUT file.',
     +       /,8x,'This record is not applied.')
810   Format(' ERROR: ',A4,' record field 5 has an ID of',I2,' but',
     +       ' there are no water rights in the WRAP-SIM OUT file.',
     +       /,8x,'This record is not applied.')
820   Format(' ERROR: ',A4,' record field 5 has an ID of',I2,' but',
     +       ' there are no reservoirs in the WRAP-SIM OUT file.',
     +       /,8x,'This record is not applied.')
830   Format(' ERROR: ',A4,' record field 5 has an ID of',I2,' but',        !day
     +       ' there are no control points in the WRAP-SIMD SUB file.',     !day
     +       /,8x,'This record is not applied.')                            !day
840   Format(' ERROR: ',A4,' record field 5 has an ID of',I2,' but',        !day
     +       ' there are no water rights in the WRAP-SIMD SUB file.',       !day
     +       /,8x,'This record is not applied.')                            !day
850   Format(' ERROR: ',A4,' record field 5 has an ID of',I2,' but',        !day
     +       ' there are no reservoirs in the WRAP-SIMD SUB file.',         !day
     +       /,8x,'This record is not applied.')                            !day
!
!  Warning message for monthly option (CR2=0) or annual option
!  with more than 12 months written to CRM file (CR3=2).
!
      If(CR1.GT.0.and.CR2.LE.0.and.CR3.LE.1) Then
         Write(20,860) CD
860      Format(' WARNING: ',A4,' record has no meaning for CR2 of 0.')
      Endif
      If(CR1.GT.0.and.CR3.GE.2) Then
         Write(20,870) CD
870      Format(' ERROR: ',A4,' record is invalid for CR3 of 2.')
         Call ERROR
      Endif
!
!  Defaults are set for Job 6 sub-monthly data processing.
!
      If(SIMD.EQ.1) Then                                                    !day
         TA=0                                                               !day
         If(PT.LE.0) PT=1                                                   !day
         If(PT.EQ.5) PT=4                                                   !day
      Endif                                                                 !day
!
!   If NUM is greater than zero, the control point,water right,
!   reservoir, or group identifiers are read from IDEN records.
!
      If(NUM.GT.0) Then
         TID=ID
         NID=NUM
         Call IDEN
      Endif
      NUM = Abs(NUM)
!
!   Since the WRAP-SIM output file (unit=4) is read as a direct
!   access file, record counters are devised for use in locating
!   the records to be read.
!
      COUNT = NUM
      If(SIMD.EQ.0) Then                                                    !day
         CREC = NWROUT + NCPTS + NREOUT
         If(ID.EQ.1) Then
            REC1 = 6
            SKIP = NWROUT
            If(NUM.EQ.0) COUNT = NWROUT
         Elseif (ID.EQ.0) Then
            REC1 = 6 + NWROUT
            SKIP = NCPTS
            If(NUM.EQ.0) COUNT = NCPTS
         Elseif (ID.EQ.2) Then
            REC1 = 6 + NWROUT + NCPTS
            SKIP = NREOUT
            If(NUM.EQ.0) COUNT = NREOUT
         Elseif (ID.EQ.3) Then
            REC1 = 6
            SKIP = NWROUT
         Endif
      Else                                                                  !day
         CREC=NWROUT2+NCPO2+NREOUT2                                         !day
         If(ID.EQ.1) Then                                                   !day
            REC1=7                                                          !day
            SKIP=NWROUT2                                                    !day
            If (NUM.EQ.0) COUNT=NWROUT2                                     !day
         Elseif(ID.EQ.0) Then                                               !day
            REC1=7+NWROUT2                                                  !day
            SKIP=NCPO2                                                      !day
            If (NUM.EQ.0) COUNT=NCPO2                                       !day
         Elseif(ID.EQ.2) Then                                               !day
            REC1=7+NWROUT2+NCPO2                                            !day
            SKIP=NREOUT2                                                    !day
            If (NUM.EQ.0) COUNT=NREOUT2                                     !day
         Elseif(ID.EQ.3) Then                                               !day
            REC1=7                                                          !day
            SKIP=NWROUT2                                                    !day
         Endif                                                              !day
      Endif                                                                 !day
!
!  Decimal is set for output data.
!
      DP=0
      If(TA.GE.1.or.PT.EQ.1.or.PT.EQ.2.or.PT.EQ.3.or.PT.EQ.6) Then
         If(DECIMAL.EQ.'0') DP=0
         If(DECIMAL.EQ.'1') DP=1
         If(DECIMAL.EQ.'2') DP=2
         If(DECIMAL.EQ.'3') DP=3
         If(DECIMAL.EQ.'4') DP=4
      Endif
      If(TA.GE.1.and.DECIMAL.EQ.' ') Then
         If(CD.EQ.'2NAT'.or.CD.EQ.'2UNA'.or.CD.EQ.'2REG'.or.
     +      CD.EQ.'2CLC'.or.CD.EQ.'2CLO'.or.CD.EQ.'2URR') Then
            DP=0
         Elseif(CD.EQ.'2TAR'.or.CD.EQ.'2SHT'.or.CD.EQ.'2DIV'.or.
     +          CD.EQ.'2IFT'.or.CD.EQ.'2IFS'.or.CD.EQ.'2WSE') Then
            DP=2
         Elseif(CD.EQ.'2EPD'.or.CD.EQ.'2EVR') Then
            DP=4
         Else
            DP=1
         Endif
      Endif
!
!  HEC-DSS file is opened and array allocated.
!
      If(PT.EQ.4.or.PT.EQ.5) Then
         HECDSS=HECDSS+1
!
!  Integer year is read from OUT or SUB file and converted to
!  character YRSTDSS.
!
         If(OUTFORM.EQ.1) Then
            Read(UO,REC=5) IO1
            Read(IO1) YRSTDSS
         Else
            Read(UO,890,REC=5) YRSTDSS
890         Format(2x,A4)
         Endif
!
!  HEC-DSS file VALUES array is allocated.
!
         If(SIMD.EQ.0) Then                                                 !day
            If(PT.EQ.4) Then
               NVALS=MONTHS
               Allocate(VALUES(MONTHS))
            Elseif(PT.EQ.5) Then
               NVALS=NYRS
               Allocate(VALUES(NYRS))
            Endif
         Else                                                               !day
            NVALS=DAYS                                                      !day
            Allocate(VALUES(DAYS))                                          !day
         Endif                                                              !day
      Endif
!
!   ++++++++++  Begin Outer "COUNT" Loop  ++++++++++
!   Beginning of loop to develop tables for the "COUNT" control points,
!   water rights, reservoirs, or water rights groups.  The loop extends
!   from statement 900 to the "IF" statement near the end of subroutine.
!
      LOOP = 0
900   LOOP = LOOP + 1
      Do I = 1,13
         SUM(I) = 0.0
      End Do
      NDSS=0
      If(TA.GE.1) Call TITLES
!
!   Find the record of the WRAP-SIM output file (unit=4) from which
!   the first data item will be read for the first entry in table.
!
      IFFLAG='  '
      RECD = REC1
      If(OUTFORM.EQ.1) Then
         If(ID.EQ.0.or.ID.EQ.2) Read(UO,REC=RECD) ID6
         If(ID.EQ.1) Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,7),ID16
         If(IO1.EQ.-1) IFFLAG='IF'
         If(ID.EQ.3) Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,7),C4,
     +                                 GPID1,GPID2
      Else
         If(ID.EQ.0.or.ID.EQ.2) Read(UO,910,REC=RECD) ID6
         If(ID.EQ.1)            Read(UO,920,REC=RECD) IFFLAG,ID16
         If(ID.EQ.3)            Read(UO,930,REC=RECD) GPID1,GPID2
910      Format(A6)
920      Format(A2,81x,A16)
930      Format(99x,2A8)
      Endif
!
!   Error check for nonzero NUM that control points, water rights,
!   reservoirs, or groups listed on TABLES input record are found
!   in the WRAP-SIM output file.
!
      N = 1
      If(NUM.EQ.0) Goto 990
940   If(ID.EQ.0.and.ID6.EQ.IDCP(LOOP)) Goto 990
      If(ID.EQ.2.and.ID6.EQ.IDRES(LOOP)) Goto 990
      If(ID.EQ.1.and.ID16.EQ.IDEN16(LOOP)) Goto 990
      If(ID.EQ.3.and.(GPID1.EQ.IDEN8(LOOP).or.
     +      GPID2.EQ.IDEN8(LOOP))) Goto 990
      RECD = RECD + 1
      N = N + 1
      If(OUTFORM.EQ.1) Then
         If(ID.EQ.0.or.ID.EQ.2) Read(UO,REC=RECD) ID6
         If(ID.EQ.1) Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,7),ID16
         If(IO1.EQ.-1) IFFLAG='IF'
         If(ID.EQ.3) Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,7),C4,
     +                                 GPID1,GPID2
      Else
         If(ID.EQ.0.or.ID.EQ.2) Read(UO,910,REC=RECD) ID6
         If(ID.EQ.1)            Read(UO,920,REC=RECD) IFFLAG,ID16
         If(ID.EQ.3)            Read(UO,930,REC=RECD) GPID1,GPID2
      Endif
      If(N.LE.CREC) Goto 940
      If(ID.EQ.0) Then
         Write(20,950) IDCP(LOOP), CD
950      Format(' ERROR: Control point ',A6,' from ',A4,' record',
     +          ' was not found in the WRAP output file.')
      Elseif(ID.EQ.1) Then
         Write(20,960) IDEN16(LOOP), CD
960      Format(' ERROR: Water right ',A16,' from ',A4,' record',
     +          ' was not found in the WRAP output file.')
      Elseif(ID.EQ.3) Then
         Write(20,970) IDEN8(LOOP), CD
970      Format(' ERROR: Group identifier ',A8,' from ',A4,
     +          ' record was not found in the WRAP output file.')
      Elseif(ID.EQ.2) Then
         Write(20,980) IDRES(LOOP), CD
980      Format(' ERROR: Reservoir ',A6,' from ',A4,' record',
     +          ' was not found in the WRAP output file.')
      Endif
      Call ERROR
!
!   Warning and return if no output is specified in record fields 2 and 3.
!
990   If(TA.EQ.0.and.PT.EQ.0) Then
         Write(20,1000)
1000     Format('WARNING: Both TA and PT are zero indicating that no ',
     +          'output is specified.')
         If(NUM.GT.0) Write(20,1010)
1010     Format(9x,'Identifiers from IDEN record(s) were read ',
     +             'for use with other records.')
         Return
      Endif
!
!   2IFT and 2IFS records are applicable only to instream flow rights.
!
      If((CD.EQ.'2IFT'.or.CD.EQ.'2IFS'.or.CD.EQ.'2FSV'.or.CD.EQ.'2FSC').
     +   and.IFFLAG.NE.'IF') Then
         Write(20,1020) CD,Adjustl(ID16)
1020     Format(' ERROR: A ',A4,' record is applicable only to ',
     +          'instream flow IF rights',/,8x,'and thus is not ',
     +          'applicable to ',A16)
         Call ERROR
      Endif
!
!   2RFL and 2XAV records are not applicable to IF record rights.
!
      If((CD.EQ.'2RFL'.or.CD.EQ.'2XAV').and.IFFLAG.EQ.'IF') Then
         Write(20,1030) CD,Adjustl(ID16)
1030     Format(' ERROR: A ',A4,' record is not applicable to ',
     +          'instream flow IF rights',/,8x,'and thus is not ',
     +          'applicable to ',A16)
         Call ERROR
      Endif
!
!   Increment column counter (MPLOT) and develop heading array for plot table.
!
      If(PT.GE.1.and.PT.LE.3) Then
         MPLOT=MPLOT+1
         HEAD(MPLOT,1)=CD(2:4)
         If(ID.EQ.0) HEAD(MPLOT,2)=ID6
         If(ID.EQ.1) Then
            ID16=Adjustl(ID16)
            HEAD(MPLOT,2)=ID16(1:8)
         Endif
         If(ID.EQ.2) HEAD(MPLOT,2)=ID6
         If(ID.EQ.3) HEAD(MPLOT,2)=IDEN8(LOOP)
      Endif
!
!   Format specifications are assigned for reading the SIM output file and
!   table headings are written to unit=2 based on values of CD and ID.
!
      If(CD.EQ.'2NAT'.or.CD.EQ.'6NAT') Then                                 !day
         K1='(83x,F11.0)'
         KO=8
         If(TA.GE.1) Write(2,10) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2REG'.or.CD.EQ.'6REG') Then                            !day
         K1='(94x,F11.0)'
         KO=9
         If(TA.GE.1) Write(2,20) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2UNA'.or.CD.EQ.'6UNA') Then                            !day
         K1='(61x,F11.0)'
         KO=6
         If(TA.GE.1) Write(2,30) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2CLO'.or.CD.EQ.'6CLO') Then                            !day
         K1='(116x,F10.0)'
         KO=11
         If(TA.GE.1) Write(2,40) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2CLC'.or.CD.EQ.'6CLC') Then                            !day
         K1='(105x,F11.0)'
         KO=10
         If(TA.GE.1) Write(2,50) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2RFR'.or.CD.EQ.'6RFR') Then                            !day
         K1='(72x,F11.0)'
         If(TA.GE.1) Write(2,60) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2URR'.or.CD.EQ.'6URR') Then                            !day
         K1='(126x,F10.0)'
         KO=12
         If(TA.GE.1) Write(2,70) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2STO'.or.CD.EQ.'6STO') Then                            !day
         K1='(39x,F11.0)'
         KO=4
         If(TA.GE.1) Then
            If(ID.EQ.1) K2='(39x,F11.0)'
            If(ID.EQ.3) K2='(39x,F11.0,49x,2A8)'
            If(ID.EQ.0) Write(2,80) UNIT,Adjustl(ID6)
            If(ID.EQ.1) Write(2,90) UNIT,Adjustl(ID16)
            If(ID.EQ.2) Write(2,100) UNIT,Adjustl(ID6)
            If(ID.EQ.3) Write(2,105) UNIT,Adjustl(IDEN8(LOOP))
         Endif
      Elseif (CD.EQ.'2EVA'.or.CD.EQ.'6EVA') Then                            !day
         K1='(28x,F11.0)'
         KO=3
         If(TA.GE.1) Then
            If(ID.EQ.1) K2='(28x,F11.0)'
            If(ID.EQ.3) K2='(28x,F11.0,60x,2A8)'
            If(ID.EQ.0) Write(2,110) UNIT,Adjustl(ID6)
            If(ID.EQ.1) Write(2,120) UNIT,Adjustl(ID16)
            If(ID.EQ.2) Write(2,130) UNIT,Adjustl(ID6)
            If(ID.EQ.3) Write(2,135) UNIT,Adjustl(IDEN8(LOOP))
         Endif
      Elseif (CD.EQ.'2DEP'.or.CD.EQ.'6DEP') Then                            !day
         K1='(50x,F11.0)'
         KO=5
         If(TA.GE.1) Then
            If(ID.EQ.1) K2='(50x,F11.0)'
            If(ID.EQ.3) K2='(50x,F11.0,38x,2A8)'
            If(ID.EQ.0) Write(2,140) UNIT,Adjustl(ID6)
            If(ID.EQ.1) Write(2,150) UNIT,Adjustl(ID16)
            If(ID.EQ.3) Write(2,160) UNIT,Adjustl(IDEN8(LOOP))
         Endif
      Elseif (CD.EQ.'2TAR'.or.CD.EQ.'6TAR') Then                            !day
         K1='(17x,F11.0)'
         KO=2
         If(ID.EQ.1) K2='(17x,F11.0)'
         If(ID.EQ.3) K2='(17x,F11.0,71x,2A8)'
         If(TA.GE.1) Then
            If(ID.EQ.0) Write(2,170) UNIT,Adjustl(ID6)
            If(ID.EQ.1) Then
               If(IFFLAG.NE.'IF') Write(2,180) UNIT,Adjustl(ID16)
               If(IFFLAG.EQ.'IF') Write(2,195) UNIT,Adjustl(ID16)
            Endif
            If(ID.EQ.3) Write(2,190) UNIT,Adjustl(IDEN8(LOOP))
         Endif
      Elseif (CD.EQ.'2SHT'.or.CD.EQ.'6SHT') Then                            !day
         K1='(6x,F11.0)'
         KO=1
         If(ID.EQ.1) K2='(6x,F11.0)'
         If(ID.EQ.3) K2='(6x,F11.0,82x,2A8)'
         If(TA.GE.1) Then
            If(ID.EQ.0) Write(2,200) UNIT,Adjustl(ID6)
            If(ID.EQ.1) Then
               If(IFFLAG.NE.'IF') Write(2,210) UNIT,Adjustl(ID16)
               If(IFFLAG.EQ.'IF') Write(2,225) UNIT,Adjustl(ID16)
            Endif
            If(ID.EQ.3) Write(2,220) UNIT,Adjustl(IDEN8(LOOP))
         Endif
      Elseif (CD.EQ.'2DIV'.or.CD.EQ.'6DIV') Then                            !day
         K1='(6x,F11.0,F11.0)'
         If(ID.EQ.1) K2='(6x,F11.0,F11.0)'
         If(ID.EQ.3) K2='(6x,F11.0,F11.0,71x,2A8)'
         If(TA.GE.1) Then
            If(ID.EQ.0) Write(2,230) UNIT,Adjustl(ID6)
            If(ID.EQ.1) Then
               If(IFFLAG.NE.'IF') Write(2,240) UNIT,Adjustl(ID16)
               If(IFFLAG.EQ.'IF') Write(2,255) UNIT,Adjustl(ID16)
            Endif
            If(ID.EQ.3) Write(2,250) UNIT,Adjustl(IDEN8(LOOP))
         Endif
      Elseif (CD.EQ.'2RFL'.or.CD.EQ.'6RFL') Then                            !day
         K1='(115x,F11.0)'
         KO=8
         If(TA.GE.1) Write(2,260) UNIT,Adjustl(ID16)
      Elseif (CD.EQ.'2ASF'.or.CD.EQ.'6ASF') Then                            !day
         K1='(61x,F11.0)'
         KO=6
         If(TA.GE.1) Write(2,270) UNIT,Adjustl(ID16)
      Elseif (CD.EQ.'2ROR'.or.CD.EQ.'6ROR') Then                            !day
         K1='(72x,F11.0)'
         KO=7
         If(TA.GE.1) Write(2,280) UNIT,Adjustl(ID16)
      Elseif (CD.EQ.'2IFT'.or.CD.EQ.'6IFT') Then                            !day
         K1='(99x,F11.0)'
         KO=8
         If(TA.GE.1) Write(2,290) UNIT,Adjustl(ID16)
      Elseif (CD.EQ.'2IFS'.or.CD.EQ.'6IFS') Then                            !day
         K1='(110x,F11.0)'
         KO=9
         If(TA.GE.1) Write(2,300) UNIT,Adjustl(ID16)
      Elseif (CD.EQ.'2HPS'.or.CD.EQ.'6HPS') Then                            !day
         K1='(6x,F11.0)'
         KO=1
         If(TA.GE.1) Write(2,310) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2HPE'.or.CD.EQ.'6HPE') Then                            !day
         K1='(17x,F11.0)'
         KO=2
         If(TA.GE.1) Write(2,320) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2RID'.or.CD.EQ.'6RID') Then                            !day
         K1='(50x,F11.0)'
         KO=5
         If(TA.GE.1) Write(2,330) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2RIR'.or.CD.EQ.'6RIR') Then                            !day
         K1='(61x,F11.0)'
         KO=6
         If(TA.GE.1) Write(2,340) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2RAH'.or.CD.EQ.'6RAH') Then                            !day
         K1='(72x,F11.0)'
         KO=7
         If(TA.GE.1) Write(2,350) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2RNA'.or.CD.EQ.'6RNA') Then                            !day
         K1='(83x,F11.0)'
         KO=8
         If(TA.GE.1) Write(2,360) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2EPD'.or.CD.EQ.'6EPD') Then                            !day
         K1='(94x,F11.0)'
         KO=9
         If(TA.GE.1) Write(2,370) Adjustl(ID6)
      Elseif (CD.EQ.'2EVR'.or.CD.EQ.'6EVR') Then                            !day
         K1='(105x,F11.0)'
         KO=10
         If(TA.GE.1) Write(2,380) Adjustl(ID6)
      Elseif (CD.EQ.'2WSE'.or.CD.EQ.'6WSE') Then                            !day
         K1='(116x,F11.0)'
         KO=11
         If(TA.GE.1) Write(2,390) Adjustl(ID6)
      Elseif (CD.EQ.'2RSC'.or.CD.EQ.'6RSC') Then                            !day
         K1='(127x,F9.0)'
         KO=12
         If(TA.GE.1) Write(2,420) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2RSD'.or.CD.EQ.'6RSD') Then                            !day
         K1='(39x,F11.0,77x,F9.0)'
         If(TA.GE.1) Write(2,430) UNIT,Adjustl(ID6)
      Elseif (CD.EQ.'2XAV'.or.CD.EQ.'6XAV') Then                            !day
         K1='(126x,F10.0)'
         KO=9
         If(TA.GE.1) Write(2,410) Adjustl(ID16)
      Elseif (CD.EQ.'2CPI'.or.CD.EQ.'6CPI') Then                            !day
         K1='(50x,F11.0,33x,F11.0,21x,F10.0)'
         If(TA.GE.1) Write(2,400) Adjustl(ID6)
      Elseif (CD.EQ.'2FSV'.or.CD.EQ.'6FSV') Then                            !day
         K1='(121x,F11.0)'
         If(TA.GE.1) Write(2,500) UNIT,Adjustl(ID16)
      Elseif (CD.EQ.'2FSC'.or.CD.EQ.'6FSC') Then                            !day
         K1='(132x,F4.0)'
         If(TA.GE.1) Write(2,510) Adjustl(ID16)
      Endif
!
!  Remainder of table heading for annual row/monthly column table.
!
      If(TA.GE.1) Then
         If(MAT.EQ.1) Write(2,1040) TIME
1040     Format('MOVING AVERAGE FOR',I3,' MONTHS')
         If(MAT.EQ.2) Write(2,1050) TIME
1050     Format('MOVING TOTAL FOR',I3,' MONTHS')
         Write(2,460)
         If(CD.EQ.'2STO'.or.CD.EQ.'2WSE'.or.CD.EQ.'2RSC'.or.
     +      CD.EQ.'2RSD') Then
            If(CR1.GT.0) Then
               Write(2,450) 'SEQ',M(L),M(L+1),M(L+2),M(L+3),M(L+4),
     +                M(L+5),M(L+6),M(L+7),M(L+8),M(L+9),M(L+10),
     +                M(L+11),'MEAN'
            Else
               Write(2,450) 'YEAR',M(L),M(L+1),M(L+2),M(L+3),M(L+4),
     +                M(L+5),M(L+6),M(L+7),M(L+8),M(L+9),M(L+10),
     +                M(L+11),'MEAN'
            Endif
         Else
            If(CR1.GT.0) Then
               Write(2,450) 'SEQ',M(L),M(L+1),M(L+2),M(L+3),M(L+4),
     +                M(L+5),M(L+6),M(L+7),M(L+8),M(L+9),M(L+10),
     +                M(L+11),'TOTAL'
            Else
               Write(2,440) 'YEAR',M(L),M(L+1),M(L+2),M(L+3),M(L+4),
     +               M(L+5),M(L+6),M(L+7),M(L+8),M(L+9),M(L+10),
     +               M(L+11),'TOTAL'
            Endif
         Endif
         Write(2,470)
      Endif
!
!  ++++++++++  Begin Inner Loop For Periods ++++++++++
!   Begin loop which is repeated for each of N=NYRS*NPRDS periods (months).
!
      SKIP = SKIP - RECD + REC1
      PERIOD = 0
      MONTH = 0
      MAM = 0
      YEAR = YRST
      SEQ = 1
      If(SIMD.EQ.0) N = MONTHS                                              !day
      If(SIMD.EQ.1) N = DAYS                                                !day
      YTOTAL = 0.0
1100  PERIOD = PERIOD + 1
      MONTH = MONTH + 1
      If(MONTH.EQ.1) MDATA = 0.0
!
!   Diversions (2DIV record) are determined by reading a target and shortage
!   from the SIM output file and then subtracting the shortage from target.
!   All other variables are read directly from the SIM output file.
!
      If(CD.EQ.'2DIV'.or.CD.EQ.'6DIV') Then
         If(ID.EQ.3) Then
            RECD=REC1+PERIOD*CREC-CREC
            K=NWROUT
            If(SIMD.EQ.1) K=NWROUT2                                         !day
            Do 1110 J=1,K
               If(OUTFORM.EQ.1) Then
                  Read(UO,REC=RECD) IO1,IO2,TDATA1,TDATA2,
     +                              (XO(I),I=3,7),C4,GP1,GP2
               Else
                  Read(UO,K2,REC=RECD) TDATA1,TDATA2,GP1,GP2
               Endif
               RECD = RECD + 1
               If(GP1.NE.IDEN8(LOOP).and.GP2.NE.IDEN8(LOOP)) Goto 1110
               TDATA=TDATA2-TDATA1
               If(SIMD.EQ.0) Then
                  MDATA(MONTH) = MDATA(MONTH) + TDATA
               Else
                  PLOTD(MONTH,MPLOT)=PLOTD(MONTH,MPLOT)+TDATA               !day
               Endif
1110        End Do
         Else
            If(OUTFORM.EQ.1) Then
               Read(UO,REC=RECD) IO1,IO2,TDATA1,TDATA2
            Else
               Read(UO,K1,REC=RECD) TDATA1,TDATA2
            Endif
            If(SIMD.EQ.0) Then
               MDATA(MONTH)=TDATA2-TDATA1
            Else
               PLOTD(MONTH,MPLOT)=TDATA2-TDATA1                             !day
            Endif
         Endif
      Elseif(CD.EQ.'2CPI'.or.CD.EQ.'6CPI') Then
         If(OUTFORM.EQ.1) Then
            Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,4),TDATA1,
     +                     (XO(I),I=6,8),TDATA2,XO(10),XO(11),TDATA3
         Else
            Read(UO,K1,REC=RECD) TDATA1,TDATA2,TDATA3
         Endif
         If(SIMD.EQ.0) Then
            MDATA(MONTH)=TDATA2+TDATA1-TDATA3
         Else
            PLOTD(MONTH,MPLOT)=TDATA2+TDATA1-TDATA3                         !day
         Endif
      Elseif(CD.EQ.'2RSD'.or.CD.EQ.'6RSD') Then
         If(OUTFORM.EQ.1) Then
            Read(UO,REC=RECD) C1,(XO(I),I=1,3),TDATA1,
     +                     (XO(I),I=5,11),TDATA2
         Else
            Read(UO,K1,REC=RECD) TDATA1,TDATA2
         Endif
         If(SIMD.EQ.0) Then
            MDATA(MONTH)=TDATA2-TDATA1
         Else
            PLOTD(MONTH,MPLOT)=TDATA2-TDATA1                               !day
         Endif
      Else
         If(ID.EQ.3) Then
            RECD=REC1+PERIOD*CREC-CREC
            K=NWROUT
            If(SIMD.EQ.1) K=NWROUT2                                         !day
            Do 1120 J=1,K
               If(OUTFORM.EQ.1) Then
                  Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,7),
     +                              C4,GP1,GP2,XO(8),XO(9)
                  TDATA=XO(KO)
               Else
                  Read(UO,K2,REC=RECD) TDATA,GP1,GP2
               Endif
               RECD = RECD + 1
               If(GP1.NE.IDEN8(LOOP).and.GP2.NE.IDEN8(LOOP)) Goto 1120
               If(SIMD.EQ.0) Then
                  MDATA(MONTH) = MDATA(MONTH) + TDATA
               Else
                  PLOTD(MONTH,MPLOT)=PLOTD(MONTH,MPLOT)+TDATA               !day
               Endif
1120        End Do
         Else
            If(OUTFORM.EQ.1) Then
               If(ID.EQ.1) Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,7),
     +                                       C4,GP1,GP2,XO(8),XO(9)
               If(ID.EQ.0) Read(UO,REC=RECD) C1,(XO(I),I=1,12)
               If(ID.EQ.2) Read(UO,REC=RECD) C1,(XO(I),I=1,11)
               If(SIMD.EQ.0) Then
                  MDATA(MONTH)=XO(KO)
               Else
                  If(PT.GE.4) MPLOT=1  !Change back to 0 after DSS written  !day
                  PLOTD(MONTH,MPLOT)=XO(KO)                                 !day
               Endif
            Else
               If(SIMD.EQ.0) Then
                  Read(UO,K1,REC=RECD) MDATA(MONTH)
               Else
                  If(PT.GE.4) MPLOT=1  !Change back to 0 after DSS written  !day
                  Read(UO,K1,REC=RECD) PLOTD(MONTH,MPLOT)                   !day
               Endif
            Endif
         Endif
      Endif
      If(SIMD.EQ.0) Then
         MDATA(MONTH)=(MDATA(MONTH)*XF)+AF
      Else
         PLOTD(MONTH,MPLOT)=(PLOTD(MONTH,MPLOT)*XF+AF)                     !day
      Endif
!
!   The monthly data MDATA(MONTH) are converted to TIME-month
!   moving averages or totals if MAT is greater than zero.
!
      If(MAT.GT.0.and.SIMD.EQ.0) Then
         MAM=MAM+1
         MX(MAM)=MDATA(MONTH)
         MAN=TIME
         If(MAM.LT.TIME) MAN=MAM
         MDATA(MONTH)=0.0
         Do I=1,MAN
            J=MAM+1-I
            MDATA(MONTH)=MDATA(MONTH)+MX(J)
         End Do
         If(MAT.EQ.1) MDATA(MONTH)=MDATA(MONTH)/Real(MAN)
      Endif
!
!   Annual totals.
!
      If(SIMD.EQ.0) Then
         YTOTAL = YTOTAL + MDATA(MONTH)
      Else
         YTOTAL=YTOTAL+PLOTD(MONTH,MPLOT)                                   !day
      Endif
!
!   The sub-monthly data PLOTD(MONTH,MPLOT) are converted to TIME-month
!   moving averages or totals if MAT is greater than zero.
!
      If(MAT.GT.0.and.SIMD.EQ.1) Then                                       !day
         MAM=MAM+1                                                          !day
         DX(MAM)=PLOTD(MONTH,MPLOT)                                         !day
         MAN=TIME                                                           !day
         If(MAM.LT.TIME) MAN=MAM                                            !day
         PLOTD(MONTH,MPLOT)=0.0                                             !day
         Do I=1,MAN                                                         !day
            J=MAM+1-I                                                       !day
            PLOTD(MONTH,MPLOT)=PLOTD(MONTH,MPLOT)+DX(J)                     !day
         End Do                                                             !day
         If(MAT.EQ.1) PLOTD(MONTH,MPLOT)=PLOTD(MONTH,MPLOT)/Real(MAN)       !day
      Endif                                                                 !day
!
!   Values for DSS file.
!
      If(PT.EQ.4) Then
         NDSS=NDSS+1
         If(SIMD.EQ.0) Then                                                 !day
            VALUES(NDSS)=MDATA(MONTH)
         Else                                                               !day
            VALUES(NDSS)=PLOTD(MONTH,MPLOT)                                 !day
         Endif                                                              !day
      Endif
      If(PT.EQ.5) Then
         If(MONTH.EQ.NPRDS) Then
            NDSS=NDSS+1
            If(CD.EQ.'2STO') Then
               VALUES(NDSS)=YTOTAL/NPRDS
            Else
               VALUES(NDSS)=YTOTAL
            Endif
         Endif
      Endif
!
!   Write a row in regular table.
!
      If(MONTH.EQ.NPRDS.and.SIMD.EQ.0) Then                                 !day
         If(TA.GE.1) Then
            If(CR1.GT.0) Then
               RTIME=SEQ
            Else
               RTIME=YEAR
            Endif
            If(CD.EQ.'2STO'.or.CD.EQ.'2WSE') YTOTAL=YTOTAL/NPRDS
            If(DP.EQ.0) Then
               Write(2,1130) RTIME, (MDATA(I),I=1,12), YTOTAL
1130           Format(I4,3X,12F9.0,F12.0)
            Elseif(DP.EQ.1) Then
               Write(2,1140) RTIME, (MDATA(I),I=1,12), YTOTAL
1140           Format(I4,3X,12F9.1,F12.1)
            Elseif(DP.EQ.2) Then
               Write(2,1150) RTIME, (MDATA(I),I=1,12), YTOTAL
1150           Format(I4,3X,12F9.2,F12.2)
            Elseif(DP.EQ.3) Then
               Write(2,1160) RTIME, (MDATA(I),I=1,12), YTOTAL
1160           Format(I4,3X,12F9.3,F12.3)
            Elseif(DP.EQ.4) Then
               Write(2,1170) RTIME, (MDATA(I),I=1,12), YTOTAL
1170           Format(I4,3X,12F9.4,F12.4)
            Endif
         Endif
!
!   Develop 12 months (a year) of a column of plot table array.
!
         If(PT.EQ.1) Then
            Do I=1,NPRDS
                IP=PERIOD-NPRDS+I
                PLOT(IP,MPLOT)=MDATA(I)
            End Do
         Endif
         If(PT.EQ.2) Then
            MYR=YEAR-YRST+1
            PLOT(MYR,MPLOT)=YTOTAL
         Endif
!
!   Summations for each month (January-December) and year for use
!   in computing means.
!
         Do I=1,NPRDS
            SUM(I) = SUM(I) + MDATA(I)
         End Do
         SUM(13) = SUM(13) + YTOTAL
         YTOTAL = 0.0
         MONTH = 0
         YEAR = YEAR + 1
         SEQ = SEQ + 1
      Endif
!
!   Repeat cycle for the next month by returning to statement 1100.
!
      RECD = RECD + CREC
      If(PERIOD.LT.N) Goto 1100
!
!   Compute means for each month (January-December) and year if
!   monthly/annual data are finished.
!
      If(NUM.EQ.0.and.ID.NE.3) REC1 = REC1 + 1
      If(SIMD.EQ.0) Then                                                    !day
         Do I = 1,NPRDS
            MEAN(I) = SUM(I) / NYRS
         End Do
         MEAN(13) = SUM(13) / NYRS
      Endif                                                                 !day
!
!   Place means as last row of regular table.
!
      If(TA.GE.1) Then
         If(DP.EQ.0) Then
            Write(2,1180) (MEAN(I),I=1,13)
1180        Format('MEAN',3X,12F9.0,F12.0)
         Elseif(DP.EQ.1) Then
            Write(2,1190) (MEAN(I),I=1,13)
1190        Format('MEAN',3X,12F9.1,F12.1)
         Elseif(DP.EQ.2) Then
            Write(2,1200) (MEAN(I),I=1,13)
1200        Format('MEAN',3X,12F9.2,F12.2)
         Elseif(DP.EQ.3) Then
            Write(2,1210) (MEAN(I),I=1,13)
1210        Format('MEAN',3X,12F9.3,F12.3)
         Elseif(DP.EQ.4) Then
            Write(2,1220) (MEAN(I),I=1,13)
1220        Format('MEAN',3X,12F9.4,F12.4)
         Endif
         Write(2,470)
      Endif
!
!   Place means in plot array.
!
      If(PT.EQ.3.and.SIMD.EQ.0) Then                                        !day
         Do I=1,NPRDS
            PLOT(I,MPLOT)=MEAN(I)
         End Do
      Endif
!
!   Table of means for entire simulation period.
!
      If(PT.EQ.6.and.SIMD.EQ.0) Then
         If(LOOP.EQ.1) Then
            Write(2,1221) CD
1221        Format(A4)
            XXX=0.0
         Endif
         If(CD.EQ.'2STO'.or.CD.EQ.'2WSE') MEAN(13)=MEAN(13)/NPRDS
         XXX=XXX+MEAN(13)
         If(ID.EQ.0.or.ID.EQ.2) Then
            Write(2,1222) Adjustl(ID6),MEAN(13)
1222        Format(A6,F12.2)
         Elseif(ID.EQ.1) Then
            Write(2,1223) Adjustl(ID16),MEAN(13)
1223        Format(A16,F12.2)
         Elseif(ID.EQ.3) Then
            Write(2,1224) Adjustl(IDEN8(LOOP)),MEAN(13)
1224        Format(A8,F12.2)
         Endif
         If(LOOP.EQ.COUNT) Then
            XXX=XXX/COUNT
            If(ID.EQ.0.or.ID.EQ.2) Then
               Write(2,1225) XXX
1225           Format('Total',F13.2,/)
            Elseif(ID.EQ.1) Then
               Write(2,1226) XXX
1226           Format('Total',8x,F15.2,/)
            Elseif(ID.EQ.3) Then
               Write(2,1227) XXX
1227           Format('Total',F17.2,/)
            Endif
         Endif
      Endif
!
!   DSS data is written to the HEC-DSS file.
!
      If(PT.EQ.4.or.PT.EQ.5) Then
         DSSDAY='01'
         If(SIMD.EQ.0) Then                                                 !day
            CDATE=DSSDAY//M(L)//YRSTDSS
            If(DSSMON.EQ.'   ') Then
               CDATE=DSSDAY//M(L)//YRSTDSS
            Else
               CDATE=DSSDAY//DSSMON//YRSTDSS
            Endif
         Else                                                               !day
            If(DSSMON.EQ.'   ') Then
               CDATE=DSSDAY//M(BEGMON)//YRSTDSS                             !day
            Else
               CDATE=DSSDAY//DSSMON//YRSTDSS                                !day
            Endif
            MPLOT=0                                                         !day
         Endif                                                              !day
         CTIME='0000'
         CUNITS=UNIT
         CTYPE=CD
         IPLAN=0
!
!     DSS pathname /A/B/C/D/E/F/ is defined.
!
         A=OROOT
         If(ID.EQ.0) Then
            B=Adjustr(ID6)
            F=' CP'
         Elseif(ID.EQ.1) Then
            B=ID16
            F=' WR'
         Elseif(ID.EQ.2) Then
            B=Adjustr(ID6)
            F='Res'
         Elseif(ID.EQ.3) Then
            B=Adjustr(IDEN8(LOOP))
            F='WRG'
         Endif
         C=CD
         D=CDATE
         If(PT.EQ.4) Then
            If(SIMD.EQ.0) Then                                              !day
               E='1MON'
            Else                                                            !day
               E='1DAY'                                                     !day
            Endif                                                           !day
         Elseif(PT.EQ.5) Then
            E='1YEAR'
         Endif
!
!     DSS routines are called.
!
*         Call ZPATH(A,B,C,D,E,F,CPATH,NPATH)
*         Call ZCHKPN(CPATH,NPATH,ISTAT)
         If(ISTAT.NE.0) Then
            Write(20,1230) ISTAT,Adjustl(CPATH)
1230        Format(' ERROR: DSS ISTAT error',I3,' occurred',
     +             ' for DSS pathname: ',A80)
            Call ERROR
         Endif
*         Call ZSRTS(IFLTAB,CPATH,CDATE,CTIME,NVALS,VALUES,
*     +              CUNITS,CTYPE,IPLAN,ISTAT)
         If(ISTAT.NE.0) Then
            Write(20,1240) ISTAT
1240        Format(' ERROR: DSS ISTAT error',I3,' occurred',
     +             ' writing data to DSS file.')
            Call ERROR
         Endif
      Endif
!
!   Start over with the next control point, water right, or reservoir.
!
      If(LOOP.LT.COUNT.and.PT.LE.0) Goto 900
      If(LOOP.LT.COUNT.and.PT.GE.1.and.MPLOT.LT.100) Goto 900
!
!   The HEC-DSS file VALUES array is deallocated.
!
      If(PT.EQ.4.or.PT.EQ.5) Then
         Deallocate(VALUES)
      Endif
!
!   Write the plot table for monthly simulation output.                     !day
!
      If(((MPLOT.GE.1.and.MORE.EQ.0).or.MPLOT.EQ.100).and.SIMD.EQ.0)Then    !day
         Call TITLES
         Write(2,1250) (HEAD(I,1),I=1,MPLOT)
1250     Format(/,8X,100(7X,A3))
         Write(2,1260) (Adjustr(HEAD(I,2)),I=1,MPLOT)
1260     Format(8X,100(2X,A8))
         Write(2,1270)
1270     Format('  ')
!
!   The number of digits to the right of the decimal point DP
!   adopted for writing PLOT(MM,I) is set at 4, 3, 2, or 1.
!
         X=0.0
         If(DECIMAL.EQ.' ') Then
            If(PT.EQ.1.or.PT.EQ.3) Then
               Do I=1,MPLOT
                  MM=0
                  Do MYR=1,NYRS
                     Do MT=1,NPRDS
                        MM=MM+1
                        If(PLOT(MM,I).GT.X) X=PLOT(MM,I)
                     End Do
                  End Do
               End Do
               DP=4
               If(X.GE.10.0) DP=3
               If(X.GE.100.0) DP=2
               If(X.GE.1000000.0) DP=1
               If(X.GE.10000000.0) DP=0
            Elseif(PT.EQ.2) Then
               Do I=1,MPLOT
                  MM=0
                  Do MYR=1,NYRS
                     MM=MM+1
                     If(PLOT(MM,I).GT.X) X=PLOT(MM,I)
                  End Do
               End Do
               DP=1
               If(X.GE.10000000.0) DP=0
            Endif
         Endif
!
!   The plot table is written.
!
         YEAR=YRST-1
         MM=0
         If(PT.EQ.1) Then
            Do MYR=1,NYRS
               YEAR=YEAR+1
               Do MT=1,NPRDS
                  MM=MM+1
                  If(DP.EQ.4) Then
                     Write(2,1280) YEAR,MT,(PLOT(MM,NN),NN=1,MPLOT)
1280                 Format(1X,I4,I3,100(F10.4))
                  Elseif(DP.EQ.3) Then
                     Write(2,1290) YEAR,MT,(PLOT(MM,NN),NN=1,MPLOT)
1290                 Format(1X,I4,I3,100(F10.3))
                  Elseif(DP.EQ.2) Then
                     Write(2,1300) YEAR,MT,(PLOT(MM,NN),NN=1,MPLOT)
1300                 Format(1X,I4,I3,100(F10.2))
                  Elseif(DP.EQ.1) Then
                     Write(2,1310) YEAR,MT,(PLOT(MM,NN),NN=1,MPLOT)
1310                 Format(1X,I4,I3,100(F10.1))
                  Else
                     Write(2,1320) YEAR,MT,(PLOT(MM,NN),NN=1,MPLOT)
1320                 Format(1X,I4,I3,100(F10.0))
                  Endif
               End Do
            End Do
         Elseif(PT.EQ.2) Then
            Do MYR=1,NYRS
               YEAR=YEAR+1
               If(DP.EQ.0) Then
                  Write(2,1330) YEAR,(PLOT(MYR,NN),NN=1,MPLOT)
1330              Format(1X,I4,3X,100(F10.0))
               Elseif(DP.EQ.1) Then
                  Write(2,1340) YEAR,(PLOT(MYR,NN),NN=1,MPLOT)
1340              Format(1X,I4,3X,100(F10.1))
               Elseif(DP.EQ.2) Then
                  Write(2,1350) YEAR,(PLOT(MYR,NN),NN=1,MPLOT)
1350              Format(1X,I4,3X,100(F10.2))
               Elseif(DP.EQ.3) Then
                  Write(2,1360) YEAR,(PLOT(MYR,NN),NN=1,MPLOT)
1360              Format(1X,I4,3X,100(F10.3))
               Elseif(DP.EQ.4) Then
                  Write(2,1370) YEAR,(PLOT(MYR,NN),NN=1,MPLOT)
1370              Format(1X,I4,3X,100(F10.4))
               Endif
            End Do
         Elseif(PT.EQ.3) Then
            Do MT=1,NPRDS
               If(DP.EQ.0) Then
                  Write(2,1380) MT,(PLOT(MT,NN),NN=1,MPLOT)
1380              Format(1X,4X,I3,100(F10.0))
               Elseif(DP.EQ.1) Then
                  Write(2,1382) MT,(PLOT(MT,NN),NN=1,MPLOT)
1382              Format(1X,4X,I3,100(F10.1))
               Elseif(DP.EQ.2) Then
                  Write(2,1384) MT,(PLOT(MT,NN),NN=1,MPLOT)
1384              Format(1X,4X,I3,100(F10.2))
               Elseif(DP.EQ.3) Then
                  Write(2,1386) MT,(PLOT(MT,NN),NN=1,MPLOT)
1386              Format(1X,4X,I3,100(F10.3))
               Elseif(DP.EQ.4) Then
                  Write(2,1390) MT,(PLOT(MT,NN),NN=1,MPLOT)
1390              Format(1X,4X,I3,100(F10.4))
               Endif
            End Do
         Endif
         MPLOT=0
         If(LOOP.LT.COUNT) Goto 900
      Endif
!
!  Notes for creating daily time step column plots
!
!  DDATA(1:DAYS,1)=YEAR of the records in the daily output file
!  DDATA(1:DAYS,2)=MONTH
!  DDATA(1:DAYS.3)=DAY
!
!  SCRIPTS is used to store the row subscripts that corresponds to the
!  number of rows in the data arrays DDATA and PLOTD.
!  During computation of the column plots PT=2,3 the global array DDATA
!  is masked for appropriate values.  The array subscripts of the rows
!  rejected by the mask (FORTRAN WHERE function) are given a value of
!  SCRIPTS=-1
!
      If(((MPLOT.GE.1.and.MORE.EQ.0).or.MPLOT.EQ.100).and.SIMD.EQ.1)Then    !day
         Call TITLES                                                        !day
         Write(2,1400) (HEAD(I,1),I=1,MPLOT)                                !day
1400     Format(/,11X,100(7X,A3))                                           !day
         Write(2,1410) (Adjustr(HEAD(I,2)),I=1,MPLOT)                       !day
1410     Format(11X,100(2X,A8))                                             !day
1420     Format('  ')                                                       !day
         If(PT.EQ.1) Then                                                   !day
            Write(2,1420)                                                   !day
            Do I=1,DAYS                                                     !day
               Write(2,1430)(DDATA(I,MM),MM=1,3),                           !day
     +                      (PLOTD(I,NN),NN=1,MPLOT)                        !day
1430           Format(1X,I4,2I3,100F10.1)                                   !day
            End Do                                                          !day
         Elseif(PT.EQ.2) Then                                               !day
            Write(2,1440)                                                   !day
1440        Format(1X,'YEAR','  DAYS','      MEAN')                         !day
            Do YEAR=BEGYR,ENDYR                                             !day
               SCRIPTS=(/(I,I=1,DAYS)/)                                     !day
               Where(.NOT.(DDATA(:,1).EQ.YEAR)) SCRIPTS=-1                  !day
               YEARMEAN=0.0                                                 !day
               Do I=1,MPLOT                                                 !day
                  Do DAY=MinVal(SCRIPTS,SCRIPTS.GT.0),MaxVal(SCRIPTS)       !day
                     YEARMEAN(I)=YEARMEAN(I)+PLOTD(DAY,I)                   !day
                  End Do                                                    !day
               End Do                                                       !day
               DAY=0                                                        !day
               Do I=MinVal(SCRIPTS,SCRIPTS.GT.0),MaxVal(SCRIPTS)            !day
                  DAY=DAY+1                                                 !day
               End Do                                                       !day
               YEARMEAN=YEARMEAN/Float(DAY)                                 !day
               Write(2,1450) YEAR,DAY,(YEARMEAN(I),I=1,MPLOT)               !day
1450           Format(1X,I4,I6,100F10.1)                                    !day
            End Do                                                          !day
         Elseif(PT.EQ.3) Then                                               !day
            Write(2,1460)                                                   !day
1460        Format('MONTH','  DAYS','      MEAN')                           !day
            Do MT=1,12                                                      !day
               SCRIPTS=(/(I,I=1,DAYS)/)                                     !day
               Where(.NOT.(DDATA(:,2).EQ.MT)) SCRIPTS=-1                    !day
               TEMP=Real(SCRIPTS)                                           !day
               Call Sort(DAYS,TEMP,0)                                       !day
               SCRIPTS=Int(TEMP)                                            !day
!
!  Compute means if there is at least 1 value.
!
               If(SCRIPTS(DAYS).GT.0) Then                                  !day
                  DAY=0                                                     !day
                  Do I=1,DAYS                                               !day
                     If(SCRIPTS(I).GT.0) DAY=DAY+1                          !day
                  End Do                                                    !day
                  MONTHMEAN=0.0                                             !day
                  Do I=1,MPLOT                                              !day
                     Do N=0,DAY-1                                           !day
                        MONTHMEAN(I)=MONTHMEAN(I)                           !day
     +                               +PLOTD(SCRIPTS(DAYS-N),I)              !day
                     End Do                                                 !day
                  End Do                                                    !day
                  MONTHMEAN=MONTHMEAN/Float(DAY)                            !day
                  Write(2,1470) MT,DAY,(MONTHMEAN(I),I=1,MPLOT)             !day
1470              Format(1X,I4,I6,100F10.1)                                 !day
               Endif                                                        !day
            End Do                                                          !day
         Endif                                                              !day
         MPLOT=0                                                            !day
         If(LOOP.LT.COUNT) Goto 900                                         !day
      Endif                                                                 !day
!
!  Return to main program from Subroutine SERIES.
!
2000  Return
      End Subroutine SERIES
!
!  ***********************************************************************
!
      Subroutine RELIAB
!
!   *-*-*-*-*-* 2REL RECORD *-*-*-*-*-*
!   Subroutine RELIAB develops a summary table of reliabilities for specified
!   control points, water rights, or hydropower reservoirs. The summary table
!   with period and volume reliabilities is described in the Reference Manual.
!   The shortage metrics table activated by 2REL record field 7 was added in
!   2009 and is described in Chapter 2 of the Supplemental Manual.
!
      Use COMVAR
!
      Real AMT,PERREL,SAMT(7),SYAMT(7),RM(7),RY(7),TOTAMT,TOTSHT,TAMT,
     +     VOLREL,TSHT,SHT,YSHT,YAMT,TA,TS,REL,TAR,TD,X,YAMTA,YSHTA
!
      Integer COUNT,CREC,ID,MCR,MON,LOOP,MONTH,NUM,NZERO,PERIOD,REC1,
     +        RFLAG,SKIP,TOTREP
!
      Integer::DC(7),YEAR,DZERO,MZERO,DVALS,MVALS,YVALS                     !day
      Integer,Allocatable,Dimension(:)::SCRIPTS                             !day
      Real::RD(7)                                                           !day
!
      Integer I,J,M,N,MC(7),YC(7)
!
      Character(len=2) IFC
      Character(len=4) CD
      Character(len=6) WRAPID,CHAR
      Character(len=8) WRID2,WRID3,WRAPIDG
      Character(len=16) WRID1
!
!   Variables used exclusively for calculation of shortage metrics
!   for the shortage metrics table added in 2009.
!
      Real SHTMAX,SHTLOCMAX,SUMLOCMAX,SUMSHT,SUMQUO,LOCPERAVG,LOCSEV,
     +     LOCSUMSHT,LOCSUMAMT
      Integer SHTCONSEC,SHTCONSECMAX,NSEQWSHT,NSHTEVENTS,SEQN,
     +        SUMSHTCONSEC,NCONSECEVENTS,VUL,RMON
      Character(len=16) CHARID
      Character(len=16),Allocatable,Dimension(:)::CPNAME
      Integer,Allocatable,Dimension(:)::CONSECMAXARR
      Real,Allocatable,Dimension(:)::SHTMAXARR,VULNERARR,RESILARR,
     +                               SEVERARR,SUMQUOARR,NFAILARR
!
      M=0
      TA=0.0
      TS=0.0
      RFLAG=0
      TAR=0.0
      N=0
      J=0
!
      If(SIMD.EQ.1) Allocate(SCRIPTS(DAYS))
!
!   SIM (UO=4) or SIMD (UO=10) output file unit number.
!
      If(SIMD.EQ.1) Then
         UO=10
      Else
         UO=4
      Endif
!
!   Output table specifications are read from input file (unit=1).
!
      Read(1,10,IOSTAT=STATUS) CD,MON,RFLAG,ID,NUM,TAR,VUL
10    Format(A4,4I4,F8.0,I4)
!
!   Error checks.
!
      If(STATUS.NE.0) Then
         Write(20,20) CD
20       Format(' ERROR: Fortran IOSTAT error occured reading an',
     *          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(ID.LT.0.or.ID.GT.3) Then
         Write(20,30) ID
30       Format(' ERROR: ID of',I4,' in 2REL field 4 is not valid.')
         Call ERROR
      Endif
      If(MON.LT.0.or.MON.GT.12) Then
         Write(20,40) MON
40       Format(' ERROR: MONTH of',I4,' in 2REL field 5 is not valid.')
         Call ERROR
      Endif
      If(NUM.LT.-80.or.NUM.GT.80) Then
         Write(20,50) NUM
50       Format(' ERROR: NUM of',I4,' in 2REL field 6 is not valid.')
         Call ERROR
      Endif
      If(SIMD.EQ.0) Then                                                    !day
         If(NCPTS.EQ.0.and.ID.EQ.0) Then
            Write(20,60) CD,ID
            Call ERROR
         Elseif(NWROUT.EQ.0.and.(ID.EQ.1.or.ID.EQ.3)) Then
            Write(20,80) CD,ID
            Call ERROR
         Elseif(NREOUT.EQ.0.and.ID.EQ.2) Then
            Write(20,100) CD,ID
            Call ERROR
         Endif
      Else                                                                  !day
         If(NCPO2.EQ.0.and.ID.EQ.0) Then                                    !day
            Write(20,70) CD,ID                                              !day
            Call ERROR                                                      !day
         Elseif(NWROUT2.EQ.0.and.(ID.EQ.1.or.ID.EQ.3)) Then                 !day
            Write(20,90) CD,ID                                              !day
            Call ERROR                                                      !day
         Elseif(NREOUT2.EQ.0.and.ID.EQ.2) Then                              !day
            Write(20,100) CD,ID                                             !day
            Call ERROR                                                      !day
         Endif                                                              !day
      Endif                                                                 !day
60    Format(' ERROR: ',A4,' record has an ID of',I2,' but',
     +       ' there are no control points in the WRAP-SIM OUT file.')
70    Format(' ERROR: ',A4,' record has an ID of',I2,' but',                !day
     +       ' there are no control points in the WRAP-SIMD SUB file.')     !day
80    Format(' ERROR: ',A4,' record has an ID of',I2,' but',
     +       ' there are no water rights in the WRAP-SIM OUT file.')
90    Format(' ERROR: ',A4,' record has an ID of',I2,' but',                !day
     +       ' there are no water rights in the WRAP-SIMD SUB file.')       !day
100   Format(' ERROR: ',A4,' record has an ID of',I2,' but',
     +       ' there are no reservoirs in the WRAP-SIM OUT file.')
110   Format(' ERROR: ',A4,' record has an ID of',I2,' but',                !day
     +       ' there are no reservoirs in the WRAP-SIMD SUB file.')         !day
      If(VUL.GE.1.and.ID.EQ.3) Then
         Write(20,120) MON
120      Format(' ERROR: Shortage metrics table (VUL>0) can not be',
     +          ' contructed for water right groups (ID=3).')
         Call ERROR
      Endif
!
!  If specific month is requested (MON>0) while using Monthly Sequencing
!  Option (CR2=0) or Annual Sequencing Option in which more than 12 months are 
!  written to CRM file (CR3=2), a warning message is printed.
!
      If(CR1.GT.0.and.MON.GT.0.and.(CR2.LE.0.or.CR3.GE.2)) Then
         Write(20,130) CD,MON
130      Format(' WARNING: ',A4,' record with MON equal to',I3,
     +          ' loses meaning with CR2 of 0 or CR3 of 2.')
      Endif
!
!   If NUM is not zero, NUM identifiers are read.
!
      If(NUM.GT.0) Then
         NID=NUM
         TID=ID
         Call IDEN
      Endif
      NUM = Abs(NUM)
!
!   Title records (Subroutine TITLES) and table heading.
!
      Call TITLES
      If(MON.EQ.0) Then
         If(ID.EQ.0) Write(2,700)
         If(ID.EQ.1) Write(2,710)
         If(ID.EQ.2) Write(2,720)
         If(ID.EQ.3) Write(2,730)
      Else
         If(ID.EQ.0) Write(2,740) MON
         If(ID.EQ.1) Write(2,750) MON
         If(ID.EQ.2) Write(2,760) MON
         If(ID.EQ.3) Write(2,770) MON
      Endif
!
!   SIMD SUB file information added to the table headings.                  !day
!
      If(SIMD.EQ.1) Call SIMDHEADER                                         !day
!
!   Conditional reliability information added to table headings.
!
      If(CR1.GT.0.and.CRHEAD.GE.0.and.SIMD.EQ.0) Then                       !day
         If(CRSFF.EQ.0) Write(2,1110)
         If(CRSFF.GT.0) Write(2,1120)
         If(CR2.GT.0) Write(2,1150) CR2
         If(CR2.EQ.0) Write(2,1160) NYRS
         Write(2,1130) CR1
         If(CR4.GT.0.0) Write(2,1140) CR4
         Write(2,*) ' '
      Endif
!
!   Headings for the columns in the table.
!
      If(ID.EQ.1) Then
         If(SIMD.EQ.0) Then                                                 !day
            If(CR1.EQ.0) Then
               Write(2,810)
               Write(2,830)
               Write(2,850)
               Write(2,870) UNIT,UNIT
               Write(2,810)
            Else
               Write(2,810)
               Write(2,1170)
               Write(2,850)
               Write(2,1210) UNIT,UNIT
               Write(2,810)
            Endif
         Else                                                               !day
               Write(2,812)                                                 !day
               Write(2,832)                                                 !day
               Write(2,850)                                                 !day
               Write(2,872) UNIT,UNIT                                       !day
               Write(2,812)                                                 !day
         Endif                                                              !day
      Else
         If(SIMD.EQ.0) Then                                                 !day
            Write(2,800)
         Else                                                               !day
            Write(2,802)                                                    !day
         Endif                                                              !day
         If(ID.EQ.2) Then
            If(SIMD.EQ.0) Then                                              !day
               If(CR1.EQ.0) Then
                  Write(2,880)
                  Write(2,890)
                  Write(2,900)
               Else
                  Write(2,1190)
                  Write(2,890)
                  Write(2,900)
               Endif
            Else                                                            !day
                  Write(2,882)                                              !day
                  Write(2,890)                                              !day
                  Write(2,902)                                              !day
            Endif                                                           !day
         Else
            If(SIMD.EQ.0) Then                                              !day
               If(CR1.EQ.0) Then
                  Write(2,820)
                  Write(2,840)
                  Write(2,860) UNIT,UNIT
               Else
                  Write(2,1170)
                  Write(2,840)
                  Write(2,1200) UNIT,UNIT
               Endif
            Else                                                            !day
                  Write(2,822)                                              !day
                  Write(2,840)                                              !day
                  Write(2,862) UNIT,UNIT                                    !day
            Endif                                                           !day
         Endif
         If(SIMD.EQ.0) Then                                                 !day
            Write(2,800)
         Else                                                               !day
            Write(2,802)                                                    !day
         Endif                                                              !day
      Endif
!
!   Since the program SIM output file (unit=4) is read as a direct
!   access file, record counters are devised for use in locating
!   the records to be read.
!
      COUNT = NUM
      If(SIMD.EQ.0) Then                                                    !day
         CREC = NWROUT + NCPTS + NREOUT
         If(ID.EQ.1) Then
            REC1 = 6
            SKIP = NWROUT
            If(NUM.EQ.0) COUNT = NWROUT
         Elseif (ID.EQ.0) Then
            REC1 = 6 + NWROUT
            SKIP = NCPTS
            If(NUM.EQ.0) COUNT = NCPTS
         Elseif (ID.EQ.2) Then
            REC1 = 6 + NWROUT + NCPTS
            SKIP = NREOUT
            If(NUM.EQ.0) COUNT = NREOUT
         Elseif (ID.EQ.3) Then
            REC1 = 6
            SKIP = NWROUT
         Endif
      Else                                                                  !day
         CREC=NWROUT2+NCPO2+NREOUT2                                         !day
         If(ID.EQ.1) Then                                                   !day
            REC1=7                                                          !day
            SKIP=NWROUT2                                                    !day
            If (NUM.EQ.0) COUNT=NWROUT2                                     !day
         Elseif(ID.EQ.0) Then                                               !day
            REC1=7+NWROUT2                                                  !day
            SKIP=NCPO2                                                      !day
            If (NUM.EQ.0) COUNT=NCPO2                                       !day
         Elseif(ID.EQ.2) Then                                               !day
            REC1=7+NWROUT2+NCPO2                                            !day
            SKIP=NREOUT2                                                    !day
            If (NUM.EQ.0) COUNT=NREOUT2                                     !day
         Elseif(ID.EQ.3) Then                                               !day
            REC1=7                                                          !day
            SKIP=NWROUT2                                                    !day
         Endif                                                              !day
      Endif                                                                 !day
!
!  Optional shortage table arrays are allocated.
!
      If(VUL.NE.0) Then
         Allocate(CPNAME(COUNT),CONSECMAXARR(COUNT),SHTMAXARR(COUNT),
     +            VULNERARR(COUNT),RESILARR(COUNT),SEVERARR(COUNT),
     +            SUMQUOARR(COUNT),NFAILARR(COUNT))
      Endif
!
!   ++++++++++ Begin Outer "COUNT" Loop ++++++++++
!   Beginning of loop to develop tables for the COUNT control points,
!   water rights, or reservoirs.  The loop extends from statement 200
!   to statement 480.
!
      LOOP = 0
200   LOOP = LOOP + 1
!
!   Find the record of the SIM output file (unit=4) from which to read
!   the permitted diversion and shortage for the first period (month).
! 
210   RECD = REC1
      If(ID.EQ.1.or.ID.EQ.3) Then
         If(OUTFORM.EQ.1) Then
            Read(UO,REC=RECD) IO1
            If(IO1.EQ.-1) Then
               IFC='IF'
               Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,7),WRID1
            Else
               IFC='  '
               Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,7),WRID1,WRID2,WRID3
            Endif
         Else
            Read(UO,220,REC=RECD) IFC,WRID1,WRID2,WRID3
220         Format(A2,81x,A16,2A8)
         Endif
         If(IFC.EQ.'IF') Then
            REC1=REC1+1
            If(NUM.EQ.0) COUNT=COUNT-1
            Goto 210
         Endif
      Else
         If(OUTFORM.EQ.1) Then
            Read(UO,REC=RECD) WRAPID
         Else
            Read(UO,230,REC=RECD) WRAPID
230         Format(A6)
         Endif
      Endif
      N = 1
      If(NUM.EQ.0) Goto 330
!
!   Check existence of control point, reservoir, water right,
!   or water right group record with specified identifier.
!
      If(ID.EQ.0) Then
240      If(WRAPID.NE.IDCP(LOOP)) Then
            RECD = RECD + 1
            N = N + 1
            If(OUTFORM.EQ.1) Then
               Read(UO,REC=RECD) WRAPID
            Else
               Read(UO,230,REC=RECD) WRAPID
            Endif
            If(N.LE.SKIP+1) Goto 240
            Print*
            Write(20,250) IDCP(LOOP), CD
250         Format(' ERROR: Identifier ',A6,' from ',A4,' record',
     +             ' was not found in SIM/SIMD output file.')               !day
            Call ERROR
         Endif
      Elseif(ID.EQ.2) Then
260      If(WRAPID.NE.IDRES(LOOP)) Then
            RECD = RECD + 1
            N = N + 1
            If(OUTFORM.EQ.1) Then
               Read(UO,REC=RECD) WRAPID
            Else
               Read(UO,230,REC=RECD) WRAPID
            Endif
            If(N.LE.SKIP+1) Goto 260
            Write(20,*) ' '
            Write(20,250) IDRES(LOOP), CD
            Call ERROR
         Endif
      Elseif(ID.EQ.1) Then
270      If(WRID1.NE.IDEN16(LOOP)) Then
            RECD = RECD + 1
            N = N + 1
            If(OUTFORM.EQ.1) Then
               Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,7),WRID1
            Else
               Read(UO,280,REC=RECD) WRID1
280            Format(83x,A16)
            Endif
            If(N.LE.NWROUT) Goto 270
            Write(20,*)
            Write(20,290) IDEN16(LOOP), CD
290         Format(' ERROR: Identifier ',A16,' from ',A4,' record',
     +             ' was not found in SIM/SIMD output file.')               !day
            Call ERROR
         Endif
      Elseif(ID.EQ.3) Then
         WRAPIDG=IDEN8(LOOP)
300      If(WRID2.NE.IDEN8(LOOP).and.WRID3.NE.IDEN8(LOOP)) Then
            RECD = RECD + 1
            N = N + 1
            If(OUTFORM.EQ.1) Then
               Read(UO,REC=RECD) IO1,IO2,(XO(I),I=1,7),C4,WRID2,WRID3
            Else
               Read(UO,310,REC=RECD) WRID2,WRID3
            Endif
310         Format(99x,2A8)
            If(N.LE.NWROUT) Goto 300
            Write(20,*) ' '
            Write(20,320) IDEN8(LOOP), CD
320         Format(' ERROR: Identifier ',A8,' from ',A4,' record',
     +             ' was not found in SIM/SIMD output file.')               !day
            Call ERROR
         Endif
      Endif
!
!   Initialize variables.
!
330   If(SIMD.EQ.0) N=MONTHS                                                !day
      If(SIMD.EQ.1) N=DAYS                                                  !day
      PERIOD = 0
      MONTH = 0
      NZERO=0
      If(NUM.EQ.0) SKIP = SKIP-RECD+REC1
      SHT = 0.0
      AMT = 0.0
      TSHT=0.0
      TAMT=0.0
      YSHT = 0.0
      YAMT = 0.0
      TOTSHT = 0.0
      TOTAMT = 0.0
      YSHTA=0.0
      YAMTA=0.0
      Do I=1,7
         MC(I) = 0
         YC(I) = 0
      End Do
      J=0
!
!   Variables used exclusively for calculation of shortage metrics.
!
      SHTMAX = 0.0
      SHTCONSECMAX = 0
      SEQN=0
      NSEQWSHT=0
      NSHTEVENTS=0
      LOCPERAVG=0.0
      LOCSEV=0.0
      LOCSUMSHT=0.0
      LOCSUMAMT=0.0
      RMON=0
      SUMSHTCONSEC=0
      NCONSECEVENTS=0
      SUMQUO=0.0
      SUMSHT=0.0
      SHTCONSEC = 0
      SUMLOCMAX=0
!
!   ++++++++++ Begin Inner Loop ++++++++++
!   Begin loop which is repeated for each of N=NYRS*NPRDS periods (months).
!
      M=1
340   PERIOD = PERIOD+1
      MONTH = MONTH+1
!
!   Calculation of optional shortage metrics.
!
      If(VUL.GT.0) Then
         If(MONTH.EQ.1) Then
            SEQN=SEQN+1
            SHTLOCMAX=0
            LOCSUMSHT=0
            LOCSUMAMT=0
            If(CR1.GT.0)Then
               SUMSHT=0
               SHTCONSEC=0
               NSHTEVENTS=0
               SUMSHTCONSEC=0
            Endif
         Endif
!
!        Read in Shortage Data
!
         If(ID.EQ.0.or.ID.EQ.2)Then
            Read(UO,350,REC=RECD) CHARID,SHT,AMT
350         Format(A6,2F11.0)
         Elseif(ID.EQ.1) Then
            Read(UO,355,REC=RECD) SHT,AMT,CHARID
355         Format(6x,2F11.0,55x,A16)
         Endif
!
         If(MON.GT.0) Then
            If(CR2.GT.0) RMON=CR2-1
            If(MONTH.NE.MON-RMON) SHT=0
         Endif
         If(SHT.LE.0.001) SHT=0
         If(SHT.GT.SHTMAX) SHTMAX = SHT
         If(CRSFF.GT.0) Then
            SHT = SHT*EXPP(SEQN)*1000000
            AMT = AMT*EXPP(SEQN)*1000000
         Endif
         LOCSUMAMT=LOCSUMAMT+AMT
         If(LOCSUMAMT.LE.0.0) LOCSUMAMT=1.0
         SUMSHT=SUMSHT+SHT
         LOCSUMSHT=LOCSUMSHT+SHT
         If(SHT.LE.0) Then
            SUMSHTCONSEC=SUMSHTCONSEC+SHTCONSEC
            SHTCONSEC=0
         Else
            SHTCONSEC=SHTCONSEC+1
            If(SHTCONSEC.EQ.1) Then
               If(CRSFF.EQ.0) NConsecEvents=NConsecEvents+1
               NShtEvents=NShtEvents+1
            Else
               If(MONTH.EQ.1.and.CR1.EQ.0) NShtEvents=NShtEvents+1
            Endif
            If(SHT.GT.SHTLOCMAX) SHTLOCMAX=SHT
         Endif
         If(SHTCONSEC.GT.SHTCONSECMAX) SHTCONSECMAX=SHTCONSEC
         If(MONTH.EQ.NPRDS) Then
            SUMLOCMAX=SUMLOCMAX+SHTLOCMAX
            SUMQUO=SUMQUO+(LOCSUMSHT/LOCSUMAMT)**2
            If(CR1.GT.0) Then
               If(CRSFF.EQ.0) Then
                  SUMSHTCONSEC=SUMSHTCONSEC+SHTCONSEC
               Else
                  NConsecEvents=NConsecEvents+Int(NShtEvents*EXPP(SEQN)
     +               *1000000)
                SUMSHTCONSEC=(SUMSHTCONSEC+SHTCONSEC)*EXPP(SEQN)*1000000
               Endif
            Endif
            If(NShtEvents.GT.0)Then
               LocPerAvg=LocPerAvg+SUMSHTCONSEC*1.0/NShtEvents
               LOCSEV=LOCSEV+SUMSHT/NShtEvents
            Endif
            If(SHTLOCMAX.GT.0.0) Then
               If(CRSFF.EQ.0) Then
                  NSeqwSht=NSeqwSht+1
               Else
                  NSeqwSht=NSeqwSht+EXPP(SEQN)*1000000
               Endif
            Endif
!
!           Shortage metrics are calculated and stored in arrays.
!
            If(SEQN.EQ.NYRS)Then
               If(CR1.EQ.0)SUMSHTCONSEC=SUMSHTCONSEC+SHTCONSEC
               CPNAME(LOOP)=CHARID
               ConsecMaxArr(LOOP)=SHTCONSECMAX
               SHTMAXARR(LOOP)=SHTMAX
               If(NSeqwSht.EQ.0) Then
                  RESILARR(LOOP)=-9999.999
                  SUMQUOARR(LOOP)=-9999.99
                  VULNERARR(LOOP)=0.0
                  SEVERARR(LOOP)=0.0
                  NFAILARR(LOOP)=0.0
               Else
                  SUMQUOARR(LOOP)=SUMQUO*100/(NYRS)
                  VULNERARR(LOOP)=SUMLOCMAX/NSeqwSht
                  If(CR1.GT.0)Then
                     RESILARR(LOOP)=1/(LocPerAvg/NSeqwSht)
                     SEVERARR(LOOP)=LOCSEV/NSeqwSht
                     If(CRSFF.EQ.0) Then
                        NFAILARR(LOOP)=NConsecEvents*1.0/NYRS
                     Else
                        NFAILARR(LOOP)=NConsecEvents*1.0/1000000
                     Endif
                  Else
                     RESILARR(LOOP)=1/(SUMSHTCONSEC*1.0/NConsecEvents)
                     SEVERARR(LOOP)=SUMSHT/NConsecEvents
                     NFAILARR(LOOP)=NShtEvents*1.0/NYRS
                  Endif
               Endif
            Endif
         Endif
      Endif
!
!   End of optional shortage metric table routine.
!   Return to development of standard reliability table.
!
      If(ID.EQ.3) Then
         RECD = REC1+PERIOD*CREC-CREC
         Do I=1,NWROUT
            If(OUTFORM.EQ.1) Then
               Read(UO,REC=RECD) IO1,IO2,TSHT,TAMT,(XO(J),J=3,7),C4,
     +                           WRID2,WRID3
            Else
               Read(UO,360,REC=RECD) TSHT,TAMT,WRID2,WRID3
360            Format(6x,2F11.0,71x,2A8)
            Endif
            RECD = RECD+1
            If(WRID2.NE.IDEN8(LOOP).and.WRID3.NE.IDEN8(LOOP)) Goto 370
            If(TSHT.LT.0.0) TSHT=0.0
            AMT = AMT+TAMT
            SHT = SHT+TSHT
370      End Do
      Elseif(ID.EQ.1) Then
         If(OUTFORM.EQ.1) Then
            Read(UO,REC=RECD) IO1,IO2,SHT,AMT
         Else
            Read(UO,380,REC=RECD) SHT, AMT
380         Format(6X,2F11.0)
         Endif
      Elseif(ID.EQ.0.or.ID.EQ.2) Then
         If(MONTH.LT.NPRDS) Then
            If(OUTFORM.EQ.1) Then
               Read(UO,REC=RECD) C1,SHT,AMT
            Else
               Read(UO,380,REC=RECD) SHT,AMT
            Endif
         Else
            If(OUTFORM.EQ.1) Then
               Read(UO,REC=RECD) CHAR,SHT,AMT
            Else
               Read(UO,390,REC=RECD) CHAR,SHT,AMT
390            Format(A6,2F11.0)
            Endif
            If(CHAR.NE.WRAPID) Then
               Write(20,*) ' '
               Write(20,400) CHAR, PERIOD, WRAPID
400            Format(' ERROR: Identifier of ',A6, ' read from output',
     +                ' file in period ',I5,' does not match ID of ',A6)
               Call ERROR
            Endif
         Endif
      Endif
      If(ID.EQ.2) Then
         AMT = AMT+SHT
         If(SHT.LT.0.0) SHT = 0.0
      Else
         If(SHT.LT.0.0) SHT = 0.0
      Endif
!
      If(SIMD.EQ.1) PLOTD(PERIOD,1:2)=[AMT,SHT]                             !day
      If(SIMD.EQ.1) Goto 405                                                !day
!
!   Monthly period reliability data.
!
      J=J+1
      MCR=J
      If(CR1.GT.0) Then
         MCR=J+CR2-1
         If(MCR.GT.12) MCR=MCR-12
      Endif
      If(MON.EQ.0.or.MCR.EQ.MON) Then
         If(ABS(AMT).LT.0.000001) NZERO=NZERO+1
         SAMT(1) = 0.00001*AMT
         SAMT(2) = 0.05*AMT
         SAMT(3) = 0.10*AMT
         SAMT(4) = 0.25*AMT
         SAMT(5) = 0.50*AMT
         SAMT(6) = 0.75*AMT
         SAMT(7) = 0.99*AMT
         Do I=1,7
            If(CRSFF.EQ.0) Then
               If(SHT.GT.SAMT(I).and.AMT.GT.0) MC(I) = MC(I)+1
            Else
               X=EXPP(M)*1000000.
               If(SHT.GT.SAMT(I).and.AMT.GT.0) MC(I) = MC(I)+INT(X)
            Endif
         End Do
         If(CRSFF.EQ.0) Then
            YSHT = YSHT+SHT
            YAMT = YAMT+AMT
         Else
            YSHT = YSHT+SHT*EXPP(M)
            YAMT = YAMT+AMT*EXPP(M)
         Endif
      Endif
!
!   Yearly period reliability data.
!
      YSHTA = YSHTA+SHT
      YAMTA = YAMTA+AMT
      If(MONTH.EQ.NPRDS) Then
         SYAMT(1) = 0.0001*YAMTA
         SYAMT(2) = 0.02*YAMTA
         SYAMT(3) = 0.05*YAMTA
         SYAMT(4) = 0.10*YAMTA
         SYAMT(5) = 0.25*YAMTA
         SYAMT(6) = 0.50*YAMTA
         SYAMT(7) = 0.99*YAMTA
         Do I=1,7
            If(CRSFF.EQ.0) Then
               If(YSHTA.GT.SYAMT(I)) YC(I) = YC(I)+1
            Else
               X=EXPP(M)*1000000.
               If(YSHTA.GT.SYAMT(I)) YC(I) = YC(I)+INT(X)
            Endif
         End Do
!
!   Totals for monthly volume reliability.
!
         TOTSHT = TOTSHT+YSHT
         TOTAMT = TOTAMT+YAMT
         MONTH = 0
         YSHT = 0.0
         YAMT = 0.0
         YSHTA =0.0
         YAMTA = 0.0
      Endif
!
!   Preparation for repeating inner loop.
!
      If(J.EQ.NPRDS) Then
         J=0
         M=M+1
      Endif
405   RECD = RECD+CREC                                                      !day
      AMT = 0
      SHT = 0
!
!   ++++++++ End of Inner Loop for Periods ++++++++++
!
      If(PERIOD.LT.N) Goto 340
!____________________________________________________________________________day
!
!   Calculations for data from SUB file.                                    !day
!   Because the SUB file does not necessarily start and end with whole years
!   of data, the calculations are easier to do after first reading in all of
!   the data in the "Inner Loop for Periods".
!
      If(SIMD.EQ.1) Then                                                    !day
!
!   DVALS will equal DAYS, unless (MON.NE.0).
!   Therefore, count DVALS valid timesteps iteratively below.
!
         DVALS=0
         MVALS=0
         YVALS=0
         DC=0.0
         MC=0.0
         YC=0.0
         DZERO=0
         MZERO=0
         YSHT = 0.0
         YAMT = 0.0
         TOTSHT = 0.0
         TOTAMT = 0.0
!
!   Daily computations, store in array DC().
!
         Do I=1,DAYS
            If(MON.LE.0.or.DDATA(I,2).EQ.MON) Then
               AMT=PLOTD(I,1)
               SHT=PLOTD(I,2)
               DVALS=DVALS+1
               SAMT(1) = 0.00001*AMT
               SAMT(2) = 0.05*AMT
               SAMT(3) = 0.10*AMT
               SAMT(4) = 0.25*AMT
               SAMT(5) = 0.50*AMT
               SAMT(6) = 0.75*AMT
               SAMT(7) = 0.99*AMT
               If(Abs(AMT).LT.0.000001) DZERO=DZERO+1
               Do J=1,7
                  If (SHT.GT.SAMT(J).and.AMT.GT.0) DC(J) = DC(J)+1
               End Do
            Endif
         End Do
!
         Do YEAR=BEGYR,ENDYR
            Do MONTH=1,12
               AMT=0.0
               SHT=0.0
               SCRIPTS=(/(I,I=1,DAYS)/)
               Where(.NOT.(DDATA(:,2).EQ.MONTH))  SCRIPTS=-1
               Where(.NOT.(DDATA(:,1).EQ.YEAR))   SCRIPTS=-1
               If(MON.NE.0) Then
                  Where(.NOT.(DDATA(:,2).EQ.MON)) SCRIPTS=-1
               Endif
!
!   No need to sort scripts since single month's subscripts are being isolated.
!
!   Monthly computations, store in MC().
!   Perform only if there is at least one value.
!
               If(MaxVal(SCRIPTS).GT.0) Then
                  Do I=MinVal(SCRIPTS,SCRIPTS.GT.0),MaxVal(SCRIPTS)
                     AMT=AMT+PLOTD(I,1)
                     SHT=SHT+PLOTD(I,2)
                  End Do
                  MVALS=MVALS+1
                  SAMT(1) = 0.00001*AMT
                  SAMT(2) = 0.05*AMT
                  SAMT(3) = 0.10*AMT
                  SAMT(4) = 0.25*AMT
                  SAMT(5) = 0.50*AMT
                  SAMT(6) = 0.75*AMT
                  SAMT(7) = 0.99*AMT
                  If(ABS(AMT).LT.0.000001) MZERO=MZERO+1
                  Do J=1,7
                     If (SHT.GT.SAMT(J).and.AMT.GT.0) MC(J) = MC(J)+1
                  End Do
!
                  TOTSHT = TOTSHT+SHT
                  TOTAMT = TOTAMT+AMT
!
                  YSHT = YSHT+SHT
                  YAMT = YAMT+AMT
               Endif
!
!              Yearly computations, store in YC().
!              Only do reliability computations for whole years of data.
!              If this is a whole year then, else if not a whole year then.
!
               If((YEAR.GT.BEGYR.and.YEAR.LT.ENDYR.and.MONTH.EQ.12)
     +         .or.(YEAR.EQ.BEGYR.and.BEGMON.EQ.1.and.MONTH.EQ.12)
     +         .or.(YEAR.EQ.ENDYR.and.MONTH.EQ.12)) Then
                  YVALS=YVALS+1
                  SYAMT(1) = 0.0001*YAMT
                  SYAMT(2) = 0.02*YAMT
                  SYAMT(3) = 0.05*YAMT
                  SYAMT(4) = 0.10*YAMT
                  SYAMT(5) = 0.25*YAMT
                  SYAMT(6) = 0.50*YAMT
                  SYAMT(7) = 0.99*YAMT
                  Do I=1,7
                     If(YSHT.GT.SYAMT(I)) YC(I) = YC(I)+1
                  Enddo
                  YSHT = 0.0
                  YAMT = 0.0
               Elseif((YEAR.EQ.BEGYR.and.BEGMON.GT.1)
     +            .or.(YEAR.EQ.ENDYR.and.ENDMON.LT.12)) Then
                  YSHT = 0.0
                  YAMT = 0.0
               Endif
            End Do
         End Do
      Endif
!____________________________________________________________________________day
!
!   Develop reliablity table.
!
      If(TOTAMT.LE.0) Then
         If(ID.EQ.2) Then
            Write(2,410) Adjustl(WRAPID)
410         Format(A6,9X,'0.0       0.00   There is no hydropower',
     +             ' requirement at this control point.')
         Elseif(ID.EQ.0) Then
            Write(2,420) Adjustl(WRAPID)
420         Format(A6,9X,'0.0       0.00   There are no diversions',
     +             ' at this control point.')
         Elseif(ID.EQ.1) Then
            Write(2,430) Adjustl(WRID1)
430         Format(A16,2X,' This water right has ',
     +             'no diversion target.')
         Elseif(ID.EQ.3) Then
            Write(2,440) Adjustl(WRAPIDG)
440         Format(A8,2X,' This water right group ',
     +             'has zero diversion target.')
         Endif
         Goto 480
      Endif
!
!  If a probability array option is used, the total number of
!  repetitions is counted.
!
      If(CRSFF.GT.0.and.SIMD.EQ.0) Then                                     !day
         TOTREP=0
         M=Size(EXPP)
         Do I=1,M
            TOTREP=INT(EXPP(I)*1000000)+TOTREP
         Enddo
      Endif
!
!  Calculate  reliabilities.
!
      If(SIMD.EQ.0) Then                                                    !day
         If(RFLAG.NE.0) Then
            If(CRSFF.EQ.0) Then
               If(MON.LE.0) Then
                  PERREL = (Real(N-MC(1))/Real(N))*100.
               Else
                  PERREL = (Real((N/NPRDS)-MC(1))/Real(N/NPRDS))*100.
               Endif
            Else
               If(MON.LE.0) Then
                  PERREL = (Real((NPRDS*TOTREP)-MC(1))/
     +                       Real(NPRDS*TOTREP))*100.
               Else
                  PERREL = (Real(TOTREP-MC(1))/Real(TOTREP))*100.
               Endif
            Endif
         Elseif(RFLAG.EQ.0) Then
            If(CRSFF.EQ.0) Then
               If(MON.LE.0) Then
                  PERREL = (Real(N-NZERO-MC(1))/Real(N-NZERO))*100.
               Else
                  PERREL = (Real((N/NPRDS)-NZERO-MC(1))/Real((N/NPRDS)-
     +                      NZERO))*100.
               Endif
            Else
               If(MON.LE.0) Then
                  PERREL = (Real((NPRDS*TOTREP-NZERO)-MC(1))/
     +                      Real(NPRDS*TOTREP-NZERO))*100.
               Else
                  PERREL = (Real(TOTREP-NZERO-MC(1))/Real(TOTREP-NZERO))
     +                     *100.
               Endif
            Endif
         Endif
         If(CRSFF.EQ.0) Then
            VOLREL = ((TOTAMT-Max(0.,TOTSHT))/TOTAMT)*100.
            YAMT = TOTAMT/Real((N/NPRDS))
            YSHT = TOTSHT/Real((N/NPRDS))
            TA=TA+YAMT
            TS=TS+YSHT
         Else
            VOLREL = ((TOTAMT-Max(0.,TOTSHT))/TOTAMT)*100.
            If(MON.LE.0) Then
               YAMT = TOTAMT
               YSHT = TOTSHT
            Else
               YAMT = TOTAMT
               YSHT = TOTSHT
            Endif
            TA=TA+YAMT
            TS=TS+YSHT
         Endif
         If(CRSFF.EQ.0) Then
            If(MON.LE.0) Then
               Do I=1,7
                  RM(I)=(Real(N-NZERO-MC(I))/Real(N-NZERO))*100.
                  RY(I)=(Real((N/NPRDS)-YC(I))/Real((N/NPRDS)))*100.
               End Do
            Else
               Do I=1,7
                  RM(I)=(Real((N/NPRDS)-NZERO-MC(I))/
     +                  Real((N/NPRDS)-NZERO))*100
                  RY(I)=(Real((N/NPRDS)-YC(I))/Real((N/NPRDS)))*100.
               End Do
            Endif
         Else
            If(MON.LE.0) Then
               Do I=1,7
                  RM(I)=(Real(NPRDS*TOTREP-NZERO-MC(I))/
     +               Real(NPRDS*TOTREP-NZERO))*100.
                  RY(I)=(Real(TOTREP-YC(I))/Real(TOTREP))*100.
               Enddo
            Else
               Do I=1,7
                  RM(I)=(Real(TOTREP-NZERO-MC(I))/Real(TOTREP-NZERO
     +               ))*100
                  RY(I)=(Real(TOTREP-YC(I))/Real(TOTREP))*100.
               Enddo
            Endif
         Endif
         If(ID.EQ.0.or.ID.EQ.2) Then
            Write(2,450) Adjustl(WRAPID),YAMT,YSHT,PERREL,VOLREL,
     +                   (RM(I),I=1,7),(RY(I),I=1,7)
450      Format(A6,F12.1,F11.2,F8.2,F7.2,'|',F5.1,6F6.1,'|',F5.1,6F6.1)
         Elseif(ID.EQ.1) Then
            Write(2,460) Adjustl(WRID1),YAMT,YSHT,PERREL,VOLREL,
     +                   (RM(I),I=1,7),(RY(I),I=1,6)
460      Format(A16,F10.1,F11.2,F8.2,F7.2,'|',F5.1,6F6.1,'|',F5.1,5F6.1)
         Elseif(ID.EQ.3) Then
            Write(2,470) Adjustl(WRAPIDG),YAMT,YSHT,PERREL,VOLREL,
     +                   (RM(I),I=1,7),(RY(I),I=1,7)
470       Format(A8,F10.1,F11.2,F8.2,F7.2,'|',F5.1,6F6.1,'|',F5.1,6F6.1)
         Endif
!
      Else                                                                  !day
         If(RFLAG.NE.0) Then        !DVALS=count of valid daily time steps  !day
            PERREL = (Real(DVALS-DC(1))/Real(DVALS))*100.0                  !day
         Elseif(RFLAG.EQ.0) Then                                            !day
            PERREL = (Real(DVALS-DZERO-DC(1))/Real(DVALS-DZERO))*100.0      !day
         Endif                                                              !day
         VOLREL = ((TOTAMT-Max(0.,TOTSHT))/TOTAMT)*100.0                    !day
         YAMT = TOTAMT/(Real(MVALS)/12.0)                                   !day
         YSHT = TOTSHT/(Real(MVALS)/12.0)                                   !day
         TA=TA+YAMT                                                         !day
         TS=TS+YSHT                                                         !day
         RY=0.0                                                             !day
         Do I=1,7                                                           !day
            RD(I)=(Real(DVALS-DZERO-DC(I))/Real(DVALS-DZERO))*100.0         !day
            RM(I)=(Real(MVALS-MZERO-MC(I))/Real(MVALS-MZERO))*100.0         !day
            If(YVALS.GT.0) RY(I)=(Real(YVALS-YC(I))/                        !day
     +                             Real(YVALS))*100.0                       !day
         End Do                                                             !day
         If(ID.EQ.0.or.ID.EQ.2) Then                                        !day
            Write(2,472) WRAPID,YAMT,YSHT,PERREL,VOLREL,(RD(I),I=1,7),      !day
     +                  (RM(I),I=1,7),(RY(I),I=1,7)                         !day
472         Format(A6,F12.1,F11.2,F8.2,F7.2,3('|',F5.1,6F6.1))              !day
         Elseif(ID.EQ.1) Then                                               !day
            Write(2,474) Adjustl(WRID1),YAMT,YSHT,PERREL,VOLREL,            !day
     +                   (RD(I),I=1,7),(RM(I),I=1,7),(RY(I),I=1,6)          !day
474         Format(A16,F10.1,F11.2,F8.2,F7.2,2('|',F5.1,6F6.1),             !day
     +            '|',F5.1,6F6.1)                                           !day
         Elseif(ID.EQ.3) Then                                               !day
            Write(2,476) Adjustl(WRAPIDG),YAMT,YSHT,PERREL,VOLREL,          !day
     +                   (RD(I),I=1,7),(RM(I),I=1,7),(RY(I),I=1,7)          !day
476         Format(A8,F10.1,F11.2,F8.2,F7.2,3('|',F5.1,6F6.1))              !day
         Endif                                                              !day
      Endif                                                                 !day
!
!   ++++++++++ End of Outer "COUNT" Loop ++++++++++
!
480   If(NUM.EQ.0) REC1 = REC1+1
      If(LOOP.LT.COUNT) Goto 200
!
!   Write totals.
!
      If(TS.LT.0) TS=0.0
      REL=0.0
      If(TA.NE.0) REL=((TA-TS)/TA)*100
      If(ID.EQ.1) Then
         Write(2,810)
         Write(2,490) TA, TS, REL
490      Format('Total',9x,F12.1,F11.2,8x,F7.2)
         Write(2,500)
500      Format(52('-'))
      Else
         Write(2,800)
         Write(2,510) TA, TS, REL
510      Format('Total ',F12.1,F11.2,8x,F7.2)
         Write(2,520)
520      Format(44('-'))
      Endif
!
!   Write optional totals table.
!
      If(TAR.NE.0.0) Then
         If(TAR.LT.0.0) TAR=TA
         TD=TA-TS
         TS=TAR-TD
         REL=(TD/TAR)*100
         Write(2,530)
530      Format(3x,'Volume Reliability Summary:')
         Write(2,540) TAR
540      Format(6x,'Annual Diversion Target =     ',F11.2,
     +             '  (2REL record TAR)')
         Write(2,550) TD
550      Format(6x,'Mean Actual Annual Diversion =',F11.2,
     +          '  (Sum of targets minus shortages from table above)')
         Write(2,560) TS
560      Format(6x,'Mean Annual Shortage =        ',F11.2,
     +          '  (Annual diversion target minus actual diversion)')
         Write(2,570) REL
570      Format(6x,'Volume Reliability (percent) =',F11.2,
     +          '  ((actual diversion / diversion target)*100%)')
         Write(2,580)
580      Format(98('-'))
      Endif
!
!   Write optional shortage metrics table.
!
      If(VUL.NE.0) Then
         Write(2,600)
         If(ID.EQ.0) Write(2,610)
         If(ID.EQ.1) Write(2,620)
         If(ID.EQ.2) Write(2,630)
         If(ID.EQ.3) Write(2,640)
         Write(2,680)
         Write(2,650)
         Write(2,660)
         Write(2,670)
         Write(2,680)
         Do I=1,COUNT
            Write(2,590) Adjustl(CPNAME(I)),SHTMAXARR(I),VULNERARR(I),
     +                   RESILARR(I),SEVERARR(I),SUMQUOARR(I),
     +                   NFAILARR(I),ConsecMaxArr(I)
590         Format(A16,F11.2,F13.3,F15.3,F12.2,F12.2,F11.2,9x,I8)
         Enddo
         Write(2,680)
!
600      Format(' ')
610      Format('SHORTAGE METRICS FOR SELECTED CONTROL POINTS')
620      Format('SHORTAGE METRICS FOR SELECTED WATER RIGHTS')
630      Format('SHORTAGE METRICS FOR SELECTED RESERVOIRS')
640      Format('SHORTAGE METRICS FOR SELECTED WATER RIGHTS GROUPS')
650      Format(20x,'MAXIMUM',32x,'AVERAGE',5x,'SHORTAGE',3x,
     +          'MEAN NUMBER',5x,'MAXIMUM NUMBER OF')
660      Format(5x,'NAME',10x,'SHORTAGE  VULNERABILITY   RESILIENCY',
     +          4x,'SEVERITY     INDEX     OF FAILURES   CONSECUTIVE ',
     +          'SHORTAGES')
670      Format(20X,'(AC-FT)    (AC-FT/SQ)    (MONTHS^-1)  ',
     +          '(AC-FT/SQ) (MONTHS^-1)  PER SEQUENCE        (MONTHS)')
680      Format(117('-'))
      Endif
!
!   Format statements for the table headings.
!
700   Format('RELIABILITY SUMMARY FOR SELECTED CONTROL POINTS',/)
710   Format('RELIABILITY SUMMARY FOR SELECTED WATER RIGHTS',/)
720   Format('RELIABILITY SUMMARY FOR SELECTED HYDROELECTRIC POWER',
     *          ' PROJECTS',/)
730   Format('RELIABILITY SUMMARY FOR SELECTED WATER RIGHT GROUPS',/)
!
740   Format('RELIABILITY SUMMARY FOR SELECTED CONTROL POINTS '
     +        'FOR MONTH',I3,/)
750   Format('RELIABILITY SUMMARY FOR SELECTED WATER RIGHTS ',
     +        'FOR MONTH',I3,/)
760   Format('RELIABILITY SUMMARY FOR SELECTED HYDROELECTRIC POWER ',
     +       'PROJECTS FOR MONTH ',I3,/)
770   Format('RELIABILITY SUMMARY FOR SELECTED WATER RIGHT GROUPS ',
     +        'FOR MONTH',I3,/)
!
800   Format(128('-'))
802   Format(170('-'))                                                      !day
810   Format(130('-'))
812   Format(172('-'))                                                      !day
820   Format(10X,'TARGET       MEAN    *RELIABILITY*| ',
     +        '+++++++++ PERCENTAGE OF MONTHS +++++++++|',
     +        '---------- PERCENTAGE OF YEARS ----------')
822   Format(10X,'TARGET       MEAN    *RELIABILITY*| ',                    !day
     +        '~~~~~~~~~ PERCENTAGE OF DAYS ~~~~~~~~~~~|',                  !day
     +        '+++++++++ PERCENTAGE OF MONTHS ++++++++++|',                 !day
     +        '---------- PERCENTAGE OF YEARS ----------')                  !day
830   Format(18X,'TARGET       MEAN    *RELIABILITY*| ',
     +        '+++++++++ PERCENTAGE OF MONTHS +++++++++|',
     +        '------- PERCENTAGE OF YEARS -------')
832   Format(18X,'TARGET       MEAN    *RELIABILITY*| ',                    !day
     +        '~~~~~~~~~ PERCENTAGE OF DAYS ~~~~~~~~~~~|',                  !day
     +        '+++++++++ PERCENTAGE OF MONTHS ++++++++++|',                 !day
     +        '------- PERCENTAGE OF YEARS -------')                        !day
840   Format('NAME     DIVERSION   SHORTAGE  PERIOD VOLUME|',4X,
     +       'WITH DIVERSIONS EQUALING OR EXCEEDING PERCENTAGE OF ',
     +       'TARGET DIVERSION AMOUNT')
850   Format(4x,'NAME         DIVERSION   SHORTAGE  PERIOD VOLUME|',2X,
     +       'WITH DIVERSIONS EQUALING OR EXCEEDING PERCENTAGE OF ',
     +       'TARGET DIVERSION AMOUNT')
860   Format(8X,'(',A5,'/YR)  (',A5,'/YR)   (%)    (%) | 100%   95%',
     +       '   90%   75%   50%   25%   1% | 100%   98%   95%   90%',
     +       '   75%   50%    1%')
862   Format(8X,'(',A5,'/YR)  (',A5,'/YR)   (%)    (%) ',                   !day
     +       2('| 100%   95%   90%   75%   50%   25%   1% '),               !day
     +         '| 100%   98%   95%   90%   75%   50%    1%')                !day
870   Format(16X,'(',A5,'/YR)  (',A5,'/YR)   (%)    (%) | 100%   95%',
     +       '   90%   75%   50%   25%   1% | 100%   98%   95%   90%',
     +       '   75%   50%')
872   Format(16X,'(',A5,'/YR)  (',A5,'/YR)   (%)    (%) ',                  !day
     +       2('| 100%   95%   90%   75%   50%   25%   1% '),               !day
     +         '| 100%   98%   95%   90%   75%   50%')                      !day
880   Format(9X,'  ENERGY     MEAN     *RELIABILITY*  ',
     +        '+++++++++ PERCENTAGE OF MONTHS +++++++++|',
     +        '---------- PERCENTAGE OF YEARS ----------')
882   Format(9X,'  ENERGY     MEAN     *RELIABILITY*  ',                    !day
     +        '~~~~~~~~~ PERCENTAGE OF DAYS ~~~~~~~~~~~|',                  !day
     +        '+++++++++ PERCENTAGE OF MONTHS ++++++++++|',                 !day
     +        '---------- PERCENTAGE OF YEARS ----------')                  !day
890   Format('NAME       TARGET   SHORTAGE   PERIOD AMOUNT',11X,
     +       'WITH ENERGY GENERATION EQUALING OR EXCEEDING PERCENTAGE',
     +       '  OF TARGET')
900   Format(29X,'    (%)    (%)   100%   95%',
     +       '   90%   75%   50%   25%   1% | 100%   98%   95%   90%',
     +       '   75%   50%    1%')
902   FORMAT(29X,'    (%)    (%) ',2('| 100%   95%',                        !day
     +       '   90%   75%   50%   25%   1% '),                             !day
     +       '| 100%   98%   95%   90%   75%   50%    1%')                  !day
!
!   Format statements for CRM table headings.
!
1110  Format('CONDITIONAL RELIABILITY MODELING:',
     +       ' Equal-Weight Option')
1120  Format('CONDITIONAL RELIABILITY MODELING:',
     +       ' Probability Array Option')
1130  Format('Length of simulation period (CR1) is',I3,' months.')
1140  Format('Initial storage multiplier (CR4) =',F7.3)
1150  Format('Annual cycles starting in month',I2)
1160  Format('Monthly cycle option with',I4,' sequences.')
!
!   Format statements for table headings.
!
1170  Format(10X,'TARGET       MEAN    *RELIABILITY*| ',
     +        '+++++++++ PERCENTAGE OF MONTHS +++++++++|',
     +        '-------- PERCENTAGE OF SEQUENCES --------')
1180  Format(18X,'TARGET       MEAN    *RELIABILITY*| ',
     +        '+++++++++ PERCENTAGE OF MONTHS +++++++++|',
     +        '----- PERCENTAGE OF SEQUENCES -----')
1190  Format(9X,'  ENERGY     MEAN     *RELIABILITY*  ',
     +        '+++++++++ PERCENTAGE OF MONTHS +++++++++|',
     +        '-------- PERCENTAGE OF SEQUENCES---------')
1200  Format(8X,'(',A5,'/SQ)  (',A5,'/SQ)   (%)    (%) | 100%   95%',
     +       '   90%   75%   50%   25%   1% | 100%   98%   95%   90%',
     +       '   75%   50%    1%')
1210  Format(16X,'(',A5,'/SQ)  (',A5,'/SQ)   (%)    (%) | 100%   95%',
     +       '   90%   75%   50%   25%   1% | 100%   98%   95%   90%',
     +       '   75%   50%')
!
!  Return to main program from Subroutine RELIAB.
!
      Return
      End Subroutine RELIAB
!
!  ***********************************************************************
!
      Subroutine FREQ
!
!   *-*-*-*-*-*   2FRE, 2FRQ, 6FRE, 6FRQ   *-*-*-*-*-*
!   Subroutine FREQ develops frequency tables for naturalized flow, regulated
!   flow, unappropriated flow, storage volume, water surface elevation, and
!   instream flow shortage.
!
      Use COMVAR
!
      Real F(21),FQ(7),QF(7),MX(MONTHS),QFREQ(21),Z(21)
      Real AF,DXF,FX,MEAN,MEANLOG,STDDEV,SDLOG,SUM,SUMSD,TEMP,TEMP2,X,XF
!
      Integer COUNT,CREC,I,ID,IF1,IF2,IJ,J,K,KKK,L,LOOP,M,NF,MAN,MAT,
     +        MCR,METHOD,MON,MS,N,NM,NP,NUM,REC1,SKIP,STOFLAG,STOFLAG2,
     +        TABLE,TIME,TOTREP
!
      Integer DVALS                                                         !day
!
      Character(len=2)  IFC
      Character(len=4)  CD
      Character(len=6)  WRAPID
      Character(len=16) WRAPID16
      Character(len=40) KK1,KK2
!
      Logical SORTED
!
      Real,Allocatable,Dimension(:)::EXPPT,Q,QT,QTOTAL
      Real,Allocatable,Dimension(:,:)::XQFREQ
!
      Character(len=6),Allocatable,Dimension(:)::XID
      Character(len=16),Allocatable,Dimension(:)::XID16
!
      STOFLAG=0
      STOFLAG2=0
      MAT=0
      XF=1.0
      AF=0.0
!
!   SIM (UO=4) or SIMD (UO=10) output file unit number.
!
      If(SIMD.EQ.1) Then
         UO=10
      Else
         UO=4
      Endif
!
!   Specifications for building the frequency table are read from
!   a 2FRE, 2FRQ, 6FRE, or 6FRQ record in TIN file (unit=1).
!
      Read(1,10,IOSTAT=STATUS) CD,ID,MON,NUM
10    Format(A4,3I4)
!
!   Error checks.
!
      If(STATUS.NE.0) Then
         Write(20,20) CD
20       Format(' ERROR: Fortran IOSTAT error occured reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(MON.LT.0.or.MON.GT.12) Then
         Write(20,30) MON
30       Format(' ERROR: MONTH of',I4,' in field 3 is not valid.')
         Call ERROR
      Endif
      If(ID.EQ.0) Then
         Write(20,40)
40       Format(' ERROR: ID of zero (blank) in field 2 is not valid.')
         Call ERROR
      Endif
      If(ID.GT.8.or.ID.LT.-6) Then
         Write(20,50) ID
50       Format(' ERROR: ID of',I4,' in field 2 is not valid.')
         Call ERROR
      Endif
      If(CD.EQ.'2FRE') Then
         If(NUM.LT.-80.or.NUM.GT.80) Then
            Write(20,60) NUM
60          Format(' ERROR: NUM of',I4,' in 2FRE field 4 is not valid.')
            Call ERROR
         Endif
      Elseif(CD.EQ.'2FRQ') Then
         If(NUM.LT.1.or.NUM.GT.7) Then
            Write(20,70) NUM
70          Format(' ERROR: NM of',I4,' in 2FRQ field 4 is not valid.')
            Call ERROR
         Endif
      Endif
      If(CD.EQ.'6FRE') Then                                                 !day
         If(NUM.LT.-80.or.NUM.GT.80) Then                                   !day
            Write(20,80) NUM                                                !day
80          Format(' ERROR: NUM of',I4,' in 6FRE field 4 is not valid.')    !day
            Call ERROR                                                      !day
         Endif                                                              !day
      Elseif(CD.EQ.'6FRQ') Then                                             !day
         If(NUM.LT.1.or.NUM.GT.7) Then                                      !day
            Write(20,90) NUM                                                !day
90          Format(' ERROR: NM of',I4,' in 6FRQ field 4 is not valid.')     !day
            Call ERROR                                                      !day
         Endif                                                              !day
      Endif                                                                 !day
      If(SIMD.EQ.0) Then                                                    !day
         If(Abs(ID).LE.4.and.NCPTS.EQ.0) Then
            Write(20,*) 'ERROR: There are no control point records in ',
     +                  'SIM output file.'
            Call ERROR
         Endif
         If((Abs(ID).EQ.5.or.ID.EQ.8).and.NWROUT.EQ.0) Then
            Write(20,*) 'ERROR: There are no water right records in ',
     +                  'SIM output file.'
            Call ERROR
         Endif
         If((Abs(ID).EQ.6.or.ID.EQ.7).and.NREOUT.EQ.0) Then
            Write(20,*) 'ERROR: There are no reservoir records in ',
     +                  'SIM output file.'
            Call ERROR
         Endif
      Else                                                                  !day
         If(Abs(ID).LE.4.and.NCPO2.EQ.0) Then                               !day
            Write(20,*) 'ERROR: There are no control point records in ',    !day
     +                  'SIMD output file.'                                 !day
            Call ERROR                                                      !day
         Endif                                                              !day
         If((Abs(ID).EQ.5.or.ID.EQ.6).and.NWROUT2.EQ.0) Then                !day
            Write(20,*) 'ERROR: There are no water right records in ',      !day
     +                  'SIMD output file.'                                 !day
            Call ERROR                                                      !day
         Endif                                                              !day
         If((Abs(ID).EQ.6.or.ID.EQ.7).and.NREOUT2.EQ.0) Then                !day
            Write(20,*) 'ERROR: There are no reservoir records in ',        !day
     +                  'SIMD output file.'                                 !day
         Endif
      Endif                                                                 !day
!
!   The remainder of the 2FRE, 6FRE, 2FRQ, or 6FRQ record is read.
!
      Backspace(1)
!
!   2FRE or 6FRE record is read.
!
      If(CD.EQ.'2FRE'.or.CD.EQ.'6FRE') Then
         Read(1,100,IOSTAT=STATUS) CD,ID,MON,NUM,TABLE,METHOD,
     +                             MAT,TIME,XF,AF
100      Format(A4,7I4,2F8.0)
         If(Abs(XF).LT.0.00001) XF=1.0
         If(Abs(AF).LT.0.00001) AF=0.0
!
!        Error checks.
!
         If(STATUS.NE.0) Then
            Write(20,20) CD
            Call ERROR
         Endif
         If(TABLE.LT.0.or.TABLE.GT.4) Then
            Write(20,110) TABLE
110         Format(' ERROR: TABLE of',I4,' is not valid.')
            Call ERROR
         Endif
         If(METHOD.LT.0.or.METHOD.GT.3) Then
            Write(20,120) METHOD
120         Format(' ERROR: METHOD of',I4,' is not valid.')
            Call ERROR
         Endif
         If(MAT.LT.0.or.MAT.GT.2) Then
            Write(20,130) MAT
130         Format(' ERROR: MAT of',I4,' is not valid.')
            Call ERROR
         Endif
         If(TIME.LT.0.or.TIME.GT.50000) Then
            Write(20,140) TIME
140         Format(' ERROR: TIME of',I4,' is not valid.')
            Call ERROR
         Endif
         If(MAT.GT.0.and.MON.GT.0) Then
            Write(20,150)
150         Format(' Warning: Non-zero MAT in 2FRE field 7 can not be',
     +             ' used with nonzero MON in field 3.')
            Return
         Endif
         If(MAT.GT.0.and.ID.LT.0) Then
            Write(20,160)
160         Format(' Warning: Non-zero MAT in 2FRE field 7 can not be',
     +             ' used with options -4, -5, or -6 in field 2.')
            Return
         Endif
!
!        Variable identifier ID.
!
         If(ID.EQ.4.or.ID.EQ.5.or.ID.EQ.6) STOFLAG2=9
         If(ID.EQ.-4.or.ID.EQ.-5.or.ID.EQ.-6) Then
            ID=ABS(ID)
            STOFLAG=9
            STOFLAG2=-9
         Endif
         If(NUM.GT.0) Then
            NID=NUM
            If(ID.LE.4) TID=0
            If(ID.EQ.5.or.ID.EQ.8) TID=1
            If(ID.EQ.6.or.ID.EQ.7) TID=2
            Call IDEN
         Endif
!
!   2FRQ or 6FRQ record is read.
!
      Elseif(CD.EQ.'2FRQ'.or.CD.EQ.'6FRQ') Then                             !day
         If(ID.LE.4) Read(1,170) CD,ID,MON,NM,IDCP(1),(QF(I),I=1,NM)
         If(ID.EQ.5.or.ID.EQ.8) Read(1,180) CD,ID,MON,NM,IDEN16(1),
     +                                      (QF(I),I=1,NM)
         If(ID.EQ.6.or.ID.EQ.7) Read(1,170) CD,ID,MON,NM,IDRES(1),
     +                                      (QF(I),I=1,NM)
170      Format(A4,3I4,2x,A6,7F8.0)
180      Format(A4,3I4,A16,7F8.0)
         IDCP(1)=Adjustr(IDCP(1))
         IDEN16(1)=Adjustr(IDEN16(1))
         IDRES(1)=Adjustr(IDRES(1))
         NUM=1
         COUNT=1
      Endif
      NUM=Abs(NUM)
!
!  If specific month is requested (MON>0) while using Monthly Sequencing
!  Option (CR2=0) or Annual Sequencing Option in which more than 12 months are 
!  written to CRM file (CR3=2), a warning message is printed.
!
      If(CR1.GT.0.and.MON.GT.0.and.CR2.LE.0.and.CR3.LE.1) Then
         Write(20,184) CD,MON
184      Format(' WARNING: ',A4,' record with MON equal to',I3,
     +          ' loses meaning with CR2 of 0.')
      Endif
      If(CR1.GT.0.and.MON.GT.0.and.CR3.GE.2) Then
         Write(20,186) CD
186      Format(' ERROR: ',A4,' record with MON greater than 0',
     +          ' is invalid for CR3 of 2.')
         Call ERROR
      Endif
!
!   The dimension of the data array depends on whether a CRM probability
!   array is provided by a 5CR2 record, whether a month MON is specified
!   on the 2FRE, 2FRQ, 6FRE, or 6FRQ record, and whether data is monthly
!   (SIMD=0) SIM/SIMD or sub-monthly (SIMD=1) SIMD output.
!
      If(CRSFF.EQ.0) Then
         If(SIMD.EQ.0) Then                                                 !day
            If(MON.EQ.0) Then
               Allocate(Q(MONTHS),QTOTAL(MONTHS))
            Else
               Allocate(Q(MONTHS/NPRDS),QTOTAL(MONTHS/NPRDS))
            Endif
!
!   SUB file is not processed for CRM.  SIMD OUT file is used for CRM.
!   For SUB file processing, determine the number of time steps DVALS.
!   If less than 1 year is reflected in the SUB file, MON may possibly
!   not correspond to any data in the SUB file.
!
         Else                                                               !day
            If(MON.EQ.0) Then                                               !day
               DVALS=DAYS                                                   !day
            Else                                                            !day
               DVALS=0                                                      !day
               Do I=1,DAYS                                                  !day
                  If(DDATA(I,2).EQ.MON) DVALS=DVALS+1                       !day
               End Do                                                       !day
            Endif                                                           !day
            If(DVALS.GT.0) Then                                             !day
               Allocate(Q(DVALS),QTOTAL(DVALS))                             !day
            Else                                                            !day
               Write(20,*) 'ERROR: There are no records in SIMD ',          !day
     +         'output file for the month listed on FRE/FRQ record.'        !day
               Call ERROR                                                   !day
            Endif                                                           !day
         Endif                                                              !day
      Else
         If(SIMD.EQ.0) Then                                                 !day
            If(MON.EQ.0) Then
               Allocate(Q(MONTHS),QT(MONTHS),QTOTAL(MONTHS))
            Elseif(MON.GT.0) Then
               Allocate(Q(MONTHS/NPRDS),QT(MONTHS/NPRDS),
     +                  QTOTAL(MONTHS/NPRDS))
            Endif
            QT=0.0
         Else                                                               !day
            If(MON.EQ.0) Then                                               !day
               DVALS=DAYS                                                   !day
            Else                                                            !day
               DVALS=0                                                      !day
               Do I=1,DAYS                                                  !day
                  If(DDATA(I,2).EQ.MON) DVALS=DVALS+1                       !day
               End Do                                                       !day
            Endif                                                           !day
            If(DVALS.GT.0) Then                                             !day
               Allocate(Q(DVALS),QT(DVALS),QTOTAL(DVALS))                   !day
            Else                                                            !day
               Write(20,*) 'ERROR: There are no records in SIMD ',          !day
     +         'output file for the month listed on FRE/FRQ record.'        !day
               Call ERROR                                                   !day
            Endif                                                           !day
         Endif                                                              !day
      Endif
      Q=0.0
      QTOTAL=0.0
!
!   Counters of months are as follows:
!   NRPDS  = 12 or CR1 from main program depending on CR1.
!   MONTHS = NPRDS*NYRS from main program.
!   MON = individual month from 2FRE/2FRQ record selected for analysis.
!   MCR = individual month of year in CRM simulation compared with MON.
!   M   = total number of months of data in the array Q(I) being sorted.
!   MS  = total number of periods in the data array Q(I) after the sorting
!         routine which may be adjusted for the 5CR2 record probabilities.
!
      If(CR1.EQ.0.and.MON.EQ.0) Then
         MS=MONTHS
      Elseif(CR1.EQ.0.and.MON.GT.0) Then
         MS=MONTHS/12
      Elseif(CR1.GT.0.and.MON.EQ.0) Then
         MS=MONTHS
      Elseif(CR1.GT.0.and.MON.GT.0) Then
         MS=MONTHS/NPRDS
      Endif
      If(SIMD.EQ.1) MS=DVALS                                                !day
!
!   Table headings are written for 2FRE record tables.
!
      Call TITLES
      If(CD.EQ.'2FRE'.or.CD.EQ.'6FRE') Then                                 !day
         If(MON.GT.0) Then
            If(ID.EQ.1) Write(2,610) MON
            If(ID.EQ.2) Write(2,620) MON
            If(ID.EQ.3) Write(2,630) MON
            If(ID.EQ.4) Write(2,640) MON
            If(ID.EQ.5) Write(2,650) MON
            If(ID.EQ.6) Write(2,670) MON
            If(ID.EQ.7) Write(2,680) MON
            If(ID.EQ.8) Write(2,660) MON
            If(SIMD.EQ.1) Call SIMDHEADER                                   !day
         Else
            If(ID.EQ.1) Write(2,710)
            If(ID.EQ.2) Write(2,720)
            If(ID.EQ.3) Write(2,730)
            If(ID.EQ.4) Write(2,740)
            If(ID.EQ.5) Write(2,750)
            If(ID.EQ.6) Write(2,770)
            If(ID.EQ.7) Write(2,780)
            If(ID.EQ.8) Write(2,760)
            If(SIMD.EQ.1) Call SIMDHEADER                                   !day
         Endif
!
         If(MAT.EQ.1) Write(2,790) TIME
         If(MAT.EQ.2) Write(2,800) TIME
!
         If(CR1.GT.0.and.CRHEAD.GE.0.and.SIMD.EQ.0) Then                    !day
            If(CRSFF.EQ.0) Write(2,1110)
            If(CRSFF.GT.0) Write(2,1120)
            If(CR2.GT.0) Write(2,1150) CR2
            If(CR2.EQ.0) Write(2,1160) NYRS
            Write(2,1130) CR1
            If(CR4.GT.0.0) Write(2,1140) CR4
            Write(2,*) ' '
         Endif
!
         If(TABLE.LE.1) Then
            If(ID.LE.4.or.ID.EQ.6.or.ID.EQ.7) Write(2,930)
            If(ID.EQ.5.or.ID.EQ.8) Write(2,932)
!
            If(SIMD.EQ.0) Then                                              !day
               If(ID.LE.3) Write(2,870)
               If(ID.LE.3) Write(2,880)
               If(ID.EQ.4) Write(2,890)
               If(ID.EQ.4) Write(2,880)
               If(ID.EQ.5) Write(2,900)
               If(ID.EQ.5) Write(2,910)
               If(ID.EQ.8) Write(2,920)
               If(ID.EQ.8) Write(2,910)
               If(ID.EQ.6.or.ID.EQ.7) Write(2,810)
               If(ID.EQ.6.or.ID.EQ.7) Write(2,830)
            Else                                                            !day
               If(ID.LE.3) Write(2,872)                                     !day
               If(ID.LE.3) Write(2,882)                                     !day
               If(ID.EQ.4) Write(2,892)                                     !day
               If(ID.EQ.4) Write(2,882)                                     !day
               If(ID.EQ.5) Write(2,902)                                     !day
               If(ID.EQ.5) Write(2,912)                                     !day
               If(ID.EQ.8) Write(2,922)                                     !day
               If(ID.EQ.8) Write(2,912)                                     !day
               If(ID.EQ.6.or.ID.EQ.7) Write(2,820)                          !day
               If(ID.EQ.6.or.ID.EQ.7) Write(2,840)                          !day
            Endif                                                           !day
!
            If(ID.LE.4.or.ID.EQ.6.or.ID.EQ.7) Write(2,930)
            If(ID.EQ.5.or.ID.EQ.8) Write(2,932)
         Endif
      Endif
!
!   Table headings are written for 2FRQ record tables.
!
      If(CD.EQ.'2FRQ'.or.CD.EQ.'6FRQ') Then                                 !day
         If(MON.LE.0) Then
            If(ID.EQ.1) Write(2,940) Adjustl(IDCP(1))
            If(ID.EQ.2) Write(2,950) Adjustl(IDCP(1))
            If(ID.EQ.3) Write(2,960) Adjustl(IDCP(1))
            If(ID.EQ.4) Write(2,970) Adjustl(IDCP(1))
            If(ID.EQ.5) Write(2,980) Adjustl(IDEN16(1))
            If(ID.EQ.8) Write(2,990) Adjustl(IDEN16(1))
            If(ID.EQ.6) Write(2,1000) Adjustl(IDRES(1))
            If(ID.EQ.7) Write(2,1010) Adjustl(IDRES(1))
            If(SIMD.EQ.1) Call SIMDHEADER                                   !day
         Else
            If(ID.EQ.1) Write(2,1040) Adjustl(IDCP(1)),MON
            If(ID.EQ.2) Write(2,1050) Adjustl(IDCP(1)),MON
            If(ID.EQ.3) Write(2,1060) Adjustl(IDCP(1)),MON
            If(ID.EQ.4) Write(2,1070) Adjustl(IDCP(1)),MON
            If(ID.EQ.5) Write(2,1080) Adjustl(IDEN16(1)),MON
            If(ID.EQ.8) Write(2,1090) Adjustl(IDEN16(1)),MON
            If(ID.EQ.6) Write(2,1020) Adjustl(IDRES(1)),MON
            If(ID.EQ.7) Write(2,1030) Adjustl(IDRES(1)),MON
            If(SIMD.EQ.1) Call SIMDHEADER                                   !day
         Endif
         If(CR1.GT.0.and.CRHEAD.GE.0.and.SIMD.EQ.0) Then                    !day
            If(CRSFF.EQ.0) Write(2,1110)
            If(CRSFF.GT.0) Write(2,1120)
            If(CR2.GT.0) Write(2,1150) CR2
            If(CR2.EQ.0) Write(2,1160) NYRS
            Write(2,1130) CR1
            If(CR4.GT.0.0) Write(2,1140) CR4
            Write(2,*) ' '
         Endif
         Write(2,190) (('------------------'),L=1,NM)
         If(ID.LE.3) Write(2,190) (('    FLOW   FREQ(%)'),L=1,NM)
         If(ID.EQ.4) Write(2,190) (('  STORAGE  FREQ(%)'),L=1,NM)
         If(ID.EQ.5) Write(2,190) (('  STORAGE  FREQ(%)'),L=1,NM)
         If(ID.EQ.6) Write(2,190) (('  STORAGE  FREQ(%)'),L=1,NM)
         If(ID.EQ.7) Write(2,190) ((' ELEVATION FREQ(%)'),L=1,NM)
         If(ID.EQ.8) Write(2,190) (('    FLOW   FREQ(%)'),L=1,NM)
         Write(2,190) (('------------------'),L=1,NM)
190      Format(7A18)
      Endif
!
!   Record counters are devised for reading the SIM OUT or SIMD SUB file.
!
      COUNT=NUM
      If(SIMD.EQ.0) Then                                                    !day
         CREC=NWROUT+NCPTS+NREOUT
         If(ID.LE.4) Then
            REC1=6+NWROUT
            SKIP=NCPTS
            If(NUM.EQ.0) COUNT=NCPTS
         Elseif(ID.EQ.5.or.ID.EQ.8) Then
            REC1=6
            SKIP=NWROUT
            If(NUM.EQ.0) COUNT=NWROUT
         Elseif(ID.EQ.6.or.ID.EQ.7) Then
            REC1=6+NWROUT+NCPTS
            SKIP=NREOUT
            If(NUM.EQ.0) COUNT=NREOUT
         Endif
      Else                                                                  !day
         CREC=NWROUT2+NCPO2+NREOUT2                                         !day
         If(ID.LE.4) Then                                                   !day
            REC1=7+NWROUT2                                                  !day
            SKIP=NCPO2                                                      !day
            If(NUM.EQ.0) COUNT=NCPO2                                        !day
         Elseif(ID.EQ.5.or.ID.EQ.8) Then                                    !day
            REC1=7                                                          !day
            SKIP=NWROUT2                                                    !day
            If(NUM.EQ.0) COUNT=NWROUT2                                      !day
         Elseif(ID.EQ.6.or.ID.EQ.7) Then                                    !day
            REC1=6+NWROUT+NCPO2                                             !day
            SKIP=NREOUT2                                                    !day
            If(NUM.EQ.0) COUNT=NREOUT2                                      !day
         Endif                                                              !day
      Endif                                                                 !day
!
!   XQFREQ array is allocated for TABLE options 2,3,4 in 2FRE/6FRE field 5.
!
      If(TABLE.GE.2) Then
         If(ID.EQ.4.or.ID.EQ.5.or.ID.EQ.6) Then
            I=COUNT+1
         Else
            I=COUNT
         Endif
         If(ID.EQ.5.or.ID.EQ.8) Then
            Allocate(XQFREQ(I,25),XID16(I))
            XID16='                '
         Else
            Allocate(XQFREQ(I,25),XID(I))
            XID='      '
         Endif
         XQFREQ=0.0
      Endif
!
!   Format specifications for reading the SIM OUT file (unit=4) are assigned.
!
      If(ID.EQ.1) Then
         KK1='(A6,77x,F11.0)'
      Elseif(ID.EQ.2) Then
         KK1='(A6,88x,F11.0)'
      Elseif(ID.EQ.3) Then
         KK1='(A6,55x,F11.0)'
      Elseif(ID.EQ.4.or.ID.EQ.6) Then
         KK1='(A6,33x,F11.0)'
      Elseif(ID.EQ.5) Then
         KK1='(39x,F11.0,33x,A16)'
      Elseif(ID.EQ.7) Then
         KK1='(A6,110x,F11.0)'
      Elseif(ID.EQ.8) Then
         KK1='(83x,A16,11x,F11.0)'
      Endif
!
!   +++++++++++++++++++++++++++ Beginning of Loop +++++++++++++++++++++++++++++++
!   Beginning of loop to develop tables for COUNT control points or water rights.
!
      LOOP=0
200   LOOP=LOOP+1
!
!   The record is found in the SIM OUT file (unit=4) or SIMD SUB file (unit=10)
!   from which the first data value is read.
!
      If(NUM.GT.0) Then
         RECD=REC1
         If(OUTFORM.EQ.1) Then
            If(ID.LE.4.or.ID.EQ.6.or.ID.EQ.7) Read(UO,REC=RECD) WRAPID
            If(ID.EQ.5.or.ID.EQ.8) Read(UO,REC=RECD) IO1,IO2,
     +                                            (XO(I),I=1,7),WRAPID16
         Else
            If(ID.LE.4.or.ID.EQ.6.or.ID.EQ.7)Read(UO,210,REC=RECD)WRAPID
            If(ID.EQ.5.or.ID.EQ.8) Read(UO,220,REC=RECD) WRAPID16
210         Format(A6)
220         Format(83x,A16)
         Endif
         N=1
230      If(WRAPID.EQ.IDCP(LOOP).and.ID.LE.4) Goto 260
         If(WRAPID.EQ.IDRES(LOOP).and.(ID.EQ.6.or.ID.EQ.7)) Goto 260
         If(WRAPID16.EQ.IDEN16(LOOP).and.(ID.EQ.5.or.ID.EQ.8)) Goto 260
         RECD=RECD+1
         N=N+1
         If(OUTFORM.EQ.1) Then
            If(ID.LE.4.or.ID.EQ.6.or.ID.EQ.7) Read(UO,REC=RECD) WRAPID
            If(ID.EQ.5.or.ID.EQ.8) Read(UO,REC=RECD) IO1,IO2,
     +                                            (XO(I),I=1,7),WRAPID16
         Else
            If(ID.LE.4.or.ID.EQ.6.or.ID.EQ.7)Read(UO,210,REC=RECD)WRAPID
            If(ID.EQ.5.or.ID.EQ.8) Read(UO,220,REC=RECD) WRAPID16
         Endif
         If(N.LE.SKIP) Goto 230
         If(ID.LE.4) Write(20,240) IDCP(LOOP),CD
         If(ID.EQ.6.or.ID.EQ.7) Write(20,240) IDRES(LOOP),CD
         If(ID.EQ.5.or.ID.EQ.8) Write(20,250) IDEN16(LOOP), CD
240      Format(' ERROR: Identifier ',A6,' from ',A4,' record',
     +          ' was not found in WRAP output file.')
250      Format(' ERROR: Identifier ',A16,' from ',A4,' record',
     +          ' was not found in WRAP output file.')
         Call ERROR
      Endif
!
!   Read Q(I) which is naturalized flow (ID=1), regulated flow (ID=2), unappropriated
!   flow (ID=3), storage (ID=4,5,6), elevation (ID=7), or instream shortage (ID=8).
!
260   If(NUM.EQ.0) RECD=REC1+LOOP-1
      L=0
      J=0
      N=MONTHS
      If(SIMD.EQ.1) N=DAYS
      Do I=1,N
         If(J.EQ.NPRDS) J=0
         J=J+1
         MCR=J
         If(CR1.GT.0) Then
            MCR=J+CR2-1
            If(MCR.GT.12) MCR=MCR-12
         Endif
         KKK=MCR
         If(SIMD.EQ.1) KKK=DDATA(I,2)
         If(MON.LE.0.or.KKK.EQ.MON) Then
            L=L+1
            If(SIMD.EQ.1) L=I
            If(OUTFORM.EQ.1) Then
               If(ID.EQ.1) Read(UO,REC=RECD) WRAPID,(XO(K),K=1,7),Q(L)
               If(ID.EQ.2) Read(UO,REC=RECD) WRAPID,(XO(K),K=1,8),Q(L)
               If(ID.EQ.3) Read(UO,REC=RECD) WRAPID,(XO(K),K=1,5),Q(L)
               If(ID.EQ.4) Read(UO,REC=RECD) WRAPID,(XO(K),K=1,3),Q(L)
               If(ID.EQ.5) Read(UO,REC=RECD) IO1,IO2,(XO(K),K=1,3),Q(L),
     +                                       (XO(K),K=5,7),WRAPID16
               If(ID.EQ.6) Read(UO,REC=RECD) WRAPID,(XO(K),K=1,3),Q(L)
               If(ID.EQ.7) Read(UO,REC=RECD) WRAPID,(XO(K),K=1,10),Q(L)
               If(ID.EQ.8) Then
                  Read(UO,REC=RECD) IO1
                  If(IO1.NE.-1) Then
                     Read(UO,REC=RECD) IO1,IO2,(XO(K),K=1,7),WRAPID16
                     Q(L)=0.0
                  Else
                     Read(UO,REC=RECD) IO1,IO2,(XO(K),K=1,7),
     +                                       WRAPID16,XO(8),Q(L)
                  Endif
               Endif
            Else
               If(ID.LE.4) Read(UO,KK1,REC=RECD) WRAPID,Q(L)
               If(ID.EQ.6.or.ID.EQ.7) Read(UO,KK1,REC=RECD) WRAPID,Q(L)
               If(ID.EQ.5) Read(UO,KK1,REC=RECD) Q(L),WRAPID16
               If(ID.EQ.8) Then
                  Read(UO,'(A2)',REC=RECD) IFC
                  If(IFC.NE.'IF') Then
                     Q(L)=0.0
                  Else
                     Read(UO,KK1,REC=RECD) WRAPID16,Q(L)
                  Endif
               Endif
            Endif
            Q(L)=(Q(L)*XF)+AF
            If(ID.EQ.4.or.ID.EQ.5.or.ID.EQ.6) QTOTAL(L)=QTOTAL(L)+Q(L)
         Endif
         RECD=RECD+CREC
      Enddo
!
!   The data Q(I) are converted to TIME-month moving averages or totals
!   if MAT is greater than zero.
!
      If(MAT.GT.0) Then
         If(SIMD.EQ.0) Then
            L=MONTHS
         Else
            L=DAYS
         Endif
         Do K=1,L
            MX(K)=Q(K)
            MAN=TIME
            If(K.LT.TIME) MAN=K
            Q(K)=0.0
            Do I=1,MAN
               J=K+1-I
               Q(K)=Q(K)+MX(J)
            End Do
            If(MAT.EQ.1) Q(K)=Q(K)/Real(MAN)
         End Do
!
!     Reservoir storage totals.
!
         If(ID.EQ.4.or.ID.EQ.5.or.ID.EQ.6) Then
            Do K=1,L
               MX(K)=QTOTAL(K)
               MAN=TIME
               If(K.LT.TIME) MAN=K
               QTOTAL(K)=0.0
               Do I=1,MAN
                  J=K+1-I
                  QTOTAL(K)=QTOTAL(K)+MX(J)
               End Do
               If(MAT.EQ.1) QTOTAL(K)=QTOTAL(K)/Real(MAN)
            End Do
         Endif
      Endif
!
!   With ID entered as -4 , -5, or -6, STOFLAG=9 and only
!   the total line is included in the storage-frequency table.
!
      If(STOFLAG.EQ.9) Go to 320
!
!   The expected probability array EXPP is stored as a temporary
!   array EXPPT for which the order will change.
!   If all the months are being used (MON=0), the equally likely
!   option is adopted, and the EXPP array is not used.
!
270   If(CRSFF.GT.0.and.MON.GT.0.and.SIMD.EQ.0) Then                        !day
         If(Allocated(EXPPT)) Deallocate(EXPPT)
         NP=SIZE(EXPP)
         Allocate(EXPPT(NP))
         EXPPT=EXPP
      Endif
!
!   Q(I) is sorted in descending order.
!
      If(SIMD.EQ.0) Then                                                    !day
         If(MON.LE.0) Then
            M=MONTHS
         Else
            M=MONTHS/NPRDS
         Endif
         SORTED=.FALSE.
280      If(.NOT.SORTED) Then
            SORTED=.TRUE.
            Do I=1,M-1
               If(Q(I).LT.Q(I+1)) Then
                  TEMP=Q(I)
                  Q(I)=Q(I+1)
                  Q(I+1)=TEMP
                  If(CRSFF.GT.0.and.MON.GT.0) Then
                     TEMP2=EXPPT(I)
                     EXPPT(I)=EXPPT(I+1)
                     EXPPT(I+1)=TEMP2
                  Endif
                  SORTED=.FALSE.
               Endif
            End Do
            Goto 280
         Endif
      Else                                                                  !day
         M=DVALS                                                            !day
         SORTED=.FALSE.                                                     !day
         Call Sort(DVALS,Q,1)                                               !day
         SORTED=.TRUE.                                                      !day
      Endif                                                                 !day
!
!   The array size is increased by TOTREP times with each Q(I) being entered
!   multiple times in proportion to the 5CR2 record probabilities.
!
      If(CRSFF.GT.0.and.MON.GT.0.and.SIMD.EQ.0) Then                        !day
         QT=Q(1:M)
         TOTREP=0
         Do I=1,M
            TOTREP=INT(EXPPT(I)*1000000)+TOTREP
         Enddo
         Deallocate(Q)
         Allocate(Q(TOTREP))
         L=0
         Do I=1,M
            N=INT(EXPPT(I)*1000000)
            If(N.GT.0) Then
               Do J=1,N
                  L=L+1
                  Q(L)=QT(I)
               Enddo
            Endif
         Enddo
         MS=L
      Endif
!
!   Mean and standard deviation are computed.
!
      SUM=0
      SUMSD=0.0
      Do I=1,MS
         SUM=SUM+Q(I)
      End Do
      MEAN=SUM/MS
      Do I=1,MS
         SUMSD=SUMSD+(Q(I)-MEAN)**2
      End Do
      STDDEV=(SUMSD/(MS-1))**0.5
!
!   Mean and standard deviation of the logarithms are computed
!   if METHOD option 2 is specified on the 2FRE or 6FRE record.
!
      If(METHOD.EQ.2) Then
         SUM=0
         SUMSD=0.0
         Do I=1,MS
            X=Q(I)
            If(X.LE.0.0) X=0.01
            SUM=SUM+LOG10(X)
         End Do
         MEANLOG=SUM/MS
         Do I=1,MS
            X=Q(I)
            If(X.LE.0.0) X=0.01
            SUMSD=SUMSD+(LOG10(X)-MEANLOG)**2
         End Do
         SDLOG=(SUMSD/(MS-1))**0.5
      Endif
!
!   Frequency-flow or -storage relationship is developed for 2FRE or 6FRE record.
!
      If(CD.EQ.'2FRE'.or.CD.EQ.'6FRE') Then                                 !day
         If(TABLE.LE.1) Then
            NF=10
            F(1)=0.99
            F(2)=0.98
            F(3)=0.95
            F(4)=0.90
            F(5)=0.75
            F(6)=0.60
            F(7)=0.50
            F(8)=0.40
            F(9)=0.25
            F(10)=0.10
         Elseif(TABLE.GE.2) Then
            NF=21
            F(1)=0.995
            F(2)=0.99
            F(3)=0.98
            F(4)=0.95
            F(5)=0.90
            F(6)=0.85
            F(7)=0.80
            F(8)=0.75
            F(9)=0.70
            F(10)=0.60
            F(11)=0.50
            F(12)=0.40
            F(13)=0.30
            F(14)=0.25
            F(15)=0.20
            F(16)=0.15
            F(17)=0.10
            F(18)=0.05
            F(19)=0.02
            F(20)=0.01
            F(21)=0.005
         Endif
!
!   Relative frequency option specified by METHOD=1 on 2FRE record.
!
         If(METHOD.LE.1) Then
            Do I=1,NF
               FX=F(I)*Real(MS)
               IF1=INT(FX)
               IF2=IF1+1
               DXF=FX-Real(IF1)
               If(IF1.GT.0.and.IF2.GT.0) Then
                  QFREQ(I)=(Q(IF1)-Q(IF2))*(1.0-DXF) + Q(IF2)
               Else
                  QFREQ(I)=Q(1)
               Endif
            End Do
         Endif
!
!   Log-normal distribution option specified by METHOD=2 on 2FRE record
!   or normal distribution option specified by METHOD=3 on 2FRE record.
!
         If(METHOD.GE.2) Then
            If(TABLE.LE.1) Then
               Z(1)=-2.32637
               Z(2)=-2.05377
               Z(3)=-1.64487
               Z(4)=-1.28156
               Z(5)=-0.674496
               Z(6)=-0.253351
               Z(7)= 0.00000
               Z(8)= 0.253351
               Z(9)= 0.674496
               Z(10)=1.28156
            Elseif(TABLE.GE.2) Then
               Z(1)=-2.57583
               Z(2)=-2.32637
               Z(3)=-2.05377
               Z(4)=-1.64487
               Z(5)=-1.28156
               Z(6)=-1.03645
               Z(7)=-0.84162
               Z(8)=-0.67450
               Z(9)=-0.52440
               Z(10)=-0.25335
               Z(11)=0.00000
               Z(12)=0.25335
               Z(13)=0.52440
               Z(14)=0.67445
               Z(15)=0.84162
               Z(16)=1.03645
               Z(17)=1.28156
               Z(18)=1.64485
               Z(19)=2.05375
               Z(20)=2.32635
               Z(21)=2.57583
            Endif
            If(METHOD.EQ.2) Then
               Do I=1,NF
                  QFREQ(I)=10**(MEANLOG+SDLOG*Z(I))
               Enddo
            Elseif(METHOD.EQ.3) Then
               Do I=1,NF
                  QFREQ(I)=MEAN+STDDEV*Z(I)
               Enddo
            Endif
         Endif
!
!   Table is developed for 2FRE/6FRE record.
!
         If(STOFLAG.EQ.0.and.TABLE.LE.1) Then
            If(ID.LE.4.or.ID.EQ.6.or.ID.EQ.7) Then
               If(ID.EQ.4.or.ID.EQ.6)
     +            KK2='(A6,1x,F8.0,F9.0,10F8.0,2F9.0)'
               If(ID.EQ.7) KK2='(A6,2x,13F8.2,F10.2)'
               If(ID.LE.3) Then
                  If(SIMD.EQ.0)KK2='(A6,2x,F8.1,F8.0,6F8.1,4F8.0,2F9.0)'
                  If(SIMD.NE.0)KK2='(A6,F9.2,F9.1,6F8.2,4F8.1,2F9.1)'    !day
               Endif
               Write(2,KK2) Adjustl(WRAPID),MEAN,STDDEV,Q(MS),
     +                             (QFREQ(I),I=1,10),Q(1)
            Elseif(ID.EQ.5) Then
               KK2='(A13,12F8.0,2F9.0)'
               Write(2,KK2) Adjustl(WRAPID16),MEAN,STDDEV,Q(MS),
     +                             (QFREQ(I),I=1,10),Q(1)
            Elseif(ID.EQ.8) Then
               If(IFC.EQ.'IF') Then
                  KK2='(A13,12F8.2,2F9.2)'
                  Write(2,KK2) Adjustl(WRAPID16),MEAN,STDDEV,Q(MS),
     +                                (QFREQ(I),I=1,10),Q(1)
               Endif
            Endif
         Endif
!
!   Output data are stored for 2FRE/6FRE record TABLE options 2,3,4.
!
         If(TABLE.GE.2) Then
            If(ID.LE.4.or.ID.EQ.6.or.ID.EQ.7) Then
               XID(LOOP)=WRAPID
            Elseif(ID.EQ.5.or.ID.EQ.8) Then
               XID16(LOOP)=WRAPID16
            Endif
            XQFREQ(LOOP,1)=MEAN
            XQFREQ(LOOP,2)=STDDEV
            XQFREQ(LOOP,3)=Q(MS)
            Do I=4,24
               J=I-3
               XQFREQ(LOOP,I)=QFREQ(J)
            Enddo
            XQFREQ(LOOP,25)=Q(1)
         Endif
      Endif
!
!   Frequencies are computed for quantities entered on 2FRQ records.
!
      If(CD.EQ.'2FRQ'.or.CD.EQ.'6FRQ') Then                                 !day
         Do 300 J=1,NM
            I=0
290         I=I+1
            If(I.GT.MS) Then
               FQ(J)=100.0
               Goto 300
            Endif
            If(QF(J).LE.Q(I)) Then
               Goto 290
            Else
               FQ(J)=(Real(I-1)/Real(MS))*100.0
            Endif
300      End Do
         Write(2,310) (QF(J),FQ(J),J=1,NM)
310      Format(7(F10.1,F8.2))
      Endif
!
!  ++++++++++++++++++++++++++  End of Loop  +++++++++++++++++++++++++++++
!
320   If(LOOP.LT.COUNT) Goto 200
!
!   Completion of 2FRQ/6FRQ record table.
!
      If(CD.EQ.'2FRQ'.or.CD.EQ.'6FRQ')
     +   Write(2,190) (('------------------'),L=1,NM)
!
!   For storage, add total storage at bottom of 2FRE/6FRE table.
!
      If((CD.EQ.'2FRE'.or.CD.EQ.'6FRE').and.MAT.EQ.0) Then
         If((ID.EQ.4.or.ID.EQ.5.or.ID.EQ.6).and.STOFLAG.GE.0) Then
            Q(1:M)=QTOTAL(1:M)
            STOFLAG=-9
            LOOP=LOOP+1
            Goto 270
         Endif
         If(TABLE.LE.1) Then
            If(ID.EQ.4.or.ID.EQ.6) Then
               Write(2,330) MEAN,STDDEV,Q(MS),(QFREQ(I),I=1,10),Q(1)
330            Format('Total ',2F9.0,10F8.0,2F9.0)
            Elseif(ID.EQ.5) Then
               Write(2,340) MEAN,STDDEV,Q(MS),(QFREQ(I),I=1,10),Q(1)
340            Format('Total      ',F10.0,11F8.0,2F9.0)
            Endif
         Endif
      Endif
!
!   Line at bottom of table.
!
      If((CD.EQ.'2FRE'.or.CD.EQ.'6FRE').and.TABLE.LE.1) Then
         If(ID.LE.4.or.ID.EQ.6.or.ID.EQ.7) Then
            Write(2,930)
         Elseif(ID.GE.5.or.ID.EQ.8) Then
            Write(2,932)
         Endif
      Endif
!
!   2FRE/6FRE record frequency table in columnar format for TABLE=2,3,4.
!
      If(TABLE.GE.2) Then
         IJ=1
         If(Abs(STOFLAG2).EQ.9) Then
            COUNT=COUNT+1
            If(ID.EQ.5) Then
               XID16(COUNT)='TOTAL'
            Else
               XID(COUNT)='TOTAL'
            Endif
            If(STOFLAG2.EQ.-9) IJ=COUNT
         Endif
         If(TABLE.EQ.4) Then
            Do I=IJ,COUNT
               Do J=3,24
                  XQFREQ(I,J)=(XQFREQ(I,J)/XQFREQ(I,25))*100.0
               Enddo
            Enddo
         Endif
         If(ID.LE.4.or.ID.EQ.6.or.ID.EQ.7) Then
            If(ID.LE.3) Then
               Write(2,400) (Adjustr(XID(LOOP)),LOOP=IJ,COUNT)
            Else
               Write(2,410) (Adjustr(XID(LOOP)),LOOP=IJ,COUNT)
            Endif
            Write(2,420) (XQFREQ(LOOP,1),LOOP=IJ,COUNT)
            If(TABLE.LE.3) Write(2,430) (XQFREQ(LOOP,2),LOOP=IJ,COUNT)
            Write(2,440) (XQFREQ(LOOP,3),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,450) (XQFREQ(LOOP,4),LOOP=IJ,COUNT)
            Write(2,452) (XQFREQ(LOOP,5),LOOP=IJ,COUNT)
            Write(2,454) (XQFREQ(LOOP,6),LOOP=IJ,COUNT)
            Write(2,456) (XQFREQ(LOOP,7),LOOP=IJ,COUNT)
            Write(2,458) (XQFREQ(LOOP,8),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,460) (XQFREQ(LOOP,9),LOOP=IJ,COUNT)
            Write(2,462) (XQFREQ(LOOP,10),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,464) (XQFREQ(LOOP,11),LOOP=IJ,COUNT)
            Write(2,466) (XQFREQ(LOOP,12),LOOP=IJ,COUNT)
            Write(2,468) (XQFREQ(LOOP,13),LOOP=IJ,COUNT)
            Write(2,470) (XQFREQ(LOOP,14),LOOP=IJ,COUNT)
            Write(2,472) (XQFREQ(LOOP,15),LOOP=IJ,COUNT)
            Write(2,474) (XQFREQ(LOOP,16),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,476) (XQFREQ(LOOP,17),LOOP=IJ,COUNT)
            Write(2,478) (XQFREQ(LOOP,18),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,480) (XQFREQ(LOOP,19),LOOP=IJ,COUNT)
            Write(2,482) (XQFREQ(LOOP,20),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,484) (XQFREQ(LOOP,21),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,486) (XQFREQ(LOOP,22),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,488) (XQFREQ(LOOP,23),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,490) (XQFREQ(LOOP,24),LOOP=IJ,COUNT)
            Write(2,492) (XQFREQ(LOOP,25),LOOP=IJ,COUNT)
400         Format('CP      ',<COUNT>(5x,A6))
410         Format('Res     ',<COUNT>(5x,A6))
420         Format('Mean    ',<COUNT>F11.2)
430         Format('Std Dev ',<COUNT>F11.2)
440         Format('Minimum ',<COUNT>F11.2)
450         Format(' 99.5%  ',<COUNT>F11.2)
452         Format('  99%   ',<COUNT>F11.2)
454         Format('  98%   ',<COUNT>F11.2)
456         Format('  95%   ',<COUNT>F11.2)
458         Format('  90%   ',<COUNT>F11.2)
460         Format('  85%   ',<COUNT>F11.2)
462         Format('  80%   ',<COUNT>F11.2)
464         Format('  75%   ',<COUNT>F11.2)
466         Format('  70%   ',<COUNT>F11.2)
468         Format('  60%   ',<COUNT>F11.2)
470         Format('  50%   ',<COUNT>F11.2)
472         Format('  40%   ',<COUNT>F11.2)
474         Format('  30%   ',<COUNT>F11.2)
476         Format('  25%   ',<COUNT>F11.2)
478         Format('  20%   ',<COUNT>F11.2)
480         Format('  15%   ',<COUNT>F11.2)
482         Format('  10%   ',<COUNT>F11.2)
484         Format('   5%   ',<COUNT>F11.2)
486         Format('   2%   ',<COUNT>F11.2)
488         Format('   1%   ',<COUNT>F11.2)
490         Format('  0.5%  ',<COUNT>F11.2)
492         Format('Maximum ',<COUNT>F11.2)
         Elseif(ID.EQ.5.or.ID.EQ.8) Then
            Write(2,510) (Adjustr(XID16(LOOP)),LOOP=IJ,COUNT)
            Write(2,520) (XQFREQ(LOOP,1),LOOP=IJ,COUNT)
            If(TABLE.LE.3) Write(2,530) (XQFREQ(LOOP,2),LOOP=IJ,COUNT)
            Write(2,540) (XQFREQ(LOOP,3),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,550) (XQFREQ(LOOP,4),LOOP=IJ,COUNT)
            Write(2,552) (XQFREQ(LOOP,5),LOOP=IJ,COUNT)
            Write(2,554) (XQFREQ(LOOP,6),LOOP=IJ,COUNT)
            Write(2,556) (XQFREQ(LOOP,7),LOOP=IJ,COUNT)
            Write(2,558) (XQFREQ(LOOP,8),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,560) (XQFREQ(LOOP,9),LOOP=IJ,COUNT)
            Write(2,562) (XQFREQ(LOOP,10),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,564) (XQFREQ(LOOP,11),LOOP=IJ,COUNT)
            Write(2,566) (XQFREQ(LOOP,12),LOOP=IJ,COUNT)
            Write(2,568) (XQFREQ(LOOP,13),LOOP=IJ,COUNT)
            Write(2,570) (XQFREQ(LOOP,14),LOOP=IJ,COUNT)
            Write(2,572) (XQFREQ(LOOP,15),LOOP=IJ,COUNT)
            Write(2,574) (XQFREQ(LOOP,16),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,576) (XQFREQ(LOOP,17),LOOP=IJ,COUNT)
            Write(2,578) (XQFREQ(LOOP,18),LOOP=IJ,COUNT)
            If(TABLE.LE.2) Write(2,580) (XQFREQ(LOOP,19),LOOP=IJ,COUNT)
            Write(2,582) (XQFREQ(LOOP,20),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,584) (XQFREQ(LOOP,21),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,586) (XQFREQ(LOOP,22),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,588) (XQFREQ(LOOP,23),LOOP=IJ,COUNT)
            If(TABLE.EQ.2) Write(2,590) (XQFREQ(LOOP,24),LOOP=IJ,COUNT)
            Write(2,592) (XQFREQ(LOOP,25),LOOP=IJ,COUNT)
510         Format('WR      ',<COUNT>A16)
520         Format('Mean    ',<COUNT>F16.3)
530         Format('Std Dev ',<COUNT>F16.3)
540         Format('Minimum ',<COUNT>F16.3)
550         Format(' 99.5%  ',<COUNT>F16.2)
552         Format('  99%   ',<COUNT>F16.2)
554         Format('  98%   ',<COUNT>F16.2)
556         Format('  95%   ',<COUNT>F16.2)
558         Format('  90%   ',<COUNT>F16.2)
560         Format('  85%   ',<COUNT>F16.2)
562         Format('  80%   ',<COUNT>F16.2)
564         Format('  75%   ',<COUNT>F16.2)
566         Format('  70%   ',<COUNT>F16.2)
568         Format('  60%   ',<COUNT>F16.2)
570         Format('  50%   ',<COUNT>F16.2)
572         Format('  40%   ',<COUNT>F16.2)
574         Format('  30%   ',<COUNT>F16.2)
576         Format('  25%   ',<COUNT>F16.2)
578         Format('  20%   ',<COUNT>F16.2)
580         Format('  15%   ',<COUNT>F16.2)
582         Format('  10%   ',<COUNT>F16.2)
584         Format('   5%   ',<COUNT>F16.2)
586         Format('   2%   ',<COUNT>F16.2)
588         Format('   1%   ',<COUNT>F16.2)
590         Format('  0.5%  ',<COUNT>F16.2)
592         Format('Maximum ',<COUNT>F16.2)
         Endif
      Endif
!
!   Format statements for 2FRE/6FRE record table headings.
!
610   Format('FLOW-FREQUENCY FOR NATURALIZED STREAMFLOWS FOR MONTH',
     +        I3,/)
620   Format('FLOW-FREQUENCY FOR REGULATED STREAMFLOWS FOR MONTH',
     +        I3,/)
630   Format('FLOW-FREQUENCY FOR UNAPPROPRIATED STREAMFLOWS FOR MONTH',
     +        I3,/)
640   Format('STORAGE-FREQUENCY FOR SPECIFIED CONTROL POINTS FOR MONTH',
     +        I3,/)
650   Format('STORAGE-FREQUENCY FOR SPECIFIED WATER RIGHTS FOR MONTH',
     +        I3,/)
660   Format('FREQUENCY VERSUS INSTREAM FLOW SHORTAGES ',
     +       'FOR SPECIFIED WATER RIGHTS FOR MONTH',I3,/)
670   Format('STORAGE-FREQUENCY FOR SPECIFIED RESERVOIRS FOR MONTH',
     +        I3,/)
680   Format('RESERVOIR WATER SURFACE ELEVATION-FREQUENCY FOR MONTH',
     +        I3,/)
!
710   Format('FLOW-FREQUENCY FOR NATURALIZED STREAMFLOWS',/)
720   Format('FLOW-FREQUENCY FOR REGULATED STREAMFLOWS',/)
730   Format('FLOW-FREQUENCY FOR UNAPPROPRIATED STREAMFLOWS',/)
740   Format('STORAGE-FREQUENCY FOR SPECIFIED CONTROL POINTS',/)
750   Format('STORAGE-FREQUENCY FOR SPECIFIED WATER RIGHTS',/)
760   Format('FREQUENCY VERSUS INSTREAM FLOW SHORTAGES ',
     +          'FOR SPECIFIED WATER RIGHTS',/)
770   Format('STORAGE-FREQUENCY FOR SPECIFIED RESERVOIRS',/)
780   Format('RESERVOIR WATER SURFACE ELEVATION-FREQUENCY',/)
!
790   Format('MOVING AVERAGE FOR',I3,' PERIODS')
800   Format('MOVING TOTAL FOR',I3,' PERIODS')
!
810   Format(16x,'STANDARD',6x,'PERCENTAGE OF MONTHS WITH'
     +        ,' FLOWS EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')
820   Format(16x,'STANDARD',6x,'PERCENTAGE OF DAYS WITH'                    !day
     +        ,' FLOWS EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')    !day
830   Format('RESERVOIR  MEAN DEVIATION  100%',5x,'99%',5x,'98%',5x,
     +          '95%',5x,'90%',5x,'75%',5x,'60%',5x,'50%',5x,'40%',5x,
     +          '25%',5x,'10%    MAXIMUM')
840   Format('RESERVOIR  MEAN DEVIATION   100%',5x,'99%',5x,'98%',5x,       !day
     +          '95%',5x,'90%',5x,'75%',5x,'60%',5x,'50%',5x,'40%',5x,      !day
     +          '25%',5x,'10%   MAXIMUM')                                   !day
!
870   Format('CONTROL',9x,'STANDARD',6x,'PERCENTAGE OF MONTHS WITH'
     +        ,' FLOWS EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')
872   Format('CONTROL',9x,'STANDARD',6x,'PERCENTAGE OF DAYS WITH'           !day
     +        ,' FLOWS EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')    !day
880   Format(' POINT     MEAN DEVIATION  100%',5x,'99%',5x,'98%',5x,
     +          '95%',5x,'90%',5x,'75%',5x,'60%',5x,'50%',5x,'40%',5x,
     +          '25%',5x,'10%    MAXIMUM')
882   Format(' POINT     MEAN DEVIATION   100%',5x,'99%',5x,'98%',5x,       !day
     +          '95%',5x,'90%',5x,'75%',5x,'60%',5x,'50%',5x,'40%',5x,      !day
     +          '25%',5x,'10%   MAXIMUM')                                   !day
890   Format('CONTROL',9x,'STANDARD',5x,'PERCENTAGE OF MONTHS WITH ',
     +       'STORAGE EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')
892   Format('CONTROL',9x,'STANDARD',5x,'PERCENTAGE OF DAYS WITH ',         !day
     +       'STORAGE EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')     !day
900   Format(' WATER',16x,'STANDARD',5x,'PERCENTAGE OF MONTHS WITH ',
     +       'STORAGE EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')
902   Format(' WATER',16x,'STANDARD',5x,'PERCENTAGE OF DAYS WITH ',         !day
     +       'STORAGE EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')     !day
910   Format(' RIGHT          MEAN DEVIATION 100%',5x,'99%',5x,'98%',5x,
     +          '95%',5x,'90%',5x,'75%',5x,'60%',5x,'50%',5x,
     +          '40%',5x,'25%',5x,'10%    MAXIMUM')
912   Format(' RIGHT          MEAN DEVIATION 100%',5x,'99%',5x,'98%',5x,    !day
     +          '95%',5x,'90%',5x,'75%',5x,'60%',5x,'50%',5x,               !day
     +          '40%',5x,'25%',5x,'10%    MAXIMUM')                         !day
920   Format(' WATER',16x,'STANDARD',4x,'PERCENTAGE OF MONTHS WITH ',
     +      'SHORTAGE EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')
922   Format(' WATER',16x,'STANDARD',4x,'PERCENTAGE OF DAYS WITH ',         !day
     +      'SHORTAGE EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')     !day
930   Format(122('-'))
932   Format(127('-'))
!
!   Format statements for 2FRQ/6FRQ record table headings.
!
940   Format('FLOW-FREQUENCY FOR NATURALIZED STREAMFLOWS',
     +          ' FOR CONTROL POINT ',A6,/)
950   Format('FLOW-FREQUENCY FOR REGULATED STREAMFLOWS',
     +          ' FOR CONTROL POINT ',A6,/)
960   Format('FLOW-FREQUENCY FOR UNAPPROPRIATED STREAMFLOWS',
     +          ' FOR CONTROL POINT ',A6,/)
970   Format('STORAGE-FREQUENCY FOR CONTROL POINT ',A6,/)
980   Format('STORAGE-FREQUENCY FOR WATER RIGHT ',A16,/)
990   Format('FREQUENCY VERSUS INSTREAM FLOW SHORTAGES FOR ',
     +          'WATER RIGHT ',A16,/)
1000  Format('STORAGE-FREQUENCY FOR RESERVOIR ',A6,/)
1010  Format('WATER SURFACE ELEVATION-FREQUENCY FOR RESERVOIR ',A6,/)
!
1020  Format('STORAGE-FREQUENCY FOR RESERVOIR ',A6,' FOR MONTH',I3,/)
1030  Format('WATER SURFACE ELEVATION-FREQUENCY FOR RESERVOIR ',A6,
     +       ' FOR MONTH',I3,/)
1040  Format('FLOW-FREQUENCY FOR NATURALIZED STREAMFLOWS',
     +       ' FOR CONTROL POINT ',A6,' FOR MONTH',I3,/)
1050  Format('FLOW-FREQUENCY FOR REGULATED STREAMFLOWS',
     +       ' FOR CONTROL POINT ',A6,' FOR MONTH',I3,/)
1060  Format('FLOW-FREQUENCY FOR UNAPPROPRIATED STREAMFLOWS',
     +       ' FOR CONTROL POINT ',A6,' FOR MONTH',I3,/)
1070  Format('STORAGE-FREQUENCY FOR CONTROL POINT ',A6,
     +       ' FOR MONTH',I3,/)
1080  Format('STORAGE-FREQUENCY FOR WATER RIGHT ',A16,
     +       ' FOR MONTH',I3,/)
1090  Format('FREQUENCY VERSUS INSTREAM FLOW SHORTAGES FOR ',
     +       'WATER RIGHT ',A16,' FOR MONTH',I3,/)
!
!   Format statements for CRM for either
!   2FRE/6FRE or 2FRQ/6FRQ record headings.
!
1110  Format('CONDITIONAL RELIABILITY MODELING:',
     +       ' Equal-Weight Option')
1120  Format('CONDITIONAL RELIABILITY MODELING:',
     +       ' Probability Array Option')
1130  Format('Length of simulation period (CR1) is',I3,' months.')
1140  Format('Initial storage multiplier (CR4) =',F7.3)
1150  Format('Annual cycles starting in month',I2)
1160  Format('Monthly cycle option with',I4,' sequences.')
!
!  Return to main program from Subroutine FREQ.
!
      Deallocate(Q,QTOTAL)
      Return
      End Subroutine FREQ
!
!  ***********************************************************************
!
      Subroutine STORAGE
!
!  *-*-*-*-*-*   2RES Record   *-*-*-*-*-*
!  Subroutine STORAGE develops three tables for up to 20 reservoirs:
!     (1) end-of-month storage expressed as a percentage of capacity
!     (2) reliability table for reservoir drawdowns
!     (3) reliability table for reservoir storage
!  Capacities are user specified. CAPAC(NUM,1) is the capacity of a lower
!  zone such as an inactive pool. CAPAC(NUM,2) is the total capacity such
!  as the capacity of the active conservation pool.
!
      Use COMVAR
!
      Real CAP(20,2),MC(20,9),MC2(20,9),PERC(20),STO(20),STOAVE(20),
     +     AVPERC
!
      Integer COUNT,CREC,I,J,K,L,LOOP,MCR,MON,MONTH,NUM,PERIOD,REC1,
     +        TABLE,YEAR
      Integer:: DVALS,P,Q                                                   !day
!
      Character(len=4) CD
      Character(len=6) IDEN(20),WRAPID
!
!   The WRAP-SIM output file must contain reservoir records.
!
      If(SIMD.EQ.0) Then                                                    !day
         If(NREOUT.LT.0) Then
            Write(20,*) ' ERROR: The SIM output file contains no',
     +                  ' reservoir output records.'
            Call ERROR
         Endif
      Else                                                                  !day
         If(NREOUT2.LT.0) Then                                              !day
            Write(20,*) ' ERROR: The SIMD SUB output file contains no',     !day
     +                  ' reservoir output records.'                        !day
            Call ERROR                                                      !day
         Endif                                                              !day
      Endif                                                                 !day
!
!   Output table specifications are read from input file (unit=1).
!
      Read(1,10,IOSTAT=STATUS) CD,TABLE,MON,NUM
10    Format(A4,3I4)
      If(STATUS.NE.0) Then
         Write(20,20) CD
20       Format(' ERROR: Fortran IOSTAT error occured reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(MON.GT.NPRDS) Then
         Write(20,30) MON,NPRDS
30       Format(' WARNING: MON of',I3,' is greater than NPRDS of',I3)
      Endif
      If(NUM.GT.20) Then
         Write(20,40) NUM
40       Format(' ERROR: NUM of',I3,' exceeds limit of 20 reservoirs.')
         Call ERROR
      Elseif (NUM.LT.1) Then
         Write(20,50) CD
50       Format(' ERROR: A ',A4,' record must have at',
     +               ' least one reservoir.')
         Call ERROR
      Endif
      If(TABLE.LT.0.or.TABLE.GT.4) Then
         Write(20,60) TABLE
60       Format(' ERROR: TABLE of',I4,' in field 2 is not valid.')
         Call ERROR
      Endif
      Backspace(1)
      Read(1,70) CD,TABLE,MON,NUM,(IDEN(I),I=1,NUM)
70    Format(A4,3I4,20(2X,A6))
      Do I=1,NUM
         IDEN(I)=Adjustr(IDEN(I))
      End Do
!
!   Reservoir storage capacities defining top and bottom of zone are read.
!
      Read(1,80) CD
80    Format(A4)
      Backspace(1)
      If((SIMD.EQ.0.and.CD.NE.'2RES').or.                                   !day
     +   (SIMD.EQ.1.and.CD.NE.'6RES')) Then                                 !day
         Write(20,90)
90       Format(' ERROR: Reservoir storage capacities are missing.')
         Call ERROR
      Endif
      Read(1,100) (CAP(I,2),I=1,NUM)
100   Format(16X,20F8.0)
      Read(1,80) CD
      Backspace(1)
      If((SIMD.EQ.0.and.CD.NE.'2RES').or.                                   !day
     +   (SIMD.EQ.1.and.CD.NE.'6RES')) Then                                 !day
         Do I=1,NUM
            CAP(I,1)=0.0
         End Do
      Else
         Read(1,100) (CAP(I,1),I=1,NUM)
      Endif
!
!   Set default for SUB file
!
      If(SIMD.EQ.1.and.TABLE.LE.1) TABLE=4                                  !day
!
!   Title records are written by Subroutine TITLES.
!
      Call TITLES
!
!   Headings for storage percentage table.
!
      If(TABLE.EQ.0.or.TABLE.EQ.1) Then
         If(MON.LE.0) Then
            Write(2,110)
110         Format('END-OF-PERIOD RESERVOIR STORAGE AS A ',
     +             'PERCENTAGE OF CAPACITY')
         Else
            Write(2,120) MON
120         Format('END-OF-PERIOD RESERVOIR STORAGE AS A ',
     +             'PERCENTAGE OF CAPACITY FOR MONTH',I3)
         Endif
         Write(2,130)
130      Format(/10X,'Percentage = 100% * (S - C2) / (C1 - C2)',3X,
     +          'where',/14X,'     S = end-of-month storage',/14X,
     +          ' C1,C2 = user defined top and bottom of storage zone'/)
         Write(2,140) ('--------',I=1,NUM)
140      Format(22('-'),20A8)
         If(CR1.EQ.0) Then
            Write(2,150) (IDEN(I),I=1,NUM)
150         Format('YEAR  MONTH',6X,'MEAN',1X,20(2X,A6))
         Else
            Write(2,160) (IDEN(I),I=1,NUM)
160         Format(' SEQ  MONTH',6X,'MEAN',1X,20(2X,A6))
         Endif
         Write(2,140) ('--------',I=1,NUM)
      Endif
!
!   Since the SIM?SIMD output file is read as a direct access file, record
!   counters are devised for use in locating the records to be read.
!
      COUNT = NUM
      If(SIMD.EQ.0) Then                                                    !day
         CREC = NWROUT+NCPTS+NREOUT
         REC1 = 6+NWROUT+NCPTS
      Else                                                                  !day
         CREC = NWROUT2+NCPO2+NREOUT2                                       !day
         REC1 = 7+NWROUT2+NCPO2                                             !day
      Endif                                                                 !day
!
!   Variables are initialized.
!
      YEAR=YRST
      MONTH=0
      AVPERC=0
      STOAVE=0
      L=0
      MC=0
      MC2=0
      RECD=REC1-1
      If(SIMD.EQ.0) Then                                                    !day
         P=MONTHS                                                           !day
         Q=NREOUT                                                           !day
         UO=4                                                               !day
      Else                                                                  !day
         P=DAYS                                                             !day
         Q=NREOUT2                                                          !day
         DVALS=0                                                            !day
         UO=10                                                              !day
      Endif                                                                 !day
!
!   Beginning of the outer loop.
!
      Do 280 PERIOD=1,P                                                     !day
!
         AVPERC = 0
         MONTH = MONTH+1
         If(SIMD.EQ.0) Then                                                 !day
            MCR=MONTH
            If(CR1.GT.0) Then
               MCR=MONTH+CR2-1
               If(MCR.GT.12) MCR=MCR-12
            Endif
         Else                                                               !day
            MCR=DDATA(PERIOD,2)                                             !day
         Endif                                                              !day
         L=L+1
!
!   Beginning of the inner loop.
!
         Do 260 LOOP=1,COUNT
!
!   The record of the SIM output file (unit = 4 or 10) is found from
!   which to read the storage for the first period (month or day).
!
            If(MON.LE.0.or.MCR.EQ.MON) Then
               RECD=REC1+(L-1)*CREC-1
               Do 230 I = 1,Q                                               !day
                  RECD=RECD+1
                  If(OUTFORM.EQ.1) Then
                     Read(UO,REC=RECD) WRAPID
                  Else
                     Read(UO,210,REC=RECD) WRAPID
210                  Format(A6)
                  Endif
                  If(WRAPID.EQ.IDEN(LOOP)) Goto 240
                  If(I.EQ.Q) Then                                           !day
                     Write(20,220) IDEN(LOOP)
220                  Format(' ERROR: Reservoir identifier ',A6,
     +                      ' is not in SIM output file.')
                     Write(20,*) ' '
                     Call ERROR
                  Endif
230            End Do
!
!   The storage volume is read.
!
240            If(OUTFORM.EQ.1) Then
                  Read(UO,REC=RECD) C1,(XO(K),K=1,3),STO(LOOP)
               Else
                  Read(UO,250,REC=RECD) STO(LOOP)
250               Format(39x,F9.0)
               Endif
               If(SIMD.NE.0.and.LOOP.EQ.1) DVALS=DVALS+1                    !day
!
!   Storage content as a percentage of storage capacity.
!
               PERC(LOOP) = 100*(STO(LOOP)-CAP(LOOP,1))/(CAP(LOOP,2)-
     +                      CAP(LOOP,1))
               AVPERC = AVPERC+PERC(LOOP)
!
!   If a conditional reliability modeling (CRM) SFF relationship is not used.
!
               If(CRSFF.EQ.0.or.SIMD.EQ.1) Then                             !day
                  STOAVE(LOOP) = STOAVE(LOOP)+STO(LOOP)
!
!   Drawdown count.
!
                  If(TABLE.EQ.0.or.TABLE.EQ.2.or.TABLE.EQ.4) Then
                     If((100-PERC(LOOP)).GE.0) MC(LOOP,1) = MC(LOOP,1)+1
                     If((100-PERC(LOOP)).GE.2) MC(LOOP,2) = MC(LOOP,2)+1
                     If((100-PERC(LOOP)).GE.5) MC(LOOP,3) = MC(LOOP,3)+1
                     If((100-PERC(LOOP)).GE.10) MC(LOOP,4)= MC(LOOP,4)+1
                     If((100-PERC(LOOP)).GE.25) MC(LOOP,5)= MC(LOOP,5)+1
                     If((100-PERC(LOOP)).GE.50) MC(LOOP,6)= MC(LOOP,6)+1
                     If((100-PERC(LOOP)).GE.75) MC(LOOP,7)= MC(LOOP,7)+1
                     If((100-PERC(LOOP)).GE.90) MC(LOOP,8)= MC(LOOP,8)+1
                     If((100-PERC(LOOP)).GE.100)MC(LOOP,9)= MC(LOOP,9)+1
                  Endif
!
!   Storage reliability.
!
                  If(TABLE.EQ.0.or.TABLE.GE.3) Then
                     If(PERC(LOOP).GE.100.0)MC2(LOOP,1) = MC2(LOOP,1)+1
                     If(PERC(LOOP).GE.98.0) MC2(LOOP,2) = MC2(LOOP,2)+1
                     If(PERC(LOOP).GE.95.0) MC2(LOOP,3) = MC2(LOOP,3)+1
                     If(PERC(LOOP).GE.90.0) MC2(LOOP,4) = MC2(LOOP,4)+1
                     If(PERC(LOOP).GE.75.0) MC2(LOOP,5) = MC2(LOOP,5)+1
                     If(PERC(LOOP).GE.50.0) MC2(LOOP,6) = MC2(LOOP,6)+1
                     If(PERC(LOOP).GE.25.0) MC2(LOOP,7) = MC2(LOOP,7)+1
                     If(PERC(LOOP).GE.10.0) MC2(LOOP,8) = MC2(LOOP,8)+1
                     If(PERC(LOOP).GE.0.0)  MC2(LOOP,9) = MC2(LOOP,9)+1
                  Endif
!
!   Else if a conditional reliability modeling (CRM) SFF relationship is used.
!
               Else
                 STOAVE(LOOP) = STOAVE(LOOP)+STO(LOOP)*EXPP(YEAR-YRST+1)
!
!   Drawdown frequency.
!
                  If(TABLE.EQ.0.or.TABLE.EQ.2.or.TABLE.EQ.4) Then
                     If((100-PERC(LOOP)).GE.0) MC(LOOP,1) = MC(LOOP,1)+
     +                                                EXPP(YEAR-YRST+1)
                     If((100-PERC(LOOP)).GE.2) MC(LOOP,2) = MC(LOOP,2)+
     +                                                EXPP(YEAR-YRST+1)
                     If((100-PERC(LOOP)).GE.5) MC(LOOP,3) = MC(LOOP,3)+
     +                                                EXPP(YEAR-YRST+1)
                     If((100-PERC(LOOP)).GE.10) MC(LOOP,4) = MC(LOOP,4)+
     +                                                EXPP(YEAR-YRST+1)
                     If((100-PERC(LOOP)).GE.25) MC(LOOP,5) = MC(LOOP,5)+
     +                                                EXPP(YEAR-YRST+1)
                     If((100-PERC(LOOP)).GE.50) MC(LOOP,6) = MC(LOOP,6)+
     +                                                EXPP(YEAR-YRST+1)
                     If((100-PERC(LOOP)).GE.75) MC(LOOP,7) = MC(LOOP,7)+
     +                                                EXPP(YEAR-YRST+1)
                     If((100-PERC(LOOP)).GE.90) MC(LOOP,8) = MC(LOOP,8)+
     +                                                EXPP(YEAR-YRST+1)
                     If((100-PERC(LOOP)).GE.100)MC(LOOP,9) = MC(LOOP,9)+
     +                                                EXPP(YEAR-YRST+1)
                  Endif
!
!   Storage reliability.
!
                  If(TABLE.EQ.0.or.TABLE.GE.3) Then
                     If(PERC(LOOP).GE.100.0) MC2(LOOP,1) = MC2(LOOP,1)+
     +                                                EXPP(YEAR-YRST+1)
                     If(PERC(LOOP).GE.98.0) MC2(LOOP,2) = MC2(LOOP,2)+
     +                                                EXPP(YEAR-YRST+1)
                     If(PERC(LOOP).GE.95.0) MC2(LOOP,3) = MC2(LOOP,3)+
     +                                                EXPP(YEAR-YRST+1)
                     If(PERC(LOOP).GE.90.0) MC2(LOOP,4) = MC2(LOOP,4)+
     +                                                EXPP(YEAR-YRST+1)
                     If(PERC(LOOP).GE.75.0) MC2(LOOP,5) = MC2(LOOP,5)+
     +                                                EXPP(YEAR-YRST+1)
                     If(PERC(LOOP).GE.50.0) MC2(LOOP,6) = MC2(LOOP,6)+
     +                                                EXPP(YEAR-YRST+1)
                     If(PERC(LOOP).GE.25.0) MC2(LOOP,7) = MC2(LOOP,7)+
     +                                                EXPP(YEAR-YRST+1)
                     If(PERC(LOOP).GE.10.0) MC2(LOOP,8) = MC2(LOOP,8)+
     +                                                EXPP(YEAR-YRST+1)
                     If(PERC(LOOP).GE.0.0) MC2(LOOP,9) = MC2(LOOP,9)+
     +                                                EXPP(YEAR-YRST+1)
                  Endif
               Endif
            Endif
260      End Do
!
!   A line is written to the percentage table.
!
         If(TABLE.LE.1) Then
            If(MON.LE.0.or.MON.EQ.MCR) Then
               AVPERC = AVPERC/Real(NUM)
               Write(2,270) YEAR,MCR,AVPERC,(PERC(I),I=1,COUNT)
270            Format(I4,2X,I5,3X,21(F8.2))
            Endif
         Endif
!
!  Year and month are incremented at end of the year or CRM sequence.
!
         If(MONTH.EQ.NPRDS) Then
            MONTH=0
            YEAR=YEAR+1
         Endif
280   End Do
!
!   End of storage percentage table.
!
      If(TABLE.LE.1) Write(2,140) ('--------',I=1,NUM)
!
!   Accumulated storage is converted to mean storage.
!
      If(SIMD.EQ.0) Then                                                    !day
         Do I=1,COUNT
            If(CRSFF.EQ.0) Then
               If(MON.LE.0) Then
                  STOAVE(I) = STOAVE(I)/(Real(MONTHS))
               Else
                  STOAVE(I) = STOAVE(I)/(Real(MONTHS/NPRDS))
               Endif
            Else
               If(MON.LE.0) Then
                  STOAVE(I) = STOAVE(I)/(Real(NPRDS))
               Else
                  STOAVE(I) = STOAVE(I)
               Endif
            Endif
         End Do
      Else                                                                  !day
         Do I=1,COUNT                                                       !day
            STOAVE(I) = STOAVE(I)/(Real(DVALS))                             !day
         End Do                                                             !day
      Endif                                                                 !day
!
!   The reservoir drawdown frequency table is written.
!
      If(TABLE.EQ.0.or.TABLE.EQ.2.or.TABLE.EQ.4) Then
         If(TABLE.EQ.0) Write(2,290) Char(12)
290      Format(A1)
         If(MON.LE.0) Then
            Write(2,300)
            If(SIMD.EQ.1) Call SIMDHEADER                                   !day
         Else
            Write(2,310) MON
            If(SIMD.EQ.1) Call SIMDHEADER                                   !day
         Endif
         If(CR1.GT.0.and.CRHEAD.GE.0.and.SIMD.EQ.0) Then                    !day
            If(CRSFF.EQ.0) Write(2,320)
            If(CRSFF.GT.0) Write(2,330)
            If(CR2.GT.0) Write(2,360) CR2
            If(CR2.EQ.0) Write(2,370) NYRS
            Write(2,340) CR1
            If(CR4.GT.0.0) Write(2,350) CR4
            Write(2,*) ' '
         Endif
         Write(2,390)
         Write(2,400)
         Write(2,410)
         Write(2,420) UNIT,UNIT,UNIT
         Write(2,390)
!
         Do I=1,COUNT
            If(CRSFF.EQ.0.or.SIMD.EQ.1) Then                                !day
               Write(2,430) Adjustl(IDEN(I)),STOAVE(I),CAP(I,1),
     +                         CAP(I,2),(MC(I,J),J=1,9)
            Else
               If(MON.LE.0) Then
                  Write(2,430) Adjustl(IDEN(I)),STOAVE(I),CAP(I,1),
     +                         CAP(I,2),(MC(I,J)*MONTHS/NPRDS,J=1,9)
               Else
                  Write(2,430) Adjustl(IDEN(I)),STOAVE(I),CAP(I,1),
     +                         CAP(I,2),(MC(I,J)*MONTHS/NPRDS,J=1,9)
               Endif
            Endif
         End Do
         Write(2,390)
      Endif
!
!   The reservoir storage reliability table is written.
!
      If(TABLE.EQ.0.or.TABLE.GE.3) Then
         If(TABLE.EQ.0.or.TABLE.EQ.4) Write(2,290) Char(12)
         If(MON.LE.0) Then
            Write(2,460)
            If(SIMD.EQ.1) Call SIMDHEADER                                   !day
         Else
            Write(2,470) MON
            If(SIMD.EQ.1) Call SIMDHEADER                                   !day
         Endif
         If(CR1.GT.0.and.CRHEAD.GE.0.and.SIMD.EQ.0) Then                    !day
            If(CRSFF.EQ.0) Write(2,320)
            If(CRSFF.GT.0) Write(2,330)
            If(CR2.GT.0) Write(2,360) CR2
            If(CR2.EQ.0) Write(2,370) NYRS
            Write(2,340) CR1
            If(CR4.GT.0.0) Write(2,350) CR4
            Write(2,*) ' '
         Endif
!
         Write(2,390)
         If(SIMD.EQ.0) Then                                                 !day
            Write(2,480)
         Else                                                               !day
            Write(2,482)                                                    !day
         Endif                                                              !day
         Write(2,490)
         Write(2,500) UNIT,UNIT,UNIT
         Write(2,390)
!
         Do I=1,COUNT
            If(CRSFF.EQ.0.or.SIMD.EQ.1) Then                                !day
               If(SIMD.EQ.0) Then                                           !day
                  If(MON.LE.0) Then
                     Write(2,510) Adjustl(IDEN(I)),STOAVE(I),CAP(I,1),
     +                      CAP(I,2),(MC2(I,J)*100/(Real(MONTHS)),J=1,9)
                  Else
                     Write(2,510) Adjustl(IDEN(I)),STOAVE(I),CAP(I,1),
     +                CAP(I,2),(MC2(I,J)*100/(Real(MONTHS/NPRDS)),J=1,9)
                  Endif
               Else                                                         !day
                  Write(2,510) Adjustl(IDEN(I)),STOAVE(I),CAP(I,1),         !day
     +                      CAP(I,2),(MC2(I,J)*100/(Real(DVALS)),J=1,9)     !day
               Endif                                                        !day
            Else
               If(MON.LE.0) Then
                  Write(2,520) Adjustl(IDEN(I)),STOAVE(I),CAP(I,1),
     +                      CAP(I,2),(MC2(I,J)*100/Real(NPRDS),J=1,9)
               Else
                  Write(2,520) Adjustl(IDEN(I)),STOAVE(I),CAP(I,1),
     +                      CAP(I,2),(MC2(I,J)*100,J=1,9)
               Endif
            Endif
         End Do
         Write(2,390)
      Endif
!
!  CRM headings.
!
300   Format('RESERVOIR STORAGE DRAWDOWN DURATION',/)
310   Format('RESERVOIR STORAGE DRAWDOWN DURATION FOR MONTH',I3,/)
320   Format('CONDITIONAL RELIABILITY MODELING:',
     +       ' Equal-Weight Option')
330   Format('CONDITIONAL RELIABILITY MODELING:',
     +       ' Probability Array Option')
340   Format('Length of simulation period (CR1) is',I3,' months.')
350   Format('Initial storage multiplier (CR4) =',F7.3)
360   Format('Annual cycles starting in month',I2)
370   Format('Monthly cycle option with',I4,' sequences.')
!
!  Headings and format statements.
!
390   Format(118('-'))
400   Format(15X,'MEAN',8X,'BOTTOM',7X,'TOP',11X,'NUMBER OF',
     +       ' PERIODS WITH DRAWDOWNS EQUALING OR EXCEEDING PERCENT')
410   Format('NAME',9X,'STORAGE',6X,'OF ZONE',5X,'OF ZONE',28X,
     +       'OF ZONE STORAGE CAPACITY')
420   Format(13X,'(',A5,')',6X,'(',A5,')',5X,'(',A5,')',7X,'0%',
     +       6X,'2%',6X,'5%',5X,'10%',5X,'25%',5X,'50%',5X,'75%',
     +       5X,'90%',4X,'100%')
430   Format(A6,2X,F12.2,4X,F9.0,3X,F9.0,1X,9(F8.0))
440   Format(A6,2X,F12.2,4X,F9.0,3X,F9.0,1X,9(F8.0))
460   Format('RESERVOIR STORAGE RELIABILITY',/)
470   Format('RESERVOIR STORAGE RELIABILITY FOR MONTH',I3,/)
480   Format(15X,'MEAN',8X,'BOTTOM',7X,'TOP',14X,'PERCENTAGE',
     +       ' OF MONTHS WITH STORAGE EQUALING OR EXCEEDING ')
482   Format(15X,'MEAN',8X,'BOTTOM',7X,'TOP',14X,'PERCENTAGE',              !day
     +       ' OF DAYS WITH STORAGE EQUALING OR EXCEEDING ')                !day
490   Format('NAME',9X,'STORAGE',6X,'OF ZONE',5X,'OF ZONE',25X,
     +       'PERCENTAGE OF STORAGE CAPACITY')
500   Format(13X,'(',A5,')',6X,'(',A5,')',5X,'(',A5,')',5X,'100%',
     +       5X,'98%',5X,'95%',5X,'90%',5X,'75%',5X,'50%',5X,'25%',
     +       5X,'10%',5X,'>0%')
510   Format(A6,2X,F12.2,4X,F9.0,3X,F9.0,1X,9(F8.1))
520   Format(A6,2X,F12.2,4X,F9.0,3X,F9.0,1X,9(F8.1))
!
!  Return to main program from Subroutine STORAGE.
!
      Return
      End Subroutine STORAGE
! 
! *************************************************************************
!
      Subroutine FLOWREC
!
!   *-*-*-*-*-*   3NAT, 3UNA, 3DEP, 3U+D, 3REG, 3EPD Records   *-*-*-*-*-*
!   Subroutine FLOWREC converts data from the SIM OUT file to input records
!   to be read by SIM or other models.
!
      Use COMVAR
      Real DATA(12),INFAC,UDATA
      Integer CONPT,CREC,HEC,I,L,MONTH,REC1,YEAR,YRLAST
      Character(len=2) CDOUT
      Character(len=4) CD
      Character(len=6) CPID
      Character(len=18) IUNIT
!
!   Read and check Program TABLES input record.
!
      Write(2,10) Char(12)
10    Format(A)
      Read(1,20,IOSTAT=STATUS) CD,CDOUT,HEC,INFAC
20    Format(A4,2x,A2,I4,F8.0)
      If(STATUS.NE.0) Then
         Write(20,30) CD
30       Format(' ERROR: Fortran IOSTAT error occured reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(INFAC.EQ.0.0) INFAC = 1.0
!
!   Read line 5 of SIM output file (unit=4) and initialize counters.
!
      If(OUTFORM.EQ.1) Then
         Read(4,REC=5) YRST,NYRS,NCPTS,NWROUT,NREOUT
      Else
         Read(4,40,REC=5) YRST,NYRS,NCPTS,NWROUT,NREOUT
40       Format(5I6)
      Endif
      NPRDS=12
      YEAR = YRST-1
      YRLAST = YRST+NYRS-1
      MONTH = 0
      CREC = NCPTS+NWROUT+NREOUT
      CONPT = 0
!
!  Error checks.
!
      If(CD.EQ.'3EPD'.and.NREOUT.LE.0) Then
         Write(20,50)
50       Format(' ERROR: 3EPD record requires reservoir',
     +          ' records in SIM output file.')
         Call ERROR
      Endif
      If(CD.NE.'3EPD'.and.NCPTS.LE.0) Then
         Write(20,60) CD
60       Format(' ERROR: ',A4,' record requires control',
     +          ' point records in SIM output file.')
         Call ERROR
      Endif
!
!   *-*-*-*-*-*   3NAT, 3UNA, 3DEP, 3REG  or 3EPD Record   *-*-*-*-*-*
!   Convert naturalized streamflows (3NAT), unappropriated flows (3UNA),
!   regulated flows (3REG) streamflow depletions (3DEP) or evaporation
!   depths (3EPD) to input records for all control points.
!
      If (CD.EQ.'3EPD') Then
         REC1= 6+NWROUT+NCPTS
      Else
         REC1 = 6+NWROUT
      Endif
170   RECD = REC1
!
!   Records grouped by year.
!   Begin nested loops for month, control point, and year.
!
      If(HEC.EQ.1) Then
         YEAR = YEAR+1
180      CONPT = CONPT+1
190      MONTH = MONTH+1
         If(OUTFORM.EQ.1) Then
            If(CD.EQ.'3NAT') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,7),DATA(MONTH)
            Elseif (CD.EQ.'3UNA') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,5),DATA(MONTH)
            Elseif (CD.EQ.'3DEP') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,4),DATA(MONTH)
            Elseif (CD.EQ.'3REG') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,8),DATA(MONTH)
            Elseif (CD.EQ.'3U+D') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,5),DATA(MONTH)
               UDATA=DATA(MONTH)
               Read(4,REC=RECD) CPID,(XO(I),I=1,4),DATA(MONTH)
               DATA(MONTH)=DATA(MONTH)+UDATA
            Elseif (CD.EQ.'3EPD') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,8),DATA(MONTH)
            Endif
         Else
            If(CD.EQ.'3NAT') Then
               Read(4,200,REC=RECD) CPID,DATA(MONTH)
200            Format(A6,77x,F11.0)
            Elseif (CD.EQ.'3UNA') Then
               Read(4,210,REC=RECD) CPID,DATA(MONTH)
210            Format(A6,55X,F11.0)
            Elseif (CD.EQ.'3DEP') Then
               Read(4,220,REC=RECD) CPID,DATA(MONTH)
220            Format(A6,44x,F11.0)
            Elseif (CD.EQ.'3REG') Then
               Read(4,230,REC=RECD) CPID,DATA(MONTH)
230            Format(A6,88x,F11.0)
            Elseif (CD.EQ.'3U+D') Then
               Read(4,210,REC=RECD) CPID,DATA(MONTH)
               UDATA=DATA(MONTH)
               Read(4,220,REC=RECD) CPID,DATA(MONTH)
               DATA(MONTH)=DATA(MONTH)+UDATA
            Elseif (CD.EQ.'3EPD') Then
               Read(4,240,REC=RECD) CPID,DATA(MONTH)
240            Format(A6,88x,F11.0)
            Endif
         Endif
         RECD=RECD+CREC
         If(MONTH.LT.12) Goto 190
         Do I=1,12
            DATA(I) = DATA(I) * INFAC
         End Do
         IUNIT='(A2,A6,I8,12F8.0)'
         Do L=1,12
            If(DATA(L).LT.100.0.and.DATA(L).GE.0.05)
     +         IUNIT='(A2,A6,I8,12F8.1)'
         End Do
         If (CD.EQ.'3EPD') Then
            IUNIT='(A2,A6,I8,12F8.4)'
         Endif
         Write(2,IUNIT) CDOUT,CPID,YEAR,(DATA(I),I=1,12)
         REC1 = REC1+1
         RECD = REC1
         MONTH = 0
         If(CD.EQ.'3EPD') Then
            If (CONPT.LT.NREOUT) Goto 180
            REC1 = 6+NWROUT+NCPTS+CREC*12*(YEAR-YRST+1)
            CONPT = 0
         Else
            If (CONPT.LT.NCPTS) Goto 180
            REC1 = 6+NWROUT+CREC*12*(YEAR-YRST+1)
            CONPT = 0
         Endif
         If(YEAR.LT.YRLAST) Goto 170
      Endif
!
!   Records grouped by control points.
!   Begin nested loops for month, year, and control point.
!
      If(HEC.EQ.0) Then
280      CONPT = CONPT+1
         YEAR = YRST-1
290      YEAR = YEAR+1
300      MONTH = MONTH+1
         If(OUTFORM.EQ.1) Then
            If(CD.EQ.'3NAT') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,7),DATA(MONTH)
            Elseif (CD.EQ.'3UNA') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,5),DATA(MONTH)
            Elseif (CD.EQ.'3DEP') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,4),DATA(MONTH)
            Elseif (CD.EQ.'3REG') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,8),DATA(MONTH)
            Elseif (CD.EQ.'3U+D') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,5),DATA(MONTH)
               UDATA=DATA(MONTH)
               Read(4,REC=RECD) CPID,(XO(I),I=1,4),DATA(MONTH)
               DATA(MONTH)=DATA(MONTH)+UDATA
            Elseif (CD.EQ.'3EPD') Then
               Read(4,REC=RECD) CPID,(XO(I),I=1,8),DATA(MONTH)
            Endif
         Else
            If(CD.EQ.'3NAT') Read(4,200,REC=RECD) CPID,DATA(MONTH)
            If(CD.EQ.'3UNA') Read(4,210,REC=RECD) CPID,DATA(MONTH)
            If(CD.EQ.'3DEP') Read(4,220,REC=RECD) CPID,DATA(MONTH)
            If(CD.EQ.'3REG') Read(4,230,REC=RECD) CPID,DATA(MONTH)
            If(CD.EQ.'3U+D') Then
               Read(4,210,REC=RECD) CPID,DATA(MONTH)
               UDATA=DATA(MONTH)
               Read(4,220,REC=RECD) CPID,DATA(MONTH)
               DATA(MONTH)=DATA(MONTH)+UDATA
            Endif
            If (CD.EQ.'3EPD') Read(4,240,REC=RECD) CPID,DATA(MONTH)
         Endif
         RECD = RECD + CREC
         If(MONTH.LT.12) Goto 300
         Do I=1,12
            DATA(I) = DATA(I)*INFAC
         End Do
         IUNIT='(A2,A6,I8,12F8.0)'
         Do L=1,12
            If(DATA(L).LT.100.0.and.DATA(L).GE.0.05)
     +         IUNIT='(A2,A6,I8,12F8.1)'
         End Do
         If (CD.EQ.'3EPD') Then
            IUNIT='(A2,A6,I8,12F8.4)'
         Endif
         Write(2,IUNIT) CDOUT,CPID,YEAR,(DATA(I),I=1,12)
         MONTH = 0
         If(YEAR.LT.YRLAST) Goto 290
         REC1 = REC1+1
         RECD = REC1
         If(CD.EQ.'3EPD') Then
            If (CONPT.LT.NREOUT) Goto 280
            REC1 = 6+NWROUT+NCPTS+CREC*12*(YEAR-YRST+1)
            CONPT = 0
         Else
            If (CONPT.LT.NCPTS) Goto 280
            REC1 = 6+NWROUT+CREC*12*(YEAR-YRST+1)
            CONPT = 0
         Endif
      Endif
!
!  Return to main program from Subroutine FLOWREC.
!
      Return
      End Subroutine FLOWREC
!
!**************************************************************************
!
      Subroutine SYSTAB
!
!   *-*-*-*-*-* 4HRR Record *-*-*-*-*-*
!   Subroutine SYSTAB develops tables of hydroelectric energy and reservoir 
!   releases for each reservoir associated with specified water rights.
!
      Use COMVAR
      Character(len=1) CHR
      Character(len=4) CD
      Character(len=6) SYSRES(50),TMPRES(50)
      Character(len=16) WRAPID
!
      Real HE,HET,HEYR,HETYR,RELMON(50),RELYR(50),TMPREL(50)
      Integer HR,MNAN,NUM,YEAR,MT,I,J,K,N,LAST,COUNT,LOOP,NUMRES
!
!   Initialize variables
!
      TMPRES='      '
      SYSRES='      '
      HE=0.0
      HET=0.0
      HEYR=0.0
      HETYR=0.0
      RELMON=0.0
      RELYR=0
!
!   Start a new page and add heading from TITL records.
!
      Call TITLES
!
!   First year, number of years, and number of rights from HRR file (unit=5).
!
      Rewind(5)
10    Format(A)
      Do I=1,4
         Read(5,10) CHR
       End Do
      Read(5,20) YRST,NYRS,NWROUT
20    Format(2I6,6x,I6)
!
!   Output table specifications from TABLES input TIN file (unit=1).
!
      Read(1,30,IOSTAT=STATUS) CD,MNAN,HR,NUM
30    Format(A4,3I4)
      If(STATUS.NE.0) Then
         Write(20,40) CD
40       Format(' ERROR: Fortran IOSTAT error occured reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(NUM.GT.0) Then
         Backspace(1)
         K = 1
         N = NUM
         If(NUM.GT.5) N=5
50       Read(1,60) (IDEN16(I), I=K,N)
60       Format(16x,5A16)
         If(NUM.GT.N) Then
            K = K+5
            N = N+5
            If(N-NUM.LT.0) N=NUM
            Goto 50
         Endif
         Do I=1,NUM
            IDEN16(I)=Adjustr(IDEN16(I))
         End Do
      Endif
      NUM=Abs(NUM)
!
!   Error checks.
!
      If(MNAN.LT.0.or.MNAN.GT.2) Then
         Write(20,70) MNAN
70       Format(' ERROR: MNAN of',I3,' on 4HRR record is not valid')
         Call ERROR
      Endif
      If(NUM.LT.0.or.NUM.GT.30) Then
         Write(20,80) NUM
80       Format(' ERROR: NUM  of',I3,' on 4HRR record is not valid')
         Call ERROR
      Endif
!
!   The SIM hydropower and reservoir release HRR file (unit=5) is read.
!   Records are selected by comparing water right identifiers.
!
!   ++++++++++  Begin Loop  ++++++++++
!   Beginning of monthly loop to develop tables for COUNT water rights.
!
90    COUNT=NUM
      LOOP = 0
100   LOOP = LOOP+1
!
!   Start a new TOU file page, rewind HRR file, and skip header lines.
!
      If(LOOP.GT.1) Write(2,10) Char(12)
      Rewind(5)
      Do I=1,6
         Read(5,10) CHR
      End Do
!
!   Determine if the SIM HRR file contains the TIN file water right.
!
      Read(5,110) WRAPID
110   Format(A16)
      WRAPID=Adjustr(WRAPID)
      N=1
120   If(WRAPID.NE.IDEN16(LOOP)) Then
         N=N+1
         Read(5,110) WRAPID
         WRAPID=Adjustr(WRAPID)
         If(N.LE.NWROUT) Goto 120
         Write(20,130) IDEN16(LOOP), CD
130      Format(' ERROR:Identifier ',A16,' from ',A4,' record',
     +             ' was not found in HRR file.')
         Call ERROR
      Endif
!
!   Determine all of the reservoirs associated with the water right.
!   NUMRES=J is the number of reservoirs connected to the water right.
!
      Backspace(5)
      Read(5,140,IOSTAT=STATUS) WRAPID,J,YEAR,MT,HET,HE,
     +                          (TMPREL(I),TMPRES(I),I=1,J)
140   Format(A16,I3,I6,I3,2F10.0,30(F10.0,1x,A6))
      If(STATUS.NE.0) Then
         Write(20,40) CD
         Call ERROR
      Endif
      Do I=1,J
         SYSRES(I)=Adjustr(TMPRES(I))
      End Do
      NUMRES=J
      Backspace(5)
!
!   Table headings.
!
      LAST=NUMRES
      If(HR.LE.1) Then
         If(MNAN.LE.1) Then
            Write(2,150) Adjustl(IDEN16(LOOP))
150         Format('MONTHLY RELEASES FROM SYSTEM RESERVOIRS ',
     +             'FOR WATER RIGHT ',A16,/)
            Write(2,160) (SYSRES(I),I=1,LAST)
160         Format('YEAR  MT',15(4x,A6))
         Else
            Write(2,170) Adjustl(IDEN16(LOOP))
170         Format('ANNUAL RELEASES FROM SYSTEM RESERVOIRS ',
     +             'FOR WATER RIGHT ',A16,/)
            Write(2,180) (SYSRES(I),I=1,LAST)
180         Format('YEAR',20(4x,A6))
         Endif
      Else
         Write(2,190) Adjustl(IDEN16(LOOP))
190      Format('HYDROELECTRIC ENERGY AND RESERVOIR RELEASES ',
     +          'FOR WATER RIGHT ',A8,/)
         If(MNAN.LE.1) Then
            Write(2,200) (SYSRES(I),I=1,LAST)
200         Format('YEAR  MT',4x,'TARGET GENERATED',15(4x,A6))
         Else
            Write(2,210) (SYSRES(I),I=1,LAST)
210         Format('YEAR',4x,'TARGET GENERATED',20(4x,A6))
         Endif
      Endif
      Write(2,220)
220   Format('  ')
!
!   A line of data is read from the HRR file.
!
230   Read(5,240,IOSTAT=STATUS) WRAPID,J,YEAR,MT,HET,HE,
     +                          (RELMON(I),TMPRES(I),I=1,J)
240   Format(A16,I3,I6,I3,2F10.0,30(F10.0,1x,A6))
      If(STATUS.NE.0) Then
         Write(20,40) CD
         Call ERROR
      Endif
!
!   Monthly or annual tables that do not include hydroelectric energy.
!
      If(HR.LE.1) Then
         If(MNAN.LE.1) Then
            Write(2,250) YEAR,MT,(RELMON(I),I=1,LAST)
250         Format(2I4,30F10.1)
         Else
            Do I=1,NUMRES
               RELYR(I)=RELYR(I)+RELMON(I)
            End Do
            If(MT.EQ.12) Then
               Write(2,260) YEAR,(RELYR(I),I=1,LAST)
260            Format(I4,30F10.1)
               Do I=1,NUMRES
                  RELYR(I)=0.
               End Do
            Endif
         Endif
!
!   Monthly or annual tables that include hydroelectric energy.
!
      Else
         If(MNAN.LE.1) Then
            Write(2,270) YEAR,MT,HET,HE,(RELMON(I),I=1,LAST)
270         Format(2I4,2F10.1,30F10.1)
         Else
            HEYR=HEYR+HE
            HETYR=HETYR+HET
            Do I=1,NUMRES
               RELYR(I)=RELYR(I)+RELMON(I)
            End Do
            If(MT.EQ.12) Then
               Write(2,280) YEAR,HETYR,HEYR,(RELYR(I),I=1,LAST)
280            Format(I4,2F10.1,30F10.1)
               HEYR=0.0
               HETYR=0.0
               Do I=1,NUMRES
                  RELYR(I)=0.
               End Do
            Endif
         Endif
      Endif
!
!   Repeat for this water right for the next month.
!
      If(YEAR.EQ.YRST+NYRS-1.and.MT.EQ.12) Goto 400
      Do I=1,NWROUT-1
         Read(5,10) CHR
      End Do
      Goto 230
!
!   ++++++++++  End Loop  ++++++++++
!   Repeat loop for next water right.
!
400   If(LOOP.LT.COUNT) Goto 100
!
!  Return to main program from Subroutine SYSTAB.
!
      Return
      End Subroutine SYSTAB
!
!************************************************************************
!
      Subroutine ZZZZ
!
!  *-*-*-*-*-*  4ZZZ and 4ZZF Records  *-*-*-*-*-*
!  Subroutine ZZZZ is called by Subroutines ZZFLOW and ZZFREQ to read and
!  store the information from the ZZZ file generated by a SIM ZZ record.
!  Subroutine ZZZ is called only once, and the ZZZ file data is stored in
!  memory for shared use by any number of 4ZZZ and 4ZZF records.
!
      Use COMVAR
!
      Integer I,J,M,MT,NM,WR,WRZ,Z,ZSPFLAG
      Integer,Allocatable,Dimension(:)::WRCOUNT,ZSP
      Character(len=4) CD
      Character(len=16) WRIDZ
      Character(len=16),Allocatable,Dimension(:)::WRIDZZZ
!
      Write(20,10)
10    Format('*** Starting to read ZZZ file.')
!
!  Header data are read.
!
      Do I=1,3
         Read(17,20,IOSTAT=STATUS) CD
20       Format(A4)
         If(STATUS.NE.0) Then
            Write(20,30)
30          Format(' ERROR: Fortran IOSTAT error reading',
     +             ' the heading at beginning of ZZZ file.')
            Call ERROR
         Endif
      End Do
      Read(17,40,IOSTAT=STATUS) YR1,NYR
40    Format(31x,I6,I4)
      If(STATUS.NE.0) Then
         Write(20,50)
50       Format(' ERROR: Fortran IOSTAT error reading the first',
     +          ' year and number of years from ZZZ file.')
         Call ERROR
      Endif
      Backspace(17)
      Read(17,60) YRSTDSS
60    Format(33x,A4)
      Read(17,70) NWR,ZZ
70    Format(42x,I5,I4)
      Do I=1,7
         Read(17,80) CD
80       Format(A4)
      End Do
!
!  Months with second pass through water rights loop are identified.
!
      NM=NYR*12
      Allocate(ZSP(NM))
      ZSP=0
      ZSPFLAG=0
100   Read(17,80,End=130) CD
      If(CD.NE.'END ') Then
         Backspace(17)
         Read(17,110,IOSTAT=STATUS,End=130) WRIDZ,M
110      Format(8x,A16,I4)
         If(STATUS.NE.0) Then
            Write(20,120)
120         Format(' ERROR: Fortran IOSTAT error during initial',
     +             ' reading of water right and month in ZZZ file.')
            Call ERROR
         Endif
         If(WRIDZ.EQ.'*** Beginning 2P') ZSP(M)=2
         Goto 100
      Endif
130   Rewind(17)
      Do I=1,12
         Read(17,140) CD
140      Format(A4)
      End Do
!
!  The number of water rights in the ZZZ file are counted,
!  The number of water rights ZZWRNUM found in the ZZZ file
!  is usually less than the number NWR in the SIM DAT file.
!  Integer identifiers WR for water rights found in the ZZZ
!  file are recorded as array ZZCOUNT(NWR) and 16-character
!  identifiers WRIDZ are recorded as WRIDZZZ(NWR).
!
      Allocate(WRCOUNT(NWR),WRIDZZZ(NWR))
      WRCOUNT=-9
      Do 200 MT=1,NM
150      Read(17,160,End=210) CD
160      Format(A4)
         If(CD.EQ.'End ') Goto 210         
         Backspace(17)
         Read(17,170,IOSTAT=STATUS,End=190) WRIDZ,M,WR
170      Format(8x,A16,I4,I5)
         If(STATUS.NE.0) Then
            Write(20,180)
180         Format(' ERROR: IOSTAT error counting water',
     +             ' rights in ZZZ file.')
            Call ERROR
         Endif
!
         If(ZSP(M).EQ.2.and.WR.EQ.0) Then
            ZSPFLAG=99
            If(WRIDZ.EQ.'*** Beginning 2P') ZSPFLAG=0
         Endif
         If(ZSPFLAG.EQ.99) Goto 150
!
         If(WR.GT.0) Then
            WRCOUNT(WR)=WR
            WRIDZZZ(WR)=WRIDZ
         Endif
         If(M.GT.MT) Goto 200
         Goto 150
190      Write(20,*)' WARNING: Reached end of ZZZ file inappropriately.'
200   End Do
!
!  The counter ZZWRNUM includes the number of water rights found in one
!  or more months in the ZZZ file plus one representing the beginning.
!
210   ZZWRNUM=1
      J=1
      Do I=1,NWR
         If(WRCOUNT(I).GE.0) Then
            J=J+1
            ZZWRNUM=ZZWRNUM+1
         Endif
      End Do
!
!  Arrays are allocated and initialized.
!
      NM=NYR*12
      Allocate(ZZCP(ZZ),ZZWRI(NWR),ZZWR(ZZWRNUM))
      Allocate(ZZF(NM,ZZWRNUM,ZZ,3))
      ZZCP='      '
      ZZWR='                '
      ZZWRI=-9
      ZZF=-9.0
      If(ZZFLAG.GT.0) Then
         Allocate(ZPLOT(NM,100))
         ZPLOT=0.0
      Endif
!
!  Water right identifiers are initially read from the ZZZ file as
!  WRCOUNT(NWR) and WRIDZZZ(NWR). ZZWRI(NWR) connects the new integer
!  identifiers (sequenced from 1 to ZZWRNUM) to the original integer
!  identifiers WR read from the ZZZ file and stored as WRCOUNT(NWR).
!  The 16-character WRIDZZZ(NWR) are converted to ZZWR(ZZWRNUM).
!
      ZZWRI(1)=0
      ZZWR(1)='*** Beginning **'
      J=1
      Do I=1,NWR
         If(WRCOUNT(I).GE.0) Then
            J=J+1
            ZZWRI(J)=WRCOUNT(I)
            ZZWR(J)=WRIDZZZ(I)
         Endif
      End Do
!
!  The first 7 lines at the beginning of the ZZZ file are skipped
!  allowing the control point identifiers to be read from the 8th
!  line.  Lines 9-12 are then skipped allowing the actual reading
!  of data to begin at the 13th record.
!
      Rewind(17)
      Do I=1,7
         Read(17,220,IOSTAT=STATUS) CD
220      Format(A4)
         If(STATUS.NE.0) Then
            Write(20,110)
            Call ERROR
         Endif
      End Do
      Read(17,230) (ZZCP(Z),Z=1,ZZ)
230   Format(34x,<ZZ>(12x,A6,12x))
      Do I=1,4
         Read(17,240) CD
240      Format(A4)
      End Do
!
!  ZZZ file flow data are read.
!
      ZSPFLAG=0
      Do MT=1,NM
         WRZ=0
!
300      Read(17,240,End=420) CD
         If(CD.EQ.'End ') Then
            WR=0
            M=0
            Goto 330
         Endif
         Backspace(17)
!
         Read(17,310,IOSTAT=STATUS,End=420) WRIDZ,M,WR
310      Format(8x,A16,I4,I5)
         If(STATUS.NE.0) Then
            Write(20,320)
320         Format(' ERROR: IOSTAT error reading water right',
     +             ' and month from ZZZ file.')
            Call ERROR
         Endif
!
         If(ZSP(M).EQ.2.and.WR.EQ.0) Then
            ZSPFLAG=99
            If(WRIDZ.EQ.'*** Beginning 2P') ZSPFLAG=0
         Endif
         If(ZSPFLAG.EQ.99) Goto 300
         Backspace(17)
!
330      WRZ=WRZ+1
         If(WR.EQ.ZZWRI(WRZ)) Then
            Read(17,340,IOSTAT=STATUS) WRIDZ,M,(ZZF(MT,WRZ,Z,1),
     +                     ZZF(MT,WRZ,Z,2),ZZF(MT,WRZ,Z,3),Z=1,ZZ)
340         Format(8x,A16,I4,5x,<ZZ>(3F10.0))
            If(STATUS.NE.0) Then
               Write(20,350)
350            Format(' ERROR: Fortran IOSTAT error reading',
     +                ' flow data from ZZZ file.')
               Call ERROR
            Endif
            If(WRIDZ.NE.ZZWR(WRZ).and.WRIDZ.NE.'*** Beginning 2P') Then
               Write(20,360) WRIDZ,ZZWR(WRZ)
360            Format(' ERROR: Following water right identifiers read',
     +                ' from ZZZ file should be same.',/,8x,A16,5x,A16)
               Call ERROR
            Endif
            If(MT.NE.M) Then
               Write(20,370) M,MT
370            Format(' ERROR: Month M of',I4,' read from ZZZ file',
     +                ' should be',I4,'.')
               Call ERROR
            Endif
            If(WRZ.LT.ZZWRNUM) Goto 300
         Else
            If(WRZ.LE.1) Then
               Write(20,380) WRIDZ,M,WR,MT,WRZ
380            Format(' ERROR: A beginning row with WR=0 appears to be',
     +                ' missing in ZZZ file in row with Water Right,'
     +                ' M, and WR as follows MT,WRZ.',/,8x,A16,3x,4I5)
               Call ERROR
            Endif
            Do Z=1,ZZ
               ZZF(MT,WRZ,Z,1)=ZZF(MT,WRZ-1,Z,1)
               ZZF(MT,WRZ,Z,2)=ZZF(MT,WRZ-1,Z,2)
               ZZF(MT,WRZ,Z,3)=ZZF(MT,WRZ-1,Z,3)
            End Do
            If(WRZ.LT.ZZWRNUM) Goto 330
         Endif
      End Do
!
!   Return from Subroutine ZZZZ to Subroutine ZZFLOW or ZZFREQ.
!
400   Write(20,410)
410   Format('*** Finished reading ZZZ file.')
      Return
420   Write(20,430) MT
430   Format('*** Reached end of ZZZ file reading data for month',I4,
     +       '.')
      Write(20,410)
      Return
      End Subroutine ZZZZ
!
!************************************************************************
!
      Subroutine ZZFLOW
!
!  *-*-*-*-*-*  4ZZZ Record  *-*-*-*-*-*
!  Subroutine ZZFLOW develops TOU file tables or DSS file records for
!  priority loop available flows, regulated flows, and reservoir releases
!  read from a ZZZ file generated by a SIM ZZ record. The ZZZ file is read
!  by Subroutine ZZZZ which is called by Subroutines ZZFLOW and ZZFREQ.
!
      Use COMVAR
!
      Real MDATA(12),MEAN(13),SUM(13),TDATA,YTOTAL
!
      Integer CP,I,IP,L,MONTH,MORE,MYR,MM,MT,NM,NN,NUM,
     +        PERIOD,PT,TA,VAR,WRI,YEAR,Z
      Integer IPLAN,ISTAT,NDSS,NPATH,NVALS
!
      Character(len=2)  DSSDAY
      Character(len=3)  M(23)
      Character(len=4)  CD,CTIME,CTYPE
      Character(len=5)  CUNITS
      Character(len=8)  HEAD(100,3)
      Character(len=9)  CDATE
      Character(len=16) RIGHT
      Character(len=32) A,B,C,D,E,F
      Character(len=64) CPATH,CNAME
!
      MDATA=0.0
      MEAN=0.0
      SUM=0.0
!
!  The ZZZ file is read if it has not already been read.
!
      If(ZZZFILE.EQ.0) Then
         ZZZFILE=99
         Call ZZZZ
      Endif
      NM=NYR*12
!
!   Table specifications are read from the input file (unit=1) record.
!
      Read(1,100,IOSTAT=STATUS) CD,TA,PT,MORE,VAR,NUM,RIGHT
100   Format(A4,5I4,A16)
      If(VAR.EQ.0) VAR=3
      RIGHT=Adjustl(RIGHT)
      If(RIGHT.EQ.'BEGIN           '.or.RIGHT.EQ.'begin           '.or.
     +   RIGHT.EQ.'Begin           ') Then
         RIGHT='*** Beginning **'
         WRI=1
      Endif
!
!     Input error checks.
!
      If(STATUS.NE.0) Then
         Write(20,110) CD
110      Format(' ERROR: Fortran IOSTAT error reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(PT.GT.5.or.PT.LT.0) Then
         Write(20,120) PT
120      Format(' ERROR: PT of',I3,' in 4ZZZ field 3 is not valid.')
         Call ERROR
      Endif
      If(VAR.LT.1.or.VAR.GT.3) Then
         Write(20,130) VAR
130      Format(' ERROR: VAR of',I3,' in 4ZZZ field 5 is not valid.')
         Call ERROR
      Endif
      If(NUM.GT.ZZ) Then
         Write(20,140) NUM,ZZ
140      Format(' ERROR: NUM of',I3,' in 4ZZZ field 6 exceeds ZZ of',I3)
         Call ERROR
      Endif
!
!  With NUM greater than zero, control point identifiers are read
!  by Subroutine IDEN from IDEN records.
!
      If(NUM.GT.0) Then
         TID=0
         NID=NUM
         Call IDEN
         Do I=1,NUM
            Do Z=1,ZZ
               If(IDCP(I).EQ.ZZCP(Z)) Goto 160
            End Do
            Write(20,150) IDCP(I)
150         Format(' ERROR: Control point ',A6,' from IDEN record',
     +             ' matches no control point in ZZZ file.')
            Call ERROR
160      End Do
      Endif
      NUM=Abs(NUM)
!
!  With NUM of zero, frequency tables are created for all the
!  control points in the ZZZ file.
!
      If(NUM.EQ.0) Then
         NUM=ZZ
         Do Z=1,ZZ
            IDCP(Z)=ZZCP(Z)
         End Do
      Endif
!
!  Water right integer identifier WRI is set.
!
      Do I=1,ZZWRNUM
         If(RIGHT.EQ.ZZWR(I)) Then
            WRI=I
            Go to 180
         Endif
      End Do
      Write(20,170) RIGHT
170   Format(' ERROR: Water right ',A16,' from 4ZZZ record matches no',
     +       ' water right in ZZZ file.')
      Call ERROR
!
!  HEC-DSS file is opened and array allocated.
!
180   If(PT.EQ.4.or.PT.EQ.5) Then
         HECDSS=HECDSS+1
!
!  HEC-DSS file VALUES array is allocated.
!
      Endif
!
!   The order in which months are listed in the table headings is set based
!   on MONTH1 specified in the UNIT record, with a default of MONTH1=JAN.
!
      L=1
      If(MONTH1.EQ.'  JAN'.or.MONTH1.EQ.'  Jan') L=1
      If(MONTH1.EQ.'  FEB'.or.MONTH1.EQ.'  Feb') L=2
      If(MONTH1.EQ.'  MAR'.or.MONTH1.EQ.'  Mar') L=3
      If(MONTH1.EQ.'  APR'.or.MONTH1.EQ.'  Apr') L=4
      If(MONTH1.EQ.'  MAY'.or.MONTH1.EQ.'  May') L=5
      If(MONTH1.EQ.'  JUN'.or.MONTH1.EQ.'  Jun') L=6
      If(MONTH1.EQ.'  JUL'.or.MONTH1.EQ.'  Jul') L=7
      If(MONTH1.EQ.'  AUG'.or.MONTH1.EQ.'  Aug') L=8
      If(MONTH1.EQ.'  SEP'.or.MONTH1.EQ.'  Sep') L=9
      If(MONTH1.EQ.'  OCT'.or.MONTH1.EQ.'  Oct') L=10
      If(MONTH1.EQ.'  NOV'.or.MONTH1.EQ.'  Nov') L=11
      If(MONTH1.EQ.'  DEC'.or.MONTH1.EQ.'  Dec') L=12
      M(1) ='JAN'
      M(2) ='FEB'
      M(3) ='MAR'
      M(4) ='APR'
      M(5) ='MAY'
      M(6) ='JUN'
      M(7) ='JUL'
      M(8) ='AUG'
      M(9) ='SEP'
      M(10)='OCT'
      M(11)='NOV'
      M(12)='DEC'
      M(13)='JAN'
      M(14)='FEB'
      M(15)='MAR'
      M(16)='APR'
      M(17)='MAY'
      M(18)='JUN'
      M(19)='JUL'
      M(20)='AUG'
      M(21)='SEP'
      M(22)='OCT'
      M(23)='NOV'
!
!   ++++++++++  Begin Control Point Loop  ++++++++++
!   Beginning of loop to develop tables for the ZZ control points,
!
      CP=0
500   CP=CP+1
!
!   Variable initialization.
!
      Do I=1,13
         SUM(I)=0.0
      End Do
      NDSS=0
!
!   Headings for table with annual rows and monthly columns.
!
      If(TA.GE.1) Then
         Call TITLES
         If(VAR.EQ.1) Write(2,510)UNIT,Adjustl(IDCP(CP)),Adjustl(RIGHT)
         If(VAR.EQ.2) Write(2,520)UNIT,Adjustl(IDCP(CP)),Adjustl(RIGHT)
         If(VAR.EQ.3) Write(2,530)UNIT,Adjustl(IDCP(CP)),Adjustl(RIGHT)
         Write(2,540)
         Write(2,550) 'YEAR',M(L),M(L+1),M(L+2),M(L+3),M(L+4),M(L+5),
     +               M(L+6),M(L+7),M(L+8),M(L+9),M(L+10),M(L+11),'TOTAL'
         Write(2,560)
      Endif
510   Format('RESERVOIR RELEASES (',A5,') AT CONTROL POINT ',A6,
     +       ' AFTER WATER RIGHT ',A16)
520   Format('REGULATED STREAMFLOWS (',A5,') AT CONTROL POINT ',A6,
     +       ' AFTER WATER RIGHT ',A16)
530   Format('AVAILABLE FLOWS (',A5,') AT CONTROL POINT ',A6,
     +       ' AFTER WATER RIGHT ',A16)
540   Format(/,127('-'))
550   Format(A4,8x,A3,11(6x,A3),7x,A5)
560   Format(127('-'))
!
!   Increment column counter (MPLOT) and develop heading array for plot table.
!
      If(PT.EQ.1.or.PT.EQ.2.or.PT.EQ.3) Then
         MPLOT=MPLOT+1
         If(VAR.EQ.1) Then
            HEAD(MPLOT,1)=' RES REL'
         Elseif(VAR.EQ.2) Then
            HEAD(MPLOT,1)='REG FLOW'
         Else
            HEAD(MPLOT,1)='AVAIL FL'
         Endif
         HEAD(MPLOT,2)=IDCP(CP)
         HEAD(MPLOT,3)=Adjustr(RIGHT(1:8))
      Endif
!
!  ++++++++++  Begin Inner Loop For Periods ++++++++++
!   Begin loop which is repeated for each of N=NYR*12 periods (months).
!
      PERIOD=0
      MONTH=0
      YEAR=YR1
      YTOTAL=0.0
      Do 610 MT=1,NM
         PERIOD=PERIOD+1
         MONTH=MONTH+1
         If(MONTH.EQ.1) MDATA=0.0
!
!  Flow data TDATA is obtained from ZZZ file array ZZF.
!
         TDATA=ZZF(MT,WRI,CP,VAR)
!
!  Totals for month (January-December) and year.
!
         MDATA(MONTH)=MDATA(MONTH)+TDATA
         YTOTAL=YTOTAL+MDATA(MONTH)
!
!  Values for DSS file.
!
         If(PT.EQ.4) Then
            NDSS=NDSS+1
            VALUES(NDSS)=MDATA(MONTH)
         Endif
         If(PT.EQ.5) Then
            If(MONTH.EQ.12) Then
               NDSS=NDSS+1
               VALUES(NDSS)=YTOTAL
            Endif
         Endif
!
!  Write a row in regular table.
!
         If(MONTH.EQ.12) Then
            If(TA.GE.1) Then
               Write(2,600) YEAR,(MDATA(I),I=1,12),YTOTAL
600            Format(I4,3X,12F9.1,F12.1)
            Endif
!
!  Develop 12 months (a year) of a column of plot table array.
!
            If(PT.EQ.1) Then
               Do I=1,12
                  IP=PERIOD-12+I
                  ZPLOT(IP,MPLOT)=MDATA(I)
               End Do
            Endif
            If(PT.EQ.2) Then
               MYR=YEAR-YR1+1
               ZPLOT(MYR,MPLOT)=YTOTAL
            Endif
!
!  Compute means for each month (January-December) and year if
!  monthly/annual data are finished or otherwise go to next month.
!
            Do I=1,12
               SUM(I)=SUM(I)+MDATA(I)
            End Do
            SUM(13)=SUM(13)+YTOTAL
            YTOTAL=0.0
            MONTH=0
            YEAR=YEAR+1
         Endif
!
!  End of monthly period loop.
!
610   End Do
!
!  Means are computed.
!
      Do I=1,12
         MEAN(I)=SUM(I)/NYR
      End Do
      MEAN(13)=SUM(13)/NYR
!
!  Means are placed as last row of regular table.
!
      If(TA.GE.1) Then
         Write(2,620) (MEAN(I),I=1,13)
620      Format('MEAN',3x,12F9.1,F12.1)
         Write(2,560)
      Endif
!
!  Means are placed in plot array.
!
      If(PT.EQ.3) Then
         Do I=1,12
            ZPLOT(I,MPLOT)=MEAN(I)
         End Do
      Endif
!
!  DSS data is written to the HEC-DSS file.
!
      If(PT.EQ.4.or.PT.EQ.5) Then
         DSSDAY='01'
         If(DSSMON.EQ.'   ') Then
            CDATE=DSSDAY//M(L)//YRSTDSS
         Else
            CDATE=DSSDAY//DSSMON//YRSTDSS
         Endif
         CDATE=DSSDAY//M(L)//YRSTDSS
         CTIME='0000'
         CUNITS=UNIT
         CTYPE=CD
         IPLAN=0
!
!     DSS pathname /A/B/C/D/E/F/ is defined.
!
         A=OROOT
         B=Adjustr(IDCP(CP))
         If(VAR.EQ.1) Then
            C='ZZ_RES_REL'
         Elseif(VAR.EQ.2) Then
            C='ZZ_REG_FLOW'
         Else
            C='ZZ_AVAIL_FL'
         Endif
         D=CDATE
         If(PT.EQ.4) Then
            E='1MON'
         Elseif(PT.EQ.5) Then
            E='1YEAR'
         Endif
         F=Adjustr(RIGHT)
!
!     DSS routines are called.
!
*         Call ZPATH(A,B,C,D,E,F,CPATH,NPATH)
*         Call ZCHKPN(CPATH,NPATH,ISTAT)
         If(ISTAT.NE.0) Then
            Write(20,630) ISTAT,Adjustl(CPATH)
630         Format(' ERROR: DSS ISTAT error',I3,' occurred',
     +             ' for DSS pathname: ',A80)
            Call ERROR
         Endif
*         Call ZSRTS(IFLTAB,CPATH,CDATE,CTIME,NVALS,VALUES,
*     +              CUNITS,CTYPE,IPLAN,ISTAT)
         If(ISTAT.NE.0) Then
            Write(20,640) ISTAT
640         Format(' ERROR: DSS ISTAT error',I3,' occurred',
     +             ' writing data to DSS file.')
            Call ERROR
         Endif
      Endif
!
!  Start over with the next control point.
!
      If(CP.LT.NUM.and.PT.LE.0) Goto 500
      If(CP.LT.NUM.and.PT.GE.1.and.MPLOT.LT.100) Goto 500
!
!  The HEC-DSS file VALUES array is deallocated.
!
      If(PT.EQ.4.or.PT.EQ.5) Deallocate(VALUES)
!
!  The plot table is written.
!
      If(((MPLOT.GE.1.and.MORE.EQ.0).or.MPLOT.EQ.100))Then
         Call TITLES
         Write(2,650) (HEAD(I,1),I=1,MPLOT)
650      Format(/,8x,100(2x,A8))
         Write(2,660) (Adjustr(HEAD(I,2)),I=1,MPLOT)
660      Format(8x,100(2x,A8))
         Write(2,660) (Adjustr(HEAD(I,3)),I=1,MPLOT)
         Write(2,670)
670      Format('  ')
         YEAR=YR1-1
         MM=0
         If(PT.EQ.1) Then
            Do MYR=1,NYR
               YEAR=YEAR+1
               Do MT=1,12
                  MM=MM+1
                  Write(2,680) YEAR,MT,(ZPLOT(MM,NN),NN=1,MPLOT)
680               Format(1x,I4,I3,100(F10.1))
               End Do
            End Do
         Elseif(PT.EQ.2) Then
            Do MYR=1,NYR
               YEAR=YEAR+1
               Write(2,690) YEAR,(ZPLOT(MYR,NN),NN=1,MPLOT)
690            Format(1X,I4,3x,100(F10.1))
            End Do
         Elseif(PT.EQ.3) Then
            Do MT=1,12
               Write(2,700) MT,(ZPLOT(MT,NN),NN=1,MPLOT)
700            Format(1X,4x,I3,100(F10.1))
            End Do
         Endif
         MPLOT=0
         If(CP.LT.NUM) Goto 500
      Endif
!
!  Return to main program from Subroutine ZZFLOW.
!
      Return
      End Subroutine ZZFLOW
!
!************************************************************************
!
      Subroutine ZZFREQ
!
!  *-*-*-*-*-*  4ZZF Record  *-*-*-*-*-*
!  Subroutine ZZFREQ develops frequency tables for priority loop flows
!  generated by SIM with a ZZ record and stored in a ZZZ file. The ZZZ
!  file is read by Subroutine ZZZZ which is called by Subroutines
!  ZZFLOW and ZZFREQ.
!
      Use COMVAR
!
      Real DXF,MEAN,STDDEV,SUM,SUMSD,TEMP,XF
      Real F(10),QFREQ(10)
      Real,Allocatable,Dimension(:)::Q
!
      Integer CP,IF1,IF2,I,J,K,M,MON,NM,NQ,NUM,VAR,Z
!
      Character(len=4) CD
      Character(len=16) ZZWRID
!
      Logical SORTED
!
!  Frequenies included in frequency table.
!
      F(1)=0.99
      F(2)=0.98
      F(3)=0.95
      F(4)=0.90
      F(5)=0.75
      F(6)=0.60
      F(7)=0.50
      F(8)=0.40
      F(9)=0.25
      F(10)=0.10
!
!  Subroutine ZZZZ is called to read the ZZZ file if it has not
!  already been read with a preceding 4ZZF or 4ZZZ record.
!
      If(ZZZFILE.EQ.0) Then
         ZZZFILE=99
         Call ZZZZ
      Endif
!
!  Specifications for building the frequency table are read from
!  the 4ZZF record (unit=1).
!
      Read(1,10,IOSTAT=STATUS) CD,VAR,MON,NUM
10    Format(A4,4I4)
      If(VAR.EQ.0) VAR=3
!
!  Input error checks.
!
      If(STATUS.NE.0) Then
         Write(20,20) CD
20       Format(' ERROR: Fortran IOSTAT error occurred reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(VAR.LT.1.or.VAR.GT.3) Then
         Write(20,30) VAR
30       Format(' ERROR: VAR of',I3,' in 4ZZF field 2 is not valid.')
         Call ERROR
      Endif
      If(MON.LT.0.or.MON.GT.12) Then
         Write(20,40) MON
40       Format(' ERROR: MON of',I3,' in 4ZZF field 3 is not valid.')
         Call ERROR
      Endif
      If(NUM.GT.ZZ) Then
         Write(20,50) NUM,ZZ
50       Format(' ERROR: NUM of',I3,' in 4ZZF field 4 exceeds ZZ of',I3)
         Call ERROR
      Endif
!
!  With NUM greater than zero, control point identifiers are read
!  by Subroutine IDEN from IDEN records.
!
      If(NUM.GT.0) Then
         TID=0
         NID=NUM
         Call IDEN
         Do I=1,NUM
            Do Z=1,ZZ
               If(IDCP(I).EQ.ZZCP(Z)) Goto 70
            End Do
            Write(20,60) IDCP(I)
60          Format(' ERROR: Control point ',A6,' from IDEN record',
     +             ' matches no control point in ZZZ file.')
            Call ERROR
70       End Do
      Endif
      NUM=Abs(NUM)
!
!  With NUM of zero, frequency tables are created for all the
!  control points in the ZZZ file.
!
      If(NUM.EQ.0) Then
         NUM=ZZ
         Do Z=1,ZZ
            IDCP(Z)=ZZCP(Z)
         End Do
      Endif
!
!  Total number of months NM and number of flows per year NQ are set.
!  Flow array Q(NQ) is allocated.
!
      NM=NYR*12
      If(MON.GE.1) Then
         NQ=NYR
      Else
         NQ=NM
      Endif
      Allocate(Q(NQ))
      Q=0.0
!
!  ++++++++++++++ Beginning of Control Point Loop ++++++++++++++
!  Beginning of loop to develop tables for the NUM control points.
!
      CP=0
100   CP=CP+1
!
!  Table headings are written.
!
      Call TITLES
      If(VAR.EQ.1) Write(2,410) IDCP(CP)
      If(VAR.EQ.2) Write(2,420) IDCP(CP)
      If(VAR.EQ.3) Write(2,430) IDCP(CP)
      If(MON.GT.0) Write(2,440) MON
      Write(2,*) ' '
      Write(2,400)
      Write(2,450)
      Write(2,460)
      Write(2,400)
!
!  ++++++++++++++ Beginning of Water Right Loop ++++++++++++++
!  Beginning of loop to develop ZZWRNUM rows of data in a frequency table.
!  First row in frequency table corresponds to beginning of priority loop.
!  Each subsequent row in the table corresponds to a water right.
!
      Do 310 K=1,ZZWRNUM
!
!  Q(I) is either reservoir releases (VAR=1), regulated flows (VAR=2), or
!  available flows (VAR=3) read from the ZZZ file as ZZF(NM,ZZWRNUM,ZZ,3).
!
         I=0
         J=0
         Do M=1,NM
            J=J+1
            If(J.EQ.13) J=1
            If(MON.EQ.0.or.MON.EQ.J) Then
               I=I+1
               Q(I)=ZZF(M,K,CP,VAR)
            Endif
         End Do
!
!  Q(I) is sorted in descending order.
!
         SORTED=.FALSE.
200      If(.NOT.SORTED) Then
            SORTED=.TRUE.
            Do I=1,NQ-1
               If(Q(I).LT.Q(I+1)) Then
                  TEMP=Q(I)
                  Q(I)=Q(I+1)
                  Q(I+1)=TEMP
                  SORTED=.FALSE.
               Endif
            End Do
            Goto 200
         Endif
!
!  Mean and standard deviation are computed.
!
         SUM=0
         SUMSD=0.0
         Do I=1,NQ
            SUM=SUM+Q(I)
         End Do
         MEAN=SUM/NQ
         Do I=1,NQ
            SUMSD=SUMSD+(Q(I)-MEAN)**2
         End Do
         STDDEV=(SUMSD/(NQ-1))**0.5
!
!  Flows QFREQ(I) for each specified frequency F(I) are determined.
!
         Do I=1,10
            XF=F(I)*Real(NQ)
            IF1=INT(XF)
            IF2=IF1+1
            DXF=XF-Real(IF1)
            If(IF1.GT.0.and.IF2.GT.0) Then
               QFREQ(I)=(Q(IF1)-Q(IF2))*(1.0-DXF)+Q(IF2)
            Else
               QFREQ(I)=Q(1)
            Endif
         End Do
!
!  Row of table is written corresponding to beginning or a water right.
!
         If(ZZWR(K).EQ.'*** Beginning **') Then
            ZZWRID='Beginning       '
         Else
            ZZWRID=Adjustl(ZZWR(K))
         Endif
         Write(2,300) ZZWRI(K),ZZWRID,MEAN,STDDEV,Q(NQ),
     +                (QFREQ(I),I=1,10),Q(1)
300      Format(I4,2x,A16,F10.1,F8.0,6F8.1,4F8.0,2F9.0)
!
!  End of water right loop that adds rows to the frequency table.
!
310   End Do
      Write(2,400)
!
! ++++++++++++++++  End of Control Point Loop  +++++++++++++++++++
!
      If(CP.LT.NUM) Goto 100
!
!  Format statements for table headings.
!
400   Format(138('-'))
410   Format('FREQUENCY TABLE FOR RESERVOIR RELEASES AT CONTROL POINT ',
     +       A6)
420   Format('FREQUENCY TABLE FOR REGULATED FLOWS AT CONTROL POINT ',A6)
430   Format('FREQUENCY TABLE FOR AVAILABLE FLOWS AT CONTROL POINT ',A6)
440   Format('for Month',I3)
!
450   Format(6x,'WATER',22x,'STANDARD',6x,'PERCENTAGE OF MONTHS WITH ',
     +       'FLOWS EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')
!
460   Format('  WR  RIGHT',16x,'MEAN DEVIATION   100%',4x,'99%',5x,
     +       '98%',5x,'95%',5x,'90%',5x,'75%',5x,'60%',5x,'50%',5x,
     +       '40%',5x,'25%',6x,'10%   MAXIMUM')
!
!  Return to main program from Subroutine ZZFREQ.
!
      Deallocate(Q)
      Return
      End Subroutine ZZFREQ
!
!************************************************************************
!
      Subroutine CRMSFF
!
!  *-*-*-*-*-* 5CR1 and 5COR Records *-*-*-*-*-*
!  Subroutine CRMSFF develops a Storage-Flow-Frequency (SFF) array for ratios
!  of simulated-to-predicted flows to be used later in Subroutine CRMIPA that
!  develops a probability array for a specified initial storage condition for
!  the Conditional Reliability Model (CRM).  The SFF array can be stored in a
!  SFF file or in memory as array SFFARR.
!
      Use COMVAR
      Use CRMVAR
!
      Integer I,J,K,KX,KY,NN,NCPTS7,NREOUT7,NWROUT7,NYRS7,NUM,P,POS,REC
      Real A2,FLOWX,MAXF,MAXP,MAXS,MEANF,MEANP,MEANS,MINF,MINP,MINS,
     +     R,RC,RR,STORE,SUMX,SDF,SDP,SDS
      Real(8) SXX,SX,SXY,SY,SYY,X,XC,XMEAN,YMEAN
      Character(len=4) CD,CD1,CD2
      Character(len=6) TEMP_ID,NAME2
!
      Integer,allocatable::RX(:),RY(:)
      Real,Allocatable,Dimension(:)::FLOWP,FLOWSPER,FLOWT,RANKX,RANKY,
     +                     STORAGEP,STORAGET,TOTAL_FLOW,TOTAL_STORAGE
      Real,Allocatable,Dimension(:,:)::QFREQ2
!
!  First 5CR1 or 5COR record is read from TIN file.
!
      SFFARRAYF=0
      Write(*,*)
      If(CR1F.GT.0) Then
         Read(1,10,IOSTAT=STATUS) CD,NFLOW,NSTOR,TCR2,FM,DIST,FIT,
     +                    INTZERO,AA,BB,CC,LOWLIM,UPLIM,FILE2
10       Format(A4,7I4,5F8.0,I4)
         If(STATUS.NE.0) Then
            Write(20,20) STATUS
20          Format(' ERROR: Fortran IOSTAT error occurred reading',
     +             ' first 5CR1 record',/,'   *** IOSTAT status',
     +             ' variable (error code) ='I4)
            Call ERROR
         Endif
      Else
         Read(1,30,IOSTAT=STATUS) CD,NFLOW,NSTOR,TCR2,FM,FILE2
30       Format(A4,5I4)
         If(STATUS.NE.0) Then
            Write(20,40) STATUS
40          Format(' ERROR: Fortran IOSTAT error occurred reading',
     +             ' first 5COR record',/,'   *** IOSTAT status',
     +             ' variable (error code) =,'I4)
            Call ERROR
         Endif
         FIT=0
         FILE2=0
      Endif
!
!  Cycling specification variables.
!
      If(FM.EQ.0.and.CR1.NE.0) FM=CR1
      TCR1=FM
      CR1=TCR1
      If(TCR2.EQ.0.and.CR2.NE.0) TCR2=CR2
      CR2=TCR2
      If(TCR2.LT.0.or.TCR2.GT.12) Then
         Write(20,50) TCR2
50       Format(' ERROR: TRC2 of',I3,' is not valid.')
         Call ERROR
      Endif
      If(FM.LT.0.or.FM.GT.1500) Then
         Write(20,60) FM
60       Format(' ERROR: FM of',I3,' is not valid.')
         Call ERROR
      Endif
!
!  Error checks for first 5CR1 or 5COR record.
!
      If(NSTOR.LT.-1.or.NSTOR.GT.15) Then
         Write(20,70) NSTOR
70       Format(' ERROR: Number of control points for reservoir',
     +          ' storage NSTOR of',I3,' is not valid.')
         Call ERROR
      Endif
      If(NFLOW.LT.0.or.NFLOW.GT.15) Then
         Write(20,80) NFLOW
80       Format(' ERROR: Number of control points for naturalized',
     +          ' flows NFLOW of',I3,' is not valid.')
         Call ERROR
      Endif
      If(FILE2.LT.0.or.FILE2.GT.4) Then
         Write(20,90) FILE2
90       Format(' ERROR: FILE2 of',I3,' is not valid.')
         Call ERROR
      Endif
!
!  CR1F=9 specifies 5CR1 record procedures not applicable to 5COR record.
!
      If(CR1F.GT.0) Then
!
!  5CR1 record error checks.
!
         If(DIST.LT.0.or.DIST.GT.2) Then
            Write(20,100) DIST
100         Format(' ERROR: DIST on 5CR1 record is not valid:'I3)
            Call ERROR
         Endif
         If(FIT.LT.-4.or.FIT.GT.4) Then
            Write(20,110) FIT
110         Format(' ERROR: FIT on 5CR1 record is not valid:'I3)
            Call ERROR
         Endif
         If((Abs(FIT).LT.2.or.Abs(FIT).GT.3).and.INTZERO.GE.1) Then
            Write(20,120)
120         Format(' ERROR: INTZERO on 5CR1 record applies only to',
     +             ' FIT options 2 and 3.')
            Call ERROR
         Endif
         If(LOWLIM.GT.UPLIM) Then
            Write(20,130)
130         Format(' ERROR: 5CR1 record LOWLIM is greater than UPLIM.')
            Call ERROR
         Endif
         If(UPLIM.EQ.0.0) UPLIM=9999999999.9
      Endif
!
!  Second 5CR1 or 5COR record is read from TIN file.
!
      If(NFLOW.EQ.0.and.(NSTOR.EQ.0.or.NSTOR.EQ.-1)) Then
         Read(1,160,IOSTAT=STATUS) CD1,CD2
160      Format(2A4)
         If(STATUS.NE.0) Then
            Write(20,170) CD,STATUS
170         Format(' ERROR: Fortran IOSTAT error occurred reading',
     +             ' second ',A4,' record',/,'   *** IOSTAT status',
     +             ' variable (error code) =,'I6)
            Call ERROR
         Endif
         If(CD1.EQ.CD) Then
            Write(20,180) CD1
180         Format(' ERROR: Since NFLOW and NSTOR are both zero, a ',
     +             A4,' record cannot be used.')
            Call ERROR
         Endif
      Elseif(NFLOW.GT.0) Then
         Read(1,160,IOSTAT=STATUS) CD1,CD2
         If(STATUS.NE.0) Then
            Write(20,190) STATUS
190         Format(' ERROR: Fortran IOSTAT error occurred reading',
     +             ' second 5CR1 record',/,'   *** IOSTAT status',
     +             ' variable (error code) =,'I6)
            Call ERROR
         Endif
         If(CD1.NE.CD) Then
            Write(20,200) CD,CD1
200         Format(' ERROR: ',A4,' record was expected instead of ',A4)
            Call ERROR
         Endif
         If((CD2.NE.'FLOW')) Then
            Write(20,210) CD,CD2
210         Format(' ERROR: Field 2 of second ',A4,' record should be',
     +                ' FLOW instead of ',A4)
            Call ERROR
         Endif
         Backspace(1)
         Read(1,220)(CPF(I),I=1,NFLOW)
220      Format(8X,15(2X,A6))
      Elseif(NFLOW.EQ.0.and.NSTOR.GT.0) Then
         Read(1,160,IOSTAT=STATUS) CD1,CD2
         If(STATUS.NE.0) Then
            Write(20,170) CD,STATUS
            Call ERROR
         Endif
         If(CD2.EQ.'STCP') Then
            Backspace(1)
            Read(1,220)(CPS(I),I=1,NSTOR)
            RESFLAG=0
         Elseif(CD2.EQ.'STRE') Then
            Backspace(1)
            Read(1,220)(CPS(I),I=1,NSTOR)
            RESFLAG=1
         Else
            Write(20,230) CD,CD2
230         Format(' ERROR: Field 2 of second ',A4,' record should be',
     +             ' STCP or STRE instead of ',A4)
            Call ERROR
         Endif
      Endif
!
!  Third 5CR1 or 5COR record is read from TIN file.
!
      If(NSTOR.EQ.0.or.NSTOR.EQ.-1) Then
         Read(1,240,IOSTAT=STATUS) CD1,CD2
         If(STATUS.NE.0) Then
            Write(20,240) CD,STATUS
240         Format(' ERROR: Fortran IOSTAT error occurred reading',
     +             ' third ',A4,' record',/,'   *** IOSTAT status',
     +             ' variable (error code) =,'I6)
            Call ERROR
         Endif
         If(CD1.EQ.CD) Then
            Write(20,250) CD1
250         Format(' ERROR: Since NSTOR is 0 or -1, a ',A4,' record',
     +             '  can not be used.')
            Call ERROR
         Endif
      Elseif(NFLOW.GT.0.and.NSTOR.GT.0) Then
         Read(1,160,IOSTAT=STATUS) CD1,CD2
         If(STATUS.NE.0) Then
            Write(20,240) CD,STATUS
            Call ERROR
         Endif
         If(CD1.NE.CD) Then
            Write(20,200) CD,CD1
            Call ERROR
         Endif
         If(CD2.EQ.'STCP') Then
            Backspace(1)
            Read(1,220)(CPS(I),I=1,NSTOR)
            RESFLAG=0
         Elseif(CD2.EQ.'STRE') Then
            Backspace(1)
            Read(1,220)(CPS(I),I=1,NSTOR)
            RESFLAG=1
         Else
            Write(20,210) CD,CD2
            Call ERROR
         Endif
      Endif
!
!  Selected variables on 5th record of SIM output OUT file are read.
!  These variables are unique to unit 7 OUT file allowing different
!  values to be read by other records from unit 4 CRM or OUT file.
!
      If(OUTFORM.EQ.1) Then
         Read(7,REC=5,IOSTAT=STATUS) YRST,NYRS7,NCPTS7,NWROUT7,NREOUT7
      Else
         Read(7,260,REC=5,IOSTAT=STATUS) NYRS7,NCPTS7,NWROUT7,NREOUT7
260      Format(6x,4I6)
      Endif
      If(STATUS.NE.0) Then
         Write(20,270) STATUS
270      Format(/,' ERROR: Fortran IOSTAT error with Subroutine',
     +         ' CRMSFF reading 5th record of SIM output OUT file.')
         Call ERROR
      Endif
      If(NCPTS7.LE.0.and.NWROUT7.LE.0.and.NREOUT7.LE.0) Then
         Write(20,280)
280      Format(/,' ERROR: NCPTS7, NWROUT7, and NREOUT7 on 5th record',
     +            ' of SIM output OUT file are all zero.')
         Write(20,120)
         Call ERROR
      Endif
!
!  NYRS7 on 5th record of OUT file is the number of years of the simulation.
!  TNYRS is defined below as the number of years in the simulation.
!  NYRS7 is changed below to the number of CRM hydrologic sequences. NN=NYRS7
!
      TNYRS=NYRS7
      If(CR1.GT.0) Then
         If(CR2.LE.0) Then
            NYRS7=12*NYRS7-CR1+1
         Elseif(CR2.GT.0) Then
            NYRS7=NYRS7-Int((CR1+CR2-2)/12)
         Endif
      Endif
      NN=NYRS7
!
!  NUM is the number of control points or reservoirs for which storage
!  volumes are being added to obtain a combined total volume.
!
      If(NSTOR.EQ.0) Then
         NUM=NCPTS7
      Elseif(NSTOR.EQ.-1) Then
         NUM=NREOUT7
      Else
         NUM=NSTOR
      Endif
!
!  Arrays TOTAL_STORAGE(NYRS7) and TOTAL_FLOW(NYRS7) are the summation of
!  storage and naturalized flow volumes from the OUT file.
!
      If(Allocated(TOTAL_STORAGE)) Deallocate(TOTAL_STORAGE)
      Allocate(TOTAL_STORAGE(NN))
      TOTAL_STORAGE=0.0
      STORE=0.0
      If(Allocated(TOTAL_FLOW)) Deallocate (TOTAL_FLOW)
      Allocate(TOTAL_FLOW(NN))
      TOTAL_FLOW=0.0
      FLOWX=0.0
!
!  Beginning-of-month storage volumes STORE for individual reservoirs are
!  extracted from the SIM output OUT file and added to TOTAL_STORAGE(NN).
!
      POS=1
      Do 330 I=1,NUM
         If(NSTOR.EQ.0) Then
            REC=5+NWROUT7+POS
            Read(7,285,REC=REC) TEMP_ID
285         Format(A6)
            POS=POS+1
         Elseif(NSTOR.EQ.-1) Then
            REC=5+NWROUT7+NCPTS7+POS
            Read(7,285,REC=REC) TEMP_ID
            POS=POS+1
            RESFLAG=1
         Else
            TEMP_ID=CPS(I)
         Endif
!
!  The first OUT file record for the control point or reservoir is found.
!
         NAME2='&&&&&&'
         POS=1
         If(RESFLAG.EQ.1) Then
            REC=5+NWROUT7+NCPTS7
            Do WHILE ((NAME2.NE.TEMP_ID).and.(POS.LE.NREOUT7))
               REC=REC+1
               Read(7,285,REC=REC) NAME2
               POS=POS+1
            Enddo
         Else
            REC=5+NWROUT7
            Do WHILE ((NAME2.NE.TEMP_ID).and.(POS.LE.NCPTS7))
               REC=REC+1
               Read(7,285,REC=REC) NAME2
               POS=POS+1
            Enddo
         Endif
         If(NAME2.NE.TEMP_ID) Then
            Write(20,290) TEMP_ID
290         Format(' ERROR: Reservoir identifier ',A6,' was not',
     +             ' found in SIM output file.')
            Call ERROR
         Endif
!
!  The beginning-of-month storage is the end-of-month storage for the preceding
!  month. The OUT file does not contain the beginning-of-month storage for the
!  first month of a simulation. The beginning-of-simulation storage volume, if
!  needed (CR2=0,1) is approximated as storage at the end of the first month.
!
!  Reservoir storage contents are extracted.
!
         Do J=1,NN
            If(J.EQ.1) Then
               If(CR2.GT.1) Then
                  REC=REC+((CR2-2)*(NWROUT7+NCPTS7+NREOUT7))
               Endif
            Endif
            Read(7,300,REC=REC,IOSTAT=STATUS) NAME2,STORE
300         Format(A6,33X,F11.2)
            If(STATUS.NE.0) Then
               Write(20,310) STATUS
310            Format(' ERROR: Fortran IOSTAT error reading',
     +                ' storage from SIM output file.',/,4x,
     +                '*** IOSTAT status variable (error code) =',I4)
               Call ERROR
            Endif
            If(NAME2.NE.TEMP_ID) Then
               Write(20,320) TEMP_ID
320            Format(' ERROR: Reservoir identifier ',A6,' does not',
     +                ' follow in sequence in SIM output file.')
               Call ERROR
            Endif
            If(J.EQ.1) Then
               If(CR2.LE.1) Then
                  REC=REC+((-1)*(NWROUT7+NCPTS7+NREOUT7))
               Endif
            Endif
            If(CR2.GT.0) Then
               REC=REC+12*(NWROUT7+NCPTS7+NREOUT7)
            Else
               REC=REC+NWROUT7+NCPTS7+NREOUT7
            Endif
            TOTAL_STORAGE(J)=TOTAL_STORAGE(J)+STORE
         Enddo
330   Enddo
!
!  Naturalized flow volumes FLOWX are extracted from SIM
!  output OUT file and summed as the array TOTAL_FLOW(NN).
!
!      The flows are read and summed for NUM control points.
!
      If(NFLOW.EQ.0) Then 
         NUM=NCPTS7
      Else
         NUM=NFLOW
      Endif
      POS=1
      Do I=1,NUM
         If(NFLOW.EQ.0) Then
            REC=5+NWROUT7+POS
            Read(7,285,REC=REC) TEMP_ID
            POS=POS+1
         Else
            TEMP_ID=CPF(I)
         Endif
!
!      The first record for the control point is found in the OUT file.
!
         NAME2='&&&&&&'
         POS=1
         REC=5+NWROUT7
         Do While ((NAME2.NE.TEMP_ID).and.(POS.LE.NCPTS7))
            REC=REC+1
            Read(7,285,REC=REC,IOSTAT=STATUS) NAME2
            If(STATUS.NE.0) Then
               Write(20,340) STATUS,TEMP_ID
340            Format(' ERROR: Fortran IOSTAT error reading control',
     +            ' point identifier ',A6,' from SIM output file.',/,
     +            '    *** IOSTAT status variable (error code) =',I4)
               Call ERROR
            Endif
            POS=POS+1
         End Do
         If(NAME2.NE.TEMP_ID) Then
            Write(20,350) TEMP_ID
350         Format(' ERROR: Control point identifier ',A6,' was not',
     +             ' found in SIM output file.')
            Call ERROR
         Endif
!
!      Naturalized flow volumes are extracted from OUT file.
!
         REC=REC+((CR2-1)*(NWROUT7+NCPTS7+NREOUT7))
         Do J=1,NYRS7
            Do K=1,TCR1
               Read(7,360,REC=REC,IOSTAT=STATUS) NAME2,FLOWX
360            Format(A6,77x,F11.2)
               If(STATUS.NE.0) Then
                  Write(20,340) STATUS,TEMP_ID
                  Call ERROR
               Endif
               If(NAME2.NE.TEMP_ID) Then
                  Write(20,370) TEMP_ID
370               Format(' ERROR: Control point identifier ',A6,
     +                   ' does not follow in sequence in OUT file.')
                  Call ERROR
               Endif
               TOTAL_FLOW(J)=TOTAL_FLOW(J)+FLOWX
               REC=REC+NWROUT7+NCPTS7+NREOUT7
            Enddo
            REC=REC+(12-TCR1)*(NWROUT7+NCPTS7+NREOUT7)
         Enddo
      EndDo
!
!  Storage volumes that are within the limits established by LOWLIM and UPLIM
!  are selected. NN is reduced to count only values falling within the limits.
!
      If(CR1F.GT.0) Then
         Do J=1,NN
            If(TOTAL_STORAGE(J).LT.LOWLIM.or.TOTAL_STORAGE(J).GT.UPLIM) 
     +         Then
               Do while(TOTAL_STORAGE(J).LT.LOWLIM.or.TOTAL_STORAGE(J).
     +                  GT.UPLIM)
                  Do K=J,NN-1
                     TOTAL_STORAGE(K)=TOTAL_STORAGE(K+1)
                     TOTAL_FLOW(K)=TOTAL_FLOW(K+1)
                  EndDo
                  NN=NN-1
                  If(J.EQ.NN+1) Goto 380
               Enddo
            Endif
         EndDo
!
!  Regression can not be performed if less than 2 storage-flow values remain.
!
380      If(NN.LT.2) Then
            Write(20,390)
390         Format(' ERROR: Less than two storages fall within limits.')
            Call ERROR
         Endif
      Endif
!
!  The standard correlation coefficient and the Spearman coefficient for storage
!  versus naturalized flow volume are developed for a 5COR record or for a 5CR1
!  record with FILE2 options 2, 3, or 4.
!
      If(CR1F.EQ.0.or.FILE2.GE.2) Then
!
!  Spearman correlation coefficient.
!
         Allocate(RX(NN),RY(NN))
         Allocate(RANKX(NN),RANKY(NN))
!
         Do I=1,NN
            RX(I)=I
            RY(I)=I
         Enddo
!
!  The elements of RX(NN) and RY(NN) are ranked in order such that:
!         X[RX(1)] <= X[RX(2)] <= X[RX(3)]  etc.
!         Y[RY(1)] <= Y[RY(2)] <= Y[RY(3)]  etc.
!
         Do I=1,NN-1
            Do J=I,NN
               If(TOTAL_STORAGE(RX(I)).GT.TOTAL_STORAGE(RX(J))) Then
                  K=RX(I)
                  RX(I)=RX(J)
                  RX(J)=K
               Endif
               If(TOTAL_FLOW(RY(I)).GT.TOTAL_FLOW(RY(J))) Then
                  K=RY(I)
                  RY(I)=RY(J)
                  RY(J)=K
               Endif
            Enddo
         Enddo
!
!  The ranks are determined.
!  For example, if RX(1)=I, the ith element of X has rank=1.
!
         KY=0
         KX=0
         RANKY(RY(1))=1
         RANKX(RX(1))=1
         Do I=2,NN
            If(TOTAL_FLOW(RY(I)).EQ.TOTAL_FLOW(RY(I-1))) Then
               KY=KY+1
               RANKY(RY(I))=Real(I-KY)
            Else
               KY=0
               RANKY(RY(I))=Real(I)
            Endif
            If(TOTAL_STORAGE(RX(I)).EQ.TOTAL_STORAGE(RX(I-1))) Then
               KX=KX+1
               RANKX(RX(I))=Real(I-KX)
            Else
               KX=0
               RANKX(RX(I))=Real(I)
            Endif
         Enddo
!
!  Spearman correlation coefficient is computed by applying linear
!  correlation to the rank arrays.
!
         If(NN.LE.1) Then
            Write(20,400)
400         Format(' ERROR: Arrays to compute correlation are empty.')
            Call ERROR
         Endif
         If(NN.NE.Size(RANKX).or.NN.NE.Size(RANKY)) Then
            Write(20,410)
410         Format(' ERROR: Arrays to compute Spearman correlation',
     +             ' coefficient do not have the same size.')
            Call ERROR
         Endif
         SXX=0
         SX =0
         SXY=0
         SY =0
         SYY=0
         Do I=1,NN
            SXX=SXX+RANKX(I)**2
            SX =SX +RANKX(I)
            SXY=SXY+RANKX(I)*RANKY(I)
            SY =SY +RANKY(I)
            SYY=SYY+RANKY(I)**2
         Enddo
         If(((SXX-(SX**2)/NN).GT.0).and.((SYY-(SY**2)/NN).GT.0))
     +      Then
            A2=(SXY-(SX*SY)/NN)/(SXX-(SX**2)/NN)
            If(Abs(A2).LT.1E-8) Then
               RR=1
            Else
             RR=(SXY-(SX*SY)/NN)/SQRT((SXX-(SX**2)/NN)*
     +          (SYY-(SY**2)/NN))
            Endif
         Else
            Write(20,420)
420         Format(' ERROR: An array has a constant value when',
     +             ' computing linear correlation.')
            Call ERROR
         Endif
         CORREL(1)=RR
!
!  End of the Spearman correlation coefficient routine.
!  Beginning of the standard linear correlation coefficient computations.
!
         SXX=0.0
         SX =0.0
         SXY=0.0
         SY =0.0
         SYY=0.0
         Do I=1,NN
            SXX=SXX+TOTAL_STORAGE(I)**2
            SX =SX +TOTAL_STORAGE(I)
            SXY=SXY+TOTAL_STORAGE(I)*TOTAL_FLOW(I)
            SY =SY +TOTAL_FLOW(I)
            SYY=SYY+TOTAL_FLOW(I)**2
         Enddo
         If(((SXX-(SX**2)/NN).GT.0).and.((SYY-(SY**2)/NN).GT.0))
     +      Then
            A2=(SXY-(SX*SY)/NN)/(SXX-(SX**2)/NN)
            If(abs(A2).LT.1E-8) Then
               RR=1
            Else
             RR=(SXY-(SX*SY)/NN)/SQRT((SXX-(SX**2)/NN)*
     +          (SYY-(SY**2)/NN))
            Endif
         Else
            Write(20,420)
            Call ERROR
         Endif
         CORREL(2)=RR
      Endif
!
!  Statistics for storage volume.
!
         MEANS=SX/NN
         SUMX=0
         Do K=1,NN
            SUMX=SUMX+(TOTAL_STORAGE(K)-MEANS)**2
         Enddo
         SDS=Sqrt(SUMX/(NN-1))
         MINS=MINVAL(TOTAL_STORAGE(1:NN))
         MAXS=MAXVAL(TOTAL_STORAGE(1:NN))
!
!  Statistics for naturalized flow volume.
!
         MEANF=SY/NN
         SUMX=0
         Do K=1,NN
            SUMX=SUMX+(TOTAL_FLOW(K)-MEANF)**2
         Enddo
         SDF=Sqrt(SUMX/(NN-1))
         MINF=MINVAL(TOTAL_FLOW(1:NN))
         MAXF=MAXVAL(TOTAL_FLOW(1:NN))
!
!  Arrays may be written to the message TMS file for information.
!
      If(FILE2.EQ.4) Then
         Write(20,*)
         Write(20,430)
430      Format(' FILE2 option 4 results in the following tabulation',
     +          /,' of arrays developed by 5CR1 or 5COR records.')
         Write(20,*)
         Write(20,440)
440      Format('Sequence   Storage        Flow   ',
     +          'Storage Rank Flow Rank')
         Do I=1,NN
            Write(20,450) I,TOTAL_STORAGE(I),TOTAL_FLOW(I),
     +                    RANKX(I),RANKY(I)
450         Format(I5,2F13.1,2F10.0)
         Enddo
         Write(20,*)
      Endif
!
!  5COR record computations are complete. Skip to statement 670 for 5COR record.
!
      If(CR1F.EQ.0) Goto 670
!
!  Regression between flows and storage use the options specified
!  by FIT on the 5CR1 record.
!
!  FIT=1  Exponential regression.
!
      If(FIT.EQ.1) Then
         Do J=1,NN
            If(TOTAL_STORAGE(J).EQ.0.0) TOTAL_STORAGE(J)=1.0
            If(TOTAL_FLOW(J).EQ.0.0) TOTAL_FLOW(J)=1.0
         EndDo
         Allocate(FLOWT(NN),FLOWP(NN))
         FLOWT=Log(TOTAL_FLOW)
         !
         XMEAN=Sum(TOTAL_STORAGE)/NN
         YMEAN=Sum(FLOWT)/NN
         SXX=0.0
         SX =0.0
         SXY=0.0
         SY =0.0
         SYY=0.0
         Do I=1,NN
            SXX=SXX+TOTAL_STORAGE(I)**2
            SX =SX +TOTAL_STORAGE(I)
            SXY=SXY+TOTAL_STORAGE(I)*FLOWT(I)
            SY =SY +FLOWT(I)
            SYY=SYY+FLOWT(I)**2
         Enddo
         If(NN.GT.0) Then
            If((SXX-(SX**2)/NN).EQ.0) Then
               Write(20,500)
500            Format('WARNING: Impossible to perform linear',
     +                ' regression because X array is constant.')
            Endif
            X=(NN*SXY-SX*SY)/(NN*SXX-SX**2)
            XC=YMEAN-X*XMEAN
            A= 2.718281828459**XC
            B=1/X
            XC=0
            R=((SXY-(SX*SY)/NN)**2)/((SXX-(SX**2)/NN)*(SYY-(SY**2)/NN))
         Else
            Write(20,510)
510         Format('WARNING: Exponential regression has no data.')
         Endif
         Do I=1,NN
            FLOWP(I)=A*EXP(TOTAL_STORAGE(I)/B)
            If(FLOWP(I).LE.0.0) FLOWP(I)=0.01
         Enddo
!
!  FIT=2  Combined function regression.
!
      Elseif(FIT.EQ.2) Then
         Do J=1,NN
            If(TOTAL_STORAGE(J).LE.0.0) Then
               TOTAL_STORAGE(J)=1.0
            Endif
            If(TOTAL_FLOW(J).LE.0.0) Then
               TOTAL_FLOW(J)=1.0
            Endif
         EndDo
         Allocate(STORAGET(NN),FLOWT(NN),STORAGEP(NN),FLOWP(NN))
         STORAGET=Log10(TOTAL_STORAGE)
         FLOWT=Log10(TOTAL_FLOW)
         Call REGRESSION(NN,STORAGET,FLOWT,A,B,R,0)
!
!        Computational problems occur if quantities are too large.
!
         If(B.GT.3.0) Then 
            Write(20,520)
520         Format(' ERROR: Regression option FIT=2 in 5CR1 field 7',
     +      ' does not work for these data. Transformed numbers',
     +      ' become too large.')
            Call ERROR
         Endif
         A=10**A
         STORAGEP=TOTAL_STORAGE(1:NN)**B
         C=B
         Call REGRESSION(NN,STORAGEP,TOTAL_FLOW,A,B,R,INTZERO)
         Do I=1,NN
            FLOWP(I)=A+B*STORAGEP(I)
            If(FLOWP(I).LE.0.0) FLOWP(I)=0.01
         Enddo
!
!  FIT=3  Linear regression.
!
      Elseif(FIT.EQ.3) Then
         Call REGRESSION(NN,TOTAL_STORAGE,TOTAL_FLOW,A,B,R,INTZERO)
         Allocate(FLOWP(NN))
         Do I=1,NN
            FLOWP(I)=A+B*TOTAL_STORAGE(I)
            If(FLOWP(I).LE.0.0) FLOWP(I)=0.01
         Enddo
!
!  FIT=4  Power function regression.
!
      Elseif(FIT.EQ.4) Then
         Do J=1,NN
            If(TOTAL_STORAGE(J).EQ.0.0) Then
               TOTAL_STORAGE(J)=1.
            Endif
            If(TOTAL_FLOW(J).EQ.0.0) Then
               TOTAL_FLOW(J)=1.
            Endif
         EndDo
         Allocate(STORAGET(NN),FLOWT(NN),FLOWP(NN))
         STORAGET=LOG10(TOTAL_STORAGE)
         FLOWT=LOG10(TOTAL_FLOW)
         Call REGRESSION(NN,STORAGET,FLOWT,A,B,R,0)
         C=B
         B=10**A
         Do I=1,NN
            FLOWP(I)=B*TOTAL_STORAGE(I)**C
            If(FLOWP(I).LE.0.0) FLOWP(I)=0.01
         Enddo
!
!  FIT=-1,-2  User defined coefficients.
!
      Elseif(FIT.EQ.-1) Then
         If(AA.EQ.0.and.BB.EQ.0) Then
            Write(20,530)
530         Format(' ERROR: FIT in field 7 of first 5CR1 record is -1',
     +             ' but fields 9-10 are empty.')
            Call ERROR
         Endif
         Allocate(FLOWP(NN))
         A=AA
         B=BB
         Do I=1,NN
            FLOWP(I)=A*EXP(TOTAL_STORAGE(I)/B)
            If(FLOWP(I).LE.0.0) Then
               FLOWP(I)=0.1
            Endif
         Enddo
      Elseif(FIT.LE.-2) Then
         If(AA.EQ.0.and.BB.EQ.0.and.CC.EQ.0) Then
            Write(20,540)
540         Format(' ERROR:FIT in field 7 of first 5CR1 record is -1',
     +             ' but fields 9-10 are empty.')
            Call ERROR
         Endif
         Allocate(FLOWP(NN))
         A=AA
         B=BB
         C=CC
         Do I=1,NN
            FLOWP(I)=A+B*TOTAL_STORAGE(I)**C
            If(FLOWP(I).LE.0.0) Then
               FLOWP(I)=0.1
            Endif
         Enddo
      Endif
!
!  Arrays SFFARR and XFREQ are allocated for NN+6 frequencies because
!  options add extra frequency points in addition to the NN sequencies.
!
      If(Allocated(SFFARR)) Deallocate(SFFARR)
      If(Allocated(XFREQ))  Deallocate(XFREQ)
      Allocate(SFFARR(NN+8,2),XFREQ(NN+8,2))
      SFFARR=0.0
      XFREQ=0.0
!
!  Array QARRAY(NN) stores naturalized flow volumes for developing a FF table
!  or flow percentages for developing a SFF table, either as the SFFARR array.
!
      If(Allocated(QARRAY)) Deallocate(QARRAY)
      Allocate(QARRAY(NN))
      QARRAY=0.0
!
!  Flows in SFFARR array are expressed as a percentage FLOWSPER of predicted
!  flow unless FIT=0.  FIT=0 results in FF rather than SFF relationship.
!
      If(FIT.NE.0) Then
         Allocate(FLOWSPER(NN))
         FLOWSPER=0.0
         FLOWSPER=TOTAL_FLOW(1:NN)*100.0/FLOWP(1:NN)
         QARRAY=FLOWSPER
      Else
         QARRAY=TOTAL_FLOW
      Endif
      Call SORT(NN,QARRAY,1)
      NUMVAL=NN
!
!  Statistics for naturalized flow as percentage of predicted naturalized flow.
!
      If(FIT.NE.0) Then
         MEANP=SUM(FLOWSPER(1:NN))/NN
         SUMX=0
         Do K=1,NN
            SUMX=SUMX+(FLOWSPER(K)-MEANP)**2
         Enddo
         SDP=Sqrt(SUMX/(NN-1))
         MINP=MINVAL(FLOWSPER(1:NN))
         MAXP=MAXVAL(FLOWSPER(1:NN))
      Endif
!
!  Frequencies corresponding to flow percentages (FIT>0) or flows (FIT=0) are
!  computed for SFFARR array. 5CR1 record options include: with (FIT>0) versus
!  without (FIT=0) storage-flow regression options, and lognormal distribution
!  (DIST=2) versus Weibull formula (DIST=0,1).
!
!  The log-normal probability distribution is applied by calling
!  subroutine LOGNORMAL.
!
      If(DIST.EQ.2) Then
         Do I=1,NN
            If(QARRAY(I).EQ.0.0) QARRAY(I)=1.0
         Enddo
         QARRAY=LOG(QARRAY)
         TYPEP=0
         Call LOGNORMAL
!
!  Weibull method is applied to develop the frequency distribution.
!
      Elseif(DIST.LE.1) Then
         If(NN.LE.1) Then
            Write(20,550)
550         Format(' ERROR: Array to compute frequencies has zero',
     +             ' or one elements.')
            Call ERROR
         Endif
!
!        Q-array is sorted in descending order.
!
         Call SORT(NN,QARRAY,1)
!
!        Frequency-flow relationship is developed.
!
         Allocate(QFREQ2(NN+1,2))
         Do I=1,NN
            XFREQ(I,1)=QARRAY(I)
            XFREQ(I,2)=Real(I)/(NN+1)
         Enddo
         XFREQ(NN+1,1)=0.0
         XFREQ(NN+1,2)=1.00
!
!        The ratio with an exceedance probability of 0.0000001 is added.
!
         QFREQ2=XFREQ
         XFREQ(1,2)=0.0000001
         Call LINEAR(XFREQ(1,2),XFREQ(1,1),QFREQ2(:,2),
     +      QFREQ2(:,1),NN+1)
         XFREQ(2:NN+2,:)=QFREQ2(:,:)
         NN=NN+2
      Endif
!
!  End of the Weibull frequency computations.
!
!  The final SFFARR is populated.
!
      SFFARR=XFREQ
      SFFARRAYF=1
!
!  The SFF file (unit 8) is opened and array SFFARRAY is written
!  to the SFF file if specified by FILE2 from the 5CR1 record.
!
      If(FILE2.EQ.1.or.FILE2.GE.3) Then
         If(SFFOPEN.EQ.0) Then
            SFFOPEN=SFFOPEN+1
            P=Index(SROOT,'   ')-1
            OUTPUTSFF=SROOT(:P)//'.SFF'
            Write(*,600) OUTPUTSFF
600         Format(3x,' Opening SFF File: ',A50,/)
            Open(UNIT=8,FILE=OUTPUTSFF,FORM='FORMATTED',ACCESS=
     +           'SEQUENTIAL',STATUS='Unknown')
         Endif
         If(CR2.GT.0) Then
            If(FIT.NE.0) Then
               Write(8,610) TCR1,CR2
610            Format('SFF Array for CR1 of',I3,
     +                ' months and CR2 of month',I2)
               Write(8,620)
620            Format('   Flow Percent  Frequency')
            Else
               Write(8,630) TCR1,CR2
630            Format('FF Array for CR1 of',I3,
     +                ' months and CR2 of month',I2)
               Write(8,640)
640            Format('        Flow     Frequency')
            Endif
         Elseif(CR2.EQ.0) Then
            If(FIT.NE.0) Then
               Write(8,610) TCR1
               Write(8,620)
            Else
               Write(8,630) TCR1
               Write(8,640)
            Endif
         Endif
         Do I=1,NN+6
            Write(8,650) SFFARR(I,1),SFFARR(I,2)
650         Format(F15.3,F11.6)
         Enddo
         Write(8,660)
660      Format('**************************')
         Write(8,*)
      Endif
!
!  Statistics are recorded in TABLES output TOU file.
!
670   If(CR1F.EQ.0.or.FILE2.GE.2) Then
         Write(2,*)
         If(CR1F.EQ.0) Then
            Write(2,1010)
         Else
            If(FIT.EQ.0) Then
                Write(2,1020)
            Else
                Write(2,1030)
            Endif
         Endif
1010     Format('STATISTICS GENERATED BY 5COR RECORD')
1020     Format('5CR1 RECORD STATISTICS FOR FF RELATIONSHIP')
1030     Format('5CR1 RECORD STATISTICS FOR SFF RELATIONSHIP')
!
         Write(2,*)
         If(CR1.GT.0) Then
            If(FIT.GT.0.or.CR1F.EQ.0) Then
               Write(2,1040) CR2
1040           Format('Beginning-of-month storage contents in month',I3)
            Else
               Write(2,1050)
1050           Format('Monthly cycle option.')
            Endif
         Endif
         Write(2,1060) CR1,CR2
1060     Format('Naturalized flows for',I3,
     +          ' months beginning in month',I2)
!
         Write(2,*)
         If(FIT.GT.0.or.CR1F.EQ.0) Then
            If(NSTOR.GT.0.and.RESFLAG.EQ.1) Then
               Write(2,1070) (Adjustl(CPS(I)),I=1,NSTOR)
            Elseif(NSTOR.EQ.-1) Then
               Write(2,1080)
            Elseif(NSTOR.GT.0.and.RESFLAG.EQ.0) Then
               Write(2,1090) (Adjustl(CPS(I)),I=1,NSTOR)
            Elseif(NSTOR.EQ.0) Then
               Write(2,1100)
            Endif
1070        Format('Storage in reservoir(s):    ',15(2x,A6))
1080        Format('Summation of storage in all reservoirs.')
1090        Format('Storage at control point(s):',15(2x,A6))
1100        Format('Summation of storage at all control points.')
         Endif
!
         If(NFLOW.GT.0) Write(2,1110) (Adjustl(CPF(I)),I=1,NFLOW)
         If(NFLOW.EQ.0) Write(2,1120)
1110     Format('Flows at control point(s):  ',15(2x,A6))
1120     Format('Summation of naturalized flows at all control points.')
!
!  Statistics for storage, flow volumes, and flow percentages.
!
         Write(2,*)
         Write(2,1200)
1200     Format('Storage and Flow Statistics:')
         Write(2,1210)
         Write(2,1220) MEANS,MEANF,MEANP
         Write(2,1230) SDS,SDF,SDP
         Write(2,1240) MINS,MINF,MINP
         Write(2,1250) MAXS,MAXF,MAXP
1210     Format(17x,'Storage',5x,'Flow(Q)',5x,'(Q/Qp)100%')
1220     Format('    Mean',F16.1,F13.1,F14.3)
1230     Format('    Std Dev',F13.1,F13.1,F14.3)
1240     Format('    Minimum',F13.1,F13.1,F14.3)
1250     Format('    Maximum',F13.1,F13.1,F14.3)
!
!  Linear correlation coefficient and Spearman correlation coefficient.
!
         Write(2,*)
         Write(2,1260) CORREL(2)
         Write(2,1270) CORREL(1)
1260     Format('Linear correlation coefficient for ',
     +          'storage versus flow volume   =',F8.4)
1270     Format('Spearman correlation coefficient for ',
     +          'storage versus flow volume =',F8.4)
      Endif
!
!      Regression and correlation coefficients.
!
      If(CR1F.GT.0.and.FIT.GT.0.and.FILE2.GE.2) Then
         Write(2,*)
         RC=Sqrt(R)
         Write(2,1300) FIT
1300     Format('Regression and correlation coefficients for FIT =',I2)
         If(FIT.EQ.1) Then
            Write(2,1310) A,B,RC
         Elseif(FIT.EQ.2) Then
            Write(2,1320) A,B,C,RC
         Elseif(FIT.EQ.3) Then
            Write(2,1330) A,B,RC
         Elseif(FIT.EQ.4) Then
            Write(2,1340) B,C,RC
         Elseif(FIT.EQ.-1) Then
            Write(2,1350) A,B
         Elseif(FIT.LE.-2) Then
            Write(2,1360) A,B,C
         Endif
1310     Format('Exponential Regression,  Q = a*exp^(S/b),   a =',
     +           F13.3,'   b =',F13.3,'   R =',F7.4)
1320     Format('Combined Regression,  Q = a + b*S^c,   a =',F12.3,
     +          '   b =',E11.5,'   c =',F7.4,'   R =',F7.4)
1330     Format('Linear Regression,  Q = a+b*S,   a =',F12.3,
     +          '   b =',E11.5,'   R = ',F7.4)
1340     Format('Power Regression,  Q = b*S^c,   b =',F13.4,
     +          '   c =',F7.4,'   R =',F7.4)
1350     Format('User Defined Exponential,  Q = a*exp^(S/b),   a =',
     +           F13.3,'   b =',F13.3)
1360     Format('User Defined,   Q = a + b*S^c,   a =',F13.3,
     +          '   b =',E11.5,'   c =',F7.4)
         Write(2,*)
      Endif
!
      If(CR1F.EQ.0.or.FILE2.GE.2) Then
         Deallocate(RX,RY,RANKX,RANKY)
      Endif
      If(Allocated(FLOWP)) Deallocate (FLOWP)
      If(Allocated(FLOWSPER)) Deallocate (FLOWSPER)
      If(Allocated(FLOWT)) Deallocate (FLOWT)
      If(Allocated(STORAGEP)) Deallocate (STORAGEP)
      If(Allocated(STORAGET)) Deallocate (STORAGET)
      If(Allocated(TOTAL_FLOW)) Deallocate (TOTAL_FLOW)
      If(Allocated(TOTAL_STORAGE)) Deallocate (TOTAL_STORAGE)
      If(Allocated(QFREQ2)) Deallocate (QFREQ2)
      If(Allocated(XFREQ)) Deallocate (XFREQ)
      If(Allocated(QARRAY)) Deallocate (QARRAY)
!
      Return
      End Subroutine CRMSFF
!
! **************************************************************************
!
      Subroutine CRMIPA
!
!  *-*-*-*-*-*  5CR2   *-*-*-*-*-*
!  Subroutine CRMIPA develops an incremental probability array (IPA) based
!  on a storage-flow-frequency (SFF) array developed by Subroutine CRMSFF.
!  The expected probability array EXPP assigns incremental probabilities to
!  the CRM hydrologic sequences which sum to 1.0. Subroutines RELIAB, FREQ,
!  and STORAGE use the array EXPP for conditional reliability modeling (CRM).
!
      Use COMVAR
      Use CRMVAR
      Logical SORTED
!
      Integer REC,N,J,CREC,REC2,CYCLEN,K,RECORD1,SKIP,N2,POS2,P,I
      Integer:: JUMP
      Integer POS,NUM,NN,NUMFQ,COUNT,COUNT2
      Real QP,TOT_INISTOR,TOTALP,NATFLO,SUMX,TEMP,TEMP2
      Real,Allocatable,Dimension(:)::TOTAL_FLOW,FLOWS,RQ,EXCP,RANK
      Character(len=4) CD,CD1,CD2
      Character(len=6) TEMP_RES,NAME2
      Character(len=17) CD3
      Character(len=50) INPUTSFF
!
!  First 5CR2 record is read from TIN file.
!
      Write(*,*)
      Read(1,10,IOSTAT=STATUS) CD,NFLOW,NSTOR,READINI,FM,FIT,FILE1,
     +                         FILE2,MFACTOR,AA,BB,CC
10    Format(A4,7I4,4F8.0)
!
!  ERROR ckecks for first 5CR2 record.
!
      If(STATUS.NE.0) Then
         Write(20,20) STATUS
20      Format(' ERROR: Fortran IOSTAT error reading first 5CR2 record.'
     +          ,/,'   *** IOSTAT status variable (error code) =,'I6)
         Call ERROR
      Endif
      If(NFLOW.LT.0.or.NFLOW.GT.15) Then
         Write(20,30) NFLOW
30       Format(' ERROR: NFLOW of',I3,' on 5CR2 record is not valid.')
         Call ERROR
      Endif
      If(NSTOR.LT.-1.or.NSTOR.GT.15) Then
         Write(20,40) NSTOR
40       Format(' ERROR: NSTOR of',I3,' on 5CR2 record is not valid.')
         Call ERROR
      Endif
      If(CR1.EQ.0) Then
         Write(20,50)
50       Format(' ERROR: A 5CR2 record is used only for a CRM',
     +          ' simulation (non-zero CR1).')
         Call ERROR
      Endif
      If(FM.LT.0.or.FM.GT.CR1) Then
         Write(20,60) FM
60       Format(' ERROR: FM of',I3,' on 5CR2 record is not valid.')
         Call ERROR
      Endif
      If(FIT.LT.-2.or.FIT.GT.2) Then
         Write(20,70) FIT
70       Format(' ERROR: FIT of',I3,' on 5CR2 record is not valid.')
         Call ERROR
      Endif
      If(FILE1.LT.0.or.FILE1.GT.2) Then
         Write(20,80) FILE1
80       Format(' ERROR: FILE1 of',I3,' on 5CR2 record is not valid.')
         Call ERROR
      Endif
      If(FILE2.LT.0) Then
         Write(20,90) FILE2
90       Format(' ERROR: FILE2 of',I3,' on 5CR2 record is not valid.')
         Call ERROR
      Endif
      If(MFACTOR.LT.0.0.or.MFACTOR.GT.10.0) Then
         Write(20,100) MFACTOR
100      Format(' WARNING: MFACTOR of',I3,' on 5CR2 record.')
      Endif
      If(READINI.EQ.0.and.(NSTOR.EQ.0.or.NSTOR.EQ.-1)) Then
         Write(20,110)
110      Format(' ERROR: NFLOW and NSTOR require storages from a BRS',
     +          ' file but READINI is zero.')
         Call ERROR
      Endif
!
!  Defaults for FM and MFACTOR from first 2CR2 record.
!
      If(FM.EQ.0) FM=CR1
      If(Abs(MFACTOR).LT.0.0000001) MFACTOR=1.0
!
!  Error checks for second 5CR2 record.
!
      If(NFLOW.EQ.0.and.(NSTOR.EQ.0.or.NSTOR.EQ.-1)) Then
         Read(1,200,IOSTAT=STATUS) CD1,CD2
         If(STATUS.NE.0) Then
            Write(20,130) CD,STATUS
130         Format(' ERROR: Fortran IOSTAT error reading second ',A4,
     +             ' record',/,'   *** IOSTAT status variable',
     +             ' (error code) =,'I6)
            Call ERROR
         Endif
         If(CD1.EQ.CD) Then
            Write(20,150)
150         Format(' ERROR: Since 5CR2 field 4 is non-zero, there',
     +             ' should not be a second 5CR2 record.')
            Call ERROR
         Else
            Backspace(1)
         Endif
!
!  Skip to statement 300 if NFLOW and NSTOR in 5CR2 fields 2 and 3 and READINI
!  in 5CR2 field 4 indicate that supplemental 5CR2 records are not needed.
!
         If(READINI.GE.1) Goto 300
      Endif
!
!  Second 5CR2 record is read from TIN file.
!
      If(NFLOW.GT.0) Then
         Read(1,200,IOSTAT=STATUS) CD1,CD2
         If(STATUS.NE.0) Then
            Write(20,130) CD,STATUS
            Call ERROR
         Endif
         If(CD1.NE.CD) Then
            Write(20,170) CD1
170         Format(' ERROR: A 5CR2 record was expected instead of ',A4)
            Call ERROR
         Endif
         If((CD2.NE.'FLOW')) Then
            Write(20,180) CD2
180         Format(' ERROR: Field 2 of second 5CR2 record should be',
     +                ' FLOW instead of ',A4)
            Call ERROR
         Endif
         Backspace(1)
         Read(1,190)(CPF(I),I=1,NFLOW)
190      Format(8x,15(2X,A6))
      Elseif(NFLOW.EQ.0.and.NSTOR.GT.0) Then
         Read(1,200,IOSTAT=STATUS) CD1,CD2
200      Format(2A4)
         If(STATUS.NE.0) Then
            Write(20,130) CD,STATUS
            Call ERROR
         Endif
         If(CD1.NE.CD) Then
            Write(20,170) CD1
            Call ERROR
         Endif
         If(CD2.EQ.'STCP') Then
            Backspace(1)
            Read(1,190) (CPS(I),I=1,NSTOR)
            RESFLAG=0
         Elseif(CD2.EQ.'STRE') Then
            Backspace(1)
            Read(1,190)(CPS(I),I=1,NSTOR)
            RESFLAG=1
         Else
            Write(20,210) CD2
210         Format(' ERROR: Field 2 of second 5CR2 record should be',
     +             ' STCP or STRE instead of ',A4)
            Call ERROR
         Endif
      Endif
!
!  Third 5CR2 is read from TIN file.
!
      Read(1,200,IOSTAT=STATUS) CD1,CD2
      If(STATUS.NE.0) Then
         Write(20,220) CD,STATUS
220      Format(' ERROR: Fortran IOSTAT error reading third ',A4,
     +          ' record.',/,'   *** IOSTAT',
     +          ' status variable (error code) =,'I6)
         Call ERROR
      Endif
      If(NSTOR.EQ.0.or.NSTOR.EQ.-1) Then
         If(READINI.GE.1) Then
            If(CD1.EQ.CD) Then
               Write(20,230)
230            Format(' ERROR: Since 5CR2 field 4 is non-zero, there',
     +                ' should not be a third 5CR2 record.')
               Call ERROR
            Else
               Backspace(1)
           Endif
         Endif
      Elseif(NFLOW.EQ.0.and.NSTOR.GT.0) Then
         If(READINI.EQ.0) Then
            If(CD1.NE.CD) Then
               Write(20,170) CD1
               Call ERROR
            Elseif(CD2.NE.'INIT') Then
               Write(20,240) CD2
240            Format(' ERROR: 5CR2 field 2 should be INIT instead of ',
     +                A4)
               Call ERROR
            Elseif(CD2.EQ.'INIT') Then
               Backspace(1)
               Read(1,260) (IS(I),I=1,NSTOR)
            Endif
         Else
            If(CD1.EQ.CD) Then
               Write(20,230)
               Call ERROR
            Else
               Backspace(1)
            Endif
         Endif
      Elseif(NFLOW.GT.0.and.NSTOR.GT.0) Then
         If(CD1.NE.CD) Then
            Write(20,170) CD1
            Call ERROR
         Endif
         If(CD2.EQ.'STCP') Then
            Backspace(1)
            Read(1,190) (CPS(I),I=1,NSTOR)
            RESFLAG=0
         Elseif(CD2.EQ.'STRE') Then
            Backspace(1)
            Read(1,190) (CPS(I),I=1,NSTOR)
            RESFLAG=1
         Else
            Write(20,210) CD2
            Call ERROR
         Endif
      Endif
!
!  Fourth 5CR2 record is read from TIN file.
!
      If(NFLOW.GT.0.and.NSTOR.GT.0) Then
         Read(1,200,IOSTAT=STATUS) CD1,CD2
         If(STATUS.NE.0) Then
            Write(20,250) CD,STATUS
250         Format(' ERROR: Fortran IOSTAT error occurred reading',
     +             ' fourth ',A4,' record',/,'   *** IOSTAT status',
     +             ' variable (error code) =,'I6)
            Call ERROR
         Endif
         If(READINI.EQ.0) Then
            If(CD1.NE.CD) Then
               Write(20,170) CD1
               Call ERROR
            Elseif(CD2.NE.'INIT') Then
               Write(20,240) CD2
               Call ERROR
            Elseif(CD2.EQ.'INIT') Then
               Backspace(1)
               Read(1,260) (IS(I),I=1,NSTOR)
260            Format(8x,15(F8.0))
            Endif
         Else
            If(CD1.EQ.CD) Then
               Write(20,230)
               Call ERROR
            Else
               Backspace(1)
            Endif
         Endif
      Endif
!
!  The incremental probability array is created based on the 5CR2 records
!  and is written to a SFF file or stored in memory as the EXPP array.
!
300   EQFLAG=0
!
!  NUM is the number of reservoirs for which storage is summed.
!
      If(NSTOR.EQ.0) Then
         NUM=NCPTS
      Elseif(NSTOR.EQ.-1) Then
         NUM=NREOUT
      Else
         NUM=NSTOR
      Endif
!
!  SFF file is opened.
!
      If(FILE1.EQ.2) Then
         P=Index(SROOT,'   ')-1
         INPUTSFF=SROOT(:P)//'.SFF'
         Write(*,310) INPUTSFF
310      Format(3x,' Opening SFF File: ',A25,/)
         INQUIRE (FILE=INPUTSFF,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(*,320) INPUTSFF
            Write(20,320) INPUTSFF
320         Format('    File does not exist: ',A50)
            Call ERROR
         Else
            OPEN(UNIT=8,FILE=INPUTSFF,FORM='FORMATTED',
     +           ACCESS='SEQUENTIAL',STATUS='OLD')
         Endif
      Endif
!
!  Beginning reservoir storage BRS file is opened.
!
      If(READINI.GE.1) Then
         INBRS=SROOT(1:len_trim(SROOT))//'.BRS'
         Write(*,330) INBRS
330      Format(3x,' Opening BRS File: ',A25,/)
         Write(*,*)
         Inquire (FILE=INBRS,EXIST=EXIST)
         If(.NOT.EXIST) Then
            Write(20,320) INBRS
            Call ERROR
         Else
            Open(UNIT=9,FILE=INBRS,FORM='FORMATTED',
     +           ACCESS='SEQUENTIAL',STATUS='OLD')
         Endif
      Endif
!
!  Initial storage volume is determined.
!
      TOT_INISTOR=0.0
      If(READINI.EQ.0) Then
         TOT_INISTOR=Sum(IS)*MFACTOR
      Elseif(NSTOR.GT.0) Then
         If(MFACTOR.NE.1.0) Then
            Write(20,340) MFACTOR
340         Format('WARNING: Initial storages are read from BRS file',
     +             ' but MFACTOR from 5CR2 record is',F8.4)
         Endif
         Do I=1,NSTOR
            TEMP_RES=CPS(I)
            Do while (RESIDBRS.NE.TEMP_RES)
               If(RESFLAG.EQ.0) Then
                  Read(9,350,IOSTAT=STATUS) RESIDBRS
350               Format(18x,A6)
                  If(STATUS.EQ.-1) Then
                     Write(20,360) TEMP_RES
360                  Format(' ERROR: Control point ID ',A6,
     +                      ' was not found in BRS file.')
                     Call ERROR
                  Endif
               Elseif(RESFLAG.EQ.1) Then
                  Read(9,370,IOSTAT=STATUS) RESIDBRS
370               Format(6x,A6)
                  If(STATUS.EQ.-1) Then
                     Write(20,380) TEMP_RES
380                  Format(' ERROR: Reservoir ID ',A6,
     +                      ' was not found in BRS file.')
                     Call ERROR
                  Endif
               Endif
            Enddo
            If(RESIDBRS.EQ.TEMP_RES) Goto 410
390         Write(20,360) TEMP_RES
400         Write(20,370) TEMP_RES
            Goto 430
410         Backspace(9)
            Read(9,420) INISTOR
420         Format(35x,F10.1)
            TOT_INISTOR=TOT_INISTOR+INISTOR
430         Rewind(9)
         Enddo
      Elseif(NSTOR.EQ.0) Then
         POS=1
         Do I=1,NCPTS
            REC=5+NWROUT+POS
            Read(4,500,REC=REC) TEMP_RES
500         Format(A6)
            POS=POS+1
            Do while (RESIDBRS.NE.TEMP_RES)
               If(RESFLAG.EQ.0) Then
                  Read(9,350,IOSTAT=STATUS) RESIDBRS
                  If(STATUS.EQ.-1) Write(20,360) TEMP_RES
               Elseif(RESFLAG.EQ.1) Then
                  Read(9,370,IOSTAT=STATUS) RESIDBRS
                  If(STATUS.EQ.-1) Write(20,380) TEMP_RES
               Endif
            Enddo
            If(RESIDBRS.EQ.TEMP_RES) Goto 530
            Write(20,360) TEMP_RES
            Write(20,380) TEMP_RES
510         Write(20,360) TEMP_RES
520         Write(20,380) TEMP_RES
            Goto 540
530         Backspace(9)
            Read(9,420)INISTOR
            TOT_INISTOR=TOT_INISTOR+INISTOR
540         Rewind(9)
         Enddo
      Elseif(NSTOR.EQ.-1) Then
         POS=1
         Do I=1,NREOUT
            REC=5+NWROUT+NCPTS+POS
            Read(4,500,REC=REC) TEMP_RES
            POS=POS+1
            Do while (RESIDBRS.NE.TEMP_RES)
               If(RESFLAG.EQ.0) Then
                  Read(9,350,IOSTAT=STATUS) RESIDBRS
                  If(STATUS.EQ.-1) Write(20,360) TEMP_RES
               Elseif(RESFLAG.EQ.1) Then
                  Read(9,370,IOSTAT=STATUS) RESIDBRS
                  If(STATUS.EQ.-1) Write(20,380) TEMP_RES
               Endif
            Enddo
            If(RESIDBRS.NE.TEMP_RES) Then
               Write(20,360) TEMP_RES
               Write(20,380) TEMP_RES
               Goto 600
               If(RESFLAG.EQ.0) Read(9,350,END=560)RESIDBRS
               If(RESFLAG.EQ.1) Read(9,370,END=550)RESIDBRS
            Endif
            If(RESIDBRS.EQ.TEMP_RES) Goto 570
550         Write(20,360) TEMP_RES
560         Write(20,380) TEMP_RES
            Goto 600
570         Backspace(9)
            Read(9,580) INISTOR
580         Format(35x,F10.1)
            TOT_INISTOR=TOT_INISTOR+INISTOR
600         Rewind(9)
         Enddo
      Endif
!
!  Calculation of QP is not necessary in developing a FF relationship.
!
      If(FIT.NE.0) Then
!
!  Calculation of predicted flow QP.
!
         If(FIT.EQ.-1) Then
            If(AA.EQ.0.and.BB.EQ.0) Then
               Write(20,610)
610            Format(' ERROR: Field 6 of first 5CR2 record is -1, but',
     +                ' fields 10 and 11 are empty.')
            Call ERROR
            Endif
            A=AA
            B=BB
            QP=A*Exp(TOT_INISTOR/B)
         Elseif(FIT.EQ.-2) Then
            If(AA.EQ.0.and.BB.EQ.0.and.CC.EQ.0) Then
               Write(20,620)
620            Format(' ERROR: Field 6 of first 5CR2 record is -2, but',
     +                ' fields 10-12 are empty.')
               Call ERROR
            Endif
            A=AA
            B=BB
            C=CC
            QP=A+B*TOT_INISTOR**C
         Elseif(FIT.EQ.1) Then
            QP=A*Exp(TOT_INISTOR/B)
         Elseif(FIT.EQ.2) Then
            QP=A+B*TOT_INISTOR**C
         Endif
         If(QP.LE.0) Then
            Write(20,630)
630         Format(' WARNING: A negative flow is predicted but changed',
     +             ' to 0.001')
            QP=0.001
            EQFLAG=1
         Endif
      Endif
!
!  Naturalized flow volumes are extracted from the CRM file.
!
      If(NFLOW.EQ.0) NUM=NCPTS
      If(NFLOW.GT.0) NUM=NFLOW
      NN=NYRS
!
!  If months to be used to sum flows is different on the 5CR1 and 5CR2
!  records, the value specified in the 5CR2 record is adopted.
!
      If(SFFARRAYF.EQ.0) Then
         TCR1=CR1
      Else
         If(FM.GT.0.and.FM.NE.TCR1) Then
            Write(20,650)
650          Format(' WARNING: The number of months used to sum flows',
     +            ' is different in the SFF versus probability arrays.')
            TCR1=FM
         Elseif(FM.GT.0) Then 
            TCR1=FM
         Endif
      Endif
!
      If(Allocated(TOTAL_FLOW)) Deallocate(TOTAL_FLOW)
      Allocate(TOTAL_FLOW(NN),FLOWS(NN))
!
      TOTAL_FLOW=0.0
      FLOWS=0.0
      REC=5+NWROUT+1
      Do N=1,NUM
         If(NFLOW.EQ.0) Then
            Read(4,500,REC=REC) TEMP_RES
            REC=REC+1
         Else
            TEMP_RES=CPF(N)
         Endif
         CYCLEN=1
         CREC=NWROUT+NCPTS+NREOUT
         FLOWS=0
         SKIP=NCPTS
         RECORD1=6+NWROUT
         N2=1
         Do While (N2.LE.SKIP)
            Read(4,660,REC=RECORD1) NAME2
660         Format(A6)
            If(NAME2.EQ.TEMP_RES) Then
               POS2=N2
               Goto 670
            Endif
            RECORD1=RECORD1+1
            N2=N2+1
         EndDo
670      REC2=5+NWROUT+POS2
         Do I=1,NN
         JUMP=0
            Do J=1,TCR1
               Read(4,680,REC=REC2) NAME2,NATFLO
680            Format(A6,77X,F11.2)
               REC2=REC2+CREC
               If(TEMP_RES.NE.NAME2) Then
                  Write(20,690) TEMP_RES
690               Format(' ERROR: Control point identifier ',A6,' does',
     +                   ' not follow in sequence in SIM output file.')
                  Call ERROR
               Endif
               FLOWS(I)=FLOWS(I)+NATFLO
            End Do
            REC2=REC2+(NPRDS-TCR1)*CREC
            Do K=1,CYCLEN
               JUMP=(TNYRS*12-K+1)*CREC+JUMP
            Enddo
            If((REC2+(NPRDS-1)*CREC).GT.(JUMP+5)) Then
               REC2=(JUMP+NWROUT+POS2+5)
               CYCLEN=CYCLEN+1
            Endif
         End Do
         TOTAL_FLOW=TOTAL_FLOW+FLOWS
      Enddo
!
!  The random variable is calculated. For a FF relationship, the random
!  variable is the cummulated flows. For a SFF relationship, the random
!  variable is the ratio of known-to-predicted flow (Q/QP).
!
      Allocate(RQ(NN))
      If(FIT.NE.0) Then
         RQ=TOTAL_FLOW*100/QP
      Else
         RQ=TOTAL_FLOW
      Endif
!
!  The SFF array SFFARR is interpolated to obtain exceedance probabilities
!  each RQ value. The SFF array is accessed from a SFF file or from memory.
!
      If(FILE1.LE.1) Then
         If(SFFARRAYF.EQ.0) Then
            Write(20,700)
700         Format(' ERROR: A SFF or FF array has not been created.')
            Call ERROR
         Endif
      Elseif(FILE1.EQ.2) Then
         Rewind(8)
         Read(8,720) CD3
         If(CD3.NE.'SFF Array for CR1'.and.CD3.NE.'FF Array for CR1 ')
     +      Then
            Write(20,710)
710         Format(' ERROR: SFF File does not contain a SFF',
     +             ' or FF array.')
            Call ERROR
         Endif
         COUNT=1
         CD3='  '
         Do while (CD3.NE.'*****************')
            Read(8,720) CD3
720         Format(A17)
            COUNT=COUNT+1
         Enddo
         Rewind(8)
         If(Allocated(SFFARR)) Deallocate(SFFARR)
         Allocate(SFFARR((COUNT-3),2))
         Read(8,*)
         Read(8,*)
         Do I=1,COUNT-3
            Read(8,730) SFFARR(I,1),SFFARR(I,2)
730         Format(F15.3,3x,F8.6)
         Enddo
         Read(8,*)
         Read(8,*)
         SFFREAD=SFFREAD+1
         If(SFFREAD.GT.1) Then
            Write(20,735) SFFREAD,SFFREAD
735         Format(' WARNING: With FILE1 option 2 on',I2, ' 5CR2',
     +             ' records, the first or only SFF array in the SFF',
     +             ' file is read',I2,' times.')
         Endif
      Endif
!
!  The normal distribution table is interpolated directly if the lognormal
!  distribution is used and the SFF or FF array is read from memory.
!
      If(FILE1.LE.1.and.DIST.EQ.2) Then
         If(Allocated(QARRAY)) Deallocate(QARRAY)
         If(Allocated(XFREQ))  Deallocate(XFREQ)
         Allocate(QARRAY(NN),RANK(NN),EXCP(NN),XFREQ(NN+6,2))
         Do J=1,NN
            If(RQ(J).EQ.0.0) Then
               RQ(J)=1.
            Endif
            RANK(J)=J*1.0
         EndDo
         QARRAY=LOG(RQ)
         NUMVAL=NN
         TYPEP=1
         Call LOGNORMAL
         EXCP=XFREQ(:,2)
      Else
         NUMFQ=Size(SFFARR(:,1))
         Allocate(EXCP(NN),RANK(NN))
!
!  The array SFFARR is sorted in descending order.
!
         If(NUMFQ.LE.1) Goto 740
         SORTED=.FALSE. 
         Do while (.NOT.SORTED)
            SORTED=.TRUE.
            Do I=1,NUMFQ-1
               If(SFFARR(I,1).LT.SFFARR(I+1,1)) Then
                  TEMP=SFFARR(I+1,1)
                  TEMP2=SFFARR(I+1,2)
                  SFFARR(I+1,1)=SFFARR(I,1)
                  SFFARR(I+1,2)=SFFARR(I,2)
                  SFFARR(I,1)=TEMP
                  SFFARR(I,2)=TEMP2
                  SORTED=.FALSE.
               Endif
            Enddo
         Enddo
!
!  Completion of the sort routine.
!
740      Do I=1,NN
            If(RQ(I).EQ.0.0) RQ(I)=1.0
            Call LINEAR(RQ(I),EXCP(I),SFFARR(:,1),SFFARR(:,2),NUMFQ)
            RANK(I)=I
            If(RQ(I).GT.SFFARR(1,1)) EXCP(I)=0.0
         Enddo
      Endif
!
!  Array EXCP is sorted in descending order keeping the original ranks in memory.
!
      Call SORTR(NN,EXCP,RQ,RANK,1)
!
!  The incremental probability for each RQ is computed based on the half-way points 
!  between the exceedance probabilities of that R and the next larger RQ and next 
!  smaller RQ. In case of two consecutive RQ with the same exceedance probability
!  then the incremental probability is calculated as the half-way between the next 
!  smaller RQ and the next greater RQ with different exceedance probabilities, 
!  divided by the number of repetitions(COUNT2).
!
!  COUNT=COUNT+1 if two consecutive values of R have the same exceedance probability.
!  COUNT2 is a counter of the number of times an exceedance probability is repeated.
!
      If(Allocated(EXPP)) Deallocate(EXPP)
      Allocate(EXPP(NN))
      If(EQFLAG.EQ.1) Then
         EXPP=Real(1.0/NN)
         Goto 780
      Endif
      COUNT=0
      Do 770 I=1,NN-1
         If(EXCP(I).EQ.EXCP(I+1)) Then
            COUNT=COUNT+1
            If(I.LT.NN-1) Then
               Goto 770
            Else
               COUNT2=COUNT+1
            Endif
         Else
            COUNT2=COUNT+1
            COUNT=0
         Endif
750      If((I-COUNT2+1).EQ.1) Then
            Do J=1,COUNT2
               EXPP(J)=(1-((EXCP(I)+EXCP(I+1))/2))/COUNT2
            Enddo
            If(I.EQ.NN-1) Then
               EXPP(NN)=(((EXCP(NN-1)+EXCP(NN))/2) - 0.)/1.
            Endif
         Elseif(I.EQ.NN-1) Then
            If(COUNT2.EQ.NN) Then
               Write(20,760)
760            Format(' WARNING: Exceedance probabilities have same',
     +                ' value.',/,10x,'Results are the same as the',
     +                ' equal-weight option.')
               EXPP=1.0/Real(NN)
               EQFLAG=1
            Elseif(COUNT2.GT.1.and.COUNT2.LT.NN) Then
               Do J=(I-COUNT2+2),(I+1)
                  EXPP(J)=(((EXCP(I+1-COUNT2)+EXCP(I-COUNT2+2))/2) - 
     +                       0.0)/COUNT2
               Enddo
            Else
               Do J=(I-COUNT2+1),(I)
                  EXPP(J)=(((EXCP(I-COUNT2)+EXCP(I-COUNT2+1))/2) - 
     +                    ((EXCP(I)+EXCP(I+1))/2))/COUNT2
               Enddo
               EXPP(NN)=((EXCP(NN-1)+EXCP(NN))/2)-0
            Endif
         Else
            Do J=(I-COUNT2+1),(I)
               EXPP(J)=(((EXCP(I-COUNT2)+EXCP(I-COUNT2+1))/2) - 
     +                 ((EXCP(I)+EXCP(I+1))/2))/COUNT2
            Enddo
         Endif
770   Enddo
780   TOTALP=SUM(EXPP)
      If(ABS(1.0-TOTALP).GT.0.0001) Then
         Write(20,790) TOTALP
790      Format(' WARNING: Probability array sums to',F8.4,
     +          ' rather than 1.000.')
      Endif
!
!  Incremental probabilities, ranks and ratios are sorted into
!  the original sequence order.
!
      Call SORTR(NN,RANK,EXPP,RQ,0)
!
!  Probability array is written to SFF file.
!
      If(FILE2.EQ.1) Then
         If(FILE1.EQ.2) Then
            Do While (1.LT.2)
               Read(8,800,End=820)
800            Format(A2)
            Enddo
         Else
            If(SFFOPEN.EQ.0) Then
               SFFOPEN=SFFOPEN+1
               P=Index(SROOT,'   ')-1
               INPUTSFF=SROOT(:P)//'.SFF'
               Write(*,810) INPUTSFF
810            Format(3x,' Opening SFF File: ',A25,/)
               Open(UNIT=8,FILE=INPUTSFF,FORM='FORMATTED',
     +              ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
            Endif
            Do while (1.LT.2)
               Read(8,800,End=820)
            Enddo
         Endif
820      Write(8,830) TCR1,CR2
830      Format('Probability Array for CR1 of',I3,
     +          ' months and CR2 of month',I2)
         If(FIT.NE.0) Then
            Write(8,840)
840         Format('   Flow Percent   Frequency')
         Else
            Write(8,850)
850         Format('        Flow      Frequency')
         Endif
         SUMX=0.0
         Do I=1,NN
            Write(8,860) RQ(I),EXPP(I)
860         Format(F15.3,F12.7)
            SUMX=SUMX+EXPP(I)
         Enddo
         Write(8,870)
870      Format(15x,12('-'))
         Write(8,880) SUMX
880      Format(10x,'Sum =',F12.7)
      Endif
!
      If(Allocated(TOTAL_FLOW)) Deallocate (TOTAL_FLOW)
      If(Allocated(FLOWS)) Deallocate (FLOWS)
      If(Allocated(RQ)) Deallocate (RQ)
      If(Allocated(EXCP)) Deallocate (EXCP)
      If(Allocated(RANK)) Deallocate (RANK)
      If(Allocated(XFREQ)) Deallocate (XFREQ)
      If(Allocated(QARRAY)) Deallocate (QARRAY)
!
      Return
      End Subroutine CRMIPA
!
! **************************************************************************
!
      Subroutine REGRESSION (N,X,Y,XA,XB,RR,INTZERO)

!  Subroutine REGRESSION performs a linear regression between arrays X and Y.
!                      Y = a + b*X  with a correlation R
!                      Y = XA+XB*X  with a correlation RR
!
      Integer I,INTZERO,N
      Real X(N),Y(N),RR
      Real(8) SXX,SX,SXY,SY,SYY,XA,XB,ST,SR,YM
!
      SXX=0.0
      SX =0.0
      SXY=0.0
      SY =0.0
      SYY=0.0
      ST=0.0
      SR=0.0
      Do I=1,N
         SXX=SXX+X(I)**2
         SX =SX +X(I)
         SXY=SXY+X(I)*Y(I)
         SY =SY +Y(I)
         SYY=SYY+Y(I)**2
      Enddo
      YM=Real(SY/N)
!
      If(N.GT.0) Then
         If((SXX-(SX**2)/N).EQ.0) Then
            Write(20,10)
10          Format(' WARNING: Impossible to perform linear regression',
     +             ' because X array is constant.')
         Endif
         If(INTZERO.EQ.0) Then
            XB=(SXY-(SX*SY)/N)/(SXX-(SX**2)/N)
            XA=(SY-XB*SX)/N
         Else
            XA=0.0
            XB=(SXY/SXX)
         Endif
         Do I=1,N
            ST=ST+(Y(I)-YM)**2
            SR=SR+(Y(I)-XB*X(I)-XA)**2
         Enddo
         RR=(ST-SR)/ST
      Else
         Write(20,20)
20       Format(' WARNING: Zero data pairs for linear regression'
     +          ' performed by Subroutine REGRESSION.')
      Endif
      Return
      End Subroutine REGRESSION
!
! **************************************************************************
!
      Subroutine LOGNORMAL
!
!  Subroutine LOGNORMAL develops the frequency distribution of the array
!  QARRAY using the lognormal probability distribution.
!
      Use CRMVAR
      Real SUMX,PI,Pz(420),ZI,Z(420)
      Real,Allocatable,dimension(:,:)::XFREQ2
      Integer I,J,K,NCOUNT
      If(NUMVAL.LE.1) Then
         Write(20,10)
10       Format(' ERROR: Array to compute frequencies does not have',
     +          ' enough elements.')
         Call ERROR
      Endif
      Allocate(XFREQ2(NUMVAL+1,2))
!
!  Mean and standard deviation are computed.
!
      If(TYPEP.EQ.0) Then
         MEANCRM=Sum(QARRAY(1:NUMVAL))/NUMVAL
         SUMX=0
         Do K=1,NUMVAL
            SUMX=SUMX+(QARRAY(K)-MEANCRM)**2
         Enddo
         STDCRM=Sqrt(SUMX/(NUMVAL-1))
         Call SORT(NUMVAL,QARRAY,1)
      Endif
!
!  Normal probability density table found in statistics books.
!  The table contains 420 values of the standard normal variate
!  Z(I)=z and corresponding cumulative probabilities PZ(I)=f(z).
!
      Z(1)=0.0
      Do I=2,400
         Z(I)=Z(I-1)+0.01
      End Do
      Do I=401,411
         Z(I)=Z(I-1)+0.1
      End Do
      Do I=412,420
         Z(I)=Z(I-1)+0.5
      End Do
!
      Data(PZ(I),I=1,420) /0.5,0.50399,0.50798,0.51197,0.51595,0.51994,
     +0.52392,0.5279,0.53188,0.53586,0.53983,0.5438,0.54776,0.55172,
     +0.55567,0.55962,0.56356,0.56749,0.57142,0.57535,0.57926,0.58317,
     +0.58706,0.59095,0.59483,0.59871,0.60257,0.60642,0.61026,0.61409,
     +0.61791,0.62172,0.62552,0.6293,0.63307,0.63683,0.64058,0.64431,
     +0.64803,0.65173,0.65542,0.6591,0.66276,0.6664,0.67003,0.67364,
     +0.67724,0.68082,0.68439,0.68793,0.69146,0.69497,0.69847,0.70194,
     +0.7054,0.70884,0.71226,0.71566,0.71904,0.7224,0.72575,0.72907,
     +0.73237,0.73565,0.73891,0.74215,0.74537,0.74857,0.75175,0.7549,
     +0.75804,0.76115,0.76424,0.7673,0.77035,0.77337,0.77637,0.77935,
     +0.7823,0.78524,0.78814,0.79103,0.79389,0.79673,0.79955,0.80234,
     +0.80511,0.80785,0.81057,0.81327,0.81594,0.81859,0.82121,0.82381,
     +0.82639,0.82894,0.83147,0.83398,0.83646,0.83891,0.84134,0.84375,
     +0.84614,0.84849,0.85083,0.85314,0.85543,0.85769,0.85993,0.86214,
     +0.86433,0.8665,0.86864,0.87076,0.87286,0.87493,0.87698,0.879,
     +0.881,0.88298,0.88493,0.88686,0.88877,0.89065,0.89251,0.89435,
     +0.89617,0.89796,0.89973,0.90147,0.9032,0.9049,0.90658,0.90824,
     +0.90988,0.91149,0.91308,0.91466,0.91621,0.91774,0.91924,0.92073,
     +0.9222,0.92364,0.92507,0.92647,0.92785,0.92922,0.93056,0.93189,
     +0.93319,0.93448,0.93574,0.93699,0.93822,0.93943,0.94062,0.94179,
     +0.94295,0.94408,0.9452,0.9463,0.94738,0.94845,0.9495,0.95053,
     +0.95154,0.95254,0.95352,0.95449,0.95543,0.95637,0.95728,0.95818,
     +0.95907,0.95994,0.9608,0.96164,0.96246,0.96327,0.96407,0.96485,
     +0.96562,0.96638,0.96712,0.96784,0.96856,0.96926,0.96995,0.97062,
     +0.97128,0.97193,0.97257,0.9732,0.97381,0.97441,0.975,0.97558,
     +0.97615,0.9767,0.97725,0.97778,0.97831,0.97882,0.97932,0.97982,
     +0.9803,0.98077,0.98124,0.98169,0.98214,0.98257,0.983,0.98341,
     +0.98382,0.98422,0.98461,0.985,0.98537,0.98574,0.9861,0.98645,
     +0.98679,0.98713,0.98745,0.98778,0.98809,0.9884,0.9887,0.98899,
     +0.98928,0.98956,0.98983,0.9901,0.99036,0.99061,0.99086,0.99111,
     +0.99134,0.99158,0.9918,0.99202,0.99224,0.99245,0.99266,0.99286,
     +0.99305,0.99324,0.99343,0.99361,0.99379,0.99396,0.99413,0.9943,
     +0.99446,0.99461,0.99477,0.99492,0.99506,0.9952,0.99534,0.99547,
     +0.9956,0.99573,0.99585,0.99598,0.99609,0.99621,0.99632,0.99643,
     +0.99653,0.99664,0.99674,0.99683,0.99693,0.99702,0.99711,0.9972,
     +0.99728,0.99736,0.99744,0.99752,0.9976,0.99767,0.99774,0.99781,
     +0.99788,0.99795,0.99801,0.99807,0.99813,0.99819,0.99825,0.99831,
     +0.99836,0.99841,0.99846,0.99851,0.99856,0.99861,0.99865,0.99869,
     +0.99874,0.99878,0.99882,0.99886,0.99889,0.99893,0.99896,0.999,
     +0.99903,0.99906,0.9991,0.99913,0.99916,0.99918,0.99921,0.99924,
     +0.99926,0.99929,0.99931,0.99934,0.99936,0.99938,0.9994,0.99942,
     +0.99944,0.99946,0.99948,0.9995,0.99952,0.99953,0.99955,0.99957,
     +0.99958,0.9996,0.99961,0.99962,0.99964,0.99965,0.99966,0.99968,
     +0.99969,0.9997,0.99971,0.99972,0.99973,0.99974,0.99975,0.99976,
     +0.99977,0.99978,0.99978,0.99979,0.9998,0.99981,0.99981,0.99982,
     +0.99983,0.99983,0.99984,0.99985,0.99985,0.99986,0.99986,0.99987,
     +0.99987,0.99988,0.99988,0.99989,0.99989,0.999896,0.999900,
     +0.999904,0.999908,0.999912,0.999915,0.999918,0.999922,0.999925,
     +0.999928,0.999931,0.999933,0.999936,0.999938,0.999941,0.999943,
     +0.999946,0.999948,0.999950,0.999952,0.999954,0.999956,0.999958,
     +0.999959,0.999961,0.999963,0.999964,0.999966,0.999967,0.99996833,
     +0.99997934,0.99998665,0.99999146,0.999994587,0.999996602,
     +0.999997888,0.9999987,0.9999992067,0.9999995208,0.9999997133,
     +0.99999998101,0.9999999990134,0.99999999995984,0.99999999999872,
     +0.999999999999968,0.999999999999999,1.0,1.0,1.0/
!
!  Exceedence probabilities for the flow ratios or flows are computed by linear
!  interpolation of the cumulative probabilities in the normal probability table.
!
      Do I=1,NUMVAL
         ZI=(QARRAY(I)-MEANCRM)/STDCRM
         If(ZI.GT.0.0) Then
            Call LINEAR(ZI,PI,Z,Pz,420)
            XFREQ(I,1)=Exp(QARRAY(I))
            XFREQ(I,2)=1-PI
         Elseif(ZI.LT.0.0) Then
            ZI=Abs(ZI)
            Call LINEAR(ZI,PI,Z,Pz,420)
            XFREQ(I,1)=Exp(QARRAY(I))
            XFREQ(I,2)=PI
         Endif
      Enddo
      XFREQ(NUMVAL+1,1)=0.0
      XFREQ(NUMVAL+1,2)=1.0
!
!  Flow ratios are calculated for probabilities of 0.01, 0.001, 0.0001, 0.00001
!  if the highest flow ratio has a probability greater than 0.01 and subroutine
!  LOGNORMAL is called by subroutine CRMSFF to develop the SFF or FF array.
!
      If(TYPEP.LT.1) Then
         J=1
         PI=0.01
         Do I=1,4
            If(XFREQ(1,2).GT.PI.or.J.GE.2) Then
               If(J.EQ.1) Then
                  XFREQ2=XFREQ
                  NCOUNT=NUMVAL+6-I
               Endif
               Call LINEAR((1-PI),ZI,Pz,Z,420)
               XFREQ(5-I,1)=Exp(STDCRM*ZI+MEANCRM)
               XFREQ(5-I,2)=PI
               J=J+1
               PI=PI/10.
            Else
               PI=PI/10.
            Endif
         Enddo
         If(J.GT.1) Then
            XFREQ(J:J+NUMVAL,:)=XFREQ2(:,:)
         Endif
         NUMVAL=NCOUNT
      Endif
      Return
      End Subroutine LOGNORMAL
!
! **************************************************************************
!
      Subroutine LINEAR(GIVEN,FIND,X,Y,NUM)
!
!  Subroutine LINEAR performs linear interpolation. LINEAR is called from
!  Subroutine CRMSFF for Weibull computations, from Subroutine LOGNORMAL to
!  read the normal probability table, and from Subroutine CRMIPA.
!
      Integer NUM,POS
      Real GIVEN,FIND,X(NUM),Y(NUM),X1,X2,Y1,Y2
      Logical ASCENDING
!
      If(NUM.LE.1) Then
         Write(20,10) NUM
10       Format(' ERROR: Array for linear interpolation has',I2,
     +          ' elements.')
         Call ERROR
      Endif 
      ASCENDING=(X(NUM).GT.X(1))
      POS=1
      If(ASCENDING) Then
         Do while (GIVEN.GE.X(POS).and.(POS.LE.NUM))
            POS=POS+1
         Enddo
      Else
         Do while (GIVEN.LE.x(POS).and.(POS.LE.NUM))
            POS=POS+1
         Enddo
      Endif
!
      If(POS.EQ.1) Then
         X1=X(1)
         Y1=Y(1)
         X2=X(2)
         Y2=Y(2)
      Elseif(POS.GT.NUM) Then
         X1=X(NUM-1)
         Y1=Y(NUM-1)
         X2=X(NUM)
         Y2=Y(NUM)
      Else
         X1=X(POS-1)
         Y1=Y(POS-1)
         X2=X(POS)
         Y2=Y(POS)
      Endif
      If(X1.EQ.X2) Then
         If(GIVEN.EQ.X1) Then
            FIND=Y1
         Else
            If(GIVEN.LT.X1) Then
               FIND=1.0
            Else
               FIND=0.0
            Endif
         Endif
      Else
         FIND=Y1+(GIVEN-X1)*((Y2-Y1)/(X2-X1))
      Endif
      Return
      End Subroutine LINEAR
!
!  ***********************************************************************
!
      Subroutine SORTR(N,X,Y,Z,ORDER)
!  
!  Subroutine SORTR is called at two locations in Subroutine CRMIPA to sort
!  the array X in descending (ORDER=1) or ascending (ORDER=0) order while
!  maintaining the rank of corresponding values in the arrays Y and Z.
!
      Integer I,ORDER,N
      Real X(N),Y(N),Z(N),TEMP,TEMP2,TEMP3
      Logical SORTED
!
      If(N.LE.1) Then
         Write(20,10)
10       Format(' WARNING: Subroutine SORTR was called to rank an',
     +          ' array that has zero or only one element.')
         Return
      Endif
!
      SORTED=.FALSE. 
      If(ORDER.EQ.1) Then
         Do While (.NOT.SORTED)
            SORTED=.TRUE.
            Do I=1,N-1
               If(X(I).LT.X(I+1)) Then
                  TEMP=X(I+1)
                  TEMP2=Y(I+1)
                  TEMP3=Z(I+1)
                  X(I+1)=X(I)
                  Y(I+1)=Y(I)
                  Z(I+1)=Z(I)
                  X(I)=TEMP
                  Y(I)=TEMP2
                  Z(I)=TEMP3
                  SORTED=.FALSE.
               Endif
            Enddo
         Enddo
      Else
         Do While (.NOT.SORTED)
            SORTED=.TRUE.
            Do I=1,N-1
               If(X(I).GT.X(I+1)) Then
                  TEMP=X(I+1)
                  TEMP2=Y(I+1)
                  TEMP3=Z(I+1)
                  X(I+1)=X(I)
                  Y(I+1)=Y(I)
                  Z(I+1)=Z(I)
                  X(I)=TEMP
                  Y(I)=TEMP2
                  Z(I)=TEMP3
                  SORTED=.FALSE.
               Endif
            Enddo
         Enddo
      Endif
      Return
      End Subroutine SORTR
!
! **************************************************************************
!
      Subroutine SORT(N,X,ORDER)
!  
!  Subroutine SORT sorts the array X in either descending (ORDER=1) or
!  ascending (ORDER=0) order.
!
      Integer I,ORDER,N
      Real X(N),TEMP
      Logical SORTED
!
      If(N.LE.1) Then
         Write(20,10)
10       Format(' WARNING: Subroutine SORT was called to sort an',
     +          ' array that has zero or only one element.')
         Return
      Endif
!
      SORTED=.FALSE. 
      If(ORDER.EQ.1) Then
         Do while (.NOT.SORTED)
            SORTED=.TRUE.
            Do I=1,N-1
               If(X(I).LT.X(I+1)) Then
                  TEMP=X(I+1)
                  X(I+1)=X(I)
                  X(I)=TEMP
                  SORTED=.FALSE.
               Endif
            Enddo
         Enddo
      Else
         Do while (.NOT.SORTED)
            SORTED=.TRUE.
            Do I=1,N-1
               If(X(I).GT.X(I+1)) Then
                  TEMP=x(I+1)
                  X(I+1)=X(I)
                  X(I)=TEMP
                  SORTED=.FALSE.
               Endif
            Enddo
         Enddo
      Endif
!
      Return
      End Subroutine SORT
!
!  **************************************************************************
!
      Subroutine SIMDHEADER
!
!   Subroutine SIMDHEADER writes a header to identify a table as being
!   compiled from daily (sub-monthly) time step simulation results from
!   a SUB file and also provides the temporal range of the data.
!
      Use COMVAR
!
      Character(len=4)::  bYEAR,eYEAR
      Character(len=10):: bMONTH,eMONTH
!
      Integer::L1,L2
!
      If(BEGMON.EQ.1)  bMONTH='January'
      If(BEGMON.EQ.2)  bMONTH='February'
      If(BEGMON.EQ.3)  bMONTH='March'
      If(BEGMON.EQ.4)  bMONTH='April'
      If(BEGMON.EQ.5)  bMONTH='May'
      If(BEGMON.EQ.6)  bMONTH='June'
      If(BEGMON.EQ.7)  bMONTH='July'
      If(BEGMON.EQ.8)  bMONTH='August'
      If(BEGMON.EQ.9)  bMONTH='September'
      If(BEGMON.EQ.10) bMONTH='October'
      If(BEGMON.EQ.11) bMONTH='November'
      If(BEGMON.EQ.12) bMONTH='December'
      L1=Len_Trim(bMONTH)
!
      If(ENDMON.EQ.1)  eMONTH='January'
      If(ENDMON.EQ.2)  eMONTH='February'
      If(ENDMON.EQ.3)  eMONTH='March'
      If(ENDMON.EQ.4)  eMONTH='April'
      If(ENDMON.EQ.5)  eMONTH='May'
      If(ENDMON.EQ.6)  eMONTH='June'
      If(ENDMON.EQ.7)  eMONTH='July'
      If(ENDMON.EQ.8)  eMONTH='August'
      If(ENDMON.EQ.9)  eMONTH='September'
      If(ENDMON.EQ.10) eMONTH='October'
      If(ENDMON.EQ.11) eMONTH='November'
      If(ENDMON.EQ.12) eMONTH='December'
      L2=Len_Trim(eMONTH)
!
      Write(bYEAR,100) BEGYR
      Write(eYEAR,100) ENDYR
100   Format(I4)
!
      Write(2,200)bMONTH,bYEAR,eMONTH,eYEAR
200   Format('Daily Data Ranging From ',A<L1>,', ',A4,
     +       ' Through ',A<L2>,', ',A4,/)
!
      Return
      End Subroutine SIMDHEADER
!
! *********************************************************************
!
      Subroutine DATE_DATA
!
!  Subroutine DATE_DATA populates a 2-D array, DDATA, with the year,
!  month, and day of each time step from the SIMD daily output file.
!  Since the daily output file can contain any subset of the entire
!  simulation,the date data must be computed month by month using the
!  information on the 5th and 6th records of the daily output file.
!
      Use COMVAR
      Integer::D,DT,MT,TEMP,Y,YEAR
      Logical LEAP
!
      YEAR=BEGYR-1
      D=0
      Do Y=1,(ENDYR-BEGYR+1)
         YEAR=YEAR+1
!
!  Test of whether the YEAR is a leap year.  For a leap year, the logical
!  variable LEAP is assigned the value true, Otherwise, LEAP is false.
!
         TEMP=YEAR
         If ((TEMP/4)*4.EQ.TEMP) Then
            LEAP=.TRUE.
            If((TEMP/400)*400.EQ.TEMP) Then
               LEAP=.TRUE.
            Elseif((TEMP/100)*100.EQ.TEMP) Then
               LEAP=.FALSE.
            Endif
         Else
            LEAP=.FALSE.
         Endif
!
!   Year, month, and day are assigned.
!
         If(NTI.EQ.0.and.LEAP) NDAY(2)=29
         If(YEAR.EQ.BEGYR) Then
            If(YEAR.EQ.ENDYR) Then
               Do MT=BEGMON,ENDMON
                  Do DT=1,NDAY(MT)
                     D=D+1
                     DDATA(D,:)=[YEAR,MT,DT]
                  Enddo
               Enddo
            Else
               Do MT=BEGMON,12
                  Do DT=1,NDAY(MT)
                     D=D+1
                     DDATA(D,:)=[YEAR,MT,DT]
                  Enddo
               Enddo
            Endif
         Elseif(YEAR.GT.BEGYR.and.YEAR.LT.ENDYR) Then
            Do MT=1,12
               Do DT=1,NDAY(MT)
                  D=D+1
                  DDATA(D,:)=[YEAR,MT,DT]
               Enddo
            Enddo
         Elseif(YEAR.EQ.ENDYR) Then
            Do MT=1,ENDMON
               Do DT=1,NDAY(MT)
                  D=D+1
                  DDATA(D,:)=[YEAR,MT,DT]
               Enddo
            Enddo
         Endif
      NDAY(2)=28
      Enddo
!
      End Subroutine DATE_DATA
!
! ***********************************************************************
!
      Subroutine FFA
!
!  *-*-*-*-*-*  7FFA, 7VOL, 7DAM Records  *-*-*-*-*-*
!  Subroutine FFA develops flood frequency/damage tables for annual series
!  read from a SIMD AFF file of peak naturalized flow (ID=1), regulated
!  flow (ID=2), reservoir storage (ID=3), or storage plus exceess (ID=4).
!
      Use COMVAR
!
      Integer CP,FFYRS,FFCPS,ID,I,J,K,N,NUM,NM,RANK,SKEW,TAB,YR
      Real AAD,APSUM,APLSUM,A,B,GR,MSER,MSES,TEMP,WP,AAV,DVV,X1,X2
      Real D(10),DAM(20),DAMQ(10),DV(20),EPI(9),FACTOR(9),FK(31,9),
     +     GK(31),PROB(10),PROBA(10),Q(9),SC1
      Character(len=4) CD
      Logical SORTED
!
      Integer,Allocatable,Dimension(:)::CPFF,CPFX
!
      Real,Allocatable,Dimension(:)::APL,APLMEAN,APLSTD,APMEAN,APSTD,
     +                               APMIN,APMAX,G,GC,GW,SC,X,XAP
      Real,Allocatable,Dimension(:,:)::AP
!
      Character(6),Allocatable,Dimension(:)::CPIDFF,FFCPID
!
!  Pearson type III frequency factor table.
!
      Data (GK(I),I=1,31) /3.0,2.8,2.6,2.4,2.2,2.0,1.8,1.6,1.4,1.2,1.0,
     +0.8,0.6,0.4,0.2,0.0,-0.2,-0.4,-0.6,-0.8,-1.0,-1.2,-1.4,-1.6,-1.8,
     +-2.0,-2.2,-2.4,-2.6,-2.8,-3.0/
      Data ((FK(I,J),I=1,31),J=1,9) /-0.6663,-0.71415,-0.76878,-0.83196,
     +-0.90521,-0.98995,-1.08711,-1.19680,-1.31815,-1.44942,-1.58838,
     +-1.73271,-1.88029,-2.02933,-2.17840,-2.32635,-2.47226,-2.61539,
     +-2.75514,-2.89101,-3.02256,-3.14944,-3.27134,-3.38804,-3.49935,
     +-3.60517,-3.70543,-3.80013,-3.88930,-3.97301,-4.05138,-0.39554,
     +-0.38353,-0.36852,-0.35062,-0.32999,-0.30685,-0.28150,-0.25422,
     +-0.22535,-0.19517,-0.16397,-0.13199,-0.09945,-0.06651,-0.03325,
     +0.0,0.03325,0.06651,0.09945,0.13199,0.16397,0.19517,0.22535,
     +0.25422,0.28150,0.30685,0.32999,0.35062,0.36852,0.38353,0.39544,
     +0.42040,0.45980,0.49872,0.53683,0.57383,0.60944,0.64335,0.67532,
     +0.70512,0.73257,0.75752,0.77986,0.79950,0.81638,0.83044,0.84162,
     +0.84986,0.85508,0.85718,0.85607,0.85161,0.84369,0.83223,0.81720,
     +0.79868,0.77686,0.75211,0.72485,0.69602,0.66603,0.63569,1.18006,
     +1.21013,1.23766,1.26240,1.28412,1.30259,1.31160,1.32900,1.33665,
     +1.34047,1.34039,1.33640,1.32850,1.31671,1.30105,1.28155,1.25824,
     +1.23114,1.20028,1.16574,1.12762,1.08608,1.04144,0.99418,0.94496,
     +0.89464,0.84422,0.79472,0.74709,0.70209,0.66023,2.27780,2.27470,
     +2.26743,2.25581,2.23967,2.21888,2.19332,2.16293,2.12768,2.08758,
     +2.04269,1.99311,1.93896,1.88039,1.81756,1.75069,1.67999,1.60574,
     +1.52830,1.44813,1.36584,1.28255,1.19842,1.11566,1.03543,0.95918,
     +0.88814,0.82315,0.76456,0.71227,0.66585,3.15193,3.11399,3.07116,
     +3.02330,2.97028,2.91202,2.84848,2.77964,2.70556,2.62631,2.54206,
     +2.45298,2.35931,2.26133,2.15935,2.05375,1.94499,1.83361,1.72033,
     +1.60604,1.49188,1.37929,1.26999,1.16584,1.06864,0.97980,0.90009,
     +0.82959,0.76779,0.71377,0.66649,4.05138,3.97301,3.88930,3.80013,
     +3.70543,3.60517,3.49935,3.38804,3.27134,3.14944,3.02256,2.89101,
     +2.75514,2.61539,2.47226,2.32635,2.17840,2.02933,1.88029,1.73271,
     +1.58838,1.44942,1.31815,1.19680,1.08711,0.98995,0.90521,0.83196,
     +0.76878,0.71415,0.66663,4.96959,4.84669,4.71815,4.58393,4.44398,
     +4.29832,4.14700,3.99016,3.82798,3.66073,3.48874,3.31243,3.13232,
     +2.94900,2.76321,2.57583,2.38795,2.20092,2.01644,1.83660,1.66390,
     +1.50114,1.35114,1.21618,1.09749,0.99499,0.90742,0.83283,0.76909,
     +0.71425,0.66666,6.20506,6.01858,5.82629,5.62818,5.42426,5.21461,
     +4.99937,4.77875,4.55304,4.32263,4.08802,3.84981,3.60872,3.36566,
     +3.12169,2.87816,2.63672,2.39942,2.16884,1.94806,1.74062,1.55016,
     +1.37981,1.23132,1.10465,0.99800,0.90854,0.83320,0.76920,0.71428,
     +0.66667/
!
!  The exceedance probabilities adopted for the frequency table are
!  used later in the average annual damage computations.
!
      PROB(1)=0.99
      PROB(2)=0.50
      PROB(3)=0.20
      PROB(4)=0.10
      PROB(5)=0.04
      PROB(6)=0.02
      PROB(7)=0.01
      PROB(8)=0.005
      PROB(9)=0.002
!
!  The number of years FFYRS and number of control points FFCPS are
!  read from the 2nd record of the SIMD AFF output file.
!
      Rewind(11)
      Read(11,10) CD
10    Format(A4)
      Read(11,20,IOSTAT=STATUS) FFYRS,FFCPS
20    Format(2I6)
      If(STATUS.NE.0) Then
         Write(20,25)
25       Format(' ERROR: Fortran IOSTAT error reading',
     +          ' number of years and cps from AFF file.')
         Call ERROR
      Endif
      Read(11,10) CD
!
!  These arrays are allocated.  Others are allocated later as needed.
!
      Allocate(CPIDFF(FFCPS),FFCPID(FFCPS),XAP(FFCPS),CPFF(FFCPS))
      CPFF=0
!
!  The 7FFA record is read from the TIN file.
!
      SC1=0.0
      Read(1,30,IOSTAT=STATUS) CD,ID,TAB,RANK,NUM,SKEW,SC1
30    Format(A4,5I4,F8.0)
      If(ID.EQ.0) ID=1
      If(TAB.EQ.0) TAB=1
      If(STATUS.NE.0) Then
         Write(20,40) CD
40       Format(' ERROR: Fortran IOSTAT error reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(CD.NE.'7FFA') Then
         Write(20,50) CD
50       Format(' ERROR: Read CD of ',A4,' instead of 7FFA.')
         Call ERROR
      Endif
      If(ID.LT.0.or.ID.GT.4) Then
         Write(20,60) ID
60       Format(' ERROR: ID of',I3,' in 7FFA field 2 is not valid.')
         Call ERROR
      Endif
      If(TAB.LT.-1.or.TAB.GT.4) Then
         Write(20,70) TAB
70       Format(' ERROR: TAB of',I3,' in 7FFA field 3 is not valid.')
         Call ERROR
      Endif
      If(SKEW.LT.0.or.SKEW.GT.6) Then
         Write(20,80) SKEW
80       Format(' ERROR: SKEW of',I3,' in 7FFA field 5 is not valid.')
         Call ERROR
      Endif
!
!  Control point indentifiers are read from the IDEN record.
!
      If(NUM.GT.0) Then
         K=1
         N=NUM
         If(NUM.GT.8) N=8
90       Read(1,100,IOSTAT=STATUS) CD,(IDCP(I),I=K,N)
100      Format(A4,8(2x,A6))
         If(STATUS.NE.0) Then
            Write(20,40) CD
            Call ERROR
         Endif
         If(CD.NE.'IDEN') Then
            Write(20,110) CD
110         Format(' ERROR: Read CD of ',A4,' instead of IDEN.')
            Call ERROR
         Endif
!
         If(NUM.GT.N) Then
            K=K+8
            N=N+8
            If(N.GT.NUM) N=NUM
            Goto 90
         Endif
         Do I=1,NUM
            IDCP(I)=Adjustr(IDCP(I))
         Enddo
      Endif
      NUM=Abs(NUM)
!
!  The control points in the AFF file are connected to the NUM control
!  points on the 7FFA record by the array CPFF(CP). If NUM is zero, the
!  control point identifiers (IDCP(CP)) are read from the AFF file. If
!  ID=3 or 4, CPFX(CP) is assigned -99 if there is no storage.
!
      If(NUM.GT.0) Then
         Allocate(CPFX(NUM))
         J=0
         Do CP=1,FFCPS
            Read(11,200) FFCPID(CP),XAP(CP)
200         Format(6x,A6,24x,F12.1)
         EndDo
         Do I=1,NUM
            CP=0
210         CP=CP+1
            If(IDCP(I).EQ.FFCPID(CP)) Then
               CPFF(CP)=I
               CPIDFF(I)=FFCPID(CP)
               If(ID.GE.3.and.XAP(CP).LE.-0.9) CPFX(I)=-99
               Goto 230
            Elseif(CP.EQ.FFCPS) Then
               Write(20,220) IDCP(I)
220            Format(' ERROR: Control point ',A6,' from IDEN record',
     +                ' was not found in in the AFF file.')
               Call ERROR
            Endif
            Goto 210
230      Enddo
      Else
         Allocate(CPFX(FFCPS))
         Do CP=1,FFCPS
            CPFF(CP)=CP
            Read(11,200) FFCPID(CP),XAP(CP)
            CPIDFF(CP)=FFCPID(CP)
            If(ID.GE.3.and.XAP(CP).LE.-0.9) CPFX(CP)=-99
         Enddo
      Endif
      If(NUM.EQ.0) NUM=FFCPS
      Do I=1,NUM
         If(ID.GE.3.and.CPFX(I).EQ.-99) Then
            Write(20,240) IDCP(I)
240         Format(' WARNING: Control point ',A6,' has no storage.')
         Endif
      Enddo
!
!  If SKEW is 4 or 5, 7FFA record SC1 is assigned to all control points.
!
      Allocate(SC(NUM))
      SC=0.0
      If(SKEW.EQ.4.or.SKEW.EQ.5) Then
         Do I=1,NUM
            SC(I)=SC1
         End Do
      Endif
!
!  If SKEW is 2 or 3, skew coefficients are read from
!  SKEW records and stored as the SC array.
!
      If(SKEW.EQ.2.or.SKEW.EQ.3) Then
         Read(1,10) CD
         If(CD.EQ.'SKEW') Then
            Backspace(1)
            K=1
            N=NUM
            If(NUM.GT.16) N=16
250         Read(1,260) (SC(I),I=K,N)
260         Format(4X,16F8.0)
            If(NUM.GT.N) Then
               K=K+16
               N=N+16
               If(N.GT.NUM) N=NUM
               Goto 250
            Endif
         Else
            Write(*,270) CD
270         Format(' ERROR: SKEW record is missing. Read CD of ',A4)
            Call ERROR
         Endif
      Endif
!
!  The annual peaks (AP) are read from the AFF file.
!
      Allocate(AP(NUM,FFYRS),APL(FFYRS))
      AP=0.0
      Rewind(11)
      Do I=1,3
         Read(11,10) CD
      End Do
      Do YR=1,FFYRS
         Do 340 CP=1,FFCPS
            I=CPFF(CP)
            If(I.LE.0) Then
                Read(11,10) CD
                Goto 340
            Endif
            If(I.NE.0) Then
               If(ID.EQ.1) Read(11,280,IOSTAT=STATUS) FFCPID(I),AP(I,YR)
280                        Format(6X,A6,F12.0)
               If(ID.EQ.2) Read(11,290,IOSTAT=STATUS) FFCPID(I),AP(I,YR)
290                        Format(6X,A6,12x,F12.0)
               If(ID.EQ.3) Read(11,300,IOSTAT=STATUS) FFCPID(I),AP(I,YR)
300                        Format(6X,A6,24x,F12.0)
               If(ID.EQ.4) Then
                  Read(11,310,IOSTAT=STATUS) FFCPID(I),X1,X2
310               Format(6X,A6,24x,2F12.0)
                  If(STATUS.NE.0) Then
                     Write(20,320)
                     Call ERROR
                  Endif
                  AP(I,YR)=X1+X2
               Endif
               If(STATUS.NE.0) Then
                  Write(20,320)
320               Format(' ERROR: Fortran IOSTAT error reading',
     +                   ' the AFF file.')
                  Call ERROR
               Endif
               If(FFCPID(I).NE.CPIDFF(I)) Then
                  Write(20,330) FFCPID(I),CPIDFF(I)
330               Format(' ERROR: Read ',A6,' when expecting CP of ',A6,
     +                   ' in the AFF file.')
                  Call ERROR
               Endif
            Endif
340      End Do
      End Do
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Log-Pearson III frequency table is developed either with (TAB=3,4)
!  or without (TAB=1,2) the economic flood damage analysis.
!
      If(TAB.GE.0) Then
         Call TITLES
         Allocate(APMEAN(NUM),APLMEAN(NUM),APSTD(NUM),APLSTD(NUM))
         Allocate(APMIN(NUM),APMAX(NUM))
         Allocate(G(NUM),GC(NUM),GW(NUM))
         G=0.0
         GC=0.0
         GW=0.0
!
!  Headings are written for the frequency table.
!
         If(TAB.GE.0.and.TAB.LE.2) Then
            If(ID.EQ.1) Write(2,350)
            If(ID.EQ.2) Write(2,360)
            If(ID.GE.3) Write(2,370)
350         Format('FLOOD FREQUENCIES FOR NATURALIZED STREAMFLOWS',/)
360         Format('FLOOD FREQUENCIES FOR REGULATED STREAMFLOWS',/)
370         Format('FLOOD FREQUENCIES FOR RESERVOIR STORAGE',/)
         Elseif(TAB.EQ.3.or.TAB.EQ.4) Then
            If(ID.EQ.1) Write(2,380)
            If(ID.EQ.2) Write(2,390)
            If(ID.GE.3) Write(2,400)
380         Format('FLOOD FREQUENCIES FOR NATURALIZED STREAMFLOWS ',
     +             'AND ASSOCIATED ECONOMIC DAMAGES',/)
390         Format('FLOOD FREQUENCIES FOR REGULATED STREAMFLOWS ',
     +             'AND ASSOCIATED ECONOMIC DAMAGES',/)
400         Format('FLOOD FREQUENCIES FOR RESERVOIR STORAGE ',
     +             'AND ASSOCIATED ECONOMIC DAMAGES',/)
         Endif
         If(TAB.GE.0.and.TAB.LE.4) Then
            Write(2,410)
            Write(2,420)
            Write(2,430)
            Write(2,440)
            Write(2,410)
410         Format(99('-'))
420   Format('                  ANNUAL RECURRENCE INTERVAL (YEARS)',
     +       ' AND EXCEEDANCE FREQUENCY (%)')
430   Format('CONTROL    1.01       2        5       10        25     ',
     +       '  50      100      200      500    EXPECTED')
440   Format(' POINT      99%      50%      20%      10%       4%     ',
     +       '  2%       1%     0.5%     0.2%       VALUE')
         Endif
!
!  The log-Pearson III frequency analysis and economic flood damage
!  analysis are performed for each control point in turn in a do-loop.
!
         Do 700 I=1,NUM
            If(ID.GE.3.and.CPFX(I).LT.0) Goto 700
            N=FFYRS
!
!  Means of annual peaks (AP) and log annual peaks (APL) are computed.
!
            APSUM=0.0
            APLSUM=0.0
            Do YR=1,N
               APSUM=APSUM+AP(I,YR)
               If(AP(I,YR).LE.0.001) Then
                  APL(YR)=0.0
               Else
                  APL(YR)=LOG10(AP(I,YR))
               Endif
               APLSUM=APLSUM+APL(YR)
            Enddo
            APMEAN(I)=APSUM/N
            APLMEAN(I)=APLSUM/N
!
!  Standard deviations are computed.
!
            APSUM=0.0
            APLSUM=0.0
            Do YR=1,N
               APSUM=APSUM+((AP(I,YR)-APMEAN(I))**2)
               APLSUM=APLSUM+((APL(YR)-APLMEAN(I))**2)
            Enddo
            APSTD(I)=Sqrt(APSUM/(N-1))
            APLSTD(I)=Sqrt(APLSUM/(N-1))
!
!  Minimum and maximum are computed.
!
            APMAX(I)=MAXVAL(AP(I,1:FFYRS))
            APMIN(I)=MINVAL(AP(I,1:FFYRS))
!
!  Skew coefficient G of logarithms of annual peaks is computed.
!
            APLSUM=0.0
            Do YR=1,N
               APLSUM=APLSUM+((APL(YR)-APLMEAN(I))**3)
            Enddo
            GC(I)=(N*APLSUM) / ((N-1)*(N-2)*APLSTD(I)**3)
            G(I)=GC(I)
!
!  SKEW record coefficients are adopted for SKEW of 2 or 4 on 7FFA record.
!
            If(SKEW.EQ.2.or.SKEW.EQ.4) G(I)=SC(I)
!
!  A weighted skew (GW) is adopted for SKEW of 3 or 5 on 7FFA record.
!
            If(SKEW.EQ.3.or.SKEW.EQ.5) Then
               GR=SC(I)
               If(Abs(G(I)).LE.0.9) A=-0.33+0.08*Abs(G(I))
               If(Abs(G(I)).GT.0.9) A=-0.52+0.3*Abs(G(I))
               If(Abs(G(I)).LE.1.5) B=0.94-0.26*Abs(G(I))
               If(Abs(G(I)).GT.1.5) B=0.55
               MSER=0.302
               MSES=10**(A-B*(LOG10(FFYRS/10.)))
               GW(I)=((MSER*G(I))+(MSES*GR)) / (MSER+MSES)
               G(I)=GW(I)
            Endif
!
!  G is zero for SKEW of 6 on 7FFA record.
!
            If(SKEW.EQ.6) G(I)=0.0
!
!  The log-Pearson III frequency computations are performed for recurrence
!  intervals of 1.01, 2, 5, 10, 25, 50, 100, 200, and 500 years.
!  The frequency factor K is determined for each recurrence interval by
!  linear interpolation of the array storing the Pearson type III table.
!
            If(G(I).GT.3.0.or.G(I).LT.-3.0) Then
               Write(20,470) G(I)
470            Format(' WARNING: Skew coefficient of',F5.2,' falls',
     +                ' outside range of K factor table.')
               If(G(I).GT.3.0)  Write(20,480)
               If(G(I).LT.-3.0) Write(20,490)
480            Format(10x,'A value for G of 3.0 was adopted.')
490            Format(10x,'A value for G of -3.0 was adopted.')
               If(G(I).GT.3.0)  G(I)=3.0
               If(G(I).LT.-3.0) G(I)=-3.0
            Endif
            J=0
500         J=J+1
            If(GK(J).GT.G(I).and.J.LT.31) Goto 500
            If(J.EQ.1) Then
               Do K=1,9
                  If(J.EQ.1) FACTOR(K)=FK(J,K) 
               Enddo
            Elseif (J.EQ.31.and.GK(J).GT.G(I)) Then
               Do K=1,9
                  If(J.EQ.31) FACTOR(K)=FK(J,K) 
               Enddo
            Else
               J=J-1
               Do K=1,9
                  FACTOR(K)=FK(J+1,K)+((G(I)-GK(J+1))
     +                           *(FK(J,K)-FK(J+1,K))/(GK(J)-GK(J+1)))
               Enddo
            Endif
!
!  The flow or storage amount corresponding to each recurrence interval and
!  expected value volume are computed and written to the frequency table.
!
            Do K=1,9
               Q(K)=10**(APLMEAN(I)+FACTOR(K)*APLSTD(I))
            Enddo
            Do K=1,9
            If (K.EQ.1) Then
               EPI(K)=(1-PROB(K))/2.+(PROB(K)-PROB(K+1))/2.
               Elseif (K.GT.1.and.K.LT.9) Then
                  EPI(K)=((PROB(K-1)-PROB(K))+(PROB(K)-PROB(K+1)))/2.
               Elseif (K.EQ.9) Then
                  EPI(K)=PROB(K)+(PROB(K-1)-PROB(K))/2.
               Endif
            Enddo
            AAV=0.0
            Do K=1,9
               AAV=AAV+Q(K)*EPI(K)
            Enddo
            Write(2,510) Adjustl(CPIDFF(I)),(Q(K),K=1,9),AAV
510         Format(A6,F10.0,8F9.0,F11.0)
!
!  The optional economic flood damage information is added to
!  the frequency/damage table.
!
            If(TAB.EQ.3.or.TAB.EQ.4) Then
               Read(1,520,IOSTAT=STATUS) CD,NM
520            Format(A4,I4)
               If(STATUS.NE.0) Then
                  Write(20,530) CD
530               Format(' ERROR: Fortran IOSTAT error occured reading',
     +                   ' an input record with CD of ',A4)
                  Call ERROR
               Endif
               If(CD.NE.'7VOL') Then
                  Write(20,540) CD
540               Format(' ERROR: Read CD of ',A4,' instead of 7VOL.')
                  Call ERROR
               Endif
               If(NM.LT.0.or.NM.GT.20) Then
                  Write(20,550) NM
550               Format(' ERROR: NM of',I3,' on 7VOL record is',
     +                   ' not valid.')
                  Call ERROR
               Endif
               Backspace(1)
               Read(1,560,IOSTAT=STATUS) CD,(DV(J),J=1,NM)
560            Format(A4,4x,20F10.0)
               If(STATUS.NE.0) Then
                  Write(20,530) CD
                  Call ERROR
               Endif
               Read(1,570,IOSTAT=STATUS) CD,K,(DAM(J),J=1,NM)
570            Format(A4,I4,20F10.0)
               If(STATUS.NE.0) Then
                  Write(20,530) CD
                  Call ERROR
               Endif
               If(CD.NE.'7DAM') Then
                  Write(20,580) CD
580               Format(' ERROR: Read CD of ',A4,' instead of 7DAM.')
                  Call ERROR
               Endif
               If(K.NE.NM) Then
                  Write(20,590) K,NM
590               Format(' ERROR: NM of',I3,' on 7DAM record does',
     +                   ' not match NM of',I3,' on 7VOL record.')
                  Call ERROR
               Endif
!
!  The damage in dollars (D(K)) associated with each probability (PROB(K))
!  is determined by applying Q(K) to linearly interpolate from the DV(J)
!  versus DAM(J) relationship read from 7VOL and 7DAM records.
!
               Do K=1,9
                  Do J=1,NM
                     If (J.GE.1.and.J.LT.NM) Then
                        If(Q(K).GE.DV(J).and.Q(K).LT.DV(J+1)) Then
                           DAMQ(K)=DAM(J)+(Q(K)-DV(J))/(DV(J+1)-DV(J))
     +                             *(DAM(J+1)-DAM(J))
                        Endif
                     Else
                        If(Q(K).GE.DV(J-1).and.Q(K).LT.DV(J)) Then
                           DAMQ(K)=DAM(J-1)+(Q(K)-DV(J-1))
     +                            /(DV(J)-DV(J-1))*(DAM(J)-DAM(J-1))
                        Endif
                     Endif
                  Enddo
               Enddo
               If(Q(9).GT.DV(NM)) Then
                  Write(20,600) CPIDFF(I),Q(9),DV(NM),DAM(NM)
600               Format('WARNING: Interpolation routine exceeded ',
     +                   'range of table ',A6,/,10X,'max X =',F9.0,
     +                   '    given X =',F9.0,'    Y = max Y=',F10.0)
                  DAMQ(9)=DAM(NM)
               Endif
!
!  If DV(K) is larger than Q(K), the probability(PROB(K)) is adjusted.
!
               DVV=0.
               Do K=1,NM
               If (DAM(K).EQ.0.and.DV(K).GT.0) Then
                  DVV=DV(K)
               Endif
            Enddo
            Do K=1,9
               If (DVV.LT.Q(1)) Then
                  PROBA(K)=PROB(K)
               Else
                  If (Q(K).LE.DVV.and.Q(K+1).GT.DVV) Then
                     PROBA(K)=PROB(K)-(PROB(K)-PROB(K+1))*(DVV-Q(K))
     +                                 /(Q(K+1)-Q(K))
                  Elseif(Q(K).LT.DVV) Then
                     PROBA(K)=0
                  Else
                     PROBA(K)=PROB(K)
                  Endif
               Endif
            Enddo
            Do K=1,9
            If (K.EQ.1) Then
                  EPI(K)=(1-PROBA(K))/2.+(PROBA(K)-PROBA(K+1))/2.
               Elseif (K.GT.1.and.K.LT.9) Then
                  EPI(K)=((PROBA(K-1)-PROBA(K))+(PROBA(K)-PROBA(K+1)))/2.
               Elseif (K.EQ.9) Then
                  EPI(K)=PROBA(K)+(PROBA(K-1)-PROBA(K))/2.
               Endif
               If(Q(K).LT.DVV) EPI(K)=0
            Enddo
            Do K=1,9
               D(K)=DAMQ(K)*EPI(K)
            Enddo
!
!  Average annual damage (AAD) is computed by numerical integration of the
!  exceedance probability (PROB(K)) versus $ damage (D(K)) relationship.
!
            AAD=0.0
            Do K=1,9
               AAD=AAD+D(K)
            Enddo
!
!  The damages associated with each probability and the average annual
!  damage are written to the frequency/damage table.
!
               Write(2,690) '$',(D(K),K=1,9),AAD
690            Format(A6,F10.0,8F9.0,F11.0)
            Endif
!
!  End of the control point do-loop for the log-Pearson III
!  frequency analysis and economic flood damage analysis.
!
700      Enddo
         If(TAB.GE.1.and.TAB.LE.4) Write(2,410)
         Write(2,*)
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  The optional statistics table is developed.
!
!  The headings are written for the statistics table.
!
         If(ID.EQ.1) Write(2,720)
         If(ID.EQ.2) Write(2,730)
         If(ID.GE.3) Write(2,740)
720      Format('STATISTICS FOR PEAK ANNUAL NATURALIZED STREAMFLOW',/)
730      Format('STATISTICS FOR PEAK ANNUAL REGULATED STREAMFLOW',/)
740      Format('STATISTICS FOR PEAK ANNUAL RESERVOIR STORAGE',/)
         Write(2,750)
         Write(2,760)
         Write(2,770)
         Write(2,780)
         Write(2,750)
750      Format(99('-'))
760      Format(56x,'Statistics for Logarithms of Annual Peaks')
770      Format('CONTROL              STANDARD                     ',
     +          '             STANDARD   INPUT   COMPUTED ADOPTED')
780      Format(' POINT      MEAN    DEVIATION   MINIMUM    MAXIMUM',
     +          '     MEAN   DEVIATION    SKEW     SKEW     SKEW')
!
!  Statistics are written to the statistics table.
!
         Do 800 I=1,NUM
            If(ID.GE.3.and.CPFX(I).LT.0) Goto 800
            Write(2,790) Adjustl(CPIDFF(I)),APMEAN(I),APSTD(I),APMIN(I),
     +                   APMAX(I),APLMEAN(I),APLSTD(I),SC(I),GC(I),G(I)
790         Format(A6,4F11.0,3F10.4,2F9.4)
800      Enddo
         Write(2,750)
      Endif
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  The annual peaks are tabulated in ranked order with
!  Weibull formula probabilities.
!
      If(RANK.GE.1) Then
         Call TITLES
         Allocate(X(FFYRS))
!
!  The title is written for the annual peak versus probability table.
!
         If(ID.EQ.1) Write(2,810)
         If(ID.EQ.2) Write(2,820)
         If(ID.GE.3) Write(2,830)
810      Format('PEAK ANNUAL NATURALIZED STREAMFLOWS',/)
820      Format('PEAK ANNUAL REGULATED STREAMFLOWS',/)
830      Format('PEAK ANNUAL RESERVOIR STORAGE',/)
!
!  The table heading is written.
!
         Write(2,840)
840      Format('-----------',<NUM>(10('-')))
         Write(2,850) (CPIDFF(CP),CP=1,NUM)
850      Format('RANK   P(%)',<NUM>(4x,A6))
         Write(2,840)
!
!  The annual peaks are sorted in ranked order for each control point.
!
         N=FFYRS
         Do 860 CP=1,NUM
            Do I=1,N
               X(I)=AP(CP,I)
            Enddo
            SORTED=.FALSE.
            Do while (.NOT.SORTED)
               SORTED=.TRUE.
               Do I=1,N-1
                  If(X(I).LT.X(I+1)) Then
                     TEMP=X(I+1)
                     X(I+1)=X(I)
                     X(I)=TEMP
                     SORTED=.FALSE.
                  Endif
               Enddo
            Enddo
            Do I=1,N
               AP(CP,I)=X(I)
            Enddo
860      Enddo
!
!  The probabilities and associated annual peaks are written.
!
         Do YR=1,FFYRS
            WP=(Real(YR)/Real(FFYRS+1))*100
            Write(2,870)YR,WP,(AP(CP,YR),CP=1,NUM)
870         Format(I3,F8.2,<NUM>F10.1)
         Enddo
         Write(2,840)
!
      Endif
!
!  Deallocate arrays.
!
      Deallocate(CPIDFF,FFCPID,XAP,CPFF,CPFX,SC,AP,APL)
      If(Allocated(APMEAN))  Deallocate(APMEAN)
      If(Allocated(APLMEAN)) Deallocate(APLMEAN)
      If(Allocated(APSTD))   Deallocate(APSTD)
      If(Allocated(APLSTD))  Deallocate(APLSTD)
      If(Allocated(APMIN))   Deallocate(APMIN)
      If(Allocated(APMAX))   Deallocate(APMAX)
      If(Allocated(G))  Deallocate(G)
      If(Allocated(GC)) Deallocate(GC)
      If(Allocated(GW)) Deallocate(GW)
      If(Allocated(X))  Deallocate(X)
!
!  Return to main program from Subroutine FFA.
!
      Return
      End Subroutine FFA
!
! ***********************************************************************
!
      Subroutine SALT
!
!  *-*-*-*-*-*-*-*-*-*-*-*-*-*  8SAL  Record  *-*-*-*-*-*-*-*-*-*-*-*-*-*
!
!  Subroutine SALT develops tables of monthly or annual series of volumes,
!  loads, or concentrations of control point inflow, storage, and outflow.
!
      Use COMVAR
!
      Real MDATA(12),YTOTAL,SUM(13),MEAN(13)
!
      Integer COUNT,CREC,I,ID,IP,L,LOOP,MONTH,MM,MORE,MT,MYR,N,
     +        NN,NUM,PERIOD,PT,REC1,SKIP,SC,TA,YEAR
      Integer IPLAN,ISTAT,NDSS,NPATH,NVALS
!
      Character(len=2)  DSSDAY
      Character(len=3)  M(23)
      Character(len=4)  CD,CTIME,CTYPE
      Character(len=5)  CUNITS
      Character(len=6)  ID6
      Character(len=8)  HEAD(100,2)
      Character(len=9)  CDATE
      Character(len=32) A,B,C,D,E,F,K
      Character(len=64) CPATH,CNAME
!
!  The order in which months are listed in the table headings is set based
!  on MONTH1 specified in the UNIT record, with a default of MONTH1 = JAN.
!
      L=1
      If(MONTH1.EQ.'  JAN'.or.MONTH1.EQ.'  Jan') L=1
      If(MONTH1.EQ.'  FEB'.or.MONTH1.EQ.'  Feb') L=2
      If(MONTH1.EQ.'  MAR'.or.MONTH1.EQ.'  Mar') L=3
      If(MONTH1.EQ.'  APR'.or.MONTH1.EQ.'  Apr') L=4
      If(MONTH1.EQ.'  MAY'.or.MONTH1.EQ.'  May') L=5
      If(MONTH1.EQ.'  JUN'.or.MONTH1.EQ.'  Jun') L=6
      If(MONTH1.EQ.'  JUL'.or.MONTH1.EQ.'  Jul') L=7
      If(MONTH1.EQ.'  AUG'.or.MONTH1.EQ.'  Aug') L=8
      If(MONTH1.EQ.'  SEP'.or.MONTH1.EQ.'  Sep') L=9
      If(MONTH1.EQ.'  OCT'.or.MONTH1.EQ.'  Oct') L=10
      If(MONTH1.EQ.'  NOV'.or.MONTH1.EQ.'  Nov') L=11
      If(MONTH1.EQ.'  DEC'.or.MONTH1.EQ.'  Dec') L=12
      M(1) ='JAN'
      M(2) ='FEB'
      M(3) ='MAR'
      M(4) ='APR'
      M(5) ='MAY'
      M(6) ='JUN'
      M(7) ='JUL'
      M(8) ='AUG'
      M(9) ='SEP'
      M(10)='OCT'
      M(11)='NOV'
      M(12)='DEC'
      M(13)='JAN'
      M(14)='FEB'
      M(15)='MAR'
      M(16)='APR'
      M(17)='MAY'
      M(18)='JUN'
      M(19)='JUL'
      M(20)='AUG'
      M(21)='SEP'
      M(22)='OCT'
      M(23)='NOV'
!
!  Format statements for building output tables.
!
10    Format('INFLOW VOLUME (',A4,') AT CONTROL POINT ',A6)              !ID=1
20    Format('INFLOW LOAD (',A4,') AT CONTROL POINT ',A6)                !ID=2
30    Format('INFLOW CONCENTRATION (',A4,') AT CONTROL POINT ',A6)       !ID=3
40    Format('RESERVOIR STORAGE VOLUME (',A4,') AT CONTROL POINT ',A6)   !ID=4
50    Format('RESERVOIR STORAGE LOAD (',A4,') AT CONTROL POINT ',A6)     !ID=5
60    Format('RESERVOIR STORAGE CONCENTRATION (',A4,')',
     +       ' AT CONTROL POINT ',A6)                                    !ID=6
70    Format('OUTFLOW VOLUME (',A4,') AT CONTROL POINT ',A6)             !ID=7
80    Format('OUTFLOW LOAD (',A4,') AT CONTROL POINT ',A6)               !ID=8
90    Format('OUTFLOW CONCENTRATION (',A4,') AT CONTROL POINT ',A6)      !ID=9
100   Format(A4,8X,12(A3,6X),2X,A5)
110   Format(/,127('-'))
120   Format(127('-'))
130   Format('DOWNSTREAM FLOW CONCENTRATION (',A4,') AT CONTROL POINT ',
     +        A6)                                                        !ID=10
140   Format('DIVERSION CONCENTRATION (',A4,') AT CONTROL POINT ',A6)    !ID=11
!
!  Table specifications are read from the input file (unit=1) record.
!
      Read(1,220,IOSTAT=STATUS) CD,TA,PT,MORE,ID,SC,NUM
220   Format(A4,6I4)
      If(STATUS.NE.0) Then
         Write(20,230) CD
230      Format(' ERROR: Fortran IOSTAT error occured reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(SC.EQ.0) SC=1
!
!  Error checks.
!
      If(SC.GT.NC) Then
         Write(20,240) SC,NC
240      Format(' ERROR: SC of',I2,' from 8SAL record exceeds NC of',I2)
         Call ERROR
      Endif
      If(TA.LT.0.or.TA.GT.1.or.PT.LT.0.or.PT.GT.5.or.MORE.GT.1) Then
         Write(20,250)
250      Format(' ERROR: Invalid TA, ID, or MORE on 8SAL record.')
         Call ERROR
      Endif
      If(ID.LT.0.or.ID.GT.11) Then
         Write(20,260) ID
260      Format(' ERROR: Invalid ID of',I3)
         Call ERROR
      Endif
!
!  An error message is printed and the record skipped if there are
!  no control points in the SALT output file.
!
      If(NCPTS.EQ.0) Then
         Write(20,270)
270      Format(' ERROR: No control points in the SALT output file.')
         Goto 560
      Endif
!
!   If NUM is greater than zero, the control point
!   identifiers are read from IDEN records.
!
      If(NUM.GT.0) Then
         TID=0
         NID=NUM
         Call IDEN
      Endif
      NUM = Abs(NUM)
!
!  Since the SALT output file (unit=12) is read as direct access,
!  record counters are devised for locating the records to be read.
!
      COUNT = NUM
      CREC =  NCPTS
      REC1 = 7
      If(SC.GT.1) REC1=2+(5*SC)+(SC-1)*NCPTS*NYRS*12
      SKIP = NCPTS
      If(NUM.EQ.0) COUNT=NCPTS
!
!  HEC-DSS file is opened and array allocated.
!
      If(PT.EQ.4.or.PT.EQ.5) Then
         HECDSS=HECDSS+1
!
!  HEC-DSS file VALUES array is allocated.
!
      Endif
!
!  +++++++++++++++++++  Begin Outer COUNT Loop  ++++++++++++++++++++
!  Beginning of loop to develop tables for the COUNT control points.
!  The loop extends from statement 400 to three alternative COUNT If
!  statements, of which the last is at the end of the subroutine.
!
      LOOP = 0
400   LOOP = LOOP + 1
      Do I = 1,13
         SUM(I) = 0.
      End Do
      NDSS=0
      If(TA.GE.1) CALL TITLES
!
!  Find the record of the SALT output file (unit=12) from which
!  the first data will be read for the first entry in table.
!
      RECD = REC1
      Read(12,410,REC=RECD) ID6
410   Format(7x,A6)
!
!  Error check for nonzero NUM that control points listed on
!  TABLES input record are found in the SALT output file.
!
      N = 1
      If(NUM.EQ.0) Go to 440
420   If(ID6.EQ.IDCP(LOOP)) Goto 440
      RECD = RECD + 1
      N = N + 1
      Read(12,410,REC=RECD) ID6
      If (N.LE.CREC) Goto 420
      Write(20,430) IDCP(LOOP), CD
430   Format(' ERROR: Control point ',A6,' from ',A4,' record',
     +       ' was not found in the SALT output file.')
      Call ERROR
!
!  Increment column counter (MPLOT) and develop heading array for plot table.
!
440   If(PT.GE.1.and.PT.LE.3) Then
         MPLOT=MPLOT+1
         HEAD(MPLOT,1)=CD(2:4)
         HEAD(MPLOT,2)=ID6
      Endif
!
!  Assign format specifications for reading the SALT output file
!  and write the table headings to TABLES output file (unit=2).
!
      If (ID.EQ.1) Then
         K='(13X,F11.2)'
         If(TA.GE.1) Write(2,10) UNIT,Adjustl(ID6)
      Elseif (ID.EQ.2) Then
         K='(24X,F11.2)'
         If(TA.GE.1) Write(2,20) UNL,Adjustl(ID6)
      Elseif (ID.EQ.3) Then
         K='(35X,F11.2)'
         If(TA.GE.1) Write(2,30) UNC,Adjustl(ID6)
      Elseif (ID.EQ.4) Then
         K='(46X,F11.2)'
         If(TA.GE.1) Write(2,40) UNIT,Adjustl(ID6)
      Elseif (ID.EQ.5) Then
         K='(57X,F11.2)'
         If(TA.GE.1) Write(2,50) UNL,Adjustl(ID6)
      Elseif (ID.EQ.6) Then
         K='(68X,F11.2)'
         If(TA.GE.1) Write(2,60) UNC,Adjustl(ID6)
      Elseif (ID.EQ.7) Then
         K='(79X,F11.2)'
         If(TA.GE.1) Write(2,70) UNIT,Adjustl(ID6)
      Elseif (ID.EQ.8) Then
         K='(90X,F11.2)'
         If(TA.GE.1) Write(2,80) UNL,Adjustl(ID6)
      Elseif (ID.EQ.9) Then
         K='(101X,F11.2)'
         If(TA.GE.1) Write(2,90) UNC,Adjustl(ID6)
      Elseif (ID.EQ.10) Then
         K='(112X,F11.2)'
         If(TA.GE.1) Write(2,130) UNC,Adjustl(ID6)
      Elseif (ID.EQ.11) Then
         K='(123X,F11.2)'
         If(TA.GE.1) Write(2,140) UNC,Adjustl(ID6)
      Endif
      If(TA.GE.1) Then
         Write(2,110)
         If(ID.EQ.3.or.ID.EQ.6.or.ID.GE.9) Then
            Write(2,100)'YEAR',M(L),M(L+1),M(L+2),M(L+3),M(L+4),M(L+5),
     +               M(L+6),M(L+7),M(L+8),M(L+9),M(L+10),M(L+11),' MEAN'
         Else
            Write(2,100)'YEAR',M(L),M(L+1),M(L+2),M(L+3),M(L+4),M(L+5),
     +               M(L+6),M(L+7),M(L+8),M(L+9),M(L+10),M(L+11),'TOTAL'
         Endif
         Write(2,120)
      Endif
!
! ++++++++++  Begin Inner Loop For Periods ++++++++++
!  Begin loop which is repeated for each of N=NYRS*NPRDS months.
!
      SKIP = SKIP - RECD + REC1
      PERIOD = 0
      MONTH = 0
      YEAR = YRST
      N = NYRS * NPRDS
      YTOTAL = 0.0
450   PERIOD = PERIOD + 1
      MONTH = MONTH + 1
      If (MONTH.EQ.1) Then
         Do I=1,12
            MDATA(I) = 0
         End Do
      Endif
      Read(12,K,REC=RECD) MDATA(MONTH)
      YTOTAL = YTOTAL + MDATA(MONTH)
!
!   Values for DSS file.
!
      If(PT.EQ.4) Then
         NDSS=NDSS+1
         VALUES(NDSS)=MDATA(MONTH)
      Endif
      If(PT.EQ.5) Then
         If(MONTH.EQ.NPRDS) Then
            NDSS=NDSS+1
            If(ID.EQ.1.or.ID.EQ.2.or.ID.EQ.7.or.ID.EQ.8) Then
               VALUES(NDSS)=YTOTAL
            Else
               VALUES(NDSS)=YTOTAL/12
            Endif
         Endif
      Endif
!
!  Write a row in regular table.
!
      If(MONTH.EQ.12) Then
         If(ID.NE.1.and.ID.NE.2.and.ID.NE.7.and.ID.NE.8)YTOTAL=YTOTAL/12
         If(TA.GE.1) Then
            Write(2,460) YEAR,(MDATA(I),I=1,12),YTOTAL
460         Format(I4,3x,12F9.0,F12.0)
         Endif
!
!  Develop 12 months (a year) of a column of plot table array.
!
         If(PT.EQ.1) Then
            Do I=1,12
                IP=PERIOD-12+I
                PLOT(IP,MPLOT)=MDATA(I)
            End Do
         Endif
         If(PT.EQ.2) Then
            MYR=YEAR-YRST+1
            PLOT(MYR,MPLOT)=YTOTAL
         Endif
!
!  Compute means for each month and year if monthly/annual data
!  are finished or otherwise go to next month.
!
         SUM(1) = SUM(1) + MDATA(1)
         SUM(2) = SUM(2) + MDATA(2)
         SUM(3) = SUM(3) + MDATA(3)
         SUM(4) = SUM(4) + MDATA(4)
         SUM(5) = SUM(5) + MDATA(5)
         SUM(6) = SUM(6) + MDATA(6)
         SUM(7) = SUM(7) + MDATA(7)
         SUM(8) = SUM(8) + MDATA(8)
         SUM(9) = SUM(9) + MDATA(9)
         SUM(10) = SUM(10) + MDATA(10)
         SUM(11) = SUM(11) + MDATA(11)
         SUM(12) = SUM(12) + MDATA(12)
         SUM(13) = SUM(13) + YTOTAL
         YTOTAL = 0.0
         MONTH = 0
         YEAR = YEAR + 1
      Endif
      RECD=RECD+CREC
      If(PERIOD.LT.N) Goto 450
      If(NUM.EQ.0) REC1 = REC1 + 1
      Do I=1,13
         MEAN(I)=SUM(I)/NYRS
      End Do
!
!  Place means as last row of regular table.
!
      If(TA.GE.1) Then
         Write(2,470) (MEAN(I),I=1,13)
470      Format('MEAN',3X,12F9.0,F12.0)
         Write(2,120)
      Endif
!
!  Place means in plot array.
!
      If(PT.EQ.3) Then
         Do I=1,12
            PLOT(I,MPLOT)=MEAN(I)
         End Do
      Endif
!
!   DSS data is written to the HEC-DSS file.
!
      If(PT.EQ.4.or.PT.EQ.5) Then
         DSSDAY='01'
         If(DSSMON.EQ.'   ') Then
            CDATE=DSSDAY//M(L)//YRSTDSS
         Else
            CDATE=DSSDAY//DSSMON//YRSTDSS
         Endif
         CTIME='0000'
         CUNITS=UNIT
         CTYPE=CD
         IPLAN=0
         A=OROOT
         B=Adjustr(ID6)
         If(ID.EQ.1) F='ID1'
         If(ID.EQ.2) F='ID2'
         If(ID.EQ.3) F='ID3'
         If(ID.EQ.4) F='ID4'
         If(ID.EQ.5) F='ID5'
         If(ID.EQ.6) F='ID6'
         If(ID.EQ.7) F='ID7'
         If(ID.EQ.8) F='ID8'
         If(ID.EQ.9) F='ID9'
         If(ID.EQ.10) F=' 10'
         If(ID.EQ.11) F=' 11'
         C=CD
         D=CDATE
         If(PT.EQ.4) Then
            E='1MON'
         Elseif(PT.EQ.5) Then
            E='1YEAR'
         Endif
*         Call ZPATH(A,B,C,D,E,F,CPATH,NPATH)
*         Call ZCHKPN(CPATH,NPATH,ISTAT)
         If(ISTAT.NE.0) Then
            Write(20,480) ISTAT,Adjustl(CPATH)
480         Format(' ERROR: DSS ISTAT error',I3,' occurred',
     +             ' for DSS pathname: ',A80)
            Call ERROR
         Endif
*         Call ZSRTS(IFLTAB,CPATH,CDATE,CTIME,NVALS,VALUES,
*     +              CUNITS,CTYPE,IPLAN,ISTAT)
         If(ISTAT.NE.0) Then
            Write(20,490) ISTAT
490         Format(' ERROR: DSS ISTAT error',I3,' occurred',
     +             ' writing data to DSS file.')
            Call ERROR
         Endif
      Endif
!
!   Start over with the next control point.
!
      If(LOOP.LT.COUNT.and.PT.LE.0) Goto 400
      If(LOOP.LT.COUNT.and.PT.GE.1.and.MPLOT.LT.100) Goto 400
!
!   The HEC-DSS file VALUES array is deallocated.
!
      If(PT.EQ.4.or.PT.EQ.5) Then
         Deallocate(VALUES)
      Endif
!
!  Write the plot table.
!
      If((MPLOT.GE.1.and.MORE.EQ.0).or.MPLOT.EQ.100) Then
         Call TITLES
         Write(2,500) (HEAD(I,1),I=1,MPLOT)
500      Format(/,8X,100(7X,A3))
         Write(2,510) (Adjustr(HEAD(I,2)),I=1,MPLOT)
510      Format(8X,100(2X,A8))
         Write(2,520)
520      Format('  ')
         YEAR=YRST-1
         MM=0
         If(PT.EQ.1) Then
            Do MYR=1,NYRS
               YEAR=YEAR+1
               Do MT=1,12
                  MM=MM+1
                     Write(2,530) YEAR,MT,(PLOT(MM,NN),NN=1,MPLOT)
530                  Format(1X,I4,I3,100(F10.1))
               End Do
            End Do
         Elseif(PT.EQ.2) Then
            Do MYR=1,NYRS
               YEAR=YEAR+1
               Write(2,540) YEAR,(PLOT(MYR,NN),NN=1,MPLOT)
540            Format(1X,I4,3X,100(F10.1))
            End Do
         Elseif(PT.EQ.3) Then
            Do MT=1,12
               Write(2,550) MT,(PLOT(MT,NN),NN=1,MPLOT)
550           Format(1X,4X,I3,100(F10.1))
            End Do
         Endif
         MPLOT=0
         If(LOOP.LT.COUNT) Goto 400
      Endif
!
!  Return to main program from Subroutine SALT.
!
560   Return
      End Subroutine SALT
!
! ***********************************************************************
!
      Subroutine SALTFREQ
!
!  *-*-*-*-*-*  8FRE, 8FRQ Records  *-*-*-*-*-*
!
!  Subroutine SALTFREQ develops frequency tables for volumes, loads, and
!  concentrations of control point inflows, storage, and outflows.
!
      Use COMVAR
!
      Real DXF,MEAN,STDDEV,SUM,SUMSD,TEMP,VOLUME,XF
      Real Q(MONTHS),QF(7),F(10),FQ(7),QFREQ(10)
!
      Integer CC,COUNT,CREC,ID,IF1,IF2,I,J,L,N,NM,NUM,LOOP,
     +        REC1,SC,SKIP,VCOUNT,VMONTH
!
      Character(len=4) CD
      Character(len=6) WRAPID
      Character(len=28) K
!
      Logical SORTED
!
!  Specifications for building the frequency table are read from
!  8FRE or 8FRQ record (unit=1).
!
      Read(1,10,IOSTAT=STATUS) CD,ID,SC,CC,NUM
10    Format(A4,4I4)
      If(STATUS.NE.0) Then
         Write(20,20) CD
20       Format(' ERROR: Fortran IOSTAT error occurred reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(SC.EQ.0) SC=1
      If(SC.GT.NC) Then
         Write(20,30) SC,NC
30       Format(' ERROR: SC of',I2,' from 8FRE record exceeds NC of',I2)
         Call ERROR
      Endif
      If(ID.LT.0.or.ID.GT.11) Then
         Write(20,40) ID
40       Format(' ERROR: Invalid ID of',I3)
         Call ERROR
      Endif
!
!  For 8FRE record with NUM greater than zero, the control
!  point identifiers are read from IDEN records.
!
      If(CD.EQ.'8FRE') Then
         If(NUM.GT.0) Then
            TID=0
            NID=NUM
            Call IDEN
         Endif
         NUM = Abs(NUM)
!
!  8FRQ record is read.
!
      Elseif(CD.EQ.'8FRQ') Then
         Backspace(1)
         Read(1,70) CD,ID,SC,CC,NM,IDCP(1),(QF(I),I=1,NM)
70       Format(A4,4I4,2x,A6,7F8.0)
         IDCP(1)=Adjustr(IDCP(1))
         NUM=1
         COUNT=1
         If(SC.EQ.0) SC=1
         If(SC.GT.NC) Then
            Write(20,80) SC,NC
80          Format(' ERROR: SC of',I2,
     +             ' from 8FRQ record exceeds NC of',I2)
            Call ERROR
         Endif
      Endif
!
!  Table headings are written.
!
      Call TITLES
      If(CD.EQ.'8FRE') Then
         If(ID.EQ.1) Write(2,410)
         If(ID.EQ.2) Write(2,420)
         If(ID.EQ.3) Write(2,430)
         If(ID.EQ.4) Write(2,440)
         If(ID.EQ.5) Write(2,450)
         If(ID.EQ.6) Write(2,460)
         If(ID.EQ.7) Write(2,470)
         If(ID.EQ.8) Write(2,480)
         If(ID.EQ.9) Write(2,490)
         If(ID.EQ.10) Write(2,494)
         If(ID.EQ.11) Write(2,496)
         Write(2,400)
         If(ID.EQ.1) Write(2,500)
         If(ID.EQ.1) Write(2,530)
         If(ID.EQ.2) Write(2,510)
         If(ID.EQ.2) Write(2,530)
         If(ID.EQ.3) Write(2,520)
         If(ID.EQ.3) Write(2,530)
         If(ID.EQ.4) Write(2,500)
         If(ID.EQ.4) Write(2,530)
         If(ID.EQ.5) Write(2,510)
         If(ID.EQ.5) Write(2,530)
         If(ID.EQ.6) Write(2,520)
         If(ID.EQ.6) Write(2,530)
         If(ID.EQ.7) Write(2,500)
         If(ID.EQ.7) Write(2,530)
         If(ID.EQ.8) Write(2,510)
         If(ID.EQ.8) Write(2,530)
         If(ID.EQ.9) Write(2,520)
         If(ID.EQ.9) Write(2,530)
         If(ID.EQ.10) Write(2,520)
         If(ID.EQ.10) Write(2,530)
         If(ID.EQ.11) Write(2,520)
         If(ID.EQ.11) Write(2,530)
         Write(2,400)
      Endif
      If(CD.EQ.'8FRQ') Then
         If(ID.EQ.1) Write(2,540) Adjustl(IDCP(1))
         If(ID.EQ.2) Write(2,550) Adjustl(IDCP(1))
         If(ID.EQ.3) Write(2,560) Adjustl(IDCP(1))
         If(ID.EQ.4) Write(2,570) Adjustl(IDCP(1))
         If(ID.EQ.5) Write(2,580) Adjustl(IDCP(1))
         If(ID.EQ.6) Write(2,590) Adjustl(IDCP(1))
         If(ID.EQ.7) Write(2,600) Adjustl(IDCP(1))
         If(ID.EQ.8) Write(2,610) Adjustl(IDCP(1))
         If(ID.EQ.9) Write(2,620) Adjustl(IDCP(1))
         If(ID.EQ.10) Write(2,630) Adjustl(IDCP(1))
         If(ID.EQ.11) Write(2,640) Adjustl(IDCP(1))
         Write(2,90) (('------------------'),L=1,NM)
         If(ID.EQ.1) Write(2,90) (('    FLOW   FREQ(%)'),L=1,NM)
         If(ID.EQ.2) Write(2,90) (('    LOAD   FREQ(%)'),L=1,NM)
         If(ID.EQ.3) Write(2,90) (('     CONC  FREQ(%)'),L=1,NM)
         If(ID.EQ.4) Write(2,90) (('   VOLUME  FREQ(%)'),L=1,NM)
         If(ID.EQ.5) Write(2,90) (('    LOAD   FREQ(%)'),L=1,NM)
         If(ID.EQ.6) Write(2,90) (('     CONC  FREQ(%)'),L=1,NM)
         If(ID.EQ.7) Write(2,90) (('    FLOW   FREQ(%)'),L=1,NM)
         If(ID.EQ.8) Write(2,90) (('    LOAD   FREQ(%)'),L=1,NM)
         If(ID.GE.9) Write(2,90) (('     CONC  FREQ(%)'),L=1,NM)
         Write(2,90) (('------------------'),L=1,NM)
90       Format(7A18)
      Endif
!
!  Record counters are devised for reading the SALT output file.
!
      COUNT=NUM
      CREC=NCPTS
      REC1= 7
      If(SC.GT.1) REC1=2+(5*SC)+(SC-1)*NCPTS*NYRS*12
      SKIP=NCPTS
      If(NUM.EQ.0) COUNT=NCPTS
!
!  Format specifications for reading the SALT output file (unit=12).
!
      If(ID.EQ.1) Then
         K='(7X,A6,F11.2)'
      Elseif(ID.EQ.2) Then
         K='(7X,A6,11x,F11.2)'
      Elseif(ID.EQ.3) Then
         K='(7X,A6,F11.2,11x,F11.2)'
      Elseif(ID.EQ.4) Then
         K='(7X,A6,33x,F11.2)'
      Elseif(ID.EQ.5) Then
         K='(7X,A6,44x,F11.2)'
      Elseif(ID.EQ.6) Then
         K='(7X,A6,33x,F11.2,11x,F11.2)'
      Elseif(ID.EQ.7) Then
         K='(7X,A6,66x,F11.2)'
      Elseif(ID.EQ.8) Then
         K='(7X,A6,77x,F11.2)'
      Elseif(ID.EQ.9) Then
         K='(7X,A6,66x,F11.2,11x,F11.2)'
      Elseif(ID.EQ.10) Then
         K='(7X,A6,66x,F11.2,22x,F11.2)'
      Elseif(ID.EQ.11) Then
         K='(7X,A6,66x,F11.2,33x,F11.2)'
      Endif
!
!  ++++++++++++++ Beginning of Control Point Loop ++++++++++++++
!  Beginning of loop to develop tables for COUNT control points.
!
      LOOP=0
230   LOOP=LOOP+1
      VCOUNT=0
      VMONTH=MONTHS
!
!  The record is found in the SALT output file (unit=12) from
!  which the first data item is read.
!
      If(NUM.GT.0) Then
         RECD=REC1
         Read(12,240,REC=RECD) WRAPID
240      Format(7x,A6)
         N=1
250      If(WRAPID.EQ.IDCP(LOOP)) Goto 270
            RECD=RECD+1
            N=N+1
            Read(12,240,REC=RECD) WRAPID
            If(N.LE.SKIP) Goto 250
            Write(20,260) IDCP(LOOP), CD
260         Format(' ERROR: Identifier ',A6,' from ',A4,' record',
     +             ' was not found in SALT output file.')
            Call ERROR
      Endif
!
!  Q(I) read from SALT output file is either:
!  inflow  volume (ID=1), load (ID=2), concentration (ID=3)
!  storage volume (ID=4), load (ID=5), concentration (ID=6)
!  outflow volume (ID=7), load (ID=8), concentration (ID=9,10,11)
!
      If(NUM.EQ.0) RECD=REC1+LOOP-1
270   Do 280 I=1,MONTHS
         If(ID.EQ.3.or.ID.EQ.6.or.ID.GE.9) Then
            Read(12,K,REC=RECD) WRAPID,VOLUME,Q(I)
            If(VOLUME.LE.0) Then
               Q(I)=0
               VCOUNT=VCOUNT+1
            Endif
         Else
            Read(12,K,REC=RECD) WRAPID,Q(I)
         Endif
         RECD=RECD+CREC
280   End Do
      If(ID.EQ.4.or.ID.EQ.5.or.ID.EQ.6) Then
         If(VCOUNT.EQ.MONTHS) Then
            Write(2,285) Adjustl(WRAPID)
285         Format(A6,6x,'There is no storage at this control point.')
            Goto 350
         Endif
      Endif
!
!  Q(I) is sorted in descending order.
!
290   SORTED=.FALSE.
300   If(.NOT.SORTED) Then
         SORTED=.TRUE.
         Do I=1,MONTHS-1
            If(Q(I).LT.Q(I+1)) Then
               TEMP=Q(I)
               Q(I)=Q(I+1)
               Q(I+1)=TEMP
               SORTED=.FALSE.
            Endif
         End Do
         Goto 300
      Endif
!
!  If CC is non-zero on 8FRE or 8FRQ record, the months
!  with zero volume are not included in determining
!  statistics and frequencies for concentrations.
!
      If(CC.LT.0.or.CC.GT.1) Then
         If(ID.EQ.3.or.ID.EQ.6.or.ID.GE.9) Then
            VMONTH=MONTHS-VCOUNT
         Endif
      Endif
!
!  Mean and standard deviation are computed.
!
      SUM=0
      SUMSD=0.0
      Do I=1,VMONTH
         SUM=SUM+Q(I)
      End Do
      MEAN=SUM/VMONTH
      Do I=1,VMONTH
         SUMSD=SUMSD+(Q(I)-MEAN)**2
      End Do
      STDDEV=(SUMSD/(VMONTH-1))**0.5
!
!  Frequency relationship is developed for 8FRE Record.
!
      If(CD.EQ.'8FRE') Then
         F(1)=0.99
         F(2)=0.98
         F(3)=0.95
         F(4)=0.90
         F(5)=0.75
         F(6)=0.60
         F(7)=0.50
         F(8)=0.40
         F(9)=0.25
         F(10)=0.10
         Do I=1,10
            XF=F(I)*Real(VMONTH)
            IF1=INT(XF)
            IF2=IF1+1
            DXF=XF-Real(IF1)
            QFREQ(I)=(Q(IF1)-Q(IF2))*(1.0-DXF)+Q(IF2)
         End Do
!
!  Table row is written for 8FRE Record.
!
         Write(2,310) Adjustl(WRAPID),VMONTH,MEAN,STDDEV,Q(VMONTH),
     +                (QFREQ(I),I=1,10),Q(1)
310      Format(A6,I5,F9.0,F8.0,F9.1,3F8.1,6F8.0,F9.0,F10.0)
      Endif
!
!  Frequencies for quantities entered on 8FRQ records.
!
      If(CD.EQ.'8FRQ') Then
         Do 330 J=1,NM
            I=0
320         I=I+1
            If(I.GT.VMONTH) Then
               FQ(J)=100.0
               Goto 330
            Endif
            If(QF(J).LE.Q(I)) Then
               Goto 320
            Else
               FQ(J)=(Real(I-1)/Real(VMONTH))*100.0
            Endif
330      End Do
         Write(2,340) (QF(J),FQ(J),J=1,NM)
340      Format(7(F10.1,F8.2))
      Endif
!
! ++++++++++++++++  End of Control Point Loop  +++++++++++++++++++
!
350   If(LOOP.LT.COUNT) Goto 230
      If(CD.EQ.'8FRQ') Write(2,90) (('------------------'),L=1,NM)
      If(CD.EQ.'8FRE') Write(2,400)
400   Format(127('-'))
!
!  Format statements for table headings.
!
410   Format('VOLUME FREQUENCY FOR CONTROL POINT INFLOWS'/)
420   Format('LOAD FREQUENCY FOR CONTROL POINT INFLOWS'/)
430   Format('CONCENTRATION FREQUENCY FOR CONTROL POINT INFLOWS'/)
440   Format('VOLUME FREQUENCY FOR RESERVOIR STORAGE'/)
450   Format('LOAD FREQUENCY FOR RESERVOIR STORAGE'/)
460   Format('CONCENTRATION FREQUENCY FOR RESERVOIR STORAGE'/)
470   Format('VOLUME FREQUENCY FOR CONTROL POINT OUTFLOWS'/)
480   Format('LOAD FREQUENCY FOR CONTROL POINT OUTFLOWS'/)
490   Format('CONCENTRATION FREQUENCY FOR CONTROL POINT OUTFLOWS'/)
494   Format('CONCENTRATION FREQUENCY FOR DOWNSTREAM STREAMFLOWS'/)
496   Format('CONCENTRATION FREQUENCY FOR DIVERSIONS'/)
!
500   Format('CONTROL',14x,'STANDARD',9x,'PERCENTAGE OF MONTHS WITH ',
     +       'FLOWS EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')
510   Format('CONTROL',14x,'STANDARD',9x,'PERCENTAGE OF MONTHS WITH ',
     +       'LOADS EQUALING OR EXCEEDING VALUES SHOWN IN THE TABLE')
520   Format('CONTROL',14x,'STANDARD',6x,'PERCENTAGE OF MONTHS WITH ',
     +       'CONCENTRATION EQUALING OR EXCEEDING VALUES SHOWN IN ',
     +       'THE TABLE')
!
530   Format(' POINT   N     MEAN DEVIATION   100%',5x,'99%',5x,'98%',
     +       5x,'95%',5x,'90%',5x,'75%',5x,'60%',5x,'50%',5x,'40%',5x,
     +       '25%',6x,'10%   MAXIMUM')
!
540   Format('VOLUME FREQUENCY FOR INFLOWS TO CONTROL POINT ',A6,/)
550   Format('LOAD FREQUENCY FOR INFLOWS TO CONTROL POINT ',A6,/)
560   Format('CONCENTRATION FREQUENCY FOR INFLOWS TO CONTROL POINT ',
     +        A6,/)
570   Format('VOLUME FREQUENCY FOR STORAGE AT CONTROL POINT ',A6,/)
580   Format('LOAD FREQUENCY FOR STORAGE AT CONTROL POINT ',A6,/)
590   Format('CONCENTRATION FREQUENCY FOR STORAGE AT CONTROL POINT ',
     +        A6,/)
600   Format('VOLUME FREQUENCY FOR OUTFLOWS FROM CONTROL POINT ',A6,/)
610   Format('LOAD FREQUENCY FOR OUTFLOWS FROM CONTROL POINT ',A6,/)
620   Format('CONCENTRATION FREQUENCY FOR OUTFLOWS FROM CONTROL POINT ',
     +        A6,/)
630   Format('CONCENTRATION FREQUENCY FOR DOWNSTREAM STREAMFLOWS ',A6,/)
640   Format('CONCENTRATION FREQUENCY FOR DIVERSIONS ',A6,/)
!
!  Return to main program from Subroutine SALTFREQ.
!
      Return
!
      End Subroutine SALTFREQ
!
! ***********************************************************************
!
      Subroutine SALTSUM
!
!  *-*-*-*-*-*  8SUM Record  *-*-*-*-*-*
!
!  Subroutine SALTSUM develops a control point salinity summary table.
!
      Use COMVAR
!
      Real X(6),XSUM(NCPTS,6),XMEAN(6),CONC(3)
!
      Integer CP,I,M,SC
!
      Character(len=4) CD
      Character(len=6) CPID(NCPTS)
!
      X=0.0
      XSUM=0.0
!
      Call TITLES
!
!  8SUM record is read from the TIN file (unit 1).
!
      Read(1,10,IOSTAT=STATUS) CD,SC
10    Format(A4,I4)
      If(STATUS.NE.0) Then
         Write(20,20) CD
20       Format(' ERROR: Fortran IOSTAT error occurred reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
      If(SC.EQ.0) SC=1
      If(SC.GT.NC) Then
         Write(20,30) SC,NC
30       Format(' ERROR: SC of',I2,' from 8CPS record exceeds NC of',I2)
         Call ERROR
      Endif
!
!  Table headings witten to TAB file (unit 2).
!
      Write(2,40)
      Write(2,50)
      Write(2,60) UNIT,UNL,UNC
      Write(2,70)
      Write(2,50)
40    Format('CONTROL POINT SUMMARY',/)
50    Format(97('-'))
60    Format('CONTROL    MEAN MONTHLY VOLUME (',A5,')',
     +              '    MEAN MONTHLY LOAD (',A4,')  ',
     +              '    MEAN CONCENTRATION (',A4,')')
!
70    Format(' POINT     Inflow   Outflow   Storage',
     +              '    Inflow   Outflow   Storage',
     +              '    Inflow   Outflow   Storage')
!
!  The SALT output file (unit 12) is read..
!
!                X(1) - Inflow Volume
!                X(2) - Inflow Load
!                X(3) - Storage Volume
!                X(4) - Storage Load
!                X(5) - Outflow Volume
!                X(6) - Outflow Load
!
      RECD= 6
      If(SC.GT.1) RECD=1+(5*SC)+(SC-1)*NCPTS*NYRS*12
      Do M=1,MONTHS
         Do CP=1,NCPTS
            RECD=RECD+1
            Read(12,80,REC=RECD) CPID(CP),X(1),X(2),X(3),X(4),X(5),X(6)
80          Format(7x,A6,2F11.2,11x,2F11.2,11x,2F11.2)
            Do I=1,6
               XSUM(CP,I)=XSUM(CP,I)+X(I)
            End Do
         End Do
      End Do
!
!  Volumes and loads are averaged. Concentrations are computed.
!
!                CONC(1) - Inflow Concentration
!                CONC(2) - Storage Concentration
!                CONC(3) - Outflow Concentration
!
      Do CP=1,NCPTS
         Do I=1,6
            XMEAN(I)=XSUM(CP,I)/(NYRS*12)
         End Do
         If(Abs(XSUM(CP,1)).GT.0.0001) Then
            CONC(1)=(XSUM(CP,2)/XSUM(CP,1))*CF
         Else
            CONC(1)=0.0
         Endif
         If(Abs(XSUM(CP,3)).GT.0.0001) Then
            CONC(2)=(XSUM(CP,4)/XSUM(CP,3))*CF
         Else
            CONC(2)=0.0
         Endif
         If(Abs(XSUM(CP,5)).GT.0.0001) Then
            CONC(3)=(XSUM(CP,6)/XSUM(CP,5))*CF
         Else
            CONC(3)=0.0
         Endif
!
!  Data are written to the table in the TAB file.
!
         CPID(CP)=Adjustl(CPID(CP))
         Write(2,90) CPID(CP),XMEAN(1),XMEAN(5),XMEAN(3),XMEAN(2),
     +               XMEAN(6),XMEAN(4),CONC(1),CONC(3),CONC(2)
90       Format(A6,F11.0,5F10.0,3F10.1)
      End Do
      Write(2,50)
!
!  Return to main program from Subroutine SALTSUM.
!
      Return
      End Subroutine SALTSUM
!
! ***********************************************************************
!
      SUBROUTINE SALTREL
!
!  *-*-*-*-*-* 8REL, 8CON Records *-*-*-*-*-*
!
!  Subroutine SALTREL develops for specified control points a table
!  of diversion reliabilities with and without salinity constraints.
!
      Use COMVAR
!
      Integer COUNT,CREC,I,IC,K,LOOP,MONTH,MT,N,NM,NUM,NUMC,PS1,PS2,
     +        PS3,PT1,REC1,RFLAG,SKIP,ZC
!
      Real AS1,AS2,AS3,AT,CONC(10),PERREL1,PERREL2,PERREL3,REL1,REL2,
     +     REL3,SUMAS1,SUMAS2,SUMAS3,SUMAT,TS1,TS2,TS3,TT1,VOLREL1,
     +     VOLREL2,VOLREL3
!
      Real,Allocatable,Dimension(:,:)::DC,DS,DT
!
      Character(len=4) CD
      Character(len=6) WRAPID,CHAR
!
!  Table specifications are read from TIN input file (unit=1).
!
      Read(1,10,IOSTAT=STATUS) CD,NUMC,RFLAG,NUM,CONC(1)
10    Format(A4,3I4,F8.0)
      If(STATUS.NE.0) Then
         Write(20,20) CD
20       Format(' ERROR: Fortran IOSTAT error occured reading an',
     +          ' input record with CD of ',A4)
         Call ERROR
      Endif
!
!  If NUM is greater than zero, the control
!  point identifiers are read from IDEN records.
!
      If(NUM.GT.0) Then
         TID=0
         NID=NUM
         Call IDEN
      Endif
      NUM = Abs(NUM)
!
!  Concentrations are read from CONC record if more than one salt constituent.
!
      If(NUMC.GT.1) Then
         Read(1,50,IOSTAT=STATUS) CD,(CONC(I),I=1,NUMC)
50       Format(A4,4x,<NUMC>F8.0)
         If(STATUS.NE.0) Then
            Write(20,20) CD
            Call ERROR
         Endif
         If(CD.NE.'8CON') Then
            Write(20,60) CD
60          Format(' ERROR: Expecting 8CON record but read CD of ',A4)
            Call ERROR
         Endif
         If(NC.LT.NUMC) Then
            Write(20,70) NUMC,NC
70          Format(' ERROR: Number of constituents on 8REL/8CON',
     +             ' records is',I4,/,8x,' which is greater',
     +             ' than SALT output file NC of',I4)
            Call ERROR
         Endif
      Endif
!
!  Reliability table headings are written.
!
      Call TITLES
      Write(2,100)
      Write(2,*)
      Write(2,110)
      Write(2,120)
      Write(2,130)
      Write(2,140)
      Write(2,150) UNIT,UNIT,UNIT,UNIT
      Write(2,110)
100   Format('RELIABILITIES WITH AND WITHOUT SALINITY CONSTRAINTS')
110   Format(111('-'))
120   Format(19x,'| Both Quantity & Quality |----- Quantity Only',
     +           ' -----|+++++ Quality Only ++++++|Number Months')
130   Format('CONTROL   TARGET   |',3(11x,'*RELIABILITY* |'),
     +       'Concentration')
140   Format(' POINT   DIVERSION |',3(' SHORTAGE |VOLUME|PERIOD |'),
     +       '  is |exceeds')
150   Format(8x,'(',A5,'/YR) |',3('(',A5,'/YR)|  (%) |  (%)  |'),
     +          ' Zero| Limit')
!
!  SALT simulation results arrays are dimensioned.
!
      NM=NYRS*12
      Allocate(DT(NM,NC),DS(NM,NC),DC(NM,NC))
!
!  Summation variables for bottom-of-table totals are initialized.
!
      SUMAT=0.0
      SUMAS1=0.0
      SUMAS2=0.0
      SUMAS2=0.0
!
!  Record counters are devised for the direct access SALT output file.
!
      COUNT=NUM
      If(NUM.EQ.0) COUNT=NCPTS
      CREC=NCPTS
      SKIP=NCPTS
      REC1=7
!
!  +++++++++++++++++++ Beginning of Control Point Loop ++++++++++++++++++
!
!  The SALT simulation results and reliability computations are performed
!  for each of the COUNT control points.  LOOP counts the control points.
!
      LOOP = 0
200   LOOP = LOOP + 1
!
!  The record of the SALT output file is found from which will be read
!  the permitted diversion and shortage for the first period (month).
!
      RECD = REC1
      Read(12,210,REC=RECD) WRAPID
210   Format(7x,A6)
      N=1
!
!  Existence of control point is checked.
!
      If(NUM.GT.0) Then
220      If (WRAPID.NE.IDCP(LOOP)) Then
            RECD = RECD + 1
            N = N + 1
            Read(12,210,REC=RECD) WRAPID
            If(N.LE.SKIP+1) Goto 220
            Write(20,230) IDCP(LOOP), CD
230         Format(' ERROR: Identifier ',A6,' from ',A4,' record',
     +             ' was not found in SALT output file.')
            Call ERROR
         Endif
      Endif
!
!  Initialize variables.
!
      PT1=0
      PS1=0
      PS2=0
      PS3=0
      TT1=0.0
      TS1=0.0
      TS2=0.0
      TS3=0.0
      AT=0.0
      AS1=0.0
      AS2=0.0
      AS3=0.0
      ZC=0
      If(NUM.EQ.0) SKIP = SKIP-RECD+REC1
!
!  ++++++++++++ Beginning of Salt Constituent Loop ++++++++++++
!  The SALT output file is read for each of the NC constituents.
!
      IC=0
240   IC=IC+1
!
!  +++++++++++++++++ Beginning of Period Loop +++++++++++++++++
!  Beginning of inner loop that reads the SALT output file for
!  the NM months.
!
      MT=0
      MONTH=0
250   MT = MT+1
      MONTH = MONTH+1
      If (MONTH.LT.NPRDS) Then
         Read(12,260,REC=RECD) DC(MT,IC),DT(MT,IC),DS(MT,IC)
260      Format(123x,3F11.2)
      Else
         Read(12,270,REC=RECD) CHAR,DC(MT,IC),DT(MT,IC),DS(MT,IC)
270      Format(7x,A6,110x,3F11.2)
         If (CHAR.NE.WRAPID) Then
            Write(20,280) CHAR, MT, WRAPID
280         Format(' ERROR: Identifier of ',A6, ' read from output',
     +             ' file in period ',I5,' does not match ID of ',A6)
            Call ERROR
         Endif
      Endif
!
!  ++++++++++++ End of Period and Constituent Loops +++++++++++++
!  The loops to read the SALT simulation results are repeated for
!  each month and each constituent.
!
      RECD=RECD+CREC
      If(MT.LT.NM) Goto 250
      If(IC.LT.NUMC) Then
         RECD=RECD+3
         Goto 240
      Endif
!
!  Diversion target and shortage volumes are accumulated and
!  months are counted.
!
      Do 430 MT=1,NM
         If(DT(MT,1).GT.0.001) Then
            TT1=TT1+DT(MT,1)
            PT1=PT1+1
!
!        Quantity only.
!
            If(DS(MT,1).GT.0.001) Then
               TS1=TS1+DS(MT,1)
               PS1=PS1+1
            Endif
!
!        Quality only.
!
            IC=0
400         IC=IC+1
            If(DC(MT,IC).GT.CONC(IC)) Then
               TS2=TS2+DT(MT,IC)
               PS2=PS2+1
               Goto 410
            Endif
            If(IC.LT.NC) Goto 400
!
!        Both quality and quantity.
!
410         IC=0
420         IC=IC+1
            If(DS(MT,IC).GT.0.001.or.DC(MT,IC).GT.CONC(IC)) Then
               If(DC(MT,IC).GT.CONC(IC)) Then
                  TS3=TS3+DT(MT,IC)
                  PS3=PS3+1
                  Goto 430
               Endif
               If(IC.LT.NC) Goto 420
               TS3=TS3+DS(MT,IC)
               PS3=PS3+1
            Endif
!
!        Number of months with zero concentration.
!
            K=0
            Do IC=1,NC
               If(DC(MT,IC).GE.0.001) K=K+1
            End Do
            If(K.EQ.0) ZC=ZC+1
         Endif
!
430   End Do
!
!  Volume and period reliablities and target and shortage totals
!  are computed and written to the reliability table (unit=2).
!
      If(TT1.LE.0) Then                                                  !June09
         Write(2,440) Adjustl(WRAPID)
440      Format(A6,9x,'0.0       0.00   There are no diversions',
     +                ' at this control point.')
      Else
         If(RFLAG.NE.0) Then
            PERREL1 = ((Real(NM-PS1))/Real(NM))*100.0
            PERREL2 = ((Real(NM-PS2))/Real(NM))*100.0
            PERREL3 = ((Real(NM-PS3))/Real(NM))*100.0
         Else
            PERREL1 = ((Real(PT1-PS1))/Real(PT1))*100.0
            PERREL2 = ((Real(PT1-PS2))/Real(PT1))*100.0
            PERREL3 = ((Real(PT1-PS3))/Real(PT1))*100.0
         Endif
         VOLREL1 = ((TT1-TS1)/TT1)*100.0
         VOLREL2 = ((TT1-TS2)/TT1)*100.0
         VOLREL3 = ((TT1-TS3)/TT1)*100.0
         AT = TT1/Real(NYRS)
         AS1= TS1/Real(NYRS)
         AS2= TS2/Real(NYRS)
         AS3= TS3/Real(NYRS)
         SUMAT=SUMAT+AT
         SUMAS1=SUMAS1+AS1
         SUMAS2=SUMAS2+AS2
         SUMAS3=SUMAS3+AS3
         Write(2,450) Adjustl(WRAPID),AT,AS3,VOLREL3,PERREL3,AS1,
     +                VOLREL1,PERREL1,AS2,VOLREL2,PERREL2,ZC,PS2
450      Format(A6,F12.1,F12.2,F7.2,F7.2,2(F12.2,F7.2,F7.2),I7,I6)
      Endif
!
!  +++++++++++++++ End of Control Point Loop +++++++++++++++
!  The computations are repeated for the next control point.
!
      If(NUM.EQ.0) REC1=REC1+1
460   If(LOOP.LT.COUNT) Goto 200
!
!  Totals.are written at the bottom of the table.
!
      REL1=((SUMAT-SUMAS1)/SUMAT)*100.0
      REL2=((SUMAT-SUMAS2)/SUMAT)*100.0
      REL3=((SUMAT-SUMAS3)/SUMAT)*100.0
      Write(2,110)
      Write(2,480) SUMAT,SUMAS3,REL3,SUMAS1,REL1,SUMAS2,REL2
480   Format('Total',F13.1,F12.2,F7.2,F19.2,F7.2,F19.2,F7.2)
      Write(2,110)
!
!  Return to main program from Subroutine SALTREL.
!
      Deallocate(DT,DS,DC)
      Return
!
      End Subroutine SALTREL
!
! *************************************************************************
!                          End of Program TABLES
! *************************************************************************
