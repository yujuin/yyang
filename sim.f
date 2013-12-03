!***************************************************************************!
!                                                                           !
!        Water Rights Analysis Package (WRAP) Simulation (SIM) Model        !
!                                 WRAP-SIM                                  !
!                           January 2011 Version                            !
!                                                                           !
!    WRAP-SIM is a component of the Water Rights Analysis Package (WRAP)    !
!    which is documented by:                                                !
!      Water Rights Analysis Package (WRAP) Modeling System, Reference      !
!      and Users Manuals, TWRI TR-255 & TR-256, 7th Edition, July 2010.     !
!                                                                           !
!***************************************************************************!
!
      Module COMVAR
!
!  Module COMVAR (COMmon VARiables) specifies the type and the number of array
!  dimensions for variables used in common by the subroutines and main program.
!  Dimension limits for arrays are allocated later after subroutine READDAT
!  counts input records from the DAT file.
!
      Integer MAXCI,MAXCP,MAXDI,MAXDU,MAXEA,MAXFS,MAXFSP,MAXFY,MAXGAG,
     +        MAXGP,MAXML,MAXMON,MAXMS,MAXOS,MAXOSS,MAXPOW,MAXPX,MAXRES,
     +        MAXRF,MAXRO,MAXSWR,MAXSYS,MAXTAB,MAXTO,MAXTS,MAXTSWR,
     +        MAXUSE,MAXWR,MAXWS
!
      Integer ADJINC,ALL,AVFLAG,BES,BRS,CLFLAG,CPFLAG,CPOUT,CR1,CR2,CR3,
     +        CR5,DIFLAG,DSS(7),DUAL1,EAFLAG,EFLAG,EPADJ,EUNIT,F1,F2,F3,
     +        F4,F5,F7,F8,F9,FAD,FDFLAG,FSFLAG,FSM,FYLEVEL,FYN,GPNUM,IQ,
     +        ICHECK,IFFLAG,IFPASS,INEV,INEVN,IRO,IUNIT,JRES,LOCNUM,MFY,
     +        MT,MSFLAG,NCPOUT,NCPO,NCPTS,NDD,NDS,NDT,NEGINC,NEV,NEVCP,
     +        NFACP,NG,NGOUT,NIF,NIFLAG,NIFLAG2,NIN,NINCP,NP,NPOPT,NRES,
     +        NPRDS,NPTABL,NPT,NREOUT,NTABLE,NTWTABL,NUMPOW,NUMXP,NUSES,
     +        NWROUT,NWRTS,NYRS,OSFLAG,OUTFILE,OUTPT,OUTWR,PASS2,P1,P2,
     +        RETNUM,RI,RUF,RUFIN,SCOUNT,SIM2,SIM3,STATUS,STODI,STOFLG,
     +        SWR,SYSOUT,TL,TLD,WRT,WR,YEAR,YRST,ZZ,ZZCALL,ZZR
!
      Real AMT,AVAMT,CNLB,CNUB,CR4,DEP,DEPTHX,EAFACT,EASBEG,EASEND,EPWL,
     +     EVCFA,EVCFB,EVCFC,EVX,FRMPOW,INRES,INX,MAKEUP,MPLB,MPUB,
     +     OUTRES,POWFCT,POWPRO,RELS,RET,RFAC,SHT,XAV,XAX,XCPCLAD,
     +     XCPAV1,ZZX
!
      Character(len=2)  CD
      Character(len=3)  DSSMONTH
      Character(len=4)  DSSYEAR
      Character(len=6)  RCP
      Character(len=16) ZZWR
      Character(len=116) ROOT,ROOTH
      Character(len=120) NAME,NAME2
      Character(len=80) TITLE1,TITLE2,TITLE3
!
!  Allocatable arrays with dimensions that are set by the number
!  of control points (MAXCP) and number of water rights (MAXWR).
!
      Real,Allocatable,Dimension(:)::ADL,ANNDEP,ARW,BUX,CL,CNGAGE,CNUG,
     +                   DAGAGE,DAUG,DEPSUM,DMRSUM,EWA,IFTARGET,MONDEP,
     +                   MPGAGE,MPUG,MRW,REGFLOW,RESREL,WSHED
      Real,Allocatable,Dimension(:,:)::CINF,CLOSS,COEF,CPDT,EVAPR,
     +                                 FSX,INFLOW,WRDAT
      Real,Allocatable,Dimension(:,:,:)::CPFLOW,FAFLOW,FLOW,EVAP,RUFA
!
      Integer,Allocatable,Dimension(:)::CPNXT,DINDEX,DUAL,EAX,EVLYR,
     +                  IDSG,IFRESREL,IFMETH,INLYR,INMETHOD,INWS,IRF,
     +                  ISHT,NOTF,RANK,RFMETH,SINDEX,SERIES,WRTO
      Integer,Allocatable,Dimension(:,:)::EAI,FSI,IGAGE,LM,NGAGE,
     +                                    NICP,WRNUM
!
      Character(len=6),Allocatable,Dimension(:)::CPEV,CPIN
      Character(len=6),Allocatable,Dimension(:,:)::CPID
      Character(len=8),Allocatable,Dimension(:,:)::WRIDS
      Character(len=16),Allocatable,Dimension(:)::WRID
!
!  Allocatable arrays with dimensions not set by MAXCP and MAXWR.
!
      Real,Allocatable,Dimension(:)::AFMIN,AFMAX,CUMBEG,CUMCAP,CUMEND,
     +                  CUMEV,DIFACT,EAL,FACT,FYFACT,FYIN,SYSREL,TPCAP,
     +                  TELEV,TQCAP,XAMIN,XAMAX
      Real,Allocatable,Dimension(:,:)::AFX,DIPER,DISTO,EAF,EVCURV,MSD,
     +                  PDUSCF,PVCURV,RESDAT,RF,OS,STMON,TOLIM,TWCURV,
     +                  WRSYS,XA,ZZF,ZZFX
      Real,Allocatable,Dimension(:,:,:)::DSSCP,DSSRE,DSSWR,QTS,SYSTEM
!
      Integer,Allocatable,Dimension(:)::BUWR,DD,DINUM,DUALX,DSSCPI,
     +          DSSWRI,DSSREI,EAO,EAR,EARNUM,EMPTY,FSOR,FYWR,ICP,IDCPEV,
     +          IDCPFA,IDCPIN,MSRES,OSRES,NEAF,NSR,TOCP,TOFLAG,TORI,
     +          TOTARGET,TOWI,XAXFLAG,XAFFLAG,XCP,XCPI,XP,XPR,ZZI
      Integer,Allocatable,Dimension(:,:)::FSN,RESNUM,SN1,SN2,SN3
!
      Character(len=3),Allocatable,Dimension(:)::TOCOMB,TSCOM
      Character(len=6),Allocatable,Dimension(:)::CPOUID,REOUID,
     +                                           RESID,USEID
      Character(len=6),Allocatable,Dimension(:,:)::DIRES,EARES
      Character(len=8),Allocatable,Dimension(:)::GROUP,BUG
      Character(len=16),Allocatable,Dimension(:)::WROUT
!
      End Module COMVAR
!  _________________________________________________________________________
!  *************************************************************************
!
!  MAIN PROGRAM OF WRAP-SIM
!
      Program WRAPSIM
!
      Use COMVAR
!
!  Variables Exclusive to Main Program
!
      Integer BESFLAG,BSFLAG,CPO,CRBEGIN,CRBEGIN1,CRI,CRLOOP,CRM,CRMN,
     +        CRMON,CRNWR,CRSIM,CRSKIP,CRWRITE,CRWRITEN,DSSCPJ,DSSREJ,
     +        DSSWRJ,DUALFLAG,EAZERO,FSJJ,FYNCOUNT,FS,I,I1,I2FS,IBACKUP,
     +        IFCM,IFCT,IFY,IR,IBES,ITS,IWR,J,JFY,JJ,K,KK,L,M,MATCH,MCP,
     +        MCPO,MMT,MSFLAG2,MT1,N,NCP,NLOOP,NSEQ,NSHT,NUSCP,NWRREC,
     +        OSFLAG2,RE,RECD,SEASON,SQEND,SQSTART,SQSTART1,YEAR1,Z
!
      Real AA,ADJAMT,AMT1,AMT2,AMT3,AMT4,AMT5,AMT6,AMT7,AMT8,AMTRF,AS,
     +     ASF,BPAREA,BPSTOR,CIN,CLX,DEPLIM,DF,DRAWDOWN,EASCAP,EPAREA,
     +     EPSTOR,EVOL,FSVX,FYAA,FYAMT,IFSHORT,LIMIT,LOSS,PERCENT,PR,R1,
     +     R2,R3,R4,R5,R6,R7,R8,RESCAP,RFLOW,RFOUT,SPILL,STORAGE,SUMX,
     +     SUMAMT,SUMWSD,TAR,TOTCAP,TOTSHT,T7AMT,VR,WSE,XBES,XCPAV,
     +     XPRESDAT,XRES,X,X1,X2,X3,XAVAMT,YRSUM
!
      Integer,Allocatable,Dimension(:)::EACOUNT,TEMPFSJ
      Integer,Allocatable,Dimension(:,:)::FSJ
      Real,Allocatable,Dimension(:)::FSREGX,PFLOW,TEMPREG,XPRET,XPSFD
      Real,Allocatable,Dimension(:,:)::CPSUM,EV,FSREG,RETSUM,FAF
      Real,Allocatable,Dimension(:)::AMTIF,ANNDIV,ANNREG,ANNRES,ANNWSD,
     +              CPRET,DDEPX,RESW,SFD,SFLOW,SHTBACK,SRETURN,WSD,WST
      Real,Allocatable,Dimension(:,:,:)::DDEP
      Real,Allocatable,Dimension(:,:,:,:)::CRDDEP
!
      Character(len=1)  TEMPCHAR
      Character(len=6)  BESID
      Character(len=8)  TIME1,TIME2
      Character(len=9)  TIME3
      Character(len=16) A1
      Character(len=80) TITLE0
!
      STATUS=0
      Call TIME(TIME1)
      NIFLAG2=0
      OUTFILE=0
!_______________________________________________________________________
!
!  Subroutine FILES1 is called to open the DAT and MSS files.
!
      Call FILES1
!
!  Header is written to MSS file.
!
      Write(14,*) ' WRAP-SIM MESSAGE FILE'
      Write(14,*) ' '
!
!  Subroutine READDAT is called to read and organize the DAT file data.
!
      P1=Index(ROOT,'   ')-1
      NAME=ROOT(:P1)//'.dat'
      Print*,' '
      Write(*,100) NAME
100   Format('    Reading the input data from file ',A120)
      Write(14,110) NAME
110   Format(' *** Starting to read file ',A120)
      Call READDAT
      If(ICHECK.GE.0) Write(14,120) NAME
120   Format(' *** Finished reading file ',A120)
!
!  Subroutine FILES2 is called to open the remaining files.
!
      Call FILES2
!
!  Dimension limits are set for arrays previously identified for allocation
!  after determination of the dimension limits in subroutine READDAT.
!
      Allocate(AMTIF(MAXWR),ANNDIV(MAXWR),ANNREG(MAXWR),ANNRES(MAXWR),
     +              ANNWSD(MAXWR),CPFLOW(MAXCP,MAXMON,2),CPRET(MAXCP),
     +      CPSUM(MAXCP,4),DEPSUM(MAXCP),DMRSUM(MAXCP),EACOUNT(MAXEA),
     +          EVAPR(MAXCP,MAXMON),FSREGX(MAXFS),FSREG(MAXFS,MAXFSP),
     +              IFTARGET(MAXCP),INFLOW(MAXCP,MAXMON),PFLOW(MAXTO),
     +  REGFLOW(MAXCP),RESREL(MAXCP),RESW(MAXWR),RETSUM(MAXCP,MAXMON),
     +          SFD(MAXWR),SFLOW(MAXCP),SHTBACK(MAXWR),SRETURN(MAXCP),
     +TEMPREG(MAXFSP),WSD(MAXWR),WST(MAXWR),XPRET(MAXPX),XPSFD(MAXPX))
!
      Allocate(EVLYR(MAXCP),FSJ(MAXFS,MAXFSP),ICP(MAXGAG),INLYR(MAXCP),
     +         SINDEX(MAXCP),TEMPFSJ(MAXFSP))
!
      If(NREOUT.GT.0) Allocate(EV(NRES,12))
      If(FAD.GE.2) Allocate(FAF(MAXCP,MAXMON))
!
!  Arrays for writing simulation results to DSS and SOU files.
!
      If(DSS(2).GT.0.or.DSS(3).GT.0) Then
         K=NYRS*12
         If(NCPO.GT.0) Then
            If(DSS(4).EQ.0) Allocate(DSSCP(NCPO,K,6))
            If(DSS(4).EQ.1) Allocate(DSSCP(NCPO,K,12))
            Allocate(DSSCPI(NCPO))
            DSSCP=0.0
         Endif
         If(NWROUT.GT.0) Then
            If(DSS(4).EQ.0) Allocate(DSSWR(NWROUT,K,4))
            If(DSS(4).EQ.1) Allocate(DSSWR(NWROUT,K,11))
            Allocate(DSSWRI(NWROUT))
            DSSWR=0.0
         Endif
         If(NREOUT.GT.0) Then
            If(DSS(4).EQ.0) Allocate(DSSRE(NREOUT,K,4))
            If(DSS(4).EQ.1) Allocate(DSSRE(NREOUT,K,12))
            Allocate(DSSREI(NREOUT))
            DSSRE=0.0
         Endif
      Endif
!
!  Dual simulation streanflow depletion array is created if
!  at least one water right has a DUAL(wr) of 3 or 4.
!
      If(NDD.GT.0) Then
         If(CR1.GT.0.and.CR2.EQ.0) Then
            Allocate(CRDDEP(MAXDU,NYRS,NPRDS,CR1))
            CRDDEP=0.0
         Else
            Allocate(DDEP(MAXDU,NYRS,NPRDS))
            DDEP=0.0
         Endif
         Allocate(DDEPX(MAXDU))
         DDEPX=0.0
      Endif
!
!  Subroutine IACNP is called to determine watershed parameters.
!
      If(F3.GE.1) Then
         NAME=ROOTH(:P2)//'.dis'
         Write(*,130) NAME
130      Format('    Reading flow distribution data from input file ',
     +           A120)
         If(ICHECK.GE.0) Write(14,140) NAME
140      Format(' *** Starting to read FD/WP records from file ',A120)
         Call IACNP
         If(ICHECK.GE.0) Write(14,150) NP
150      Format(' *** Determined watershed parameters for',I5,
     +          ' control points.')
      Endif
!
!  IN and EV records are read from text files and written to a DSS file
!  if DSS(5) on the OF record is activated.  SIM is then terminated.
!
      If(DSS(5).EQ.1) Then
*         Call DSSINPUT
         Goto 3090
      Endif
!
!  Subroutine DSSINPUT is called to read flows and evap depths from a
!  DSS file before the simulation loops if INEV on the JO record is 6.
!
      If(INEV.EQ.6) Then
*         Call DSSINPUT
      Endif
!
!  Subroutine INEV1 is called to read the IN and EV records before
!  the simulation loops if INEV on the JO record is 2 or 4.
!
      If(INEV.EQ.2.or.INEV.EQ.4) Then
         Call INEV1
      Endif
!
!  Subroutine FADFILE is called to read the FA records in the FAD file
!  if FAD on the JO record is 2.
!
      If(FAD.GE.2) Then
         Call FADFILE
      Endif
!
!  RU record adjustments to convert unappropriated flows to regulated flows
!  are read from RUF file if activated by JO record parameter RUF.
!
      If(RUFIN.GE.1.or.RUF.GE.1) Then
         Call READRUF
      Endif
!
!  Subroutine RANKWR or NATURAL is called to rank water rights in priority order.
!
      If (NPOPT.NE.1) Then
         Print*,'   Sorting water rights in priority order'
         Call RANKWR
         If(ICHECK.GE.0) Write(14,*)'*** Finished ranking ',
     +                              'water rights in priority order.'
      Else
         Print*,'  Sorting water rights in upstream to downstream order'
         Call NATURAL
         If(ICHECK.GE.0) Write(14,*)'*** Finished sorting ',
     +                   'water rights in upstream to downstream order.'
      Endif
!
!  Header and basic information are written to the OUT or CRM file.
!
      If(DSS(1).GT.0) Then
         If(CR1.GT.0) Then
            TITLE0='WRAP-SIM (January 2011 Version) CRM Output File'
         Else
            TITLE0='WRAP-SIM (January 2011 Version) OUT Output File'
         Endif
         If(OUTFILE.GE.2) Then
            Write(4,REC=1) TITLE0
            Write(4,REC=2) TITLE1
            Write(4,REC=3) TITLE2
            Write(4,REC=4) TITLE3
            Write(4,REC=5) YRST,NYRS,NCPO,NWROUT,NREOUT,CR1,CR2,CR3,CR4
         Else
            Write(4,220,REC=1) TITLE0
            Write(4,230,REC=2) TITLE1
            Write(4,230,REC=3) TITLE2
            Write(4,230,REC=4) TITLE3
            Write(4,240,REC=5) YRST,NYRS,NCPO,NWROUT,NREOUT,CR1,CR2,CR3,
     +                         CR4
         Endif
         RECD=6
220      Format(A54)
230      Format(A80)
240      Format(8I6,F8.3)
      Endif
!
!  Header and basic information are written to the HRR file.
!
      If(F7.EQ.1) Then
         TITLE0=' HYDROPOWER AND RESERVOIR SYSTEM RELEASE (HRR) FILE'
         Write(13,220) TITLE0
         Write(13,230) TITLE1
         Write(13,230) TITLE2
         Write(13,230) TITLE3
         N=0
         Do I=1,NWRTS
            WR=RANK(I)
            If(WRNUM(WR,6).EQ.1.and.WRNUM(WR,9).LT.0) N=N+1
         End Do
         If(N.GT.0) Then
            Write(13,250) YRST,NYRS,NPRDS,N
250         Format(4I6)
         Else
            Write(14,260)
260         Format(' WARNING: An HRR file has been created but no',
     +             ' system water rights are specified.')
         Endif
         Write(13,*)
      Endif
!
!  Title records and counts are written to the MSS file.
!
      Write(14,*) ' '
      Write(14,290)
      Write(14,*) ' System components counted from input file:'
      Write(14,300) NCPTS
      Write(14,310) NINCP
      Write(14,320) NEVCP
      Write(14,330) NRES
      Write(14,340) NIF
      N=NWRTS-NIF-NUMXP
      Write(14,350) N
      N=MAXSWR-1
      If(N.GE.1) Write(14,360) N
      If(NUMPOW.GT.0) Write(14,370) NUMPOW
      N=NUSES-2
      If(N.GT.0) Write(14,380) N
      If(NTABLE.GT.0) Write(14,390) NTABLE
      If(NPTABL.GT.0) Write(14,400) NPTABL
      If(DIFLAG.GT.0) Write(14,410) DIFLAG
      If(FSFLAG.GT.0) Write(14,420) FSFLAG
      If(NUMXP.GT.0) Write(14,430) NUMXP
      If(NDT.GT.0) Write(14,440) NDT
      If(FDFLAG.GT.0) Write(14,450) FDFLAG
      If(MAXGAG.GT.1) Write(14,460) MAXGAG
      Write(14,290)
290   Format(' ***************************************************',
     +       '****')
300   Format(1X,I7,' control points (CP records)')
310   Format(1X,I7,' primary control points (INMETHOD=1)')
320   Format(1X,I7,' control points with evap input (CPEV=blank)')
330   Format(1X,I7,' reservoirs')
340   Format(1X,I7,' instream flow rights (IF records)')
350   Format(1X,I7,' all water rights except IF rights (WR records)')
360   Format(1X,I7,' system water rights')
370   Format(1X,I7,' hydropower rights')
380   Format(1X,I7,' sets of water use coefficients (UC records)')
390   Format(1X,I7,' storage-area tables (SV/SA records)')
400   Format(1X,I7,' storage-elevation tables (PV/PE records)')
410   Format(1X,I7,' drought indices (DI records)')
420   Format(1X,I7,' flow switches (FS records)')
430   Format(1X,I7,' rights with priority circumvention PX record')
440   Format(1X,I7,' dual simulation rights')
450   Format(1X,I7,' FD records in the DIS file')
460   Format(1X,I7,' maximum upstream gaged cpts on FD records')
      If(CR1.GT.0) Then
         Write(14,*)
         Write(14,*)' Conditional reliability modeling (CR record):'
         Write(14,470) CR1
470      Format(5x,'The simulation period CR1 is',I3,' months.')
         If(CR2.GT.0) Then
            Write(14,480) CR2
480         Format(5x,'Annual simulations begin in month',I3,
     +                ' each year.')
         Else
            I=12*NYRS-CR1+1
            Write(14,490) I
490         Format(I8,' simulations are performed with the monthly',
     +                ' cycle option.')
         Endif
         Write(14,330)
      Endif
      Write(14,*) ' '
!_______________________________________________________________________
!
!  Initial values are assigned to variables before the firm yield loop.
!
      BSFLAG=0
      FYAMT=0.0
      IFY=0
!
!  Firm yield (FY record) iterations begin at statement 500 and end near
!  end of main program. Returns to 500 follow statements 2240 and 2550.
!
500   BESFLAG=0
      DUALFLAG=0
      If(BES.EQ.4.or.BES.EQ.5) BESFLAG=-9
!
!  The beginning-ending storage (BES) and dual simulation cycle begins
!  at statement 510 and ends after the end of the annual loop.
!
510   AMTRF=0.0
      IBACKUP=0
      NSHT=0
      TOTSHT=0.0
      SCOUNT=0
      AVFLAG=0
      IFCM=0
      CRNWR=0
!
!  Conditional reliability modeling variables are initialized.
!
      CRBEGIN=1
      CRI=0
      CRLOOP=1
      CRWRITE=0
      CRWRITEN=0
      CRSKIP=-9
      CRSIM=0
      CRM=0
      NLOOP=0
      If(CR1.GT.0) Then
         CRMN=Int((12*NYRS)/CR1)*CR1
         If(CR2.GT.0) Then
            NSEQ=NYRS-Int((CR1+CR2-2)/12)
         Else
            NSEQ=12*NYRS-CR1+1
         Endif
      Endif
!
!  The arrays dimensioned for NCPTS and NWRTS are initialized.
!
520   CPSUM=0.0
      DEPSUM=0.0
      DMRSUM=0.0
      CPFLOW=0.0
      EVAPR=0.0
      INFLOW=0.0
      RETSUM=0.0
      Do I=1,NCPTS
         If((BES.EQ.4.or.BES.EQ.5).and.BESFLAG.EQ.0) Then
            CPRET(I)=CPRET(I)
         Else
            CPRET(I)=0.0
         Endif
      End Do
      SFD=0.0
      RESW=0.0
      WSD=0.0
      WST=0.0
      ANNDIV=0.0
      ANNRES=0.0
      ANNWSD=0.0
      ANNREG=0.0
      SHTBACK=0.0
      AMTIF=0.0
!
!  FS record flow arrays are initialized.
!
      FSREGX=0.0
      FSREG=0.0
      TEMPREG=0.0
!
!  Beginning-of-simulation reservoir storage contents are set.
!
      If(BSFLAG.EQ.0) Then
         Do I=1,NRES
            If(RESDAT(I,5).GT.RESDAT(I,1)) Then
               RESDAT(I,5)=RESDAT(I,1)
               Write(14,600) RESDAT(I,5),RESID(I),RESDAT(I,1)
600            Format(' WARNING: The beginning-of-simulation storage',
     +           F9.1,' for reservoir ',A6,' exceeds its capacity.',/,
     +            10X,'The initial storage is set at the capacity',F9.1)
            Elseif(RESDAT(I,5).LT.0.0) Then
               RESDAT(I,5)=RESDAT(I,1)
            Endif
            RESDAT(I,14)=RESDAT(I,5)
            BSFLAG=99
         End Do
      Endif
      If(FYLEVEL.GT.0.or.DUALFLAG.GT.0) Then
         Do I=1,NRES
            RESDAT(I,5)=RESDAT(I,14)
         End Do
      Endif
      If(BES.EQ.5.and.BESFLAG.EQ.0) Then
         Do I=1,NRES
            RESDAT(I,5)=RESDAT(I,6)
         End Do
      Endif
!
!  Beginning reservoir storages are read from BES file if activated
!  by BES from JO record.
!
      If(BES.EQ.2.or.BES.EQ.3.or.(BES.EQ.4.and.BESFLAG.EQ.0)) Then
         Do I=1,5
            Read(15,*)
         End Do
610      Read(15,620) IBES,BESID,XBES
620      Format(I4,2x,A6,24x,F12.2)
         If(IBES.EQ.0.and.BESID.EQ.'      ') Goto 650
         If(XBES.GE.0.0) RESDAT(IBES,5)=XBES
         If(BESID.NE.RESID(IBES)) Then
            Write(14,*)' '
            Write(14,630) BESID,IBES,RESID(IBES)
630         Format(' ERROR: The ID of ',A6,' for reservoir',I4,
     +             ' from BES file should be ',A6)
            Write(14,*)' '
            Call ERROR
         Endif
         If(RESDAT(IBES,5).GT.RESDAT(IBES,1)) Then
            Write(14,640) RESDAT(IBES,5),RESID(IBES),RESDAT(IBES,1)
640         Format(' WARNING: The BES beginning storage of',F9.1,' for',
     +             ' reservoir ',A6,' exceeds its capacity of',F9.1)
         Endif
         RESDAT(IBES,14)=RESDAT(IBES,5)
         Goto 610
      Endif
!
!  End-of-period storages are set equal to beginning period storage.
!  Reservoir release decisions can be made with intermediate
!  end-of-period storages.
!
650   Do I=1,NRES
         RESDAT(I,6)=RESDAT(I,5)
      End Do
!
!  Subroutine DROUGHT is called to determine drought index factors.
!
      If(DIFLAG.GE.1) Then
         MT=12
         Call DROUGHT
         If(ICHECK.GE.0) Write(14,*)'*** Finished determining initial',
     +       ' drought index multiplier factors.'
      Endif
!____________________________________________________________________________
!
!  BEGINNING OF SYSTEM SIMULATION COMPUTATIONAL LOOPS  **********
! 
!  +++++++++++++++++  BEGINNING OF ANNUAL LOOP  +++++++++++++++++
!
      If(ICHECK.GE.0) Write(14,*)'*** Beginning annual loop.'
      Do 1700 I=1,NYRS
!
         YEAR=YRST+I-1
!
!  If INEV on the JO record is 2, 4, or 6, subroutines INEV1 or DSSINPUT
!  have already stored flow and evaporation rates as FLOW and EVAP arrays.
!  Subroutine INEV2 is called to determine the INFLOW and EVAPR arrays.
!
         If(INEV.EQ.2.or.INEV.EQ.4.or.INEV.EQ.6) Then
            Call INEV2
!
!  Otherwise, the inflows and evaporation rates (IN and EV records) are read
!  and INFLOW and EVAPR arrays set for the year by calling subroutine INEVYR.
!
         Else
            Call INEVYR
            If(ICHECK.GE.0.and.I.EQ.1) Then
               Write(14,780) NIN,NEV,YEAR
780            Format(' ***',I4,' IN and',I4,' EV records were read',
     +                ' for the first year (',I4,').')
            Endif
         Endif
!
!  Inflows are assigned for control points with FA records.
!
         FAF=0.0
         If(FAD.GE.2) Then
           If(NFACP.GT.0) Then
               Do J=1,NFACP
                  K=IDCPFA(J)
                  Do L=1,12
                     FAF(K,L)=FAFLOW(J,I,L)
                  Enddo
               Enddo
            Endif
         Endif
!
!  Inflow adjustments specified by RUFIN option 1 on JO record.
!
         If(RUFIN.EQ.1) Then
            Do J=1,NCPTS
               Do M=1,12
                  INFLOW(J,M)=INFLOW(J,M)+RUFA(J,I,M)
               End Do
            End Do
         Endif
!
!  Evap-precip depths from EV records are saved for reservoir output records.
!
         If(NREOUT.GT.0) Then
            Do K=1,NRES
               Do L=1,NPRDS
                  EV(K,L)=EVAPR(RESNUM(K,1),L)
               End Do
            End Do
         Endif
!
!  *+*+*+*+*   Call Subroutine FLDIST   *+*+*+*+*
!  Flows at some control points may be determined from the inflows at other
!  control points within FLDIST. Subroutine FLDIST also adjusts the net
!  evaporation-precipitation rates for the portion of the naturalized flows
!  representing runoff from land now covered by reservoirs.
!
         If(F3.GE.1) Then
            Call FLDIST
         Endif
!
!  *+*+*+*+*  Call Subroutine FLOWADJ  *+*+*+*+*
!  Adjustments to naturalized flows are read from FAD file.
!
         If(FAD.EQ.1) Then
            Call FLOWADJ
            If(ICHECK.GE.0.and.I.EQ.1) Write(14,*) '*** Flow ',
     +    'adjustments from FA records were applied for the first year.'
         Endif
!
!  The initial unappropriated flows are set equal to the inflows. The loop
!  also includes another net evaporation-precipitation adjustment option.
!
         Do K=1,NCPTS
            Do L=1,NPRDS
               CPFLOW(K,L,2)=INFLOW(K,L)
               If(EWA(K).GT.0.0) Then
                  EVAPR(K,L)=EVAPR(K,L)+INFLOW(K,L)/EWA(K)
               Endif
            End Do
         End Do
!
!  *+*+*+*+*  Call Subroutine INCREM  *+*+*+*+*
!  Negative incremental inflow adjustment subroutine is called.
!
         If((ADJINC.GE.2.and.ADJINC.LE.4).or.NEGINC.GE.2) Then
            Call INCREM
            If(ICHECK.GE.0.and.I.EQ.1) Write(14,*) '*** Negative ',
     +          'incremental flow adjustments were performed for ',
     +          'the first year.'
         Endif
!
!  End of input data trace.
!
         If(ICHECK.GE.0.and.I.EQ.1) Then
            Write(14,*)'*** End of input data trace.'
            Write(14,*) ' '
         Endif
!      
!  Negative incremental flows are written to message file.
!
         If(NEGINC.GE.2.and.ADJINC.LT.5) Then
            If(YEAR.EQ.YRST) Then
               Write(14,*) ' '
               Write(14,790) NEGINC
790            Format(' Negative Incremental Inflows for JD Record ',
     +                'NEGINC of',I2)
               Write(14,*) ' '
               YRSUM=0.0
            Endif
            Do K=1,NCPTS
               SUMX=0.0
               Do J=1,12
                  SUMX=SUMX+CPFLOW(K,J,1)
                  YRSUM=YRSUM+SUMX
               End Do
               If(Abs(SUMX).GT.0.0001) Then
                  Write(14,800) CPID(K,1),YEAR,(CPFLOW(K,J,1),J=1,12)
800               Format(A6,I6,12F8.1)
               Endif
            End Do
            If(I.EQ.NYRS) Then
               If(Abs(YRSUM).LT.0.001) Write(14,810)
810            Format('  All incremental flows are positive.',
     +                '  There are no negative incrementals.')
               Write(14,*) ' '
            Endif
         Endif
!
!  *+*+*+*+*  Call Subroutine ADJUST  *+*+*+*+*
!  Initial available flows in first month of year are adjusted
!  by the return flows from the last month of previous year.
!
         DEP=0.0
         MT=1
         RFAC=1.0
         Do RETNUM=1,NCPTS
            If(CPRET(RETNUM).GT.0.) Then
               RET=CPRET(RETNUM)
               RETSUM(RETNUM,MT)=RET
               CPRET(RETNUM)=0.
               Call ADJUST
            Endif
         End Do
!
!  Current year message is written to the monitor.
!
         Write(*,820) YEAR
820      Format('    Performing simulation for year',I5)
!____________________________________________________________________________
!
!  ++++++++++++  BEGINNING OF MONTHLY (PERIOD) COMPUTATION LOOP  ++++++++++++
!
         Do 1650 MT=1,NPRDS
            CLOSS=0.0
!
!  Conditional reliability simulations are activated by a CR record,
!
            If(CR1.GT.0) Then
               CRM=CRM+1
               CRWRITE=0
!
!              Annual simulation cycle activated by non-zero CR2 on CR record.
!
               If(CR2.GT.0) Then
                  CRMON=CRM-12*NYRS*(CRLOOP-1)
                  If(CRMON.EQ.1) Then
                     SQEND=0
                     SQSTART1=0
                     SQSTART=CR2+12*(CRLOOP-1)
                  Endif
                  If(CRMON.GT.SQEND.and.CRMON.LT.SQSTART)
     +               CRWRITE=-99
                  If(CRMON.EQ.SQSTART) Then
                     SQSTART1=SQSTART
                     SQEND=SQSTART+CR1-1
                     If(CR1.LE.12)Then
                        SQSTART=SQSTART+12
                     Else
                        SQSTART=SQSTART+12*Ceiling((CR1+CR2-2)/12.)
                     Endif
                  Endif
                  If(CR3.LE.1.and.CRMON.GE.SQSTART1.and.
     +               CRMON.LT.SQEND-11) CRWRITE=-99
                  If(SQEND.GT.12*NYRS) CRWRITE=-99
                  If(CR5.NE.0) Goto 1650
                  If(CRMON.EQ.SQSTART1.and.SQEND.LE.12*NYRS) Then
                     Do K=1,NRES
                        RESDAT(K,5)=RESDAT(K,14)*CR4
                        RESDAT(K,6)=RESDAT(K,5)
                     End Do
                     CRSIM=CRSIM+1
                  Endif
!
!              Monthly simulation cycle activated by zero CR2 on CR record.
!
               Else
                  If((MT.LE.CRSKIP-Int((CRLOOP-1)/12)*12).or.
     +               (YEAR.LE.YRST+Int((CRLOOP-1)/12)-1).or.
     +               (CRM.GT.CRMN)) CRWRITE=-99
                  If((MT.EQ.CRSKIP-Int((CRLOOP-1)/12)*12).and.
     +              (Int((CRLOOP-1)/12)+YRST.EQ.YEAR)) Then
                     CRSKIP=-9
                     CRBEGIN1=CRBEGIN
                     CRBEGIN=CRM+1
                  Endif
                  If(CRM.EQ.CRBEGIN.and.CRM.LE.CRMN) Then
                     Do K=1,NRES
                        RESDAT(K,5)=RESDAT(K,14)*CR4
                        RESDAT(K,6)=RESDAT(K,5)
                     End Do
                     CRBEGIN1=CRBEGIN
                     CRBEGIN=CRBEGIN+CR1
                     CRSIM=CRSIM+1

                  Endif
                  If(CR1.GT.12.and.CR3.LE.1.and.
     +               (CRM.LT.CRBEGIN1+CR1-12)) CRWRITE=-99
               Endif
!
!              Conditional reliability trace message written to MSS file.
!
               If(ICHECK.EQ.10) Then
                  If(CRWRITE.LT.0) CRWRITEN=CRWRITEN+1
                  CRI=CRI+1
                  If(CRWRITE.LT.0) CRI=0
                  Write(14,830) CRM,CRI,CRSIM,CRLOOP,YEAR,MT,CRWRITE
830               Format('Mon:',I5,'   CR Mon:',I4,'   Sim:',I4,
     +                   '   Cycle:',I2,'   Yr:',I5,'   Mon:',I3,
     +                   '   CRWRITE:',I3)
               Endif
            Endif
!
!  For reservoir with monthly varying capacity limits set by OS records,
!  end-of-month storage capacity limit is stored as RESDAT(K,15).
!
            If(OSFLAG.GT.0.or.MSFLAG.GT.0) Then
               Do K=1,NRES
                  RESDAT(K,15)=RESDAT(K,1)
               End Do
            Endif
            If(OSFLAG.GT.0) Then
               IR=0
835            IR=IR+1
               K=OSRES(IR)
               L=12*(I-1)+MT
               If(RESDAT(K,15).GT.OS(IR,L)) RESDAT(K,15)=OS(IR,L)
               If(IR.LT.OSFLAG) Goto 835
            Endif
!
!  For reservoir with monthly varying capacity limits set by a MS record,
!  spills are computed and storage is constrained to capacity.  Flows at
!  downstream control points are increased by spills adjusted for losses.
!
            If(MSFLAG.GT.0) Then
               IR=0
840            IR=IR+1
               K=MSRES(IR)
               If(RESDAT(K,15).GT.STMON(IR,MT))RESDAT(K,15)=STMON(IR,MT)
               If(RESDAT(K,5).GT.STMON(IR,MT)) Then
                  SPILL=RESDAT(K,5)-STMON(IR,MT)
                  RESDAT(K,5)=STMON(IR,MT)
                  If(SPILL.GT.STMON(IR,13)) Then
                     RESDAT(K,5)=STMON(IR,MT)+SPILL-STMON(IR,13)
                     SPILL=STMON(IR,13)
                  Endif
                  DF=SPILL
                  NCP=RESNUM(K,1)
850               CPFLOW(NCP,MT,2)=CPFLOW(NCP,MT,2)+DF
                  LOSS=CL(NCP)*DF
                  CLOSS(NCP,2)=CLOSS(NCP,2)+LOSS
                  DF=(1.0-CL(NCP))*DF
                  NCP=CPNXT(NCP)
                  If(NCP.GT.0) Goto 850
               Endif
               If(IR.LT.MSFLAG) Goto 840
            Endif
!
!  Starting available flows and return flows are stored for second pass
!  through water rights loop for instream flow IFMETH options 2 and 4.
!
            Do K=1,NCPTS
               SFLOW(K)=CPFLOW(K,MT,2)
               SRETURN(K)=RETSUM(K,MT)
               REGFLOW(K)=0.0
            End Do
            IFPASS=-1
!
!  Second pass through the water rights loop for instream flow requirement
!  IFMETH options 2 and 4 begins here.
!
860         If(Abs(IFPASS).EQ.2) Then
               Do 870 K=1,NCPTS
                  CPFLOW(K,MT,2)=SFLOW(K)
                  If(MT.LT.NPRDS) Then
                     If(ADJINC.EQ.4) Then
                        CPFLOW(K,MT+1,2)=INFLOW(K,MT+1)+CPFLOW(K,MT+1,1)
                     Else
                        CPFLOW(K,MT+1,2)=INFLOW(K,MT+1)
                     Endif
                  Endif
                  RETSUM(K,MT)=SRETURN(K)
                  If(MT.LT.NPRDS) RETSUM(K,MT+1)=0.0
870            End Do
               CLOSS=0.0
            Endif
!
!  Variables are initialized.
!
            Do 880 K=1,NCPTS
               IFTARGET(K)=0.0
               RESREL(K)=0.0
               CPRET(K)=0.0
880         End Do
            DSSWRJ=0
            NWRREC=0
            JFY=0
            FYAA=0.0
            FYNCOUNT=0
            SUMAMT=0.0
            SUMWSD=0.0
!
!  Adjust the inflows from the IN records plus previous month return flows
!  by adding or subtracting constant inflows from the CI records and varying
!  inflows from the FA records for FAD options 1 and 2.
!
            If(MAXCI.GT.0.or.NFACP.GT.0) Then
               Do 900 K=1,NCPTS
                  X=0.0
                  If(MAXCI.GT.0) X=CINF(K,MT)
                  If(NFACP.GT.0) X=FAF(K,MT)
                  If(Abs(X).GE.0.0001) Then
                     CIN=X
                     If(CIN.GE.0.0) Then
                        CPFLOW(K,MT,2)=CPFLOW(K,MT,2)+CIN
                     Elseif(Abs(CIN).LE.CPFLOW(K,MT,2)) Then
                        CPFLOW(K,MT,2)=CPFLOW(K,MT,2)+CIN
                     Else
                        CIN=-CPFLOW(K,MT,2)
                        CPFLOW(K,MT,2)=0.0
                     Endif
                     NUSCP=K
                     NPT=CPNXT(K)
890                  If(CL(NUSCP).GT.0.00001) Then
                        If(CIN.GT.0.0001) Then
                           CLOSS(NUSCP,2)=CLOSS(NUSCP,2)+CIN*CL(NUSCP)
                        Else
                           CLOSS(NUSCP,1)=CLOSS(NUSCP,1)+
     +                                              ABS(CIN)*CL(NUSCP)
                        Endif
                        CIN=CIN*(1.0-CL(NUSCP))
                     Endif
                     If(NPT.LT.0) Goto 900
                     CPFLOW(NPT,MT,2)=CPFLOW(NPT,MT,2)+CIN
                     If(CPFLOW(NPT,MT,2).LT.0.0) Then
                        CIN=-CPFLOW(NPT,MT,2)
                        CPFLOW(NPT,MT,2)=0.0
                     Endif
                     NUSCP=NPT
                     NPT=CPNXT(NPT)
                     Goto 890
                  Endif
900            End Do
            Endif
!
!  Initial cumulative net evap-precip and beginning-of-period and
!  end-of-period storage are set at zero for each EA record.
!
            If(EAFLAG.GT.0) Then
               Do 910 K=1,EAFLAG
                  CUMEV(K)=0.0
                  CUMBEG(K)=0.0
                  CUMEND(K)=0.0
                  CUMCAP(K)=0.0
910            End Do
               EACOUNT=0
            Endif
!
!  Initial flows are written to ZZZ file if activated by ZZ record.
!
            If((SIM2.EQ.9.and.DUALFLAG.EQ.0).or.SIM3.GE.9) Then
               Continue
            Else
               ZZR=0
               If(ZZ.GE.1) Call ZZFLOW
            Endif
!____________________________________________________________________________
!
!  ++++++++++++++  BEGINNING OF WATER RIGHTS COMPUTATION LOOP  ++++++++++++++
!
            Do 1300 K=1,NWRTS
               WR=RANK(K)
!
!  Dual simulation rights are activated during either the first
!  or second simulation as specified by DUAL(wr) on SO/PX record.
!
               If(NDT.GT.0) Then
                  If(DUALFLAG.EQ.0.and.DUAL(WR).EQ.2) Goto 1300
                  If(DUALFLAG.EQ.0.and.DUAL(WR).EQ.5) Goto 1300
                  If(DUALFLAG.GT.0.and.DUAL(WR).EQ.1) Goto 1300
                  If(DUALFLAG.GT.0.and.DUAL(WR).EQ.4) Goto 1300
               Endif
!
!  Variables are initialized.
!
               AMT=WRDAT(WR,1)
               AVAMT=0.0
               BPSTOR=0.0
               DEP=0.0
               EPSTOR=0.0
               EVOL=0.0
               FRMPOW=0.0
               FSVX=0.0
               FSJJ=0
               MAKEUP=0.0
               OUTPT=WRNUM(WR,6)
               POWPRO=0.0
               RELS=0.0
               RETNUM=WRNUM(WR,3)
               RFAC=WRDAT(WR,2)
               SHT=0.0
               WRT=WRNUM(WR,5)
!
!  WRNUM(WR,10) is an alternate control point from SO record field 10 for
!  a WR record right or the IF record field 7 switch for an IF record right.
!
               LOCNUM=WRNUM(WR,1)
               If(WRNUM(WR,10).GT.0) Then
                  If(WRIDS(WR,1).NE.'IF#IF*IF') LOCNUM=WRNUM(WR,10)
               Endif
!
!  System water right data are assigned, and the number of reservoirs
!  associated with each system or non-system water right is set.
!
               SWR=0
               If(WRNUM(WR,9).LT.0) Then
                  SWR=-WRNUM(WR,9)
                  RE=NSR(SWR)
                  RI=SN1(SWR,1)
                  Do J=1,RE
                     Do L=1,10
                        WRSYS(J,L)=SYSTEM(SWR,J,L)
                     End Do
                  End Do
               Elseif(WRNUM(WR,9).GT.0) Then
                  RE=1
                  RI=WRNUM(WR,9)
                  If(WRT.LE.-1.or.WRT.EQ.2.or.WRT.EQ.3) SWR=RI
                  WRSYS(1,1)=WRDAT(WR,3)
                  WRSYS(1,2)=WRDAT(WR,3)
                  WRSYS(1,3)=WRDAT(WR,4)
               Else
                  RE=0
               Endif
!
!  System release data are initialized.
!
               If(RE.GE.1) Then
                  Do Z=1,RE
                     SYSREL(Z)=0.0
                  End Do
               Endif
!
!  Beginning-of-month storage is saved for PX record transient rights.
!
               If(WRNUM(WR,8).GT.0.and.RI.GT.0) Then
                  If(XPR(WRNUM(WR,8)).LE.1) XPRESDAT=RESDAT(RI,5)
               Endif
!
!  For FY record firm yield computations, annual target is set.
!
               If(FYLEVEL.NE.0) Then
                  If(FYLEVEL.EQ.-99) FYAMT=FYIN(2)
                  J=0
920               J=J+1
                  If(WR.EQ.FYWR(J)) Then
                     If(MFY.EQ.2) Then
                        JFY=JFY+1
                        If((FYAMT-FYAA).LE.0) Then
                           AMT=0.0
                        Elseif((FYAMT-FYAA).GT.WRDAT(WR,1)) Then
                           If(JFY.EQ.FYN) AMT=FYAMT-FYAA
                           If(JFY.NE.FYN) AMT=WRDAT(WR,1)
                        Else
                           AMT=FYAMT-FYAA
                        Endif
                        FYAA=FYAA+AMT
                     Else
                        AMT=FYAMT*FYFACT(J)
                     Endif
                  Endif
                  If(J.LT.FYN.and.WR.NE.FYWR(J)) Goto 920
               Endif
!____________________________________________________________________________
!
!  The monthly target (AMT) is set starting with AMT from WR or IF record
!  field 3 and then adjusted based on parameters from other input records.
!
               If(WRNUM(WR,2).GT.0) Then 
                  AMT=AMT*PDUSCF(WRNUM(WR,2),MT)
               Endif
               ADJAMT=0.0
               AMT1=AMT
!
!     Shortages are added to the target of a back-up right.
!
               N=WRTO(WR)
               If(N.GE.1) Then
                  If(Abs(TOFLAG(N)).EQ.8.or.Abs(TOFLAG(N)).EQ.98) Then
                     If(BUWR(N).GT.0) Then
                        AMT=AMT+(SHTBACK(BUWR(N))*BUX(N))
                     Endif
                  Endif
                  If(BUG(N).NE.'        ') Then
                     Do J=1,NWRTS
                        If(WRIDS(J,1).EQ.BUG(N).or.
     +                     WRIDS(J,2).EQ.BUG(N)) Then
                           AMT=AMT+(SHTBACK(J)*BUX(N))
                        Endif
                     Enddo
                  Endif
               Endif
               AMT2=AMT
!
!     Application of drought index.
!
               If(DINDEX(WR).GE.1) Then
                  If(STODI.GT.0) Then
                     Call DROUGHT
                  Endif
                  AMT=AMT*DIFACT(DINDEX(WR))
               Endif
               AMT3=AMT
!
!     Application of target setting features of TO record.
!
930            If(N.GT.0) Then
!
                  If(TOTARGET(N).LT.-12.or.TOTARGET(N).GT.14) Then
                     Write(14,940) TOTARGET(N),Adjustl(WRID(WR))
940                  Format(' ERROR: TOTARGET of',I3,' from TO record',
     +                      ' is not valid. Water right: ',A16)
                     Call ERROR
                  Endif
!
!        Backup right switch is set.
!
                  If(Abs(TOFLAG(N)).EQ.9.or.Abs(TOFLAG(N)).EQ.99)
     +               IBACKUP=9
                  If(TOFLAG(N).EQ.7.or.TOFLAG(N).EQ.97) IBACKUP=7
!
!        TOFLOW from TO record sets optional control point location.
!
                  If(TOCP(N).GT.0) Then
                     L=TOCP(N)
                  Else
                     L=WRNUM(WR,1)
                  Endif
!
!        Naturalized streamflow based target

!
                  If(TOTARGET(N).EQ.1) ADJAMT=FACT(N)*INFLOW(L,MT)
                  If(TOTARGET(N).EQ.-1) Then
                     If(YEAR.EQ.YRST.and.MT.EQ.1) Then
                        ADJAMT=FACT(N)*INFLOW(L,MT)
                     Else
                        ADJAMT=FACT(N)*PFLOW(N)
                     Endif
                  Endif
!
!        Regulated streamflow based target
!
                  If(Abs(TOTARGET(N)).EQ.2) Then
                     ADJAMT=CPFLOW(L,MT,2)+RESREL(L)
                     If(ADJINC.EQ.4) Then
                        ADJAMT=ADJAMT-CPFLOW(L,MT,1)
                        If(ADJAMT.LT.0.0) ADJAMT=0.0
                     Endif
                     If(TOTARGET(N).EQ.2) ADJAMT=FACT(N)*ADJAMT
                     If(TOTARGET(N).EQ.-2) Then
                        If(YEAR.EQ.YRST.and.MT.EQ.1) Then
                           ADJAMT=FACT(N)*ADJAMT
                        Else
                           ADJAMT=FACT(N)*PFLOW(N)
                        Endif
                     Endif
                  Endif
!
!        Unappropriated streamflow based target
!
                  If(TOTARGET(N).EQ.3) Then
                     KK=LOCNUM
                     LOCNUM=L
                     Call AVALB
                     LOCNUM=KK
                     ADJAMT=FACT(N)*AVAMT
                  Endif
                  If(TOTARGET(N).EQ.-3) Then
                     If(YEAR.EQ.YRST.and.MT.EQ.1) Then
                        KK=LOCNUM
                        LOCNUM=L
                        Call AVALB
                        LOCNUM=KK
                        ADJAMT=FACT(N)*AVAMT
                     Else
                        ADJAMT=FACT(N)*PFLOW(N)
                     Endif
                  Endif
!
!        Reservoir storage based target
!
                  If(Abs(TOTARGET(N)).EQ.4) Then
                     IR=TORI(N)
                     If(TOTARGET(N).EQ.4) Then
                        ADJAMT=RESDAT(IR,6)*FACT(N)
                     Elseif(TOTARGET(N).EQ.-4) Then
                        ADJAMT=RESDAT(IR,5)*FACT(N)
                     Endif
                  Endif
!
!        Reservoir drawdown based target
!
                  If(Abs(TOTARGET(N)).EQ.5) Then
                     IR=TORI(N)
                     If(TOTARGET(N).EQ.5) Then
                        DRAWDOWN=RESDAT(IR,1)-RESDAT(IR,6)
                     Elseif(TOTARGET(N).EQ.-5) Then
                        DRAWDOWN=RESDAT(IR,1)-RESDAT(IR,5)
                     Endif
                     ADJAMT=DRAWDOWN*FACT(N)
                  Endif
!
!        Water right based target. Monthly or annual streamflow depletion,
!        storage withdrawal, diversion, or instream flow target or shortage.
!
                  If(Abs(TOTARGET(N)).GE.6.and.TOTARGET(N).NE.10)
     +               IWR=TOWI(N)
                  If(TOTARGET(N).EQ.6) Then
                     ADJAMT=SFD(IWR)*FACT(N)
                  Elseif(TOTARGET(N).EQ.7) Then
                     ADJAMT=(ANNDIV(IWR)+SFD(IWR))*FACT(N)
                  Elseif(TOTARGET(N).EQ.-7) Then
                     ADJAMT=ANNDIV(IWR)*FACT(N)
                  Elseif(TOTARGET(N).EQ.8) Then
                     ADJAMT=RESW(IWR)*FACT(N)
                  Elseif(TOTARGET(N).EQ.9) Then
                     ADJAMT=(ANNRES(IWR)+RESW(IWR))*FACT(N)
                  Elseif(TOTARGET(N).EQ.-9) Then
                     ADJAMT=ANNRES(IWR)*FACT(N)
                  Elseif(TOTARGET(N).EQ.11) Then
                     ADJAMT=WSD(IWR)*FACT(N)
                  Elseif(TOTARGET(N).EQ.12) Then
                     ADJAMT=(ANNWSD(IWR)+WSD(IWR))*FACT(N)
                  Elseif(TOTARGET(N).EQ.-12) Then
                     ADJAMT=ANNWSD(IWR)*FACT(N)
                  Elseif(TOTARGET(N).EQ.13) Then
                     If(WRIDS(IWR,1).NE.'IF#IF*IF') Then
                        ADJAMT=WST(IWR)*FACT(N)
                     Else
                        ADJAMT=AMTIF(IWR)*FACT(N)
                     Endif
                  Elseif(TOTARGET(N).EQ.14) Then
                     KK=WRNUM(IWR,1)
                     IFSHORT=AMTIF(IWR)-CPFLOW(KK,MT,2)-RESREL(KK)
                     If(ADJINC.EQ.4) IFSHORT=IFSHORT+CPFLOW(KK,MT,1)
                     If(IFSHORT.LT.0.0) IFSHORT=0.0
                     ADJAMT=IFSHORT*FACT(N)
                  Endif
!
!        Target applied or combined as specified by TOCOMB(N).
!
                  If(TOCOMB(N).EQ.'SET') Then
                     AMT=ADJAMT
                  Elseif(TOCOMB(N).EQ.'ADD') Then
                     AMT=AMT+ADJAMT
                  Elseif(TOCOMB(N).EQ.'SUB') Then
                     AMT=AMT-ADJAMT
                     If(AMT.LT.0.0) AMT=0.0
                  Elseif(TOCOMB(N).EQ.'MUL') Then
                     AMT=AMT*ADJAMT
                  Elseif(TOCOMB(N).EQ.'DIV') Then
                     If(ADJAMT.EQ.0.0) Then
                        Write(14,950) Adjustl(WRID(WR))
950                     Format(' WARNING: The TOCOMB=DIV option for ',
     +                         'right ',A16,/,10x,'involved dividing ',
     +                         'by zero and thus was skipped.')
                     Else
                        AMT=AMT/ADJAMT
                     Endif
                  Elseif(TOCOMB(N).EQ.'MAX') Then
                     If(AMT.LT.ADJAMT) AMT=ADJAMT
                  Elseif(TOCOMB(N).EQ.'MIN') Then
                     If(AMT.GT.ADJAMT) AMT=ADJAMT
                  Elseif(TOCOMB(N).EQ.'LIM') Then
                     If(N.EQ.1) Then
                        AMT=0.0
                     Elseif(Abs(TOFLAG(N-1)).LT.89) Then
                        AMT=0.0
                     Endif
                     If(ADJAMT-TOLIM(N,1).GT.-0.001.and.
     +                  TOLIM(N,2)-ADJAMT.GT.-0.001) AMT=AMT3
                  Endif
!
!  Upper and lower limits from TO record.
!
                  If(TOLIM(N,1).GE.0.001.or.TOLIM(N,1).LE.0.-0.1.or.
     +               TOLIM(N,2).LE.98000000.) Then
                     If(TOTARGET(N).NE.10.and.TOCOMB(N).NE.'LIM') Then
                        X1=TOLIM(N,1)
                        X2=TOLIM(N,2)
                        If(TOLIM(N,1).LE.-1.)X1=CPFLOW(L,MT,2)+DEPSUM(L)
                        If(TOLIM(N,2).LE.-1.)X2=CPFLOW(L,MT,2)+DEPSUM(L)
                        If(AMT.LT.X1) AMT=X1
                        If(AMT.GT.X2) AMT=X2
                     Endif
                  Endif
!
!  If a continuation TO record, above routine is repeated for next TO record.
!
                  If(Abs(TOFLAG(N)).GE.90) Then
                     N=N+1
                     Goto 930
                  Endif
!
!  End of TO record target setting routine that began with statement 930.
!
               Endif
               AMT4=AMT
!
!  Multiple-year monthly targets from TS records.
!
               If(SERIES(WR).GT.0) Then
                  ITS=SERIES(WR)
                  If(TSCOM(ITS).EQ.'MAX') Then
                     If(AMT.LT.QTS(I,ITS,MT)) AMT=QTS(I,ITS,MT)
                  Elseif(TSCOM(ITS).EQ.'MIN') Then
                     If(AMT.GT.QTS(I,ITS,MT)) AMT=QTS(I,ITS,MT)
                  Elseif(TSCOM(ITS).EQ.'ADD') Then
                     AMT=AMT+QTS(I,ITS,MT)
                  Elseif(TSCOM(ITS).EQ.'SUB') Then
                     AMT=AMT-QTS(I,ITS,MT)
                     If(AMT.LT.0.0) AMT=0.0
                  Elseif(TSCOM(ITS).EQ.'MUL') Then
                     AMT=AMT*QTS(I,ITS,MT)
                  Elseif(TSCOM(ITS).EQ.'   ') Then
                     AMT=QTS(I,ITS,MT)
                  Endif
               Endif
               AMT5=AMT
!
!  Drought index is applied.
!
               If(DINDEX(WR).LE.-1) Then
                  If(STODI.GT.0) Then
                     Call DROUGHT
                  Endif
                  JJ=Abs(DINDEX(WR))
                  AMT=AMT*DIFACT(JJ)
               Endif
               AMT6=AMT
!
!  Upper and lower limits from TO record.
!
               If(N.GT.0) Then
                  If(TOTARGET(N).EQ.10) Then
                     X1=TOLIM(N,1)
                     X2=TOLIM(N,2)
                     If(TOLIM(N,1).LE.-1.0) X1=CPFLOW(L,MT,2)+DEPSUM(L)
                     If(TOLIM(N,2).LE.-1.0) X2=CPFLOW(L,MT,2)+DEPSUM(L)
                     If(AMT.LT.X1) AMT=X1
                     If(AMT.GT.X2) AMT=X2
                  Endif
               Endif
               AMT7=AMT
!
!  For a type 4 right, the return flow is set.
!
               If(WRNUM(WR,5).EQ.4) Then
                  If(RFMETH(WR).EQ.3.or.RFMETH(WR).EQ.4) Then
                     KK=IRF(WR)
                     AMTRF=AMT*RF(KK,MT)
                     If(IBACKUP.EQ.7.and.BUWR(N).GT.0) Then
                        AMTRF=AMTRF-RF(KK,MT)*SHTBACK(BUWR(N))
                     Endif
                  Else
                     If(RFAC.LE.0.0001) RFAC=1.0
                     AMTRF=AMT*RFAC
                     If(IBACKUP.EQ.7) AMTRF=AMTRF-RFAC*SHTBACK(BUWR(N))
                  Endif
                  AMT=0.0
                  IBACKUP=0
               Endif
!
!  Flow switch FS record adjustment based on cumulative flow.
!
               If(FSFLAG.GT.0) Then
                  Do 966 J=1,FSM
                     If(FSN(WR,J).NE.0) Then
                        FS=FSN(WR,J)
                        X2=FSREGX(FS)
                        JJ=FSI(FS,11)
                        If(FSI(FS,8).LE.2) Then
!
!                    The amount X1 for current month may be added to X2.
!
                           If(FSI(FS,6).GT.0) Then
                              If(FSI(FS,7).GE.FSI(FS,6)) Then
                                 If(MT.LT.FSI(FS,6).or.MT.GT.FSI(FS,7))
     +                              Goto 960
                              Else
                                 If(MT.GT.FSI(FS,7).and.MT.LT.FSI(FS,6))
     +                              Goto 960
                              Endif
                           Endif
!
                           If(FSI(FS,1).EQ.1) Then
                           X1=CPFLOW(FSI(FS,10),MT,2)+RESREL(FSI(FS,10))
                           Elseif(FSI(FS,1).EQ.2) Then
                              X1=INFLOW(FSI(FS,10),MT)
                           Elseif(FSI(FS,1).EQ.3) Then
                              KK=LOCNUM
                              LOCNUM=FSI(FS,10)
                              Call AVALB
                              X1=AVAMT
                              LOCNUM=KK
                           Elseif(FSI(FS,1).EQ.4) Then
                              X1=DEPSUM(FSI(FS,10))
                           Elseif(FSI(FS,1).EQ.5) Then
                              X1=CPSUM(FSI(FS,10),2)-CPSUM(FSI(FS,10),1)
                           Elseif(FSI(FS,1).EQ.6) Then
                           X1=CPFLOW(FSI(FS,10),MT,2)+DEPSUM(FSI(FS,10))
                           Endif
!
                           If(FSI(FS,1).EQ.1.and.FSI(FS,8).LE.1.and.
     +                        Abs(IFPASS).EQ.2) Then
                              X2=FSREGX(FS)+REGFLOW(FSI(FS,10))
                              If(REGFLOW(FSI(FS,10))-FSX(FS,3).GT.
     +                         -0.0001.and.FSX(FS,4)-REGFLOW(FSI(FS,10))
     +                           .GT.-0.0001) JJ=FSI(FS,11)+1
                           Else
                              X2=FSREGX(FS)+X1
                              If(X1-FSX(FS,3).GT.-0.0001.and.
     +                          FSX(FS,4)-X1.GT.-0.0001) JJ=FSI(FS,11)+1
                           Endif
                        Endif
!
!                    FS record multiplier factors are applied if X2 or JJ
!                    falls within the bounds defined by the FS record.
!
960                     FSI(FS,12)=0
                        If(FSI(FS,2).EQ.1.or.FSI(FS,2).EQ.3) Then          Jun10
                           If(X2-FSX(FS,3).GT.-0.001.and.
     +                           FSX(FS,4)-X2.GT.-0.001) FSI(FS,12)=99
                        Endif
                        If(FSI(FS,2).EQ.2.or.FSI(FS,2).EQ.4) Then
                           If(JJ.GE.FSI(FS,3).and.JJ.LE.FSI(FS,4))
     +                                                   FSI(FS,12)=99
                        Endif
                        If(FSI(FS,2).EQ.1.or.FSI(FS,2).EQ.2) Then
                           If(FSI(FS,12).EQ.99) Then
                              AMT=AMT*FSX(FS,1)
                              If(WRNUM(WR,5).EQ.4) AMTRF=AMTRF*FSX(FS,1)
                           Else
                              AMT=AMT*FSX(FS,2)
                              If(WRNUM(WR,5).EQ.4) AMTRF=AMTRF*FSX(FS,2)
                           Endif
                        Endif
!
!                    FS results are written to message MSS file.
!
                        If(FSI(FS,9).GT.0) Then
                           Write(14,964) YEAR,MT,Adjustl(WRID(WR)),FS,
     +                                  FSX(FS,3),FSX(FS,4),X2,JJ,AMT
964                        Format(I4,I3,2x,A16,' FS',I3,'  Bounds:',
     +                          F9.1,F10.1,'  FSV Flow',F10.1,'  Count',
     +                          I3,'  Target',F10.1)
                        Endif
                     Endif
                     If(J.EQ.1) Then
                        FSVX=X2
                        FSJJ=JJ
                     Endif
966               End Do
               Endif
               AMT8=AMT
!
!  If this is a BACKUP right, the shortage for other rights are added.
!
               If(IBACKUP.EQ.9) Then
                  If(BUWR(N).GT.0) Then
                     AMT=AMT+(SHTBACK(BUWR(N))*BUX(N))
                  Endif
                  IBACKUP=0
                  If(BUG(N).NE.'        ') Then
                      Do J=1,NWRTS
                         If(WRIDS(J,1).EQ.BUG(N).or.
     +                      WRIDS(J,2).EQ.BUG(N)) Then
                            AMT=AMT+(SHTBACK(J)*BUX(N))
                         Endif
                      Enddo
                  Endif
               Endif
!____________________________________________________________________________
!
!  Variables related to instream flow requirements are initialized, and the
!  instream flow target is set.
!
               IFFLAG=99
               If(WRIDS(WR,1).EQ.'IF#IF*IF') Then
                  IFFLAG=IFMETH(WR)
                  If(ADL(WR).GT.0.0) Then
                     If(ANNREG(WR)-ADL(WR).GE.-0.0001) AMT=0.0
                  Endif
                  If(WRNUM(WR,10).LE.1) Then
                     IFTARGET(LOCNUM)=AMT
                  Elseif(WRNUM(WR,10).EQ.2) Then
                     If(IFTARGET(LOCNUM).LT.AMT) IFTARGET(LOCNUM)=AMT
                  Elseif(WRNUM(WR,10).EQ.3) Then
                     If(IFTARGET(LOCNUM).GT.AMT) IFTARGET(LOCNUM)=AMT
                  Endif
                  AMTIF(WR)=IFTARGET(LOCNUM)
                  If(IFTARGET(LOCNUM).LT.0.0) IFTARGET(LOCNUM)=0.0
                  If(IFTARGET(LOCNUM).GT.0.0) IFPASS=Abs(IFPASS)
                  If(IFPASS.NE.2.and.(IFFLAG.EQ.2.or.IFFLAG.EQ.4))
     +                IFTARGET(LOCNUM)=0.0
                  IFFLAG=Abs(IFFLAG)
                  RFAC=1.0
                  RET=0.0
                  AMT=0.0
!
!  The IFRESREL switch is activated if specified by non-zero IFFLAG2 in IF
!  record field 8, which is recorded in READDAT as WRIDS(WR,2)="IFRESREL".
!
                  IFRESREL(LOCNUM)=0
                  If(WRIDS(WR,2).EQ.'IFRESREL') IFRESREL(LOCNUM)=-99
!
!  If this is an instream flow right (IF record) without storage (WS record),
!  skip to statement 1190.
!
                  If(IFFLAG.LE.2) Then
                     RE=0
                     Goto 1190
                  Endif
!
!  If the instream flow requirement includes reservoir storage (WS record and
!  IFFLAG = 3 or 4), the instream flow shortage is treated as a diversion
!  with a return flow factor of 1.0.
!
                  If(IFFLAG.EQ.3.or.(IFFLAG.EQ.4.and.IFPASS.EQ.2)) Then
                     LIMIT=0.0
                     If(WRIDS(WR,2).EQ.'IFRESREL') Then
                        RFLOW=CPFLOW(LOCNUM,MT,2)
                     Else
                        RFLOW=CPFLOW(LOCNUM,MT,2)+RESREL(LOCNUM)
                     Endif
                     If(ADJINC.EQ.4) Then
                        RFLOW=RFLOW-CPFLOW(LOCNUM,MT,1)
                        If(RFLOW.LT.0.0) RFLOW=0.0
                     Endif
                     If(IFFLAG.EQ.3) LIMIT=RFLOW-IFTARGET(LOCNUM)
                     If(IFFLAG.EQ.4.and.IFPASS.EQ.2) Then
                        LIMIT=RFLOW-IFTARGET(LOCNUM)
                        If(REGFLOW(LOCNUM).GT.RFLOW)
     +                          LIMIT=REGFLOW(LOCNUM)-IFTARGET(LOCNUM)
                     Endif
                     If(LIMIT.GE.0.0) Then
                        Goto 1190
                     Else
                        AMT=-LIMIT
                        RFAC=1.0
                        RETNUM=LOCNUM
                     Endif
                  Endif
               Endif
!____________________________________________________________________________
!
!  The amount of water available to the right is determined.
!               *+*+*+*+*  Call Subroutine AVALB  *+*+*+*+*
!
               If(WRT.EQ.-3.or.WRT.EQ.11.or.WRT.EQ.12) Goto 970
               Call AVALB
               XCPAV=XCPAV1
               XAVAMT=AVAMT
!
!  Restrict water available to right based on annual or seasonal and monthly
!  streamflow depletion limits from SO and ML records.
!
               If(ANNDEP(WR).GT.0.0009.or.MONDEP(WR).GT.0.0009.or.
     +            WRNUM(WR,4).NE.0) Then
                  DEPLIM=AVAMT
                  KK=WRNUM(WR,4)
                  If(ANNDEP(WR).GT.0.0009) Then
                     DEPLIM=ANNDEP(WR)-ANNDIV(WR)
                  Endif
                  If(MONDEP(WR).GT.0.0009) Then
                     If(MONDEP(WR).LT.DEPLIM) DEPLIM=MONDEP(WR)
                  Endif
                  If(KK.GT.0) Then
                     If(MSD(KK,MT).LT.DEPLIM) DEPLIM=MSD(KK,MT)
                  Endif
                  If(DEPLIM.LT.0.0) DEPLIM=0.0
                  If(DEPLIM.LT.AVAMT) AVAMT=DEPLIM
               Endif
!
!  Restrict water available to right based on streamflow
!  depletion limits from dual simulation options.
!
               If((DUAL(WR).EQ.3.or.DUAL(WR).EQ.5).and.DUALFLAG.GT.0)
     +         Then
                  DEPLIM=AVAMT
                  If(CR1.GT.0.and.CR2.EQ.0) Then
                     X=CRDDEP(DD(WR),I,MT,CRLOOP)+DDEPX(DD(WR))
                  Else
                     X=DDEP(DD(WR),I,MT)+DDEPX(DD(WR))
                  Endif
                  If(X.LT.DEPLIM) DEPLIM=X
                  If(DEPLIM.LT.0.0) DEPLIM=0.0
                  If(DEPLIM.LT.AVAMT) AVAMT=DEPLIM
               Endif
!
!  Restrict water available to right based on streamflow
!  depletion limits from TS records.
!
               If(SERIES(WR).GT.0) Then
                  ITS=SERIES(WR)
                  If(TSCOM(ITS).EQ.'SDL') Then
                     DEPLIM=AVAMT
                     If(QTS(I,ITS,MT).LT.DEPLIM) DEPLIM=QTS(I,ITS,MT)
                     If(DEPLIM.LT.0.0) DEPLIM=0.0
                     If(DEPLIM.LT.AVAMT) AVAMT=DEPLIM
                  Endif
               Endif
!
!  Restrict target amount based on annual and monthly reservoir release limits.
!
               If(MRW(WR).GT.0.0.or.ARW(WR).GT.0.0) Then
                  If(WRT.EQ.3) AVAMT=0.0
                  XRES=AMT-AVAMT
                  If(XRES.LT.0.0) XRES=0.0
                  If(XRES.GT.MRW(WR).and.MRW(WR).GT.0.00001) Then
                     AMT=MRW(WR)+AVAMT
                     XRES=MRW(WR)
                  Endif
                  If(XRES.GT.(ARW(WR)-ANNRES(WR)).and.
     +               ARW(WR).GT.0.00001) AMT=ARW(WR)-ANNRES(WR)+AVAMT
                  If(AMT.LT.0.0) AMT=0.0
               Endif
!
!  Restrict target amount based on annual or seasonal diversion limit.
!
               If(ADL(WR).GT.0.0001) Then
                  If(AMT.GT.(ADL(WR)-ANNWSD(WR))) Then
                     AMT=ADL(WR)-ANNWSD(WR)
                     If(AMT.LT.0.0) AMT=0.0
                  Endif
               Endif
!____________________________________________________________________________
!
!  The diversion or energy supplied, shortage, and reservoir releases are
!  computed based on water right type WRT specified on WR record.
!
!  Hydropower right.  Subroutine POWER is called to determine streamflow
!  depletion and power produced. *+*+*+*+*  Call Subroutine POWER  *+*+*+*+*
!
970            If(WRT.LT.0) Then
                  If(WRT.EQ.-3) Then
                     AVAMT=0.0
                     DEP=0.0
                  Endif
                  Call POWER
                  Goto 1150
               Endif
!
!  Run-of-river right.  Type 1 or 2 right with no reservoir.
!
               If((WRT.EQ.1.or.WRT.EQ.2).and.RE.EQ.0) Then
                  DEP=Min(AMT,AVAMT)
                  SHT=Max(AMT-AVAMT,0.)
                  MAKEUP=0.0
                  Goto 1140
               Endif
!
!  Type 1 or type 7 water right with a reservoir.

               If(WRT.EQ.1.or.WRT.EQ.7) Then
!
!  Reservoir computations are skipped for run-of-river rights.
!
                  If(RI.EQ.0) Goto 1140
!
!  Reservoir storage capacity for this water right.
!
                  RESCAP=WRSYS(1,3)
!
!  Storage capacity (RESCAP) is set at target (AMT) for Type 7 right.
!
                  If(WRT.EQ.7) Then
                     RESCAP=AMT
                     T7AMT=AMT
                     AMT=0.0
                  Endif
!
!  Limit end-of-period storage to MS record monthly varying storage capacity.
!
                  If(MSFLAG.GT.0) Then
                     IR=0
980                  IR=IR+1
                     If(MSRES(IR).EQ.WRNUM(WR,9)) Then
                        If(STMON(IR,MT).LT.RESCAP) RESCAP=STMON(IR,MT)
                        MSFLAG2=9
                        Goto 990
                     Endif
                     If(IR.LT.MSFLAG) Goto 980
990               Endif
!
!  Limit end-of-period storage to OS record monthly varying storage capacity.
!
                  If(OSFLAG.GT.0) Then
                     IR=0
1000                 IR=IR+1
                     If(OSRES(IR).EQ.WRNUM(WR,9)) Then
                        L=12*(I-1)+MT
                        If(OS(IR,L).LT.RESCAP) RESCAP=OS(IR,L)
                        OSFLAG2=9
                        Goto 1010
                     Endif
                     If(IR.LT.OSFLAG) Goto 1000
1010              Endif
!
!  End-of-period storage EPSTOR is set at capacity RESCAP at the
!  beginning of the algorithm subject to change in later iterations.
!
                  EPSTOR=RESCAP
!
!  Evaporation volume is allocated for EA record reservoirs. Total storage in
!  all reservoirs on the EA record is used to determine water surface area for
!  NEAF(N) options 2 and 3. Area is based on accumulative storage in priority
!  loop for NEAF(N) options 1 and 4.
!
                  If(EAR(RI).GT.0) Then
                     EASBEG=0.0
                     EASEND=0.0
                     TOTCAP=0.0
                     STORAGE=0.0
                     N=EAR(RI)
                     MATCH=0
!
!                 Reservoir is identified and total capacity and
!                 total beginning and ending storage are summed.
!
                     If(NEAF(N).GE.2) Then
                        EAZERO=0
                        Do 1020 J=1,EARNUM(N)
                           If(EARES(N,J).EQ.RESID(RI)) JRES=J
                           L=EAI(N,J)
                           EASBEG=EASBEG+RESDAT(L,5)
                           EASEND=EASEND+RESDAT(L,6)
                           TOTCAP=TOTCAP+RESDAT(L,1)
                           If(L.EQ.RI) MATCH=MATCH+1
                           If(EAO(N).LE.1.or.EAO(N).EQ.3) Then
                              X=RESDAT(L,5)/RESDAT(L,1)
                              If(X.LE.EAL(N)) Then
                                 EAZERO=9
                              Endif
                           Endif
1020                    End Do
                        If(EASBEG.GT.TOTCAP) EASBEG=TOTCAP
                        If(EASEND.GT.TOTCAP) EASEND=TOTCAP
!
!                 Storage limit switch is defined by EF record fields 2 and 3.
!
                        If(EAO(N).EQ.2.or.EAO(N).EQ.4) Then
                           X=EASBEG/TOTCAP
                           If(X.LE.EAL(N)) EAZERO=9
                        Endif
!
!                 Error check that the reservoir is listed on the EA record.
!
                        If(MATCH.EQ.0) Then
                           Write(14,1030) RESID(RI)
1030                       Format(' ERROR: Reservoir ',A6,' could not ',
     +                        'be matched with an EA record reservoir.')
                           Write(14,1620)
                           Call ERROR
                        Endif
                     Endif
!
!                 EA record NEAF option 2 factors are based on beginning storage.
!
                     If(NEAF(N).EQ.2) Then
                        If(EASBEG.LE.0.0) Then
                           EAFACT=1.0/Real(EARNUM(N))
                        Else
                           EAFACT=RESDAT(RI,5)/EASBEG
                        Endif
                     Endif
!
!                 EA record NEAF option 3 uses multiplier factors from EF records.
!
                     If(NEAF(N).EQ.3) Then
                        If(EAZERO.EQ.9) Then
                           If(EASBEG.LE.0.0) Then
                              EAFACT=EAF(N,JRES)
                           Else
                              EAFACT=RESDAT(RI,5)/EASBEG
                           Endif
                        Else
                           EAFACT=EAF(N,JRES)
                        Endif
                     Endif
!
!                 EA record NEAF option 4 uses multiplier factors from EF records
!                 in combination with incremental evaporation.
!
                     If(NEAF(N).EQ.4) Then
                        If(EAZERO.EQ.9) Then
                           If(EASBEG.LE.0.0) Then
                              EAFACT=1.0
                           Else
                              If(EAO(N).LE.2) Then
                                 EAFACT=RESDAT(RI,5)/EASBEG
                              Else
                                 EAFACT=1.0
                              Endif
                           Endif
                        Else
                           EAFACT=EAF(N,JRES)
                        Endif
                     Endif
!
!                 Options 1 & 4 are based on incremental evap. Accumulated
!                 storage as each reservoir is considered in priority loop.
!
                     If(NEAF(N).LE.1.or.NEAF(N).EQ.4) Then
                        EASBEG=CUMBEG(N)+RESDAT(RI,5)
                        EASEND=CUMEND(N)+RESDAT(RI,6)
                        EASCAP=CUMCAP(N)+RESDAT(RI,1)
                     Endif
!
!                 Options 2 and 3 compute evaporation based on total storage
!                 in all the EA record reservoirs. EASCAP is total capacity.
!
                     If(NEAF(N).EQ.2.or.NEAF(N).EQ.3) EASCAP=TOTCAP
!
!                 First estimate of evaporation volume is computed for a EA
!                 record reservoir.*+*+*+*  Call Subroutine LINEAR  *+*+*+*
!
                     If(RESDAT(RI,2).LT.0.0) Then
                        Call LINEAR(EASBEG,BPAREA,RESNUM(RI,2),EVCURV)
                        Call LINEAR(EASCAP,EPAREA,RESNUM(RI,2),EVCURV)
                     Else
                        EVCFA=RESDAT(RI,2)
                        EVCFB=RESDAT(RI,3)
                        EVCFC=RESDAT(RI,4)
                        BPAREA=EVCFA*(EASBEG**EVCFB)+EVCFC
                        EPAREA=EVCFA*(EASCAP**EVCFB)+EVCFC
                     Endif
                     EVOL=EVAPR(RESNUM(RI,1),MT)*(EPAREA+BPAREA)/2.0
                     If(NEAF(N).LE.1) EVOL=EVOL-CUMEV(N)
                     If(NEAF(N).GE.2) Then
                        If(JRES.EQ.EARNUM(N).and.EAX(N).EQ.0) Then
                           X=0.0
                           Do J=1,EARNUM(N)-1
                              X=X+RESDAT(EAI(N,J),7)
                           End Do
                           EVOL=EVOL-X
                        Else
                           If(NEAF(N).EQ.2.or.NEAF(N).EQ.3) Then
                              EVOL=EVOL*EAFACT
                           Elseif(NEAF(N).EQ.4) Then
                              EVOL=(EVOL-CUMEV(N))*EAFACT
                           Endif
                        Endif
                     Endif
!
!  Otherwise without a EA record, the net evaporation volume EVOL is
!  computed as follows. *+*+*+*+*  Call Subroutine LINEAR  *+*+*+*+*
!
                  Else
                     BPSTOR=Min(RESDAT(RI,5),RESCAP)
                     If(OSFLAG2.EQ.9) BPSTOR=RESDAT(RI,5)
                     If(RESDAT(RI,2).LT.0.0) Then
                        Call LINEAR(BPSTOR,BPAREA,RESNUM(RI,2),EVCURV)
                        Call LINEAR(RESCAP,EPAREA,RESNUM(RI,2),EVCURV)
                     Else
                        EVCFA=RESDAT(RI,2)
                        EVCFB=RESDAT(RI,3)
                        EVCFC=RESDAT(RI,4)
                        BPAREA=EVCFA*(BPSTOR**EVCFB)+EVCFC
                        EPAREA=EVCFA*(RESCAP**EVCFB)+EVCFC
                     Endif
                     EVOL=EVAPR(RESNUM(RI,1),MT)*(EPAREA+BPAREA)/2.0
                  Endif
!
!  The target streamflow depletion (TAR) needed to refill reservoir, meet
!  diversion target,and account for evaporation is determined, accounting
!  for previous flow depletions and releases at the reservoir.
!
                  BPSTOR=Min(RESDAT(RI,5),RESCAP)
                  If(OSFLAG2.EQ.9) BPSTOR=RESDAT(RI,5)
                  TAR=RESCAP-BPSTOR+EVOL+AMT+RESDAT(RI,9)
     +                +RESDAT(RI,10)-RESDAT(RI,8)-RESDAT(RI,11)
                  If(TAR.LT.0.0.and.NOTF(WR).NE.-88) Then
                     If((RESDAT(RI,1)-RESCAP).GT.0.001) TAR=0.0
                     If(NOTF(WR).EQ.-8) TAR=0.0
                  Endif
!
!  The streamflow depletion and reservoir release are determined.
!               *+*+*+*+*  Call Subroutine RELEASE  *+*+*+*+*
!
                  DEP=TAR
                  RELS=0.0
                  SHT=0.0
                  If(TAR-AVAMT.GE.0.0001) Then
                     X=(TAR-AVAMT)/TAR
                     If(X.GE.0.00001) Then
                        DEP=AVAMT
                        MAKEUP=TAR-AVAMT
                        If(SWR.GT.0) Call RELEASE
                     Endif
                  Endif
!
!  Subroutine RESCAL computes end-of-period storage if the target depletion
!  is greater than available streamflow and system releases. Otherwise, with
!  the target depletion less/equal total water available (AVAMT+RELS), the
!  reservoir is refilled, and the permitted diversion is met.
!               *+*+*+*+*  Call Subroutine RESCAL  *+*+*+*+*
!
                  If(TAR-AVAMT-RELS.GE.0.0001) Then
                     X=(TAR-AVAMT-RELS)/TAR
                     If(X.GE.0.0001) Then
                        INRES=AVAMT+RELS
                        OUTRES=AMT
                        Call RESCAL(RI,WRSYS(1,1),WRSYS(1,3),INRES,
     +                              OUTRES,EPSTOR,EVOL)
                        SHT=AMT-(OUTRES-RESDAT(RI,9)-RESDAT(RI,10))
                        If(SHT.LT.0.0) SHT=0.0
                     Endif
                  Endif
!
!  With OS or MS record monthly varying storage capacity limits, the end-of-
!  month storage is limited to not exceed the OS or MS record storage volume.
!
                  If(OSFLAG2.GT.0.or.MSFLAG2.GT.0) Then
                     X=EPSTOR/RESDAT(RI,15)
                     If(X.GT.1.00001) Then
                        SPILL=EPSTOR-RESDAT(RI,15)
                        EPSTOR=RESDAT(RI,15)
                        DF=SPILL
                        NCP=RESNUM(RI,1)
1120                    CPFLOW(NCP,MT,2)=CPFLOW(NCP,MT,2)+DF
                        LOSS=CL(NCP)*DF
                        CLOSS(NCP,2)=CLOSS(NCP,2)+LOSS
                        DF=(1-CL(NCP))*DF
                        NCP=CPNXT(NCP)
                        If(NCP.GT.0) Goto 1120
                     Endif
                  Endif
!
!  Reservoir data arrays are updated.
!
                  RESDAT(RI,6)=EPSTOR
                  RESDAT(RI,7)=EVOL
                  RESDAT(RI,8)=RESDAT(RI,8)+DEP
                  RESDAT(RI,11)=RESDAT(RI,11)+RELS
                  SYSREL(1)=AMT-SHT
!
!  Storage amounts are acumulated for EA record resevoirs
!  with NEAF options 1 or 4.
!
                  If(EAR(RI).GT.0) Then
                     N=EAR(RI)
!
!        Arrays are re-zeroed for a second or subsequent set of rights.
!
                     EACOUNT(N)=EACOUNT(N)+1
                     If(EACOUNT(N).GT.EARNUM(N)) Then
                        EACOUNT(N)=1
                        CUMEV(N)=0.0
                        CUMBEG(N)=0.0
                        CUMEND(N)=0.0
                        CUMCAP(N)=0.0
                     Endif
!
!        Arrays are incremented for this right of the EA set of rights.
!
                     CUMEV(N)=CUMEV(N)+EVOL
                     CUMBEG(N)=CUMBEG(N)+RESDAT(RI,5)
                     CUMEND(N)=CUMEND(N)+EPSTOR
                     CUMCAP(N)=CUMCAP(N)+RESDAT(RI,1)
                  Endif
!
!  Warning message is printed if the net evaporation volume is
!  negative relative to the sign of the net evaporation depth.
!
                  If(ICHECK.EQ.1) Then
                  If((EVAPR(RESNUM(RI,1),MT).GT.0.001.and.EVOL.LT.-0.1)
     +              .or.(EVAPR(RESNUM(RI,1),MT).LT.-0.001.and.
     +               EVOL.GT.0.1)) Then
                     Write(14,1130) YEAR,MT,RESID(RI),EVOL,
     +                              (EVAPR(RESNUM(RI,1),MT))
1130                 Format(' WARNING: Evaporation volume has wrong',
     +                      ' sign.'I6,I3,2x,A6,'   Volume =',F9.1,
     +                      '   depth =',F7.3)
                  Endif
                  Endif
!
!  Add releases through outlet works or from reservoir pool.
!
                  If(SWR.GT.0) Then
                     If(SN3(SWR,1).LT.0) Then
                        RESDAT(RI,10)=RESDAT(RI,10)+AMT-SHT
                     Else
                        RESDAT(RI,9)=RESDAT(RI,9)+AMT-SHT
                     Endif
                  Else
                     If(WRNUM(WR,11).LT.0) Then
                        RESDAT(RI,10)=RESDAT(RI,10)+AMT-SHT
                     Else
                        RESDAT(RI,9)=RESDAT(RI,9)+AMT-SHT
                     Endif
                  Endif
!
!  Type 2 water right.
!
               Elseif(WRT.EQ.2) Then
!
!  Determine depletion amount (DEP) and amount to release from system 
!  reservoirs (MAKEUP). *+*+*+*+*  Call Subroutine  RELEASE  *+*+*+*+*
!
                  If(AVAMT-AMT.GT.0.0001) Then
                     DEP=AMT
                     MAKEUP=0.0
                  Else
                     DEP=AVAMT
                     MAKEUP=AMT-DEP
                     If(MAKEUP.LT.0.0001) Then
                        MAKEUP=0.0
                     Else
                        If(SWR.GT.0) Call RELEASE
                     Endif
                  Endif
                  SHT=Max(0.0,MAKEUP-RELS)
!
!  Type 3 water right.
!
               Elseif(WRT.EQ.3) Then
!
!  Determine amount to release from system reservoirs (MAKEUP) since
!  Type 3 rights are not permitted to make streamflow depletions.
!
                  AVAMT=0.0
                  DEP=0.0
                  MAKEUP=AMT
                  If(MAKEUP.GT.0.0001.and.SWR.GT.0) Call RELEASE
                  SHT=Max(0.0,MAKEUP-RELS)
!
!  Transient priority rights created by PX record adjust
!  available flows for prior return flows and depletions.
!           *+*+*+*+*  Call Subroutine ADJUST  *+*+*+*+*
!
               Elseif(WRNUM(WR,8).LT.0) Then
                  L=Abs(WRNUM(WR,8))
                  If(XP(L).EQ.1) Then
                     RET=XPRET(L)
                     DEP=0.0
                  Elseif(XP(L).EQ.2) Then
                     DEP=XPRET(L)
                     RET=XPSFD(L)
                  Endif
                  RFOUT=RET
                  XCPAV1=XCPAV
                  Call ADJUST
                  Goto 1200
               Endif
!____________________________________________________________________________
!
!  Return flow amount is computed and accumulated for IF rights with storage.
!
               If(IFFLAG.NE.99) Then
                  RET=(AMT-SHT)*1.0
                  RETSUM(RETNUM,MT)=RETSUM(RETNUM,MT)+RET
                  Goto 1160
               Endif
!
!  Return flow amount is computed and accumulated for WR record rights.
!
1140           If(RFMETH(WR).EQ.0.and.RFAC.EQ.0.0) RET=0.0
               If(RFMETH(WR).EQ.0.and.RFAC.NE.0.0) RET=(AMT-SHT)*RFAC
               If(RFMETH(WR).EQ.1.or.RFMETH(WR).EQ.2)
     +               RET=(AMT-SHT)*RFAC
               If(RFMETH(WR).EQ.3.or.RFMETH(WR).EQ.4) Then
                     KK=IRF(WR)
                     RET=(AMT-SHT)*RF(KK,MT)
               Endif
1150           If(WRNUM(WR,5).LE.-1) Then
                  If(RFAC.LT.0.001) RFAC=1.0
                  RET=RET*RFAC
               Endif                  
               If(WRNUM(WR,5).EQ.4) RET=AMTRF
               RFOUT=RET
               If(WRNUM(WR,3).EQ.-99) Then
                  RET=0.0
               Elseif(RFMETH(WR).EQ.2.and.MT.LT.NPRDS) Then
                  RETSUM(RETNUM,MT+1)=RETSUM(RETNUM,MT+1)+RET
               Elseif(RFMETH(WR).EQ.4.and.MT.LT.NPRDS) Then
                  RETSUM(RETNUM,MT+1)=RETSUM(RETNUM,MT+1)+RET
               Elseif(RFMETH(WR).NE.2.and.RFMETH(WR).NE.4) Then
                  RETSUM(RETNUM,MT)=RETSUM(RETNUM,MT)+RET
               Endif
               If(RFMETH(WR).EQ.2.or.RFMETH(WR).EQ.4) Then
                  RFAC=-99.99
                  If(MT.EQ.NPRDS.and.RETNUM.GT.0) Then
                     CPRET(RETNUM)=CPRET(RETNUM)+RET
                     RET=0.0
                  Endif
               Endif
!
!  If XP option 1 is selected in PX record field 6, the return
!  flow occurs later in the priority loop than the diversion.
!
               If(WRNUM(WR,8).GT.0) Then
                  L=WRNUM(WR,8)
                  If(XP(L).EQ.1) Then
                     XPRET(L)=RET
                     RET=0.0
                     RFOUT=RET
                  Endif
               Endif
!____________________________________________________________________________
!
!  Instream flow right with storage (IFFLAG = 3 or 4) skips to here.
!
1160           Continue
!
!  Streamflow depletions are accumulated.
!
               DEPSUM(LOCNUM)=DEPSUM(LOCNUM)+DEP
               DMRSUM(LOCNUM)=DMRSUM(LOCNUM)+DEP
!
!  Unapropriated flows are adjusted by depletion and return flow amounts.
!                           *+*+*+*+*  Call Subroutine ADJUST  *+*+*+*+*
!
               XCPAV1=XCPAV
               Call ADJUST
!
! Determine what storage amounts to output.
!
               If((WRT.EQ.1.and.RE.GT.0).or.WRT.EQ.-1.or.WRT.EQ.7) Then
                  EPSTOR=RESDAT(RI,6)
                  EVOL=RESDAT(RI,7)
               Else
                  EPSTOR=0
                  EVOL=0
               Endif
!
!  RESREL is additional flow at a control point from releases from secondary
!  reservoirs located upstream for diversions located downstream.  RESREL
!  at each CP is updated here for each water right with secondary reservoir
!  releases for use in adjusting regulated flows later in the main program
!  and subroutine AVALB.
!
               If(RELS.GT.0.001.and.WRT.GE.0.and.SWR.GT.0) Then
                  If(WRT.EQ.1) Then
                     JJ=2
                  Else
                     JJ=1
                  Endif
                  Do 1180 Z=JJ,RE
                     If(SN2(SWR,Z).GE.0) Then
                        L=SN1(SWR,Z)
                        NPT=RESNUM(L,1)
                        CLX=1.0
1170                    If(NPT.EQ.LOCNUM.or.NPT.EQ.SN2(SWR,Z)) Goto 1180
                        If(NPT.LT.0) Goto 1180
                        RESREL(NPT)=RESREL(NPT)+SYSREL(Z)*CLX
                        NUSCP=NPT
                        If(CL(NUSCP).GT.0.0) Then
                           LOSS=SYSREL(Z)*CLX*CL(NUSCP)
                           CLX=CLX*(1.0-CL(NUSCP))
                           CLOSS(NUSCP,2)=CLOSS(NUSCP,2)+LOSS
                        Endif
                        NPT=CPNXT(NPT)
                        Goto 1170
                     Endif
1180              End Do
               Endif
!____________________________________________________________________________
!
!  Instream flow rights without storage skip to here.
!
1190           Continue
!
!  Store streamflow depletion, diversion, and withdrawals from reservoir storage.
!
               SFD(WR)=DEP
               WSD(WR)=AMT-SHT
               WST(WR)=AMT
               ASF=AVAMT-EVOL
               If(ASF.LT.0.0) ASF=0.0
               RESW(WR)=AMT-SHT-ASF
               If(RESW(WR).LT.0.0) RESW(WR)=0.0
!
!  Streamflow depletions are stored during first simulation for DUAL(wr)
!  options 3 or 4 specified on the JO, SO, or PX record.
!
               If(DUALFLAG.EQ.0.and.NDD.GT.0) Then
                  If(DUAL(WR).EQ.3.or.DUAL(WR).EQ.4) Then
                     If(CR1.GT.0.and.CR2.EQ.0) Then
                        CRDDEP(DD(WR),I,MT,CRLOOP)=DEP
                     Else
                        DDEP(DD(WR),I,MT)=DEP
                     Endif
                  Endif
               Endif
!
!  Cumulative depletions DDEPX are updated at end of second simulation
!  for JO, SO, or PX record DUAL options DUALX of 33, 333, 55, or 555.
!
               If(DUALFLAG.GT.0.and.NDD.GT.0.and.DD(WR).GT.0) Then
                  If(DUALX(DD(WR)).GE.33) Then
                     If(CR1.GT.0.and.CR2.EQ.0) Then
                        X2=CRDDEP(DD(WR),I,MT,CRLOOP)
                     Else
                        X2=DDEP(DD(WR),I,MT)
                     Endif
                     DDEPX(DD(WR))=DDEPX(DD(WR))+X2-DEP
                     If(DDEPX(DD(WR)).LT.0.0) DDEPX(DD(WR))=0.0
                     If(DUALX(DD(WR)).GE.333) Then
                        If((WRT.EQ.1.or.WRT.EQ.7).and.RE.GT.0) Then
                           X=EPSTOR/RESCAP
                           If(X.GE.0.9999) DDEPX(DD(WR))=0.0
                        Else
                           DDEPX(DD(WR))=X2-DEP
                           If(DDEPX(DD(WR)).LT.0.0) DDEPX(DD(WR))=0.0
                        Endif
                     Endif
                  Endif
               Endif
!
!  Define target and shortage based on ISHT(wr) from SO record and type.
!
               If(ISHT(WR).EQ.1) Then
                  SHT=SHT+AMT1-AMT
                  AMT=AMT1
               Elseif(ISHT(WR).EQ.2) Then
                  SHT=SHT+AMT2-AMT
                  AMT=AMT2
               Elseif(ISHT(WR).EQ.3) Then
                  SHT=SHT+AMT3-AMT
                  AMT=AMT3
               Elseif(ISHT(WR).EQ.4) Then
                  SHT=SHT+AMT4-AMT
                  AMT=AMT4
               Elseif(ISHT(WR).EQ.5) Then
                  SHT=SHT+AMT5-AMT
                  AMT=AMT5
               Elseif(ISHT(WR).EQ.6) Then
                  SHT=SHT+AMT6-AMT
                  AMT=AMT6
               Elseif(ISHT(WR).EQ.7) Then
                  SHT=SHT+AMT7-AMT
                  AMT=AMT7
               Elseif(ISHT(WR).EQ.8) Then
                  SHT=SHT+AMT7-AMT
                  AMT=AMT8
               Endif
               If(WRT.EQ.4) Then
                  AMT=-AMTRF
                  SHT=0.0
               Endif
               If(WRT.EQ.7) Then
                  AMT=T7AMT
                  SHT=EPSTOR-AMT
               Endif
               If(WRT.LT.0) Then
                  AMT=RESDAT(RI,13)
                  SHT=RESDAT(RI,13)-RESDAT(RI,12)
                  If(SHT.LE.-999999.0) SHT=-999999.0
               Endif
!______________________________________________________Write Output Files
!
!  Write water right output record to output file (root.OUT).
!
1200           If(CRWRITE.EQ.-99) Goto 1280
               If((SIM2.EQ.9.and.DUALFLAG.EQ.0).or.SIM3.GE.9) Goto 1280
               If(WRNUM(WR,6).EQ.1) Then
                  If(DSS(1).EQ.0) Goto 1240
                  If(WRIDS(WR,1).EQ.'IF#IF*IF') Then
                     If(OUTFILE.GE.2) Then
                        J=-1
                        Write(4,REC=RECD) J,MT,SHT,AMT,EVOL,
     +                        EPSTOR,DEP,AVAMT,RELS,WRID(WR),FSVX,FSJJ
                     Else
                        Write(4,1210,REC=RECD) MT,SHT,AMT,EVOL,EPSTOR,
     +                               DEP,AVAMT,RELS,WRID(WR),FSVX,FSJJ
1210                    Format('IF',2x,I2,7F11.2,A16,22(' '),F11.2,I4)
                     Endif
                  Else
                     If(OUTFILE.GE.2) Then
                        Write(4,REC=RECD) YEAR,MT,SHT,AMT,EVOL,
     +                               EPSTOR,DEP,AVAMT,RELS,WRID(WR),
     +                               WRIDS(WR,1),WRIDS(WR,2),RFOUT,XAV
                     Else
                        If(AMT.LE.9999000.0.and.AVAMT.LE.99999000.0)Then
                           Write(4,1220,REC=RECD) YEAR,MT,SHT,AMT,EVOL,
     +                                  EPSTOR,DEP,AVAMT,RELS,WRID(WR),
     +                                 WRIDS(WR,1),WRIDS(WR,2),RFOUT,XAV
                        Else
                           Write(4,1230,REC=RECD) YEAR,MT,SHT,AMT,EVOL,
     +                                  EPSTOR,DEP,AVAMT,RELS,WRID(WR),
     +                                 WRIDS(WR,1),WRIDS(WR,2),RFOUT,XAV
                        Endif
1220                    Format(I4,I2,2F11.3,5F11.2,A16,2A8,F11.2,F10.2)
1230                    Format(I4,I2,7F11.1,A16,2A8,F11.1,F10.1)
                     Endif
                  Endif
1240              RECD=RECD+1
                  NWRREC=NWRREC+1
                  If(NWRREC.GE.CRNWR) CRNWR=NWRREC
!
!  Arrays for DSS and SOU files.
!
                  If(DSS(2).EQ.1.or.DSS(3).EQ.1) Then
                     DSSWRJ=DSSWRJ+1
                     DSSWRI(DSSWRJ)=WR
                     J=((I-1)*12)+MT
                     If(DSS(4).EQ.0.and.WRIDS(WR,1).NE.'IF#IF*IF') Then
                        DSSWR(DSSWRJ,J,1)=AMT
                        DSSWR(DSSWRJ,J,2)=SHT
                        DSSWR(DSSWRJ,J,3)=EPSTOR
                        DSSWR(DSSWRJ,J,4)=DEP
                     Endif
                     If(DSS(4).EQ.1) Then
                        DSSWR(DSSWRJ,J,1)=EPSTOR
                        DSSWR(DSSWRJ,J,2)=EVOL
                        DSSWR(DSSWRJ,J,3)=DEP
                        DSSWR(DSSWRJ,J,4)=AMT
                        DSSWR(DSSWRJ,J,5)=SHT
                        DSSWR(DSSWRJ,J,6)=AVAMT
                        DSSWR(DSSWRJ,J,7)=RELS
                        If(WRIDS(DSSWRJ,1).NE.'IF#IF*IF') Then
                           DSSWR(DSSWRJ,J,8)=RFOUT
                           DSSWR(DSSWRJ,J,9)=XAV
                        Else
                           DSSWR(DSSWRJ,J,10)=FSVX
                           DSSWR(DSSWRJ,J,11)=Real(FSJJ)
                        Endif
                     Endif
                  Endif
!
!  Write output to hydropower and multi-reservoir system release file (root.HRR).
!
                  If(SWR.GT.0.and.F7.GT.0) Then
                     Write(13,1250) Adjustl(WRID(WR)),NSR(SWR),
     +               YEAR,MT,FRMPOW,POWPRO,
     +               (SYSREL(Z),Adjustl(RESID(SN1(SWR,Z))),Z=1,NSR(SWR))
1250                 Format(A16,I3,I6,I3,2F10.1,30(F10.1,1x,A6))
                  Endif
               Endif
!
!  Priority loop flows are written to ZZZ file if activated by ZZ record.
!
               If(ZZ.GE.1) Then
                  ZZR=K
                  If(K.EQ.1) ZZCALL=99
                  If(ZZCALL.EQ.99) Call ZZFLOW
                  If(ZZWR.EQ.WRID(WR)) ZZCALL=-99
               Endif
!___________________________________________________________________________
!
!  Hydropower (type -1 or -3), inflow (type 4), or storage (type 7) target and
!  shortage set to zero to prevent increasing control point AMT and SHT totals.
!
1280           If(WRT.EQ.4.or.WRT.EQ.7.or.WRT.LT.0) Then
                  AMT=0.0
                  SHT=0.0
               Endif
!
!  Control point monthly summary values are accumulated.
!
               If(AMT.GT.0.0.and.WRIDS(WR,1).NE.'IF#IF*IF') Then
                  CPSUM(LOCNUM,1)=CPSUM(LOCNUM,1)+SHT
                  CPSUM(LOCNUM,2)=CPSUM(LOCNUM,2)+AMT
               Endif
!
!  Save the shortage SHT as SHTBACK(WR) for possible backup right.
!
               SHTBACK(WR)=SHT
!
!  Old SO record version of backup right skips IF record rights.
!
               If(WRTO(WR).GT.0) Then
                  If(WR.GE.2.and.TOFLAG(WRTO(WR)).LE.-8) Then
                     If(WRIDS(WR,1).EQ.'IF#IF*IF') Then
                        SHTBACK(WR)=SHTBACK(WR-1)
                     Endif
                  Endif
               Endif
!
!  PX record control point limit options.
!
               If(WRNUM(WR,8).GT.0) Then
                  L=WRNUM(WR,8)
                  If(XCP(L).GE.1) Then
                     SHTBACK(WR)=0.0
                     If(DEP.GT.(XAVAMT-XAV)) Then
                        SHTBACK(WR)=(DEP-XAVAMT+XAV)*XCPCLAD
                        If(SHTBACK(WR).LT.0.0) SHTBACK(WR)=0.0
                        If(XAV.GT.XAVAMT) SHTBACK(WR)=0.0
                     Endif
                  Endif
!
!  End-of-right loop variables are set for a PX record transient right.
!
                  If(XP(L).GT.0) Then
                     If(XP(L).EQ.2) Then
                        XPRET(L)=RET
                        XPSFD(L)=DEP
                        If(XPR(L).LE.1) RESDAT(RI,6)=XPRESDAT
                     Endif
                     DEP=0.0
                     RET=0.0
                  Endif
               Endif
!
!  For FY record firm yield computations, AMT and WSD are summed for the
!  the FY-record rights for this month. SHT is totalled and NSHT is counted
!  for the entire simulation.
!
               If(FYLEVEL.NE.0) Then
                  J=0
1290              J=J+1
                  If(FYWR(J).EQ.WR) Then
                     SUMAMT=SUMAMT+AMT
                     SUMWSD=SUMWSD+WSD(WR)
                     TOTSHT=TOTSHT+SHT
                     FYNCOUNT=FYNCOUNT+1
                  Endif
                  If(J.LT.FYN.and.WR.NE.FYWR(J)) Goto 1290
                  If(FYNCOUNT.EQ.FYN) Then
                     X=(SUMAMT*FYIN(1)-SUMWSD)/(SUMAMT*FYIN(1))
                     If(X.GT.0.00001) NSHT=NSHT+1
                     FYNCOUNT=0
                  Endif
               Endif
!
!   ++++++++++++++++++++  END OF WATER RIGHTS LOOP  ++++++++++++++++++++
!
1300        End Do
!____________________________________________________________________________
!
!  Flow adjustment activated by JO record RUF options to
!  determine regulated flows for a condensed dataset.
!
            If(RUF.GE.1) Then
               Do K=1,NCPTS
                  LOCNUM=K
                  Call AVALB
                  REGFLOW(K)=AVAMT+RUFA(K,I,MT)
                  If(RUF.EQ.1) Then
                     X=CPFLOW(K,MT,2)+RESREL(K)
                     If(ADJINC.EQ.4) X=X-CPFLOW(K,MT,1)
                     If(RUFA(K,I,MT).LE.0.0001*AVAMT.or.
     +                  RUFA(K,I,MT).LE.0.1) Then
                        REGFLOW(K)=X
                     Else
                        If(X.GT.REGFLOW(K)) REGFLOW(K)=X
                     Endif
                  Elseif(RUF.EQ.3) Then
                     REGFLOW(K)=REGFLOW(K)+RESREL(K)
                  Endif
                  If(REGFLOW(K).LT.0.0) REGFLOW(K)=0.0
               End Do
            Else
!
!  Set regulated flows as CPFLOW(K,MT,2) plus releases for upstream reservoirs.
!  Also, negative incremental flow adjustment is removed for regulated flows.
!
               Do K=1,NCPTS
                  REGFLOW(K)=CPFLOW(K,MT,2)+RESREL(K)
                  If(ADJINC.EQ.4) Then
                     REGFLOW(K)=REGFLOW(K)-CPFLOW(K,MT,1)
                     If(REGFLOW(K).LT.0.0) REGFLOW(K)=0.0
                  Endif
               End Do
            Endif
!
!  Determine failures to meet instream flow requirements and initiate a
!  second pass through the water rights loop if appropriate.
!
            If(IFPASS.LT.0) Goto 1430
            If(IFPASS.EQ.2) Goto 1390
            If(PASS2.EQ.2) Then
               If(IFPASS.LT.0) Write(14,*) ' WARNING: PASS2 on JO ',
     +             'record is 2, but there are no IF records.'
            Else
               IFCT=0
               Do 1340 K=1,NWRTS
                  WR=RANK(K)
                  If(WRIDS(WR,1).EQ.'IF#IF*IF') Then
                     IFFLAG=IFMETH(WR)
                     If(IFFLAG.LE.1.or.IFFLAG.EQ.3) Goto 1340
                     LOCNUM=WRNUM(WR,1)
                     If(WRIDS(WR,2).EQ.'IFRESREL') Then
                        IFSHORT=AMTIF(WR)-CPFLOW(LOCNUM,MT,2)
                     Else
                        IFSHORT=AMTIF(WR)-CPFLOW(LOCNUM,MT,2)
     +                               -RESREL(LOCNUM)
                     Endif
                     If(ADJINC.EQ.4) IFSHORT=IFSHORT+CPFLOW(LOCNUM,MT,1)
                     If(IFSHORT.LT.0.0) IFSHORT=0.0
                     If(IFSHORT.GT.0.0001) IFCT=IFCT+1
                  Endif
1340           End Do
               If(IFCT.EQ.0) Goto 1390
            Endif
!
!  Variables are reinitialized for second pass through water rights loop.
!
            IFPASS=-2
            RECD=RECD-NWRREC
            Do K=1,NCPTS
               CPSUM(K,1)=0.0
               CPSUM(K,2)=0.0
               CPSUM(K,3)=0.0
               CPSUM(K,4)=0.0
               CPFLOW(K,MT,2)=0.0
               RETSUM(K,MT)=0.0
               DEPSUM(K)=0.0
               DMRSUM(K)=0.0
            End Do
            Do K=1,NRES
               RESDAT(K,6)=RESDAT(K,5)
               RESDAT(K,7)=0.0
               RESDAT(K,8)=0.0
               RESDAT(K,9)=0.0
               RESDAT(K,10)=0.0
               RESDAT(K,11)=0.0
               RESDAT(K,12)=0.0
               RESDAT(K,13)=0.0
            End Do
            DSSWRJ=0
!
!  For FY record firm yield computations, NSHT and TOTSHT, adjustments
!  during first pass are removed in preparation for second pass.
!
            If(FYLEVEL.NE.0) Then
               If((SUMAMT*FYIN(1)-SUMWSD).GE.0.001) NSHT=NSHT-1
               If(SUMWSD.LT.SUMAMT) TOTSHT=TOTSHT+SUMWSD-SUMAMT
               SUMAMT=0.0
               SUMWSD=0.0
            Endif
!
!  Unappropriated flows (inflows) are adjusted in first month of year
!  by the return flows from the last month of previous year.
!              *+*+*+*+*  CALL SUBROUTINE ADJUST  *+*+*+*+*
!
            If(MT.EQ.1) Then
               DEP=0.0
               RFAC=1.0
               Do RETNUM=1,NCPTS
                  If(CPRET(RETNUM).GT.0.) Then
                     RET=CPRET(RETNUM)
                     RETSUM(RETNUM,MT)=RET
                     CPRET(RETNUM)=0.0
                     Call ADJUST
                  Endif
               End Do
            Endif
!
!  The system release HRR file is backed up to the first record for the
!  current month in order to overwrite data during the second pass.
!
            If((SIM2.EQ.9.and.DUALFLAG.EQ.0).or.SIM3.GE.9) Goto 1380
            If(F7.GT.0) Then
               Rewind(13)
               Do K=1,5
                  Read(13,1350) TEMPCHAR
1350              Format(A1)
               End Do
1360           Read(13,1370) YEAR1,MT1
1370           Format(24x,2I8)
               If(YEAR1.EQ.YEAR.and.MT1.EQ.MT) Then
                  Backspace(13)
               Else
                  Read(13,1350) TEMPCHAR
                  Goto 1360
               Endif
            Endif
!____________________________________________________________________________
!
!  A second pass through the water rights loop is initiated to reconsider
!  instream flow requirements.
!
1380        IFCM=IFCM+1
            Goto 860
!________________________________________________Add to IF Output Records
!
!  Write instream flow targets and shortages to IF records.
!
1390        If(NWRREC.EQ.0) Goto 1430
            If(CRWRITE.EQ.-99) Goto 1430
            If((SIM2.EQ.9.and.DUALFLAG.EQ.0).or.SIM3.GE.9) Goto 1430
            RECD=RECD-NWRREC
            DSSWRJ=0
            Do 1420 K=1,NWRTS
               WR=RANK(K)
               DSSWRJ=DSSWRJ+1
               If(NDT.GT.0) Then
                  If(DUALFLAG.EQ.0.and.DUAL(WR).EQ.2) Goto 1420
                  If(DUALFLAG.EQ.0.and.DUAL(WR).EQ.5) Goto 1420
                  If(DUALFLAG.GT.0.and.DUAL(WR).EQ.1) Goto 1420
                  If(DUALFLAG.GT.0.and.DUAL(WR).EQ.4) Goto 1420
               Endif
               If(WRNUM(WR,6).NE.1) Goto 1420
               If(IFMETH(WR).EQ.5) Goto 1420
               If(WRIDS(WR,1).EQ.'IF#IF*IF') Then
                  LOCNUM=WRNUM(WR,1)
                  If(WRIDS(WR,2).EQ.'IFRESREL') Then
                     IFSHORT=AMTIF(WR)-REGFLOW(LOCNUM)+RESREL(LOCNUM)
                  Else
                     IFSHORT=AMTIF(WR)-REGFLOW(LOCNUM)
                  Endif
                  If(IFSHORT.LT.0.0) IFSHORT=0.0
                  If(DSS(1).GT.0) Then
                     If(OUTFILE.GE.2) Then
                        Read (4,REC=RECD) J,I1,R1,R2,R3,R4,R5,R6,R7,A1,
     +                                    R8,I2FS
                        Write(4,REC=RECD) J,I1,R1,R2,R3,R4,R5,R6,R7,A1,
     +                                    AMTIF(WR),IFSHORT,R8,I2FS
                     Else
                        Read(4,1400,REC=RECD) CD,I1,R1,R2,R3,R4,R5,R6,
     +                                        R7,A1,R8,I2FS
                        Write(4,1410,REC=RECD) CD,I1,R1,R2,R3,R4,R5,R6,
     +                               R7,A1,AMTIF(WR),IFSHORT,R8,I2FS
                     Endif
1400                 Format(A2,2x,I2,7F11.2,A16,22x,F11.2,I4)
1410                 Format(A2,2x,I2,7F11.2,A16,3F11.2,I4)
                  Endif
                  If(DSS(2).EQ.1.or.DSS(3).EQ.1) Then
                     J=((I-1)*12)+MT
                     If(DSS(4).EQ.0) Then
                        DSSWR(DSSWRJ,J,1)=AMTIF(WR)
                        DSSWR(DSSWRJ,J,2)=IFSHORT
                     Elseif(DSS(4).EQ.1) Then
                        DSSWR(DSSWRJ,J,8)=AMTIF(WR)
                        DSSWR(DSSWRJ,J,9)=IFSHORT
                     Endif
                  Endif
               Endif
               RECD=RECD+1
1420        End Do
1430        Continue
!____________________________________________________________________________
!
!  Accumulate streamflow depletion ANNDIV(WR), reservoir withdrawal ANNRES(WR),
!  and diversion ANNWSD(WR) for right since the beginning of year or season.
!  SEASON=1 refers to a season with month LM(wr,1) less than month LM(wr,2).
!  SEASON=-1 refers to season with month LM(wr,1) greater than month LM(wr,2).
!
            Do 1450 K=1,NWRTS
               WR=RANK(K)
               L=LM(WR,1)
               M=LM(WR,2)
               MMT=0
               SEASON=1
               If(L.GT.M) SEASON=-1
               If(L.EQ.1.and.M.EQ.12) Then
                  If(MT.EQ.12) MMT=-9
               Else
                  If(L.LT.0) Then
                     If(M*1.EQ.MT.or.M*2.EQ.MT.or.M*3.EQ.MT.or.
     +               M*4.EQ.MT) MMT=-9
                     If(M*5.EQ.MT.or.M*6.EQ.MT.or.L*7.EQ.MT.or.
     +               M*8.EQ.MT) MMT=-9
                     If(M*9.EQ.MT.or.M*10.EQ.MT.or.M*11.EQ.MT) MMT=-9
                  Else
                     If(SEASON.EQ.1) Then
                        If(MT.LT.L.or.MT.GE.M) MMT=-9
                     Else
                        If(MT.LT.L.and.MT.GE.M) MMT=-9
                     Endif
                  Endif
               Endif
               If(SFD(WR).LT.0.0) SFD(WR)=0.0
               If(RESW(WR).LT.0.0) RESW(WR)=0.0
               If(WSD(WR).LT.0.0) WSD(WR)=0.0
               If(MMT.EQ.-9) Then
                  ANNDIV(WR)=0.0
                  ANNRES(WR)=0.0
                  ANNWSD(WR)=0.0
               Else
                  ANNDIV(WR)=ANNDIV(WR)+SFD(WR)
                  ANNRES(WR)=ANNRES(WR)+RESW(WR)
                  ANNWSD(WR)=ANNWSD(WR)+WSD(WR)
               Endif
               If(WRIDS(WR,1).EQ.'IF#IF*IF'.and.ADL(WR).GT.0.0) Then
                  If(MMT.EQ.-9) Then
                     ANNREG(WR)=0.0
                  Else
                     ANNREG(WR)=ANNREG(WR)+REGFLOW(WRNUM(WR,1))
                  Endif
               Endif
!
!  Set streamflow based TO record targets for next month.
!
               N=WRTO(WR)
               If(N.GT.0) Then
1440              If(TOCP(N).GT.0) Then
                     LOCNUM=TOCP(N)
                  Else
                     LOCNUM=WRNUM(WR,1)
                  Endif
                  PFLOW(N)=0.0
                  If(TOTARGET(N).EQ.-1) Then
                     PFLOW(N)=INFLOW(LOCNUM,MT)
                  Elseif(TOTARGET(N).EQ.-2) Then
                     PFLOW(N)=REGFLOW(LOCNUM)
                  Elseif(TOTARGET(N).EQ.-3) Then
                     Call AVALB
                     PFLOW(N)=AVAMT
                  Endif
                  If(Abs(TOFLAG(N)).GE.90) Then
                     N=N+1
                     Goto 1440
                  Endif
               Endif
1450        End Do
!
!  Flow summation for flow switch FS record routine.
!
            If(FSFLAG.GT.0) Then
               Do 1455 FS=1,FSFLAG
                  If(FSI(FS,1).EQ.1) Then
                     X=REGFLOW(FSI(FS,10))
                  Elseif(FSI(FS,1).EQ.2) Then
                     X=INFLOW(FSI(FS,10),MT)
                  Elseif(FSI(FS,1).EQ.3) Then
                     LOCNUM=FSI(FS,10)
                     Call AVALB
                     X=AVAMT
                  Elseif(FSI(FS,1).EQ.4) Then
                     X=DEPSUM(FSI(FS,10))
                  Elseif(FSI(FS,1).EQ.5) Then
                     X=CPSUM(FSI(FS,10),2)-CPSUM(FSI(FS,10),1)
                  Elseif(FSI(FS,1).EQ.6) Then
                     X=CPFLOW(FSI(FS,10),MT,2)+DEPSUM(FSI(FS,10))
                  Endif
!
!                 Flag of whether flow X falls within bounds.
!
                  JJ=0
                  If(X-FSX(FS,3).GT.-0.0001.and.
     +               FSX(FS,4)-X.GT.-0.0001) JJ=1
!
!                 Volume is accumulated.
!
                  If(FSI(FS,5).GT.0) Then
                     K=(I-1)*12+MT
                     If(K.LT.FSI(FS,5)) Then
                        M=K
                     Else
                        M=FSI(FS,5)
                     Endif
!
!                    Either flow for this month or zero is added to flow
!                    sequence as FSREG(FS,1). Count is updated as FSJ(FS,1).
!
                     Do J=1,M
                        TEMPREG(J)=FSREG(FS,J)
                        TEMPFSJ(J)=FSJ(FS,J)
                     Enddo
                     FSREG(FS,1)=0.0
                     FSJ(FS,1)=0
                     If(FSI(FS,6).LE.0) Then
                        FSREG(FS,1)=X
                        FSJ(FS,1)=JJ
                     Else
                        If(FSI(FS,7).GE.FSI(FS,6)) Then
                           If(MT.GE.FSI(FS,6).and.
     +                        MT.LE.FSI(FS,7)) Then
                              FSREG(FS,1)=X
                              FSJ(FS,1)=JJ
                           Endif
                        Else
                           If(MT.GE.FSI(FS,6).or.
     +                        MT.LE.FSI(FS,7)) Then
                              FSREG(FS,1)=X
                              FSJ(FS,1)=JJ
                           Endif
                        Endif
                     Endif
                     If(M.GE.2) Then
                        Do J=2,M
                           N=J-1
                           FSREG(FS,J)=TEMPREG(N)
                           FSJ(FS,J)=TEMPFSJ(N)
                        Enddo
                     Endif
!
!                    Summation of flows FSREGX(FS). Total count FSI(FS,11).
!
                     FSREGX(FS)=0.0
                     FSI(FS,11)=0
                     Do J=1,M
                        FSREGX(FS)=FSREGX(FS)+FSREG(FS,J)
                        FSI(FS,11)=FSI(FS,11)+FSJ(FS,J)
                     Enddo
                  Endif
1455           Enddo
            Endif
!
!  Determine channel losses associated with return flows.  These channel
!  losses are written to the ouput file but affect no other computations.
!
            If(CLFLAG.GT.0) Then
               Do L=1,NCPTS
                  If(RETSUM(L,MT).NE.0.0) Then
                     NUSCP=L
                     NPT=CPNXT(L)
                     CLX=1.0
1460                 If(CL(NUSCP).NE.0.0) Then
                        LOSS=RETSUM(L,MT)*CLX*CL(NUSCP)
                        CLOSS(NUSCP,2)=CLOSS(NUSCP,2)+LOSS
                        CLX=CLX*(1.0-CL(NUSCP))
                     Endif
                     If(NPT.GT.0) Then
                        NUSCP=NPT
                        NPT=CPNXT(NPT)
                        Goto 1460
                     Endif
                  Endif
               End Do
            Endif
!_________________________________________________Write CP Output Records
!
!  Control point data are written to the output file.
!                      *+*+*+*+*  Call Subroutine AVALB  *+*+*+*+*
!
            If(CRWRITE.EQ.-99) Goto 1640
            If((SIM2.EQ.9.and.DUALFLAG.EQ.0).or.SIM3.GE.9) Goto 1640
            DSSCPJ=0
            MCPO=0
            Do 1600 LOCNUM=1,NCPTS
               CPSUM(LOCNUM,3)=0.0
               CPSUM(LOCNUM,4)=0.0
               Do 1466 Z=1,NRES
                  If(RESNUM(Z,1).EQ.LOCNUM) Then
                     If(DUALFLAG.GT.0.and.DUAL1.GT.0) Then
                        Do J=1,NWRTS
                           WR=RANK(J)
                           If(WRNUM(WR,9).EQ.Z.and.
     +                        DUAL(WR).NE.1.and.DUAL(WR).NE.4) Goto 1464
                        End Do
                        Goto 1466
                     Endif
1464                 CPSUM(LOCNUM,3)=CPSUM(LOCNUM,3)+RESDAT(Z,7)
                     CPSUM(LOCNUM,4)=CPSUM(LOCNUM,4)+RESDAT(Z,6)
                  Endif
1466           End Do
               MCP=0
               If(CPOUT.EQ.-1.or.(CPOUT.EQ.-2.and.INMETHOD(LOCNUM).LE.1)
     +            .or.(CPOUT.GT.0.and.LOCNUM.LE.CPOUT)) Then
                  MCP=MCP+1
               Elseif(NCPOUT.GT.0) Then
                  Do CPO=1,NCPOUT
                     If(CPOUID(CPO).EQ.CPID(LOCNUM,1)) MCP=MCP+1
                  End Do
               Endif
               If(MCP.GE.1) Then
                  AVFLAG=99
                  Call AVALB
               Endif
!
!  CP record INMETHOD option 9.
!
               If(INMETHOD(LOCNUM).EQ.9.and.MCP.GE.1.and.DSS(1).GT.0)
     +            Then
                  INFLOW(LOCNUM,MT)=-9.0
                  REGFLOW(LOCNUM)=-9.0
               Endif
!
!  Inflows are adjusted by RUFA from RUF file if specified by JO record RUFIN.
!
               If(RUFIN.EQ.2) Then
                  INFLOW(LOCNUM,MT)=INFLOW(LOCNUM,MT)+RUFA(LOCNUM,I,MT)
               Endif
!
!  Warning messages if format limits are exceeded.
!
               If(MCP.GE.1.and.DSS(1).GT.0.and.OUTFILE.LE.1) Then
1470              Format(' WARNING: A -1.0 in output file indicates',
     +                   ' format limit exceeded. Year:',I5,'  CP: ',A6)
                  If(CPSUM(LOCNUM,1).GT.9999999.0) Then
                     Write(14,1470) YEAR,CPID(LOCNUM,1)
                     Write(14,1480) CPSUM(LOCNUM,1)
1480                 Format(10x,'diversion shortage =',F14.1)
                     CPSUM(LOCNUM,1)=-1.0
                  Endif
                  If(CPSUM(LOCNUM,2).GT.9999999.0) Then
                     Write(14,1470) YEAR,CPID(LOCNUM,1)
                     Write(14,1490) CPSUM(LOCNUM,2)
1490                 Format(10x,'diversion target =',F14.1)
                     CPSUM(LOCNUM,2)=-1.0
                  Endif
                  If(CPSUM(LOCNUM,4).GT.99999999.0) Then
                     Write(14,1470) YEAR,CPID(LOCNUM,1)
                     Write(14,1500) CPSUM(LOCNUM,4)
1500                 Format(10x,'reservoir storage =',F14.1)
                     CPSUM(LOCNUM,4)=-1.0
                  Endif
                  If(DEPSUM(LOCNUM).GT.99999999.0) Then
                     Write(14,1470) YEAR,CPID(LOCNUM,1)
                     Write(14,1510) DEPSUM(LOCNUM)
1510                 Format(10x,'streamflow depletion =',F14.1)
                     DEPSUM(LOCNUM)=-1.0
                  Endif
                  If(AVAMT.GT.99999999.0) Then
                     Write(14,1470) YEAR,CPID(LOCNUM,1)
                     Write(14,1520) AVAMT
1520                 Format(10x,'unappropriated flow =',F14.1)
                     AVAMT=-1.0
                  Endif
                  If(RETSUM(LOCNUM,MT).GT.99999999.0) Then
                     Write(14,1470) YEAR,CPID(LOCNUM,1)
                     Write(14,1530) RETSUM(LOCNUM,MT)
1530                 Format(10x,'return flows =',F14.1)
                     RETSUM(LOCNUM,MT)=-1.0
                  Endif
                  If(INFLOW(LOCNUM,MT).GT.99999999.0) Then
                     Write(14,1470) YEAR,CPID(LOCNUM,1)
                     Write(14,1540) INFLOW(LOCNUM,MT)
1540                 Format(10x,'naturalized flow =',F14.1)
                     INFLOW(LOCNUM,MT)=-1.0
                  Endif
                  If(REGFLOW(LOCNUM).GT.99999999.0) Then
                     Write(14,1470) YEAR,CPID(LOCNUM,1)
                     Write(14,1550) REGFLOW(LOCNUM)
1550                 Format(10x,'regulated flow =',F14.1)
                     REGFLOW(LOCNUM)=-1.0
                  Endif
                  If(CLOSS(LOCNUM,1).GT.99999999.0) Then
                     Write(14,1470) YEAR,CPID(LOCNUM,1)
                     Write(14,1560) CLOSS(LOCNUM,1)
1560                 Format(10x,'channel loss credits =',F12.1)
                     CLOSS(LOCNUM,1)=-1.0
                  Endif
                  If(CLOSS(LOCNUM,2).GT.9999999.0) Then
                     Write(14,1470) YEAR,CPID(LOCNUM,1)
                     Write(14,1570) CLOSS(LOCNUM,1)
1570                 Format(10x,'channel losses =',F12.1)
                     CLOSS(LOCNUM,2)=-1.0
                  Endif
                  If(RESREL(LOCNUM).GT.9999999.0) RESREL(LOCNUM)=-1.0
!
!  Control point output records are written to the OUT output file.
!
                  Write(4,1580,REC=RECD) CPID(LOCNUM,1),
     +                     (CPSUM(LOCNUM,Z),Z=1,4),DEPSUM(LOCNUM),AVAMT,
     +              RETSUM(LOCNUM,MT),INFLOW(LOCNUM,MT),REGFLOW(LOCNUM),
     +                   CLOSS(LOCNUM,1),CLOSS(LOCNUM,2),RESREL(LOCNUM)
1580              Format(A6,2F11.3,8F11.2,2F10.2)
               Endif
               If(MCP.GE.1.and.DSS(1).GT.0.and.OUTFILE.GE.2) Then
                  Write(4,REC=RECD) CPID(LOCNUM,1),
     +                     (CPSUM(LOCNUM,Z),Z=1,4),DEPSUM(LOCNUM),AVAMT,
     +              RETSUM(LOCNUM,MT),INFLOW(LOCNUM,MT),REGFLOW(LOCNUM),
     +                    CLOSS(LOCNUM,1),CLOSS(LOCNUM,2),RESREL(LOCNUM)
               Endif
!
!  Arrays for the SOU and DSS output files.
!
               If(MCP.GE.1.and.(DSS(2).EQ.1.or.DSS(3).EQ.1)) Then
                  DSSCPJ=DSSCPJ+1
                  DSSCPI(DSSCPJ)=LOCNUM
                  J=((I-1)*12)+MT
                  DSSCP(DSSCPJ,J,1)=INFLOW(LOCNUM,MT)
                  DSSCP(DSSCPJ,J,2)=REGFLOW(LOCNUM)
                  DSSCP(DSSCPJ,J,3)=AVAMT
                  If(DSS(4).EQ.0) Then
                     DSSCP(DSSCPJ,J,4)=CPSUM(LOCNUM,4)
                     DSSCP(DSSCPJ,J,5)=CPSUM(LOCNUM,2)
                     DSSCP(DSSCPJ,J,6)=CPSUM(LOCNUM,1)
                  Elseif(DSS(4).EQ.1) Then
                     DSSCP(DSSCPJ,J,4)=CLOSS(LOCNUM,1)
                     DSSCP(DSSCPJ,J,5)=CLOSS(LOCNUM,2)
                     DSSCP(DSSCPJ,J,6)=RETSUM(LOCNUM,MT)
                     DSSCP(DSSCPJ,J,7)=RESREL(LOCNUM)
                     DSSCP(DSSCPJ,J,8)=CPSUM(LOCNUM,4)
                     DSSCP(DSSCPJ,J,9)=CPSUM(LOCNUM,3)
                     DSSCP(DSSCPJ,J,10)=DEPSUM(LOCNUM)
                     DSSCP(DSSCPJ,J,11)=CPSUM(LOCNUM,2)
                     DSSCP(DSSCPJ,J,12)=CPSUM(LOCNUM,1)
                  Endif
               Endif
               If(MCP.GE.1) Then
                  RECD=RECD+1
                  MCPO=MCPO+1
               Endif
1600        End Do
            If(MCPO.NE.NCPO) Then
               Write(14,*) ' '
               Write(14,1610) MCPO,NCPO
1610           Format(' ERROR: CP output written for ',I4,
     +                ' control points, but expecting ',I4)
               Write(14,1620)
1620           Format(7x,' Stopped in main program due to error.')
               Write(14,*) ' '
               Call ERROR
            Endif
!__________________________________________Write Reservoir Output Records
!
!  Reservoir data are written to the output file.
!
            DSSREJ=0
            Do K=1,NRES
               If(RESNUM(K,3).EQ.1) Then
                  WSE=0.0
                  If(RESNUM(K,4).GT.0) Then
                     Call Linear(RESDAT(K,6),WSE,RESNUM(K,4),PVCURV)
                  Endif
                  If(DSS(1).GT.0) Then
                     If(OUTFILE.GE.2) Then
                        Write(4,REC=RECD) RESID(K),(RESDAT(K,13)-
     +                   RESDAT(K,12)),RESDAT(K,12),RESDAT(K,7),
     +                   RESDAT(K,6),RESDAT(K,8),RESDAT(K,11),
     +                   RESDAT(K,9),RESDAT(K,10),EVAPR(RESNUM(K,1),MT),
     +                   EV(K,MT),WSE,RESDAT(K,10),RESDAT(K,15)
                     Else
                        Write(4,1630,REC=RECD) RESID(K),(RESDAT(K,13)-
     +                   RESDAT(K,12)),RESDAT(K,12),RESDAT(K,7),
     +                   RESDAT(K,6),RESDAT(K,8),RESDAT(K,11),
     +                   RESDAT(K,9),RESDAT(K,10),EVAPR(RESNUM(K,1),MT),
     +                   EV(K,MT),WSE,RESDAT(K,15)
1630                    Format(A6,8F11.2,2F11.4,F11.3,F9.1)
                     Endif
                  Endif
!
!  Arrays for the SOU and DSS output files.
!
                  If(DSS(2).EQ.1.or.DSS(3).EQ.1) Then
                     DSSREJ=DSSREJ+1
                     DSSREI(DSSREJ)=K
                     J=((I-1)*12)+MT
                     DSSRE(DSSREJ,J,1)=RESDAT(K,6)
                     DSSRE(DSSREJ,J,2)=RESDAT(K,7)
                     If(DSS(4).EQ.0) Then
                        DSSRE(DSSREJ,J,3)=EVAPR(RESNUM(K,1),MT)
                        DSSRE(DSSREJ,J,4)=WSE
                     Elseif(DSS(4).EQ.1) Then
                        DSSRE(DSSREJ,J,3)=RESDAT(K,13)-RESDAT(K,12)
                        DSSRE(DSSREJ,J,4)=RESDAT(K,12)
                        DSSRE(DSSREJ,J,5)=RESDAT(K,8)
                        DSSRE(DSSREJ,J,6)=RESDAT(K,11)
                        DSSRE(DSSREJ,J,7)=RESDAT(K,9)
                        DSSRE(DSSREJ,J,8)=RESDAT(K,10)
                        DSSRE(DSSREJ,J,9)=EVAPR(RESNUM(K,1),MT)
                        DSSRE(DSSREJ,J,10)=EV(K,MT)
                        DSSRE(DSSREJ,J,11)=WSE
                        DSSRE(DSSREJ,J,12)=RESDAT(K,15)
                     Endif
                  Endif
                  RECD=RECD+1
               Endif
            End Do
!___________________________________________________________________________
!
!  Reservoir arrays are reinitialized.
!
1640        Do K=1,NRES
               RESDAT(K,5)=RESDAT(K,6)
               RESDAT(K,8)=0.0
               RESDAT(K,9)=0.0
               RESDAT(K,10)=0.0
               RESDAT(K,11)=0.0
               RESDAT(K,12)=0.0
               RESDAT(K,13)=0.0
            End Do
!
!  Control point arrays are reset to zero.
!
            DEPSUM=0.0
            DMRSUM=0.0
            CPSUM=0.0
!___________________________________________________________________________
!
!  Drought index multiplier factor DIFACT(DI) is determined.
!         *+*+*+*+*+*  Call Subroutine DROUGHT  *+*+*+*+*+*
!
            If(DIFLAG.GE.1.and.STODI.EQ.0) Call DROUGHT
!
!   +++++++++++++++++++  END OF MONTHLY (PERIOD) LOOP  ++++++++++++++++++
!
1650     End Do
!___________________________________________________________________________
!
!  Control point arrays are reset to zero.
!
         CPFLOW=0.0
         RETSUM=0.0
!
!   ++++++++++++++++++++++++  END OF ANNUAL LOOP  +++++++++++++++++++++++
!
1700  End Do
!____________________________________________________________________________
!
!  Conditional reliability simulations are activated by the CR record.
!
!  Number of loops NLOOP for monthly cycle option is determined
!  based on the number of monthly sequences NSEQ.
!
      If(CR1.GT.0.and.CR2.LE.0) Then
         If(NSEQ.LT.CR1) Then
            NLOOP=NSEQ
         Else
            NLOOP=CR1
         Endif
      Endif
!
!  Number of loops NLOOP for annual cycle option is determined
!  based on the number of annual sequences NSEQ.
!
      If(CR1.GT.0.and.CR2.GT.0) Then
         If(CR1.LE.12) Then
            NLOOP = Ceiling(NSEQ*1.0/NYRS)
         Else
            NLOOP=Ceiling(NSEQ*1.0/Int(NYRS/Ceiling((CR1+CR2-2)/12.)))
         Endif
      Endif
!
!  Files are returned to their beginning and the next CRM loop
!  is initiated by returning to statement 520.
!
      If(CRLOOP.LT.NLOOP) Then
         If(F1.GE.0) Rewind(1)
         If(F2.GE.0) Rewind(2)
         If(F4.GE.1) Rewind(12)
         If(F5.GE.1) Rewind(21)
         If(F7.GE.1) Rewind(13)
         If(INEV.EQ.3.or.INEV.EQ.4) Then
            Rewind(3)
1710        Read(3,1715) CD
1715        Format(A2)
            If(CD.NE.'ED') Goto 1710
         Endif
         CRMN=CRM+CR1*Int((12*NYRS-CRLOOP)/CR1)+CRLOOP
         CRSKIP=CRLOOP
         CRLOOP=CRLOOP+1
         Goto 520
      Endif
      If(CR1.GT.0.and.CR2.EQ.0.and.ICHECK.EQ.10) Then
         Write(14,1720) CRM,CRWRITEN
1720     Format(/,'Of the total of 12(years)(cycles)=',I5,' months, ',
     +            'the',I3,' months flagged',/,'with CRWRITE of -99 ',
     +            'were excluded from the simulation results OUT file.')
      Endif
!
!  If ICHECK=8, dual simulation rights are listed and streamflow depletions
!  are written to the message file after the initial simulation (DUALFLAG=0).
!
      If(ICHECK.EQ.8.and.DUALFLAG.EQ.0) Then
         If(NDT.EQ.0) Then
            Write(14,1730)
1730        Format(' WARNING: JD field 4 ICHECK is 8 but there are no',
     +             ' dual simulation rights.')
         Else
            Write(14,1740)
1740        Format(' *** Rights with DUAL greater than zero are',
     +             ' listed as follows:')
            Do K=1,NWRTS
               WR=RANK(K)
               If(DUAL(WR).GE.1) Then
                  J=WRNUM(WR,9)
                  If(J.GT.0)Then
                     Write(14,1750) Adjustl(WRID(WR)),DUAL(WR),
     +               Adjustl(CPID(WRNUM(WR,1),1)),Adjustl(RESID(J))
1750                 Format(10x,A16,2x,'DUAL(wr)=',I2,4x,A6,4x,A6)
                  Elseif(J.EQ.0)Then
                     Write(14,1750) Adjustl(WRID(WR)),DUAL(WR),
     +               Adjustl(CPID(WRNUM(WR,1),1))
                  Elseif(J.LT.0) Then
                     Write(14,1760) Adjustl(WRID(WR)),DUAL(WR),
     +               Adjustl(CPID(WRNUM(WR,1),1)),Abs(J)
1760                 Format(10x,A16,2x,'DUAL(wr)=',I2,4x,A6,4x,
     +                      'system water right',I3)
                  Endif
               Endif
            End Do
         Endif
         If(NDD.GT.0) Then
            Write(14,1770)
1770        Format(' *** Streamflow depletions from the initial',
     +             ' simulation for DUAL of 3 or 4 are as follows.')
            Do K=1,NWRTS
               WR=RANK(K)
               If(DUAL(WR).EQ.3.or.DUAL(WR).EQ.4) Then
                  Write(14,1780) Adjustl(WRID(WR))
1780              Format('Water Right: ',A16)
                  Do I=1,NYRS
                     YEAR=YRST+I-1
                     If(CR1.GT.0.and.CR2.EQ.0) Then
                        Do J=1,CR1
                           Write(14,1790) YEAR,
     +                          (CRDDEP(DD(WR),I,MT,J),MT=1,NPRDS)
                        End Do
                     Else
                        Write(14,1790) YEAR,
     +                       (DDEP(DD(WR),I,MT),MT=1,NPRDS)
1790                    Format(I4,12F8.1)
                     Endif
                  End Do
               Endif
            End Do
         Endif
      Endif
!____________________________________________________________________________
!
!  Beginning/ending storage BES options specified on the JO record control
!  the writing of storages to the BES file and/or repeats the simulation
!  with beginning storages set equal to ending storages.
!
      If(BES.GE.1.and.BES.LE.6) Then
         If(BES.EQ.3.or.BES.EQ.4) Rewind(15)
         If(BES.EQ.1.or.BES.EQ.3.or.BES.EQ.4.or.BES.EQ.6) Then
            Write(15,1800)
            Write(15,1810)
            Write(15,1820)
1800        Format('      Beginning-Ending Storage (BES) File',/)
1810        Format('                 Storage   Beginning      Ending')
1820        Format('Reservoir ID    Capacity    Storage      Storage',/)
            X1=0.0
            X2=0.0
            X3=0.0
            Do K=1,NRES
               Write(15,1830) K,RESID(K),RESDAT(K,1),RESDAT(K,14),
     +                        RESDAT(K,6)
1830           Format(I4,2x,A6,3F12.2)
               X1=X1+RESDAT(K,1)
               X2=X2+RESDAT(K,14)
               X3=X3+RESDAT(K,6)
               If(BES.EQ.6) Then
                  Do I=1,NWRTS
                     If(WRNUM(I,9).EQ.K) Then
                        Write(15,1840) Adjustl(WRID(I)),
     +                 Adjustl(CPID(WRNUM(I,1),1)),WRDAT(I,4),WRDAT(I,3)
1840                    Format(13x,'Water Right ',A16,' ;Control Point '
     +                       ,A6,' ;Capacity:',F8.0,' ;Inactive:',F7.0)
                     Endif
                     If(WRNUM(I,9).LT.0) Then
                        SWR=-WRNUM(I,9)
                        Do J=1,NSR(SWR)
                           If(SN1(SWR,J).EQ.K) Then
                              Write(15,1850) SWR,Adjustl(WRID(I)),
     +                                       (SYSTEM(SWR,J,L),L=1,8)
1850                          Format(13x,'System Water Right',I3,5x,A16,
     +                        /,18x,'WRSYS(SR,1-8) = ',3F8.0,4F8.2,F8.1)
                           Endif
                        End Do
                     Endif
                  End Do
               Endif
            End Do
            Write(15,1860) X1,X2,X3
1860        Format(/,'    Totals',2x,3F12.1,/)
         Endif
         If(BES.EQ.3.or.BES.EQ.4) Rewind(15)
         If((BES.EQ.4.or.BES.EQ.5).and.BESFLAG.EQ.-9) Then
            BESFLAG=0
            DUALFLAG=99
         Endif
      Endif
!____________________________________________________________________________
!
!  The simulation is repeated a second time if either: (1) BES options 4, 5
!  or 6 are specified by the JO record or (2) the dual simulation option is
!  specified by at least one right with DUAL(wr)=3 or 5 on its PX/SO record.
!
      If(DUALFLAG.EQ.99.or.(NDS.GT.0.and.DUALFLAG.EQ.0)) Then
         If(F1.LT.0.and.F4.LE.0) Then
            Rewind(3)
1880        Read(3,1890,END=1900) CD
1890        Format(A2)
            If(CD.EQ.'ED') Goto 1920
            Goto 1880
1900        Write(14,*) ' '
            Write(14,1910)
1910        Format(' ERROR: Reached end of file without finding ED',
     +             ' record.')
            Write(14,1620)
            Write(14,*) ' '
            Call ERROR
         Endif
1920     If(F1.GE.0) Rewind(1)
         If(F2.GE.0) Rewind(2)
         If(F4.GE.1) Rewind(12)
         If(F5.GE.1) Rewind(21)
         If(F9.GE.1) Rewind(15)
         Write(14,1930)
1930     Format(' *** Initial simulation was completed.')
         If(DUALFLAG.EQ.99) Write(14,1940) BES
1940     Format(' *** The beginning/ending storage (BES) option was',
     +          ' activated by BES of',I2,' on the JO record.'/,5x,
     +          'The simulation is being repeated with initial ',
     +          'storages set equal to ending storages.')
         If(NDS.GT.0) Write(14,1950)
1950     Format(' *** The dual simulation option is activated by',
     +          ' DUAL of 3 or 5',/,5x,'on the PX or SO record of',
     +          ' one or more rights or by JO field 11.')
         DUALFLAG=999
         Write(*,*) ' '
         Write(*,*) '   Initial simulation is complete.'
         Write(*,*) '   Second simulation is beginning.'
         Write(*,*) ' '
         Goto 510
      Endif
      If(DUALFLAG.GT.0.and.NDS.GT.0.and.DSS(1).GT.0) Then
         If(CR1.GT.0) NWRREC=CRNWR
         If(OUTFILE.GE.2) Then
            Write(4,REC=5) YRST,NYRS,NCPO,NWRREC,NREOUT,CR1,CR2,CR3,CR4
         Else
            Write(4,1960,REC=5) YRST,NYRS,NCPO,NWRREC,NREOUT,CR1,CR2,
     +                          CR3,CR4
1960        Format(8I6,F8.3)
         Endif
      Endif
!____________________________________________________________________________
!
!  Firm yield routine that iteratively reruns the simulation with different
!  target amounts for the water right or water right group specified on the
!  FY record.
!
      If(FYLEVEL.NE.0.and.SIM3.NE.-999) Then
         IFY=IFY+1
!
!     Output table heading written to YRO file.
!
         If(FYLEVEL.EQ.-99) Then
            Write(11,2020)
2020        Format(10x,'Yield Versus Reliability Table for the ',
     +             'Following Water Right(s):'/)
            If(FYN.EQ.1) Then
               WR=FYWR(1)
               Write(11,2030) Adjustl(WRID(WR)),
     +                        Adjustl(WRIDS(WR,1)),Adjustl(WRIDS(WR,2))
2030           Format(14x,'One right (100%): ',A16,2x,A8,2x,A8)
            Else
               J=0
2040           J=J+1
               WR=FYWR(J)
               If(MFY.EQ.2) Then
                  Write(11,2050) WRNUM(WR,7),Adjustl(WRID(WR)),
     +                        Adjustl(WRIDS(WR,1)),Adjustl(WRIDS(WR,2))
2050              Format(11x,I8,' priority: ',A16,2x,A8,2x,A8)
               Else
                  PERCENT=FYFACT(J)*100.0
                  Write(11,2060) PERCENT,Adjustl(WRID(WR)),
     +                        Adjustl(WRIDS(WR,1)),Adjustl(WRIDS(WR,2))
2060              Format(11x,F8.3,' percent: ',A16,2x,A8,2x,A8)
               Endif
               If(J.LT.FYN) Goto 2040
            Endif
            J=NYRS*12
            PERCENT=FYIN(1)*100
            If(MFY.EQ.2) Then
               Write(11,2070)
2070           Format(/,4x,'If more than one right, the target amount ',
     +                     'is distributed using the priorities')
            Else
               Write(11,2080)
2080           Format(/,4x,'If more than one right, the target amount ',
     +                     'is distributed using the percentages')
            Endif
            Write(11,2090) J,PERCENT
2090        Format(4x,'shown above. The total number of periods is',I4,
     +       '. The period reliability is the',/,4x,'percentage of the',
     +       ' periods for which at least',F6.1,' percent (FY record ',
     +       'field 2;',/,4x,'default=100%) of the target is supplied.',
     +       ' The table below ends with the maximum',/,4x,'target',
     +       ' that results in a mean annual shortage of less than',
     +       ' 0.05 units.',/)
            Write(11,2100)
2100        Format(86('-'))
            Write(11,2110)
            Write(11,2120)
2110        Format(21x,'Annual',7x,'Mean',8x,'Mean',6x,'Volume',4x,
     +                 'Periods',4x,'Period')
2120        Format('Iteration Level',6x,'Target',5x,'Shortage',5x,
     +             'Actual',3x,'Reliability',1x,'Without',2x,
     +             'Reliability',/,58x,'(%)',5x,'Shortage',5x,'(%)')
            Write(11,2100)
         Endif
!
!     Results for one simulation written as one line in output table.
!
         AS=TOTSHT/NYRS
         AA=FYAMT-AS
         VR=(AA/FYAMT)*100.0
         K=12*NYRS-NSHT
         PR=((REAL(K)/REAL(12*NYRS)))*100.0
         If(FYLEVEL.EQ.-99) FYLEVEL=0
         If(AS.LE.9.99) Then
            If(AS.LT.0.0) AS=0.0
            Write(11,2130) IFY,FYLEVEL,FYAMT,AS,AA,VR,K,PR
2130        Format(I5,I8,F14.1,F12.2,F12.1,F11.2,I9,F12.2)
         Else
            Write(11,2140) IFY,FYLEVEL,FYAMT,AS,AA,VR,K,PR
2140        Format(I5,I8,F14.1,F12.1,F12.1,F11.2,I9,F12.2)
         Endif
         If(FYLEVEL.EQ.0) FYLEVEL=-99
!
!     The simulation is iteratively repeated as specified by
!     the FY record until the firm yield is found.
!
         If(FYLEVEL.EQ.-99) Then
            If(AS.LT.0.05) Then
               Write(11,2150)
2150           Format(/,'Since the initial target from the FY record ',
     +         'is fully met with no shortage,',/,'no further ',
     +         'computations are performed.',/)
                Goto 2500
            Else
               FYLEVEL=1
            Endif
         Endif
         If(FYLEVEL.EQ.1) Then
            If(AS.LT.0.05) Then
               If(FYIN(4).EQ.0.0.and.FYIN(5).EQ.0.0) Goto 2500
               FYAMT=FYAMT+FYIN(3)
               Write(11,*) '          ---'
               If(FYIN(4).NE.0.0) Then
                  FYLEVEL=-2
               Else
                  FYLEVEL=-3
               Endif
            Else
               FYAMT=FYAMT-FYIN(3)
               If(FYAMT.LE.0.0) Then
                  If(FYIN(4).EQ.0.0.and.FYIN(5).EQ.0.0) Goto 2500
                  FYAMT=FYAMT+FYIN(3)
                  Write(11,*) '          ---'
                  If(FYIN(4).NE.0.0) Then
                     FYLEVEL=-2
                  Else
                     FYLEVEL=-3
                  Endif
               Endif
            Endif
         Endif
         If(Abs(FYLEVEL).EQ.2.and.FYIN(4).GT.0.0) Then
            If(FYLEVEL.EQ.2.and.AS.LT.0.05) Then
               If(FYIN(5).EQ.0.0) Goto 2500
               FYAMT=FYAMT+FYIN(4)
               FYLEVEL=-3
               Write(11,*) '          ---'
            Else
               FYAMT=FYAMT-FYIN(4)
               FYLEVEL=2
               If(FYAMT.LE.0.0) Then
                  FYAMT=FYAMT+FYIN(4)
                  FYLEVEL=-3
                  Write(11,*) '          ---'
                  If(FYIN(5).EQ.0.0) Then
                     Write(11,2160)
2160                 Format('Routine finished without ',
     +                      'reaching a firm yield.')
                     Goto 2500
                  Endif
               Endif
            Endif
         Endif
         If(Abs(FYLEVEL).EQ.3.and.FYIN(5).GT.0.0) Then
            If(FYLEVEL.EQ.3.and.AS.LT.0.05) Then
               FYAMT=FYAMT+FYIN(5)
               FYLEVEL=-4
               Write(11,*) '          ---'
            Else
               FYAMT=FYAMT-FYIN(5)
               FYLEVEL=3
               If(FYAMT.LE.0.0) Then
                  FYAMT=FYAMT+FYIN(5)
                  FYLEVEL=-4
                  Write(11,*) '          ---'
               Endif
            Endif
         Endif
         If(Abs(FYLEVEL).EQ.4) Then
            If(FYLEVEL.EQ.4.and.AS.LT.0.05) Then
               Goto 2500
            Else
               FYAMT=FYAMT-0.1*FYIN(5)
               FYLEVEL=4
               If(FYAMT.LE.0.0) Then
                  Write(11,2160)
                  Goto 2500
               Endif
            Endif
         Endif
         If(IFY.GE.200) Then
            Write(11,2170) IFY
2170        Format(/,'Firm yield search stopped after reaching ',
     +               'maximum of',I4,' iterations.',/)
            Goto 2500
         Endif
!
!     The simulation is repeated by returning to statement 500. Files are
!     reinitialized prior to restarting the simulation.
!
         If(F1.LT.0.and.F4.LE.0) Then
            Rewind(3)
2180        Read(3,2190,END=2200) CD
2190        Format(A2)
            If(CD.EQ.'ED') Goto 2220
            Goto 2180
2200        Write(14,*) ' '
            Write(14,2210)
2210        Format(' ERROR: Reached end of file without finding ED',
     +             ' record.')
            Write(14,1620)
            Write(14,*) ' '
            Call ERROR
         Endif
2220     If(F1.GE.0) Rewind(1)
         If(F2.GE.0) Rewind(2)
         If(F4.GE.1) Rewind(12)
         If(F5.GE.1) Rewind(21)
         If(F7.GE.1) Rewind(13)
         If(F9.GE.1) Rewind(15)
         J=IFY+1
         Print*,' '
         Write(*,2230) J
2230     Format('    Beginning firm yield iteration',I3)
         Print*,' '
         Write(14,2240) J
2240     Format(' *** Beginning firm yield iteration',I3)
         Goto 500
!
!  End of iterative firm yield loop with return to statement 500.
!
2500     Write(11,2100)
         If(SIM3.EQ.9) Then
            If(OUTFILE.LE.1) Write(4,2510,REC=1)
2510        Format(/,' The simulation output file root.OUT is not used',
     +               ' with a FY record yield-reliability analysis',/,
     +               ' unless activated by FY field 10.')
!
!     A final simulation to build the OUT file is performed if specified by
!     SIM3 from FY record field 10. The simulation is repeated by returning
!     to statement 500. Files are reinitialized before returning.
!
         Elseif(SIM3.EQ.99) Then
            SIM3=-999
            FYLEVEL=5
            If(F1.LT.0.and.F4.LE.0) Then
               Rewind(3)
2520           Read(3,2190,END=2530) CD
               If(CD.EQ.'ED') Goto 2540
               Goto 2520
2530           Write(14,*) ' '
               Write(14,2210)
               Write(14,1620)
               Write(14,*) ' '
               Call ERROR
            Endif
2540        If(F1.GE.0) Rewind(1)
            If(F2.GE.0) Rewind(2)
            If(F4.GE.1) Rewind(12)
            If(F5.GE.1) Rewind(21)
            If(F7.GE.1) Rewind(13)
            If(F9.GE.1) Rewind(15)
            Print*,' '
            Write(*,2550)
2550        Format('    Final simulation to create OUT file')
            Print*,' '
            Goto 500
         Endif
      Endif
!____________________________________________________________________________
!
!  Simulation results are written to SOU and DSS files.
!
      If(DSS(2).EQ.1.or.DSS(3).EQ.1) Then
         Deallocate(CPFLOW,INFLOW,EVAPR,RETSUM,CPSUM)
         If(DSS(2).EQ.1) Then
            Write(*,2560)
2560        Format('    Writing simulation results in SOU file.')
            Call SIMOUT
         Endif
         If(DSS(3).EQ.1) Then
            Write(*,2570)
2570        Format('    Writing simulation results in DSS file.')
*            Call DSSOUT
         Endif
      Endif
!
!  Beginning reservoir storage (BRS) file is written.
!
      If(BRS.EQ.1) Then
         Write(16,*) '    Beginning Reservoir Storage (BRS) File'
         Write(16,*)
         Write(16,*) '                Control   Storage  Beginning'
         Write(16,*) '   Reservoir     Point   Capacity   Storage '
         Write(16,*)
         Do K=1,NRES
            Write(16,2580) K,RESID(K),RESNUM(K,1),CPID(RESNUM(K,1),1),
     +                     RESDAT(K,1),RESDAT(K,14)*CR4
2580        Format(I5,1x,A6,I5,1x,A6,2F10.1)
         Enddo
      Endif
!
!  An ending message is written in the ZZZ file.
!
      If(ZZ.GE.1) Then
         Write(17,2590)
2590     Format('End of ZZZ File')
      Endif
!____________________________________________________________________________
!
!  File names are written to the monitor at the end of the simulation.
!
      Print*,' '
      P1=Index(ROOT,'   ')-1
      NAME=ROOT(:P1)//'.dat'
      Write(*,3010) NAME
3010  Format('    Input File:   ',A120)
      NAME=ROOT(:P1)//'.MSS'
      Write(*,3020) NAME
3020  Format('    Message File: ',A120)
!
3030  Format('    Output File:  ',A120)
      If(CR1.GT.0) Then
         NAME=ROOT(:P1)//'.CRM'
         Write(*,3030) NAME
      Endif
      If(DSS(1).GT.0) Then
         NAME=ROOT(:P1)//'.OUT'
         Write(*,3030) NAME
      Endif
      If(DSS(2).EQ.1) Then
         NAME=ROOT(:P1)//'.SOU'
         Write(*,3030) NAME
      Endif
      If(DSS(3).EQ.1) Then
         NAME=ROOT(:P1)//'.DSS'
         Write(*,3030) NAME
      Endif
!
      If(F7.GT.0) Then
         NAME=ROOT(:P1)//'.HRR'
         Write(*,3040) NAME
3040     Format('    Release File: ',A120)
      Endif
      If(FYLEVEL.NE.0) Then
         NAME=ROOT(:P1)//'.YRO'
         Write(*,3050) NAME
3050     Format('    Yld-Rel File: ',A120)
      Endif
      If(BES.NE.0.and.BES.NE.5) Then
         NAME=ROOT(:P1)//'.BES'
         Write(*,3060) NAME
3060     Format('    Storage File: ',A120)
      Endif
      If(BRS.EQ.1) Then
         NAME=ROOT(:P1)//'.BRS'
         Write(*,3070) NAME
3070     Format('    Storage File: ',A120)
      Endif
!
!  IF record second pass option count written to MSS file.
!
      If(IFCM.GT.0) Then
         Write(14,3080) IFCM
3080     Format(/,' *** Second pass defined by IF field 7 or',
     +            ' JO field 10 was activated in',I4,' months.')
      Endif
!
!  Date and time messages to MSS file.
!
* RSS comments
3090	continue
*3090  Call DATE(TIME3)
*      Write(14,4000) TIME3
*4000  Format(/,' Date:  ',A9)
*      Write(14,4010) TIME1
*4010  Format(' Beginning time: 'A8)
*      Call TIME(TIME2)
*      Write(14,4020) TIME2
*4020  Format(' Ending time:    'A8)
!
!  Completion message.
!
      Write(14,*) ' '
      Write(14,*)' ***** Normal Completion of Program WRAP-SIM *****'
      Write(14,*) ' '
      Print*,' '
      Print*,'   ***** Normal Completion of Program WRAP-SIM *****'
      Print*,' '
      Write(*,4030)
4030  Format(/,'    Exit WRAP-SIM',/)
      Read(*,4040) CD
4040  Format(A2)
!
!  End of Main Program
!
      Stop
      End
!
!  ***********************************************************************
!
      Subroutine ERROR
!
!   Subroutine ERROR writes a message to the monitor and stops execution
!   of the program.  It is called in conjunction with all error messages.
!
      Use COMVAR
      Character(len=2) EXIT
      Write(*,10)
10    Format(/,'    *** Execution of SIM terminated due to an',
     +         ' input error.')
      If(STATUS.NE.0) Then
         Write(14,10)
         Write(*,20) STATUS
         Write(14,20) STATUS
20       Format('    *** IOSTAT status variable (error code) =',I6)
         If(STATUS.EQ.61.or.STATUS.EQ.64) Then
            Write(*,30) CD
            Write(14,30) CD
30          Format('    *** ',A2,' record contains data',
     +                           ' in wrong format.')
         Elseif(STATUS.EQ.-1) Then
            Write(*,40)
            Write(14,40)
40          Format('    *** End of file was reached without',
     +             ' finding data record.')
         Elseif(STATUS.EQ.-2) Then
            Write(*,50) CD
            Write(14,50) CD
50          Format('    *** End of ',A2,' record was reached without',
     +             ' finding data.')
         Endif
      Endif
      Write(*,60)
60    Format('    *** See message file.',/)
      Write(*,70)
70    Format(/,'    Exit WRAP-SIM')
      Read(*,80) EXIT
80    Format(A2)
      Stop
      Return
      End Subroutine ERROR
!
! *****************************************************************************
!
      Subroutine FILES1
!
!  Subroutine FILES1 writes banner to monitor and opens the DAT and MSS files.
!  Subroutine CHECK is called as each file is opened.  Parameters controlling
!  opening of other files later by Subroutine FILES2 are initialized.
!
      Use COMVAR
      ALL=0
!
!  The banner is displayed and the filename roots are read.
!
      Print*,'  *****************************************************'
      Print*,'  **                                                 **'
      Print*,'  **          Water Rights Analysis Package          **'
      Print*,'  **                Simulation Model                 **'
      Print*,'  **                    WRAP-SIM                     **'
      Print*,'  **              January 2011 Version               **'
      Print*,'  **                                                 **'
      Print*,'  *****************************************************'
      Print*,' '
      Print*,' '
      Print*,'   Root of input and output file names is entered.'
      Print*,' '
      Read(*,10) ROOT
10    Format(A)
      ROOTH=ROOT
      Print*,' '
      Print*,'   Root of hydrology file names is entered.'
      Print*,' '
      Read(*,10) ROOTH
      Print*,' '
      If(ROOTH.EQ.' ') ROOTH=ROOT
      P1=Index(ROOT,'   ')-1
      P2=Index(ROOTH,'   ')-1
!
!  DAT file is opened.
!
      NAME=ROOT(:P1)//'.dat'
      Write(*,20)NAME
20    Format(1x,'   Opening Input File:   ',A120)
      Call CHECK
      Open(UNIT=3,FILE=NAME,STATUS='OLD')
!
!  MSS file is opened.
!
      NAME=ROOT(:P1)//'.MSS'
      Write(*,30) NAME
30    Format(1x,'   Opening Message File: ',A120)
      Call CHECK
      Open(UNIT=14,FILE=NAME,STATUS='UNKNOWN')
!
!  Defaults are set for the file switches with 1 meaning open and -1 not open.
!
      F1=1    ! FLO file:  INEV from JO record
      F2=1    ! EVA file:  INEV from JO record
      F3=-1   ! DIS file:  FDFLAG, EFFLAG, EPADJ
      F4=-1   ! HYD file:  If(INEV.EQ.5) F4=1
      F5=-1   ! FAD file:  If(FAD.GE.1) F5=1
      F7=-1   ! HRR file:  If(SYSOUT.GE.1) F7=1
      F8=-1   ! YRO file:  FY record
      F9=-1   ! BES file:  If(BES.GE.1) F9=1
!
!  End of Subroutine FILES1
!
      Return
      End Subroutine FILES1
!
! *****************************************************************************
!
      Subroutine FILES2
!
!  Subroutine FILES2 opens all of the input and output files except for the DAT
!  and MSS files which are opened earlier by Subroutine FILES1. Parameters read
!  by Subroutine READDAT from the DAT file JO, OF, CR, and FY records control
!  which other files to open. Subroutine CHECK is called as each file is opened.
!  The dimension limit MAXGAG is set if the DIS file is opened.
!
      Use COMVAR
      Integer N
      Character(len=6) CP
!
!  The mechanisms for activating the switches controlling which files are opened
!  are listed below along with the defaults set earlier by Subroutine FILES1.
!
!       File    Switch       Control                Default             Unit
!       DAT file       always open                                        3
!       FLO file: F1     INEV from JO record        F1=1   open           1
!       EVA file: F2     INEV from JO record        F2=1   open           2
!       DIS file: F3     FDFLAG, EFFLAG, EPADJ      F3=-1  not open       8
!       HYD file: F4     If(INEV.EQ.5) F4=1         F4=-1  not open      12
!       FAD file: F5     If(FAD.GE.1) F5=1          F5=-1  not open      21
!       MSS file: F6 (removed) always opened in current SIM version      14
!       HRR file: F7     If(SYSOUT.GE.1) F7=1       F7=-1  not open      13
!       YRO file: F8     FY record                  F8=-1  not open      11
!       BES file: F9     If(BES.GE.1) F9=1          F9=-1  not open      15
!       BRS file: BRS    If(BRS.EQ.1)               BRS=0  not open      16
!       CRM file: CR1    If(CR1.GE.1)               CR1=0  not open       4
!       OUT file: CR1    If(CR1.LE.0)               CR1=0  open           4
!       SOU file: DSS(2) If(DSS(2).EQ.1)            DSS(2)=0 not open    10
!       ZZZ file: ZZ     If ZZ > 0                  ZZ=0   not open      17
!       RUF file: RUF    If RUF or RUFIN > 0        RUF=0  not open      18
!
!  Switches are set for specifying which files to open.
!
      If(INEV.EQ.1.or.INEV.EQ.2) Then
         F1=1
         F2=2
         If(INEVN.EQ.9) F2=-1
         F4=-1
      Elseif(INEV.GE.3) Then
         F1=-1
         F2=-1
      Endif
      If(INEV.EQ.5) F4=1
      If(F4.GE.1) INEV=5
      If(NEVCP.LT.0) F2=-1
      If(FDFLAG.GE.1.or.EFLAG.EQ.9.or.EPADJ.EQ.-1.or.EPADJ.EQ.-2) F3=1
      If(FAD.GE.1) F5=1
      If(SYSOUT.GE.1) F7=1
      If(FYLEVEL.EQ.-99) F8=1
      If(BES.GE.1.and.BES.NE.5) F9=1
!
!  Input files are opened.
!
      Write(14,*) '*** Starting to open remaining files.'
10    Format(1X,'   Opening Input File:  ',A120)
20    Format(1X,'   Opening Output File: ',A120)
30    Format(' *** Opened file ',A120)
!
      If(F1.GE.0) Then
         NAME=ROOTH(:P2)//'.FLO'
         Write(*,10) NAME
         Call CHECK
         Open(UNIT=1,FILE=NAME,STATUS='OLD')
         Write(14,30) NAME
      Endif
      If(F2.GE.0) Then
         NAME=ROOTH(:P2)//'.eva'
         Write(*,10) NAME
         Call CHECK
         Open(UNIT=2,FILE=NAME,STATUS='OLD')
         Write(14,30) NAME
      Endif
      If(F3.EQ.1) Then
         NAME=ROOTH(:P2)//'.dis'
         Write(*,10) NAME
         Call CHECK
         Open(UNIT=8,FILE=NAME,STATUS='OLD')
         Write(14,30) NAME
      Endif
      If(F4.GE.1) Then
         NAME=ROOTH(:P2)//'.HYD'
         Write(*,10) NAME
         Call CHECK
         Open(UNIT=12,FILE=NAME,STATUS='OLD')
         Write(14,30) NAME
      Endif
      If((F1.GE.0.or.F2.GE.0).and.F4.GE.1) Then
         Print*, ' '
         Print*,' ERROR: Can not combine HYD with FLO/EVA files.'
         Print*,'        Stopped in Subroutine FILES2 due to error.'
         Call ERROR
      Endif
      If(F5.GE.1) Then
         NAME=ROOTH(:P2)//'.FAD'
         Write (*,10) NAME
         Call CHECK
         Open(UNIT=21,FILE=NAME,STATUS='OLD')
         Write(14,30) NAME
      Endif
      If(RUF.GE.1.or.RUFIN.GE.1) Then
         NAME=ROOTH(:P2)//'.RUF'
         Write (*,10) NAME
         Call CHECK
         Open(UNIT=18,FILE=NAME,STATUS='OLD')
         Write(14,30) NAME
      Endif
!
!  Output files are opened.
!
      If(DSS(1).GT.0) Then
         If(CR1.LE.0) Then
            NAME=ROOT(:P1)//'.OUT'
         Else
            NAME=ROOT(:P1)//'.CRM'
         Endif
         Write(*,20) NAME
         Call CHECK
         If(OUTFILE.GE.2) Then
            Open(UNIT=4,FILE=NAME,ACCESS='DIRECT',FORM='UNFORMATTED',
     +           STATUS='UNKNOWN',RECL=20)
         Else
            Open(UNIT=4,FILE=NAME,ACCESS='DIRECT',FORM='FORMATTED',
     +           STATUS='UNKNOWN',RECL=137,CARRIAGECONTROL='LIST')
         Endif
         Write(14,30) NAME
      Endif
      If(DSS(2).EQ.1) Then
         NAME=ROOT(:P1)//'.SOU'
         Write(*,20) NAME
         Call CHECK
         Open(UNIT=10,FILE=NAME,STATUS='UNKNOWN')
         Write(14,30) NAME
      Endif
      If(F7.GE.1) Then
         NAME=ROOT(:P1)//'.HRR'
         Write(*,20) NAME
         Call CHECK
         Open(UNIT=13,FILE=NAME,STATUS='UNKNOWN')
         Write(14,30) NAME
      Endif
      If(BRS.EQ.1) Then
         NAME=ROOT(:P1)//'.BRS'
         Write (*,20) NAME
         Call CHECK
         Open(UNIT=16,FILE=NAME,STATUS='UNKNOWN')
         Write(14,30) NAME
      Endif
      If(F8.EQ.1) Then
         NAME=ROOT(:P1)//'.YRO'
         Write(*,20) NAME
         Call CHECK
         Open(UNIT=11,FILE=NAME,STATUS='UNKNOWN')
         Write(14,30) NAME
      Endif
      If(F9.EQ.1) Then
         NAME=ROOT(:P1)//'.BES'
         Write (*,40) NAME
40       Format(1X,'   Opening In/Out File: ',A120)
         Open(UNIT=15,FILE=NAME,STATUS='UNKNOWN')
         Write(14,30) NAME
      Endif
      If(ZZ.GE.1) Then
         NAME=ROOT(:P1)//'.ZZZ'
         Write (*,20) NAME
         Call CHECK
         Open(UNIT=17,FILE=NAME,STATUS='UNKNOWN')
         Write(14,30) NAME
      Endif
!
      Write(14,*) '*** Finished opening text files.'
!
!  The dimension limit MAXGAG is set if the DIS file is opened.
!  The maximum number of upstream gages is determined by counting
!  the number of control points on the FD records in the DIS file.
!
      If(F3.EQ.1) Then
50       Read(8,60,IOSTAT=STATUS,END=100) CD
60       Format(A2)
         If(STATUS.NE.0) Then
            Write(14,70) CD,STATUS
70          Format(' ERROR: Fortran IOSTAT error occurred reading DIS',
     +             ' file from Subroutine FILES2.',/,8x,'CD = ',A2,3x,
     +             ' IOSTAT status variable =',I6)
            Call ERROR
         Endif
         If(CD.EQ.'ED') Goto 100
         If(CD.NE.'FD') Goto 50
         Backspace(8)
         NG=0
         N=22
80       Read(8,90,IOSTAT=STATUS) CD,CP
90       Format(A2,<N>x,2x,A6)
         If(STATUS.NE.0) Then
            Write(14,70) CD,STATUS
            Call ERROR
         Endif
         If(CP.NE.'      ') Then
            NG=NG+1
            N=N+8
            Backspace(8)
            Goto 80
         Endif
         If(NG.GT.MAXGAG) MAXGAG=NG
         NG=0
         Goto 50
      Endif
100   Rewind(8)
!
!  End of Subroutine FILES2
!
      Return
      End Subroutine FILES2
!
! *****************************************************************************
!
      Subroutine CHECK
!
!  Subroutine CHECK is called by Subroutines FILES1 and FILES2 during while
!  opening the input and output files.  Each data file is checked to see if
!  it exists. The program is terminated if an input file is missing. If an
!  output file exists, the file is overwritten or the program is terminated
!  at the users discretion.
!
      Use COMVAR
      Character(len=1) CHR
      Integer P3
      Logical TRY
!
!  Check if input data file exists.
!
      Inquire(FILE=NAME,EXIST=TRY)
      If(NAME(P1+1:P1+5).EQ.'.dat'.or.NAME(P2+1:P2+5).EQ.'.HYD'.or.
     +   NAME(P2+1:P2+5).EQ.'.inf'.or.NAME(P2+1:P2+5).EQ.'.eva'.or.
     +   NAME(P2+1:P2+5).EQ.'.dis'.or.NAME(P2+1:P2+5).EQ.'.FAD'.or.
     +   NAME(P2+1:P2+5).EQ.'.FLO'.or.NAME(P2+1:P2+5).EQ.'.RUF') Then
         If(.NOT.TRY) Then
            If(NAME(P2+1:P2+5).EQ.'.FLO') Then
               NAME2=NAME
               NAME=ROOTH(:P2)//'.inf'
               Inquire(FILE=NAME,EXIST=TRY)
               If(.NOT.TRY) Then
                  Print*,' '
                  Write(*,10) NAME2
               Else
                  Goto 40
               Endif
            Endif
            Print*,' '
            Write(*,10) NAME
10          Format('    Input file does not exist:  ',A120)
!
            P3=Index(NAME,'   ')-1
            NAME2=NAME(:P3)//'.TXT'
            Inquire(FILE=NAME2,EXIST=TRY)
            If(TRY) Then
               Write(*,20) NAME2
20             Format('    Extension TXT should be removed for file: ',
     +                 A120)
            Endif
            Print*,' '
            Print*,'   Program Terminated'
            Goto 50
         Endif
!
!  Check if output data file exists.
!
      Elseif(NAME(P1+1:P1+5).EQ.'.MSS'.or.NAME(P1+1:P1+5).EQ.'.OUT'.or.
     +       NAME(P1+1:P1+5).EQ.'.CRM'.or.NAME(P1+1:P1+5).EQ.'.HRR'.or.
     +       NAME(P1+1:P1+5).EQ.'.YRO'.or.NAME(P1+1:P1+5).EQ.'.BRS'.or.
     +       NAME(P1+1:P1+5).EQ.'.ZZZ') Then
         If(TRY) Then
            If(ALL.EQ.0) Then
               Print*, '   Replace the existing output file?'
               Print*, '     Yes(Y), No(N), Replace All(A)'
               Read(*,30) CHR
30             Format(A)
               If(CHR.EQ.'N'.or.CHR.EQ.'n')  Then
                  Print*, ' '
                  Print*,'   Program Terminated to Prevent',
     +                   ' Replacing File'
                  Goto 50
               Endif
               If(CHR.EQ.'A'.or.CHR.EQ.'a') ALL=ALL+1
            Endif
            Open(UNIT=7,FILE=NAME,STATUS='OLD')
            Close(UNIT=7,STATUS='DELETE')
         Endif
      Endif
40    Return
!
!  Termination of execution.
!
50    Write(*,60)
60    Format(/,'    Exit WRAP-SIM',/)
      Read(*,70) CD
70    Format(A2)
      Stop
      End Subroutine CHECK
!
! ***************************************************************************
!
      Subroutine READDAT
!
!  Subroutine READDAT reads the control point and water right data from the
!  DAT file (unit=3) and performs various manipulations organizing the data.
!
      Use COMVAR
!
      Real BEGIN,CIX,ENERGY,INACT,SAX,STX,TAILWT,TOTAL,TSA,TSB,TURCAP,
     +     TURELE,X
      Real CI(12),TARA(24),TARB(24)
!
      Integer BU,CRMAX,D,DUALD,FLAG,FS,FSC,FSV,I,IDSET,IEAR,IFFLAG2,ISO,
     +        J,J1,K,L,LAKESD,M,MATCH,MERROR,ML,N,NDI,NEA,NR,NRFS,NUMRG,
     +        NTORES,NTOWR,NUMCO,NUMGO,NUMWO,NUMWR,NWOUT,RGFLAG1,
     +        RGFLAG2,RGN,SA,SR,SWRCNT,SWRFLAG,TEMP,TSCOUNT,TSFLAG,
     +        TSYR1,TSYR2,WRFLAG,WSCOUNT,XPX,XPCOUNT,YR,YRLAST,Z
!
      Character(len=16) BUWRID,FYWRID,RGWRID
      Character(len=8)  BUGROUP,FYGROUP,TOCONT,RG1,RG2
      Character(len=6)  ACPID,BACKUP,CIID,CP,FSCP,NOTFLAG,RFIDWR,RES,
     +                  TOFLOW,USETYPE,XCPID
      Character(len=3)  TSL
      Character(len=1)  TSC
!
      Real,Allocatable,Dimension(:)::USEFAC,USEMUL
      Real,Allocatable,Dimension(:,:)::RGA
      Integer,Allocatable,Dimension(:)::RGWR,UPFLAG,USEP,USEADD,
     +                                  XPOUT,XPRIORITY
      Integer,Allocatable,Dimension(:,:)::RGCPI,RGI
      Character(len=16),Allocatable,Dimension(:)::TOWR,WRID1,BUID
      Character(len=8),Allocatable,Dimension(:)::WRID2,WRID3
      Character(len=8),Allocatable,Dimension(:,:)::RGID
      Character(len=6),Allocatable,Dimension(:)::RFID,TORES,ZZCP
      Character(len=6),Allocatable,Dimension(:,:)::RGCP
      Character(len=80) TITLE
!_____________________________________________________________________
!
!  Dimension limits are set based on counting records in the DAT file.
!
      D=0
      DSSYEAR='    '
      FSM=0
      MAXCI=0
      MAXCP=1
      MAXDI=0
      MAXDU=0
      MAXEA=0
      MAXFY=100
      MAXGAG=1
      MAXFS=0
      MAXFSP=1
      MAXML=0
      MAXMON=12
      MAXMS=0
      MAXPOW=0
      MAXRF=0
      MAXRO=0
      MAXTAB=0
      MAXTO=0
      MAXTS=0
      MAXSWR=1
      MAXSYS=1
      MAXTSWR=0
      MAXUSE=0
      MAXWR=1
      MAXWS=1
      MAXPX=0
      MAXOS=0
      MAXOSS=0
      NUMCO=0
      NUMGO=0
      NUMRG=0
      NUMWO=0
      RGFLAG1=0
      RGFLAG2=0
      TEMP=0
      TL=12
      TLD=25
      TSFLAG=0
      WRFLAG=0
      ZZ=0
10    Format(A2)
20    Read(3,10,IOSTAT=STATUS,End=160) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'ED') Goto 160
      If(CD.EQ.'RO') MAXRO=MAXRO+1
      If(CD.EQ.'CO') NUMCO=NUMCO+1
      If(CD.EQ.'WO') NUMWO=NUMWO+1
      If(CD.EQ.'GO') NUMGO=NUMGO+1
      If(CD.EQ.'RF') MAXRF=MAXRF+1
      If(CD.EQ.'CP') MAXCP=MAXCP+1
      If(CD.EQ.'CI') MAXCI=MAXCI+1
      If(CD.EQ.'DI') MAXDI=MAXDI+1
      If(CD.EQ.'EA') MAXEA=MAXEA+1
      If(CD.EQ.'TO') MAXTO=MAXTO+1
      If(CD.EQ.'BU') MAXTO=MAXTO+1
      If(CD.EQ.'MS') MAXMS=MAXMS+1
      If(CD.EQ.'OS') MAXOS=MAXOS+1
      If(CD.EQ.'ML') MAXML=MAXML+1
      If(CD.EQ.'SV') MAXTAB=MAXTAB+1
      If(CD.EQ.'PV') MAXPOW=MAXPOW+1
      If(CD.EQ.'UC') Then
         MAXUSE=MAXUSE+1
         Read(3,30) CD,USETYPE
30       Format(A2,A6)
         If(CD.EQ.'UC'.and.USETYPE.EQ.'      ') Then
            Continue
         Else
            Backspace(3)
         Endif
      Endif
      If(CD.EQ.'SO') Then
         Backspace(3)
         Read(3,40,IOSTAT=STATUS) BACKUP
40       Format(34x,A6)
         If(STATUS.NE.0) Goto 6000
         If(BACKUP.NE.'      ') MAXTO=MAXTO+1
      Endif
      If(CD.EQ.'WR'.or.CD.EQ.'IF') Then
         MAXWR=MAXWR+1
         WSCOUNT=0
         FLAG=0
         TSFLAG=0
         FSC=0
         Backspace(3)
         If(CD.EQ.'WR') Then
            Read(3,50,IOSTAT=STATUS) TEMP
50          Format(32x,I4)
            If(STATUS.NE.0) Goto 6000
            If(TEMP.NE.0.and.TEMP.NE.1.and.TEMP.NE.4) Then
               MAXSWR=MAXSWR+1
               FLAG=-99
            Endif
         Elseif(CD.EQ.'IF') Then
            Read(3,60,IOSTAT=STATUS) TEMP,J1
60          Format(32x,I4,I4)
            If(STATUS.NE.0) Goto 6000
            If(TEMP.GE.2.or.J1.EQ.3.or.J1.EQ.4.or.J1.EQ.-4) Then
               MAXSWR=MAXSWR+1
               FLAG=-99
            Endif
         Endif
      Endif
      If(CD.EQ.'WS') Then
         MAXWS=MAXWS+1
         WSCOUNT=WSCOUNT+1
         If(WSCOUNT.GE.MAXSYS) MAXSYS=WSCOUNT+1
         If(FLAG.EQ.0) Then
            If(WSCOUNT.EQ.2.) MAXSWR=MAXSWR+1
         Endif
         Backspace(3)
         Read(3,70,IOSTAT=STATUS) TEMP
70       Format(72x,I8)
         If(STATUS.NE.0) Goto 6000
         If(TEMP.LT.0) MAXSWR=MAXSWR+1
      Endif
      If(CD.EQ.'FS') Then
         MAXFS=MAXFS+1
         Backspace(3)
         Read(3,80,IOSTAT=STATUS) TEMP
80       Format(56x,I8)
         If(STATUS.NE.0) Goto 6000
         If(TEMP.GT.MAXFSP) MAXFSP=TEMP
         FSC=FSC+1
         If(FSC.GT.FSM) FSM=FSC
      Endif
      If(CD.EQ.'JD') Then
         Backspace(3)
         Read(3,90,IOSTAT=STATUS) NYRS,DSSYEAR,TL
90       Format(2x,I6,4x,A4,56x,I8)
         If(STATUS.NE.0) Goto 6000
         MAXTS=NYRS
         If(TL.LE.12) TL=12
         TLD=2*TL+1
      Endif
      If(CD.EQ.'TS'.and.TSFLAG.EQ.0) Then
         MAXTSWR=MAXTSWR+1
         TSFLAG=99
      Endif
      If(CD.EQ.'PX') Then
         Backspace(3)
         Read(3,100,IOSTAT=STATUS) J,X,K,L
100      Format(2x,I6,F8.0,I8,8x,I8)
         If(STATUS.NE.0) Goto 6000
         If(X.NE.0.0.or.K.GT.0.or.L.GT.0) Then
            MAXPX=MAXPX+1
            If(L.GT.0) MAXWR=MAXWR+1
         Endif
         If(D.EQ.0.and.(J.EQ.3.or.J.EQ.33.or.J.EQ.333.or.J.EQ.4))
     +      MAXDU=MAXDU+1
      Endif
      If(CD.EQ.'RG') Then
         Backspace(3)
         Read(3,104) CD,I
104      Format(A2,I1)
         If(I.EQ.0) Then
            NUMRG=NUMRG+1
            Backspace(3)
            Read(3,106,IOSTAT=STATUS) J,K
106         Format(16x,2I4)
            If(STATUS.NE.0) Goto 6000
            If(J.GT.0) RGFLAG1=99
            If(K.GT.0) RGFLAG2=99
         Endif
      Endif
      If(CD.EQ.'SO'.and.D.EQ.0) Then
         Backspace(3)
         Read(3,110,IOSTAT=STATUS) J
110      Format(96x,I8)
         If(STATUS.NE.0) Goto 6000
         If(J.EQ.3.or.J.EQ.33.or.J.EQ.333.or.J.EQ.4) MAXDU=MAXDU+1
      Endif
      If(CD.EQ.'JO') Then
         Backspace(3)
         Read(3,120,IOSTAT=STATUS) J
120      Format(72x,I8)
         If(STATUS.NE.0) Goto 6000
         If(J.GE.3) D=9
      Endif
      If(CD.EQ.'ZZ') Then
         Backspace(3)
         Read(3,130,IOSTAT=STATUS) ZZ
130      Format(2x,I6)
         If(STATUS.NE.0) Goto 6000
         If(ZZ.EQ.0) ZZ=1
      Endif
!
!  Error routine is activated if a SIMD record is found.
!
      If(CD.EQ.'FF'.or.CD.EQ.'FR'.or.CD.EQ.'DW'.or.CD.EQ.'C3'.or.
     +   CD.EQ.'JT'.or.CD.EQ.'TI'.or.CD.EQ.'W2'.or.CD.EQ.'R2'.or.
     +   CD.EQ.'C2'.or.CD.EQ.'G2'.or.CD.EQ.'FV'.or.CD.EQ.'FQ') Then
         Write(14,*) ' '
         Write(14,140) CD
140      Format(' ERROR: CD of ',A2,' is used only in SIMD, not SIM.')
         Write(14,5000)
         Call ERROR
      Endif
!
!  Error routine is activated if obsolete DT record is found.
!
      If(CD.EQ.'DT') Then
         Write(14,150)
150      Format(' ERROR: DT record is obsolete.',
     +          ' It is replaced with PX record.')
         Write(14,5000)
         Call ERROR
      Endif
!
!  End of the counting loop.
!
      Goto 20
160   Rewind(3)
      MAXGP=Max(NUMCO,NUMWO,NUMGO)*5
      MAXRO=MAXRO*5
      MAXUSE=MAXUSE+2
      MAXRES=MAXWS
      If(MAXOS.GT.0) MAXOSS=12*NYRS
      If(MAXPOW.LT.50) MAXPOW=50
      If(MAXTAB.LT.50) MAXTAB=50
      If(MAXFS.EQ.0) MAXFSP=0
      If(D.EQ.9) MAXDU=MAXWR
      If(RGFLAG1.EQ.99) MAXDU=MAXWR
      If(RGFLAG2.EQ.99) MAXPX=MAXWR
!
!  Allocatable arrays dimensioned for MAXCP and MAXWR.
!
!    Real arrays
!
      Allocate(ADL(MAXWR),ANNDEP(MAXWR),ARW(MAXWR),CL(MAXCP),EWA(MAXCP),
     +         MONDEP(MAXWR),MRW(MAXWR),WSHED(MAXWR))
      Allocate(CLOSS(MAXCP,2),CPDT(MAXCP,2),WRDAT(MAXWR,4))
      If(MAXCI.GT.0) Allocate(CINF(MAXCP,MAXMON))
!
!    Integer arrays.
!
      Allocate(CPNXT(MAXCP),DINDEX(MAXWR),DD(MAXWR),DUAL(MAXWR),
     +         IFRESREL(MAXCP),IFMETH(MAXWR),INMETHOD(MAXCP),
     +         INWS(MAXCP),IRF(MAXWR),ISHT(MAXWR),LM(MAXWR,2),
     +         NICP(MAXCP,2),NOTF(MAXWR),RFMETH(MAXWR),SERIES(MAXWR),
     +         WRTO(MAXWR),WRNUM(MAXWR,11))
!
      If(MAXFS.GT.0) Then
         N=FSM
         If(NUMRG.GT.FSM) N=FSM+1
         If(NUMRG.GT.FSM+1) N=FSM+2
         Allocate(FSN(MAXWR,N))
         FSN=0
      Endif
!
!    Character arrays.
!
      Allocate(CPEV(MAXCP),CPIN(MAXCP),CPID(MAXCP,2))
      Allocate(WRIDS(MAXWR,2),WRID(MAXWR))
!
!    Character and interger arrays associated with ZZ record.
!
      If(ZZ.GE.1) Then
         Allocate(ZZCP(ZZ))
         Allocate(ZZI(ZZ))
         Allocate(ZZF(ZZ,3),ZZFX(ZZ,3))
      Endif
!
!  Allocatable arrays dimensioned with limits other than MAXCP and MAXWR.
!
!    Real arrays
!
      Allocate(AFX(MAXEA,20),AFMIN(MAXEA),AFMAX(MAXEA),BUX(MAXTO),
     +    CUMBEG(MAXEA),CUMCAP(MAXEA),CUMEND(MAXEA),CUMEV(MAXEA),
     +    DIFACT(MAXDI),DIPER(MAXDI,TL),DISTO(MAXDI,TL),EAL(MAXEA),
     +    EAF(MAXEA,20),EVCURV(MAXTAB,TLD),FACT(MAXTO),FYFACT(MAXFY),
     +    PVCURV(MAXPOW,TLD),FYIN(5),FSX(MAXFS,4),MSD(MAXML,12),
     +    PDUSCF(MAXUSE,MAXMON),QTS(MAXTS,MAXTSWR,12),RESDAT(MAXRES,15),
     +    RF(MAXRF,MAXMON),RGA(NUMRG,2),OS(MAXOS,MAXOSS),
     +    STMON(MAXMS,MAXMON+1),SYSREL(MAXSYS),SYSTEM(MAXSWR,MAXSYS,10),
     +    TELEV(MAXRES),TPCAP(MAXRES),TQCAP(MAXRES),TOLIM(MAXTO,2),
     +    TWCURV(MAXPOW,TLD),USEFAC(MAXUSE),USEMUL(MAXUSE),
     +    WRSYS(MAXSYS,10),XA(MAXPX,12),XAMIN(MAXPX),XAMAX(MAXPX))
!
!    Integer arrays.
!
      Allocate(BUWR(MAXTO),DINUM(MAXDI),DUALX(MAXDU),EAI(MAXEA,20),
     +    EAO(MAXEA),EAR(MAXRES),EARNUM(MAXEA),EAX(MAXEA),EMPTY(MAXDI),
     +    FSI(MAXFS,12),FSOR(MAXSYS),FYWR(MAXFY),MSRES(MAXMS),
     +    OSRES(MAXOS),NEAF(MAXEA),NSR(MAXSWR),RANK(MAXWR),
     +    RESNUM(MAXRES,5),RGCPI(NUMRG,12),RGI(NUMRG,9),
     +    SN1(MAXSWR,MAXSYS),SN2(MAXSWR,MAXSYS),SN3(MAXSWR,MAXSYS),
     +    TOCP(MAXTO),TORI(MAXTO),TOFLAG(MAXTO),TOTARGET(MAXTO),
     +    TOWI(MAXTO),UPFLAG(MAXUSE),USEP(MAXUSE),USEADD(MAXUSE),
     +    XAFFLAG(MAXEA),XAXFLAG(MAXPX),XCP(MAXPX),XCPI(MAXPX),
     +    XP(MAXPX),XPOUT(MAXPX),XPR(MAXPX),XPRIORITY(MAXPX))
!
!    Character arrays.
!
      Allocate(BUG(MAXTO),BUID(MAXTO),CPOUID(MAXGP),DIRES(MAXDI,12),
     +        EARES(MAXEA,20),GROUP(MAXGP),REOUID(MAXRO),RESID(MAXRES),
     +        RFID(MAXRF),RGID(NUMRG,3),RGCP(NUMRG,12),TOCOMB(MAXTO),
     +        TSCOM(MAXTSWR),USEID(MAXUSE),WROUT(MAXGP),TORES(MAXTO),
     +        TOWR(MAXTO),WRID1(MAXPX),WRID2(MAXPX),WRID3(MAXPX))
!
!  Character variables are initialized.
!
      RCP=' '
      CD=' '
      TSL='   '
      DSSMONTH='JAN'
!
!  Integer variables used as counters are initialized.
!
      CPFLAG=0
      DUAL1=0
      EFLAG=0
      FDFLAG=0
      FS=0
      FSFLAG=0
      NDD=0
      NDS=0
      NDT=0
      ISO=0
      ML=0
      NCPTS=0
      NIF=0
      NPTABL=0
      NINCP=0
      NREOUT=0
      NRES=0
      NRFS=0
      NTABLE=0
      NTWTABL=0
      NTORES=0
      NTOWR=0
      NUSES=2
      NWROUT=0
      NWRTS=0
      STATUS=0
      SWR=0
      SWRCNT=0
      RGN=0
      RUF=0
      RUFIN=0
      TSCOUNT=0
      XPX=0
      XPCOUNT=0
!
!  Other integer variables are intialized.
!
      ADJINC=0
      BES=0
      CPOUT=0
      CR1=0
      CR2=0
      CR4=1.0
      DUALD=0
      FYN=0
      FYLEVEL=0
      GPNUM=0
      IRO=0
      NEGINC=0
      NPRDS=12
      NPT=0
      NTABLE=0
      NUMPOW=0
      OUTWR=0
      PASS2=0
      SIM2=0
      SIM3=0
      STATUS=0
      STOFLG=0
      SYSOUT=0
      XCP=0
      XP=0
      ZZCALL=0
!
!  Real variables are initialized.
!
      AVAMT=0.0
      EVCFA=0.0
      EVCFB=0.0
      EVCFC=0.0
      INRES=0.0
      OUTRES=0.0
      RET=0.0
      TELEV=0.0
      TQCAP=0.0
      TPCAP=0.0
!
!  Character arrays are initialized.
!
      BUG='        '
      BUID='                '
      CPID=' '
      CPOUID=' '
      GROUP=' '
      REOUID=' '
      RESID=' '
      RGCP='      '
      TOCOMB=' '
      TORES='      '
      TOWR='                '
      WROUT=' '
      WRID=' '
      WRIDS=' '
!
!  Integer arrays are initialized.
!
      BUWR=0
      CPNXT=0
      DINDEX=0
      DD=0
      DSS=0
      DSS(1)=1
      DSS(6)=2
      DUAL=0
      DUALX=0
      EAR=0
      EARNUM=0
      EAX=0
      FYWR=0
      IFRESREL=0
      IRF=0
      ISHT=0
      Do I=1,MAXWR
         LM(I,1)=1
         LM(I,2)=12
      End Do
      NOTF=0
      NSR=0
      RGI=0
      SERIES=0
      SN1=0
      SN2=0
      SN3=0
      TOCP=0
      TOTARGET=0
      TOFLAG=0
      TORI=0
      TOWI=0
      WRNUM=0
      WRTO=0
      XAFFLAG=0
      XAXFLAG=0
!
!  Real arrays are initialized.
!
      ADL=0.0
      AFMIN=9999999000000.0
      AFMAX=9999999000000.0
      AFX=1.0
      ARW=0.0
      BUX=1.0
      CL=0.0
      If(MAXCI.GT.0) CINF=0.0
      DIFACT=1.0
      DIPER=-9.0
      DISTO=0.0
      EVCURV=0.0
      FACT=1.0
      FYFACT=0.0
      FYIN=0.0
      MRW=0.0
      PVCURV=0.0
      RESNUM=0
      RESDAT=0.0
      RF=0.0
      OS=0.0
      STMON=0.0
      TWCURV=0.0
      Do I=1,MAXTO
         TOLIM(I,1)=0.0
         TOLIM(I,2)=99000000.0
      End Do
      WRDAT=0.0
      WRSYS=0.0
      XAMIN=9999999000000.0
      XAMAX=9999999000000.0
      XA=1.0
!_______________________________________________________________T1,T2,T3
!
!  Title T1,T2,T3 records are read.
!
      TITLE1='  '
      TITLE2='  '
      TITLE3='  '
180   Read(3,190,IOSTAT=STATUS,END=7000) CD,TITLE
190   Format(A2,A80)
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Goto 180
      If(CD.EQ.'T1') Then
         TITLE1=TITLE
         Goto 180
      Elseif(CD.EQ.'T2') Then
         TITLE2=TITLE
         Goto 180
      Elseif(CD.EQ.'T3') Then
         TITLE3=TITLE
         Goto 180
      Endif
      Backspace(3)
!_______________________________________________________________CR,JD,JO
!
!  The CR, JD, and JO records are read.
!
      MATCH=0
200   Read(3,10,IOSTAT=STATUS,END=7000) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Goto 200
      If(CD.NE.'JD'.and.CD.NE.'JO'.and.CD.NE.'CR'.
     +              and.CD.NE.'FO') Then
         Backspace(3)
         Goto 290
!
!  FO record is obsolete.   __________________________________________FO
!
      Elseif(CD.EQ.'FO') Then
         Backspace(3)
         Read(3,210,IOSTAT=STATUS) CD,F1,F2,F3,F4,F5,F7,F8,F9
210      Format(A2,I6,4I8,8x,3I8)
         If(STATUS.NE.0) Goto 6000
         Write(14,220)
220      Format(' WARNING: The file options FO record is obsolete,',
     +          ' having been replaced by a',/,10x,'combination of',
     +          ' JO record fields 3-6 and automatic file activation.')
         Goto 200
!
!  CR record is read.   ______________________________________________CR
!
      Elseif(CD.EQ.'CR') Then
         Backspace(3)
         Read(3,230,IOSTAT=STATUS) CD,CR1,CR2,CR3,CR4,CR5
230      Format(A2,I6,2I8,F8.0,I8)
         If(STATUS.NE.0) Goto 6000
         If(ICHECK.GE.0) Write(14,*) '*** CR record was read.'
         If(CR1.EQ.0.0) CR1=12
         If(CR4.LE.0.0.and.CR4.GE.-0.1) CR4=1.0
         If(CR4.LT.-0.1) CR4=0.0
         If(CR2.LE.0) CRMAX=12*NYRS
         If(CR2.GT.0) CRMAX=12*NYRS-CR2+1
         If(CR1.LE.0.or.CR1.GT.CRMAX) Then
            Write(14,*) ' '
            If(CR2.LE.0) Write(14,240)
            If(CR2.GT.0) Write(14,245)
240         Format(' ERROR: For CR2 of zero, CR1 on CR record must be',
     +             ' greater than zero and less than 12*NYRS.')
245         Format(' ERROR: For CR2 greater than 0, CR1 on CR record',
     +             ' must be greater than zero and less than',
     +             ' 12*NYRS-CR2+1.')
            Write(14,5000)
            Call ERROR
         Endif
         If(CR2.GT.12) Then
            Write(14,*) ' '
            Write(14,250) CR2
250         Format(' ERROR: CR2 on CR record must be a month between',
     +             ' 1 and 12. Read CR2 of',I4)
            Write(14,5000)
            Call ERROR
         Endif
         If(CR3.GE.2.and.CR1.LE.12) Then
            Write(14,*) ' '
            Write(14,260) CR3,CR1
260         Format(' ERROR: CR3 of',I3,' is not valid for CR1 of',I3)
            Write(14,5000) 
            Call ERROR
         Endif
         If(CR3.LT.0.or.CR3.GT.2.or.CR5.LT.0.or.CR5.GT.2) Then
            Write(14,*) ' '
            Write(14,265) CR3,CR5
265         Format(' ERROR: CR3 of ',I2,' or CR5 of ',I2,' on CR',
     +             ' record is not valid.')
            Write(14,5000)
            Call ERROR
         Endif
         Goto 200
!
!  JD record is read.   ___________________________________________JD,JO
!
      Elseif(CD.EQ.'JD') Then
         MATCH=MATCH+1
         Backspace(3)
         Read(3,270,IOSTAT=STATUS) CD,NYRS,YRST,ICHECK,CPOUT,OUTWR,
     +          OUTFILE,ADJINC,NEGINC,EPADJ,TL,IDSET
270      Format(A2,I6,10I8)
         If(STATUS.NE.0) Goto 6000
         If(ICHECK.GE.0) Write(14,*) '*** JD record was read.'
         If(TL.LE.12) TL=12
         Goto 200
!
!  JO record is read.
!
      Elseif(CD.EQ.'JO') Then
         Backspace(3)
         Read(3,280,IOSTAT=STATUS) CD,INEV,FAD,SYSOUT,BES,BRS,STOFLG,
     +                             STODI,NPOPT,PASS2,DUALD,RUFIN,RUF
280      Format(A2,I6,9I8,2I4)
         If(STATUS.NE.0) Goto 6000
         If(ICHECK.GE.0) Write(14,*) '*** JO record was read.'
         Goto 200
      Endif
!
!  Data from JD and JO records are organized.
!
290   If(IDSET.NE.2) IDSET=1
      If(BES.EQ.4.or.BES.EQ.5) SIM2=9
      NIFLAG=-9
      If(ADJINC.EQ.-3.or.ADJINC.EQ.-4) Then
         NIFLAG=9
         ADJINC=Abs(ADJINC)
      Endif
      If(INEV.EQ.0) INEV=1
      INEVN=0
      If(INEV.LT.0) Then
         INEVN=9
         INEV=Abs(INEV)
      Endif
!
!  Error checks are performed for JD and JO records.
!
      If((ADJINC.GE.3.and.NEGINC.EQ.2).or.
     +   (ADJINC.EQ.2.and.NEGINC.GE.3).or.
     +   (ADJINC.EQ.5.and.NEGINC.GE.2)) Then
         Write(14,*) ' '
         Write(14,300) ADJINC,NEGINC
300      Format(' ERROR: ADJINC of',I4,' and NEGINC of',I4,
     +          ' on JD record are not compatible.') 
         Write(14,5000)
         Call ERROR
      Endif
      If(ADJINC.GT.5.or.ADJINC.LT.0) Then
         Write(14,*) ' '
         Write(14,310) ADJINC
310      Format(' ERROR: ADJINC of',I4,' on JD record not allowed.')
         Write(14,5000)
         Call ERROR
      Endif
      If(MATCH.NE.1) Then
         Write(14,*) ' '
         Write(14,*) ' ERROR: Missing JD record.'
         Write(14,5000) 
         Call ERROR
      Endif
      If(NYRS.LT.1) Then
         Write(14,*)' '
         Write(14,*) ' ERROR: Number of years in JD record field 2',
     +               ' must be at least one.'
         Write(14,5000)
         Call ERROR
      Endif
      If(IDSET.LT.0.or.IDSET.GE.3) Then
         Write(14,*) ' '
         Write(14,320) IDSET
320      Format(' ERROR: IDSET of',I3,' in JD field 7 is not valid.')
         Write(14,5000)
         Call ERROR
      Endif
      If(INEV.LT.-2.or.INEV.GT.6) Then
         Write(14,*) ' '
         Write(14,330) INEV
330      Format(' ERROR: INEV of',I3,' on JO record is not valid.')
         Write(14,5000)
         Call ERROR
      Endif
      If(SYSOUT.LT.0.or.SYSOUT.GT.3) Then
         Write(14,*) ' '
         Write(14,340) SYSOUT
340      Format(' ERROR: SYSOUT of',I3,' on JO record is not valid.')
         Write(14,5000)
         Call ERROR
      Endif
      If(PASS2.LT.0.or.PASS2.GT.2) Then
         Write(14,*) ' '
         Write(14,350) PASS2
350      Format(' ERROR: PASS2 of',I3,' on JO record is not valid.')
         Write(14,5000)
         Call ERROR
      Endif
      If(EPADJ.LT.-2.or.EPADJ.GT.0) Then
         Write(14,*) ' '
         Write(14,360) EPADJ
360      Format(' ERROR: EPADJ of',I3,' in JD field 10 is not valid.')
         Write(14,5000)
         Call ERROR
      Endif
      If(BES.LT.0.or.BES.GT.6) Then
         Write(14,*) ' '
         Write(14,370) BES
370      Format(' ERROR: BES =',I2,' in JO field 5 is not valid.')
         Write(14,5000)
         Call ERROR
      Endif
      If(BRS.LT.0.or.BRS.GT.1) Then
         Write(14,*) ' '
         Write(14,380) BRS
380      Format(' ERROR: BRS =',I2,' in JO field 6 is not valid.')
         Write(14,5000)
         Call ERROR
      Endif
      If(STODI.LT.0.or.STODI.GT.2) Then
         Write(14,*) ' '
         Write(14,390) STODI
390      Format(' ERROR: STODI of',I2,' on JO record is not valid.')
         Write(14,5000)
         Call ERROR
      Endif
      If(DUALD.LT.0.or.DUALD.GT.5) Then
         If(DUALD.NE.33.and.DUALD.NE.333.and.
     +      DUALD.NE.55.and.DUALD.NE.555) Then
            Write(14,*) ' '
            Write(14,400) DUALD
400         Format(' ERROR: DUALD of',I2,' on JO record is not valid.')
            Write(14,5000)
            Call ERROR
         Endif
      Endif
      If(RUF.LT.0.or.RUF.GT.3) Then
         Write(14,410) RUF
410      Format(' ERROR: RUF =',I2,' on JO record is not valid.')
         Write(14,5000)
         Call ERROR
      Endif
      If(RUFIN.LT.0.or.RUFIN.GT.2) Then
         Write(14,420) RUF
420      Format(' ERROR: RUFIN =',I2,' on JO record is not valid.')
         Write(14,5000)
         Call ERROR
      Endif
      If(RUFIN.EQ.1.and.RUF.GE.1) Then
         Write(14,430) RUFIN,RUF
430      Format(' ERROR: RUFIN =',I2,' and RUF =',I2,' on JO record.')
         Write(14,5000)
         Call ERROR
      Endif
      If(FAD.LT.0.or.FAD.GT.3) Then
         Write(14,440) FAD
440      Format(' ERROR: FAD =',I2,' on JO record is not valid.')
         Write(14,5000)
         Call ERROR
      Endif

!________________________________________________XL,FY,WO,GO,RO,CO,OF,ZZ
!
      CIX=1.0
      CNLB=0.0
      CNUB=100.0
      DEPTHX=0.01875
      EPWL=2.0
      EVX=1.0
      INX=1.0
      MATCH=0
      MPLB=0.0
      MPUB=100.0
      NCPOUT=0
      NGOUT=0
      NREOUT=0
      NWOUT=0
      POWFCT=0.0010237
      SAX=1.0
      STX=1.0
!
!  The next record is read.
!
580   Read(3,10,IOSTAT=STATUS,END=7000) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Goto 580
!_____________________________________________________________________XL
!
!  Multiplier factors and limits record.
!
      If(CD.EQ.'XL') Then
         Backspace(3)
         Read(3,590,IOSTAT=STATUS) CD,STX,INX,EVX,CIX,SAX,POWFCT,DEPTHX,
     +                             CNLB,CNUB,MPLB,MPUB,EPWL
590      Format(A2,F6.0,11F8.0)
         If(STATUS.NE.0) Goto 6000
         If(INX.LE.0.0) INX=1.0
         If(EVX.LE.0.0) EVX=1.0
         If(CIX.LE.0.0) CIX=1.0
         If(SAX.LE.0.0) SAX=1.0
         If(STX.LE.0.0) STX=1.0
         If(POWFCT.LE.0.0) POWFCT=0.0010237
         If(DEPTHX.LE.0.0) DEPTHX=0.01875
         If(CNUB.LE.0.0) CNUB=100.0
         If(MPUB.LE.0.0) MPUB=100.0
         If(EPWL.LE.0.0) EPWL=2.0
         Goto 580
      Endif
!_____________________________________________________________________FY
!
!  Firm yield record.
!
      If(CD.EQ.'FY') Then
         Backspace(3)
         Read(3,600,IOSTAT=STATUS) CD,(FYIN(J),J=1,5),FYWRID,FYGROUP,
     +                             MFY,SIM3
600      Format(A2,F6.0,4F8.0,A16,A8,I8,I8)
         If(STATUS.NE.0) Goto 6000
         If(FYIN(1).LE.0.0001) FYIN(1)=1.0
         FYLEVEL=-99
         If(SIM3.EQ.0) Then
            SIM3=9
         Else
            SIM3=99
         Endif
         FYN=0
         FYFACT(1)=1.0
         FYWRID=Adjustr(FYWRID)
         FYGROUP=Adjustr(FYGROUP)
         If(FYIN(2).LE.0.0.or.FYIN(3).LE.0.0) Then
            Write(14,*)' '
            Write(14,610) FYIN(1),FYIN(3)
610         Format(' ERROR: FYIN(2) and FYIN(3) on FY record must be',
     +             ' positive non-zero numbers. Read: ',2F9.1)
            Write(14,5000)
            Call ERROR
         Endif
         If(FYIN(3).GT.FYIN(2).or.FYIN(4).GT.FYIN(3)) Then
            Write(14,*)' '
            Write(14,620) FYIN(2),FYIN(3),FYIN(4)
620         Format(' ERROR: The incremental decreases on FY record '
     +        'must each be less than previous level. Read: ',3F9.1)
            Write(14,5000)
            Call ERROR
         Endif
         Goto 580
      Endif
!
!  OF record is read.   ______________________________________________OF
!
      If(CD.EQ.'OF') Then
         Backspace(3)
         Read(3,630,IOSTAT=STATUS) CD,DSS(1),DSS(2),DSS(3),DSS(4),
     +                             DSS(5),DSS(6),DSSMONTH
630      Format(A2,I6,5I8,5x,A3)
         If(STATUS.NE.0) Goto 6000
         If(ICHECK.GE.0) Write(14,*) '*** OF record was read.'
         DSSMONTH=Adjustr(DSSMONTH)
         If(DSSMONTH.EQ.'   ') DSSMONTH='JAN'
         DSS(7)=0
         If(DSS(4).GE.2) DSS(7)=DSS(4)
         If(DSS(4).GE.2.and.DSS(4).LE.4)  DSS(4)=0
         If(DSS(4).GE.5.and.DSS(4).LE.14) DSS(4)=1
         If(DSS(4).GE.15) Then
            Write(14,640) DSS(4)
640         Format(' ERROR: DSS(4) of',I3,' on OF record is not valid.')
            Write(14,5000)
            Call ERROR
         Endif
         If(DSS(6).EQ.0) DSS(6)=2
         If(DSS(6).GT.9.or.DSS(6).LT.0) Then
            Write(14,650) DSS(6)
650         Format(' ERROR: DSS(6) of',I3,' on OF record is not valid.')
            Write(14,5000)
            Call ERROR
         Endif
         Do I=1,3
            If(DSS(I).GT.1.or.DSS(I).LT.0) Then
               Write(14,660) I,DSS(I)
660            Format(' ERROR: DSS(',I1,') of',I3,
     +                ' on OF record is not valid.')
               Write(14,5000)
               Call ERROR
            Endif
         End Do
         If(DSS(5).EQ.1.and.INEV.EQ.6) Then
            Write(14,670)
670         Format(' ERROR: JD record INEV of 6 is not valid with',
     +             ' OF record DSS(5) of 1.')
            Call ERROR
         Endif
         Goto 580
      Endif
!____________________________________________________________WO,GO,RO,CO
!
      If(CD.EQ.'WO'.or.CD.EQ.'GO'.or.CD.EQ.'RO'.or.CD.EQ.'CO') Then
         Write(14,680) CD 
680      Format(' *** Reading ',A2,' record.')
      Endif
!
!  Water right identifiers from WO record.
!
      If(CD.EQ.'WO') Then
         MATCH=MATCH+1
         Backspace(3)
         Read(3,690,IOSTAT=STATUS) CD,NWOUT,(WROUT(J),J=1,5)
690      Format(A2,I6,5A16)
         If(STATUS.NE.0) Goto 6000
         If(NWOUT.EQ.0) Goto 580
         If(NWOUT.GT.5) Then
            K=INT((NWOUT-1)/5)+1
            Do 720 I=1,K-1
               Read(3,10,IOSTAT=STATUS) CD
               If(STATUS.NE.0) Goto 6000
               Backspace(3)
               If(CD.NE.'WO') Then
                  Write(14,*) ' '
                  Write(14,700) CD
700               Format(' ERROR: WO record missing. Read CD = ',A2)
                  Write(14,5000)
                  Call ERROR
               Endif
               Read(3,710,IOSTAT=STATUS) CD,(WROUT(J),J=(5*I+1),(5*I+5))
710            Format(A2,6X,5A16)
               If(STATUS.NE.0) Goto 6000
720         End Do
         Endif
         Do J=1,NWOUT
            WROUT(J)=Adjustr(WROUT(J))
         End Do
         Goto 580
!
!  Water right group identifiers from GO record.
!
      Elseif(CD.EQ.'GO') Then
         MATCH=MATCH+1
         Backspace(3)
         Read(3,790,IOSTAT=STATUS) CD,NGOUT,(GROUP(J),J=1,5)
         If(STATUS.NE.0) Goto 6000
         If(NGOUT.EQ.0) Goto 580
         If(NGOUT.GT.5) Then
            K=Int((NGOUT-1)/5)+1
            Do 740 I=1,K-1
               Read(3,10,IOSTAT=STATUS) CD
               If(STATUS.NE.0) Goto 6000
               Backspace(3)
               If(CD.NE.'GO') Then
                  Write(14,*) ' '
                  Write(14,730) CD
730               Format(' ERROR: GO record missing. Read CD = ',A2)
                  Write(14,5000)
                  Call ERROR
               Endif
               Read(3,800,IOSTAT=STATUS) CD,(GROUP(J),J=(5*I+1),(5*I+5))
               If(STATUS.NE.0) Goto 6000
               If(CD.NE.'GO') Then
                    MATCH=-1
                    Goto 810
               Endif
740        End Do
         Endif
         Do J=1,NGOUT
            GROUP(J)=Adjustr(GROUP(J))
         End Do
         Goto 580
!
!  Reservoir identifiers from RO record.
!
      Elseif(CD.EQ.'RO') Then
         MATCH=MATCH+1
         Backspace(3)
         Read(3,790,IOSTAT=STATUS) CD,NREOUT,(REOUID(J),J=1,5)
         If(STATUS.NE.0) Goto 6000
         If(NREOUT.EQ.0) Goto 580
         If(NREOUT.LT.0) Then
            REOUID(1)='   ALL'
            Goto 580
         Elseif(NREOUT.GT.5) Then
            K=Int((NREOUT-1)/5)+1
            Do 760 I=1,K-1
               Read(3,10,IOSTAT=STATUS) CD
               If(STATUS.NE.0) Goto 6000
               Backspace(3)
               If(CD.NE.'RO') Then
                  Write(14,*) ' '
                  Write(14,750) CD
750               Format(' ERROR: RO record missing. Read CD = ',A2)
                  Write(14,5000)
                  Call ERROR
               Endif
               Read(3,800,IOSTAT=STATUS)CD,(REOUID(J),J=(5*I+1),(5*I+5))
               If(STATUS.NE.0) Goto 6000
               If(CD.NE.'RO') Then
                    MATCH=-1
                    Goto 810
               Endif
760         End Do
         Endif
         Do J=1,NREOUT
            REOUID(J)=Adjustr(REOUID(J))
         End Do
         Goto 580
!
!  Control point identifiers from CO record.
!
      Elseif(CD.EQ.'CO') Then
         MATCH=MATCH+1
         Backspace(3)
         Read(3,790,IOSTAT=STATUS) CD,NCPOUT,(CPOUID(J),J=1,5)
         If(STATUS.NE.0) Goto 6000
         If(NCPOUT.EQ.0) Goto 580
         If(NCPOUT.GT.5) Then
            K=Int((NCPOUT-1)/5)+1
            Do 780 I=1,K-1
               Read(3,10,IOSTAT=STATUS) CD
               If(STATUS.NE.0) Goto 6000
               Backspace(3)
               If(CD.NE.'CO') Then
                  Write(14,*) ' '
                  Write(14,770) CD
770               Format(' ERROR: CO record missing. Read CD = ',A2)
                  Write(14,5000)
                  Call ERROR
               Endif
               Read(3,800,IOSTAT=STATUS)CD,(CPOUID(J),J=(5*I+1),(5*I+5))
               If(STATUS.NE.0) Goto 6000
               If(CD.NE.'CO') Then
                    MATCH=-1
                    Goto 810
               Endif
780         End Do
         Endif
         Do J=1,NCPOUT
            CPOUID(J)=Adjustr(CPOUID(J))
         End Do
         Goto 580
      Endif
!
!  Format statements and error message.
!
790   Format(A2,I6,5A8)
800   Format(A2,6X,5A8)
810   If(MATCH.LT.0) Then
         Write(14,*)' '
         Write(14,820)
820      Format(' ERROR: Missing WO, GO, RO, or CO record.')
         Write(14,5000)
         Call ERROR
      Endif
      If(MATCH.EQ.0.and.CPOUT.EQ.0.and.OUTWR.EQ.0.and.FYIN(2).EQ.0.0)
     +   Write(14,830) 
830      Format(' WARNING: No output is specified on JD, CO,',
     +          ' RO, WO, and/or GO records.')
      MATCH=0
!
!_____________________________________________________________________ZZ
!
!  ZZ record creating priority loop streamflow output ZZZ file.
!
      If(CD.EQ.'ZZ') Then
         Backspace(3)
         Read(3,840,IOSTAT=STATUS) CD,ZZX,ZZWR,(ZZCP(Z),Z=1,ZZ)
840      Format(A2,6x,F8.0,A16,<ZZ>(2x,A6))
         If(STATUS.NE.0) Goto 6000
         If(ICHECK.GE.0) Write(14,*) '*** ZZ record was read.'
         If(ZZX.LE.0.0) ZZX=-1.0
         ZZWR=Adjustr(ZZWR)
         Do Z=1,ZZ
            ZZCP(Z)=Adjustr(ZZCP(Z))
         End Do
!
!  Error checks.
!
         If(ZZ.LE.0.or.ZZ.GE.5000) Then
            Write(14,850) ZZ
850         Format(' ERROR: ZZ of',I5,' is not valid.')
            Write(14,5000)
            Call ERROR
         Endif
         Do Z=1,ZZ
            If(ZZCP(Z).EQ.'      ') Then
               Write(14,860) ZZ
860            Format(' ERROR: One or more control point identifiers',
     +                ' missing on ZZ record. ZZ =',I3)
               Write(14,5000)
               Call ERROR
            Endif
         End Do
         Goto 580
      Endif
!_____________________________________________________________________RG
!
!  RG record results in water rights being altered later in this subroutine.
!
      If(CD.EQ.'RG') Then
         Backspace(3)
         Read(3,870,IOSTAT=STATUS) CD,K
870      Format(A2,I1)
         If(STATUS.NE.0) Goto 6000
         If(K.EQ.2.and.RGN.EQ.0) Then
            Write(14,872)
872         Format(' ERROR: Read RG2 record without a preceding',
     +             ' RG record.')
            Write(14,5000)
            Call ERROR
         Endif
         If(K.NE.0.and.K.NE.2) Then
            Write(14,874) CD,K
874         Format(' ERROR: ',A2,I1,' is not a valid record',
     +             ' identifier. Should probably be RG2.')
            Write(14,5000)
            Call ERROR
         Endif
         Backspace(3)
!
!  RG record is read.
!
         If(K.EQ.0) Then
            RGN=RGN+1
            I=RGN
            Read(3,876,IOSTAT=STATUS) CD,RGI(I,1),RGID(I,1),RGI(I,2),
     +           RGI(I,3),RGCP(I,1),RGI(I,4),RGI(I,5),RGA(I,1),RGA(I,2),
     +           RGI(I,6),RGID(I,2),RGID(I,3),RGCP(I,2)
876         Format(A2,I6,A8,2I4,2x,A6,2I4,2F8.0,I8,2A8,2x,A6)
            If(STATUS.NE.0) Goto 6000
            RGID(I,1)=Adjustr(RGID(I,1))
            RGID(I,2)=Adjustr(RGID(I,2))
            RGID(I,3)=Adjustr(RGID(I,3))
            Do J=1,12
               RGCP(I,J)=Adjustr(RGCP(I,J))
            End Do
!
!  Error checks.
!
            If(RGI(I,3).LT.0.or.RGI(I,3).GE.5) Then
               Write(14,878) RGI(I,3)
878            Format(' ERROR: RGI(I,3) of',I4,' is not valid.')
               Write(14,5000)
               Call ERROR
            Endif
            If(RGI(I,3).NE.0.and.RGCP(I,1).EQ.'      ') Then
               Write(14,880)
880            Format(' ERROR: Non-blank RG field 6 is required for',
     +                ' non-zero RGI(I,3) in RG field 5.')
               Call ERROR
            Endif
            If(RGI(I,3).EQ.0.and.RGCP(I,1).NE.'      ') Then
               Write(14,882)
882            Format(' ERROR: RG field 6 should be blank for RGI(I,3)',
     +                ' of zero in RG field 5.')
               Call ERROR
            Endif
            If(RGI(I,4).LT.0.or.RGI(I,4).GE.4) Then
               Write(14,884) RGI(I,4)
884            Format(' ERROR: RGI(I,4) of',I3,' is not valid.')
               Write(14,5000)
               Call ERROR
            Endif
         Endif
!
!  Supplemental RG2 record is read.
!
         If(K.EQ.2) Then
            Read(3,886,IOSTAT=STATUS) CD,RGI(I,7),RGI(I,8),RGI(I,9),
     +                                (RGCP(I,J),J=3,12)
886         Format(A2,1x,I5,2I4,10(2x,A6))
            If(STATUS.NE.0) Goto 6000
            J=RGI(I,7)+RGI(I,8)+RGI(I,9)
            If(J.GT.10.or.J.LE.0) Then
               Write(14,888) J
888            Format(' ERROR: ',I3,' control points indicated in RG2',
     +                ' record fields 2, 3, and 4 is not valid.')
               Write(14,5000)
               Call ERROR
            Endif
         Endif
!
         Goto 580
      Endif
!_____________________________________________________________________UC
!
!  Monthly water use distribution coefficient UC records.
!
!  The default set of water use distribution factors is a
!  constant uniform 1/12 for each month.
!
      USEID(1)='      '
      Do J=1,12
         PDUSCF(1,J)=1.0/12.0
      End Do
!
!  Factors for use identifier NDAYS is based on the
!  number of days in each month.
!
      USEID(2)=' NDAYS'
      PDUSCF(2,1)=31
      PDUSCF(2,2)=28
      PDUSCF(2,3)=31
      PDUSCF(2,4)=30
      PDUSCF(2,5)=31
      PDUSCF(2,6)=30
      PDUSCF(2,7)=31
      PDUSCF(2,8)=31
      PDUSCF(2,9)=30
      PDUSCF(2,10)=31
      PDUSCF(2,11)=30
      PDUSCF(2,12)=31
!
!  Monthly water use distribution coefficient UC records are read.
!
      If(ICHECK.GE.0)Write(14,*)'*** Starting to read UC records.'
      Backspace(3)
890   Read(3,10,IOSTAT=STATUS,END=7000) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Then
         Goto 890
      Elseif(CD.EQ.'UC') Then
         NUSES=NUSES+1
         Read(3,900,IOSTAT=STATUS,END=7000) CD,USETYPE
900      Format(A2,A6)
         Backspace(3)
         Backspace(3)
         If(CD.EQ.'UC'.and.USETYPE.EQ.'      ') Then
            Read(3,910,IOSTAT=STATUS)CD,USEID(NUSES),
     +                               (PDUSCF(NUSES,J),J=1,6)
910         Format(A2,A6,6F8.0)
            If(STATUS.NE.0) Goto 6000
            Read(3,920,IOSTAT=STATUS) CD,(PDUSCF(NUSES,J),J=7,12)
920         Format(A2,6x,6F8.0)
            If(STATUS.NE.0) Goto 6000
         Else
            Read(3,930,IOSTAT=STATUS)CD,USEID(NUSES),
     +                               (PDUSCF(NUSES,J),J=1,12)
930         Format(A2,A6,12F8.0)
            If(STATUS.NE.0) Goto 6000
         Endif
         If(ICHECK.EQ.2) Then
            Write(14,900) CD,USEID(NUSES),(PDUSCF(NUSES,J),J=1,12)
         Endif
         Goto 890
      Elseif(CD.EQ.'CP'.or.CD.EQ.'RF'.or.CD.EQ.'UP') Then
         Backspace(3)
         Goto 950
      Else
         Write(14,*)' '
         Write(14,940) CD
940      Format(' ERROR: Read inappropriate record with CD of ',A2)
         Write(14,5000)
         Call ERROR
      Endif
!
!  Use distribution data are converted to fractional multiplier format.
!
950   Do I=1,NUSES
         TOTAL=0.
         Do J=1,NPRDS
            TOTAL=TOTAL+PDUSCF(I,J)
         End Do
         Do J=1,NPRDS
            PDUSCF(I,J)=PDUSCF(I,J)/TOTAL
         End Do
      End Do
      If(ICHECK.GE.0)Write(14,*)'*** Finished reading UC records.'
!______________________________________________________________________UP
!
!  Water use priority adjustment UP records are read.
!
      If(CD.EQ.'UP') Then
         If(ICHECK.GE.0)Write(14,*)'*** Starting to read UP records.'
      Endif
      Do K=1,NUSES
         UPFLAG(K)=0
         USEP(K)=0
         USEADD(K)=0
         USEMUL(K)=0.0
         USEFAC(K)=0.0
      End Do
960   Read(3,10,IOSTAT=STATUS,END=7000) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Then
         Goto 960
      Elseif(CD.EQ.'CP'.or.CD.EQ.'RF') Then
         Backspace(3)
         Goto 1010
      Elseif(CD.EQ.'UP') Then
         Backspace(3)
         Read(3,970) CD,USETYPE
970      Format(A2,A6)
         USETYPE=Adjustr(USETYPE)
         Backspace(3)
         K=0
980      K=K+1
         If(USETYPE.EQ.USEID(K)) Then
            Read(3,990,IOSTAT=STATUS) USEP(K),USEADD(K),
     +                                USEMUL(K),USEFAC(K)
990         Format(8x,2I8,2F8.0)
            If(STATUS.NE.0) Goto 6000
            UPFLAG(K)=9
            Goto 960
         Elseif(K.GT.NUSES) Then
            Write(14,*) ' '
            Write(14,1000) USETYPE
1000        Format(' ERROR: Use identifier ',A6,' from UP record',
     +             ' matches no identifier on the UC records.')
            Write(14,5000)
            Call ERROR
         Else
            Goto 980
         Endif
      Else
         Write(14,*)' '
         Write(14,940) CD
         Write(14,5000)
         Call ERROR
      Endif
!_____________________________________________________________________RF
!
!  Monthly return flow factor RF records are read.
!
1010  Read(3,10,IOSTAT=STATUS,END=7000) CD
         If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Then
         Goto 1010
      Elseif(CD.EQ.'RF') Then
         NRFS=NRFS+1
         Read(3,1020) CD,RFID(NRFS)
1020     Format(A2,A6)
         If(STATUS.NE.0) Goto 6000
         Backspace(3)
         Backspace(3)
         If(CD.EQ.'RF'.and.RFID(NRFS).EQ.'      ') Then
            Read(3,1030,IOSTAT=STATUS)CD,RFID(NRFS),(RF(NRFS,MT),MT=1,6)
1030        Format(A2,A6,6F8.0)
            Read(3,1040,IOSTAT=STATUS) CD,(RF(NRFS,MT),MT=7,12)
1040        Format(A2,6X,6F8.0)
            If(STATUS.NE.0) Goto 6000
         Else
            Read(3,1050,IOSTAT=STATUS) CD,RFID(NRFS),
     +                                 (RF(NRFS,MT),MT=1,12)
1050        Format(A2,A6,12F8.0)
            If(STATUS.NE.0) Goto 6000
         Endif
         RFID(NRFS)=Adjustr(RFID(NRFS))
         If(ICHECK.EQ.2) Then
            Write(14,690) CD,RFID(NRFS),(RF(NRFS,MT),MT=1,12)
         Endif
         Goto 1010
!_____________________________________________________________________CP
!
!  Control point CP records are read.
!
      Elseif(CD.EQ.'CP') Then
         Backspace(3)
         If(ICHECK.GE.0) Write(14,*)
     +      '*** Starting to read CP records.'
      Else
         Write(14,*)' '
         Write(14,1060) CD
1060     Format(' ERROR: Read CD of ',A2,' when expecting CP records.')
         Write(14,5000)
         Call ERROR
      Endif
1070  Read(3,10,IOSTAT=STATUS,END=7000) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Then
         Goto 1070
      Elseif(CD.EQ.'CP') Then
         Backspace(3)
         NCPTS=NCPTS+1
!
!  The CP record is read.
!
         Read(3,1080,IOSTAT=STATUS) CD,CPID(NCPTS,1),CPID(NCPTS,2),
     +          CPDT(NCPTS,1),CPDT(NCPTS,2),INMETHOD(NCPTS),CPIN(NCPTS),
     +          CPEV(NCPTS),EWA(NCPTS),CL(NCPTS),INWS(NCPTS)
1080     Format(A2,A6,2x,A6,2F8.0,I8,2x,A6,2x,A6,2F8.0,I8)
         If(STATUS.NE.0) Goto 6000
         If(ICHECK.EQ.3) Write(14,1080) CD,CPID(NCPTS,1),CPID(NCPTS,2),
     +       CPDT(NCPTS,1),CPDT(NCPTS,2),INMETHOD(NCPTS),CPIN(NCPTS),
     +       CPEV(NCPTS),EWA(NCPTS),CL(NCPTS),INWS(NCPTS)
!
!  Counts and flags.
!
         If(INMETHOD(NCPTS).LE.1) NINCP=NINCP+1
         If(CPEV(NCPTS).EQ.'      ') NEVCP=NEVCP+1
         If(INMETHOD(NCPTS).GE.3.and.INMETHOD(NCPTS).NE.9)
     +                                                FDFLAG=FDFLAG+1
         If(EWA(NCPTS).EQ.-1.0.or.EWA(NCPTS).EQ.-2.0) EFLAG=9
         If(INMETHOD(NCPTS).EQ.9) CPFLAG=CPFLAG+1
!
!  Character variables are adjusted.
!
         CPID(NCPTS,1)=Adjustr(CPID(NCPTS,1))
         CPID(NCPTS,2)=Adjustr(CPID(NCPTS,2))
         CPIN(NCPTS)=Adjustr(CPIN(NCPTS))
         CPEV(NCPTS)=Adjustr(CPEV(NCPTS))
         If(CPDT(NCPTS,1).LE.0.00001) CPDT(NCPTS,1)=1.0
         If(CPDT(NCPTS,2).LE.0.00001) CPDT(NCPTS,2)=1.0
         If(CPIN(NCPTS).EQ.'  NONE'.or.CPIN(NCPTS).EQ.'  none'.or.
     +      CPIN(NCPTS).EQ.'  zero') CPIN(NCPTS)='  ZERO'
         If(CPEV(NCPTS).EQ.'  NONE'.or.CPEV(NCPTS).EQ.'  none'.or.
     +      CPEV(NCPTS).EQ.'  zero') CPEV(NCPTS)='  ZERO'
         If(CPEV(NCPTS).EQ.'  next') CPEV(NCPTS)='  NEXT'
!
!  Default for EWA(cp) from CP record field 9 is set as specified by
!  EPADJ from JD record.
!
         If(EPADJ.EQ.-1.and.Abs(EWA(NCPTS)).LT.0.001) EWA(NCPTS)=-1.0
         If(EPADJ.EQ.-2.and.Abs(EWA(NCPTS)).LT.0.001) EWA(NCPTS)=-2.0
         If(EWA(NCPTS).LT.-2.5) EWA(NCPTS)=0.0
         If(CPEV(NCPTS).EQ.'ZERO') EWA(NCPTS)=0.0
!
!  The next record is read. Data is organized after all CP records are read.
!
         Goto 1070
      Elseif(CD.EQ.'WR'.or.CD.EQ.'IF'.or.CD.eq.'CI'.or.CD.EQ.'FS') Then
         Backspace(3)
         Goto 1130
      Else
         Write(14,*)' '
         Write(14,1120) CD
1120     Format(' ERROR: Read inappropriate CD of ',A2,
     +          ' after or within CP records.')
         Write(14,5000)
         Call ERROR
      Endif
1130  If(ICHECK.GE.0) Write(14,*)'*** Finished reading CP records.'
!
!  Location relationships are specified by CPID(I,2) from CP records.
!
      Do 1160 I=1,NCPTS
         If(CPID(I,2).EQ.'   OUT'.or.CPID(I,2).EQ.'   out'.or.
     +      CPID(I,2).EQ.'      ') Then
            CPNXT(I)=-99
            Goto 1160
         Endif
         Do 1140 J=1,NCPTS
            If(CPID(I,2).EQ.CPID(J,1)) Then
                 CPNXT(I)=J
                 Goto 1160
            Endif
1140     End Do
         Write(14,*) ' '
         Write(14,1150) CPID(I,2),CPID(I,1)
1150     Format(' ERROR: Downstream control point identifier ',A6,
     +          ' [CPID(cp,2)] on CP record for ',A6,/,
     +           8x,' matches no CPID(cp,1) on CP records.') 
         Write(14,5000)
         Call ERROR
1160  End Do
!
!  Error checks.  Each control point must have a unique identifier.
!  INMETHOD(cp) must be between 0 and 10.
!
      If(ICHECK.GE.1) Then
         Do I=1,NCPTS
            If(INMETHOD(I).LT.0.or.INMETHOD(I).GT.10) Then
               Write(14,*) ' '
               Write(14,1170) CPID(I,1),INMETHOD(I)
1170           Format(' ERROR: Control point ',A6,' has an invalid',
     +                ' INMETHOD of',I3)
               Write(14,5000)
               Call ERROR
            Endif
            Do 1190 J=1,NCPTS
               If(J.EQ.I) Goto 1190
               If(CPID(I,1).EQ.CPID(J,1)) Then
                  Write(14,*) ' '
                  Write(14,1180) CPID(I,1),I,J
1180              Format(' ERROR: Identifier ',A6,' assigned to both',
     +                   ' control points:',I4,' and',I4)
                  Write(14,5000)
                  Call ERROR
               Endif
1190        End Do
            If(CPID(I,1).EQ.CPID(I,2)) Then
               Write(14,*) ' '
               Write(14,1200) CPID(I,1)
1200           Format(' ERROR: Identifier ',A6,' is assigned to both a',
     +               ' control point and its downstream control point.')
               Write(14,5000)
               Call ERROR
            Endif
         End Do
      Endif
!_____________________________________________________________________CI
!
!  Constant monthly inflow/outflow CI records for control points are read.
!
1320  Read(3,10,IOSTAT=STATUS,END=7000) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Then
         Goto 1320
      Elseif(CD.EQ.'CI') Then
         Read(3,1330) CD,CIID
1330     Format(A2,A6)
         Backspace(3)
         Backspace(3)
         If(CD.EQ.'CI'.and.CIID.EQ.'      ') Then
            Read(3,1030,IOSTAT=STATUS) CD,CIID,(CI(MT),MT=1,6)
            If(STATUS.NE.0) Goto 6000
            Read(3,1040,IOSTAT=STATUS) CD,(CI(MT),MT=7,12)
            If(STATUS.NE.0) Goto 6000
         Else
            Read(3,1340,IOSTAT=STATUS) CD,CIID,(CI(MT),MT=1,12)
1340        Format(A2,A6,12F8.0)
            If(STATUS.NE.0) Goto 6000
         Endif
         CIID=Adjustr(CIID)
         Do K=1,NCPTS
            If(CIID.EQ.CPID(K,1)) Then
               Do MT=1,12
                  CINF(K,MT)=CINF(K,MT)+(CIX*CI(MT))
               End Do
               Goto 1320
            Endif
            If(K.EQ.NCPTS) Then
               Write(14,*) ' '
               Write(14,5010) CIID,CD
               Write(14,5000)
               Call ERROR
            Endif
         End Do
      Elseif(CD.EQ.'WR'.or.CD.EQ.'IF'.or.CD.EQ.'FS') Then
         If(ICHECK.GE.0) Write(14,*)
     +      '*** Starting to read IF/WR records.'
         Backspace(3)
         Goto 1350
      Endif
!__________________________________________________________________WR,IF
!
!  Water right WR and instream flow IF records are read.
!
1350  Read(3,10,IOSTAT=STATUS,END=7000) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Then
         Goto 1350
      Elseif(CD.EQ.'WR'.or.CD.EQ.'IF'.or.CD.EQ.'FS') Then
         If(CD.EQ.'FS') Then
            Backspace(3)
            If(ICHECK.GE.0.and.FSFLAG.EQ.0) Write(14,*)
     +         '*** Reading FS records prior to IF/WR records.'
            Goto 1640
         Endif
         WRFLAG=9
         Backspace(3)
         NWRTS=NWRTS+1
         I=NWRTS
         WRSYS(1,1)=0.0
         WRSYS(1,2)=0.0
         WRSYS(1,3)=0.0
         SR=0
         SWR=0
         LAKESD=0
         FSC=0
!
!  Instream flow requirement IF record is read.
!
         If(CD.EQ.'IF') Then
            NIF=NIF+1
            IFFLAG2=0
            Read(3,1420,IOSTAT=STATUS) CD,CP,AMT,USETYPE,WRNUM(I,7),
     +            IFFLAG2,WRNUM(I,10),IFMETH(I),N,DINDEX(I),WRID(I)
1420        Format(A2,A6,F8.0,2x,A6,I8,2I2,3I4,A16)
            If(STATUS.NE.0) Goto 6000
            If(ICHECK.EQ.4) Write(14,1420) CD,CP,AMT,USETYPE,WRNUM(I,7),
     +                IFFLAG2,WRNUM(I,10),IFMETH(I),N,DINDEX(I),WRID(I)
            If(IFFLAG2.NE.0) WRIDS(I,2)="IFRESREL"
            If(IFMETH(I).EQ.0) IFMETH(I)=1
            RCP=CP
            RFIDWR = "      "
            WSHED(I)=0.0
            WRIDS(I,1)="IF#IF*IF"
            WRID(I)=Adjustr(WRID(I))
            USETYPE=Adjustr(USETYPE)
            CP=Adjustr(CP)
            If(IFMETH(I).EQ.3.or.Abs(IFMETH(I)).EQ.4) WRNUM(I,5)=2
!
!        IF record error checks.
!
            If(WRNUM(I,10).LT.0.or.WRNUM(I,10).GT.3) Then
               Write(14,*)' '
               Write(14,1430) WRNUM(I,10),Adjustl(WRID(I))
1430           Format(' ERROR: WRNUM(wr,10) of',I3,' in IF record',
     +                ' field 6 is not valid. IF right: ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If(Abs(IFMETH(I)).GT.4) Then
               Write(14,*)' '
               Write(14,1440) IFMETH(I),Adjustl(WRID(I))
1440           Format(' ERROR: IFMETH of',I3,' is not valid.',
     +                ' IF right: ',A16)
               Write(14,5000)
               Call ERROR
            Endif
!
!  Water right WR record is read.
!
         Elseif(CD.EQ.'WR') Then
            Read(3,1450,IOSTAT=STATUS) CD,CP,AMT,USETYPE,WRNUM(I,7),
     +                                WRNUM(I,5),RFMETH(I)
1450        Format(A2,A6,F8.0,2x,A6,I8,I4,I4)
            If(STATUS.NE.0) Goto 6000
            Backspace(3)
            If(RFMETH(I).GE.3) Then
               If(IDSET.EQ.1) Then
                  Read(3,1460,IOSTAT=STATUS) CD,CP,AMT,USETYPE,
     +                       WRNUM(I,7),WRNUM(I,5),RFMETH(I),RFIDWR,RCP,
     +                       N,DINDEX(I),WRID(I),WRIDS(I,1),WRIDS(I,2)
1460             Format(A2,A6,F8.0,2x,A6,I8,2I4,2x,A6,2x,A6,2I4,A16,2A8)
               Elseif(IDSET.EQ.2) Then
                  Read(3,1470,IOSTAT=STATUS) CD,CP,AMT,USETYPE,
     +                       WRNUM(I,7),WRNUM(I,5),RFMETH(I),RFIDWR,RCP,
     +                       N,DINDEX(I),WRID(I),WRIDS(I,1),WRIDS(I,2)
1470              Format(A2,A6,F8.0,2x,A6,I8,2I4,2x,A6,
     +                   2x,A6,2I4,32x,A16,2A8)
               Endif
               If(STATUS.NE.0) Goto 6000
               RFAC = 0.0
               If(ICHECK.EQ.4) Write(14,1460) CD,CP,AMT,USETYPE,
     +              WRNUM(I,7),WRNUM(I,5),RFMETH(I),RFIDWR,RCP,
     +              DINDEX(I),WRID(I),WRIDS(I,1),WRIDS(I,2)
            Else
               If(IDSET.EQ.1) Then
                  Read(3,1480,IOSTAT=STATUS) CD,CP,AMT,USETYPE,
     +                       WRNUM(I,7),WRNUM(I,5),RFMETH(I),RFAC,RCP,
     +                       N,DINDEX(I),WRID(I),WRIDS(I,1),WRIDS(I,2)
1480              Format(A2,A6,F8.0,2x,A6,I8,2I4,F8.0,2x,A6,2I4,A16,2A8)
               Elseif(IDSET.EQ.2) Then
                  Read(3,1490,IOSTAT=STATUS) CD,CP,AMT,USETYPE,
     +                       WRNUM(I,7),WRNUM(I,5),RFMETH(I),RFAC,RCP,
     +                       N,DINDEX(I),WRID(I),WRIDS(I,1),WRIDS(I,2)
1490          Format(A2,A6,F8.0,2x,A6,I8,2I4,F8.0,2x,A6,2I4,32x,A16,2A8)
               Endif
               If(STATUS.NE.0) Goto 6000
               RFIDWR = "      "
               If(ICHECK.EQ.4) Write(14,1480) CD,CP,AMT,USETYPE,
     +                WRNUM(I,7),WRNUM(I,5),RFMETH(I),RFAC,RCP,N,
     +                DINDEX(I),WRID(I),WRIDS(I,1),WRIDS(I,2)
            Endif
            WRID(I)=Adjustr(WRID(I))
            WRIDS(I,1)=Adjustr(WRIDS(I,1))
            WRIDS(I,2)=Adjustr(WRIDS(I,2))
            USETYPE=Adjustr(USETYPE)
            CP=Adjustr(CP)
            RCP=Adjustr(RCP)
            RFIDWR=Adjustr(RFIDWR)
!
!           Error checks.
!
            If(RFMETH(I).GT.4.or.RFMETH(I).LT.0) Then
               Write(14,*) ' '
               Write(14,1500) RFMETH(I),Adjustl(WRID(I))
1500           Format(' ERROR: Invalid RFMETH of',I3,
     +                ' in WR field 7 for water right ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If(WRNUM(I,5).LT.-3.or.WRNUM(I,5).GT.7) Then
               Write(14,*) ' '
               Write(14,1510) WRNUM(I,5),Adjustl(WRID(I))
1510           Format(' ERROR: Invalid Type of',I3,
     +                ' in WR field 6 for water right ',A16)
               Write(14,5000)
               Call ERROR
            Endif
         Endif
!
!  The following code extending to just before statement 1640 organizes
!  the water rights data read above from the WR or IF record.
!
!  Water right is connected to flow switch FS record
!
         If(N.GT.0) Then
           If(N.GT.MAXFS) Then
              Write(14,1514) N,CD,Adjustl(WRID(I)),MAXFS,N
1514          Format(' WARNING: N of',I3,' from ',A2,' record for',
     +               ' right ',A16,/,10x,'exceeds number of FS records',
     +               ' of',I3,' indicating that',/,10x,'IFFLAG2 likely',
     +               ' has not been changed to new IF record format.',/,
     +              10x,'IFFLAG2 is set to',I3,' and N is set to zero.')
               WRIDS(I,2)="IFRESREL"
               Goto 1516
            Endif
            FSC=FSC+1
            FSN(I,FSC)=N
         Endif
!
!  Integer identifier is assigned to a firm yield water right.
!
1516     If(FYLEVEL.NE.0) Then
            If(FYWRID.EQ.WRID(I).and.FYWRID.NE.'                ') Then
               FYN=FYN+1
               FYWR(FYN)=I
            Elseif(FYGROUP.NE.'        ') Then
               If(FYGROUP.EQ.WRIDS(I,1).or.FYGROUP.EQ.WRIDS(I,2)) Then
                  FYN=FYN+1
                  FYWR(FYN)=I
               Endif
            Endif
         Endif
!
!  Blank WR record field 6 is assigned WRT=WRNUM(I,5) of 1.
!  Hydropower right is assigned WRT=WRNUM(I,5) of either -1, or -3.
!
         If(WRNUM(I,5).EQ.0) WRNUM(I,5) = 1
         If(WRNUM(I,5).EQ.5) WRNUM(I,5) = -1 
         If(WRNUM(I,5).EQ.6) WRNUM(I,5) = -3
!
!  Control point identifiers are assigned integer identifiers and checked
!  for match on CP records.
!
         Do K=1,NCPTS
            If(CP.EQ.CPID(K,1)) Then
               WRNUM(I,1)=K
               If(RCP.EQ.'      ') WRNUM(I,3)=CPNXT(K)
               Goto 1520
            Endif
            If(K.EQ.NCPTS) Then
               Write(14,*) ' '
               Write(14,5010) CP,CD
               Write(14,5000)
               Call ERROR
            Endif
         End Do
1520     Do 1530 K=1,NCPTS
            If(RCP.EQ.CPID(K,1)) Then
               WRNUM(I,3)=K
               Goto 1540
            Elseif(RCP.EQ.'   OUT') Then
               WRNUM(I,3)=-99
               Goto 1540
            Endif
            If(K.EQ.NCPTS) Then
               If(RCP.EQ.'      ') Goto 1530
               Write(14,*) ' '
               Write(14,5010) RCP,CD
               Write(14,5000)
               Call ERROR
            Endif
1530     End Do
!
!  Water use integer identifier is assigned to right.
!
1540     If(USETYPE.EQ.'XMONTH'.or.USETYPE.EQ.'xmonth') Then
            WRNUM(I,2)=-99
         Else
            Do 1560 K=1,NUSES
               If(USETYPE.EQ.USEID(K)) Then
                  WRNUM(I,2)=K
                  Goto 1570
               Endif
               If(K.EQ.NUSES) Then
                  Write(14,*) ' '
                  Write(14,1550) USETYPE,NWRTS
1550              Format(' ERROR: Water use identifier ',A6,
     +                   ' from WR or IF record',I4,/,8x,
     +                   ' matches no identifier on the UC records.')
                  Write(14,5000)
                  Call ERROR
               Endif
1560        End Do
         Endif
!
!  Priorities and multipliers from UP records are applied.
!
1570     K=WRNUM(I,2)
         If(K.GT.0) Then
            If(UPFLAG(K).EQ.9) Then
              If(USEP(K).NE.0) Then
                  WRNUM(I,7)=USEP(K)
               Elseif(USEADD(K).NE.0) Then
                  WRNUM(I,7)=WRNUM(I,7)+USEADD(K)
               Elseif(USEMUL(K).NE.0.00001) Then
                  WRNUM(I,7)=WRNUM(I,7)*USEMUL(K)
               Elseif(USEFAC(K).GE.0.00001) Then
                  AMT=AMT*USEFAC(K)
               Elseif(USEFAC(K).LT.-0.00001) Then
                  AMT=0.0
               Endif
            Endif
         Endif
!
!  Return flow integer identifier is assigned to right.
!
         If(RFIDWR.NE."      ") Then
            Do 1590 K=1,NRFS
               If(RFIDWR.EQ.RFID(K)) Then
                  IRF(I)=K
                  Goto 1600
               Endif
               If(K.EQ.NRFS) Then
                  Write(14,*) ' '
                  Write(14,1580) RFIDWR,NWRTS
1580              Format(' ERROR: Return flow identifier '
     +                    ,A6,' from WR record',I4,/,8x,
     +                   'matches no identifier on the RF records.')
                  Write(14,5000)
                  Call ERROR
               Endif
1590        End Do
         Endif
!
!  An integer identifier SWR is assigned to each system water right, 
!  defined as having multiple reservoirs or type -3, -1, 2, or 3.
!  SWRFLAG is a mechanism to assign SWR status due to right type here
!  and due to multiple reservoirs when WS records are read without
!  incrementing SWR by more than one for any one water right.
!
1600     SWRFLAG=0
         If(WRNUM(I,5).LE.-1.or.WRNUM(I,5).EQ.2.or.WRNUM(I,5).EQ.3) Then
            SWRCNT=SWRCNT+1
            SWR=SWRCNT
            SWRFLAG=-99
            If(SWR.GT.MAXSWR) Then
               Write(14,*)' '
               Write(14,1610) MAXSWR,SWR
1610           Format(' ERROR: Subroutine READDAT set MAXSWR at',I3,
     +          ' but tried to read WR/IF records for at least',/,6x,I4,
     +          ' system rights, indicating a problem with hydropower',
     +          ' or multireservoir rights.')
               Write(14,5000)
               Call ERROR
            Endif
         Endif
!
!  Determine whether to include right in output file.
!
         If(OUTWR.EQ.-1.and.WRNUM(I,5).GE.0) Then
            WRNUM(I,6)=1
            NWROUT=NWROUT+1
            Goto 1640
         Endif
         If(OUTWR.EQ.-2) Then
            WRNUM(I,6)=1
            NWROUT=NWROUT+1
            Goto 1640
         Endif
         If(WRIDS(I,1).NE."IF#IF*IF") Then
            If(OUTWR.EQ.-3.and.WRNUM(I,5).GT.0) Then
               WRNUM(I,6)=1
               NWROUT=NWROUT+1
               Goto 1640
            Endif
         Endif
         If(OUTWR.EQ.-4.and.WRIDS(I,1).EQ."IF#IF*IF") Then
            WRNUM(I,6)=1
            NWROUT=NWROUT+1
            Goto 1640
         Endif
         If(OUTWR.EQ.-5.and.WRNUM(I,5).LT.0) Then
            WRNUM(I,6)=1
            NWROUT=NWROUT+1
            Goto 1640
         Endif
         If(I.LE.OUTWR) Then
            WRNUM(I,6)=1
            NWROUT=NWROUT+1
            Goto 1640
         Endif
         If(NWOUT.GT.0) Then
            Do K=1,NWOUT
               If(WRID(I).EQ.WROUT(K)) Then
                  WRNUM(I,6)=1
                  NWROUT=NWROUT+1
                  Goto 1640
               Endif
           End Do
         Endif
         If(NGOUT.GT.0) Then
            Do K=1,NGOUT
               If(WRIDS(I,1).EQ.GROUP(K).or.WRIDS(I,2).EQ.GROUP(K)) Then
                  If(WRNUM(I,6).NE.1) NWROUT=NWROUT+1
                  WRNUM(I,6)=1
                  Goto 1640
               Endif
            End Do
         Endif
!
!__________________________SO,PX,TO,TS,ML,FS,WS,HP,OR______________________
!
!  Check if water right includes reservoir storage or other options
!  specified by SO, PX, TO, TS, ML, FS, WS, HP, and OR records.
!
1640     Read(3,10,IOSTAT=STATUS,END=7000) CD
         If(STATUS.NE.0) Goto 6000
         If(CD.NE.'**'.and.CD.NE.'WS'.and.CD.NE.'TO'.and.CD.NE.'PX'.and.
     +      CD.NE.'ML'.and.CD.NE.'SO'.and.CD.NE.'TS'.and.CD.NE.'FS'.and.
     +      CD.NE.'BU') Then
            Backspace(3)
            Goto 2310
         Elseif(CD.EQ.'**') Then
            Goto 1640
!__________________________________________________________________PX,AX
!
!  Read priority circumvention PX record.
!
         Elseif(CD.EQ.'PX') Then
            Backspace(3)
            Read(3,1660,IOSTAT=STATUS) CD,J,XAX,K,L
1660        Format(A2,I6,F8.0,I8,8x,I8)
            If(STATUS.NE.0) Goto 6000
            If(XAX.NE.0.0.or.K.GT.0.or.L.NE.0) Then
               Backspace(3)
               XPX=XPX+1
               WRNUM(I,8)=XPX
               If(Abs(XAX).GE.0.00001) XAXFLAG(XPX)=9
               If(L.GT.0) XPCOUNT=XPCOUNT+1
!
!           PX record is read.
!
               Read(3,1670,IOSTAT=STATUS) CD,J,XAX,XCP(XPX),XCPID,
     +                     XP(XPX),XPR(XPX),XPRIORITY(XPX),XPOUT(XPX),
     +                     WRID1(XPX),WRID2(XPX),WRID3(XPX)
1670           Format(A2,I6,F8.0,I8,2x,A6,4I8,A16,2A8)
               If(STATUS.NE.0) Goto 6000
!
!           Control point limit option is checked.
!
               If(XCP(XPX).GT.4.or.XCP(XPX).LT.0) Then
                  Write(14,1680) XCP(XPX),Adjustl(WRID(I))
1680              Format(' ERROR: Invalid XCP option of',I3,' on PX',
     +                   ' record field 4 for water right ',A16)
                  Call ERROR
               Endif
               If(XCP(XPX).NE.0) Then
                  If(XCPID.EQ.'      ') Then
                     Write(14,1690) Adjustl(WRID(I))
1690                 Format(' ERROR: XCPID missing in PX record',
     +                      ' field 5 for water right ',A16)
                     Call ERROR
                  Else
                     XCPID=Adjustr(XCPID)
                     Do N=1,NCPTS
                        If(XCPID.EQ.CPID(N,1)) Then
                           XCPI(XPX)=N
                           Goto 1700
                        Endif
                        If(N.EQ.NCPTS) Then
                           Write(14,5010) XCPID,CD
                           Write(14,5000)
                           Call ERROR
                        Endif
                     End Do
                  Endif
1700           Endif
!
!           Transient priority (XP) data are organized and checked.
!
               If(XP(XPX).GT.2.or.XP(XPX).LT.0) Then
                  Write(14,1710) XP(XPX),Adjustl(WRID(I))
1710              Format(' ERROR: Invalid XP option of',I3,' on PX',
     +                   ' record field 6 for water right ',A16)
                  Call ERROR
               Endif
               If(XPR(XPX).LT.0.or.XPR(XPX).GT.2) Then
                  Write(14,1720) XPR(XPX),Adjustl(WRID(I))
1720              Format(' ERROR: Invalid XPR option of',I3,' on PX',
     +                   ' record field 6 for water right ',A16)
                  Call ERROR
               Endif
               If(XPR(XPX).GT.0.and.XPRIORITY(XPX).LT.WRNUM(I,7)) Then
                  Write(14,1730) XPRIORITY(XPX),WRNUM(I,7),
     +                           Adjustl(WRID(I))
1730              Format(' ERROR: PX record priority of ',I8,
     +                   ' is less than WR record priority of '
     +                   ,I8,/,8x,'for water right ',A16)
                  Call ERROR
               Endif
               If(RFMETH(I).EQ.2.or.RFMETH(I).EQ.4) Then
                  If(XP(XPX).GT.0) Then
                     Write(14,1740) XP(XPX),Adjustl(WRID(I))
1740                 Format(' WARNING: A next-month return flow option',
     +                      ' is used with PX record field 5 option',I2,
     +                      /,9x,' for water right ',A16)
                  Endif
               Endif
               If(XP(XPX).EQ.1.and.XPR(XPX).GT.0) Then
                  Write(14,1750) XP(XPX),Adjustl(WRID(I))
1750              Format(' WARNING: A non-zero XPR does not pertain to',
     +                   ' XP option 1 (PX record fields 5 and 6).'/,9x,
     +                   ' water right ',A16)
               Endif
            Endif
!
!           DUAL option from SO record field 14 or PX record field 2.
!
            If(J.GT.0) Then
               If(DUAL(I).GT.0.and.DUAL(I).NE.J) Then
                  Write(14,1760) J,DUAL(I),Adjustl(WRID(I)),J
1760              Format(' WARNING: DUAL is',I3,' on PX record and',I3,
     +                   ' on SO record for right ',A16,/,10x,I3,
     +                   ' is used in the model.')
               Endif
               DUAL(I)=J
               If(J.EQ.1.or.J.EQ.4) DUAL1=DUAL1+1
            Endif
!
!  Read monthly streamflow availability multiplier factors AX record.
!
            If(XAX.LE.-0.001) Then
               Read(3,1770,IOSTAT=STATUS,END=7000) CD,
     +             (XA(XPX,J),J=1,12),XAMIN(XPX),XAMAX(XPX)
1770           Format(A2,14F8.0)
               If(STATUS.NE.0) Goto 6000
            Endif
            If(XAX.GT.0.000001) Then
               Do J=1,12
                  XA(XPX,J)=XAX
               End Do
               XAMIN(XPX)=0.00
               XAMAX(XPX)=9999999000000.0
            Endif
            Goto 1640
!_____________________________________________________________________BU
!
!  Read back-up right BU record.
!
         Elseif(CD.EQ.'BU') Then
            Backspace(3)
            Read(3,1780,IOSTAT=STATUS) CD,BU,X,BUWRID,BUGROUP
1780        Format(A2,I6,F8.0,A16,A8)
            If(STATUS.NE.0) Goto 6000
            If(WRTO(I).EQ.0) Then
               ISO=ISO+1
               WRTO(I)=ISO
            Endif
            N=WRTO(I)
            If(Abs(X).LT.0.000001) X=1.0
            BUX(N)=X
            If(TOFLAG(N).EQ.90) Then
               If(BU.EQ.9) TOFLAG(N)=99
               If(BU.LE.2) TOFLAG(N)=98
            Else
               If(BU.EQ.9) TOFLAG(N)=9
               If(BU.LE.2) TOFLAG(N)=8
            Endif
            If(I.GT.1.and.BUWRID.EQ.'                '.and.
     +                   BUGROUP.EQ.'        ') BUWR(N)=I-1
            BUID(N)=Adjustr(BUWRID)
            BUG(N)=Adjustr(BUGROUP)
            If(BU.GT.2.and.BU.NE.9) Then
               Write(14,1790) BU,Adjustl(WRID(I))
1790           Format(' ERROR: BU of',I3,' on BU record is not'
     +                ' valid. Right: ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            Goto 1640
!_____________________________________________________________________SO
!
!  Read supplemental options SO record.
!
         Elseif(CD.EQ.'SO') Then
            Backspace(3)
            Read(3,1800,IOSTAT=STATUS) CD,WSHED(I),MONDEP(I),ANNDEP(I),
     +                      ACPID,BACKUP,MRW(I),ARW(I),ISHT(I),ADL(I),
     +                      LM(I,1),LM(I,2),NOTFLAG,J
1800        Format(A2,F6.0,2F8.0,2x,A6,2x,A6,2F8.0,I8,F8.0,2I8,2x,A6,I8)
            If(STATUS.NE.0) Goto 6000
            If(J.GT.0.and.DUAL(I).EQ.0) DUAL(I)=J
            If(J.EQ.1.or.J.EQ.4) DUAL1=DUAL1+1
            ACPID=Adjustr(ACPID)
            BACKUP=Adjustr(BACKUP)
            If(NOTFLAG.EQ.'NOCLWR') NOTF(I)=999
            If(NOTFLAG.EQ.'NORFCL') NOTF(I)=99
            If(NOTFLAG.EQ.'IFNOTA') NOTF(I)=-9
            If(NOTFLAG.EQ.'NONSFD') NOTF(I)=-8
            If(NOTFLAG.EQ.'NEGSFD') NOTF(I)=-88
            If(LM(I,1).EQ.0) LM(I,1)=1
            If(LM(I,2).EQ.0) LM(I,2)=12
            If(ACPID.NE.'      ') Then
               Do 1810 K=1,NCPTS
                  If(ACPID.EQ.CPID(K,1)) Then
                     WRNUM(I,10)=K
                     Goto 1820
                  Endif
1810           End Do
               If(K.EQ.NCPTS) Then
                    Write(14,*) ' '
                    Write(14,5010) ACPID,CD
                    Write(14,5000)
                    Call ERROR
               Endif
            Endif
1820        If(BACKUP.EQ.'BACKUP'.or.BACKUP.EQ.'BFIRST'
     +         .or.BACKUP.EQ.'RETURN') Then
               If(WRTO(I).EQ.0) Then
                  ISO=ISO+1
                  WRTO(I)=ISO
               Endif
               N=WRTO(I)
               If(TOFLAG(N).EQ.90) Then
                  If(BACKUP.EQ.'BACKUP') TOFLAG(N)=-99
                  If(BACKUP.EQ.'BFIRST') TOFLAG(N)=-98
                  If(BACKUP.EQ.'RETURN') TOFLAG(N)=97
               Else
                  If(BACKUP.EQ.'BACKUP') TOFLAG(N)=-9
                  If(BACKUP.EQ.'BFIRST') TOFLAG(N)=-8
                  If(BACKUP.EQ.'RETURN') TOFLAG(N)=7
               Endif
               If(I.GT.1.and.BACKUP.NE.'        ') BUWR(N)=I-1
            Elseif(BACKUP.NE.'      ') Then
               Write(14,*) ' '
               Write(14,1830) BACKUP
1830           Format(' ERROR: SO field 6 is limited to ',
     +                'BACKUP, BFIRST, or RETURN. Read: ',A6)
               Write(14,5000)
               Call ERROR
            Endif
            If(ISHT(I).LT.0.or.ISHT(I).GT.9) Then
               Write(14,*) ' '
               Write(14,1840) ISHT(I),Adjustl(WRID(I))
1840           Format(' ERROR: ISHT of ',I3,' in SO field 9 is invalid',
     +                ' Right: ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            Goto 1640
!_____________________________________________________________________TO
!
!  Read target options TO record(s).
!
         Elseif(CD.EQ.'TO') Then
            Backspace(3)
            If(WRTO(I).EQ.0) Then
               ISO=ISO+1
               WRTO(I)=ISO
            Endif
            N=WRTO(I)
            Read(3,1850,IOSTAT=STATUS) CD,TOTARGET(N),FACT(N),TOCOMB(N),
     +           TOLIM(N,1),TOLIM(N,2),TOFLOW,TORES(N),TOWR(N),TOCONT
1850        Format(A2,I6,F8.0,5x,A3,2F8.0,2x,A6,2x,A6,A16,A8)
            If(STATUS.NE.0) Goto 6000
1860        TOCOMB(N)=Adjustr(TOCOMB(N))
            TOFLOW=Adjustr(TOFLOW)
            TORES(N)=Adjustr(TORES(N))
            TOWR(N)=Adjustr(TOWR(N))
            TOCONT=Adjustr(TOCONT)
            If(TOCONT.EQ.'    CONT') Then
               If(TOFLAG(N).EQ.9) Then
                  TOFLAG(N)=99
               Elseif(TOFLAG(N).EQ.8) Then
                  TOFLAG(N)=98
               Else
                  TOFLAG(N)=90
               Endif
            Endif
            If(TOTARGET(N).EQ.0.or.TOTARGET(N).LT.-12.or.
     +         TOTARGET(N).GT.14) Then
               Write(14,*) ' '
               Write(14,1870) TOTARGET(N), Adjustl(WRID(I))
1870           Format(' ERROR: TOTARGET of ',I3,' is not valid.',
     +              ' Water right: ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If(TOTARGET(N).EQ.10.and.TOCOMB(N).EQ.'LIM') Then
               Write(14,*) ' '
               Write(14,1880) Adjustl(WRID(I))
1880           Format(' ERROR: TOTARGET=10 combined with TOCOMB=LIM',
     +                ' is not valid. Water right: ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If(TOCONT.NE.'        '.and.TOCONT.NE.'    CONT') Then
               Write(14,*) ' '
               Write(14,1890) TOCONT
1890           Format(' ERROR: TO field 10 is limited to ',
     +                'blank or CONT. Read: ',A8)
               Write(14,5000)
               Call ERROR
            Endif
            If(Abs(TOTARGET(N)).LE.3) Then
               If(TOFLOW.EQ.'      ') TOFLOW=CPID(WRNUM(I,1),1)
               Do 1900 K=1,NCPTS
                  If(TOFLOW.EQ.CPID(K,1)) Then
                     TOCP(N)=K
                     Goto 1910
                  Endif
                  If(K.EQ.NCPTS) Then
                     Write(14,*) ' '
                     Write(14,5010) TOFLOW,CD
                     Write(14,5000)
                     Call ERROR
                  Endif
1900           End Do
            Endif
1910        If(Abs(FACT(N)).LE.0.00001) FACT(N)=1.0
            If(Abs(TOLIM(N,2)).LE.0.00001) TOLIM(N,2)=99000000.0
            If(TOCOMB(N).EQ.'   ') TOCOMB(N)='SET'
            If(TOCOMB(N).NE.'SET'.and.TOCOMB(N).NE.'ADD'.and.TOCOMB(N).
     +      NE.'MAX'.and.TOCOMB(N).NE.'MIN'.and.TOCOMB(N).NE.'LIM'.and.
     +      TOCOMB(N).NE.'SUB'.and.TOCOMB(N).NE.'DIV'.and.TOCOMB(N).NE.
     +         'MUL') Then
               Write(14,*) ' '
               Write(14,1920) TOCOMB(N),WRID(I)
1920           Format(' ERROR: TOCOMB of ',A3, ' on TO record is not',
     +                ' valid. Water right: ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If((Abs(TOTARGET(N)).EQ.4.or.Abs(TOTARGET(N)).EQ.5).and.
     +          TORES(N).EQ.'      ') Then
               Write(14,*) ' '
               Write(14,1930) Adjustl(WRID(I))
1930           Format(' ERROR: Reservoir identifier is missing from',
     +                ' TO record field 8 for water right ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If(Abs(TOTARGET(N)).GE.6.and.TOTARGET(N).NE.10.
     +          and.TOWR(N).EQ.'                ') Then
               Write(14,*) ' '
               Write(14,1940) Adjustl(WRID(I))
1940           Format(' ERROR: Water right identifier is missing from',
     +                ' TO record field 9 for water right ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If(TORES(N).NE.'      ') NTORES=N
            If(TOWR(N).NE.'                ') NTOWR=N
            If(TOCONT.EQ.'    CONT') Then
               N=N+1
               ISO=N
               Read(3,1850,IOSTAT=STATUS) CD,TOTARGET(N),FACT(N),
     +                    TOCOMB(N),TOLIM(N,1),TOLIM(N,2),TOFLOW,
     +                    TORES(N),TOWR(N),TOCONT
               If(STATUS.NE.0) Goto 6000
               If(CD.NE.'TO') Then
                  Write(14,*) ' '
                  Write(14,1950) CD,Adjustl(WRID(I))
1950              Format(' ERROR: Read CD of ',A2,' instead of TO ',
     +            'for a continuation TO record for water right: ',A16)
                  Write(14,5000)
                  Call ERROR
               Endif
               Goto 1860
            Endif
            Goto 1640
!_____________________________________________________________________ML
!
!  Read monthly limits on streamflow depletion ML record.
!
         Elseif(CD.EQ.'ML') Then
            Backspace(3)
            ML=ML+1
            Read(3,1960,IOSTAT=STATUS) CD,(MSD(ML,MT),MT=1,12)
1960        Format(A2,F6.0,11F8.0)
            If(STATUS.NE.0) Goto 6000
            WRNUM(I,4)=ML
            Goto 1640
!_____________________________________________________________________TS
!
!  Read time series of monthly targets from TS records.
!
         Elseif(CD.EQ.'TS') Then
            Backspace(3)
            If(SERIES(I).EQ.0) Then
               TSCOUNT=TSCOUNT+1
               SERIES(I)=TSCOUNT
            Endif
            TSC=' '
            K=0
            TSA=1.0
            TSB=0.0
            Read(3,1970,IOSTAT=STATUS) CD,TSC,TSL,K,TSA,TSB
1970        Format(A2,A1,2x,A3,I8,2F8.0)
            If(STATUS.NE.0) Goto 6000
            If(TSL.EQ.'max'.or.TSL.EQ.'MAX'.or.TSL.EQ.'Max') Then
               TSCOM(TSCOUNT)='MAX' 
            Elseif(TSL.EQ.'min'.or.TSL.EQ.'MIN'.or.TSL.EQ.'Min') Then
               TSCOM(TSCOUNT)='MIN'
            Elseif(TSL.EQ.'add'.or.TSL.EQ.'ADD'.or.TSL.EQ.'Add') Then
               TSCOM(TSCOUNT)='ADD'
            Elseif(TSL.EQ.'sub'.or.TSL.EQ.'SUB'.or.TSL.EQ.'Sub') Then
               TSCOM(TSCOUNT)='SUB'
            Elseif(TSL.EQ.'mul'.or.TSL.EQ.'MUL'.or.TSL.EQ.'Mul') Then
               TSCOM(TSCOUNT)='MUL'
            Elseif(TSL.EQ.'sdl'.or.TSL.EQ.'SDL'.or.TSL.EQ.'Sdl') Then
               TSCOM(TSCOUNT)='SDL'
            Elseif(TSL.EQ.'   ') Then
               TSCOM(TSCOUNT)='   '
            Else
               Write(14,*)
               Write(14,1980) TSL,Adjustl(WRID(I))
1980           Format(' ERROR: ',A3,' for TSL from TS record is not',
     +                ' valid. Water right: ',A16)
               Write(14,5000)
               Call ERROR
            Endif
!
!  Repeat a previously entered time series for a TS-repeat TSR record.
!
            If(TSC.EQ.'R') Then
               If(K.EQ.0) K=TSCOUNT-1
               If(K.LT.0.or.K.GT.TSCOUNT) Then
                  Write(14,*)
                  Write(14,1990) K,Adjustl(WRID(I))
1990              Format(' ERROR: ',I3,' for K in TSR record ',
     +                   'field 3 is not valid. Water right: ',A16)
                  Write(14,5000)
                  Call ERROR
               Endif
               If(TSA.EQ.0) TSA=1.0
               Do YR=1,NYRS
                  YEAR=YRST+YR-1
                  Do MT=1,12
                     QTS(YR,TSCOUNT,MT)=TSA*QTS(YR,K,MT)+TSB
                  End Do
               End Do
!
!  Otherwise, without TS-repeat TSR, read a time series.
!
            Else
               Backspace(3)
               YRLAST=YRST+NYRS-1
               Do YR=1,NYRS
                  YEAR=YRST+YR-1
                  Read(3,2000,IOSTAT=STATUS) CD,TSYR1,TSYR2,
     +                              (QTS(YR,TSCOUNT,MT),MT=1,12)
2000              Format(A2,6X,I4,I4,12F8.0)
                  If(STATUS.NE.0) Goto 6000
                  If((CD.NE.'TS'.and.CD.NE.'  ').or.TSYR1.GT.TSYR2.or.
     +            TSYR2.LT.YEAR.or.TSYR2.GT.YRLAST.or.TSYR2.LT.YRST)Then
                     Write(14,*)
                     Write(14,2010) YEAR,ADJUSTL(WRID(I)),CD,TSL,TSYR1,
     +                              TSYR2
2010                 Format(' ERROR: TS record is not valid for year ',
     +               I4,' for water right ',A16,/,8X,'CD,TSYR1,TSYR2',
     +                  ' read as follows: ',8x,A2,3X,A3,I4,I4)
                     Write(14,5000)
                     Call ERROR
                  Endif
                  If(YEAR.LT.TSYR2) Backspace(3)
               End Do
            Endif
            Goto 1640
!_____________________________________________________________________FS
!
!  Read flow switch FS record.
!
         Elseif(CD.EQ.'FS') Then
            Backspace(3)
            FS=FS+1
            FSFLAG=FS
            If(WRFLAG.GT.0) Then
               If(FSC.GE.1) Then
                  IF(FSN(I,FSC).NE.FS) FSC=FSC+1
               Else
                  FSC=FSC+1
               Endif
               FSN(I,FSC)=FS
            Else
               FSC=FSC+1
            Endif
            Read(3,2020,IOSTAT=STATUS)CD,N,FSV,FSCP,FSX(FS,1),FSX(FS,2),
     +                FSX(FS,3),FSX(FS,4),FSI(FS,2),FSI(FS,3),FSI(FS,4),
     +                FSI(FS,5),FSI(FS,6),FSI(FS,7),FSI(FS,8),FSI(FS,9)
2020        Format(A2,2I3,2x,A6,4F8.0,8I4)
            If(STATUS.NE.0) Goto 6000
            If(FSV.EQ.0) FSV=1
            FSI(FS,1)=FSV
            If(FSCP.EQ.'      '.and.WRFLAG.GT.0) Then
               FSI(FS,10)=WRNUM(I,1)
            Else
               FSCP=Adjustr(FSCP)
               Do K=1,NCPTS
                  If(FSCP.EQ.CPID(K,1)) Then
                     FSI(FS,10)=K
                     Goto 2030
                  Endif
                  If(K.EQ.NCPTS) Then
                     Write(14,5010) FSCP,CD
                     Write(14,5000)
                     Call ERROR
                  Endif
               End Do
2030        Endif
            If(FSX(FS,3).LT.0.00001) FSX(FS,3)=0.0
            If(FSX(FS,4).LE.0.00001) FSX(FS,4)=9000000000.0
            If(FSI(FS,4).EQ.0) FSI(FS,4)=9000000
            If(N.NE.0.and.N.NE.FS) Then
               Write(14,2035) N,FS
2035           Format(' ERROR: Incorrect identifier N of',I3,
     +                ' in FS record field 2 should be',I3)
               Call ERROR
            Endif
            If(FSI(FS,1).LT.0.or.FSI(FS,1).GT.6) Then
               Write(14,2040) FSI(FS,1)
2040          Format(' ERROR: FSV of',I2,' in FS field 3 is not valid.')
               Call ERROR
            Endif
            If(FSI(FS,2).LT.0.or.FSI(FS,2).GT.4) Then
               Write(14,2044) FSI(FS,2)
2044           Format(' ERROR: FSI(FS,1) of',I2,' in FS field 9 is',
     +                ' not valid.')
               Call ERROR
            Endif
            If(ICHECK.GE.1.and.FSI(FS,2).EQ.0) Then
               Write(14,2046) FS
2046           Format(' WARNING: FSI(FS,2) in field 9 of FS record',I3,
     +                ' is zero, indicating 2FSV and 2FSC tables are',
     +                ' of interest,',/,10x,'but targets or releases',
     +                ' are not affected in the SIM simulation.')
            Endif
            If(FSX(FS,4).LT.FSX(FS,3)) Then
               Write(14,2050) FSX(FS,4),FSX(FS,3),Adjustl(WRID(I))
2050           Format(' ERROR: Upper bound of',F8.0,' is less than',
     +                ' lower bound of',F8.0,' on FS record for ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If(FSI(FS,4).LT.FSI(FS,3)) Then
               Write(14,2055) FSI(FS,4),FSI(FS,3),Adjustl(WRID(I))
2055           Format(' ERROR: Upper bound of',I3,' is less than',
     +                ' lower bound of',I3,' on FS record for ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If(FSI(FS,5).LE.0.and.FSI(FS,6).NE.0) Then
               Write(14,2060)  WRID(I)
2060           Format(' WARNING: FS record field 13 beginning month is',
     +                ' not used if FS field 12 is zero. Right: ',A16)
            Endif
            If(FSI(FS,6).GT.0.and.FSI(FS,7).LE.0) Then
               Write(14,2070)  WRID(I)
2070           Format(' ERROR: FS field 13 beginning month is not used',
     +              ' without an ending month in field 14. Right: ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If(FSI(FS,6).LE.0.and.FSI(FS,7).GT.0) Then
               Write(14,2080) WRID(I)
2080           Format(' ERROR: FS record field 14 is not used without',
     +                ' a beginning month in field 13. Right: ',A16)
               Write(14,5000)
               Call ERROR
            Endif
!
!  The next record is read. The next record read depends on whether
!  the first WR or IF record has been read. 
!
            If(WRFLAG.EQ.0) Then
               Goto 1350
            Else
               Goto 1640
            Endif
!__________________________________________________________________WS,HP
!
!  Read reservoir storage WS record.
!
         Elseif(CD.EQ.'WS') Then
            Backspace(3)
            SR=SR+1
            If(SR.GT.MAXSYS) Then
               Write(14,*)' '
               Write(14,2090) MAXSYS,WRID(I)
2090           Format(' ERROR: The number of system reservoirs (WS ',
     +                'records) exceeds MAXSYS of',I3,' for right ',A16)
               Write(14,5000)
               Call ERROR
            Endif
!
!  Initialize hydropower defaults in case HP record is not used.
!
            WRSYS(SR,9)=0.0
            WRSYS(SR,10)=0.0
            LAKESD=0
            TURCAP=0.0
            TURELE=0.0
            ENERGY=0.0
!
!  Read the old format of WS record first to check whether it was used.
!
            Read(3,2100,IOSTAT=STATUS) CD,RES,WRSYS(SR,3),EVCFA,EVCFB,
     +                     EVCFC,INACT,BEGIN,WRSYS(SR,10),WRSYS(SR,9),
     +                     LAKESD,TURCAP,TURELE,IEAR,SA
2100        Format(A2,A6,8F8.0,I8,2F8.0,2I8)
            If(STATUS.NE.0) Goto 6000
            If(WRSYS(SR,9).GT.0.1.or.TURCAP.GT.0.1.or.
     +         TURELE.GT.0.1.or.IEAR.GE.1) Then
               If(ICHECK.EQ.1) Write(14,2110) Adjustl(WRID(I))
2110           Format(' WARNING: The format of the WS record used with',
     +               ' water right ',A16,/,10x,'has been replaced with',
     +               ' the new WS/HP record format but still works.')
            Endif
!
!  Convert to IEAR and SA if the WS record is in the new format.
!
            If(WRSYS(SR,10).LT.200.and.WRSYS(SR,9).LE.0.1.and.
     +         TURCAP.LE.0.1.and.TURELE.LE.0.1) Then
               If(WRSYS(SR,10).GT.0.4) Then
                  IEAR=INT(WRSYS(SR,10))
                  SA=INT(WRSYS(SR,9))
                  WRSYS(SR,10)=0.0
                  WRSYS(SR,9)=0.0
               Endif
            Endif
!
!  Read hydropower HP record.
!
2120        Read(3,10,IOSTAT=STATUS,END=7000) CD
            If(STATUS.NE.0) Goto 6000
            If(CD.EQ.'**') Goto 2120
            If(CD.EQ.'HP') Then
               Backspace(3)
               Read(3,2130,IOSTAT=STATUS) CD,WRSYS(SR,9),WRSYS(SR,10),
     +                                    TURELE,TURCAP,ENERGY
2130           Format(A2,F6.0,4F8.0)
               If(STATUS.NE.0) Goto 6000
               If(WRNUM(I,5).GT.0) Then
                  Write(14,2140) WRNUM(I,5),Adjustl(WRID(I))
2140              Format(' WARNING: Read HP record but type is',I2,
     +                   ' for right ',A16)
               Endif
               If(ENERGY.GE.0.001.and.ENERGY.LT.AMT) Then
                  Write(14,2150) ENERGY,AMT,Adjustl(WRID(I))
2150              Format(' WARNING: Energy limit of',F8.0,
     +                   ' in HP record field 6 is less than WR record',
     +                   ' target of',F8.0,/,10x,'for water right ',A16)
               Endif
            Else
               Backspace(3)
            Endif
!
!  Data from WS and HP records are organized and adjusted.
!
            RES=Adjustr(RES)
            If(WRNUM(I,5).LT.0.and.WRSYS(SR,9).EQ.0.0) WRSYS(SR,9)=0.85
            If(Abs(STX-1.0).GE.0.0001) Then
               WRSYS(SR,3)=WRSYS(SR,3)*STX
               INACT=INACT*STX
               BEGIN=BEGIN*STX
            Endif
            TAILWT=WRSYS(SR,10)
            If(WRNUM(I,5).LT.0.0) Then
               WRSYS(SR,2)=INACT
            Else
               WRSYS(SR,1)=INACT
            Endif
            If(LAKESD.LT.0) Then
               LAKESD=-1
               If(SWRFLAG.EQ.0) Then
                  SWRCNT=SWRCNT+1
                  SWR=SWRCNT
                  SWRFLAG=-99
                  If(SWR.GT.MAXSWR) Then
                     Write(14,1030) MAXSWR,SWR
                     Call ERROR
                  Endif
               Endif
            Endif
!
!  Warning checks.are performed for data from WS and HP records.
!
            If(ICHECK.EQ.1) Then
            If(WRSYS(SR,3).LE.0) Then
               Write (14,2160) RES,Adjustl(WRID(I))
2160           Format(' WARNING: No storage capacity on WS record',
     +                ' for reservoir ',A6,', right ',A16)
            Endif
            If(INACT.GT.WRSYS(SR,3)) Then
               Write(14,2170) INACT,WRSYS(SR,3),Adjustl(WRID(I))
2170           Format(' WARNING: Inactive storage of',F9.0,' exceeds',
     +                ' capacity of',F9.0,/,10x,'for water right ',A16)
            Endif
            If(BEGIN.GT.WRSYS(SR,3)) Then
               Write(14,2180) BEGIN,WRSYS(SR,3),Adjustl(WRID(I))
2180           Format(' WARNING: Beginning storage of',F9.0,' exceeds',
     +                ' capacity of',F9.0,/,10x,'for water right ',A16)
            Endif
            If(IEAR.GE.1.and.(WRNUM(I,5).LT.0.or.WRNUM(I,5).GE.2)) Then
               Write(14,2190) Adjustl(WRID(I)),WRNUM(I,5)
2190           Format(' WARNING: EA record evaporation allocation ',
     +               'routine is activated only by type 1 or 7 rights.',
     +                /,10x,'Water right ',A16,' is type',I2)
            Endif
            Endif
!_____________________________________________________________________OR
!
!  Multiple WS records define multi-reservoir system operations.
!  Reservoir operating rule (OR) record is read or defaults set.
!
2200        Read(3,10,IOSTAT=STATUS,END=7000) CD
            If(STATUS.NE.0) Goto 6000
            If(CD.EQ.'**') Goto 2200
            If(CD.EQ.'OR'.or.CD.EQ.'WS') Then
               If(SWRFLAG.EQ.0) Then
                  SWRCNT=SWRCNT+1
                  SWR=SWRCNT
                  SWRFLAG=-99
                  If(SWR.GT.MAXSWR) Then
                     Write(14,1610) MAXSWR,SWR
                     Call ERROR
                  Endif
               Endif
            Endif
            Backspace(3)
            If(CD.EQ.'OR') Then
               Read(3,2210,IOSTAT=STATUS) CD,CP,WRSYS(SR,2),WRSYS(SR,5),
     +                     WRSYS(SR,4),SN2(SWR,SR),WRSYS(SR,6),
     +                     WRSYS(SR,7),WRSYS(SR,8),FSOR(SR)
2210           Format(A2,A6,3F8.0,I8,3F8.0,I8)
               If(STATUS.NE.0) Goto 6000
               If(STX.NE.1.0) WRSYS(SR,2)=WRSYS(SR,2)*STX
               If(WRNUM(I,5).LE.1.and.SR.EQ.1.and.SN2(SWR,SR).EQ.0.
     +            and.ICHECK.EQ.1) Then
                  Write (14,2220) Adjustl(WRID(I))
2220              Format(' WARNING: An OR record may be inappropria',
     +             'te for water right ',A16,/,10x,'OR records are not',
     +               ' used with single-reservoir type 1 or hydropower',
     +             /,10x,'rights or with the first reservoir for these',
     +              ' type rights with multiple',/,10x,'reservoirs, ',
     +              'since storage is refilled in the first reservoir.')
               Endif
               CP=Adjustr(CP)
               If(CP.EQ.'      ') CP='*CHECK'
            Else
               If(SWR.GT.0) SN2(SWR,2)=0
               WRSYS(SR,2)=INACT
               If(WRNUM(I,5).GE.0) WRSYS(SR,4)=1.0
               WRSYS(SR,5)=1.0
               WRSYS(SR,6)=0.0
               WRSYS(SR,7)=0.0
               WRSYS(SR,8)=0.0
            Endif
            If(WRSYS(SR,2).EQ.0.0) WRSYS(SR,2)=INACT
            If(WRSYS(SR,4).EQ.0.0) WRSYS(SR,4)=1.0
            If(WRSYS(SR,5).EQ.0.0) WRSYS(SR,5)=1.0
            If(CD.EQ.'OR') Then
               If(WRSYS(SR,4).LE.-0.01) WRSYS(SR,4)=0.0
               If(WRSYS(SR,5).LE.-0.01) WRSYS(SR,5)=0.0
            Endif
!
!  If SN2(SWR,SR).LT.0, releases from this reservoir to this right are not
!  constrained by senior downstream depletions or channel losses. Reservoir
!  releases are treated as being conveyed by pipeline or canal to the right.
!  CONFLU will set SN2(SWR,SR)=-1 if releases are not constrained.
!  CONFLU will set SN2(SWR,SR)=0 if the reservoir is upstream.
!  CONFLU will set SN2(SWR,SR)=CONFL, which is the index of the first control
!  point downstream of both the water right location and the reservoir.
!
!  Count hydropower rights.
!
            If(WRNUM(I,5).LT.0) Then
               NUMPOW=NUMPOW+1
            Endif
!
!  NRES=0 indicates that this is the first reservoir in the data set.
!
            If(NRES.EQ.0) Goto 2260
!
!  Reservoir loop checks whether other senior rights are associated with
!  this reservoir and, if so, sets data accordingly.
!
            Do 2250 J=1,NRES
               If(RES.EQ.RESID(J)) Then
!
!  Error check for cumulative storage capacity for multiple type 1 rights with
!  the same reservoir. The cumulative storage capacity for a junior right can
!  not be less than the cumulative storage capacity for a more senior right.
!
                  If((Abs(WRNUM(I,5)).EQ.1.or.WRNUM(I,5).EQ.-3.or.
     +                    WRNUM(I,5).EQ.7).and.SR.EQ.1) Then
                     Do 2240 L=1,NWRTS
                        If(Abs(WRNUM(L,5)).EQ.1.or.WRNUM(L,5).EQ.-3.
     +                                          or.WRNUM(L,5).EQ.7) Then
                           If(WRNUM(L,9).LT.0) Then
                              K=-WRNUM(L,9)
                              TEMP=SN2(K,1)
                           Else
                              TEMP=WRNUM(L,9)
                           Endif
                           If(TEMP.EQ.J.and.((WRNUM(I,7).LT.WRNUM(L,7)
     +                           .and.WRSYS(1,3).GT.WRDAT(L,4)).or.
     +                           (WRNUM(I,7).GE.WRNUM(L,7).and.
     +                            WRSYS(1,3).LT.WRDAT(l,4)))) Then
                              Write(14,2230) WRID(I),WRID(L),RESID(J)
2230                          Format(' WARNING: Water rights',A16,' and'
     +                        ,A16,' at reservoir ',A6,' have storage', 
     +                        /,10x,'capacities that are inconsistent',
     +                       ' with priorities.  A junior right can not'
     +                        ,' have',/,10x,'a smaller cumulative ',
     +                        'storage capacity than a senior right ',
     +                        'at the same reservoir.')
                           Endif
                        Endif
2240                 End Do
                  Endif
!
!  Various hydropower-related arrays are assigned data values.
!
                  If(WRSYS(SR,3).GT.RESDAT(J,1))
     +                              RESDAT(J,1)=WRSYS(SR,3)
                  If(WRNUM(I,5).LT.0.and.SR.EQ.1) Then
                     If(TELEV(J).EQ.0.0) TELEV(J)=TURELE
                     If(TQCAP(J).EQ.0.0) TQCAP(J)=TURCAP
                     If(TPCAP(J).EQ.0.0) TPCAP(J)=ENERGY
                     If(RESNUM(J,5).NE.9999.and.TAILWT.EQ.0) Then
                         NTWTABL=NTWTABL+1
                         RESNUM(J,5)=9999
                     Endif
                  Endif
                  If(SWR.GT.0) SN1(SWR,SR)=J
                  WRNUM(I,9)=J
!
!  End of routine for a right that has a senior right at the same reservoir.
!  End of both WS record loop and SO,TO,TS,ML,SD,WS,OR record loop for this case.
!
                  RESDAT(J,15)=RESDAT(J,1)
                  Goto 1640
               Endif
2250        End Do
!
!  Continue if no other right at the same reservoir has been previously read.
!  Hydropower and reservoir related arrays are assigned data values.
!
2260        NRES=NRES+1
            If(SWR.GT.0) SN1(SWR,SR)=NRES
            If(SR.GT.0) WRNUM(I,9)=NRES
            If((EVCFA.LE.0.0001.and.EVCFB.LE.0.0001.and.
     +          EVCFC.LE.0.0001).or.EVCFA.LT.-0.001) Then
               NTABLE=NTABLE+1
               EVCFA=-1.0
            Endif
            If(SA.LE.-1) Then
               NTABLE=NTABLE-1
               RESNUM(NRES,2)=-100
            Endif
            If(WRNUM(I,5).LT.0.and.SR.EQ.1) Then
               RESNUM(NRES,1)=WRNUM(I,1)
               If(TAILWT.EQ.0) Then
                  NTWTABL=NTWTABL+1
                  RESNUM(NRES,5)=9999
               Endif
            Endif
            If(TQCAP(NRES).EQ.0.0) TQCAP(NRES)=TURCAP
            If(TPCAP(NRES).EQ.0.0) TPCAP(NRES)=ENERGY
            If(TELEV(NRES).EQ.0.0) TELEV(NRES)=TURELE
            RESID(NRES)=RES
            EAR(NRES)=IEAR
!
!  If a type 1 right, and the first reservoir associated with the right, and 
!  a new reservoir, then the reservoir location is the same as the right.
!  Otherwise, the reservoir control point location must be input.
!
            If(WRNUM(I,5).EQ.1.and.SR.EQ.1) Then
               RESNUM(NRES,1) = WRNUM(I,1)
               Goto 2280
            Endif
            If(CP.EQ.'*CHECK') Then
               Write(14,2270) Adjustl(WRID(I))
2270           Format(' WARNING: Control point ID of a reservoir is ',
     +               'missing for multiple-reservoir system right ',A16)
            Endif
            Do K=1,NCPTS
               If(CP.EQ.CPID(K,1)) Then
                  RESNUM(NRES,1)=K
                  Goto 2280
               Endif
            End Do
2280        If(NREOUT.LT.0) Then
               RESNUM(NRES,3)=1
            Elseif(NREOUT.EQ.0) Then
               RESNUM(NRES,3)=0
            Else
               Do K=1,NREOUT
                  If(REOUID(K).EQ.RESID(NRES)) Then
                     RESNUM(NRES,3)=1
                     Goto 2300
                  Endif
               End Do
            Endif
!
!  Reservoir data arrays are assigned values.
!
2300        RESDAT(NRES,1)=WRSYS(SR,3)
            RESDAT(NRES,2)=EVCFA
            RESDAT(NRES,3)=EVCFB
            RESDAT(NRES,4)=EVCFC
            RESDAT(NRES,5) = -1
            If(BEGIN.GT.0.0) RESDAT(NRES,5)=BEGIN
            RESDAT(NRES,6)=0.0
            RESDAT(NRES,7)=0.0
            RESDAT(NRES,8)=0.0
            RESDAT(NRES,9)=0.0
            RESDAT(NRES,10)=0.0
            RESDAT(NRES,15)=WRSYS(SR,3)
!
!  End of routine for a right that has no senior right at the same reservoir.
!  End of both WS record loop and PX,SO,TO,TS,ML,SD,WS,OR record loop.
!  Next record is read to either repeat a loop or continue on below.
!
            Goto 1640
         Endif
!______________________________________________________________________
!
!  Downstream confluence locations for system rights.
!      *+*+*+*+*  Call Subroutine  CONFLU  *+*+*+*+*
!
2310     If(SWR.GT.0) Then
            NSR(SWR)=SR
            Call CONFLU(I)
         Endif
!
!  Values assigned to arrays.
!
         WRDAT(I,1)=AMT
         WRDAT(I,2)=RFAC
         WRDAT(I,3)=WRSYS(1,1)
         WRDAT(I,4)=WRSYS(1,3)
         If(LAKESD.EQ.-1) SN3(SWR,SR)=-1
         WRNUM(I,11)=LAKESD
!
!  The counter index SWR and final reservoir count SR are stored for
!  a system water right as a negative WRNUM(I,9) and NSR(SWR). The
!  WRSYS(J,K) array for this right is stored as SYSTEM(SWR,J,K).
!
         If(SWR.GT.0) Then
            WRNUM(I,9)=-SWR
            NSR(SWR)=SR
            Do J=1,SR
               Do K=1,10
                  SYSTEM(SWR,J,K)=WRSYS(J,K)
               End Do
            End Do
         Endif
!
!  Error and warning checks.
!
!      Reservoir location should be at the water right control point
!      for type 1 and hydropower rights if there is a reservoir.
!
         If(ICHECK.EQ.1) Then
         If(Abs(WRNUM(I,5)).EQ.1.and.WRNUM(I,9).GT.0) Then
            If(WRNUM(I,1).NE.RESNUM(WRNUM(I,9),1)) Then
               Write(14,2320) RESID(NRES),Adjustl(WRID(I))
2320           Format(' WARNING: The water right and reservoir normally'
     +          ' should be at the same control point',/,10x,'for a ',
     +          'type 1 right.   Reservoir: ',A6,4x,'Water right: ',A16)
            Endif
         Endif
!
!       Hydropower rights must have a reservoir (WS record).
!
         If(WRNUM(I,5).EQ.-1.or.WRNUM(I,5).EQ.-3) Then
            If(NSR(SWR).EQ.0) Then
               Write(14,2330) WRID(I)
2330           Format(' ERROR: No reservoir assigned to hydropower',
     +                ' right', A16)
               Call ERROR
            Endif
         Endif
!
!      IF record must be followed by WS record for IFMETH = 3 or 4.
!
         If(WRIDS(I,1).EQ."IF#IF*IF") Then
            If((IFMETH(I).EQ.3.or.IFMETH(I).EQ.4).and.SR.EQ.0)
     +      Then
               Write(14,*) ' '
               Write(14,2340) IFMETH(I),Adjustl(WRID(I))
2340           Format(' ERROR: IFMETH is',I2,' but there is no '
     +                'reservoir (WS record) for IF right ',A16)
               Write(14,5000)
               Call ERROR
            Endif
            If((IFMETH(I).NE.3.and.IFMETH(I).NE.4).and.SR.NE.0)
     +      Then
               Write(14,2350) Adjustl(WRID(I))
2350           Format(' WARNING: There is a reservoir (WS record) but'
     +                ' IFMETH is not 3 or 4 for IF right ',A16)
            Endif
         Endif
         Endif
!
!  Dual simulation data are organized and checked. DUAL option specified
!  by JO record field 11, SO record field 14, or PX record field 2.
!
         If(DUALD.GT.0.and.DUAL(I).EQ.0) DUAL(I)=DUALD
                  If(DUAL(I).LT.0) DUAL(I)=0
         J=0
         If(DUAL(I).EQ.33) Then
            J=33
            DUAL(I)=3
         Elseif(DUAL(I).EQ.333) Then
            J=333
            DUAL(I)=3
         Elseif(DUAL(I).EQ.55) Then
            J=55
            DUAL(I)=5
         Elseif(DUAL(I).EQ.555) Then
            J=555
            DUAL(I)=5
         Endif
         If(DUAL(I).NE.0) Then
            If(DUAL(I).EQ.3.or.DUAL(I).EQ.5) SIM2=9
            NDT=NDT+1
            If(DUAL(I).EQ.5) Then
               DD(I)=NDD
               NDS=NDS+1
               If(NDD.EQ.0) Then
                  Write(14,*) ' '
                  Write(14,2360) Adjustl(WRID(I))
2360              Format(' ERROR: DUAL is 5 for water right ',
     +                     A16,/,8x,'but there is no preceding',
     +                   ' dual option 4 right.')
                  Write(14,5000)
                  Call ERROR
               Endif
            Elseif(DUAL(I).EQ.3.or.DUAL(I).EQ.4) Then
               NDD=NDD+1
               DD(I)=NDD
               If(J.NE.0) DUALX(NDD)=J
               If(DUAL(I).EQ.3) NDS=NDS+1
            Endif
            If(DUAL(I).LT.0.or.DUAL(I).GT.5) Then
               Write(14,*) ' '
               Write(14,2370) DUAL(I),Adjustl(WRID(I))
2370           Format(' ERROR: DUAL of',I3,' is invalid for',
     +                ' water right ',A16)
               Write(14,5000)
               Call ERROR
            Endif
         Endif
!________________________________________________________________________
!
!  End of WR/IF record loop. The next record is read, and either the loop
!  repeats for the next WR/IF record or execution continues on below.
!
         Goto 1350
!
      Else
         WRFLAG=0
         If(ICHECK.GE.0)Write(14,*)'*** Finished reading IF/WR records.'
         Backspace(3)
      Endif
!__________________________________________________________________SV,SA
!
!  List of reservoirs requiring SV and SA records.
!
      If(ICHECK.EQ.5) Then
         Write(14,2480)
2480     Format(' SV and SA records are required for the following',
     +          ' reservoirs',/,' which have no entries for EVCFA,',
     +          ' EVCFC, or SA on WS records.')
         J=0
         Do I=1,NRES
            If(RESDAT(I,2).LT.0.00001.and.RESDAT(I,4).LT.0.00001.and.
     +         RESNUM(I,2).NE.-100) Then
               J=J+1
               Write(14,2490) J,I,RESID(I)
2490           Format(18x,I4,I5,3x,A6)
            Endif
         End Do
      Endif
!
!  Storage-area tables (SV/SA records) and storage-elevation
!  tables (PV/PE records) are read.
!
2500  Read(3,10,IOSTAT=STATUS,END=7000) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Then
         Goto 2500
      Elseif(CD.EQ.'SV') Then
         Backspace(3)
         If(ICHECK.GE.0) Write(14,*)
     +      '*** Starting to read SV/SA records.'
         If(NTABLE.EQ.0) Then
            Write(14,*) ' WARNING: SV/SA records are provided but not',
     +                             ' assigned to a reservoir.'
            Read(3,10,IOSTAT=STATUS) CD
            Read(3,10,IOSTAT=STATUS) CD
            If(STATUS.NE.0) Goto 6000
            If(CD.NE.'SA') Call ERROR
            Goto 2500
         Endif
         Do 2610 I=1,NTABLE
            MATCH=0
2510        Read(3,10,IOSTAT=STATUS,END=7000) CD
            If(STATUS.NE.0) Goto 6000
            If(CD.EQ.'**') Goto 2510
            If(CD.NE.'SV') Then
               Write(14,*)' '
               J=I-1
               Write(14,2515) CD,NTABLE,J
2515           Format(' ERROR: Missing SV record. Read CD of ',A2,/,8x,
     +         'Number of SV/SA records required based on WS records =',
     +                I4,/8x,'Number of SV/SA records read =',
     +                I4,/8x,'Reservoirs and SV/SA records can be '
     +                       'listed with JD record ICHECK option 5.')
               Write(14,5000)
               Call ERROR
            Endif
            Backspace(3)
            Read(3,2520,IOSTAT=STATUS) CD,RES,(TARA(J),J=1,TL)
2520        Format(A2,A6,<TL>F8.0)
            If(STATUS.NE.0) Goto 6000
            RES=Adjustr(RES)
            If(ICHECK.EQ.5) Write(14,2520) CD,RES,(TARA(J),J=1,TL)
            Read(3,2550,IOSTAT=STATUS,END=7000) CD,(TARB(J),J=1,TL)
2550        Format(A2,6X,<TL>F8.0)
            If(STATUS.NE.0) Goto 6000
            If(ICHECK.EQ.5) Write(14,2550) CD,(TARB(J),J=1,TL)
2560        If(CD.NE.'SA') Then
               Write(14,2565) RES,CD
2565           Format(' ERROR: Missing SA record for reservoir ',A6,
     +                '. Read CD of ',A2)
               Write(14,5000)
               Call ERROR
            Endif
            Do 2580 J=1,NRES
               If(RES.EQ.RESID(J)) Then
                  MATCH=MATCH+1
                  Do 2570 K=1,TL
                     EVCURV(I,(K*2))=TARA(K)*STX
                     EVCURV(I,(K*2)+1)=TARB(K)*SAX
2570              End Do
                  RESNUM(J,2)=I
               Endif
2580        End Do
            If(MATCH.NE.1) Then
               Write(14,*)' '
               Write(14,2590)
2590           Format(' ERROR: Missing or duplicate reservoir ID ',
     +                'found while reading SV/SA records.')
               Write(14,2600) I,RES
2600           Format(8X,' Error occurred on table ',I3,
     +                   ' for reservoir ID ',A6)
               Write(14,5000)
               Call ERROR
            Endif
2610     End Do
         If(ICHECK.GE.0)Write(14,*)'*** Finished reading SV/SA records.'
         Goto 2500
!__________________________________________________________________PV,PE
!
!  Storage-elevation tables (PV/PE records) for hydropower computations are read.
!
      Elseif(CD.EQ.'PV') Then
         If(ICHECK.GE.0) Write(14,*)
     +      '*** Starting to read PV/PE records.'
         I=0
3220     I=I+1
         Backspace(3)
         Read(3,2520,IOSTAT=STATUS) CD,RES,(TARA(J),J=1,TL)
         If(STATUS.NE.0) Goto 6000
         RES=Adjustr(RES)
         Read(3,10,IOSTAT=STATUS) CD
         If(STATUS.NE.0) Goto 6000
         If(CD.NE.'PE') Then
            Write(14,3230) RES,CD
3230        Format(' ERROR: Missing PE record for reservoir ',A6,
     +             '. Read CD of ',A2)
            Write(14,5000)
            Call ERROR
         Else
            Backspace(3)
            Read(3,2550,IOSTAT=STATUS) CD,(TARB(J),J=1,TL)
            If(STATUS.NE.0) Goto 6000
         Endif
!
!        Reservoir is identified and volume/elevation table assigned to arrays.
!
         J=0
3240     J=J+1
         If(RES.EQ.RESID(J)) Then
            Do K=1,TL
               PVCURV(I,(K*2))=TARA(K)*STX
               PVCURV(I,(K*2)+1)=TARB(K)
            End Do
            RESNUM(J,4)=I
            Goto 3260
         Endif
         If(J.GT.NRES) Then
            Write(14,3250) RES,CD
3250        Format(' ERROR: Reservoir ',A6,' on ',A2,
     +             ' record has no WS record.')
            Write(14,5000)
            Call ERROR
         Endif
         Goto 3240
!
!        CD on next record is checked,  Next PV/PE records are read.
!
3260     Read(3,10,IOSTAT=STATUS) CD
         If(STATUS.NE.0) Goto 6000
         If(CD.EQ.'**') Goto 3260
         If(CD.EQ.'PV') Goto 3220
!
!        All PV/PE records have been read.
!
         NPTABL=I
         If(ICHECK.GE.0)Write(14,*)'*** Finished reading PV/PE records.'
      Endif
      Backspace(3)
!______________________________________________________________________TQ,TE
!
!  Tailwater rating curve (discharge vs elevation) is read from TQ/TE records.
!
      MERROR=0
      I=0
3300  Read(3,10,IOSTAT=STATUS,END=7000) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Then
         Goto 3300
      Elseif(CD.EQ.'TQ') Then
         If(ICHECK.GE.0) Write(14,*) 
     +       '*** Starting to read TQ/TE records.'
         I=0
         Do while (CD.EQ.'TQ')
            Backspace(3)
            Read(3,2520,IOSTAT=STATUS) CD,RES,(TARA(J), J=1,TL)
            If(STATUS.NE.0) Goto 6000
            RES=Adjustr(RES)
            Read(3,2550,IOSTAT=STATUS) CD,(TARB(J), J=1,TL)
            If(STATUS.NE.0) Goto 6000
            If (CD.NE.'TE') Then 
               MERROR=101
               Goto 3310
            Endif
            I=I+1
            Do K=1,TL
               TWCURV(I,(2*K))=TARA(K)
               TWCURV(I,(2*K+1))=TARB(K)
            End do
            J=1
            Do while ((RES.NE.RESID(J)).and.(J.LE.NRES))
               J=J+1
            End Do
            If (J.GT.NRES) Then 
               Write(14,3340) RES
               I=I-1
            Elseif (RESNUM(J,5).EQ.0) Then
               MERROR=102
               Goto 3310
            Elseif (RESNUM(J,5).NE.9999) Then
               MERROR = 104
               Goto 3310
            Else
               RESNUM(J,5)=I
            Endif
            Read(3,10,IOSTAT=STATUS) CD
            If(STATUS.NE.0) Goto 6000
            Do while (CD.EQ.'**') 
               Read (3,10,IOSTAT=STATUS) CD
               If(STATUS.NE.0) Goto 6000
            End Do
         End Do
      Endif
!
!  Check that the number of tables read equals the number expected (NTWTABL).
! 
      If(I.GT.NTWTABL) MERROR=104
      If(I.LT.NTWTABL) MERROR=105
      If(ICHECK.GE.0.and.NTWTABL.GT.0)
     +   Write(14,*) '*** Finished reading TQ/TE records.'
!
!  Report errors reading TQ and TE records.
!
3310  If(MERROR.GT.0) Then
         If(MERROR.EQ.101) Then
            Write(14,3320) CD
         Elseif(MERROR.EQ.102) Then
            Write(14,3330) RES
         Elseif(MERROR.EQ.104) Then
            Write(14,3350)
         Else
            Write(14,3360)
         Endif
         Write(14,5000)
         Call ERROR
3320     Format(' ERROR: Missing TQ or TE record. Read CD of ',A2)
3330     Format(' ERROR: WS record is not compatible with TQ/TE record'
     +           ' for resevoir ',A6)
3340     Format(' WARNING: TQ/QE records are provided, but not assigned'
     +           ,' to a reservoir.',/'Reservoir ID = ', A6)    
3350     Format(' ERROR: Duplicate reservoir ID found while reading ',
     +           'TQ/TE records')
3360     Format(' ERROR: Missing tailwater-turbine flow table.')
      Endif
      Backspace(3)
!_________________________MS,OS,DI,IP,IS,EA,EF__________________________
!
!  Check whether either a MS, OS, DI, or EA record is read.
!
      DIFLAG=0
      EAFLAG=0
      MSFLAG=0
      OSFLAG=0
3370  Read(3,10,IOSTAT=STATUS,END=7000) CD
      If(STATUS.NE.0) Goto 6000
      If(CD.EQ.'**') Then
         Goto 3370
!_____________________________________________________________________MS
!
!  Monthly varying reservoir storage capacities on MS records are read.
!
      Elseif(CD.EQ.'MS') Then
         If(MSFLAG.EQ.0) Write(14,*)'*** Starting to read MS records.'
         Backspace(3)
         MSFLAG=MSFLAG+1
         Read(3,3380,IOSTAT=STATUS) CD,RES,(STMON(MSFLAG,MT),MT=1,13)
3380     Format(A2,A6,13F8.0)
         If(STATUS.NE.0) Goto 6000
         RES=Adjustr(RES)
         If(STMON(MSFLAG,13).EQ.0.0) STMON(MSFLAG,13)=9000000000.0
         If(STX.NE.1.0) Then
            Do MT=1,12
               STMON(MSFLAG,MT)=STMON(MSFLAG,MT)*STX
            End Do
         Endif
!
!  Check that reservoir on MS record is found on a WS record
!  and assign integer identifier MSRES(MSFLAG).
!
         K=0
3390     K=K+1
         If(K.GT.NRES) Then
            Write(14,*) ' '
            Write(14,3400) RES,CD
3400        Format(' ERROR: Reservoir ',A6,' on ',A2,' record',
     +             ' is not on any WS record.')
            Write(14,5000)
            Call ERROR
         Endif
         If(RES.EQ.RESID(K)) Then
            MSRES(MSFLAG)=K
            Goto 3370
         Endif
         Goto 3390
!_____________________________________________________________________OS
!
!  Observed storage OS records are read.
!
      Elseif(CD.EQ.'OS') Then
         If(OSFLAG.EQ.0) Write(14,*)'*** Starting to read OS records.'
         Backspace(3)
         OSFLAG=OSFLAG+1
         Read(3,3410,IOSTAT=STATUS) CD,RES,(OS(OSFLAG,J),J=1,12)
3410     Format(A2,A6,12F8.0)
         Do I=2,NYRS
            K=12*(I-1)
3412        Read(3,3414,IOSTAT=STATUS) CD
3414        Format(A2)
            If(CD.EQ.'**') Goto 3412
            If(STATUS.NE.0) Goto 6000
            If(CD.NE.'OS') Then
               Write(14,3415) CD,RES
3415           Format(' ERROR: Read CD of ',A2,' instead of OS',
     +                ' record for reservoir ',A6)
               Write(14,5000)
               Call ERROR
            Endif
            Backspace(3)
            Read(3,3416,IOSTAT=STATUS) CD,(OS(OSFLAG,K+J),J=1,12)
3416        Format(A2,6x,12F8.0)
            If(STATUS.NE.0) Goto 6000
         Enddo
         If(STX.NE.1.0) Then
            K=12*NYRS
            Do MT=1,K
               OS(OSFLAG,MT)=OS(OSFLAG,MT)*STX
            End Do
         Endif
!
!  Check that reservoir on OS record is found on a WS record
!  and assign integer identifier OSRES(OSFLAG).
!
         K=0
3418     K=K+1
         If(K.GT.NRES) Then
            Write(14,*) ' '
            Write(14,3400) RES,CD
            Write(14,5000)
            Call ERROR
         Endif
         If(RES.EQ.RESID(K)) Then
            OSRES(OSFLAG)=K
            Goto 3370
         Endif
         Goto 3418
!_______________________________________________________________DI,IS,IP
!
!  Drought index reservoir specifications are read from DI,IS,IP records.
!
      Elseif(CD.EQ.'DI') Then
         If(ICHECK.GE.0.and.DIFLAG.EQ.0)
     +               Write(14,*)'*** Starting to read DI/IS/IP records.'
         DIFLAG=DIFLAG+1
         DINUM(DIFLAG)=0
         Backspace(3)
         Read(3,3420,IOSTAT=STATUS) CD,NDI,EMPTY(DIFLAG),NR
         If(STATUS.NE.0) Goto 6000
3420     Format(A2,I6,2I4)
         If(EMPTY(DIFLAG).GT.12) Then
            Write(14,*) ' '
            Write(14,3430) EMPTY(DIFLAG)
3430        Format(' ERROR: EMPTY of ',I4,' on DI record is not valid.')
            Write(14,5000)
            Call ERROR
         Endif
         If(NR.LT.0) Then
            DINUM(DIFLAG)=99
         Else
            Backspace(3)
            Read(3,3440,IOSTAT=STATUS) CD,NDI,EMPTY(DIFLAG),NR,
     +                                 (DIRES(DIFLAG,I),I=1,NR)
3440        Format(A2,I6,2I4,12(2x,A6))
            If(STATUS.NE.0) Goto 6000
            Do 3450,I=1,NR
               DIRES(DIFLAG,I)=Adjustr(DIRES(DIFLAG,I))
3450        End Do
            If(NR.GE.1.and.NR.LE.12) Then
               DINUM(DIFLAG)=NR
            Else
               Write(14,*) ' '
               Write(14,*)' ERROR: Number of reservoirs specified on ',
     +                    'DI record must be 1 to 12 or all(-1).'
               Write(14,5000)
               Call ERROR
            Endif
            If(ICHECK.GE.1) Then
               Do 3480 J=1,NR
                  I=0
3460              I=I+1
                  If(DIRES(DIFLAG,J).EQ.RESID(I)) Goto 3480
                  If(I.GE.NRES) Then
                     Write(14,*) ' '
                     Write(14,3470) DIRES(DIFLAG,J),DIFLAG
3470                 Format(' ERROR: Reservoir ',A6,' on DI record',
     +                      I3,' is not on any WS record.')
                     Write(14,5000)
                     Call ERROR
                  Endif
                  Goto 3460
3480           End Do
            Endif
            If(NDI.NE.DIFLAG.and.NDI.NE.0) Then
               Write(14,*)' WARNING: Incorrect NDI identifier in ',
     +                    '          field 2 of DI record.'
            Endif
         Endif
         Read(3,3490,IOSTAT=STATUS) CD,N,(DISTO(DIFLAG,I),I=1,N)
3490     Format(A2,I6,<TL>F8.0)
         If(STATUS.NE.0) Goto 6000
         If(STX.NE.1.0) Then
            Do I=1,N
               DISTO(DIFLAG,I)=DISTO(DIFLAG,I)*STX
            End Do
         Endif
         Read(3,3500,IOSTAT=STATUS) CD,(DIPER(DIFLAG,I),I=1,N)
3500     Format(A2,6x,<TL>F8.0)
         If(STATUS.NE.0) Goto 6000
         Goto 3370
!_______________________________________________________________EA,EF,AF
!
!  Evaporation allocation specifications EA/EF records are read.
!
      Elseif(CD.EQ.'EA') Then
         If(ICHECK.GE.0.and.EAFLAG.EQ.0)
     +               Write(14,*)'*** Starting to read EA records.'
         EAFLAG=EAFLAG+1
         Backspace(3)
         Read(3,3510,IOSTAT=STATUS) CD,NEA,NEAF(EAFLAG),
     +                       (EARES(EAFLAG,I),I=1,20)
3510     Format(A2,I6,I8,20(2x,A6))
         If(STATUS.NE.0) Goto 6000
         If(NEAF(EAFLAG).EQ.22.or.NEAF(EAFLAG).EQ.33) Then
            If(NEAF(EAFLAG).EQ.22) NEAF(EAFLAG)=2
            If(NEAF(EAFLAG).EQ.33) NEAF(EAFLAG)=3
            EAX(EAFLAG)=-9
         Endif
         Do I=1,20
            EARES(EAFLAG,I)=Adjustr(EARES(EAFLAG,I))
            If(EARES(EAFLAG,I).NE.'      ') EARNUM(EAFLAG)=
     +                                      EARNUM(EAFLAG)+1
         End Do
         If(NEAF(EAFLAG).LT.0.or.NEAF(EAFLAG).GT.4) Then
            Write(14,*) ' '
            Write(14,3520) NEAF(EAFLAG)
3520        Format(' ERROR: NEAF of',I3,' in EA field 3 is invalid.')
            Write(14,5000)
            Call ERROR
         Endif
!
!   EF record is read for NEAF options 3 and 4.
!
         If(NEAF(EAFLAG).LE.2) Then
3530        Read(3,3540,IOSTAT=STATUS,END=7000) CD
3540        Format(A2)
            If(STATUS.NE.0) Goto 6000
            If(CD.EQ.'**') Then
               Goto 3530
            Elseif(CD.EQ.'EF') Then
               Write(14,*) ' '
               Write(14,3550)
3550           Format(' ERROR: EF record is used only if NEAF is 3',
     +                ' or 4 in EA field 3.')
               Write(14,5000)
               Call ERROR
            Else
               Backspace(3)
            Endif
         Endif
         If(NEAF(EAFLAG).GE.3) Then
3560        Read(3,3570,IOSTAT=STATUS,END=7000) CD,EAO(EAFLAG),
     +           EAL(EAFLAG),(EAF(EAFLAG,I),I=1,EARNUM(EAFLAG))
3570        Format(A2,I6,F8.0,20F8.0)
            If(STATUS.NE.0) Goto 6000
            If(CD.EQ.'**') Then
               Goto 3560
            Elseif(CD.NE.'EF') Then
               Write(14,*) ' '
               Write(14,3580) CD
3580           Format(' ERROR: Read CD of ',A2,' when expecting EF.')
               Write(14,5000)
               Call ERROR
            Endif
            If(EAO(EAFLAG).LT.0.or.EAO(EAFLAG).GT.4) Then
               Write(14,*) ' '
               Write(14,3590) EAO(EAFLAG)
3590           Format(' ERROR: EAO in EF field 2 should be between',
     +                ' 0 and 4. Read',I3)
               Write(14,5000)
               Call ERROR
            Endif
            If(EAO(EAFLAG).GE.3.and.NEAF(EAFLAG).NE.4) Then
               Write(14,*) ' '
               Write(14,3600)
3600           Format(' ERROR: EAO of 3 or 4 in EF field 2 is valid',
     +                ' only for NEAF of 4 in EA field 3.')
               Write(14,5000)
               Call ERROR
            Endif
            If(EAL(EAFLAG).LT.0.or.EAL(EAFLAG).GT.1.0) Then
               Write(14,3610) EAL(EAFLAG)
3610           Format(' WARNING: EAL on EF record should be between',
     +                ' 0.0 and 1.0. Read',F8.2)
            Endif
            If(EAX(EAFLAG).EQ.0.and.
     +         EAF(EAFLAG,EARNUM(EAFLAG)).NE.0.0)Then
               Write(14,3620) EAF(EAFLAG,EARNUM(EAFLAG))
3620           Format(' WARNING: EF record EAF of',F6.3,' for last',
     +                ' reservoir listed on EA record is not used.')
            Endif
         Endif
!
!  SV/SA tables are assigned to the second and subsequent EA record reservoirs.
!
         Do 3670 J=1,20
            If(EARES(EAFLAG,J).EQ.'      ') Goto 3670
            I=0
3630        I=I+1
            If(EARES(EAFLAG,J).EQ.RESID(I)) Goto 3650
            If(I.GE.NRES) Then
               Write(14,*) ' '
               Write(14,3640) EARES(EAFLAG,J)
3640           Format(' ERROR: Reservoir ',A6,' on EA record',
     +                ' is not on any WS record.')
               Write(14,5000)
               Call ERROR
            Endif
            Goto 3630
3650        If(J.EQ.1) Then
               J1=I
            Elseif(RESNUM(I,2).LE.-99) Then
               If(RESNUM(J1,2).EQ.0) Then
                  Write(14,*)
                  Write(14,3660)  RESID(J1)
3660              Format(' ERROR: No SV/SA records are assigned ',
     +                       'to reservoir ',A6,' on EA record.')
                  Write(14,5000)
                  Call ERROR
               Endif
               RESNUM(I,2)=RESNUM(J1,2)
            Endif
3670     End Do
         If(NEA.NE.EAFLAG.and.NEA.NE.0) Then
            Write(14,*)' WARNING: Incorrect NEA identifier in',
     +                 ' field 2 of EA record.'
         Endif
!
!  Integer reservoir identifiers for EA record reservoirs are
!  recorded as the array EAI(N,20).
!
         Do 3700 J=1,EARNUM(EAFLAG)
            L=0
3680        If(L.GT.NRES) Then
               Write(14,3690) EARES(EAFLAG,J)
3690           Format(' ERROR: Reservoir ',A6,' from EA record could ',
     +           'not be matched with WS record reservoir identifiers.')
               Write(14,5000)
               Call ERROR
            Endif
            L=L+1
            If(RESID(L).NE.EARES(EAFLAG,J)) Goto 3680
            EAI(EAFLAG,J)=L
3700     End Do
!
!  Streamflow availability allocation factors AF record is read.
!
3710     Read(3,3720,IOSTAT=STATUS,END=7000) CD
3720     Format(A2)
         If(STATUS.NE.0) Goto 6000
         If(CD.EQ.'**') Goto 3710
         Backspace(3)
         If(CD.EQ.'AF') Then
            Read(3,3730,IOSTAT=STATUS,END=7000) CD,AFMIN(EAFLAG),
     +             AFMAX(EAFLAG),(AFX(EAFLAG,I),I=1,EARNUM(EAFLAG))
3730        Format(A2,F6.0,F8.0,20F8.0)
            If(STATUS.NE.0) Goto 6000
            XAFFLAG=9
            Do I=1,EARNUM(EAFLAG)
               If(AFX(EAFLAG,I).LT.0.0.or.AFX(EAFLAG,I).GT.1.0) Then
                  Write(14,3740) AFX(EAFLAG,I)
3740              Format(' WARNING: AFX on AF record should normally',
     +                   ' be between 0.0 and 1.0. Read',F8.2)
               Endif
               If(AFX(EAFLAG,I).LE.0.00001) Then
                  Write(14,3750) AFX(EAFLAG,I),EARES(EAFLAG,I)
3750              Format(' WARNING: AFX on AF record is',F6.3,' for ',
     +            'reservoir ',A6,' thus allocating it no streamflow.')
               Endif
            End Do
            If(AFMAX(EAFLAG).LE.0.0) AFMAX(EAFLAG)=9999999000000.0
         Endif
         Goto 3370
      Endif
!_____________________________________________________________________ED
!
!  End-of-data ED record is read.
!
      If(ICHECK.GE.0) Then
         If(DIFLAG.GT.0) Write(14,*)'*** Finished reading DI/IS/IP ',
     +                              'records.'
         If(EAFLAG.GT.0) Write(14,*)'*** Finished reading EA records.'
         If(MSFLAG.GT.0) Write(14,*)'*** Finished reading MS records.'
      Endif
      If(CD.EQ.'ED') Then
         If(FYLEVEL.LE.0) Then
            Write(*,3800) NCPTS
            Write(*,3810) NWRTS
            Write(*,3820) NRES
3800        Format(3X,I8,' control points')
3810        Format(3X,I8,' water rights')
3820        Format(3X,I8,' reservoirs')
            Write(*,*) ' '
         Endif
      Else
!_______________________________________________________________________
!  Error message.
!
         Write(14,*)' '
         Write(14,3830) CD
3830     Format(' ERROR: The following invalid record identifier (CD ',
     +         'in field 1) was read: ',A2,/,8X,'This indicates either',
     +         ' an incorrect CD, a missing record, or a blank record.')
         Backspace(3)
         Backspace(3)
         Backspace(3)
         Write(14,*)' '
         Write(14,*)'The first 80 characters of each of the last',
     +              ' two records read are as follows:'
         Read(3,3840) TITLE
         Write(14,3840) TITLE
         Read(3,3840) TITLE
         Write(14,3840) TITLE
3840     Format(A80)
         Write(14,*)' '
         Write(14,5000)
         Call ERROR
      Endif
!_______________________________________________________________________

!  Additional water rights are created for transient rights specified
!  on PX records, which reverse streamflow depletions and return flows.
!
      If(XPCOUNT.GT.0) Then
         NUMXP=0
         NUMWR=NWRTS
         Do I=1,NUMWR
            If(WRNUM(I,8).GT.0) Then
               NWRTS=NWRTS+1
               NUMXP=NUMXP+1
               K=WRNUM(I,8)
               J=NUMWR+K
               WRID(J)=Adjustr(WRID1(K))
               WRIDS(J,1)=Adjustr(WRID2(K))
               WRIDS(J,2)=Adjustr(WRID3(K))
               If(XP(K).EQ.1) Then
                  WRNUM(J,5)=11
                  WRNUM(J,1)=WRNUM(I,1)
                  WRNUM(J,3)=WRNUM(I,3)
               Elseif(XP(K).EQ.2) Then
                  WRNUM(J,5)=12
                  WRNUM(J,1)=WRNUM(I,3)
                  WRNUM(J,3)=WRNUM(I,1)
               Endif
               WRNUM(J,7)=XPRIORITY(K)
               WRNUM(J,8)=-1*WRNUM(I,8)
               WRNUM(J,2)=1
               WRNUM(J,9)=WRNUM(I,9)
               WRNUM(J,10)=WRNUM(I,10)
               WRNUM(J,11)=WRNUM(I,11)
!
!  XPR is set to 9 for run-of-river right.
!
               If(WRNUM(J,9).EQ.0) Then
                  If(XPR(K).NE.0) Then
                     Write(14,3850) XPR(K),Adjustl(WRID(I))
3850                 Format(' WARNING: XPR is',I2,' on PX record for',
     +                      ' run-of-river water right: ',A16)
                  Endif
                  XPR(K)=9
               Endif
!
!  WRNUM(wr,6) flag of whether to include water right records in output file.
!
               If(XPOUT(K).LT.0) Then
                  If(WRNUM(I,6).EQ.1.and.Abs(XP(K)).EQ.2) Then
                     WRNUM(I,6)=0
                     NWROUT=NWROUT-1
                  Endif
                  WRNUM(J,6)=0
               Elseif(XPOUT(K).GE.0) Then
                  WRNUM(J,6)=1
                  NWROUT=NWROUT+1
               Endif
            Endif
         End Do
      Endif
!
!  The number of control points included in output file is counted.
!
      If(CPOUT.EQ.-1) Then
         NCPO=NCPTS
      Elseif(CPOUT.NE.-1) Then
         NCPO=0
         Do 3860 I=1,NCPTS
            If((CPOUT.EQ.-2.and.INMETHOD(I).LE.1).or.
     +         (CPOUT.GT.0.and.I.LE.CPOUT)) Then
               NCPO=NCPO+1
            Elseif(NCPOUT.GT.0) Then
               Do J=1,NCPOUT
                  If(CPOUID(J).EQ.CPID(I,1)) Then
                     NCPO=NCPO+1
                     Goto 3860
                  Endif
               End Do
            Endif
3860     End Do
      Endif
!
!  Channel loss flag CLFLAG is set indicating whether channel losses occur.
!
      CLFLAG=0
      Do I=1,NCPTS
         If(CL(I).GT.0.0) CLFLAG=CLFLAG+1
      End Do
!
!  Integer CP identifiers are assigned to ZZ record control points.
!
      If(ZZ.GE.1) Then
        Do Z=1,ZZ
           J=0
3870       J=J+1
           If(CPID(J,1).NE.ZZCP(Z)) Then
              If(J.GT.NCPTS) Then
                 Write(14,3880) ZZCP(Z)
3880             Format(' ERROR: Control point ',A6,' on ZZ record is',
     +                  ' is not on any CP record.')
                 Write(14,5000)
                 Call ERROR
              Endif
              Goto 3870
           Endif
           ZZI(Z)=J
         End Do
      Endif
!_______________________________________________________________________
!
!  Warning check for identifiers on CO, WO, RO, and GO records.
!
      If(ICHECK.EQ.1) Then
         If(NCPOUT.GT.0) Then
            Do 4070 I=1,NCPOUT
               J=0
4050           J=J+1
               If(J.GT.NCPTS) Then
                  Write(14,4060) CPOUID(I)
4060              Format(' WARNING: ',A6,' on CO record',
     +                   ' is not on any CP record.')
                  Goto 4070
               Endif               
               If(CPID(J,1).NE.CPOUID(I)) Goto 4050
4070        End Do
         Endif
         If(NWOUT.GT.0) Then
            Do 4100 I=1,NWOUT
               J=0
4080           J=J+1
               If(J.GT.NWRTS) Then
                  Write(14,4090) WROUT(I)
4090              Format(' WARNING: ',A16,' on WO record',
     +                   ' is not on any WR record.')
                  Goto 4100
               Endif               
               If(WRID(J).NE.WROUT(I)) Goto 4080
4100        End Do
         Endif
         If(NGOUT.GT.0) Then
            Do 4130 I=1,NGOUT
               J=0
4110           J=J+1
               If(J.GT.NWRTS) Then
                  Write(14,4120) GROUP(I)
4120              Format(' WARNING: ',A8,' on GO record',
     +                   ' is not on any WR record.')
                  Goto 4130
               Endif               
               If(WRIDS(J,1).NE.GROUP(I).and.WRIDS(J,2).NE.GROUP(I))
     +            Goto 4110
4130        End Do
         Endif
         If(NREOUT.GT.0) Then
            Do 4160 I=1,NREOUT
               J=0
4140           J=J+1
               If(J.GT.NRES) Then
                  Write(14,4150) REOUID(I)
4150              Format(' WARNING: ',A8,' on RO record',
     +                   ' is not on any WS record.')
                  Goto 4160
               Endif               
               If(RESID(J).NE.REOUID(I)) Goto 4140
4160        End Do
         Endif
      Endif
!______________________________________________________________________
!
!  Integer reservoir identifiers are assigned to each reservoir
!  character identifier entered in field 8 of a TO record.
!
      If(NTORES.GT.0) Then
         Do 4190 I=1,NTORES
            If(TORES(I).EQ.'      ') Goto 4190
            K=0
4170        K=K+1
            If(K.GT.NRES) Then
               Write(14,*) ' '
               Write(14,4180) TORES(I)
4180           Format(' ERROR: Reservoir ',A6,' entered in field 8',
     +                ' of a TO record is not on any WS record.')
               Write(14,5000)
               Call ERROR
            Endif
            If(TORES(I).EQ.RESID(K)) Then
               TORI(I)=K
            Goto 4190
            Endif
            Goto 4170
4190     End Do
      Endif
!
!  Integer water right identifiers are assigned to each water right
!  identifier entered in field 9 of a TO record.
!
      If(NTOWR.GT.0) Then
         Do 4220 I=1,NTOWR
            If(TOWR(I).EQ.'                ') Goto 4220
            K=0
4200        K=K+1
            If(K.GT.NWRTS) Then
               Write(14,*) ' '
               Write(14,4210) TOWR(I)
4210           Format(' ERROR: Water right ',A16,' entered in field',
     +                ' 9 of a TO record is not on any WR record.')
               Write(14,5000)
               Call ERROR
            Endif
            If(TOWR(I).EQ.WRID(K)) Then
               TOWI(I)=K
            Goto 4220
            Endif
            Goto 4200
4220     End Do
      Endif
!
!  Check that WS and EA records are properly connected.
!
      If(ICHECK.GE.1) Then
         Do 4320 I=1,NRES
            If(EAFLAG.GT.0) Then
               Do K=1,EAFLAG
                   Do  J=1,10
                      If(EARES(K,J).EQ.RESID(I).and.EAR(I).NE.K) Then
                         Write(14,*)' '
                         Write(14,4290) RESID(I),K,EAR(I)
4290                     Format(' ERROR: Reservoir ',A6,' on EA ',
     +                          'record ',I2,' has EAR of ',I2,
     +                          ' on WS record.')
                         Write(14,5000)
                         Call ERROR 
                      Endif
                   End Do
               End Do
            Endif
            If(EAR(I).GT.0) Then
               If(EAFLAG.LT.EAR(I)) Then
                  Write(14,*)' '
                  Write(14,4300) RESID(I),EAR(I)
4300              Format(' ERROR: Reservoir ',A6,' has EAR of ',I2,
     +                   ' on WS record but has no EA record.')
                  Write(14,5000)
                  Call ERROR 
               Else
                  J=0
4310              J=J+1
                  If(EARES(EAR(I),J).NE.RESID(I)) Goto 4320
                  If(J.LT.10) Goto 4310
                  Write(14,*)' '
                  Write(14,4300) RESID(I),EAR(I)
                  Write(14,*) ' '
               Endif
            Endif
4320     End Do
      Endif
!
!  If a FY record is read with more than one water right (FYN>1),
!  multiplier factors FYFACT(FYN) are computed.
!
      If(FYLEVEL.NE.0) Then
         If(FYN.EQ.0) Then
            Write(14,*)' '
            Write(14,4330) 
4330        Format(' ERROR: The FY record water right identifier did',
     +             ' not match any water right on WR records.')
            Write(14,5000)
            Call ERROR
         Elseif(FYN.GT.100) Then
            Write(14,*)' '
            Write(14,4340) FYN
4340        Format(' ERROR: The FY record water right identifier',
     +             ' matched',I3,' water rights on WR records,',/,
     +             8x,'which exceeds the dimension limit of 100.')
            Write(14,5000)
            Call ERROR
         Elseif(FYN.EQ.1.or.MFY.EQ.2) Then
            FYFACT(1)=1.0
         Elseif(FYN.GT.1.and.MFY.NE.2) Then
            TOTAL=0.0
            Do J=1,FYN
               I=FYWR(J)
               FYFACT(J)=WRDAT(I,1)
               TOTAL=TOTAL+FYFACT(J)
            End Do
            Do J=1,FYN
               FYFACT(J)=FYFACT(J)/TOTAL
            End Do
         Endif
      Endif
!
!  Connections between DI records and DINDEX on WR/IF records are checked.
!
      If(ICHECK.GE.1) Then
         J=0
         K=99999
         L=0
         Do I=1,NWRTS
            If(Abs(DINDEX(I)).GT.0) Then
               J=J+1
               If(Abs(DINDEX(I)).LT.K) K=Abs(DINDEX(I))
               If(Abs(DINDEX(I)).GT.L) L=Abs(DINDEX(I))
               If(Abs(DINDEX(I)).GT.MAXDI) Then
                  Write(14,*)' '
                  Write(14,4350) DINDEX(I),Adjustl(WRID(I)),MAXDI
4350              Format(' ERROR: The drought index DINDEX from the',
     +             ' WR/IF record is',I3,' for right ',A16,/,8x,'but',
     +             ' there are only',I3,' DI records.')
                  Write(14,5000)
                  Call ERROR
               Endif
            Endif
         End Do
         If(MAXDI.GT.0) Then
            If(J.LT.MAXDI.or.K.NE.1.or.L.NE.MAXDI) Then
               If(J.EQ.0) Then
                  Write(14,4360) MAXDI
4360              Format(' WARNING: No WR/IF records have drought ',
     +                   'indices but there are',I3,' DI records.')
               Else
                  Write(14,4370) J,K,L,MAXDI
4370              Format(' WARNING:',I3,' rights have drought indices',
     +                   ' ranging from',I3,' to',I3,' and MAXDI is',I3)
               Endif
            Endif
         Endif
!
!  Counts of water rights, control points, and reservoirs are checked.
!
         J=MAXWR-1
         K=MAXCP-1
         L=MAXWS-1
         If(J.NE.NWRTS) Write(14,4380) NWRTS,MAXWR
4380     Format(' WARNING: Inconsistency in water right counts.',/,9x,
     +          ' NWRTS is',I4,' and MAXWR is',I4)
         If(K.NE.NCPTS) Write(14,4390) NCPTS,MAXCP
4390     Format(' WARNING: Inconsistency in control point counts.',/,9x,
     +          ' NCPTS is',I4,' and MAXCP is',I4)
         If(L.LT.NRES) Write(14,4400) NRES,MAXWS
4400     Format(' WARNING: Inconsistency in reservoir counts.',/,9x,
     +          ' NRES is',I4,' and MAXWS is',I4)
!
!  Counts of SV/SA tables and PV/PE tables are checked.
!
         If(NTABLE.GT.MAXTAB) Write(14,4410) NTABLE,MAXTAB
4410     Format(' WARNING: Inconsistency in SV/SA table counts.',/,7x,
     +          ' NTABLE is',I4,' and MAXTAB is',I4)
         If(NPTABL.GT.MAXPOW) Write(14,4420) NPTABL,MAXPOW
4420     Format(' WARNING: Inconsistency in PV/PE table counts.',/,7x,
     +          ' NTABLE is',I4,' and MAXTAB is',I4)
      Endif
!
!  Hydropower rights must have PV/PE records.
!
      If(ICHECK.GE.1.and.NUMPOW.GT.0) Then
         Do I=1,NWRTS
             If(WRNUM(I,5).LT.0) Then
                J=SN1(Abs(WRNUM(I,9)),1)
                If(RESNUM(J,4).EQ.0) Then
                   Write(14,4430) Adjustl(WRID(I))
4430               Format(' ERROR: No PV/PE records for hydropower',
     +                    ' right: ',A16)
                   Write(14,5000)
                   Call ERROR
                Endif
                If(WRNUM(I,9).EQ.0) Then
                   Write(14,4440) Adjustl(WRID(I))
4440               Format(' ERROR: No reservoir for hydropower',
     +                    ' right: ',A16)
                   Write(14,5000)
                   Call ERROR
                Endif
             Endif
         End Do
      Endif
!_______________________________________________________________________
!
!  RG record alterations to selected water rights.
!
      If(NUMRG.GT.0) Then
         Write(14,*) '*** Starting RG record operations.'
         Allocate(RGWR(MAXWR))
         Do 4800 I=1,NUMRG
!
!  Integer identifiers are are assigned to control points on RG record
!  along with error check that they match control points on CP records.
!
            Do 4610 K=1,12
               If(RGCP(I,K).EQ.'      ') Goto 4610
               L=0
4600           L=L+1
               If(RGCP(I,K).EQ.CPID(L,1)) Then
                  RGCPI(I,K)=L
                  Goto 4610
               Endif
               If(L.EQ.NCPTS) Then
                  CD='RG'
                  Write(14,5010) RGCP(I,K),CD
                  Write(14,5000)
                  Call ERROR
               Endif
               Goto 4600
4610        End Do
!
!  Control points are selected that do not meet RG record criteria.
!  RGWR(J) of -1 means that one or more criteria are violated.
!  RGWR(J) of 9 means that no criteria are violated.
!
            Do J=1,NWRTS
               RGWR(J)=9
            End Do
            Do 4700 J=1,NWRTS
!
!           WR and IF record rights are either excluded or included.
!
               If(RGI(I,4).LE.1.and.WRIDS(J,1).EQ."IF#IF*IF") Then
                  RGWR(J)=-1
                  Goto 4700
               Elseif(RGI(I,4).EQ.3.and.WRIDS(J,1).NE."IF#IF*IF") Then
                  RGWR(J)=-1
                  Goto 4700
               Endif
!
!           Water right type from WR or IF record field 3.
!
               If(RGI(I,5).GT.0) Then
                  If(WRNUM(J,5).LT.RGI(I,5)) RGWR(J)=-1
               Elseif(RGI(I,5).LT.0) Then
                  If(WRNUM(J,5).GT.Abs(RGI(I,5))) RGWR(J)=-1
               Endif
!
!           Annual target and reservoir capacity.
!
               If(RGA(I,1).GT.0.000001) Then
                  If(WRDAT(J,1).LT.RGA(I,1)) RGWR(J)=-1
               Elseif(RGA(I,1).LT.-0.000001) Then
                  If(WRDAT(J,1).GT.Abs(RGA(I,1))) RGWR(J)=-1
               Endif
               If(Abs(RGA(I,2)).LE.0.000001) Then
                  If(WRNUM(J,9).GT.0) Then
                     X=RESDAT(WRNUM(J,9),1)
                  Else
                     X=0.0
                  Endif
               Endif
               If(RGA(I,2).GT.0.000001) Then
                  If(X.LT.RGA(I,2)) RGWR(J)=-1
               Elseif(RGA(I,2).LT.-0.000001) Then
                  If(X.GT.Abs(RGA(I,2))) RGWR(J)=-1
               Endif
!
!           Water right priority number.
!
               If(RGI(I,6).GT.0) Then
                  If(WRNUM(J,7).LT.RGI(I,6)) RGWR(J)=-1
               Elseif(RGI(I,6).LT.0) Then
                  If(WRNUM(J,7).GT.Abs(RGI(I,6))) RGWR(J)=-1
               Endif
               If(RGWR(J).EQ.-1) Goto 4700
!
!           Group identifiers from WR record.
!
               If(RGID(I,2).NE.'        ') Then
                 If(RGID(I,2).NE.WRIDS(J,1).and.RGID(I,2).NE.WRIDS(J,2))
     +               RGWR(J)=-1
               Endif
               If(RGID(I,3).NE.'        ') Then
                  If(RGID(I,3).EQ.WRIDS(J,1).or.RGID(I,3).EQ.WRIDS(J,2))
     +               RGWR(J)=-1
               Endif
               If(RGWR(J).EQ.-1) Goto 4700
!
!           Downstream control point limit RGCP(I,2) from RG field 14.
!
               If(RGCPI(I,2).GT.0) Then
                  L=WRNUM(J,1)
4620              If(L.EQ.RGCPI(I,2)) Goto 4630
                  L=CPNXT(L)
                  If(L.LT.0) Then
                     RGWR(J)=-1
                     Goto 4700
                  Endif
                  Goto 4620
               Endif
!
!           Downstream control point limits from Supplemental RG record.
!
4630           If(RGI(I,7).GT.0) Then
                  L=WRNUM(J,1)
                  M=3
                  N=RGI(I,7)+2
                  Do K=M,N
4640                 If(L.EQ.RGCPI(I,K)) Goto 4650
                     L=CPNXT(L)
                     If(L.LT.0) Then
                        RGWR(J)=-1
                        Goto 4700
                     Endif
                     Goto 4640
                  End Do
               Endif
!
!           Upstream control point limits from Supplemental RG record.
!
4650           If(RGI(I,8).GT.0) Then
                  M=RGI(I,7)+3
                  N=M+RGI(I,8)-1
                  Do K=M,N
                     L=WRNUM(J,1)
                     If(L.EQ.RGCPI(I,K)) Goto 4670
4660                 L=CPNXT(L)
                     If(L.EQ.RGCPI(I,K)) Goto 4670
                     If(L.LT.0.and.K.EQ.N) Then
                        RGWR(J)=-1
                        Goto 4700
                     Endif
                     If(L.GT.0) Goto 4660
                  End Do
               Endif
!
4670           If(RGI(I,9).GT.0) Then
                  M=RGI(I,7)+RGI(I,8)+3
                  N=M+RGI(I,9)-1
                  Do K=M,N
                     L=RGCPI(I,K)
                     If(L.EQ.WRNUM(J,1)) Goto 4700
4680                 L=CPNXT(L)
                     If(L.EQ.WRNUM(J,1)) Goto 4700
                     If(L.LT.0.and.K.EQ.N) Then
                        RGWR(J)=-1
                        Goto 4700
                     Endif
                     If(L.GT.0) Goto 4680
                  End Do
               Endif
!
!  End of water rights loop that selects the water right group.
!
4700        End Do
!
!  Parameters in RG record fields 3-6 are assigned to the selected rights.
!
            Do J=1,NWRTS
               If(RGWR(J).EQ.9) Then
                  If(RGID(I,1).NE.'        ') WRIDS(J,2)=RGID(I,1)
                  If(RGI(I,2).NE.0) DUAL(J)=RGI(I,2)
                  If(RGI(I,3).NE.0) Then
                     If(WRNUM(J,8).EQ.0) Then
                        XPX=XPX+1
                        WRNUM(J,8)=XPX
                     Endif
                     XCP(WRNUM(J,8))=RGI(I,3)
                     XCPI(WRNUM(J,8))=RGCPI(I,1)
                     XP(WRNUM(J,8))=0
                  Endif
               Endif
            End Do
!
!  RG record selected water rights are listed in the message file.
!
            If(RGI(I,1).GT.0) Then
               Write(14,4710) I
4710           Format(/,'RG Record Water Right Group',I2,/)
               Do 4730 J=1,NWRTS
                  If(RGWR(J).EQ.-1) Goto 4730
                  RGWRID=Adjustl(WRID(J))
                  RG1=Adjustl(WRIDS(J,1))
                  RG2=Adjustl(WRIDS(J,2))
                  X=0.0
                  If(WRNUM(J,9).GT.0) X=RESDAT(WRNUM(J,9),1)
                  If(WRNUM(J,8).EQ.0) Then
                     Write(14,4720) RGWRID,RG1,RG2,CPID(WRNUM(J,1),1),
     +                  WRDAT(J,1),X,WRNUM(J,7),WRNUM(J,5),DUAL(J)
                  Elseif(XCPI(WRNUM(J,8)).EQ.0) Then
                     Write(14,4720) RGWRID,RG1,RG2,CPID(WRNUM(J,1),1),
     +                  WRDAT(J,1),X,WRNUM(J,7),WRNUM(J,5),DUAL(J),
     +                  XCP(WRNUM(J,8))
                  Else
                     Write(14,4720) RGWRID,RG1,RG2,CPID(WRNUM(J,1),1),
     +                  WRDAT(J,1),X,WRNUM(J,7),WRNUM(J,5),DUAL(J),
     +                  XCP(WRNUM(J,8)),CPID(XCPI(WRNUM(J,8)),1)
                  Endif
4720              Format(A16,1x,A8,1x,A8,2x,A6,2F10.0,I10,I4,2I4,2x,A6)
4730           End Do
               Write(14,*)
            Endif
4800     End Do
         Write(14,*) '*** Finished RG record operations.'
      Endif
!
!  Back-up right identifiers are assigned integer indices.
!
      If(ISO.GT.0) Then
         Do 4980 I=1,NWRTS
            N=WRTO(I)
            If(N.GT.0) Then
               If(BUID(N).NE.'                ') Then
                  J=0
4930              J=J+1
                  If(WRID(J).EQ.BUID(N)) Then
                     BUWR(N)=J
                     Goto 4950
                  Endif
                  If(J.EQ.NWRTS) Then
                     Write(14,4940) BUID(N),Adjustl(WRID(I))
4940                 Format(' ERROR: BUWRID of ',A16,' for right ',A16,
     +                      /,8x,'matches no WRID on WR or IF records.')
                     Write(14,5000)
                     Call ERROR
                  Endif
                  Goto 4930
               Endif
4950           If(BUG(N).NE.'        ') Then
                  J=0
4960              J=J+1
                  If(WRIDS(J,1).EQ.BUG(N).or.WRIDS(J,2).EQ.BUG(N)) Then
                     Goto 4980
                  Endif
                  If(J.EQ.NWRTS) Then
                     Write(14,4970) BUGROUP,Adjustl(WRID(I))
4970                 Format(' ERROR: BUG of ',A8,' for right ',A16,
     +                      /,8x,'matches no WRID on WR or IF records.')
                     Write(14,5000)
                     Call ERROR
                  Endif
                  Goto 4960
               Endif
            Endif
4980     Enddo
      Endif
!_______________________________________________________________________
!
!  Additional error and warning messages.
!
5000  Format('        Stopped from Subroutine READDAT due to error.')
5010  Format(' ERROR: Control point identifier ',A6,' from a ',A2,
     +       ' record',/,8x,'does not match any identifier on the CP',
     +       ' records.')
!
      If(NCPTS.LT.1.or.NWRTS.LT.1) Then
         Write(14,*)' '
         Write(14,5020) 
5020     Format(' ERROR: Number of control points and water rights, ',/,
     +          '        must each be at least one.')
         Write(14,5030) NCPTS,NWRTS
5030     Format('        NCPTS =',I5,'NWRTS =',I5) 
         Write(14,5000)
         Call ERROR
      Endif
!
      If(SWR.EQ.0.and.F7.GE.1) Then
         Write(14,5040)
5040     Format(' WARNING: A HRR file is activated, but there are',
     +          ' no system water rights.')
      Endif
!
      If(CR1.GT.0.and.DSS(1).EQ.0) Then
         Write(14,5050)
5050     Format(' WARNING: CRM is activated by CR record, but DSS(1)',
     +          ' of zero on OF record deactivated CRM output file.')
      Endif
      If(CR1.GT.0.and.(DSS(2).EQ.1.or.DSS(3).EQ.1)) Then
         Write(14,5060)
5060     Format(' WARNING: Some CR record CRM options will not work',
     +          ' with a DSS or SOU output file.')
      Endif
!
!  IOSTAT error message
!
6000  If(STATUS.NE.0) Then
         Write(14,*)' '
         Write(14,6010) CD,STATUS
6010     Format(' ERROR: Fortran IOSTAT error occurred reading an ',
     +       /,8x,'input record with CD identifier of ',A2,/
     +       '        IOSTAT status variable =',I6)
         Write(14,5000)
         Backspace(3)
         Backspace(3)
         Write(14,*)' '
         Write(14,6020)
6020     Format('The first 82 characters of each of the last',
     +          ' two records read are as follows:')
         Read(3,6030) CD,TITLE1
         Write(14,6030) CD,TITLE1
         Read(3,6030) CD,TITLE1
         Write(14,6030) CD,TITLE1
         Write(14,*)' '
6030     Format(A2,A80)
         Call ERROR
      Endif
!
!  Return to main program.
!
      If(NREOUT.LT.0) NREOUT=NRES
      Return
!
!  Error message is written if end-of-file is read. Read(END=7000)
!
7000  Write(14,7010) CD
7010  Format(' ERROR: Inappropriately reached end-of-file while',
     +       ' reading DAT file.',/,8x,'The last CD read by',
     +       ' Subroutine READDAT was ',A2)
      Write(14,5000)
      Call ERROR
!
!  End of Subroutine READDAT
!
      End Subroutine READDAT
!
! ***************************************************************************
!
      Subroutine DROUGHT
!
!  Subroutine DROUGHT determines the drought index DIFACT(DI) as a function of
!  storage. DIFACT(DI) is determined by first computing the total DI reservoir
!  storage and then applying linear interpolation to the DI relationship from
!  IS versus IP records to determine the drought index multiplier DIFACT(DI).
!
      Use COMVAR
      Real DISUM(MAXDI),X
      Integer DI,J,K,N,L
!
      Do 60 DI=1,DIFLAG
         DISUM(DI)=0.0
         N=DINUM(DI)
         Do 20 K=1,NRES
            If(N.EQ.99) Then   
               If(STODI.LE.0) Then
                  DISUM(DI)=DISUM(DI)+RESDAT(K,5)
               Elseif(STODI.EQ.1) Then
                  DISUM(DI)=DISUM(DI)+RESDAT(K,6)
               Elseif(STODI.EQ.2) Then
                  DISUM(DI)=DISUM(DI)+((RESDAT(K,5)+RESDAT(K,6))/2.0)
               Endif
               If(EMPTY(DI).LT.0.or.EMPTY(DI).EQ.MT) RESDAT(K,5)=0.0
            Else
               Do 10 J=1,N
                  If(RESID(K).EQ.DIRES(DI,J)) Then
                     If(STODI.LE.0) Then
                        DISUM(DI)=DISUM(DI)+RESDAT(K,5)
                     Elseif(STODI.EQ.1) Then
                        DISUM(DI)=DISUM(DI)+RESDAT(K,6)
                     Elseif(STODI.EQ.2) Then
                        DISUM(DI)=DISUM(DI)+((RESDAT(K,5)+RESDAT(K,6))
     +                                                  /2.0)
                     Endif
                     If(EMPTY(DI).EQ.-1.or.EMPTY(DI).EQ.MT)
     +                                      RESDAT(K,5)=0.0
                  Endif
10             End Do
            Endif
20       End Do
         DIFACT(DI)=1.0
         L=0
30       L=L+1
         If(L.GE.TL) Then
            Write(14,*) ' '
            Write(14,40) DI,YEAR,MT,DISUM(DI)
40          Format(' ERROR: Interpolation of drought index',I3,
     +             ' is out of range.',/,8x,'Year:',I5,'   MT:',I3,
     +             '      Storage =',F10.1)
            Write(14,50)
50          Format(8x,'Stopped in Subroutine DROUGHT due to error.') 
            Call ERROR
         Else
            If(DIPER(DI,L+1).LE.-1.0) Then
               Write(14,40) DI,YEAR,MT,DISUM(DI)
               Write(14,50)
               Call ERROR
            Endif
         Endif
         X=DISUM(DI)
         If(Abs(X-DISTO(DI,L)).LE.0.0001) Then
            DIFACT(DI)=DIPER(DI,L)/100.0
         Elseif(Abs(X-DISTO(DI,L+1)).LE.0.0001) Then
            DIFACT(DI)=DIPER(DI,L+1)/100.0
         Elseif((X.GE.DISTO(DI,L).and.X.LE.DISTO(DI,L+1)).or.
     +          (X.LE.DISTO(DI,L).and.X.GE.DISTO(DI,L+1))) Then
            DIFACT(DI) = ((X-DISTO(DI,L))/(DISTO(DI,L+1)-
     +          DISTO(DI,L)))*(DIPER(DI,L+1)-DIPER(DI,L))+DIPER(DI,L)
            DIFACT(DI)=DIFACT(DI)/100.0
         Else
            Goto 30
         Endif
60    End Do
      Return
      End Subroutine DROUGHT
!
! ***************************************************************************
!
      Subroutine CONFLU(K)
!
!  Subroutine CONFLU finds the first downstream control point that is common
!  to both the water right location and each associated reservoir.
!
!  SN2(SWR,I) equal -1 if releases are conveyed through pipelines/canals.
!  SN2(SWR,I) equal zero if the reservoir is upstream of the water right.
!  SN2(SWR,I) equal index number of the first common downstream control point
!    if the reservoir can release to mitigate the effects of downstream senior
!    rights. This is the case only when the reservoir can release only to a 
!    point that is downstream of the water right location.
!
      Use COMVAR
!
      Integer I,K,WRPT,RESPT,DIVLOC
!
      DIVLOC=WRNUM(K,1)
      If(WRNUM(K,10).GT.0) DIVLOC=WRNUM(K,10)
      Do 30 I=1,NSR(SWR)
          WRPT=DIVLOC
!
!  If SN2((I) is flagged in input with a negative number, releases are not
!  constrained by location nor downstream senior depletions. SN2((I) will
!  then be set to -1 for use by subroutine RELEASE in making system
!  reservoir release decisions.
!
          If(SN2(SWR,I).LT.0) Then
              SN2(SWR,I)=-1
              Goto 30
          Endif
10        RESPT=RESNUM(SN1(SWR,I),1)
20        If(RESPT.EQ.DIVLOC) Then
              SN2(SWR,I)=0
              Goto 30
          Endif
          If(RESPT.LT.0.and.WRPT.LT.0) Then
              SN2(SWR,I)=-1
              Goto 30
          Endif
          If(RESPT.LT.0) Then
              WRPT=CPNXT(WRPT)
              Goto 10
          Endif
          If(WRPT.EQ.RESPT) Then
              SN2(SWR,I)=WRPT
              Goto 30
          Endif
          RESPT=CPNXT(RESPT)
          Goto 20
30    End Do
      Return
      End Subroutine CONFLU
!
! ***************************************************************************
!
      Subroutine RANKWR
!
!  Subroutine RANKWR develops an array that identifies the water rights in
!  priority order.  NPOPT from JO record field 9 sets alternative options.
!
      Use COMVAR
!
      Integer I,K,II
!
      If(NPOPT.NE.0) Then
         Write(14,10) NPOPT
10       Format(' *** JO record NPOPT priority option',I2,' activated.')
      Endif
!
      Do I=1,NWRTS
         RANK(I)=I
      End Do
      If(NPOPT.EQ.2) Return
      Do I=1,NWRTS-1
         Do II=I+1,NWRTS
            If(NPOPT.EQ.3) Then
               If((WRNUM(RANK(II),1).LT.WRNUM(RANK(I),1)).or.
     +            (WRNUM(RANK(II),1).EQ.WRNUM(RANK(I),1).and.
     +            RANK(II).LT.RANK(I))) Then
                  K=RANK(II)
                  RANK(II)=RANK(I)
                  RANK(I)=K
               Endif
            Else
               If((WRNUM(RANK(II),7).LT.WRNUM(RANK(I),7)).or.
     +            (WRNUM(RANK(II),7).EQ.WRNUM(RANK(I),7).and.
     +            RANK(II).LT.RANK(I))) Then
                  K=RANK(II)
                  RANK(II)=RANK(I)
                  RANK(I)=K
               Endif
            Endif
         End Do
      End Do
      Return
      End Subroutine RANKWR
!
! ***************************************************************************
!
      Subroutine NATURAL
!
!  Subroutine NATURAL ranks water rights in natural order, with upstream rights
!  having higher priorities relative to downstream rights.  Subroutine NATURAL
!  is an optional alternative to Subroutine RANKWR.  The natural priority
!  option is activated by NPOPT from the the JO record.
!
      Use COMVAR
!
      Integer I,K,M,CRANK
      Integer CPRANK(NCPTS),CPNXTAUX(NCPTS),RANKTEMP(NWRTS)
!
!  Initialize variables.
!
      RANK=0
      Do I=1,NCPTS
         CPNXTAUX(I)=CPNXT(I)
         CPRANK(I)=0
      End Do
      CRANK = 500000
!
!  Rank control points in upstream to downstream order.
!
      Do While (Still_Unranked(CPRANK))
         Do I=1,NCPTS
            If((CPNXTAUX(I).EQ.-99).and.(CPRANK(I).EQ.0)) Then 
               CPRANK(I)=CRANK
               CRANK = CRANK - 100
               Do K=1,NCPTS
                  If(CPNXTAUX(K).EQ.I) CPNXTAUX(K)=-99
               End Do
            End If
         End Do
      End Do
! 
!  Rank of water rights is equal to the rank of their control point.
!
      Do I=1,NWRTS
         RANKTEMP(I)=CPRANK(WRNUM(I,1))
      End Do
!
!  If several water rights are located at the same control point, different ranks 
!  are assigned based on priority numbers.
!
      Do I=1,NCPTS
         Do K=1,NWRTS
            If (WRNUM(K,1).EQ.I) Then
               Do M=K+1,NWRTS
                  If (WRNUM(K,7).GT.WRNUM(M,7).and.(WRNUM(M,1).EQ.I))
     +            RANKTEMP(K) = RANKTEMP(K)+1
                  If (WRNUM(K,7).LE.WRNUM(M,7).and.(WRNUM(M,1).EQ.I))
     +            RANKTEMP(M) = RANKTEMP(M)+1
               End Do
            End If
         End Do
      End Do
!
!  RANK is converted to an array with numbers between 0 and NWRTS.
!
      Do I=1,NWRTS
         M=1
         Do K=1,NWRTS
            If (RANKTEMP(K).LT.RANKTEMP(I)) M=M+1
         End Do
         Do While (RANK(M).NE.0)
            M=M+1
         End Do
         RANK(M)=I
      End Do
      Return
!
!  Function STILL_UNRANKED determines if there are still control points
!  left to assign a natural rank. (Some value of current rank is still 0.)
!
      Contains
      Logical Function STILL_UNRANKED(ARRAYS)
         Integer II,ARRAYS(NCPTS)
         Logical STILL
         II=1
         STILL = .false.
         Do While (II.LE.NCPTS)
            If (ARRAYS(II).EQ.0) Then
               STILL =.TRUE.
               Exit
            Else
               II=II+1
            End If
         End Do
      STILL_UNRANKED = STILL
      End Function STILL_UNRANKED
!
      End Subroutine NATURAL
!
! ***************************************************************************
!
      Subroutine INEVYR
!
!  Subroutine INEVYR reads the inflow IN and evaporation EV records and
!  arranges them in the same order as the control points.
!
      Use COMVAR
!
      Integer J,K,L,X,Y,M,IOUNIT,FYR,LYR
      Character(len=6) CPTID(MAXCP),REPEAT
      Logical YES
      Real,Allocatable,Dimension(:,:)::DUMMY
      Allocate(DUMMY(MAXCP,MAXMON))
!
      DUMMY=0.0
      EVAPR=0.0
      INFLOW=0.0
      CPTID='      '
!
!  First year of simulation only.
!
      If(YEAR.EQ.YRST) Then
         INLYR=0
         EVLYR=0
!
!  File selection: DAT (UNIT=3), HYD (UNIT=12), INF and EVA (UNITS 1 and 2)
!
         IUNIT=3
         EUNIT=3
         INQUIRE(UNIT=12,OPENED=YES)
         If(YES) IUNIT=12
         If(YES) EUNIT=12
         INQUIRE(UNIT=1,OPENED=YES)
         If(YES) IUNIT=1
         INQUIRE(UNIT=2,OPENED=YES)
         If(YES) EUNIT=2
         If(IUNIT.EQ.1.and.EUNIT.EQ.3) EUNIT=-99
!
!  Error checks for first IN record.
!
120      Read(IUNIT,130,IOSTAT=STATUS) CD
         If(STATUS.NE.0) Goto 900
130      Format(A2)
         If(CD.EQ.'**') Then
            Goto 120
         Elseif(CD.EQ.'IN') Then
            Backspace(IUNIT)
            Read(IUNIT,140,IOSTAT=STATUS) CD,FYR,LYR
140         Format(A2,6x,I4,I4)
            If(STATUS.NE.0) Goto 900
            If(LYR.LT.YRST) Goto 120
            If(FYR.GT.LYR.or.LYR.LT.0) Then
               Write(14,*) ' '
               Write(14,150) YEAR,FYR,LYR
150            Format(' ERROR: In reading first IN record for first ',
     *                'year',I5,' read FYR of',I5,' and INLYR of',I5)
               Write(14,800)
               Call ERROR
            Endif
            Backspace(IUNIT)
         Else
            Write(14,*) ' '
            Write(14,160) CD
160         Format(' ERROR: In reading first IN record, read CD of ',
     +              A2,' instead of IN.')
            Write(14,800)
            Call ERROR
         Endif
      Endif
!
!  Read pass comment records.
!
170   Read(IUNIT,130,IOSTAT=STATUS) CD
      If(STATUS.NE.0) Goto 900
      If(CD.EQ.'**') Goto 170
      Backspace(IUNIT)
!
!  Read inflows from file root.FLO (unit=1), if unit 1 has been opened,
!  or otherwise from the file root.dat (unit=3).
!
      If(IUNIT.EQ.1.or.IUNIT.EQ.3) Then
         Do 210 J=1,NCPTS
            If(INMETHOD(J).GT.1) Goto 210
            If(YEAR.GT.YRST) Then
               If(INLYR(J).GE.YEAR) Goto 210
            Endif
            Read(IUNIT,180,IOSTAT=STATUS) CD,CPTID(J),FYR,INLYR(J),
     +                                   (DUMMY(J,L),L=1,NPRDS)
180         Format(A2,A6,I4,I4,12F8.0)
            CPTID(J)=Adjustr(CPTID(J))
            If(STATUS.NE.0) Goto 900
            If(ICHECK.EQ.6) Write(14,180) CD,CPTID(J),FYR,INLYR(J),
     +                                   (DUMMY(J,L),L=1,NPRDS)
            If(INLYR(J).LT.YEAR) Then
               Write(14,*) ' '
               Write(14,190) CPTID(J),YEAR,INLYR(J)
190            Format(' ERROR: In reading IN records for control point '
     +                ,A6,' for year',I5,' read INLYR of',I5)
               Write(14,800)
               Call ERROR
            Endif
            If(CD.NE.'IN') Then
               Write(14,*) ' '
               Write(14,200) YEAR,CD
200            Format(' ERROR: In reading IN records for year ',I4,
     +         ' a CD of ',A2,' was read.')
               Write(14,800)
               Call ERROR
            Endif
210      End Do
      Endif
!
!  Otherwise, read inflows from root.HYD (unit=12) in the old WRAP3 format.
!
      If(IUNIT.EQ.12) Then
         Do 260 J=1,NCPTS
            If(INMETHOD(J).GT.1) Goto 260
            If(YEAR.GT.YRST) Then
               If(INLYR(J).GE.YEAR) Goto 260
            Endif
            Read(IUNIT,220,IOSTAT=STATUS) CD,CPTID(J),INLYR(J),REPEAT
220         Format(A2,A6,I8,A8)
            CPTID(J)=Adjustr(CPTID(J))
            REPEAT=Adjustr(REPEAT)
            If(STATUS.NE.0) Goto 900
            If(CD.NE.'IN') Then
               Write(14,*)' '
               Write(14,200) YEAR,CD
               Write(14,800) 
               Call ERROR
            Endif
            If(INLYR(J).GT.0) Then
               Backspace(IUNIT)
               X=Min(6,NPRDS)
               Read(IUNIT,230,IOSTAT=STATUS) CD,CPTID(J),INLYR(J),
     +                                      (DUMMY(J,L),L=1,X)
230            Format(A2,A6,I8,6F8.0)
               CPTID(J)=Adjustr(CPTID(J))
               If(STATUS.NE.0) Goto 900
               X=7
240            If(NPRDS.GE.X) Then
                  Y=Min(X+5,NPRDS)
                  Read(IUNIT,250,IOSTAT=STATUS) CD,(DUMMY(J,L),L=X,Y)
250               Format(A2,14X,6F8.0)
                  If(STATUS.NE.0) Goto 900
                  If(CD.NE.'IN') Then
                     Write(14,*)' '
                     Write(14,200) YEAR,CD
                     Write(14,800) 
                     Call ERROR
                  Endif
                  X=X+6
                  Goto 240
               Endif
            Else
               INLYR(J)=-INLYR(J)
               Do K=1,J-1
                  If(CPTID(K).EQ.REPEAT) Then
                     Do M=1,NPRDS
                        DUMMY(J,M)=DUMMY(K,M)
                     End Do
                  Endif
               End Do
            Endif
260      End Do
      Endif
!
!  Error checks.
!
      If(ICHECK.EQ.1) Then
         Do 268 K=1,NCPTS
            If(CPTID(K).EQ.'      ') Goto 268
            J=0
262         J=J+1
            If(CPTID(K).EQ.CPID(J,1)) Then
               If(INMETHOD(J).GT.1) Then
                  Write(14,264) CPTID(K),INMETHOD(J),YEAR
264               Format(' WARNING: IN record was found for control',
     +                   ' point ',A6,' which has INMETHOD of',I2,/,9x,
     +                   ' The input error occurred in year',I5)
               Endif
               Goto 268
            Endif
            If(J.LT.NCPTS) Goto 262
            Write(14,266) CPTID(K)
266         Format(' WARNING: IN record was found for control',
     +             ' point ',A6,' which has no CP record.')
268      End Do
      Endif
!
!  This loop places the inflows at the respective control points
!  in the same order as the control points and multiplies by the
!  factors from the CP records.
!
      NIN=0
      Do 290 J=1,NCPTS
         If(INMETHOD(J).LE.1) NIN=NIN+1
         If(INMETHOD(J).GT.1) Goto 290
         K=0
270      K=K+1    
         If(CPTID(K).EQ.CPID(J,1)) Then
            Do L=1,NPRDS
               INFLOW(J,L)=DUMMY(K,L)*CPDT(J,1)*INX
            End Do
            Goto 290
         Endif
         If(K.LT.NCPTS) Goto 270
         Write(14,*) ' '
         Write(14,280) YEAR,CPID(J,1)
280      Format(' ERROR: IN record was not found for year ',I4,
     *          ' for control point ',A6)
         Write(14,800)
         Call ERROR
290   End Do
!
      If(ICHECK.EQ.1.and.NIN.NE.NINCP) Write(14,295) NIN,NINCP,YEAR
295   Format(' WARNING: Read',I3,' IN records but',I3,
     +       ' CP records have INMETHOD option 1.',/,9x,
     +       ' The input error occurred in year',I5)
!
!  If no IN records are input for the control point, inflows are assigned
!  values of zero or the values from another control point.
!
      Do 340 J=1,NCPTS
         If(INMETHOD(J).EQ.2) Then
            If(CPIN(J).EQ.'      '.or.CPIN(J).EQ.'  ZERO') Then
               Do L=1,NPRDS
                  INFLOW(J,L)=0.0
               End Do
               If(CPIN(J).EQ.'      '.and.YEAR.EQ.YRST.and.ICHECK.EQ.1)
     +         Then
                  Write(14,300) CPID(J,1)
300               Format(' WARNING: Inflows are not provided for ',
     +             'control point ',A6,'. Zero flows are assumed.')
               Endif
            Else
               K=0
310            K=K+1
               If(CPIN(J).EQ.CPID(K,1)) Then
                  Do L=1,NPRDS
                     INFLOW(J,L)=INFLOW(K,L)*CPDT(J,1)
                  End Do
                  If(INMETHOD(K).GT.1.and.YEAR.EQ.YRST.and.ICHECK.EQ.1)
     +            Then
                     Write(14,320) CPID(J,1),CPID(K,1)
320                  Format(' WARNING: CPIN repeats IN records from a',
     +                   ' CP that has no IN records.',/,10x,'Control',
     +                   ' points CPID and CPIN are ',A6,' and ',A6)
                  Endif
               Elseif(K.EQ.NCPTS) Then
                  Write(14,*) ' '
                  Write(14,330) CPIN(J),CPID(J,1)
330               Format(' ERROR: CPIN of ',A6,' in field 7 of CP ',
     +                   'record for ',A6,' was not found.')
                  Write(14,800)
                  Call ERROR
               Else
                  Goto 310
               Endif
            Endif
         Endif
340   End Do
!
!  Comment ** records at beginning of EV records and records with year
!  before the first year of simulation are skipped.
!  If there are no EV records, return to main program.
!
      If(EUNIT.EQ.-99) Goto 960
500   Read(EUNIT,130,END=960) CD
      If(CD.EQ.'**') Goto 500
      Backspace(EUNIT)
      Read(EUNIT,510,END=960) CD,LYR
510   Format(A2,10X,I4)
      If (CD.EQ.'EV'.and.LYR.LT.YEAR) Goto 500
      Backspace(EUNIT)
!
!  Reset CPTID(J) to blank.
!  
      Do J=1,NCPTS
         CPTID(J)='      '
      End Do
!
!  Read evaporation rates from file root.eva (unit 2) if it has been opened
!  or from root.dat
!
      If(EUNIT.EQ.2.or.EUNIT.EQ.3) Then
         Do 560 J=1,NCPTS
            If(CPEV(J).NE.'      ') Goto 560
            If(YEAR.GT.YRST) Then
               If(EVLYR(J).GE.YEAR) Goto 560
            Endif
            Read(EUNIT,520,IOSTAT=STATUS) CD,CPTID(J),FYR,EVLYR(J),
     +                                    (DUMMY(J,L),L=1,NPRDS)
520         Format(A2,A6,I4,I4,12F8.0)
            CPTID(J)=Adjustr(CPTID(J))
            If(STATUS.NE.0) Goto 910
            If(ICHECK.EQ.6) Write(14,530) CD,CPTID(J),FYR,EVLYR(J),
     +                                    (DUMMY(J,L),L=1,NPRDS)
530         Format(A2,A6,I4,I4,12F8.2)
            If(EVLYR(J).LT.YEAR) Then
               Write(14,*) ' '
               Write(14,540) CPTID(J),YEAR,EVLYR(J)
540            Format('ERROR: In reading EV records for control point ',
     *                 A6,' for year',I5,' read EVLYR of',I5)
               Write(14,800)
               Call ERROR
            Endif
            If(CD.NE.'EV') Then
               Write(14,*) ' '
               Write(14,550) YEAR,CD
550            Format(' ERROR: In reading EV records for year ',I4,
     *         ' a CD of ',A2,' was read.')
               Write(14,800)
               Call ERROR
            Endif
560      End Do
!
!  Otherwise, read evaporation rates from root.HYD (unit=12).
!
      Elseif(EUNIT.EQ.12) Then
         Do 610 J=1,NCPTS
            If(CPEV(J).NE.'      ') Goto 610
            If(YEAR.GT.YRST) Then
               If(EVLYR(J).GE.YEAR) Goto 610
            Endif
            READ(EUNIT,220,IOSTAT=STATUS) CD,CPTID(J),EVLYR(J),REPEAT
            If(STATUS.NE.0) Goto 910
            CPTID(J)=Adjustr(CPTID(J))
            If(CD.NE.'EV') Then
               Write(14,*) ' '
               Write(14,550) YEAR,CD
               Write(14,800)
               Call ERROR
            Endif
            If(EVLYR(J).GT.0) Then
               Backspace(EUNIT)
               X=Min(6,NPRDS)
               Read(EUNIT,230,IOSTAT=STATUS) CD,CPTID(J),EVLYR(J),
     +                                      (DUMMY(J,L),L=1,X)
               If(STATUS.NE.0) Goto 910
               X=7
570            If(NPRDS.GE.X) Then
                    Y=Min(X+5,NPRDS)
                    Read(EUNIT,250,IOSTAT=STATUS) CD,(DUMMY(J,L),L=X,Y)
                    If(STATUS.NE.0) Goto 910
                    If(CD.NE.'EV') Then
580                    Write(14,*) ' '
                       Write(14,550) YEAR,CD
                       Write(14,800)
                       Call ERROR
                    Endif 
                    X=X+6
                    Goto 570
               Endif
            Elseif(EVLYR(J).LT.0) Then
               EVLYR(J)=-EVLYR(J)
               Do 600 K=1,J-1
                  If(CPTID(K).EQ.REPEAT) Then
                     Do 590 M=1,NPRDS
                        DUMMY(J,M)=DUMMY(K,M)
590                  End Do
                  Endif
600            End Do
            Endif
610      End Do
      Endif
!
!  This loop places the evaporation rates in the same order as the
!  control points and multiplies by the factors from the cp records.
!
620   NEV=0
      Do 660 J=1,NCPTS
         If(CPEV(J).EQ.'      ') NEV=NEV+1
         If(CPEV(J).NE.'      ') Goto 660
         K=0
630      K=K+1 
         If(CPTID(K).EQ.CPID(J,1)) Then
            Do 640 L=1,NPRDS
               EVAPR(J,L)=DUMMY(K,L)*CPDT(J,2)*EVX
640         End Do
            Goto 660
         Endif
         If(K.LT.NCPTS) Goto 630
         If(ICHECK.EQ.1) Write(14,650) CPID(J,1)
650      Format(' WARNING: Net evaporation-precipitation depths are',
     +          ' not provided for control point ',A6,/,10x,'Zero',
     +          ' net evaporation-precipitation depths are assumed.')
660   End Do
!
!  If no EV records are input for the control point, evaporation rates
!  are assigned values of zero or the values from another control point.
!
670   Do 730 J=1,NCPTS
         If(CPEV(J).EQ.'      ') Goto 730
         If(CPEV(J).EQ.'  ZERO') Then
            Do L=1,NPRDS
               EVAPR(J,L)=0.0
            End Do
         Elseif(CPEV(J).EQ.'  NEXT') Then
            NPT=CPNXT(J)
680         If(CPEV(NPT).EQ.'      ') Then
               Do L=1,NPRDS
                  EVAPR(J,L)=EVAPR(K,L)*CPDT(J,2)
               End Do
            Else
               NPT=CPNXT(NPT)
               If(NPT.LT.0) Then
                  Write(14,690) CPID(J,1)
690               Format(' ERROR: CPEV is NEXT for CP ',A6,' but there',
     +                   ' is no downstream CP with EV records.')
                  Call ERROR
               Endif
               Goto 680
            Endif
         Else
            K=0
700         K=K+1
            If(CPEV(J).EQ.CPID(K,1)) Then
               Do L=1,NPRDS
                  EVAPR(J,L)=EVAPR(K,L)*CPDT(J,2)
               End Do
               If(CPEV(K).NE.'      '.and.ICHECK.EQ.1) Then
                  Write(14,710) CPID(J,1),CPID(K,1)
710               Format(' WARNING: CPEV repeats EV records from a CP',
     +                   ' that has no EV records.',/,10x,'Control',
     +                   ' points CPID and CPEV are ',A6,' and ',A6)
               Endif
            Elseif(K.EQ.NCPTS) Then
               Write(14,*) ' '
               Write(14,720) CPEV(J),CPID(J,1)
720            Format(' ERROR: CPEV of ',A6,' in field 8 of CP ',
     +                'record for ',A6,' was not found.')
               Write(14,800)
               Call ERROR
            Else
               Goto 700
            Endif
         Endif
730   End Do
!
!  Additional error checks.
!
      If(ICHECK.EQ.1) Then
         If(NEV.NE.NEVCP) Then
            Write(14,740) NEV,NEVCP
740         Format(' WARNING: Read',I3,' EV records but',I3,
     +             ' CP records have a blank CPEV.',/,9x,
     +             ' The input error occurred in year',I5)
         Endif
         Do 780 K=1,NCPTS
            If(CPTID(K).EQ.'      ') Goto 780
            J=0
750         J=J+1
            If(CPTID(K).EQ.CPID(J,1)) Then
               If(CPEV(J).NE.'      ') Then
                  Write(14,760) CPTID(K),CPEV(J),YEAR
760               Format(' WARNING: EV record was found for control ',
     +            'point ',A6,' which has a CP record field 8 CPEV of ',
     +                   A6,/,10x,'The input error occurred in year',I5)
               Endif
               Goto 780
            Endif
            If(J.LT.NCPTS) Goto 750
            Write(14,770) CPTID(K)
770         Format(' WARNING: EV record was found for control',
     +             ' point ',A6,' which has no CP record.')
780      End Do
      Endif
!
!  Error message.
!
800   FORMAT('        Stopped in Subroutine INEVYR due to error.')
!
!  IOSTAT error message
!
900   IOUNIT=IUNIT
      Goto 920
910   IOUNIT=EUNIT
920   If(STATUS.NE.0) Then
         Write(14,*)' '
         Write(14,930) CD,STATUS
930      Format(' ERROR: Fortran IOSTAT error occurred reading an',
     +       ' input',/,8x,'record with CD identifier of ',A2,/
     +       '        IOSTAT status variable =',I6)
         Write(14,800)
         Write(*,*)' '
         Write(*,930) CD,STATUS
         Write(*,800)
         Write(*,*) ' '
         Backspace(IOUNIT)
         Backspace(IOUNIT)
         Write(14,*)' '
         Write(14,940)
940      Format('The first 80 characters of each of the last',
     +          ' two records read are as follows:')
         Read(IOUNIT,950) CD,TITLE1
         Write(14,950) CD,TITLE1
         Read(IOUNIT,950) CD,TITLE1
         Write(14,950) CD,TITLE1
         Write(14,*)' '
950      Format(A2,A78)
         Call ERROR
      Endif
!
!  End of Subroutine INEVYR
!
960   Deallocate(DUMMY)
      Return
      End Subroutine INEVYR
!
! ***************************************************************************
!
      Subroutine INEV1
!
!  Subroutine INEV1 reads the IN and EV records grouped by control point.
!  Subroutine INEV1 is called once near the beginning of the simulation if
!  JO record INEV is 2 or 4.  FLOW(cp,year,month) and EVAP(cp,year,month)
!  arrays are read by Subroutine INEV1 and used by Subroutine INEV2 to set
!  the INFLOW(cp,month) and EVAPR(cp,month) arrays in the annual loop.
!
      Use COMVAR
      Integer I,J,K,M,IOUNIT,FYR,LYR
      Character(len=6) IDCP,IDCPX
      If(DSS(5).EQ.0) Then
         Allocate(FLOW(NINCP,NYRS,12),EVAP(NEVCP,NYRS,12))
         Allocate(IDCPIN(NINCP),IDCPEV(NEVCP))
      Endif
      Write(14,*) '*** Starting to read IN and EV records.'
      IUNIT=1
      EUNIT=2
      If(INEV.EQ.4) Then
         IUNIT=3
         EUNIT=3
      Endif
      FLOW=0.0
      EVAP=0.0
!
!  Control point and annual loops for IN records.
!
      Do 120 J=1,NINCP
         IDCPX='      '
         YEAR=YRST-1
!
!  The 12 monthly flows are read for each year along with error checks.
!
         Do K=1,NYRS
            YEAR=YEAR+1
10          Read(IUNIT,20,End=960) CD
20          Format(A2)
            If(CD.EQ.'**') Goto 10
            If(CD.EQ.'IN') Then
               Backspace(IUNIT)
               Read(IUNIT,30,IOSTAT=STATUS) CD,IDCP,FYR,LYR,
     +                                    (FLOW(J,K,M),M=1,12)
30             Format(A2,A6,2I4,12F8.0)
               If(STATUS.NE.0) Goto 900
               If(K.EQ.1) IDCPX=IDCP
               If(FYR.LE.0) FYR=LYR
               If(IDCP.NE.'      '.and.IDCP.NE.IDCPX) Then
                  Write(14,*) ' '
                  Write(14,40) IDCP,IDCPX
40                Format(' ERROR: In reading IN records, read control'
     +                   ' point ID of ',A6,' when expecting ',A6)
                  Write(14,800)
                  Call ERROR
               Endif
               If(FYR.LT.YRST) Goto 10
               If(LYR.GT.YEAR) Backspace(IUNIT)
               If(FYR.GT.YEAR.or.FYR.GT.LYR) Then
                  Write(14,*) ' '
                  Write(14,60) IDCP,YEAR,FYR
60                Format(' ERROR: In reading IN records for CP '
     +                ,A6,' for year',I5,' read FYR of',I5)
                  Write(14,800)
                  Call ERROR
               Endif
            Else
               Write(14,*) ' '
               Write(14,70) CD
70             Format(' ERROR: Read CD of ',A2,
     +                ' when expecting IN record.')
               Write(14,800)
               Call ERROR
            Endif
            If(ICHECK.EQ.6) Write(14,30) CD,IDCP,FYR,LYR,
     +                                   (FLOW(J,K,M),M=1,12)
         End Do
!
!  The integer control point identifier is assigned.
!
         I=0
80       I=I+1
         If(IDCPX.EQ.CPID(I,1)) Then
            IDCPIN(J)=I
            Goto 100
         Elseif(I.LT.NCPTS) Then
            Goto 80
         Else
            Write(14,*) ' '
            Write(14,90) CD
90          Format(' ERROR: CP ID of ',A2,' on IN record',
     +             ' matches no identifier on the CP records.')
            Write(14,800)
            Call ERROR
         Endif
!
!  Error check to confirm that INMETHOD from CP record is 0 or 1.
!
100      If(INMETHOD(I).GT.1) Then
            Write(14,*) ' '
            Write(14,110) CPID(I,1),INMETHOD(I)
110         Format(' ERROR: IN records were read for CP ',A6,
     +             ' but INMETHOD =',I3,' on CP record.')
            Write(14,800)
            Call ERROR
         Endif
120   End Do
!
!  IN records must be read for all control points with INMETHOD=1
!
      Do 140 I=1,NCPTS
         If(INMETHOD(I).LE.1) Then
            Do J=1,NINCP
               If(IDCPIN(J).EQ.I) Then
                  Goto 140
               Endif
            End Do
            Write(14,130) CPID(I,1),INMETHOD(I)
130         Format(' WARNING: For CP ',A6,' INMETHOD=',I2,
     +             ' on CP record but there are no IN records.')
         Endif
140   End Do
!
!  Control point and annual loops for EV records.
!
      Do 320 J=1,NEVCP
200      IDCPX='      '
         YEAR=YRST-1
!
!  The 12 monthly depths are read for each year along with error checks.
!
         Do K=1,NYRS
            YEAR=YEAR+1
210         Read(EUNIT,20,End=960) CD
220         Format(A2)
            If(CD.EQ.'**') Goto 210
            If(CD.EQ.'EV') Then
               Backspace(EUNIT)
               Read(EUNIT,230,IOSTAT=STATUS) CD,IDCP,FYR,LYR,
     +                                       (EVAP(J,K,M),M=1,12)
230            Format(A2,A6,2I4,12F8.0)
               If(STATUS.NE.0) Goto 910
               If(K.EQ.1) IDCPX=IDCP
               If(FYR.LE.0) FYR=LYR
               If(IDCPX.EQ.'      '.and.IDCP.NE.'      ') IDCPX=IDCP
               If(IDCP.NE.'      '.and.IDCP.NE.IDCPX) Then
                  Write(14,*) ' '
                  Write(14,240) IDCP,IDCPX
240               Format(' ERROR: In reading EV records, read control'
     +                   ' point ID of ',A6,' when expecting ',A6)
                  Write(14,800)
                  Call ERROR
               Endif
               If(FYR.LT.YRST) Goto 210
               If(LYR.GT.YEAR) Backspace(EUNIT)
               If(FYR.GT.YEAR.or.FYR.GT.LYR) Then
                  Write(14,*) ' '
                  Write(14,260) IDCP,YEAR,FYR
260               Format(' ERROR: In reading EV records for CP '
     +                ,A6,' for year',I5,' read FYR of',I5)
                  Write(14,800)
                  Call ERROR
               Endif
            Else
               Write(14,*) ' '
               Write(14,270) CD
270            Format(' ERROR: Read CD of ',A2,
     +                ' when expecting EV record.')
               Write(14,800)
               Call ERROR
            Endif
            If(ICHECK.EQ.6) Write(14,230) CD,IDCP,FYR,LYR,
     +                                    (EVAP(J,K,M),M=1,12)
         End Do
!
!  The integer control point identifier is assigned.
!
         I=0
280      I=I+1
         If(IDCPX.EQ.CPID(I,1)) Then
            IDCPEV(J)=I
            Goto 300
         Elseif(I.LT.NCPTS) Then
            Goto 280
         Else
            Write(14,*) ' '
            Write(14,290) CD
290         Format(' ERROR: CP ID of ',A2,' on EV record',
     +             ' matches no identifier on the CP records.')
            Write(14,800)
            Call ERROR
         Endif
!
!  Error check to confirm that CPEV from CP record is 0 or 1.
!
300      If(CPEV(I).NE.'      ') Then
            Write(14,*) ' '
            Write(14,310) CPID(I,1),CPEV(I)
310         Format(' ERROR: EV records were read for CP ',A6,
     +             ' but CPEV = ',A6,' on CP record.')
            Write(14,800)
            Call ERROR
         Endif
320   End Do
!
!  Error message.
!
800   FORMAT('        Stopped in Subroutine INEV1 which reads'
     +       ' IN and EV records.')
!
!  EV records must be read for all control points with blank CPEV.
!
      Do 820 I=1,NCPTS
         If(CPEV(I).EQ.'      ') Then
            Do J=1,NEVCP
               If(IDCPEV(J).EQ.I) Goto 820
            End Do
            Write(14,810) CPID(I,1),CPEV(I)
810         Format(' WARNING: For CP ',A6,' CPEV is blank on',
     +             ' CP record but there are no EV records.')
         Endif
820   End Do
!
!  IOSTAT error message
!
900   IOUNIT=IUNIT
      Goto 920
910   IOUNIT=EUNIT
920   If(STATUS.NE.0) Then
         Write(14,*)' '
         Write(14,930) CD,STATUS
930      Format(' ERROR: Fortran IOSTAT error occurred reading an',
     +       ' input',/,8x,'record with CD identifier of ',A2,/
     +       '        IOSTAT status variable =',I6)
         Write(14,800)
         Write(*,*)' '
         Write(*,930) CD,STATUS
         Write(*,800)
         Write(*,*) ' '
         Backspace(IOUNIT)
         Backspace(IOUNIT)
         Write(14,*)' '
         Write(14,940)
940      Format('The first 82 characters of each of the last',
     +          ' two records read are as follows:')
         Read(IOUNIT,950) CD,TITLE1
         Write(14,950) CD,TITLE1
         Read(IOUNIT,950) CD,TITLE1
         Write(14,950) CD,TITLE1
         Write(14,*)' '
950      Format(A2,A78)
         Call ERROR
      Endif
!
!  End of Subroutine INEV1
!
      Write(14,*) '*** Finished reading IN and EV records.'
      Return
!
!  Error message if end of file is reached too soon.
!
960   Write(14,*) ' '
      Write(14,970)
970   Format(' ERROR: Reached end-of-file too soon while',
     +       ' reading IN and EV records.')
      Write(14,980) NINCP
980   Format(8x,'Number of CP records with INMETHOD ',
     +          'requiring IN records:',I4)
      Write(14,990) NEVCP
990   Format(8x,'Number of CP records with CPEV ',
     +          'requiring EV records:',I4)
      Write(14,800)
      Call ERROR
      End Subroutine INEV1
!
! ***************************************************************************
!
      Subroutine INEV2
!
!  Subroutine INEV2 assigns flows INFLOW(cp,month) and net evaporation rates
!  EVAPR(cp,months) each year from the FLOW(cp,year,month) and EVAP(cp,year,
!  month) arrays read by Subroutine INEV1.
!
      Use COMVAR
      Integer I,J,K,L
!
      I=YEAR-YRST+1
!
!  Inflows are assigned for primary control points with IN records.
!
      INFLOW=0.0
      Do J=1,NINCP
         K=IDCPIN(J)
         Do L=1,12
            INFLOW(K,L)=FLOW(J,I,L)*CPDT(K,1)*INX
         End Do
      End Do
!
!  Net evaporation rates are assigned for control points with EV records.
!
      EVAPR=0.0
      Do J=1,NEVCP
         K=IDCPEV(J)
         Do L=1,12
            EVAPR(K,L)=EVAP(J,I,L)*CPDT(K,2)*EVX
         End Do
      End Do
!
!  If no IN records are input for the control point, inflows are assigned
!  values of zero or the values from another control point.
!
      Do J=1,NCPTS
         If(INMETHOD(J).EQ.2) Then
            If(CPIN(J).EQ.'      '.or.CPIN(J).EQ.'  ZERO') Then
               Do L=1,NPRDS
                  INFLOW(J,L)=0.0
               End Do
               If(CPIN(J).EQ.'      '.and.YEAR.EQ.YRST.and.ICHECK.EQ.1)
     +         Then
                  Write(14,100) CPID(J,1)
100               Format(' WARNING: Inflows are not provided for ',
     +             'control point ',A6,'. Zero flows are assumed.')
               Endif
            Else
               K=0
110            K=K+1
               If(CPIN(J).EQ.CPID(K,1)) Then
                  Do L=1,NPRDS
                     INFLOW(J,L)=INFLOW(K,L)*CPDT(J,1)
                  End Do
                  If(INMETHOD(K).GT.1.and.YEAR.EQ.YRST.and.ICHECK.EQ.1)
     +            Then
                     Write(14,120) CPID(J,1),CPID(K,1)
120                  Format(' WARNING: CPIN repeats IN records from a',
     +                   ' CP that has no IN records.',/,10x,'Control',
     +                   ' points CPID and CPIN are ',A6,' and ',A6)
                  Endif
               Elseif(K.EQ.NCPTS) Then
                  Write(14,*) ' '
                  Write(14,130) CPIN(J),CPID(J,1)
130               Format(' ERROR: CPIN of ',A6,' in field 7 of CP ',
     +                   'record for ',A6,' was not found.')
                  Write(14,800)
                  Call ERROR
               Else
                  Goto 110
               Endif
            Endif
         Endif
      End Do
!
!  If no EV records are input for the control point, evaporation rates
!  are assigned values of zero or the values from another control point.
!
      Do 230 J=1,NCPTS
         If(CPEV(J).EQ.'      ') Goto 230
         If(CPEV(J).EQ.'  ZERO') Then
            Do L=1,NPRDS
               EVAPR(J,L)=0.0
            End Do
         Elseif(CPEV(J).EQ.'  NEXT') Then
            NPT=CPNXT(J)
140         If(CPEV(NPT).EQ.'      ') Then
               Do L=1,NPRDS
                  EVAPR(J,L)=EVAPR(K,L)*CPDT(J,2)
               End Do
            Else
               NPT=CPNXT(NPT)
               If(NPT.LT.0) Then
                  Write(14,150) CPID(J,1)
150               Format(' ERROR: CPEV is NEXT for CP ',A6,' but there',
     +                   ' is no downstream CP with EV records.')
                  Call ERROR
               Endif
               Goto 140
            Endif
         Else
            K=0
200         K=K+1
            If(CPEV(J).EQ.CPID(K,1)) Then
               Do L=1,NPRDS
                  EVAPR(J,L)=EVAPR(K,L)*CPDT(J,2)
               End Do
               If(CPEV(K).NE.'      '.and.YEAR.EQ.YRST.and.ICHECK.EQ.1)
     +         Then
                  Write(14,210) CPID(J,1),CPID(K,1)
210               Format(' WARNING: CPEV repeats EV records from a CP',
     +                   ' that has no EV records.',/,10x,'Control',
     +                   ' points CPID and CPEV are ',A6,' and ',A6)
               Endif
            Elseif(K.EQ.NCPTS) Then
               Write(14,*) ' '
               Write(14,220) CPEV(J),CPID(J,1)
220            Format(' ERROR: CPEV of ',A6,' in field 8 of CP ',
     +                'record for ',A6,' was not found.')
               Write(14,800)
               Call ERROR
            Else
               Goto 200
            Endif
         Endif
230   End Do
!
!  Error message.
!
800   FORMAT('        Stopped in Subroutine INEV2 which reads'
     +       ' IN and EV records.')
!
!  End of Subroutine INEV2
!
      Return
      End Subroutine INEV2

      Subroutine SIMOUT
!
!  Subroutine SIMOUT writes simulation results to the SOU file at the
!  completion of the simulation.
!
      Use COMVAR
!
      Integer I,J,K,L,N
      Real X1,X2
!
      Write(14,10)
10    Format(' *** Beginning to write simulation results to SOU file.')
!
!  Control point output is written.
!
      If(NCPO.GT.0) Then
         Do 180 K=1,NCPO
!
!           Headings are written for control point table.
!
            N=DSSCPI(K)
            Write(10,100) Adjustl(CPID(N,1))
100         Format('Control point identifier: ',A6,/)
            If(DSS(4).EQ.0) Then
               Write(10,110)
110            Format(79('-'))
               Write(10,120)
120            Format('YEAR MT',8x,'NAT',9x,'REG',9x,'UNA',9x,'STO',
     +                          9x,'TAR',9x,'SHT')
               Write(10,110)
            Else
               Write(10,130)
130            Format(175('-'))
               Write(10,140)
140            Format('YEAR MT',8x,'NAT',9x,'REG',9x,'UNA',9x,'CLC',9x,
     +                    'CLO',9x,'RFR',9x,'URR',9x,'CPI',9x,'STO',9x,
     +                    'EVA',9x,'DEP',9x,'TAR',9x,'SHT',9x,'DIV')
               Write(10,130)
            Endif
!
!           Data are written for control point table.
!
            Do I=1,NYRS
               YEAR=YRST+I-1
               Do MT=1,12
                  J=((I-1)*12)+MT
                  If(DSS(4).EQ.0) Then
                     Write(10,150) YEAR,MT,(DSSCP(K,J,L),L=1,6)
150                  Format(I4,I3,4F12.2,2F12.3)
                  Else
                     X1=DSSCP(K,J,2)+DSSCP(K,J,10)-DSSCP(K,J,7)
                     X2=DSSCP(K,J,11)-DSSCP(K,J,12)
                     Write(10,160) YEAR,MT,(DSSCP(K,J,L),L=1,7),X1,
     +                                     (DSSCP(K,J,L),L=8,12),X2
160                  Format(I4,I3,11F12.2,3F12.3)
                  Endif
               End Do
            End Do
            If(DSS(4).EQ.0) Write(10,110)
            If(DSS(4).GE.1) Write(10,130)
            Write(10,170)
170         Format(//)
180      End Do
      Endif
!_______________________________________________________________
!
!  Water rights output is written.
!
      If(NWROUT.GT.0) Then
         Do 320 K=1,NWROUT
!
!           Headings are written for water right table.
!
            WR=DSSWRI(K)
            Write(10,200) Adjustl(WRID(WR))
200         Format('Water right identifier: ',A16,/)
            If(DSS(4).EQ.0) Then
               If(WRIDS(WR,1).EQ.'IF#IF*IF') Then
                  Write(10,210)
210               Format(31('-'))
                  Write(10,220)
220               Format('YEAR MT',8x,'IFT',9x,'IFS')
                  Write(10,210)
               Else
                  Write(10,230)
230               Format(67('-'))
                  Write(10,240)
240               Format('YEAR MT',8x,'TAR',9x,'SHT',9x,'DIV',9x,'STO',
     +                             9x,'DEP')
                  Write(10,230)
               Endif
            Endif
            If(DSS(4).EQ.1) Then
250            Format(127('-'))
255            Format(150('-'))
               If(WRIDS(WR,1).EQ.'IF#IF*IF') Then
                  Write(10,255)
                  Write(10,260)
260               Format('YEAR MT',8x,'STO',9x,'EVA',9x,'DEP',9x,'TAR',
     +                             9x,'SHT',9x,'DIV',9x,'ASF',9x,'ROR',
     +                             9x,'IFT',9x,'IFS',9x,'FSV',9x,'FSC')
                  Write(10,255)
               Else
                  Write(10,250)
                  Write(10,270)
270               Format('YEAR MT',8x,'STO',9x,'EVA',9x,'DEP',9x,'TAR',
     +                             9x,'SHT',9x,'DIV',9x,'ASF',9x,'ROR',
     +                             9x,'RFL',9x,'XAV')
                  Write(10,250)
               Endif
            Endif
!
!           Data are written for water right table.
!
            Do I=1,NYRS
               YEAR=YRST+I-1
               Do MT=1,12
                  J=((I-1)*12)+MT
                  If(DSS(4).EQ.0) Then
                     If(WRIDS(WR,1).EQ.'IF#IF*IF') Then
                        Write(10,280) YEAR,MT,DSSWR(K,J,1),DSSWR(K,J,2)
280                     Format(I4,I3,2F12.3)
                     Else
                        X1=DSSWR(K,J,1)-DSSWR(K,J,2)
                        Write(10,290) YEAR,MT,DSSWR(K,J,1),DSSWR(K,J,2),
     +                                     X1,DSSWR(K,J,3),DSSWR(K,J,4)
290                     Format(I4,I3,3F12.3,2F12.2)
                     Endif
                  Endif
                  If(DSS(4).GE.1) Then
                     X1=DSSWR(K,J,4)-DSSWR(K,J,5)
                     If(WRIDS(WR,1).EQ.'IF#IF*IF') Then
                        Write(10,295) YEAR,MT,(DSSWR(K,J,L),L=1,5),X1,
     +                                        (DSSWR(K,J,L),L=6,11)
295                     Format(I4,I3,4F12.2,7F12.3,F11.0)
                     Else
                        Write(10,300) YEAR,MT,(DSSWR(K,J,L),L=1,5),X1,
     +                                        (DSSWR(K,J,L),L=6,9)
300                     Format(I4,I3,4F12.2,6F12.3)
                     Endif
                  Endif
               End Do
            End Do
            If(DSS(4).EQ.0) Write(10,230)
            If(DSS(4).GE.1) Then
               If(WRIDS(WR,1).EQ.'IF#IF*IF') Then
                  Write(10,255)
               Else
                  Write(10,250)
               Endif
            Endif
            Write(10,310)
310         Format(//)
320      End Do
      Endif
!_______________________________________________________________
!
!  Reservoir output is written.
!
      If(NREOUT.GT.0) Then
         Do 480 K=1,NREOUT
!
!           Headings are written for reservoir table.
!
            N=DSSREI(K)
            Write(10,400) Adjustl(RESID(N))
400         Format('Reservoir identifier: ',A6,/)
            If(DSS(4).EQ.0) Then
               Write(10,410)
410            Format(55('-'))
               Write(10,420)
420            Format('YEAR MT',8x,'STO',9x,'EVA',9x,'EPD',9x,'WSE')
               Write(10,410)
            Else
               Write(10,430)
430            Format(151('-'))
               Write(10,440)
440            Format('YEAR MT',8x,'STO',9x,'EVA',9x,'HPS',9x,'HPE',9x,
     +                             'RID',9x,'RIR',9x,'RAH',9x,'RNA',9x,
     +                             'EPD',9x,'EVR',9x,'WSE',9x,'RSC')
               Write(10,430)
            Endif
!
!           Data are written for reservoir table.
!
            Do I=1,NYRS
               YEAR=YRST+I-1
               Do MT=1,12
                  J=((I-1)*12)+MT
                  If(DSS(4).EQ.0) Then
                     Write(10,450) YEAR,MT,(DSSRE(K,J,L),L=1,4)
450                  Format(I4,I3,2F12.2,F12.3,F12.2)
                  Else
                     Write(10,460) YEAR,MT,(DSSRE(K,J,L),L=1,12)
460                  Format(I4,I3,8F12.2,2F12.3,F12.2,F12.1)
                  Endif
               End Do
            End Do
            If(DSS(4).EQ.0) Write(10,410)
            If(DSS(4).GE.1) Write(10,430)
            Write(10,470)
470         Format(//)
480      End Do
      Endif
!_______________________________________________________________
!
      Write(14,490)
490   Format(' *** Finished writing simulation results to SOU file.')
      Return
      End Subroutine SIMOUT
!
! ***************************************************************************
!
      Subroutine INCREM
!
!  Subroutine INCREM computes flow adjustments to deal with negative
!  incrementals. Naturalized flows are adjusted as specified by ADJINC from
!  the JD record. Negative incremental flows may be written to the MSS file
!  from the main program if specified by NEGINC on the JD record.
!
      Use COMVAR
!
      Real TEMP(NCPTS,2),ERR,X
      Integer I,J,K,L,NCP,Z
!
!  The identifiers NICP(I,1&2) of the first and last control points
!  located immediately upstream of control point I are determined
!  the first time subroutine INCREM is called.
!
      If(NIFLAG2.EQ.0) Then
         NIFLAG2=99
         NICP=0
         Do 20 I=1,NCPTS
            L=0
            Do 10 J=1,NCPTS
               If(CPNXT(J).EQ.I) Then
                  If(L.EQ.0) Then
                     L=99
                     NICP(I,1)=J
                  Endif
                  NICP(I,2)=J
               Endif
10          End Do
20       End Do
      Endif
!
!  Computations are repeated for each time step.
!
      Do 90 MT=1,NPRDS
!
!  Determine negative incremental flows looking upstream.
!
         If(ADJINC.GE.3.or.NEGINC.GE.3) Then
            Do I=1,NCPTS
               TEMP(I,2)=CPFLOW(I,MT,2)
            End Do
            Do 70 Z=1,NCPTS+1
               K=0
               Do 60 I=1,NCPTS
                  If(NIFLAG.EQ.9.and.INMETHOD(I).GE.3) Goto 60
                  CPFLOW(I,MT,1)=0.0
!
!  If JD record ADJINC is not -3 or -4, all control points are included
!  in negative incremental flow adjustments.
!
                  If(NIFLAG.NE.9.and.NICP(I,1).GT.0) Then
                     Do J=NICP(I,1),NICP(I,2)
                        If(CPNXT(J).EQ.I)
     +                         CPFLOW(I,MT,1)=CPFLOW(I,MT,1)+TEMP(J,2)
                     End Do
!
!  If ADJINC is -3 or -4, NIFLAG=9 and control points with synthesized
!  flows are not included in negative incremental flow adjustments.
!  With CPFLAG>0, control points with INMETHOD(cp)=9 are also excluded.
!
                  Elseif(NIFLAG.EQ.9.or.CPFLAG.GT.0) Then
                     J=0
30                   J=J+1
                     If(J.EQ.I) Goto 30
                     If(NIFLAG.EQ.9.and.INMETHOD(J).GE.3) Goto 30
                     If(CPFLAG.GT.0.and.INMETHOD(J).EQ.9) Goto 30
                     If(J.GT.NCPTS) Goto 50
                     NCP=CPNXT(J)
40                   If(NCP.LE.0) Goto 30
                     If(NCP.EQ.I) Then
                        CPFLOW(I,MT,1)=CPFLOW(I,MT,1)+TEMP(J,2)
                        Goto 30
                     Endif
                     If(INMETHOD(NCP).LE.2) Goto 30
                     NCP=CPNXT(NCP)
                     Goto 40
                  Endif 
!
!  End of iterative loop accumulating flows upstream of control point I.
!  CPFLOW(I,MT,1), TEMP(I,1), and TEMP(I,2) are all representations of
!  the total flow at control points immediately upstream of CP I.
!
50                TEMP(I,1)=Max(CPFLOW(I,MT,1),TEMP(I,2))
                  X=0.0001
                  If(TEMP(I,2).LT.10.0) Then
                     ERR=Abs(TEMP(I,1)-TEMP(I,2))
                     X=0.001
                  Else
                     ERR=Abs(TEMP(I,1)-TEMP(I,2))/TEMP(I,2)
                  Endif
                  If(Z.GT.20) X=0.001
                  TEMP(I,2)=TEMP(I,1)
                  If(ERR.GT.X) K=K+1
60             End Do
               If(K.EQ.0) Goto 80
70          End Do
!
!  Finish determination of negative incremental flows looking upstream.
!
80          Do I=1,NCPTS
               CPFLOW(I,MT,1)=TEMP(I,2)-CPFLOW(I,MT,2)
               If(ADJINC.EQ.3) INFLOW(I,MT)=
     +                               INFLOW(I,MT)+CPFLOW(I,MT,1)
               If(ADJINC.GE.3) CPFLOW(I,MT,2)=
     +                               CPFLOW(I,MT,2)+CPFLOW(I,MT,1)
            End Do
!
!  Flow adjustments are based on negative incrementals looking downstream
!  for ADJINC or NEGINC of 2 on JD record.  Subroutine AVALB is called.
!
         Elseif(ADJINC.EQ.2.or.NEGINC.EQ.2) Then
            Do I=1,NCPTS
               LOCNUM=I
               AVFLAG=9
               Call AVALB
               CPFLOW(I,MT,1)=Min(0.,AVAMT-CPFLOW(I,MT,2))
            End Do
            If(ADJINC.EQ.2) Then
               Do I=1,NCPTS
                  INFLOW(I,MT)=INFLOW(I,MT)+CPFLOW(I,MT,1)
               End Do
            Endif
         Endif
90    End Do
      Return
      End Subroutine INCREM
!
! ***************************************************************************
!
      Subroutine AVALB
!
!  Subroutine AVALB determines the amount of water available to a right.
!
      Use COMVAR
!
      Integer J,K,NUSCP,XCPFLAG
      Real IFLIMIT,LIMIT,CLAD,RFLOW,FLOWCL,FLOWDS,FLAMT,X
!
!  Subroutine AVALB_2 is called for negative incremental option 5.
!
      If(ADJINC.EQ.5) Then
         Call AVALB_2
         Return
      Endif
!
!  PX record control point limit options activate dual passes through
!  Subroutine AVALB designated by XCPFLAG = 1 and 2.
!
      XCPAV1=0.0
      XCPFLAG=0
      J=0
5     If(WR.GT.0) Then
         If(WRNUM(WR,8).GT.0) Then
            J=WRNUM(WR,8)
            If(XCP(J).GT.0) XCPFLAG=XCPFLAG+1
         Endif
      Endif
!
!  Determine water availability based on unappropriated flow at the CP
!  of this right and at downstream control points.
!
      AVAMT=CPFLOW(LOCNUM,MT,2)
      If(INMETHOD(LOCNUM).EQ.9) AVAMT=900000000.0
      NUSCP=LOCNUM
      CLAD=1.0
      NPT=CPNXT(LOCNUM)
10    If(NPT.LT.0) Goto 20
!
!     PX record control point limit options.
!
      If(XCPFLAG.EQ.2.and.J.GE.1) Then
         If(NPT.EQ.XCPI(J)) Then
            XCPCLAD=CLAD*(1.0-CL(NUSCP))
            If(XCP(J).EQ.1.or.XCP(J).EQ.2) Then
               Goto 20
            Elseif(XCP(J).EQ.3.or.XCP(J).EQ.4) Then
               NPT=CPNXT(NPT)
            Endif
         Endif
      Endif
!
!     Flow availability limited by flow at downstream control points.
!
      If(CL(NUSCP).GT.0.0) CLAD=CLAD*(1.0-CL(NUSCP))
      If(Abs(CLAD).LE.0.00001) Goto 20
      If(INMETHOD(NPT).NE.9) Then
         FLOWCL=(CPFLOW(NPT,MT,2)-CPFLOW(NPT,MT,1))/CLAD
         FLOWDS=FLOWCL
         If(CPFLOW(NPT,MT,2).GT.FLOWCL) FLOWDS=CPFLOW(NPT,MT,2)
         If(FLOWDS.LT.AVAMT) AVAMT=FLOWDS
      Endif
!
!     The flow check is repeated for the next control point.
!
      NUSCP=NPT
      NPT=CPNXT(NPT)
      Goto 10
20    If(INMETHOD(LOCNUM).EQ.9.and.AVAMT.GE.890000000.0) AVAMT=0.0
!
!  Restrict water available to right (AVAMT) to proportion of control point
!  inflow that is accessible at watershed location of right (FLAMT).
!
      If(AVFLAG.NE.9.and.WR.GT.0) Then
         If(WSHED(WR).GT.0.001) Then
            FLAMT = WSHED(WR) * INFLOW(LOCNUM,MT)
            If(FLAMT.LT.AVAMT) AVAMT=FLAMT
         Endif
      Endif
!
!  The instream flow requirement computations (statements 30 through 50) are
!  skipped if AVALB is called by Subroutine INCREM (AVFLAG=9); for hydropower
!  rights (WRT<0); or if there are no instream flow targets yet (IFPASS<0).
!  Statements 30-50 are executed if final unappropriated flows are being
!  determined at completion of water rights loop (AVFLAG=99).
!
      If(AVFLAG.EQ.9) Then
         AVFLAG=0
         Goto 60
      Elseif(AVFLAG.EQ.99.and.IFPASS.GE.0) Then
         AVFLAG=0
         Goto 30
      Endif
      If(IFPASS.LT.0.or.WRT.LT.0) Goto 60
      If(NOTF(WR).EQ.-9) Goto 60
!
!  Constrain water availability based on regulated flows to reflect
!  instream flow requirements
!
30    If(IFRESREL(LOCNUM).EQ.-99) Then
         RFLOW=CPFLOW(LOCNUM,MT,2)
      Else
         RFLOW=CPFLOW(LOCNUM,MT,2)+RESREL(LOCNUM)
      Endif
      If(ADJINC.EQ.4) RFLOW=RFLOW-CPFLOW(LOCNUM,MT,1)
      IFLIMIT=RFLOW-IFTARGET(LOCNUM)
      If(REGFLOW(LOCNUM).GT.RFLOW.and.IFRESREL(LOCNUM).NE.-99)
     +   IFLIMIT=REGFLOW(LOCNUM)-IFTARGET(LOCNUM)
      If(IFLIMIT.LT.0.0) IFLIMIT=0.0
      If(INMETHOD(LOCNUM).EQ.9) IFLIMIT=900000000.0
      NUSCP=LOCNUM
      NPT=CPNXT(LOCNUM)
      CLAD=1.0
40    If(NPT.LT.0) Goto 50
      If(CL(NUSCP).GT.0.0) CLAD=CLAD*(1.0-CL(NUSCP))
      If(Abs(CLAD).LE.0.00001) Goto 50
      If(INMETHOD(NPT).NE.9) Then
         If(IFRESREL(NPT).EQ.-99) Then
            RFLOW=CPFLOW(NPT,MT,2)
         Else
            RFLOW=CPFLOW(NPT,MT,2)+RESREL(NPT)
         Endif
         If(ADJINC.EQ.4) RFLOW=RFLOW-CPFLOW(NPT,MT,1)
         If(IFTARGET(NPT).GT.0.0) Then
            LIMIT=RFLOW-IFTARGET(NPT)
            If(REGFLOW(NPT).GT.RFLOW.and.IFRESREL(NPT).NE.-99)
     +         LIMIT=REGFLOW(NPT)-IFTARGET(NPT)
            If(LIMIT.LT.0.0) LIMIT=0.0
            LIMIT=LIMIT/CLAD
            If(LIMIT.LT.IFLIMIT) IFLIMIT=LIMIT
         Endif
      Endif
      NUSCP=NPT
      NPT=CPNXT(NPT)
!
!     PX record control point limit options.
!
      If(XCPFLAG.EQ.2.and.J.GE.1) Then
         If(NPT.EQ.XCPI(J)) Then
            If(XCP(J).EQ.1.or.XCP(J).EQ.2) Then
               Goto 50
            Elseif(XCP(J).EQ.3.or.XCP(J).EQ.4) Then
               NPT=CPNXT(NPT)
            Endif
         Endif
      Endif
!
!     The flow check is repeated for the next control point
!     until all pertinent control points are checked.
!
      Goto 40
50    If(AVAMT.GT.IFLIMIT) AVAMT=IFLIMIT
!
!  Option 4 for accounting for negative incremental flows.  If ADJINC=4
!  on JD record, the available flow is set equal to the lesser of AVAMT
!  and the unappropriated flow minus negative incremental.
!
60    If(ADJINC.EQ.4.and.INMETHOD(LOCNUM).NE.9) Then
         AVAMT=Min(AVAMT,CPFLOW(LOCNUM,MT,2)-CPFLOW(LOCNUM,MT,1))
         AVAMT=Max(0.,AVAMT)
      Endif
!
!  End of subroutine if this is a ZZ record call before the water rights loop.
!
      If(WR.EQ.0) Return
!
!  PX record control point limit options.
!
      If(XCPFLAG.EQ.1.and.J.GE.1) Then
         XCPAV1=AVAMT
         Goto 5
      Endif
      If(XCPFLAG.EQ.2.and.J.GE.1) Then
         XAV=AVAMT-XCPAV1
         If(XCP(J).EQ.2.or.XCP(J).EQ.4) Then
            X=DEPSUM(XCPI(J))/XCPCLAD
            If(XAV.GT.X) Then
               XAV=X
               AVAMT=XCPAV1+XAV
            Endif
         Endif
      Endif
!
!  PX/AX record streamflow availability multiplier factor option.
!
      If(WRNUM(WR,8).GT.0) Then
         J=WRNUM(WR,8)
         If(XAXFLAG(J).GT.0) Then
            If(AVAMT.GT.XAMIN(J)) Then
               AVAMT=(AVAMT-XAMIN(J))*XA(J,MT) + XAMIN(J)
            Endif
            If(AVAMT.GT.XAMAX(J)) AVAMT=XAMAX(J)
         Endif
      Endif
!
!  EA/AF record streamflow availability multiplier factor option.
!
      If(WRNUM(WR,9).GT.0) Then
         If(EAR(WRNUM(WR,9)).GT.0) Then
            J=EAR(WRNUM(WR,9))
            If(XAFFLAG(J).GT.0) Then
!
!           EA record reservoir identifier index K is determined.
!
               K=0
70             K=K+1
               If(K.GT.EARNUM(J)) Then
                  Write(14,80) 
80                Format(' ERROR: Subroutine AVALB could not match',
     +                   ' reservoirs for EA/AF record option.')
                  Call ERROR
               Endif
               If(EARES(J,K).NE.RESID(WRNUM(WR,9))) Goto 70
!
!              Streamflow availability adjustment.
!
               If(AVAMT.GT.AFMIN(J)) Then
                  AVAMT=(AVAMT-AFMIN(J))*AFX(J,K) + AFMIN(J)
               Endif
               If(AVAMT.GT.AFMAX(J)) AVAMT=AFMAX(J)
            Endif
         Endif
      Endif
!
      Return
      End Subroutine AVALB
!
! ***************************************************************************
!
      Subroutine AVALB_2
!
!  Subroutine AVALB_2 replaces AVALB for negative incremental flow Option 5.
!
      Use COMVAR
!
      Integer J,K,NUSCP,XCPFLAG
      Real IFLIMIT,CLAD,DELTA,RFLOW,X
!
!  PX record control point limit options activate dual passes through
!  Subroutine AVALB_2 designated by XCPFLAG = 1 and 2.
!
      XCPAV1=0.0
      XCPFLAG=0
      J=0
5     If(WR.GT.0) Then
         If(WRNUM(WR,8).GT.0) Then
            J=WRNUM(WR,8)
            If(XCP(J).GT.0) XCPFLAG=XCPFLAG+1
         Endif
      Endif
!
!  Available streamflow AVAMT is initially set considering only
!  the flow at the control point location of the water right.
!
      AVAMT=CPFLOW(LOCNUM,MT,2)
      If(INMETHOD(LOCNUM).EQ.9) AVAMT=900000000.0
!
!  Decide whether or not to perform adjustment due instream flow requirement.
!
      If(AVFLAG.EQ.9) Then
         Goto 20
      Elseif(AVFLAG.EQ.99.and.IFPASS.GE.0) Then
         Goto 10
      Endif
      If(IFPASS.LT.0.or.WRT.LT.0.or.WR.EQ.0) Goto 20
      If(NOTF(WR).EQ.-9) Goto 20
!
!  Adjustment due to instream flow requirement
!
10    If(IFTARGET(LOCNUM).GT.0.0) Then
         If(IFRESREL(LOCNUM).EQ.-99) Then
            RFLOW=CPFLOW(LOCNUM,MT,2)
         Else
            RFLOW=CPFLOW(LOCNUM,MT,2)+RESREL(LOCNUM)
         Endif
         IFLIMIT=RFLOW-IFTARGET(LOCNUM)
         If(REGFLOW(LOCNUM).GT.RFLOW.and.IFRESREL(LOCNUM).NE.-99)
     +      IFLIMIT=REGFLOW(LOCNUM)-IFTARGET(LOCNUM)
         IFLIMIT=Max(IFLIMIT,0.0)
         If(INMETHOD(LOCNUM).EQ.9) IFLIMIT=900000000.0
         AVAMT=Min(AVAMT,IFLIMIT)
      Endif
!
20    DELTA=AVAMT
      NUSCP=LOCNUM
      CLAD=1.0
!
30    NPT=CPNXT(NUSCP)
      If(NPT.LT.0) Goto 60
!
!  CP record INMETHOD option 9.
!
      If(INMETHOD(NPT).EQ.9) Then
         NUSCP=NPT
         NPT=CPNXT(NPT)
         Goto 30
      Endif
!
!  PX record control point limit options.
!
         If(XCPFLAG.EQ.2.and.J.GE.1) Then
            If(NPT.EQ.XCPI(J)) Then
               XCPCLAD=CLAD*(1.0-CL(NUSCP))
               If(XCP(J).EQ.1.or.XCP(J).EQ.2) Then
                  Goto 60
               Elseif(XCP(J).EQ.3.or.XCP(J).EQ.4) Then
                  NPT=CPNXT(NPT)
               Endif
            Endif
         Endif
!
!  If unappropriated flow exists at the control point, check if the current
!  maximum change in flow exceeds the unappropriated flow.  If it does, limit
!  the change translated downstream to the unapropriated flow.  If senior water
!  rights have made depletions at this control point,  limit the estimate of
!  availability to the unappropriated flow at this control point, adjusted upstream
!  for channel losses.  If no senior water rights have made depletions at this
!  control point, then all water upstream may be taken, but the net effect
!  downstream is the change in unappropriated flow at this control point.
!
!  If the current maximum change in flow is less than the unappropriated flow,
!  update the change in flow to be translated downstream, and leave the
!  the current estimate of availability the same.
!
!  If the unappropriated flow at this control point is zero, then no water
!  is available unless no senior depletions have been made at this control
!  point.  If no senior depletions have been made at this control point and
!  the unappropriated flow is zero, then the current estimate of availability
!  will have no effect downstream of this control point and the routine
!  can be exited.
!
      CLAD=CLAD*(1.0-CL(NUSCP))
      If(CPFLOW(NPT,MT,2).GT.0.0001) Then
!
!  Check maximum change in flow allowed, based on "unappropriated" flows
!
         If(DELTA*(1-CL(NUSCP)).GT.CPFLOW(NPT,MT,2)) Then
            DELTA=CPFLOW(NPT,MT,2)
            If(DEPSUM(NPT).GT.0.0) Then
!
!  Only limit availability if delivery greater than 0.
!
               If(CLAD.GT.0.0) AVAMT=DELTA/CLAD
            Endif
         Else
            DELTA=DELTA*(1.0-CL(NUSCP))
         Endif
!
!  Check maximum change in flow allowed by instream flow requirements.
!  Decide whether or not to perform adjustment for instream flow requirement
!
         If(AVFLAG.EQ.9.or.WR.EQ.0) Then
            Goto 50
         Elseif(AVFLAG.EQ.99.and.IFPASS.GE.0) Then
            Goto 40
         Endif
         If(IFPASS.LT.0.or.WRT.LT.0) Goto 50
         If(NOTF(WR).EQ.-9) Goto 50
40       If(IFTARGET(NPT).GT.0.0.and.DELTA.GT.0.0) Then
            If(IFRESREL(NPT).EQ.-99) Then
               RFLOW=CPFLOW(NPT,MT,2)
            Else
               RFLOW=CPFLOW(NPT,MT,2)+RESREL(NPT)
            Endif
            IFLIMIT=RFLOW-IFTARGET(NPT)
            If(REGFLOW(NPT).GT.RFLOW.and.IFRESREL(NPT).NE.-99)
     +         IFLIMIT=REGFLOW(NPT)-IFTARGET(NPT)
            IFLIMIT=Max(IFLIMIT,0.0)
            If(IFLIMIT.LT.DELTA.and.CLAD.GT.0.0) Then
               DELTA=IFLIMIT
               AVAMT=DELTA/CLAD
            Endif
         Endif
!
!     The flow check is repeated for the next control point.
!
50       NUSCP=NPT
         NPT=CPNXT(NPT)
         Goto 30
      Else
         If(DEPSUM(NPT).GT.0.) AVAMT=0.0
      Endif
!
60    If(AVFLAG.EQ.9.or.AVFLAG.EQ.99) AVFLAG=0
!
!  CP record INMETHOD option 9.
!
      If(INMETHOD(LOCNUM).EQ.9.and.AVAMT.GE.890000000.0) AVAMT=0.0
!
!  PX record control point limit options.
!
      If(XCPFLAG.EQ.1.and.J.GE.1) Then
         XCPAV1=AVAMT
         Goto 5
      Endif
      If(XCPFLAG.EQ.2.and.J.GE.1) Then
         XAV=AVAMT-XCPAV1
         If(XCP(J).EQ.2.or.XCP(J).EQ.4) Then
            X=DEPSUM(XCPI(J))/XCPCLAD
            If(XAV.GT.X) Then
               XAV=X
               AVAMT=XCPAV1+XAV
            Endif
         Endif
      Endif
!
!  End of subroutine if this is a ZZ record call before the water rights loop.
!
      If(WR.EQ.0) Return
!
!  PX/AX record streamflow availability multiplier factor option.
!
      If(WRNUM(WR,8).GT.0) Then
         J=WRNUM(WR,8)
         If(XAXFLAG(J).GT.0) Then
            If(AVAMT.GT.XAMIN(J)) Then
               AVAMT=(AVAMT-XAMIN(J))*XA(J,MT) + XAMIN(J)
            Endif
            If(AVAMT.GT.XAMAX(J)) AVAMT=XAMAX(J)
         Endif
      Endif
!
!  EA/AF record streamflow availability multiplier factor option.
!
      If(WRNUM(WR,9).GT.0) Then
         If(EAR(WRNUM(WR,9)).GT.0) Then
            J=EAR(WRNUM(WR,9))
            If(XAFFLAG(J).GT.0) Then
!
!           EA record reservoir identifier index K is determined.
!
               K=0
70             K=K+1
               If(K.GT.EARNUM(J)) Then
                  Write(14,80)
80                Format(' ERROR: Subroutine AVALB could not match',
     +                   ' reservoirs for EA/AF record option.')
                  Call ERROR
               Endif
               If(EARES(J,K).NE.RESID(WRNUM(WR,9))) Goto 70
!
!              Streamflow availability adjustment.
!
               If(AVAMT.GT.AFMIN(J)) Then
                  AVAMT=(AVAMT-AFMIN(J))*AFX(J,K) + AFMIN(J)
               Endif
               If(AVAMT.GT.AFMAX(J)) AVAMT=AFMAX(J)
            Endif
         Endif
      Endif
!
      Return
      End Subroutine AVALB_2
!
! ***************************************************************************
!
      Subroutine RELEASE
!
!  Subroutine RELEASE calculates releases from reservoirs for system rights.
!  For type 1 and hydropower rights, subroutine RELEASE is applicable only
!  to the second and subsequent reservoirs in a multiple-reservoir system.
!  Subroutine RELEASE is applicable to type 2 and 3 rights with either one
!  or multiple reservoirs.
!
      Use COMVAR
!
      Integer CHCKST,CONFL,FIRST,HILO,I,J,NUSCP,PRIREL(MAXSYS),
     +        PT,RESINT,SYSRES,TEMPI,ZONE
      Real CLX,CLX1,CLX2,CLX3,ESX,EVAPX,PASSNC,PRTY(MAXSYS),TEMPR,
     +     TMPREL,X
!
      ESX=0.0
      EVAPX=0.0
      PRTY=0.0
      RELS=0.0
      TEMPR=0.0
      TMPREL=0.0
      HILO=2
      ZONE=0
!
!  Subroutine RELEASE is called only for system water rights.
!
      If(SWR.LE.0) Goto 170
!
!  Release decisions may be based on either beginning or ending storage.
!
      CHCKST=6
      If(STOFLG.EQ.1) CHCKST=5
!
!  A type 1 right (refill storage) only makes releases from
!  its second and subsequent reservoirs (secondary reservoirs).
!
      FIRST=1
      If(WRT.EQ.1.or.WRT.LT.0) FIRST=2
!
      If(NSR(SWR).GE.FIRST) Then
!
!  Iterative loop extends from statement 10 to near the end of subroutine.
!  Multiple reservoir release decisions are based on assigning priorities
!  based on storage. If HILO equals 2, top zone is checked. For HILO of 1,
!  lower zone is checked. ZONE=0 means no reservoirs are able to release
!  from the zone being considered.
!
10       Do 20 I=FIRST,NSR(SWR)
            RESINT=SN1(SWR,I)
            If(RESDAT(RESINT,CHCKST).GT.WRSYS(I,HILO).and.
     +         WRSYS(I,HILO+1).GT.WRSYS(I,HILO)) Then
               ZONE=ZONE+1
               PRIREL(ZONE)=I
               PRTY(ZONE)=(WRSYS(I,HILO+3)*(RESDAT(RESINT,CHCKST)-
     +                  WRSYS(I,HILO))/(WRSYS(I,HILO+1)-WRSYS(I,HILO)))
     +                + WRSYS(I,HILO+5)
            Endif
20       End Do
!
!  Reservoirs are sorted by priority from high to low.
!
         If(ZONE.GT.0) Then
            If(ZONE.GE.2) Then
               Do 30 I=1,ZONE-1
                  Do J=I+1,ZONE
                     If(PRTY(J).GT.PRTY(I).or.(PRTY(J).EQ.PRTY(I).and.
     +                  PRIREL(J).LT.PRIREL(I))) Then
                        TEMPI=PRIREL(J)
                        PRIREL(J)=PRIREL(I)
                        PRIREL(I)=TEMPI
                        TEMPR=PRTY(J)
                        PRTY(J)=PRTY(I)
                        PRTY(I)=TEMPR
                     Endif
                  End Do
30             End Do
            Endif
!
!  The maximum that may be released from each reservoir is found in turn.
!  The amount that may be released by any reservoir that is not upstream of
!  the water right location is constrained by the sum of senior downstream
!  depletions, the amount that has already been released to meet them, and
!  the amount that the current water right allowed to pass to them.
!
            Do 160 I=1,ZONE
               If(PRIREL(I).LE.0) Then
                  Write(14,40) PRIREL(I),Adjustl(WRID(WR))
40                Format(' WARNING: Subroutine RELEASE, SYSRES=PRIREL(',
     +            'I) of',I3,' is not valid.',/,10x,'Water right: ',A16)
               Endif
               SYSRES=PRIREL(I)
               RESINT=SN1(SWR,SYSRES)
               CONFL=SN2(SWR,SYSRES)
!
!  If reservoir is upstream of water right location (CONFL=0), TMPREL is the
!  amount that the reservoir has to release to overcome channel losses.  If
!  reservoir releases are not constrained (CONFL=negative), TMPREL is simply
!  the amount the reservoir has to release; channel losses are not considered.
!
               If(CONFL.LE.0) Then
                  CLX=1.0
                  PT=RESNUM(RESINT,1)
50                If(PT.NE.LOCNUM.and.CONFL.GE.0.and.PT.GE.0) Then
                     NUSCP=PT
                     CLX=CLX*(1.0-CL(NUSCP))
                     PT=CPNXT(PT)
                     Goto 50
                  Endif
                  If(CLX.GT.0.0) Then
                     TMPREL=(MAKEUP-RELS)/CLX
                  Else
                     TMPREL=0.0
                     Write(14,120) WRID(WR),RESID(RESINT)
                  Endif
!
!  Release is constrained by the release capacity specified on the OR record.
!
                  If(WRSYS(SYSRES,8).GT.0.0001) Then
                     If(TMPREL.GT.WRSYS(SYSRES,8))TMPREL=WRSYS(SYSRES,8)
                     If(FSOR(SYSRES).GT.0) Then
                        If(FSI(FSOR(SYSRES),12).EQ.99) Then
                           TMPREL=TMPREL*FSX(FSOR(SYSRES),1)
                        Elseif(FSI(FSOR(SYSRES),12).EQ.0) Then
                           TMPREL=TMPREL*FSX(FSOR(SYSRES),2)
                        Endif
                     Endif
                  Endif
!
!  Make release from reservoir
!  *+*+*+*+*  Call Subroutine RESCAL  *+*+*+*+*
!
                  If(TMPREL.GT.0.00001) Then
                     INRES=0.0
                     OUTRES=TMPREL
                     Call RESCAL(RESINT,WRSYS(SYSRES,HILO),
     +                    RESDAT(RESINT,1),INRES,OUTRES,ESX,EVAPX)
                     TMPREL=OUTRES-RESDAT(RESINT,9)-RESDAT(RESINT,10)
                     SYSREL(SYSRES)=SYSREL(SYSRES)+TMPREL
                     RELS=RELS+TMPREL*CLX
                     If(SN3(SWR,SYSRES).LT.0) Then
                        RESDAT(RESINT,10)=OUTRES-RESDAT(RESINT,9)
                     Else
                        RESDAT(RESINT,9)=OUTRES-RESDAT(RESINT,10)
                     Endif
                     RESDAT(RESINT,6)=ESX
                     RESDAT(RESINT,7)=EVAPX
                  Endif
!
!  The reservoir will release to a point that is downstream of both
!  the water right and the reservoir.
!
!  TMPREL = amount of downstream senior depletions that the
!           reservoir is able to release to
!
!  PASSNC = total flow that the control point at the water right
!           location passed to downstream senior depletions to
!           which the reservoir is able to release
!
!  The amount that the right passed to downstream senior rights is found.
!  This is the minimum amount passed by each control point between the water
!  right location and the confluence.  These values are brought back to the
!  water right location using channel loss factors.  This amount is reduced
!  by the cumulative delivery factor between the water right location and
!  the confluence. This is the maximum the reservoir has to release to the
!  confluence location to meet PASSNC at the water right location.
!  CLX1 = cumulative delivery factor between water right and confluence
!
               Elseif(CONFL.GT.0) Then
                  TMPREL=0.0
                  PASSNC=MAKEUP-RELS
                  CLX1=1.0
                  PT=LOCNUM
70                If(PT.NE.CONFL) Then
                     AVAMT=TEMPR
                     Call AVALB
                     TEMPR=AVAMT
                     If(CLX1.GT.0.0) Then
                        PASSNC=Min(PASSNC,(CPFLOW(PT,MT,2)-TEMPR)/CLX1)
                        PASSNC=Max(0.0,PASSNC)
                        CLX1=CLX1*(1.0-CL(PT))
                        PT=CPNXT(PT)
                     Endif
                     Goto 70
                  Endif
                  PASSNC=PASSNC*CLX1
                  If(CLX1.EQ.0.0) Then
                     Write(14,120) WRID(WR),RESID(RESINT)
                  Endif
                  If(PASSNC.EQ.0.0) Then
                     TMPREL=0.0
                     Goto 130
                  Endif
!
!  The summation of senior depletions is found at the confluence point and 
!  downstream that have not been 'met' by previous reservoir releases.  The 
!  value from each control point downstream from the confluece point will be
!  brought back to the confluence point using the channel loss factors.
!  CLX2 = cumulative delivery factor between confluence and basin outlet
!
                  CLX2=1.0
90                If(PT.GE.0) Then
                     TMPREL=TMPREL+DMRSUM(PT)/CLX2
                     PT=CPNXT(PT)
                     If(PT.GT.0) Then
                        If(CL(PT).LT.1.0) Then
                           CLX2=CLX2*(1.0-CL(PT))
                           Goto 90
                        Endif
                     Endif
                  Endif
                  TMPREL=Min(TMPREL,PASSNC)
!
!  Determine the amount to be released from the reservoir to overcome
!  channel losses between the reservoir and the confluence location.
!  CLX3 = cumulative delivery factor between reservoir and confluence
!
                  If(TMPREL.GT.0.0001) Then
                     CLX3=1.0
                     NUSCP=RESNUM(RESINT,1)
110                  If(NUSCP.NE.CONFL) Then
                        CLX3=CLX3*(1.0-CL(NUSCP))
                        NUSCP=CPNXT(NUSCP)
                        Goto 110
                     Endif
                     If(CLX3.GT.0.0) Then
                        TMPREL=TMPREL/CLX3
                     Else
                        TMPREL=0.0
                        Write(14,120) WRID(WR),RESID(RESINT)
120                     Format(' WARNING: Unable to deliver releases',
     +                    ' to water right ',A16,/,'from reservoir ',A6,
     +                    ' due to input channel loss factor of 1.0')
                     Endif
                  Endif
!
!  Subroutine RESCAL is called to determine reservoir release.
!
130               If(TMPREL.GT.0.0001) Then
                     INRES=0.
                     OUTRES=TMPREL
                     Call RESCAL(RESINT,WRSYS(SYSRES,HILO),
     +                    RESDAT(RESINT,1),INRES,OUTRES,ESX,EVAPX)
                     TMPREL=OUTRES-RESDAT(RESINT,9)-RESDAT(RESINT,10)
                     SYSREL(SYSRES)=SYSREL(SYSRES)+TMPREL
!
!  Determine how much of the release actually can be used to meet the target.
!
                     RELS=RELS+TMPREL*(CLX3/CLX1)
                     If(SN3(SWR,SYSRES).LT.0) Then
                        RESDAT(RESINT,10)=OUTRES-RESDAT(RESINT,9)
                     Else
                        RESDAT(RESINT,9)=OUTRES-RESDAT(RESINT,10)
                     Endif
                     RESDAT(RESINT,6)=ESX
                     RESDAT(RESINT,7)=EVAPX
!
!  TMPREL is now the released flow that can be utilized by the water right.
!
                     TMPREL=TMPREL*CLX3/CLX1
!
!  Adjust flows from water right diversion location to, but not including,
!  confluence because the flow released from the reservoir has now been
!  diverted at the water right control point and should not be included
!  in regulated flow from the water right location to the confluence.
!
                     PT=LOCNUM
                     CLX=1.0
140                  If(PT.NE.CONFL.and.CLX.NE.0.0) Then
                        CPFLOW(PT,MT,2)=CPFLOW(PT,MT,2)-TMPREL*CLX
                        CLX=CLX*(1.0-CL(PT))
                        PT=CPNXT(PT)
                        Goto 140
!
!  Adjust running total of downstream senior water rights depletions that
!  can be released to at confluence location and downstream. First adjust
!  TMPREL back to the released flow that reached the confluence.
!
                     Endif
                     TMPREL=TMPREL*CLX1
                     Call CUMREL(CONFL,TMPREL)
                  Endif
               Endif
!
!  The computations are complete if the release RELS and
!  the target MAKUP are equal.  Return to main program.
!
               X=Abs(RELS-MAKEUP)/MAKEUP
               If(X.LT.0.00001) Goto 170
160         End Do
         Endif
!
!  If the computed release RELS is less than the target MAKEUP and
!  only the lower zone has been considered, the computations are
!  repeated for the upper zone of the reservoirs.
!
         X=(MAKEUP-RELS)/MAKEUP
         If(X.GT.0.00001.and.HILO.EQ.2) Then
            HILO=1
            ZONE=0
            Goto 10
         Endif
      Endif
!
170   Return
      End Subroutine RELEASE
!
! ****************************************************************************
!
      Subroutine ADJUST
!
!  Subroutine ADJUST reduces or increases the available streamflow at a control
!  point and all downstream control points by the depletion and return amounts.
!
      Use COMVAR
!
      Integer J,K,MONTH,NUSCP,NXCP
      Real CLX,LOSS,DELTA,XDEP
!
      NXCP=0
!
!  Streamflows at LOCNUM and downstream control points are adjusted for
!  the streamflow depletion (DEP) at control point LOCNUM.
!
      If(DEP.NE.0.0) Then
!
!  The following algorithm is activated for negative incremental flow
!  ADJINC options 1,2,3,4 or if the PX record XCP options are used.
!  Otherwise, the second alternative algorithm is activated.
!
         K=0
         If(ADJINC.LT.5) K=K+1
         If(WRNUM(WR,8).GT.0) Then
            If(XCP(WRNUM(WR,8)).GT.0) K=K+1
         Endif
!
!       Negative incremental flow options 1, 2, 3, or 4 and/or XCP option.
!
         If(K.GT.0) Then
            CPFLOW(LOCNUM,MT,2)=CPFLOW(LOCNUM,MT,2)-DEP
            If(CPFLOW(LOCNUM,MT,2).LT.0.0) Then
               If(ADJINC.EQ.3.or.ADJINC.EQ.4) Then
                  CPFLOW(LOCNUM,MT,1)=CPFLOW(LOCNUM,MT,1)
     +                               +CPFLOW(LOCNUM,MT,2)
                  If(CPFLOW(LOCNUM,MT,1).LT.0.0) CPFLOW(LOCNUM,MT,1)=0.0
               Endif
               CPFLOW(LOCNUM,MT,2)=0.0
            Endif
            NPT=CPNXT(LOCNUM)
            NUSCP=LOCNUM
            CLX=1.0
10          If(NPT.LT.0) Goto 30
!
!       PX record control point limit option.
!
            If(WRNUM(WR,8).GT.0.and.NXCP.EQ.0) Then
               J=WRNUM(WR,8)
               If(XCP(J).GT.0) Then
                  If(NPT.EQ.XCPI(J)) NXCP=NXCP+1
               Endif
            Endif
            If(NXCP.GT.0) Then
               If(DEP.GT.XCPAV1) XDEP=XCPAV1
               If(CL(NUSCP).NE.0.0.and.NOTF(WR).LT.998) Then
                  LOSS=XDEP*CLX*CL(NUSCP)
                  CLOSS(NUSCP,1)=CLOSS(NUSCP,1)+LOSS
                  CLX=CLX*(1.0-CL(NUSCP))
               Endif
               CPFLOW(NPT,MT,2)=CPFLOW(NPT,MT,2)-XDEP*CLX
!
!       End of PX record control point limit option.
!
            Else
               If(CL(NUSCP).NE.0.0.and.NOTF(WR).LT.998) Then
                  LOSS=DEP*CLX*CL(NUSCP)
                  CLOSS(NUSCP,1)=CLOSS(NUSCP,1)+LOSS
                  CLX=CLX*(1.0-CL(NUSCP))
               Endif
               CPFLOW(NPT,MT,2)=CPFLOW(NPT,MT,2)-DEP*CLX
            Endif
            If(CPFLOW(NPT,MT,2).LT.0.0) Then
               If(ADJINC.GE.3) Then
                  CPFLOW(NPT,MT,1)=CPFLOW(NPT,MT,1)+CPFLOW(NPT,MT,2)
                  If(CPFLOW(NPT,MT,1).LT.0.0) CPFLOW(NPT,MT,1)=0.0
               Endif
               CPFLOW(NPT,MT,2)=0.0
            Endif
            NUSCP=NPT
            NPT=CPNXT(NPT)
!
!       The adjustment is repeated for the next downstream control point.
!
            Goto 10
!
!       Negative incremental flow option 5.
!
         Else
            DELTA=DEP
            NPT=LOCNUM
            CLX=1.0
20          If(NPT.LT.0) Goto 30
            CPFLOW(NPT,MT,2)=CPFLOW(NPT,MT,2)-DELTA*CLX
            If(CPFLOW(NPT,MT,2).LT.0.0) Then
               DELTA=CPFLOW(NPT,MT,2)+DELTA*CLX
               CLOSS(NPT,1)=CLOSS(NPT,1)+DELTA*CL(NPT)
               CPFLOW(NPT,MT,2)=0.0
            Else
               CLOSS(NPT,1)=CLOSS(NPT,1)+DELTA*CL(NPT)
               DELTA=DELTA*CLX
            Endif
            CLX=(1.0-CL(NPT))
            NPT=CPNXT(NPT)
            If(CLX.EQ.0.0) Goto 30
            Goto 20
         Endif
      Endif
!
!  Streamflows at LOCNUM and downstream control points are adjusted for
!  the return flow RET at control point LOCNUM.
!
30    If(RET.NE.0.) Then
         If(RFAC.LT.0) Then
            MONTH=MT+1
         Else
            MONTH=MT
         Endif
         CPFLOW(RETNUM,MONTH,2)=CPFLOW(RETNUM,MONTH,2)+RET
         NUSCP=RETNUM
         NPT=RETNUM
         CLX=1.0
40       NPT=CPNXT(NPT)
         If(NPT.LT.0) Goto 50
         If(CL(NUSCP).NE.0.0.and.NOTF(WR).LT.98) CLX=CLX*(1.0-CL(NUSCP))
         CPFLOW(NPT,MONTH,2)=CPFLOW(NPT,MONTH,2)+RET*CLX
         NUSCP=NPT
         Goto 40
      Endif
!
!  CPFLOW(J,MT,2) is flagged as -9.0 for CP record INMETHOD(J) option 9.
!
50    If(CPFLAG.GT.0) Then
         Do J=1,MAXCP
            If(INMETHOD(J).EQ.9) CPFLOW(J,MT,2)=0.0
         End Do
      Endif
!
      Return
      End Subroutine ADJUST
!
! ***************************************************************************
!
      Subroutine CUMREL(LOC,REL)
!
!  Subroutine CUMREL accumulates releases from reservoirs to downstream senior
!  water rights.  CUMREL is used by the release routine to aid in making release
!  decisions for reservoirs that are not upstream of a water right location or
!  are unable to release directly to a water right.  It keeps track of the total 
!  streamflow depletions for senior rights below a control point that may be
!  supplied in order to free water at the water right location.
!
      Use COMVAR
      Integer LOC,PT
      Real REL,CLX
!
      PT=Abs(LOC)
      CLX=1.0
10    If(PT.LT.0.or.REL.EQ.0.0) Goto 20
      DMRSUM(PT)=DMRSUM(PT)-REL*CLX
      If(DMRSUM(PT).LT.0) Then
         REL=-DMRSUM(PT)
         DMRSUM(PT)=0.0
      Else
         REL=0.0
      Endif
      CLX=(1.0-CL(PT))
      If(CLX.EQ.0.0) Goto 20
      PT=CPNXT(PT)
      Goto 10
20    Return
      End Subroutine CUMREL
!
! ***************************************************************************
!
      Subroutine RESCAL(IDNUM,LOSTOR,HISTOR,IN,OUT,ESY,EVAPY)
!
!  Subroutine RESCAL performs the iterative computations to determine the
!  reservoir outflow OUT, end-of-period storage ESY, and net evaporation
!  volume EVAPY.
!
      Use COMVAR
!
      Real AREAVE,BEGSTO,BPAREA,BSY,ENDSTO,EPAREA,EPST1,EPST2,ESY,
     +     EVACST,EVAPY,HISTOR,IN,INEST,LOSTOR,OUT,OUTEST,PREIN,
     +     PREOUT,STOP1,STOP2,STORAGE,X
      Integer I,J,IDNUM,MAXIT,N
!
      MAXIT=50
      EVACST=EVAPR(RESNUM(IDNUM,1),MT)
      PREIN=RESDAT(IDNUM,8)+RESDAT(IDNUM,11)
      PREOUT=RESDAT(IDNUM,9)+RESDAT(IDNUM,10)
      INEST=IN+PREIN
      EVCFA=RESDAT(IDNUM,2)
      EVCFB=RESDAT(IDNUM,3)
      EVCFC=RESDAT(IDNUM,4)
      BSY=Min(RESDAT(IDNUM,5),HISTOR)
      BEGSTO=BSY
!
!  The end-of-period storage volume EPST1 is initially estimated as
!  the beginning-of-period storage volume BSY.
!  The initial estimate of total releases from the reservoir OUTEST
!  is set as the target release OUT for this water right plus any
!  previous releases PREOUT for senior rights.
!  Subroutine RESCAL iteratively computes improved values for OUT and
!  OUTEST along with corresponding volumes of net evaporation EVAPY
!  and end-of-period storage ESY.
!
      EPST1=BSY
      OUTEST=OUT+PREOUT
!
!  Evaporation volume EVAPY is computed for given BSY, EPST1, and OUTEST.
!
!------------- Evaporation Allocation EA Record Reservoirs -----------------
!  For the evaporation allocation option, total storage in all reservoirs
!  on the EA record is used to determine water surface area. The beginning
!  storage BEGSTO is the total for all reservoirs. The ending storage ENDSTO
!  is estimated by summing the latest ending storages if already computed
!  and beginning storages for junior reservoirs for which ending storage is
!  still unknown. BEGSTO and ENDSTO are used only for determining surface
!  area. BEGSTO includes this reservoir; ENDSTO does not.
!
      If(EAR(IDNUM).GT.0.and.(WRT.EQ.1.or.WRT.EQ.7)) Then
         STORAGE=0.0
         N=EAR(IDNUM)
         If(NEAF(N).LE.1.or.NEAF(N).EQ.4) Then
            BEGSTO=CUMBEG(N)+RESDAT(IDNUM,5)
            ENDSTO=CUMEND(N)
         Else
            BEGSTO=EASBEG
            ENDSTO=EASEND-RESDAT(IDNUM,6)
         Endif
      Endif
!---------------------------------------------------------------------------
!
!  The beginning-of-period water surface area is determined.
!
      If(EVCFA.LT.0.0) Then
         Call LINEAR(BEGSTO,BPAREA,RESNUM(IDNUM,2),EVCURV)
      Else
         BPAREA=EVCFA*(BEGSTO**EVCFB)+EVCFC
      Endif
!
!  Begin iterations to determine end-of-period storage and evaporation.
!
      I=0
100   I=I+1
!
!  End-of-period and average water surface areas are determined.
!
      STORAGE=EPST1
!
!  ------------- EA record reservoir --------------------
!
      If(EAR(IDNUM).GT.0.and.(WRT.EQ.1.or.WRT.EQ.7)) Then
         STORAGE=EPST1+ENDSTO
      Endif
!--------------------------------------------------------
!
      If(EVCFA.LT.0.0) Then
         Call LINEAR(STORAGE,EPAREA,RESNUM(IDNUM,2),EVCURV)
      Else
         EPAREA=EVCFA*(EPST1**EVCFB)+EVCFC
      Endif
      AREAVE=(BPAREA+EPAREA)/2
!
!  Initial estimate of evaporation volume EVAPY.
!
!  ------------- EA record reservoir --------------------
!
      If(EAR(IDNUM).GT.0.and.(WRT.EQ.1.or.WRT.EQ.7)) Then
         EVAPY=AREAVE*EVACST
         If(NEAF(N).LE.1) EVAPY=EVAPY-CUMEV(N)
         If(NEAF(N).GE.2) Then
            If(JRES.EQ.EARNUM(N).and.EAX(N).EQ.0) Then
               X=0.0
               Do J=1,EARNUM(N)-1
                  X=X+RESDAT(EAI(N,J),7)
               End Do
               EVAPY=EVAPY-X
            Else
              If(NEAF(N).EQ.2.or.NEAF(N).EQ.3) Then
                 EVAPY=EVAPY*EAFACT
              Elseif(NEAF(N).EQ.4) Then
                 EVAPY=(EVAPY-CUMEV(N))*EAFACT
              Endif
            Endif
         Endif
!--------------------------------------------------------
!
      Else
         EVAPY=Min(AREAVE*EVACST,BSY+INEST)
      Endif
!
!  Improved estimates of the reservoir volume accounting budget.
!
!     Net evaporation EVAPY can not be greater than the sum of
!     (BSY + INEST - PREOUT) even with zero new releses OUT added
!     to releases PREOUT associated with previous senior rights.
!
      If(Abs(BSY+INEST-PREOUT).LE.0.00001) Then
         X=EVAPY
      Else
         X=(EVAPY-(BSY+INEST-PREOUT))/Abs(BSY+INEST-PREOUT)
      Endif
      If(X.GT.0.00001) Then
         EVAPY=BSY+INEST-PREOUT
         OUTEST=PREOUT
      Endif
!
!     Updated estimate of end-of-period storage volume EPST2 equals
!     beginning-of-period storage volume BSY plus inflow minus outflow.
!
      EPST2=BSY+INEST-OUTEST-EVAPY
!
!     Revised estimates for particular storage and flow conditions.
!     X computed below is used in the Elseif statement below to assure
!     to algorithm is not sensitive to internal computer precision.
!
      If(Abs(HISTOR).LE.0.00001) Then
         X=EPST2
      Else
         X=(EPST2-HISTOR)/HISTOR
      Endif
!
!     If EPST2 is below bottom of active pool LOSTOR or less than zero.
!
      If(EPST2-LOSTOR.LE.-0.0001) Then
         If(OUTEST-PREOUT.GT.0.0001) Then
            OUTEST=Max(PREOUT,OUTEST-(LOSTOR-EPST2))
            EPST2=Max(LOSTOR,BSY+INEST-OUTEST-EVAPY)
         Else
            OUTEST=PREOUT
            EPST2=Max(0.0,BSY+INEST-OUTEST-EVAPY)
         Endif
      Elseif(EPST2.LT.0.0001) Then
         EPST2=0.0
         OUTEST=Max(PREOUT,BSY+INEST-EVAPY)
!
!     If EPST2 is above top of active pool HISTOR or in the active pool.
!
      Elseif(X.GT.0.00001) Then
         EPST2=HISTOR
         OUTEST=BSY-EPST2+INEST-EVAPY
      Elseif(EPST2-LOSTOR.GT.-0.0001) Then
         OUTEST=Min(PREOUT+OUT,OUTEST+EPST2-LOSTOR)
      Endif
!
!  A second iteration is performed.
!
      If(I.LT.2) Then
         EPST1=EPST2
         Goto 100
      Endif
!
!  Stop criteria are set for use in the determination of whether
!  or not to continue to repeat the iterative computations.
!
      STOP1=Abs(EPST1-EPST2)
      If(EPST1.GT.0.0) Then
         STOP2=STOP1/EPST1
      Else
         STOP2=0.0
      Endif
      If((STOP1.GE.0.1.or.STOP2.GE.0.0001).and.I.LE.MAXIT) Then
         EPST1=EPST2
         Goto 100
!
!  A warning message is written to MSS file if maximum iteration limit is
!  reached, stop criteria have not been met, and STOP2 GE 0.0000010.
!
      Elseif(STOP1.GE.0.1.and.I.GT.MAXIT.and.ICHECK.EQ.1) Then
         STOP2=STOP1/EPST2
         If(STOP2.GE.0.000001) Then
            If(ICHECK.EQ.1) Then
               Write(14,110) Adjustl(WRID(WR))
               Write(14,120) Adjustl(RESID(IDNUM)),
     +                   Adjustl(CPID(RESNUM(IDNUM,1),1)),YEAR,MT,EVAPY
               Write(14,130) BSY,EPST1,EPST2
110            Format(' WARNING: Reservoir storage and evaporation ',
     +            'computations did not converge to within',/,10x,
     +            'stop criteria in 50 iterations for water right ',A16)
120            Format(10x,'Reservoir: ',A6,'; CP: ',A6,'; Year:',I5,
     +                    '; Month:',I2,';  Final Evap:',F8.2)
130            Format(10x,'Begin Storage:',F10.2,'; 49th End Storage:',
     +                F10.2,'; 50th End Storage adopted:',F10.2)
            Endif
         Endif
      Endif
!
!  Final storage, inflow, and outflow volumes are set.
!
      ESY=EPST2
      IN=INEST
      OUT=OUTEST
      Return
      End Subroutine RESCAL
!
! ***************************************************************************
!
      Subroutine LINEAR(GIVEN,YFIND,NUM,DUMMY)
!
!  Subroutine LINEAR performs linear interpolation of tables of reservoir area
!  versus volume; elevation versus volume; or volume versus flow.
!
      Use COMVAR
!
      Real GIVEN,YFind,DUMMY(MAXTAB,TLD),X1,X2,Y1,Y2,XMAX,YMAX
      Integer I,J,NUM,MIN
!
      XMAX=0.0
      YMAX=0.0
      GIVEN=Max(0.0,GIVEN)
!
!  Error checks.
!
      If(NUM.LE.0) Then
         Write(14,10) NUM,Adjustl(WRID(WR))
10       Format(' ERROR: Table number must be a positive integer.',/,
     +       8x,'Table number of',I5,' associated with water right ',
     +       A16,/,8x,'Stopped in linear interpolation subroutine.')
         Call ERROR
      Endif
      If(ICHECK.EQ.1.and.DUMMY(NUM,2)-GIVEN.GE.0.0001) Then
         Write(14,20) NUM,Adjustl(WRID(WR))
20       Format(' WARNING: In linear interpolation routine, the given',
     +          ' amount was less than smallest value in table',I3,/,
     +          10x,'Extrapolated based on adding zero/zero point.',
     +          '   Water Right: ',A16)
         Write(14,30) YEAR,MT,DUMMY(NUM,2),GIVEN
30       Format(10x,'Year:',I5,'  Month:',I3,'   Smallest in table:',
     +          F9.2,'   Given:',F10.3)
      Endif
!
!  The values of X in the X-Y table that bracket YFIND are found.
!
      J=(TLD-1)/2
      Do 40 I=1,J
         If(DUMMY(NUM,(I*2)).GT.XMAX) Then
            XMAX=DUMMY(NUM,(I*2))
            YMAX=DUMMY(NUM,(I*2)+1)
         Endif
         If(Abs(GIVEN-DUMMY(NUM,I*2)).LT.0.001) Then
            YFIND=DUMMY(NUM,(I*2)+1)
            Goto 90
         Endif
         If(GIVEN.GT.DUMMY(NUM,I*2)) Goto 40
         If(I.EQ.1) Then
            X1=0.0
            Y1=0.0
            X2=DUMMY(NUM,2)
            Y2=DUMMY(NUM,3)
            Goto 80
         Endif
         MIN=(I*2)-2
         Goto 70
40    End Do
!
!  Y is set at maximum Y in table if X exceeds maximum X in table.
!
      YFIND=YMAX
      If(ICHECK.EQ.1.and.GIVEN.GT.1.00001*XMAX) Then
      Write(14,50) NUM,XMAX,GIVEN,YFIND
50       Format(' WARNING: Interpolation routine exceeded range of',
     +          ' table',I3,/,10x,'max X =',F11.2,'   given X =',F12.3,
     +          '   Y = max Y =',F10.1)
         Write(14,60) YEAR,MT,Adjustl(WRID(WR))
60       Format(10x,'Year:',I5,'  Month:',I3,'    Water Right: ',A16)
      Endif
      Goto 90
!
!  The linear interpolation is performed.
!
70    Y1=DUMMY(NUM,MIN+1)
      Y2=DUMMY(NUM,(MIN+3))
      X1=DUMMY(NUM,MIN)
      X2=DUMMY(NUM,MIN+2)
80    YFIND=Y1+(GIVEN-X1)*((Y2-Y1)/(X2-X1))
!
90    Return
      End Subroutine LINEAR
!
! ***************************************************************************
!
      Subroutine POWER
!
!  Subroutine POWER performs hydroelectric power computations.
!  Subroutine POWER is called only for hydropower rights.
!
      Use COMVAR
!
      Integer I,IDNUM,J,L,MAXIT,N
      Real BSZ,BPAREA,BPELEV,EABEG,EAEND,EPAREA,EPELEV,ERRMAX,ELAVE,ESZ,
     +     EVAPZ,HEAD,LOLIM,MINLEVEL,MINSTOR,PREPASS,PREREL,QREL,QT,
     +     RELS1,STEP,STOINV,TAR,TOTAVAIL,TWATER,TWATER1,UPLIM,X
!
      IDNUM=SN1(SWR,1)
      MAXIT=50
      ERRMAX=0.0001*AMT
      BSZ=RESDAT(IDNUM,5)
      FRMPOW=AMT
      RELS=0.0
      RELS1=0.0
      RESDAT(IDNUM,13)=FRMPOW
      TAR=0.0
      TWATER=WRSYS(1,10)
!
!  If the reservoir is connected to an evaporation allocation EA record,
!  total beginning and ending storage EABEG and EAEND for all EA record
!  reservoirs are summed.  EAEND total storage excludes this reservoir.
!
      EABEG=0.0
      EAEND=0.0
      If(EAR(IDNUM).GT.0) Then
         N=EAR(IDNUM)
         Do J=1,EARNUM(N)
            L=0
10          If(L.GT.NRES) Then
               Write(14,20) EARES(N,J),WRID(WR)
20             Format(' ERROR: Reservoir ',A6,' from EA record could',
     +                ' not be matched with WS record reservoir',
     +                ' identifiers.',/,8x,'Hydropower right: ',A16)
               Call ERROR
            Endif
            L=L+1
            If(RESID(L).NE.EARES(N,J)) Goto 10
            EABEG=EABEG+RESDAT(L,5)
            If(L.NE.IDNUM) EAEND=EAEND+RESDAT(L,6)
         Enddo
      Endif
!
!  Beginning-of-period reservoir elevation BPELEV for determining head.
!
      If(EAR(IDNUM).GT.0) Then
         Call LINEAR(EABEG,BPELEV,RESNUM(IDNUM,4),PVCURV)
      Else
         Call LINEAR(BSZ,BPELEV,RESNUM(IDNUM,4),PVCURV)
      Endif
!
!  Beginning-of-period surface area BPAREA for evaporation computations.
!
      If(RESDAT(IDNUM,2).LT.0.0) Then
         Call LINEAR(BSZ,BPAREA,RESNUM(IDNUM,2),EVCURV)
      Else
         EVCFA=RESDAT(IDNUM,2)
         EVCFB=RESDAT(IDNUM,3)
         EVCFC=RESDAT(IDNUM,4)
         BPAREA=EVCFA*(BSZ**EVCFB)+EVCFC
      Endif
!
!  The storage volume STOINV at the turbine invert is the inactive storage
!  WRSYS(1,2) from WS record or optionally elevation TELEV from HP record.
!
      STOINV=WRSYS(1,2)
      If(TELEV(IDNUM).GT.0.0) Then
         UPLIM=WRSYS(1,3)
         LOLIM=0.0
30       STOINV=(UPLIM+LOLIM)/2.0
         Call LINEAR(STOINV,MINLEVEL,RESNUM(IDNUM,4),PVCURV)
         If(Abs(MINLEVEL-TELEV(IDNUM)).GE.0.001) Then
            If(MINLEVEL.GT.TELEV(IDNUM)) Then
               UPLIM=STOINV
            Else
               LOLIM=STOINV
            Endif
            Goto 30
         Endif
      Endif
!
!  Previous releases through the turbines for other water rights
!  associated with the reservoir.
!
      PREREL=RESDAT(IDNUM,9)
!
!  Flows passed through the dam for downstream senior rights
!  not associated with the reservoir.
!
      QREL=0.0
      Do J=1,NCPTS
         If(CPNXT(J).EQ.RESNUM(IDNUM,1)) Then
            QREL=QREL+RESREL(J)*(1.0-CL(J))
         Endif
      End Do
      PREPASS=CPFLOW(RESNUM(IDNUM,1),MT,2)+QREL-AVAMT
!
!  Reservoir storage refilling and power production is computed
!  initially assuming no additional reservoir releases are made.
!
!     Net evaporation-precipitation volume EVAPZ.
!
      ESZ=WRSYS(1,3)
      If(RESDAT(SN1(SWR,1),2).LT.0.0) Then
         Call LINEAR(ESZ,EPAREA,RESNUM(SN1(SWR,1),2),EVCURV)
      Else
         EVCFA=RESDAT(SN1(SWR,1),2)
         EVCFB=RESDAT(SN1(SWR,1),3)
         EVCFC=RESDAT(SN1(SWR,1),4)
         EPAREA=EVCFA*(ESZ**EVCFB)+EVCFC
      Endif
      EVAPZ=EVAPR(RESNUM(SN1(SWR,1),1),MT)*(EPAREA+BPAREA)/2.0
!
!     Target additional reservoir inflows to refill storage.
!     Hold current releases from system reservoirs.
!
      TAR=ESZ-BSZ+EVAPZ
     +    +RESDAT(SN1(SWR,1),9)+RESDAT(SN1(SWR,1),10)
     +    -RESDAT(SN1(SWR,1),8)-RESDAT(SN1(SWR,1),11)
      If(TAR.LT.0.0) TAR=0.0
      DEP=TAR
      INRES=DEP
      If(TAR.GE.0.001) Then
         X=(TAR-AVAMT)/TAR
         If(X.GE.0.001) Then
            DEP=AVAMT
            MAKEUP=TAR-AVAMT
            If(NSR(SWR).GE.2) Call RELEASE
            INRES=AVAMT+RELS
            RELS1=RELS
         Endif
      Endif
      If(TAR-INRES.GE.0.001) Then
         OUTRES=0.0
         Call RESCAL(IDNUM,WRSYS(1,2),WRSYS(1,3),INRES,
     +        OUTRES,ESZ,EVAPZ)
      Endif
!
!     Average water surface elevation ELAVE during the period (month).
!
      Call LINEAR(ESZ+EAEND,EPELEV,RESNUM(IDNUM,4),PVCURV)
      ELAVE=(EPELEV+BPELEV)/2.0
!
!     Power produced POWPRO as function of turbine flow QT and head HEAD,
!
      QT=PREREL+PREPASS
      If(RESNUM(IDNUM,5).GT.0)
     +   Call LINEAR(QT,TWATER,RESNUM(IDNUM,5),TWCURV)
      HEAD=Max(ELAVE-TWATER,0.0)
      POWPRO=POWFCT*HEAD*QT*WRSYS(1,9)
!
!  No power is produced if the average elevation is below the invert.
!
      If(TELEV(IDNUM)-ELAVE.GE.0.001.and.
     +   Abs(TELEV(IDNUM)).GE.0.001) Then
         POWPRO=0.0
         OUTRES=0.0
         DEP=AVAMT
         MINSTOR=STOINV-EAEND
         If(MINSTOR.LT.0.0) MINSTOR=0.0
         Goto 100
      Endif
!
!  No further releases through turbines are necessary if power produced
!  with releases and pass-through flows exceeds the power requirement.
!
      If(POWPRO-FRMPOW.GE.-0.0001) Then
         DEP=Min(TAR,AVAMT)
         OUTRES=0
         MINSTOR=STOINV-EAEND
         If(MINSTOR.LT.0.0) MINSTOR=0.0
         Goto 100
      Endif
!
!  If power requirements are not met incidental to other releases,
!  additional depletions and system reservoir releases are determined.
!
!     Reservoir was refilled and average elevation is known.
!     QT = total reservoir release required to meet hydropower target
!
      If(ESZ-WRSYS(1,3).GE.-0.0001) Then
!
!        Tailwater rating curve option (TQ/TE records).
!
         If(RESNUM(IDNUM,5).NE.0) Then
40          HEAD=Max(ELAVE-TWATER,0.0)
            If(HEAD.NE.0.0) Then
               QT=FRMPOW/(POWFCT*WRSYS(1,9)*HEAD)
            Else
               QT=0.0
            Endif
            Call LINEAR(QT,TWATER1,RESNUM(IDNUM,5),TWCURV)
            If(Abs(TWATER-TWATER1).GE.0.01) Then
               TWATER=TWATER1
               Goto 40
            Endif
!
!        Constant tailwater option.
!
         Else
            HEAD=Max(ELAVE-TWATER,0.0)
            If(HEAD.NE.0.0) Then
               QT=FRMPOW/(POWFCT*WRSYS(1,9)*HEAD)
            Else
               QT=0.0
            Endif
         Endif
!
!        OUTRES = additional releases from reservoir needed for power
!
         OUTRES=QT-PREREL-PREPASS
         TAR=TAR+OUTRES
         If(AVAMT-TAR.GT.-0.0001) Then
            DEP=TAR
            POWPRO=FRMPOW
            MAKEUP=0.0
            Goto 100
         Else
            DEP=AVAMT
            MAKEUP=TAR-DEP-RELS1
            If(NSR(SWR).GE.2) Call RELEASE
            RELS=RELS+RELS1
            If(Abs((RELS-RELS1)-MAKEUP).LE.0.001) Then
               OUTRES=QT-PREREL-PREPASS
               POWPRO=FRMPOW
               MAKEUP=0.0
               Goto 100
            Endif
         Endif
      Endif
!
!  If insufficient streamflow and system reservoir releases are available to
!  refill storage and meet power requirements, an iterative algorithm is used
!  to determine the end-of-period storage and power produced.
!  Reservoir releases OUTRES are changed in 10 percent flow increments until
!  either the total storage, available streamflow, and system releases are
!  exhausted, or until the power requirement is meet. Releases are controlled
!  by the maximum value of (1) inactive pool (2) invert elevation.
!
      If((ESZ+EAEND-WRSYS(1,2)).GT.0.0001) Then
         MINSTOR=Max(WRSYS(1,2),STOINV)
         MINSTOR=MINSTOR-EAEND
         If(MINSTOR.LT.0.0) MINSTOR=0.0
         STEP=0.1
         I=0
         DEP=AVAMT
         LOLIM=0.0
         TOTAVAIL=ESZ-MINSTOR+AVAMT+RELS
         UPLIM=TOTAVAIL
         INRES=DEP+RELS
50       OUTRES=LOLIM+(UPLIM-LOLIM)*STEP
60       I=I+1
         Call RESCAL(IDNUM,MINSTOR,WRSYS(1,3),INRES,OUTRES,ESZ,EVAPZ)
         INRES=DEP+RELS
         OUTRES=OUTRES-RESDAT(IDNUM,9)-RESDAT(IDNUM,10)
         If(OUTRES.LT.0.0) OUTRES=0.0
         Call LINEAR(ESZ+EAEND,EPELEV,RESNUM(IDNUM,4),PVCURV)
         ELAVE=(EPELEV+BPELEV)/2.0
         QT=OUTRES+PREREL+PREPASS
         If(RESNUM(IDNUM,5).GT.0)
     +                 Call LINEAR(QT,TWATER,RESNUM(IDNUM,5),TWCURV)
         HEAD=Max(ELAVE-TWATER,0.0) 
         POWPRO=POWFCT*HEAD*QT*WRSYS(1,9)
         If(Abs(POWPRO-FRMPOW).LT.ERRMAX) Then
            Goto 100
         Elseif((POWPRO.LT.FRMPOW).and.(I.LT.MAXIT)) Then
            If (Abs(ESZ-MINSTOR).LT.ERRMAX) Goto 100
            OUTRES=OUTRES+(UPLIM-LOLIM)*STEP
            Goto 60
         Elseif((POWPRO.GT.FRMPOW).and.(I.LT.MAXIT)) Then
            LOLIM=OUTRES-(UPLIM-LOLIM)*STEP
            UPLIM=OUTRES
            Goto 50
         Elseif(I.GE.MAXIT.and.ICHECK.EQ.1) Then
!
!  Warning message written to MSS file if maximum iteration limit
!  is reached and error is still greater than 0.0001 of target.
!
            Write(14,70) Adjustl(WRID(WR))
            Write(14,80) Adjustl(RESID(IDNUM)),YEAR,MT,FRMPOW
            Write(14,90) BSZ,POWPRO
70          Format(' WARNING: Energy produced did not converge to ',
     +             'within 0.01 percent of target in 50 iterations',
     +             /,10x,'of iterative hydropower computations for',
     +             ' water right ',A16)
80          Format(10x,'Reservoir: ',A6,'   Year:',I5,'   Month:',I2,
     +                 '    Energy Target:',F9.2)
90          Format(10x,'Beginning Storage:',F10.2,6x,
     +                 '50th POWPRO adopted:',F9.2)
         Endif
!
!  Else if end-of-period storage ESZ is below the bottom of power pool.
!
      Else
         OUTRES=0.0
      Endif
!
!  The final release through the turbines is limited to not exceed turbine
!  capacity. If the previously computed turbine flow exceeds capacity, the
!  flow is set at capacity, and evaporation, storage, and power recomputed.
!
100   QT=OUTRES+PREREL+PREPASS
      If((QT-TQCAP(IDNUM).GT.0.001).and.(TQCAP(IDNUM).GT.0.001)) Then
         QT=TQCAP(IDNUM)
         INRES=AVAMT+RELS
         OUTRES=QT-PREREL-PREPASS
         If(OUTRES.LT.0.0) OUTRES=0.0
         Call RESCAL(IDNUM,MINSTOR,WRSYS(1,3),INRES,OUTRES,ESZ,EVAPZ)
         OUTRES=OUTRES-RESDAT(IDNUM,9)-RESDAT(IDNUM,10)
         Call LINEAR(ESZ+EAEND,EPELEV,RESNUM(IDNUM,4),PVCURV)
         ELAVE=(EPELEV+BPELEV)/2.0
         If(RESNUM(IDNUM,5).GT.0)
     +      Call LINEAR(QT,TWATER,RESNUM(IDNUM,5),TWCURV)
         HEAD=Max(ELAVE-TWATER,0.0)
         POWPRO=POWFCT*HEAD*QT*WRSYS(1,9)
         If(ESZ.GE.BSZ) Then
            DEP=ESZ-BSZ+EVAPZ+OUTRES+RESDAT(SN1(SWR,1),9)
     +          +RESDAT(SN1(SWR,1),10)-RESDAT(SN1(SWR,1),8)
     +          -RESDAT(SN1(SWR,1),11)
            If(DEP.GT.AVAMT) DEP=AVAMT
         Else
            DEP=AVAMT
         Endif
      Endif
!
!  The energy produced POWPRO can not exceed the energy limit TPCAP(IDNUM)
!  from HP record that is not less than the firm energy target AMT.
!
      If(TPCAP(IDNUM).GE.0.001) Then
         If(TPCAP(IDNUM).GT.FRMPOW) Then
            If(POWPRO.GT.TPCAP(IDNUM)) POWPRO=TPCAP(IDNUM)
         Endif
      Endif
!
!  Rounding error in power shortage computations is removed.
!
      If(Abs(POWPRO-FRMPOW).LE.ERRMAX) POWPRO=FRMPOW
!
!  The final computed amounts are saved in reservoir data arrays.
!
      RET=OUTRES
      RESDAT(IDNUM,6)=ESZ
      RESDAT(IDNUM,7)=EVAPZ
      RESDAT(IDNUM,8)=RESDAT(IDNUM,8)+DEP
      RESDAT(IDNUM,9)=RESDAT(IDNUM,9)+OUTRES
      RESDAT(IDNUM,11)=RESDAT(IDNUM,11)+RELS
      RESDAT(IDNUM,12)=POWPRO
      SYSREL(1)=OUTRES
!
      Return
      End Subroutine POWER
!
!***********************************************************************
!
      Subroutine FLOWADJ
!
!  Subroutine FLOWADJ adds flow adjustments it reads from a FAD file to
!  the flows from IN records. The flow adjustments are cascaded downstream
!  accounting for channel losses. The subroutine was originally developed
!  for adjusting naturalized flows for the effects of aquifer management
!  scenarios on spring flows, but may also be used for other adjustments.
!
      Use COMVAR
!
      Real DELTA
      Integer SPYR,FYEAR,NUSCP,I,J,K
      Character(len=2) TEMP2
      Character(len=6) ID
      Character(len=96) TEMP
!
      Real,Allocatable,Dimension(:)::SDELTA
      Real,Allocatable,Dimension(:,:)::NDELTA,PDELTA
!
      Allocate(NDELTA(NCPTS,MAXMON),PDELTA(NCPTS,MAXMON),SDELTA(MAXMON))

      Do J=1,NCPTS
         Do K=1,12
            NDELTA(J,K)=0.0
            PDELTA(J,K)=0.0
         End Do
      End Do
!
!  The number of control points in the FAD file is counted and control point
!  indices are assigned the first time Subroutine FLOWADJ is called, 
!
      If(SCOUNT.LT.1) Then
10       Read(21,"(A2)",END=100) TEMP2
         If(TEMP2.EQ.'FA') Then
            Backspace(21)
         Else
            Goto 10
         Endif
20       Read(21,200) ID,SPYR
         If(SCOUNT.LT.1) Then
            FYEAR=SPYR
         Elseif(SPYR.NE.FYEAR) Then
            Goto 40
         Endif
         SCOUNT=SCOUNT+1
         Do 30 I=1,NCPTS
            If(ID.EQ.CPID(I,1)) Then
               SINDEX(SCOUNT)=I
               Goto 20
            Endif
30       End Do
         Write(14,210) ID
         Call ERROR
40       Rewind(21)
      Endif
!
!  Skip over comments before FA records for each year.
!
50    Read(21,"(A2)",End=100) TEMP2
      If(TEMP2.EQ.'FA') Then
         Backspace(21)
      Else
         Goto 50
      Endif
!
!  FA record adjustments are read and accumulated.
!
      Do 90 I=1,SCOUNT
60       Read(21,220,End=100) ID,SPYR,TEMP
         If(SPYR.LT.YEAR) Then
            Goto 60
!
!           Error check.
!
         Elseif(SPYR.GT.YEAR) Then
            Write(14,230) YEAR
            If(I.EQ.1) Then
               Backspace(21)
               Write(14,270) YEAR
               Goto 300
            Else
               Write(14,240) ID,SPYR,YEAR
               Call ERROR
            Endif
         Else
!
!           FA record adjustments are read,
!
            Read(TEMP,250) (SDELTA(K),K=1,12)
            Do 80 K=1,12
!
!           Negative adjustments.
!
               DELTA=SDELTA(K)
               NPT=SINDEX(I)
               If(SDELTA(K).LT.0.0) Then
                  DELTA=Max(DELTA,-1.0*INFLOW(NPT,K))
                  INFLOW(NPT,K)=INFLOW(NPT,K)+DELTA
               Endif
               NUSCP=SINDEX(I)
               NPT=CPNXT(NUSCP)
70             If(NPT.LT.0) Goto 80
!
!  Translate negative flow changes to each control point and increment
!  positive flow changes at each control point.  Positive flow changes will
!  be applied after application of all negative flow changes.
!
               If(DELTA.LT.0.0) Then
                  DELTA=Max(DELTA*(1.0-CL(NUSCP)),-1.0*INFLOW(NPT,K))
                  INFLOW(NPT,K)=INFLOW(NPT,K)+DELTA
                  NDELTA(NPT,K)=NDELTA(NPT,K)+DELTA
               Else
                  DELTA=DELTA*(1.0-CL(NUSCP))
                  PDELTA(NPT,K)=PDELTA(NPT,K)+DELTA
               Endif
               If(DELTA.EQ.0.0) Goto 80
               NUSCP=NPT
               NPT=CPNXT(NPT)
               Goto 70
80          End Do
         Endif
90    End Do
!
!  Adjust all flows at each control point by positive flow changes.
!
      Do K=1,12
         Do J=1,NCPTS
            INFLOW(J,K)=INFLOW(J,K)+PDELTA(J,K)
         End Do
      End Do
      Goto 300
100   Write(14,230) YEAR
      Write(14,270) YEAR
200   Format(2X,A6,4X,I4)
210   Format(' ERROR: Control point identifier ', A6,' in FAD file ',
     +' matches no control point identifier on CP records.')
220   Format(2X,A6,4X,I4,A96)
230   Format(' WARNING: Year ',I4,' not found in FAD file.')
240   Format(' ERROR: Computations terminated due to error in FAD file',
     +/,'        Error occured at control point ',A6,/
     +  '        During year ',I4,' in FAD file'/
     +  '        During year ',I4,' of WRAP simulation'/
     +  '        Probably missing data for a control point')
250   Format(12F8.0)
270   Format(' WARNING: No FAD file adjustment made for year ',I4/)
!
!  End of Subroutine FLOWADJ
!
300   Deallocate(NDELTA,PDELTA,SDELTA)
      Return
      End Subroutine FLOWADJ
!
! ***************************************************************************
!
      Subroutine FADFILE
!
!  Subroutine FADFILE reads the FA records from the FAD file.  Subroutine
!  FADFILE is called for JO record parameter FAD option 2 to store the FA
!. record flow adjustments in a FAFLOW(cp,year,month) array which is used
!  in the main program to populate the FAF(cp,month) array used to adjust
!  the CPFLOW(cp,month) array along with the CI record adjustments.
!
      Use COMVAR
      Integer I,J,K,M,FYR,LYR,NFA
      Character(len=6) IDCP,IDCPX
      Write(14,*) '*** Starting to read FA records from FAD file.'
!
      NFA=0
      NFACP=0
      IDCPX='      '
!
!  JO record FAD option 2 means FA records are grouped by control point.
!
      If(FAD.EQ.2) Then
!
!  The number of control points in the FAD file is counted.
!
10       Read(21,20,End=30) CD,IDCP,I,J
20       Format(A2,A6,2I4)
         If(CD.EQ.'**') Goto 10
         If(CD.EQ.'ED') Goto 30
         If(CD.EQ.'FA') Then
            If(I.NE.0) Then
               NFA=NFA+J-I+1
            Else
               NFA=NFA+1
            Endif
            If(IDCP.NE.IDCPX.and.IDCP.NE.'      ') Then
               IDCPX=IDCP
               NFACP=NFACP+1
            Endif
         Else
            Write(14,150) CD
            Write(14,800)
            Call ERROR
         Endif
         Goto 10
30       I=NFA/NFACP
         If(I.NE.NYRS) Then
            Write(14,40) NFA,NFACP,I,NYRS
40          Format(' ERROR: Inconsistent FAD file counts:',/,8x,
     +             'Number of FA records =',I6,/,8x,
     +             'Number of control points =',I4,/,8x,
     +             'Number of years =',I4,' rather than',I4)
            Write(14,800)
            Call ERROR
         Endif
         Rewind(21)
!
!  Arrays are allocated and initialized.
!
         Allocate(FAFLOW(NFACP,NYRS,12),IDCPFA(NFACP))
         FAFLOW=0.0
         IDCPFA='      '
!
!  The 12 monthly flows are read for each FA record along with error checks.
!
         Do 200 J=1,NFACP
            IDCPX='      '
            YEAR=YRST-1
            Do 160 K=1,NYRS
               YEAR=YEAR+1
100            Read(21,110,End=960) CD
110            Format(A2)
               If(CD.EQ.'**') Goto 100
               If(CD.EQ.'FA') Then
                  Backspace(21)
                  Read(21,120,IOSTAT=STATUS) CD,IDCP,FYR,LYR,
     +                                    (FAFLOW(J,K,M),M=1,12)
120               Format(A2,A6,2I4,12F8.0)
                  If(STATUS.NE.0) Goto 900
                  If(K.EQ.1) IDCPX=IDCP
                  If(FYR.LE.0) FYR=LYR
                  If(IDCP.NE.'      '.and.IDCP.NE.IDCPX) Then
                     Write(14,*) ' '
                     Write(14,130) IDCP,IDCPX
130                  Format(' ERROR: In reading FA records, read '
     +                 'control point ID of ',A6,' when expecting ',A6)
                     Write(14,800)
                     Call ERROR
                  Endif
                  If(FYR.LT.YRST) Goto 100
                  If(LYR.GT.YEAR) Backspace(21)
                  If(FYR.GT.YEAR.or.FYR.GT.LYR) Then
                     Write(14,*) ' '
                     Write(14,140) IDCP,YEAR,FYR
140                  Format(' ERROR: In reading FA records for CP ',A6,
     +                      ' for year',I5,' read FYR of',I5)
                     Write(14,800)
                     Call ERROR
                  Endif
               Else
                  Write(14,*) ' '
                  Write(14,150) CD
150               Format(' ERROR: Read CD of ',A2,
     +                   ' when expecting FA record.')
                  Write(14,800)
                  Call ERROR
               Endif
               If(ICHECK.EQ.6) Write(14,120) CD,IDCP,FYR,LYR,
     +                                    (FAFLOW(J,K,M),M=1,12)
160         End Do
!
!  The integer control point identifier is assigned.
!
            I=0
170         I=I+1
            If(IDCPX.EQ.CPID(I,1)) Then
               IDCPFA(J)=I
               Goto 200
            Elseif(I.LT.NCPTS) Then
               Goto 170
            Else
               Write(14,*) ' '
               Write(14,180) IDCP
180            Format(' ERROR: CP ID of ',A6,' on FA record',
     +                ' matches no identifier on the CP records.')
               Write(14,800)
               Call ERROR
            Endif
200      Enddo
!
!  JO record FAD option 3 means FA records are grouped by year.
!
      Elseif(FAD.EQ.3) Then
!
!  The number of control points in the FAD file is counted.
!
410      Read(21,420,End=430) CD,IDCP
420      Format(A2,A6)
         If(CD.EQ.'**') Goto 410
         If(CD.EQ.'ED') Goto 430
         If(CD.NE.'FA') Then
            Write(14,150) CD
            Write(14,800)
            Call ERROR
         Else
            NFA=NFA+1
            If(NFA.EQ.1) Then
               IDCPX=IDCP
               NFACP=0
               K=0
            Else
               If(IDCP.EQ.IDCPX) K=K+1
            Endif
            If(K.EQ.0) NFACP=NFACP+1
         Endif
         Goto 410
430      I=NFA/NFACP
         If(I.NE.NYRS) Then
            Write(14,40) NFA,NFACP,I,NYRS
            Write(14,800)
            Call ERROR
         Endif
         Rewind(21)
!
!  Arrays are allocated and initialized.
!
         Allocate(FAFLOW(NFACP,NYRS,12),IDCPFA(NFACP))
         FAFLOW=0.0
         IDCPFA='      '
!
!  The integer control point identifier is assigned.
!
         Do 460 J=1,NFACP
440         Read(21,420,End=960) CD,IDCP
            If(CD.NE.'FA') Goto 440
            I=0
450         I=I+1
            If(IDCP.EQ.CPID(I,1)) Then
               IDCPFA(J)=I
               Goto 460
            Elseif(I.LT.NCPTS) Then
               Goto 450
            Else
               Write(14,*) ' '
               Write(14,180) IDCP
               Write(14,800)
               Call ERROR
            Endif
460      Enddo
         Rewind(21)
!
!  The 12 monthly flows are read from each FA record along with error checks.
!
         Do 490 K=1,NYRS
            Do 480 J=1,NFACP
470            Read(21,110,End=960) CD
               If(CD.EQ.'**') Goto 470
               If(CD.EQ.'FA') Then
                  Backspace(21)
                  Read(21,120,IOSTAT=STATUS) CD,IDCP,FYR,LYR,
     +                                    (FAFLOW(J,K,M),M=1,12)
                  If(STATUS.NE.0) Goto 900
               Else
                  Write(14,*) ' '
                  Write(14,150) CD
                  Write(14,800)
                  Call ERROR
               Endif
               If(ICHECK.EQ.6) Write(14,120) CD,IDCP,FYR,LYR,
     +                                       (FAFLOW(J,K,M),M=1,12)
480         End Do
490      End Do
      Endif
!
!  Error message.
!
800   Format(8x,'Stopped in Subroutine FADFILE which reads '
     +          'FA records in FAD file.')
!
!  IOSTAT error message
!
900   If(STATUS.NE.0) Then
         Write(14,*)' '
         Write(14,930) CD,STATUS
930      Format(' ERROR: Fortran IOSTAT error occurred reading an',
     +       ' input',/,8x,'record with CD identifier of ',A2,/
     +       '        IOSTAT status variable =',I6)
         Write(14,800)
         Write(*,*)' '
         Write(*,930) CD,STATUS
         Write(*,800)
         Write(*,*) ' '
         Backspace(21)
         Backspace(21)
         Write(14,*)' '
         Write(14,940)
940      Format('The first 82 characters of each of the last',
     +          ' two records read are as follows:')
         Read(21,950) CD,TITLE1
         Write(14,950) CD,TITLE1
         Read(21,950) CD,TITLE1
         Write(14,950) CD,TITLE1
         Write(14,*)' '
950      Format(A2,A78)
         Call ERROR
      Endif
!
!  End of Subroutine FADFILE
!
      Write(14,*) '*** Finished reading FA records from FAD file.'
      Return
!
!  Error message if end of file is reached too soon.
!
960   Write(14,*) ' '
      Write(14,970)
970   Format(' ERROR: Reached end-of-file too soon while',
     +       ' reading FA records.')
      Call ERROR
      End Subroutine FADFILE
!
! ***************************************************************************
!
      Subroutine IACNP
!
!  Subroutine IACNP reads the flow distribution file [root.dis (unit=8)] and
!  determines the parameters (drainage area, CN, mean precipitation) for
!  total and/or incremental watersheds.  These parameters are used by 
!  Subroutine FLDIST to distribute flows and also adjust evap-precip depths.
!
      Use COMVAR
!
      Integer FCCOUNT,FCFLAG,I,II,K,KGA,KUG,L,M,MD,MU,N,NCP,NFD,NWP,
     +        NUMBERCP
      Real COEF1,COEF2,COEF3,CN1,DA1,DAF,DAF2,MP1,SUMCNDA,SUMMPDA
!
      Character(len=6)  DSG,ID,UGID(MAXGAG)
      Character(len=22) TEMPC
!
      Real,Allocatable,Dimension(:)::CN,DA,MP
      Real,Allocatable,Dimension(:,:)::DAG,DAUGC
      Allocate(CN(NCPTS),DA(NCPTS),MP(NCPTS))
      Allocate(DAG(NCPTS,MAXGAG),DAUGC(NCPTS,MAXGAG))
!
      Allocate(CNGAGE(NCPTS),CNUG(NCPTS),DAGAGE(NCPTS),DAUG(NCPTS),
     +         IDSG(NCPTS),IGAGE(NCPTS,MAXGAG),MPGAGE(NCPTS),
     +         MPUG(NCPTS),NGAGE(NCPTS,2))
!
!  Variables are initialized.
!
      FCCOUNT=0
      FCFLAG=0
      IGAGE=0
      IDSG=0
      NGAGE=0
      NFD=0
      NWP=0
      DAF=1.0
      UGID='        '
!
!  Records are read.  Error checks are performed as the data are read.
!
30    Read(8,40,IOSTAT=STATUS,END=600) CD
40    Format(A2)
      If(STATUS.NE.0) Call ERROR
      If(CD.EQ.'**') Then
         Goto 30
!
!  FD and FC records are read and checked.
!
      Elseif(CD.EQ.'FD') Then
         NFD=NFD+1
         Backspace(8)
         COEF1=0.0
         COEF2=1.0
         COEF3=0.0
         Read(8,50,IOSTAT=STATUS) CD,ID,DSG,NG,(UGID(K),K=1,MAXGAG)
50       Format(A2,A6,A8,I8,<MAXGAG>A8)
         ID=Adjustr(ID)
         DSG=Adjustr(DSG)
         Do K=1,MAXGAG
            UGID(K)=Adjustr(UGID(K))
         End Do
         If(STATUS.NE.0) Call ERROR
         If(ICHECK.EQ.7) Write(14,50) CD,ID,DSG,NG,(UGID(K),K=1,MAXGAG)
70       Read(8,80,IOSTAT=STATUS,END=600) CD,TEMPC
         If(STATUS.NE.0) Call ERROR
80       Format(A2,A22)
         If(CD.EQ.'**') Then
            Goto 70
         Elseif(CD.EQ.'ED') Then
            If(ICHECK.GE.0) Write(14,380) NFD,NWP
            Goto 390
         Elseif(CD.EQ.'FD'.or.CD.EQ.'WP') Then
            Backspace(8)
            Goto 130
         Elseif(CD.EQ.'FC') Then
            Read(TEMPC,90,IOSTAT=STATUS) COEF1,COEF2,COEF3
            If(STATUS.NE.0) Call ERROR
            If(ICHECK.EQ.7) Write(14,100) CD,COEF1,COEF2,COEF3
90          Format(F6.0,2F8.0)
100         Format(A2,F6.3,2F8.2)
            FCFLAG=1
            FCCOUNT=FCCOUNT+1
         Else
            Write(14,*) ' '
            Write(14,110) CD
110         Format(' ERROR: Found CD of ',A2,' in the DIS file, when',
     +             ' expecting FD, FC, or WP record.')
            Write(14,120)
120         Format(8X,'Stopped from Subroutine IACNP due to error.')
            Call ERROR
         Endif
!
!  The integer control point identifier I for the ungaged site is assigned
!  based on CP record order. Error messages are written and execution stopped
!  if no control point identifier on CP records match that on FD record.
!
130      I=0
140      I=I+1
         If(ID.NE.CPID(I,1)) Then
            If(I.GT.NCPTS) Then
               Write(14,*) ' '
               Write(14,150) ID,NFD
150            Format(' ERROR: ',A6,' from field 2 of',I4,' FD record'
     +                ,/,8x,'matches no control point',
     +                ' identifier on CP records.')
               Write(14,120)
               Call ERROR
            Endif
            Goto 140
         Endif
         If(INMETHOD(I).LE.2.and.ICHECK.EQ.1) Then
            If(Abs(EWA(I)).LT.0.1) Write(14,160) INMETHOD(I),ID
160         Format(' WARNING: INMETHOD =',I2,' and EWA = 0 from the',
     +             ' CP record for FD record ungaged',/,10x,
     +             ' control point ',A6)
         Endif
!
!  Coefficients read from the FC record are assigned to arrays.
!
         If(FCFLAG.GE.1) Then
            FCFLAG=0
            If(FCCOUNT.EQ.1) Then
               Allocate(COEF(NCPTS,3))
            Endif
            COEF(I,1)=COEF1
            If(INMETHOD(I).EQ.3.and.COEF2.EQ.0.0) COEF2=1.0
            COEF(I,2)=COEF2
            COEF(I,3)=COEF3
         Endif
!
!  For evap-precip depth adjustment [EWA(I)<0], DSG is not required, and field 3
!  on FD record may be blank.  Set DSG=CPID(I,1) to allow algorithms to continue
!  even if flows are not being distributed.
!
         If(EWA(I).LT.0.0.and.INMETHOD(I).LT.3) Then 
            If(DSG.EQ.'      ') DSG=ID
         Endif
!
!  Number of upstream gages entered on FD record NGAGE(I,1) is counted.
!  Integer control point identifier is assigned for each gage.
!  The number of upstream gages above ungaged site NG is assigned NGAGE(I,2).
!
         Do K=1,MAXGAG
            If(UGID(K).EQ.'      ') Goto 170
         End Do
170      NGAGE(I,1)=K-1
         NGAGE(I,2)=NG
         Do M=1,NCPTS
            If(DSG.EQ.CPID(M,1)) IDSG(I)=M
            Do K=1,NGAGE(I,1)
               If(UGID(K).EQ.CPID(M,1)) IGAGE(I,K)=M
            End Do
         End Do
!
!  The CPID and INMETHOD is checked for the source control point.
!
         If(IDSG(I).EQ.0.and.INMETHOD(I).NE.10.and.ICHECK.EQ.1) Then
            Write(14,180) DSG,ID
180         Format(' WARNING: The source gage CP identifier of ',A6,
     +             ' on FD record for CP ',A6,/,10x,'matches no CP',
     +             ' identifier on CP records.')
         Endif
         If(INMETHOD(IDSG(I)).GE.3.and.INMETHOD(IDSG(I)).NE.10.and.
     +               ICHECK.EQ.1) Write(14,190) DSG,ID,INMETHOD(IDSG(I))
190      Format(' WARNING: The source gage CP identifier of ',A6,
     +          ' on FD record for CP ',A6,/,10x,'has an INMETHOD of ',
     +          I2,' on its CP record which is not valid.')
!
!  Check that control point identifiers for upstream gages match CP records.
!
         If(NGAGE(I,1).GE.1.and.ICHECK.GE.1) Then
            Do 220 K=1,NGAGE(I,1)
               II=0
200            II=II+1
               If(II.GT.NCPTS) Then
                  Write(14,*) ' '
                  Write(14,210) UGID(K),ID
210               Format(' ERROR: Upstream gage identifier ',A6,
     +                   ' from FD record for CP ',A6,/,8x, 
     +                   'matches no CP identifier on CP records.')
                  Write(14,120)
                  Call ERROR
               Endif
               If(UGID(K).EQ.CPID(II,1)) Goto 220
               Goto 200
220         End Do
            If(INMETHOD(II).GT.2.and.INMETHOD(II).NE.10.and.ICHECK.EQ.1)
     +      Write(14,230) ID,INMETHOD(II),UGID(K)
230         Format(' WARNING: FD record for cp ',A6,' lists upstream ',
     +             'cp ',A6,' with INMETHOD=',I2,' which is not valid.')
         Endif
!
!  Check that upstream gages are located upstream of source gage and ungaged CP.
!
         If(INMETHOD(I).NE.10.and.NGAGE(I,1).GE.1.and.ICHECK.GE.1) Then
            Do 270 K=1,NGAGE(I,1)
               MD=IDSG(I)
               MU=IGAGE(I,K)
               KGA=0
               KUG=0
               NUMBERCP=0
               NCP=MU
240            NCP=CPNXT(NCP)
               If(NCP.GT.0) Then
                  NUMBERCP=NUMBERCP+1
                  If(K.GT.NGAGE(I,2)) KUG=1
                  If(CPID(NCP,1).EQ.CPID(MD,1)) KGA=KGA+1
                  If(K.LE.NGAGE(I,2).and.NGAGE(I,2).GT.0.and.
     +                               CPID(NCP,1).EQ.CPID(I,1)) KUG=KUG+1
                  If(KGA.EQ.0.and.NUMBERCP.LT.NCPTS) Goto 240
                  If(KUG.EQ.0.and.NGAGE(I,2).GT.0.and.
     +               NGAGE(I,2).LT.K.and.NUMBERCP.LT.NCPTS) Goto 240
               Endif
               If(KGA.EQ.0) Then
                  Write(14,*) ' '
                  Write(14,250) CPID(I,1),UGID(K),DSG
250               Format(' ERROR: On FD record for ',A6,' the upstream',
     +                   ' gage ',A6,/,8X,'is not upstream of the ',
     +                   'downstream gage site ',A6)
                  Write(14,120)
                  Call ERROR
               Endif
               If(KUG.EQ.0.and.NGAGE(I,2).GT.0) Then
                  Write(14,*) ' '
                  Write(14,260) CPID(I,1),UGID(K),CPID(I,1)
260               Format(' ERROR: On FD record for ',A6,
     +                   ' the upstream gage ',A6,/,8X,
     +                   'is not upstream of the ungaged site ',A6)
                  Write(14,120)
                  Call ERROR
               Endif
270         End Do
!
!  Check that the same upstream control point UGID(I) is not repeated twice.
!
            Do 300 K=1,NGAGE(I,1)
               Do 290 L=1,NGAGE(I,1)
                  If(L.EQ.K) Goto 290
                  If(UGID(K).EQ.UGID(L)) Then
                     Write(14,*)
                     Write(14,280) UGID(K),CPID(I,1)
280                  Format(' ERROR: Upstream control point UGID(I) of '
     +             ,A6,/,8X,'is repeated twice on FD record for CP ',A6)
                     Write(14,120)
                     Call ERROR
                  Endif
290            End Do
300         End Do
         Endif
!
!  Check that ungaged CP is downstream of gage CP for NGAGE(I,2) = -1.
!
         If(NGAGE(I,2).EQ.-1) Then
            NCP=IDSG(I)
            NUMBERCP=0
310         NCP=CPNXT(NCP)
            NUMBERCP=NUMBERCP+1
            If(NCP.LT.0.or.NUMBERCP.GT.NCPTS) Then
               Write(14,*) ' '
               Write(14,320) CPID(I,1),DSG
320            Format(' ERROR: NG is -1 on FD record for ',A6,
     +                ' but the source gage ',A6,/,8X,' is not',
     +                ' upstream of the ungaged control point.')
               Write(14,120)
               Call ERROR
            Endif
            If(CPID(NCP,1).NE.CPID(I,1)) Goto 310
         Endif
!
!  Checks on source control points for ungaged cps with INMETHOD=10.
!
         If(INMETHOD(I).EQ.10.and.ICHECK.EQ.1) Then
            If(IDSG(I).GT.0) Then
               If(INMETHOD(IDSG(I)).EQ.10.and.IDSG(I).GT.I) Then
                  Write(14,325) CPID(IDSG(I),1),CPID(I,1)
325               Format(' WARNING: Control point ',A6,' is not valid',
     +                   ' on FD record for control point ',A6)
               Endif
            Endif
            Do K=1,NGAGE(I,2)
               If(IGAGE(I,K).GT.I.or.(INMETHOD(IGAGE(I,K)).GE.3.and.
     +                                INMETHOD(IGAGE(I,K)).LE.8)) Then
                  Write(14,325) CPID(IGAGE(I,K),1),CPID(I,1)
               Endif
            End Do
         Endif
!
!  Upstream gage identifiers are reset to blank.
!
         Do K=1,NGAGE(I,1)
            UGID(K)='      '
         End Do
!
!  The next record is read.
!
         Goto 30
!
!  WP records are read.  Integer identifiers that number the control points
!  from the WP records are assigned based on the order of the CP records.
!
      Elseif(CD.EQ.'WP') Then
         NWP=NWP+1
         I=0
         Backspace(8)
         DAF2=DAF
         Read(8,330,IOSTAT=STATUS) CD,ID,DA1,CN1,MP1,DAF
330      Format(A2,A6,5F8.0)
         ID=Adjustr(ID)
         If(STATUS.NE.0) Call ERROR
         If(ICHECK.EQ.7) Write(14,340) CD,ID,DA1,CN1,MP1,DAF
340      Format(A2,A6,F8.1,2F8.2,F8.3,F8.5)
         If(DAF.LT.0.00001) DAF=DAF2
         If(DAF.NE.1.0) DA1=DA1*DAF
350      I=I+1
         If(ID.EQ.CPID(I,1)) Then
            DA(I)=DA1
            CN(I)=CN1
            MP(I)=MP1
            If(ICHECK.EQ.1) Then
               If(DA(I).EQ.0.0) Write(14,360) Adjustl(CPID(I,1))
360            Format(' WARNING: The drainage area is zero for ',
     +                'control point ',A6)
               If(DA(I).LT.0.0) Write(14,362) CPID(I,1),DA(I)
362            Format(' WARNING: The drainage area for CP ',
     +                  A6,' is negative:',F8.2)
               If((CN(I).LT.CNLB.or.CN(I).GT.CNUB))
     +         Write(14,364) CPID(I,1),CN(I),CNLB,CNUB
364            Format(' WARNING: The CN for CP ',A6,' of',F8.2,
     +                ' violates the CN bounds:',F6.1,' to',F6.1)
               If((MP(I).LT.MPLB.or.MP(I).GT.MPUB))
     +         Write(14,366) CPID(I,1),MP(I),MPLB,MPUB
366            Format(' WARNING: The MP for CP ',A6,' of',F8.2,
     +                ' violates the MP bounds:',F6.1,' to',F6.1)
            Endif
            Goto 30
         Endif
         If(I.EQ.NCPTS) Then
            Write(14,*) ' '
            Write(14,370) ID,NWP
370         Format(' ERROR: ',A6,' on ',I4,'WP record matches no '
     +             'control point identifier on CP records.')
            Write(14,120)
            Call ERROR
         Endif
         Goto 350
      Elseif(CD.EQ.'ED') Then
         If(ICHECK.GE.0) Write(14,380) NFD,NWP
380      Format(' *** Finished reading',I5,' FD and',I5,' WP records.')
         Goto 390
      Else
         Write(14,*)' '
         Write(14,110) CD
         Write(14,120)
         Call ERROR
      Endif
!
!  Determine parameters for incremental watershed areas.
!
390   NP=0
      Do 550 I=1,NCPTS
         If(INMETHOD(I).EQ.10) Goto 550
         N=IDSG(I)
         If(INMETHOD(I).GE.4.or.EWA(I).LT.-0.001) Then
            NP=NP+1
!
!  Error checks.
!
            If(N.LE.0) Then
               Write(14,*) ' '
               Write(14,400) CPID(I,1)
400            Format(' ERROR: The downstream gaged source control ',
     +                'point associated with ungaged CP ',A6,/,8X,
     +                'is missing or not specified on a FD record.')
               Write(14,120)
               Call ERROR
            Endif
            If(DA(N).LE.0) Then
               Write(14,*) ' '
               Write(14,410) CPID(N,1),DA(N)
410            Format(' ERROR: The drainage area for CP ',
     +                 A6,' is missing, zero, or negative:',F8.2)
               Write(14,120)
               Call ERROR
            Endif
            If(DA(I).LE.0) Then
               Write(14,*) ' '
               Write(14,410) CPID(I,1),DA(I)
               Write(14,120)
               Call ERROR
            Endif
!
!  If the parameters on WP records are already for incremental watersheds
!  then incrementals do not have to be computed.
!
            If(INWS(I).GT.0) Then
               DAGAGE(I)=DA(N)
               CNGAGE(I)=CN(N)
               MPGAGE(I)=MP(N)
               DAUG(I)=DA(I)
               CNUG(I)=CN(I)
               MPUG(I)=MP(I)
               Goto 550
            Endif
!
!  Incremental drainage area above gaged control point.
!
            DAGAGE(I)=DA(IDSG(I))
            If(NGAGE(I,1).GE.1) Then
               Do 420 K=1,NGAGE(I,1)
                  DAG(I,K)=DA(IGAGE(I,K))
                  DAGAGE(I)=DAGAGE(I)-DAG(I,K)
420            End Do
            Endif
            If(DAGAGE(I).LE.0.0) Then
               Write(14,*) ' '
               Write(14,430) CPID(I,1),DAGAGE(I)
430            Format(' ERROR: The incremental drainage area for CP ',
     +                 A6,' is zero or negative:',F8.2)
               Write(14,120)
               Call ERROR
            Endif
!
!  Incremental drainage area above ungaged control point.
!
            If(NGAGE(I,2).EQ.-1) Then
               DAUG(I)=DA(I)-DA(IDSG(I))
            Elseif(NGAGE(I,2).EQ.0) Then
               DAUG(I)=DA(I)
            Elseif(NGAGE(I,2).GE.1) Then
               DAUG(I)=DA(I)
               Do 440 K=1,NGAGE(I,1)
                  If(K.LE.NGAGE(I,2)) Then
                     DAUGC(I,K)=DA(IGAGE(I,K))
                     DAUG(I)=DAUG(I)-DAUGC(I,K)
                  Else
                     DAUGC(I,K)=0.
                  Endif
                  If(DAUG(I).LE.0.0) Then
                     Write(14,*) ' '
                     Write(14,430) CPID(I,1),DAUG(I)
                     Write(14,120)
                     Call ERROR
                  Endif
440            End Do
            Endif
         Endif
!
!  Incremental CN and mean precipitation for gaged control point,
!  for use in methods based on NRCS curve number equation.
!
         If(INMETHOD(I).EQ.4.or.INMETHOD(I).EQ.5.or.INMETHOD(I).EQ.8)
     +      Then
            SUMCNDA=0.
            SUMMPDA=0.
            If(NGAGE(I,1).EQ.0) Then
               CNGAGE(I)=CN(IDSG(I))
               MPGAGE(I)=MP(IDSG(I))
            Else
               Do 450 K=1,NGAGE(I,1)
                  SUMCNDA=SUMCNDA+DAG(I,K)*CN(IGAGE(I,K))
                  SUMMPDA=SUMMPDA+DAG(I,K)*MP(IGAGE(I,K))
450            End Do
               CNGAGE(I)=(CN(IDSG(I))*DA(IDSG(I))-SUMCNDA)/DAGAGE(I)
               MPGAGE(I)=(MP(IDSG(I))*DA(IDSG(I))-SUMMPDA)/DAGAGE(I)
            Endif
!
!  Incremental CN and mean precipitation for ungaged control point.
!
            If(NGAGE(I,2).EQ.-1) Then
               CNUG(I)=(CN(I)*DA(I)-CN(IDSG(I))*DA(IDSG(I)))/DAUG(I)
               MPUG(I)=(MP(I)*DA(I)-MP(IDSG(I))*DA(IDSG(I)))/DAUG(I)
            Elseif(NGAGE(I,2).EQ.0) Then
               CNUG(I)=CN(I)
               MPUG(I)=MP(I)
            Elseif(NGAGE(I,2).GE.1) Then
               SUMCNDA=0.
               SUMMPDA=0.
               Do 460 K=1,NGAGE(I,2)
                  SUMCNDA=SUMCNDA+DAUGC(I,K)*CN(IGAGE(I,K))
                  SUMMPDA=SUMMPDA+DAUGC(I,K)*MP(IGAGE(I,K))
460            End Do
               CNUG(I)=(CN(I)*DA(I)-SUMCNDA)/DAUG(I)
               MPUG(I)=(MP(I)*DA(I)-SUMMPDA)/DAUG(I)
            Endif
!
!  Warning messages for CN that are less than CNLB or greater than CNUB.
!
            If(ICHECK.NE.1) Goto 550
            If(CNGAGE(I).LT.CNLB) Then
               Write(14,470) CNLB,CPID(IDSG(I),1),CNGAGE(I)
470            Format(' WARNING: Lower CN bound of',F5.1,' exceeded at',
     +                ' gaged CP ',A6,', incremental CN = ',F7.2)
            Endif
            If(CNGAGE(I).GT.CNUB) Then
               Write(14,480) CNUB,CPID(IDSG(I),1),CNGAGE(I)
480            Format(' WARNING: Upper CN bound of',F6.1,' exceeded at',
     +                ' gaged CP ',A6,', incremental CN = ',F7.2)
            Endif
            If(CNUG(I).LT.CNLB) Then
               Write(14,490) CNLB,CPID(I,1),CNUG(I)
490            Format(' WARNING: Lower CN bound of',F5.1,' exceeded at',
     +                ' ungaged CP ',A6,', incremental CN = ',F7.2)
            Endif
            If(CNUG(I).GT.CNUB) Then
               Write(14,500) CNUB,CPID(I,1),CNUG(I)
500            Format(' WARNING: Upper CN bound of',F6.1,' exceeded at',
     +                ' ungaged CP ',A6,', incremental CN = ',F7.2)
            Endif
!
!  Warning messages for mean precipitation MP that violates MPLB or MPUB.
!
            If(MPGAGE(I).LT.MPLB) Then
               Write(14,510) MPLB,CPID(IDSG(I),1),MPGAGE(I)
510            Format(' WARNING: Lower MP bound of',F5.1,' exceeded at',
     +                ' gaged CP ',A6,', incremental MP = ',F7.2)
            Endif
            If(MPGAGE(I).GT.MPUB) Then
               Write(14,520) MPUB,CPID(IDSG(I),1),MPGAGE(I)
520            Format(' WARNING: Upper MP bound of',F6.1,' exceeded at',
     +                ' gaged CP ',A6,', incremental MP = ',F7.2)
            Endif
            If(MPUG(I).LT.MPLB) Then
               Write(14,530) MPLB,CPID(I,1),MPUG(I)
530            Format(' WARNING: Lower MP bound of',F5.1,' exceeded at',
     +                ' ungaged CP ',A6,', incremental MP = ',F7.2)
            Endif
            If(MPUG(I).GT.MPUB) Then
               Write(14,540) MPUB,CPID(I,1),MPUG(I)
540            Format(' WARNING: Upper MP bound of',F6.1,' exceeded at',
     +                ' ungaged CP ',A6,', incremental MP = ',F7.2)
            Endif
         Endif
!
!  End of do-loop determining parameters for incremental watershed areas.
!
550   End Do
!
!  End of Subroutine IACNP
!
      Deallocate(CN,DA,MP)
      Deallocate(DAG,DAUGC)
!
      Return
600   Write(14,*)' '
      Write(14,*) ' ERROR: Reached end of DIS file without reading',
     +            ' ED record.'
      Write(14,120)
      Call ERROR
      End Subroutine IACNP
!
! **************************************************************************
!
      Subroutine FLDIST
!
!  Subroutine FLDIST distributes sequences of flows from gaged (known-flow)
!  control points to ungaged (unknown-flow) control points.
!  FLDIST also adjusts evaporation-precipitation depths from EV records to
!  remove the runoff from the land area covered by a reservoir.
!
      Use COMVAR
!
      Integer I,IC,II,J,KCOUNT,N
!
      Real DAR,DELF,QADJ,QQSUM,QUGADJ,RUNOFF,STOP,SUMQ
      Real DF(MAXGAG)
!
      Real,Allocatable,Dimension(:)::QQ
      Real,Allocatable,Dimension(:,:)::QGAGE,QTOTAL,QUG
!
      Allocate(QGAGE(NCPTS,12),QQ(MAXGAG),
     +         QTOTAL(NCPTS,12),QUG(NCPTS,12))
!
!  Incremental flows are computed at each ungaged control point for each month
!  within the DO loops ending with statements 110 and 120.
!
      QGAGE=0.0
      QUG=0.0
      QTOTAL=0.0
      Do 120 MT=1,NPRDS
         Do 110 I=1,NCPTS
            If(INMETHOD(I).EQ.9) Goto 110
!
!  Compute flows using equation for INMETHOD(I)=10.
!
            If(INMETHOD(I).EQ.10) Then
               QTOTAL(I,MT)=0.0
               If(IDSG(I).GT.0) QTOTAL(I,MT)=INFLOW(IDSG(I),MT)
               If(NGAGE(I,2).GT.0) Then
                  Do J=1,NGAGE(I,2)
                     QTOTAL(I,MT)=QTOTAL(I,MT)+
     +                            COEF(I,J)*INFLOW(IGAGE(I,J),MT)
                  End Do
               Endif
               INFLOW(I,MT)=QTOTAL(I,MT)*CPDT(I,1)
               Goto 110
            Endif
!
!  Compute incremental flow at gaged control point.
!
            If(INMETHOD(I).GE.3.or.EWA(I).LT.-0.001) Then
               QQSUM=0.0
               If(NGAGE(I,1).GE.1) Then
                  Do 40 J=1,NGAGE(I,1)
                     NPT=IGAGE(I,J)
                     DF(J)=1.0
30                   DF(J)=DF(J)*(1.0-CL(NPT))
                     NPT=CPNXT(NPT)
                     If(NPT.NE.IDSG(I)) Goto 30
                     ICP(J)=IGAGE(I,J)
                     If(ICP(J).GT.0) Then
                        QQ(J)=INFLOW(ICP(J),MT)*DF(J)
                        QQSUM=QQSUM+QQ(J)
                     Endif
40                End Do
               Endif
               QGAGE(I,MT)=INFLOW(IDSG(I),MT)-QQSUM
!
!  Only allow negative incremental flows if using DAR method combined
!  with channel losses (INMETHOD(I)=6).  However, INMETHOD 8 may revert
!  to INMETHOD 6 under certain conditions.
!
               If(QGAGE(I,MT).LT.0.0.and.INMETHOD(I).NE.6.
     +            and.INMETHOD(I).NE.8) Then
                  QGAGE(I,MT)=0.0
               Endif
            Endif
!
!  Compute incremental flows at the ungaged control point using the
!  flow distribution equation for INMETHOD(I)=3.
!
            If(INMETHOD(I).EQ.3) Then
               QUG(I,MT)=COEF(I,1)*QGAGE(I,MT)**COEF(I,2)+COEF(I,3)
            Endif
!
!  Error check for zero drainage area.
!
            If(INMETHOD(I).GE.4) Then
               If(DAGAGE(I).LE.0.0.or.DAUG(I).LE.0.0) Then
                  Write(14,50) CPID(I,1)
50                Format(' ERROR: Negative or zero gaged or ungaged '
     +                'drainage area when distributing flows to CP ',A6)
                  Write(14,80)
                  Call ERROR
               Endif
            Endif
!
!  Compute incremental flows at the ungaged control point using the
!  drainage area ratio for INMETHOD(I)=7.
!
            If(INMETHOD(I).EQ.7) Then
               DAR=(DAUG(I)/DAGAGE(I))
               QUG(I,MT)=QGAGE(I,MT)*DAR
            Endif
!
!  Compute incremental flows at the ungaged control point using the
!  modified NRCS CN method for INMETHOD(I) = 4 or 5.
!
            If(INMETHOD(I).EQ.4.or.INMETHOD(I).EQ.5) Then
!
!      The drainage area method is used if either CN or MP are out of bounds,
!      or if either CN or MP is zero, or if both CN and MP are same for gaged 
!      and ungaged watershed.  Otherwise, CN method is used by calling CURNUM.
!
               If(QGAGE(I,MT).GE.0.001) Then
!
!           Conditions for applying the drainage area ratio method.
!
                  If(CNGAGE(I).LE.0.1.or.CNUG(I).LE.0.1.or.
     +             (CNGAGE(I).EQ.CNUG(I).and.MPGAGE(I).EQ.MPUG(I))) Then
                     QUG(I,MT)=QGAGE(I,MT)*(DAUG(I)/DAGAGE(I))
                  Elseif(CNGAGE(I).LT.CNLB.or.CNGAGE(I).GT.CNUB.or.
     +                     CNUG(I).LT.CNLB.or.  CNUG(I).GT.CNUB.or.
     +                   MPGAGE(I).LT.MPLB.or.MPGAGE(I).GT.MPUB.or.
     +                     MPUG(I).LT.MPLB.or.  MPUG(I).GT.MPUB)  Then
                     QUG(I,MT)=QGAGE(I,MT)*DAUG(I)/DAGAGE(I)
                  Else
!
!           Otherwise, the CN method is applied by calling Subroutine CURNUM.
!                 *+*+*+*+*  Call Subroutine CURNUM  *+*+*+*+* 
!
                     Call CURNUM(DAGAGE(I),CNGAGE(I),MPGAGE(I),
     +                    DAUG(I),CNUG(I),MPUG(I),QGAGE(I,MT),QUG(I,MT))
                  Endif
!
               Else
                  QUG(I,MT)=0.0
               Endif
            Endif
!
!  For INMETHOD(I) = 6 or 8, channel loss factors for stream reaches between
!  gaged and ungaged control points are combined into a delivery factor DELF.
!
            If(INMETHOD(I).EQ.6.or.INMETHOD(I).EQ.8) Then
               DELF=1.0
               NPT=I
60             DELF=DELF*(1.0-CL(NPT))
               NPT=CPNXT(NPT)
               If(NPT.LE.0) Then
                  Write(14,*) ' '
                  Write(14,70) CPID(IDSG(I),1),CPID(I,1),INMETHOD(I)
70                Format(' ERROR: Gaged CP ',A6,' is not downstream of'
     +               ,' ungaged CP ',A6,' as required for INMETHOD',I2)
                  Write(14,80)
80                Format('        Stopped from ',
     +                    'Subroutine FLDIST due to error.')
                  Call ERROR
               Endif
               If(NPT.NE.IDSG(I)) Goto 60
            Endif
!
!  Compute incremental flows at the ungaged control point using the combined
!  channel loss and drainage area ratio approach INMETHOD(I)=6.
!
            If(INMETHOD(I).EQ.6) Then
               DAR=DAUG(I)/DAGAGE(I)
               QUG(I,MT)=QGAGE(I,MT)*DAR/(1.0-DAR*(1.0-DELF))
            Endif
!
!  Compute incremental flows at the ungaged control point using the modified
!  NRCS CN method combined with channel losses for INMETHOD = 8
!  *+*+*+*+*  Call Subroutine CURNUM  *+*+*+*+*
!
            If(INMETHOD(I).EQ.8) Then
               If(QGAGE(I,MT).GE.0.001) Then
!
!      The drainage area method is used if either CN or MP are out of bounds,
!      or if either CN or MP is zero, or if both CN and MP are same for gaged 
!      and ungaged watershed.  Otherwise, CN method is used by calling CURNUM.
!
                  If(CNGAGE(I).LT.CNLB.or.CNGAGE(I).GT.CNUB.or.
     +                 CNUG(I).LT.CNLB.or.  CNUG(I).GT.CNUB.or.
     +               MPGAGE(I).LT.MPLB.or.MPGAGE(I).GT.MPUB.or.
     +                 MPUG(I).LT.MPLB.or.  MPUG(I).GT.MPUB)  Then
                     DAR=DAUG(I)/DAGAGE(I)
                     QUG(I,MT)=QGAGE(I,MT)*DAR/(1.0-DAR*(1.0-DELF))
                  Elseif(CNGAGE(I).LE.0.1.or.CNUG(I).LE.0.1.or.
     +             (CNGAGE(I).EQ.CNUG(I).and.MPGAGE(I).EQ.MPUG(I))) Then
                     DAR=DAUG(I)/DAGAGE(I)
                     QUG(I,MT)=QGAGE(I,MT)*DAR/(1.0-DAR*(1.0-DELF))
                  Else
                     QUGADJ=0.0
                     QADJ=QGAGE(I,MT)
                     IC=0
90                   IC=IC+1
                     Call CURNUM(DAGAGE(I),CNGAGE(I),MPGAGE(I),DAUG(I),
     +                           CNUG(I),MPUG(I),QADJ,QUGADJ)
                     STOP=Abs(1.-(QADJ/(QGAGE(I,MT)+QUGADJ*(1.-DELF))))
                     QADJ=QGAGE(I,MT)+QUGADJ*(1.-DELF)
                     If(STOP.GT.0.005.and.IC.LT.100) Goto 90
                     QUG(I,MT)=QUGADJ
                     If(IC.GE.100) Then
                        Write(14,100) CPID(I,2),YEAR,MT,QUG(I,MT)
100                     Format(' WARNING: Convergence criterion of 0.5',
     +                  '% was not met for flow distribution option 8 ',
     +                  'after 100',/,10x,'iterations at ungaged CP ',
     +                  A6,' for year',I5,', month',I3,/,10x,' Last ',
     +                  'flow computed of',F8.0,' was adopted.')
                     Endif
                  Endif
               Else
                  DAR=DAUG(I)/DAGAGE(I)
                  QUG(I,MT)=QGAGE(I,MT)*DAR/(1.0-DAR*(1.0-DELF))
               Endif
            Endif
110      End Do
120   End Do
!
!  Total cumulative flows for the ungaged control points are determined.
!
      Do 210 I=1,NCPTS
         If(INMETHOD(I).GE.9) Goto 210
         If(NGAGE(I,2).GE.1) Then
            Do 140 J=1,NGAGE(I,2)
               NPT=IGAGE(I,J)
               DF(J)=1.0
130            DF(J)=DF(J)*(1.0-CL(NPT))
               NPT=CPNXT(NPT)
               If(NPT.NE.I) Goto 130
140         End Do
            Do 160 MT=1,12
               SUMQ=0.0
               Do 150 N=1,NGAGE(I,2)
                  IQ=IGAGE(I,N)
                  SUMQ=SUMQ+INFLOW(IQ,MT)*DF(N)
150            End Do
               QTOTAL(I,MT)=QUG(I,MT)+SUMQ
               If(QTOTAL(I,MT).LT.0.) QTOTAL(I,MT)=0.0
160         End Do
         Elseif(NGAGE(I,2).EQ.-1) Then
            NPT=IDSG(I)
            DF(1)=1.0
170         DF(1)=DF(1)*(1.0-CL(NPT))
            NPT=CPNXT(NPT)
            If(NPT.NE.I) Goto 170
            II=IDSG(I)
            Do 180 MT=1,12
               QTOTAL(I,MT)=QUG(I,MT)+INFLOW(II,MT)*DF(1)
               If(QTOTAL(I,MT).LT.0.) QTOTAL(I,MT)=0.0
180         End Do
         Elseif(NGAGE(I,2).EQ.0) Then
            Do 190 MT=1,12
               QTOTAL(I,MT)=QUG(I,MT)
               If(QTOTAL(I,MT).LT.0.) QTOTAL(I,MT)=0.0
190         End Do
         Endif
         If(INMETHOD(I).EQ.4) Then
            Do 200 MT=1,12
               II=IDSG(I)
               If(QTOTAL(I,MT).GT.INFLOW(II,MT))
     +                       QTOTAL(I,MT)=INFLOW(II,MT)
200         End Do
         Endif
210   End Do
!
!  Determine final flows. Multiply by CPDT(cp,1) from CP record.
!
      Do 220 I=1,NCPTS
         If(INMETHOD(I).GE.3.and.INMETHOD(I).LE.8) Then
            Do MT=1,NPRDS
               INFLOW(I,MT)=QTOTAL(I,MT)*CPDT(I,1)
            End Do
         Endif
220   End Do
!
!  End of flow distribution computations.
!  Trace message only for first year.
!
      If(ICHECK.GE.0.and.YEAR.EQ.YRST) Write(14,*) '*** Flow ',
     +         'distribution was performed for the first year.'
!
!  Net reservoir evaporation-precipitation rates are adjusted for the runoff
!  from the land area now covered by a reservoir.
!
      KCOUNT=0
      Do 290 J=1,NCPTS
         If(CPEV(J).EQ.'  ZERO') Goto 290
         If(EWA(J).LT.-0.001.and.EWA(J).GT.-1.5) Then
            KCOUNT=KCOUNT+1
            If(DAUG(J).LE.0.0) Then
               Write(14,*) ' '
               Write(14,240) CPID(J,1),EWA(J),DAUG(J)
240            Format(' ERROR: Evap-precip adjustment at control ',
     +         'point ',A6,' specified by EWA(cp) of',F3.0,/,8X,
     +         'Drainage area of ',F8.2,' is not acceptable.')
               Write(14,80)
               Call ERROR
            Endif
            Do 270 MT=1,NPRDS
               RUNOFF=QUG(J,MT)/(DAUG(J)*640)
               EVAPR(J,MT)=EVAPR(J,MT)+RUNOFF
               If(RUNOFF.GT.EPWL.and.ICHECK.EQ.1.and.IRO.LE.2000) Then
                  Write(14,250) CPID(J,1),RUNOFF,YEAR,MT,EWA(J),EPWL
250               Format(' WARNING: EP-adjustment depth at CP ',A6,
     +             ' is',F6.3,' for year',I5,', month',I2,', EWA=',F3.0,
     +                /,10x,'Message printed if runoff depth adjustment'
     +                ,' exceeds warning limit of',F4.1)
                  IRO=IRO+1
                  If(IRO.GT.2000) Write(14,260)
260               Format(' WARNING: The EP-adjustment warning message',
     +                   ' has been printed above 2,000 times.',/,10x,
     +                   'Since the 2,000 limit has been reached, the',
     +                   ' message will no longer be printed.')
               Endif
270         End Do
         Elseif(EWA(J).LT.-1.5.and.EWA(J).GT.-2.5) Then
            KCOUNT=KCOUNT+1
            If(DAGAGE(J).LE.0.0) Then
               Write(14,*) ' '
               Write(14,240) CPID(I,1),EWA(J),DAGAGE(J)
               Write(14,80) 
               Call ERROR
            Endif
            Do 280 MT=1,NPRDS
               RUNOFF=QGAGE(J,MT)/(DAGAGE(J)*640)
               EVAPR(J,MT)=EVAPR(J,MT)+RUNOFF
               If(RUNOFF.GT.EPWL.and.ICHECK.EQ.1.and.IRO.LE.2000) Then
                  Write(14,250) CPID(J,1),RUNOFF,YEAR,MT,EWA(J),EPWL
                  IRO=IRO+1
                  If(IRO.GT.2000) Write(14,260)
               Endif
280         End Do
         Endif
290   End Do
!
!  End of EV record net evap-precip depth adjustment computations.
!  Trace message only for first year.
!
      If(ICHECK.GE.0.and.YEAR.EQ.YRST.and.KCOUNT.GT.0) Then
         Write(14,300) KCOUNT
300      Format(' *** Reservoir evaporation-precipitation depths were ',
     +          'adjusted to reflect site runoff',/,5x,'for the first ',
     +          'year at',I4,' control points from FD/WP record data.')
      Endif
!
!  End of Subroutine FLDIST
!
      Deallocate(QGAGE,QQ,QTOTAL,QUG)
      Return
      End Subroutine FLDIST
!
! *********************************************************************
!
      Subroutine CURNUM (DA1,CN1,MP1,DA2,CN2,MP2,Q1,Q2)
!
!  Subroutine CurNum transfers flows using the modified NRCS CN method.
!  Q2 for the ungaged site is computed, given Q1 for the gaged site.
!
      Use COMVAR
!
      Real Q1,Q2,CN1,CN2,DA1,DA2,MP1,MP2,P,S1,S2
!
!  Calculate PR.
!
      S1=(1000/CN1)-10
      S2=(1000/CN2)-10
      Q1=DEPTHX*Q1/DA1
      Call BISECT(S1,Q1,P)
      If(MP1.GT.0.0.and.MP2.GT.0.0) P=P*(MP2/MP1)
!
!  Calculate Q2.
!
      Q2=(((P-(.2*S2))**2)/(P+.8*S2))
      If(P.LE.0.2*S2) Q2=0.0
      Q2=(1.0/DEPTHX)*Q2*DA2
      Q1=(1.0/DEPTHX)*Q1*DA1
!
      Return
      End Subroutine CURNUM
!
! **************************************************************************
!
      Subroutine BISECT(S1,Q,XR)
!
!  Subroutine BISECT solves the NRCS CN equation for P given Q
!  using the iterative bisection method.
!
      Real XL,XU,ES,XR,EA,S1,Q,TEST,FXL,FXR
      Integer MAXIT,ITER
      ES=.0001
      EA=1.1*ES
      XL=0.2*S1
      If(XL.EQ.0.0) XL=0.0001
      XU=30.0
      MAXIT=101
      ITER=0
10    If((EA.GT.ES).and.(ITER.LT.MAXIT)) Then
         XR=(XL+XU)/2.
         ITER=ITER+1
         If(ITER.EQ.MAXIT)Then
            Write(14,*)' WARNING: Subroutine BISECT stopped at 100 ',
     +            'iterations in solving the NRCS CN equation for P.'
          Write(14,*) ' '
            Goto 20
         Endif
         If((XL+XU).NE.0.) Then
            EA=Abs((XU-XL)/(XL+XU))*100.
         Endif
         FXL=(((XL-(.2*S1))**2)/(XL+(.8*S1)))-Q
         FXR=(((XR-(.2*S1))**2)/(XR+(.8*S1)))-Q
         TEST=FXL*FXR
         If(TEST.EQ.0.) Then
            EA=0.
         Else
            If(TEST.LT.0.) Then
               XU=XR
            Else
               XL=XR
            Endif
         Endif
         Goto 10
      Endif
20    If(XR.LE.0.0001) XR=0.0
      Return
      End Subroutine BISECT
!
! **************************************************************************
!
      Subroutine READRUF
!
!  Subroutine READRUF reads RU record adjustments from a RUF file used to
!  convert unappropriated flows to regulated flows in a condensed dataset.
!  This feature is activated by RUFIN or RUF from the JO record.
!
      Use COMVAR
      Integer CP,I,J,K,M,FYR,LYR
      Character(len=6) IDCP
!
      Write(14,10)
10    Format(' *** Starting to read RU records from RUF file.')
!
      Allocate(RUFA(MAXCP,NYRS,12))
      RUFA=0.0
!
      Do 110 J=1,NCPTS
         YEAR=YRST-1
         Do 100 K=1,NYRS
            YEAR=YEAR+1
!
!  Read and check CD which should be either **, RU, or ED.
!
20          Read(18,30,IOSTAT=STATUS,End=140) CD,IDCP
30          Format(A2,A6)
            If(STATUS.NE.0) Then
               Write(14,40)
40             Format(' ERROR: Fortran IOSTAT error reading the RUF',
     +                ' file.')
               Call ERROR
            Endif
            If(CD.EQ.'**') Goto 20
            If(CD.EQ.'ED') Goto 120
            If(CD.NE.'RU') Then
               Write(14,50) CD
50             Format(' ERROR: Read CD of ',A2,
     +                ' from RUF file instead of RU.')
               Call ERROR
            Endif
!
!  The integer control point identifier is assigned.
!
            I=0
60          I=I+1
            If(IDCP.EQ.CPID(I,1)) Then
               CP=I
            Elseif(I.LT.NCPTS) Then
               Goto 60
            Endif
            If(I.GT.NCPTS) Then
               Write(14,70) IDCP
70             Format(' Control point identifier ',A6,
     +                ' on RU record is not on a CP record.')
               Call Error
            Endif
!
!  The flow adjustments are read from the RUF file.
!
            Backspace(18)
            Read(18,80,IOSTAT=STATUS) CD,IDCP,FYR,LYR,
     +                                (RUFA(CP,K,M),M=1,12)
80          Format(A2,A6,2I4,12F8.0)
            If(STATUS.NE.0) Then
               Write(14,40)
               Call ERROR
            Endif
            If(ICHECK.EQ.11) Write(14,80) CD,IDCP,FYR,LYR,
     +                                    (RUFA(CP,K,M),M=1,12)
            If(FYR.LE.0) FYR=LYR
            If(FYR.LT.YRST) Goto 20
            If(LYR.GT.YEAR) Backspace(18)
            If(FYR.GT.YEAR.or.FYR.GT.LYR) Then
               Write(14,90) IDCP,YEAR,FYR
90             Format(' ERROR: In reading RUF file for CP '
     +               ,A6,' for year',I5,' read FYR of',I5)
               Call ERROR
            Endif
100      End Do
!
!  End of control point loop.
!
110   End Do
!
120   Write(14,130)
130   Format(' *** Finished reading RU records from RUF file.')
      Return
140   Write(14,150)
150   Format(' *** Reached end of RUF file without reading ED record.')
      Return
      End Subroutine READRUF
!
! **************************************************************************
!
      Subroutine ZZFLOW
!
!  Subroutine ZZFLOW develops a table of reservoir releases, regulated flows,
!  and available flows as specified by the ZZ record.  The flows are written
!  to the ZZZ file during each month of the simulation at the beginning of
!  the priority loop and after each water right is considered in the loop.
!
      Use COMVAR
      Integer I,J,M,Z,ZZFLAG
!
      ZZF=0.0
      M=(YEAR-YRST)*12+MT
!
!  Headings for ZZZ file table.
!
      If(ZZCALL.EQ.0) Then
         ZZCALL=99
         Write(17,10)
10       Format('REGULATED AND AVAILABLE STREAMFLOWS COMPUTED IN WATER',
     +          ' RIGHTS',/,'PRIORITY SEQUENCE AT CONTROL POINTS',
     +          ' SPECIFIED BY ZZ RECORD',/)
         J=NWRTS
         If(ZZWR.NE.'                ') Then
            Do I=1,NWRTS
               If(WRID(RANK(I)).EQ.ZZWR) Then
                  J=I
                  Goto 20
               Endif
            End Do
         Endif
20       Write(17,30) YRST,NYRS
30       Format('First year and number of years:',I6,I4)
         Write(17,40) J,ZZ
40       Format('Number of water rights and control points:',I5,I4,/)
         Write(17,50)
50       Format(34('-'),<ZZ>(30('-')))
         Write(17,60) (CPID(ZZI(Z),1),Z=1,ZZ)
60       Format('Control Point',21x,
     +          <ZZ>('|---------  ',A6,'   ---------'),/,
     +          34x,<ZZ>('|Reservoir Regulated Available'))
         Write(17,70)
70       Format('Year M  Water Right        M   WR ',
     +           <ZZ>('| Releases    Flow      Flow  '))
         Write(17,80)
80       Format(34('-'),<ZZ>(30('-')),/)
      Endif
!
!  Flows are written initially each month before the water rights sequence.
!
      If(ZZR.EQ.0) Then
         Do Z=1,ZZ
            ZZF(Z,2)=CPFLOW(ZZI(Z),MT,2)
            If(ADJINC.EQ.4) Then
               ZZF(Z,2)=ZZF(Z,2)-CPFLOW(ZZI(Z),MT,1)
               If(ZZF(Z,2).LT.0.0) ZZF(Z,2)=0.0
            Endif
            LOCNUM=ZZI(Z)
            Call AVALB
            ZZF(Z,3)=AVAMT
         End Do
         If(Abs(IFPASS).EQ.2) Then
            Write(17,90) YEAR,MT,M,ZZR,(ZZF(Z,2),ZZF(Z,3),Z=1,ZZ)
90          Format(I4,I2,2x,'*** Beginning 2P',I4,I5,<ZZ>(10x,2F10.1))
         Else
            Write(17,100) YEAR,MT,M,ZZR,(ZZF(Z,2),ZZF(Z,3),Z=1,ZZ)
100         Format(I4,I2,2x,'*** Beginning **',I4,I5,<ZZ>(10x,2F10.1))
         Endif
      Else
!
!  The array ZZF contains reservoir releases, regulated flows, and available
!  flows computed after a water right is simulated in the priority sequence.
!  ZZF array includes each of ZZ control points specified on the ZZ record.
!
         ZZFLAG=0
         Do Z=1,ZZ
            ZZF(Z,1)=RESREL(ZZI(Z))
            ZZF(Z,2)=CPFLOW(ZZI(Z),MT,2)+RESREL(ZZI(Z))
            If(ADJINC.EQ.4) Then
               ZZF(Z,2)=ZZF(Z,2)-CPFLOW(ZZI(Z),MT,1)
               If(ZZF(Z,2).LT.0.0) ZZF(Z,2)=0.0
            Endif
            LOCNUM=ZZI(Z)
            Call AVALB
            ZZF(Z,3)=AVAMT
!
!  If ZZX from the ZZ record is greater than zero, only rights causing
!  flow changes are included in the ZZZ file output.
!
            If(ZZX.GT.0.0) Then
               If(Abs(ZZF(Z,1)-ZZFX(Z,1)).GE.ZZX) ZZFLAG=ZZFLAG+1
               If(Abs(ZZF(Z,2)-ZZFX(Z,2)).GE.ZZX) ZZFLAG=ZZFLAG+1
               If(Abs(ZZF(Z,3)-ZZFX(Z,3)).GE.ZZX) ZZFLAG=ZZFLAG+1
            Else
               ZZFLAG=99
            Endif
         End Do
!
!  The flows are recorded as an output record in the ZZZ file.
!
         If(ZZFLAG.GT.0) Then
!
            Write(17,110) YEAR,MT,Adjustl(WRID(WR)),M,ZZR,
     +                   (ZZF(Z,1),ZZF(Z,2),ZZF(Z,3),Z=1,ZZ)
110         Format(I4,I2,2x,A16,I4,I5,<ZZ>(3F10.1))
         Endif
      Endif
!
!  Since with nonzero ZZX only rights causing flow changes are included in
!  the output, array ZZFX stores flows for comparison with flows after the
!  next water right to determine whether changes occur.
!
      If(ZZX.GT.0.0) Then
         Do Z=1,ZZ
            ZZFX(Z,1)=ZZF(Z,1)
            ZZFX(Z,2)=ZZF(Z,2)
            ZZFX(Z,3)=ZZF(Z,3)
         End Do
      Endif
!
!  End of Subroutine ZZFLOW.
!
      Return
      End Subroutine ZZFLOW
!
! **************************************************************************
!                          End of Program WRAP-SIM
! **************************************************************************
