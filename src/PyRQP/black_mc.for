*    Fortran subroutine for Mass Balance Calculations - MONTE CARLO -----------
*    Called as a Dynamic Link Library (DLL) from Visual Basic interface -------
*    Used for Monte Carlo (MC); Non-Parametric (NP); Intermittent (IN) --------
*    Modified by Tony Warn - January 2017 -------------------------------------
*    **************************************************************************
       
      subroutine MONTE(vrfm,vrf5,vrcm,vrcs,vrcns,vrcml,vrcmu,
     &vrcsl,vrcsu,vrcq,vrcql,vrcqu,
     &vefm,vefs,vecm,vecs,vecns,vecml,vecmu,
     &vecsl,vecsu,vec95,vec95l,
     &vec95u,vec99,vec99l,vec99u,vec995,vec995l,vec995u,
     &vforw,vtarg,vxper,vtype,
     &vcofc1,vcoff2,vcofc3,vcocf4,vcofc5,vcocc6,
     &vsens,vmsg1,vmsg2,vmsg3, 
     &vtcm,vtcs,vtcns,vtcml,vtcmu,vtcsl,vtcsu,
     &vtcx,vtcxl,vtcxu,vtcxx,vtcxxl,vtcxxu,
     &vtecm,vtecs,vtecml,vtecmu,vtecsl,vtecsu,vte95,vte95l,vte95u,
     &vte99,vte99l,vte99u,vte995,vte995l,vte995u,
     &vx1,vx2,vx3,vx4,vx5,vx6,vx7,vx8,vx9,vx10,vx11)
*    &vfsh, #######################
*    &vmsg4, ! NON-PARAMETRIC DATA ###################
*    &vspc, ! SPILL DURATIONS ... INTERMITTENT DISCHARGE iiiiiiiiiiiiiiiiiiiiiii
*    &vsfm,vsfs,vscm,vscs,vcoie) ! INTERMITTENT plus correlation iiiiiiiiiiiiiii
*    &vadd,vreg,vrgm,vrgs,viregq,
*    &vrffile,vrcfile,veffile,vecfile,
*     ==========================================================================
!DEC$ ATTRIBUTES DLLEXPORT, STDCALL,REFERENCE, ALIAS : "MONTE" :: MONTE

      integer vforw,viregq,vmsg1,vmsg2,vmsg3,vsens,vmsg4,vtype
      integer vrcns,vecns
      real vint(10) ! intermittent iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      character *150 vrcfile,vrffile,veffile,vecfile,vidfile
      character *1 zed

      include 'montecp.for'
      open(03,file = "Background Results\monte.out")
      call write the heading
      
*     open(unit=4,file="FILES.TMP",status="UNKNOWN")
*	  read(4,40)vrffile
*	  read(4,40)vrcfile
*	  read(4,40)veffile
*	  read(4,40)vecfile
*	  read(4,40)vidfile
*  40 format(A150)
*	  close(4)
*     vtype=0: MC or NP with a percentile target
*     vtype=1: MC or NP with a mean target
*     vtype=2: IN with a percentile target
*     vtype=3: UT with a percentile target (convert to 0, with iutflag=1)

      rfm = vrfm
      rf5 = vrf5
      rcm = vrcm
      rcs = vrcs
      efm = vefm
      efs = vefs
      ecm = vecm
      ecs = vecs
      ecv = ecs/ecm
      forw = vforw
      targ = vtarg
      xper = vxper
      sens = vsens
      add = vadd
      reg = vreg
      rgm = vrgm
      rgs = vrgs
      
      rcns = vrcns
      ecns = vecns
      tcns = (ecm*efm*ecns+rcm*rfm*rcns)/(ecm*efm+rcm*rfm)

      fsh = vfsh
      iregq = viregq

      cofc1 = vcofc1
      coff2 = vcoff2
      cofc3 = vcofc3
      cocf4 = vcocf4
      cofc5 = vcofc5
      cocc6 = vcocc6

      type = vtype
      
      coie = vcoie! uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu
      spc = vspc ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      sfm = vsfm
      sfs = vsfs
      scm = vscm
      scs = vscs

      idfile = vidfile
      rffile = vrffile
      rcfile = vrcfile
      effile = veffile
      ecfile = vecfile
      
      if ( efm .lt. 0.00001 ) then
      vsens = 0
      vforw = 1
      call CONAV( rcm,rcs,rcns,rcml,rcmu)
      call CONSDEV (rcs,rcns,rcsl,rcsu)
      call LNCL (rcm,rcs,rcns,xper,rcq,rcql,rcqu,0) 
      
      vrcml = rcml
      vrcmu = rcmu
      vrcsl = rcsl
      vrcsu = rcsu
      vrcq = rcq
      vrcql = rcql
      vrcqu = rcqu
      
      call CONAV (ecm,ecs,ecns,ecml,ecmu)
      call CONSDEV (ecs,ecns,ecsl,ecsu)
      call LNCL (ecm,ecs,ecns,95.0,ec95,ec95l,ec95u,0) 
      call LNCL (ecm,ecs,ecns,99.0,ec99,ec99l,ec99u,0) 
      call LNCL (ecm,ecs,ecns,99.5,ec995,ec995l,ec995u,0) 
      
      vtcm = rcm
      vtcml = rcml
      vtcmu = rcmu
      vtcs = rcs
      vtcsl = rcsl
      vtcsu = rcsu
      vtcq = rcq
      vtcql = rcql
      vtcqu = rcqu
      vtcns = rcns
      
      vecm = ecm
      vecml = ecml
      vecmu = ecmu
      vecs = ecs
      vecsl = ecsl
      vecsu = ecsu
      vec95 = ec95
      vec95l = ec95l
      vec95u = ec95u
      vec99 = ec99
      vec99l = ec99l
      vec99u = ec99u
      vec995 = ec995
      vec995l = ec995l
      vec995u = ec995u
      
      vtecm = ecm
      vtecml = ecml
      vtecmu = ecmu
      vtecs = ecs
      vtecsl = ecsl
      vtecsu = ecsu
      vte95 = ec95
      vte95l = ec95l
      vte95u = ec95u
      vte99 = ec99
      vte99l = ec99l
      vte99u = ec99u
      vte995 = ec995
      vte995l = ec995l
      vte995u = ec995u
      
      close(03)
      return
      endif
      
      NS = 5000
      if ( NS .lt. 12 ) then ! nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
      read(rffile(1:1),'(a1)')zed
      if ( zed .ne. " " ) then
      call rfnprd
      endif
      read(rcfile(1:1),'(a1)')zed
      if ( zed .ne. " " ) then
      call rcnprd
      endif
      read(effile(1:1),'(a1)')zed
      if ( zed .ne. " ") then
      call efnprd
      endif
      read(ecfile(1:1),'(a1)')zed
      if ( zed .ne. " ") then
      call ecnprd
      endif
      if (type .eq. 2) then ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      read(idfile(1:1),'(a1)')zed
      if ( zed .ne. " " ) then
      call idnprd
      endif
      endif ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      if (msg4 .gt. 0) then
      vmsg4 = msg4
      return
      endif 
      endif ! nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn

      call init      
      call do the calculations (0) ! do the calculations

      vrfm = rfm
      vrf5 = rf5
      vrcm  =rcm
      vrcml = rcml
      vrcmu = rcmu
      vrcs = rcs
      vrcsl = rcsl
      vrcsu = rcsu
      vrcq = rcq
      vrcql = rcql
      vrcqu = rcqu
      vefm = efm
      vefs = efs
      vecm = ecm
      vecml = ecml
      vecmu = ecmu
      vecs = ecs
      vecsl = ecsl
      vecsu = ecsu
      vec95 = ec95
      vec95l = ec95l
      vec95u = ec95u
      vec99 = ec99
      vec99l = ec99l
      vec99u = ec99u
      vec995 = ec995
      vec995l = ec995l
      vec995u = ec995u
      vmsg1 = msg1      
      vmsg2 = msg2
      vmsg3 = msg3
      vtcm = tcm
      vtcml = tcml
      vtcmu = tcmu
      vtcs = tcs
      vtcsl = tcsl
      vtcsu = tcsu
      vtcns = tcns
      vtcxx = tcxx
      vtcxxl = tcxxl
      vtcxxu = tcxxu
      vtcx95 = tcx95
      vtcx99 = tcx99
      
      vtecm = tecm
      vtecml = tecml
      vtecmu = tecmu
      vtecs = tecs
      vtecsl = tecsl
      vtecsu = tecsu
      vte95 = te95
      vte95l = te95l
      vte95u = te95u
      vte99 = te99
      vte99l = te99l
      vte99u = te99u
      vte995 = te995
      vte995l = te995l
      vte995u = te995u
      vte999 = te999
      vte999l = te999l
      vte999u = te999u
      
      vcofc1 = cofc1
      vcoff2 = coff2
      vcofc3 = cofc3
      vcocf4 = cocf4
      vcofc5 = cofc5
      vcocc6 = cocc6

      if (type .eq. 2 .or. iutflag .eq.1) then ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      vint(1) = c(4,K80)
      vint(2) = c(4,K95)
      vint(3) = c(4,K98)
      vint(4) = c(4,K99)
      vint(5) = c(1,K98)
      vint(6) = c(1,K99)
      vint(7) = c(1,K995)
      vint(8) = c(1,K999)
      vint(9) = c(2,K80)
      vint(10) = kevin
      vspc = spc
      vsfm = sfm
      vsfs = sfs
      vscm = scm
      vscs = scs
      endif ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii

      if ( sens .eq. 1 ) then ! perform the sensitivity tests - backward
      if ( te95 .gt. 0.001) then
      call sens1
      else
      vsens = 999
      endif
      endif ! if ( sens .eq. 1 ) perform the sensitivity tests - backward
      
      if ( sens .eq. 2 ) then ! perform the sensitivity tests - forward
      call sens2
      endif ! if ( sens .eq. 2 )  ! perform the sensitivity tests - forward
      vsens = sens

      vx1 = abs(x1)
      vx2 = abs(x2)
      vx3 = abs(x3)
      vx4 = abs(x4)
      vx5 = abs(x5)
      vx6 = abs(x6)
      vx7 = abs(x7)
      vx8 = abs(x8)
      vx9 = abs(x9)
      vx10 = abs(x10)
      vx11 = abs(x11)
      vx12 = abs(x11)
      vx13 = abs(x13)
      vx14 = abs(x14)
      vx15 = abs(x15)
      
      close(03)

      return
      end
      
 

    
*     perform the calculation
      subroutine do the calculations (isens)
      include 'montecp.for'

*     set the starting guess for the discharge quality   
      if ( forw .eq. 0 ) call steff

      if ( forw .eq. 1 ) then ! set up a forward calculation
*     set discharge quality
      tecm = ecm
      tecs = ecs
      if ( isens .eq. 0 ) write(3,581)
  581 format(68('-')/'Calculation of the river quality downstream ',
     &'of the input discharge quality....')
      endif
      
*     store the starting variables to allow resetting after repeat runs
*     which request a temporary change to the data 
      if ( new .eq. 0 .and. isens .eq. 0 ) call set

*     sort out correlation between the Monte Carlo variables
      call correl(nvalid) 

*     set the iteration counter for the backward calculation
      ITER = 0
    
*     compute the mean and standard deviation for logged variables
      call LogFCf

*     test for and execute the forward calculation
      if (forw .eq. 1) then ! ==================================================
      if (ecm .lt. Small) then
      gecm = - Big
      gecs =   0.0
      else
      gecm = ALOG( (ecm*ecm)/SQRT(ecm*ecm + ecs*ecs) )
      gecs = SQRT( ALOG(1. + (ecs*ecs)/(ecm*ecm)) )
      endif
      
      if (type .eq. 2) then ! intermittent effluent iiiiiiiiiiiiiiiiiiiiiiiiii
      if (scm .lt. Small) then
      gscm = - Big
      gscs =   0.0
      else
      gscm = ALOG( (scm*scm)/SQRT(scm*scm + scs*scs) )
      gscs = SQRT( ALOG(1. + (scs*scs)/(scm*scm)) )
      endif
      endif ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      
      call massb setup

      call massb ! perform the mass balance ... for the forward calculation ...
      goto 995
      endif ! if ( forw .eq. 1 ) do the forward calculation ====================

*     prepare for a backward calculation ---------------------------------------

  996 ITER = ITER + 1 ! increase the iteration counter - start next iteration
    
*     compute mean and standard deviation of effluent quality in the log domain
      if (tecm .lt. Small) then
      gecm = - Big
      gecs =   0.0
      else
      gecm = ALOG( (tecm*tecm)/SQRT(tecm*tecm + tecs*tecs) )
      gecs = SQRT( ALOG(1.0 + (tecs*tecs)/(tecm*tecm)) )
      endif
      
      if (type .eq. 2) then ! intermittent effluent iiiiiiiiiiiiiiiiiiiiiiiiiii
      if (scm .lt. Small) then
      gscm = - Big
      gscs =   0.0
      else
      gscm = ALOG( (scm*scm)/SQRT(scm*scm + scs*scs) )
      gscs = SQRT( ALOG(1. + (scs*scs)/(scm*scm)) )
      endif
      endif ! intermittent effluent iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii

      if ( ITER .eq. 1 ) call massb setup
      call massb ! perform the mass balance

*     obtain resulting percentile of downstream river quality ...
      
      targp = C(1,KTG)
      targm = tcm

*     compare the target with the downstream river quality =====================
*     check whether the target is achievable ===================================
      if ( IDIL .eq. 1 ) then 
*     zero discharge quality was tried on the last iteration ===================
*     did this indicate that the target was achieveable? =======================
      if ( type .ne. 1 ) then
      if ( targp .gt. 1.0001 * targ ) goto 995
      else
      if ( targm .gt. 1.0001 * targ ) goto 995
      endif
*     it looks, after all, as though the target is achievable
      IDIL = 0 ! reset the indicator ...
      endif ! if ( IDIL .eq. 1 ) ===============================================

*     test for convergence ( that the correct discharge quality has been found) 
      if (type .ne. 1) then
      CONV = ABS( ( targp - targ) / targ )
      else
      CONV = ABS( ( targm - targ) / targ )
      endif     

*     finish if convergence is achieved or if the number of iterations exceeds 50 ...
      if (CONV .lt. CFAC .OR. ITER .gt. 50) go to 995

*     set up the data for the next iteration
      call next (ichek)

      if (ichek .eq. 0) goto 996 ! proceed to the next iteration
      
  995 continue
      
      rcq = C(3,KTG)
      tcxx = C(1,KTG)
      te95 = C(2,K95)

      call CONAV1(rcm,rcs,rcns,rcml,rcmu)    
      rcml = rcm + rcml - ucm
      rcmu = rcm + rcmu - ucm
      call CONSDEV1 (rcs,rcns,rcsl,rcsu)
      rcsl = rcs + rcsl - ucs
      rcsu = rcs + rcsu - ucs
      call LNCL1 (rcm,rcs,rcns,xper,rcqxxx,rcql,rcqu,0) 
      rcql = rcq + rcql - rcqxxx
      rcqu = rcq + rcqu - rcqxxx
      
      call CONAV1 (ecm,ecs,ecns,ecml,ecmu)
      call CONSDEV1 (ecs,ecns,ecsl,ecsu)
      call LNCL1 (ecm,ecs,ecns,95.0,ec95,ec95l,ec95u,0) 
      call LNCL1 (ecm,ecs,ecns,99.0,ec99,ec99l,ec99u,0) 
      call LNCL1 (ecm,ecs,ecns,99.5,ec995,ec995l,ec995u,0) 
      
      call CONAV1 (tcm,tcs,tcns,tcml,tcmu)
      call CONSDEV1 (tcs,tcns,tcsl,tcsu)
      call LNCL1 (tcm,tcs,tcns,xper,tcxxx,tcxxl,tcxxu,0) 
      tcxxl = tcxx + tcxxl - tcxxx
      tcxxu = tcxx + tcxxu - tcxxx
      
      call CONAV1 (tecm,tecs,ecns,tecml,tecmu)
      call CONSDEV1 (tecs,ecns,tecsl,tecsu)
      call LNCL1 (tecm,tecs,ecns,80.0,te80xxx,te80l,te80u,0) 
      te80l = te80 + te80l - te80xxx
      te80u = te80 + te80u - te80xxx
      call LNCL1 (tecm,tecs,ecns,95.0,te95xxx,te95l,te95u,0) 
      te95l = te95 + te95l - te95xxx
      te95u = te95 + te95u - te95xxx
      call LNCL1 (tecm,tecs,ecns,99.0,te99xxx,te99l,te99u,0) 
      te99l = te99 + te99l - te99xxx
      te99u = te99 + te99u - te99xxx
      call LNCL1 (tecm,tecs,ecns,99.5,te995xxx,te995l,te995u,0) 
      te995l = te995 + te995l - te995xxx
      te995u = te995 + te995u - te995xxx
      call LNCL1 (tecm,tecs,ecns,99.9,te999xxx,te999l,te999u,0)
      te999l = te999 + te999l - te999xxx
      te999u = te999 + te999u - te999xxx

      if ( isens .eq. 0 ) then
      call input table ! write out the input data
      call results ! calculation finished - write out the results
      endif

*     check for convergence failure      
      if ( IDIL .eq. 0 .and. CONV .ge. CFAC) msg2 = 1

*     check for failure to meet the target ...
      if (type .ne. 1) then
      if ( tcxx .gt. 1.0001 * targ ) msg3 = 1
      else
      if ( tcm .gt. 1.0001 * targ ) msg3 = 1
      endif  

      return
      end





*     perform mass balance ...
*     calculate river quality downstream of discharge ...

      subroutine massb
      include 'montecp.for'

*     accumulators for mean and standard deviations
      ucm = 0.0 ! river quality upstream of discharge
      ucs = 0.0
      tecm = 0.0 ! quality of discharge
      tecs = 0.0
      tcm = 0.0 ! river quality downstream of discharge
      tcs = 0.0
      
      crfm = 0.0
      crf5 = 0.0
      crcm = 0.0
      crcs = 0.0
      cefm = 0.0
      cefs = 0.0
      cecm = 0.0
      cecs = 0.0
      
      R = RR ! set the random number staters
      S = RS
      
      if (type .eq. 2 ) then ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      RA1 = RA
      ST = RST
      endif ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      
      do 2 I = 1,NS ! loop around the Monte Carlo shots 
      call GetRan ! obtain random normal deviates

      if (nprf .eq. 0 ) then
      RFN = EXP (R1 * grfs + grfm) - fsh
      RFN = RFN * BM(1)
      else
      call rfnpar (RFN)
      endif
      crfm = crfm + RFN
      TMC(1,I) = RFN

*     compute the flow added by regulation
      REQ = add + AMAX1 (reg - RFN, 0.0)
      RF = AMAX1 (0.0, RFN + REQ)

*     upstream river quality
      if (nprc .eq. 0) then 
      RC = EXP (R2 * grcs + grcm)
      RC = RC * BM(2)
      else 
      call rcnpar(RCN)
      RC = RCN
      endif  
      
      if (iregq .eq. 1) then      
*     add in the effect of regulation water quality
      RG = EXP ( R5 * grgs + grgm)
      RC = ( RC*RFN + RG*REQ ) / RF
      endif
      C(3,I) = RC 

      crcm = crcm + RC
      crcs = crcs + RC * RC

*     effluent flow shot
      if (npef .eq. 0) then
      EF = EXP (R3 * gefs + gefm)
      EF = EF * BM(3)
      else 
      call efnpar(EFN)
      EF = EFN
      endif
      cefm = cefm + EF
      cefs = cefs + EF * EF

*     effluent quality shot
      if ( npec .eq. 0 ) then
*     for Upper Tiers expand high shots (beyond deviate of 1.80) UUUUUUUUUUUUUUU
      if (iutflag .eq. 1) then ! UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU
      if ( R4 .gt. 1.80 ) then
      R4 = 1.80 + ( R4 - 1.80 ) * 1.8
      endif
*     correct remaining shots after extending the tail ....
      if ( R4 .gt. 0.0 ) R4 = 0.9 * R4
      endif ! if (iutflag .eq. 1) UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU
      EC = EXP( R4 * gecs + gecm)
      EC = EC * BM(4)
      C(2,I) = EC
      else  
      call ecnpar(ECN)
      C(2,I) = ECN *tecm/ecm  
      endif
      cecm = cecm + EC
      cecs = cecs + EC * EC
      
*     IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      if (type .eq. 2) then ! intermittent effluent flow shot iiiiiiiiiiiiiiiiii
      if ( R3 .ge. TIM ) then ! 1 intermittent operates iiiiiiiiiiiiiiiiiiiiiiii
      if (npdi .eq. 0) then ! non-parametric distribution nnnnnnnnnnnnnnnnnnnnnn
      SF = EXP ( R7 * gsfs + gsfm )
      else 
      call dinpar (SFN)
      SF = SFN
      endif ! if (npdi .eq. 0)  non-parametric distribution nnnnnnnnnnnnnnnn
    
      if ( scm .gt. Small ) then ! quality shot for intermittent effluent
      C(4,I) = EXP( R8 * gscs + gscm )
      else
      C(4,I) = 0.0
      endif
      else ! ! ( R3 < TIM ) iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      SF = 0.0
      C(4,I) = 0.0
      endif ! if ( R3 .ge. TIM ) iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      endif ! if (type .eq. 2) intermittent effluent flow shot iiiiiiiiiiiiiiiii
*     IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII

*     mass balance for river quality downstream of discharge
    
      if (EF .lt. Small .and. SF .lt. Small) then
      C(1,I) = RC
      goto 1
      endif

      if (RF .lt. Small) then
      if (type .eq. 2) then
      C(1,I) = ( (EF*C(2,I)) + (SF*C(4,I)) ) / (EF + SF)
      else  
      C(1,I) = C(2,I)
      endif  
      goto 1
      endif
      
      if (type .eq. 2) then
      C(1,I) = ( (RF*RC) + (EF*C(2,I) + (SF*C(4,I))  )) / (RF + EF + SF)
      else
      C(1,I) = ( (RF*RC) + (EF*C(2,I))) / (RF + EF)
      endif

    1 continue

*     accumulate values for the calculation of mean and standard deviation
      tcm = tcm + C(1,I)
      tcs = tcs + C(1,I) * C(1,I)
      tecm = tecm + C(2,I)
      tecs = tecs + C(2,I) * C(2,I)
      ucm = ucm + C(3,I)
      ucs = ucs + C(3,I) * C(3,I)

    2 continue

*     compute standard deviation of upstream river quality
*     if (iregq .eq. 1) then
      XXX = ucs - ucm*ucm/FLOAT(NS)
      if (XXX .le. Small) then 
      ucs = 0.0
      else
      ucs = SQRT (XXX/FLOAT(NS-1))
      endif
*     endif
    
*     compute standard deviation of downstream river quality
      if (gecs .lt. Small .and. grcs .lt. Small
     &.and. (type .eq. 2 .and. gscs .lt. Small) .AND. iregq .eq. 0) then
      tcs = 0.0
      else
      XXX = tcs - tcm*tcm/FLOAT(NS)
      if (XXX .le. Small) then 
      tcs = 0.0
      else
      tcs = SQRT (XXX/FLOAT(NS-1))
      endif
      endif

      if (gecs .eq. 0.0 ) then ! compute standard deviation of effluent quality
      tecs = 0.0
      else
      XXX = tecs-tecm*tecm/FLOAT(NS)
      if (XXX .le. Small) then
      tecs = 0.0
      else
      tecs = SQRT (XXX/FLOAT(NS-1))
      endif
      endif ! compute standard deviation of effluent quality

*     corresponding means
      ucm = ucm / FLOAT(NS)
      tcm = tcm / FLOAT(NS)
      tecm = tecm / FLOAT(NS)
    
*     obtain the percentiles
      call sequence
      tcxx = C(1,KTG) ! percentile of downstream river quality ...
      te80 = C(2,K80)
      te95 = C(2,K95)
      te99 = C(2,K99)
      te995 = C(2,K995)
      te999 = C(2,K999)
      
*     compute summary statistics for checking errors in Monte Carlo simulation
      crfm = crfm / FLOAT(NS)
      XXX = crcs - crcm*crcm/FLOAT(NS)
      if (XXX .le. Small) then 
      crcs = 0.0
      else
      crcs = SQRT (XXX/FLOAT(NS-1))
      endif
      crcm = crcm / FLOAT(NS)
      XXX = cefs - cefm*cefm/FLOAT(NS)
      if (XXX .le. Small) then 
      cefs = 0.0
      else
      cefs = SQRT (XXX/FLOAT(NS-1))
      endif
      cefm = cefm / FLOAT(NS)
      XXX = cecs - cecm*cecm/FLOAT(NS)
      if (XXX .le. Small) then 
      cecs = 0.0
      else
      cecs = SQRT (XXX/FLOAT(NS-1))
      endif
      cecm = cecm / FLOAT(NS)
      crf5 = TMC(1,k05)
      
      xx1 = 100.0 * (crfm/rfm - 1.0)
      xx2 = 100.0 * (crcm/rcm - 1.0)
      xx3 = 100.0 * (cefm/efm - 1.0)
      xx4 = 100.0 * (cecm/ecm - 1.0)
      xx5 = 100.0 * (crf5/rf5 - 1.0)
      xx6 = 100.0 * (crcs/rcs - 1.0)
      xx7 = 100.0 * (cefs/efs - 1.0)
      xx8 = 100.0 * (cecs/ecs - 1.0)
      
!      BM(1) = rfm / crfm
!      BM(2) = rcm / crcm
!      BM(3) = efm / cefm
!      BM(4) = ecm / cecm
      
!      write(3,8000)crfm,rfm,xx1,crf5,rf5,xx5,BM(1)
! 8000 format(2f10.2,3x,f12.6,' %',2f10.2,3x,f12.6,' %',3x,f12.6,' ###')
!      write(3,8000)crcm,rcm,xx2,crcs,rcs,xx6,BM(2)
!      write(3,8000)cefm,efm,xx3,cefs,efs,xx7,BM(3)
!      write(3,8000)cecm,ecm,xx4,cecs,ecs,xx8,BM(4)

      return 
      end

      
      
      
*     perform mass balance ...
*     calculate river quality downstream of discharge ...

      subroutine massb setup
      include 'montecp.for'

*     accumulators for mean and standard deviations
      crfm = 0.0
      crf5 = 0.0
      crcm = 0.0
      crcs = 0.0
      cefm = 0.0
      cefs = 0.0
      cecm = 0.0
      cecs = 0.0
      
      R = RR ! set the random number staters
      
      do 2 I = 1,NS ! loop around the Monte Carlo shots 
      call GetRan ! obtain random normal deviates

      RF = EXP (R1 * grfs + grfm) - fsh
      crfm = crfm + RF
      TMC(1,I) = RF

*     upstream river quality
      RC = EXP (R2 * grcs + grcm)
      crcm = crcm + RC
      crcs = crcs + RC * RC

*     effluent flow shot
      EF = EXP (R3 * gefs + gefm)
      cefm = cefm + EF
      cefs = cefs + EF * EF

*     effluent quality shot
      EC = EXP( R4 * gecs + gecm)
      cecm = cecm + EC
      cecs = cecs + EC * EC
      
    2 continue

*     order shots for upsteam river flow
      K = 1
      do 8 I = 1, NS-1
      do 11 J = I+1, NS
      if (TMC(K,I) .lt. TMC(K,J)) go to 11
      CC = TMC(K,I)
      TMC(K,I) = TMC(K,J)
      TMC(K,J) = CC
   11 continue
    8 continue

*     compute summary statistics for checking errors in Monte Carlo simulation
      crfm = crfm / FLOAT(NS)
      XXX = crcs - crcm*crcm/FLOAT(NS)
      if (XXX .le. Small) then 
      crcs = 0.0
      else
      crcs = SQRT (XXX/FLOAT(NS-1))
      endif
      crcm = crcm / FLOAT(NS)
      XXX = cefs - cefm*cefm/FLOAT(NS)
      if (XXX .le. Small) then 
      cefs = 0.0
      else
      cefs = SQRT (XXX/FLOAT(NS-1))
      endif
      cefm = cefm / FLOAT(NS)
      XXX = cecs - cecm*cecm/FLOAT(NS)
      if (XXX .le. Small) then 
      cecs = 0.0
      else
      cecs = SQRT (XXX/FLOAT(NS-1))
      endif
      cecm = cecm / FLOAT(NS)
      crf5 = TMC(1,k05)
      
      xx1 = 100.0 * (crfm/rfm - 1.0)
      xx2 = 100.0 * (crcm/rcm - 1.0)
      xx3 = 100.0 * (cefm/efm - 1.0)
      xx4 = 100.0 * (cecm/ecm - 1.0)
      xx5 = 100.0 * (crf5/rf5 - 1.0)
      xx6 = 100.0 * (crcs/rcs - 1.0)
      xx7 = 100.0 * (cefs/efs - 1.0)
      xx8 = 100.0 * (cecs/ecs - 1.0)
      !xx9 = 100.0 * (ucm/rcm - 1.0)
      !xx10 = 100.0 * (ucs/rcs - 1.0)
      
      BM(1) = rfm / crfm
      BM(2) = rcm / crcm
      BM(3) = efm / cefm
      if ( forw .eq. 1 ) then
      BM(4) = ecm / cecm
      else
      BM(4) = tecm / cecm
      endif
          
      
      !write(3,8000)crfm,rfm,xx1,crf5,rf5,xx5,BM(1)
 8000 format(2f10.2,3x,f12.6,' %',2f10.2,3x,f12.6,' %',3x,f12.6,' ###')
      !write(3,8000)crcm,rcm,xx2,crcs,rcs,xx6,BM(2)
      !write(3,8000)cefm,efm,xx3,cefs,efs,xx7,BM(3)
      !write(3,8000)cecm,ecm,xx4,cecs,ecs,xx8,BM(4)
      !write(3,8000)ucm,rcm,xx9,ucs,rcs,xx10

      return 
      end


      subroutine GetRan ! get a set of random numbers
      include 'montecp.for'

      RR1 = GAS1(R) ! river flow
      RR2 = GAS1(R) ! river quality upstream of discharge
      RR3 = GAS1(R) ! discharge flow
      RR4 = GAS1(R) ! discharge quality

      R5 = GAS5(S) ! quality of regulation water
      if (type .eq. 2) then ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      RR7 = GAS7(ST) ! flow of intermittent discharge
      R8 = GAS7(ST) ! quality of intermittent discharge ! ####################
      endif
     
*     introduce correlation
      R1 = RR1
      R2 = b1*RR1 + b2*RR2
      R3 = c1*RR1 + c2*RR2 + c3*RR3
      R4 = d1*RR1 + d2*RR2 + d3*RR3 + d4*RR4
      
      if (type .eq. 2) then ! intermittent discharge
      R7 = e1*R3 + e2*RR7 ! ####################################
      endif  

*     For non-parametric convert to uniform variate
*     (I have to do something here but I'm not sure what ... )

      if (nprf .gt. 0) R1 = CUMNOR (R1)
      if (nprc .gt. 0) R2 = CUMNOR (R2)
      if (npef .gt. 0) R3 = CUMNOR (R3)
      if (npec .gt. 0) R4 = CUMNOR (R4)      

      return
      end



      subroutine input table ! tabulate the input data ...
      include 'montecp.for'

*     upstream river flow
      if ( rfm .gt. Small ) then
      write(3,2)rfm,rf5
    2 format(68('-')/'Input data ... '/68('-')/
     &'Mean river flow u/s of discharge  ',F18.2/
     &'95-percent exceedence river flow  ',F18.2)
      endif

*     third parameter for Log-normal distribution
      if ( fsh .ne. 0.0 ) then
      write(3,8)fsh
    8 format('Shift parameter          ',F27.2)
      endif

*     upstream river quality
      write(3,3)rcm,rcml,rcmu
    3 format(68('-')/
     &'Mean upstream river quality',F25.2,2f8.2)
      if ( rcm .gt. Small ) then
      write(3,942)rcs,rcsl,rcsu,int(rcns)
  942 format(
     &'Standard deviation    ',F30.2,2f8.2/
     &'Number of samples     ',i30)
      endif

*     constant addition of river flow upstream of the discharge
      if ( add .ne. 0.0 ) then 
      write(3,6)add
    6 format(68('-')/
     &'Constant addition to river flow         ',F22.2)
      endif

*     regulated flow upstream of the discharge
      if ( reg .ne. 0.0 ) then
      write(3,7)reg
    7 format(68('-')/'Maintained river flow         ',F32.2)
      endif

*     quality of added water or regulation water
      if ( iregq .eq. 1 ) then
      write(3,10)rgm
   10 format(68('-')/'Mean quality of added water   ',F32.2)
      write(3,11)rgs
   11 format('Corresponding standard deviation      ',F24.2)
      endif

      return
      end




      subroutine results ! write out the results
      include 'montecp.for'

      tcxx = c(1,KTG)
      tcx95 = c(1,K95)
      tcx99 = c(1,K99)

      te95 = c(2,K95)
      te80 = C(2,K80)
      te99 = c(2,K99)
      te995 = c(2,K995)
      te999 = c(2,K999)
      
      xxx = float (int(xper)) -xper
      kkk = 0
      if ( xxx .lt. Small .and. xxx .gt. -Small ) kkk = int(xper)
      
*     check for illegal shift flow
*     if (ifsh .eq. 1) write(3,2)
    2 format(68('-')/
     &8x,'###  The shift flow is too large a negative value ...'/
     &8x,'###  The value has been reset to -0.99 of the '/
     &8x,'###  95% exceedence river flow ...')
*     upstream river quality (if regulation used)
*     if (iregq .eq. 1) then
*     write(3,958)ucm,ucs
  958 format(68('-')/
     &'Effect of the added river flows ...'/68('-')/
     &'Mean river quality upstream of discharge',F24.2/
     &'Standard deviation                      ',F24.2)
*     endif

      if ( kkk .eq. 0 .and. rcm .gt. Small ) then
      write(3,819)xper,rcq,rcql,rcqu
  819 format(f4.1,'-percentile u/s quality   ',F20.2,2f8.2)
      else
      write(3,829)int(xper),rcq,rcql,rcqu
  829 format(i2,'-percentile u/s quality     ',F22.2,2f8.2)
      endif
 
      write(3,943)efm,efs ! discharge flow
  943 format(68('-')/
     &'Mean flow of discharge          ',F20.2/
     &'Standard deviation              ',F20.2)

      write(3,944)ecm, ecml,ecmu,ecs,ecsl,ecsu,int(ecns) ! discharge quality
  944 format(68('-')/
     &'Mean quality of discharge       ',F20.2,2f8.2/
     &'Standard deviation              ',F20.2,2f8.2/
     &'Number of samples               ',i20)
      write(3,908)tcm,tcml,tcmu,tcs,tcsl,tcsu,tcns ! downstream river quality
  908 format(68('-')/'Results ...'/68('-')/
     &'Mean river quality downstream of discharge',F10.2,2f8.2/
     &'Standard deviation          ',f24.2,2f8.2/
     &'Number of samples           ',f24.1)
	
      if ( kkk .eq. 0 .and. rcm .gt. Small ) then
      write(3,919)xper,tcxx,tcxxl,tcxxu
  919 format(f4.1,'-percentile river quality  ',F19.2,2f8.2)
      else
      write(3,929)kkk,tcxx,tcxxl,tcxxu
  929 format(i2,'-percentile river quality    ',F21.1,2f8.2)
      endif ! if ( kkk .eq. 0 ) then

      if (forw .eq. 0) then
      write(3,125) xper,targ ! River target 
  125 format(68('-')/'River Quality Target (',F4.1,'-percentile)',F14.2)
      endif

*     discharge quality needed to meet river target
      if (efm .lt. Small) return
      if (forw .eq. 0) then
      write(3,978)tecm,tecml,tecmu,tecs,tecsl,tecsu
  978 format(68('-')/
     &'Mean quality of discharge               ',F12.2,2f8.2/
     &'  Standard deviation                    ',F12.2,2f8.2)
      else
      write(3,994)
      endif

*     discharge quality standards
      if (C(2,K95) .gt. Small) then 
      write(3,168)C(2,K80)
  168 format('80-percentile quality of discharge      ',F12.2)
      write(3,968)C(2,K95),te95l,te95u
  968 format('95-percentile quality of discharge      ',F12.2,2f8.2)
      write(3,948)C(2,K99),te99l,te99u
  948 format('99-percentile                           ',F12.2,2f8.2)
      write(3,868)C(2,K995),te995l,te995u
  868 format('99.5-percentile                         ',F12.2,2f8.2)
      write(3,768)C(2,K999),te999l,te999u
  768 format('99.9-percentile                         ',F12.2,2f8.2)
      endif
      write(3,994)
  994 format(68('-'))

      return
      end



*     Arrange high shots in increasing order ...
*     In order to obtain the percentiles ...

      subroutine sequence
      include 'montecp.for'

      KL = min0 ( KTG, K90 ) ! ##############################

      K = 3
      do 3 I = NS, KL, -1
      do 6 J = 1, I - 1
      if (C(K,I) .gt. C(K,J)) go to 6
      CC = C(K,I)
      C(K,I) = C(K,J)
      C(K,J) = CC
    6 continue
    3 continue

*     order river quality shots in downstream river
      K = 1
      do 4 I = NS, KL, -1
      do 5 J = 1, I - 1
      if (C(K,I) .gt. C(K,J)) go to 5
      CC = C(K,I)
      C(K,I) = C(K,J)
      C(K,J) = CC
    5 continue
    4 continue

*     order quality shots in effluent
      K = 2
      do 7 I = NS, K95, -1
      do 10 J = 1, I - 1
      if (C(K,I) .gt. C(K,J)) go to 10
      CC = C(K,I)
      C(K,I) = C(K,J)
      C(K,J) = CC
   10 continue
    7 continue
    
      if (type .eq. 2) then ! order quality shots in intermittent effluent iiiii
      K = 4
      do 1 I = NS, K95, -1
      do 2 J = 1, I - 1
      if (C(K,I) .gt .C(K,J)) go to 2
      CC = C(K,I)
      C(K,I) = C(K,J)
      C(K,J) = CC
    2 continue
    1 continue
      endif ! if (type .eq. 2) order quality shots in intermittent effluent iiii

*     order shots for upsteam river flow
      K = 1
      do 8 I = 1, NS-1
      do 11 J = I+1, NS
      if (TMC(K,I) .lt. TMC(K,J)) go to 11
      CC = TMC(K,I)
      TMC(K,I) = TMC(K,J)
      TMC(K,J) = CC
   11 continue
    8 continue

      return
      end



*     set up a forward calculation
      subroutine Forwd
      include 'montecp.for'

*     set discharge quality
      tecm = ecm
      tecs = ecs

      write(3,581)
  581 format(68('-')/'Calculation of the river quality downstream ',
     &'of the input discharge quality ...')

      return
      end






*     compute the mean and standard deviation of logged variables   
      subroutine LogFCf
      include 'montecp.for'

*     River flow
*     First check for non-parametric distributions

      if (nprf .eq. 0 ) then
      if (rfm .lt. Small) then
      grfm = -Big
      grfs = 0.0
      else

*     Standard deviation in log domain
      grfs = SQRT( 2.7057 + 2.*ALOG((rfm + fsh)/(rf5 + fsh)) )
     &      - 1.6449

*     mean in log domain
      grfm = ALOG (rfm + fsh) - 0.5 * grfs * grfs
*     compute standard deviation of unlogged flows   
      rfs = rfm * SQRT ( EXP( grfs * grfs ) - 1.0 )
      endif
      
      endif

*     river quality
      if (rcm .lt. Small) then
      grcm = -Big
      grcs = 0.0
      else
*     mean in log domain
      grcm = ALOG( (rcm*rcm) / SQRT(rcm*rcm + rcs*rcs) )
*     standard deviation in log domain
      grcs = SQRT( ALOG(1.0 + (rcs*rcs)/(rcm*rcm)) )
      endif

      if (efm .lt. Small) then ! effluent flow
      gefm = -Big
      gefs = 0.0
      else
*     mean in log domain
      gefm = ALOG( (efm*efm) / SQRT(efm*efm + efs*efs) )
*     standard deviation in log domain
      gefs = SQRT( ALOG(1.0 + (efs*efs)/(efm*efm)) )
      endif
      
      if (type .eq. 2) then ! intermittent discharge ---------------------------
      if (sfm .lt. Small) then
      gsfm = -Big
      gsfs = 0.0
      else
*     mean in log domain
      gsfm = ALOG( (sfm*sfm) / SQRT(sfm*sfm + sfs*sfs) )
*     standard deviation in log domain
      gsfs = SQRT( ALOG(1.0 + (sfs*sfs)/(sfm*sfm)) )
      endif
      endif !  ! intermittent discharge ----------------------------------------

      return
      end




*     set starting value of effluent quality ...  

      subroutine steff
      include 'montecp.for'

      IDIL = 0
      KDIL = 0

*     guess the mean river quality downstream of discharge 
      if (type .ne. 1) then
      trcm = 0.6 * targ
      else
      trcm = targ
      endif

*     guess the mean discharge quality
      tecm = amax1 ( 0.01 * targ,
     &      (trcm * (rfm + efm)-(rcm * rfm))/efm)
*     standard deviation - preserve the coefficient of variation, ecv
      tecs = ecv * tecm

      return
      end



      subroutine fail ! Convergence failure detected
      include 'montecp.for'

      PCFAC = 100.0 * CFAC

*     write(3,3)PCFAC
    3 format(8x,'###  Problem failed to converge to within',F5.2,
     &'% of the target'/
     &8x,'###  Check that the results make sense ....'/80('-'))

      return
      end




*     sensitivity analysis - backwards calculation
      subroutine sens1
      include 'montecp.for'

      call do the calculations (1) 

      effmas = te95 
      
      if ( te95 .lt. 0.001 ) then
      sens = 999
      write(3,4430)
 4430 format(//
     &'The sensitivity analysis was suppressed ...'/
     &'Effluent quality is zero ...'//)
      return
      endif
      
      if ( nprf .eq. 0 ) then
      rfm = rfm / 0.9
      call do the calculations (1)
      x1 = 100.0 * c(2,K95) / effmas - 100.0
      rfm = xrfm
      
      rf5 = rf5 / 0.9
      call do the calculations (1)
      x2 = 100.0 * c(2,K95) / effmas - 100.0
      rf5 = xrf5
      fsh = 0.5 * rf5
      call do the calculations (1)
      x12 = 100.0 * c(2,K95) / effmas - 100.0
      fsh = xfsh
      endif

      if ( npef .eq. 0 ) then
      efm = 0.9 * efm
      call do the calculations (1)
      x3 = 100.0 * c(2,K95) / effmas - 100.0
      efm = xefm
      efs = efs / 0.9
      call do the calculations (1)
      x4 = 100.0 * c(2,K95) / effmas - 100.0
      efs = xefs
      endif

      if ( nprc .eq. 0 ) then
      rcm = 0.9 * rcm
      call do the calculations (1)
      x5 = 100.0 * c(2,K95) / effmas - 100.0
      rcm = xrcm

      rcs = 0.9 * rcs
      call do the calculations (1)
      x6 = 100.0 * c(2,K95) / effmas - 100.0
      rcs = xrcs
      endif
      
      if ( npec .eq. 0 ) then
      ecv = ecv / 0.9
      call do the calculations (1)
      x7 = 100.0 * c(2,K95) / effmas - 100.0
      ecv = xecs/xecm
      endif

      targ = targ / 0.9
      call do the calculations (1)
      x8 = 100.0 * te95 / effmas - 100.0
      targ = xtarg

      if (nprf+npef .eq. 0) then
      CoFf2=CoFf2-sign(0.1, CoFf2)
      call do the calculations (1)
      x9 = 100.0 * c(2,K95) / effmas - 100.0
      CoFf2=xCoFf2   
      endif
      
      if (nprf+nprc .eq. 0) then
      CoFc1=CoFc1-sign(0.1, CoFc1)
      call do the calculations (1)
      x10 = 100.0 * c(2,K95) / effmas - 100.0
      CoFc1=xCoFc1
      endif
     
      if (npef+npec .eq. 0) then
      Cofc5=Cofc5-sign(0.1, Cofc5)
      call do the calculations (1)
      x11 = 100.0 * c(2,K95) / effmas - 100.0
      Cofc5=xCofc5
      endif
	
      write(3,7742)x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12
 7742 format(
     &'Impact on the discharge standard of a 10% change in ...'/68('-')/
     &'Mean upstream river flow:         ',f18.2,' %'/
     &'95-percentile river flow:         ',f18.2,' %'/
     &'Mean discharge flow:              ',f18.2,' %'/
     &'Standard deviation:               ',f18.2,' %'/
     &'Mean upstream river quality:      ',f18.2,' %'/
     &'Standard deviation:               ',f18.2,' %'/
     &'CoV of discharge quality:         ',f18.2,' %'/
     &'River target:                     ',f18.2,' %'/
     &'Correlation of F on f (change of 0.1):',f14.2,' %'/
     &'Correlation of F on C (change to 0.1):',f14.2,' %'/
     &'Correlation of f on c (change to 0.1):',f14.2,' %'/
     &'River flow shift (half of q95):   ',f18.2,' %'/
     &68('='))

      return
      end





*     sensitivity analysis - forwards calculation
      subroutine sens2
      include 'montecp.for'

      call do the calculations (1)

      tcxmas = c(1,K90)
      if ( tcxmas .lt. 0.001 ) then
      sens = 999
      write(3,4430)
 4430 format(//
     &'The sensitivity analysis was suppressed ...'/
     &'Effluent quality is zero ...'//)
      return
      endif

      if ( nprf .eq. 0 ) then
      rfm = rfm / 0.9      
      call do the calculations (1)
      x1 = 100.0 * c(1,K90) / tcxmas - 100.0
      rfm = xrfm
      
      rf5 = rf5 / 0.9
      call do the calculations (1)
      x2 = 100.0 * c(1,K90) / tcxmas - 100.0
      rf5 = xrf5
      
      fsh = 0.5 * rf5
      call do the calculations (1)
      x12 = 100.0 * c(1,K90) / tcxmas - 100.0
      fsh = xfsh
      endif

      if ( npef .eq. 0 ) then
      efm = 0.9 * efm
      call do the calculations (1)
      x3 = 100.0 * c(1,K90) / tcxmas - 100.0
      efm = xefm
      
      efs = efs / 0.9
      call do the calculations (1)
      x4 = 100.0 * c(1,K90) / tcxmas - 100.0
      efs = xefs
      endif

      if ( nprc .eq. 0 ) then
      rcm = 0.9 * rcm
      call do the calculations (1)
      x5 = 100.0 * c(1,K90) / tcxmas - 100.0
      rcm = xrcm

      rcs = 0.9 * rcs
      call do the calculations (1)
      x6 = 100.0 * c(1,K90) / tcxmas - 100.0
      rcs = xrcs
      endif

*     mean discharge quality
      if ( npec .eq. 0 ) then
      ecm = 0.9 * ecm 
	  call do the calculations (1)
	  x7 = 100.0 * c(1,K90) / tcxmas - 100.0
      ecm = xecm
     
*     standard deviation for discharge quality
      ecs = ecs / 0.9
	  call do the calculations (1)
	  x8 = 100.0 * c(1,K90) / tcxmas - 100.0
      ecs = xecs
      endif
           
      if (nprf+npef .eq. 0) then
      CoFf2 = CoFf2-sign(0.1, CoFf2)
      call do the calculations (1)
      x9  =  100.0 * c(1,K90) / tcxmas - 100.0
      CoFf2 = xCoFf2   
      endif
      
      if (nprf+nprc .eq. 0) then
      CoFc1 = CoFc1-sign(0.1, CoFc1)
      call do the calculations (1)
      x10 = 100.0 * c(1,K90) / tcxmas - 100.0
      CoFc1 = xCoFc1
      endif
     
      if (npef+npec .eq. 0) then
      Cofc5 = Cofc5-sign(0.1, Cofc5)
      call do the calculations (1)
      x11 = 100.0 * c(1,K90) / tcxmas - 100.0
      Cofc5 = xCofc5
      endif

      write(3,7742)x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12
 7742 format('Impact on the 90-percentile river quality of a 10% ',
     &' change in ...'/68('-')/
     &'Mean upstream river flow (F):     ',f18.2,' %'/
     &'95-percentile river flow:         ',f18.2,' %'/
     &'Mean discharge flow (f):          ',f18.2,' %'/
     &'Standard deviation:               ',f18.2,' %'/
     &'Mean upstream river quality (C):  ',f18.2,' %'/
     &'Standard deviation:               ',f18.2,' %'/
     &'Mean discharge quality (c):       ',f18.2,' %'/
     &'Standard deviation:               ',f18.2,' %'/
     &'Correlation of F on f (change of 0.1):',f12.2,' %'/
     &'Correlation of F on C (change to 0.1):',f12.2,' %'/
     &'Correlation of f on c (change to 0.1):',f12.2,' %'/
     &'River flow shift (half of q95):   ',f18.2,' %'/
     &68('='))

      return
      end



      subroutine next (ichek)
      include 'montecp.for'

      ichek = 0

      if ( ITER .eq. 1 ) then ! set up the data for the first trial
*     set the values to be used to calculate data for the next trial
      if ( type .ne. 1 ) then
      tem1 = rcm ! effluent quality
      else
      tem1 = 0.5 * rcm
      endif
      out1 = 0.0

*     store the results of the first trial
      tem2 = tecm
      if ( type .ne. 1 ) then
      out2 = tcxx
      else
      out2 = tcm
      endif    
      if ( tem1 .eq. tem2 ) tem1 = 0.956 * tem1
      goto 891
      endif

      if (ITER .eq. 2 ) then ! second iteration
      tem1 = tecm ! store the results of the last trial
      if (type .ne. 1) then
      out1 = C(1,KTG) 
      else
      out1 = tcm
      endif     
      goto 891
      endif

*     Iterations after the first two ...
*     Check which of the two retained trials is best ...

      if (ABS ( out1 - targ ) .lt. ABS ( out2 - targ ) ) then

*     over-write second set
      tem2 = tecm
      if (type .ne. 1) then
      out2 = C(1,KTG)
      else
      out2 = tcm
      endif    
      else

*     over-write the first set
      tem1 = tecm
      if (type .ne. 1) then
      out1 = C(1,KTG) 
      else
      out1 = tcm
      endif    
      endif

*     interpolate between latest trial and retained trial
  891 denom = out2 - out1
      if (denom. gt. Small .or. denom .lt. -Small) then
      tecm = tem1 + (targ-out1) * (tem2-tem1) / denom
      else
      ichek = 1
      return
      endif

*     check for zero discharge quality
      if ( tecm .gt. 0.0 ) go to 183

*     discharge quality is zero. Replace with fraction of previous trials.
      tecm = 0.1 * AMIN1( tem1, tem2 ) ! #############################
*     check again for zero ...
      if (tecm .gt. 0.0) go to 183 ! #####################################
*     is this the second time that a zero has been obtained ?
*     if so, a fault has been detected ...

      if (KDIL .gt. 0) then
      msg1 = 1

*     set up a calculation to check whether target can be achieved at all
      else
      IDIL = 1
      KDIL = 1
      tecm = 0.0
      tecs = 0.0
      return
      endif    

*     compute standard deviation of effluent quality ... for next iteration
  183 tecs = ecv * tecm

      return
      end


      subroutine init ! set the starting values of data items
      include 'montecp.for'

*     precision factor - iterative scheme stops when successive
*     trials are within 100*CFAC %
      CFAC = 0.00001

      new = 0
      one = 0.9999 ! test parameter for correlation coefficients
      Small = 1.0e-20
      Big = 1.0e5    

      RR = -5119
      RR = -1858
*     RR = -1119
      
      RFSTART = -5119 ! random number statert - river flow - MPER and SIMCAT
      EFSTART = -1849 + 9 ! discharge flow (1849 for effluent flows)
      RCSTART = -7849 + 6 ! river quality for dissolved metal (1849 for river quality)
      ECSTART = -5149 + 1 ! discharge quality for dissolved metal (1849 for discharge quality)
      
      KTG = INT(0.5 + 0.01 * XPER * NS)
      K05 = INT(0.5 + 0.05 * NS)
      K80 = INT(0.5 + 0.80 * NS)
      K90 = INT(0.5 + 0.90 * NS)
      K95 = INT(0.5 + 0.95 * NS)
      K98 = INT(0.5 + 0.98 * NS)
      K99 = INT(0.5 + 0.99 * NS)
      K995 = INT(0.5 + 0.995 * NS)
      K999 = INT(0.5 + 0.999 * NS)

      RST = -6411
      TIM =  TDIST1 (9999.0, (1.0-0.01*spc) )
      kevin = 0 ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii

      rs = -2379
      npmax = 1000
      
      if (rgm .le. Small) then
      vrgs =  0.0
      rgs=0.0
      grgm = - Big
      grgs = 0.0     
      else
      grgm = ALOG( (rgm*rgm)/SQRT(rgm*rgm + rgs*rgs) )
      grgs = SQRT( ALOG( 1.0 + (rgs*rgs)/(rgm*rgm)) )
      endif

      nprc = 0
      nprf = 0
      npef = 0
      npec = 0
      npdi = 0 ! iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii

      if (type .eq. 3) then ! upper-tier uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu
      type = 0
      iutflag = 1
      else
      iutflag = 0
      endif ! uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu

      msg1 = 0
      msg2 = 0
      msg3 = 0
      msg4 = 0
      
      do j = 1, 5000
      do i = 1, 4
      C(i,j) = 0.0
      enddo
      do i = 1, 8
      TMC(i,j) = 0.0
      enddo
      enddo
      
      return 
      end
      
      
      subroutine Correl(nvalid)

*     Takes 6 correlation coefficients, checks their validity, scales
*     them if necessary to make them valid, and returns nvalid=1 if they
*     have been scaled.

*     'test' is a scaling factor and testhigh and testlow are the
*     max and min values of test when iterating using the bisection
*     method.

      include 'montecp.for'

      sCoFC1=CoFC1
      sCoFf2=CoFf2
      sCoFc3=CoFc3
      sCoCf4=CoCf4
      sCofc5=Cofc5
      sCoCc6=CoCc6

      testhigh=1
      testlow=0
      test=testhigh

      call Check4(jvalid,nvalid)
      if (jvalid .eq. 0) then
      return
      endif

      test=0
      do while (testhigh-testlow .gt. 0.005)
      CoFC1=sCoFC1*test
      CoFf2=sCoFf2*test
      CoFc3=sCoFc3*test
      CoCf4=sCoCf4*test
      Cofc5=sCofc5*test
      CoCc6=sCoCc6*test
      call Check4(jvalid,nvalid)
      If (jvalid .eq. 0) then
      testlow=test
      else
      testhigh=test
      endif
      test=0.5*(testhigh+testlow)
      enddo

      test=testlow
      CoFC1=sCoFC1*test
      CoFf2=sCoFf2*test
      CoFc3=sCoFc3*test
      CoCf4=sCoCf4*test
      Cofc5=sCofc5*test
      CoCc6=sCoCc6*test

      call Check4(jvalid,nvalid)
      nvalid=1

      return
      end


      subroutine Check4(jvalid,nvalid)

*     Checks validity of 6 correlation coefficients for 4 variables.
*     Each iteration involves checking CCs in turn for 4 sets of 3 variables,
*     scaling any set of 3 CCs as necessary, and then checking for
*     valid values of c3, d3 and d4.

      include 'montecp.for'

      nvalid=0

      call Check3(CoFC1,CoFf2,CoCf4,f)
      if ( f .lt. 1 ) then
      nvalid=1
      endif

      call Check3(CoFC1,CoFc3,CoCc6,f)
      if ( f .lt. 1 ) then
      nvalid=1
      endif

      call Check3(CoFf2,CoFc3,Cofc5,f)
      if ( f .lt. 1 ) then
      nvalid=1
      endif

      call Check3(CoCf4,Cofc5,CoCc6,f)
      if ( f .lt. 1 ) then
      nvalid=1
      endif


      b1=CoFC1
      b2=sqrt(1.-b1*b1)
      c1=CoFf2

      if (b2 .gt. 0.) then
      c2 = (CoCf4 - b1 * c1) / b2
      else
      c2 = 0.
      endif

      ck = 1. - c1 * c1 - c2 * c2
      if (ck .gt. 0.) then
      c3 = sqrt (ck)
      else
      c3 = 0.
      endif

      d1 = CoFc3

      if (b2 .gt. 0.) then
      d2=(CoCc6 - b1 * d1 ) /b2
      else
      d2 = 0.
      endif

      if (c3 .gt. 0.) then
      d3 = (Cofc5 - c1 * d1 - c2 * d2) / c3
      else
      d3 = 0.
      endif

      ck = 1. - d1 * d1 - d2 * d2 - d3 * d3
      if (ck .gt. 0.) then
      d4 = sqrt (ck)
      else
      d4 = 0.
      endif

      jvalid=0
      if ( abs(d3) .gt. 1.0 .or. ck .lt. -0.00001 ) jvalid=1

*     correlation for intermittent flow
      if (type .eq. 2) then
      e1 = COIE
      sq = 1.0 - e1*e1
      if (sq .gt. 0.00001) then
      e2 = SQRT (sq)
      else
      e2 = 0.0
      endif
      endif

      return
      end


      subroutine Check3 ( corr1, corr2, corr3, f )

*     Checks the validity of the three correlation coefficients relating
*     to a set of three variables. f is a scaling factor and fhigh and flow
*     are the max and min values of f when iterating using the bisection
*     method.

      scorr1 = corr1
      scorr2 = corr2
      scorr3 = corr3

      fhigh = 1
      flow = 0
      f = fhigh

      temp = 1.0-corr1*corr1-corr2*corr2-corr3*corr3+2*corr1*corr2*corr3
      if (temp .ge.0.0) then
      return
      endif

      f = 0
      do while (fhigh-flow .gt. 0.005)
      corr1 = scorr1*f
      corr2 = scorr2*f
      corr3 = scorr3*f
      temp = 1.0-corr1*corr1-corr2*corr2-corr3*corr3+2*corr1*corr2*corr3
      If (temp.ge.0.0) then
      flow = f
      else
      fhigh = f
      endif
      f = 0.5*(fhigh+flow)
      enddo

      f = flow
      corr1 = scorr1*f
      corr2 = scorr2*f
      corr3 = scorr3*f

      return
      end
      



      function GAS1(IDUM) ! ---------------------------- random number gerarator
      save ISET,GSET
      data ISET/0/
      if (IDUM .lt. 0) ISET = 0
      if (ISET .eq. 0) then
    1 continue  
      V1 = 2.*RAN1(IDUM)-1.0
	V2 = 2.*RAN1(IDUM)-1.0
	R = V1**2 + V2**2
	if (R .ge. 1.) goto 1
	FAC = SQRT(-2.*LOG(R)/R)
	GSET = V1*FAC
	GAS1 = V2*FAC
	ISET = 1
      else
	GAS1 = GSET
	ISET = 0
      endif
      return
      end ! -------------------------------------------- random number gerarator
      function GAS5(IDUM) ! ---------------------------- random number gerarator
      save iset,gset
      data ISET/0/
      if (IDUM .lt. 0) ISET = 0
      if (ISET .eq. 0) then
    1 V1 = 2.*RAN5(IDUM)-1.0
	V2 = 2.*RAN5(IDUM)-1.0
	R = V1**2 + V2**2
	if (R.GE.1.) goto 1
	FAC = SQRT(-2.*LOG(R)/R)
	GSET = V1*FAC
	GAS5 = V2*FAC
	ISET = 1
      else
	GAS5 = GSET
	ISET = 0
      endif
      return
      end ! -------------------------------------------- random number gerarator
      function GAS7(IDUM) ! ---------------------------- random number gerarator
      save iset,gset
      data ISET/0/
      if (IDUM .lt. 0) ISET = 0
      if (ISET .eq. 0) then
    1 V1 = 2.*RAN7(IDUM)-1.0
	V2 = 2.*RAN7(IDUM)-1.0
	R = V1**2 + V2**2
	if (R.GE.1.) goto 1
	FAC = SQRT(-2.*LOG(R)/R)
	GSET = V1*FAC
	GAS7 = V2*FAC
	ISET = 1
      else
	GAS7 = GSET
	ISET = 0
      endif
      return
      end ! -------------------------------------------- random number gerarator

      function RAN1(IDUM) ! ============================ random number gerarator
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if (IDUM .lt. 0 ) then
	IFF = 1
	IX1 = MOD (IC1-IDUM,M1)
	IX1 = MOD (IA1*IX1 + IC1,M1)
	IX2 = MOD (IX1,M2)
	IX1 = MOD (IA1*IX1 + IC1,M1)
	IX3 = MOD (IX1,M3)
	DO J = 1,97
      IX1 = MOD (IA1*IX1 + IC1,M1)
      IX2 = MOD (IA2*IX2 + IC2,M2)
      R(J) = (FLOAT(IX1) + FLOAT(IX2)*RM2)*RM1
      enddo
	IDUM = 1
      endif
      IX1 = MOD (IA1*IX1 + IC1,M1)
      IX2 = MOD (IA2*IX2 + IC2,M2)
      IX3 = MOD (IA3*IX3 + IC3,M3)
      JJ = 1 + (97*IX3)/M3
      J = iabs(JJ)
      if (J .gt. 97 .or. J .lt. 1)PAUSE
      RAN1 = R(J)
      R(J) = (FLOAT(IX1) + FLOAT(IX2)*RM2)*RM1
      return
      end ! ============================================ random number gerarator
      function RAN5(IDUM) ! ============================ random number gerarator
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if (IDUM .lt. 0 ) then
	IFF = 1
	IX1 = MOD (IC1-IDUM,M1)
	IX1 = MOD (IA1*IX1 + IC1,M1)
	IX2 = MOD (IX1,M2)
	IX1 = MOD (IA1*IX1 + IC1,M1)
	IX3 = MOD (IX1,M3)
	DO J = 1,97
      IX1 = MOD (IA1*IX1 + IC1,M1)
      IX2 = MOD (IA2*IX2 + IC2,M2)
      R(J) = (FLOAT(IX1) + FLOAT(IX2)*RM2)*RM1
      enddo
	IDUM = 1
      endif
      IX1 = MOD (IA1*IX1 + IC1,M1)
      IX2 = MOD (IA2*IX2 + IC2,M2)
      IX3 = MOD (IA3*IX3 + IC3,M3)
      J = 1 + (97*IX3)/M3
      if (J .gt. 97.OR.J .lt. 1)PAUSE
      RAN5 = R(J)
      R(J) = (FLOAT(IX1) + FLOAT(IX2)*RM2)*RM1
      return
      end ! ============================================ random number gerarator
      function RAN7(IDUM) ! ============================ random number gerarator
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if (IDUM .lt. 0 ) then
	IFF = 1
	IX1 = MOD (IC1-IDUM,M1)
	IX1 = MOD (IA1*IX1 + IC1,M1)
	IX2 = MOD (IX1,M2)
	IX1 = MOD (IA1*IX1 + IC1,M1)
	IX3 = MOD (IX1,M3)
	DO J = 1,97
      IX1 = MOD (IA1*IX1 + IC1,M1)
      IX2 = MOD (IA2*IX2 + IC2,M2)
      R(J) = (FLOAT(IX1) + FLOAT(IX2)*RM2)*RM1
      enddo
	IDUM = 1
      endif
      IX1 = MOD (IA1*IX1 + IC1,M1)
      IX2 = MOD (IA2*IX2 + IC2,M2)
      IX3 = MOD (IA3*IX3 + IC3,M3)
      J = 1 + (97*IX3)/M3
      if (J .gt. 97.OR.J .lt. 1)PAUSE
      RAN7 = R(J)
      R(J) = (FLOAT(IX1) + FLOAT(IX2)*RM2)*RM1
      return
      end ! ============================================ random number gerarator

      
*     read in the data for a non-parametric distribution of river flow

      subroutine rfnprd
      logical exists
      character*80 string

      include 'montecp.for'

*     attach data file
 
      Inquire(FILE=rffile,EXIST=exists)
      if ( .NOT. exists) then
      msg4 = msg4 + 1   
      return
      endif

      open(10,FILE=rffile,STATUS='OLD') ! open the file ------------------------
      read (10, *, ERR=20) nprf ! read the file --------------------------------
      if(nprf.eq.-1)goto 20
      iflag = 0
      goto 50

*     test for Aardvark data
   20 read(10,'(A1)',err=7500)ans
      i = 1
   30 read(10,31,end=40)string
   31 format(A80)
      read(string,*,err=30)idum1,idum2,idum3,cptemp
      rfnpvl(i) = cptemp
      i = i + 1
      goto 30
   40 close(10)
      nprf = i - 1
      iflag = 1

*     check the number of data points - are there too many ?      
   50 if (nprf .gt. npmax) then
      msg4 = msg4 + 3
      return
      endif

*     check the number of data points - are there too few ?      

      if (nprf .lt. 5) then
      msg4 = msg4 + 4
      return
      endif

      if (iflag .eq. 0) then
      backspace (10)
      read (10, *, ERR=7500) nprf, (rfnpvl(i),i = 1, nprf)
      rewind (10)
      close(10)
      endif
      
*     arrange the data in sequence

      do 11 i = 1, nprf-1
      do 12 j = i+1, nprf
      if (rfnpvl(i) .ge. rfnpvl(j)) goto 12
      x = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = x
   12 continue 
   11 continue

*     compute cumulative frequencies and store them in cfd
      CUMUL = 1.0 / float (nprf + 1)
      do 10 i = 1, nprf
   10 rfnpfd(i) = float(i) * CUMUL

*     compute mean and 5-percentile
      rfm = 0.0
      do 13 i = 1, nprf
      rfm = rfm + rfnpvl (i)
   13 continue
      rfm = rfm / float (nprf)  
      R1 = 0.95
      call rfnpar (rf5)

      return

 7500 msg4 = msg4 + 2
      return
      end


*     read in the data for a non-parametric distribution of river qual.
      subroutine rcnprd

      logical exists
      character*80 string

      include 'montecp.for'

*     attach data file
      Inquire(FILE=rcfile,EXIST=exists)
      if ( .NOT. exists) then
      msg4 = msg4 + 10
      return
      endif

*     open the file

      open(10,FILE=rcfile,STATUS='OLD')

*     read the file

      read (10, *, ERR=20) nprc
      if(nprc.eq.-1)goto 20
      iflag = 0
      goto 50

*     Test for Aardvark data
   20 read(10,'(A1)',err=7500)ans
*     read(10,'(A1)',err=7500)ans
      i = 1
   30 read(10,31,end=40)string
   31 format(A80)
      read(string,*,err=30)idum1,idum2,idum3,cptemp
      rcnpvl(i) = cptemp
      i = i + 1
      goto 30
   40 close(10)
      nprc = i - 1
      iflag = 1

*     check the number of data points - are there too many ?      
   50 if (nprc .gt. npmax) then
      msg4 = msg4 + 30
      return
      endif

*     check the number of data points - are there too few ?      
      if (nprc .lt. 5) then
      msg4 = msg4 + 40
      return
      endif

      if (iflag .eq. 0) then
      backspace (10)
      read (10, *, ERR=7500) nprc, (rcnpvl(i),i=1 , nprc)
      rewind (10)
      close(10)
      endif

*     arrange the data in sequence
      do 11 i = 1, nprc-1
      do 12 j = i+1, nprc

      if (rcnpvl(i) .ge. rcnpvl(j)) goto 12
      x = rcnpvl (j)
      rcnpvl (j) = rcnpvl (i)
      rcnpvl (i) = x
   12 continue 
   11 continue

*     compute cumulative frequencies and store them in cfd

      CUMUL = 1.0 / float (nprc + 1)

      do 10 i = 1, nprc
   10 rcnpfd(i) = float(i) * CUMUL

*     compute mean and standard deviation

      rcm = 0.0
      do 13 i = 1, nprc
      rcm = rcm + rcnpvl (i)
   13 continue
      rcm = rcm / float (nprc)  

      rc2 = 0.0
      do 14 i = 1, nprc
     
*     sum of the x^2
      rc2 = rc2 + rcnpvl(i)**2
   14 continue
      rcs = SQRT((1/(float(nprc)-1))*(rc2-float(nprc)*rcm**2))  

      return

 7500 msg4 = msg4 + 20
      return

      end


*     read in the data for a non-parametric distribution of effluent f.
      subroutine efnprd

      logical exists
      character*80 string

      include 'montecp.for'

*     attach data file
      Inquire(FILE=effile,EXIST=exists)
      if ( .NOT. exists) then
      msg4 = msg4 + 100
      return
      endif

*     open the file
      open(10,FILE=effile,STATUS='OLD')

*     read the file
      read (10, *, ERR=20) npef
      if(npef.eq.-1)goto 20
      iflag = 0
      go to 50

*     test for Aardvark data
   20 read(10,'(A1)',err=7500)ans
      i = 1
   30 read(10,31,end=40)string
   31 format(A80)
      read(string,*,err=30)idum1,idum2,idum3,cptemp
      efnpvl(i) = cptemp
      i = i + 1
      goto 30
   40 close(10)
      npef = i - 1
      iflag = 1

*     check the number of data points - are there too many ?      
   50 if (npef .gt. npmax) then
      msg4 = msg4 + 300
      return
      endif

*     check the number of data points - are there too few ?      

      if (npef .lt. 5) then
      msg4 = msg4 + 400
      return
      endif

      if (iflag .eq. 0) then
      backspace (10)
      read (10, *, ERR=7500) npef, (efnpvl(i),i=1 , npef)
      rewind (10)
      close(10)
      endif

*     arrange the data in sequence

      do 11 i = 1, npef-1
      do 12 j = i+1, npef

      if (efnpvl(i) .ge. efnpvl(j)) goto 12

      x = efnpvl (j)
      efnpvl (j) = efnpvl (i)
      efnpvl (i) = x

   12 continue 
   11 continue

*     compute cumulative frequencies and store them in cfd

      CUMUL = 1.0 / float (npef + 1)

      do 10 i = 1, npef

   10 efnpfd(i) = float(i) * CUMUL

*     compute mean and standard deviation

      efm = 0.0
      do 13 i = 1, npef
      efm = efm + efnpvl (i)
   13 continue
      efm = efm / float (npef)  

      ef2=0.0
      do 14 i = 1, npef
     
*     sum of the x^2
     
      ef2 = ef2 + efnpvl(i)**2
   14 continue
      efs = SQRT((1/(float(npef)-1))*(ef2-float(npef)*efm**2))  

      return

 7500 msg4 = msg4 + 200
      return
      end


*     read in the data for a non-parametric distribution of effluent quality
      subroutine ecnprd

      Logical exists
      character*80 string
      
      include 'montecp.for'

*     attach data file
      Inquire(FILE=ecfile,EXIST=exists)
      if ( .NOT. exists) then
      msg4 = msg4 + 1000
      return
      endif

*     open the file
      open(10,FILE=ecfile,STATUS='OLD')

*     read the file
      read (10, *, ERR=20) npec
      if(npec.eq.-1)goto 20
      iflag = 0
      go to 50

*     test for Aardvark data
   20 read(10,'(A1)',err=7500)ans
      i = 1
   30 read(10,31,end=40)string
   31 format(A80)
      read(string,*,err=30)idum1,idum2,idum3,cptemp
      ecnpvl(i)=cptemp
      i = i + 1
      goto 30
   40 close(10)
      npec = i - 1
      iflag = 1

*     check the number of data points - are there too many ?      

   50 if (npec .gt. npmax) then
      msg4 = msg4 + 3000
      return
      endif

*     check the number of data points - are there too few ?      

      if (npec .lt. 5) then
      msg4 = msg4 + 4000
      return
      endif

      if (iflag .eq. 0) then
      backspace (10)
      read (10, *, ERR=7500) npec, (ecnpvl(i),i=1 , npec)
      rewind (10)
      close(10)
      endif

*     arrange the data in sequence
      do 11 i = 1,   npec-1
      do 12 j = i+1, npec
      if (ecnpvl(i) .ge. ecnpvl(j)) goto 12
      x = ecnpvl (j)
      ecnpvl (j) = ecnpvl (i)
      ecnpvl (i) = x
   12 continue 
   11 continue

*     compute cumulative frequencies and store them in cfd
      CUMUL = 1.0 / float (npec + 1)

      do 10 i = 1, npec
   10 ecnpfd(i) = float(i) * CUMUL

*     compute mean and standard deviation
      ecm = 0.0
      do 13 i = 1, npec
      ecm = ecm + ecnpvl (i)
   13 continue
      ecm = ecm / float (npec)  

      ec2 = 0.0
      do 14 i = 1, npec

*     sum of the x^2
      ec2 = ec2 + ecnpvl(i)**2
   14 continue
      ecs = SQRT((1/(float(npec)-1))*(ec2-float(npec)*ecm**2))  
      ecv = ecs / ecm

      return

 7500 msg4 = msg4 + 2000
      return

      end

     
*     provide a random sample from a non-parametric distribution 
      subroutine rfnpar (RFN) 

*     rfnpvl    is an array of class marks,
*     rfnpfd    is an array of cumulative frequencies,      
*     nprf      is the number of classes,
*     R1        is the uniform variate,
*     RFN       is the returned variate.

      include 'montecp.for'

*     prepare to deal with values beyond the first and the last values
*     entered as data ....
*     we shall interpolate between the last point and a point half the last
*     interval beyond the last point

*     calculate the first interval
      CLINT1 = rfnpvl (2) - rfnpvl (1)

*     TAU is the minimum - the first value less half the interval
      TAU = rfnpvl (1) - CLINT1 / 2.

*     the last interval 
      CLINT2 = rfnpvl (nprf) - rfnpvl (nprf-1)

*     THETA is the maximum - last value plus half the interval
      THETA = rfnpvl (nprf) + CLINT2 / 2.

*     calculate the gradients of these tails of the distribution
      GRAD1 = ( rfnpvl(1) - TAU  ) / rfnpfd (1)
      GRAD2 = ( THETA - rfnpvl (nprf) ) / (1. - rfnpfd (nprf) )

*     locate this point on the cumulative frequency distribution
      if ( R1 .le. rfnpfd(1) ) then
      RFN = GRAD1 * R1 + TAU

      else if ( R1 .ge. rfnpfd(nprf) ) then
      RFN = GRAD2 * R1 + THETA - GRAD2
      else
      CL = float(nprf)
      L = 1
      U = nprf
   10 cxx = (U + float(L) )/ 2.
      I = INT (cxx + 0.5)

      if (R1 .le. rfnpfd(I) .and. R1 .ge. rfnpfd(I - 1)) then

      Y1 = rfnpvl(I - 1)
      Y2 = rfnpvl(I)
      XX1 = rfnpfd(I - 1)
      XX2 = rfnpfd(I)
 
      RFN = (R1 - XX1) * (Y2 - Y1) / (XX2 - XX1) + Y1

      else

      if (R1 .GT. rfnpfd(I)) L = I
      if (R1 .LT. rfnpfd(I)) U = I
      goto 10
   
      endif
      endif

      RFN = amax1( 0.0, RFN )

      return
      end




*     provide a random sample from a non-parametric distribution for river quality
      subroutine rcnpar (RCN) 

*     rcnpvl    is an array of class marks,
*     rcnpfd    is an array of cumulative frequencies,      
*     nprc      is the number of classes,
*     R2        is the uniform variate,
*     RCN       is the returned variate.

      include 'montecp.for'

*     prepare to deal with values beyond the first and the last values
*     entered as data ....
*     we shall interpolate between the last point and a point half the last
*     interval beyond the last point

*     calculate the first interval
      CLINT1 = rcnpvl (2) - rcnpvl (1)

*     TAU is the minimum - the first value less half the interval
      TAU = rcnpvl (1) - CLINT1 / 2.

*     the last interval 
      CLINT2 = rcnpvl (nprc) - rcnpvl (nprc-1)

*     THETA is the maximum - last value plus half the interval
      THETA = rcnpvl (nprc) + CLINT2 / 2.

*     calculate the gradients of these tails of the distribution
      GRAD1 = ( rcnpvl(1) - TAU  ) / rcnpfd (1)
      GRAD2 = ( THETA - rcnpvl (nprc) ) / (1. - rcnpfd (nprc) )

*     locate this point on the cumulative frequency distribution.
      if ( R2 .le. rcnpfd(1) ) then

      RCN = GRAD1 * R2 + TAU

      else if ( R2 .ge. rcnpfd(nprc) ) then
      RCN = GRAD2 * R2 + THETA - GRAD2
      else
      CL = float(nprc)
      L = 1
      U = nprc
   10 cxx = (U + float(L) )/ 2.
      I = INT (cxx + 0.5)

      if (R2 .le. rcnpfd(I) .and. R2 .ge. rcnpfd(I - 1)) then

      Y1 = rcnpvl(I - 1)
      Y2 = rcnpvl(I)
      XX1 = rcnpfd(I - 1)
      XX2 = rcnpfd(I)
 
      XXN = (R2 - XX1) * (Y2 - Y1) / (XX2 - XX1) + Y1
      RCN = XXN
      else ! ###### only in AM
      if (R2 .GT. rcnpfd(I)) L = I
      if (R2 .LT. rcnpfd(I)) U = I
      goto 10
   
      endif
      endif

      RCN = amax1( 0.0, RCN )

      return
      end


*     provide a random sample from a non-parametric distribution for effluent flow
      subroutine efnpar (EFN) 

*     efnpvl    is an array of class marks,
*     efnpfd    is an array of cumulative frequencies,      
*     npef      is the number of classes,
*     R3        is the uniform variate,   
*     EFN       is the returned variate.

      include 'montecp.for'

*     prepare to deal with values beyond the first and the last values
*     entered as data ....
*     we shall interpolate between the last point and a point half the last
*     interval beyond the last point

      R3 = 1.0 - R3

*     calculate the first interval
      CLINT1 = efnpvl (2) - efnpvl (1)
*     TAU is the minimum - the first value less half the interval
      TAU = efnpvl (1) - CLINT1 / 2.

*     the last interval 
      CLINT2 = efnpvl (npef) - efnpvl (npef-1)
*     THETA is the maximum - last value plus half the interval
      THETA = efnpvl (npef) + CLINT2 / 2.

*     calculate the gradients of these tails of the distribution
      GRAD1 = ( efnpvl(1) - TAU  ) / efnpfd (1)
      GRAD2 = ( THETA - efnpvl (npef) ) / (1. - efnpfd (npef) )

*     locate this point on the cumulative frequency distribution.

      if ( R3 .le. efnpfd(1) ) then

      EFN = GRAD1 * R3 + TAU

      else if ( R3 .ge. efnpfd(npef) ) then

      XXN = GRAD2 * R3 + THETA - GRAD2
 
      else

      CL = float(npef)
      L = 1
      U = npef

   10 cxx = (U + float(L) )/ 2.
      I = INT (cxx + 0.5)

      if (R3 .le. efnpfd(I) .and. R3 .ge. efnpfd(I - 1)) then

      Y1 = efnpvl(I - 1)
      Y2 = efnpvl(I)
      XX1 = efnpfd(I - 1)
      XX2 = efnpfd(I)
 
      EFN = (R3 - XX1) * (Y2 - Y1) / (XX2 - XX1) + Y1

      else

      if (R3 .GT. efnpfd(I)) L = I
      if (R3 .LT. efnpfd(I)) U = I
      goto 10
   
      endif
      endif

      EFN = amax1( 0.0, EFN )

      return
      end





*     provide a random sample from a non-parametric distribution for effluent quality

      subroutine ecnpar (ECN) 

*     ecnpvl    is an array of class marks,       
*     ecnpfd    is an array of cumulative frequencies,      
*     npec      is the number of classes,
*     R4        is the uniform variate,
*     ECN       is the returned variate.

      include 'montecp.for'

*     prepare to deal with values beyond the first and the last values
*     entered as data ....
*     we shall interpolate between the last point and a point half the last
*     interval beyond the last point

*     calculate the first interval
      CLINT1 = ecnpvl (2) - ecnpvl (1)

*     TAU is the minimum - the first value less half the interval
      TAU = ecnpvl (1) - CLINT1 / 2.

*     the last interval 
      CLINT2 = ecnpvl (npec) - ecnpvl (npec-1)

*     THETA is the maximum - last value plus half the interval
      THETA = ecnpvl (npec) + CLINT2 / 2.

*     calculate the gradients of these tails of the distribution
      GRAD1 = ( ecnpvl(1) - TAU  ) / ecnpfd (1)
      GRAD2 = ( THETA - ecnpvl (npec) ) / (1. - ecnpfd (npec) )

*     locate this point on the cumulative frequency distribution.
      if ( R4 .le. ecnpfd(1) ) then
      ECN = GRAD1 * R4 + TAU
      else if ( R4 .ge. ecnpfd(npec) ) then
      ECN = GRAD2 * R4 + THETA - GRAD2
      else

      CL = float(npec)
      L = 1
      U = npec
   10 cxx = (U + float(L) )/ 2.
      I = INT (cxx + 0.5)

      if (R4 .le. ecnpfd(I) .and. R4 .ge. ecnpfd(I - 1)) then
      Y1 = ecnpvl(I - 1)
      Y2 = ecnpvl(I)
      XX1 = ecnpfd(I - 1)
      XX2 = ecnpfd(I)
      ECN = (R4 - XX1) * (Y2 - Y1) / (XX2 - XX1) + Y1

      else
      if (R4 .GT. ecnpfd(I)) L = I
      if (R4 .LT. ecnpfd(I)) U = I
      goto 10
   
      endif
      endif

      ECN = amax1( 0.0, ECN )

      return
      end




*     Store the values of all the variables input for the first calculation ...
*     The original values can then always replace values changed for repeat runs ...

      subroutine set
      include 'montecp.for'

      new = 1

      cofc1x = cofc1
      coff2x = coff2
      cofc5x = cofc5

      xrfm = rfm
      xrf5 = rf5
*     xreg = reg
*     xadd = add
      xfsh = fsh
*     ishhx = ifsh
      xrcm = rcm
      xrcs = rcs
      xefm = efm
      xefs = efs
      xecm = ecm
      xecs = ecs
      xecv = ecv
      xtarg = targ

      sndvx = sndv

      tcm = 0.0
      tcs = 0.0
      tcxx = 0.0

      IDIL = 0
      KDIL = 0

      return
      end


      subroutine write the heading
      character *1 a1(2)
      character *2 bh,bmin,bday,bmon

      call GETDAT ( IYR, IMON, IDAY ) ! get the date
      call GETTIM ( IHR, IMIN, ISEC, IHUN ) ! get the time

      write(bh,2)IHR
    2 format(i2)
      read(bh,1)a1(1),a1(2)
    1 format(2a1)
      if ( a1(1) .eq. ' ' ) a1(1) = '0'
      write(bh,1)a1(1),a1(2)
 
      write(bmin,2)IMIN
      read(bmin,1)a1(1),a1(2)
      if ( a1(1) .eq. ' ' ) a1(1) = '0'
      write(bmin,1)a1(1),a1(2)

      write(bday,2)iday
      read(bday,1)a1(1),a1(2)
      if ( a1(1) .eq. ' ' ) a1(1) = '0'
      write(bday,1)a1(1),a1(2)

      write(bmon,2)imon
      read(bmon,1)a1(1),a1(2)
      if ( a1(1) .eq. ' ' ) a1(1) = '0'
      write(bmon,1)a1(1),a1(2)

      write(3,3)bday,bmon,IYR,BH,bmin
    3 format(68('=')/'Mass Balance Calculation ...',24x,
     &'Date: ',a2,'/',a2,'/',I4,/
     &'VERSION 4.4 (Tony Warn 27/12/16)',25x,
     &'Time: ',a2,'.',a2)
      
      return
      end

      
      
      
      subroutine set up data (vrfm,vrf5,vrcm,vrcs,vefm,vefs,vecm,vecs,
     &vforw,vtarg,vxper,vsens,vadd,vreg,vrgm,vrgs,vrcns,vecns,vfsh,
     &viregq,vcofc1,vcoff2,vcofc5,vtype,vcopt,vcoat,vcoft,vcotf,vsensa,
     &vphm,vphs,vphns,vtm,vts,valkm,valks,valkns,vtdsm,vtdss,vtdsns,
     &vdom,vdos,vdons,vfree)
      
      include 'montecp.for'
      
      integer vforw,viregq,vmsg1,vmsg2,vmsg3,vsens,vmsg4,vtype,vfree
      integer vrcns,vecns,vphns,valkns,vtdsns,vdons,vsensa
      character*150 vrcfile,vrffile,veffile,vecfile,vidfile

      rfm = vrfm
      rf5 = vrf5
      rcm = vrcm
      rcs = vrcs
      efm = vefm
      efs = vefs
      ecm = vecm
      ecs = vecs
      ecv = ecs/ecm
      forw = vforw
      targ = vtarg
      xper = vxper
      sens = vsens
      add = vadd
      reg = vreg
      rgm = vrgm
      rgs = vrgs

      rcns = vrcns
      ecns = vecns
      tcns = (ecm*efm*ecns+rcm*rfm*rcns)/(ecm*efm+rcm*rfm)
     
      fsh = vfsh
      iregq = viregq
     
      cofc1 = vcofc1
      coff2 = vcoff2
      cofc5 = vcofc5

      type = vtype

      copt = vcopt ! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      coat = vcoat
      coft = vcoft
      cotf = vcotf
      sensa = vsensa
      
     
      RS = -2379
      RA = -7765
      RB = -8322 ! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      
      nss = 5000
      rs = -2379

      nprc = 0
      nprf = 0
      npef = 0
      npec = 0

!      rffile=vrffile
!      rcfile=vrcfile
!      effile=veffile
!      ecfile=vecfile
  
      x1 = 0.0
      x2 = 0.0
      x3 = 0.0
      x4 = 0.0
      x5 = 0.0
      x6 = 0.0
      x7 = 0.0
      x8 = 0.0
      x9 = 0.0
      x10 = 0.0
      x11 = 0.0
      
*     open(unit=4,file="FILES.TMP",status="UNKNOWN")
*	  read(4,40)vrffile
*	  read(4,40)vrcfile
*	  read(4,40)veffile
*	  read(4,40)vecfile
*	  read(4,40)vidfile
*  40 format(A150)
*	  close(4)
*     vtype = 0: MC or NP with a percentile target
*     vtype = 1: MC or NP with a mean target
*     vtype = 2: IN with a percentile target
*     vtype = 3: UT with a percentile target (convert to 0, with iutflag = 1)

      if ( NS .lt. 12 ) then ! nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
      read(rffile(1:1),'(a1)')zed
      if ( zed .ne. " " ) then
      call rfnprd
      endif
      read(rcfile(1:1),'(a1)')zed
      if ( zed .ne. " " ) then
      call rcnprd
      endif
      read(effile(1:1),'(a1)')zed
      if ( zed .ne. " ") then
      call efnprd
      endif
      read(ecfile(1:1),'(a1)')zed
      if ( zed .ne. " ") then
      call ecnprd
      endif
      
      if (msg4 .gt. 0) then
      vmsg4 = msg4 
      return
      endif  
      endif ! nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn

      return
      end
      
      
      subroutine zero flow (vsens,vsensa,vforw,vrcml,vrcmu,vrcsl,
     &vrcsu,vrcq,
     &vrcql,vrcqu,vtcm,vtcml,vtcmu,vtcs,vtcsl,vtcsu,vtcq,vtcql,vtcqu,
     &vtcns,vuam,vuaml,vuamu,vuas,vuasl,vuasu,vuaxx,vfam,vfaml,vfamu,
     &vfas,vfasl,vfasu,vfaxx,vecm,vecml,vecmu,vecs,vecsl,vecsu,vec95,
     &vec95l,vec95u,vec99,vec99l,vec99u,vec995,vec995l,vec995u,
     &vtecm,vtecml,vtecmu,vtecs,vtecsl,vtecsu,vte95,vte95l,vte95u,vte99,
     &vte99l,vte99u,vte995,vte995l,vte995u)

      include 'montecp.for'
      
      vsens = 0
      vsensa = 0
      vforw = 1
      call CONAV (rcm,rcs,rcns,rcml,rcmu)
      call CONSDEV (rcs,rcns,rcsl,rcsu)
      call LNCL (rcm,rcs,rcns,xper,rcq,rcql,rcqu,0) 
      
      vrcml = rcml
      vrcmu = rcmu
      vrcsl = rcsl
      vrcsu = rcsu
      vrcq = rcq
      vrcql = rcql
      vrcqu = rcqu
      
      call CONAV (ecm,ecs,ecns,ecml,ecmu)
      call CONSDEV (ecs,ecns,ecsl,ecsu)
      call LNCL (ecm,ecs,ecns,95.0,ec95,ec95l,ec95u,0) 
      call LNCL (ecm,ecs,ecns,99.0,ec99,ec99l,ec99u,0) 
      call LNCL (ecm,ecs,ecns,99.5,ec995,ec995l,ec995u,0) 
      
      vtcm = rcm
      vtcml = rcml
      vtcmu = rcmu
      vtcs = rcs
      vtcsl = rcsl
      vtcsu = rcsu
      vtcq = rcq
      vtcql = rcql
      vtcqu = rcqu
      vtcns = rcns
      
      vuam = uam ! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      vuaml = uaml ! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      vuamu = uamu ! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      vuas = uas
      vuasl = uasl
      vuasu = uasu
      vuaxx = uaq

      vfam = rcm ! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      vfaml = rcml ! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      vfamu = rcmu ! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      vfas = rcs
      vfasl = rcsl
      vfasu = rcsu
      vfaxx = rcq

      vecm = ecm
      vecml = ecml
      vecmu = ecmu
      vecs = ecs
      vecsl = ecsl
      vecsu = ecsu
      vec95 = ec95
      vec95l = ec95l
      vec95u = ec95u
      vec99 = ec99
      vec99l = ec99l
      vec99u = ec99u
      vec995 = ec995
      vec995l = ec995l
      vec995u = ec995u
      
      vtecm = ecm
      vtecml = ecml
      vtecmu = ecmu
      vtecs = ecs
      vtecsl = ecsl
      vtecsu = ecsu
      vte95 = ec95
      vte95l = ec95l
      vte95u = ec95u
      vte99 = ec99
      vte99l = ec99l
      vte99u = ec99u
      vte995 = ec995
      vte995l = ec995l
      vte995u = ec995u
      
      close(03)
      close(09)
      
      return
      end
