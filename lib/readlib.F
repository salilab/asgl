      subroutine rdrtlin(line,natmcns,ibnd,dcatm,mcnsres,matmcns,idctyp,
     -                   imthtyp,imodal,iftyp,iphytyp)
        implicit none
        integer maxbuf
        parameter (maxbuf=30)
        integer ia, ic, ibnd, nbuffr
        integer mcnsres,matmcns,iftyp,iphytyp,imthtyp,imodal
        integer natmcns,nc,ibuffr
        integer idctyp(4,mcnsres)
        character dcatm(matmcns,mcnsres)*(*), line*(*)
        character buffr(maxbuf)*4

        call str_sn3(line, buffr, maxbuf, nbuffr)
        if(mod(nbuffr,natmcns).ne.0)
     -     stop'Error[rdrtlin]; format error 1.'
        nc = nbuffr / natmcns
        if((ibnd+nc).gt.mcnsres) then
          write(*,*)
     -      'Error[rdrtlin];increase mcnsres; ibnd,nbuffr,natmcns,',
     -      'mcnsres: ', ibnd,nbuffr,natmcns,mcnsres
          stop
        end if
        ibuffr = 0
        do  ic = 1, nc
          ibnd = ibnd + 1
          idctyp(1,ibnd) = imthtyp
          idctyp(2,ibnd) = imodal
          idctyp(3,ibnd) = iftyp
          idctyp(4,ibnd) = iphytyp
          do  ia = 1, natmcns
            ibuffr = ibuffr + 1
            dcatm(ia,ic) = buffr(ibuffr)
          end do
        end do

        return
      end


      subroutine scalvdw(vdwcnt,nvdwtyp,vdwfact,svdwcnt,rvdw2)
        implicit none
        integer nvdwtyp,i
        real vdwfact, vdwcnt(nvdwtyp), svdwcnt(nvdwtyp), rvdw2

        rvdw2 = -1.0
        do  i = 1, nvdwtyp
          svdwcnt(i) = vdwfact*vdwcnt(i)
          if (svdwcnt(i) .gt. rvdw2) rvdw2 = svdwcnt(i)
        end do
        rvdw2 = 2.0*rvdw2

        return
      end


      subroutine rdwilmot(ioinp, fnwilmot, wilmot, iwilmot)
        implicit none
        integer ioinp,i,j,iwilmot(36,36)
        character wilmot(36,36)*1, fnwilmot*(*)
        character dummy*1

        call openf(ioinp, fnwilmot, 'old')
        read(ioinp, 10) dummy
10      format(a)
        do  i = 1, 36
          read(ioinp, 20) (wilmot(j,i),j=1,36)
20        format(2x,36a1)
        end do
        do  i = 1, 36
          do  j = 1, 36
            if (wilmot(j,i) .eq. 'A') iwilmot(j,i) = 1
            if (wilmot(j,i) .eq. 'B') iwilmot(j,i) = 2
            if (wilmot(j,i) .eq. 'P') iwilmot(j,i) = 3
            if (wilmot(j,i) .eq. 'L') iwilmot(j,i) = 4
            if (wilmot(j,i) .eq. 'G') iwilmot(j,i) = 5
            if (wilmot(j,i) .eq. 'E') iwilmot(j,i) = 6
            if (wilmot(j,i) .eq. ' ') iwilmot(j,i) = 7
          end do
        end do

c ----- quick check
        if (wilmot(1,1) .ne. 'B') stop 'wilmot(1,1) <> B'
        if (wilmot(36,36) .ne. 'E') stop 'wilmot(36,36) <> E'

        close(ioinp)
        return
      end


c --- read the atom names of pseudo atoms:
      subroutine rdpsdlib(ioinp,fpsdlib,psdalib,npsdalib,mpsdalib)
        implicit none
        integer npsdalib,mpsdalib,ioinp,i
        character fpsdlib*(*), psdalib(mpsdalib)*(*)

        call openf(ioinp, fpsdlib, 'old')
        read(ioinp, *) npsdalib
        if (npsdalib .gt. mpsdalib) 
     -     stop 'Error[rpsdlib]; increase mpsdalib.'
        do  i = 1, npsdalib
          read(ioinp, '(a4)') psdalib(i)
        end do
        close(ioinp)

        return
      end


c --- atom names are returned left-justified!
      subroutine readvdw(ioinp,vdwfnam,vdwatm,vdwcnt,nvdwtyp,mattyp,
     -                   polar,electr)
        implicit none
        integer ioinp,nvdwtyp,mattyp,ii
        real vdwcnt(mattyp),polar(mattyp),electr(mattyp),wghtatm
        character vdwatm(mattyp)*(*), vdwfnam*(*)
        character line*80

c ----- read the van der Waals table file
        call openf(ioinp, vdwfnam, 'old')
c ----- '*' is a comment character
        nvdwtyp = 0
10      read(ioinp, '(a)', end=100) line
          if (line(1:1) .eq. '*') go to 10
          nvdwtyp = nvdwtyp + 1
          if (nvdwtyp .gt. mattyp) 
     -       stop 'Error[readvdw]; increase mattyp.'
          read(line, '(1x,i4,1x,a4,1x,f8.4,1x,2f7.4,f8.4)')
     -       ii,vdwatm(nvdwtyp),wghtatm,vdwcnt(nvdwtyp),polar(nvdwtyp),
     -       electr(nvdwtyp)
          call ljust(vdwatm(nvdwtyp))
          if (ii.ne.nvdwtyp) then
            write(*,*) 'Error[readvdw]; format error; ii<>nvdwtyp: ',
     -                 ii,nvdwtyp
            stop
          end if
          go to 10
100     continue
        close(ioinp)

        write(*,'(a,i4)')
     -  'readvdw__> van der Waals contact radii library read in; natm: '
     -  ,nvdwtyp
        return
      end


      subroutine getlnrd(scale,vdwcnt,polar,electr,aij,bij,nvdwtyp,
     -                   mattyp)
        implicit none

        real small
        parameter (small=1.0E-6)
        real pi, eps0, echrg, planck, emass, avogdr
        parameter (
c --- pi:
     -          pi      =  3.1415926,
c --- permitivity of vacuum [F/m = C/V/m] * 10^12:
     -          eps0    =  8.854187818,
c --- electron charge [C=Amp s] * 10^19:
     -          echrg   =  1.6021892,
c --- Planck constant / (2pi) [Js = Nms = kg m^2 s^2] * 10^34
     -          planck  =  1.0545887,
c --- Avogadro number [/mol]i * 10^-23
     -          avogdr  =  6.022045,
c --- electron rest mass [kg] * 10^31:
     -          emass   =  9.109534)
          
        integer nvdwtyp,mattyp,i,j
        real vdwcnt(nvdwtyp), electr(nvdwtyp), polar(nvdwtyp)
        real scale, aij(mattyp,mattyp), bij(mattyp,mattyp)

        if (scale .gt. small) then
          do  i = 1, nvdwtyp 
            do  j = 1, nvdwtyp 
c --------  - [kJ * A^6 / mol]:
              bij(i,j)=10.0**3.5*(avogdr*(1.5*sqrt(1.0/(4.0*pi*eps0))* 
     -           echrg*planck/sqrt(emass)*(polar(i)*polar(j)))) /
     -           (sqrt(polar(i)/electr(i))+sqrt(polar(j)/electr(j)))
              aij(i,j) = 0.5*bij(i,j)*(vdwcnt(i)+vdwcnt(j))**6
              bij(i,j) = bij(i,j) / scale
              aij(i,j) = aij(i,j) / scale
            end do
          end do
        end if

        return
      end


c -- returns Aij and Bij in [kJ x] from sqrt(Ai) and sqrt(Bi) in [sqrt(kcal x)]
c     (GROMOS and MODELLER x is [Ang^6/mol])
      subroutine lnrdij(scale,alnrd,blnrd,nvdwtyp,aij,bij,mattyp)
        implicit none
        real small
        parameter (small=1.0E-6)
        integer mattyp,nvdwtyp,i,j
        real alnrd(nvdwtyp), blnrd(nvdwtyp), joules, scale
        real aij(mattyp,mattyp), bij(mattyp,mattyp)

        if (scale .gt. small) then
          do  i = 1, nvdwtyp
            do  j = 1, nvdwtyp
              aij(i,j) = alnrd(i)*alnrd(j)
              bij(i,j) = blnrd(i)*blnrd(j)
c --------- from kcal to kJ and scale (divide, because this is energy):
              aij(i,j) = joules(aij(i,j))/scale
              bij(i,j) = joules(bij(i,j))/scale
            end do
          end do
        end if

        return
      end



C --- the format for the standard residue library is as for the BRK file,
C     except that lines beginning with '*' are comments and that before
C     every residue entry there is a record: [residue name, number of atoms]
C     in a format A3, 2X, I3 ;

      subroutine readxyz(ioinp,xyzfnam,nxyzres,xlibnam,
     -                   xlibatm,xyzlib,nxyzlib,mapres,mrestyp)
        implicit none
        integer mrestyp,nxyzres,mapres,nxyzlib(mrestyp),ioinp,iatm
        real xyzlib(3,mapres,mrestyp),chrglib
        character xyzfnam*(*), xlibnam(mrestyp)*(*)
        character xlibatm(mapres,mrestyp)*(*)
        character card*54

        call openf(ioinp, xyzfnam, 'old')
        nxyzres = 0
10      read(ioinp, '(a54)', end=100)card
          if(card(1:1) .ne. '*') then
            nxyzres = nxyzres + 1
            read(card,'(a3,2x,i3)')xlibnam(nxyzres), nxyzlib(nxyzres)
            do  iatm = 1, nxyzlib(nxyzres)
              read(ioinp,'(12x, a4, 14x, 3f8.3, 6x, f8.3)')
     -             xlibatm(iatm,nxyzres),xyzlib(1,iatm,nxyzres),
     *             xyzlib(2,iatm,nxyzres),xyzlib(3,iatm,nxyzres),
     -             chrglib
            end do
          end if
          go to 10
100     continue

        close(ioinp)

        write(*,*)'Standard residues library read in; nres: ', nxyzres
        return
      end


c --- read charges from the same library file as the residue XYZ coordinates
      subroutine rdchrg(ioinp,xyzfnam,nxyzres,xlibnam,
     -                  xlibatm,chrglib,nxyzlib,mapres,mrestyp)
        implicit none
        integer mrestyp,nxyzres,mapres,nxyzlib(mrestyp),ioinp,iatm
        real chrglib(mapres,mrestyp)
        real xlib,ylib,zlib,chrg
        character xyzfnam*(*), xlibnam(mrestyp)*(*)
        character xlibatm(mapres,mrestyp)*(*)
        character card*54

        call openf(ioinp, xyzfnam, 'old')
        nxyzres = 0
10      read(ioinp, '(a54)', end=100)card
          if(card(1:1) .ne. '*') then
            nxyzres = nxyzres + 1
            read(card,'(a3,2x,i3)')xlibnam(nxyzres), nxyzlib(nxyzres)
            chrg = 0.0
            do  iatm = 1, nxyzlib(nxyzres)
              read(ioinp,'(12x, a4, 14x, 3f8.3, 6x, f8.3)')
     -             xlibatm(iatm,nxyzres),xlib,ylib,zlib,
     -             chrglib(iatm,nxyzres)
              chrg = chrg + chrglib(iatm,nxyzres)
            end do
c            write(*,'(a,i3,1x,a3,1x,f7.2)') 
c     -      'Index, Residue, Net charge: ',nxyzres,xlibnam(nxyzres),chrg
          end if
          go to 10
100     continue

        close(ioinp)

        write(*,*)'Standard charges library read in; nres: ', nxyzres
        return
      end



      subroutine rdfeats(ioinp,matfnam,im2s,
     -                   ncnsprm,ncnsatm,csrnam,
     -                   nmthcns,nfeats,nphycns,
     -                   mmthcns,mfeats,mphycns)
        implicit none
        integer mmthcns,mphycns,nphycns,mfeats,nmthcns,nfeats,ioinp,ii,i
        integer ncnsprm(mmthcns), ncnsatm(mfeats), im2s(mmthcns)
        character matfnam*(*), ch*1, csrnam(mphycns)*(*)

        call openf(ioinp, matfnam, 'old')

        read(ioinp, *) nmthcns
        if (nmthcns .gt. mmthcns) then
          write(*,*) 
     -       'Error[rdfeats]; nmthcns > mmthcns (increase mmthcns): ',
     -       nmthcns, mmthcns
          stop
        end if

        read(ioinp, *) nfeats
        if (nfeats .gt. mfeats) then
          write(*,*) 
     -       'Error[rdfeats]; nfeats > mfeats (increase mfeats): ',
     -       nfeats, mfeats
          stop
        end if

        read(ioinp, *) nphycns
        if (nphycns .gt. mphycns) then
          write(*,*)
     -      'Error[rdfeats]; nphycns > mphycns (increase mphycns): ',
     -             nphycns, mphycns
          stop
        end if

        read(ioinp, '(a)') ch
        do  i = 1, nmthcns
          read(ioinp, *) ii, ncnsprm(i), im2s(i)
        end do

        read(ioinp, '(a)') ch
        do  i = 1, nfeats
          read(ioinp, *) ii, ncnsatm(i)
        end do

        read(ioinp, '(a)') ch
        do  i = 1, nphycns
          read(ioinp, '(5x,a)') csrnam(i)
        end do

        close(ioinp)
        return 
      end

      subroutine rdmnch(ioinp,mnchf,wmnch,rmnchm,stdvm,nmnch,maxmnch,
     -                  mrestyp)
        implicit none
        real eps
        parameter (eps = 1.0E-4)
        integer ioinp,nmnch,maxmnch,mrestyp,i,j,nrestyp
        real wmnch(maxmnch,mrestyp),rmnchm(maxmnch,2),stdvm(maxmnch,2)
        real sum
        character mnchf*(*), ch*1

        call openf(ioinp, mnchf, 'old')
        read(ioinp, '(a)') ch
        read(ioinp, *) nrestyp, nmnch
        if (nrestyp .ne. 20) 
     -  write(*,*)'Warning[rdmnch]: are you sure NRESTYP <> 20 is ok?'
        read(ioinp, '(a/a)') ch,ch
        do  i = 1, nmnch
          read(ioinp,*) rmnchm(i,1),rmnchm(i,2),stdvm(i,1),stdvm(i,2)
        end do
        read(ioinp, '(a/a)') ch,ch
        do  i = 1, nrestyp
          read(ioinp,*) (wmnch(j,i),j=1,nmnch)
          sum = 0.0
          do  j = 1, nmnch
            sum = sum + wmnch(j,i)
          end do
          if (abs(sum-1.0) .gt. eps)
     -      write(*,*) 'Warning[rdmnch]; sum <> 1; irest: ',sum,i
          do  j = 1, nmnch
            wmnch(j,i) = wmnch(j,i) / sum
          end do
        end do
        close(ioinp)

        return
      end


      subroutine rdsdch(ioinp,sdchf,wsdch,sdchm,stdvs,nsdch,maxsdch,
     -                  mrestyp)
        implicit none
        real eps
        parameter (eps=1.0E-3)
        integer nsdch,ioinp,maxsdch,mrestyp,i,j,ichr2int,nrestyp
        real wsdch(maxsdch,mrestyp),sdchm(maxsdch,2,mrestyp)
        real stdvs(maxsdch,2,mrestyp),sum
        character sdchf*(*), ch*1

        call openf(ioinp, sdchf, 'old')
        read(ioinp, '(a)') ch
        read(ioinp,*) nrestyp, nsdch
        if (nrestyp .ne. 20) 
     -  write(*,*)'Warning[rdsdch]: are you sure NRESTYP <> 20 is ok?'
        read(ioinp, '(a/a)') ch,ch
        do  i = 1, nrestyp
          read(ioinp, '(a)') ch
          if (ichr2int(ch) .ne. i) then
            write(*,*) 
     -      'Error[rdsdch]; SDCH LIB format error; wrong residue ',
     -      'order: ', ch
            stop
          end if
          read(ioinp,*) (sdchm(j,1,i),j=1,nsdch)
          read(ioinp,*) (stdvs(j,1,i),j=1,nsdch)
          read(ioinp,*) (sdchm(j,2,i),j=1,nsdch)
          read(ioinp,*) (stdvs(j,2,i),j=1,nsdch)
        end do

        read(ioinp, '(a/a)') ch,ch
        do  i = 1, nrestyp
          read(ioinp,*) (wsdch(j,i),j=1,nsdch)
          sum = 0.0
          do  j = 1, nsdch
            sum = sum + wsdch(j,i)
          end do
c          if (abs(sum-1.0) .gt. eps)
c     -      write(*,*) 'Warning[rdsdch]; sum <> 1; irest: ',sum,i
          do  j = 1, nsdch
            wsdch(j,i) = wsdch(j,i) / sum
          end do
        end do
        close(ioinp)

        return
      end


c --- Reads GROMOS IFP37C4.DAT file.
c 
c --- Input: ioinp       ... io channel
c            ifpfil      ... IFP filename
c            mcnslib     ... maximal no of constraints of all types
c            mattyp      ... maximal no of GROMOS atom types
c            matmcns     ... maximal no of atoms per constraint
c            mmthcns     ... maximal no of math types of constraints
c            mprmcns     ... maximal no of parameters per constraint
c 
c --- Output: natmlib    ... no of GROMOS atom types
c             atmlib(i)  ... names of GROMOS atom types
c             ndccns(i)  ... no of constraints of mathematical type i
c             idccns(i)  ... starting index of constraints of math type i
c             indifp(i,j) .. atom type indices of constr i
c             dcprm(i,j) ... i=2 - force const; i=1 - mean, of constr j
c
c --- Three types of constraints are read in:
c
c     math multi feat phys    description   # atms    # parms (mean,st dev)
c        3     1    1    1    bond length        2          2
c        3     1    2    2    bond angle         3          2
c        3     1    3    3    dihedral angle     2          2
c
c --- Note: dihedral angle has only 1,4 atoms specified

      subroutine rdifp(ioinp,ifpfil,mcnslib,mattyp,idcbeg,ndcbeg,indifp,
     -                 dcprm,atmlib,natmlib,matmcns,mphycns,mprmcns)
        implicit none
        integer mphycns,matmcns,mcnslib,mprmcns,ioinp,i
        integer ndcbeg(mphycns),idcbeg(mphycns),indifp(matmcns,mcnslib)
        integer mattyp,natmlib,k,ii
        real dcprm(mprmcns,mcnslib),wghtlib
        character ifpfil*(*), atmlib(mattyp)*(*)
        character line*(80)

        do  i = 1, mphycns
          idcbeg(i) = 0
          ndcbeg(i) = 0
        end do

        call openf(ioinp, ifpfil, 'old')

c ----- header
        read(ioinp, '(a)') line
c ----- number of atoms in the library:
        read(ioinp, *) natmlib
        if (natmlib.gt.mattyp) stop'Error[rdifp]; increase mattyp.'
        do  i = 1, natmlib
c ------- GROMOS atom name (right justified), its mass
          read(ioinp, '(6x,a4,1x,f8.4)') atmlib(i), wghtlib
          call ljust(atmlib(i))
c ------- comment
          read(ioinp, '(a)') line
        end do
c ----- for each constraint type, how many different combinations of atom
c       types are there:
        read(ioinp, *) ndcbeg(1), ndcbeg(2), ndcbeg(3)
        if (ndcbeg(1)+ndcbeg(2)+ndcbeg(3) .gt. mcnslib) 
     -    stop'Error[rdifp]; increase mcnslib.'
c ----- parameters:
        idcbeg(1) = 1
        do  i = idcbeg(1), idcbeg(1)+ndcbeg(1)-1
          read(ioinp,'(3i5,2f10.5)')ii,(indifp(k,i),k=1,2),
     -                              (dcprm(k,i),k=2,1,-1)
        end do
        idcbeg(2) = ndcbeg(1) + 1
        do  i = idcbeg(2), idcbeg(2)+ndcbeg(2)-1
          read(ioinp,'(4i5,2f10.5)')ii,(indifp(k,i),k=1,3),
     -                              (dcprm(k,i),k=2,1,-1)
        end do
        idcbeg(3) = ndcbeg(1) + ndcbeg(2) + 1
        do  i = idcbeg(3), idcbeg(3)+ndcbeg(3)-1
          read(ioinp,'(3i5,2f10.5)')ii,(indifp(k,i),k=1,2),
     -                              (dcprm(k,i),k=2,1,-1)
        end do

        close(ioinp)

        return
      end


      subroutine rdlnrd(ioinp,ifplib,mattyp,alnrd,blnrd,natmlib)
        implicit none
        integer ioinp,i,i1,i2,i3,i4,natmlib,mattyp
        real alnrd(mattyp),blnrd(mattyp)
        character ifplib*(*)
        character ch*1

        call openf(ioinp, ifplib, 'old')

        read(ioinp, '(a)') ch
        read(ioinp, *) natmlib
        if (natmlib.gt.mattyp) stop'Error[rdlnrd]; increase mattyp.'
        do  i = 1, 2*natmlib
          read(ioinp, '(a)') ch
        end do
        read(ioinp, *) i1,i2,i3,i4
        do  i = 1, i1+i2+i3+i4
          read(ioinp,'(a)') ch
        end do
        do  i = 1, natmlib
          read(ioinp, '(10x,f7.2,f9.1)') blnrd(i), alnrd(i)
          read(ioinp,'(a)') ch
        end do

        close(ioinp)

        return
      end


      subroutine rdgrbrk(ioinp,grmbrkf,groatm,brkatm,mrestyp,mapres,
     -                   nresdc,natmdc,resdc)
        implicit none
        integer mrestyp,ioinp,nresdc,mapres
        integer natmdc(mrestyp),ia,ir
        character groatm(mapres,mrestyp)*(*), brkatm(mapres,mrestyp)*(*)
        character line*(14),resdc(mrestyp)*(*),grmbrkf*(*)

c ----- format: RESI record is the first record for the residue, then
c       ATOM records follow with no other records until the end of the
c       residue, then at least one non-RESI/ATOM record and then RESI
c       record for another residue.

        call openf(ioinp, grmbrkf, 'old')
c ----- '*' is a comment character
        nresdc = 0
10      read(ioinp, '(a)', end=100) line
          if (line(1:5) .ne. 'RESI ') go to 10
          nresdc = nresdc + 1
          if (nresdc.gt.mrestyp)stop 'Error[rdgrbrk] increase mrestyp.'
          resdc(nresdc) = line(6:8)
          natmdc(nresdc) = 0
20        read(ioinp, '(a)', end=100) line
            if (line(1:5) .ne. 'ATOM ') go to 10
            natmdc(nresdc) = natmdc(nresdc) + 1
            if (natmdc(nresdc) .gt. mapres) 
     -        stop 'Error[rdgrbrk] increase mapres.'
            brkatm(natmdc(nresdc),nresdc) = line(6:9)
            groatm(natmdc(nresdc),nresdc) = line(11:14)
          go to 20
100     continue

        close(ioinp)

        do  ir = 1, nresdc
          do  ia = 1, natmdc(ir)
            call ljust(groatm(ia,ir))
            call ljust(brkatm(ia,ir))
          end do
        end do

        write(*,'(a,i4)') 
     -  'rdgrbrk__> GROMOS <=> BROOKHAVEN library read in: ',nresdc

        return
      end

      subroutine rdrt(ioinp,rtfil,ndcres,dcres,ndccns,idctyp,
     -                dcatm,mcnsres,mrestyp,matmcns)
        implicit none
        integer lenlin
        parameter (lenlin=80)
        integer matmcns,mrestyp,mcnsres,ndcres,ioinp,ndccns(mrestyp)
        integer idctyp(4,mcnsres,mrestyp)
        character dcatm(matmcns,mcnsres,mrestyp)*(*)
        character dcres(mrestyp)*(*),rtfil*(*)
        character line*(lenlin)

        ndcres = 0
        call openf(ioinp, rtfil, 'old')

c----- format: RESI record followed by BOND/THET/IMPH records, non-interrupted,
c      then any other non-RESI/BOND/THET/IMPH record (at least one) and then 
c      another RESI again.
10      read(ioinp, '(a)', end=100, err=888)line
          if (line(1:5) .ne. 'RESI ') go to 10
          ndcres = ndcres + 1
          dcres(ndcres) = line(6:8)
          ndccns(ndcres) = 0 
20        read(ioinp, '(a)', end=100, err=888) line
            if (line(1:5) .eq. 'BOND ') then
              call rdrtlin(line(6:lenlin),2,ndccns(ndcres),
     -                     dcatm(1,ndccns(ndcres)+1,ndcres),mcnsres,
     -                     matmcns,idctyp(1,1,ndcres),3,1,1,1)
c ----------- next line of the same residue
              go to 20           
            else
              if (line(1:5) .eq. 'THET ') then
                call rdrtlin(line(6:lenlin),3,ndccns(ndcres),
     -                       dcatm(1,ndccns(ndcres)+1,ndcres),mcnsres,
     -                       matmcns,idctyp(1,1,ndcres),3,1,2,2)
c ------------- next line of the same residue
                go to 20         
              else
                if (line(1:5) .eq. 'IMPH ') then
                   call rdrtlin(line(6:lenlin),4,ndccns(ndcres),
     -                       dcatm(1,ndccns(ndcres)+1,ndcres),mcnsres,
     -                       matmcns,idctyp(1,1,ndcres),3,1,3,3)
c ---------------- next line of the same residue
                   go to 20 
                else
c --------------- new residue
                   go to 10     
                end if
              end if
            end if
100     continue

        close(ioinp)
      
        write(*,*)'A priori distance constraints library read in;nres: '
     -            ,ndcres
        return

888     write(*,*) 'Error[rdrt]; reading file: ', rtfil
        stop
      end


      subroutine rd2sdch(ioinp,dihfnam,nscrot,iscrot,nchi,chiwgh,
     -           chiavr,chistd,maxchi,mrestyp,mscrot,nlsdch,degr)
        implicit none
        integer mrestyp,mscrot,maxchi,nscrot(mrestyp)
        integer iscrot(4,mscrot,mrestyp),nlsdch
        integer nchi(mscrot,mrestyp),i,j,k,ioinp
        real chiwgh(maxchi,mscrot,mrestyp),radians
        real chiavr(maxchi,mscrot,mrestyp)
        real chistd(maxchi,mscrot,mrestyp)
        character dihfnam*(*),ch*1
        logical degr

        call openf(ioinp, dihfnam, 'old')
        read(ioinp, '(a)') ch
        read(ioinp, *) nlsdch
        if (nlsdch.gt.mrestyp) stop 'Error[rd2sdch]; increase mrestyp'
        read(ioinp, '(a)') ch
        do  i = 1, nlsdch
          read(ioinp,*) nscrot(i)
          if(nscrot(i).gt.mscrot)stop'Error[rd2sdch]; increase mscrot'
          do  j = 1, nscrot(i)

            read(ioinp,*) (iscrot(k,j,i),k=1,4), 
     -                    nchi(j,i),
     -                    (chiwgh(k,j,i),k=1,min(maxchi,nchi(j,i))), 
     -                    (chiavr(k,j,i),k=1,min(maxchi,nchi(j,i))), 
     -                    (chistd(k,j,i),k=1,min(maxchi,nchi(j,i)))

            if (nchi(j,i) .gt. maxchi-1) 
     -        stop 'Error[rd2sdch]; increase maxchi'

c --------- convert to radians
            if (.not. degr) then
              do  k = 1, nchi(j,i)
                chiavr(k,j,i) = radians(chiavr(k,j,i))
                chistd(k,j,i) = radians(chistd(k,j,i))
              end do
            end if

c --------- for the purposes of constr5() in GETCSR (to allow use of
c           mdt files with undefined bin) add another undefined
c           class to the end of each dihedral angle; but nchi still
c           counts only defined optima!
            do  k = nchi(j,i)+1, maxchi
              chiwgh(k,j,i) = 999.0
              chiavr(k,j,i) = 999.0
              chistd(k,j,i) = 999.0
            end do

          end do
        end do
        close(ioinp)

        return
      end


c --- reads the library of all dihedral angles for each residue type
c
      subroutine rdrdih(ioinp,dihfnam,nrdih,iatdih,ndopt,dihwgh,
     -           dihavr,dihstd,maxrdih,mrestyp,maxdopt,nresdih,degr)
        implicit none
        real eps
        parameter (eps=0.01)
        integer mrestyp,maxrdih,maxdopt,nrdih(mrestyp)
        integer iatdih(4,maxrdih,mrestyp),nresdih
        integer ndopt(maxrdih,mrestyp),i,j,k,ioinp
        real dihwgh(maxdopt,maxrdih,mrestyp),radians
        real dihavr(maxdopt,maxrdih,mrestyp),sum
        real dihstd(maxdopt,maxrdih,mrestyp)
        character dihfnam*(*),ch*1
        logical degr

        call openf(ioinp, dihfnam, 'old')
 10     read(ioinp, '(a)') ch
        if (ch .ne. '-') go to 10
        read(ioinp, *) nresdih
        if (nresdih.gt.mrestyp) stop 'Error[rdrdih]; increase mrestyp'
        do  i = 1, nresdih
          read(ioinp,*) nrdih(i)
          if(nrdih(i).gt.maxrdih)stop'Error[rdrdih]; increase maxrdih'
          do  j = 1, nrdih(i)

            read(ioinp,*) (iatdih(k,j,i),k=1,4), 
     -                    ndopt(j,i),
     -                    (dihwgh(k,j,i),k=1,min(maxdopt,ndopt(j,i))), 
     -                    (dihavr(k,j,i),k=1,min(maxdopt,ndopt(j,i))), 
     -                    (dihstd(k,j,i),k=1,min(maxdopt,ndopt(j,i)))

            if (ndopt(j,i) .gt. maxdopt-1) 
     -        stop 'Error[rdrdih]; increase maxdopt'

c --------- convert to radians
            if (.not. degr) then
              do  k = 1, ndopt(j,i)
                dihavr(k,j,i) = radians(dihavr(k,j,i))
                dihstd(k,j,i) = radians(dihstd(k,j,i))
              end do
            end if

c --------- quick check
            sum = 0.0
            do  k = 1, ndopt(j,i)
              sum = sum + dihwgh(k,j,i)
            end do
            if (abs(sum-1.0) .gt. eps) then
              write(*,*) 'Warning[rdrdih]; weights do nut sum to 1: ',
     -                   sum
              write(*,*) '      idih,ires,wgh1: ',j,i,dihwgh(1,j,i)
            end if

c --------- for the purposes of constr5() in GETCSR (to allow use of
c           the mdt files with undefined bin) add another undefined
c           class to the end of each dihedral angle; but ndopt still
c           counts only the defined optima!
            do  k = ndopt(j,i)+1, maxdopt
              dihwgh(k,j,i) = 0.0
              dihavr(k,j,i) = 999.0
              dihstd(k,j,i) = 999.0
            end do

          end do
        end do
        close(ioinp)

        return
      end

