      subroutine setdrad(atrad,natm,iattyp,nvdwtyp,vdwcnt,scale)
        implicit none
        integer i, natm, nvdwtyp, iattyp(natm)
        real vdwcnt(nvdwtyp), atrad(natm), scale

        do  i = 1, natm
          atrad(i) = scale*vdwcnt(iattyp(i))
        end do

        return
      end

      subroutine setrad(atrad,natm,isel,numsel,radius,scale)
        implicit none
        integer i, natm, numsel,isel(numsel), ia
        real atrad(natm), radius, scale

        do  i = 1, numsel
          ia = isel(i)
          atrad(ia) = scale*radius
        end do

        return
      end


      subroutine setifeat2(acolor,natm,color)
        implicit none
        integer natm, ia
        integer acolor(natm), color

        do  ia = 1, natm
          acolor(ia) = color
        end do

        return
      end

      subroutine setifeat(acolor,natm,isel,numsel,color)
        implicit none
        integer i, natm, numsel,isel(numsel), ia
        integer acolor(natm), color

        do  i = 1, numsel
          ia = isel(i)
          acolor(ia) = color
        end do

        return
      end

      subroutine setrfeat2(acolor,natm,color)
        implicit none
        integer natm, ia
        real acolor(natm), color

        do  ia = 1, natm
          acolor(ia) = color
        end do

        return
      end


      subroutine setrfeat(acolor,natm,isel,numsel,color)
        implicit none
        integer i, natm, numsel,isel(numsel), ia
        real acolor(natm), color

        do  i = 1, numsel
          ia = isel(i)
          acolor(ia) = color
        end do

        return
      end

c --- set the colors and line types of atoms to default
      subroutine setdcol(acolor,ialine,natm,iattyp,nvdwtyp,atypc,
     -           iatypl,natm2)
        implicit none
        integer i, natm, nvdwtyp, iattyp(natm), iatypl(nvdwtyp)
        integer ialine(natm),natm2
        real acolor(natm),atypc(nvdwtyp)

        do  i = natm2+1, natm
          acolor(i) = atypc(iattyp(i))
          ialine(i) = iatypl(iattyp(i))
        end do

        return
      end


     
c --- label the selected atoms, according to the parts string: 
      subroutine labatm(natm,isel,numsel,atmnam,resnam,resnum,
     -                  nres,iresatm,parts,nparts,labls)
        implicit none 
        integer lenpart
        character delim*1
c ----- maximal length of a part, delimiter between parts:
        parameter (lenpart=20, delim = ' ')
        integer nres, natm, numsel, isel(numsel), i, ia
        integer iresatm(natm),nparts, l, indexw
        character labls(numsel)*(*),atmnam(natm)*(*)
        character resnam(nres)*(*), resnum(nres)*(*)
        character parts(nparts)*(*), part*(lenpart), dpart*(lenpart)

        do  i = 1, numsel
          ia = isel(i)
  
          call blank(labls(i))
          do  l = 1, nparts
 
            call blank(part)
            if (indexw(parts(l), 'ATOM_INDEX') .gt. 0)
     -        call i_str(ia,part,lenpart)
            if (indexw(parts(l), 'RESIDUE_INDEX') .gt. 0)
     -        call i_str(iresatm(ia),part,lenpart)
            if (indexw(parts(l), 'RESIDUE_NUMBER') .gt. 0)
     -        part = resnum(iresatm(ia))
            if (indexw(parts(l), 'ATOM_TYPE') .gt. 0)
     -        part = atmnam(ia)
            if (indexw(parts(l), 'RESIDUE_TYPE3') .gt. 0)
     -        part = resnam(iresatm(ia))
            if (indexw(parts(l), 'RESIDUE_TYPE1') .gt. 0)
     -        call str2chr(resnam(iresatm(ia)), part(1:1))

            call ljust(part)
            if (l .eq. 1) then 
              labls(i) = part
            else
              dpart = delim // part
              call add2str(labls(i), dpart)
            end if

          end do

        end do

        return
      end 


     
      subroutine pltlab(perspective,xpdb,ypdb,zpdb,natm,isel,
     -           numsel,labls,atrad,iloc,lblfnt)
        implicit none 
        integer natm, numsel, isel(numsel), i, ia, iloc
        integer ihstyl, ivstyl, lblfnt
        real xpdb(natm), ypdb(natm), zpdb(natm), xtxt, ytxt
        real atrad(natm), persp
        character labls(numsel)*(*)
        logical perspective

        do  i = 1, numsel
          ia = isel(i)

          go to (1,2) iloc

c --------- label centered in the atom center
1           continue
            xtxt = xpdb(ia)
            ytxt = ypdb(ia)
            ihstyl = 2
            ivstyl = 2
            go to 10

c --------- label leftjustified next to the atom on its right side
2           continue
            xtxt = xpdb(ia) + 1.2*atrad(ia)
            ytxt = ypdb(ia)
            ihstyl = 1
            ivstyl = 2
            go to 10

10        continue

          if (perspective) then
            xtxt = persp(xtxt, zpdb(ia))
            ytxt = persp(ytxt, zpdb(ia))
          end if
          call text(labls(i),xtxt,ytxt,lblfnt,0.0,ihstyl,ivstyl,.true.)
        end do

        return
      end 


c --- Format of DRAW.LIB:
c
c     number of atom types
c     index,line type,color  .... for all atoms in VDWLIB order, one per line
c     .....................
c     number of bond types
c     index,line type,color  .... for all bonds, one per line
c     .....................
c
      subroutine rddraw(ioinp,drawlib,nvdwtyp,mbndtyp,nbndtyp,
     -           atypc,iatypl,btypc,ibtypl)
        implicit none
        integer i, ioinp, nvdwtyp, mbndtyp, nbndtyp, ndummy
        integer iatypl(nvdwtyp),ibtypl(mbndtyp),indx
        real atypc(nvdwtyp), btypc(mbndtyp)
        character drawlib*(*)

        call openf(ioinp, drawlib, 'old')
        read(ioinp, *) ndummy
        if (ndummy .ne. nvdwtyp) 
     -   stop 'Error[rdraw]; incorrect numb of atm types in DRAW.LIB'
        do  i = 1, nvdwtyp
          read(ioinp, *) indx, iatypl(i), atypc(i)
        end do

        read(ioinp, *) nbndtyp
        if (nbndtyp .gt. mbndtyp) 
     -   stop 'Error[rdraw]; increase MBNDTYP.'
        do  i = 1, nbndtyp
          read(ioinp, *) indx, ibtypl(i), btypc(i)
        end do

        close(ioinp)

        write(*,'(a)') 'rddraw___> molecule drawing library read in'

        return
      end

   
      subroutine bndtyp(ibndtyp,nbndtyp,numbond,nbond2)
        implicit none
        integer i, nbndtyp, numbond, ibndtyp(numbond), nbond2
   
        do  i = nbond2+1, numbond
          ibndtyp(i) = 1
        end do

        return
      end


c --- select a sphere from the atoms currently selected in the
c     selected set of atoms (display,bond,label); add or inititate
c     the selected set of atoms. Center specified by the residue number
c     and atom name.
      subroutine selatm(sphere,x,y,z,cresnum,catmnam,restyp,atmsel,
     -           atmnam,natm,iatmr1,iatmr2,iresatm,resnam,resnum,
     -           nres,isel,numsel,iselb,numbsel,isell,numlsel,
     -           whatfor,istep,mode,dz1,dz2,xtrans,ytrans,
     -           ztrans,whatfrom,search,select,res1,res2,ialine,acolor,
     -           iline,color,vdwcnt,nvdwtyp,radius,radfact,atrad,
     -           iattyp)
        implicit none
        integer maxatm
        parameter (maxatm=5000)
        integer indexw,ia,iresc,iatmc,ifind2word,lenl,iaa,irr,lenr
        integer natm,iatmr1(natm),iatmr2(natm),nres,ir,ns,ierr
        integer iresatm(natm),numsel,isel(natm),istep,iattyp(natm)
        integer ires1,ires2,ires1a,ires2a,istepa,iline,ialine(natm)
        integer iselb(natm),numbsel,isell(natm),numlsel,nvdwtyp
        real color,acolor(natm),radfact,radius,vdwcnt(nvdwtyp)
        real sphere,x(natm),y(natm),z(natm),dist,dz1,dz2
        real xtrans, ytrans, ztrans, spherea,dz1a,dz2a
        real xc,yc,zc,atrad(natm)
        character atmsel*(*),atmnam(natm)*(*),cresnum*(*),catmnam*(*)
        character resnum(nres)*(*),restyp*(*),resnam(nres)*(*)
        character whatfor*(*), whatfor2*(50), mode2*(20), mode*(*)
        character whatfrom*(*), whatfrom2*(50),res1*(*),res2*(*)
        character search*(*), search2*(50),select*(*),select2*(50)
        logical mnch, sdch, sc, mc, all, chosen, allres
        logical picatm(maxatm), picbatm(maxatm), piclatm(maxatm)
        logical selctd(maxatm),resmod,selunsel
        logical addatm, disp, bond, labl, sph
        logical dispfrom, bondfrom, lablfrom, allfrom

c ----- you never know, maybe these strings are values, not a variables

        mode2 = mode(lenl(mode):)
        call upper(mode2)
        whatfor2 = whatfor(lenl(whatfor):)
        call upper(whatfor2)
        disp = (indexw(whatfor2, 'DISPLAYING') .gt. 0)
        bond = (indexw(whatfor2, 'BONDING') .gt. 0)
        labl = (indexw(whatfor2, 'LABELLING') .gt. 0)
        whatfrom2 = whatfrom(lenl(whatfrom):)
        call upper(whatfrom2)
        dispfrom = (indexw(whatfrom2, 'DISPLAYING') .gt. 0)
        bondfrom = (indexw(whatfrom2, 'BONDING') .gt. 0)
        lablfrom = (indexw(whatfrom2, 'LABELLING') .gt. 0)
        allfrom  = (indexw(whatfrom2, 'ALL') .gt. 0)
        search2  = search(lenl(search):)
        call upper(search2)
        sph = (indexw(search2, 'SPHERE') .gt. 0)
        select2 = select(lenl(select):)
        call upper(select2)
        selunsel = (indexw(select2, 'REMOVE') .lt. 1)
        addatm   = (indexw(select2, 'INITIALIZE') .lt. 1)
        if (.not.addatm .and. .not. selunsel) 
     -    write(*,'(a,a)') 'selatm__W> weird SELECTION_STATUS: ',
     -                     'both initialization and removing.'

c ----- flag all atoms from which the selection will be made:
        if (allfrom) then
c ------- all atoms are used for searching for the selected ones:
          do  ia = 1, natm
            selctd(ia) = .true.
          end do
        else
c ------- only the atoms in some subsets will be used:
          do  ia = 1, natm
            selctd(ia) = .false.
          end do
          if (dispfrom) call inisels(selctd,natm,numsel,isel)
          if (bondfrom)  call inisels(selctd,natm,numbsel,iselb)
          if (lablfrom)  call inisels(selctd,natm,numlsel,isell)
        end if

        ns = 0
        do  ia = 1, natm
          if (selctd(ia)) ns = ns + 1
        end do

c ----- now initialize the output selections:
c ----- for all specified selection types:
c          it is zero if addatm=.false., equals input selection otherwise
c          (if a selection type not specified it is not changed at all)
        if (disp) call inisel(addatm,picatm,natm,numsel,isel)
        if (bond) call inisel(addatm,picbatm,natm,numbsel,iselb)
        if (labl) call inisel(addatm,piclatm,natm,numlsel,isell)

c ----- which of the atom sets was selected (MNCH & SDCH = ALL):
        sdch    = (indexw(atmsel,'SDCH') .gt. 0)
        mnch    = (indexw(atmsel,'MNCH') .gt. 0)
        all     = (indexw(atmsel,'ALL') .gt. 0)
        allres  = (indexw(restyp,'ALL') .gt. 0)
c ----- do we add all atoms in the whole residue if only 
c       one of its atoms satisfies the search criteria:
        resmod  = (indexw(mode2, 'RESIDUE') .gt. 0)

        write(*,'(a,2i6)') 
     -  'selatm___> Number of atoms to choose from, total: ',ns,natm
        write(*,'(a,a)')
     -  'selatm___> Atoms are selected from the follow. sets (FROM): ',
     -  whatfrom(1:lenr(whatfrom))
        write(*,'(a,a)')
     -  'selatm___> Atom sets to be modified (FOR): ',
     -  whatfor(1:lenr(whatfor))
        write(*,'(a,a)') 
     -  'selatm___> Atom types to be searched for (ATOM_TYPE): ',
     -  atmsel(1:lenr(atmsel))
        write(*,'(a,a)') 
     -  'selatm___> Residue types to be searched for (RES_TYPE): ',
     -  restyp(1:lenr(restyp))
        write(*,'(a,a)') 
     -  'selatm___> Selection mode (SELECTION_MODE): ', 
     -  mode2(1:lenr(mode2))
        write(*,'(a,a)') 
     - 'selatm___> What to do with atoms & sets (SELECTION_STATUS):', 
     -  select(1:lenr(select))


c ----- find the search range (SEGMENT) or center (SPHERE)

        if (sph) then
c ------- SPHERE:
          if (cresnum .eq. 'INDEX') then
            call str_i(catmnam, iatmc, ierr)
            if (iatmc .gt. 0 .and. iatmc .lt. natm) then
              iresc = iresatm(iatmc)
            else
              iresc = 0
            end if
            if (iresc .eq. 0) then
              write(*,'(2a)') 'selatm__E> center residue not found: ',
     -      cresnum
              stop
            end if
          else
            iresc = ifind2word(cresnum,resnum,nres)
            if (iresc .eq. 0) then
              write(*,'(2a)') 'selatm__E> center residue not found: ',
     -        cresnum
              stop
            end if
            iatmc = ifind2word(catmnam,atmnam(iatmr1(iresc)),
     -                         iatmr2(iresc)-iatmr1(iresc)+1)
            if (iatmc .eq. 0) then
              write(*,'(2a)') 'selatm__E> center atom not found: ',
     -        catmnam
              stop
            end if
            iatmc = iatmr1(iresc) + iatmc - 1
          end if

          write(*,'(a,2i6,2x,2a6)')
     -    'selatm___> SPHERE search; Central atom (2i,2a): ',
     -    iresc,iatmc,cresnum,catmnam
          write(*,'(a,5f7.3)')
     -    'selatm___> Slab, sphere center translation (SLAB): ',
     -    dz1,dz2,xtrans,ytrans,ztrans

c ------- to use the SPHERE search:
          xc = x(iatmc) + xtrans
          yc = y(iatmc) + ytrans
          zc = z(iatmc) + ztrans
          spherea =  sphere
          dz1a    =  dz1
          dz2a    =  dz2

c ------- to neglect the SEGMENT criterion:
          ires1a = 1
          ires2a = nres
          istepa = 1

        else
c ------- SEGMENT:
          ires1 = ifind2word(res1,resnum,nres)
          if (ires1 .eq. 0) ires1 = 1
          ires2 = ifind2word(res2,resnum,nres)
          if (ires2 .eq. 0) ires2 = nres

          write(*,'(a,2i6,2x,2a6)') 
     -    'selatm___> SEGMENT search; residue range (2i5,2a5): ',
     -    ires1,ires2,resnum(ires1),resnum(ires2)
          write(*,'(a,i6)') 
     -    'selatm___> Selection residue step (SELECTION_STEP): ', istep

c ------- to use the SEGMENT search:
          ires1a = ires1
          ires2a = ires2
          istepa = istep 

c ------- to neglect the SPHERE criterion:
          xc = x(1)
          yc = y(1)
          zc = z(1)
          spherea =  1.0E32
          dz1a    = -1.0E32
          dz2a    =  1.0E32
        end if

        if (ires2 .lt. ires1) then
          write(*,'(a,2i6)') 'selatm__E> ires2 < ires1 : ',ires1,ires2
          stop
        end if

c ----- This code is slightly inefficient because the same code is used for 
c       the SEGMENT and SPHERE search mode; it is done by suitably modifying
c       the search criteria (ires1,ires2,sphere radius,slab) to make
c       one or the other criterion transparent. This is possible because
c       it does not make sense to use both at the same time.
        do  ir = ires1a, ires2a, istepa
c ------ is it the right residue type:
         if ((indexw(restyp,resnam(ir)).gt.0) .or. allres) then
          do  ia = iatmr1(ir), iatmr2(ir)
            if (selctd(ia)) then
              if((dist(xc,yc,zc,x(ia),y(ia),z(ia)).lt.spherea).and.
     -          (z(ia).ge.zc+dz1a.and.z(ia).le.zc+dz2a))then
c ------------- what kind of an atom is the ia-th atom:
c ------------- is it a mainchain atom:
                if((atmnam(ia).eq.' O  ').or.(atmnam(ia).eq.' C  ').or.
     *             (atmnam(ia).eq.' CA ').or.(atmnam(ia).eq.' N  '))then
                   mc = .true.
                else
                   mc = .false.
                end if
c ------------- is it a sidechain atom (sidechain is everything which
c               is not mainchain, including HETATM, WATERS, ...)
                sc = .not. mc
c ------------- IS THE IA-TH ATOM NAME ONE OF THE CHOSEN ONES:
                chosen = (all.or.(sc.and.sdch).or.
     -                   (mc.and.mnch).or.
     -                   (indexw(atmsel,atmnam(ia)).gt.0))
c ------------- possibly add it to the selections:
                if (chosen) then
                  if (resmod) then
                   irr = iresatm(ia)
                   do iaa = iatmr1(irr), iatmr2(irr)
                    if (selctd(iaa) .and. disp) then
                      call addsel(picatm,numsel,isel,iaa,natm,selunsel)
                      ialine(ia) = iline
                      acolor(ia) = color
                      if (abs(radius+999.0) .le. 0.001) then
                        atrad(ia) = vdwcnt(iattyp(ia))*radfact
                      else
                        atrad(ia) = radius*radfact
                      end if
                    end if
                    if (selctd(iaa) .and. bond)
     -              call addsel(picbatm,numbsel,iselb,iaa,natm,selunsel)
                    if (selctd(iaa) .and. labl)
     -              call addsel(piclatm,numlsel,isell,iaa,natm,selunsel)
                   end do
                  else
                    if(disp) then
                      call addsel(picatm,numsel,isel,ia,natm,selunsel)
                      ialine(ia) = iline
                      acolor(ia) = color
                      if (abs(radius+999.0) .le. 0.001) then
                        atrad(ia) = vdwcnt(iattyp(ia))*radfact
                      else
                        atrad(ia) = radius*radfact
                      end if
                    end if
                    if(bond)
     -              call addsel(picbatm,numbsel,iselb,ia,natm,selunsel)
                    if(labl)
     -              call addsel(piclatm,numlsel,isell,ia,natm,selunsel)
                  end if
                end if
              end if
            end if
          end do
         end if
        end do

        write(*,'(a,i6)') 
     - 'selatm___> Number of selected atoms in DISPLAYING set: ',numsel
        write(*,'(a,i6)') 
     - 'selatm___> Number of selected atoms in BONDING set   : ',numbsel
        write(*,'(a,i6)') 
     - 'selatm___> Number of selected atoms in LABELLING set : ',numlsel

        return
      end



      subroutine addsel(picatm,numsel,isel,ia,natm,selunsel)
        implicit none
        integer ia,natm,numsel,isel(natm)
        logical picatm(natm),selunsel

        if (.not. picatm(ia)) then
c -------- YES: ADD OR REMOVE IT FROM THE selection
           picatm(ia) = selunsel
           numsel = numsel + 1
           isel(numsel) = ia
        end if

        return
      end


      subroutine inisels(selctd,natm,numsel,isel)
        implicit none
        integer ia, natm, numsel, isel(numsel)
        logical selctd(natm)

c ----- add atoms to the selection for searching:
        do  ia = 1, numsel
          selctd(isel(ia)) = .true.
        end do

        return
      end


      subroutine inisel(addatm,picatm,natm,numsel,isel)
        implicit none
        integer i, natm, numsel, isel(numsel)
        logical picatm(natm), addatm

        do  i = 1, natm
          picatm(i) = .false.
        end do

        if (addatm) then
c ------- add atoms to the current selection
          do  i = 1, numsel
            picatm(isel(i)) = .true.
          end do
        else
c ------- start selection from scratch
          numsel = 0
        end if

        return
      end


c --- LPI-Fortran numerical precision fix:
      real function acosfix(aangle)
        implicit none
        real aangle
        if(aangle .lt. -1.0) aangle = -1.0
        if(aangle .gt.  1.0) aangle =  1.0
        acosfix = acos(aangle)
        return
      end


      subroutine writbrk(ioout,brkname,x,y,z,natm,nres,resnam,
     -                   resnum,atmnam,iresatm)
        implicit none

        integer natm,nres,iresatm(natm),ioout,i
        real x(natm), y(natm), z(natm)
        character resnam(nres)*(*), resnum(nres)*(*)
        character atmnam(natm)*(*), brkname*(*)

        call openf(ioout, brkname, 'unknown')
        do  i = 1, natm
          write(ioout,15) i,atmnam(i),resnam(iresatm(i)),
     *                 resnum(iresatm(i)),x(i),y(i),z(i)
15        format('ATOM  ',i5,1x,a4,1x,a3,2x,a5,3x,3f8.3)
        end do
        close(ioout)

        write(*,30)brkname,nres,natm
30      format('Message[writbrk]; fname, res, atms: ',a25,2i5)

        return
      end
