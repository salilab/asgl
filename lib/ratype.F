      subroutine atmtype(natm,atmnam,vdwatm,nvdwtyp,iattyp,svdwcnt,
     -                   brkatm,groatm,iresatm,resnam,natmdc,mapres,
     -                   ndcres,nres)
        implicit none
#include "io.cst"
        integer ndcres, natm, nvdwtyp, iattyp(natm), natmdc(ndcres)
        integer iresatm(natm)
        integer iatm,irest,ibrk,ifind2word,istr2int,mapres,nres
        real svdwcnt(nvdwtyp)
        character atmnam(natm)*(*),vdwatm(nvdwtyp)*(*)
        character resnam(nres)*(*),groatm(mapres,ndcres)*(*)
        character brkatm(mapres,ndcres)*(*)
        logical ishydrogen
        external ishydrogen

C ----- associate the GROMOS (vdwatm) type with every atom in the list ;
        do 200  iatm = 1, natm

c ------- residue type index
          irest = istr2int(resnam(iresatm(iatm)))
          if (irest .eq. 0) then
            write(iolog,'(2a)') 
     -      'atmtype_W> residue type not recognized (set to 1): ', 
     -                  resnam(iresatm(iatm))
            irest = 1
          end if

c ------- PDB residue atom type index:
          ibrk=ifind2word(atmnam(iatm),brkatm(1,irest),natmdc(irest))
          if (ibrk .gt. 0) then
c --------- GROMOS atom type index
            iattyp(iatm)=ifind2word(groatm(ibrk,irest),vdwatm,nvdwtyp)
          else
            if (ishydrogen(atmnam(iatm))) then
c ----------- GROMOS atom type index
              iattyp(iatm)=ifind2word('H',vdwatm,nvdwtyp)
            else
              write(iolog,'(2a/a,2i4,1x,a)')
     -        'atmtype_W> PDB atom not in GRO<->BRK file: ',
     -        atmnam(iatm),
     -        '           ires,irest,resnam: ', 
     -        iresatm(iatm),irest,resnam(iresatm(iatm))
              iattyp(iatm)=0
            end if
          end if

          if (iattyp(iatm).eq.0) then
            write(iolog,190)atmnam(iatm),iresatm(iatm),
     -                  resnam(iresatm(iatm)),svdwcnt(1)
190         format('atmtype__W> Atom type not found in VDW.LIB: '/
     -      '    Atom, residue in BRK file  : ',a4,2x,i3,1x,a3/
     -      '    Radius arbitrarilly set to : ', f6.2/)
            iattyp(iatm) = 1
          end if

200     continue

        return
      end


cf ----------------------------------------------------------------------------
cf
cf    ISHYDROGEN function returns .T. if the atmnam is the PDB hydrogen
cf    atom code (if the first non-blank character is 'H').
cf
cf    logical function ishydrogen(atmnam)
cf
cf ----------------------------------------------------------------------------

      logical function ishydrogen(atmnam)
        implicit none
        character atmnam*(*), atdum*(5)

        atdum = atmnam 
        call ljust(atdum)
        if (atdum(1:1) .eq. 'H') then
          ishydrogen = .true.
        else
          if (index('1234567890',atmnam(1:1)).gt.0.and.
     -        atdum(2:2).eq.'H') then
            ishydrogen = .true.
          else
            ishydrogen = .false.
          end if
        end if

        return
      end
