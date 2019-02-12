*/*
* CSCI3180 Principles of Programming Languages
*
* --- Declaration ---
*
* I declare that the assignment here submitted is original except for source
* material explicitly acknowledged. I also acknowledge that I am aware of
* University policy and regulations on honesty in academic work, and of the
* disciplinary guidelines and procedures applicable to breaches of such policy
* and regulations, as contained in the website
* http://www.cuhk.edu.hk/policy/academichonesty/
*
* Assignment 1
* Name : SETO TSZ KIN
* Student ID : 1155092585
* Email Addr : 1155092585@link.cuhk.edu.hk
**/
      PROGRAM ASG1
      IMPLICIT NONE
      CHARACTER etxt*50, mtxt*50
      integer aRecTemp, aRecCount, aRec(10000,2)
      CHARACTER atxt*50
      common /coeff/ aRecTemp, aRecCount, aRec, atxt
      integer empnum, empsal
      character*10 emp1, line
      character*20 emp2
      character*21 emptemp
      character*3  empdep
      common /emprec/ empnum, empsal, emp1, emp2, emptemp, empdep
      integer atdabs, atdsus, atdtoprec, atdlate, atdover
      common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
      integer sumwritten
      common /sumstatus/ sumwritten
      integer suscount, abscount, latecount, precount
      common /counter/ suscount, abscount, latecount, precount
      integer bigflag, bigflag2
      common /bigloopflag/ bigflag
      integer io32, io31, io34, io33
      common /ios/ io32, io31, io34, io33
      integer atdm, atdy, my, mm, nmflag
      common /newm/ atdm, atdy, my, mm, nmflag
*Variable end
*vaiable initialize
      nmflag = 0
      bigflag = 0
      bigflag2 = 0
      atdabs = 0
      atdsus = 0
      atdtoprec = 0
      suscount = 0
      abscount = 0
      latecount = 0
      precount = 0
      io31 = 0
      io32 = 0
      io33 = 0
      io34 = 0
*vaiable initialize end
      CALL GETARG(1,etxt)
      CALL GETARG(2,atxt)
      CALL GETARG(3,mtxt)
      open(unit=31, file=etxt,
     &access='sequential', action='read')
      open(unit=32, file=atxt,
     &access='sequential', action='read')
      open(unit=33, file=mtxt,
     &access='sequential', action='read')
      open(unit=34, file='temp.txt',
     &access='sequential', action='write')
      CALL preporcess1
      CALL sortattandance
      close(32)
      open(unit=32, file=atxt,
     &access='sequential', action='read')
      close(34)
      open(unit=34, file='temp.txt',
     &access='sequential', action='read')
      read(34, '(A)') line
      open(unit=35, file='monthly-attendancefor.txt',
     &access='sequential', action='write')
      open(unit=36, file='summaryfor.txt',
     &access='sequential', action='write')
      CALL preporcess2
 101  if(io31 .eq. 0) read(31, 10, iostat=io31)
     &empnum, emp1, emp2, emptemp, empdep,empsal
 10   FORMAT(I4, A10, A20, A21, A3, I6)
      if (io31 .eq. 0) atdlate = 0
      if (io31 .eq. 0) bigflag2 = 1
      if (io31 .eq. 0) atdover = 0
      if (io31 .eq. 0) atdtoprec = 0
      if (io31 .eq. 0) sumwritten = 0
      if (io31 .eq. 0) CALL updateatd
      if (io31 .eq. 0) GOTO 101
      bigflag = 1
      atdlate = 0
      atdover = 0
      atdtoprec = 0
      sumwritten = 0
      if(bigflag2 .eq. 1)CALL updateatd
 103  CALL proprocess
      close(31)
      close(32)
      close(33)
      close(34)
      close(35)
      close(36)
      STOP
      END
***********************************************************************
*0th----preporcess
      SUBROUTINE preporcess1
        integer io32, io31, io34, io33
        common /ios/ io32, io31, io34, io33
        character temp, temp2
        integer atdm, atdy, my, mm, nmflag
        common /newm/ atdm, atdy, my, mm, nmflag
        read(32, 10, iostat=io32) atdy, temp, atdm, temp2, atdd
 10     FORMAT(I4, A, I2, A, I2)
        write(34, 20) atdy, temp, atdm, temp2, atdd
        write(*, 20) atdy, temp, atdm, temp2, atdd
 20     FORMAT(I4.4, A, I2.2, A, I2.2, '\r')
      END
      SUBROUTINE preporcess2
        integer year, month, date
        CHARACTER hi, hi2
        common /prepro2/  year, month, date, hi, hi2
        CHARACTER hi
        integer io32, io31, io34, io33
        common /ios/ io32, io31, io34, io33
        integer atdm, atdy, my, mm, nmflag
        common /newm/ atdm, atdy, my, mm, nmflag
* Read and write first line of monthly attendance
        read(33, 10, iostat=io33) my, hi, mm
 10     FORMAT(I4, A, I2)
        if (mm .ne. atdm) nmflag = 1
        if (mm .ne. atdm) mm = atdm
        if (my .ne. atdy) my = atdy
        write(35, 30) my, hi, mm
 30     FORMAT(I4.4, A, I2.2, '\r')
* Read and first line of attendance
        read(32, 20, iostat=io32) year, hi, month, hi2, date
 20     format(I4, A, I2, A, I2)
        CALL genDate
        call writesumheader
      END
      subroutine genDate
        integer year, month, date
        CHARACTER hi, hi2
        common /prepro2/  year, month, date, hi, hi2
        integer monthsize
        CHARACTER dateword*24, monthword*9
        common /sumhead/ monthsize, dateword, monthword
        if (month .eq. 1) goto 101
        if (month .eq. 2) goto 102
        if (month .eq. 3) goto 103
        if (month .eq. 4) goto 104
        if (month .eq. 5) goto 105
        if (month .eq. 6) goto 106
        if (month .eq. 7) goto 107
        if (month .eq. 8) goto 108
        if (month .eq. 9) goto 109
        if (month .eq. 10) goto 110
        if (month .eq. 11) goto 111
        if (month .eq. 12) goto 112
 101    monthword = 'January'
        monthsize = 7
        goto 30
 102    monthword = 'February'
        monthsize = 8
        goto 30
 103    monthword = 'March'
        monthsize = 5
        goto 30
 104    monthword = 'April'
        monthsize = 4
        goto 30
 105    monthword = 'May'
        monthsize = 3
        goto 30
 106    monthword = 'June'
        monthsize = 4
        goto 30
 107    monthword = 'July'
        monthsize = 4
        goto 30
 108    monthword = 'August'
        monthsize = 6
        goto 30
 109    monthword = 'September'
        monthsize = 9
        goto 30
 110    monthword = 'October'
        monthsize = 7
        goto 30
 111    monthword = 'November'
        monthsize = 8
        goto 30
 112    monthword = 'December'
        monthsize = 8
        goto 30
* 30     write(* , '(A)') 'finish switch'
*        write(* , 20) monthword, date, year
 30     if (date .lt. 10) write(dateword, 10 ) 
     &monthword(1:monthsize), date, year
        if (date .Ge. 10) write(dateword, 20 )
     &monthword(1:monthsize), date, year
 10     format('Date: ',A, ' ', I1,', ',I4)
 20     format('Date: ',A, ' ', I2,', ',I4)
      END
      subroutine writesumheader
        integer year, month, date
        CHARACTER hi, hi2
        common /prepro2/  year, month, date, hi, hi2
        integer monthsize
        CHARACTER dateword*24, monthword*9
        common /sumhead/ monthsize, dateword, monthword
        write(36, 10) dateword
 10     format('Daily Attendance Summary\r',/,A,'\r',/,
     &'Staff-ID Name                            Department Status\r',/,
     &'--------------------------------------------------------------',
     &'\r')
      END
*0th----preporcess END
***********************************************************************
***********************************************************************
*1st----sortattandance
      SUBROUTINE sortattandance
        integer aRecTemp, aRecCount, aRec(10000,2)
        CHARACTER atxt*50
        common /coeff/ aRecTemp, aRecCount, aRec, atxt
        integer io32, io31, io34, io33
        common /ios/ io32, io31, io34, io33
        aRecCount = 1
 101    if (io32 .eq. 0) read(32, 10, iostat=io32) aRecTemp
 10     FORMAT(I4)
        if (io32 .eq. 0) CALL loadAttandance
        if (io32 .eq. 0) GOTO 101
        CALL loadAttandance
        aRecCount = aRecCount - 1
        CALL sort
        CALL writesorted
      end

      SUBROUTINE loadAttandance
        integer aRecTemp, aRecCount, aRec(10000,2)
        CHARACTER atxt*50
        common /coeff/ aRecTemp, aRecCount, aRec, atxt
        aRec(aRecCount, 1) = aRecTemp
        aRec(aRecCount, 2) = aRecCount
        aRecCount = aRecCount + 1
      end

      subroutine sort
        integer aRecTemp, aRecCount, aRec(10000,2)
        CHARACTER atxt*50
        common /coeff/ aRecTemp, aRecCount, aRec, atxt
        integer icount, loopcond, jcount
        common /sortloop/ icount, loopcond, jcount
        icount = 0
        loopcond = aRecCount - 1
 10     if (icount .lt. loopcond) CALL sortinner
        icount = icount + 1
        if (icount .lt. loopcond) GOTO 10
      END

      subroutine sortinner
        integer aRecTemp, aRecCount, aRec(10000,2)
        CHARACTER atxt*50
        common /coeff/ aRecTemp, aRecCount, aRec, atxt
        integer icount, loopcond, jcount
        common /sortloop/ icount, loopcond, jcount
        integer inloopcond
        jcount = 0
        inloopcond = loopcond - icount - 1
 10     if (jcount .lt. loopcond) CALL sortcond
        jcount = jcount + 1
        if (jcount .lt. loopcond) GOTO 10
      END

      subroutine sortcond
        integer aRecTemp, aRecCount, aRec(10000,2)
        CHARACTER atxt*50
        common /coeff/ aRecTemp, aRecCount, aRec, atxt
        integer icount, loopcond, jcount
        common /sortloop/ icount, loopcond, jcount
        integer index
        index = jcount + 1
        if (aRec(index,1) .gt. aRec(index+1,1))
     &  call swap(index,index+1)
      END

      subroutine swap(ifirst,isecond)
        integer aRecTemp, aRecCount, aRec(10000,2)
        CHARACTER atxt*50
        common /coeff/ aRecTemp, aRecCount, aRec, atxt
        integer atemp(1,2)
        atemp(1,1) = aRec(ifirst,1)
        atemp(1,2) = aRec(ifirst,2)
        aRec(ifirst,1) = aRec(isecond,1)
        aRec(ifirst,2) = aRec(isecond,2)
        aRec(isecond,1) = atemp(1,1)
        aRec(isecond,2) = atemp(1,2)
      END

      subroutine writesorted
        integer aRecTemp, aRecCount, aRec(10000,2)
        CHARACTER atxt*50
        common /coeff/ aRecTemp, aRecCount, aRec, atxt
        integer icount
        icount = 0
 10     if (icount .lt. aRecCount) CALL findwrite(icount + 1)
        icount = icount + 1
        if (icount .lt. aRecCount) GOTO 10
      END

      subroutine findwrite(idfind)
        integer aRecTemp, aRecCount, aRec(10000,2)
        CHARACTER atxt*50
        common /coeff/ aRecTemp, aRecCount, aRec, atxt
        CHARACTER line*26, firstline*10
        integer itemp
        integer io32, io31, io34, io33
        common /ios/ io32, io31, io34, io33
        idfind = aRec(idfind,2)
        itemp = 1
        close(32)
        open(unit=32, file=atxt,access='sequential', action='read')
        read(32, 10, iostat=io32) firstline
*       write(*, '(A)') firstline
 40     if (itemp .gt. idfind) goto 30
 20     if(io31 .eq. 0) read(32, 10, iostat=io32) line
 10     FORMAT(A)
        itemp = itemp + 1
*        write(*, '(A)') line
*        write(*, '(I4, I4, i4)') itemp, idfind, io32
        goto 40
 30     write(34, 90) line
 90     FORMAT(A,'\r')
      end
*1st----sortattandance end
***********************************************************************
***********************************************************************
*2nd----update attendance
      subroutine updateatd
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        integer atdemp1, atdhr1, atdmin1
        character*6 atdio1
        character atddate1*11, colon1
        common /atdrec1/atdemp1,atdhr1,atdmin1,atdio1,atddate1,colon1
        integer atdemp2, atdhr2, atdmin2
        character*6 atdio2
        character atddate2*11, colon2
        common /atdrec2/atdemp2,atdhr2,atdmin2,atdio2,atddate2,colon2
        if ((atdabs .eq. 0 ) .and. (atdsus .eq. 0))
     &call upatdrec
        atdabs = 0
        if (atdsus .eq. 1) CALL swapatdrec
        atdsus = 0
        call atdchk
        call atdcal
      end
      subroutine swapatdrec
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        integer atdemp1, atdhr1, atdmin1
        character*6 atdio1
        character atddate1*11, colon1
        common /atdrec1/atdemp1,atdhr1,atdmin1,atdio1,atddate1,colon1
        integer atdemp2, atdhr2, atdmin2
        character*6 atdio2
        character atddate2*11, colon2
        common /atdrec2/atdemp2,atdhr2,atdmin2,atdio2,atddate2,colon2
        atdemp1 = atdemp2
        atdhr1 = atdhr2
        atdmin1 = atdmin2
        atdio1 = atdio2
        atddate1 = atddate2
        colon1 = colon2
        call upatdrec2
      end
      subroutine upatdrec
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        integer atdemp1, atdhr1, atdmin1
        character*6 atdio1
        character atddate1*11, colon1
        common /atdrec1/atdemp1,atdhr1,atdmin1,atdio1,atddate1,colon1
        integer atdemp2, atdhr2, atdmin2
        character*6 atdio2
        character atddate2*11, colon2
        common /atdrec2/atdemp2,atdhr2,atdmin2,atdio2,atddate2,colon2
        call upatdrec1
        call upatdrec2
      end
      subroutine upatdrec1
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        integer atdemp1, atdhr1, atdmin1
        character*6 atdio1
        character atddate1*11, colon1
        common /atdrec1/atdemp1,atdhr1,atdmin1,atdio1,atddate1,colon1
        integer atdemp2, atdhr2, atdmin2
        character*6 atdio2
        character atddate2*11, colon2
        common /atdrec2/atdemp2,atdhr2,atdmin2,atdio2,atddate2, colon2
        integer bigflag
        common /bigloopflag/ bigflag
        integer io32, io31, io34, io33
        common /ios/ io32, io31, io34, io33
* 200    if(bigflag .eq. 0) read(34,10,iostat=io34)
 200     if(io34 .eq. 0) read(34,10,iostat=io34)
     &atdemp1,atdio1,atddate1,atdhr1,colon1,atdmin1
 10     format(I4,A6,A11,I2,A,I2)
        if ((io34 .eq. 0) .and. (atdtoprec .ne. 0) .and.
     & (atdemp1 .eq. atdemp2)) goto 200
        if (atdtoprec .eq. 0) atdtoprec = 1
      end
      subroutine upatdrec2
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        integer atdemp1, atdhr1, atdmin1
        character*6 atdio1
        character atddate1*11, colon1
        common /atdrec1/atdemp1,atdhr1,atdmin1,atdio1,atddate1,colon1
        integer atdemp2, atdhr2, atdmin2
        character*6 atdio2
        character atddate2*11, colon2
        common /atdrec2/atdemp2,atdhr2,atdmin2,atdio2,atddate2, colon2
        integer bigflag
        common /bigloopflag/ bigflag
        integer io32, io31, io34, io33
        common /ios/ io32, io31, io34, io33
* 200    if(bigflag .eq. 0) read(34,10,iostat=io34)
 200     if(io34 .eq. 0) read(34,10,iostat=io34)
     &atdemp2,atdio2,atddate2,atdhr2,colon2,atdmin2
 10     format(I4,A6,A11,I2,A,I2)
        if ((io34 .eq. 0) .and. (atdemp1 .eq. atdemp2) .and.(atdio2 .eq.
     &  'ARRIVE')) goto 200
      end
      subroutine atdchk
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        integer atdemp1, atdhr1, atdmin1
        character*6 atdio1
        character atddate1*11, colon1
        common /atdrec1/atdemp1,atdhr1,atdmin1,atdio1,atddate1,colon1
        integer atdemp2, atdhr2, atdmin2
        character*6 atdio2
        character atddate2*11, colon2
        common /atdrec2/atdemp2,atdhr2,atdmin2,atdio2,atddate2,colon2
        integer empnum, empsal
        character*10 emp1
        character*20 emp2
        character*21 emptemp
        character*3  empdep
        common /emprec/ empnum, empsal, emp1, emp2, emptemp, empdep
*        write(*, '(A,I4,A10,A20)') 'emp: ',empnum, emp1, emp2
*        write(*, '(A,I4,I4,I4)') 'atd1: ',atdemp1, atdhr1, atdmin1
*        write(*, '(A,I4,I4,I4)') 'atd2: ',atdemp2, atdhr2, atdmin2
        if ((empnum .ne. atdemp2) .and. (empnum .ne. atdemp1))
     &atdabs = 1
        if ((atdabs .ne. 1) .and. (atdemp2 .ne. atdemp1))
     &atdsus = 1
      end
*2nd----update attendance
***********************************************************************
***********************************************************************
*3rd----calculate attendance
      subroutine atdcal
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        if ((atdabs .eq. 0) .and. (atdsus .eq. 1)) goto 200
        if ((atdabs .eq. 0) .and. (atdsus .eq. 0)) goto 300
        if (atdabs .eq. 1) goto 400
 200    call calsus
        goto 999
 300    call callate
        call calover
        goto 999
 400    call calabs
 999    call makeatd
      end
      subroutine calabs
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        atdlate = 0
        atdover = 0
      end
      subroutine calsus
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        atdlate = 0
        atdover = 0
      end
      subroutine callate
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        integer atdemp1, atdhr1, atdmin1
        character*6 atdio1
        character atddate1*11, colon1
        common /atdrec1/atdemp1,atdhr1,atdmin1,atdio1,atddate1,colon1
        atdlate = ((atdhr1 - 10) * 60 + atdmin1 ) / 15
        end
      subroutine calover
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        integer atdemp2, atdhr2, atdmin2
        character*6 atdio2
        character atddate2*11, colon2
        common /atdrec2/atdemp2,atdhr2,atdmin2,atdio2,atddate2,colon2
        atdover = atdhr2 - 17
      end
*3rd----calculate attendance
***********************************************************************
***********************************************************************
*4th----make attendance
      subroutine makeatd
        call readmonth
        call writeatd
        call makesum
      end
      subroutine readmonth
        integer monthemp, monthabs, monthlate, monthover
        common /monthstatus/ monthemp, monthabs, monthlate, monthover
        integer bigflag
        common /bigloopflag/ bigflag
        integer io32, io31, io34, io33
        common /ios/ io32, io31, io34, io33
        integer atdm, atdy, my, mm, nmflag
        common /newm/ atdm, atdy, my, mm, nmflag
*        if(bigflag .eq. 0) 
*     &read(33, 10) monthemp, monthabs, monthlate, monthover
*        write(*, 10) monthemp, monthabs, monthlate, monthover, io33
        if(io33 .eq. 0) 
     &read(33, 10, iostat=io33) monthemp,monthabs,monthlate,monthover
*        write(*, 10) monthemp, monthabs, monthlate, monthover, io33
        if (nmflag .eq. 1) monthabs = 0
        if (nmflag .eq. 1) monthlate = 0
        if (nmflag .eq. 1) monthover = 0
 10     format(I4, I3, I3, I3)
 20     format(I4, I3, I3, I3, I3)
      end
      subroutine writeatd
        integer monthemp, monthabs, monthlate, monthover
        common /monthstatus/ monthemp, monthabs, monthlate, monthover
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        monthlate = monthlate + atdlate
        monthabs = monthabs + atdabs
        monthover = monthover + atdover
        if(monthover .gt. 30) monthover = 30
        write(35, 10) monthemp, monthabs, monthlate, monthover
 10     format(I4.4, I3.3, I3.3, I3.3,'\r')
      end
*4th----make attendance
***********************************************************************
***********************************************************************
*5th----GENERATE SUMMARY
      subroutine makesum
        call writesum
      end
      subroutine writesum
        integer empnum, empsal
        character*10 emp1
        character*20 emp2
        character*21 emptemp
        character*3  empdep
        common /emprec/ empnum, empsal, emp1, emp2, emptemp, empdep
        integer sumwritten
        common /sumstatus/ sumwritten
        character*4 sumemp
        character*10 sumname1
        character*20 sumname2
        character*3 sumdep
        character*10 sumstate
        common /sumrec/ sumemp, sumname1, sumname2, sumdep, sumstate
        integer atdabs, atdsus, atdtoprec, atdlate, atdover
        common /atdstatus/ atdabs, atdsus, atdtoprec, atdlate, atdover
        integer suscount, abscount, latecount, precount
        common /counter/ suscount, abscount, latecount, precount
*vvvvvvvvvv                vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
*vvvvvvvvvv                vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
*vvvvvvvvvv cobol line 539 vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
*vvvvvvvvvv                vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
*vvvvvvvvvv                vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        write(sumemp, '(I4)') empnum
        sumname1 = emp1
        sumname2 = emp2
        sumdep = empdep
        if ((sumwritten .eq. 0) .and. (atdsus .ne. 0))
     &sumstate = 'SUSPICIOUS'
        if ((sumwritten .eq. 0) .and. (atdsus .ne. 0))
     &suscount = suscount + 1
        if ((sumwritten .eq. 0) .and. (atdsus .ne. 0)) sumwritten = 1
        if ((sumwritten .eq. 0) .and. (atdlate .ne. 0))
     &sumstate = 'LATE      '
        if ((sumwritten .eq. 0) .and. (atdlate .ne. 0))
     &latecount = latecount + 1
        if ((sumwritten .eq. 0) .and. (atdlate .ne. 0)) sumwritten = 1
        if ((sumwritten .eq. 0) .and. (atdabs .ne. 0))
     &sumstate = 'ABSENT    '
        if ((sumwritten .eq. 0) .and. (atdabs .ne. 0))
     &abscount = abscount + 1
        if ((sumwritten .eq. 0) .and. (atdabs .ne. 0)) sumwritten = 1
        if (sumwritten .eq. 0) sumstate = 'PRESENT   '
        if (sumwritten .eq. 0) precount = precount + 1
        if (sumwritten .eq. 0) sumwritten = 1
        write(36, 10) sumemp, sumname1, sumname2, sumdep, sumstate
 10     format(A4,5X,A10,1X,A20,1X,A3,8X,A10,'\r')
      end
*5th----GENERATE SUMMARY
***********************************************************************
***********************************************************************
*999th----proprocess
      subroutine proprocess
        integer suscount, abscount, latecount, precount
        common /counter/ suscount, abscount, latecount, precount
        write(36, 10) precount, abscount, latecount, suscount
 10     format('------------------------------------------------------',
     &'--------\r',/,'Number of Presences: ', I4,'\r',/,
     &'Number of Absences: ', I4,'\r',/,
     &'Number of Late Arrivals: ', I4,'\r',/,
     &'Number of Suspicious Records: ', I4,'\r')
      end
*999th----proprocess
***********************************************************************
