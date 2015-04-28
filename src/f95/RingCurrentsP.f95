!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copyright (C) 2009+ Aleksandr B. Sahakyan (aleksahak[at]cantab.net).         !
!                                                                              !
! License: You may redistribute this source code (or its components) and/or    !
!   modify/translate it under the terms of the GNU General Public License      !
!   as published by the Free Software Foundation; version 2 of the License     !
!   (GPL2). You can find the details of GPL2 in the following link:            !
!   https://www.gnu.org/licenses/gpl-2.0.html                                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM RingCurrentsP_test

USE ringcurrents, ONLY : RingCurrentsP
USE datatypes,    ONLY : RINGDATA
USE definitions,  ONLY : PR

IMPLICIT NONE
TYPE(RINGDATA) :: R
INTEGER        :: natom, i, narg           
CHARACTER(255) :: arg
REAL(KIND=PR), DIMENSION(1:21) :: inp_array

narg = iargc()

DO i = 1, narg
  CALL getarg(i, arg)
  
  IF(i==1) THEN
    READ(arg,*) natom
  ELSE
    READ(arg,*) inp_array(i-1)
  END IF

END DO

IF(narg/= ((natom+1)*3)+1) THEN
  WRITE(*,*) "ERROR: The number of arguments is not as expected!"
  STOP
END IF
  

  
R = RingCurrentsP(natom, & 
                  inp_array(1:3), &
                  inp_array(4:6),  &
                  inp_array(7:9),  &
                  inp_array(10:12),  &
                  inp_array(13:15),  &
                  inp_array(16:18),  &
                  inp_array(19:21)    )

WRITE (*,*) R%gfPople

END PROGRAM RingCurrentsP_test
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

