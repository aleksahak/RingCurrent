!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copyright (C) 2009+ Aleksandr B. Sahakyan (aleksahak[at]cantab.net).         !
!                                                                              !
! License: You may redistribute this source code (or its components) and/or    !
!   modify/translate it under the terms of the GNU General Public License      !
!   as published by the Free Software Foundation; version 2 of the License     !
!   (GPL2). You can find the details of GPL2 in the following link:            !
!   https://www.gnu.org/licenses/gpl-2.0.html                                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE datatypes
  USE definitions, ONLY : PR
  IMPLICIT NONE
  SAVE
  
  TYPE RINGDATA
    REAL(KIND=PR)                 :: distanceCOR
    REAL(KIND=PR), DIMENSION(1:3) :: query_xyz
    REAL(KIND=PR), DIMENSION(1:3) :: Centered0RealNormal
    REAL(KIND=PR), DIMENSION(1:3) :: Normal_E
    REAL(KIND=PR), DIMENSION(1:3) :: Normal_rev_E
    REAL(KIND=PR), DIMENSION(1:3) :: COR
    REAL(KIND=PR)                 :: Theta
    REAL(KIND=PR), DIMENSION(1:3) :: Project
    REAL(KIND=PR)                 :: gfPople
  END TYPE RINGDATA
  

END MODULE datatypes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

