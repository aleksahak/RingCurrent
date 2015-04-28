!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copyright (C) 2009+ Aleksandr B. Sahakyan (aleksahak[at]cantab.net).         !
!                                                                              !
! License: You may redistribute this source code (or its components) and/or    !
!   modify/translate it under the terms of the GNU General Public License      !
!   as published by the Free Software Foundation; version 2 of the License     !
!   (GPL2). You can find the details of GPL2 in the following link:            !
!   https://www.gnu.org/licenses/gpl-2.0.html                                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE xyzfun
  USE definitions, ONLY : PR, PI
  IMPLICIT NONE
  SAVE
  
  CONTAINS 
  !#########################################################################
  !## This function calculates the distance between 2 points given the xyz #
  !## coordinates of those points organized in xyz1 and xyz2 vectors.      #
  !#########################################################################
  PURE FUNCTION XYZ_Dist(xyz1, xyz2) 
  IMPLICIT NONE
  REAL(KIND=PR) :: XYZ_Dist
  REAL(KIND=PR), DIMENSION(1:3), INTENT(IN) :: xyz1, xyz2
  
  XYZ_Dist = SQRT( ( (xyz1(1) - xyz2(1))**2 ) + &
                   ( (xyz1(2) - xyz2(2))**2 ) + &
                   ( (xyz1(3) - xyz2(3))**2 )   )
  
  END FUNCTION XYZ_Dist
  
  !#########################################################################
  !##   This function calculates the normalized and centered to 0 vector  ##
  !##  showing the direction. It takes as an argument the xyzB1, xyzE1    ##
  !## beginning and ending coordinates of the vector, centers the vector  ##
  !## translating the beginning to the 0,0,0 coordinate and normalizes by ##
  !## setting the length to 0. As a result, the program returns the xyz   ##
  !##       coordinates of the end point of the transformed vector.       ##
  !#########################################################################
  PURE FUNCTION XYZ_NormCent(xyzB1, xyzE1, Lngth)
  IMPLICIT NONE
  REAL(KIND=PR), DIMENSION(1:3)  :: XYZ_NormCent
  REAL(KIND=PR), DIMENSION(1:3), INTENT(IN) :: xyzB1, xyzE1
  REAL(KIND=PR), INTENT(IN) :: Lngth
  REAL(KIND=PR) :: Lx, Ly, Lz, L
  
  Lx = xyzE1(1) - xyzB1(1)
  Ly = xyzE1(2) - xyzB1(2)
  Lz = xyzE1(3) - xyzB1(3)
  L  = XYZ_Dist(xyzB1,xyzE1)
  
  XYZ_NormCent = (/  (Lx/L)*Lngth, (Ly/L)*Lngth, (Lz/L)*Lngth  /)
  
  END FUNCTION XYZ_NormCent
  
  !#########################################################################
  !##  This function takes the coordinates of the ends of 2 centered and  ##
  !##  normalized vectors (or the components of the i,j,k) and calculates ##
  !##    the end point coordinates of the centerd and normalized normal   ##    
  !##                              vector.                                ##
  !#########################################################################
  PURE FUNCTION XYZ_NormalVector(NCxyzE1, NCxyzE2) 
  IMPLICIT NONE
  REAL(KIND=PR), DIMENSION(1:3)  :: XYZ_NormalVector
  REAL(KIND=PR), DIMENSION(1:3), INTENT(IN) :: NCxyzE1, NCxyzE2
  REAL(KIND=PR) :: x, y, z  
  
  ! x = ay*bz-az*by
  x = (NCxyzE1(2)*NCxyzE2(3)) - (NCxyzE1(3)*NCxyzE2(2))
  ! y = az*bx-ax*bz
  y = (NCxyzE1(3)*NCxyzE2(1)) - (NCxyzE1(1)*NCxyzE2(3))
  ! z = ax*by-ay*bx
  z = (NCxyzE1(1)*NCxyzE2(2)) - (NCxyzE1(2)*NCxyzE2(1))

  XYZ_NormalVector = XYZ_NormCent((/ 0.0_PR,0.0_PR,0.0_PR /), (/ x,y,z /), 1.0_PR) 
  
  END FUNCTION XYZ_NormalVector
 
  !#########################################################################
  !##  This function takes 2 centered and normalized vectors and returns  ##
  !##           the ending coordinates of the 3D averaged vector          ##
  !#########################################################################
  PURE FUNCTION XYZ_Average2Vec(NCxyzE1, NCxyzE2) 
  IMPLICIT NONE
  REAL(KIND=PR), DIMENSION(1:3)  :: XYZ_Average2Vec
  REAL(KIND=PR), DIMENSION(1:3), INTENT(IN) :: NCxyzE1, NCxyzE2
  REAL(KIND=PR) :: aver_x, aver_y, aver_z  

  aver_x = (NCxyzE1(1) + NCxyzE2(1))/2
  aver_y = (NCxyzE1(2) + NCxyzE2(2))/2
  aver_z = (NCxyzE1(3) + NCxyzE2(3))/2
  XYZ_Average2Vec = (/ aver_x, aver_y, aver_z /)

  END FUNCTION XYZ_Average2Vec
  
  !#########################################################################
  !## This function calculates the angle between 2 vectors, having as an  ##
  !## argument the coordinates of the ends of 2 normalized and centered   ##
  !##                 vectors in NCxyzE1 and NCxyzE2 objects              ##
  !## If both cosine and radian are false, the function returns degrees.  ##
  !#########################################################################
  PURE FUNCTION XYZ_AngleNC(NCxyzE1, NCxyzE2, cosine, radian) 
  IMPLICIT NONE
  REAL(KIND=PR) :: XYZ_AngleNC
  REAL(KIND=PR), DIMENSION(1:3), INTENT(IN) :: NCxyzE1, NCxyzE2
  LOGICAL, INTENT(IN) :: cosine, radian
  REAL(KIND=PR) :: COS_AB
  
  COS_AB = &
     ( (NCxyzE1(1)*NCxyzE2(1)) + (NCxyzE1(2)*NCxyzE2(2)) + (NCxyzE1(3)*NCxyzE2(3)) )/ &
     (  ( SQRT( ((NCxyzE1(1))**2)+((NCxyzE1(2))**2)+((NCxyzE1(3))**2) ) )*            &
        ( SQRT( ((NCxyzE2(1))**2)+((NCxyzE2(2))**2)+((NCxyzE2(3))**2) ) )  ) 
        
  ! Design is kept to be compatible with the R code.
  IF((cosine .EQV. .TRUE.) .AND. (radian .EQV. .FALSE.)) THEN
    XYZ_AngleNC = COS_AB
  END IF
  
  IF( (cosine .EQV. .FALSE.) .AND. (radian .EQV. .TRUE.)) THEN
    XYZ_AngleNC = ACOS(COS_AB)
  END IF
  
  IF( (cosine .EQV. .FALSE.) .AND. (radian .EQV. .FALSE.)) THEN
    XYZ_AngleNC = ( ACOS(COS_AB)/PI )*180
  END IF
  
  
  END FUNCTION XYZ_AngleNC
  
  !#########################################################################
  !##   This function takes the beginning and endpoint coordinates of a   ##
  !##  vector and the xyzNewB1 new begining coordinates. It returns the   ##
  !##    ending coordinates of the vector translated to the enterd new    ##
  !##                               beginnig.                             ##
  !#########################################################################
  PURE FUNCTION XYZ_VecTranslate(xyzB1, xyzE1, xyzNewB1) 
  IMPLICIT NONE
  REAL(KIND=PR), DIMENSION(1:3) :: XYZ_VecTranslate
  REAL(KIND=PR), DIMENSION(1:3), INTENT(IN) :: xyzB1, xyzE1, xyzNewB1
  REAL(KIND=PR) :: Lx, Ly, Lz
  
  Lx = xyzE1(1) - xyzB1(1)
  Ly = xyzE1(2) - xyzB1(2)
  Lz = xyzE1(3) - xyzB1(3)
  
  XYZ_VecTranslate = (/ (xyzNewB1(1)+Lx), (xyzNewB1(2)+Ly), (xyzNewB1(3)+Lz) /)
  
  END FUNCTION XYZ_VecTranslate
  
 
END MODULE xyzfun
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

