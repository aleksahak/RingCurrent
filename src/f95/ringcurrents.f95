!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copyright (C) 2009+ Aleksandr B. Sahakyan (aleksahak[at]cantab.net).         !
!                                                                              !
! License: You may redistribute this source code (or its components) and/or    !
!   modify/translate it under the terms of the GNU General Public License      !
!   as published by the Free Software Foundation; version 2 of the License     !
!   (GPL2). You can find the details of GPL2 in the following link:            !
!   https://www.gnu.org/licenses/gpl-2.0.html                                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE ringcurrents
  USE definitions,   ONLY : PR, PI
  USE xyzfun,        ONLY : XYZ_Dist, XYZ_NormCent, XYZ_NormalVector, XYZ_Average2Vec, &
                            XYZ_AngleNC, XYZ_VecTranslate
  USE datatypes,     ONLY : RINGDATA
  IMPLICIT NONE
  SAVE
  
  CONTAINS 
  !#########################################################################
  !##                                                                      #
  !##                                                                      #
  !#########################################################################    
  PURE FUNCTION RingCurrentsP(natom_ring, query_xyz, xyz1, xyz2, xyz3, xyz4, xyz5, xyz6)                       
  TYPE(RINGDATA)      :: RingCurrentsP                     
  INTEGER, INTENT(IN) :: natom_ring
  REAL(KIND=PR), DIMENSION(1:3), INTENT(IN) :: query_xyz, xyz1, xyz2, xyz3, xyz4, xyz5, xyz6
  
  REAL(KIND=PR), DIMENSION(1:3) :: cor, &
                                   CNvec1E, CNvec2E, &
                                   normal1, normal1_rev, &
                                   normal2, normal2_rev, &
                                   Normal, Normal_rev, &
                                   Normal_E, Normal_rev_E, &
                                   ctr_E, query_COM_xyz
  INTEGER    :: at2, a, at1, bt2, b, bt1
  REAL(KIND=PR), DIMENSION(:,:), ALLOCATABLE :: ring_xyz
  REAL(KIND=PR)  :: distance, Theta_radian, newNormal_length


  a = 1; at1 = 3
  b = 4; bt1 = 2

  ! Creating a joint object holding the ring xyz coordinates
  !    row - atom indices, column - x, y, z coordinates
  ALLOCATE (ring_xyz(1:natom_ring, 1:3))
  ring_xyz(1,:) =  xyz1
  ring_xyz(2,:) =  xyz2
  ring_xyz(3,:) =  xyz3
  ring_xyz(4,:) =  xyz4
  ring_xyz(5,:) =  xyz5
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  IF (natom_ring == 5) THEN
    ! 5-membered ring
    ! Atom indices
    at2 = 4
    bt2 = 5
  ELSE IF (natom_ring == 6) THEN
    ! 6-membered ring
    ! Generating the joint ring object to hold the coordinates
    ring_xyz(6,:) =  xyz6
    ! Atom indices
    at2 = 5
    bt2 = 6
  !*PURE*!ELSE
    !*PURE*!PRINT *, "ERROR(RING_CURRENT): natom_ring other then 5 or 6 & 
    !*PURE*!         & is specified..."
    !*PURE*!STOP  
  END IF
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  ! center of ring x,y,z
  cor = (/ (SUM(ring_xyz(:,1))/REAL(natom_ring, KIND=PR) ), & 
           (SUM(ring_xyz(:,2))/REAL(natom_ring, KIND=PR) ), & 
           (SUM(ring_xyz(:,3))/REAL(natom_ring, KIND=PR) ) /) 

           
  ! Calculating 2 possible ring normals from different vector pairs
  ! in the ring, for accounting the ring planarity distortions.
  CNvec1E     = XYZ_NormCent( ring_xyz(at2, : ), ring_xyz(a, : ), 1.0_PR ) 
  CNvec2E     = XYZ_NormCent( ring_xyz(a, : )  , ring_xyz(at1, : ), 1.0_PR ) 
  normal1     = XYZ_NormalVector(CNvec1E, CNvec2E)  
  normal1_rev = XYZ_NormalVector(CNvec2E, CNvec1E)  

  CNvec1E     = XYZ_NormCent( ring_xyz(bt1, : ), ring_xyz(b, : ), 1.0_PR ) 
  CNvec2E     = XYZ_NormCent( ring_xyz(b, : )  , ring_xyz(bt2, : ), 1.0_PR ) 
  normal2     = XYZ_NormalVector(CNvec1E, CNvec2E)  
  normal2_rev = XYZ_NormalVector(CNvec2E, CNvec1E)  
          
  ! Averaging the two normals for eliminating the ring planarity
  ! distortion effect.
  Normal = XYZ_NormCent((/ 0.0_PR,0.0_PR,0.0_PR /), XYZ_Average2Vec(normal1, normal2), 1.0_PR)
  Normal_rev = XYZ_NormCent((/ 0.0_PR,0.0_PR,0.0_PR /), XYZ_Average2Vec(normal1_rev, normal2_rev), 1.0_PR)

  ! Placeing the normalized ring normal vectors onto the COR of the ring
  ! Normal_B = cor; Normal_rev_B = cor
  Normal_E = (/  Normal(1) + ( cor(1) ), &
                 Normal(2) + ( cor(2) ), &
                 Normal(3) + ( cor(3) )    /)
  Normal_rev_E = (/  Normal_rev(1) + ( cor(1) ), &
                     Normal_rev(2) + ( cor(2) ), &
                     Normal_rev(3) + ( cor(3) )    /)

  ! Distance between the query and the center of mass of the ring:
  distance = XYZ_Dist(cor, query_xyz)

  RingCurrentsP%distanceCOR = distance
  RingCurrentsP%query_xyz = query_xyz
  RingCurrentsP%Centered0RealNormal = Normal
  RingCurrentsP%COR = cor

  ! Calculating the distances between the end-points of the determined
  ! two normals and  the queried point (query.xyz), in order to determine
  ! the normal, which faces the point.
  
  IF (XYZ_Dist(Normal_E, query_xyz) < XYZ_Dist(Normal_rev_E, query_xyz)) THEN
  ! RingCurrentsP%Normal_E is always the closest (faceing) normal ending coord.
    RingCurrentsP%Normal_E = Normal_E
    RingCurrentsP%Normal_rev_E = Normal_rev_E
  ELSE
  ! RingCurrentsP%Normal_E is always the closest (faceing) normal ending coord.!!!
    RingCurrentsP%Normal_E = Normal_rev_E
    RingCurrentsP%Normal_rev_E = Normal_E 
  END IF
       
  ! Calculating the angle between the query.xyz---COM  vector and the 
  ! ring normal.
  RingCurrentsP%Theta = XYZ_AngleNC( XYZ_NormCent(RingCurrentsP%COR, RingCurrentsP%Normal_E,  1.0_PR),  &
                            XYZ_NormCent(RingCurrentsP%COR, RingCurrentsP%query_xyz, 1.0_PR),  &
                            .FALSE. , .FALSE. )

  ! Calculating the vector of xyz coordinates for the projection of the
  !  query point onto the surface determined by the calculated normal.
  Theta_radian = (RingCurrentsP%Theta/180)*PI
  newNormal_length = COS(Theta_radian)*(RingCurrentsP%distanceCOR)
  ctr_E = XYZ_NormCent(RingCurrentsP%COR, RingCurrentsP%Normal_E, newNormal_length)
  
  
  query_COM_xyz = XYZ_VecTranslate( (/ 0.0_PR,0.0_PR,0.0_PR /), ctr_E, RingCurrentsP%COR )

  RingCurrentsP%Project  = XYZ_VecTranslate(query_COM_xyz, RingCurrentsP%query_xyz, RingCurrentsP%COR)
  
  ! Calculating the geometrical factor for ring current effects,
  !  according to the Pople's model.
  RingCurrentsP%gfPople = ( (3*( (COS(Theta_radian))**2 ))-1 ) / (RingCurrentsP%distanceCOR**3)


   END FUNCTION RingCurrentsP


END MODULE ringcurrents
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

