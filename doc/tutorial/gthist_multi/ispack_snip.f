*************************************************************************
*   このファイルに記述されるサブルーチン群は,                           *
*   gtool5 ライブラリのチュートリアルで使用するため,                    *
*   ISPACK FORTRAN SUBROUTINE LIBRARY                                   *
*   <http://www.gfd-dennou.org/library/ispack>                          *
*   のサブルーチンの一部を抜粋したものです.                             *
*   ライセンスはオリジナル ISPACK に準拠するため,                       *
*   改変や再配布に際しては ISPACK のライセンスを参照ください.           *
*************************************************************************

*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
      SUBROUTINE BSCOPY(N,A,B)
 
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N)
 
      DO 10 I=1,N
        B(I)=A(I)
   10 CONTINUE
 
      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     DUMP MESSAGES
*-----------------------------------------------------------------------
      SUBROUTINE BSDMSG(CL,CS,CM)

      CHARACTER CL*1,CS*(*),CM*(*)
      CHARACTER CSD*6,CMD*53
      DATA MMSG,IMSG/20,0/
      SAVE

      CSD=CS
      CMD=CM

      IF(CL.EQ.'E') THEN
        WRITE(6,'(A)') '***** ERROR ('//CSD//') ***  '//CMD
        STOP
      END IF

      IF(IMSG.LT.MMSG) THEN
        IF(CL.EQ.'W') THEN
          IMSG=IMSG+1
          WRITE(*,*) '*** WARNING ('//CSD//') ***  '//CMD
        ELSE IF(CL.EQ.'M') THEN
          IMSG=IMSG+1
          WRITE(*,*) '*** MESSAGE ('//CSD//') ***  '//CMD
        END IF
        IF(IMSG.EQ.MMSG) THEN
          WRITE(*,*) '+++ THE FOLLOWING MESSAGES ARE SUPRRESSED.'
        END IF
      END IF

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
      SUBROUTINE BSSET0(N,A)
 
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N)
 
      DO 10 I=1,N
        A(I)=0
   10 CONTINUE
 
      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     CALCULATE NONLINEAR TERM (for 2 components)             2000/10/02
************************************************************************
      SUBROUTINE C2AJB2(LM,KM,JM,IM,R,Z,S,DZ,DS,WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Z(-KM:KM,LM)
      DIMENSION DZ(-KM:KM,LM)
      DIMENSION S(-KM:KM,LM)
      DIMENSION DS(-KM:KM,LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,4)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

      DO L=1,LM
        DO K=-KM,KM
          DZ(K,L)=-Z(K,L)/((R*K)*(R*K)+L*L)
        END DO
      END DO

* CALCULATE V COMPONENT --> WG(*,2)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-R*K*DZ(-K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(1,2),WG,ITJ,TJ,ITI,TI,3)

* CALCULATE U COMPONENT --> WG(*,3)

      CALL BSSET0(2*KM+1,WS)
      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-L*DZ(K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS,WG(1,3),WG,ITJ,TJ,ITI,TI,4)

* V*V-U*U --> WG(*,4)

      DO JI=1,(JM+1)*IM
        WG(JI,4)=WG(JI,2)*WG(JI,2)-WG(JI,3)*WG(JI,3)
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),WS,WG,ITJ,TJ,ITI,TI,4)

* U*V --> WG(*,4)

      DO JI=1,(JM+1)*IM
        WG(JI,4)=WG(JI,2)*WG(JI,3)
      END DO
      
      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),DZ,WG,ITJ,TJ,ITI,TI,3)

      DO L=1,LM
        DO K=-KM,KM
          DZ(K,L)=((R*K)*(R*K)-L*L)*DZ(K,L)-(R*K)*L*WS(-K,L)
        END DO
      END DO

* S --> WG(JI,4)

      CALL C2S2GA(LM,KM,JM,IM,S,WG(1,4),WG,ITJ,TJ,ITI,TI,3)
      
* U*S --> WG(JI,3)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,3)*WG(JI,4)
      END DO

* U*S のスペクトル --> WS

      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),WS(-KM,1),WG,ITJ,TJ,ITI,TI,3)

* ∂(U*S)/∂x --> DS

      DO L=1,LM
        DO K=-KM,KM
          DS(K,L)=-R*K*WS(-K,L)
        END DO
      END DO

* V*S  --> WG(JI,2)

      DO JI=1,(JM+1)*IM
        WG(JI,2)=WG(JI,2)*WG(JI,4)
      END DO

* V*S のスペクトル --> WS

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),WS,WG,ITJ,TJ,ITI,TI,4)

* Finally calculate Jacobian DS)

      DO L=1,LM
        DO K=-KM,KM
          DS(K,L)=-DS(K,L)+L*WS(K,L)
        END DO
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     CALCULATE NONLINEAR TERM                                2000/10/02
************************************************************************
      SUBROUTINE C2AJBS(LM,KM,JM,IM,R,Z,DZ,WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Z(-KM:KM,LM)
      DIMENSION DZ(-KM:KM,LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,3)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

      DO L=1,LM
        DO K=-KM,KM
          DZ(K,L)=-Z(K,L)/((R*K)*(R*K)+L*L)
        END DO
      END DO

* CALCULATE V COMPONENT --> WG(*,2)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-R*K*DZ(-K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(1,2),WG,ITJ,TJ,ITI,TI,3)

* CALCULATE U COMPONENT --> WG(*,3)

      CALL BSSET0(2*KM+1,WS)
      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-L*DZ(K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS,WG(1,3),WG,ITJ,TJ,ITI,TI,4)

* V*V-U*U --> WG(*,1), UV --> WG(*,2)

      DO JI=1,(JM+1)*IM
        WG(JI,1)=WG(JI,2)*WG(JI,2)-WG(JI,3)*WG(JI,3)
        WG(JI,2)=WG(JI,2)*WG(JI,3)
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG,WS,WG(1,3),ITJ,TJ,ITI,TI,4)      
      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),DZ,WG,ITJ,TJ,ITI,TI,3)

      DO L=1,LM
        DO K=-KM,KM
          DZ(K,L)=((R*K)*(R*K)-L*L)*DZ(K,L)-(R*K)*L*WS(-K,L)
        END DO
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     CALCULATE JACOBIAN for 2 components                     2000/10/06
************************************************************************
      SUBROUTINE C2AJC2(LM,KM,JM,IM,SA,SB1,SB2,SC1,SC2,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA(-KM:KM,LM)
      DIMENSION SB1(-KM:KM,LM),SB2(-KM:KM,LM)
      DIMENSION SC1(-KM:KM,LM),SC2(-KM:KM,LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,4)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

* ∂A/∂y --> WG(JI,2)

      CALL BSSET0(2*KM+1,WS)
      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=L*SA(K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS,WG(1,2),WG,ITJ,TJ,ITI,TI,4)

* B1 --> WG(JI,3)

      CALL C2S2GA(LM,KM,JM,IM,SB1,WG(1,3),WG,ITJ,TJ,ITI,TI,3)

* B1 × ∂A/∂y  --> WG(JI,4)

      DO JI=1,(JM+1)*IM
        WG(JI,4)=WG(JI,3)*WG(JI,2)
      END DO

* B1 × ∂A/∂y のスペクトル --> WS

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),WS(-KM,1),WG,ITJ,TJ,ITI,TI,3)

* ∂(B1 × ∂A/∂y)/∂x --> SC1

      DO L=1,LM
        DO K=-KM,KM
          SC1(K,L)=-K*WS(-K,L)
        END DO
      END DO

* B2 --> WG(JI,4)

      CALL C2S2GA(LM,KM,JM,IM,SB2,WG(1,4),WG,ITJ,TJ,ITI,TI,3)

* B2 × ∂A/∂y  --> WG(JI,2)

      DO JI=1,(JM+1)*IM
        WG(JI,2)=WG(JI,4)*WG(JI,2)
      END DO

* B2 × ∂A/∂y のスペクトル --> WS

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),WS(-KM,1),WG,ITJ,TJ,ITI,TI,3)

* ∂(B2 × ∂A/∂y)/∂x --> SC1

      DO L=1,LM
        DO K=-KM,KM
          SC2(K,L)=-K*WS(-K,L)
        END DO
      END DO

*---------------------
* ∂A/∂x --> WG(JI,2)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-K*SA(-K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(1,2),WG,ITJ,TJ,ITI,TI,3)

* B1 × ∂A/∂x  --> WG(JI,3)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,3)*WG(JI,2)
      END DO

* B1 × ∂A/∂x のスペクトル --> WS      

      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),WS,WG,ITJ,TJ,ITI,TI,4)

* Finally calculate Jacobian (SC1)

      DO L=1,LM
        DO K=-KM,KM
          SC1(K,L)=-(SC1(K,L)+L*WS(K,L))
        END DO
      END DO

* B2 × ∂A/∂x  --> WG(JI,4)

      DO JI=1,(JM+1)*IM
        WG(JI,4)=WG(JI,4)*WG(JI,2)
      END DO

* B1 × ∂A/∂x のスペクトル --> WS      

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),WS,WG,ITJ,TJ,ITI,TI,4)

* Finally calculate Jacobian (SC2)

      DO L=1,LM
        DO K=-KM,KM
          SC2(K,L)=-(SC2(K,L)+L*WS(K,L))
        END DO
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     CALCULATE JACOBIAN                                      2000/10/02
************************************************************************
      SUBROUTINE C2AJCB(LM,KM,JM,IM,SA,SB,SC,WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA(-KM:KM,LM)
      DIMENSION SB(-KM:KM,LM)
      DIMENSION SC(-KM:KM,LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,3)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

* A --> WG(JI,3)

      CALL C2S2GA(LM,KM,JM,IM,SA,WG(1,3),WG,ITJ,TJ,ITI,TI,3)

* ∂B/∂y --> WG(JI,2)

      CALL BSSET0(2*KM+1,WS)
      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=L*SB(K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS,WG(1,2),WG,ITJ,TJ,ITI,TI,4)

* A × ∂B/∂y  --> WG(JI,2)

      DO JI=1,(JM+1)*IM
        WG(JI,2)=WG(JI,3)*WG(JI,2)
      END DO

* A × ∂B/∂y のスペクトル --> WS

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),WS(-KM,1),WG,ITJ,TJ,ITI,TI,3)

* ∂(A × ∂B/∂y)/∂x --> SC      

      DO L=1,LM
        DO K=-KM,KM
          SC(K,L)=-K*WS(-K,L)
        END DO
      END DO
      
* ∂B/∂x --> WG(JI,2)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-K*SB(-K,L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(1,2),WG,ITJ,TJ,ITI,TI,3)

* A × ∂B/∂x  --> WG(JI,2)

      DO JI=1,(JM+1)*IM
        WG(JI,2)=WG(JI,3)*WG(JI,2)
      END DO

* A × ∂B/∂x のスペクトル --> WS      

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),WS,WG,ITJ,TJ,ITI,TI,4)

* Finally calculate Jacobian

      DO L=1,LM
        DO K=-KM,KM
          SC(K,L)=SC(K,L)+L*WS(K,L)
        END DO
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     TRANSFORM GRID TO SPECTRA (including everything)        2000/09/29
************************************************************************
*     LM≦JM-1, W((JM+1)*IM)
*         S(-KM:KM,0:LM-1) for ISW=1,3
*      or S(-KM:KM,0:LM)   for ISW=2,4
*      
*     ISW=1: SINE TRAPEZOIDAL
*     ISW=2: COSINE TRAPEZOIDAL
*     ISW=3: SINE MIDPOINT
*     ISW=4: COSINE MIDPOINT
*-----------------------------------------------------------------------      
      SUBROUTINE C2G2SA(LM,KM,JM,IM,G,S,W,ITJ,TJ,ITI,TI,ISW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(0:JM,0:IM-1)
*      DIMENSION S(-KM:KM,0:LM)
      DIMENSION S(-KM:KM,0:*)
      DIMENSION W(-KM:KM,0:JM)            
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

      CALL FTTRUF(JM+1,IM,G,W,ITI,TI)

      DO K=1,KM
        DO J=0,JM
          W( K,J)=G(J,2*K  )
          W(-K,J)=G(J,2*K+1)
        END DO
      END DO
      DO J=0,JM
        W(0,J)=G(J,0)
      END DO

      IF(ISW.EQ.1) THEN
        CALL FTTSTF(2*KM+1,JM,W(-KM,1),G,ITJ,TJ)
      ELSE IF(ISW.EQ.2) THEN
        CALL FTTCTF(2*KM+1,JM,W,G,ITJ,TJ)
      ELSE IF(ISW.EQ.3) THEN
        CALL FTTSMF(2*KM+1,JM,W,G,ITJ,TJ)
      ELSE IF(ISW.EQ.4) THEN
        CALL FTTCMF(2*KM+1,JM,W,G,ITJ,TJ)
      ELSE
        CALL BSDMSG('E','ISW IS INVALID.','')
      END IF

      IF(ISW.EQ.1) THEN
        CALL BSCOPY((2*KM+1)*LM,W(-KM,1),S)
      ELSE IF(ISW.EQ.3) THEN
        CALL BSCOPY((2*KM+1)*LM,W,S)
      ELSE
        DO K=-KM,KM
          S(K,0)=0.5D0*W(K,0)
        END DO
        CALL BSCOPY((2*KM+1)*LM,W(-KM,1),S(-KM,1))
      END IF

      END
************************************************************************
*     TRANSPOSE FOR C2G2SA
*-----------------------------------------------------------------------
      SUBROUTINE C2G2ST(JM,IM,G,W)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(0:JM,0:IM-1)      
      DIMENSION G(0:IM-1,0:JM)

      DO J=0,JM
        DO I=0,IM-1
          W(J,I)=G(I,J)
        END DO
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     INITIALIZATION OF C2PACK                                2000/09/26
************************************************************************
      SUBROUTINE C2INIT(JM,IM,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

      CALL FTTSMI(JM,ITJ,TJ)
      CALL FTTRUI(IM,ITI,TI)

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     TRANSFORM SPECTRA TO GRID (including everything)        2000/09/29
************************************************************************
*     LM≦JM-1, W((JM+1)*IM)
*         S(-KM:KM,0:LM-1) for ISW=1,3
*      or S(-KM:KM,0:LM)   for ISW=2,4
*      
*     ISW=1: SINE TRAPEZOIDAL
*     ISW=2: COSINE TRAPEZOIDAL
*     ISW=3: SINE MIDPOINT
*     ISW=4: COSINE MIDPOINT
*-----------------------------------------------------------------------
      SUBROUTINE C2S2GA(LM,KM,JM,IM,S,G,W,ITJ,TJ,ITI,TI,ISW)

      IMPLICIT REAL*8(A-H,O-Z)
*      DIMENSION S(-KM:KM,0:LM)
      DIMENSION S(-KM:KM,0:*)
      DIMENSION G(0:JM,0:IM-1)
      DIMENSION W(-KM:KM,0:JM)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

      IF(ISW.EQ.1) THEN
        CALL BSSET0((2*KM+1),W)
        CALL BSCOPY((2*KM+1)*LM,S,W(-KM,1))
        CALL BSSET0((2*KM+1)*(JM-LM),W(-KM,LM+1))
      ELSE IF(ISW.EQ.3) THEN
        CALL BSCOPY((2*KM+1)*LM,S,W)
        CALL BSSET0((2*KM+1)*(JM-LM+1),W(-KM,LM))
      ELSE
        DO K=-KM,KM
          W(K,0)=2*S(K,0)
        END DO
        CALL BSCOPY((2*KM+1)*LM,S(-KM,1),W(-KM,1))
        CALL BSSET0((2*KM+1)*(JM-LM),W(-KM,LM+1))
      END IF

      IF(ISW.EQ.1) THEN
        CALL FTTSTB(2*KM+1,JM,W(-KM,1),G,ITJ,TJ)
      ELSE IF(ISW.EQ.2) THEN
        CALL FTTCTB(2*KM+1,JM,W,G,ITJ,TJ)
      ELSE IF(ISW.EQ.3) THEN
        CALL FTTSMB(2*KM+1,JM,W,G,ITJ,TJ)
      ELSE IF(ISW.EQ.4) THEN
        CALL FTTCMB(2*KM+1,JM,W,G,ITJ,TJ)        
      ELSE
        CALL BSDMSG('E','ISW IS INVALID.','')
      END IF

      DO J=0,JM
        G(J,0)=W(0,J)
        G(J,1)=0
      END DO
      DO K=1,KM        
        DO J=0,JM
          G(J,2*K  )=W( K,J)
          G(J,2*K+1)=W(-K,J)
        END DO
      END DO
      DO I=2*KM+2,IM-1
        DO J=0,JM
          G(J,I)=0
        END DO
      END DO

      CALL FTTRUB(JM+1,IM,G,W,ITI,TI)

      END
************************************************************************
*     TRANSPOSE FOR C2S2GA
*-----------------------------------------------------------------------      
      SUBROUTINE C2S2GT(JM,IM,W,G)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(0:IM-1,0:JM)
      DIMENSION W(0:JM,0:IM-1)

      DO J=0,JM
        DO I=0,IM-1
          G(I,J)=W(J,I)
        END DO
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     浅水方程式の初期値化                                    2000/10/11
************************************************************************
      SUBROUTINE C2SWBL(LM,KM,JM,IM,R,BARPHI,AVT,PHI,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-KM:KM,LM)
      DIMENSION PHI(-KM:KM,0:LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,4)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

* V --> WS, U --> PHI

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-(-R*K*AVT(-K,L))/(R*K*R*K+L*L)
          PHI(K,L)=-(-L*AVT(K,L))/(R*K*R*K+L*L)
        END DO
      END DO

      L=0
      DO K=-K,KM
        PHI(K,0)=0
      END DO

* U --> WG(*,2)

      CALL C2S2GA(LM,KM,JM,IM,PHI,WG(1,2),WG,ITJ,TJ,ITI,TI,4)

* V --> WG(*,3)
      
      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(1,3),WG,ITJ,TJ,ITI,TI,3)

* -(U*U+V*V)/2 --> WG(*,4)
      
      DO JI=1,(JM+1)*IM
        WG(JI,4)=-0.5D0*(WG(JI,2)*WG(JI,2)+WG(JI,3)*WG(JI,3))
      END DO

* -(U*U+V*V)/2 --> PHI

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),PHI,WG,ITJ,TJ,ITI,TI,4)      
      
* AVT --> WG(*,4)

      CALL C2S2GA(LM,KM,JM,IM,AVT,WG(1,4),WG,ITJ,TJ,ITI,TI,3)

* V*AVT --> WG(*,3), U*AVT --> WG(*,2)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,3)*WG(JI,4)        
        WG(JI,2)=WG(JI,2)*WG(JI,4)
      END DO

* U*AVT --> WS

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),WS(-KM,1),WG,ITJ,TJ,ITI,TI,3)

      DO L=1,LM
        DO K=-KM,KM
          PHI(K,L)=PHI(K,L)+(L*WS(K,L))/(R*K*R*K+L*L)
        END DO
      END DO

* V*AVT --> WS
      
      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),WS,WG,ITJ,TJ,ITI,TI,4)

      DO L=1,LM
        DO K=-KM,KM
          PHI(K,L)=PHI(K,L)+(R*K*WS(-K,L))/(R*K*R*K+L*L)
        END DO
      END DO

      L=0
      DO K=1,KM
        PHI(K,L)=PHI(K,L)+(R*K*WS(-K,L))/(R*K*R*K+L*L)
        PHI(-K,L)=PHI(-K,L)+(-R*K*WS(K,L))/(R*K*R*K+L*L)        
      END DO

      PHI(0,0)=BARPHI

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     浅水方程式の保存量のチェック                            2000/10/10
************************************************************************
      SUBROUTINE C2SWCK(LM,KM,JM,IM,R,AVT,DIV,PHI,AENE,AENS,AMOM,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-KM:KM,LM)
      DIMENSION DIV(-KM:KM,0:LM)
      DIMENSION PHI(-KM:KM,0:LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG(0:JM,IM,4)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

      CALL C2S2GA(LM,KM,JM,IM,PHI,WG(0,1,2),WG,ITJ,TJ,ITI,TI,4)
      CALL C2S2GA(LM,KM,JM,IM,AVT,WG(0,1,3),WG,ITJ,TJ,ITI,TI,3)

      AENS=0
      DO I=1,IM
        DO J=0,JM-1
          AENS=AENS+WG(J,I,3)*WG(J,I,3)/WG(J,I,2)
        END DO
      END DO
      AENS=AENS/(2*JM*IM)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-(-R*K*AVT(-K,L)-L*DIV(K,L))/(R*K*R*K+L*L)
        END DO
      END DO

      CALL C2S2GA(LM,KM,JM,IM,WS(-KM,1),WG(0,1,3),WG,ITJ,TJ,ITI,TI,3)

      DO L=1,LM
        DO K=-KM,KM
          WS(K,L)=-(-R*K*DIV(-K,L)-L*AVT(K,L))/(R*K*R*K+L*L)
        END DO
      END DO
      L=0
      DO K=1,KM
        WS(K,0)=-(-R*K*DIV(-K,0))/(R*K*R*K)
        WS(-K,0)=-(R*K*DIV(K,0))/(R*K*R*K)        
      END DO
      WS(0,0)=0

      CALL C2S2GA(LM,KM,JM,IM,WS,WG(0,1,4),WG,ITJ,TJ,ITI,TI,4)
      
      AENE=0
      DO I=1,IM
        DO J=0,JM-1
          AENE=AENE
     &      +WG(J,I,2)*(WG(J,I,3)*WG(J,I,3)
     &                  +WG(J,I,4)*WG(J,I,4)+WG(J,I,2))
        END DO
      END DO
      AENE=AENE/(2*JM*IM)

      AMOM=0
      DO I=1,IM
        DO J=0,JM-1
          AMOM=AMOM+WG(J,I,2)*WG(J,I,4)
        END DO
      END DO
      AMOM=AMOM/(JM*IM)
      
      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     CALCULATE NONLINEAR TERM FOR SW EQ.                     2000/10/06
************************************************************************
      SUBROUTINE C2SWNL(LM,KM,JM,IM,R,AVT,DIV,PHI,DAVT,DDIV,DPHI,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-KM:KM,LM)
      DIMENSION DIV(-KM:KM,0:LM)
      DIMENSION PHI(-KM:KM,0:LM)
      DIMENSION DAVT(-KM:KM,LM)
      DIMENSION DDIV(-KM:KM,0:LM)
      DIMENSION DPHI(-KM:KM,0:LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,4)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

* V --> DAVT, U --> DDIV

      DO L=1,LM
        DO K=-KM,KM
          DAVT(K,L)=-(-R*K*AVT(-K,L)-L*DIV(K,L))/(R*K*R*K+L*L)
          DDIV(K,L)=-(-R*K*DIV(-K,L)-L*AVT(K,L))/(R*K*R*K+L*L)
        END DO
      END DO

      L=0
      DO K=1,KM
        DDIV(K,0)=-(-R*K*DIV(-K,0))/(R*K*R*K)
        DDIV(-K,0)=-(R*K*DIV(K,0))/(R*K*R*K)
      END DO
      DDIV(0,0)=0

* U --> WG(*,2)

      CALL C2S2GA(LM,KM,JM,IM,DDIV,WG(1,2),WG,ITJ,TJ,ITI,TI,4)

* CALCULATE PHI COMPONENT

      CALL C2S2GA(LM,KM,JM,IM,PHI,WG(1,4),WG,ITJ,TJ,ITI,TI,4)

* U*PHI --> WG(*,3)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,2)*WG(JI,4)        
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),DDIV,WG,ITJ,TJ,ITI,TI,4)

* V=WG(*,3)
      
      CALL C2S2GA(LM,KM,JM,IM,DAVT,WG(1,3),WG,ITJ,TJ,ITI,TI,3)

* V*PHI --> WG(*,4)

      DO JI=1,(JM+1)*IM
        WG(JI,4)=WG(JI,3)*WG(JI,4)
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),DAVT,WG,ITJ,TJ,ITI,TI,3)

* CALCULATE DPHI      

      DO L=1,LM
        DO K=-KM,KM
          DPHI(K,L)=R*K*DDIV(-K,L)-L*DAVT(K,L)
        END DO
      END DO

      L=0
      DO K=-KM,KM
        DPHI(K,0)=R*K*DDIV(-K,0)
      END DO

* (U*U+V*V)/2 --> WG(*,4)
      
      DO JI=1,(JM+1)*IM
        WG(JI,4)=0.5D0*(WG(JI,2)*WG(JI,2)+WG(JI,3)*WG(JI,3))
      END DO

* (U*U+V*V)/2 --> WS      

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),WS,WG,ITJ,TJ,ITI,TI,4)      
      
* AVT --> WG(*,4)

      CALL C2S2GA(LM,KM,JM,IM,AVT,WG(1,4),WG,ITJ,TJ,ITI,TI,3)

* V*AVT --> WG(*,3), U*AVT --> WG(*,2)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,3)*WG(JI,4)        
        WG(JI,2)=WG(JI,2)*WG(JI,4)
      END DO

* V*AVT --> DDIV, U*AVT --> DAVT

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),DAVT,WG,ITJ,TJ,ITI,TI,3)
      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),DDIV,WG,ITJ,TJ,ITI,TI,4)

      DO L=1,LM
        DO K=1,KM
          DAVTK=DAVT(K,L)
          DDIVK=DDIV(-K,L)
          DAVT(K,L)=R*K*DAVT(-K,L)+L*DDIV(K,L)
          DDIV(-K,L)=R*K*DDIV(K,L)-L*DAVT(-K,L)
     &      +(R*K*R*K+L*L)*(WS(-K,L)+PHI(-K,L))
          DAVT(-K,L)=-R*K*DAVTK+L*DDIVK
          DDIV(K,L)=-R*K*DDIVK-L*DAVTK
     &      +(R*K*R*K+L*L)*(WS(K,L)+PHI(K,L))
        END DO
      END DO

      DO L=1,LM
        K=0
        DAVTK=DAVT(0,L)
        DAVT(0,L)=L*DDIV(0,L)
        DDIV(0,L)=-L*DAVTK+L*L*(WS(0,L)+PHI(0,L))
      END DO

      L=0
      DO K=1,KM
        DDIVK=DDIV(-K,0)
        DDIV(-K,0)=R*K*DDIV(K,0)+(R*K*R*K)*(WS(-K,0)+PHI(-K,0))
        DDIV(K,0)=-R*K*DDIVK+(R*K*R*K)*(WS(K,0)+PHI(K,0))
      END DO
      DDIV(0,0)=0

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 2000 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     CALCULATE NONLINEAR TERM FOR SW EQ. (NL term ONLY)      2000/10/13
************************************************************************
      SUBROUTINE C2SWNN(LM,KM,JM,IM,R,BARPHI,AVT,DIV,PHI,DAVT,DDIV,DPHI,
     &  WS,WG,ITJ,TJ,ITI,TI)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT(-KM:KM,LM)
      DIMENSION DIV(-KM:KM,0:LM)
      DIMENSION PHI(-KM:KM,0:LM)
      DIMENSION DAVT(-KM:KM,LM)
      DIMENSION DDIV(-KM:KM,0:LM)
      DIMENSION DPHI(-KM:KM,0:LM)
      DIMENSION WS(-KM:KM,0:LM)
      DIMENSION WG((JM+1)*IM,4)
      DIMENSION ITJ(5),TJ(JM*6),ITI(5),TI(IM*2)

* V --> DAVT, U --> DDIV

      DO L=1,LM
        DO K=-KM,KM
          DAVT(K,L)=-(-R*K*AVT(-K,L)-L*DIV(K,L))/(R*K*R*K+L*L)
          DDIV(K,L)=-(-R*K*DIV(-K,L)-L*AVT(K,L))/(R*K*R*K+L*L)
        END DO
      END DO

      L=0
      DO K=1,KM
        DDIV(K,0)=-(-R*K*DIV(-K,0))/(R*K*R*K)
        DDIV(-K,0)=-(R*K*DIV(K,0))/(R*K*R*K)
      END DO
      DDIV(0,0)=0

* U --> WG(*,2)

      CALL C2S2GA(LM,KM,JM,IM,DDIV,WG(1,2),WG,ITJ,TJ,ITI,TI,4)

* CALCULATE PHI COMPONENT

      CALL C2S2GA(LM,KM,JM,IM,PHI,WG(1,4),WG,ITJ,TJ,ITI,TI,4)

* U*(PHI-BARPHI) --> WG(*,3)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,2)*(WG(JI,4)-BARPHI)
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),DDIV,WG,ITJ,TJ,ITI,TI,4)

* V=WG(*,3)
      
      CALL C2S2GA(LM,KM,JM,IM,DAVT,WG(1,3),WG,ITJ,TJ,ITI,TI,3)

* V*PHI --> WG(*,4)

      DO JI=1,(JM+1)*IM
        WG(JI,4)=WG(JI,3)*(WG(JI,4)-BARPHI)
      END DO

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),DAVT,WG,ITJ,TJ,ITI,TI,3)

* CALCULATE DPHI      

      DO L=1,LM
        DO K=-KM,KM
          DPHI(K,L)=R*K*DDIV(-K,L)-L*DAVT(K,L)
        END DO
      END DO

      L=0
      DO K=-KM,KM
        DPHI(K,0)=R*K*DDIV(-K,0)
      END DO

* (U*U+V*V)/2 --> WG(*,4)
      
      DO JI=1,(JM+1)*IM
        WG(JI,4)=0.5D0*(WG(JI,2)*WG(JI,2)+WG(JI,3)*WG(JI,3))
      END DO

* (U*U+V*V)/2 --> WS      

      CALL C2G2SA(LM,KM,JM,IM,WG(1,4),WS,WG,ITJ,TJ,ITI,TI,4)      
      
* AVT --> WG(*,4)

      CALL C2S2GA(LM,KM,JM,IM,AVT,WG(1,4),WG,ITJ,TJ,ITI,TI,3)

* V*AVT --> WG(*,3), U*AVT --> WG(*,2)

      DO JI=1,(JM+1)*IM
        WG(JI,3)=WG(JI,3)*WG(JI,4)        
        WG(JI,2)=WG(JI,2)*WG(JI,4)
      END DO

* V*AVT --> DDIV, U*AVT --> DAVT

      CALL C2G2SA(LM,KM,JM,IM,WG(1,2),DAVT,WG,ITJ,TJ,ITI,TI,3)
      CALL C2G2SA(LM,KM,JM,IM,WG(1,3),DDIV,WG,ITJ,TJ,ITI,TI,4)

      DO L=1,LM
        DO K=1,KM
          DAVTK=DAVT(K,L)
          DDIVK=DDIV(-K,L)
          DAVT(K,L)=R*K*DAVT(-K,L)+L*DDIV(K,L)
          DDIV(-K,L)=R*K*DDIV(K,L)-L*DAVT(-K,L)
     &      +(R*K*R*K+L*L)*WS(-K,L)
          DAVT(-K,L)=-R*K*DAVTK+L*DDIVK
          DDIV(K,L)=-R*K*DDIVK-L*DAVTK
     &      +(R*K*R*K+L*L)*WS(K,L)
        END DO
      END DO

      DO L=1,LM
        K=0
        DAVTK=DAVT(0,L)
        DAVT(0,L)=L*DDIV(0,L)
        DDIV(0,L)=-L*DAVTK+L*L*WS(0,L)
      END DO

      L=0
      DO K=1,KM
        DDIVK=DDIV(-K,0)
        DDIV(-K,0)=R*K*DDIV(K,0)+(R*K*R*K)*WS(-K,0)
        DDIV(K,0)=-R*K*DDIVK+(R*K*R*K)*WS(K,0)
      END DO
      DDIV(0,0)=0

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
*     COSINE TRANSFORM (MID-POINT)                   2000/09/19 K.Ishioka      
*************************************************************************
************************************************************************
      SUBROUTINE FTTCMI(N,IT,T)
 
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)      
      DIMENSION T(0:N/2-1,12),IT(5)

      CALL FTTRUI(N,IT,T)
 
      N2=N*2
 
      DO I=0,N/2-1
        T(I,5)=SIN(2*PI*(2*I+1)/N2)        
        T(I,6)=COS(2*PI*(2*I+1)/N2)
        T(I,7)=COS(PI*I/N)
        T(I,8)=SIN(PI*I/N)
        T(I,9)=SIN(PI*(4*I+1)/N2)
        T(I,10)=SIN(PI*(4*I+3)/N2)
        T(I,11)=1/T(I,9)
        T(I,12)=1/T(I,10)
      END DO
 
      END
************************************************************************
      SUBROUTINE FTTCMF(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,12)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)
 
      DO J=0,N/2-1
        J1=2*J
        J2=N-1-J1
        J3=2*J+1
        J4=N-1-J3
        DO I=1,M
          Y(I,J1)=X(I,J1)+X(I,J2)+2*T(J,9 )*(X(I,J1)-X(I,J2))
          Y(I,J3)=X(I,J3)+X(I,J4)+2*T(J,10)*(X(I,J3)-X(I,J4))
        END DO
      END DO

      CALL FTTRUF(M,N,Y,X,IT,T)
 
      DO I=1,M
        X(I,0)=Y(I,0)
        X(I,N-1)=0.5D0*Y(I,1)
      END DO
 
      DO J=N/2-1,1,-1
        DO I=1,M
          X(I,2*J)=T(J,7)*Y(I,2*J)+T(J,8)*Y(I,2*J+1)
          X(I,2*J-1)=X(I,2*J+1)+T(J,8)*Y(I,2*J)-T(J,7)*Y(I,2*J+1)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE FTTCMB(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,12)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)
 
      DO I=1,M
        Y(I,0)=X(I,0)
        Y(I,1)=2*X(I,N-1)
      END DO
 
      DO J=1,N/2-1
        DO I=1,M
          Y(I,2*J)=T(J,7)*X(I,2*J)-(X(I,2*J+1)-X(I,2*J-1))*T(J,8)
          Y(I,2*J+1)=T(J,8)*X(I,2*J)+(X(I,2*J+1)-X(I,2*J-1))*T(J,7)
        END DO
      END DO

      CALL FTTRUB(M,N,Y,X,IT,T)      

      DO J=0,N/2-1
        DO I=1,M
          X(I,2*J  )=0.25D0*(Y(I,2*J)+Y(I,2*(N/2-1-J)+1))
     &      +0.125D0*(Y(I,2*J)-Y(I,2*(N/2-1-J)+1))*T(J,11)
          X(I,2*J+1)=0.25D0*(Y(I,2*J+1)+Y(I,2*(N/2-1-J)))
     &      +0.125D0*(Y(I,2*J+1)-Y(I,2*(N/2-1-J)))*T(J,12)
        END DO
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
*     COSINE TRANSFORM (TRAPEZOIDAL)                 2000/09/19 K.Ishioka      
*************************************************************************
      SUBROUTINE FTTCTI(N,IT,T)
 
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)      
      DIMENSION T(0:N/2-1,6),IT(5)

      CALL FTTRUI(N,IT,T)
 
      N2=N*2
 
      DO I=0,N/2-1
        T(I,5)=SIN(2*PI*(2*I+1)/N2)
        T(I,6)=COS(2*PI*(2*I+1)/N2)
      END DO
 
      END
************************************************************************
      SUBROUTINE FTTCTF(M,N,X,Y,IT,T)
 
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,6)
      DIMENSION X(M,0:N),Y(M,0:N-1)
 
      DO J=0,N/2-1
        J1=2*J
        J2=N-J1
        J3=2*J+1
        J4=N-J3
        DO I=1,M
          Y(I,J1)=X(I,J1)+X(I,J2)+2*T(J,4)*(X(I,J1)-X(I,J2))
          Y(I,J3)=X(I,J3)+X(I,J4)+2*T(J,5)*(X(I,J3)-X(I,J4))
        END DO
      END DO
 
      F=1D0/N
      DO I=1,M
        X(I,N)=(X(I,0)-X(I,N)+2*X(I,1)*T(0,6))*F
      END DO

      F=2D0/N
      DO J=1,N/2-1
        DO I=1,M
          X(I,N)=X(I,N)+(X(I,2*J)*T(J,3)+X(I,2*J+1)*T(J,6))*F
        END DO
      END DO

      CALL FTTRUF(M,N,Y,X,IT,T)
 
      DO I=1,M
        X(I,0)=Y(I,0)
        X(I,1)=X(I,N)
        X(I,N)=Y(I,1)
      END DO
 
      DO J=1,N/2-1
        DO I=1,M
          X(I,2*J)=Y(I,2*J)
          X(I,2*J+1)=X(I,2*J-1)+Y(I,2*J+1)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE FTTCTB(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,6)
      DIMENSION X(M,0:N),Y(M,0:N-1)

      CALL FTTCTF(M,N,X,Y,IT,T)

      DO J=0,N
        DO I=1,M
          X(I,J)=(N/2D0)*X(I,J)
        END DO
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     DUMP MESSAGES
*-----------------------------------------------------------------------
      SUBROUTINE FTDMSG(CL,CS,CM)

      CHARACTER CL*1,CS*(*),CM*(*)
      CHARACTER CSD*6,CMD*53
      DATA MMSG,IMSG/20,0/
      SAVE

      CSD=CS
      CMD=CM

      IF(CL.EQ.'E') THEN
        WRITE(6,'(A)') '***** ERROR ('//CSD//') ***  '//CMD
        STOP
      END IF

      IF(IMSG.LT.MMSG) THEN
        IF(CL.EQ.'W') THEN
          IMSG=IMSG+1
          WRITE(*,*) '*** WARNING ('//CSD//') ***  '//CMD
        ELSE IF(CL.EQ.'M') THEN
          IMSG=IMSG+1
          WRITE(*,*) '*** MESSAGE ('//CSD//') ***  '//CMD
        END IF
        IF(IMSG.EQ.MMSG) THEN
          WRITE(*,*) '+++ THE FOLLOWING MESSAGES ARE SUPRRESSED.'
        END IF
      END IF

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
      SUBROUTINE FTTRUI(N,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION T(0:N/2-1,4),IT(5)

      IF(MOD(N,2).NE.0) THEN
        CALL FTDMSG('E','FTTRUI','N MUST BE EVEN.')
      END IF

      L=N/2

      CALL FTTZLI(L,IT,T)

      DO 10 I=0,L-1
        T(I,3)=COS(2*PI*I/N)
        T(I,4)=SIN(2*PI*I/N)
   10 CONTINUE

      END
************************************************************************
      SUBROUTINE FTTRUF(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M,2,0:N/2-1),Y(M,0:N/2-1,2)
      DIMENSION T(0:N/2-1,4),IT(5)

      L=N/2

      DO 20 J=0,L-1
        DO 10 I=1,M
          Y(I,J,1)=X(I,1,J)
          Y(I,J,2)=X(I,2,J)
   10   CONTINUE
   20 CONTINUE

      CALL FTTZLM(M,L,Y,X,IT,T)

      R=1D0/N
      S=R/2

      DO 30 I=1,M
        X(I,1,0)=R*(Y(I,0,1)+Y(I,0,2))
        X(I,2,0)=R*(Y(I,0,1)-Y(I,0,2))
   30 CONTINUE

      DO 50 J=1,L-1
        DO 40 I=1,M
          X(I,1,J)=S*(        (Y(I,L-J,1)+Y(I,J,1))
     &                +T(J,3)*(Y(I,L-J,2)+Y(I,J,2))
     &                -T(J,4)*(Y(I,L-J,1)-Y(I,J,1)))
          X(I,2,J)=S*(        (Y(I,L-J,2)-Y(I,J,2))
     &                -T(J,3)*(Y(I,L-J,1)-Y(I,J,1))
     &                -T(J,4)*(Y(I,L-J,2)+Y(I,J,2)))
   40   CONTINUE
   50 CONTINUE

      END
************************************************************************
      SUBROUTINE FTTRUB(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M,2,0:N/2-1),Y(M,0:N/2-1,2)
      DIMENSION T(0:N/2-1,4),IT(5)

      L=N/2

      DO 10 I=1,M
        Y(I,0,1)=X(I,1,0)+X(I,2,0)
        Y(I,0,2)=X(I,1,0)-X(I,2,0)
   10 CONTINUE

      DO 30 J=1,L-1
        DO 20 I=1,M
          Y(I,J,1)=        (X(I,1,L-J)+X(I,1,J))
     &             -T(J,3)*(X(I,2,L-J)+X(I,2,J))
     &             +T(J,4)*(X(I,1,L-J)-X(I,1,J))
          Y(I,J,2)=       -(X(I,2,L-J)-X(I,2,J))
     &             -T(J,3)*(X(I,1,L-J)-X(I,1,J))
     &             -T(J,4)*(X(I,2,L-J)+X(I,2,J))
   20   CONTINUE
   30 CONTINUE

      CALL FTTZLM(M,L,Y,X,IT,T)

      DO 50 J=0,L-1
        DO 40 I=1,M
          X(I,1,J)=Y(I,J,1)
          X(I,2,J)=Y(I,J,2)
   40   CONTINUE
   50 CONTINUE

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
*     SINE TRANSFORM (MID-POINT)                     2000/09/19 K.Ishioka      
*************************************************************************
************************************************************************
      SUBROUTINE FTTSMI(N,IT,T)
 
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)      
      DIMENSION T(0:N/2-1,12),IT(5)

      CALL FTTRUI(N,IT,T)
 
      N2=N*2

      DO I=0,N/2-1
        T(I,5)=SIN(2*PI*(2*I+1)/N2)        
        T(I,6)=COS(2*PI*(2*I+1)/N2)
        T(I,7)=COS(PI*I/N)
        T(I,8)=SIN(PI*I/N)
        T(I,9)=SIN(PI*(4*I+1)/N2)
        T(I,10)=SIN(PI*(4*I+3)/N2)
        T(I,11)=1/T(I,9)
        T(I,12)=1/T(I,10)
      END DO
 
      END
************************************************************************
      SUBROUTINE FTTSMF(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,12)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)
 
      DO J=0,N/2-1
        J1=2*J
        J2=N-1-J1
        J3=2*J+1
        J4=N-1-J3
        DO I=1,M
          Y(I,2*J)=-(X(I,J1)-X(I,J2))+2*T(J,9 )*(X(I,J1)+X(I,J2))
          Y(I,2*J+1)=-(X(I,J3)-X(I,J4))+2*T(J,10)*(X(I,J3)+X(I,J4))
        END DO
      END DO

      CALL FTTRUF(M,N,Y,X,IT,T)
 
      DO I=1,M
        X(I,0)=0.5D0*Y(I,0)
        X(I,N-1)=-Y(I,1)
      END DO
 
      DO J=1,N/2-1
        DO I=1,M
          X(I,2*J-1)=T(J,7)*Y(I,2*J+1)-T(J,8)*Y(I,2*J)
          X(I,2*J)=X(I,2*J-2)+T(J,7)*Y(I,2*J)+T(J,8)*Y(I,2*J+1)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE FTTSMB(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,12)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)

      DO I=1,M
        Y(I,0)=2*X(I,0)
        Y(I,1)=-X(I,N-1)
      END DO
 
      DO J=1,N/2-1
        DO I=1,M
          Y(I,2*J)=(X(I,2*J)-X(I,2*J-2))*T(J,7)-T(J,8)*X(I,2*J-1)
          Y(I,2*J+1)=(X(I,2*J)-X(I,2*J-2))*T(J,8)+T(J,7)*X(I,2*J-1)
        END DO
      END DO
      
      CALL FTTRUB(M,N,Y,X,IT,T)

      DO J=0,N/2-1
        DO I=1,M
          X(I,2*J  )=-0.25D0*(Y(I,2*J)-Y(I,2*(N/2-1-J)+1))
     &      +0.125D0*(Y(I,2*J)+Y(I,2*(N/2-1-J)+1))*T(J,11)
          X(I,2*J+1)=-0.25D0*(Y(I,2*J+1)-Y(I,2*(N/2-1-J)))
     &      +0.125D0*(Y(I,2*J+1)+Y(I,2*(N/2-1-J)))*T(J,12)
        END DO
      END DO
      
      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
*     SINE TRANSFORM (TRAPEZOIDAL)                   2000/09/19 K.Ishioka      
*************************************************************************
************************************************************************
      SUBROUTINE FTTSTI(N,IT,T)
 
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)      
      DIMENSION T(0:N/2-1,5),IT(5)

      CALL FTTRUI(N,IT,T)
 
      N2=N*2
 
      DO I=0,N/2-1
        T(I,5)=SIN(2*PI*(2*I+1)/N2)
      END DO
 
      END
************************************************************************
      SUBROUTINE FTTSTF(M,N,X,Y,IT,T)
 
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,5)
      DIMENSION X(M,N),Y(M,0:N-1)

      DO J=1,N/2-1
        J1=2*J
        J2=N-J1
        J3=2*J+1
        J4=N-J3
        DO I=1,M
          Y(I,J1)=X(I,J2)-X(I,J1)+2*T(J,4)*(X(I,J2)+X(I,J1))
          Y(I,J3)=X(I,J4)-X(I,J3)+2*T(J,5)*(X(I,J4)+X(I,J3))
        END DO
      END DO
 
      J=0
      J3=2*J+1
      J4=N-J3
      DO I=1,M
        Y(I,0)=0
        Y(I,1)=X(I,J4)-X(I,J3)+2*T(J,5)*(X(I,J4)+X(I,J3))
      END DO
 
      CALL FTTRUF(M,N,Y,X,IT,T) 
 
      DO I=1,M
        X(I,N)=0
        X(I,1)=0.5D0*Y(I,0)
*        X(I,N-1)=-0.5D0*Y(I,1)
      END DO
 
      DO J=1,N/2-1
        DO I=1,M
          X(I,2*J)=Y(I,2*J+1)
          X(I,2*J+1)=X(I,2*J-1)+Y(I,2*J)
        END DO
      END DO
 
      END
************************************************************************
      SUBROUTINE FTTSTB(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(0:N/2-1,5)
      DIMENSION X(M,0:N-1),Y(M,0:N-1)

      CALL FTTSTF(M,N,X,Y,IT,T)

      DO J=0,N-1
        DO I=1,M
          X(I,J)=(N/2D0)*X(I,J)
        END DO
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
      SUBROUTINE FTTZUI(N,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION T(0:N-1,2),IT(5)

      CALL FTTZLI(N,IT,T)

      END
************************************************************************
      SUBROUTINE FTTZUF(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M*N,2),Y(M*N,2)
      DIMENSION T(0:N-1,2),IT(5)

      F=1D0/N

      DO I=1,M*N
        X(I,2)=-X(I,2)
      END DO
      CALL FTTZLM(M,N,X,Y,IT,T)
      DO I=1,M*N
        X(I,1)= X(I,1)*F
        X(I,2)=-X(I,2)*F
      END DO

      END
************************************************************************
      SUBROUTINE FTTZUB(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M*N,2),Y(M*N,2)
      DIMENSION T(0:N-1,2),IT(5)

      CALL FTTZLM(M,N,X,Y,IT,T)

      END
************************************************************************
      SUBROUTINE FTTZLI(N,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION T(0:N-1,2),IT(5)

      IF(N.LE.0) THEN
        CALL FTDMSG('E','FTTZLI','N MUST BE .GT. 0')
      END IF

      J=N
      DO 20 I=5,2,-1
        IT(I)=0
   10   CONTINUE
        K=MOD(J,I)
        IF(K.EQ.0) THEN
          IT(I)=IT(I)+1
          J=J/I
          GO TO 10
        END IF
   20 CONTINUE

      IF(J.NE.1) THEN
        CALL FTDMSG('E','FTTZLI','N.NE.(2**P)*(3**Q)*(5**R)')
      END IF

      IT(1)=MOD(IT(2)+IT(3)+IT(4)+IT(5),2)

      IF(IT(1).EQ.1.AND.IT(4).GE.1) THEN
        IT(2)=IT(2)+2
        IT(4)=IT(4)-1
        IT(1)=0
      END IF

      DO 30 I=0,N-1
        T(I,1)=COS(2*PI*I/N)
        T(I,2)=SIN(2*PI*I/N)
   30 CONTINUE

      END
************************************************************************
      SUBROUTINE FTTZLM(M,N,X,Y,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M*N,2),Y(M*N,2)
      DIMENSION T(0:N-1,2),IT(5)

      K=N
      L=1
      IP=1

      DO 10 I=1,IT(1)
        CALL FTTZL1(M,N,X,Y)
        IP=-IP
   10 CONTINUE

      DO 20 I=1,IT(2)
        IF(IP.EQ. 1) CALL FTTZL2(M,K,L,X(1,1),X(1,2),Y(1,1),Y(1,2),T)
        IF(IP.EQ.-1) CALL FTTZL2(M,K,L,Y(1,1),Y(1,2),X(1,1),X(1,2),T)
        IP=-IP
   20 CONTINUE

      DO 30 I=1,IT(3)
        IF(IP.EQ. 1) CALL FTTZL3(M,K,L,X(1,1),X(1,2),Y(1,1),Y(1,2),T)
        IF(IP.EQ.-1) CALL FTTZL3(M,K,L,Y(1,1),Y(1,2),X(1,1),X(1,2),T)
        IP=-IP
   30 CONTINUE

      DO 40 I=1,IT(4)
        IF(IP.EQ. 1) CALL FTTZL4(M,K,L,X(1,1),X(1,2),Y(1,1),Y(1,2),T)
        IF(IP.EQ.-1) CALL FTTZL4(M,K,L,Y(1,1),Y(1,2),X(1,1),X(1,2),T)
        IP=-IP
   40 CONTINUE

      DO 50 I=1,IT(5)
        IF(IP.EQ. 1) CALL FTTZL5(M,K,L,X(1,1),X(1,2),Y(1,1),Y(1,2),T)
        IF(IP.EQ.-1) CALL FTTZL5(M,K,L,Y(1,1),Y(1,2),X(1,1),X(1,2),T)
        IP=-IP
   50 CONTINUE

      END
************************************************************************
*     FT BY FACTOR 1
*-----------------------------------------------------------------------
      SUBROUTINE FTTZL1(M,N,X,Y)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(M*N*2),Y(M*N*2)

      DO 10 I=1,M*N*2
        Y(I)=X(I)
   10 CONTINUE

      END
************************************************************************
*     FT BY FACTOR 2
*-----------------------------------------------------------------------
      SUBROUTINE FTTZL2(M,K,L,A,B,C,D,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M*L,0:K/2-1,0:1),B(M*L,0:K/2-1,0:1)
      DIMENSION C(M*L,0:1,0:K/2-1),D(M*L,0:1,0:K/2-1)
      DIMENSION T(0:L-1,0:K-1,2)

      DO 20 J=0,K/2-1
        DO 10 I=1,M*L
          C(I,0,J)=A(I,J,0)+A(I,J,1)
          D(I,0,J)=B(I,J,0)+B(I,J,1)
          C(I,1,J)= T(0,1*J,1)*(A(I,J,0)-A(I,J,1))
     &             -T(0,1*J,2)*(B(I,J,0)-B(I,J,1))
          D(I,1,J)= T(0,1*J,1)*(B(I,J,0)-B(I,J,1))
     &             +T(0,1*J,2)*(A(I,J,0)-A(I,J,1))
   10   CONTINUE
   20 CONTINUE

      K=K/2
      L=L*2

      END
************************************************************************
*     FT BY FACTOR 3
*-----------------------------------------------------------------------
*     C1 = COS(PI/3), S1 = SIN(PI/3)
*-----------------------------------------------------------------------
      SUBROUTINE FTTZL3(M,K,L,A,B,C,D,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(C1=0.5D0,S1=0.86602540378443864676D0)
      DIMENSION A(M*L,0:K/3-1,0:2),B(M*L,0:K/3-1,0:2)
      DIMENSION C(M*L,0:2,0:K/3-1),D(M*L,0:2,0:K/3-1)
      DIMENSION T(0:L-1,0:K-1,2)

      DO 20 J=0,K/3-1
        DO 10 I=1,M*L
          C(I,0,J)=A(I,J,0)+(A(I,J,1)+A(I,J,2))
          D(I,0,J)=B(I,J,0)+(B(I,J,1)+B(I,J,2))
          C(I,1,J)=
     &      T(0,1*J,1)*(
     &        (A(I,J,0)-C1*(A(I,J,1)+A(I,J,2)))-S1*(B(I,J,1)-B(I,J,2)))
     &     -T(0,1*J,2)*(
     &        (B(I,J,0)-C1*(B(I,J,1)+B(I,J,2)))+S1*(A(I,J,1)-A(I,J,2)))
          D(I,1,J)=
     &      T(0,1*J,1)*(
     &        (B(I,J,0)-C1*(B(I,J,1)+B(I,J,2)))+S1*(A(I,J,1)-A(I,J,2)))
     &     +T(0,1*J,2)*(
     &        (A(I,J,0)-C1*(A(I,J,1)+A(I,J,2)))-S1*(B(I,J,1)-B(I,J,2)))
          C(I,2,J)=
     &      T(0,2*J,1)*(
     &        (A(I,J,0)-C1*(A(I,J,1)+A(I,J,2)))+S1*(B(I,J,1)-B(I,J,2)))
     &     -T(0,2*J,2)*(
     &        (B(I,J,0)-C1*(B(I,J,1)+B(I,J,2)))-S1*(A(I,J,1)-A(I,J,2)))
          D(I,2,J)=
     &      T(0,2*J,1)*(
     &        (B(I,J,0)-C1*(B(I,J,1)+B(I,J,2)))-S1*(A(I,J,1)-A(I,J,2)))
     &     +T(0,2*J,2)*(
     &        (A(I,J,0)-C1*(A(I,J,1)+A(I,J,2)))+S1*(B(I,J,1)-B(I,J,2)))
   10   CONTINUE
   20 CONTINUE

      K=K/3
      L=L*3

      END
************************************************************************
*     FT BY FACTOR 4
*-----------------------------------------------------------------------
      SUBROUTINE FTTZL4(M,K,L,A,B,C,D,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M*L,0:K/4-1,0:3),B(M*L,0:K/4-1,0:3)
      DIMENSION C(M*L,0:3,0:K/4-1),D(M*L,0:3,0:K/4-1)
      DIMENSION T(0:L-1,0:K-1,2)

      DO 20 J=0,K/4-1
        DO 10 I=1,M*L
          C(I,0,J)=(A(I,J,0)+A(I,J,2))+(A(I,J,1)+A(I,J,3))
          D(I,0,J)=(B(I,J,0)+B(I,J,2))+(B(I,J,1)+B(I,J,3))
          C(I,2,J)=
     &        T(0,2*J,1)*((A(I,J,0)+A(I,J,2))-(A(I,J,1)+A(I,J,3)))
     &       -T(0,2*J,2)*((B(I,J,0)+B(I,J,2))-(B(I,J,1)+B(I,J,3)))
          D(I,2,J)=
     &        T(0,2*J,1)*((B(I,J,0)+B(I,J,2))-(B(I,J,1)+B(I,J,3)))
     &       +T(0,2*J,2)*((A(I,J,0)+A(I,J,2))-(A(I,J,1)+A(I,J,3)))
          C(I,1,J)=
     &        T(0,1*J,1)*((A(I,J,0)-A(I,J,2))-(B(I,J,1)-B(I,J,3)))
     &       -T(0,1*J,2)*((B(I,J,0)-B(I,J,2))+(A(I,J,1)-A(I,J,3)))
          D(I,1,J)=
     &        T(0,1*J,1)*((B(I,J,0)-B(I,J,2))+(A(I,J,1)-A(I,J,3)))
     &       +T(0,1*J,2)*((A(I,J,0)-A(I,J,2))-(B(I,J,1)-B(I,J,3)))
          C(I,3,J)=
     &        T(0,3*J,1)*((A(I,J,0)-A(I,J,2))+(B(I,J,1)-B(I,J,3)))
     &       -T(0,3*J,2)*((B(I,J,0)-B(I,J,2))-(A(I,J,1)-A(I,J,3)))
          D(I,3,J)=
     &        T(0,3*J,1)*((B(I,J,0)-B(I,J,2))-(A(I,J,1)-A(I,J,3)))
     &       +T(0,3*J,2)*((A(I,J,0)-A(I,J,2))+(B(I,J,1)-B(I,J,3)))
   10   CONTINUE
   20 CONTINUE

      K=K/4
      L=L*4

      END
************************************************************************
*     FT BY FACTOR 5
*-----------------------------------------------------------------------
*     C1 = COS(PI/5), S1 = SIN(PI/5), C2 = COS(2*PI/5), S2 = SIN(2*PI/5)
*-----------------------------------------------------------------------
      SUBROUTINE FTTZL5(M,K,L,A,B,C,D,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(C1=0.80901699437494742410D0,S1=0.58778525229247312917D0)
      PARAMETER(C2=0.30901699437494742410D0,S2=0.95105651629515357212D0)
      DIMENSION A(M*L,0:K/5-1,0:4),B(M*L,0:K/5-1,0:4)
      DIMENSION C(M*L,0:4,0:K/5-1),D(M*L,0:4,0:K/5-1)
      DIMENSION T(0:L-1,0:K-1,2)

      DO 20 J=0,K/5-1
        DO 10 I=1,M*L
          C(I,0,J)=A(I,J,0)+(A(I,J,1)+A(I,J,4))+(A(I,J,2)+A(I,J,3))
          D(I,0,J)=B(I,J,0)+(B(I,J,1)+B(I,J,4))+(B(I,J,2)+B(I,J,3))
          C(I,1,J)=
     &      T(0,1*J,1)*(
     &       (A(I,J,0)+(C2*(A(I,J,1)+A(I,J,4))-C1*(A(I,J,2)+A(I,J,3)))
     &                -(S2*(B(I,J,1)-B(I,J,4))+S1*(B(I,J,2)-B(I,J,3)))))
     &     -T(0,1*J,2)*(
     &       (B(I,J,0)+(C2*(B(I,J,1)+B(I,J,4))-C1*(B(I,J,2)+B(I,J,3)))
     &                +(S2*(A(I,J,1)-A(I,J,4))+S1*(A(I,J,2)-A(I,J,3)))))
          D(I,1,J)=
     &      T(0,1*J,1)*(
     &       (B(I,J,0)+(C2*(B(I,J,1)+B(I,J,4))-C1*(B(I,J,2)+B(I,J,3)))
     &                +(S2*(A(I,J,1)-A(I,J,4))+S1*(A(I,J,2)-A(I,J,3)))))
     &     +T(0,1*J,2)*(
     &       (A(I,J,0)+(C2*(A(I,J,1)+A(I,J,4))-C1*(A(I,J,2)+A(I,J,3)))
     &                -(S2*(B(I,J,1)-B(I,J,4))+S1*(B(I,J,2)-B(I,J,3)))))
          C(I,4,J)=
     &      T(0,4*J,1)*(
     &       (A(I,J,0)+(C2*(A(I,J,1)+A(I,J,4))-C1*(A(I,J,2)+A(I,J,3)))
     &                +(S2*(B(I,J,1)-B(I,J,4))+S1*(B(I,J,2)-B(I,J,3)))))
     &     -T(0,4*J,2)*(
     &       (B(I,J,0)+(C2*(B(I,J,1)+B(I,J,4))-C1*(B(I,J,2)+B(I,J,3)))
     &                -(S2*(A(I,J,1)-A(I,J,4))+S1*(A(I,J,2)-A(I,J,3)))))
          D(I,4,J)=
     &      T(0,4*J,1)*(
     &       (B(I,J,0)+(C2*(B(I,J,1)+B(I,J,4))-C1*(B(I,J,2)+B(I,J,3)))
     &                -(S2*(A(I,J,1)-A(I,J,4))+S1*(A(I,J,2)-A(I,J,3)))))
     &     +T(0,4*J,2)*(
     &       (A(I,J,0)+(C2*(A(I,J,1)+A(I,J,4))-C1*(A(I,J,2)+A(I,J,3)))
     &                +(S2*(B(I,J,1)-B(I,J,4))+S1*(B(I,J,2)-B(I,J,3)))))
          C(I,2,J)=
     &      T(0,2*J,1)*(
     &       (A(I,J,0)-(C1*(A(I,J,1)+A(I,J,4))-C2*(A(I,J,2)+A(I,J,3)))
     &                -(S1*(B(I,J,1)-B(I,J,4))-S2*(B(I,J,2)-B(I,J,3)))))
     &     -T(0,2*J,2)*(
     &       (B(I,J,0)-(C1*(B(I,J,1)+B(I,J,4))-C2*(B(I,J,2)+B(I,J,3)))
     &                +(S1*(A(I,J,1)-A(I,J,4))-S2*(A(I,J,2)-A(I,J,3)))))
          D(I,2,J)=
     &      T(0,2*J,1)*(
     &       (B(I,J,0)-(C1*(B(I,J,1)+B(I,J,4))-C2*(B(I,J,2)+B(I,J,3)))
     &                +(S1*(A(I,J,1)-A(I,J,4))-S2*(A(I,J,2)-A(I,J,3)))))
     &     +T(0,2*J,2)*(
     &       (A(I,J,0)-(C1*(A(I,J,1)+A(I,J,4))-C2*(A(I,J,2)+A(I,J,3)))
     &                -(S1*(B(I,J,1)-B(I,J,4))-S2*(B(I,J,2)-B(I,J,3)))))
          C(I,3,J)=
     &      T(0,3*J,1)*(
     &       (A(I,J,0)-(C1*(A(I,J,1)+A(I,J,4))-C2*(A(I,J,2)+A(I,J,3)))
     &                +(S1*(B(I,J,1)-B(I,J,4))-S2*(B(I,J,2)-B(I,J,3)))))
     &     -T(0,3*J,2)*(
     &       (B(I,J,0)-(C1*(B(I,J,1)+B(I,J,4))-C2*(B(I,J,2)+B(I,J,3)))
     &                -(S1*(A(I,J,1)-A(I,J,4))-S2*(A(I,J,2)-A(I,J,3)))))
          D(I,3,J)=
     &      T(0,3*J,1)*(
     &       (B(I,J,0)-(C1*(B(I,J,1)+B(I,J,4))-C2*(B(I,J,2)+B(I,J,3)))
     &                -(S1*(A(I,J,1)-A(I,J,4))-S2*(A(I,J,2)-A(I,J,3)))))
     &     +T(0,3*J,2)*(
     &       (A(I,J,0)-(C1*(A(I,J,1)+A(I,J,4))-C2*(A(I,J,2)+A(I,J,3)))
     &                +(S1*(B(I,J,1)-B(I,J,4))-S2*(B(I,J,2)-B(I,J,3)))))
   10   CONTINUE
   20 CONTINUE

      K=K/5
      L=L*5

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1998 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
***********************************************************************
*     CALCULATE GAUSSIAN LATITUDES AND WEIGHTS                 98/02/13
***********************************************************************
*     X(J): sin(\phi_j)
*     W(J): w_j/2
***********************************************************************
      SUBROUTINE SNGAUS(JM,X,W)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(PI=3.1415926535897932385D0)
      PARAMETER(NB=64)
      DIMENSION X(JM/2),W(JM/2),E(NB)

      JH=JM/2

      EPS=1
      DO I=1,NB
        EPS=EPS/2
        E(I)=EPS+1
      END DO

      I=0
      EPS=1
   10 CONTINUE
        I=I+1
        EPS=EPS/2
      IF(E(I).GT.1) GOTO 10

      EPS=EPS*16
      
      DO J=1,JH
        Z=SIN(PI*(2*J-1)/(2*JM+1))
        IFLAG=0
   20   CONTINUE
          P0=0
          P1=1
          DO N=1,JM-1,2
            P0=((2*N-1)*Z*P1-(N-1)*P0)/N
            P1=((2*N+1)*Z*P0-N*P1)/(N+1)
          END DO
          DP=JM*(P0-Z*P1)/(1-Z*Z)
          DZ=P1/DP
          Z=Z-DZ
        IF(IFLAG.EQ.0) THEN
          IF(ABS(DZ/Z).LE.EPS) THEN
            IFLAG=1
            X(J)=Z
          END IF
          GOTO 20
        END IF
        W(J)=1/(DP*DP)/(1-X(J)*X(J))
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1999 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     INITIALIZATION OF SNPACK                                  99/03/20
************************************************************************
      SUBROUTINE SNINIT(MM,IM,JM,IT,T,Y,IP,P,R,IA,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(-(MM+1)/2:MM,2)
      DIMENSION P(-(MM+1)/2:MM,2,JM/2)
      DIMENSION R(-(MM+1)/2:(MM+1)/2+2,0:MM/2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

*/ FFTに使う配列の初期化 /*

      CALL SNINI1(IM,IT,T)

*/ ガウス緯度およびウェイトの初期化 /*

      CALL SNINI2(JM,Y)

*/ Legendre陪関数計算のための係数および初期値の設定 /*

      CALL SNINI3(MM,JM,Y,IP,P,R,IA,A)

      END
************************************************************************
*     FFTに使う配列の初期化
************************************************************************
      SUBROUTINE SNINI1(IM,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IT(5),T(IM*2)

      CALL FTTRUI(IM,IT,T)

      END
************************************************************************
*     ガウス緯度およびウェイトの初期化
************************************************************************
      SUBROUTINE SNINI2(JM,Y)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM/2,4)

      IF(MOD(JM,2).NE.0) THEN
        CALL BSDMSG('E','SNINI2','JM MUST BE EVEN.')
      END IF

      JH=JM/2

      CALL SNGAUS(JM,Y(1,1),Y(1,2))

      DO J=1,JH
        Y(J,3)=SQRT(1-Y(J,1)*Y(J,1))
        Y(J,4)=1/Y(J,3)
      END DO

      END
************************************************************************
*     Legendre陪関数計算のための係数および初期値の設定
************************************************************************
      SUBROUTINE SNINI3(MM,JM,Y,IP,P,R,IA,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(-(MM+1)/2:MM,2),IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION P(-(MM+1)/2:MM,2,JM/2)
      DIMENSION R(-(MM+1)/2:(MM+1)/2+2,0:MM/2)

      EPSL(N,M)=SQRT((1D0*N*N-M*M)/(4D0*N*N-1))

*/ 便宜上用いる定数の設定

      MMD=MM/2
      MMP=(MM+1)/2
      JH=JM/2

*/    漸化式に使う係数(準備)  /*

      CALL BSSET0((2*MMP+3)*(MMD+1),R)

      DO N=0,MM
        CALL SNNMKL(MM,N,N,K,L)
        R(K,L)=1
      END DO

      DO N=1,MM
        M=N-1
        CALL SNNMKL(MM,N,M,K,L)
        R(K,L)=1/EPSL(N,M)
      END DO

      DO M=0,MM-2
        DO N=M+2,MM
          CALL SNNMKL(MM,N,M,K,L)
          CALL SNNMKL(MM,N-2,M,KD,LD)
          R(K,L)=-R(KD,LD)*EPSL(N-1,M)/EPSL(N,M)
        END DO
      END DO

*/    スペクトル変数格納位置変換のためのリストベクトル  /*

      DO N=0,MM
        DO M=-N,N
          CALL SNNM2L(N,M,L)
          MD=ABS(M)
          CALL SNNMKL(MM,N,MD,K,LD)
          A(L,1)=R(K,LD)
          IF(M.GE.0) THEN
            CALL SNKLIA(MM,K,LD,1,IA(L,1))
          ELSE
            CALL SNKLIA(MM,K,LD,2,IA(L,1))
          END IF
        END DO
      END DO

*/    リストベクトルおよび係数(経度微分用) /*

      DO N=0,MM
        DO M=-N,N
          CALL SNNM2L(N,M,L)
          MD=ABS(M)
          CALL SNNMKL(MM,N,MD,K,LD)
          A(L,2)=M*A(L,1)
          IF(M.GE.0) THEN
            CALL SNKLIA(MM,K,LD,2,IA(L,2))
          ELSE
            CALL SNKLIA(MM,K,LD,1,IA(L,2))
          END IF
        END DO
      END DO

*/    リストベクトル(緯度微分用) /*

      DO N=0,MM
        DO M=-N,N
          CALL SNNM2L(N,M,L)
          MD=ABS(M)
          CALL SNNMKL(MM,N,MD,K,LD)
          CALL SNNMKL(MM,N-1,MD,K1,LD1)
          CALL SNNMKL(MM,N+1,MD,K2,LD2)
          IF(M.GE.0) THEN
            CALL SNKLIA(MM,K1,LD1,1,IA(L,3))
            CALL SNKLIA(MM,K2,LD2,1,IA(L,4))
          ELSE
            CALL SNKLIA(MM,K1,LD1,2,IA(L,3))
            CALL SNKLIA(MM,K2,LD2,2,IA(L,4))
          END IF
          A(L,6)=(-1)**(N-MD)/R(K,LD)
          A(L,4)=-N*A(L,6)
          IF(MD.EQ.N) THEN
            A(L,3)=0
            A(L,5)=0
          ELSE
            A(L,5)=-(-1)**(N-MD)/R(K,LD)
            A(L,3)=(N+1)*A(L,5)
          END IF
        END DO
      END DO

*/    漸化式に使う係数  /*

      DO N=0,MM
        DO M=0,N
          CALL SNNMKL(MM,N,M,K,L)
          R(K,L)=(-1)**(N-M)*R(K,L)*R(K,L)
        END DO
      END DO

*/    Legendre陪関数の初期値(マイナス側)  /*
*/    (マイナス側はn=MM/2までの P^m_m成分, ただし, M=m-1)
*/    3次元目の添字は 1: ベースの初期値, 2: ベースからnを一つ増した初期値

      DO J=1,JH
        P(-1,1,J)=1
      END DO

      DO M=1,MMP-1
        ALPHA=SQRT(1D0*(2*M+1)/(2*M))
        DO J=1,JH
          P(-M-1,1,J)=ALPHA*P(-M,1,J)*Y(J,3)
        END DO
      END DO

      DO M=0,MMP-1
        DO J=1,JH
          P(-M-1,2,J)=Y(J,1)*P(-M-1,1,J)
        END DO
      END DO
      
*/    Legendre陪関数の初期値のプラス側の計算の準備  /*

      DO J=1,JH
        P(0,1,J)=1
      END DO

      DO M=1,MM
        ALPHA=SQRT(1D0*(2*M+1)/(2*M))
        DO J=1,JH
          P(M,1,J)=ALPHA*P(M-1,1,J)*Y(J,3)
        END DO
      END DO

*/    Legendre陪関数の初期値のプラス側の半分の計算  /*

      DO J=1,JH
        DO M=0,MMP
          P0=0
          P1=P(M,1,J)
          DO N=M+1,MMP,2
            CALL SNNMKL(MM,N-1,M,K,L)
            P0=P0+R(K,L)*P1*Y(J,1)
            CALL SNNMKL(MM,N,M,K,L)
            P1=P1+R(K,L)*P0*Y(J,1)
          END DO
          IF(MOD(MMP-M,2).EQ.1) THEN
            P(M,1,J)=P0
            P(M,2,J)=P1
          ELSE
            N=MMP
            CALL SNNMKL(MM,N,M,K,L)
            P0=P0+R(K,L)*P1*Y(J,1)
            P(M,1,J)=P1
            P(M,2,J)=P0
          END IF
        END DO
      END DO

*/    Legendre陪関数の初期値のプラス側の残りの半分の計算  /*
*/    (漸化式のために交互に0を入れておく /*

      DO J=1,JH
        DO M=MMP+1,MM-1,2
          P(M,2,J)=P(M,1,J)
          P(M,1,J)=0
          P(M+1,2,J)=0
        END DO
        IF(MOD(MM-MMP,2).NE.0) THEN
          P(MM,2,J)=P(M,1,J)
          P(MM,1,J)=0
        END IF
      END DO

*/    パリティ変数  /*

      DO M=-MMP,MM
        IP(M,1)=1
      END DO
      
      IF(MOD(MMP,2).EQ.0) THEN
        DO M=1,MM,2
          IP(M,1)=-1
        END DO
      ELSE
        DO M=0,MM,2
          IP(M,1)=-1
        END DO
      END IF

      DO M=-MMP,MM
        IP(M,2)=IP(M,1)
      END DO

      END
************************************************************************
      SUBROUTINE SNKLNM(MM,K,L,N,M)

      MMP=(MM+1)/2

      IF(K.GE.-L) THEN
        N=L+MMP
        M=K+L
      ELSE
        N=-K-1
        M=-K-L-1
      END IF

      END
************************************************************************
      SUBROUTINE SNNMKL(MM,N,M,K,L)

      MMP=(MM+1)/2
      MMD=MM/2

      IF(M.GT.N) THEN
        IF(N+1.LE.MMD) THEN
          K=-MMP-1
          L=N+1
        ELSE
          K=MMP+1
          L=N-MMD
        END IF
      ELSE
        IF(N.GE.MMP) THEN
          L=N-MMP
          K=M-L
        ELSE
          L=N-M
          K=-N-1
        END IF
      END IF

      END
************************************************************************
      SUBROUTINE SNKLIA(MM,K,L,I,IA)

      MMP=(MM+1)/2
      MMD=MM/2

      ND1=MMP*2+3
      ND2=MMD+2

      IA=1+((I-1)*ND2+L)*ND1+K+MMP+1

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1999 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     DUPLICATE COEFFICIENTS                                    99/02/22
************************************************************************
      SUBROUTINE SNKINI(MM,JM,KM,IP,P,R,IPK,PK,RK)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IPK(KM*((MM+1)/2+MM+1)*2)
      DIMENSION PK(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION RK(KM*((MM+1)/2*2+3)*(MM/2+1))

      CALL SNKCPR(((MM+1)/2+MM+1)*JM,KM,P,PK)
      CALL SNKCPR(((MM+1)/2*2+3)*(MM/2+1),KM,R,RK)
      CALL SNKCPI(((MM+1)/2+MM+1)*2,KM,IP,IPK)

      END
************************************************************************
      SUBROUTINE SNKCPR(N,K,A,AK)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),AK(K,N)

      DO J=1,K
        DO I=1,N
          AK(J,I)=A(I)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNKCPI(N,K,IA,IAK)

      DIMENSION IA(N),IAK(K,N)

      DO J=1,K
        DO I=1,N
          IAK(J,I)=IA(I)
        END DO
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1999 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
************************************************************************
*     CALCULATE THE POSITION OF A SPECTRUM COEFFICIENT OF P_N^M 99/02/09
*-----------------------------------------------------------------------
*     IF M.GE.0 --> REAL PART
*     IF M.LT.0 --> IMAGINARY PART
************************************************************************
      SUBROUTINE SNNM2L(N,M,L)

      L=M+1+N*(N+1)

      END
************************************************************************
      SUBROUTINE SNL2NM(L,N,M)

      N=SQRT(1D0*(L-1))
      M=L-N*(N+1)-1

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1999--2005 Keiichi Ishioka                            *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
*     SPECTRAL TRANSFORM USING OPENMP                          2005/07/01
************************************************************************      
*     TRANSFORM SPECTRA TO GRID
*-----------------------------------------------------------------------
*     WS, WW はあちこちで作業領域として使用されるため,
*
*        KM*(IM+MM+1)*3*JM/2
*
*     以上の領域を確保しておくこと. WV は
*
*        KM*(MM+4)*(MM+3)*NP
*      
*     以上の領域を確保しておくこと. ここに, NP は利用しうる thread 数.
************************************************************************      
      SUBROUTINE SNTSOG(MM,IM,ID,JM,KM,
     &  S,G,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,WV,IPOW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1)*KM)
      DIMENSION G(ID*JM*KM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION P(KM*((MM+1)/2+MM+1)*2,JM/2)
      DIMENSION R(KM*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(KM*((MM+1)/2+MM+1)*2,JM/2)
      DIMENSION WS(*),WW(*),WV(*)
!$    INTEGER omp_get_num_threads,omp_get_thread_num

      JH=JM/2
      IW=IM+MM+1

      CALL SNCSOG(MM,KM,S,WV,IA,A,WW,IFLAG)              

      NP=1
      I=0
!$omp parallel private(i,np,jp,jd,js,je,jc,is)
!$    NP=omp_get_num_threads()
!$    I=omp_get_thread_num()
      JP=(JH-1)/NP+1
      JD=JP*2+1
      JS=JP*I+1
      JE=MIN(JP*(I+1),JH)
      JC=(JE-JS+1)*2
      IS=IW*KM*JD*I+1
      IF(JE.GE.JS) THEN
        CALL SNLS2G(MM,JC,KM,WV,WW(IS),Y(JS,1),P(1,JS),R,Q(1,JS))
        CALL SNPSOG(MM,JC,JD,KM,WW(IS),WS(IS),IP,Y(JS,4),IPOW)
        CALL SNFS2G(MM,IM,JD,KM,WS(IS),WW(IS),IT,T)
        CALL SNGSOG(IM,ID,JM,JD,JS,JE,KM,WW(IS),G)
      END IF
!$omp end parallel      

      END
************************************************************************
      SUBROUTINE SNGSOG(IM,ID,JM,JD,JS,JE,KM,WW,G)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WW(JD,KM,IM/2,2)
      DIMENSION G(ID,JM,KM)

      JH=JM/2
      JCH=JE-JS+1

      DO K=1,KM
        DO I=1,IM/2
          DO J=JS,JE
            G(2*I-1,JH+J,K)=WW(JCH+J-JS+1,K,I,1)
            G(2*I,JH+J,K)=WW(JCH+J-JS+1,K,I,2)
            G(2*I-1,JH-J+1,K)=WW(JCH-(J-JS),K,I,1)
            G(2*I,JH-J+1,K)=WW(JCH-(J-JS),K,I,2)
          END DO
        END DO
        DO I=IM+1,ID
          DO J=JS,JE
            G(I,JH+J,K)=WW(JCH+J-JS+1,K,1,1)
            G(I,JH-J+1,K)=WW(JCH-(J-JS),K,1,1)            
          END DO
        END DO
      END DO
      
      END
************************************************************************
      SUBROUTINE SNPSOG(MM,JM,JD,KM,W,S,IP,Y4,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(KM*((MM+1)/2+MM+1)*2,JM/2,2)
      DIMENSION S(JD,KM*((MM+1)/2+MM+1)*2)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION Y4(JM/2)

      JH=JM/2

      DO J=1,JH
        DO K=1,KM*((MM+1)/2+MM+1)*2
          S(JH+J,K)=        (W(K,J,1)+W(K,J,2))*Y4(J)**IPOW
          S(JH-J+1,K)=IP(K)*(W(K,J,1)-W(K,J,2))*Y4(J)**IPOW
        END DO
      END DO

      DO J=JM+1,JD
        DO K=1,KM*((MM+1)/2+MM+1)*2
          S(J,K)=S(JM,K)
          S(J,K)=S(JM,K)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNCSOG(MM,KM,S,WS,IA,A,WW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1),KM)
      DIMENSION WS(KM,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION WW(KM,((MM+1)/2*2+3)*(MM/2+2)*2)

      LM=(MM+1)*(MM+1)
      CALL SNSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS)

*/    IFLAG=0: 微分なし, IFLAG=1: 緯度微分, IFLAG=-1: 経度微分
*/    IFLAG=2: μの演算

      IF(IFLAG.EQ.0) THEN
        DO K=1,KM
!$omp parallel do          
          DO L=1,LM
            WS(K,IA(L,1))=A(L,1)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
      ELSE IF(IFLAG.EQ.-1) THEN
        DO K=1,KM
!$omp parallel do          
          DO L=1,LM
            WS(K,IA(L,2))=A(L,2)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
      ELSE IF(IFLAG.EQ.1) THEN
        CALL SNSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WW)
        DO K=1,KM
!$omp parallel do          
          DO L=1,LM
            WW(K,IA(L,3))=A(L,3)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
        DO K=1,KM
!$omp parallel do                    
          DO L=1,LM
            WS(K,IA(L,4))=A(L,4)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
        CALL SNOADD(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS,WW)
      ELSE IF(IFLAG.EQ.2) THEN
        CALL SNSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WW)
        DO K=1,KM
!$omp parallel do          
          DO L=1,LM
            WW(K,IA(L,3))=A(L,5)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
        DO K=1,KM
!$omp parallel do          
          DO L=1,LM
            WS(K,IA(L,4))=A(L,6)*S(L,K)
          END DO
!$omp end parallel do          
        END DO
        CALL SNOADD(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS,WW)
      END IF

      END
************************************************************************
      SUBROUTINE SNOADD(N,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N)

!$omp parallel do
      DO I=1,N
        A(I)=A(I)+B(I)
      END DO
!$omp end parallel do
      
      END
************************************************************************
      SUBROUTINE SNSET0(N,A)
 
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N)

!$omp parallel do      
      DO I=1,N
        A(I)=0
      END DO
!$omp end parallel do      
 
      END
************************************************************************
*     TRANSFORM GRID TO SPECTRA
*-----------------------------------------------------------------------
*     WS, WW はあちこちで作業領域として使用されるため,
*
*        KM*(IM+MM+1)*3*JM/2
*
*     以上の領域を確保しておくこと. WV は
*
*        KM*(MM+4)*(MM+3)*NP
*      
*     以上の領域を確保しておくこと. ここに, NP は利用しうる thread 数.
************************************************************************
      SUBROUTINE SNTGOS(MM,IM,ID,JM,KM,
     &  G,S,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,WV,IPOW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(ID*JM*KM)
      DIMENSION S((MM+1)*(MM+1)*KM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(KM*((MM+1)/2+MM+1))
      DIMENSION P(KM*((MM+1)/2+MM+1)*2,JM/2)
      DIMENSION R(KM*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(KM*((MM+1)/2+MM+1)*2,JM/2)
      DIMENSION WS(*),WW(*),WV(*)
!$    INTEGER omp_get_num_threads,omp_get_thread_num
      
      JH=JM/2
      LH=KM*(MM+4)*(MM+3)
      IW=IM+MM+1

      NP=1
      I=0
!$omp parallel private(i,np,jp,jd,js,je,jc,is,ib,ls,le,ips,l,lp)
!$    NP=omp_get_num_threads()
!$    I=omp_get_thread_num()
      JP=(JH-1)/NP+1
      JD=JP*2+1
      JS=JP*I+1
      JE=MIN(JP*(I+1),JH)
      JC=(JE-JS+1)*2
      IS=IW*KM*JD*I+1
      IB=LH*I+1
      IF(JE.GE.JS) THEN
        CALL SNGGOS(IM,ID,JM,JD,JS,JE,KM,G,WW(IS))
        CALL SNFG2S(MM,IM,JD,KM,WW(IS),WS(IS),IT,T)
        CALL SNPGOS(MM,JC,JD,KM,WS(IS),WW(IS),IP,Y(JS,2),Y(JS,4),IPOW)
        CALL SNLG2S(MM,JC,KM,WW(IS),WV(IB),Y(JS,1),P(1,JS),R,Q(1,JS))
      END IF
!$omp barrier
      LP=(LH-1)/NP+1
      LS=LP*I+1
      LE=MIN(LP*(I+1),LH)
      IF(LE.GE.LS) THEN
        DO IPS=1,NP-1
          JS=JP*IPS+1
          JE=MIN(JP*(IPS+1),JH)
          IB=LH*IPS+1        
          IF(JE.GE.JS) THEN
            DO L=LS,LE
              WV(L)=WV(L)+WV(IB+L-1)
            END DO
          END IF
        END DO
      END IF
!$omp end parallel

      CALL SNCGOS(MM,KM,WV,S,IA,A,IFLAG)

      END
************************************************************************
      SUBROUTINE SNGGOS(IM,ID,JM,JD,JS,JE,KM,G,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(ID,JM,KM)
      DIMENSION WW(JD,KM,IM/2,2)

      JH=JM/2
      JCH=JE-JS+1

      DO K=1,KM
        DO I=1,IM/2
          DO J=JS,JE
            WW(JCH+J-JS+1,K,I,1)=G(2*I-1,JH+J,K)
            WW(JCH+J-JS+1,K,I,2)=G(2*I,JH+J,K)
            WW(JCH-(J-JS),K,I,1)=G(2*I-1,JH-J+1,K)
            WW(JCH-(J-JS),K,I,2)=G(2*I,JH-J+1,K)
          END DO
        END DO
      END DO

      END
************************************************************************      
      SUBROUTINE SNPGOS(MM,JM,JD,KM,S,W,IP,Y2,Y4,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(JD,KM*((MM+1)/2+MM+1)*2)
      DIMENSION W(KM*((MM+1)/2+MM+1)*2,JM/2,2)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION Y2(JM/2),Y4(JM/2)

      JH=JM/2

      DO J=1,JH
        DO K=1,KM*((MM+1)/2+MM+1)*2
          W(K,J,1)=(S(JH+J,K)+IP(K)*S(JH-J+1,K))*(Y2(J)*Y4(J)**IPOW)
          W(K,J,2)=(S(JH+J,K)-IP(K)*S(JH-J+1,K))*(Y2(J)*Y4(J)**IPOW)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNCGOS(MM,KM,WS,S,IA,A,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WS(KM,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION S((MM+1)*(MM+1),KM)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

      LM=(MM+1)*(MM+1)

*/    IFLAG=0: 微分なし, IFLAG=1: 緯度微分, IFLAG=-1: 経度微分
*/    IFLAG=2: μの演算

      IF(IFLAG.EQ.0) THEN
        DO K=1,KM
!$omp parallel do                    
          DO L=1,LM
            S(L,K)=A(L,1)*WS(K,IA(L,1))
          END DO
!$omp end parallel do
        END DO
      ELSE IF(IFLAG.EQ.-1) THEN
        DO K=1,KM
!$omp parallel do                    
          DO L=1,LM
            S(L,K)=-A(L,2)*WS(K,IA(L,2))
          END DO
!$omp end parallel do                
        END DO
      ELSE IF(IFLAG.EQ.1) THEN
        DO K=1,KM
!$omp parallel do                    
          DO L=1,LM
            S(L,K)=-A(L,3)*WS(K,IA(L,3))-A(L,4)*WS(K,IA(L,4))
          END DO
!$omp end parallel do                
        END DO
      ELSE IF(IFLAG.EQ.2) THEN
        DO K=1,KM
!$omp parallel do                    
          DO L=1,LM
            S(L,K)=A(L,5)*WS(K,IA(L,3))+A(L,6)*WS(K,IA(L,4))
          END DO
!$omp end parallel do                
        END DO
      END IF

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     SPECTRAL TRANSFORM                                      1999/03/04
************************************************************************
*     TRANSFORM SPECTRA TO GRID
*-----------------------------------------------------------------------
*     WS, WW はあちこちで作業領域として使用されるため,
*
*        KM*MAX(((MM+1)/2*2+3)*(MM/2+2)*2,JD*((MM+1)/2+MM+1)*2,JD*IM)
*
*     を満す大きさであること. または, さらに単純には, やや余裕をもって,
*
*        KM*MAX((MM+4)*(MM+3),JD*3*(MM+1),JD*IM)
*
*     としておいてもよい. ただし, Fortran90などで動的に領域を確保するの
*     でない限り, PARAMETER 文中でこのような値を MM,JD,IM から自動的に
*     設定できるようにするのは難しい( MAX のような関数が使えないため).
*     
*     しかし, 実際的な場合を考えると, 以下のように簡単に設定できる場合が
*     多い筈である. (安全のため, MM≧3 としておく).
*
*     1. 逆変換→正変換 で元のスペクトルデータへの復元が保証される条件
*        ( JD≧JM≧MM+1, IM≧2*MM+2 (IMは偶数より) )が満されている場合.
*
*        IW=IM+MM+1 とし, 作業領域を IW*JD*KM なる大きさにとればよい.
*
*     2. 2次の非線形項からのエリアジングを除く条件
*        ( JD≧JM≧3*MM/2D0, IM≧3*MM+1 )が満されている場合.
*
*        IW=IM+2 とし, 作業領域を IW*JD*KM なる大きさにとればよい.
*
*     3. 1,2のどちらの場合でもないが, メモリが潤沢にあり, 多少余分な領域
*        を使っても構わない場合.
*
*        IW=IM+3*(MM+1)とし, 作業領域を IW*JD*KM なる大きさにとればよい.
************************************************************************
      SUBROUTINE SNTS2G(MM,IM,ID,JM,JD,KM,
     &  S,G,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,IPOW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1)*KM)
      DIMENSION G(ID*JD*KM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION P(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION R(KM*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*),WW(*)

*/ スペクトルの詰め替え
      CALL SNCS2G(MM,KM,S,WS,IA,A,WW,IFLAG)
      
*/ ルジャンドル変換
      CALL SNLS2G(MM,JM,KM,WS,WW,Y,P,R,Q)

*/ パリティ変換
      CALL SNPS2G(MM,JM,JD,KM,WW,WS,IP,Y,IPOW)

*/ フーリエ変換
      CALL SNFS2G(MM,IM,JD,KM,WS,WW,IT,T)

*/ 添字の並べ替え
      CALL SNGS2G(IM,ID,JD,KM,WW,G)

      END
************************************************************************
      SUBROUTINE SNCS2G(MM,KM,S,WS,IA,A,WW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S((MM+1)*(MM+1),KM)
      DIMENSION WS(KM,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION WW(KM,((MM+1)/2*2+3)*(MM/2+2)*2)

      LM=(MM+1)*(MM+1)
      CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS)

*/    IFLAG=0: 微分なし, IFLAG=1: 緯度微分, IFLAG=-1: 経度微分
*/    IFLAG=2: μの演算

      IF(IFLAG.EQ.0) THEN
        DO K=1,KM
          DO L=1,LM
            WS(K,IA(L,1))=A(L,1)*S(L,K)
          END DO
        END DO
      ELSE IF(IFLAG.EQ.-1) THEN
        DO K=1,KM
          DO L=1,LM
            WS(K,IA(L,2))=A(L,2)*S(L,K)
          END DO
        END DO
      ELSE IF(IFLAG.EQ.1) THEN
        CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WW)
        DO K=1,KM
          DO L=1,LM
            WW(K,IA(L,3))=A(L,3)*S(L,K)
          END DO
        END DO
        DO K=1,KM
          DO L=1,LM
            WS(K,IA(L,4))=A(L,4)*S(L,K)
          END DO
        END DO
        CALL SNBADD(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS,WW)
      ELSE IF(IFLAG.EQ.2) THEN
        CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WW)
        DO K=1,KM
          DO L=1,LM
            WW(K,IA(L,3))=A(L,5)*S(L,K)
          END DO
        END DO
        DO K=1,KM
          DO L=1,LM
            WS(K,IA(L,4))=A(L,6)*S(L,K)
          END DO
        END DO
        CALL SNBADD(KM*((MM+1)/2*2+3)*(MM/2+2)*2,WS,WW)
      END IF

      END
************************************************************************
      SUBROUTINE SNBADD(N,A,B)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N)

      DO I=1,N
        A(I)=A(I)+B(I)
      END DO

      END
************************************************************************
      SUBROUTINE SNGS2G(IM,ID,JD,KM,WW,G)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WW(JD*KM,IM/2,2)
      DIMENSION G(ID,JD*KM)

      DO I=1,IM/2
        DO J=1,JD*KM
          G(2*I-1,J)=WW(J,I,1)
          G(2*I,J)=WW(J,I,2)
        END DO
      END DO

      DO I=IM+1,ID
        DO J=1,JD*KM
          G(I,J)=WW(J,1,1)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNFS2G(MM,IM,JD,KM,S,W,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT05=0.7071067811865475244D0)
      DIMENSION S(JD*KM,-(MM+1)/2:MM,2)
      DIMENSION W(JD*KM,0:IM/2-1,2)
      DIMENSION IT(5),T(0:IM/2-1,4)

      MMP=(MM+1)/2
      IH=IM/2
      A=SQRT05

*/ MM がもし IH-1 以上であっても, 波数 IH-1 成分までしか使わない
      
      MD=MIN(MM,IH-1)
      
*/ ベクトル化のために退避していた成分の合成

      DO M=0,MMP-1
        DO J=1,JD*KM
          S(J,M,1)=S(J,M,1)+S(J,-M-1,1)
          S(J,M,2)=S(J,M,2)+S(J,-M-1,2)
        END DO
      END DO
      
*/ 波数零成分

      DO J=1,JD*KM
        W(J,0,1)=S(J,0,1)
        W(J,0,2)=S(J,0,1)
      END DO

*/ 低波数側のみの寄与

      DO M=1,IH-MD-1
        DO J=1,JD*KM
          W(J,M,1)=A*(S(J,M,1)-T(M,3)*S(J,M,2)-T(M,4)*S(J,M,1))
          W(J,M,2)=A*(S(J,M,2)+T(M,3)*S(J,M,1)-T(M,4)*S(J,M,2))
        END DO
      END DO

*/ 両側が重なってる場合

      DO M=IH-MD,MD
        DO J=1,JD*KM
          W(J,M,1)=A*(        (S(J,IH-M,1)+S(J,M,1))
     &                -T(M,3)*(S(J,IH-M,2)+S(J,M,2))
     &                +T(M,4)*(S(J,IH-M,1)-S(J,M,1)))
          W(J,M,2)=A*(       -(S(J,IH-M,2)-S(J,M,2))
     &                -T(M,3)*(S(J,IH-M,1)-S(J,M,1))
     &                -T(M,4)*(S(J,IH-M,2)+S(J,M,2)))
        END DO
      END DO

*/ 高波数側のみの寄与

      DO M=MAX(IH-MD,MD+1),IH-1
        DO J=1,JD*KM
          W(J,M,1)=A*( S(J,IH-M,1)-T(M,3)*S(J,IH-M,2)
     &                            +T(M,4)*S(J,IH-M,1))
          W(J,M,2)=A*(-S(J,IH-M,2)-T(M,3)*S(J,IH-M,1)
     &                            -T(M,4)*S(J,IH-M,2))
        END DO
      END DO

*/ 両方から寄与が無い場合

      DO M=MD+1,IH-MD-1
        DO J=1,JD*KM
          W(J,M,1)=0
          W(J,M,2)=0
        END DO
      END DO

      CALL FTTZLM(JD*KM,IH,W,S,IT,T)

      END
************************************************************************
      SUBROUTINE SNPS2G(MM,JM,JD,KM,W,S,IP,Y,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(KM*((MM+1)/2+MM+1)*2,JM/2,2)
      DIMENSION S(JD,KM*((MM+1)/2+MM+1)*2)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION Y(JM/2,4)

      JH=JM/2
      MMP=(MM+1)/2

      DO J=1,JH
        DO K=1,KM*((MM+1)/2+MM+1)*2
          S(JH+J,K)=        (W(K,J,1)+W(K,J,2))*Y(J,4)**IPOW
          S(JH-J+1,K)=IP(K)*(W(K,J,1)-W(K,J,2))*Y(J,4)**IPOW
        END DO
      END DO

      DO J=JM+1,JD
        DO K=1,KM*((MM+1)/2+MM+1)*2
          S(J,K)=S(JM,K)
          S(J,K)=S(JM,K)
        END DO
      END DO

      END
***********************************************************************
      SUBROUTINE SNLS2G(MM,JM,KM,S,W,Y,P,R,Q)
*-----------------------------------------------------------------------
* 以下にある ifdef によるループの分解は, 今一つ賢くない SX4 のコンパイラ
* でも外側ループのアンローリングができるようにするためのもので, VPでは,
* 不要(むしろ有害)である.
*-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION W(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION Y(JM/2)
      DIMENSION P(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION R(KM*((MM+1)/2*2+3),0:MM/2)
      DIMENSION Q(KM*((MM+1)/2+MM+1),2,JM/2)

      MMP=(MM+1)/2
      MMD=MM/2
      JH=JM/2

      CALL BSCOPY(KM*((MM+1)/2+MM+1)*JM,P,Q)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W)

      L=0
      DO J=1,JH
        DO K=1,KM*(2*MMP+2)
          M=K+L*KM
          W(M,1,J,1)=W(M,1,J,1)+S(K+KM,L,1)*Q(M,1,J)
          W(M,2,J,1)=W(M,2,J,1)+S(K+KM,L,2)*Q(M,1,J)
          W(M,1,J,2)=W(M,1,J,2)+S(K,L+1,1)*Q(M,2,J)
          W(M,2,J,2)=W(M,2,J,2)+S(K,L+1,2)*Q(M,2,J)
        END DO
      END DO
      DO L=2,MMD-1,2
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            W(M,1,J,1)=W(M,1,J,1)+S(K+KM,L,1)*Q(M,1,J)
            W(M,2,J,1)=W(M,2,J,1)+S(K+KM,L,2)*Q(M,1,J)
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            W(M,1,J,2)=W(M,1,J,2)+S(K,L+1,1)*Q(M,2,J)
            W(M,2,J,2)=W(M,2,J,2)+S(K,L+1,2)*Q(M,2,J)
          END DO
        END DO
      END DO
      IF(MOD(MMD,2).EQ.0) THEN
        L=MMD
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            W(M,1,J,1)=W(M,1,J,1)+S(K+KM,L,1)*Q(M,1,J)
            W(M,2,J,1)=W(M,2,J,1)+S(K+KM,L,2)*Q(M,1,J)
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            W(M,1,J,2)=W(M,1,J,2)+S(K,L+1,1)*Q(M,2,J)
            W(M,2,J,2)=W(M,2,J,2)+S(K,L+1,2)*Q(M,2,J)
          END DO
        END DO
      ELSE
        L=MMD+1
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM-KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K,L-1)*Q(M,2,J)
            W(M,1,J,1)=W(M,1,J,1)+S(K,L,1)*Q(M,1,J)
            W(M,2,J,1)=W(M,2,J,1)+S(K,L,2)*Q(M,1,J)
          END DO
        END DO
      END IF

      END
************************************************************************
*     TRANSFORM SPECTRA TO GRID
*-----------------------------------------------------------------------
*     作業領域 WS,WW の大きさの設定については SNTS2Gを参照のこと.
************************************************************************
      SUBROUTINE SNTG2S(MM,IM,ID,JM,JD,KM,
     &  G,S,IT,T,Y,IP,P,R,IA,A,Q,WS,WW,IPOW,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(ID*JD*KM)
      DIMENSION S((MM+1)*(MM+1)*KM)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(KM*((MM+1)/2+MM+1))
      DIMENSION P(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION R(KM*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*),WW(*)

      LM=(MM+1)*(MM+1)

*/ 添字の並べ替え
      CALL SNGG2S(IM,ID,JD,KM,G,WW)

*/ フーリエ変換
      CALL SNFG2S(MM,IM,JD,KM,WW,WS,IT,T)

*/ パリティ変換
      CALL SNPG2S(MM,JM,JD,KM,WS,WW,IP,Y,IPOW)

*/ ルジャンドル変換
      CALL SNLG2S(MM,JM,KM,WW,WS,Y,P,R,Q)

*/ スペクトルの詰め替え
      CALL SNCG2S(MM,KM,WS,S,IA,A,IFLAG)

      END
************************************************************************
      SUBROUTINE SNCG2S(MM,KM,WS,S,IA,A,IFLAG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WS(KM,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION S((MM+1)*(MM+1),KM)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

      LM=(MM+1)*(MM+1)

*/    IFLAG=0: 微分なし, IFLAG=1: 緯度微分, IFLAG=-1: 経度微分
*/    IFLAG=2: μの演算

      IF(IFLAG.EQ.0) THEN
        DO K=1,KM
          DO L=1,LM
            S(L,K)=A(L,1)*WS(K,IA(L,1))
          END DO
        END DO
      ELSE IF(IFLAG.EQ.-1) THEN
        DO K=1,KM
          DO L=1,LM
            S(L,K)=-A(L,2)*WS(K,IA(L,2))
          END DO
        END DO
      ELSE IF(IFLAG.EQ.1) THEN
        DO K=1,KM
          DO L=1,LM
            S(L,K)=-A(L,3)*WS(K,IA(L,3))-A(L,4)*WS(K,IA(L,4))
          END DO
        END DO
      ELSE IF(IFLAG.EQ.2) THEN
        DO K=1,KM
          DO L=1,LM
            S(L,K)=A(L,5)*WS(K,IA(L,3))+A(L,6)*WS(K,IA(L,4))
          END DO
        END DO
      END IF

      END
************************************************************************
      SUBROUTINE SNGG2S(IM,ID,JD,KM,G,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(ID,JD*KM)
      DIMENSION WW(JD*KM,IM/2,2)

      DO I=1,IM/2
        DO J=1,JD*KM
          WW(J,I,1)=G(2*I-1,J)
          WW(J,I,2)=G(2*I,J)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNFG2S(MM,IM,JD,KM,W,S,IT,T)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT2=1.4142135623730950488D0)
      DIMENSION W(JD*KM,0:IM/2-1,2)
      DIMENSION S(JD*KM,-(MM+1)/2:MM,2)
      DIMENSION IT(5),T(0:IM/2-1,4)

      MMP=(MM+1)/2
      IH=IM/2
      
*/ MM がもし IH-1 以上である場合には, 波数 IH-1 成分までしか格納しない.
*/ それ以上の波数成分には零を格納する.      
      
      MD=MIN(MM,IH-1)

      CALL FTTZLM(JD*KM,IM/2,W,S,IT,T)

      A1=1D0/IM
*/ 以下のSQRT2は正規化のため.
      A2=A1/2*SQRT2
      
      DO J=1,JD*KM
        S(J,0,1)=A1*(W(J,0,1)+W(J,0,2))
        S(J,0,2)=0
      END DO

      DO I=1,MD
        DO J=1,JD*KM
          S(J,I,1)=A2*(       (W(J,IH-I,1)+W(J,I,1))
     &                +T(I,3)*(W(J,IH-I,2)+W(J,I,2))
     &                -T(I,4)*(W(J,IH-I,1)-W(J,I,1)))
          S(J,I,2)=A2*(       (W(J,IH-I,2)-W(J,I,2))
     &                -T(I,3)*(W(J,IH-I,1)-W(J,I,1))
     &                -T(I,4)*(W(J,IH-I,2)+W(J,I,2)))
        END DO
      END DO
      
      DO I=MD+1,MM
        DO J=1,JD*KM
          S(J,I,1)=0
          S(J,I,2)=0
        END DO
      END DO

      DO M=0,MMP-1
        DO J=1,JD*KM
          S(J,-M-1,1)=S(J,M,1)
          S(J,-M-1,2)=S(J,M,2)
        END DO
      END DO

      END
************************************************************************      
      SUBROUTINE SNPG2S(MM,JM,JD,KM,S,W,IP,Y,IPOW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(JD,KM*((MM+1)/2+MM+1)*2)
      DIMENSION W(KM*((MM+1)/2+MM+1)*2,JM/2,2)
      DIMENSION IP(KM*((MM+1)/2+MM+1)*2)
      DIMENSION Y(JM/2,4)

      JH=JM/2

      DO J=1,JH
        DO K=1,KM*((MM+1)/2+MM+1)*2
          W(K,J,1)=(S(JH+J,K)+IP(K)*S(JH-J+1,K))*(Y(J,2)*Y(J,4)**IPOW)
          W(K,J,2)=(S(JH+J,K)-IP(K)*S(JH-J+1,K))*(Y(J,2)*Y(J,4)**IPOW)
        END DO
      END DO

      END
************************************************************************
      SUBROUTINE SNLG2S(MM,JM,KM,W,S,Y,P,R,Q)
*-----------------------------------------------------------------------
* 以下にある ifdef によるループの分解は, 今一つ賢くない SX4 のコンパイラ
* でも外側ループのアンローリングができるようにするためのもので, VPでは,
* 不要(むしろ有害)である.
*-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION S(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION Y(JM/2)
      DIMENSION P(KM*((MM+1)/2+MM+1),2,JM/2)
      DIMENSION R(KM*((MM+1)/2*2+3),0:MM/2)
      DIMENSION Q(KM*((MM+1)/2+MM+1),2,JM/2)

      MMP=(MM+1)/2
      MMD=MM/2
      JH=JM/2

      CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,S)
      CALL BSCOPY(KM*((MM+1)/2+MM+1)*JM,P,Q)

      L=0
      DO J=1,JH
        DO K=1,KM*(2*MMP+2)
          M=K+L*KM
          S(K+KM,L,1)=S(K+KM,L,1)+W(M,1,J,1)*Q(M,1,J)
          S(K+KM,L,2)=S(K+KM,L,2)+W(M,2,J,1)*Q(M,1,J)
          S(K,L+1,1)=S(K,L+1,1)+W(M,1,J,2)*Q(M,2,J)
          S(K,L+1,2)=S(K,L+1,2)+W(M,2,J,2)*Q(M,2,J)
        END DO
      END DO
      DO L=2,MMD-1,2
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            S(K+KM,L,1)=S(K+KM,L,1)+W(M,1,J,1)*Q(M,1,J)
            S(K+KM,L,2)=S(K+KM,L,2)+W(M,2,J,1)*Q(M,1,J)
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            S(K,L+1,1)=S(K,L+1,1)+W(M,1,J,2)*Q(M,2,J)
            S(K,L+1,2)=S(K,L+1,2)+W(M,2,J,2)*Q(M,2,J)
          END DO
        END DO
      END DO
      IF(MOD(MMD,2).EQ.0) THEN
        L=MMD
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            S(K+KM,L,1)=S(K+KM,L,1)+W(M,1,J,1)*Q(M,1,J)
            S(K+KM,L,2)=S(K+KM,L,2)+W(M,2,J,1)*Q(M,1,J)
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            S(K,L+1,1)=S(K,L+1,1)+W(M,1,J,2)*Q(M,2,J)
            S(K,L+1,2)=S(K,L+1,2)+W(M,2,J,2)*Q(M,2,J)
          END DO
        END DO
      ELSE
        L=MMD+1
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM-KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K,L-1)*Q(M,2,J)
            S(K,L,1)=S(K,L,1)+W(M,1,J,1)*Q(M,1,J)
            S(K,L,2)=S(K,L,2)+W(M,2,J,1)*Q(M,1,J)
          END DO
        END DO
      END IF

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1999 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
*     経度微分の演算                                          1999/03/29
************************************************************************
      SUBROUTINE SPCLAM(MM,A,B,IRM)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1))
      DIMENSION B((MM+1)*(MM+1))
      DIMENSION IRM((MM+1)*(MM+1),2)

      LM=(MM+1)*(MM+1)

      DO L=1,LM
        B(IRM(L,1))=IRM(L,2)*A(L)
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1999 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
*     ラプラシアンの演算                                      1999/03/29
************************************************************************
      SUBROUTINE SPCLAP(MM,A,B,RN)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A((MM+1)*(MM+1))
      DIMENSION B((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1))

      LM=(MM+1)*(MM+1)

      DO L=1,LM
        B(L)=RN(L)*A(L)
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1999 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
*     SPPACKで使われる変数の初期化(IRM)                       1999/03/29
************************************************************************
      SUBROUTINE SPMINI(MM,IRM)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IRM((MM+1)*(MM+1),2)

      LM=(MM+1)*(MM+1)

      DO L=1,LM
        N=SQRT(1D0*(L-1))
        M=L-N*(N+1)-1
        IRM(L,1)=-M+1+N*(N+1)
        IRM(L,2)=M
      END DO

      END
*************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING          *
*   Copyright (C) 1999 Keiichi Ishioka                                  *
*                                                                       *
*   This library is free software; you can redistribute it and/or       *
*   modify it under the terms of the GNU Library General Public         *
*   License as published by the Free Software Foundation; either        *
*   version 2 of the License, or (at your option) any later version.    *
*                                                                       *
*   This library is distributed in the hope that it will be useful,     *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of      *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
*   Library General Public License for more details.                    *
*                                                                       *
*   You should have received a copy of the GNU Library General Public   *
*   License along with this library; if not, write to the Free          *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *
*************************************************************************
*     SPPACKで使われる変数の初期化(RN)                        1999/03/29
************************************************************************
      SUBROUTINE SPNINI(MM,RN)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RN((MM+1)*(MM+1),2)

      LM=(MM+1)*(MM+1)

      RN(1,1)=0
      RN(1,2)=1

      DO L=2,LM
        N=SQRT(1D0*(L-1))
        RN(L,1)=-N*(N+1)
        RN(L,2)=1D0/RN(L,1)
      END DO

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     2次元ヤコビアンの計算                                   1999/03/21
************************************************************************
      SUBROUTINE SPNJCB(MM,IM,ID,JM,JD,
     &  SA,SB,SC,IT,T,Y,IP2,P2,R2,IP3,P3,R3,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA((MM+1)*(MM+1)),SB((MM+1)*(MM+1))
      DIMENSION SC((MM+1)*(MM+1))
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP2(2*((MM+1)/2+MM+1)*2)
      DIMENSION P2(2*((MM+1)/2+MM+1)*JM)
      DIMENSION R2(2*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IP3(3*((MM+1)/2+MM+1)*2)
      DIMENSION P3(3*((MM+1)/2+MM+1)*JM)
      DIMENSION R3(3*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(3*((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(ID*JD,3)

*/ スペクトルの詰め替え
      CALL SPNS2G(MM,SA,SB,WS,IA,A,WW)
      
*/ ルジャンドル変換
      CALL SNLS2G(MM,JM,3,WS,WW,Y,P3,R3,Q)

*/ パリティ変換
      CALL SNPS2G(MM,JM,JD,3,WW,WS,IP3,Y,0)

*/ フーリエ変換
      CALL SNFS2G(MM,IM,JD,3,WS,WW,IT,T)

*/ 添字の並べ替え
      CALL SNGS2G(IM,ID,JD,3,WW,WS)

*/ 非線形項の計算

      DO IJ=1,ID*JD
        WS(IJ,1)=WS(IJ,1)*WS(IJ,3)
        WS(IJ,2)=WS(IJ,2)*WS(IJ,3)
      END DO

*/ 添字の並べ替え
      CALL SNGG2S(IM,ID,JD,2,WS,WW)

*/ フーリエ変換
      CALL SNFG2S(MM,IM,JD,2,WW,WS,IT,T)

*/ パリティ変換
      CALL SNPG2S(MM,JM,JD,2,WS,WW,IP2,Y,2)

*/ ルジャンドル変換
      CALL SNLG2S(MM,JM,2,WW,WS,Y,P2,R2,Q)

*/ スペクトルの詰め替え
      CALL SPNG2S(MM,WS,SC,IA,A)

      END
************************************************************************
      SUBROUTINE SPNS2G(MM,SA,SB,WS,IA,A,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA((MM+1)*(MM+1)),SB((MM+1)*(MM+1))
      DIMENSION WS(3,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION WW(((MM+1)/2*2+3)*(MM/2+2)*2)

      LM=(MM+1)*(MM+1)
      CALL BSSET0(3*((MM+1)/2*2+3)*(MM/2+2)*2,WS)

*/ SBの値を求めるための準備

      DO L=1,LM
        WS(3,IA(L,1))=A(L,1)*SB(L)
      END DO

*/ SAの経度微分を求めるたの準備

      DO L=1,LM
        WS(2,IA(L,2))=A(L,2)*SA(L)
      END DO

*/ SAの緯度微分(の符号を変えたもの)を求めるたの準備

      CALL BSSET0(((MM+1)/2*2+3)*(MM/2+2)*2,WW)
      DO L=1,LM
        WW(IA(L,3))=-A(L,3)*SA(L)
      END DO
      DO L=1,LM
        WS(1,IA(L,4))=-A(L,4)*SA(L)
      END DO
      DO L=1,((MM+1)/2*2+3)*(MM/2+2)*2
        WS(1,L)=WS(1,L)+WW(L)
      END DO

      END
************************************************************************
      SUBROUTINE SPNG2S(MM,WS,SC,IA,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WS(2,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION SC((MM+1)*(MM+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

      LM=(MM+1)*(MM+1)

      DO L=1,LM
        SC(L)=-A(L,2)*WS(1,IA(L,2))
     &    -A(L,3)*WS(2,IA(L,3))-A(L,4)*WS(2,IA(L,4))
      END DO

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の初期値化                                    2000/05/06
************************************************************************
      SUBROUTINE SPSWBL(MM,IM,ID,JM,JD,OMEGA,BARPHI,
     &    AVT,PHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)      
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*)      
      DIMENSION WW(*)

      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      LMD2=JD*((MM+1)/2+MM+1)*2
      MAXDIM=MAX(ID*JD,LMD,LMD2)

*/ 実際の操作 */
      CALL SPSWBB(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,
     &  AVT,PHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      END
************************************************************************
      SUBROUTINE SPSWBB(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,
     &    AVT,PHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(MAXDIM,3)

      LM=(MM+1)*(MM+1)
      LMD=((MM+1)/2*2+3)*(MM/2+2)*2

*/ ψとχの計算(一時的に, ψとしてWS(*,3)を使う)
*/ 静止系から見ることにする.
      
      CALL BSSET0(LMD,WS(1,3))
      DO L=1,LM
        WS(L,3)=RN(L,2)*AVT(L)
      END DO
      WS(3,3)=WS(3,3)+OMEGA/SQRT3
      
*/ ｕの計算(WS(*,1))

      CALL BSSET0(LMD,WS(1,2))
      CALL BSSET0(LMD,WS(1,1))
      DO L=1,LM
        WS(IA(L,3),1)=-A(L,3)*WS(L,3)
        WS(IA(L,4),2)=-A(L,4)*WS(L,3)
      END DO
      DO L=1,LMD
        WS(L,1)=WS(L,1)+WS(L,2)
      END DO

*/ ｖの計算(WS(*,2))

      CALL BSSET0(LMD,WS(1,2))
      DO L=1,LM
        WS(IA(L,2),2)=A(L,2)*WS(L,3)
      END DO

*/ ｑ(WS(*,3))の計算の準備

      CALL BSSET0(LMD,WS(1,3))
      DO L=1,LM
        WS(IA(L,1),3)=A(L,1)*AVT(L)
      END DO

*/ スペクトル→グリッド
      DO IV=1,3
*/      ルジャンドル変換
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,0)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

*/ 非線形項の計算

      DO IJ=1,ID*JD
        U=WS(IJ,1)
        V=WS(IJ,2)
        WS(IJ,1)=WS(IJ,3)*U
        WS(IJ,2)=WS(IJ,3)*V
        WS(IJ,3)=(U*U+V*V)*0.5D0
      END DO
      
      DO IV=1,3
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW)
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW,WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW,IP,Y,2)
*/      ルジャンドル変換
        CALL SNLG2S(MM,JM,1,WW,WS(1,IV),Y,P,R,Q)
      END DO
 
      DO L=1,LM
        PHI(L)=-A(L,2)*WS(IA(L,2),2)
     &    +A(L,3)*WS(IA(L,3),1)+A(L,4)*WS(IA(L,4),1)
     &    -RN(L,1)*A(L,1)*WS(IA(L,1),3)
      END DO

      DO L=2,LM
        PHI(L)=PHI(L)*RN(L,2)
      END DO
      PHI(1)=BARPHI

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の保存量のチェック                            1999/03/31
************************************************************************
      SUBROUTINE SPSWCK(MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AMOM,AENE,AENS,
     &  RN,IT,T,Y,IP4,P4,R4,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP4(4*((MM+1)/2+MM+1)*2)
      DIMENSION P4(4*((MM+1)/2+MM+1)*JM)
      DIMENSION R4(4*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(4*((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(ID,JD,4)

*/ スペクトルの詰め替え
      CALL SPSWSG(MM,OMEGA,AVT,DIV,PHI,WS,RN,IA,A,WW)

*/ ルジャンドル変換
      CALL SNLS2G(MM,JM,4,WS,WW,Y,P4,R4,Q)

*/ パリティ変換
      CALL SNPS2G(MM,JM,JD,4,WW,WS,IP4,Y,0)

*/ フーリエ変換
      CALL SNFS2G(MM,IM,JD,4,WS,WW,IT,T)

*/ 添字の並べ替え
      CALL SNGS2G(IM,ID,JD,4,WW,WS)

*/ 保存量の計算

      AMOM=0
      AENE=0
      AENS=0
      DO J=1,JM/2
        Y2=Y(J,2)
        Y3=Y(J,3)
        Y4=Y(J,4)
        J1=JM/2+J
        J2=JM/2-J+1
        DO I=1,IM
          U1=WS(I,J1,1)
          U2=WS(I,J2,1)
          V1=WS(I,J1,2)
          V2=WS(I,J2,2)
          Q1=WS(I,J1,3)
          Q2=WS(I,J2,3)
          H1=WS(I,J1,4)
          H2=WS(I,J2,4)
          AMOM=AMOM+Y2*( H1*(U1+OMEGA*Y3*Y3)
     &                  +H2*(U2+OMEGA*Y3*Y3) )
          AENE=AENE+0.5D0*Y2*( H1*((U1*U1+V1*V1)*Y4*Y4+H1)
     &                        +H2*((U2*U2+V2*V2)*Y4*Y4+H2) )
          AENS=AENS+0.5D0*Y2*(Q1*Q1/H1+Q2*Q2/H2)
        END DO
      END DO
      AMOM=AMOM/IM
      AENE=AENE/IM
      AENS=AENS/IM

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の保存量のチェック(作業領域削減版)            2000/04/03
************************************************************************
      SUBROUTINE SPSWCV(MM,IM,ID,JM,JD,OMEGA,
     &    AVT,DIV,PHI,AMOM,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)      
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*)      
      DIMENSION WW(*)

      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      LMD2=JD*((MM+1)/2+MM+1)*2
      MAXDIM=MAX(ID*JD,LMD,LMD2)

*/ 実際の操作 */
      CALL SPSWCL(MAXDIM,MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AMOM,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      END
************************************************************************
      SUBROUTINE SPSWCL(MAXDIM,MM,IM,ID,JM,JD,OMEGA,
     &    AVT,DIV,PHI,AMOM,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)            
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(MAXDIM,4)

      LM=(MM+1)*(MM+1)
      LMD=((MM+1)/2*2+3)*(MM/2+2)*2

*/ ψとχの計算(一時的に, ψとしてWW(*)を, χとしてWS(*,4)を使う)

      CALL BSSET0(LMD,WW)
      DO L=1,LM
        WW(L)=RN(L,2)*AVT(L)
      END DO
      WW(3)=WW(3)+OMEGA/SQRT3

*/ ｕの計算(WS(*,1))

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      CALL BSSET0(LMD,WS(1,1))
      DO L=1,LM
        WS(IA(L,2),1)=A(L,2)*RN(L,2)*DIV(L)
        WS(IA(L,3),3)=-A(L,3)*WW(L)
        WS(IA(L,4),4)=-A(L,4)*WW(L)
      END DO
      DO L=1,LMD
        WS(L,1)=WS(L,1)+WS(L,3)+WS(L,4)
      END DO

*/ ｖの計算(WS(*,2))

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      CALL BSSET0(LMD,WS(1,2))
      DO L=1,LM
        WS(IA(L,2),2)=A(L,2)*WW(L)
        WS(IA(L,3),3)=A(L,3)*RN(L,2)*DIV(L)
        WS(IA(L,4),4)=A(L,4)*RN(L,2)*DIV(L)
      END DO
      DO L=1,LMD
        WS(L,2)=WS(L,2)+WS(L,3)+WS(L,4)
      END DO

*/ ｑ(WS(*,3))とΦ(WS(*,4))の計算の準備

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      DO L=1,LM
        WS(IA(L,1),3)=A(L,1)*AVT(L)
        WS(IA(L,1),4)=A(L,1)*PHI(L)
      END DO

*/ スペクトル→グリッド
      DO IV=1,4
*/      ルジャンドル変換
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,0)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

*/ 保存量の計算

      AMOM=0
      AENE=0
      AENS=0
      DO J=1,JM/2
        Y2=Y(J,2)
        Y3=Y(J,3)
        Y4=Y(J,4)
        J1=JM/2+J
        J2=JM/2-J+1
        DO I=1,IM
          IJ1=ID*(J1-1)+I
          IJ2=ID*(J2-1)+I
          U1=WS(IJ1,1)
          U2=WS(IJ2,1)
          V1=WS(IJ1,2)
          V2=WS(IJ2,2)
          Q1=WS(IJ1,3)
          Q2=WS(IJ2,3)
          H1=WS(IJ1,4)
          H2=WS(IJ2,4)
          AMOM=AMOM+Y2*( H1*(U1+OMEGA*Y3*Y3)
     &                  +H2*(U2+OMEGA*Y3*Y3) )
          AENE=AENE+0.5D0*Y2*( H1*((U1*U1+V1*V1)*Y4*Y4+H1)
     &                        +H2*((U2*U2+V2*V2)*Y4*Y4+H2) )
          AENS=AENS+0.5D0*Y2*(Q1*Q1/H1+Q2*Q2/H2)
          IF(H1.LT.0) THEN
            PRINT *,'*** PHI IS NEGATIVE!! I,J,PHI=',I,J1,H1
          END IF
          IF(H2.LT.0) THEN
            PRINT *,'*** PHI IS NEGATIVE!! I,J,PHI=',I,J2,H2
          END IF
        END DO
      END DO
      AMOM=AMOM/IM
      AENE=AENE/IM
      AENS=AENS/IM

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の保存量のチェック(作業領域削減版)            2000/09/07
*     (角運動量を3成分計算する. ただし回転系では地軸まわり以外の
*      角運動量は保存量にならないことに注意)
************************************************************************
      SUBROUTINE SPSWCX(MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AM1,AM2,AM3,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)      
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*)      
      DIMENSION WW(*)

      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      LMD2=JD*((MM+1)/2+MM+1)*2
      MAXDIM=MAX(ID*JD,LMD,LMD2)

*/ 実際の操作 */
      CALL SPSWCT(MAXDIM,MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AM1,AM2,AM3,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      END
************************************************************************
      SUBROUTINE SPSWCT(MAXDIM,MM,IM,ID,JM,JD,OMEGA,
     &  AVT,DIV,PHI,AM1,AM2,AM3,AENE,AENS,RN,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      PARAMETER(PI=3.1415926535897932385D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)            
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM/2,4)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(MAXDIM,4)

      LM=(MM+1)*(MM+1)
      LMD=((MM+1)/2*2+3)*(MM/2+2)*2

*/ ψとχの計算(一時的に, ψとしてWW(*)を, χとしてWS(*,4)を使う)

      CALL BSSET0(LMD,WW)
      DO L=1,LM
        WW(L)=RN(L,2)*AVT(L)
      END DO
      WW(3)=WW(3)+OMEGA/SQRT3

*/ ｕの計算(WS(*,1))

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      CALL BSSET0(LMD,WS(1,1))
      DO L=1,LM
        WS(IA(L,2),1)=A(L,2)*RN(L,2)*DIV(L)
        WS(IA(L,3),3)=-A(L,3)*WW(L)
        WS(IA(L,4),4)=-A(L,4)*WW(L)
      END DO
      DO L=1,LMD
        WS(L,1)=WS(L,1)+WS(L,3)+WS(L,4)
      END DO

*/ ｖの計算(WS(*,2))

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      CALL BSSET0(LMD,WS(1,2))
      DO L=1,LM
        WS(IA(L,2),2)=A(L,2)*WW(L)
        WS(IA(L,3),3)=A(L,3)*RN(L,2)*DIV(L)
        WS(IA(L,4),4)=A(L,4)*RN(L,2)*DIV(L)
      END DO
      DO L=1,LMD
        WS(L,2)=WS(L,2)+WS(L,3)+WS(L,4)
      END DO

*/ ｑ(WS(*,3))とΦ(WS(*,4))の計算の準備

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      DO L=1,LM
        WS(IA(L,1),3)=A(L,1)*AVT(L)
        WS(IA(L,1),4)=A(L,1)*PHI(L)
      END DO

*/ スペクトル→グリッド
      DO IV=1,4
*/      ルジャンドル変換
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,0)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

*/ 保存量の計算

      AM1=0
      AM2=0
      AM3=0            
      AENE=0
      AENS=0
      DO I=1,IM
        C=COS(2*PI*(I-1)/IM)
        S=SIN(2*PI*(I-1)/IM)
        DO J=1,JM/2
          Y1=Y(J,1)
          Y2=Y(J,2)
          Y3=Y(J,3)
          Y4=Y(J,4)
          J1=JM/2+J
          J2=JM/2-J+1
          IJ1=ID*(J1-1)+I
          IJ2=ID*(J2-1)+I
          U1=WS(IJ1,1)*Y4
          U2=WS(IJ2,1)*Y4
          UA1=U1+OMEGA*Y3
          UA2=U2+OMEGA*Y3
          V1=WS(IJ1,2)*Y4
          V2=WS(IJ2,2)*Y4
          Q1=WS(IJ1,3)
          Q2=WS(IJ2,3)
          H1=WS(IJ1,4)
          H2=WS(IJ2,4)
          AM1=AM1+Y2*(H1*UA1+H2*UA2)*Y3
          AM2=AM2+Y2*(H1*(-UA1*C*Y1+V1*S)+H2*(UA2*C*Y1+V2*S))
          AM3=AM3+Y2*(H1*(-UA1*S*Y1-V1*C)+H2*(UA2*S*Y1-V2*C))
          AENE=AENE+Y2/2*(H1*((U1*U1+V1*V1)+H1)+H2*((U2*U2+V2*V2)+H2))
          AENS=AENS+0.5D0*Y2*(Q1*Q1/H1+Q2*Q2/H2)
          IF(H1.LT.0) THEN
            PRINT *,'*** PHI IS NEGATIVE!! I,J,PHI=',I,J1,H1
          END IF
          IF(H2.LT.0) THEN
            PRINT *,'*** PHI IS NEGATIVE!! I,J,PHI=',I,J2,H2
          END IF
        END DO
      END DO
      AM1=AM1/IM
      AM2=AM2/IM
      AM3=AM3/IM            
      AENE=AENE/IM
      AENS=AENS/IM

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の線形項による発展のための行列の初期化    
*                        (角運動量保存の高階粘性項含む)       2000/08/16
************************************************************************
      SUBROUTINE SPSWHI(MM,BARPHI,DNU,ALPHA,LEV,DT,CL)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION CL((MM+1)*(MM+1),5)

      CL(1,1)=1
      CL(1,2)=1
      CL(1,3)=0
      CL(1,4)=1
      CL(1,5)=0
      DO L=2,(MM+1)*(MM+1)
        N=SQRT(1D0*(L-1))
        DNUD=DNU*(N*(N+1)-2D0)**LEV
        C=-DNUD*((2-ALPHA)*(-N*(N+1))+2)/2
        S=-BARPHI*(-N*(N+1))
        FREQ2=S-C*C
        ECD=EXP(-C*DT)
        CL(L,1)=EXP(DNUD*DT*(-N*(N+1)+2))
        IF(FREQ2.GT.0) THEN
          FREQ=SQRT(FREQ2)
          CFD=COS(FREQ*DT)
          SFDF=SIN(FREQ*DT)/FREQ
        ELSE IF(FREQ2.LT.0) THEN
          FREQ=SQRT(-FREQ2)
          CFD=COSH(FREQ*DT)
          SFDF=SINH(FREQ*DT)/FREQ
        ELSE
          CFD=1
          SFDF=DT
        END IF
        CL(L,2)=ECD*(CFD-C*SFDF)
        CL(L,3)=ECD*N*(N+1)*SFDF
        CL(L,4)=ECD*(CFD+C*SFDF)
        CL(L,5)=ECD*(-BARPHI)*SFDF
      ENDDO

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の非線形項の計算
*     (角運動量を保存する高階粘性項も含む)                    2000/08/16
************************************************************************
      SUBROUTINE SPSWHV(MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     & LEV,AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)      
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*)      
      DIMENSION WW(*)

      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      LMD2=JD*((MM+1)/2+MM+1)*2
      MAXDIM=MAX(ID*JD,LMD,LMD2)

*/ 実際の操作 */
      CALL SPSWHL(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     & LEV,AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      END
************************************************************************
      SUBROUTINE SPSWHL(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     & LEV,AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(MAXDIM,11)

      LM=(MM+1)*(MM+1)
      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      DNUD=DNU/BARPHI

*/ ψとχの計算(一時的に, ψとしてWS(*,3)を, χとしてWS(*,4)を使う)
*/ 静止系から見ることにする.
      
      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))      
      DO L=1,LM
        WS(L,3)=RN(L,2)*AVT(L)
        WS(L,4)=RN(L,2)*DIV(L)
      END DO

*/ ｕの計算(WS(*,1))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,1))
      DO L=1,LM
        WS(IA(L,2),1)=A(L,2)*WS(L,4)
        WS(IA(L,3),7)=-A(L,3)*WS(L,3)
        WS(IA(L,4),8)=-A(L,4)*WS(L,3)
      END DO
      DO L=1,LMD
        WS(L,1)=WS(L,1)+WS(L,7)+WS(L,8)
      END DO

*/ ｖの計算(WS(*,2))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,2))
      DO L=1,LM
        WS(IA(L,2),2)=A(L,2)*WS(L,3)
        WS(IA(L,3),7)=A(L,3)*WS(L,4)
        WS(IA(L,4),8)=A(L,4)*WS(L,4)
      END DO
      DO L=1,LMD
        WS(L,2)=WS(L,2)+WS(L,7)+WS(L,8)
      END DO

*/ u^, v^ の計算の準備

      DO I=1,LEV
        DO L=1,LM
          WS(L,3)=-(RN(L,1)+2)*WS(L,3)
          WS(L,4)=-(RN(L,1)+2)*WS(L,4)
        END DO
      END DO

*/ u^の計算(WS(*,5))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,5))
      DO L=1,LM
        WS(IA(L,2),5)=A(L,2)*WS(L,4)
        WS(IA(L,3),7)=-A(L,3)*WS(L,3)
        WS(IA(L,4),8)=-A(L,4)*WS(L,3)
      END DO
      DO L=1,LMD
        WS(L,5)=WS(L,5)+WS(L,7)+WS(L,8)
      END DO

*/ v^の計算(WS(*,6))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,6))
      DO L=1,LM
        WS(IA(L,2),6)=A(L,2)*WS(L,3)
        WS(IA(L,3),7)=A(L,3)*WS(L,4)
        WS(IA(L,4),8)=A(L,4)*WS(L,4)
      END DO
      DO L=1,LMD
        WS(L,6)=WS(L,6)+WS(L,7)+WS(L,8)
      END DO

*/ ∂Φ/∂λ(WS(*,3)), cosφ∂Φ/∂φ(WS(*,4))の計算

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      DO L=1,LM
        WS(IA(L,3),3)=A(L,3)*PHI(L)
        WS(IA(L,4),4)=A(L,4)*PHI(L)
      END DO
      DO L=1,LMD
        WS(L,4)=WS(L,4)+WS(L,3)
      END DO

      CALL BSSET0(LMD,WS(1,3))
      DO L=1,LM
        WS(IA(L,2),3)=A(L,2)*PHI(L)
      END DO

*/ ｑ(WS(*,7))とΦ(WS(*,8))と△Φ(WS(*,9))とq^(WS(*,10))とD^(WS(*,11))
*/          の計算の準備

      DO L=1,LM
        WS(L,7)=AVT(L)
        WS(L,8)=DIV(L)
      END DO
      
      DO I=1,LEV
        DO L=1,LM
          WS(L,7)=-(RN(L,1)+2)*WS(L,7)
          WS(L,8)=-(RN(L,1)+2)*WS(L,8)
        END DO
      END DO

      CALL BSSET0(LMD,WS(1,10))
      CALL BSSET0(LMD,WS(1,11))
      DO L=1,LM
        WS(IA(L,1),10)=A(L,1)*WS(L,7)
        WS(IA(L,1),11)=A(L,1)*WS(L,8)
      END DO
      
      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,9))      
      DO L=1,LM
        WS(IA(L,1),7)=A(L,1)*AVT(L)
        WS(IA(L,1),8)=A(L,1)*PHI(L)
        WS(IA(L,1),9)=A(L,1)*RN(L,1)*PHI(L)
      END DO

*/ スペクトル→グリッド
      DO IV=1,6
*/      ルジャンドル変換
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,1)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

      DO IV=7,11
*/      ルジャンドル変換
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,0)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

*/ 非線形項の計算

      DO IJ=1,ID*JD
        U=WS(IJ,1)
        V=WS(IJ,2)
        PHID=WS(IJ,8)-BARPHI
        WS(IJ,1)=WS(IJ,7)*U
     &    +DNUD*( 2*WS(IJ,9)*WS(IJ,6)+WS(IJ,10)*WS(IJ,3)
     &    +ALPHA*WS(IJ,11)*WS(IJ,4))
        WS(IJ,2)=WS(IJ,7)*V
     &    +DNUD*(-2*WS(IJ,9)*WS(IJ,5)+WS(IJ,10)*WS(IJ,4)
     &    -ALPHA*WS(IJ,11)*WS(IJ,3))
        WS(IJ,3)=PHID*U
        WS(IJ,4)=PHID*V
        WS(IJ,5)=PHID*WS(IJ,5)
        WS(IJ,6)=PHID*WS(IJ,6)
        WS(IJ,7)=PHID*WS(IJ,10)        
        WS(IJ,8)=(U*U+V*V)*0.5D0+DNUD*ALPHA*PHID*WS(IJ,11)
      END DO

*/ グリッド→スペクトル(IV=5,6の成分だけは SNPG2Sの変換が異なるので注意)
      
      DO IV=1,6
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW)
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW,WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW,IP,Y,1)
*/      ルジャンドル変換
        CALL SNLG2S(MM,JM,1,WW,WS(1,IV),Y,P,R,Q)
      END DO
 
      DO IV=7,8
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW)
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW,WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW,IP,Y,0)
*/      ルジャンドル変換
        CALL SNLG2S(MM,JM,1,WW,WS(1,IV),Y,P,R,Q)
      END DO
 
      DO L=1,LM
        DAVT(L)=A(L,2)*WS(IA(L,2),1)
     &    +A(L,3)*WS(IA(L,3),2)+A(L,4)*WS(IA(L,4),2)
     &    -DNUD*RN(L,1)*A(L,1)*WS(IA(L,1),7)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),6)
     &    +A(L,3)*WS(IA(L,3),5)+A(L,4)*WS(IA(L,4),5))
        DDIV(L)=-A(L,2)*WS(IA(L,2),2)
     &    +A(L,3)*WS(IA(L,3),1)+A(L,4)*WS(IA(L,4),1)
     &    -RN(L,1)*A(L,1)*WS(IA(L,1),8)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),5)
     &    -A(L,3)*WS(IA(L,3),6)-A(L,4)*WS(IA(L,4),6))
        DPHI(L)=A(L,2)*WS(IA(L,2),3)
     &    +A(L,3)*WS(IA(L,3),4)+A(L,4)*WS(IA(L,4),4)
      END DO

*/ 回転系に戻るために以下の変換が必要
      
      DO L=1,LM
        DAVT(L)=DAVT(L)-OMEGA*IRM(L,2)*AVT(IRM(L,1))
        DDIV(L)=DDIV(L)-OMEGA*IRM(L,2)*DIV(IRM(L,1))
        DPHI(L)=DPHI(L)-OMEGA*IRM(L,2)*PHI(IRM(L,1))
      END DO
      DDIV(7)=DDIV(7)-OMEGA*OMEGA*2/SQRT(5D0)

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の線形項による発展のための行列の初期化    
*                              (角運動量保存の散逸含む)       2000/04/08
************************************************************************
      SUBROUTINE SPSWLI(MM,BARPHI,DNU,ALPHA,DT,CL)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION CL((MM+1)*(MM+1),5)

      CL(1,1)=1
      CL(1,2)=1
      CL(1,3)=0
      CL(1,4)=1
      CL(1,5)=0
      DO L=2,(MM+1)*(MM+1)
        N=SQRT(1D0*(L-1))
        C=-DNU*((2-ALPHA)*(-N*(N+1))+2)/2
        S=-BARPHI*(-N*(N+1))
        FREQ2=S-C*C
        ECD=EXP(-C*DT)
        CL(L,1)=EXP(DNU*DT*(-N*(N+1)+2))
        IF(FREQ2.GT.0) THEN
          FREQ=SQRT(FREQ2)
          CFD=COS(FREQ*DT)
          SFDF=SIN(FREQ*DT)/FREQ
        ELSE IF(FREQ2.LT.0) THEN
          FREQ=SQRT(-FREQ2)
          CFD=COSH(FREQ*DT)
          SFDF=SINH(FREQ*DT)/FREQ
        ELSE
          CFD=1
          SFDF=DT
        END IF
        CL(L,2)=ECD*(CFD-C*SFDF)
        CL(L,3)=ECD*N*(N+1)*SFDF
        CL(L,4)=ECD*(CFD+C*SFDF)
        CL(L,5)=ECD*(-BARPHI)*SFDF
      ENDDO

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の線形項による発展(角運動量保存の散逸含む)    2000/04/08
************************************************************************
      SUBROUTINE SPSWLV(MM,AVT,DIV,PHI,CL)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION CL((MM+1)*(MM+1),5)

      DO L=1,(MM+1)*(MM+1)
        AVT(L)=CL(L,1)*AVT(L)
        TMPDIV=DIV(L)
        DIV(L)=CL(L,2)*DIV(L)+CL(L,3)*PHI(L)
        PHI(L)=CL(L,4)*PHI(L)+CL(L,5)*TMPDIV
      END DO

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の非線形項の計算                              1999/03/29
************************************************************************
      SUBROUTINE SPSWNL(MM,IM,ID,JM,JD,OMEGA,
     &    AVT,DIV,PHI,DAVT,DDIV,DPHI,
     &    RN,IT,T,Y,IP4,P4,R4,IP5,P5,R5,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP4(4*((MM+1)/2+MM+1)*2)
      DIMENSION P4(4*((MM+1)/2+MM+1)*JM)
      DIMENSION R4(4*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IP5(5*((MM+1)/2+MM+1)*2)
      DIMENSION P5(5*((MM+1)/2+MM+1)*JM)
      DIMENSION R5(5*((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(5*((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(ID*JD,5)

*/ スペクトルの詰め替え
      CALL SPSWSG(MM,OMEGA,AVT,DIV,PHI,WS,RN,IA,A,WW)

*/ ルジャンドル変換
      CALL SNLS2G(MM,JM,4,WS,WW,Y,P4,R4,Q)

*/ パリティ変換
      CALL SNPS2G(MM,JM,JD,4,WW,WS,IP4,Y,0)

*/ フーリエ変換
      CALL SNFS2G(MM,IM,JD,4,WS,WW,IT,T)

*/ 添字の並べ替え
      CALL SNGS2G(IM,ID,JD,4,WW,WS)

*/ 非線形項の計算

      DO IJ=1,ID*JD
        U=WS(IJ,1)
        V=WS(IJ,2)
        WS(IJ,1)=U*WS(IJ,3)
        WS(IJ,2)=U*WS(IJ,4)
        WS(IJ,3)=V*WS(IJ,3)
        WS(IJ,4)=V*WS(IJ,4)
        WS(IJ,5)=(U*U+V*V)*0.5D0
      END DO

*/ 添字の並べ替え
      CALL SNGG2S(IM,ID,JD,5,WS,WW)

*/ フーリエ変換
      CALL SNFG2S(MM,IM,JD,5,WW,WS,IT,T)

*/ パリティ変換
      CALL SNPG2S(MM,JM,JD,5,WS,WW,IP5,Y,2)

*/ ルジャンドル変換
      CALL SNLG2S(MM,JM,5,WW,WS,Y,P5,R5,Q)

*/ スペクトルの詰め替え
      CALL SPSWGS(MM,PHI,WS,DAVT,DDIV,DPHI,RN,IA,A)

      END
************************************************************************
      SUBROUTINE SPSWSG(MM,OMEGA,AVT,DIV,PHI,WS,RN,IA,A,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION WS(4,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION WW(((MM+1)/2*2+3)*(MM/2+2)*2,4)

      LM=(MM+1)*(MM+1)
      CALL BSSET0(4*((MM+1)/2*2+3)*(MM/2+2)*2,WS)

*/ PSIとCHIの計算(一時的に, PSIとしてW(L,1)を, CHIとしてW(L,2)を使う)

      DO L=1,LM
        WW(L,1)=RN(L,2)*AVT(L)
        WW(L,2)=RN(L,2)*DIV(L)
      END DO
      WW(3,1)=WW(3,1)+OMEGA/SQRT3

*/ ｑとΦの計算の準備

      DO L=1,LM
        WS(3,IA(L,1))=A(L,1)*AVT(L)
        WS(4,IA(L,1))=A(L,1)*PHI(L)
      END DO

*/ ｕの計算

      CALL BSSET0(2*((MM+1)/2*2+3)*(MM/2+2)*2,WW(1,3))
      DO L=1,LM
        WS(1,IA(L,2))=A(L,2)*WW(L,2)
        WW(IA(L,3),3)=-A(L,3)*WW(L,1)
        WW(IA(L,4),4)=-A(L,4)*WW(L,1)
      END DO
      DO L=1,((MM+1)/2*2+3)*(MM/2+2)*2
        WS(1,L)=WS(1,L)+WW(L,3)+WW(L,4)
      END DO

*/ ｖの計算

      CALL BSSET0(2*((MM+1)/2*2+3)*(MM/2+2)*2,WW(1,3))
      DO L=1,LM
        WS(2,IA(L,2))=A(L,2)*WW(L,1)
        WW(IA(L,3),3)=A(L,3)*WW(L,2)
        WW(IA(L,4),4)=A(L,4)*WW(L,2)
      END DO
      DO L=1,((MM+1)/2*2+3)*(MM/2+2)*2
        WS(2,L)=WS(2,L)+WW(L,3)+WW(L,4)
      END DO

      END
************************************************************************
      SUBROUTINE SPSWGS(MM,PHI,WS,DAVT,DDIV,DPHI,RN,IA,A)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION WS(5,((MM+1)/2*2+3)*(MM/2+2)*2)
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)

      LM=(MM+1)*(MM+1)

      DO L=1,LM
        DAVT(L)=A(L,2)*WS(1,IA(L,2))
     &    +A(L,3)*WS(3,IA(L,3))+A(L,4)*WS(3,IA(L,4))
        DDIV(L)=-A(L,2)*WS(3,IA(L,2))
     &    +A(L,3)*WS(1,IA(L,3))+A(L,4)*WS(1,IA(L,4))
     &    -RN(L,1)*(A(L,1)*WS(5,IA(L,1))+PHI(L))
        DPHI(L)=A(L,2)*WS(2,IA(L,2))
     &    +A(L,3)*WS(4,IA(L,3))+A(L,4)*WS(4,IA(L,4))
      END DO

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の非線形項の計算
*     (角運動量を保存する散逸も含む)                          2000/04/07
************************************************************************
      SUBROUTINE SPSWNV(MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     &    AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)      
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*)      
      DIMENSION WW(*)

      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      LMD2=JD*((MM+1)/2+MM+1)*2
      MAXDIM=MAX(ID*JD,LMD,LMD2)

*/ 実際の操作 */
      CALL SPSWSV(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     &  AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      END
************************************************************************
      SUBROUTINE SPSWSV(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     &    AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WW(*)
      DIMENSION WS(MAXDIM,8)

      LM=(MM+1)*(MM+1)
      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      DNUD=DNU/BARPHI

*/ ψとχの計算(一時的に, ψとしてWS(*,3)を, χとしてWS(*,4)を使う)
*/ 静止系から見ることにする.
      
      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))      
      DO L=1,LM
        WS(L,3)=RN(L,2)*AVT(L)
        WS(L,4)=RN(L,2)*DIV(L)
      END DO

*/ ｕの計算(WS(*,1))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,1))
      DO L=1,LM
        WS(IA(L,2),1)=A(L,2)*WS(L,4)
        WS(IA(L,3),7)=-A(L,3)*WS(L,3)
        WS(IA(L,4),8)=-A(L,4)*WS(L,3)
      END DO
      DO L=1,LMD
        WS(L,1)=WS(L,1)+WS(L,7)+WS(L,8)
      END DO

*/ ｖの計算(WS(*,2))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,2))
      DO L=1,LM
        WS(IA(L,2),2)=A(L,2)*WS(L,3)
        WS(IA(L,3),7)=A(L,3)*WS(L,4)
        WS(IA(L,4),8)=A(L,4)*WS(L,4)
      END DO
      DO L=1,LMD
        WS(L,2)=WS(L,2)+WS(L,7)+WS(L,8)
      END DO

*/ ∂Φ/∂λ(WS(*,3)), cosφ∂Φ/∂φ(WS(*,4))の計算

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      DO L=1,LM
        WS(IA(L,3),3)=A(L,3)*PHI(L)
        WS(IA(L,4),4)=A(L,4)*PHI(L)
      END DO
      DO L=1,LMD
        WS(L,4)=WS(L,4)+WS(L,3)
      END DO

      CALL BSSET0(LMD,WS(1,3))
      DO L=1,LM
        WS(IA(L,2),3)=A(L,2)*PHI(L)
      END DO

*/ ｑ(WS(*,5))とＤ(WS(*,6))とΦ(WS(*,7))と△Φ(WS(*,8))の計算の準備

      CALL BSSET0(LMD,WS(1,5))
      CALL BSSET0(LMD,WS(1,6))
      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))      
      DO L=1,LM
        WS(IA(L,1),5)=A(L,1)*AVT(L)
        WS(IA(L,1),6)=A(L,1)*DIV(L)        
        WS(IA(L,1),7)=A(L,1)*PHI(L)
        WS(IA(L,1),8)=A(L,1)*RN(L,1)*PHI(L)
      END DO

*/ スペクトル→グリッド
      DO IV=1,4
*/      ルジャンドル変換
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,1)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

      DO IV=5,8
*/      ルジャンドル変換
        CALL SNLS2G(MM,JM,1,WS(1,IV),WW,Y,P,R,Q)
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW,WS(1,IV),IP,Y,0)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW,IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW,WS(1,IV))
      END DO

*/ 非線形項の計算

      DO IJ=1,ID*JD
        U=WS(IJ,1)
        V=WS(IJ,2)
        PHID=WS(IJ,7)-BARPHI
        WS(IJ,1)=WS(IJ,5)*U
     &   +DNUD*( 2*WS(IJ,8)*V+WS(IJ,5)*WS(IJ,3)+ALPHA*WS(IJ,6)*WS(IJ,4))
        WS(IJ,2)=WS(IJ,5)*V
     &   +DNUD*(-2*WS(IJ,8)*U+WS(IJ,5)*WS(IJ,4)-ALPHA*WS(IJ,6)*WS(IJ,3))
        WS(IJ,3)=PHID*U
        WS(IJ,4)=PHID*V
        WS(IJ,5)=PHID*WS(IJ,5)        
        WS(IJ,6)=(U*U+V*V)*0.5D0+DNUD*ALPHA*PHID*WS(IJ,6)
      END DO
      
*/ グリッド→スペクトル(IV=5,6の成分だけは SNPG2Sの変換が異なるので注意)
      
      DO IV=1,4
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW)
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW,WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW,IP,Y,1)
*/      ルジャンドル変換
        CALL SNLG2S(MM,JM,1,WW,WS(1,IV),Y,P,R,Q)
      END DO
 
      DO IV=5,6
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW)
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW,WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW,IP,Y,0)
*/      ルジャンドル変換
        CALL SNLG2S(MM,JM,1,WW,WS(1,IV),Y,P,R,Q)
      END DO

      DO L=1,LM
        DAVT(L)=A(L,2)*WS(IA(L,2),1)
     &    +A(L,3)*WS(IA(L,3),2)+A(L,4)*WS(IA(L,4),2)
     &    -DNUD*RN(L,1)*A(L,1)*WS(IA(L,1),5)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),4)
     &    +A(L,3)*WS(IA(L,3),3)+A(L,4)*WS(IA(L,4),3))
        DDIV(L)=-A(L,2)*WS(IA(L,2),2)
     &    +A(L,3)*WS(IA(L,3),1)+A(L,4)*WS(IA(L,4),1)
     &    -RN(L,1)*A(L,1)*WS(IA(L,1),6)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),3)
     &    -A(L,3)*WS(IA(L,3),4)-A(L,4)*WS(IA(L,4),4))
        DPHI(L)=A(L,2)*WS(IA(L,2),3)
     &    +A(L,3)*WS(IA(L,3),4)+A(L,4)*WS(IA(L,4),4)
      END DO

*/ 回転系に戻るために以下の変換が必要
      
      DO L=1,LM
        DAVT(L)=DAVT(L)-OMEGA*IRM(L,2)*AVT(IRM(L,1))
        DDIV(L)=DDIV(L)-OMEGA*IRM(L,2)*DIV(IRM(L,1))
        DPHI(L)=DPHI(L)-OMEGA*IRM(L,2)*PHI(IRM(L,1))
      END DO
      DDIV(7)=DDIV(7)-OMEGA*OMEGA*2/SQRT(5D0)

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の非線形項の計算(角運動量を保存する散逸も含む)
*     (若干高速化を図った版)                                 2000/04/08
************************************************************************
      SUBROUTINE SPSWNW(MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     &    AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)      
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*)      
      DIMENSION WW(*)

      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      LMD2=JD*((MM+1)/2+MM+1)*2
      MAXDIM=MAX(ID*JD,LMD,LMD2)

*/ 実際の操作 */
      CALL SPSWSW(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     &  AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      END
************************************************************************
      SUBROUTINE SPSWSW(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,ALPHA,
     &    AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(MAXDIM,8)      
      DIMENSION WW(MAXDIM,8)

      LM=(MM+1)*(MM+1)
      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      DNUD=DNU/BARPHI

*/ ψとχの計算(一時的に, ψとしてWS(*,3)を, χとしてWS(*,4)を使う)
*/ 静止系から見ることにする.
      
      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))      
      DO L=1,LM
        WS(L,3)=RN(L,2)*AVT(L)
        WS(L,4)=RN(L,2)*DIV(L)
      END DO

*/ ｕの計算(WS(*,1))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,1))
      DO L=1,LM
        WS(IA(L,2),1)=A(L,2)*WS(L,4)
        WS(IA(L,3),7)=-A(L,3)*WS(L,3)
        WS(IA(L,4),8)=-A(L,4)*WS(L,3)
      END DO
      DO L=1,LMD
        WS(L,1)=WS(L,1)+WS(L,7)+WS(L,8)
      END DO

*/ ｖの計算(WS(*,2))

      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))
      CALL BSSET0(LMD,WS(1,2))
      DO L=1,LM
        WS(IA(L,2),2)=A(L,2)*WS(L,3)
        WS(IA(L,3),7)=A(L,3)*WS(L,4)
        WS(IA(L,4),8)=A(L,4)*WS(L,4)
      END DO
      DO L=1,LMD
        WS(L,2)=WS(L,2)+WS(L,7)+WS(L,8)
      END DO

*/ ∂Φ/∂λ(WS(*,3)), cosφ∂Φ/∂φ(WS(*,4))の計算

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      DO L=1,LM
        WS(IA(L,3),3)=A(L,3)*PHI(L)
        WS(IA(L,4),4)=A(L,4)*PHI(L)
      END DO
      DO L=1,LMD
        WS(L,4)=WS(L,4)+WS(L,3)
      END DO

      CALL BSSET0(LMD,WS(1,3))
      DO L=1,LM
        WS(IA(L,2),3)=A(L,2)*PHI(L)
      END DO

*/ ｑ(WS(*,5))とＤ(WS(*,6))とΦ(WS(*,7))と△Φ(WS(*,8))の計算の準備

      CALL BSSET0(LMD,WS(1,5))
      CALL BSSET0(LMD,WS(1,6))
      CALL BSSET0(LMD,WS(1,7))
      CALL BSSET0(LMD,WS(1,8))      
      DO L=1,LM
        WS(IA(L,1),5)=A(L,1)*AVT(L)
        WS(IA(L,1),6)=A(L,1)*DIV(L)        
        WS(IA(L,1),7)=A(L,1)*PHI(L)
        WS(IA(L,1),8)=A(L,1)*RN(L,1)*PHI(L)
      END DO

*/      ルジャンドル変換
      
      CALL SPLSG8(MM,JM,1,
     &  WS(1,1),WS(1,2),WS(1,3),WS(1,4),WS(1,5),WS(1,6),WS(1,7),WS(1,8),
     &  WW(1,1),WW(1,2),WW(1,3),WW(1,4),WW(1,5),WW(1,6),WW(1,7),WW(1,8),
     &  Y,P,R,Q)

*/ スペクトル→グリッド
      DO IV=1,4
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW(1,IV),WS(1,IV),IP,Y,1)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW(1,IV),IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW(1,IV),WS(1,IV))
      END DO

      DO IV=5,8
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW(1,IV),WS(1,IV),IP,Y,0)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW(1,IV),IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW(1,IV),WS(1,IV))
      END DO

*/ 非線形項の計算

      DO IJ=1,ID*JD
        U=WS(IJ,1)
        V=WS(IJ,2)
        PHID=WS(IJ,7)-BARPHI
        WS(IJ,1)=WS(IJ,5)*U
     &   +DNUD*( 2*WS(IJ,8)*V+WS(IJ,5)*WS(IJ,3)+ALPHA*WS(IJ,6)*WS(IJ,4))
        WS(IJ,2)=WS(IJ,5)*V
     &   +DNUD*(-2*WS(IJ,8)*U+WS(IJ,5)*WS(IJ,4)-ALPHA*WS(IJ,6)*WS(IJ,3))
        WS(IJ,3)=PHID*U
        WS(IJ,4)=PHID*V
        WS(IJ,5)=PHID*WS(IJ,5)        
        WS(IJ,6)=(U*U+V*V)*0.5D0+DNUD*ALPHA*PHID*WS(IJ,6)
      END DO
      
*/ グリッド→スペクトル(IV=5,6の成分だけは SNPG2Sの変換が異なるので注意)
      
      DO IV=1,4
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW(1,IV))
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW(1,IV),WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW(1,IV),IP,Y,1)
      END DO
 
      DO IV=5,6
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW(1,IV))
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW(1,IV),WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW(1,IV),IP,Y,0)
      END DO

*/      ルジャンドル変換      

      CALL SPLGS6(MM,JM,1,
     &  WW(1,1),WW(1,2),WW(1,3),WW(1,4),WW(1,5),WW(1,6),
     &  WS(1,1),WS(1,2),WS(1,3),WS(1,4),WS(1,5),WS(1,6),
     &  Y,P,R,Q)

      DO L=1,LM
        DAVT(L)=A(L,2)*WS(IA(L,2),1)
     &    +A(L,3)*WS(IA(L,3),2)+A(L,4)*WS(IA(L,4),2)
     &    -DNUD*RN(L,1)*A(L,1)*WS(IA(L,1),5)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),4)
     &    +A(L,3)*WS(IA(L,3),3)+A(L,4)*WS(IA(L,4),3))
        DDIV(L)=-A(L,2)*WS(IA(L,2),2)
     &    +A(L,3)*WS(IA(L,3),1)+A(L,4)*WS(IA(L,4),1)
     &    -RN(L,1)*A(L,1)*WS(IA(L,1),6)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),3)
     &    -A(L,3)*WS(IA(L,3),4)-A(L,4)*WS(IA(L,4),4))
        DPHI(L)=A(L,2)*WS(IA(L,2),3)
     &    +A(L,3)*WS(IA(L,3),4)+A(L,4)*WS(IA(L,4),4)
      END DO

*/ 回転系に戻るために以下の変換が必要
      
      DO L=1,LM
        DAVT(L)=DAVT(L)-OMEGA*IRM(L,2)*AVT(IRM(L,1))
        DDIV(L)=DDIV(L)-OMEGA*IRM(L,2)*DIV(IRM(L,1))
        DPHI(L)=DPHI(L)-OMEGA*IRM(L,2)*PHI(IRM(L,1))
      END DO
      DDIV(7)=DDIV(7)-OMEGA*OMEGA*2/SQRT(5D0)

      END
***********************************************************************
      SUBROUTINE SPLSG8(MM,JM,KM,
     &  S1,S2,S3,S4,S5,S6,S7,S8,W1,W2,W3,W4,W5,W6,W7,W8,Y,P,R,Q)
*-----------------------------------------------------------------------
* 以下にある ifdef によるループの分解は, 今一つ賢くない SX4 のコンパイラ
* でも外側ループのアンローリングができるようにするためのもので, VPでは,
* 不要(むしろ有害)である.
*-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S1(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S2(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S3(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S4(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S5(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S6(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S7(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S8(KM*((MM+1)/2*2+3),0:MM/2+1,2)      
      DIMENSION W1(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W2(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W3(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W4(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W5(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W6(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W7(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W8(KM*((MM+1)/2+MM+1),2,JM/2,2)                  
      DIMENSION Y(JM/2)
      DIMENSION P(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION R(KM*((MM+1)/2*2+3),0:MM/2)
      DIMENSION Q(KM*((MM+1)/2+MM+1),2,JM/2)

      MMP=(MM+1)/2
      MMD=MM/2
      JH=JM/2

      CALL BSCOPY(KM*((MM+1)/2+MM+1)*JM,P,Q)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W1)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W2)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W3)      
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W4)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W5)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W6)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W7)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W8)
      
      L=0
      DO J=1,JH
        DO K=1,KM*(2*MMP+2)
          M=K+L*KM
          W1(M,1,J,1)=W1(M,1,J,1)+S1(K+KM,L,1)*Q(M,1,J)
          W1(M,2,J,1)=W1(M,2,J,1)+S1(K+KM,L,2)*Q(M,1,J)
          W2(M,1,J,1)=W2(M,1,J,1)+S2(K+KM,L,1)*Q(M,1,J)
          W2(M,2,J,1)=W2(M,2,J,1)+S2(K+KM,L,2)*Q(M,1,J)
          W3(M,1,J,1)=W3(M,1,J,1)+S3(K+KM,L,1)*Q(M,1,J)
          W3(M,2,J,1)=W3(M,2,J,1)+S3(K+KM,L,2)*Q(M,1,J)
          W4(M,1,J,1)=W4(M,1,J,1)+S4(K+KM,L,1)*Q(M,1,J)
          W4(M,2,J,1)=W4(M,2,J,1)+S4(K+KM,L,2)*Q(M,1,J)
          W5(M,1,J,1)=W5(M,1,J,1)+S5(K+KM,L,1)*Q(M,1,J)
          W5(M,2,J,1)=W5(M,2,J,1)+S5(K+KM,L,2)*Q(M,1,J)
          W6(M,1,J,1)=W6(M,1,J,1)+S6(K+KM,L,1)*Q(M,1,J)
          W6(M,2,J,1)=W6(M,2,J,1)+S6(K+KM,L,2)*Q(M,1,J)
          W7(M,1,J,1)=W7(M,1,J,1)+S7(K+KM,L,1)*Q(M,1,J)
          W7(M,2,J,1)=W7(M,2,J,1)+S7(K+KM,L,2)*Q(M,1,J)
          W8(M,1,J,1)=W8(M,1,J,1)+S8(K+KM,L,1)*Q(M,1,J)
          W8(M,2,J,1)=W8(M,2,J,1)+S8(K+KM,L,2)*Q(M,1,J)
          W1(M,1,J,2)=W1(M,1,J,2)+S1(K,L+1,1)*Q(M,2,J)
          W1(M,2,J,2)=W1(M,2,J,2)+S1(K,L+1,2)*Q(M,2,J)
          W2(M,1,J,2)=W2(M,1,J,2)+S2(K,L+1,1)*Q(M,2,J)
          W2(M,2,J,2)=W2(M,2,J,2)+S2(K,L+1,2)*Q(M,2,J)
          W3(M,1,J,2)=W3(M,1,J,2)+S3(K,L+1,1)*Q(M,2,J)
          W3(M,2,J,2)=W3(M,2,J,2)+S3(K,L+1,2)*Q(M,2,J)
          W4(M,1,J,2)=W4(M,1,J,2)+S4(K,L+1,1)*Q(M,2,J)
          W4(M,2,J,2)=W4(M,2,J,2)+S4(K,L+1,2)*Q(M,2,J)
          W5(M,1,J,2)=W5(M,1,J,2)+S5(K,L+1,1)*Q(M,2,J)
          W5(M,2,J,2)=W5(M,2,J,2)+S5(K,L+1,2)*Q(M,2,J)
          W6(M,1,J,2)=W6(M,1,J,2)+S6(K,L+1,1)*Q(M,2,J)
          W6(M,2,J,2)=W6(M,2,J,2)+S6(K,L+1,2)*Q(M,2,J)
          W7(M,1,J,2)=W7(M,1,J,2)+S7(K,L+1,1)*Q(M,2,J)
          W7(M,2,J,2)=W7(M,2,J,2)+S7(K,L+1,2)*Q(M,2,J)
          W8(M,1,J,2)=W8(M,1,J,2)+S8(K,L+1,1)*Q(M,2,J)
          W8(M,2,J,2)=W8(M,2,J,2)+S8(K,L+1,2)*Q(M,2,J)
        END DO
      END DO
      DO L=2,MMD-1,2
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            W1(M,1,J,1)=W1(M,1,J,1)+S1(K+KM,L,1)*Q(M,1,J)
            W1(M,2,J,1)=W1(M,2,J,1)+S1(K+KM,L,2)*Q(M,1,J)
            W2(M,1,J,1)=W2(M,1,J,1)+S2(K+KM,L,1)*Q(M,1,J)
            W2(M,2,J,1)=W2(M,2,J,1)+S2(K+KM,L,2)*Q(M,1,J)
            W3(M,1,J,1)=W3(M,1,J,1)+S3(K+KM,L,1)*Q(M,1,J)
            W3(M,2,J,1)=W3(M,2,J,1)+S3(K+KM,L,2)*Q(M,1,J)
            W4(M,1,J,1)=W4(M,1,J,1)+S4(K+KM,L,1)*Q(M,1,J)
            W4(M,2,J,1)=W4(M,2,J,1)+S4(K+KM,L,2)*Q(M,1,J)
            W5(M,1,J,1)=W5(M,1,J,1)+S5(K+KM,L,1)*Q(M,1,J)
            W5(M,2,J,1)=W5(M,2,J,1)+S5(K+KM,L,2)*Q(M,1,J)
            W6(M,1,J,1)=W6(M,1,J,1)+S6(K+KM,L,1)*Q(M,1,J)
            W6(M,2,J,1)=W6(M,2,J,1)+S6(K+KM,L,2)*Q(M,1,J)
            W7(M,1,J,1)=W7(M,1,J,1)+S7(K+KM,L,1)*Q(M,1,J)
            W7(M,2,J,1)=W7(M,2,J,1)+S7(K+KM,L,2)*Q(M,1,J)
            W8(M,1,J,1)=W8(M,1,J,1)+S8(K+KM,L,1)*Q(M,1,J)
            W8(M,2,J,1)=W8(M,2,J,1)+S8(K+KM,L,2)*Q(M,1,J)
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            W1(M,1,J,2)=W1(M,1,J,2)+S1(K,L+1,1)*Q(M,2,J)
            W1(M,2,J,2)=W1(M,2,J,2)+S1(K,L+1,2)*Q(M,2,J)
            W2(M,1,J,2)=W2(M,1,J,2)+S2(K,L+1,1)*Q(M,2,J)
            W2(M,2,J,2)=W2(M,2,J,2)+S2(K,L+1,2)*Q(M,2,J)
            W3(M,1,J,2)=W3(M,1,J,2)+S3(K,L+1,1)*Q(M,2,J)
            W3(M,2,J,2)=W3(M,2,J,2)+S3(K,L+1,2)*Q(M,2,J)
            W4(M,1,J,2)=W4(M,1,J,2)+S4(K,L+1,1)*Q(M,2,J)
            W4(M,2,J,2)=W4(M,2,J,2)+S4(K,L+1,2)*Q(M,2,J)
            W5(M,1,J,2)=W5(M,1,J,2)+S5(K,L+1,1)*Q(M,2,J)
            W5(M,2,J,2)=W5(M,2,J,2)+S5(K,L+1,2)*Q(M,2,J)
            W6(M,1,J,2)=W6(M,1,J,2)+S6(K,L+1,1)*Q(M,2,J)
            W6(M,2,J,2)=W6(M,2,J,2)+S6(K,L+1,2)*Q(M,2,J)
            W7(M,1,J,2)=W7(M,1,J,2)+S7(K,L+1,1)*Q(M,2,J)
            W7(M,2,J,2)=W7(M,2,J,2)+S7(K,L+1,2)*Q(M,2,J)
            W8(M,1,J,2)=W8(M,1,J,2)+S8(K,L+1,1)*Q(M,2,J)
            W8(M,2,J,2)=W8(M,2,J,2)+S8(K,L+1,2)*Q(M,2,J)
          END DO
        END DO
      END DO
      IF(MOD(MMD,2).EQ.0) THEN
        L=MMD
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            W1(M,1,J,1)=W1(M,1,J,1)+S1(K+KM,L,1)*Q(M,1,J)
            W1(M,2,J,1)=W1(M,2,J,1)+S1(K+KM,L,2)*Q(M,1,J)
            W2(M,1,J,1)=W2(M,1,J,1)+S2(K+KM,L,1)*Q(M,1,J)
            W2(M,2,J,1)=W2(M,2,J,1)+S2(K+KM,L,2)*Q(M,1,J)
            W3(M,1,J,1)=W3(M,1,J,1)+S3(K+KM,L,1)*Q(M,1,J)
            W3(M,2,J,1)=W3(M,2,J,1)+S3(K+KM,L,2)*Q(M,1,J)
            W4(M,1,J,1)=W4(M,1,J,1)+S4(K+KM,L,1)*Q(M,1,J)
            W4(M,2,J,1)=W4(M,2,J,1)+S4(K+KM,L,2)*Q(M,1,J)
            W5(M,1,J,1)=W5(M,1,J,1)+S5(K+KM,L,1)*Q(M,1,J)
            W5(M,2,J,1)=W5(M,2,J,1)+S5(K+KM,L,2)*Q(M,1,J)
            W6(M,1,J,1)=W6(M,1,J,1)+S6(K+KM,L,1)*Q(M,1,J)
            W6(M,2,J,1)=W6(M,2,J,1)+S6(K+KM,L,2)*Q(M,1,J)
            W7(M,1,J,1)=W7(M,1,J,1)+S7(K+KM,L,1)*Q(M,1,J)
            W7(M,2,J,1)=W7(M,2,J,1)+S7(K+KM,L,2)*Q(M,1,J)
            W8(M,1,J,1)=W8(M,1,J,1)+S8(K+KM,L,1)*Q(M,1,J)
            W8(M,2,J,1)=W8(M,2,J,1)+S8(K+KM,L,2)*Q(M,1,J)
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            W1(M,1,J,2)=W1(M,1,J,2)+S1(K,L+1,1)*Q(M,2,J)
            W1(M,2,J,2)=W1(M,2,J,2)+S1(K,L+1,2)*Q(M,2,J)
            W2(M,1,J,2)=W2(M,1,J,2)+S2(K,L+1,1)*Q(M,2,J)
            W2(M,2,J,2)=W2(M,2,J,2)+S2(K,L+1,2)*Q(M,2,J)
            W3(M,1,J,2)=W3(M,1,J,2)+S3(K,L+1,1)*Q(M,2,J)
            W3(M,2,J,2)=W3(M,2,J,2)+S3(K,L+1,2)*Q(M,2,J)
            W4(M,1,J,2)=W4(M,1,J,2)+S4(K,L+1,1)*Q(M,2,J)
            W4(M,2,J,2)=W4(M,2,J,2)+S4(K,L+1,2)*Q(M,2,J)
            W5(M,1,J,2)=W5(M,1,J,2)+S5(K,L+1,1)*Q(M,2,J)
            W5(M,2,J,2)=W5(M,2,J,2)+S5(K,L+1,2)*Q(M,2,J)
            W6(M,1,J,2)=W6(M,1,J,2)+S6(K,L+1,1)*Q(M,2,J)
            W6(M,2,J,2)=W6(M,2,J,2)+S6(K,L+1,2)*Q(M,2,J)
            W7(M,1,J,2)=W7(M,1,J,2)+S7(K,L+1,1)*Q(M,2,J)
            W7(M,2,J,2)=W7(M,2,J,2)+S7(K,L+1,2)*Q(M,2,J)
            W8(M,1,J,2)=W8(M,1,J,2)+S8(K,L+1,1)*Q(M,2,J)
            W8(M,2,J,2)=W8(M,2,J,2)+S8(K,L+1,2)*Q(M,2,J)
          END DO
        END DO
      ELSE
        L=MMD+1
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM-KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K,L-1)*Q(M,2,J)
            W1(M,1,J,1)=W1(M,1,J,1)+S1(K,L,1)*Q(M,1,J)
            W1(M,2,J,1)=W1(M,2,J,1)+S1(K,L,2)*Q(M,1,J)
            W2(M,1,J,1)=W2(M,1,J,1)+S2(K,L,1)*Q(M,1,J)
            W2(M,2,J,1)=W2(M,2,J,1)+S2(K,L,2)*Q(M,1,J)
            W3(M,1,J,1)=W3(M,1,J,1)+S3(K,L,1)*Q(M,1,J)
            W3(M,2,J,1)=W3(M,2,J,1)+S3(K,L,2)*Q(M,1,J)
            W4(M,1,J,1)=W4(M,1,J,1)+S4(K,L,1)*Q(M,1,J)
            W4(M,2,J,1)=W4(M,2,J,1)+S4(K,L,2)*Q(M,1,J)
            W5(M,1,J,1)=W5(M,1,J,1)+S5(K,L,1)*Q(M,1,J)
            W5(M,2,J,1)=W5(M,2,J,1)+S5(K,L,2)*Q(M,1,J)
            W6(M,1,J,1)=W6(M,1,J,1)+S6(K,L,1)*Q(M,1,J)
            W6(M,2,J,1)=W6(M,2,J,1)+S6(K,L,2)*Q(M,1,J)
            W7(M,1,J,1)=W7(M,1,J,1)+S7(K,L,1)*Q(M,1,J)
            W7(M,2,J,1)=W7(M,2,J,1)+S7(K,L,2)*Q(M,1,J)
            W8(M,1,J,1)=W8(M,1,J,1)+S8(K,L,1)*Q(M,1,J)
            W8(M,2,J,1)=W8(M,2,J,1)+S8(K,L,2)*Q(M,1,J)
          END DO
        END DO
      END IF

      END
************************************************************************
      SUBROUTINE SPLGS6(MM,JM,KM,
     & W1,W2,W3,W4,W5,W6,S1,S2,S3,S4,S5,S6,Y,P,R,Q)
*-----------------------------------------------------------------------
* 以下にある ifdef によるループの分解は, 今一つ賢くない SX4 のコンパイラ
* でも外側ループのアンローリングができるようにするためのもので, VPでは,
* 不要(むしろ有害)である.
*-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION W1(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W2(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W3(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W4(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W5(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W6(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION S1(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S2(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S3(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S4(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S5(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S6(KM*((MM+1)/2*2+3),0:MM/2+1,2)      
      DIMENSION Y(JM/2)
      DIMENSION P(KM*((MM+1)/2+MM+1),2,JM/2)
      DIMENSION R(KM*((MM+1)/2*2+3),0:MM/2)
      DIMENSION Q(KM*((MM+1)/2+MM+1),2,JM/2)

      MMP=(MM+1)/2
      MMD=MM/2
      JH=JM/2

      CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,S1)
      CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,S2)
      CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,S3)
      CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,S4)
      CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,S5)
      CALL BSSET0(KM*((MM+1)/2*2+3)*(MM/2+2)*2,S6)
      CALL BSCOPY(KM*((MM+1)/2+MM+1)*JM,P,Q)

      L=0
      DO J=1,JH
        DO K=1,KM*(2*MMP+2)
          M=K+L*KM
          S1(K+KM,L,1)=S1(K+KM,L,1)+W1(M,1,J,1)*Q(M,1,J)
          S1(K+KM,L,2)=S1(K+KM,L,2)+W1(M,2,J,1)*Q(M,1,J)
          S2(K+KM,L,1)=S2(K+KM,L,1)+W2(M,1,J,1)*Q(M,1,J)
          S2(K+KM,L,2)=S2(K+KM,L,2)+W2(M,2,J,1)*Q(M,1,J)
          S3(K+KM,L,1)=S3(K+KM,L,1)+W3(M,1,J,1)*Q(M,1,J)
          S3(K+KM,L,2)=S3(K+KM,L,2)+W3(M,2,J,1)*Q(M,1,J)
          S4(K+KM,L,1)=S4(K+KM,L,1)+W4(M,1,J,1)*Q(M,1,J)
          S4(K+KM,L,2)=S4(K+KM,L,2)+W4(M,2,J,1)*Q(M,1,J)
          S5(K+KM,L,1)=S5(K+KM,L,1)+W5(M,1,J,1)*Q(M,1,J)
          S5(K+KM,L,2)=S5(K+KM,L,2)+W5(M,2,J,1)*Q(M,1,J)
          S6(K+KM,L,1)=S6(K+KM,L,1)+W6(M,1,J,1)*Q(M,1,J)
          S6(K+KM,L,2)=S6(K+KM,L,2)+W6(M,2,J,1)*Q(M,1,J)
          S1(K,L+1,1)=S1(K,L+1,1)+W1(M,1,J,2)*Q(M,2,J)
          S1(K,L+1,2)=S1(K,L+1,2)+W1(M,2,J,2)*Q(M,2,J)
          S2(K,L+1,1)=S2(K,L+1,1)+W2(M,1,J,2)*Q(M,2,J)
          S2(K,L+1,2)=S2(K,L+1,2)+W2(M,2,J,2)*Q(M,2,J)
          S3(K,L+1,1)=S3(K,L+1,1)+W3(M,1,J,2)*Q(M,2,J)
          S3(K,L+1,2)=S3(K,L+1,2)+W3(M,2,J,2)*Q(M,2,J)
          S4(K,L+1,1)=S4(K,L+1,1)+W4(M,1,J,2)*Q(M,2,J)
          S4(K,L+1,2)=S4(K,L+1,2)+W4(M,2,J,2)*Q(M,2,J)
          S5(K,L+1,1)=S5(K,L+1,1)+W5(M,1,J,2)*Q(M,2,J)
          S5(K,L+1,2)=S5(K,L+1,2)+W5(M,2,J,2)*Q(M,2,J)
          S6(K,L+1,1)=S6(K,L+1,1)+W6(M,1,J,2)*Q(M,2,J)
          S6(K,L+1,2)=S6(K,L+1,2)+W6(M,2,J,2)*Q(M,2,J)
        END DO
      END DO
      DO L=2,MMD-1,2
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            S1(K+KM,L,1)=S1(K+KM,L,1)+W1(M,1,J,1)*Q(M,1,J)
            S1(K+KM,L,2)=S1(K+KM,L,2)+W1(M,2,J,1)*Q(M,1,J)
            S2(K+KM,L,1)=S2(K+KM,L,1)+W2(M,1,J,1)*Q(M,1,J)
            S2(K+KM,L,2)=S2(K+KM,L,2)+W2(M,2,J,1)*Q(M,1,J)
            S3(K+KM,L,1)=S3(K+KM,L,1)+W3(M,1,J,1)*Q(M,1,J)
            S3(K+KM,L,2)=S3(K+KM,L,2)+W3(M,2,J,1)*Q(M,1,J)
            S4(K+KM,L,1)=S4(K+KM,L,1)+W4(M,1,J,1)*Q(M,1,J)
            S4(K+KM,L,2)=S4(K+KM,L,2)+W4(M,2,J,1)*Q(M,1,J)
            S5(K+KM,L,1)=S5(K+KM,L,1)+W5(M,1,J,1)*Q(M,1,J)
            S5(K+KM,L,2)=S5(K+KM,L,2)+W5(M,2,J,1)*Q(M,1,J)
            S6(K+KM,L,1)=S6(K+KM,L,1)+W6(M,1,J,1)*Q(M,1,J)
            S6(K+KM,L,2)=S6(K+KM,L,2)+W6(M,2,J,1)*Q(M,1,J)
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            S1(K,L+1,1)=S1(K,L+1,1)+W1(M,1,J,2)*Q(M,2,J)
            S1(K,L+1,2)=S1(K,L+1,2)+W1(M,2,J,2)*Q(M,2,J)
            S2(K,L+1,1)=S2(K,L+1,1)+W2(M,1,J,2)*Q(M,2,J)
            S2(K,L+1,2)=S2(K,L+1,2)+W2(M,2,J,2)*Q(M,2,J)
            S3(K,L+1,1)=S3(K,L+1,1)+W3(M,1,J,2)*Q(M,2,J)
            S3(K,L+1,2)=S3(K,L+1,2)+W3(M,2,J,2)*Q(M,2,J)
            S4(K,L+1,1)=S4(K,L+1,1)+W4(M,1,J,2)*Q(M,2,J)
            S4(K,L+1,2)=S4(K,L+1,2)+W4(M,2,J,2)*Q(M,2,J)
            S5(K,L+1,1)=S5(K,L+1,1)+W5(M,1,J,2)*Q(M,2,J)
            S5(K,L+1,2)=S5(K,L+1,2)+W5(M,2,J,2)*Q(M,2,J)
            S6(K,L+1,1)=S6(K,L+1,1)+W6(M,1,J,2)*Q(M,2,J)
            S6(K,L+1,2)=S6(K,L+1,2)+W6(M,2,J,2)*Q(M,2,J)
          END DO
        END DO
      END DO
      IF(MOD(MMD,2).EQ.0) THEN
        L=MMD
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            S1(K+KM,L,1)=S1(K+KM,L,1)+W1(M,1,J,1)*Q(M,1,J)
            S1(K+KM,L,2)=S1(K+KM,L,2)+W1(M,2,J,1)*Q(M,1,J)
            S2(K+KM,L,1)=S2(K+KM,L,1)+W2(M,1,J,1)*Q(M,1,J)
            S2(K+KM,L,2)=S2(K+KM,L,2)+W2(M,2,J,1)*Q(M,1,J)
            S3(K+KM,L,1)=S3(K+KM,L,1)+W3(M,1,J,1)*Q(M,1,J)
            S3(K+KM,L,2)=S3(K+KM,L,2)+W3(M,2,J,1)*Q(M,1,J)
            S4(K+KM,L,1)=S4(K+KM,L,1)+W4(M,1,J,1)*Q(M,1,J)
            S4(K+KM,L,2)=S4(K+KM,L,2)+W4(M,2,J,1)*Q(M,1,J)
            S5(K+KM,L,1)=S5(K+KM,L,1)+W5(M,1,J,1)*Q(M,1,J)
            S5(K+KM,L,2)=S5(K+KM,L,2)+W5(M,2,J,1)*Q(M,1,J)
            S6(K+KM,L,1)=S6(K+KM,L,1)+W6(M,1,J,1)*Q(M,1,J)
            S6(K+KM,L,2)=S6(K+KM,L,2)+W6(M,2,J,1)*Q(M,1,J)
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            S1(K,L+1,1)=S1(K,L+1,1)+W1(M,1,J,2)*Q(M,2,J)
            S1(K,L+1,2)=S1(K,L+1,2)+W1(M,2,J,2)*Q(M,2,J)
            S2(K,L+1,1)=S2(K,L+1,1)+W2(M,1,J,2)*Q(M,2,J)
            S2(K,L+1,2)=S2(K,L+1,2)+W2(M,2,J,2)*Q(M,2,J)
            S3(K,L+1,1)=S3(K,L+1,1)+W3(M,1,J,2)*Q(M,2,J)
            S3(K,L+1,2)=S3(K,L+1,2)+W3(M,2,J,2)*Q(M,2,J)
            S4(K,L+1,1)=S4(K,L+1,1)+W4(M,1,J,2)*Q(M,2,J)
            S4(K,L+1,2)=S4(K,L+1,2)+W4(M,2,J,2)*Q(M,2,J)
            S5(K,L+1,1)=S5(K,L+1,1)+W5(M,1,J,2)*Q(M,2,J)
            S5(K,L+1,2)=S5(K,L+1,2)+W5(M,2,J,2)*Q(M,2,J)
            S6(K,L+1,1)=S6(K,L+1,1)+W6(M,1,J,2)*Q(M,2,J)
            S6(K,L+1,2)=S6(K,L+1,2)+W6(M,2,J,2)*Q(M,2,J)
          END DO
        END DO
      ELSE
        L=MMD+1
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM-KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K,L-1)*Q(M,2,J)
            S1(K,L,1)=S1(K,L,1)+W1(M,1,J,1)*Q(M,1,J)
            S1(K,L,2)=S1(K,L,2)+W1(M,2,J,1)*Q(M,1,J)
            S2(K,L,1)=S2(K,L,1)+W2(M,1,J,1)*Q(M,1,J)
            S2(K,L,2)=S2(K,L,2)+W2(M,2,J,1)*Q(M,1,J)
            S3(K,L,1)=S3(K,L,1)+W3(M,1,J,1)*Q(M,1,J)
            S3(K,L,2)=S3(K,L,2)+W3(M,2,J,1)*Q(M,1,J)
            S4(K,L,1)=S4(K,L,1)+W4(M,1,J,1)*Q(M,1,J)
            S4(K,L,2)=S4(K,L,2)+W4(M,2,J,1)*Q(M,1,J)
            S5(K,L,1)=S5(K,L,1)+W5(M,1,J,1)*Q(M,1,J)
            S5(K,L,2)=S5(K,L,2)+W5(M,2,J,1)*Q(M,1,J)
            S6(K,L,1)=S6(K,L,1)+W6(M,1,J,1)*Q(M,1,J)
            S6(K,L,2)=S6(K,L,2)+W6(M,2,J,1)*Q(M,1,J)
          END DO
        END DO
      END IF

      END
************************************************************************
*   ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING         *
*   Copyright (C) 1999 Keiichi Ishioka                                 *
*                                                                      *
*   This library is free software; you can redistribute it and/or      *
*   modify it under the terms of the GNU Library General Public        *
*   License as published by the Free Software Foundation; either       *
*   version 2 of the License, or (at your option) any later version.   *
*                                                                      *
*   This library is distributed in the hope that it will be useful,    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
*   Library General Public License for more details.                   *
*                                                                      *
*   You should have received a copy of the GNU Library General Public  *
*   License along with this library; if not, write to the Free         *
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. *
************************************************************************
************************************************************************
*     浅水方程式の非線形項の計算(角運動量を保存する散逸も含む)
*     (若干高速化を図り, さらに ALPHA=0の場合に限定した版)    2000/04/08
************************************************************************
      SUBROUTINE SPSWNX(MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,
     &    AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)      
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1)*4)
      DIMENSION A((MM+1)*(MM+1)*6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(*)      
      DIMENSION WW(*)

      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      LMD2=JD*((MM+1)/2+MM+1)*2
      MAXDIM=MAX(ID*JD,LMD,LMD2)

*/ 実際の操作 */
      CALL SPSWSX(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,
     &  AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      END
************************************************************************
      SUBROUTINE SPSWSX(MAXDIM,MM,IM,ID,JM,JD,OMEGA,BARPHI,DNU,
     &    AVT,DIV,PHI,DAVT,DDIV,DPHI,RN,IRM,IT,T,Y,IP,P,R,IA,A,Q,WS,WW)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(SQRT3=1.7320508075688772935D0)
      DIMENSION AVT((MM+1)*(MM+1))
      DIMENSION DIV((MM+1)*(MM+1))
      DIMENSION PHI((MM+1)*(MM+1))
      DIMENSION DAVT((MM+1)*(MM+1))
      DIMENSION DDIV((MM+1)*(MM+1))
      DIMENSION DPHI((MM+1)*(MM+1))
      DIMENSION RN((MM+1)*(MM+1),2)
      DIMENSION IRM((MM+1)*(MM+1),2)
      DIMENSION IT(5),T(IM*2)
      DIMENSION Y(JM*2)
      DIMENSION IP(((MM+1)/2+MM+1)*2)
      DIMENSION P(((MM+1)/2+MM+1)*JM)
      DIMENSION R(((MM+1)/2*2+3)*(MM/2+1))
      DIMENSION IA((MM+1)*(MM+1),4)
      DIMENSION A((MM+1)*(MM+1),6)
      DIMENSION Q(((MM+1)/2+MM+1)*JM)
      DIMENSION WS(MAXDIM,7)
      DIMENSION WW(MAXDIM,7)

      LM=(MM+1)*(MM+1)
      LMD=((MM+1)/2*2+3)*(MM/2+2)*2
      DNUD=DNU/BARPHI

*/ ψとχの計算(一時的に, ψとしてWS(*,3)を, χとしてWS(*,4)を使う)
*/ 静止系から見ることにする.
      
      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))      
      DO L=1,LM
        WS(L,3)=RN(L,2)*AVT(L)
        WS(L,4)=RN(L,2)*DIV(L)
      END DO

*/ ｕの計算(WS(*,1))

      CALL BSSET0(LMD,WS(1,5))
      CALL BSSET0(LMD,WS(1,6))
      CALL BSSET0(LMD,WS(1,1))
      DO L=1,LM
        WS(IA(L,2),1)=A(L,2)*WS(L,4)
        WS(IA(L,3),5)=-A(L,3)*WS(L,3)
        WS(IA(L,4),6)=-A(L,4)*WS(L,3)
      END DO
      DO L=1,LMD
        WS(L,1)=WS(L,1)+WS(L,5)+WS(L,6)
      END DO

*/ ｖの計算(WS(*,2))

      CALL BSSET0(LMD,WS(1,5))
      CALL BSSET0(LMD,WS(1,6))
      CALL BSSET0(LMD,WS(1,2))
      DO L=1,LM
        WS(IA(L,2),2)=A(L,2)*WS(L,3)
        WS(IA(L,3),5)=A(L,3)*WS(L,4)
        WS(IA(L,4),6)=A(L,4)*WS(L,4)
      END DO
      DO L=1,LMD
        WS(L,2)=WS(L,2)+WS(L,5)+WS(L,6)
      END DO

*/ ∂Φ/∂λ(WS(*,3)), cosφ∂Φ/∂φ(WS(*,4))の計算

      CALL BSSET0(LMD,WS(1,3))
      CALL BSSET0(LMD,WS(1,4))
      DO L=1,LM
        WS(IA(L,3),3)=A(L,3)*PHI(L)
        WS(IA(L,4),4)=A(L,4)*PHI(L)
      END DO
      DO L=1,LMD
        WS(L,4)=WS(L,4)+WS(L,3)
      END DO

      CALL BSSET0(LMD,WS(1,3))
      DO L=1,LM
        WS(IA(L,2),3)=A(L,2)*PHI(L)
      END DO

*/ ｑ(WS(*,5))とΦ(WS(*,6))と△Φ(WS(*,7))の計算の準備

      CALL BSSET0(LMD,WS(1,5))
      CALL BSSET0(LMD,WS(1,6))
      CALL BSSET0(LMD,WS(1,7))
      DO L=1,LM
        WS(IA(L,1),5)=A(L,1)*AVT(L)
        WS(IA(L,1),6)=A(L,1)*PHI(L)
        WS(IA(L,1),7)=A(L,1)*RN(L,1)*PHI(L)
      END DO

*/      ルジャンドル変換
      
      CALL SPLSG7(MM,JM,1,
     &  WS(1,1),WS(1,2),WS(1,3),WS(1,4),WS(1,5),WS(1,6),WS(1,7),
     &  WW(1,1),WW(1,2),WW(1,3),WW(1,4),WW(1,5),WW(1,6),WW(1,7),
     &  Y,P,R,Q)

*/ スペクトル→グリッド
      DO IV=1,4
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW(1,IV),WS(1,IV),IP,Y,1)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW(1,IV),IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW(1,IV),WS(1,IV))
      END DO

      DO IV=5,7
*/      パリティ変換
        CALL SNPS2G(MM,JM,JD,1,WW(1,IV),WS(1,IV),IP,Y,0)
*/      フーリエ変換
        CALL SNFS2G(MM,IM,JD,1,WS(1,IV),WW(1,IV),IT,T)
*/      添字の並べ替え
        CALL SNGS2G(IM,ID,JD,1,WW(1,IV),WS(1,IV))
      END DO

*/ 非線形項の計算

      DO IJ=1,ID*JD
        U=WS(IJ,1)
        V=WS(IJ,2)
        PHID=WS(IJ,6)-BARPHI
        WS(IJ,1)=WS(IJ,5)*U
     &   +DNUD*( 2*WS(IJ,7)*V+WS(IJ,5)*WS(IJ,3))
        WS(IJ,2)=WS(IJ,5)*V
     &   +DNUD*(-2*WS(IJ,7)*U+WS(IJ,5)*WS(IJ,4))
        WS(IJ,3)=PHID*U
        WS(IJ,4)=PHID*V
        WS(IJ,5)=PHID*WS(IJ,5)        
        WS(IJ,6)=(U*U+V*V)*0.5D0
      END DO
      
*/ グリッド→スペクトル(IV=5,6の成分だけは SNPG2Sの変換が異なるので注意)
      
      DO IV=1,4
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW(1,IV))
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW(1,IV),WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW(1,IV),IP,Y,1)
      END DO
 
      DO IV=5,6
*/      添字の並べ替え
        CALL SNGG2S(IM,ID,JD,1,WS(1,IV),WW(1,IV))
*/      フーリエ変換
        CALL SNFG2S(MM,IM,JD,1,WW(1,IV),WS(1,IV),IT,T)
*/      パリティ変換
        CALL SNPG2S(MM,JM,JD,1,WS(1,IV),WW(1,IV),IP,Y,0)
      END DO

*/      ルジャンドル変換      

      CALL SPLGS6(MM,JM,1,
     &  WW(1,1),WW(1,2),WW(1,3),WW(1,4),WW(1,5),WW(1,6),
     &  WS(1,1),WS(1,2),WS(1,3),WS(1,4),WS(1,5),WS(1,6),
     &  Y,P,R,Q)

      DO L=1,LM
        DAVT(L)=A(L,2)*WS(IA(L,2),1)
     &    +A(L,3)*WS(IA(L,3),2)+A(L,4)*WS(IA(L,4),2)
     &    -DNUD*RN(L,1)*A(L,1)*WS(IA(L,1),5)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),4)
     &    +A(L,3)*WS(IA(L,3),3)+A(L,4)*WS(IA(L,4),3))
        DDIV(L)=-A(L,2)*WS(IA(L,2),2)
     &    +A(L,3)*WS(IA(L,3),1)+A(L,4)*WS(IA(L,4),1)
     &    -RN(L,1)*A(L,1)*WS(IA(L,1),6)
     &    +DNUD*2*(RN(L,1)+1)*(-A(L,2)*WS(IA(L,2),3)
     &    -A(L,3)*WS(IA(L,3),4)-A(L,4)*WS(IA(L,4),4))
        DPHI(L)=A(L,2)*WS(IA(L,2),3)
     &    +A(L,3)*WS(IA(L,3),4)+A(L,4)*WS(IA(L,4),4)
      END DO

*/ 回転系に戻るために以下の変換が必要
      
      DO L=1,LM
        DAVT(L)=DAVT(L)-OMEGA*IRM(L,2)*AVT(IRM(L,1))
        DDIV(L)=DDIV(L)-OMEGA*IRM(L,2)*DIV(IRM(L,1))
        DPHI(L)=DPHI(L)-OMEGA*IRM(L,2)*PHI(IRM(L,1))
      END DO
      DDIV(7)=DDIV(7)-OMEGA*OMEGA*2/SQRT(5D0)

      END
***********************************************************************
      SUBROUTINE SPLSG7(MM,JM,KM,
     &  S1,S2,S3,S4,S5,S6,S7,W1,W2,W3,W4,W5,W6,W7,Y,P,R,Q)
*-----------------------------------------------------------------------
* 以下にある ifdef によるループの分解は, 今一つ賢くない SX4 のコンパイラ
* でも外側ループのアンローリングができるようにするためのもので, VPでは,
* 不要(むしろ有害)である.
*-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S1(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S2(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S3(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S4(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S5(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S6(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION S7(KM*((MM+1)/2*2+3),0:MM/2+1,2)
      DIMENSION W1(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W2(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W3(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W4(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W5(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W6(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION W7(KM*((MM+1)/2+MM+1),2,JM/2,2)
      DIMENSION Y(JM/2)
      DIMENSION P(KM*((MM+1)/2+MM+1)*JM)
      DIMENSION R(KM*((MM+1)/2*2+3),0:MM/2)
      DIMENSION Q(KM*((MM+1)/2+MM+1),2,JM/2)

      MMP=(MM+1)/2
      MMD=MM/2
      JH=JM/2

      CALL BSCOPY(KM*((MM+1)/2+MM+1)*JM,P,Q)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W1)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W2)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W3)      
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W4)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W5)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W6)
      CALL BSSET0(KM*(MM+1+MMP)*2*JM,W7)
      
      L=0
      DO J=1,JH
        DO K=1,KM*(2*MMP+2)
          M=K+L*KM
          W1(M,1,J,1)=W1(M,1,J,1)+S1(K+KM,L,1)*Q(M,1,J)
          W1(M,2,J,1)=W1(M,2,J,1)+S1(K+KM,L,2)*Q(M,1,J)
          W2(M,1,J,1)=W2(M,1,J,1)+S2(K+KM,L,1)*Q(M,1,J)
          W2(M,2,J,1)=W2(M,2,J,1)+S2(K+KM,L,2)*Q(M,1,J)
          W3(M,1,J,1)=W3(M,1,J,1)+S3(K+KM,L,1)*Q(M,1,J)
          W3(M,2,J,1)=W3(M,2,J,1)+S3(K+KM,L,2)*Q(M,1,J)
          W4(M,1,J,1)=W4(M,1,J,1)+S4(K+KM,L,1)*Q(M,1,J)
          W4(M,2,J,1)=W4(M,2,J,1)+S4(K+KM,L,2)*Q(M,1,J)
          W5(M,1,J,1)=W5(M,1,J,1)+S5(K+KM,L,1)*Q(M,1,J)
          W5(M,2,J,1)=W5(M,2,J,1)+S5(K+KM,L,2)*Q(M,1,J)
          W6(M,1,J,1)=W6(M,1,J,1)+S6(K+KM,L,1)*Q(M,1,J)
          W6(M,2,J,1)=W6(M,2,J,1)+S6(K+KM,L,2)*Q(M,1,J)
          W7(M,1,J,1)=W7(M,1,J,1)+S7(K+KM,L,1)*Q(M,1,J)
          W7(M,2,J,1)=W7(M,2,J,1)+S7(K+KM,L,2)*Q(M,1,J)
          W1(M,1,J,2)=W1(M,1,J,2)+S1(K,L+1,1)*Q(M,2,J)
          W1(M,2,J,2)=W1(M,2,J,2)+S1(K,L+1,2)*Q(M,2,J)
          W2(M,1,J,2)=W2(M,1,J,2)+S2(K,L+1,1)*Q(M,2,J)
          W2(M,2,J,2)=W2(M,2,J,2)+S2(K,L+1,2)*Q(M,2,J)
          W3(M,1,J,2)=W3(M,1,J,2)+S3(K,L+1,1)*Q(M,2,J)
          W3(M,2,J,2)=W3(M,2,J,2)+S3(K,L+1,2)*Q(M,2,J)
          W4(M,1,J,2)=W4(M,1,J,2)+S4(K,L+1,1)*Q(M,2,J)
          W4(M,2,J,2)=W4(M,2,J,2)+S4(K,L+1,2)*Q(M,2,J)
          W5(M,1,J,2)=W5(M,1,J,2)+S5(K,L+1,1)*Q(M,2,J)
          W5(M,2,J,2)=W5(M,2,J,2)+S5(K,L+1,2)*Q(M,2,J)
          W6(M,1,J,2)=W6(M,1,J,2)+S6(K,L+1,1)*Q(M,2,J)
          W6(M,2,J,2)=W6(M,2,J,2)+S6(K,L+1,2)*Q(M,2,J)
          W7(M,1,J,2)=W7(M,1,J,2)+S7(K,L+1,1)*Q(M,2,J)
          W7(M,2,J,2)=W7(M,2,J,2)+S7(K,L+1,2)*Q(M,2,J)
        END DO
      END DO
      DO L=2,MMD-1,2
        DO J=1,JH
          DO K=1,KM*(2*MMP+2)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            W1(M,1,J,1)=W1(M,1,J,1)+S1(K+KM,L,1)*Q(M,1,J)
            W1(M,2,J,1)=W1(M,2,J,1)+S1(K+KM,L,2)*Q(M,1,J)
            W2(M,1,J,1)=W2(M,1,J,1)+S2(K+KM,L,1)*Q(M,1,J)
            W2(M,2,J,1)=W2(M,2,J,1)+S2(K+KM,L,2)*Q(M,1,J)
            W3(M,1,J,1)=W3(M,1,J,1)+S3(K+KM,L,1)*Q(M,1,J)
            W3(M,2,J,1)=W3(M,2,J,1)+S3(K+KM,L,2)*Q(M,1,J)
            W4(M,1,J,1)=W4(M,1,J,1)+S4(K+KM,L,1)*Q(M,1,J)
            W4(M,2,J,1)=W4(M,2,J,1)+S4(K+KM,L,2)*Q(M,1,J)
            W5(M,1,J,1)=W5(M,1,J,1)+S5(K+KM,L,1)*Q(M,1,J)
            W5(M,2,J,1)=W5(M,2,J,1)+S5(K+KM,L,2)*Q(M,1,J)
            W6(M,1,J,1)=W6(M,1,J,1)+S6(K+KM,L,1)*Q(M,1,J)
            W6(M,2,J,1)=W6(M,2,J,1)+S6(K+KM,L,2)*Q(M,1,J)
            W7(M,1,J,1)=W7(M,1,J,1)+S7(K+KM,L,1)*Q(M,1,J)
            W7(M,2,J,1)=W7(M,2,J,1)+S7(K+KM,L,2)*Q(M,1,J)
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            W1(M,1,J,2)=W1(M,1,J,2)+S1(K,L+1,1)*Q(M,2,J)
            W1(M,2,J,2)=W1(M,2,J,2)+S1(K,L+1,2)*Q(M,2,J)
            W2(M,1,J,2)=W2(M,1,J,2)+S2(K,L+1,1)*Q(M,2,J)
            W2(M,2,J,2)=W2(M,2,J,2)+S2(K,L+1,2)*Q(M,2,J)
            W3(M,1,J,2)=W3(M,1,J,2)+S3(K,L+1,1)*Q(M,2,J)
            W3(M,2,J,2)=W3(M,2,J,2)+S3(K,L+1,2)*Q(M,2,J)
            W4(M,1,J,2)=W4(M,1,J,2)+S4(K,L+1,1)*Q(M,2,J)
            W4(M,2,J,2)=W4(M,2,J,2)+S4(K,L+1,2)*Q(M,2,J)
            W5(M,1,J,2)=W5(M,1,J,2)+S5(K,L+1,1)*Q(M,2,J)
            W5(M,2,J,2)=W5(M,2,J,2)+S5(K,L+1,2)*Q(M,2,J)
            W6(M,1,J,2)=W6(M,1,J,2)+S6(K,L+1,1)*Q(M,2,J)
            W6(M,2,J,2)=W6(M,2,J,2)+S6(K,L+1,2)*Q(M,2,J)
            W7(M,1,J,2)=W7(M,1,J,2)+S7(K,L+1,1)*Q(M,2,J)
            W7(M,2,J,2)=W7(M,2,J,2)+S7(K,L+1,2)*Q(M,2,J)
          END DO
        END DO
      END DO
      IF(MOD(MMD,2).EQ.0) THEN
        L=MMD
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K+KM,L-1)*Q(M,2,J)
            W1(M,1,J,1)=W1(M,1,J,1)+S1(K+KM,L,1)*Q(M,1,J)
            W1(M,2,J,1)=W1(M,2,J,1)+S1(K+KM,L,2)*Q(M,1,J)
            W2(M,1,J,1)=W2(M,1,J,1)+S2(K+KM,L,1)*Q(M,1,J)
            W2(M,2,J,1)=W2(M,2,J,1)+S2(K+KM,L,2)*Q(M,1,J)
            W3(M,1,J,1)=W3(M,1,J,1)+S3(K+KM,L,1)*Q(M,1,J)
            W3(M,2,J,1)=W3(M,2,J,1)+S3(K+KM,L,2)*Q(M,1,J)
            W4(M,1,J,1)=W4(M,1,J,1)+S4(K+KM,L,1)*Q(M,1,J)
            W4(M,2,J,1)=W4(M,2,J,1)+S4(K+KM,L,2)*Q(M,1,J)
            W5(M,1,J,1)=W5(M,1,J,1)+S5(K+KM,L,1)*Q(M,1,J)
            W5(M,2,J,1)=W5(M,2,J,1)+S5(K+KM,L,2)*Q(M,1,J)
            W6(M,1,J,1)=W6(M,1,J,1)+S6(K+KM,L,1)*Q(M,1,J)
            W6(M,2,J,1)=W6(M,2,J,1)+S6(K+KM,L,2)*Q(M,1,J)
            W7(M,1,J,1)=W7(M,1,J,1)+S7(K+KM,L,1)*Q(M,1,J)
            W7(M,2,J,1)=W7(M,2,J,1)+S7(K+KM,L,2)*Q(M,1,J)
            Q(M,2,J)=Q(M,2,J)+Y(J)*R(K,L)*Q(M,1,J)
            W1(M,1,J,2)=W1(M,1,J,2)+S1(K,L+1,1)*Q(M,2,J)
            W1(M,2,J,2)=W1(M,2,J,2)+S1(K,L+1,2)*Q(M,2,J)
            W2(M,1,J,2)=W2(M,1,J,2)+S2(K,L+1,1)*Q(M,2,J)
            W2(M,2,J,2)=W2(M,2,J,2)+S2(K,L+1,2)*Q(M,2,J)
            W3(M,1,J,2)=W3(M,1,J,2)+S3(K,L+1,1)*Q(M,2,J)
            W3(M,2,J,2)=W3(M,2,J,2)+S3(K,L+1,2)*Q(M,2,J)
            W4(M,1,J,2)=W4(M,1,J,2)+S4(K,L+1,1)*Q(M,2,J)
            W4(M,2,J,2)=W4(M,2,J,2)+S4(K,L+1,2)*Q(M,2,J)
            W5(M,1,J,2)=W5(M,1,J,2)+S5(K,L+1,1)*Q(M,2,J)
            W5(M,2,J,2)=W5(M,2,J,2)+S5(K,L+1,2)*Q(M,2,J)
            W6(M,1,J,2)=W6(M,1,J,2)+S6(K,L+1,1)*Q(M,2,J)
            W6(M,2,J,2)=W6(M,2,J,2)+S6(K,L+1,2)*Q(M,2,J)
            W7(M,1,J,2)=W7(M,1,J,2)+S7(K,L+1,1)*Q(M,2,J)
            W7(M,2,J,2)=W7(M,2,J,2)+S7(K,L+1,2)*Q(M,2,J)
          END DO
        END DO
      ELSE
        L=MMD+1
        DO J=1,JH
          DO K=1,KM*(2*MMP+1)
            M=K+L*KM-KM
            Q(M,1,J)=Q(M,1,J)+Y(J)*R(K,L-1)*Q(M,2,J)
            W1(M,1,J,1)=W1(M,1,J,1)+S1(K,L,1)*Q(M,1,J)
            W1(M,2,J,1)=W1(M,2,J,1)+S1(K,L,2)*Q(M,1,J)
            W2(M,1,J,1)=W2(M,1,J,1)+S2(K,L,1)*Q(M,1,J)
            W2(M,2,J,1)=W2(M,2,J,1)+S2(K,L,2)*Q(M,1,J)
            W3(M,1,J,1)=W3(M,1,J,1)+S3(K,L,1)*Q(M,1,J)
            W3(M,2,J,1)=W3(M,2,J,1)+S3(K,L,2)*Q(M,1,J)
            W4(M,1,J,1)=W4(M,1,J,1)+S4(K,L,1)*Q(M,1,J)
            W4(M,2,J,1)=W4(M,2,J,1)+S4(K,L,2)*Q(M,1,J)
            W5(M,1,J,1)=W5(M,1,J,1)+S5(K,L,1)*Q(M,1,J)
            W5(M,2,J,1)=W5(M,2,J,1)+S5(K,L,2)*Q(M,1,J)
            W6(M,1,J,1)=W6(M,1,J,1)+S6(K,L,1)*Q(M,1,J)
            W6(M,2,J,1)=W6(M,2,J,1)+S6(K,L,2)*Q(M,1,J)
            W7(M,1,J,1)=W7(M,1,J,1)+S7(K,L,1)*Q(M,1,J)
            W7(M,2,J,1)=W7(M,2,J,1)+S7(K,L,2)*Q(M,1,J)
          END DO
        END DO
      END IF

      END
