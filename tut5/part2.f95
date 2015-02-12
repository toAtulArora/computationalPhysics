program part2
  real :: x(0:50),fx(0:50),xb(0:250),intb(0:250),fb(0:250),dfb,ddfb,dfx(0:50)
  integer :: i=0,ier,nuse=50
  open(1,file="original1")
  do i=0,50
     x(i)=i*(5.0/50.0)
     fx(i)=f(x(i))
     dfx(i)=df(x(i))
     write(2,*) x(i),fx(i),dfx(i)
  end do
  !call divdif(0.5,x,fx,nuse,50,fb,0.01,ier,dfb,ddfb)
  close(1)

  open(1,file="predicted1")
  do i=0,250
     xb(i)=i*(5.0/250.0)
     call divdif(xb(i),x,fx,nuse,50,fb,0.01,ier,dfb,ddfb)
     write(1,*) xb(i),dfb,df(xb(i)),dfb-df(xb(i))
  end do
  close(1)

  open(1,file="command")
  write(1,*) "set terminal jpeg"
  write(1,*) "set output 'part2.jpg'"
!  write(1,*) "plot 'predicted1' using 1:2, 'predicted1' using 1:3, 'predicted1' using 1:4"
  write(1,*) "plot 'predicted1' using 1:4 w l"
  close(1)

  call system("gnuplot 'command'")
  
contains
  function f(x)
    real :: f,x
    f=sin(x*x)
  end function f

  function df(x)
    real :: df,x
    df=cos(x*x)*2*x
  end function df

  !Interpolation using Newton's divided difference formula
!
  !XB : (input) value of x at which interpolation is required
  !X : (input) real array of length NTAB containing x values
  !F : (input) real array of length NTAB containing function values
  !F(I) is the tabulated function value at X(I).
  !NUSE : (input/output) Number of points to be used for interpolation
  !After execution it will contain the number actually used
  !NTAB : (input) Number of points in the table
  !FB : (output) Real array containing interpolated values
  !       FB(I) should contain interpolation using I points
  !       FB(NUSE) should be the final value
  !AEPS : (input) Required accuracy
  !IER : Error parameter, IER=0 if the execution is successful
  !IER=21 implies NUSE<1, in which case it is set to MIN(6,NTAB)
  !IER=22 implies NUSE>NTAB or NMAX, in which case it is reduced
  !IER=23 implies interpolation has not converged to specified accuracy
  !DFB : (output) First derivative of interpolating function at XB
  !DDFB : (output) Second derivative of interpolating function at XB
!
  !Required routines : NEARST


      SUBROUTINE DIVDIF(XB,X,F,NUSE,NTAB,FB,AEPS,IER,DFB,DDFB)
!      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(NMAX=10)
      DIMENSION X(NTAB),F(NTAB),FB(*),XN(NMAX),XD(NMAX)

      !Find the nearest point
      NEXT=NEARST(XB,X,NTAB)
      FB(1)=F(NEXT)
      XD(1)=F(NEXT)
      XN(1)=X(NEXT)
      IER=0
      PX=1.0

      !Initialisation for the derivatives
      DFB=0.0
      DDFB=0.0
      DPX=0.0
      DDPX=0.0

      !Points between IN and IP are used for interpolation
      IP=NEXT
      IN=NEXT

      !Maximum number of points to be used for interpolation
      NIT=MIN(NMAX,NUSE,NTAB)
      IF(NUSE.GT.NMAX.OR.NUSE.GT.NTAB) IER=22
      IF(NUSE.LT.1) THEN
        IER=21
        NIT=MIN(6,NTAB,NMAX)
      ENDIF
      NUSE=1

      !Calculate successive interpolation polynomial
      DO 5000 J=2,NIT

         !Choose the next nearest point to XB
        IF(IN.LE.1) GO TO 2200
        IF(IP.GE.NTAB) GO TO 2000
        IF(ABS(XB-X(IP+1)).LT.ABS(XB-X(IN-1))) GO TO 2200
2000    IN=IN-1
        NEXT=IN
        GO TO 2800
2200    IP=IP+1
        NEXT=IP

        !Calculating the divided differences
2800    XD(J)=F(NEXT)
        XN(J)=X(NEXT)
        DO 3000 K=J-1,1,-1
3000    XD(K)=(XD(K+1)-XD(K))/(XN(J)-XN(K))

           !Calculating the derivatives
        DDPX=DDPX*(XB-XN(J-1))+2.*DPX
        DPX=DPX*(XB-XN(J-1))+PX
        DFB=DFB+DPX*XD(1)
        DDFB=DDFB+DDPX*XD(1)

        PX=PX*(XB-XN(J-1))
        ERR=XD(1)*PX
        FB(J)=FB(J-1)+ERR
        NUSE=J

        IF(ABS(ERR).LT.AEPS) RETURN
5000  CONTINUE

      IER=23
      END

      !To locate the nearest point in an ordered table using bisection
!
      !XB : (input) given value of x for which nearest point is needed
      !X : (input) array of length NTAB containing table of values
      !NTAB : (input) length of table
      !After execution X(NEARST) is the tabular point closest to XB 
!
      !Required routines : None

      FUNCTION NEARST(XB,X,NTAB)
!      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(NTAB)

      LOW=1
      IGH=NTAB
      IF(.NOT.(XB.LT.X(LOW).EQV.XB.LT.X(IGH))) THEN

         !If the point is within the range of table, then locate it by bisection

1500    IF(IGH-LOW.GT.1) THEN
          MID=(LOW+IGH)/2
          IF(XB.LT.X(MID).EQV.XB.LT.X(LOW)) THEN
            LOW=MID
          ELSE
            IGH=MID
          ENDIF
          GO TO 1500
        ENDIF
      ENDIF

      IF(ABS(XB-X(LOW)).LT.ABS(XB-X(IGH))) THEN
        NEARST=LOW
      ELSE
        NEARST=IGH
      ENDIF
      END

end program
