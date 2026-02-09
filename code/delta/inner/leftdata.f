      subroutine leftdata(ny, d, x, Delta, fc, psic, y, debug)

      implicit none
      integer ny
      double precision x, Delta, d, fc(ny), psic(ny), y(ny)
      logical debug

      integer nymax, j
      include '../nymax.inc'
      double precision coeff1(nymax), coeff2(nymax),
     $     f(nymax), mu(nymax), pi(nymax), psi(nymax),
     $     f0(nymax),             pi0(nymax), psi0(nymax),
     $     f2(nymax), mu2(nymax), pi2(nymax), psi2(nymax),
     $     f4(nymax), mu4(nymax), pi4(nymax), psi4(nymax),
     $     df0dtau(nymax), dpi0dtau(nymax), dpsi0dtau(nymax), 
     $     d2f0dtau2(nymax), d2pi0dtau2(nymax), d2psi0dtau2(nymax), 
     $     d3f0dtau3(nymax), d3pi0dtau3(nymax), d3psi0dtau3(nymax),
     $     d4psi0dtau4(nymax), junkxi(nymax), dpi2dtau(nymax),
     $     u1(nymax), du1dtau(nymax), v2(nymax), u2(nymax), 
     $     v3(nymax), u3(nymax), v1(nymax),
     $     u(nymax), v(nymax), d2u1dtau2(nymax)

      double precision maxf0, maxf2, maxf4, maxmu2, maxmu4,
     $     maxpi0, maxpi2, maxpi4, maxpsi0, maxpsi2, maxpsi4

      if (debug) write(6,*) 'leftdata debug at x=', x


C *** Order 0 ***

      do j=1,ny
         f0(j) = fc(j)
         psi0(j) = psic(j)
      end do
     
    
C     Free data.
      call fourdiff1(ny, f0, df0dtau, Delta)
      call fourdiff1(ny, df0dtau, d2f0dtau2, Delta)
      call fourdiff1(ny, d2f0dtau2, d3f0dtau3, Delta)
      call fourdiff1(ny, psi0, dpsi0dtau, Delta)
      call fourdiff1(ny, dpsi0dtau, d2psi0dtau2, Delta)
      call fourdiff1(ny, d2psi0dtau2, d3psi0dtau3, Delta)
      call fourdiff1(ny, d3psi0dtau3, d4psi0dtau4, Delta)
      
     
      
C     Solve a linear inhomogeneous ODE for u1.
      do j=1,ny
         coeff1(j) = 1.d0
         coeff2(j) = -(d - 1.d0) * psi0(j) * f0(j)
      end do
      call inhom(u1, coeff1, coeff2, Delta, ny)
      call fourdiff1(ny, u1, du1dtau, Delta)
      call fourdiff1(ny, du1dtau, d2u1dtau2, Delta)

      do j=1,ny
         v1(j)=u1(j)
      end do
C *** Order 2 ***


C     Solve a linear inhomogeneous ODE for mu2.
C      do j=1,ny
C         coeff1(j) = -1.d0
C         coeff2(j) = 2.d0 * pi0(j) * ( pi0(j) - 2.d0 * f0(j) * psi0(j) )
C      end do
C      call inhom(mu2, coeff1, coeff2, Delta, ny)
C     f2, pi2 and psi2 are given algebraically (note that we only
C     divide by f0, which never vanishes).
      do j=1,ny
C        mu2(j) = 2.d0 * pi0(j)**2 / 3.d
         f2(j) = ((d - 3.d0) * (d - 2.d0)**3 * f0(j) * u1(j)**2) 
     $           / (8.d0 * (d - 1.d0))
         u2(j) = - psi0(j)
         v2(j) = psi0(j)
C         u2(j) = (u1(j) + du1dtau(j)) / ((1.d0 - d) * f0(j))
C         v2(j) = (u1(j) + du1dtau(j)) / ((d - 1.d0) * f0(j))
      end do
C      call fourdiff1(ny, pi2, dpi2dtau, Delta)
C      do j=1, ny
C         psi2(j) = ( 2.d0 * psi0(j)
C     $             - f0(j)**2 * mu2(j) * psi0(j)
C     $             + f0(j) * ( - mu2(j) * pi0(j) + pi2(j)
C     $                       - 3.d0 * f2(j) * psi0(j) + dpi2dtau(j) 
C     $                       ) 
C     $             + dpsi0dtau(j)
C     $             ) / 5.d0 / f0(j)**2
C      end do
C     Note that up to second order we only need second derivatives of
C     psic, due to dpi2dtau.


C *** Order 3 ***

      do j=1,ny
         u3(j) = ((d - 3.d0) * (d - 2.d0)**3 * f0(j)**3 * u1(j)**3  
     $             + 4.d0*df0dtau(j) * (u1(j) + du1dtau(j))
     $             - 4.d0*f0(j) * (2.d0*u1(j) 
     $                             + 3.d0*du1dtau(j) + d2u1dtau2(j)))
     $             / (8.d0*(1.d0 - d) * f0(j)**3)
         v3(j) = ((d - 3.d0) * (d - 2.d0)**3 * f0(j)**3 * u1(j)**3  
     $             + 4.d0*df0dtau(j) * (u1(j) + du1dtau(j))
     $             - 4.d0*f0(j) * (2.d0*u1(j) 
     $                             + 3.d0*du1dtau(j) + d2u1dtau2(j)))
     $             / (8.d0*(1.d0 - d) * f0(j)**3)
      end do
      



C     Put together the expansion around x=0. 
      do j=1,ny
         f(j) = f0(j)  + x**2 * f2(j)   
         u(j) = x * u1(j) + x**2 * u2(j)   
     $                    + x**3 * u3(j)
         v(j) = x * v1(j) + x**2 * v2(j)  
     $                    + x**3 * v3(j)
      end do
      
    
C     Debugging output.
      if (debug) then
      end if

      do j=1,ny
         junkxi(j) = 0.0d0
      end do

      call yfromfields(ny, u, v, f, y)
      y(5) = Delta

C     Debugging output.
      if (debug) then

         maxf0 = 0.d0
         maxf2 = 0.d0
         maxf4 = 0.d0
         maxmu2 = 0.d0
         maxmu4 = 0.d0
         maxpi0 = 0.d0
         maxpi2 = 0.d0
         maxpi4 = 0.d0
         maxpsi0 = 0.d0
         maxpsi2 = 0.d0
         maxpsi4 = 0.d0
         do j=1,ny
             maxf0 = max( maxf0, abs(f0(j)))
             maxf2 = max( maxf2, abs(f2(j)))
             maxf4 = max( maxf4, abs(f4(j)))
             maxmu2 = max( maxmu2, abs(mu2(j)))
             maxmu4 = max( maxmu4, abs(mu4(j)))
             maxpi0 = max( maxpi0, abs(pi0(j)))
             maxpi2 = max( maxpi2, abs(pi2(j)))
             maxpi4 = max( maxpi4, abs(pi4(j)))
             maxpsi0 = max( maxpsi0, abs(psi0(j)))
             maxpsi2 = max( maxpsi2, abs(psi2(j)))
             maxpsi4 = max( maxpsi4, abs(psi4(j)))
         end do

         write(6,*)
         write(6,*) '##Info from leftdata:'
         write(6,*)
         write(6,*) '     Max f0, f2, f4: ', maxf0, maxf2, maxf4
         write(6,*) '     Max f0, f2*x2, f4*x4: ', 
     $           maxf0, maxf2 * x**2, maxf4 * x**4
         write(6,*) '     Contributions: ', 
     $           1.d0, maxf2 * x**2/maxf0, maxf4 * x**2/maxf2,
     $           ' (',maxf4 * x**4 / maxf0,')'
         write(6,*)
         write(6,*) '     Max mu0, mu2, mu4: ', 0.d0, maxmu2, maxmu4
         write(6,*) '     Max mu0, mu2*x2, mu4*x4: ',
     $           0.d0, maxmu2 * x**2, maxmu4 * x**4
         write(6,*) '     Contributions: ', 
     $           0.d0, 1.d0, maxmu4 * x**2/maxmu2
         write(6,*)
         write(6,*) '     Max pi0, pi2, pi4: ', maxpi0, maxpi2, maxpi4
         write(6,*) '     Max pi0, pi2*x2, pi4*x4: ',
     $           maxpi0, maxpi2 * x**2, maxpi4 * x**4
         write(6,*) '     Contributions: ', 
     $           1.d0, maxpi2 * x**2/maxpi0, maxpi4 * x**2/maxpi2,
     $           ' (',maxpi4 * x**4 / maxpi0,')'
         write(6,*)
         write(6,*) '     Max psi0, psi2, pxi4: ',
     $           maxpsi0, maxpsi2, maxpsi4
         write(6,*) '     Max psi0, psi2*x2, pxi4*x4: ',
     $            maxpsi0, maxpsi2 * x**2, maxpsi4 * x**4
         write(6,*) '     Contributions: ', 
     $           1.d0, maxpsi2 * x**2/maxpsi0, maxpsi4 * x**2/maxpsi2,
     $           ' (',maxpsi4 * x**4 / maxpsi0,')'
         write(6,*)

         do j=1,ny
             write(30,99) pi0(j)
             write(31,99) dpi0dtau(j)
             write(32,99) d2pi0dtau2(j)
             write(33,99) d3pi0dtau3(j)

             write(34,99) psi0(j)
             write(35,99) dpsi0dtau(j)
             write(36,99) d2psi0dtau2(j)
             write(37,99) d3psi0dtau3(j)
             write(38,99) d4psi0dtau4(j)
            
             write(40,99) f0(j)
             write(41,99) f2(j)
             write(42,99) mu2(j)
             write(43,99) pi2(j)
             write(44,99) psi2(j)
             write(45,99) f4(j)
             write(46,99) mu4(j)
             write(47,99) pi4(j)
             write(48,99) psi4(j)

             write(51,99) f(j)
             write(52,99) mu(j)
             write(53,99) pi(j)
             write(54,99) psi(j)
         end do

      end if

99    format(F28.16)

      return
      end
