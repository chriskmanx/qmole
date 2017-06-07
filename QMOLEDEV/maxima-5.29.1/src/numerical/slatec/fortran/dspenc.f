      double precision function dspenc (x)
c feb 1978 edition.   w. fullerton, c3, los alamos scientific lab.
c
c evaluate a form of spence-s function defined by
c        integral from 0 to x of  -alog(abs(1-y))/y  dy.
c for abs(x).le.1, the uniformly convergent expansion
c        spenc = sum k=1,infinity  x**k / k**2     is valid.
c
c spence-s function can be used to evaluate much more general integral
c forms.  for example,
c        integral from 0 to z of  alog(a*x+b)/(c*x+d)  dx  =
c             alog(abs(b-a*d/c))*alog(abs(a*(c*z+d)/(a*d-b*c)))/c
c             - dspenc (a*(c*z+d)/(a*d-b*c)) / c.
c
c ref -- k. mitchell, philosophical magazine, 40, p.351 (1949).
c        stegun and abramowitz, ams 55, p.1004.
c
      double precision x, spencs(38), aln, pi26, xbig, d1mach, dcsevl
      external d1mach, dcsevl, initds
c
c series for spen       on the interval  0.          to  5.00000e-01
c                                        with weighted error   4.74e-32
c                                         log weighted error  31.32
c                               significant figures required  30.37
c                                    decimal places required  32.11
c
      data spencs(  1) / +.1527365598 8924058729 4668491002 8 d+0      /
      data spencs(  2) / +.8169658058 0510144035 0183818527 1 d-1      /
      data spencs(  3) / +.5814157140 7787308729 7735064118 2 d-2      /
      data spencs(  4) / +.5371619814 5415275422 4788900531 9 d-3      /
      data spencs(  5) / +.5724704675 1858262332 1060305478 2 d-4      /
      data spencs(  6) / +.6674546121 6493363436 0783543858 9 d-5      /
      data spencs(  7) / +.8276467339 7156769815 8439168901 1 d-6      /
      data spencs(  8) / +.1073315673 0306789512 7000587335 4 d-6      /
      data spencs(  9) / +.1440077294 3032394023 3459033151 3 d-7      /
      data spencs( 10) / +.1984442029 9659063678 9887713960 8 d-8      /
      data spencs( 11) / +.2794005822 1636387202 0199482161 5 d-9      /
      data spencs( 12) / +.4003991310 8833118230 7258044590 8 d-10     /
      data spencs( 13) / +.5823462892 0446384713 6813583575 7 d-11     /
      data spencs( 14) / +.8576708692 6386892780 9791477122 4 d-12     /
      data spencs( 15) / +.1276862586 2801930459 8948303343 3 d-12     /
      data spencs( 16) / +.1918826209 0425170811 6238041606 2 d-13     /
      data spencs( 17) / +.2907319206 9771381777 9579971967 3 d-14     /
      data spencs( 18) / +.4437112685 2767804625 5747364174 5 d-15     /
      data spencs( 19) / +.6815727787 4145995278 6735913560 7 d-16     /
      data spencs( 20) / +.1053017386 0155744295 4701941664 4 d-16     /
      data spencs( 21) / +.1635389806 7523771000 5182173457 0 d-17     /
      data spencs( 22) / +.2551852874 9404639323 1090164258 1 d-18     /
      data spencs( 23) / +.3999020621 9993601127 7047037951 9 d-19     /
      data spencs( 24) / +.6291501645 2168118765 1414917119 9 d-20     /
      data spencs( 25) / +.9933827435 6756776438 0388775253 3 d-21     /
      data spencs( 26) / +.1573679570 7499648167 2176380586 6 d-21     /
      data spencs( 27) / +.2500595316 8494761293 6927095466 6 d-22     /
      data spencs( 28) / +.3984740918 3838111392 1066325333 3 d-23     /
      data spencs( 29) / +.6366473210 0828438926 9132629333 3 d-24     /
      data spencs( 30) / +.1019674287 2396783670 7706197333 3 d-24     /
      data spencs( 31) / +.1636881058 9135188411 1107413333 3 d-25     /
      data spencs( 32) / +.2633310439 4176501173 4527999999 9 d-26     /
      data spencs( 33) / +.4244811560 1239768172 2436266666 6 d-27     /
      data spencs( 34) / +.6855411983 6800529168 2474666666 6 d-28     /
      data spencs( 35) / +.1109122433 4380564340 1898666666 6 d-28     /
      data spencs( 36) / +.1797431304 9998914573 6533333333 3 d-29     /
      data spencs( 37) / +.2917505845 9760951732 9066666666 6 d-30     /
      data spencs( 38) / +.4742646808 9286710613 3333333333 3 d-31     /
c
c pi26 = pi**2/6.0
      data pi26 / +1.644934066 8482264364 7241516664 6025189219 d0 /
      data nspenc, xbig / 0, 0.0d0 /
c
      if (nspenc.ne.0) go to 10
      nspenc = initds (spencs, 38, 0.1*sngl(d1mach(3)))
      xbig = 1.0d0/d1mach(3)
c
 10   if (x.gt.2.0d0) go to 60
      if (x.gt.1.0d0) go to 50
      if (x.gt.0.5d0) go to 40
      if (x.ge.0.0d0) go to 30
      if (x.gt.(-1.d0)) go to 20
c
c here if x .le. -1.0
c
      aln = dlog(1.0d0-x)
      dspenc = -pi26 - 0.5d0*aln*(2.0d0*dlog(-x)-aln)
      if (x.gt.(-xbig)) dspenc = dspenc
     1  + (1.d0 + dcsevl (4.d0/(1.d0-x)-1.d0, spencs, nspenc))/(1.d0-x)
      return
c
c -1.0 .lt. x .lt. 0.0
c
 20   dspenc = -0.5d0*dlog(1.0d0-x)**2
     1  - x*(1.d0+dcsevl(4.d0*x/(x-1.d0)-1.d0, spencs, nspenc))/(x-1.d0)
      return
c
c 0.0 .le. x .le. 0.5
c
 30   dspenc = x*(1.d0 + dcsevl (4.d0*x-1.d0, spencs, nspenc))
      return
c
c 0.5 .lt. x .le. 1.0
c
 40   dspenc = pi26
      if (x.ne.1.d0) dspenc = pi26 - dlog(x)*dlog(1.0d0-x)
     1  - (1.d0-x)*(1.d0+dcsevl(4.d0*(1.d0-x)-1.d0, spencs, nspenc))
      return
c
c 1.0 .lt. x .le. 2.0
c
 50   dspenc = pi26 - 0.5d0*dlog(x)*dlog((x-1.d0)**2/x)
     1  + (x-1.d0)*(1.d0+dcsevl(4.d0*(x-1.d0)/x-1.d0, spencs, nspenc))/x
      return
c
c x .gt. 2.0
c
 60   dspenc = 2.0d0*pi26 - 0.5d0*dlog(x)**2
      if (x.lt.xbig) dspenc = dspenc
     1  - (1.d0 + dcsevl (4.d0/x-1.d0, spencs, nspenc))/x
      return
c
      end
