'cw letter variables
'1=dit, 2=dah
Dim a(4):a(0)=1:a(1)=2
Dim b(4):b(0)=2:b(1)=1:b(2)=1:b(3)=1
Dim c(4):c(0)=2:c(1)=1:c(2)=2:c(3)=1
Dim d(4):d(0)=2:d(1)=1:d(2)=1
Dim e(4):e(0)=1
Dim f(4):f(0)=1:f(1)=1:f(2)=2:f(3)=1
Dim g(4):g(0)=2:g(1)=2:g(2)=1
Dim h(4):h(0)=1:h(1)=1:h(2)=1:h(3)=1
Dim ii(4):ii(0)=1:ii(1)=1
Dim j(4):j(0)=1:j(1)=2:j(2)=2:j(3)=2
Dim k(4):k(0)=2:k(1)=1:k(2)=2
Dim l(4):l(0)=1:l(1)=2:l(2)=1:l(3)=1
Dim m(4):m(0)=2:m(1)=2
Dim n(4):n(0)=2:n(1)=1
Dim o(4):o(0)=2:o(1)=2:o(2)=2
Dim p(4):p(0)=1:p(1)=2:p(2)=2:p(3)=1
Dim q(4):q(0)=2:q(1)=2:q(2)=1:q(3)=2
Dim r(4):r(0)=1:r(1)=2:r(2)=1
Dim s(4):s(0)=1:s(1)=1:s(2)=1
Dim t(4):t(0)=2
Dim u(4):u(0)=1:u(1)=1:u(2)=2
Dim v(4):v(0)=1:v(1)=1:v(2)=1:v(3)=2
Dim w(4):w(0)=1:w(1)=2:w(2)=2
Dim x(4):x(0)=2:x(1)=1:x(2)=1:x(3)=2
Dim y(4):y(0)=2:y(1)=1:y(2)=2:y(3)=2
Dim z(4):z(0)=2:z(1)=2:z(2)=1:z(3)=1
'numbers
Dim n1(5) 'number 1
n1(0)=1:n1(1)=2:n1(2)=2:n1(3)=2:n1(4)=2
Dim n2(5)
n2(0)=1:n2(1)=1:n2(2)=2:n2(3)=2:n2(4)=2
Dim n3(5)
n3(0)=1:n3(1)=1:n3(2)=1:n3(3)=2:n3(4)=2
Dim n4(5)
n4(0)=1:n4(1)=1:n4(2)=1:n4(3)=1:n4(4)=2
Dim n5(5)
n5(0)=1:n5(1)=1:n5(2)=1:n5(3)=1:n5(4)=1
Dim n6(5)
n6(0)=2:n6(1)=1:n6(2)=1:n6(3)=1:n6(4)=1
Dim n7(5)
n7(0)=2:n7(1)=2:n7(2)=1:n7(3)=1:n7(4)=1
Dim n8(5)
n8(0)=2:n8(1)=2:n8(2)=2:n8(3)=1:n8(4)=1
Dim n9(5)
n9(0)=2:n9(1)=2:n9(2)=2:n9(3)=2:n9(4)=1
Dim n0(5)
n0(0)=2:n0(1)=2:n0(2)=2:n0(3)=2:n0(4)=2

'define letter vars for times
letter%=0 'used in randomizer
Dim atimes(99)
Dim btimes(99)
Dim ctimes(99)
Dim dtimes(99)
Dim etimes(99)
Dim ftimes(99)
Dim gtimes(99)
Dim htimes(99)
Dim itimes(99)
Dim jtimes(99)
Dim ktimes(99)
Dim ltimes(99)
Dim mtimes(99)
Dim ntimes(99)
Dim otimes(99)
Dim ptimes(99)
Dim qtimes(99)
Dim rtimes(99)
Dim stimes(99)
Dim ttimes(99)
Dim utimes(99)
Dim vtimes(99)
Dim wtimes(99)
Dim xtimes(99)
Dim ytimes(99)
Dim ztimes(99)
Dim n1times(99)
Dim n2times(99)
Dim n3times(99)
Dim n4times(99)
Dim n5times(99)
Dim n6times(99)
Dim n7times(99)
Dim n8times(99)
Dim n9times(99)
Dim n0times(99)

'define vars for position in arrays
Dim integer acounter = 0
Dim integer bcounter = 0
Dim integer ccounter = 0
Dim integer dcounter = 0
Dim integer ecounter = 0
Dim integer fcounter = 0
Dim integer gcounter = 0
Dim integer hcounter = 0
Dim integer icounter = 0
Dim integer jcounter = 0
Dim integer kcounter = 0
Dim integer lcounter = 0
Dim integer mcounter = 0
Dim integer ncounter = 0
Dim integer ocounter = 0
Dim integer pcounter = 0
Dim integer qcounter = 0
Dim integer rcounter = 0
Dim integer scounter = 0
Dim integer tcounter = 0
Dim integer ucounter = 0
Dim integer vcounter = 0
Dim integer wcounter = 0
Dim integer xcounter = 0
Dim integer ycounter = 0
Dim integer zcounter = 0
Dim integer n1counter = 0
Dim integer n2counter = 0
Dim integer n3counter = 0
Dim integer n4counter = 0
Dim integer n5counter = 0
Dim integer n6counter = 0
Dim integer n7counter = 0
Dim integer n8counter = 0
Dim integer n9counter = 0
Dim integer n0counter = 0

'define cw variables
fdit = 800 'frequency dit
fdah = 800 'frequency dah
ldit = 48 'length dit 60/(50*wpm)
ldah = ldit * 3 'length dah
spaceinltr = ldit 'space in letter
spacebtwltr = ldit*3'space betw. letters
farnsworth = spacebtwltr
delay = 600 'buffer for keypress
Dim diff=0 'time difference

loadvars 'load variables from flash

'assign 1000 for average calculation
For i = 0 To 99
  If atimes(i)=null Then atimes(i)=1000
  If btimes(i)=null Then btimes(i)=1000
  If ctimes(i)=null Then ctimes(i)=1000
  If dtimes(i)=null Then dtimes(i)=1000
  If etimes(i)=null Then etimes(i)=1000
  If ftimes(i)=null Then ftimes(i)=1000
  If gtimes(i)=null Then gtimes(i)=1000
  If htimes(i)=null Then htimes(i)=1000
  If itimes(i)=null Then itimes(i)=1000
  If jtimes(i)=null Then jtimes(i)=1000
  If ktimes(i)=null Then ktimes(i)=1000
  If ltimes(i)=null Then ltimes(i)=1000
  If mtimes(i)=null Then mtimes(i)=1000
  If ntimes(i)=null Then ntimes(i)=1000
  If otimes(i)=null Then otimes(i)=1000
  If ptimes(i)=null Then ptimes(i)=1000
  If qtimes(i)=null Then qtimes(i)=1000
  If rtimes(i)=null Then rtimes(i)=1000
  If stimes(i)=null Then stimes(i)=1000
  If ttimes(i)=null Then ttimes(i)=1000
  If utimes(i)=null Then utimes(i)=1000
  If vtimes(i)=null Then vtimes(i)=1000
  If wtimes(i)=null Then wtimes(i)=1000
  If xtimes(i)=null Then xtimes(i)=1000
  If ytimes(i)=null Then ytimes(i)=1000
  If ztimes(i)=null Then ztimes(i)=1000
  If n1times(i)=null Then n1times(i)=1000
  If n2times(i)=null Then n2times(i)=1000
  If n3times(i)=null Then n3times(i)=1000
  If n4times(i)=null Then n4times(i)=1000
  If n5times(i)=null Then n5times(i)=1000
  If n6times(i)=null Then n6times(i)=1000
  If n7times(i)=null Then n7times(i)=1000
  If n8times(i)=null Then n8times(i)=1000
  If n9times(i)=null Then n9times(i)=1000
  If n0times(i)=null Then n0times(i)=1000
Next i

Sub cwplay element
  If element = 1 Then
    Play tone fdit,fdit,ldit
    Pause ldit:Pause spaceinltr
  Else If element = 2 Then
    Play tone fdah,fdah,ldah
    Pause ldah:Pause spaceinltr
  End If
End Sub

'calculate average from array
Function getavg(times())
  sum = 0
  'add all times in array
  For i = 0 To 99
    sum = sum + times(i)
  Next i
  'calculate average
  If sum/100 <= 1000 Then
    getavg = sum / 100
  Else
    getavg = 1000
  End If
End Function

Sub getduration answer
  starttime = Timer
  ink$ = ""
  Text 0,10,"                          "
  Do
    Text 0,10,"waiting"
    'get time
    endtime = Timer
    diff = endtime - starttime
    diff2 = diff - delay
    If diff2 < 100 Then diff2 = 100
    ink$ = Inkey$ 'get keypress
    'display number of key pressed
    'If Asc(ink$) <> 0 Then
      'Print Asc(ink$)
      'Pause 1000
    'End If
    Select Case Asc(ink$)
    'pressed key is correct
    Case answer
      Text 0,10,"                      "
      Text 0,10,"right "
      Print diff2
      Circle 140,70,40,0,,,RGB(green)
      'asssign time to array element
      Select Case letter%
        Case 1 'a
          atimes(acounter) = diff2
        Case 2 'b
          btimes(bcounter) = diff2
        Case 3 'c
          ctimes(ccounter) = diff2
        Case 4 'd
          dtimes(dcounter) = diff2
        Case 5 'e
          etimes(ecounter) = diff2
        Case 6 'f
          ftimes(fcounter) = diff2
        Case 7 'g
          gtimes(gcounter) = diff2
        Case 8 'h
          htimes(hcounter) = diff2
        Case 9 'i
          itimes(icounter) = diff2
        Case 10'j
          jtimes(jcounter) = diff2
        Case 11'k
          ktimes(kcounter) = diff2
        Case 12'l
          ltimes(lcounter) = diff2
        Case 13'm
          mtimes(mcounter) = diff2
        Case 14'n
          ntimes(ncounter) = diff2
        Case 15'o
          otimes(ocounter) = diff2
        Case 16'p
          ptimes(pcounter) = diff2
        Case 17'q
          qtimes(qcounter) = diff2
        Case 18'r
          rtimes(rcounter) = diff2
        Case 19's
          stimes(scounter) = diff2
        Case 20't
          ttimes(tcounter) = diff2
        Case 21'u
          utimes(ucounter) = diff2
        Case 22'v
          vtimes(vcounter) = diff2
        Case 23'w
          wtimes(wcounter) = diff2
        Case 24'x
          xtimes(xcounter) = diff2
        Case 25'y
          ytimes(ycounter) = diff2
        Case 26'z
          ztimes(zcounter) = diff2
        Case 27'1
          n1times(n1counter) = diff2
        Case 28'2
          n2times(n2counter) = diff2
        Case 29'3
          n3times(n3counter) = diff2
        Case 30'4
          n4times(n4counter) = diff2
        Case 31'5
          n5times(n5counter) = diff2
        Case 32'6
          n6times(n6counter) = diff2
        Case 33'7
          n7times(n7counter) = diff2
        Case 34'8
          n8times(n8counter) = diff2
        Case 35'9
          n9times(n9counter) = diff2
        Case 36'0
          n0times(n0counter) = diff2
      End Select
      'add all times for avg calc
      For i = 0 To 99
        Select Case letter%
          Case 1 'a
            asum=asum+atimes(acounter)
          Case 2 'b
            bsum=bsum+btimes(bcounter)
          Case 3 'c
            csum=csum+ctimes(ccounter)
          Case 4 'd
            dsum=dsum+dtimes(dcounter)
          Case 5 'e
            esum=esum+etimes(ecounter)
          Case 6 'f
            fsum=fsum+ftimes(fcounter)
          Case 7 'g
            gsum=gsum+gtimes(gcounter)
          Case 8 'h
            hsum=hsum+htimes(hcounter)
          Case 9 'i
            isum=isum+itimes(icounter)
          Case 10'j
            jsum=jsum+jtimes(jcounter)
          Case 11'k
            ksum=ksum+ktimes(kcounter)
          Case 12'l
            lsum=lsum+ltimes(lcounter)
          Case 13'm
            msum=msum+mtimes(mcounter)
          Case 14'n
            nsum=nsum+ntimes(ncounter)
          Case 15'o
            osum=osum+otimes(ocounter)
          Case 16'p
            psum=psum+ptimes(pcounter)
          Case 17'q
            qsum=qsum+qtimes(qcounter)
          Case 18'r
            rsum=rsum+rtimes(rcounter)
          Case 19's
            ssum=ssum+stimes(scounter)
          Case 20't
            tsum=tsum+ttimes(tcounter)
          Case 21'u
            usum=usum+utimes(ucounter)
          Case 22'v
            vsum=vsum+vtimes(vcounter)
          Case 23'w
            wsum=wsum+wtimes(wcounter)
          Case 24'x
            xsum=xsum+xtimes(xcounter)
          Case 25'y
            ysum=ysum+ytimes(ycounter)
          Case 26'z
            zsum=zsum+ztimes(zcounter)
          Case 27'1
            n1sum=n1sum+n1times(n1counter)
          Case 28'2
            n2sum=n2sum+n2times(n2counter)
          Case 29'3
            n3sum=n3sum+n3times(n3counter)
          Case 30'4
            n4sum=n4sum+n4times(n4counter)
          Case 31'5
            n5sum=n5sum+n5times(n5counter)
          Case 32'6
            n6sum=n6sum+n6times(n6counter)
          Case 33'7
            n7sum=n7sum+n7times(n7counter)
          Case 34'8
            n8sum=n8sum+n8times(n8counter)
          Case 35'9
            n9sum=n9sum+n9times(n9counter)
          Case 36'0
            n0sum=n0sum+n0times(n0counter)
        End Select
      Next i
      'go to end of sub
      GoTo endduration
    Case 149 ' F5 to exit program
      savevars
      End
    Case Else 'wrong entries
      If ink$ <> "" Then
        Text 0,10,"                    "
        Text 0,10,"wrong"
        Circle 140,70,40,0,,,RGB(red)
        Select Case letter%
        Case 1 'a
          Print " A"
          atimes(acounter) = 1000
        Case 2 'b
          Print " B"
          btimes(bcounter) = 1000
        Case 3 'c
          Print " C"
          ctimes(ccounter) = 1000
        Case 4 'd
          Print " D"
          dtimes(dcounter) = 1000
        Case 5 'e
          Print " E"
          etimes(ecounter) = 1000
        Case 6 'f
          Print " F"
          ftimes(fcounter) = 1000
        Case 7 'g
          Print " G"
          gtimes(gcounter) = 1000
        Case 8 'h
          Print " H"
          htimes(hcounter) = 1000
        Case 9 'i
          Print " I"
          itimes(icounter) = 1000
        Case 10'j
          Print " J"
          jtimes(jcounter) = 1000
        Case 11'k
          Print " K"
          ktimes(kcounter) = 1000
        Case 12'l
          Print " L"
          ltimes(lcounter) = 1000
        Case 13'm
          Print " M"
          mtimes(mcounter) = 1000
        Case 14'n
          Print " N"
          ntimes(ncounter) = 1000
        Case 15'o
          Print " O"
          otimes(ocounter) = 1000
        Case 16'p
          Print " P"
          ptimes(pcounter) = 1000
        Case 17'q
          Print " Q"
          qtimes(qcounter) = 1000
        Case 18'r
          Print " R"
          rtimes(rcounter) = 1000
        Case 19's
          Print " S"
          stimes(scounter) = 1000
        Case 20't
          Print " T"
          ttimes(tcounter) = 1000
        Case 21'u
          Print " U"
          utimes(ucounter) = 1000
        Case 22'v
          Print " V"
          vtimes(vcounter) = 1000
        Case 23'w
          Print " W"
          wtimes(wcounter) = 1000
        Case 24'x
          Print " X"
          xtimes(xcounter) = 1000
        Case 25'y
          Print " Y"
          ytimes(ycounter) = 1000
        Case 26'z
          Print " Z"
          ztimes(zcounter) = 1000
        Case 27'1
          Print " 1"
          n1times(n1counter) = 1000
        Case 28'2
          Print " 2"
          n2times(n2counter) = 1000
        Case 29'3
          Print " 3"
          n3times(n3counter) = 1000
        Case 30'4
          Print " 4"
          n4times(n4counter) = 1000
        Case 31'5
          Print " 5"
          n5times(n5counter) = 1000
        Case 32'6
          Print " 6"
          n6times(n6counter) = 1000
        Case 33'7
          Print " 7"
          n7times(n7counter) = 1000
        Case 34'8
          Print " 8"
          n8times(n8counter) = 1000
        Case 35'9
          Print " 9"
          n9times(n9counter) = 1000
        Case 36'0
          Print " 0"
          n0times(n0counter) = 1000
        End Select
        GoTo endduration'goto end of sub
      End If
    End Select
    'end after 1 second without entry
    If diff-delay > 1000 Then
      Text 0,10,"                      "
      Text 0,10,"time's up"
      Circle 140,70,40,0,,,RGB(yellow)
      'store 1000 in arrayelement
      Select Case letter%
        Case 1 'a
          'acounter = acounter -1
          atimes(acounter) = 1000
          Print " A"
        Case 2 'b
          'bcounter = bcounter -1
          btimes(bcounter) = 1000
          Print " B"
        Case 3 'c
          'ccounter = ccounter -1
          ctimes(ccounter) = 1000
          Print " C"
        Case 4 'd
          'dcounter = dcounter -1
          dtimes(dcounter) = 1000
          Print " D"
        Case 5 'e
          'ecounter = ecounter -1
          etimes(ecounter) = 1000
          Print " E"
        Case 6 'f
          'fcounter = fcounter -1
          ftimes(fcounter) = 1000
          Print " F"
        Case 7 'g
          'gcounter = gcounter -1
          gtimes(gcounter) = 1000
          Print " G"
        Case 8 'h
          'hcounter = hcounter -1
          htimes(hcounter) = 1000
          Print " H"
        Case 9 'i
          'icounter = icounter -1
          itimes(icounter) = 1000
          Print " I"
        Case 10'j
          'jcounter = jcounter -1
          jtimes(jcounter) = 1000
          Print " J"
        Case 11'k
          'kcounter = kcounter -1
          ktimes(kcounter) = 1000
          Print " K"
        Case 12'l
          'lcounter = lcounter -1
          ltimes(lcounter) = 1000
          Print " L"
        Case 13'm
          'mcounter = mcounter -1
          mtimes(mcounter) = 1000
          Print " M"
        Case 14'n
          'ncounter = ncounter -1
          ntimes(ncounter) = 1000
          Print " N"
        Case 15'o
          'ocounter = ocounter -1
          otimes(ocounter) = 1000
          Print " O"
        Case 16'p
          'pcounter = pcounter -1
          ptimes(pcounter) = 1000
          Print " P"
        Case 17'q
          'qcounter = qcounter -1
          qtimes(qcounter) = 1000
          Print " Q"
        Case 18'r
          'rcounter = rcounter -1
          rtimes(rcounter) = 1000
          Print " R"
        Case 19's
          'scounter = scounter -1
          stimes(scounter) = 1000
          Print " S"
        Case 20't
          'tcounter = tcounter -1
          ttimes(tcounter) = 1000
          Print " T"
        Case 21'u
          'ucounter = ucounter -1
          utimes(ucounter) = 1000
          Print " U"
        Case 22'v
          'vcounter = vcounter -1
          vtimes(vcounter) = 1000
          Print " V"
        Case 23'w
          'wcounter = wcounter -1
          wtimes(wcounter) = 1000
          Print " W"
        Case 24'x
          'xcounter = xcounter -1
          xtimes(xcounter) = 1000
          Print " X"
        Case 25'y
          'ycounter = ycounter -1
          ytimes(ycounter) = 1000
          Print " Y"
        Case 26'z
          'zcounter = zcounter -1
          ztimes(zcounter) = 1000
          Print " Z"
        Case 27'1
          n1times(n1counter) = 1000
          Print " 1"
        Case 28'2
          n2times(n2counter) = 1000
          Print " 2"
        Case 29'3
          n3times(n3counter) = 1000
          Print " 3"
        Case 30'4
          n4times(n4counter) = 1000
          Print " 4"
        Case 31'5
          n5times(n5counter) = 1000
          Print " 5"
        Case 32'6
          n6times(n6counter) = 1000
          Print " 6"
        Case 33'7
          n7times(n7counter) = 1000
          Print " 7"
        Case 34'8
          n8times(n8counter) = 1000
          Print " 8"
        Case 35'9
          n9times(n9counter) = 1000
          Print " 9"
        Case 36'0
          n0times(n0counter) = 1000
          Print " 0"
      End Select
      GoTo endduration'end of sub
    End If
  Loop
endduration: End Sub

'load variabless from flash
Sub loadvars
  Drive "b:"
  exists = MM.Info(exists file "morsetrainer.var")
  If exists = 1 Then
    Open "morsetrainer.var" For input As #1
    For i = 0 To 99
      Line Input #1, temp$ 'atimes
      atimes(i) = Val(temp$) 'to number
      Line Input #1, temp$ 'btimes etc
      btimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      ctimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      dtimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      etimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      ftimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      gtimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      htimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      itimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      jtimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      ktimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      ltimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      mtimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      ntimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      otimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      ptimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      qtimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      rtimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      stimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      ttimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      utimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      vtimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      wtimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      xtimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      ytimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      ztimes(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      n1times(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      n2times(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      n3times(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      n4times(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      n5times(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      n6times(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      n7times(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      n8times(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      n9times(i) = Val(temp$)
      Line Input #1, temp$ 'btimes etc
      n0times(i) = Val(temp$)
    Next i
    Close #1
  End If
End Sub

'save variables to flash
Sub savevars
  '1 line per array element
  'array elements are written
  'line 1: atimes(0)
  'line 2: btimes(0) etc.
  Drive "b:"
  Open "morsetrainer.var" For output As #1
  For i = 0 To 99
    Print #1, atimes(i)
    Print #1, btimes(i)
    Print #1, ctimes(i)
    Print #1, dtimes(i)
    Print #1, etimes(i)
    Print #1, ftimes(i)
    Print #1, gtimes(i)
    Print #1, htimes(i)
    Print #1, itimes(i)
    Print #1, jtimes(i)
    Print #1, ktimes(i)
    Print #1, ltimes(i)
    Print #1, mtimes(i)
    Print #1, ntimes(i)
    Print #1, otimes(i)
    Print #1, ptimes(i)
    Print #1, qtimes(i)
    Print #1, rtimes(i)
    Print #1, stimes(i)
    Print #1, ttimes(i)
    Print #1, utimes(i)
    Print #1, vtimes(i)
    Print #1, wtimes(i)
    Print #1, xtimes(i)
    Print #1, ytimes(i)
    Print #1, ztimes(i)
    Print #1, n1times(i)
    Print #1, n2times(i)
    Print #1, n3times(i)
    Print #1, n4times(i)
    Print #1, n5times(i)
    Print #1, n6times(i)
    Print #1, n7times(i)
    Print #1, n8times(i)
    Print #1, n9times(i)
    Print #1, n0times(i)
  Next i
  Close #1
End Sub

Sub draw
  'calculate last time
  lasttime = diff2
  If lasttime > 1000 Then lasttime=1000
  lasttime = lasttime / 5
  'overdraw with black box
  Box 0,30,320,320,,0,RGB(black)

  'draw letter boxes
  'a
  Line 0,320,0,320-(aaverage/5),7,RGB(blue)
  Text 0,310,"A",,,,RGB(green)
  'b
  Line 8,320,8,320-(baverage/5),7,RGB(blue)
  Text 8,310,"B",,,,RGB(green)
  'c
  Line 16,320,16,320-(caverage/5),7,RGB(blue)
  Text 16,310,"C",,,,RGB(green)
  'd
  Line 24,320,24,320-(daverage/5),7,RGB(blue)
  Text 24,310,"D",,,,RGB(green)
  'e
  Line 32,320,32,320-(eaverage/5),7,RGB(blue)
  Text 32,310,"E",,,,RGB(green)
  'f
  Line 40,320,40,320-(faverage/5),7,RGB(blue)
  Text 40,310,"F",,,,RGB(green)
  'g
  Line 48,320,48,320-(gaverage/5),7,RGB(blue)
  Text 48,310,"G",,,,RGB(green)
  'h
  Line 56,320,56,320-(haverage/5),7,RGB(blue)
  Text 56,310,"H",,,,RGB(green)
  'i
  Line 64,320,64,320-(iaverage/5),7,RGB(blue)
  Text 64,310,"I",,,,RGB(green)
  'j
  Line 72,320,72,320-(javerage/5),7,RGB(blue)
  Text 72,310,"J",,,,RGB(green)
  'k
  Line 80,320,80,320-(kaverage/5),7,RGB(blue)
  Text 80,310,"K",,,,RGB(green)
  'l
  Line 88,320,88,320-(laverage/5),7,RGB(blue)
  Text 88,310,"L",,,,RGB(green)
  'm
  Line 96,320,96,320-(maverage/5),7,RGB(blue)
  Text 96,310,"M",,,,RGB(green)
  'n
  Line 104,320,104,320-(naverage/5),7,RGB(blue)
  Text 104,310,"N",,,,RGB(green)
  'o
  Line 112,320,112,320-(oaverage/5),7,RGB(blue)
  Text 112,310,"O",,,,RGB(green)
  'p
  Line 120,320,120,320-(paverage/5),7,RGB(blue)
  Text 120,310,"P",,,,RGB(green)
  'q
  Line 128,320,128,320-(qaverage/5),7,RGB(blue)
  Text 128,310,"Q",,,,RGB(green)
  'r
  Line 136,320,136,320-(raverage/5),7,RGB(blue)
  Text 136,310,"R",,,,RGB(green)
  's
  Line 144,320,144,320-(saverage/5),7,RGB(blue)
  Text 144,310,"S",,,,RGB(green)
  't
  Line 152,320,152,320-(taverage/5),7,RGB(blue)
  Text 152,310,"T",,,,RGB(green)
  'u
  Line 160,320,160,320-(uaverage/5),7,RGB(blue)
  Text 160,310,"U",,,,RGB(green)
  'v
  Line 168,320,168,320-(vaverage/5),7,RGB(blue)
  Text 168,310,"V",,,,RGB(green)
  'w
  Line 176,320,176,320-(waverage/5),7,RGB(blue)
  Text 176,310,"W",,,,RGB(green)
  'x
  Line 184,320,184,320-(xaverage/5),7,RGB(blue)
  Text 184,310,"X",,,,RGB(green)
  'y
  Line 192,320,192,320-(yaverage/5),7,RGB(blue)
  Text 192,310,"Y",,,,RGB(green)
  'z
  Line 200,320,200,320-(zaverage/5),7,RGB(blue)
  Text 200,310,"Z",,,,RGB(green)
  '1
  Line 208,320,208,320-(n1average/5),7,RGB(blue)
  Text 208,310,"1",,,,RGB(green)
  '2
  Line 216,320,216,320-(n2average/5),7,RGB(blue)
  Text 216,310,"2",,,,RGB(green)
  '3
  Line 224,320,224,320-(n3average/5),7,RGB(blue)
  Text 224,310,"3",,,,RGB(green)
  '4
  Line 232,320,232,320-(n4average/5),7,RGB(blue)
  Text 232,310,"4",,,,RGB(green)
  '5
  Line 240,320,240,320-(n5average/5),7,RGB(blue)
  Text 240,310,"5",,,,RGB(green)
  '6
  Line 248,320,248,320-(n6average/5),7,RGB(blue)
  Text 248,310,"6",,,,RGB(green)
  '7
  Line 256,320,256,320-(n7average/5),7,RGB(blue)
  Text 256,310,"7",,,,RGB(green)
  '8
  Line 264,320,264,320-(n8average/5),7,RGB(blue)
  Text 264,310,"8",,,,RGB(green)
  '9
  Line 272,320,272,320-(n9average/5),7,RGB(blue)
  Text 272,310,"9",,,,RGB(green)
  '0
  Line 280,320,280,320-(n0average/5),7,RGB(blue)
  Text 280,310,"0",,,,RGB(green)

  'draw benchmark lines
  'max duration
  Line 0,119,320,119,,RGB(red)
  Line 0,120,320,120,,RGB(red)
  '500ms
  Line 0,219,320,219,,RGB(green)
  Line 0,220,320,220,,RGB(green)
  'draw last time line
  Line 0,320-lasttime,320,320-lasttime,,RGB(magenta)
End Sub

'Print help
Text 0,0,"Morse Trainer by DN9HEM F5:quit"

'main loop
Do
  'calculate and draw averages
  aaverage = getavg(atimes())
  baverage = getavg(btimes())
  caverage = getavg(ctimes())
  daverage = getavg(dtimes())
  eaverage = getavg(etimes())
  faverage = getavg(ftimes())
  gaverage = getavg(gtimes())
  haverage = getavg(htimes())
  iaverage = getavg(itimes())
  javerage = getavg(jtimes())
  kaverage = getavg(ktimes())
  laverage = getavg(ltimes())
  maverage = getavg(mtimes())
  naverage = getavg(ntimes())
  oaverage = getavg(otimes())
  paverage = getavg(ptimes())
  qaverage = getavg(qtimes())
  raverage = getavg(rtimes())
  saverage = getavg(stimes())
  taverage = getavg(ttimes())
  uaverage = getavg(utimes())
  vaverage = getavg(vtimes())
  waverage = getavg(wtimes())
  xaverage = getavg(xtimes())
  yaverage = getavg(ytimes())
  zaverage = getavg(ztimes())
  n1average = getavg(n1times())
  n2average = getavg(n2times())
  n3average = getavg(n3times())
  n4average = getavg(n4times())
  n5average = getavg(n5times())
  n6average = getavg(n6times())
  n7average = getavg(n7times())
  n8average = getavg(n8times())
  n9average = getavg(n9times())
  n0average = getavg(n0times())
  draw

  ink$ = Inkey$ 'get keypress
  If Asc(ink$) = 149 Then 'F5
    savevars
    End
  End If
  'get random number for 26 letters
  letter% = Math(rand)*36

  Select Case letter%
    Case 1 'a
      If acounter > 99 Then acounter = 0
      For i = 0 To 4
        cwplay a(i)
      Next i
      'call sub with ascii
      'of right letter
      getduration 97
      acounter = acounter + 1
      Pause farnsworth*2
    Case 2 'b
      If bcounter > 99 Then bcounter = 0
      For i = 0 To 4
        cwplay b(i)
      Next i
      getduration 98
      bcounter = bcounter + 1
      Pause farnsworth*2
    Case 3 'c
      If ccounter > 99 Then ccounter = 0
      For i = 0 To 4
        cwplay c(i)
      Next i
      getduration 99
      ccounter = ccounter + 1
      Pause farnsworth*2
    Case 4 'd
      If dcounter > 99 Then dcounter = 0
      For i = 0 To 4
        cwplay d(i)
      Next i
      getduration 100
      dcounter = dcounter + 1
      Pause farnsworth*2
    Case 5 'e
      If ecounter > 99 Then ecounter = 0
      For i = 0 To 4
        cwplay e(i)
      Next i
      getduration 101
      ecounter = ecounter + 1
      Pause farnsworth*2
    Case 6 'f
      If fcounter > 99 Then fcounter = 0
      For i = 0 To 4
        cwplay f(i)
      Next i
      getduration 102
      fcounter = fcounter + 1
      Pause farnsworth*2
    Case 7 'g
      If gcounter > 99 Then gcounter = 0
      For i = 0 To 4
        cwplay g(i)
      Next i
      getduration 103
      gcounter = gcounter + 1
      Pause farnsworth*2
    Case 8 'h
      If hcounter > 99 Then hcounter = 0
      For i = 0 To 4
        cwplay h(i)
      Next i
      getduration 104
      hcounter = hcounter + 1
      Pause farnsworth*2
    Case 9 'i
      If icounter > 99 Then icounter = 0
      For i = 0 To 4
        cwplay ii(i)
      Next i
      getduration 105
      icounter = icounter + 1
      Pause farnsworth*2
    Case 10 'j
      If jcounter > 99 Then bcounter = 0
      For i = 0 To 4
        cwplay j(i)
      Next i
      getduration 106
      jcounter = jcounter + 1
      Pause farnsworth*2
    Case 11 'k
      If kcounter > 99 Then kcounter = 0
      For i = 0 To 4
        cwplay k(i)
      Next i
      getduration 107
      kcounter = kcounter + 1
      Pause farnsworth*2
    Case 12 'l
      If lcounter > 99 Then lcounter = 0
      For i = 0 To 4
        cwplay l(i)
      Next i
      getduration 108
      lcounter = lcounter + 1
      Pause farnsworth*2
    Case 13 'm
      If mcounter > 99 Then mcounter = 0
      For i = 0 To 4
        cwplay m(i)
      Next i
      getduration 109
      mcounter = mcounter + 1
      Pause farnsworth*2
    Case 14 'n
      If ncounter > 99 Then ncounter = 0
      For i = 0 To 4
        cwplay n(i)
      Next i
      getduration 110
      ncounter = ncounter + 1
      Pause farnsworth*2
    Case 15 'o
      If ocounter > 99 Then ocounter = 0
      For i = 0 To 4
        cwplay o(i)
      Next i
      getduration 111
      ocounter = ocounter + 1
      Pause farnsworth*2
    Case 16 'p
      If pcounter > 99 Then pcounter = 0
      For i = 0 To 4
        cwplay p(i)
      Next i
      getduration 112
      pcounter = pcounter + 1
      Pause farnsworth*2
    Case 17 'q
      If qcounter > 99 Then qcounter = 0
      For i = 0 To 4
        cwplay q(i)
      Next i
      getduration 113
      qcounter = qcounter + 1
      Pause farnsworth*2
    Case 18 'r
      If rcounter > 99 Then rcounter = 0
      For i = 0 To 4
        cwplay r(i)
      Next i
      getduration 114
      rcounter = rcounter + 1
      Pause farnsworth*2
    Case 19 's
      If scounter > 99 Then scounter = 0
      For i = 0 To 4
        cwplay s(i)
      Next i
      getduration 115
      scounter = scounter + 1
      Pause farnsworth*2
    Case 20 't
      If tcounter > 99 Then tcounter = 0
      For i = 0 To 4
        cwplay t(i)
      Next i
      getduration 116
      tcounter = tcounter + 1
      Pause farnsworth*2
    Case 21 'u
      If ucounter > 99 Then ucounter = 0
      For i = 0 To 4
        cwplay u(i)
      Next i
      getduration 117
      ucounter = ucounter + 1
      Pause farnsworth*2
    Case 22 'v
      If vcounter > 99 Then vcounter = 0
      For i = 0 To 4
        cwplay v(i)
      Next i
      getduration 118
      vcounter = vcounter + 1
      Pause farnsworth*2
    Case 23 'w
      If wcounter > 99 Then wcounter = 0
      For i = 0 To 4
        cwplay w(i)
      Next i
      getduration 119
      wcounter = wcounter + 1
      Pause farnsworth*2
    Case 24 'x
      If xcounter > 99 Then xcounter = 0
      For i = 0 To 4
        cwplay x(i)
      Next i
      getduration 120
      xcounter = xcounter + 1
      Pause farnsworth*2
    Case 25 'y
      If ycounter > 99 Then ycounter = 0
      For i = 0 To 4
        cwplay y(i)
      Next i
      getduration 121
      ycounter = ycounter + 1
      Pause farnsworth*2
    Case 26 'z
      If zcounter > 99 Then zcounter = 0
      For i = 0 To 4
        cwplay z(i)
      Next i
      getduration 122
      zcounter = zcounter + 1
      Pause farnsworth*2
    Case 27 '1
      If n1zcounter>99 Then n1zcounter=0
      For i = 0 To 4
        cwplay n1(i)
      Next i
      getduration 49
      n1counter = n1counter + 1
      Pause farnsworth*2
    Case 28 '2
      If n2counter>99 Then n2counter=0
      For i = 0 To 4
        cwplay n2(i)
      Next i
      getduration 50
      n2counter = ncounter + 1
      Pause farnsworth*2
    Case 29 '3
      If n3counter>99 Then n3counter=0
      For i = 0 To 4
        cwplay n3(i)
      Next i
      getduration 51
      n3counter = n3counter + 1
      Pause farnsworth*2
    Case 30 '4
      If n4counter>99 Then n4counter=0
      For i = 0 To 4
        cwplay n4(i)
      Next i
      getduration 52
      n4counter = n4counter + 1
      Pause farnsworth*2
    Case 31 '5
      If n5counter>99 Then n5counter=0
      For i = 0 To 4
        cwplay n5(i)
      Next i
      getduration 53
      n5counter = n5counter + 1
      Pause farnsworth*2
    Case 32 '6
      If n6counter>99 Then n6counter=0
      For i = 0 To 4
        cwplay n6(i)
      Next i
      getduration 54
      n6counter = n6counter + 1
      Pause farnsworth*2
    Case 33 '7
      If n7counter>99 Then n7counter=0
      For i = 0 To 4
        cwplay n7(i)
      Next i
      getduration 55
      n7counter = n7counter + 1
      Pause farnsworth*2
    Case 34 '8
      If n8counter>99 Then n8counter=0
      For i = 0 To 4
        cwplay n8(i)
      Next i
      getduration 56
      n8counter = n8counter + 1
      Pause farnsworth*2
    Case 35 '9
      If n1counter>99 Then n1counter=0
      For i = 0 To 4
        cwplay n9(i)
      Next i
      getduration 57
      n9counter = n9counter + 1
      Pause farnsworth*2
    Case 36 '0
      If n0counter>99 Then n0counter=0
      For i = 0 To 4
        cwplay n0(i)
      Next i
      getduration 48
      n0counter = n0counter + 1
      Pause farnsworth*2
  End Select
Loop
