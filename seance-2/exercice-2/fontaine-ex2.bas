001 REM Commore 64 Basic       Hoare Logic (bfontaine flavor)
002 REM ================       ==============================
003 REM
010 REM create your array here:
018 REM                        ________________
019 REM                        {} L:= 5 { L=5 }
020 LET L = 5
030 DIM A$(L)
040 A(0) = 18
050 A(1) = 24
060 A(2) = 15
070 A(3) = 91
080 A(4) = 25
090 REM end of array creation
100 REM
110 REM Look Ma! OCaml code!
120 LET M = A(0)
126 REM                              ____________________________
127 REM                              {I>0 /\ I<=L-1} I:=I+1 {I>0}
128 REM                        ___________________________________________
129 REM                        {I>0} while I<=L-1 do I:=I+1 {!I<=L /\ I>0}
130 FOR I = 1 TO L-1
132 REM                        Avec E : A(I) < M
133 REM                             C : M=A(I)
134 REM                             Q : M <= A(I)
135 REM                             P : A(I) <= A(I)  on a :
136 REM                        ______________     __________________
137 REM                        {E /\ P} C {Q}     {!E /\ P} skip {Q}
138 REM                        _____________________________________
139 REM                            {P} if E then C else skip {Q}
140 IF A(I) < M THEN M = A(I)
148 REM                        ______________________________
149 REM                        { I+1 = n } I := I+1 { I = n }
150 NEXT I
160 REM TADAAAAA :)
170 PRINT M
