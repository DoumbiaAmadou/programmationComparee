0100 REM Fontaine's ed
0110 REM     version 1
0120 REM     codename "Do Reinvent The Wheel To Learn How To Build A Wheel"
0130 REM input file
0140 ID%=2:IN$="": ? "input":INPUT IN$
0150 REM output file
0160 OD%=3:OU$="": ? "output":INPUT OU$
0170 REM last command
0180 CM$=""
0190 REM current line number
0200 LN%=0
0210 REM current line
0220 LS$=""
0225 REM return code
0226 RE%=0
0230 REM open the input file
0240 OPEN ID%, 1, 0, IN$
0250 REM open the output file
0260 OPEN OD%, 1, 1, OU$
0270 REM start the editor
0280 GOTO 1000

1000 REM editor
1010 REM read the command
1020 GOSUB 2000
1030 REM interpret the command
1040 GOSUB 3000
1050 REM continue the editor loop
1060 GOTO 1000

2000 REM read the command
2010 INPUT CM$
2020 RETURN

3000 REM interpret the command
3010 REM TODO for now, just copy the text
3020 INPUT# ID%,LS$
3029 REM FIXME
3030 PRINT# OD%,LS$
3040 RETURN

9000 REM end of the program
9010 REM close the input & output files
9020 CLOSE ID%: CLOSE OD%
9030 ? "End of the program. Return "RE%
