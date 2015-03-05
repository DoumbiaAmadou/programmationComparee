declare Full in
proc {Full R}
   D0 D1 D2 D3 D4
   P G
in
   R = full(p:P g:G dices:[D0 D1 D2 D3 D4])
   {FD.dom 1#6 [P G]}
%   {FD.distinct [P G]}
   {FD.dom 1#6 [D0 D1 D2 D3 D4] }
   {FD.distribute naive [P G]}
   {FD.exactly 2 [D0 D1 D2 D3 D4] P}
   {FD.exactly 3 [D0 D1 D2 D3 D4] G}
   {FD.distribute naive [D0 D1 D2 D3 D4]}
end



declare
proc {Suite R}
   Dices
   Low
in
   {FD.dom 1#2 [Low]}
   {FD.list 5 Low#6 Dices}
%   {FD.dom Low#6 Dices}
   {FD.distribute ff [Low]}
   {FD.exactly 1 Dices Low}
   {FD.exactly 1 Dices Low+1}
   {FD.exactly 1 Dices Low+2}
   {FD.exactly 1 Dices Low+3}
   {FD.exactly 1 Dices Low+4}
   R = suites(Dices)
   {FD.distribute ff Dices}
end

{Browse {SearchAll Full}}
{Browse {SearchAll Suite}}