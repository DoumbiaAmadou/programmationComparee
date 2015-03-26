% version working for 5 dices
declare Full in
proc {Full R}
   D0 D1 D2 D3 D4
   P G
in
   R = full(primary:G secondary:P dices:[D0 D1 D2 D3 D4])
   {FD.dom 1#6 [P G]}
   {FD.dom 1#6 [D0 D1 D2 D3 D4] }
   {FD.distribute naive [P G]}
    if P == G then
      {FD.exactly 5 [D0 D1 D2 D3 D4] P}
   else
      {FD.exactly 2 [D0 D1 D2 D3 D4] P}
      {FD.exactly 3 [D0 D1 D2 D3 D4] G}
   end
   {FD.distribute naive [D0 D1 D2 D3 D4]}
end

% version working for 5 dices
declare
proc {Suite R}
   Dices
   Low
in
   R = suites(begin:Low dices:Dices)
   {FD.dom 1#2 [Low]}
   {FD.distribute ff [Low]}
   {FD.list 5 Low#6 Dices}
   {FD.exactly 1 Dices Low}
   {FD.exactly 1 Dices Low+1}
   {FD.exactly 1 Dices Low+2}
   {FD.exactly 1 Dices Low+3}
   {FD.exactly 1 Dices Low+4}
   {FD.distribute ff Dices}
end


% version for arbitrary nb of dices, and specification
declare
fun {FullN N Great Small}
   proc {Script R}
      Dices
      Pri Sec
   in
      R = full(dices:Dices)
      {FD.dom 1#6 [Pri Sec]}
      {FD.list N 1#6 Dices}
      {FD.distribute naive [Pri Sec]}
      if Pri == Sec then
         {FD.exactly N Dices Pri}
      else
         {FD.exactly Small Dices Pri}
         {FD.exactly Great Dices Sec}
      end
      {FD.distribute naive Dices}
   end
in
   {SearchAll Script}
end

% version working for arbitrary number of dices
declare
fun {SuiteN N}
   proc {Script R}
      Dices
      Low MaxLow
      proc {Follow L N S}
         if S == 0 then {FD.distinct L}  
         else
            {FD.exactly 1 L N}
            {Follow L N+1 S-1}
         end
      end
   in
      R = suite(dices:Dices)
      MaxLow = 6 - N + 1
      {FD.dom 1#MaxLow [Low]}
      {FD.distribute ff [Low]}
      {FD.list N Low#6 Dices}
      {Follow Dices Low N}
      {FD.distribute ff Dices}
   end
   in {SearchAll Script}
end


%{Browse {SearchAll Full}}
{Browse {FullN 5 3 2}}
%{Browse {SearchAll Suite}}
{Browse {SuiteN 5}}