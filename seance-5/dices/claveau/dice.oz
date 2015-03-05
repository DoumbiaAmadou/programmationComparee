declare Brelan
proc {Brelan Sol}
   local
      Val2 Val1 Y
   in
      Sol = {FD.record sol [d1 d2 d3 d4 d5] 1#6}
      Val1 = {FD.record sol [v] 1#6}
      Val2 = {FD.record sol [v] 1#6}
      {FD.distribute naive Val1}
      {FD.distribute naive Val2}
      if (Val1.v == Val2.v) then
	 {FD.atLeast 5 Sol Val1.v}
      else
	 {FD.atLeast 2 Sol Val1.v}
	 {FD.atLeast 3 Sol Val2.v}
      end
      {FD.distribute naive Sol}
   end
end

{Browse {SearchAll Brelan}}

declare Suite
proc {Suite Sol}
   local
      Sol1 Sol2
   in
      Sol1 = {FD.record sol [d1 d2 d3 d4 d5] 1#5}
      {FD.distinct Sol1}
      {FD.distribute naive Sol1}
      Sol2 = {Record.map Sol1 fun {$ A} A+1 end}
      Sol = Sol1|Sol2
   end
end

{Browse {SearchAll Suite}}