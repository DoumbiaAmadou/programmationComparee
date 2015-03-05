declare
proc {CinqDFull Sol}
   local
      D1 D2 D3 D4 D5
   in
	
      Sol = sol(d1:D1 d2:D2 d3:D3 d4:Dd d5:D5)
      Sol:::1#6
      d1 =: d2
      d2 =: d3
      d4 =: d5
      {FD.distribute ff Sol}
   end
end

{Browse {SearchAll CinqDFull}}
{ExploreAll CinqDFull}

declare
proc {CinqDSuite Sol}
   local
      D1 D2 D3 D4 D5
   in
      Sol = sol(d1:D1 d2:D2 d3:D3 d4:Dd d5:D5)
      Sol:::1#6
      d1 =: d2-1
      d2 =: d3-1
      d3 =: d4-1
      d4 =: d5-1
      {FD.distribute ff Sol}
   end
end

{Browse {SearchAll CinqDFull}}
{ExploreAll CinqDFull}