set Cells;   # vertex set of the spacial graph
set Species; # Names of Species in the problem
set Landuses; # Name of possible Landuses

param SpeciesSuitabilityLanduse {Species, Landuses,Cells} ; #suitability for each species in each cell in each Landuses
param TransitionCost {Cells, Landuses}; # Cost of transforming a cell to this landuse
param b; #budget

var LanduseDecision {c in Cells,l in Landuses} binary; # decision on which landuse to use for cell Cell

minimize InvShanonDiv{s in Species}:
(sum{c in Cells, l in Landuses} LanduseDecision[c,l]*SpeciesSuitabilityLanduse[s,l,c]/
 sum{t in Species, d in Cells, m in Landuses} LanduseDecision[d,m]*SpeciesSuitabilityLanduse[t,m,d])*log(sum{c in Cells, l in Landuses} LanduseDecision[c,l]*SpeciesSuitabilityLanduse[s,l,c]/
 sum{t in Species, d in Cells, m in Landuses} LanduseDecision[d,m]*SpeciesSuitabilityLanduse[t,m,d]);


subj to PropotionalUse{c in Cells}:
  sum{l in Landuses} LanduseDecision[c,l] = 1;

subj to Budget:
  sum{c in Cells, l in Landuses} LanduseDecision[c,l]*TransitionCost[c,l] <= b;
