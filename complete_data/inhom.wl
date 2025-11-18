(* ::Package:: *)

BeginPackage["inhom`"];


solinhom::usage = "Integrates f'+gf+h=0 with periodic bc's.";
lamint


Begin["`Private`"];


lamintold[Nt_,f_,Delta_,lam_]:=Module[{faux,f0,bigf},
faux=1/Sqrt[Nt] Fourier[f];
f0=faux[[1]];
(*Zero frequency*)
If[lam==0.,faux[[1]]=0.,faux[[1]]=faux[[1]]/lam];
(*positive frequency*)
Do[faux[[k+1]]=faux[[k+1]]/(lam-I*k*(2\[Pi])/Delta),{k,1,Nt/2-1}];
(*high frequency cosine*)
faux[[Nt/2+1]]=faux[[Nt/2+1]]*lam/(lam*lam+(Nt/2 (2\[Pi])/Delta)^2);
(*negative frequency*)
Do[faux[[k+1]]=faux[[k+1]]/(lam-I*(k-Nt)*(2\[Pi])/Delta),{k,Nt/2+1,Nt-1}];
bigf=Sqrt[Nt]*InverseFourier[faux];
{bigf,f0}
]


lamrescale=Compile[{{Nt,_Integer},{fF,_Complex,1},{Delta,_Real},{lam,_Real}},
Module[{faux=fF},
(*Zero frequency*)
f0=fF[[1]];
If[lam==0.,faux[[1]]=0.,faux[[1]]=fF[[1]]/lam];
(*positive frequency*)
Do[faux[[k+1]]=fF[[k+1]]/(lam-I*k*(2\[Pi])/Delta),{k,1,IntegerPart[Nt/2-1]}];
(*high frequency cosine*)
faux[[IntegerPart[Nt/2+1]]]=fF[[IntegerPart[Nt/2+1]]]*lam/(lam*lam+(Nt/2 (2\[Pi])/Delta)^2);
(*If[lam==0.,faux[[IntegerPart[Nt/2+1]]]=0.,faux[[IntegerPart[Nt/2+1]]]=fF[[IntegerPart[Nt/2+1]]]/lam];*)
(*negative frequency*)
Do[faux[[k+1]]=fF[[k+1]]/(lam-I*(k-Nt)*(2\[Pi])/Delta),{k,IntegerPart[Nt/2+1],Nt-1}];
(*bigf=Sqrt[Nt]*InverseFourier[faux];*)
faux
]
,CompilationTarget->"C"];


lamint[Nt_,f_,Delta_,lam_]:=Block[{bigfF,f0,bigf,fF},
fF=1/Sqrt[Nt] Fourier[f];
bigfF=lamrescale[Nt,fF,Delta,lam];
bigf=Sqrt[Nt]InverseFourier[bigfF];
{bigf,f0}]


solinhom[g_,h_,Delta_,Nt_]:=Module[{t,bigg0,g0,bigaux,f},

(*solves f'+gf+h=0 with periodic boundary conditions*)
{bigg0,g0}=lamint[Nt,g,Delta,0.];
bigaux=h*Exp[bigg0];
f=-Exp[-bigg0]*lamint[Nt,bigaux,Delta,g0][[1]];
Re[f]
]


End[];


EndPackage[];
