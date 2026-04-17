(* ::Package:: *)

(* :Title: RuffiniPackage *)
(* :Context: Ruffini` *)
(* :Author: SM *)
(* :Summary:  *)
(* :Copyright: SM 2026 *)
(* :Package Version: 1 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 16/04/2026 *)
(* :Sources:  *)
(* :Limitations: educational purposes *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning:  *)

BeginPackage["Ruffini`"] (*Context of the package*)

generateInterface::usage = "function that generates the interface" (* main function*)

Begin["`Private`"] (*Private Context*)

(*function that takes the degree of the polynomial as input and returns a polynomial 
of that degree with integer roots ranging from -6 to 6*)

generatePolynom[g_Integer]:=Module[{x,radici,poli},
x=Symbol["x"]; (*specify that want the symbol x*)
radici=RandomInteger[{-6,6},g]; (*Create a list of g random integer roots*)
poli=Expand[Product[(x-r),{r,radici}]]; (*generate the polynomial through the products of the 
roots and report it in mathematical notation*)
TraditionalForm[poli]
];

(*function that generates the interface through which the user interacts. For now: Generator Button, Polynomial (esercizio) Display Panel*)
generateInterface[] := DynamicModule[{esercizio = "", gradoScelto=2},
  Column[{
  
  Row[{
      Style["Grado del polinomio: ", FontSize->14,FontFamily->"Times"], 
      Spacer[5],
      PopupMenu[Dynamic[gradoScelto], {2 -> "Grado 2", 3 -> "Grado 3", 4 -> "Grado 4"}]
    }],
    
    Spacer[10],
    
    (* Generator Button *)
    Button["Genera Esercizio", 
      esercizio = generatePolynom[gradoScelto], (*generate the polynom*)
      Method -> "Queued", (*the operation related to the button is queued*)
      ImageSize -> {100, 50}, 
      Appearance -> "FramedPalette" (*option that gives the button a raised appearance*)
    ],
    
    Spacer[10],
    
    (*Polynomial Display Panel*)
     Panel[
     Dynamic[
      Style[
       If[esercizio === "", "Clicca per generare", esercizio], 
       FontSize -> 26,      (* Font of the polynomial text *)
       FontFamily -> "Times" 
      ]
     ],
     ImageSize -> All (* Variable panel size *),
     Background -> White,
     FrameMargins -> 20 (*panel margins, text must not touch the edges*)
    ]
    
  }, Alignment -> Left]
  
];

End[]
EndPackage[]
