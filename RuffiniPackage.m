(* ::Package:: *)

(* :Title: RuffiniPackage *)
(* :Context: Ruffini` *)
(* :Author: Stefano Mercurio, Pietro Cimino, Mattia Boffa, Giovanni Previtera *)
(* :Summary: Il pacchetto una volta importato permette di invocare una funzione (generateInterface[] . 
 Tale funzione lancia un'interfaccia che genera esercizi basati sulla fattorizzazione dei polinomi .
 Gli esercizi sono risolvibili tramite il Teorema di Ruffini e/o discriminante *)
(* :Copyright: SM PC MB GP 2026 *)
(* :Package Version: 0.9 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 14/05/2026 *)
(* :Sources: Bibliografia di Mathematica *)
(* :Limitations: educational purposes *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning:  *)

BeginPackage["Ruffini`"]

generateInterface::usage = "generateInterface[] lancia un interfaccia che permette all'utente di svolgere esercizi di fattorizzazione dei polinomi.
Per svolgere tali esercizi l'utente dovr\[AGrave] applicare il teorema di Ruffini e/o il calcolo del discriminante "

Begin["`Private`"]

(* generatePolynom[g_Integer, var_] si occupa della generazione di polinomi monici con radici intere e una eventuale coppia di radici complesse.
 g_Integer : grado del polinomio , var : variabile del polinomio.
 radiciIntere : lista delle radici intere del polinomio , complessi : polinomio di grado 2 generato attraverso la parte reale e immaginaria .
 poli : il polinomio generato , a : eventuale parte reale,  b : eventuale parte immaginaria. *)
 
generatePolynom[g_Integer, var_] := Module[{radiciIntere, complessi, poli, a=Null, b=Null},

  If[RandomChoice[{False, False, False, True}],  (* Decide casualmente se includere una coppia complessa coniugata con probabilit\[AGrave] del 25% *)
    
    (*caso then : includiamo una coppia di complessi coniugati *)
    a = RandomInteger[{-4, 4}];      (* parte reale generata casualmente nell'intervallo [-4,4] *)
    b = RandomInteger[{1, 4}];       (* parte immaginaria (sempre > 0) generata casualmente nell'intervallo [1,4]]*)
    complessi = var^2 - 2a*var + (a^2 + b^2); (* Il polinomio corrispondente \[EGrave] (var - (a+bi))(var - (a-bi)) = x\.b2 - 2ax + (a\.b2+b\.b2) *)
    radiciIntere = RandomInteger[{-6, 6}, g - 2]; (* Le restanti g-2 radici sono intere e le genero casualmente *)
    poli = Expand[complessi * Product[(var - r), {r, radiciIntere}]],(*Usando la built-in Expand svolgo i prodotti e calcolo il polinomio risultante *)
    
    (*caso else : il nostro polinomio sar\[AGrave] composto solo da radici intere *)
    radiciIntere = RandomInteger[{-6, 6}, g]; (*genero casualmente le radici intere*)
    poli = Expand[Product[(var - r), {r, radiciIntere}]](* Usando la built-in Expand svolgo i prodotti e calcolo il polinomio risultante  *)
  ];
  
  {poli , a , b} (*restituisco il polinomio e le eventuali parti, reale e immaginaria , delle radici complesse *)
]

(* 
  drawRuffiniTable[poly_, r_, var_] Disegna la tabella di Ruffini per la divisione del polinomio poly
  per il binomio (var - r), dove var \[EGrave] la variabile simbolica.
  poly : il polinomio da dividere , r : la radice (divisore della forma var - r),  var : la variabile del polinomio
  coeff: lista di coefficienti del polinomio in ordine decrescente e rappresenta la prima riga della tabella , n : lunghezza di coeff
  mult : lista che rappresenta la seconda riga della tabella dove vengono messi i risultati dei prodotti 
  result : lista che rappresenta la terza riga della tabella dove vengono messi i risultati delle somme 
*)

drawRuffiniTable[poly_, r_, var_] := Module[{coeff, n, mult, result},

  coeff = Reverse[CoefficientList[poly, var]];(* Estrae i coefficienti del polinomio in ordine DECRESCENTE di grado*)
 
  n = Length[coeff]; (* Numero totale di coefficienti = grado del polinomio + 1 *)
  
  result = {coeff[[1]]}; (* La riga dei risultati inizia con il coefficiente di grado massimo, che nella divisione di Ruffini scende invariato *)
 
  mult = {""};(* La riga dei prodotti inizia con una cella vuota perch\[EAcute] sotto il primo coefficiente non c'\[EGrave] alcun prodotto *)
  
  (*  CICLO PRINCIPALE DI RUFFINI 
     Per ogni coefficiente dal secondo all'ultimo:
       1. Moltiplica l'ultimo valore di result per r  \[RightArrow]  aggiunge a mult
       2. Somma il coefficiente corrente con quel prodotto  \[RightArrow]  aggiunge a result
     
     Questo implementa lo schema:
       coeff:   a0 | a1 | a2 | ... | an
       mult:       |    | a0*r| ... |
       result:  a0 | a0*r+a1 | ...| resto
  *)
  Do[
    AppendTo[mult, Last[result] * r];      (* prodotto: ultimo risultato \[Times] r *)
    AppendTo[result, coeff[[i]] + Last[mult]], (* somma: coeff corrente + prodotto *)
    {i, 2, n}
  ];
  
  (*  COSTRUZIONE DELLA GRIGLIA 
     La tabella ha 3 righe e (n + 1) colonne:
     
     Colonna 1 : contiene r nella riga centrale)
     Colonne 2..n : coefficienti / prodotti / risultati
     Colonna n+1 : ultima cella (resto evidenziato)
  *)
  
  Grid[{
  (*Most prende tutta la lista tranne l'ultimo elemento , Last prende solo l'ultimo*)
    (* RIGA 1 \[LongDash] Coefficienti del polinomio originale:
      una celle vuota iniziale, poi tutti i coeff tranne l'ultimo in nero,
       l'ultimo in grassetto (per allinearlo visivamente con il resto) *)
    Join[{""}, Most[coeff], {Style[Last[coeff], Bold]}],
    
    (* RIGA 2 \[LongDash] Prodotti intermedi:
       mult contiene gi\[AGrave] "" nella prima posizione,
       poi i valori r*result calcolati nel ciclo.
       La radice r \[EGrave] mostrata in grassetto blu nella prima cella. *)
    Join[{Style[r, Bold, Blue]}, mult],
    
    (* RIGA 3 \[LongDash] Risultati della divisione:
       una cella vuota, poi i coefficienti del polinomio quoziente,
       l'ultimo valore (il RESTO) in rosso e grassetto *)
    Join[{""}, Most[result], {Style[Last[result], Red, Bold]}]
    
  },
  
  (* OPZIONI GRAFICHE *)
  
  (* Dividers: linee verticali e orizzontali della griglia.
     Verticali: {False, True,{False}, True, False}
       - Nessuna linea a sinistra
       - Linea dopo colonna 1 (separa la radice r)
       - Nessuna linea tra le colonne interne  {False}
       - Linea prima dell'ultima colonna (separa il resto)
       - Nessuna linea a destra
     Orizzontali: {False, False, True, False}
       - Nessuna linea sopra riga 1
       - Nessuna linea tra riga 1 e riga 2
       - Linea tra riga 2 e riga 3 (la "barra" di Ruffini)
       - Nessuna linea sotto riga 3                            *)
  Dividers -> {
    {False, True,{False}, True, False},
    {False, False, True, False}
  },
  
  ItemSize  -> {5, 2},     (* larghezza 5 e altezza 2 per ogni cella *)
  Alignment -> Center,     (* contenuto centrato in ogni cella *)
  BaseStyle -> {FontSize -> 17},  (* dimensione font globale *)
  Background -> RGBColor[0.96, 0.97, 1.0] (*colore dello sfondo*)
    ] (*restituisce in output la griglia stessa *)
];

(*possibleRoots[polynomial_, var_] genera le possibili radici intere del polinomio essenziali per il suggerimento e per estrarne una corretta.
polynomial : polinomio di input sulla quale effettuiamo i calcoli , var : variabile del polinomio 
coefficients : lista dei coefficienti 
divisorsKnownTerm : i divisori del termine noto 
candidates: possibili radici del polinomio 
correct : tutte le radici corrette 
wrong : tutte le radici sbagliate 
*)
possibleRoots[polynomial_, var_] := Module[{coefficents, divisorsKnownTerm, candidates,correct,wrong},
    coefficents = CoefficientList[polynomial, var]; (*Prelevo i coefficenti del polinomio*)
    If[coefficents[[1]]===0, {0}, (*se il termine noto \[EGrave] 0 restituisco come unica radice possibile 0 anche se non \[EGrave] cos\[IGrave] *)
        divisorsKnownTerm = Abs[Divisors[coefficents[[1]]]]; (*Il coefficente del termine noto \[EGrave] il primo coefficente della lista, una volta prelavato ottengo i suoi divisori in forma assoulta*)
        candidates = Union[divisorsKnownTerm, -divisorsKnownTerm]; (*Restituisco i divisori del termine noto sia pos che neg, le radici possibili sono queste poich\[EGrave] i polinomi sono monici *)
        (* Separo le radici corrette da quelle non corrette *)
        correct = Select[candidates, (polynomial /. var -> #) == 0 &];
        wrong  = Select[candidates, (polynomial /. var -> #) != 0 &];
       (* Restituisco tutte le corrette + quelle errate fino a riempire 6 posti e le mescolo per evitare che quelle corrette sono le prime*)
        RandomSample[Join[correct, Take[wrong, UpTo[6 - Length[correct]]]]]
        ]
    
];

(* findCorrectRoot[polynomial_, var_] Trova la prima radice intera corretta tra le candidate ,
   polynomial : polinomio di input sulla quale effettuiamo i calcoli , var : variabile del polinomio
   candidates : tutte le possibili radici intere 
   correctRoots : Tutte le radici corrette  *)
findCorrectRoot[polynomial_, var_] := Module[{candidates, correctRoots},
\[NonBreakingSpace] \[NonBreakingSpace] candidates=possibleRoots[polynomial,var]; (*considero tutte le possibili radici intere*)
\[NonBreakingSpace] \[NonBreakingSpace] correctRoots = Select[candidates, (polynomial /. var -> #) == 0 &]; (*prendo solo quelle corrette sostituendo nel polinomio all'incognita la radice candidata , verificando che il risultato sia 0*)
\[NonBreakingSpace] \[NonBreakingSpace] If[Length[correctRoots] == 0, Null (*non ci sono radici corrette*)
\[NonBreakingSpace] \[NonBreakingSpace] ,First[correctRoots]] (*restituisci la prima radice *)
\[NonBreakingSpace] ];

(*verifyAnswer[polinomioGenerato_, radice_, polinomioInput_, var_] verifica la soluzione proposta dall'utente
e restituisce il messaggio corrispondente
polinomioGenerato : polinomio corrente da fattorizzare 
radice : la radice inserita dall'utente
polinomioInput : il polinomio inserito dall'utente anche detto quoziente
var : la variabile del polinomio
*)

verifyAnswer[polinomioGenerato_, radice_, polinomioInput_, var_] := Module[{expr},
  
  (* 1. TENTATIVO DI CONVERSIONE E VALIDAZIONE *)
  (* Trasformiamo la stringa polinomioInput in un'espressione matematica di Mathematica. 
     Usiamo Quiet per non mostrare errori a video se l'utente scrive cose senza senso. *)
  

  Which[
    
    (* CASO 1: DATI MANCANTI *)
    (* Controlliamo se la radice \[EGrave] nulla o se la stringa del polinomio \[EGrave] vuota (pulita dagli spazi). *)
    radice === Null || StringTrim[polinomioInput] === "",
    {"missingData", Style["Inserisci prima tutti i dati! \|01f937\:200d\[Mars]\:fe0f", Red, 16, Bold]},

    (* CASO 2: VALIDAZIONE SINTATTICA E STRUTTURALE *)
    (* Verifichiamo tre cose:
       - radice deve essere un intero;
       - l'espressione non deve essere fallita e deve essere effettivamente un polinomio nella variabile (var). *)
    !IntegerQ[radice] || StringContainsQ[polinomioInput,"="] || StringContainsQ[polinomioInput,";"] || !StringContainsQ[polinomioInput,ToString[var]],
    {"badRoot", Style["\|01f923\|01f923\|01f923 Abbiamo un simpaticone qui!!! Controlla il formato degli input", Red, 16, Bold]},
    
    (* Mathematica contolla anche le condizioni successiva ad un true in una sequenza di "or" *)
    (expr = Quiet[ToExpression[polinomioInput]]; (expr === $Failed || !PolynomialQ[expr, var])),
    {"badRoot", Style["\|01f923\|01f923\|01f923 Abbiamo un simpaticone qui!!! Controlla il formato degli input", Red, 16, Bold]},

    (* CASO 3: RISPOSTA CORRETTA *)
    (* Verifichiamo che:
       - (polinomioInput) - (polinomioGenerato / (var - radice)) sia uguale a zero;
       - Il polinomio originale sia divisibile per (var - radice). *)
    Simplify[expr - (polinomioGenerato/(var - radice))] === 0 && (polinomioGenerato /. var -> radice) == 0,
    {"correct", Style["Ottimo! Procediamo col prossimo step \|01f44d", Green, 16, Bold]},

    (* CASO 4: RADICE CORRETTA, MA QUOZIENTE ERRATO *)
    (* Se la sostituzione della radice nel polinomio originale d\[AGrave] zero, la radice \[EGrave] giusta. 
       Se siamo qui, significa che il controllo precedente (Caso 3) \[EGrave] fallito, quindi il quoziente \[EGrave] sbagliato. *)
    (polinomioGenerato /. var -> radice) == 0,
    {"wrongQuotient", Style["Ci sei quasi! La radice \[EGrave] corretta ma non il quoziente \|01fae3", Orange, 16, Bold]},

    (* CASO 5: RISPOSTA ERRATA (DEFAULT) *)
    (* Se non siamo rientrati in nessuno dei casi precedenti, la radice inserita \[EGrave] proprio sbagliata. *)
    True,
    {"wrongRoot", Style["Riprova! La radice non \[EGrave] corretta \|01f62d", Red, 16, Bold]}
    
  ]
];
\[NonBreakingSpace] 
\[NonBreakingSpace] (*nextStep[history_, radice_, var_, listaFattori_, polinomioGenerato_] si occupa di aggionare lo storico , il polinomio corrente e la listaFattori (determinante per restituire il risultato finale) , 
\[NonBreakingSpace] tutto in funzione di uno step svolto dall'utente o in automatico dal sistema, se richiesto dall'utente , nel caso di ruffini .
\[NonBreakingSpace] La funzione oltre a restituire tali elementi aggiornati restituisce anche i valori con la quale resettare alcuni elementi dell'interfaccia stessa .
\[NonBreakingSpace] history : tabella dello storico corrente , memorizza i risultati dei vari step dell'utente fino ad ora 
\[NonBreakingSpace] listaFattori : lista che contiene elementi della forma (x-radice) con le radici trovate fino ad ora, fondamentale per il calcolo del risultato finale 
\[NonBreakingSpace] radice : radice inserita in input dall'utente 
\[NonBreakingSpace] var : variabile del polinomio 
\[NonBreakingSpace] polinomioGenerato : polinomio corrente da fattorizzare 
\[NonBreakingSpace] newHistory : storico aggiornato 
\[NonBreakingSpace] newFattori : listafattori aggiornata
\[NonBreakingSpace] newPoly : polinomio corrente aggiornato *) 
\[NonBreakingSpace] 
\[NonBreakingSpace] nextStep[history_, radice_, var_, listaFattori_, polinomioGenerato_] :=
  Module[{newHistory, newFattori, newPoly},

    newHistory  = Append[history,      {radice, TraditionalForm[Expand[Simplify[polinomioGenerato / (var - radice)]]]}];(*aggiorno la history aggiungendo radice e polinomio, il polinomio lo calcolo per gestire il caso Mostra Soluzione *)
    newFattori  = Append[listaFattori, (var - radice)]; (*aggiorno la listaFattori *)
    newPoly     = Simplify[polinomioGenerato / (var - radice)]; (*aggiorno il polinomio corrente*)

    {newHistory, newFattori, newPoly, "", "", ""} (*restituisco una lista di tutti gli elementi che desidero aggiornare *)
  ]
  
  (*nextStep[history_, radice_, var_, listaFattori_, polinomioGenerato_] si occupa di aggionare lo storico e la listaFattori (determinante per restituire il risultato finale) , 
\[NonBreakingSpace] tutto in funzione di uno step svolto dall'utente o in automatico dal sistema, se richiesto dall'utente , nel caso dei complessi .
\[NonBreakingSpace] La funzione oltre a restituire tali elementi aggiornati restituisce anche i valori con la quale resettare alcuni elementi dell'interfaccia stessa.
\[NonBreakingSpace] history : tabella dello storico corrente , memorizza i risultati dei vari step dell'utente fino ad ora 
\[NonBreakingSpace] listaFattori : lista che contiene elementi della forma (x-radice) con le radici trovate fino ad ora, fondamentale per il calcolo del risultato finale 
\[NonBreakingSpace] var : variabile del polinomio 
\[NonBreakingSpace] polinomioGenerato : polinomio corrente da fattorizzare 
\[NonBreakingSpace] a:parte reale del polinomio 
\[NonBreakingSpace] b: parte immaginaria del polinomio 
\[NonBreakingSpace] newHistory : storico aggiornato 
\[NonBreakingSpace] newFattori : listafattori aggiornata*)
\[NonBreakingSpace] 
  nextStepComplessi[listaFattori_,var_,a_,b_,history_,polinomioGenerato_]:=Module[{newHistory,newListaFattori},
  newListaFattori=Join[listaFattori, {(var - (a - b*I)),(var - (a + b*I))}];(*aggiorniamo i fattori con le radici complesse estratte durante la generazione del polinomio*)
  newHistory = Join[history, {
  {TraditionalForm[a - b*I], TraditionalForm[Simplify[polinomioGenerato/(var - (a + b*I))]]},
  {TraditionalForm[a + b*I], 1}}];(*aggiorno la history aggiungendo radici e polinomi, i polinomi li calcolo per gestire il caso Mostra Soluzione *)
  (*restituisco una lista di tutti gli elementi che desidero aggiornare *)
  {newListaFattori,newHistory,"","",""}
  ]
  
(*generateInterface[] lancia un interfaccia che permette all'utente di svolgere esercizi di fattorizzazione dei polinomi .
Per svolgere tali esercizi l'utente dovr\[AGrave] applicare il teorema di Ruffini e/o il calcolo del discriminante 
La deploy rende l'interfaccia non modificabile se non attrverso gli oppotuni campi,button ecc...*)

generateInterface[] := Deploy[DynamicModule[{
    polinomioGenerato = Null, (* Polinomio attualmente visibile all'utente nel panel*) 
    gradoScelto = 2, (* Grado del polinomio attualmente visibile modificabile tramite pop up *) 
    seed = Null, (*imposta il numero dell'esercizio*)
    seedMessage = "", (*messaggio di errore se si prova a generare senza aver impostato un seed intero*)
    radiceInput = Null, (*la radice che inserisce l'utente*)
    polinomioInput = "", (*il quoziente che l'utente inserisce *)
    feedbackMessage = "", (*messaggio di ritorno per l'utente in base all'azione corrispondente*)
    var = Null, (*variabile del polinomio *)
    showPossibleRoots = "", (*suggerimento restituito all'utente *)
    result=Null, (*ausiliaria che prende l'input della verifyanswer in verifica esercizio *)
    rootFound=Null,(*variabile ausiliaria che viene usata per memorizzare la radice corretta *)
    history={}, (*storico degli step svolti*)
    listaFattori={},(*memorizza i fattori che compongono il risultato finale*)
    griglia=Null,(*tabella di ruffini*)
    traccia=Null,(*traccia inziale dell'esercizio *)
    ruffini = True, (*flag per catturare il caso non ruffini*)
    fineEsercizio=False,(*flag per catturare il caso fine esercizio*)
    pReale=Null,(*input utente parte reale*)
    pImmaginaria=Null,(*input utente parte immaginaria*)
    a=Null,(*parte reale corretta estratta dal generatePolynom*)
    b=Null (*parte immaginaria corretta estratta dal generatePolynom*)
   
  },
  
  Pane[
  (* L'intera interfaccia e' costruita con Column[{...}], che impila
   verticalmente tutti i pannelli uno sotto l'altro, dall'alto verso il basso. *)
Column[{
  
    (* Header titolo dell'applicazione *)

Panel[
  Style[" \|01f469\:200d\|01f3eb Fattorizzazione di Polinomi \[LongDash] Ruffini & Discriminante \|01f469\:200d\|01f3eb ",
        Bold, 20, White, FontFamily -> "Helvetica"],
  (* Style applica: testo grassetto, dimensione 20, colore bianco, font Helvetica *)
  Background  -> RGBColor[0.18, 0.32, 0.55], (* sfondo blu scuro *)
  ImageSize   -> Full, (* occupa tutta la larghezza disponibile *)
  Alignment   -> Center, (* testo centrato orizzontalmente *)
  FrameMargins -> {{16, 16}, {12, 12}} (* margini interni: 16px sx/dx, 12px su/giu' *)
],
  
    (* Prima riga interfaccia : Input per il seed ,scelta del grado e bottoni genera e verifica *)
    Panel[
    Row[{
    
      (* Campo "Numero Esercizio" (seed) *)
      Column[{Style["Esercizio:", Bold, 13, RGBColor[0.18, 0.32, 0.55]],
              (* Etichetta testuale sopra il campo di input *)
              InputField[Dynamic[seed],Number, ImageSize -> {80, 30},
                         BaseStyle -> {FontSize -> 14, FontFamily -> "Helvetica"}]
              (* InputField collegato alla variabile dinamica "seed".
                 Dynamic[] fa si' che ogni modifica dell'utente aggiorni subito la variabile. *)
              }],(* scelta del numero dell'esercizio (seed) *)
     
      Spacer[10], (* spazio orizzontale di 10px tra gli elementi *)
      
      (* Messaggio di errore sul seed (appare solo se necessario) *)
      Dynamic[Style[seedMessage, 12, Red, Bold]],
      (* seedMessage e' una variabile testuale; quando e' vuota non si vede nulla,
         quando contiene un avviso lo mostra in rosso grassetto *)
      
      Spacer[10],
      
      (* Menu a tendina per scegliere il grado del polinomio *)
      Column[{Style["Grado:", Bold, 13, RGBColor[0.18, 0.32, 0.55]],
              PopupMenu[Dynamic[gradoScelto], {2 -> "Grado 2", 3 -> "Grado 3", 4 -> "Grado 4"},
                        ImageSize -> {120, 30},
                        BaseStyle -> {FontSize -> 13, FontFamily -> "Helvetica"}]
              (* PopupMenu mostra un menu a discesa.
                 Dynamic[gradoScelto] aggiorna automaticamente la variabile con la scelta dell'utente.
                 Le chiavi (2,3,4) sono i valori numerici; le stringhe sono le etichette visibili. *)
              }],(* possibili gradi del polinomio selezionabili tramite popup *)
      
      Spacer[30],
      
      (* BOTTONE "Genera Esercizio" *)
      
      Button[
         (* Mouseover gestisce l'effetto hover: mostra due versioni del bottone,
            una normale e una piu' chiara quando il mouse ci passa sopra *)
         Mouseover[
           Framed[Style["\|01f3c1  Genera Esercizio", White, Bold, 14],
                  Background -> RGBColor[0.16, 0.55, 0.30], (* verde scuro = stato normale *)
                  FrameStyle -> Directive[RGBColor[0.10, 0.40, 0.20], Thickness[2]],
                  RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {180, 42}],
           Framed[Style["\|01f3c1  Genera Esercizio", White, Bold, 14],
                  Background -> RGBColor[0.22, 0.70, 0.40], (* verde piu' chiaro = hover *)
                  FrameStyle -> Directive[RGBColor[0.10, 0.40, 0.20], Thickness[2]],
                  RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {180, 42}]
         ],
         (* Azione eseguita al click: Which[] e' come un if-elseif-else .... *)
         Which[ 
         
         seed === Null , (* Caso 1 : manca il seed *)
         seedMessage = " \|01f448 Seleziona numero esercizio",
         
         !IntegerQ[seed] || seed<0,(* Caso 2 : il seed c'\[EGrave] ma non \[EGrave] naturale *)
         seedMessage = " \|01f448 Inserire un numero naturale" ,
         
         True, (* Default case *)
         (SeedRandom[seed];
             (* SeedRandom fissa il generatore casuale: stesso seed = stesso esercizio *)
             var = RandomChoice[{Symbol["x"], Symbol["y"], Symbol["t"], Symbol["z"],Symbol["s"],Symbol["w"]}];
             (* Sceglie casualmente la variabile del polinomio (x, y, t, z, s, w) *)
             {polinomioGenerato, a , b}= generatePolynom[gradoScelto, var]; 
             (* Chiama la funzione precedentemente definita generatePolynom che restituisce:
                - il polinomio da fattorizzare
                - a, b: parametri per la gestione dei casi con radici complesse *)
             traccia=TraditionalForm[Expand[polinomioGenerato]];
             (* Salva la forma espansa del polinomio originale per mostrarla come "traccia" *)
             
             (* Reset di tutte le variabili di stato dell'esercizio. Alla prima generazione le variabili sono gi\[AGrave] "Vuote", ma dalla seconda in poi \[EGrave] necessario resettarle  *)
             listaFattori={}; (* lista dei fattori trovati finora *)
             polinomioInput = ""; (* campo quoziente svuotato *)
             radiceInput = Null; (* campo radice svuotato *)
             showPossibleRoots = ""; (* Suggerimento azzerato *)
             griglia=Null; (* tabella di Ruffini nascosta *)
             history = {}; (* storico dei passaggi azzerato *)
             seedMessage = ""; (* messaggio errore seed rimosso *)
             feedbackMessage = Style["In attesa di input utente \|01f4ad", Gray, 16, Bold];
             ruffini = True; (* si parte con la modalita' Ruffini attiva, ovvero esistono ancora radici reali *)
             fineEsercizio=False; (* l'esercizio non e' ancora completato *)
             pReale=Null; (* campo parte reale azzerato *)
             pImmaginaria=Null;) (* campo parte immaginaria azzerato *)
              ], Method -> "Preemptive", Appearance -> "Frameless"
      ],
      
      Spacer[20],
      
      (* BOTTONE "Pulisci Campi" *)
      
      Button[
        Mouseover[
          Framed[Style["\|01f9f9 Pulisci Campi", White, Bold, 14],
                 Background -> RGBColor[0.85, 0.45, 0.10], (* arancione scuro = stato normale *)
                 FrameStyle -> Directive[RGBColor[0.60, 0.30, 0.05], Thickness[2]],
                 RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {180, 42}],
          Framed[Style["\|01f9f9 Pulisci Campi", White, Bold, 14],
                 Background -> RGBColor[0.95, 0.60, 0.20], (* arancione piu' chiaro = hover *)
                 FrameStyle -> Directive[RGBColor[0.60, 0.30, 0.05], Thickness[2]],
                 RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {180, 42}]
        ],
        (* Al click: svuota solo i campi di input, senza rigenerare l'esercizio *)
        polinomioInput="";radiceInput=Null;pReale=Null;pImmaginaria=Null,
        Method -> "Preemptive", Appearance -> "Frameless"
      ]
      }, Spacer[5]], (* spaziatura tra gli elementi della Row *)
      
      Background -> RGBColor[0.96, 0.97, 1.0],
      FrameMargins -> 15,
      ImageSize -> {Full, Automatic},Alignment->Center
    ],
    
      
    
    (* Seconda riga con : pannello di visualizzazione del polinomio, della tabella storico e bottone per visualizzare la tabella di Ruffini *)
    Panel[
    Row[{

  (* PANEL POLINOMIO *)
  (* Mostra il polinomio corrente: istruzione iniziale, risultato finale o polinomio residuo *)
  Panel[
    Dynamic[
      Style[
        Which[
          polinomioGenerato === Null,
          "Clicca Genera Esercizio",
          (* nessun esercizio generato: mostra istruzione *)

          fineEsercizio,
          TraditionalForm[Apply[Times, listaFattori]],
          (* esercizio completato: mostra il prodotto di tutti i fattori trovati *)

          True,
          TraditionalForm[Expand[polinomioGenerato]]
          (* durante l'esercizio: mostra il polinomio residuo corrente *)
        ],
        24, Bold, RGBColor[0.10, 0.20, 0.45]] (* testo grande, blu scuro *)
    ],
    Background -> RGBColor[0.96, 0.97, 1.0],
    ImageSize -> All,
    FrameMargins -> 30,
    Alignment -> Center
  ],

  Spacer[15],

  (* STORICO *)
  (* Colonna con traccia originale + tabella degli step completati *)
  Column[{

    (* Mostra la "traccia": il polinomio originale generato all'inizio *)
    Dynamic[
      If[traccia =!= Null,
        Row[{
          Style["Traccia: ", Bold, 13, RGBColor[0.18, 0.32, 0.55]],
          Style[traccia, 12, Italic]
        }],
        "" (* se non c'e' ancora nulla, non mostra niente *)
      ]
    ],

    Style["Storico Passaggi:", Bold, 13, RGBColor[0.18, 0.32, 0.55]],
    
      (* Tabella storico: aggiornata dinamicamente ad ogni passaggio corretto *)
      Dynamic[
        If[history === {},
          Style["Nessun passaggio completato", Italic, Gray, 14],
          (* Se history e' vuota -> messaggio placeholder *)
          Grid[
            Prepend[
              history,
              (* Prepend aggiunge una riga di intestazione in cima *)
              {
                Style["Radice", Bold, White],
                Style["Quoziente", Bold, White]
              }
            ],
            Frame -> All, (* bordi su tutte le celle *)
            FrameStyle -> GrayLevel[0.8],
            Background -> {
              None,
              {
                RGBColor[0.18, 0.32, 0.55], (* intestazione: blu scuro *)
                {RGBColor[0.95, 0.97, 1.0], White} (* righe alternate: azzurro/bianco *)
              }
            },
            ItemSize -> All,
            Alignment -> Center,
            Spacings -> {1, 1}
          ]
          (* Grid costruisce la tabella: ogni elemento di history e' una riga {radice, quoziente} *)
        ]
      ]
   
  }],

  Spacer[30],

  (* RUFFINI  *)
Dynamic[
  If[griglia =!= Null,
    (* Tabella di Ruffini mostrata quando disponibile *)
    Button[
      Mouseover[
        Framed[Style["\|01f4ca Mostra Tabella Ruffini", White, Bold, 13], (*Stato del bottone di default*)
               Background -> RGBColor[0.35, 0.20, 0.65],
               FrameStyle -> Directive[RGBColor[0.20, 0.10, 0.45], Thickness[2]],
               RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {210, 42}],
        Framed[Style["\|01f4ca Mostra Tabella Ruffini", White, Bold, 13], (*Stato del bottone con mouse over*)
               Background -> RGBColor[0.50, 0.30, 0.85],
               FrameStyle -> Directive[RGBColor[0.20, 0.10, 0.45], Thickness[2]],
               RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {210, 42}]
      ],
      DialogInput[ (*Apre il pop up della tabella di Ruffini*)
  Column[{
    Panel[
      Pane[
        griglia, (* griglia e' la tabella costruita da drawRuffiniTable *)
        ImageSize -> {UpTo[800],UpTo[250]},          (* dimensione visibile del popup *)
        Scrollbars -> True,               (* abilita scrollbar *)
        AppearanceElements -> All
      ],
      FrameMargins -> 12,
      Background -> RGBColor[0.96, 0.97, 1.0]
    ],
    DefaultButton["OK", DialogReturn[]] (*Bottone per chiudere il pop up*)
  }, Alignment -> Center],
  WindowTitle -> "Tabella di Ruffini", (*Titolo della finestra del pop up*)
  WindowSize -> All, (*Dimensione della tabella nel pop up*)
  Modal->True (* Non rende possibile operare sull'interfaccia fino alla chiusura del pop up*)
],
      Method -> "Queued", Appearance -> "Frameless"
    ],
    Invisible[Button["placeholder", Null]] (*Caso di tabella non presenta, bottone non mostrato*)
  ]
]

}],
      Background -> RGBColor[0.96, 0.97, 1.0],
      FrameMargins -> 15,
      ImageSize -> {Full, Automatic}, Alignment->Center],
       

(* PANEL INPUT *)
(* Campi di input: in modalita' Ruffini mostra radice+quoziente,
   in modalita' discriminante mostra parte reale e immaginaria *)
Panel[
  Dynamic[
    Column[{
      If[ruffini,
        (* MODALITA' RUFFINI: l'utente inserisce radice intera e quoziente *)
        Row[{
          Column[{
            Style["Inserisci la radice:", Bold, 13, RGBColor[0.18, 0.32, 0.55]],
            InputField[Dynamic[radiceInput], Number,ImageSize -> {120, 32}, BaseStyle -> {FontSize -> 14}]
          }],
          Spacer[25],
          Column[{
            Style["Quoziente:", Bold, 13, RGBColor[0.18, 0.32, 0.55]],
            InputField[Dynamic[polinomioInput], String, ImageSize -> {280, 32}, BaseStyle -> {FontSize -> 14}]
          }],
          Spacer[15],
          (* Piccolo bottone "^" per inserire il simbolo di potenza nel campo quoziente *)
          Column[{
            Style[" ", 13],
            Button[
              Mouseover[
                Framed[Style["^", White, Bold, 16], Background -> RGBColor[0.53, 0.81, 0.92], FrameStyle -> Directive[RGBColor[0.20, 0.45, 0.60], Thickness[2]], RoundingRadius -> 8, FrameMargins -> 6],
                Framed[Style["^", White, Bold, 16], Background -> RGBColor[0.70, 0.90, 1.0], FrameStyle -> Directive[RGBColor[0.20, 0.45, 0.60], Thickness[2]], RoundingRadius -> 8, FrameMargins -> 6]
              ],
              polinomioInput = polinomioInput <> "^",
              (* <> concatena il simbolo "^" alla fine del testo gia' presente nel campo *)
              Appearance -> "Frameless" (* Rimuove i bordi del frame *)
            ]
          }]
        }],
        (* MODALITA' DISCRIMINANTE (ruffini = False):
           il polinomio residuo e' irriducibile, ed ha radici complesse;
           l'utente inserisce la parte reale e immaginaria separatamente *)
        Row[{
          Column[{
            Style["Parte reale della radice", Bold, 13, RGBColor[0.18, 0.32, 0.55]],
            InputField[Dynamic[pReale], Number , ImageSize -> {180, 32}, BaseStyle -> {FontSize -> 14}]
          }],
          Spacer[30],
          Column[{
            Style["Parte immaginaria della radice", Bold, 13, RGBColor[0.18, 0.32, 0.55]],
            InputField[Dynamic[pImmaginaria], Number ,ImageSize -> {180, 32}, BaseStyle -> {FontSize -> 14}]
          }]
        }]
      ]
    }]
  ],
  Background -> RGBColor[0.96, 0.97, 1.0],
  ImageSize -> {Full, Automatic},Alignment->Center
],

(* PANEL FEEDBACK SOTTO *)
(* feedbackMessage e' aggiornato dai bottoni: puo' essere un messaggio di errore,
   conferma, completamento, ecc. Il Dynamic lo aggiorna automaticamente. *)
Panel[
  Dynamic[feedbackMessage],
  Background -> RGBColor[0.96, 0.97, 1.0],
  ImageSize -> {Full, Automatic},
  Alignment->Center
],

      
(* BLOCCO BOTTONI CENTRATI *)
(* I bottoni sono visibili solo se c'e' un esercizio attivo e non e' ancora finito *)
Panel[
Dynamic[
  Pane[
    If[polinomioGenerato =!= Null && ! fineEsercizio,
      Row[{
        (* BOTTONE "Help me" *)
        (* Mostra un suggerimento: le possibili radici intere (teorema delle radici razionali), un avviso che il polinomio richiede il discriminante, oppure suggerisce il raccogliemento totale *)
        Button[
          Mouseover[
            Framed[Style["\|01f4a1 Help me", White, Bold, 13], Background -> RGBColor[0.20, 0.55, 0.75], FrameStyle -> Directive[RGBColor[0.10, 0.35, 0.55], Thickness[2]], RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {160, 42}],
            Framed[Style["\|01f4a1 Help me", White, Bold, 13], Background -> RGBColor[0.30, 0.70, 0.90], FrameStyle -> Directive[RGBColor[0.10, 0.35, 0.55], Thickness[2]], RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {160, 42}]
          ],
          If[ruffini,
            (* In modalita' Ruffini: calcola e mostra le radici candidate *)
            (showPossibleRoots = possibleRoots[polinomioGenerato, var];
            (* Radice = 0, suggerisce il raccoglimento totale*)
             If[showPossibleRoots == {0},
               showPossibleRoots = "Prova ad applicare il raccoglimento totale! \|01faf4",
               showPossibleRoots = ToString[showPossibleRoots]]),
            (* In modalita' discriminante: ricorda all'utente il metodo da usare *)
            showPossibleRoots = "Ricordati che potresti applicare il discriminante! \|01f9e0"
          ],
          Method -> "Preemptive", Appearance -> "Frameless"
        ],
        
        Spacer[20],
        
        (* BOTTONE "Verifica Esercizio" *)
        (* Controlla se la risposta dell'utente e' corretta e aggiorna
           la tabella di Ruffini e lo storico di conseguenza *)
        Button[
          Mouseover[
            Framed[Style["\:2705 Verifica Esercizio", White, Bold, 13], Background -> RGBColor[0.16, 0.55, 0.30], FrameStyle -> Directive[RGBColor[0.10, 0.40, 0.20], Thickness[2]], RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {180, 42}],
            Framed[Style["\:2705 Verifica Esercizio", White, Bold, 13], Background -> RGBColor[0.22, 0.70, 0.40], FrameStyle -> Directive[RGBColor[0.10, 0.40, 0.20], Thickness[2]], RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {180, 42}]
          ],
          If[! ruffini,
            (* VERIFICA in modalita' DISCRIMINANTE *)
            (griglia = Null; (* la tabella Ruffini non serve in questa modalita' *)
             Which[
               (* Campi vuoti *)
               pReale === Null || pImmaginaria === Null, feedbackMessage = Style["Inserisci prima tutti i dati! \|01f937\:200d\[Mars]\:fe0f", Red, 16, Bold],
               (* I valori inseriti non sono interi -> errore di formato *)
               !IntegerQ[pReale] || !IntegerQ[pImmaginaria], feedbackMessage = Style["Attenzione! La parte reale e immaginaria sono entrambe intere \|01f926\:200d\[Mars]\:fe0f" , Red, 16, Bold],
               (* Valori errati rispetto alla soluzione *)
               pReale =!= a || pImmaginaria =!= b, feedbackMessage = Style["Sbagliato riprova! \|01f62d", Red, 16, Bold],
               (* Risposta corretta -> aggiorna fattori, storico e chiude l'esercizio *)
               True, ({listaFattori, history, showPossibleRoots, pReale, pImmaginaria} = nextStepComplessi[listaFattori, var, a, b, history, polinomioGenerato]; fineEsercizio = True; feedbackMessage = Style["Esercizio Completato! Genera un nuovo esercizio \|01f973", Green, 16, Bold])
             ]),
            (* VERIFICA in modalita' RUFFINI *)
            result = verifyAnswer[polinomioGenerato, radiceInput, polinomioInput, var];
            (* verifyAnswer restituisce {codiceEsito, messaggioFeedback} *)
            feedbackMessage = result[[2]]; (* mostra subito il feedback *)
            Which[
              result[[1]] === "correct",
              (* La radice e il quoziente sono entrambi corretti *)
              (griglia = drawRuffiniTable[polinomioGenerato, radiceInput, var];
               (* Disegna la tabella di Ruffini per questa radice *)
               {history, listaFattori, polinomioGenerato, polinomioInput, radiceInput, showPossibleRoots} = nextStep[history, radiceInput, var, listaFattori, polinomioGenerato];
               (* Aggiorna storico, lista fattori e polinomio residuo per il passo successivo *)
               If[Length[CoefficientList[polinomioGenerato, var]] <= 2,
                 (* Il polinomio residuo e' di grado <= 1 -> si puo' trovare l'ultima radice automaticamente *)
                 rootFound = findCorrectRoot[polinomioGenerato, var];
                 {history, listaFattori, polinomioGenerato, polinomioInput, radiceInput, showPossibleRoots} = nextStep[history, rootFound, var, listaFattori, polinomioGenerato];
                 feedbackMessage = Style["Esercizio Completato! Genera un nuovo esercizio \|01f973", Green, 16, Bold];
                 fineEsercizio = True,
                 feedbackMessage = result[[2]] 
               ]),
              result[[1]] === "wrongRoot", 
              (* La radice non e' una radice del polinomio -> mostra comunque la tabella
                 di Ruffini con il resto non zero, cosi' l'utente vede l'errore *)
              griglia = drawRuffiniTable[polinomioGenerato, radiceInput, var],
              True, 
              (* Altra tipologia di errore (es. formato quoziente errato) -> nascondi la griglia *)
              griglia = Null
            ]
          ],
          Method -> "Preemptive", Appearance -> "Frameless"
        ],
        
        Spacer[20],
        
        (* BOTTONE "Mostra Soluzione" *)
        (* Rivela il prossimo passo corretto, aggiornando la tabella e lo storico automaticamente *)
        Button[
          Mouseover[
            Framed[Style["\|01f3f3\:fe0f Mostra Soluzione", White, Bold, 13], Background -> RGBColor[0.20, 0.30, 0.65], FrameStyle -> Directive[RGBColor[0.10, 0.20, 0.45], Thickness[2]], RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {180, 42}],
            Framed[Style["\|01f3f3\:fe0f Mostra Soluzione", White, Bold, 13], Background -> RGBColor[0.30, 0.45, 0.85], FrameStyle -> Directive[RGBColor[0.10, 0.20, 0.45], Thickness[2]], RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {180, 42}]
          ],
          If[! ruffini,
            (* Modalita' discriminante: rivela la soluzione complessa e chiude l'esercizio *)
            {listaFattori, history, showPossibleRoots, pReale, pImmaginaria} = nextStepComplessi[listaFattori, var, a, b, history, polinomioGenerato];
            (griglia = Null; fineEsercizio = True; feedbackMessage = Style["Soluzione mostrata \[LongDash] Esercizio Concluso - riprova da solo la prossima volta! \|01f925", Blue, 16, Bold]),
            (* Modalita' Ruffini: trova la radice corretta e mostra il passaggio *)
            rootFound = findCorrectRoot[polinomioGenerato, var];
            Which[
              (* Il polinomio residuo non ha piu' radici intere -> va in modalita' discriminante *)
              rootFound === Null, (feedbackMessage = Style["Soluzione mostrata : Il polinomio residuo \[EGrave] irriducibile , continua senza ruffini \|01f925", Blue, 16, Bold]; showPossibleRoots = ""; ruffini = False; griglia = Null),
              (* Trovata la radice: mostra la tabella, aggiorna tutto *)
              True,
              griglia = drawRuffiniTable[polinomioGenerato, rootFound, var];
              {history, listaFattori, polinomioGenerato, polinomioInput, radiceInput, showPossibleRoots} = nextStep[history, rootFound, var, listaFattori, polinomioGenerato];
              If[Length[CoefficientList[polinomioGenerato, var]] <= 2,
                (* Polinomio residuo di grado <= 1: completa automaticamente *)
                feedbackMessage = Style["Soluzione mostrata \[LongDash] Esercizio Concluso - riprova da solo la prossima volta! \|01f925", Blue, 16, Bold];
                rootFound = findCorrectRoot[polinomioGenerato, var];
                {history, listaFattori, polinomioGenerato, polinomioInput, radiceInput, showPossibleRoots} = nextStep[history, rootFound, var, listaFattori, polinomioGenerato];
                fineEsercizio = True,
                feedbackMessage = Style["Soluzione mostrata \[LongDash] riprova da solo la prossima volta! \|01f925", Blue, 16, Bold]
              ]
            ]
          ],
          Method -> "Preemptive", Appearance -> "Frameless"
        ],
        
        Spacer[20],
        
        (* BOTTONE "Irriducibile" *)
        (* Visibile solo in modalita' Ruffini. L'utente dichiara che il polinomio
           residuo non ha piu' radici reali. Il sistema verifica se e' corretto. *)
        If[ruffini,
          Button[
            Mouseover[
              Framed[Style["\|01f90f Irriducibile", White, Bold, 13], Background -> RGBColor[0.70, 0.20, 0.40], FrameStyle -> Directive[RGBColor[0.45, 0.10, 0.25], Thickness[2]], RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {170, 42}],
              Framed[Style["\|01f90f Irriducibile", White, Bold, 13], Background -> RGBColor[0.85, 0.30, 0.55], FrameStyle -> Directive[RGBColor[0.45, 0.10, 0.25], Thickness[2]], RoundingRadius -> 10, FrameMargins -> 8, ImageSize -> {170, 42}]
            ],
            rootFound = findCorrectRoot[polinomioGenerato, var];
            griglia = Null;
            If[rootFound === Null,
              (* Corretto: il polinomio non ha radici intere -> passa alla modalita' discriminante *)
              (feedbackMessage = Style["Indovinato \[Checkmark] : Il polinomio residuo \[EGrave] irriducibile , continua senza ruffini \|01f609", Green, 16, Bold]; ruffini = False; showPossibleRoots = ""),
              (* Sbagliato: esistono ancora radici intere -> l'utente deve continuare con Ruffini *)
              feedbackMessage = Style["Il polinomio ha ancora radici intere, continua a fattorizzare con Ruffini \|01f627", Red, 16, Bold]
            ],
            Method -> "Preemptive", Appearance -> "Frameless"
          ],
          (* Se siamo in modalita' discriminante (ruffini = False):
             al posto del bottone Irriducibile si mette un'area invisibile
             della stessa dimensione, per non spostare il layout *)
          Invisible[Button["Non ruffini", Null, ImageSize -> {170, 42}]]
        ]
      }],
      Null (* Se non \[EGrave] attivo l'esercizio, non mostra nessun bottone *)
    ],
    ImageSize -> {Full, Automatic}, (* Forza il contenitore a espandersi in larghezza *)
    Alignment -> Center           (* Centra orizzontalmente la Row al suo interno *)
  ]
],
      Background -> RGBColor[0.96, 0.97, 1.0],
      FrameMargins -> 15,
      ImageSize -> {Full, Automatic}],
      
      
(* Testo per visualizzare il suggerimento *)
(* showPossibleRoots contiene il testo del suggerimento generato dal bottone "Help me" *)
Panel[
  Dynamic[Style[showPossibleRoots, Bold, 16, White]],
  Background -> RGBColor[0.18, 0.32, 0.55], (* blu scuro, come l'header *)
  ImageSize -> {Full, Automatic},
  FrameMargins -> 15
]
    
  }, Alignment -> Left, Frame -> True,
     FrameStyle -> Directive[RGBColor[0.18, 0.32, 0.55], Thickness[1.5]],
     Background -> RGBColor[0.96, 0.97, 1.0]
     ],ImageSize->{1040,720}]
]];

End[]
EndPackage[]
