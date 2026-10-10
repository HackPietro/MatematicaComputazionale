# Ruffini: fattorizzazione di polinomi in Mathematica

Progetto universitario per l'esame del corso **Matematica Computazionale**, Laurea Magistrale in Informatica, **Università di Bologna**.

`RuffiniPackage.m` è un pacchetto Wolfram Mathematica che, richiamando una sola funzione, lancia un'interfaccia interattiva per **esercitarsi sulla fattorizzazione dei polinomi**. Gli esercizi si risolvono con il **teorema di Ruffini** e, quando il polinomio residuo non ha più radici intere, con il **discriminante**.

> Scopo didattico: il programma genera esercizi, verifica le risposte, dà suggerimenti e può mostrare la soluzione passo per passo.

---

## Autori

- Stefano Mercurio
- Pietro Cimino
- Mattia Boffa
- Giovanni Previtera

---

## Funzionalità

- **Generazione di esercizi riproducibili**: ogni esercizio è identificato da un numero (seed). Lo stesso seed produce sempre lo stesso polinomio.
- **Scelta del grado** del polinomio: 2, 3 o 4.
- **Polinomi monici** con radici intere comprese tra -6 e 6 e, con probabilità del 25%, una coppia di radici complesse coniugate.
- **Variabile casuale** del polinomio, scelta tra `x`, `y`, `t`, `z`, `s`, `w`.
- **Verifica della risposta** con feedback differenziato: dati mancanti, formato errato, radice sbagliata, radice corretta ma quoziente errato, risposta corretta.
- **Tabella di Ruffini** mostrata in una finestra a parte, con il resto evidenziato.
- **Storico dei passaggi** con radice e quoziente di ogni step.
- **Help me**: suggerisce le possibili radici intere (divisori del termine noto), consiglia il raccoglimento totale se il termine noto è 0, oppure ricorda il discriminante.
- **Irriducibile**: l'utente dichiara che il residuo non ha radici intere e il sistema lo verifica. Se è corretto, si passa alla modalità discriminante.
- **Mostra soluzione**: svolge automaticamente il prossimo passo.
- **Modalità discriminante**: per il fattore di secondo grado residuo, l'utente inserisce parte reale e parte immaginaria della radice complessa.
- **Pulisci campi** per svuotare gli input senza rigenerare l'esercizio.

---

## Requisiti

- **Wolfram Mathematica 14** (versione per cui il pacchetto è stato sviluppato)

---

## Come si usa

1. Scarica il repository e metti `RuffiniPackage.m` in una cartella a tua scelta.
2. In un notebook di Mathematica carica il pacchetto indicando il percorso del file:

   ```mathematica
   Get["C:\\percorso\\della\\cartella\\RuffiniPackage.m"]
   ```

   In alternativa, imposta la cartella del repository come directory del notebook e usa:

   ```mathematica
   SetDirectory[NotebookDirectory[]];
   Get["RuffiniPackage.m"]
   ```

3. Lancia l'interfaccia:

   ```mathematica
   Ruffini`generateInterface[]
   ```

Per una guida passo passo apri il notebook **`Tutorial.nb`**.

### Svolgere un esercizio

1. Inserisci il **numero esercizio** (intero ≥ 0) e scegli il **grado**, poi premi **Genera Esercizio**.
2. Se il polinomio ha una radice intera, inserisci la **radice** e il **quoziente** della divisione (usa il pulsante `^` per scrivere le potenze, ad esempio `x^2 + 3x - 4`).
3. Premi **Verifica Esercizio**. Se è corretto, il passaggio entra nello storico e puoi continuare con il polinomio residuo.
4. Quando non restano radici intere, premi **Irriducibile** e inserisci parte reale e immaginaria della radice complessa.
5. A fine esercizio il polinomio di partenza viene mostrato come prodotto dei fattori trovati.

---

## Contenuto del repository

| File | Descrizione |
|---|---|
| `RuffiniPackage.m` | Pacchetto con la logica e l'interfaccia |
| `Tutorial.nb` | Notebook di esempio e guida all'uso |
| `Ruffini.mp4` | Video dimostrativo dell'interfaccia |
| `Start.png`, `Beginning.jpg`, `Middle.jpg`, `End.jpg`, `Irriducibile.jpg`, `RuffiniTable.jpg` | Schermate dell'interfaccia nelle varie fasi |
| `Stefano.png`, `Pietro.png`, `Mattia.png`, `Giovanni.png` | Immagini degli autori |

---

## Struttura del codice

Tutte le funzioni, tranne `generateInterface[]`, sono definite nel contesto privato `Ruffini`Private``.

| Funzione | Ruolo |
|---|---|
| `generateInterface[]` | Unica funzione pubblica: costruisce l'interfaccia con `DynamicModule` e `Deploy` |
| `generatePolynom[g, var]` | Genera il polinomio di grado `g`, restituendo anche parte reale e immaginaria dell'eventuale coppia complessa |
| `possibleRoots[poly, var]` | Calcola i candidati a radice (divisori del termine noto) e restituisce fino a 6 valori mescolati |
| `findCorrectRoot[poly, var]` | Trova la prima radice intera corretta, oppure `Null` |
| `verifyAnswer[...]` | Valida l'input dell'utente e restituisce codice di esito e messaggio |
| `drawRuffiniTable[poly, r, var]` | Costruisce la tabella di Ruffini come `Grid` |
| `nextStep[...]` | Aggiorna storico, fattori e polinomio residuo dopo uno step con radice intera |
| `nextStepComplessi[...]` | Aggiorna storico e fattori per la coppia di radici complesse |

---

## Informazioni sul pacchetto

- **Versione**: 0.9
- **Mathematica**: 14
- **Ultima modifica**: 14/05/2026
- **Limitazioni**: uso didattico
