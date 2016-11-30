#load "testingCode.fsx"

open mastermind

// Dette er black box testing af programmet

// Funktioner der bliver testet:
//    1 charToColor
//    2 inputCodeToCode
//    3 randomCode
//    4 userCode
//    5 makeCode
//    6 makeHisto
//    7 validate
//    8 playersInputCorrect
//    9 playersInputCorrectStartScreen
//   10 guess
//   11 gamePlay
//   12 game

printfn "Black box testing af mastermind.fsx"

// Test cases for charToColor
let testCases1 =
        [("R", Red);
         ("G", Green);
         ("y", Yellow)]

printfn "  1. charToColor function"
for i = 0 to testCases1.Length - 1 do
    printfn "    test %d - %b" (i+1) ((charToColor (fst testCases1.[i])) = 
                                                        (snd testCases1.[i]))

printf "\n\n"
(******************************************************************************)

// Test cases for inputCodeToCode
let testCases2 =
    [(["R"; "B"; "Y"; "G"],[Red;Black;Yellow;Green]);
     (["y";"p";"W";"w"],[Yellow; Purple; White; White]);
     (["w"; "w"; "b"; "B"],[White; White; Black; Black])]

printfn "  2. inputCodeToCode funktion"
for i = 0 to testCases2.Length - 1 do
    printfn "    test %d - %b" (i+1) ((inputCodeToCode (fst testCases2.[i])) =
                                                        (snd testCases2.[i]))

printf "\n\n"
(******************************************************************************)

// Test cases for randomCode
// Ja, det bliver nok lidt svært at forudsige...
printfn "  3. randomCode funktion"
printfn "    Kan ikke testes, da man ikke kan forudsige hvilken kode"
printfn "    randomCode vil returnere."

printf "\n\n"
(******************************************************************************)

// Test cases for userCode
let testCases4 =
    [(["R"; "G"; "B"; "w"],true);
     (["w"; "P"; "w"; "P"],true);
     (["J"; "r"; "W"; "B"],false);
     (["R"; "R"; "g"; "U"],false)]

printfn "  4. userCode funktion"
for i = 0 to testCases4.Length - 1 do
    printfn "    test %d - %b" (i+1) ((userCode (fst testCases4.[i])) =
                                                    (snd testCases4.[i]))

printf "\n\n"
(******************************************************************************)

// Test cases for makeCode
// Denne funktion tager imod brugerinput i funktionen. Derfor kan man ikke
// teste den.
printfn "  5. makeCode funktion"
printfn "    Kan ikke testes da den skal have bruger input"


printf "\n\n"
(******************************************************************************)

// Test cases for makeHisto
let testCases6 =
        [([Red; Red; Green; Green],[2; 2; 0; 0; 0; 0]);
         ([White; Black; Purple; Green],[0; 1; 0; 1; 1; 1]);
         ([White; White; White; White],[0; 0; 0; 0; 4; 0]);
         ([Yellow; Green; Green; Black],[0; 2; 1; 0; 0; 1])]

printfn "  6. makeHisto funktion"
for i = 0 to testCases6.Length - 1 do
    printfn "    test %d - %b" (i+1) ((makeHisto (fst testCases6.[i])) = 
                                                    (snd testCases6.[i]))

printf "\n\n"
(******************************************************************************)

// Test cases for validate
let testCases7 =
        [(([Red; Red; Green; Green], [Red; Red; Green; Green]),(0,4));
         (([Yellow; Purple; Yellow; White], [Black; Black; Purple; White]), (1,1));
         (([Yellow; Purple; White; Black],[Red; Red; Green; Green]),(0,0));
         (([Green; Yellow; Purple; White],[Yellow; Purple; White; Green]),(4,0))]

printfn "  7. validate funktion"
for i = 0 to testCases7.Length - 1 do
    printfn "    test %d - %b" (i+1)
            ((validate (fst (fst testCases7.[i])) (snd (fst testCases7.[i]))) =
                    (snd testCases7.[i]))


printf "\n\n"
(******************************************************************************)

// Test cases for playersInputCorrect
let testCases8 =
        [("1",true);
         ("2",true);
         ("3",true);
         ("4",true);
         ("9",false)]

printfn "  8. playersInputCorrect funktion"
for i = 0 to testCases8.Length - 1 do
    printfn "    test %d - %b" (i+1)
            ((playersInputCorrect (fst testCases8.[i])) = (snd testCases8.[i]))


printf "\n\n"
(******************************************************************************)

// Test cases for playersInputCorrectStartScreen
let testCases9 =
        [("help",true);
         ("Help",true);
         ("start",true);
         ("Start",true);
         ("foobar",false)]

printfn "  9. playersInputCorrectStartScreen funktion"
for i = 0 to testCases8.Length - 1 do
        printfn "    test %d - %b" (i+1)
                ((playersInputCorrectStartScreen (fst testCases9.[i])) =
                        (snd testCases9.[i]))


printf "\n\n"
(******************************************************************************)

// Funktionen guess bliver kun brugt når brugeren skal gætte.
// Inde i funktionen bliver der spurgt om input fra brugeren, så det er ikke
// muligt at blackbox teste guess funktionen.

printfn "  10. guess funktion"
printfn "    Inde i funktionen bliver der spurgt om input fra brugeren, så det er"
printfn "    ikke muligt at blackbox teste guess funktionen."


printf "\n\n"
(******************************************************************************)

// Funktionen gamePlay tager bruger input og kan derfor ikke blive testet med
// black box testing.

printfn "  11. gamePlay funktion"
printfn "    Funktionen gamePlay tager bruger input og kan derfor ikke blive"
printfn "    testet med black box testing."


printf "\n\n"
(******************************************************************************)

// Funktionen gamePlay tager bruger input og kan derfor ikke blive testet med
// black box testing.

printfn "  12. game funktion"
printfn "    Funktionen game tager bruger input og kan derfor ikke blive testet"
printfn "    med black box testing."
