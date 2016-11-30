// POP opgave 8 - Mastermind i F#
// Skrevet af Asibi Ayagiba, Mads Carstensen og Andreas Drivsholm

// De praefinerede typer vi skal bruge i vores kode.
type codeColor = 
    Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

/// <summary>Denne funktion soerger for at faa konverteret et bogstav om til
/// den passende farve.</summary>
/// <remarks>Wildcardet bliver aldrig matchet, da denne funktion altid
/// bliver kaldt med enten "R", "r", "G", etc, pga hjaelpefunktionen
/// legitColors soerger for at input altid bliver tjekket inden det bliver
/// kaldt som argument til charToColor</remarks>
/// <params>charToColor tager et parameter med navnet color som er af typen
/// string</params>
/// <returns>charToColor returnerer en farve af typen codeColor</returns>
let charToColor (color : string)  =
    match color with
    | "R" | "r" -> Red
    | "G" | "g" -> Green
    | "Y" | "y" -> Yellow
    | "P" | "p" -> Purple
    | "W" | "w" -> White
    | "B" | "b" -> Black
    | _ -> failwith "Kommer ikke til at ske."

/// <summary>inputCode oversaetter en liste af bogstaver som definerer farver
/// til en liste af egentlige farver, altsaa er typen codeColor</summary>
/// <params>Tager et input koden som paramterer</params>
/// <returns>Returnerer en liste af typen code som er defineret som vaerende
/// en liste med elementer af typen codeColor.</returns>
let rec inputCodeToCode inputCode : code =
    match inputCode with
    | [] -> []
    | x :: xs -> charToColor x :: inputCodeToCode xs


/// <summary>Genererer en tilfaeldig kode. Bliver brugt naar computer fungerer
/// som opgavestilleren.</summary>
/// <remarks>Opretter et nyt object rand, som er defineret ved klassen Random.
/// Vi bruger klassens metode Next() til at generere tal mellem 1 og 6, med
/// begge tal inklusive. Vi skriver Next(1,7), men her er syvtallet eksklusivt.
/// </remarks>
/// <params>Denne funktion tager inten parametre</params>
/// <returns>Returnerer en en "kode". Det er en liste af 4 elementer, hvor
/// hvert element er af typen codeColor. Denne liste af farver er tilfaeldige
/// </returns>
let randomCode () : code = 
    let rand = new System.Random() 

    let rec randomCodeHelper (random : System.Random) acc =
        let random_number = random.Next(1,7)

        match acc with
        | 1 | 2 | 3 | 4 ->
            match random_number with
            | 1 -> Red    :: (randomCodeHelper random (acc-1)) 
            | 2 -> Green  :: (randomCodeHelper random (acc-1)) 
            | 3 -> Yellow :: (randomCodeHelper random (acc-1)) 
            | 4 -> Purple :: (randomCodeHelper random (acc-1)) 
            | 5 -> White  :: (randomCodeHelper random (acc-1)) 
            | 6 -> Black  :: (randomCodeHelper random (acc-1)) 
            | _ -> failwith "Kommer ikke til at ske."
        | 0 ->
            []
        | _ -> failwith "Kommer ikke til at ske."

    (randomCodeHelper rand 4)


/// <summary>Denne funktion tjekker om brugerens input er korrekt.</summary>
/// <params>Den tager et parameter af typen code</params>
/// <returns>Returnerer et bools true eller false, alt efter om brugerens
/// input er hhv. korrekt eller ikke korrekt.</returns>
let userCode (code : string list) =
    let legitColor col =
        match col with
        | "R" | "G" | "Y" | "P" | "W" | "B" -> true
        | "r" | "g" | "y" | "p" | "w" | "b" -> true
        | _ -> false

    let mutable returnValue = true

    if code.Length <> 4 then
        false
    else
        for i = 0 to code.Length - 1 do
            if (legitColor code.[i]) = false then
                returnValue <- false

        returnValue
    
/// <summary>makeCode genererer en kode af typen code.</summary>
/// <remarks>Funktionen har to branches, alt afhaengig om det er computeren
/// der fungerer som "codemaker" eller om det er en bruger.</remarks>
/// <params>Tager som parameter en spiller type som kan vaere enten Human eller
/// Computer</params>
/// <returns>Returnerer en kode af typen code</summary>
let makeCode (player : player) : code =
    match player with
    // Hvis "codemaker" er Computer, bliver funktionen randomCode kaldt.
    // randomCode returnerer en kode af typen code.
    | Computer ->
        randomCode ()
    // Hvis "codemaker" er Human, bliver denne branch koert.
    | Human -> 
        let mutable inputCode = [] // variabel til at indeholde brugerens input
        printfn "\nPlease create a secret code."
        printfn "You can choose from the following colors:"
        printfn "Red, Green, Yellow, Purple, White and Black"
        printfn "Please enter four colors with a space between each," 
        printfn "and each denoted by the first character in the name of the color"
        printfn "Example: B or b = Black"
        printfn "Example: r g y b"
        printf "\n> "
        
        // inputCode saettes til at vaere lige med brugerens input.
        // Brugerens input bestaar af 4 tegn separeret af mellemrum. Disse tegn
        // bliver udvindet af tekststrengen ved hjaelpe af metoden Split, som
        // foraarsager at inputet bliver omdannet til en liste af tegn.
        // Denne liste gemmes i den muterbare liste inputCode.
        inputCode <- Array.toList ((System.Console.ReadLine ()).Split(' '))
        
        // Funktionen userCode tjekker om brugeren input er korrekt.
        // Hvis inputtet ikke er korrekt, spoerges der om nyt input.
        while (userCode inputCode) = false do
            printfn "\nYou have to choose a valid color"
            printf  "\n> "

            inputCode <- Array.toList ((System.Console.ReadLine ()).Split(' '))
        
        // Naar inputtet er korrekt, kaldes funktionen inputCodeToCode med
        // inputCode som argument. Dette konverterer koden fra at vaere en liste
        // af tegn, til at vaere en liste af typen code.
        let code = inputCodeToCode inputCode
        code

/// <summary>makeHisto laver et histo gram over en liste af farver. Lad
/// i = 1,2,3...6. Histo.[i] indeholde en heltal for hvor ofte farve i
/// forekommer. Talvaerdien for farven og selv farven, hoerer sammen som
/// de blev defineret oeverst i programkoden. Saa 0 = Red, 1 = Green, etc.
/// </summary>
/// <params>makeHisto tager et parameter af typen code</params>
/// <returns>makeHisto returnerer et histogram af farverne for den paagaeldende
/// liste</returns>
let makeHisto (code : code) =
    let histo = (Array.init 6 (fun _ -> 0))

    for i = 0 to code.Length - 1 do
        if code.[i] = Red then
            histo.[0] <- histo.[0] + 1
        elif code.[i] = Green then
            histo.[1] <- histo.[1] + 1
        elif code.[i] = Yellow then
            histo.[2] <- histo.[2] + 1 
        elif code.[i] = Purple then
            histo.[3] <- histo.[3] + 1
        elif code.[i] = White then
            histo.[4] <- histo.[4] + 1
        else
            histo.[5] <- histo.[5] + 1

    (Array.toList histo)

let mutable guessCount = 0
let mutable blacksValidate = 0

/// <summary>validate validerer om hvor taet computerens eller brugerens gaet er
/// paa at vaere det rigtige - i form af hvide og sorte pinde.</summary>
/// <params>Tager to parametre. hidden er en den skjulte kode, og guess er den
/// kode som computer eller brugeren har gaettet</params>
/// <returns>Returnerer en tuple af typen answer som er en tuple af typen
/// (int * int). Det foerste element i tuplen er antallet af hvide pinde, og det
/// andet element i tuplen er antallet af sorte pinde.</returns>
let validate (hidden : code) (guess : code) : answer =
    // Dette er de to variable som lagrer histogrammerne for den skjulte kode
    // og brugerens/computerens gaet.
    let histoHidden = makeHisto hidden
    let histoGuess = makeHisto guess
    
    // Her sammenligner men de to histogrammer. Summen af pinde (hvide + sorte
    // pinde) regne ved at finde min(hiddenHisto[i], guessHisto[i]) for 
    // i = 0,1,2,..5
    let rec minHisto hiddenHisto guessHisto =
        match (hiddenHisto, guessHisto) with
        | (x :: xs, y :: ys) -> (min x y) :: minHisto xs ys
        | ([], _) -> []
        | _ -> failwith "Kommer ikke til at ske"
    
    // sumPegs indeholder summen af hvide og sorte pinde
    let sumPegs = List.sum (minHisto histoHidden histoGuess)
    
    // Antallet af sorte pinde findes med blackFun funktionen.
    // For i = 0,1,2,3, hvis hidden[i] = guess[i] er den i'te pind den rigtige
    // farve paa det rigtige sted.
    // funktionen returnerer et heltal mellem 0 og 4, begge inklusive.
    let rec blackFun hidden guess =
        match (hidden, guess) with
        | (x :: xs, y :: ys) ->
                            // Hvis hidden[i] og guess[i] er samme farve,
                            // skal der returneres en mere sort pind.
                            if x = y then
                                1 + blackFun xs ys
                            else
                                0 + blackFun xs ys
        | ([], _) -> 0
        | _ -> failwith "hej"

    // blackPegs betegner antallet af sorte pinde.
    let blackPegs = blackFun hidden guess

    // Antallet af hvide pinde kan regnes som summen af sorte og hvide pinde
    // minus antallet af sorte pinde.
    let whitePegs = sumPegs - blackPegs

    // Holder styr paa hvor mange gaet man har brugt.
    guessCount <- guessCount + 1


    if blacksValidate = 0 then
        printfn "%d:  %A" guessCount guess
    else ()
    
    // Returnerer en tuple med antallet af hvide- og sorte pinde.
    (whitePegs, blackPegs)

// Tjekker om brugeren har valgt det rigtige input naar han/hun skal vaelge
// game mode.
// Brugeren bliver bedt om at vaelge et af de fire game modes. Hvis brugeren
// indtaster noget andet, vil han/hun faa at vide han/hun skal vaelge et tal
// mellem 1 og 4
let playersInputCorrect number =
    match number with
    | "1" | "2" | "3" | "4" -> true
    | _ -> false

// Tjekker om brugeren vaelger et gyldigt valg paa den foerste velkomstskaermen.
let playersInputCorrectStartScreen input =
    match input with
    | "help" | "Help" | "start" | "Start" ->
        true
    | _ ->
        false

/// <summary>Funktionen guess tager mod et brugergaet og returnerer en
/// kode</summary>
/// <remarks>Funktionen har et parameter af typen player, saa den baade
/// kan tage gaet fra en bruger og et computer. Dette stod der i opgaven
/// at den skulle. Men gaettet fra computeren bliver lavet et andet sted
/// i koden, og derfor matcher player altid med Human og aldrig med
/// Computer. Vi satte bare Computer branchen til at returnerer en liste
/// med farven Red, da guess skal returnere en variable af typen code
/// </remarks>
/// <params>guess har to parametere. Player er af typen player, og bliver brugt
/// til at finde ud af om gaettet kommer fra en computer eller en bruger. board
/// er af typen board, og tager variable af typen board. Board er en liste med
/// elementer som har typen (code * answer).</params>
/// <returns>guess returnerer en kode hvis brugerinputtet er korrekt</returns>
let guess (player : player) (board : board) : code =
    match player with
    | Human ->
        let mutable userInput = []
        
        // Printer spillebraettet indeholdende tidliger gaet og dets svarpinde.
        printfn "\nYour board so far:"
        
        for i = 0 to board.Length-1 do
            printfn "%A" (board.[i])
        
        printf "\n> "
        
        userInput <- Array.toList (System.Console.ReadLine().Split(' '))
        
        // validerer bruger input
        while (userCode userInput) = false do
            printfn "\nInvalid input\n"
            printf  "> "
            userInput <- Array.toList (System.Console.ReadLine().Split(' '))
        
        inputCodeToCode userInput
    | Computer ->
        [Red]

/// <summary>Funktioen gamePlay koerer indtil brugeren har gaettet rigtigt
/// </summary>
/// <remarks>Funktionen koerer kun naar det er brugeren der fungerer som
/// kodeloeser</remarks>
/// <params>Tager som parameter hiddenCode af typen code. Den bruger dette
/// parameter til at sammeligne de gaet der bliver lavet med den hemmelige
/// kode, saa den ved hvornaar den skal terminere</params>
/// <returns>Returnerer et heltal med hvor mange forsoeg brugeren har brugt paa
/// at gaette koden.</returns>
let gamePlay hiddenCode =
    let mutable board : board = []
    let mutable humanGuess : code = [Red]
    let mutable validation = (0,0)
    let mutable tries : int = 0

    // Det er foerst naar validate returnerer (0,4) (Naar der er 4 sorte pinde)
    // at loekken terminerer, og brugeren har vundet spillet.
    while snd (validation) <> 4 do
        System.Console.Clear()

        // Printer hvor mange gaet man har brugt indtil videre.
        printfn "You have used %d guesses so far." tries
        
        humanGuess <- guess Human board
        
        validation <- (validate hiddenCode humanGuess)
        
        // Spillebraettet bliver opdateret med det nye gaet
        board <- board @ [(humanGuess, validation)]

        tries <- tries + 1
    tries

/// <summary>game funktionen er "main" funktionen i vores program. Den fungerer
/// ved at den kalder sig selv naar der skal startes et nyt spil - derfor er den
/// rekursivt defineret.</summary>
/// <param name="choice">Choice fortaeller hvilket
/// game mode der skal spilles. choice kan antage foelgende vaerdier:
///   0: Startskaerm
///   1: Human vs. Computer, hvor computer som "codemaker"
///   2: Human vs. Human
///   3: Computer vs. Human, hvor brugeren fungerer som "codemaker"
///   4: Computer vs Computer
/// </param>
let rec game choice = 

    /// <summary>Win funktionen bliver kaldt naar enten brugeren eller computer
    /// har gaettet rigtigt, og derfor vundet spillet.</summary>
    /// <param name="player">Parameteret player bruges til at afgoere om det er en bruger eller
    /// computeren som har vundet.</param>
    /// <param name="gameModeChoice">
    /// Parameteret gameModeChoice bruges til at finde ud af hvilket gamemode
    /// brugeren/computer spillede da han/hun/den vandt. Dette bliver brugt
    /// naar man skal spille igen, for saa kalder man game med dette parameter,
    /// hvis man oensker at spille samme game mode igen.
    /// </param>
    /// <param name="guessC">guessC er hvor mange gaet computeren har brugt</param>
    /// <param name="tries">tries er hvor mange gaet brugeren har brugt.</param>
    /// <returns>Returneringsvaedien afhaenger af hvad brugeren taster ind
    /// efter overstaaet spil</returns>
    let rec win player gameModeChoice guessCounter tries = 
        match player with
        | "Human" -> 
            System.Console.Clear()
            printfn "You win!"
            printfn "It took you %d guesses!" tries

        | _ -> 
            printfn "\nThe computer, Allan, wins!"
            printfn "It took it %d guesses!" guessCounter
            
        printfn "\nWould you like to:"
        printfn "1. Go to the startmenu?"
        printfn "2. Play the same gamemode again?"
        printfn "3. Quit the game?"
        printf "> "
        let userInput = System.Console.ReadLine ()
   
        match userInput with
        | "1" -> game "0" // Her ledes brugeren tilbage til startskaermen
        | "2" -> game gameModeChoice // Her spilles det samme game mode igen
        | "3" -> System.Environment.Exit 0 // Her afsluttes spillet

        // Her kalder funktionen sig selv, med de nuvaerende parametre som
        // argumenter. Den goer dette i stedet for en fejlmeddelelse, og
        // spoerger saa brugeren igen om hvad han/hun vil.
        | _ -> win player gameModeChoice guessCounter tries

    /// <summary>Funktionen compGuess bliver kaldt naar det er computeren der
    /// skal loese en kode</summary>
    /// <param name="hiddenCode">Opgaven der skal loeses</param>
    /// <param name="gameChoice">Hvilken gamemode der bliver kaldt fra. Bruges til at spille samme type
    /// igen fra winskaerm</param>
    /// <remarks>Funktionen koeres kun naar computeren skal gaette</remarks>
    let compGuess hiddenCode gameChoice =
        //Variable der bruges under funktionen
        let mutable guessCode : code = []
        let mutable validation = (0,0)
        let mutable whitePegs : int = 0
        let mutable blackPegs : int = 0
        //Laver en variabel med 6 elementer, der alle er 0'er.
        let temp = (Array.init 6 (fun _ -> 0))
        
        //Foerste 6 gaet 
        // Hvis koden bestaar af fire ens farver, kan programmet gaette det paa
        // i+1 gaet, hvis koden har vaerdi i, hvor Red = 0, Green = 1, etc.
        let mutable whiteCounter = 0
        let mutable n = 0
        let colors = [Red; Green; Yellow; Purple; White; Black]
        //Laver gaet med samme farve paa alle pladser, en farve af gangen
        while (whiteCounter < 4) && (n < 6) do
            guessCode <- [colors.[n]; colors.[n]; colors.[n]; colors.[n]]
            //Midlertidigt histogram der bruges til at sammensaette 7. gaet. Indsaetter tal svarende
            //til hvor mange sorte stifter der modtages som respons paa gaet paa plads svarende til
            //farvenummer
            temp.[n] <- snd (validate hiddenCode [colors.[n]; colors.[n];
                                                  colors.[n]; colors.[n]])
            whiteCounter <- whiteCounter + temp.[n]
            n <- n + 1
        
        
        
        //Sammensaetter 7. gaet 
        guessCode <- []
        for i = 0 to 5 do
            while temp.[i] > 0 do
                guessCode <- List.append guessCode [colors.[i]]
                temp.[i] <- temp.[i] - 1
        
        //Tjekker hvor mange sorte der gives som respons paa sammensatte 7. gaet. 
        //Hvis opgaven er den samme farve paa alle 4 pladser, vil programmet bruge et ekstra
        //gaet, selvom den har det rigtige gaet. Derfor aendrer vi variablen blacksValidate, som goer at
        //guessCount ikke taeller op naar den er forskellig fra 0.
        let mutable blacks = 
            guessCount <- guessCount - 1
            blacksValidate <- 1
            snd (validate hiddenCode guessCode)
        
        //aendrer den tilbage saa gaet taeller op igen ved hver validate
        blacksValidate <- 0
        
        if blacks = 4 then
            let mutable s = 0
            //Hvis alle farverne er den samme, bruges der ikke et gaet.
            for i = 0 to 5 do
                if guessCode.[0..3] = [colors.[i]; colors.[i]; colors.[i]; colors.[i]] then
                    s <- s + 1
                else ()     
            //Men hvis opgaven nu er [Red; Red; Green; Green] vil programmet have den efter andet
            //gaet. Den skal dog stadig bruge et gaet paa rent faktisk at gaette paa netop det.
            if s = 0 then
                //Bruges udelukkende for at bruge et gaet
                validate hiddenCode guessCode |> ignore
            else ()
        else
            blacks <- snd (validate hiddenCode guessCode)
        
        /// <summary>Tjekker antal sorte og omrokerer farverne i positionerne</summary>
        /// <param name="guess">Bruges naar programmet kaldes rekursivt til have gemme nuvaerende
        /// vaerdi af guessCode</param>
        /// <returns>Kalder win funktionen hvis der gives 4 sorte stifter som respons, ellers
        /// omrokerer den farver</returns>
        let rec compRearrange guess =
            match blacks with
            | 4 -> 
                win "Comp" gameChoice guessCount 0
            | 2 ->
                guessCode <- [guessCode.[3]] @ guessCode.[1..2] @ [guessCode.[0]]
                // Bliver til 4 2 3 1 
                blacks <- snd (validate hiddenCode guessCode)
        
                match blacks with
                | 4 -> 
                    win "Comp" gameChoice guessCount 0
                | 2 ->
                    if guessCode.[0] = guessCode.[2] && guessCode.[1] = guessCode.[3] then
                        guessCode <- guessCode.[0..1] @ [guessCode.[3]] @ [guessCode.[2]]
                        blacks <- snd (validate hiddenCode guessCode)
                        if blacks = 4 then
                            win "Comp" gameChoice guessCount 0
                        else
                            guessCode <- [guessCode.[1]] @ [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[2]]
                            blacks <- snd (validate hiddenCode guessCode)
                            win "Comp" gameChoice guessCount 0
                    elif guessCode.[0] = guessCode.[2] then
                        guessCode <- guessCode.[0..1] @ [guessCode.[3]] @ [guessCode.[2]]
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameChoice guessCount 0
                    else
                        guessCode <- [guessCode.[1]] @ [guessCode.[0]] @ guessCode.[2..3]
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameChoice guessCount 0
                | 0 ->
                    guessCode <- List.rev guessCode
                    blacks <- snd (validate hiddenCode guessCode)
                    win "Comp" gameChoice guessCount 0
                | 1 ->
                    guessCode <- [guessCode.[2]] @ [guessCode.[1]] @ [guessCode.[3]] @ [guessCode.[0]]
                    // Bliver til 3 2 1 4
                    blacks <- snd (validate hiddenCode guessCode)
                    match blacks with
                    | 4 -> 
                        win "Comp" gameChoice guessCount 0
                    | 2 -> 
                        guessCode <- [guessCode.[0]] @ [guessCode.[2]] @ [guessCode.[1]] @ [guessCode.[3]]
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameChoice guessCount 0
                    | 0 ->
                        guessCode <- [guessCode.[2]] @ [guessCode.[3]] @ [guessCode.[0]] @ [guessCode.[1]]
                        // Bliver til 1 4 3 2
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameChoice guessCount 0
                    | 1 ->
                        guessCode <- [guessCode.[1]] @ [guessCode.[2]] @ [guessCode.[0]] @ [guessCode.[3]]
                        // Bliver til 2 1 3 4
                        blacks <- snd (validate hiddenCode guessCode)
                        match blacks with
                        | 4 -> 
                            win "Comp" gameChoice guessCount 0
                        | 2 -> 
                            guessCode <- guessCode.[0..1] @ [guessCode.[3]] @ [guessCode.[2]]
                            win "Comp" gameChoice guessCount 0
                        | 0 ->
                            guessCode <- [guessCode.[1]] @ [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[2]]
                            // Bliver til 1 2 4 3
                            blacks <- snd (validate hiddenCode guessCode)
                            win "Comp" gameChoice guessCount 0
                        | _ -> printfn "Fejl paa 2. indre"
                    | _ -> printfn "Fejl paa 2. midt"
                | _ -> printfn "Fejl paa 2. ydre"
            | 1 ->
                guessCode <- [guessCode.[3]] @ guessCode.[1..2] @ [guessCode.[0]]
                // Bliver til 4 2 3 1
                blacks <- snd (validate hiddenCode guessCode)
                match blacks with
                | 2 -> 
                    if guessCode.[0] = guessCode.[2] then
                        guessCode <- [guessCode.[1]] @ [guessCode.[0]] @ guessCode.[2..3]
                        blacks <- snd (validate hiddenCode guessCode)
                        if blacks = 4 then
                            win "Comp" gameChoice guessCount 0
                        else
                            guessCode <- [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[2]] @ [guessCode.[1]]
                            blacks <- snd (validate hiddenCode guessCode)
                            win "Comp" gameChoice guessCount 0
                    else
                        compRearrange guessCode
                | 1 ->
                    if guessCode.[0] = guessCode.[2] then
                        guessCode <- [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[1]] @ [guessCode.[2]]
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameChoice guessCount 0
                    else
                        guessCode <- [guessCode.[1]] @ [guessCode.[2]] @ [guessCode.[0]] @ [guessCode.[3]]
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameChoice guessCount 0
                | 0 ->
                    guessCode <- [guessCode.[3]] @ [guessCode.[2]] @ [guessCode.[1]] @ [guessCode.[0]]
                    compRearrange guessCode 
                | _ -> printfn "Fejl paa 1."
        
            | 0 ->
                guessCode <- guessCode.[3] :: guessCode.[0..2]
                // Bliver til 4 1 2 3
                blacks <- snd (validate hiddenCode guessCode)
                match blacks with
                | 4 -> 
                    win "Comp" gameChoice guessCount 0
                | 1 ->  
                    if guessCode.[0] = guessCode.[3] then 
                        guessCode <- [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[1]] @ [guessCode.[2]]
                        blacks <- snd (validate hiddenCode guessCode)
                        //bliver til 4 3 1 2 
                        win "Comp" gameChoice guessCount 0
                    else 
                        compRearrange guessCode
                | 2 ->
                    if guessCode.[0] = guessCode.[3] then 
                        guessCode <- [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[2]] @ [guessCode.[1]]
                        blacks <- snd (validate hiddenCode guessCode)
                        //bliver til 4 3 2 1
                        win "Comp" gameChoice guessCount 0
                    else 
                        compRearrange guessCode
                | 0 ->
                    compRearrange guessCode
                | _ -> printfn "Fejl paa 0."
            | _ -> printfn "Fejl paa ydre"
        compRearrange guessCode

    if choice = "0" then        
        //Startskaerm
       
        System.Console.Clear()

        printfn "Hey. And welcome to the game Mastermind. If this is your"
        printfn "first time, you are strongly encourged to write \"help\", and you"
        printfn "will receive a help screen explaining the goal of the games"
        printfn "and the controls. If you are already familiar, you can simply"
        printfn "just type 'start' to start the game.\n"
        printf  "> "

        let mutable userInputStartScreen = System.Console.ReadLine ()

        //Indtil brugeren skriver enten help eller start, kommer man ikke videre
        while (playersInputCorrectStartScreen userInputStartScreen) = false do
            printfn "\nPlease enter either \"help\" or \"start\"\n"
            printf  ">"
            userInputStartScreen <- System.Console.ReadLine ()


        if userInputStartScreen = "help" || userInputStartScreen = "Help" then
            System.Console.Clear()
            printfn "THE GOAL OF MASTERMIND"
            printfn "The goal is to guess the secret code made by the \"codemaker\""
            printfn "A code consists of 4 coloured pins, withe the following"
            printfn "color: Red, Green, Yellow, Purple, White, and Black."
            printfn "After each of your guess, you get a pair of white and black"
            printfn "pegs, telling you how much off you are."
            printfn "The number of white pegs tells how many of the pegs in your"
            printfn "guess is the right color, but in the wrong place. And the"
            printfn "number of black pegs, tells the how many of the pegs in"
            printfn "your guess is right, both in terms of color and position."
            printfn "\nFor example, if the hidden code is: R G B P"
            printfn "and your guess is R B W W, your answer in white and black"
            printfn "pegs, will be shown to the right. In this example it will"
            printfn "look like this:"
            printfn "                       W B"
            printfn "R B W W                1 1"
            printfn "When your guess has 4 black pegs, it means your are guessed"
            printfn "the secret code, and won the game."
            printfn "Now there is really only one thing left, who is to play?"

        printfn "\n"
        printfn "The different game modes:\n"
        printfn "(1) Human vs Computer, with computer acting as \"codemaker\""
        printfn "(2) Human vs Human"
        printfn "(3) Computer vs Human, with human acting as \"codemaker\""
        printfn "(4) Computer vs Computer"
        printfn "\nChoose a number between 1-4\n"
        printf  "> "
        
        let mutable playersInput = System.Console.ReadLine ()
        
        //Indtil man skriver tal mellem 1 og 4 kommer man ikke videre
        while (playersInputCorrect playersInput) = false do
            printfn "\nYou have to choose a number between 1 and 4\n"
            printf  "> "

            playersInput <- System.Console.ReadLine ()
        //Kalder spilfunktion rekursivt med indtastede tal som argument 
        game playersInput
    
    elif choice = "1" then
        //Human vs Computer
       
        System.Console.Clear()

        printfn "You chose the game mode Human vs Computer, where the computer"
        printfn "acts as the \"codemaker\""
        
        //Opgaven generes automatisk med et kald til makeCode og gemmes i variablen hiddenCode
        let hiddenCode = makeCode Computer
        
        printfn "\nThe computer has now generated a code.\n"
        printf "Press ENTER to begin playing..."
        let p = System.Console.ReadLine()
         
        //Kalder funktionen gamePlay der foerst terminerer naar opgaven er loest
        let humanNumberOfTries = gamePlay hiddenCode
        //Kalder win funktionen. Giver den player, hvilken spiltype der lige blev spillet og hvor
        //mange forsoeg det tog.
        win "Human" "1" 0 humanNumberOfTries
    
    elif choice = "2" then
        //Human vs Human
        
        System.Console.Clear()

        printfn "You chose the game mode Human vs Human"
        
        //Opgaven laves ved et kald til makeCode og gemmes i variablen hiddenCode
        let hiddenCode = makeCode Human
        
        printfn "\nCode accepted.\n"
        printf "Press ENTER to give control to player 2..."
        let p = System.Console.ReadLine()
        
        //Kalder funktionen gamePlay der foerst terminerer naar opgaven er loest
        let humanNumberOfTries = gamePlay hiddenCode
        //Kalder win funktionen. Giver den player, hvilken spiltype der lige blev spillet og hvor
        //mange forsoeg det tog.
        win "Human" "2" 0 humanNumberOfTries
        
    elif choice = "3" then
        //Computer vs Human
        //guessCount skal nulstilles da det er en global variabel
        guessCount <- 0

        System.Console.Clear()
        
        printfn "Computer vs Human, with human acting as \"codemaker\""

        //Opgaven laves ved et kald til makeCode og gemmes i variablen hiddenCode
        let hiddenCode = makeCode Human
        
        printfn "\nCode accepted.\n"
        printf "Press ENTER to give control to the computer..."
        let p = System.Console.ReadLine()
        let k = System.Console.Clear()

        //Funktionen compGuess kaldes med opgaven og spiltypen. Den terminerer foerst naar opgaven er
        //loest, og win kaldes videre fra den.
        compGuess hiddenCode "3"

    else
        //Computer vs Computer
        //guessCount skal nulstilles da det er en global variabel
        guessCount <- 0

        System.Console.Clear()
        
        printfn "You chose the game mode Computer vs Computer"
        
        //Opgaven generes automatisk med et kald til makeCode og gemmes i variablen hiddenCode
        let hiddenCode = makeCode Computer
        
        printfn "\nThe computer has now generated a code.\n"
        printfn "It is: %A\n" hiddenCode 
        printf "Press ENTER to begin the game..."
        let p = System.Console.ReadLine()
        let k = System.Console.Clear()
       
        //Funktionen compGuess kaldes med opgaven og spiltypen. Den terminerer foerst naar opgaven er
        //loest, og win kaldes videre fra den.
        compGuess hiddenCode "4"

game "0"
