// POP opgave 8 - Mastermind i F#
// Skrevet af Asibi Ayagiba, Mads Carstensen og Andreas Drivsholm

module mastermind

// De præfinerede typer vi skal bruge i vores kode.
type codeColor = 
    Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

/// <summary>Denne funktion sørger for at få konverteret et bogstav om til
/// den passende farve.</summary>
/// <remarks>Wildcardet bliver aldrig matchet, da denne funktion altid
/// bliver kaldt med enten "R", "r", "G", etc, pga hjælpefunktionen
/// legitColors sørger for at input altid bliver tjekket inden det bliver
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

/// <summary>inputCode oversætter en liste af bogstaver som definerer farver
/// til en liste af egentlige farver, altså er typen codeColor</summary>
/// <params>Tager et input koden som paramterer</params>
/// <returns>Returnerer en liste af typen code som er defineret som værende
/// en liste med elementer af typen codeColor.</returns>
let rec inputCodeToCode inputCode : code =
    match inputCode with
    | [] -> []
    | x :: xs -> charToColor x :: inputCodeToCode xs


/// <summary>Genererer en tilfældig kode. Bliver brugt når computer fungerer
/// som opgavestilleren.</summary>
/// <remarks>Opretter et nyt object rand, som er defineret ved klassen Random.
/// Vi bruger klassens metode Next() til at generere tal mellem 1 og 6, med
/// begge tal inklusive. Vi skriver Next(1,7), men her er syvtallet eksklusivt.
/// </remarks>
/// <params>Denne funktion tager inten parametre</params>
/// <returns>Returnerer en en "kode". Det er en liste af 4 elementer, hvor
/// hvert element er af typen codeColor. Denne liste af farver er tilfældige
/// </returns>
let randomCode : code = 
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
/// <remarks>Funktionen har to branches, alt afhængig om det er computeren
/// der fungerer som "codemaker" eller om det er en bruger.</remarks>
/// <params>Tager som parameter en spiller type som kan være enten Human eller
/// Computer</params>
/// <returns>Returnerer en kode af typen code</summary>
let makeCode (player : player) : code =
    match player with
    // Hvis "codemaker" er Computer, bliver funktionen randomCode kaldt.
    // randomCode returnerer en kode af typen code.
    | Computer ->
        randomCode
    // Hvis "codemaker" er Human, bliver denne branch kørt.
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
        
        // inputCode sættes til at være lige med brugerens input.
        // Brugerens input består af 4 tegn separeret af mellemrum. Disse tegn
        // bliver udvindet af tekststrengen ved hjælpe af metoden Split, som
        // forårsager at inputet bliver omdannet til en liste af tegn.
        // Denne liste gemmes i den muterbare liste inputCode.
        inputCode <- Array.toList ((System.Console.ReadLine ()).Split(' '))
        
        // Funktionen userCode tjekker om brugeren input er korrekt.
        // Hvis inputtet ikke er korrekt, spørges der om nyt input.
        while (userCode inputCode) = false do
            printfn "\nYou have to choose a valid color"
            printf  "\n> "

            inputCode <- Array.toList ((System.Console.ReadLine ()).Split(' '))
        
        // Når inputtet er korrekt, kaldes funktionen inputCodeToCode med
        // inputCode som argument. Dette konverterer koden fra at være en liste
        // af tegn, til at være en liste af typen code.
        let code = inputCodeToCode inputCode
        code

let makeHisto (code : codeColor list) =
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

let validate (hidden : code) (guess : code) : answer =
    let histoHidden = makeHisto hidden
    let histoGuess = makeHisto guess
    
    let rec minHisto hiddenHisto guessHisto =
        match (hiddenHisto, guessHisto) with
        | (x :: xs, y :: ys) -> (min x y) :: minHisto xs ys
        | ([], _) -> []
        | _ -> failwith "hej"
    
    let sumPegs = List.sum (minHisto histoHidden histoGuess)
    
    let rec blackFun hidden guess =
        match (hidden, guess) with
        | (x :: xs, y :: ys) ->
                            if x = y then
                                1 + blackFun xs ys
                            else
                                0 + blackFun xs ys
        | ([], _) -> 0
        | _ -> failwith "hej"

    let blackPegs = blackFun hidden guess
    let whitePegs = sumPegs - blackPegs

    guessCount <- guessCount + 1
    if blacksValidate = 0 then
        printfn "%d:  %A" guessCount guess
    else ()
    
    (whitePegs, blackPegs)

let playersInputCorrect number =
    match number with
    | "1" | "2" | "3" | "4" -> true
    | _ -> false

let playersInputCorrectStartScreen input =
    match input with
    | "help" | "Help" | "start" | "Start" ->
        true
    | _ ->
        false

let guess (player : player) (board : board) : code =
    match player with
    | Human ->
        let mutable userInput = []
        
        printfn "\nYour board so far:"
        
        for i = 0 to board.Length-1 do
            printfn "%A" (board.[i])
        
        printf "\n> "
        
        userInput <- Array.toList (System.Console.ReadLine().Split(' '))
        
        while (userCode userInput) = false do
            printfn "\nInvalid input\n"
            printf  "> "
            userInput <- Array.toList (System.Console.ReadLine().Split(' '))
        
        inputCodeToCode userInput
    | Computer ->
        [Red]

let gamePlay hiddenCode =
    let mutable board : board = []
    let mutable humanGuess : code = [Red]
    let mutable validation = (0,0)
    let mutable tries : int = 0
    while snd (validation) <> 4 do
        System.Console.Clear()

        printfn "You have used %d guesses so far." tries
        
        humanGuess <- guess Human board
        
        validation <- (validate hiddenCode humanGuess)
        
        
        board <- board @ [(humanGuess, validation)]

        tries <- tries + 1
    tries
    

let rec game choice = 
    let rec win player choice1 guessC tries = 
        match player with
        | "Human" -> 
            System.Console.Clear()
            printfn "You win!"
            printfn "It took you %d guesses!" tries

        | _ -> 
            printfn "\nThe computer wins!"
            printfn "It took it %d guesses!" guessC
            
        printfn "\nWould you like to:"
        printfn "1. Go to the startpage?"
        printfn "2. Play the same gamemode again?"
        printfn "3. Quit the game?"
        printf "> "
        let userInput = System.Console.ReadLine ()
   
        match userInput with
        | "1" -> game "0"
        | "2" -> game choice1 
        | "3" -> System.Environment.Exit 1
        | _ -> win player choice1 guessC tries

    let compGuess hiddenCode gameC =
        //Gættet
        let mutable guessCode : code = []
         
        // All of these variables will be used, no matter the game mode
        let board : board = []
        let mutable validation = (0,0)
        let mutable whitePegs : int = 0
        let mutable blackPegs : int = 0

        let temp = (Array.init 6 (fun _ -> 0))
        
        //Første 6 gæt 
        let mutable whiteCounter = 0
        let mutable n = 0
        let colors = [Red; Green; Yellow; Purple; White; Black]
        while (whiteCounter < 4) && (n < 6) do
            guessCode <- [colors.[n]; colors.[n]; colors.[n]; colors.[n]]
            temp.[n] <- snd (validate hiddenCode [colors.[n]; colors.[n];
                                                  colors.[n]; colors.[n]])
            whiteCounter <- whiteCounter + temp.[n]
            n <- n + 1
        
        
        
        //Histogram til gæt 
        guessCode <- []
        for i = 0 to 5 do
            while temp.[i] > 0 do
                guessCode <- List.append guessCode [colors.[i]]
                temp.[i] <- temp.[i] - 1
        
        //Black validate 
        let mutable blacks = 
            guessCount <- guessCount - 1
            blacksValidate <- 1
            snd (validate hiddenCode guessCode)
            
        blacksValidate <- 0
        
        if blacks = 4 then
            let mutable s = 0
            for i = 0 to 5 do
                if guessCode.[0..3] = [colors.[i]; colors.[i]; colors.[i]; colors.[i]] then
                    s <- s + 1
                else ()     
            if s = 0 then
                validate hiddenCode guessCode |> ignore
            else ()
        else ()
        
        //Tjekker antal sorte og gætter
        let rec compRearrange guess =
            match blacks with
            | 4 -> 
                win "Comp" gameC guessCount 0
            | 2 ->
                guessCode <- [guessCode.[3]] @ guessCode.[1..2] @ [guessCode.[0]]
                // Bliver til 4 2 3 1 
                blacks <- snd (validate hiddenCode guessCode)
        
                match blacks with
                | 4 -> 
                    win "Comp" gameC guessCount 0
                | 2 ->
                    if guessCode.[0] = guessCode.[2] && guessCode.[1] = guessCode.[3] then
                        guessCode <- guessCode.[0..1] @ [guessCode.[3]] @ [guessCode.[2]]
                        blacks <- snd (validate hiddenCode guessCode)
                        if blacks = 4 then
                            win "Comp" gameC guessCount 0
                        else
                            guessCode <- [guessCode.[1]] @ [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[2]]
                            blacks <- snd (validate hiddenCode guessCode)
                            win "Comp" gameC guessCount 0
                    elif guessCode.[0] = guessCode.[2] then
                        guessCode <- guessCode.[0..1] @ [guessCode.[3]] @ [guessCode.[2]]
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameC guessCount 0
                    else
                        guessCode <- [guessCode.[1]] @ [guessCode.[0]] @ guessCode.[2..3]
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameC guessCount 0
                | 0 ->
                    guessCode <- List.rev guessCode
                    blacks <- snd (validate hiddenCode guessCode)
                    win "Comp" gameC guessCount 0
                | 1 ->
                    guessCode <- [guessCode.[2]] @ [guessCode.[1]] @ [guessCode.[3]] @ [guessCode.[0]]
                    // Bliver til 3 2 1 4
                    blacks <- snd (validate hiddenCode guessCode)
                    match blacks with
                    | 4 -> 
                        win "Comp" gameC guessCount 0
                    | 0 ->
                        guessCode <- [guessCode.[2]] @ [guessCode.[3]] @ [guessCode.[0]] @ [guessCode.[1]]
                        // Bliver til 1 4 3 2
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameC guessCount 0
                    | 1 ->
                        guessCode <- [guessCode.[1]] @ [guessCode.[2]] @ [guessCode.[0]] @ [guessCode.[3]]
                        // Bliver til 2 1 3 4
                        blacks <- snd (validate hiddenCode guessCode)
                        match blacks with
                        | 4 -> 
                            win "Comp" gameC guessCount 0
                        | 2 -> 
                            guessCode <- guessCode.[0..1] @ [guessCode.[3]] @ [guessCode.[2]]
                            win "Comp" gameC guessCount 0
                        | 0 ->
                            guessCode <- [guessCode.[1]] @ [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[2]]
                            // Bliver til 1 2 4 3
                            blacks <- snd (validate hiddenCode guessCode)
                            win "Comp" gameC guessCount 0
                        | _ -> printfn "Nej"
                    | _ -> printfn "Hej"
                | _ -> printfn "Hej"
            | 1 ->
                guessCode <- [guessCode.[3]] @ guessCode.[1..2] @ [guessCode.[0]]
                // Bliver til 4 2 3 1
                blacks <- snd (validate hiddenCode guessCode)
                match blacks with
                | 2 -> compRearrange guessCode
                | 1 ->
                    if guessCode.[0] = guessCode.[2] then
                        guessCode <- [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[1]] @ [guessCode.[2]]
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameC guessCount 0
                    else
                        guessCode <- [guessCode.[1]] @ [guessCode.[2]] @ [guessCode.[0]] @ [guessCode.[3]]
                        blacks <- snd (validate hiddenCode guessCode)
                        win "Comp" gameC guessCount 0
                | 0 ->
                    guessCode <- [guessCode.[3]] @ [guessCode.[2]] @ [guessCode.[1]] @ [guessCode.[0]]
                    compRearrange guessCode 
                | _ -> printfn "Hej"
        
            | 0 ->
                guessCode <- guessCode.[3] :: guessCode.[0..2]
                // Bliver til 4 1 2 3
                blacks <- snd (validate hiddenCode guessCode)
                match blacks with
                | 4 -> 
                    win "Comp" gameC guessCount 0
                | 1 ->  
                    if guessCode.[0] = guessCode.[3] then 
                        guessCode <- [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[1]] @ [guessCode.[2]]
                        blacks <- snd (validate hiddenCode guessCode)
                        //bliver til 4 3 1 2 
                        win "Comp" gameC guessCount 0
                    else 
                        compRearrange guessCode
                | 2 ->
                    if guessCode.[0] = guessCode.[3] then 
                        guessCode <- [guessCode.[0]] @ [guessCode.[3]] @ [guessCode.[2]] @ [guessCode.[1]]
                        blacks <- snd (validate hiddenCode guessCode)
                        //bliver til 4 3 2 1
                        win "Comp" gameC guessCount 0
                    else 
                        compRearrange guessCode
                | 0 ->
                    compRearrange guessCode
                | _ -> printfn "hej"
            | _ -> printfn "hej"
        compRearrange guessCode

    if choice = "0" then        
        //Startpage
       
        System.Console.Clear()

        printfn "Hey. And welcome to the game Mastermind. If this is your"
        printfn "first time, you are strongly encourged to write \"help\", and you"
        printfn "will receive a help screen explaining the goal of the games"
        printfn "and the controls. If you are already familiar, you can simply"
        printfn "just type 'start' to start the game.\n"
        printf  "> "

        let mutable userInputStartScreen = System.Console.ReadLine ()

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
            printfn "the secret code, and won the game. But be clever, you only"
            printfn "have 10 tries to guess the secret code!\n"
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
        
        while (playersInputCorrect playersInput) = false do
            printfn "\nYou have to choose a number between 1 and 4\n"
            printf  "> "

            playersInput <- System.Console.ReadLine ()
        
        game playersInput
    
    elif choice = "1" then
        //Human vs Computer
       
        System.Console.Clear()

        printfn "You chose the game mode Human vs Computer, where the computer"
        printfn "acts as the \"codemaker\""
        
        let hiddenCode = makeCode Computer
        
        printfn "\nThe computer has now generated a code.\n"
        printf "Press ENTER to begin playing..."
        let p = System.Console.ReadLine()
         
        let k = gamePlay hiddenCode
        win "Human" "1" 0 k  
    
    elif choice = "2" then
        //Human vs Human
        
        System.Console.Clear()

        printfn "You chose the game mode Human vs Human"
        
        let hiddenCode = makeCode Human
        
        printfn "\nCode accepted.\n"
        printf "Press ENTER to give control to player 2..."
        let p = System.Console.ReadLine()
        
        let k = gamePlay hiddenCode
        win "Human" "2" 0 k 
        
    elif choice = "3" then
        //Computer vs Human
        guessCount <- 0

        System.Console.Clear()
        
        printfn "Computer vs Human, with human acting as \"codemaker\""

        let hiddenCode = makeCode Human
        
        printfn "\nCode accepted.\n"
        printf "Press ENTER to give control to the computer..."
        let p = System.Console.ReadLine()
        let k = System.Console.Clear()

        compGuess hiddenCode "3"

    else
        //Computer vs Computer
        guessCount <- 0

        System.Console.Clear()
        
        printfn "You chose the game mode Computer vs Computer"
        
        let hiddenCode = makeCode Computer
        
        printfn "\nThe computer has now generated a code.\n"
        printfn "It is: %A\n" hiddenCode 
        printf "Press ENTER to begin the game..."
        let p = System.Console.ReadLine()
        let k = System.Console.Clear()
       
        compGuess hiddenCode "4"

game "0"
