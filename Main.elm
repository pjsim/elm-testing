-- random shuffle
-- Msg should be Won or Lost including string saying "blackjack" ""won" etc
-- properly generate cards and suits
-- should use test for each case
-- svg for cards in future

module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    { round : String
    , deck : List Card
    , playerHand : List Card
    , dealerHand : List Card
    }


type alias Card =
    Int


type Msg
    = DealCards
    | NewRound
    | RoundWon
    | RoundLost
    | Blackjack
    | Ongoing
    | Stay


createDeck : List Card
createDeck =
    let
        ace =
            [ 11 ]

        king_queen_jack =
            [ 10, 10, 10 ]

        others =
            List.range 2 9

        suit =
            List.concat [ ace, king_queen_jack, others ]
    in
        List.concat (List.repeat 4 suit)


shuffleDeck : List Card -> List Card -> ( List Card, List Card )
shuffleDeck oldDeck newDeck =
    let
        grabbedCard =
            List.take 1 oldDeck

        shuffledNewDeck =
            List.append grabbedCard (List.reverse newDeck)

        shuffledOldDeck =
            List.drop 1 oldDeck
    in
        case List.isEmpty oldDeck of
            True ->
                ( newDeck, oldDeck )

            False ->
                shuffleDeck shuffledOldDeck shuffledNewDeck


init : Model
init =
    { round = "Ongoing"
    , deck = (shuffleDeck createDeck []) |> Tuple.first
    , playerHand = []
    , dealerHand = []
    }


main : Program Never Model Msg
main =
    beginnerProgram { model = init, update = update, view = view }



-- UPDATE


dealCard : List Card -> List Card -> ( List Card, List Card )
dealCard hand deck =
    let
        deltCard =
            case List.head deck of
                Just card ->
                    card :: hand

                Nothing ->
                    []

        remainingDeck =
            case List.tail deck of
                Just deck ->
                    deck

                Nothing ->
                    []
    in
        if List.length hand < 1 then
            dealCard deltCard remainingDeck
        else
            ( deltCard, remainingDeck )


dealHands : Model -> Model
dealHands model =
    let
        ( playerHand, deckAfterPlayerDelt ) =
            dealCard model.playerHand model.deck

        ( dealerHand, deckAfterDealerDelt ) =
            dealCard model.dealerHand deckAfterPlayerDelt
    in
        { model | deck = deckAfterDealerDelt, playerHand = playerHand, dealerHand = dealerHand }


onlyDealerHands : Model -> Model
onlyDealerHands model =
    let
        ( dealerHand, deckAfterDealerDelt ) =
            dealCard model.dealerHand model.deck
    in
        { model | deck = deckAfterDealerDelt, dealerHand = dealerHand }


blackjack : List Card -> List Card -> Msg
blackjack playerHand dealerHand =
    let
        player =
            List.foldl (+) 0 playerHand

        dealer =
            List.foldl (+) 0 dealerHand
    in
        if player == 21 then
            Blackjack
        else if dealer == 21 then
            RoundLost
        else if player > 21 then
            RoundLost
        else if dealer > 21 then
            RoundWon
        else if dealer > 16 && player > dealer then
            RoundWon
        else if dealer > 16 && player < dealer then
            RoundLost
        else
            Ongoing


update : Msg -> Model -> Model
update msg model =
    case msg of
        DealCards ->
            let
                newModel =
                    dealHands model

                newMsg =
                    blackjack newModel.playerHand newModel.dealerHand
            in
                update newMsg newModel

        Stay ->
            let
                newModel =
                    onlyDealerHands model

                newMsg =
                    blackjack newModel.playerHand newModel.dealerHand
            in
                update newMsg newModel

        NewRound ->
            init

        RoundWon ->
            { model | round = "Won!" }

        RoundLost ->
            { model | round = "Bust!" }

        Blackjack ->
            { model | round = "Blackjack!" }

        Ongoing ->
            model


view : Model -> Html Msg
view model =
    let
        action =
            if model.round == "Ongoing" then
                div []
                    [ button [ onClick DealCards ] [ text "Hit Me!" ]
                    , button [ onClick Stay ] [ text "Stay" ]
                    ]
            else
                div []
                    [ button [ onClick NewRound ] [ text "New Round" ]
                    ]
    in
        div []
            [ action
            , div [] [ text <| toString <| List.length model.deck ]
            , div [] [ model.round |> toString |> String.append "Round: " |> text ]
            , div [] [ model.deck |> toString |> String.append "Deck: " |> text ]
            , div [] [ model.playerHand |> toString |> String.append "My Hand: " |> text ]
            , div [] [ model.dealerHand |> toString |> String.append "Dealer Hand: " |> text ]
            ]
