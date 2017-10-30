[%%shared
  open Eliom_lib
  open Eliom_content
  open Html.D
  open Aux
]

module State = struct
  let games = ref []
  let game_id = ref 0
  let new_game () =
    game_id := !game_id + 1;
    games   := game_id :: !games;
    !game_id

  let lookup_game id =
    if List.mem (ref id) (!games)
    then Some id
    else None
end

module Rttt = struct
  module Info = struct
    let application_name = "rttt"
    let global_data_path = None
  end
  module App = Eliom_registration.App (Info)
end

module Service = struct
  open Eliom_service
  module Param = Eliom_parameter

  let main =
    create
      ~path:(Path [])
      ~meth:(Get Param.unit)
      ()

  let game =
    create
      ~path:(Path ["game"])
      ~meth:(Get (Param.int "id"))
      ()

  let games =
    create
      ~path:(Path ["games"; ""])
      ~meth:(Get Param.unit)
      ()

end

[%%shared
module Game = struct

  module Letter = struct
    type t =
      | Revealed of char
      | Hidden   of char
    [@@deriving show]

    let mk_hidden
      : char -> t
      = fun char -> Hidden char

    let reveal
      : t -> t = function
      | Hidden c -> Revealed c
      | revealed -> revealed

    let to_char
      : t -> char = function
      | Revealed c -> c
      | Hidden c   -> c

    let is_char
      : char -> t -> bool
      = fun c l -> to_char l = c

    let is_revealed
      : t -> bool = function
      | Revealed _ -> true
      | Hidden _   -> false

  end

  module Word = struct

    type t = Letter.t list
    [@@deriving show]

    let of_string
      : string -> t
      = fun str ->
        str |> CCString.to_list |> CCList.map Letter.mk_hidden

    let reveal
      : char -> t -> t
      = fun c word ->
        let reveal_match l =
          if Letter.is_char c l then Letter.reveal l else l
        in List.map reveal_match word

    let includes_char
      : t -> char -> bool
      = fun word char ->
        CCList.exists (Letter.is_char char) word
  end

  type t =
    { guesses   : int
    ; word      : Word.t
    ; incorrect : char list }
  [@@deriving show]

  let max_guesses = 6

  module Update = struct
    let start
      : string -> (t, t) result
      = fun str ->
        Ok { guesses   = 0
           ; word      = Word.of_string str
           ; incorrect = [] }

    let guess
      : char -> t -> (t, t) result
      = fun guess ({guesses; word; incorrect} as game) ->
        if Word.includes_char word guess then
          Ok { guesses
             ; word = (Word.reveal guess word)
             ; incorrect }
        else if List.mem guess incorrect then
          Ok game
        else if guesses < max_guesses then
          Ok { guesses = (guesses + 1)
             ; word
             ; incorrect = (guess :: incorrect) }
        else
          Error game
  end
end

module Render = struct
  open Eliom_content.Html.F
  module F = Eliom_content.Html.F

  let gallows guesses =
    let image = ["1";"2";"3";"4";"5";"6";"7";"8";"9";"10"] in (* XXX *)
    let index =
      if guesses < List.length image
      then guesses
      else (List.length image) - 1
    in
    div [ p [pcdata @@ List.nth image index] ]

  let letter = function
    | Game.Letter.Revealed c -> c
    | Game.Letter.Hidden _   -> '_'

  let mystery_word word =
    div [p [pcdata @@ CCString.of_list @@ List.map letter word]]

  let already_tried incorrect =
    div [p [pcdata @@ CCString.of_list incorrect]]

  let game =
    function
    | Error _ ->
      div [p [pcdata "Game over!"]]
    | Ok Game.{guesses; word; incorrect} ->
      if List.for_all Game.Letter.is_revealed word then
        div [p [pcdata "You win!"]]
      else
        div [ gallows guesses
            ; mystery_word word
            ; already_tried incorrect ]
end
]

[%%client

open Eliom_content.Html

let value_signal, set_value = React.S.create @@ Game.Update.start "initial"

let content_signal : Html_types.div_content_fun elt React.signal =
  React.S.map Render.game value_signal

let make_client_nodes () =
  [ R.node content_signal ]

]

let make_input () =
  let open Html in
  let inp = D.Raw.input ~a:[F.a_input_type `Text] () in
  let _ =
    [%client
    (let open Lwt_js_events in
     async (fun () ->
       let inp = To_dom.of_input ~%inp in
       keyups inp (fun _ _ ->
         match CCString.to_list (Js.to_string (inp##.value)) with (* XXX *)
         | []      -> Lwt.return ()
         | char::_ ->
           let guess = Game.Update.guess char in
           let game = React.S.value value_signal in
           set_value @@ CCResult.flat_map guess game;
           Lwt.return ()))
     : unit)
    ]
  in
  inp

module Handler = struct
  let basic_page = Eliom_tools.F.html ~css:[["css"; "rttt.css"]]

  let main _get _post =
    let open Eliom_content.Html in
    let inp = make_input () in
    let cldiv = C.node [%client D.div (make_client_nodes ())] in
    let body = let open Html.F in
      body [ h1 [pcdata "Reactive Tic-Tac-Toe"]
           ; inp
           ; F.h2 [pcdata "Client side reactive nodes"]
           ; cldiv ]
    in
    Lwt.return @@ basic_page
      ~title:"rttt"
      body

  let game id _post =
    let body =
      let open Html.F in
      match State.lookup_game id with
      | None ->
        body [h1 [pcdata "Sorry! That game does not exist."]]
      | Some id ->
        let game_number = Printf.sprintf "Game #%i" id in
        body [h1 [pcdata game_number]]
    in
    Lwt.return @@ basic_page
      ~title:("Reactive Tic-Tac-Toe Game")
      body

  let games _get _post =
    let games = "tbd" in
    let body = let open Html.F in
      body [ h1 [pcdata "Ongoing Games"]
           ; p  [pcdata games]]
    in
    Lwt.return @@ basic_page
      ~title:("Ongoing Games")
      body
end

let () =
  Rttt.App.register
    ~service:Service.main
    Handler.main

let () =
  Rttt.App.register
    ~service:Service.game
    Handler.game

let () =
  Rttt.App.register
    ~service:Service.games
    Handler.games
