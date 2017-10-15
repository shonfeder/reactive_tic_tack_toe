[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

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

  let game_room =
    create
      ~path:(Path ["games"; ""])
      ~meth:(Get Param.int)
      ()
end

module Handler = struct
  let page = Eliom_tools.F.html

  let main _get _post =
    let body = let open Html.F in
      body [h1 [pcdata "Reactive Tic-Tac-Toe"]]
    in
    Lwt.return @@ page
      ~title:"rttt"
      ~css:[["css";"rttt.css"]]
      body

  let game_room game_id _post =
    let body = let open Html.F in
      let heading = Printf.sprintf "Game #%i" game_id in
      body [h1 [pcdata "Game "]]
    in
    Lwt.return @@ page
      ~title:"rttt"
      ~css:[["css";"rttt.css"]]
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
