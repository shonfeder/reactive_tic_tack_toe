[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

module Reactive_tic_tack_toe_app =
  Eliom_registration.App (
    struct
      let application_name = "reactive_tic_tack_toe"
      let global_data_path = None
    end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let () =
  Reactive_tic_tack_toe_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"reactive_tic_tack_toe"
           ~css:[["css";"reactive_tic_tack_toe.css"]]
           Html.F.(body [
             h1 [pcdata "Welcome from Eliom's distillery!"];
           ])))
