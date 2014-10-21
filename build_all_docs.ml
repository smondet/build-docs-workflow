

#use "topfind"
#thread
#require "ketrew"

open Nonstd
module Oldschool_string = String
module String = Sosa.Native_string
let (//) = Filename.concat
(*
:Use ketrew
:set makeprg=ocaml\ build_all_docs.ml
*)

let project
    ~description 
    ?build_documentation
    ?(interesting_checkouts=[])
    ~repository name =
  object (self)
    method name = name
    method description = description
    method build_documentation = build_documentation
    method repository = repository
    method clone_url =
      match repository with
      | `Bitbucket path -> sprintf "https://bitbucket.org/%s" path
      | `Github path -> sprintf "https://github.com/%s" path
    method repository_kind = 
      match repository with
      | `Github _ -> "Github"
      | `Bitbucket _ -> "Bitbucket"
    method repository_web_url = self#clone_url
    method basename = name (* this is an assumption that repository = name *)
    method interesting_checkouts = interesting_checkouts
  end

let build_website ~host ~work_dir projects =
  let open Ketrew.EDSL in
  let make prog =
    daemonize ~using:`Python_daemon ~host prog
  in
  let make_shell cmd = make Program.(sh cmd) in
  let tmp_dir m = 
    let name = work_dir // m in
    target name  ~product:(file ~host name)
      (* ~done_when:(Condition.(program ~returns:0  ~host *)
      (*                          Program.( *)
      (*                            shf "ls -la %s | grep 'total 0'" name))) *)
      ~make:(make Program.(
          exec ["rm"; "-fr"; name]
          && exec ["mkdir"; "-p"; name]
        ))
  in
  let tmp = tmp_dir "clones" in
  let results = tmp_dir "results" in
  let documentations =
    List.filter_map projects ~f:(fun p ->
        Option.map p#build_documentation (fun stuff_todo ->
            target (sprintf "docof-%s" p#name)
              ~dependencies:[tmp; results]
              ~make:(make Program.(
                  let do_stuff co =
                    List.map (stuff_todo co) (function
                      | `Do e -> exec e
                      | `Get f -> 
                        let dest = 
                          let dir = match co with "master" -> "" | _ -> co in
                          results#product#path // p#basename // dir in
                        exec ["mkdir"; "-p"; dest]
                        && exec ["rsync";  "-a"; sprintf "%s/" f; dest]
                      ) |> chain
                  in
                  exec ["cd"; tmp#product#path]
                  && exec ["git"; "clone"; p#clone_url]
                  && exec ["cd"; p#basename]
                  && shf "export ADD_TO_MENU='\
                          - Repository [at %s](%s)\n\
                          - Up: [Software Projects](../index.html)'"
                    p#repository_kind p#repository_web_url
                  && do_stuff "master"
                  && chain (List.map p#interesting_checkouts (fun co ->
                      exec ["git"; "checkout"; co]
                      &&  do_stuff co
                    ))
                ))
          )
      )
  in
  let index_page =
    let content =
      "# Software Projects\n\n\
       ## Current\n\n\
      "
      :: List.map projects (fun p ->
          let par = 
            sprintf "%s (see on [%s](%s)%s)." p#description
              p#repository_kind p#repository_web_url
              (match p#interesting_checkouts with
               | [] -> ""
               | more -> 
                 sprintf ", and also the documentation for version%s %s"
                   (if List.length more = 1 then "" else "s: ")
                   (List.map more ~f:(fun m ->
                        sprintf "[`%s`](%s/%s/index.html)" m 
                          p#basename m)
                    |> String.concat ~sep:", "))
          in
          match p#build_documentation with
          | Some _ ->
            sprintf "- [%s](./%s/index.html): %s\n"
              (Oldschool_string.capitalize p#name) p#basename par
          | None ->
            sprintf "- %s: %s\n"
              (Oldschool_string.capitalize p#name) par
        )
      @ [
        "\n\n## Contact\n\n\
         Please use if possible the issue trackers of the corresponding \
         repositories. \
         You may also contact [Seb Mondet](http://seb.mondet.org).\
         \n\
         This website was proudly brought to you by a \
         [Ketrew workflow](https://github.com/smondet/build-docs-workflow).
         "
      ]
      |> String.concat ~sep:""
    in
    let index_md = (results#product#path // "index.md") in
    target "index-page"
      ~dependencies:[results]
      ~make:(make Program.(
          shf "echo %s > %s" Filename.(quote content)  index_md
          && shf "INDEX=%s OUTPUT_DIR=%s INPUT= oredoc" index_md
            results#product#path
        ))
  in
  let workflow_ancestor =
    target "build-all-docs"
      ~dependencies:(index_page :: documentations)
      ~make:(make_shell "echo 'SUCCESS'")
  in
  workflow_ancestor

let module_name_rex =
  "(^[A-Z]+[A-Za-z0-9]*_[A-Za-z0-9]*)|(^[A-Z]+$)"

let module_type_name_rex =
  "^[A-Z]+[A-Z0-9_]*$"

(*
let test_module_name =
  Re_posix.compile_pat module_name_rex
  |> (fun re ->
      let t s = printf "%S → %b\n" s (Re.execp re s) in
      t "Mp_dskljdsMsd_ds";
      t "_Mp_dskljdsMsd_ds";
      t "kldjfsd";
      t "KLDJFSD";
      t "kl_djfsd";
    )
*)

let please_dot_ml_doc_building ?(catch_more=[]) ?(more_files=[]) name =
  let modname = Oldschool_string.capitalize name in
  [
    `Do ["ocaml"; "please.ml"; "clean"];
    `Do ["ocaml"; "please.ml"; "build"];
    `Do ["ocaml"; "please.ml"; "build_doc"];
    `Do ["sh"; "-c"; sprintf 
           "API=_build/doc/ INPUT=%s.ml%s INDEX=README.md \
            CATCH_MODULE_PATHS='%s^%s:,' \
            TITLE_SUBSTITUTIONS='%s%s.ml:Literate Implementation' \
            TITLE_PREFIX='%s: ' oredoc"
           name
           (List.map more_files ~f:(fun (a, b) -> sprintf ",%s" a)
            |> String.concat ~sep:"")
           (List.map catch_more ~f:(fun (a, b) -> sprintf "%s:%s," a b)
            |> String.concat ~sep:"")
           modname
           (List.map more_files ~f:(fun (a, b) ->
                sprintf "%s:%s," (Filename.basename a) b)
            |> String.concat ~sep:"")
           name
           modname];
    `Get "_doc/";
  ]

let call_ocaml_doc ~packages ~title sources = [
  `Do ["mkdir"; "-p"; "_apidoc"];
  `Do ["sh"; "-c";
   sprintf
     "ocamlfind ocamldoc -rectypes -html -d _apidoc/ %s \
      -thread  -charset UTF-8 -t %S -keep-code -colorize-code \
      -sort -I _build/ %s"
     (List.map packages ~f:(sprintf "-package %s") |> String.concat ~sep:" ")
     title
     (List.map sources ~f:(sprintf "%s") |> String.concat ~sep:" ")
  ]
]

let add_prefix_catches l ~prefix = List.map l (fun m -> (m, prefix))

let projects = [
  project "nonstd"
    ~description:"Nano-library providing very few Core-like modules (like `List`, `Option`)"
    ~build_documentation:(fun _ ->
        let catch_more =
          add_prefix_catches ~prefix:"Nonstd."
            ["List"; "Option"; "Float"; "Int"; "Array"]
        in
        please_dot_ml_doc_building "nonstd" ~catch_more)
    ~repository:(`Bitbucket "smondet/nonstd");
  project "sosa"
    ~description:"The “Sane OCaml String API” library is a set of APIs (module types) \
                  that define what a string of characters should be, \
                  and a set of modules and functors that implement them"
    ~build_documentation:(fun _ ->
        please_dot_ml_doc_building "sosa"
          ~catch_more:[module_type_name_rex,"Sosa.";
                       "Make_output", "Sosa.BASIC_STRING."]
          ~more_files:["test/sosa_test.ml", "Tests & Benchmarks (`sosa_test.ml`)"]
      )
    ~repository:(`Bitbucket "smondet/sosa");
  project "docout"
    ~description:"The functor `Docout.Make_logger` creates a nice embedded \
                  DSL on top of the \
                  [smart-print](https://github.com/clarus/smart-print) library"
    ~build_documentation:(fun _ ->
        please_dot_ml_doc_building
          ~catch_more:[module_type_name_rex,"Docout.";]
          "docout")
    ~repository:(`Bitbucket "smondet/docout");
  project "pvem"
    ~description:"The \"Polymorphic Variants-based Error Monad\", \
                  `Pvem` (pronounce /pi:vɛm/), is a module providing \
                  simple handling of an error monad type based on \
                  polymorphic variants"
    ~build_documentation:(fun _ ->
        let catch_more = [module_type_name_rex, "Pvem.";
                          "ERROR_MONAD.[a-z]+", "Pvem." ] in
        let more_files = ["doc/implementation.md", "Implementation Notes"] in
        please_dot_ml_doc_building ~catch_more ~more_files "pvem")
    ~repository:(`Bitbucket "smondet/pvem");
  project "atd2cconv" 
    ~description:"The `atd2cconv` application compiles \
                  [ATD](https://github.com/mjambon/atd/blob/master/atd_ast.mli) \
                  descriptions to OCaml code which defines \
                  [CConv](https://github.com/c-cube/cconv) sources and sinks"
    ~build_documentation:(fun branch ->
      call_ocaml_doc ~packages:["atd"; "nonstd"; "smart_print"]
        ~title:"Atd2cconv API" ["src/lib/atd2cconv.mli"]
      @ [
        `Do ["sh"; "-c";  
             "API=_apidoc/ INPUT=src/app,src/lib INDEX=README.md \
              CATCH_MODULE_PATHS='Atd2cconv:' \
              TITLE_SUBSTITUTIONS='main.ml:Application Implementation, \
              atd2cconv.ml:Library Implementation' \
              TITLE_PREFIX='Atd2cconv: ' oredoc"];
        `Get ("_doc");
      ]
    )
    ~repository:(`Github "smondet/atd2cconv");
  project  "pvem_lwt_unix"
      ~description:"`Pvem_lwt_unix` provides a high-level API on top of \
                    `Lwt_unix`, with comprehensive error types"
      ~build_documentation:(fun _ ->
        let catch_more = [module_type_name_rex, "Pvem_lwt_unix."; ] in
        please_dot_ml_doc_building ~catch_more "pvem_lwt_unix")
    ~repository:(`Bitbucket "smondet/pvem_lwt_unix");
  project "ketrew"
    ~description:"Workflow Engine for complex computational experiments"
    ~repository:(`Github "hammerlab/ketrew")
    ~interesting_checkouts:["pbs_backend"; "long_running_factorization"]
    ~build_documentation:(fun branch -> [
          `Do ["bash"; "please.sh"; "clean"; "build"];
          `Do ["bash"; "please.sh"; "doc"];
          `Get (match branch with
            | "master" -> "_doc"
            | branch -> sprintf "_doc/%s" branch);
        ]);
  project "oredoc"
    ~description:"Build documentation websites for *some* OCaml projects"
    ~repository:(`Github "smondet/oredoc")
    ~build_documentation:(fun _ -> [
      `Do ["make"];
      `Do ["make"; "doc"];
      `Get "_doc/";
    ]);
]

let () =
  match Array.to_list Sys.argv with
  | exec :: "go" :: url :: auth_token :: work_dir ::  [] ->
    let override_configuration =
      let open Ketrew_configuration in
      client url ~token:auth_token
      |> create ~debug_level:2 in
    Ketrew.EDSL.(
      run (build_website
             ~work_dir ~host:(Host.parse "/tmp/KT") projects)
        ~override_configuration
    )
  | other -> printf "usage: %s go <URL> <TOKEN> <TMPDIR>\n%!" Sys.argv.(0)

