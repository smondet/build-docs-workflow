Build Websites With Ketrew
==========================

This script uses Ketrew to build the documentation of a bunch of projects.

Type-check:

    ocaml build_all_docs.ml

Run:

    ocaml build_all_docs.ml go <URL> <TOKEN>

For now, this works with the `dev` branch of Ketrew, expecting a server to be
running at `<URL>` and accepting `<TOKEN>` as authentication token.

For example, if Ketrew is running locally, with its “test” configuration:

    ocaml build_all_docs.ml go https://localhost:8443 netok

And go to see <file:///tmp/websites/results/index.html> when/if it succeeds.
 

