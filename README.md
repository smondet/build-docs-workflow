Build Websites With Ketrew
==========================

This script uses Ketrew to build the documentation of a bunch of projects.

Type-check:

    ocaml build_all_docs.ml

Run:

    ocaml build_all_docs.ml go <URL> <TOKEN> <TMPDIR>

The script expects a Ketrew server running at `<URL>` and accepting `<TOKEN>`
as authentication token.

`<TMPDIR>` is the *work* directory.

For example, if Ketrew is running locally, with its “test” configuration:

    ocaml build_all_docs.ml go https://localhost:8443 netok /some/tmp/

And go to see `file:///some/tmp/results/index.html` when/if it succeeds.
 

MacOSX Note
-----------

This problem is not specific to this script; it's more related to OCamldoc's
file naming scheme and MacOSX case-insensitive file-system:

If two module-items have names which differ only by case, OCamldoc-generated
HTML files will be erasing each other (for example when a submodule and a
module type follow ML conventions).

**The Solution:**

- Create a case-insensitive disk image (see for example
[here](https://coderwall.com/p/mgi8ja)).
- Use it as `<TMPDIR>`.


