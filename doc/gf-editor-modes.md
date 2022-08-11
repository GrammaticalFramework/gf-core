
We collect GF modes for various editors on this page. Contributions are
welcome!

## Emacs 

[gf.el](https://github.com/GrammaticalFramework/gf-emacs-mode) by Johan
Bockgård provides syntax highlighting and automatic indentation and
lets you run the GF Shell in an emacs buffer.  See installation
instructions inside.

## Atom 

[language-gf](https://atom.io/packages/language-gf), by John J. Camilleri

## Visual Studio Code 

 * [Grammatical Framework Language Server](https://marketplace.visualstudio.com/items?itemName=anka-213.gf-vscode) by Andreas Källberg.
This provides syntax highlighting and a client for the Grammatical Framework language server. Follow the installation instructions in the link.
 * [Grammatical Framework](https://marketplace.visualstudio.com/items?itemName=GrammaticalFramework.gf-vscode) is a simpler extension
without any external dependencies which provides only syntax highlighting.

==Eclipse==

[GF Eclipse Plugin](https://github.com/GrammaticalFramework/gf-eclipse-plugin/), by John J. Camilleri

==Gedit==

By John J. Camilleri

Copy the file below to
`~/.local/share/gtksourceview-3.0/language-specs/gf.lang` (under Ubuntu).

 * [gf.lang](../src/tools/gf.lang)

Some helpful notes/links:

 * The code is based heavily on the `haskell.lang` file which I found in
`/usr/share/gtksourceview-2.0/language-specs/haskell.lang`.
 * Ruslan Osmanov recommends
[registering your file extension as its own MIME type](http://osmanov-dev-notes.blogspot.com/2011/04/how-to-add-new-highlight-mode-in-gedit.html)
(see also [here](https://help.ubuntu.com/community/AddingMimeTypes)),
however on my system the `.gf` extension was already registered
as a generic font (`application/x-tex-gf`) and I didn't want to risk
messing any of that up.
 * This is a quick 5-minute job and might require some tweaking.
[The GtkSourceView language definition tutorial](http://developer.gnome.org/gtksourceview/stable/lang-tutorial.html)
is the place to start looking.
 * Contributions are welcome!

## Geany 

By John J. Camilleri

[Custom filetype](http://www.geany.org/manual/dev/index.html#custom-filetypes)
config files for syntax highlighting in [Geany](http://www.geany.org/).

Copy one of the files below to `/usr/share/geany/filetypes.GF.conf`
(under Ubuntu). You will need to manually create the file.

 * [light-filetypes.GF.conf](../src/tools/light-filetypes.GF.conf)
 * [dark-filetypes.GF.conf](../src/tools/dark-filetypes.GF.conf)

You will also need to edit the `filetype_extensions.conf` file and add the
following line somewhere:

    GF=*.gf

## Vim 

[vim-gf](https://github.com/gdetrez/vim-gf)

