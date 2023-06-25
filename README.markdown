# Welcome to aeson-pretty

This is a JSON pretty-printing Haskell library compatible with [aeson](http://hackage.haskell.org/package/aeson) as well as a command-line tool to improve readabilty of streams of JSON data.

The **library** provides a single function `encodePretty`. It is a drop-in replacement for aeson's `encode` function, producing JSON-ByteStrings for human readers.

The **command-line tool** reads JSON from stdin and writes prettified JSON to stdout. It also offers a complementary "compact"-mode, essentially the opposite of pretty-printing.


# Join in!

We are happy to receive bug reports, fixes, documentation enhancements, and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/informatikr/aeson-pretty/issues).

Master [git repository](http://github.com/informatikr/aeson-pretty):

* `git clone git://github.com/informatikr/aeson-pretty.git`

# Authors

This library is written and maintained by Falko Peters, <falko.peters@gmail.com>.
