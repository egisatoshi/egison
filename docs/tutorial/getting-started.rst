===============
Getting Started
===============

Installation
============

See :doc:`../reference/install` for the install guide of the Egison interpreter.

How to use the Egison interpreter
=================================

Interactive mode
----------------

Just typing ``egison`` starts the REPL (read-eval-print loop) of the interpreter.

::

   $ egison

You can load definitions from a file by passing ``-l`` option.
The following example is equivalent to starting the REPL and then executing ``loadFile "name-of-file-to-load.egi"``.

::

   $ egison -l name-of-file-to-load.egi


Executing a program in files
----------------------------

You can write any expressions at the top level of program.
With ``-t`` option, the interpreter prints out the evaluation results of each expression written at the top level.

Note that the statements (such as definitions and ``loadFile``) are not expressions, and hence not printed.

::

   $ cat name-of-file-to-test.egi
   def x := 1
   x + 2
   "This is the third line"

   $ egison -t name-of-file-to-test.egi
   3
   "This is the third line"

Finally, simply passing a file name to ``egison`` executes the ``main`` function defined in the file.
The ``main`` should be a unary (1 argument) function that returns an IO function.
Command line arguments are given to the ``main`` function as a collection of string.

::

   $ cat name-of-file-to-run.egi
   def main args :=
      print "Hello, world!"

   $ egison name-of-file-to-run.egi
   Hello, world!
