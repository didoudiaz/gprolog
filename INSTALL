			INSTALLATION PROCEDURE
			----------------------



See file README for a list of currently available ports:

The file PROBLEMS contains a list of known problems/limitations depending on 
the architecture. Please consult it before reporting a problem.



Installing the source distribution
**********************************

1) Introduction
---------------

** Win32 preamble **
	To compile GNU-Prolog under win32 see file src/WINDOWS.

The following tools are required to compile and install the source package:
   gcc, as, ranlib (if needed), sh, mkdir, cp, rm, sed, test,...

The installation process is as follows:

   cd src				go to source directory
   ./configure [OPTIONS]		configure the system
   make					compile locally the package
   make install or (make install-strip)	install the package

you can check the result of the local compilation using:

   make check


2) Installation directories
---------------------------

Directories used for the installation are as follows:

   INSTALL_DIR the root directory for the core package, contains:
      INSTALL_DIR/bin     all binaries (compiler, top-level,...)
      INSTALL_DIR/lib     all libraries and objects
      INSTALL_DIR/include header files needed to write foreign C code

These other directories are optional:

   LINKS_DIR the directory for links to binaries of INSTALL_DIR/bin

   DOC_DIR the directory for the documentation (LaTeX, DVI, PostScript,...)

   HTML_DIR the directory for the HTML documentation

   EXAMPLES_DIR the directory for the examples, contains:
      EXAMPLES_DIR/ExamplsPl  some classical Prolog examples
      EXAMPLES_DIR/ExamplesFD≈ some examples using FD constraint solving

Default directory values and associated configuration options are as follows:

   The value of INSTALL_DIR is as follows:

      - PREFIX/gprolog-VERSION_NUMBER (this is the default)
	the default value of PREFIX is /usr/local but can be explicitly
        specified using --prefix=PREFIX

      - another location can be specified using --with-install-dir=INSTALL_DIR

      - the source distribution directory (i.e. where reside this INSTALL file)
        This in-place installation can be specified using --prefix=in-place or
        --with-install-dir=in-place

   The value of LINKS_DIR is as follows:

      - EPREFIX/bin. The value of EPREFIX is the same as PREFIX but can be
        explicitly specified using --exec-prefix=EPREFIX

      - another location can be specified --with-links-dir=LINKS_DIR

      - To prevent the installation of the links use --without-links-dir
        (this is the default when doing an in-place installation).

   The default value of DOC_DIR is INSTALL_DIR/doc
   another location can be specified using --with-doc-dir=DOC_DIR
   To prevent the installation of documentation --without-doc-dir
   (this is the default when doing an in-place installation).

   The default value of HTML_DIR is DOC_DIR/Html
   another location can be specified using --with-html-dir=HTML_DIR
   To prevent the installation of the HTML documentation --without-html-dir
   (this is the default when doing an in-place installation).

   The default value of EXAMPLES_DIR is INSTALL_DIR
   another location can be specified using --with-examples-dir=EXAMPLES_DIR
   To prevent the installation of the examples --without-examples-dir
   (this is the default when doing an in-place installation).


The configure command displays the value of INSTALL_DIR, LINKS_DIR, DOC_DIR,
HTML_DIR and EXAMPLES_DIR.

To summarize, by default the whole package (+ documentation + HTML +
examples) is installed in /usr/local/gprolog-xxx and linked files are installed
in /usr/local/bin.



3) Configuration
----------------

GNU Prolog uses autoconf. To configure the package:

   ./configure [OPTIONS]

This script attempts to guess correct values for various system-dependent
variables used during compilation. For more detail about autoconf refer to
src/AUTOCONF-INFO (try also './configure --help'). The GNU-Prolog specific
options are:

Options to control the installation directory:

   --with-install-dir=INSTALL_DIR  specify INSTALL_DIR
   --prefix=PREFIX                 specify PREFIX (INSTALL_DIR=PREFIX/gprolog-xxx)
   --prefix=in-place               specify an in-place installation

   Default: --prefix=/usr/local

Options to control the location of links to binaries:

   --with-links-dir=LINKS_DIR      specify LINKS_DIR
   --without-links-dir             do not create link to binaries
   --exec-prefix=EPREFIX           specify EPREFIX (LINKS_DIR=EPREFIX/bin)

   Default: --exec-prefix=PREFIX
            (links are not installed for an in-place installation).

Options to control the location of other components:

  --with-doc-dir=DOC_DIR           specify DOC_DIR
  --without-doc-dir                do not install the documentation

  --with-html-dir=HTML_DIR         specify HTML_DIR
  --without-html-dir               do not install the HTML documentation

  --with-examples-dir=EXAMPLES_DIR specify EXAMPLESS_DIR
  --without-examples-dir           do not install the examples

   Defaults: INSTALL_DIR/doc for DOC_DIR, DOC_DIR/Html for HTML_DIR
             INSTALL_DIR for EXAMPLES_DIR
             (these components are not installed for an in-place installation).

Options to control C compiler optimization flags:

   --with-msvc                use MSVC++ compiler under Win32 (else use gcc)

   --without-c-flags          do not use any optimization flag
   --with-c-flags             use default C optimization flags
   --with-c-flags=CFLAGS      use CFLAGS (instead of default optimization flags)
   --with-c-flags=debug       use C debug flags (e.g. '-g -Wall' for gcc)

   Default: --with-c-flags

Options to control GNU features to include/exclude:

   --disable-regs             do not use machine registers to optimize speed
   --enable-ebp               use the ebp register on ix86 machines
   --disable-fast-call        do not use fast call mechanism for ix86 processors
   --disable-linedit          do not include line editor facility
   --disable-piped-consult    do not pipe stdin of pl2wam when consult/1
   --disable-sockets          do not include sockets facility
   --disable-fd-solver        do not include the finite domain constraint solver
   --disable-gui-console      do not use a GUI console (only with MSVC++ or MinGW)
   --disable-htmlhelp         do not use HtmlHelp in the GUI Console
   --enable-htmlhelp[=static] use HtmlHelp statically  linked (default)
   --enable-htmlhelp=dynamic  use HtmlHelp dynamically linked

   Default: all features are included.


Some examples of using configure:

To configure the package for an installation in the default directory
/usr/local and links to binaries in /usr/local/bin:

   ./configure

To configure the package for an installation in the home directory with
linked files in ~/bin/i586 use

   ./configure --prefix=$HOME --with-links-dir=$HOME/bin/i586

To configure the package for an in-place installation:

   ./configure --prefix=in-place


4) Compiling the package locally
--------------------------------

To locally compile the package:

   make


5) Installing the package
-------------------------

To install the package according to options given to ./configure (see 2):

   make install

You can either install stripped versions of the binaries (whose size is then
reduced):

   make install-strip

It is possible to re-run './configure' to change the value of some
installation directories (see 2) after the local compilation (i.e. the
compilation will not be done again).


6) Cleaning
-----------

To remove installed files (remove the content of INSTALL_DIR):

   make uninstall

To clean up the local compilation (does not erase configuration files):

   make clean

To fully clean up the local compilation:

   make distclean



Setting up environment variables
********************************

To be able to execute GNU Prolog from anywhere the directory LINKS_DIR should
be a part of your PATH environment variable (generally this directory is
already in the PATH variable). However, if no links have been created
(either --without-links-dir has been specified or in case of default in-place
installation) you should add the directory INSTALL_DIR/bin to your PATH
variable. This can be done as follows (let us suppose INSTALL_DIR is
/usr/local/gprolog-xxx):

under sh/bash:  PATH=$PATH:/usr/local/gprolog-xxx/bin; export PATH
under csh/tcsh: setenv PATH ${PATH}:/usr/local/gprolog-xxx/bin

GNU Prolog needs to know the value of INSTALL_DIR (to locate its
libraries). To do this, it uses its own path at execution-time, expanding
symbolic links. So you should not move or copy the executables, but you can
create links to them (as done by the installation procedure in LINKS_DIR).
However, to prevent this case, GNU Prolog first consults the value of the
PL_PATH environment variable. If it is defined GNU Prolog uses this path. If
you want to define it, simply set it to the value of INSTALL_DIR as follows:

under sh/bash:  PL_PATH=/usr/local/gprolog-xxx; export PL_PATH
under csh/tcsh: setenv PL_PATH /usr/local/gprolog-xxx

To summarize, by default you can avoid to define PATH and PL_PATH. If you 
need to defines these variables it is a good idea to put them in your shell
start-up file ($HOME/.bashrc / .cshrc / .tcshrc depending on the used shell).



Problems
********

See file PROBLEMS for more information on architecture-dependent known
problems.

If your installation does not work (compilation is ok but when running the
system fails) you can try to recompile the whole system with --disable-regs
(after make distclean).

If this fails, try to recompile the whole system with --with-c-flags=debug and --disable-regs

If this installation works with -O and not -O2 (or higher) it can be due to
strict-aliasing. Try compiling with --with-c-flags='-O2 -fno-strict-aliasing'.

Finally you can use the GNU Prolog mailing lists to ask for help or to report
a bug/problem.
