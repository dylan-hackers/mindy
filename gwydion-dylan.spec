#
# Spec file for Gwydion Dylan RPM.
#
# Flaws in this spec file:
#  * This package is not relocatable until the compiler is fixed.
#  * This package does not support a build root until we clean up
#    Makegen to allow an install prefix.
#
# To use this spec file, copy it to /usr/src/redhat/SPECS. Get the
# latest source and documentation tarball from the FTP site and
# put them into /usr/src/redhat/SOURCES. Edit your local copy of
# this file, replacing:
#   * VERSION with a number of the form '2.1.2'. (2 occurances)
#   * libcX with either libc5 or libc6, depending on your Linux
#     distribution.
# Then type 'cd /usr/src/redhat/SPECS/; rpm -ba gwydion-dylan.spec`.

Name: gwydion-dylan
Summary: CMU's Gwydion Dylan development tools
Version: VERSION
Release: 1-libcX
Copyright: X-style
Group: Development/Languages/Dylan
Source0: ftp://berlin.ccc.de/pub/gd/v2.1/src/gd-VERSION.tar.gz
Source1: ftp://berlin.ccc.de/pub/gd/doc/gd20-html.tar.gz
URL: http://www.randomhacks.com/dylan/
Packager: eric.kidd@pobox.com
Prefix: /usr

%description
CMU's Gwydion Dylan provides d2c, a Dylan-to-C batch compiler. It produces
fairly efficient output but requires strange incantations to compile even
simple programs. Tools for interfacing with C and possibly C++ are
included. (The Mindy bytecode interpreter is available as a separate
package.)  If you're really excited about writing Dylan programs for
Linux--and don't mind the inconveniences of d2c--these are the tools to
use. If you prefer a mature development environment, try another language.

For more infomration, see the Gwydion Dylan maintainers' web page at
<http://www.randomhacks.com/dylan/>.


%package extras
Summary: Tools used for recompiling Gwydion Dylan
Requires: gwydion-dylan
Group: Development/Languages/Dylan

%description extras
These tools are required to recompile d2c. They include a LISP to Dylan
translator, a parser generator, and various tools for maintaining the
build tree. This package also contains a few general-purpose development
utilities written in Dylan.


%package -n mindy
Summary: CMU's Gwydion Dylan interpreter
Group: Development/Languages/Dylan

%description -n mindy
Mindy is a Dylan bytecode interpreter, originally written as part of CMU's
Gwydion Dylan project. It compiles faster than d2c and includes much better
debugging tools. Unfortunately, Mindy makes no attempt to run fast.

Documentation for Mindy can be found in the main gwydion-dylan package, or
on the web at <http://www.randomhacks.com/dylan/>.


%changelog

* Wed Nov 24 1998 Eric Kidd <eric.kidd@pobox.com>
  - Commented out LD_LIBRARY_PATH warning for relocated packages.

* Sat Nov 21 1998 Eric Kidd <eric.kidd@pobox.com>
  - Added primitive relocation support for Gwydion and Mindy.
  - Commented out code to check for parsergen, because Andreas says
    we can build with only a copy of d2c now.
  - Updated some other comments.

* Sat Aug 15 1998 Eric Kidd <eric.kidd@pobox.com>
  - Final cleanup for 2.1 snapshots
  - Removed handy macros for compatibility with
    SuSE RPM.

* Sat Aug 15 1998 Eric Kidd <eric.kidd@pobox.com>
  - Updated for Gwdyion 2.1 snapshot
  - Added in new website and FTP locations
  - Added new files

* Wed Apr 21 1998 Eric Kidd <eric.kidd@pobox.com>
  - Broke Mindy out into another package.
  - Moved everything into Development/Languages/Dylan

* Tue Apr 21 1998 Eric Kidd <eric.kidd@pobox.com>
  - Parameterized Gwydion Dylan URL.
  - Added a changelog section.
  - Made gwydion-dylan-extras into a separate package.


%prep
if [ ! -x /usr/bin/d2c -a ! -x /usr/local/bin/d2c ]; then
  # Fail now. (./configure can probably take care of this by itself now,
  # but this check remains for now)
  echo "d2c prep: Must have /usr/bin/d2c installed to recompile d2c."
  echo "d2c prep: Trying installing the Gwydion Dylan RPMs."
  exit 1
fi
# We should be able to build parsergen from source now.
#if [ ! -x /usr/bin/parsergen -a ! -x /usr/local/bin/parsergen ]; then
#  # Look for the other utilities need to bootstrap d2c.
#  echo "d2c prep: Must have gwydion-dylan-extras installed to"
#  echo "d2c prep: recompile d2c."
#  exit 1
#fi
%setup -n gd -a 1


%build
cd src/
# If our source tarball came out of CVS, make a configure script.
if [ ! -f configure ]; then
  autoconf
fi
# This does the wrong thing if d2c isn't in PATH.
./configure --prefix=/usr
make


%install
cd src/
make install


%post
# We could automate all of this if the package is installed as root, but
# these instructions will always need to remain for non-root installs.
if test "x$RPM_INSTALL_PREFIX" != "x/usr"; then
    echo "Gwydion Dylan was installed someplace other than /usr, so additional"
    echo "setup will be required:"
    echo "  * Set DYLANDIR to $RPM_INSTALL_PREFIX"
    echo "  * Make sure your PATH contains $RPM_INSTALL_PREFIX/bin"
#    echo "  * Either add $RPM_INSTALL_PREFIX/lib/dylan to LD_LIBRARY_PATH, or"
#    echo "    add it to /etc/ld.so.conf and rerun /sbin/ldconfig"
fi


%post -n mindy
# We could automate all of this if the package is installed as root, but
# these instructions will always need to remain for non-root installs.
if test "x$RPM_INSTALL_PREFIX" != "x/usr"; then
    echo "Mindy was installed someplace other than /usr, so additional"
    echo "setup will be required:"
    echo "  * Set DYLANDIR to $RPM_INSTALL_PREFIX"
    echo "  * Make sure your PATH contains $RPM_INSTALL_PREFIX/bin"
fi


%files

# This is the only non-relocatable file.
%config /usr/etc/platforms.descr

# Our documentation
%doc docs/

# Our well-behaved files, including some programs with dumb names.
/usr/bin/d2c
/usr/bin/melange
/usr/bin/gen-makefile
/usr/bin/mk-build-tree
/usr/bin/make-dylan-app
/usr/include/runtime.h
/usr/lib/dylan/elisp/

# Random libraries and other cruft. Be careful to keep this up-to-date.
/usr/lib/dylan/libruntime.a
/usr/lib/dylan/libgc.a
/usr/lib/dylan/dylan.lib.du
/usr/lib/dylan/libdylan.a
/usr/lib/dylan/melange-support.lib.du
/usr/lib/dylan/libmelange.a
/usr/lib/dylan/streams.lib.du
/usr/lib/dylan/libstreams.a
/usr/lib/dylan/standard-io.lib.du
/usr/lib/dylan/libstdio.a
/usr/lib/dylan/print.lib.du
/usr/lib/dylan/libprint.a
/usr/lib/dylan/format.lib.du
/usr/lib/dylan/libformat.a
/usr/lib/dylan/collection-extensions.lib.du
/usr/lib/dylan/libcollext.a
/usr/lib/dylan/table-extensions.lib.du
/usr/lib/dylan/libtableext.a
/usr/lib/dylan/string-extensions.lib.du
/usr/lib/dylan/libstringext.a
/usr/lib/dylan/regular-expressions.lib.du
/usr/lib/dylan/libregexp.a
/usr/lib/dylan/format-out.lib.du
/usr/lib/dylan/libformatout.a
/usr/lib/dylan/matrix.lib.du
/usr/lib/dylan/libmatrix.a
/usr/lib/dylan/time.lib.du
/usr/lib/dylan/libtime.a
/usr/lib/dylan/stream-extensions.lib.du
/usr/lib/dylan/libstreamext.a
/usr/lib/dylan/transcendental.lib.du
/usr/lib/dylan/libtranscendental.a
/usr/lib/dylan/random.lib.du
/usr/lib/dylan/librandom.a


%files extras
/usr/bin/line-count
/usr/bin/lisp2dylan
/usr/bin/make-exports
/usr/bin/mk-rcs-links
/usr/bin/parsergen
/usr/bin/synopsis
/usr/bin/versioner


%files -n mindy
/usr/bin/mindy
/usr/bin/mindycomp
/usr/bin/mindyexec

/usr/lib/dylan/dylan-lib.dbc
/usr/lib/dylan/random-lib.dbc
/usr/lib/dylan/tk-lib.dbc
/usr/lib/dylan/inspector-base-lib.dbc
/usr/lib/dylan/text-inspector-lib.dbc
/usr/lib/dylan/x-inspector-lib.dbc
/usr/lib/dylan/streams-lib.dbc
/usr/lib/dylan/standard-io-lib.dbc
/usr/lib/dylan/print-lib.dbc
/usr/lib/dylan/format-lib.dbc
/usr/lib/dylan/collection-extensions-lib.dbc
/usr/lib/dylan/table-extensions-lib.dbc
/usr/lib/dylan/string-extensions-lib.dbc
/usr/lib/dylan/regular-expressions-lib.dbc
/usr/lib/dylan/format-out-lib.dbc
/usr/lib/dylan/matrix-lib.dbc
/usr/lib/dylan/time-lib.dbc
/usr/lib/dylan/stream-extensions-lib.dbc
/usr/lib/dylan/transcendental-lib.dbc
