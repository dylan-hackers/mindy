#
# Spec file for Gwydion Dylan RPM.
#
# To use this spec file, copy it to /usr/src/redhat/SPECS. Get the
# latest source and documentation tarball from the FTP site and
# put them into /usr/src/redhat/SOURCES. Edit your local copy of
# this file, replacing:
#   * VERSION with a number of the form '2.3.1'. (2 occurances)
#   * libcX with either libc5 or libc6, depending on your Linux
#     distribution.
# Then type 'cd /usr/src/redhat/SPECS/; rpm -ba gwydion-dylan.spec`.

Name: gwydion-dylan
Summary: CMU's Gwydion Dylan development tools
Version: VERSION
Release: 1.libcX
Copyright: X-style
Group: Development/Languages/Dylan
Source0: ftp://berlin.ccc.de/pub/gd/src/gd-VERSION.tar.gz
Source1: ftp://berlin.ccc.de/pub/gd/doc/gd20-html.tar.gz
URL: http://www.gwydiondylan.org/
Packager: eric.kidd@pobox.com
BuildRoot: /tmp/gd-root
Prefix: /usr

%description
CMU's Gwydion Dylan provides d2c, a Dylan-to-C batch compiler. It produces
fairly efficient output but requires strange incantations to compile even
simple programs. Tools for interfacing with C are included. (The Mindy
bytecode interpreter is available as a separate package.) If you're really
excited about writing Dylan programs for Linux--and don't mind the
inconveniences of d2c--these are the tools to use. If you prefer a mature
development environment, try another language.

For more infomration, see the Gwydion Dylan maintainers' web page at
<http://www.gwydiondylan.org/>.


%package extras
Summary: Tools used for recompiling Gwydion Dylan
Requires: gwydion-dylan
Group: Development/Languages/Dylan
Prefix: /usr

%description extras
These tools are required to recompile d2c. They include a LISP to Dylan
translator, a parser generator, and various tools for maintaining the
build tree. This package also contains a few general-purpose development
utilities written in Dylan.


%package -n mindy
Summary: CMU's Gwydion Dylan interpreter
Group: Development/Languages/Dylan
Prefix: /usr

%description -n mindy
Mindy is a Dylan bytecode interpreter, originally written as part of CMU's
Gwydion Dylan project. It compiles faster than d2c and includes much better
debugging tools. Unfortunately, Mindy makes no attempt to run fast.

Documentation for Mindy can be found in the main gwydion-dylan package, or
on the web at <http://www.gwydiondylan.org/>.


%changelog
* Sun Aug 22 1999 Andreas Bogk <andreas@andreas.org>
  - add shared libraries to package

* Tue Jun 01 1999 Andreas Bogk <andreas@andreas.org>
  - added site-local directory according to FSSTND
  - changed the Dylan library location to reflect 2.3.1 changes

* Sat Jan 09 1999 Eric Kidd <eric.kidd@pobox.com>
  - Added a build root.
  - Simplified file list.
  - Edited description.
  - Deleted some commented out code.

* Thu Dec 29 1998 Eric Kidd <eric.kidd@pobox.com>
  - Added prefix for gwydion-dylan-extras and mindy.
  - Added mindycomp.1 to list of installed files.
  - Updated URLs.

* Thu Dec 24 1998 Eric Kidd <eric.kidd@pobox.com>
  - Added new man pages to RPMS.

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
%setup -n gd -a 1


%build
cd src/
# If our source tarball came out of CVS, make a configure script.
if [ ! -f configure ]; then
./autogen.sh --prefix=/usr --with-site-dylan-prefix=/usr/local
fi
# This does the wrong thing if d2c isn't in PATH.
./configure --prefix=/usr --with-site-dylan-prefix=/usr/local
make


%install
cd src/
make DESTDIR=$RPM_BUILD_ROOT install


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
%doc src/README src/INSTALL src/NEWS src/ONEWS.html src/CREDITS

# Our files, including some programs with dumb names.
/usr/bin/d2c
/usr/bin/melange
/usr/bin/gen-makefile
/usr/bin/mk-build-tree
/usr/bin/make-dylan-app
/usr/bin/make-dylan-lib
/usr/include/runtime.h
/usr/lib/dylan/VERSION/*/elisp/

# Our man pages (but see below for mindy.1).
/usr/man/man7/dylan.7
/usr/man/man7/gwydion.7
/usr/man/man1/d2c.1
/usr/man/man1/melange.1
/usr/man/man1/make-dylan-app.1
/usr/man/man4/platforms.descr.4

# Our libraries.
/usr/lib/dylan/VERSION/*/*.a
/usr/lib/dylan/VERSION/*/*.so
/usr/lib/dylan/VERSION/*/*.du


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

/usr/man/man1/mindy.1
/usr/man/man1/mindycomp.1

/usr/lib/dylan/VERSION/*/*.dbc
