Name:          asgl
Version:       1.3.1
Release:       1
License:       Copyright
Summary:       PostScript plot creator
Vendor:        Andrej Sali
URL:           http://salilab.org/asgl/
Packager:      Ben Webb <ben@salilab.org>
Group:         Applications/Engineering
Source0:       ftp://salilab.org/asgl/%{name}-%{version}.tar.gz
BuildRoot:     %{_tmppath}/%{name}-%{version}-root
BuildRequires: /usr/bin/latex, /usr/bin/latex2html, /usr/bin/perl, /usr/bin/dvips

%description
The ASGL program produces a PostScript or Encapsulated PostScript file
that can contain scatter plots, line plots, histograms, 2D density
plots, and/or bond-and-stick plots of molecules.  Input are data
files and a TOP program guiding the creation of a plot. TOP is an
interpreter of ASGL commands similar to Fortran.

%prep
%setup

%build
topdir=${RPM_BUILD_DIR}/%{name}-%{version}
(cd src; ASGL_EXECUTABLE_TYPE=g77 make opt)
mkdir exe
cp src/top.ini src/psgl.ini data/egromos.vdw data/egromos.brk \
   data/3d.lib scripts/__asgl.top src/asgl_g77 exe/
ln -s asgl_g77 exe/asgl
BIN_ASGL="${topdir}/exe"
LIB_ASGL=${BIN_ASGL}
export BIN_ASGL LIB_ASGL
PATH="${PATH}:${BIN_ASGL}"
(cd doc && make ps)

%install
bindir=${RPM_BUILD_ROOT}/usr/bin
libdir=${RPM_BUILD_ROOT}/usr/lib/asgl
install -d ${bindir}
install -d ${libdir}
install rpm/asgl.script ${bindir}/asgl
rm -f exe/asgl
install exe/* ${libdir}

%clean
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
%doc doc/manual.ps
/usr/bin/asgl
%dir /usr/lib/asgl
/usr/lib/asgl/asgl_g77
/usr/lib/asgl/__asgl.top
/usr/lib/asgl/3d.lib
/usr/lib/asgl/egromos.brk
/usr/lib/asgl/egromos.vdw
/usr/lib/asgl/psgl.ini
/usr/lib/asgl/top.ini

%changelog
* Thu Oct 09 2003 Ben Webb <ben@salilab.org>
- Rebuild with most recent ASGL version

* Tue Apr 30 2002 Ben Webb <ben@bellatrix.pcl.ox.ac.uk>
- Initial build
