Name:          asgl
Version:       1.3
Release:       1
License:       Copyright
Summary:       PostScript plot creator
Vendor:        Andrej Sali
URL:           http://salilab.org/asgl/asgl.html
Packager:      Ben Webb <ben@salilab.org>
Group:         Applications/Engineering
Source0:       ftp://salilab.org/asgl/asgl.tar.gz
BuildRoot:     %{_tmppath}/%{name}-%{version}-root
BuildRequires: /usr/bin/latex, /usr/bin/latex2html, /usr/bin/perl, /usr/bin/dvips

%description
ASGL program produces a PostScript or Encapsulated PostScript file
that can contain scatter plots, line plots, histograms, 2D density
plots, and/or bond-and-stick plots of molecules.  Input are data
files and a TOP program guiding the creation of a plot. TOP is an
interpreter of ASGL commands similar to Fortran.

%prep
%setup -n asgl

%build
topdir=${RPM_BUILD_DIR}/asgl
(cd src; ASGL_EXECUTABLE_TYPE=g77 make opts)
mkdir exe
cp src/top.ini src/psgl.ini data/egromos.vdw data/egromos.brk \
   data/3d.lib scripts/__asgl.top src/asgl_g77 exe/
ln -s asgl_g77 exe/asgl
g77 -o exe/collect doc/collect.f
BIN_ASGL="${topdir}/exe"
LIB_ASGL=${BIN_ASGL}
export BIN_ASGL LIB_ASGL
PATH="$PATH:${BIN_ASGL}"
(cd examples && make)
(cd doc && make ps)

%install
bindir=${RPM_BUILD_ROOT}/usr/bin
libdir=${RPM_BUILD_ROOT}/usr/lib/asgl
install -d ${bindir}
install -d ${libdir}
install asgl.script ${bindir}/asgl
rm -f exe/collect
install exe/* ${libdir}

%clean
rm -rf ${RPM_BUILD_ROOT}

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
* Wed Oct 08 2003 Ben Webb <ben@salilab.org>
- Rebuild with most recent ASGL version

* Tue Apr 30 2002 Ben Webb <ben@bellatrix.pcl.ox.ac.uk>
- Initial build
