Name:          asgl
Version:       1.3.2
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
(cd src && ASGL_EXECUTABLE_TYPE=gfortran make opt)
(cd doc && make ps)

%install
bindir=${RPM_BUILD_ROOT}/usr/bin
libdir=${RPM_BUILD_ROOT}/usr/lib/asgl
install -d ${bindir}
install -d ${libdir}
install rpm/asgl.script ${bindir}/asgl
install data/* src/asgl_gfortran ${libdir}

%clean
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
%doc doc/manual.ps
/usr/bin/asgl
%dir /usr/lib/asgl
/usr/lib/asgl/asgl_gfortran
/usr/lib/asgl/__asgl.top
/usr/lib/asgl/3d.lib
/usr/lib/asgl/egromos.brk
/usr/lib/asgl/egromos.vdw
/usr/lib/asgl/psgl.ini
/usr/lib/asgl/top.ini

%changelog
* Thu Nov 09 2006 Ben Webb <ben@salilab.org>    1.3.2
- Rebuild with most recent ASGL version

* Thu Oct 09 2003 Ben Webb <ben@salilab.org>    1.3.1
- Rebuild with most recent ASGL version

* Tue Apr 30 2002 Ben Webb <ben@bellatrix.pcl.ox.ac.uk>
- Initial build
