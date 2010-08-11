#-----------------------------------------------------------------------------
# Debian stuff from Vegar Bønes
# requires: all debian-files are in 'debian_files' catalogue
#           update changelog for correct version
#           dpkg-buildpackage
#           devscripts
#           lintian
# generates targets: update-debian
#                    debian
#                    clean-debian
# usage: in Makefile.am: include m4/debian.mk
#-----------------------------------------------------------------------------

PKG_DIR = $(PACKAGE)-$(VERSION)
DEBIAN_DIR = $(PKG_DIR)/debian
DEBIAN_PACKAGE = $(PACKAGE)_$(VERSION)
ARCH = `dpkg-architecture -qDEB_HOST_ARCH_CPU`
DEBIAN_PACKAGE_NAME = `head -n1 $(top_srcdir)/debian_files/changelog | sed "s/ (/_/" | sed "s/).*//"`_$(ARCH).deb
DEBIAN_PACKAGE_VERSION = `head -n1 $(top_srcdir)/debian_files/changelog | sed "s/.*(\(.*\)).*/\1/"`
prepare-debian: dist clean-debian
	tar xvzf $(PKG_DIR).tar.gz
	mkdir -p $(DEBIAN_DIR)
	cp $(PKG_DIR).tar.gz $(DEBIAN_PACKAGE).orig.tar.gz

update-debian:
	rm -rf $(DEBIAN_DIR)/*
	cp -r $(top_srcdir)/debian_files/* $(DEBIAN_DIR)
	mv $(DEBIAN_DIR)/libfimex.install $(DEBIAN_DIR)/libfimex-$(VERSION)-0.install
	mv $(DEBIAN_DIR)/libfelt.install $(DEBIAN_DIR)/libfelt-$(VERSION)-0.install
	mv $(DEBIAN_DIR)/libfimex.preinst $(DEBIAN_DIR)/libfimex-$(VERSION)-0.preinst
	mv $(DEBIAN_DIR)/libfelt.preinst $(DEBIAN_DIR)/libfelt-$(VERSION)-0.preinst
	mv $(DEBIAN_DIR)/libfimex.shlibs $(DEBIAN_DIR)/libfimex-$(VERSION)-0.shlibs
	mv $(DEBIAN_DIR)/libfelt.shlibs $(DEBIAN_DIR)/libfelt-$(VERSION)-0.shlibs
	for i in $(DEBIAN_DIR)/control $(DEBIAN_DIR)/libfelt-$(VERSION)-0.shlibs $(DEBIAN_DIR)/libfimex-$(VERSION)-0.shlibs; do \
	   perl -pi -e "s/0\.19/$(VERSION)/g;" $$i; \
	done
	chmod 774 $(DEBIAN_DIR)/rules
	cd $(PKG_DIR) && dpkg-buildpackage -rfakeroot -us -uc -nc

lintian:
	lintian libfelt_$(DEBIAN_PACKAGE_VERSION)_$(ARCH).deb
	lintian libfelt-dev_$(DEBIAN_PACKAGE_VERSION)_$(ARCH).deb
	lintian libfimex_$(DEBIAN_PACKAGE_VERSION)_$(ARCH).deb
	lintian libfimex-dev_$(DEBIAN_PACKAGE_VERSION)_$(ARCH).deb
	lintian fimex-bin_$(DEBIAN_PACKAGE_VERSION)_$(ARCH).deb

debian: prepare-debian update-debian lintian

clean-debian:
	debclean
	rm -rf $(PKG_DIR) $(DEBIAN_PACKAGE)*
