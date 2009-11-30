#-----------------------------------------------------------------------------
# Debian stuff from Vegar Bønes
# requires: all debian-files are in 'debian_files' catalogue
#           update changelog for correct version
#           dpkg-buildpackage
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

prepare-debian: dist clean-debian
	tar xvzf $(PKG_DIR).tar.gz
	mkdir -p $(DEBIAN_DIR)
	cp $(PKG_DIR).tar.gz $(DEBIAN_PACKAGE).orig.tar.gz


#debian:	dist clean-debian 
#	tar xvzf $(PKG_DIR).tar.gz
#	mkdir -p $(DEBIAN_DIR)
#	cp $(PKG_DIR).tar.gz $(DEBIAN_PACKAGE).orig.tar.gz
#	rm -rf $(DEBIAN_DIR)/*
#	cp -r $(top_srcdir)/debian_files/* $(DEBIAN_DIR)
#	chmod 774 $(DEBIAN_DIR)/rules
#	cd $(PKG_DIR) && dpkg-buildpackage -rfakeroot -us -uc
#	lintian $(DEBIAN_PACKAGE_NAME)

update-debian:
	rm -rf $(DEBIAN_DIR)/*
	cp -r $(top_srcdir)/debian_files/* $(DEBIAN_DIR)
	chmod 774 $(DEBIAN_DIR)/rules
	cd $(PKG_DIR) && dpkg-buildpackage -rfakeroot -us -uc -nc
	lintian $(DEBIAN_PACKAGE_NAME)

debian: prepare-debian update-debian

clean-debian:
	debclean
	rm -rf $(PKG_DIR) $(DEBIAN_PACKAGE)*
