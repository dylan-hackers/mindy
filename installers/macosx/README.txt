MacOS X Double-Clickable Installer For Gwydion Dylan.

INSTRUCTIONS.

Build gwydion, then get a copy of the installation in this directory

cd ../..
make DESTDIR=/tmp/foo install
cp /tmp/foo/usr/local ./installers/macosx

Open the installer file and set the correct version and date information. Check the license file as well. 
The build the installer, make a .dmg of it using a drag&drop Disk Image maker, and upload it to the Gwydion FTP site.

Rob Myers
rob@robmyers.org