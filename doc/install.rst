Installation
============

Installation in the field will be via binary, USB "kick-start" flash drives.

The steps below are for developers interested in installing this software on
their own computers.

1. Download and install debian squeeze (6.0.4) from netinst image. The iso
   image is available from::

    http://cdimage.debian.org/debian-cd/6.0.3/amd64/iso-cd/debian-6.0.3-amd64-netinst.iso

2. Install git-core and sudo (as root user)::

    # apt-get install git-core sudo
    # git clone git@github.com:MITHaystackObservatory/Mark6.git
    # cd Mark6
    # export VDAS_ROOT=/home/<username>/Mark6
    # export VDAS_USER=<username>
    # ./setup -b
    # ./setup -p

3. Add the following line to /etc/modules::

    pf_ring transparent_mode=0 min_num_slots=16384 enable_ip_defrag=1

4. Reboot::

    # reboot

5. Add the following to /etc/apt/sources.list::

    deb http://debian.lcs.mit.edu/debian/ squeeze main non-free contrib
    deb-src http://debian.lcs.mit.edu/debian/ squeeze main non-free contrib
    deb http://hwraid.le-vert.net/debian squeeze main

6. Update apt-get and install RAID packages::

    # apt-get update
    # apt-get install megacli megactl lshw

7. Setup the build environment::

    # ./setup --build
    # ./setup --install

8. After installation, add the following to the .bashrc of the ${VDAS_USER}::

    export MANPATH=/opt/mit/mark6/man:${MANPATH}
    export PATH=/opt/mit/mark6/bin:${PATH}
