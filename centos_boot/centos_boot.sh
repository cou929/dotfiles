#! /bin/sh

# Bootstrap script for CentOS 5.x
#
# USAGE:
# # sh centos_boot.sh
#
# REFERENCES
# - http://tanaka.sakura.ad.jp/archives/001065.html
# - http://www.iex3.info/archives/1008
#
# TODO
# - iptables

USER=kosei
EMAIL=cou929@gmail.com
FILEROOT=/root/init

# expand files
echo "== Expand files =="
tar -zxvf init.tar.gz

# update sshd_config
echo "== update sshd_config =="
cp /etc/ssh/sshd_config /etc/ssh/sshd_config.orig
cp ${FILEROOT}/sshd_config /etc/ssh/sshd_config

# update inittab
echo "== update inittab =="
cp /etc/inittab /etc/inittab.orig
cp ${FILEROOT}/inittab /etc/inittab

# update pam.d/su
echo "== update pam.d/su =="
cp /etc/pam.d/su /etc/pam.d/su.orig
cp ${FILEROOT}/su /etc/pam.d/su

# stop unused daemons
echo "== stop unused daemons =="
chkconfig acpid off
chkconfig auditd off
chkconfig autofs off
chkconfig avahi-daemon off
chkconfig bluetooth off
chkconfig cups off
chkconfig firstboot off
chkconfig gpm off
chkconfig haldaemon off
chkconfig hidd off
chkconfig isdn off
chkconfig kudzu off
chkconfig lvm2-monitor off
chkconfig mcstrans off
chkconfig mdmonitor off
chkconfig messagebus off
chkconfig netfs off
chkconfig nfslock off
chkconfig pcscd off
chkconfig portmap off
chkconfig rawdevices off
chkconfig restorecond off
chkconfig rpcgssd off
chkconfig rpcidmapd off
chkconfig smartd off
chkconfig xfs off
chkconfig yum-updatesd off

# change root password
echo "== change root passwd =="
echo 'input root password'
passwd

# remove yum-updatesd, update,  and add yum-cron
echo "== remove yum-updatesd, update,  and add yum-cron =="
/etc/rc.d/init.d/yum-updatesd stop
yum -y remove yum-updatesd

yum -y update

yum -y install yum-cron
/etc/rc.d/init.d/yum-cron start
chkconfig yum-cron on

# install zsh
echo "== install zsh =="
yum -y install zsh

# add new user
echo "== add new user =="
useradd ${USER}
echo "input password of ${USER}"
passwd ${USER}
usermod -G wheel ${USER}

# change login shell
echo "== change login shell =="
chsh ${USER} -s /bin/zsh

# change root email
echo "== change root email =="
sed -i '/^root:/d' /etc/aliases
echo "root: ${EMAIL}" >> /etc/aliases
newaliases

# visudo
echo "== visudo =="
echo "visudo"
visudo

# set authorized_keys
echo "== set authorized_keys =="
mkdir -p /root/.ssh
mkdir -p /home/${USER}/.ssh
cp ${FILEROOT}/authorized_keys /root/.ssh/
cp ${FILEROOT}/authorized_keys /${USER}/.ssh/

# set dotfiles
echo "== set dotfiles =="
cp ${FILEROOT}/.zshrc /home/${USER}/

# setup rpmforge repo
echo "== setup rpmforge repo =="
yum -y install yum-priorities
cp /etc/yum.repos.d/CentOS-Base.repo /etc/yum.repos.d/CentOS-Base.repo.orig
cp ${FILEROOT}/CentOS-Base.repo /etc/yum.repos.d/CentOS-Base.repo
wget http://apt.sw.be/RPM-GPG-KEY.dag.txt
rpm --import RPM-GPG-KEY.dag.txt
rm -f RPM-GPG-KEY.dag.txt
wget http://packages.sw.be/rpmforge-release/rpmforge-release-0.5.2-2.el5.rf.i386.rpm
rpm -ivh rpmforge-release-0.5.2-2.el5.rf.i386.rpm
yum -y update rpmforge-release

# setup epel repo
echo "== setup epel repo =="
wget http://download.fedoraproject.org/pub/epel/5/i386/epel-release-5-4.noarch.rpm
rpm -ivh epel-release-5-4.noarch.rpm

# install git
echo "== install git =="
yum -y install git-core

# install openssl
yum -y install openssl openssl-devel

# install nginx
echo "== install nginx =="
yum -y install nginx
/etc/init.d/nginx start
/sbin/chkconfig nginx on

# complete
echo "bootstrap complete!"
