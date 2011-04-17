CentOS 5.x bootscripts
====================================

Simple automation script for new CentOS 5.x instance.
You can use this for Sakura Internat VPS, for instance.

Usage
------------------------------------

   % tar -zcvf init.tar.gz init
   % scp centos_boot.sh root@<destination host>:.
   % scp init.tar.gz root@<destination host>:.
   % ssh root@<destination host>

   # sh centos_boot.sh
   # shutdown -r now

Login after script run
------------------------------------

After you run bootstrap script, login like this

   % ssh -p 1234 <destination host>
