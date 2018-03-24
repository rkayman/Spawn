sudo mkdir /usr/local/libexec/spawnd
sudo mkdir /etc/spawnd
sudo cp ~/GitHub/Spawn/bin/Debug/netcoreapp2.0/osx.10.12-x64/* /usr/local/libexec/spawnd
sudo cp ~/GitHub/Spawn/configs/amberPipe-atom.json /etc/spawnd
sudo cp ~/GitHub/Spawn/daemon/com.pwc.tax.amber.spawnd.plist /Library/LaunchDaemons
