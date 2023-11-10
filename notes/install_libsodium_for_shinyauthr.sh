
#from https://unix.stackexchange.com/a/42569


wget https://download.libsodium.org/libsodium/releases/LATEST.tar.gz .

tar -xvf LATEST.tar.gz 

 cd libsodium-stable/

 make

 ./configure --prefix=$HOME/libsodium

 make install

 nano ~/.bashrc 

#add line:
export PATH="$HOME/libsodium-stable/bin:$PATH"

#CTRL x to quit and Y to update file
 
 source ~/.bashrc