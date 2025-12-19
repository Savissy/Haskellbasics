https://docs.microsoft.com/en-us/windows/wsl/install  

https://us02web.zoom.us/j/86904319201?pwd=CZ9Jj2pbyNWeoRotFgjc8PDTYgzx5K.1
# Create a directory for WSL
mkdir C:\WSL

# Import the downloaded Ubuntu image
wsl --import Ubuntu-20.04 C:\WSL\Ubuntu-20.04 "$env:USERPROFILE\Downloads\ubuntu-wsl.tar.gz"

# Set it as default
wsl --set-default Ubuntu-20.04

# Run it to set up your user account
wsl -d Ubuntu-20.04

# Create the directory
mkdir C:\WSL
cd C:\WSL

# Download the Ubuntu 20.04 rootfs
# Note: This is a direct link to the rootfs tarball from the Ubuntu cloud images.
$url = "https://cloud-images.ubuntu.com/releases/focal/release/ubuntu-20.04-server-cloudimg-amd64-wsl.rootfs.tar.gz"
$output = "C:\WSL\ubuntu-20.04-wsl.rootfs.tar.gz"
Invoke-WebRequest -Uri $url -OutFile $output

Get-ChildItem C:\WSL\


cabal run bls12-381-costs 4. Launch REPL: cabal repl plutus-tx Test expressions: 

https://chatgpt.com/share/69443a73-2ae8-8008-8079-1999fa8cf9e7
EBU/Onchain

create an off chain code for the working smart contract above without using lucid to properly test the smart contract from my terminal. all emulator tests and the necessary cabal modifications should be provided. note that the above smart contract fully compiles successfully 
