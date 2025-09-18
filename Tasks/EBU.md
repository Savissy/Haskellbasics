https://us02web.zoom.us/j/86904319201?pwd=CZ9Jj2pbyNWeoRotFgjc8PDTYgzx5K.1
# Create a directory for WSL
mkdir C:\WSL

# Import the downloaded Ubuntu image
wsl --import Ubuntu-20.04 C:\WSL\Ubuntu-20.04 "$env:USERPROFILE\Downloads\ubuntu-wsl.tar.gz"

# Set it as default
wsl --set-default Ubuntu-20.04

# Run it to set up your user account
wsl -d Ubuntu-20.04
