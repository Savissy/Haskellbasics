# Enable sandboxing for reproducible builds 
sandbox = true 
# Enable modern Nix features (required for Plutus) 
experimental-features = nix-command flakes 
# Automatically optimize store to save space 
auto-optimise-store = true 
# Use official and IOHK binary caches to speed up builds 
substituters = https://cache.nixos.org https://hydra.iohk.io https://iohk.cachix.org/ 
# Trusted keys for the caches above 
trusted-public-keys = \ cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= \ hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= \ iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= 
# Allow users in the nix-users group to control builds 
trusted-users = root hope 
# Reduce logs and improve performance 
max-jobs = auto cores = 0 keep-outputs = true keep-derivations = true 
