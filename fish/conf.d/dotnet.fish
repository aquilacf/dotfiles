# Disable .NET CLI telemetry
set -gx DOTNET_CLI_TELEMETRY_OPTOUT 1

# Add .NET CLI and tools to PATH
fish_add_path /usr/local/share/dotnet
fish_add_path $HOME/.dotnet/tools
