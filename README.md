# Setting up Arch Linux

## Setup timezone
ln -sf /usr/share/zoneinfo/Europe/London /etc/localtime
hwclock --systohc

## Locale
echo "en_GB.UTF-8 UTF-8" >> /etc/locale.gen
echo "pt_BR.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen
echo "LANG=en_GB.UTF-8" >> /etc/locale.conf
echo "KEYMAP=uk" >> /etc/vconsole.conf

## Hostname
echo "arch" > /etc/hostname

## Users
useradd -m -G wheel aquila
passwd aquila


## Graphics
pacman -Qi wayland
